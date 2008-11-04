/* translates tree gimple into Sun IR.
   Copyright (C) 2007, 2008 by Sun Microsystems, Inc. All rights reserved.  
   File is licensed under the GNU Public License.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "ggc.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "timevar.h"
#include "tree-dump.h"
#include "toplev.h"
#include "except.h"
#include "cfgloop.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "c-common.h"
#include "tree-ir.h"

int default_opt_level = 0;
int tree_ir_verbosity_level = 0;

ir_proc_hdl_t irProc; /* current IR procedure */
ir_mod_hdl_t irMod;  /* current IR module */

int npointers;
static int naliases;

int ehnode_count;
ir_eh_node_hdl_t clean_up_ehnode;

/* LNI, line number information */

/* The default proc, this never changes */
static LN_ProcHandle_t	lni_proc;

/* Stack maintained for inlining */
typedef struct lni_ctx_t
{
    LN_FileCtx_t file_hdl;
    char *file;
    LN_Element_t call_site;
    struct lni_ctx_t *prev;
} lni_ctx_t;

/* Remember how deep we are nested */
static int inline_depth = 0;

/* The current lni stack pointer */
lni_ctx_t *current_lni_ctx = NULL;

void
push_lni_inline_context (tree stmt)
{
  lni_ctx_t *lni_ctx;
  gcc_assert (EXPR_HAS_LOCATION (stmt));

  /* During inlining we wrap the inline function
     body with a builtin function call. The call
     carries the line number information of
     the original call triple */

  expanded_location xloc = expand_location (EXPR_LOCATION (stmt));
  
  lni_ctx = (lni_ctx_t *) xmalloc (sizeof (lni_ctx_t));
  lni_ctx->prev = current_lni_ctx;
  lni_ctx->file_hdl = current_lni_ctx->file_hdl;
  
  if (xloc.line <= 0 || xloc.file == NULL)
    {
      /* Some problem with line numbers simply use
         the last valid line number as the point where
         inlining happens */
      lni_ctx->call_site = current_lni_ctx->call_site;
      lni_ctx->file = xstrdup (current_lni_ctx->file);
    }
  else
    {
      /* We have identified inlining. create the lni for
         the call site. Note: the caller line number is
         probably never user in the current proc, we
         generate it for passing the inlining transform
         information */
      current_lni_handle = linenum_create (lni_proc, current_lni_ctx->file_hdl,
                                           xloc.line, LN_COL_UNASSIGNED,
                                           LN_PH_ACOMP);
      lni_ctx->call_site = current_lni_handle;
      
      /* These will be filled in when we encounter the first
         line number for the callee */
      lni_ctx->file = NULL;
    }

  current_lni_ctx = lni_ctx;
  inline_depth++;
}

void
pop_lni_inline_context (tree stmt)
{
  lni_ctx_t *lni_ctx;
  gcc_assert (EXPR_HAS_LOCATION (stmt));
  gcc_assert (current_lni_ctx->prev != NULL);
  gcc_assert (inline_depth >= 1);
  inline_depth--;
  lni_ctx = current_lni_ctx;
  current_lni_ctx = lni_ctx->prev;
  if (lni_ctx->file)
    free (lni_ctx->file);
  free (lni_ctx);
  current_lni_handle = current_lni_ctx->call_site;
}

/* Switch the lni source context if needed */
LN_Element_t
lni_source_transition (expanded_location *xloc)
{
  /* Sometimes the line number is zero. Especially
     for temp statements created by the compiler.
     LNI does not like it. Dont transition. Return
     the old handle */

  LN_Element_t ln;
    
  if (xloc->line <= 0 || xloc->file == NULL)
    return current_lni_handle;

  if (current_lni_ctx->file == NULL)
    {
      current_lni_ctx->file = xstrdup (xloc->file);
      if (current_lni_ctx->prev
          && strcmp (xloc->file, current_lni_ctx->prev->file) != 0)
        {
          /* Inlining has caused a source file name change
             create a new file handle */
          current_lni_ctx->file_hdl =
              linenum_file_context_push (lni_proc,
                                         LN_FILECTX_EMPTY,
                                         xloc->file,
                                         0, 0,  /* Required */
                                         LN_OP_NONE, LN_PH_ACOMP);
        }
    }
  else if (strcmp (xloc->file, current_lni_ctx->file) != 0)
    {
        /* Source file name change probably due to header
           file inclusion. Simply push a new file context */
        current_lni_ctx->file_hdl =
            linenum_file_context_push (lni_proc,
                                       LN_FILECTX_EMPTY,
                                       xloc->file,
                                       0, 0,  /* Required */
                                       LN_OP_NONE, LN_PH_ACOMP);
        free (current_lni_ctx->file);
        current_lni_ctx->file = xstrdup (xloc->file);
    }

  ln = linenum_create (lni_proc, current_lni_ctx->file_hdl, xloc->line,
                       LN_COL_UNASSIGNED, LN_PH_ACOMP);
  if (inline_depth > 0) {
      ln = linenum_transform (lni_proc, ln, lni_proc,
                              current_lni_ctx->call_site, LN_XT_INLINE,
                              LN_PH_ACOMP);
  }
                              
  return ln;
}

static void
lni_init (const char *source_file,
          int source_lineno)
{
  lni_proc = ir_proc_linenum_proc_hdl (irProc);
  /* TODO! Need to add a marker for GCC. Currently
     using acomp */
  lni_ctx_t *lni_ctx = (lni_ctx_t *) xmalloc (sizeof (lni_ctx_t));
  lni_ctx->file = xstrdup (source_file);
  lni_ctx->file_hdl = linenum_file_context_push (lni_proc, LN_FILECTX_EMPTY,
                                                 source_file, 0, 0, LN_OP_NONE,
                                                 LN_PH_ACOMP);
  lni_ctx->prev = NULL;
  inline_depth = 0;

  /* LNI does not tolerate line number zero Fix 6624815 */
  if (source_lineno <= 0)
    source_lineno = 1;
  
  current_lni_handle = linenum_create (lni_proc, lni_ctx->file_hdl,
                                       source_lineno,
                                       LN_COL_UNASSIGNED, LN_PH_ACOMP);
  lni_ctx->call_site = current_lni_handle;
  current_lni_ctx = lni_ctx;
}

static void
lni_fini (void)
{
  gcc_assert (current_lni_ctx != NULL);
  gcc_assert (current_lni_ctx->prev == NULL);
  gcc_assert (inline_depth == 0);
  free (current_lni_ctx->file);
  free (current_lni_ctx);
  
  lni_proc = NULL;
  current_lni_handle = NULL;
  current_lni_ctx = NULL;
  inline_depth = 0;
}

/* Generate IR for exit block. Only one exit block per procedure */
void
dump_ir_exit (TYPE ret_type, IR_TYPE_NODE *ret_ir_type,
              int ret_label, unsigned int floatmap)
{
  build_ir_block (NULL, ret_label, IR_FALSE, IR_FALSE);

  if (func_ret_leaf != NULL) 
    {
      switch (ret_type.tword) 
        {
        case PCC_TVOID:
          break;
        case PCC_STRTY:
        case PCC_UNIONTY:
          {
            IR_NODE *tmp;
            IR_NODE *np;
            TRIPLE *tp;
            tree ptr_fval_type;
            TYPE argtype;

            if (!TARGET_ARCH64 && TREE_CODE (fval_type) == REFERENCE_TYPE)
              {
                tmp = func_ret_leaf;
              }
            else
              {
                 /* cleanup: now it's done in adjust_leaf_overlaps()
                  * func_ret_leaf->leaf.no_reg = IR_TRUE;
                 if (func_ret_leaf->leaf.elvarno == -1)
                   func_ret_leaf->leaf.elvarno = naliases++;
                       
                 add_ir_leaf_overlap (func_heap_leaf, (LEAF*)func_ret_leaf);*/

                 ptr_fval_type = build_pointer_type (fval_type);
                 argtype = map_gnu_type_to_TYPE (ptr_fval_type);
                       
                 tmp = build_ir_addr_const (func_ret_leaf, 0, argtype, 
                                            map_gnu_type_to_IR_TYPE_NODE (ptr_fval_type));

                 /* ippt optimization in iropt requires fval (ifetch (var_leaf)) */
                 /* TODO fix ippt
                 if (globalize_flag)
                   {
                     IR_NODE * new_leaf = get_tmp_leaf (argtype,
                                                  map_gnu_type_to_IR_TYPE_NODE (ptr_fval_type));
                     build_ir_triple (IR_ASSIGN, new_leaf, tmp, new_leaf->operand.type, NULL);
                     tmp = new_leaf;
                   }
                   */
              }
            np = build_ir_triple (IR_IFETCH, tmp, NULL, ret_type,
                             ret_ir_type);

            tp = (TRIPLE *) build_ir_triple (IR_FVAL, np, NULL, ret_type, NULL);
            tp->param_float_map = floatmap;
            break;
          }
        default:
          build_ir_triple (IR_FVAL, func_ret_leaf, NULL, func_ret_leaf->operand.type, NULL);
        }
    } 

  func_ret_leaf = NULL;
  build_ir_goto (EXIT_LABELNO);
}

void 
dump_ir_init (const char *procname,  tree fn,
              int procno,  TYPE proctype,  int procglobal, 
              int rval_is_structptr, const char *source_file,
              int source_lineno)
{
  ir_ADDRESS addr;
  int main_label;
  TYPE undeftype = {PCC_UNDEF, 0, 0};

  irProc = ir_mod_new_proc (irMod);

  lni_init (source_file, source_lineno);
  init_ir_segment_htab_once ();
  init_ir_leaf_htab_once ();
  
  memset ((char*)&addr, 0, sizeof(addr));
  init_segments ();

  /* reset resource id counters */
  npointers = 0;
  ehnode_count = 0;
  clean_up_ehnode = NULL;
  /* cleanup: elvarnos are tracked in adjust_leaf_overlaps()
   * naliases = 0; */

  ir_proc_set_is_comdat (irProc, (procglobal && DECL_COMDAT (fn) && flag_comdat));

  /* build heap leaf */
  addr.seg = segtab[HEAP_SEGNO];
  func_heap_leaf = build_ir_leaf (VAR_LEAF, undeftype,
                                  map_gnu_type_to_IR_TYPE_NODE (void_type_node),
                                  (LEAF_VALUE *)&addr, IR_TRUE);
  
  func_heap_leaf->pass1_id = build_ir_proc_string ("<heap>");
  /* cleanup: elvarnos are tracked in adjust_leaf_overlaps()
   * func_heap_leaf->elvarno = naliases++; */

  /* initialize header */
  ir_proc_set_name (irProc, procname);
  ir_proc_set_id (irProc, procno);
  ir_proc_set_source_file (irProc, source_file);
  ir_proc_set_source_line (irProc, source_lineno);
  ir_proc_set_lang (irProc, C); /* can be changed later */
  ir_proc_set_type (irProc, proctype);
  if (flag_tm_mode)
    {
      if (DECL_IS_TM_ATOMIC_P (fn)) 
        ir_proc_set_tm_atomic (irProc, 1);

      if (DECL_IS_TM_CALLABLE_P (fn) || DECL_IS_TM_ABORT_OK_P (fn))
        ir_proc_set_tm_callable (irProc, 1);
    }

  /* define entry block */
  build_ir_block (procname, gen_ir_label (),  IR_TRUE, procglobal);
  func_entry_triple = (TRIPLE *) build_ir_triple (IR_ENTRY, NULL, NULL, proctype, NULL);
  main_label = gen_ir_label ();
  build_ir_goto (main_label);
  
  /* define main block */
  build_ir_block (NULL, main_label, IR_FALSE, IR_FALSE);
  
  /* IR directly to CG, skipping IROPT... TODO
  	main_label = gen_ir_label ();
      build_ir_block (procname, main_label, IR_TRUE, (BOOLEAN)procglobal);
      func_entry_triple = (TRIPLE *) build_ir_triple (IR_ENTRY, NULL, NULL, hdr.proc_type, NULL);
  }*/	
  if (rval_is_structptr)
    func_entry_triple->is_stcall = IR_TRUE;
  
  /* Initialize ir lexical scopes generation.  */
  if (dbg_gen_generate_scopes ())
    ir_scope_gen_init ();
}

/* set ld_scope. It is valid only when np is a variable or const function pointer leaf. */
void
set_leaf_ld_scope (IR_NODE *np, int vis)
{
  static IR_LD_SCOPE const ld_scopes[] = {
    IR_GLOBAL_LD_SCOPE,
    IR_SYMBOLIC_LD_SCOPE,
    IR_HIDDEN_LD_SCOPE,
    IR_HIDDEN_LD_SCOPE
  };
  if (vis <4)
      np->leaf.ld_scope=ld_scopes[vis];
}

/* add leaf2 to leaf1's overlap list */
static void
add_ir_leaf_overlap (LEAF *leaf1, LEAF *leaf2)
{
  LEAF *leafp;
  LIST *lp, *overlap, *newlistp;

  if (leaf1 != leaf2) 
    {
      overlap = leaf1->overlap;
      LFOR(lp,overlap) 
        {
          leafp = (LEAF*)lp->datap;
          if (leafp == leaf2) return;
        }
      newlistp = build_ir_proc_list();
      newlistp->datap = (union list_u *)leaf2;
      newlistp->next = leaf1->overlap;
      leaf1->overlap = newlistp;
    }
}

/* cleanup: now it's done in build_ir_leaf() 
* void
* set_leaf_pointerno (IR_NODE *np, TWORD tword)
* {
*   if (PCC_ISPTR (tword) && np->leaf.pointerno == -1)
*     np->leaf.pointerno = npointers++;
* } */

void
adjust_leaf_overlaps (void)
{
  LEAF *leaf1, *leaf2;
  IR_OFFSZ lb1, lb2, ub1, ub2;
  IR_OFFSZ bfsz1, bfsz2;
  IR_OFFSZ lub, glb;
  int unknownsize;
  SEGMENT * seg;

  /* L0 or heap_leaf should have elvarno == 0 */
  func_heap_leaf->elvarno = 0;
  naliases = 1;
 
  gcc_assert (func_heap_leaf == first_leaf);

  /* debug: set elvarno to -1 for all leaves of current function
  for (leaf1 = first_leaf->next_leaf; leaf1; leaf1 = leaf1->next_leaf) 
    {
      if (leaf1->class == VAR_LEAF)
        leaf1->elvarno = -1;
    } */

  /* iterate through the leaves and set elvarno/no_reg bits */
  for (leaf1 = first_leaf->next_leaf; leaf1; leaf1 = leaf1->next_leaf) 
    {
      if (leaf1->class == ADDR_CONST_LEAF)
        {
          if (leaf1->addressed_leaf->elvarno == -1)
            {
              LIST * p;
              /* leaf1->addressed_leaf is address taken.
                 initialize its elvarno */
              int new_elvarno = naliases ++;
              leaf1->addressed_leaf->elvarno = new_elvarno;
              
              /* all leaves in a given IR segment should have the same elvarno */
              seg = leaf1->addressed_leaf->val.addr.seg;
              LFOR (p, seg->leaves) 
                {  
                  leaf2 = (LEAF*)p->datap;
                  leaf2->elvarno = new_elvarno;
                }
            }
          leaf1->addressed_leaf->no_reg = IR_TRUE;
        }
      else if (leaf1->class == VAR_LEAF
               && (leaf1->val.addr.seg->descr.external == EXTSTG_SEG
                   || leaf1->uplevel_addressed))
        {
          leaf1->elvarno = naliases ++;
          /* it seems acomp adds such leaves to L0 overlap list */
	  add_ir_leaf_overlap (func_heap_leaf, leaf1);
          /* but don't set no_reg on them
             leaf1->no_reg = IR_TRUE; */
        }
    }

  /* build overlap lists */
  for (leaf1 = first_leaf; leaf1; leaf1 = leaf1->next_leaf) 
    {
      LIST * p;
      int added_to_heap = 0;
      
      if (leaf1->class != VAR_LEAF) continue;

      if (leaf1->elvarno > 0 && leaf1->no_reg)
        {
	  add_ir_leaf_overlap (func_heap_leaf, leaf1);
          added_to_heap = 1;
        }

      bfsz1 = leaf1->type.size;
      seg = leaf1->val.addr.seg;

      LFOR (p, seg->leaves) 
        {  
          leaf2 = (LEAF*)p->datap;
    
          if (seg != leaf2->val.addr.seg)
            abort ();

          if (leaf2->class != VAR_LEAF)
            abort ();

          if (leaf1 != leaf2 
              && seg != segtab[DREG_SEGNO]
              && seg != segtab[FREG_SEGNO]) 
            {

              /* compute lower and upper bounds for leaf1 and leaf2 */
              bfsz2 = leaf2->type.size;
              /* zero sized leaves */
              unknownsize = (bfsz1 == 0) || (bfsz2 == 0);
              
              /* lower and upper bounds of leaf 1 */
              lb1 = leaf1->val.addr.offset;
              ub1 = lb1 + bfsz1;
              
              /* lower and upper bounds of leaf 2 */
              lb2 = leaf2->val.addr.offset;
              ub2 = lb2 + bfsz2;
              
              /* least upper and greatest lower bounds */
              lub = (ub1 < ub2 ? ub1 : ub2);
              glb = (lb1 > lb2 ? lb1 : lb2);

#ifdef ASYM_OVERLAP
              /* asymmetric overlap */
              if (!unknownsize && lb1 <= lb2 && ub1 >= ub2)
                { /* leaf1 contains leaf2 */
                  add_ir_leaf_overlap (leaf1, leaf2);
                  if (leaf1->no_reg)
                    leaf2->no_reg = IR_TRUE;
                  if (leaf1->elvarno != -1 && leaf2->elvarno == -1)
                    leaf2->elvarno = leaf1->elvarno;
                  if (leaf1->is_volatile)
                    leaf2->is_volatile = IR_TRUE;
                  if (leaf1->is_const)
                    leaf2->is_const = IR_TRUE;
                  if (leaf1->is_restrict)
                    leaf2->is_restrict = IR_TRUE;
                  if (leaf1->is_unshared)
                    leaf2->is_unshared = IR_TRUE;
                }
              /* asymmetric overlap */
              else if (!unknownsize && lb2 <= lb1 && ub2 >= ub1)
                { /* leaf2 contains leaf1 */
                  add_ir_leaf_overlap (leaf2, leaf1);
                  if (leaf2->no_reg)
                    leaf1->no_reg = IR_TRUE;
                  
                  if (leaf2->elvarno != -1 && leaf1->elvarno == -1)
                    leaf1->elvarno = leaf2->elvarno;
                  if (leaf2->is_volatile)
                    leaf1->is_volatile = IR_TRUE;
                  if (leaf2->is_const)
                    leaf1->is_const = IR_TRUE;
                  if (leaf2->is_restrict)
                    leaf1->is_restrict = IR_TRUE;
                  if (leaf2->is_unshared)
                    leaf1->is_unshared = IR_TRUE;
                }
              else 
#endif
              /* symmetric overlap */
              if (unknownsize || glb < lub) 
                {
                  /* add leaf2 to leaf1's overlap list */
                  add_ir_leaf_overlap (leaf1, leaf2);
                  add_ir_leaf_overlap (leaf2, leaf1);

                  if (leaf1->no_reg || leaf2->no_reg)
                    {
                      leaf1->no_reg = IR_TRUE;
                      leaf2->no_reg = IR_TRUE;
                      if (!added_to_heap && leaf1->elvarno > 0)
                        {
                          add_ir_leaf_overlap (func_heap_leaf, leaf1);
                          added_to_heap = 1;
                        }
                    }
      
                  if (leaf1->is_volatile)
                    leaf2->is_volatile = IR_TRUE;
                  if (leaf1->is_const)
                    leaf2->is_const = IR_TRUE;
                  if (leaf1->is_restrict)
                    leaf2->is_restrict = IR_TRUE;
                  if (leaf1->is_unshared)
                    leaf2->is_unshared = IR_TRUE;
                  if (leaf1->in_taskcommon_block)
                    leaf2->in_taskcommon_block = IR_TRUE;

                  if (leaf2->is_volatile)
                    leaf1->is_volatile = IR_TRUE;
                  if (leaf2->is_const)
                    leaf1->is_const = IR_TRUE;
                  if (leaf2->is_restrict)
                    leaf1->is_restrict = IR_TRUE;
                  if (leaf2->is_unshared)
                    leaf1->is_unshared = IR_TRUE;
                  if (leaf2->in_taskcommon_block)
                    leaf1->in_taskcommon_block = IR_TRUE;
                }
            }
        }
    }
 
  /* cleanup IR segments */
  for (leaf1 = first_leaf; leaf1; leaf1 = leaf1->next_leaf) 
    {
      if (leaf1->class != VAR_LEAF) continue;

      seg = leaf1->val.addr.seg;
      seg->leaves = 0;
    }
}

void
ir_backend_init (void)
{
  const char * ir_outfname;
  char * dump_name = NULL;
  
  ir_lib_set_triple_size(sizeof(TRIPLE));
  
  if (ir_file_name == NULL)
    {
      dump_name = concat (aux_base_name, ".ir", NULL);
      ir_outfname = dump_name;
    }
  else
    ir_outfname = ir_file_name;
  
  irMod = ir_mod_new();
  if (! ir_mod_open_for_output(irMod, ir_outfname))
    error ("can't open ir file %s", ir_outfname);

  if (dump_name)
    free (dump_name);
}

void
ir_backend_fini (void)
{
  irProc = NULLIRPROCHDL;
  
  if (tree_ir_verbosity_level) 
    ir_print_types();

  /* Populate the module's debug symbol table from dbg_gen. */
  dbg_gen_extract_mod_debug_info (irMod);
  
  /* Previously would write pseudo-procedure here.  This is
     now done when ir_mod_close_for_output() is called. */
  /* Note that this statement must be guarded to prevent an
     attempt to close the file when it isn't open--namely,
     when yabe is being used. */
  ir_mod_close_for_output(irMod); /* !!! check for error */
}

/* Finish current procedure and if 'do_write' is true, then
   generate debug information for procedure before writing 
   procedure to the IR file. 
   Things must be done in this order because data in the IR 
   tables for the procedure which are needed for debug 
   information generation are modified during the write to 
   IR file (ir_proc_write()).
*/
void
dump_ir_fini (tree fn, int do_write)
{
  ir_triple_iter_t iter = ir_proc_triple_iter(irProc);
  TRIPLE * triple;

  /* do we need to clear some triples here ? TODO */
  while (NULL != (triple = ir_iter_next_triple(&iter)) )
    {
      if (NULL == triple->tnext && NULL == triple->tprev)
      	ir_proc_free_triple(irProc, triple);
    }

  ir_proc_set_num_ptrs (irProc, npointers);
  ir_proc_set_num_elvars (irProc, naliases);
  ir_proc_set_opt_level (irProc, default_opt_level);


  ir_proc_make_lists_circular (irProc);
  ir_proc_renumber_triples (irProc);
  if (do_write) 
  {
      (*debug_hooks->function_decl) (fn);    
      if (tree_ir_verbosity_level)
        ir_print_proc ();
      ir_proc_write (irProc);
  }

  ir_proc_minimize (irProc);
  irProc = NULLIRPROCHDL;
  fini_ir_proc ();
  fini_ir_segment_htab ();
  fini_ir_leaf_htab ();
  if (dbg_gen_generate_scopes ())
    ir_scope_gen_fini ();

  lni_fini ();
}

