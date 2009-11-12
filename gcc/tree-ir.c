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
#include "cp/cp-tree.h"
#include "toplev.h"
#include "except.h"
#include "cfgloop.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "c-common.h"
#include "tree-ssa-propagate.h"
#include "regs.h"
#include "tree-ir.h"
#include "df.h"

#define FIRST_SGCC_VIRTUAL_REGNO 14159000

const char *ir_file_name = NULL; /* flag syntax: -r filename */

const char *ir_global_prefix = 0;

/* Nonzero if we want to emit defined symbols with common-like linkage as
   comdat symbols where possible, in order to conform to C++ semantics.
   Otherwise, emit them as weak or local symbols, depending on flag_weak.  */

#if USE_GNU_LD
int flag_comdat = 0;
#else
int flag_comdat = 1;
#endif

LANG ir_language;

/* use g++ calling convention == return struct val in [sp+64] even for c++ */
int gxx_call_abi = 1;

/* don't skip (bool) conversion. may help some benchs, but usually don't */
int skip_bool_conv = 0; 

int fall_through_label = 0;
int inside_eh_region = 0; 
int number_of_pbranch = 0;
static IR_NODE *last_may_cause_exception_node = 0;
static struct eh_region *current_eh_region = 0; 
static IR_NODE *cfun_eh_filter = 0;
static IR_NODE *cfun_eh_exc_ptr = 0;

tree fval_type;
static tree __builtin_va_alist_node;

static void ir_add_loopinfo (tree);

static TYPE offsettype = {PCC_UNDEF, 0, 0}; /* to be initialized later */

/* leaf contains function return value */
IR_NODE *func_ret_leaf = NULL;

/* leaf contains heap leaf of the current function */
LEAF *func_heap_leaf = NULL;

/* triple points to IR_ENTRY triple in the beginning of the function */
TRIPLE *func_entry_triple;

/* number of label in the last basic block */
int return_label_num;

/* 0 - generate IR
   1 - stop IR gen for current function
  -1 - stop IR gen for compilation unit */
HOST_WIDE_INT flag_use_rtl_backend;

/* xpagesize_stack=
   Controls the preferred page size for the stack, ={8K|64K|512K|4M|32M|256M|2G|16G|default}
  
   xpagesize_heap=
   Controls the preferred page size for the heap, ={8K|64K|512K|4M|32M|256M|2G|16G|default} */

unsigned HOST_WIDE_INT pagesize_stack;
unsigned HOST_WIDE_INT pagesize_heap;

typedef struct 
{
  TRIPLE *begin_scope;
  TRIPLE *end_scope;
} ir_scope_t;

static bool scope_gen_failed;
static varray_type scope_stack;
static varray_type ir_scopes_table;
static int block_scope_id = 0;

enum MAP_FOR {
    ERR,
    MAP_FOR_ADDR,
    MAP_FOR_VALUE
};

static IR_NODE * dump_ir_call (tree stmt, int for_value);
static IR_NODE * dump_ir_builtin_call (tree stmt, int need_return);
static void dump_ir_stmt (tree stmt);
static IR_NODE * dump_ir_call_main (tree stmt, int for_value, tree return_slot);
static IR_NODE * dump_ir_expr (tree stmt, enum MAP_FOR map_for);
static IR_NODE * dump_ir_flushw (tree stmt);

static bool ir_branch_triple_p (IR_OP);
static bool ir_lexical_scope_p (tree);
static tree ir_immediate_pushed_ancestor (tree);
static void ir_change_scope (tree);
static bool ir_ancestor_scope_p (tree ancestor, tree scope);
static bool ir_scope_stack_empty_p (void);
static void ir_push_scope (tree);
static void ir_pop_scope (void);
static void ir_pop_lexical_scopes (void);
static tree ir_top_scope (void);
static void ir_disable_scope_gen (void);

static void ir_scopes_table_init (void);
static void ir_scopes_table_fini (void);
static void ir_begin_scope_triple (TRIPLE *);
static void ir_end_scope_triple (unsigned, TRIPLE *);
static void ir_remove_scope_triples (void);

static void dump_omp_parallel (tree stmt);
static void dump_omp_for (tree stmt);
static void dump_omp_sections (tree stmt);
static void dump_omp_section (tree stmt);
static void dump_omp_single (tree stmt);
static void dump_omp_master (tree stmt);
static void dump_omp_ordered (tree stmt);
static void dump_omp_critical (tree stmt);
static void dump_omp_barrier (tree stmt);
static void dump_omp_atomic_start (tree stmt);
static void dump_omp_atomic_end (tree stmt);
static void dump_omp_flush (tree stmt);
static void dump_omp_return (tree stmt);
static void dump_omp_task (tree stmt);
static void dump_omp_taskwait (tree stmt);
static void generate_cxx_constructor_wrappers (void);

/* In builtins.c */
extern tree fold_builtin_constant_p (tree);

typedef struct region_list
{
  int region_number;
  struct region_list *next;
}region_list;

typedef struct omp_ir_context_t 
{
    PRAGMAINFO *pinfo; /* pragma information.*/
    tree stmt;
    int l1_lab, l2_lab; /* l1: loop_exit; l2: loop body.*/
    int exception_label; /* pragma end. */
    bool lab_used; /* omp region contains exception. */
    region_list *r_list; /* record all eh region. */ 
    struct omp_ir_context_t *prev_ctx;
} omp_ir_context_t;

static omp_ir_context_t *cur_omp_context = NULL;

struct ir_node_list 
{
  union {
    IR_NODE * node;
    int label;
  } u;
  struct ir_node_list *next;
};
static struct ir_node_list * indirect_goto_list;
static struct ir_node_list * indirect_goto_list_tail;
static struct ir_node_list * label_list;
static struct ir_node_list * label_list_tail;

/* Current LNI information.
   NOTE: These must not be manipulated except
   through the functions below or in
   dump_one_function_statement */

static expanded_location ir_location;
LN_Element_t current_lni_handle;

/* Utility to save and restore all relevant
   line number related information. NOTE: this
   is not a stack so calling it repeatedly will
   wipe out old information. It is single level
   only. TODO: Fix this if required. */

static location_t saved_location;
static LN_Element_t saved_lni_handle;

static void
save_and_switch_line_information (tree stmt)
{
  if (!EXPR_HAS_LOCATION (stmt))
    return;
    
  saved_location = input_location;
  saved_lni_handle = current_lni_handle;
  
  input_location = EXPR_LOCATION (stmt);
  ir_location = expand_location (input_location);
  current_lni_handle = lni_source_transition (&ir_location);
}

static void
restore_line_information (tree stmt)
{
  if (!EXPR_HAS_LOCATION (stmt))
    return;
    
  input_location = saved_location;
  current_lni_handle = saved_lni_handle;
  ir_location = expand_location (input_location);
}

static void
add_region_list (omp_ir_context_t *c, int val)
{
  region_list *n = xmalloc (sizeof (struct region_list));
  n->region_number = val;
  n->next = c->r_list;
  c->r_list = n;
}

static void
add_to_label_list (int labelno)
{
  struct ir_node_list *n;

  n = label_list;
  while (n != NULL)
    {
      if (labelno == n->u.label)
        return; /* labelno already in the list */
      n = n->next;
    }

  /* otherwise add to the list */
  
  n = xmalloc (sizeof (struct ir_node_list));

  n->u.label = labelno;
  n->next = 0;
  
  if (label_list == NULL)
    label_list = label_list_tail = n;
  else
    {
      label_list_tail->next = n;
      label_list_tail = n;
    }
}

/* map tree opcode into IR opcode */
static IR_OP
conv_treecode2ir (tree node)
{
  switch (TREE_CODE (node))
    {
    case TRUTH_OR_EXPR:
    case TRUTH_ORIF_EXPR:
      return IR_OR;

    case TRUTH_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
      return IR_AND;

    case BIT_IOR_EXPR:
      return IR_OR;

    case TRUTH_XOR_EXPR:
    case BIT_XOR_EXPR:
      return IR_XOR;

    case BIT_AND_EXPR:
      return IR_AND;

    case EQ_EXPR:
      return IR_EQ;
    
    case UNEQ_EXPR:
      return IR_QGL;

    case NE_EXPR:
      return IR_NE;

    case LT_EXPR:
      return IR_LT;
    
    case UNLT_EXPR:
      return IR_QLT;

    case LE_EXPR:
      return IR_LE;
    
    case UNLE_EXPR:
      return IR_QLE;

    case GT_EXPR:
      return IR_GT;
    
    case UNGT_EXPR:
      return IR_QGT;

    case GE_EXPR:
      return IR_GE;
    
    case UNGE_EXPR:
      return IR_QGE;
      
    case UNORDERED_EXPR:
      return IR_QUO;

    case LSHIFT_EXPR:
      return IR_LSHIFT;

    case RSHIFT_EXPR:
      return IR_RSHIFT;

    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
      return IR_PLUS;

    case NEGATE_EXPR:
      return IR_NEG;

    case MINUS_EXPR:
      return IR_MINUS;

    case TRUTH_NOT_EXPR:
      return IR_NOT;
      
    case BIT_NOT_EXPR:
      return IR_COMPL;

    case MULT_EXPR:
      return IR_MULT;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case EXACT_DIV_EXPR:
      return IR_DIV;

    case TRUNC_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      return IR_REMAINDER;

    default:
      debug_tree (node);
      abort ();
      return IR_ERR;
    }
}

int
gen_ir_label (void)
{
  static int label = 31415926; /* 57000000;  */
  return label++;
}

/* turns on if we compile for cross module opts - xipo in backend 
   and -ftree-ir-crossfile in cc1 */
int globalize_flag = 0;

/* encode bits to allowed chars */
static void
encode_bits (char *buf, unsigned int bits, int count)
{
  static char allowed_chars[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_$";
  while (count-- > 0) 
    {
      *buf++ = allowed_chars[bits & 0x3f];
      bits >>= 6;
    }
}

/* generate unix prefix for globalization */
const char *
generate_prefix (void)
{
  static char prefix[20];
  unsigned tod = (unsigned) time (NULL);
  unsigned pid = (unsigned) getpid ();
  unsigned bits;

  strcpy (prefix, "$XA_sgcc");
  encode_bits (prefix + 8, tod, 5);
  bits = (pid << 2) | (tod >> 30);
  encode_bits (prefix + 13, bits, 3);
  prefix[16] = '.';
  prefix[17] = 0;
  return prefix;
}

static IR_NODE *
get_err_triple (void)
{
 static IR_NODE * err_triple = 0;
 if (err_triple == 0)
   {
     TYPE argtype = map_gnu_type_to_TYPE (void_type_node);
     err_triple = build_ir_triple (IR_ERR, 0, 0, argtype,
                                   map_gnu_type_to_IR_TYPE_NODE (void_type_node));
   }
 return err_triple;
}

/* generate assembler label number for given LABEL_DECL */
int
get_ir_label (tree label)
{
  int uid;

  if (TREE_CODE (label) != LABEL_DECL)
    abort ();

  /* (int) IDENTIFIER_POINTER (DECL_NAME (label)) - is not unique within 
     compilation unit */

  /* LABEL_DECL_UID (label) + 16535; - is not unique within compilation unit */
  
  uid = DECL_UID (label) + UID_IR_OFFSET; /* <D*> */ /* unique for file */

/* this part is moved to stmt.c */
  /*  if (!DECL_RTL_SET_P (label))
    { 
      rtx r = gen_label_rtx ();
      CODE_LABEL_NUMBER (r) = uid;
      SET_DECL_RTL (label, r);
      if (FORCED_LABEL (label) || DECL_NONLOCAL (label))
        LABEL_PRESERVE_P (r) = 1;
    }*/

  return uid;
}

/* Compute the inverse of X mod 2**n, i.e., find Y such that X * Y is
   congruent to 1 (mod 2**N).  */

static unsigned HOST_WIDE_INT
invert_mod2n (unsigned HOST_WIDE_INT x, int n)
{
  /* Solve x*y == 1 (mod 2^n), where x is odd.  Return y.  */

  /* The algorithm notes that the choice y = x satisfies
     x*y == 1 mod 2^3, since x is assumed odd.
     Each iteration doubles the number of bits of significance in y.  */

  unsigned HOST_WIDE_INT mask;
  unsigned HOST_WIDE_INT y = x;
  int nbit = 3;

  mask = (n == HOST_BITS_PER_WIDE_INT
	  ? ~(unsigned HOST_WIDE_INT) 0
	  : ((unsigned HOST_WIDE_INT) 1 << n) - 1);

  while (nbit < n)
    {
      y = y * (2 - x*y) & mask;		/* Modulo 2^N */
      nbit *= 2;
    }
  return y;
}

/* generate CONST_LEAF for simple function call */
bool builtin_function_disabled_p (const char *);

static IR_NODE * 
dump_ir_funcname (tree fn)
{
  IR_NODE * ret;
  const char * real_name = NULL;
  int is_setjmp_call = 0;
  
  if (TREE_CODE (fn) != FUNCTION_DECL) abort();
  
  if (setjmp_call_p (fn))
    is_setjmp_call = 1;

  if (DECL_BUILT_IN (fn))
    {
      if (DECL_BUILT_IN_CLASS (fn) == BUILT_IN_MD)
        {
          real_name = (* targetm.strip_name_encoding) (
                         IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn)));

          /* replace __builtin_vis_fmul8x16 with __vis_fmul8x16 */
          if (strncmp (real_name, "__builtin_", sizeof ("__builtin_") - 1) == 0)
            {
              static char buf[512] = "__";
              strcpy (buf + 2, real_name + sizeof ("__builtin_") - 1);
              real_name = buf;
  	      if (!strcmp(real_name,"__vis_pdist") && TARGET_ARCH64)
		real_name = "__vis_pxldist64";
            }
        }
      else
        switch (DECL_FUNCTION_CODE (fn))
          {
          case BUILT_IN_ALLOCA:
            real_name = "__builtin_alloca"; /* need this name for CG */
            break;
          case BUILT_IN_SETJMP:
            real_name = "setjmp"; /* need this name for CG */
            is_setjmp_call = 1;
            break;
          case BUILT_IN_LONGJMP:
            real_name = "longjmp"; /* need this name for CG */
            break;
          case BUILT_IN_CTZ:
          case BUILT_IN_CTZL:  if (!TARGET_ARCH64) {real_name = "__ctzsi2"; break;}
          case BUILT_IN_CTZLL: real_name = "__ctzdi2"; break;

          case BUILT_IN_CLZ:
          case BUILT_IN_CLZL:  if (!TARGET_ARCH64) {real_name = "__clzsi2"; break;}
          case BUILT_IN_CLZLL: real_name = "__clzdi2"; break;
                               
          case BUILT_IN_PARITY:
          case BUILT_IN_PARITYL:  if (!TARGET_ARCH64) {real_name = "__paritysi2"; break;}
          case BUILT_IN_PARITYLL: real_name = "__paritydi2"; break;
                                  
          case BUILT_IN_POPCOUNT:
          case BUILT_IN_POPCOUNTL:  if (!TARGET_ARCH64) {real_name = "__popcountsi2"; break;}
          case BUILT_IN_POPCOUNTLL: real_name = "__popcountdi2"; break;
                                    
          case BUILT_IN_FFS: if (TARGET_ARCH64) {real_name = "ffs"; break;}
          case BUILT_IN_FFSL:  if (!TARGET_ARCH64) {real_name = "__ffssi2"; break;}
          case BUILT_IN_FFSLL: real_name = "__ffsdi2"; break;
          
          default:
            real_name = (* targetm.strip_name_encoding) (
                         IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn)));
            break;
          }
    }
  else if (lookup_attribute ("weakref", DECL_ATTRIBUTES (fn)))
    {
      tree ultimate_transparent_alias_target (tree *alias);
      tree id;
      real_name = targetm.strip_name_encoding (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn)));
      id = maybe_get_identifier (real_name);
      if (id)
        {
          tree id_orig = id;

          mark_referenced (id);
          ultimate_transparent_alias_target (&id);
          if (id != id_orig)
    	    real_name = IDENTIFIER_POINTER (id);
          gcc_assert (! TREE_CHAIN (id));
        }
    }
  else if (real_name == NULL)
    {
      tree id;
      real_name = targetm.strip_name_encoding (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn)));
      
      id = maybe_get_identifier (real_name);
      if (id)
        mark_referenced (id);
    }
  
  ret = build_ir_funcname (real_name, 
                           map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (fn))), 
                           map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (TREE_TYPE (fn)))); 

  if (strcmp (real_name, "getcontext") == 0
      || strcmp (real_name, "on_fault") == 0)
    is_setjmp_call = 1;

  if (is_setjmp_call)
    {
      DECL_VISIBILITY_SPECIFIED(fn) = 1;
      DECL_VISIBILITY (fn) = VISIBILITY_DEFAULT;
      ret->leaf.unknown_control_flow = IR_TRUE;
      ret->leaf.func_descr = SUPPORT_FUNC;
    }
  
  /* make no-inline any function that matches -fno-builtin-funcname,
     not only builtin funcs */
  if (builtin_function_disabled_p (real_name))
    ret->leaf.is_volatile = IR_TRUE;

  if (strcmp (real_name, "_Znwj") == 0
      || strcmp (real_name, "_Znaj") == 0
      || strcmp (real_name, "_ZdlPv") == 0
      || strcmp (real_name, "_ZdaPv") == 0
      || strcmp (real_name, "_ZSt9terminatev") == 0)
    {
        /* Mark operator new and delete and some
           other exception related routines as support
           functions. */
        ret->leaf.func_descr = SUPPORT_FUNC;
    }
  
  if (DECL_BUILT_IN (fn))
    {
      ret->leaf.func_descr = SUPPORT_FUNC;

      /* never mark all builtin funcs as no-inline with -O0 and !__builtin_ prefix */
      /* if (default_opt_level == 0
          && strncmp (IDENTIFIER_POINTER (DECL_NAME (fn)), "__builtin_", 10)
          && DECL_FUNCTION_CODE (fn) != BUILT_IN_ALLOCA)
        ret->leaf.is_volatile = IR_TRUE; */ 
    }
  
  /* mark some builtin funcs as pure_func, so they don't read/write
     global variables */
  if (default_opt_level
      && (TREE_READONLY (fn) /* quick check, flags_from_decl_or_type() will check it too */
          || flags_from_decl_or_type (fn) & ECF_CONST)) /* ECF_PURE may read globals */
    ret->leaf.pure_func = IR_TRUE;

  /* mark all system functions as support_func to help iropt recognize whole
     program mode */
  if (DECL_IN_SYSTEM_HEADER (fn))
    ret->leaf.func_descr = SUPPORT_FUNC;
      
/*  gcc 3.5.x style of marking used names */
  mark_decl_referenced (fn);

  gcc_assert(ret->operand.tag ==ISLEAF);
  if (TREE_PUBLIC (fn) && !DECL_EXTERNAL(fn))
  {
    enum symbol_visibility vis = DECL_VISIBILITY (fn);
    set_leaf_ld_scope(ret,vis);
  }

  return ret;
}

static long long cur_auto_off = 0;

static long long
lookup_auto_offset (tree stmt, TYPE argtype)
{
  if (DECL_IR_OFFSET (stmt)) return DECL_IR_OFFSET (stmt);

  /* anonymous union variables should have same offset as the union */
  if (DECL_VALUE_EXPR (stmt))
    {
      tree object = TREE_OPERAND (DECL_VALUE_EXPR (stmt), 0);
      if (lang_hooks.tree_inlining.anon_aggr_type_p (TREE_TYPE (object)))
        {
          long long offset = lookup_auto_offset (object, 
                             map_gnu_type_to_TYPE (TREE_TYPE (object)));
          DECL_IR_OFFSET (stmt) = offset;
          return offset;
        }
    }
  
  cur_auto_off -= argtype.size;

  /* round down for positive #
     cur_auto_off = (cur_auto_off - argtype.align + 1) & ~(argtype.align - 1); */
  
  /* round down for negative offsets. auto_off is always negative */
  cur_auto_off = cur_auto_off & ~(argtype.align - 1LL);
  
  DECL_IR_OFFSET (stmt) = cur_auto_off;

  return cur_auto_off;
}

static long long cur_param_off; /* 68 in v8* archs */

static long long
lookup_param_offset (tree stmt, TYPE argtype)
{
  long long off;
  
  if (TARGET_ARCH64)
    return lookup_auto_offset (stmt, argtype);

  if (DECL_IR_OFFSET (stmt)) return DECL_IR_OFFSET (stmt);
  
  off = (argtype.size >= 4) ? argtype.size : 4;
  
  DECL_IR_OFFSET (stmt) = cur_param_off + off - argtype.size;
  
  cur_param_off += off;

  return DECL_IR_OFFSET (stmt);
}

#define SCOPE_TEMP_AS_PRIVATE(irnode) \
      if (cur_omp_context && (cur_omp_context->pinfo \
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo))) \
        { \
          LIST *lp; \
          PRAGMAINFO *pinfo; \
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo \
                                         : cur_omp_context->prev_ctx->pinfo; \
          lp = build_ir_proc_list (); \
          lp->datap = (LDATA *) ((irnode)); \
          LAPPEND (pinfo->u.s.omp_private, lp); \
        }  

#define SCOPE_TEMP_AS_PRIVATE_IFNEEDED(tree, name, irnode) \
      if (DECL_ARTIFICIAL ((tree)) && !(name) \
          && !(TREE_CODE ((tree)) == VAR_DECL \
               && DECL_HAS_SCOPED_CLAUSE_P ((tree)))) \
        { \
          SCOPE_TEMP_AS_PRIVATE((irnode)) \
          if (TREE_CODE ((tree)) == VAR_DECL) \
            DECL_HAS_SCOPED_CLAUSE_P ((tree)) = 1; \
        }

IR_NODE *
get_tmp_leaf (TYPE argtype, IR_TYPE_NODE * typep)
{
  IR_NODE * ret;
  static int cnt = 0;
  char buf[64];

  snprintf (buf, sizeof (buf), "#tmp.var.%03u", ++cnt);
  
  cur_auto_off -= argtype.size;

  /* round down for negative offsets. auto_off is always negative */
  cur_auto_off = cur_auto_off & ~(argtype.align - 1LL);

  ret = build_ir_auto_var (buf, cur_auto_off /*segoff*/,
                     argtype.size /*seglen*/,
                     argtype.align /*align*/, 0 /*off*/,
                     argtype, typep);

  SCOPE_TEMP_AS_PRIVATE(ret);
    
  /* cleanup: now it's done in adjust_leaf_overlaps()
  ret->leaf.elvarno = naliases++; */
  /* cleanup: now it's done in build_ir_leaf() 
  set_leaf_pointerno (ret, argtype.tword);*/

  return ret;
}

static IR_NODE *
get_tmp_leaf_with_name (TYPE argtype, IR_TYPE_NODE * typep, const char * name)
{
  IR_NODE * ret;
  
  cur_auto_off -= argtype.size;

  /* round down for negative offsets. auto_off is always negative */
  cur_auto_off = cur_auto_off & ~(argtype.align - 1LL);

  ret = build_ir_auto_var (name, cur_auto_off /*segoff*/,
                     argtype.size /*seglen*/,
                     argtype.align /*align*/, 0 /*off*/,
                     argtype, typep);

  SCOPE_TEMP_AS_PRIVATE(ret);
    
  /* cleanup: now it's done in adjust_leaf_overlaps()
  ret->leaf.elvarno = naliases++; */
  /* cleanup: now it's done in build_ir_leaf() 
  set_leaf_pointerno (ret, argtype.tword); */

  return ret;
}

static IR_NODE *
get_ir_chain_reg (TYPE argtype, IR_TYPE_NODE * typep)
{
  IR_NODE * ret = TARGET_ARCH64 
                  ? build_ir_reg_var ("%g5", IR_REG_G5, argtype, typep)
                  : build_ir_reg_var ("%g2", IR_REG_G2, argtype, typep);
  ret->leaf.uplevel_addressed = IR_TRUE;
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
get_ir_stack_pointer_reg (void)
{
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_NODE * ret = build_ir_reg_var ("%sp", IR_REG_SP, argtype, 
                                     map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
get_ir_frame_pointer_reg (void)
{
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_NODE * ret = build_ir_reg_var ("%fp", IR_REG_FP, argtype, 
                                    map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
get_ir_i7_reg (void)
{
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_NODE * ret = build_ir_reg_var ("%i7", IR_REG_I7, argtype,
                                    map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

/* search Struct/Union/Enum for given 'name' */
static IR_TYPE_NODE *
ir_suembr2 (IR_TYPE_NODE *sue, const char * name)
{
  LIST * head;
  LIST * child;

  /* make sure 'sue' is a struct type */
  if (sue->tid != PCC_STRTY && sue->tid != PCC_UNIONTY)
    return 0;

  head = sue->t.s.children;

  if (name == NULL) return 0;

  LFOR(child, head) 
    {
      IR_TYPE_NODE * ir_type = LCAST(child, IR_TYPE_NODE);
      if (strcmp (ir_type->label, name) == 0)
        return ir_type;
    }
  return 0;
}

static int internal_labelno = 1000;
      
char *
make_global_name (const char * basename, int infunc, tree func_decl)
{
  static char *infunc_glob_prefix = NULL; 
  static int   prefix_len;
  static char *text = NULL;
  static int   textsize = BUFSIZ;

  int fname_len;
  int sname_len;

  if (text == NULL)
    text = xmalloc (textsize + 1);

  if (infunc_glob_prefix == NULL) 
    {
      prefix_len = strlen (ir_global_prefix);
      infunc_glob_prefix = (char *) xmalloc (prefix_len + 1);
      strcpy (infunc_glob_prefix, ir_global_prefix);
      infunc_glob_prefix[2] = 'B'; /* to make dbx happy ? */
    }
  
  if (!infunc)
    {
      sname_len = strlen (basename);
      if (textsize < prefix_len + sname_len) 
        {
          textsize = prefix_len + sname_len;
          text = (char *) xrealloc (text, textsize + 1);
        }
      strcpy (text, ir_global_prefix);
      strcpy (text + prefix_len, basename);
    }
  else
    {
      const char * func_name;
      sname_len = strlen (basename);
      gcc_assert (func_decl && TREE_CODE (func_decl) == FUNCTION_DECL);
      
      func_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (func_decl));
      func_name = (* targetm.strip_name_encoding) (func_name);
      fname_len = strlen (func_name);

      if (textsize < prefix_len + sname_len + fname_len + 1) 
        {
          textsize = prefix_len + sname_len + fname_len + 1;
          text = (char *) xrealloc (text, textsize + 1);
        }
      strcpy (text, infunc_glob_prefix);
      strcpy (text + prefix_len, func_name);
      *(text + prefix_len + fname_len) = '.';
      strcpy (text + prefix_len + fname_len + 1, basename);
    }

  return text;
}

static int
is_large_vector_type (tree type)
{
  return TREE_CODE (type) == VECTOR_TYPE && TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type)) > 32;
}

/* Hacky way to establish if the pointer type is a
   pointer to the virtual table. Assume required
   check for language type and the type of the
   parameter being a pointer type are already done. */
static int
is_virtual_pointer_type (tree type)
{
    const char *name = get_type_name (type);
    if (name != NULL
        && strcmp (name, "__vtbl_ptr_type") == 0)
      return 1;
    return 0;
}

static IR_NODE *
dump_string_cst (tree stmt, enum MAP_FOR map_for)
{
  IR_NODE * ret = 0;
  TYPE ary_t = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
  TYPE ptr_t = map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (stmt)));
  IR_TYPE_NODE *ir_ary_t = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
  IR_TYPE_NODE *ir_ptr_t = map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (TREE_TYPE (stmt)));

  rtx rtl = output_constant_def (stmt, 0/* don't defer asm output */);
  const char *label = XSTR (XEXP (rtl, 0), 0);
  
  int size_in_rtl = int_size_in_bytes (TREE_TYPE (SYMBOL_REF_DECL (XEXP (rtl, 0))));

  /* if string type requests to assign more characters than present in rtl,
     cut it to rtl_size. dump_ir_modify() will add extra memset() in this case */
  if (ary_t.size > size_in_rtl)
    ary_t.size = size_in_rtl;
  
  ret = build_ir_string_var (targetm.strip_name_encoding (label), ary_t, ir_ary_t);

  ret->leaf.is_const = IR_TRUE;
  
  if (map_for == MAP_FOR_ADDR)
    {
      ret = build_ir_addr_const (ret, (IR_OFFSZ) 0,  ptr_t, ir_ptr_t);
  
      /* read only strings. flag_writable_strings is no longer supported by GCC*/
      if (!flag_writable_strings)
        ret->leaf.val.addr.seg->readonly = IR_TRUE;
    }
  return ret;
}

/* dump gimplified constructor. should have only zeros */
static IR_NODE *
dump_ir_constructor (tree stmt, enum MAP_FOR map_for)
{
  TYPE argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
  IR_TYPE_NODE *ir_argtype = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
  IR_NODE * ret = 0;

  if (TREE_CODE (TREE_TYPE (stmt)) != VECTOR_TYPE)
    {
      /* ok for non vector_cst ??? */
      rtx rtl = output_constant_def (stmt, 0/* don't defer asm output */);
      const char *label = XSTR (XEXP (rtl, 0), 0);
      ret = build_ir_string_var (targetm.strip_name_encoding (label), argtype, ir_argtype);
      
      ret->leaf.is_const = IR_TRUE;
      
      if (map_for == MAP_FOR_ADDR)
        {
          TYPE ptr_t = map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (stmt)));
          IR_TYPE_NODE *ir_ptr_t = map_gnu_type_to_IR_TYPE_NODE (
                                       build_pointer_type (TREE_TYPE (stmt)));
          ret = build_ir_addr_const (ret, (IR_OFFSZ) 0,  ptr_t, ir_ptr_t);
      
          /* read only constructor initializer */
          ret->leaf.val.addr.seg->readonly = IR_TRUE;
        }
    } 
  else 
    {
      int align = argtype.align * BITS_PER_UNIT;
      char label[16];
      const char * label_p;
      int reloc;
      int labelno;
      
      labelno = internal_labelno ++; 
      
      snprintf (label, sizeof (label), ".L%d", labelno);
      label_p = label;
      
      if (globalize_flag)
        label_p = make_global_name (label, cfun ? 1 : 0, current_function_decl);
      
      ret = build_ir_string_var (label_p, argtype, ir_argtype);
      
      ret->leaf.is_const = IR_TRUE;
      
      if (map_for == MAP_FOR_ADDR)
        {
          TYPE ptr_t = map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (stmt)));
          IR_TYPE_NODE *ir_ptr_t = map_gnu_type_to_IR_TYPE_NODE (
                                       build_pointer_type (TREE_TYPE (stmt)));
          ret = build_ir_addr_const (ret, (IR_OFFSZ) 0,  ptr_t, ir_ptr_t);
      
          /* read only constructor initializer */
          ret->leaf.val.addr.seg->readonly = IR_TRUE;
        }

      #ifdef CONSTANT_ALIGNMENT
      align = CONSTANT_ALIGNMENT (stmt, align);
      #endif
      
      reloc = compute_reloc_for_constant (stmt);
      switch_to_section (targetm.asm_out.select_section (stmt, reloc, align));
      
      if (align > BITS_PER_UNIT)
        {
          ASM_OUTPUT_ALIGN (asm_out_file, floor_log2 (align / BITS_PER_UNIT));
        }
        
      if (globalize_flag)
        targetm.asm_out.globalize_label (asm_out_file, label_p);

      /* Standard thing is just output label for the constant.  */
      ASM_OUTPUT_LABEL (asm_out_file, label_p);
      
      /* Output the value of constructor. Only zeros? */
      output_constant (stmt, argtype.size, align);
    }
  return ret;
}

/* The "standard" implementation of va_arg: read the value from the
   current (padded) address and increment by the (padded) size.  */

static IR_NODE *
dump_ir_builtin_va_arg (tree valist, tree type)
{
  HOST_WIDE_INT size, rsize, align;
  tree addr, incr, t;
  int indirect = 0;
  IR_NODE * ret = 0;

  /* Round up sizeof(type) to a word.  */
  size = int_size_in_bytes (type);
  rsize = (size + UNITS_PER_WORD - 1) & -UNITS_PER_WORD;
  align = 0;

  abort ();
  if (TARGET_ARCH64)
    {
    }
  else
    {
      if (AGGREGATE_TYPE_P (type)
          || TYPE_MODE (type) == SCmode 
          || is_large_vector_type (type) || GET_MODE_SIZE (TYPE_MODE (type)) > 8)
	{
	  indirect = 1;
	  size = rsize = UNITS_PER_WORD;
	}
    }

  incr = valist;
  if (align)
    {
      incr = fold (build2 (PLUS_EXPR, ptr_type_node, incr, build_int_cst (NULL_TREE, align - 1)));
      incr = fold (build2 (BIT_AND_EXPR, ptr_type_node, incr, build_int_cst (NULL_TREE,-align)));
    }

  addr = incr;

  incr = fold (build2 (PLUS_EXPR, ptr_type_node, incr, build_int_cst (NULL_TREE, rsize)));

  incr = build2 (GIMPLE_MODIFY_STMT, ptr_type_node, valist, incr);
  TREE_SIDE_EFFECTS (incr) = 1;

  dump_ir_stmt (incr);

  /* BYTES_BIG_ENDIAN only */
  addr = fold (build2 (MINUS_EXPR, ptr_type_node, addr, build_int_cst (NULL_TREE, size)));

  if (indirect && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
    {
      addr = build1 (INDIRECT_REF, build_pointer_type (type), 
                     build1 (NOP_EXPR, build_pointer_type (build_pointer_type (type)), addr));
    }
  
  t = build1 (INDIRECT_REF, type, build1 (NOP_EXPR, build_pointer_type (type), addr
                                          /*build2 (MINUS_EXPR, TREE_TYPE (valist), valist,
                                                 rounded_size)*/ /*addr_tree*/));
  
  ret = dump_ir_expr (t, MAP_FOR_VALUE);
  ret->triple.no_dalign = IR_TRUE;
  return ret;
}

const char *
get_ir_name (tree stmt)
{
  const char * name;

  if (DECL_NAME (stmt) || DECL_EXTERNAL (stmt) || TREE_PUBLIC (stmt) || TREE_STATIC (stmt)
      || TREE_CODE (stmt) == TYPE_DECL)
    {
      if (TREE_PUBLIC (stmt) || TREE_STATIC (stmt) || DECL_EXTERNAL (stmt)
          || TREE_CODE (stmt) == TYPE_DECL)
        {
          /* asm name for external vars */
          name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (stmt));
          name = (* targetm.strip_name_encoding) (name);
        }
      else
        /* regular name for automatic vars */
        name = IDENTIFIER_POINTER (DECL_NAME (stmt)); 
    }
  else
    {
      /* = ""; no NULL here! iropt's IPPT aborts when function param has NULL pass1_id name */
      /* TODO fix ippt */
      name = NULL; 
    }

  return name;
}

const char *
get_ir_field_name (tree stmt, int offset, char * fld_name_buf)
{
  const char * name;

  if (DECL_NAME (stmt))
    {
      /* known name of the field */
      name = IDENTIFIER_POINTER (DECL_NAME (stmt)); 
    }
  else
    { /* parent type reference by child class in C++ or unnamed bitfield*/
      const char * fmt = ".unknown.field.offset%u";
      snprintf (fld_name_buf, 64, fmt, offset);
      name = fld_name_buf;
    }

  return name;
}

/*static void
globalize_if_needed (tree stmt)
{
  if (!TREE_PUBLIC (stmt) && globalize_flag && !DECL_ARTIFICIAL (stmt))
    {
      TREE_PUBLIC (stmt) = 1;
      TREE_STATIC (stmt) = 0;
    }
}*/

/* Return true if TYPE should be passed by invisible reference. Additional check with transparent union. */

bool
tu_pass_by_reference (CUMULATIVE_ARGS *ca, enum machine_mode mode,
                   tree type, bool named_arg)
{
  if (type && TREE_CODE (type) == UNION_TYPE && TYPE_TRANSPARENT_UNION (type))
    {
       type = TREE_TYPE (TYPE_FIELDS (type));
       mode = TYPE_MODE (type);
    }
  return pass_by_reference (ca, mode, type, named_arg);
}

/* Accmulate the offset in a structure field array. */
static IR_NODE * gir_offset = NULL;

/* The default value is -1. 
   When it is in a component_ref which is not in the structure field 
   array access, it should be 0.
   When the tree is in a COMPONENT_REF/ARRAY_REF which is in the 
   structure field array access, it should be 1. */
static int is_field_array = -1;

/* Recode the outest tree node in a structure field array. */
static tree outer_tree;

/* Recode the lowest level field's type size. */
static int g_esize;

/* compute the whole offset in a struture field array. */
static void
compute_goffset (tree stmt)
{
  tree t, op0, op1;
  CONSZ offset, offset_bits;
  IR_NODE *ir_offset;
  t = stmt;
  do
    {
      switch (TREE_CODE (t))
        {
	case COMPONENT_REF:
	  op0 = TREE_OPERAND (t, 0);
	  if ( TREE_CODE (op0) == VAR_DECL
		|| (TREE_CODE (op0) == PARM_DECL
		  && !tu_pass_by_reference (NULL, TYPE_MODE (TREE_TYPE (op0)),
                                          TREE_TYPE (op0), false)))
	    /* later we will genetate a new leaf for the field. 
	       so do not accumulate this offset. */
	    break;
	  if (TREE_CODE (op0) == INDIRECT_REF && TREE_CODE (TREE_TYPE (op0)) == RECORD_TYPE)
	    /* this format is enabled by -fgen-more-array.
	       net->arcs[3*i-1].cost 181.mcf/src/readmin.c : line 176.
	       later will generate a ifetch for it. 
	       so do not accumulate this offset. */
	    break;

	  op1 = TREE_OPERAND (t, 1);
	  offset_bits = TREE_INT_CST_LOW (DECL_FIELD_OFFSET (op1)) * BITS_PER_UNIT
			+ TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (op1));
	  offset = offset_bits / BITS_PER_UNIT;
	  ir_offset = build_ir_int_const (offset, inttype, 0);
          if (gir_offset == NULL)
            gir_offset = ir_offset;
          else
            gir_offset = build_ir_triple (IR_PLUS, gir_offset, ir_offset, inttype, 0);
	  break;
	case ARRAY_REF:
	  break;
	default:
	  gcc_unreachable();
        }
      t = TREE_OPERAND (t, 0);
  } while (TREE_CODE (t) != VAR_DECL && TREE_CODE (t) != PARM_DECL
		&& TREE_CODE (t) != INDIRECT_REF);
  return;
}

static IR_TYPE_NODE *
compute_field_array_ir_type (tree stmt)
{
  tree op0, op1;
  IR_TYPE_NODE * ret, *ir_type;

  op0 = TREE_OPERAND (stmt, 0);
  op1 = TREE_OPERAND (stmt, 1);  
  switch (TREE_CODE (stmt))
    {
    case COMPONENT_REF:
      {
	tree aligned_offset = TREE_OPERAND (stmt, 2);
	CONSZ offset_bits;
	const char * fld_name;
	char fld_name_buf[64];
	IR_NODE * ir_offset;

        /* If an offset was specified in the COMPONENT_REF, it's the offset measured
           in units of DECL_OFFSET_ALIGN / BITS_PER_UNIT.  So multiply by that
value. */
        if (aligned_offset)
          {
	    /* Todo. need to handle this case in compute_goffset(). */
	    gcc_unreachable(); 
          }
        else if (TREE_CODE (DECL_FIELD_OFFSET (op1)) == INTEGER_CST)
            /* regular struct of known size */
          {
            /* offset in bits */
            offset_bits = TREE_INT_CST_LOW (DECL_FIELD_OFFSET (op1)) * BITS_PER_UNIT
                          + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (op1));
            ir_offset = 0;
          }
        else /* variable length struct */
	  gcc_unreachable();

        fld_name = get_ir_field_name (op1, offset_bits, fld_name_buf);

        if (TREE_CODE (op0) == VAR_DECL || TREE_CODE (op0) == PARM_DECL)
  	  {
	    IR_TYPE_NODE * sue_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0));
	    ir_type = ir_suembr2 (sue_ir_type, fld_name);
            if (ir_type == 0) /* _vptr.* case? */
              {
                if (op1)
                  ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op1));
                else
                  ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
              }
	    ret = ir_type;
	  }
        else if (TREE_CODE (op0) == INDIRECT_REF )
	  {
	    IR_TYPE_NODE * sue_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0));
            ret = ir_suembr2 (sue_ir_type, fld_name);
            if (ret == 0)
              ret = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
          }
        else if ( TREE_CODE (op0) == ARRAY_REF
		  || TREE_CODE (op0) == COMPONENT_REF)
	  {
	    ir_type = compute_field_array_ir_type (op0);
	    ret = ir_suembr2 (ir_type, fld_name);
	    if (ret == 0)
              ret = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
	  }
        else if ( TREE_CODE (op0) == RESULT_DECL)
	  gcc_unreachable();/* l'd like to see when it will generate. */
      }
      break;
    case ARRAY_REF:
      {
	if (TREE_CODE (op0) == ARRAY_REF || TREE_CODE (op0) == COMPONENT_REF)
	  {
	    ir_type = compute_field_array_ir_type(op0);
	    ret = ir_decref (ir_type);
	  }
	else
	  {
	    ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
	    ret = ir_type;
	  }
      }
      break;
    default:
      gcc_unreachable();
    }
  return ret;
}

static IR_NODE *
dump_ir_component_ref (tree stmt, tree op0, tree op1, const char * fld_name, int offset, 
                       tree stmt_type, TYPE argtype, enum MAP_FOR map_for, IR_NODE * ir_offset)
{
  IR_NODE * ret;
  int saved_gesize ;  /* saved the current status. restore when back from subindex. */
  tree saved_outer_tree = NULL ;
  IR_NODE * saved_offset;


  if (is_field_array != 1 && flag_field_array 
      && TREE_CODE (stmt) == COMPONENT_REF) 
    {
      is_field_array = check_field_array (op0, 1);
      outer_tree = stmt;
      if (is_field_array == 1)
	{
	  /* record the element size. used as ntuple_array[1] */
	  g_esize = TREE_INT_CST_LOW (TYPE_SIZE (stmt_type))/ BITS_PER_UNIT;

	  /* recode the initial offset */
	  gcc_assert ( gir_offset == 0 );
          if (ir_offset)
	    gir_offset = ir_offset;
          else
	    gir_offset = build_ir_int_const (offset, inttype, 0);

	  /* compute the whole offset */
	  compute_goffset (op0);
        }
    }

  if (TREE_CODE (op0) == VAR_DECL || TREE_CODE (op0) == PARM_DECL)
    {
      const char * name;
      IR_TYPE_NODE * ir_type, *ptr_ir_type;
      LEAF * ir_parent_struct;
      
      TYPE sue_type = map_gnu_type_to_TYPE (TREE_TYPE (op0));
      IR_TYPE_NODE * sue_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0));
      name = get_ir_name (op0);
      ir_type = ir_suembr2 (sue_ir_type, fld_name); /* IR_TYPE_NODE for given field op1*/
      if (ir_type == 0) /* _vptr.* case? */
        {
          if (op1)
            {
              ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op1));
              argtype = map_gnu_type_to_TYPE (TREE_TYPE (op1));
            }
          else
            {
              ir_type = map_gnu_type_to_IR_TYPE_NODE (stmt_type);
              argtype = map_gnu_type_to_TYPE (stmt_type);
            }
        }
      ptr_ir_type = make_ptr_to_ir_type_node(ir_type);
      
      ir_parent_struct = (LEAF*) dump_ir_expr (op0, TREE_CODE (op0) == VAR_DECL 
                                                    ? MAP_FOR_VALUE : MAP_FOR_ADDR);

      if (ir_parent_struct->tag != ISLEAF)
        abort ();

      /* variable length struct */
      if (TYPE_SIZE (TREE_TYPE (op0))
          && TREE_CODE (TYPE_SIZE (TREE_TYPE (op0))) != INTEGER_CST)
        {
          tree t = build_pointer_type (stmt_type);
          TYPE ptr_argtype = map_gnu_type_to_TYPE (t);
          
          ret = (IR_NODE*)ir_parent_struct;
          ret = build_ir_triple (IR_PCONV, ret, NULL, ptr_argtype, 
                            map_gnu_type_to_IR_TYPE_NODE (t));
          
          /* variable length structs are always pointers. need to decref once
           * ot get to struct information */
          ir_type = ir_suembr2 (sue_ir_type, fld_name);
          ptr_ir_type = make_ptr_to_ir_type_node(ir_type);

          if (ir_offset)
            ret = build_ir_triple (IR_PLUS, ret, ir_offset, ptr_argtype, ptr_ir_type);

          ret = build_ir_triple (IR_PLUS, ret, build_ir_int_const (offset, offsettype, 0), 
                            ptr_argtype, ptr_ir_type);
          
          ret->triple.ldst_ir_type = ir_type;
          ret->triple.ldst_type = argtype;
          
          if (map_for == MAP_FOR_VALUE)
            ret = build_ir_triple (IR_IFETCH, ret, NULL, 
                              argtype /* type of given field op1*/, 
                              ir_type /* IR_TYPE_NODE for given field op1*/);
        }
      
      /* regular struct with known const int size */
      else if (TREE_CODE (op0) == VAR_DECL
               || (TREE_CODE (op0) == PARM_DECL 
                   && !tu_pass_by_reference (NULL, TYPE_MODE (TREE_TYPE (op0)), 
                                          TREE_TYPE (op0), false)))
        {
          if (TREE_CODE (op0) == PARM_DECL && !TARGET_ARCH64)
            {
              /* because of overlap in arg_seg, iropt will not allocate 
                 these vars in registers and cg will keep them in stack.
                 bad performance.
                 probably need to remember one temp var, store param into it
                 and create overlaps out of one temp var
                 TODO */
              /* component_ref/bit_field_ref to access PARM_DECL not passed by reference
                 that can only be float/double types that represent vector types */
              ret = build_ir_parm_var (name,
                                       lookup_param_offset (op0, ir_parent_struct->type) + offset, 
                                       argtype, ir_type);
            }
          else if (DECL_REGISTER (op0) && DECL_ASSEMBLER_NAME_SET_P (op0)
            && DECL_NAME (op0))
            {
              /* register name for redefined register vars */
              const char *regname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (op0));

              int regno = decode_reg_name (++ regname);

              if (regno <= IR_REG_I7)
                {
                   if (PCC_ISFLOATING (argtype.tword))
                     error ("register %qs not expected for floating-type field member %qs.%qs",
                              regname, IDENTIFIER_POINTER (DECL_NAME (op0)), IDENTIFIER_POINTER (DECL_NAME (op1))); 
                }
              else if (PCC_ISINTEGRAL (argtype.tword)
                       && (argtype.tword != PCC_INT) && (argtype.tword != PCC_UNSIGNED))
                {
                   error ("register %qs not expected for integral-type field member %qs.%qs",
                            regname, IDENTIFIER_POINTER (DECL_NAME (op0)), IDENTIFIER_POINTER (DECL_NAME (op1))); 
                }

              ret = build_ir_reg_var (regname, regno, argtype, ir_type);
              ret->leaf.is_volatile = IR_TRUE;

              tree tp = lang_hooks.decls.omp_threadprivate_decl(op0);
              if (tp != NULL_TREE)
                {
                  ret->leaf.in_taskcommon_block = IR_TRUE;
                  /* Associate the segments */
                  LEAF *tpleaf = (LEAF *) dump_ir_expr (tp, MAP_FOR_VALUE);
                  gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
                            IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
                }
            }
          else if (TREE_PUBLIC (op0) || TREE_STATIC (op0) || DECL_EXTERNAL (op0))
            {
              ret = build_ir_extern_var (op0 /*VAR_DECL*/, offset /*off*/, 
                                         argtype, ir_type);
              /* pre-gcc 3.5.x style: */  
              {tree id = maybe_get_identifier (name); if (id) mark_referenced (id);}
              /* gcc 3.5.x style of marking used names */
              mark_decl_referenced (op0); 

              /* If field name is available, use it. Makes IR debugging
                 easier */
              if (fld_name)
                ret->leaf.pass1_id = build_ir_proc_string (fld_name);
            }
          else
            {
              ret = build_ir_auto_var (name, lookup_auto_offset (op0, sue_type) /*segoff*/,
                               sue_type.size /*seglen*/,
                               sue_type.align /*segalign*/, offset /*off*/, 
                               argtype, ir_type);

              SCOPE_TEMP_AS_PRIVATE_IFNEEDED(op0, name, ret);
            }

          if (TREE_THIS_VOLATILE (op0))
            ret->leaf.is_volatile = IR_TRUE;

          tree tp = lang_hooks.decls.omp_threadprivate_decl(op0);
          if (tp != NULL_TREE)
            {
              ret->leaf.in_taskcommon_block = IR_TRUE;
              /* Associate the segments */
              LEAF *tpleaf = (LEAF *) dump_ir_expr (tp, MAP_FOR_VALUE);
              gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
                          IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
            }
          
          /* cleanup: now it's done in adjust_leaf_overlaps()
          ret->leaf.elvarno = ir_parent_struct->elvarno; */
          /* cleanup: now it's done in build_ir_leaf() 
          set_leaf_pointerno (ret, argtype.tword); */
          
          if ((IS_ARRAY_TYPE (stmt_type) || IS_RECORD_TYPE (stmt_type)) 
              && map_for == MAP_FOR_ADDR)
            {
              tree t = build_pointer_type (stmt_type);
              argtype = map_gnu_type_to_TYPE (t);
              
              /* This leave cannot reside in a register */
              /* cleanup: now it's done in adjust_leaf_overlaps()
              ret->leaf.no_reg = IR_TRUE;
              if (ret->leaf.elvarno == -1)
                ret->leaf.elvarno = naliases++; */
               
              /* C++ style addr_const */
              if (ret->leaf.typep)
                ret = build_ir_addr_const (ret, 0, argtype,
                                           make_ptr_to_ir_type_node (ret->leaf.typep));
              else
                ret = build_ir_addr_const (ret, 0, argtype,
                                           map_gnu_type_to_IR_TYPE_NODE (t));
              /* C style addr_const */
              /* ret = build_ir_addr_const ((IR_NODE*)ir_parent_struct, 
                              offset, argtype, map_gnu_type_to_IR_TYPE_NODE (t)); */
            }
        }

      /* regular struct passed as parameter */
      else /* PARM_DECL */
        {
          ret = build_ir_parm_var (name, 
                              lookup_param_offset (op0, ir_parent_struct->type), 
                              ir_parent_struct->type, ir_parent_struct->typep);

          if (TREE_THIS_VOLATILE (op0))
            ret->leaf.is_volatile = IR_TRUE;

#ifdef FIX_MISMATCH_WARNING
          /* to get rid of "mismatch" warning in iropt */
          ret = build_ir_triple (IR_PCONV, ret, NULL, ir_parent_struct->type, 
                            ir_parent_struct->typep);
#endif

          if (is_field_array != 1)
                      ret = build_ir_triple (IR_PLUS, ret, build_ir_int_const (offset, offsettype, 0), 
                                      ir_parent_struct->type, ptr_ir_type);
          else /* Workaround for 186:utility.c:239 pos.board[i] */
            ret = build_ir_triple (IR_PLUS, ret, build_ir_int_const (0, offsettype, 0), ir_parent_struct->type, ptr_ir_type);
          
          ret->triple.ldst_ir_type = ir_type;
          ret->triple.ldst_type = argtype;
          
          if (map_for == MAP_FOR_VALUE)
            ret = build_ir_triple (IR_IFETCH, ret, NULL, 
                              argtype /* type of given field op1*/, 
                              ir_type /* IR_TYPE_NODE for given field op1*/);
        }
        
    }
  else if (TREE_CODE (op0) == INDIRECT_REF || TREE_CODE (op0) == ARRAY_REF)
    {
      IR_NODE * ir_op0 = 0;
      IR_TYPE_NODE * ir_type, * ptr_ir_type;
      
      IR_TYPE_NODE * sue_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0));
      tree ptr_stmt_type = build_pointer_type (stmt_type);
      
      /* variable length struct */
      if (TYPE_SIZE (TREE_TYPE (op0))
          && TREE_CODE (TYPE_SIZE (TREE_TYPE (op0))) != INTEGER_CST)
        {
          /* variable length structs are alwasy pointers. need to decref once
             ot get to struct information */
          /* prior to gcc 4.x needed to dereference sue_ir_type once */
          ir_type = ir_suembr2 (sue_ir_type, fld_name);
        }
      else
        {
          ir_type = ir_suembr2 (sue_ir_type, fld_name);
          if (ir_type == 0)
            ir_type = map_gnu_type_to_IR_TYPE_NODE (stmt_type);
        }

      /* For nested darrayloc.*/
      if (is_field_array == 1 && TREE_CODE (op0) == INDIRECT_REF )
        {
          is_field_array = -1;
          saved_outer_tree = outer_tree;
          saved_gesize = g_esize;
          saved_offset = gir_offset;
          gir_offset = NULL;
        }
      
      ir_op0 = dump_ir_expr (op0, MAP_FOR_ADDR);

      /* recover the darrrayloc status. */
      if (saved_outer_tree != NULL)
        {
          gcc_assert (TREE_CODE (op0) == INDIRECT_REF);
          is_field_array = 1;
          outer_tree = saved_outer_tree;
          g_esize = saved_gesize;
          gir_offset = saved_offset;
          saved_outer_tree = NULL;
        }

      if (ir_op0->operand.tag == ISTRIPLE && ir_op0->triple.ldst_ir_type)
        {
          IR_TYPE_NODE * t = ir_suembr2 (ir_op0->triple.ldst_ir_type, fld_name);
          if (t)
            ir_type = t;
        }
      ptr_ir_type = make_ptr_to_ir_type_node(ir_type);

#ifdef FIX_MISMATCH_WARNING
      /* to get rid of "mismatch" warning in iropt */
      ir_op0 = build_ir_triple (IR_PCONV, ir_op0, NULL, ir_tword1 (ptr_t1word),
                           ir_get_type (ptr_t1word, 0, 0));
#endif           

/*#define IROPT_BUG_FIX*/

#ifdef IROPT_BUG_FIX_2
      if (offset == 0)
        ret = build_ir_triple (IR_CONV, ir_op0, NULL, ir_tword1 (ptr_t1word), 
                          ir_get_type (ptr_t1word, 0, 0));
      else
#elif defined(IROPT_BUG_FIX)
      if (offset == 0 && ir_op0->operand.tag == ISTRIPLE 
          && ir_op0->triple.op == IR_ARRAYLOC)
        ret = ir_op0;
      else
#endif
        if (is_field_array == 1 )
	  {
	    if (TREE_CODE (op0) == ARRAY_REF && ir_op0 ->operand.tag == ISTRIPLE
		  && ir_op0->triple.op == IR_DARRAYLOC)
	      ret = ir_op0;
	    if (TREE_CODE (op0) == INDIRECT_REF)
	      {
		IR_TYPE_NODE * ir_type = compute_field_array_ir_type(outer_tree);
                IR_TYPE_NODE * ptr_ir_type = make_ptr_to_ir_type_node(ir_type);

		if (TREE_CODE (TREE_TYPE (op0)) == RECORD_TYPE)
		  ret = build_ir_triple (IR_PLUS, ir_op0, build_ir_int_const (offset, offsettype, 0),
				map_gnu_type_to_TYPE (ptr_stmt_type), ptr_ir_type);
		else
		  {
		    gcc_assert (gir_offset != NULL);
		    ret = build_ir_triple (IR_PLUS, ir_op0, gir_offset, map_gnu_type_to_TYPE (ptr_stmt_type), ptr_ir_type);
		    gir_offset = NULL;
		  }
	      }
	  }
	else
	  {
            ret = build_ir_triple (IR_PLUS, ir_op0, build_ir_int_const (offset, offsettype, 0), 
                          map_gnu_type_to_TYPE (ptr_stmt_type), ptr_ir_type);
            if (ir_offset)
              ret = build_ir_triple (IR_PLUS, ret, ir_offset, map_gnu_type_to_TYPE (ptr_stmt_type), ptr_ir_type);
	  }
 
      ret->triple.ldst_ir_type = ir_type;
      ret->triple.ldst_type = argtype;
      
      if (map_for == MAP_FOR_VALUE)
        ret = build_ir_triple (IR_IFETCH, ret, NULL, argtype, ir_type);
    }
  else if (TREE_CODE (op0) == COMPONENT_REF || TREE_CODE (op0) == RESULT_DECL
           /*|| TREE_CODE (op0) == VAR_DECL*/)
    {
      IR_NODE * ir_op0 = 0;
      IR_TYPE_NODE * ir_type, * ptr_ir_type;
      ir_ADDRESS addr;
      
      /* variable length struct */
      if (TYPE_SIZE (TREE_TYPE (op0))
          && TREE_CODE (TYPE_SIZE (TREE_TYPE (op0))) != INTEGER_CST)
        abort (); /* not supported here */

      ir_op0 = dump_ir_expr (op0, MAP_FOR_ADDR);
          
      if (ir_op0->operand.tag == ISLEAF 
          && (ir_op0->leaf.class == ADDR_CONST_LEAF
              || (ir_op0->leaf.class == VAR_LEAF
                  && (TARGET_ARCH64 || TREE_CODE (TREE_TYPE (op0)) == VECTOR_TYPE))))
        {
          LEAF * leaf_struct = ir_op0->leaf.class == ADDR_CONST_LEAF 
                               ? ir_op0->leaf.addressed_leaf : &ir_op0->leaf;
          ir_type = ir_suembr2 (leaf_struct->typep, fld_name); /* ir_type of field */
          
          if (ir_type == 0)
            ir_type = map_gnu_type_to_IR_TYPE_NODE (stmt_type);

          /* C++ style addr_const */
          addr = leaf_struct->val.addr;
          addr.offset += offset;

	 /* Fix build error. expmed.c:152(((&all.reg)->u.fld[0]).rt_uint)
	    (&all.reg)->u is generate as ir leaf. So we have to minus the
	    offset from gir_offset.*/
	  if (is_field_array == 1 && offset != 0)
	    {
	      gir_offset = build_ir_triple (IR_MINUS, gir_offset, build_ir_int_const (offset, inttype, 0), inttype, 0);
	    }
          
          ret = (IR_NODE*) build_ir_leaf (VAR_LEAF, argtype, ir_type, 
                                        (LEAF_VALUE *)&addr, IR_TRUE);

          ret->leaf.pass1_id = build_ir_proc_string (leaf_struct->pass1_id);
          ret->leaf.visited = IR_FALSE;
          if (leaf_struct->in_taskcommon_block)
            ret->leaf.in_taskcommon_block = IR_TRUE;
              
          if ((IS_ARRAY_TYPE (stmt_type) || IS_RECORD_TYPE (stmt_type)) 
              && map_for == MAP_FOR_ADDR)
            {
              tree t = build_pointer_type (stmt_type);
              argtype = map_gnu_type_to_TYPE (t);
              
              /* C++ style addr_const */
              if (ret->leaf.typep)
                ret = build_ir_addr_const (ret, 0, argtype, 
                                           make_ptr_to_ir_type_node (ret->leaf.typep));
              else
                ret = build_ir_addr_const (ret, 0, argtype, map_gnu_type_to_IR_TYPE_NODE (t));
            }
        }
      else if (ir_op0->operand.tag == ISTRIPLE && (ir_op0->triple.op == IR_PLUS
		|| (is_field_array == 1 && ir_op0->triple.op == IR_DARRAYLOC)
#ifdef IROPT_BUG_FIX
               || ir_op0->triple.op == IR_ARRAYLOC
#endif
               ))
        {
          tree ptr_stmt_type  = build_pointer_type (stmt_type);
          ir_type = ir_suembr2 (ir_op0->triple.ldst_ir_type, fld_name);

          if (ir_type == 0)
            ir_type = map_gnu_type_to_IR_TYPE_NODE (stmt_type);
          ptr_ir_type = make_ptr_to_ir_type_node(ir_type);

#ifdef FIX_MISMATCH_WARNING
          /* to get rid of "mismatch" warning in iropt */
          ir_op0 = build_ir_triple (IR_PCONV, ir_op0, NULL, 
                               map_gnu_type_to_TYPE (ptr_stmt_type),
                               map_gnu_type_to_IR_TYPE_NODE (ptr_stmt_type));
#endif

	  if (is_field_array == 1)
	    ret = ir_op0;
	  else
	    {
              ret = build_ir_triple (IR_PLUS, ir_op0, build_ir_int_const (offset, offsettype, 0), 
                            map_gnu_type_to_TYPE (ptr_stmt_type), ptr_ir_type);
          
              if (ir_offset)
                ret = build_ir_triple (IR_PLUS, ret, ir_offset,
                              map_gnu_type_to_TYPE (ptr_stmt_type), ptr_ir_type);
	    }

          ret->triple.ldst_ir_type = ir_type;
          ret->triple.ldst_type = argtype;
            
          if (map_for == MAP_FOR_VALUE)
            ret = build_ir_triple (IR_IFETCH, ret, NULL, argtype, ir_type);
        }
      else 
        abort ();
    }
  else if (TREE_CODE (op0) == VECTOR_CST) /* now I don't modify this part for darrayloc */
    {
      tree ptr_stmt_type  = build_pointer_type (stmt_type);
      ret = dump_ir_expr (op0, MAP_FOR_ADDR);

      if (is_field_array != 1)
        ret = build_ir_triple (IR_PLUS, ret, build_ir_int_const (offset, offsettype, 0), 
                        map_gnu_type_to_TYPE (ptr_stmt_type), 0);
      if (map_for != MAP_FOR_VALUE || fld_name || op1)
        abort ();
      
      ret = build_ir_triple (IR_IFETCH, ret, NULL, argtype,
                        map_gnu_type_to_IR_TYPE_NODE (stmt_type));
    }
  else
    abort ();

  if (is_field_array != -1 && outer_tree == stmt)
    is_field_array = -1;

  if (map_for == MAP_FOR_VALUE
      && ret->operand.tag == ISTRIPLE
      && ir_language == CDOUBLEPLUS
      && TREE_TYPE (stmt_type)
      && TREE_CODE (TREE_TYPE (stmt_type)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (stmt_type))) == FUNCTION_TYPE
      && is_virtual_pointer_type (TREE_TYPE (stmt_type)))
    ret->triple.access_info = cpp_vtptr_ref;
  
  return ret;
}

static IR_NODE *
create_ir_exception_filter (void)
{
  return get_tmp_leaf_with_name (map_gnu_type_to_TYPE (intSI_type_node),
                                 map_gnu_type_to_IR_TYPE_NODE (intSI_type_node), 
                                 "#tmp.eh.filter_expr");
}

IR_NODE *
get_ir_exception_filter (void)
{
  if (!cfun_eh_filter)
    cfun_eh_filter = create_ir_exception_filter ();
  return cfun_eh_filter;
}

static IR_NODE *
create_ir_exception_pointer (void)
{
  return get_tmp_leaf_with_name (map_gnu_type_to_TYPE (ptr_type_node),
                                 map_gnu_type_to_IR_TYPE_NODE (ptr_type_node), 
                                 "#tmp.eh.exc_ptr_expr");
}

IR_NODE *
get_ir_exception_pointer (void)
{
  if (!cfun_eh_exc_ptr)
    cfun_eh_exc_ptr = create_ir_exception_pointer ();
  return cfun_eh_exc_ptr;
}

struct pinfo_list
{
  PRAGMAINFO* data;
  struct pinfo_list *next;
};
struct pinfo_list *plist_cur, *plist_head, *plist_prev;

void
save_eh_registers (void)
{
  TYPE type;
  IR_TYPE_NODE *typep;
  IR_NODE *reg_i0, *reg_i1, *assign;
  /* save %i0 for EXC_PTR_EXPR <<<exception object>>> */
  type = map_gnu_type_to_TYPE (ptr_type_node);
  typep = map_gnu_type_to_IR_TYPE_NODE (ptr_type_node);
  reg_i0 = build_ir_reg_var ("%i0", IR_REG_I0, type, typep),
  reg_i0->leaf.is_volatile = IR_TRUE;
  assign = build_ir_triple (IR_ASSIGN, get_ir_exception_pointer (), 
                            reg_i0, type, NULL);
  /* save %i1 for FILTER_EXPR <<<filter object>>> */
  typep = map_gnu_type_to_IR_TYPE_NODE (intSI_type_node);
  reg_i1 = build_ir_reg_var ("%i1", IR_REG_I1, inttype, typep),
  reg_i1->leaf.is_volatile = IR_TRUE;
  assign = build_ir_triple (IR_ASSIGN, get_ir_exception_filter (), 
                            reg_i1, inttype, NULL);

  while (plist_head)
  {
    LIST *lp;
    lp = build_ir_proc_list ();
    lp->datap = (LDATA *) reg_i0;
    LAPPEND (plist_head->data->u.s.firstprivate, lp);
    lp = build_ir_proc_list ();
    lp->datap = (LDATA *) reg_i1;
    LAPPEND (plist_head->data->u.s.firstprivate, lp);

    plist_prev = plist_head;
    plist_head = plist_head->next;
    free (plist_prev);
  }
  plist_cur = plist_prev = NULL;
}

void
restore_eh_registers (void)
{
  TYPE type;
  IR_TYPE_NODE *typep;
  IR_NODE *reg_i0, *reg_i1;
  /* restore %i0 from EXC_PTR_EXPR <<<exception object>>> */
  type = map_gnu_type_to_TYPE (ptr_type_node);
  typep = map_gnu_type_to_IR_TYPE_NODE (ptr_type_node);
  reg_i0 = build_ir_reg_var ("%i0", IR_REG_I0, type, typep),
  reg_i0->leaf.is_volatile = IR_TRUE;
  build_ir_triple (IR_ASSIGN, reg_i0,
                   get_ir_exception_pointer (), 
                   type, NULL);
  /* restore %i1 from FILTER_EXPR <<<filter object>>> */
  typep = map_gnu_type_to_IR_TYPE_NODE (intSI_type_node);
  reg_i1 = build_ir_reg_var ("%i1", IR_REG_I1, inttype, typep),
  reg_i1->leaf.is_volatile = IR_TRUE;
  build_ir_triple (IR_ASSIGN, reg_i1,
                   get_ir_exception_filter (), 
                   inttype, NULL);
}


static TYPE
bf_preprocess (TYPE t)
{
  if (t.tword == PCC_LLONG)
    t.tword = PCC_ULLONG;
  else if (t.tword == PCC_INT)
    t.tword = PCC_UNSIGNED;
  else if (t.tword == PCC_LONG)
    t.tword = PCC_ULONG;
  else if (t.tword == PCC_SHORT)
    t.tword = PCC_USHORT;
  else if (t.tword == PCC_CHAR)
    t.tword = PCC_UCHAR;

  return t;
}

static TYPE
bf_preprocess_reverse (TYPE t)
{
  if (t.tword == PCC_ULLONG)
    t.tword = PCC_LLONG;
  else if (t.tword == PCC_UNSIGNED)
    t.tword = PCC_INT;
  else if (t.tword == PCC_ULONG)
    t.tword = PCC_LONG;
  else if (t.tword == PCC_USHORT)
    t.tword = PCC_SHORT;
  else if (t.tword == PCC_UCHAR)
    t.tword = PCC_CHAR;

  return t;
}

static  IR_NODE *
ir_reduce_bit_field_operations (IR_NODE * node, tree type)
{
  HOST_WIDE_INT prec = TYPE_PRECISION (type);
  if (TYPE_UNSIGNED (type))
    {
      IR_NODE * mask;
      mask = build_ir_int_const (((unsigned HOST_WIDE_INT) 1 << prec) - 1, 
                      bf_preprocess (node->operand.type), 0);
      return build_ir_triple (IR_AND, node, mask, bf_preprocess (node->operand.type), NULL);
    }
  else
    {
      IR_NODE * count, * ret;
      count = build_ir_int_const (node->operand.type.size * BITS_PER_UNIT - prec, 
                       bf_preprocess_reverse (node->operand.type), 0);
      ret = build_ir_triple (IR_LSHIFT, node, count, bf_preprocess_reverse (node->operand.type), NULL);
      return build_ir_triple (IR_RSHIFT, ret, count, bf_preprocess_reverse (node->operand.type), NULL);
    }
}

/* build vis library call instead of basic op */
static IR_NODE *
ir_dump_vis (tree stmt, tree op0, tree op1)
{
  tree arglist, t;
  tree fn;
  char buf[64] = "__vis_f";
  int inner_size = TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (TREE_TYPE (stmt))));
  int need_size = 0;
  IR_NODE * ret;
  
  switch (TREE_CODE (stmt))
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR: strcat (buf, "padd"); need_size = 1; break;
    case MINUS_EXPR: strcat (buf, "psub"); need_size = 1; break;
    case BIT_AND_EXPR:
      /*  ~a & b pattern */
      if (TREE_CODE (op0) == BIT_NOT_EXPR 
          && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op0)))
        {
          strcat (buf, "andnot"); 
          op0 = TREE_OPERAND (op0, 0);
        }
      else if (TREE_CODE (op1) == BIT_NOT_EXPR 
               && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op1)))
        { /* a & ~b can be transformed into ~b & a */
          tree t = op0;
          strcat (buf, "andnot"); 
          op0 = TREE_OPERAND (op1, 0);
          op1 = t;
        }
      else
        strcat (buf, "and"); 
      break;
    case BIT_IOR_EXPR: 
      /*  ~a | b pattern */
      if (TREE_CODE (op0) == BIT_NOT_EXPR 
          && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op0)))
        {
          strcat (buf, "ornot"); 
          op0 = TREE_OPERAND (op0, 0);
        }
      else if (TREE_CODE (op1) == BIT_NOT_EXPR 
               && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op1)))
        { /* a | ~b can be transformed into ~b | a */
          tree t = op0;
          strcat (buf, "ornot"); 
          op0 = TREE_OPERAND (op1, 0);
          op1 = t;
        }
      else
        strcat (buf, "or");
      break;
    case BIT_XOR_EXPR: 
       /* xnor patterns.  Note that (a ^ ~b) == (~a ^ b) == ~(a ^ b). */
      if (TREE_CODE (op0) == BIT_NOT_EXPR 
          && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op0)))
        {
          strcat (buf, "xnor"); 
          op0 = TREE_OPERAND (op0, 0);
        }
      else if (TREE_CODE (op1) == BIT_NOT_EXPR 
               && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op1)))
        {
          strcat (buf, "xnor"); 
          op1 = TREE_OPERAND (op1, 0);
        }
      else
        strcat (buf, "xor"); 
      break;
    case BIT_NOT_EXPR: 
       /* xnor patterns.  Note that (a ^ ~b) == (~a ^ b) == ~(a ^ b). */
      if (TREE_CODE (op0) == BIT_XOR_EXPR 
          && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op0)))
        {
          strcat (buf, "xnor"); 
          op1 = TREE_OPERAND (op0, 1);
          op0 = TREE_OPERAND (op0, 0);
        }
      else if (TREE_CODE (op0) == BIT_AND_EXPR 
          && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op0)))
        { /* ~ (a & b) pattern */
          strcat (buf, "nand"); 
          op1 = TREE_OPERAND (op0, 1);
          op0 = TREE_OPERAND (op0, 0);
        }
      else if (TREE_CODE (op0) == BIT_IOR_EXPR 
          && TREE_CODE (TREE_TYPE (stmt)) == TREE_CODE (TREE_TYPE (op0)))
        { /* ~ (a | b) pattern */
          strcat (buf, "nor"); 
          op1 = TREE_OPERAND (op0, 1);
          op0 = TREE_OPERAND (op0, 0);
        }
      else
        strcat (buf, "not");
      break;
    default: abort ();
    }
  
  if (need_size)
    {
      if (inner_size == 16)
        strcat (buf, "16");
      else if (inner_size == 32)
        strcat (buf, "32");
    }
 
  if (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (stmt))) == 32)
    strcat (buf, "s");
 
  if (op1) /* binary op */ 
    {
      fn = build_decl (FUNCTION_DECL, get_identifier (buf), /* func name */
                       build_function_type (TREE_TYPE (stmt), /* return type */
                         tree_cons (NULL_TREE, TREE_TYPE (stmt), /* arg1 type */
                         build_tree_list (NULL_TREE, TREE_TYPE (stmt))))); /* arg2 type */
      arglist = tree_cons (NULL_TREE, op0,
                           build_tree_list (NULL_TREE, op1));
    }
  else /* unary op */
    {
      fn = build_decl (FUNCTION_DECL, get_identifier (buf), /* func name */
                       build_function_type (TREE_TYPE (stmt), /* return type */
                         build_tree_list (NULL_TREE, TREE_TYPE (stmt)))); /* arg1 type */
      arglist = build_tree_list (NULL_TREE, op0);
    }
  DECL_ARTIFICIAL (fn) = 1;
  DECL_EXTERNAL (fn) = 1;
  TREE_PUBLIC (fn) = 1;
  TREE_NOTHROW (fn) = 1;

  t = build_function_call_expr (fn, arglist);
  ret = dump_ir_call (t, 1);
  return ret;
}

static IR_NODE *
dump_ir_parm_var (tree stmt, enum MAP_FOR map_for)
{
  IR_NODE *ret = 0;
  const char *name;
  tree gnu_var_type;   /* GNU type used for passing PARM_DECL.  */
  tree gnu_param_type; /* GNU type of created IR PARAM LEAF.  */
  TYPE var_type, param_type;
  IR_TYPE_NODE *ir_param_type_node;

  name = get_ir_name (stmt);
  gnu_var_type = TREE_CODE (TREE_TYPE (stmt)) == REAL_TYPE
                 ? DECL_ARG_TYPE (stmt) : TREE_TYPE (stmt); 
  var_type = map_gnu_type_to_TYPE (gnu_var_type);
  gnu_param_type = gnu_type_of_ir_param_leaf (stmt);
  param_type = map_gnu_type_to_TYPE (gnu_param_type);
  ir_param_type_node = map_gnu_type_to_IR_TYPE_NODE (gnu_param_type);
  ret = build_ir_parm_var (name, lookup_param_offset (stmt, param_type),
                           param_type, ir_param_type_node);
  if (TREE_THIS_VOLATILE (stmt))
    ret->leaf.is_volatile = IR_TRUE;
  /* cleanup: now it's done in build_ir_leaf() 
  set_leaf_pointerno (ret, param_type.tword); */
  
  if (tu_pass_by_reference (NULL, TYPE_MODE (gnu_var_type), gnu_var_type, false))
    {
      if (map_for == MAP_FOR_VALUE)
        ret = build_ir_triple (IR_IFETCH, ret, NULL, var_type, 
                               map_gnu_type_to_IR_TYPE_NODE (gnu_var_type));
      else if (var_type.size > 0) /* regular structs should have size>0 */
        ret->leaf.is_indirect = 1;
    }
  else
    {
    if (TREE_CODE (TREE_TYPE (stmt)) == REAL_TYPE 
        && DECL_ARG_TYPE (stmt) != TREE_TYPE (stmt))
      {
        /* floats promoted to doubles should be gimplified by gimplify_parameters()
           and copied to temp var before any use (including taking the address) */
        if (map_for == MAP_FOR_VALUE)
          ret = build_ir_triple (IR_CONV, ret, NULL,
                                 map_gnu_type_to_TYPE (TREE_TYPE (stmt)), 
                                 map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
        /* for MAP_FOR_ADDR just return the leaf,
           only dump_function_ir() should need it */
      }
    }  
  return ret;
}

static IR_NODE *
dump_ir_complexpart_expr (tree stmt, int is_realpart, enum MAP_FOR map_for)
{
  tree op0 = TREE_OPERAND (stmt, 0); /* complex expression */
  tree t;
  tree type = TREE_TYPE (stmt);
  tree record_type, f_real, f_imag;
  
  /* access bit packed complex type */
  if ((TREE_CODE (op0) == COMPONENT_REF 
       && (DECL_PACKED (TREE_OPERAND (op0, 1)) || DECL_USER_ALIGN (TREE_OPERAND (op0, 1))))
      || TYPE_PACKED (TREE_TYPE (op0)) || TYPE_USER_ALIGN (TREE_TYPE (op0)))
    {
      unsigned int align;

      /* get align of complex type */
      if (TYPE_PACKED (TREE_TYPE (op0)) || TYPE_USER_ALIGN (TREE_TYPE (op0)))
        align = MAX (TYPE_ALIGN (TREE_TYPE (op0)), 8);
      else
        align = MAX (DECL_ALIGN (TREE_OPERAND (op0, 1)), 8);
      
      /* check if align of inner type is higher than user align of complex type */
      if (TYPE_ALIGN (type) > align)
        {
          for (t = TYPE_MAIN_VARIANT (type); t; t = TYPE_NEXT_VARIANT (t))
            if (TYPE_ALIGN (t) == align)
              {
                type = t;
                break;
              }

          if (t == 0) /* type variant with needed align wasn't found */
            { /* create one */
              type = build_variant_type_copy (type);
              TYPE_ALIGN (type) = align;
            }
        }
    }

  /* use .context field to remember aux struct type used to access
     .real and .imag part of complex type
    
     for user-aligned complex types 'type' will point to variant of inner
     type with proper alignment set by the code above */
  if (TYPE_CONTEXT (type))
    {
      record_type = TYPE_CONTEXT (type);
      gcc_assert (TREE_CODE (record_type) == RECORD_TYPE);
      f_real = TYPE_FIELDS (record_type);
      f_imag = TREE_CHAIN (f_real);
    }
  else
    {
      /* create aux type to access .real or .imag part of complex type */
      record_type = make_node (RECORD_TYPE);
      f_real = build_decl (FIELD_DECL, get_identifier ("_._real"), type);
      DECL_CONTEXT (f_real) = record_type;
      f_imag = build_decl (FIELD_DECL, get_identifier ("_._imag"), type);
      DECL_CONTEXT (f_imag) = record_type;
      
      TYPE_FIELDS (record_type) = f_real;
      TREE_CHAIN (f_real) = f_imag;
      
      layout_type (record_type);

      /* mark it to indicate alias=any for ir_type_node generator */
      TYPE_LANG_FLAG_2 (record_type) = 1;

      /* remeber created type for future reference */
      TYPE_CONTEXT (type) = record_type; 
    }

  if (is_gimple_addressable (op0))
    t = build_fold_addr_expr_with_type (op0, build_pointer_type (record_type));
  else
    {
      tree var = create_tmp_var_raw (TREE_TYPE (op0), "__complex_tmp_var.");
      TREE_ADDRESSABLE (var) = 1;
      t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (op0), var, op0);
      TREE_SIDE_EFFECTS (t) = 1;
      dump_ir_stmt (t);
      t = build_fold_addr_expr_with_type (var, build_pointer_type (record_type));
    }
  t = build_fold_indirect_ref (t);

  if (is_realpart)
    t = build3 (COMPONENT_REF, type, t,  f_real, NULL);
  else
    t = build3 (COMPONENT_REF, type, t,  f_imag, NULL);
  
  return dump_ir_expr (t, map_for);
}

/* generate IR_NODE (leaf or triple) for given gimple expression */
static IR_NODE *
dump_ir_expr (tree stmt, enum MAP_FOR map_for)
{
  IR_NODE * ret = 0;
  TYPE argtype;

  if (errorcount != 0) 
    return get_err_triple ();

  /*  STRIP_NOPS (stmt);*/
  while (TREE_CODE (stmt) == NON_LVALUE_EXPR)
    stmt = TREE_OPERAND (stmt, 0);

  switch (TREE_CODE (stmt))
    {
    case VAR_DECL:
      {
        const char * name;
        tree threadprivate;

        if (ir_language == FORTRAN
            && DECL_HAS_VALUE_EXPR_P (stmt)
            && DECL_LANG_FLAG_3 (stmt))
          {
            /* common block reference, this is really a field
               in a common block. */
            return dump_ir_expr (DECL_VALUE_EXPR (stmt), map_for);
          }
        
        threadprivate = lang_hooks.decls.omp_threadprivate_decl(stmt);
        if (threadprivate != NULL_TREE)
          {
            LEAF *var;
            /* If this is a thread private variable generate
               the segment and leaves for the pointer to tp
               artificial variable also. Required by iropt */
            var = (LEAF *) dump_ir_expr (threadprivate,
                                         MAP_FOR_VALUE);
            gcc_assert (var->tag == ISLEAF);
            /*var->in_taskcommon_block = IR_TRUE;*/
          }
        
        /* type of the variable.  */
        tree var_type = TREE_TYPE (stmt); 
        name = get_ir_name (stmt);
        argtype = map_gnu_type_to_TYPE (var_type);
        
        if (DECL_USER_ALIGN (stmt)) 
          /* user requested align. see gcc.c-torture/execute/20020619-1.c */
          argtype.align = MAX (DECL_ALIGN (stmt), 8) / BITS_PER_UNIT; /* at least one byte */
            
        if (DECL_REGISTER (stmt) && DECL_ASSEMBLER_NAME_SET_P (stmt)
            && DECL_NAME (stmt))
          {
            /* register name for redefined register vars */
            const char *regname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (stmt));

            /* gcc should have caught this earlier with:
               "error: data type of 'somevar' isn't suitable for a register" */
            gcc_assert (TREE_CODE (var_type) != RECORD_TYPE);

            if (regname[0] != '*')
              {
                error ("%Jregister name not specified for %qD", stmt, stmt);
              }
            else
              {
                int regno = decode_reg_name (++ regname);
            
                /* First detect errors in declaring global registers.  */
                if (regno == -1)
                  error ("%Jregister name not specified for %qD", stmt, stmt);
                else if (regno < 0)
                  error ("%Jinvalid register name for %qD", stmt, stmt);
                else if (TYPE_MODE (TREE_TYPE (stmt)) == BLKmode)
                  error ("%Jdata type of %qD isn%'t suitable for a register",
                         stmt, stmt);
                else if (! HARD_REGNO_MODE_OK (regno, TYPE_MODE (TREE_TYPE (stmt))))
                  error ("%Jregister specified for %qD isn%'t suitable for data type",
                         stmt, stmt);
                /* Now handle properly declared static register variables.  */
                else
                  {
                    if (DECL_INITIAL (stmt) != 0 && TREE_STATIC (stmt))
                      {
                        DECL_INITIAL (stmt) = 0;
                        error ("global register variable has initial value");
                      }
                    if (TREE_THIS_VOLATILE (stmt))
                      warning0 ("volatile register variables don%'t "
                               "work as you might wish");
                    ret = build_ir_reg_var (regname, regno, 
                                            argtype, map_gnu_type_to_IR_TYPE_NODE (var_type));
                    ret->leaf.is_volatile = IR_TRUE;
                    if (threadprivate != NULL_TREE)
		      {
                        ret->leaf.in_taskcommon_block = IR_TRUE;
			/* Associate the segments */
			LEAF *tpleaf = (LEAF *) dump_ir_expr (threadprivate, MAP_FOR_VALUE);
			gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
			IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
		      }
                  }
              }
            if (errorcount != 0) 
              break;
          }
        else if (TREE_PUBLIC (stmt) || TREE_STATIC (stmt) || DECL_EXTERNAL (stmt))
          {
            if (TREE_READONLY (stmt) /* const type var = 0; in C++ */
                && map_for == MAP_FOR_VALUE
                && DECL_INITIAL (stmt)    /* extra checks like in decl_constant_value () */
                && TREE_CONSTANT (DECL_INITIAL (stmt))
                && TREE_CODE (DECL_INITIAL (stmt)) != CONSTRUCTOR
                && !TREE_THIS_VOLATILE (stmt)
                && TREE_CODE (var_type) != ARRAY_TYPE
                && (TREE_CONSTANT (stmt) 
                    /* const int var; in C doesn't have TREE_CONSTANT attribute */
                    || ir_language == C) 
                && default_opt_level) /* -O1 or above */
              {
                /* optimize 'const type var = constant;' into just 'constant'
                   the same way as gcc does in tree-ssa-ccp.c 
                   to fix: g++.dg/opt/static3.C */
                ret = dump_ir_expr (DECL_INITIAL (stmt), MAP_FOR_VALUE);
                break;
              }
                
            ret = build_ir_extern_var (stmt /*VAR_DECL*/, 0 /*off*/, 
                                       argtype, 
                                       map_gnu_type_to_IR_TYPE_NODE (var_type));
            /* pre-gcc.3.5.x style: */
            {tree id = maybe_get_identifier (name); if (id) mark_referenced (id);}
            /* gcc 3.5.x style of marking used names: */
            mark_decl_referenced (stmt); 
            /* cleanup: now it's done in adjust_leaf_overlaps()
            if (ret->leaf.elvarno == -1)
              add_ir_leaf_overlap (func_heap_leaf, (LEAF*)ret); */
            if (threadprivate != NULL_TREE)
	      {
                ret->leaf.in_taskcommon_block = IR_TRUE;
		/* Associate the segments */
		LEAF *tpleaf = (LEAF *) dump_ir_expr (threadprivate, MAP_FOR_VALUE);
		gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
			IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
	      }
          }
        else if (flag_tree_ir_regalloc /* ON by default */
                 && DECL_IGNORED_P (stmt) && !TREE_ADDRESSABLE (stmt)
                 && TREE_CODE (var_type) != VECTOR_TYPE
                 && ((argtype.tword >= PCC_CHAR && argtype.tword <= PCC_DOUBLE)
                     || (argtype.tword >= PCC_BOOL && argtype.tword <= PCC_ULLONG)
                     || PCC_ISPTR (argtype.tword)))
          {
            ir_ADDRESS addr;
            LEAF *leaf;
            static int regno = FIRST_SGCC_VIRTUAL_REGNO;
            int reg;
            
            memset (&addr, 0, sizeof (addr));
            if (DECL_IR_OFFSET (stmt)) 
              reg = DECL_IR_OFFSET (stmt);
            else
              reg = regno++;
            
            if (argtype.tword == PCC_FLOAT || argtype.tword == PCC_DOUBLE)
              addr.seg = segtab[FREG_SEGNO];
            else
              addr.seg = segtab[DREG_SEGNO];
            
            addr.offset = reg;
            DECL_IR_OFFSET (stmt) = reg;
            leaf = build_ir_leaf (VAR_LEAF, argtype, map_gnu_type_to_IR_TYPE_NODE (var_type), 
                                  (LEAF_VALUE *)&addr, IR_TRUE);
            leaf->pass1_id = build_ir_proc_string (name);
            ret = (IR_NODE*)leaf;
            if (threadprivate != NULL_TREE)
	      {
                ret->leaf.in_taskcommon_block = IR_TRUE;
		/* Associate the segments */
		LEAF *tpleaf = (LEAF *) dump_ir_expr (threadprivate, MAP_FOR_VALUE);
		gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
			IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
	      }
          }
        else
          {
            if (skip_bool_conv
                && ir_language == CDOUBLEPLUS
                && DECL_IGNORED_P (stmt) /* g++ temp var */
                && !TREE_ADDRESSABLE (stmt)
                && TREE_CODE (var_type) == BOOLEAN_TYPE)
              /* place func local bool types in 'int' vars to simplify optimizer work */
              argtype = inttype;
             
            ret = build_ir_auto_var (name, lookup_auto_offset (stmt, argtype) /*segoff*/, 
                                     argtype.size /*seglen*/,
                                     argtype.align /*align*/, 0 /*off*/, 
                                     argtype, map_gnu_type_to_IR_TYPE_NODE (var_type));

            SCOPE_TEMP_AS_PRIVATE_IFNEEDED(stmt, name, ret);

            if (threadprivate != NULL_TREE)
	      {
                ret->leaf.in_taskcommon_block = IR_TRUE;
		/* Associate the segments */
		LEAF *tpleaf = (LEAF *) dump_ir_expr (threadprivate, MAP_FOR_VALUE);
		gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
			IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
	      }
          }

        if (TREE_THIS_VOLATILE (stmt))
          ret->leaf.is_volatile = IR_TRUE;
       
        gcc_assert(ret->operand.tag ==ISLEAF);
        if (TREE_PUBLIC (stmt) && !DECL_EXTERNAL(stmt))
        {
          enum symbol_visibility vis = DECL_VISIBILITY (stmt);
          set_leaf_ld_scope(ret,vis);
        }
 
        /* cleanup: now it's done in build_ir_leaf() 
        set_leaf_pointerno (ret, argtype.tword); */
        if (DECL_NONLOCAL (stmt) || stmt == cfun->static_chain_decl)
          {
            //ret->leaf.uplevel_addressed = IR_TRUE;
          }
        /* cleanup: now it's done in adjust_leaf_overlaps()
        if (ret->leaf.elvarno == -1 
            && (TREE_ADDRESSABLE (stmt)
                || TREE_PUBLIC (stmt) || TREE_STATIC (stmt) || DECL_EXTERNAL (stmt)))
          {
            ret->leaf.elvarno = naliases++;
          }*/
        if ((IS_ARRAY_TYPE (var_type) || IS_RECORD_TYPE (var_type)) 
            && map_for == MAP_FOR_ADDR)
          {
            /* This leave cannot reside in a register */
            /* cleanup: now it's done in adjust_leaf_overlaps()
            ret->leaf.no_reg = IR_TRUE;
            if (ret->leaf.elvarno == -1)
              ret->leaf.elvarno = naliases++;*/
            
            var_type = build_pointer_type (var_type);
            argtype = map_gnu_type_to_TYPE (var_type);
            
            ret = build_ir_addr_const (ret, 0 /*offset*/, argtype, 
                                       map_gnu_type_to_IR_TYPE_NODE (var_type));
            if (threadprivate != NULL_TREE)
	      {
                ret->leaf.in_taskcommon_block = IR_TRUE;
		/* Associate the segments */
		LEAF *tpleaf = (LEAF *) dump_ir_expr (threadprivate, MAP_FOR_VALUE);
		gcc_assert (ir_proc_associate_objs (irProc, ret->leaf.val.addr.seg,
			IR_ASSOC_VAR_TLS, tpleaf->val.addr.seg));
	      }
          }
      }
      break;
    case PARM_DECL:
      ret = dump_ir_parm_var (stmt, map_for);
      break;
    case RESULT_DECL:
      {
        ret = func_ret_leaf;

        /* inline will import result_decl from other function */
        if (DECL_CONTEXT(stmt) != current_function_decl)
          {
	    tree var_type = TREE_TYPE (stmt);
	    ret = get_tmp_leaf_with_name (map_gnu_type_to_TYPE (var_type),
					  map_gnu_type_to_IR_TYPE_NODE (var_type),
					  get_ir_name (stmt));
	  }

        
        /* using extended IR to pass struct values out of funcs */
        if (TREE_CODE (TREE_TYPE (stmt)) == REFERENCE_TYPE
            && DECL_BY_REFERENCE (stmt))
          break;

        if ((IS_RECORD_TYPE (TREE_TYPE (stmt))
             /*(TREE_CODE (TREE_TYPE (stmt)) == RECORD_TYPE 
              || TREE_CODE (TREE_TYPE (stmt)) == UNION_TYPE) */
             && map_for == MAP_FOR_ADDR)
            || TREE_CODE (TREE_TYPE (stmt)) == ARRAY_TYPE
            || is_large_vector_type (TREE_TYPE (stmt))
            || TREE_CODE (TREE_TYPE (stmt)) == REFERENCE_TYPE)
          {
            /* This leave cannot reside in a register */
            /* cleanup: now it's done in adjust_leaf_overlaps()
            func_ret_leaf->leaf.no_reg = IR_TRUE;
            if (func_ret_leaf->leaf.elvarno == -1)
              func_ret_leaf->leaf.elvarno = naliases++;*/

            ret = build_ir_addr_const (func_ret_leaf, 0 /*offset*/, 
                              map_gnu_type_to_TYPE (build_pointer_type (fval_type)), 
                              map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (fval_type)));
          }
      }
      break;
    case INDIRECT_REF:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* pointer */
        tree stmt_type = TREE_TYPE (stmt);
        
        /* don't build_pointer_type (stmt_type) for ARRAY_TYPEs
           see CR 6589773 */
        argtype = map_gnu_type_to_TYPE (stmt_type);
        
        ret = dump_ir_expr (op0, MAP_FOR_VALUE);

        if (ret->operand.tag == ISTRIPLE && ret->triple.ldst_ir_type)
          {
            ret->triple.ldst_ir_type = ir_decref (ret->triple.ldst_ir_type);
            ret->triple.ldst_type = argtype;
          }
        
        if (TYPE_SIZE (TREE_TYPE (stmt)) /* variable length type */
             && TREE_CODE (TYPE_SIZE (TREE_TYPE (stmt))) != INTEGER_CST)
          {
             /* can not ifetch zero sized VLA struct
                only prior to gcc 4.x: 
                ret = build_ir_triple (IR_IFETCH, ret, NULL, argtype, decref ir type */
          }
        else if (map_for == MAP_FOR_VALUE)
          {
            if (ret->operand.tag == ISLEAF && TREE_CODE (op0) == RESULT_DECL 
                && ret->leaf.addressed_leaf)
              ret = (IR_NODE*) (ret->leaf.addressed_leaf);
            else
              {
                ret = build_ir_triple (IR_IFETCH, ret, NULL, argtype, 
                                       map_gnu_type_to_IR_TYPE_NODE (stmt_type));
                if (TREE_THIS_VOLATILE (stmt))
                  ret->triple.is_volatile = IR_TRUE;
                
                if (ir_language == CDOUBLEPLUS
                    && TREE_CODE (TREE_TYPE (op0)) == POINTER_TYPE
                    && TREE_CODE (TREE_TYPE (TREE_TYPE (op0))) == POINTER_TYPE)
                  {
                      /* Fix 6545539. Make sure we mark the deference to obtain
                         the vptr type through member functions also as vptr
                         references, and the second de-reference as read only
                         data. */
                      tree ttype = TREE_TYPE (TREE_TYPE (TREE_TYPE (op0)));
                      if (TREE_CODE (ttype) == POINTER_TYPE
                          && is_virtual_pointer_type (ttype))
                        ret->triple.access_info = cpp_vtptr_ref;
                      else if (TREE_CODE (ttype) == FUNCTION_TYPE
                               && is_virtual_pointer_type (TREE_TYPE (TREE_TYPE (op0))))
                        ret->triple.access_info = cpp_rodata_ref;
                  }
              }
          }
      }
      break;

    case OBJ_TYPE_REF:
      ret = dump_ir_expr (OBJ_TYPE_REF_EXPR (stmt), map_for);
      if (TREE_CODE (OBJ_TYPE_REF_EXPR (stmt)) == INDIRECT_REF
          && TREE_CODE (TREE_TYPE (OBJ_TYPE_REF_EXPR (stmt))) == POINTER_TYPE
          && is_virtual_pointer_type (TREE_TYPE (OBJ_TYPE_REF_EXPR (stmt))))
        {
          /* virtual ptr references can have the following patterns, and
             the various accesses in the pattern must be marked
             appropriately. The dereference of the instance that results
             in obtaining the virtual table should be marked with cpp_vtptr_ref.
             The subsequent dereference that produces the
             function pointer, must be marked with cpp_rodata_ref.
             
             CALL <RO data><RO ref>*(<VTPTR ref>*(e + 0))(e);
             
             Alternately, certain optimizations within the
             front end result in generation of slightly different
             patterns, one where the instance may be allocated
             locally on the stack, which can result in a call
             through the virtual table. Dont perform any marking
             for such calls. For example
             see xalan/Stylesheet.cpp, g++.dg/eh/spec7.C */
            
            IR_NODE *op0;
            gcc_assert (ret->operand.tag == ISTRIPLE
                        && ret->triple.op == IR_IFETCH);
          
            op0 = ret->triple.left;
            if (op0->operand.tag == ISTRIPLE)
              {
                if (op0->triple.op == IR_IFETCH)
                  {
                    ret->triple.rodata_ref = IR_TRUE;
                    ret->triple.access_info = cpp_rodata_ref;
                    op0->triple.access_info = cpp_vtptr_ref;
                  }
                  else if (op0->triple.op == IR_PLUS)
                  {
                    IR_NODE *op1 = op0->triple.left;
                    if (op1->triple.op == IR_IFETCH)
                      {
                        ret->triple.rodata_ref = IR_TRUE;
                        ret->triple.access_info = cpp_rodata_ref;
                        op1->triple.access_info = cpp_vtptr_ref;   
                      }
                    else
                      {
                        /* Have a direct ifetch of var leaf. Dont
                           mark the access. This is a result of front end
                           propagating the assignment of virtual
                           table to a stack allocated class.
                           see func addTemplete in xalan/Stylesheet.cpp */
                      }
                  }
              }
            else
              {
                /* This is a result of gcc front end converting some
                   class accesses on the stack to direct references.
                   so we may end up with IR of the form
                   d = &_ZTV1D;
                   CALL *(d)(&d);
                   Dontt mark the access in any way 
                   see g++.dg/eh/spec7.C */
              }
        }
      break;
    
    case WITH_SIZE_EXPR:
      ret = dump_ir_expr (TREE_OPERAND (stmt, 0), map_for);
      break;
      
    case ADDR_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* pointer */
        IR_NODE * ir_op0 = 0;
        
        /* ADDR_EXPR (INDIRECT_REF (x)) - invalid tree */
        gcc_assert (TREE_CODE (op0) != INDIRECT_REF);

        if (TREE_CODE (op0) == STRING_CST)
          {
            ret = dump_string_cst (op0, MAP_FOR_ADDR);
          }
        else if (TREE_CODE (op0) == FUNCTION_DECL)
          {
            ret = dump_ir_funcname (op0);
          }
        else if (TREE_CODE (op0) == LABEL_DECL)
          {
            TYPE argtype = labelno_TYPE;
            int labelno = get_ir_label (op0);

            ret = build_ir_int_const (labelno, argtype, 0);
            
            if (DECL_NONLOCAL (op0) || FORCED_LABEL (op0))
              ret->leaf.unknown_control_flow = IR_TRUE;
            
            /* this label had its address taken and therefore can 
               never be deleted and is a jump target for computed gotos */
            if (FORCED_LABEL (op0)) 
              /* do not add NONLOCAL labels, because iropt/cg will not
                 be able to build cfg correctly */
              add_to_label_list (labelno);

            ret = build_ir_triple (IR_CONV, ret, NULL,
                              map_gnu_type_to_TYPE (ptr_type_node), 
                              map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
          }
        else if (TREE_CODE (op0) == PARM_DECL)
          {
            ret = dump_ir_expr (op0, MAP_FOR_ADDR);
            if (tu_pass_by_reference (NULL, TYPE_MODE (TREE_TYPE (op0)), TREE_TYPE (op0), false))
              break;
            
            argtype = map_gnu_type_to_TYPE (TREE_TYPE (op0));
            
            /* llongs and doubles in 32-bit should be gimplified 
               by gimplify_parameters() */
            gcc_assert (argtype.size <= 4 || TARGET_ARCH64);
                
            ret = build_ir_addr_const (ret, 0,
                         map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (op0))),
                         map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (TREE_TYPE (op0))));
          }
        else
          {
            argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
            
            ir_op0 = dump_ir_expr (op0, MAP_FOR_ADDR);
            
            if (ir_op0->operand.tag == ISTRIPLE)
              {
                ret = ir_op0;
                if (ret->triple.ldst_ir_type)
                  {
                    ret->triple.ldst_ir_type = make_ptr_to_ir_type_node (ret->triple.ldst_ir_type);
                    
                    ret->triple.ldst_type = argtype;
                  }
                break;
              }
           
            gcc_assert (ir_op0->operand.tag == ISLEAF);

            if ((TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE
                 || is_large_vector_type (TREE_TYPE (op0)))
                && PCC_ISPTR (ir_op0->leaf.type.tword))
              {
                ret = ir_op0;
                break;
              }

            if (ir_op0->leaf.class == ADDR_CONST_LEAF)
              ret = ir_op0; /* value was already returend from build_ir_addr_const */
            else if (ir_op0->leaf.class == CONST_LEAF)
              {
                IR_NODE * var = get_tmp_leaf (ir_op0->operand.type, 
                                              map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0)));

                /* cleanup: now it's done in adjust_leaf_overlaps()
                var->leaf.no_reg = IR_TRUE;
                if (var->leaf.elvarno == -1)
                  var->leaf.elvarno = naliases++; */
                
                build_ir_triple (IR_ASSIGN, var, ir_op0, ir_op0->operand.type, NULL);

                ret = build_ir_addr_const (var, 0 /*offset*/, argtype,
                                           map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
              }
            else
              {
                /* This leave cannot reside in a register */
                /* cleanup: now it's done in adjust_leaf_overlaps()
                ir_op0->leaf.no_reg = IR_TRUE;
                if (ir_op0->leaf.elvarno == -1)
                  ir_op0->leaf.elvarno = naliases++; */
            
                if (ir_op0->leaf.typep)
                  ret = build_ir_addr_const (ir_op0, 0 /*offset*/, argtype, 
                                             make_ptr_to_ir_type_node (ir_op0->leaf.typep));
                else
                  ret = build_ir_addr_const (ir_op0, 0 /*offset*/, argtype, 
                                             map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
              }
          }
      }
      break;
    case COMPONENT_REF:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* struct/union. or struct/union-> or */
        tree op1 = TREE_OPERAND (stmt, 1); /* field of op0*/
        tree aligned_offset = TREE_OPERAND (stmt, 2);
        CONSZ offset, offset_bits;
        TYPE argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        const char * fld_name; /* name of field */
        char fld_name_buf[64];
        IR_NODE * ir_offset;
        TYPE argtype_stmt = argtype;
        tree fld_type = TREE_TYPE (stmt);

#ifdef ENABLE_CHECKING
        if (TREE_CODE (TREE_TYPE (op0)) != RECORD_TYPE &&
            TREE_CODE (TREE_TYPE (op0)) != UNION_TYPE)
          abort ();

        if (TREE_CODE (op1) != FIELD_DECL)
          abort ();
#endif
      
        /* If an offset was specified in the COMPONENT_REF, it's the offset measured
           in units of DECL_OFFSET_ALIGN / BITS_PER_UNIT.  So multiply by that value. */
        if (aligned_offset)
          {
            if (TREE_TYPE (aligned_offset) != sizetype)
              aligned_offset = fold_convert (sizetype, aligned_offset);
            
            /* aligned_offset - offset in bytes from the beginning of the structure */
            ir_offset = dump_ir_expr (aligned_offset, MAP_FOR_VALUE);

            /* the rest of the offset in bits */
            offset_bits = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (op1));
          }
        else if (TREE_CODE (DECL_FIELD_OFFSET (op1)) == INTEGER_CST) 
            /* regular struct of known size */
          {
            /* offset in bits */
            offset_bits = TREE_INT_CST_LOW (DECL_FIELD_OFFSET (op1)) * BITS_PER_UNIT
                          + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (op1));
            ir_offset = 0;
          }
        else /* variable length struct */
          {
            offset_bits = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (op1));
            ir_offset = dump_ir_expr (DECL_FIELD_OFFSET (op1), MAP_FOR_VALUE);
          }
        
        fld_name = get_ir_field_name (op1, offset_bits, fld_name_buf);
        
        /* offset in bytes */
        offset = offset_bits / BITS_PER_UNIT;
        
        if (DECL_BIT_FIELD (op1))
          {
            fld_type = DECL_BIT_FIELD_TYPE (op1);
            argtype = map_gnu_type_to_TYPE (fld_type);
            
            /* always use DECL_ALIGN for struct fields
               DECL_PACKED (op1) || DECL_USER_ALIGN (op1) might not be set,
               but field align will be different from container type align.
               DECL_ALIGN (op1) - user requested align for fields
               DECL_ALIGN_UNIT (op1) == 1 for bit 'packed' fields
               DECL_ALIGN (op1) == 1 bit - for bit 'packed' bit-fields
               DECL_ALIGN_UNIT (op1) == 0 - for bit 'packed' bit-fields
               need at least byte align for IR */
            argtype.align = MAX (DECL_ALIGN_UNIT (op1), 1);

            /* no need to access bit-fields with align larger than 
               size of container type */
            if (argtype.align > argtype.size)
              argtype.align = argtype.size;
            
            /* need to use argtype.size to align offset
               instead of argtype.align to avoid issues with CG
               see CR 6588610 */
            offset = (offset / argtype.size) * argtype.size;
          }
        else if (DECL_C_BIT_FIELD (op1))
          {
            argtype = map_gnu_bitfield_type_to_TYPE (op1);
            fld_type = lang_hooks.types.type_for_size (argtype.size * 8, DECL_UNSIGNED (op1));
          }
        else
          /* DECL_ALIGN_UNIT (op1) >= 1 here, since op1 is not a bit-field */
          argtype.align = DECL_ALIGN_UNIT (op1);
        
        ret = dump_ir_component_ref (stmt, op0, op1, fld_name, offset, 
                                     fld_type, argtype, map_for, ir_offset);
        
        if (TREE_THIS_VOLATILE (stmt) || TREE_THIS_VOLATILE (op1))
          {
            if (ret->operand.tag == ISLEAF)
              ret->leaf.is_volatile = IR_TRUE;
            else if (ret->triple.op == IR_IFETCH)
              ret->triple.is_volatile = IR_TRUE;
          }
        
        if (DECL_BIT_FIELD (op1) && map_for == MAP_FOR_VALUE)
          {
            int bf_width = TREE_INT_CST_LOW (DECL_SIZE (op1));
            int bf_offset = offset_bits - offset * BITS_PER_UNIT; 
            if (bf_offset + bf_width <= argtype.size * 8)
              {
                ret = build_ir_triple (IR_BFEXT, ret, NULL, /*bf_preprocess*/ (argtype), NULL);
                ret->triple.bfoffset = bf_offset;
                ret->triple.bfwidth = bf_width;
              }
            else
              {
                IR_NODE * part1, * part2;
                int size1 = argtype.size * 8 - bf_offset;
                int size2 = bf_offset + bf_width - argtype.size * 8;

                part1 = build_ir_triple (IR_BFEXT, ret, NULL, argtype, NULL);
                part1->triple.bfoffset = bf_offset;
                part1->triple.bfwidth = size1;
                
                ret = dump_ir_component_ref (stmt, op0, op1, fld_name, offset + argtype.size, 
                                             fld_type, argtype, map_for, ir_offset);

                part2 = build_ir_triple (IR_BFEXT, ret, NULL, argtype, NULL);
                part2->triple.bfoffset = 0;
                part2->triple.bfwidth = size2;

                ret = build_ir_triple (IR_LSHIFT, part1, build_ir_int_const (size2, inttype, 0), argtype, NULL);
                ret = build_ir_triple (IR_OR, ret, part2, argtype, NULL);
              }
            
            if ((DECL_BIT_FIELD (op1) || DECL_C_BIT_FIELD (op1)) 
                && argtype.tword != argtype_stmt.tword)
              {
                ret = build_ir_triple (IR_CONV, ret, NULL, argtype_stmt, 
                                       map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
              }
            
/*            if (lang_hooks.reduce_bit_field_operations
                && TREE_CODE (TREE_TYPE (stmt)) == INTEGER_TYPE
                && argtype.size * BITS_PER_UNIT > TYPE_PRECISION (TREE_TYPE (stmt)))
              ret = ir_reduce_bit_field_operations (ret, TREE_TYPE (stmt));*/
          }
      }
      break;
    case BIT_FIELD_REF:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* struct/union*/
        tree op1 = TREE_OPERAND (stmt, 1); /* size in bits*/
        tree op2 = TREE_OPERAND (stmt, 2); /* offset in bits*/
        int offset, offset_bits, bf_width, bf_offset;
        TYPE argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

#ifdef ENABLE_CHECKING
        if (TREE_CODE (TREE_TYPE (op0)) != RECORD_TYPE
            && TREE_CODE (TREE_TYPE (op0)) != UNION_TYPE 
            && TREE_CODE (TREE_TYPE (op0)) != ARRAY_TYPE 
            && TREE_CODE (TREE_TYPE (op0)) != VECTOR_TYPE)
          abort ();
        if (map_for == MAP_FOR_ADDR)
          abort ();
#endif
        
        /* size in bits */
        bf_width = TREE_INT_CST_LOW (op1);
        
        /* offset in bits */
        offset_bits = TREE_INT_CST_LOW (op2);
        
        /* offset in bytes */
        offset = offset_bits / BITS_PER_UNIT;
        offset = (offset / argtype.align) * argtype.align;

        ret = dump_ir_component_ref (stmt, op0, NULL, NULL, offset, TREE_TYPE (stmt), 
                                     argtype, map_for, 0);
        
        bf_offset = offset_bits - offset * BITS_PER_UNIT; 
        
        if (bf_offset + bf_width <= argtype.size * 8)
          {
            if (bf_offset != 0 || bf_width != argtype.size * 8)
              { 
                ret = build_ir_triple (IR_BFEXT, ret, NULL, /*bf_preprocess*/ (argtype), NULL);
                ret->triple.bfoffset = bf_offset;
                ret->triple.bfwidth = bf_width;
              }
          }
        else
          {
            IR_NODE * part1, * part2;
            int size1 = argtype.size * 8 - bf_offset;
            int size2 = bf_offset + bf_width - argtype.size * 8;

            part1 = build_ir_triple (IR_BFEXT, ret, NULL, argtype, NULL);
            part1->triple.bfoffset = bf_offset;
            part1->triple.bfwidth = size1;
            
            ret = dump_ir_component_ref (stmt, op0, NULL, NULL, offset + argtype.size, 
                                         TREE_TYPE (stmt), argtype, map_for, 0);

            part2 = build_ir_triple (IR_BFEXT, ret, NULL, argtype, NULL);
            part2->triple.bfoffset = 0;
            part2->triple.bfwidth = size2;

            ret = build_ir_triple (IR_LSHIFT, part1, build_ir_int_const (size2, inttype, 0), argtype, NULL);
            ret = build_ir_triple (IR_OR, ret, part2, argtype, NULL);
          }

/*        if (lang_hooks.reduce_bit_field_operations
            && TREE_CODE (TREE_TYPE (stmt)) == INTEGER_TYPE
            && argtype.size * BITS_PER_UNIT > TYPE_PRECISION (TREE_TYPE (stmt)))
          ret = ir_reduce_bit_field_operations (ret, TREE_TYPE (stmt));*/
      }
      break;
    case ARRAY_REF:
      {
        tree array;
        tree op0 = array = TREE_OPERAND (stmt, 0); /* var_decl of array_type || array_ref*/
        tree t, op1, inest;
        IR_NODE * ir_op0 = 0, * ir_op1 = 0, * ir_bounds, * ir_diminfo, 
                * ir_subindex, * ir_arrloc, * prev_ir_size, * ir_op0_for_diminfo;
        IR_NODE ** ntuple_pp;
        int nsubs = 1, in_nsubs; /* number of sub indexes */
        int subindex, sub_subindex;
	int saved_gesize ;  /* saved the current status. restore when back from subindex. */
	tree saved_outer_tree = NULL;
	IR_NODE * saved_offset;
	
        long long prev_size, stride;
        
        tree ptr_stmt_type = build_pointer_type (TREE_TYPE (stmt));
        IR_TYPE_NODE *arrloc_ir_type;
	IR_TYPE_NODE * ir_type = NULL;
        TYPE arrloc_type;

        if (is_field_array != 1 && flag_field_array)
          {
            is_field_array = check_field_array (stmt, 0);
            outer_tree = stmt;
            if (is_field_array == 1)
              {
                g_esize = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (stmt)));
      
      	        /* compute the global offset */
      	        gir_offset = NULL;
      	        compute_goffset(stmt);
              }
          }
        if (is_field_array == 1)
          {
            ir_type = compute_field_array_ir_type(outer_tree);
            inest = op0;
            while (TREE_CODE (op0) != VAR_DECL && TREE_CODE (op0) != PARM_DECL 
      		&& TREE_CODE (op0) != INDIRECT_REF)
              {
              	switch (TREE_CODE (op0))
              	  {
              	    case COMPONENT_REF:
              	      in_nsubs = 0;
              	      break;
              	    case ARRAY_REF:
              	      inest = TREE_OPERAND (op0, 0);
              	      nsubs ++;
              	      in_nsubs ++;
              	      break;
              	    default:
              	      gcc_unreachable ();
              	  }
              	op0 = TREE_OPERAND (op0, 0);
              }
            op0 = inest;
          }
        else 
          while (TREE_CODE (op0) == ARRAY_REF)
            {
              op0 = TREE_OPERAND (op0, 0); /* loop through indexes */
              nsubs ++; /* just to figure out number of indexes in 'nsubs' */
            }
        arrloc_type = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        
        /* simplify 'var[const_int]' into 'var' within the same IR segment
           with offset = const_int * sizeof(element of array).
           That saves a lot of triples in IR representation.
           &var will later be recognized and simplified in dump_ir_modify() */
        if (1 && /* could hurt some benchmarks */
            nsubs == 1 /* one dimensional array */
            && TREE_CODE (TREE_OPERAND (stmt, 1)) == INTEGER_CST /* inc const index*/
            && TREE_CODE (op0) == VAR_DECL /* variable */
            && TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE /* of array type */
            && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (op0)))) 
               == INTEGER_CST /* array elements of constant size */)
          {
            int ind = TREE_INT_CST_LOW (TREE_OPERAND (stmt, 1));
            int offset;
            TYPE array_type = map_gnu_type_to_TYPE (TREE_TYPE (op0));
            IR_TYPE_NODE * array_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0));
            IR_NODE * ret;
            const char * name = get_ir_name (op0);
            
            /* deduct array lower bound from the index */
            ind -= get_array_lower_bound (TREE_TYPE (op0));
            
            offset = ind * TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (op0))));
            
            arrloc_ir_type = array_ir_type->t.a.element_type;
      
            ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
            
            if (TREE_PUBLIC (op0) || TREE_STATIC (op0) || DECL_EXTERNAL (op0))
              {
                ret = build_ir_extern_var (op0 /*VAR_DECL*/, offset /*off*/, 
                                           arrloc_type, arrloc_ir_type);
                /* pre-gcc 3.5.x style: */  
                {tree id = maybe_get_identifier (name); if (id) mark_referenced (id);}
                /* gcc 3.5.x style of marking used names */
                mark_decl_referenced (op0); 

                /* cleanup: now it's done in adjust_leaf_overlaps()
                if (ret->leaf.elvarno == -1)
                  add_ir_leaf_overlap (func_heap_leaf, (LEAF*)ret); */
              }
            else
	      {
                ret = build_ir_auto_var (name, lookup_auto_offset (op0, array_type) /*segoff*/, 
                                       array_type.size /*seglen*/,
                                       array_type.align /*segalign*/, offset /*off*/, 
                                       arrloc_type, arrloc_ir_type);
                SCOPE_TEMP_AS_PRIVATE_IFNEEDED(op0, name, ret);
              }

            if (TREE_THIS_VOLATILE (op0))
              ret->leaf.is_volatile = IR_TRUE;

            /* cleanup: now it's done in adjust_leaf_overlaps()
            ret->leaf.elvarno = ir_op0->leaf.elvarno; */
            /* cleanup: now it's done in build_ir_leaf() 
            set_leaf_pointerno (ret, arrloc_type.tword); */

            /* Don't generate darrayloc for the case spec_fd[0].limit (401:spec.c:300).*/
            if ( is_field_array == 1)
              {
                is_field_array = -1;
                gir_offset = NULL;
              }
	            
            if (map_for == MAP_FOR_VALUE)
              return ret;
            else
              {
                TYPE argtype = map_gnu_type_to_TYPE (ptr_stmt_type);
              
                /* This leave cannot reside in a register */
                /* cleanup: now it's done in adjust_leaf_overlaps()
                ret->leaf.no_reg = IR_TRUE;
                if (ret->leaf.elvarno == -1)
                  ret->leaf.elvarno = naliases++; */
                 
                /* C++ style addr_const */
                gcc_assert (ret->leaf.typep);
                ret = build_ir_addr_const (ret, 0, argtype,
                                           make_ptr_to_ir_type_node (ret->leaf.typep));
                return ret;
              }
          }

        /* For nested darrayloc. */ 
        if (is_field_array == 1 && TREE_CODE (op0) == INDIRECT_REF)
          {
            is_field_array = -1;
            saved_outer_tree = outer_tree;
            saved_gesize = g_esize;
            saved_offset = gir_offset;
            gir_offset = NULL;
          } 
           
        /* op0 can only be var_decl, array_type or indirect_ref now */
        if (TREE_CODE (TREE_TYPE (op0)) == POINTER_TYPE)
          /* a[i] where 'a' is a pointer type, was converted by build_array_ref()
             into array_ref. If so, op0 type should be a pointer_type. */
          ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE); 
        else
          ir_op0 = dump_ir_expr (op0, MAP_FOR_ADDR); 

        if (saved_outer_tree != NULL)
          {
            gcc_assert (TREE_CODE (op0) == INDIRECT_REF);
            is_field_array = 1;
            outer_tree = saved_outer_tree;
            g_esize = saved_gesize;
            gir_offset = saved_offset;
            saved_outer_tree = NULL;
          }

        /* in case we need '&ar[0]' return 'ar' */
        if (nsubs == 1 /* one dimensional array */
            && TREE_CODE (TREE_TYPE (op0)) == POINTER_TYPE
            && map_for == MAP_FOR_ADDR /* looking for address */
            && integer_zerop (TREE_OPERAND (stmt, 1)) /* zero index */
            && (INTEGRAL_TYPE_P (TREE_TYPE (stmt))
                || SCALAR_FLOAT_TYPE_P (TREE_TYPE (stmt)) /* scalar type */
                || POINTER_TYPE_P (TREE_TYPE (stmt))))
          {
            if (is_field_array != -1 && outer_tree == stmt)
              is_field_array = -1;

	    return ir_op0;
	  }

        if (is_field_array != 1)
          {        
            if (ir_op0->operand.tag == ISTRIPLE && ir_op0->triple.ldst_ir_type)
              {
                IR_TYPE_NODE *t = ir_op0->triple.ldst_ir_type;
                int i;
                
                for (i = 0; i < nsubs; i ++)
                  t = ir_decref (t);
                
                arrloc_ir_type = t;
              }
            else if (ir_op0->operand.tag == ISLEAF && ir_op0->leaf.typep)
              {
                IR_TYPE_NODE *t = ir_op0->leaf.typep;
                int i = 0;
                
                for (; i < nsubs; i ++)
                  t = ir_decref (t);
    
                if (TREE_CODE (TREE_TYPE (op0)) != POINTER_TYPE)
                  /* a[i] where 'a' is a pointer type, was converted by build_array_ref()
                     into array_ref. If so, op0 type should be a pointer_type.
                     do extra decref only for array_types */
                  t = ir_decref (t);
                    
                arrloc_ir_type = t;
              }
            else
                arrloc_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));
          }
        else
          arrloc_ir_type = compute_field_array_ir_type ( stmt );
                
        if (is_field_array == 1 && gir_offset)
          {
            ir_op0 = build_ir_triple (IR_PLUS, ir_op0, gir_offset, ir_op0->operand.type, ir_type); 
            gir_offset = NULL;
          }

        /* considering gfortran generate xxx[1] with lower bound '1' for params of length 1,
           gccfss just access it by base addr rather than generating array_loc for such case.  */
        if (nsubs == 1
            && ir_language == FORTRAN
            && TREE_CODE (op0) == INDIRECT_REF
            && TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE
            && TREE_CODE (TREE_OPERAND (op0, 0)) == PARM_DECL
            && get_array_type_size (TREE_TYPE (op0)) == 1
            && get_array_lower_bound (TREE_TYPE (op0)) == 1
            && TREE_CODE (TREE_OPERAND (stmt, 1)) == INTEGER_CST 
            && TREE_INT_CST_LOW (TREE_OPERAND (stmt, 1)) == 1)
          {
            if (map_for == MAP_FOR_ADDR)
              ret = ir_op0;
            else if (map_for == MAP_FOR_VALUE)
              ret = build_ir_triple (IR_IFETCH, ir_op0, NULL, arrloc_type, arrloc_ir_type);

            if (TREE_THIS_VOLATILE (stmt))
              ret->triple.is_volatile = IR_TRUE;

            return ret;
          }

        if (ir_op0->operand.tag == ISLEAF && ir_op0->leaf.class == ADDR_CONST_LEAF
            && nsubs == 1                               /* one dimension access */
            && TREE_CODE (op0) == STRING_CST            /* to "const string" */
            && TREE_CODE (TREE_OPERAND (stmt, 1)) == INTEGER_CST) /* with int index */
          {
            long long lowerbound = get_array_lower_bound (TREE_TYPE (op0));

            if (!PCC_ISPTR (ir_op0->leaf.type.tword)
                || !PCC_ISARY (PCC_DECREF (ir_op0->leaf.type.tword)) 
                || ir_op0->leaf.addressed_leaf->is_const != IR_TRUE)
              abort (); /* safety check */

            /* quick workaround for &"string"[0] */
            if (map_for == MAP_FOR_ADDR && integer_zerop (TREE_OPERAND (stmt, 1))
                && lowerbound == 0)
              return ir_op0;
            else if (map_for == MAP_FOR_VALUE) /* optimize "string"[1] to 't' */
              {
                const char * p;
                int ind;
                enum machine_mode mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (op0)));

                /* optimize only for string of char */
                if (GET_MODE_CLASS (mode) == MODE_INT
                    && GET_MODE_SIZE (mode) == 1)
                  {
                    p = TREE_STRING_POINTER (op0);
                    ind = TREE_INT_CST_LOW (TREE_OPERAND (stmt, 1));
                    /* deduct array lower bound from the index */
                    ind -= lowerbound;
                    ret = build_ir_int_const (p[ind], arrloc_type, arrloc_ir_type);
                    return ret;
                  }
              }
            /* else fall through. let regular processing do the work */
          }
	else if (default_opt_level >= 1
                 && map_for == MAP_FOR_VALUE 
                 /* only when value is returned.
                    otherwise &"1234"[2] could return the address
                    of some char '3', not the address to the 3rd element */
		 && TREE_READONLY (array)
                 && ! TREE_SIDE_EFFECTS (array)
		 && TREE_CODE (array) == VAR_DECL 
                 && DECL_INITIAL (array)
		 && TREE_CODE (DECL_INITIAL (array)) != ERROR_MARK
		 && targetm.binds_local_p (array))
	  {
            tree index = TREE_OPERAND (stmt, 1);
	    if (TREE_CODE (index) == INTEGER_CST)
	      {
		tree init = DECL_INITIAL (array);

		if (TREE_CODE (init) == CONSTRUCTOR)
		  {
		    tree value;
                    tree idx;
                    int i;
                    tree val = 0;

                    FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), i, idx, value)
		      if (tree_int_cst_equal (idx, index))
                        { 
                          val = value;
                          break;
                        }

		    if (val && !TREE_SIDE_EFFECTS (val))
		      return dump_ir_expr (fold (val), MAP_FOR_VALUE);
		  }
              }
            /* else fall through. let regular processing do the work */
          }

        if (is_field_array != 1)
          {
            if (ir_op0->operand.tag == ISTRIPLE
                && flag_triple_as_base /* true by default */
                && (ir_op0->triple.op == IR_CONV || ir_op0->triple.op == IR_PCONV
                    || ir_op0->triple.op == IR_PLUS)
                && ir_op0->triple.left->operand.tag == ISLEAF)
              { /* when op0 is (type*)leaf it's cheaper to regenerate that triple
                   than to create extra leaf */
                ir_op0_for_diminfo = dump_ir_expr (op0, MAP_FOR_ADDR); 
              }
            else if (ir_op0->operand.tag != ISLEAF)
              {
                IR_NODE * new_leaf = get_tmp_leaf (ir_op0->operand.type, 
                                                   make_ptr_to_ir_type_node (arrloc_ir_type));
    
                /* need a copy of ir_op0 to be used in 'arrayloc' and in 'diminfo' */
                build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, arrloc_ir_type);
    
                ir_op0_for_diminfo = ir_op0 = new_leaf;
              }
            else
              ir_op0_for_diminfo = ir_op0;
          }

        /* Allocate 'ntuple' vector, populate nsubs and element size entries */

        ntuple_pp = ir_proc_new_ntuple_array (irProc, (nsubs+2));
        ntuple_pp[0] = build_ir_int_const (nsubs + 1, inttype, 0);  /* nsubs + 1 */
	if (is_field_array == 1)
	  ntuple_pp[1] = build_ir_int_const (g_esize, inttype, 0); /*  the lowest level field's type size */
	else
        if (TREE_CODE (TYPE_SIZE (TREE_TYPE (stmt))) == INTEGER_CST)
          ntuple_pp[1] = build_ir_int_const (TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (stmt)))
                                             / BITS_PER_UNIT, inttype, 0); /* element size */
        else /* elements of the array are of variable length */
          {
            tree size_unit = TYPE_SIZE_UNIT (TREE_TYPE (stmt));
            IR_NODE * esize;
            
            if (TREE_CODE (size_unit) == NOP_EXPR)
              /* remove extra (unsigned) cast since size >=0 and
                 we hope it won't overlap */
              size_unit = TREE_OPERAND (size_unit, 0);

            esize = dump_ir_expr (size_unit, MAP_FOR_VALUE);
            
            /* make sure iropt doesn't see triples in 'esize' of arrayloc */
            if (esize->operand.tag != ISLEAF)
              {
                IR_NODE * var = get_tmp_leaf (esize->operand.type, 
                                              map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (size_unit)));
                
                build_ir_triple (IR_ASSIGN, var, esize, esize->operand.type, NULL);
                esize = var;
              }
            ntuple_pp[1] = esize; /* element size */
          }

        t = stmt;   /* back to top again */
        subindex = 1; /* current sub index */
	sub_subindex = 1;
        prev_size = -1; /* number of elmenents in previous array level */
        prev_ir_size = 0;
        do 
          {
            long long size;
            long long lowerbound = 0;
            IR_NODE * ir_size = 0;
            
            op0 = TREE_OPERAND (t, 0); /* array_ref or var_decl */
            op1 = TREE_OPERAND (t, 1); /* index */
            
            if (TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE
                && TYPE_SIZE (TREE_TYPE (op0)) 
                && TYPE_SIZE (TREE_TYPE (TREE_TYPE (op0))))
              {
                lowerbound = get_array_lower_bound (TREE_TYPE (op0));
                
                if (TREE_CODE (TYPE_SIZE (TREE_TYPE (op0))) == INTEGER_CST)
                  /* number of elements accessible by the current index */
                  size = get_array_type_size (TREE_TYPE (op0));
                else /* variable length array */
                  {
                    size = 0;
                    if (TREE_OPERAND (t, 4))
                      {
                        tree sz = fold (build2 (PLUS_EXPR, TREE_TYPE (TREE_OPERAND (t, 4)), 
                                                TREE_OPERAND (t, 4), 
                                                build_int_cst (NULL_TREE, 1)));
                        ir_size = dump_ir_expr (sz, MAP_FOR_VALUE);
                        
                        if (ir_size->operand.tag != ISLEAF)
                          { /* need a copy of ir_size to be used in IR_BOUNDS 
                               and in IR_DIMINFO */
                            IR_NODE * new_leaf = get_tmp_leaf (ir_size->operand.type, 
                                 map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (TREE_OPERAND (t, 4))));

                            build_ir_triple (IR_ASSIGN, new_leaf, ir_size, 
                                             ir_size->operand.type, NULL);

                            ir_size = new_leaf;
                          }
                      }
#if 0
                    tree domain = TYPE_DOMAIN (TREE_TYPE (op0));
                    size = 0;
                    if (domain)
                      {
                        tree min = TYPE_MIN_VALUE (domain);
                        tree max = TYPE_MAX_VALUE (domain);
                        if (min && max && integer_zerop (min))
                          {
                            /* need to add 1, because max - is the last
                               referenced index in the current array dimension,
                               therefore dimension size is max+1 */
                            max = fold (build2 (PLUS_EXPR, TREE_TYPE (max), max,
                                         build_int_cst (NULL_TREE, 1)));
                            ir_size = dump_ir_expr (max, MAP_FOR_VALUE);
                          }
                      }
#endif
                    /* can not use 
                       ir_size = dump_ir_expr (TREE_OPERAND (t, 3), MAP_FOR_VALUE);
                       because it's a precalculated multiple from the beginning
                       of the array and not the size of the current dimension.
                       In order to use (t, 3) we would need to divide (t, 3) of
                       current domain with (t, 3) of previous domain in IR,
                       which would hurt performance a lot */
                  }
              }
            else if (is_field_array == 1
			&& TREE_CODE (TREE_TYPE (op0)) == POINTER_TYPE)
	      size = -1;
	    else
              size = 0;

            /* For nested darrayloc.*/
            if (is_field_array == 1 )
              {
                is_field_array = -1;
                saved_outer_tree = outer_tree;
                saved_gesize = g_esize;
              } 
            ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
            if (saved_outer_tree != NULL)
              {
                is_field_array = 1;
                outer_tree = saved_outer_tree;
                g_esize = saved_gesize;
                saved_outer_tree = NULL;
              }
            else
              is_field_array = -1;

            /* make sure we have size_t type of 'index' expression for array_ref */
            if (!TARGET_ARCH64 
                && ir_op1->operand.type.tword != PCC_INT) 
              ir_op1 = build_ir_triple (IR_CONV, ir_op1, NULL, 
                                   map_gnu_type_to_TYPE (integer_type_node),
                                   map_gnu_type_to_IR_TYPE_NODE (integer_type_node));
            else if (TARGET_ARCH64
                     && ir_op1->operand.type.tword != PCC_LONG)
              ir_op1 = build_ir_triple (IR_CONV, ir_op1, NULL,
                                   map_gnu_type_to_TYPE (long_integer_type_node),
                                   map_gnu_type_to_IR_TYPE_NODE (long_integer_type_node));
            if (is_field_array == 1)
              {
                ir_bounds = 
                  build_ir_triple (IR_BOUNDS, 
                                   /* leaf for lower bound */
                                   build_ir_int_const (lowerbound, longtype, 0),
                                   /* leaf for upper bound */
                                   size == 0 && ir_size ? 
                                   ir_size: build_ir_int_const (size, longtype, 0),
                                   longtype, NULL);
                if (subindex == 1)
    	          stride = TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (stmt)))/ BITS_PER_UNIT;
    	        else if (sub_subindex == 1)
    	          {
    		    if (TREE_CODE (TREE_TYPE (op0)) == POINTER_TYPE || TREE_CODE (TREE_TYPE (op0)) == ARRAY_TYPE )
    		      stride = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (op0))));
    		    else
    		      stride = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (op0)));
    	          }
    	        else
    	          stride = stride * prev_size;
    
                ir_diminfo = 
                  build_ir_triple (IR_DIMINFO, ir_bounds, build_ir_int_const(stride, longtype, 0),
                                   longtype, NULL);
              }
            else
              {
                ir_bounds =
                  build_ir_triple (IR_BOUNDS,
                                   /* leaf for lower bound */
                                   build_ir_int_const (lowerbound, longtype, 0),
                                   /* leaf for upper bound */
                                   size == 0 && ir_size ?
                                   build_ir_triple (IR_MINUS, ir_size,
                                                    build_ir_int_const(1, inttype, 0),
                                                    ir_size->operand.type, NULL) :
                                   build_ir_int_const (size - 1 + lowerbound, longtype, 0),
                                   longtype, NULL);
    
                ir_diminfo =
                  build_ir_triple (IR_DIMINFO, ir_bounds,
                                   (subindex != 1
                                    ? (prev_ir_size
                                       ? prev_ir_size
                                       : build_ir_int_const (prev_size, longtype, 0))
                                    : ir_op0_for_diminfo),
                                   longtype, NULL);
              }

            if (ir_language == FORTRAN && lowerbound != 0)
              /* iropt ignores lower bound in IR_BOUNDS during linearization.
                 here is the workaround */
              ir_subindex = 
                build_ir_triple (IR_SUBINDEX, 
                                 build_ir_triple (IR_MINUS, ir_op1, 
                                                  build_ir_int_const (lowerbound, longtype, 0), 
                                                  ir_op1->operand.type, NULL), 
                                 ir_diminfo, longtype, NULL);
            else
              ir_subindex = build_ir_triple (IR_SUBINDEX, ir_op1, ir_diminfo, longtype, NULL);

            ntuple_pp [subindex + 1] = ir_subindex;
            
            t = op0; /* loop through indexes */

            if (is_field_array == 1)
              {
	        if (TREE_CODE (op0) != ARRAY_REF)
	          sub_subindex = 0;

	        while (subindex < nsubs && TREE_CODE (op0) != ARRAY_REF)
	          op0 = TREE_OPERAND (op0, 0);
	        t = op0;
              }

            subindex ++;
	    sub_subindex ++;
            prev_size = size; /* to be referenced in ir_diminfo by the next index */
            prev_ir_size = ir_size;
          }
        while (TREE_CODE (op0) == ARRAY_REF);
                
        /* generate IR for the arrayloc */
	if (is_field_array == 1)
	  ir_arrloc = build_ir_triple (IR_DARRAYLOC, ir_op0, (IR_NODE *)ntuple_pp, map_gnu_type_to_TYPE (ptr_stmt_type), ir_type);
	else
          ir_arrloc = build_ir_triple (IR_ARRAYLOC, ir_op0, (IR_NODE *)ntuple_pp,
                                     map_gnu_type_to_TYPE (ptr_stmt_type), arrloc_ir_type); 
       
        ir_arrloc->triple.ldst_ir_type = arrloc_ir_type;
        ir_arrloc->triple.ldst_type = arrloc_type;
                    
        if (map_for == MAP_FOR_ADDR)
          ret = ir_arrloc;
        else if (map_for == MAP_FOR_VALUE)
          ret = build_ir_triple (IR_IFETCH, ir_arrloc, NULL, 
                                 ir_arrloc->triple.ldst_type,
                                 ir_arrloc->triple.ldst_ir_type);
        
        if (TREE_THIS_VOLATILE (stmt))
          ret->triple.is_volatile = IR_TRUE;
	
	if (is_field_array != -1 && outer_tree == stmt)
	  is_field_array = -1;
      }
      break;

    case VIEW_CONVERT_EXPR:
    case CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
    case NOP_EXPR:
    case FLOAT_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0);
        IR_NODE * ir_op0 = 0;
        IR_OP opcode;
        
        if (TREE_CODE (TREE_TYPE (op0)) == VECTOR_TYPE
            || TREE_CODE (TREE_TYPE (stmt)) == VECTOR_TYPE
            || TREE_CODE (stmt) == VIEW_CONVERT_EXPR)
          {
            tree t, var;
            
            /* copy op0 into temporary */
            var = create_tmp_var_raw (TREE_TYPE (op0), "__vis_tmp_var.");
            TREE_ADDRESSABLE (var) = 1;
            t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (op0), var, op0);
            TREE_SIDE_EFFECTS (t) = 1;
            dump_ir_stmt (t);
            
            if (get_type_size (TREE_TYPE (op0)) == get_type_size (TREE_TYPE (stmt)))
              {
                /* get the address of the temporary and load from it with new type */
                t = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (stmt)), var);
                t = build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (stmt)), t);
                t = build1 (INDIRECT_REF, TREE_TYPE (stmt), t);
                ret = dump_ir_expr (t, map_for);
                return ret;
              }
            else if (TREE_CODE (TREE_TYPE (stmt)) != VECTOR_TYPE)
              {
                /* get the address of the temporary and load from it with new type */
                tree intermed_type = 
                    lang_hooks.types.type_for_size (get_type_size (TREE_TYPE (op0)) * 8, 
                                                    TYPE_UNSIGNED (TREE_TYPE (op0)));
                t = build1 (ADDR_EXPR, build_pointer_type (intermed_type), var);
                t = build1 (NOP_EXPR, build_pointer_type (intermed_type), t);
                t = build1 (INDIRECT_REF, intermed_type, t);
                t = build1 (NOP_EXPR, TREE_TYPE (stmt), t);
                ret = dump_ir_expr (t, map_for);
                return ret;
              }
            else
              /* gnu c front-end should catch such cases and give error to user */
              abort ();
          }
        else if (TYPE_MODE (TREE_TYPE (op0)) == TYPE_MODE (TREE_TYPE (stmt))
                 && ((TREE_CODE (TREE_TYPE (op0)) == RECORD_TYPE
                      && TREE_CODE (TREE_TYPE (stmt)) == RECORD_TYPE)
                     || (TREE_CODE (TREE_TYPE (op0)) == UNION_TYPE
                         && TREE_CODE (TREE_TYPE (stmt)) == UNION_TYPE)))
          {
            /* ignore conversion if structs have the same size */
            if (get_type_size (TREE_TYPE (op0)) == get_type_size (TREE_TYPE (stmt)))
              /* could be asked to map_for_addr, though all other nop_expr suppose
                 to be map_for_value only */
              return dump_ir_expr (op0, map_for); 
            else
              abort (); /* should never see this */
          }
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
       
        if (skip_bool_conv
            && ir_language == CDOUBLEPLUS 
            && TREE_CODE (TREE_TYPE (stmt)) == BOOLEAN_TYPE)
          return ir_op0;

        if (PCC_ISPTR (argtype.tword)) 
	  {
	    tree tp1, tp2;
	    tp1 = TREE_TYPE(stmt);
	    tp2 = TREE_TYPE(op0);
	    while ( TREE_CODE(tp1) == POINTER_TYPE
		    && TREE_CODE(tp2) == POINTER_TYPE )
	    {
	      tp1 = TREE_TYPE(tp1);
	      tp2 = TREE_TYPE(tp2);
	    }
	    if ( tp1 == tp2)
	      opcode = -1;
	    else 
	      if ((ir_op0->operand.type.tword == PCC_INT
                      || ir_op0->operand.type.tword == PCC_UNSIGNED 
                      || PCC_ISPTR (ir_op0->operand.type.tword))
                   && ir_op0->operand.tag == ISLEAF)
                opcode = IR_PCONV; /* TODO do we need this optimization? */
	      else
	        opcode = IR_CONV;
	  }
        else
	  if (TREE_TYPE(stmt) == TREE_TYPE(op0))
	    opcode = -1;
	  else
            opcode = IR_CONV;

        /* generate simple type conversion.
           argtype and ir_op0->operant.type are NOT equal */
	if (opcode != -1)
          ret = build_ir_triple (opcode, ir_op0, NULL, argtype, 
                               map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));	else
	  ret = ir_op0;
        
        if (lang_hooks.reduce_bit_field_operations
            && TREE_CODE (TREE_TYPE (stmt)) == INTEGER_TYPE
            && argtype.size * BITS_PER_UNIT > TYPE_PRECISION (TREE_TYPE (stmt)))
          ret = ir_reduce_bit_field_operations (ret, TREE_TYPE (stmt));
      }
      break;

      /* unary arithmetic */
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* operand of unary operation */
        IR_NODE * ir_op0 = 0;

        if (TREE_CODE (TREE_TYPE (stmt)) == VECTOR_TYPE)
          return ir_dump_vis (stmt, op0, 0);
              
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);

        ret = build_ir_triple (conv_treecode2ir (stmt), ir_op0, NULL, 
                          ir_op0->operand.type, NULL);
      }
      break;

      /* binary arithmetic and logic expressions.  */
    case MULT_EXPR:
    case PLUS_EXPR:
    case POINTER_PLUS_EXPR:
    case MINUS_EXPR:
      
    case RDIV_EXPR: /* div for float types */
    case TRUNC_DIV_EXPR: /* regular div for int types */
    case TRUNC_MOD_EXPR:

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0 = 0, * ir_op1 = 0;
	/* temporary vars for R|LROTATE_EXPR */
	IR_NODE * ir_cmp = 0, * ir_modesize = 0, * ir_ret0 = 0, * ir_ret1 = 0;
	int rrotate=0;

        if (TREE_CODE (stmt) == POINTER_PLUS_EXPR)
	  gcc_assert (POINTER_TYPE_P (TREE_TYPE (op0)) && INTEGRAL_TYPE_P (TREE_TYPE (op1)));

        if (TREE_CODE (TREE_TYPE (stmt)) == VECTOR_TYPE)
          return ir_dump_vis (stmt, op0, op1);

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
       
        /* need to use longtype for op1==int_cst when left operand is a pointer */
        if ((TREE_CODE (stmt) == PLUS_EXPR 
             || TREE_CODE (stmt) == POINTER_PLUS_EXPR
             || TREE_CODE (stmt) == MINUS_EXPR)
            && TREE_CODE (op1) == INTEGER_CST
            && PCC_ISPTR (ir_op0->operand.type.tword))
          {
            ir_op1 = build_ir_int_const ((CONSZ)TREE_INT_CST_LOW (op1), offsettype, 0);
          }
        else if (TREE_CODE (op1) == INTEGER_CST
                 && (TREE_CODE (stmt) == LSHIFT_EXPR 
                     || TREE_CODE (stmt) == RSHIFT_EXPR
		     || TREE_CODE (stmt) == LROTATE_EXPR
		     || TREE_CODE (stmt) == RROTATE_EXPR))
          { /* right operand is always 'int' for rshift/lshift/rrotate/lrotate, no matter what
               TREE_TYPE (op1) is */
            ir_op1 = build_ir_int_const ((CONSZ)TREE_INT_CST_LOW (op1), inttype, 0);
          }
        else if (TREE_CODE (op1) == INTEGER_CST
                 && (TREE_CODE (stmt) == MULT_EXPR 
                     || TREE_CODE (stmt) == BIT_IOR_EXPR
                     || TREE_CODE (stmt) == BIT_XOR_EXPR
                     || TREE_CODE (stmt) == BIT_AND_EXPR))
                     /* want all of the ops here, but cond_elim will fail 
                        for compare ops if left op is ptr and right op is offsettype */
          { /* vectorization can create weird integer constants for mult.
               make sure right op matches left op */
            if (PCC_ISPTR (ir_op0->operand.type.tword))
              /* special case for ptr * int_const */
              ir_op1 = build_ir_int_const ((CONSZ)TREE_INT_CST_LOW (op1), 
                                           offsettype, 0);

            /* if type of left op == type of target -> trunc or widen right constant
               if size of type of left op > size of type of right op -> widen right const */
            else if (TREE_TYPE (op0) == TREE_TYPE (stmt)
                     || argtype.tword == ir_op0->operand.type.tword
                     || TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (op0))) >
                          TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (op1))))
              ir_op1 = build_ir_int_const ((CONSZ)TREE_INT_CST_LOW (op1), 
                                           ir_op0->operand.type, 0);
            /* if size of type of left op < size of type of right op -> widen left op */
            else if (TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (op0))) <
                          TREE_INT_CST_LOW (TYPE_SIZE_UNIT (TREE_TYPE (op1))))
              {
                ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
                gcc_assert (ir_op1->operand.tag == ISLEAF);
                ir_op0 = build_ir_triple (IR_CONV, ir_op0, NULL, ir_op1->operand.type,
                                          ir_op1->leaf.typep);
              }
            else /* same size, but not equal -> shouldn't be any, 
                    just convert right op to the type of left op */
              {
                ir_op1 = build_ir_int_const ((CONSZ)TREE_INT_CST_LOW (op1), 
                                             ir_op0->operand.type, 0);
              }

          }
        else
          {
            ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);

            /* cg needs op0 ullong to match op1 ullong.
               same for long/ulong in 64-bit */
            if ((PCC_ISLLONG (ir_op0->operand.type.tword) 
                 && ir_op1->operand.tag == ISLEAF && ir_op1->leaf.class == CONST_LEAF
                 && !PCC_ISLLONG (ir_op1->operand.type.tword))
                || (PCC_ISLONG (ir_op0->operand.type.tword) 
                    && ir_op1->operand.tag == ISLEAF && ir_op1->leaf.class == CONST_LEAF
                    && !PCC_ISLONG (ir_op1->operand.type.tword)
                    && TARGET_ARCH64))
              {
                /* new const_leaf with type of right operand set to 
                   llong/ullong type of left op */
                ir_op1 = build_ir_int_const ((CONSZ)ir_op1->leaf.val.cnst.c.i, 
                                  ir_op0->operand.type, 0);
              }
            else if ((!PCC_ISLLONG (ir_op0->operand.type.tword) 
                      && ir_op1->operand.tag == ISLEAF
                      && PCC_ISLLONG (ir_op1->operand.type.tword))
                     || (!PCC_ISLONG (ir_op0->operand.type.tword) 
                         && ir_op1->operand.tag == ISLEAF
                         && PCC_ISLONG (ir_op1->operand.type.tword)
                         && TARGET_ARCH64))
                     
              {
                ir_op0 = build_ir_triple (IR_CONV, ir_op0, NULL, ir_op1->operand.type,
                                          ir_op1->leaf.typep);
              }
          }
        
#ifdef IROPT_SIMPLIFY_BUG_WORKAROUND
        /* workaround for IROPT simplify bug ran within cond_elim phase */
        switch (TREE_CODE (stmt))
          {
          case LT_EXPR:
          case LE_EXPR:
          case GT_EXPR:
          case GE_EXPR:
            if (PCC_ISCHAR (ir_op0->operand.type.tword))
              {
                ir_op0 = build_ir_triple (IR_CONV, ir_op0, NULL, inttype, ir type for int);
              }
            break;
          default:
            break;
          }
#endif
        
        switch (TREE_CODE (stmt))
          {
          case MINUS_EXPR:
            /* ptr - ptr -> inttype in V8;   ptr - ptr -> longtype in V9*/
            if (PCC_ISPTR (ir_op0->operand.type.tword) 
                && PCC_ISPTR (ir_op1->operand.type.tword))
              ret = build_ir_triple (conv_treecode2ir (stmt), ir_op0, ir_op1, offsettype, NULL);
            else
	    {
              ret = build_ir_triple (conv_treecode2ir (stmt), ir_op0, ir_op1, ir_op0->operand.type, NULL);
              /* do conversion if needed */
              if (argtype.tword != ir_op0->operand.type.tword
		&& !PCC_ISPTR (ir_op0->operand.type.tword) 
		&& !PCC_ISPTR (ir_op1->operand.type.tword))
                ret = build_ir_triple (IR_CONV, ret, NULL, argtype, 
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
	    }
            break;
          case LT_EXPR:
          case LE_EXPR:
          case GT_EXPR:
          case GE_EXPR:
          case EQ_EXPR:
          case NE_EXPR:
            /* GCC use _Bool type for all compare expression. 
               Need to generate 'inttype' for IR instead */
            ret = build_ir_triple (conv_treecode2ir (stmt), ir_op0, ir_op1, inttype, NULL);

            /* if it's not _Bool, do a conversion */
            if (TREE_CODE (TREE_TYPE (stmt)) != BOOLEAN_TYPE && argtype.tword != PCC_INT)
              ret = build_ir_triple (IR_CONV, ret, NULL, argtype, 
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            break;
          case MULT_EXPR:
          case RSHIFT_EXPR:
          case LSHIFT_EXPR:
          case BIT_IOR_EXPR:
          case BIT_XOR_EXPR:
          case BIT_AND_EXPR:
            /* for shifts and mult use ir_op0 type as a type of triple */
            ret = build_ir_triple (conv_treecode2ir (stmt), ir_op0, ir_op1, 
                                   ir_op0->operand.type, NULL);
            /* do conversion if needed */
            if (argtype.tword != ir_op0->operand.type.tword)
              ret = build_ir_triple (IR_CONV, ret, NULL, argtype, 
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            break;
	  case RROTATE_EXPR:
	  case LROTATE_EXPR:
            /* do it as the IOR of two shifts.  I.e., to left rotate A
                 by N bits, compute (A << N) | ((unsigned) A >> (C - N))
                 where C is the bitsize of A. */
	    if (TREE_CODE (stmt) == RROTATE_EXPR) 
	      rrotate = 1;

	    if (TREE_CODE (op1) == INTEGER_CST)
	    {
	      ir_cmp = build_ir_int_const ((CONSZ)GET_MODE_BITSIZE(TYPE_MODE(TREE_TYPE(op1)))
					   -(CONSZ)TREE_INT_CST_LOW (op1), inttype, 0);
	    }
	    else
	    {
              if (ir_op1->operand.tag != ISLEAF)
              {
                IR_NODE * new_leaf_op1 = get_tmp_leaf (ir_op1->operand.type,
                                map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op1)));
                build_ir_triple (IR_ASSIGN, new_leaf_op1, ir_op1,
                                             ir_op1->operand.type, NULL);
                ir_op1 = new_leaf_op1;
              }
	      ir_modesize = build_ir_int_const ((CONSZ)GET_MODE_BITSIZE(TYPE_MODE(TREE_TYPE(op1))), 
						inttype, 0);
	      ir_cmp = build_ir_triple (IR_MINUS, ir_modesize, ir_op1, ir_op1->operand.type, NULL); 
	    }
            if (ir_op0->operand.tag != ISLEAF)
            {
              IR_NODE * new_leaf_op0 = get_tmp_leaf (ir_op0->operand.type,
                              map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE(op0)));
              build_ir_triple (IR_ASSIGN, new_leaf_op0, ir_op0, ir_op0->operand.type, NULL);
              ir_op0 = new_leaf_op0;
            }
	    ir_ret0 = build_ir_triple (rrotate ? IR_RSHIFT : IR_LSHIFT, 
				       ir_op0, ir_op1, ir_op0->operand.type, NULL);
	    ir_ret1 = build_ir_triple (rrotate ? IR_LSHIFT : IR_RSHIFT, 
				       ir_op0, ir_cmp, ir_op0->operand.type, NULL);
	    ret = build_ir_triple (IR_OR, ir_ret0, ir_ret1, ir_ret0->operand.type, NULL);
            /* do conversion if needed */
            if (argtype.tword != ir_op0->operand.type.tword)
              ret = build_ir_triple (IR_CONV, ret, NULL, argtype, 
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
	    break;
          default:
            /* use GCC recommended type "argtype" for all other expressions */
            ret = build_ir_triple (conv_treecode2ir (stmt), ir_op0, ir_op1, argtype, map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            break;
          }
       
            
        if (lang_hooks.reduce_bit_field_operations
            && TREE_CODE (TREE_TYPE (stmt)) == INTEGER_TYPE
            && argtype.size * BITS_PER_UNIT > TYPE_PRECISION (TREE_TYPE (stmt)))
          ret = ir_reduce_bit_field_operations (ret, TREE_TYPE (stmt));
      }
      break;
      
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0, * ir_op1, * ret1, *ret2;
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
        ret1 = build_ir_triple (conv_treecode2ir (stmt), ir_op0, ir_op1, inttype, NULL);
        
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
        ret2 = build_ir_triple (IR_QUO, ir_op0, ir_op1, inttype, NULL);
        
        ret = build_ir_triple (IR_OR, ret1, ret2, inttype, NULL);
      }
      break;
    
    case LTGT_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0 = 0, * ir_op1 = 0;
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
        ret = build_ir_triple (IR_QGL, ir_op0, ir_op1, inttype, NULL);
      }
      break;
      
    case UNEQ_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0 = 0, * ir_op1 = 0;
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
        ret = build_ir_triple (IR_QGL, ir_op0, ir_op1, inttype, NULL);
        ret = build_ir_triple (IR_NOT, ret, 0, inttype, NULL);
      }
      break;
    
    case ORDERED_EXPR:
    case UNORDERED_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0 = 0, * ir_op1 = 0;
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);

        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);

        ret = build_ir_triple (IR_QUO, ir_op0, ir_op1, inttype, NULL);
        if (TREE_CODE (stmt) == ORDERED_EXPR)
          ret = build_ir_triple (IR_NOT, ret, 0, inttype, NULL);
      }
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0, * ir_op1, * new_leaf;

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);

        if (TYPE_UNSIGNED (TREE_TYPE (stmt)))
          {
            unsigned HOST_WIDE_INT d;

#define EXACT_POWER_OF_2_OR_ZERO_P(x) (((x) & ((x) - 1)) == 0)

            if (TREE_CODE (op1) != INTEGER_CST
                || !EXACT_POWER_OF_2_OR_ZERO_P (TREE_INT_CST_LOW (op1)))
              abort ();

            d = TREE_INT_CST_LOW (op1);

            ret = build_ir_triple (IR_PLUS, ir_op0, build_ir_int_const (d - 1, inttype, 0), argtype, NULL);
            ir_op1 = build_ir_int_const (d, inttype, 0);
            if (TREE_CODE (stmt) == CEIL_MOD_EXPR)
              ret = build_ir_triple (IR_REMAINDER, ret, ir_op1, argtype, NULL);
            else
              ret = build_ir_triple (IR_DIV, ret, ir_op1, argtype, NULL);
          }
        else
          {
            /*do it by conditionally adjusting op0.
                when (op1 < 0) and (op0 <0 ) op0 = op0 +op1 +1.
                when (op1 > 0) and (op0 > 0) op0= op0+ op1- 1 */

            IR_NODE *ir_cond, *ir_then, *ir_else, *tmp0, *orig_ir_op0;

            int label1 = gen_ir_label();
            int label2 = gen_ir_label();
            int label3 = gen_ir_label();
            int label4 = gen_ir_label();
            int label_exit = gen_ir_label();

            if (TREE_CODE (stmt) == CEIL_MOD_EXPR
	       && ir_op0->operand.tag != ISLEAF)
	    {
	       new_leaf = get_tmp_leaf (ir_op0->operand.type,
	          		        map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE(op0)));
	       build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);
	       ir_op0 = new_leaf;
	    }
            orig_ir_op0 = ir_op0;

            /* assign a temporary leaf for op0 in case left value be changed. */
            new_leaf = get_tmp_leaf (ir_op0->operand.type,
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0)));
            build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);
            ir_op0 = new_leaf;
            
	    ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);

	    if (ir_op1->operand.tag != ISLEAF)
	    {
	      new_leaf = get_tmp_leaf (ir_op1->operand.type,
			               map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE(op1)));
	      build_ir_triple (IR_ASSIGN, new_leaf, ir_op1, ir_op1->operand.type, NULL);
	      ir_op1 = new_leaf;
	    }

	    ir_cond = build_ir_triple (IR_LT, ir_op1, build_ir_int_const (0, inttype, 0), argtype, NULL);
	    ir_then = build_ir_labelref (label1, 1);
	    ir_else = build_ir_labelref (label2, 0);
	    {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label1);
            ir_cond = build_ir_triple (IR_GT, ir_op0, build_ir_int_const (0, inttype, 0), argtype, NULL);
            ir_then = build_ir_labelref (label_exit, 1);
            ir_else = build_ir_labelref (label3, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label3);
                /* if (op1 < 0) and (op0 < 0) left = op0 + op1 + 1 */
            tmp0 = build_ir_triple (IR_PLUS, ir_op0, build_ir_int_const(1, inttype, 0), argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);
            tmp0 = build_ir_triple (IR_PLUS, ir_op0, ir_op1, argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);
            build_ir_goto (label_exit);

            build_ir_labeldef (label2);
            ir_cond = build_ir_triple (IR_GT, ir_op0, build_ir_int_const (0, inttype, 0), argtype, NULL);
            ir_then = build_ir_labelref (label4, 1);
            ir_else = build_ir_labelref (label_exit, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label4);
            /* if (op1 > 0) and (op0 > 0) left = op0 + op1- 1 */
            tmp0 = build_ir_triple (IR_MINUS, ir_op0, build_ir_int_const(1, inttype, 0), argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);
            tmp0 = build_ir_triple (IR_PLUS, ir_op0, ir_op1, argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);

            build_ir_labeldef (label_exit);
            if (TREE_CODE (stmt) == CEIL_MOD_EXPR)
            {
              /* remainder = op0 - (left / op1) * op1. 
                 Here, ir_op0 -> left, orig_ir_op0 -> op0. */
              ret = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
              ret = build_ir_triple (IR_MULT, ret, ir_op1, argtype, NULL);
              ret = build_ir_triple (IR_MINUS, orig_ir_op0, ret, argtype, NULL);
            }
            else
            {
              ret = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
            }
          }
      }
      break;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand of binary operation */
        IR_NODE * ir_op0, * ir_op1, * new_leaf;

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

        if (TYPE_UNSIGNED (TREE_TYPE (stmt)))
          { /* FLOOR_DIV == TRUNC_DIV for unsigned types */
            if (TREE_CODE (stmt) == FLOOR_MOD_EXPR)
              ret = build_ir_triple (IR_REMAINDER, ir_op0, ir_op1, argtype, NULL);
            else
              ret = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
          }
        else
          {
            /*do it by conditionally adjusting op0 */

	    IR_NODE *ir_cond, *ir_then, *ir_else, *tmp0, *orig_ir_op0;

	    int label1 = gen_ir_label();
	    int label2 = gen_ir_label();
	    int label3 = gen_ir_label();
	    int label4 = gen_ir_label();
	    int label_exit = gen_ir_label();

            if (TREE_CODE (stmt) == FLOOR_MOD_EXPR
	        && ir_op0->operand.tag != ISLEAF)
	    {
	       new_leaf = get_tmp_leaf (ir_op0->operand.type,
	          		        map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE(op0)));
	       build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);
	       ir_op0 = new_leaf;
	    }
            orig_ir_op0 = ir_op0;

            /* assign a temporary leaf for op0 in case left value be changed. */
            new_leaf = get_tmp_leaf (ir_op0->operand.type,
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0)));
            build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);
            ir_op0 = new_leaf;
            
            if (ir_op1->operand.tag != ISLEAF)
            {
              new_leaf = get_tmp_leaf (ir_op1->operand.type,
                                       map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op1)));
              build_ir_triple (IR_ASSIGN, new_leaf, ir_op1, ir_op1->operand.type, NULL);
              ir_op1 = new_leaf;
            }

            ir_cond = build_ir_triple (IR_LT, ir_op1, build_ir_int_const (0, inttype, 0), argtype, NULL);
	    ir_then = build_ir_labelref (label1, 1);
	    ir_else = build_ir_labelref (label2, 0);
	    {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label1);
            ir_cond = build_ir_triple (IR_GT, ir_op0, build_ir_int_const (0, inttype, 0), argtype, NULL);
            ir_then = build_ir_labelref (label3, 1);
            ir_else = build_ir_labelref (label_exit, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            /* (op1 < 0) and (op0 > 0) , op0 = op0- op1- 1*/
            build_ir_labeldef (label3);
            tmp0 = build_ir_triple (IR_MINUS, ir_op0, build_ir_int_const(1, inttype, 0), argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);
            tmp0 = build_ir_triple (IR_MINUS, ir_op0, ir_op1, argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);
            build_ir_goto (label_exit);

            build_ir_labeldef (label2);
            ir_cond = build_ir_triple (IR_LT, ir_op0, build_ir_int_const (0, inttype, 0), argtype, NULL);
            ir_then = build_ir_labelref (label4, 1);
            ir_else = build_ir_labelref (label_exit, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            /* (op1 > 0) and (op0 < 0), op0 = op0- op1+ 1 */
            build_ir_labeldef (label4);
            tmp0 = build_ir_triple (IR_PLUS, ir_op0, build_ir_int_const(1, inttype, 0), argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);
            tmp0 = build_ir_triple (IR_MINUS, ir_op0, ir_op1, argtype, NULL);
            build_ir_triple (IR_ASSIGN, ir_op0, tmp0, argtype, NULL);

            build_ir_labeldef (label_exit);
            if (TREE_CODE (stmt) == FLOOR_MOD_EXPR)
            {
              /* remainder = op0 - (left / op1) * op1.
                 Here, ir_op0 -> left, orig_ir_op0 -> op0. */
              ret = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
              ret = build_ir_triple (IR_MULT, ret, ir_op1, argtype, NULL);
              ret = build_ir_triple (IR_MINUS, orig_ir_op0, ret, argtype, NULL);
            }
            else
            {
              ret = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
            }
          }
      }
      break;

    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand (divisor) */
        IR_NODE *ir_op0, * ir_op1, * quotient, * remainder, * ir_const0, * ir_const1;
        IR_NODE *ir_cond, * ir_then, * ir_else, * new_leaf, * temp;

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);

        if (ir_op0->operand.tag != ISLEAF)
          {
            new_leaf = get_tmp_leaf (ir_op0->operand.type,
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0)));
            build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);
            ir_op0 = new_leaf;
          }
        if (ir_op1->operand.tag != ISLEAF)
          {
            new_leaf = get_tmp_leaf (ir_op1->operand.type,
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op1)));
            build_ir_triple (IR_ASSIGN, new_leaf, ir_op1, ir_op1->operand.type, NULL);
            ir_op1 = new_leaf;
          }

        /* op0 / op1 = Q, op0 % op 1 = R */
        if (TREE_CODE (stmt) == ROUND_DIV_EXPR)
          {
            quotient = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
            new_leaf = get_tmp_leaf (quotient->operand.type,
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            build_ir_triple (IR_ASSIGN, new_leaf, quotient, quotient->operand.type, NULL);
            quotient = new_leaf;
          }

        remainder = build_ir_triple (IR_REMAINDER, ir_op0, ir_op1, argtype, NULL);
        new_leaf = get_tmp_leaf (remainder->operand.type,
                                 map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
        build_ir_triple (IR_ASSIGN, new_leaf, remainder, remainder->operand.type, NULL);
        remainder = new_leaf;

        ir_const0 = build_ir_int_const (0, inttype, 0);
        ir_const1 = build_ir_int_const (1, inttype, 0);

        if (TYPE_UNSIGNED (TREE_TYPE (stmt)))
          {
            /* if R < (op1 - 1)/2 then
                 Q = Q + 1, R = R - op1 */
            int label1 = gen_ir_label();
            int label_exit = gen_ir_label();
            IR_NODE * midd;

            midd = build_ir_triple (IR_MINUS, ir_op1, ir_const1, argtype, NULL);
            midd = build_ir_triple (IR_RSHIFT, midd, ir_const1, argtype, NULL);

            ir_cond = build_ir_triple (IR_GT, remainder, midd, argtype, NULL);
            ir_then = build_ir_labelref (label1, 1);
            ir_else = build_ir_labelref (label_exit, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label1);
            if (TREE_CODE (stmt) == ROUND_DIV_EXPR)
            {
              temp = build_ir_triple (IR_PLUS, quotient, ir_const1, argtype, NULL);
              build_ir_triple (IR_ASSIGN, quotient, temp, argtype, NULL);
            }
            else
            {
              temp = build_ir_triple (IR_MINUS, remainder, ir_op1, argtype, NULL);
              build_ir_triple (IR_ASSIGN, remainder, temp, argtype, NULL);
            }

            build_ir_labeldef (label_exit);
            if (TREE_CODE (stmt) == ROUND_DIV_EXPR)
               ret = quotient;
            else
               ret = remainder;
          }
        else
          {
            int label1 = gen_ir_label();
            int label2 = gen_ir_label();
            int label3 = gen_ir_label();
            int label4 = gen_ir_label();
            int label5 = gen_ir_label();
            int label_exit = gen_ir_label();
            IR_NODE * abs_remainder, * abs_op1, *mask;

            /* firstly, get abs results of remainder and ir_op1. */
            abs_remainder = get_tmp_leaf (remainder->operand.type,
                              map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            build_ir_triple (IR_ASSIGN, abs_remainder, remainder, argtype, NULL);
            abs_op1 = get_tmp_leaf (ir_op1->operand.type,
                              map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            build_ir_triple (IR_ASSIGN, abs_op1, ir_op1, argtype, NULL);

            ir_cond = build_ir_triple (IR_LT, remainder, ir_const0, argtype, NULL);
            ir_then = build_ir_labelref (label1, 1);
            ir_else = build_ir_labelref (label2, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label1);
            temp = build_ir_triple (IR_MINUS, ir_const0, remainder, argtype, NULL);
            build_ir_triple (IR_ASSIGN, abs_remainder, temp, argtype, NULL);

            build_ir_labeldef (label2);
            ir_cond = build_ir_triple (IR_LT, ir_op1, ir_const0, argtype, NULL);
            ir_then = build_ir_labelref (label3, 1);
            ir_else = build_ir_labelref (label4, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label3);
            temp = build_ir_triple (IR_MINUS, ir_const0, ir_op1, argtype, NULL);
            build_ir_triple (IR_ASSIGN, abs_op1, temp, argtype, NULL);

            /* if (2 * abs (R) >= abs (op1)) then
                 Q = Q + ((op0 ^ op1) >> (modesize - 1)) ^ 1 - (op0 ^ op1) >> (modesize - 1),
                 R = R - ((op0 ^ op1) >> (modesize - 1)) ^ op1 + (op0 ^ op1) >> (modesize - 1). */
            build_ir_labeldef (label4);
            abs_remainder = build_ir_triple (IR_LSHIFT, abs_remainder, ir_const1, argtype, NULL);
            ir_cond = build_ir_triple (IR_LT, abs_op1, abs_remainder, argtype, NULL);
            ir_then = build_ir_labelref (label5, 1);
            ir_else = build_ir_labelref (label_exit, 0);
            {
              TRIPLE *t = (TRIPLE *) ir_then;
              TAPPEND(t, (TRIPLE *) ir_else);
              ir_then = (IR_NODE *)t;
            }
            build_ir_triple (IR_CBRANCH, ir_cond, ir_then, argtype, NULL);

            build_ir_labeldef (label5);
            temp = build_ir_triple (IR_XOR, ir_op0, ir_op1, argtype, NULL);
            mask = build_ir_triple (IR_RSHIFT, temp,
                                    build_ir_int_const (GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (stmt)))- 1, inttype, 0),
                                    argtype, NULL);
            new_leaf = get_tmp_leaf (mask->operand.type,
                                     map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
            build_ir_triple (IR_ASSIGN, new_leaf, mask, mask->operand.type, NULL);
            mask = new_leaf;
            if (TREE_CODE (stmt) == ROUND_DIV_EXPR)
            {
               temp = build_ir_triple (IR_XOR, mask, ir_const1, argtype, NULL);
               temp = build_ir_triple (IR_MINUS, temp, mask, argtype, NULL);
               temp = build_ir_triple (IR_PLUS, quotient, temp, argtype, NULL);
               build_ir_triple (IR_ASSIGN, quotient, temp, argtype, NULL);
            }
            else
            {
               temp = build_ir_triple (IR_XOR, mask, ir_op1, argtype, NULL);
               temp = build_ir_triple (IR_MINUS, temp, mask, argtype, NULL);
               temp = build_ir_triple (IR_MINUS, remainder, temp, argtype, NULL);
               build_ir_triple (IR_ASSIGN, remainder, temp, argtype, NULL);
            }

            build_ir_labeldef (label_exit);
            if (TREE_CODE (stmt) == ROUND_DIV_EXPR)
               ret = quotient;
            else
               ret = remainder;
          }
      }
      break;
      
    case EXACT_DIV_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* left */
        tree op1 = TREE_OPERAND (stmt, 1); /* right operand (divisor) */
        IR_NODE *ir_op0;

        HOST_WIDE_INT d;
        unsigned HOST_WIDE_INT ml;
        int pre_shift;
                        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);

        if (TREE_CODE (op1) != INTEGER_CST 
            || HOST_BITS_PER_WIDE_INT < argtype.size * BITS_PER_UNIT)
          { /* cannot do exact div efficiently and IR doesn't have a way
               to represent division which is not supposed to need rounding,
               so fall to conservative */
            IR_NODE *ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
            
            ret = build_ir_triple (IR_DIV, ir_op0, ir_op1, argtype, NULL);
            break;
          }
            
        d = TREE_INT_CST_LOW (op1);
            
        /* optimize into shift and mul, insted of:
           ret = build_ir_triple (IR_DIV, ir_op0, 
                                  build_ir_int_const (d, inttype, 0), argtype, NULL); */

        pre_shift = floor_log2 (d & -d);
        ml = invert_mod2n (d >> pre_shift, argtype.size * BITS_PER_UNIT);
        
        ret = build_ir_triple (IR_RSHIFT, ir_op0, build_ir_int_const (pre_shift, argtype, 0), 
                          argtype, NULL);
        
        ret = build_ir_triple (IR_MULT, ret, build_ir_int_const (ml, argtype, 0), 
                          argtype, NULL);

      }
      break;

    case FUNCTION_DECL: 
      break;

    case SAVE_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* expression to be saved */
        ret = dump_ir_expr (op0, MAP_FOR_VALUE);
      }
      break;
      
    case INTEGER_CST:
      {
        unsigned long long i;

        if (TREE_INT_CST_HIGH (stmt) == 0 || TREE_INT_CST_HIGH (stmt) == -1)
          i = TREE_INT_CST_LOW (stmt);
        else
          gcc_assert (TYPE_MODE (TREE_TYPE (stmt)) == TImode);

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        if (i == 0 && argtype.tword == PCC_STRTY)
          {
            ret = get_tmp_leaf (argtype, map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
          }
        else
          {
            ret = build_ir_int_const (i, argtype, 
                                      map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
          }
      }
      break;
    case REAL_CST:
      {
        REAL_VALUE_TYPE d;
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        d = TREE_REAL_CST (stmt); 
        ret = build_ir_float_const (&d, argtype);
      }
      break;
    case STRING_CST:
      ret = dump_string_cst (stmt, map_for);
      break;
    case CALL_EXPR:
      /* if (TREE_CODE (TREE_OPERAND (stmt, 0)) == ADDR_EXPR
          && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0))
              == FUNCTION_DECL)
          && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0))) */ 
      if (TREE_CODE (CALL_EXPR_FN (stmt)) == ADDR_EXPR
          && (TREE_CODE (TREE_OPERAND (CALL_EXPR_FN (stmt), 0))
              == FUNCTION_DECL)
          && DECL_BUILT_IN (TREE_OPERAND (CALL_EXPR_FN (stmt), 0)))
        {       
          if (DECL_BUILT_IN_CLASS (TREE_OPERAND (CALL_EXPR_FN (stmt), 0))
              == BUILT_IN_FRONTEND)
            abort ();
          else
            ret = dump_ir_builtin_call (stmt, 1);
          break;
        }
      ret = dump_ir_call (stmt, 1/* function call. need return value*/);
      break;
    case LABEL_DECL:
      ret = 0;
      break;
    case CONSTRUCTOR:
      if (CONSTRUCTOR_ELTS (stmt) 
          && TREE_CODE (TREE_TYPE (stmt)) == VECTOR_TYPE) /* non zero initializer */
        {
          TYPE inner_type = map_gnu_type_to_TYPE (TREE_TYPE (TREE_TYPE (stmt)));
          IR_TYPE_NODE * ir_inner_type = 
              map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (TREE_TYPE (stmt)));

          TYPE ptr_to_inner = 
              map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (TREE_TYPE (stmt))));
          IR_TYPE_NODE * ir_ptr_to_inner = 
              map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (TREE_TYPE (TREE_TYPE (stmt))));

          IR_NODE * new_leaf = get_tmp_leaf (map_gnu_type_to_TYPE (TREE_TYPE (stmt)), 
                                             map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));
          IR_NODE * ptr_to_new_leaf;
          int eltpos;
          tree index, value;

          /* This leave cannot reside in a register */
          /* cleanup: now it's done in adjust_leaf_overlaps()
          new_leaf->leaf.no_reg = IR_TRUE;
          if (new_leaf->leaf.elvarno == -1)
             new_leaf->leaf.elvarno = naliases++; */
          
          ptr_to_new_leaf = build_ir_addr_const (new_leaf, 0,
                             map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (stmt))), 
                             map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (TREE_TYPE (stmt))));
         
	  FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (stmt), eltpos, index, value)
            {
              IR_NODE * ir_value, *ir_addr;
              int i;
             
              if (index)
                {
                  if (TREE_CODE (index) != INTEGER_CST)
                    abort ();
                  i = TREE_INT_CST_LOW (index) * inner_type.size;
                }
              else
                i = eltpos * inner_type.size;

        
              ir_value = dump_ir_expr (value, MAP_FOR_VALUE);
              
              ir_addr = build_ir_triple (IR_CONV, ptr_to_new_leaf, NULL, ptr_to_inner, 
                                    ir_ptr_to_inner);
          
              ir_addr = build_ir_triple (IR_PLUS, ir_addr, 
                                    build_ir_int_const (i, offsettype, 0), ptr_to_inner, 0);
              
              build_ir_triple (IR_ISTORE, ir_addr, ir_value, inner_type,
                          ir_inner_type);
            }

          if (map_for == MAP_FOR_VALUE)
            return new_leaf;
          else
            return ptr_to_new_leaf;
        }
      else
        ret = dump_ir_constructor (stmt, map_for);
      break;

    case CONST_DECL:
      ret = dump_ir_constructor (DECL_INITIAL (stmt), map_for);
      break;
      
    case VECTOR_CST:
      ret = dump_ir_constructor (stmt, map_for);
      break;

    case VA_ARG_EXPR:
       ret = dump_ir_builtin_va_arg (TREE_OPERAND (stmt, 0), TREE_TYPE (stmt));
       break;

    case REALPART_EXPR:
      ret = dump_ir_complexpart_expr (stmt, 1, map_for);
      break;
    
    case IMAGPART_EXPR:
      ret = dump_ir_complexpart_expr (stmt, 0, map_for);
      break;
    
    case COMPLEX_CST:
      {
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        if (TREE_CODE (TREE_REALPART (stmt)) == REAL_CST)
          {
            REAL_VALUE_TYPE d_real, d_imag;
            d_real = TREE_REAL_CST (TREE_REALPART (stmt)); 
            d_imag = TREE_REAL_CST (TREE_IMAGPART (stmt)); 
            ret = build_ir_complex_const (&d_real, &d_imag, argtype);
          }
        else /* nasty hack for lack of support of int-complex in cg */
          {
            /* assume that host long long can hold target int */
            long long n_real, n_imag;
            n_real = TREE_INT_CST_LOW (TREE_REALPART (stmt));
            n_imag = TREE_INT_CST_LOW (TREE_IMAGPART (stmt));
            argtype.tword = PCC_LDOUBLE_COMPLEX;
            argtype.size = 32;
            argtype.align = 16;
            ret = build_ir_int_complex_const (n_real, n_imag, argtype);
          }
      }
      break;
      
    case COMPLEX_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* real part of complex expression */
        tree op1 = TREE_OPERAND (stmt, 1); /* imag part of complex expression */
        IR_NODE *ir_op0, *ir_op1;
        TYPE imag_type;
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        if (map_for == MAP_FOR_ADDR)
          abort ();
       
        switch (TYPE_MODE (TREE_TYPE (stmt)))
          {
          case CSImode: 
            imag_type.tword = PCC_FLOAT_IMAGINARY; /*int complex*/
            imag_type.size = 4; imag_type.align = 4;
            break;
          case CDImode: 
            imag_type.tword = PCC_DOUBLE_IMAGINARY; /*long long complex*/
            imag_type.size = 8; imag_type.align = 8;
            break;
          case SCmode: 
            imag_type.tword = PCC_FLOAT_IMAGINARY; /*float complex*/
            imag_type.size = 4; imag_type.align = 4;
            break;
          case DCmode: 
            imag_type.tword = PCC_DOUBLE_IMAGINARY; /*double complex*/
            imag_type.size = 8; imag_type.align = 8;
            break;
          case TCmode: 
            imag_type.tword = PCC_LDOUBLE_IMAGINARY; /*long double complex*/
            imag_type.size = 16; imag_type.align = 8;
            break;
          default: fprintf (stderr, "unknown complex"); abort (); break;
          }

        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);

        if (TYPE_MODE (TREE_TYPE (stmt)) == CSImode)
          {
            ir_op0 = build_ir_triple (IR_CONV, ir_op0, 0, 
                                 map_gnu_type_to_TYPE (float_type_node),
                                 map_gnu_type_to_IR_TYPE_NODE (float_type_node));
            ir_op1 = build_ir_triple (IR_CONV, ir_op1, 0,
                                 map_gnu_type_to_TYPE (float_type_node),
                                 map_gnu_type_to_IR_TYPE_NODE (float_type_node));
          }
        else if (TYPE_MODE (TREE_TYPE (stmt)) == CDImode)
          {
            ir_op0 = build_ir_triple (IR_CONV, ir_op0, 0,
                                 map_gnu_type_to_TYPE (double_type_node),
                                 map_gnu_type_to_IR_TYPE_NODE (double_type_node));
            ir_op1 = build_ir_triple (IR_CONV, ir_op1, 0,
                                 map_gnu_type_to_TYPE (double_type_node),
                                 map_gnu_type_to_IR_TYPE_NODE (double_type_node));
          }
        ret = build_ir_triple (IR_MULT, ir_op1, build_ir_float_const (&dconst1, imag_type), imag_type, 0);
        ret = build_ir_triple (IR_PLUS, ir_op0, ret, argtype, 0);
      }
      break;

     /* Complex conjugate of operand */
    case CONJ_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* complex expression */
        tree t;
        tree imag_type = TREE_TYPE (TREE_TYPE (stmt));
        
        t = build1 (IMAGPART_EXPR, imag_type, op0);

        t = build1 (NEGATE_EXPR, imag_type, t);
        
        t = build2 (COMPLEX_EXPR, TREE_TYPE (stmt), 
                   build1 (REALPART_EXPR, imag_type, op0), t); 

        ret = dump_ir_expr (t, map_for);
      }
      break;
     
    case MIN_EXPR:
    case MAX_EXPR:
      {
        tree op0 = TREE_OPERAND (stmt, 0);
        tree op1 = TREE_OPERAND (stmt, 1);
        IR_NODE *ir_op0, *ir_op1;

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
        ret = build_ir_triple (TREE_CODE (stmt) == MAX_EXPR ? IR_MAX : IR_MIN, 
                          ir_op0, ir_op1, argtype, 0);
      }
      break;

    case ABS_EXPR:
      {
        const char * abs_name;
        tree op0 = TREE_OPERAND (stmt, 0); /* argument */
        tree fn, arglist, t;

        /* Unsigned abs is simply the operand.  
	   Testing here means we don't risk generating incorrect code below.  */
        if (TYPE_UNSIGNED (TREE_TYPE (stmt)))
          {
             ret = dump_ir_expr (op0, map_for);
             break;
          }

        if (INTEGRAL_TYPE_P (TREE_TYPE (stmt)))
          abs_name = "i_abs";
        else if (TREE_CODE (TREE_TYPE (stmt)) == REAL_TYPE)
          {
            if (TYPE_MODE (TREE_TYPE (stmt)) == TFmode)
              {
                /* implementation of fabsl() in IR */
                tree t, var, t2;
            
                /* copy op0 into temporary */
                var = create_tmp_var_raw (TREE_TYPE (op0), "__fabsl_tmp_var.");
                TREE_ADDRESSABLE (var) = 1;
                t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (op0), var, op0);
                dump_ir_stmt (t);
            
                /* get the address of the temporary */
                t = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (stmt)), var);
                t = build1 (NOP_EXPR, integer_ptr_type_node, t);
                t = build1 (INDIRECT_REF, integer_type_node, t);
                t = build2 (BIT_AND_EXPR, integer_type_node, t, 
                            build_int_cst (NULL_TREE, 0x7fffffff));
                
                t2 = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (stmt)), var);
                t2 = build1 (NOP_EXPR, integer_ptr_type_node, t2);
                t2 = build1 (INDIRECT_REF, integer_type_node, t2);
                t2 = build2 (GIMPLE_MODIFY_STMT, integer_type_node, t2, t);
                dump_ir_stmt (t2);

                ret = dump_ir_expr (var, map_for);
                break;
              }
            else switch (TYPE_MODE (TREE_TYPE (stmt)))
              {
              case SFmode:
                abs_name = "r_abs";
                break;
              case DFmode:
                abs_name = "d_abs";
                break;
              default:
                abort (); /* shouldn't see any other real types */
              }
          }
        else
          {
            gcc_unreachable ();
          }
        
        fn = build_decl (FUNCTION_DECL, get_identifier (abs_name), /* func name */
                         build_function_type (TREE_TYPE (stmt), /* return type */
                         build_tree_list (NULL_TREE, TREE_TYPE (stmt)))); /* arg1 type */
        arglist = build_tree_list (NULL_TREE, op0);
        DECL_ARTIFICIAL (fn) = 1;
        DECL_EXTERNAL (fn) = 1;
        TREE_PUBLIC (fn) = 1;
        TREE_NOTHROW (fn) = 1;
        TREE_READONLY (fn) = 1; /* never reads or writes globals. same as ECF_CONST */

        t = build_function_call_expr (fn, arglist);
        ret = dump_ir_call (t, 1);
        ret->triple.left->leaf.func_descr = INTR_FUNC;
       
#if 0
        IR_NODE *ir_op0, *ir_neg;

        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        
        ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
        
        if (ir_op0->operand.tag != ISLEAF)
          { /* need a copy of ir_op0 to be used in IR_MAX */
            IR_NODE * new_leaf = get_tmp_leaf (ir_op0->operand.type, 
                                               map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt)));

            build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);

            ir_op0 = new_leaf;
          }

        ir_neg = build_ir_triple (IR_NEG, ir_op0, NULL, argtype, NULL);
        
        ret = build_ir_triple (IR_MAX, ir_op0, ir_neg, argtype, 0);
#endif
      }
      break;
    
    case COND_EXPR: /* condition ? val1 : val2  proceesing */
      {
        tree op0 = TREE_OPERAND (stmt, 0); /* condition */
        tree op1 = TREE_OPERAND (stmt, 1); /* val1 */
        tree op2 = TREE_OPERAND (stmt, 2); /* val2 */
        IR_NODE *var;
        IR_NODE *ir_cond, *ir_then, *ir_else;
        int true_lab = gen_ir_label ();
        int false_lab = gen_ir_label ();
        int end_lab = gen_ir_label ();

        if (TREE_TYPE (stmt) == void_type_node)
        /* should be processed in dump_ir_stmt, but not in dump_ir_expr */
          abort (); 
        
        argtype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));
        
        /* tmp leaf to keap the value - cond ? val1 : val2; */
        var = get_tmp_leaf (argtype, 
                            map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt))); 
        
        ir_cond = dump_ir_expr (op0, MAP_FOR_VALUE);
        

        ir_then = build_ir_labelref (true_lab, 1);
        
        ir_else = build_ir_labelref (false_lab, 0);
        {
          TRIPLE *t = (TRIPLE *) ir_then;
          TAPPEND(t, (TRIPLE *) ir_else);
          ir_then = (IR_NODE *)t;
        }
        build_ir_triple (IR_CBRANCH, ir_cond, ir_then, longtype, NULL);
        
        /* true branch */
        build_ir_labeldef (true_lab);
        build_ir_triple (IR_ASSIGN, var, dump_ir_expr (op1, MAP_FOR_VALUE), 
                         var->operand.type, NULL);
        build_ir_goto (end_lab);
        
        /* false branch */
        build_ir_labeldef (false_lab);
        build_ir_triple (IR_ASSIGN, var, dump_ir_expr (op2, MAP_FOR_VALUE), 
                         var->operand.type, NULL);
        build_ir_goto (end_lab);
        
        /* end label */
        build_ir_labeldef (end_lab);
        ret = var; /* return tmp leaf */
      }
      break;
    
    case FILTER_EXPR:
      {
        ret = get_ir_exception_filter ();
        break;
      } 
    case EXC_PTR_EXPR:
      {
        ret = get_ir_exception_pointer ();
        break;
      }
    default: debug_tree (stmt); abort (); break;
    }

  if (errorcount != 0) 
    return get_err_triple ();
  else
    return ret;
}

/* generate single IR_PARAM triple */

static IR_NODE *
dump_ir_genargs (tree stmt)
{
  IR_NODE * ret = 0, * ir_op0;
  tree field, orig_stmt = stmt;
  tree node = TREE_TYPE (stmt);
  IR_TYPE_NODE * ir_type = NULL;

  if (TREE_CODE (node) == UNION_TYPE && TYPE_TRANSPARENT_UNION (node))
    {
       field = TYPE_FIELDS (node);
       stmt = build3 (COMPONENT_REF, TREE_TYPE (field), stmt,  field, NULL);
    }
 
  ir_op0 = dump_ir_expr (stmt, MAP_FOR_VALUE);
  stmt = orig_stmt;

  /* generate IR_TYPE_NODE if the ir_op0 is a PTR type. */
  if (PCC_ISPTR (ir_op0->operand.type.tword))
    if (ir_op0->operand.tag == ISTRIPLE && ir_op0->triple.typep)
      ir_type = ir_op0->triple.typep;

  /* generate function parameter*/
  ret = build_ir_triple (IR_PARAM, ir_op0, NULL, ir_op0->operand.type, ir_type);
  ret->triple.param_float_map = make_floatmap (TREE_TYPE (stmt));
  
  return ret;
}
                                 
/* generate IR_PBRANCH triple */
IR_NODE *
ir_pbranch (int action_num, int eh_landing_label, int fall_through_label)
{
  IR_NODE *action, *lab0, *lab1;
  TRIPLE *tmp;
  if (!last_may_cause_exception_node)
    abort ();
  if (last_may_cause_exception_node->triple.never_returns == IR_TRUE)
    {
      /* Required optimization: when a call never returns, the 
         fall_through_label should be the return_label.  Adjust
         the eh_landing_label if it is the same as fall_through_label.
       */
      if (eh_landing_label == fall_through_label)
        {
          if (cur_omp_context != NULL)
            {
              /* Redirect the exceptions to the openmp end pragma. */
              eh_landing_label = cur_omp_context->exception_label;
              fall_through_label = cur_omp_context->exception_label;
              cur_omp_context->lab_used = true;
            }
          else
            {
              eh_landing_label = return_label_num;
              fall_through_label = return_label_num;
            }
        }
      else if (cur_omp_context != NULL)
        {  
          fall_through_label = cur_omp_context->exception_label;
          cur_omp_context->lab_used = true;
        }
      else
        fall_through_label = return_label_num;
    }
  else
    {
      if (cur_omp_context != NULL)
	{
	  int region_number;
	  cur_omp_context->lab_used = true;
	  region_number = get_eh_region_number (current_eh_region);
	  add_region_list (cur_omp_context, region_number);
	}
    }

  /* generate new __EH_LFi node */
  action = (IR_NODE*) build_eh_leaf (action_num);

  if (eh_landing_label != fall_through_label && eh_landing_label != 0)
    {
      lab1 = build_ir_labelref (eh_landing_label, 1);
      lab0 = build_ir_labelref (fall_through_label, 0);
      tmp = (TRIPLE *) lab1;
      TAPPEND (tmp, (TRIPLE *) lab0);
      lab1 = (IR_NODE *)tmp;
    }
  else
    {
      /* no exception handler for this call in the current function */
      lab1 = build_ir_labelref (fall_through_label, 0);
    }
  cfun->uses_pbranch = 1;
  number_of_pbranch--;
  return build_ir_triple (IR_PBRANCH, action, lab1, longtype, NULL);
}

void
set_may_cause_exception (IR_NODE *node)
{
  node->triple.may_cause_exception = IR_TRUE;
  last_may_cause_exception_node = node;
  number_of_pbranch++;
}

static void 
check_stmt_eh_region (tree stmt, IR_NODE *ir_node)
{
  struct eh_region *region = find_eh_region (stmt);
  if (region)
    {
      set_may_cause_exception (ir_node);
      if (!current_eh_region)
        {
          if (number_of_pbranch != 1)
            abort ();
          current_eh_region = region;
        }
      else if (region != current_eh_region)
        abort ();
    }
  else
    {
      /* need a null call site info for every call */
      if (has_eh_region (cfun->eh) && !in_must_not_throw_region (stmt))
        set_may_cause_exception (ir_node);
    }
}

int 
is_gxx_operator_new (tree fn)
{
  /* recognize built-in operator new */
  if ((DECL_OVERLOADED_OPERATOR_P (fn) == NEW_EXPR
       || DECL_OVERLOADED_OPERATOR_P (fn) == VEC_NEW_EXPR)
      && (strcmp (get_ir_name (fn), "_Znwj") == 0
          || strcmp (get_ir_name (fn), "_Znaj") == 0))
    {
      return 1;
    }
  return 0;
}
    

/* generate IR_SCALL triple */
static IR_NODE *
dump_ir_call_main (tree stmt, int for_value, tree return_slot)
{
  IR_NODE * ir_arglist = 0;
  IR_NODE * ir_callnode;
  IR_NODE * ir_dest = 0;
  IR_NODE * ir_result;
  IR_NODE * ir_argp = 0;
  IR_NODE * chain_reg = 0;
  TYPE fncalltype;
  tree op0, op1, op2;
  int is_fake_call = 0, is_pure_call = 0;
  int has_hidden_result = 0;
  tree formal_type = 0;
  int formals_unknown = 0;
  IR_TYPE_NODE * fn_ir_type;
  int call_flags;

  if (TREE_CODE (stmt) != CALL_EXPR)
    {
      if (for_value)
        abort ();
      dump_ir_stmt (stmt);
      return NULL;
    }

  fn_ir_type = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (stmt));

  fncalltype = map_gnu_type_to_TYPE (TREE_TYPE (stmt));

  op0 = CALL_EXPR_FN (stmt); /* The call expression */
  op2 = CALL_EXPR_STATIC_CHAIN (stmt); /* chain reg. if not zero it's a call to nested function */

  if (0)
  /* The messages of __attribute__ "error" or "warning" are to be issued after
     dead code elimination or other optimizations. Seen in rfe 6788857. 
     Use rtl as backend to workaround the issue before gccfss propose a way 
     to pass the attributes to iropt. */ 
  {
    tree fndecl = get_callee_fndecl (stmt), attr;
    if (fndecl
        && (attr = lookup_attribute ("error",
                                     DECL_ATTRIBUTES (fndecl))) != NULL)
      error ("%Kcall to %qs declared with attribute error: %s",
                 stmt, lang_hooks.decl_printable_name (fndecl, 1),
                 TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr))));
    if (fndecl
        && (attr = lookup_attribute ("warning",
                                     DECL_ATTRIBUTES (fndecl))) != NULL)
      warning (0, "%Kcall to %qs declared with attribute warning: %s",
                   stmt, lang_hooks.decl_printable_name (fndecl, 1),
                   TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr))));
  }
 
  /* look for list of arg types. 
     triple.param_info field is used in v9 to correctly pass floating point types,
     ignored in v8 */
  if (TREE_CODE (op0) == ADDR_EXPR && TREE_CODE (TREE_OPERAND (op0, 0)) == FUNCTION_DECL)
    formal_type = TYPE_ARG_TYPES (TREE_TYPE (TREE_OPERAND (op0, 0)));
  
  else if (TREE_TYPE (op0) && TREE_TYPE (TREE_TYPE (op0)) && 
           (TREE_CODE (TREE_TYPE (TREE_TYPE (op0))) == FUNCTION_TYPE
            || TREE_CODE (TREE_TYPE (TREE_TYPE (op0))) == METHOD_TYPE))
     formal_type = TYPE_ARG_TYPES (TREE_TYPE (TREE_TYPE (op0)));

  if (formal_type == NULL_TREE)
    formals_unknown = 1;

  ir_dest = dump_ir_expr (op0, MAP_FOR_VALUE);

  if (!for_value && CALL_EXPR_RETURN_SLOT_OPT (stmt) && return_slot)
    {
      tree return_slot_addr;
      has_hidden_result = 1;
      return_slot_addr = build_fold_addr_expr (return_slot);
      STRIP_USELESS_TYPE_CONVERSION (return_slot_addr);
      ir_argp = dump_ir_genargs (return_slot_addr);
      ir_argp->triple.param_info = IrParamIsDeclared;

      if (ir_arglist == 0)
        ir_arglist = ir_argp;
      else
        {
          TRIPLE *l = (TRIPLE *) ir_arglist;
          TAPPEND (l, (TRIPLE *) ir_argp);
          ir_arglist = (IR_NODE *)l;
        }
      
    }

  /* functions that return structs and result is ignored, can not be translated
     to SCALL, make them FCALL. For C and C++ */
  else if (!for_value 
           && (TREE_CODE (TREE_TYPE (stmt)) == RECORD_TYPE 
               || TREE_CODE (TREE_TYPE (stmt)) == UNION_TYPE))
    {
      tree fndecl = get_callee_fndecl (stmt);
      if (IS_RECORD_TYPE (TREE_TYPE (stmt))
          /* small structures are supposed to be in registers in 64-bit, but
             decl_by_reference forces them to be passed through memory 
             with address in %o0. Make sure we emit ifetch/foreff in 
             such callsite, so CG will pass the temp struct addr in %o0 */
          || (fndecl && DECL_RESULT (fndecl)
              && DECL_BY_REFERENCE (DECL_RESULT (fndecl))))
        {
          for_value = 1;
          is_fake_call = 1;
        }
    }
  
  /* Ignore zero sized structs in C in v8 and v9 */
  if (for_value && IS_RECORD_TYPE (TREE_TYPE (stmt))
      && get_type_size (TREE_TYPE (stmt)) == 0)
    {
      /* gcc doesn't generate unimp 0. So we use scall instead and do not init sp+64 */
      for_value = 0;
      has_hidden_result = 0;
      is_fake_call = 0;
    }

  /* recognize pure function. */
  if (TREE_CODE (op0) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (op0, 0)) == FUNCTION_DECL
      && DECL_IS_PURE (TREE_OPERAND (op0, 0)))
    is_pure_call = 1;

  op1 = CALL_EXPR_ARGS (stmt);
  for (; op1 != NULL_TREE; op1 = TREE_CHAIN (op1))
    {
      ir_argp = dump_ir_genargs (TREE_VALUE (op1));
      if (ir_argp == 0) continue;

      if (formal_type && TREE_VALUE (formal_type) != void_type_node)
        {
          ir_argp->triple.param_info = IrParamIsDeclared;
          formal_type = TREE_CHAIN (formal_type);
        }
      else
        ir_argp->triple.param_info = formal_type || formals_unknown
                                     ? IrParamNotDeclared: IrParamIsEllipsis;

      /* mark PM_IN for pure function's parameters. */
      if (is_pure_call)
        ir_argp->triple.param_mode = PM_IN;

      if (ir_arglist == 0)
        ir_arglist = ir_argp;
      else
        {
          TRIPLE *l = (TRIPLE *) ir_arglist;
          TAPPEND (l, (TRIPLE *) ir_argp);
          ir_arglist = (IR_NODE *)l;
        }
    }
  
  if (op2) /* prepare call to nested function */
    {
      IR_NODE *chain_var = dump_ir_expr (op2, MAP_FOR_VALUE);
      IR_NODE *assign;
      
      if (chain_var->operand.tag == ISLEAF)
        chain_reg = get_ir_chain_reg (chain_var->operand.type, chain_var->leaf.typep);
      else
        chain_reg = get_ir_chain_reg (chain_var->operand.type, chain_var->triple.typep);

      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) chain_reg;
          LAPPEND (pinfo->u.s.firstprivate, lp);
        }

      assign = build_ir_triple (IR_ASSIGN, chain_reg, chain_var, chain_var->operand.type, NULL);
      assign->triple.is_volatile = IR_TRUE;
      /* will set reserved_tailcall flag for ir_callnode later to tell CG that
         %g2/%g5 register is used to pass chain_reg into nested function */
    }

  if (for_value)
    { 
      ir_callnode = build_ir_triple (IR_FCALL, ir_dest, ir_arglist, fncalltype, fn_ir_type);
      ((TRIPLE *)ir_callnode)->param_float_map = make_floatmap (TREE_TYPE (stmt));

      if (IS_RECORD_TYPE (TREE_TYPE (stmt)) || is_fake_call)
        {
          ir_callnode->triple.is_stcall = IR_TRUE;
          /* make it a pointer, but keep size and align from structure */
          ir_callnode->triple.type.tword = PCC_INCREF (fncalltype.tword); 
          ir_result = build_ir_triple (IR_IFETCH, ir_callnode, NULL, 
                                       fncalltype, fn_ir_type);
          if (is_fake_call)
            build_ir_triple (IR_FOREFF, ir_result, NULL, fncalltype, NULL);
        }
      else
        ir_result = ir_callnode;
    }
  else
    {
      ir_callnode = build_ir_triple (IR_SCALL, ir_dest, ir_arglist, fncalltype, fn_ir_type);
      ir_result = NULL;
    }
  
  if (TREE_THIS_VOLATILE (stmt))
    ir_callnode->triple.is_volatile = IR_TRUE;

  /* mark __attribute__ ((__pure__)) funcs as never_writes_global
     to help iropt do some optimization. */
  if (is_pure_call)
    ir_callnode->triple.never_writes_globals = 1; 
      
  /* do not tail call throw functions */
  /* mark throw calls as no-return and rarely-executed */
  if (TREE_CODE (op0) == ADDR_EXPR 
      && TREE_CODE (TREE_OPERAND (op0, 0)) == FUNCTION_DECL)
    {
      tree fn = TREE_OPERAND (op0, 0);
      const char *name = (* targetm.strip_name_encoding) (
                    IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn)));

      if (flag_tm_mode) 
        if (DECL_IS_TM_CALLABLE_P (fn)
            || DECL_IS_TM_ABORT_OK_P (fn))
          ir_callnode->triple.tm_callable = 1;

      if (!strcmp (name, "__cxa_throw")
          || !strcmp (name, "_Unwind_Resume")) 
        {
          ir_callnode->triple.dont_tailcall = IR_TRUE;
          ir_callnode->triple.never_returns = IR_TRUE;
          ir_callnode->triple.is_rarely_executed = IR_TRUE;
        }
      else if (!strcmp (name, "__cxa_begin_catch")
                || !strcmp (name, "__cxa_end_catch")) 
        {
          ir_callnode->triple.is_rarely_executed = IR_TRUE;
        }
      
      if (is_gxx_operator_new (fn))
          ir_callnode->triple.allocates_new_memory = IR_TRUE;

      if (DECL_LANGUAGE (fn) == lang_cplusplus
          && DECL_CONSTRUCTOR_P (fn))
        {
          TRIPLE *first_arg;
          
          /* Mark constructors as modifying virtual pointer
             fields. The first argument to the constructor
             is the instance which is modified, mark its
             access info field also
             
             Studio currently seems to makr only constructors
             this way. TODO! look into destructors, copy constrcutors
             and other support functions that could modify
             the virtual pointer in the instance. */
          
          ir_callnode->triple.access_info = cpp_vtptr_ref;
          
          /* The first argument to the constructor
             is the instance, mark it also as vtptr ref
             for the aliaser to be more agressive.
             ir_arglist points to the tail of the list
             of params, loop around to get the first one */
          
          if (ir_arglist)
            {
              first_arg = ir_arglist->triple.tnext;
              first_arg->access_info = cpp_vtptr_ref;
            }
        }
    }

  /* mark noreturn and longjump functions */
  call_flags = call_expr_flags (stmt);
  if (call_flags & ECF_NORETURN)
    ir_callnode->triple.never_returns = IR_TRUE;

  /* mark malloc-like calls */
  if (call_flags & ECF_MALLOC)
    ir_callnode->triple.allocates_new_memory = IR_TRUE;
      
  /* current function prepared the save area for nonlocal goto,
     mark all callees as tricky */
  if (cfun->nonlocal_goto_save_area)
    {
      if (ir_dest->operand.tag == ISLEAF)
        ir_dest->leaf.unknown_control_flow = IR_TRUE;
      /* TODO need to mark current function as tricky */
      /* func_entry_triple->reserved_tailcall = IR_TRUE; */
    }

  /* patch each IR_PARAM triple to refer back to the call triple */
  {
    TRIPLE *p = 0; 
    TFOR (p, (TRIPLE *) ir_arglist) 
      {
        ((IR_NODE *)p)->triple.right = ir_callnode;
      }
  }

  if (cfun->has_old_style_inline_asm)
    {
      /* pessimize optimizations */
      /* do not allow iropt to do tailcall optimizations */
      ir_callnode->triple.dont_tailcall = IR_TRUE;
    }

  /* it's a nested function call */
  if (op2)
    {
      /* set the flag to tell CG to treat %g2 as input reg in v8 and %g5 in v9 */
      ir_callnode->triple.reserved_tailcall = IR_TRUE; /*is_nested_func_call = IR_TRUE;*/

      /* do not allow iropt to do tailcall optimizations */
      ir_callnode->triple.dont_tailcall = IR_TRUE;

      if (TREE_CODE (op2) == ADDR_EXPR)
        {
          /* add a leaf that represents a FRAME. structure to the can_access list */
          ir_callnode->triple.can_access = 
              ir_copy_overlaps ((LEAF*)dump_ir_expr (TREE_OPERAND (op2, 0), MAP_FOR_VALUE));
        }
      else if (TREE_CODE (op2) == VAR_DECL || TREE_CODE (op2) == PARM_DECL)
        {
          /* add a leaf that was copied to %g2/%g5 to the can_access list */
          ir_callnode->triple.can_access = 
              ir_copy_overlaps ((LEAF*)dump_ir_expr (op2, MAP_FOR_VALUE));
        }
      else
        {
          /* add %g2/%g5 leaf to the can_access list */
          ir_callnode->triple.can_access = ir_copy_overlaps ((LEAF*)chain_reg);
        }
    }

  /* workaround to disable inlining for no-builtin-FUNCNAME */
  if (ir_dest->operand.tag == ISLEAF && ir_dest->leaf.is_volatile)
    {
      ir_callnode->triple.in_line = DO_NOT_INLINE_CALL;
    }
  
  if (call_flags & ECF_PURE) /* may read memory, but never writes */
    {
      ir_callnode->triple.never_writes_globals = IR_TRUE;
    }

  if ((ir_dest->operand.tag == ISLEAF && ir_dest->leaf.pure_func)
      || (call_flags & ECF_CONST)) /* never reads or writes memory */
    {
      ir_callnode->triple.never_reads_globals = IR_TRUE;
      ir_callnode->triple.never_writes_globals = IR_TRUE;
    }

  if (has_hidden_result)
    {
      /* using extended IR to pass struct values out of funcs */
      if (gxx_call_abi)
        ir_callnode->triple.is_stcall = IR_TRUE;
      ir_callnode->triple.type.tword = PCC_TVOID;
      gcc_assert (fncalltype.tword == PCC_STRTY || fncalltype.tword == PCC_LDOUBLE);
      /* ir_callnode->triple.type.size and type.align already 
         contain size and align of structure */
    }

  if (call_flags & ECF_NOTHROW)
    {
      /* callee doesn't throw an exception */
      if (ir_dest->operand.tag == ISLEAF)
        ir_dest->leaf.throw_nothing = IR_TRUE;
    }
  else
    check_stmt_eh_region (stmt, ir_callnode);
  
  return ir_result; 
}

static IR_NODE *
dump_ir_call (tree stmt, int for_value)
{
  return dump_ir_call_main (stmt, for_value, NULL_TREE);
}

/* generate '=' IR_ASSIGN triple */
static IR_NODE *
dump_ir_modify (tree stmt)
{
  IR_NODE * ret = 0, * ir_op0 = 0, * ir_op1 = 0;
  tree op0, op1;
  int is_indirect = 0;

  if (TREE_CODE (stmt) != GIMPLE_MODIFY_STMT)
    abort();
  
  op0 = GIMPLE_STMT_OPERAND (stmt, 0); /* left */
  op1 = GIMPLE_STMT_OPERAND (stmt, 1); /* right */
    
  if (TREE_CODE (op1) == CALL_EXPR && CALL_EXPR_RETURN_SLOT_OPT (op1)
      && TREE_CODE (CALL_EXPR_FN (op1)) == ADDR_EXPR)
    {
      tree fndecl = get_callee_fndecl (op1);
      if (fndecl && DECL_RESULT (fndecl)
          && DECL_BY_REFERENCE (DECL_RESULT (fndecl)))
        {
          /* paranoid check to make sure we don't screw up return_slot_opt
             convention */
          gcc_assert (CALLEXPR_IS_PUBLIC (op1));
        }
    }
  
  /* check for 'return slot' call first,
     since we want to emit it even if it returns an empty class
     see CR 6601435 */
  if (TREE_CODE (op1) == CALL_EXPR && CALL_EXPR_RETURN_SLOT_OPT (op1)
      && CALLEXPR_IS_PUBLIC (op1) /* internal convention. see cp/semantics.c */)
    {
      /* using extended IR to pass struct values out of funcs */
      return dump_ir_call_main (op1, 0, op0);
    }

  /* don't emit assignments for zero sized objects */
  if (int_expr_size (op1) == 0)
    {
      if (TREE_SIDE_EFFECTS (op1))
        {
          ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
          if (ir_op1 && ir_op1->operand.tag != ISLEAF)
            ret = build_ir_triple (IR_FOREFF, ir_op1, NULL, ir_op1->operand.type, NULL);
        }
      return 0;
    }
  else if (TREE_CODE (op1) == VECTOR_CST && TREE_VECTOR_CST_ELTS (op1) == NULL)
    return 0;

  ir_op0 = dump_ir_expr (op0, MAP_FOR_ADDR);

  ir_op1 = dump_ir_expr (op1, MAP_FOR_VALUE);
  
  if (errorcount != 0) 
    return 0;
  
  if (TREE_CODE (op0) == ARRAY_REF || TREE_CODE (op0) == INDIRECT_REF)
    is_indirect = 1;
  
  if (IS_RECORD_TYPE (TREE_TYPE (op0))
      /*(TREE_CODE (TREE_TYPE (op0)) == RECORD_TYPE 
       || TREE_CODE (TREE_TYPE (op0)) == UNION_TYPE)*/
      && TYPE_SIZE (TREE_TYPE (op0)) 
      && TREE_CODE (TYPE_SIZE (TREE_TYPE (op0))) == INTEGER_CST)
    is_indirect = 1;

  if (ir_op0->operand.tag == ISLEAF && ir_op0->leaf.class == VAR_LEAF
      && ir_op0->leaf.typep != NULL && ir_op0->leaf.typep->tag == ISTYPE
      && !is_indirect && !ir_op0->leaf.is_indirect)
    {
      /* convert op1 if it has different type than op0 */
      if (ir_op0->operand.type.tword != ir_op1->operand.type.tword
               && !TREE_LANG_FLAG_6 (stmt)) /* special assignment, don't bother converting */
        {
          ir_op1 = build_ir_triple (IR_CONV, ir_op1, NULL, ir_op0->operand.type, 
                               ir_op0->leaf.typep);
        }

      if (ir_op0->leaf.typep->tid == IR_BITFIELD)
        {
          if (ir_op0->leaf.typep->t.bf.bit_offset + ir_op0->leaf.typep->t.bf.bit_width 
              <= ir_op0->operand.type.size * 8)
            {
              /* when offset and size of bit-field fits into container type */
              ret = build_ir_triple (IR_BFINS, ir_op0, ir_op1, ir_op0->operand.type, NULL);
              ret->triple.bfoffset = ir_op0->leaf.typep->t.bf.bit_offset;
              ret->triple.bfwidth = ir_op0->leaf.typep->t.bf.bit_width;
            }
          else
            {
              ir_ADDRESS addr;
              LEAF * leaf_var = (LEAF*)ir_op0;
              LEAF * new_leaf;
              int size1 = leaf_var->type.size * 8 - leaf_var->typep->t.bf.bit_offset;
              int size2 = leaf_var->typep->t.bf.bit_offset + leaf_var->typep->t.bf.bit_width
                          - leaf_var->type.size * 8;
              
              if (ir_op1->operand.tag == ISTRIPLE)
                {
                  /* need a copy of ir_op1 to be used in 'rshift' and 'and' */
                  IR_NODE * op1_copy = get_tmp_leaf (leaf_var->type, leaf_var->typep);
                  build_ir_triple (IR_ASSIGN, op1_copy, ir_op1, leaf_var->type, NULL);
                  ir_op1 = op1_copy;
                }

              /* geting rid of couple right bits, that will be stored in second bfins */
              /* sign of rshift doesn't matter, because those bits not going to be used by
                 bfins anyway */

              ret = build_ir_triple (IR_RSHIFT, ir_op1, build_ir_int_const (size2, inttype, 0), 
                                     leaf_var->type, NULL);

              ret = build_ir_triple (IR_BFINS, ir_op0, ret, leaf_var->type, NULL);
              /* only using size1 bits for this bfins */
              ret->triple.bfoffset = leaf_var->typep->t.bf.bit_offset;
              ret->triple.bfwidth = size1;
        
              addr = leaf_var->val.addr;
            
              addr.offset += leaf_var->type.size;
            
              /* creating the same leaf with modified offset */
              new_leaf = build_ir_leaf (VAR_LEAF, leaf_var->type, leaf_var->typep, 
                                        (LEAF_VALUE *)&addr, IR_TRUE);
              
              new_leaf->pass1_id = build_ir_proc_string (leaf_var->pass1_id);
              new_leaf->visited = IR_FALSE;
            
              /* cleanup: now it's done in adjust_leaf_overlaps()
              new_leaf->elvarno = leaf_var->elvarno; */
              /* cleanup: now it's done in build_ir_leaf() 
              set_leaf_pointerno ((IR_NODE*)new_leaf, leaf_var->type.tword); */
              
              /* masking last bits of op1, which were not stored in first bfins */
              ret = build_ir_triple (IR_AND, ir_op1, 
                                     build_ir_int_const (~((~0)<<size2), leaf_var->type, 0), 
                                     leaf_var->type, NULL);

              /* using the rest of op1 for bfins */
              ret = build_ir_triple (IR_BFINS, (IR_NODE*)new_leaf, ret, leaf_var->type, NULL);
              ret->triple.bfoffset = 0;
              ret->triple.bfwidth = size2;
            }
        }
      else
        {
          ret = build_ir_triple (IR_ASSIGN, ir_op0, ir_op1, ir_op0->operand.type, NULL);
        }
    }
  else
    {
      tree ptr_op0_type = build_pointer_type (TREE_TYPE (op0));
        
      TYPE argtype = map_gnu_type_to_TYPE (TREE_TYPE (op0));
      IR_TYPE_NODE * typep;
      int orig_argtype_size = 0;
     
      if (ir_op0->operand.tag == ISTRIPLE && ir_op0->triple.ldst_ir_type)
        {
          typep = ir_op0->triple.ldst_ir_type;
          argtype = ir_op0->triple.ldst_type;
        }
      else
        typep = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (op0));

      /* convert op1 if it has different type than op0 */
      if (argtype.tword != ir_op1->operand.type.tword)
        {
          ir_op1 = build_ir_triple (IR_CONV, ir_op1, NULL, argtype, typep);
        }
      else if (argtype.size != ir_op1->operand.type.size
               && PCC_ISARY (argtype.tword)) /* specail case for shortened strings */
        {
           orig_argtype_size = argtype.size;
           if ((argtype.size < ir_op1->operand.type.size)
               && !(TYPE_SIZE (TREE_TYPE (op0))
                    && TREE_CODE (TYPE_SIZE (TREE_TYPE (op0))) != INTEGER_CST))/*exclude the variable length array*/
             abort ();
           argtype.size = ir_op1->operand.type.size;
        }
      
      if (typep->tid == IR_BITFIELD)
        {
          if (typep->t.bf.bit_offset + typep->t.bf.bit_width <= argtype.size * 8)
            {
              /* when offset and size of bit-field fits into container type */
              ret = build_ir_triple (IR_IBFINS, ir_op0, ir_op1, argtype, typep);
              ret->triple.bfoffset = typep->t.bf.bit_offset;
              ret->triple.bfwidth = typep->t.bf.bit_width;
            }
          else
            {
              int size1 = argtype.size * 8 - typep->t.bf.bit_offset;
              int size2 = typep->t.bf.bit_offset + typep->t.bf.bit_width - argtype.size * 8;
              IR_NODE * part1, * part2;
              
              IR_NODE * new_leaf = get_tmp_leaf (ir_op0->operand.type, 
                                                 map_gnu_type_to_IR_TYPE_NODE (ptr_op0_type));

              /* need a copy of ir_op0 to be used in both bfins */
              build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);
              
              if (ir_op1->operand.tag == ISTRIPLE)
                {
                  /* need a copy of ir_op1 to be used in 'rshift' and 'and' */
                  IR_NODE * op1_copy = get_tmp_leaf (ir_op1->operand.type, typep);
                  build_ir_triple (IR_ASSIGN, op1_copy, ir_op1, ir_op1->operand.type, NULL);
                  ir_op1 = op1_copy;
                }
              
              /* geting rid of couple right bits, that will be stored in second bfins */
              /* sign of rshift doesn't matter, because those bits not going to be used by
                 bfins anyway */
              part1 = build_ir_triple (IR_RSHIFT, ir_op1, 
                                       build_ir_int_const (size2, inttype, 0), 
                                       ir_op1->operand.type, NULL);

              /* using copy of ir_op0 in new_leaf to make iropt happy */
              ret = build_ir_triple (IR_IBFINS, new_leaf, part1, argtype, typep);
              /* only using size1 bits for this bfins */
              ret->triple.bfoffset = typep->t.bf.bit_offset;
              ret->triple.bfwidth = size1;
        
              /* masking last bits of op1, which were not stored in first bfins */
              part2 = build_ir_triple (IR_AND, ir_op1, 
                                       build_ir_int_const (~((~0)<<size2), 
                                                           ir_op1->operand.type, 0), 
                                       ir_op1->operand.type, NULL);

              /* using copy of ir_op0 in new_leaf to make iropt happy */
              ret = build_ir_triple (IR_PLUS, new_leaf, 
                                     build_ir_int_const (argtype.size, offsettype, 0), 
                                     ir_op0->operand.type, NULL);

              /* using the rest of op1 for bfins */
              ret = build_ir_triple (IR_IBFINS, ret, part2, argtype, typep);
              ret->triple.bfoffset = 0;
              ret->triple.bfwidth = size2;
            }
        }
      else
        {
      
          if (IS_RECORD_TYPE (TREE_TYPE (op0)))
            {
              int emit_foreff = 0; /* seems to be slightly better, not to emit ir_foreff */

              if (ir_op0->operand.tag == ISLEAF 
                  && ir_op0->leaf.class == ADDR_CONST_LEAF
                  && ir_op0->leaf.addressed_leaf->class == VAR_LEAF
                  /* indirect_ref of struct_type may have op0 - addr_expr 
                     pointer type to different type */
                  && ir_op0->leaf.addressed_leaf->type.tword == argtype.tword
                  && ir_op0->leaf.addressed_leaf->type.size == argtype.size)
                {
                  /* got addr_const of var_leaf and var_leaf is struct_type
                     convert "*&op0 = op1" into "op0 = op1" */
                  IR_NODE * str_val = (IR_NODE*)ir_op0->leaf.addressed_leaf;
                  argtype.align = MIN (ir_op0->leaf.addressed_leaf->type.align, argtype.align);
                  ret = build_ir_triple (IR_ASSIGN, str_val, ir_op1, argtype, NULL);
                  if (emit_foreff)
                    build_ir_triple (IR_FOREFF, ir_op0, NULL, argtype, NULL);
                }
              else if (ir_op1->operand.tag == ISLEAF && ir_op1->leaf.class == VAR_LEAF
                       /* some array_of_structs[const_index] may be optimized
                          to a single struct. we can't MAP_FOR_ADDR them */
                       && TREE_CODE (op1) != ARRAY_REF)
                {
                  /* generate ADDR_CONST for leaf struct/union to be used in foreff.
                     MAP_FOR_ADDR returns ADDR_CONST for structs/unions */
                  IR_NODE * str_addr = NULL;
                  if (emit_foreff)
                    {
                      str_addr = dump_ir_expr (op1, MAP_FOR_ADDR);
                      if (str_addr->operand.tag != ISLEAF 
                          || str_addr->leaf.class != ADDR_CONST_LEAF)
                        {
                          debug_tree (op1);
                          abort ();
                        }
                    }

                  if (ir_op0->operand.tag == ISLEAF)
                    {
                      ret = build_ir_triple (IR_ISTORE, ir_op0, ir_op1, argtype, typep);
                      if (emit_foreff)
                        build_ir_triple (IR_FOREFF, str_addr, NULL, argtype, NULL);
                    }
                  else
                    { 
                      IR_NODE * new_leaf = NULL;
                      if (emit_foreff)
                        {
                          new_leaf = get_tmp_leaf (str_addr->operand.type, 
                                                   map_gnu_type_to_IR_TYPE_NODE (ptr_op0_type));

                          /* need a copy of ir_op0 to be used in 'istore' and in 'foreff' */
                          build_ir_triple (IR_ASSIGN, new_leaf, str_addr, 
                                           str_addr->operand.type, NULL);
                        }
                      ret = build_ir_triple (IR_ISTORE, ir_op0, ir_op1, argtype, typep);
                      if (emit_foreff)
                        build_ir_triple (IR_FOREFF, new_leaf, NULL, argtype, NULL);
                    }
                }
              else if (ir_op0->operand.tag == ISLEAF && ir_op0->leaf.class == VAR_LEAF)
                {
                  ret = build_ir_triple (IR_ISTORE, ir_op0, ir_op1, argtype, typep);
                  if (emit_foreff)
                    build_ir_triple (IR_FOREFF, ir_op0, NULL, argtype, NULL);
                }
              else
                {
                  if (emit_foreff)
                    {
                      IR_NODE * new_leaf = get_tmp_leaf (ir_op0->operand.type, 
                                                   map_gnu_type_to_IR_TYPE_NODE (ptr_op0_type));
                                         /* TODO use make_ptr_to_ir_type_node() here ? */

                      /* need a copy of ir_op0 to be used in 'istore' and in 'foreff' */
                      build_ir_triple (IR_ASSIGN, new_leaf, ir_op0, ir_op0->operand.type, NULL);

                      ret = build_ir_triple (IR_ISTORE, new_leaf, ir_op1, argtype, typep);
                      build_ir_triple (IR_FOREFF, new_leaf, NULL, argtype, NULL);
                    }
                  else
                    ret = build_ir_triple (IR_ISTORE, ir_op0, ir_op1, argtype, typep);
                }

            }
          else
            { /* not a record type */
              if (ir_op0->operand.tag == ISLEAF 
                  && ir_op0->leaf.class == ADDR_CONST_LEAF
                  && ir_op0->leaf.addressed_leaf->class == VAR_LEAF
                  && ir_op0->leaf.addressed_leaf->type.tword == argtype.tword
                  && ir_op0->leaf.addressed_leaf->type.size == argtype.size
                  /* cannot do this optimizations if assigning arrays of different
                     size, since backend will look at the size of the left leaf
                     and not the type.size of ir_assign triple,
                     and we don't want to creat extra overlaping leaf just
                     for this assignment */
                  && !orig_argtype_size)
                {
                  /* got addr_const of var_leaf,
                     convert "*&op0 = op1" into "op0 = op1" */
                  argtype.align = MIN (ir_op0->leaf.addressed_leaf->type.align, argtype.align);
                  ret = build_ir_triple (IR_ASSIGN, (IR_NODE*)ir_op0->leaf.addressed_leaf, 
                                         ir_op1, argtype, NULL);
                }
              else
                ret = build_ir_triple (IR_ISTORE, ir_op0, ir_op1, argtype, typep);
            }
        }
      
      if (orig_argtype_size)
        {
          /* string assignment above will assign op1 to the lower portion of op0.
             the rest of op0 needs to be zeroed per C standard.
             orig_argtype_size always > argtype.size */
          tree args, t, addr, zero, size;
        
          t = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (op0)), op0);
          addr = build2 (PLUS_EXPR, build_pointer_type (TREE_TYPE (op0)), t,
                        build_int_cst (NULL_TREE, argtype.size));
          zero = build_int_cst (NULL_TREE, 0);
          size = build_int_cst (NULL_TREE, orig_argtype_size - argtype.size);
          
          args = tree_cons (NULL_TREE, addr,
                            tree_cons (NULL_TREE, zero,
                            build_tree_list (NULL_TREE, size)));

          t = build_function_call_expr (built_in_decls[BUILT_IN_MEMSET], args);
          dump_ir_call (t, 0);
        }
    }
  /* Mark the istore or assign of virtual table pointer
     as cpp_vtptr_ref. */
  if (ir_language == CDOUBLEPLUS
      && TREE_CODE (TREE_TYPE (op1)) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (op1))) == POINTER_TYPE
      && TREE_CODE (TREE_TYPE (TREE_TYPE (TREE_TYPE (op1)))) == FUNCTION_TYPE
      && is_virtual_pointer_type (TREE_TYPE (TREE_TYPE (op1))))
      ret->triple.access_info = cpp_vtptr_ref;

  if (TREE_THIS_VOLATILE (op0) || TREE_THIS_VOLATILE (op1))
    ret->triple.is_volatile = IR_TRUE;

  return ret;
}

static void
dump_ir_builtin_va_end (tree arglist) 
{   
  tree valist = TREE_VALUE (arglist);
    
  /* Evaluate for side effects, if needed. */
  if (TREE_SIDE_EFFECTS (valist))
    dump_ir_expr (valist, MAP_FOR_VALUE);
}   

/* Make it easier for the backends by protecting the valist argument
   from multiple evaluations.  */

static tree
stabilize_va_list (tree valist, int needs_lvalue)
{
  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      /* should never be the case for SPARC */
      abort ();
    }
  else
    {
/*      tree pt;*/

      if (! needs_lvalue)
	{
/*	  if (! TREE_SIDE_EFFECTS (valist))*/
	    return valist;

/*	  pt = build_pointer_type (va_list_type_node);
	  valist = fold (build1 (ADDR_EXPR, pt, valist));
	  TREE_SIDE_EFFECTS (valist) = 1;*/
	}

/*      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);*/
      valist = fold (build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)),
			     valist));
    }

  return valist;
}

/* Expand ARGLIST, from a call to __builtin_va_copy.  We do this as a
   builtin rather than just as an assignment in stdarg.h because of the
   nastiness of array-type va_list types.  */

static void
dump_ir_builtin_va_copy (tree arglist)
{
  tree dst, src, t;

  dst = TREE_VALUE (arglist);
  src = TREE_VALUE (TREE_CHAIN (arglist));

  dst = stabilize_va_list (dst, 1);
  src = stabilize_va_list (src, 0);

  if (TREE_CODE (va_list_type_node) != ARRAY_TYPE)
    {
      t = build2 (GIMPLE_MODIFY_STMT, va_list_type_node, dst, src);
      TREE_SIDE_EFFECTS (t) = 1;
      dump_ir_stmt (t);
    }
  else
    {
      /* should never be the case for SPARC */
      abort ();
    }
}

/* Expand ARGLIST, from a call to __builtin_next_arg.  */

static tree
dump_ir_builtin_next_arg (void)
{
  return fold (build1 (ADDR_EXPR, build_pointer_type (va_list_type_node),
                       __builtin_va_alist_node));
}

/* Expand ARGLIST, from a call to __builtin_va_start.  */

static void
dump_ir_builtin_va_start (tree exp)
{
  tree nextarg;
  tree valist, t;

  if (call_expr_nargs (exp) < 2)
    {
      error ("too few arguments to function %<va_start%>");
      return;
    }

  if (fold_builtin_next_arg (exp, true))
    return;

  nextarg = dump_ir_builtin_next_arg ();
  valist = stabilize_va_list (CALL_EXPR_ARG (exp, 0), 1);

  t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (valist), valist, nextarg);
  TREE_SIDE_EFFECTS (t) = 1;

  dump_ir_stmt (t);
}

static void
dump_ir_builtin_nonlocal_goto (tree stmt ATTRIBUTE_UNUSED, tree arglist)
{
  tree nl_goto_target = TREE_VALUE (arglist);
  tree nl_save_area = TREE_VALUE (TREE_CHAIN (arglist));
  IR_NODE *n, *fp, *target, *clobber;
  TRIPLE *args = NULL;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_TYPE_NODE * ir_argtype = map_gnu_type_to_IR_TYPE_NODE (ptr_type_node);
  
  if (TREE_CODE (TREE_TYPE (nl_goto_target)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (nl_save_area)) != POINTER_TYPE)
    abort ();
 
  n = dump_ir_expr (nl_goto_target, MAP_FOR_ADDR);
  target = get_tmp_leaf (argtype, ir_argtype);
  build_ir_triple (IR_ASSIGN, target, n, n->operand.type, NULL);
  

  n = dump_ir_expr (nl_save_area, MAP_FOR_ADDR);
  if (TARGET_ARCH64)
    n = build_ir_triple (IR_PLUS, n, build_ir_int_const (8, offsettype, 0), n->operand.type, 0);
  else
    n = build_ir_triple (IR_PLUS, n, build_ir_int_const (4, offsettype, 0), n->operand.type, 0);
  n = build_ir_triple (IR_IFETCH, n, NULL, argtype, ir_argtype);
  fp = get_tmp_leaf (argtype, ir_argtype);
  build_ir_triple (IR_ASSIGN, fp, n, n->operand.type, NULL);

  n = build_ir_triple (IR_ASM_INPUT, target, build_ir_string_const ("r"), argtype, NULL);
  n->triple.param_mode = PM_IN;
  TAPPEND (args, (TRIPLE *)n);
  
  n = build_ir_triple (IR_ASM_INPUT, fp, build_ir_string_const ("r"), argtype, NULL);
  n->triple.param_mode = PM_IN;
  TAPPEND (args, (TRIPLE *)n);
  
  clobber = build_ir_triple (IR_ASM_CLOBBER, build_ir_string_const ("memory"), NULL, argtype, NULL);
  clobber->triple.is_volatile = IR_TRUE;
  TAPPEND (args, (TRIPLE *)clobber);

  if (TARGET_ARCH64)
    n = build_ir_triple (IR_ASM_STMT, 
                         build_ir_string_const ("flushw\n"
                                                "mov\t%1,%%fp\n"
                                                "mov\t%0,%%g1\n\trestore\n\tjmp\t%%g1\n\t nop"), 
                         (IR_NODE*)args, argtype, NULL);
  else
    n = build_ir_triple (IR_ASM_STMT, 
                         build_ir_string_const ("ta\t3\n"
                                                "mov\t%1,%%fp\n"
                                                "mov\t%0,%%g1\n\trestore\n\tjmp\t%%g1\n\t nop"), 
                         (IR_NODE*)args, argtype, NULL);
  clobber->triple.right = n;
  n->triple.is_volatile = IR_TRUE;
  /* n->triple.may_cause_exception = IR_TRUE; */
  /* check_stmt_eh_region (stmt, n); */
  current_function_has_nonlocal_goto = 1;
}

static IR_NODE *
dump_ir_builtin_return_addr (tree fndecl, tree arglist, tree stmt, int need_return)
{
  if (arglist == 0)
    /* Warning about missing arg was already issued.  */
    return dump_ir_call (stmt, need_return);
  else if (! host_integerp (TREE_VALUE (arglist), 1))
    {
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
        error ("invalid arg to %<__builtin_frame_address%>");
      else
        error ("invalid arg to %<__builtin_return_address%>");
      return dump_ir_call (stmt, need_return);
    }
  else
    {
      enum built_in_function fndecl_code = DECL_FUNCTION_CODE (fndecl);
      int count = tree_low_cst (TREE_VALUE (arglist), 1);
      IR_NODE * fp, *tmp;
      int i;
      
      /* Some machines need special handling before we can access
         arbitrary frames.  For example, on the sparc, we must first flush
         all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
      if (count > 0)
        dump_ir_flushw (stmt);
#endif
      /* On the sparc, the return address is not in the frame, it is in a
         register.  There is no way to access it off of the current frame
         pointer, but it can be accessed off the previous frame pointer by
         reading the value from the register window save area.  */
#ifdef RETURN_ADDR_IN_PREVIOUS_FRAME
      if (fndecl_code == BUILT_IN_RETURN_ADDRESS)
        count--;
#endif

#ifndef SPARC_STACK_BIAS
      abort ();
#endif
          
      cfun->builtin_return_addr_called = 1;
      if (count == -1)
        {
          //return get_ir_i7_reg ();
          IR_NODE * reg_i7 = get_ir_i7_reg ();
          if (cur_omp_context && (cur_omp_context->pinfo
              || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
            {
              LIST *lp;
              PRAGMAINFO *pinfo;
              pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                             : cur_omp_context->prev_ctx->pinfo;
              lp = build_ir_proc_list ();
              lp->datap = (LDATA *) reg_i7;
              LAPPEND (pinfo->u.s.omp_private, lp);
            }
          return reg_i7;
        }
      
      fp = tmp = get_ir_frame_pointer_reg ();
      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) fp;
          LAPPEND (pinfo->u.s.omp_private, lp);
        }

      /* Scan back COUNT frames to the specified frame.  */
      for (i = 0; i < count; i++)
        {
          /* Assume the dynamic chain pointer is in the word that the
             frame address points to, unless otherwise specified.  */
          tmp = build_ir_triple (IR_PLUS, tmp, 
                                 build_ir_int_const (14 * UNITS_PER_WORD + SPARC_STACK_BIAS, 
                                                     offsettype, 0),
                                 fp->leaf.type, 0);
          tmp = build_ir_triple (IR_IFETCH, tmp, 0, fp->leaf.type, fp->leaf.typep);
        }

      /* For __builtin_frame_address, return what we've got. But on
         the SPARC for example, we may have to add a bias. */
      if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
        {
          if (TARGET_ARCH64)
            tmp = build_ir_triple (IR_PLUS, tmp, 
                                 build_ir_int_const (SPARC_STACK_BIAS, 
                                                     offsettype, 0),
                                 fp->leaf.type, 0);
          return tmp;
        }

      /* For __builtin_return_address, Get the return address from that
         frame.  */
      tmp = build_ir_triple (IR_PLUS, tmp, 
                             build_ir_int_const (15 * UNITS_PER_WORD + SPARC_STACK_BIAS, 
                                                 offsettype, 0),
                             fp->leaf.type, 0);
      return build_ir_triple (IR_IFETCH, tmp, 0, fp->leaf.type, fp->leaf.typep);
    }
}

static IR_NODE *
dump_ir_builtin_expect (tree stmt, tree arglist, int need_return)
{
  tree exp, c;
  IR_NODE * ret;

  if (arglist == NULL_TREE
      || TREE_CHAIN (arglist) == NULL_TREE)
    return dump_ir_call (stmt, need_return);

  exp = TREE_VALUE (arglist);
  c = TREE_VALUE (TREE_CHAIN (arglist));

  if (TREE_CODE (c) != INTEGER_CST)
    {
      error ("second arg to %<__builtin_expect%> must be a constant");
      c = integer_zero_node;
    }

  ret = dump_ir_expr (exp, MAP_FOR_VALUE);
  
  if (flag_guess_branch_prob)
    {
      /* pass branch "expectation" (integer_cst c) to cg somehow */
    }
  return ret;
}

static IR_NODE *
dump_ir_builtin_trap (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *n, *clobber;
  TRIPLE *args = NULL;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);

  clobber = build_ir_triple (IR_ASM_CLOBBER, build_ir_string_const ("memory"), NULL, argtype, NULL);
  clobber->triple.is_volatile = IR_TRUE;
  TAPPEND (args, (TRIPLE *)clobber);
  
  n = build_ir_triple (IR_ASM_STMT, build_ir_string_const ("ta\t5"), (IR_NODE*)args, argtype, NULL);
  clobber->triple.right = n;
  n->triple.is_volatile = IR_TRUE;
  /* n->triple.may_cause_exception = IR_TRUE; */
  /* check_stmt_eh_region (stmt, n); */
  return n;
}

static IR_NODE *
dump_ir_flushw (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *n, *clobber;
  TRIPLE *args = NULL;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);

  clobber = build_ir_triple (IR_ASM_CLOBBER, build_ir_string_const ("memory"), NULL, argtype, NULL);
  clobber->triple.is_volatile = IR_TRUE;
  TAPPEND (args, (TRIPLE *)clobber);

  if (TARGET_V9)
    n = build_ir_triple (IR_ASM_STMT, build_ir_string_const ("flushw"), (IR_NODE*)args, argtype, NULL);
  else
    n = build_ir_triple (IR_ASM_STMT, build_ir_string_const ("ta\t3"), (IR_NODE*)args, argtype, NULL);
  clobber->triple.right = n;
  n->triple.is_volatile = IR_TRUE;
  /* n->triple.may_cause_exception = IR_TRUE; */
  /* check_stmt_eh_region (stmt, n); */
  return n;
}

static IR_NODE *
dump_ir_builtin_profile_func (int is_enter)
{
  tree t, arglist;
  IR_NODE * ret;
  tree ptr_to_cfun = build1 (ADDR_EXPR, 
                             build_pointer_type (TREE_TYPE (current_function_decl)), 
                             current_function_decl);
  tree ret_addr_call = build_function_call_expr (built_in_decls[BUILT_IN_RETURN_ADDRESS], 
                                                 build_tree_list (NULL_TREE, integer_zero_node));
  tree fn = build_decl (FUNCTION_DECL, get_identifier (is_enter 
                                                       ? "__cyg_profile_func_enter"
                                                       : "__cyg_profile_func_exit"),
                        build_function_type (integer_type_node, NULL_TREE));
  DECL_ARTIFICIAL (fn) = 1;
  DECL_EXTERNAL (fn) = 1;
  TREE_PUBLIC (fn) = 1;
  TREE_NOTHROW (fn) = 1;

  arglist = tree_cons (NULL_TREE, ptr_to_cfun,
                       build_tree_list (NULL_TREE, ret_addr_call));

  t = build_function_call_expr (fn, arglist);
  ret = dump_ir_call (t, 0);
  return ret;
}

/* from builtins.c */
/* Return a char pointer for a C string if it is a string constant
   or sum of string constant and integer constant.  */
const char * c_getstr (tree src);
/* from builtins.c */
/* Create a new constant string literal and return a char* pointer to it.
   The STRING_CST value is the LEN characters at STR.  */
tree build_string_literal (int len, const char *str);

/* Expand a call to printf or printf_unlocked with argument list ARGLIST.
   UNLOCKED indicates this is a printf_unlocked call.  */
static IR_NODE *
dump_builtin_printf (tree stmt, tree arglist, int need_return, bool unlocked)
{
  tree fn_putchar = unlocked ? built_in_decls[BUILT_IN_PUTCHAR_UNLOCKED]
		             : implicit_built_in_decls[BUILT_IN_PUTCHAR];
  tree fn_puts = unlocked ? built_in_decls[BUILT_IN_PUTS_UNLOCKED]
			  : implicit_built_in_decls[BUILT_IN_PUTS];
  const char *fmt_str;
  tree fn, fmt, arg;

  /* If the return value is used, don't do the transformation.  */
  if (need_return)
    return dump_ir_call (stmt, need_return);

  /* Verify the required arguments in the original call.  */
  if (! arglist)
    return dump_ir_call (stmt, need_return);
  fmt = TREE_VALUE (arglist);
  if (TREE_CODE (TREE_TYPE (fmt)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  arglist = TREE_CHAIN (arglist);

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return dump_ir_call (stmt, need_return);

  /* If the format specifier was "%s\n", call __builtin_puts(arg).  */
  if (strcmp (fmt_str, "%s\n") == 0)
    {
      if (! arglist
          || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      fn = fn_puts;
    }
  /* If the format specifier was "%c", call __builtin_putchar(arg).  */
  else if (strcmp (fmt_str, "%c") == 0)
    {
      if (! arglist
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      fn = fn_putchar;
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (fmt_str, '%'))
        return dump_ir_call (stmt, need_return);

      if (arglist)
        return dump_ir_call (stmt, need_return);

      /* If the format specifier was "", printf does nothing.  */
      if (fmt_str[0] == '\0')
	return 0;
      
      /* If the format specifier has length of 1, call putchar.  */
      if (fmt_str[1] == '\0')
	{
	  /* Given printf("c"), (where c is any one character,)
	     convert "c"[0] to an int and pass that to the replacement
	     function.  */
	  arg = build_int_cst (NULL_TREE, fmt_str[0]);
	  arglist = build_tree_list (NULL_TREE, arg);
	  fn = fn_putchar;
	}
      else
	{
	  /* If the format specifier was "string\n", call puts("string").  */
	  size_t len = strlen (fmt_str);
	  if (fmt_str[len - 1] == '\n')
	    {
	      /* Create a NUL-terminated string that's one char shorter
		 than the original, stripping off the trailing '\n'.  */
	      char *newstr = alloca (len);
	      memcpy (newstr, fmt_str, len - 1);
	      newstr[len - 1] = 0;

	      arg = build_string_literal (len, newstr);
	      arglist = build_tree_list (NULL_TREE, arg);
	      fn = fn_puts;
	    }
	  else
	    /* We'd like to arrange to call fputs(string,stdout) here,
	       but we need stdout and don't have a way to get it yet.  */
            return dump_ir_call (stmt, need_return);
	}
    }

  if (!fn)
    return dump_ir_call (stmt, need_return);
  dump_ir_stmt (build_function_call_expr (fn, arglist));
  return 0;
}

/* Expand a call to fprintf or fprintf_unlocked with argument list ARGLIST.
   UNLOCKED indicates this is a fprintf_unlocked call.  */

static IR_NODE *
dump_builtin_fprintf (tree stmt, tree arglist, int need_return, bool unlocked)
{
  tree fn_fputc = unlocked ? built_in_decls[BUILT_IN_FPUTC_UNLOCKED]
			   : implicit_built_in_decls[BUILT_IN_FPUTC];
  tree fn_fputs = unlocked ? built_in_decls[BUILT_IN_FPUTS_UNLOCKED]
			   : implicit_built_in_decls[BUILT_IN_FPUTS];
  const char *fmt_str;
  tree fn, fmt, fp, arg;

  /* If the return value is used, don't do the transformation.  */
  if (need_return)
    return dump_ir_call (stmt, need_return);

  /* Verify the required arguments in the original call.  */
  if (! arglist)
    return 0;
  fp = TREE_VALUE (arglist);
  if (TREE_CODE (TREE_TYPE (fp)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  arglist = TREE_CHAIN (arglist);
  if (! arglist)
    return dump_ir_call (stmt, need_return);
  fmt = TREE_VALUE (arglist);
  if (TREE_CODE (TREE_TYPE (fmt)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  arglist = TREE_CHAIN (arglist);

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return dump_ir_call (stmt, need_return);

  /* If the format specifier was "%s", call __builtin_fputs(arg,fp).  */
  if (strcmp (fmt_str, "%s") == 0)
    {
      if (! arglist
          || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      arg = TREE_VALUE (arglist);
      arglist = build_tree_list (NULL_TREE, fp);
      arglist = tree_cons (NULL_TREE, arg, arglist);
      fn = fn_fputs;
    }
  /* If the format specifier was "%c", call __builtin_fputc(arg,fp).  */
  else if (strcmp (fmt_str, "%c") == 0)
    {
      if (! arglist
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      arg = TREE_VALUE (arglist);
      arglist = build_tree_list (NULL_TREE, fp);
      arglist = tree_cons (NULL_TREE, arg, arglist);
      fn = fn_fputc;
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (fmt_str, '%'))
        return dump_ir_call (stmt, need_return);

      if (arglist)
        return dump_ir_call (stmt, need_return);

      /* If the format specifier was "", fprintf does nothing.  */
      if (fmt_str[0] == '\0')
	{
	  /* Evaluate and ignore FILE* argument for side-effects.  */
	  dump_ir_expr (fp, MAP_FOR_VALUE);
	  return 0;
	}

      /* When "string" doesn't contain %, replace all cases of
	 fprintf(stream,string) with fputs(string,stream).  The fputs
	 builtin will take care of special cases like length == 1.  */
      arglist = build_tree_list (NULL_TREE, fp);
      arglist = tree_cons (NULL_TREE, fmt, arglist);
      fn = fn_fputs;
    }

  if (!fn)
    return dump_ir_call (stmt, need_return);
  
  dump_ir_stmt (build_function_call_expr (fn, arglist));
  return 0;
}

static IR_NODE *
dump_builtin_memset (tree stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree val = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  IR_NODE *ret, *ir_val;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (val)) != INTEGER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      /* Evaluate and ignore VAL in case it has side-effects.  */
      if (TREE_SIDE_EFFECTS (val)/* || TREE_CODE (val) != VAR_DECL*/)
        {
          ir_val = dump_ir_expr (val, MAP_FOR_VALUE);
          /* generate FOREFF only if 'val' is not leaf. 
             iropt will take care of the rest */
          if (ir_val->operand.tag != ISLEAF)
            ret = build_ir_triple (IR_FOREFF, ir_val, NULL, ir_val->operand.type, NULL);
        }
      return dump_ir_expr (dest, MAP_FOR_VALUE);
    }
  
  if (!host_integerp (len, 1) || tree_low_cst (len, 1) > 1)
    {
      return dump_ir_call (stmt, need_return);
    }
 
  /* remove extra pointer conversion to make sure that 'dest'
     will be converted to leaf */
  STRIP_NOPS (dest); 
                        
  ret = dump_ir_expr (dest, MAP_FOR_VALUE);
  ir_val = dump_ir_expr (val, MAP_FOR_VALUE);
  ir_val = build_ir_triple (IR_CONV, ir_val, 0, 
                       map_gnu_type_to_TYPE (intQI_type_node),
                       map_gnu_type_to_IR_TYPE_NODE (intQI_type_node));

  if (ret->operand.tag != ISLEAF && need_return)
    {
      IR_NODE * var = get_tmp_leaf (ret->operand.type, 
                                    map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (dest)));
      build_ir_triple (IR_ASSIGN, var, ret, ret->operand.type, NULL);
      ret = var;
    }
  
  build_ir_triple (IR_ISTORE, ret, ir_val, 
              map_gnu_type_to_TYPE (intQI_type_node),
              map_gnu_type_to_IR_TYPE_NODE (intQI_type_node));
  
  return ret;
}

bool readonly_data_expr (tree);

static IR_NODE*
dump_builtin_memcpy (tree stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree src = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  tree result;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);

  if (integer_onep (len))
    { /* memcpy (dest, src, 1) */
      tree new_dest = build1 (INDIRECT_REF, char_type_node, 
                     build1 (NOP_EXPR, ptr_type_node, dest));
      src = build1 (INDIRECT_REF, char_type_node, 
                     build1 (NOP_EXPR, const_ptr_type_node, src));
      result = build2 (GIMPLE_MODIFY_STMT, char_type_node, new_dest, src);
      dump_ir_stmt (result);
      return dump_ir_expr (dest, MAP_FOR_VALUE);
    }
  
  return dump_ir_call (stmt, need_return);
}

/* Expand expression EXP, which is a call to the strcpy builtin. */
static IR_NODE*
dump_builtin_strcpy (tree stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree src = TREE_VALUE (TREE_CHAIN (arglist));
  tree len;
  tree fn;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  
  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (operand_equal_p (src, dest, 0))
    return dump_ir_expr (fold_convert (TREE_TYPE (stmt), dest), MAP_FOR_VALUE);

  if (optimize_size)
    return dump_ir_call (stmt, need_return);

  fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
  if (!fn)
    return dump_ir_call (stmt, need_return);
  
  len = c_strlen (src, 1);
  if (! len || TREE_SIDE_EFFECTS (len))
    return dump_ir_call (stmt, need_return);

  len = size_binop (PLUS_EXPR, len, ssize_int (1));
  arglist = build_tree_list (NULL_TREE, len);
  arglist = tree_cons (NULL_TREE, src, arglist);
  arglist = tree_cons (NULL_TREE, dest, arglist);
  return dump_ir_call (fold_convert (TREE_TYPE (stmt),
		       build_function_call_expr (fn, arglist)), need_return);
}

/* Expand expression EXP, which is a call to the memmove builtin. */
static IR_NODE*
dump_builtin_memmove (tree stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree src = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);
  
  /* If src is categorized for a readonly section we can use
     normal memcpy.  */
  if (readonly_data_expr (src))
    {
      tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
      if (fn)
        {
          fn = build_function_call_expr (fn, arglist);
          if (TREE_CODE (fn) == CALL_EXPR)
            {
              CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (stmt);
              return dump_builtin_memcpy (fn, arglist, need_return);
            }
          else if (need_return || TREE_SIDE_EFFECTS (fn))
            {
              IR_NODE * ret = dump_ir_expr (fn, MAP_FOR_VALUE);
              
              if (need_return) /* expr will be used somewhere */
                return ret;
              
              /* generate FOREFF to make sure we don't have zombie triples */
              if (ret->operand.tag != ISLEAF)
                build_ir_triple (IR_FOREFF, ret, NULL, ret->operand.type, NULL);
              
              return 0;
            }
          else
            return 0;
        }
    }

  /* If length is 1 and we can expand memcpy call inline,
     it is ok to use mempcpy as well.  */
  if (integer_onep (len))
    {
      return dump_builtin_memcpy (stmt, arglist, need_return);
    }
  
  if (integer_zerop (len) && !need_return)
    /* can be seen in gcc.c-torture/execute/builtins/lib/memmove.c */
    return 0;

  /* Otherwise, call the normal function.  */
  return dump_ir_call (stmt, need_return);
}

/* Expand expression EXP, which is a call to the bcopy builtin. */
static IR_NODE*
dump_builtin_bcopy (tree stmt, tree arglist, int need_return)
{
  tree src = TREE_VALUE (arglist);
  tree dest = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  tree newarglist;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);
  
  /* New argument list transforming bcopy(ptr x, ptr y, int z) to
     memmove(ptr y, ptr x, size_t z).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bcopy instead of memmove.  */

  newarglist = build_tree_list (NULL_TREE, fold_convert (sizetype, len));
  newarglist = tree_cons (NULL_TREE, src, newarglist);
  newarglist = tree_cons (NULL_TREE, dest, newarglist);

  return dump_builtin_memmove (stmt, newarglist, need_return);
}

static IR_NODE *
dump_ir_builtin_init_trampoline (tree stmt, tree arglist, int need_return)
{
  tree tramp = TREE_VALUE (arglist);
  tree func = TREE_VALUE (TREE_CHAIN (arglist));
  tree ctx = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

  if (TREE_CODE (TREE_TYPE (tramp)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (func)) != POINTER_TYPE 
      || TREE_CODE (TREE_TYPE (ctx)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);

  if (TARGET_ARCH64)
    {
      /* SPARC 64-bit trampoline:
    
            rd	%pc, %g1
            ldx	[%g1+24], %g5
            jmp	%g5
            ldx	[%g1+16], %g5
            +16 bytes data
       */
      IR_NODE *ir_tramp, *t;
      TYPE uinttype = map_gnu_type_to_TYPE (unsigned_intSI_type_node);
      IR_TYPE_NODE * ir_uinttype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intSI_type_node);
      TYPE ulltype = map_gnu_type_to_TYPE (unsigned_intDI_type_node);
      IR_TYPE_NODE * ir_ulltype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intDI_type_node);
                             
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      t = build_ir_int_const (0x83414000, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, tramp),
            	  GEN_INT (trunc_int_for_mode (0x83414000, SImode)));*/

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (4, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xca586018, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 4)),
            	  GEN_INT (trunc_int_for_mode (0xca586018, SImode)));*/

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (8, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0x81c14000, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 8)),
            	  GEN_INT (trunc_int_for_mode (0x81c14000, SImode)));*/
      
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (12, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xca586010, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 12)),
            	  GEN_INT (trunc_int_for_mode (0xca586010, SImode)));*/

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (16, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, ulltype, ir_ulltype);
      /*emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, 16)), cxt);*/
      
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (24, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = dump_ir_expr (func, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, ulltype, ir_ulltype);
      /*emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, 24)), fnaddr);*/

     /* emit_insn (gen_flushdi (validize_mem (gen_rtx_MEM (DImode, tramp))));*/
    
      /*if (sparc_cpu != PROCESSOR_ULTRASPARC
          && sparc_cpu != PROCESSOR_ULTRASPARC3)
        emit_insn (gen_flushdi (validize_mem (gen_rtx_MEM (DImode, plus_constant (tramp, 8)))));*/
    
      /* Call __enable_execute_stack after writing onto the stack to make sure
         the stack address is accessible.  */
      func = build_decl (FUNCTION_DECL, get_identifier ("__enable_execute_stack"), /* func name */
                       build_function_type (void_type_node, /* return type */
                         build_tree_list (NULL_TREE, ptr_type_node))); /* arg1 type */
      arglist = build_tree_list (NULL_TREE, tramp);
      DECL_ARTIFICIAL (func) = 1;
      DECL_EXTERNAL (func) = 1;
      TREE_PUBLIC (func) = 1;
      TREE_NOTHROW (func) = 1;

      func = build_function_call_expr (func, arglist);
      dump_ir_call (func, 0);
      /*emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
                         LCT_NORMAL, VOIDmode, 1, tramp, Pmode);*/
    }
  else
    {
      /* SPARC 32-bit trampoline:
    
     	sethi	%hi(fn), %g1
     	sethi	%hi(static), %g2
     	jmp	%g1+%lo(fn)
     	or	%g2, %lo(static), %g2
    
        SETHI i,r  = 00rr rrr1 00ii iiii iiii iiii iiii iiii
        JMPL r+i,d = 10dd ddd1 1100 0rrr rr1i iiii iiii iiii
       */
    
      IR_NODE *ir_tramp, *ir_func, *ir_ctx, *t;
      TYPE uinttype = map_gnu_type_to_TYPE (unsigned_intSI_type_node);
      IR_TYPE_NODE * ir_uinttype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intSI_type_node);
                             
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_func = dump_ir_expr (func, MAP_FOR_VALUE);
      t = build_ir_triple (IR_RSHIFT, ir_func, build_ir_int_const (10, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x03000000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 0)),
         expand_binop (SImode, ior_optab,
            	   expand_shift (RSHIFT_EXPR, SImode, fnaddr,
            			 size_int (10), 0, 1),
            	   GEN_INT (trunc_int_for_mode (0x03000000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (4, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      ir_ctx = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_RSHIFT, ir_ctx, build_ir_int_const (10, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x05000000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 4)),
         expand_binop (SImode, ior_optab,
            	   expand_shift (RSHIFT_EXPR, SImode, cxt,
            			 size_int (10), 0, 1),
            	   GEN_INT (trunc_int_for_mode (0x05000000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (8, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      ir_func = dump_ir_expr (func, MAP_FOR_VALUE);
      t = build_ir_triple (IR_AND, ir_func, build_ir_int_const (0x3ff, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x81c06000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 8)),
         expand_binop (SImode, ior_optab,
            	   expand_and (SImode, fnaddr, GEN_INT (0x3ff), NULL_RTX),
            	   GEN_INT (trunc_int_for_mode (0x81c06000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (12, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      ir_ctx = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_AND, ir_ctx, build_ir_int_const (0x3ff, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x8410a000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 12)),
         expand_binop (SImode, ior_optab,
            	   expand_and (SImode, cxt, GEN_INT (0x3ff), NULL_RTX),
            	   GEN_INT (trunc_int_for_mode (0x8410a000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      /* On UltraSPARC a flush flushes an entire cache line.  The trampoline is
         aligned on a 16 byte boundary so one flush clears it all.  */
     /*  emit_insn (gen_flush (validize_mem (gen_rtx_MEM (SImode, tramp))));*/
      func = build_decl (FUNCTION_DECL, get_identifier ("__enable_execute_stack"), /* func name */
                       build_function_type (void_type_node, /* return type */
                         build_tree_list (NULL_TREE, ptr_type_node))); /* arg1 type */
      arglist = build_tree_list (NULL_TREE, tramp);
      DECL_ARTIFICIAL (func) = 1;
      DECL_EXTERNAL (func) = 1;
      TREE_PUBLIC (func) = 1;
      TREE_NOTHROW (func) = 1;

      func = build_function_call_expr (func, arglist);
      dump_ir_call (func, 0);
    } 
  return 0;
}

static IR_NODE *
dump_builtin_eh_return_data_regno (tree arglist)
{
  tree which = TREE_VALUE (arglist);
  unsigned HOST_WIDE_INT iwhich;

  if (TREE_CODE (which) != INTEGER_CST)
    {
      error ("argument of %<__builtin_eh_return_regno%> must be constant");
      return build_ir_int_const (-1, inttype, 0);
    }

  iwhich = tree_low_cst (which, 1);
  iwhich = EH_RETURN_DATA_REGNO (iwhich);
  if (iwhich == INVALID_REGNUM)
    return build_ir_int_const (-1, inttype, 0);

#ifdef DWARF_FRAME_REGNUM
  iwhich = DWARF_FRAME_REGNUM (iwhich);
#else
  iwhich = DBX_REGISTER_NUMBER (iwhich);
#endif

  return build_ir_int_const (iwhich, inttype, 0);
}

/* lower lfloor builtin, into expression to (int)(floor(x))  */
static IR_NODE *
dump_ir_builtin_int_roundingfn (tree stmt, int need_return)
{
  tree fndecl = get_callee_fndecl (stmt);
  tree arglist = CALL_EXPR_ARGS (stmt);
  enum built_in_function fallback_fn;
  tree fallback_fndecl;
  tree arg, exp;
  IR_NODE * ret;

  /* no need to validate arglist */

  arg = TREE_VALUE (arglist);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
      fallback_fn = BUILT_IN_CEIL;
      break;

    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      fallback_fn = BUILT_IN_FLOOR;
      break;

    default:
      gcc_unreachable ();
    }

  /* Fall back to floating point rounding optab.  */
  fallback_fndecl = mathfn_built_in (TREE_TYPE (arg), fallback_fn);
  gcc_assert (fallback_fndecl != NULL_TREE);
  exp = build_function_call_expr (fallback_fndecl, arglist);

  exp = build1 (NOP_EXPR, TREE_TYPE (stmt), exp);
  ret = dump_ir_expr (exp, MAP_FOR_VALUE);
  if (need_return)
    return ret;
  else
    return build_ir_triple (IR_FOREFF, ret, NULL, ret->operand.type, NULL);
}

/* map built-in function tree opcode into IR opcode */
static IR_OP
conv_c99_treecode2ir (tree node)
{
  switch (DECL_FUNCTION_CODE (node))
    {
    CASE_FLT_FN (BUILT_IN_SIGNBIT):
      return IR_SIGNBIT;
    CASE_FLT_FN (BUILT_IN_ISINF):
      return IR_ISINF;
    CASE_FLT_FN (BUILT_IN_ISNAN):
      return IR_ISNAN;
    CASE_FLT_FN (BUILT_IN_ISFINITE):
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
      return IR_ISFINITE;
    CASE_FLT_FN (BUILT_IN_ISNORMAL):
      return IR_ISNORMAL;
    CASE_FLT_FN (BUILT_IN_FPCLASSIFY):
      return IR_FPCLASSIFY;
    default:
      debug_tree (node);
      abort ();
      return IR_ERR;
    }
}

/* We could map the builtin's below to appropriate call in atomics.h on S10. 
   If can't, return NULL and later use inline templates. */
static char *
map_sync2solaris_fname (enum built_in_function fcode)
{
  switch (fcode)
    {
    case BUILT_IN_ADD_AND_FETCH_N:
      return "atomic_add_32";
    case BUILT_IN_ADD_AND_FETCH_1:
      return "atomic_add_8";
    case BUILT_IN_ADD_AND_FETCH_2:
      return "atomic_add_16";
    case BUILT_IN_ADD_AND_FETCH_4:
      return "atomic_add_32";
    case BUILT_IN_ADD_AND_FETCH_8:
      return "atomic_add_64";
    case BUILT_IN_OR_AND_FETCH_N:
      return "atomic_or_32";
    case BUILT_IN_OR_AND_FETCH_1:
      return "atomic_or_8";
    case BUILT_IN_OR_AND_FETCH_2:
      return "atomic_or_16";
    case BUILT_IN_OR_AND_FETCH_4:
      return "atomic_or_32";
    case BUILT_IN_OR_AND_FETCH_8:
      return "atomic_or_64";
    case BUILT_IN_AND_AND_FETCH_N:
      return "atomic_and_32";
    case BUILT_IN_AND_AND_FETCH_1:
      return "atomic_and_8";
    case BUILT_IN_AND_AND_FETCH_2:
      return "atomic_and_16";
    case BUILT_IN_AND_AND_FETCH_4:
      return "atomic_and_32";
    case BUILT_IN_AND_AND_FETCH_8:
      return "atomic_and_64";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_N:
      return "atomic_cas_32";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
      return "atomic_cas_8";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
      return "atomic_cas_16";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_4:
      return "atomic_cas_32";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
      return "atomic_cas_64";
    case BUILT_IN_LOCK_TEST_AND_SET_N:
      return "atomic_swap_32";
    case BUILT_IN_LOCK_TEST_AND_SET_1:
      return "atomic_swap_8";
    case BUILT_IN_LOCK_TEST_AND_SET_2:
      return "atomic_swap_16";
    case BUILT_IN_LOCK_TEST_AND_SET_4:
      return "atomic_swap_32";
    case BUILT_IN_LOCK_TEST_AND_SET_8:
      return "atomic_swap_64";
    default: 
      return NULL;
    }
}

static IR_NODE *
dump_ir_builtin_call (tree stmt, int need_return)
{
  tree fndecl = get_callee_fndecl (stmt);
  tree arglist = CALL_EXPR_ARGS (stmt);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  IR_NODE * ret = 0;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    {
      return dump_ir_call (stmt, need_return);
    }

  switch (fcode)
    {
    CASE_FLT_FN (BUILT_IN_SIGNBIT):
      /* If no header files found, gccfss help to generate IR_* independent of 'gccbuiltins.il'. */
      if (strncmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "__builtin_", 10) == 0)
        return dump_ir_call (stmt, need_return);

      /* fall through */
    CASE_FLT_FN (BUILT_IN_ISINF):
    CASE_FLT_FN (BUILT_IN_ISNAN):
    CASE_FLT_FN (BUILT_IN_ISFINITE):
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
    CASE_FLT_FN (BUILT_IN_ISNORMAL):
    CASE_FLT_FN (BUILT_IN_FPCLASSIFY):
      /* signbit, isinf, isnan, isfinite, isnormal, fpclassify are library micro defined in 'iso/math_c99.h'. */
      if (! arglist /* No args */
          || TREE_CHAIN (arglist) /* More than one arg */)
        return dump_ir_call (stmt, need_return);

      IR_NODE * ir_arg = dump_ir_expr (TREE_VALUE (arglist), MAP_FOR_VALUE);

      ret = build_ir_triple (conv_c99_treecode2ir (fndecl), ir_arg, NULL, inttype, 0);
      break;

    case BUILT_IN_CLZ:
      if (TARGET_ARCH64)
        {
          tree val = TREE_VALUE (arglist);
          TREE_VALUE (arglist) = build1 (NOP_EXPR, unsigned_intDI_type_node, val);
          ret = dump_ir_call (stmt, need_return);

          ret = build_ir_triple (IR_MINUS, ret, build_ir_int_const (32, ret->operand.type, 0), 
                                 ret->operand.type, 0);
          break;
        }
    case BUILT_IN_CTZ:
    case BUILT_IN_PARITY:
    case BUILT_IN_POPCOUNT:
      ret = dump_ir_call (stmt, need_return);
      break;
    case BUILT_IN_VA_START:
    case BUILT_IN_STDARG_START:
      dump_ir_builtin_va_start (stmt);
      break;
    case BUILT_IN_VA_END:
      dump_ir_builtin_va_end (arglist);
      break;
    case BUILT_IN_VA_COPY:
      dump_ir_builtin_va_copy (arglist);
      break;
    case BUILT_IN_STACK_SAVE:
      ret = get_ir_stack_pointer_reg ();
      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) ret;
          LAPPEND (pinfo->u.s.omp_private, lp);
        }

      break;
    case BUILT_IN_STACK_RESTORE:
      {
        IR_NODE * sp_reg = get_ir_stack_pointer_reg ();
        IR_NODE * var = dump_ir_expr (TREE_VALUE (arglist), MAP_FOR_VALUE);
        ret = build_ir_triple (IR_ASSIGN, sp_reg, var, sp_reg->operand.type, NULL);
        break;
      }
    case BUILT_IN_ADJUST_TRAMPOLINE:
      ret = dump_ir_expr (TREE_VALUE (arglist), MAP_FOR_VALUE);
      break;
    case BUILT_IN_INIT_TRAMPOLINE:
      ret = dump_ir_builtin_init_trampoline (stmt, arglist, need_return);
      break;
/*    case BUILT_IN_STACK_ALLOC:
      dump_ir_builtin_stack_alloc (arglist);
      break;*/
    case BUILT_IN_FRAME_ADDRESS:
    case BUILT_IN_RETURN_ADDRESS:
      ret = dump_ir_builtin_return_addr (fndecl, arglist, stmt, need_return);
      break;
    case BUILT_IN_NONLOCAL_GOTO:
      dump_ir_builtin_nonlocal_goto (stmt, arglist);
      break;
    case BUILT_IN_MEMSET:
      ret = dump_builtin_memset (stmt, arglist, need_return);
      break;
    case BUILT_IN_MEMMOVE:
      ret = dump_builtin_memmove (stmt, arglist, need_return);
      break;
    case BUILT_IN_STRCPY:
      ret = dump_builtin_strcpy (stmt, arglist, need_return);
      break;
    case BUILT_IN_BCOPY:
      ret = dump_builtin_bcopy (stmt, arglist, need_return);
      break;
    case BUILT_IN_FPRINTF:
      ret = dump_builtin_fprintf (stmt, arglist, need_return, 0);
      break;
    case BUILT_IN_FPRINTF_UNLOCKED:
      ret = dump_builtin_fprintf (stmt, arglist, need_return, 1);
      break;
    case BUILT_IN_PRINTF:
      ret = dump_builtin_printf (stmt, arglist, need_return, 0);
      break;
    case BUILT_IN_PRINTF_UNLOCKED:
      ret = dump_builtin_printf (stmt, arglist, need_return, 1);
      break;
    case BUILT_IN_STPCPY:
      {
        tree new_stmt = stmt;
        if (!need_return)
          {
            tree fn = implicit_built_in_decls[BUILT_IN_STRCPY];
            if (fn)
              new_stmt = build_function_call_expr (fn, arglist);
          }
        ret = dump_ir_call (new_stmt, need_return);
      }
      break;
    case BUILT_IN_MEMPCPY:
      {
        tree new_stmt = stmt;
        if (!need_return)
          {
            tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
            if (fn)
              new_stmt = build_function_call_expr (fn, arglist);
          }
        ret = dump_ir_call (new_stmt, need_return);
      }
      break;
    case BUILT_IN_EXPECT:
      ret = dump_ir_builtin_expect (stmt, arglist, need_return);
      break;
    case BUILT_IN_TRAP:
      ret = dump_ir_builtin_trap (stmt);
      break;
    case BUILT_IN_PROFILE_FUNC_ENTER:
      ret = dump_ir_builtin_profile_func (1);
      break;
    case BUILT_IN_PROFILE_FUNC_EXIT:
      ret = dump_ir_builtin_profile_func (0);
      break;
    case BUILT_IN_EH_RETURN_DATA_REGNO:
      ret = dump_builtin_eh_return_data_regno (arglist);
      break;
    case BUILT_IN_PREFETCH: 
      {
        tree arg0, arg1, arg2, t, fn;
        int op1, op2;
        const char * real_name;

        arg0 = TREE_VALUE (arglist);

        if (TREE_CODE (TREE_TYPE (arg0)) != POINTER_TYPE)
          return dump_ir_call (stmt, need_return);

        /* Arguments 1 and 2 are optional; argument 1 (read/write) defaults to
           zero (read) and argument 2 (locality) defaults to 3 (high degree of
           locality).  */
        if (TREE_CHAIN (arglist))
          {
            arg1 = TREE_VALUE (TREE_CHAIN (arglist));
            if (TREE_CHAIN (TREE_CHAIN (arglist)))
              arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
            else
              arg2 = build_int_cst (NULL_TREE, 3);
          }
        else
          {
            arg1 = integer_zero_node;
            arg2 = build_int_cst (NULL_TREE, 3);
          }

        /* Argument 1 (read/write flag) must be a compile-time constant int.  */
        if (TREE_CODE (arg1) != INTEGER_CST)
          {
            error ("second argument to %<__builtin_prefetch%> must be a constant");
            arg1 = integer_zero_node;
          }
        
        op1 = TREE_INT_CST_LOW (arg1);
        /* Argument 1 must be either zero or one.  */
        if (op1 != 0 && op1 != 1)
          {
            warning0 ("invalid second argument to %<__builtin_prefetch%>; using zero");
            op1 = 0;
          }

        /* Argument 2 (locality) must be a compile-time constant int.  */
        if (TREE_CODE (arg2) != INTEGER_CST)
          {
            error ("third argument to %<__builtin_prefetch%> must be a constant");
            arg2 = integer_zero_node;
          }
        
        op2 = TREE_INT_CST_LOW (arg2);
        /* Argument 2 must be 0, 1, 2, or 3.  */
        if (op2 < 0 || op2 > 3)
          {
            warning0 ("invalid third argument to %<__builtin_prefetch%>; using zero");
            op2 = 0;
          }

        if (op1 == 0) /* prefetch read */
          {
            if (op2 <=1)
              real_name = "__sparc_prefetch_read_once_intrinsic";
            else
              real_name = "__sparc_prefetch_read_many_intrinsic";
          }
        else /* prefetch write */
          {
            if (op2 <=1)
              real_name = "__sparc_prefetch_write_once_intrinsic";
            else
              real_name = "__sparc_prefetch_write_many_intrinsic";
          }
        
        fn = build_decl (FUNCTION_DECL, get_identifier (real_name),
                         build_function_type (void_type_node, NULL_TREE));
        DECL_EXTERNAL (fn) = 1;
        TREE_PUBLIC (fn) = 1;
        DECL_ARTIFICIAL (fn) = 1;
        TREE_NOTHROW (fn) = 1;

        t = build_function_call_expr (fn, arglist);
        ret = dump_ir_call (t, need_return);
      }
      break;

    case BUILT_IN_GOMP_BARRIER:
      dump_omp_barrier (stmt);
      break;

    case BUILT_IN_GOMP_ATOMIC_START:
      dump_omp_atomic_start (stmt);
      break;

    case BUILT_IN_GOMP_ATOMIC_END:
      dump_omp_atomic_end (stmt);
      break;

    case BUILT_IN_SYNCHRONIZE:
      dump_omp_flush (stmt);
      break;

    /* We could map all the builtin's below to
       appropriate call in atomics.h on S10. */
    case BUILT_IN_FETCH_AND_ADD_N:
    case BUILT_IN_FETCH_AND_ADD_1:
    case BUILT_IN_FETCH_AND_ADD_2:
    case BUILT_IN_FETCH_AND_ADD_4:
    case BUILT_IN_FETCH_AND_ADD_8:
    case BUILT_IN_FETCH_AND_ADD_16:
    case BUILT_IN_FETCH_AND_SUB_N:
    case BUILT_IN_FETCH_AND_SUB_1:
    case BUILT_IN_FETCH_AND_SUB_2:
    case BUILT_IN_FETCH_AND_SUB_4:
    case BUILT_IN_FETCH_AND_SUB_8:
    case BUILT_IN_FETCH_AND_SUB_16:
    case BUILT_IN_FETCH_AND_OR_N:
    case BUILT_IN_FETCH_AND_OR_1:
    case BUILT_IN_FETCH_AND_OR_2:
    case BUILT_IN_FETCH_AND_OR_4:
    case BUILT_IN_FETCH_AND_OR_8:
    case BUILT_IN_FETCH_AND_OR_16:
    case BUILT_IN_FETCH_AND_AND_N:
    case BUILT_IN_FETCH_AND_AND_1:
    case BUILT_IN_FETCH_AND_AND_2:
    case BUILT_IN_FETCH_AND_AND_4:
    case BUILT_IN_FETCH_AND_AND_8:
    case BUILT_IN_FETCH_AND_AND_16:
    case BUILT_IN_FETCH_AND_XOR_N:
    case BUILT_IN_FETCH_AND_XOR_1:
    case BUILT_IN_FETCH_AND_XOR_2:
    case BUILT_IN_FETCH_AND_XOR_4:
    case BUILT_IN_FETCH_AND_XOR_8:
    case BUILT_IN_FETCH_AND_XOR_16:
    case BUILT_IN_FETCH_AND_NAND_N:
    case BUILT_IN_FETCH_AND_NAND_1:
    case BUILT_IN_FETCH_AND_NAND_2:
    case BUILT_IN_FETCH_AND_NAND_4:
    case BUILT_IN_FETCH_AND_NAND_8:
    case BUILT_IN_FETCH_AND_NAND_16:
    case BUILT_IN_ADD_AND_FETCH_N:
    case BUILT_IN_ADD_AND_FETCH_1:
    case BUILT_IN_ADD_AND_FETCH_2:
    case BUILT_IN_ADD_AND_FETCH_4:
    case BUILT_IN_ADD_AND_FETCH_8:
    case BUILT_IN_ADD_AND_FETCH_16:
    case BUILT_IN_SUB_AND_FETCH_N:
    case BUILT_IN_SUB_AND_FETCH_1:
    case BUILT_IN_SUB_AND_FETCH_2:
    case BUILT_IN_SUB_AND_FETCH_4:
    case BUILT_IN_SUB_AND_FETCH_8:
    case BUILT_IN_SUB_AND_FETCH_16:
    case BUILT_IN_OR_AND_FETCH_N:
    case BUILT_IN_OR_AND_FETCH_1:
    case BUILT_IN_OR_AND_FETCH_2:
    case BUILT_IN_OR_AND_FETCH_4:
    case BUILT_IN_OR_AND_FETCH_8:
    case BUILT_IN_OR_AND_FETCH_16:
    case BUILT_IN_AND_AND_FETCH_N:
    case BUILT_IN_AND_AND_FETCH_1:
    case BUILT_IN_AND_AND_FETCH_2:
    case BUILT_IN_AND_AND_FETCH_4:
    case BUILT_IN_AND_AND_FETCH_8:
    case BUILT_IN_AND_AND_FETCH_16:
    case BUILT_IN_XOR_AND_FETCH_N:
    case BUILT_IN_XOR_AND_FETCH_1:
    case BUILT_IN_XOR_AND_FETCH_2:
    case BUILT_IN_XOR_AND_FETCH_4:
    case BUILT_IN_XOR_AND_FETCH_8:
    case BUILT_IN_XOR_AND_FETCH_16:
    case BUILT_IN_NAND_AND_FETCH_N:
    case BUILT_IN_NAND_AND_FETCH_1:
    case BUILT_IN_NAND_AND_FETCH_2:
    case BUILT_IN_NAND_AND_FETCH_4:
    case BUILT_IN_NAND_AND_FETCH_8:
    case BUILT_IN_NAND_AND_FETCH_16:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_N:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_16:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_N:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_16:
    case BUILT_IN_LOCK_TEST_AND_SET_N:
    case BUILT_IN_LOCK_TEST_AND_SET_1:
    case BUILT_IN_LOCK_TEST_AND_SET_2:
    case BUILT_IN_LOCK_TEST_AND_SET_4:
    case BUILT_IN_LOCK_TEST_AND_SET_8:
    case BUILT_IN_LOCK_TEST_AND_SET_16:
    case BUILT_IN_LOCK_RELEASE_N:
    case BUILT_IN_LOCK_RELEASE_1:
    case BUILT_IN_LOCK_RELEASE_2:
    case BUILT_IN_LOCK_RELEASE_4:
    case BUILT_IN_LOCK_RELEASE_8:
    case BUILT_IN_LOCK_RELEASE_16:
    {
      char * fname;

#ifdef __linux__
      tree fn;

      fn = build_decl (FUNCTION_DECL, get_identifier ("membar_enter"),
                       build_function_type (void_type_node, NULL_TREE));
      DECL_ARTIFICIAL (fn) = 1;
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      TREE_NOTHROW (fn) = 1;
      DECL_IN_SYSTEM_HEADER (fn) = 1;

      fn = build_function_call_expr (fn, NULL_TREE);
      dump_ir_call (fn, 0);
#endif

      fname = map_sync2solaris_fname (fcode);
      if (fname)
        {
          stmt = build_decl (FUNCTION_DECL, get_identifier (fname), 
                             build_function_type (TREE_TYPE (stmt), 
                                                  TYPE_ARG_TYPES (TREE_TYPE (fndecl)))); 
          DECL_ARTIFICIAL (stmt) = 1;
          DECL_EXTERNAL (stmt) = 1;
          DECL_IN_SYSTEM_HEADER (stmt) = 1;
          TREE_PUBLIC (stmt) = 1;
          TREE_NOTHROW (stmt) = 1;
          stmt = build_function_call_expr (stmt, arglist);
        }

      return dump_ir_call (stmt, need_return);
    }

    case BUILT_IN_LNI_START:
      push_lni_inline_context (stmt);
      break;

    case BUILT_IN_LNI_END:
      pop_lni_inline_context (stmt);
      break;
      
    case BUILT_IN_CONSTANT_P:
      {
	tree val;

	val = fold_builtin_constant_p (TREE_VALUE (arglist));
	/* Gimplification will pull the CALL_EXPR for the builtin out of
	   an if condition.  When not optimizing, we'll not CSE it back.
	   To avoid link error types of regressions, return false now.  */
	if (!val && !optimize)
	  val = integer_zero_node;
	
	if (val)
          ret = dump_ir_expr (val, MAP_FOR_VALUE);
        else
          ret = dump_ir_call (stmt, need_return);
        break;
      }
    case BUILT_IN_VA_ARG_PACK:
    case BUILT_IN_VA_ARG_PACK_LEN:
      /* All valid uses of __builtin_va_arg_pack () are removed during
         inlining.  */
      error ("%Kinvalid use of %<__builtin_va_arg_pack ()%>", stmt);
      break;
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      ret = dump_ir_builtin_int_roundingfn (stmt, need_return);
      break;
    default:
      ret = dump_ir_call (stmt, need_return);
      break;
    }
  
  return ret;
}

static void
init_indirect_goto_list (void)
{
  indirect_goto_list = NULL;
  label_list = NULL;
}

static IR_NODE *
make_labelref_list (void)
{
  TRIPLE *ret = NULL;
  struct ir_node_list *n;
  int cnt = 0;

  if (label_list == NULL)
    {
      /* labalref for last basic block */
      IR_NODE * l = build_ir_labelref (return_label_num, 0);
      l->triple.left->leaf.unknown_control_flow = IR_TRUE;
      TAPPEND (ret, (TRIPLE *)l);
      
      /* labelref for first basic block in a function */
      l = build_ir_labelref (return_label_num + 1, 0);
      l->triple.left->leaf.unknown_control_flow = IR_TRUE;
      TAPPEND (ret, (TRIPLE *)l);
      
      /* labelref for 2nd basic block in a function */
      /* workaround CG issue of CR 6544084 */
      l = build_ir_labelref (return_label_num + 2, 0);
      l->triple.left->leaf.unknown_control_flow = IR_TRUE;
      TAPPEND (ret, (TRIPLE *)l);

      /* need to mark indirect gotos with those two labelrefs to generate "jmpl"s */
      return (IR_NODE*)ret;
    }
  
  n = label_list;
  while (n != NULL)
    {
      IR_NODE * ir_label = build_ir_labelref (n->u.label, 0);
      ir_label->triple.left->leaf.unknown_control_flow = IR_TRUE;
      cnt ++;
      TAPPEND (ret, (TRIPLE *)ir_label);
      n = n->next;
    }
  
  return (IR_NODE*)ret;
}

static void
patch_indirect_gotos (void)
{
  struct ir_node_list *n;

  if (/* mark all &&labels as volatile.
         never know when/where those references will be used.*/
      (indirect_goto_list == NULL || cfun->nonlocal_goto_save_area)
      && label_list != NULL)
      /* forced labels:
         let's mark those labels as volatile to presereve them
         for possible nonlocal gotos from other functions */
    {
      n = label_list;
      while (n != NULL)
        {
          int labelno_to_find = n->u.label; 
          ir_BLOCK *bp;
          TRIPLE *first, *tp;
          TRIPLE *tp_tnext;
          for (bp = first_block; bp; bp=bp->next) 
            {
              first = (TRIPLE*) NULL;
              if (bp->last_triple)
                first = bp->last_triple->tnext;
              for (tp = first; tp; tp = (tp_tnext == first ? TNULL : tp_tnext)) 
                {
                  if (tp->tag == ISTRIPLE && tp->op == IR_LABELDEF
                      && tp->left->leaf.val.cnst.c.i == labelno_to_find)
                    tp->is_volatile = IR_TRUE;
                  tp_tnext = tp->tnext;
                }
            }
          n = n->next;
        }
    }

  n = indirect_goto_list;

  while (n != NULL)
    {
       struct ir_node_list *tmp = n;
       n->u.node->triple.right = (IR_NODE*) make_labelref_list ()->triple.tnext;
       n = n->next;
       free (tmp);
    }
  indirect_goto_list = NULL;

  n = label_list;
  while (n != NULL)
    {
      struct ir_node_list *tmp = n;
      n = n->next;
      free (tmp);
    }
  label_list = NULL;
}

static int
is_memref_constraint (const char * c)
{
  while (*c == '=' || *c == '+') c++;
  if (*c == 'm' || *c == 'o' || *c == 'v' || *c == 'p')
    return 1;
  return 0;
}

static void
check_asm_expr (tree stmt)
{
/*  tree _string = ASM_STRING (stmt);*/
  tree _outputs = ASM_OUTPUTS (stmt);
  tree _inputs = ASM_INPUTS (stmt);
/*  tree _clobbers = ASM_CLOBBERS (stmt);*/
  int ninputs = list_length (_inputs);
  int noutputs = list_length (_outputs);
  
  if (ninputs + noutputs > MAX_RECOG_OPERANDS)
    {
       error ("more than %d operands in %<asm%>", MAX_RECOG_OPERANDS);
       return;
    }
}
static int
is_pass_indirect_to_asm (tree type)
{
  if (TREE_CODE (type) == RECORD_TYPE 
      || TREE_CODE (type) == UNION_TYPE)
    return 1;
  
  if (!TYPE_SIZE (type) 
      || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return 1;

  return 0;
}
static void
process_asm_parameters (tree asm_args, int is_outputs, TRIPLE ** args_list)
{
  while (asm_args)
    {
      tree _purpose = TREE_VALUE (TREE_PURPOSE (asm_args));
      tree _value = TREE_VALUE (asm_args);
      
      TRIPLE * tp;
      IR_NODE * new_leaf;
      /* not used TYPE val_t = map_gnu_type_to_TYPE (TREE_TYPE (_value));*/
      TYPE ptr_to_val_t = map_gnu_type_to_TYPE (build_pointer_type (TREE_TYPE (_value)));
      IR_TYPE_NODE *ir_val_t = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (_value));
      IR_TYPE_NODE *ir_ptr_to_val_t = 
          map_gnu_type_to_IR_TYPE_NODE (build_pointer_type (TREE_TYPE (_value)));
      
      if (is_memref_constraint (TREE_STRING_POINTER (_purpose)))
          /* if we need to pass something via "m" constraint then
             pass it as addr via special "#" contraint */
        {
          IR_NODE * value = dump_ir_expr (_value, MAP_FOR_ADDR);
          IR_NODE * can_access_leaf = 0;
   
          if (TREE_CODE (_value) == INDIRECT_REF)
            {
              if (value->operand.tag != ISLEAF || value->leaf.class == ADDR_CONST_LEAF
                  || (value->leaf.class == VAR_LEAF 
                      && value->leaf.val.addr.seg->descr.class != AUTO_SEG
                      && value->leaf.val.addr.seg->descr.class != DREG_SEG))
                {
                  new_leaf = get_tmp_leaf (value->operand.type, ir_ptr_to_val_t);
                  build_ir_triple (IR_ASSIGN, new_leaf, value, value->operand.type, NULL);
                  value = new_leaf;
                }
              can_access_leaf = (IR_NODE*)func_heap_leaf;
            }
          else
            {
              if (value->operand.tag == ISLEAF)
                {
                  if (value->leaf.class == CONST_LEAF)
                    {
                      new_leaf = get_tmp_leaf (value->operand.type, ir_val_t);
                      build_ir_triple (IR_ASSIGN, new_leaf, value, value->operand.type, NULL);
                      value = new_leaf;
                    }
                  
                  if (value->leaf.class == VAR_LEAF)
                    {
                      /* cleanup: now it's done in adjust_leaf_overlaps()
                      value->leaf.no_reg = IR_TRUE;
                      if (value->leaf.elvarno == -1)
                        value->leaf.elvarno = naliases++; */

                      can_access_leaf = value;
                      value = build_ir_addr_const (value, 0, ptr_to_val_t, ir_ptr_to_val_t);
                    }
                  else /* ADDR_CONST_LEAF */
                    can_access_leaf = (IR_NODE*)value->leaf.addressed_leaf;
                      
                }
              else
                {
                  can_access_leaf = (IR_NODE*)func_heap_leaf;
                }
                  
              new_leaf = get_tmp_leaf (value->operand.type, ir_ptr_to_val_t);
              build_ir_triple (IR_ASSIGN, new_leaf, value, value->operand.type, NULL);
              value = new_leaf;
            }
          
          tp = (TRIPLE*) build_ir_triple (IR_ASM_ADDRESS, value, build_ir_string_const ("#"),
                                     value->leaf.type, NULL);
          tp->can_access = ir_copy_overlaps ((LEAF*)can_access_leaf);

          tp->param_mode = PM_INOUT;
      
          TAPPEND ( (*args_list), tp);

          asm_args = TREE_CHAIN (asm_args);

        }
      else if (is_pass_indirect_to_asm (TREE_TYPE (_value))) 
        { /* error if not a memref contraint and struct value */
          error ("impossible constraint in %<asm%>");
          return;
        }
      else
        {
          IR_NODE * purpose;
          IR_NODE * value = dump_ir_expr (_value, MAP_FOR_VALUE);

          if (value->operand.tag != ISLEAF || value->leaf.class == ADDR_CONST_LEAF
              || (value->leaf.class == VAR_LEAF 
                  && ((value->leaf.val.addr.seg->descr.class != AUTO_SEG
                       && value->leaf.val.addr.seg->descr.class != DREG_SEG)
                      /* allow only leaves that take full IR segment.
                         Leaves that use only part of the large parent segment 
                         make it difficult for codegen */
                      || value->leaf.val.addr.seg->len != value->leaf.type.size)))
            { /* need to copy value in a leaf */
              new_leaf = get_tmp_leaf (value->operand.type, ir_val_t);
              build_ir_triple (IR_ASSIGN, new_leaf, value, value->operand.type, NULL);
              value = new_leaf;
             
              if (is_outputs)
                {
                  if (TREE_LANG_FLAG_1 (asm_args))
                    abort ();
                  /* mark current asm_args with lang_1 to copy the value from
                     temp new_leaf back to original location in epilogue
                     after asm_stmt */
                  TREE_LANG_FLAG_1 (asm_args) = 1;

                  /* can't use TREE_PURPOSE (TREE_PURPOSE (asm_args))) field,
                     because it can be used by gcc when asm_stmt is called like
                     ("# foo on %[third] %[second] %[fourth] %[first]"
                      : [first] "=r" (kk)
                      : [second] "i" (j), etc... */
                }
            }
          
          purpose = build_ir_string_const (TREE_STRING_POINTER (_purpose));
          
          if (is_outputs)
            {
              tp = (TRIPLE*) build_ir_triple (IR_ASM_OUTPUT, value, purpose,
                                         value->leaf.type, NULL);
              tp->param_mode = PM_OUT;
            }
          else
            {
              tp = (TRIPLE*) build_ir_triple (IR_ASM_INPUT, value, purpose,
                                         value->leaf.type, NULL);
              tp->param_mode = PM_IN;
            }

          TAPPEND ( (*args_list), tp);

          asm_args = TREE_CHAIN (asm_args);
        }
    }
}

#define MAX_CASE_RANGE_SIZE 64

/* process statements (toplevel expressions in basic blocks) */

static void
dump_ir_stmt (tree stmt)
{
  if (errorcount != 0) 
    return;

  STRIP_NOPS (stmt);

  switch (TREE_CODE (stmt))
    {
    case CALL_EXPR:
      /* if (TREE_CODE (TREE_OPERAND (stmt, 0)) == ADDR_EXPR
          && (TREE_CODE (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0))
              == FUNCTION_DECL)
          && DECL_BUILT_IN (TREE_OPERAND (TREE_OPERAND (stmt, 0), 0))) */ 
      if (TREE_CODE (CALL_EXPR_FN (stmt)) == ADDR_EXPR
          && (TREE_CODE (TREE_OPERAND (CALL_EXPR_FN (stmt), 0))
              == FUNCTION_DECL)
          && DECL_BUILT_IN (TREE_OPERAND (CALL_EXPR_FN (stmt), 0)))
        {       
          if (DECL_BUILT_IN_CLASS (TREE_OPERAND (CALL_EXPR_FN (stmt), 0))
              == BUILT_IN_FRONTEND)
            abort ();
          else
            dump_ir_builtin_call (stmt, 0);
          break;
        }   

      dump_ir_call (stmt, 0/* procedure call*/);
      break;
    case MODIFY_EXPR:
      abort();
    case GIMPLE_MODIFY_STMT:
      dump_ir_modify (stmt);
      break;
    case RETURN_EXPR:
      {
        tree op0;
        
        op0 = TREE_OPERAND (stmt, 0); /* left */

        if (op0) 
          {
            if (TREE_CODE (op0) == GIMPLE_MODIFY_STMT)
              {
                tree left, right;
		left = GIMPLE_STMT_OPERAND (op0, 0); /* left */
		right = GIMPLE_STMT_OPERAND (op0, 1); /* right */
                
                /* case of 'return_expr (result_decl = var_decl)' */
                if (TREE_CODE (left) == RESULT_DECL
                    && !TREE_USED (left)
                    && TREE_CODE (right) == VAR_DECL)
                  {
                    IR_NODE * ret = dump_ir_expr (right, MAP_FOR_VALUE);
                    gcc_assert (ret->operand.tag == ISLEAF);
                    if (ret->leaf.type.tword == func_ret_leaf->leaf.type.tword
                        && ret->leaf.typep == func_ret_leaf->leaf.typep)
                      {
                        /* doesn't seem that libsunir allows deletion of leaves:
                           ir_proc_free_leaf (irProc, (LEAF*)func_ret_leaf); */
                        /* cleanup: now it's done in adjust_leaf_overlaps()
                           ret->leaf.elvarno = func_ret_leaf->leaf.elvarno; */
                        func_ret_leaf = ret;
                        TREE_USED (left) = 1;
                      }
                    else
                      dump_ir_modify (op0);
                  }
                else
                  dump_ir_modify (op0);
              }
            else if (TREE_CODE (op0) != RESULT_DECL) /* new gcc case */
              {
                tree t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (op0), 
                                DECL_RESULT (current_function_decl), op0);
                dump_ir_stmt (t);
              }
          }  

        build_ir_goto (return_label_num);
      }
      break;
    case COND_EXPR:
      if (TREE_TYPE (stmt) == void_type_node)
        {
          IR_NODE *ir_cond, *ir_then, *ir_else;
          tree _cond = COND_EXPR_COND (stmt);
          tree _then = COND_EXPR_THEN (stmt);
          tree _else = COND_EXPR_ELSE (stmt);

          if (TREE_CODE (_cond) == NOP_EXPR
              && TREE_CODE (TREE_TYPE (_cond)) == BOOLEAN_TYPE)
            {
              /* simplify if ((_Bool)cond) to if (cond) */
              ir_cond = dump_ir_expr (TREE_OPERAND (_cond, 0), MAP_FOR_VALUE);
            }
          else if (TREE_CODE (_cond) == NE_EXPR
                   && TREE_CODE (TREE_TYPE (_cond)) == BOOLEAN_TYPE
                   && integer_zerop (TREE_OPERAND (_cond, 1)))
            {
              /* simplify if (cond != 0) to if (cond) */
              ir_cond = dump_ir_expr (TREE_OPERAND (_cond, 0), MAP_FOR_VALUE);
            }
          else
            ir_cond = dump_ir_expr (_cond, MAP_FOR_VALUE);

          
          if (_then && TREE_CODE (_then) == GOTO_EXPR
              && _else && TREE_CODE (_else) == GOTO_EXPR)
            {
              int true_lab = get_ir_label (TREE_OPERAND (_then, 0)); 
              int false_lab = get_ir_label (TREE_OPERAND (_else, 0)); 

              ir_then = build_ir_labelref (true_lab, 1);
              
              ir_else = build_ir_labelref (false_lab, 0);
              {
                TRIPLE *t = (TRIPLE *) ir_then;
                TAPPEND(t, (TRIPLE *) ir_else);
                ir_then = (IR_NODE *)t;
              }
              build_ir_triple (IR_CBRANCH, ir_cond, ir_then, longtype, NULL);
            }
          else
            {
              debug_tree (_then);
              debug_tree (_else);
              abort ();
            }
        }
      else
        {
          /* result = (op0) ? (op1) : (op2); */
          dump_ir_expr (stmt, MAP_FOR_VALUE);
        }
      break;

    case LABEL_EXPR:
      {
        tree op0 = LABEL_EXPR_LABEL (stmt); /* label_decl */
        int labelno = get_ir_label (op0);
        IR_NODE * ret;

        TRIPLE *last_tp = current_block_last_triple ();
        if (last_tp && last_tp->op == IR_LABELDEF)
          /* make sure we don't have labeldef after labeldef
             iropt cfg builder doesn't like it */
          build_ir_goto (labelno);
        
        ret = build_ir_labeldef (labelno);
 
        /* check if this label had its address taken and therefore can
             never be deleted and is a jump target for computed gotos */
	/* fix for CR 6563127 */
	if (FORCED_LABEL (op0))
          add_to_label_list (labelno);
 
	if (TREE_LANG_FLAG_6 (stmt))
          ir_add_loopinfo (op0);

        if (DECL_NONLOCAL (op0)) /* don't need to mark FORCED_LABELs as volatile */
          {
            TYPE argtype = labelno_TYPE;
            /* need to add a nonlocal labelno to list of leaves of current routine */
            IR_NODE * label = build_ir_int_const (labelno, argtype, 0);
            label->leaf.unknown_control_flow = IR_TRUE; 

            /* mark as is_volatile to prevent renaming in iropt and cg,
               and to force iropt and cg to create fake edge front entry to this labeldef */
            ret->triple.is_volatile = IR_TRUE;

          }
      }
      break;
    case GOTO_EXPR:
      {
        tree op0;
        
        op0 = TREE_OPERAND (stmt, 0); /* label */
        
        if (TREE_CODE (op0) == LABEL_DECL)
          build_ir_goto (get_ir_label (op0));
        else
          {
            IR_NODE *ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
            struct ir_node_list *n = xmalloc (sizeof (struct ir_node_list));
            
            n->u.node = build_ir_triple (IR_INDIRGOTO, ir_op0, NULL, longtype, NULL);
            n->next = 0;
            
            if (indirect_goto_list == NULL)
              indirect_goto_list = indirect_goto_list_tail = n;
            else
              {
                indirect_goto_list_tail->next = n;
                indirect_goto_list_tail = n;
              }
          }
      }
      break;
    case SWITCH_EXPR:
      {
        IR_NODE  *ir_cond;
        tree _cond = SWITCH_COND (stmt);
        tree _body = SWITCH_BODY (stmt);
        tree _labels = SWITCH_LABELS (stmt);
        size_t i, n = TREE_VEC_LENGTH (_labels);
        TRIPLE * caselist = NULL, * ir_case, * ir_default_case = NULL;
       
        if (_body)
          { 
            debug_tree (_body);
            abort ();
          }
        
        ir_cond = dump_ir_expr (_cond, MAP_FOR_VALUE);

        for (i = 0; i < n; ++i)
          {
            tree elt = TREE_VEC_ELT (_labels, i);
            tree _case = CASE_LOW (elt);
            tree _case_high = CASE_HIGH (elt);
            tree _label = CASE_LABEL (elt);
            
            if (_case)
              {
                long long i_case = TREE_INT_CST_LOW (_case);
                long long i_case_high = _case_high 
                                        ? (long long)TREE_INT_CST_LOW (_case_high) 
                                        : i_case;
                long long i;
                
                /* handle case 0x20200002 ... 0x202fffff: */
                if (i_case_high - i_case >= MAX_CASE_RANGE_SIZE)
                  {
                    /* emit if (i_case <= ir_cond & ir_cond <= i_case_high) goto _label;
                       in front of the switch */
                    IR_NODE * cond_low, * cond_high, * caserange_cond, * ir_then, * ir_else;
                    
                    /* create a label for the beginning of the switch */
                    int end_of_caserange_label = gen_ir_label ();
                    
                    if (ir_cond->operand.tag != ISLEAF)
                      { /* make sure ir_cond is a leaf */
                        IR_NODE * new_leaf = 
                            get_tmp_leaf (ir_cond->operand.type,
                                          map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (_cond)));

                        build_ir_triple (IR_ASSIGN, new_leaf, ir_cond, 
                                         ir_cond->operand.type, NULL);
                        /* reassign ir_cond to:
                           . create just one new leaf for all caseranges
                           . switch stmt at the end will have it too */
                        ir_cond = new_leaf;
                      }
                    cond_low = 
                         build_ir_triple (IR_LE,
                                          build_ir_int_const (i_case, ir_cond->operand.type, 0), 
                                          ir_cond, inttype, NULL);
                     
                    cond_high = 
                         build_ir_triple (IR_LE, ir_cond,
                                          build_ir_int_const (i_case_high, 
                                                              ir_cond->operand.type, 0), 
                                          inttype, NULL);
                     

                    /* do the branch merging. Pros: shorter IR, less branches */
                    caserange_cond = build_ir_triple (IR_AND, cond_low, cond_high, inttype, NULL);
                    
                    ir_then = build_ir_labelref (get_ir_label (_label), 1);
                    
                    ir_else = build_ir_labelref (end_of_caserange_label, 0);
                    {
                      TRIPLE *t = (TRIPLE *) ir_then;
                      TAPPEND(t, (TRIPLE *) ir_else);
                      ir_then = (IR_NODE *)t;
                    }
                    build_ir_triple (IR_CBRANCH, caserange_cond, ir_then, longtype, NULL);

                    build_ir_labeldef (end_of_caserange_label);
                  }
                else for (i = i_case; i <= i_case_high && i >= i_case; i++)
                  {
                    ir_case = (TRIPLE*) build_ir_labelref_with_type (get_ir_label (_label), i,
                                                                     ir_cond->operand.type);
                    TAPPEND (caselist, ir_case);
                  }
              }
            else
              {
                ir_default_case = (TRIPLE*) build_ir_labelref_with_type (get_ir_label (_label), 0,
                                                                         ir_cond->operand.type);
              }

          }
        
        if (!ir_default_case)
          {
            abort (); /* GCC took care of that */
          }
        
        TAPPEND (caselist, ir_default_case);

        build_ir_triple (IR_SWITCH, ir_cond, (IR_NODE*)caselist, 
                         ir_cond->operand.type, NULL);
      }
      break;

    case ASM_EXPR:
      {
        tree _string = ASM_STRING (stmt);
        tree _outputs = ASM_OUTPUTS (stmt);
        tree _inputs = ASM_INPUTS (stmt);
        tree _clobbers = ASM_CLOBBERS (stmt);
        IR_NODE * ir_string, * t;
        TRIPLE * asm_args = NULL;
       
        check_asm_expr (stmt); /* emit erros/warnings if needed */

        ir_string = build_ir_string_const (TREE_STRING_POINTER (_string));

        if (ASM_INPUT_P (stmt))
        /* if old-style asm, pass it as is */
          {
            /* Sun backend is very conservative with ir_pass triple */
            t = build_ir_triple (IR_PASS, ir_string, NULL, ir_string->leaf.type, NULL);
            if (ASM_VOLATILE_P (stmt))
              t->triple.is_volatile = IR_TRUE; 
            /* pessimize optimizations the hard way ?
               default_opt_level = 1; */
            break;
          }
          
        process_asm_parameters (_outputs, 1, &asm_args);
        
        process_asm_parameters (_inputs, 0, &asm_args);

        if (_clobbers == NULL) 
            /* no clobbers, need to make one up to create a reference to 
               asm_stmt via right tree */
          {
              IR_NODE * value = build_ir_string_const ("");
              TRIPLE * tp = (TRIPLE*) build_ir_triple (IR_ASM_CLOBBER, value, NULL, 
                                                  value->leaf.type, NULL);

              TAPPEND (asm_args, (TRIPLE *) tp);
          }
        else
          while (_clobbers)
            {
              tree _value = TREE_VALUE (_clobbers);
              IR_NODE * value = TREE_CODE (_value) != STRING_CST || 
                                 TREE_PURPOSE (_clobbers) ? abort (), NULL : 
                                 build_ir_string_const (TREE_STRING_POINTER (_value));
              TRIPLE * tp = (TRIPLE*) build_ir_triple (IR_ASM_CLOBBER, value, NULL, 
                                                  value->leaf.type, NULL);

              int regn = decode_reg_name (TREE_STRING_POINTER (_value));

              if (regn == -4)
                tp->is_volatile = IR_TRUE; /* 'memory' clobber */
              else if (regn == -2)
                error ("unknown register name %qs in %<asm%>", TREE_STRING_POINTER (_value));

              if (TARGET_ARCH64)
                /* in V9 mark %g[2367] as used, so .register will be 
                   output in the side door file */
                if (regn == 2 || regn == 3 || regn == 6 || regn == 7)
                  df_set_regs_ever_live (regn, true);
                          
              TAPPEND (asm_args, (TRIPLE *) tp);

              _clobbers = TREE_CHAIN (_clobbers);
            }

        gcc_assert (asm_args != NULL);

        /* asm_args points to the last ir_asm_clobber */

        /* iropt will resequence asm args to start from ir_asm_input,
           otherwise dataflow will be corrupted */
        t = build_ir_triple (IR_ASM_STMT, ir_string, (IR_NODE*)asm_args, 
                             ir_string->leaf.type, NULL);
        
        if (ASM_VOLATILE_P (stmt))
          t->triple.is_volatile = IR_TRUE; 
        
        
        { /* patch IR_ASM_CLOBBERs right tree to point to IR_ASM_STMT for 
             iropt find_parent algorithm */
          TRIPLE *p = 0; 
          TFOR (p, (TRIPLE *) asm_args) 
            {
              if (p->op == IR_ASM_CLOBBER)
                p->right = t;
            }
        }

        _outputs = ASM_OUTPUTS (stmt);

        while (_outputs)
          {
            /* iterate 'asm_args' IR_NODEs simultaneously with '_outputs' trees.
               actually step ahead, because asm_args was pointing to last asm_clobber */
            asm_args = asm_args -> tnext; 

            /* list of 'asm_args' should start from ASM_OUTPUTs or ASM_ADDRESS */
            if (asm_args->op != IR_ASM_OUTPUT && asm_args->op != IR_ASM_ADDRESS)
              abort ();

            /* if '_outputs' is marking with lang_1, we would need to copy
               value from 'tmp_leaf' used in the current ASM_OUTPUT 'asm_args'
               back to original location in TREE_VALUE (_outputs) */
            if (TREE_LANG_FLAG_1 (_outputs))
              {
                tree _value = TREE_VALUE (_outputs);
                IR_NODE * value = dump_ir_expr (_value, MAP_FOR_ADDR);
                IR_NODE * tmp_leaf = asm_args->left; 
                TYPE argtype = map_gnu_type_to_TYPE (TREE_TYPE (_value));
                IR_TYPE_NODE* ir_argtype = map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (_value));
                
                if (value->operand.tag == ISLEAF && value->leaf.class == VAR_LEAF
                    && !value->leaf.is_indirect 
                    && TREE_CODE (_value) != ARRAY_REF && TREE_CODE (_value) != INDIRECT_REF)
                  build_ir_triple (IR_ASSIGN, value, tmp_leaf, argtype, 0);
                else
                  build_ir_triple (IR_ISTORE, value, tmp_leaf, argtype, ir_argtype);
                
                TREE_PURPOSE (TREE_PURPOSE (_outputs)) = 0;
              }
            
            _outputs = TREE_CHAIN (_outputs);
          }
      }
      break;
    
    case RESX_EXPR:
      dump_ir_resx_expr (stmt);
      break;              

    case NOP_EXPR:
      {
        tree op0;
        
        op0 = TREE_OPERAND (stmt, 0); 
        if (TREE_SIDE_EFFECTS (op0))
          {
            IR_NODE * ir_op0 = dump_ir_expr (op0, MAP_FOR_VALUE);
            /* generate FOREFF only if 'op0' is not leaf. 
               iropt will take care of the rest */
            if (ir_op0->operand.tag != ISLEAF)
              build_ir_triple (IR_FOREFF, ir_op0, NULL, ir_op0->operand.type, NULL);
          }
        break;
      }

    case OMP_PARALLEL:
      dump_omp_parallel (stmt);
      break;

    case OMP_FOR:
      dump_omp_for (stmt);
      break;

    case OMP_SECTIONS:
      dump_omp_sections (stmt);
      break;

    case OMP_SECTION:
      dump_omp_section (stmt);
      break;
            
    case OMP_SINGLE:
      dump_omp_single (stmt);
      break;

    case OMP_MASTER:
      dump_omp_master (stmt);
      break;

    case OMP_ORDERED:
      dump_omp_ordered (stmt);
      break;

    case OMP_CRITICAL:
      dump_omp_critical (stmt);
      break;

    case OMP_RETURN:
      dump_omp_return (stmt);
      break;

    case OMP_TASK:
      dump_omp_task (stmt);
      break;
            
    case OMP_TASKWAIT:
      dump_omp_taskwait (stmt);
      break;
            
    default:
      if (!quiet_flag)
        debug_tree (stmt);
      break;
    }
}
    
static void
dump_one_function_statement (tree stmt)
{
  bool have_loc;
  expanded_location for_check;
  current_eh_region = 0;
  number_of_pbranch = 0;

  if (EXPR_HAS_LOCATION (stmt))
    {   
      input_location = EXPR_LOCATION (stmt);
      have_loc = true;
      ir_location = expand_location (input_location);
      current_lni_handle = lni_source_transition (&ir_location);
      for_check = ir_location;
    }
  else if (TREE_CODE (stmt) == RETURN_EXPR)
    {  
      input_location = cfun->function_end_locus;
      have_loc = true;
      ir_location = expand_location (input_location);
      current_lni_handle = lni_source_transition (&ir_location);
      for_check = ir_location;
    }
  else
    {
      /* Continue to use previous statements line number */
      have_loc = false;
      for_check = ir_location;
    }
  
  if (ir_gen_scope_triple_p () && TREE_CODE (stmt) != LABEL_EXPR)
    {
      tree block;
          
      /* When gimplify pass lowers return stmts, it replaces similar 
         RETURN_EXPRS in different scopes with one at the end of the function.
         The replacement expr has no location and its TREE_BLOCK () is not 
         updated to reflect the new position. This transformation is done 
         regardless of optimization level. So check for this case.  */
      block = EXPR_HAS_LOCATION (stmt) ? TREE_BLOCK (stmt) : NULL;
      
      /* Check for change in lexical scope.  */
      if (IR_RELEVANT_SCOPE_P (block) 
          && BLOCK_SCOPE_ID (block) != BLOCK_SCOPE_ID (ir_top_scope ()))
        ir_change_scope (block);
    }

  if (have_loc)
    {
      /* We have already generated a new LNI context which
         will get attached to all triples generated from
         this statement. TODO: get rid of the IR_STMT once
         iropt/cg can completely handle it */
      build_ir_triple (IR_STMT, build_ir_string_const (ir_location.file), 
                       build_ir_int_const (ir_location.line, inttype, 0),
                       undeftype, NULL);
    }

  /* Generate IR */
  dump_ir_stmt (stmt);

  /* Temp. Verification nobody is messing with the line numbers */
  gcc_assert (for_check.line == ir_location.line
              && for_check.file == ir_location.file);
  
  if (TREE_CODE (stmt) == LABEL_EXPR && have_loc)
    {
      /* generate SunIR for LABEL_EXPR and add ir_stmt 
         pointing to source location.
         TODO: What is the equivalent hack for LNI, or
               is it even needed then?. */
      build_ir_triple (IR_STMT, build_ir_string_const (ir_location.file), 
                       build_ir_int_const (ir_location.line, inttype, 0),
                       undeftype, NULL);
    }
    
      
  if (flag_use_rtl_backend)
    abort();
      
  /* at the end of each statement */
  if (number_of_pbranch > 0)
    {
      int label;
      gcc_assert (number_of_pbranch == 1);
      /* emit a pbranch */
      label = gen_ir_label ();
      if (current_eh_region)
        ir_pbranch (get_action_number (current_eh_region), 
                    get_landing_label (current_eh_region), 
                    label);
      else
        ir_pbranch (0, label, label);
      /* create a new label */
      build_ir_labeldef (label);
      number_of_pbranch = 0;
    }
}

static void
dump_function_ir_statements (tree t)
{
  if (t == NULL)
    return;

  /* Fix 6563978. The OMP FOR pre init may
     contain arbitrary code. We do not gimplify this
     and it may contain bind statements */
  if (TREE_CODE(t) == BIND_EXPR)
    t = BIND_EXPR_BODY (t);

  if (TREE_CODE (t) == STATEMENT_LIST)
    {
      tree_stmt_iterator tsi;
      for (tsi = tsi_start (t); !tsi_end_p (tsi); tsi_next (&tsi))
        {
          /* Optimize condition pattern like:
               AUTO.0 = x <cond> y;
               if (AUTO.0)
             to the pattern like: 
               AUTO.0 = x <cond> y; // leave iropt for optimization 
               if (x <cond> y)
           */
          do  
            {
              tree cur, nxt, op0, op1;
              tree_stmt_iterator new_tsi = tsi;
              /* In general, c parser doesn't build such temporaries for 
                 comparison. */
              if (ir_language != CDOUBLEPLUS)
                break;
              tsi_next (&new_tsi);
              if (tsi_end_p (new_tsi))
                break;
              cur = tsi_stmt (tsi); 
              nxt = tsi_stmt (new_tsi);
              /* start with looong comparison. */
              if (TREE_CODE (cur) == GIMPLE_MODIFY_STMT 
                  && (op0 = GIMPLE_STMT_OPERAND (cur, 0)) /* get lvalue */ 
                  && (DECL_P (op0) && DECL_ARTIFICIAL (op0)) 
                  && (op1 = GIMPLE_STMT_OPERAND (cur, 1)) /* get rvalue */ 
                  && TREE_CODE_CLASS (TREE_CODE (op1)) == tcc_comparison 
                  && TREE_CODE (nxt) == COND_EXPR 
                  && op0 == TREE_OPERAND (nxt, 0)) 
                {
                  TREE_OPERAND (nxt, 0) = op1; 
                }
            } while (0); 
          dump_one_function_statement (tsi_stmt (tsi));
          if (flag_use_rtl_backend)
	    abort();
        }
    }
  else
    dump_one_function_statement (t);
}

/* Dump IR of FUNCTION_DECL FN to file "ir_file_name" */
void
dump_function_ir (tree fn)
{
  tree arg, vars, var;
  int func_num;
  tree result;
  TWORD ret_type; /* type of function result */
  int ret_is_structptr = 0;
  unsigned int ret_floatmap = 0;
  const char * func_name;
  TRIPLE * first_label;
  TYPE result_type;

  /* defining result data */
  static int frv_cnt=0;
  char result_name[128];
  IR_TYPE_NODE * result_ir_type;

  /* pch generation is on, but flag_use_rtl_backend may be zero
     because we want to store pre-processed trees in gcc4ss-ed gimple form
     and not in plain gimple */
  if (pch_file)
    return;

  /* For errors treat it as if IR gen was successful, as
     we will quit anyway. Not doing tis causes wired
     bailing out messages later. */
  if (errorcount || sorrycount)
    return;
  
  if (DECL_STATIC_CONSTRUCTOR (fn)
      && targetm.have_ctors_dtors)
    targetm.asm_out.constructor (XEXP (DECL_RTL (fn), 0),
                                 decl_init_priority_lookup (fn));
  if (DECL_STATIC_DESTRUCTOR (fn)
      && targetm.have_ctors_dtors)
    targetm.asm_out.destructor (XEXP (DECL_RTL (fn), 0),
                                decl_fini_priority_lookup (fn)); 

  func_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (fn));
  func_name = (* targetm.strip_name_encoding) (func_name);
  cfun_eh_filter = cfun_eh_exc_ptr = 0;
  
  /* If this function is `main' and -xpagesize is used, add extra 
     variables to the side door file */
  if (DECL_NAME (fn)
      && MAIN_NAME_P (DECL_NAME (fn))
      && DECL_FILE_SCOPE_P (fn))
    {
      if (pagesize_stack > 0 || pagesize_heap > 0) 
        {
          tree pagesize = build_decl (VAR_DECL, get_identifier ("__mpss_stack_pagesize"),
                                      unsigned_intDI_type_node);
          /* The variable was declared by the compiler.  */
          DECL_ARTIFICIAL (pagesize) = 1;
          /* And we don't want debug info for it.  */
          DECL_IGNORED_P (pagesize) = 1;
          /* Make the variable writable.  */
          TREE_READONLY (pagesize) = 0;
          DECL_EXTERNAL (pagesize) = 0; /* assemble_variable doesn't output external vars */
          TREE_PUBLIC (pagesize) = 1;
          TREE_USED (pagesize) = 1;
          DECL_CONTEXT (pagesize) = NULL_TREE;
          TREE_THIS_VOLATILE (pagesize) = 0;
          TREE_ADDRESSABLE (pagesize) = 0;
          DECL_INITIAL (pagesize) = build_int_cst (unsigned_intDI_type_node, pagesize_stack);
          assemble_variable (pagesize, 0, 0, 0);
         
          pagesize = build_decl (VAR_DECL, get_identifier ("__mpss_heap_pagesize"),
                                 unsigned_intDI_type_node);
          /* The variable was declared by the compiler.  */
          DECL_ARTIFICIAL (pagesize) = 1;
          /* And we don't want debug info for it.  */
          DECL_IGNORED_P (pagesize) = 1;
          /* Make the variable writable.  */
          TREE_READONLY (pagesize) = 0;
          DECL_EXTERNAL (pagesize) = 0; /* assemble_variable doesn't output external vars */
          TREE_PUBLIC (pagesize) = 1;
          TREE_USED (pagesize) = 1;
          DECL_CONTEXT (pagesize) = NULL_TREE;
          TREE_THIS_VOLATILE (pagesize) = 0;
          TREE_ADDRESSABLE (pagesize) = 0;
          DECL_INITIAL (pagesize) = build_int_cst (unsigned_intDI_type_node, pagesize_heap);
          assemble_variable (pagesize, 0, 0, 0);
          
          /* to ensure that we emit them only once */
          pagesize_stack = 0;
          pagesize_heap = 0;
        }
    }

  switch_to_section (function_section (fn)); /* from assemble_start_function */
  ir_location = expand_location (DECL_SOURCE_LOCATION (fn));
  
  /* no need to clear string_cst hash table here.
     GCC combines equal strings through out a compilation unit (file) */
  /* but need to generate separate labels for different functions
     in ipo mode */
  /* not any more? see cr 6433088
  if (globalize_flag && 0)
    empty_const_desc_htab (); */

  /* using extended IR to pass struct values out of funcs */
  if (TREE_CODE (TREE_TYPE (DECL_RESULT (fn))) == REFERENCE_TYPE
      && DECL_BY_REFERENCE (DECL_RESULT (fn)))
    {
      result = void_type_node;
      fval_type = TREE_TYPE (DECL_RESULT (fn));
    }
  else
    fval_type = result = TREE_TYPE (TREE_TYPE (fn));

  ret_type = map_gnu_type_to_tword (result);

  result_type = map_gnu_type_to_TYPE (result);

  ret_is_structptr = IS_RECORD_TYPE (result);

  /* workaround for missing .stret16 in libc */
  if (ret_is_structptr && result_type.align > 8)
    result_type.align = 8; 

  ret_floatmap = make_floatmap (result);
        
  return_label_num = func_num = gen_ir_label ();
  
  init_indirect_goto_list ();

  dump_ir_init (func_name, fn, func_num, result_type, 
                TREE_PUBLIC (fn) || globalize_flag, 
                ret_is_structptr, ir_location.file, ir_location.line);
  
  first_label = current_block_last_triple ();
  
  if (first_label->op != IR_LABELDEF)
    abort ();

  /* COMDAT functions should be declared as .global, not .weak */
  if (TREE_PUBLIC (fn) && DECL_COMDAT (fn) && flag_comdat)
    targetm.asm_out.globalize_label (asm_out_file, func_name);
  else if (DECL_WEAK (fn))
    ASM_WEAKEN_LABEL (asm_out_file, func_name);
      
  /* COMDAT functions will use Sun comdat section names. */
  if (DECL_SECTION_NAME (fn) == NULL_TREE && DECL_ONE_ONLY (fn)
      && !(TREE_PUBLIC (fn) && DECL_COMDAT (fn) && flag_comdat))
    {
      (*targetm.asm_out.unique_section) (fn, 0);
      ir_proc_set_section (irProc, TREE_STRING_POINTER (DECL_SECTION_NAME (fn)));
    }
 
  if (ir_language == FORTRAN && !flag_mark_ir_as_cpp)
    ir_proc_set_lang (irProc, FORTRAN90);
  else if (ir_language == CDOUBLEPLUS && flag_mark_ir_as_cpp)
    ir_proc_set_lang (irProc, CDOUBLEPLUS);
  else
    ir_proc_set_lang (irProc, C);

  cfun->uses_pbranch = 0;

  /* IR optimizer does not have enough EH information to 
     inline C++ functions with eh regions. */
  if ( flag_xinline)
    {
    if (in_xinline_string (IDENTIFIER_POINTER (DECL_NAME (fn)), tree_ir_noinline_list)
         || (flag_tree_ir_no_inline && !in_xinline_string (IDENTIFIER_POINTER (DECL_NAME (fn)), tree_ir_inline_list)))
      ir_proc_set_inline_control (irProc, DO_NOT_INLINE_CALL);
    else if (in_xinline_string(IDENTIFIER_POINTER (DECL_NAME (fn)), tree_ir_inline_list))
      ir_proc_set_inline_control (irProc, HIGH_PRIORITY_INLINE_CALL);
    }

  if (lookup_attribute ("noinline", DECL_ATTRIBUTES (fn))
      || strcmp (func_name, "__init_task_common") == 0) /* Fix 6588138 */
    ir_proc_set_inline_control (irProc, DO_NOT_INLINE_CALL);
  else if (DECL_DECLARED_INLINE_P (fn)
           || (ir_language == CDOUBLEPLUS
               && DECL_LANG_SPECIFIC (fn)
               && DECL_TEMPLATE_INSTANTIATION (fn)
               && TREE_PUBLIC (fn)
               && TREE_STATIC (fn)
               && DECL_WEAK (fn)))
    ir_proc_set_inline_control (irProc, HIGH_PRIORITY_INLINE_CALL);

  /* need to tell cg to preserve this function */
  if (DECL_PRESERVE_P (fn) || lookup_attribute ("used", DECL_ATTRIBUTES (fn))) 
    {
      targetm.asm_out.mark_decl_preserved (func_name); /* empty on sparc */
      if (!TREE_PUBLIC (fn))
        {
          /* just add a reference in .s file to prevent deletion in cg */
          fputs ("\t.local ", asm_out_file);
          assemble_name (asm_out_file, func_name);
          putc ('\n', asm_out_file);
        }
      /* else: external functions are not going to be deleted anyway, therefore
         do nothing */

      /* "used" also means no-inline */
      ir_proc_set_inline_control (irProc, DO_NOT_INLINE_CALL);
    }
  
  if (lookup_attribute ("unused", DECL_ATTRIBUTES (fn))) 
    /* "unused" also means no-inline */
    ir_proc_set_inline_control (irProc, DO_NOT_INLINE_CALL);

  if (TREE_PUBLIC (fn))
    {
      enum symbol_visibility vis = DECL_VISIBILITY (fn);
      
      if (vis != VISIBILITY_DEFAULT)
        targetm.asm_out.visibility (fn, vis);
    }

  cur_auto_off = 0;
  if (TARGET_ARCH64)
    cur_param_off = 0;
  else
    cur_param_off = 68;

  result_ir_type = map_gnu_type_to_IR_TYPE_NODE (result);
  
  /* using extended IR to pass struct values out of funcs */
  if (TREE_CODE (TREE_TYPE (DECL_RESULT (fn))) == REFERENCE_TYPE
      && DECL_BY_REFERENCE (DECL_RESULT (fn)))
    {
      IR_NODE * p;
      TYPE hidden_ref_type = map_gnu_type_to_TYPE (fval_type);
      IR_TYPE_NODE * hidden_ref_ir_type = map_gnu_type_to_IR_TYPE_NODE (fval_type);

      snprintf (result_name, sizeof (result_name),
                "func_hidden_return_value.%d.%d", func_num, frv_cnt++);

      if (TARGET_ARCH64)
        {
          cur_auto_off -= hidden_ref_type.size;
          /* round down for negative offsets. auto_off is always negative */
          cur_auto_off = cur_auto_off & ~(hidden_ref_type.align - 1LL);

          func_ret_leaf = build_ir_auto_var (result_name, cur_auto_off, 
                                             hidden_ref_type.size, hidden_ref_type.align, 0, 
                                             hidden_ref_type, hidden_ref_ir_type);

          SCOPE_TEMP_AS_PRIVATE(func_ret_leaf);
        }
      else
        {
          if (gxx_call_abi)
            cur_param_off = 64; /* workaround to make sure that args#>6 are 
                                   in correct stack location */
          else
            cur_param_off = 68;
          func_ret_leaf = build_ir_parm_var (result_name, cur_param_off, 
                                             hidden_ref_type, hidden_ref_ir_type);
          SCOPE_TEMP_AS_PRIVATE(func_ret_leaf);
          cur_param_off += 4;
        }
      /* cleanup: now it's done in adjust_leaf_overlaps()
      func_ret_leaf->leaf.elvarno = naliases++; */
      
      /* cleanup: now it's done in build_ir_leaf() 
      set_leaf_pointerno (func_ret_leaf, hidden_ref_type.tword); */

      p = build_ir_triple (IR_FPARAM, func_ret_leaf, (IR_NODE*)func_entry_triple, 
                           func_ret_leaf->operand.type, NULL);
      
      if (func_entry_triple->right != NULL) 
        {
          TRIPLE *tp = (TRIPLE *) func_entry_triple->right;
          TAPPEND (tp, (TRIPLE *) p);
          func_entry_triple->right = (IR_NODE *) tp;
        }
      
      func_entry_triple->right = p;
      if (gxx_call_abi)
        func_entry_triple->is_stcall = IR_TRUE;
      gcc_assert (func_entry_triple->type.tword == PCC_TVOID);
      func_entry_triple->type.size = get_type_size (TREE_TYPE (fval_type));
      func_entry_triple->type.align = TYPE_ALIGN (TREE_TYPE (fval_type)) / 8;
      
      ir_proc_set_type (irProc, map_gnu_type_to_TYPE (TREE_TYPE (fval_type)));
    }

  arg = DECL_ARGUMENTS (fn);
  while (arg)
    {
      tree argtype = TREE_TYPE (arg);

      IR_NODE * ir_op = dump_ir_expr (arg, MAP_FOR_ADDR);

      IR_NODE * p = build_ir_triple (IR_FPARAM, ir_op, (IR_NODE*)func_entry_triple, 
                                     ir_op->operand.type, NULL);
     
      /* mark all pointer func arguments as is_restrict if -xrestrict flag specified 
         or if IR comes from fortran front-end */
      if ((flag_xrestrict 
           || ir_language == FORTRAN)
          && (TREE_CODE (TREE_TYPE (arg)) == POINTER_TYPE
              || TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE)
          && ir_op->operand.tag == ISLEAF)
        ir_op->leaf.is_restrict = IR_TRUE;

      if ((ir_language == C || ir_language == CDOUBLEPLUS) 
        /* fortran never pass aggregate value as parameters 
           so leave alone the bit allocates_new_memory. */ 
          && TREE_CODE (argtype) == REFERENCE_TYPE)
        argtype = TREE_TYPE (argtype);

      if (tu_pass_by_reference (NULL, TYPE_MODE (argtype), argtype, false))
        p->triple.allocates_new_memory = IR_TRUE;

      p->triple.param_float_map = make_floatmap (TREE_TYPE (arg));
      if (func_entry_triple->right != NULL) 
        {
          TRIPLE *tp = (TRIPLE *) func_entry_triple->right;
          TAPPEND (tp, (TRIPLE *) p);
          func_entry_triple->right = (IR_NODE *) tp;
        }
      func_entry_triple->right = p;

      arg = TREE_CHAIN (arg);
    }

  /* varargs */
  if (TYPE_ARG_TYPES (TREE_TYPE (fn)) != 0
      && TREE_VALUE (tree_last (TYPE_ARG_TYPES (TREE_TYPE (fn)))) != void_type_node)
    {
      IR_NODE * ir_op, * p;
      
      __builtin_va_alist_node = build_decl (PARM_DECL, get_identifier ("__builtin_va_alist"),
                                            va_list_type_node);
      DECL_ARG_TYPE (__builtin_va_alist_node) = va_list_type_node;
      DECL_ARTIFICIAL (__builtin_va_alist_node) = 1;
      TREE_READONLY (__builtin_va_alist_node) = 0;
      DECL_EXTERNAL (__builtin_va_alist_node) = 0;
      TREE_STATIC (__builtin_va_alist_node) = 0; /* TODO ? */
      TREE_PUBLIC (__builtin_va_alist_node) = 0;
      TREE_USED (__builtin_va_alist_node) = 1;
      DECL_CONTEXT (__builtin_va_alist_node) = current_function_decl;
      TREE_THIS_VOLATILE (__builtin_va_alist_node) = 1;
      TREE_ADDRESSABLE (__builtin_va_alist_node) = 1;
      
      ir_op = dump_ir_expr (__builtin_va_alist_node, MAP_FOR_ADDR); 

      ir_op->leaf.is_va_alist = IR_TRUE;
      /* cleanup: now it's done in adjust_leaf_overlaps()
      ir_op->leaf.elvarno = naliases++; */

      p = build_ir_triple (IR_FPARAM, ir_op, (IR_NODE*)func_entry_triple, 
                           ir_op->operand.type, NULL);
      
      if (func_entry_triple->right != NULL) 
        {
          TRIPLE *tp = (TRIPLE *) func_entry_triple->right;
          TAPPEND (tp, (TRIPLE *) p);
          func_entry_triple->right = (IR_NODE *) tp;
        }
      func_entry_triple->right = p;
    }
 
  if (ret_type != PCC_TVOID) 
    {
      /* acomp would create "#ftn.%d", func_num); name here */
      snprintf (result_name, sizeof (result_name),
                "#tmp.retval.%d.%d", func_num, frv_cnt++);

      cur_auto_off -= result_type.size;
      /* round down for negative offsets. auto_off is always negative */
      cur_auto_off = cur_auto_off & ~(result_type.align - 1LL);

      func_ret_leaf = build_ir_auto_var (result_name, cur_auto_off, 
                                         result_type.size, result_type.align, 0, 
                                         result_type, result_ir_type);
      SCOPE_TEMP_AS_PRIVATE (func_ret_leaf);

      /* cleanup: now it's done in adjust_leaf_overlaps()
      func_ret_leaf->leaf.elvarno = naliases++; */
      /* cleanup: now it's done in build_ir_leaf() 
      set_leaf_pointerno (func_ret_leaf, result_type.tword); */
    }
   
  
  /* When gimple is lowered, the variables are no longer available in the
     bind_exprs, so display them separately.  */
  if (cfun->unexpanded_var_list)
    {
      for (vars = cfun->unexpanded_var_list; vars; vars = TREE_CHAIN (vars))
	{
	  var = TREE_VALUE (vars);
	  /* print_generic_decl (stderr, var, 0); */
         
          /* pre-dump IR for used var_decls */
          if (TREE_CODE (var) == VAR_DECL) 
            /* some tests need to see unused vars in gdb output && TREE_USED (var)) */
            {
              if (!(flag_tm_mode 
                    && DECL_NAME (var) 
                    && strcmp (IDENTIFIER_POINTER (DECL_NAME (var)), 
                               "$TEMP.sgcc.tm.atomic") == 0))
                dump_ir_expr (var, MAP_FOR_VALUE);
            }
          
	 /*  cleanup: now it's done in dump_ir_stmt () 
          if (TREE_CODE (var) == LABEL_DECL && FORCED_LABEL (var))
            {
              add_to_label_list (get_ir_label (var));
            } */
	}
    }

  /* If the function receives a non-local goto, then store the
     bits we need to restore the frame pointer */
  if (cfun->nonlocal_goto_save_area)
    {
      tree t = build5 (ARRAY_REF, ptr_type_node, cfun->nonlocal_goto_save_area,
                      integer_one_node, NULL_TREE, NULL_TREE, NULL_TREE);
      IR_NODE *save_area = dump_ir_expr (t, MAP_FOR_ADDR);
      IR_NODE *reg_sp, *n;
      
      reg_sp = get_ir_stack_pointer_reg ();
      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) reg_sp;
          LAPPEND (pinfo->u.s.omp_private, lp);
        }

      n = build_ir_triple (IR_ISTORE, save_area, reg_sp, save_area->triple.ldst_type, 
                      save_area->triple.ldst_ir_type);
      n->triple.is_volatile = IR_TRUE;
    }

  
  if (cfun->static_chain_decl) /* cfun is a nested function */
    {
      /* need to initialize chain_var with contents of %g2 for v8 and %g5 for v9*/
      IR_NODE *chain_var = dump_ir_expr (cfun->static_chain_decl, MAP_FOR_VALUE);
      IR_NODE *chain_reg, *assign;
      
      if (chain_var->operand.tag != ISLEAF)
        abort ();

      chain_reg = get_ir_chain_reg (chain_var->operand.type, chain_var->leaf.typep);
      assign = build_ir_triple (IR_ASSIGN, chain_var, chain_reg, chain_var->operand.type, NULL);
/*      assign->triple.is_volatile = IR_TRUE;*/
    }
  
  /* generate all eh region filter numbers, to be used in PBRANCH triple */
  generate_cfun_eh_filters ();

  /* Push function scope.  */
  if (ir_gen_scope_triple_p ())
    ir_push_scope (DECL_INITIAL (fn));
  
  gcc_assert (TREE_CODE (DECL_SAVED_TREE (fn)) == STATEMENT_LIST);
  /* clear plist_* pointers always here. */
  plist_head = plist_prev = plist_cur = NULL;
  dump_function_ir_statements (DECL_SAVED_TREE (fn));
  if (flag_use_rtl_backend)
    {
      abort();
      dump_ir_fini (fn, 0/* finish and do not write*/);
      return;
    }
  
  /* make sure to finish main block with goto to the exit block */
  if (current_block_last_triple ()->op != IR_GOTO)
    build_ir_goto (func_num);
  
  /* Pop all open lexical scopes.  */
  if (ir_gen_scope_triple_p ()) 
    ir_pop_lexical_scopes ();

  if (cfun->uses_pbranch && !cfun->uses_eh_lsda)
    {
      /* very unusual case */
      cfun->uses_eh_lsda = 1;
    }
  
  /* generate all landing pads at the end */
  generate_cfun_landing_pads (func_num);
  
  if (cfun->builtin_return_addr_called)
    {
      /* mark first label as volatile to tell cg that this function needs stack
         in terms of save/restore */
      first_label->is_volatile = IR_TRUE; 
    }
  
  dump_ir_exit (result_type, result_ir_type, func_num, ret_floatmap);

  patch_indirect_gotos ();

  adjust_leaf_overlaps ();

  if (errorcount == 0) 
      dump_ir_fini (fn, 1/* do write*/);
  
  /* and make sure that the current sections is a ".text" section after
     function definition */
  switch_to_section (function_section (fn)); 
  sunir_output_init_fini(asm_out_file, fn);

  return;
}

void
global_ir_init (void)
{
  ir_backend_init ();

  init_ir_type_node_htab_once ();
  
  if (globalize_flag && !ir_global_prefix)
    {
      ir_global_prefix = generate_prefix ();
    }

  if (TARGET_ARCH64)
    {
      longtype.size = 8;
      longtype.align = 8;
    }
    
  offsettype = (!TARGET_ARCH64 ? inttype : longtype);
  /* pointers are unsigned, so?...
   {
    TYPE uinttype = {PCC_UNSIGNED,   4, 4};
    TYPE ulongtype = {PCC_ULONG,  8, 8};
    offsettype = (!TARGET_ARCH64 ? uinttype : ulongtype);
  }*/

  current_function_number = 0;

  if (strcmp (lang_hooks.name, "GNU C++") == 0)
    ir_language = CDOUBLEPLUS;
  else if (strcmp (lang_hooks.name, "GNU F95") == 0)
    ir_language = FORTRAN;
  else
    ir_language = C;
}

void
global_ir_fini (void)
{
  sparc_output_scratch_registers (asm_out_file);
  ir_backend_fini ();
  if (flag_xinline)
    {
      free (tree_ir_noinline_list);
      free (tree_ir_inline_list);
    }
}

static void
ir_add_loopinfo (tree stmt)
{
  LOOPINFO * lp;
  lp = build_ir_loop ();

  if (TREE_CODE (stmt) == LABEL_DECL)
    {
      save_and_switch_line_information (stmt);
      lp->looplabel = (LEAF *) build_ir_int_const (get_ir_label (stmt),
                                                   inttype, 0);
      lp->loopfilename = (LEAF *) build_ir_string_const (ir_location.file);
      lp->looplineno = (LEAF *) build_ir_int_const (ir_location.line,
                                                    inttype, 0);
      restore_line_information (stmt);
    }
  else if (TREE_CODE (stmt) == GOTO_EXPR)
    {
      lp->looplabel = (LEAF *) build_ir_int_const (get_ir_label (TREE_OPERAND (stmt, 0)), 
                                                   inttype, 0);
      lp->loopfilename = (LEAF *) build_ir_string_const (get_filename (stmt));
      lp->looplineno = (LEAF *) build_ir_int_const (get_lineno (stmt), inttype, 0);
    }
  else
    abort ();

  lp->loop_type = LT_FOR;
  /*lp->loopindex = (LEAF*) ;*/
  lp->unroll_factor = 0; /* pragma specified unroll factor */
  lp->ncpus = (LEAF *) build_ir_int_const (0, inttype, 0); /* number of desired CPUs for autopar */
  lp->schedtype = IR_ST_NONE;
  lp->chunksize = NULL; lp->numchunks = NULL;
  lp->local = NULL; lp->shared = NULL;  lp->byvalue = NULL; lp->storeback = NULL;
  lp->reduction = NULL; lp->dep_dist = 0;
  lp->storeback_all = IR_FALSE; lp->pipe_loop = IR_FALSE; lp->no_mem_dep = IR_FALSE;
}

/* Support for IR lexical scopes generation.  */
/* At all? compilation level IR scope triples are generated
   to help dbggen produce lexical scope information in the dwarf 
   debug information. 
   
   TODO Lexical scope information for debugging should not be
    generated at higher optimization levels ?

   1. iropt deletes scope triples at higher optimization levels ?
   2. dbggen and dwarf currently do not support fragmented scopes, and 
   at higher optimization levels code transformations like pipelining,
   loop unrolling will result in fragmented scopes ?

   There are two major parts to the algorithm,

   1. Generation of IR (BEGIN_SCOPE/END_SCOPE) triples during IR generation. 
   This is implemented in tree-ir.c

   2. Modification of the scope triples for tracking dbggen scopes.
   This is done by replacing the scope ids, with dbggen scope ids.
   It is important to note that the scope triples were not designed for
   storing dbggen scope ids, so this is technically an abuse of scope
   triples because we know that ir scope ids are similar to dbggen scope
   ids. This is implemented in tree-ir.c and tree-ir-debug.c. 

   Generation of IR scope triples
   ------------------------------
    The objective of the scopes triples generation is to insert a BEGIN_SCOPE 
  triple at the begining of a scope, i.e before the first statement of the 
  scope and to insert an END_SCOPE triple at the end of the scope, i.e after
  the last statement in the scope. Scope triples are not generated for the 
  function level scope. The algorithm tracks lexical scopes using a stack,
  where a scope is pushed onto the stack when it begins and popped when it ends.
    When a statement is encountered that belongs to a scope that has never been
  pushed onto the stack, it is assumed that the statement is the first statement
  in that scope. If the previous scope, which is top of the stack is not an 
  ancestor of the new scope, then that scope is popped of the stack because it
  is assumed to end after the previous statement. The algorithm initializes 
  by pushing the function level scope onto the stack, and since it is an 
  ancestor for all lexical scopes it is never popped until the very end.
  At the end of the processing the function, all the scopes in the stack are
  popped off including the function level scope.
    Sometimes the situations arise where this
  strategy does not work and the algorithm resorts to a failure mode.
  For example, it is possible to encounter a statement 
  belonging to a scope that has been pushed and popped in the past. This 
  is probably an instance of fragmented scopes that the algorithm cannot handle.

   Storing dbggen scope ids in scope triples
   -----------------------------------------
   If scope triples generation was successful during IR generation for 
   a function, dbggen scopes and corresponding dbggen scopes ids are
   created when generating debug information for the lexical scopes
   in the function. The scope triples created for each lexical
   scope are updated with the corresponding dbggen scope id.
   Later on cg is expected to bind the scopes to function offsets
   in the dwarf information.
*/

bool
ir_gen_scope_triple_p (void)
{
  return (dbg_gen_generate_scopes () && !scope_gen_failed);
}


bool
ir_ancestor_scope_p (tree ancestor, tree scope)
{
  tree temp = BLOCK_SUPERCONTEXT (scope);
  bool result = false;
  
  while (TREE_CODE (temp) == BLOCK)
    {
      if (ancestor == temp)
        {
          result = true;
          break;
        }
      temp = BLOCK_SUPERCONTEXT (temp);
    }
  return result;
}

static void
ir_disable_scope_gen (void)
{
  ir_pop_lexical_scopes ();
  ir_remove_scope_triples ();
  scope_gen_failed = true;
}

void 
ir_scope_gen_init (void)
{
  scope_gen_failed = false;
  VARRAY_TREE_INIT (scope_stack, 10, "ir_scope_stack");
  ir_scopes_table_init ();
}

void 
ir_scope_gen_fini (void)
{
  ir_scopes_table_fini ();
  /* VARRAY_DATA_TREE_PTR type uses gc, so dont free.  */
  scope_stack = NULL;
}

static bool
ir_scope_stack_empty_p (void)
{
  return (VARRAY_ACTIVE_SIZE (scope_stack) == 0);
}

static bool
ir_branch_triple_p (IR_OP op)
{
  bool result;
  
  switch (op)
    {
    case IR_CBRANCH:
    case IR_GOTO:
    case IR_INDIRGOTO:
    case IR_PBRANCH:
    case IR_SWITCH:
      result = true;
      break;
    default:
      result = false;
    }

  return result;
}

static tree 
ir_top_scope (void)
{
  if (ir_scope_stack_empty_p ())
    internal_error ("Cannot return top of empty scope stack");

  return VARRAY_TOP_TREE (scope_stack);
}

/* Pop scope and generate corresponding END_SCOPE triple. Left operand 
   of the triple will be filled while emitting debug information.  */
static void 
ir_pop_scope (void)
{
  tree top_scope = ir_top_scope ();
  TRIPLE *triple = NULL;
  TRIPLE *prev_triple = NULL;
  
  if (ir_lexical_scope_p (top_scope))
    {
      /* END_SCOPE triple created only for used scopes.  */
      if (TREE_USED (top_scope)) {
        triple = (TRIPLE *) build_ir_triple  (IR_END_SCOPE, 
                                              build_ir_int_const (BLOCK_SCOPE_ID (ir_top_scope ()), inttype, 0),
                                              NULL, undeftype, NULL);
        /* branch triples should not precede an END_SCOPE triple, since iropt
         will delete the END_SCOPE triple as unreachable code.  */
        prev_triple = triple->tprev;
        if (prev_triple && ir_branch_triple_p (prev_triple->op))
          {
            while (ir_branch_triple_p (prev_triple->op))
              {
                prev_triple = prev_triple->tprev;
              }
            remove_ir_triple (triple);
            triple->tnext = triple->tprev = triple;
            TAPPEND (prev_triple, triple);
          }
      }
      ir_end_scope_triple (BLOCK_SCOPE_ID(top_scope), (TRIPLE *)triple);
    }
  VARRAY_POP (scope_stack);
}

static void 
ir_pop_lexical_scopes (void)
{ 
  tree block;
  
  while (!ir_scope_stack_empty_p ())
    {
      block  = ir_top_scope ();
      if (BLOCK_SUPERCONTEXT (block) == NULL
          || TREE_CODE (BLOCK_SUPERCONTEXT (block)) == FUNCTION_DECL)
        break;
      ir_pop_scope ();
    }
}

static void 
ir_push_scope (tree block)
{
  tree parent;
  IR_NODE *triple = NULL;
  
  if (TREE_CODE (block) != BLOCK)
    internal_error ("Expecting BLOCK tree node");

  parent = BLOCK_SUPERCONTEXT (block);

  if (!ir_lexical_scope_p (block)) 
    {
      /* Function scope.  */
      if (parent == NULL
          || TREE_CODE (parent) == FUNCTION_DECL) 
        block_scope_id = 1;
      
      BLOCK_SCOPE_ID (block) = block_scope_id++;
      VARRAY_PUSH_TREE (scope_stack, block);
      /* BEGIN_SCOPE/END_SCOPE triples are not created for function scope.  */
      ir_begin_scope_triple (NULL);
      return;
    }
  
  /* If parent scope has not been pushed, push parent.  */
  if (BLOCK_SCOPE_ID (parent) == 0)
    ir_push_scope (parent);
  
  /* Push new scope and generate BEGIN_SCOPE triple */
  BLOCK_SCOPE_ID (block) = block_scope_id++;
  VARRAY_PUSH_TREE (scope_stack, block);
  /* BEGIN_SCOPE and END_SCOPE triples are created only for blocks with variable
     declarations. Operands of triples get filled while emitting debug information.
     Unused scopes still get an entry with NULL BEGIN_SCOPE/END_SCOPE triples
     in the dbggen scopes table, this allows the use of BLOCK_SCOPE_ID (block) 
     in indexing the scopes table.  */
  if (TREE_USED (block)) {
    int parent_id = ir_lexical_scope_p (parent) ? BLOCK_SCOPE_ID (parent) : 1;
    triple = build_ir_triple (IR_BEGIN_SCOPE, 
                              build_ir_int_const (BLOCK_SCOPE_ID (block), inttype, 0),
                              build_ir_int_const (parent_id, inttype, 0),
                              undeftype, NULL);
    if (flag_tm_mode && BLOCK_TM_ATOMIC (block))
      triple->triple.tm_atomic_block = 1;
  }
  
  ir_begin_scope_triple ((TRIPLE *) triple);
}

static bool
ir_lexical_scope_p (tree block)
{
  tree parent = BLOCK_SUPERCONTEXT (block);
  
  if (parent == NULL)
    return false;
  
  if (TREE_CODE (parent) == FUNCTION_DECL)
    return false;
  
  if (ir_language == CDOUBLEPLUS)
    {
      parent = BLOCK_SUPERCONTEXT (parent);
      if (parent && (TREE_CODE (parent) == FUNCTION_DECL))
        return false;
    }
  
  return true;
}


/* Return the most immediate pushed ancestor. There is always such an 
 ancestor since the function scope is always pushed.  */
static tree
ir_immediate_pushed_ancestor (tree scope)
{
  tree temp = BLOCK_SUPERCONTEXT (scope);
  
  while (BLOCK_SCOPE_ID (temp) == 0)
    temp = BLOCK_SUPERCONTEXT (temp);
  
  return temp;
}

/* Handle lexical scope changs during IR generation. Due to block reordering
 only the following kinds of scope changes can be encountered.  
 1. Begin descendant scope of current scope.
 2. Begin sibling/cousin of current scope => End of current scope.
 3. Resume ancestor scope => End of current scope.
 4. All other cases => disable scope generation.
 */
static void
ir_change_scope (tree new_scope)
{
  tree current_scope = ir_top_scope ();
  
  if (BLOCK_SCOPE_ID (new_scope) == 0)
    { /* Begin scope cases.  */
      if (!ir_ancestor_scope_p (current_scope, new_scope))
        { 
          tree c_ancestor, n_ancestor;
          
          n_ancestor = ir_immediate_pushed_ancestor (new_scope);
          c_ancestor = ir_immediate_pushed_ancestor (current_scope);
          if (c_ancestor != n_ancestor)
            {
              if (!ir_ancestor_scope_p (n_ancestor, c_ancestor))
                ir_disable_scope_gen ();
            }
          while ((BLOCK_SCOPE_ID (ir_top_scope ()) != 1)
                 && (n_ancestor != ir_top_scope ()))
            ir_pop_scope ();
        }
      ir_push_scope (new_scope);
    }
  else 
    { /* Resume scope cases.  */
      if (ir_ancestor_scope_p (new_scope, current_scope))
        { /* Resume ancestor, end scopes up till ancestor.  */
          while ((BLOCK_SCOPE_ID (ir_top_scope()) != 1)
                 && new_scope != ir_top_scope ())
            {
              ir_pop_scope ();
            }
        }
      else 
        {
          ir_disable_scope_gen ();
        }
    }
}

/* Create table to record scopes triples created during IR generation. 
   Entries in the table will be referenced using scope_id field of the 
   corresponding tree_block. Scope ids are assigned starting from 1 for 
   the function scope, so push a dummy NULL entry to make mapping easy.
*/
static void 
ir_scopes_table_init (void)
{
  VARRAY_GENERIC_PTR_INIT (ir_scopes_table, 10, "ir_scopes_table");
  VARRAY_PUSH_GENERIC_PTR (ir_scopes_table, NULL);
}

/* Free up scopes table and its entries. The first entry is NULL.  */
static void
ir_scopes_table_fini (void)
{
  int i;
  
  for (i = 1; i < (int)VARRAY_ACTIVE_SIZE (ir_scopes_table); i++)
    free (VARRAY_GENERIC_PTR (ir_scopes_table, i));
  
  ir_scopes_table = NULL;
}

static void
ir_begin_scope_triple (TRIPLE *triple)
{
  ir_scope_t *ir_scope = (ir_scope_t *) xmalloc (sizeof (ir_scope_t));
  
  ir_scope->begin_scope = triple;
  ir_scope->end_scope = NULL;
  VARRAY_PUSH_GENERIC_PTR (ir_scopes_table, ir_scope);
}

static void
ir_end_scope_triple (unsigned scope_id, TRIPLE *triple)
{
  ir_scope_t *ir_scope = VARRAY_GENERIC_PTR (ir_scopes_table, scope_id);
  
  ir_scope->end_scope = triple;
}

static tree
ir_used_ancestor_scope (tree scope)
{
  tree ancestor = BLOCK_SUPERCONTEXT (scope);
  
  while (!TREE_USED (ancestor))
    ancestor = BLOCK_SUPERCONTEXT (ancestor);
  
  return ancestor;
}


void 
ir_update_scope_triples (tree scope, DbgScopeID dbg_cur_scope_id,
                         DbgScopeID dbg_parent_scope_id)
{
  unsigned ir_parent_scope_id;
  ir_scope_t *ir_scope;
  
  if (BLOCK_SCOPE_ID (scope) && ir_lexical_scope_p (scope))
    {
      /* In SUNIR the procedure scope id is always 1.  */
      /*
        if (BLOCK_SCOPE_ID (ir_used_ancestor_scope (scope)) == 1) 
        ir_parent_scope_id = 1;
        else 
        ir_parent_scope_id = dbg_parent_scope_id;
      */

      if (ir_lexical_scope_p (ir_used_ancestor_scope (scope)))
        ir_parent_scope_id = dbg_parent_scope_id;
      else
        ir_parent_scope_id = 1;

      ir_scope = VARRAY_GENERIC_PTR (ir_scopes_table, BLOCK_SCOPE_ID (scope));
      if ((ir_scope->begin_scope == NULL) || (ir_scope->end_scope == NULL))
        internal_error ("BEGIN_SCOPE and (or) END_SCOPE triples missing");
      
      ir_scope->begin_scope->left = build_ir_int_const (dbg_cur_scope_id, inttype, 0);
      ir_scope->begin_scope->right = build_ir_int_const (ir_parent_scope_id, inttype, 0);
      ir_scope->end_scope->left = build_ir_int_const (dbg_cur_scope_id, inttype, 0); 
    }
}

static void
ir_remove_scope_triples (void)
{
  int i;
  ir_scope_t *ir_scope;
  
  for (i = 1; i < (int)VARRAY_ACTIVE_SIZE (ir_scopes_table); i++)
    {
      ir_scope = VARRAY_GENERIC_PTR (ir_scopes_table, i);
      if (ir_scope->begin_scope != NULL)
        remove_ir_triple (ir_scope->begin_scope);
      
      if (ir_scope->end_scope != NULL)
        remove_ir_triple (ir_scope->end_scope);
    }
}


/* OpenMP related section. */

#define END_PRAGMA_LINE(xloc) ((xloc).line + 1)

static IR_NODE *
dump_num_threads_clause (tree clauses)
{
  tree c, val;
  IR_NODE *leafp, *var;
  
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_NUM_THREADS)
	{
            val = OMP_CLAUSE_NUM_THREADS_EXPR (c);
            val = fold_convert (unsigned_type_node, val);
            var = dump_ir_expr (val, MAP_FOR_VALUE);
            if (var->operand.tag == ISTRIPLE)
              {
                leafp = get_tmp_leaf (((TRIPLE *)var)->type,
                                      ((TRIPLE *)var)->typep);
                build_ir_triple (IR_ASSIGN, leafp, var, 
                                 leafp->operand.type, NULL);
                var = leafp;
              }
            return var;
        }
    }

  return (IR_NODE *) NULL;
}

static unsigned
dump_collapse_clause (tree clauses)
{
  tree c, val;
  unsigned result;
  
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_COLLAPSE)
	{
            val = OMP_CLAUSE_COLLAPSE_EXPR (c);
            result = TREE_INT_CST_LOW (val);
            return result;
        }
    }

  return 0;
}

static IR_NODE *
dump_if_cond_clause (tree clauses)
{
  tree c, cond;
  IR_NODE *leafp, *var;

  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_IF)
        {      
          cond = OMP_CLAUSE_IF_EXPR (c);
          cond = gimple_boolify (cond);
          var = dump_ir_expr (cond, MAP_FOR_VALUE);
          if (var->operand.tag == ISTRIPLE)
            {
              leafp = get_tmp_leaf (((TRIPLE *)var)->type,
                                    ((TRIPLE *)var)->typep);
              build_ir_triple (IR_ASSIGN, leafp, var, 
                               leafp->operand.type, NULL);
              var = leafp;
            }
          return var;
        }
    }

  return (IR_NODE *) NULL;
}

static dscope_t
find_default_clause (tree clauses) 
{
  tree c;
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_DEFAULT)
        {
          switch (OMP_CLAUSE_DEFAULT_KIND (c))
            {
            case OMP_CLAUSE_DEFAULT_SHARED:
              return SCOPE_SHARED;
            case OMP_CLAUSE_DEFAULT_PRIVATE:
              return SCOPE_PRIVATE;        
            case OMP_CLAUSE_DEFAULT_UNSPECIFIED:
            case OMP_CLAUSE_DEFAULT_NONE:
            case OMP_CLAUSE_DEFAULT_AUTO:
              return SCOPE_NONE;
            }
        }
    }

  return SCOPE_NONE;
}

static void
dump_schedule_chunksize (tree clauses,
                         PRAGMAINFO *pinfo)
{
  tree c, chunk, val;
  IR_NODE *var, *leafp;
  
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      if (OMP_CLAUSE_CODE (c) == OMP_CLAUSE_SCHEDULE)
        {
          switch (OMP_CLAUSE_SCHEDULE_KIND (c))
            {
            case OMP_CLAUSE_SCHEDULE_STATIC:
              pinfo->u.s.schedtype = ST_OMP_STATIC;
              break;
            case OMP_CLAUSE_SCHEDULE_DYNAMIC:
              pinfo->u.s.schedtype = ST_OMP_DYNAMIC;
              break;
            case OMP_CLAUSE_SCHEDULE_GUIDED:
              pinfo->u.s.schedtype = ST_OMP_GUIDED;
              break;
            case OMP_CLAUSE_SCHEDULE_RUNTIME:
              pinfo->u.s.schedtype = ST_OMP_RUNTIME;
              break;
            case OMP_CLAUSE_SCHEDULE_AUTO:
              /* IR GEN NOT READY */
              pinfo->u.s.schedtype = ST_OMP_AUTO;
              break;
            default:
              gcc_unreachable ();
            }
          chunk = OMP_CLAUSE_SCHEDULE_CHUNK_EXPR (c);
          if (chunk != NULL_TREE)
            {
                val = fold_convert (long_integer_type_node, chunk);
                var = dump_ir_expr (val, MAP_FOR_VALUE);
                if (var->operand.tag == ISTRIPLE)
                  {
                      leafp = get_tmp_leaf (((TRIPLE *)var)->type,
                                            ((TRIPLE *)var)->typep);
                      build_ir_triple (IR_ASSIGN, leafp, var, 
                                       leafp->operand.type, NULL);
                      var = leafp;
                  }
                pinfo->u.s.chunksize = (LEAF *) var;
            }
        }
    }
}

static IR_NODE *
create_ir_scope (tree decl,
                 LIST **append_to,
                 PRAGMAINFO *pinfo,
                 int need_misc_list)
{
  IR_NODE *var;
  LIST *lp;
  tree orig;
  
  if (TREE_CODE (decl) != VAR_DECL
      && TREE_CODE (decl) != PARM_DECL
      && TREE_CODE (decl) != RESULT_DECL)
    return NULL;

  orig = decl;
  if (DECL_HAS_VALUE_EXPR_P (decl))
    decl = DECL_VALUE_EXPR (decl);
  
  if (TREE_CODE (decl) == PARM_DECL)
    var = dump_ir_expr (decl, MAP_FOR_ADDR);
  else
    var = dump_ir_expr (decl, MAP_FOR_VALUE);
  if (var->operand.tag == ISTRIPLE)
    {
      IR_NODE *lf = get_tmp_leaf (var->triple.type,
                                  var->triple.typep);
      const char *name = get_ir_name (orig);
      if (name)
        lf->leaf.pass1_id = build_ir_proc_string (name);
      (void) build_ir_triple (IR_ASSIGN, lf, var,
                              var->triple.type, NULL);
      var = lf;
    }

  /* For VLA types during gimplification the array
     pointer leaf is already added to the scope
     information, so we may invariantly generate
     it twice when whe generate scope for the main
     VLA leaf. Avoid this. */
  LFOR (lp, *append_to)
    {
      if (lp->datap == (void *) var)
        return var;
    }
  
  if (need_misc_list)
    {
      /* We need to emit misc list information for
         VLA types in C/C++, fortran variables
         that are privatized by reference. Params
         that are passed by reference due to size restriction.
         First discover if any of these conditions apply here.

         variable sized reference. During gimplification
         this variable should have been substituted by an
         indirect reference. The omp clauses are updated with
         the temps created for this reference. Only generate
         the misc list information and discard the variable.
         Example:
         
         int A[n];
         #pragma omp parallel private (A)
         .. A[..] ..
         
         We will see A in the clause, however gimplification
         would have substituted A with couple of auto variables
         
         size_t AUTO.2 = n;
         int *AUTO.1 = __builtin_alloca (AUTO.2 * 4)
         #pragma omp parallel private (A, AUTO.1) shared (AUTO.2)
         .. *AUTO.1 ..
         
         There will be no further use of A in the code. A is
         preserved in the clause to re-associate the AUTO.1 and
         AUTO.2 information back and generate correct code.
         
         Since A is unused, we will not generate scoping information
         for it, we will ony generate it for AUTO.1 and AUTO.2, which
         will happen automatically as gimplification adds it to the
         clauses. We will use A only to generate the misc list information
         to let iropt know the dimensions of the array. */

      int need_param_misc_list = 0;
      tree decl_size;
      tree gnu_var_type = TREE_TYPE (decl);

      if (TREE_CODE (gnu_var_type) == POINTER_TYPE
          || TREE_CODE (gnu_var_type) == REFERENCE_TYPE)
        gnu_var_type = TREE_TYPE (gnu_var_type);
      
      if (VOID_TYPE_P (gnu_var_type))
        decl_size = size_zero_node;
      else
        decl_size = TYPE_SIZE_UNIT (gnu_var_type);

      if (TREE_CODE (decl) == PARM_DECL
          || TREE_CODE (decl) == RESULT_DECL)
        {
          /* Special handling for C/C++ for ABI handling issue.
             The parameter is a struct or array type, however
             since we will pass this by reference, we will need
             to explain to iropt what we did here. */
          if (TREE_CODE (TREE_TYPE (decl)) == REAL_TYPE)
            gnu_var_type = DECL_ARG_TYPE (decl);

          /* Fix cr6676176. example A.33 in spec3.0. for c/c++ 
	     program, when the type of a formal parameter is array,
	     we don't need to generate misc_list. */ 
          if ((ir_language == C || ir_language == CDOUBLEPLUS)
                && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE
		&& TREE_CODE (gnu_var_type) == ARRAY_TYPE)
	      need_param_misc_list = 0;
	  else
	     if (tu_pass_by_reference (NULL, TYPE_MODE (gnu_var_type),
                                 gnu_var_type, false))
              need_param_misc_list = 1;
        }
      
      if (decl_size != NULL_TREE
          && ((!TREE_CONSTANT (decl_size) && TREE_CODE (decl) != PARM_DECL 
               && TREE_CODE (decl) != RESULT_DECL)
              || need_param_misc_list
              || (ir_language == FORTRAN
                  && lang_hooks.decls.omp_privatize_by_reference (decl) 
                  /* workaround for 6657230. */ 
                  && !(TREE_CODE (decl) == VAR_DECL 
                      && TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE 
                      && !AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))))) 
        {
          LIST *tmp, *listhead;
          IR_NODE *size_lf, *node;
          
          node = dump_ir_expr (decl_size, MAP_FOR_VALUE);
          if (node->operand.tag == ISTRIPLE)
            {
              size_lf = get_tmp_leaf (((TRIPLE *)node)->type,
                                      node->triple.typep);
              (void) build_ir_triple (IR_ASSIGN, size_lf, node,
                                          ((TRIPLE *)node)->type, NULL);
            }
          else
            size_lf = node;

          listhead = pinfo->misc_list;

          node = get_tmp_leaf (map_gnu_type_to_TYPE (gnu_var_type),
                               map_gnu_type_to_IR_TYPE_NODE (gnu_var_type));
          tmp = build_ir_proc_list ();
          tmp->datap = (LDATA *) node;
          tmp->next = listhead;
          listhead = tmp;

          tmp = build_ir_proc_list ();
          tmp->datap = (LDATA *) size_lf;
          tmp->next = listhead;
          listhead = tmp;

          tmp = build_ir_proc_list ();
          tmp->datap = (LDATA *) var;
          tmp->next = listhead;
          listhead = tmp;

          tmp = build_ir_proc_list ();
          tmp->datap = (LDATA *) build_ir_int_const ((CONSZ)1, inttype, 0);
          tmp->next = listhead;
          listhead = tmp;

          pinfo->misc_list = listhead;
        }
    }

  lp = build_ir_proc_list ();
  lp->datap = (LDATA *) var;
  LAPPEND (*append_to, lp);
  return var;
}

/* C++ specific handling for passing default ctor, dtor,
   copy ctor and copy assign info to iropt. For default ctors
   which take a single argument of this, we will simply pass the
   address of the constructor. If we have any constructors
   that take multiple arguments, then the remaining arguments
   have to be assigned constants. For this we have to create
   a special function body that takes a single argument of
   the this pointer, which inturn will call the constructor
   with the remaining arguments setup correctly. Similarly
   for array constructors we have to generate a special
   function body */

static void
cxx_misc_list (IR_NODE *var,
               tree fn,
               PRAGMAINFO *pinfo,
               int code)
{
  LIST *tmp, *listhead;
  IR_NODE *fsym;
  
  if (!fn || !var)
    return;

  listhead = pinfo->misc_list;
  
  /* Addr of function */
  fsym = dump_ir_expr (build_fold_addr_expr (fn), MAP_FOR_ADDR);
  gcc_assert (fsym->operand.tag == ISLEAF);

  tmp = build_ir_proc_list ();
  tmp->datap = (LDATA *) fsym;
  tmp->next = listhead;
  listhead = tmp;
  
  /* The variable */
  tmp = build_ir_proc_list ();
  tmp->datap = (LDATA *) var;
  tmp->next = listhead;
  listhead = tmp;

  /* Misc list code */
  tmp = build_ir_proc_list ();
  tmp->datap = (LDATA *) build_ir_int_const ((CONSZ)code, inttype, 0);
  tmp->next = listhead;
  listhead = tmp;

  pinfo->misc_list = listhead;
}

static void
fill_scope_info (pragmaEntry_t ptype,
                 tree clauses,
                 PRAGMAINFO *pinfo)
{
  tree c, decl;
  LIST *lp, *red_lp;
  reduct_op op;
  IR_NODE *var;
  
  for (c = clauses; c; c = OMP_CLAUSE_CHAIN (c))
    {
      switch (OMP_CLAUSE_CODE (c))
	{
        case OMP_CLAUSE_PRIVATE:
          decl = OMP_CLAUSE_DECL (c);
          if (ir_language == FORTRAN
              && ptype == OMP_E_PARALLEL
              && lang_hooks.decls.omp_privatize_by_reference (decl))
            {
              /* Make it firstprivate */
              var = create_ir_scope (decl, &pinfo->u.s.firstprivate, pinfo, 1);
            }
          else
            {
              var = create_ir_scope (decl, &pinfo->u.s.omp_private, pinfo, 1);
            }
          if (ir_language == CDOUBLEPLUS && CP_OMP_CLAUSE_INFO (c))
            {
              /* Default constructor and destructor */
              cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 0),
                             pinfo, 101);
              cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 1),
                             pinfo, 104);
            }
          break;

	case OMP_CLAUSE_SHARED:
          gcc_assert (ptype == OMP_E_PARALLEL
                      || ptype == OMP_E_PARDO
                      || ptype ==  OMP_E_PARSECTIONS
                      || ptype == OMP_E_TASK);
          decl = OMP_CLAUSE_DECL (c);
          if (lang_hooks.decls.omp_disregard_value_expr (decl, true))
            {
              /* Handled in hacky way in omp_lowering. Mainly
                 effects fortran. If the var is shared, it will
                 still be lowered as private. */
              (void) create_ir_scope (decl, &pinfo->u.s.omp_private, pinfo, 1);
            }
          else 
            (void) create_ir_scope (decl, &pinfo->u.s.shared, pinfo, 0);
          break;
          
	case OMP_CLAUSE_LASTPRIVATE:
          gcc_assert (ptype != OMP_E_SINGLE);
          decl = OMP_CLAUSE_DECL (c);
          var = create_ir_scope (decl, &pinfo->u.s.lastprivate, pinfo, 1);
          if (ir_language == CDOUBLEPLUS && CP_OMP_CLAUSE_INFO (c))
            {
              /* Default constructor and destructor as well as copy assignment. */
              cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 0),
                             pinfo, 101);
	      if (!OMP_CLAUSE_LASTPRIVATE_FIRSTPRIVATE (c))
                cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 1),
                               pinfo, 104);
              cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 2),
                             pinfo, 103);
            }
          break;

	case OMP_CLAUSE_FIRSTPRIVATE:
          decl = OMP_CLAUSE_DECL (c);
          if (ir_language == FORTRAN
            && DECL_HAS_VALUE_EXPR_P (decl))
           {
             tree value, t;
             value = DECL_VALUE_EXPR (decl); 
             t = build2 (GIMPLE_MODIFY_STMT, TREE_TYPE (value), decl, value); 
             dump_ir_stmt (t); 
           }
          var = create_ir_scope (decl, &pinfo->u.s.firstprivate, pinfo, 1);
          if (ir_language == CDOUBLEPLUS && CP_OMP_CLAUSE_INFO (c))
	    {
              /* copy constructor */
              cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 0),
                             pinfo, 102);
	      /* default destructor */
	      cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 1),
			     pinfo, 104);
	    }
          break;
            
	case OMP_CLAUSE_REDUCTION:
          gcc_assert (ptype != OMP_E_SINGLE && ptype != OMP_E_TASK);
          decl = OMP_CLAUSE_DECL (c);
          red_lp = NULL;
          (void) create_ir_scope (decl, &red_lp, pinfo, 1);
          red_lp->next = NULL;
          switch (OMP_CLAUSE_REDUCTION_CODE (c))
            {
            case PLUS_EXPR:
              op = REDU_PLUS;
              break;
            case MINUS_EXPR:
              op = REDU_MINUS;
              break;
            case BIT_IOR_EXPR:
              op = REDU_IOR;
              break;
            case BIT_XOR_EXPR:
              op = REDU_IEOR;
              break;
            case BIT_AND_EXPR:
              op = REDU_IAND;
              break;
            case TRUTH_OR_EXPR:
            case TRUTH_ORIF_EXPR:
              op = REDU_OR;
              break;
            case EQ_EXPR:
              op = REDU_EQV;
              break;
            case NE_EXPR:
              op = REDU_NEQV;
              break;
            case MULT_EXPR:
              op = REDU_TIMES;
              break;
            case TRUTH_AND_EXPR:
            case TRUTH_ANDIF_EXPR:
              op = REDU_AND;
              break;
            case MAX_EXPR:
              op = REDU_MAX;
              break;
            case MIN_EXPR:
              op = REDU_MIN;
              break;
            case TRUTH_XOR_EXPR:
            default:
              gcc_unreachable ();
            }
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) build_ir_int_const ((CONSZ)op, inttype, 0);
          lp->next = red_lp;
          red_lp = lp;
          if (pinfo->u.s.reduction == NULL)
            pinfo->u.s.reduction = red_lp;
          else
            {
              lp = pinfo->u.s.reduction;
              while (lp->next)
                  lp = lp->next;
              lp->next = red_lp;
            }
          break;

	case OMP_CLAUSE_COPYPRIVATE:
          gcc_assert (ptype == OMP_E_SINGLE);
          decl = OMP_CLAUSE_DECL (c);
          var = create_ir_scope (decl, &pinfo->u.s.copyprivate, pinfo, 1);
          if (ir_language == CDOUBLEPLUS && CP_OMP_CLAUSE_INFO (c))
            /* copy assign */
            cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 2),
                           pinfo, 103);
          break;

	case OMP_CLAUSE_COPYIN:
          gcc_assert (ptype == OMP_E_PARALLEL
                      || ptype == OMP_E_PARDO
                      || ptype ==  OMP_E_PARSECTIONS);
          decl = OMP_CLAUSE_DECL (c);
          var = create_ir_scope (decl, &pinfo->u.s.copyin, pinfo, 1);
          if (ir_language == CDOUBLEPLUS && CP_OMP_CLAUSE_INFO (c))
            /* copy assign */
            cxx_misc_list (var, TREE_VEC_ELT (CP_OMP_CLAUSE_INFO (c), 2),
                           pinfo, 103);
          break;
          
        case OMP_CLAUSE_AUTO:
          gcc_assert (ptype == OMP_E_PARALLEL
                      || ptype == OMP_E_PARDO
                      || ptype ==  OMP_E_PARSECTIONS);
          decl = OMP_CLAUSE_DECL (c);
          (void) create_ir_scope (decl, &pinfo->u.s.autoscope, pinfo, 1);
          break;
          
        case OMP_CLAUSE_IF:
        case OMP_CLAUSE_DEFAULT:
	case OMP_CLAUSE_NUM_THREADS:
	case OMP_CLAUSE_SCHEDULE:
	case OMP_CLAUSE_NOWAIT:
        case OMP_CLAUSE_ORDERED:
        case OMP_CLAUSE_UNTIED:
        case OMP_CLAUSE_COLLAPSE:
          /* Dont care */
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  if (pinfo == NULL)
    return;

  if (plist_head == NULL)
    plist_head = (plist_cur = xmalloc (sizeof (struct pinfo_list)));
  else
    plist_cur = xmalloc (sizeof (struct pinfo_list));
  plist_cur->data = pinfo;
  plist_cur->next = NULL;
  if (plist_prev)
    plist_prev->next = plist_cur;
  else
    plist_prev = plist_cur;
}

static void
push_omp_context (PRAGMAINFO *pinfo,
                  tree stmt) 
{
  omp_ir_context_t *c;

  c = (omp_ir_context_t *) xcalloc (1, sizeof (struct omp_ir_context_t));
  c->pinfo = pinfo;
  c->stmt = stmt;
  c->exception_label = gen_ir_label ();
  c->lab_used = false;
  c->r_list = NULL;
  c->prev_ctx = cur_omp_context;
  cur_omp_context = c;
}

static void
pop_omp_context (void)
{
  omp_ir_context_t *c;
  gcc_assert (cur_omp_context != NULL);
  c = cur_omp_context;
  cur_omp_context = c->prev_ctx;
  free (c);
}

static void
generate_exception_label (omp_ir_context_t *c)
{
  /* In order to handle exceptions, we will generate the dummy exception
     label. To maintain structured blocks, we would have forced any exception
     arcs going out of the omp region to terminate here. */
  if (c->lab_used)
    {
      region_list *p = c->r_list;
      build_ir_goto (c->exception_label);
      gcc_assert (c->exception_label != 0);
      while ( p != NULL) 
	{
	  generate_special_landing_pads (c->exception_label, p->region_number);
	  p = p->next;
	}
 
      build_ir_labeldef (c->exception_label);
    }
}

/* Generate

   PRAGMAINFO for parallel region
   
   IR_pragma  OMP_PARALLEL_BEGIN
       parallel body
   IR_pragma OMP_PARALLEL_END

   par region may contain the following clauses
   make sure we fill them out
     - num_threads
     - if
     - private
     - shared
     - firstprivate
     - lastprivate
     - default
     - reduction
     - copyin
*/

static void
dump_omp_parallel (tree stmt) 
{
  tree clauses;
  LEAF *nthreads, *if_cond;
  PRAGMAINFO *pinfo;

  clauses = OMP_PARALLEL_CLAUSES (stmt);

  /* OMP_PARALLEL region clauses can generate some side effects
     due to if clause whose condition may need to be normalized
     and num_threads, which may also need to be handled before
     the pragma triple is generated. Make sure we handle these
     before we create the first pragma triple. */
  
  nthreads = (LEAF *) dump_num_threads_clause (clauses);
  if_cond = (LEAF *) dump_if_cond_clause (clauses);

  pinfo = build_ir_pragmainfo ();
  push_omp_context (pinfo, stmt);
  pinfo->type = OMP_E_PARALLEL;
  pinfo->num_threads = nthreads;
  pinfo->begin_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);

  gcc_assert (find_omp_clause (clauses, OMP_CLAUSE_NOWAIT) == NULL_TREE);
  gcc_assert (find_omp_clause (clauses, OMP_CLAUSE_ORDERED) == NULL_TREE);
  
  pinfo->u.s.default_scope = find_default_clause (clauses);
  pinfo->u.s.if_clause = if_cond;

  fill_scope_info (OMP_E_PARALLEL, clauses, pinfo);

  /* If there is inner nested parallel for/sections to
     combine. Dont generate the pragma triple here, allow the
     inner region to generate the correct pragma  triple. This
     is important as we do not want to generate triples before
     we generate some of the for/sections side effect */

  if (!OMP_PARALLEL_COMBINED (stmt))
    {
      IR_NODE *pragma_typ, *pragma_tp, *begin_lno;
      pragma_typ = build_ir_int_const ((CONSZ) OMP_PARALLEL_BEGIN,
                                       inttype, 0);
      begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                      inttype, 0);
      pragma_tp = build_ir_triple (IR_PRAGMA,
                                   pragma_typ,
                                   begin_lno,
                                   undeftype,
                                   NULL);
      pinfo->begin_triple = (TRIPLE *) pragma_tp;
    }
}


static void
dump_omp_parallel_end (tree stmt)
{
  PRAGMAINFO *pinfo;
  IR_NODE *pragma_typ, *pragma_tp, *lineno;

  pinfo = cur_omp_context->pinfo;  
  if (OMP_PARALLEL_COMBINED (stmt))
    {
      switch (pinfo->type)
        {
        case OMP_E_PARDO:              
          pragma_typ = build_ir_int_const ((CONSZ) OMP_PARDO_END,
                                           inttype, 0);
          break;
        
        case OMP_E_PARSECTIONS:
          pragma_typ = build_ir_int_const ((CONSZ) OMP_PARSECTIONS_END,
                                           inttype, 0);
          break;

        default:
          gcc_assert (0);
        }
    }
  else
    {
      pragma_typ = build_ir_int_const ((CONSZ) OMP_PARALLEL_END,
                                       inttype, 0);
    }

  generate_exception_label(cur_omp_context);
  
  lineno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                               inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               lineno,
                               undeftype,
                               NULL);
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = END_PRAGMA_LINE(ir_location);
  
  pop_omp_context ();
}

/* Generate

   PRAGMAINFO --> for
   LOOPINFO for the loop
   
   IR_pragma  OMP_FOR_BEGIN
       for body
   IR_pragma OMP_FOR_END

   for may contain the following clauses
   make sure we fill them out
     - private
     - firstprivate
     - lastprivate
     - reduction
     - ordered
     - schedule
     - chunksize
     - nowait

   If it is a combined parallel region, then we will
   generate a OMP_PAR_DO instead of OMP_FOR. Note: In
   this case the parent pragma info may already be
   populated with the clauses the parallel region can
   carry. We simply append any other clauses we find
   in the omp for region
     - num_threads
     - if
     - private
     - shared
     - firstprivate
     - lastprivate
     - default
     - reduction
     - copyin
     
   The for body generated in the OMP_FOR does not contain
   any explicit loop, we will need to recreate the loop
   structure for iropt. We generate the loop pre-body
   before the praga, and expand the init/cond/ in the following
   following way

     loop init
     loop cond (goto L0 or L1)
     L0:
     L2: 
       loop body
       loop increment
       loop cond goto L0 or L2
     L1:
       End pragma */

static void
dump_omp_for (tree stmt)
{
  tree clauses, loop_index;
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno, *cond, *index;
  PRAGMAINFO *pinfo;
  IR_NODE *loop_body, *loop_exit;
  int l0_lab, l1_lab, l2_lab;
  TRIPLE *t;
  LOOPINFO *linfo;
  
  clauses = OMP_FOR_CLAUSES (stmt);
  
  if (cur_omp_context
      && TREE_CODE (cur_omp_context->stmt) == OMP_PARALLEL
      && OMP_PARALLEL_COMBINED (cur_omp_context->stmt))
    {
      /* combined parallel for. Do not allocate another praga info */
      pinfo = cur_omp_context->pinfo;
      pinfo->type = OMP_E_PARDO;
    }
  else
    {
      pinfo = build_ir_pragmainfo ();
      pinfo->type = OMP_E_DO;
      pinfo->begin_lineno = ir_location.line;
    }
  
  push_omp_context (pinfo, stmt);

  /* Generate the collapse clause, may have side effects
     For now nowhere to put the collapse leaf */
  pinfo->u.s.collapse = dump_collapse_clause (clauses);
  
  /* The generation of the chunksize clause may have
     side effects, dump it first before creating the
     pragma triple */
  dump_schedule_chunksize (clauses, pinfo);

  /* Generate the pre loop body. The upper bound/step
     may be invariants and must be generated outside
     the pragma region */
  dump_function_ir_statements (OMP_FOR_PRE_BODY (stmt));

  pragma_typ = build_ir_int_const ((CONSZ) (pinfo->type == OMP_E_PARDO ? OMP_PARDO_BEGIN : OMP_DO_BEGIN),
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);

  if (pinfo->type == OMP_E_DO)
    pinfo->is_end_nowait =
        (find_omp_clause (clauses, OMP_CLAUSE_NOWAIT) != NULL_TREE);

  pinfo->has_order = (find_omp_clause (clauses, OMP_CLAUSE_ORDERED) != NULL_TREE);
  pinfo->begin_triple = (TRIPLE *) pragma_tp;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);

  fill_scope_info (pinfo->type, clauses, pinfo);

  /* We need the loop index and the loop body label to
     construct the loop info, The loop index info should
     be available from the OMP_FOR_INIT body. */
  
  loop_index = OMP_FOR_INIT (stmt);
  gcc_assert (TREE_CODE (loop_index) == GIMPLE_MODIFY_STMT);
  loop_index = GIMPLE_STMT_OPERAND (loop_index, 0);
  gcc_assert (DECL_P (loop_index));
  l0_lab = gen_ir_label ();
  l1_lab = gen_ir_label ();
  l2_lab = gen_ir_label ();
  cur_omp_context->l1_lab = l1_lab;
  cur_omp_context->l2_lab = l2_lab;
  
  /* Generate the loop init body. Our index var will
     be created during the process. */
  save_and_switch_line_information (OMP_FOR_INIT (stmt));
  dump_function_ir_statements (OMP_FOR_INIT (stmt));
  restore_line_information (OMP_FOR_INIT (stmt));
  
  /* Generate loopinfo structure */
  index = dump_ir_expr (loop_index, MAP_FOR_VALUE);
  gcc_assert (index->operand.tag == ISLEAF);
  linfo = build_ir_loop ();
  linfo->looplabel = (LEAF *) build_ir_int_const ((CONSZ)l2_lab,
                                                  inttype, 0);
  linfo->loopfilename = pinfo->filename;
  linfo->looplineno = (LEAF *) build_ir_int_const ((CONSZ)pinfo->begin_lineno,
                                                   inttype, 0);
  linfo->loop_type = LT_FOR;
  linfo->loopindex = index;
  linfo->schedtype = pinfo->u.s.schedtype;
  linfo->ncpus = (LEAF *) build_ir_int_const ((CONSZ)0, inttype, 0);
  /* Rest of loopinfo struct is essentially zero */
  
  /* Generate the loop body */
  cond = dump_ir_expr (OMP_FOR_COND (stmt), MAP_FOR_VALUE);
  loop_body = build_ir_labelref (l0_lab, 1);
  loop_exit = build_ir_labelref (l1_lab, 0);
  t = (TRIPLE *) loop_body;
  TAPPEND(t, (TRIPLE *) loop_exit);
  loop_body = (IR_NODE *)t;

  build_ir_triple (IR_CBRANCH, cond, loop_body, longtype, NULL);
  build_ir_labeldef (l0_lab);
  build_ir_labeldef (l2_lab);
}

static void
dump_omp_for_end (tree stmt)
{
  IR_NODE *pragma_typ, *pragma_tp, *lno, *cond;
  PRAGMAINFO *pinfo;
  IR_NODE *loop_body, *loop_exit;
  TRIPLE *t;
  
  /* Make sure we stash off the correct line number here
     as genertion the loop bounds check will distort this
     information */
  save_and_switch_line_information (stmt);
  
  /* Fix 6627216. For omp_for regions, we need the
     exceptions to be terminated inside the for loop
     that is any edges going out must be connected
     to the loop exit test, and not the omp end
     pragma as in the other constructs, as iropt
     has trouble discovering the loop. It sees it
     as multi exit loop. */
  generate_exception_label(cur_omp_context);
  
  dump_function_ir_statements (OMP_FOR_INCR (stmt));
  restore_line_information (stmt);
  save_and_switch_line_information (OMP_FOR_COND (stmt));
  cond = dump_ir_expr (OMP_FOR_COND (stmt), MAP_FOR_VALUE);
  loop_body = build_ir_labelref (cur_omp_context->l2_lab, 1);
  loop_exit = build_ir_labelref (cur_omp_context->l1_lab, 0);
  t = (TRIPLE *) loop_body;
  TAPPEND(t, (TRIPLE *) loop_exit);
  build_ir_triple (IR_CBRANCH, cond, loop_body, longtype, NULL);
  build_ir_labeldef (cur_omp_context->l1_lab);
  restore_line_information (OMP_FOR_COND (stmt));
  
  if (cur_omp_context->prev_ctx
      && OMP_PARALLEL_COMBINED (cur_omp_context->prev_ctx->stmt)) {
      /*
       * combined parallel for!. end pragma will be generated by end parallel 
       */
      pop_omp_context ();
      return;
  }

  pinfo = cur_omp_context->pinfo;
  pragma_typ = build_ir_int_const ((CONSZ) OMP_DO_END,
                                   inttype, 0);
  lno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                            inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               lno,
                               undeftype,
                               NULL);
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = END_PRAGMA_LINE(ir_location);

  pop_omp_context ();
}

/* Generate

   PRAGMAINFO for sections
   
   IR_pragma  OMP_SECTIONS_BEGIN
       sections body
   IR_pragma OMP_SECTIONS_END
   
   The sections body may consist of further
   #pragma omp section which we will convert
   when we walk the body of the sections and
   visit dump_omp_section

   Sections may contain the following clauses
   make sure we fill them out
     - private
     - firstprivate
     - lastprivate
     - reduction
     - nowait

   If it is a combined parallel region, then we will
   generate a OMP_PAR_SECTIONS instead of OMP_SECTIONS. Note: In
   this case the parent pragma info may already be
   populated with the clauses the parallel region can
   carry. We simply append any other clauses we find
   in the omp sections region
     - num_threads
     - if
     - private
     - shared
     - firstprivate
     - lastprivate
     - default
     - reduction
     - copyin
*/

static void
dump_omp_sections (tree stmt)
{
  tree clauses;
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;
  PRAGMAINFO *pinfo;
  
  if (cur_omp_context
      && TREE_CODE (cur_omp_context->stmt) == OMP_PARALLEL
      && OMP_PARALLEL_COMBINED (cur_omp_context->stmt))
    {
      /* combined parallel for. Do not allocate another praga info */
      pinfo = cur_omp_context->pinfo;
      pinfo->type = OMP_E_PARSECTIONS;
    }
  else
    {
      pinfo = build_ir_pragmainfo ();
      pinfo->type = OMP_E_SECTIONS;
      pinfo->begin_lineno = ir_location.line;
    }

  clauses = OMP_SECTIONS_CLAUSES (stmt);
 
  pragma_typ = build_ir_int_const ((CONSZ) (pinfo->type == OMP_E_PARSECTIONS ?
                                            OMP_PARSECTIONS_BEGIN : OMP_SECTIONS_BEGIN),
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  
  pinfo->begin_triple = (TRIPLE *) pragma_tp;
  pinfo->begin_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);

  if (pinfo->type != OMP_E_PARSECTIONS)
    pinfo->is_end_nowait =
        (find_omp_clause (clauses, OMP_CLAUSE_NOWAIT) != NULL_TREE);

  fill_scope_info (pinfo->type, clauses, pinfo);
  push_omp_context (pinfo, stmt);
}

static void
dump_omp_sections_end (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *pragma_tp, *lno;
  PRAGMAINFO *pinfo;

  /* Make sure we stash off the correct line number here
     as genertion the loop bounds check will distort this
     information */
  if (cur_omp_context->prev_ctx
      && OMP_PARALLEL_COMBINED (cur_omp_context->prev_ctx->stmt)) {
      /*
       * combined parallel for!. end pragma will be generated by end parallel 
       */
      pop_omp_context ();
      return;
  }

  generate_exception_label(cur_omp_context);
  
  pinfo = cur_omp_context->pinfo;
  pragma_typ = build_ir_int_const ((CONSZ) OMP_SECTIONS_END,
                                   inttype, 0);
  lno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                            inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               lno,
                               undeftype,
                               NULL);
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = END_PRAGMA_LINE(ir_location);

  pop_omp_context ();
}

/* Generate
   IR_pragma OMP_SECTION
   body of section

   NOTE: There is no OMP_SECTION_END in ir.
   we should probably add it */

static void
dump_omp_section (tree stmt)
{
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_IR_SECTION,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  push_omp_context (NULL, stmt);
}


static void
dump_omp_section_end (tree stmt ATTRIBUTE_UNUSED)
{
  generate_exception_label(cur_omp_context);
  
  /* No end sections pragma ! */
  pop_omp_context ();
}

/* Generate

   PRAGMAINFO for single
   
   IR_pragma  OMP_SINGLE_BEGIN
       single body
   IR_pragma OMP_SINGLE_END

   Sections may contain the following clauses
   make sure we fill them out
     - private
     - firstprivate
     - copyprivate
     - nowait
*/

static void
dump_omp_single (tree stmt)
{
  tree clauses;
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;
  PRAGMAINFO *pinfo;

  clauses = OMP_SINGLE_CLAUSES (stmt);
  pinfo = build_ir_pragmainfo ();
  pinfo->type = OMP_E_SINGLE;
  pinfo->begin_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);
  
  pinfo->is_end_nowait = (find_omp_clause (clauses, OMP_CLAUSE_NOWAIT) != NULL_TREE);

  /* Fix 6594853. Must fill scope info first as this
     has side effect of IR generation which must happen
     before begin pragma triple */
  fill_scope_info (OMP_E_SINGLE, clauses, pinfo);
  
  pragma_typ = build_ir_int_const ((CONSZ) OMP_SINGLE_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  pinfo->begin_triple = (TRIPLE *) pragma_tp;
  
  push_omp_context (pinfo, stmt);
}

static void
dump_omp_single_end (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;
  PRAGMAINFO *pinfo;

  generate_exception_label(cur_omp_context);
  pinfo = cur_omp_context->pinfo;
  
  pragma_typ = build_ir_int_const ((CONSZ) OMP_SINGLE_END,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = END_PRAGMA_LINE(ir_location);
  
  pop_omp_context ();
}

/* Generate
   IR_pragma  OMP_MASTER_BEGIN
   master body
   IR_pragma OMP_MASTER_END */
static void
dump_omp_master (tree stmt)
{
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_MASTER_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  push_omp_context (NULL, stmt);
}

static void
dump_omp_master_end (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *pragma_tp;
  IR_NODE *begin_lno;

  generate_exception_label(cur_omp_context);
  gcc_assert (cur_omp_context->pinfo == NULL);
  
  pragma_typ = build_ir_int_const ((CONSZ) OMP_MASTER_END,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  
  pop_omp_context ();
}

/* Generate
   IR_pragma  OMP_ORDER_BEGIN
   ordered body
   IR_pragma OMP_ORDER_END */

static void
dump_omp_ordered (tree stmt)
{
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_ORDER_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  
  push_omp_context (NULL, stmt);
}

static void
dump_omp_ordered_end (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *pragma_tp;
  IR_NODE *begin_lno;

  generate_exception_label(cur_omp_context);
  gcc_assert (cur_omp_context->pinfo == NULL);
  
  pragma_typ = build_ir_int_const ((CONSZ) OMP_ORDER_END,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  
  pop_omp_context ();
}

/* Generate IR for OMP_CRITICAL statement. Save decl's created
   for named critical section in the splay tree. See omp-low.c
   Generate

   PRAGMAINFO -> critical name list
   
   IR_pragma OMP_CRITICAL_BEGIN
   critical body
   IR_pragma OMP_CRITICAL_END */

static GTY((param1_is (tree), param2_is (tree)))
  splay_tree critical_name_mutexes;
  
static void
dump_omp_critical (tree stmt)
{
  tree lock_name, decl;
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;
  IR_NODE *lock_var;
  PRAGMAINFO *pinfo;
  
  pragma_typ = build_ir_int_const ((CONSZ) OMP_CRITICAL_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  
  pinfo = build_ir_pragmainfo ();
  pinfo->type = OMP_E_CRITICAL;
  
  pinfo->begin_triple = (TRIPLE *) pragma_tp;
  pinfo->begin_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);
  
  lock_name = OMP_CRITICAL_NAME (stmt);

  if (lock_name)
    {
      splay_tree_node n;
      
      if (!critical_name_mutexes)
        critical_name_mutexes
            = splay_tree_new_ggc (splay_tree_compare_pointers);

      n = splay_tree_lookup (critical_name_mutexes, (splay_tree_key) lock_name);
      if (n == NULL)
        {
          char *new_str;
	  decl = create_tmp_var_raw (intSI_type_node, NULL);
	  new_str = ACONCAT (("__mt_critical_",
			      IDENTIFIER_POINTER (lock_name), NULL));
	  DECL_NAME (decl) = get_identifier (new_str);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  DECL_COMMON (decl) = 1;
	  DECL_ARTIFICIAL (decl) = 1;
	  DECL_IGNORED_P (decl) = 1;
	  varpool_finalize_decl (decl);

	  splay_tree_insert (critical_name_mutexes, (splay_tree_key) lock_name,
			     (splay_tree_value) decl);
        }
      else
          decl = (tree) n->value;
      
      lock_var = dump_ir_expr (decl, MAP_FOR_VALUE);
      gcc_assert (lock_var->operand.tag == ISLEAF);
      pinfo->u.cs_lock_leaf = (LEAF *) lock_var;
    }

  push_omp_context (pinfo, stmt);
}

static void
dump_omp_critical_end (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *pragma_tp;
  PRAGMAINFO *pinfo;
  IR_NODE *begin_lno;

  generate_exception_label(cur_omp_context);
  pinfo = cur_omp_context->pinfo;
  
  pragma_typ = build_ir_int_const ((CONSZ) OMP_CRITICAL_END,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = END_PRAGMA_LINE(ir_location);
  
  pop_omp_context ();
}

/* Convert the GOMP builtin function call to a
   pragma expected by the IR backend. Generate
   IR_pragma OMP_BARRIER */

static void
dump_omp_barrier (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_BARRIER,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

/* Convert the GOMP builtin atomic start to the pragma
   for iropt. */

static void
dump_omp_atomic_start (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_ATOMIC_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

static void
dump_omp_atomic_end (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_ATOMIC_END,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

/* Convert call to BUILTIN_SYNC to pragma
   OMP FLUSH. The argument list contains
   the list of variables to be flushed. */

static void
dump_omp_flush (tree stmt)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;
  PRAGMAINFO *pinfo;
  tree op1;
  LIST *lp;
  IR_NODE *var;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_FLUSH,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);

  pinfo = build_ir_pragmainfo ();
  pinfo->type = OMP_E_FLUSH;
  
  pinfo->begin_triple = (TRIPLE *) pragma_tp;
  pinfo->begin_lineno = ir_location.line;
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);
  
  op1 = CALL_EXPR_ARGS (stmt); /* list of arguments */
  for (; op1 != NULL_TREE; op1 = TREE_CHAIN (op1))
    {
      var = dump_ir_expr (TREE_VALUE (op1), MAP_FOR_VALUE);
      gcc_assert (var->operand.tag == ISLEAF);
      lp = build_ir_proc_list ();
      lp->datap = (LDATA *) var;
      LAPPEND (pinfo->u.flush, lp);
    }
}

static void
dump_omp_task (tree stmt)
{
  tree clauses;
  PRAGMAINFO *pinfo;
  IR_NODE *pragma_typ, *pragma_tp, *begin_lno;
  
  clauses = OMP_TASK_CLAUSES (stmt);

  pinfo = build_ir_pragmainfo ();
  push_omp_context (pinfo, stmt);
  pinfo->type = OMP_E_TASK;
  pinfo->begin_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);
  pinfo->is_task_untied =
      (find_omp_clause (clauses, OMP_CLAUSE_UNTIED) != NULL_TREE);

  fill_scope_info (OMP_E_TASK, clauses, pinfo);

  pragma_typ = build_ir_int_const ((CONSZ) OMP_TASK_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
  pinfo->begin_triple = (TRIPLE *) pragma_tp;
}

static void
dump_omp_task_end (tree stmt ATTRIBUTE_UNUSED)
{
  PRAGMAINFO *pinfo;
  IR_NODE *pragma_typ, *pragma_tp, *lineno;
  
  pinfo = cur_omp_context->pinfo;
  pragma_typ = build_ir_int_const ((CONSZ) OMP_TASK_END,
                                   inttype, 0);
  generate_exception_label(cur_omp_context);
  
  lineno = build_ir_int_const ((CONSZ) END_PRAGMA_LINE(ir_location),
                               inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               lineno,
                               undeftype,
                               NULL);
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = END_PRAGMA_LINE(ir_location);
  
  pop_omp_context ();
}
    
static void
dump_omp_taskwait (tree stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_IR_TASKWAIT, inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line, inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

/* Given a thread private decl, creat the pointer to
   the thread private decls and save it. We will require
   it later to generate debugging stabs also. */

typedef struct tp_info GTY(())
{
  /* The special __tp pointer */
  tree tp_var;

  /* C++ specific fields. valid only
     on C++ decl's. */
  tree default_ctor;
  tree default_copyctor;
  tree default_copyassign;
} tp_info;
    
static GTY((param1_is (tree), param2_is (struct tp_info *)))
  splay_tree thread_private_list;

void
register_threadprivate_variable (tree tpvar, tree ctor,
                                 tree copyctor, tree copyassgn)
{
  splay_tree_node n;
  tree decl;
  tp_info *tp;
  
  if (!thread_private_list)
    thread_private_list
        = splay_tree_new_ggc (splay_tree_compare_pointers);

  n = splay_tree_lookup (thread_private_list, (splay_tree_key) tpvar);
  if (n == NULL)
    {
        /* Should'nt it always be null!. */
        char *new_str;
        decl = create_tmp_var_raw (ptr_type_node, NULL);
        new_str = ACONCAT (("__tls_ptr_",
                            IDENTIFIER_POINTER (DECL_NAME (tpvar)), NULL));
        DECL_NAME (decl) = get_identifier (new_str);
        TREE_PUBLIC (decl) = 1;
        TREE_STATIC (decl) = 1;
        DECL_COMMON (decl) = 1;
        DECL_ARTIFICIAL (decl) = 1;
        DECL_IGNORED_P (decl) = 1;
        varpool_finalize_decl (decl);

        tp = xmalloc (sizeof (tp_info));
        tp->tp_var = decl;
        tp->default_ctor = ctor;
        tp->default_copyctor = copyctor;
        tp->default_copyassign = copyassgn;
        
        splay_tree_insert (thread_private_list, (splay_tree_key) tpvar,
                           (splay_tree_value) tp);
    }
}

void
remove_threadprivate_variable (tree decl)
{
  struct varpool_node *node;
  tree tls_ptr;

  tls_ptr = lookup_threadprivate_variable (decl);
  if (tls_ptr == NULL_TREE)
    return;

  node = varpool_node (tls_ptr);

  /* Skipped dumping it in varpool_assemble_decl. */
  DECL_EXTERNAL (tls_ptr) = 1;

  /* Marked it as candidate for varpool_remove_unreferenced_decls. */
  node->finalized = false;

  splay_tree_remove (thread_private_list, (splay_tree_key) decl);
}

tree
lookup_threadprivate_variable (tree decl)
{
  splay_tree_node n;
  tp_info *tp;
  /* gcc_assert (thread_private_list != NULL); */

  if (thread_private_list == NULL)
    return NULL_TREE;
  
  n = splay_tree_lookup (thread_private_list, (splay_tree_key) decl);
  if (n == NULL)
    return NULL_TREE;
  
  tp = (tp_info *) n->value;
  return tp->tp_var;
}

/* Generate a call
   void
   __mt_declare_threadprivate_ (char *<variable name>
                                char *<address of TP (var or common block)>
                                size_t size <size of TP in bytes>
                                char ***<address of tls_ptr>
                                int <line no>
                                char *<file name> */
static tree
emit_threadprivate_initializer (tree var, tree tp_var)
{
  tree args, t;
  const char *name;
  tree arg1, arg2, arg3, arg4, arg5, arg6, type;
  
  ir_location = expand_location (DECL_SOURCE_LOCATION (var));
  name = (*targetm.strip_name_encoding) (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (var)));
  
  arg1 = build_string_literal (strlen (name) + 1, name);
  type = build_pointer_type (TREE_TYPE (var));
  arg2 = build1 (ADDR_EXPR, type, var);
  arg3 = build_int_cst (sizetype, get_type_size (TREE_TYPE(var)));
  type = build_pointer_type (TREE_TYPE (tp_var));
  arg4 = build1 (ADDR_EXPR, type, tp_var);
  if (ir_location.file)
    {
      arg5 = build_int_cst (NULL_TREE, ir_location.line);
      arg6 = build_string_literal (strlen (ir_location.file) + 1, ir_location.file);
    }
  else
    {
      arg5 = build_int_cst (NULL_TREE, 0);
      arg5 = build_int_cst (ptr_type_node, 0);
    }
        
  args = tree_cons (NULL_TREE, arg6, NULL_TREE);
  args = tree_cons (NULL_TREE, arg5, args);
  args = tree_cons (NULL_TREE, arg4, args);
  args = tree_cons (NULL_TREE, arg3, args);
  args = tree_cons (NULL_TREE, arg2, args);
  args = tree_cons (NULL_TREE, arg1, args);

  t = build_function_type_list (void_type_node, void_type_node, NULL);
  t = build_decl (FUNCTION_DECL, get_identifier ("__mt_declare_threadprivate_"), t);
  
  t = build_function_call_expr (t, args);
  return t;
}

/* Generate a call
   void
   __mt_declare_threadprivate_class_object_ (char *<variable name>
                                char *<address of TP (var or common block)>
                                size_t size <size of TP in bytes>
                                char ***<address of tls_ptr>
                                void (*ctor) (),
                                void (*copy_ctor) (),
                                void (*copy_assign) (),
                                int <line no>
                                char *<file name> */
/* when flag_new_tp is 1, generate a call
   void
   __mt_init_tp_class_obj (char *name <variable name>
                       char *base <address of user declared TP variable>
                       size_t size <size of class object>
                       char ***ptls_ptr <address of tls_ptr>
                       void (*init_fn) (void *)
                       void (*copy_assign)()
                       int line_no
                       char *source_file);
*/

static tree
emit_cxx_threadprivate_initializer (tree var, tree tp_var, tree ctor,
                                    tree copyctor, tree copy_assign)
{
  tree args, t;
  const char *name;
  tree arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, type;
  struct cgraph_node* node;
  
  ir_location = expand_location (DECL_SOURCE_LOCATION (var));
  name = (*targetm.strip_name_encoding) (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (var)));
  
  arg1 = build_string_literal (strlen (name) + 1, name);
  type = build_pointer_type (TREE_TYPE (var));
  arg2 = build1 (ADDR_EXPR, type, var);
  arg3 = build_int_cst (sizetype, get_type_size (TREE_TYPE(var)));
  type = build_pointer_type (TREE_TYPE (tp_var));
  arg4 = build1 (ADDR_EXPR, type, tp_var);

  if (ctor != NULL_TREE)
    {
      /* Force generation of the body for IR */
      if (DECL_SAVED_TREE (ctor)
          && !TREE_ASM_WRITTEN (ctor))
        {
          node = cgraph_node (ctor);
          cgraph_finalize_function (ctor, IR_FALSE);
          cgraph_mark_needed_node (node);
        }
      type = build_pointer_type (TREE_TYPE (ctor));
      arg5 = build1 (ADDR_EXPR, type, ctor);
    }
  else
    arg5 = build_int_cst (ptr_type_node, 0);

  if (copyctor != NULL_TREE)
    {
      /* Force generation of the body for IR */
      if (DECL_SAVED_TREE (copyctor)
          && !TREE_ASM_WRITTEN (copyctor))
        {
          node = cgraph_node (copyctor);
          cgraph_finalize_function (copyctor, IR_FALSE);
          cgraph_mark_needed_node (node);
        }
      type = build_pointer_type (TREE_TYPE (copyctor));
      arg6 = build1 (ADDR_EXPR, type, copyctor);
    }
  else
    arg6 = build_int_cst (ptr_type_node, 0);

  if (copy_assign != NULL_TREE)
    {
      /* Force generation of the body for IR */
      if (DECL_SAVED_TREE (copy_assign)
          && !TREE_ASM_WRITTEN (copy_assign))
        {
          node = cgraph_node (copy_assign);
          cgraph_finalize_function (copy_assign, IR_FALSE);
          cgraph_mark_needed_node (node);
        }
      type = build_pointer_type (TREE_TYPE (copy_assign));
      arg7 = build1 (ADDR_EXPR, type, copy_assign);
    }
  else
    arg7 = build_int_cst (ptr_type_node, 0);
  
   if (ir_location.file)
    {
      arg8 = build_int_cst (NULL_TREE, ir_location.line);
      arg9 = build_string_literal (strlen (ir_location.file) + 1, ir_location.file);
    }
  else
    {
      arg8 = build_int_cst (NULL_TREE, 0);
      arg9 = build_int_cst (ptr_type_node, 0);
    }

  args = tree_cons (NULL_TREE, arg9, NULL_TREE);
  args = tree_cons (NULL_TREE, arg8, args);
  args = tree_cons (NULL_TREE, arg7, args);
  if ( flag_new_tp == 0)
    args = tree_cons (NULL_TREE, arg6, args);
  args = tree_cons (NULL_TREE, arg5, args);
  args = tree_cons (NULL_TREE, arg4, args);
  args = tree_cons (NULL_TREE, arg3, args);
  args = tree_cons (NULL_TREE, arg2, args);
  args = tree_cons (NULL_TREE, arg1, args);

  t = build_function_type_list (void_type_node, void_type_node, NULL);
  if ( flag_new_tp == 0)
    t = build_decl (FUNCTION_DECL, get_identifier ("__mt_declare_threadprivate_class_object_"), t);
  else
    t = build_decl (FUNCTION_DECL, get_identifier ("__mt_init_tp_class_obj"), t);
  
  t = build_function_call_expr (t, args);
  return t;
}
  
static int
dump_one_tp_association (splay_tree_node n, void *data)
{
  tree t, *stmt_p;
  tp_info *tp;
  
  stmt_p = (tree *) data;
  tp = (tp_info *) n->value;

  if (tp->default_ctor || tp->default_copyctor || tp->default_copyassign)
    t = emit_cxx_threadprivate_initializer ((tree) n->key, tp->tp_var, tp->default_ctor,
                                            tp->default_copyctor,
                                            tp->default_copyassign);
  else
    t = emit_threadprivate_initializer ((tree) n->key, tp->tp_var);
  
  append_to_statement_list (t, stmt_p);
  return 0;
}

  
/* when flag_new_tp is 1, generate a call
   void
   __mt_fini_tp_class_obj (
                       char *base <address of user declared TP variable>
                       void (*fini_fn) (void *))
*/

static tree
emit_cxx_threadprivate_finisher (tree var, tree copyctor)
{
  tree args, t;
  tree arg1, arg2, type;
  struct cgraph_node* node;
  
  ir_location = expand_location (DECL_SOURCE_LOCATION (var));
  type = build_pointer_type (TREE_TYPE (var));
  arg1 = build1 (ADDR_EXPR, type, var);
  
  if (copyctor != NULL_TREE)
    {
      /* Force generation of the body for IR */
      if (DECL_SAVED_TREE (copyctor)
          && !TREE_ASM_WRITTEN (copyctor))
        {
          node = cgraph_node (copyctor);
          cgraph_finalize_function (copyctor, IR_FALSE);
          cgraph_mark_needed_node (node);
        }
      type = build_pointer_type (TREE_TYPE (copyctor));
      arg2 = build1 (ADDR_EXPR, type, copyctor);
    }
  else
    arg2 = build_int_cst (ptr_type_node, 0);

  args = tree_cons (NULL_TREE, arg2, NULL_TREE);
  args = tree_cons (NULL_TREE, arg1, args);

  t = build_function_type_list (void_type_node, void_type_node, NULL);
  t = build_decl (FUNCTION_DECL, get_identifier ("__mt_fini_tp_class_obj"), t);
  t = build_function_call_expr (t, args);
  return t;
}
  
static int
dump_one_tp_association_fini (splay_tree_node n, void *data)
{
  tree t, *stmt_p;
  tp_info *tp;
  
  stmt_p = (tree *) data;
  tp = (tp_info *) n->value;

  if (tp->default_ctor || tp->default_copyctor || tp->default_copyassign)
    {
      t = emit_cxx_threadprivate_finisher ((tree) n->key, tp->default_copyctor);
      append_to_statement_list (t, stmt_p);
    }
  return 0;
}

static void
dump_ir_threadprivate_fn_1 (int initp)
{
  tree decl, resdecl, body, t;

  body = NULL_TREE;
  if (initp == 1)
    decl = build_fn_decl ("__init_task_common",
                        build_function_type (void_type_node, void_list_node));
  else 
    decl = build_fn_decl ("__fini_task_common",
                        build_function_type (void_type_node, void_list_node));

  current_function_decl = decl;

  resdecl = build_decl (RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (resdecl) = 1;
  DECL_IGNORED_P (resdecl) = 1;
  DECL_RESULT (decl) = resdecl;

  allocate_struct_function (decl, false);

  TREE_STATIC (decl) = 1;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 1;
  TREE_PUBLIC (decl) = 0;
  DECL_EXTERNAL (decl) = 0;
  DECL_NO_INSTRUMENT_FUNCTION_ENTRY_EXIT (decl) = 1;
  DECL_UNINLINABLE (decl) = 1;
  /* Do not allow globalization of this function name */
  SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));

  DECL_SOURCE_LOCATION (decl) = input_location;
  cfun->function_end_locus = input_location;

  t = build3 (BIND_EXPR, void_type_node, NULL_TREE,
              NULL_TREE, NULL_TREE);
  TREE_SIDE_EFFECTS (t) = 1;

  if (initp == 1)
    splay_tree_foreach (thread_private_list, dump_one_tp_association, &body);
  else
    splay_tree_foreach (thread_private_list, dump_one_tp_association_fini, &body);

  if (body == NULL_TREE)
    {
      /* generate a empty function body. use a artificial_label to make sure it is not deleted. */
      tree label_decl = create_artificial_label ();
      tree stmt;
      stmt = build1 (GOTO_EXPR, void_type_node, label_decl);
      append_to_statement_list (stmt, &body);
      stmt = build1 (LABEL_EXPR, void_type_node, label_decl);
      append_to_statement_list (stmt, &body);
    }

  BIND_EXPR_BODY (t) = body;
  DECL_SAVED_TREE (decl) = t;
  DECL_INITIAL (decl) = make_node (BLOCK);
  BLOCK_SUPERCONTEXT (DECL_INITIAL (decl)) = decl;
  DECL_CONTEXT (DECL_RESULT (decl)) = decl;

  /* Fix 6606178. Call finalize_function. If we
     are not compiling unit at a time, we will
     end up emitting this function immediately.
     If not it will be deferred for later. However
     ensure we mark the node as needed, if emitted later */
  cgraph_finalize_function (decl, IR_FALSE);
  struct cgraph_node *n = cgraph_node (decl);
  cgraph_mark_needed_node (n);
}

void
dump_ir_threadprivate_fn (void)
{
  tree body, t;

  generate_cxx_constructor_wrappers ();
  
  if (!thread_private_list)
    return;

  /* initializae part */
  dump_ir_threadprivate_fn_1 (true);

  /* Now generate a call to the function __init_task_common */
  body = NULL;
  t = build_decl (FUNCTION_DECL, get_identifier ("__init_task_common"),
                  build_function_type_list (void_type_node, void_type_node, NULL));
  t = build_function_call_expr (t, NULL);
  append_to_statement_list (t, &body);
  cgraph_build_static_cdtor ('I', body, DEFAULT_INIT_PRIORITY);

  /* destruction part */
  if (flag_new_tp == 1 && ir_language == CDOUBLEPLUS)
  {
  dump_ir_threadprivate_fn_1 (false);

  /* Now generate a call to the function __fini_task_common */
  body = NULL;
  t = build_decl (FUNCTION_DECL, get_identifier ("__fini_task_common"),
                  build_function_type_list (void_type_node, void_type_node, NULL));
  t = build_function_call_expr (t, NULL);
  append_to_statement_list (t, &body);
  cgraph_build_static_cdtor ('D', body, DEFAULT_INIT_PRIORITY);
  }
}

/* We may have constructors that accept default parametres
   When generating IR for OpenMP, we will need to create
   a wrapper function that behaves like the default constructor
   the will accept the instance and call the real constructor
   with the default parametres

   class A {
   public:
     A(int x=3);
   };

   we will generate something like

   __constructor_wrapper_1 (A *a) {
      A::A (a, 3);
   }
*/

/* Save constructor, destructor etc. decl pairs for later emission */
    
static GTY((param1_is (tree), param2_is (tree)))
  splay_tree cxx_constructor_list;
  
static GTY((param1_is (tree), param2_is (tree)))
  splay_tree cxx_destructor_list;

static GTY((param1_is (tree), param2_is (tree)))
  splay_tree cxx_copy_ctor_list;

static GTY((param1_is (tree), param2_is (tree)))
  splay_tree cxx_copy_assign_list;;

tree
cxx_omp_constructor_wrapper_for_irgen (tree vecs, location_t location, int flag)
{
  tree parm, decl, argtype;
  tree fn = TREE_VEC_ELT (vecs, 0);
  static int unique_count = 0;
  char buf[64];
  splay_tree_node n;
  splay_tree *list;
  
  if (errorcount || sorrycount || fn == NULL)
    return fn;

  parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  argtype = TREE_VALUE (parm);
  parm = TREE_CHAIN (parm);
  
  if (!parm)
    return fn;
 
  switch (flag)
    {
    case 0:
      list = &cxx_destructor_list;
      sprintf (buf, "__destructor_wrapper_%d", unique_count++);
      break;
    case 1:
      list = &cxx_constructor_list;
      sprintf (buf, "__constructor_wrapper_%d", unique_count++);
      break;
    case 2:
      list = &cxx_copy_ctor_list;
      sprintf (buf, "__copy_ctor_wrapper_%d", unique_count++);
      break;
    case 3:
      list = &cxx_copy_assign_list;
      sprintf (buf, "__copy_assign_wrapper_%d", unique_count++);
      break;
    default:
      gcc_unreachable ();
      break;
    }
 
  if (!(*list))
    *list
        = splay_tree_new_ggc (splay_tree_compare_pointers);

  n = splay_tree_lookup (*list, (splay_tree_key) fn);
  if (n == NULL)
    {
      decl = build_decl (FUNCTION_DECL, get_identifier (buf),
                         build_function_type_list (void_type_node,
                                                   argtype, NULL));
      TREE_STATIC (decl) = 1;
      TREE_USED (decl) = 1;
      DECL_ARTIFICIAL (decl) = 1;
      DECL_IGNORED_P (decl) = 1;
      TREE_PUBLIC (decl) = 0;
      DECL_UNINLINABLE (decl) = 1;
      DECL_EXTERNAL (decl) = 0;
      SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));
      DECL_SOURCE_LOCATION (decl) = location;
      splay_tree_insert (*list, (splay_tree_key) vecs,
                         (splay_tree_value) decl);
    }
  else
    decl = (tree) n->value;
  
  return decl;
}

static int
dump_one_constructor_wrapper_1 (splay_tree_node n, int flag)
{
  tree vecs, fn, var, wrapper, parm, t, arg, arg2, argtype, body, bind;
  tree clause, info;
 
  vecs = (tree) n->key;
  fn = TREE_VEC_ELT (vecs, 0);
  var = TREE_VEC_ELT (vecs, 1); 
  wrapper = (tree) n->value;
  body = NULL;
  
  parm = TYPE_ARG_TYPES (TREE_TYPE (fn));
  argtype = TREE_VALUE (parm);

  DECL_CONTEXT (wrapper) = NULL_TREE;
  DECL_INITIAL (wrapper) = make_node (BLOCK);
  
  t = build_decl (RESULT_DECL, NULL_TREE, void_type_node);
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (wrapper) = t;

  allocate_struct_function (wrapper, false);
  cfun->function_end_locus = DECL_SOURCE_LOCATION (wrapper);
  
  bind = build3 (BIND_EXPR, void_type_node, NULL_TREE,
                 NULL_TREE, NULL_TREE);
  TREE_SIDE_EFFECTS (bind) = 1;
  if (TREE_CODE (var) == VAR_DECL && TREE_CODE (TREE_TYPE (var)) == ARRAY_TYPE)
    {
      arg = build_decl (PARM_DECL, DECL_NAME (var), TREE_TYPE (var));
      t = arg;
      DECL_ARTIFICIAL (arg) = 1;
      DECL_ARG_TYPE (arg) = TREE_TYPE (var);
      DECL_CONTEXT (arg) = wrapper;
      TREE_USED (arg) = 1;
      if (flag == 2 || flag == 3)
        {
           arg2 = build_decl (PARM_DECL, get_identifier ("original"), TREE_TYPE (var));
           DECL_ARTIFICIAL (arg2) = 1;
           DECL_ARG_TYPE (arg2) = TREE_TYPE (var);
           DECL_CONTEXT (arg2) = wrapper;
           TREE_USED (arg2) = 1;
           TREE_CHAIN (arg) = arg2;
        }
      DECL_ARGUMENTS (wrapper) = arg;
    }
  else
    {
      arg = build_decl (PARM_DECL, get_identifier ("this"), argtype);
      DECL_ARTIFICIAL (arg) = 1;
      DECL_ARG_TYPE (arg) = argtype;
      DECL_CONTEXT (arg) = wrapper;
      TREE_USED (arg) = 1;
      DECL_ARGUMENTS (wrapper) = arg;
      t = build_fold_indirect_ref (arg);
    }
  append_to_statement_list (t, &body);

  info = make_tree_vec (3);
  clause = build_omp_clause (OMP_CLAUSE_PRIVATE);
  CP_OMP_CLAUSE_INFO (clause) = info;

  switch (flag)
    {
    case 0:
      TREE_VEC_ELT (info, 1) = fn;
      t = lang_hooks.decls.omp_clause_dtor (clause,t);
      break;
    case 1:
      TREE_VEC_ELT (info, 0) = fn;
      t = lang_hooks.decls.omp_clause_default_ctor (clause, t);
      break;
    case 2:
      TREE_VEC_ELT (info, 0) = fn;
      t = lang_hooks.decls.omp_clause_copy_ctor (clause, arg, arg2);
      break;
    case 3:
      TREE_VEC_ELT (info, 2) = fn;
      t = lang_hooks.decls.omp_clause_assign_op (clause, arg, arg2);
      break;
    default:
      gcc_unreachable ();
      break;
    }

  push_gimplify_context ();
  gimplify_and_add (t, &body);
  pop_gimplify_context (NULL_TREE);
  BIND_EXPR_BODY (bind) = body;
  DECL_SAVED_TREE (wrapper) = bind;
  cgraph_finalize_function (wrapper, IR_FALSE);
  struct cgraph_node *node = cgraph_node (wrapper);
  cgraph_mark_needed_node (node);
  
  return 0;
}

static int
dump_one_constructor_wrapper (splay_tree_node n, void *ignore ATTRIBUTE_UNUSED)
{
  return dump_one_constructor_wrapper_1 (n, 1);
}

static int
dump_one_destructor_wrapper (splay_tree_node n, void *ignore ATTRIBUTE_UNUSED)
{
  return dump_one_constructor_wrapper_1 (n, 0);
}

static int
dump_one_copy_ctor_wrapper (splay_tree_node n, void *ignore ATTRIBUTE_UNUSED)
{
  return dump_one_constructor_wrapper_1 (n, 2);
}

static int
dump_one_copy_assign_wrapper (splay_tree_node n, void *ignore ATTRIBUTE_UNUSED)
{
  return dump_one_constructor_wrapper_1 (n, 3);
}

static void
generate_cxx_constructor_wrappers (void)
{
  if (cxx_constructor_list)
    splay_tree_foreach (cxx_constructor_list, dump_one_constructor_wrapper, NULL);

  if (cxx_destructor_list)
    splay_tree_foreach (cxx_destructor_list, dump_one_destructor_wrapper, NULL);

  if (cxx_copy_ctor_list)
    splay_tree_foreach (cxx_copy_ctor_list, dump_one_copy_ctor_wrapper, NULL);

  if (cxx_copy_assign_list)
    splay_tree_foreach (cxx_copy_assign_list, dump_one_copy_assign_wrapper, NULL);
}


static void
dump_omp_return (tree stmt)
{
  gcc_assert (cur_omp_context != NULL);
  
  switch (TREE_CODE(cur_omp_context->stmt))
    {
    case OMP_PARALLEL:
      dump_omp_parallel_end (cur_omp_context->stmt);
      break;

    case OMP_FOR:
      dump_omp_for_end (cur_omp_context->stmt);
      break;

    case OMP_SECTIONS:
      dump_omp_sections_end (cur_omp_context->stmt);
      break;

    case OMP_SECTION:
      dump_omp_section_end (cur_omp_context->stmt);
      break;
            
    case OMP_SINGLE:
      dump_omp_single_end (cur_omp_context->stmt);
      break;

    case OMP_MASTER:
      dump_omp_master_end (cur_omp_context->stmt);
      break;

    case OMP_ORDERED:
      dump_omp_ordered_end (stmt);
      break;

    case OMP_CRITICAL:
      dump_omp_critical_end (stmt);
      break;

    case OMP_TASK:
      dump_omp_task_end (stmt);
      break;

    default:
      gcc_assert (0);
            
    }
}

void
sunir_check_builtin_handling (tree function)
{
  enum built_in_function code = DECL_FUNCTION_CODE (function);

  switch (code)
    {
    case BUILT_IN_UNWIND_INIT:
    case BUILT_IN_DWARF_CFA:
    case BUILT_IN_DWARF_SP_COLUMN:
    case BUILT_IN_INIT_DWARF_REG_SIZES:
    case BUILT_IN_FROB_RETURN_ADDR:
    case BUILT_IN_EXTRACT_RETURN_ADDR:
    case BUILT_IN_EH_RETURN:
    case BUILT_IN_EXTEND_POINTER:
    case BUILT_IN_APPLY:
    case BUILT_IN_APPLY_ARGS:
    /* cannot handle object_size computations without SSA and optimizations */
    case BUILT_IN_OBJECT_SIZE: 
    /* all builtins below most like will be used in the same function
       with builtin_object_size(), so no point generating IR for them */
    case BUILT_IN_MEMCPY_CHK: 
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
    case BUILT_IN_SPRINTF_CHK:
    case BUILT_IN_VSPRINTF_CHK:
    case BUILT_IN_BSWAP32:
    case BUILT_IN_BSWAP64:
      DECL_DONT_GENERATE_SUNIR (current_function_decl) = 1;
      DECL_DONT_GENERATE_SUNIR (function) = 1;
      break;
    default:
      break;
    }
}

int
sunir_check_128bits_handling (tree node)
{
  if (TYPE_IR_TWORD (node)) 
    return 0;

  if (TREE_CODE (node) != ENUMERAL_TYPE 
      && TREE_CODE (node) != INTEGER_TYPE)
    return 0;

  if (TARGET_ARCH64)
    {
      const char *name = 0;
      tree type = TYPE_MAIN_VARIANT (node);
      /* Carefully distinguish all the standard types of C,
         without messing up if the language is not C. */
      if (TYPE_NAME (type) != 0
          && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
          && DECL_NAME (TYPE_NAME (type)) != 0
          && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
        name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
      else
        {
          type = TREE_TYPE (type);
          if (type && TYPE_NAME (type) != 0
              && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
              && DECL_NAME (TYPE_NAME (type)) != 0
              && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
            name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
        }

      if (name
          && (!strcmp (name, "long int")
             || !strcmp (name, "long unsigned int")))
        return 0;
      if (node == bitsizetype || TYPE_PRECISION (node) != 128)
        return 0;
    }
  if (current_function_decl)
    DECL_DONT_GENERATE_SUNIR (current_function_decl) = 1;
  return 1;
}

#include "gt-tree-ir.h"
