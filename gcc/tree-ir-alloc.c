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

#include <string.h>
#include <stdlib.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "toplev.h"
#include "hashtab.h"
#include "tree-ir.h"

static LEAF *last_leaf;
static ir_BLOCK   *current_block;
LEAF *first_leaf;
ir_BLOCK *first_block;
SEGMENT *first_seg, *last_seg;
IR_TYPE_NODE * first_ir_type;

TYPE undeftype = {PCC_UNDEF, 0, 0};
TYPE inttype   = {PCC_INT,   4, 4};
TYPE longtype  = {PCC_LONG,  4, 4};


static int triple_id = 0;

/* Segment descriptors.  */
static struct segdescr_st auto_descr = {AUTO_SEG, STG_SEG, USER_SEG, LCLSTG_SEG, 0, SEGEXT_GLOBAL};
static struct segdescr_st extern_descr = {BSS_SEG, STG_SEG, USER_SEG, EXTSTG_SEG, 0, SEGEXT_GLOBAL};
static struct segdescr_st lstatic_descr = {BSS_SEG, STG_SEG, USER_SEG, LCLSTG_SEG, 0, SEGEXT_GLOBAL};
/* End Segment descriptors.  */

/* Definition for ir_segment hashtable entries.  */
struct ir_segment_elem
{
  const char *segname;
  struct segdescr_st *segdescr;
  int segbase;
  IR_OFFSZ segoffset;
  IR_OFFSZ seglen;
  int segalign;
};

struct ir_segment_entry
{
  struct ir_segment_elem e;
  SEGMENT * p;
};

/* Definition for ir_leaf hashtable entries.  */
struct ir_leaf_elem
{
  LEAF_CLASS class;
  TYPE type;
  IR_TYPE_NODE *typep;
  LEAF_VALUE location;
};

struct ir_leaf_entry
{
  struct ir_leaf_elem e;
  LEAF * p;
};

static LEAF *get_ir_var_leaf (struct ir_segment_elem *, IR_OFFSZ, TYPE, IR_TYPE_NODE *);

static __inline__ int
packed_field_ir_type_node (IR_TYPE_NODE * typep)
{
  return typep && typep->parent; /* all types with parent can be packed */
  /* Ex:
   * typedef struct
   * {
   *   short a __attribute__ ((aligned (2),packed));
   *   short *ap[2]  __attribute__ ((aligned (2),packed));
   * } A;
   *     
   * ty[3]   STRUCT level=BASIC align=2 size=10 off=0 tag=<unknown>
   * ty[4]     a: SHORT parent=ty[3] level=BASIC align=2 size=2 off=0
   * ty[5]     ap: ARRAY parent=ty[3] level=BASIC align=2 size=8 off=2
   * ty[8]       ARRAYDIM parent=ty[5] level=ANY align=4 lower=0 upper=1
   * ty[6]       POINTER parent=ty[5] level=BASIC align=2 size=4 off=0 pointee=ty[7]
   *  ty[6] - is packed, though only 'ap' and 'a' are packed fields */

}

IR_NODE *
build_ir_triple (IR_OP op, IR_NODE *arg1, IR_NODE *arg2,
                 TYPE type, IR_TYPE_NODE *typep)
{
  TRIPLE * t = ir_proc_new_triple(irProc);
  t->tag = ISTRIPLE;
  t->op = op;
  t->left = arg1;
  t->right = arg2;
  t->type = type;
  t->typep = typep;
  t->tripleno = triple_id++;
  t->tprev = t->tnext = t;
  t->line_num = current_lni_handle;

  switch (op) 
    {
    case IR_LABELREF:
    case IR_PARAM:
    case IR_FPARAM:
    case IR_ASM_OUTPUT:
    case IR_ASM_INPUT:
    case IR_ASM_ADDRESS:
    case IR_ASM_CLOBBER:
	break;

    default:
      if (current_block != NULL)
        TAPPEND (current_block->last_triple, t);
      
      /* ir_type_node.align should win over type.align */
      if (packed_field_ir_type_node (typep)
          && (op == IR_IFETCH || op == IR_ISTORE))
        {
          if (1u<<typep->align < type.align)
            t->type.align = 1u<<typep->align;
        }
      else if (op == IR_ASSIGN)
        { 
          if (arg1->operand.tag == ISLEAF
              && arg1->leaf.class == VAR_LEAF
              && packed_field_ir_type_node (arg1->leaf.typep))
            {
              if (1u<<arg1->leaf.typep->align < type.align)
                t->type.align = 1u<<arg1->leaf.typep->align;
            }
          else if (arg2->operand.tag == ISLEAF
                   && arg2->leaf.class == VAR_LEAF
                   && packed_field_ir_type_node (arg2->leaf.typep))
            {
              if (1u<<arg2->leaf.typep->align < type.align)
                t->type.align = 1u<<arg2->leaf.typep->align;
            }
        }
          
    }
  
  return (IR_NODE *)t;
}

TRIPLE*
current_block_last_triple(void)
{
  return current_block ? current_block->last_triple : NULL;
}

void 
remove_ir_triple (TRIPLE *triple)
{
  if (triple->tprev != NULL)
    triple->tprev->tnext = triple->tnext;
  
  if (triple->tnext != NULL)
    triple->tnext->tprev = triple->tprev;

  if (current_block->last_triple == triple)
    current_block->last_triple = triple->tprev;
  
  return;
}


LOOPINFO *
build_ir_loop (void)
{
  LOOPINFO *p;

  p = ir_proc_new_loopinfo(irProc);
  p->tag = ISLOOPINFO;
  p->loopno = ir_proc_num_loopinfos(irProc) - 1;
  p->loop_type = LT_UNKNOWN;
  p->looplabel = NULL;
  p->loopfilename = NULL;
  p->looplineno = NULL;
  p->misc_list = NULL;
  p->pragmas = NULL;
  p->assume_kind = ASSUME_NONE;
  p->assumed_trip_count = 0;
  return p;
}

PRAGMAINFO *
build_ir_pragmainfo (void)
{
  return ir_proc_new_pragmainfo(irProc);
}

LIST *
build_ir_proc_list (void)
{
  LIST * p = ir_proc_new_listnode(irProc);
  p->next = p;
  return p;
}

LIST *
build_ir_mod_list (void)
{
  LIST * p = ir_mod_new_listnode(irMod);
  p->next = p;
  return p;
}


/* build_ir_internal_list() is used to allocate LIST nodes that are NOT part
 * of the IR to be generated. */

#define LISTSPERCHUNK 100
typedef struct list_chunk {
	LIST list_nodes[LISTSPERCHUNK];
	struct list_chunk *prev;
	int first_free;
} List_chunk;

static List_chunk *tail_list_block = NULL;

static LIST *
build_ir_internal_list (void)
{
	LIST *p;

	if (NULL == tail_list_block || 
			tail_list_block->first_free >= LISTSPERCHUNK) {
		/* We need to allocate a new chunk of list nodes. */
		List_chunk *old_tail = tail_list_block;
		tail_list_block = (List_chunk *) xmalloc(sizeof(List_chunk));
		tail_list_block->prev = old_tail;
		tail_list_block->first_free = 0;
	}
	p = &tail_list_block->list_nodes[tail_list_block->first_free++];
	p->next = p;
	return p;
}

static void
free_internal_lists (void)
{
	while (NULL != tail_list_block) {
		List_chunk *old_tail = tail_list_block;
		tail_list_block = tail_list_block->prev;
		free(old_tail);
	}
}

ir_BLOCK *
build_ir_block_nolabel (const char *s, int labelno, int is_entry, int is_global)
{
  ir_BLOCK *p;

  p = ir_proc_new_block(irProc);
  p->tag = ISBLOCK;
  if (current_block == NULL) 
    {
      first_block = current_block = p;
      ir_proc_set_block_head (irProc, first_block);
    } 
  else 
    {
      current_block->next = p;
      current_block = p;
    }
  p->blockno = ir_proc_num_blocks(irProc) - 1;
  if (s != NULL)
    p->entryname = build_ir_proc_string (s);
  
  p->labelno = labelno;
  p->is_ext_entry = is_entry;
  p->entry_is_global = is_global;
  return p;
}

ir_BLOCK *
build_ir_block (const char *s, int labelno, int is_entry, int is_global)
{
  ir_BLOCK *p = build_ir_block_nolabel (s, labelno, is_entry, is_global);
  build_ir_triple (IR_LABELDEF, build_ir_int_const (p->labelno, inttype, 0), NULL, longtype, NULL);
  return p;
}

static char *
build_ir_byte_array (void *p, int len)
{
  char * str = (char *)p;
  char * ptr = ir_proc_new_char_array(irProc, len+1);
  int i;
  for (i = 0; i < len; i++)
    ptr[i] = str[i];
  ptr[len] = '\0';
  return ptr;
}

char *
build_ir_proc_string (const char *str)
{
  return (char *) ir_proc_get_string (irProc, str);
}

char *
build_ir_mod_string (const char *str)
{
  return (char *) ir_mod_get_string (irMod, str);
}

DbgSymID *
build_ir_proc_dbgsym (DbgSymID dbg_symid)
{
  DbgSymID *p = NULL;

  if (DBG_SYM_IS_NULL (dbg_symid))
    return p;

  p = ir_proc_new_dbgid(irProc);
  *p = dbg_symid;
  return p;
}

DbgSymID *
build_ir_mod_dbgsym (DbgSymID dbg_symid)
{
  DbgSymID *p = NULL;

  if (DBG_SYM_IS_NULL (dbg_symid))
    return p;

  p = ir_mod_new_dbgid(irMod);
  *p = dbg_symid;
  return p;
}

IR_TYPE_NODE *
build_ir_type (TWORD tword, int align)
{
  static IR_TYPE_NODE *last_ir_type;
  static int ntypes;
  IR_TYPE_NODE *t;

  t = ir_mod_new_typenode(irMod);
  t->tag = ISTYPE;
  t->tid = tword;
  t->align = align;
  t->typeno = ntypes++;
  t->label = NULL;
  t->parent = NULL;
  t->dbg_sym_id = NULL;
  t->next = NULL;
  if (last_ir_type == NULL) 
    {
      first_ir_type = last_ir_type = t;
      ir_mod_set_type_head(irMod, first_ir_type);
    } 
  else 
    {
      last_ir_type->next = t;
      last_ir_type = t;
    }
  return t;
}

void
fini_ir_proc (void)
{
  free_internal_lists ();

  first_leaf = NULL;
  last_leaf = NULL;
  
  current_block = NULL;
  first_block = NULL;
  
  first_seg = NULL;
  last_seg = NULL;
  
  triple_id = 0;
}
  
/* hash table for segments */

static htab_t ir_segment_htab = NULL;

static int
ir_segment_entry_eq (const void *p1, const void *p2)
{
  const struct ir_segment_entry *old = p1;
  const struct ir_segment_elem *new = p2;

  return old->e.segdescr == new->segdescr && old->e.segbase == new->segbase
         && old->e.segoffset == new->segoffset && old->e.seglen == new->seglen 
         /* don't check segment align during search, elements of an array
          * may have different align than array itself, but they belong
          * to the same segment
          * && old->e.segalign == new->segalign */ 
         && strcmp (old->e.segname, new->segname) == 0;
}

static hashval_t
ir_segment_entry_hash (const void *p)
{
  const struct ir_segment_entry *old = p;
  return htab_hash_string (old->e.segname);
}

static void
free_ir_segment (void * p)
{
  struct ir_segment_entry *entry = (struct ir_segment_entry *) p;

  free ((void*)entry->e.segname);
  free (entry);
}

void
init_ir_segment_htab_once (void)
{
  ir_segment_htab = htab_create (307, ir_segment_entry_hash, ir_segment_entry_eq, free_ir_segment);
}

void 
fini_ir_segment_htab (void)
{
  htab_empty (ir_segment_htab);
  ir_segment_htab = NULL;
}

static SEGMENT *
build_ir_segment (const char *segname, struct segdescr_st *segdescr, 
                  int segbase, IR_OFFSZ segoffset, IR_OFFSZ seglen, int segalign)
{
  SEGMENT * seg;
  struct ir_segment_entry **slot, *entry;
  
  /* find ir_segment_elem in the hash table */
  slot = (struct ir_segment_entry **)
    htab_find_slot (ir_segment_htab, 
                    &(__extension__ (struct ir_segment_elem) {segname, segdescr, segbase, 
                      segoffset, seglen, segalign}), INSERT);

  entry = *slot;

  if (entry)
    return entry->p;

  /* ir_segment_entry not found. create new one and new segment */
  entry = xmalloc (sizeof (*entry));
  *slot = entry;
  entry->e.segname = xstrdup (segname);
  entry->e.segdescr = segdescr;
  entry->e.segbase = segbase;
  entry->e.segoffset = segoffset;
  entry->e.seglen = seglen;
  entry->e.segalign = segalign;
  
  seg = ir_proc_new_segment(irProc);
  if (last_seg == NULL) 
    {
      /*fatal_error ("build_ir_segment: last_seg is null -> init_segments wasn't called"); */
      first_seg = last_seg = seg; 
      ir_proc_set_segment_head (irProc, first_seg);
    } 
  else 
    {
      last_seg->next_seg = seg;
      last_seg = seg;
    }
  seg->name = build_ir_proc_string (segname);
  seg->descr = *segdescr;
  seg->base = segbase;
  seg->segno = ir_proc_num_segments(irProc) - 1;
  seg->offset = segoffset;
  seg->len = seglen;
  /* the biggest supported alignment for IR segment is 16 bytes */
  seg->align = (segalign <= 16 ? segalign : 16);
  entry->p = seg;
  return seg;
}

static
SEGMENT segtab_init[] = {
    { NULL,  {ARG_SEG,STG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, IR_REG_FP, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {BSS_SEG,STG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, NOBASEREG, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {BSS_SEG,STG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, NOBASEREG, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {DATA_SEG,STG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, NOBASEREG, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {AUTO_SEG,STG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, IR_REG_FP, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {HEAP_SEG,STG_SEG,BUILTIN_SEG,EXTSTG_SEG,0,SEGEXT_GLOBAL}, NOBASEREG, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {DREG_SEG,REG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, NOBASEREG, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {FREG_SEG,REG_SEG,BUILTIN_SEG,LCLSTG_SEG,0,SEGEXT_GLOBAL}, NOBASEREG, 0,0,0,0,0,0,0,0,0,0 },
    { NULL,  {0,0,0,0,0,0}, 0, 0,0,0,0,0,0,0,0,0,0 }
};

SEGMENT *segtab[sizeof(segtab_init)/sizeof(SEGMENT)];

void
init_segments(void)
{
  int index;

  static const char *segnames[] = {".ARG_SEG", ".BSS_SMALL", ".BSS_LARGE", ".DATA_SEG",
                                   ".AUTO_SEG", ".HEAP_SEG", ".DREG_SEG", ".FREG_SEG", NULL};
  for (index = 0; segnames[index] != NULL; index++ ) 
    {
      segtab[index] = build_ir_segment (segnames[index], &segtab_init[index].descr,
                                        segtab_init[index].base, 0, 0, 0);
    }
}


/* hash table for leaves */


static htab_t ir_leaf_htab = NULL;

static int
ir_leaf_entry_eq (const void *p1, const void *p2)
{
  const struct ir_leaf_entry *old = p1;
  const struct ir_leaf_elem *new = p2;

  if (old->e.class != new->class) /* old style compare: || old->e.typep != new->typep */
    return 0;
  
  if (old->e.type.tword != new->type.tword)
      /* TODO PCC_ISPTR (old->e.type.tword) && PCC_ISPTR (new->type.tword)) */
    return 0;
      
  if (old->e.type.size != new->type.size 
      || old->e.type.align != new->type.align)
    return 0;
   
  if (old->e.typep != new->typep)
    {
#ifdef NEW_IR_TYPE_COMPARE
      if (old->e.typep == NULL || new->typep == NULL 
          || old->e.typep->tid != new->typep->tid 
          || old->e.typep->align != new->typep->align)
        return 0;
      
      if (!is_ir_type_node_eq (old->e.typep, new->typep))
#endif
        return 0;
    }

  if (new->class == VAR_LEAF || new->class == ADDR_CONST_LEAF) 
    {
      const ir_ADDRESS * old_addr = &old->e.location.addr;
      const ir_ADDRESS * new_addr = &new->location.addr;

      if (old_addr->labelno != new_addr->labelno
          || old_addr->seg != new_addr->seg
          || old_addr->offset != new_addr->offset)
         return 0;
    }
  else
    {
      const struct constant *old_cnst = &old->e.location.cnst;
      const struct constant *new_cnst = &new->location.cnst;
      
      if (old_cnst->isbinary != new_cnst->isbinary)
        return 0;

      if (IR_ISINT (new->type.tword)) 
        {
          if (old_cnst->c.i != new_cnst->c.i)
            return 0;
        }
      else if (IR_ISCHAR (new->type.tword) || IR_ISPTRFTN (new->type.tword)) 
        {
          if (strcmp (old_cnst->c.cp, new_cnst->c.cp) != 0) 
            return 0;
        } 
      else if (PCC_ISFLOATING (new->type.tword) || PCC_ISIMAGINARY (new->type.tword)) 
        {
          if (new_cnst->isbinary)
            {
              if (memcmp (old_cnst->c.fp[0], new_cnst->c.fp[0], new->type.size) != 0) 
                return 0;
            }
          else
            {
              fatal_error ("non binary floating point constant");
            }
        }
      else if (PCC_ISCOMPLEX (new->type.tword)) 
        {
          if (new_cnst->isbinary)
            {
              if (memcmp (old_cnst->c.fp[0], new_cnst->c.fp[0], new->type.size) != 0
                  || memcmp (old_cnst->c.fp[1], new_cnst->c.fp[1], new->type.size) != 0)
                return 0;
            }
          else
            {
              fatal_error ("non binary complex constant");
            }
        }
      else
        fatal_error ("unrecongnizable leaf");
    }
  return 1;
}

static hashval_t
ir_leaf_entry_hash (const void *p)
{
  const struct ir_leaf_entry *old = p;
  hashval_t hash;

  hash = 0;
  if (old->e.class == VAR_LEAF || old->e.class == ADDR_CONST_LEAF)  
    {
      const ir_ADDRESS *addr = &old->e.location.addr;
      SEGMENT *seg = addr->seg;
      if (seg->descr.builtin == BUILTIN_SEG) 
        {
          hash = ((hashval_t) seg->descr.class << 16) | addr->offset;
        } 
      else 
        {
          hash = htab_hash_string (seg->name);
        }
    } 
  else 
    {
      const struct constant *cnst = &old->e.location.cnst;
      if (IR_ISINT (old->e.type.tword))
      	hash = (hashval_t) cnst->c.i; 
      else
      	hash = (hashval_t) cnst->c.cp;
    }
  return hash;
}

void
init_ir_leaf_htab_once (void)
{
  ir_leaf_htab = htab_create (1021, ir_leaf_entry_hash, ir_leaf_entry_eq, free);
}

void 
fini_ir_leaf_htab (void)
{
  htab_empty (ir_leaf_htab);
  ir_leaf_htab = NULL;
}

LEAF *
build_ir_leaf (LEAF_CLASS class, TYPE type, IR_TYPE_NODE *typep, LEAF_VALUE *location, int insert)
{
  LEAF *leafp;

  struct ir_leaf_entry **slot, *entry;
  
  /* find ir_leaf_elem in the hash table */
  slot = (struct ir_leaf_entry **)
    htab_find_slot (ir_leaf_htab, 
                    &(__extension__ (struct ir_leaf_elem) {class, type, typep, *location}), 
                    insert);

  entry = *slot;

  if (entry)
    return entry->p;
  
  if (!insert)
    return NULL;

  /* ir_leaf_entry not found. create new one and new leaf */
  leafp = ir_proc_new_leaf(irProc);
  leafp->tag = ISLEAF;
  leafp->leafno = ir_proc_num_leaves(irProc) - 1;
  if (class == VAR_LEAF && PCC_ISPTR (type.tword))
    leafp->pointerno = npointers++;
  else
    leafp->pointerno = -1;
  leafp->elvarno = -1;
  leafp->next_leaf = NULL;
  leafp->visited = IR_TRUE;
  leafp->type = type;
  leafp->typep = typep;
  leafp->class = class;
  if (last_leaf == NULL) 
    {
      first_leaf = last_leaf = leafp;
      ir_proc_set_leaf_head (irProc, first_leaf);
    } 
  else 
    {
      last_leaf->next_leaf = leafp;
      last_leaf = leafp;
    }
  
  if (leafp->class == VAR_LEAF || leafp->class == ADDR_CONST_LEAF) 
    {
      leafp->val.addr = *(ir_ADDRESS*)location; /* copy only part of the union */
      if (leafp->class == VAR_LEAF)
        {
          LIST *new_l;
          new_l = build_ir_internal_list();
          new_l->datap = (union list_u *) leafp;
          LAPPEND (leafp->val.addr.seg->leaves, new_l);

          if (packed_field_ir_type_node (typep)
              && 1u<<typep->align < type.align) 
            {
              leafp->type.align = 1u<<typep->align;
            }
        }
    }
  else
    leafp->val.cnst = *(struct constant *)location; /* copy only part of the union */
  
  entry = xmalloc (sizeof (*entry));
  *slot = entry;
  entry->e.class = class;
  entry->e.type = type;
  entry->e.typep = typep;
  entry->e.location = *location;
  entry->p = leafp;

  return leafp;
}

/* build_ir_*_var() functions:
   These are used to create IR LEAFs for variables during IR generation. 
   build_ir_extern_var() differs from others in this class of functions, 
   in that it takes a VAR_DECL as parameter instead of the "const char *" 
   name of the variable to enable generation of debug information 
   for globals during IR generation. In SUNIR globals are associated 
   with a procedure scope, whereas in GCC world globals are associated 
   with a translation unit. Therefore the easiest way of figuring out 
   which globals are used in a given IR procedure is at IR generation point.
   
   get_ir_*_var_leaf () functions:
   These are used during debug information generation to retrieve the 
   IR LEAF generated for a variable. NULL is returned if no such LEAF
   exists.
*/
IR_NODE *
build_ir_reg_var (const char *name, int regno, TYPE type, IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf;

  memset (&addr, 0, sizeof(addr));
  
  if (regno <= IR_REG_I7) 
    {
      addr.seg = segtab[DREG_SEGNO];
      addr.offset = regno - IR_REG_G0;
    }
  else
    {
      addr.seg = segtab[FREG_SEGNO];
      addr.offset = regno - IR_REG_F0;
    }
  
  leaf =  build_ir_leaf (VAR_LEAF, type, typep, (LEAF_VALUE *)&addr, IR_TRUE);
  leaf->pass1_id = build_ir_proc_string (name);

  return (IR_NODE*)leaf;
}

/* Retrieve previously created VAR_LEAF of register variable.  */
LEAF *
get_ir_reg_var_leaf (const char *name ATTRIBUTE_UNUSED, int regno, TYPE type, IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf = NULL;
  struct ir_leaf_entry *leaf_entry;
  
  memset (&addr, 0, sizeof(addr));
  
  if (regno <= IR_REG_I7) 
    {
      addr.seg = segtab[DREG_SEGNO];
      addr.offset = regno - IR_REG_G0;
    }
  else
    {
      addr.seg = segtab[FREG_SEGNO];
      addr.offset = regno - IR_REG_F0;
    }
  
  leaf_entry = (struct ir_leaf_entry *)
    htab_find (ir_leaf_htab, 
               &(__extension__ (struct ir_leaf_elem) {VAR_LEAF, type, typep, (LEAF_VALUE) addr}));
  if (leaf_entry != NULL)
    leaf = leaf_entry->p;
  
  return leaf;
}


IR_NODE *
build_ir_auto_var (const char *name, IR_OFFSZ segoffset, IR_OFFSZ seglen, int segalign, 
                   IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf;
  char lname[40];

  memset (&addr, 0, sizeof (addr));
  sprintf (lname, "AUTO_SEG.%lld.%lld", (long long) segoffset, (long long) seglen);
   
  addr.seg = build_ir_segment (lname, &auto_descr, IR_REG_FP, segoffset, seglen, segalign);
  
  addr.offset = offset;
  leaf = build_ir_leaf (VAR_LEAF, type, typep, (LEAF_VALUE *)&addr, IR_TRUE);
  leaf->pass1_id = build_ir_proc_string (name);
  
  return (IR_NODE*)leaf;
}

/* Helper for the various get_ir_*_var_leaf() (exceptions listed below). 
   Returns the VAR_LEAF corresponding to the given segment descriptor.
   Not used for : get_ir_reg_var_leaf()
*/
static LEAF *
get_ir_var_leaf (struct ir_segment_elem *ir_seg_elem, IR_OFFSZ offset, TYPE type, 
                 IR_TYPE_NODE *typep)
{
  LEAF *leaf = NULL;
  ir_ADDRESS addr;
  struct ir_segment_entry *seg_entry;
  struct ir_leaf_entry *leaf_entry;
  
  /* Return NULL if either IR segment hashtable or IR leaf hash is 
   empty. This could happen when looking up unused global variables.  */
  if ((ir_segment_htab == NULL) || (ir_leaf_htab == NULL))
    return NULL;

  memset (&addr, 0, sizeof (addr));
  seg_entry = (struct ir_segment_entry *) htab_find (ir_segment_htab, ir_seg_elem);
  if (seg_entry != NULL) 
    {
      addr.seg = seg_entry->p;
      addr.offset = offset;
      leaf_entry = (struct ir_leaf_entry *) 
        htab_find (ir_leaf_htab, 
                   &(__extension__ (struct ir_leaf_elem) {VAR_LEAF, type, typep, 
                     (LEAF_VALUE) addr}));
      if (leaf_entry != NULL)
        leaf = leaf_entry->p;
    }
  
  return leaf;
  
}

/* Retrieve previously created VAR_LEAF of auto variable, if not found
 return NULL.  */
LEAF *
get_ir_auto_var_leaf (const char *name ATTRIBUTE_UNUSED, IR_OFFSZ segoffset, IR_OFFSZ seglen, 
                      int segalign, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  char lname[40];
  __extension__ struct ir_segment_elem ir_seg_elem = {lname, &auto_descr, IR_REG_FP, 
                                                      segoffset, seglen, segalign};
  
  sprintf (lname, "AUTO_SEG.%lld.%lld", (long long) segoffset, (long long) seglen);
  return get_ir_var_leaf (&ir_seg_elem, offset, type, typep);
}


IR_NODE *
build_ir_parm_var (const char *name, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf;

  if (TARGET_ARCH64) /* TODO XXX V9 */
    {
      return build_ir_auto_var (name, offset, type.size, type.align, 0, type, typep);
    }
  
  memset (&addr, 0, sizeof (addr));
  addr.seg = segtab[ARG_SEGNO];
  addr.offset = offset;
  leaf =  build_ir_leaf (VAR_LEAF, type, typep, (LEAF_VALUE *)&addr, IR_TRUE);
  leaf->pass1_id = build_ir_proc_string (name);
  leaf->must_store_arg = IR_TRUE;
  
  return (IR_NODE*)leaf;
}

/* Retrieve previously created VAR_LEAF of parm variable, if none exists
   return NULL.  */
LEAF *
get_ir_parm_var_leaf (const char *name, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  LEAF *leaf = NULL;
  ir_ADDRESS addr;
  struct ir_leaf_entry *leaf_entry;
  
  memset (&addr, 0, sizeof (addr));
  if (TARGET_ARCH64)
    {
      leaf = get_ir_auto_var_leaf (name, offset, type.size, type.align,
                                   0, type, typep);
    }
  else 
    {
      addr.seg = segtab[ARG_SEGNO];
      addr.offset = offset;
      leaf_entry = (struct ir_leaf_entry *) 
        htab_find (ir_leaf_htab, 
                   &(__extension__ (struct ir_leaf_elem) {VAR_LEAF, type, typep, 
                     (LEAF_VALUE) addr}));
      if (leaf_entry != NULL)
        leaf = leaf_entry->p;
    }
  
  return leaf;
}




IR_NODE *
build_ir_string_var (const char *label, TYPE type, IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf;

  memset (&addr, 0, sizeof (addr));

  /* anonymous local static */
  addr.seg = build_ir_segment (label, &lstatic_descr, NOBASEREG, 0, 0, 0);
  addr.offset = 0;
  leaf = build_ir_leaf (VAR_LEAF, type, typep, (LEAF_VALUE *)&addr, IR_TRUE);
  leaf->pass1_id = build_ir_proc_string ("");
  
  return (IR_NODE*)leaf;
}

IR_NODE *
build_ir_static_var (const char *name, int labelno, IR_OFFSZ offset, TYPE type, 
                     IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf;
  char lnamebuf[200];
  char *lname = lnamebuf;

  memset (&addr, 0, sizeof (addr));
  sprintf (lnamebuf, ".L%d", labelno);
  
  if (globalize_flag)
    lname = make_global_name (lname, 1, current_function_decl);

  if (name[0] == '\0') 
    {
      /* anonymous local static */
      addr.seg = build_ir_segment (lname, &lstatic_descr, NOBASEREG, 0, 0, 0);
    } 
  else 
    {
      /* named local static: map as external */
      addr.seg = build_ir_segment (lname, &extern_descr, NOBASEREG, 0, 0, 0);
    }
  addr.offset = offset;
  leaf = build_ir_leaf (VAR_LEAF, type, typep, (LEAF_VALUE *)&addr, IR_TRUE);
  leaf->pass1_id = build_ir_proc_string (name);
  
  return (IR_NODE*)leaf;
}

/* Unlike other build_ir_*_var() functions, build_ir_extern_var() is 
   called with a VAR_DECL parameter instead of the "const char *" name 
   of the variable to enable generation of debug information for globals.
*/
IR_NODE *
build_ir_extern_var (tree decl, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  ir_ADDRESS addr;
  LEAF *leaf;
  const char *name = get_ir_name (decl);
  
  memset (&addr, 0, sizeof (addr));
  addr.seg = build_ir_segment (name, &extern_descr, NOBASEREG, 0, 0, 0);

  /* 6721968: iropt has a phase to replace ifetch(addr) by constant if the 
     source of ifetch is a read-only segment. */
  if (TREE_READONLY (decl))
    addr.seg->readonly = 1;

  addr.offset = offset;
  leaf = build_ir_leaf (VAR_LEAF, type, typep, (LEAF_VALUE *)&addr, IR_TRUE);
  leaf->pass1_id = build_ir_proc_string (name);
  if (decl && TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
    leaf->is_unshared = TRUE;
  dbg_gen_global_var_decl (decl);

  return (IR_NODE*)leaf;
}

/* Retrieve previously created VAR_LEAF for global/file static variable.
   Return NULL if none exists.  */
LEAF *
get_ir_extern_var_leaf (const char *name, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  __extension__ struct ir_segment_elem ir_seg_elem = {name, &extern_descr, NOBASEREG, 0, 0, 0};

  return get_ir_var_leaf (&ir_seg_elem, offset, type, typep);
}

IR_NODE *
build_ir_funcname (const char *name, TYPE type, IR_TYPE_NODE *typep)
{
  struct constant cnst;

  memset (&cnst, 0, sizeof (cnst));
  cnst.isbinary = IR_FALSE;
  cnst.c.cp = build_ir_proc_string(name);

  return (IR_NODE*)build_ir_leaf (CONST_LEAF, type, typep, (LEAF_VALUE *)&cnst, IR_TRUE);
}

IR_NODE *
build_ir_addr_const (IR_NODE *np, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep)
{
  LEAF *leafp;
  ir_ADDRESS addr;

  if (np->operand.tag != ISLEAF)
    fatal_error ("build_ir_addr_const: operand must be leaf node");
  
  addr = np->leaf.val.addr;
  addr.offset += offset;
  
  leafp = build_ir_leaf (ADDR_CONST_LEAF, type, typep, (LEAF_VALUE*)&addr, IR_TRUE);
  leafp->pass1_id = np->leaf.pass1_id;
  leafp->addressed_leaf = (LEAF*)np;
  leafp->is_unshared = leafp->addressed_leaf->is_unshared;
  leafp->in_taskcommon_block = leafp->addressed_leaf->in_taskcommon_block;
  
  return (IR_NODE*)leafp;
}

/* Make a copy of the 'overlap' lists for a leaf of class VAR_LEAF.  
 * Used to construct the 'can_access' list for asm_address triples.
 */
static LIST *
ir_copy_leaf_list (LIST *tail)
{
  LIST *lp1, *lp2, *last;

  last = LNULL;
  LFOR (lp1,tail) 
    {
      lp2 = build_ir_proc_list ();
      lp2->datap = lp1->datap;
      LAPPEND (last,lp2);
    }

  if (last != LNULL)
    return last->next;
  else
    return last;
}

LIST *
ir_copy_overlaps (LEAF *leafp)
{
  LIST *newlistp, *overlap;

  if (leafp == NULL)
    return NULL;
  
  newlistp = build_ir_proc_list ();
  newlistp->datap = (union list_u *)leafp;
  
  if (leafp->overlap != NULL) 
    {
      overlap = ir_copy_leaf_list (leafp->overlap);
      LAPPEND (newlistp, overlap);
    }

  return newlistp;
}

IR_NODE *
build_ir_float_const (const REAL_VALUE_TYPE *val, TYPE type)
{
  struct constant cnst;

  memset (&cnst, 0, sizeof(cnst));
  cnst.isbinary = IR_TRUE;

  if (type.tword == PCC_FLOAT || type.tword == PCC_FLOAT_IMAGINARY) 
    {
      long n;
      /* gcc_assert (sizeof (long) == type.size); */
      REAL_VALUE_TO_TARGET_SINGLE (*val, n);
      cnst.c.fp[0] = build_ir_byte_array (&n, type.size);
    } 
  else if (type.tword == PCC_DOUBLE || type.tword == PCC_DOUBLE_IMAGINARY) 
    {
      long lval[2], tmp;
      /* gcc_assert (sizeof (long) * 2 == type.size); */
      REAL_VALUE_TO_TARGET_DOUBLE (*val, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[1];
      lval[1] = lval[0];
      lval[0] = tmp;
#endif
      cnst.c.fp[0] = build_ir_byte_array (lval, type.size);
    } 
  else 
    {
      long lval[4], tmp;
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*val, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[0];
      lval[0] = lval[3];
      lval[3] = tmp;
      tmp = lval[1];
      lval[1] = lval[2];
      lval[2] = tmp;
#endif
      cnst.c.fp[0] = build_ir_byte_array (lval, type.size);
    }

  return (IR_NODE*)build_ir_leaf (CONST_LEAF, type, NULL, (LEAF_VALUE *)&cnst, IR_TRUE);
}


IR_NODE *
build_ir_int_complex_const (long long real_val, long long imag_val, TYPE type)
{
  struct constant cnst;

  memset (&cnst, 0, sizeof (cnst));
  cnst.isbinary = IR_TRUE;

  if (type.tword == PCC_FLOAT_COMPLEX || 
      type.tword == PCC_DOUBLE_COMPLEX) 
    {
	fatal_error ("expect int complex type");
    } 
  else 
    {
      long lval[4], tmp;
      REAL_VALUE_TYPE ld_value;
      /* gcc_assert (sizeof (long) * 4 * 2 >= type.size); */
      /* assume that target long double can hold all size of __complex__ int */
      REAL_VALUE_FROM_INT(ld_value, real_val, (real_val<0)?(~0L):0L, VOIDmode);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (ld_value, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[0];
      lval[0] = lval[3];
      lval[3] = tmp;
      tmp = lval[1];
      lval[1] = lval[2];
      lval[2] = tmp;
#endif
      cnst.c.fp[0] = build_ir_byte_array (lval, type.size/2);
      REAL_VALUE_FROM_INT(ld_value, imag_val, (imag_val<0)?(~0L):0L, VOIDmode);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (ld_value, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[0];
      lval[0] = lval[3];
      lval[3] = tmp;
      tmp = lval[1];
      lval[1] = lval[2];
      lval[2] = tmp;
#endif
      cnst.c.fp[1] = build_ir_byte_array (lval, type.size/2);
    }

  return (IR_NODE*)build_ir_leaf (CONST_LEAF, type, NULL, (LEAF_VALUE *)&cnst, IR_TRUE);
}

IR_NODE *
build_ir_complex_const (const REAL_VALUE_TYPE *real_val, 
                        const REAL_VALUE_TYPE *imag_val, TYPE type)
{
  struct constant cnst;

  memset (&cnst, 0, sizeof (cnst));
  cnst.isbinary = IR_TRUE;

  if (type.tword == PCC_FLOAT_COMPLEX) 
    {
      long n;
      /* gcc_assert (sizeof (long) * 2 == type.size); */
      REAL_VALUE_TO_TARGET_SINGLE (*real_val, n);
      cnst.c.fp[0] = build_ir_byte_array (&n, type.size/2);
      REAL_VALUE_TO_TARGET_SINGLE (*imag_val, n);
      cnst.c.fp[1] = build_ir_byte_array (&n, type.size/2);
    } 
  else if (type.tword == PCC_DOUBLE_COMPLEX) 
    {
      long lval[2], tmp;
      /* gcc_assert (sizeof (long) * 2 * 2 == type.size); */
      REAL_VALUE_TO_TARGET_DOUBLE (*real_val, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[1];
      lval[1] = lval[0];
      lval[0] = tmp;
#endif
      cnst.c.fp[0] = build_ir_byte_array (lval, type.size/2);
      REAL_VALUE_TO_TARGET_DOUBLE (*imag_val, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[1];
      lval[1] = lval[0];
      lval[0] = tmp;
#endif
      cnst.c.fp[1] = build_ir_byte_array (lval, type.size/2);
    } 
  else 
    {
      long lval[4], tmp;
      /* gcc_assert (sizeof (long) * 4 * 2 == type.size); */
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*real_val, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[0];
      lval[0] = lval[3];
      lval[3] = tmp;
      tmp = lval[1];
      lval[1] = lval[2];
      lval[2] = tmp;
#endif
      cnst.c.fp[0] = build_ir_byte_array (lval, type.size/2);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*imag_val, lval);
#ifdef CROSS_DIRECTORY_STRUCTURE
      tmp = lval[0];
      lval[0] = lval[3];
      lval[3] = tmp;
      tmp = lval[1];
      lval[1] = lval[2];
      lval[2] = tmp;
#endif
      cnst.c.fp[1] = build_ir_byte_array (lval, type.size/2);
    }

  return (IR_NODE*)build_ir_leaf (CONST_LEAF, type, NULL, (LEAF_VALUE *)&cnst, IR_TRUE);
}

IR_NODE *
build_ir_int_const (CONSZ i, TYPE t, IR_TYPE_NODE *triple_ir_type)
{
  struct constant cnst;
  TWORD tword;
  IR_NODE *np;

  memset (&cnst, 0, sizeof (cnst));
  tword = t.tword;
  
  if (PCC_ISCHAR (tword)) 
    {
      np = build_ir_char_const ((char)i, t);
      np->operand.type = t;
      return np;
    }
  
  if (PCC_ISPTR (tword))
    /* i is the value of a no-name pointer constant */
    t.tword = PCC_LONG;

  cnst.isbinary = IR_FALSE;
  cnst.c.i = i;
  np = (IR_NODE*)build_ir_leaf (CONST_LEAF, t, NULL, (LEAF_VALUE *)&cnst, IR_TRUE);
  
  if (PCC_ISPTR (tword)) 
    {
      t.tword = tword;
      np = (IR_NODE*)build_ir_triple (IR_CONV, np, NULL, t, triple_ir_type);
    }
  return np;
}

IR_NODE *
build_ir_char_const (char c, TYPE t)
{
  struct constant cnst;
  char s[2];
  s[0] = c;
  s[1] = '\0'; /* chars have to be represented as strings */
  memset (&cnst, 0, sizeof (cnst));
  cnst.isbinary = IR_FALSE;
  cnst.c.cp = build_ir_proc_string (s);

  return (IR_NODE*) build_ir_leaf (CONST_LEAF, t, NULL, (LEAF_VALUE *)&cnst, IR_TRUE);
}

IR_NODE *
build_ir_string_const (const char *s)
{
  struct constant cnst;
  TYPE t;

  memset (&cnst, 0, sizeof (cnst));
  t.tword = IR_STRING;
  t.size = strlen (s);
  t.align = sizeof (char);

  cnst.isbinary = IR_FALSE;
  cnst.c.cp = build_ir_proc_string (s);
  
  return (IR_NODE*) build_ir_leaf (CONST_LEAF, t, NULL, (LEAF_VALUE *)&cnst, IR_TRUE);
}

IR_NODE *
build_ir_labeldef (CONSZ l)
{
  return build_ir_triple (IR_LABELDEF, build_ir_int_const (l, inttype, 0), NULL, longtype, NULL);
}

IR_NODE *
build_ir_labelref (CONSZ l1, CONSZ l2)
{
  return build_ir_triple (IR_LABELREF, build_ir_int_const (l1, inttype, 0), 
                          build_ir_int_const (l2, inttype, 0), longtype, NULL);
}

IR_NODE *
build_ir_labelref_with_type (CONSZ l1, CONSZ l2, TYPE t)
{
  TYPE constval_type = !PCC_ISCHAR (t.tword) && !PCC_ISPTR (t.tword) ? t : longtype;
  
  return build_ir_triple (IR_LABELREF, build_ir_int_const (l1, inttype, 0)/* label */, 
                          build_ir_int_const (l2, constval_type, 0)/* case # or condition */, 
                          longtype, NULL);
}

void
build_ir_goto (CONSZ l)
{
  IR_NODE *lab = build_ir_labelref (l, 0);

  build_ir_triple (IR_GOTO, NULL, lab, longtype, NULL);
}

ir_eh_node_hdl_t
build_ir_eh_node (ir_eh_node_kind_t kind)
{
  ir_eh_node_hdl_t eh_node = ir_proc_new_eh_node(irProc, kind);
  ir_eh_node_set_id(eh_node, ehnode_count++);
  return eh_node;
}
