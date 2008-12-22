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
#undef ENABLE_TREE_CHECKING
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
#include "tree-ir.h"

int default_alias_level = ALIAS_BASIC;

TYPE labelno_TYPE = {IR_LABELNO, 4, 4};

#define IR_TYPE_ALIGN(node) (MIN (TYPE_ALIGN (node), 128))

TWORD 
map_gnu_type_to_tword (tree node)
{
  TWORD ret;
  
  if (TYPE_IR_TWORD (node)) return TYPE_IR_TWORD (node);
  
  switch (TREE_CODE (node))
    {
    case VOID_TYPE:
      ret = PCC_TVOID;
      break;
      
    case ENUMERAL_TYPE:
    case INTEGER_TYPE:
    handle_integer:
        /* no performance benefit of using 'long' instead of 'int' in 32-bit mode,
         * but need to distinguish 'long' and 'long long' in 64-bit */
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
	  {
	    name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
          }
        else
          {
            type = TREE_TYPE (type);
            if (type && TYPE_NAME (type) != 0
                && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
                && DECL_NAME (TYPE_NAME (type)) != 0
                && TREE_CODE (DECL_NAME (TYPE_NAME (type))) == IDENTIFIER_NODE)
              {
                name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type)));
              }
          }

        if (name)
          {
            if (!strcmp (name, "long int"))
              {
                ret = PCC_LONG;
                break;
              }
            
            if (!strcmp (name, "long unsigned int"))
              {
                ret = PCC_ULONG;
                break;
              }
          }
      }
      switch (TYPE_MODE (node))
        {
        case QImode: 
          ret = (TYPE_UNSIGNED (node) ? PCC_UCHAR : PCC_CHAR);
          break;
        case HImode: 
          ret = (TYPE_UNSIGNED (node) ? PCC_USHORT : PCC_SHORT);
          break;
        case SImode: 
          ret = (TYPE_UNSIGNED (node) ? PCC_UNSIGNED : PCC_INT);
          break;
        case DImode: 
          ret = (TYPE_UNSIGNED (node) ? PCC_ULLONG : PCC_LLONG);
          break;
        case TImode: /* 128 bit integer type */ 
          if (TARGET_ARCH64) 
            { 
	      /* workaround for v9, cause bit_size_type has a 
		 precision of 68 bits */
              if (node == bitsizetype)
                ret = PCC_ULONG;
              else if (TYPE_PRECISION (node) == 128)
		ret = 0; /* zero TYPE_IR_TWORD (node). Don't cache it */
              else /* for others let's hope that llong will fit */
                ret = (TYPE_UNSIGNED (node) ? PCC_ULLONG : PCC_LLONG);
            }
          else
	    abort();
          break;
        default: abort ();
        }
      break;
    
    case BOOLEAN_TYPE:
      /*if (TARGET_ARCH64)
        ret = PCC_INT; // no _Bool in v9 XXX
      else*/
      if (ir_language == FORTRAN)
        goto handle_integer;
      if (ir_language == CDOUBLEPLUS)
        ret = PCC_UCHAR; /* hack for c++ */
      else
        ret = PCC_BOOL;
      break;
      
    case ARRAY_TYPE:
      ret = PCC_ADDTYPE (map_gnu_type_to_tword (TREE_TYPE (node)), PCC_ARY);
      break;
      
    case REAL_TYPE:
      switch (TYPE_MODE (node))
        {
        case SFmode: 
          ret = PCC_FLOAT;
          break;
        case DFmode: 
          ret = PCC_DOUBLE;
          break;
        case TFmode: 
          ret = PCC_LDOUBLE;
          break;
        default: abort ();
        }
      break;

    case COMPLEX_TYPE:
      switch (TYPE_MODE (node))
        {
        case CQImode:
        case CHImode:
        case CSImode: 
          ret = PCC_FLOAT_COMPLEX; /*int complex*/
          break;
        case CDImode: 
          ret = PCC_DOUBLE_COMPLEX; /*long long complex*/
          break;
        case SCmode: 
          ret = PCC_FLOAT_COMPLEX; /*float complex*/
          break;
        case DCmode: 
          ret = PCC_DOUBLE_COMPLEX; /*double complex*/
          break;
        case TCmode: 
          ret = PCC_LDOUBLE_COMPLEX; /*long double complex*/
          break;
        default: abort ();
        }
      break;
      
    case VECTOR_TYPE:
      {
        int size;
        if (!TYPE_SIZE (node) || TREE_CODE (TYPE_SIZE (node)) != INTEGER_CST
            || !TYPE_SIZE_UNIT (node) || TREE_CODE (TYPE_SIZE_UNIT (node)) != INTEGER_CST)
          abort ();
        
        size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (node));
        if (size <= 4)
          ret = PCC_FLOAT;
        else if (size <= 8)
          {
            if (TREE_CODE (TREE_TYPE (node)) == REAL_TYPE)
              /* vector ABI says "pass all vector floats as structures" */
              ret = PCC_FLOAT_COMPLEX; 
            else
              ret = PCC_DOUBLE;  /* vector of ints */
          }
        else if (size <= 16)
          ret = PCC_DOUBLE_COMPLEX;
        else if (size <= 32)
          ret = PCC_LDOUBLE_COMPLEX;
        else
          ret = PCC_ADDTYPE (map_gnu_type_to_tword (TREE_TYPE (node)), PCC_ARY);
        break;
      }

    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
      ret = PCC_ADDTYPE (map_gnu_type_to_tword (TREE_TYPE (node)), PCC_PTR);
      break;
    
    case METHOD_TYPE:
    case FUNCTION_TYPE:
      ret = PCC_ADDTYPE (map_gnu_type_to_tword (TREE_TYPE (node)), PCC_FTN);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      ret = PCC_STRTY;
      break;
      
    case LANG_TYPE:
      ret = 0;
      break;
      
    default:
      debug_tree (node);
      abort();
    }

  TYPE_IR_TWORD (node) = ret; /* set type for future quick reference */
  return ret;
}

long long
get_type_size (tree node)
{
  if (TYPE_SIZE (node) && TREE_CODE (TYPE_SIZE (node)) == INTEGER_CST)
    return TREE_INT_CST_LOW (TYPE_SIZE (node)) / 8;
  else
    return 0;
}
    
TYPE 
map_gnu_type_to_TYPE (tree node)
{
  TYPE t;
  
  t.tword = map_gnu_type_to_tword (node);
  
  t.size = get_type_size (node);
  
  t.align = IR_TYPE_ALIGN (node) / 8;
  
  return t;
}

/* get array lower bound */
long long
get_array_lower_bound (tree type)
{
  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);

  if (ir_language == FORTRAN)
    {
      tree domain = TYPE_DOMAIN (type);
      if (domain)
        {
          tree min = TYPE_MIN_VALUE (domain);
          if (min && host_integerp (min, 0))
            return TREE_INT_CST_LOW (min);
        }

      debug_tree (type);
      abort ();
    }
  else
    return 0;
}

/* get size of the array with given type */
long long
get_array_type_size (tree type)
{
  tree domain;
  
  gcc_assert (TREE_CODE (type) == ARRAY_TYPE);
  
  domain = TYPE_DOMAIN (type);
  
  if (domain)
    {
      tree min = TYPE_MIN_VALUE (domain);
      tree max = TYPE_MAX_VALUE (domain);
      if (min && max
          && host_integerp (max, 0))
        {
          if (integer_zerop (min)) /* always true for C and C++ */
            return TREE_INT_CST_LOW (max) + 1;
          else if (host_integerp (min, 0)) /* fortran */
            return TREE_INT_CST_LOW (max) - TREE_INT_CST_LOW (min) + 1;
          else 
            return 0;
        }
      else
        return 0;
    }
  else
    return 0;
 /* old way:
  *  TREE_INT_CST_LOW (TYPE_SIZE (type)) /
  *  TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (type)));
  */
}

static TWORD 
map_gnu_bitfield_type_to_tword (tree node)
{
  TWORD ret = 0;
  
  switch (DECL_MODE (node)) /* DECL_MODE here, not TYPE_MODE */
    {
    case QImode: 
      ret = (DECL_UNSIGNED (node) ? PCC_UCHAR : PCC_CHAR);
      break;
    case HImode: 
      ret = (DECL_UNSIGNED (node) ? PCC_USHORT : PCC_SHORT);
      break;
    case SImode: 
      ret = (DECL_UNSIGNED (node) ? PCC_UNSIGNED : PCC_INT);
      break;
    case DImode: 
      ret = (DECL_UNSIGNED (node) ? PCC_ULLONG : PCC_LLONG);
      break;
    default: abort ();
    }
  return ret;
}

TYPE
map_gnu_bitfield_type_to_TYPE (tree node)
{
  TYPE t;
  int i = 0;
  
  t.tword = map_gnu_bitfield_type_to_tword (node);
  
  switch (DECL_MODE (node)) /* DECL_MODE here, not TYPE_MODE */
    {
    case QImode: i = 1; break;
    case HImode: i = 2; break;
    case SImode: i = 4; break;
    case DImode: i = 8; break;
    default: abort ();
    }
  
  t.size = i;
  t.align = i;
  
  return t;
}

#if 0
static void set_ir_type_alias_level (IR_TYPE_NODE *ir_type, ALIAS_LEVEL level);

/*
 * set_ir_type_alias_child takes a list of children ir types and an alias level
 * and sets the alias level for each of the child types by recursively calling
 * set_ir_type_alias_level.
 */
static void
set_ir_type_alias_level_child(LIST *head, ALIAS_LEVEL level)
{
    LIST  *child;

    LFOR(child, head) {
        set_ir_type_alias_level(LCAST(child, IR_TYPE_NODE), level);
    }
}

/*
 * set_ir_type_alias_level takes an ir_type and alias level and sets
 * the alias_level for the type and all its associated type subtrees.
 */
static void
set_ir_type_alias_level (IR_TYPE_NODE *ir_type, ALIAS_LEVEL level)
{
  TWORD t;

  if (ir_type == NULL)
    return;

  if (ir_type->alias_level == level)
    return;

  ir_type->alias_level = level;
  t = ir_type->tid;
  switch (t) 
    {
    case PCC_STRTY:
    case PCC_UNIONTY:
    case PCC_ENUMTY:
      set_ir_type_alias_level_child (ir_type->t.s.children, level);
      break;

    case IR_ARRAY:
      set_ir_type_alias_level (ir_decref (ir_type), level);
      set_ir_type_alias_level_child (ir_type->t.a.dimensions, level);
      break;

    case IR_FUNCTION:
      set_ir_type_alias_level (ir_decref (ir_type), level);
      break;

    default:
      break;
  }
}
#endif

static int
ir_type_node_align (int bit_align)
{
  int byte_align;
  int log;

  if (bit_align == 0)
	return 0;

  byte_align = bit_align >> 3;

  /* find log base 2 */
  log = 0;
  while ((byte_align & 0x1) == 0) 
    {
      log += 1;
      byte_align >>= 1;
    }

  if (byte_align != 1)
    error ("ir_type_node_align(): alignment must be a power of 2");
  
  return log;
}

struct ir_type_node_elem
{
  tree type;
  IR_TYPE_NODE * parent;
  const char * name;
  bool complete;        /* true if type is complete */
};

struct ir_type_node_entry
{
  struct ir_type_node_elem e;
  IR_TYPE_NODE * p;
};

/*static gTY((param_is (struct ir_type_node_entry))) htab_t ir_type_node_htab;*/
static htab_t ir_type_node_htab;

static int
ir_type_node_entry_eq (const void *p1, const void *p2)
{
  const struct ir_type_node_entry *old = p1;
  const struct ir_type_node_elem *new = p2;

  return old->e.type == new->type 
         && old->e.parent == new->parent
         && ((old->e.name == 0 && new->name == 0) 
             || (old->e.name != 0 && new->name != 0 && strcmp (old->e.name, new->name) == 0));
}

static hashval_t
ir_type_node_entry_hash (const void *p)
{
  const struct ir_type_node_entry *old = p;
  return htab_hash_pointer (old->e.type) | htab_hash_pointer (old->e.parent);
}

void
init_ir_type_node_htab_once (void)
{
  ir_type_node_htab = htab_create (307, ir_type_node_entry_hash, ir_type_node_entry_eq, NULL);
}

const char *
get_type_name (tree node)
{
  if (TYPE_NAME (node))
    {
      if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
         return IDENTIFIER_POINTER (TYPE_NAME (node));
      else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL && DECL_NAME (TYPE_NAME (node)))
        return IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node)));
    }
  else
    {
      switch (TREE_CODE (node))
        {
        case VOID_TYPE:
        case BOOLEAN_TYPE:
        case INTEGER_TYPE:
        case REAL_TYPE:
          node = lang_hooks.types.type_for_mode (TYPE_MODE (node), TYPE_UNSIGNED (node));
          if (TREE_CODE (node) == IDENTIFIER_NODE)
             return IDENTIFIER_POINTER (node);
          else if (TREE_CODE (node) == TYPE_DECL && DECL_NAME (node))
            return IDENTIFIER_POINTER (DECL_NAME (node));
        default:
          break;
        }
    }
  return NULL;
}

#define IR_TYPE_NODE_ALIGN(node) ir_type_node_align (IR_TYPE_ALIGN (node)) 

static IR_TYPE_NODE * 
map_gnu_type_to_IR_TYPE_NODE_internal (tree node, IR_TYPE_NODE * parent, const char * name, 
                                       int packed_align, tree binfo)
{
  IR_TYPE_NODE *ir_type, *ret = 0;
  bool replace_old_entry = 0;
  char *typedef_name = 0;

  struct ir_type_node_entry **slot, *entry;
  const char *tag_name;
  tree type_name = TYPE_NAME (node);

  tree orig_type = NULL;

  /* ignore typedefs */
  if (type_name && TREE_CODE (type_name) == TYPE_DECL
      && DECL_ORIGINAL_TYPE (type_name) && !DECL_ARTIFICIAL (type_name))
    orig_type = DECL_ORIGINAL_TYPE (type_name);

  /* array_ref use nonamed union or struct as type. be careful. */
  if (type_name == NULL 
      && (TREE_CODE (node) == RECORD_TYPE
          || TREE_CODE (node) == UNION_TYPE)
      && TYPE_IR_TAGNODE (node)) 
    orig_type = node;

  if (orig_type)
    { 
       if (TYPE_NAME (orig_type) == NULL 
            && (TREE_CODE (orig_type) == RECORD_TYPE 
                || TREE_CODE (orig_type) == UNION_TYPE))
         {
           node = TYPE_IR_TAGNODE (orig_type);

           /* get tag_name so early just to know the original length. */
           if (ir_language == CDOUBLEPLUS)
             {
               if (TYPE_STUB_DECL (node) == 0)
                tag_name = NULL;
               else
                tag_name = get_ir_name (TYPE_STUB_DECL (node));
             }
           else
             tag_name = get_type_name (node);

           typedef_name = tag_name? alloca (strlen (tag_name) + 2) : 0;
           if (typedef_name)
             {
               typedef_name [0] = '$';
               typedef_name [1] = 0;
             }

           tag_name = NULL;
        }
      else
        {
          return map_gnu_type_to_IR_TYPE_NODE_internal (orig_type, parent, name,
                                                        packed_align, binfo);
        }
    }
  
  if (TYPE_QUALS (node))
    {
      return map_gnu_type_to_IR_TYPE_NODE_internal (TYPE_MAIN_VARIANT (node), parent, name,
                                                    packed_align, binfo);
    }

  /* find ir_type_node_elem in the hash table */
  slot = (struct ir_type_node_entry **)
    htab_find_slot (ir_type_node_htab, 
                    &(__extension__ (struct ir_type_node_elem) {node, parent, name, 0}), INSERT);

  entry = *slot;

  if (entry && TYPE_IR_TYPE_NODE_VALID (node)) {
    /* If the previous entry was created from an incomplete type
       and the current type node is complete, replace the old one. */
    if (!entry->e.complete && !C_TYPE_INCOMPLETE_P (node))
        replace_old_entry = 1;
    else
        return entry->p;
  }
  
#define SAVE_IF_NEEDED \
  {\
    if (!entry || replace_old_entry)\
      {\
        if (entry)\
            free (entry);\
        entry = xmalloc (sizeof (*entry));\
        *slot = entry;\
      }\
    TYPE_IR_TYPE_NODE_VALID (node) = 1;\
    entry->e.complete = ! C_TYPE_INCOMPLETE_P (node);\
    entry->e.type = node;\
    entry->e.parent = parent;\
    entry->e.name = name ? xstrdup (name) : 0;\
    entry->p = ir_type;\
  }

#define ADD_TO_LIST(head, tail, member) \
  if (tail == LNULL) \
    { \
      head = tail = member; \
    } \
  else \
    { \
      tail->next = member; \
      tail = member; \
    }

  switch (TREE_CODE (node)) 
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
	ir_type = build_ir_type (IR_POINTER, IR_TYPE_NODE_ALIGN (node));
	ir_type->parent = parent;
        if (packed_align >= 0) /* make sure child align is less than parent packed align */
          ir_type->align = MIN (ir_type->align, packed_align);
	ir_type->t.p.size = get_type_size (node);
	ir_type->t.p.offset = 0;
        SAVE_IF_NEEDED;
	ir_type->t.p.designator = map_gnu_type_to_IR_TYPE_NODE_internal (TREE_TYPE (node), 
                                                                         0, 0, -1, 0);
        if (flag_strict_aliasing /* -O2 or higher */
            && default_alias_level < ALIAS_C_STD)
          /* set alias_level of pointers to 'standard' 
             which is the 1st level of strictness of -fstrict-aliasing flag */
          ir_type->alias_level = ALIAS_C_STD; 
        else if (TYPE_REF_CAN_ALIAS_ALL (node))
          ir_type->alias_level = ALIAS_ANY;
        else
          ir_type->alias_level = default_alias_level;
	ret = ir_type;
        break;

    case ARRAY_TYPE:
	ir_type = build_ir_type (IR_ARRAY, IR_TYPE_NODE_ALIGN (node));
	ir_type->parent = parent;
        if (packed_align >= 0) /* make sure child align is less than parent packed align */
          ir_type->align = MIN (ir_type->align, packed_align);
	ir_type->t.a.size = get_type_size (node);
	ir_type->t.a.offset = 0;
        ir_type->alias_level = default_alias_level;
        SAVE_IF_NEEDED;
	ir_type->t.a.element_type = map_gnu_type_to_IR_TYPE_NODE_internal (TREE_TYPE (node), 
                                                                           ir_type, 0, 
                                                                           packed_align, 0);
        {
          LIST *dim;
          IR_TYPE_NODE *ir_dim;
          ir_dim = build_ir_type (IR_ARRAYDIM, 2);
          ir_dim->parent = ir_type;
          if (ir_language == FORTRAN)
            {
              ir_dim->t.d.lower = get_array_lower_bound (node);
              ir_dim->t.d.upper = get_array_type_size (node) - 1 + ir_dim->t.d.lower;
            }
          else
            {
              ir_dim->t.d.lower = 0;
              ir_dim->t.d.upper = get_array_type_size (node) - 1;
            }
          dim = build_ir_mod_list();
          dim->next = LNULL;
          dim->datap = (LDATA *) ir_dim;
	  ir_type->t.a.dimensions = dim;
        }
	ret = ir_type;
        break;

    case RECORD_TYPE:
    case UNION_TYPE:
      {
	if (TREE_CODE (node) == RECORD_TYPE) 
          {
	    ir_type = build_ir_type (PCC_STRTY, IR_TYPE_NODE_ALIGN (node));
	  } 
        else 
          {
	    ir_type = build_ir_type (PCC_UNIONTY, IR_TYPE_NODE_ALIGN (node));
	  }
	ir_type->parent = parent;
        if (packed_align >= 0) /* make sure child align is less than parent packed align */
          ir_type->align = MIN (ir_type->align, packed_align);
	ir_type->t.s.size = get_type_size (node);
	ir_type->t.s.offset = 0;
        
        if (ir_language == CDOUBLEPLUS)
          {
            if (TYPE_STUB_DECL (node) == 0)
              /* 'node' is compiler generated structure */
              tag_name = NULL;
            else
              /* get fully mangled C++ type name */
              tag_name = get_ir_name (TYPE_STUB_DECL (node));
          }
        else
          /* get plain type name for C */
          tag_name = get_type_name (node);
        
        if (tag_name == NULL)
          {
            char *name;
            ASM_FORMAT_PRIVATE_NAME (name, "internal", TYPE_UID (node));
            ir_type->t.s.tag_name = build_ir_mod_string (name);
          }
        else 
          {
            char *name = typedef_name? strcat(typedef_name, tag_name):tag_name;
            ir_type->t.s.tag_name = build_ir_mod_string (name);
          }

        if (TYPE_LANG_FLAG_2 (node))
          ir_type->alias_level = ALIAS_ANY;
        else if (flag_strict_aliasing /* -O2 or higher */
            && tag_name && strncmp (tag_name + 2, "__gnu_cxx", 9) == 0)
          /* iterators in libstdc++ satisfy -fstrict-aliasing rules */
          ir_type->alias_level = ALIAS_C_STD;
        else if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (node)))
          ir_type->alias_level = ALIAS_ANY;
        else
          ir_type->alias_level = default_alias_level;
        SAVE_IF_NEEDED;
        if (ir_language == CDOUBLEPLUS)
          {
            if (binfo == NULL) /* nothing has been passed from parent */
              binfo = TYPE_BINFO (node); /* so we are the root class */
          }
        {
          LIST *member;
          LIST *head = LNULL;
          LIST *tail = LNULL;
          IR_TYPE_NODE *ir_mbr = 0;
          tree tmp = TYPE_FIELDS (node);
          while (tmp)
            { 
              if (TREE_CODE (tmp) == FIELD_DECL)
                {
                  const char * field_name;
                  char fld_name_buf[64];
                  long long size;
                  long long offset;
                  
                  
                  if (DECL_SIZE (tmp) && TREE_CODE (DECL_SIZE (tmp)) == INTEGER_CST) 
                    size = TREE_INT_CST_LOW (DECL_SIZE (tmp));
                  else
                    size = get_type_size (TREE_TYPE (tmp));
                  
                  if (TREE_CODE (DECL_FIELD_OFFSET (tmp)) == INTEGER_CST)
                    offset = (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (tmp)) * BITS_PER_UNIT
                                  + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (tmp)));
                  else
                    offset = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (tmp));
                  
                  field_name = get_ir_field_name (tmp, offset, fld_name_buf);
                  field_name = build_ir_mod_string (field_name);

                  member = build_ir_mod_list();
                  member->next = LNULL;

                  if (DECL_BIT_FIELD (tmp)) 
                    {
                      unsigned int align;
                      /* container type */
                      tree fld_type = DECL_BIT_FIELD_TYPE (tmp);
                      /* container type size in bytes */
                      long long fld_size = get_type_size (fld_type); 

                      /* DECL_ALIGN (op1) == 1 bit - for 'packed' bit-fields */
                      /* DECL_ALIGN (op1) - user requested align for bit-fields */
                      /* always use DECL_ALIGN for struct fields */
                      align = MAX (DECL_ALIGN (tmp), 8); /* at least one byte */

                      ir_mbr = build_ir_type (IR_BITFIELD, 
                                              /* make sure child align is less than 
                                               * parent struct align */
                                              MIN (ir_type->align, ir_type_node_align (align)));
                      ir_mbr->parent = ir_type;
                      ir_mbr->label = field_name;

                      /* align of bit-field contain type doesn't need to adjusted,
                       * so use packed_align == -1
                       */
                      ir_mbr->t.bf.bf_type = 
                          map_gnu_type_to_IR_TYPE_NODE_internal (fld_type, 0, field_name, 
                                                                 -1, 0);

                      ir_mbr->t.bf.offset = ((offset / 8) / fld_size) * fld_size;
                      ir_mbr->t.bf.bit_width = size;
                      ir_mbr->t.bf.bit_offset = offset % (fld_size * 8);
                    } 
                  else if (DECL_C_BIT_FIELD (tmp))
                    {
                      /* make sure child align is less than parent struct align */
                      int decl_align = ir_type_node_align (DECL_ALIGN (tmp));
                      int min_align = MIN (decl_align, ir_type->align);
                      ir_mbr = build_ir_type (map_gnu_bitfield_type_to_tword (tmp), min_align);
                      ir_mbr->parent = ir_type;
                      ir_mbr->alias_level = default_alias_level;
                      ir_mbr->label = field_name;
                      ir_mbr->t.b.offset = offset / 8;
                      ir_mbr->t.b.size = size / 8;
                    }
                  else
                    {
                      int decl_align = ir_type_node_align (DECL_ALIGN (tmp));
                      int min_align = MIN (decl_align, ir_type->align);
                      int vptr_skew = 0;

                      /* iropt needs vptr name to match vtable name */
                      if (ir_language == CDOUBLEPLUS && DECL_VIRTUAL_P (tmp))
                        {
                          tree vtable = BINFO_VTABLE (binfo);
                          tree op0, vt, skew;
                          if (vtable == NULL)
                            {
                              tree bi = binfo;
                              /* empty binfo.vtable, but have vptr field_decl,
                               * should only mean that current struct is in
                               * chain of primary base classes, so look for derived
                               * class via BINFO_INHERITANCE_CHAIN */
                              while (BINFO_INHERITANCE_CHAIN (bi) 
                                   /*  && BINFO_PRIMARY_P (bi) check here ? */
                                     && BINFO_VTABLE (bi) == NULL)
                                bi = BINFO_INHERITANCE_CHAIN (bi);
                              
                              vtable = BINFO_VTABLE (bi);
                              gcc_assert (vtable != NULL); /* shouldn't be NULL now */
                            }
                          
                          if (vtable && TREE_CODE (vtable) == PLUS_EXPR)
                            {
                              op0 = TREE_OPERAND (vtable, 0);
                              gcc_assert (TREE_CODE (op0) == ADDR_EXPR);
                              vt = TREE_OPERAND (op0, 0);
                              gcc_assert (TREE_CODE (vt) == VAR_DECL);
                              field_name = get_ir_name (vt);
                              gcc_assert (field_name != NULL);
                              
                              field_name = build_ir_mod_string (field_name);
                              
                              skew = TREE_OPERAND (vtable, 1);
                              gcc_assert (TREE_CODE (skew) == INTEGER_CST);
                              vptr_skew = TREE_INT_CST_LOW (skew);
                            }
                        }
                      
                      if (ir_language == CDOUBLEPLUS && DECL_FIELD_IS_BASE (tmp))
                        {
                          /* 'binfo' is the binfo of this class 'node',
                           * need to find corresponding binfo for field_decl 'tmp' */
                          tree child_binfo = 0;
                          tree base_binfo = 0;
	                  VEC (tree,gc) *vbases;
                          int i;
                          bool found = false;
                          /* iterate through primary base classes */
                          for (i = 0; BINFO_BASE_ITERATE (binfo, i, base_binfo); ++i)
                            {
                              if (BINFO_TYPE (base_binfo) == TREE_TYPE (tmp))
                                {
                                 /* offset of class "node" within its parent class
                                    HOST_WIDE_INT binfo_offset = 
                                      tree_low_cst (BINFO_OFFSET (binfo), 0);
                                      
                                    offset of sub-class within "node's" parent class
                                    HOST_WIDE_INT base_binfo_offset =
                                      tree_low_cst (BINFO_OFFSET (base_binfo), 0);
                                      
                                    it suppose to:
                                    base_binfo_offset - binfo_offset == offset / 8
                                  
                                    for most of the types, but virtual bases can have
                                    paddings by C++ front-end, so no way to verify
                                    offset reliably */
                                  child_binfo = base_binfo;
                                  found = true;
                                  break;
                                }
                            }
                          /* iterate through indirect virtual bases */
                          if (!found && abi_version_at_least (2)
                              && (vbases = CLASSTYPE_VBASECLASSES (BINFO_TYPE (binfo))) != NULL)
                            for (i = 0; VEC_iterate (tree, vbases, i, base_binfo); i++)
                              {
                                if (BINFO_TYPE (base_binfo) == TREE_TYPE (tmp))
                                  {
                                    child_binfo = base_binfo;
                                    found = true;
                                    break;
                                  }
                              }
                         
                          gcc_assert (found);

                          ir_mbr = 
                            map_gnu_type_to_IR_TYPE_NODE_internal (TREE_TYPE (tmp), ir_type, 
                                                                   field_name, min_align, 
                                                                   child_binfo);
                        }
                      else  
                        /* field align may be different than type align for packed structs,
                         * so always use DECL_ALIGN.
                         * DECL_ALIGN may be different from type align even
                         * if DECL_PACKED (tmp) and DECL_USER_ALIGN (tmp) are not set */
                        ir_mbr = 
                          map_gnu_type_to_IR_TYPE_NODE_internal (TREE_TYPE (tmp), ir_type, 
                                                                 field_name, min_align, 0);
                      ir_mbr->label = field_name;
                      ir_mbr->t.b.offset = offset / 8;
                      ir_mbr->t.b.size = size / 8;

                      if (ir_language == CDOUBLEPLUS)
                        {
                          if (DECL_FIELD_IS_BASE (tmp))
                            ir_mbr->is_base_class = -1;
                          
                          if (DECL_VIRTUAL_P (tmp))
                            {
                              ir_mbr->is_vptr = -1;
                              gcc_assert (ir_mbr->tid == IR_POINTER);
                              ir_mbr->t.p.vptr_skew = vptr_skew;
                            }
                        }
                    }
                  member->datap = (LDATA *)ir_mbr;
                  ADD_TO_LIST (head, tail, member);
                }
              tmp = TREE_CHAIN (tmp);
            }

          if (head == NULL) /* empty structure - need at least one field for iropt/cg */
            {
              member = build_ir_mod_list();
              member->next = LNULL;
              ir_mbr = map_gnu_type_to_IR_TYPE_NODE_internal (void_type_node, ir_type,
                                                              ".fake.field", -1, 0);
              ir_mbr->label =  build_ir_mod_string (".fake.field");
              ir_mbr->t.b.offset = 0;
              ir_mbr->t.b.size = 0;
              member->datap = (LDATA *)ir_mbr;
              head = tail = member;
            }
          
          ir_type->t.s.children = head;
        }
	ret = ir_type;
        break;
      } 
    case METHOD_TYPE:
    case FUNCTION_TYPE:
	ir_type = build_ir_type (IR_FUNCTION, 0);
	ir_type->parent = parent;
	ir_type->t.f.func_name = NULL;
        SAVE_IF_NEEDED;
	ir_type->t.f.return_type = map_gnu_type_to_IR_TYPE_NODE_internal (TREE_TYPE (node), 
                                                                          0, 0, -1, 0);
        ir_type->alias_level = default_alias_level;
        {
          LIST *head = LNULL;
          LIST *tail = LNULL;
          LIST *param;

          tree parms = TYPE_ARG_TYPES (node);
          if (parms && parms != void_list_node)
            {
              for ( ; parms && parms != void_list_node; parms = TREE_CHAIN (parms))
                if (TREE_CODE (TREE_VALUE (parms)) != VOID_TYPE)
                  {
                    param = build_ir_mod_list();
                    param->next = LNULL;
                    param->datap = (LDATA *) 
                        map_gnu_type_to_IR_TYPE_NODE_internal (TREE_VALUE (parms), ir_type, 
                                                               0, -1, 0);

                    ADD_TO_LIST (head, tail, param);
                  }
              
              if (!parms)
                {
                  IR_TYPE_NODE *t;

                  t = build_ir_type (IR_ELLIPSIS, 0);
                  t->parent = ir_type;

                  param = build_ir_mod_list();
                  param->next = LNULL;
                  param->datap = (LDATA *) t;

                  ADD_TO_LIST (head, tail, param);
                }
            }
	  ir_type->t.f.params = head;
        }
	ret = ir_type;
        break;
/* case ENUMERAL_TYPE: // TODO XXX
      r_type = build_ir_type (PCC_ENUMTY, IR_TYPE_NODE_ALIGN (node));
      ir_type->parent = parent;
      ir_type->t.s.size = get_type_size (node);
      ir_type->t.s.offset = 0;
      ir_type->alias_level = default_alias_level;
      // ir_type->t.s.tag_name = build_ir_mod_string((char*)tp->ts_tag); 
      // ir_type->t.s.children = ir_build_menum_list(tp, ir_type);
      SAVE_IF_NEEDED;
      ret = ir_type;
  */  
    case LANG_TYPE:
      ret = 0;
      break;

    case VECTOR_TYPE:
      {
        int size;
        if (!TYPE_SIZE (node) || TREE_CODE (TYPE_SIZE (node)) != INTEGER_CST
            || !TYPE_SIZE_UNIT (node) || TREE_CODE (TYPE_SIZE_UNIT (node)) != INTEGER_CST)
          abort ();
        
        size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (node));
        /* doesn't fit into builtin type */
        if (size > 32)
          { /* almost the same as for arrays,
               except new definition of t.d.upper */
            ir_type = build_ir_type (IR_ARRAY, IR_TYPE_NODE_ALIGN (node));
            ir_type->parent = parent;
            if (packed_align >= 0) /* make sure child align is less than parent align */
              ir_type->align = MIN (ir_type->align, packed_align);
            ir_type->t.a.size = get_type_size (node);
            ir_type->t.a.offset = 0;
            ir_type->alias_level = default_alias_level;
            SAVE_IF_NEEDED;
            ir_type->t.a.element_type = map_gnu_type_to_IR_TYPE_NODE_internal (TREE_TYPE (node), 
                                                                               ir_type, 0,
                                                                               packed_align, 0);
            {
              LIST *dim;
              IR_TYPE_NODE *ir_dim;
              ir_dim = build_ir_type (IR_ARRAYDIM, 2);
              ir_dim->parent = ir_type;
              ir_dim->t.d.lower = 0;
              ir_dim->t.d.upper = TREE_INT_CST_LOW (TYPE_SIZE (node)) /
                                    TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (node))) - 1;
             dim = build_ir_mod_list();
             dim->next = LNULL;
             dim->datap = (LDATA *) ir_dim;
             ir_type->t.a.dimensions = dim;
            }
            ret = ir_type;
            break;
          }
        /* otherwise fallthrow for vector types that fit into builtin type */
      }
    default:
      ir_type = build_ir_type (map_gnu_type_to_tword (node), IR_TYPE_NODE_ALIGN (node));
      ir_type->parent = parent;
      if (packed_align >= 0) /* make sure child align is less than parent align */
        ir_type->align = MIN (ir_type->align, packed_align);
      ir_type->t.b.size = get_type_size (node);
      ir_type->t.b.offset = 0;
      if (parent)
        ir_type->alias_level = parent->alias_level;
      else if (lookup_attribute ("may_alias", TYPE_ATTRIBUTES (node)))
        ir_type->alias_level = ALIAS_ANY;
      else
        ir_type->alias_level = default_alias_level;
      SAVE_IF_NEEDED;
      ret = ir_type;
      break;
    }

  if (!entry) /* sanity check */
    abort ();
  
#undef SAVE_IF_NEEDED
#undef ADD_TO_LIST

  return ret;
}

IR_TYPE_NODE * 
make_ptr_to_ir_type_node (IR_TYPE_NODE * node)
{
  IR_TYPE_NODE * ir_type;
  struct ir_type_node_entry **slot, *entry;
 
#define PTR_TO_INTERNAL_NAME ".make_ptr_to.internal"

  /* find ir_type_node_elem in the hash table */
  slot = (struct ir_type_node_entry **)
    htab_find_slot (ir_type_node_htab, 
                    /* 0 - for gcc tree type
                     * node - for 'parent'. not really a parent though */
                    &(__extension__ (struct ir_type_node_elem) {0, node, PTR_TO_INTERNAL_NAME, 0}),
                    INSERT);
  entry = *slot;

  if (entry)
    return entry->p;

  /* create a pointer to 'node' */
  ir_type = build_ir_type (IR_POINTER, TARGET_ARCH64 ? 3 : 2);
  ir_type->parent = 0;
  ir_type->t.p.size = TARGET_ARCH64 ? 8 : 4;
  ir_type->t.p.offset = 0;
  ir_type->alias_level = default_alias_level;
  ir_type->t.p.designator = node;

  /* remember it in a hash table */
  entry = xmalloc (sizeof (*entry));
  *slot = entry;
  entry->e.type = 0;
  entry->e.parent = node;\
  entry->e.name = PTR_TO_INTERNAL_NAME;
  entry->p = ir_type;
  
  return ir_type;
}

IR_TYPE_NODE * 
map_gnu_type_to_IR_TYPE_NODE (tree node)
{
  IR_TYPE_NODE *ir_type;
  
  ir_type = map_gnu_type_to_IR_TYPE_NODE_internal (node, 0, 0, -1, 0);
  
/* not anymore  set_ir_type_alias_level (ir_type, default_alias_level); */

  return ir_type;
}

int
map_gnu_type (tree node)
{
  int tword = map_gnu_type_to_tword (node);
  map_gnu_type_to_IR_TYPE_NODE (node);
  return tword;
}

static unsigned int
internal_make_floatmap (tree type, unsigned int bitoff)
{
  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
      {
        TWORD tword;
        unsigned int off = bitoff / 8;
        /* misaligned float */
        if (off != ((off / get_type_size (type)) * get_type_size (type)))
          return 0;

        tword = map_gnu_type_to_tword (type);
        if (tword == PCC_FLOAT)
          return 1;/*0x80;*/
        else if (tword == PCC_DOUBLE || tword == PCC_FLOAT_COMPLEX)
          return 3;/*0xc0;*/
        else if (tword == PCC_LDOUBLE || tword == PCC_DOUBLE_COMPLEX)
          return 0xf;/*0xf0;*/
        else if (tword == PCC_LDOUBLE_COMPLEX)
          return 0xff;
        else
          return 0;
      }
    case RECORD_TYPE:
      {
        int cur_word_off = 0;
        unsigned int floatmap = 0;
        tree tmp = TYPE_FIELDS (type);
        while (tmp)
          { 
            if (TREE_CODE (tmp) == FIELD_DECL)
              {
                int offset;
                int word_off;
                
                if (TREE_CODE (DECL_FIELD_OFFSET (tmp)) == INTEGER_CST)
                  offset = (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (tmp)) * BITS_PER_UNIT
                                + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (tmp)));
                else
                  offset = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (tmp));

                word_off = offset / 32;

                if (word_off > 7)
                  return floatmap;

                if (cur_word_off < word_off)
                  cur_word_off = word_off;

                floatmap |= internal_make_floatmap (TREE_TYPE (tmp), offset) << cur_word_off;
              }
            tmp = TREE_CHAIN (tmp);
          }
        return floatmap;
      }
    case ARRAY_TYPE:
      {
        unsigned int floatmap = 0;
        unsigned int elem_floatmap = 0;
        int cur_word_off = 0;
        
        long long size = get_array_type_size (type);
        if (size == 0)
          return 0;
        
        elem_floatmap = internal_make_floatmap (TREE_TYPE (type), bitoff);
        if (elem_floatmap)
          while (cur_word_off < 8 && size -- > 0)
            {
              floatmap |= elem_floatmap << cur_word_off;
              cur_word_off += get_type_size (TREE_TYPE (type)) / 4;
            }
        
        return floatmap;
      }
    default:
      return 0;
    }
}

unsigned int
make_floatmap (tree type)
{
  if (!TARGET_ARCH64)
    return 0;

  if (TREE_CODE (type) != RECORD_TYPE)
    return 0;

  /* create one */
  return internal_make_floatmap (type, 0);
}

IR_TYPE_NODE *
ir_decref (IR_TYPE_NODE *ir_type)
{
  if (ir_type == NULL) return NULL;

  switch (ir_type->tid) 
    {
    case IR_ARRAY:
      return ir_type->t.a.element_type;
    case IR_POINTER:
      return ir_type->t.p.designator;
    case IR_FUNCTION:
      return ir_type->t.f.return_type;
    default:
      abort(); /* unexpected ir_type */
      return NULL;
    }
}

static int 
compare_ir_type_children (LIST * l1, LIST * l2)
{
  LIST * ch1 = l1;
  LIST * ch2 = l2;

  while (ch1 && ch2)
    {
      if (!is_ir_type_node_eq (LCAST (ch1, IR_TYPE_NODE), LCAST (ch2, IR_TYPE_NODE)))
        return 0;

      if (ch1->next == l1)
        ch1 = NULL;
      else
        ch1 = ch1->next;
      
      if (ch2->next == l2)
        ch2 = NULL;
      else
        ch2 = ch2->next;
    }

  if (ch1 != NULL || ch2 != NULL)
    return 0;

  return 1;
}

int 
is_ir_type_node_eq (IR_TYPE_NODE * t1, IR_TYPE_NODE * t2)
{
  if (t1 == t2)
    return 1;

  if (t1 == NULL || t2 == NULL || t1->tid != t2->tid || t1->align != t2->align)
    return 0;
  
  switch (t1->tid) 
    {
    case PCC_STRTY:
    case PCC_UNIONTY:
    case PCC_ENUMTY:
	if (t1->t.s.size != t2->t.s.size ||
	    t1->t.s.offset != t2->t.s.offset)
          return 0;

        /* check if tag_names are different */
	if (t1->t.s.tag_name != NULL && t2->t.s.tag_name != NULL)
          {
	    if (t1->t.s.tag_name[0] != t2->t.s.tag_name[0])
              return 0;
            
            if (strcmp (t1->t.s.tag_name, t2->t.s.tag_name))
              return 0;
          }
            
	return compare_ir_type_children (t1->t.s.children, t2->t.s.children);

    case IR_ARRAY:
	if (t1->t.a.size != t2->t.a.size ||
	    t1->t.a.offset != t2->t.a.offset)
          return 0;
        
	if (!is_ir_type_node_eq (t1->t.a.element_type, t2->t.a.element_type))
          return 0;
        
	return compare_ir_type_children (t1->t.a.dimensions, t2->t.a.dimensions);

    case IR_ARRAYDIM:
	if (t1->t.d.lower != t2->t.d.lower ||
	    t1->t.d.upper != t2->t.d.upper)
          return 0;
        
        return 1;

    case IR_POINTER:
	if (t1->t.p.size != t2->t.p.size ||
	    t1->t.p.offset != t2->t.p.offset)
          return 0;
        
	if (!is_ir_type_node_eq (t1->t.p.designator, t2->t.p.designator))
          return 0;
        
        return 1;

    case IR_FUNCTION:
	if (!is_ir_type_node_eq (t1->t.f.return_type, t2->t.f.return_type))
          return 0;
        
	return compare_ir_type_children (t1->t.f.params, t2->t.f.params);

    case IR_ELLIPSIS:
        return 1;

    case PCC_MOETY:
        if (t1->t.e.enumv != t2->t.e.enumv)
          return 0;

        return 1;

    case IR_BITFIELD:
	if (t1->t.bf.size != t2->t.bf.size ||
	    t1->t.bf.offset != t2->t.bf.offset ||
	    t1->t.bf.bit_width != t2->t.bf.bit_width ||
	    t1->t.bf.bit_offset != t2->t.bf.bit_offset)
          return 0;
        
	if (!is_ir_type_node_eq (t1->t.bf.bf_type, t2->t.bf.bf_type))
          return 0;

        return 1;
    default:
        /* base type */
	if (t1->t.b.size != t2->t.b.size ||
	    t1->t.b.offset != t2->t.b.offset)
          return 0;
        
        return 1;
    }
}

/* The IR_TYPE of the IR PARAM leaf created for a PARM_DECL does not
 always match the type of the PARM_DECL. Examples are 
 1. Param of double type is created for float params.
 2. Param of struct pointer type is created for structs passed by value.  */    
tree 
gnu_type_of_ir_param_leaf (tree decl)
{
  tree type;
  
  gcc_assert (TREE_CODE (decl) == PARM_DECL);
  
  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == REAL_TYPE)
    type = DECL_ARG_TYPE (decl);
  
  if (IS_ARRAY_TYPE (type) 
      || tu_pass_by_reference (NULL, TYPE_MODE (type), type, false))
    type = build_pointer_type (type);
  
  return type;
}


