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

#ifndef __GCC2IR_TREE_IR_H
#define __GCC2IR_TREE_IR_H

#ifndef __ir_common__
#include "ir/ir_common.h"
#endif

#ifndef _DBG_GEN_H
#include "ir/dbg_gen.h"
#endif

extern const char * dbg_cmdline;

#define BLOCK ir_BLOCK
#define PRAGMAINFO int
#include "ir/IrLib.h"
#undef BLOCK
#undef PRAGMAINFO

extern ir_proc_hdl_t irProc;
extern ir_mod_hdl_t irMod;

#include "ir/ir_types.h"
#include "real.h"
#include "ir/line_num.h"
#include "input.h"

typedef union list_u {	/* union of items stored on LIST structures */
	IR_NODE ir_node;
	IR_TYPE_NODE ir_type;
} LDATA;

#define IR_REG_G0 0
#define IR_REG_G2 2
#define IR_REG_G5 5
#define IR_REG_G7 7
#define IR_REG_O0 8
#define IR_REG_O6 14
#define IR_REG_O7 15
#define IR_REG_L0 16
#define IR_REG_L7 23
#define IR_REG_I0 24
#define IR_REG_I1 25
#define IR_REG_I6 30
#define IR_REG_I7 31
#define IR_REG_F0 32
#define IR_REG_SP IR_REG_O6
#define IR_REG_FP IR_REG_I6


#define IS_RECORD_TYPE(type) ((TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE)\
                              && !(TARGET_ARCH64 && (get_type_size (type) <= 32 \
                                                     && get_type_size (type) != 0)))
#define IS_ARRAY_TYPE(type) (TREE_CODE (type) == ARRAY_TYPE \
                             || (TREE_CODE (type) == VECTOR_TYPE \
                                 && TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type)) > 32))
#define IS_COMPLEX_TYPE(type) (TREE_CODE (type) == COMPLEX_TYPE \
                               || (TREE_CODE (type) == VECTOR_TYPE \
                                   && TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type)) > 8 \
                                   && TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type)) <= 32))
#define IS_LONGDOUBLE_TYPE(type) ((TREE_CODE (type) == REAL_TYPE \
                                   && TYPE_MODE (type) == TFmode)\
                                  || (TREE_CODE (type) == INTEGER_TYPE \
                                      && TYPE_MODE (type) == TImode))

/* Interface to IR conversion  */

extern char * make_global_name (const char * basename, int infunc, tree func);
extern int globalize_flag;
extern int default_alias_level;
void dbg_gen_fake_init (void);
void global_ir_init (void);
void global_ir_fini (void);
TWORD map_gnu_type_to_tword (tree);
TYPE map_gnu_type_to_TYPE (tree);
IR_TYPE_NODE * map_gnu_type_to_IR_TYPE_NODE (tree);
IR_TYPE_NODE * make_ptr_to_ir_type_node (IR_TYPE_NODE *);
TYPE map_gnu_bitfield_type_to_TYPE (tree node);
IR_TYPE_NODE * ir_decref (IR_TYPE_NODE *ir_type);
int is_ir_type_node_eq (IR_TYPE_NODE*, IR_TYPE_NODE*);
tree gnu_type_of_ir_param_leaf (tree);

int map_gnu_type (tree);
extern TYPE labelno_TYPE;
extern tree fval_type;
extern int default_opt_level;
extern const char *ir_global_prefix;
extern TYPE inttype;
extern TYPE ptrtype;
extern TYPE longtype;
extern TYPE undeftype;

extern int inside_eh_region;    /* true if current block is in EH region */
extern int fall_through_label;  /* of the current block */
extern int number_of_pbranch;   /* in the current block */

void save_eh_registers (void);
void restore_eh_registers (void);
int  get_ir_label (tree label);
int  gen_ir_label (void);

IR_NODE * ir_pbranch (int action_num, 
                      int eh_landing_label, int fall_through_label);
IR_NODE * get_ir_exception_filter (void);
IR_NODE * get_ir_exception_pointer (void);
void set_may_cause_exception (IR_NODE *);

extern SEGMENT * segtab[];
extern IR_NODE * func_ret_leaf; /* leaf contains function return value */
extern TRIPLE  * func_entry_triple; /* triple points to IR_ENTRY triple */
extern LEAF    * func_heap_leaf; /* leaf contains heap leaf of the current function */
extern SEGMENT * first_seg, *last_seg;
extern IR_TYPE_NODE *first_ir_type;
extern LOOPINFO *first_loop;

extern int npointers;	/* #trackable pointers (simple vars only) */
/*extern int naliases;*//* #names that can be aliased by pointers */
extern int ehnode_count;
extern ir_eh_node_hdl_t clean_up_ehnode;

extern LANG ir_language;

const char * get_ir_name (tree);
const char * get_ir_field_name (tree, int, char*);
const char * get_type_name (tree node);
int is_gxx_operator_new (tree);
long long get_type_size (tree node);
long long get_array_lower_bound (tree type);
long long get_array_type_size (tree type);
unsigned int make_floatmap (tree type);
extern void dump_function_ir (tree);
extern void global_ir_init (void);
extern void global_ir_fini (void);
extern void register_threadprivate_variable (tree, tree, tree, tree);
extern void remove_threadprivate_variable (tree);
extern void dump_ir_threadprivate_fn (void);
extern tree lookup_threadprivate_variable (tree decl);
extern tree cxx_omp_constructor_wrapper_for_irgen (tree, location_t, int);
void dump_ir_exit (TYPE ret_type, IR_TYPE_NODE *ret_ir_type, int ret_label, unsigned int floatmap);
void dump_ir_init (const char *procname,  tree fn, 
                   int procno,  TYPE proctype,  int procglobal, 
                   int rval_is_structptr, const char *source_file, int source_lineno);
void dump_ir_fini (tree fn, int do_write);
void init_segments (void);
void adjust_leaf_overlaps (void);
void set_leaf_pointerno (IR_NODE *np, TWORD tword);
void set_leaf_ld_scope (IR_NODE *np, int vis);
void init_ir_type_node_htab_once (void);
void init_ir_leaf_htab_once (void);
void init_ir_segment_htab_once (void);
void fini_ir_segment_htab (void);
void fini_ir_leaf_htab (void);
void fini_ir_proc (void);
bool ir_anon_aggr_field_decl_p (tree);

/* dbg gen */
void dbg_gen_extract_mod_debug_info (ir_mod_hdl_t);
void dbg_gen_global_var_decl (tree);

/* IR lexical scope support.  */
/* Nonzero if this is a scope that is relevant during IR generation. 
Abstract instance blocks are not relevant.  */
#define IR_RELEVANT_SCOPE_P(block) ((block != NULL_TREE) \
                                    && !(BLOCK_ABSTRACT (block) || BLOCK_ABSTRACT_ORIGIN (block)))

bool dbg_gen_generate_scopes (void);
bool ir_gen_scope_triple_p (void);
void ir_update_scope_triples (tree, DbgScopeID, DbgScopeID);
void ir_scope_gen_init (void);
void ir_scope_gen_fini (void);

void ir_backend_init (void);
void ir_backend_fini (void);

int gen_ir_label (void);
    
LOOPINFO * build_ir_loop (void);
IR_NODE * build_ir_triple (IR_OP , IR_NODE *,IR_NODE *, TYPE, IR_TYPE_NODE *);
ir_BLOCK * build_ir_block_nolabel (const char *s, int labelno, int is_entry, int is_global);
ir_BLOCK * build_ir_block (const char *s, int labelno, int is_entry, int is_global);
IR_TYPE_NODE * build_ir_type (TWORD tword, int align);
LEAF * build_ir_leaf (LEAF_CLASS c, TYPE type, IR_TYPE_NODE *typep, LEAF_VALUE *location, int insert);

IR_NODE * build_ir_labeldef (CONSZ l);
IR_NODE * build_ir_labelref (CONSZ l1, CONSZ l2);
IR_NODE * build_ir_labelref_with_type (CONSZ l1, CONSZ l2, TYPE);
void build_ir_goto (CONSZ l);

LEAF * get_ir_reg_var_leaf (const char *name, int regno, TYPE type, IR_TYPE_NODE *typep);
LEAF * get_ir_auto_var_leaf (const char *name, IR_OFFSZ segoffset, IR_OFFSZ seglen, int segalign, 
                             IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
LEAF * get_ir_extern_var_leaf (const char *name, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
LEAF * get_ir_parm_var_leaf (const char * name, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_reg_var (const char *name, int regno, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_auto_var (const char *name, IR_OFFSZ segoffset, IR_OFFSZ seglen, int segalign, 
                             IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_parm_var (const char *name, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_string_var (const char *label, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_static_var (const char *name, int labelno, IR_OFFSZ offset, TYPE type, 
                               IR_TYPE_NODE *typep);
/* Unlike other build_ir_*_var() functions, that take the name of variable as parameter,
   build_ir_extern_var() takes the VAR_DECL to enable debug information generation for 
   global variables during IR generation.  */
IR_NODE * build_ir_extern_var (tree decl, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_funcname (const char *name, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_addr_const (IR_NODE *np, IR_OFFSZ offset, TYPE type, IR_TYPE_NODE *typep);
IR_NODE * build_ir_char_const (char c, TYPE t);
IR_NODE * build_ir_string_const (const char *s);
IR_NODE * build_ir_int_const (CONSZ i, TYPE t, IR_TYPE_NODE *triple_ir_type);
IR_NODE * build_ir_int_complex_const (long long real_val, long long imag_val, TYPE type);
IR_NODE * build_ir_complex_const (const REAL_VALUE_TYPE *real_val, 
                                  const REAL_VALUE_TYPE *imag_val, TYPE type);
IR_NODE * build_ir_float_const (const REAL_VALUE_TYPE *val, TYPE type);

char * build_ir_proc_string (const char *str);
char * build_ir_mod_string (const char *str);

LIST * build_ir_proc_list (void);
LIST * build_ir_mod_list (void);

DbgSymID * build_ir_proc_dbgsym (DbgSymID dbg_symid);
DbgSymID * build_ir_mod_dbgsym (DbgSymID dbg_symid);

PRAGMAINFO *build_ir_pragmainfo (void);

TRIPLE* current_block_last_triple (void);
void remove_ir_triple (TRIPLE *);

/* in varasm.c */
void empty_const_desc_htab (void);

extern void check_ir_type (IR_NODE *);

extern IR_NODE * get_tmp_leaf (TYPE argtype, IR_TYPE_NODE * typep);
extern const char *generate_prefix(void);
extern LIST * ir_copy_overlaps (LEAF *leafp);

/* debug prints */
extern int tree_ir_verbosity_level;
extern void debug_type (IR_TYPE_NODE *np);
extern void debug_node (IR_NODE * np);
extern void ir_print_proc (void);
extern void ir_print_types (void);

/* LNI */
extern LN_Element_t lni_source_transition (expanded_location *);
extern LN_Element_t current_lni_handle;
extern void push_lni_inline_context (tree stmt);
extern void pop_lni_inline_context (tree stmt);

extern ir_eh_node_hdl_t build_ir_eh_node (ir_eh_node_kind_t);
extern void sunir_check_builtin_handling (tree function);
extern int sunir_check_128bits_handling (tree node);

#endif  /* GCC_TREE_IR_H  */
