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

#ifndef _IRPROC_H_
#define _IRPROC_H_

#include "ir_common.h"
#include "IrTable.h"
#include "IrEhNode.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Null values for procedure and module handles. */
#define NULLIRPROCHDL NULL
#define NULLIRMODHDL NULL
                                                                                
struct IrModule;
typedef struct IrModule *ir_mod_hdl_t;
                                                                                
#ifndef IRPROCHDL
#define IRPROCHDL
struct IrProc;
typedef struct IrProc *ir_proc_hdl_t;
#endif

/* Iterators of the various objects within a procedure. */
typedef IrTableIterator ir_triple_iter_t;
typedef IrTableIterator ir_block_iter_t;
typedef IrTableIterator ir_leaf_iter_t;
typedef IrTableIterator ir_segment_iter_t;
typedef IrTableIterator ir_dbgid_iter_t;
typedef IrTableIterator ir_loopinfo_iter_t;
typedef IrTableIterator ir_pragmainfo_iter_t;
typedef IrTableIterator ir_linenum_iter_t;
typedef IrTableIterator ir_commsg_iter_t;
typedef IrTableIterator ir_edge_iter_t;
typedef IrTableIterator ir_dda_iter_t;
typedef IrTableIterator ir_looplife_iter_t;
typedef IrTableIterator ir_listnode_iter_t;
typedef IrTableIterator ir_typenode_iter_t;
typedef IrTableIterator ir_eh_node_iter_t;
typedef IrTableIterator ir_association_iter_t;

/* Mark a procedure as "dirty".  A procedure is dirty if the table contents
   have changed since it was last written/minimized.  
   Allocations and deallocations automatically mark the procedure as dirty.
   If a user changes an object and wants that change to be retained after
   minimizing the proc, he should mark the proc as being dirty.
*/
void ir_proc_mark_dirty(ir_proc_hdl_t proc);
BOOLEAN ir_proc_is_dirty(ir_proc_hdl_t proc);
void ir_proc_minimize_dirty(ir_proc_hdl_t proc); /* markDirty() + minimize() */

/* Restore a procedure from an input file.  If the procedure was stored
   to a temporary file (e.g., minimized dirty) restore from that.  Otherwise,
   restore from the original input file.  If the procedure is not contained
   in any input file, report an error.
*/
BOOLEAN ir_proc_restore_from_file(ir_proc_hdl_t proc);

/* Create a copy of an existing procedure.
   The copy of the procedure becomes part of the specified module.
   If module is NULL, it becomes part of the module containing the original.
   If the original was read from an input SunIR file, this copy may simply
   be re-read from the file.  Or it may be copied from the in-memory version.
   Used for cloning and inlining.
*/
ir_proc_hdl_t ir_proc_copy(ir_proc_hdl_t proc, ir_mod_hdl_t mod);

/* Just like ir_proc_copy(), except that the copy is always an image of
   the original as it appeared in the input file.
*/
ir_proc_hdl_t ir_proc_copy_from_input(ir_proc_hdl_t proc, ir_mod_hdl_t mod);

/* Make a copy of a procedure by reading it from the most recently written
   file copy.
*/
ir_proc_hdl_t ir_proc_copy_from_last_save(ir_proc_hdl_t srcProc,
                ir_mod_hdl_t destMod);


/* Merge two procedures.
   All table entries in src will be moved to dest.  
   The addresses of these entries will not change.
   Objects that are contained in a single linked list (e.g, LEAF, SEGMENT, etc.)
   will have the src and dest lists merged such that the dest list elements
   precede the src list elements.
   FALSE is returned on failure, TRUE otherwise.
*/
BOOLEAN ir_proc_merge(ir_proc_hdl_t dest, ir_proc_hdl_t src);

/* Completely destroy a procedure. */
void ir_proc_free(ir_proc_hdl_t proc);

/* Minimize the memory used by a procedure by destroying its tables.
   If necessary, table contents will be saved to a file.
   FALSE is returned on failure. */
BOOLEAN ir_proc_minimize(ir_proc_hdl_t proc);

/* Read all table data for the procedure into memory, if not already there.
   Returns FALSE on failure.  This procedure is useful for when the user
   forbids automatic maximization. */
BOOLEAN ir_proc_maximize(ir_proc_hdl_t proc);

/* Write the procedure to an output file.  
   The file used is the one specified in the most recent call to 
   ir_mod_open_for_output() for the module containing this procedure.  
   The module must be open for output when this function is called.
   FALSE is returned on error. */
BOOLEAN ir_proc_write(ir_proc_hdl_t proc);

/* Determine if the given procedure has been written to the currently open
   output file. */
BOOLEAN ir_proc_is_written(ir_proc_hdl_t proc);

/* Determine if the given procedure is in the minimized state. */
BOOLEAN ir_proc_is_minimized(ir_proc_hdl_t proc);


struct DbgSymID;	/* Same as the type DbgSymID */
struct compcom_msg;	/* Same as the type CompComMsg_t */
struct line_num;	/* Same as the type LineNum_t */
struct loop_life;	/* Same as the type LoopLife_t */
struct ln_prochdl;	/* The type pointed to by LN_ProcHandle_t */
typedef enum ir_assoc {
    IR_ASSOC_VAR_TLS = 0,
    IR_ASSOC_VLA_SIZE,
    IR_ASSOC_LEAF_ADDR_CONST,
    /* Additional entries as needed */
    IR_ASSOC_MAX  /* Must always be last value */
} ir_association_t;

/* Allocate a new procedure-level SunIR object.  NULL is returned on error. */

BLOCK *ir_proc_new_block(ir_proc_hdl_t proc);
TRIPLE *ir_proc_new_triple(ir_proc_hdl_t proc);
LEAF *ir_proc_new_leaf(ir_proc_hdl_t proc);
SEGMENT *ir_proc_new_segment(ir_proc_hdl_t proc);
struct DbgSymID *ir_proc_new_dbgid(ir_proc_hdl_t proc);
LOOPINFO *ir_proc_new_loopinfo(ir_proc_hdl_t proc);
PRAGMAINFO *ir_proc_new_pragmainfo(ir_proc_hdl_t proc);
struct line_num *ir_proc_new_linenum(ir_proc_hdl_t proc);
struct compcom_msg *ir_proc_new_commsg(ir_proc_hdl_t proc);
PROF_EDGE *ir_proc_new_edge(ir_proc_hdl_t proc);
struct loop_life *ir_proc_new_looplife(ir_proc_hdl_t proc);
LIST *ir_proc_new_listnode(ir_proc_hdl_t proc);
ir_eh_node_hdl_t ir_proc_new_eh_node(ir_proc_hdl_t, ir_eh_node_kind_t kind);

/* Allocate an array of SunIR objects of the specified object type.
   This is similar to ir_proc_new_*() except that multiple objects are
   allocated, and they are guaranteed to be contiguous.  
   The return value points to the first object in the array.
   NULL is returned on error. */

IR_NODE **ir_proc_new_ntuple_array(ir_proc_hdl_t proc, int elements);
int *ir_proc_new_int_array(ir_proc_hdl_t proc, int elements);
char *ir_proc_new_char_array(ir_proc_hdl_t proc, int elements);

/* Free an object previously allocated. */

void ir_proc_free_block(ir_proc_hdl_t proc, BLOCK *ptr);
void ir_proc_free_triple(ir_proc_hdl_t proc, TRIPLE *ptr);
void ir_proc_free_leaf(ir_proc_hdl_t proc, LEAF *ptr);
void ir_proc_free_segment(ir_proc_hdl_t proc, SEGMENT *ptr);
void ir_proc_free_dbgid(ir_proc_hdl_t proc, struct DbgSymID *ptr);
void ir_proc_free_loopinfo(ir_proc_hdl_t proc, LOOPINFO *ptr);
void ir_proc_free_pragmainfo(ir_proc_hdl_t proc, PRAGMAINFO *ptr);
void ir_proc_free_linenum(ir_proc_hdl_t proc, struct line_num *ptr);
void ir_proc_free_commsg(ir_proc_hdl_t proc, struct compcom_msg *ptr);
void ir_proc_free_edge(ir_proc_hdl_t proc, PROF_EDGE *ptr);
void ir_proc_free_looplife(ir_proc_hdl_t proc, struct loop_life *ptr);
void ir_proc_free_listnode(ir_proc_hdl_t proc, LIST *ptr);
void ir_proc_free_ntuple(ir_proc_hdl_t proc, IR_NODE **ntupleArray);
void ir_proc_free_int_array(ir_proc_hdl_t proc, int **intarray, int numElem);
void ir_proc_free_eh_node(ir_proc_hdl_t proc, ir_eh_node_hdl_t eh_node);

/* Free all objects of a given type. */
void ir_proc_free_all_blocks(ir_proc_hdl_t proc);
void ir_proc_free_all_triples(ir_proc_hdl_t proc);
void ir_proc_free_all_leaves(ir_proc_hdl_t proc);
void ir_proc_free_all_segments(ir_proc_hdl_t proc);
void ir_proc_free_all_dbgids(ir_proc_hdl_t proc);
void ir_proc_free_all_loopinfo(ir_proc_hdl_t proc);
void ir_proc_free_all_pragmainfo(ir_proc_hdl_t proc);
void ir_proc_free_all_linenums(ir_proc_hdl_t proc);
void ir_proc_free_all_commsgs(ir_proc_hdl_t proc);
void ir_proc_free_all_edges(ir_proc_hdl_t proc);
void ir_proc_free_all_dda(ir_proc_hdl_t proc);
void ir_proc_free_all_looplives(ir_proc_hdl_t proc);
void ir_proc_free_all_listnodes(ir_proc_hdl_t proc);
void ir_proc_free_all_ntuples(ir_proc_hdl_t proc);
void ir_proc_free_all_int_arrays(ir_proc_hdl_t proc);
void ir_proc_free_all_ehnodes(ir_proc_hdl_t proc);

/* Return the number of objects of the type specified. */

int ir_proc_num_blocks(ir_proc_hdl_t proc);
int ir_proc_num_triples(ir_proc_hdl_t proc);
int ir_proc_num_leaves(ir_proc_hdl_t proc);
int ir_proc_num_segments(ir_proc_hdl_t proc);
int ir_proc_num_dbgids(ir_proc_hdl_t proc);
int ir_proc_num_loopinfos(ir_proc_hdl_t proc);
int ir_proc_num_pragmainfos(ir_proc_hdl_t proc);
int ir_proc_num_linenums(ir_proc_hdl_t proc);
int ir_proc_num_commsgs(ir_proc_hdl_t proc);
int ir_proc_num_edges(ir_proc_hdl_t proc);
int ir_proc_num_ddas(ir_proc_hdl_t proc);
int ir_proc_num_looplives(ir_proc_hdl_t proc);
int ir_proc_num_listnodes(ir_proc_hdl_t proc);
int ir_proc_num_eh_nodes(ir_proc_hdl_t proc);

/* If the given string exists in the string table, return a pointer to it.
   If it does not already exist, add it and return a pointer to it.
   NULL is returned on error. */
const char *ir_proc_get_string(ir_proc_hdl_t proc, const char *string);


/* Retrieve the module to which this procedure belongs. 
   NULLIRMODHDL is returned on error. */
ir_mod_hdl_t ir_proc_module(ir_proc_hdl_t proc);


/* Retrieve/set the given procedure attribute. */

const char *ir_proc_name(ir_proc_hdl_t proc);
void ir_proc_set_name(ir_proc_hdl_t proc, const char *name);

const char *ir_proc_source_file(ir_proc_hdl_t proc);
void ir_proc_set_source_file(ir_proc_hdl_t proc, const char *sourcefile);

const char *ir_proc_section(ir_proc_hdl_t proc);
void ir_proc_set_section(ir_proc_hdl_t proc, const char *section);

TYPE ir_proc_type(ir_proc_hdl_t proc);
void ir_proc_set_type(ir_proc_hdl_t proc, TYPE procType);

int ir_proc_id(ir_proc_hdl_t proc);
void ir_proc_set_id(ir_proc_hdl_t proc, int procId);

int ir_proc_source_line(ir_proc_hdl_t proc);
void ir_proc_set_source_line(ir_proc_hdl_t proc, int sourceLine);

int ir_proc_opt_level(ir_proc_hdl_t proc);
void ir_proc_set_opt_level(ir_proc_hdl_t proc, int optLevel);

int ir_proc_num_ptrs(ir_proc_hdl_t proc);
void ir_proc_set_num_ptrs(ir_proc_hdl_t proc, int nptrs);

int ir_proc_num_elvars(ir_proc_hdl_t proc);
void ir_proc_set_num_elvars(ir_proc_hdl_t proc, int nels);

int ir_proc_reg_mask(ir_proc_hdl_t proc);
void ir_proc_set_reg_mask(ir_proc_hdl_t proc, int regmask);

int ir_proc_freg_mask(ir_proc_hdl_t proc);
void ir_proc_set_freg_mask(ir_proc_hdl_t proc, int fregmask);

LANG ir_proc_lang(ir_proc_hdl_t proc);
void ir_proc_set_lang(ir_proc_hdl_t proc, LANG lang);

INLINE_FIELD ir_proc_inline_control(ir_proc_hdl_t proc);
void ir_proc_set_inline_control(ir_proc_hdl_t proc, INLINE_FIELD inlineControl);

func_kind_t ir_proc_kind(ir_proc_hdl_t proc);
void ir_proc_set_kind(ir_proc_hdl_t proc, func_kind_t kind);

BOOLEAN ir_proc_is_comdat(ir_proc_hdl_t proc);
void ir_proc_set_is_comdat(ir_proc_hdl_t proc, BOOLEAN isComdat);

BOOLEAN ir_proc_is_thunk(ir_proc_hdl_t proc);
void ir_proc_set_is_thunk(ir_proc_hdl_t proc, BOOLEAN isThunk);

BOOLEAN ir_proc_has_local_aliases(ir_proc_hdl_t proc);
void ir_proc_set_has_local_aliases(ir_proc_hdl_t proc, BOOLEAN hasLocalAliases);

BOOLEAN ir_proc_must_have_frame(ir_proc_hdl_t proc);
void ir_proc_set_must_have_frame(ir_proc_hdl_t proc, BOOLEAN mustHaveFrame);

BOOLEAN ir_proc_tm_atomic (ir_proc_hdl_t proc);
void ir_proc_set_tm_atomic (ir_proc_hdl_t proc, BOOLEAN atomic);

/* If the forbid_auto_max attribute is true, an error will be reported when
   table data is accessed within a minimized procedure.  If the user sets an
   error handler that returns from this error, maximization will procede. */
BOOLEAN ir_proc_forbid_auto_max(ir_proc_hdl_t proc);
void ir_proc_set_forbid_auto_max(ir_proc_hdl_t proc, BOOLEAN forbidAutoMax);

/* Retrieve the LNI procedure handle. */
struct ln_prochdl *ir_proc_linenum_proc_hdl(ir_proc_hdl_t proc);

/* Retrieve/set the pointer to the head of an object list.  
   All objects of the indicated type within the procedure are expected to 
   be a part of this list.  The list is connected by a "next" pointer 
   within each object.  NULL is returned if the head has not been set. */

LEAF *ir_proc_leaf_head(ir_proc_hdl_t proc);
void ir_proc_set_leaf_head(ir_proc_hdl_t proc, LEAF *head);

BLOCK *ir_proc_block_head(ir_proc_hdl_t proc);
void ir_proc_set_block_head(ir_proc_hdl_t proc, BLOCK *head);

SEGMENT *ir_proc_segment_head(ir_proc_hdl_t proc);
void ir_proc_set_segment_head(ir_proc_hdl_t proc, SEGMENT *head);


/* Modify any linear lists in the LIST table and make them circular.  
   FALSE is returned on error.
   (Some of the front-ends create linear lists and convert them to 
   circular lists prior to writing a procedure.) */
BOOLEAN ir_proc_make_lists_circular(ir_proc_hdl_t proc);

/* Renumber the triples in the procedure.  
   Renumbering is performed by traversing the blocks in block list order and
   traversing the triples within each block in the order they appear in that
   block's triple chain. */
void ir_proc_renumber_triples(ir_proc_hdl_t proc);

/* Renumber leaves in list order.  The first leaf in the list is given id 0. */
void ir_proc_renumber_leaves(ir_proc_hdl_t irProc);

/* Renumber segments in list order.  The first segment is given id 0. */
void ir_proc_renumber_segments(ir_proc_hdl_t irProc);
 
/* Renumber blocks in list order.  The first block is given id 0. */
void ir_proc_renumber_blocks(ir_proc_hdl_t irProc);

BOOLEAN ir_proc_remove_unused_listnodes(ir_proc_hdl_t proc);
BOOLEAN ir_proc_remove_unused_objects(ir_proc_hdl_t proc);

/* Association map interface */

BOOLEAN ir_proc_associate_objs (ir_proc_hdl_t proc, void *obj_a,
                                ir_association_t assoc, void *obj_b);
BOOLEAN ir_proc_disassociate_objs (ir_proc_hdl_t proc, void *obj_a,
                                   ir_association_t assoc, void *obj_b);
void *ir_proc_get_associated (ir_proc_hdl_t proc, void *obj_a,
                              ir_association_t assoc);

/* Return an iterator to iterate over each object of the given type in the 
   procedure.  No order is guaranteed, but multiple passes over a table will
   return objects in the same order, provided no insertions or deletions 
   have been performed between those passes.
   Deletions may be performed while iterating over the objects.  (I.e., one 
   can iterate over the objects, deleting each object as it is seen.)
   Insertions may be performed while iterating over objects, but the inserted
   object may or may not be seen in a future iteration of the current pass. */

ir_block_iter_t ir_proc_block_iter(ir_proc_hdl_t proc);
ir_triple_iter_t ir_proc_triple_iter(ir_proc_hdl_t proc);
ir_leaf_iter_t ir_proc_leaf_iter(ir_proc_hdl_t proc);
ir_segment_iter_t ir_proc_segment_iter(ir_proc_hdl_t proc);
ir_dbgid_iter_t ir_proc_dbgid_iter(ir_proc_hdl_t proc);
ir_loopinfo_iter_t ir_proc_loopinfo_iter(ir_proc_hdl_t proc);
ir_pragmainfo_iter_t ir_proc_pragmainfo_iter(ir_proc_hdl_t proc);
ir_linenum_iter_t ir_proc_linenum_iter(ir_proc_hdl_t proc);
ir_commsg_iter_t ir_proc_commsg_iter(ir_proc_hdl_t proc);
ir_edge_iter_t ir_proc_edge_iter(ir_proc_hdl_t proc);
ir_dda_iter_t ir_proc_dda_iter(ir_proc_hdl_t proc);
ir_looplife_iter_t ir_proc_looplife_iter(ir_proc_hdl_t proc);
ir_listnode_iter_t ir_proc_listnode_iter(ir_proc_hdl_t proc);
ir_eh_node_iter_t ir_proc_eh_node_iter(ir_proc_hdl_t proc);
ir_association_iter_t ir_proc_association_iter(ir_proc_hdl_t proc,
                                               void *obj_a,
                                               ir_association_t assoc);

/* Retrieve a pointer to the next object of a given type within a procedure.  
   The procedure is specified when the iterator is created.  
   NULL is returned when all objects have been seen.
   Prior to calling this function, iter must be initialized with the
   value returned from a call to the appropriate ir_proc_*_iter() routine. */

BLOCK *ir_iter_next_block(ir_block_iter_t *iter);
TRIPLE *ir_iter_next_triple(ir_triple_iter_t *iter);
LEAF *ir_iter_next_leaf(ir_leaf_iter_t *iter);
SEGMENT *ir_iter_next_segment(ir_segment_iter_t *iter);
struct DbgSymID *ir_iter_next_dbgid(ir_dbgid_iter_t *iter);
LOOPINFO *ir_iter_next_loopinfo(ir_loopinfo_iter_t *iter);
PRAGMAINFO *ir_iter_next_pragmainfo(ir_pragmainfo_iter_t *iter);
struct line_num *ir_iter_next_linenum(ir_linenum_iter_t *iter);
struct compcom_msg *ir_iter_next_commsg(ir_commsg_iter_t *iter);
PROF_EDGE *ir_iter_next_edge(ir_edge_iter_t *iter);
struct loop_life *ir_iter_next_looplife(ir_looplife_iter_t *iter);
LIST *ir_iter_next_listnode(ir_listnode_iter_t *iter);
IR_TYPE_NODE *ir_iter_next_typenode(ir_typenode_iter_t *iter);
ir_eh_node_hdl_t ir_iter_next_eh_node(ir_eh_node_iter_t *iter);
void *ir_iter_next_association (ir_association_iter_t *iter);

/* Determine if there are more objects to be returned by the iterator.  
   If so, TRUE is returned. */

BOOLEAN ir_iter_more_blocks(ir_block_iter_t *iter);
BOOLEAN ir_iter_more_triples(ir_triple_iter_t *iter);
BOOLEAN ir_iter_more_leaves(ir_leaf_iter_t *iter);
BOOLEAN ir_iter_more_segments(ir_segment_iter_t *iter);
BOOLEAN ir_iter_more_dbgids(ir_dbgid_iter_t *iter);
BOOLEAN ir_iter_more_loopinfos(ir_loopinfo_iter_t *iter);
BOOLEAN ir_iter_more_pragmainfos(ir_pragmainfo_iter_t *iter);
BOOLEAN ir_iter_more_linenums(ir_linenum_iter_t *iter);
BOOLEAN ir_iter_more_commsgs(ir_commsg_iter_t *iter);
BOOLEAN ir_iter_more_edges(ir_edge_iter_t *iter);
BOOLEAN ir_iter_more_ddas(ir_dda_iter_t *iter);
BOOLEAN ir_iter_more_looplives(ir_looplife_iter_t *iter);
BOOLEAN ir_iter_more_listnodes(ir_listnode_iter_t *iter);
BOOLEAN ir_iter_more_typenodes(ir_typenode_iter_t *iter);
BOOLEAN ir_iter_more_eh_nodes(ir_eh_node_iter_t *iter);
BOOLEAN ir_iter_more_associations(ir_association_iter_t *iter);

/* Reset the iterator so that the next call to ir_iter_next_*() using this 
   iterator begins a new pass over the objects. */

void ir_iter_reset_block(ir_block_iter_t *iter);
void ir_iter_reset_triple(ir_triple_iter_t *iter);
void ir_iter_reset_leaf(ir_leaf_iter_t *iter);
void ir_iter_reset_segment(ir_segment_iter_t *iter);
void ir_iter_reset_dbgid(ir_dbgid_iter_t *iter);
void ir_iter_reset_loopinfo(ir_loopinfo_iter_t *iter);
void ir_iter_reset_pragmainfo(ir_pragmainfo_iter_t *iter);
void ir_iter_reset_linenum(ir_linenum_iter_t *iter);
void ir_iter_reset_commsg(ir_commsg_iter_t *iter);
void ir_iter_reset_edge(ir_edge_iter_t *iter);
void ir_iter_reset_dda(ir_dda_iter_t *iter);
void ir_iter_reset_looplife(ir_looplife_iter_t *iter);
void ir_iter_reset_listnode(ir_listnode_iter_t *iter);
void ir_iter_reset_typenode(ir_typenode_iter_t *iter);
void ir_iter_reset_eh_node(ir_eh_node_iter_t *iter);
void ir_iter_reset_association(ir_association_iter_t *iter);

/* Return size of the procedure, including tables and other dynamic memory. */
size_t ir_proc_sizeof(ir_proc_hdl_t proc);
size_t ir_proc_sizeof2(ir_proc_hdl_t proc, BOOLEAN print, FILE *outfile);

#ifdef __cplusplus
}
#endif

#endif
