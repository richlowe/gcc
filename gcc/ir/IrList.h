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

#ifndef _IRLIST_H_
#define _IRLIST_H_

#ifdef __cplusplus
extern "C" {
#endif
                                                                                
/* 	ir_list_t represents a list of pointers.
	Items added to the list are not copied when added.
	When an item is removed from the list, the item itself is not
	deallocated (just the list node containing it.)
*/

#include "ir_common.h"
#include "IrTable.h"

struct IrTable;
typedef struct IrTable *ir_table_hdl_t;

typedef struct {
	LIST *tail;	/* Tail of the list. */
	ir_table_hdl_t table;	/* Table from which to allocate list nodes. */
} ir_list_t;


typedef struct {
	ir_list_t *list;	/* The list over which we are iterating. */
	LIST *curNode;	/* The node holding the most recently returned item. */
	LIST *prevNode;	/* The node before curNode. */
	BOOLEAN precomputed;	/* true if curNode already contains next node */
} ir_list_iter_t;

/* Initialize an empty list.  listnode_table is that table from which list nodes
   are to be allocated. */
BOOLEAN ir_list_init(ir_list_t *list, ir_table_hdl_t listnode_table);

/* Initialize a list from an existing circular list of LIST nodes. */
/* The LIST nodes must have been allocated from the given table. */
BOOLEAN ir_list_init_from_LIST(ir_list_t *list, ir_table_hdl_t listnode_table,
		const LIST *tail);

/* Destroy the list by deallocating all list nodes.
   This is not necessary if the list nodes are allocated from the list table
   of a module or procedure because the list nodes will be freed when the
   module or procedure is freed. */
BOOLEAN ir_list_destroy(ir_list_t *list);


/* Append an item to the end of a list. */
BOOLEAN ir_list_append(ir_list_t *list, void *item);


/* Prepend an item to the head of a list. */
BOOLEAN ir_list_prepend(ir_list_t *list, void *item);


/* Determine if the list is empty.  (More efficient than ir_list_items().) */
BOOLEAN ir_list_is_empty(const ir_list_t *list);


/* Return the number of items in the list.
   Note that if a NULL pointer is one of the items, it is still counted. */
int ir_list_items(const ir_list_t *list);


/* Create an iterator over the given list. */
ir_list_iter_t ir_list_iter(ir_list_t *list);


/* Return the next item in the list. */
void *ir_iter_next_list_item(ir_list_iter_t *iter);


/* Reset the list iterator to start over from the beginning of the list. */
void ir_iter_reset_list_item(ir_list_iter_t *iter);


/* Returns true if there are more items n the list. */
BOOLEAN ir_iter_more_list_items(const ir_list_iter_t *iter);


/* Remove from the list the item that was returned by the most recent call
   to ir_iter_next_list_item(). */
BOOLEAN ir_iter_remove_current_list_item(ir_list_iter_t *iter);


/* Replace in the list the item that was returned by the most recent call
   to ir_iter_next_list_item(). */
BOOLEAN ir_iter_replace_current_list_item(ir_list_iter_t *iter, void *item);

/* Return the pointer to the tail LIST node for the list.
   This is intended only for routines that must write out lists.
   The list is not modified by this routine. */
LIST *ir_list_to_LIST(const ir_list_t *list);

#ifdef __cplusplus
}
#endif

#endif

