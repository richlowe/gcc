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

#ifndef _EHNODE_H_
#define _EHNODE_H_

#include "ir_common.h"
#include "IrProc.h"
#include "IrList.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Null EH node handle. */
#define NULLIREHNODEHDL NULL

/* Determine if the given EH node kind can have a list of EH nodes associated
   with it. */
#define EH_NODE_CAN_HAVE_LIST(kind) (IR_ESTL == (kind) || IR_TYPELIST == (kind))

/* Determine if the EH node kind has a type name string associated with it. */
#define EH_NODE_CAN_HAVE_STRING(kind) (IR_RTTI == (kind))

/* EH node handle */
struct ir_eh_node_s;
typedef struct ir_eh_node_s *ir_eh_node_hdl_t;

/* Iterator for the list contained in an EH node. */
typedef ir_list_iter_t ir_eh_node_list_iter_t;

/* Different kinds of EH nodes. */
#define IR_EH_NODE_KIND_FIRST	IR_RTTI
#define IR_EH_NODE_KIND_LAST	IR_TYPELIST
typedef enum {
	IR_RTTI, IR_CATCH_ALL, IR_CLEANUP, IR_ESTL, IR_TYPELIST
} ir_eh_node_kind_t;


/* Operations that are valid for all kinds of EH nodes: */

/* Set the numeric ID for the given EH node. */
BOOLEAN ir_eh_node_set_id(ir_eh_node_hdl_t eh_node, uint32_t id);

/* Retrieve the numeric ID of the given EH node. */
uint32_t ir_eh_node_id(ir_eh_node_hdl_t eh_node);

/* Retrieve the kind of node of the given EH node. */
ir_eh_node_kind_t ir_eh_node_kind(ir_eh_node_hdl_t eh_node);
/* Note that there is no interface routine to change the kind
   of the node.  Although this would not be a big deal, it seems like it
   would make things a bit less error prone to not allow changing the kind
   of node after it is created.  (For example, what should we do if we have
   an IR_ESTL node that has a list and change its kind to be IR_RTTI,
   which cannot have a list?)
*/


/* Valid only for IR_RTTI: */
/* vvvvvvvvvvvvvvvvvvvvvvv */

/* Set the name of the RTTI type.  This routine automatically enters the
   string into the procedure's string table so the caller does not have to. */
BOOLEAN ir_eh_node_set_type_string(ir_eh_node_hdl_t, const char *);

/* Retrieve the name of the RTTI type. */
const char *ir_eh_node_type_string(ir_eh_node_hdl_t);

/* ^^^^^^^^^^^^^^^^^^^^^^^ */

/* Valid only for IR_ESTL and IR_TYPELIST: */
/* vvvvvvvvvvvvvvvvvvvvvvv */

/* Append node2 to the list of nodes contained in node1. */
BOOLEAN ir_eh_node_list_append(ir_eh_node_hdl_t node1, ir_eh_node_hdl_t node2);

/* Prepend node2 to the list of nodes contained in node1. */
BOOLEAN ir_eh_node_list_prepend(ir_eh_node_hdl_t node1, ir_eh_node_hdl_t node2);

/* For iterating over the list of EH nodes: */
/* Obtain an iterator over the list of EH nodes in the given EH node.*/
ir_eh_node_list_iter_t ir_eh_node_list_iter(ir_eh_node_hdl_t);

/* Get the next EH node in the list. */
ir_eh_node_hdl_t ir_iter_next_eh_list_node(ir_eh_node_list_iter_t *iter);

/* Determine if there are more EH nodes in the list. */
BOOLEAN ir_iter_more_eh_list_nodes(ir_eh_node_list_iter_t *iter);

/* Reset the iterator. */
void ir_iter_reset_eh_list_node(ir_eh_node_list_iter_t *iter);

/* ^^^^^^^^^^^^^^^^^^^^^^^ */

/* Given an EH node kind, return a string containing the kind.
   This is useful for debug/error printing. */
const char *ir_eh_node_kind_string(ir_eh_node_kind_t);

#ifdef __cplusplus
}
#endif

#endif
