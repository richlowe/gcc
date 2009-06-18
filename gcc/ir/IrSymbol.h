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

#ifndef _IRSYMBOL_H_
#define _IRSYMBOL_H_

#include "ir_common.h"
#include "IrProc.h"

#ifdef __cplusplus
extern "C" {
#endif

#define NULLIRSYMHDL NULL

/* type of symbol */
typedef enum { 
	IR_SYMTYPE_UNKNOWN = 0,	/* Unknown symbol type */
	IR_SYMTYPE_OBJECT, 	/* object */
	IR_SYMTYPE_TLS_OBJECT, 	/* thread-local storage object */
	IR_SYMTYPE_PROC		/* procedure */
} ir_sym_type_t;

#define IR_SYMTYPE_FIRST	((ir_sym_type_t) 0)
#define IR_SYMTYPE_LAST		IR_SYMTYPE_PROC

/* indicator of how the symbol is defined */
typedef enum {
	IR_SYMDEF_UNDEFINED = 0,/* the symbol is undefined */
	IR_SYMDEF_PROCEDURE,	/* the symbol is a procedure */
	IR_SYMDEF_OBJECT,	/* the symbol is an object/tls object */
	IR_SYMDEF_SYMBOL	/* the symbol is equated to another symbol */
} ir_sym_def_t;

#define IR_SYMDEF_FIRST	((ir_sym_def_t) 0)
#define IR_SYMDEF_LAST	IR_SYMDEF_SYMBOL

/* the binding of a symbol */
typedef enum {
	IR_SYMBINDING_UNKNOWN = 0,	/* unknown */
	IR_SYMBINDING_GLOBAL,		/* global */
	IR_SYMBINDING_LOCAL,		/* local */
	IR_SYMBINDING_WEAK		/* weak */
} ir_sym_binding_t;

#define IR_SYMBINDING_FIRST	((ir_sym_binding_t) 0)
#define IR_SYMBINDING_LAST	IR_SYMBINDING_WEAK

/* Scopes of symbols are defined in ir_common.h.  
   ir_sym_scope_t is defined to use a consistent naming scheme. */
typedef IR_LD_SCOPE ir_sym_scope_t;

/* Symbol handle */
struct ir_symbol_s;
typedef struct ir_symbol_s *ir_sym_hdl_t;

/* Set/get the symbol name */
BOOLEAN ir_sym_set_name(ir_sym_hdl_t symbol, const char *name);
const char *ir_sym_name(ir_sym_hdl_t symbol);

/* Set/get the symbol's user name if different than the actual name.
   The user name is usually the name as it appears in the source code
   prior to any mangling/globalization. */
BOOLEAN ir_sym_set_user_name(ir_sym_hdl_t symbol, const char *name);
const char *ir_sym_user_name(ir_sym_hdl_t symbol);

/* Set/get the symbol scope. */
BOOLEAN ir_sym_set_scope(ir_sym_hdl_t symbol, ir_sym_scope_t scope);
ir_sym_scope_t ir_sym_scope(ir_sym_hdl_t symbol);

/* Set/get the symbol binding. */
BOOLEAN ir_sym_set_binding(ir_sym_hdl_t symbol, ir_sym_binding_t binding);
ir_sym_binding_t ir_sym_binding(ir_sym_hdl_t symbol);

/* Set/get the symbol type. */
BOOLEAN ir_sym_set_type(ir_sym_hdl_t symbol, ir_sym_type_t type);
ir_sym_type_t ir_sym_type(ir_sym_hdl_t symbol);

/* Determine how the symbol is defined, if at all. 
   This attribute is set automatically when one of the 
   ir_sym_set_def_* procedures are called. */
ir_sym_def_t ir_sym_definition(ir_sym_hdl_t symbol);

/* Remove any definition of this symbol. */
BOOLEAN ir_sym_undefine(ir_sym_hdl_t symbol);

/* Define the symbol to be the specified procedure.
   This will fail if the type of the symbol is not IR_SYMTYPE_PROC. */
BOOLEAN ir_sym_set_def_procedure(ir_sym_hdl_t symbol, ir_proc_hdl_t);
/* NULLIRPROCHDL is returned if the symbol is not defined as a procedure. */
ir_proc_hdl_t ir_sym_def_procedure(ir_sym_hdl_t symbol);

/* Define the symbol to be the specified static object.
   This will fail if the type of the symbol is not IR_SYMTYPE_OBJECT or
   IR_SYMTYPE_TLS_OBJECT. */
struct ir_sobj_s;
BOOLEAN ir_sym_set_def_sobj(ir_sym_hdl_t symbol, struct ir_sobj_s *);
/* NULLIRSOBJHDL is returned if the symbol is not defined as an object. */
struct ir_sobj_s *ir_sym_def_sobj(ir_sym_hdl_t symbol);

/* Define the symbol to be equivalent to the given symbol + the given offset. */
BOOLEAN ir_sym_set_def_equivalent(ir_sym_hdl_t symbol, ir_sym_hdl_t equiv,
	int64_t offset);
/* NULLIRSYMHDL is returned if the symbol is not defined as equivalent to 
   another symbol. *offset will contain the offset from the returned symbol. */
ir_sym_hdl_t ir_sym_def_equivalent(ir_sym_hdl_t symbol, int64_t *offset);


/* Get the string representions of various enumeration constants. */
const char *ir_sym_binding_string(ir_sym_binding_t binding);
const char *ir_sym_type_string(ir_sym_type_t symtype);
const char *ir_sym_scope_string(ir_sym_scope_t scope);
const char *ir_sym_definition_string(ir_sym_def_t def);

#ifdef __cplusplus
}
#endif

#endif
