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

#ifndef _IRMODULE_H_
#define _IRMODULE_H_

#include "IrProc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Note: ir_mod_hdl_t and NULLIRMODHDL are defined in IrProc.h above. */

struct DbgSymID;	/* Same as type DbgSymID defined elsewhere */

typedef LIST *IrModContext;

typedef struct {
	ir_mod_hdl_t irMod;
	LIST *curProc;
	BOOLEAN reset;
} ir_proc_iter_t;

/* Create a new, empty module.  Return NULLIRMODHDL on failure. */
ir_mod_hdl_t ir_mod_new(void);
 
/* Create a module by reading its contents from the given SunIR file.  Return NULLIRMODHDL on failure. */
ir_mod_hdl_t ir_mod_read(const char *inFileName);
 
/* Destroy the specified module and all procedures and objects contained in it.
*/
void ir_mod_free(ir_mod_hdl_t mod);
 
 
/* Open the module for output, creating or truncating the specified outputfile.
   All subsequent calls to ir_proc_write() for procedures contained in the
   given module will be written to this file.  
   No more than one output file may be open for a module at any given time.  
   FALSE is returned on error. */
BOOLEAN ir_mod_open_for_output(ir_mod_hdl_t module, const char *outFileName);
 
/* Write all module-level data to the output file specified in the previous
   call to ir_mod_open_for_output() and close the module for output.  
   The module must be open for output when this function is called.
   FALSE is returned on error. */
BOOLEAN ir_mod_close_for_output(ir_mod_hdl_t module);

/* Return TRUE if the module is currently open for output, FALSE otherwise. */
BOOLEAN ir_mod_is_open_for_output(ir_mod_hdl_t module);
 
/* Return the name of the current SunIR output file if the module is open for
   output, NULL otherwise. */
const char *ir_mod_outfile_name(ir_mod_hdl_t module);
 
/* Return the name of the SunIR input file from which this module was read
   or NULL if the module was not read from a SunIR file. */
const char *ir_mod_infile_name(ir_mod_hdl_t module);
 
 
/* If the module contains debug information (e.g., the input SunIR file
   contained dbg_gen information), initialize dbg_gen with this information.
   TRUE is returned upon successful dbg_gen initialization, FALSE if there is
   no dbg_gen information in the module. */
BOOLEAN ir_mod_init_dbggen_from_input(ir_mod_hdl_t ir_mod);
 
/* Extract the debug information from dbg_gen and place it in the module
   in preparation for output to a SunIR file.  
   This must be done after the last modification to the debug information but 
   before the call to ir_mod_close_for_output().  
   FALSE is returned on error. */
BOOLEAN ir_mod_prepare_dbggen_output (ir_mod_hdl_t ir_mod);

/* Return a pointer to the given string in the module-level string table.  
   If the string does not already exist in the table, it is inserted.
   NULL is returned on error. */
const char *ir_mod_get_string(ir_mod_hdl_t module, const char *string);


/* Return an iterator to traverse the procedures contained in the module. */
ir_proc_iter_t ir_mod_proc_iter(ir_mod_hdl_t module);


/* Retrieve/set the head of the list of SunIR type nodes.  
   NULL is returned if the head has not been set. */
IR_TYPE_NODE *ir_mod_type_head(ir_mod_hdl_t module);
void ir_mod_set_type_head(ir_mod_hdl_t module, IR_TYPE_NODE *head);


/* Retrieve/set the SunIR version numbers.  
   When a module is read from a SunIR input file, its version number is set to
   the version number found in the file. */
void ir_mod_version(ir_mod_hdl_t module, int *majorVer, int *minorVer, 
		int *devVer);
void ir_mod_set_version(ir_mod_hdl_t module, int majorVer, int minorVer, 
		int devVer);

/* Retrieve/Set the optimization status of the module. */
FILE_STATUS ir_mod_status(ir_mod_hdl_t module);
void ir_mod_set_status(ir_mod_hdl_t module, FILE_STATUS status);


/* Create a new, empty procedure within the module.  
   NULLIRPROCHDL is returned on failure. */
ir_proc_hdl_t ir_mod_new_proc(ir_mod_hdl_t module);

/* Allocate a new module-level object.
   Module-level objects include SunIR type nodes,list nodes used by the type 
   nodes, and debug symbol handles used by type nodes.
   NULL is returned on failure. */

IR_TYPE_NODE *ir_mod_new_typenode(ir_mod_hdl_t module);
struct DbgSymID *ir_mod_new_dbgid(ir_mod_hdl_t module);
LIST *ir_mod_new_listnode(ir_mod_hdl_t module);

/* Retrieve the handle of the next procedure in the module.  
   Prior to calling this function, iter must be initialized with the
   value returned from a call to ir_mod_proc_iter().
   NULLIRPROCHDL is returned when all procedures have been seen. */
ir_proc_hdl_t ir_iter_next_proc(ir_proc_iter_t *iter);
                                                                                
/* Determine if there are more procedures to be returned by the iterator.  
   If so, TRUE is returned. */
void ir_iter_reset_proc(ir_proc_iter_t *iter);
BOOLEAN ir_iter_more_procs(ir_proc_iter_t *iter);

#ifdef __cplusplus
}
#endif

#endif
