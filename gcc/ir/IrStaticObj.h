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

#ifndef _IRSTATICOBJ_H_
#define _IRSTATICOBJ_H_

#include "ir_common.h"
#include "IrTable.h"
#include "IrSymbol.h"
#include "IrSection.h"
#include "IrInitr.h"

#ifdef __cplusplus
extern "C" {
#endif

#define NULLIRSOBJHDL NULL

/* ir_initr_ret_status_t is used by the ir_sobj_get_init_value() routine. */
typedef enum { 
	IR_IRSTATUS_SUCCESS, /* successful retrieval of initializer bytes */
	IR_IRSTATUS_FAILURE, /* failure to retrieve initializer bytes */
	IR_IRSTATUS_PARTIAL  /* bytes retrieved contain a relocation */
} ir_initr_ret_status_t;

struct ir_sobj_s;
typedef struct ir_sobj_s *ir_sobj_hdl_t;

/* Initializer iterator */
typedef struct {
	ir_sobj_hdl_t sobj;	/* sobj whose init'rs we are iterating over */
	ir_initr_hdl_t current;	/* The next initializer to be returned */
} ir_initr_iter_t;

ir_sym_hdl_t ir_sobj_symbol(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_symbol(ir_sobj_hdl_t sobj, ir_sym_hdl_t symbol);

/* Get/set the size of the static object (in bytes). */
uint64_t ir_sobj_size(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_size(ir_sobj_hdl_t sobj, uint64_t size);

/* Get/set the alignment of the static obj.  Alignment must be a power of 2. */
int ir_sobj_alignment(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_alignment(ir_sobj_hdl_t sobj, int alignment);

/* Get/set the ELF section containing the static object. */
ir_sect_hdl_t ir_sobj_section(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_section(ir_sobj_hdl_t sobj, ir_sect_hdl_t section);

/* Get/set the tree type of the static object. */
/* If the SunIR tree type of the object is known, this may help optimization. */
IR_TYPE_NODE *ir_sobj_treetype(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_treetype(ir_sobj_hdl_t sobj, IR_TYPE_NODE *type);

/* Get/set the read-only attribute of the static object. */
/* Read-only objects must have initializers. */
BOOLEAN ir_sobj_is_readonly(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_is_readonly(ir_sobj_hdl_t sobj, BOOLEAN is_readonly);

/* Determine if the static object has any initializers. */
BOOLEAN ir_sobj_has_initializers(ir_sobj_hdl_t sobj);
/* Note that there is no routine to set this attribute.  It is derived
   by determining if initializers have been created for this object. */

/* Get/set whether or not the static object is compiler generated. */
/* Compiler-generated objects such as run-time type tables, etc. should be
   marked as such. */
BOOLEAN ir_sobj_is_compiler_generated(ir_sobj_hdl_t sobj);
BOOLEAN ir_sobj_set_is_compiler_generated(ir_sobj_hdl_t sobj, BOOLEAN is_gen);


/* PROTOTYPES FOR CREATING INITIALIZERS */

ir_initr_hdl_t ir_sobj_new_int8(ir_sobj_hdl_t sobj, int8_t, 
	ir_position_t position);
ir_initr_hdl_t ir_sobj_new_int16(ir_sobj_hdl_t sobj, int16_t, 
	ir_position_t position, BOOLEAN misaligned);
ir_initr_hdl_t ir_sobj_new_int32(ir_sobj_hdl_t sobj, int32_t, 
	ir_position_t position, BOOLEAN misaligned);
ir_initr_hdl_t ir_sobj_new_int64(ir_sobj_hdl_t sobj, int64_t, 
	ir_position_t position, BOOLEAN misaligned);
ir_initr_hdl_t ir_sobj_new_float32(ir_sobj_hdl_t sobj, float, 
	ir_position_t position, BOOLEAN misaligned);
ir_initr_hdl_t ir_sobj_new_float64(ir_sobj_hdl_t sobj, double, 
	ir_position_t position, BOOLEAN misaligned);
/* long double is 96 bits on Intel and AMD64 */
ir_initr_hdl_t ir_sobj_new_float96(ir_sobj_hdl_t sobj, long double, 
	ir_position_t position, BOOLEAN misaligned);
/* long double is 128 bits on SPARC */
ir_initr_hdl_t ir_sobj_new_float128(ir_sobj_hdl_t sobj, long double, 
	ir_position_t position, BOOLEAN misaligned);
/* This is essentially an alias for the two preceding functions. */
ir_initr_hdl_t ir_sobj_new_longdouble(ir_sobj_hdl_t sobj, long double, 
	ir_position_t position, BOOLEAN misaligned);
/* string must be null terminated.  Use ir_sobj_new_bytes() otherwise. */
ir_initr_hdl_t ir_sobj_new_string(ir_sobj_hdl_t sobj, const char *string, 
	ir_position_t position);
/* is_ascii argument is only used to determine print format */
ir_initr_hdl_t ir_sobj_new_bytes(ir_sobj_hdl_t sobj, int size, const char *init,
	ir_position_t position, BOOLEAN is_ascii);
ir_initr_hdl_t ir_sobj_new_rel32(ir_sobj_hdl_t sobj, ir_sym_hdl_t symbol, 
	int32_t offset, ir_position_t position, BOOLEAN misaligned);
ir_initr_hdl_t ir_sobj_new_rel64(ir_sobj_hdl_t sobj, ir_sym_hdl_t symbol, 
	int64_t offset, ir_position_t position, BOOLEAN misaligned);
/* symbol2 may NOT be "." */
ir_initr_hdl_t ir_sobj_new_diff16(ir_sobj_hdl_t sobj, ir_sym_hdl_t symbol1, 
	ir_sym_hdl_t symbol2, int16_t offset, ir_position_t position, 
	BOOLEAN misaligned);
/* symbol2 may be "." */
ir_initr_hdl_t ir_sobj_new_diff32(ir_sobj_hdl_t sobj, ir_sym_hdl_t symbol1, 
	ir_sym_hdl_t symbol2, int32_t offset, ir_position_t position, 
	BOOLEAN misaligned);
/* symbol2 may be "." */
ir_initr_hdl_t ir_sobj_new_diff64(ir_sobj_hdl_t sobj, ir_sym_hdl_t symbol1, 
	ir_sym_hdl_t symbol2, int64_t offset, ir_position_t position, 
	BOOLEAN misaligned);
/* uleb128 and sleb128 encoding are required for g++ exception handling data */
ir_initr_hdl_t ir_sobj_new_uleb128(ir_sobj_hdl_t sobj, uint64_t value, 
	ir_position_t position);
ir_initr_hdl_t ir_sobj_new_sleb128(ir_sobj_hdl_t sobj, int64_t value, 
	ir_position_t position);

/* For sparse initializations, skip uninitialized bytes. */
ir_initr_hdl_t ir_sobj_new_skip(ir_sobj_hdl_t sobj, int64_t bytes_to_skip,
	ir_position_t position);

/* For potentially huge initializers. */
ir_initr_hdl_t ir_sobj_new_bulk_initr(ir_sobj_hdl_t sobj, 
	ir_position_t position);
ir_initr_hdl_t ir_sobj_new_bulk_initr_from_file(ir_sobj_hdl_t sobj, 
	ir_position_t position, const char *inFileName);

/* For repeated sequences of initialization. */
ir_initr_hdl_t ir_sobj_new_repeat(ir_sobj_hdl_t sobj, int repeat_count, 
	ir_position_t position);
/* For nesting within repetition. */
ir_initr_hdl_t ir_sobj_new_begin_nest(ir_sobj_hdl_t sobj);
ir_initr_hdl_t ir_sobj_new_end_nest(ir_sobj_hdl_t sobj);

/* Operations for iterator over initializers of a sobj. */
ir_initr_iter_t ir_sobj_initr_iter(ir_sobj_hdl_t sobj);
ir_initr_hdl_t ir_iter_next_initr(ir_initr_iter_t *iter);
BOOLEAN ir_iter_more_initrs(ir_initr_iter_t *iter);
void ir_iter_reset_initr(ir_initr_iter_t *iter);


/* Retrieve the initial value of the static object at the given offset.
   The return value is false if an error has occurred.  An error will also be
   reported in such cases.
   size is the number of bytes to be retrieved.
   buffer is the address of the buffer to contain the retrieved bytes.
   buffer must be at least size bytes.
   status indicates the status of the retrieval.  Values include:
	IR_IRSTATUS_SUCCESS
	IR_IRSTATUS_FAILURE
	IR_IRSTATUS_PARTIAL	Some (or all) of the bytes were set to zero
		because they contained relocations.
   In the case of a partial success, the return value will be true.
*/
BOOLEAN ir_sobj_get_init_value(ir_sobj_hdl_t sobj, int64_t offset, 
		int64_t size, void *buffer, ir_initr_ret_status_t *status);

/* Retrieve the initializer at the given offset within the initialized
   static object.  If there is an error, NULLIRINITRHDL is returned.
   Errors include:
	There is no initializer at the given offset.
	The offset falls in the middle of an initializer.
	Invalid arguments.
   Note that only invalid arguments or some kind of internal error will
   cause this routine to call a libsunir error handling routine.
*/
ir_initr_hdl_t ir_sobj_get_initr(ir_sobj_hdl_t sobj, int64_t offset);


#ifdef __cplusplus
}
#endif

#endif
