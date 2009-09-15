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

#ifndef _IRINITR_H_
#define _IRINITR_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include "IrSymbol.h"

/* Null initializer handle */
#define NULLIRINITRHDL NULL
/* NULL initializer position */
#define NULLIRINITRPOS (-1)

/* First and last constants of ir_initr_kind_t */
#define INITR_FIRST	INITR_UNKNOWN
#define INITR_LAST	INITR_END_NEST
typedef enum {	
	INITR_UNKNOWN, 
	INITR_INT8,	INITR_INT16,	INITR_INT32,	INITR_INT64, 
	INITR_FLOAT32,	INITR_FLOAT64,	INITR_FLOAT96,	INITR_FLOAT128, 
	INITR_STRING,	INITR_BYTES,
	INITR_DIFF32,	INITR_DIFF64,	INITR_REL32,	INITR_REL64, 
	INITR_ULEB128,	INITR_SLEB128,	INITR_SKIP,	INITR_BULK,
	INITR_REPEAT,	INITR_BEGIN_NEST,		INITR_END_NEST
} ir_initr_kind_t;

#if TARGET == SPARC
#define INITR_LONGDOUBLE	INITR_FLOAT128
#else
#define INITR_LONGDOUBLE	INITR_FLOAT96
#endif

/* Initializer position (byte offset) within a static object. */
typedef int64_t ir_position_t;

/* Initializer handle */
struct ir_initr_s;
typedef struct ir_initr_s *ir_initr_hdl_t;

/* Get the kind of initializer.  INITR_UNKNOWN returned on error */
ir_initr_kind_t ir_initr_kind(ir_initr_hdl_t initr);

/* Get the size in bytes of the initializer. */
int64_t ir_initr_size(ir_initr_hdl_t initr);

/* Get the position of the initializer from the beginning of the static object.
   Returns NULLIRINITRPOS if the initializer is within a repeat or on error. */
ir_position_t ir_initr_position(ir_initr_hdl_t initr);

/* Get the alignment of the initializer. */
int ir_initr_alignment(ir_initr_hdl_t initr);

/* Retrieve the value, position, etc. of various kinds of initializers. */
/* For INITR_INT8 */
BOOLEAN ir_initr_int8(ir_initr_hdl_t initr, int8_t *int8, 
		ir_position_t *position);
/* For INITR_INT16 */
BOOLEAN ir_initr_int16(ir_initr_hdl_t initr, int16_t *int16, ir_position_t *position, 
		BOOLEAN *misaligned);
/* For INITR_INT32 */
BOOLEAN ir_initr_int32(ir_initr_hdl_t initr, int32_t *int32, ir_position_t *position, 
		BOOLEAN *misaligned);
/* For INITR_INT64 */
BOOLEAN ir_initr_int64(ir_initr_hdl_t initr, int64_t *int64, ir_position_t *position, 
		BOOLEAN *misaligned);
/* For INITR_FLOAT32 */
BOOLEAN ir_initr_float32(ir_initr_hdl_t initr, float*float32, ir_position_t *position,
		BOOLEAN *misaligned);
/* For INITR_FLOAT64 */
BOOLEAN ir_initr_float64(ir_initr_hdl_t initr, double *float64, 
		ir_position_t *position, BOOLEAN *misaligned);
/* For INITR_FLOAT96 (long double on Intel/AMD64 */
BOOLEAN ir_initr_float96(ir_initr_hdl_t initr, long double *float96, 
		ir_position_t *position, BOOLEAN *misaligned);
/* For INITR_FLOAT128 (long double on SPARC) */
BOOLEAN ir_initr_float128(ir_initr_hdl_t initr, long double *float128, 
		ir_position_t *position, BOOLEAN *misaligned);
/* For both INITR_FLOAT96 and INITR_FLOAT128.  Aliases to either 
   ir_initr_float96() or ir_initr_float128() depending on the platform. */
BOOLEAN ir_initr_longdouble(ir_initr_hdl_t initr, long double *value, 
		ir_position_t *position, BOOLEAN *misaligned);
/* For INITR_STRING */
BOOLEAN ir_initr_string(ir_initr_hdl_t initr, const char **string, 
		ir_position_t *position);
/* For INITR_BYTES */
BOOLEAN ir_initr_bytes(ir_initr_hdl_t initr, unsigned *size, const char **init, 
		ir_position_t *position, BOOLEAN *is_ascii);
/* For INITR_REL32 */
BOOLEAN ir_initr_rel32(ir_initr_hdl_t initr, ir_sym_hdl_t *symbol, 
		int32_t *offset, ir_position_t *position, BOOLEAN *misaligned);
/* For INITR_REL64 */
BOOLEAN ir_initr_rel64(ir_initr_hdl_t initr, ir_sym_hdl_t *symbol, 
		int64_t *offset, ir_position_t *position, BOOLEAN *misaligned);
/* For INITR_DIFF16*/
BOOLEAN ir_initr_diff16(ir_initr_hdl_t initr, ir_sym_hdl_t *symbol1, 
		ir_sym_hdl_t *symbol2, int16_t *offset, ir_position_t *position,
		BOOLEAN *misaligned);
/* For INITR_DIFF32*/
BOOLEAN ir_initr_diff32(ir_initr_hdl_t initr, ir_sym_hdl_t *symbol1, 
		ir_sym_hdl_t *symbol2, int32_t *offset, ir_position_t *position,
		BOOLEAN *misaligned);
/* For INITR_DIFF64 */
BOOLEAN ir_initr_diff64(ir_initr_hdl_t initr, ir_sym_hdl_t *symbol1, 
		ir_sym_hdl_t *symbol2, int64_t *offset, ir_position_t *position,
		BOOLEAN *misaligned);
/* For INITR_ULEB128 */
BOOLEAN ir_initr_uleb128(ir_initr_hdl_t initr, uint64_t *value, 
		ir_position_t *position);
/* For INITR_SLEB128 */
BOOLEAN ir_initr_sleb128(ir_initr_hdl_t initr, int64_t *value, 
		ir_position_t *position);
/* For INITR_ULEB128 and INITR_SLEB128 */
/* Returns the number of bytes used to encode the given leb128 initializer.*/
int ir_initr_leb128_size(ir_initr_hdl_t initr); 
/* For INITR_SKIP */
BOOLEAN ir_initr_skip(ir_initr_hdl_t initr, uint64_t *bytes_to_skip, 
		ir_position_t *position);
/* For INITR_BULK.  See "Retrieving from Bulk Initializers". */
BOOLEAN ir_initr_bulk(ir_initr_hdl_t initr, uint64_t *size, 
		ir_position_t *position);
/* For INITR_REPEAT */
BOOLEAN ir_initr_repeat(ir_initr_hdl_t initr, unsigned *repeat_count, 
		ir_position_t *position);

/* Operations for bulk initializers.  (These are created by either
   ir_sobj_new_bulk_initr() or ir_sobj_new_bulk_initr_from_file().) */

/* Open a bulk initializer for reading. */
BOOLEAN ir_initr_bulk_open(ir_initr_hdl_t initr);
/* Get the specified number of bytes from the specified offset from the
   open bulk initializer and store the bytes into the given buffer. */
BOOLEAN ir_initr_bulk_get(ir_initr_hdl_t initr, uint64_t size, uint64_t offset, 
	void *buffer);
/* Put the contents of "buffer" (whose size is "size" bytes) into the
   bulk initializer.  */
BOOLEAN ir_initr_bulk_put(ir_initr_hdl_t initr, int64_t size, const void *buffer);
/* Close the bulk initializer, indicating the end of puts or gets. */
BOOLEAN ir_initr_bulk_close(ir_initr_hdl_t initr);


/* Get the string representions of ir_initr_kind_t enumeration constants. */
const char *ir_initr_kind_string(ir_initr_kind_t initr_kind);

/* Print the value of the initializer. */
void ir_initr_print_value(ir_initr_hdl_t initr, FILE *outfile);

#ifdef __cplusplus
}
#endif

#endif /* _IRINTR_H_ */
