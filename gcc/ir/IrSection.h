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

#ifndef _IRSECTION_H_
#define _IRSECTION_H_

#include "ir_common.h"
#include "IrProc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Null section handle. */
#define NULLIRSECTIONHDL NULL

/* Section handle */
struct ir_section_s;
typedef struct ir_section_s *ir_sect_hdl_t;

/* Section iterator */
typedef struct ir_table_iter_t ir_section_iter_t;

/* ir_sect_base_t enumerates all known base sections. */
#define IR_SECT_FIRST IR_SECT_UNKNOWN
#define IR_SECT_LAST (IR_SECT_NULL - 1)
typedef enum {
	IR_SECT_UNKNOWN,
	IR_SECT_BSS,
	IR_SECT_TBSS,
	IR_SECT_COMMENT,
	IR_SECT_DATA,
	IR_SECT_TDATA,
	IR_SECT_DATA1,
	IR_SECT_RODATA,
	IR_SECT_RODATA1,
	IR_SECT_TEXT,
	IR_SECT_EXCEPTION_RANGES,
	IR_SECT_PICDATA,
	IR_SECT_INIT,
	IR_SECT_FINI,
	IR_SECT_EH_FRAME,
	IR_SECT_LDATA,
	IR_SECT_LRODATA,
	IR_SECT_LBSS,
	IR_SECT_LDATA1,
	IR_SECT_LRODATA1,
	IR_SECT_COMMON,		/* pseudo-section for .common objects */
	IR_SECT_LBCOMMON,	/* pseudo-section for .lbcommon objects */
	IR_SECT_DATAPROF,	/* section for data profiling information */
        IR_SECT_JCR,            /* gcc .jcr (java class registration) section */
        IR_SECT_CTORS,          /* gcc .ctors section */
        IR_SECT_DTORS,          /* gcc .dtors section */
        IR_SECT_DATAREL,        /* gcc .data.rel section */
        IR_SECT_DATAREL_LOCAL,  /* gcc .data.rel.local section */
        IR_SECT_DATAREL_RO,     /* gcc .data.rel.ro section */
        IR_SECT_DATAREL_RO_LOCAL, /* gcc .data.rel.ro.local section */
	IR_SECT_NULL	/* This must always be last. */
} ir_sect_base_t;

/* Get/set the base of the section. */
ir_sect_base_t ir_sect_base(ir_sect_hdl_t section);
BOOLEAN ir_sect_set_base(ir_sect_hdl_t section, ir_sect_base_t base);

/* Get/set the section fragment name. */
const char *ir_sect_fragment(ir_sect_hdl_t section);
BOOLEAN ir_sect_set_fragment(ir_sect_hdl_t section, const char *fragment);

/* Get/set whether or not the section is in a section group. */
BOOLEAN ir_sect_is_in_group(ir_sect_hdl_t section);
BOOLEAN ir_sect_set_is_in_group(ir_sect_hdl_t section, BOOLEAN isInGroup);

/* Get/set whether or not the section is a comdat section. */
BOOLEAN ir_sect_is_comdat(ir_sect_hdl_t section);
BOOLEAN ir_sect_set_is_comdat(ir_sect_hdl_t section, BOOLEAN isComdat);

/* Determine the permissions of the section. */
BOOLEAN ir_sect_is_writable(ir_sect_hdl_t section);
BOOLEAN ir_sect_is_allocatable(ir_sect_hdl_t section);
BOOLEAN ir_sect_is_executable(ir_sect_hdl_t section);

/* Given a section, return the base name. */
const char *ir_sect_base_name(ir_sect_hdl_t section);

/* Given a section, copy the full name of the section into the buffer.
   If the name will not fit into the buffer or any other error occurs,
   false will be returned.
 */
BOOLEAN ir_sect_get_name(ir_sect_hdl_t section, char buffer[], int bufSize);

/* Create an iterator over the sections in a module. */
ir_section_iter_t ir_mod_section_iter(ir_mod_hdl_t module);

ir_sect_hdl_t ir_iter_next_section(ir_section_iter_t *iter);
BOOLEAN ir_iter_more_sections(ir_section_iter_t *iter);
void ir_iter_reset_section(ir_section_iter_t *iter);

#ifdef __cplusplus
}
#endif

#endif
