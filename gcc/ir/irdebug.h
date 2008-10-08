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
 
/* The routines described in this file are print routines used for
   debugging purposes during development.
   THEY ARE NOT A SUPPORTED INTERFACE TO THE LIBRARY.
   These routines will be removed once a supported SunIR printing
   interface is implemented.
*/

#ifndef _IRDEBUG_H_
#define _IRDEBUG_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include "IrLib.h"

typedef void (*ptr_print_func_t)(const void *, FILE *outfile);

void irBlockPrint(const ir_BLOCK *block, FILE *outfile);
void irBlockPrintList(const ir_BLOCK *blockList, FILE *outfile);
void irLeafPrintShort(const LEAF *leaf, FILE *outfile);
void irLeafPrint(const LEAF *leaf, FILE *outfile);
void irLeafPrintList(const LEAF *leaf, FILE *outfile);
void irListNodePrintPointers(const LIST *list, FILE *outfile);
void irListPrintPointers(const LIST *list, FILE *outfile);
void irListPrint(const LIST *list, ptr_print_func_t printFunc, FILE *outfile);
void irListPrintGeneric(const LIST *list, FILE *outfile);
void irLoopiPrint(const LOOPINFO *loop, FILE *outfile);
void irNodePrintShort(IR_NODE *node, FILE *outfile);
void irNodeListPrint(LIST *list, FILE *outfile);
void irSegPrint(const SEGMENT *segPtr, FILE *outfile);
void irSegPrintList(const SEGMENT *seg, FILE *outfile);
void irTriplePrintRowHeader(FILE *outfile);
void irTriplePrintRow(const TRIPLE *triple, FILE *outfile);
void irTriplePrint(const TRIPLE *triple, FILE * outfile);
void irTriplePrintChain(const TRIPLE *triple, int indent, FILE *outfile);
void irTypePrint(TYPE type, const IR_TYPE_NODE *ir_type, int size_and_alignment, FILE *outfile);
void irTypeTreePrint(const IR_TYPE_NODE *typeTree, FILE *outfile);
void irProcPrint2(ir_proc_hdl_t irProc, FILE *outfile);
void irProcPrint3(ir_proc_hdl_t irProc, FILE *outfile);

#ifdef __cplusplus
}
#endif

#endif
