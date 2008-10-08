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

#include <stdio.h>
#include <memory.h>
#include <stdarg.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "toplev.h"
#include "tree-ir.h"
#include "ir/irdebug.h"

void
ir_print_types (void)
{
  IR_TYPE_NODE *tp;

  fprintf(stderr, "IR_TYPE_NODEs:\n");
  for (tp = first_ir_type; tp; tp = tp->next) 
    if (tp->parent == NULL) 
      irTypeTreePrint (tp, stderr);
}

void __attribute__((used))
debug_type (IR_TYPE_NODE *np)
{
  irTypeTreePrint (np, stderr);
}

void __attribute__((used))
debug_node (IR_NODE * np)
{
  if (np->operand.tag == ISLEAF)
    irLeafPrint (&np->leaf, stderr);
  else if (np->operand.tag == ISTRIPLE)
    irTriplePrint (&np->triple, stderr);
  else
    fprintf (stderr, "unknown IR node %d\n", np->operand.tag);
}

void
ir_print_proc (void)
{
  irProcPrint3 (irProc, stderr);
}

