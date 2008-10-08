/* Passing line number context information through Sun IR.
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

#ifndef __line_num_h
#define __line_num_h

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * This module implements the line-number-in-IR-node interface.  See
 * CAT proposal at
 * http://techpubs.eng.sun.com/teams/cat/proposals/yr2004/40.04-TAutrey-improve-line-number-info/
 */

/*
 * Incomplete types to ensure proper typing at the abstract interface.
 */

/*
 * Type for an abstract line number element.  Each IR node has a
 * collection of the concrete elements, which records the initial line
 * number information and all significant transformations.
 */

typedef struct ln_sl *LN_Element_t;

/* Type for an abstract include-file chain. */

typedef struct ln_ic *LN_FileCtx_t;

/* Type for an abstract iterator over line number elements. */

typedef struct ln_iter *LN_Iterator_t;

/* Type for an abstract procedure handle */

typedef struct ln_prochdl *LN_ProcHandle_t;

/*
 * Line number element kinds -- which of two sets of information
 * does this element belong to (2 bits).
 */

typedef enum ln_kind
{
    LN_KN_INVALID = 0,
    LN_KN_BASE,
    LN_KN_EXTENDED,
    LN_KN_COMPOSED,
    LN_KN_MAX
}
LN_Kind_t;

/*
 * Significant transformations on line numbers (32 allowed values + 32
 * in reserve, 6 bits).  When add a value, update op2kind() in
 * line_num.c.
 */

typedef enum ln_ops
{
    LN_OP_NONE=0,
    LN_XT_DEFAULT,
    LN_XT_MACRO_EXPAND,
    LN_XT_INCLUDE,
    LN_XT_INLINE,
    LN_XT_TEMPLATE_EXTDEF,      /* C++ implicit template definition file */
    LN_CM_DEFAULT = 16,
    LN_CM_CSE,
    LN_CM_CSE_CALL,
    LN_CM_CONSTANT_FOLD,
    LN_CM_SR,
    LN_CM_ALGEBRA,
    LN_OP_MAX
}
LN_Op_t;

/* Compiler phase (4 bits) */

typedef enum ln_phase
{
    LN_PH_PREPROCESSOR = 0,
    LN_PH_ACOMP,
    LN_PH_CCFE,
    LN_PH_F90COMP,
    LN_PH_OPTIMIZER,
    LN_PH_SPARCCG,
    LN_PH_INTELCG,
    LN_PH_MAX
}
LN_Phase_t;

/*
 * Flags to control global behavior of the LNI library
 * . LN_ALLOW_UNASSIGNED -- don't return an error when given designated
 *   "unassigned" values for file name, line number, column number.
 */

typedef enum ln_initflags
{
    LN_IFL_NONE = 0x0,
    LN_IFL_ALLOW_UNASSIGNED = 0x1,
    LN_IFL_MAP_NEGATIVE_TO_UNASSIGNED = 0x2,
    LN_IFL_USE_INTERNAL_MALLOC = 0x4
}
LN_InitFlags_t;

/*
 * Flags to control per-procedure behaviour.
 */
typedef enum ln_procflags
{
    LN_PFL_NONE = 0x0,
    LN_PFL_USE_INTERNAL_MALLOC = 0x1
}
LN_ProcFlags_t;

/*
 * Flags to control 'reset' behaviour.
 */
typedef enum ln_resetflags
{
    LN_RFL_NONE = 0x0
}
LN_ResetFlags_t;

/* Return (error) codes, also 2nd parameter of error callback */

typedef enum ln_sts
{
    LN_SUCCESS = 0,
    LN_ERR_ALLOC_FAILED,
    LN_ERR_PARAM_INVALID,
    LN_ERR_ITERATOR_INVALID,
    LN_WARN_END_OF_LIST,
    LN_ERR_INIT_CALLED,
    LN_ERR_INIT_NOT_CALLED,
    LN_ERR_BAD_STATE,
    LN_ERR_BUFFER_INVALID,
    LN_ERR_BEGIN_NOT_CALLED,
    LN_ERR_KEY_NOT_FOUND,
    LN_ERR_INTERNAL,
    LN_ERR_UNSUPPORTED_VERSION,
    LN_STS_MAX
}
LN_Status_t;

/* Routine codes - 1st parameter of error callback */

typedef enum ln_rtn
{
    LN_RTN_NONE = 0,
    LN_RTN_INIT,
    LN_RTN_RESET,
    LN_RTN_FINI,
    LN_RTN_FILE_CONTEXT_PUSH,
    LN_RTN_CREATE,
    LN_RTN_TRANSFORM,
    LN_RTN_TREE_VISIT,
    LN_RTN_TOP_ITERATOR_CREATE,
    LN_RTN_SUB_ITERATOR_CREATE,
    LN_RTN_ITERATOR_DESTROY,
    LN_RTN_ITERATOR_NEXT,
    LN_RTN_ELEMENT_EXTRACT,
    LN_RTN_FILE_CONTEXT_BASENAME,
    LN_RTN_ISBASE,
    LN_RTN_ISCOMPOSED,
    LN_RTN_ISEXTENDED,
    LN_RTN_SWIZZLE_BEGIN,
    LN_RTN_SWIZZLE_1NODE,
    LN_RTN_SWIZZLE_END,
    LN_RTN_UNSWIZZLE_BEGIN,
    LN_RTN_UNSWIZZLE_1NODE,
    LN_RTN_UNSWIZZLE_END,
    LN_RTN_PROCEDURE_CREATE,
    LN_RTN_PROCEDURE_DESTROY,
    LN_RTN_COPY,
    LN_RTN_MAX
}
LN_Rtn_t;

/*
 * Special column numbers.
 * Note (1): All non-positive column numbers are reserved.
 *
 * The implementation may support column numbers only upto some
 * value (initially 2^20 - 1; implies 21 bits needed for representation),
 * need a distinguished value for non-representable column numbers.
 *
 * Need a distinguished number for columns when the correct one isn't known.
 */

#define LN_COL_MAXCOLUMN  -2
#define LN_COL_UNASSIGNED -1

/* Special value for initial (empty) include-file chain */

#define LN_FILECTX_EMPTY ((LN_FileCtx_t)0)

/* Special value for nil LN_Element_t */

#define LN_NULL ((LN_Element_t)0)

/* Special file name when the correct one isn't known */

#define LN_FILE_UNASSIGNED "Unknown file"

/* Special line number for when the correct one isn't known */

#define LN_LINE_UNASSIGNED -1

/* Procedure interface to line number manipulation library */

LN_Status_t linenum_init (void *(*ln_alloc) (size_t),
                          void (*ln_free) (void *,
                                           size_t),
                          void (*ln_error) (LN_Rtn_t,
                                            LN_Status_t,
                                            void *),
                          LN_InitFlags_t flags);

LN_Status_t linenum_fini (void);

LN_ProcHandle_t linenum_procedure_create (LN_ProcFlags_t flags);

void linenum_procedure_destroy (LN_ProcHandle_t);

LN_FileCtx_t linenum_file_context_push (LN_ProcHandle_t hdl,
                                        LN_FileCtx_t ctx,
                                        const char *filename,
                                        int line_num,
                                        int column,
                                        LN_Op_t op,
                                        LN_Phase_t phase);

LN_Element_t linenum_create (LN_ProcHandle_t hdl,
                             LN_FileCtx_t ctx,
                             int line_num,
                             int column,
                             LN_Phase_t phase);

LN_Element_t linenum_transform (LN_ProcHandle_t thdl,
                                LN_Element_t target,
                                LN_ProcHandle_t chdl,
                                LN_Element_t context,
                                LN_Op_t op,
                                LN_Phase_t phase);

LN_Element_t linenum_copy (LN_ProcHandle_t shdl,
                           LN_Element_t src,
                           LN_ProcHandle_t dhdl);

void linenum_tree_visit (LN_Element_t root,
                         void *ctx,
                         void (*visit_fn)(void *ctx,
                                          const char *filename,
                                          int line_num,
                                          int column,
                                          LN_Op_t op,
                                          LN_Phase_t phase),
                         LN_Kind_t kind);

LN_Iterator_t linenum_top_iterator_create (LN_Element_t base,
                                           LN_Kind_t kind);

LN_Iterator_t linenum_sub_iterator_create (LN_Iterator_t base_iterator,
                                           LN_Kind_t kind);

void linenum_iterator_destroy (LN_Iterator_t iterator);

LN_Status_t linenum_iterator_next (LN_Iterator_t iterator);

void linenum_element_extract (LN_Iterator_t iterator,
                              const char **filename,
                              int *line_num,
                              int *column,
                              LN_Op_t *op,
                              LN_Phase_t *phase,
                              LN_Kind_t *kind);

const char *linenum_file_context_basename (LN_FileCtx_t ctx);

int linenum_isbase (LN_Iterator_t iterator);

int linenum_iscomposed (LN_Iterator_t iterator);

int linenum_isextended (LN_Iterator_t iterator);

void linenum_swizzle_begin (LN_ProcHandle_t hdl,
                            char *buf,
                            int len);

LN_Element_t linenum_swizzle_1node (LN_ProcHandle_t hdl,
                                    LN_Element_t node);

void linenum_swizzle_end (LN_ProcHandle_t hdl);

void linenum_unswizzle_begin (LN_ProcHandle_t hdl);

LN_Element_t linenum_unswizzle_1node (LN_ProcHandle_t hdl,
                                      LN_Element_t node);

void *linenum_unswizzle_end (LN_ProcHandle_t hdl,
                             int *len,
                             int *alloc);

#ifdef __cplusplus
}
#endif

#endif
