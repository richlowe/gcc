/* Expand builtin functions from tree gimple into Sun IR.
   Copyright (C) 2009 by Sun Microsystems, Inc. All rights reserved.  
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "ggc.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "timevar.h"
#include "cp/cp-tree.h"
#include "toplev.h"
#include "except.h"
#include "cfgloop.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "c-common.h"
#include "tree-ssa-propagate.h"
#include "regs.h"
#include "tree-ir.h"
#include "df.h"
#include "tree-iterator.h"

#ifdef TARGET_CPU_x86
IR_NODE *get_ir_stack_pointer_reg (void);
static IR_NODE *get_ir_frame_pointer_reg (void);
static IR_NODE *dump_ir_builtin_return_addr (gimple, tree, tree, int);
static IR_NODE *dump_ir_builtin_init_trampoline (gimple, tree, int);

IR_NODE *
get_ir_stack_pointer_reg (void)
{
  IR_NODE * ret;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  
  if (TARGET_ARCH64)
    ret = build_ir_reg_var ("%rsp", IR_REG_SP, argtype, 
                            map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  else
    ret = build_ir_reg_var ("%esp", IR_REG_SP, argtype, 
                            map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
get_ir_frame_pointer_reg (void)
{
  IR_NODE * ret;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);

  if (TARGET_ARCH64)
    ret = build_ir_reg_var ("%rbp", IR_REG_FP, argtype, 
                            map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  else 
    ret = build_ir_reg_var ("%ebp", IR_REG_FP, argtype, 
                            map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
dump_ir_builtin_return_addr (gimple stmt, tree fndecl, tree arglist, int need_return)
{
  if (arglist == 0)
    /* Warning about missing arg was already issued.  */
    return dump_ir_call (stmt, need_return);
  else if (! host_integerp (TREE_VALUE (arglist), 1))
    {
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
        error ("invalid arg to %<__builtin_frame_address%>");
      else
        error ("invalid arg to %<__builtin_return_address%>");
      return dump_ir_call (stmt, need_return);
    }
  else
    {
      enum built_in_function fndecl_code = DECL_FUNCTION_CODE (fndecl);
      int count = tree_low_cst (TREE_VALUE (arglist), 1);
      IR_NODE * fp, *tmp;
      int i;
      
      /* Some machines need special handling before we can access
         arbitrary frames.  For example, on the sparc, we must first flush
         all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
      if (count > 0)
        SETUP_FRAME_ADDRESSES ();
#endif

      cfun->builtin_return_addr_called = 1;

#ifdef TARGET_CPU_sparc
      fp = tmp = get_ir_frame_pointer_reg ();
#elif TARGET_CPU_x86
      {
        TRIPLE * tp, * asm_args = NULL;
        IR_NODE * ir_string, * ir_clobber;
        tree decl = create_tmp_var_raw (ptr_type_node, NULL);

        TREE_USED (decl) = 1;
        DECL_ARTIFICIAL (decl) = 1;
        SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));

        fp = tmp = dump_ir_expr (decl, MAP_FOR_VALUE);

        if (TARGET_ARCH64)
          ir_string = build_ir_string_const ("movq %rbp, %0");
        else
          ir_string = build_ir_string_const ("mov %ebp, %0");

        ir_clobber = build_ir_string_const ("");
        tp = (TRIPLE*) build_ir_triple (IR_ASM_CLOBBER, ir_clobber, NULL,
                                        ir_clobber->leaf.type, NULL);
        TAPPEND (asm_args, (TRIPLE *) tp);

        tp = (TRIPLE*) build_ir_triple (IR_ASM_OUTPUT, fp,
                                      build_ir_string_const ("=r"),
                                      fp->leaf.type, NULL);
        tp->param_mode = PM_OUT;
        TAPPEND (asm_args, (TRIPLE *) tp);

        (void) build_ir_triple (IR_ASM_STMT, ir_string, (IR_NODE*)asm_args,
                              ir_string->leaf.type, NULL);
      }
#endif
      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) fp;
          LAPPEND (pinfo->u.s.omp_private, lp);
        }

      /* Scan back COUNT frames to the specified frame.  */
      for (i = 0; i < count; i++)
	{
          tmp = build_ir_triple (IR_IFETCH, tmp, 0, fp->leaf.type, fp->leaf.typep);
        }

      /* For __builtin_frame_address, return what we've got.*/ 
      if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
        return tmp;

      /* For __builtin_return_address, Get the return address from that
         frame.  */
      tmp = build_ir_triple (IR_PLUS, tmp, 
                             build_ir_int_const (4, 
                                                 offsettype, 0),
                             fp->leaf.type, 0);
      return build_ir_triple (IR_IFETCH, tmp, 0, fp->leaf.type, fp->leaf.typep);
    }
}

/* Emit IR stmts to initialize the variable parts of a trampoline.
   It is for x86. */

static IR_NODE *
dump_ir_builtin_init_trampoline (gimple stmt, tree arglist, int need_return)
{
  tree tramp = TREE_VALUE (arglist);
  tree func = TREE_VALUE (TREE_CHAIN (arglist));
  tree ctx = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  IR_NODE *ir_tramp, *ir_func, *ir_ctx, *t;
  TYPE uinttype = map_gnu_type_to_TYPE (unsigned_intSI_type_node);
  IR_TYPE_NODE * ir_uinttype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intSI_type_node);
  TYPE uchartype = map_gnu_type_to_TYPE (unsigned_intQI_type_node);
  IR_TYPE_NODE * ir_uchartype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intQI_type_node);

  if (TREE_CODE (TREE_TYPE (tramp)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (func)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (ctx)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);

  tramp_used = ctx;
  if (!TARGET_64BIT)
    {
      /* Compute offset from the end of the jmp to the target function.  
      rtx disp = expand_binop (SImode, sub_optab, fnaddr,
                               plus_constant (tramp, 10),
                               NULL_RTX, 1, OPTAB_DIRECT);
      emit_move_insn (gen_rtx_MEM (QImode, tramp),
                      gen_int_mode (0xb9, QImode));
      emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 1)), cxt);
      emit_move_insn (gen_rtx_MEM (QImode, plus_constant (tramp, 5)),
                      gen_int_mode (0xe9, QImode));
      emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 6)), disp);*/

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, build_ir_int_const (0xb9, uchartype, 0), uchartype, ir_uchartype);

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (1, uinttype, 0),
				  ir_tramp->operand.type, 0);
      ir_ctx = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, ir_ctx, uinttype, ir_uinttype);

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (5, uinttype, 0),
                                  ir_tramp->operand.type, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, build_ir_int_const (0xe9, uchartype, 0), uchartype, ir_uchartype);

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (6, uinttype, 0),
				 ir_tramp->operand.type, 0);
      ir_func = dump_ir_expr (func, MAP_FOR_VALUE);
      t = dump_ir_expr (tramp, MAP_FOR_VALUE);
      t = build_ir_triple (IR_PLUS, t, build_ir_int_const (10, uinttype, 0),
				t->operand.type, 0);
      t = build_ir_triple (IR_MINUS, ir_func, t, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
    }
  else
    {
      int offset = 0;
      TYPE ulltype = map_gnu_type_to_TYPE (unsigned_intDI_type_node);
      IR_TYPE_NODE * ir_ulltype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intDI_type_node);
      TYPE ushorttype = map_gnu_type_to_TYPE (unsigned_intHI_type_node);
      IR_TYPE_NODE * ir_ushorttype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intHI_type_node);

      /* Try to load address using shorter movl instead of movabs.
         We may want to support movq for kernel mode, but kernel does not use
         trampolines at the moment.  */
      if (x86_64_zext_immediate_operand (func, VOIDmode))
        {
          /* fnaddr = copy_to_mode_reg (DImode, fnaddr);
          emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
                          gen_int_mode (0xbb41, HImode));
          emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, offset + 2)),
                          gen_lowpart (SImode, fnaddr));*/
	  /* TODO: */
          warning (0, "This case isn't supported.");
          offset += 6;
        }
      else
        {
          /*emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
                          gen_int_mode (0xbb49, HImode));
          emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, offset + 2)),
                          fnaddr);*/
          ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
	  ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (offset, uinttype, 0),
				      ir_tramp->operand.type, 0);
          t = build_ir_int_const (0xbb49, ushorttype, 0);
	  t = build_ir_triple (IR_ISTORE, ir_tramp, t, ushorttype, ir_ushorttype);

	  ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
	  ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (offset + 2, uinttype, 0),
 				      ir_tramp->operand.type, 0);
          ir_func = dump_ir_expr (func, MAP_FOR_VALUE);
	  t = build_ir_triple (IR_ISTORE, ir_tramp, ir_func, ulltype, ir_ulltype);
          offset += 10;
        }
      /* Load static chain using movabs to r10.  
      emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
                      gen_int_mode (0xba49, HImode));
      emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, offset + 2)),
                      cxt);*/
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (offset, uinttype, 0),
				  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xba49, ushorttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, ushorttype, ir_ushorttype);

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (offset + 2, uinttype, 0),
				 ir_tramp->operand.type, 0);
      ir_ctx = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, ir_ctx, ulltype, ir_ulltype);
      offset += 10;
      /* Jump to the r11 
      emit_move_insn (gen_rtx_MEM (HImode, plus_constant (tramp, offset)),
                      gen_int_mode (0xff49, HImode));
      emit_move_insn (gen_rtx_MEM (QImode, plus_constant (tramp, offset+2)),
                      gen_int_mode (0xe3, QImode)); */
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (offset, uinttype, 0),
				  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xff49, ushorttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, ushorttype, ir_ushorttype);

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (offset + 2, uinttype, 0),
				ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xe3, uchartype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uchartype, ir_uchartype);

      offset += 3;
      gcc_assert (offset <= TRAMPOLINE_SIZE);
    }

#ifdef ENABLE_EXECUTE_STACK
/*  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
                     LCT_NORMAL, VOIDmode, 1, tramp, Pmode);*/
      func = build_decl (FUNCTION_DECL, get_identifier ("__enable_execute_stack"),
			 build_function_type (void_type_node, /* return type */
			 build_tree_list (NULL_TREE, ptr_type_node))); /* arg1 type */
      DECL_ARTIFICIAL (func) = 1;
      DECL_EXTERNAL (func) = 1;
      TREE_PUBLIC (func) = 1;
      TREE_NOTHROW (func) = 1;

      dump_ir_call (gimple_build_call (func, 1, tramp), 0);
#endif
}

#else /* Sparc. */
IR_NODE *get_ir_stack_pointer_reg (void);
static IR_NODE *get_ir_frame_pointer_reg (void);
static IR_NODE *get_ir_i7_reg (void);
static IR_NODE *dump_ir_flushw (gimple stmt ATTRIBUTE_UNUSED);
static IR_NODE *dump_ir_builtin_return_addr (gimple stmt, tree fndecl, tree arglist, int need_return);
static IR_NODE *dump_ir_builtin_init_trampoline (gimple stmt, tree arglist, int need_return);

IR_NODE *
get_ir_stack_pointer_reg (void)
{
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  
  IR_NODE * ret = build_ir_reg_var ("%sp", IR_REG_SP, argtype, 
                                     map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
get_ir_frame_pointer_reg (void)
{
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_NODE * ret = build_ir_reg_var ("%fp", IR_REG_FP, argtype, 
                                    map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
get_ir_i7_reg (void)
{
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_NODE * ret = build_ir_reg_var ("%i7", IR_REG_I7, argtype,
                                    map_gnu_type_to_IR_TYPE_NODE (ptr_type_node));
  ret->leaf.is_volatile = IR_TRUE;
  return ret;
}

static IR_NODE *
dump_ir_flushw (gimple stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *n, *clobber;
  TRIPLE *args = NULL;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);

  clobber = build_ir_triple (IR_ASM_CLOBBER, build_ir_string_const ("memory"), NULL, argtype, NULL);
  clobber->triple.is_volatile = IR_TRUE;
  TAPPEND (args, (TRIPLE *)clobber);

  if (TARGET_ARCH64)
    n = build_ir_triple (IR_ASM_STMT, build_ir_string_const ("flushw"), (IR_NODE*)args, argtype, NULL);
  else
    n = build_ir_triple (IR_ASM_STMT, build_ir_string_const ("ta\t3"), (IR_NODE*)args, argtype, NULL);
  clobber->triple.right = n;
  n->triple.is_volatile = IR_TRUE;
  return n;
}

/* Return a pointer to a stack frame, follow the dynamic chain COUNT
   times to get the address of either a higher stack frame, or a return
   address located within it (depending on FNDECL_CODE).
   __builtin_return_address(0) yields the address to which the current
   function will return.  __builtin_return_address(1) yields the address to
   which the caller will return, and so on up the stack.*/

static IR_NODE *
dump_ir_builtin_return_addr (gimple stmt, tree fndecl, tree arglist, int need_return)
{
  if (arglist == 0)
    /* Warning about missing arg was already issued.  */
    return dump_ir_call (stmt, need_return);
  else if (! host_integerp (TREE_VALUE (arglist), 1))
    {
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
        error ("invalid arg to %<__builtin_frame_address%>");
      else
        error ("invalid arg to %<__builtin_return_address%>");
      return dump_ir_call (stmt, need_return);
    }
  else
    {
      enum built_in_function fndecl_code = DECL_FUNCTION_CODE (fndecl);
      int count = tree_low_cst (TREE_VALUE (arglist), 1);
      IR_NODE * fp, *tmp;
      int i;
      
      /* Some machines need special handling before we can access
         arbitrary frames.  For example, on the sparc, we must first flush
         all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
      if (count > 0)
        dump_ir_flushw (stmt);
#endif

      /* On the sparc, the return address is not in the frame, it is in a
         register.  There is no way to access it off of the current frame
         pointer, but it can be accessed off the previous frame pointer by
         reading the value from the register window save area.  */
#ifdef RETURN_ADDR_IN_PREVIOUS_FRAME
      if (fndecl_code == BUILT_IN_RETURN_ADDRESS)
        count--;
#endif

      cfun->builtin_return_addr_called = 1;

#ifndef SPARC_STACK_BIAS
      abort();
#endif
          
      if (count == -1)
        {
          IR_NODE * reg_i7 = get_ir_i7_reg ();
          if (cur_omp_context && (cur_omp_context->pinfo
              || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
            {
              LIST *lp;
              PRAGMAINFO *pinfo;
              pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                             : cur_omp_context->prev_ctx->pinfo;
              lp = build_ir_proc_list ();
              lp->datap = (LDATA *) reg_i7;
              LAPPEND (pinfo->u.s.omp_private, lp);
            }
          return reg_i7;
        }
      
      fp = tmp = get_ir_frame_pointer_reg ();
      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) fp;
          LAPPEND (pinfo->u.s.omp_private, lp);
        }

      /* Scan back COUNT frames to the specified frame.  */
      for (i = 0; i < count; i++)
        {
          /* Assume the dynamic chain pointer is in the word that the
             frame address points to, unless otherwise specified.  */
          tmp = build_ir_triple (IR_PLUS, tmp, 
                                 build_ir_int_const (14 * UNITS_PER_WORD + SPARC_STACK_BIAS, 
                                                     offsettype, 0),
                                 fp->leaf.type, 0);
          tmp = build_ir_triple (IR_IFETCH, tmp, 0, fp->leaf.type, fp->leaf.typep);
        }

      /* For __builtin_frame_address, return what we've got. But on
         the SPARC for example, we may have to add a bias. */
      if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
        {
          if (TARGET_ARCH64)
            tmp = build_ir_triple (IR_PLUS, tmp, 
                                 build_ir_int_const (SPARC_STACK_BIAS, 
                                                     offsettype, 0),
                                 fp->leaf.type, 0);
          return tmp;
        }

      /* For __builtin_return_address, Get the return address from that
         frame.  */
      tmp = build_ir_triple (IR_PLUS, tmp, 
                             build_ir_int_const (15 * UNITS_PER_WORD + SPARC_STACK_BIAS, 
                                                 offsettype, 0),
                             fp->leaf.type, 0);
      return build_ir_triple (IR_IFETCH, tmp, 0, fp->leaf.type, fp->leaf.typep);
    }
}

/* Sparc: Emit IR stmts to initialize the variable parts of a trampoline.*/

static IR_NODE *
dump_ir_builtin_init_trampoline (gimple stmt, tree arglist, int need_return)
{
  tree tramp = TREE_VALUE (arglist);
  tree func = TREE_VALUE (TREE_CHAIN (arglist));
  tree ctx = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

  if (TREE_CODE (TREE_TYPE (tramp)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (func)) != POINTER_TYPE 
      || TREE_CODE (TREE_TYPE (ctx)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);

  if (TARGET_ARCH64)
    {
      /* SPARC 64-bit trampoline:
    
            rd	%pc, %g1
            ldx	[%g1+24], %g5
            jmp	%g5
            ldx	[%g1+16], %g5
            +16 bytes data
       */
      IR_NODE *ir_tramp, *t;
      TYPE uinttype = map_gnu_type_to_TYPE (unsigned_intSI_type_node);
      IR_TYPE_NODE * ir_uinttype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intSI_type_node);
      TYPE ulltype = map_gnu_type_to_TYPE (unsigned_intDI_type_node);
      IR_TYPE_NODE * ir_ulltype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intDI_type_node);
                             
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      t = build_ir_int_const (0x83414000, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, tramp),
            	  GEN_INT (trunc_int_for_mode (0x83414000, SImode)));*/
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (4, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xca586018, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 4)),
            	  GEN_INT (trunc_int_for_mode (0xca586018, SImode)));*/

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (8, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0x81c14000, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 8)),
            	  GEN_INT (trunc_int_for_mode (0x81c14000, SImode)));*/
      
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (12, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = build_ir_int_const (0xca586010, uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
      /*emit_move_insn (gen_rtx_MEM (SImode, plus_constant (tramp, 12)),
            	  GEN_INT (trunc_int_for_mode (0xca586010, SImode)));*/

      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (16, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, ulltype, ir_ulltype);
      /*emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, 16)), cxt);*/
      
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (24, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      t = dump_ir_expr (func, MAP_FOR_VALUE);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, ulltype, ir_ulltype);
      /*emit_move_insn (gen_rtx_MEM (DImode, plus_constant (tramp, 24)), fnaddr);*/

     /* emit_insn (gen_flushdi (validize_mem (gen_rtx_MEM (DImode, tramp))));*/
    
      /*if (sparc_cpu != PROCESSOR_ULTRASPARC
          && sparc_cpu != PROCESSOR_ULTRASPARC3)
        emit_insn (gen_flushdi (validize_mem (gen_rtx_MEM (DImode, plus_constant (tramp, 8)))));*/
    
      /* Call __enable_execute_stack after writing onto the stack to make sure
         the stack address is accessible.  */
      func = build_decl (FUNCTION_DECL, get_identifier ("__enable_execute_stack"), /* func name */
                       build_function_type (void_type_node, /* return type */
                         build_tree_list (NULL_TREE, ptr_type_node))); /* arg1 type */
      DECL_ARTIFICIAL (func) = 1;
      DECL_EXTERNAL (func) = 1;
      TREE_PUBLIC (func) = 1;
      TREE_NOTHROW (func) = 1;

      dump_ir_call (gimple_build_call (func, 1, tramp), 0);
    }
  else
    {
      /* SPARC 32-bit trampoline:
    
     	sethi	%hi(fn), %g1
     	sethi	%hi(static), %g2
     	jmp	%g1+%lo(fn)
     	or	%g2, %lo(static), %g2
    
        SETHI i,r  = 00rr rrr1 00ii iiii iiii iiii iiii iiii
        JMPL r+i,d = 10dd ddd1 1100 0rrr rr1i iiii iiii iiii
       */
    
      IR_NODE *ir_tramp, *ir_func, *ir_ctx, *t;
      TYPE uinttype = map_gnu_type_to_TYPE (unsigned_intSI_type_node);
      IR_TYPE_NODE * ir_uinttype = map_gnu_type_to_IR_TYPE_NODE (unsigned_intSI_type_node);
                             
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_func = dump_ir_expr (func, MAP_FOR_VALUE);
      t = build_ir_triple (IR_RSHIFT, ir_func, build_ir_int_const (10, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x03000000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 0)),
         expand_binop (SImode, ior_optab,
            	   expand_shift (RSHIFT_EXPR, SImode, fnaddr,
            			 size_int (10), 0, 1),
            	   GEN_INT (trunc_int_for_mode (0x03000000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (4, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      ir_ctx = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_RSHIFT, ir_ctx, build_ir_int_const (10, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x05000000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 4)),
         expand_binop (SImode, ior_optab,
            	   expand_shift (RSHIFT_EXPR, SImode, cxt,
            			 size_int (10), 0, 1),
            	   GEN_INT (trunc_int_for_mode (0x05000000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (8, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      ir_func = dump_ir_expr (func, MAP_FOR_VALUE);
      t = build_ir_triple (IR_AND, ir_func, build_ir_int_const (0x3ff, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x81c06000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 8)),
         expand_binop (SImode, ior_optab,
            	   expand_and (SImode, fnaddr, GEN_INT (0x3ff), NULL_RTX),
            	   GEN_INT (trunc_int_for_mode (0x81c06000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
      ir_tramp = dump_ir_expr (tramp, MAP_FOR_VALUE);
      ir_tramp = build_ir_triple (IR_PLUS, ir_tramp, build_ir_int_const (12, uinttype, 0), 
                                  ir_tramp->operand.type, 0);
      ir_ctx = dump_ir_expr (ctx, MAP_FOR_VALUE);
      t = build_ir_triple (IR_AND, ir_ctx, build_ir_int_const (0x3ff, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_OR, t, build_ir_int_const (0x8410a000, uinttype, 0), uinttype, 0);
      t = build_ir_triple (IR_ISTORE, ir_tramp, t, uinttype, ir_uinttype);
     /*  emit_move_insn
        (gen_rtx_MEM (SImode, plus_constant (tramp, 12)),
         expand_binop (SImode, ior_optab,
            	   expand_and (SImode, cxt, GEN_INT (0x3ff), NULL_RTX),
            	   GEN_INT (trunc_int_for_mode (0x8410a000, SImode)),
            	   NULL_RTX, 1, OPTAB_DIRECT));*/
    
      /* On UltraSPARC a flush flushes an entire cache line.  The trampoline is
         aligned on a 16 byte boundary so one flush clears it all.  */
     /*  emit_insn (gen_flush (validize_mem (gen_rtx_MEM (SImode, tramp))));*/
      func = build_decl (FUNCTION_DECL, get_identifier ("__enable_execute_stack"), /* func name */
                       build_function_type (void_type_node, /* return type */
                         build_tree_list (NULL_TREE, ptr_type_node))); /* arg1 type */
      DECL_ARTIFICIAL (func) = 1;
      DECL_EXTERNAL (func) = 1;
      TREE_PUBLIC (func) = 1;
      TREE_NOTHROW (func) = 1;

      dump_ir_call (gimple_build_call (func, 1, tramp), 0);
    } 
  return 0;
}

#endif /* TARGET_CPU_x86 */



static void
dump_ir_builtin_va_end (tree arglist) 
{   
  tree valist = TREE_VALUE (arglist);
    
  /* Evaluate for side effects, if needed. */
  if (TREE_SIDE_EFFECTS (valist))
    dump_ir_expr (valist, MAP_FOR_VALUE);
}   

/* Make it easier for the backends by protecting the valist argument
   from multiple evaluations.  */

static tree
stabilize_va_list (tree valist, int needs_lvalue)
{
  tree vatype = targetm.canonical_va_list_type (TREE_TYPE (valist));

  gcc_assert (vatype != NULL_TREE);

  if (TREE_CODE (vatype) == ARRAY_TYPE)
    {
      /* should never be the case for SPARC */

      /* For this case, the backends will be expecting a pointer to
         TREE_TYPE (va_list_type_node), but it's possible we've
         actually been given an array (an actual va_list_type_node).
         So fix it.  */
      if (TREE_CODE (TREE_TYPE (valist)) == ARRAY_TYPE)
        {
          tree p1 = build_pointer_type (TREE_TYPE (vatype));
          valist = build_fold_addr_expr_with_type (valist, p1);
        }
    }
  else
    {

      if (! needs_lvalue)
	return valist;

      valist = fold (build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (valist)),
			     valist));
    }

  return valist;
}

/* Expand ARGLIST, from a call to __builtin_va_copy.  We do this as a
   builtin rather than just as an assignment in stdarg.h because of the
   nastiness of array-type va_list types.  */

static void
dump_ir_builtin_va_copy (tree arglist)
{
  tree dst, src;
  gimple t;

  dst = TREE_VALUE (arglist);
  src = TREE_VALUE (TREE_CHAIN (arglist));

  dst = stabilize_va_list (dst, 1);
  src = stabilize_va_list (src, 0);

  if (TREE_CODE (targetm.fn_abi_va_list (cfun->decl)) != ARRAY_TYPE)
    {
      t = gimple_build_assign (dst, src);
      dump_ir_stmt (t);
    }
  else
    {
      tree fn, len;
      /* should never be the case for SPARC */

      /* copy. */
      gcc_assert (TREE_CODE (TREE_TYPE (dst)) == POINTER_TYPE
		 || TREE_CODE (TREE_TYPE (src)) == POINTER_TYPE);

      len = TYPE_SIZE_UNIT (targetm.fn_abi_va_list (cfun->decl));
      fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
      t = gimple_build_call (fn, 3, dst, src, len);
      dump_ir_stmt (t);
    }
}

/* Expand ARGLIST, from a call to __builtin_next_arg.  */

static tree
dump_ir_builtin_next_arg (void)
{
  return fold (build1 (ADDR_EXPR, build_pointer_type (va_list_type_node),
                       __builtin_va_alist_node));
}

/* Expand ARGLIST, from a call to __builtin_va_start.  */

static void
dump_ir_builtin_va_start (gimple exp)
{
  tree nextarg;
  tree valist;
  gimple t;

  if (gimple_call_num_args (exp) < 2)
    {
      error ("too few arguments to function %<va_start%>");
      return;
    }

#ifdef TARGET_CPU_x86
  if (TARGET_ARCH64)
    {
      gimple_call_set_arg (exp, 1, integer_zero_node);
      /* __builtin_sysv_va_start may perform the same as va_start. */
      if (strcmp (IDENTIFIER_POINTER (DECL_NAME (gimple_call_fndecl (exp))), "__builtin_va_start"))
        SET_DECL_ASSEMBLER_NAME (gimple_call_fndecl (exp), get_identifier ("__builtin_va_start"));
      (void) dump_ir_call (exp, 0);
      return;
    }
#endif

  nextarg = dump_ir_builtin_next_arg ();
  valist = stabilize_va_list (gimple_call_arg (exp,0), 1);

  t = gimple_build_assign (valist, nextarg);

  dump_ir_stmt (t);
}

static void
dump_ir_builtin_nonlocal_goto (gimple stmt ATTRIBUTE_UNUSED, tree arglist)
{
  tree nl_goto_target = TREE_VALUE (arglist);
  tree nl_save_area = TREE_VALUE (TREE_CHAIN (arglist));
  IR_NODE *n, *fp, *sp, *target, *clobber;
  TRIPLE *args = NULL;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);
  IR_TYPE_NODE * ir_argtype = map_gnu_type_to_IR_TYPE_NODE (ptr_type_node);
  
  if (TREE_CODE (TREE_TYPE (nl_goto_target)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (nl_save_area)) != POINTER_TYPE)
    abort ();
 
  n = dump_ir_expr (nl_goto_target, MAP_FOR_ADDR);
  target = get_tmp_leaf (argtype, ir_argtype);
  build_ir_triple (IR_ASSIGN, target, n, n->operand.type, NULL);
  

  n = dump_ir_expr (nl_save_area, MAP_FOR_ADDR);
#ifdef TARGET_CPU_sparc
  if (TARGET_ARCH64)
    n = build_ir_triple (IR_PLUS, n, build_ir_int_const (8, offsettype, 0), n->operand.type, 0);
  else
    n = build_ir_triple (IR_PLUS, n, build_ir_int_const (4, offsettype, 0), n->operand.type, 0);
#endif
  n = build_ir_triple (IR_IFETCH, n, NULL, argtype, ir_argtype);
  fp = get_tmp_leaf (argtype, ir_argtype);
  build_ir_triple (IR_ASSIGN, fp, n, n->operand.type, NULL);

  n = build_ir_triple (IR_ASM_INPUT, target, build_ir_string_const ("r"), argtype, NULL);
  n->triple.param_mode = PM_IN;
  TAPPEND (args, (TRIPLE *)n);
  
  n = build_ir_triple (IR_ASM_INPUT, fp, build_ir_string_const ("r"), argtype, NULL);
  n->triple.param_mode = PM_IN;
  TAPPEND (args, (TRIPLE *)n);
  
#ifdef TARGET_CPU_x86
  n = dump_ir_expr (nl_save_area, MAP_FOR_ADDR);
  n = build_ir_triple (IR_PLUS, n, build_ir_int_const (4, offsettype, 0), n->operand.type, 0);
  n = build_ir_triple (IR_IFETCH, n, NULL, argtype, ir_argtype);
  sp = get_tmp_leaf (argtype, ir_argtype);
  build_ir_triple (IR_ASSIGN, sp, n, n->operand.type, NULL);
  n = build_ir_triple (IR_ASM_INPUT, sp, build_ir_string_const ("r"), argtype, NULL);
  n->triple.param_mode = PM_IN;
  TAPPEND (args, (TRIPLE *)n);
#endif
  
  clobber = build_ir_triple (IR_ASM_CLOBBER, build_ir_string_const ("memory"), NULL, argtype, NULL);
  clobber->triple.is_volatile = IR_TRUE;
  TAPPEND (args, (TRIPLE *)clobber);

#ifdef TARGET_CPU_x86
  if (TARGET_ARCH64)
    n = build_ir_triple (IR_ASM_STMT, 
                         build_ir_string_const (
                                                "movq\t%1,%%rbp\n"
                                                "movq\t%2,%%rsp\n"
                                                "movq\t%0,%%rdx\n\tjmp\t*%%rdx\n\t nop"), 
                         (IR_NODE*)args, argtype, NULL);
  else
    n = build_ir_triple (IR_ASM_STMT, 
                         build_ir_string_const (
                                                "movl\t%1,%%ebp\n"
                                                "movl\t%2,%%esp\n"
                                                "movl\t%0,%%edx\n\tjmp\t*%%edx\n\t nop"), 
                         (IR_NODE*)args, argtype, NULL);
#else   
  if (TARGET_ARCH64)
    n = build_ir_triple (IR_ASM_STMT, 
                         build_ir_string_const ("flushw\n"
                                                "mov\t%1,%%fp\n"
                                                "mov\t%0,%%g1\n\trestore\n\tjmp\t%%g1\n\t nop"), 
                         (IR_NODE*)args, argtype, NULL);
  else
    n = build_ir_triple (IR_ASM_STMT, 
                         build_ir_string_const ("ta\t3\n"
                                                "mov\t%1,%%fp\n"
                                                "mov\t%0,%%g1\n\trestore\n\tjmp\t%%g1\n\t nop"), 
                         (IR_NODE*)args, argtype, NULL);
#endif
  clobber->triple.right = n;
  n->triple.is_volatile = IR_TRUE;
  /* I can see no effect of this assignment.
  crtl->has_nonlocal_goto = 1;*/
}

static IR_NODE *
dump_ir_builtin_expect (gimple stmt, tree arglist, int need_return)
{
  tree exp, c;
  IR_NODE * ret;

  if (arglist == NULL_TREE
      || TREE_CHAIN (arglist) == NULL_TREE)
    return dump_ir_call (stmt, need_return);

  exp = TREE_VALUE (arglist);
  c = TREE_VALUE (TREE_CHAIN (arglist));

  if (TREE_CODE (c) != INTEGER_CST)
    {
      error ("second arg to %<__builtin_expect%> must be a constant");
      c = integer_zero_node;
    }

  ret = dump_ir_expr (exp, MAP_FOR_VALUE);
  
  if (flag_guess_branch_prob)
    {
      /* pass branch "expectation" (integer_cst c) to cg somehow */
    }
  return ret;
}

static IR_NODE *
dump_ir_builtin_trap (gimple stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *n, *clobber;
  TRIPLE *args = NULL;
  TYPE argtype = map_gnu_type_to_TYPE (ptr_type_node);

  clobber = build_ir_triple (IR_ASM_CLOBBER, build_ir_string_const ("memory"), NULL, argtype, NULL);
  clobber->triple.is_volatile = IR_TRUE;
  TAPPEND (args, (TRIPLE *)clobber);

#ifdef TARGET_CPU_x86
  n = build_ir_triple (IR_ASM_STMT, build_ir_string_const (".value\t0x0b0f"), (IR_NODE*)args, argtype, NULL);
#else
  n = build_ir_triple (IR_ASM_STMT, build_ir_string_const ("ta\t5"), (IR_NODE*)args, argtype, NULL);
#endif
  clobber->triple.right = n;
  n->triple.is_volatile = IR_TRUE;
  return n;
}

static IR_NODE *
dump_ir_builtin_profile_func (int is_enter)
{
  gimple stmt;
  tree var, fn, ptr_to_cfun;
  IR_NODE * ret;

  stmt = gimple_build_call (built_in_decls[BUILT_IN_RETURN_ADDRESS], 
                            1, integer_zero_node); 
  var = create_tmp_var_raw (TREE_TYPE (TREE_TYPE (built_in_decls[BUILT_IN_RETURN_ADDRESS])), NULL);
  gimple_call_set_lhs (stmt, var);
  dump_ir_stmt (stmt);

  fn = build_decl (FUNCTION_DECL, get_identifier (is_enter 
                                                  ? "__cyg_profile_func_enter"
                                                  : "__cyg_profile_func_exit"),
                   build_function_type (integer_type_node, NULL_TREE));
  DECL_ARTIFICIAL (fn) = 1;
  DECL_EXTERNAL (fn) = 1;
  TREE_PUBLIC (fn) = 1;
  TREE_NOTHROW (fn) = 1;

  ptr_to_cfun = build1 (ADDR_EXPR, 
                        build_pointer_type (TREE_TYPE (current_function_decl)), 
                        current_function_decl);

  stmt = gimple_build_call (fn, 2, ptr_to_cfun, var);
  ret = dump_ir_call (stmt, 0);
  return ret;
}

/* from builtins.c */
/* Return a char pointer for a C string if it is a string constant
   or sum of string constant and integer constant.  */
const char * c_getstr (tree src);

/* from builtins.c */
/* Create a new constant string literal and return a char* pointer to it.
   The STRING_CST value is the LEN characters at STR.  */
tree build_string_literal (int len, const char *str);

/* Expand a call to printf or printf_unlocked with argument list ARGLIST.
   UNLOCKED indicates this is a printf_unlocked call.  */
static IR_NODE *
dump_builtin_printf (gimple stmt, tree arglist, int need_return, bool unlocked)
{
  tree fn_putchar = unlocked ? built_in_decls[BUILT_IN_PUTCHAR_UNLOCKED]
		             : implicit_built_in_decls[BUILT_IN_PUTCHAR];
  tree fn_puts = unlocked ? built_in_decls[BUILT_IN_PUTS_UNLOCKED]
			  : implicit_built_in_decls[BUILT_IN_PUTS];
  const char *fmt_str;
  tree fn, fmt, arg, t;

  /* If the return value is used, don't do the transformation.  */
  if (need_return)
    return dump_ir_call (stmt, need_return);

  /* Verify the required arguments in the original call.  */
  if (! arglist)
    return dump_ir_call (stmt, need_return);
  fmt = TREE_VALUE (arglist);
  if (TREE_CODE (TREE_TYPE (fmt)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  arglist = TREE_CHAIN (arglist);

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return dump_ir_call (stmt, need_return);

  /* If the format specifier was "%s\n", call __builtin_puts(arg).  */
  if (strcmp (fmt_str, "%s\n") == 0)
    {
      if (! arglist
          || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      fn = fn_puts;
    }
  /* If the format specifier was "%c", call __builtin_putchar(arg).  */
  else if (strcmp (fmt_str, "%c") == 0)
    {
      if (! arglist
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      fn = fn_putchar;
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (fmt_str, '%'))
        return dump_ir_call (stmt, need_return);

      if (arglist)
        return dump_ir_call (stmt, need_return);

      /* If the format specifier was "", printf does nothing.  */
      if (fmt_str[0] == '\0')
	return 0;
      
      /* If the format specifier has length of 1, call putchar.  */
      if (fmt_str[1] == '\0')
	{
	  /* Given printf("c"), (where c is any one character,)
	     convert "c"[0] to an int and pass that to the replacement
	     function.  */
	  arg = build_int_cst (NULL_TREE, fmt_str[0]);
	  arglist = build_tree_list (NULL_TREE, arg);
	  fn = fn_putchar;
	}
      else
	{
	  /* If the format specifier was "string\n", call puts("string").  */
	  size_t len = strlen (fmt_str);
	  if (fmt_str[len - 1] == '\n')
	    {
	      /* Create a NUL-terminated string that's one char shorter
		 than the original, stripping off the trailing '\n'.  */
	      char *newstr = (char *) alloca (len);
	      memcpy (newstr, fmt_str, len - 1);
	      newstr[len - 1] = 0;

	      arg = build_string_literal (len, newstr);
	      arglist = build_tree_list (NULL_TREE, arg);
	      fn = fn_puts;
	    }
	  else
	    /* We'd like to arrange to call fputs(string,stdout) here,
	       but we need stdout and don't have a way to get it yet.  */
            return dump_ir_call (stmt, need_return);
	}
    }

  if (!fn)
    return dump_ir_call (stmt, need_return);
  t = build_function_call_expr (fn, arglist);
  dump_ir_stmt (gimple_build_call_from_tree (t));
  return 0;
}

/* Expand a call to fprintf or fprintf_unlocked with argument list ARGLIST.
   UNLOCKED indicates this is a fprintf_unlocked call.  */

static IR_NODE *
dump_builtin_fprintf (gimple stmt, tree arglist, int need_return, bool unlocked)
{
  tree fn_fputc = unlocked ? built_in_decls[BUILT_IN_FPUTC_UNLOCKED]
			   : implicit_built_in_decls[BUILT_IN_FPUTC];
  tree fn_fputs = unlocked ? built_in_decls[BUILT_IN_FPUTS_UNLOCKED]
			   : implicit_built_in_decls[BUILT_IN_FPUTS];
  const char *fmt_str;
  tree fn, fmt, fp, arg, t;

  /* If the return value is used, don't do the transformation.  */
  if (need_return)
    return dump_ir_call (stmt, need_return);

  /* Verify the required arguments in the original call.  */
  if (! arglist)
    return 0;
  fp = TREE_VALUE (arglist);
  if (TREE_CODE (TREE_TYPE (fp)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  arglist = TREE_CHAIN (arglist);
  if (! arglist)
    return dump_ir_call (stmt, need_return);
  fmt = TREE_VALUE (arglist);
  if (TREE_CODE (TREE_TYPE (fmt)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  arglist = TREE_CHAIN (arglist);

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return dump_ir_call (stmt, need_return);

  /* If the format specifier was "%s", call __builtin_fputs(arg,fp).  */
  if (strcmp (fmt_str, "%s") == 0)
    {
      if (! arglist
          || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != POINTER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      arg = TREE_VALUE (arglist);
      arglist = build_tree_list (NULL_TREE, fp);
      arglist = tree_cons (NULL_TREE, arg, arglist);
      fn = fn_fputs;
    }
  /* If the format specifier was "%c", call __builtin_fputc(arg,fp).  */
  else if (strcmp (fmt_str, "%c") == 0)
    {
      if (! arglist
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE
	  || TREE_CHAIN (arglist))
        return dump_ir_call (stmt, need_return);
      arg = TREE_VALUE (arglist);
      arglist = build_tree_list (NULL_TREE, fp);
      arglist = tree_cons (NULL_TREE, arg, arglist);
      fn = fn_fputc;
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (fmt_str, '%'))
        return dump_ir_call (stmt, need_return);

      if (arglist)
        return dump_ir_call (stmt, need_return);

      /* If the format specifier was "", fprintf does nothing.  */
      if (fmt_str[0] == '\0')
	{
	  /* Evaluate and ignore FILE* argument for side-effects.  */
	  dump_ir_expr (fp, MAP_FOR_VALUE);
	  return 0;
	}

      /* When "string" doesn't contain %, replace all cases of
	 fprintf(stream,string) with fputs(string,stream).  The fputs
	 builtin will take care of special cases like length == 1.  */
      arglist = build_tree_list (NULL_TREE, fp);
      arglist = tree_cons (NULL_TREE, fmt, arglist);
      fn = fn_fputs;
    }

  if (!fn)
    return dump_ir_call (stmt, need_return);
  t = build_function_call_expr (fn, arglist); 
  dump_ir_stmt (gimple_build_call_from_tree (t));
  return 0;
}

static IR_NODE *
dump_builtin_memset (gimple stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree val = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  IR_NODE *ret, *ir_val;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (val)) != INTEGER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      /* Evaluate and ignore VAL in case it has side-effects.  */
      if (TREE_SIDE_EFFECTS (val)/* || TREE_CODE (val) != VAR_DECL*/)
        {
          ir_val = dump_ir_expr (val, MAP_FOR_VALUE);
          /* generate FOREFF only if 'val' is not leaf. 
             iropt will take care of the rest */
          if (ir_val->operand.tag != ISLEAF)
            ret = build_ir_triple (IR_FOREFF, ir_val, NULL, ir_val->operand.type, NULL);
        }
      return dump_ir_expr (dest, MAP_FOR_VALUE);
    }
  
  if (!host_integerp (len, 1) || tree_low_cst (len, 1) > 1)
    {
      return dump_ir_call (stmt, need_return);
    }
 
  /* remove extra pointer conversion to make sure that 'dest'
     will be converted to leaf */
  STRIP_NOPS (dest); 
                        
  ret = dump_ir_expr (dest, MAP_FOR_VALUE);
  ir_val = dump_ir_expr (val, MAP_FOR_VALUE);
  ir_val = build_ir_triple (IR_CONV, ir_val, 0, 
                       map_gnu_type_to_TYPE (intQI_type_node),
                       map_gnu_type_to_IR_TYPE_NODE (intQI_type_node));

  if (ret->operand.tag != ISLEAF && need_return)
    {
      IR_NODE * var = get_tmp_leaf (ret->operand.type, 
                                    map_gnu_type_to_IR_TYPE_NODE (TREE_TYPE (dest)));
      build_ir_triple (IR_ASSIGN, var, ret, ret->operand.type, NULL);
      ret = var;
    }
  
  build_ir_triple (IR_ISTORE, ret, ir_val, 
              map_gnu_type_to_TYPE (intQI_type_node),
              map_gnu_type_to_IR_TYPE_NODE (intQI_type_node));
  
  return ret;
}

bool readonly_data_expr (tree);

static IR_NODE*
dump_builtin_memcpy (gimple stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree src = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  gimple result;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);

  if (integer_onep (len))
    { /* memcpy (dest, src, 1) */
      tree new_dest = build1 (INDIRECT_REF, char_type_node, 
                     build1 (NOP_EXPR, ptr_type_node, dest));
      src = build1 (INDIRECT_REF, char_type_node, 
                     build1 (NOP_EXPR, const_ptr_type_node, src));
      result = gimple_build_assign (new_dest, src);
      dump_ir_stmt (result);
      return dump_ir_expr (dest, MAP_FOR_VALUE);
    }
  
  return dump_ir_call (stmt, need_return);
}

/* Expand expression EXP, which is a call to the strcpy builtin. */
static IR_NODE*
dump_builtin_strcpy (gimple stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree src = TREE_VALUE (TREE_CHAIN (arglist));
  tree len;
  tree fn;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE)
    return dump_ir_call (stmt, need_return);
  
  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (operand_equal_p (src, dest, 0))
    return dump_ir_expr (fold_convert (gimple_call_return_type (stmt), dest), MAP_FOR_VALUE);

  if (optimize_size)
    return dump_ir_call (stmt, need_return);

  fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
  if (!fn)
    return dump_ir_call (stmt, need_return);
  
  len = c_strlen (src, 1);
  if (! len || TREE_SIDE_EFFECTS (len))
    return dump_ir_call (stmt, need_return);

  len = size_binop (PLUS_EXPR, len, ssize_int (1));
  arglist = build_tree_list (NULL_TREE, len);
  arglist = tree_cons (NULL_TREE, src, arglist);
  arglist = tree_cons (NULL_TREE, dest, arglist);
  return dump_ir_expr (fold_convert (gimple_call_return_type (stmt),
		       build_function_call_expr (fn, arglist)), need_return);
}

/* Expand expression EXP, which is a call to the memmove builtin. */
static IR_NODE*
dump_builtin_memmove (gimple stmt, tree arglist, int need_return)
{
  tree dest = TREE_VALUE (arglist);
  tree src = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);
  
  /* If src is categorized for a readonly section we can use
     normal memcpy.  */
  if (readonly_data_expr (src))
    {
      tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
      gimple fn_gimple;
      if (fn)
        {
          fn = build_function_call_expr (fn, arglist);
          if (TREE_CODE (fn) == CALL_EXPR)
            {
              fn_gimple = gimple_build_call_from_tree (fn);
              gimple_call_set_tail (fn_gimple, gimple_call_tail_p (stmt));
              return dump_builtin_memcpy (fn_gimple, arglist, need_return);
            }
          else if (need_return || TREE_SIDE_EFFECTS (fn))
            {
              IR_NODE * ret = dump_ir_expr (fn, MAP_FOR_VALUE);
              
              if (need_return) /* expr will be used somewhere */
                return ret;
              
              /* generate FOREFF to make sure we don't have zombie triples */
              if (ret->operand.tag != ISLEAF)
                build_ir_triple (IR_FOREFF, ret, NULL, ret->operand.type, NULL);
              
              return 0;
            }
          else
            return 0;
        }
    }

  /* If length is 1 and we can expand memcpy call inline,
     it is ok to use mempcpy as well.  */
  if (integer_onep (len))
    {
      return dump_builtin_memcpy (stmt, arglist, need_return);
    }
  
  if (integer_zerop (len) && !need_return)
    /* can be seen in gcc.c-torture/execute/builtins/lib/memmove.c */
    return 0;

  /* Otherwise, call the normal function.  */
  return dump_ir_call (stmt, need_return);
}

/* Expand expression EXP, which is a call to the bcopy builtin. */
static IR_NODE*
dump_builtin_bcopy (gimple stmt, tree arglist, int need_return)
{
  tree src = TREE_VALUE (arglist);
  tree dest = TREE_VALUE (TREE_CHAIN (arglist));
  tree len = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
  tree newarglist;

  if (TREE_CODE (TREE_TYPE (dest)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (src)) != POINTER_TYPE
      || TREE_CODE (TREE_TYPE (len)) != INTEGER_TYPE)
    return dump_ir_call (stmt, need_return);
  
  /* New argument list transforming bcopy(ptr x, ptr y, int z) to
     memmove(ptr y, ptr x, size_t z).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bcopy instead of memmove.  */

  newarglist = build_tree_list (NULL_TREE, fold_convert (sizetype, len));
  newarglist = tree_cons (NULL_TREE, src, newarglist);
  newarglist = tree_cons (NULL_TREE, dest, newarglist);

  return dump_builtin_memmove (stmt, newarglist, need_return);
}

static IR_NODE *
dump_builtin_eh_return_data_regno (tree arglist)
{
  tree which = TREE_VALUE (arglist);
  unsigned HOST_WIDE_INT iwhich;

  if (TREE_CODE (which) != INTEGER_CST)
    {
      error ("argument of %<__builtin_eh_return_regno%> must be constant");
      return build_ir_int_const (-1, inttype, 0);
    }

  iwhich = tree_low_cst (which, 1);
  iwhich = EH_RETURN_DATA_REGNO (iwhich);
  if (iwhich == INVALID_REGNUM)
    return build_ir_int_const (-1, inttype, 0);

#ifdef DWARF_FRAME_REGNUM
  iwhich = DWARF_FRAME_REGNUM (iwhich);
#else
  iwhich = DBX_REGISTER_NUMBER (iwhich);
#endif

  return build_ir_int_const (iwhich, inttype, 0);
}

/* lower lfloor builtin, into expression to (int)(floor(x))  */
static IR_NODE *
dump_ir_builtin_int_roundingfn (gimple stmt, int need_return)
{
  tree fndecl = gimple_call_fndecl (stmt);
  tree arglist = gimple_call_arglist (stmt);
  enum built_in_function fallback_fn;
  tree fallback_fndecl;
  tree arg, exp;
  IR_NODE * ret;

  /* no need to validate arglist */

  arg = TREE_VALUE (arglist);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
      fallback_fn = BUILT_IN_CEIL;
      break;

    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      fallback_fn = BUILT_IN_FLOOR;
      break;

    default:
      gcc_unreachable ();
    }

  /* Fall back to floating point rounding optab.  */
  fallback_fndecl = mathfn_built_in (TREE_TYPE (arg), fallback_fn);
  gcc_assert (fallback_fndecl != NULL_TREE);
  exp = build_function_call_expr (fallback_fndecl, arglist);

  exp = build1 (NOP_EXPR, gimple_call_return_type (stmt), exp);
  ret = dump_ir_expr (exp, MAP_FOR_VALUE);
  if (need_return)
    return ret;
  else
    return build_ir_triple (IR_FOREFF, ret, NULL, ret->operand.type, NULL);
}

/* map built-in function tree opcode into IR opcode */
static IR_OP
conv_c99_treecode2ir (tree node)
{
  switch (DECL_FUNCTION_CODE (node))
    {
    CASE_FLT_FN (BUILT_IN_SIGNBIT):
      return IR_SIGNBIT;
    CASE_FLT_FN (BUILT_IN_ISINF):
      return IR_ISINF;
    CASE_FLT_FN (BUILT_IN_ISNAN):
    case BUILT_IN_ISNAND32:
    case BUILT_IN_ISNAND64:
    case BUILT_IN_ISNAND128:
      return IR_ISNAN;
    CASE_FLT_FN (BUILT_IN_ISFINITE):
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
      return IR_ISFINITE;
    CASE_FLT_FN (BUILT_IN_ISNORMAL):
      return IR_ISNORMAL;
    CASE_FLT_FN (BUILT_IN_FPCLASSIFY):
      return IR_FPCLASSIFY;
    default:
      debug_tree (node);
      abort ();
      return IR_ERR;
    }
}

/* We could map the builtin's below to appropriate call in atomics.h on S10. 
   If can't, return NULL and later use inline templates. */
static const char *
map_sync2solaris_fname (enum built_in_function fcode)
{
  switch (fcode)
    {
    case BUILT_IN_ADD_AND_FETCH_N:
      return "atomic_add_32_nv";
    case BUILT_IN_ADD_AND_FETCH_1:
      return "atomic_add_8_nv";
    case BUILT_IN_ADD_AND_FETCH_2:
      return "atomic_add_16_nv";
    case BUILT_IN_ADD_AND_FETCH_4:
      return "atomic_add_32_nv";
    case BUILT_IN_ADD_AND_FETCH_8:
      return "atomic_add_64_nv";
    case BUILT_IN_OR_AND_FETCH_N:
      return "atomic_or_32_nv";
    case BUILT_IN_OR_AND_FETCH_1:
      return "atomic_or_8_nv";
    case BUILT_IN_OR_AND_FETCH_2:
      return "atomic_or_16_nv";
    case BUILT_IN_OR_AND_FETCH_4:
      return "atomic_or_32_nv";
    case BUILT_IN_OR_AND_FETCH_8:
      return "atomic_or_64_nv";
    case BUILT_IN_AND_AND_FETCH_N:
      return "atomic_and_32_nv";
    case BUILT_IN_AND_AND_FETCH_1:
      return "atomic_and_8_nv";
    case BUILT_IN_AND_AND_FETCH_2:
      return "atomic_and_16_nv";
    case BUILT_IN_AND_AND_FETCH_4:
      return "atomic_and_32_nv";
    case BUILT_IN_AND_AND_FETCH_8:
      return "atomic_and_64_nv";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_N:
      return "atomic_cas_32";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
      return "atomic_cas_8";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
      return "atomic_cas_16";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_4:
      return "atomic_cas_32";
    case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
      return "atomic_cas_64";
    case BUILT_IN_LOCK_TEST_AND_SET_N:
      return "atomic_swap_32";
    case BUILT_IN_LOCK_TEST_AND_SET_1:
      return "atomic_swap_8";
    case BUILT_IN_LOCK_TEST_AND_SET_2:
      return "atomic_swap_16";
    case BUILT_IN_LOCK_TEST_AND_SET_4:
      return "atomic_swap_32";
    case BUILT_IN_LOCK_TEST_AND_SET_8:
      return "atomic_swap_64";
    default: 
      return NULL;
    }
}

/* Emit warning if a free is called with address of a variable.  */

static void
maybe_emit_free_warning (gimple stmt)
{
  tree arg = gimple_call_arg (stmt, 0);
  location_t loc = gimple_location (stmt);

  STRIP_NOPS (arg);
  if (TREE_CODE (arg) != ADDR_EXPR)
    return;

  arg = get_base_address (TREE_OPERAND (arg, 0));
  if (arg == NULL || INDIRECT_REF_P (arg))
    return;

  if (SSA_VAR_P (arg))
    warning_at (loc, 
                0, "%Hattempt to free a non-heap object %qD", &loc, arg);
  else
    warning_at (loc, 
                0, "%Hattempt to free a non-heap object", &loc);
}

static void
warn_if_sync_nand_changed (enum built_in_function fcode)
{
  static bool warned_f_a_n, warned_n_a_f;
  tree fndecl;

  switch (fcode)
    {
    case BUILT_IN_FETCH_AND_NAND_N:
    case BUILT_IN_FETCH_AND_NAND_1:
    case BUILT_IN_FETCH_AND_NAND_2:
    case BUILT_IN_FETCH_AND_NAND_4:
    case BUILT_IN_FETCH_AND_NAND_8:
    case BUILT_IN_FETCH_AND_NAND_16:
      if (warned_f_a_n)
        break;

      fndecl = implicit_built_in_decls[BUILT_IN_FETCH_AND_NAND_N];
      inform (input_location,
                  "%qD changed semantics in GCC 4.4", fndecl);
      warned_f_a_n = true;
      break;

    case BUILT_IN_NAND_AND_FETCH_N:
    case BUILT_IN_NAND_AND_FETCH_1:
    case BUILT_IN_NAND_AND_FETCH_2:
    case BUILT_IN_NAND_AND_FETCH_4:
    case BUILT_IN_NAND_AND_FETCH_8:
    case BUILT_IN_NAND_AND_FETCH_16:
      if (warned_n_a_f)
        break;

      fndecl = implicit_built_in_decls[BUILT_IN_NAND_AND_FETCH_N];
      inform (input_location,
                  "%qD changed semantics in GCC 4.4", fndecl);
      warned_n_a_f = true;
      break;

    default:
      break;
    }
}

/* Convert call to BUILTIN_SYNC to pragma
   OMP FLUSH. The argument list contains
   the list of variables to be flushed. */

static void
dump_omp_flush (gimple stmt)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;
  PRAGMAINFO *pinfo;
  tree op1;
  LIST *lp;
  IR_NODE *var;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_FLUSH,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);

  pinfo = build_ir_pragmainfo ();
  pinfo->type = OMP_E_FLUSH;

  pinfo->begin_triple = (TRIPLE *) pragma_tp;
  pinfo->begin_lineno = ir_location.line;
  pinfo->end_triple = (TRIPLE *) pragma_tp;
  pinfo->end_lineno = ir_location.line;
  pinfo->filename = (LEAF *) build_ir_string_const (ir_location.file);

  op1 = gimple_call_arglist (stmt); /* list of arguments */
  for (; op1 != NULL_TREE; op1 = TREE_CHAIN (op1))
    {
      var = dump_ir_expr (TREE_VALUE (op1), MAP_FOR_VALUE);
      gcc_assert (var->operand.tag == ISLEAF);
      lp = build_ir_proc_list ();
      lp->datap = (LDATA *) var;
      LAPPEND (pinfo->u.flush, lp);
    }
}

/* Convert the GOMP builtin function call to a
   pragma expected by the IR backend. Generate
   IR_pragma OMP_BARRIER */

static void
dump_omp_barrier (gimple stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_BARRIER,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

/* Convert the GOMP builtin atomic start to the pragma
   for iropt. */

static void
dump_omp_atomic_start (gimple stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_ATOMIC_BEGIN,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

static void
dump_omp_atomic_end (gimple stmt ATTRIBUTE_UNUSED)
{
  IR_NODE *pragma_typ, *begin_lno, *pragma_tp;

  pragma_typ = build_ir_int_const ((CONSZ) OMP_ATOMIC_END,
                                   inttype, 0);
  begin_lno = build_ir_int_const ((CONSZ) ir_location.line,
                                  inttype, 0);
  pragma_tp = build_ir_triple (IR_PRAGMA,
                               pragma_typ,
                               begin_lno,
                               undeftype,
                               NULL);
}

IR_NODE *
dump_ir_builtin_call (gimple stmt, int need_return)
{
  tree fndecl = gimple_call_fndecl (stmt);
  tree arglist = gimple_call_arglist (stmt);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  IR_NODE * ret = 0;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    {
      return dump_ir_call (stmt, need_return);
    }

  switch (fcode)
    {
    CASE_FLT_FN (BUILT_IN_SIGNBIT):
    CASE_FLT_FN (BUILT_IN_ISINF):
    CASE_FLT_FN (BUILT_IN_ISNAN):
    CASE_FLT_FN (BUILT_IN_ISFINITE):
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
    CASE_FLT_FN (BUILT_IN_ISNORMAL):
    CASE_FLT_FN (BUILT_IN_FPCLASSIFY):
      /* signbit, isinf, isnan, isfinite, isnormal, fpclassify are library micro defined in 'iso/math_c99.h'. */
      if (! arglist /* No args */
          || TREE_CHAIN (arglist) /* More than one arg */)
        return dump_ir_call (stmt, need_return);

      IR_NODE * ir_arg = dump_ir_expr (TREE_VALUE (arglist), MAP_FOR_VALUE);

      ret = build_ir_triple (conv_c99_treecode2ir (fndecl), ir_arg, NULL, inttype, 0);
      break;

    case BUILT_IN_CLZ:
      if (TARGET_ARCH64)
        {
          tree val = TREE_VALUE (arglist);
          TREE_VALUE (arglist) = build1 (NOP_EXPR, unsigned_intDI_type_node, val);
          ret = dump_ir_call (stmt, need_return);

          ret = build_ir_triple (IR_MINUS, ret, build_ir_int_const (32, ret->operand.type, 0), 
                                 ret->operand.type, 0);
          break;
        }
    case BUILT_IN_CTZ:
    case BUILT_IN_PARITY:
    case BUILT_IN_POPCOUNT:
      ret = dump_ir_call (stmt, need_return);
      break;
    case BUILT_IN_VA_START:
      dump_ir_builtin_va_start (stmt);
      break;
    case BUILT_IN_VA_END:
      dump_ir_builtin_va_end (arglist);
      break;
    case BUILT_IN_VA_COPY:
      dump_ir_builtin_va_copy (arglist);
      break;
    case BUILT_IN_STACK_SAVE:
#ifdef TARGET_CPU_sparc
      ret = get_ir_stack_pointer_reg ();
#elif TARGET_CPU_x86
      {
        TRIPLE * tp, * asm_args = NULL;
        IR_NODE * ir_string, * ir_clobber;
        tree decl = create_tmp_var_raw (ptr_type_node, NULL);

        TREE_USED (decl) = 1;
        DECL_ARTIFICIAL (decl) = 1;
        SET_DECL_ASSEMBLER_NAME (decl, DECL_NAME (decl));

        ret = dump_ir_expr (decl, MAP_FOR_VALUE);

        if (TARGET_ARCH64)
          ir_string = build_ir_string_const ("movq %rsp, %0");
        else
          ir_string = build_ir_string_const ("mov %esp, %0");

        ir_clobber = build_ir_string_const ("");
        tp = (TRIPLE*) build_ir_triple (IR_ASM_CLOBBER, ir_clobber, NULL,  
                                        ir_clobber->leaf.type, NULL);
        TAPPEND (asm_args, (TRIPLE *) tp);

        tp = (TRIPLE*) build_ir_triple (IR_ASM_OUTPUT, ret,
                                      build_ir_string_const ("=r"),
                                      ret->leaf.type, NULL);
        tp->param_mode = PM_OUT;
        TAPPEND (asm_args, (TRIPLE *) tp);

        (void) build_ir_triple (IR_ASM_STMT, ir_string, (IR_NODE*)asm_args,
                              ir_string->leaf.type, NULL);
      }
#endif
      if (cur_omp_context && (cur_omp_context->pinfo
          || (cur_omp_context->prev_ctx && cur_omp_context->prev_ctx->pinfo)))
        {
          LIST *lp;
          PRAGMAINFO *pinfo;
          pinfo = cur_omp_context->pinfo ? cur_omp_context->pinfo
                                         : cur_omp_context->prev_ctx->pinfo;
          lp = build_ir_proc_list ();
          lp->datap = (LDATA *) ret;
          LAPPEND (pinfo->u.s.omp_private, lp);
        }

      break;
    case BUILT_IN_STACK_RESTORE:
      {
        IR_NODE * var = dump_ir_expr (TREE_VALUE (arglist), MAP_FOR_VALUE);
#ifdef TARGET_CPU_sparc
        IR_NODE * sp_reg = get_ir_stack_pointer_reg ();
        ret = build_ir_triple (IR_ASSIGN, sp_reg, var, sp_reg->operand.type, NULL);
#elif TARGET_CPU_x86
        TRIPLE * tp, * asm_args = NULL;
        IR_NODE * ir_string, * ir_clobber;

        if (TARGET_ARCH64)
          ir_string = build_ir_string_const ("movq %0, %rsp");
        else
          ir_string = build_ir_string_const ("mov %0, %esp");

        ir_clobber = build_ir_string_const ("");
        tp = (TRIPLE*) build_ir_triple (IR_ASM_CLOBBER, ir_clobber, NULL,  
                                        ir_clobber->leaf.type, NULL);
        TAPPEND (asm_args, (TRIPLE *) tp);

        tp = (TRIPLE*) build_ir_triple (IR_ASM_INPUT, var,
                                      build_ir_string_const ("r"),
                                      var->leaf.type, NULL);
        tp->param_mode = PM_IN;
        TAPPEND (asm_args, (TRIPLE *) tp);

        ret = build_ir_triple (IR_ASM_STMT, ir_string, (IR_NODE*)asm_args,
                              ir_string->leaf.type, NULL);
#endif

        break;
      }
    case BUILT_IN_ADJUST_TRAMPOLINE:
      ret = dump_ir_expr (TREE_VALUE (arglist), MAP_FOR_VALUE);
      break;
    case BUILT_IN_INIT_TRAMPOLINE:
      ret = dump_ir_builtin_init_trampoline (stmt, arglist, need_return);
      break;
/*    case BUILT_IN_STACK_ALLOC:
      dump_ir_builtin_stack_alloc (arglist);
      break;*/
    case BUILT_IN_FRAME_ADDRESS:
    case BUILT_IN_RETURN_ADDRESS:
      ret = dump_ir_builtin_return_addr (stmt, fndecl, arglist, need_return);
      break;
    case BUILT_IN_NONLOCAL_GOTO:
      dump_ir_builtin_nonlocal_goto (stmt, arglist);
      break;
    case BUILT_IN_MEMSET:
      ret = dump_builtin_memset (stmt, arglist, need_return);
      break;
    case BUILT_IN_MEMMOVE:
      ret = dump_builtin_memmove (stmt, arglist, need_return);
      break;
    case BUILT_IN_STRCPY:
      ret = dump_builtin_strcpy (stmt, arglist, need_return);
      break;
    case BUILT_IN_BCOPY:
      ret = dump_builtin_bcopy (stmt, arglist, need_return);
      break;
    case BUILT_IN_FPRINTF:
      ret = dump_builtin_fprintf (stmt, arglist, need_return, 0);
      break;
    case BUILT_IN_FPRINTF_UNLOCKED:
      ret = dump_builtin_fprintf (stmt, arglist, need_return, 1);
      break;
    case BUILT_IN_PRINTF:
      ret = dump_builtin_printf (stmt, arglist, need_return, 0);
      break;
    case BUILT_IN_PRINTF_UNLOCKED:
      ret = dump_builtin_printf (stmt, arglist, need_return, 1);
      break;
    case BUILT_IN_STPCPY:
      {
        gimple new_stmt = stmt;

        if (!need_return)
          {
	    int i, nargs = gimple_call_num_args (stmt);
	    VEC(tree, heap) *vargs = VEC_alloc (tree, heap, nargs);

	    for ( i = 0; i < nargs; i++)
	      VEC_quick_push (tree, vargs, gimple_call_arg (stmt, i));

            tree fn = implicit_built_in_decls[BUILT_IN_STRCPY];
            if (fn)
 	      new_stmt = gimple_build_call_vec (fn, vargs);
	    VEC_free (tree, heap, vargs);
          }
        ret = dump_ir_call (new_stmt, need_return);
      }
      break;
    case BUILT_IN_MEMPCPY:
      {
        gimple new_stmt = stmt;
        if (!need_return)
          {
	    int i, nargs = gimple_call_num_args (stmt);
	    VEC(tree, heap) *vargs = VEC_alloc (tree, heap, nargs);

	    for ( i = 0; i < nargs; i++)
	      VEC_quick_push (tree, vargs, gimple_call_arg (stmt, i));

            tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
            if (fn)
              new_stmt = gimple_build_call_vec (fn, vargs);
	    VEC_free (tree, heap, vargs);
          }
        ret = dump_ir_call (new_stmt, need_return);
      }
      break;
    case BUILT_IN_EXPECT:
      ret = dump_ir_builtin_expect (stmt, arglist, need_return);
      break;
    case BUILT_IN_TRAP:
      ret = dump_ir_builtin_trap (stmt);
      break;
    case BUILT_IN_PROFILE_FUNC_ENTER:
      ret = dump_ir_builtin_profile_func (1);
      break;
    case BUILT_IN_PROFILE_FUNC_EXIT:
      ret = dump_ir_builtin_profile_func (0);
      break;
    case BUILT_IN_EH_RETURN_DATA_REGNO:
      ret = dump_builtin_eh_return_data_regno (arglist);
      break;
    case BUILT_IN_PREFETCH: 
      {
        tree arg0, arg1, arg2, t, fn;
        int op1, op2;
        const char * real_name;

        arg0 = TREE_VALUE (arglist);

        if (TREE_CODE (TREE_TYPE (arg0)) != POINTER_TYPE)
          return dump_ir_call (stmt, need_return);

        /* Arguments 1 and 2 are optional; argument 1 (read/write) defaults to
           zero (read) and argument 2 (locality) defaults to 3 (high degree of
           locality).  */
        if (TREE_CHAIN (arglist))
          {
            arg1 = TREE_VALUE (TREE_CHAIN (arglist));
            if (TREE_CHAIN (TREE_CHAIN (arglist)))
              arg2 = TREE_VALUE (TREE_CHAIN (TREE_CHAIN (arglist)));
            else
              arg2 = build_int_cst (NULL_TREE, 3);
          }
        else
          {
            arg1 = integer_zero_node;
            arg2 = build_int_cst (NULL_TREE, 3);
          }

        /* Argument 1 (read/write flag) must be a compile-time constant int.  */
        if (TREE_CODE (arg1) != INTEGER_CST)
          {
            error ("second argument to %<__builtin_prefetch%> must be a constant");
            arg1 = integer_zero_node;
          }
        
        op1 = TREE_INT_CST_LOW (arg1);
        /* Argument 1 must be either zero or one.  */
        if (op1 != 0 && op1 != 1)
          {
            warning (0, "invalid second argument to %<__builtin_prefetch%>; using zero");
            op1 = 0;
          }

        /* Argument 2 (locality) must be a compile-time constant int.  */
        if (TREE_CODE (arg2) != INTEGER_CST)
          {
            error ("third argument to %<__builtin_prefetch%> must be a constant");
            arg2 = integer_zero_node;
          }
        
        op2 = TREE_INT_CST_LOW (arg2);
        /* Argument 2 must be 0, 1, 2, or 3.  */
        if (op2 < 0 || op2 > 3)
          {
            warning (0, "invalid third argument to %<__builtin_prefetch%>; using zero");
            op2 = 0;
          }
#ifdef TARGET_CPU_x86
        if (op1 == 0) /* prefetch read */
          {
            if (op2 <=1)
              real_name = "__sun_x86_prefetch_read_once_intrinsic";
            else
              real_name = "__sun_x86_prefetch_read_many_intrinsic";
          }
        else /* prefetch write */
          {
            if (op2 <=1)
              real_name = "__sun_x86_prefetch_write_once_intrinsic";
            else
              real_name = "__sun_x86_prefetch_write_many_intrinsic";
          }
#else        
        if (op1 == 0) /* prefetch read */
          {
            if (op2 <=1)
              real_name = "__sparc_prefetch_read_once_intrinsic";
            else
              real_name = "__sparc_prefetch_read_many_intrinsic";
          }
        else /* prefetch write */
          {
            if (op2 <=1)
              real_name = "__sparc_prefetch_write_once_intrinsic";
            else
              real_name = "__sparc_prefetch_write_many_intrinsic";
          }
#endif        
        fn = build_decl (FUNCTION_DECL, get_identifier (real_name),
                         build_function_type (void_type_node, NULL_TREE));
        DECL_EXTERNAL (fn) = 1;
        TREE_PUBLIC (fn) = 1;
        DECL_ARTIFICIAL (fn) = 1;
        TREE_NOTHROW (fn) = 1;

        t = build_function_call_expr (fn, arglist);
        ret = dump_ir_call (gimple_build_call_from_tree (t), need_return);
      }
      break;

    case BUILT_IN_GOMP_BARRIER:
      dump_omp_barrier (stmt);
      break;

    case BUILT_IN_GOMP_ATOMIC_START:
      dump_omp_atomic_start (stmt);
      break;

    case BUILT_IN_GOMP_ATOMIC_END:
      dump_omp_atomic_end (stmt);
      break;

    case BUILT_IN_SYNCHRONIZE:
      dump_omp_flush (stmt);
      break;

    /* We could map all the builtin's below to
       appropriate call in atomics.h on S10. */
    case BUILT_IN_FETCH_AND_ADD_N:
    case BUILT_IN_FETCH_AND_ADD_1:
    case BUILT_IN_FETCH_AND_ADD_2:
    case BUILT_IN_FETCH_AND_ADD_4:
    case BUILT_IN_FETCH_AND_ADD_8:
    case BUILT_IN_FETCH_AND_ADD_16:
    case BUILT_IN_FETCH_AND_SUB_N:
    case BUILT_IN_FETCH_AND_SUB_1:
    case BUILT_IN_FETCH_AND_SUB_2:
    case BUILT_IN_FETCH_AND_SUB_4:
    case BUILT_IN_FETCH_AND_SUB_8:
    case BUILT_IN_FETCH_AND_SUB_16:
    case BUILT_IN_FETCH_AND_OR_N:
    case BUILT_IN_FETCH_AND_OR_1:
    case BUILT_IN_FETCH_AND_OR_2:
    case BUILT_IN_FETCH_AND_OR_4:
    case BUILT_IN_FETCH_AND_OR_8:
    case BUILT_IN_FETCH_AND_OR_16:
    case BUILT_IN_FETCH_AND_AND_N:
    case BUILT_IN_FETCH_AND_AND_1:
    case BUILT_IN_FETCH_AND_AND_2:
    case BUILT_IN_FETCH_AND_AND_4:
    case BUILT_IN_FETCH_AND_AND_8:
    case BUILT_IN_FETCH_AND_AND_16:
    case BUILT_IN_FETCH_AND_XOR_N:
    case BUILT_IN_FETCH_AND_XOR_1:
    case BUILT_IN_FETCH_AND_XOR_2:
    case BUILT_IN_FETCH_AND_XOR_4:
    case BUILT_IN_FETCH_AND_XOR_8:
    case BUILT_IN_FETCH_AND_XOR_16:
    case BUILT_IN_FETCH_AND_NAND_N:
    case BUILT_IN_FETCH_AND_NAND_1:
    case BUILT_IN_FETCH_AND_NAND_2:
    case BUILT_IN_FETCH_AND_NAND_4:
    case BUILT_IN_FETCH_AND_NAND_8:
    case BUILT_IN_FETCH_AND_NAND_16:
    case BUILT_IN_ADD_AND_FETCH_N:
    case BUILT_IN_ADD_AND_FETCH_1:
    case BUILT_IN_ADD_AND_FETCH_2:
    case BUILT_IN_ADD_AND_FETCH_4:
    case BUILT_IN_ADD_AND_FETCH_8:
    case BUILT_IN_ADD_AND_FETCH_16:
    case BUILT_IN_SUB_AND_FETCH_N:
    case BUILT_IN_SUB_AND_FETCH_1:
    case BUILT_IN_SUB_AND_FETCH_2:
    case BUILT_IN_SUB_AND_FETCH_4:
    case BUILT_IN_SUB_AND_FETCH_8:
    case BUILT_IN_SUB_AND_FETCH_16:
    case BUILT_IN_OR_AND_FETCH_N:
    case BUILT_IN_OR_AND_FETCH_1:
    case BUILT_IN_OR_AND_FETCH_2:
    case BUILT_IN_OR_AND_FETCH_4:
    case BUILT_IN_OR_AND_FETCH_8:
    case BUILT_IN_OR_AND_FETCH_16:
    case BUILT_IN_AND_AND_FETCH_N:
    case BUILT_IN_AND_AND_FETCH_1:
    case BUILT_IN_AND_AND_FETCH_2:
    case BUILT_IN_AND_AND_FETCH_4:
    case BUILT_IN_AND_AND_FETCH_8:
    case BUILT_IN_AND_AND_FETCH_16:
    case BUILT_IN_XOR_AND_FETCH_N:
    case BUILT_IN_XOR_AND_FETCH_1:
    case BUILT_IN_XOR_AND_FETCH_2:
    case BUILT_IN_XOR_AND_FETCH_4:
    case BUILT_IN_XOR_AND_FETCH_8:
    case BUILT_IN_XOR_AND_FETCH_16:
    case BUILT_IN_NAND_AND_FETCH_N:
    case BUILT_IN_NAND_AND_FETCH_1:
    case BUILT_IN_NAND_AND_FETCH_2:
    case BUILT_IN_NAND_AND_FETCH_4:
    case BUILT_IN_NAND_AND_FETCH_8:
    case BUILT_IN_NAND_AND_FETCH_16:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_N:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_16:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_N:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_16:
    case BUILT_IN_LOCK_TEST_AND_SET_N:
    case BUILT_IN_LOCK_TEST_AND_SET_1:
    case BUILT_IN_LOCK_TEST_AND_SET_2:
    case BUILT_IN_LOCK_TEST_AND_SET_4:
    case BUILT_IN_LOCK_TEST_AND_SET_8:
    case BUILT_IN_LOCK_TEST_AND_SET_16:
    case BUILT_IN_LOCK_RELEASE_N:
    case BUILT_IN_LOCK_RELEASE_1:
    case BUILT_IN_LOCK_RELEASE_2:
    case BUILT_IN_LOCK_RELEASE_4:
    case BUILT_IN_LOCK_RELEASE_8:
    case BUILT_IN_LOCK_RELEASE_16:
    {
      char * fname;
      tree fn;

      if (warn_sync_nand)
        warn_if_sync_nand_changed (fcode);

#ifdef __linux__

      fn = build_decl (FUNCTION_DECL, get_identifier ("membar_enter"),
                       build_function_type (void_type_node, NULL_TREE));
      DECL_ARTIFICIAL (fn) = 1;
      DECL_EXTERNAL (fn) = 1;
      TREE_PUBLIC (fn) = 1;
      TREE_NOTHROW (fn) = 1;

      dump_ir_call (gimple_build_call (fn, 0), 0);
#endif

      fname = (char *) map_sync2solaris_fname (fcode);
      if (fname)
        {
          fn = build_decl (FUNCTION_DECL, get_identifier (fname), 
                             build_function_type (gimple_call_return_type (stmt), 
                                                  TYPE_ARG_TYPES (TREE_TYPE (fndecl)))); 
          DECL_ARTIFICIAL (fn) = 1;
          DECL_EXTERNAL (fn) = 1;
          TREE_PUBLIC (fn) = 1;
          TREE_NOTHROW (fn) = 1;
          fn = build_function_call_expr (fn, arglist);
          stmt = gimple_build_call_from_tree (fn);
        }

      return dump_ir_call (stmt, need_return);
    }

    case BUILT_IN_LNI_START:
      push_lni_inline_context (stmt);
      break;

    case BUILT_IN_LNI_END:
      pop_lni_inline_context (stmt);
      break;
      
    case BUILT_IN_CONSTANT_P:
      {
	tree val;

	val = fold_builtin_constant_p (TREE_VALUE (arglist));
	/* Gimplification will pull the CALL_EXPR for the builtin out of
	   an if condition.  When not optimizing, we'll not CSE it back.
	   To avoid link error types of regressions, return false now.  */
	if (!val && !optimize)
	  val = integer_zero_node;
	
	if (val)
          ret = dump_ir_expr (val, MAP_FOR_VALUE);
        else
          ret = dump_ir_call (stmt, need_return);
        break;
      }
    case BUILT_IN_VA_ARG_PACK:
    case BUILT_IN_VA_ARG_PACK_LEN:
      /* All valid uses of __builtin_va_arg_pack () are removed during
         inlining.  */
      {
        location_t loc = gimple_location (stmt);
        error ("%Hinvalid use of %<__builtin_va_arg_pack ()%>", &loc);
      }
      break;
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      ret = dump_ir_builtin_int_roundingfn (stmt, need_return);
      break;
    case BUILT_IN_FREE:
      maybe_emit_free_warning (stmt);
    /* Fall through */
    default:
      ret = dump_ir_call (stmt, need_return);
      break;
    }
  
  return ret;
}

void
sunir_check_builtin_handling (tree function)
{
  enum built_in_function code = DECL_FUNCTION_CODE (function);

  switch (code)
    {
    case BUILT_IN_UNWIND_INIT:
    case BUILT_IN_DWARF_CFA:
    case BUILT_IN_DWARF_SP_COLUMN:
    case BUILT_IN_INIT_DWARF_REG_SIZES:
    case BUILT_IN_FROB_RETURN_ADDR:
    case BUILT_IN_EXTRACT_RETURN_ADDR:
    case BUILT_IN_EH_RETURN:
    case BUILT_IN_EXTEND_POINTER:
    case BUILT_IN_APPLY:
    case BUILT_IN_APPLY_ARGS:
    /* cannot handle object_size computations without SSA and optimizations */
    case BUILT_IN_OBJECT_SIZE: 
    /* all builtins below most like will be used in the same function
       with builtin_object_size(), so no point generating IR for them */
    case BUILT_IN_MEMCPY_CHK: 
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
    case BUILT_IN_SPRINTF_CHK:
    case BUILT_IN_VSPRINTF_CHK:
    case BUILT_IN_BSWAP32:
    case BUILT_IN_BSWAP64:
      DECL_DONT_GENERATE_SUNIR (current_function_decl) = 1;
      DECL_DONT_GENERATE_SUNIR (function) = 1;
      break;
    case BUILT_IN_VA_START:
      /* Currently we can't handle ms_va_start. */
      if (!strcmp (IDENTIFIER_POINTER (DECL_NAME (function)), "__builtin_ms_va_start"))
        flag_use_rtl_backend = -1;
      break;
    default:
      break;
    }
}
