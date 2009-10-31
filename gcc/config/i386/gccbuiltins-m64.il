!
! This file contains inline template implementations 
! to support gccfss builtin functions.
!
!   Copyright (C) 2006, 2007 by Sun Microsystems, Inc. All rights reserved.  
!   File is licensed under the GNU Public License.
! 
! This file is part of GCC.
!
! GCC is free software; you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
! 
! GCC is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with GCC; see the file COPYING.  If not, write to
! the Free Software Foundation, 59 Temple Place - Suite 330,
! Boston, MA 02111-1307, USA.
!

.inline sqrtf,0
  sqrtss  %xmm0,%xmm0
.end
.inline __sqrtf,0
  sqrtss  %xmm0,%xmm0
.end

.inline __sync_fetch_and_add_1,0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  addb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_add_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  addw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_add_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  addl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end
.inline __sync_fetch_and_add, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  addl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_add_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  addq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_sub_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  subb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_sub_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  subw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_sub_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  subl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end
.inline __sync_fetch_and_sub, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  subl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_sub_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  subq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_and_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  andb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_and_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  andw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_and_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  andl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end
.inline __sync_fetch_and_and, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  andl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_and_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  andq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_nand_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  andb	%al, %cl
  notb	%cl	/ and;not to get the new value 
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_nand_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  andw	%ax, %cx	/ %cx = new value
  notw	%cx
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_nand_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end
.inline __sync_fetch_and_nand, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_nand_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  andq	%rax, %rcx
  notq	%rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_or_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  orb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_or_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  orw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_or_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  orl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end
.inline __sync_fetch_and_or, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  orl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_or_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  orq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_xor_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  xorb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_xor_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  xorw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_xor_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  xorl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end
.inline __sync_fetch_and_xor, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  xorl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
.end

.inline __sync_fetch_and_xor_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  xorq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
.end

.inline __sync_xor_and_fetch_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  xorb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%cl, %eax	/ return new value
.end

.inline __sync_xor_and_fetch_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  xorw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%cx, %eax	/ return new value
.end

.inline __sync_xor_and_fetch_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  xorl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
  movl	%ecx, %eax	/ return new value
.end
.inline __sync_xor_and_fetch, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  xorl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
  movl	%ecx, %eax	/ return new value
.end

.inline __sync_xor_and_fetch_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  xorq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
  movq	%rcx, %rax	/ return new value
.end

.inline __sync_sub_and_fetch_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  subb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%cl, %eax	/ return new value
.end

.inline __sync_sub_and_fetch_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  subw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%cx, %eax	/ return new value
.end

.inline __sync_sub_and_fetch_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  subl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
  movl	%ecx, %eax	/ return new value
.end
.inline __sync_sub_and_fetch, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  subl	%eax, %ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
  movl	%ecx, %eax	/ return new value
.end

.inline __sync_sub_and_fetch_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  subq	%rax, %rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
  movq	%rcx, %rax	/ return new value
.end

.inline __sync_nand_and_fetch_1, 0
  movb	(%rdi), %al	/ %al = old value
1:
  movb	%sil, %cl
  andb	%al, %cl
  notb	%cl	/ and;not to get the new value 
  lock
  cmpxchgb %cl, (%rdi)	/ try to stick it in
  jne	1b
  movzbl	%cl, %eax	/ return new value
.end

.inline __sync_nand_and_fetch_2, 0
  movw	(%rdi), %ax	/ %ax = old value
1:
  movw	%si, %cx
  andw	%ax, %cx
  notw	%cx
  lock
  cmpxchgw %cx, (%rdi)	/ try to stick it in
  jne	1b
  movzwl	%cx, %eax	/ return new value
.end

.inline __sync_nand_and_fetch_4, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
  movl	%ecx, %eax	/ return new value
.end
.inline __sync_nand_and_fetch, 0
  movl	(%rdi), %eax
1:
  movl	%esi, %ecx
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%rdi)
  jne	1b
  movl	%ecx, %eax	/ return new value
.end

.inline __sync_nand_and_fetch_8, 0
  movq	(%rdi), %rax
1:
  movq	%rsi, %rcx
  andq	%rax, %rcx
  notq	%rcx
  lock
  cmpxchgq %rcx, (%rdi)
  jne	1b
  movq	%rcx, %rax	/ return new value
.end

.inline __sync_bool_compare_and_swap_1, 0
  movzbl	%sil, %eax
  lock
  cmpxchgb %dl, (%rdi)
  sete	%al
  andl	$255, %eax
.end

.inline __sync_bool_compare_and_swap_2, 0
  movzwl	%si, %eax
  lock
  cmpxchgw %dx, (%rdi)
  sete	%al
  andl	$255, %eax
.end

.inline __sync_bool_compare_and_swap_4, 0
  movl	%esi, %eax
  lock
  cmpxchgl %edx, (%rdi)
  sete	%al
  andl	$255, %eax
.end
.inline __sync_bool_compare_and_swap, 0
  movl	%esi, %eax
  lock
  cmpxchgl %edx, (%rdi)
  sete	%al
  andl	$255, %eax
.end

.inline __sync_bool_compare_and_swap_8, 0
  movq	%rsi, %rax
  lock
  cmpxchgq %rdx, (%rdi)
  sete	%al
  andq	$255, %rax
.end

.inline __sync_lock_release_1, 0
  movzbl	$0, (%rdi)
.end

.inline __sync_lock_release_2, 0
  movzwl	$0, (%rdi)
.end

.inline __sync_lock_release_4, 0
  movl	$0, (%rdi)
.end
.inline __sync_lock_release, 0
  movl	$0, (%rdi)
.end

.inline __sync_lock_release_8, 0
  movq	$0, (%rdi)
.end

