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
  flds    (%esp)
  fsqrt
.end
.inline __sqrtf,0
  flds    (%esp)
  fsqrt
.end

.inline __sync_fetch_and_add_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  addb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_add_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  addw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_add_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  addl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end
.inline __sync_fetch_and_add, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  addl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end

.inline __sync_fetch_and_add_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  addl	%eax, %ebx
  adcl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  popl	%ebx
  popl	%edi
.end

.inline __sync_fetch_and_sub_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_sub_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_sub_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end
.inline __sync_fetch_and_sub, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end

.inline __sync_fetch_and_sub_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  subl	%eax, %ebx
  sbbl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  popl	%ebx
  popl	%edi
.end

.inline __sync_fetch_and_and_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_and_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_and_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end
.inline __sync_fetch_and_and, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end

.inline __sync_fetch_and_and_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  andl	%eax, %ebx
  andl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  popl	%ebx
  popl	%edi
.end

.inline __sync_fetch_and_nand_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andb	%al, %cl
  notb	%cl	/ and;not to get the new value 
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_nand_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andw	%ax, %cx
  notw	%cx
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_nand_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end
.inline __sync_fetch_and_nand, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end

.inline __sync_fetch_and_nand_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  andl	%eax, %ebx
  notl	%ebx
  andl	%edx, %ecx	/ %ecx:%ebx = new value
  notl	%ecx
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  popl	%ebx
  popl	%edi
.end

.inline __sync_fetch_and_or_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  orb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_or_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  orw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_or_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  orl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end
.inline __sync_fetch_and_or, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  orl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end

.inline __sync_fetch_and_or_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  orl	%eax, %ebx
  orl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  popl	%ebx
  popl	%edi
.end

.inline __sync_fetch_and_xor_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%al, %eax	/ return old value
.end

.inline __sync_fetch_and_xor_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%ax, %eax	/ return old value
.end

.inline __sync_fetch_and_xor_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end
.inline __sync_fetch_and_xor, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
.end

.inline __sync_fetch_and_xor_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  xorl	%eax, %ebx
  xorl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  popl	%ebx
  popl	%edi
.end

.inline __sync_xor_and_fetch_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%cl, %eax	/ return new value
.end

.inline __sync_xor_and_fetch_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%cx, %eax	/ return new value
.end

.inline __sync_xor_and_fetch_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
  movl	%ecx, %eax	/ return new value
.end
.inline __sync_xor_and_fetch, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  xorl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
  movl	%ecx, %eax	/ return new value
.end

.inline __sync_xor_and_fetch_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  xorl	%eax, %ebx
  xorl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  movl	%ebx, %eax
  movl	%ecx, %edx	/ return new value
  popl	%ebx
  popl	%edi
.end

.inline __sync_sub_and_fetch_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subb	%al, %cl	/ %cl = new value
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%cl, %eax	/ return new value
.end

.inline __sync_sub_and_fetch_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subw	%ax, %cx	/ %cx = new value
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%cx, %eax	/ return new value
.end

.inline __sync_sub_and_fetch_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
  movl	%ecx, %eax	/ return new value
.end
.inline __sync_sub_and_fetch, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  subl	%eax, %ecx	/ %ecx = new value
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
  movl	%ecx, %eax	/ return new value
.end

.inline __sync_sub_and_fetch_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  subl	%eax, %ebx
  sbbl	%edx, %ecx	/ %ecx:%ebx = new value
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  movl	%ebx, %eax
  movl	%ecx, %edx	/ return new value
  popl	%ebx
  popl	%edi
.end

.inline __sync_nand_and_fetch_1, 0
  movl	4(%esp), %edx	/ %edx = target address
  movb	(%edx), %al	/ %al = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andb	%al, %cl
  notb	%cl	/ and;not to get the new value 
  lock
  cmpxchgb %cl, (%edx)	/ try to stick it in
  jne	1b
  movzbl	%cl, %eax	/ return new value
.end

.inline __sync_nand_and_fetch_2, 0
  movl	4(%esp), %edx	/ %edx = target address
  movw	(%edx), %ax	/ %ax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andw	%ax, %cx
  notw	%cx
  lock
  cmpxchgw %cx, (%edx)	/ try to stick it in
  jne	1b
  movzwl	%cx, %eax	/ return new value
.end

.inline __sync_nand_and_fetch_4, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
  movl	%ecx, %eax	/ return new value
.end
.inline __sync_nand_and_fetch, 0
  movl	4(%esp), %edx	/ %edx = target address
  movl	(%edx), %eax	/ %eax = old value
1:
  movl	8(%esp), %ecx	/ %ecx = delta
  andl	%eax, %ecx
  notl	%ecx
  lock
  cmpxchgl %ecx, (%edx)	/ try to stick it in
  jne	1b
  movl	%ecx, %eax	/ return new value
.end

.inline __sync_nand_and_fetch_8, 0
  pushl	%edi
  pushl	%ebx
  movl	12(%esp), %edi	/ %edi = target address
  movl	(%edi), %eax
  movl	4(%edi), %edx	/ %edx:%eax = old value
1:
  movl	16(%esp), %ebx
  movl	20(%esp), %ecx	/ %ecx:%ebx = delta
  andl	%eax, %ebx
  notl	%ebx
  andl	%edx, %ecx	/ %ecx:%ebx = new value
  notl	%ecx
  lock
  cmpxchg8b (%edi)	/ try to stick it in
  jne	1b
  movl	%ebx, %eax
  movl	%ecx, %edx	/ return new value
  popl	%ebx
  popl	%edi
.end

.inline __sync_bool_compare_and_swap_1, 0
  movl	4(%esp), %edx
  movzbl	8(%esp), %eax
  movb	12(%esp), %cl
  lock
  cmpxchgb %cl, (%edx)
  sete	%al
  andl	$255, %eax
.end

.inline __sync_bool_compare_and_swap_2, 0
  movl	4(%esp), %edx
  movzwl	8(%esp), %eax
  movw	12(%esp), %cx
  lock
  cmpxchgw %cx, (%edx)
  sete	%al
  andl	$255, %eax
.end

.inline __sync_bool_compare_and_swap_4, 0
  movl	4(%esp), %edx
  movl	8(%esp), %eax
  movl	12(%esp), %ecx
  lock
  cmpxchgl %ecx, (%edx)
  sete	%al
  andl	$255, %eax
.end
.inline __sync_bool_compare_and_swap, 0
  movl	4(%esp), %edx
  movl	8(%esp), %eax
  movl	12(%esp), %ecx
  lock
  cmpxchgl %ecx, (%edx)
  sete	%al
  andl	$255, %eax
.end

.inline __sync_bool_compare_and_swap_8, 0
  pushl	%ebx
  pushl	%esi
  movl	12(%esp), %esi
  movl	16(%esp), %eax
  movl	20(%esp), %edx
  movl	24(%esp), %ebx
  movl	28(%esp), %ecx
  lock
  cmpxchg8b (%esi)
  sete	%al
  andl	$255, %eax
  popl	%esi
  popl	%ebx
.end

.inline __sync_lock_release_1, 0
  movl	4(%esp), %eax
  movzbl	$0, (%eax)
.end

.inline __sync_lock_release_2, 0
  movl	4(%esp), %eax
  movzwl	$0, (%eax)
.end

.inline __sync_lock_release_4, 0
  movl	4(%esp), %eax
  movl	$0, (%eax)
.end
.inline __sync_lock_release, 0
  movl	4(%esp), %eax
  movl	$0, (%eax)
.end

.inline __sync_lock_release_8, 0
  movl	4(%esp), %eax
  movl	$0, (%eax)
  movl	$0, 4(%eax)
.end

