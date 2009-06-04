!
! This file contains inline template implementations 
! to support gccfss builtin functions for V9 arch.
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

.inline signbit,1
  st      %f0,[%sp+0x87f]
  ld      [%sp+0x87f],%o0
  srl     %o0,31,%o0
.end

.inline signbitf,1
  st      %f1,[%sp+0x87f]
  ld      [%sp+0x87f],%o0
  srl     %o0,31,%o0
.end

.inline signbitl,1
  st      %f0,[%sp+0x87f]
  ld      [%sp+0x87f],%o0
  srl     %o0,31,%o0
.end

.inline __builtin_powil,2
  call	__powitf2
  nop
.end

.inline __builtin_powi,2
  call	__powidf2
  nop
.end

.inline __builtin_powif,2
  call	__powisf2
  nop
.end

.inline sqrtf,1
  fsqrts  %f1,%f0
.end
.alias  __sqrtf = sqrtf

.inline __sync_fetch_and_add_1, 2
  and	%o0, 0x3, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x3, %g1		! %g1 = byte offset, right-to-left
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  set	0xff, %o3		! %o3 = mask
  sll	%o3, %g1, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single byte value
  andn	%o0, 0x3, %o0		! %o0 = word address
  ld	[%o0], %o2		! read old value
1:
  add	%o2, %o1, %o5		! add value to the old value
  and	%o5, %o3, %o5		! clear other bits
  andn	%o2, %o3, %o4		! clear target bits
  or	%o4, %o5, %o5		! insert the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_add_2, 2
  and	%o0, 0x2, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x2, %g1		! %g1 = byte offset, right-to-left
  sll	%o4, 3, %o4		! %o4 = bit offset, left-to-right
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  sethi	%hi(0xffff0000), %o3	! %o3 = mask
  srl	%o3, %o4, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single short value
  andn	%o0, 0x2, %o0		! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld	[%o0], %o2		! read old value
1:
  add	%o1, %o2, %o5		! add value to the old value
  and	%o5, %o3, %o5		! clear other bits
  andn	%o2, %o3, %o4		! clear target bits
  or	%o4, %o5, %o5		! insert the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_add_4, 2
  ld    [%o0], %o2
1:
  add   %o2, %o1, %o3
  cas   [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %icc, 1b
  mov   %o3, %o2
  mov   %o2, %o0                ! %o0 = old value for return
.end
.alias  __sync_fetch_and_add = __sync_fetch_and_add_4

.inline __sync_fetch_and_add_8, 2
  ldx	[%o0], %o2
1:
  add	%o2, %o1, %o3
  casx	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %xcc, 1b
  mov	%o3, %o2
  mov   %o2, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_sub_1, 2
  and	%o0, 0x3, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x3, %g1		! %g1 = byte offset, right-to-left
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  set	0xff, %o3		! %o3 = mask
  sll	%o3, %g1, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single byte value
  andn	%o0, 0x3, %o0		! %o0 = word address
  ld	[%o0], %o2		! read old value
1:
  sub	%o2, %o1, %o5		! add value to the old value
  and	%o5, %o3, %o5		! clear other bits
  andn	%o2, %o3, %o4		! clear target bits
  or	%o4, %o5, %o5		! insert the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_sub_2, 2
  and	%o0, 0x2, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x2, %g1		! %g1 = byte offset, right-to-left
  sll	%o4, 3, %o4		! %o4 = bit offset, left-to-right
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  sethi	%hi(0xffff0000), %o3	! %o3 = mask
  srl	%o3, %o4, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single short value
  andn	%o0, 0x2, %o0		! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld	[%o0], %o2		! read old value
1:
  sub	%o2, %o1, %o5		! add value to the old value
  and	%o5, %o3, %o5		! clear other bits
  andn	%o2, %o3, %o4		! clear target bits
  or	%o4, %o5, %o5		! insert the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_sub_4, 2
  ld    [%o0], %o2
1:
  sub	%o2, %o1, %o3
  cas   [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %icc, 1b
  mov   %o3, %o2
  mov   %o2, %o0                ! %o0 = old value for return
.end
.alias __sync_fetch_and_sub = __sync_fetch_and_sub_4

.inline __sync_fetch_and_sub_8, 2
  ldx	[%o0], %o2
1:
  sub	%o2, %o1, %o3
  casx	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %xcc, 1b
  mov	%o3, %o2
  mov   %o2, %o0		! return old value
.end

.inline __sync_fetch_and_or_1, 2
  and	%o0, 0x3, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x3, %g1		! %g1 = byte offset, right-to-left
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  set	0xff, %o3		! %o3 = mask
  sll	%o3, %g1, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single byte value
  andn	%o0, 0x3, %o0		! %o0 = word address
  ld	[%o0], %o2		! read old value
1:
  or	%o2, %o1, %o5		! or in the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_or_2, 2
  and	%o0, 0x2, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x2, %g1		! %g1 = byte offset, right-to-left
  sll	%o4, 3, %o4		! %o4 = bit offset, left-to-right
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  sethi	%hi(0xffff0000), %o3	! %o3 = mask
  srl	%o3, %o4, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single short value
  andn	%o0, 0x2, %o0		! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld	[%o0], %o2		! read old value
1:
  or	%o2, %o1, %o5		! or in the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_or_4, 2
  ld	[%o0], %o2
1:
  or	%o2, %o1, %o3
  cas	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %icc, 1b
  mov	%o3, %o2
  mov	%o2, %o0		! return old value
.end
.alias __sync_fetch_and_or = __sync_fetch_and_or_4

.inline __sync_fetch_and_or_8, 2
  ldx	[%o0], %o2
1:
  or	%o2, %o1, %o3
  casx	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %xcc, 1b
  mov	%o3, %o2
  mov	%o2, %o0		! return old value
.end

.inline __sync_fetch_and_and_1, 2
  and	%o0, 0x3, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x3, %g1		! %g1 = byte offset, right-to-left
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  set	0xff, %o3		! %o3 = mask
  sll	%o3, %g1, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  orn	%o1, %o3, %o1		! all ones in other bytes
  andn	%o0, 0x3, %o0		! %o0 = word address
  ld	[%o0], %o2		! read old value
1:
  and	%o2, %o1, %o5		! and in the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_nand_1, 2
  and	%o0, 0x3, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x3, %g1		! %g1 = byte offset, right-to-left
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  set	0xff, %o3		! %o3 = mask
  sll	%o3, %g1, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  orn	%o1, %o3, %o1		! all ones in other bytes
  andn	%o0, 0x3, %o0		! %o0 = word address
  ld	[%o0], %o2		! read old value
1:
  andn	%o2, %o1, %o5		! andn in the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_and_2, 2
  and	%o0, 0x2, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x2, %g1		! %g1 = byte offset, right-to-left
  sll	%o4, 3, %o4		! %o4 = bit offset, left-to-right
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  sethi	%hi(0xffff0000), %o3	! %o3 = mask
  srl	%o3, %o4, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  orn	%o1, %o3, %o1		! all ones in the other half
  andn	%o0, 0x2, %o0		! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld	[%o0], %o2		! read old value
1:
  and	%o2, %o1, %o5		! and in the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_nand_2, 2
  and	%o0, 0x2, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x2, %g1		! %g1 = byte offset, right-to-left
  sll	%o4, 3, %o4		! %o4 = bit offset, left-to-right
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  sethi	%hi(0xffff0000), %o3	! %o3 = mask
  srl	%o3, %o4, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  orn	%o1, %o3, %o1		! all ones in the other half
  andn	%o0, 0x2, %o0		! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld	[%o0], %o2		! read old value
1:
  andn	%o2, %o1, %o5		! andn in the new value
  cas	[%o0], %o2, %o5
  cmp	%o2, %o5
  bne,a,pn %icc, 1b
  mov	%o5, %o2		! %o2 = old value
  and	%o2, %o3, %o2
  srl	%o2, %g1, %o0		! %o0 = old value for return
.end

.inline __sync_fetch_and_and_4, 2
  ld	[%o0], %o2
1:
  and	%o2, %o1, %o3
  cas	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %icc, 1b
  mov	%o3, %o2
  mov	%o2, %o0		! return old value
.end
.alias __sync_fetch_and_and = __sync_fetch_and_and_4

.inline __sync_fetch_and_nand_4, 2
  ld	[%o0], %o2
1:
  andn	%o2, %o1, %o3
  cas	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %icc, 1b
  mov	%o3, %o2
  mov	%o2, %o0		! return old value
.end
.alias __sync_fetch_and_nand = __sync_fetch_and_nand_4

.inline __sync_fetch_and_and_8, 2
  ldx	[%o0], %o2
1:
  and	%o2, %o1, %o3
  casx	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %xcc, 1b
  mov	%o3, %o2
  mov	%o2, %o0		! return old value
.end

.inline __sync_fetch_and_nand_8, 2
  ldx	[%o0], %o2
1:
  andn	%o2, %o1, %o3
  casx	[%o0], %o2, %o3
  cmp	%o2, %o3
  bne,a,pn %xcc, 1b
  mov	%o3, %o2
  mov	%o2, %o0		! return old value
.end

.inline __sync_fetch_and_xor_1, 2
  and   %o0, 0x3, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x3, %g1           ! %g1 = byte offset, right-to-left
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  set   0xff, %o3               ! %o3 = mask
  sll   %o3, %g1, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  orn   %o1, %o3, %o1           ! all ones in other bytes
  andn  %o0, 0x3, %o0           ! %o0 = word address
  ld    [%o0], %o2              ! read old value
1:
  xor	%o2, %o1, %o5           ! and in the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  and   %o2, %o3, %o2
  srl   %o2, %g1, %o0           ! %o0 = old value for return
.end

.inline __sync_fetch_and_xor_2, 2
  and   %o0, 0x2, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x2, %g1           ! %g1 = byte offset, right-to-left
  sll   %o4, 3, %o4             ! %o4 = bit offset, left-to-right
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  sethi %hi(0xffff0000), %o3    ! %o3 = mask
  srl   %o3, %o4, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  orn   %o1, %o3, %o1           ! all ones in the other half
  andn  %o0, 0x2, %o0           ! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld    [%o0], %o2              ! read old value
1:
  xor	%o2, %o1, %o5           ! and in the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  and   %o2, %o3, %o2
  srl   %o2, %g1, %o0           ! %o0 = old value for return
.end

.inline __sync_fetch_and_xor_4, 2
  ld    [%o0], %o2
1:
  xor	%o2, %o1, %o3
  cas   [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %icc, 1b
  mov   %o3, %o2
  mov   %o2, %o0                ! return old value
.end
.alias __sync_fetch_and_xor = __sync_fetch_and_xor_4

.inline __sync_fetch_and_xor_8, 2
  ldx   [%o0], %o2
1:
  xor	%o2, %o1, %o3
  casx  [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %xcc, 1b
  mov   %o3, %o2
  mov   %o2, %o0                ! return old value
.end

.inline __sync_xor_and_fetch_1, 2
  and   %o0, 0x3, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x3, %g1           ! %g1 = byte offset, right-to-left
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  set   0xff, %o3               ! %o3 = mask
  sll   %o3, %g1, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  orn   %o1, %o3, %o1           ! all ones in other bytes
  andn  %o0, 0x3, %o0           ! %o0 = word address
  ld    [%o0], %o2              ! read old value
1:
  xor   %o2, %o1, %o5           ! and in the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  xor	%o2, %o1, %o5
  and   %o5, %o3, %o5
  srl   %o5, %g1, %o0           ! %o0 = new value for return
.end

.inline __sync_xor_and_fetch_2, 2
  and   %o0, 0x2, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x2, %g1           ! %g1 = byte offset, right-to-left
  sll   %o4, 3, %o4             ! %o4 = bit offset, left-to-right
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  sethi %hi(0xffff0000), %o3    ! %o3 = mask
  srl   %o3, %o4, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  orn   %o1, %o3, %o1           ! all ones in the other half
  andn  %o0, 0x2, %o0           ! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld    [%o0], %o2              ! read old value
1:
  xor   %o2, %o1, %o5           ! and in the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  xor	%o2, %o1, %o5
  and   %o5, %o3, %o5
  srl   %o5, %g1, %o0           ! %o0 = new value for return
.end

.inline __sync_xor_and_fetch_4, 2
  ld    [%o0], %o2
1:
  xor   %o2, %o1, %o3
  cas   [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %icc, 1b
  mov   %o3, %o2
  xor	%o2, %o1, %o0		! return new value
.end
.alias __sync_xor_and_fetch = __sync_xor_and_fetch_4

.inline __sync_xor_and_fetch_8, 2
  ldx   [%o0], %o2
1:
  xor   %o2, %o1, %o3
  casx  [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %xcc, 1b
  mov   %o3, %o2
  xor	%o2, %o1, %o0		! return new value
.end

.inline __sync_sub_and_fetch_1, 2
  and   %o0, 0x3, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x3, %g1           ! %g1 = byte offset, right-to-left
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  set   0xff, %o3               ! %o3 = mask
  sll   %o3, %g1, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  and   %o1, %o3, %o1           ! %o1 = single byte value
  andn  %o0, 0x3, %o0           ! %o0 = word address
  ld    [%o0], %o2              ! read old value
1:
  sub   %o2, %o1, %o5           ! add value to the old value
  and   %o5, %o3, %o5           ! clear other bits
  andn  %o2, %o3, %o4           ! clear target bits
  or    %o4, %o5, %o5           ! insert the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  sub   %o2, %o1, %o5
  and   %o5, %o3, %o5
  srl   %o5, %g1, %o0           ! %o0 = new value for return
.end

.inline __sync_sub_and_fetch_2, 2
  and   %o0, 0x2, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x2, %g1           ! %g1 = byte offset, right-to-left
  sll   %o4, 3, %o4             ! %o4 = bit offset, left-to-right
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  sethi %hi(0xffff0000), %o3    ! %o3 = mask
  srl   %o3, %o4, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  and   %o1, %o3, %o1           ! %o1 = single short value
  andn  %o0, 0x2, %o0           ! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld    [%o0], %o2              ! read old value
1:
  sub   %o2, %o1, %o5           ! sub value from the old value
  and   %o5, %o3, %o5           ! clear other bits
  andn  %o2, %o3, %o4           ! clear target bits
  or    %o4, %o5, %o5           ! insert the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  sub	%o2, %o1, %o5 
  and   %o5, %o3, %o5
  srl   %o5, %g1, %o0           ! %o0 = new value for return
.end

.inline __sync_sub_and_fetch_4, 2
  ld    [%o0], %o2
1:
  sub   %o2, %o1, %o3
  cas   [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %icc, 1b
  mov   %o3, %o2
  sub   %o2, %o1, %o0
.end
.alias __sync_sub_and_fetch = __sync_sub_and_fetch_4

.inline __sync_sub_and_fetch_8, 2
  ldx   [%o0], %o2
1:
  sub   %o2, %o1, %o3
  casx  [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %xcc, 1b
  mov   %o3, %o2
  sub   %o2, %o1, %o0		! return new value
.end

.inline __sync_nand_and_fetch_1, 2
  and   %o0, 0x3, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x3, %g1           ! %g1 = byte offset, right-to-left
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  set   0xff, %o3               ! %o3 = mask
  sll   %o3, %g1, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  orn   %o1, %o3, %o1           ! all ones in other bytes
  andn  %o0, 0x3, %o0           ! %o0 = word address
  ld    [%o0], %o2              ! read old value
1:
  andn	%o2, %o1, %o5           ! andn in the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  andn	%o2, %o1, %o5           ! and in the new value
  and   %o5, %o3, %o5
  srl   %o5, %g1, %o0           ! %o0 = new value for return
.end

.inline __sync_nand_and_fetch_2, 2
  and   %o0, 0x2, %o4           ! %o4 = byte offset, left-to-right
  xor   %o4, 0x2, %g1           ! %g1 = byte offset, right-to-left
  sll   %o4, 3, %o4             ! %o4 = bit offset, left-to-right
  sll   %g1, 3, %g1             ! %g1 = bit offset, right-to-left
  sethi %hi(0xffff0000), %o3    ! %o3 = mask
  srl   %o3, %o4, %o3           ! %o3 = shifted to bit offset
  sll   %o1, %g1, %o1           ! %o1 = shifted to bit offset
  orn   %o1, %o3, %o1           ! all ones in the other half
  andn  %o0, 0x2, %o0           ! %o0 = word address
! if low-order bit is 1, we will properly get an alignment fault here
  ld    [%o0], %o2              ! read old value
1:
  andn	%o2, %o1, %o5           ! andn in the new value
  cas   [%o0], %o2, %o5
  cmp   %o2, %o5
  bne,a,pn %icc, 1b
  mov   %o5, %o2                ! %o2 = old value
  andn	%o2, %o1, %o5           ! andn in the new value
  and   %o5, %o3, %o5
  srl   %o5, %g1, %o0           ! %o0 = new value for return
.end

.inline __sync_nand_and_fetch_4, 2
  ld    [%o0], %o2
1:
  andn	%o2, %o1, %o3
  cas   [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %icc, 1b
  mov   %o3, %o2
  andn	%o2, %o1, %o0		! return new value
.end
.alias __sync_nand_and_fetch = __sync_nand_and_fetch_4

.inline __sync_nand_and_fetch_8, 2
  ldx   [%o0], %o2
1:
  andn	%o2, %o1, %o3
  casx  [%o0], %o2, %o3
  cmp   %o2, %o3
  bne,a,pn %xcc, 1b
  mov   %o3, %o2
  andn	%o2, %o1, %o0		! return new value
.end

.inline __sync_bool_compare_and_swap_1, 2
  and	%o0, 0x3, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x3, %g1		! %g1 = byte offset, right-to-left
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  set	0xff, %o3		! %o3 = mask
  sll	%o3, %g1, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single byte value
  sll	%o2, %g1, %o2		! %o2 = shifted to bit offset
  and	%o2, %o3, %o2		! %o2 = single byte value
  andn	%o0, 0x3, %o0		! %o0 = word address
  or	%g0, 1, %g1		! %g1 = return value (default to 1)
  ld	[%o0], %o4		! read old value
1:
  andn	%o4, %o3, %o4		! clear target bits
  or	%o4, %o2, %o5		! insert the new value
  or	%o4, %o1, %o4		! insert the comparison value
  cas	[%o0], %o4, %o5
  cmp	%o4, %o5		! did we succeed?
  be,pt	%icc, 2f
  and	%o5, %o3, %o4		! isolate the old value
  cmp	%o1, %o4		! should we have succeeded?
  be,a,pt	%icc, 1b		! yes, try again
  mov	%o5, %o4		! %o4 = old value
  or	%g0, 0, %g1		! %g1 = 0 : not written. 
2:
  mov	%g1, %o0		! %o0 = return value
.end

.inline __sync_bool_compare_and_swap_2, 2
  and	%o0, 0x2, %o4		! %o4 = byte offset, left-to-right
  xor	%o4, 0x2, %g1		! %g1 = byte offset, right-to-left
  sll	%o4, 3, %o4		! %o4 = bit offset, left-to-right
  sll	%g1, 3, %g1		! %g1 = bit offset, right-to-left
  sethi	%hi(0xffff0000), %o3	! %o3 = mask
  srl	%o3, %o4, %o3		! %o3 = shifted to bit offset
  sll	%o1, %g1, %o1		! %o1 = shifted to bit offset
  and	%o1, %o3, %o1		! %o1 = single short value
  sll	%o2, %g1, %o2		! %o2 = shifted to bit offset
  and	%o2, %o3, %o2		! %o2 = single short value
  andn	%o0, 0x2, %o0		! %o0 = word address
  or	%g0, 1, %g1		! %g1 = return value (default to 1)
! if low-order bit is 1, we will properly get an alignment fault here
  ld	[%o0], %o4		! read old value
1:
  andn	%o4, %o3, %o4		! clear target bits
  or	%o4, %o2, %o5		! insert the new value
  or	%o4, %o1, %o4		! insert the comparison value
  cas	[%o0], %o4, %o5
  cmp	%o4, %o5		! did we succeed?
  be,pt	%icc, 2f
  and	%o5, %o3, %o4		! isolate the old value
  cmp	%o1, %o4		! should we have succeeded?
  be,a,pt	%icc, 1b		! yes, try again
  mov	%o5, %o4		! %o4 = old value
  or	%g0, 0, %g1		! %g1 = 0 : not written. 
2:
  mov	%g1, %o0		! %o0 = return value
.end

.inline __sync_bool_compare_and_swap_4, 2
  cas	[%o0], %o1, %o2
  cmp	%o1, %o2
  or	%g0, 0, %o0
  movz	%icc, 1, %o0
.end
.alias __sync_bool_compare_and_swap = __sync_bool_compare_and_swap_4

.inline __sync_bool_compare_and_swap_8, 2
  casx	[%o0], %o1, %o2
  cmp   %o1, %o2
  or    %g0, 0, %o0
  movz  %icc, 1, %o0
.end

.inline __sync_lock_release_1, 1
  stb	%g0, [%o0]
.end

.inline __sync_lock_release_2, 1
  sth   %g0, [%o0]
.end

.inline __sync_lock_release_4, 1
  st	%g0, [%o0]
.end
.alias __sync_lock_release = __sync_lock_release_4

.inline __sync_lock_release_8, 1
  stx   %g0, [%o0]
.end

