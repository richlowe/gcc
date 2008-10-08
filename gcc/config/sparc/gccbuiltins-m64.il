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

.inline	__builtin_copysign,2
  fabsd	%f0,%f0
  st	%f0,[%sp+0x87f]
  ld	[%sp+0x87f],%o0
  st	%f2,[%sp+0x887]
  ld	[%sp+0x887],%o1
  srl	%o1,31,%o1
  sll	%o1,31,%o1
  or	%o0,%o1,%o0
  st	%o0,[%sp+0x87f]
  ld	[%sp+0x87f],%f0
.end

.inline	__builtin_copysignf,2
  fabss	%f1,%f1
  st	%f1,[%sp+0x87f]
  ld	[%sp+0x87f],%o0
  st	%f3,[%sp+0x887]
  ld	[%sp+0x887],%o1
  srl	%o1,31,%o1
  sll	%o1,31,%o1
  or	%o0,%o1,%o0
  st	%o0,[%sp+0x87f]
  ld	[%sp+0x87f],%f0
.end

.inline	__builtin_copysignl,2
  sethi       0x1fffff, %o5     ! 0x7ffffc00
  std         %f0, [%sp + 0x87f]
  sethi       0x200000, %o3     ! 0x80000000
  inc         1023, %o5         ! 0x7fffffff
  std         %f4, [%sp + 0x887]
  ld          [%sp + 0x887], %o2
  std         %f2, [%sp + 0x887]
  and         %o2, %o3, %o1
  ld          [%sp + 0x87f], %o4
  and         %o4, %o5, %o0
  or          %o0, %o1, %g5
  st          %g5, [%sp + 0x87f]
  ldd         [%sp + 0x87f], %f0
  ldd         [%sp + 0x887], %f2
.end

.inline sqrtf,1
  fsqrts  %f1,%f0
.end
.alias  __sqrtf = sqrtf

