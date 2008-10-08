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

.inline signbit,1
  srl     %o0,31,%o0
.end

.inline signbitf,1
  srl     %o0,31,%o0
.end

.inline signbitl,1
  ld      [%o0],%o0
  srl     %o0,31,%o0
.end

.inline __builtin_powil,2
  call __powitf2
  nop
  unimp 16
.end

.inline __builtin_powi,2
  call __powidf2
  nop
.end

.inline __builtin_powif,2
  call __powisf2
  nop
.end

.inline	__builtin_copysign,4
  set     0x80000000,%o3
  and     %o2,%o3,%o2
  andn    %o0,%o3,%o0
  or      %o0,%o2,%o0
  std      %o0,[%sp+0x48]
  ldd     [%sp+0x48],%f0
.end
  
.inline	__builtin_copysignf,2
  set	0x80000000,%o2
  andn	%o0,%o2,%o0
  and	%o1,%o2,%o1
  or	%o0,%o1,%o0
  st	%o0,[%sp+0x44]
  ld	[%sp+0x44],%f0
.end

.inline __builtin_copysignl,2
  ld    [%o0], %o5
  set   0x80000000,%o4
  ld    [%o1], %o3
  andn  %o5, %o4, %o2
  ldx   [%o0 + 8], %o5
  and   %o3, %o4, %o1
  or    %o2, %o1, %o2
  st    %o2, [%o0]
  ldx   [%o0], %o3
  ld    [%sp + 64], %o0
  stx   %o3, [%o0]
  stx   %o5, [%o0 + 8]
.end

.inline sqrtf,1
  st      %o0,[%sp+0x44]
  ld      [%sp+0x44],%f0
  fsqrts  %f0,%f0
.end
.alias  __sqrtf = sqrtf

