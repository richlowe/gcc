/* Definitions for RISC-V GNU/Linux systems with ELF format.
   Copyright (C) 1998-2018 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


#define CPP_SPEC "%{pthread:-D_REENTRANT}"
#define GNU_USER_DYNAMIC_LINKER "/lib/ld.so.1"

#undef LINK_SPEC
#define LINK_SPEC "%{G*} %{R*} %{h*}	\
   %{mno-relax:--no-relax}				\
   %{static:-Bstatic}					\
   %{shared:-shared}					\
   %{symbolic:-Bsymbolic}				\
   %{!static:%{rdynamic:-export-dynamic}}	\
   -dynamic-linker " GNU_USER_DYNAMIC_LINKER "	\
  -melf" XLEN_SPEC "lriscv"

#undef TARGET_SUN_TLS
#define TARGET_SUN_TLS 1

#define ASM_OUTPUT_CALL(FILE, FN)			        \
  do								\
    {								\
      fprintf (FILE, "\tcall\t");				\
      riscv_print_operand (FILE, XEXP (DECL_RTL (FN), 0), 0);	\
      fprintf (FILE, "\n");					\
    }								\
  while (0)

#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR 1

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef	WINT_TYPE
#define	WINT_TYPE "int"
#undef	WINT_TYPE_SIZE
#define	WINT_TYPE_SIZE 32

#undef	CC1_SPEC
#define CC1_SPEC "%{!fno-PIC:-fPIC}"

#undef LIB_SPEC
#define LIB_SPEC  "%{shared:-lc -lgcc}%{!shared:%{profile:-lc -lgcc}%{!profile:-lc -lgcc}} "

#define SUBTARGET_CPU_EXTRA_SPECS

/* Register the Solaris-specific #pragma directives.  */
#undef REGISTER_TARGET_PRAGMAS
#define REGISTER_TARGET_PRAGMAS() do { solaris_register_pragmas (); } while (0)

#undef INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	"\t.section\t.init, \"ax\",@progbits"

#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP	"\t.section\t.fini, \"ax\",@progbits"

#undef EXTRA_SPECS
#define EXTRA_SPECS \
  { "startfile_arch",	 	STARTFILE_ARCH_SPEC },		\
  { "startfile_crtbegin",	STARTFILE_CRTBEGIN_SPEC },	\
  { "endfile_arch",	 	ENDFILE_ARCH_SPEC },		\
  { "endfile_crtend",		ENDFILE_CRTEND_SPEC },		\
  SUBTARGET_CPU_EXTRA_SPECS

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS
#define ENDFILE_ARCH_SPEC ""

#define ICACHE_FLUSH_FUNC "__riscv_flush_icache"

#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "march=rv64gc", "mabi=lp64d" }
