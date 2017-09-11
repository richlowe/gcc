/* Definitions of target machine for GCC, for SPARC running Solaris 2
   Copyright 1992, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005,
   2006, 2007, 2008, 2010 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@netcom.com).
   Additional changes by David V. Henkel-Wallace (gumby@cygnus.com).

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

/* We are compiling for Solaris 2 now.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT MASK_FPREGS

/* Show that we need a GP when profiling.  */
#undef TARGET_PROFILING_NEEDS_GP
#define TARGET_PROFILING_NEEDS_GP 1

/* This macro closes up a function definition for the assembler.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, NAME, DECL)		\
  do								\
    {								\
      alpha_end_function(FILE,NAME,DECL);			\
      solaris_output_init_fini (FILE, DECL);			\
    }								\
  while (0)

#define CPP_SPEC "%{pthread:-D_REENTRANT}"

#define GNU_USER_DYNAMIC_LINKER "/lib/ld.so.1"

#undef LINK_SPEC
#define LINK_SPEC  "%{G*} %{R*}  %{h*}		\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{rdynamic:-export-dynamic}			\
   -dynamic-linker " GNU_USER_DYNAMIC_LINKER "	\
   %{relax:-relax}				\
   -X						\
   -melf64alpha"

#undef TARGET_SUN_TLS
#define TARGET_SUN_TLS 1

#define ASM_OUTPUT_CALL(FILE, FN)			        \
  do								\
    {								\
      fprintf (FILE, "\t.set macro\n");				\
      fprintf (FILE, "\tjsr\t$26, ");				\
      alpha_print_operand (FILE, XEXP (DECL_RTL (FN), 0), 0);	\
      fprintf (FILE, "\n");					\
      fprintf (FILE, "\tldgp\t$29, 0($26)\n");			\
      fprintf (FILE, "\t.set nomacro\n");			\
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
#define INIT_SECTION_ASM_OP	"\t.section\t.init, \"ax\""

#undef FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP	"\t.section\t.fini, \"ax\""

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

