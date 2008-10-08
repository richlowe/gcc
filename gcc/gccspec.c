/* Specific flags and argument handling of the C front-end.
   Copyright (C) 1999, 2001, 2003, 2007 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Modified by Sun Microsystems 2008 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "gcc.h"

/* Filter argc and argv before processing by the gcc driver proper.  */
void
lang_specific_driver (int *in_argc ATTRIBUTE_UNUSED,
		      const char *const **in_argv ATTRIBUTE_UNUSED,
		      int *in_added_libraries ATTRIBUTE_UNUSED)
{
  /* Systems which use the NeXT runtime by default should arrange
     for the shared libgcc to be used when -fgnu-runtime is passed
     through specs.  */
#if defined(ENABLE_SHARED_LIBGCC) && ! defined(NEXT_OBJC_RUNTIME)
  int i;

  /* The new argument list will be contained in this.  */
  const char **arglist;

  /* True if we should add -shared-libgcc to the command-line.  */
  int shared_libgcc = 0;
  int forget_shared_libgcc = 0;

  /* True if we should add -Zpedummy to the command-line. */
  int add_pecdummy = 0;

  /* The total number of arguments with the new stuff.  */
  int argc;

  /* The argument list.  */
  const char *const *argv;

  argc = *in_argc;
  argv = *in_argv;

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-')
	{
	  if (strcmp (argv[i], "-static-libgcc") == 0
	      || strcmp (argv[i], "-static") == 0)
	    forget_shared_libgcc = 1;
          else if (strncmp(argv[i], "-Zpec=", 6) == 0)
            add_pecdummy = 1;
	}
      else
	{
	  int len;

	  /* If the filename ends in .m or .mi, we are compiling ObjC
	     and want to pass -shared-libgcc.  */
	  len = strlen (argv[i]);
	  if ((len > 2 && argv[i][len - 2] == '.' && argv[i][len - 1] == 'm')
	      ||  (len > 3 && argv[i][len - 3] == '.' && argv[i][len - 2] == 'm'
		   && argv[i][len - 1] == 'i') && (forget_shared_libgcc == 0))
	    shared_libgcc = 1;
	}
    }

  if  ((shared_libgcc && (forget_shared_libgcc == 0))|| add_pecdummy)
    {
      /* Make sure to have room for the trailing NULL argument.  */
      arglist = XNEWVEC (const char *, argc + 1 + shared_libgcc + add_pecdummy);

      i = 0;
      do
	{
	  arglist[i] = argv[i];
	  i++;
	}
      while (i < argc);

       if (shared_libgcc) 
        arglist[i++] = "-shared-libgcc";

      if (add_pecdummy)
        arglist[i++] = "-Zpecdummyc";

      arglist[i] = NULL;

      *in_argc = i;
      *in_argv = arglist;
    }
#endif
}

/* Called before linking.  Returns 0 on success and -1 on failure.  */
int
lang_specific_pre_link (void)
{
  return 0;  /* Not used for C.  */
}

/* Number of extra output files that lang_specific_pre_link may generate.  */
int lang_specific_extra_outfiles = 0;  /* Not used for C.  */

#ifdef TARGET_OPTION_TRANSLATE_TABLE
/* for now -fast does not expand with -xmemalign=8s */
const struct {
  const char *const option_found;
  const char *const replacements;
} target_option_translations[] =
#ifndef __linux__
{
  { "-fast", "-Zfast -xtarget=native -Zfns=yes -ffast-math -Zfsimple=2 -Zftrap=%none -xbuiltin=%all -xlibmil -xlibmopt -Zalias_level=basic -xdepend=yes -xprefetch=auto,explicit -xprefetch_level=2" },
  { "-Zpecdummy", "-Zpecdummyc" },
  { "-xautopar", "-xautopar -xdepend=yes"},
  { "-xinstrument=datarace", "-xinstrument=datarace -g"},
  { "-O1", "-O1 -Zalias_level=any -xprefetch=auto,explicit -xprefetch_level=1 -xdepend=no"}, \
  { "-O", "-O -Zalias_level=any -xprefetch=auto,explicit -xprefetch_level=2 -xdepend=no"}, \
  { "-O2", "-O2 -Zalias_level=basic -xprefetch=auto,explicit -xprefetch_level=2 -xdepend=yes"}, \
  { "-O3", "-O3 -Zalias_level=std -xprefetch=auto,explicit -xprefetch_level=2 -xdepend=yes"}, \
  { "-Os", "-O2 -Zalias_level=basic -xspace -xprefetch=auto,explicit -xprefetch_level=1 -xdepend=no"}, \
  TARGET_OPTION_TRANSLATE_TABLE,
  { 0, 0 }
};
#else
{
  { "-fast", "-Zfast -xtarget=native -Zfns=yes -ffast-math -Zfsimple=2 -Zftrap=%none -xbuiltin=%all -Zalias_level=basic -xdepend=yes -xprefetch=auto,explicit -xprefetch_level=1" },
  { "-Zpecdummy", "-Zpecdummyc" },
  { "-xautopar", "-xautopar -xdepend=yes"},
  { "-xinstrument=datarace", "-xinstrument=datarace -g"},
  { "-O1", "-O1 -Zalias_level=any -xprefetch=auto,explicit -xprefetch_level=1 -xdepend=no"}, \
  { "-O", "-O -Zalias_level=any -xprefetch=auto,explicit -xprefetch_level=2 -xdepend=no"}, \
  { "-O2", "-O2 -Zalias_level=basic -xprefetch=auto,explicit -xprefetch_level=2 -xdepend=yes"}, \
  { "-O3", "-O3 -Zalias_level=std -xprefetch=auto,explicit -xprefetch_level=2 -xdepend=yes"}, \
  { "-Os", "-O2 -Zalias_level=basic -xspace -xprefetch=auto,explicit -xprefetch_level=1 -xdepend=no"}, \
  TARGET_OPTION_TRANSLATE_TABLE,
  { 0, 0 }
};
#endif
#endif

