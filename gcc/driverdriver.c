/* Meta driver for gcc. Decides whether to invoke intel-S2, sparc-S2 
   or sparc-linux compiler. 
   Copyright (C) 2008 by Sun Microsystems, Inc. All rights reserved.  
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


/* This is a metadriver for GCC.  Based on the target it figures out which
   GCC it should invoke.  It assumes the following installation structure:

   $INSTALLDIR
     |
     |-- bin
     |    |--drdr (the metadriver)
     |    |--gcc  (link to drdr)
     |    |--g++  (link to drdr)
     |
     |-- intel-S2
     |    |
     |    |-- gcc
     |
     |-- sparc-S2
          |
          |-- gcc
          |-- SUNW0scgfss

   On an Intel/Solaris box the intel-S2 directory contains the GCC compiler 
   targeted for an Intel/Solaris box.  The sparc-S2 directory contains the
   GCC compiler targeted for a SPARC/Solaris box.  Sometimes these are refered
   to as the native and cross compilers respecitively.

   On a SPARC/Solaris box we only have a "native" compiler and thus the 
   directory structure looks like

   $INSTALLDIR
     |
     |-- gcc
     |-- SUNWscgfss


   There are also some environment variables to allow using the GCC compilers
   from different locations. SUNW_GCCFSS_INTEL_S2_PATH which should point to a 
   directory equivalent to $INSTALLDIR/intel-S2.  And similary for
   SUNW_GCCFSS_SPARC_S2_PATH.

   There is some work to extend this to having a Linux target.

*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "multilib.h" /* before tm.h */
#include "tm.h"
#include <signal.h>
#if ! defined( SIGCHLD ) && defined( SIGCLD )
#  define SIGCHLD SIGCLD
#endif
#include "xregex.h"
#include "obstack.h"
#include "intl.h"
#include "prefix.h"
#include "gcc.h"
#include "flags.h"
#include "opts.h"

static char * debug_driver;
static long debug_driver_val;

extern char *progname;

static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };

static void perror_with_name (const char *);
static char *programname;

void
pfatal_with_name (const char *name)
{
  perror_with_name (name);
  exit (1);
}

static void
perror_with_name (const char *name)
{
  error ("%s: %s", name, xstrerror (errno));
}

/* Structures to keep track of prefixes to try when looking for files.  */

struct prefix_list
{
  const char *prefix;	      /* String to prepend to the path.  */
  struct prefix_list *next;   /* Next in linked list.  */
};

struct path_prefix
{
  struct prefix_list *plist;  /* List of prefixes to try */
  int max_len;                /* Max length of a prefix in PLIST */
  const char *name;           /* Name of this list (used in config stuff) */
};

/* List of prefixes to try when looking for this driverdriver.  */

static struct path_prefix driver_prefixes = { 0, 0, "driver" };

struct Known_Targets 
{
   const char *name;
   const int targetcpuissparc;
};

static struct Known_Targets known_targets[500] = {
        { "v7", TRUE },
        { "cypress", TRUE },
        { "v8", TRUE },
        { "supersparc", TRUE },
        { "sparclite", TRUE },
        { "f930", TRUE },
        { "f934", TRUE },
        { "hypersparc", TRUE },
        { "sparclitex86x", TRUE },
        { "sparclet", TRUE },
        { "tsc701", TRUE },
        { "v9", TRUE },
	{ "ultrsparc2i", TRUE },
	{ "ultrsparc2e", TRUE },
	{ "ultrsparc2", TRUE },
	{ "ultrsparc3iplus", TRUE },
	{ "ultrsparc3i", TRUE },
	{ "ultrsparc3cu", TRUE },
	{ "ultrsparc4plus", TRUE },
	{ "panther", TRUE },
	{ "ultrsparc4", TRUE },
        { "niagra", TRUE },
        { "ultraT1", TRUE },
        { "ultraT2", TRUE },
        { "v8a", TRUE },
        { "v8b", TRUE },
        { "v8plus", TRUE },
        { "v8plusa", TRUE },
        { "v8plusb", TRUE },
        { "v8plusc", TRUE },
        { "v9a", TRUE },
        { "v9b", TRUE },
        { "v9c", TRUE },
        { "v9d", TRUE },
        { "sparc", TRUE },
        { "sparcfmaf", TRUE },
        { "sparcimaf", TRUE },
        { "sparcvis", TRUE },
        { "sparcvis2", TRUE },
        { "sparc64vii", TRUE },
        { "sparc64vi", TRUE },
        { "sparcv9", TRUE },
        { "generic", TRUE },
        { "generic64", TRUE },
#ifdef __sparc__
        { "native", TRUE },
        { "native64", TRUE }
#else
        { "native", FALSE },
        { "native64", FALSE }
#endif
};

static const int num_known_targets = 46;

static int
is_target_cpu_sparc(char *targetname) 
{
  int i;

  if (debug_driver_val & 512) 
     fprintf(stdout,"is_target_cpu_sparc with %s\n",targetname);
  
  for (i = 0; i < num_known_targets; i++)
    if (! strcmp (targetname, known_targets[i].name ) ) {
      if (debug_driver_val & 512)
         fprintf(stdout, "is_target_cpu_sparc returning %d\n", 
                          known_targets[i].targetcpuissparc);
      return known_targets[i].targetcpuissparc;
    }

  /* not known target; so assume not sparc */
  return FALSE;
}

/* Check whether NAME can be accessed in MODE.  This is like access,
   except that it never considers directories to be executable.  */

static int
access_check (const char *name, int mode)
{
  if (mode == X_OK)
    {
      struct stat st;

      if (stat (name, &st) < 0
	  || S_ISDIR (st.st_mode))
	return -1;
    }

  return access (name, mode);
}


/* Search for NAME using the prefix list PREFIXES.  MODE is passed to
   access to check permissions.  
   Return 0 if not found, otherwise return its name, allocated with malloc.  */

static char *
find_a_file (const struct path_prefix *pprefix, const char *name, int mode)
{
  char * path = NULL;
  int len;
  int namelen;
  struct prefix_list *pl;

  /* Determine the filename to execute (special case for absolute paths).  */

  if (IS_ABSOLUTE_PATH (name))
    {
      if (access (name, mode) == 0)
	return xstrdup (name);

      return NULL;
    }

  namelen = strlen (name);

  /* see if a file exists with the right mode and name */
  for (pl = pprefix->plist; pl != 0; pl = pl->next) 
    {
       len = strlen (pl->prefix);
       path = xmalloc ( len + namelen + 1 );
       memcpy (path, pl->prefix, len);
       memcpy (path + len, name, namelen );
       path[len+namelen] = '\0';
       if (access_check (path, mode) == 0)
         return path;
    }

  /* could not find a file */
  return NULL;
}


/* Add an entry for PREFIX in PLIST. 

   If WARN is nonzero, we will warn if no file is found
   through this prefix.  WARN should point to an int
   which will be set to 1 if this entry is used.

   COMPONENT is the value to be passed to update_path.  */

static void
add_prefix (struct path_prefix *pprefix, const char *prefix,
	    const char *component )
{
  struct prefix_list *pl, **prev;
  int len;

  for (prev = &pprefix->plist;
       (*prev) != NULL;
       prev = &(*prev)->next)
    ;

  /* Keep track of the longest prefix.  */

  prefix = update_path (prefix, component);
  len = strlen (prefix);
  if (len > pprefix->max_len)
    pprefix->max_len = len;

  pl = XNEW (struct prefix_list);
  pl->prefix = prefix;

  /* Insert after PREV.  */
  pl->next = (*prev);
  (*prev) = pl;
}

int
main (int argc, const char **argv)
{
  char *path_to_driver = NULL;
  char *installdir;
  int target_cpu_is_sparc;
  int target_os_is_solaris;
  char *temp;
  char *cp;
  char *sunw_sparc_s2_path, *sunw_sparc_linux_path; 
  char *sunw_intel_linux_path, *sunw_intel_s2_path;
  char *sparc_s2_path, *sparc_linux_path;
  char *intel_s2_path, *intel_linux_path;

  char *current_work_directory;
  int i;
  struct pex_obj *pex;
  char *errmsg;
  long err;

  GET_ENVIRONMENT(debug_driver, "DEBUG_GCC_DRIVER");
  if (debug_driver) debug_driver_val = strtol(debug_driver, (char **)NULL, 10);

  if (debug_driver) fprintf(stdout,"DEBUG_DRIVER=%s DEBUG_DRIVER_VAL=%ld\n",debug_driver, debug_driver_val);

  if (debug_driver_val & 0x01) 
    fprintf (stdout, "argv[0]=%s\n", argv[0]);

  programname = argv[0];
  /* strip off driver name (gcc, g++, etc) */
  cp = programname + strlen (programname) - 1; 
  while (cp >= programname && *cp != '/') cp--;
  cp++; /* keep the / */
  programname = xstrdup(cp);
  progname = xstrdup(cp);

  /* if not /path or ./path or ../path */
  if (!(argv[0][0] == '/'
        || (argv[0][0] == '.' 
            && (argv[0][1] == '/' 
                || (argv[0][1] == '.' && argv[0][2] == '/')))))
    {
      /* determine how we found the driver */
      GET_ENVIRONMENT (temp, "PATH");
      if (temp)
        {
          const char *startp, *endp;
          char *nstore = alloca (strlen (temp) + 3);

          startp = endp = temp;
          while (1)
           {
             if (*endp == PATH_SEPARATOR || *endp == 0)
               {
                 strncpy (nstore, startp, endp - startp);
                 if (endp == startp)
                   strcpy (nstore, concat (".", dir_separator_str, NULL));
                 else if (!IS_DIR_SEPARATOR (endp[-1]))
                   {
                     nstore[endp - startp] = DIR_SEPARATOR;
                     nstore[endp - startp + 1] = 0;
                   }
                 else
                   nstore[endp - startp] = 0;
                 add_prefix (&driver_prefixes, nstore, 0);
                 if (*endp == 0)
                   break;
                 endp = startp = endp + 1;
               }
             else
               endp++;
           }
        }

      /* setup the starting points for the search path using the location of the
         driver as the starting point if invoking from a install area */
      path_to_driver = find_a_file (&driver_prefixes, argv[0], 0);
    }
  
  if (path_to_driver == NULL) 
    {
      path_to_driver = xstrdup (argv[0]);
    }

  if (debug_driver_val & 512 ) 
    fprintf (stdout, "before making absolute path_to_driver=%s\n", path_to_driver);

  /* need to make path_to_driver absolute */
  if (path_to_driver[0] != '/') 
    {
      char *p;
      if (current_work_directory == NULL) 
        {
          current_work_directory = xmalloc (MAXPATHLEN+1);
          (void) getcwd (current_work_directory, MAXPATHLEN+1);
        }
      p = xmalloc (strlen (current_work_directory) + strlen (path_to_driver) + 2);
      (void) strcpy (p, current_work_directory);
      (void) strcat (p, "/");
      (void) strcat (p, path_to_driver);
      path_to_driver = p;
    }

  /* canonicalize driver path */ 
  path_to_driver = lrealpath (path_to_driver);
  if (debug_driver_val & 512 ) 
    fprintf (stdout, "path_to_driver=%s\n", path_to_driver);

  /* figure out path to potential drivers */
  if (path_to_driver) 
    {
      char *cp;

      installdir = xstrdup (path_to_driver);
      cp = installdir + strlen (installdir) - 1;
      /* strip off driver name (gcc, g++, etc) */
      while (*cp != '/') cp--;
      cp++; /* keep the / */
      /* now back out the "bin/" */
      cp -= 4;
      *cp = 0;
      if (debug_driver_val & 512)
        fprintf (stdout,"PATH_TO_DRIVER_WO_DRIVER=%s\n", installdir);
      
      GET_ENVIRONMENT (sunw_intel_s2_path, "SUNW_GCCFSS_INTEL_S2_PATH");
      GET_ENVIRONMENT (sunw_sparc_s2_path, "SUNW_GCCFSS_SPARC_S2_PATH");
#ifdef __linux__
      GET_ENVIRONMENT (sunw_sparc_linux_path, "SUNW_GCCFSS_SPARC_LINUX_PATH");
      GET_ENVIRONMENT (sunw_intel_linux_path, "SUNW_GCCFSS_INTEL_LINUX_PATH");
#endif

      if (sunw_intel_s2_path == NULL)
         intel_s2_path = concat (installdir, 
                                    "/intel-S2/gcc/bin/",
                                     programname, NULL );
      else
         intel_s2_path = concat (sunw_intel_s2_path, "/gcc/bin/", 
                                programname, NULL );

      if (sunw_sparc_s2_path == NULL)
         sparc_s2_path = concat (installdir, 
                                    "/sparc-S2/gcc/bin/",
                                     programname, NULL );
      else
         sparc_s2_path = concat (sunw_sparc_s2_path, "/gcc/bin/", 
                              programname, NULL );
#ifdef __linux__
      if (sunw_intel_linux_path == NULL)
         intel_linux_path = concat (installdir, 
                                    "/intel-linux/gcc/bin/",
                                     programname, NULL );
      else
         intel_linux_path = concat (sunw_intel_linux_path, "/gcc/bin/", 
                              programname, NULL );
      if (sunw_sparc_linux_path == NULL)
         sparc_linux_path = concat (installdir, 
                                    "/sparc-linux/gcc/bin/",
                                     programname, NULL );
      else
         sparc_linux_path = concat (sunw_sparc_linux_path, "/gcc/bin/", 
                              programname, NULL );
#endif

    } 

  target_cpu_is_sparc = FALSE; /* by default use native compiler */
  /* figure out target cpu */
  for (i = 1; i < argc; i++)
    {
      if (! strncmp (argv[i], "-mcpu=", 6))
	  target_cpu_is_sparc = is_target_cpu_sparc(xstrdup(argv[i]+6));
      else if (! strncmp (argv[i], "-mtune=", 7))
	  target_cpu_is_sparc = is_target_cpu_sparc(xstrdup(argv[i]+7));
      else if (! strncmp (argv[i], "-xtarget=", 9))
	  target_cpu_is_sparc = is_target_cpu_sparc(xstrdup(argv[i]+9));
      else if (! strncmp (argv[i], "-xarch=", 7))
	  target_cpu_is_sparc = is_target_cpu_sparc(xstrdup(argv[i]+7));
      else if (! strncmp (argv[i], "-fast", 5))
	  target_cpu_is_sparc = TRUE;
    }

  /*  figure out target os */
  /*TODO: should incorporate into above loop when we have decided on flag
          to use to determine target os */
#ifdef __linux__
  target_os_is_solaris = FALSE;
#else
  target_os_is_solaris = TRUE;
#endif

  /* now invoke the real driver */
  argv[0] = target_cpu_is_sparc  ?
                    ( target_os_is_solaris ? sparc_s2_path : sparc_linux_path ) :
                    ( target_os_is_solaris ? intel_s2_path : intel_linux_path ) ;

  if ( debug_driver_val & 512 ) {
     fprintf ( stdout, "driverdriver: invoking %s\n", argv[0] );
     fprintf ( stdout, "driverdriver: access returns %d\n",
                        access( argv[0], 0) );
  }

  if (access (argv[0], 0) != 0 ) {
     /* the real GCC is not installed */
     if (target_cpu_is_sparc)
        error ("need to install the sparc-S2 components");
     else
        error ("need to install the intel-S2 components");
     exit(1);
  }

  pex = pex_init (0, argv[0], NULL);
  if (pex == NULL)
    pfatal_with_name (_("pex_init failed"));


  errmsg = pex_run (pex, PEX_LAST,
			argv[0], (char * const *) argv,
			NULL, NULL, &err);
  if (errmsg != NULL)
	{
	  if (err == 0)
	    fatal (errmsg);
	  else
	    {
	      errno = err;
	      pfatal_with_name (errmsg);
	    }
	}

   return 0;
}
