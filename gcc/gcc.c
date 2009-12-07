/* Compiler driver program that can handle many languages.
   Copyright (C) 1987, 1989, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.

This paragraph is here to try to keep Sun CC from dying.
The number of chars here seems crucial!!!!  */

/* Modified by Sun Microsystems 2008 */

/* This program is the user interface to the C compiler and possibly to
other compilers.  It is used because compilation is a complicated procedure
which involves running several programs and passing temporary files between
them, forwarding the users switches to those programs selectively,
and deleting the temporary files at the end.

CC recognizes how to compile each input file by suffixes in the file names.
Once it knows which kind of compilation to perform, the procedure for
compilation is specified by a string called a "spec".  */

/* A Short Introduction to Adding a Command-Line Option.

   Before adding a command-line option, consider if it is really
   necessary.  Each additional command-line option adds complexity and
   is difficult to remove in subsequent versions.

   In the following, consider adding the command-line argument
   `--bar'.

   1. Each command-line option is specified in the specs file.  The
   notation is described below in the comment entitled "The Specs
   Language".  Read it.

   2. In this file, add an entry to "option_map" equating the long
   `--' argument version and any shorter, single letter version.  Read
   the comments in the declaration of "struct option_map" for an
   explanation.  Do not omit the first `-'.

   3. Look in the "specs" file to determine which program or option
   list should be given the argument, e.g., "cc1_options".  Add the
   appropriate syntax for the shorter option version to the
   corresponding "const char *" entry in this file.  Omit the first
   `-' from the option.  For example, use `-bar', rather than `--bar'.

   4. If the argument takes an argument, e.g., `--baz argument1',
   modify either DEFAULT_SWITCH_TAKES_ARG or
   DEFAULT_WORD_SWITCH_TAKES_ARG in gcc.h.  Omit the first `-'
   from `--baz'.

   5. Document the option in this file's display_help().  If the
   option is passed to a subprogram, modify its corresponding
   function, e.g., cppinit.c:print_help() or toplev.c:display_help(),
   instead.

   6. Compile and test.  Make sure that your new specs file is being
   read.  For example, use a debugger to investigate the value of
   "specs_file" in main().  */

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

/* By default there is no special suffix for target executables.  */
/* FIXME: when autoconf is fixed, remove the host check - dj */
#if defined(TARGET_EXECUTABLE_SUFFIX) && defined(HOST_EXECUTABLE_SUFFIX)
#define HAVE_TARGET_EXECUTABLE_SUFFIX
#endif

/* By default there is no special suffix for host executables.  */
#ifdef HOST_EXECUTABLE_SUFFIX
#define HAVE_HOST_EXECUTABLE_SUFFIX
#else
#define HOST_EXECUTABLE_SUFFIX ""
#endif

/* By default, the suffix for target object files is ".o".  */
#ifdef TARGET_OBJECT_SUFFIX
#define HAVE_TARGET_OBJECT_SUFFIX
#else
#define TARGET_OBJECT_SUFFIX ".o"
#endif

static const char dir_separator_str[] = { DIR_SEPARATOR, 0 };

/* Most every one is fine with LIBRARY_PATH.  For some, it conflicts.  */
#ifndef LIBRARY_PATH_ENV
#define LIBRARY_PATH_ENV "LIBRARY_PATH"
#endif

#ifndef HAVE_KILL
#define kill(p,s) raise(s)
#endif

/* If a stage of compilation returns an exit status >= 1,
   compilation of that file ceases.  */

#define MIN_FATAL_STATUS 1

/* Flag set by cppspec.c to 1.  */
int is_cpp_driver;

/* Flag set to nonzero if an @file argument has been supplied to gcc.  */
static bool at_file_supplied;

/* Flag saying to pass the greatest exit code returned by a sub-process
   to the calling program.  */
static int pass_exit_codes;

/* Definition of string containing the arguments given to configure.  */
#include "configargs.h"

/* Flag saying to print the directories gcc will search through looking for
   programs, libraries, etc.  */

static int print_search_dirs;

/* Flag saying to print the full filename of this file
   as found through our usual search mechanism.  */

static const char *print_file_name = NULL;

/* As print_file_name, but search for executable file.  */

static const char *print_prog_name = NULL;

/* Flag saying to print the relative path we'd use to
   find libgcc.a given the current compiler flags.  */

static int print_multi_directory;

/* Flag saying to print the relative path we'd use to
   find OS libraries given the current compiler flags.  */

static int print_multi_os_directory;

/* Flag saying to print the list of subdirectories and
   compiler flags used to select them in a standard form.  */

static int print_multi_lib;

/* Flag saying to print the command line options understood by gcc and its
   sub-processes.  */

static int print_help_list;

/* Flag saying to print the sysroot suffix used for searching for
   headers.  */

static int print_sysroot_headers_suffix;

/* Flag indicating whether we should print the command and arguments */

static int verbose_flag;

/* Flag indicating whether we should ONLY print the command and
   arguments (like verbose_flag) without executing the command.
   Displayed arguments are quoted so that the generated command
   line is suitable for execution.  This is intended for use in
   shell scripts to capture the driver-generated command line.  */
static int verbose_only_flag;

/* Flag indicating how to print command line options of sub-processes.  */

static int print_subprocess_help;

/* Flag indicating whether we should report subprocess execution times
   (if this is supported by the system - see pexecute.c).  */

static int report_times;

/* Nonzero means place this string before uses of /, so that include
   and library files can be found in an alternate location.  */

#ifdef TARGET_SYSTEM_ROOT
static const char *target_system_root = TARGET_SYSTEM_ROOT;
#else
static const char *target_system_root = 0;
#endif

/* Nonzero means pass the updated target_system_root to the compiler.  */

static int target_system_root_changed;

/* Nonzero means append this string to target_system_root.  */

static const char *target_sysroot_suffix = 0;

/* Nonzero means append this string to target_system_root for headers.  */

static const char *target_sysroot_hdrs_suffix = 0;

/* Nonzero means write "temp" files in source directory
   and use the source file's name in them, and don't delete them.  */

static int save_temps_flag;

/* Nonzero means pass multiple source files to the compiler at one time.  */

static int combine_flag = 0;

/* Nonzero means use pipes to communicate between subprocesses.
   Overridden by either of the above two flags.  */

static int use_pipes;

/* The compiler version.  */

static const char *compiler_version;

/* The target version specified with -V */

static const char *const spec_version = DEFAULT_TARGET_VERSION;

/* The target machine specified with -b.  */

static const char *spec_machine = DEFAULT_TARGET_MACHINE;

/* Nonzero if cross-compiling.
   When -b is used, the value comes from the `specs' file.  */

#ifdef CROSS_DIRECTORY_STRUCTURE
static const char *cross_compile = "1";
#else
static const char *cross_compile = "0";
#endif

#ifdef MODIFY_TARGET_NAME

/* Information on how to alter the target name based on a command-line
   switch.  The only case we support now is simply appending or deleting a
   string to or from the end of the first part of the configuration name.  */

static const struct modify_target
{
  const char *const sw;
  const enum add_del {ADD, DELETE} add_del;
  const char *const str;
}
modify_target[] = MODIFY_TARGET_NAME;
#endif

/* The number of errors that have occurred; the link phase will not be
   run if this is nonzero.  */
static int error_count = 0;

/* Greatest exit code of sub-processes that has been encountered up to
   now.  */
static int greatest_status = 1;

/* This is the obstack which we use to allocate many strings.  */

static struct obstack obstack;

/* This is the obstack to build an environment variable to pass to
   collect2 that describes all of the relevant switches of what to
   pass the compiler in building the list of pointers to constructors
   and destructors.  */

static struct obstack collect_obstack;

/* Forward declaration for prototypes.  */
struct path_prefix;
struct prefix_list;

static void init_spec (void);
static void store_arg (const char *, int, int);
static char *load_specs (const char *);
static void read_specs (const char *, int);
static void set_spec (const char *, const char *);
static struct compiler *lookup_compiler (const char *, size_t, const char *);
static char *build_search_list (const struct path_prefix *, const char *,
				bool, bool);
static void xputenv (const char *);
static void putenv_from_prefixes (const struct path_prefix *, const char *,
				  bool);
static int access_check (const char *, int);
static char *find_a_file (const struct path_prefix *, const char *, int, bool);
static void add_prefix (struct path_prefix *, const char *, const char *,
			int, int, int);
static void add_sysrooted_prefix (struct path_prefix *, const char *,
				  const char *, int, int, int);
static void translate_options (int *, const char *const **);
static char *skip_whitespace (char *);
static void delete_if_ordinary (const char *);
static void delete_temp_files (void);
static void delete_failure_queue (void);
static void clear_failure_queue (void);
static int check_live_switch (int, int);
static const char *handle_braces (const char *);
static inline bool input_suffix_matches (const char *, const char *);
static inline bool switch_matches (const char *, const char *, int);
static inline void mark_matching_switches (const char *, const char *, int);
static inline void process_marked_switches (void);
static const char *process_brace_body (const char *, const char *, const char *, int, int);
static const struct spec_function *lookup_spec_function (const char *);
static const char *eval_spec_function (const char *, const char *);
static const char *handle_spec_function (const char *);
static char *save_string (const char *, int);
static void set_collect_gcc_options (void);
static int do_spec_1 (const char *, int, const char *);
static int do_spec_2 (const char *);
static void do_option_spec (const char *, const char *);
static void do_self_spec (const char *);
static const char *find_file (const char *);
static int is_directory (const char *, bool);
static const char *validate_switches (const char *);
static void validate_all_switches (void);
static inline void validate_switches_from_spec (const char *);
static void give_switch (int, int);
static int used_arg (const char *, int);
static int default_arg (const char *, int);
static void set_multilib_dir (void);
static void print_multilib_info (void);
static void perror_with_name (const char *);
static void fatal_ice (const char *, ...) ATTRIBUTE_PRINTF_1 ATTRIBUTE_NORETURN;
static void notice (const char *, ...) ATTRIBUTE_PRINTF_1;
static void display_help (void);
static void add_preprocessor_option (const char *, int);
static void add_assembler_option (const char *, int);
static void add_linker_option (const char *, int);
static void process_command (int, const char **);
static int execute (void);
static void alloc_args (void);
static void clear_args (void);
static void fatal_error (int);
#if defined(ENABLE_SHARED_LIBGCC) && !defined(REAL_LIBGCC_SPEC)
static void init_gcc_specs (struct obstack *, const char *, const char *,
			    const char *);
#endif
#if defined(HAVE_TARGET_OBJECT_SUFFIX) || defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
static const char *convert_filename (const char *, int, int);
#endif

static const char *getenv_spec_function (int, const char **);
static const char *if_exists_spec_function (int, const char **);
static const char *if_exists_else_spec_function (int, const char **);
static const char *replace_outfile_spec_function (int, const char **);
static const char *version_compare_spec_function (int, const char **);
static const char *include_spec_function (int, const char **);
static const char *print_asm_header_spec_function (int, const char **);

static char *make_absolute_from_Y(const char *, const char *);
static char *debug_driver = NULL;
/* values for debug_driver_val and what you will see
   0x01 - show what is happening with command line options
   0x02 - values of variables containing location of files
   0x04 - multilib_dir
   0x08 - input_basename
   0x10 - do_spec_path  
   0x20 - add-prefix (startfile prefix)
   0x40 - -isystem from include prefix */
static long debug_driver_val = 0;
static void delete_duplicate_options (int *, char ***);
static int translate_native_options (int *, char ***);
static const char *find_executable_file (const char *);
static void warning (const char *, ...) ATTRIBUTE_PRINTF_1;
static void add_iropt_option (const char *, int);
static void add_sscg_option (const char *, int);
static void add_ipo_option (const char *, int);
static void add_postopt_option (const char *, int);

/* The Specs Language

Specs are strings containing lines, each of which (if not blank)
is made up of a program name, and arguments separated by spaces.
The program name must be exact and start from root, since no path
is searched and it is unreliable to depend on the current working directory.
Redirection of input or output is not supported; the subprograms must
accept filenames saying what files to read and write.

In addition, the specs can contain %-sequences to substitute variable text
or for conditional text.  Here is a table of all defined %-sequences.
Note that spaces are not generated automatically around the results of
expanding these sequences; therefore, you can concatenate them together
or with constant text in a single argument.

 %%	substitute one % into the program name or argument.
 %i     substitute the name of the input file being processed.
 %b     substitute the basename of the input file being processed.
	This is the substring up to (and not including) the last period
	and not including the directory.
 %B	same as %b, but include the file suffix (text after the last period).
 %gSUFFIX
	substitute a file name that has suffix SUFFIX and is chosen
	once per compilation, and mark the argument a la %d.  To reduce
	exposure to denial-of-service attacks, the file name is now
	chosen in a way that is hard to predict even when previously
	chosen file names are known.  For example, `%g.s ... %g.o ... %g.s'
	might turn into `ccUVUUAU.s ccXYAXZ12.o ccUVUUAU.s'.  SUFFIX matches
	the regexp "[.0-9A-Za-z]*%O"; "%O" is treated exactly as if it
	had been pre-processed.  Previously, %g was simply substituted
	with a file name chosen once per compilation, without regard
	to any appended suffix (which was therefore treated just like
	ordinary text), making such attacks more likely to succeed.
 %|SUFFIX
	like %g, but if -pipe is in effect, expands simply to "-".
 %mSUFFIX
        like %g, but if -pipe is in effect, expands to nothing.  (We have both
	%| and %m to accommodate differences between system assemblers; see
	the AS_NEEDS_DASH_FOR_PIPED_INPUT target macro.)
 %uSUFFIX
	like %g, but generates a new temporary file name even if %uSUFFIX
	was already seen.
 %USUFFIX
	substitutes the last file name generated with %uSUFFIX, generating a
	new one if there is no such last file name.  In the absence of any
	%uSUFFIX, this is just like %gSUFFIX, except they don't share
	the same suffix "space", so `%g.s ... %U.s ... %g.s ... %U.s'
	would involve the generation of two distinct file names, one
	for each `%g.s' and another for each `%U.s'.  Previously, %U was
	simply substituted with a file name chosen for the previous %u,
	without regard to any appended suffix.
 %jSUFFIX
        substitutes the name of the HOST_BIT_BUCKET, if any, and if it is
        writable, and if save-temps is off; otherwise, substitute the name
        of a temporary file, just like %u.  This temporary file is not
        meant for communication between processes, but rather as a junk
        disposal mechanism.
 %.SUFFIX
        substitutes .SUFFIX for the suffixes of a matched switch's args when
        it is subsequently output with %*. SUFFIX is terminated by the next
        space or %.
 %+SUFFIX
        appends .SUFFIX to the matched switch's args when
        it is subsequently output with %*. SUFFIX is terminated by the next
        space or %.
 %d	marks the argument containing or following the %d as a
	temporary file name, so that that file will be deleted if CC exits
	successfully.  Unlike %g, this contributes no text to the argument.
 %w	marks the argument containing or following the %w as the
	"output file" of this compilation.  This puts the argument
	into the sequence of arguments that %o will substitute later.
 %V	indicates that this compilation produces no "output file".
 %W{...}
	like %{...} but mark last argument supplied within
	as a file to be deleted on failure.
 %o	substitutes the names of all the output files, with spaces
	automatically placed around them.  You should write spaces
	around the %o as well or the results are undefined.
	%o is for use in the specs for running the linker.
	Input files whose names have no recognized suffix are not compiled
	at all, but they are included among the output files, so they will
	be linked.
 %O	substitutes the suffix for object files.  Note that this is
        handled specially when it immediately follows %g, %u, or %U
	(with or without a suffix argument) because of the need for
	those to form complete file names.  The handling is such that
	%O is treated exactly as if it had already been substituted,
	except that %g, %u, and %U do not currently support additional
	SUFFIX characters following %O as they would following, for
	example, `.o'.
 %q     Add -I<include directories> from the cross compiler environment variable
 %I	Substitute any of -iprefix (made from GCC_EXEC_PREFIX), -isysroot
	(made from TARGET_SYSTEM_ROOT), -isystem (made from COMPILER_PATH
	and -B options) and -imultilib as necessary.
 %H     Substitute the directory STANDARD_STARTFILE_PREFIX for libstdc++
 %F     Substitute the directory STANDARD_STARTFILE_PREFIX for gccbuiltins.il
 %J     Substitute the directory STUDIOPRODDIR_LIB
 %K     Use alternate directory or standard directory for SunStudio 
        processes
 %s     current argument is the name of a library or startup file of some sort.
        Search for that file in a standard list of directories
	and substitute the full name found.
 %eSTR  Print STR as an error message.  STR is terminated by a newline.
        Use this when inconsistent options are detected.
 %nSTR  Print STR as a notice.  STR is terminated by a newline.
 %NSTR  Print STR as a warning.  STR is terminated by a newline.
 %x{OPTION}	Accumulate an option for %X.
 %M	Output the accumulated postopt options specified by compilations.
 %P	Output the accumulated ipo options specified by compilations.
 %Q	Output the accumulated iropt options specified by compilations.
 %T	Output the accumulated Sun cg options specified by compilations. 
 %X	Output the accumulated linker options specified by compilations.
 %Y	Output the accumulated assembler options specified by compilations.
 %Z	Output the accumulated preprocessor options specified by compilations.
 %a     process ASM_SPEC as a spec.
        This allows config.h to specify part of the spec for running as.
 %A	process ASM_FINAL_SPEC as a spec.  A capital A is actually
	used here.  This can be used to run a post-processor after the
	assembler has done its job.
 %D	Dump out a -L option for each directory in startfile_prefixes.
	If multilib_dir is set, extra entries are generated with it affixed.
 %l     process LINK_SPEC as a spec.
 %L     process LIB_SPEC as a spec.
 %G     process LIBGCC_SPEC as a spec.
 %R     Output the concatenation of target_system_root and
        target_sysroot_suffix.
 %S     process STARTFILE_SPEC as a spec.  A capital S is actually used here.
 %E     process ENDFILE_SPEC as a spec.  A capital E is actually used here.
 %C     process CPP_SPEC as a spec.
 %1	process CC1_SPEC as a spec.
 %2	process CC1PLUS_SPEC as a spec.
 %*	substitute the variable part of a matched option.  (See below.)
	Note that each comma in the substituted string is replaced by
	a single space.
 %<S    remove all occurrences of -S from the command line.
        Note - this command is position dependent.  % commands in the
        spec string before this one will see -S, % commands in the
        spec string after this one will not.
 %<S*	remove all occurrences of all switches beginning with -S from the
        command line.
 %:function(args)
	Call the named function FUNCTION, passing it ARGS.  ARGS is
	first processed as a nested spec string, then split into an
	argument vector in the usual fashion.  The function returns
	a string which is processed as if it had appeared literally
	as part of the current spec.
 %{S}   substitutes the -S switch, if that switch was given to CC.
	If that switch was not specified, this substitutes nothing.
	Here S is a metasyntactic variable.
 %{S*}  substitutes all the switches specified to CC whose names start
	with -S.  This is used for -o, -I, etc; switches that take
	arguments.  CC considers `-o foo' as being one switch whose
	name starts with `o'.  %{o*} would substitute this text,
	including the space; thus, two arguments would be generated.
 %{S*&T*} likewise, but preserve order of S and T options (the order
	of S and T in the spec is not significant).  Can be any number
	of ampersand-separated variables; for each the wild card is
	optional.  Useful for CPP as %{D*&U*&A*}.

 %{S:X}   substitutes X, if the -S switch was given to CC.
 %{!S:X}  substitutes X, if the -S switch was NOT given to CC.
 %{S*:X}  substitutes X if one or more switches whose names start
          with -S was given to CC.  Normally X is substituted only
          once, no matter how many such switches appeared.  However,
          if %* appears somewhere in X, then X will be substituted
          once for each matching switch, with the %* replaced by the
          part of that switch that matched the '*'.
 %{.S:X}  substitutes X, if processing a file with suffix S.
 %{!.S:X} substitutes X, if NOT processing a file with suffix S.
 %{,S:X}  substitutes X, if processing a file which will use spec S.
 %{!,S:X} substitutes X, if NOT processing a file which will use spec S.
	  
 %{S|T:X} substitutes X if either -S or -T was given to CC.  This may be
	  combined with '!', '.', ',', and '*' as above binding stronger
	  than the OR.
	  If %* appears in X, all of the alternatives must be starred, and
	  only the first matching alternative is substituted.
 %{S:X;   if S was given to CC, substitutes X;
   T:Y;   else if T was given to CC, substitutes Y;
    :D}   else substitutes D.  There can be as many clauses as you need.
          This may be combined with '.', '!', ',', '|', and '*' as above.

 %(Spec) processes a specification defined in a specs file as *Spec:
 %[Spec] as above, but put __ around -D arguments

The conditional text X in a %{S:X} or similar construct may contain
other nested % constructs or spaces, or even newlines.  They are
processed as usual, as described above.  Trailing white space in X is
ignored.  White space may also appear anywhere on the left side of the
colon in these constructs, except between . or * and the corresponding
word.

The -O, -f, -m, and -W switches are handled specifically in these
constructs.  If another value of -O or the negated form of a -f, -m, or
-W switch is found later in the command line, the earlier switch
value is ignored, except with {S*} where S is just one letter; this
passes all matching options.

The character | at the beginning of the predicate text is used to indicate
that a command should be piped to the following command, but only if -pipe
is specified.

Note that it is built into CC which switches take arguments and which
do not.  You might think it would be useful to generalize this to
allow each compiler's spec to say which switches take arguments.  But
this cannot be done in a consistent fashion.  CC cannot even decide
which input files have been specified without knowing which switches
take arguments, and it must know which input files to compile in order
to tell which compilers to run.

CC also knows implicitly that arguments starting in `-l' are to be
treated as compiler output files, and passed to the linker in their
proper position among the other output files.  */

/* Define the macros used for specs %a, %l, %L, %S, %C, %1.  */

/* config.h can define ASM_SPEC to provide extra args to the assembler
   or extra switch-translations.  */
#ifndef ASM_SPEC
#define ASM_SPEC ""
#endif

/* config.h can define ASM_FINAL_SPEC to run a post processor after
   the assembler has run.  */
#ifndef ASM_FINAL_SPEC
#define ASM_FINAL_SPEC ""
#endif

/* config.h can define CPP_SPEC to provide extra args to the C preprocessor
   or extra switch-translations.  */
#ifndef CPP_SPEC
#define CPP_SPEC ""
#endif

/* config.h can define CC1_SPEC to provide extra args to cc1 and cc1plus
   or extra switch-translations.  */
#ifndef CC1_SPEC
#define CC1_SPEC ""
#endif

/* config.h can define CC1PLUS_SPEC to provide extra args to cc1plus
   or extra switch-translations.  */
#ifndef CC1PLUS_SPEC
#define CC1PLUS_SPEC ""
#endif

/* config.h can define LINK_SPEC to provide extra args to the linker
   or extra switch-translations.  */
#ifndef LINK_SPEC
#define LINK_SPEC ""
#endif

/* config.h can define LIB_SPEC to override the default libraries.  */
#ifndef LIB_SPEC
#define LIB_SPEC "%{!shared:%{g*:-lg} %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"
#endif

/* mudflap specs */
#ifndef MFWRAP_SPEC
/* XXX: valid only for GNU ld */
/* XXX: should exactly match hooks provided by libmudflap.a */
#define MFWRAP_SPEC " %{static: %{fmudflap|fmudflapth: \
 --wrap=malloc --wrap=free --wrap=calloc --wrap=realloc\
 --wrap=mmap --wrap=munmap --wrap=alloca\
} %{fmudflapth: --wrap=pthread_create\
}} %{fmudflap|fmudflapth: --wrap=main}"
#endif
#ifndef MFLIB_SPEC
#define MFLIB_SPEC "%{fmudflap|fmudflapth: -export-dynamic}"
#endif

/* config.h can define LIBGCC_SPEC to override how and when libgcc.a is
   included.  */
#ifndef LIBGCC_SPEC
#if defined(REAL_LIBGCC_SPEC)
#define LIBGCC_SPEC REAL_LIBGCC_SPEC
#elif defined(LINK_LIBGCC_SPECIAL_1)
/* Have gcc do the search for libgcc.a.  */
#define LIBGCC_SPEC "libgcc.a%s"
#else
#define LIBGCC_SPEC "-lgcc"
#endif
#endif

/* config.h can define STARTFILE_SPEC to override the default crt0 files.  */
#ifndef STARTFILE_SPEC
#define STARTFILE_SPEC  \
  "%{!shared:%{pg:gcrt0%O%s}%{!pg:%{p:mcrt0%O%s}%{!p:crt0%O%s}}}"
#endif

/* config.h can define SWITCHES_NEED_SPACES to control which options
   require spaces between the option and the argument.  */
#ifndef SWITCHES_NEED_SPACES
#define SWITCHES_NEED_SPACES ""
#endif

/* config.h can define ENDFILE_SPEC to override the default crtn files.  */
#ifndef ENDFILE_SPEC
#define ENDFILE_SPEC ""
#endif

#ifndef LINKER_NAME
#define LINKER_NAME "collect2"
#endif

#ifdef HAVE_AS_DEBUG_PREFIX_MAP
#define ASM_MAP " %{fdebug-prefix-map=*:--debug-prefix-map %*}"
#else
#define ASM_MAP ""
#endif

/* Define ASM_DEBUG_SPEC to be a spec suitable for translating '-g'
   to the assembler.  */
#ifndef ASM_DEBUG_SPEC
# if defined(DBX_DEBUGGING_INFO) && defined(DWARF2_DEBUGGING_INFO) \
     && defined(HAVE_AS_GDWARF2_DEBUG_FLAG) && defined(HAVE_AS_GSTABS_DEBUG_FLAG)
#  define ASM_DEBUG_SPEC						\
      (PREFERRED_DEBUGGING_TYPE == DBX_DEBUG				\
       ? "%{gdwarf-2*:--gdwarf2}%{!gdwarf-2*:%{g*:--gstabs}}" ASM_MAP	\
       : "%{gstabs*:--gstabs}%{!gstabs*:%{g*:--gdwarf2}}" ASM_MAP)
# else
#  if defined(DBX_DEBUGGING_INFO) && defined(HAVE_AS_GSTABS_DEBUG_FLAG)
#   define ASM_DEBUG_SPEC "%{g*:--gstabs}" ASM_MAP
#  endif
#  if defined(DWARF2_DEBUGGING_INFO) && defined(HAVE_AS_GDWARF2_DEBUG_FLAG)
#   define ASM_DEBUG_SPEC "%{g*:--gdwarf2}" ASM_MAP
#  endif
# endif
#endif
#ifndef ASM_DEBUG_SPEC
# define ASM_DEBUG_SPEC ""
#endif

/* Here is the spec for running the linker, after compiling all files.  */

/* This is overridable by the target in case they need to specify the
   -lgcc and -lc order specially, yet not require them to override all
   of LINK_COMMAND_SPEC.  */
#ifndef LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC "%G %L %G"
#endif

#ifndef LINK_SSP_SPEC
#ifdef TARGET_LIBC_PROVIDES_SSP
#define LINK_SSP_SPEC "%{fstack-protector:}"
#else
#define LINK_SSP_SPEC "%{fstack-protector|fstack-protector-all:-lssp_nonshared -lssp}"
#endif
#endif

#ifndef LINK_PIE_SPEC
#ifdef HAVE_LD_PIE
#define LINK_PIE_SPEC "%{pie:-pie} "
#else
#define LINK_PIE_SPEC "%{pie:} "
#endif
#endif

#ifdef __linux__
#define LINKER_PATH "/usr/bin/ld "
#else
#define LINKER_PATH "/usr/ccs/bin/ld "
#endif

#ifndef __linux__
#define LINK_COMMAND_LIB  "\
    %{!xlibmopt: %{O3: -lmopt } }\
    %{xautopar: -lmtsk -lthread} %{Zmt: -lthread}\
    %{!shared: %{xprofile=collect* : \
      %{m64 : %J/v9/xprof_fini.o ; \
        m32 : %J/xprof_fini.o ; \
            : %J/xprof_fini.o } \
        -Bdynamic -lxprof -lthread } } \
"
#else
#define LINK_COMMAND_LIB "\
    %{xautopar: -lmtsk -lpthread} %{Zmt: -lpthread}\
    %{!shared: %{xprofile=collect* : \
    %{m64 : %J/v9/xprof_fini.o ; \
      m32 : %J/xprof_fini.o ; \
          : %J/xprof_fini.o } \
         -Bdynamic -lxprof -lpthread} } \
"
#endif

/* Moved out guts of LINK_COMMAND_SPEC since it needs to be shared with
   ipo phase 2 */
#ifndef LINK_COMMAND_GUTS
#define LINK_COMMAND_GUTS "\
    %{!A:%{!nostdlib:%{!nostartfiles:%S}}} %l \
    " LINK_PIE_SPEC "%X %{o*} %{A} %{d} %{e*} %{m} %{N} %{n} %{r}\
    %{s} %{t} %{u*} %{x} %{z} %{Z} \
    %{static:} %{L*} %(mfwrap) %(link_libgcc) %o %(mflib)\
    %{xlibmopt: -lmopt }\
    %{Zfprofile-arcs|Zfprofile-generate:-lgcov}\
    %{!nostdlib:%{!nodefaultlibs:%(link_gcc_c_sequence)}}\
    %{R*} \
    %{xpagesize* : %{m64: %J/v9/pagesize.o ; \
                     m32: %J/pagesize.o ; \
                        : %J/pagesize.o} } \
    %{xinstrument=datarace : %{m64: %J/v9/libtha.so.1; \
                               m32: %J/libtha.so.1; \
                                  : %J/libtha.so.1} }\
    %{fopenmp|xopenmp|xopenmp=noopt|xopenmp=parallel: -lmtsk} \
    %{fstack-protector|fstack-protector-all: -lssp_nonshared -lssp} \
    %{xvector: -lmvec} %{xvector=yes: -lmvec} \
    %{xgccdriver=*:}\
" LINK_COMMAND_LIB
#endif
#ifndef LINK_COMMAND_GUTS2
#define LINK_COMMAND_GUTS2 "\
    %{!A:%{!nostdlib:%{!nostartfiles:%E}}} %{T*} \
"
#endif

#ifdef __linux__
#define LINK_ANNOTATE_GCCFSS ""
#else
#define LINK_ANNOTATE_GCCFSS \
"  %{xannotate=no: ; \
     xannotate=yes: \
	    -zld32=-S%J/libld_annotate.so -zld64=-S%J/v9/libld_annotate.so ; \
     !xannotate=*: \
	    -zld32=-S%J/libld_annotate.so -zld64=-S%J/v9/libld_annotate.so  \
}"
#endif

/* these options are passed only to iropt and not to ipo */
static const char *iropt_only_options =
" %{xprofile=*: \
        -xlibxprof_tls=yes \
   }";

/* -u* was put back because both BSD and SysV seem to support it.  */
/* %{static:} simply prevents an error message if the target machine
   doesn't handle -static.  */
/* We want %{T*} after %{L*} and %D so that it can be used to specify linker
   scripts which exist in user specified directories, or in standard
   directories.  */
#ifndef LINK_COMMAND_SPEC
#define LINK_COMMAND_SPEC "\
%{!fsyntax-only:%{!c:%{!M:%{!MM:%{!E:%{!S:\
    %{xpec|Zpec=*: %(invoke_ipo2) ; \
      xipo=1|xipo=2: %{!xprofile=collect*: %(invoke_ipo2)} } \
    %{xlinkopt: |\n collect2 --ld-filename %Kpostopt \
                %{v} -optlevel=1  %{save-temps: -keeptmp}\
		%(ssbe_xarch) \
                -startldline " LINKER_PATH LINK_COMMAND_GUTS "\
                      %{xpec: %{save-temps: %b.pec} %{!save-temps: %d%U.pec} } \
                            " LINK_COMMAND_GUTS2 " -stopldline %M; \
      xlinkopt=*: |\n collect2 --ld-filename %Kpostopt \
                %{v} -optlevel=%*  %{save-temps: -keeptmp}\
		%(ssbe_xarch) \
                -startldline " LINKER_PATH LINK_COMMAND_GUTS " \
                      %{xpec: %{save-temps: %b.pec} %{!save-temps: %d%U.pec} } \
                             " LINK_COMMAND_GUTS2 " -stopldline %M ; \
      xbinopt=prepare: |\n collect2 --ld-filename %Kpostopt \
                %{v} -optlevel=0  %{save-temps: -keeptmp}\
		%(ssbe_xarch) \
                -xbinopt=prepare \
                -startldline " LINKER_PATH LINK_COMMAND_GUTS " \
                      %{xpec: %{save-temps: %b.pec} %{!save-temps: %d%U.pec} } \
                             " LINK_COMMAND_GUTS2 " -stopldline %M ; \
              : |\n %(linker) " LINK_COMMAND_GUTS " \
                      %{xpec: %{save-temps: %b.pec} %{!save-temps: %d%U.pec} } \
                      " LINK_ANNOTATE_GCCFSS LINK_COMMAND_GUTS2 " } \
}}}}}}"
#endif

#ifndef LINK_LIBGCC_SPEC
/* Generate -L options for startfile prefix list.  */
# define LINK_LIBGCC_SPEC "%D"
#endif

#ifndef STARTFILE_PREFIX_SPEC
# define STARTFILE_PREFIX_SPEC ""
#endif

#ifndef SYSROOT_SPEC
# define SYSROOT_SPEC "--sysroot=%R"
#endif

#ifndef SYSROOT_SUFFIX_SPEC
# define SYSROOT_SUFFIX_SPEC ""
#endif

#ifndef SYSROOT_HEADERS_SUFFIX_SPEC
# define SYSROOT_HEADERS_SUFFIX_SPEC ""
#endif

static const char *asm_debug;
static const char *cpp_spec = CPP_SPEC;
static const char *cc1_spec = CC1_SPEC;
static const char *cc1plus_spec = CC1PLUS_SPEC;
static const char *link_gcc_c_sequence_spec = LINK_GCC_C_SEQUENCE_SPEC;
static const char *link_ssp_spec = LINK_SSP_SPEC;
static const char *asm_spec = ASM_SPEC;
static const char *asm_final_spec = ASM_FINAL_SPEC;
static const char *link_spec = LINK_SPEC;
static const char *lib_spec = LIB_SPEC;
static const char *mfwrap_spec = MFWRAP_SPEC;
static const char *mflib_spec = MFLIB_SPEC;
static const char *link_gomp_spec = "";
static const char *libgcc_spec = LIBGCC_SPEC;
static const char *endfile_spec = ENDFILE_SPEC;
static const char *startfile_spec = STARTFILE_SPEC;
static const char *switches_need_spaces = SWITCHES_NEED_SPACES;
static const char *linker_name_spec = LINKER_NAME;
static const char *link_command_spec = LINK_COMMAND_SPEC;
static const char *link_libgcc_spec = LINK_LIBGCC_SPEC;
static const char *startfile_prefix_spec = STARTFILE_PREFIX_SPEC;
static const char *sysroot_spec = SYSROOT_SPEC;
static const char *sysroot_suffix_spec = SYSROOT_SUFFIX_SPEC;
static const char *sysroot_hdrs_suffix_spec = SYSROOT_HEADERS_SUFFIX_SPEC;

/* Standard options to cpp, cc1, and as, to reduce duplication in specs.
   There should be no need to override these in target dependent files,
   but we need to copy them to the specs file so that newer versions
   of the GCC driver can correctly drive older tool chains with the
   appropriate -B options.  */

/* When cpplib handles traditional preprocessing, get rid of this, and
   call cc1 (or cc1obj in objc/lang-specs.h) from the main specs so
   that we default the front end language better.  */
static const char *trad_capable_cpp =
"cc1 -E %{traditional|ftraditional|traditional-cpp:-traditional-cpp}";

/* We don't wrap .d files in %W{} since a missing .d file, and
   therefore no dependency entry, confuses make into thinking a .o
   file that happens to exist is up-to-date.  */
static const char *cpp_unique_options =
"%{C|CC:%{!E:%eGCC does not support -C or -CC without -E}}\
 %{!Q:-quiet} %{nostdinc*} %{C} %{CC} %{v} %q %{I*&F*} %{P} %I\
 %{MD:-MD %{!o:%b.d}%{o*:%.d%*}}\
 %{MMD:-MMD %{!o:%b.d}%{o*:%.d%*}}\
 %{M} %{MM} %{MF*} %{MG} %{MP} %{MQ*} %{MT*}\
 %{!E:%{!M:%{!MM:%{!MT:%{!MQ:%{MD|MMD:%{o*:-MQ %*}}}}}}}\
 %{remap} %{g3|ggdb3|gstabs3|gcoff3|gxcoff3|gvms3:-dD}\
 %{H} %C %{D*&U*&A*} %{i*} %Z %i\
 %{fmudflap:-D_MUDFLAP -include mf-runtime.h}\
 %{fmudflapth:-D_MUDFLAP -D_MUDFLAPTH -include mf-runtime.h}\
 %{E|M|MM:%W{o*}} \
 %{m32: %{Zarchm32=v8plusc: -D__FP_FAST_FMA__ -D__FP_FAST_FMAF__ } \
        %{Zarchm32=v8plusd: -D__FP_FAST_FMA__ -D__FP_FAST_FMAF__ } ; \
   m64: %{Zarchm64=v9c: -D__FP_FAST_FMA__ -D__FP_FAST_FMAF__ } \
        %{Zarchm64=v9d: -D__FP_FAST_FMA__ -D__FP_FAST_FMAF__ } } \
 %:prod-dir-include() \
 %{!frtl-backend: %:add-sun-prefetch()} \
";

/* This contains cpp options which are common with cc1_options and are passed
   only when preprocessing only to avoid duplication.  We pass the cc1 spec
   options to the preprocessor so that it the cc1 spec may manipulate
   options used to set target flags.  Those special target flags settings may
   in turn cause preprocessor symbols to be defined specially.  */
static const char *cpp_options =
"%(cpp_unique_options) %1 %{m*} %{std*&ansi&trigraphs} %{W*&pedantic*} %{w}\
 %{f*} %{g*:%{!g0:%{!fno-working-directory:-fworking-directory}}} %{O*}\
 %{undef} %{save-temps:-fpch-preprocess} %{xopenmp*}";

/* This contains cpp options which are not passed when the preprocessor
   output will be used by another program.  */
static const char *cpp_debug_options = "%{d*}";

/* These are options not supported by cross compiler on x86 */
#ifdef CROSS_DIRECTORY_STRUCTURE
#define UNSUPPORTED_OPTIONS_SPEC \
"%{xlinkopt: %e-xlinkopt not supported by cross compiler } \
"
#else
#define UNSUPPORTED_OPTIONS_SPEC
#endif

#define COMMON_CC1_OPTIONS_SPEC \
"%{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
 %1 %{!Q:-quiet} -dumpbase %B %{d*} %{m*} %{a*}\
 %{c|S:%{o*:-auxbase-strip %*}%{!o*:-auxbase %b}}%{!c:%{!S:-auxbase %b}}\
 %{g*} %{O*} %{W*&pedantic*} %{w} %{std*&ansi&trigraphs}\
 %{v:-version} %{pg:-p} %{p} \
 %{fstrict-aliasing: %{Zalias_level=*: %<Zalias_level=*  -xalias_level=basic} \
        %{!Zalias_level=*: -xalias_level=basic} \
        %{xalias_level=*: %<xalias_level=* %Nboth -fstrict-aliasing and -xalias_level can not be specified, -xalias_level ignored} }\
 %{fno-strict-aliasing: %{Zalias_level=*: %<Zalias_level=* -xalias_level=any} \
        %{!Zalias_level=*: -xalias_level=any } \
        %{xalias_level=*: %<xalias_level=* %Nboth -fno-strict-aliasing and -xalias_level can not be specified, -xalias_level ignored} }\
 %{f*} \
 %{g*:-cmdline %:print-orig-cmdline()} \
 %{Zsunir-backend: %:verify-sunir-backend()} \
 %{Zignore: }\
 %{Zfprofile-generate: %{frtl-backend: -fprofile-generate}}  \
 %{Zfprofile-use: %{frtl-backend: -fprofile-use}  }\
 %{Zfprofile-arcs: %{frtl-backend: -fprofile-arcs}}  \
 %{Zfprofile-values: %{frtl-backend: -fprofile-values}}  \
 %{Zfbranch-probabilities: %{frtl-backend: -fbranch-probabilities}}  \
 %{Zfvpt: %{frtl-backend: -fvpt}}  \
 %{undef}\
 %{Qn:-fno-ident} %{--help:--help}\
 %{--target-help:--target-help}\
 %{fsyntax-only:-o %j} %{-param*}\
 %{xcode=pic13: -fpic} \
 %{xcode=pic32: -fPIC} \
 %{xhwcprof | xhwcprof=enable: -g} \
 %{Zfast: -O3} \
 %{xrestrict*} \
 %{xinline=@auto*: -xinline=%%auto%*; \
   xinline=@none : ; \
   xinline=* : -xinline=%*} \
 %{xmemalign=4s: -mno-integer-ldd-std} \
 %{xarch=*: %Ninvalid value for -xarch, option is ignored}\
 %{xO0: %N`-xO0' is not a valid option, option is ignored}\
 %{xO1: %N`-xO1' is not a valid option, option is ignored}\
 %{xO2: %N`-xO2' is not a valid option, option is ignored}\
 %{xO3: %N`-xO3' is not a valid option, option is ignored}\
 %{xO4: %N`-xO4' is not a valid option, option is ignored}\
 %{xO5: %N`-xO5' is not a valid option, option is ignored}\
 %{xipo=1|xipo=2: \
     %{O    : %<xipo* %N`-xipo' requires `-O3' or `-fast', option is ignored;\
       O0   : %<xipo* %N`-xipo' requires `-O3' or `-fast', option is ignored;\
       O1   : %<xipo* %N`-xipo' requires `-O3' or `-fast', option is ignored;\
       O2   : %<xipo* %N`-xipo' requires `-O3' or `-fast', option is ignored;\
       Os   : %<xipo* %N`-xipo' requires `-O3' or `-fast', option is ignored;\
       O3   : ; \
       O*   : ; \
       Zfast: ; \
         : %<xipo* %N`-xipo' requires `-O3' or `-fast', option is ignored} }\
 %{xautopar: \
     %{O    : %<xautopar %N`-xautopar' requires `-O2', `-O3' or `-fast', option is ignored;\
       O0   : %<xautopar %N`-xautopar' requires `-O2', `-O3' or `-fast', option is ignored;\
       O1   : %<xautopar %N`-xautopar' requires `-O2', `-O3' or `-fast', option is ignored;\
       O2   : ; \
       Os   : ; \
       O3   : ; \
       O*   : ; \
       Zfast: ; \
         : %<xautopar %N`-xautopar' requires `-O2', `-O3' or `-fast', option is ignored} }\
 %{xopenmp} \
 %{xopenmp=parallel} \
 %{xopenmp=noopt} \
 %{xopenmp=none} \
 %{tm_mode} \
 %{tm_mode=none} \
 %{tm_mode=htm} \
 %{tm_mode=phtm} \
 %{tm_mode=stm} \
 %{xexplicitpar:} \
 %{xprofile=*: \
    %{O: ;\
      O0: %{!ftest-coverage: -O1 %N`-xprofile' requires `-O1', `-O2', `-O3' or `-fast', assuming `-O1' option} ;\
      O1: ;\
      O2: ;\
      Os: ;\
      O3: ;\
      O*: ;\
      Zfast: ;\
        : %{!ftest-coverage: -O1 %N`-xprofile' requires `-O1', `-O2', `-O3' or `-fast', assuming `-O1' option} } }\
 %{ftest-coverage: %{xipo*: %<xipo* %e`-ftest-coverage' and `-xipo' are incompatible}}\
 %{fmudflap|fmudflapth:-fno-builtin -fno-merge-constants}"

/* NB: This is shared amongst all front-ends.  */
static const char *cc1_options =
"%{pg:%{fomit-frame-pointer:%e-pg and -fomit-frame-pointer are incompatible}}\
 %1 %{!Q:-quiet} -dumpbase %B %{d*} %{m*} %{a*}\
 %{c|S:%{o*:-auxbase-strip %*}%{!o*:-auxbase %b}}%{!c:%{!S:-auxbase %b}}\
 %{g*} %{O*} %{W*&pedantic*} %{w} %{std*&ansi&trigraphs}\
 %{v:-version} %{pg:-p} %{p} %{f*} %{undef}\
 %{Qn:-fno-ident} %{--help:--help}\
 %{--target-help:--target-help}\
 %{--help=*:--help=%(VALUE)}\
 %{!fsyntax-only:%{S:%W{o*}%{!o*:-o %b.s}}}\
 %{fsyntax-only:-o %j} %{-param*}\
 %{fmudflap|fmudflapth:-fno-builtin -fno-merge-constants}\
 %{coverage:-fprofile-arcs -ftest-coverage}";

/* NB: This is shared amongst front-ends which invoke IR backend. 
   Now they are C, C++. */
static const char *gccfss_cc1_options =
 UNSUPPORTED_OPTIONS_SPEC
 COMMON_CC1_OPTIONS_SPEC
"%{xipo=1|xipo=2: -ftree-ir-crossfile} \
 %{Zfsimple*: } \
 %{xalias_level=*} \
 %{Zalias_level=*: -xalias_level=%* }\
 %{xdebugformat=dwarf: %{g*:-gdwarf-2}} \
 %{xpagesize=*: -xpagesize_heap=%* -xpagesize_stack=%* }\
 %{xpagesize_heap=*} %{xpagesize_stack=*} \
 %{m32: -m32 %{Zarchm32=v8plusc: -D__FP_FAST_FMA__ -D__FP_FAST_FMAF__ } ; \
   m64: -m64 %{Zarchm64=v9c: -D__FP_FAST_FMA__ -D__FP_FAST_FMAF__ } ; \
      : -m32 } \
 %{Zpec=*: %:is-it-pec_dummy() }\
 %{xpec|Zpec=*: -ftree-ir-crossfile } \
 %{!fsyntax-only:  \
              %{frtl-backend: %{S: %{!o*: -o %b.s; : %{o*}} ; : -o %U.s }; \
                            : -o %U.s}\
              -r %{save-temps:%b.ir} %{!save-temps: %d%U.ir} }\
 %{xautopar: -fno-tree-ir-regalloc} \
 %{xreduction: %{!xautopar: %e-xreduction requires -xautopar } }\
 %r \
";

/* the .ir file in this situation is really equivalent to /dev/null 
   but the file needs to be writable */
static const char *oldstyle_cc1_options =
 COMMON_CC1_OPTIONS_SPEC
"-r %d%U.ir %{!fsyntax-only:%{S:%W{o*}%{!o*:-o %b.s}}}"; 

static const char *asm_name =
#ifdef __linux__
"%{xfbe: fbe; xas: as; : as}";
#else
"%{xfbe: fbe; xas: as; : fbe}";
#endif

static const char *asm_options =
"%{--target-help:%:print-asm-header()} "
#if HAVE_GNU_AS
/* If GNU AS is used, then convert -w (no warnings), -I, and -v
   to the assembler equivalents.  */
"%{v} %{w:-W} %{I*} "
#endif
"%a %Y %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c:-o %d%w%u%O}";

static const char *cc1_unique_options =
" %{Xc: %{xc99: -std=c99 ; \
          xc99=all : -std=c99 ; \
          xc99=none : -std=iso9899:199409 ; \
              : -std=iso9899:199409 } }\
";

/* used in config/sparc/sol-bi.h for assembler */
/* match on Zarch, Zarchm32, Zarchm64 to prevent warning about not using it */
static const char *ssbe_xarch =
"%{m32: %{Zarchm32=v8plusd: -xarch=sparcima ; \
          Zarchm32=*: -xarch=%* ; \
                    : -xarch=v8plus} ;\
   m64: %{Zarchm64=v9d: -xarch=sparcima ;\
          Zarchm64=*: -xarch=%* ; \
                    : -xarch=v9} ;\
      : %{Zarchm32=v8plusd: -xarch=sparcima ; \
          Zarchm32=*: -xarch=%*; \
                    : -xarch=v8plus} } \
 %{Zarch=*: } %{Zarchm32=*: } %{Zarchm64=*: }";

/* The following two variables are defined in the language specific 
   spec files.  They hold language specific flags passed to iropt and cg */
/* for C */
const char *ssiropt_lang_spec = 
"%{fexceptions: -h_exception=gcc} \
";
const char *sscg_lang_spec =
"%{fexceptions: -h_exception=gcc} \
";           

/* for C++ */
const char *ssiropt_lang_spec_gxx = 
"-Rclone \
%{ fexceptions: -h_exception=gpp; \
    fno-exceptions: ; \
               : -h_exception=gpp } \
";

const char *sscg_lang_spec_gxx =
"%{ fexceptions: -h_exception=gpp; \
    fno-exceptions: ; \
               : -h_exception=gpp } \
";

/* for Fortran */
const char *ssiropt_lang_spec_fortran = 
"";
const char *sscg_lang_spec_fortran =
"";           

/* NB: This is shared amongst all front-ends.  */
static const char *invoke_as =
#ifdef AS_NEEDS_DASH_FOR_PIPED_INPUT
"%{!S:-o %|.s |\n as %(asm_options) %|.s %A }";
#else
"%{!S:-o %|.s |\n as %(asm_options) %m.s %A }";
#endif

/* NB: This is shared amongst front-ends which invoke IR backend. 
   Now they are C, C++. */
static const char *gccfss_invoke_as =
#ifdef AS_NEEDS_DASH_FOR_PIPED_INPUT
"%{!S: |\n %(asm_name) %(asm_options)  \
     %{xforceas: %U.s; \
       frtl-backend: %U.s ; \
               :%|.s} %A }";
#else
"%{!S: |\n %(asm_name) %(asm_options)  \
     %{xforceas: %U.s ; \
       frtl-backend: %U.s ; \
               :%m.s} %A }";
#endif

static const char *ssbe_xarch_xchip =
"%(ssbe_xarch) \
 %{xchip=realT2: -xchip=ultraT2 ; \
   xchip=*: -xchip=%* ; \
          : -xchip=generic } \
 %{xprefetch=* : -xprefetch=%* } \
 %{xprefetch_level=* : -xprefetch_level=%* }";

/* like ssbe_xarch_xchip but with special handling for ultraT2 for cg */
static const char *sscg_xarch_xchip =
"%(ssbe_xarch) \
 %{xchip=ultraT2: -xchip=rock ; \
   xchip=ultraT2plus: -xchip=rock ; \
   xchip=realT2: -xchip=ultraT2 ; \
   xchip=*: -xchip=%* ; \
          : -xchip=generic } \
 %{xprefetch=* : -xprefetch=%* } \
 %{xprefetch_level=* : -xprefetch_level=%* }";

static const char *ssbe_optlevel =
"%{O:-O3;\
   O0: %{xprofile=* : -O3  ;\
                    : -OO0 -T3 -Qiselect-C0 -Qrm:newregman:coalescing=0 \
                           -gen_loclist_gcc=1} ;\
   O1:-O3;\
   O2:-O3;\
   O3:-O5;\
   Os:-O3; \
   O*:-O5; \
   Zfast:-O5 ;\
     : %{xprofile=* : -O3 ; \
                    : -OO0 -T3 -Qiselect-C0 -Qrm:newregman:coalescing=0 \
                           -gen_loclist_gcc=1} }";

/* some slightly different stuff for iropt */
static const char *ssiropt_optlevel =
"%{O:-O3 %{xinline=*: -I} ;\
   O0: %{xprofile=*: -O3 ; : -O1} %{xinline=*: -I} ;\
   O1:-O3 %{xinline=*: -I} ;\
   O2:-O3 %{xinline=*: -I} ;\
   O3:-O5 %{xinline=@auto*: ; \
            xinline=*: -I} ;\
   Os:-O3 %{xinline=*: -I} ; \
   O*:-O5 %{xinline=@auto*: ; \
            xinline=*: -I} ; \
   Zfast: -O5 %{xinline=@auto*: ; \
                xinline=*: -I} ;\
     : %{xprofile=*: -O3 ; : -O1} %{xinline=*: -I} } ";

static const char *xtarget =
"%{xtarget=*: %einvalid value of <target> in -xtarget=<target>}";

static const char *iropt_ipo_options =
" %(ssbe_xarch_xchip) %(xtarget) %(ssiropt_optlevel) \
 %{m64} \
 %{xprofile=use=* | Zprofile=use=*: \
        -xlibxprof \
        %{c:%{!o*:-oo %b.o}     \
                  %{o* :-oo %*  }}   \
        %{!c: %{Zpec=*:-oo %d%w%U%O;  \
                      :-oo %b.o} }  \
	 -xprofile=use:%+profile%* } \
 %{xprofile=collect=*: \
        -xlibxprof \
        %{c:%{!o*:-oo %b.o}     \
                  %{o* :-oo %*  }}   \
        %{!c: %{Zpec=*:-oo %d%w%U%O;  \
                      :-oo %b.o} }  \
	-xprofile=collect:%+profile%* }   \
 %{Zfprofile-arcs: \
	%{Zfprofile-values: ; \
	  Zfvpt: ; \
              : -Rvp -Ricall_opt } } \
 %{xsafe=mem} %{xsafe=unboundsym}\
 %{!xvector=*: -xvector=no }\
 %{xvector=*: -xvector=%* }\
 %{xbuiltin=*: -xbuiltin=%*; :  -xbuiltin=%%none} \
 %{xautopar : -xautopar -mt %{xreduction: -reduction} }\
 %{xexplicitpar : -xexplicitpar -mt }\
 %{xopenmp|xopenmp=parallel|xopenmp=noopt|xexplicitpar|fopenmp : \
              -xexplicitpar -mt \
              %{O | O1 | O2 | O3  | Zfast | Os: ;\
                O0: -xopenmp=noopt ;\
                O*: ; \
                  : -xopenmp=noopt} } \
 %{tm_mode: -tm_mode } \
 %{tm_mode=none: -tm_mode=none } \
 %{tm_mode=htm: -tm_mode=htm } \
 %{tm_mode=phtm: -tm_mode=phtm } \
 %{tm_mode=stm: -tm_mode=stm } \
 %{xloopinfo: -looptable}\
 %{Zmt: -mt} \
 %{g0: ; g*:-g} \
 %{Zfsimple=*: -fsimple=%*}  %{Zfsimple:-fsimple} %{Zftrap=* : -ftrap=%* }\
 %{xlibmopt: -xlibmopt } \
 %{Zfns=yes: -fns } %{Zfns=no: } \
 %{xalias_level=*} %{Zalias_level=*: -xalias_level=%*}\
 %{fstrict-aliasing: -xalias_level=basic }\
 %{fno-strict-aliasing: -xalias_level=any }\
 %{fno-optimize-sibling-calls: -Rtail }\
 %{xdepend=yes: -depend ; \
   xdepend=no: }\
 %{xcache=* : -xcache=%* ; \
            : -xcache=generic }\
 %{xcode=pic32:-k} %{xcode=pic13: -k}\
 %{xhwcprof=enable: -xhwcprof} %{xhwcprof} \
 %{xsafe=unboundsym} \
 %{xspace} \
 %{xinstrument=datarace} \
 %{xprefetch_auto_type=* } \
 -Qy -h_gcc \
 %{Zquietdriver: } \
 %{fwrapv: -xwrap_int }\
";

static const char *mvis_il =
"%{mvis: %{xarch=v8plusa: -il %J/v8plusa/vis.il } \
         %{xarch=v8plusb: -il %J/v8plusb/vis.il } \
         %{xarch=v8plusc: -il %J/v8plusc/vis.il } \
         %{xarch=v8plusd: -il %J/v8plusd/vis.il } \
         %{xarch=v9: -il %J/v9/vis.il } \
         %{xarch=v9a: -il %J/v9a/vis.il } \
         %{xarch=v9b: -il %J/v9b/vis.il } \
         %{xarch=v9c: -il %J/v9c/vis.il } \
         %{xarch=v9d: -il %J/v9d/vis.il } \
         %{xarch=v7: %e-mvis and -xarch=v7 are incompatable}\
         %{xarch=v8: %e-mvis and -xarch=v8 are incompatable}\
         %{xarch=v8plus: %e-mvis and -xarch=v8plus are incompatable}\
         %{xarch=sparcima: \
              %{m64: -il %J/sparcima/64/vis.il ; \
                   : -il %J/sparcima/vis.il } } \
         %{!xarch=*: %{m64: -il %J/v9a/vis.il ; \
                             : -il %J/v8plusa/vis.il } } }";
#ifdef __linux__
#define CG_IPO_OPTIONS \
" %{xannotate*: %N-xannotate is not supported}"
#else // sparc
#define CG_IPO_OPTIONS \
" %{xannotate=yes: -xannotate=yes; \
    xannotate=no: ; \
    !xannotate: -xannotate=yes} "
#endif

static const char *cg_ipo_options =
 CG_IPO_OPTIONS
"-Qy %(sscg_xarch_xchip) %(xtarget) \
 %{m32} %{m64} \
 %{g0: ; g*:-g -gen_loclist_gcc=1} \
 %(ssbe_optlevel) \
 %{xcode=*} \
 %{!xcode=*: %{mcmodel=medlow: -xcode=abs32; \
                   : %{m64: -xcode=abs44 ; \
                       m32: -xcode=abs32 ; \
                          : -xcode=abs32 } }}\
 %{xcache=* : -xcache=%* ; \
            : -xcache=generic }\
 %{xcheck : -xcheck=%%all } %{xcheck=*} \
 %{xsafe=mem} \
 %{xspace} \
 %{p} %{pg} \
 %{xmemalign=* : -xmemalign=%* } \
 %{!xmemalign=*: \
      %{mno-integer-ldd-std: %{m64: -xmemalign=8s; \
                               m32: -xmemalign=4s; \
                                  : -xmemalign=4s} ; \
                           : %{m64: -xmemalign=8s ; \
                               m32: -xmemalign=8i ;\
                                   : -xmemalign=8i }}} \
 %{m64 : -il %F/"
 #ifdef __linux__
  "64"
 #else
  "sparcv9"
 #endif
  "/gccbuiltins.il ;\
   m32 : -il %F/gccbuiltins.il ;\
       : -il %F/gccbuiltins.il } \
 %{Zfma=*: -fma=%* } \
 %{xlibmil: \
    %{m64: %{Zarchm64=v9: -il %J/v9/libm.il;\
             Zarchm64=v9a: -il %J/v9a/libm.il; \
             Zarchm64=v9b: -il %J/v9b/libm.il; \
             Zarchm64=v9c: -il %J/v9c/libm.il; \
             Zarchm64=v9d: -il %J/v9d/libm.il; \
                         : -il %J/v9/libm.il} ; \
      m32: %{Zarchm32=v7: -il %J/v7/libm.il;\
             Zarchm32=v8: -il %J/v8/libm.il;\
             Zarchm32=v8plus: -il %J/v8plus/libm.il;\
             Zarchm32=v8plusa: -il %J/v8plusa/libm.il;\
             Zarchm32=v8plusb: -il %J/v8plusb/libm.il;\
             Zarchm32=v8plusc: -il %J/v8plusc/libm.il;\
             Zarchm32=v8plusd: -il %J/v8plusd/libm.il;\
                        : -il %J/libm.il};\
         : %{Zarch=v7: -il %J/v7/libm.il;\
             Zarch=v8: -il %J/v8/libm.il;\
             Zarch=v8plus: -il %J/v8plus/libm.il;\
             Zarch=v8plusa: -il %J/v8plusa/libm.il;\
             Zarch=v8plusb: -il %J/v8plusb/libm.il;\
             Zarch=v8plusc: -il %J/v8plusc/libm.il;\
             Zarch=v8plusd: -il %J/v8plusd/libm.il;\
                          : -il %J/libm.il} } }\
 %(mvis_il) \
 %{xvector*} %{!xvector* : -xvector=no } \
 %{xthreadvar*} \
 %{!xthreadvar* : %{xcode=pic32|xcode=pic13 : -xthreadvar=dynamic ; \
                                            : -xthreadvar=no%%dynamic }} \
 %{xbuiltin=* : -xbuiltin=%* ; : -xbuiltin=%%none} \
 %{xlibmopt: -xlibmopt } \
 %{Zfsimple*: -fsimple%*} \
 %{Zfns=yes: -fns }\
 %{xautopar|fopenmp|xopenmp|xopenmp=parallel|xopenmp=noopt|Zmt : -mt }\
 %{fno-jump-tables: -Qiselect-sw_table=0}\
 %{xprefetch_auto_type=* } \
 %{xlinkopt} %{xlinkopt=*}\
 %{xbinopt=noprepare: } \
 %{xbinopt=prepare} \
 %{xprofile=use*: -ip } \
 %{xprofile=collect=*: } \
 %{xhwcprof} %{xhwcprof=enable: -xhwcprof}\
 %{xunroll=*} \
 %{xsafe=unboundsym} \
 %{fno-got: -Qiselect-gd_enable=0} \
 %{xregs=*} \
 %{mapp-regs: -xregs=appl} \
 %{mno-app-regs: -xregs=no%%appl} \
 %{mfpu: -xregs=float} \
 %{mno-fpu: -xregs=no%%float} \
 %{fno-unit-at-a-time: -Qassembler-ounrefsym=0; \
   funit-at-a-time: ;\
                  : %{O: -Qassembler-ounrefsym=0; \
		      O0: -Qassembler-ounrefsym=0; \
                      O1: -Qassembler-ounrefsym=0; \
                      O*:  ; \
                      Zfast: ; \
                        : -Qassembler-ounrefsym=0} }\
 %{foptimize-sibling-calls: -Qiselect-T1; \
   fno-optimize-sibling-calls: -Qiselect-T0; \
                  : %{O: -Qiselect-T0; \
		      O0: -Qiselect-T0; \
                      O1: -Qiselect-T0; \
                      O*: -Qiselect-T1; \
                      Zfast: -Qiselect-T1; \
                        : -Qiselect-T0} }\
 -Qassembler-I -Qassembler-U \
 "
#if USE_GNU_LD
 " -h_gcc"
#else
 " -comdat -h_gcc"
#endif
 ;

/* the namelist file which is the -N option to iropt and -n option to ipo
   is not created by the sgcc front end so use /dev/null */
static const char *invoke_iropt =
"|\n iropt -F %(iropt_ipo_options) %(iropt_only_options) \
 -o %{save-temps: %b.ircg} %{!save-temps: %d%u.ircg} \
  %{save-temps:%b.ir} %{!save-temps:%d%U.ir} \
 -N/dev/null \
 -is %U.s \
 %{xipo=1|xipo=2|xpec|Zpec=*: %{!xprofile=collect*: %{Zipo_fast_phase_1: -O0 } } } ";

static const char *invoke_cg = 
"|\n cg %:add-user-il-routines() %(cg_ipo_options) \
 -is %U.s \
 -ir %{save-temps:%b.ircg} %{!save-temps:%d%U.ircg} \
 %{!xforceas: \
     %{!S: %{c:%{!o*:-oo %w%b%O}%W{o*:-oo %*}}%{!c: %{Zpec=*: -oo %d%w%U%O; :-oo %d%w%u%O} }} \
     %{S: %W{s*} %{!o*:-os %w%b.s} %{o*:-os %*} } }\
 %{xforceas: \
     %{!S: %{!o*: -os %w%u.s} %{o*: -os %w%u.s} } \
     %{S: %W{s*} %{!o*:-os %w%b.s} %{o*:-os %*} } }\
 %{xlinkopt: -xlinkopt=1 ; \
   xlinkopt=*: -xlinkopt=%* } \
 %{xipo=1|xipo=2|xpec|Zpec=*: %{!xprofile=collect*: -xcrossfile=1 %{Zipo_fast_phase_1: -OO0} } } ";

static const char *invoke_ipo1 = 
"|\n ipo -phase1 %{v: -#} %{save-temps: -keeptmp}\
 %{xprofile=collect=*:-level=0 ; \
   xipo=1:-level=1 ; \
   xipo=2:-level=2 ; \
         :-level=0 } \
 %{xjob=*:-j %* }\
 %{Ztime: -xtime=1 } \
 %{xpec:-pec -cmdline %:print-orig-cmdline() %i}\
 %{!S: %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c: -o %d%U%O}} \
 -a %U.s \
 -ir %d%U.ir \
 -n /dev/null \
 -ipo_iroptoption_start %(iropt_ipo_options) %(ssiropt_spec) %Q -ipo_iroptoption_end \
 -ipo_cgoption_start -xcrossfile=1 %(cg_ipo_options) %(sscg_spec) %T -ipo_cgoption_end %P\
 ";

static const char *invoke_fortranipo1 = 
"|\n ipo -phase1 %{v: -#} %{save-temps: -keeptmp}\
 %{xprofile=collect=*:-level=0 ; \
   xipo=1:-level=1 ; \
   xipo=2:-level=2 ; \
         :-level=0 } \
 %{xjob=*:-j %* }\
 %{Ztime: -xtime=1 } \
 %{xpec:-pec -cmdline %:print-orig-cmdline() %i}\
 %{!S: %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c: -o %d%U%O}} \
 -a %U.s \
 -ir %d%U.ir \
 -n /dev/null \
 -ipo_iroptoption_start %(iropt_ipo_options) %(ssiropt_spec_fortran) %Q -ipo_iroptoption_end \
 -ipo_cgoption_start -xcrossfile=1 %(cg_ipo_options) %(sscg_spec_fortran) %T -ipo_cgoption_end %P\
 ";

static const char *invoke_cppipo1 = 
"|\n ipo -phase1 %{v: -#} %{save-temps: -keeptmp}\
 %{xipo=1:-level=1 ; \
   xipo=2:-level=2 ; \
         :-level=0 } \
 %{xpec:-pec -cmdline %:print-orig-cmdline() %i}\
 %{!S: %{c:%W{o*}%{!o*:-o %w%b%O}}%{!c: -o %d%U%O}} \
 -a %U.s \
 -ir %d%U.ir \
 -n /dev/null \
 -ipo_iroptoption_start %(iropt_ipo_options) %(ssiropt_spec_gxx) %Q -ipo_iroptoption_end \
 -ipo_cgoption_start -xcrossfile=1  %(cg_ipo_options) %(sscg_spec_gxx) %T -ipo_cgoption_end %P\
 ";

static const char *invoke_ipo2 = 
"|\n ipo -phase2 %{v: -#} %{save-temps: -keeptmp}\
 %{xprofile=collect=*:-level=0 ; \
   xipo=1:-level=1 ; \
   xipo=2:-level=2 ; \
         :-level=0 } \
 %{xjobs=*:-j %* } \
 %{Ztime: -xtime=1 } \
 %{xpec:-pec -cmdline %:print-orig-cmdline() X \
        -temp %{save-temps: %w%b.pec} %{!save-temps: %w%d%U.pec} } \
 %{Zpec=*:-i %* -cmdline %:print-orig-cmdline() X \
          -temp %{save-temps: %w%U%O} %{!save-temps: %w%d%U%O} } \
 %{xipo_archive=readonly} %{xipo_archive=writeback} \
 -iroptpath %Kiropt -cgpath %Kcg \
 -ipo_ldarg_start " LINKER_PATH LINK_COMMAND_GUTS LINK_COMMAND_GUTS2 " -ipo_ldarg_end %P\
 ";


/* Some compilers have limits on line lengths, and the multilib_select
   and/or multilib_matches strings can be very long, so we build them at
   run time.  */
static struct obstack multilib_obstack;
static const char *multilib_select;
static const char *multilib_matches;
static const char *multilib_defaults;
static const char *multilib_exclusions;

/* Check whether a particular argument is a default argument.  */

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "" }
#endif

static const char *const multilib_defaults_raw[] = MULTILIB_DEFAULTS;

#ifndef DRIVER_SELF_SPECS
#define DRIVER_SELF_SPECS ""
#endif

/* Adding -fopenmp should imply pthreads.  This is particularly important
   for targets that use different start files and suchlike.  */
#ifndef GOMP_SELF_SPECS
#define GOMP_SELF_SPECS "%{fopenmp|ftree-parallelize-loops=*: -pthread}"
#endif

static const char *const driver_self_specs[] = {
  DRIVER_SELF_SPECS, GOMP_SELF_SPECS
};

#ifndef OPTION_DEFAULT_SPECS
#define OPTION_DEFAULT_SPECS { "", "" }
#endif

struct default_spec
{
  const char *name;
  const char *spec;
};

static const struct default_spec
  option_default_specs[] = { OPTION_DEFAULT_SPECS };

struct user_specs
{
  struct user_specs *next;
  const char *filename;
};

static struct user_specs *user_specs_head, *user_specs_tail;

#ifndef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR) DEFAULT_SWITCH_TAKES_ARG(CHAR)
#endif

#ifndef WORD_SWITCH_TAKES_ARG
#define WORD_SWITCH_TAKES_ARG(STR) DEFAULT_WORD_SWITCH_TAKES_ARG (STR)
#endif

#ifdef HAVE_TARGET_EXECUTABLE_SUFFIX
/* This defines which switches stop a full compilation.  */
#define DEFAULT_SWITCH_CURTAILS_COMPILATION(CHAR) \
  ((CHAR) == 'c' || (CHAR) == 'S')

#ifndef SWITCH_CURTAILS_COMPILATION
#define SWITCH_CURTAILS_COMPILATION(CHAR) \
  DEFAULT_SWITCH_CURTAILS_COMPILATION(CHAR)
#endif
#endif

/* Record the mapping from file suffixes for compilation specs.  */

struct compiler
{
  const char *suffix;		/* Use this compiler for input files
				   whose names end in this suffix.  */

  const char *spec;		/* To use this compiler, run this spec.  */

  const char *cpp_spec;         /* If non-NULL, substitute this spec
				   for `%C', rather than the usual
				   cpp_spec.  */
  const int combinable;          /* If nonzero, compiler can deal with
				    multiple source files at once (IMA).  */
  const int needs_preprocessing; /* If nonzero, source files need to
				    be run through a preprocessor.  */
};

/* Pointer to a vector of `struct compiler' that gives the spec for
   compiling a file, based on its suffix.
   A file that does not end in any of these suffixes will be passed
   unchanged to the loader and nothing else will be done to it.

   An entry containing two 0s is used to terminate the vector.

   If multiple entries match a file, the last matching one is used.  */

static struct compiler *compilers;

/* Number of entries in `compilers', not counting the null terminator.  */

static int n_compilers;

/* The default list of file name suffixes and their compilation specs.  */

static const struct compiler default_compilers[] =
{
  /* Add lists of suffixes of known languages here.  If those languages
     were not present when we built the driver, we will hit these copies
     and be given a more meaningful error than "file not used since
     linking is not done".  */
  {".m",  "#Objective-C", 0, 0, 0}, {".mi",  "#Objective-C", 0, 0, 0},
  {".mm", "#Objective-C++", 0, 0, 0}, {".M", "#Objective-C++", 0, 0, 0},
  {".mii", "#Objective-C++", 0, 0, 0},
  {".cc", "#C++", 0, 0, 0}, {".cxx", "#C++", 0, 0, 0},
  {".cpp", "#C++", 0, 0, 0}, {".cp", "#C++", 0, 0, 0},
  {".c++", "#C++", 0, 0, 0}, {".C", "#C++", 0, 0, 0},
  {".CPP", "#C++", 0, 0, 0}, {".ii", "#C++", 0, 0, 0},
  {".ads", "#Ada", 0, 0, 0}, {".adb", "#Ada", 0, 0, 0},
  {".f", "#Fortran", 0, 0, 0}, {".for", "#Fortran", 0, 0, 0},
  {".fpp", "#Fortran", 0, 0, 0}, {".F", "#Fortran", 0, 0, 0},
  {".FOR", "#Fortran", 0, 0, 0}, {".FPP", "#Fortran", 0, 0, 0},
  {".f90", "#Fortran", 0, 0, 0}, {".f95", "#Fortran", 0, 0, 0},
  {".F90", "#Fortran", 0, 0, 0}, {".F95", "#Fortran", 0, 0, 0},
  {".r", "#Ratfor", 0, 0, 0},
  {".p", "#Pascal", 0, 0, 0}, {".pas", "#Pascal", 0, 0, 0},
  {".java", "#Java", 0, 0, 0}, {".class", "#Java", 0, 0, 0},
  {".zip", "#Java", 0, 0, 0}, {".jar", "#Java", 0, 0, 0},
  /* Next come the entries for C.  */
  {".c", "@c", 0, 1, 1},
  {"@c",
   /* cc1 has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps is given.  */
     "%{E|M|MM:%(trad_capable_cpp) %(cpp_options) %(cpp_debug_options)}\
      %{!E:%{!M:%{!MM:\
          %{traditional|ftraditional:\
%eGNU C no longer supports -traditional without -E}\
       %{!combine:\
	  %{save-temps|traditional-cpp|no-integrated-cpp:%(trad_capable_cpp) \
		%(cpp_options) -o %{save-temps:%b.i} %{!save-temps:%g.i} \n\
		    cc1 -fpreprocessed %{save-temps:%b.i} %{!save-temps:%g.i} \
			%(gccfss_cc1_options) %(cc1_unique_options)}\
	  %{!save-temps:%{!traditional-cpp:%{!no-integrated-cpp:\
		cc1 %(cpp_unique_options) %(gccfss_cc1_options) %(cc1_unique_options)}}}\
          %{!fsyntax-only: \
             %{frtl-backend: %(gccfss_invoke_as) ; \
               : %(invoke_iropt) %(ssiropt_spec) %Q \
                 %(invoke_cg) %(sscg_spec) %T } \
                %{!S: \
                  %{!frtl-backend: %{xforceas : %(gccfss_invoke_as)}} \
                  %{Zpec=*: %(invoke_ipo1) ; \
                    xpec: %(invoke_ipo1); \
                    xipo=1|xipo=2:%{!xprofile=collect*:%(invoke_ipo1) } } } }\
           } \
      %{combine:\
	  %{save-temps|traditional-cpp|no-integrated-cpp:%(trad_capable_cpp) \
		%(cpp_options) -o %{save-temps:%b.i} %{!save-temps:%g.i}}\
	  %{!save-temps:%{!traditional-cpp:%{!no-integrated-cpp:\
		cc1 %(cpp_unique_options) %(gccfss_cc1_options) %(cc1_unique_options)}}\
                %{!fsyntax-only:\
                   %{frtl-backend: %(gccfss_invoke_as) ;\
                     : %(invoke_iropt) %(ssiropt_spec) %Q \
                       %(invoke_cg) %(sscg_spec) %T }\
			%{!S: \
                          %{!frtl-backend: %{xforceas: %(gccfss_invoke_as)}} \
                          %{Zpec=*: %(invoke_ipo1) ; \
                            xpec: %(invoke_ipo1); \
                            xipo=1|xipo=2:%{!xprofile=collect*:%(invoke_ipo1) } } }\
           }}}}}}", 0, 1, 1},
  {"-",
   "%{!E:%e-E or -x required when input is from standard input}\
    %(trad_capable_cpp) %(cpp_options) %(cpp_debug_options)", 0, 0, 0},
  {".h", "@c-header", 0, 0, 0},
  {"@c-header",
   /* cc1 has an integrated ISO C preprocessor.  We should invoke the
      external preprocessor if -save-temps is given.  */
     "%{E|M|MM:%(trad_capable_cpp) %(cpp_options) %(cpp_debug_options)}\
      %{!E:%{!M:%{!MM:\
	  %{save-temps|traditional-cpp|no-integrated-cpp:%(trad_capable_cpp) \
		%(cpp_options) -o %{save-temps:%b.i} %{!save-temps:%g.i} \n\
		    cc1 -fpreprocessed %{save-temps:%b.i} %{!save-temps:%g.i} \
			%(oldstyle_cc1_options)\
                        -o %g.s %{!o*:--output-pch=%i.gch}\
                        %W{o*:--output-pch=%*}%V}\
	  %{!save-temps:%{!traditional-cpp:%{!no-integrated-cpp:\
		cc1 %(cpp_unique_options) %(oldstyle_cc1_options)\
                    -o %g.s %{!o*:--output-pch=%i.gch}\
                    %W{o*:--output-pch=%*}%V}}}}}}", 0, 0, 0},
  {".i", "@cpp-output", 0, 1, 0},
  {"@cpp-output",
   "%{!M:%{!MM:%{!E:cc1 -fpreprocessed %i %(gccfss_cc1_options) %(cc1_unique_options) \
                %{!fsyntax-only:\
                  %{frtl-backend: %(gccfss_invoke_as) ; \
                    : %(invoke_iropt) %(ssiropt_spec) %Q \
                      %(invoke_cg) %(sscg_spec) %T }\
		     %{!S: \
                       %{!frtl-backend: %{xforceas: %(gccfss_invoke_as)}} \
                       %{Zpec=*: %(invoke_ipo1) ; \
                         xpec: %(invoke_ipo1); \
                         xipo=1|xipo=2:%{!xprofile=collect*:%(invoke_ipo1) } } }\
               }}}}", 0, 1, 0},
  {".s", "@assembler", 0, 1, 0},
  {"@assembler",
   "%{!M:%{!MM:%{!E:%{!S:%(asm_name) %(asm_debug) %(asm_options) %i %A }}}}", 0, 1, 0},
  {".sx", "@assembler-with-cpp", 0, 1, 0},
  {".S", "@assembler-with-cpp", 0, 1, 0},
  {"@assembler-with-cpp",
#ifdef AS_NEEDS_DASH_FOR_PIPED_INPUT
   "%(trad_capable_cpp) -lang-asm %:reset-xprefetch-explicit() %(cpp_options) -fno-directives-only\
      %{E|M|MM:%(cpp_debug_options)}\
      %{!M:%{!MM:%{!E:%{!S:-o %|.s |\n\
       %(asm_name) %(asm_debug) %(asm_options) %|.s %A }}}}"
#else
   "%(trad_capable_cpp) -lang-asm %:reset-xprefetch-explicit() %(cpp_options) -fno-directives-only\
      %{E|M|MM:%(cpp_debug_options)}\
      %{!M:%{!MM:%{!E:%{!S:-o %|.s |\n\
       %(asm_name) %(asm_debug) %(asm_options) %m.s %A }}}}"
#endif
   , 0, 1, 0},

#include "specs.h"
  /* Mark end of table.  */
  {0, 0, 0, 0, 0}
};

/* Number of elements in default_compilers, not counting the terminator.  */

static const int n_default_compilers = ARRAY_SIZE (default_compilers) - 1;

/* the original command line */
static char **orig_argv;
static int orig_argc;
static const char *print_orig_cmdline(int, const char **);

/* verify backend really exists */
static const char *verify_sunir_backend(int, const char **);

static char *current_work_directory;
static char *path_to_driver;

static int directory_exists(const char *);
static int valid_backend_version(const char *);

static const char *add_user_il_routines(int, const char **);
static int is_il_file(char * );

/* if -Wd,-w then keep the driver quiet from warnings and notes */
static int quiet_driver = 0;

static const char *is_it_pec_dummy(int, const char **);

/* A vector of options to give to the linker.
   These options are accumulated by %x,
   and substituted into the linker command with %X.  */
static int n_linker_options;
static char **linker_options;

/* A vector of options to give to the front-end.
   These options are accumulated by -W0,
   and substituted into the assembler command with %r.  */
static int n_frontend_options;
static char **frontend_options;

/* A vector of options to give to the assembler.
   These options are accumulated by -Wa,
   and substituted into the assembler command with %Y.  */
static int n_assembler_options;
static char **assembler_options;

/* A vector of options to give to the sscg.
   These options are accumulated by -Wc,
   and substituted into the sscg command with %T.  */
static int n_sscg_options;
static char **sscg_options;

/* A vector of options to give to the ipo.
   These options are accumulated by -WO,
   and substituted into the iropt command with %P.  */
static int n_ipo_options;
static char **ipo_options;

/* A vector of options to give to the iropt.
   These options are accumulated by -W2,
   and substituted into the iropt command with %Q.  */
static int n_iropt_options;
static char **iropt_options;

/* A vector of options to give to the postopt
   These options are accumulated by -Wo,
   and substituted into the iropt command with %M.  */
static int n_postopt_options;
static char **postopt_options;

/* A vector of options to give to the preprocessor.
   These options are accumulated by -Wp,
   and substituted into the preprocessor command with %Z.  */
static int n_preprocessor_options;
static char **preprocessor_options;

/* Define how to map long options into short ones.  */

/* This structure describes one mapping.  */
struct option_map
{
  /* The long option's name.  */
  const char *const name;
  /* The equivalent short option.  */
  const char *const equivalent;
  /* Argument info.  A string of flag chars; NULL equals no options.
     a => argument required.
     o => argument optional.
     j => join argument to equivalent, making one word.
     * => require other text after NAME as an argument.  */
  const char *const arg_info;
};

/* This is the table of mappings.  Mappings are tried sequentially
   for each option encountered; the first one that matches, wins.  */

static const struct option_map option_map[] =
 {
   {"--all-warnings", "-Wall", 0},
   {"--ansi", "-ansi", 0},
   {"--assemble", "-S", 0},
   {"--assert", "-A", "a"},
   {"--classpath", "-fclasspath=", "aj"},
   {"--bootclasspath", "-fbootclasspath=", "aj"},
   {"--CLASSPATH", "-fclasspath=", "aj"},
   {"--combine", "-combine", 0},
   {"--comments", "-C", 0},
   {"--comments-in-macros", "-CC", 0},
   {"--compile", "-c", 0},
   {"--debug", "-g", "oj"},
   {"--define-macro", "-D", "aj"},
   {"--dependencies", "-M", 0},
   {"--dump", "-d", "a"},
   {"--dumpbase", "-dumpbase", "a"},
   {"--encoding", "-fencoding=", "aj"},
   {"--entry", "-e", 0},
   {"--extra-warnings", "-W", 0},
   {"--extdirs", "-fextdirs=", "aj"},
   {"--for-assembler", "-Wa", "a"},
   {"--for-linker", "-Xlinker", "a"},
   {"--force-link", "-u", "a"},
   {"--coverage", "-coverage", 0},
   {"--imacros", "-imacros", "a"},
   {"--include", "-include", "a"},
   {"--include-barrier", "-I-", 0},
   {"--include-directory", "-I", "aj"},
   {"--include-directory-after", "-idirafter", "a"},
   {"--include-prefix", "-iprefix", "a"},
   {"--include-with-prefix", "-iwithprefix", "a"},
   {"--include-with-prefix-before", "-iwithprefixbefore", "a"},
   {"--include-with-prefix-after", "-iwithprefix", "a"},
   {"--language", "-x", "a"},
   {"--library-directory", "-L", "a"},
   {"--machine", "-m", "aj"},
   {"--machine-", "-m", "*j"},
   {"--no-integrated-cpp", "-no-integrated-cpp", 0},
   {"--no-line-commands", "-P", 0},
   {"--no-precompiled-includes", "-noprecomp", 0},
   {"--no-standard-includes", "-nostdinc", 0},
   {"--no-standard-libraries", "-nostdlib", 0},
   {"--no-warnings", "-w", 0},
   {"--optimize", "-O", "oj"},
   {"--output", "-o", "a"},
   {"--output-class-directory", "-foutput-class-dir=", "ja"},
   {"--param", "--param", "a"},
   {"--pass-exit-codes", "-pass-exit-codes", 0},
   {"--pedantic", "-pedantic", 0},
   {"--pedantic-errors", "-pedantic-errors", 0},
   {"--pie", "-pie", 0},
   {"--pipe", "-pipe", 0},
   {"--prefix", "-B", "a"},
   {"--preprocess", "-E", 0},
   {"--print-search-dirs", "-print-search-dirs", 0},
   {"--print-file-name", "-print-file-name=", "aj"},
   {"--print-libgcc-file-name", "-print-libgcc-file-name", 0},
   {"--print-missing-file-dependencies", "-MG", 0},
   {"--print-multi-lib", "-print-multi-lib", 0},
   {"--print-multi-directory", "-print-multi-directory", 0},
   {"--print-multi-os-directory", "-print-multi-os-directory", 0},
   {"--print-prog-name", "-print-prog-name=", "aj"},
   {"--print-sysroot-headers-suffix", "-print-sysroot-headers-suffix", 0},
   {"--profile", "-p", 0},
   {"--profile-blocks", "-a", 0},
   {"--quiet", "-q", 0},
   {"--resource", "-fcompile-resource=", "aj"},
   {"--save-temps", "-save-temps", 0},
   {"--shared", "-shared", 0},
   {"--silent", "-q", 0},
   {"--specs", "-specs=", "aj"},
   {"--static", "-static", 0},
   {"--std", "-std=", "aj"},
   {"--symbolic", "-symbolic", 0},
   {"--sysroot", "--sysroot=", "aj"},
   {"--time", "-time", 0},
   {"--trace-includes", "-H", 0},
   {"--traditional", "-traditional", 0},
   {"--traditional-cpp", "-traditional-cpp", 0},
   {"--trigraphs", "-trigraphs", 0},
   {"--undefine-macro", "-U", "aj"},
   {"--user-dependencies", "-MM", 0},
   {"--verbose", "-v", 0},
   {"--warn-", "-W", "*j"},
   {"--write-dependencies", "-MD", 0},
   {"--write-user-dependencies", "-MMD", 0},
   {"--", "-f", "*j"},
   /* because specs can't match on ':' need to change to '=' */
   {"-xprofile=collect:", "-xprofile=collect=", "*j"},
   {"-xprofile=use:", "-xprofile=use=", "*j"},
   /* because specs can't match on '%auto' */
   {"-xinline=%auto", "-xinline=@auto", "*j"},
   /* because TARGET_OPTION_TRANSLATION_TABLE can't handle complex stuff */
   {"-fns", "-Zfns=", "*j"},
   {"-ftrap=", "-Zftrap=", "*j"},
   {"-keeptmp", "-save-temps", 0},
   {"-mt", "-Zmt", 0},
   {"-Wd,-pec","-Zpec=", "*j"}
 };


#ifdef TARGET_OPTION_TRANSLATE_TABLE
/* the definition of this structure has been 
   moved to gccspec.c, g++spec.c, and cppspec */
extern const struct {
  const char *const option_found;
  const char *const replacements;
} target_option_translations[];
#endif

/* Translate the options described by *ARGCP and *ARGVP.
   Make a new vector and store it back in *ARGVP,
   and store its length in *ARGVC.  */

static void
translate_options (int *argcp, const char *const **argvp)
{
  int i;
  int argc = *argcp;
  char **argv = *argvp;
  int newvsize = (argc + 3) * 2 * sizeof (const char *);
  const char **newv = xmalloc (newvsize);
  int newindex = 0;

  i = 0;
  newv[newindex++] = argv[i];
  argv[i] = xstrdup("-mcpu=v9"); /* the default */
  newv[newindex++] = xstrdup("-Zsunir-backend"); /* the default */

  while (i < argc)
    {
#ifdef TARGET_OPTION_TRANSLATE_TABLE
      int tott_idx;

      /* make sure -O4, -O55 are mapped to -O3 as gcc does */
      if (strncmp (argv[i], "-O", 2) == 0)
        {
          char level = argv[i][2];
          if (level != 0
              && level != '0'
              && level != '1'
              && level != '2'
              && level != '3'
              && level != 's')
           argv[i] = "-O3";
        }
      
      for (tott_idx = 0;
	   target_option_translations[tott_idx].option_found;
	   tott_idx++)
	{
	  if (strcmp (target_option_translations[tott_idx].option_found,
		      argv[i]) == 0)
	    {
	      int spaces = 1;
	      const char *sp;
	      char *np;

	      for (sp = target_option_translations[tott_idx].replacements;
		   *sp; sp++)
		{
		  if (*sp == ' ')
		    spaces ++;
		}

	      newvsize += spaces * sizeof (const char *);
	      newv =  xrealloc (newv, newvsize);

	      sp = target_option_translations[tott_idx].replacements;
	      np = xstrdup (sp);

	      while (1)
		{
		  while (*np == ' ')
		    np++;
		  if (*np == 0)
		    break;
		  newv[newindex++] = np;
		  while (*np != ' ' && *np)
		    np++;
		  if (*np == 0)
		    break;
		  *np++ = 0;
		}

	      i ++;
	      break;
	    }
	}
      if (target_option_translations[tott_idx].option_found)
	continue;
#endif

      /* Translate -- options.  and some SunStudio conflicts */
      if (argv[i][0] == '-' &&
          (argv[i][1] == '-'
           || (argv[i][1] == 'x'
               && strlen(argv[i])!=2)
           || (argv[i][1] == 'm'
               && strlen(argv[i])!=2)
           || argv[i][1] == 'f'
           || (strlen(argv[i])>4
               && argv[i][1] == 'W'
               && argv[i][2] == 'd'
               && argv[i][3] == ',') ) )
	{
	  size_t j;
          int prevnewindex = newindex;
	  /* Find a mapping that applies to this option.  */
	  for (j = 0; j < ARRAY_SIZE (option_map); j++)
	    {
	      size_t optlen = strlen (option_map[j].name);
	      size_t arglen = strlen (argv[i]);
	      size_t complen = arglen > optlen ? optlen : arglen;
	      const char *arginfo = option_map[j].arg_info;

	      if (arginfo == 0)
		arginfo = "";

	      if (!strncmp (argv[i], option_map[j].name, complen))
		{
		  const char *arg = 0;

		  if (arglen < optlen)
		    {
		      size_t k;
		      for (k = j + 1; k < ARRAY_SIZE (option_map); k++)
			if (strlen (option_map[k].name) >= arglen
			    && !strncmp (argv[i], option_map[k].name, arglen))
			  {
			    error ("ambiguous abbreviation %s", argv[i]);
			    break;
			  }

		      if (k != ARRAY_SIZE (option_map))
			break;
		    }

		  if (arglen > optlen)
		    {
		      /* If the option has an argument, accept that.  */
		      if (argv[i][optlen] == '=')
			arg = argv[i] + optlen + 1;

		      /* If this mapping requires extra text at end of name,
			 accept that as "argument".  */
		      else if (strchr (arginfo, '*') != 0)
			arg = argv[i] + optlen;

		      /* Otherwise, extra text at end means mismatch.
			 Try other mappings.  */
		      else
			continue;
		    }

		  else if (strchr (arginfo, '*') != 0)
		    {
		      error ("incomplete '%s' option", option_map[j].name);
		      break;
		    }

		  /* Handle arguments.  */
		  if (strchr (arginfo, 'a') != 0)
		    {
		      if (arg == 0)
			{
			  if (i + 1 == argc)
			    {
			      error ("missing argument to '%s' option",
				     option_map[j].name);
			      break;
			    }

			  arg = argv[++i];
			}
		    }
		  else if (strchr (arginfo, '*') != 0)
		    ;
		  else if (strchr (arginfo, 'o') == 0)
		    {
		      if (arg != 0)
			error ("extraneous argument to '%s' option",
			       option_map[j].name);
		      arg = 0;
		    }

		  /* Store the translation as one argv elt or as two.  */
		  if (arg != 0 && strchr (arginfo, 'j') != 0)
		    newv[newindex++] = concat (option_map[j].equivalent, arg,
					       NULL);
		  else if (arg != 0)
		    {
		      newv[newindex++] = option_map[j].equivalent;
		      newv[newindex++] = arg;
		    }
		  else
		    newv[newindex++] = option_map[j].equivalent;

		  break;
		}
	    }
          if (prevnewindex == newindex)
             newv[newindex++] = argv[i]; 
	  i++;
	}

      /* Handle old-fashioned options--just copy them through,
	 with their arguments.  */
      else if (argv[i][0] == '-')
	{
	  const char *p = argv[i] + 1;
	  int c = *p;
	  int nskip = 1;

	  if (SWITCH_TAKES_ARG (c) > (p[1] != 0))
	    nskip += SWITCH_TAKES_ARG (c) - (p[1] != 0);
	  else if (WORD_SWITCH_TAKES_ARG (p))
	    nskip += WORD_SWITCH_TAKES_ARG (p);
	  else if ((c == 'B' || c == 'b' || c == 'x')
		   && p[1] == 0)
	    nskip += 1;
	  else if (! strcmp (p, "Xlinker"))
	    nskip += 1;
	  else if (! strcmp (p, "Xpreprocessor"))
	    nskip += 1;
	  else if (! strcmp (p, "Xassembler"))
	    nskip += 1;

	  /* Watch out for an option at the end of the command line that
	     is missing arguments, and avoid skipping past the end of the
	     command line.  */
	  if (nskip + i > argc)
	    nskip = argc - i;

	  while (nskip > 0)
	    {
	      newv[newindex++] = argv[i++];
	      nskip--;
	    }
	}
      else
	/* Ordinary operands, or +e options.  */
	newv[newindex++] = argv[i++];
    }

  newv[newindex] = 0;
  
  if ((debug_driver_val & 0x01)) {
     fprintf(stdout,"IN TRANSLATE_OPTIONS\n");
     { int i;
       for (i=0; i<argc; i++)
        fprintf(stdout,"argv[%d]=%s\n",i,argv[i]);
     }


     { int i;
       for (i=0; i<newindex; i++)
        fprintf(stdout,"newv[%d]=%s\n",i,newv[i]);
    }
  }
  
  *argvp = newv;
  *argcp = newindex;
}

static int xprefetch_auto = 1; /* 1=> xprefetch=auto 0=> xprefetch=no%auto */
static int xprefetch_explicit = 1; /* likewise for explicit */
static char *xprefetch_latx = NULL;

static const char *reset_xprefetch_explicit (int dummy __attribute__ ((unused)),
                           const char** dummy2 __attribute__ ((unused)) ) {
  xprefetch_explicit = 0;
  return xstrdup(" ");
}

static const char *add_sun_prefetch(int dummy __attribute__ ((unused)), 
                           const char** dummy2 __attribute__ ((unused)) ) {
  if (xprefetch_explicit == 1) 
     return xstrdup("-D__SUN_PREFETCH");
  else
     return xstrdup(" ");
}

static int
crack_xprefetch (char * p)
{
   char *e, *p1;
   int badval = 0;

   /* find the various values on -xprefetch=.... 
      start with the 11th char of the string to strip off -xprefetch= */
   p1 = p;
   p += 11;

   while ((e = strchr (p, ','))) /* values are comma delimited */
   {
      if (strncmp( p, "auto", 4) == 0)
        xprefetch_auto = 1;
      else if (strncmp ( p, "no%auto", 7) == 0)
        xprefetch_auto = 0;
      else if (strncmp ( p, "explicit", 8) == 0)
        xprefetch_explicit = 1;
      else if (strncmp ( p, "no%explicit", 11) == 0)
        xprefetch_explicit = 0;
      else if (strncmp ( p, "latx:", 5) == 0) {
        *e = 0; /* make it easier to duplicate string */
        xprefetch_latx = xstrdup( p );
      }
      else {
        *e = 0;
        error ("%s is an invalid value for -xprefetch", p );
        badval = 1;
      }
      p = e + 1; /* skip over "," */
   }
   if (strncmp( p, "auto", 4) == 0)
     xprefetch_auto = 1;
   else if (strncmp ( p, "no%auto", 7) == 0)
     xprefetch_auto = 0;
   else if (strncmp ( p, "explicit", 8) == 0)
     xprefetch_explicit = 1;
   else if (strncmp ( p, "no%explicit", 11) == 0)
     xprefetch_explicit = 0;
   else if (strncmp ( p, "latx:", 5) == 0) 
     xprefetch_latx  = xstrdup( p );
   else {
     error ("%s is an invalid value for -xprefetch", p );
     badval = 1;
   }

   if ((debug_driver_val & 0x01)) 
      fprintf(stdout,"crack: %s xprefetch_auto=%d xpreftch_explicit=%d\n",
              p1, xprefetch_auto, xprefetch_explicit);
 
   return badval;   
}

static int xregs_appl = 1; /* 1=> xregs=appl 0=> xregs=no%appl */
static int xregs_float = 1; /* likewise for float */
static int xregs_syst = -1; /* -1=>neither syst nor no%syst  */

static int 
crack_xregs (char * p)
{
   char *e, *p1;
   int badval = 0;

   /* find the various values on -xregs=.... 
      start with the 7th char of the string to strip off -xregs= */
   p1 = p;
   p += 7;

   while ((e = strchr (p, ','))) /* values are comma delimited */
   {
      if (strncmp( p, "appl", 4) == 0)
        xregs_appl = 1;
      else if (strncmp ( p, "no%appl", 7) == 0)
        xregs_appl = 0;
      else if (strncmp ( p, "float", 5) == 0)
        xregs_float = 1;
      else if (strncmp ( p, "no%float", 8) == 0)
        xregs_float = 0;
      else if (strncmp ( p, "syst", 4) == 0)
        xregs_syst = 1;
      else if (strncmp ( p, "no%syst", 7) == 0)
        xregs_syst = 0;
      else {
        *e = 0;
        error ( "%s is an invalid value for -xregs", p );
        badval = 1;
      }
      p = e + 1; /* skip over "," */
   }
   if (strncmp( p, "appl", 4) == 0)
     xregs_appl = 1;
   else if (strncmp ( p, "no%appl", 7) == 0)
     xregs_appl = 0;
   else if (strncmp ( p, "float", 5) == 0)
     xregs_float = 1;
   else if (strncmp ( p, "no%float", 8) == 0)
     xregs_float = 0;
   else if (strncmp ( p, "syst", 4) == 0)
     xregs_syst = 1;
   else if (strncmp ( p, "no%syst", 7) == 0)
     xregs_syst = 0;
   else {
     error ( "%s is an invalid value for -xregs", p );
     badval = 1;
   }
 
   return badval;
}

/* some options cause problems with duplicates, 
   so delete the leftmost duplicates 
   Also handle -Zquietdriver and multiple -xprofile=use 

   The algorithm works by looping through the options,
   1. for each option look through the remainder for a "match"
   2. if a match is found then NULL out the original option
   2a. for concatenating type option, replace the matching option
       with the concatenated values.
   3. continue with the outer loop (that is only look for ONE match for now)
*/
static void
delete_duplicate_options (int *argcp, char ***argvp)
{
  int i,j;
  int argc = *argcp;
  int newc = *argcp;
  char **argv = *argvp;
  int have_previous_xprofile_use = 0;

  i=0;
  while (i < argc ) {
    if (debug_driver_val & 0x01)
        fprintf(stdout,"delete_duplicate_options: examine argv[%d]=%s\n",
              i, argv[i] ? argv[i] : "==nothing==");
    if (argv[i] && strncmp(argv[i], "-xprofile=collect", 17) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xprofile=", 10) == 0) {
          have_previous_xprofile_use = 0; /* safe even if -xprofile=collect */
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xprofile=use", 13) == 0) {
      have_previous_xprofile_use++;
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xprofile=collect", 17) == 0) {
          have_previous_xprofile_use = 0;
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
      if (have_previous_xprofile_use > 1) {
        /* 2nd, 3rd, .. -xprofile=use need -xprofile=use changed so 
           -u2 can be passed to IROPT.  Replace this option with 
           -Zprofile=use */
        argv[i][1] = 'Z'; /* change the x to Z */
	if ((debug_driver_val & 0x01)) 
             fprintf(stdout,"setting argv[%d] to %s\n",i,argv[i]);
      }
    }
    else if (argv[i] 
             && (strncmp (argv[i], "-Zfast", 6) == 0 || strncmp (argv[i], "-O", 2) == 0))
      {
        j = i + 1;
        while (j < argc)
          {
            if (argv[j]
                && (strncmp (argv[j], "-O", 2) == 0 || strncmp (argv[j], "-Zfast", 6) == 0))
              {
                /* set the old one to NULL, and try another option */
                argv[i] = NULL;
                newc--;
                goto NEXTI;
              }
            j++;
          }
      }
    else if (argv[i] && strncmp(argv[i], "-frtl-backend", 13) == 0) {
	/* this eliminates -xprofile=collect, -xprofile=use, 
           and other -xprofile* */
      j = 0; /* remove them all; so start at the beginning */
      while (j < argc) {
	if (argv[j] && strncmp(argv[j], "-xprofile", 9) == 0) {
	  /* set it to NULL, and try another option */
	  argv[j] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",j);
	  newc--;
	} /* if argv */
	j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xipo=", 6) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xipo=", 6) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xtarget", 8) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xtarget", 8) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xchip", 6) == 0 ) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-xchip", 6) == 0 ||
               strncmp(argv[j], "-xtarget", 8) == 0) ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xcache", 7) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-xcache", 7) == 0 ||
               strncmp(argv[j], "-xtarget", 8) == 0) ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xarch", 6) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-xarch", 6) == 0 ||
               strncmp(argv[j], "-xtarget", 8) == 0) ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zm=32", 6) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zm=32", 6) == 0 ||
               strncmp(argv[j], "-Zm=64", 6) == 0 ||
               strncmp(argv[j], "-m64", 4) == 0 ||
               strncmp(argv[j], "-m32", 4) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
      /* have -Zm=32 and don't have -m32 or -m64
         so convert it to -m32 */
      if ((debug_driver_val & 0x01)) 
              fprintf(stdout,"setting argv[%d] to -m32\n",i);
      argv[i] = xstrdup("-m32");
    }
    else if (argv[i] && strncmp(argv[i], "-Zm=64", 6) == 0) {
      j = i + 1;
      if ((debug_driver_val & 0x01)) 
        fprintf(stdout,"Looking at argv[%d] = %s\n",i,argv[i]);
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zm=32", 6) == 0 ||
               strncmp(argv[j], "-Zm=64", 6) == 0 ||
               strncmp(argv[j], "-m64", 4) == 0 ||
               strncmp(argv[j], "-m32", 4) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
      /* have -Zm=64 and don't have -m32 or -m64
         so convert it to -m64 */
      if ((debug_driver_val & 0x01)) 
              fprintf(stdout,"setting argv[%d] to -m64\n",i);
      argv[i] = xstrdup("-m64");
    }
    else if (argv[i] && strncmp(argv[i], "-m32", 4) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zm=32", 6) == 0  ||
               strncmp(argv[j], "-Zm=64", 6) == 0) ){
          /* set the new one to NULL, and try another option */
          argv[j] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",j);
          newc--;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-m64", 4) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zm=32", 6) == 0  ||
               strncmp(argv[j], "-Zm=64", 6) == 0 ) ){
          /* set the new one to NULL, and try another option */
          argv[j] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",j);
          newc--;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zarch=", 7) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zarch=", 7) == 0 ) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } else if (argv[j] && 
              (strcmp(argv[j], "-mv8plus") == 0 ) ){
          /* modify (possibly) the original , and try another option */
          if (strcmp(argv[i], "-Zarch=v7") == 0 ||
              strcmp(argv[i], "-Zarch=v8") == 0 ) {
            argv[i] = xstrdup("-Zarch=v8plus");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarch=v8plus\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mno-v8plus") == 0 ) ){
          /* modify the original (possibly), and try another option */
          if (strcmp(argv[i], "-Zarch=v8plus") == 0 ||
              strcmp(argv[i], "-Zarch=v8plusa") == 0 ||
              strcmp(argv[i], "-Zarch=v8plusb") == 0 ) {
            argv[i] = xstrdup("-Zarch=v8");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarch=v8\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mvis") == 0 ) ){
          /* modify (possibly) the original , and try another option */
          if (strcmp(argv[i], "-Zarch=v7") == 0 ||
              strcmp(argv[i], "-Zarch=v8") == 0 ||
              strcmp(argv[i], "-Zarch=v8plus") == 0) {
            argv[i] = xstrdup("-Zarch=v8plusa");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarch=v8plusa\n",i);
          } else if (strcmp(argv[i], "-Zarch=v9") == 0 ) {
            argv[i] = xstrdup("-Zarch=v9a");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarch=v9a\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mno-vis") == 0 ) ){
          /* modify the original (possibly), and try another option */
          if (strcmp(argv[i], "-Zarch=v8plusa") == 0 ||
              strcmp(argv[i], "-Zarch=v8plusb") == 0 ) {
            argv[i] = xstrdup("-Zarch=v8plus");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarch=v8plus\n",i);
          }else if (strncmp(argv[i], "-Zarch=v9a", 10) == 0 ||
              strncmp(argv[i], "-Zarch=v9b", 10) == 0 ) {
            argv[i] = xstrdup("-Zarch=v9");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarch=v9\n",i);
          }
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zarchm32", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zarchm32", 9) == 0 ) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } else if (argv[j] && 
              (strcmp(argv[j], "-mv8plus") == 0 ) ){
          /* modify (possibly) the original, and try another option */
          if (strcmp(argv[i], "-Zarchm32=v7") == 0 ||
              strcmp(argv[i], "-Zarchm32=v8") == 0 ) {
            argv[i] = xstrdup("-Zarchm32=v8plus");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarchm32=v8plus\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mno-v8plus") == 0 ) ){
          /* modify (possibly) the original , and try another option */
          if (strcmp(argv[i], "-Zarchm32=v8plus") == 0 ||
              strcmp(argv[i], "-Zarchm32=v8plusa") == 0 ||
              strcmp(argv[i], "-Zarchm32=v8plusb") == 0 ) {
            argv[i] = xstrdup("-Zarchm32=v8");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarchm32=v8\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mvis") == 0 ) ){
          /* modify (possibly) the original , and try another option */
          if (strcmp(argv[i], "-Zarchm32=v7") == 0 ||
              strcmp(argv[i], "-Zarchm32=v8") == 0 ||
              strcmp(argv[i], "-Zarchm32=v8plus") == 0) {
            argv[i] = xstrdup("-Zarchm32=v8plusa");
	    if ((debug_driver_val & 0x01))
                 fprintf(stdout,"setting argv[%d] to -Zarchm32=v8plusa\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mno-vis") == 0 ) ){
          /* modify the original (possibly), and try another option */
          if (strcmp(argv[i], "-Zarchm32=v8plusa") == 0 ||
              strcmp(argv[i], "-Zarchm32=v8plusb") == 0 ) {
            argv[i] = xstrdup("-Zarchm32=v8plus");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarchm32=v8plus\n",i);
          }
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zarchm64", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-Zarchm64", 9) == 0 ) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } else if (argv[j] && 
              (strcmp(argv[j], "-mvis") == 0 ) ){
          /* modify (possibly) the original , and try another option */
          if (strcmp(argv[i], "-Zarchm64=v9") == 0 ) {
            argv[i] = xstrdup("-Zarchm64=v9a");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarchm64=v9a\n",i);
          }
        } else if (argv[j] && 
              (strcmp(argv[j], "-mno-vis") == 0 ) ){
          /* modify the original (possibly), and try another option */
          if (strcmp(argv[i], "-Zarchm64=v9a") == 0 ||
              strcmp(argv[i], "-Zarchm64=v9b") == 0 ) {
            argv[i] = xstrdup("-Zarchm64=v9");
	    if ((debug_driver_val & 0x01)) 
                 fprintf(stdout,"setting argv[%d] to -Zarchm64=v9\n",i);
          }
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-mcpu", 5) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-mcpu", 5) == 0 ||
                        strcmp(argv[j], "-mv8") == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strcmp(argv[i], "-mv8") == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-mcpu", 5) == 0 ||
                        strcmp(argv[j], "-mv8") == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-mv8plus", 8) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-mcpu", 5) == 0 ||          
                        strncmp(argv[j], "-xtarget", 8) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xcheck", 7) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xcheck", 7) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-mno-vis", 8) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-mcpu", 5) == 0 ||
                        strncmp(argv[j], "-xtarget", 8) == 0 ||
                        strncmp(argv[j], "-mvis", 5) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-mvis", 5) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-mcpu", 5) == 0 ||
                        strncmp(argv[j], "-xtarget", 8) == 0 ||
                        strncmp(argv[j], "-mno-vis", 8) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xcode=", 7) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
               strncmp(argv[j], "-xcode=", 7) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xannotate=", 11) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xannotate=", 11) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zfns=", 6) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-Zfns=", 6) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zfsimple=", 10) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-Zfsimple=", 10) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zftrap=", 8) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-Zftrap=", 8) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xbuiltin", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xbuiltin", 9) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xinstrument", 12) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xinstrument", 12) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
          if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xopenmp", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xopenmp", 9) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-tm_mode", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-tm_mode", 9) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
          if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp (argv[i], "-xprefetch_level", 16) == 0) 
      {
        j = i + 1;
        while (j < argc) 
          {
            if (argv[j] && strncmp (argv[j], "-xprefetch_level", 16) == 0) 
              {
                /* set the old one to NULL, and try another option */
                argv[i] = NULL;
                if (debug_driver_val & 0x01) 
                  fprintf (stdout, "setting argv[%d] to NULL\n", i);
                newc--;
                goto NEXTI;
              }
            j++;
          }
      }
    else if (argv[i] && strncmp (argv[i], "-xprefetch=", 11) == 0)
      {
        int badval = crack_xprefetch (argv[i]);

        j = i + 1;
        while (j < argc)
          {
            if (argv[j] && strncmp (argv[j], "-xprefetch=", 11) == 0) 
              {
                crack_xprefetch(argv[j]);
                /* set the old one to NULL, replace with merged values
                   and move on*/
                argv[i] = NULL;
                if (debug_driver_val & 0x01) 
                  fprintf (stdout, "setting argv[%d] to NULL\n", i);
                argv[j] = concat ("-xprefetch=",
                                  xprefetch_auto == 1 ? "auto," : "no%auto,",
                                  xprefetch_explicit == 1 ? "explicit" : "no%explicit",
                                  xprefetch_latx != NULL ? "," : NULL,
                                  xprefetch_latx != NULL ? xprefetch_latx : NULL,
                                  NULL);
                if (debug_driver_val & 0x01)
                  fprintf (stdout, "setting argv[%d] to %s\n", j, argv[j]);
                newc--;
                goto NEXTI;
              }
            j++;
          }
        /* If we get here, there are no other -xprefetch */
        if (badval) 
          {
            argv[i] = NULL;
            newc--;
            if (debug_driver_val & 0x01)
              fprintf (stdout, "setting argv[%d] to NULL\n", i);
          }
        else 
          {
            argv[i] = concat ("-xprefetch=",
                              xprefetch_auto == 1 ? "auto," : "no%auto,",
                              xprefetch_explicit == 1 ? "explicit" : "no%explicit",
                              xprefetch_latx != NULL ? "," : NULL,
                              xprefetch_latx != NULL ? xprefetch_latx : NULL,
                              NULL);
            if (debug_driver_val & 0x01)
              fprintf (stdout, "setting argv[%d] to %s\n", i, argv[i]);
          }
      }
    else if (argv[i] && strncmp(argv[i], "-xregs=", 7) == 0) {
      int badval = crack_xregs(argv[i]);

      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xregs=", 7) == 0) {
          crack_xregs(argv[j]);
          /* set the old one to NULL, replace with merged values
              and move on*/
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) 
             fprintf(stdout,"setting argv[%d] to NULL\n",i);
          argv[j] = concat ( "-xregs=",
                             xregs_appl == 1 ? "appl," : "no%appl,",
                             xregs_float == 1 ? "float" : "no%float",
                             xregs_syst == 1 ? ",syst" 
                                             : (xregs_syst == 0 ? ",no%syst" : NULL),
                             NULL);
	  if ((debug_driver_val & 0x01)) 
             fprintf(stdout,"setting argv[%d] to %s\n",j,argv[j]);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
      /* If we get here, there are no other -xregs */
      if (badval)
        {
          argv[i] = NULL;
          newc--;
	  if ((debug_driver_val & 0x01)) 
             fprintf(stdout,"setting argv[%d] to NULL\n",i);
        }
      else 
        {
          argv[i] = concat ( "-xregs=",
                             xregs_appl == 1 ? "appl," : "no%appl,",
                             xregs_float == 1 ? "float" : "no%float",
                             xregs_syst == 1 ? ",syst" 
                                             : (xregs_syst == 0 ? ",no%syst" : NULL),
                             NULL);
	  if ((debug_driver_val & 0x01)) 
             fprintf(stdout,"setting argv[%d] to %s\n",i,argv[i]);
        }
    }
    else if (argv[i] && strncmp(argv[i], "-xmemalign=", 11) == 0) {
      j = i + 1;
      while (j < argc) {
        if ( (argv[j] && strncmp(argv[j], "-xmemalign=", 11) == 0) ||
             (argv[j] && strncmp(argv[j], "-mno-integer-ldd-std", 20) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-mno-integer-ldd-std", 20) == 0) {
      j = i + 1;
      while (j < argc) {
        if ( (argv[j] && strncmp(argv[j], "-xmemalign=", 11) == 0) ||
             (argv[j] && strncmp(argv[j], "-mno-integer-ldd-std", 20) == 0) ){
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xalias_level=", 14) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-xalias_level=", 14) == 0 ||
                        strncmp(argv[j], "-Zalias_level=", 14) == 0) ) {
          /* if the new one is Zalias_level; change it to xalias_level */
          argv[j][1] = 'x';
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zalias_level=", 14) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && (strncmp(argv[j], "-xalias_level=", 14) == 0 ||
                        strncmp(argv[j], "-Zalias_level=", 14) ==0 ) ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xdepend=", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xdepend=", 9) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xinline=", 9) == 0) {
      j = i + 1;
      /* The last -xinline on the command line is used. */
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-xinline=", 9) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-xhwcprof", 9) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && 
              (strncmp(argv[j], "-xhwcprof", 9) == 0 ) ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
            goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-frtl-backend", 13) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-frtl-backend", 13) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } 
        else if (argv[j] && strncmp(argv[j], "-Zsunir-backend", 15) == 0 ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }
    else if (argv[i] && strncmp(argv[i], "-Zsunir-backend", 15) == 0) {
      j = i + 1;
      while (j < argc) {
        if (argv[j] && strncmp(argv[j], "-Zsunir-backend", 15) == 0) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } 
        else if (argv[j] && strncmp(argv[j], "-frtl-backend", 13) == 0 ) {
          /* set the old one to NULL, and try another option */
          argv[i] = NULL;
	  if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",i);
          newc--;
          goto NEXTI;
        } /* if argv */
        j++;
      } /* while (j < argc ) */
    }        
    else if (argv[i] && strncmp(argv[i], "-Zquietdriver", 13) == 0) {
       quiet_driver++;
    }
NEXTI:
    i++;
  } /* while (i < argc) */

  if (newc != argc) {
    if ((debug_driver_val & 0x01)) fprintf(stdout,"COMPACTING ARGV\n");
    /* need to compact argv */
    for (i=0; i < newc; i++) {
      if (argv[i] == NULL) {
        /* find something to move to here */
        for (j=i+1; j < argc; j++)
          if (argv[j] != NULL) {
            argv[i] = argv[j];
	    if ((debug_driver_val & 0x01)) 
              fprintf(stdout,"moving argv[%d] to arg[%d]\n",j,i);
            argv[j] = NULL;
	    if ((debug_driver_val & 0x01)) fprintf(stdout,"setting argv[%d] to NULL\n",j);
            break;
          } /* argv[j] != NULL */
      } /* argv[i] == NULL */
    } /* for i */
  } /* if newc != argc */

  *argcp = newc;
}

static char *
skip_whitespace (char *p)
{
  while (1)
    {
      /* A fully-blank line is a delimiter in the SPEC file and shouldn't
	 be considered whitespace.  */
      if (p[0] == '\n' && p[1] == '\n' && p[2] == '\n')
	return p + 1;
      else if (*p == '\n' || *p == ' ' || *p == '\t')
	p++;
      else if (*p == '#')
	{
	  while (*p != '\n')
	    p++;
	  p++;
	}
      else
	break;
    }

  return p;
}
/* Structures to keep track of prefixes to try when looking for files.  */

struct prefix_list
{
  const char *prefix;	      /* String to prepend to the path.  */
  struct prefix_list *next;   /* Next in linked list.  */
  int require_machine_suffix; /* Don't use without machine_suffix.  */
  /* 2 means try both machine_suffix and just_machine_suffix.  */
  int priority;		      /* Sort key - priority within list.  */
  int os_multilib;	      /* 1 if OS multilib scheme should be used,
				 0 for GCC multilib scheme.  */
};

struct path_prefix
{
  struct prefix_list *plist;  /* List of prefixes to try */
  int max_len;                /* Max length of a prefix in PLIST */
  const char *name;           /* Name of this list (used in config stuff) */
};

/* List of prefixes in PATH */

static struct path_prefix PATH_prefixes = { 0, 0, "PATH"};

/* List of prefixes to try when looking for executables.  */

static struct path_prefix exec_prefixes = { 0, 0, "exec" };

/* List of prefixes to try when looking for startup (crt0) files.  */

static struct path_prefix startfile_prefixes = { 0, 0, "startfile" };

/* List of prefixes to try when looking for include files.  */

static struct path_prefix include_prefixes = { 0, 0, "include" };

/* List of prefixes used by cross compiler when looking for include files
   and specified with environment variable GCCFSS_LINUX_SPARC_INCLUDE or
   GCCFSS_SOLARIS_SPARC_INCLUDE */

static struct path_prefix cross_include_prefixes = { 0, 0, "include" };

/* Suffix to attach to directories searched for commands.
   This looks like `MACHINE/VERSION/'.  */

static const char *machine_suffix = 0;

/* Suffix to attach to directories searched for commands.
   This is just `MACHINE/'.  */

static const char *just_machine_suffix = 0;

/* Adjusted value of GCC_EXEC_PREFIX envvar.  */

static const char *gcc_exec_prefix;

/* Adjusted value of standard_libexec_prefix.  */

static const char *gcc_libexec_prefix;

/* Default prefixes to attach to command names.  */

#ifndef STANDARD_STARTFILE_PREFIX_1
#define STANDARD_STARTFILE_PREFIX_1 "/lib/"
#endif
#ifndef STANDARD_STARTFILE_PREFIX_2
#define STANDARD_STARTFILE_PREFIX_2 "/usr/lib/"
#endif

#ifdef CROSS_DIRECTORY_STRUCTURE  /* Don't use these prefixes for a cross compiler.  */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef MD_STARTFILE_PREFIX_1
#endif

/* If no prefixes defined, use the null string, which will disable them.  */
#ifndef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX ""
#endif
#ifndef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX ""
#endif
#ifndef MD_STARTFILE_PREFIX_1
#define MD_STARTFILE_PREFIX_1 ""
#endif

/* These directories are locations set at configure-time based on the
   --prefix option provided to configure.  Their initializers are
   defined in Makefile.in.  These paths are not *directly* used when
   gcc_exec_prefix is set because, in that case, we know where the
   compiler has been installed, and use paths relative to that
   location instead.  */
static const char *const standard_exec_prefix = STANDARD_EXEC_PREFIX;
static const char *const standard_libexec_prefix = STANDARD_LIBEXEC_PREFIX;
static const char *const standard_bindir_prefix = STANDARD_BINDIR_PREFIX;
static const char *const standard_startfile_prefix = STANDARD_STARTFILE_PREFIX;

/* For native compilers, these are well-known paths containing
   components that may be provided by the system.  For cross
   compilers, these paths are not used.  */
static const char *const standard_exec_prefix_1 = "/usr/libexec/gcc/";
static const char *const standard_exec_prefix_2 = "/usr/lib/gcc/";
static const char *md_exec_prefix = MD_EXEC_PREFIX;
static const char *md_startfile_prefix = MD_STARTFILE_PREFIX;
static const char *md_startfile_prefix_1 = MD_STARTFILE_PREFIX_1;
static const char *const standard_startfile_prefix_1
  = STANDARD_STARTFILE_PREFIX_1;
static const char *const standard_startfile_prefix_2
  = STANDARD_STARTFILE_PREFIX_2;

/* A relative path to be used in finding the location of tools
   relative to the driver.  */
static const char *const tooldir_base_prefix = TOOLDIR_BASE_PREFIX;

static const char *studioproddir = NULL;
static char *studioproddir_bin = NULL;
static char *studioproddir_lib = NULL;
static char *sunw_studio_path = NULL;
static char *sunw_scgfss_path = NULL;

static const char *prod_dir_include(int dummy __attribute__ ((unused)), 
                           const char** dummy2 __attribute__ ((unused)) ) {
  char * ptr;
  ptr = concat(studioproddir, "/include", NULL);
  if (directory_exists(ptr))
     return xstrdup(concat("-I",ptr,NULL));
  else
     return xstrdup(" ");
}

static char *alternate_as = NULL;
static char *alternate_iropt = NULL;
static char *alternate_cg = NULL;
static char *alternate_ipo = NULL;
static char *alternate_postopt = NULL;
static char *current_work_directory = NULL;

/* Subdirectory to use for locating libraries.  Set by
   set_multilib_dir based on the compilation options.  */

static const char *multilib_dir;

/* Subdirectory to use for locating libraries in OS conventions.  Set by
   set_multilib_dir based on the compilation options.  */

static const char *multilib_os_dir;

/* Structure to keep track of the specs that have been defined so far.
   These are accessed using %(specname) or %[specname] in a compiler
   or link spec.  */

struct spec_list
{
				/* The following 2 fields must be first */
				/* to allow EXTRA_SPECS to be initialized */
  const char *name;		/* name of the spec.  */
  const char *ptr;		/* available ptr if no static pointer */

				/* The following fields are not initialized */
				/* by EXTRA_SPECS */
  const char **ptr_spec;	/* pointer to the spec itself.  */
  struct spec_list *next;	/* Next spec in linked list.  */
  int name_len;			/* length of the name */
  int alloc_p;			/* whether string was allocated */
};

#define INIT_STATIC_SPEC(NAME,PTR) \
{ NAME, NULL, PTR, (struct spec_list *) 0, sizeof (NAME) - 1, 0 }

/* List of statically defined specs.  */
static struct spec_list static_specs[] =
{
  INIT_STATIC_SPEC ("asm",			&asm_spec),
  INIT_STATIC_SPEC ("asm_debug",		&asm_debug),
  INIT_STATIC_SPEC ("asm_final",		&asm_final_spec),
  INIT_STATIC_SPEC ("asm_options",		&asm_options),
  INIT_STATIC_SPEC ("invoke_as",		&invoke_as),
  INIT_STATIC_SPEC ("gccfss_invoke_as",		&gccfss_invoke_as),
  INIT_STATIC_SPEC ("cpp",			&cpp_spec),
  INIT_STATIC_SPEC ("cpp_options",		&cpp_options),
  INIT_STATIC_SPEC ("cpp_debug_options",	&cpp_debug_options),
  INIT_STATIC_SPEC ("cpp_unique_options",	&cpp_unique_options),
  INIT_STATIC_SPEC ("trad_capable_cpp",		&trad_capable_cpp),
  INIT_STATIC_SPEC ("cc1",			&cc1_spec),
  INIT_STATIC_SPEC ("cc1_options",		&cc1_options),
  INIT_STATIC_SPEC ("gccfss_cc1_options",	&gccfss_cc1_options),
  INIT_STATIC_SPEC ("cc1_unique_options",	&cc1_unique_options),
  INIT_STATIC_SPEC ("oldstyle_cc1_options",	&oldstyle_cc1_options),
  INIT_STATIC_SPEC ("cc1plus",			&cc1plus_spec),
  INIT_STATIC_SPEC ("link_gcc_c_sequence",	&link_gcc_c_sequence_spec),
  INIT_STATIC_SPEC ("link_ssp",			&link_ssp_spec),
  INIT_STATIC_SPEC ("endfile",			&endfile_spec),
  INIT_STATIC_SPEC ("link",			&link_spec),
  INIT_STATIC_SPEC ("lib",			&lib_spec),
  INIT_STATIC_SPEC ("mfwrap",			&mfwrap_spec),
  INIT_STATIC_SPEC ("mflib",			&mflib_spec),
  INIT_STATIC_SPEC ("link_gomp",		&link_gomp_spec),
  INIT_STATIC_SPEC ("libgcc",			&libgcc_spec),
  INIT_STATIC_SPEC ("startfile",		&startfile_spec),
  INIT_STATIC_SPEC ("switches_need_spaces",	&switches_need_spaces),
  INIT_STATIC_SPEC ("cross_compile",		&cross_compile),
  INIT_STATIC_SPEC ("version",			&compiler_version),
  INIT_STATIC_SPEC ("multilib",			&multilib_select),
  INIT_STATIC_SPEC ("multilib_defaults",	&multilib_defaults),
  INIT_STATIC_SPEC ("multilib_extra",		&multilib_extra),
  INIT_STATIC_SPEC ("multilib_matches",		&multilib_matches),
  INIT_STATIC_SPEC ("multilib_exclusions",	&multilib_exclusions),
  INIT_STATIC_SPEC ("multilib_options",		&multilib_options),
  INIT_STATIC_SPEC ("linker",			&linker_name_spec),
  INIT_STATIC_SPEC ("link_libgcc",		&link_libgcc_spec),
  INIT_STATIC_SPEC ("md_exec_prefix",		&md_exec_prefix),
  INIT_STATIC_SPEC ("md_startfile_prefix",	&md_startfile_prefix),
  INIT_STATIC_SPEC ("md_startfile_prefix_1",	&md_startfile_prefix_1),
  INIT_STATIC_SPEC ("startfile_prefix_spec",	&startfile_prefix_spec),
  INIT_STATIC_SPEC ("sysroot_spec",             &sysroot_spec),
  INIT_STATIC_SPEC ("sysroot_suffix_spec",	&sysroot_suffix_spec),
  INIT_STATIC_SPEC ("sysroot_hdrs_suffix_spec",	&sysroot_hdrs_suffix_spec),
  INIT_STATIC_SPEC ("invoke_iropt",		&invoke_iropt),
  INIT_STATIC_SPEC ("invoke_cg",		&invoke_cg),
  INIT_STATIC_SPEC ("ssbe_xarch",		&ssbe_xarch),
  INIT_STATIC_SPEC ("ssbe_xarch_xchip",		&ssbe_xarch_xchip),
  INIT_STATIC_SPEC ("sscg_xarch_xchip",		&sscg_xarch_xchip),
  INIT_STATIC_SPEC ("ssiropt_optlevel",		&ssiropt_optlevel),
  INIT_STATIC_SPEC ("ssbe_optlevel",		&ssbe_optlevel),
  INIT_STATIC_SPEC ("ssiropt_spec_gxx",		&ssiropt_lang_spec_gxx),
  INIT_STATIC_SPEC ("ssiropt_spec_fortran",	&ssiropt_lang_spec_fortran),
  INIT_STATIC_SPEC ("sscg_spec_gxx",	        &sscg_lang_spec_gxx),
  INIT_STATIC_SPEC ("sscg_spec_fortran",        &sscg_lang_spec_fortran),
  INIT_STATIC_SPEC ("ssiropt_spec",		&ssiropt_lang_spec),
  INIT_STATIC_SPEC ("sscg_spec",	        &sscg_lang_spec),
  INIT_STATIC_SPEC ("xtarget",			&xtarget),
  INIT_STATIC_SPEC ("invoke_ipo1",		&invoke_ipo1),
  INIT_STATIC_SPEC ("invoke_fortranipo1",	&invoke_fortranipo1),
  INIT_STATIC_SPEC ("invoke_cppipo1",		&invoke_cppipo1),
  INIT_STATIC_SPEC ("invoke_ipo2",		&invoke_ipo2),
  INIT_STATIC_SPEC ("iropt_ipo_options",        &iropt_ipo_options),
  INIT_STATIC_SPEC ("iropt_only_options",       &iropt_only_options),
  INIT_STATIC_SPEC ("cg_ipo_options",           &cg_ipo_options),
  INIT_STATIC_SPEC ("mvis_il",                  &mvis_il),
  INIT_STATIC_SPEC ("asm_name",			&asm_name),
};

#ifdef EXTRA_SPECS		/* additional specs needed */
/* Structure to keep track of just the first two args of a spec_list.
   That is all that the EXTRA_SPECS macro gives us.  */
struct spec_list_1
{
  const char *const name;
  const char *const ptr;
};

static const struct spec_list_1 extra_specs_1[] = { EXTRA_SPECS };
static struct spec_list *extra_specs = (struct spec_list *) 0;
#endif

/* List of dynamically allocates specs that have been defined so far.  */

static struct spec_list *specs = (struct spec_list *) 0;

/* List of static spec functions.  */

static const struct spec_function static_spec_functions[] =
{
  { "getenv",                   getenv_spec_function },
  { "if-exists",		if_exists_spec_function },
  { "if-exists-else",		if_exists_else_spec_function },
  { "replace-outfile",		replace_outfile_spec_function },
  { "version-compare",		version_compare_spec_function },
  { "include",			include_spec_function },
  { "print-orig-cmdline",	print_orig_cmdline}, 
  { "is-it-pec_dummy",		is_it_pec_dummy},
  { "verify-sunir-backend",	verify_sunir_backend},
  { "add-user-il-routines",	add_user_il_routines},
  { "print-asm-header",		print_asm_header_spec_function },
  { "add-sun-prefetch",		add_sun_prefetch },
  { "prod-dir-include",		prod_dir_include },
  { "reset-xprefetch-explicit", reset_xprefetch_explicit},
#ifdef EXTRA_SPEC_FUNCTIONS
  EXTRA_SPEC_FUNCTIONS
#endif
  { 0, 0 }
};

static int processing_spec_function;

/* Add appropriate libgcc specs to OBSTACK, taking into account
   various permutations of -shared-libgcc, -shared, and such.  */

#if defined(ENABLE_SHARED_LIBGCC) && !defined(REAL_LIBGCC_SPEC)

#ifndef USE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 0
#endif

static void
init_gcc_specs (struct obstack *obstack, const char *shared_name,
		const char *static_name, const char *eh_name)
{
  char *buf;

  buf = concat ("%{static|static-libgcc:", static_name, " ", eh_name, "}"
		"%{!static:%{!static-libgcc:"
#if USE_LD_AS_NEEDED
		"%{!shared-libgcc:",
		static_name, " --as-needed ", shared_name, " --no-as-needed"
		"}"
		"%{shared-libgcc:",
		shared_name, "%{!shared: ", static_name, "}"
		"}"
#else
		"%{!shared:"
		"%{!shared-libgcc:", static_name, " ", eh_name, "}"
		"%{shared-libgcc:", shared_name, " ", static_name, "}"
		"}"
#ifdef LINK_EH_SPEC
		"%{shared:"
		"%{shared-libgcc:", shared_name, "}"
		"%{!shared-libgcc:", static_name, "}"
		"}"
#else
		"%{shared:", shared_name, "}"
#endif
#endif
		"}}", NULL);

  obstack_grow (obstack, buf, strlen (buf));
  free (buf);
}
#endif /* ENABLE_SHARED_LIBGCC */

/* Initialize the specs lookup routines.  */

static void
init_spec (void)
{
  struct spec_list *next = (struct spec_list *) 0;
  struct spec_list *sl   = (struct spec_list *) 0;
  int i;

  if (specs)
    return;			/* Already initialized.  */

  if (verbose_flag)
    notice ("Using built-in specs.\n");

#ifdef EXTRA_SPECS
  extra_specs = xcalloc (sizeof (struct spec_list),
			 ARRAY_SIZE (extra_specs_1));

  for (i = ARRAY_SIZE (extra_specs_1) - 1; i >= 0; i--)
    {
      sl = &extra_specs[i];
      sl->name = extra_specs_1[i].name;
      sl->ptr = extra_specs_1[i].ptr;
      sl->next = next;
      sl->name_len = strlen (sl->name);
      sl->ptr_spec = &sl->ptr;
      next = sl;
    }
#endif

  for (i = ARRAY_SIZE (static_specs) - 1; i >= 0; i--)
    {
      sl = &static_specs[i];
      sl->next = next;
      next = sl;
    }

#if defined(ENABLE_SHARED_LIBGCC) && !defined(REAL_LIBGCC_SPEC)
  /* ??? If neither -shared-libgcc nor --static-libgcc was
     seen, then we should be making an educated guess.  Some proposed
     heuristics for ELF include:

	(1) If "-Wl,--export-dynamic", then it's a fair bet that the
	    program will be doing dynamic loading, which will likely
	    need the shared libgcc.

	(2) If "-ldl", then it's also a fair bet that we're doing
	    dynamic loading.

	(3) For each ET_DYN we're linking against (either through -lfoo
	    or /some/path/foo.so), check to see whether it or one of
	    its dependencies depends on a shared libgcc.

	(4) If "-shared"

	    If the runtime is fixed to look for program headers instead
	    of calling __register_frame_info at all, for each object,
	    use the shared libgcc if any EH symbol referenced.

	    If crtstuff is fixed to not invoke __register_frame_info
	    automatically, for each object, use the shared libgcc if
	    any non-empty unwind section found.

     Doing any of this probably requires invoking an external program to
     do the actual object file scanning.  */
  {
    const char *p = libgcc_spec;
    int in_sep = 1;

    /* Transform the extant libgcc_spec into one that uses the shared libgcc
       when given the proper command line arguments.  */
    while (*p)
      {
	if (in_sep && *p == '-' && strncmp (p, "-lgcc", 5) == 0)
	  {
	    init_gcc_specs (&obstack,
			    "-lgcc_s"
#ifdef USE_LIBUNWIND_EXCEPTIONS
			    " -lunwind"
#endif
			    ,
			    "-lgcc",
			    "-lgcc_eh"
#ifdef USE_LIBUNWIND_EXCEPTIONS
# ifdef HAVE_LD_STATIC_DYNAMIC
			    " %{!static:-Bstatic} -lunwind %{!static:-Bdynamic}"
# else
			    " -lunwind"
# endif
#endif
			    );

	    p += 5;
	    in_sep = 0;
	  }
	else if (in_sep && *p == 'l' && strncmp (p, "libgcc.a%s", 10) == 0)
	  {
	    /* Ug.  We don't know shared library extensions.  Hope that
	       systems that use this form don't do shared libraries.  */
	    init_gcc_specs (&obstack,
			    "-lgcc_s",
			    "libgcc.a%s",
			    "libgcc_eh.a%s"
#ifdef USE_LIBUNWIND_EXCEPTIONS
			    " -lunwind"
#endif
			    );
	    p += 10;
	    in_sep = 0;
	  }
	else
	  {
	    obstack_1grow (&obstack, *p);
	    in_sep = (*p == ' ');
	    p += 1;
	  }
      }

    obstack_1grow (&obstack, '\0');
    libgcc_spec = XOBFINISH (&obstack, const char *);
  }
#endif
#ifdef USE_AS_TRADITIONAL_FORMAT
  /* Prepend "--traditional-format" to whatever asm_spec we had before.  */
  {
    static const char tf[] = "--traditional-format ";
    obstack_grow (&obstack, tf, sizeof(tf) - 1);
    obstack_grow0 (&obstack, asm_spec, strlen (asm_spec));
    asm_spec = XOBFINISH (&obstack, const char *);
  }
#endif
#ifdef LINK_EH_SPEC
  /* Prepend LINK_EH_SPEC to whatever link_spec we had before.  */
  obstack_grow (&obstack, LINK_EH_SPEC, sizeof(LINK_EH_SPEC) - 1);
  obstack_grow0 (&obstack, link_spec, strlen (link_spec));
  link_spec = XOBFINISH (&obstack, const char *);
#endif

  specs = sl;
}

/* Change the value of spec NAME to SPEC.  If SPEC is empty, then the spec is
   removed; If the spec starts with a + then SPEC is added to the end of the
   current spec.  */

static void
set_spec (const char *name, const char *spec)
{
  struct spec_list *sl;
  const char *old_spec;
  int name_len = strlen (name);
  int i;

  /* If this is the first call, initialize the statically allocated specs.  */
  if (!specs)
    {
      struct spec_list *next = (struct spec_list *) 0;
      for (i = ARRAY_SIZE (static_specs) - 1; i >= 0; i--)
	{
	  sl = &static_specs[i];
	  sl->next = next;
	  next = sl;
	}
      specs = sl;
    }

  /* See if the spec already exists.  */
  for (sl = specs; sl; sl = sl->next)
    if (name_len == sl->name_len && !strcmp (sl->name, name))
      break;

  if (!sl)
    {
      /* Not found - make it.  */
      sl = XNEW (struct spec_list);
      sl->name = xstrdup (name);
      sl->name_len = name_len;
      sl->ptr_spec = &sl->ptr;
      sl->alloc_p = 0;
      *(sl->ptr_spec) = "";
      sl->next = specs;
      specs = sl;
    }

  old_spec = *(sl->ptr_spec);
  *(sl->ptr_spec) = ((spec[0] == '+' && ISSPACE ((unsigned char)spec[1]))
		     ? concat (old_spec, spec + 1, NULL)
		     : xstrdup (spec));

#ifdef DEBUG_SPECS
  if (verbose_flag)
    notice ("Setting spec %s to '%s'\n\n", name, *(sl->ptr_spec));
#endif

  /* Free the old spec.  */
  if (old_spec && sl->alloc_p)
    free (CONST_CAST(char *, old_spec));

  sl->alloc_p = 1;
}

/* Accumulate a command (program name and args), and run it.  */

/* Vector of pointers to arguments in the current line of specifications.  */

static const char **argbuf;

/* Number of elements allocated in argbuf.  */

static int argbuf_length;

/* Number of elements in argbuf currently in use (containing args).  */

static int argbuf_index;

/* Position in the argbuf array containing the name of the output file
   (the value associated with the "-o" flag).  */

static int have_o_argbuf_index = 0;

/* Were the options -c or -S passed.  */
static int have_c = 0;

/* Was the option -o passed.  */
static int have_o = 0;

/* This is the list of suffixes and codes (%g/%u/%U/%j) and the associated
   temp file.  If the HOST_BIT_BUCKET is used for %j, no entry is made for
   it here.  */

static struct temp_name {
  const char *suffix;	/* suffix associated with the code.  */
  int length;		/* strlen (suffix).  */
  int unique;		/* Indicates whether %g or %u/%U was used.  */
  const char *filename;	/* associated filename.  */
  int filename_length;	/* strlen (filename).  */
  struct temp_name *next;
} *temp_names;

/* Number of commands executed so far.  */

static int execution_count;

/* Number of commands that exited with a signal.  */

static int signal_count;

/* Name with which this program was invoked.  */

static const char *programname;

/* Allocate the argument vector.  */

static void
alloc_args (void)
{
  argbuf_length = 10;
  argbuf = XNEWVEC (const char *, argbuf_length);
}

/* Clear out the vector of arguments (after a command is executed).  */

static void
clear_args (void)
{
  argbuf_index = 0;
}

/* Add one argument to the vector at the end.
   This is done when a space is seen or at the end of the line.
   If DELETE_ALWAYS is nonzero, the arg is a filename
    and the file should be deleted eventually.
   If DELETE_FAILURE is nonzero, the arg is a filename
    and the file should be deleted if this compilation fails.  */

static void
store_arg (const char *arg, int delete_always, int delete_failure)
{
  if (argbuf_index + 1 == argbuf_length)
    argbuf = xrealloc (argbuf, (argbuf_length *= 2) * sizeof (const char *));

  argbuf[argbuf_index++] = arg;
  argbuf[argbuf_index] = 0;

  if (strcmp (arg, "-o") == 0)
    have_o_argbuf_index = argbuf_index;
  if (delete_always || delete_failure)
    record_temp_file (arg, delete_always, delete_failure);
}

/* Load specs from a file name named FILENAME, replacing occurrences of
   various different types of line-endings, \r\n, \n\r and just \r, with
   a single \n.  */

static char *
load_specs (const char *filename)
{
  int desc;
  int readlen;
  struct stat statbuf;
  char *buffer;
  char *buffer_p;
  char *specs;
  char *specs_p;

  if (verbose_flag)
    notice ("Reading specs from %s\n", filename);

  /* Open and stat the file.  */
  desc = open (filename, O_RDONLY, 0);
  if (desc < 0)
    pfatal_with_name (filename);
  if (stat (filename, &statbuf) < 0)
    pfatal_with_name (filename);

  /* Read contents of file into BUFFER.  */
  buffer = XNEWVEC (char, statbuf.st_size + 1);
  readlen = read (desc, buffer, (unsigned) statbuf.st_size);
  if (readlen < 0)
    pfatal_with_name (filename);
  buffer[readlen] = 0;
  close (desc);

  specs = XNEWVEC (char, readlen + 1);
  specs_p = specs;
  for (buffer_p = buffer; buffer_p && *buffer_p; buffer_p++)
    {
      int skip = 0;
      char c = *buffer_p;
      if (c == '\r')
	{
	  if (buffer_p > buffer && *(buffer_p - 1) == '\n')	/* \n\r */
	    skip = 1;
	  else if (*(buffer_p + 1) == '\n')			/* \r\n */
	    skip = 1;
	  else							/* \r */
	    c = '\n';
	}
      if (! skip)
	*specs_p++ = c;
    }
  *specs_p = '\0';

  free (buffer);
  return (specs);
}

/* Read compilation specs from a file named FILENAME,
   replacing the default ones.

   A suffix which starts with `*' is a definition for
   one of the machine-specific sub-specs.  The "suffix" should be
   *asm, *cc1, *cpp, *link, *startfile, etc.
   The corresponding spec is stored in asm_spec, etc.,
   rather than in the `compilers' vector.

   Anything invalid in the file is a fatal error.  */

static void
read_specs (const char *filename, int main_p)
{
  char *buffer;
  char *p;

  buffer = load_specs (filename);

  /* Scan BUFFER for specs, putting them in the vector.  */
  p = buffer;
  while (1)
    {
      char *suffix;
      char *spec;
      char *in, *out, *p1, *p2, *p3;

      /* Advance P in BUFFER to the next nonblank nocomment line.  */
      p = skip_whitespace (p);
      if (*p == 0)
	break;

      /* Is this a special command that starts with '%'? */
      /* Don't allow this for the main specs file, since it would
	 encourage people to overwrite it.  */
      if (*p == '%' && !main_p)
	{
	  p1 = p;
	  while (*p && *p != '\n')
	    p++;

	  /* Skip '\n'.  */
	  p++;

	  if (!strncmp (p1, "%include", sizeof ("%include") - 1)
	      && (p1[sizeof "%include" - 1] == ' '
		  || p1[sizeof "%include" - 1] == '\t'))
	    {
	      char *new_filename;

	      p1 += sizeof ("%include");
	      while (*p1 == ' ' || *p1 == '\t')
		p1++;

	      if (*p1++ != '<' || p[-2] != '>')
		fatal ("specs %%include syntax malformed after %ld characters",
		       (long) (p1 - buffer + 1));

	      p[-2] = '\0';
	      new_filename = find_a_file (&startfile_prefixes, p1, R_OK, true);
	      read_specs (new_filename ? new_filename : p1, FALSE);
	      continue;
	    }
	  else if (!strncmp (p1, "%include_noerr", sizeof "%include_noerr" - 1)
		   && (p1[sizeof "%include_noerr" - 1] == ' '
		       || p1[sizeof "%include_noerr" - 1] == '\t'))
	    {
	      char *new_filename;

	      p1 += sizeof "%include_noerr";
	      while (*p1 == ' ' || *p1 == '\t')
		p1++;

	      if (*p1++ != '<' || p[-2] != '>')
		fatal ("specs %%include syntax malformed after %ld characters",
		       (long) (p1 - buffer + 1));

	      p[-2] = '\0';
	      new_filename = find_a_file (&startfile_prefixes, p1, R_OK, true);
	      if (new_filename)
		read_specs (new_filename, FALSE);
	      else if (verbose_flag)
		notice ("could not find specs file %s\n", p1);
	      continue;
	    }
	  else if (!strncmp (p1, "%rename", sizeof "%rename" - 1)
		   && (p1[sizeof "%rename" - 1] == ' '
		       || p1[sizeof "%rename" - 1] == '\t'))
	    {
	      int name_len;
	      struct spec_list *sl;
	      struct spec_list *newsl;

	      /* Get original name.  */
	      p1 += sizeof "%rename";
	      while (*p1 == ' ' || *p1 == '\t')
		p1++;

	      if (! ISALPHA ((unsigned char) *p1))
		fatal ("specs %%rename syntax malformed after %ld characters",
		       (long) (p1 - buffer));

	      p2 = p1;
	      while (*p2 && !ISSPACE ((unsigned char) *p2))
		p2++;

	      if (*p2 != ' ' && *p2 != '\t')
		fatal ("specs %%rename syntax malformed after %ld characters",
		       (long) (p2 - buffer));

	      name_len = p2 - p1;
	      *p2++ = '\0';
	      while (*p2 == ' ' || *p2 == '\t')
		p2++;

	      if (! ISALPHA ((unsigned char) *p2))
		fatal ("specs %%rename syntax malformed after %ld characters",
		       (long) (p2 - buffer));

	      /* Get new spec name.  */
	      p3 = p2;
	      while (*p3 && !ISSPACE ((unsigned char) *p3))
		p3++;

	      if (p3 != p - 1)
		fatal ("specs %%rename syntax malformed after %ld characters",
		       (long) (p3 - buffer));
	      *p3 = '\0';

	      for (sl = specs; sl; sl = sl->next)
		if (name_len == sl->name_len && !strcmp (sl->name, p1))
		  break;

	      if (!sl)
		fatal ("specs %s spec was not found to be renamed", p1);

	      if (strcmp (p1, p2) == 0)
		continue;

	      for (newsl = specs; newsl; newsl = newsl->next)
		if (strcmp (newsl->name, p2) == 0)
		  fatal ("%s: attempt to rename spec '%s' to already defined spec '%s'",
		    filename, p1, p2);

	      if (verbose_flag)
		{
		  notice ("rename spec %s to %s\n", p1, p2);
#ifdef DEBUG_SPECS
		  notice ("spec is '%s'\n\n", *(sl->ptr_spec));
#endif
		}

	      set_spec (p2, *(sl->ptr_spec));
	      if (sl->alloc_p)
		free (CONST_CAST (char *, *(sl->ptr_spec)));

	      *(sl->ptr_spec) = "";
	      sl->alloc_p = 0;
	      continue;
	    }
	  else
	    fatal ("specs unknown %% command after %ld characters",
		   (long) (p1 - buffer));
	}

      /* Find the colon that should end the suffix.  */
      p1 = p;
      while (*p1 && *p1 != ':' && *p1 != '\n')
	p1++;

      /* The colon shouldn't be missing.  */
      if (*p1 != ':')
	fatal ("specs file malformed after %ld characters",
	       (long) (p1 - buffer));

      /* Skip back over trailing whitespace.  */
      p2 = p1;
      while (p2 > buffer && (p2[-1] == ' ' || p2[-1] == '\t'))
	p2--;

      /* Copy the suffix to a string.  */
      suffix = save_string (p, p2 - p);
      /* Find the next line.  */
      p = skip_whitespace (p1 + 1);
      if (p[1] == 0)
	fatal ("specs file malformed after %ld characters",
	       (long) (p - buffer));

      p1 = p;
      /* Find next blank line or end of string.  */
      while (*p1 && !(*p1 == '\n' && (p1[1] == '\n' || p1[1] == '\0')))
	p1++;

      /* Specs end at the blank line and do not include the newline.  */
      spec = save_string (p, p1 - p);
      p = p1;

      /* Delete backslash-newline sequences from the spec.  */
      in = spec;
      out = spec;
      while (*in != 0)
	{
	  if (in[0] == '\\' && in[1] == '\n')
	    in += 2;
          /* '#' is a normal character for SGCC specs.
	  else if (in[0] == '#')
	    while (*in && *in != '\n')
            in++; */

	  else
	    *out++ = *in++;
	}
      *out = 0;

      if (suffix[0] == '*')
	{
	  if (! strcmp (suffix, "*link_command"))
	    link_command_spec = spec;
	  else
	    set_spec (suffix + 1, spec);
	}
      else
	{
	  /* Add this pair to the vector.  */
	  compilers
	    = xrealloc (compilers,
			(n_compilers + 2) * sizeof (struct compiler));

	  compilers[n_compilers].suffix = suffix;
	  compilers[n_compilers].spec = spec;
	  n_compilers++;
	  memset (&compilers[n_compilers], 0, sizeof compilers[n_compilers]);
	}

      if (*suffix == 0)
	link_command_spec = spec;
    }

  if (link_command_spec == 0)
    fatal ("spec file has no spec for linking");
}

/* Record the names of temporary files we tell compilers to write,
   and delete them at the end of the run.  */

/* This is the common prefix we use to make temp file names.
   It is chosen once for each run of this program.
   It is substituted into a spec by %g or %j.
   Thus, all temp file names contain this prefix.
   In practice, all temp file names start with this prefix.

   This prefix comes from the envvar TMPDIR if it is defined;
   otherwise, from the P_tmpdir macro if that is defined;
   otherwise, in /usr/tmp or /tmp;
   or finally the current directory if all else fails.  */

static const char *temp_filename;

/* Length of the prefix.  */

static int temp_filename_length;

/* Define the list of temporary files to delete.  */

struct temp_file
{
  const char *name;
  struct temp_file *next;
};

/* Queue of files to delete on success or failure of compilation.  */
static struct temp_file *always_delete_queue;
/* Queue of files to delete on failure of compilation.  */
static struct temp_file *failure_delete_queue;

/* Record FILENAME as a file to be deleted automatically.
   ALWAYS_DELETE nonzero means delete it if all compilation succeeds;
   otherwise delete it in any case.
   FAIL_DELETE nonzero means delete it if a compilation step fails;
   otherwise delete it in any case.  */

void
record_temp_file (const char *filename, int always_delete, int fail_delete)
{
  char *const name = xstrdup (filename);

  if (always_delete)
    {
      struct temp_file *temp;
      for (temp = always_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already1;

      temp = XNEW (struct temp_file);
      temp->next = always_delete_queue;
      temp->name = name;
      always_delete_queue = temp;

    already1:;
    }

  if (fail_delete)
    {
      struct temp_file *temp;
      for (temp = failure_delete_queue; temp; temp = temp->next)
	if (! strcmp (name, temp->name))
	  goto already2;

      temp = XNEW (struct temp_file);
      temp->next = failure_delete_queue;
      temp->name = name;
      failure_delete_queue = temp;

    already2:;
    }
}

/* Delete all the temporary files whose names we previously recorded.  */

#ifndef DELETE_IF_ORDINARY
#define DELETE_IF_ORDINARY(NAME,ST,VERBOSE_FLAG)        \
do                                                      \
  {                                                     \
    if (stat (NAME, &ST) >= 0 && S_ISREG (ST.st_mode))  \
      if (unlink (NAME) < 0)                            \
	if (VERBOSE_FLAG)                               \
	  perror_with_name (NAME);                      \
  } while (0)
#endif

static void
delete_if_ordinary (const char *name)
{
  struct stat st;
#ifdef DEBUG
  int i, c;

  printf ("Delete %s? (y or n) ", name);
  fflush (stdout);
  i = getchar ();
  if (i != '\n')
    while ((c = getchar ()) != '\n' && c != EOF)
      ;

  if (i == 'y' || i == 'Y')
#endif /* DEBUG */
  DELETE_IF_ORDINARY (name, st, verbose_flag);
}

static void
delete_temp_files (void)
{
  struct temp_file *temp;

  for (temp = always_delete_queue; temp; temp = temp->next)
    delete_if_ordinary (temp->name);
  always_delete_queue = 0;
}

/* Delete all the files to be deleted on error.  */

static void
delete_failure_queue (void)
{
  struct temp_file *temp;

  for (temp = failure_delete_queue; temp; temp = temp->next)
    delete_if_ordinary (temp->name);
}

static void
clear_failure_queue (void)
{
  failure_delete_queue = 0;
}

/* Call CALLBACK for each path in PATHS, breaking out early if CALLBACK
   returns non-NULL.
   If DO_MULTI is true iterate over the paths twice, first with multilib
   suffix then without, otherwise iterate over the paths once without
   adding a multilib suffix.  When DO_MULTI is true, some attempt is made
   to avoid visiting the same path twice, but we could do better.  For
   instance, /usr/lib/../lib is considered different from /usr/lib.
   At least EXTRA_SPACE chars past the end of the path passed to
   CALLBACK are available for use by the callback.
   CALLBACK_INFO allows extra parameters to be passed to CALLBACK.

   Returns the value returned by CALLBACK.  */

static void *
for_each_path (const struct path_prefix *paths,
	       bool do_multi,
	       size_t extra_space,
	       void *(*callback) (char *, void *),
	       void *callback_info)
{
  struct prefix_list *pl;
  const char *multi_dir = NULL;
  const char *multi_os_dir = NULL;
  const char *multi_suffix;
  const char *just_multi_suffix;
  char *path = NULL;
  void *ret = NULL;
  bool skip_multi_dir = false;
  bool skip_multi_os_dir = false;

  multi_suffix = machine_suffix;
  just_multi_suffix = just_machine_suffix;
  if (do_multi && multilib_dir && strcmp (multilib_dir, ".") != 0)
    {
      multi_dir = concat (multilib_dir, dir_separator_str, NULL);
      multi_suffix = concat (multi_suffix, multi_dir, NULL);
      just_multi_suffix = concat (just_multi_suffix, multi_dir, NULL);
    }
  if (do_multi && multilib_os_dir && strcmp (multilib_os_dir, ".") != 0)
    multi_os_dir = concat (multilib_os_dir, dir_separator_str, NULL);

  if (!multi_suffix)
    multi_suffix = "";
  
  if (!just_multi_suffix)
    just_multi_suffix = "";
  
  while (1)
    {
      size_t multi_dir_len = 0;
      size_t multi_os_dir_len = 0;
      size_t suffix_len;
      size_t just_suffix_len;
      size_t len;

      if (multi_dir)
	multi_dir_len = strlen (multi_dir);
      if (multi_os_dir)
	multi_os_dir_len = strlen (multi_os_dir);
      suffix_len = strlen (multi_suffix);
      just_suffix_len = strlen (just_multi_suffix);

      if (path == NULL)
	{
	  len = paths->max_len + extra_space + 1;
	  if (suffix_len > multi_os_dir_len)
	    len += suffix_len;
	  else
	    len += multi_os_dir_len;
	  path = XNEWVEC (char, len);
	}

      for (pl = paths->plist; pl != 0; pl = pl->next)
	{
	  len = strlen (pl->prefix);
	  memcpy (path, pl->prefix, len);

	  /* Look first in MACHINE/VERSION subdirectory.  */
	  if (!skip_multi_dir)
	    {
	      memcpy (path + len, multi_suffix, suffix_len + 1);
	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }

	  /* Some paths are tried with just the machine (ie. target)
	     subdir.  This is used for finding as, ld, etc.  */
	  if (!skip_multi_dir
	      && pl->require_machine_suffix == 2)
	    {
	      memcpy (path + len, just_multi_suffix, just_suffix_len + 1);
	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }

	  /* Now try the base path.  */
	  if (!pl->require_machine_suffix
	      && !(pl->os_multilib ? skip_multi_os_dir : skip_multi_dir))
	    {
	      const char *this_multi;
	      size_t this_multi_len;

	      if (pl->os_multilib)
		{
		  this_multi = multi_os_dir;
		  this_multi_len = multi_os_dir_len;
		}
	      else
		{
		  this_multi = multi_dir;
		  this_multi_len = multi_dir_len;
		}

	      if (this_multi_len)
		memcpy (path + len, this_multi, this_multi_len + 1);
	      else
		path[len] = '\0';

	      ret = callback (path, callback_info);
	      if (ret)
		break;
	    }
	}
      if (pl)
	break;

      if (multi_dir == NULL && multi_os_dir == NULL)
	break;

      /* Run through the paths again, this time without multilibs.
	 Don't repeat any we have already seen.  */
      if (multi_dir)
	{
	  free (CONST_CAST (char *, multi_dir));
	  multi_dir = NULL;
	  free (CONST_CAST (char *, multi_suffix));
	  multi_suffix = machine_suffix;
	  free (CONST_CAST (char *, just_multi_suffix));
	  just_multi_suffix = just_machine_suffix;
	}
      else
	skip_multi_dir = true;
      if (multi_os_dir)
	{
	  free (CONST_CAST (char *, multi_os_dir));
	  multi_os_dir = NULL;
	}
      else
	skip_multi_os_dir = true;
    }

  if (multi_dir)
    {
      free (CONST_CAST (char *, multi_dir));
      free (CONST_CAST (char *, multi_suffix));
      free (CONST_CAST (char *, just_multi_suffix));
    }
  if (multi_os_dir)
    free (CONST_CAST (char *, multi_os_dir));
  if (ret != path)
    free (path);
  return ret;
}

/* Callback for build_search_list.  Adds path to obstack being built.  */

struct add_to_obstack_info {
  struct obstack *ob;
  bool check_dir;
  bool first_time;
};

static void *
add_to_obstack (char *path, void *data)
{
  struct add_to_obstack_info *info = data;

  if (info->check_dir && !is_directory (path, false))
    return NULL;

  if (!info->first_time)
    obstack_1grow (info->ob, PATH_SEPARATOR);

  obstack_grow (info->ob, path, strlen (path));

  info->first_time = false;
  return NULL;
}

/* Add or change the value of an environment variable, outputting the
   change to standard error if in verbose mode.  */
static void
xputenv (const char *string)
{
  if (verbose_flag)
    notice ("%s\n", string);
  putenv (CONST_CAST (char *, string));
}

/* Build a list of search directories from PATHS.
   PREFIX is a string to prepend to the list.
   If CHECK_DIR_P is true we ensure the directory exists.
   If DO_MULTI is true, multilib paths are output first, then
   non-multilib paths.
   This is used mostly by putenv_from_prefixes so we use `collect_obstack'.
   It is also used by the --print-search-dirs flag.  */

static char *
build_search_list (const struct path_prefix *paths, const char *prefix,
		   bool check_dir, bool do_multi)
{
  struct add_to_obstack_info info;

  info.ob = &collect_obstack;
  info.check_dir = check_dir;
  info.first_time = true;

  obstack_grow (&collect_obstack, prefix, strlen (prefix));
  obstack_1grow (&collect_obstack, '=');

  for_each_path (paths, do_multi, 0, add_to_obstack, &info);

  obstack_1grow (&collect_obstack, '\0');
  return XOBFINISH (&collect_obstack, char *);
}

/* Rebuild the COMPILER_PATH and LIBRARY_PATH environment variables
   for collect.  */

static void
putenv_from_prefixes (const struct path_prefix *paths, const char *env_var,
		      bool do_multi)
{
  xputenv (build_search_list (paths, env_var, true, do_multi));
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

/* Callback for find_a_file.  Appends the file name to the directory
   path.  If the resulting file exists in the right mode, return the
   full pathname to the file.  */

struct file_at_path_info {
  const char *name;
  const char *suffix;
  int name_len;
  int suffix_len;
  int mode;
};

static void *
file_at_path (char *path, void *data)
{
  struct file_at_path_info *info = data;
  size_t len = strlen (path);

  memcpy (path + len, info->name, info->name_len);
  len += info->name_len;

  if (debug_driver_val & 0x04)
     fprintf(stdout,"file_at_path:%s\n",path);
  
  /* Some systems have a suffix for executable files.
     So try appending that first.  */
  if (info->suffix_len)
    {
      memcpy (path + len, info->suffix, info->suffix_len + 1);
      if (access_check (path, info->mode) == 0)
	return path;
    }

  path[len] = '\0';
  if (access_check (path, info->mode) == 0)
    return path;

  return NULL;
}

/* convert the directory, if necessary, to absolute, and append "name" */
static char *make_absolute_from_Y(const char *path, const char *name) 
{
  char *p;

    if(path[0] == '/') {
      p = xmalloc(strlen(path) + strlen(name) + 2);
      (void) strcpy(p, path);
    } else {
      if (current_work_directory == NULL) {
        current_work_directory = xmalloc(MAXPATHLEN+1);
        (void) getcwd(current_work_directory, MAXPATHLEN+1);
      }
      p = xmalloc(strlen(current_work_directory) + strlen(path) + 
                 strlen(name) + 3);
      (void) strcpy(p, current_work_directory);
      (void) strcat(p, "/");
      (void) strcat(p, path);
    }

    if (p[strlen(p) - 1] != '/')
            (void) strcat(p, "/");

    (void) strcat(p, name);

    return (p);
}

/* Search for NAME using the prefix list PREFIXES.  MODE is passed to
   access to check permissions.  If DO_MULTI is true, search multilib
   paths then non-multilib paths, otherwise do not search multilib paths.
   Return 0 if not found, otherwise return its name, allocated with malloc.  */

static char *
find_a_file (const struct path_prefix *pprefix, const char *name, int mode,
	     bool do_multi)
{
  struct file_at_path_info info;

#ifdef DEFAULT_ASSEMBLER
  if (! strcmp (name, "as") && access (DEFAULT_ASSEMBLER, mode) == 0)
    return xstrdup (DEFAULT_ASSEMBLER);
#endif

#ifdef DEFAULT_LINKER
  if (! strcmp(name, "ld") && access (DEFAULT_LINKER, mode) == 0)
    return xstrdup (DEFAULT_LINKER);
#endif

  if (!strcmp(name, "iropt") && alternate_iropt)
    return xstrdup (alternate_iropt);

  if (!strcmp(name, "cg") && alternate_cg)
    return xstrdup (alternate_cg);

  if (!strcmp(name, "ipo") && alternate_ipo)
    return xstrdup (alternate_ipo);

  if (!strcmp(name, "postopt") && alternate_postopt)
    return xstrdup (alternate_postopt);

  if (!strcmp (name, "as") && alternate_as)
    return xstrdup (alternate_as);

  if (!strcmp (name, "fbe") && alternate_as)
    return xstrdup (alternate_as);
  
  /* Determine the filename to execute (special case for absolute paths).  */

  if (IS_ABSOLUTE_PATH (name))
    {
      if (access (name, mode) == 0)
	return xstrdup (name);

      return NULL;
    }

  info.name = name;
  info.suffix = (mode & X_OK) != 0 ? HOST_EXECUTABLE_SUFFIX : "";
  info.name_len = strlen (info.name);
  info.suffix_len = strlen (info.suffix);
  info.mode = mode;

  return for_each_path (pprefix, do_multi, info.name_len + info.suffix_len,
			file_at_path, &info);
}

/* Ranking of prefixes in the sort list. -B prefixes are put before
   all others.  */

enum path_prefix_priority
{
  PREFIX_PRIORITY_B_OPT,
  PREFIX_PRIORITY_LAST
};

/* Add an entry for PREFIX in PLIST.  The PLIST is kept in ascending
   order according to PRIORITY.  Within each PRIORITY, new entries are
   appended.

   If WARN is nonzero, we will warn if no file is found
   through this prefix.  WARN should point to an int
   which will be set to 1 if this entry is used.

   COMPONENT is the value to be passed to update_path.

   REQUIRE_MACHINE_SUFFIX is 1 if this prefix can't be used without
   the complete value of machine_suffix.
   2 means try both machine_suffix and just_machine_suffix.  */

static void
add_prefix (struct path_prefix *pprefix, const char *prefix,
	    const char *component, /* enum prefix_priority */ int priority,
	    int require_machine_suffix, int os_multilib)
{
  struct prefix_list *pl, **prev;
  int len;

  for (prev = &pprefix->plist;
       (*prev) != NULL && (*prev)->priority <= priority;
       prev = &(*prev)->next)
    ;

  /* Keep track of the longest prefix.  */

  prefix = update_path (prefix, component);
  len = strlen (prefix);
  if (len > pprefix->max_len)
    pprefix->max_len = len;

  pl = XNEW (struct prefix_list);
  pl->prefix = prefix;
  pl->require_machine_suffix = require_machine_suffix;
  pl->priority = priority;
  pl->os_multilib = os_multilib;

  /* Insert after PREV.  */
  pl->next = (*prev);
  (*prev) = pl;
}

/* Same as add_prefix, but prepending target_system_root to prefix.  */
/* The target_system_root prefix has been relocated by gcc_exec_prefix.  */
static void
add_sysrooted_prefix (struct path_prefix *pprefix, const char *prefix,
		      const char *component,
		      /* enum prefix_priority */ int priority,
		      int require_machine_suffix, int os_multilib)
{
  if (!IS_ABSOLUTE_PATH (prefix))
    fatal ("system path '%s' is not absolute", prefix);

  if (target_system_root)
    {
      if (target_sysroot_suffix)
	  prefix = concat (target_sysroot_suffix, prefix, NULL);
      prefix = concat (target_system_root, prefix, NULL);

      /* We have to override this because GCC's notion of sysroot
	 moves along with GCC.  */
      component = "GCC";
    }

  add_prefix (pprefix, prefix, component, priority,
	      require_machine_suffix, os_multilib);
}

/* Execute the command specified by the arguments on the current line of spec.
   When using pipes, this includes several piped-together commands
   with `|' between them.

   Return 0 if successful, -1 if failed.  */

static int
execute (void)
{
  int i;
  int n_commands;		/* # of command.  */
  char *string;
  struct pex_obj *pex;
  struct command
  {
    const char *prog;		/* program name.  */
    const char **argv;		/* vector of args.  */
  };

  struct command *commands;	/* each command buffer with above info.  */

  gcc_assert (!processing_spec_function);

  /* Count # of piped commands.  */
  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      n_commands++;

  /* Get storage for each command.  */
  commands = alloca (n_commands * sizeof (struct command));

  /* Split argbuf into its separate piped processes,
     and record info about each one.
     Also search for the programs that are to be run.  */

  commands[0].prog = argbuf[0]; /* first command.  */
  commands[0].argv = &argbuf[0];
  string = find_a_file (&exec_prefixes, commands[0].prog, X_OK, false);

  if (string)
    commands[0].argv[0] = string;

  for (n_commands = 1, i = 0; i < argbuf_index; i++)
    if (strcmp (argbuf[i], "|") == 0)
      {				/* each command.  */
#if defined (__MSDOS__) || defined (OS2) || defined (VMS)
	fatal ("-pipe not supported");
#endif
	argbuf[i] = 0;	/* termination of command args.  */
	commands[n_commands].prog = argbuf[i + 1];
	commands[n_commands].argv = &argbuf[i + 1];
	string = find_a_file (&exec_prefixes, commands[n_commands].prog,
			      X_OK, false);
	if (string)
	  commands[n_commands].argv[0] = string;
	n_commands++;
      }

  argbuf[argbuf_index] = 0;

  /* If -v, print what we are about to do, and maybe query.  */

  if (verbose_flag)
    {
      /* For help listings, put a blank line between sub-processes.  */
      if (print_help_list)
	fputc ('\n', stderr);

      /* Print each piped command as a separate line.  */
      for (i = 0; i < n_commands; i++)
	{
	  const char *const *j;

	  if (verbose_only_flag)
	    {
	      for (j = commands[i].argv; *j; j++)
		{
		  const char *p;
		  fprintf (stderr, " \"");
		  for (p = *j; *p; ++p)
		    {
		      if (*p == '"' || *p == '\\' || *p == '$')
			fputc ('\\', stderr);
		      fputc (*p, stderr);
		    }
		  fputc ('"', stderr);
		}
	    }
	  else
	    for (j = commands[i].argv; *j; j++)
	      fprintf (stderr, " %s", *j);

	  /* Print a pipe symbol after all but the last command.  */
	  if (i + 1 != n_commands)
	    fprintf (stderr, " |");
	  fprintf (stderr, "\n");
	}
      fflush (stderr);
      if (verbose_only_flag != 0)
        {
	  /* verbose_only_flag should act as if the spec was
	     executed, so increment execution_count before
	     returning.  This prevents spurious warnings about
	     unused linker input files, etc.  */
	  execution_count++;
	  return 0;
        }
#ifdef DEBUG
      notice ("\nGo ahead? (y or n) ");
      fflush (stderr);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n')
	  ;

      if (i != 'y' && i != 'Y')
	return 0;
#endif /* DEBUG */
    }

#ifdef ENABLE_VALGRIND_CHECKING
  /* Run the each command through valgrind.  To simplify prepending the
     path to valgrind and the option "-q" (for quiet operation unless
     something triggers), we allocate a separate argv array.  */

  for (i = 0; i < n_commands; i++)
    {
      const char **argv;
      int argc;
      int j;

      for (argc = 0; commands[i].argv[argc] != NULL; argc++)
	;

      argv = alloca ((argc + 3) * sizeof (char *));

      argv[0] = VALGRIND_PATH;
      argv[1] = "-q";
      for (j = 2; j < argc + 2; j++)
	argv[j] = commands[i].argv[j - 2];
      argv[j] = NULL;

      commands[i].argv = argv;
      commands[i].prog = argv[0];
    }
#endif

  /* see if backend has been installed */
  if (find_executable_file("iropt") == NULL)
    fatal("SUNW0scgfss %s has not been installed\n",compiler_version);

  /* Run each piped subprocess.  */

  pex = pex_init (PEX_USE_PIPES | (report_times ? PEX_RECORD_TIMES : 0),
		  programname, temp_filename);
  if (pex == NULL)
    pfatal_with_name (_("pex_init failed"));

  for (i = 0; i < n_commands; i++)
    {
      const char *errmsg;
      int err;
      const char *string = commands[i].argv[0];
      int is_frontend = 0;
      char *ld_library_path = NULL;
     
      /* catch cc1 and cc1plus */
      if (commands[i].prog[0] == 'c' && commands[i].prog[1] == 'c'
	  && commands[i].prog[2] == '1')
	is_frontend = 1;
      else if (commands[i].prog[0] == 'f' && commands[i].prog[1] == '9'
               && commands[i].prog[2] == '5' && commands[i].prog[3] == '1')
	is_frontend = 1;

      /* set ld_library_path for cc1 and cc1plus,
         since they need several backend shared libraries to run */
      if (is_frontend && studioproddir_lib)
        {
          char *ldpath;
	  /* linux doesn't support _32 & _64 suffixes,
	     but on Solaris LD_LIBRARY_PATH_32 environment variable 
	     overrides LD_LIBRARY_PATH */
#ifdef __linux__
#define LD_LIB_PATH "LD_LIBRARY_PATH"
#else
#define LD_LIB_PATH "LD_LIBRARY_PATH_32"
#endif
          GET_ENVIRONMENT (ld_library_path, LD_LIB_PATH);
          ldpath = concat (LD_LIB_PATH "=", studioproddir_lib, ":",
                           studioproddir_lib, "/sys/:", ld_library_path, NULL);
          putenv (ldpath);
          if (debug_driver_val & 0x02) 
            fprintf (stderr, "export %s\n", ldpath);
        }
      
      errmsg = pex_run (pex,
			((i + 1 == n_commands ? PEX_LAST : 0)
			 | (string == commands[i].prog ? PEX_SEARCH : 0)),
			string, CONST_CAST (char **, commands[i].argv),
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

      if (is_frontend && studioproddir_lib)
	{
	  /* restore LD_LIBRARY_PATH */
	  if (ld_library_path)
	    {
              char *ldpath;
              ldpath = concat (LD_LIB_PATH "=", ld_library_path, NULL);
              putenv (ldpath);
	    }
	  else
	    unsetenv (LD_LIB_PATH);
	}
      
      if (string != commands[i].prog)
	free (CONST_CAST (char *, string));
    }

  execution_count++;

  /* Wait for all the subprocesses to finish.  */

  {
    int *statuses;
    struct pex_time *times = NULL;
    int ret_code = 0;

    statuses = alloca (n_commands * sizeof (int));
    if (!pex_get_status (pex, n_commands, statuses))
      pfatal_with_name (_("failed to get exit status"));

    if (report_times)
      {
	times = alloca (n_commands * sizeof (struct pex_time));
	if (!pex_get_times (pex, n_commands, times))
	  pfatal_with_name (_("failed to get process times"));
      }

    pex_free (pex);

    for (i = 0; i < n_commands; ++i)
      {
	int status = statuses[i];

	if (WIFSIGNALED (status))
	  {
#ifdef SIGPIPE
	    /* SIGPIPE is a special case.  It happens in -pipe mode
	       when the compiler dies before the preprocessor is done,
	       or the assembler dies before the compiler is done.
	       There's generally been an error already, and this is
	       just fallout.  So don't generate another error unless
	       we would otherwise have succeeded.  */
	    if (WTERMSIG (status) == SIGPIPE
		&& (signal_count || greatest_status >= MIN_FATAL_STATUS))
	      {
		signal_count++;
		ret_code = -1;
	      }
	    else
#endif
	      fatal_ice ("\
Internal error: %s (program %s)\n\
Please submit a full bug report to\n\
%s.",
		 	strsignal (WTERMSIG (status)), commands[i].prog,
		 	bug_report_url);
	  }
	else if (WIFEXITED (status)
		 && WEXITSTATUS (status) >= MIN_FATAL_STATUS)
	  {
	    if (WEXITSTATUS (status) > greatest_status)
	      greatest_status = WEXITSTATUS (status);
	    ret_code = -1;
	  }

	if (report_times)
	  {
	    struct pex_time *pt = &times[i];
	    double ut, st;

	    ut = ((double) pt->user_seconds
		  + (double) pt->user_microseconds / 1.0e6);
	    st = ((double) pt->system_seconds
		  + (double) pt->system_microseconds / 1.0e6);

	    if (ut + st != 0)
	      notice ("# %s %.2f %.2f\n", commands[i].prog, ut, st);
	  }
      }

    return ret_code;
  }
}

/* Find all the switches given to us
   and make a vector describing them.
   The elements of the vector are strings, one per switch given.
   If a switch uses following arguments, then the `part1' field
   is the switch itself and the `args' field
   is a null-terminated vector containing the following arguments.
   Bits in the `live_cond' field are:
   SWITCH_LIVE to indicate this switch is true in a conditional spec.
   SWITCH_FALSE to indicate this switch is overridden by a later switch.
   SWITCH_IGNORE to indicate this switch should be ignored (used in %<S).
   The `validated' field is nonzero if any spec has looked at this switch;
   if it remains zero at the end of the run, it must be meaningless.  */

#define SWITCH_LIVE    0x1
#define SWITCH_FALSE   0x2
#define SWITCH_IGNORE  0x4

struct switchstr
{
  const char *part1;
  const char **args;
  unsigned int live_cond;
  unsigned char validated;
  unsigned char ordering;
};

static struct switchstr *switches;

static int n_switches;

/* Language is one of three things:

   1) The name of a real programming language.
   2) NULL, indicating that no one has figured out
   what it is yet.
   3) '*', indicating that the file should be passed
   to the linker.  */
struct infile
{
  const char *name;
  const char *language;
  struct compiler *incompiler;
  bool compiled;
  bool preprocessed;
};

/* Also a vector of input files specified.  */

static struct infile *infiles;

int n_infiles;

/* likewise a vector of input .il files specified */
static struct infile *ilfiles;
int n_ilfiles;

/* True if multiple input files are being compiled to a single
   assembly file.  */

static bool combine_inputs;

/* This counts the number of libraries added by lang_specific_driver, so that
   we can tell if there were any user supplied any files or libraries.  */

static int added_libraries;

/* And a vector of corresponding output files is made up later.  */

const char **outfiles;

#if defined(HAVE_TARGET_OBJECT_SUFFIX) || defined(HAVE_TARGET_EXECUTABLE_SUFFIX)

/* Convert NAME to a new name if it is the standard suffix.  DO_EXE
   is true if we should look for an executable suffix.  DO_OBJ
   is true if we should look for an object suffix.  */

static const char *
convert_filename (const char *name, int do_exe ATTRIBUTE_UNUSED,
		  int do_obj ATTRIBUTE_UNUSED)
{
#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
  int i;
#endif
  int len;

  if (name == NULL)
    return NULL;

  len = strlen (name);

#ifdef HAVE_TARGET_OBJECT_SUFFIX
  /* Convert x.o to x.obj if TARGET_OBJECT_SUFFIX is ".obj".  */
  if (do_obj && len > 2
      && name[len - 2] == '.'
      && name[len - 1] == 'o')
    {
      obstack_grow (&obstack, name, len - 2);
      obstack_grow0 (&obstack, TARGET_OBJECT_SUFFIX, strlen (TARGET_OBJECT_SUFFIX));
      name = XOBFINISH (&obstack, const char *);
    }
#endif

#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
  /* If there is no filetype, make it the executable suffix (which includes
     the ".").  But don't get confused if we have just "-o".  */
  if (! do_exe || TARGET_EXECUTABLE_SUFFIX[0] == 0 || (len == 2 && name[0] == '-'))
    return name;

  for (i = len - 1; i >= 0; i--)
    if (IS_DIR_SEPARATOR (name[i]))
      break;

  for (i++; i < len; i++)
    if (name[i] == '.')
      return name;

  obstack_grow (&obstack, name, len);
  obstack_grow0 (&obstack, TARGET_EXECUTABLE_SUFFIX,
		 strlen (TARGET_EXECUTABLE_SUFFIX));
  name = XOBFINISH (&obstack, const char *);
#endif

  return name;
}
#endif

/* Display the command line switches accepted by gcc.  */
static void
display_help (void)
{
  printf (_("Usage: %s [options] file...\n"), programname);
  fputs (_("Options:\n"), stdout);

  fputs (_("  -pass-exit-codes         Exit with highest error code from a phase\n"), stdout);
  fputs (_("  --help                   Display this information\n"), stdout);
  fputs (_("  --target-help            Display target specific command line options\n"), stdout);
  fputs (_("  --help={target|optimizers|warnings|undocumented|params}[,{[^]joined|[^]separate}]\n"), stdout);
  fputs (_("                           Display specific types of command line options\n"), stdout);
  if (! verbose_flag)
    fputs (_("  (Use '-v --help' to display command line options of sub-processes)\n"), stdout);
  fputs (_("  -dumpspecs               Display all of the built in spec strings\n"), stdout);
  fputs (_("  -dumpversion             Display the version of the compiler\n"), stdout);
  fputs (_("  -dumpmachine             Display the compiler's target processor\n"), stdout);
  fputs (_("  -print-search-dirs       Display the directories in the compiler's search path\n"), stdout);
  fputs (_("  -print-libgcc-file-name  Display the name of the compiler's companion library\n"), stdout);
  fputs (_("  -print-file-name=<lib>   Display the full path to library <lib>\n"), stdout);
  fputs (_("  -print-prog-name=<prog>  Display the full path to compiler component <prog>\n"), stdout);
  fputs (_("  -print-multi-directory   Display the root directory for versions of libgcc\n"), stdout);
  fputs (_("\
  -print-multi-lib         Display the mapping between command line options and\n\
                           multiple library search directories\n"), stdout);
  fputs (_("  -print-multi-os-directory Display the relative path to OS libraries\n"), stdout);
  fputs (_("  -print-sysroot-headers-suffix Display the sysroot suffix used to find headers\n"), stdout);
  fputs (_("  -Wa,<options>            Pass comma-separated <options> on to the assembler\n"), stdout);
  fputs (_("  -Wp,<options>            Pass comma-separated <options> on to the preprocessor\n"), stdout);
  fputs (_("  -Wl,<options>            Pass comma-separated <options> on to the linker\n"), stdout);
  fputs (_("  -Xassembler <arg>        Pass <arg> on to the assembler\n"), stdout);
  fputs (_("  -Xpreprocessor <arg>     Pass <arg> on to the preprocessor\n"), stdout);
  fputs (_("  -Xlinker <arg>           Pass <arg> on to the linker\n"), stdout);
  fputs (_("  -combine                 Pass multiple source files to compiler at once\n"), stdout);
  fputs (_("  -save-temps              Do not delete intermediate files\n"), stdout);
  fputs (_("  -pipe                    Use pipes rather than intermediate files\n"), stdout);
  fputs (_("  -time                    Time the execution of each subprocess\n"), stdout);
  fputs (_("  -specs=<file>            Override built-in specs with the contents of <file>\n"), stdout);
  fputs (_("  -std=<standard>          Assume that the input sources are for <standard>\n"), stdout);
  fputs (_("\
  --sysroot=<directory>    Use <directory> as the root directory for headers\n\
                           and libraries\n"), stdout);
  fputs (_("  -B <directory>           Add <directory> to the compiler's search paths\n"), stdout);
  fputs (_("  -b <machine>             Run gcc for target <machine>, if installed\n"), stdout);
  fputs (_("  -V <version>             Run gcc version number <version>, if installed\n"), stdout);
  fputs (_("  -v                       Display the programs invoked by the compiler\n"), stdout);
  fputs (_("  -###                     Like -v but options quoted and commands not executed\n"), stdout);
  fputs (_("  -E                       Preprocess only; do not compile, assemble or link\n"), stdout);
  fputs (_("  -S                       Compile only; do not assemble or link\n"), stdout);
  fputs (_("  -c                       Compile and assemble, but do not link\n"), stdout);
  fputs (_("  -o <file>                Place the output into <file>\n"), stdout);
  fputs (_("\
  -x <language>            Specify the language of the following input files\n\
                           Permissible languages include: c c++ assembler none\n\
                           'none' means revert to the default behavior of\n\
                           guessing the language based on the file's extension\n\
"), stdout);

  printf (_("\
\nOptions starting with -g, -f, -m, -O, -W, or --param are automatically\n\
 passed on to the various sub-processes invoked by %s.  In order to pass\n\
 other options on to these processes the -W<letter> options must be used.\n\
"), programname);

  printf (_("\
\nAdditional options specific to GCC For SPARC(R) Systems:\n\
") );
  fputs (_("  -dalign                  Same as -xmemalign=8s\n"), stdout);
  fputs (_("  -fast                    Starting point for maximum runtime performance\n"), stdout);
  fputs (_("  -fns[={no|yes}]          Turn on nostandard floating point mode\n"), stdout);
  fputs (_("  -fnonstd                 Same as -fns -ftrap=common\n"), stdout);
  fputs (_("  -fsimple[=<val>]         Amount of floating point simplification\n"), stdout);
  fputs (_("  -ftrap=<val>             Select floating-point trapping mode\n"), stdout);
  fputs (_("  -KPIC                    Same as -xcode=pic32\n"), stdout);
  fputs (_("  -Kpic                    Same as -xcode=pic13\n"), stdout);
  fputs (_("  -mt                      Macro for -D_REENTRANT -lthread\n"), stdout);
  fputs (_("  -native                  Same as -xtarget=native\n"), stdout);
  fputs (_("  -xalias_level=<level>    Assumptions for type-based alias-analysis\n"), stdout);
  fputs (_("  -xannotate[={no|yes}]    Turn on annotations (default is yes)\n"), stdout);
  fputs (_("  -xarch=<arch>            Compile for architecture <arch>\n"), stdout);
  fputs (_("  -xautopar                Autoparallize\n"), stdout);
  fputs (_("  -xbinopt=prepare         Prepare binary for Binary Improvement Tools (bit)\n"), stdout);
  fputs (_("  -xcache=<cache>          Compile for cache with <cache>\n"), stdout);
  fputs (_("  -xchar=<c>               Same as -fsigned-char or -funsigned-char\n"), stdout);
  fputs (_("  -xchip=<c>               Compile for chip <c>\n"), stdout);
  fputs (_("  -xcode[=<val>]           Specify code address space\n"), stdout);
  fputs (_("  -xc99                    (gcc only) Works with -Xc for c99 standard\n"), stdout);
  fputs (_("  -xdepend                 Analyzes loops for inter-iteration data dependencies\n"), stdout);
  fputs (_("  -xexplicitpar            Parallelize loops explicitly marked with directives\n"), stdout);
  fputs (_("  -xhwcprof[=<eord>]       Hardware counter profiling\n"), stdout);
  fputs (_("  -xinline=[<f>]           Control inlining\n"), stdout);
  fputs (_("  -xipo[=0|1|2]            Invoke IPO at level 1 or 2\n"), stdout);
  fputs (_("  -xipo_archive=[<a>]      Optimize files in library with IPO\n"), stdout);
  fputs (_("  -xlibmil                 Inline some library routines\n"), stdout);
  fputs (_("  -xlinkopt[=<level>]      Perform link-time optimizations\n"), stdout);
  fputs (_("  -xloopinfo               Information about parallelization\n"), stdout);
  fputs (_("  -xmemalign=<val>         Specify maximum assumed memory alignment\n"), stdout);
  fputs (_("  -xopenmp[={noopt|parallel|none}] Enable OpenMP language extensions\n"), stdout);
  fputs (_("  -tm_mode[={none|htm|phtm|stm}] Enable TM language extensions\n"), stdout);
  fputs (_("  -xpagesize=<val>         Set the size of pages to <val>\n"), stdout);
  fputs (_("  -xparallel               Equivalent to -xautopar -xdepend -xexplicitpar\n"), stdout);
  fputs (_("  -xpec                    Generate portable executable code for automatic tuning\n"), stdout);
  fputs (_("  -xprefetch=<val>         Enable prefetch instructions\n"), stdout);
  fputs (_("  -xprefetch_auto_type=<val>   Control aggressiveness of prefetch insertion\n"), stdout);
  fputs (_("  -xprefetch_level=<val>   Control aggressiveness of prefetch insertion\n"), stdout);
  fputs (_("  -xprofile=collect[:<loc>] Compile for collecting profile information\n"), stdout);
  fputs (_("  -xprofile=use[:<loc>]    Compile using profile information\n"), stdout);
  fputs (_("  -xregs=<val>             Specifies the usage of registers for the generated code\n"), stdout);
  fputs (_("  -xreduction              Autoparallize reductions (needs -xautopar too)\n"), stdout);
  fputs (_("  -xrestrict[=<funcs>]     Treats pointer-valued function parameters as restricted pointers\n"), stdout);
  fputs (_("  -xsafe=mem               Assume no memory-based traps occur\n"), stdout);
  fputs (_("  -xspace                  Does no optimization or parallelization that increase code size\n"), stdout);
  fputs (_("  -xtarget=<targ>          Compile for <targ>\n"), stdout);
  fputs (_("  -xthreadvar              Control implementation of thread local variables\n"), stdout);
  fputs (_("  -xunroll=<value>         Suggest unrolling <value> times\n"), stdout);
  fputs (_("  -xvector                 Enable automatic generation of call to the vector library functions\n"), stdout);
  fputs (_("  -xvis[=yes]              Same as -mvis\n"), stdout);
  fputs (_("  -W0,<options>            Pass comma-separated <options> on to the frontend\n"), stdout);
  fputs (_("  -W2,<options>            Pass comma-separated <options> on to the iropt\n"), stdout);
  fputs (_("  -Wc,<options>            Pass comma-separated <options> on to the code generator\n"), stdout);
  fputs (_("  -WO,<options>            Pass comma-separated <options> on to the ipo\n"), stdout);
  fputs (_("  -Wo,<options>            Pass comma-separated <options> on to the postopt\n"), stdout);
  fputs (_("  -Xc                      (gcc only) which C language standard \n"), stdout);
  printf (_("\
\nA forum for discussion about GCC For SPARC(R) Systems \
\nis available at URL http://forum.java.sun.com/forum.jspa?forumID=905 .\
\nAlso visit http://cooltools.sunsource.net for the various CoolTools\n\
") );
  
  /* The rest of the options are displayed by invocations of the various
     sub-processes.  */
}

static void
add_preprocessor_option (const char *option, int len)
{
  n_preprocessor_options++;

  if (! preprocessor_options)
    preprocessor_options = XNEWVEC (char *, n_preprocessor_options);
  else
    preprocessor_options = xrealloc (preprocessor_options,
				     n_preprocessor_options * sizeof (char *));

  preprocessor_options [n_preprocessor_options - 1] =
    save_string (option, len);
}

static void
add_frontend_option (const char *option, int len)
{
  n_frontend_options++;

  if (! frontend_options)
    frontend_options = xmalloc (n_frontend_options * sizeof (char *));
  else
    frontend_options = xrealloc (frontend_options,
				  n_frontend_options * sizeof (char *));

  frontend_options [n_frontend_options - 1] = save_string (option, len);
}

static void
add_iropt_option (const char *option, int len)
{
  n_iropt_options++;

  if (! iropt_options)
    iropt_options = xmalloc (n_iropt_options * sizeof (char *));
  else
    iropt_options = xrealloc (iropt_options,
				  n_iropt_options * sizeof (char *));

  iropt_options [n_iropt_options - 1] = save_string (option, len);
}

static void
add_sscg_option (const char *option, int len)
{
  n_sscg_options++;

  if (! sscg_options)
    sscg_options = xmalloc (n_sscg_options * sizeof (char *));
  else
    sscg_options = xrealloc (sscg_options,
				  n_sscg_options * sizeof (char *));

  sscg_options [n_sscg_options - 1] = save_string (option, len);
}

static void
add_ipo_option (const char *option, int len)
{
  n_ipo_options++;

  if (! ipo_options)
    ipo_options = xmalloc (n_ipo_options * sizeof (char *));
  else
    ipo_options = xrealloc (ipo_options,
				  n_ipo_options * sizeof (char *));

  ipo_options [n_ipo_options - 1] = save_string (option, len);
}

static void
add_postopt_option (const char *option, int len)
{
  n_postopt_options++;

  if (! postopt_options)
    postopt_options = xmalloc (n_postopt_options * sizeof (char *));
  else
    postopt_options = xrealloc (postopt_options,
				  n_postopt_options * sizeof (char *));

  postopt_options [n_postopt_options - 1] = save_string (option, len);
}

static void
add_assembler_option (const char *option, int len)
{
  n_assembler_options++;

  if (! assembler_options)
    assembler_options = XNEWVEC (char *, n_assembler_options);
  else
    assembler_options = xrealloc (assembler_options,
				  n_assembler_options * sizeof (char *));

  assembler_options [n_assembler_options - 1] = save_string (option, len);
}

static void
add_linker_option (const char *option, int len)
{
  n_linker_options++;

  if (! linker_options)
    linker_options = XNEWVEC (char *, n_linker_options);
  else
    linker_options = xrealloc (linker_options,
			       n_linker_options * sizeof (char *));

  linker_options [n_linker_options - 1] = save_string (option, len);
}

/* Create the vector `switches' and its contents.
   Store its length in `n_switches'.  */

static void
process_command (int argc, const char **argv)
{
  int i;
  const char *temp;
  char *temp1;
  const char *spec_lang = 0;
  int last_language_n_infiles;
  int lang_n_infiles = 0;
  char *path_to_driver_wo_driver;
#ifdef MODIFY_TARGET_NAME
  int is_modify_target_name;
  unsigned int j;
#endif
  const char *tooldir_prefix;
  
  GET_ENVIRONMENT(debug_driver, "DEBUG_GCC_DRIVER");
  if (debug_driver)
    debug_driver_val = strtol(debug_driver, (char **)NULL, 10);

  if (debug_driver)
    fprintf(stdout,"DEBUG_DRIVER=%s DEBUG_DRIVER_VAL=%ld\n",debug_driver, debug_driver_val);

  GET_ENVIRONMENT (gcc_exec_prefix, "GCC_EXEC_PREFIX");

  n_switches = 0;
  n_infiles = 0;
  added_libraries = 0;
  n_ilfiles = 0;

  if (debug_driver_val & 0x01) 
    fprintf (stdout, "argv[0]=%s\n", argv[0]);

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
                 add_prefix (&PATH_prefixes, nstore, 0,
                             PREFIX_PRIORITY_LAST, 0, 0);
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
      path_to_driver = find_a_file (&PATH_prefixes, argv[0], X_OK, 0);
    }
  
  if (path_to_driver == NULL) 
    {
      path_to_driver = xstrdup (argv[0]);
    }

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
  if (debug_driver_val & 0x01) 
    fprintf (stdout, "path_to_driver=%s\n", path_to_driver);
            
  /* Figure compiler version from version string.  */

  compiler_version = temp1 = xstrdup (version_string);

  for (; *temp1; ++temp1)
    {
      if (*temp1 == ' ')
	{
	  *temp1 = '\0';
	  break;
	}
    }

  /* If there is a -V or -b option (or both), process it now, before
     trying to interpret the rest of the command line.
     Use heuristic that all configuration names must have at least
     one dash '-'. This allows us to pass options starting with -b.  */
  if (argc > 1 && argv[1][0] == '-'
      && (argv[1][1] == 'V' ||
	 ((argv[1][1] == 'b') && (NULL != strchr(argv[1] + 2,'-')))))
    {
      const char *new_version = DEFAULT_TARGET_VERSION;
      const char *new_machine = DEFAULT_TARGET_MACHINE;
      const char *progname = argv[0];
      char **new_argv;
      char *new_argv0;
      int baselen;

      while (argc > 1 && argv[1][0] == '-'
	     && (argv[1][1] == 'V' ||
		((argv[1][1] == 'b') && ( NULL != strchr(argv[1] + 2,'-')))))
	{
	  char opt = argv[1][1];
	  const char *arg;
	  if (argv[1][2] != '\0')
	    {
	      arg = argv[1] + 2;
	      argc -= 1;
	      argv += 1;
	    }
	  else if (argc > 2)
	    {
	      arg = argv[2];
	      argc -= 2;
	      argv += 2;
	    }
	  else
	    fatal ("'-%c' option must have argument", opt);
	  if (opt == 'V')
	    new_version = arg;
	  else
	    new_machine = arg;
	}

      for (baselen = strlen (progname); baselen > 0; baselen--)
	if (IS_DIR_SEPARATOR (progname[baselen-1]))
	  break;
      new_argv0 = xmemdup (progname, baselen,
			   baselen + concat_length (new_version, new_machine,
						    "-gcc-", NULL) + 1);
      strcpy (new_argv0 + baselen, new_machine);
      strcat (new_argv0, "-gcc-");
      strcat (new_argv0, new_version);

      new_argv = xmemdup (argv, (argc + 1) * sizeof (argv[0]),
			  (argc + 1) * sizeof (argv[0]));
      new_argv[0] = new_argv0;

      execvp (new_argv0, new_argv);
      fatal ("couldn't run '%s': %s", new_argv0, xstrerror (errno));
    }

#ifndef __linux__
  else
    {
#include <sys/systeminfo.h>
      /* don't use modified argv[0] for execvp below, but use 'path_to_driver',
         since we want to look for solaris dependent driver in the real
         location of the 'gcc' driver and not in symlinked dir */
      const char *progname = path_to_driver;
      int baselen;
      
      for (baselen = strlen (progname); baselen > 0; baselen--)
        if (IS_DIR_SEPARATOR (progname[baselen-1]))
          break;
      
      /* recursive check */
      if (strncmp (progname + baselen, "sparc-sun-solaris2", 18) != 0)
        {
          char sysinfo_buf[32];
          const char * new_machine;
          char **new_argv;
          char *new_argv0;

          sysinfo_buf[0] = 0; /* init */
          /* get Solaris version number.
           * 5.9 -> solaris 2.9
           * 5.10 -> solaris 2.10 */
          sysinfo (SI_RELEASE, sysinfo_buf, 32); 

          if (strlen (sysinfo_buf) <= 3) /* 5.7, 5.8, 5.9 */
            new_machine = "sparc-sun-solaris2.9"; /* hardcoded triplet */
          else
            new_machine = "sparc-sun-solaris2.10";

          new_argv0 = xmemdup (progname, baselen,
                               baselen + concat_length (new_machine, "-", 
                                                        progname + baselen, NULL) + 1);
          strcpy (new_argv0 + baselen, new_machine);
          strcat (new_argv0, "-");
          strcat (new_argv0, progname + baselen);

          new_argv = xmemdup (argv, (argc + 1) * sizeof (argv[0]),
                              (argc + 1 + 1) * sizeof (argv[0]));
          new_argv[0] = new_argv0;
          new_argv[argc+1] = new_argv[argc];
          new_argv[argc] = concat("-xgccdriver=",path_to_driver,NULL);

          /* call new driver
           * Ex. sparc-sun-solaris2.9-g++ */
          execvp (new_argv0, new_argv);
          /* fall through -> failed to execute new_argv0. Ok during the build. */
          if (debug_driver_val & 0x01)
            notice ("couldn't run '%s': %s\n", new_argv0, xstrerror (errno));
        }
    }        
#endif

  /* save off the original command line */
  orig_argv = xmemdup (argv, argc * sizeof (argv[0]),
			  argc * sizeof (argv[0]));
  orig_argc = argc;
  
  /* Set up the default search paths.  If there is no GCC_EXEC_PREFIX,
     see if we can create it from the pathname specified in argv[0].  */

  gcc_libexec_prefix = standard_libexec_prefix;
#ifndef VMS
  /* FIXME: make_relative_prefix doesn't yet work for VMS.  */
  if (!gcc_exec_prefix)
    {
      gcc_exec_prefix = make_relative_prefix (argv[0], standard_bindir_prefix,
					      standard_exec_prefix);
      gcc_libexec_prefix = make_relative_prefix (argv[0],
						 standard_bindir_prefix,
						 standard_libexec_prefix);
      if ((debug_driver_val & 0x02)) {
        fprintf(stdout,"GCC_EXEC_PREFIX(1)=%s\n",gcc_exec_prefix ? gcc_exec_prefix : "NULL");
        fprintf(stdout,"GCC_LIBEXEC_PREFIX=%s\n", gcc_libexec_prefix ? gcc_libexec_prefix : "NULL");
      }
      if (gcc_exec_prefix)
	xputenv (concat ("GCC_EXEC_PREFIX=", gcc_exec_prefix, NULL));
    }
  else
    {
      /* make_relative_prefix requires a program name, but
	 GCC_EXEC_PREFIX is typically a directory name with a trailing
	 / (which is ignored by make_relative_prefix), so append a
	 program name.  */
      char *tmp_prefix = concat (gcc_exec_prefix, "gcc", NULL);
      gcc_libexec_prefix = make_relative_prefix (tmp_prefix,
						 standard_exec_prefix,
						 standard_libexec_prefix);

      /* The path is unrelocated, so fallback to the original setting.  */
      if (!gcc_libexec_prefix)
	gcc_libexec_prefix = standard_libexec_prefix;

      free (tmp_prefix);
      if ((debug_driver_val & 0x02))
        {
          fprintf(stdout,"GCC_EXEC_PREFIX(2)=%s\n",
                  gcc_exec_prefix ? gcc_exec_prefix : "NULL");
          fprintf(stdout,"GCC_LIBEXEC_PREFIX=%s\n",
                  gcc_libexec_prefix ? gcc_libexec_prefix : "NULL");
        }
    }
#else
#endif

  if ((debug_driver_val & 0x02))
    {
      fprintf(stdout,"GCC_EXEC_PREFIX(3)=%s\n",gcc_exec_prefix ? gcc_exec_prefix : "NULL");
      fprintf(stdout,"GCC_LIBEXEC_PREFIX=%s\n", gcc_libexec_prefix ? gcc_libexec_prefix : "NULL");
      fprintf(stdout,"STANDARD_STARTFILE_PREFIX=%s\n", 
                     standard_startfile_prefix ? standard_startfile_prefix : "NULL");
      fprintf(stdout,"STANDARD_EXEC_PREFIX=%s\n", 
                     standard_exec_prefix ? standard_exec_prefix : "NULL");
    }
  
  /* From this point onward, gcc_exec_prefix is non-null if the toolchain
     is relocated. The toolchain was either relocated using GCC_EXEC_PREFIX
     or an automatically created GCC_EXEC_PREFIX from argv[0].  */

  if (gcc_exec_prefix)
    {
      int len = strlen (gcc_exec_prefix);

      if (len > (int) sizeof ("/lib/gcc/") - 1
	  && (IS_DIR_SEPARATOR (gcc_exec_prefix[len-1])))
	{
	  temp = gcc_exec_prefix + len - sizeof ("/lib/gcc/") + 1;
	  if (IS_DIR_SEPARATOR (*temp)
	      && strncmp (temp + 1, "lib", 3) == 0
	      && IS_DIR_SEPARATOR (temp[4])
	      && strncmp (temp + 5, "gcc", 3) == 0)
	    len -= sizeof ("/lib/gcc/") - 1;
	}

      set_std_prefix (gcc_exec_prefix, len);
      add_prefix (&exec_prefixes, gcc_libexec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 0, 0);
      add_prefix (&startfile_prefixes, gcc_exec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 0, 0);
    }

  /* add path_to_driver 
     Needed for installing in /export and invoking via /net/mach/export */ 
  if (path_to_driver) 
    {
      char *cp;

      path_to_driver_wo_driver = xstrdup (path_to_driver);
      cp = path_to_driver_wo_driver + strlen (path_to_driver_wo_driver) - 1;
      while (*cp != '/') cp--;
      cp++; /* keep the / */
      *cp = 0;
      if (debug_driver_val & 0x02)
        fprintf (stdout,"PATH_TO_DRIVER_WO_DRIVER=%s\n", path_to_driver_wo_driver);
      
      add_prefix (&exec_prefixes, path_to_driver_wo_driver, "GCC",
                  PREFIX_PRIORITY_LAST, 0, 0);
      add_prefix (&startfile_prefixes, 
                  concat(path_to_driver_wo_driver,"../lib/",NULL),
                  "GCC", PREFIX_PRIORITY_LAST, 0, 1);
      
      if (debug_driver_val & 0x20) 
          fprintf (stdout,"add_prefix(startfile)=%s\n",
                   concat (path_to_driver_wo_driver, "../lib/", NULL));
      
      GET_ENVIRONMENT (sunw_studio_path, "SUNW_STUDIO_PATH");
      GET_ENVIRONMENT (sunw_scgfss_path, "SUNW_SCGFSS_PATH");

      /* check for where the backend is located 
          and fixup, if needed, studioproddir */
      if (studioproddir && directory_exists (studioproddir)) 
        {
        /* nothing to do */
        }
      /* or where they hinted via the environment variable */
      else if (directory_exists (sunw_studio_path)
               && valid_backend_version (sunw_studio_path))
        {
           studioproddir = concat (sunw_studio_path, "/prod/", NULL);
        } 
      /* or where they hinted via the environment variable */
      else if (directory_exists (sunw_scgfss_path)
               && valid_backend_version (concat (sunw_scgfss_path, "/", spec_version, NULL)))
        {
           studioproddir = concat (sunw_scgfss_path, "/", 
                                   spec_version, "/prod/", NULL);
        }
      else if (directory_exists (concat (path_to_driver_wo_driver,
                                         "/../../SUNW0scgfss/", 
                                         spec_version, "/prod", NULL) ) )
        {
           /* the usual place */
           studioproddir = concat (path_to_driver_wo_driver, 
                                   "/../../SUNW0scgfss/",
                                   spec_version, "/prod", NULL);
        } 
      else if (directory_exists (concat (path_to_driver_wo_driver,
                                         "/../lib/gcc/sparc-sun-solaris2.11/gccfss/",
                                         spec_version, "/prod", NULL) ) )
        {
           /* the usual OpenSolaris place */
           studioproddir = concat (path_to_driver_wo_driver, 
                                   "/../lib/gcc/sparc-sun-solaris2.11/gccfss/",
                                   spec_version, "/prod", NULL);
        } 
      /* how about in ../../SUNWspro , ie with SUNWspro */
      else if (directory_exists (concat (path_to_driver_wo_driver, 
                                         "../../SUNWspro/",NULL)) 
               && valid_backend_version (concat (path_to_driver_wo_driver, 
						 "../../SUNWspro/", NULL))) 
        {
           studioproddir = concat (path_to_driver_wo_driver,
                                   "../../SUNWspro/prod/", NULL);
        } 
     else if (directory_exists (concat ("/usr/lib/gcc/", spec_machine,
                                         "/gccfss/", spec_version,
                                         "/prod", NULL) ) )
        {
           /* the usual OpenSolaris place */
           studioproddir = concat ("/usr/lib/gcc/", spec_machine,
                                   "/gccfss/", spec_version,
                                   "/prod", NULL);
        }
    } 
  /* want gcc_exec_prefix and studioproddir as places for library files but
     after the places where gcc normally stashes them; ie. get the Studio 
     libs after looking for the GCC libs */
  if (gcc_exec_prefix) 
    {
      add_prefix (&startfile_prefixes, gcc_exec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 0, 0);
      if (debug_driver_val & 0x20) 
         fprintf (stdout, "add_prefix(startfile)=%s\n", gcc_exec_prefix);
    }
 
  if (studioproddir)
    {
      struct stat statbuf;
      
      studioproddir = lrealpath (studioproddir);
      studioproddir_bin = concat (studioproddir, "/bin/", NULL);
      studioproddir_lib = concat (studioproddir, "/lib", NULL);
      /* add in usual location of iropt and cg */
      add_prefix (&exec_prefixes, studioproddir_bin, "GCC",
                    PREFIX_PRIORITY_LAST, 0, 0);

      if (debug_driver_val & 0x02)
        fprintf (stdout,"PATH_TO_SS=%s\n", studioproddir_lib);

      add_iropt_option ("-h_gen_eh_table", sizeof ("-h_gen_eh_table"));
    }


#ifdef CROSS_DIRECTORY_STRUCTURE
  /* GCCFSS_SOLARIS_SPARC_INCLUDE, GCCFSS_SOLARIS_SPARC_LIB,
     GCCFSS_LINUX_SPARC_INCLUDE and GCCFSS_LINUX_SPARC_LIB have values
     that are lists of directory names with colons.  */

  temp = NULL;
  /* assumes if linux target then linux host
     or if solaris target then solaris host */
#ifdef __linux__
  GET_ENVIRONMENT (temp, "GCCFSS_LINUX_SPARC_INCLUDE");
#else
  GET_ENVIRONMENT (temp, "GCCFSS_SOLARIS_SPARC_INCLUDE");
#endif
  if (temp)
    {
      const char *startp, *endp;
      char *nstore = alloca (strlen (temp) + 3);

      if ((debug_driver_val & 0x01)) 
         fprintf(stdout,"GCCFSS_*_SPARC_INCLUDE=%s\n",temp);

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
	      add_prefix (&cross_include_prefixes, nstore, 0,
			  PREFIX_PRIORITY_LAST, 0, 0);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  temp = NULL;
#ifdef __linux__
  GET_ENVIRONMENT (temp, "GCCFSS_LINUX_SPARC_LIB");
#else
  GET_ENVIRONMENT (temp, "GCCFSS_SOLARIS_SPARC_LIB");
#endif
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
	      add_prefix (&startfile_prefixes, nstore, NULL,
			  PREFIX_PRIORITY_LAST, 0, 1);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }
#endif
  
  /* COMPILER_PATH and LIBRARY_PATH have values
     that are lists of directory names with colons.  */

  GET_ENVIRONMENT (temp, "COMPILER_PATH");
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
	      add_prefix (&exec_prefixes, nstore, 0,
			  PREFIX_PRIORITY_LAST, 0, 0);
	      add_prefix (&include_prefixes, nstore, 0,
			  PREFIX_PRIORITY_LAST, 0, 0);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  GET_ENVIRONMENT (temp, LIBRARY_PATH_ENV);
  if (temp && *cross_compile == '0')
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
	      add_prefix (&startfile_prefixes, nstore, NULL,
			  PREFIX_PRIORITY_LAST, 0, 1);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  /* Use LPATH like LIBRARY_PATH (for the CMU build program).  */
  GET_ENVIRONMENT (temp, "LPATH");
  if (temp && *cross_compile == '0')
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
	      add_prefix (&startfile_prefixes, nstore, NULL,
			  PREFIX_PRIORITY_LAST, 0, 1);
	      if (*endp == 0)
		break;
	      endp = startp = endp + 1;
	    }
	  else
	    endp++;
	}
    }

  /* Convert new-style -- options to old-style.  */
  translate_options (&argc, (const char *const **) &argv);

  if (translate_native_options (&argc, (char ***)&argv) != 0)
    /* translated some native option and generated -xtarget */
    translate_options (&argc, (const char *const **) &argv);

  if (debug_driver_val & 0x01) 
    {
      int i;
      fprintf (stdout, "AFTER TRANSLATE_NATIVE_OPTIONS\n");
      for (i=0; i< argc; i++)
        fprintf (stdout, "argv[%d]=%s\n", i, argv[i]);
    }

  /* Delete duplicate options that could cause problems 
     and handle right-most wins (almost always) rule */
  delete_duplicate_options (&argc, (char ***)&argv);
  
  /* Do language-specific adjustment/addition of flags.  */
  lang_specific_driver (&argc, (const char *const **) &argv, &added_libraries);

  if (debug_driver_val & 0x01) 
    {
      int i;
      fprintf (stdout, "AFTER LANG_SPECIFIC_DRIVER\n");
      for (i=0; i< argc; i++)
        fprintf (stdout, "argv[%d]=%s\n", i, argv[i]);
    }

  /* Delete duplicate options that could cause problems
     which were introduced by lang-specific-driver      */
  delete_duplicate_options (&argc, (char ***)&argv);

  /* At this point all -Zm= should either be eliminated to turned 
     into -m32 or -m64  */

  /* This is really language specific but want to avoid expanding
     scope of some variables */
  for (i = 1; i < argc; i++) 
    {
      if (strcmp (argv[i], "-Zpecdummyc") == 0)
        argv[i] = concat (studioproddir_lib, "/ipo/pec_dummy.c", NULL);
      else if (strcmp (argv[i], "-Zpecdummycc") == 0)
        argv[i] = concat (studioproddir_lib, "/ipo/pec_dummy.cc", NULL);
    }

  if ((debug_driver_val & 0x01)) {
    int i;
    fprintf(stdout,"AFTER LANG_SPECIFIC_DRIVER and DELETE_DUPLICATE_OPTIONS\n");
    for (i=0; i< argc; i++)
      fprintf(stdout,"argv[%d]=%s\n",i,argv[i]);
  }
  
  /* Scan argv twice.  Here, the first time, just count how many switches
     there will be in their vector, and how many input files in theirs.
     Here we also parse the switches that cc itself uses (e.g. -v).  */

  for (i = 1; i < argc; i++)
    {
      if (! strcmp (argv[i], "-dumpspecs"))
	{
	  struct spec_list *sl;
	  init_spec ();
	  for (sl = specs; sl; sl = sl->next)
	    printf ("*%s:\n%s\n\n", sl->name, *(sl->ptr_spec));
	  if (link_command_spec)
	    printf ("*link_command:\n%s\n\n", link_command_spec);
	  exit (0);
	}
      else if (! strcmp (argv[i], "-dumpversion"))
	{
	  printf ("%s\n", spec_version);
	  exit (0);
	}
      else if (! strcmp (argv[i], "-dumpmachine"))
	{
	  printf ("%s\n", spec_machine);
	  exit (0);
	}
      else if (strcmp (argv[i], "-fversion") == 0)
	{
	  /* translate_options () has turned --version into -fversion.  */
	  printf (_("%s %s%s\n"), programname, pkgversion_string,
		  version_string);
	  printf ("Copyright %s 2008 Free Software Foundation, Inc.\n",
		  _("(C)"));
	  fputs (_("This is free software; see the source for copying conditions.  There is NO\n\
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.\n\n"),
		 stdout);
	  exit (0);
	}
      else if (strcmp (argv[i], "-fhelp") == 0)
	{
	  /* translate_options () has turned --help into -fhelp.  */
	  print_help_list = 1;

	  /* We will be passing a dummy file on to the sub-processes.  */
	  n_infiles++;
	  n_switches++;

	  /* CPP driver cannot obtain switch from cc1_options.  */
	  if (is_cpp_driver)
	    add_preprocessor_option ("--help", 6);
	  add_assembler_option ("--help", 6);
	  add_linker_option ("--help", 6);
	}
      else if (strncmp (argv[i], "-fhelp=", 7) == 0)
	{
	  /* translate_options () has turned --help into -fhelp.  */
	  print_subprocess_help = 2;

	  /* We will be passing a dummy file on to the sub-processes.  */
	  n_infiles++;
	  n_switches++;
	}
      else if (strcmp (argv[i], "-ftarget-help") == 0)
	{
	  /* translate_options() has turned --target-help into -ftarget-help.  */
	  print_subprocess_help = 1;

	  /* We will be passing a dummy file on to the sub-processes.  */
	  n_infiles++;
	  n_switches++;

	  /* CPP driver cannot obtain switch from cc1_options.  */
	  if (is_cpp_driver)
	    add_preprocessor_option ("--target-help", 13);
	  add_assembler_option ("--target-help", 13);
	  add_linker_option ("--target-help", 13);
	}
      else if (! strcmp (argv[i], "-pass-exit-codes"))
	{
	  pass_exit_codes = 1;
	  n_switches++;
	}
      else if (! strcmp (argv[i], "-print-search-dirs"))
	print_search_dirs = 1;
      else if (! strcmp (argv[i], "-print-libgcc-file-name"))
	print_file_name = "libgcc.a";
      else if (! strncmp (argv[i], "-print-file-name=", 17))
	print_file_name = argv[i] + 17;
      else if (! strncmp (argv[i], "-print-prog-name=", 17))
	print_prog_name = argv[i] + 17;
      else if (! strcmp (argv[i], "-print-multi-lib"))
	print_multi_lib = 1;
      else if (! strcmp (argv[i], "-print-multi-directory"))
	print_multi_directory = 1;
      else if (! strcmp (argv[i], "-print-multi-os-directory"))
	print_multi_os_directory = 1;
      else if (! strcmp (argv[i], "-print-sysroot-headers-suffix"))
	print_sysroot_headers_suffix = 1;
      else if (! strncmp (argv[i], "-Wa,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the assembler.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_assembler_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_assembler_option (argv[i] + prev, j - prev);
	}
      else if (! strncmp (argv[i], "-W0,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the frontend.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_frontend_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_frontend_option (argv[i] + prev, j - prev);
	}
      else if (! strncmp (argv[i], "-W2,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the iropt.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_iropt_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_iropt_option (argv[i] + prev, j - prev);
	}
      else if (! strncmp (argv[i], "-Wo,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the postopt.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_postopt_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_postopt_option (argv[i] + prev, j - prev);
	}
      else if (! strncmp (argv[i], "-Wc,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the Sun cg.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_sscg_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_sscg_option (argv[i] + prev, j - prev);
	}
      else if (! strncmp (argv[i], "-WO,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the Sun ipo.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_ipo_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_ipo_option (argv[i] + prev, j - prev);
	}
      else if (! strncmp (argv[i], "-Wp,", 4))
	{
	  int prev, j;
	  /* Pass the rest of this option to the preprocessor.  */

	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		add_preprocessor_option (argv[i] + prev, j - prev);
		prev = j + 1;
	      }

	  /* Record the part after the last comma.  */
	  add_preprocessor_option (argv[i] + prev, j - prev);
	}
      else if (argv[i][0] == '+' && argv[i][1] == 'e')
	/* The +e options to the C++ front-end.  */
	n_switches++;
      else if (strncmp (argv[i], "-Wl,", 4) == 0)
	{
	  int j;
	  /* Split the argument at commas.  */
	  for (j = 3; argv[i][j]; j++)
	    n_infiles += (argv[i][j] == ',');
	}
      else if (strcmp (argv[i], "-Xlinker") == 0)
	{
	  if (i + 1 == argc)
	    fatal ("argument to '-Xlinker' is missing");

	  n_infiles++;
	  i++;
	}
      else if (strcmp (argv[i], "-Xpreprocessor") == 0)
	{
	  if (i + 1 == argc)
	    fatal ("argument to '-Xpreprocessor' is missing");

	  add_preprocessor_option (argv[i+1], strlen (argv[i+1]));
	}
      else if (strcmp (argv[i], "-Xassembler") == 0)
	{
	  if (i + 1 == argc)
	    fatal ("argument to '-Xassembler' is missing");

	  add_assembler_option (argv[i+1], strlen (argv[i+1]));
	}
      else if (strcmp (argv[i], "-l") == 0)
	{
	  if (i + 1 == argc)
	    fatal ("argument to '-l' is missing");

	  n_infiles++;
	  i++;
	}
      else if (strncmp (argv[i], "-l", 2) == 0)
	n_infiles++;
#if USE_GNU_LD
     /* for now do nothing to change the passing of -z flags */
#else
     /* only do this for Solaris ld 
        Plain GCC only has position indepentent -z flags
        Solaris has position dependent -z flags such as -z extractall */
      else if (strcmp (argv[i], "-z") == 0)
	{
	  if (i + 1 == argc)
	    fatal ("argument to '-z' is missing");

	  n_infiles++;
	  i++;
	}
      else if (strncmp (argv[i], "-z", 2) == 0)
	n_infiles++;
#endif
      else if (strcmp (argv[i], "-save-temps") == 0)
	{
	  save_temps_flag = 1;
	  n_switches++;
	}
      else if (strcmp (argv[i], "-combine") == 0)
	{
	  combine_flag = 1;
	  n_switches++;
	}
      else if (strcmp (argv[i], "-specs") == 0)
	{
	  struct user_specs *user = XNEW (struct user_specs);
	  if (++i >= argc)
	    fatal ("argument to '-specs' is missing");

	  user->next = (struct user_specs *) 0;
	  user->filename = argv[i];
	  if (user_specs_tail)
	    user_specs_tail->next = user;
	  else
	    user_specs_head = user;
	  user_specs_tail = user;
	}
      else if (strncmp (argv[i], "-specs=", 7) == 0)
	{
	  struct user_specs *user = XNEW (struct user_specs);
	  if (strlen (argv[i]) == 7)
	    fatal ("argument to '-specs=' is missing");

	  user->next = (struct user_specs *) 0;
	  user->filename = argv[i] + 7;
	  if (user_specs_tail)
	    user_specs_tail->next = user;
	  else
	    user_specs_head = user;
	  user_specs_tail = user;
	}
      else if (strcmp (argv[i], "-time") == 0)
	report_times = 1;
      else if (strcmp (argv[i], "-pipe") == 0)
	{
	  /* -pipe has to go into the switches array as well as
	     setting a flag.  */
	  use_pipes = 0;  /* permanently disable -pipe */
	  n_switches++;
	}
      else if (strcmp (argv[i], "-###") == 0)
	{
	  /* This is similar to -v except that there is no execution
	     of the commands and the echoed arguments are quoted.  It
	     is intended for use in shell scripts to capture the
	     driver-generated command line.  */
	  verbose_only_flag++;
	  verbose_flag++;
	}
      else if (! strncmp (argv[i], "-Y2,", 4))
        {
          alternate_iropt = make_absolute_from_Y(argv[i]+4, "iropt");
        }
      else if (! strncmp (argv[i], "-Ya,", 4))
        {
          alternate_as = make_absolute_from_Y(argv[i]+4, "as");
        }
      else if (! strncmp (argv[i], "-Yf,", 4))
        {
          alternate_as = make_absolute_from_Y(argv[i]+4, "fbe");
        }
      else if (! strncmp (argv[i], "-Yc,", 4))
        {
          alternate_cg = make_absolute_from_Y(argv[i]+4, "cg");
        }
      else if (! strncmp (argv[i], "-Yo,", 4))
        {
          alternate_postopt = make_absolute_from_Y(argv[i]+4, "postopt");
        }
      else if (! strncmp (argv[i], "-Yb,", 4))
        {
          alternate_postopt = make_absolute_from_Y(argv[i]+4, "postopt");
        }
      else if (! strncmp (argv[i], "-YO,", 4))
        {
          alternate_ipo = make_absolute_from_Y(argv[i]+4, "ipo");
        }
      else if (! strncmp (argv[i], "-xgccdriver=", 12))
        {
          int ii;

          path_to_driver = xstrdup(argv[i]+12); 

          /* get the driver name (gcc,g++,c++) from the path */
          for (ii = strlen(path_to_driver) - 1; ii >= 0; ii--)
             if (IS_DIR_SEPARATOR (path_to_driver[ii]) ||
                 path_to_driver[ii] == '=')
               break;
          programname = xstrdup(path_to_driver+ii+1);
          if (debug_driver_val & 0x40)
             fprintf(stdout,"programname:%s\n", programname);
        
        }
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  const char *p = &argv[i][1];
	  int c = *p;

	  switch (c)
	    {
	    case 'b':
	      if (NULL == strchr(argv[i] + 2, '-'))
		goto normal_switch;

	      /* Fall through.  */
	    case 'V':
	      fatal ("'-%c' must come at the start of the command line", c);
	      break;

	    case 'B':
	      {
		const char *value;
		int len;

		if (p[1] == 0 && i + 1 == argc)
		  fatal ("argument to '-B' is missing");
		if (p[1] == 0)
		  value = argv[++i];
		else
		  value = p + 1;

		len = strlen (value);

		/* Catch the case where the user has forgotten to append a
		   directory separator to the path.  Note, they may be using
		   -B to add an executable name prefix, eg "i386-elf-", in
		   order to distinguish between multiple installations of
		   GCC in the same directory.  Hence we must check to see
		   if appending a directory separator actually makes a
		   valid directory name.  */
		if (! IS_DIR_SEPARATOR (value [len - 1])
		    && is_directory (value, false))
		  {
		    char *tmp = XNEWVEC (char, len + 2);
		    strcpy (tmp, value);
		    tmp[len] = DIR_SEPARATOR;
		    tmp[++ len] = 0;
		    value = tmp;
		  }

		add_prefix (&exec_prefixes, value, NULL,
			    PREFIX_PRIORITY_B_OPT, 0, 0);
		add_prefix (&startfile_prefixes, value, NULL,
			    PREFIX_PRIORITY_B_OPT, 0, 0);
		add_prefix (&include_prefixes, value, NULL,
			    PREFIX_PRIORITY_B_OPT, 0, 0);
		n_switches++;
	      }
	      break;

	    case 'v':	/* Print our subcommands and print versions.  */
	      n_switches++;
	      /* If they do anything other than exactly `-v', don't set
		 verbose_flag; rather, continue on to give the error.  */
	      if (p[1] != 0)
		break;
	      verbose_flag++;
	      break;

	    case 'S':
	    case 'c':
	      if (p[1] == 0)
		{
		  have_c = 1;
		  n_switches++;
		  break;
		}
	      goto normal_switch;

	    case 'o':
	      have_o = 1;
#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX)
	      if (! have_c)
		{
		  int skip;

		  /* Forward scan, just in case -S or -c is specified
		     after -o.  */
		  int j = i + 1;
		  if (p[1] == 0)
		    ++j;
		  while (j < argc)
		    {
		      if (argv[j][0] == '-')
			{
			  if (SWITCH_CURTAILS_COMPILATION (argv[j][1])
			      && argv[j][2] == 0)
			    {
			      have_c = 1;
			      break;
			    }
			  else if ((skip = SWITCH_TAKES_ARG (argv[j][1])))
			    j += skip - (argv[j][2] != 0);
			  else if ((skip = WORD_SWITCH_TAKES_ARG (argv[j] + 1)))
			    j += skip;
			}
		      j++;
		    }
		}
#endif
#if defined(HAVE_TARGET_EXECUTABLE_SUFFIX) || defined(HAVE_TARGET_OBJECT_SUFFIX)
	      if (p[1] == 0)
		argv[i + 1] = convert_filename (argv[i + 1], ! have_c, 0);
	      else
		argv[i] = convert_filename (argv[i], ! have_c, 0);
#endif
	      goto normal_switch;

	    default:
	    normal_switch:

#ifdef MODIFY_TARGET_NAME
	      is_modify_target_name = 0;

	      for (j = 0; j < ARRAY_SIZE (modify_target); j++)
		if (! strcmp (argv[i], modify_target[j].sw))
		  {
		    char *new_name = xmalloc (strlen (modify_target[j].str)
					      + strlen (spec_machine));
		    const char *p, *r;
		    char *q;
		    int made_addition = 0;

		    is_modify_target_name = 1;
		    for (p = spec_machine, q = new_name; *p != 0; )
		      {
			if (modify_target[j].add_del == DELETE
			    && (! strncmp (q, modify_target[j].str,
					   strlen (modify_target[j].str))))
			  p += strlen (modify_target[j].str);
			else if (modify_target[j].add_del == ADD
				 && ! made_addition && *p == '-')
			  {
			    for (r = modify_target[j].str; *r != 0; )
			      *q++ = *r++;
			    made_addition = 1;
			  }

			*q++ = *p++;
		      }

		    spec_machine = new_name;
		  }

	      if (is_modify_target_name)
		break;
#endif

	      n_switches++;

	      if (SWITCH_TAKES_ARG (c) > (p[1] != 0))
		i += SWITCH_TAKES_ARG (c) - (p[1] != 0);
	      else if (WORD_SWITCH_TAKES_ARG (p))
		i += WORD_SWITCH_TAKES_ARG (p);
	    }
	}
      else
	{
          /* is this an .il file or something else */
          if ( is_il_file(argv[i]) )
            {
              n_ilfiles++;
            }
          else
            {
              n_infiles++;
              lang_n_infiles++;
            }
        }
    }

  if (save_temps_flag && use_pipes)
    {
      /* -save-temps overrides -pipe, so that temp files are produced */
      if (save_temps_flag)
	error ("warning: -pipe ignored because -save-temps specified");
      use_pipes = 0;
    }

  /* Set up the search paths.  We add directories that we expect to
     contain GNU Toolchain components before directories specified by
     the machine description so that we will find GNU components (like
     the GNU assembler) before those of the host system.  */ 

  /* If we don't know where the toolchain has been installed, use the
     configured-in locations.  */
  if (!gcc_exec_prefix)
    {
#ifndef OS2
      add_prefix (&exec_prefixes, standard_libexec_prefix, "GCC",
		  PREFIX_PRIORITY_LAST, 1, 0);
      add_prefix (&exec_prefixes, standard_libexec_prefix, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 2, 0);
      add_prefix (&exec_prefixes, standard_exec_prefix, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 2, 0);
#endif
      add_prefix (&startfile_prefixes, standard_exec_prefix, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 1, 0);
    }

  /* If not cross-compiling, search well-known system locations.  */
  if (*cross_compile == '0')
    {
#ifndef OS2
      add_prefix (&exec_prefixes, standard_exec_prefix_1, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 2, 0);
      add_prefix (&exec_prefixes, standard_exec_prefix_2, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 2, 0);
#endif
      add_prefix (&startfile_prefixes, standard_exec_prefix_2, "BINUTILS",
		  PREFIX_PRIORITY_LAST, 1, 0);
    }

  gcc_assert (!IS_ABSOLUTE_PATH (tooldir_base_prefix));
  tooldir_prefix = concat (tooldir_base_prefix, spec_machine,
			   dir_separator_str, NULL);

  /* Look for tools relative to the location from which the driver is
     running, or, if that is not available, the configured prefix.  */
  tooldir_prefix
    = concat (gcc_exec_prefix ? gcc_exec_prefix : standard_exec_prefix,
	      spec_machine, dir_separator_str,
	      spec_version, dir_separator_str, tooldir_prefix, NULL);

  add_prefix (&exec_prefixes,
	      concat (tooldir_prefix, "bin", dir_separator_str, NULL),
	      "BINUTILS", PREFIX_PRIORITY_LAST, 0, 0);
  add_prefix (&startfile_prefixes,
	      concat (tooldir_prefix, "lib", dir_separator_str, NULL),
	      "BINUTILS", PREFIX_PRIORITY_LAST, 0, 1);

#if defined(TARGET_SYSTEM_ROOT_RELOCATABLE) && !defined(VMS)
  /* If the normal TARGET_SYSTEM_ROOT is inside of $exec_prefix,
     then consider it to relocate with the rest of the GCC installation
     if GCC_EXEC_PREFIX is set.
     ``make_relative_prefix'' is not compiled for VMS, so don't call it.  */
  if (target_system_root && gcc_exec_prefix)
    {
      char *tmp_prefix = make_relative_prefix (argv[0],
					       standard_bindir_prefix,
					       target_system_root);
      if (tmp_prefix && access_check (tmp_prefix, F_OK) == 0)
	{
	  target_system_root = tmp_prefix;
	  target_system_root_changed = 1;
	}
    }
#endif

  /* More prefixes are enabled in main, after we read the specs file
     and determine whether this is cross-compilation or not.  */

  /* Then create the space for the vectors and scan again.  */

  switches = XNEWVEC (struct switchstr, n_switches + 1);
  infiles = XNEWVEC (struct infile, n_infiles + 1);
  ilfiles = XNEWVEC (struct infile, n_ilfiles + 1);

  if ((debug_driver_val & 0x01)) 
     fprintf(stdout,"ALLOCATING %d members of ilfiles\n", n_ilfiles);

  n_switches = 0;
  n_infiles = 0;
  n_ilfiles = 0;
  last_language_n_infiles = -1;

  /* This, time, copy the text of each switch and store a pointer
     to the copy in the vector of switches.
     Store all the infiles in their vector.  */

  for (i = 1; i < argc; i++)
    {
      /* Just skip the switches that were handled by the preceding loop.  */
#ifdef MODIFY_TARGET_NAME
      is_modify_target_name = 0;

      for (j = 0; j < ARRAY_SIZE (modify_target); j++)
	if (! strcmp (argv[i], modify_target[j].sw))
	  is_modify_target_name = 1;

      if (is_modify_target_name)
	;
      else
#endif
      if (! strncmp (argv[i], "-Wa,", 4))
	;
      else if (! strncmp (argv[i], "-W0,", 4))
	;
      else if (! strncmp (argv[i], "-W2,", 4))
	;
      else if (! strncmp (argv[i], "-Wc,", 4))
	;      
      else if (! strncmp (argv[i], "-Wp,", 4))
	;
      else if (! strncmp (argv[i], "-Wo,", 4))
	;
      else if (! strncmp (argv[i], "-WO,", 4))
	;
      else if (! strncmp (argv[i], "-Y2,", 4))
        ;
      else if (! strncmp (argv[i], "-Ya,", 4))
        ;
      else if (! strncmp (argv[i], "-Yf,", 4))
        ;
      else if (! strncmp (argv[i], "-Yc,", 4))
        ;
      else if (! strncmp (argv[i], "-Yo,", 4))
        ;
      else if (! strncmp (argv[i], "-Yb,", 4))
        ;
      else if (! strncmp (argv[i], "-YO,", 4))
        ;
      else if (! strncmp (argv[i], "-xgccdriver=", 12))
        ;      
      else if (! strcmp (argv[i], "-pass-exit-codes"))
	;
      else if (! strcmp (argv[i], "-print-search-dirs"))
	;
      else if (! strcmp (argv[i], "-print-libgcc-file-name"))
	;
      else if (! strncmp (argv[i], "-print-file-name=", 17))
	;
      else if (! strncmp (argv[i], "-print-prog-name=", 17))
	;
      else if (! strcmp (argv[i], "-print-multi-lib"))
	;
      else if (! strcmp (argv[i], "-print-multi-directory"))
	;
      else if (! strcmp (argv[i], "-print-multi-os-directory"))
	;
      else if (! strcmp (argv[i], "-print-sysroot-headers-suffix"))
	;
      else if (! strncmp (argv[i], "--sysroot=", strlen ("--sysroot=")))
	{
	  target_system_root = argv[i] + strlen ("--sysroot=");
	  target_system_root_changed = 1;
	}
      else if (argv[i][0] == '+' && argv[i][1] == 'e')
	{
	  /* Compensate for the +e options to the C++ front-end;
	     they're there simply for cfront call-compatibility.  We do
	     some magic in default_compilers to pass them down properly.
	     Note we deliberately start at the `+' here, to avoid passing
	     -e0 or -e1 down into the linker.  */
	  switches[n_switches].part1 = &argv[i][0];
	  switches[n_switches].args = 0;
	  switches[n_switches].live_cond = 0;
	  switches[n_switches].validated = 0;
	  n_switches++;
	}
      else if (strncmp (argv[i], "-Wl,", 4) == 0)
	{
	  int prev, j;
	  /* Split the argument at commas.  */
	  prev = 4;
	  for (j = 4; argv[i][j]; j++)
	    if (argv[i][j] == ',')
	      {
		infiles[n_infiles].language = "*";
		infiles[n_infiles++].name
		  = save_string (argv[i] + prev, j - prev);
		prev = j + 1;
	      }
	  /* Record the part after the last comma.  */
	  infiles[n_infiles].language = "*";
	  infiles[n_infiles++].name = argv[i] + prev;
	}
      else if (strcmp (argv[i], "-Xlinker") == 0)
	{
	  infiles[n_infiles].language = "*";
	  infiles[n_infiles++].name = argv[++i];
	}
      /* Xassembler and Xpreprocessor were already handled in the first argv
	 scan, so all we need to do here is ignore them and their argument.  */
      else if (strcmp (argv[i], "-Xassembler") == 0)
	i++;
      else if (strcmp (argv[i], "-Xpreprocessor") == 0)
	i++;
      else if (strcmp (argv[i], "-l") == 0)
	{ /* POSIX allows separation of -l and the lib arg;
	     canonicalize by concatenating -l with its arg */
	  infiles[n_infiles].language = "*";
	  infiles[n_infiles++].name = concat ("-l", argv[++i], NULL);
	}
      else if (strncmp (argv[i], "-l", 2) == 0)
	{
	  infiles[n_infiles].language = "*";
	  infiles[n_infiles++].name = argv[i];
	}
#if USE_GNU_LD
     /* for now do nothing to change the passing of -z flags */
#else
     /* only do this for Solaris ld 
        Plain GCC only has position indepentent -z flags
        Solaris has position dependent -z flags such as -z extractall */

      else if (strcmp (argv[i], "-z") == 0)
	{ /* POSIX allows separation of -z and the lib arg;
	     canonicalize by concatenating -z with its arg */
	  infiles[n_infiles].language = "*";
	  infiles[n_infiles++].name = concat ("-z", argv[++i], NULL);
	}
      else if (strncmp (argv[i], "-z", 2) == 0)
	{
	  infiles[n_infiles].language = "*";
	  infiles[n_infiles++].name = argv[i];
	}
#endif
      else if (strcmp (argv[i], "-specs") == 0)
	i++;
      else if (strncmp (argv[i], "-specs=", 7) == 0)
	;
      else if (strcmp (argv[i], "-time") == 0)
	;
      else if (strcmp (argv[i], "-###") == 0)
	;
      else if (argv[i][0] == '-' && argv[i][1] != 0)
	{
	  const char *p = &argv[i][1];
	  int c = *p;

	  if (c == 'x')
	    {
	      if (p[1] == 0 && i + 1 == argc)
		fatal ("argument to '-x' is missing");
	      if (p[1] == 0)
		spec_lang = argv[++i];
              else if (strcmp(argv[i], "-xc++") == 0
                       || strcmp(argv[i], "-xc++-cpp-output") == 0
                       || strcmp(argv[i], "-xc++-header") == 0
                       || strcmp(argv[i], "-xassembler-with-cpp") == 0
                       || strcmp(argv[i], "-xc") == 0
                       || strcmp(argv[i], "-xnone") == 0)
                spec_lang = p + 1;
	      else if (p[1] != ' ')
                {
                  /* one of the Sun -x flags */
                  switches[n_switches].part1 = &argv[i][1];
                  switches[n_switches].args = 0;
                  switches[n_switches].live_cond = 0;
                  switches[n_switches].validated = 0;
                  n_switches++;
                  continue; /*it is one of the -x* options from Sun */
                }
	      else
		spec_lang = p + 1;
	      if (! strcmp (spec_lang, "none"))
		/* Suppress the warning if -xnone comes after the last input
		   file, because alternate command interfaces like g++ might
		   find it useful to place -xnone after each input file.  */
		spec_lang = 0;
	      else
		last_language_n_infiles = n_infiles;
	      continue;
	    }
	  switches[n_switches].part1 = p;
	  /* Deal with option arguments in separate argv elements.  */
	  if ((SWITCH_TAKES_ARG (c) > (p[1] != 0))
	      || WORD_SWITCH_TAKES_ARG (p))
	    {
	      int j = 0;
	      int n_args = WORD_SWITCH_TAKES_ARG (p);

	      if (n_args == 0)
		{
		  /* Count only the option arguments in separate argv elements.  */
		  n_args = SWITCH_TAKES_ARG (c) - (p[1] != 0);
		}
	      if (i + n_args >= argc)
		fatal ("argument to '-%s' is missing", p);
	      switches[n_switches].args
		= XNEWVEC (const char *, n_args + 1);
	      while (j < n_args)
		switches[n_switches].args[j++] = argv[++i];
	      /* Null-terminate the vector.  */
	      switches[n_switches].args[j] = 0;
	    }
	  else if (strchr (switches_need_spaces, c))
	    {
	      /* On some systems, ld cannot handle some options without
		 a space.  So split the option from its argument.  */
	      char *part1 = XNEWVEC (char, 2);
	      part1[0] = c;
	      part1[1] = '\0';

	      switches[n_switches].part1 = part1;
	      switches[n_switches].args = XNEWVEC (const char *, 2);
	      switches[n_switches].args[0] = xstrdup (p+1);
	      switches[n_switches].args[1] = 0;
	    }
	  else
	    switches[n_switches].args = 0;

	  switches[n_switches].live_cond = 0;
	  switches[n_switches].validated = 0;
	  switches[n_switches].ordering = 0;
	  /* These are always valid, since gcc.c itself understands the
	     first four and gfortranspec.c understands -static-libgfortran.  */
	  if (!strcmp (p, "save-temps")
	      || !strcmp (p, "static-libgcc")
	      || !strcmp (p, "shared-libgcc")
	      || !strcmp (p, "pipe")
	      || !strcmp (p, "static-libgfortran"))
	    switches[n_switches].validated = 1;
	  else
	    {
	      char ch = switches[n_switches].part1[0];
	      if (ch == 'B')
		switches[n_switches].validated = 1;
	    }
	  n_switches++;
	}
      else
	{
#ifdef HAVE_TARGET_OBJECT_SUFFIX
	  argv[i] = convert_filename (argv[i], 0, access (argv[i], F_OK));
#endif

	  if (strcmp (argv[i], "-") != 0 && access (argv[i], F_OK) < 0)
	    {
	      perror_with_name (argv[i]);
	      error_count++;
	    }
	  else
	    {
              /* handle .il files similarly but different as source files */
              if ( is_il_file( argv[i] ) )
                {
                   ilfiles[n_ilfiles++].name = argv[i];
                }
              else
                {
                    infiles[n_infiles].language = spec_lang;
                    infiles[n_infiles++].name = argv[i];
                }
	    }
	}
    }

  if (n_infiles == last_language_n_infiles && spec_lang != 0)
    error ("warning: '-x %s' after last input file has no effect", spec_lang);

  /* Ensure we only invoke each subprocess once.  */
  if (print_subprocess_help || print_help_list)
    {
      n_infiles = 1;

      /* Create a dummy input file, so that we can pass
	 the help option on to the various sub-processes.  */
      infiles[0].language = "c";
      infiles[0].name   = "help-dummy";
    }

  switches[n_switches].part1 = 0;
  infiles[n_infiles].name = 0;
}

/* Store switches not filtered out by %<S in spec in COLLECT_GCC_OPTIONS
   and place that in the environment.  */

static void
set_collect_gcc_options (void)
{
  int i;
  int first_time;

  /* Build COLLECT_GCC_OPTIONS to have all of the options specified to
     the compiler.  */
  obstack_grow (&collect_obstack, "COLLECT_GCC_OPTIONS=",
		sizeof ("COLLECT_GCC_OPTIONS=") - 1);

  first_time = TRUE;
  for (i = 0; (int) i < n_switches; i++)
    {
      const char *const *args;
      const char *p, *q;
      if (!first_time)
	obstack_grow (&collect_obstack, " ", 1);

      first_time = FALSE;

      /* Ignore elided switches.  */
      if ((switches[i].live_cond & SWITCH_IGNORE) != 0)
	continue;

      obstack_grow (&collect_obstack, "'-", 2);
      q = switches[i].part1;
      while ((p = strchr (q, '\'')))
	{
	  obstack_grow (&collect_obstack, q, p - q);
	  obstack_grow (&collect_obstack, "'\\''", 4);
	  q = ++p;
	}
      obstack_grow (&collect_obstack, q, strlen (q));
      obstack_grow (&collect_obstack, "'", 1);

      for (args = switches[i].args; args && *args; args++)
	{
	  obstack_grow (&collect_obstack, " '", 2);
	  q = *args;
	  while ((p = strchr (q, '\'')))
	    {
	      obstack_grow (&collect_obstack, q, p - q);
	      obstack_grow (&collect_obstack, "'\\''", 4);
	      q = ++p;
	    }
	  obstack_grow (&collect_obstack, q, strlen (q));
	  obstack_grow (&collect_obstack, "'", 1);
	}
    }
  obstack_grow (&collect_obstack, "\0", 1);
  xputenv (XOBFINISH (&collect_obstack, char *));
}

/* Process a spec string, accumulating and running commands.  */

/* These variables describe the input file name.
   input_file_number is the index on outfiles of this file,
   so that the output file name can be stored for later use by %o.
   input_basename is the start of the part of the input file
   sans all directory names, and basename_length is the number
   of characters starting there excluding the suffix .c or whatever.  */

static const char *input_filename;
static int input_file_number;
size_t input_filename_length;
static int basename_length;
static int suffixed_basename_length;
static const char *input_basename;
static const char *input_suffix;
#ifndef HOST_LACKS_INODE_NUMBERS
static struct stat input_stat;
#endif
static int input_stat_set;

/* The compiler used to process the current input file.  */
static struct compiler *input_file_compiler;

/* These are variables used within do_spec and do_spec_1.  */

/* Nonzero if an arg has been started and not yet terminated
   (with space, tab or newline).  */
static int arg_going;

/* Nonzero means %d or %g has been seen; the next arg to be terminated
   is a temporary file name.  */
static int delete_this_arg;

/* Nonzero means %w has been seen; the next arg to be terminated
   is the output file name of this compilation.  */
static int this_is_output_file;

/* Nonzero means %s has been seen; the next arg to be terminated
   is the name of a library file and we should try the standard
   search dirs for it.  */
static int this_is_library_file;

/* Nonzero means %K has been seen; the next arg to be terminated
   is the name of a executable file and we should try the standard
   search dirs for it.  */
static int this_is_executable_file;

/* Nonzero means that the input of this command is coming from a pipe.  */
static int input_from_pipe;

/* Nonnull means substitute this for any suffix when outputting a switches
   arguments.  */
static const char *suffix_subst;

/* Nonnull means add this as a suffix when outputting a switches
   arguments.  */
static const char *suffix_add;

/* If there is an argument being accumulated, terminate it and store it.  */

static void
end_going_arg (void)
{
  if (arg_going)
    {
      const char *string;

      obstack_1grow (&obstack, 0);
      string = XOBFINISH (&obstack, const char *);
      if (this_is_library_file)
	string = find_file (string);
      else if (suffix_add) {
          if (debug_driver_val & 0x40)
              fprintf(stdout,"ADD_SUFFIX_1: string=%s,suffix=%s\n",
                      string,suffix_add);
          string = concat(string,suffix_add, NULL);
          if (debug_driver_val & 0x40)
              fprintf(stdout,"ADD_SUFFIX_1: string=%s\n",string);
          suffix_add = NULL;
      }
      else if (this_is_executable_file)
	string = find_executable_file (string);
      store_arg (string, delete_this_arg, this_is_output_file);
      if (this_is_output_file)
	outfiles[input_file_number] = string;
      arg_going = 0;
    }
}

/* Process the spec SPEC and run the commands specified therein.
   Returns 0 if the spec is successfully processed; -1 if failed.  */

int
do_spec (const char *spec)
{
  int value;

  value = do_spec_2 (spec);

  /* Force out any unfinished command.
     If -pipe, this forces out the last command if it ended in `|'.  */
  if (value == 0)
    {
      if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	argbuf_index--;

      set_collect_gcc_options ();

      if (argbuf_index > 0)
	value = execute ();
    }

  return value;
}

static int
do_spec_2 (const char *spec)
{
  int result;

  clear_args ();
  arg_going = 0;
  delete_this_arg = 0;
  this_is_output_file = 0;
  this_is_library_file = 0;
  input_from_pipe = 0;
  this_is_executable_file = 0;
  suffix_subst = NULL;
  suffix_add = NULL;

  result = do_spec_1 (spec, 0, NULL);

  end_going_arg ();

  return result;
}


/* Process the given spec string and add any new options to the end
   of the switches/n_switches array.  */

static void
do_option_spec (const char *name, const char *spec)
{
  unsigned int i, value_count, value_len;
  const char *p, *q, *value;
  char *tmp_spec, *tmp_spec_p;

  if (configure_default_options[0].name == NULL)
    return;

  for (i = 0; i < ARRAY_SIZE (configure_default_options); i++)
    if (strcmp (configure_default_options[i].name, name) == 0)
      break;
  if (i == ARRAY_SIZE (configure_default_options))
    return;

  value = configure_default_options[i].value;
  value_len = strlen (value);

  /* Compute the size of the final spec.  */
  value_count = 0;
  p = spec;
  while ((p = strstr (p, "%(VALUE)")) != NULL)
    {
      p ++;
      value_count ++;
    }

  /* Replace each %(VALUE) by the specified value.  */
  tmp_spec = alloca (strlen (spec) + 1
		     + value_count * (value_len - strlen ("%(VALUE)")));
  tmp_spec_p = tmp_spec;
  q = spec;
  while ((p = strstr (q, "%(VALUE)")) != NULL)
    {
      memcpy (tmp_spec_p, q, p - q);
      tmp_spec_p = tmp_spec_p + (p - q);
      memcpy (tmp_spec_p, value, value_len);
      tmp_spec_p += value_len;
      q = p + strlen ("%(VALUE)");
    }
  strcpy (tmp_spec_p, q);

  do_self_spec (tmp_spec);
}

/* Process the given spec string and add any new options to the end
   of the switches/n_switches array.  */

static void
do_self_spec (const char *spec)
{
  do_spec_2 (spec);
  do_spec_1 (" ", 0, NULL);

  if (argbuf_index > 0)
    {
      int i, first;

      first = n_switches;
      n_switches += argbuf_index;
      switches = xrealloc (switches,
			   sizeof (struct switchstr) * (n_switches + 1));

      switches[n_switches] = switches[first];
      for (i = 0; i < argbuf_index; i++)
	{
	  struct switchstr *sw;

	  /* Each switch should start with '-'.  */
	  if (argbuf[i][0] != '-')
	    fatal ("switch '%s' does not start with '-'", argbuf[i]);

	  sw = &switches[i + first];
	  sw->part1 = &argbuf[i][1];
	  sw->args = 0;
	  sw->live_cond = 0;
	  sw->validated = 0;
	  sw->ordering = 0;
	}
    }
}

/* Callback for processing %D and %I specs.  */

struct spec_path_info {
  const char *option;
  const char *append;
  size_t append_len;
  bool omit_relative;
  bool separate_options;
};

static void *
spec_path (char *path, void *data)
{
  struct spec_path_info *info = data;
  size_t len = 0;
  char save = 0;

  /* Used on systems which record the specified -L dirs
     and use them to search for dynamic linking.  */
  /* Relative directories always come from -B,
     and it is better not to use them for searching
     at run time.  In particular, stage1 loses.  */
  if (info->omit_relative && !IS_ABSOLUTE_PATH (path))
    return NULL;

  if (info->append_len != 0)
    {
      len = strlen (path);
      memcpy (path + len, info->append, info->append_len + 1);
    }

  if (!is_directory (path, true))
    return NULL;

  do_spec_1 (info->option, 1, NULL);
  if (info->separate_options)
    do_spec_1 (" ", 0, NULL);

  if (info->append_len == 0)
    {
      len = strlen (path);
      save = path[len - 1];
      if (IS_DIR_SEPARATOR (path[len - 1]))
	path[len - 1] = '\0';
    }

  do_spec_1 (path, 1, NULL);
  do_spec_1 (" ", 0, NULL);

  /* Must not damage the original path.  */
  if (info->append_len == 0)
    path[len - 1] = save;

  return NULL;
}

/* Process the sub-spec SPEC as a portion of a larger spec.
   This is like processing a whole spec except that we do
   not initialize at the beginning and we do not supply a
   newline by default at the end.
   INSWITCH nonzero means don't process %-sequences in SPEC;
   in this case, % is treated as an ordinary character.
   This is used while substituting switches.
   INSWITCH nonzero also causes SPC not to terminate an argument.

   Value is zero unless a line was finished
   and the command on that line reported an error.  */

static int
do_spec_1 (const char *spec, int inswitch, const char *soft_matched_part)
{
  const char *p = spec;
  int c;
  int i;
  int value;

  while ((c = *p++))
    /* If substituting a switch, treat all chars like letters.
       Otherwise, NL, SPC, TAB and % are special.  */
    switch (inswitch ? 'a' : c)
      {
      case '\n':
	end_going_arg ();

	if (argbuf_index > 0 && !strcmp (argbuf[argbuf_index - 1], "|"))
	  {
	    /* A `|' before the newline means use a pipe here,
	       but only if -pipe was specified.
	       Otherwise, execute now and don't pass the `|' as an arg.  */
	    if (use_pipes)
	      {
		input_from_pipe = 1;
		break;
	      }
	    else
	      argbuf_index--;
	  }

	set_collect_gcc_options ();

	if (argbuf_index > 0)
	  {
	    value = execute ();
	    if (value)
	      return value;
	  }
	/* Reinitialize for a new command, and for a new argument.  */
	clear_args ();
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
        this_is_executable_file = 0;
	input_from_pipe = 0;
	break;

      case '|':
	end_going_arg ();

	/* Use pipe */
	obstack_1grow (&obstack, c);
	arg_going = 1;
	break;

      case '\t':
      case ' ':
        end_going_arg ();
          
	/* Reinitialize for a new argument.  */
	delete_this_arg = 0;
	this_is_output_file = 0;
	this_is_library_file = 0;
        this_is_executable_file = 0;
	break;

      case '"': /* want to have a literal string with no processing */
	/* obstack_1grow (&obstack, c);  starting double-quote */
        while ((c = *p++) != '"') /* find the matching double-quote */
	  obstack_1grow (&obstack, c);
	/* obstack_1grow (&obstack, c);  ending double-quote */
        arg_going = 1;
        break;
        
      case '%':
	switch (c = *p++)
	  {
	  case 0:
	    fatal ("spec '%s' invalid", spec);

	  case 'b':
	    obstack_grow (&obstack, input_basename, basename_length);
	    arg_going = 1;
	    break;

	  case 'B':
	    obstack_grow (&obstack, input_basename, suffixed_basename_length);
	    arg_going = 1;
	    break;

	  case 'd':
	    delete_this_arg = 2;
	    break;

	  /* Dump out the directories specified with LIBRARY_PATH,
	     followed by the absolute directories
	     that we search for startfiles.  */
	  case 'D':
	    {
	      struct spec_path_info info;

	      info.option = "-L";
	      info.append_len = 0;
#ifdef RELATIVE_PREFIX_NOT_LINKDIR
	      /* Used on systems which record the specified -L dirs
		 and use them to search for dynamic linking.
		 Relative directories always come from -B,
		 and it is better not to use them for searching
		 at run time.  In particular, stage1 loses.  */
	      info.omit_relative = true;
#else
	      info.omit_relative = false;
#endif
	      info.separate_options = false;

	      for_each_path (&startfile_prefixes, true, 0, spec_path, &info);
	    }
	    break;

          case 'q':
	    {
	      struct spec_path_info info;

	      info.option = "-I";
	      info.append_len = 0;
	      info.omit_relative = false;
	      info.separate_options = false;

	      for_each_path (&cross_include_prefixes, true, 0, spec_path, &info);
	    }
	    break;
            
	  case 'e':
	    /* %efoo means report an error with `foo' as error message
	       and don't execute any more commands for this file.  */
	    {
	      const char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n')
		p++;
	      buf = alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      error ("%s", buf);
	      return -1;
	    }
	    break;

	  case 'N':
	    /* %Noo means report a warning with `foo' on stderr.  */
	    {
	      const char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n')
		p++;
	      buf = alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      warning ("%s\n", buf);
	      if (*p)
		p++;
	    }
	    break;
            
	  case 'n':
	    /* %nfoo means report a notice with `foo' on stderr.  */
	    {
	      const char *q = p;
	      char *buf;
	      while (*p != 0 && *p != '\n')
		p++;
	      buf = alloca (p - q + 1);
	      strncpy (buf, q, p - q);
	      buf[p - q] = 0;
	      notice ("%s\n", buf);
	      if (*p)
		p++;
	    }
	    break;

	  case 'j':
	    {
	      struct stat st;

	      /* If save_temps_flag is off, and the HOST_BIT_BUCKET is
		 defined, and it is not a directory, and it is
		 writable, use it.  Otherwise, treat this like any
		 other temporary file.  */

	      if ((!save_temps_flag)
		  && (stat (HOST_BIT_BUCKET, &st) == 0) && (!S_ISDIR (st.st_mode))
		  && (access (HOST_BIT_BUCKET, W_OK) == 0))
		{
		  obstack_grow (&obstack, HOST_BIT_BUCKET,
				strlen (HOST_BIT_BUCKET));
		  delete_this_arg = 0;
		  arg_going = 1;
		  break;
		}
	    }
	    goto create_temp_file;
	  case '|':
	    if (use_pipes)
	      {
		obstack_1grow (&obstack, '-');
		delete_this_arg = 0;
		arg_going = 1;

		/* consume suffix */
		while (*p == '.' || ISALNUM ((unsigned char) *p))
		  p++;
		if (p[0] == '%' && p[1] == 'O')
		  p += 2;

		break;
	      }
	    goto create_temp_file;
	  case 'm':
	    if (use_pipes)
	      {
		/* consume suffix */
		while (*p == '.' || ISALNUM ((unsigned char) *p))
		  p++;
		if (p[0] == '%' && p[1] == 'O')
		  p += 2;

		break;
	      }
	    goto create_temp_file;
	  case 'g':
	  case 'u':
	  case 'U':
	  create_temp_file:
	      {
		struct temp_name *t;
		int suffix_length;
		const char *suffix = p;
		char *saved_suffix = NULL;

		while (*p == '.' || ISALNUM ((unsigned char) *p))
		  p++;
		suffix_length = p - suffix;
		if (p[0] == '%' && p[1] == 'O')
		  {
		    p += 2;
		    /* We don't support extra suffix characters after %O.  */
		    if (*p == '.' || ISALNUM ((unsigned char) *p))
		      fatal ("spec '%s' has invalid '%%0%c'", spec, *p);
		    if (suffix_length == 0)
		      suffix = TARGET_OBJECT_SUFFIX;
		    else
		      {
			saved_suffix
			  = XNEWVEC (char, suffix_length
				     + strlen (TARGET_OBJECT_SUFFIX));
			strncpy (saved_suffix, suffix, suffix_length);
			strcpy (saved_suffix + suffix_length,
				TARGET_OBJECT_SUFFIX);
		      }
		    suffix_length += strlen (TARGET_OBJECT_SUFFIX);
		  }

		/* If the input_filename has the same suffix specified
		   for the %g, %u, or %U, and -save-temps is specified,
		   we could end up using that file as an intermediate
		   thus clobbering the user's source file (.e.g.,
		   gcc -save-temps foo.s would clobber foo.s with the
		   output of cpp0).  So check for this condition and
		   generate a temp file as the intermediate.  */

		if (save_temps_flag)
		  {
		    char *tmp;
		    
		    temp_filename_length = basename_length + suffix_length;
		    tmp = alloca (temp_filename_length + 1);
		    strncpy (tmp, input_basename, basename_length);
		    strncpy (tmp + basename_length, suffix, suffix_length);
		    tmp[temp_filename_length] = '\0';
		    temp_filename = tmp;
		    if (strcmp (temp_filename, input_filename) != 0)
		      {
#ifndef HOST_LACKS_INODE_NUMBERS
			struct stat st_temp;

			/* Note, set_input() resets input_stat_set to 0.  */
			if (input_stat_set == 0)
			  {
			    input_stat_set = stat (input_filename, &input_stat);
			    if (input_stat_set >= 0)
			      input_stat_set = 1;
			  }

			/* If we have the stat for the input_filename
			   and we can do the stat for the temp_filename
			   then the they could still refer to the same
			   file if st_dev/st_ino's are the same.  */
			if (input_stat_set != 1
			    || stat (temp_filename, &st_temp) < 0
			    || input_stat.st_dev != st_temp.st_dev
			    || input_stat.st_ino != st_temp.st_ino)
#else
			/* Just compare canonical pathnames.  */
			char* input_realname = lrealpath (input_filename);
			char* temp_realname = lrealpath (temp_filename);
			bool files_differ = strcmp (input_realname, temp_realname);
			free (input_realname);
			free (temp_realname);
			if (files_differ)
#endif
			  {
			    temp_filename = save_string (temp_filename,
							 temp_filename_length + 1);
			    obstack_grow (&obstack, temp_filename,
						    temp_filename_length);
			    arg_going = 1;
			    delete_this_arg = 0;
			    break;
			  }
		      }
		  }

		/* See if we already have an association of %g/%u/%U and
		   suffix.  */
		for (t = temp_names; t; t = t->next)
		  if (t->length == suffix_length
		      && strncmp (t->suffix, suffix, suffix_length) == 0
		      && t->unique == (c == 'u' || c == 'U' || c == 'j'))
		    break;

		/* Make a new association if needed.  %u and %j
		   require one.  */
		if (t == 0 || c == 'u' || c == 'j')
		  {
		    if (t == 0)
		      {
			t = xmalloc (sizeof (struct temp_name));
			t->next = temp_names;
			temp_names = t;
		      }
		    t->length = suffix_length;
		    if (saved_suffix)
		      {
			t->suffix = saved_suffix;
			saved_suffix = NULL;
		      }
		    else
		      t->suffix = save_string (suffix, suffix_length);
		    t->unique = (c == 'u' || c == 'U' || c == 'j');
		    temp_filename = make_temp_file (t->suffix);
		    temp_filename_length = strlen (temp_filename);
		    t->filename = temp_filename;
		    t->filename_length = temp_filename_length;
		  }

		if (saved_suffix)
		  free (saved_suffix);

		obstack_grow (&obstack, t->filename, t->filename_length);
		delete_this_arg = 1;
	      }
	    arg_going = 1;
	    break;

	  case 'i':
	    if (combine_inputs)
	      {
		for (i = 0; (int) i < n_infiles; i++)
		  if ((!infiles[i].language) || (infiles[i].language[0] != '*'))
		    if (infiles[i].incompiler == input_file_compiler)
		      {
			store_arg (infiles[i].name, 0, 0);
			infiles[i].compiled = true;
		      }
	      }
	    else
	      {
		obstack_grow (&obstack, input_filename, input_filename_length);
		arg_going = 1;
	      }
	    break;

	  case 'I':
	    {
	      struct spec_path_info info;

	      if (multilib_dir)
		{
		  do_spec_1 ("-imultilib", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (multilib_dir, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      if (gcc_exec_prefix)
		{
		  do_spec_1 ("-iprefix", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (gcc_exec_prefix, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      if (target_system_root_changed ||
		  (target_system_root && target_sysroot_hdrs_suffix))
		{
		  do_spec_1 ("-isysroot", 1, NULL);
		  /* Make this a separate argument.  */
		  do_spec_1 (" ", 0, NULL);
		  do_spec_1 (target_system_root, 1, NULL);
		  if (target_sysroot_hdrs_suffix)
		    do_spec_1 (target_sysroot_hdrs_suffix, 1, NULL);
		  do_spec_1 (" ", 0, NULL);
		}

	      info.option = "-isystem";
	      info.append = "include";
	      info.append_len = strlen (info.append);
	      info.omit_relative = false;
	      info.separate_options = true;

	      for_each_path (&include_prefixes, false, info.append_len,
			     spec_path, &info);

	      info.append = "include-fixed";
	      if (*sysroot_hdrs_suffix_spec)
		info.append = concat (info.append, dir_separator_str,
				      multilib_dir, NULL);
	      info.append_len = strlen (info.append);
	      for_each_path (&include_prefixes, false, info.append_len,
			     spec_path, &info);
	    }
	    break;

          case 'J':
            if ((debug_driver_val & 0x02)) {
               fprintf(stdout,"GCC_EXEC_PREFIX(6)=%s\n",gcc_exec_prefix ? gcc_exec_prefix : "NULL");
               fprintf(stdout,"GCC_LIBEXEC_PREFIX(6)=%s\n", gcc_libexec_prefix ? gcc_libexec_prefix : "NULL");
               fprintf(stdout,"STANDARD_STARTFILE_PREFIX(6)=%s\n", 
                     standard_startfile_prefix ? standard_startfile_prefix : "NULL");
               fprintf(stdout,"STANDARD_EXEC_PREFIX(6)=%s\n", 
                     standard_exec_prefix ? standard_exec_prefix : "NULL");
               fprintf(stdout,"MACHINE_SUFFIX(6)=%s\n",
                     machine_suffix ? machine_suffix : "NULL");
            }
	    if (gcc_libexec_prefix) 
	      {
                char *pth = find_a_file(&startfile_prefixes, "libm.il",
					R_OK, 0); 
                if (pth) 
		  {
                    pth[strlen(pth)-7]=0;
                    do_spec_1(pth, 1, NULL);
                  }
 		else 
		  do_spec_1(studioproddir_lib, 1, NULL);
              } 
	    else 
              do_spec_1 (studioproddir_lib, 1, NULL);
            break;

          case 'H':
            if ((debug_driver_val & 0x02)) {
               fprintf(stdout,"GCC_EXEC_PREFIX(5)=%s\n",gcc_exec_prefix ? gcc_exec_prefix : "NULL");
               fprintf(stdout,"GCC_LIBEXEC_PREFIX(5)=%s\n", gcc_libexec_prefix ? gcc_libexec_prefix : "NULL");
               fprintf(stdout,"STANDARD_STARTFILE_PREFIX(5)=%s\n", 
                     standard_startfile_prefix ? standard_startfile_prefix : "NULL");
               fprintf(stdout,"STANDARD_EXEC_PREFIX(5)=%s\n", 
                     standard_exec_prefix ? standard_exec_prefix : "NULL");
               fprintf(stdout,"MACHINE_SUFFIX(5)=%s\n",
                     machine_suffix ? machine_suffix : "NULL");
            }
            /* doesn't seem that standard_startfile_prefix can ever be something
             * else than ../../..
            if (IS_ABSOLUTE_PATH(standard_startfile_prefix))
                do_spec_1 (standard_startfile_prefix, 1, NULL);
            else 
             */
            if (gcc_libexec_prefix) 
              {
                char *pth = find_a_file (&startfile_prefixes, "libstdc++.so.6.0.10",
					 R_OK, 0); 
                if (pth) 
                  {
                    pth[strlen (pth) - 19] = 0;
                    do_spec_1 (pth, 1, NULL);
                  }
                else
                  {
                    /* if no --enable-version-specific-runtime-libs given 
                       do_spec_1 (concat (gcc_libexec_prefix, "../../lib", NULL), 
                                  1, NULL);
                     */
                    do_spec_1 (concat (gcc_libexec_prefix, "../../lib/", 
                                       machine_suffix, NULL), 
                               1, NULL);
                  }
              }
            else
              { 
                /* if no --enable-version-specific-runtime-libs given 
                 * do_spec_1( standard_libdir_prefix, 1, NULL);
                */
#ifdef CROSS_DIRECTORY_STRUCTURE
                /* libstdc++.so is located in different directory between local and cross compilers. 
                   so we have to re-direct it to right path. 
                   given 
                     standard_exec_prefix : $INSTALL_DIR/lib/gcc 
                     machine_suffix : sparc-sun-solaris2.10[9]/$GCC_VERSION
                   we want the run path like:
                     $INSTALL_DIR/sparc-sun-solaris2.10[9]/lib/ 
                 */
                char *pth, *mch;
                int len;

                pth = alloca (strlen (standard_exec_prefix) + 1);
                if (pth == 0)
                  return 0; 
                strcpy (pth, standard_exec_prefix);
                pth[strlen (pth) - 8] = 0;

                mch = alloca (strlen (machine_suffix) + 1);
                if (mch == 0) 
                  return 0; 
                len = strchr (machine_suffix, '/') - machine_suffix;
                if (len <= 0) 
                  return 0; 
                strncpy (mch, machine_suffix, len);
                strcat (mch, "/lib/");

                do_spec_1 (concat (pth, mch, NULL), 1, NULL);
#else
                do_spec_1 (concat (standard_exec_prefix, machine_suffix, NULL), 1, NULL);
#endif
              }
            break;
          case 'F':
            if (gcc_libexec_prefix) 
              {
                char *pth;
                pth = find_a_file (&startfile_prefixes, "gccbuiltins.il", R_OK, 0);
                if (pth) 
                  {
                    pth[strlen (pth) - 14] = 0;
                    do_spec_1 (pth, 1, NULL);
                  }
                else
                  {
                    /* if no --enable-version-specific-runtime-libs given 
                       do_spec_1 (concat (gcc_libexec_prefix, "../../lib", NULL), 
                                  1, NULL);
                     */
                    do_spec_1 (concat (gcc_libexec_prefix, "../../lib/", 
                                       machine_suffix, NULL), 
                               1, NULL);
                  }
                
              }
            else
              { 
                /* if no --enable-version-specific-runtime-libs given 
                   do_spec_1( standard_libdir_prefix, 1, NULL);
                */
                do_spec_1 (concat (standard_exec_prefix, machine_suffix, NULL), 1, NULL);
              }
            break;
                 
	  case 'o':
	    {
	      int max = n_infiles;
	      max += lang_specific_extra_outfiles;

              if (HAVE_GNU_LD && at_file_supplied)
                {
                  /* We are going to expand `%o' to `@FILE', where FILE
                     is a newly-created temporary filename.  The filenames
                     that would usually be expanded in place of %o will be
                     written to the temporary file.  */

                  char *temp_file = make_temp_file ("");
                  char *at_argument;
                  char **argv;
                  int n_files, j, status;
                  FILE *f;

                  at_argument = concat ("@", temp_file, NULL);
                  store_arg (at_argument, 0, 0);

                  /* Convert OUTFILES into a form suitable for writeargv.  */

                  /* Determine how many are non-NULL.  */
                  for (n_files = 0, i = 0; i < max; i++)
                    n_files += outfiles[i] != NULL;

                  argv = alloca (sizeof (char *) * (n_files + 1));

                  /* Copy the strings over.  */
                  for (i = 0, j = 0; i < max; i++)
                    if (outfiles[i])
                      {
                        argv[j] = CONST_CAST (char *, outfiles[i]);
                        j++;
                      }
                  argv[j] = NULL;

                  f = fopen (temp_file, "w");

                  if (f == NULL)
                    fatal ("could not open temporary response file %s",
                           temp_file);

                  status = writeargv (argv, f);

                  if (status)
                    fatal ("could not write to temporary response file %s",
                           temp_file);

                  status = fclose (f);

                  if (EOF == status)
                    fatal ("could not close temporary response file %s",
                           temp_file);

                  record_temp_file (temp_file, !save_temps_flag, !save_temps_flag);
                }
              else
                {
                  if ((debug_driver_val & 0x01))
                    for (i=0; i < max; i++)
                      if (outfiles[i])
                        fprintf(stdout,"OUTFILE[%d]=%s\n",i,outfiles[i]);
                  
                  for (i = 0; i < max; i++)
                    if (outfiles[i])
                      store_arg (outfiles[i], 0, 0);
                }
	      break;
	    }

	  case 'O':
	    obstack_grow (&obstack, TARGET_OBJECT_SUFFIX, strlen (TARGET_OBJECT_SUFFIX));
	    arg_going = 1;
	    break;
            
	  /* Dump out the options accumulated previously using -WO,.  */
	  case 'P':
	    for (i = 0; i < n_ipo_options; i++)
	      {
		do_spec_1 (ipo_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	  /* Dump out the options accumulated previously using -W2,.  */
	  case 'M':
	    for (i = 0; i < n_postopt_options; i++)
	      {
		do_spec_1 (postopt_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	  case 'Q':
	    for (i = 0; i < n_iropt_options; i++)
	      {
		do_spec_1 (iropt_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	  case 'r':
	    for (i = 0; i < n_frontend_options; i++)
	      {
		do_spec_1 (frontend_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	  /* Dump out the options accumulated previously using -Wc,.  */
	  case 'T':
	    for (i = 0; i < n_sscg_options; i++)
	      {
		do_spec_1 (sscg_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;
            
	  case 's':
	    this_is_library_file = 1;
	    break;

 	  case 'K':
	    this_is_executable_file = 1;
	    break;
            
	  case 'V':
	    outfiles[input_file_number] = NULL;
	    break;

	  case 'w':
	    this_is_output_file = 1;
	    break;

	  case 'W':
	    {
	      int cur_index = argbuf_index;
	      /* Handle the {...} following the %W.  */
	      if (*p != '{')
		fatal ("spec '%s' has invalid '%%W%c", spec, *p);
	      p = handle_braces (p + 1);
	      if (p == 0)
		return -1;
	      end_going_arg ();
	      /* If any args were output, mark the last one for deletion
		 on failure.  */
	      if (argbuf_index != cur_index)
		record_temp_file (argbuf[argbuf_index - 1], 0, 1);
	      break;
	    }

	  /* %x{OPTION} records OPTION for %X to output.  */
	  case 'x':
	    {
	      const char *p1 = p;
	      char *string;

	      /* Skip past the option value and make a copy.  */
	      if (*p != '{')
		fatal ("spec '%s' has invalid '%%x%c'", spec, *p);
	      while (*p++ != '}')
		;
	      string = save_string (p1 + 1, p - p1 - 2);

	      /* See if we already recorded this option.  */
	      for (i = 0; i < n_linker_options; i++)
		if (! strcmp (string, linker_options[i]))
		  {
		    free (string);
		    return 0;
		  }

	      /* This option is new; add it.  */
	      add_linker_option (string, strlen (string));
	    }
	    break;

	  /* Dump out the options accumulated previously using %x.  */
	  case 'X':
	    for (i = 0; i < n_linker_options; i++)
	      {
		do_spec_1 (linker_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	  /* Dump out the options accumulated previously using -Wa,.  */
	  case 'Y':
	    for (i = 0; i < n_assembler_options; i++)
	      {
		do_spec_1 (assembler_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	  /* Dump out the options accumulated previously using -Wp,.  */
	  case 'Z':
	    for (i = 0; i < n_preprocessor_options; i++)
	      {
		do_spec_1 (preprocessor_options[i], 1, NULL);
		/* Make each accumulated option a separate argument.  */
		do_spec_1 (" ", 0, NULL);
	      }
	    break;

	    /* Here are digits and numbers that just process
	       a certain constant string as a spec.  */

	  case '1':
	    value = do_spec_1 (cc1_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case '2':
	    value = do_spec_1 (cc1plus_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'a':
	    value = do_spec_1 (asm_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'A':
	    value = do_spec_1 (asm_final_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'C':
	    {
	      const char *const spec
		= (input_file_compiler->cpp_spec
		   ? input_file_compiler->cpp_spec
		   : cpp_spec);
	      value = do_spec_1 (spec, 0, NULL);
	      if (value != 0)
		return value;
	    }
	    break;

	  case 'E':
	    value = do_spec_1 (endfile_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'l':
	    value = do_spec_1 (link_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'L':
	    value = do_spec_1 (lib_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'G':
	    value = do_spec_1 (libgcc_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	  case 'R':
	    /* We assume there is a directory
	       separator at the end of this string.  */
	    if (target_system_root)
	      {
	        obstack_grow (&obstack, target_system_root,
			      strlen (target_system_root));
		if (target_sysroot_suffix)
		  obstack_grow (&obstack, target_sysroot_suffix,
				strlen (target_sysroot_suffix));
	      }
	    break;

	  case 'S':
	    value = do_spec_1 (startfile_spec, 0, NULL);
	    if (value != 0)
	      return value;
	    break;

	    /* Here we define characters other than letters and digits.  */

	  case '{':
	    p = handle_braces (p);
	    if (p == 0)
	      return -1;
	    break;

	  case ':':
	    p = handle_spec_function (p);
	    if (p == 0)
	      return -1;
	    break;

	  case '%':
	    obstack_1grow (&obstack, '%');
	    break;

	  case '.':
	    {
	      unsigned len = 0;

	      while (p[len] && p[len] != ' ' && p[len] != '%')
		len++;
	      suffix_subst = save_string (p - 1, len + 1);
	      p += len;
	    }
	   break;

	  case '+':
	    {
	      unsigned len = 0;

	      while (p[len] && p[len] != ' ' && p[len] != '%')
		len++;
              /* replace + by . */
	      suffix_add = concat(".",save_string (p, len), NULL);
              if (debug_driver_val & 0x40)
                 fprintf(stdout,"ADD_SUFFIX_2: suffix_add=%s\n", suffix_add);
	      p += len;
	    }
	   break;
           
	   /* Henceforth ignore the option(s) matching the pattern
	      after the %<.  */
	  case '<':
	    {
	      unsigned len = 0;
	      int have_wildcard = 0;
	      int i;

	      while (p[len] && p[len] != ' ' && p[len] != '\t')
		len++;

	      if (p[len-1] == '*')
		have_wildcard = 1;

	      for (i = 0; i < n_switches; i++)
		if (!strncmp (switches[i].part1, p, len - have_wildcard)
		    && (have_wildcard || switches[i].part1[len] == '\0'))
		  {
		    switches[i].live_cond |= SWITCH_IGNORE;
		    switches[i].validated = 1;
		  }

	      p += len;
	    }
	    break;

	  case '*':
	    if (soft_matched_part)
	      {
		do_spec_1 (soft_matched_part, 1, NULL);
		do_spec_1 (" ", 0, NULL);
	      }
	    else
	      /* Catch the case where a spec string contains something like
		 '%{foo:%*}'.  i.e. there is no * in the pattern on the left
		 hand side of the :.  */
	      error ("spec failure: '%%*' has not been initialized by pattern match");
	    break;

	    /* Process a string found as the value of a spec given by name.
	       This feature allows individual machine descriptions
	       to add and use their own specs.
	       %[...] modifies -D options the way %P does;
	       %(...) uses the spec unmodified.  */
	  case '[':
	    error ("warning: use of obsolete %%[ operator in specs");
	  case '(':
	    {
	      const char *name = p;
	      struct spec_list *sl;
	      int len;

	      /* The string after the S/P is the name of a spec that is to be
		 processed.  */
	      while (*p && *p != ')' && *p != ']')
		p++;

	      /* See if it's in the list.  */
	      for (len = p - name, sl = specs; sl; sl = sl->next)
		if (sl->name_len == len && !strncmp (sl->name, name, len))
		  {
		    name = *(sl->ptr_spec);
#ifdef DEBUG_SPECS
		    notice ("Processing spec %c%s%c, which is '%s'\n",
			    c, sl->name, (c == '(') ? ')' : ']', name);
#endif
		    break;
		  }

	      if (sl)
		{
		  if (c == '(')
		    {
		      value = do_spec_1 (name, 0, NULL);
		      if (value != 0)
			return value;
		    }
		  else
		    {
		      char *x = alloca (strlen (name) * 2 + 1);
		      char *buf = x;
		      const char *y = name;
		      int flag = 0;

		      /* Copy all of NAME into BUF, but put __ after
			 every -D and at the end of each arg.  */
		      while (1)
			{
			  if (! strncmp (y, "-D", 2))
			    {
			      *x++ = '-';
			      *x++ = 'D';
			      *x++ = '_';
			      *x++ = '_';
			      y += 2;
			      flag = 1;
			      continue;
			    }
			  else if (flag
				   && (*y == ' ' || *y == '\t' || *y == '='
				       || *y == '}' || *y == 0))
			    {
			      *x++ = '_';
			      *x++ = '_';
			      flag = 0;
			    }
			  if (*y == 0)
			    break;
			  else
			    *x++ = *y++;
			}
		      *x = 0;

		      value = do_spec_1 (buf, 0, NULL);
		      if (value != 0)
			return value;
		    }
		}

	      /* Discard the closing paren or bracket.  */
	      if (*p)
		p++;
	    }
	    break;

	  default:
	    error ("spec failure: unrecognized spec option '%c'", c);
	    break;
	  }
	break;

      case '\\':
	/* Backslash: treat next character as ordinary.  */
	c = *p++;

	/* Fall through.  */
      default:
	/* Ordinary character: put it into the current argument.  */
	obstack_1grow (&obstack, c);
	arg_going = 1;
      }

  /* End of string.  If we are processing a spec function, we need to
     end any pending argument.  */
  if (processing_spec_function)
    end_going_arg ();

  return 0;
}

/* Look up a spec function.  */

static const struct spec_function *
lookup_spec_function (const char *name)
{
  const struct spec_function *sf;

  for (sf = static_spec_functions; sf->name != NULL; sf++)
    if (strcmp (sf->name, name) == 0)
      return sf;

  return NULL;
}

/* Evaluate a spec function.  */

static const char *
eval_spec_function (const char *func, const char *args)
{
  const struct spec_function *sf;
  const char *funcval;

  /* Saved spec processing context.  */
  int save_argbuf_index;
  int save_argbuf_length;
  const char **save_argbuf;

  int save_arg_going;
  int save_delete_this_arg;
  int save_this_is_output_file;
  int save_this_is_library_file;
  int save_input_from_pipe;
  int save_this_is_executable_file;
  const char *save_suffix_subst;
  const char *save_suffix_add;

  sf = lookup_spec_function (func);
  if (sf == NULL)
    fatal ("unknown spec function '%s'", func);

  /* Push the spec processing context.  */
  save_argbuf_index = argbuf_index;
  save_argbuf_length = argbuf_length;
  save_argbuf = argbuf;

  save_arg_going = arg_going;
  save_delete_this_arg = delete_this_arg;
  save_this_is_output_file = this_is_output_file;
  save_this_is_library_file = this_is_library_file;
  save_input_from_pipe = input_from_pipe;
  save_suffix_subst = suffix_subst;
  save_this_is_executable_file = this_is_executable_file;
  save_suffix_add = suffix_add;

  /* Create a new spec processing context, and build the function
     arguments.  */

  alloc_args ();
  if (do_spec_2 (args) < 0)
    fatal ("error in args to spec function '%s'", func);

  /* argbuf_index is an index for the next argument to be inserted, and
     so contains the count of the args already inserted.  */

  funcval = (*sf->func) (argbuf_index, argbuf);

  /* Pop the spec processing context.  */
  argbuf_index = save_argbuf_index;
  argbuf_length = save_argbuf_length;
  free (argbuf);
  argbuf = save_argbuf;

  arg_going = save_arg_going;
  delete_this_arg = save_delete_this_arg;
  this_is_output_file = save_this_is_output_file;
  this_is_library_file = save_this_is_library_file;
  input_from_pipe = save_input_from_pipe;
  suffix_subst = save_suffix_subst;
  this_is_executable_file = save_this_is_executable_file;
  suffix_add = save_suffix_add;
  
  return funcval;
}

/* Handle a spec function call of the form:

   %:function(args)

   ARGS is processed as a spec in a separate context and split into an
   argument vector in the normal fashion.  The function returns a string
   containing a spec which we then process in the caller's context, or
   NULL if no processing is required.  */

static const char *
handle_spec_function (const char *p)
{
  char *func, *args;
  const char *endp, *funcval;
  int count;

  processing_spec_function++;

  /* Get the function name.  */
  for (endp = p; *endp != '\0'; endp++)
    {
      if (*endp == '(')		/* ) */
        break;
      /* Only allow [A-Za-z0-9], -, and _ in function names.  */
      if (!ISALNUM (*endp) && !(*endp == '-' || *endp == '_'))
	fatal ("malformed spec function name");
    }
  if (*endp != '(')		/* ) */
    fatal ("no arguments for spec function");
  func = save_string (p, endp - p);
  p = ++endp;

  /* Get the arguments.  */
  for (count = 0; *endp != '\0'; endp++)
    {
      /* ( */
      if (*endp == ')')
	{
	  if (count == 0)
	    break;
	  count--;
	}
      else if (*endp == '(')	/* ) */
	count++;
    }
  /* ( */
  if (*endp != ')')
    fatal ("malformed spec function arguments");
  args = save_string (p, endp - p);
  p = ++endp;

  /* p now points to just past the end of the spec function expression.  */

  funcval = eval_spec_function (func, args);
  if (funcval != NULL && do_spec_1 (funcval, 0, NULL) < 0)
    p = NULL;

  free (func);
  free (args);

  processing_spec_function--;

  return p;
}

/* Inline subroutine of handle_braces.  Returns true if the current
   input suffix matches the atom bracketed by ATOM and END_ATOM.  */
static inline bool
input_suffix_matches (const char *atom, const char *end_atom)
{
  return (input_suffix
	  && !strncmp (input_suffix, atom, end_atom - atom)
	  && input_suffix[end_atom - atom] == '\0');
}

/* Subroutine of handle_braces.  Returns true if the current
   input file's spec name matches the atom bracketed by ATOM and END_ATOM.  */
static bool
input_spec_matches (const char *atom, const char *end_atom)
{
  return (input_file_compiler
	  && input_file_compiler->suffix
	  && input_file_compiler->suffix[0] != '\0'
	  && !strncmp (input_file_compiler->suffix + 1, atom,
		       end_atom - atom)
	  && input_file_compiler->suffix[end_atom - atom + 1] == '\0');
}

/* Subroutine of handle_braces.  Returns true if a switch
   matching the atom bracketed by ATOM and END_ATOM appeared on the
   command line.  */
static bool
switch_matches (const char *atom, const char *end_atom, int starred)
{
  int i;
  int len = end_atom - atom;
  int plen = starred ? len : -1;

  for (i = 0; i < n_switches; i++)
    if (!strncmp (switches[i].part1, atom, len)
	&& (starred || switches[i].part1[len] == '\0')
	&& check_live_switch (i, plen))
      return true;

  return false;
}

/* Inline subroutine of handle_braces.  Mark all of the switches which
   match ATOM (extends to END_ATOM; STARRED indicates whether there
   was a star after the atom) for later processing.  */
static inline void
mark_matching_switches (const char *atom, const char *end_atom, int starred)
{
  int i;
  int len = end_atom - atom;
  int plen = starred ? len : -1;

  for (i = 0; i < n_switches; i++)
    if (!strncmp (switches[i].part1, atom, len)
	&& (starred || switches[i].part1[len] == '\0')
	&& check_live_switch (i, plen))
      switches[i].ordering = 1;
}

/* Inline subroutine of handle_braces.  Process all the currently
   marked switches through give_switch, and clear the marks.  */
static inline void
process_marked_switches (void)
{
  int i;

  for (i = 0; i < n_switches; i++)
    if (switches[i].ordering == 1)
      {
	switches[i].ordering = 0;
	give_switch (i, 0);
      }
}

/* Handle a %{ ... } construct.  P points just inside the leading {.
   Returns a pointer one past the end of the brace block, or 0
   if we call do_spec_1 and that returns -1.  */

static const char *
handle_braces (const char *p)
{
  const char *atom, *end_atom;
  const char *d_atom = NULL, *d_end_atom = NULL;
  const char *orig = p;

  bool a_is_suffix;
  bool a_is_spectype;
  bool a_is_starred;
  bool a_is_negated;
  bool a_matched;

  bool a_must_be_last = false;
  bool ordered_set    = false;
  bool disjunct_set   = false;
  bool disj_matched   = false;
  bool disj_starred   = true;
  bool n_way_choice   = false;
  bool n_way_matched  = false;

#define SKIP_WHITE() do { while (*p == ' ' || *p == '\t') p++; } while (0)

  do
    {
      if (a_must_be_last)
	goto invalid;

      /* Scan one "atom" (S in the description above of %{}, possibly
	 with '!', '.', '@', ',', or '*' modifiers).  */
      a_matched = false;
      a_is_suffix = false;
      a_is_starred = false;
      a_is_negated = false;
      a_is_spectype = false;

      SKIP_WHITE();
      if (*p == '!')
	p++, a_is_negated = true;

      SKIP_WHITE();
      if (*p == '.')
	p++, a_is_suffix = true;
      else if (*p == ',')
	p++, a_is_spectype = true;

      atom = p;
      while (ISIDNUM(*p) || *p == '-' || *p == '+' || *p == '='
	     || *p == ',' || *p == '.' || *p == '@')
	p++;
      end_atom = p;

      if (*p == '*')
	p++, a_is_starred = 1;

      SKIP_WHITE();
      switch (*p)
	{
	case '&': case '}':
	  /* Substitute the switch(es) indicated by the current atom.  */
	  ordered_set = true;
	  if (disjunct_set || n_way_choice || a_is_negated || a_is_suffix
	      || a_is_spectype || atom == end_atom)
	    goto invalid;

	  mark_matching_switches (atom, end_atom, a_is_starred);

	  if (*p == '}')
	    process_marked_switches ();
	  break;

	case '|': case ':':
	  /* Substitute some text if the current atom appears as a switch
	     or suffix.  */
	  disjunct_set = true;
	  if (ordered_set)
	    goto invalid;

	  if (atom == end_atom)
	    {
	      if (!n_way_choice || disj_matched || *p == '|'
		  || a_is_negated || a_is_suffix || a_is_spectype 
		  || a_is_starred)
		goto invalid;

	      /* An empty term may appear as the last choice of an
		 N-way choice set; it means "otherwise".  */
	      a_must_be_last = true;
	      disj_matched = !n_way_matched;
	      disj_starred = false;
	    }
	  else
	    {
	      if ((a_is_suffix || a_is_spectype) && a_is_starred)
		goto invalid;
	      
	      if (!a_is_starred)
		disj_starred = false;

	      /* Don't bother testing this atom if we already have a
		 match.  */
	      if (!disj_matched && !n_way_matched)
		{
		  if (a_is_suffix)
		    a_matched = input_suffix_matches (atom, end_atom);
		  else if (a_is_spectype)
		    a_matched = input_spec_matches (atom, end_atom);
		  else
		    a_matched = switch_matches (atom, end_atom, a_is_starred);
		  
		  if (a_matched != a_is_negated)
		    {
		      disj_matched = true;
		      d_atom = atom;
		      d_end_atom = end_atom;
		    }
		}
	    }

	  if (*p == ':')
	    {
	      /* Found the body, that is, the text to substitute if the
		 current disjunction matches.  */
	      p = process_brace_body (p + 1, d_atom, d_end_atom, disj_starred,
				      disj_matched && !n_way_matched);
	      if (p == 0)
		return 0;

	      /* If we have an N-way choice, reset state for the next
		 disjunction.  */
	      if (*p == ';')
		{
		  n_way_choice = true;
		  n_way_matched |= disj_matched;
		  disj_matched = false;
		  disj_starred = true;
		  d_atom = d_end_atom = NULL;
		}
	    }
	  break;

	default:
	  goto invalid;
	}
    }
  while (*p++ != '}');

  return p;

 invalid:
  fatal ("braced spec '%s' is invalid at '%c'", orig, *p);

#undef SKIP_WHITE
}

/* Subroutine of handle_braces.  Scan and process a brace substitution body
   (X in the description of %{} syntax).  P points one past the colon;
   ATOM and END_ATOM bracket the first atom which was found to be true
   (present) in the current disjunction; STARRED indicates whether all
   the atoms in the current disjunction were starred (for syntax validation);
   MATCHED indicates whether the disjunction matched or not, and therefore
   whether or not the body is to be processed through do_spec_1 or just
   skipped.  Returns a pointer to the closing } or ;, or 0 if do_spec_1
   returns -1.  */

static const char *
process_brace_body (const char *p, const char *atom, const char *end_atom,
		    int starred, int matched)
{
  const char *body, *end_body;
  unsigned int nesting_level;
  bool have_subst     = false;

  /* Locate the closing } or ;, honoring nested braces.
     Trim trailing whitespace.  */
  body = p;
  nesting_level = 1;
  for (;;)
    {
      if (*p == '{')
	nesting_level++;
      else if (*p == '}')
	{
	  if (!--nesting_level)
	    break;
	}
      else if (*p == ';' && nesting_level == 1)
	break;
      else if (*p == '%' && p[1] == '*' && nesting_level == 1)
	have_subst = true;
      else if (*p == '\0')
	goto invalid;
      p++;
    }

  end_body = p;
  while (end_body[-1] == ' ' || end_body[-1] == '\t')
    end_body--;

  if (have_subst && !starred)
    goto invalid;

  if (matched)
    {
      /* Copy the substitution body to permanent storage and execute it.
	 If have_subst is false, this is a simple matter of running the
	 body through do_spec_1...  */
      char *string = save_string (body, end_body - body);
      if (!have_subst)
	{
	  if (do_spec_1 (string, 0, NULL) < 0)
	    return 0;
	}
      else
	{
	  /* ... but if have_subst is true, we have to process the
	     body once for each matching switch, with %* set to the
	     variant part of the switch.  */
	  unsigned int hard_match_len = end_atom - atom;
	  int i;

	  for (i = 0; i < n_switches; i++)
	    if (!strncmp (switches[i].part1, atom, hard_match_len)
		&& check_live_switch (i, hard_match_len))
	      {
		if (do_spec_1 (string, 0,
			       &switches[i].part1[hard_match_len]) < 0)
		  return 0;
		/* Pass any arguments this switch has.  */
		give_switch (i, 1);
		suffix_subst = NULL;
	      }
	}
    }

  return p;

 invalid:
  fatal ("braced spec body '%s' is invalid", body);
}

/* Return 0 iff switch number SWITCHNUM is obsoleted by a later switch
   on the command line.  PREFIX_LENGTH is the length of XXX in an {XXX*}
   spec, or -1 if either exact match or %* is used.

   A -O switch is obsoleted by a later -O switch.  A -f, -m, or -W switch
   whose value does not begin with "no-" is obsoleted by the same value
   with the "no-", similarly for a switch with the "no-" prefix.  */

static int
check_live_switch (int switchnum, int prefix_length)
{
  const char *name = switches[switchnum].part1;
  int i;

  /* In the common case of {<at-most-one-letter>*}, a negating
     switch would always match, so ignore that case.  We will just
     send the conflicting switches to the compiler phase.  */
  if (prefix_length >= 0 && prefix_length <= 1)
    return 1;

  /* If we already processed this switch and determined if it was
     live or not, return our past determination.  */
  if (switches[switchnum].live_cond != 0)
    return ((switches[switchnum].live_cond & SWITCH_LIVE) != 0
	    && (switches[switchnum].live_cond & SWITCH_FALSE) == 0);

  /* Now search for duplicate in a manner that depends on the name.  */
  switch (*name)
    {
    case 'O':
      for (i = switchnum + 1; i < n_switches; i++)
	if (switches[i].part1[0] == 'O')
	  {
	    switches[switchnum].validated = 1;
	    switches[switchnum].live_cond = SWITCH_FALSE;
	    return 0;
	  }
      break;

    case 'W':  case 'f':  case 'm':
      if (! strncmp (name + 1, "no-", 3))
	{
	  /* We have Xno-YYY, search for XYYY.  */
	  for (i = switchnum + 1; i < n_switches; i++)
	    if (switches[i].part1[0] == name[0]
		&& ! strcmp (&switches[i].part1[1], &name[4]))
	      {
		switches[switchnum].validated = 1;
		switches[switchnum].live_cond = SWITCH_FALSE;
		return 0;
	      }
	}
      else
	{
	  /* We have XYYY, search for Xno-YYY.  */
	  for (i = switchnum + 1; i < n_switches; i++)
	    if (switches[i].part1[0] == name[0]
		&& switches[i].part1[1] == 'n'
		&& switches[i].part1[2] == 'o'
		&& switches[i].part1[3] == '-'
		&& !strcmp (&switches[i].part1[4], &name[1]))
	      {
		switches[switchnum].validated = 1;
		switches[switchnum].live_cond = SWITCH_FALSE;
		return 0;
	      }
	}
      break;
    }

  /* Otherwise the switch is live.  */
  switches[switchnum].live_cond |= SWITCH_LIVE;
  return 1;
}

/* Pass a switch to the current accumulating command
   in the same form that we received it.
   SWITCHNUM identifies the switch; it is an index into
   the vector of switches gcc received, which is `switches'.
   This cannot fail since it never finishes a command line.

   If OMIT_FIRST_WORD is nonzero, then we omit .part1 of the argument.  */

static void
give_switch (int switchnum, int omit_first_word)
{
  if ((switches[switchnum].live_cond & SWITCH_IGNORE) != 0)
    return;

  if (!omit_first_word)
    {
      do_spec_1 ("-", 0, NULL);
      do_spec_1 (switches[switchnum].part1, 1, NULL);
    }

  if (switches[switchnum].args != 0)
    {
      const char **p;
      for (p = switches[switchnum].args; *p; p++)
	{
	  const char *arg = *p;

	  do_spec_1 (" ", 0, NULL);
	  if (suffix_subst)
	    {
	      unsigned length = strlen (arg);
	      int dot = 0;

	      while (length-- && !IS_DIR_SEPARATOR (arg[length]))
		if (arg[length] == '.')
		  {
		    (CONST_CAST(char *, arg))[length] = 0;
		    dot = 1;
		    break;
		  }
	      do_spec_1 (arg, 1, NULL);
	      if (dot)
		(CONST_CAST(char *, arg))[length] = '.';
	      do_spec_1 (suffix_subst, 1, NULL);
	    }
          else if (suffix_add)
	    {
              if(debug_driver_val & 0x40)
                fprintf(stdout,"ADD_SUFFIX_3\n");
	      do_spec_1 (concat(arg,suffix_add, NULL), 1, NULL);
	    }
	  else
	    do_spec_1 (arg, 1, NULL);
	}
    }

  do_spec_1 (" ", 0, NULL);
  switches[switchnum].validated = 1;
}

/* Search for a file named NAME trying various prefixes including the
   user's -B prefix and some standard ones.
   Return the absolute file name found.  If nothing is found, return NAME.  */

static const char *
find_file (const char *name)
{
  char *newname = find_a_file (&startfile_prefixes, name, R_OK, true);
  if (newname == NULL)
    newname = find_a_file(&exec_prefixes, name, R_OK, 0);
  
  return newname ? newname : name;
}

/* similar to find_file but for the executables for the iropt, etc */

static const char *
find_executable_file (const char *name)
{
  if (!strcmp(name, "iropt") && alternate_iropt)
    return xstrdup (alternate_iropt);

  if (!strcmp(name, "cg") && alternate_cg)
    return xstrdup (alternate_cg);

  if (!strcmp(name, "ipo") && alternate_ipo)
    return xstrdup (alternate_ipo);

  if (!strcmp(name, "postopt") && alternate_postopt)
    return xstrdup (alternate_postopt);

  if (!strcmp(name, "as") && alternate_as)
    return xstrdup (alternate_as);

  if (!strcmp(name, "fbe") && alternate_as)
    return xstrdup (alternate_as);

  return find_a_file(&exec_prefixes, name, X_OK, 0);

}  

/* Determine whether a directory exists.  If LINKER, return 0 for
   certain fixed names not needed by the linker.  */

static int
is_directory (const char *path1, bool linker)
{
  int len1;
  char *path;
  char *cp;
  struct stat st;

  /* Ensure the string ends with "/.".  The resulting path will be a
     directory even if the given path is a symbolic link.  */
  len1 = strlen (path1);
  path = alloca (3 + len1);
  memcpy (path, path1, len1);
  cp = path + len1;
  if (!IS_DIR_SEPARATOR (cp[-1]))
    *cp++ = DIR_SEPARATOR;
  *cp++ = '.';
  *cp = '\0';

  /* Exclude directories that the linker is known to search.  */
  if (linker
      && IS_DIR_SEPARATOR (path[0])
      && ((cp - path == 6
	   && strncmp (path + 1, "lib", 3) == 0)
	  || (cp - path == 10
	      && strncmp (path + 1, "usr", 3) == 0
	      && IS_DIR_SEPARATOR (path[4])
	      && strncmp (path + 5, "lib", 3) == 0)))
    return 0;

  return (stat (path, &st) >= 0 && S_ISDIR (st.st_mode));
}

/* Set up the various global variables to indicate that we're processing
   the input file named FILENAME.  */

void
set_input (const char *filename)
{
  const char *p;

  input_filename = filename;
  input_filename_length = strlen (input_filename);

  if (filename)
    /* don't change input_basename when input file is an option like -lm */
    if (filename[0] == '-') return; 
  
  input_basename = input_filename;
#ifdef HAVE_DOS_BASED_FILE_SYSTEM
  /* Skip drive name so 'x:foo' is handled properly.  */
  if (input_basename[1] == ':')
    input_basename += 2;
#endif
  for (p = input_basename; *p; p++)
    if (IS_DIR_SEPARATOR (*p))
      input_basename = p + 1;

  /* Find a suffix starting with the last period,
     and set basename_length to exclude that suffix.  */
  basename_length = strlen (input_basename);
  suffixed_basename_length = basename_length;
  p = input_basename + basename_length;
  while (p != input_basename && *p != '.')
    --p;
  if (*p == '.' && p != input_basename)
    {
      basename_length = p - input_basename;
      input_suffix = p + 1;
    }
  else
    input_suffix = "";

  /* If a spec for 'g', 'u', or 'U' is seen with -save-temps then
     we will need to do a stat on the input_filename.  The
     INPUT_STAT_SET signals that the stat is needed.  */
  input_stat_set = 0;

  if (debug_driver_val & 0x08)
    fprintf(stdout,"INPUT_BASENAME=%s\n",
            input_basename ? input_basename : "NULL");
}

/* On fatal signals, delete all the temporary files.  */

static void
fatal_error (int signum)
{
  signal (signum, SIG_DFL);
  delete_failure_queue ();
  delete_temp_files ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

extern int main (int, char **);

int
main (int argc, char **argv)
{
  size_t i;
  int value;
  int linker_was_run = 0;
  int lang_n_infiles = 0;
  int num_linker_inputs = 0;
  char *explicit_link_files;
  char *specs_file;
  const char *p;
  struct user_specs *uptr;
  char **old_argv = argv;

  /* Initialize here, not in definition.  The IRIX 6 O32 cc sometimes chokes
     on ?: in file-scope variable initializations.  */
  asm_debug = ASM_DEBUG_SPEC;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && !IS_DIR_SEPARATOR (p[-1]))
    --p;
  programname = p;

  xmalloc_set_program_name (programname);

  expandargv (&argc, &argv);

  /* Determine if any expansions were made.  */
  if (argv != old_argv)
    at_file_supplied = true;

  prune_options (&argc, &argv);

#ifdef GCC_DRIVER_HOST_INITIALIZATION
  /* Perform host dependent initialization when needed.  */
  GCC_DRIVER_HOST_INITIALIZATION;
#endif

  /* Unlock the stdio streams.  */
  unlock_std_streams ();

  gcc_init_libintl ();

  if (signal (SIGINT, SIG_IGN) != SIG_IGN)
    signal (SIGINT, fatal_error);
#ifdef SIGHUP
  if (signal (SIGHUP, SIG_IGN) != SIG_IGN)
    signal (SIGHUP, fatal_error);
#endif
  if (signal (SIGTERM, SIG_IGN) != SIG_IGN)
    signal (SIGTERM, fatal_error);
#ifdef SIGPIPE
  if (signal (SIGPIPE, SIG_IGN) != SIG_IGN)
    signal (SIGPIPE, fatal_error);
#endif
#ifdef SIGCHLD
  /* We *MUST* set SIGCHLD to SIG_DFL so that the wait4() call will
     receive the signal.  A different setting is inheritable */
  signal (SIGCHLD, SIG_DFL);
#endif

  /* Allocate the argument vector.  */
  alloc_args ();

  obstack_init (&obstack);

  /* Build multilib_select, et. al from the separate lines that make up each
     multilib selection.  */
  {
    const char *const *q = multilib_raw;
    int need_space;

    obstack_init (&multilib_obstack);
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_select = XOBFINISH (&multilib_obstack, const char *);

    q = multilib_matches_raw;
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_matches = XOBFINISH (&multilib_obstack, const char *);

    q = multilib_exclusions_raw;
    while ((p = *q++) != (char *) 0)
      obstack_grow (&multilib_obstack, p, strlen (p));

    obstack_1grow (&multilib_obstack, 0);
    multilib_exclusions = XOBFINISH (&multilib_obstack, const char *);

    need_space = FALSE;
    for (i = 0; i < ARRAY_SIZE (multilib_defaults_raw); i++)
      {
	if (need_space)
	  obstack_1grow (&multilib_obstack, ' ');
	obstack_grow (&multilib_obstack,
		      multilib_defaults_raw[i],
		      strlen (multilib_defaults_raw[i]));
	need_space = TRUE;
      }

    obstack_1grow (&multilib_obstack, 0);
    multilib_defaults = XOBFINISH (&multilib_obstack, const char *);
  }

  /* Set up to remember the pathname of gcc and any options
     needed for collect.  We use argv[0] instead of programname because
     we need the complete pathname.  */
  obstack_init (&collect_obstack);
  obstack_grow (&collect_obstack, "COLLECT_GCC=", sizeof ("COLLECT_GCC=") - 1);
  obstack_grow (&collect_obstack, argv[0], strlen (argv[0]) + 1);
  xputenv (XOBFINISH (&collect_obstack, char *));

#ifdef INIT_ENVIRONMENT
  /* Set up any other necessary machine specific environment variables.  */
  xputenv (INIT_ENVIRONMENT);
#endif

  /* Make a table of what switches there are (switches, n_switches).
     Make a table of specified input files (infiles, n_infiles).
     Decode switches that are handled locally.  */

  process_command (argc, (const char **) argv);

  /* Initialize the vector of specs to just the default.
     This means one element containing 0s, as a terminator.  */

  compilers = xmalloc (sizeof default_compilers);
  memcpy (compilers, default_compilers, sizeof default_compilers);
  n_compilers = n_default_compilers;

  /* Read specs from a file if there is one.  */

  machine_suffix = concat (spec_machine, dir_separator_str,
			   spec_version, dir_separator_str, NULL);
  just_machine_suffix = concat (spec_machine, dir_separator_str, NULL);

  specs_file = find_a_file (&startfile_prefixes, "specs", R_OK, true);
  /* Read the specs file unless it is a default one.  */
  if (specs_file != 0 && strcmp (specs_file, "specs"))
    read_specs (specs_file, TRUE);
  else
    init_spec ();

  /* We need to check standard_exec_prefix/just_machine_suffix/specs
     for any override of as, ld and libraries.  */
  specs_file = alloca (strlen (standard_exec_prefix)
		       + strlen (just_machine_suffix) + sizeof ("specs"));

  strcpy (specs_file, standard_exec_prefix);
  strcat (specs_file, just_machine_suffix);
  strcat (specs_file, "specs");
  if (access (specs_file, R_OK) == 0)
    read_specs (specs_file, TRUE);

  /* Process any configure-time defaults specified for the command line
     options, via OPTION_DEFAULT_SPECS.  */
  for (i = 0; i < ARRAY_SIZE (option_default_specs); i++)
    do_option_spec (option_default_specs[i].name,
		    option_default_specs[i].spec);

  /* Process DRIVER_SELF_SPECS, adding any new options to the end
     of the command line.  */

  for (i = 0; i < ARRAY_SIZE (driver_self_specs); i++)
    do_self_spec (driver_self_specs[i]);

  /* If not cross-compiling, look for executables in the standard
     places.  */
  if (*cross_compile == '0')
    {
      if (*md_exec_prefix)
	{
	  add_prefix (&exec_prefixes, md_exec_prefix, "GCC",
		      PREFIX_PRIORITY_LAST, 0, 0);
	}
    }

  /* Process sysroot_suffix_spec.  */
  if (*sysroot_suffix_spec != 0
      && do_spec_2 (sysroot_suffix_spec) == 0)
    {
      if (argbuf_index > 1)
        error ("spec failure: more than one arg to SYSROOT_SUFFIX_SPEC");
      else if (argbuf_index == 1)
        target_sysroot_suffix = xstrdup (argbuf[argbuf_index -1]);
    }

#ifdef HAVE_LD_SYSROOT
  /* Pass the --sysroot option to the linker, if it supports that.  If
     there is a sysroot_suffix_spec, it has already been processed by
     this point, so target_system_root really is the system root we
     should be using.  */
  if (target_system_root)
    {
      obstack_grow (&obstack, "%(sysroot_spec) ", strlen ("%(sysroot_spec) "));
      obstack_grow0 (&obstack, link_spec, strlen (link_spec));
      set_spec ("link", XOBFINISH (&obstack, const char *));
    }
#endif

  /* Process sysroot_hdrs_suffix_spec.  */
  if (*sysroot_hdrs_suffix_spec != 0
      && do_spec_2 (sysroot_hdrs_suffix_spec) == 0)
    {
      if (argbuf_index > 1)
        error ("spec failure: more than one arg to SYSROOT_HEADERS_SUFFIX_SPEC");
      else if (argbuf_index == 1)
        target_sysroot_hdrs_suffix = xstrdup (argbuf[argbuf_index -1]);
    }

  /* Look for startfiles in the standard places.  */
  if (*startfile_prefix_spec != 0
      && do_spec_2 (startfile_prefix_spec) == 0
      && do_spec_1 (" ", 0, NULL) == 0)
    {
      int ndx;
      for (ndx = 0; ndx < argbuf_index; ndx++)
	add_sysrooted_prefix (&startfile_prefixes, argbuf[ndx], "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
    }
  /* We should eventually get rid of all these and stick to
     startfile_prefix_spec exclusively.  */
  else if (*cross_compile == '0' || target_system_root)
    {
      if (*md_startfile_prefix)
	add_sysrooted_prefix (&startfile_prefixes, md_startfile_prefix,
			      "GCC", PREFIX_PRIORITY_LAST, 0, 1);

      if (*md_startfile_prefix_1)
	add_sysrooted_prefix (&startfile_prefixes, md_startfile_prefix_1,
			      "GCC", PREFIX_PRIORITY_LAST, 0, 1);

      /* If standard_startfile_prefix is relative, base it on
	 standard_exec_prefix.  This lets us move the installed tree
	 as a unit.  If GCC_EXEC_PREFIX is defined, base
	 standard_startfile_prefix on that as well.

         If the prefix is relative, only search it for native compilers;
         otherwise we will search a directory containing host libraries.  */
      if (IS_ABSOLUTE_PATH (standard_startfile_prefix))
	add_sysrooted_prefix (&startfile_prefixes,
			      standard_startfile_prefix, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
      else if (*cross_compile == '0')
	{
	  add_prefix (&startfile_prefixes,
		      concat (gcc_exec_prefix 
			      ? gcc_exec_prefix : standard_exec_prefix, 
			      machine_suffix, 
			      standard_startfile_prefix, NULL),
		      NULL, PREFIX_PRIORITY_LAST, 0, 1);
	}

      /* Sysrooted prefixes are relocated because target_system_root is
	 also relocated by gcc_exec_prefix.  */
      if (*standard_startfile_prefix_1)
 	add_sysrooted_prefix (&startfile_prefixes,
			      standard_startfile_prefix_1, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
      if (*standard_startfile_prefix_2)
	add_sysrooted_prefix (&startfile_prefixes,
			      standard_startfile_prefix_2, "BINUTILS",
			      PREFIX_PRIORITY_LAST, 0, 1);
    }

  /* Process any user specified specs in the order given on the command
     line.  */
  for (uptr = user_specs_head; uptr; uptr = uptr->next)
    {
      char *filename = find_a_file (&startfile_prefixes, uptr->filename,
				    R_OK, true);
      read_specs (filename ? filename : uptr->filename, FALSE);
    }

  /* If we have a GCC_EXEC_PREFIX envvar, modify it for cpp's sake.  */
  if (gcc_exec_prefix)
    {
      gcc_exec_prefix = concat (gcc_exec_prefix, spec_machine, dir_separator_str,
                                spec_version, dir_separator_str, NULL);
      if ((debug_driver_val & 0x02))
        fprintf(stdout,"GCC_EXEC_PREFIX(4)=%s\n",
                gcc_exec_prefix ? gcc_exec_prefix : "NULL");
    }

  /* Now we have the specs.
     Set the `valid' bits for switches that match anything in any spec.  */

  validate_all_switches ();

  /* Now that we have the switches and the specs, set
     the subdirectory based on the options.  */
  set_multilib_dir ();

  /* Warn about any switches that no pass was interested in.  */

  for (i = 0; (int) i < n_switches; i++)
    if (! switches[i].validated)
      error ("unrecognized option '-%s'", switches[i].part1);

  /* Obey some of the options.  */

  if (print_search_dirs)
    {
      printf (_("install: %s%s\n"),
	      gcc_exec_prefix ? gcc_exec_prefix : standard_exec_prefix,
	      gcc_exec_prefix ? "" : machine_suffix);
      printf (_("programs: %s\n"),
	      build_search_list (&exec_prefixes, "", false, false));
      printf (_("libraries: %s\n"),
	      build_search_list (&startfile_prefixes, "", false, true));
      return (0);
    }

  if (print_file_name)
    {
      printf ("%s\n", find_file (print_file_name));
      return (0);
    }

  if (print_prog_name)
    {
      char *newname = find_a_file (&exec_prefixes, print_prog_name, X_OK, 0);
      printf ("%s\n", (newname ? newname : print_prog_name));
      return (0);
    }

  if (print_multi_lib)
    {
      print_multilib_info ();
      return (0);
    }

  if (print_multi_directory)
    {
      if (multilib_dir == NULL)
	printf (".\n");
      else
	printf ("%s\n", multilib_dir);
      return (0);
    }

  if (print_multi_os_directory)
    {
      if (multilib_os_dir == NULL)
	printf (".\n");
      else
	printf ("%s\n", multilib_os_dir);
      return (0);
    }

  if (print_sysroot_headers_suffix)
    {
      if (*sysroot_hdrs_suffix_spec)
	{
	  printf("%s\n", (target_sysroot_hdrs_suffix
			  ? target_sysroot_hdrs_suffix
			  : ""));
	  return (0);
	}
      else
	/* The error status indicates that only one set of fixed
	   headers should be built.  */
	fatal ("not configured with sysroot headers suffix");
    }

  if (print_help_list)
    {
      display_help ();

      if (! verbose_flag)
	{
	  printf (_("Submit bugs to:\n"));
	  printf ("%s.\n", bug_report_url);

	  return (0);
	}

      /* We do not exit here.  Instead we have created a fake input file
	 called 'help-dummy' which needs to be compiled, and we pass this
	 on the various sub-processes, along with the --help switch.  */
    }

  if (verbose_flag)
    {
      int n;
      const char *thrmod;

      notice ("Target: %s\n", spec_machine);
      notice ("Configured with: %s\n", configuration_arguments);

#ifdef THREAD_MODEL_SPEC
      /* We could have defined THREAD_MODEL_SPEC to "%*" by default,
	 but there's no point in doing all this processing just to get
	 thread_model back.  */
      obstack_init (&obstack);
      do_spec_1 (THREAD_MODEL_SPEC, 0, thread_model);
      obstack_1grow (&obstack, '\0');
      thrmod = XOBFINISH (&obstack, const char *);
#else
      thrmod = thread_model;
#endif

      notice ("Thread model: %s\n", thrmod);

      /* compiler_version is truncated at the first space when initialized
	 from version string, so truncate version_string at the first space
	 before comparing.  */
      for (n = 0; version_string[n]; n++)
	if (version_string[n] == ' ')
	  break;

      if (! strncmp (version_string, compiler_version, n)
	  && compiler_version[n] == 0)
	notice ("gcc version %s %s\n", version_string, pkgversion_string);
      else
	notice ("gcc driver version %s %sexecuting gcc version %s\n",
		version_string, pkgversion_string, compiler_version);

      if (n_infiles == 0)
	return (0);
    }

  if (n_infiles == added_libraries)
    fatal ("no input files");

  /* Make a place to record the compiler output file names
     that correspond to the input files.  */

  i = n_infiles;
  i += lang_specific_extra_outfiles;
  outfiles = XCNEWVEC (const char *, i);

  /* Record which files were specified explicitly as link input.  */

  explicit_link_files = XCNEWVEC (char, n_infiles);

  if (combine_flag)
    combine_inputs = true;
  else
    combine_inputs = false;

  for (i = 0; (int) i < n_infiles; i++)
    {
      const char *name = infiles[i].name;
      struct compiler *compiler = lookup_compiler (name,
						   strlen (name),
						   infiles[i].language);

      if (compiler && !(compiler->combinable))
	combine_inputs = false;

      if (lang_n_infiles > 0 && compiler != input_file_compiler
	  && infiles[i].language && infiles[i].language[0] != '*')
	infiles[i].incompiler = compiler;
      else if (compiler)
	{
	  lang_n_infiles++;
	  input_file_compiler = compiler;
	  infiles[i].incompiler = compiler;
	}
      else
	{
	  /* Since there is no compiler for this input file, assume it is a
	     linker file.  */
	  explicit_link_files[i] = 1;
	  infiles[i].incompiler = NULL;
	}
      infiles[i].compiled = false;
      infiles[i].preprocessed = false;
    }

  if (!combine_inputs && have_c && have_o && lang_n_infiles > 1)
   fatal ("cannot specify -o with -c or -S with multiple files");

  if (combine_flag && save_temps_flag)
    {
      bool save_combine_inputs = combine_inputs;
      /* Must do a separate pre-processing pass for C & Objective-C files, to
	 obtain individual .i files.  */

      combine_inputs = false;
      for (i = 0; (int) i < n_infiles; i++)
	{
	  int this_file_error = 0;

	  input_file_number = i;
	  set_input (infiles[i].name);
	  if (infiles[i].incompiler
	      && (infiles[i].incompiler)->needs_preprocessing)
	    input_file_compiler = infiles[i].incompiler;
	  else
	    continue;

	  if (input_file_compiler)
	    {
	      if (input_file_compiler->spec[0] == '#')
		{
		  error ("%s: %s compiler not installed on this system",
			 input_filename, &input_file_compiler->spec[1]);
		  this_file_error = 1;
		}
	      else
		{
		  value = do_spec (input_file_compiler->spec);
		  infiles[i].preprocessed = true;
		  if (!have_o_argbuf_index)
		    fatal ("spec '%s' is invalid", input_file_compiler->spec);
		  infiles[i].name = argbuf[have_o_argbuf_index];
		  infiles[i].incompiler
		    = lookup_compiler (infiles[i].name,
				       strlen (infiles[i].name),
				       infiles[i].language);

		  if (value < 0)
		    this_file_error = 1;
		}
	    }

	  if (this_file_error)
	    {
	      delete_failure_queue ();
	      error_count++;
	      break;
	    }
	  clear_failure_queue ();
	}
      combine_inputs = save_combine_inputs;
    }

  for (i = 0; (int) i < n_infiles; i++)
    {
      int this_file_error = 0;

      /* Tell do_spec what to substitute for %i.  */

      input_file_number = i;
      set_input (infiles[i].name);

      if (infiles[i].compiled)
	continue;

      /* Use the same thing in %o, unless cp->spec says otherwise.  */

      outfiles[i] = input_filename;

      /* Figure out which compiler from the file's suffix.  */

      if (! combine_inputs)
	input_file_compiler
	  = lookup_compiler (infiles[i].name, input_filename_length,
			     infiles[i].language);
      else
	input_file_compiler = infiles[i].incompiler;

      if (input_file_compiler)
	{
	  /* Ok, we found an applicable compiler.  Run its spec.  */

	  if (input_file_compiler->spec[0] == '#')
	    {
	      error ("%s: %s compiler not installed on this system",
		     input_filename, &input_file_compiler->spec[1]);
	      this_file_error = 1;
	    }
	  else
	    {
	      value = do_spec (input_file_compiler->spec);
	      infiles[i].compiled = true;
	      if (value < 0)
		this_file_error = 1;
	    }
	}

      /* If this file's name does not contain a recognized suffix,
	 record it as explicit linker input.  */

      else
	explicit_link_files[i] = 1;

      /* Clear the delete-on-failure queue, deleting the files in it
	 if this compilation failed.  */

      if (this_file_error)
	{
	  delete_failure_queue ();
	  error_count++;
	}
      /* If this compilation succeeded, don't delete those files later.  */
      clear_failure_queue ();
    }

  /* Reset the input file name to the first compile/object file name, for use
     with %b in LINK_SPEC. We use the first input file that we can find
     a compiler to compile it instead of using infiles.language since for
     languages other than C we use aliases that we then lookup later.  */
  if (n_infiles > 0)
    {
      int i;

      for (i = 0; i < n_infiles ; i++)
	if (infiles[i].language && infiles[i].language[0] != '*')
	  {
	    set_input (infiles[i].name);
	    break;
	  }
    }

  if (error_count == 0)
    {
      /* Make sure INPUT_FILE_NUMBER points to first available open
	 slot.  */
      input_file_number = n_infiles;
      if (lang_specific_pre_link ())
	error_count++;
    }

  /* Determine if there are any linker input files.  */
  num_linker_inputs = 0;
  for (i = 0; (int) i < n_infiles; i++)
    if (explicit_link_files[i] || outfiles[i] != NULL)
      num_linker_inputs++;

  /* Run ld to link all the compiler output files.  */

  if (num_linker_inputs > 0 && error_count == 0 && print_subprocess_help < 2)
    {
      int tmp = execution_count;

      /* We'll use ld if we can't find collect2.  */
      if (! strcmp (linker_name_spec, "collect2"))
	{
	  char *s = find_a_file (&exec_prefixes, "collect2", X_OK, false);
	  if (s == NULL)
	    linker_name_spec = "ld";
	}
      /* Rebuild the COMPILER_PATH and LIBRARY_PATH environment variables
	 for collect.  */
      putenv_from_prefixes (&exec_prefixes, "COMPILER_PATH", false);
      putenv_from_prefixes (&startfile_prefixes, LIBRARY_PATH_ENV, true);

      if (print_subprocess_help == 1)
	{
	  printf (_("\nLinker options\n==============\n\n"));
	  printf (_("Use \"-Wl,OPTION\" to pass \"OPTION\""
		    " to the linker.\n\n"));
	  fflush (stdout);
	}
      value = do_spec (link_command_spec);
      if (value < 0)
	error_count = 1;
      linker_was_run = (tmp != execution_count);
    }

  /* If options said don't run linker,
     complain about input files to be given to the linker.  */

  if (! linker_was_run && error_count == 0)
    for (i = 0; (int) i < n_infiles; i++)
      if (explicit_link_files[i]
	  && !(infiles[i].language && infiles[i].language[0] == '*'))
	error ("%s: linker input file unused because linking not done",
	       outfiles[i]);

  /* Delete some or all of the temporary files we made.  */

  if (error_count)
    delete_failure_queue ();
  delete_temp_files ();

  if (print_help_list)
    {
      printf (("\nSubmit bug reports to:\n"));
      printf ("%s\n", bug_report_url);
    }

  return (signal_count != 0 ? 2
	  : error_count > 0 ? (pass_exit_codes ? greatest_status : 1)
	  : 0);
}

/* Find the proper compilation spec for the file name NAME,
   whose length is LENGTH.  LANGUAGE is the specified language,
   or 0 if this file is to be passed to the linker.  */

static struct compiler *
lookup_compiler (const char *name, size_t length, const char *language)
{
  struct compiler *cp;

  /* If this was specified by the user to be a linker input, indicate that.  */
  if (language != 0 && language[0] == '*')
    return 0;

  /* Otherwise, look for the language, if one is spec'd.  */
  if (language != 0)
    {
      for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
	if (cp->suffix[0] == '@' && !strcmp (cp->suffix + 1, language))
	  return cp;

      error ("language %s not recognized", language);
      return 0;
    }

  /* Look for a suffix.  */
  for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
    {
      if (/* The suffix `-' matches only the file name `-'.  */
	  (!strcmp (cp->suffix, "-") && !strcmp (name, "-"))
	  || (strlen (cp->suffix) < length
	      /* See if the suffix matches the end of NAME.  */
	      && !strcmp (cp->suffix,
			  name + length - strlen (cp->suffix))
	 ))
	break;
    }

#if defined (OS2) ||defined (HAVE_DOS_BASED_FILE_SYSTEM)
  /* Look again, but case-insensitively this time.  */
  if (cp < compilers)
    for (cp = compilers + n_compilers - 1; cp >= compilers; cp--)
      {
	if (/* The suffix `-' matches only the file name `-'.  */
	    (!strcmp (cp->suffix, "-") && !strcmp (name, "-"))
	    || (strlen (cp->suffix) < length
		/* See if the suffix matches the end of NAME.  */
		&& ((!strcmp (cp->suffix,
			     name + length - strlen (cp->suffix))
		     || !strpbrk (cp->suffix, "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
		    && !strcasecmp (cp->suffix,
				    name + length - strlen (cp->suffix)))
	   ))
	  break;
      }
#endif

  if (cp >= compilers)
    {
      if (cp->spec[0] != '@')
	/* A non-alias entry: return it.  */
	return cp;

      /* An alias entry maps a suffix to a language.
	 Search for the language; pass 0 for NAME and LENGTH
	 to avoid infinite recursion if language not found.  */
      return lookup_compiler (NULL, 0, cp->spec + 1);
    }
  return 0;
}

static char *
save_string (const char *s, int len)
{
  char *result = XNEWVEC (char, len + 1);

  memcpy (result, s, len);
  result[len] = 0;
  return result;
}

void
pfatal_with_name (const char *name)
{
  perror_with_name (name);
  delete_temp_files ();
  exit (1);
}

static void
perror_with_name (const char *name)
{
  error ("%s: %s", name, xstrerror (errno));
}

/* Output an error message and exit.  */

void
fancy_abort (const char *file, int line, const char *func)
{
  fatal_ice ("internal gcc abort in %s, at %s:%d", func, file, line);
}

/* Output an error message and exit.  */

void
fatal_ice (const char *cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);

  fprintf (stderr, "%s: ", programname);
  vfprintf (stderr, _(cmsgid), ap);
  va_end (ap);
  fprintf (stderr, "\n");
  delete_temp_files ();
  exit (pass_exit_codes ? ICE_EXIT_CODE : 1);
}

void
fatal (const char *cmsgid, ...)
{
  va_list ap;

  va_start (ap, cmsgid);

  fprintf (stderr, "%s: ", programname);
  vfprintf (stderr, _(cmsgid), ap);
  va_end (ap);
  fprintf (stderr, "\n");
  delete_temp_files ();
  exit (1);
}

/* The argument is actually c-format, not gcc-internal-format,
   but because functions with identical names are used through
   the rest of the compiler with gcc-internal-format, we just
   need to hope all users of these functions use the common
   subset between c-format and gcc-internal-format.  */

void
error (const char *gmsgid, ...)
{
  va_list ap;

  va_start (ap, gmsgid);
  fprintf (stderr, "%s: ", programname);
  vfprintf (stderr, _(gmsgid), ap);
  va_end (ap);

  fprintf (stderr, "\n");
}

static void
warning (const char *cmsgid, ...)
{
  va_list ap;

  if (quiet_driver) return;

  va_start (ap, cmsgid);
  fprintf (stderr, "%s: warning: ", programname);
  vfprintf (stderr, _(cmsgid), ap);
  va_end (ap);
}

static void
notice (const char *cmsgid, ...)
{
  va_list ap;
  
  if (quiet_driver) return;

  va_start (ap, cmsgid);
  vfprintf (stderr, _(cmsgid), ap);
  va_end (ap);
}

static inline void
validate_switches_from_spec (const char *spec)
{
  const char *p = spec;
  char c;
  while ((c = *p++))
    if (c == '%' && (*p == '{' || *p == '<' || (*p == 'W' && *++p == '{')))
      /* We have a switch spec.  */
      p = validate_switches (p + 1);
}

static void
validate_all_switches (void)
{
  struct compiler *comp;
  struct spec_list *spec;

  for (comp = compilers; comp->spec; comp++)
    validate_switches_from_spec (comp->spec);

  /* Look through the linked list of specs read from the specs file.  */
  for (spec = specs; spec; spec = spec->next)
    validate_switches_from_spec (*spec->ptr_spec);

  validate_switches_from_spec (link_command_spec);
}

/* Look at the switch-name that comes after START
   and mark as valid all supplied switches that match it.  */

static const char *
validate_switches (const char *start)
{
  const char *p = start;
  const char *atom;
  size_t len;
  int i;
  bool suffix = false;
  bool starred = false;

#define SKIP_WHITE() do { while (*p == ' ' || *p == '\t') p++; } while (0)

next_member:
  SKIP_WHITE ();

  if (*p == '!')
    p++;

  SKIP_WHITE ();
  if (*p == '.' || *p == ',')
    suffix = true, p++;

  atom = p;
  while (ISIDNUM (*p) || *p == '-' || *p == '+' || *p == '='
	 || *p == ',' || *p == '.' || *p == '@')
    p++;
  len = p - atom;

  if (*p == '*')
    starred = true, p++;

  SKIP_WHITE ();

  if (!suffix && len > 0)
    {
      /* need len>0 so don't match on NULL : in switch "cascading if" */   
      /* Mark all matching switches as valid.  */
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i].part1, atom, len)
	    && (starred || switches[i].part1[len] == 0))
	  switches[i].validated = 1;
    }

  if (*p) p++;
  if (*p && (p[-1] == '|' || p[-1] == '&'))
    goto next_member;

  if (*p && p[-1] == ':')
    {
      while (*p && *p != ';' && *p != '}')
	{
	  if (*p == '%')
	    {
	      p++;
	      if (*p == '{' || *p == '<')
		p = validate_switches (p+1);
	      else if (p[0] == 'W' && p[1] == '{')
		p = validate_switches (p+2);
	    }
	  else
	    p++;
	}

      if (*p) p++;
      if (*p && p[-1] == ';')
	goto next_member;
    }

  return p;
#undef SKIP_WHITE
}

struct mdswitchstr
{
  const char *str;
  int len;
};

static struct mdswitchstr *mdswitches;
static int n_mdswitches;

/* Check whether a particular argument was used.  The first time we
   canonicalize the switches to keep only the ones we care about.  */

static int
used_arg (const char *p, int len)
{
  struct mswitchstr
  {
    const char *str;
    const char *replace;
    int len;
    int rep_len;
  };

  static struct mswitchstr *mswitches;
  static int n_mswitches;
  int i, j;

  if (!mswitches)
    {
      struct mswitchstr *matches;
      const char *q;
      int cnt = 0;

      /* Break multilib_matches into the component strings of string
         and replacement string.  */
      for (q = multilib_matches; *q != '\0'; q++)
	if (*q == ';')
	  cnt++;

      matches = alloca ((sizeof (struct mswitchstr)) * cnt);
      i = 0;
      q = multilib_matches;
      while (*q != '\0')
	{
	  matches[i].str = q;
	  while (*q != ' ')
	    {
	      if (*q == '\0')
		{
		invalid_matches:
		  fatal ("multilib spec '%s' is invalid", multilib_matches);
		}
	      q++;
	    }
	  matches[i].len = q - matches[i].str;

	  matches[i].replace = ++q;
	  while (*q != ';' && *q != '\0')
	    {
	      if (*q == ' ')
		goto invalid_matches;
	      q++;
	    }
	  matches[i].rep_len = q - matches[i].replace;
	  i++;
	  if (*q == ';')
	    q++;
	}

      /* Now build a list of the replacement string for switches that we care
	 about.  Make sure we allocate at least one entry.  This prevents
	 xmalloc from calling fatal, and prevents us from re-executing this
	 block of code.  */
      mswitches
	= XNEWVEC (struct mswitchstr, n_mdswitches + (n_switches ? n_switches : 1));
      for (i = 0; i < n_switches; i++)
	if ((switches[i].live_cond & SWITCH_IGNORE) == 0)
	  {
	    int xlen = strlen (switches[i].part1);
	    for (j = 0; j < cnt; j++)
	      if (xlen == matches[j].len
		  && ! strncmp (switches[i].part1, matches[j].str, xlen))
		{
		  mswitches[n_mswitches].str = matches[j].replace;
		  mswitches[n_mswitches].len = matches[j].rep_len;
		  mswitches[n_mswitches].replace = (char *) 0;
		  mswitches[n_mswitches].rep_len = 0;
		  n_mswitches++;
		  break;
		}
	  }

      /* Add MULTILIB_DEFAULTS switches too, as long as they were not present
	 on the command line nor any options mutually incompatible with
	 them.  */
      for (i = 0; i < n_mdswitches; i++)
	{
	  const char *r;

	  for (q = multilib_options; *q != '\0'; q++)
	    {
	      while (*q == ' ')
		q++;

	      r = q;
	      while (strncmp (q, mdswitches[i].str, mdswitches[i].len) != 0
		     || strchr (" /", q[mdswitches[i].len]) == NULL)
		{
		  while (*q != ' ' && *q != '/' && *q != '\0')
		    q++;
		  if (*q != '/')
		    break;
		  q++;
		}

	      if (*q != ' ' && *q != '\0')
		{
		  while (*r != ' ' && *r != '\0')
		    {
		      q = r;
		      while (*q != ' ' && *q != '/' && *q != '\0')
			q++;

		      if (used_arg (r, q - r))
			break;

		      if (*q != '/')
			{
			  mswitches[n_mswitches].str = mdswitches[i].str;
			  mswitches[n_mswitches].len = mdswitches[i].len;
			  mswitches[n_mswitches].replace = (char *) 0;
			  mswitches[n_mswitches].rep_len = 0;
			  n_mswitches++;
			  break;
			}

		      r = q + 1;
		    }
		  break;
		}
	    }
	}
    }

  for (i = 0; i < n_mswitches; i++)
    if (len == mswitches[i].len && ! strncmp (p, mswitches[i].str, len))
      return 1;

  return 0;
}

static int
default_arg (const char *p, int len)
{
  int i;

  for (i = 0; i < n_mdswitches; i++)
    if (len == mdswitches[i].len && ! strncmp (p, mdswitches[i].str, len))
      return 1;

  return 0;
}

/* Work out the subdirectory to use based on the options. The format of
   multilib_select is a list of elements. Each element is a subdirectory
   name followed by a list of options followed by a semicolon. The format
   of multilib_exclusions is the same, but without the preceding
   directory. First gcc will check the exclusions, if none of the options
   beginning with an exclamation point are present, and all of the other
   options are present, then we will ignore this completely. Passing
   that, gcc will consider each multilib_select in turn using the same
   rules for matching the options. If a match is found, that subdirectory
   will be used.  */

static void
set_multilib_dir (void)
{
  const char *p;
  unsigned int this_path_len;
  const char *this_path, *this_arg;
  const char *start, *end;
  int not_arg;
  int ok, ndfltok, first;

  n_mdswitches = 0;
  start = multilib_defaults;
  while (*start == ' ' || *start == '\t')
    start++;
  while (*start != '\0')
    {
      n_mdswitches++;
      while (*start != ' ' && *start != '\t' && *start != '\0')
	start++;
      while (*start == ' ' || *start == '\t')
        start++;
    }

  if (n_mdswitches)
    {
      int i = 0;

      mdswitches = XNEWVEC (struct mdswitchstr, n_mdswitches);
      for (start = multilib_defaults; *start != '\0'; start = end + 1)
	{
	  while (*start == ' ' || *start == '\t')
	    start++;

	  if (*start == '\0')
	    break;

	  for (end = start + 1;
	       *end != ' ' && *end != '\t' && *end != '\0'; end++)
	    ;

	  obstack_grow (&multilib_obstack, start, end - start);
	  obstack_1grow (&multilib_obstack, 0);
	  mdswitches[i].str = XOBFINISH (&multilib_obstack, const char *);
	  mdswitches[i++].len = end - start;

	  if (*end == '\0')
	    break;
	}
    }

  p = multilib_exclusions;
  while (*p != '\0')
    {
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Check the arguments.  */
      ok = 1;
      while (*p != ';')
	{
	  if (*p == '\0')
	    {
	    invalid_exclusions:
	      fatal ("multilib exclusions '%s' is invalid",
		     multilib_exclusions);
	    }

	  if (! ok)
	    {
	      ++p;
	      continue;
	    }

	  this_arg = p;
	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		goto invalid_exclusions;
	      ++p;
	    }

	  if (*this_arg != '!')
	    not_arg = 0;
	  else
	    {
	      not_arg = 1;
	      ++this_arg;
	    }

	  ok = used_arg (this_arg, p - this_arg);
	  if (not_arg)
	    ok = ! ok;

	  if (*p == ' ')
	    ++p;
	}

      if (ok)
	return;

      ++p;
    }

  first = 1;
  p = multilib_select;
  while (*p != '\0')
    {
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Get the initial path.  */
      this_path = p;
      while (*p != ' ')
	{
	  if (*p == '\0')
	    {
	    invalid_select:
	      fatal ("multilib select '%s' is invalid",
		     multilib_select);
	    }
	  ++p;
	}
      this_path_len = p - this_path;

      /* Check the arguments.  */
      ok = 1;
      ndfltok = 1;
      ++p;
      while (*p != ';')
	{
	  if (*p == '\0')
	    goto invalid_select;

	  if (! ok)
	    {
	      ++p;
	      continue;
	    }

	  this_arg = p;
	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		goto invalid_select;
	      ++p;
	    }

	  if (*this_arg != '!')
	    not_arg = 0;
	  else
	    {
	      not_arg = 1;
	      ++this_arg;
	    }

	  /* If this is a default argument, we can just ignore it.
	     This is true even if this_arg begins with '!'.  Beginning
	     with '!' does not mean that this argument is necessarily
	     inappropriate for this library: it merely means that
	     there is a more specific library which uses this
	     argument.  If this argument is a default, we need not
	     consider that more specific library.  */
	  ok = used_arg (this_arg, p - this_arg);
	  if (not_arg)
	    ok = ! ok;

	  if (! ok)
	    ndfltok = 0;

	  if (default_arg (this_arg, p - this_arg))
	    ok = 1;

	  if (*p == ' ')
	    ++p;
	}

      if (ok && first)
	{
	  if (this_path_len != 1
	      || this_path[0] != '.')
	    {
	      char *new_multilib_dir = XNEWVEC (char, this_path_len + 1);
	      char *q;

	      strncpy (new_multilib_dir, this_path, this_path_len);
	      new_multilib_dir[this_path_len] = '\0';
	      q = strchr (new_multilib_dir, ':');
	      if (q != NULL)
		*q = '\0';
	      multilib_dir = new_multilib_dir;
	    }
	  first = 0;
	}

      if (ndfltok)
	{
	  const char *q = this_path, *end = this_path + this_path_len;

	  while (q < end && *q != ':')
	    q++;
	  if (q < end)
	    {
	      char *new_multilib_os_dir = XNEWVEC (char, end - q);
	      memcpy (new_multilib_os_dir, q + 1, end - q - 1);
	      new_multilib_os_dir[end - q - 1] = '\0';
	      multilib_os_dir = new_multilib_os_dir;
	      break;
	    }
	}

      ++p;
    }

  if (multilib_dir == NULL && multilib_os_dir != NULL
      && strcmp (multilib_os_dir, ".") == 0)
    {
      free (CONST_CAST (char *, multilib_os_dir));
      multilib_os_dir = NULL;
    }
  else if (multilib_dir != NULL && multilib_os_dir == NULL)
    multilib_os_dir = multilib_dir;
}

/* Print out the multiple library subdirectory selection
   information.  This prints out a series of lines.  Each line looks
   like SUBDIRECTORY;@OPTION@OPTION, with as many options as is
   required.  Only the desired options are printed out, the negative
   matches.  The options are print without a leading dash.  There are
   no spaces to make it easy to use the information in the shell.
   Each subdirectory is printed only once.  This assumes the ordering
   generated by the genmultilib script. Also, we leave out ones that match
   the exclusions.  */

static void
print_multilib_info (void)
{
  const char *p = multilib_select;
  const char *last_path = 0, *this_path;
  int skip;
  unsigned int last_path_len = 0;

  while (*p != '\0')
    {
      skip = 0;
      /* Ignore newlines.  */
      if (*p == '\n')
	{
	  ++p;
	  continue;
	}

      /* Get the initial path.  */
      this_path = p;
      while (*p != ' ')
	{
	  if (*p == '\0')
	    {
	    invalid_select:
	      fatal ("multilib select '%s' is invalid", multilib_select);
	    }

	  ++p;
	}

      /* When --disable-multilib was used but target defines
	 MULTILIB_OSDIRNAMES, entries starting with .: are there just
	 to find multilib_os_dir, so skip them from output.  */
      if (this_path[0] == '.' && this_path[1] == ':')
	skip = 1;

      /* Check for matches with the multilib_exclusions. We don't bother
         with the '!' in either list. If any of the exclusion rules match
         all of its options with the select rule, we skip it.  */
      {
	const char *e = multilib_exclusions;
	const char *this_arg;

	while (*e != '\0')
	  {
	    int m = 1;
	    /* Ignore newlines.  */
	    if (*e == '\n')
	      {
		++e;
		continue;
	      }

	    /* Check the arguments.  */
	    while (*e != ';')
	      {
		const char *q;
		int mp = 0;

		if (*e == '\0')
		  {
		  invalid_exclusion:
		    fatal ("multilib exclusion '%s' is invalid",
			   multilib_exclusions);
		  }

		if (! m)
		  {
		    ++e;
		    continue;
		  }

		this_arg = e;

		while (*e != ' ' && *e != ';')
		  {
		    if (*e == '\0')
		      goto invalid_exclusion;
		    ++e;
		  }

		q = p + 1;
		while (*q != ';')
		  {
		    const char *arg;
		    int len = e - this_arg;

		    if (*q == '\0')
		      goto invalid_select;

		    arg = q;

		    while (*q != ' ' && *q != ';')
		      {
			if (*q == '\0')
			  goto invalid_select;
			++q;
		      }

		    if (! strncmp (arg, this_arg,
				   (len < q - arg) ? q - arg : len)
			|| default_arg (this_arg, e - this_arg))
		      {
			mp = 1;
			break;
		      }

		    if (*q == ' ')
		      ++q;
		  }

		if (! mp)
		  m = 0;

		if (*e == ' ')
		  ++e;
	      }

	    if (m)
	      {
		skip = 1;
		break;
	      }

	    if (*e != '\0')
	      ++e;
	  }
      }

      if (! skip)
	{
	  /* If this is a duplicate, skip it.  */
	  skip = (last_path != 0
		  && (unsigned int) (p - this_path) == last_path_len
		  && ! strncmp (last_path, this_path, last_path_len));

	  last_path = this_path;
	  last_path_len = p - this_path;
	}

      /* If this directory requires any default arguments, we can skip
	 it.  We will already have printed a directory identical to
	 this one which does not require that default argument.  */
      if (! skip)
	{
	  const char *q;

	  q = p + 1;
	  while (*q != ';')
	    {
	      const char *arg;

	      if (*q == '\0')
		goto invalid_select;

	      if (*q == '!')
		arg = NULL;
	      else
		arg = q;

	      while (*q != ' ' && *q != ';')
		{
		  if (*q == '\0')
		    goto invalid_select;
		  ++q;
		}

	      if (arg != NULL
		  && default_arg (arg, q - arg))
		{
		  skip = 1;
		  break;
		}

	      if (*q == ' ')
		++q;
	    }
	}

      if (! skip)
	{
	  const char *p1;

	  for (p1 = last_path; p1 < p && *p1 != ':'; p1++)
	    putchar (*p1);
	  putchar (';');
	}

      ++p;
      while (*p != ';')
	{
	  int use_arg;

	  if (*p == '\0')
	    goto invalid_select;

	  if (skip)
	    {
	      ++p;
	      continue;
	    }

	  use_arg = *p != '!';

	  if (use_arg)
	    putchar ('@');

	  while (*p != ' ' && *p != ';')
	    {
	      if (*p == '\0')
		goto invalid_select;
	      if (use_arg)
		putchar (*p);
	      ++p;
	    }

	  if (*p == ' ')
	    ++p;
	}

      if (! skip)
	{
	  /* If there are extra options, print them now.  */
	  if (multilib_extra && *multilib_extra)
	    {
	      int print_at = TRUE;
	      const char *q;

	      for (q = multilib_extra; *q != '\0'; q++)
		{
		  if (*q == ' ')
		    print_at = TRUE;
		  else
		    {
		      if (print_at)
			putchar ('@');
		      putchar (*q);
		      print_at = FALSE;
		    }
		}
	    }

	  putchar ('\n');
	}

      ++p;
    }
}

/* getenv built-in spec function.

   Returns the value of the environment variable given by its first
   argument, concatenated with the second argument.  If the
   environment variable is not defined, a fatal error is issued.  */

static const char *
getenv_spec_function (int argc, const char **argv)
{
  char *value;
  char *result;
  char *ptr;
  size_t len;

  if (argc != 2)
    return NULL;

  value = getenv (argv[0]);
  if (!value)
    fatal ("environment variable \"%s\" not defined", argv[0]);

  /* We have to escape every character of the environment variable so
     they are not interpretted as active spec characters.  A
     particulaly painful case is when we are reading a variable
     holding a windows path complete with \ separators.  */
  len = strlen (value) * 2 + strlen (argv[1]) + 1;
  result = xmalloc (len);
  for (ptr = result; *value; ptr += 2)
    {
      ptr[0] = '\\';
      ptr[1] = *value++;
    }
  
  strcpy (ptr, argv[1]);
  
  return result;
}

/* if-exists built-in spec function.

   Checks to see if the file specified by the absolute pathname in
   ARGS exists.  Returns that pathname if found.

   The usual use for this function is to check for a library file
   (whose name has been expanded with %s).  */

static const char *
if_exists_spec_function (int argc, const char **argv)
{
  /* Must have only one argument.  */
  if (argc == 1 && IS_ABSOLUTE_PATH (argv[0]) && ! access (argv[0], R_OK))
    return argv[0];

  return NULL;
}

/* if-exists-else built-in spec function.

   This is like if-exists, but takes an additional argument which
   is returned if the first argument does not exist.  */

static const char *
if_exists_else_spec_function (int argc, const char **argv)
{
  /* Must have exactly two arguments.  */
  if (argc != 2)
    return NULL;

  if (IS_ABSOLUTE_PATH (argv[0]) && ! access (argv[0], R_OK))
    return argv[0];

  return argv[1];
}

/* replace-outfile built-in spec function.

   This looks for the first argument in the outfiles array's name and
   replaces it with the second argument.  */

static const char *
replace_outfile_spec_function (int argc, const char **argv)
{
  int i;
  /* Must have exactly two arguments.  */
  if (argc != 2)
    abort ();

  for (i = 0; i < n_infiles; i++)
    {
      if (outfiles[i] && !strcmp (outfiles[i], argv[0]))
	outfiles[i] = xstrdup (argv[1]);
    }
  return NULL;
}

/* Given two version numbers, compares the two numbers.
   A version number must match the regular expression
   ([1-9][0-9]*|0)(\.([1-9][0-9]*|0))*
*/
static int
compare_version_strings (const char *v1, const char *v2)
{
  int rresult;
  regex_t r;

  if (regcomp (&r, "^([1-9][0-9]*|0)(\\.([1-9][0-9]*|0))*$",
	       REG_EXTENDED | REG_NOSUB) != 0)
    abort ();
  rresult = regexec (&r, v1, 0, NULL, 0);
  if (rresult == REG_NOMATCH)
    fatal ("invalid version number `%s'", v1);
  else if (rresult != 0)
    abort ();
  rresult = regexec (&r, v2, 0, NULL, 0);
  if (rresult == REG_NOMATCH)
    fatal ("invalid version number `%s'", v2);
  else if (rresult != 0)
    abort ();

  return strverscmp (v1, v2);
}


/* version_compare built-in spec function.

   This takes an argument of the following form:

   <comparison-op> <arg1> [<arg2>] <switch> <result>

   and produces "result" if the comparison evaluates to true,
   and nothing if it doesn't.

   The supported <comparison-op> values are:

   >=  true if switch is a later (or same) version than arg1
   !>  opposite of >=
   <   true if switch is an earlier version than arg1
   !<  opposite of <
   ><  true if switch is arg1 or later, and earlier than arg2
   <>  true if switch is earlier than arg1 or is arg2 or later

   If the switch is not present, the condition is false unless
   the first character of the <comparison-op> is '!'.

   For example,
   %:version-compare(>= 10.3 mmacosx-version-min= -lmx)
   adds -lmx if -mmacosx-version-min=10.3.9 was passed.  */

static const char *
version_compare_spec_function (int argc, const char **argv)
{
  int comp1, comp2;
  size_t switch_len;
  const char *switch_value = NULL;
  int nargs = 1, i;
  bool result;

  if (argc < 3)
    fatal ("too few arguments to %%:version-compare");
  if (argv[0][0] == '\0')
    abort ();
  if ((argv[0][1] == '<' || argv[0][1] == '>') && argv[0][0] != '!')
    nargs = 2;
  if (argc != nargs + 3)
    fatal ("too many arguments to %%:version-compare");

  switch_len = strlen (argv[nargs + 1]);
  for (i = 0; i < n_switches; i++)
    if (!strncmp (switches[i].part1, argv[nargs + 1], switch_len)
	&& check_live_switch (i, switch_len))
      switch_value = switches[i].part1 + switch_len;

  if (switch_value == NULL)
    comp1 = comp2 = -1;
  else
    {
      comp1 = compare_version_strings (switch_value, argv[1]);
      if (nargs == 2)
	comp2 = compare_version_strings (switch_value, argv[2]);
      else
	comp2 = -1;  /* This value unused.  */
    }

  switch (argv[0][0] << 8 | argv[0][1])
    {
    case '>' << 8 | '=':
      result = comp1 >= 0;
      break;
    case '!' << 8 | '<':
      result = comp1 >= 0 || switch_value == NULL;
      break;
    case '<' << 8:
      result = comp1 < 0;
      break;
    case '!' << 8 | '>':
      result = comp1 < 0 || switch_value == NULL;
      break;
    case '>' << 8 | '<':
      result = comp1 >= 0 && comp2 < 0;
      break;
    case '<' << 8 | '>':
      result = comp1 < 0 || comp2 >= 0;
      break;

    default:
      fatal ("unknown operator '%s' in %%:version-compare", argv[0]);
    }
  if (! result)
    return NULL;

  return argv[nargs + 2];
}

/* %:include builtin spec function.  This differs from %include in that it
   can be nested inside a spec, and thus be conditionalized.  It takes
   one argument, the filename, and looks for it in the startfile path.
   The result is always NULL, i.e. an empty expansion.  */

static const char *
include_spec_function (int argc, const char **argv)
{
  char *file;

  if (argc != 1)
    abort ();

  file = find_a_file (&startfile_prefixes, argv[0], R_OK, 0);
  read_specs (file ? file : argv[0], FALSE);

  return NULL;
}

#ifdef CROSS_DIRECTORY_STRUCTURE
/* Handle -xtarget=native and -xarch=native */
/* Returns 0: no -xtarget/-xarch=native
 *         1: saw -xtarget/-xarch=native
 * Basically the above options are translated to generic
 */
static  int
translate_native_options (int *argcp, char ***argvp)
{
  int argc = *argcp;
  char **argv = *argvp;
  int i;
  int retval = 0;

  i = 0;
  while (i < argc)
    {
      if (strcmp(argv[i], "-xarch=native") == 0)
        {
          argv[i] = xstrdup("-xarch=generic");
          retval = 1;
        }
      else if (strcmp(argv[i], "-xarch=native64") == 0)
        {
          argv[i] = xstrdup("-xarch=generic64");
          retval = 1;
        }
      else if (strcmp(argv[i], "-xtarget=native64") == 0)
        {
          argv[i] = xstrdup("-xtarget=generic64");
          retval = 1;
        }
      else if (strcmp(argv[i], "-xtarget=native") == 0)
        {
          argv[i] = xstrdup("-xtarget=generic");
          retval = 1;
        }
      if ((debug_driver_val & 0x01))
        {
          fprintf(stdout, "TRANSLATE_NATIVE: argv[%d] = %s\n",i, argv[i]);
          fflush(stdout);
        }
      i++;
    }
  
  return retval;
}
#elif defined(__linux__)
/* Handle -xtarget=native and -xarch=native */
/* Returns 0: no -xtarget/-xarch=native
 *         1: saw -xtarget/-xarch=native
 */
static  int
translate_native_options (int *argcp, char ***argvp)
{
  int argc = *argcp;
  char **argv = *argvp;
  int i;
  int retval = 0;
  FILE* cpuinfo;
  char* last = NULL;
  char* prev = NULL;
  char* processornickname = NULL;

  i = 0;
  while (i < argc) {
    if (strcmp(argv[i], "-xtarget=native" ) == 0 ||
        strcmp(argv[i], "-xtarget=native64") == 0 ||
        strcmp(argv[i], "-xarch=native64") == 0 ||
        strcmp(argv[i], "-xarch=native") == 0) {

        /* the cpu information is buried in /proc/cpuinfo.
         * Need to find a line that looks like:
           cpu             : UltraSparc T1 (Niagara)
         * Use the name inside ( ) to determine the native system 
         * The actual values are not well defined so expect to add/modify
         * the strcmp's that follow
         */
         cpuinfo = fopen("/proc/cpuinfo", "r");
         if (!cpuinfo) {
            processornickname = NULL;
            goto CONT;
         }
         while ((!feof(cpuinfo)) && fscanf(cpuinfo, "%as", &last)) {
	   if (strcmp(last,":") == 0 && strcmp(prev,"cpu") == 0) {
	      /* seen    cpu      :     <something>
	       * now want the name of processor in ( ) */
	      while ((!feof(cpuinfo)) && fscanf(cpuinfo, "%as", &last)) {
		if (strncmp(last,"(",1) == 0) {
		   /* have the processor name: Niagara, Cheetah, etc */
		   processornickname = xstrdup(last);
                   goto MORETODO;
	        }
              }
	      processornickname = NULL;
            }
            prev =last;
         }
MORETODO:
         fclose(cpuinfo);
CONT:
         if(debug_driver_val & 0x01)
             fprintf(stdout,"NATIVE: %s\n",
		     processornickname ? processornickname : "NOT SPECIFIED");

         if (strcmp(argv[i], "-xarch=native") == 0) {
           /* for -xarch=target; only pass on -xarch  */
	   if (processornickname == NULL)
	     argv[i] = xstrdup("-xarch=v8plusa");
	   else if (strcmp(processornickname, "(SpitFire)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusa");
	   else if (strcmp(processornickname, "(BlackBird)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusa");
	   else if (strcmp(processornickname, "(Sabre)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusa");
	   else if (strcmp(processornickname, "(Hummingbird)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusa");
	   else if (strcmp(processornickname, "(Cheetah)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusb");
	   else if (strcmp(processornickname, "(Cheetah+)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusb");
	   else if (strcmp(processornickname, "(Jalapeno)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusb");
	   else if (strcmp(processornickname, "(Jaguar)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusb");
	   else if (strcmp(processornickname, "(Panther)") == 0)
               argv[i] = xstrdup("-xarch=v8plusb");
	   else if (strcmp(processornickname, "(Serrano)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusb");
	   else if (strcmp(processornickname, "(Niagara)") == 0)
	     argv[i] = xstrdup("-xarch=v8plus");
	   else if (strcmp(processornickname, "(Niagara2)") == 0)
	     argv[i] = xstrdup("-xarch=v8plusb");
           retval = 1;
         }
         else if (strcmp(argv[i], "-xarch=native64") == 0) {
           /* for -xarch=target; only pass on -xarch  */
	   if (processornickname == NULL)
 	     argv[i] = xstrdup("-xarch=v9a");
	   else if (strcmp(processornickname, "(SpitFire)") == 0)
	     argv[i] = xstrdup("-xarch=v9a");
	   else if (strcmp(processornickname, "(BlackBird)") == 0)
	     argv[i] = xstrdup("-xarch=v9a");
	   else if (strcmp(processornickname, "(Sabre)") == 0)
	     argv[i] = xstrdup("-xarch=v9a");
	   else if (strcmp(processornickname, "(Hummingbird)") == 0)
	     argv[i] = xstrdup("-xarch=v9a");
	   else if (strcmp(processornickname, "(Cheetah)") == 0)
	     argv[i] = xstrdup("-xarch=v9b");
	   else if (strcmp(processornickname, "(Cheetah+)") == 0)
	     argv[i] = xstrdup("-xarch=v9b");
	   else if (strcmp(processornickname, "(Jalapeno)") == 0)
	     argv[i] = xstrdup("-xarch=v9b");
	   else if (strcmp(processornickname, "(Jaguar)") == 0)
	     argv[i] = xstrdup("-xarch=v9b");
	   else if (strcmp(processornickname, "(Panther)") == 0)
               argv[i] = xstrdup("-xarch=v9b");
	   else if (strcmp(processornickname, "(Serrano)") == 0)
	     argv[i] = xstrdup("-xarch=v9b");
	   else if (strcmp(processornickname, "(Niagara)") == 0)
	     argv[i] = xstrdup("-xarch=v9");
	   else if (strcmp(processornickname, "(Niagara2)") == 0)
	     argv[i] = xstrdup("-xarch=v9b");
           retval = 1;
         }
         else {
	   if (processornickname == NULL)
	     argv[i] = xstrdup("-xtarget=generic");
	   else if (strcmp(processornickname, "(SpitFire)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra");
	   else if (strcmp(processornickname, "(BlackBird)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra2");
	   else if (strcmp(processornickname, "(Sabre)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra2i");
	   else if (strcmp(processornickname, "(Hummingbird)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra2e");
	   else if (strcmp(processornickname, "(Cheetah)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra3");
	   else if (strcmp(processornickname, "(Cheetah+)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra3cu");
	   else if (strcmp(processornickname, "(Jalapeno)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra3i");
	   else if (strcmp(processornickname, "(Jaguar)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra4");
	   else if (strcmp(processornickname, "(Panther)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra4plus");
	   else if (strcmp(processornickname, "(Serrano)") == 0)
	     argv[i] = xstrdup("-xtarget=ultra3iplus");
	   else if (strcmp(processornickname, "(Niagara)") == 0)
	     argv[i] = xstrdup("-xtarget=ultraT1"); 
	   else if (strcmp(processornickname, "(Niagara2)") == 0)
	     argv[i] = xstrdup("-xtarget=ultraT2"); 
         }
         if ((debug_driver_val & 0x01)) {
            fprintf(stdout, "TRANSLATE_NATIVE: argv[%d] = %s\n",i, argv[i]);
            fflush(stdout);
         }
         retval = 1;
      } 
    i++;
  } /* while (i < argc ) */
  return retval;
}
#else /* Solaris */
static  int
translate_native_options (int *argcp, char ***argvp)
{
  int argc = *argcp;
  char **argv = *argvp;
  int i,k, enditall;
  char xtarget_string[100];
  char fpver[3][100] = {"fpversion", "-foption"};
  int stdout_save = -1;
  int redir_handle; 
  FILE *redir_file;
  int pid;
  char *temp_filename;
  char *errmsg_fmt, *errmsg_arg;
  const char *string = xmalloc(sizeof(char *));
  const char **newv = xmalloc(3*sizeof(char *));
  int retval = 0;

  i = 0;
  while (i < argc) {
    if (strcmp(argv[i], "-xtarget=native" ) == 0 ||
        strcmp(argv[i], "-xtarget=native64") == 0 ||
        strcmp(argv[i], "-xarch=native64") == 0 ||
        strcmp(argv[i], "-xarch=native") == 0)
      {
        /* redirect stdout from fpversion */
        fflush(stdout);
        temp_filename = make_temp_file("fpversion"); 
        record_temp_file(temp_filename, 1, 1);
        string = find_a_file (&exec_prefixes, "fpversion", X_OK, 0);
        if (string)
          {
            newv[0] = string;
            newv[1] = xstrdup(fpver[1]);
            newv[2] = NULL;
            redir_handle = open(temp_filename, O_WRONLY | O_TRUNC | O_CREAT);
            stdout_save = dup (STDOUT_FILENO);
            dup2( redir_handle, STDOUT_FILENO); 
            pid = pexecute (string, (char *const *)newv, string, NULL, 
                            &errmsg_fmt, &errmsg_arg,
                            (PEXECUTE_FIRST | PEXECUTE_LAST | PEXECUTE_SEARCH ) );
            pid = pwait(pid, &k, 0);
            close (redir_handle);
            dup2( stdout_save, STDOUT_FILENO);
            redir_file = fopen(temp_filename, "r");
            if (redir_file == NULL)
              fprintf(stderr,"cant open fpversion temp file\n");
            fread(&xtarget_string[1], 99, 1, redir_file );
            fclose(redir_file); 
          }
        else
          {
            if (strcmp(argv[i], "-xarch=native") == 0)
              {
                notice ("could not find fpversion, setting -xarch=generic \n");
                argv[i] = xstrdup("-xarch=generic");
              }
            else if (strcmp(argv[i], "-xarch=native64") == 0)
              {
                notice ("could not find fpversion, setting -xarch=generic \n");
                argv[i] = xstrdup("-xarch=generic64");
              }
            else
              {
	        notice ("could not find fpversion, setting -xtarget=generic \n");
                argv[i] = xstrdup("-xtarget=generic");
              }
            retval = 1;
            goto skipit;
          }
        xtarget_string[0] = '-';
        k=1;
         enditall = strlen(xtarget_string)+1 ;
        enditall = enditall < 100 ? enditall : 100; /* min(enditall,100) */
        while (k<enditall)
          {
            if (xtarget_string[k] == ' ')
              {
                xtarget_string[k] = '\0';
                break;
              }
            k++; 
          }
        /* for -xarch=target; only pass on -xarch and not -xtarget */
        if (strcmp(argv[i], "-xarch=native") == 0)
          {
            if (strcmp(xtarget_string,"-xtarget=ultra") == 0 ||
                strcmp(xtarget_string,"-xtarget=ultra2") == 0 ||
                strcmp(xtarget_string,"-xtarget=ultra2i") == 0 ||
                strcmp(xtarget_string,"-xtarget=ultra2e") == 0 ||
                strcmp(xtarget_string,"-xtarget=gemini") == 0 )
              argv[i] = xstrdup("-xarch=v8plusa");
            else if (strcmp(xtarget_string,"-xtarget=ultra3") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra3cu") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra3i") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra4") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra4plus") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra3iplus") == 0 )
              argv[i] = xstrdup("-xarch=v8plusb");
            else if (strcmp(xtarget_string,"-xtarget=ultraT1") == 0 )
              argv[i] = xstrdup("-xarch=v8plus");
            else if (strcmp(xtarget_string,"-xtarget=ultraT2") == 0 )
              argv[i] = xstrdup("-xarch=v8plusb");
            else if (strcmp(xtarget_string,"-xtarget=ultraT2plus") == 0 )
              argv[i] = xstrdup("-xarch=sparcvis2");
            else if (strcmp(xtarget_string,"-xtarget=sparc64vii") == 0 )
              argv[i] = xstrdup("-xarch=v8plusd");
            else if (strcmp(xtarget_string,"-xtarget=sparc64vi") == 0 )
              argv[i] = xstrdup("-xarch=v8plusc");
            else
              argv[i] = xstrdup("-xarch=v8plusa");
            retval = 1;
          }
        else if (strcmp(argv[i], "-xarch=native64") == 0)
          {
            if (strcmp(xtarget_string,"-xtarget=ultra") == 0 ||
                strcmp(xtarget_string,"-xtarget=ultra2") == 0 ||
                strcmp(xtarget_string,"-xtarget=ultra2i") == 0 ||
                strcmp(xtarget_string,"-xtarget=ultra2e") == 0 ||
                strcmp(xtarget_string,"-xtarget=gemini") == 0 )
              argv[i] = xstrdup("-xarch=v9a");
            else if (strcmp(xtarget_string,"-xtarget=ultra3") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra3cu") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra3i") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra4") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra4plus") == 0 ||
                     strcmp(xtarget_string,"-xtarget=ultra3iplus") == 0 )
              argv[i] = xstrdup("-xarch=v9b");
            else if (strcmp(xtarget_string,"-xtarget=ultraT1") == 0 )
              argv[i] = xstrdup("-xarch=v9");
            else if (strcmp(xtarget_string,"-xtarget=ultraT2") == 0 )
              argv[i] = xstrdup("-xarch=v9b");
            else if (strcmp(xtarget_string,"-xtarget=ultraT2plus") == 0 )
              argv[i] = xstrdup("-xarch=v9b");
            else if (strcmp(xtarget_string,"-xtarget=sparc64vii") == 0 )
              argv[i] = xstrdup("-xarch=v9d");
            else if (strcmp(xtarget_string,"-xtarget=sparc64vi") == 0 )
              argv[i] = xstrdup("-xarch=v9c");
            else
              argv[i] = xstrdup("-xarch=v9a");
            retval = 1;
          }
        else
          {
            argv[i] = xstrdup(xtarget_string);
            retval = 1;
          }

skipit:
        if ((debug_driver_val & 0x01))
          {
            fprintf(stdout, "TRANSLATE_NATIVE: argv[%d] = %s\n",i, argv[i]);
            fflush(stdout);
          }
      }
    i++;
  } /* while (i < argc) */ 
  
  return retval;
}
#endif

/* If we are compiling pec_dummy then add -w to turn off warnings */
static const char *is_it_pec_dummy(int dummy __attribute__ ((unused)), 
                           const char** dummy2 __attribute__ ((unused)) ) {
   if (debug_driver_val & 0x08) 
       fprintf(stdout,"BASENAME=%s\n",
               input_basename ? input_basename : "NULL");
   if ((strcmp(input_basename,"pec_dummy.c") == 0) ||
       (strcmp(input_basename,"pec_dummy.cc") == 0) )
          return "-w";
   else return " ";
}


/* for -Wd,-pec and -Wd,-pec=<exe> we need the pass the original command line
   as a string.  However, it needs some modification.  The source file name
   is deleted and -c is added to the end */
static const char *print_orig_cmdline(int dummy __attribute__ ((unused)), 
                           const char** dummy2 __attribute__ ((unused)) ) {
  int cmdlinelen = 6;
  char *cmdlinestr;
  int i;
  char * p, * pn;
  int length;

  if (debug_driver_val & 0x01)
    for (i=1; i<orig_argc; i++)
      fprintf(stdout,"print_orig_cmdline: orig_argv[%d]=%s\n",
              i, orig_argv[i]);

  cmdlinelen += strlen(path_to_driver);
  for (i=1; i<orig_argc; i++) cmdlinelen += strlen(orig_argv[i]);

  cmdlinelen += orig_argc;

  cmdlinestr = xmalloc(cmdlinelen);
  cmdlinestr[0] = '"'; cmdlinestr[1] = 0;
  (void)strcat(cmdlinestr,path_to_driver); /* invocation line */
  (void)strcat(cmdlinestr," ");

  for (i=1; i<orig_argc; i++) 
    /* bring over the options except for the 
       -o <filename>, -###, -v and -xgccdriver=<path> options */
    if (orig_argv[i][0] == '-' && 
        !(strcmp(orig_argv[i],"-o") == 0  ||
          strcmp(orig_argv[i],"-v") == 0  ||
          strcmp(orig_argv[i],"--verbose") == 0  ||
          strncmp(orig_argv[i],"-###",4) == 0 ||
          strncmp(orig_argv[i],"-xgccdriver=",12) == 0) ) {
       /* '"' in argv shall be replaced by '\'' in case of cutting off whole argument. 
          e.g. -DENV="ONE TWO THREE" is transformed to -DENV='ONE TWO THREE'. */
      length = strlen (cmdlinestr);
      pn = p = orig_argv[i];
      while ((pn = strchr (p, '"')))
        {
           (void)strncat (cmdlinestr, p, pn - p); 
           length += pn - p;
           cmdlinestr[length ++] = '\'';
           cmdlinestr[length] = 0;
           p = pn + 1;
        }
      if (p != orig_argv[i])
        cmdlinestr[length] = 0;
      else /* no '"' be replaced. */
        (void)strcat(cmdlinestr,orig_argv[i]);

      (void)strcat(cmdlinestr," ");
    }

  (void)strcat(cmdlinestr, "-c"); 
  i = strlen(cmdlinestr); cmdlinestr[i] = '"'; cmdlinestr[i+1] = 0;

  if ((debug_driver_val & 0x01)) fprintf(stdout,"PRINT_ORIG_CMDLINE: %s\n",cmdlinestr);
  return cmdlinestr;
}

/* valid_backend_version:
   is this backend a version that can be used with gccfss?
   argument is the pathname to a Studio "prod" directory and includes "prod" in the path name
   1. use "version -v" to see what version it is
   2. based on the return string return TRUE or FALSE
 */
#ifdef __linux__
static  int
valid_backend_version (const char *sunstudioproddir)
{
  return 0; /* for now Studio add-on is not possible for Linux */
}
#else /* Solaris */
static  int
valid_backend_version (const char *sunwspro_dir)
{
  char ss_string[1000];
  int stdout_save = -1, stderr_save = -1;
  int redir_handle, redir_handle1; 
  FILE *redir_file;
  int pid, k, numread;
  char *temp_filename, *temp_filename1;
  char *errmsg_fmt, *errmsg_arg;
  const char **newv = xmalloc(3*sizeof(char *));
  int retval = 0;
  char *version_path;

  version_path = concat(sunwspro_dir, "/prod/bin/version", NULL);
  if (access_check(version_path, X_OK) == 0) 
    {
      /* redirect stdout/stderr from version */
      fflush (stdout); fflush (stderr);
      temp_filename = make_temp_file ("version"); 
      temp_filename1 = make_temp_file ("version1"); 
      record_temp_file (temp_filename, 1, 1);
      record_temp_file (temp_filename1, 1, 1);
      newv[0] = version_path;
      newv[1] = "-V";
      newv[2] = NULL;
      redir_handle = open (temp_filename, O_WRONLY | O_TRUNC | O_CREAT);
      stdout_save = dup (STDOUT_FILENO);
      dup2 (redir_handle, STDOUT_FILENO); 
      redir_handle1 = open (temp_filename1, O_WRONLY | O_TRUNC | O_CREAT);
      stderr_save = dup (STDERR_FILENO);
      dup2 (redir_handle1, STDERR_FILENO); 
      pid = pexecute (version_path,
                     (char *const *)newv, 
                     version_path, NULL, 
                     &errmsg_fmt, &errmsg_arg,
                     (PEXECUTE_FIRST | PEXECUTE_LAST | PEXECUTE_SEARCH ) );
      pid = pwait (pid, &k, 0);
      close (redir_handle);
      close (redir_handle1);
      dup2 ( stdout_save, STDOUT_FILENO);
      dup2 ( stderr_save, STDERR_FILENO);
      redir_file = fopen (temp_filename1, "r");
      if (redir_file == NULL) 
        {
          fclose (redir_file);
          fprintf (stderr,"cant open version temp file\n");
          return 0;
        }
      numread = fread (&ss_string, 1000, 1, redir_file );

      fclose (redir_file);

      retval = strstr (ss_string, "Sun Studio") != NULL ; 

      if (debug_driver_val & 0x01)
        {
          fprintf (stdout, "VALID_BACKEND_VERSION: retval=%d\n", retval);
          fprintf (stdout, "ss_string=%s\n", ss_string);
          fflush (stdout);
        }
    } /* version exists and is executable */
  else 
    {
      if (debug_driver_val & 0x01) 
        fprintf (stdout, 
                 "VALID_BACKEND_VERSION: executable version not found at %s\n",
                 version_path);
    }

  return retval;
}
#endif

static int
directory_exists (const char *path)
{
  struct stat st;
  int retval;

  retval = (stat (path, &st) >= 0 && S_ISDIR (st.st_mode));

  if ((debug_driver_val & 0x01)) 
    {
      fprintf(stdout, "DIRECTORY_EXISTS: %s retval=%d\n", 
              path != NULL ? path : "NULL", retval);
      fflush(stdout);
    }
  return retval;
}

/* before we start calling sunir backend, make sure we can find it.
 * If we can't find iropt then fatal error
 */
static const char *verify_sunir_backend(int dummy __attribute__ ((unused)), 
                           const char** dummy2 __attribute__ ((unused)) ) {

  /* see if sunir backend has been installed */
  if (find_executable_file("iropt") == NULL)
    fatal("SUNW0scgfss %s has not been installed. Either install it or use -frtl-backend.\n",compiler_version);
    
  return NULL;

}

static const char *add_user_il_routines (int dummy __attribute__ ((unused)),
                           const char** dummy2 __attribute__ ((unused)) )
{
  int i;
  int slen = 2;
  char *sret;

  if ((debug_driver_val & 0x01)) 
     fprintf(stdout,"IN ADD_USER_IL_ROUTINES with %d il files\n", n_ilfiles);
  

  for (i=0; i < n_ilfiles; i++)
    {
       slen += strlen(ilfiles[i].name) + 5;
    }

  sret = xmalloc(slen);
  sret[0] = ' ';
  sret[1] = 0;

  for (i=0; i < n_ilfiles; i++)
    {
      sret = concat(sret, " -il ", ilfiles[i].name, NULL);
    }

  if ((debug_driver_val & 0x01)) 
     fprintf(stdout,"IN ADD_USER_IL_ROUTINES returning %s\n", sret);

  return sret;

}

static int is_il_file ( char * name )
{
  int val;

  if (strlen(name) <= 3) val = 0; /* not .il suffix */
  else if (strcmp(name + strlen(name) - 3, ".il") == 0)
     val = 1;
  else
     val = 0;

  if ((debug_driver_val & 0x01)) 
     fprintf(stdout,"IN IS_IL_FILE with %s returning %d\n", name, val);

  return val;
}

/* %:print-asm-header spec function.  Print a banner to say that the
   following output is from the assembler.  */

static const char *
print_asm_header_spec_function (int arg ATTRIBUTE_UNUSED,
				const char **argv ATTRIBUTE_UNUSED)
{
  printf (_("Assembler options\n=================\n\n"));
  printf (_("Use \"-Wa,OPTION\" to pass \"OPTION\" to the assembler.\n\n"));
  fflush (stdout);
  return NULL;
}
