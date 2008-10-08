/*
 * Copyright 07/26/06 Sun Microsystems, Inc. All Rights Reserved
 *
 */

#ifndef _DBG_GEN_H
#define _DBG_GEN_H

/* FILE NOT I18N */

/* ________________________________________________________________________ */
/*
 *	Description:
 *		This is the Debug Generation Library Interface, the
 *		debug generation library is contained in the "lang"
 *		or compiler workspace where it is used.
 *
 *      Documentation:
 *		See the manual (lang/dbg_gen/src/manual.html) for detailed
 *		usage information.
 *
 *	WARNINGS TO USERS:
 *		1. DO NOT use the names of any of the fields in any of
 *		   the structs or unions defined in this file. They may
 *		   change!
 *		2. You can't use an older dbg_gen library when you have
 *		   compiled with a newer version of this file.
 *		   You can use a newer library, e.g. a release of
 *		   the C compiler built for and with a 3.0 dbg_gen,
 *                 could use a 4.0 dbg_gen library, but a C compiler built
 *		   for and with a 4.0 dbg_gen library cannot use a 3.0
 *		   dbg_gen library.
 *
 *      DBG_GEN Rules for Changing the Interface:
 * 		 1. DO NOT remove an interface that was used by any compiler
 *		    in the previous release. This version of dbg_gen should
 *		    be able to be used with the previous version of the
 *		    compilers.
 *		 2. DO NOT add arguments to an existing interface that was used
 *		    in this release, or the previous compiler release.
 *		 3. DO NOT change an enum constant that was used 
 *		    in this release, or the previous compiler release.
 *		 4. DO NOT change the size of any struct, union, or typedef
 *		    in this file.
 *		 5. DO add all interfaces to the appropriate place in the
 *		    mapfile for the library ("mapfile").
 *		 6. DO add a shim for all function interfaces in *_shims.c
 *		 7. DO add a FLAGCASE for all enum constants in *_debug.c
 *		 8. DO add an example set of calls to the testcase ex_tester.c
 *		 9. DO add documentation on the interface to the manual.
 *		10. DO "make" and "make test" on sparc-S2, intel-S2, sparcv9-S2
 *		11. DO fix ALL lint errors and compiler warnings
 *		12. DO update the DBG_GEN_VERSION number, add comments on
 *		    what changed, and make sure the version number reflects
 *		    what the change is. The version is Major.Minor.Micro.
 *		    Micro Change: minor internal code changes, minor additions
 *			          Example: bug fix, small interface addition
 *		    Minor Change: more extensive changes or strategy change
 *				  or change to the table format.
 *				  Example: many new interfaces, minor
 *					   compiler release, adding field to
 *					   any table written to disk.
 *		    Major Change: incompatible change, or major re-write
 *				  Example: old interfaces won't work the same,
 *				 	   major compiler release
 *		13. DO "make dbx_tests" on sparc-S2, intel-S2, sparcv9-S2
 *
 * NOTES: On compatability, if you want to maintain 100% binary compatibility:
 *	1. Never change a prototype in anyway, even the return value.
 *	2. Never re-order or change the enum constant values
 *	3. Never change the size of any structs passed by value
 *	4. Never remove an interface unless you KNOW it isn't used.
 *	5. Never tell a lie or kiss another man's wife.
 *
 *	Naming Conventions:
 *		All functions are part of the interface and should start 
 *			with "dbg".
 *		All interface macros/enums should start with "DBG".
 *		All interface typedefs should start with "Dbg".
 *
 *	Shims:
 *		Shim functions are provided for every function in the
 *		interface that modifies the state of the dbg_gen data
 *              or internal dbg_gen tables. You must use the Debug 
 *		Generation Library that was compiled with:
 *		     #define DBG_SHIMS or cc -DDBG_SHIMS ...
 *		This is true typically for the -g version of the library.
 *              [Look in the lib/debug directory of $(DESTDIR).]
 *		If you ARE using a -g version of this library you can
 *		obtain a shim logfile of all calls made to this interface
 *		by setting the environment variable __DBG_GEN_LOGFILE to
 *		the full pathname prefix which dbg_gen will suffix with the 
 *		pattern	"[.N].shims". The dbg_gen library will create these
 *		files, one per dbg_gen session (dbg_begin/dbg_end pair). 
 *              So as the compiler uses dbg_gen with a library that has
 *		shims enabled, you should see messages like the following:
 * 		   DBG_GEN: Shims in operation, output file is ...
 *		These shims log files are also C source files that can 
 *		be compiled, linked, and run to re-execute the same set of 
 *		dbg_gen calls made by the compiler. These files can also 
 *		be used to determine if calls have been made and/or the
 *		proper arguments were supplied.
 *		To compile and link a shims log file:
 *		  ws=/set/lang/work/lang   (Or a built lang workspace)
 *		  bl=/set/lang/sparc-S2/lib/debug (Or built library loc)
 *		  cp YOUR.shims shims.c
 *		  cc -I${ws}/dbg_gen/src -L${bl} -R${bl} -l_I_dbg_gen shims.c
 *		Excecution of the resulting a.out should result in stdout
 *		printing that should reflect the stabs that were sent to the
 *		compilers sidedoor file. These "fake" !SDF functions are
 *              contained in the library for shims use only.
 *              
 *  	Compilers:
 *		The compilers have have dbg_gen controls available via the
 *		internal compiler frontend option -xdbggen.
 *
 __________________________________________________________________________ */

/* ________________________________________________________________________

 Version History:
 
 BEGIN RELEASE NOTES:

	4.0.*			- Krakatoa versions of dbg_gen
	------------------------------------------------------------------
	4.1.0 .. 4.14.15	- K2 versions of dbg_gen
	------------------------------------------------------------------
	4.14.16 .. 4.14.28 	- mercury versions of dbg_gen
	------------------------------------------------------------------
	4.14.29 .. 4.14.30	- vulcan versions of dbg_gen
	------------------------------------------------------------------
	4.14.31		- Functions for reading debug info.
	5.0.0		- DbgSymFlags => 64 bits; redefine overloaded values
        5.0.1		- new symflag DBG_AMD64_PARMDUMP 6223034
	5.0.2		- dbg_type_forward_bind to help us omit unused type
			  info.  Needed to reduce dwarf info.
        5.0.3           - add dbg_func_hwcprof_signature, and dwarf generation
                        - Also many fixes related to C++/dwarf.
			- add DBG_AMD64_PARMDUMP for OS group (6223034)
        		- add dbg_type_forward_bind so C can tell us about 
			  struct resolution.  This helps us implement usedonly.
        5.0.4		- add functions so variable array bounds can be
                          queried and updated.  Moved DbgValueKind to
                          dbg_gen.h so it is public. 
        5.0.5           - fix for outlined functions.
        5.0.6		- better dwarf/c++ support
        5.0.7		- fix 5025010: invalid type for LCSYM/STSYM stab
        5.0.8           - fixes for clone functions and dw anon unions C++
                        - nobugid - related to problems in 6280899 
                          (anonymous unions C++/dwarf)
                        - 6252312  cg: assertion failed in file 
                          ../src/asm+/dbg_gen_interface.cc ...
                        - 5012903 cloned functions should get N_FUN stabs
        5.1.0           - Added code to retrieve string attributes 
                          information of a debug sections from libdwarf.
                        - Added code to emit string data in ascii format
                          into assembly files.
        5.1.1           - fix 6336699: Emit DW_LNE_end_sequence to mark
                          end of line number information of a function.                          
	5.1.2		- misc fixes (see ../Integration.log)
	5.1.3		- dbg_type_array_vla_smax
	                  fix 6194057: "(noname)"
	5.1.4           - C++ and Fortran fixes.  See Integration.log
	5.1.5		- Remove macro stabs for now, causing trouble in dbx
	5.1.6		- fix shims formatting bug, and x86/comdat/reloc bug
	5.1.7		- fix 6385985: can't print VLAs
        5.1.8           - DW_OP_piece support. 
                        - dbg_bind_reg_kind.
	5.1.9		- 6386953 dwarf support for f90 
			  allocatable/assume-shape-array/f90-pointer
	5.1.10          - 6402529 (offsets wrong for unaligned fields)
        5.1.11		- Support for GNU demangling.      
			- Artificial functions. 
  	5.1.12		- 6407933 comdat local label references outside group
			- 6408741 dbggen asserts on C++ template code ...
			- 6406898 10x compile-time performance regression ...
			- nobugid Add better libdwarf error reporting
			- nobugid memory read of unintialized data ...
	5.1.13		- 6383277 support compile sessions with no tables
	5.1.14		- 6407933 comdat local label references outside group
        5.1.15          - Dwarf class method declarations and definition.
                        - GNU linkage name of globals/static variables.   
        5.1.16          - dbg_type_class_gnu_containing_type.                        
        5.1.17          - 6331946 Fix filenames formation for DW_AT_decl_file 
        5.1.18          - 6449128 Fix infinite recursion in symbol recycling.
        5.1.19          - Changes for sparc-Linux port.
 END RELEASE NOTES:
  _________________________________________________________________________ */

#define DBG_GEN_VERSION_NUMBER 5.1.19	/* dbg_gen version number */
#define DBG__STRINGIZE1(s) #s
#define DBG__STRINGIZE2(s) DBG__STRINGIZE1(s)
#define DBG_GEN_VERSION DBG__STRINGIZE2(DBG_GEN_VERSION_NUMBER)

/* This is a C interface */

#ifdef __cplusplus
    extern          "C" {
#endif

#include <time.h>		/* To get time_t */

/* -----------------------------------------------------------------------
 *
 * Interface typedefs and structs needed:
 *
 */

/* The basic char* type used in the interface */

typedef const char * DbgString;

/* The basic integer types. */

typedef int     		DbgSint32;
typedef unsigned int 		DbgUint32;
typedef long long 		DbgSint64;
typedef unsigned long long 	DbgUint64;

/* Interface mappings of integral things to the above integer types */

typedef DbgUint32 	DbgIntIndex;	/* Index into a table */
typedef DbgUint32 	DbgRegNum;      /* Register number */

/* These are defined in terms of target Address quantities */

typedef DbgSint32 DbgSmallByteOff;	/* Small Byte offset in memory  */
typedef DbgSint32 DbgSmallBytes;	/* Byte count or size in bytes */
typedef DbgSint32 DbgSmallBitOffset;	/* Bit offset into record */
typedef DbgUint32 DbgSmallBitSize;	/* Bit size of field */
typedef DbgSint64 DbgLargeByteOff;	/* Large Byte offset in memory  */
typedef DbgSint64 DbgLargeBytes;	/* Byte count or size in bytes */
typedef DbgSint64 DbgLargeBitOffset;	/* Bit offset into record */
typedef DbgUint64 DbgLargeBitSize;	/* Bit size of field */
typedef DbgSint64 DbgValueInt;		/* Range min/max, enum values, etc. */
typedef DbgSint64 DbgCommByteOff;	/* COMMON data offset (64-bit) */
typedef DbgUint32 DbgSmallUnsigned;     /* Unsigned integer */

typedef DbgLargeByteOff DbgStackOffset;
typedef DbgSmallByteOff DbgArgOffset;

/* These are defined the same for both 64bit and 32bit OS. */

typedef DbgUint32 DbgIntLine;		/* Line number */
typedef DbgUint32 DbgIntLineOffset;	/* Byte offset from start of line */
typedef DbgSint32 DbgVtVersion;		/* Virtual table lookup version */
typedef DbgLargeByteOff DbgVtOffset;	/* Virtual table byte offset */
typedef DbgUint32 DbgVtIndex;		/* Virtual table index */
typedef DbgSint32 DbgDestState;		/* Destructor state or state mod */

/* Various ID handles returned by the interface for files, scopes, types,
 *      lines, and symbols 
 */

typedef DbgIntIndex DbgFileID;	/* Index into file table */
typedef DbgIntIndex DbgScopeID;	/* Index into scope table */
typedef struct DbgTypeID {
    DbgFileID       file_id;
    DbgIntIndex     type_no;	/* Index into file's type table */
} DbgTypeID;
typedef struct DbgSymID {
    DbgScopeID      scope_id;
    DbgIntIndex     sym_no;	/* Index into scope's sym table */
} DbgSymID;
typedef DbgIntIndex DbgAuxID;
typedef DbgIntIndex DbgStrID;
typedef DbgIntIndex DbgLineID;

#define DBG_MAKE_TYPE(id,fi,no) ((id).file_id=(fi),(id).type_no=(no),(id)) 
#define DBG_MAKE_SYM(id,sc,no) ((id).scope_id=(sc),(id).sym_no=(no),(id)) 

/* Various bit flags needed when generating symbols of any kind.
 *	All flags are unique bits to allow for most combinations
 *	of flags, even though each function that accepts a flags
 *	argument will only accept a particular subset of these flags.
 */

#define	DBG_INITIALIZED     (1ULL << 0)  /* initial value storage (.data) */
#define	DBG_EXTERN 	    (1ULL << 1)  /* a global symbol */
#define	DBG_BY_VALUE 	    (1ULL << 2)  /* paramter pass by value */
#define	DBG_BY_REF 	    (1ULL << 3)  /* parameter pass by reference */
#define	DBG_FUNC_REF 	    (1ULL << 4)  /* parameter function reference */
#define	DBG_FUNC_RESULT     (1ULL << 5)  /* function result name */
#define	DBG_STACK 	    (1ULL << 6)  /* runtime stack storage */
#define	DBG_STATIC 	    (1ULL << 7)  /* local static storage */
#define	DBG_FILE_STATIC     (1ULL << 8)  /* file or global static storage */
#define	DBG_INLINE 	    (1ULL << 9)  /* inline function */
#define	DBG_INTERNAL 	    (1ULL << 10) /* f90 internal subroutine */
#define	DBG_LITERAL 	    (1ULL << 11) /* C++ literal */
#define	DBG_READONLY 	    (1ULL << 12) /* read-only storage (.rodata) */
#define	DBG_SYM_VISIBLE     (1ULL << 13) /* Visible C++ member of class */
#define	DBG_CONSTANT 	    (1ULL << 14) /* f90 parameter */
#define	DBG_BASED 	    (1ULL << 15) /* Fortran "based" var/formal */
#define	DBG_SYNTHETIC 	    (1ULL << 16) /* Symbol created by compiler */
#define	DBG_SYM_COMDAT	    (1ULL << 17) /* Comdat function */
#define	DBG_MAIN 	    (1ULL << 18) /* "main" or fortran "MAIN" */
#define	DBG_SYM_OMP_TPRIV   (1ULL << 19) /* OpenMP Thread Private var */
#define	DBG_SYM_FRAG	    (1ULL << 20) /* Fragmented data */
#define	DBG_AUTOMATIC 	    (1ULL << 21) /* f90 automatic array */
#define	DBG_F90PURE 	    (1ULL << 22) /* f90 pure function */
#define	DBG_F90ELEMENTAL    (1ULL << 23) /* f90 elemental function */
#define	DBG_F90RECURSIVE    (1ULL << 24) /* f90 recursive function */
#define	DBG_USE_ONLY 	    (1ULL << 25) /* f90 use module "only" clause */
#define	DBG_EXTERN_UNDEF    (1ULL << 26) /* Undefined extern data */
#define	DBG_SYM_ANON	    (1ULL << 27) /* Anon union variable */
#define	DBG_SYM_DIFFMBR	    (1ULL << 28) /* Differentially mangled name */
#define	DBG_FUNC_DEF	    (1ULL << 29) /* C++ only usage */
#define	DBG_FUNC_DECL	    (1ULL << 30) /* C++ only usage */
#define	DBG_OMP_BY_REF	    (1ULL << 31) /* OPENMP by reference variable */
#define	DBG_SYM_TLS	    (1ULL << 32) /* Thread Local Storage */
#define DBG_AMD64_PARMDUMP  (1ULL << 33) /* amd64 reg parms copied to stack */

typedef unsigned long long DbgSymFlags;

/* Enumeration values for the language settings. */

typedef enum {
	DBG_LANG_NONE 		= 0,
	DBG_LANG_AS 		= (1 << 0),
	DBG_LANG_ANSI_C 	= (1 << 1),
	DBG_LANG_CC 		= (1 << 2), /* C++, Not Used */
	DBG_LANG_ANSI_CC 	= (1 << 3), /* C++ */
	DBG_LANG_F77 		= (1 << 4),
	DBG_LANG_F90 		= (1 << 5),
	DBG_LANG_PASCAL		= (1 << 6), /* Unused */
	DBG_LANG_JAVA 		= (1 << 7), /* NJC (Native Java Compiler) */
	DBG_LANG_C99 		= (1 << 9)  /* C99 */
} DbgLang;

/* To convert dbg_gen 1.4 ascii language names to the DbgLang enum */

DbgLang 	dbg_lang(DbgString);

/* Enumeration values for file flags. */

enum {
	DBG_FILE_SOURCE 	= (1 << 0),	/* source file */
	DBG_FILE_INCLUDE 	= (1 << 1),	/* #include named files */
	DBG_FILE_REFERENCE 	= (1 << 3), 	/* #line files */
	_DBG_FILE_Width		= 8
};
typedef unsigned DbgFileFlags;

/* NULL values for DbgTypeID, DbgFileID,...*/

DbgFileID 	dbg_get_null_file(void);
DbgScopeID 	dbg_get_null_scope(void);
DbgTypeID 	dbg_get_null_type(void);
DbgSymID 	dbg_get_null_sym(void);

/* Just to be compatable with past releases. */

#define DBG_NULL_FILE 	dbg_get_null_file()
#define DBG_NULL_SCOPE	dbg_get_null_scope()
#define DBG_NULL_TYPE	dbg_get_null_type()
#define DBG_NULL_SYM	dbg_get_null_sym()
#define DBG_NULL_VALUE	dbg_get_null_value()

/* Used for dbg_func_hwcprof_signature() */

#define DBG_NULL_HWC_SIG 0

/* Null initializers for these types */

#define DBG_NULL_FILE_INIT 	0
#define DBG_NULL_SCOPE_INIT 	0
#define DBG_NULL_TYPE_INIT 	{0,0}
#define DBG_NULL_SYM_INIT 	{0,0}
#define DBG_NULL_VALUE_INIT 	{{0},0}
#define DBG_NULL_OREF_INIT 	{0,0,0}

/* Checking for NULL handles */

int dbg_null_file(DbgFileID);
int dbg_null_scope(DbgScopeID);
int dbg_null_type(DbgTypeID);
int dbg_null_sym(DbgSymID);

/* Comparing handles */
    
int dbg_file_equal(DbgFileID, DbgFileID);
int dbg_scope_equal(DbgScopeID, DbgScopeID);
int dbg_type_equal(DbgTypeID, DbgTypeID);
int dbg_sym_equal(DbgSymID, DbgSymID);

/* Macro versions of above */

#define DBG_FILE_EQUAL(f1,f2)       ((f1)==(f2))
#define DBG_SCOPE_EQUAL(s1,s2)      ((s1)==(s2))
#define DBG_SYM_EQUAL(s1,s2) ( DBG_SCOPE_EQUAL((s1).scope_id,(s2).scope_id) \
			&& (s1).sym_no==(s2).sym_no )
#define DBG_TYPE_EQUAL(t1,t2) ( DBG_FILE_EQUAL((t1).file_id,(t2).file_id) \
			&& (t1).type_no==(t2).type_no )

#define DBG_FILE_IS_NULL(f)         ((f)==0)
#define DBG_SCOPE_IS_NULL(s)        ((s)==0)
#define DBG_SYM_IS_NULL(s)          ( DBG_SCOPE_IS_NULL((s).scope_id) \
			&& (s).sym_no==0)
#define DBG_TYPE_IS_NULL(t)         ( DBG_FILE_IS_NULL((t).file_id)  \
			&& (t).type_no==0)


/* Line information */

typedef struct {
	DbgFileID       file_id;
	DbgIntLine      line;
	DbgIntLineOffset col;
} DbgLine;

/* convert register name to register number */
    
DbgRegNum dbg_regname(DbgString);

/* The DbgObjectRef structure is returned by dbg_object_ref() and is
 *   used to hold all the object file reference information needed to
 *   fill in the stab 'value' field or to locate a debugging object
 *   using the object file symbolics (linker visible symbols).
 *   This object reference serves as the "binding" of a debug handle or
 *   piece of information to the actual objects known about by the linker.
 *   They can represent both "text" and "data" addresses so they apply
 *   equally to data objects and to instruction locations.
 *
 *   The base_label (if set) is the name of a global linker symbol.
 *   The label (if set) is the name of a normally local linker symbol, but
 *   it could be global also.
 *   The offset is a signed value that will be used to find the final
 *   byte offset for this object reference.
 *
 *   There are 3 possible combinations here:
 *	(base_label==NULL, label==NULL, offset)
 *		This is simple a constant value for a stab 'value' field.
 *		This normally relates to an offset from the beginning of the
 *		current function being defined, or a simple constant in
 *		the value field of a stab with no objectfile reference.
 *	(base_label=="lname", label==NULL, offset)
 *		This means that the debugging reference is to the object
 *		(or linker symbol or assembler label) "lname" plus the
 *		byte offset 'offset'. Normally this form shouldn't be
 *		used except to refer to the entire object itself, like a
 *		common block name or a function name. Normally the offset is
 *		0. With Solaris 2.X, this usually translates into just
 *		the offset in the stab value field
 *	        since Solaris 2.X never have actual linker relocation
 *		records relating to the debug sections.
 *		A common use for this would be for the object reference to a
 *		static variable that has been globalized:
 *			("$X.........foobar",NULL,0)
 *		or a function:
 *			("function_name",NULL,0).
 *              In fortran equivalence statements, the fortran compiler
 *              will use a non-zero offset with a linker symbol to describe
 *              the starting location for a variable.
 *	(base_label=="lname", label=="label", offset)
 *		This means that the debugging reference is constant but
 *		is calculated by subtracting the address of the object
 *		(or linker symbol or assembler label) "lname" from the
 *		object "label" and adding 'offset'.
 *		For source line object references (text segment refs)
 *		normally take the form:
 *		    ("function_name","local_line_label",0).
 *		All non-globalized data MUST use the base_label of Ddata.data,
 *		Bbss.bss, Rrodata.rodata, Ttdata.data, or Ttbss.bss
 *		e.g. use the form:
 *		   ("Ddata.data","foobar",0) or
 *		   ("Bbss.bss","foobar",0) or
 *		   ("Rrodata.rodata","foobar",0).
 *		   ("Ttdata.data","foobar",0).
 *		   ("Ttbss.bss","foobar",0).
 *		NOTE: Globalized data coming in as:
 *		   ("Ddata.data","$X.........foobar",0) will be treated as
 *		   ("$X.........foobar",NULL,0).
 *
 *   See the dbg_bind_*() functions for more information on how the
 *	binding between debug info and linker objects work.
 *
 */

typedef struct {
	DbgStrID 	base_label;
	DbgStrID 	label;
	DbgLargeByteOff offset;
} DbgObjectRef;

DbgObjectRef    dbg_object_ref(DbgString, DbgString, DbgLargeByteOff);

/* To get the "const char*" for a DbgStrID */

DbgString dbg_get_string(DbgStrID);

/* DbgCodeLoc's are used to describe a range of instructions.
 *   	These may be used by themselves to describe where source lines are
 *	or used in conjunction with data locations when identifying the
 *	lifetime or range of certain data locations for a variable.
 *	In the past a simple DbgObjectRef was used and the actual range
 *	would be assumed from the DbgObjectRef of the next equivalent object.
 *	This DbgCodeLoc is more explicit.
 *	If nbytes==0, the implication is 1 instruction, or up-to the next
 *	equivalent entry.
 */

typedef struct {
	DbgObjectRef    oref;
	DbgLargeBytes   nbytes;
} DbgCodeLoc;

DbgCodeLoc dbg_code_loc(DbgObjectRef, DbgLargeBytes);

/* Different kinds of registers for holding data. */
typedef enum {
    DBG_REG_SINGLE,
    DBG_REG_PAIR
} DbgRegKind;

/* A simple constant value, value from stack, argument value, or register.
 *	These are used in several situations, the obvious in defining the
 *	data value of a variable, and also in describing where to find
 *	the range minimums or maximums for arrays.
 */

typedef enum {
        DBG_VAL_UNKNOWN = 0,    /* No known value */
        DBG_VAL_INT     = 1,    /* Simple integer constant value */
        DBG_VAL_STACK   = 2,    /* Location on the stack has the value */
        DBG_VAL_ARG     = 3,    /* Location in the passed argument area */
        DBG_VAL_REG     = 4,    /* The contents of a register */
        DBG_VAL_OREF    = 5,    /* An object data area has the value */
        DBG_VAL_DATA    = 6,    /* A block constant value */
        DBG_VAL_SIZE    = 7     /* Location on the stack has the size */
} DbgValueKind;

typedef struct {
    union {
	    DbgValueInt     i;
	    DbgStackOffset  stack_offset;
	    DbgArgOffset    arg_offset;
	    DbgRegNum       regnum;
	    DbgObjectRef    oref;
	    struct {
		DbgAuxID        data;	/* constant data */
		DbgLargeBytes   nbytes;	/* # of bytes */
	    }               data;
    }               variant;
    unsigned char kind;
} DbgValue;

DbgValue 	dbg_get_null_value(void);
int 		dbg_null_value(DbgValue);
DbgValue 	dbg_value_unknown(void);
DbgValue 	dbg_value_int(DbgValueInt);
DbgValue 	dbg_value_stack(DbgStackOffset);
DbgValue 	dbg_value_size(DbgStackOffset);
DbgValue 	dbg_value_arg(DbgArgOffset);
DbgValue 	dbg_value_reg(DbgRegNum);
DbgValue 	dbg_value_oref(DbgObjectRef);
DbgValue 	dbg_value_data(void *, DbgLargeBytes);

/* DbgDataLoc's are used to convey the location or literal value of a
 *    variable, array bounds, formal, etc.
 *    They include a PC range where the value or address is valid, and
 *    that address or value.
 */

typedef struct {
	DbgCodeLoc      cloc;
	DbgValue        value;
}               DbgDataLoc;

DbgDataLoc dbg_data_loc(DbgCodeLoc, DbgValue);

/* -----------------------------------------------------------------------
 *
 * Interface functions:
 *
 */

/* Functions for library startup and shutdown.
 *	The very first call should be dbg_begin(), the very last dbg_end().
 */

#define DBG_STABS_MINIMUM DBG_80_STABS /* Mimimum Stabs version */
#define DBG_STABS_LATEST  DBG_100_STABS /* Used by compiler frontends */

enum {
	DBG_ELF_32 		= (1 << 0),
	DBG_60_STABS 		= (1 << 1),  /* DO NOT USE: WS6 */
	DBG_TEXT_PLANML		= (1 << 2),
	DBG_ELF_64 		= (1 << 3),
	DBG_OPTIMIZED 		= (1 << 4),
	DBG_SDF_STABS 		= (1 << 5),
	DBG_COMDAT 		= (1 << 6),
	DBG_STATS 		= (1 << 7),
#define DBG_STABS 		DBG_SDF_STABS
	DBG_INDEX_ONLY 		= (1 << 8),
	DBG_LINKER_NAMES 	= (1 << 9),  /* Triggers DBG_80_STABS too */
	DBG_70_STABS 		= (1 << 10), /* DO NOT USE: Lionel or WS6U1 */
	DBG_SINGLE_TYPE_NUMBERS = (1 << 11), /* Default with version 3.0 */
	DBG_DOUBLE_TYPE_NUMBERS = (1 << 12),
	DBG_80_STABS 		= (1 << 13), /* Nozomi WS6U2 */
	DBG_FUNC_INDEX 		= (1 << 14),
	DBG_90_STABS 		= (1 << 15), /* Krakatoa S1S 7 CC */
	DBG_USED_ONLY 		= (1 << 16),
	DBG_DIFF_MANGLING	= (1 << 17),
	DBG_BINARY_BIDS		= (1 << 18),
	DBG_BINARY_TABLES	= (1 << 19),
	DBG_BINARY_DWARF2	= (1 << 20) ,
	DBG_100_STABS		= (1 << 21),  /* K2 S1S 8 CC */
	DBG_BACKEND_GENERATION  = (1 << 22),  /* Backends generate debug info */
	DBG_STABS_INCL          = (1 << 23),  /* No .stab.excl, -xs used */
	DBG_HWCPROF             = (1 << 24),
	DBG_COMPAT_4            = (1 << 25),  /* C++ with -compat=4 */
	DBG_DWARF_COMDAT_HEADERS =(1 << 26),
        DBG_GNU_FRONT_END        =(1 << 27),
	_DBG_RETIRED_OUTFLAG	= (int)(1U << 31)
};
typedef unsigned DbgOutFlags;

DbgString dbg_gen_version(void);
int    dbg_check_version(DbgString);
void * dbg_set_state(void *);
size_t dbg_get_state_length(void);
typedef void (*DbgErrorFptr)(DbgString, DbgIntLine, DbgString);
void   dbg_begin(DbgString, DbgErrorFptr, DbgOutFlags);
void   dbg_end(void);
void   dbg_log_comment(DbgString, ...);

/* Functions that provide control to the created debug information.
 *	The debug information format is currently "stabs" and those stabs
 *	are sent back into the compiler backend through the !SDF interface.
 *	The dbg_sdf_interface() call is made to inform dbg_gen of the
 *	addresses of these !SDF functions and need only be called once.
 *	The function dbg_sdf_write_debug() is used to trigger all the
 *	calls to the !SDF functions once a file has been completely
 *	described through the dbg_gen interface and dbg_file_end() was
 *	called on that file_id. This must be the file_id of a .o related
 *	file and NOT a #include file.
 */

#ifdef __sparcv9
    #define SdfHandle DbgSint64	/* See !SDF as to what this should be */
#else
    #define SdfHandle DbgSint32	/* See !SDF as to what this should be */
#endif

typedef void (*sdStabnD_t) (SdfHandle, int, int, int, const char *,
			 const char *, int);
typedef void (*sdStabnN_t) (SdfHandle, int, int, int, int);
typedef void (*sdStabsD_t) (SdfHandle, const char *, int, int, int,
			 const char *, const char *, int);
typedef void (*sdStabsDD_t) (SdfHandle, const char *, int, int, const char *,
			 const char *, int, const char *, const char *, int);
typedef void (*sdStabsDR_t) (SdfHandle, const char *, int, int, const char *,
			 const char *, int, const char *, int);
typedef void (*sdStabsN_t) (SdfHandle, const char *, int, int, int, int);
typedef void (*sdStabsR_t) (SdfHandle, const char *, int, int, int,
			 const char *, int);
typedef void (*sdXstabsN_t) (SdfHandle, const char *, const char *, int, int,
			 int, int);
typedef void (*sdPushSection_t) (SdfHandle, const char *);
typedef void (*sdPopSection_t) (SdfHandle);
typedef void (*sdXCstabsN_t) (SdfHandle, const char*, const char*, int,
			int, int, int);
typedef void (*sdByte_t) (SdfHandle, int);
typedef void (*sdLabel_t) (SdfHandle, const char *);
typedef void (*sdWordR_t) (SdfHandle, const char *, int);
typedef void (*sdUaWordR_t) (SdfHandle, const char *, int);
typedef void (*sdUaWordRX_t) (SdfHandle, const char *, long long);
typedef void (*sdAlign_t) (SdfHandle, int);
typedef int  (*sdLabelDiff_t) (SdfHandle, const char *, const char *, int *);	

typedef enum 
{
    DBG_COMDAT_SECTION_FLAG 	= 0x1, /* Section is comdat */
    DBG_STR_SECTION_FLAG 	= 0x2  /* Section is a string section */
} sdSectionFlags_t;

typedef enum
{
    DBG_SD_RELOC_LABEL,     /* Simple relocation record plus label_offset */
    DBG_SD_RELOC_LABEL_SUB, /* Subtraction of labels label2-label1+label_offset */
    DBG_SD_RELOC_SECT,      /* Relocation of beginning of section name in label1 */
    DBG_SD_RELOC_SECT_END,  /* Relocation of end of section name in label1 */
    DBG_SD_RELOC_LABEL2     /* Used internally only, See dbg_sdf.c */
} sdRelocKind_t;

typedef struct {
        sdRelocKind_t   kind;
        char            *label1;      /* Main label or section name */
        char            *label2;      /* Only used by SD_RELOC_LABEL_SUB */
				      /* and by DBG_SD_RELOC_LABEL2 */
        int             label_offset; /* Always add this to label address */
        int             byte_offset;  /* Location in the current buffer */
        unsigned        reloc_size;   /* Relocation size */
} sdRelocationInfo_t;

typedef struct {
    unsigned long long offset;
    unsigned long long nbytes;
} sdStringAttrib_t;

    
/* Warning:
 * I don't know if these symbols should be:
 * 1) STB_GLOBAL/LOCAL/WEAK
 * 2) STV_INTERNAL/HIDDEN/PROTECTED
 * 3) STT_OBJECT or something else
 * We'll have to figure it out as we go.
 * I think comdat processing prevents using STB_LOCAL
 * Try STB_GLOBAL, STV_DEFAULT, STT_OBJECT for now.
 */

typedef struct {
    const char * name;	       /* st_name: symbol to be added to main symtab */
    unsigned long long value;  /* st_value: needs to account for 64-bit values */
    /* add new fields here: */
    /* ... */
    char global;    /* ==1 means global sym ==0 means local symbol */
} sdSymInfo_t;    

typedef struct {
    unsigned char *bytes;	  /* data bytes for this chunk of section data */
    int nbytes;			  /* count of data bytes in this chunk */
} sdSectionData_t;

/* sdSectionSpec_t */
typedef struct {
    const char * secname;   /* sh_name: the name of the section */
    const char * link_name; /* section whose index goes in sh_link, or NULL */
    int entry_size;
    int align;
    sdSectionFlags_t flags; /* COMDAT or STRTAB */
    sdSectionData_t * blocks; /* pointers to blocks of section data */
    int nblocks;	    /* num of pointers in the blocks list */
    sdSymInfo_t ** syms;    /* new symbols which point into this section */
    int nsyms;		    /* number of pointers in syms list */
    sdRelocationInfo_t * relocs;  /* ALL relocs for this section. */
    int nrelocs;
    sdStringAttrib_t *string_attribs; /* ALL string attribs in section. */
    int n_string_attribs;
    /* add new fields here: */
    /* ... */
} sdSectionSpec_t;


/* old interfaces */
typedef void (*sdWriteSectionBuffer_t) (SdfHandle, const char *, 
			unsigned char *, int, sdRelocationInfo_t *, int);
typedef void (*sdSectionInfo_t) (SdfHandle, const char *, 
			const char *, int, int, unsigned);

/* new interface */
typedef void (*sdCreateSection_t) (SdfHandle, sdSectionSpec_t *);

void dbg_sdf_interface(sdStabnD_t, sdStabnN_t, sdStabsD_t, sdStabsDD_t,
		       sdStabsDR_t, sdStabsN_t, sdStabsR_t, sdXstabsN_t,
		       sdPushSection_t, sdPopSection_t);
void dbg_sdf_write_debug(DbgFileID, SdfHandle);

typedef struct {
	DbgString	options;
	int		gflag;
	int		opt_level;
	int		fragmentflag;
	int		v9_flag;
	int		pic_flag;
	DbgString	da_glob_prefix;
	DbgString	cpl_rel;
        unsigned 	filler[12];
} DbgOptionsInput;

int dbg_xdbggen(DbgString, DbgOutFlags *);

int dbg_options(DbgOptionsInput,
	    DbgOutFlags *poutflags, DbgString optstring, size_t optlen);

typedef struct {
	DbgOutFlags	    outflags;
	SdfHandle           sdf_handle;
	sdStabnD_t          sdStabnD;
	sdStabnN_t          sdStabnN;
	sdStabsD_t          sdStabsD;
	sdStabsDD_t         sdStabsDD;
	sdStabsDR_t         sdStabsDR;
	sdStabsN_t          sdStabsN;
	sdStabsR_t          sdStabsR;
	sdXstabsN_t         sdXstabsN;
	sdPushSection_t     sdPushSection;
	sdPopSection_t      sdPopSection;
	sdXCstabsN_t        sdXCstabsN;
	sdByte_t	    sdByte;
	sdLabel_t	    sdLabel;
	sdWordR_t	    sdWordR;
	sdUaWordR_t	    sdUaWordR;
	sdAlign_t	    sdAlign;
	sdWriteSectionBuffer_t	    sdWriteSectionBuffer;
	sdUaWordRX_t	    sdUaWordRX;
	sdSectionInfo_t	    sdSectionInfo;
        sdLabelDiff_t       sdLabelDiff;
        sdCreateSection_t   sdCreateSection;
        void * 	            filler[2];
} DbgWriteInput;

void dbg_write(DbgFileID, DbgWriteInput);

/* New style routines for writing out various kinds of information. */

typedef void (*DbgWriteFptr)(unsigned char *, size_t len, void*);
typedef void (*DbgReadFptr)(unsigned char *, size_t len, void*);

int dbg_tables_loaded(void);
void dbg_write_tables_binary(DbgWriteFptr, void*);
void dbg_read_tables_binary(DbgReadFptr, DbgErrorFptr, void*);
void dbg_write_stabs_sdf(DbgFileID, SdfHandle,
		sdStabnD_t, sdStabnN_t, sdStabsD_t, sdStabsDD_t,
	        sdStabsDR_t, sdStabsN_t, sdStabsR_t, sdXstabsN_t,
	        sdPushSection_t, sdPopSection_t);

void dbg_contributor_info(DbgString name, DbgString comment);

size_t dbg_write_tables(FILE *);
size_t dbg_read_tables(FILE*, DbgErrorFptr);

void dbg_read_dwarf(void);

void dbg_write_assembly(DbgFileID, DbgOutFlags, DbgWriteFptr, void*);

/* Functions to begin and end file processing.
 *	These functions should be used to describe the actual source files
 *	read in by the compiler. Usually in the order the compiler has
 *	seen the files. Note that each #include file that you would want
 *	separately stored debug information would need a unique DbgFileID.
 */

DbgFileID dbg_file_begin(DbgString, DbgString, DbgLang, DbgString, DbgString, time_t, 
	DbgFileFlags);
void dbg_file_end(DbgFileID);
DbgFileID dbg_file_reference(DbgString, DbgFileFlags);

/* Function to associate a new cmdline with the existing source file */

void dbg_file_backend_cmdline(DbgFileID, DbgString);

/* Function to associate object file name with the source file */

void dbg_set_obj_file_name(DbgFileID, DbgString);

/* Function to associate the Elf STT_FILE name to the DbgFileID */

void dbg_set_elf_file_name(DbgFileID, DbgString);

/* Functions to describe the #include of files.
 *	The included file must be completed with a dbg_file_end() call
 *	before calling dbg_include.
 *	The ho_directory is reserved for shared stabs in the future.
 *	All #include lines must be communicated with dbg_include()
 *	calls and these calls must be made before any of the types
 *	from this include file are used directly or indirectly.
 *	The file containing the #include must be supplied in the DbgLine
 *	argument to dbg_include.
 */

void dbg_include(DbgFileID, DbgLine, DbgString, DbgOutFlags);

/* Functions that describe scopes.
 *	All symbols (almost anything with a "name") will typically require
 *	some kind of scope. The basic kinds of scope currently are:
 *		Global scope: externs, static functions, static file data
 *		Function scope:	currently only for placing formals in
 *		Block scope: typical block scope where all local vars live
 *	Scoping is independent of files since scopes can span files.
 *	Only block scopes need a binding, see dbg_bind_scope_*(), or better
 *     		yet, use the new dbg_scope_block_() interfaces below.
 */

DbgScopeID dbg_scope_global(void);
DbgScopeID dbg_scope_func(DbgScopeID, DbgSymID);
DbgScopeID dbg_scope_block(DbgScopeID);

/* Alternative to dbg_scope_block/func() and associated dbg_bind_scope_*() calls.
 *    These accept the label in the code where the block scope starts and end.
 *    The *_start() call returns the new block scope given the parent scope.
 *    The *_end() call returns the parent scope of the block scope.
 */

DbgScopeID dbg_scope_block_start(DbgScopeID, DbgString);
DbgScopeID dbg_scope_block_end(DbgScopeID, DbgString);
DbgScopeID dbg_scope_func_start(DbgScopeID, DbgSymID, DbgString);
DbgScopeID dbg_scope_func_end(DbgScopeID, DbgString);
void dbg_scope_start(DbgScopeID, DbgString);
void dbg_scope_end(DbgScopeID, DbgString);

/* To delete a scope.
 */
void dbg_scope_delete(DbgScopeID); 

/* Functions used for function definitions.
 *	Function definitions require a scope (usually global scope),
 *	a name, and a function type (see type_func*()).
 *	The flags argument is used to distinguish the different kinds of
 *	function definitions, e.g. extern, file static, etc.
 *	A function will also need a function scope and a block scope
 *	created so that the formal's (calls to dbg_func_formal()) locals
 *	will have a scope to use for their creations.
 *	Formals need a function scope and the flags will tell whether
 *	the formal is being passed by value, by reference, or by
 *	value/reference, etc.
 *	dbg_func_qualified_name() is optional, to be used only
 *	when the qualified name is to be used in the stabs in place
 *	of the user name.  Initial use is for F90 internal subroutines
 *	and subroutines contained in modules.
 *	These symbols will need dbg_src_sym() and dbg_bind_*() calls
 *	to bind the symbol to the source location and object reference.
 */

DbgSymID dbg_func_begin(DbgScopeID, DbgString, DbgTypeID, DbgSymFlags);
void     dbg_func_location(DbgSymID, DbgString);
void     dbg_func_hwcprof_signature(DbgSymID, DbgUint32);
void     dbg_func_end(DbgSymID);
void     dbg_func_sym_begin(DbgSymID, DbgSymFlags);
DbgSymID dbg_func_formal(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
	DbgSymFlags);
DbgSymID dbg_func_formal_arg(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
	DbgSymFlags, DbgArgOffset,
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);
DbgSymID dbg_func_formal_reg(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
	DbgSymFlags, DbgRegNum,
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);
void     dbg_func_formal_static(DbgSymID, DbgLargeBytes);
void     dbg_func_qualified_name(DbgSymID, DbgString);

/* Entry points in Fortran are treated special.
 *	They are attached to the main entry point (a dbg_func_begin() handle)
 *	and the entry point formals are attached to the entry point symbol.
 */

DbgSymID dbg_entry(DbgSymID, DbgString, DbgTypeID, DbgSymFlags);
DbgSymID dbg_entry_formal(DbgSymID, DbgString, DbgTypeID, DbgLargeBytes, 
	DbgSymFlags);

/* Functions for describing other function-like symbols.
 *	To describe functions called but not defined in this object file.
 */

DbgSymID dbg_func_declaration(DbgScopeID, DbgString, DbgTypeID, DbgSymFlags);
void     dbg_func_sym_declaration(DbgSymID, DbgSymFlags);
DbgSymID dbg_func_symbol(DbgScopeID, DbgString, DbgTypeID, DbgSymFlags);

/* Function to search for a particularly named function.
 */

DbgSymID dbg_func_search(DbgScopeID, DbgString, DbgString);

/* Deletes all lin information for this function. */

void dbg_func_delete_lines(DbgSymID); 

/* Defines an outline region for this function. */

DbgSymID dbg_func_outline_region(DbgSymID, DbgString); 

/* Defines a clone function for this function. */

DbgSymID dbg_func_clone(DbgSymID, DbgString); 

/* Defines a mfunc function and it's parent function */

void dbg_func_mfunc(DbgSymID, DbgSymID); 

/* Fortran Generic functions.
 *      To define the generic function symbol. This symbol can be bound
 *      to multiple functions by dbg_bind_func().  Interface blocks
 *      with no names, nested blocks should not be passed through this
 *	interface.
 */

DbgSymID dbg_generic(DbgScopeID, DbgString);


/* Functions for creating the source line coordinates of interest.
 *	A source file id is a simple file_id, all source files will have
 *	already been created and added to the file table with dbg_file_begin().
 */

DbgLine dbg_line(DbgFileID, DbgIntLine, DbgIntLineOffset);

/* Functions to bind user names to symbols and types.
 *     Normally only when the linker name differs from the user name
 *     and the linker name was supplied as the "name" of the symbol
 *     and type. This is currently the C++ style. Most of the "name"
 *     arguments will be the C++ "mangled name" and then calls to
 *     dbg_*_user_name() will be used to provide the user name.
 */

void dbg_type_user_name(DbgTypeID, DbgString);
void dbg_sym_user_name(DbgSymID, DbgString);
void dbg_sym_linker_name(DbgSymID, DbgString);
void dbg_sym_omp_tpriv_ptr(DbgSymID, DbgString);

/* Functions to bind source coordinates to debug objects.
 *	Various debug objects need to be bound to source addresses,
 *	specifically functions, variables, labels, and some types.
 *	Eventually the line may or may not be bound to an object
 *	reference, depending on the existence of code for the line.
 *	Types themselves do not contain source coordinates, but many
 *	types have symbols connected to them and the dbg_src_type()
 *	call actually sets the source coordinate of the connected
 *	symbol, e.g. typedefs are types and symbols but the interface
 *	only provides the type handle.
 */

void dbg_src_type(DbgTypeID, DbgLine, DbgIntLine);
void dbg_src_sym(DbgSymID, DbgLine, DbgIntLine);
void dbg_type_src_location(DbgTypeID, DbgFileID, DbgIntLine, DbgIntLineOffset);
void dbg_sym_src_location(DbgSymID, DbgFileID, DbgIntLine, DbgIntLineOffset);
void dbg_sym_delete(DbgSymID); 

/* Functions that are used to create types.
 *	The debug type is created with these calls.
 *	The basic types must be created based on the language specifics,
 *	then those base types are used to create the various types
 *	describe below.
 *	The file_id argument is critical for types because these are
 *	actually what can be shared (along with the type symbol that
 *	uses them).
 *	Some of these symbols might need dbg_src_type() calls
 *	to bind the symbol to the source location.
 */

enum {
	DBG_SIGNED 		= (1 << 0),
	DBG_UNSIGNED 		= (1 << 1),
	DBG_CHAR 		= (1 << 2),
	DBG_BOOLEAN 		= (1 << 3),
	DBG_PRIVATE 		= (1 << 4),
	DBG_PROTECTED 		= (1 << 5),
	DBG_PUBLIC 		= (1 << 6),
	DBG_VIRTUAL 		= (1 << 7),
	DBG_VAX_TYPE 		= (1 << 8),
	DBG_STRUCT 		= (1 << 9),
	DBG_UNION 		= (1 << 10),
	DBG_ANON 		= (1 << 11),
	DBG_NESTED 		= (1 << 13),
	DBG_FIELD_STATIC 	= (1 << 15),
	DBG_FRIEND_FUNC 	= (1 << 16),
	DBG_FRIEND_CLASS 	= (1 << 17),
	DBG_VARARGS 		= (1 << 18),
	DBG_MUTABLE 		= (1 << 19),
	DBG_CLASS 		= (1 << 20),
	DBG_PASS_WITH_CONST 	= (1 << 21),
	DBG_RETURN_WITH_CONST	= (1 << 22),
	DBG_PURE_VIRTUAL	= (1 << 23),
	DBG_EXPLICIT_CONST	= (1 << 24), 
	DBG_PROTOTYPE		= (1 << 25)
};
typedef unsigned DbgTypeFlags;

/* Creating basic types */

DbgTypeID dbg_type_basic_integral(DbgFileID, DbgString, int, int, int, 
	DbgTypeFlags);
DbgTypeID dbg_type_basic_float(DbgFileID, DbgString, int);
DbgTypeID dbg_type_basic_complex(DbgFileID, DbgString, int);
DbgTypeID dbg_type_basic_interval(DbgFileID, DbgString, int);
DbgTypeID dbg_type_basic_imaginary(DbgFileID, DbgString, int);

/* Creating ranges (for array bounds) */

DbgTypeID dbg_type_range(DbgFileID, DbgTypeID, DbgValue, DbgValue);

/* Creating arrays, pointers, const, volatile, restricted, etc. types */

DbgTypeID dbg_type_array_byval(DbgFileID, DbgTypeID, DbgTypeID, 
	DbgValue, DbgValue);
DbgTypeID dbg_type_array_fixed(DbgFileID, DbgTypeID, DbgTypeID, 
	DbgValueInt, DbgValueInt);
DbgTypeID dbg_type_array_vla(DbgFileID, DbgTypeID, DbgTypeID, 
	DbgValueInt, DbgValue);
DbgTypeID dbg_type_array_vla_smax(DbgFileID, DbgTypeID, DbgTypeID, 
	DbgValueInt, DbgSymID);
DbgTypeID dbg_type_array(DbgFileID, DbgTypeID, DbgTypeID);
DbgTypeID dbg_type_pointer(DbgFileID, DbgTypeID);
DbgTypeID dbg_type_member_func_pointer(DbgFileID, DbgTypeID, DbgTypeID);
DbgTypeID dbg_type_member_data_pointer(DbgFileID, DbgTypeID, DbgTypeID);
DbgTypeID dbg_type_ref(DbgFileID, DbgTypeID);
DbgTypeID dbg_type_restrict(DbgFileID, DbgTypeID);
DbgTypeID dbg_type_const(DbgFileID, DbgTypeID);
DbgTypeID dbg_type_volatile(DbgFileID, DbgTypeID);

/* To create typedef's, regular and those defined in classes */

DbgTypeID dbg_type_define(DbgFileID, DbgScopeID, DbgString, DbgTypeID);

/* To bind a 'forward' type to its real type */

void dbg_type_forward_bind(DbgTypeID forward_type, DbgTypeID type_id);

/* To create enum types */

DbgTypeID dbg_type_enum_forward(DbgFileID, DbgScopeID, DbgString);
DbgTypeID dbg_type_enum_begin(DbgFileID, DbgScopeID, DbgString, DbgTypeID);
DbgSymID  dbg_type_enum_name(DbgTypeID, DbgString, DbgValueInt);
void 	  dbg_type_enum_end(DbgTypeID);

/* Used to signify the use of #pragma pack() on a type. */

void      dbg_type_pack(DbgTypeID, DbgSmallBytes);

/* To create simple structs */

DbgTypeID dbg_type_struct_forward(DbgFileID, DbgScopeID, DbgString);
DbgTypeID dbg_type_struct_begin(DbgFileID, DbgScopeID, DbgString, DbgLargeBytes);
DbgSymID  dbg_type_struct_field(DbgTypeID, DbgString, DbgTypeID, 
	DbgLargeBitOffset, DbgLargeBitSize);
void      dbg_type_struct_end(DbgTypeID);

/* To create unions */

DbgTypeID dbg_type_union_forward(DbgFileID, DbgScopeID, DbgString);
DbgTypeID dbg_type_union_begin(DbgFileID, DbgScopeID, DbgString, DbgLargeBytes);
DbgSymID  dbg_type_union_field(DbgTypeID, DbgString, DbgTypeID, DbgLargeBitSize);
void      dbg_type_union_end(DbgTypeID);

/* To create F90 dope vector things */

DbgTypeID dbg_type_f90_pointer(DbgFileID, DbgTypeID);
DbgTypeID dbg_type_f90_allocatable(DbgFileID, DbgTypeID);
DbgTypeID dbg_type_f90_assumed_shape(DbgFileID, DbgTypeID);

/* To create function "proto" types */

DbgTypeID dbg_type_func_begin(DbgFileID, DbgTypeID, DbgLang);
void      dbg_type_func_formal(DbgTypeID, DbgTypeID);
void      dbg_type_func_formal_default_int(DbgTypeID func_type, 
		  DbgTypeID formal_type, DbgValueInt default_value);
void      dbg_type_func_formal_default_func(DbgTypeID func_type,
		  DbgTypeID formal_type, DbgString helper_func);
void      dbg_type_func_varargs(DbgTypeID);
void      dbg_type_func_throws(DbgTypeID, DbgTypeID);
void      dbg_type_func_throws_nothing(DbgTypeID);
void      dbg_type_func_end(DbgTypeID);

/* Functions that deal with describing variables etc.
 *	Variables need a scope_id, name, and type.
 *	These symbols will need dbg_src_sym() and dbg_bind_*() calls
 *	to bind the symbol to the source location and data location.
 */

DbgSymID dbg_var(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, DbgSymFlags);
void     dbg_var_sym(DbgSymID, DbgSymFlags);
DbgSymID dbg_var_global(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
		DbgSymFlags,
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);
DbgSymID dbg_var_stack(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
		DbgSymFlags, DbgStackOffset, 
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);
DbgSymID dbg_var_reg(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
		DbgSymFlags, DbgRegNum, 
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);
DbgSymID dbg_var_static(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
		DbgSymFlags, DbgString, 
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);
DbgSymID dbg_var_static_local(DbgScopeID, DbgString, DbgTypeID, DbgLargeBytes, 
		DbgSymFlags, DbgString,  DbgString,
		DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col);

/* Pointee for Fortran for POINTER(var_for_pointer,pointee) */

DbgSymID dbg_var_pointee(DbgSymID, DbgString, DbgTypeID);

/* Fortran modules.
 *	Module definitions require a scope (usually global scope),
 *	and a user name.  The "members" are all the types, variables,
 *	functions, etc that are defined within the module.  Their
 *	names are given here, and also in separate dbg_* calls
 *	appropriate for what they are.  All these members should be
 *	placed in the module's scope.
 */
    
DbgSymID dbg_f90_module_begin(DbgScopeID, DbgString);
void     dbg_f90_module_member(DbgSymID, DbgString, DbgTypeFlags);
void     dbg_f90_module_end(DbgSymID);

/*
 * Support for f90 module usage.  There are two functions, one
 * to define the name of the module being used, the other to
 * define each variable (and its local name) specified in the
 * 'use' statement (no subroutines or type names).  The local
 * name should be NULL if there is no rename of it.
 */

DbgSymID dbg_f90_use_module(DbgScopeID, DbgString, DbgSymFlags);
void     dbg_f90_use_renames(DbgSymID, DbgString, DbgString);


/* Functions for other "named" local entities (mostly used in Fortran).
 *	These symbols will need dbg_src_sym() calls
 *	to bind the symbol to the source location.
 */

DbgSymID dbg_namelist(DbgScopeID, DbgString, DbgString);
DbgSymID dbg_alias_variable(DbgScopeID, DbgString, DbgString);
DbgSymID dbg_alias_global(DbgScopeID, DbgString, DbgString);
DbgSymID dbg_alias_file_local(DbgScopeID, DbgString, DbgString);

DbgSymID dbg_name_sym(DbgScopeID, DbgString);
void     dbg_weak_sym(DbgSymID);
void     dbg_weak_name(DbgSymID, DbgSymID);
void     dbg_alias_name(DbgSymID, DbgSymID);


/* Functions for macro definitions, undef's, and statement functions.
 *	Macros and statement functions have been grouped together even
 *	though they differ significantly with respect to semantics.
 */

void dbg_statement_func(DbgScopeID, DbgString, DbgString, DbgString, DbgLine);
void dbg_mac_define(DbgScopeID, DbgString, DbgString, DbgString, DbgLine);
void dbg_mac_undef(DbgScopeID, DbgString, DbgLine);

/* Functions to describe common blocks.
 *	Common blocks are treated as local symbols, along with all their
 *	common variables.
 *	These symbols will need dbg_src_sym() and dbg_bind_*() calls
 *	to bind the symbol to the source location and object reference.
 *	[Note: All of the common variables and the common block had better
 *	have an object reference to the same object.]
 */

DbgSymID dbg_common_block(DbgScopeID, DbgString, DbgSymFlags);
DbgSymID dbg_common_var(DbgSymID, DbgString, DbgTypeID, DbgCommByteOff);

/* Binding symbols to the object file and code tags about object references.
 *	Different kinds of symbols need different kinds of bindings, e.g.
 *	functions need to be bound to a object reference, variables to
 *	object references or stack offsets etc.
 *	Also, code tags about certain object references tell the debugger or
 *	tools valuable information that it would otherwise find difficult
 *	or impossible to figure out.
 *	For now most of these bindings are 1-to-1 with a few exceptions.
 *	In the future the bindings between variables and their data locations
 *	will be 1-to-N, where N could be 0 or any number of data locations
 *	where each data location would be qualified with an instruction
 *	range where that data location is valid.
 *	Some of these functions imply a 1-to-1 mapping for the symbol or scope:
 *		dbg_bind_data
 *		dbg_bind_stack
 *		dbg_bind_reg
 *              dbg_bind_reg_kind
 *		dbg_bind_constant
 *		dbg_bind_literal
 *		dbg_bind_formal
 *		dbg_bind_scope_start and dbg_bind_scope_end
 */

void dbg_bind_func(DbgSymID, DbgObjectRef);
void dbg_bind_data(DbgSymID, DbgObjectRef);
void dbg_bind_stack(DbgSymID, DbgStackOffset);
void dbg_bind_reg(DbgSymID, DbgRegNum);
void dbg_bind_reg_kind(DbgSymID, DbgRegNum, DbgRegKind);
void dbg_bind_constant(DbgSymID, DbgValue);
void dbg_bind_literal(DbgSymID, DbgValue);
void dbg_bind_formal(DbgSymID, DbgArgOffset);
void dbg_bind_dataloc(DbgSymID, DbgDataLoc);
void dbg_bind_scope_start(DbgScopeID, DbgObjectRef);
void dbg_bind_scope_end(DbgScopeID, DbgObjectRef);
void dbg_bind_line(DbgLine, DbgSymID, DbgCodeLoc);

/* For C++ line and local object construction and destruction state. */

void dbg_bind_line_state(DbgSymID func_sym, DbgLine line, 
	     int state_mod, DbgString label);
void dbg_bind_func_line_state(DbgSymID func_sym, 
             DbgFileID file_id, DbgIntLine line, DbgIntLineOffset col, 
	     int state_mod, DbgString label);
void dbg_bind_line_constructor(DbgSymID func_sym, DbgString var_name,
	     DbgDestState new_state, DbgString slabel, DbgString elabel);
void dbg_bind_line_destructor(DbgSymID func_sym, DbgDestState from_state, 
	     DbgDestState to_state, DbgString slabel, DbgString elabel);
void dbg_bind_func_line(DbgSymID func_sym, DbgFileID file_id, 
		DbgIntLine line, DbgIntLineOffset col, DbgString label);
void dbg_bind_func_line_binary(DbgSymID, DbgFileID, DbgIntLine, 
		DbgIntLineOffset, DbgSmallByteOff); 

/* Function to walk line information. */

typedef int (*DbgLineFptr)(DbgSymID,DbgFileID,DbgIntLine,DbgIntLineOffset,DbgString,DbgString,DbgSmallByteOff, DbgLineID); 
int  dbg_walk_func_lines(DbgSymID, DbgLineFptr); 
void dbg_rebind_func_line(DbgSymID, DbgLineID, DbgString, DbgSmallByteOff); 

/* Code tags to mark code with. */

typedef enum {
	DBG_CODETAG_BITFIELD 	= 1,
	DBG_CODETAG_SPILL 	= 2,
	DBG_CODETAG_SCOPY 	= 3,
	DBG_CODETAG_FSTART 	= 4,
	DBG_CODETAG_END_CTORS	= 5,
	DBG_CODETAG_CTI_TARGET  = 6,
	DBG_CODETAG_STACK_PROBE = 7,
	DBG_CODETAG_EPILOG      = 8,
	_DBG_CODETAG_Max = 8
} DbgCodeTagMarker;

void dbg_bind_code_tag(DbgSymID, DbgCodeTagMarker, DbgString label, DbgSmallByteOff);
void dbg_bind_code_tag_binary(DbgSymID, DbgCodeTagMarker, DbgSmallByteOff); 

/* Function to bind attributes to a memory op instruction */

void dbg_bind_memop(DbgSymID, DbgSmallByteOff, DbgSmallUnsigned, DbgSmallUnsigned, DbgSymID);

/* Function to output source browser stab
 *	specifies the path to the .bd file.  This stab is placed in
 *	the .stab.sbfocus section.  Only one call per file_id.
 */

void dbg_browse(DbgFileID, DbgString);

/* C++ namespace */

DbgTypeID dbg_type_namespace(DbgFileID, DbgScopeID, DbgString, DbgTypeFlags);
DbgTypeID dbg_type_namespace_alias(DbgFileID, DbgScopeID, DbgString, 
			DbgTypeID, DbgTypeFlags);

/* C++ namespace "using" directive and declaration */

void dbg_using_declaration_sym(DbgScopeID use_scope_id, 
	   	DbgSymID namespace_member);

void dbg_using_declaration_type(DbgScopeID use_scope_id,
       		DbgTypeID namespace_member);

void dbg_using_directive(DbgScopeID scope_id, DbgTypeID nspace);

/* C++ classes */

/* For anonymous union inside a class, first define the anonymouse union
 * like a nested union, then use dbg_type_class_field() to define it as
 * a datamember except parameter name & bit_size are ignored.
 */

DbgTypeID dbg_type_class_forward(DbgFileID, DbgScopeID, DbgString);
void      dbg_type_class_forward_bind(DbgTypeID, DbgTypeID);
DbgTypeID dbg_type_class_begin(DbgFileID, DbgScopeID, DbgString, DbgLargeBytes, 
	DbgVtOffset, DbgVtVersion, DbgTypeFlags);

/* If the inheritance is non-virtual, byte_offset is the offset of the
 * embedded instance of this base class. If the inheritance is virtual,
 * vt_offset is the offset to the pointer which implements the virtual
 * inheritance, and byte_offset is the offset where this virtual base
 * class resides.
 */
    
void dbg_type_class_base(DbgTypeID, DbgTypeID, DbgLargeByteOff, 
	DbgVtOffset, DbgTypeFlags);
void dbg_type_class_virtual_base(DbgTypeID, DbgTypeID, DbgLargeByteOff);
void dbg_type_class_adjust(DbgTypeID, DbgSymID, DbgTypeFlags);
void dbg_type_class_gnu_containing_type (DbgTypeID, DbgTypeID);

/* For static data members, only class type_id, name & flags are
 * used.
 */
    
DbgSymID dbg_type_class_field(DbgTypeID, DbgString, DbgTypeID, DbgLargeBitOffset, 
	DbgLargeBitSize, DbgTypeFlags);

/* virtual_index is only used when the method is virutal */
    
DbgSymID dbg_type_class_method(DbgTypeID, DbgString, DbgTypeID,
	DbgVtIndex, DbgTypeFlags);
void dbg_type_class_friend(DbgTypeID, DbgSymID, DbgTypeFlags);
void dbg_type_class_nested(DbgTypeID, DbgTypeID, DbgTypeFlags);
void dbg_type_class_end(DbgTypeID);

/* C++ dummy type */

DbgTypeID dbg_type_dummy(DbgFileID, DbgScopeID, DbgString);

/* C++ Template definition information.
 *      This is used to define a template in terms of what kind of template
 *	and the names of it's formals. This template definition type
 *	then should be used in identifying the instances of the template.
 */

DbgTypeID dbg_templ_def_function(DbgFileID file_id, DbgScopeID scope_id, 
		DbgString def_name, DbgIntLine start, DbgIntLine end, 
		DbgTypeFlags templ_type_flags);

void      dbg_templ_def_func_type(DbgTypeID tdef, DbgTypeID func_type);

DbgTypeID dbg_templ_def_class(DbgFileID file_id, DbgScopeID scope_id, 
		DbgString def_name, 
		DbgTypeFlags templ_type_flags);

DbgTypeID dbg_templ_def_named_formal(DbgTypeID tdef, 
		DbgString formal_name);
void      dbg_templ_def_typed_formal(DbgTypeID tdef, 
		DbgString formal_name, DbgTypeID formal_type);

void      dbg_templ_def_param(DbgTypeID tdef, 
		DbgString name, DbgTypeID param_type);

/* C++ Template Instantiations.
 *	These functions inform us that a function or class is actually a
 *	template instantiation, and what template definition was used.
 *	Any instantiation arguments are also supplied by passing the
 *      template instance DbgSymID to the dbg_templ_inst_*_arg() calls.
 */

DbgSymID dbg_templ_inst_func(DbgScopeID scope_id, DbgSymID func_id,
		DbgTypeID tdef, DbgSymFlags flags);
DbgSymID dbg_templ_inst_mfunc(DbgScopeID scope_id, DbgSymID func_id,
		DbgString tdef_name, DbgSymFlags flags);
DbgSymID dbg_templ_inst_class(DbgScopeID scope_id, DbgTypeID class_type_id, 
		DbgTypeID tdef, DbgSymFlags flags);

void      dbg_templ_inst_int_type_arg(DbgSymID tinst,
		DbgString arg_name, DbgValueInt literal, 
		DbgTypeID arg_type_id);
void      dbg_templ_inst_pointer_type_arg(DbgSymID tinst,
		DbgString arg_name, DbgString symbol_name, 
		DbgTypeID arg_type_id);
void      dbg_templ_inst_type_arg(DbgSymID tinst,
		DbgString arg_name, DbgTypeID arg_type_id);
void      dbg_templ_inst_symbol_arg(DbgSymID tinst,
		DbgString arg_name, DbgSymID arg_symbol_id);

/* Functions to describe C++ RTTI's.
 *	These functions are used to describe an RTTI variable and a thrown
 *	type. These need to be placed in the appropriate scope.
 */

DbgSymID dbg_rtti(DbgScopeID, DbgString, DbgTypeID, DbgSymFlags);

/* Functions that query the dbg_gen data */

/* DbgValue kind query */
DbgValueKind 	dbg_value_kind(DbgValue);

/* DbgValue accessor functions (must match kind) */
DbgValueInt	dbg_value_get_int(DbgValue);
DbgStackOffset	dbg_value_get_stack(DbgValue);
DbgStackOffset	dbg_value_get_size(DbgValue);
DbgArgOffset	dbg_value_get_arg(DbgValue);
DbgRegNum	dbg_value_get_reg(DbgValue);
DbgObjectRef	dbg_value_get_oref(DbgValue);
/* not yet	dbg_value_get_data(DbgValue);  */

DbgString   dbg_sym_get_name(DbgSymID);
DbgTypeID   dbg_sym_get_type(DbgSymID);
DbgScopeID  dbg_sym_get_scope(DbgSymID);
DbgScopeID  dbg_sym_get_func_scope(DbgSymID);
DbgSymFlags dbg_sym_get_flags(DbgSymID);

DbgObjectRef    dbg_sym_get_oref_bind(DbgSymID);
DbgLargeByteOff dbg_sym_get_stack_bind(DbgSymID);
DbgRegNum       dbg_sym_get_reg_bind(DbgSymID);

void        dbg_sym_set_flags(DbgSymID, DbgSymFlags);
void	    dbg_sym_initial_value(DbgSymID, DbgValueInt);

DbgSymID     dbg_type_get_sym(DbgTypeID);
DbgFileID    dbg_type_get_file(DbgTypeID);

/*
 * get_upperbound and set_upperbound can handle any DbgValue, and
 * so are more generalized than get_ubound and set_ubound, which
 * we keep around just for backward compatibility.
 */
DbgValue     dbg_type_get_upperbound(DbgTypeID);
void         dbg_type_set_upperbound(DbgTypeID, DbgValue);
DbgValueInt  dbg_type_get_ubound(DbgTypeID);
void         dbg_type_set_ubound(DbgTypeID, DbgValueInt);

DbgTypeFlags dbg_type_get_flags(DbgTypeID);
void         dbg_type_set_flags(DbgTypeID, DbgTypeFlags);
int          dbg_type_is_array(DbgTypeID); 
int          dbg_type_is_vla(DbgTypeID); 
int          dbg_type_is_local(DbgTypeID); 
DbgTypeID    dbg_type_get_target(DbgTypeID); 
DbgLang      dbg_type_get_lang(DbgTypeID); 
void         dbg_type_set_lang(DbgTypeID, DbgLang); 

DbgScopeID dbg_get_type_scope(DbgTypeID);
DbgScopeID dbg_get_sibling_scope(DbgScopeID);
DbgScopeID dbg_get_child_scope(DbgScopeID);
DbgScopeID dbg_get_parent_scope(DbgScopeID);

DbgObjectRef dbg_get_scope_start_bind(DbgScopeID);
DbgObjectRef dbg_get_scope_end_bind(DbgScopeID);

DbgFileID  dbg_file_search(DbgString);
DbgFileID  dbg_file_get_source(void);

DbgOutFlags dbg_get_outflags(void);

/* Dump functions */

void dbg_dump_DbgFileID(DbgFileID);
void dbg_dump_DbgScopeID(DbgScopeID);
void dbg_dump_DbgTypeID(DbgTypeID);
void dbg_dump_DbgSymID(DbgSymID);

/* Walk functions */

typedef void (*DbgFileFptr)(DbgFileID);
typedef void (*DbgScopeFptr)(DbgScopeID);
typedef void (*DbgSymFptr)(DbgSymID);
typedef void (*DbgTypeFptr)(DbgTypeID);
void dbg_walk_DbgFileID(DbgFileFptr);
void dbg_walk_DbgScopeID(DbgScopeFptr);
void dbg_walk_DbgTypeID(DbgFileID, DbgTypeFptr);
void dbg_walk_DbgSymID(DbgScopeID, DbgSymFptr);

/* For shims use mostly, not part of the interface */

DbgWriteInput dbg_fake_sdf_write_input(DbgOutFlags, SdfHandle);
void dbg_fake_stdout_writer(unsigned char *, size_t, void *);

/* New read functions for PostOpt */

/* Function to get a name of an internal file */

DbgString dbg_get_file_name(DbgFileID);

/* Function to read source line information associated with a function */

typedef void (*DbgLineInfoFptr)(DbgSymID, DbgFileID, DbgIntLine, 
    DbgIntLineOffset, DbgSmallByteOff, void *context);
void dbg_get_func_lines(DbgSymID, DbgLineInfoFptr, void *context);

/* Function to read code tags associated with function instructions */

typedef void (*DbgCodeTagInfoFptr)(DbgSymID, DbgCodeTagMarker, DbgSmallByteOff, 
    void *context);
void dbg_get_code_tags(DbgSymID, DbgCodeTagInfoFptr, void *context);

/* Function to read memory op attributes of function instructions */

typedef void (*DbgMemopInfoFptr)(DbgSymID, DbgSmallByteOff, DbgSmallUnsigned,
    DbgSmallUnsigned, DbgSymID, void *context);
void dbg_get_memops(DbgSymID, DbgMemopInfoFptr, void *context);

#ifdef __cplusplus
    }				/* End of extern "C" */
#endif

#endif	/* _DBG_GEN_H */
