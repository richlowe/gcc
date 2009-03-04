/* translates tree gimple into Sun IR.
   Copyright (C) 2007, 2008 by Sun Microsystems, Inc. All rights reserved.  
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

#include "config.h"
#undef ENABLE_TREE_CHECKING
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "output.h"
#include "errors.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "ggc.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "tree-flow.h"
#include "timevar.h"
#include "tree-dump.h"
#include "cp/cp-tree.h"
#include "toplev.h"
#include "except.h"
#include "cfgloop.h"
#include "real.h"
#include "debug.h"
#include "target.h"
#include "cgraph.h"
#include "c-common.h"
#include "tree-ir.h"
#include "ir/dbg_gen.h"
#include "tree-pass.h"

/* Stack type that can be used for various Dbg*ID types,
 * such as DbgFileID, DbgScopeID that exhibit recursive structure. */
typedef struct 
{
   DbgIntIndex *table;
   DbgSint32 size;
   DbgSint32 top_index;
} dbg_id_stack_t;

static struct {
  int             on;             /* TRUE if dbg_gen is used */
  DbgOutFlags     outflags;       /* DbgOutFlags for dbg_begin() */
  DbgLang         lang_flag;      /* Source language of compilation unit.  */
  const char      *func_name;     /* Source name of current function */
  const char      *link_name;     /* Linker name of current function */
  DbgFileID       file;           /* file handle for source file */
  DbgSymID        func_sym;       /* symbol handle for current func */
  DbgScopeID      global_scope;   /* global scope handle */
  DbgScopeID      func_scope;     /* function scope handle */
  DbgScopeID      cur_scope;      /* the current scope */
  DbgTypeID       null_type;      /* null type */
  DbgScopeID      null_scope;     /* null scope */
  DbgFileID       null_file;      /* null file */
  DbgSymID        null_sym;       /* null sym */
  char            *optstring;     /* N_OPT string */
  dbg_id_stack_t  file_id_stack;  /* DbgFileID stack */
} dbggendata;

static GTY(()) tree forward_bind_types; /* List of forward bound global types */   

static int is_dwarf_base_type (tree);
static bool is_user_defined_typedef (tree);
static DbgTypeID dbg_gen_get_integral_TypeID (tree);
static DbgTypeID dbg_gen_get_base_TypeID (tree);
static DbgTypeID dbg_gen_get_TypeID (tree type);
static DbgSymFlags dbg_gen_get_var_decl_sym_flags (tree);
static DbgSymFlags dbg_gen_get_parm_decl_sym_flags (tree);
static DbgSymFlags dbg_gen_get_func_decl_sym_flags (tree);
static DbgTypeID dbg_gen_make_qualified_TypeID (tree type);
static void dbg_gen_init_1 (void);
static void dbg_gen_start_main_file (const char *);
static void dbg_gen_end_main_file (void);
static void dbg_gen_begin_function_decl (tree);
static void dbg_gen_end_function_decl (void);
static void dbg_gen_var_decl (tree, DbgScopeID);
static DbgFileID dbg_gen_lookup_filename (const char *);
static void dbg_gen_init_dwarf_base_types (void);
static void dbg_gen_decls_for_scope (tree);
static DbgTypeID dbg_gen_enum_TypeID (tree);
static DbgTypeID dbg_gen_struct_union_TypeID (tree);
static DbgTypeID dbg_gen_forward_bind_TypeID (tree);
static void dbg_gen_resolve_forward_bind_types (void);
static void dbg_gen_function_args (tree);
static const char *dbg_gen_decl_linkage_name (tree);
static int dbg_gen_get_field_offset (tree);
static DbgTypeID dbg_gen_get_field_TypeID (tree);
static bool dbg_gen_global_scope_var_p (tree decl);
static const char * dbg_gen_get_type_debug_name (tree);
static DbgScopeID dbg_gen_context_ScopeID (tree);


/* C specific features.  */
static DbgTypeID dbg_gen_C_struct_union_TypeID (tree);

/* C++ specific features.  */
static DbgTypeFlags dbg_gen_CC_type_flags (tree type);
static DbgTypeID dbg_gen_CC_struct_union_TypeID (tree);
static DbgTypeFlags dbg_gen_CC_member_type_flags (tree decl);
static DbgTypeFlags dbg_gen_CC_data_member_type_flags (tree decl);
static DbgTypeFlags dbg_gen_CC_method_member_type_flags (tree decl);
static void dbg_gen_CC_method_members (tree type);
static void dbg_gen_CC_data_members (tree type);
static void dbg_gen_CC_direct_base_classes (tree type);
static void dbg_gen_CC_field_decl_member (DbgTypeID, tree);
static void dbg_gen_CC_type_decl_member (DbgTypeID, tree);
static DbgScopeID dbg_gen_CC_nspace_ScopeID (tree);

/* Functions for manipulating Dbg*ID handles */
static void dbg_id_stack_push (dbg_id_stack_t *, DbgIntIndex);
static void dbg_id_stack_pop (dbg_id_stack_t *);
static DbgIntIndex dbg_id_stack_top (dbg_id_stack_t *);
static bool dbg_id_stack_empty_p (dbg_id_stack_t *);
static void dbg_id_stack_init (dbg_id_stack_t *);
static void dbg_id_stack_free (dbg_id_stack_t *);

/* SUN dbg_gen debug hooks functions */
static void dbg_gen_init (const char *);
static void dbg_gen_start_source_file (unsigned int, const char *);
static void dbg_gen_end_source_file (unsigned int);
static void dbg_gen_function_decl (tree);
static void dbg_gen_finish (const char *);
static void dbg_gen_begin_prologue (unsigned, const char *);
static void dbg_gen_end_epilogue (unsigned, const char *);
static void dbg_gen_type_decl (tree, int);
static void dbg_gen_global_decl (tree);
static bool debug_true_tree (tree block);

/* The SUN dbg gen debug hooks structure.  
   See debug.h for description of each hook.  */
const struct gcc_debug_hooks dbg_gen_debug_hooks =
{
  dbg_gen_init,
  dbg_gen_finish,
  debug_nothing_int_charstar,  /* define  */
  debug_nothing_int_charstar,  /* undef  */
  dbg_gen_start_source_file,
  dbg_gen_end_source_file,
  debug_nothing_int_int,       /* begin_block  */
  debug_nothing_int_int,       /* end_block  */
  debug_true_tree,             /* ignore_block  */
  debug_nothing_int_charstar,  /* source_line  */
  dbg_gen_begin_prologue,
  debug_nothing_int_charstar,  /* end_prologue  */
  dbg_gen_end_epilogue,
  debug_nothing_tree,          /* begin_function  */
  debug_nothing_int,           /* end_function  */
  dbg_gen_function_decl,
  dbg_gen_global_decl, 
  dbg_gen_type_decl,
  debug_nothing_tree_tree,     /* imported_module_or_decl  */
  debug_nothing_tree,          /* deferred_inline_function  */
  debug_nothing_tree,          /* outlining_inline_function  */
  debug_nothing_rtx,           /* label  */
  debug_nothing_int,           /* handle_pch  */
  debug_nothing_rtx,           /* var_location  */
  debug_nothing_void,          /* switch_text_section */
  0                            /* start_end_main_source_file  */
};

static bool
debug_true_tree (tree block ATTRIBUTE_UNUSED)
{
  return true;
}

#define INIT_STACK_SIZE 64

/* Allocate memory for dbg_id_stack.  */
static void 
dbg_id_stack_init (dbg_id_stack_t *id_stack)
{
  memset (id_stack, 0, sizeof (dbg_id_stack_t));
  id_stack->size = INIT_STACK_SIZE;
  id_stack->table = (DbgIntIndex *) xmalloc (id_stack->size * sizeof (DbgIntIndex));
  id_stack->top_index = -1;
  return;
}

/* Free dbg_id_stack space.  */
static void
dbg_id_stack_free (dbg_id_stack_t *id_stack)
{
  if (id_stack->table)
    {
      free (id_stack->table);
    }
  
  return;
}

/* TRUE if dbg_id_stack is empty.  */
static bool
dbg_id_stack_empty_p (dbg_id_stack_t *id_stack)
{
  return (id_stack->top_index == -1);
}

/* Returns top Dbg*ID handle.  */
static DbgIntIndex
dbg_id_stack_top (dbg_id_stack_t *id_stack)
{
  if (dbg_id_stack_empty_p (id_stack)) 
    error ("Cannot return top element of an empty stack\n");
  
  return id_stack->table[id_stack->top_index];
}

/* Push Dbg*ID handle onto dbg_id_stack.  */
static void 
dbg_id_stack_push (dbg_id_stack_t *id_stack, DbgIntIndex new_id)
{
  if ((id_stack->top_index + 1) == id_stack->size) 
    {
      id_stack->size *= 2;
      id_stack->table = (DbgIntIndex *) xrealloc (id_stack->table, 
                                                  (id_stack->size *  sizeof(DbgIntIndex)));
    }
  id_stack->top_index++;
  id_stack->table[id_stack->top_index] = new_id;
  
  return;
}

/* Pops top Dbg*ID handle from dbg_id_stack.  */
static void 
dbg_id_stack_pop (dbg_id_stack_t *id_stack) 
{
  if (!dbg_id_stack_empty_p (id_stack))
    {
      id_stack->top_index--;
    }
  return;
}

static void
dbg_gen_begin_prologue (unsigned int lineno ATTRIBUTE_UNUSED, 
                        const char * file ATTRIBUTE_UNUSED)
{
  /* Needed for frame unwinding and exception handling.  */
  dwarf2out_begin_prologue (0, NULL); 
  return;
}

static void 
dbg_gen_end_epilogue (unsigned int lineno, const char *file)
{
  /* Needed for frame unwinding and exception handling.  */
  dwarf2out_end_epilogue (lineno, file);
  return;
}

/* Inform dbg_gen about a #included file. Push its FileID onto file_id stack.
 * FileID of where it was included is current top of the file_id stack.  */
static void 
dbg_gen_start_source_file (unsigned int lineno, const char *filename)
{
  DbgFileID src_id, new_id;
  
  src_id = dbg_id_stack_top (&(dbggendata.file_id_stack));
  new_id = dbg_file_begin ("", filename, dbggendata.lang_flag, "", "",
                           0ULL, DBG_FILE_INCLUDE);
  dbg_include (new_id, dbg_line (src_id, lineno, 0), "", 0);
  dbg_id_stack_push (&(dbggendata.file_id_stack), new_id);
    
  dbggendata.file = new_id;
  return;
}

/* Inform dbg_gen about the end of a #included file, pop its 
 * FileID from top of the file id stack.  */
static void 
dbg_gen_end_source_file (unsigned int lineno ATTRIBUTE_UNUSED)
{
  DbgFileID id;
  
  id = dbg_id_stack_top (&(dbggendata.file_id_stack));
  dbg_id_stack_pop (&(dbggendata.file_id_stack));
  dbg_file_end (id);
  
  dbggendata.file = dbg_id_stack_top (&(dbggendata.file_id_stack));
  return;
}

/* Extract debug information for module from dbg_gen.  
   Data written into the dbg_gen tables after this point is 
   effectively lost since it would not make it into the ir module.  */
void
dbg_gen_extract_mod_debug_info (ir_mod_hdl_t ir_mod)
{
  if (dbggendata.on)
    {
      /* Resolve forward binding of types.  */
      dbg_gen_resolve_forward_bind_types ();
      /* Announce gcc2ir dwarf debug contribution to dbg_gen.  */
      dbg_contributor_info ("gcc2ir", "dwarf debug info");

      if (!ir_mod_prepare_dbggen_output(ir_mod))
        {
          error ("Error extracting debug information from DBG GEN");
        }
    }

  return;
}

/* Record end of main source file.  */
static void
dbg_gen_end_main_file (void)
{
  if (dbggendata.on && dbggendata.file != dbggendata.null_file) 
    {
      dbg_id_stack_pop (&(dbggendata.file_id_stack));
      dbg_file_end (dbggendata.file);
    }

  return;
}

/* Wrap up debug information generation by dbg_gen.  */
static void
dbg_gen_finish (const char *filename ATTRIBUTE_UNUSED)
{
  if (dbggendata.on) 
    {
      dbg_gen_end_main_file ();
      
      /* free saved copy of N_OPT string */
      free (dbggendata.optstring);
      dbggendata.optstring = NULL;
      dbg_id_stack_free (&(dbggendata.file_id_stack));
      
      /* cleanup dbg_gen tables */
      dbg_end();
    }

  return;
}

const char * dbg_cmdline;

/* Inform dbg_gen of main source file. Helper for dbg_gen_init.  */
static void
dbg_gen_start_main_file (const char * filename)
{
  if (dbggendata.on) 
    {
      time_t t = 123; /* want to keep it the same to have match in recompile */
      char *cmdline, * orig_cmdline;

      static const char *cwd;

      if (!cwd)
        {
          cwd = get_src_pwd ();
          if (cwd[0] == '\0')
            cwd = "/";
          else if (!IS_DIR_SEPARATOR (cwd[strlen (cwd) - 1]))
            cwd = concat (cwd, "/", NULL);
        }

      /* The driver is expected to pass '-cmdline ***' that points to original command line.
         Use filename if the option missed. */
      if (dbg_cmdline == NULL)
        {
          orig_cmdline = (char *) xmalloc (strlen (filename) + 1);
          strcpy (orig_cmdline, filename);
        }
      else
        {
          /* dbg_cmdline keeps return string of print_orig_cmdline in gcc.c
             which modify the original command line by deleting source file name and adding '-c' to the end.
             Here we re-modify it to original one before outputing debug information. */
          orig_cmdline = (char *) xmalloc (strlen (dbg_cmdline) + strlen (filename));
          strncpy (orig_cmdline, dbg_cmdline, strlen (dbg_cmdline) - 2);
          orig_cmdline [strlen (dbg_cmdline) - 2] = 0;
          strcat (orig_cmdline, filename);
        }

      if (write_symbols == NO_DEBUG && ir_global_prefix != NULL)
        {
          cmdline = (char *) xmalloc (strlen (cwd) + strlen (orig_cmdline) 
                                      + strlen (ir_global_prefix) + 32);
          sprintf (cmdline,"%s; %s -W0,-xp%s", cwd, orig_cmdline, ir_global_prefix);
        } 
      else 
        {
          cmdline = (char *) xmalloc (strlen (cwd) + strlen (orig_cmdline) + 32);
          sprintf (cmdline,"%s; %s", cwd, orig_cmdline);
        }

      dbggendata.file = dbg_file_begin (cwd, filename, dbggendata.lang_flag, 
                                        cmdline, dbggendata.optstring, t, DBG_FILE_SOURCE);
      
      dbggendata.global_scope 	= dbg_scope_global();
      dbggendata.cur_scope 	= dbggendata.global_scope;
      dbggendata.func_scope 	= dbggendata.null_scope;
      dbg_id_stack_push (&(dbggendata.file_id_stack), dbggendata.file);

      free (orig_cmdline);
    }
}

/* called by dbg_gen library for error detection. passed as a param to dbg_begin() */

static void
dbggen_error (const char *file, DbgIntLine line, const char *message)
{   
  if (file == NULL) 
    file = "";
  
  if (message == NULL) 
    message = "";
  
  error ("DBGGEN error \"%s\":%d, %s", file, line, message);
}   

/* Examine specified debugging options and inform dbg_gen of what 
 * it should be doing. Helper to dbg_gen_init.  */ 
static void
dbg_gen_init_1 (void)
{
  DbgOptionsInput option_input;	/* Input for dbg_options() */
  char optstring[BUFSIZ]; /* string for N_OPT */
  const char *gnu_prefix = "GNU ";
	    
  memset (&option_input, 0, sizeof (option_input));
  
  if (write_symbols == NO_DEBUG)
    option_input.options = "%none";
  else if (write_symbols == DBX_DEBUG)
    option_input.options = "stabs+no%dwarf2";
  else if (write_symbols == DWARF2_DEBUG)
    option_input.options = "no%stabs+dwarf2";
  else
    option_input.options = "bids";

  if (write_symbols == NO_DEBUG)
    option_input.gflag = 0;
  else
    option_input.gflag = 1;
  
  option_input.opt_level = optimize;
  option_input.pic_flag	= flag_pic;
  option_input.v9_flag = TARGET_ARCH64;
  option_input.cpl_rel = "gccfss 4.2.1";
  option_input.da_glob_prefix = ir_global_prefix;
  
  strcpy (optstring, "Xa");  /* Xt or Xs.  */

  /* Parse dbggen options and figure out what dbggen will be doing */
  dbggendata.on = dbg_options (option_input, &dbggendata.outflags, (DbgString) optstring, 
                               sizeof (optstring));

  if ( dbggendata.on ) 
    {
      /* If doing stabs request the latest stabs version */
      if ( dbggendata.outflags & DBG_SDF_STABS )
        dbggendata.outflags |= DBG_STABS_LATEST;

      /* Allow COMDAT sections processing by dbg_gen */
      dbggendata.outflags |= DBG_COMDAT;

      /* Indicate GNU frontend.  */
      dbggendata.outflags |= DBG_GNU_FRONT_END;
      
      /* Start using the dbg_gen library (this MUST be first call) */
      dbg_begin (DBG_GEN_VERSION, dbggen_error, dbggendata.outflags);

      /* store optstring for future use in dbg_file_begin */
      dbggendata.optstring = xstrdup (optstring);
      /* various null values */
      dbggendata.null_type = DBG_NULL_TYPE;
      dbggendata.null_scope = DBG_NULL_SCOPE;
      dbggendata.null_sym = DBG_NULL_SYM;
      dbggendata.null_file = DBG_NULL_FILE;
      dbg_id_stack_init (&(dbggendata.file_id_stack));
      if (!strncmp (lang_hooks.name, gnu_prefix, strlen (gnu_prefix)))
        {
           /* "GNU F95" defined in fortran/f95-lang.c is mapped to DBG_LANG_F90 in enum DbgLang. */
           if (!strcmp (lang_hooks.name, "GNU F95"))
             dbggendata.lang_flag = DBG_LANG_F90; 
           else
             dbggendata.lang_flag = dbg_lang (lang_hooks.name + strlen (gnu_prefix));
        }
      else 
        dbggendata.lang_flag = DBG_LANG_C99;
      forward_bind_types = NULL_TREE;
    }
}

/* Initialize dbg_gen for generating debug information.  */
static void 
dbg_gen_init (const char *filename)
{
  dbg_gen_init_1 ();
  dbg_gen_start_main_file (filename);
  dbg_gen_init_dwarf_base_types ();
  
  return;
}

/* Return the debugger name for a given type.  */
static const char *
dbg_gen_get_type_debug_name (tree type)
{
  const char *name;
  tree type_name = TYPE_NAME (type);
  
  if (lang_hooks.tree_inlining.anon_aggr_type_p (type))
    name = "";
  else if (type_name && (TREE_CODE (type_name) == TYPE_DECL)
           && (DECL_IGNORED_P (type_name))) 
    name = "";
  else 
    name = get_type_name (type);
  
  return name;
}

static DbgScopeID
dbg_gen_context_ScopeID (tree node)
{
  tree context;
  DbgScopeID scope_id;
  
  if (DECL_P (node))
    context = DECL_CONTEXT (node);
  else if (TYPE_P (node))
    context = TYPE_CONTEXT (node);
  else 
      gcc_unreachable ();
  
  if (context == NULL)
    return dbggendata.global_scope;
  
  switch (TREE_CODE (context))
    {
    case TRANSLATION_UNIT_DECL:
      scope_id = dbggendata.global_scope;
      break;
    case RECORD_TYPE: 
    case UNION_TYPE:
      scope_id = dbg_get_type_scope (dbg_gen_get_TypeID (context));
      break;
    case NAMESPACE_DECL:
      scope_id = dbg_gen_CC_nspace_ScopeID (context);
      break;
    default:
      scope_id = dbggendata.cur_scope;
      break;
    }
  
  return scope_id;
}


/* Handle forward binding of incomplete types.  
   Note that since types that are completed in a function scope
   (or subscopes) are available before conversion of FUNCTION_DECL 
   tree into SUNIR, forward binding is needed only for types that
   are completed in the global scope.  */

static DbgTypeID 
dbg_gen_forward_bind_TypeID (tree type)
{
  DbgTypeID ret;
  const char *name;
  DbgScopeID scope;

  gcc_assert (!COMPLETE_TYPE_P (type));
  name = dbg_gen_get_type_debug_name (type);
  scope = dbg_gen_context_ScopeID (type);
  switch (TREE_CODE (type))
    {
    case ENUMERAL_TYPE:
      ret = dbg_type_enum_forward (dbggendata.file, scope, name);
      break;
    case RECORD_TYPE:
      if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
        ret = dbg_type_class_forward (dbggendata.file, scope, name);
      else 
        ret = dbg_type_struct_forward (dbggendata.file, scope, name);
      break;
    case UNION_TYPE:
      if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
        ret = dbg_type_class_forward (dbggendata.file, scope, name);
      else 
        ret = dbg_type_union_forward (dbggendata.file, scope, name);
      break;
    default:
        gcc_unreachable ();
    }
      
  TYPE_IR_DBG_GEN_1 (type) = ret.file_id;
  TYPE_IR_DBG_GEN_2 (type) = ret.type_no;
  forward_bind_types = tree_cons (NULL, type, forward_bind_types);

  return ret;
}

/* Helper for dbg_gen_get_TypeID(). Creates DbgTypeID for 
 complete enum type.  */
static DbgTypeID 
dbg_gen_enum_TypeID (tree type)
{
  DbgTypeID ret;
  DbgScopeID scope;
  tree value;
  tree fields = TYPE_VALUES (type);
    
  gcc_assert (COMPLETE_TYPE_P (type));
  scope = dbg_gen_context_ScopeID (type);
  /* DWARF2 does not provide a way of indicating whether or
     not enumeration constants are signed or unsigned.  GDB
     always assumes the values are signed, so we output all
     values as if they were signed.  That means that
     enumeration constants with very large unsigned values
     will appear to have negative values in the debugger.  */
  ret = dbg_type_enum_begin (dbggendata.file, scope, get_type_name (type), 
                             dbg_gen_get_TypeID (integer_type_node));
  while (fields != NULL_TREE)
    {
      value = TREE_VALUE (fields);
      if (host_integerp (value, TYPE_UNSIGNED (TREE_TYPE (value))))
        {
          dbg_type_enum_name (ret, IDENTIFIER_POINTER (TREE_PURPOSE (fields)), 
                              tree_low_cst (value, (tree_int_cst_sgn (value) > 0)));
        }
      fields = TREE_CHAIN (fields);
    }
  dbg_type_enum_end (ret);

  return ret;
}

static int
dbg_gen_get_field_offset (tree decl)
{
  int offset;
  
  if (TREE_CODE (DECL_FIELD_OFFSET (decl)) == INTEGER_CST)
    offset = (TREE_INT_CST_LOW (DECL_FIELD_OFFSET (decl)) * BITS_PER_UNIT
              + TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (decl)));
  else
    offset = TREE_INT_CST_LOW (DECL_FIELD_BIT_OFFSET (decl));
  
  return offset;
}

static DbgTypeID 
dbg_gen_get_field_TypeID (tree decl)
{
  if (DECL_BIT_FIELD_TYPE (decl)) 
    return  dbg_gen_get_TypeID (DECL_BIT_FIELD_TYPE (decl));
  else
    return dbg_gen_get_TypeID (TREE_TYPE (decl));
}


/* Create and return DbgTypeID for C struct/union type.  */
static DbgTypeID 
dbg_gen_C_struct_union_TypeID (tree type)
{
  DbgTypeID ret;
  tree tmp = TYPE_FIELDS (type);
  const char *name;
  DbgScopeID scope;

  name = get_type_name (type);
  scope = dbg_gen_context_ScopeID (type);
  if (TREE_CODE (type) == RECORD_TYPE) 
    ret = dbg_type_struct_begin (dbggendata.file, scope, name, get_type_size (type));
  else
    ret = dbg_type_union_begin (dbggendata.file, scope, name, get_type_size (type));
  
  TYPE_IR_DBG_GEN_1 (type) = ret.file_id; /* set type for future quick reference */
  TYPE_IR_DBG_GEN_2 (type) = ret.type_no; /* set type for future quick reference */
  
  while (tmp)
    { 
      if (TREE_CODE (tmp) == FIELD_DECL)
        {
          const char * field_name;
          char fld_name_buf[64];
          int size;
          int offset;
          DbgTypeID fld_type;
          
          if (DECL_BIT_FIELD_TYPE (tmp))
            {
              if (DECL_SIZE (tmp) && TREE_CODE (DECL_SIZE (tmp)) == INTEGER_CST) 
                size = TREE_INT_CST_LOW (DECL_SIZE (tmp));
              else
                size = get_type_size (TREE_TYPE (tmp));
            }
          else 
            size = 0; /* Bit size needed only for bitfields.  */
          
          offset = dbg_gen_get_field_offset (tmp);
          field_name = get_ir_field_name (tmp, offset, fld_name_buf);
          fld_type = dbg_gen_get_field_TypeID (tmp);
          
          if (TREE_CODE (type) == RECORD_TYPE) 
            dbg_type_struct_field (ret, field_name, fld_type, offset, size);
          else
            dbg_type_union_field (ret, field_name, fld_type, size);
        }
      tmp = TREE_CHAIN (tmp);
    }
  if (TREE_CODE (type) == RECORD_TYPE) 
    dbg_type_struct_end (ret);
  else
    dbg_type_union_end (ret);  
  
  return ret;
}

/* Compute DbgTypeFlags for a given tree type.  */
static DbgTypeFlags
dbg_gen_CC_type_flags (tree type)
{
  DbgTypeFlags flags = 0;
  enum tree_code code = TREE_CODE (type);
  tree context = TYPE_CONTEXT (type);

  if (lang_hooks.tree_inlining.anon_aggr_type_p (type))
    flags |= DBG_ANON;
  if (code == UNION_TYPE)
    flags |= DBG_UNION;
  else if (code == RECORD_TYPE)
    {
      if (CLASSTYPE_DECLARED_CLASS (type))
        flags |= DBG_CLASS;
      else 
        flags |= DBG_STRUCT;
    }
  if (TYPE_BINFO (type) && BINFO_VIRTUAL_P (TYPE_BINFO (type)))
    flags |= DBG_VIRTUAL;
  if (context && IS_AGGR_TYPE_CODE (TREE_CODE (context)))
    flags |= DBG_NESTED;
    
  return flags;
}

/* Compute DbgTypeFlags for a given struct/union/class data/method
   member.  */
static DbgTypeFlags
dbg_gen_CC_member_type_flags (tree decl)
{
  DbgTypeFlags flags = 0;

  if (TREE_PRIVATE (decl))
    flags |= DBG_PRIVATE;
  else if (TREE_PROTECTED (decl))
    flags |= DBG_PROTECTED;
  else
    flags |= DBG_PUBLIC;
  
  return flags;
  
}

/* Compute DbgTypeFlags for C++ struct/union/class data member.  */
static DbgTypeFlags
dbg_gen_CC_data_member_type_flags (tree decl)
{
  DbgTypeFlags flags = 0;

  flags = dbg_gen_CC_member_type_flags (decl);
  if (DECL_MUTABLE_P (decl))
    flags |= DBG_MUTABLE;
  if (TREE_STATIC (decl))
    flags |= DBG_FIELD_STATIC;
  
  return flags;
}

/* Generate debug information for C++ struct/union/class data members.  
   This handles 
   1. Static fields
   2. Instance variables
   3. Type definitions, e.g nested class.  */
static void
dbg_gen_CC_data_members (tree type)
{
  DbgTypeID class_type_id;
  tree tmp = TYPE_FIELDS (type);
  
  class_type_id = dbg_gen_get_TypeID (type);
  while (tmp)
    { 
      if (!DECL_IGNORED_P (tmp))
        {
          if ((TREE_CODE (tmp) == FIELD_DECL) || (TREE_CODE (tmp) == VAR_DECL))
            dbg_gen_CC_field_decl_member (class_type_id, tmp);
          else if (TREE_CODE (tmp) == TYPE_DECL)
            {
              if (!is_user_defined_typedef (tmp) 
                  && (TREE_TYPE (tmp) != type))
                dbg_gen_CC_type_decl_member (class_type_id, tmp);
            }
        }
      tmp = TREE_CHAIN (tmp);
    }
  return;
}

static void
dbg_gen_CC_field_decl_member (DbgTypeID class_type_id, tree decl)
{
  const char * field_name;
  char fld_name_buf[64];
  int size;
  int offset;
  DbgTypeID fld_type;
  DbgTypeFlags fld_type_flags;
  DbgSymID fld_sym_id;
  
  
  if (TREE_CODE (decl) == FIELD_DECL)
    {
      offset = dbg_gen_get_field_offset (decl);
      if (DECL_NAME (decl))
        field_name = get_ir_field_name (decl, offset, fld_name_buf);
      else 
        field_name = "";
      fld_type = dbg_gen_get_field_TypeID (decl);
      
      if (DECL_BIT_FIELD_TYPE (decl))
        {
          if (DECL_SIZE (decl) && TREE_CODE (DECL_SIZE (decl)) == INTEGER_CST) 
            size = TREE_INT_CST_LOW (DECL_SIZE (decl));
          else
            size = get_type_size (TREE_TYPE (decl));
        }
      else 
        size = 0; /* Bit size needed only for bit fields.  */
    }
  else 
    { 
      offset = size = 0;
      fld_type = dbg_gen_get_TypeID (TREE_TYPE (decl));          
      if (DECL_NAME (decl) && IDENTIFIER_POINTER (DECL_NAME (decl)))
        field_name = lang_hooks.decl_printable_name (decl, 0);
      else 
        field_name = "";
    }
  fld_type_flags = dbg_gen_CC_data_member_type_flags (decl);
  fld_sym_id = dbg_type_class_field (class_type_id, field_name, fld_type, offset, 
                                     size, fld_type_flags);
  
  /* Mark artificial member fields as compiler generated.  */
  if (DECL_ARTIFICIAL (decl))
    {
      dbg_sym_set_flags (fld_sym_id, (dbg_sym_get_flags (fld_sym_id) | DBG_SYNTHETIC));
    }
  
  if (fld_type_flags & DBG_FIELD_STATIC)
    {
      dbg_sym_set_flags (fld_sym_id, DBG_EXTERN_UNDEF);
      dbg_sym_linker_name (fld_sym_id, get_ir_name (decl));  
    }

  return;
}

/* Generate debug information for type declarations in a C++ class.
   Handles nested unions/structs/class declarations.  */
static void
dbg_gen_CC_type_decl_member (DbgTypeID class_type_id, tree type_decl)
{
  DbgTypeFlags flags;
  DbgTypeID type_id;
  enum tree_code code;
  
  code = TREE_CODE (TREE_TYPE (type_decl));
  
  if (!((code == RECORD_TYPE) || (code == UNION_TYPE)))
    return;
  
  if (type_decl != TYPE_STUB_DECL (TREE_TYPE (type_decl)))
    return;
  
  flags = dbg_gen_CC_data_member_type_flags (type_decl);
  type_id = dbg_gen_get_TypeID (TREE_TYPE (type_decl));
  dbg_type_class_nested (class_type_id, type_id, flags);

  return;
}


/* Compute DbgTypeFlags for C++ class method member.  */
static DbgTypeFlags 
dbg_gen_CC_method_member_type_flags (tree decl)
{
  DbgTypeFlags flags = 0;
  
  flags = dbg_gen_CC_member_type_flags (decl);
  if (DECL_PURE_VIRTUAL_P (decl))
    flags |= DBG_PURE_VIRTUAL;
  if (DECL_VIRTUAL_P (decl))
    flags |= DBG_VIRTUAL;
  if (DECL_ARTIFICIAL (decl))
    flags |= DBG_SYNTHETIC;
  if (DECL_INLINE (decl))
    flags |= DBG_INLINE; 

  return flags;
}

/* Generate debug information for C++ class methods.  */
static void
dbg_gen_CC_method_members (tree type)
{
  const char *linkage_name;
  const char *debug_name;
  DbgTypeID method_type;
  DbgVtIndex vt_index;
  DbgTypeFlags flags;
  DbgTypeID class_type_id;
  tree method; 
  DbgSymID sym_id;
  
  method = TYPE_METHODS (type);
  class_type_id = dbg_gen_get_TypeID (type);
  while (method)
    {
      if (!(DECL_ABSTRACT_ORIGIN (method) 
            /* || decl_ultimate_origin (method) != NULL */
            || DECL_IGNORED_P (method)))
        {
          /* see cp/mangle.c, g++ adds special mark for internal methods.
             GCC uses unmangled names in debug info and does not
             care about this special mark in mangled names.
             gccfss uses cg and dbggen, which still depend on
             mangled names.  So, we need to remove this special mark.
          */
          bool special_mangled_name = false;
          linkage_name = get_ir_name (method);
          /* use only mangled name, no trailing " *INTERNAL*" */
          if (DECL_LANG_SPECIFIC (method)
	        && (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (method)
	            || DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (method)))
            {
              char *p = strchr (linkage_name, ' ');
              if (p && p != linkage_name)
                {
                  int new_length = (p - linkage_name);
                  char *mangled_name = (char*) xmalloc (new_length + 1);
                  memcpy (mangled_name, linkage_name, new_length);
                  mangled_name[new_length] = 0;
                  linkage_name = mangled_name;
                  special_mangled_name = true;
                }
            }
          if (DECL_NAME (method) && IDENTIFIER_POINTER (DECL_NAME (method)))
            debug_name = lang_hooks.decl_printable_name (method, 0);
          else
            debug_name = "";
          method_type = dbg_gen_get_TypeID (TREE_TYPE (method));
          flags = dbg_gen_CC_method_member_type_flags (method);
          if (DECL_VIRTUAL_P (method) && DECL_VINDEX (method) 
              && host_integerp (DECL_VINDEX (method), 0))
            vt_index = tree_low_cst (DECL_VINDEX (method), 0);
          else
            vt_index = 0;
          flags |= (DBG_SYM_VISIBLE | DBG_SYM_DIFFMBR);
          sym_id = dbg_type_class_method (class_type_id, debug_name, method_type,
                                          vt_index, flags);
          dbg_sym_linker_name (sym_id, linkage_name); 
          DECL_DBG_SYM_ID_1 (method) = sym_id.scope_id;
          DECL_DBG_SYM_ID_2 (method) = sym_id.sym_no;
          if (special_mangled_name)
            free ((void*) linkage_name);
        }
      
      method = TREE_CHAIN (method);
    }

  return;
}

/* Debug information for direct non virtual inheritance.  */
static void
dbg_gen_CC_direct_base_classes (tree type)
{
  int i, base_offset, v_offset;
  tree base, access_node;
  DbgTypeFlags flags;
  DbgTypeID class_type_id = dbg_gen_get_TypeID (type);
  tree binfo = TYPE_BINFO (type);
  DbgTypeID base_type_id;  

  if (binfo)
    {
      access_node = access_public_node;

      for (i = 0; BINFO_BASE_ITERATE (binfo, i, base); i++)
        {
          flags = DBG_PUBLIC;
          if (BINFO_BASE_ACCESSES (binfo))
            {
              access_node = BINFO_BASE_ACCESS (binfo, i);
              if (access_node == access_private_node)
                flags = DBG_PRIVATE;
              else if (access_node == access_protected_node)
                flags = DBG_PROTECTED;
            }
          base_type_id = dbg_gen_get_TypeID (BINFO_TYPE (base));
          base_offset = tree_low_cst (BINFO_OFFSET (base), 0);
          if (BINFO_VIRTUAL_P (base))
            {
              v_offset = tree_low_cst (BINFO_VPTR_FIELD (base), 0);
              flags |= DBG_VIRTUAL;
            }
          else 
            v_offset = 0;

          dbg_type_class_base (class_type_id, base_type_id, base_offset, v_offset, flags);
        }
    }
  return;
}

/* Create and return DbgTypeID for C++ struct/union/class type.  
 Static members are VAR_DECLs, while non static members are FIELD_DECLs.  */
static DbgTypeID
dbg_gen_CC_struct_union_TypeID (tree type)
{
  DbgTypeID ret;
  DbgScopeID scope;
  DbgTypeFlags flags;
  const char *name;

  name = dbg_gen_get_type_debug_name (type);
  flags = dbg_gen_CC_type_flags (type);
  scope = dbg_gen_context_ScopeID (type);
  ret = dbg_type_class_begin (dbggendata.file, scope, name, get_type_size (type),
                              0, 0, flags);
  TYPE_IR_DBG_GEN_1 (type) = ret.file_id; /* set type for future quick reference */
  TYPE_IR_DBG_GEN_2 (type) = ret.type_no; /* set type for future quick reference */

  dbg_gen_CC_direct_base_classes (type);
  dbg_gen_CC_data_members (type);
  if (flags & (DBG_CLASS | DBG_STRUCT))
    dbg_gen_CC_method_members (type);

  /* GNU extension: Record what type our vtable lives in.  */
  if (TYPE_VFIELD (type))
    {
      tree vtype =  DECL_FCONTEXT (TYPE_VFIELD (type));
      dbg_type_class_gnu_containing_type (dbg_gen_get_TypeID (type),
                                          dbg_gen_get_TypeID (vtype));
    }
   dbg_type_class_end (ret);

   return ret;
}

/* Return dbg scope id of NAMESPACE_DECL.  Return global scope id if 
   not C++ compilation.  */
static DbgScopeID
dbg_gen_CC_nspace_ScopeID (tree decl)
{
  DbgScopeID scope_id, context_id;
  DbgFileID file_id;
  expanded_location xloc;
  const char *debug_name;
  DbgTypeID type_id;
  
  gcc_assert (TREE_CODE (decl) == NAMESPACE_DECL);

  if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
    {
      if (lang_hooks.decls.global_namespace_decl_p (decl))
        scope_id = dbggendata.global_scope;
      else 
        {
          if (!NSPACE_DBG_TYPE_ID_SET_P (decl))
            {
              xloc = expand_location (DECL_SOURCE_LOCATION (decl));
              file_id = dbg_gen_lookup_filename (xloc.file);
              context_id = dbg_gen_context_ScopeID (decl);
              debug_name = lang_hooks.decl_printable_name (decl, 0);
              type_id = dbg_type_namespace (file_id, context_id, debug_name, 0);
              if (DECL_NAME (decl) && (IDENTIFIER_POINTER (DECL_NAME (decl))))
                dbg_type_user_name (type_id, IDENTIFIER_POINTER (DECL_NAME (decl)));
              NSPACE_DBG_TYPE_ID_1 (decl) = type_id.file_id;
              NSPACE_DBG_TYPE_ID_2 (decl) = type_id.type_no;
            }
          scope_id = dbg_get_type_scope (__extension__(DbgTypeID) {NSPACE_DBG_TYPE_ID_1 (decl),
                                                      NSPACE_DBG_TYPE_ID_2 (decl)});
        }
    }
  else 
    scope_id = dbggendata.global_scope;
  
  return scope_id;
}

/* TRUE if VAR_DECL was defined in C/C++ global scope.  */
static bool
dbg_gen_global_scope_var_p (tree decl)
{
  bool result = false;
  
  if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
    { /* C++  */
      tree scope = DECL_CONTEXT (decl);
          
      if (scope == NULL_TREE)
        result = true;
      else if (TREE_CODE (scope) == NAMESPACE_DECL)
        {
          if (lang_hooks.decls.global_namespace_decl_p (scope))
            result = true;
        }
    }
  else 
    {  /* C  */
      if (DECL_FILE_SCOPE_P (decl))
        result = true;
    }
  
  return result;
}

/* Helper for dbg_gen_get_TypeID().
 Creates dbg_gen TypeID for a complete union/struct type.  */
static DbgTypeID 
dbg_gen_struct_union_TypeID (tree type)
{
  gcc_assert (COMPLETE_TYPE_P (type));
  if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
    return dbg_gen_CC_struct_union_TypeID (type);
  else 
    return dbg_gen_C_struct_union_TypeID (type);
}

/* Helper for dbg_gen_get_TypeID().
 * Creates dbg_gen TypeID for a GNU qualifed type. */
static DbgTypeID
dbg_gen_make_qualified_TypeID (tree type)
{
  DbgTypeID ret;
  
  if (!TYPE_QUALS (type))
    abort();
  
  ret = dbg_gen_get_TypeID (TYPE_MAIN_VARIANT (type));
  if (TYPE_READONLY (type)) 
    {
      ret = dbg_type_const (dbggendata.file, ret);      
    }
  if (TYPE_VOLATILE (type))
    {
      ret = dbg_type_volatile (dbggendata.file, ret);
    }
  if (dbggendata.lang_flag != DBG_LANG_ANSI_CC)
  {  /* C++ does not support restrict type qualifier.  */
    if (TYPE_RESTRICT (type))
      {
        ret = dbg_type_restrict (dbggendata.file, ret);
      }
  }
  TYPE_IR_DBG_GEN_1 (type) = ret.file_id;
  TYPE_IR_DBG_GEN_2 (type) = ret.type_no;
  
  return ret;
}
/* Returns TRUE if type is a DBGGEN base type */
static int
is_dwarf_base_type (tree type)
{
  switch (TREE_CODE (type))
    {
    case ERROR_MARK:
    case VOID_TYPE:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case COMPLEX_TYPE:
    case BOOLEAN_TYPE:
      return 1;

    case ARRAY_TYPE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
    case ENUMERAL_TYPE:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    case LANG_TYPE:
    case VECTOR_TYPE:
      return 0;
      
    default:
      gcc_unreachable ();
    } 
  
  return 0;
  
}
/* Create DbgTypeIDs for dwarf base types in fixed order.
   This approach seems to fix all tricky interractions of 
   precompiled header files and dbggen.  */
static void
dbg_gen_init_dwarf_base_types (void)
{
  enum tree_index i;
  tree type;
  enum integer_type_kind j;
  
  if (!(dbggendata.on && gate_generate_ir ()))
    return;
  
  /* Integer types.  */
  for (j = itk_char; j < itk_none; j++)
    {
      type = integer_types[j];
      TYPE_IR_DBG_GEN_1 (type) = TYPE_IR_DBG_GEN_2 (type) = 0;
      dbg_gen_get_base_TypeID (type);
    }
  /* Float types.  */
  for (i = TI_FLOAT_TYPE; i <= TI_LONG_DOUBLE_TYPE; i++)
    {
      type = global_trees[i];
      TYPE_IR_DBG_GEN_1 (type) = TYPE_IR_DBG_GEN_2 (type) = 0;
      dbg_gen_get_base_TypeID (type);
    }
  /* Complex types.  */
  for (i = TI_COMPLEX_INTEGER_TYPE; 
       i <= TI_COMPLEX_LONG_DOUBLE_TYPE;
       i++)
    {
      type = global_trees[i];
      TYPE_IR_DBG_GEN_1 (type) = TYPE_IR_DBG_GEN_2 (type) = 0;
      dbg_gen_get_base_TypeID (type);
    }  
  return;
}

/* Identify explicit(user defined) typedefs. */
static bool
is_user_defined_typedef (tree type_decl)
{
  if (type_decl && (TREE_CODE (type_decl) == TYPE_DECL)
      && DECL_ORIGINAL_TYPE (type_decl) && !DECL_ARTIFICIAL (type_decl))
    return true;
  
  return false;
}

static DbgTypeID
dbg_gen_get_TypeID (tree type)
{
  DbgTypeID ret;
  tree type_decl;
  enum machine_mode mode;
  DbgScopeID scope;
    
  if (!dbggendata.on || write_symbols == NO_DEBUG)
    return dbggendata.null_type;
  
  if (TYPE_IR_DBG_GEN_1 (type) && TYPE_IR_DBG_GEN_2 (type))
    return (__extension__ (struct DbgTypeID) {TYPE_IR_DBG_GEN_1 (type), TYPE_IR_DBG_GEN_2 (type)});

  mode = TYPE_MODE (type);
  
  type_decl = TYPE_NAME (type);
  if (is_user_defined_typedef (type_decl))
    {
      if (DECL_FILE_SCOPE_P (type_decl))
        scope = dbggendata.global_scope;
      else 
        scope = dbggendata.cur_scope;
      ret = dbg_type_define (dbggendata.file, scope, get_type_name (type), 
                             dbg_gen_get_TypeID (DECL_ORIGINAL_TYPE (type_decl)));
      TYPE_IR_DBG_GEN_1 (type) = ret.file_id; /* set type for future quick reference */
      TYPE_IR_DBG_GEN_2 (type) = ret.type_no; /* set type for future quick reference */
      
      return ret;
    }
  
  /*  Type qualifiers */
  if (TYPE_QUALS (type)) 
    return dbg_gen_make_qualified_TypeID (type);

  if (is_dwarf_base_type (type))
    return  dbg_gen_get_base_TypeID (type);
  
  switch (TREE_CODE (type))
    {
    case ENUMERAL_TYPE:
      if (COMPLETE_TYPE_P (type))
        ret = dbg_gen_enum_TypeID (type);
      else 
        ret = dbg_gen_forward_bind_TypeID (type);
      break;
      
    case ARRAY_TYPE:
      {
        DbgTypeID elem_type = dbg_gen_get_TypeID (TREE_TYPE (type));
        ret = dbg_type_array_fixed (dbggendata.file, elem_type,
                                    (!TARGET_ARCH64 
                                     ? dbg_gen_get_TypeID (intSI_type_node)
                                     : dbg_gen_get_TypeID (intDI_type_node)),
                                    0, get_array_type_size (type) - 1);
      }
      break;
      
    case VECTOR_TYPE:
      {
        int size;
        if (!TYPE_SIZE (type) || TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
            || !TYPE_SIZE_UNIT (type) || TREE_CODE (TYPE_SIZE_UNIT (type)) != INTEGER_CST)
          abort ();
        
        size = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (type));
        if (size <= 4)
          ret = dbg_type_basic_float (dbggendata.file, "float", 4);
        else if (size <= 8)
          ret = dbg_type_basic_float (dbggendata.file, "double", 8);
        else if (size <= 16)
          ret = dbg_type_basic_complex (dbggendata.file, "double complex", 16);
        else if (size <= 32)
          ret = dbg_type_basic_complex (dbggendata.file, "long double complex", 32);
        else
          {
            DbgTypeID elem_type = dbg_gen_get_TypeID (TREE_TYPE (type));
            ret = dbg_type_array_fixed (dbggendata.file, elem_type,
                                        (!TARGET_ARCH64 
                                         ? dbg_gen_get_TypeID (intSI_type_node)
                                         : dbg_gen_get_TypeID (intDI_type_node)), 0,
                                        TREE_INT_CST_LOW (TYPE_SIZE (type)) /
                                        TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (type))) - 1);
          }
        break;
      }
      
    case POINTER_TYPE:
    case OFFSET_TYPE:
      ret = dbg_type_pointer (dbggendata.file, dbg_gen_get_TypeID (TREE_TYPE (type))); 
      break;
      
    case REFERENCE_TYPE:
      ret = dbg_type_ref (dbggendata.file, dbg_gen_get_TypeID (TREE_TYPE (type)));
      break;
    
    case METHOD_TYPE:
    case FUNCTION_TYPE:
      {
        DbgTypeID fn_ret_type = dbg_gen_get_TypeID (TREE_TYPE (type));
        tree parms = TYPE_ARG_TYPES (type);
        
        ret = dbg_type_func_begin (dbggendata.file, fn_ret_type, dbggendata.lang_flag);
        /* See CR #6466704, set prototype flag if parms exist */
        if (parms)
          {
            DbgTypeFlags flags = dbg_type_get_flags (ret);
            dbg_type_set_flags (ret, flags | DBG_PROTOTYPE);
          }
        /* Set the DbgTypeID of a METHOD_TYPE immediately because when generating 
         DbgTypeID for the "this" parm which is of the enclosing class type, this method
         type will be encountered again.  It is ok to do this for FUNCTION_TYPES.  */
        TYPE_IR_DBG_GEN_1 (type) = ret.file_id; 
        TYPE_IR_DBG_GEN_2 (type) = ret.type_no; 

        if (parms && parms != void_list_node)
          {
            for ( ; parms && parms != void_list_node; parms = TREE_CHAIN (parms))
              if (TREE_CODE (TREE_VALUE (parms)) != VOID_TYPE)
                {
                  DbgTypeID param_type = dbg_gen_get_TypeID (TREE_VALUE (parms));
                  dbg_type_func_formal (ret, param_type);
                }
            
            if (!parms)
              {
                dbg_type_func_varargs (ret);
              }
          }
        dbg_type_func_end (ret);
      }
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (COMPLETE_TYPE_P (type))
        ret = dbg_gen_struct_union_TypeID (type);
      else 
        ret = dbg_gen_forward_bind_TypeID (type);
      
      break;
      
    default:
      debug_tree (type);
      abort();
    }
  
  TYPE_IR_DBG_GEN_1 (type) = ret.file_id; /* set type for future quick reference */
  TYPE_IR_DBG_GEN_2 (type) = ret.type_no; /* set type for future quick reference */
  
  return ret;
}
 
/* Generate DbgTypeID for INTEGER_TYPE, BOOLEAN_TYPE, ENUMERAL_TYPE.  */
static DbgTypeID
dbg_gen_get_integral_TypeID (tree type)
{
  DbgTypeID ret;
  enum machine_mode mode = TYPE_MODE (type);
  DbgTypeFlags flags;
    
  if (!INTEGRAL_TYPE_P (type))
    {
      abort ();
    }
  
  if (TYPE_IR_DBG_GEN_1 (type) && TYPE_IR_DBG_GEN_2 (type))
    return (__extension__ (struct DbgTypeID) {TYPE_IR_DBG_GEN_1 (type), TYPE_IR_DBG_GEN_2 (type)});

  switch (mode)
    {
    case QImode: 
    case HImode: 
    case SImode:
    case DImode:
    case TImode:
      flags = (TYPE_UNSIGNED (type)) ? DBG_UNSIGNED : DBG_SIGNED;
      if (mode == QImode)
        { 
          if (TREE_CODE (type) == BOOLEAN_TYPE)
            flags |= DBG_BOOLEAN;
          else 
            flags |= DBG_CHAR;
        }
      ret = dbg_type_basic_integral (dbggendata.file, get_type_name (type), 
                                     GET_MODE_SIZE (mode), 0, GET_MODE_BITSIZE (mode), flags);
      break;
      
    default: gcc_unreachable ();
    }
  
  return ret;
}
 

/* Return DbgTypeID for dwarf base types aborts on all other types. */
static DbgTypeID
dbg_gen_get_base_TypeID (tree type)
{
  DbgTypeID ret;
  const char * name;
  DbgTypeFlags flags;
  enum machine_mode mode;
  unsigned mode_size_bytes;
    
  if (TYPE_IR_DBG_GEN_1 (type) && TYPE_IR_DBG_GEN_2 (type))
    return (__extension__ (struct DbgTypeID) {TYPE_IR_DBG_GEN_1 (type), TYPE_IR_DBG_GEN_2 (type)});
  
  name = get_type_name (type);
  mode = TYPE_MODE (type);
  mode_size_bytes = GET_MODE_SIZE (mode);
  flags = (TYPE_UNSIGNED (type)) ? DBG_UNSIGNED : DBG_SIGNED;  

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      ret = dbg_type_basic_integral (dbggendata.file, name, 0, 0, 0, DBG_SIGNED);
      break;
      
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
      ret = dbg_gen_get_integral_TypeID (type);
      break;
      
    case REAL_TYPE:
      switch (mode)
        {
        case SFmode: /*"float"*/
        case DFmode: /*"double"*/
        case TFmode: /*"long double"*/
          ret = dbg_type_basic_float (dbggendata.file, name, mode_size_bytes);
          break;
        default: gcc_unreachable ();
        }
      break;
      
    case COMPLEX_TYPE:
      switch (mode)
        {
        case CQImode:
        case CHImode:
        case CSImode: 
          ret = dbg_type_basic_complex (dbggendata.file, "int complex", mode_size_bytes);
          break;
        case CDImode: 
          ret = dbg_type_basic_complex (dbggendata.file, "long long complex", mode_size_bytes);
          break;
        case SCmode: 
          ret = dbg_type_basic_complex (dbggendata.file, "float complex", mode_size_bytes);
          break;
        case DCmode: 
          ret = dbg_type_basic_complex (dbggendata.file, "double complex", mode_size_bytes);
          break;
        case TCmode: 
          ret = dbg_type_basic_complex (dbggendata.file, "long double complex", mode_size_bytes);
          break;

        default: gcc_unreachable ();        
        }
      break;

    default:
      debug_tree (type);
      gcc_unreachable ();
    }
  TYPE_IR_DBG_GEN_1 (type) = ret.file_id; /* set type for future quick reference */
  TYPE_IR_DBG_GEN_2 (type) = ret.type_no; /* set type for future quick reference */
  
  return ret;
}

/* End current function definition in dbg gen.  */
static void
dbg_gen_end_function_decl (void)
{
  dbg_func_end (dbggendata.func_sym);
  dbggendata.cur_scope = dbggendata.global_scope;

  return;
}

static DbgSymFlags 
dbg_gen_get_func_decl_sym_flags (tree fn)
{
   DbgSymFlags flags = 0;
   
   if (strcmp (get_ir_name (fn), "main") == 0)
     flags |= DBG_MAIN;
   
   if (TREE_PUBLIC (fn) || globalize_flag)
     flags |= DBG_EXTERN;
   else
     flags |= DBG_FILE_STATIC;  
   
   if (DECL_ARTIFICIAL (fn))
     flags |= DBG_SYNTHETIC;
   
   /* disable for now to fix CR #6273991
    *if (DECL_DECLARED_INLINE_P (fn))
    {
    flags |= DBG_INLINE;
    flags |= DBG_SYM_COMDAT;
    flags &= ~(DBG_FILE_STATIC);
    flags |= DBG_EXTERN;
    }*/
   
   return flags;
}


/* Begin function definition in dbg gen.  */
static void 
dbg_gen_begin_function_decl (tree fn)
{
  DbgSymFlags flags;
  DbgTypeID func_type;
  expanded_location xloc;
  DbgScopeID scope;
  tree fn_type = TREE_TYPE (fn);
  tree fn_type_decl = TYPE_NAME (fn_type);
  
  /* See CR 6464325, should not use mangled name in dwarf nested scopes */
  dbggendata.func_name = IDENTIFIER_POINTER (DECL_NAME (fn));
  dbggendata.link_name = get_ir_name (fn);
  scope = dbg_gen_context_ScopeID (fn);
  xloc = expand_location (DECL_SOURCE_LOCATION (fn));
  /* See CR 6466707, dbggendata.file is not neccessarily for xloc.file. */
  dbggendata.file = dbg_gen_lookup_filename(xloc.file);
  flags = dbg_gen_get_func_decl_sym_flags (fn);
  /* Handle case where function was declared using a typedef, by 
     1. Generating a DbgTypeID for the typedef, debug information completeness.
     2. Extracting the original tree type, dbg_func_begin() must use the 
     DbgTypeID associated with this and not that of the typedef type.  */
  while (is_user_defined_typedef (fn_type_decl))
    {
      dbg_gen_get_TypeID (fn_type);
      fn_type = DECL_ORIGINAL_TYPE (fn_type_decl);
      fn_type_decl = TYPE_NAME (fn_type);
    }
  /* create the function type */
  func_type = dbg_gen_get_TypeID (fn_type);
  scope = dbg_gen_context_ScopeID (fn);
  if (!DECL_DBG_SYM_ID_SET_P (fn))
    {
      /* begin the function definition */
      /* See CR 6464325, should not use mangled name in dwarf nested classes */
      if (scope != 1 /* not global and not in namespace */
          && !(DECL_P (fn) && DECL_CONTEXT (fn) 
               && NAMESPACE_DECL == TREE_CODE (DECL_CONTEXT (fn))))
        {
          /* only declaration in the nested scope */
          dbg_func_symbol (scope, dbggendata.func_name, 
                                        func_type, flags);	
          /* the link name is at top level */
          dbggendata.func_sym = dbg_func_begin (1, dbggendata.link_name, 
                                            func_type, flags);	
        }
      else if (dbggendata.lang_flag == DBG_LANG_ANSI_CC
                && DECL_LANG_SPECIFIC (fn)
                && DECL_LANGUAGE (fn) == lang_cplusplus
                && !DECL_FUNCTION_MEMBER_P(fn)
                && strcmp(dbggendata.link_name, dbggendata.func_name))
        {
          dbggendata.func_sym = dbg_func_begin (scope, dbggendata.func_name, 
                                            func_type, flags);	
          dbg_sym_linker_name (dbggendata.func_sym, dbggendata.link_name);
        }
      else
        {
          dbggendata.func_sym = dbg_func_begin (scope, dbggendata.link_name, 
                                            func_type, flags);	
        }
      /* dbg_gen API: we must set DBG_PROTOTYPE on the type symbol */  
      if (TYPE_ARG_TYPES (TREE_TYPE (fn)))
        {
          DbgTypeID func_type = dbg_sym_get_type (dbggendata.func_sym);
          DbgTypeFlags flags = dbg_type_get_flags (func_type);
          dbg_type_set_flags (func_type, flags | DBG_PROTOTYPE);
        }

      if (DECL_NAME (fn) && IDENTIFIER_POINTER (DECL_NAME (fn)))
        dbg_sym_user_name (dbggendata.func_sym, IDENTIFIER_POINTER (DECL_NAME (fn)));
      DECL_DBG_SYM_ID_1 (fn) = dbggendata.func_sym.scope_id;
      DECL_DBG_SYM_ID_2 (fn) = dbggendata.func_sym.sym_no;
    }
  else 
    {
      dbggendata.func_sym = (__extension__ (DbgSymID) {DECL_DBG_SYM_ID_1 (fn),
                                                       DECL_DBG_SYM_ID_2 (fn)});
      flags |= (dbg_sym_get_flags (dbggendata.func_sym) | DBG_FUNC_DEF) ;
      dbg_func_sym_begin (dbggendata.func_sym, flags);
    }
  
  /* tell dbg_gen where this function was defined */
  dbg_sym_src_location (dbggendata.func_sym, dbggendata.file, xloc.line, 0);
  dbggendata.func_scope = dbg_scope_func (scope, dbggendata.func_sym);
  dbggendata.cur_scope = dbggendata.func_scope;
  
  return;
}

/* Store debug information of function DECL into dbg gen.  */
static void
dbg_gen_function_decl (tree fn)
{
  tree func_scope;
  tree func_decl = fn;
  if (dbggendata.on && gate_generate_ir ()) 
    {
      if (dbggendata.lang_flag == DBG_LANG_ANSI_CC
          && DECL_ABSTRACT_ORIGIN (fn))
        {
          func_decl = DECL_ABSTRACT_ORIGIN (fn);
          func_scope = BLOCK_ABSTRACT_ORIGIN (DECL_INITIAL (fn));
        }
      else
        {
          func_scope = DECL_INITIAL (func_decl);
        }
      dbg_gen_begin_function_decl (func_decl);
      dbg_gen_function_args (func_decl);
      if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
        { /* In c++ functions, the outermost scope is for ctor/dtors,
             go one level deeper to find function body scope.  */
          func_scope = BLOCK_SUBBLOCKS (func_scope);
        }
      dbg_gen_decls_for_scope (func_scope);
      dbg_gen_end_function_decl ();
    }
  return;
}

/* Compute Dbg gen symbol flags of a VAR_DECL.  */
static DbgSymFlags
dbg_gen_get_var_decl_sym_flags (tree decl)
{
  DbgSymFlags flags = 0;
  
  if (dbg_gen_global_scope_var_p (decl) ||
      ((DECL_CONTEXT (decl) && 
       (TREE_CODE (DECL_CONTEXT (decl)) == NAMESPACE_DECL))))
    {
      if (!TREE_PUBLIC (decl))
        flags |= DBG_FILE_STATIC;
      else 
        flags |= DBG_EXTERN;
      
      if (DECL_INITIAL (decl)) {
        /* Will be located in .data section instead of .bss section.  */
        flags |= DBG_INITIALIZED;
      }
    }
  else if (DECL_CONTEXT (decl) && 
           (TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL))
    {
      if (TREE_STATIC (decl))
        flags |= DBG_STATIC;
      else 
        flags |= DBG_STACK;
    }
  
  if (TREE_READONLY (decl))
    flags |= DBG_READONLY;
  if (DECL_ARTIFICIAL (decl))
    flags |= DBG_SYNTHETIC;
  if (DECL_EXTERNAL (decl))
    flags |= DBG_EXTERN;
  
  return flags;
}

/* Compute DbgSymFlags of a PARM_DECL.  */
static DbgSymFlags
dbg_gen_get_parm_decl_sym_flags (tree decl)
{
  DbgSymFlags flags = 0;

  if (tu_pass_by_reference (NULL, TYPE_MODE (TREE_TYPE (decl)), 
                         TREE_TYPE (decl), false))
    flags |= DBG_BY_REF;
  else 
    flags |= DBG_BY_VALUE;
  
  if (DECL_ARTIFICIAL (decl))
    flags |= DBG_SYNTHETIC;
  
  return flags;
}

/* Query dbg_gen for file id of filename, if not found,
   create entry for filename in dbg_gen and return id.
*/
static DbgFileID
dbg_gen_lookup_filename (const char *filename)
{
  DbgFileID file_id;
  
  file_id = dbg_file_search (filename);
  if (dbg_null_file (file_id))
    file_id = dbg_file_reference (filename, DBG_FILE_REFERENCE);
  
  return file_id;
}

/* TRUE if decl is a field decl of an anon union/struct.  */
bool 
ir_anon_aggr_field_decl_p (tree decl)
{
  if (DECL_VALUE_EXPR (decl))
    {
      tree object = TREE_OPERAND (DECL_VALUE_EXPR (decl), 0);
      if (lang_hooks.tree_inlining.anon_aggr_type_p (TREE_TYPE (object)))
        return true;
    }
  return false;
}

/* Return linker name that dbggen will use for static VAR_DECL.
   dbggen expects that a linker symbol (label) matching this name
   exists in the data sections. 
   In most cases the linker name is the same as the ir name. 
   Exceptions are fields of C++ static anonymous unions.
   In g++ these fields are treated as static VAR_DECLs. But no
   linker symbol will be generated for them in the assembly.
   In this case linkage name of the union variable is used,
   because a linker symbol is always generated for it, and also
   the fields share the same address as the union.
*/
static const char *
dbg_gen_decl_linkage_name (tree decl)
{
  const char *linkage_name = get_ir_name (decl);  

  if (ir_anon_aggr_field_decl_p (decl))
    {
      tree object = TREE_OPERAND (DECL_VALUE_EXPR (decl), 0);
      linkage_name = get_ir_name (object);
    }
  return linkage_name;
}

/* Generate debug info for VAR_DECL in given scope. 
   This is done only for decls which have a corresponding VAR_LEAF.
   The dbg_sym_id of the VAR_LEAF is updated here.
   The 3 different ways a VAR_DECL could be referrenced are supported as 
   1. debug_name : name visible to the user in the debugger
   2. ir_leaf_name : possibly mangled name of corresponding VAR_LEAF
   3. linkage name : name corresponding to linker symbol (for static vars only).
*/
static void
dbg_gen_var_decl (tree decl, DbgScopeID scope)
{
  DbgSymFlags flags;
  tree type;
  int size;
  DbgFileID file_id;
  DbgTypeID type_id;
  expanded_location xloc;
  const char *debug_name;  
  const char *ir_leaf_name;
  const char *linkage_name;
  LEAF *var_leaf = NULL;
  ir_ADDRESS *ir_addr;
  SEGMENT *seg;
  DbgSymID dbg_sym_id = dbg_get_null_sym ();
  TYPE argtype;
  IR_TYPE_NODE *ir_type;

  if (DECL_IGNORED_P (decl))
    return;

  if (DECL_DBG_SYM_ID_SET_P (decl))
    return;

  type = TREE_TYPE (decl);
  if (!lang_hooks.tree_inlining.anon_aggr_type_p (type)) {
    /* Cannot generate debug information for nameless variables except
       anonymous struct/union variables.  */
    if (! (DECL_NAME (decl) && IDENTIFIER_POINTER (DECL_NAME (decl))))
      return;
  }

  if (ir_language == FORTRAN && DECL_HAS_VALUE_EXPR_P (decl))
    {
      /* TODO need to generate proper debug info for value-expr in such decls.
         ignore for now */
      return;
    }

  argtype = map_gnu_type_to_TYPE (type);
  ir_type = map_gnu_type_to_IR_TYPE_NODE (type);
  type_id = dbg_gen_get_TypeID(type);
  size = get_type_size(type);
  xloc = expand_location (DECL_SOURCE_LOCATION (decl));
  file_id = dbg_gen_lookup_filename (xloc.file);
  flags = dbg_gen_get_var_decl_sym_flags (decl);
  ir_leaf_name = get_ir_name (decl);

  if (lang_hooks.tree_inlining.anon_aggr_type_p (type))
    debug_name = ""; 
  else 
    debug_name = IDENTIFIER_POINTER (DECL_NAME (decl));

  if (flags &  DBG_STACK)
    {
      /* Local var.  */
      if (DECL_REGISTER (decl) && DECL_ASSEMBLER_NAME_SET_P (decl))
        {
          /* Local register var.  */
          ir_leaf_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
          var_leaf = get_ir_reg_var_leaf (ir_leaf_name, decode_reg_name (ir_leaf_name),
                                          argtype, ir_type);
          if (var_leaf != NULL) 
            {
              IR_OFFSZ reg_number;
              ir_addr = &(var_leaf->val.addr);
              seg = ir_addr->seg;
              if (seg->descr.content != REG_SEG)
                error ("VAR_LEAF of register variable not in register segment");
              reg_number = ir_addr->offset;
              if (seg->descr.class == FREG_SEG) 
                reg_number += 32;
              dbg_sym_id = dbg_var_reg (scope, debug_name, type_id, size, flags, 
                                        reg_number, file_id, xloc.line, 0);
            }
        }
      else 
        {
          /* Local stack var.  */
          var_leaf = get_ir_auto_var_leaf (ir_leaf_name, DECL_IR_OFFSET (decl),
                                           argtype.size, argtype.align, 0, argtype, 
                                           ir_type);
          if (var_leaf != NULL) 
            {
              ir_addr = &(var_leaf->val.addr);
              seg = ir_addr->seg;
              if (seg->descr.content != STG_SEG)
                error ("VAR_LEAF of stack variable not in memory segment");
              dbg_sym_id = dbg_var_stack (scope, debug_name, type_id, size, flags, 
                                          (seg->offset + ir_addr->offset), file_id,
                                          xloc.line, 0);
            }
        }
    }
  else if (flags & DBG_STATIC)
    {
      /* Local static var.  */
      var_leaf = get_ir_extern_var_leaf (ir_leaf_name, 0, argtype, ir_type);
      if (var_leaf != NULL) 
        {
          linkage_name = dbg_gen_decl_linkage_name (decl);
          dbg_sym_id = dbg_var_static_local (scope, debug_name, type_id, size, flags,
                                             linkage_name, NULL, file_id, 
                                             xloc.line, 0);
        }
    }
  else if (flags & DBG_FILE_STATIC)
    {
      /* File static variable.  */
      var_leaf = get_ir_extern_var_leaf (ir_leaf_name, 0, argtype, ir_type);
      if (var_leaf != NULL)
        {
          linkage_name = dbg_gen_decl_linkage_name (decl);
          dbg_sym_id = dbg_var_static (scope, debug_name, type_id, size, flags, linkage_name, 
                                       file_id, xloc.line, 0);
        }
    }
  else if (flags & DBG_EXTERN)
    {
      /* Global variable accessible outside current module..  */ 
      var_leaf = get_ir_extern_var_leaf (ir_leaf_name, 0, argtype, ir_type);
      /* An unused global variable will not have an IR leaf, but it will still be emitted 
         into the sidedoor file, so generate debug information for it. */
      linkage_name = dbg_gen_decl_linkage_name (decl);
      /* gdb works with linkage_name here for global variables */
      dbg_sym_id = dbg_var_static (scope, linkage_name, type_id, size, flags, linkage_name, 
                                   file_id, xloc.line, 0);
    }
  
  
  if (!dbg_null_sym (dbg_sym_id))
    {
      if (var_leaf != NULL)
        {
          var_leaf->dbg_sym_id = build_ir_proc_dbgsym (dbg_sym_id);
        }
      DECL_DBG_SYM_ID_1 (decl) = dbg_sym_id.scope_id;
      DECL_DBG_SYM_ID_2 (decl) = dbg_sym_id.sym_no;  
    }
  
  return;
}

/* Create debug information for global variable.  
   TODO: Handle NESTED NAMESPACE SCOPE globals.  */
void 
dbg_gen_global_var_decl (tree decl)
{
  if (dbggendata.on && gate_generate_ir ())
    {
      /* Dont generate debug info for extern variables declarations.  */
      if (DECL_EXTERNAL (decl))
        return;

      /* Don't generate debug info for 128 bits variables delaratons. */
      if (sunir_check_128bits_handling (TREE_TYPE (decl)))
        return;
      
      if (!dbg_gen_global_scope_var_p (decl)) 
        {
          /* cannot do function static variables yet, cg assertion. */
          if ((DECL_CONTEXT (decl) && 
               (TREE_CODE (DECL_CONTEXT (decl)) == FUNCTION_DECL)))
            return;
          dbg_gen_var_decl (decl, dbg_gen_context_ScopeID (decl));
        }
      else
        dbg_gen_var_decl (decl, dbggendata.global_scope);
    }
  
  return;
}


static void
dbg_gen_global_decl (tree decl)
{
  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (TREE_PUBLIC (decl))
        dbg_gen_global_var_decl (decl);
    }
  return;
}

static void 
dbg_gen_decls_for_scope (tree scope)
{
  tree decl;
  tree subblocks;
  DbgScopeID parent_scope_id;
  
  if (scope == NULL_TREE)
    return;
  
  parent_scope_id = dbggendata.cur_scope;

  if (TREE_USED (scope))
    {
      if (ir_gen_scope_triple_p () && BLOCK_SCOPE_ID (scope)) 
        {
          dbggendata.cur_scope = dbg_scope_block (parent_scope_id);
          ir_update_scope_triples (scope, dbggendata.cur_scope, parent_scope_id);
        }
      for (decl = BLOCK_VARS (scope); decl != NULL_TREE; decl = TREE_CHAIN (decl))
        {
          if (DECL_ARTIFICIAL (decl)) /* Ignore compiler generated temps */
            continue;
          
          /* Ignore variables declared in inline functions.  */
          if (DECL_CONTEXT (decl) 
              && cgraph_function_possibly_inlined_p (DECL_CONTEXT (decl)))
            continue;
          
          if (TREE_CODE (decl) == VAR_DECL)
            {
              dbg_gen_var_decl (decl, dbggendata.cur_scope);
            }
        }
    }  /* if TREE_USED (scope).  */
  
  /* The subblocks of a block with no var declarations may themselves
     have var declarations. The scope id of the closest ancestor with var
     declarations will be their parent scope id.  */
  for (subblocks = BLOCK_SUBBLOCKS (scope);
       subblocks != NULL_TREE;
       subblocks = BLOCK_CHAIN (subblocks))
    {
      dbg_gen_decls_for_scope (subblocks);
    }

  dbggendata.cur_scope = parent_scope_id;

  return;
}

/* Resolve forward binding of types in dbggen.  */
static void
dbg_gen_resolve_forward_bind_types (void)
{
  tree list, type;
  DbgTypeID forward_type_id, type_id;
  DbgString forward_type_name, type_name;
  
  list = forward_bind_types;
  while (list != NULL_TREE)
    {
      type = TREE_VALUE (list);
      if (! ((TREE_CODE (type) == ENUMERAL_TYPE) 
             || (TREE_CODE (type) == RECORD_TYPE) 
             || (TREE_CODE (type) == UNION_TYPE)))
          gcc_unreachable ();
      if (COMPLETE_TYPE_P (type))
        {
          forward_type_id.file_id = TYPE_IR_DBG_GEN_1 (type);
          forward_type_id.type_no = TYPE_IR_DBG_GEN_2 (type);
          TYPE_IR_DBG_GEN_1 (type) = TYPE_IR_DBG_GEN_2 (type) = 0;
          type_id = dbg_gen_get_TypeID (type);
          /* Sanity check to match name of forwarded type and completed type.  
             dbggen checks that the forwarded type and complete type are 
             of the same kind (e.g enum) before binding.  */
          forward_type_name = dbg_sym_get_name (dbg_type_get_sym (forward_type_id));
          type_name = dbg_sym_get_name (dbg_type_get_sym (type_id));
          if ((type_name != forward_type_name) && strcmp (type_name, forward_type_name))
            error ("Mismatch of forward type name and complete type name");
          if (dbggendata.lang_flag == DBG_LANG_ANSI_CC)
            dbg_type_class_forward_bind (forward_type_id, type_id);
          else 
            dbg_type_forward_bind (forward_type_id, type_id);
        }
      list = TREE_CHAIN (list);
    }
  forward_bind_types = NULL_TREE;
  return;
}

static void 
dbg_gen_type_decl (tree decl, int local)
{
  /* Used debug types are emitted elsewhere, so if no request to emit
   unused debug types, then nothing to do here.  */
  if (flag_eliminate_unused_debug_types)
    return;
  
  if (DECL_IGNORED_P (decl))
    return;
  
  if (dbggendata.on && gate_generate_ir () && !pch_file)
    {
      if (!local)
        dbg_gen_get_TypeID (TREE_TYPE (decl));
    }
  return;
}

static void 
dbg_gen_function_args (tree decl)
{
  tree args;
  DbgSymFlags flags;
  tree type;
  int size;
  const char *name;
  DbgTypeID type_id;
  expanded_location xloc;
  LEAF *leaf = NULL, *var_leaf;
  ir_ADDRESS *ir_addr;
  SEGMENT *seg;
  DbgSymID dbg_sym_id;
  DbgFileID file_id;
  TYPE argtype;
  IR_TYPE_NODE *ir_type_node;
  tree ir_lookup_type;
  
  for (args = DECL_ARGUMENTS (decl); args != NULL_TREE; args = TREE_CHAIN (args))
    {
      if (DECL_P (args) && DECL_IGNORED_P (args))
        continue;
      
      if (TREE_CODE (args) != PARM_DECL)
        continue;

      type = TREE_TYPE (args);
      ir_lookup_type = gnu_type_of_ir_param_leaf (args);
      flags = dbg_gen_get_parm_decl_sym_flags (args);

      argtype = map_gnu_type_to_TYPE (ir_lookup_type);
      ir_type_node = map_gnu_type_to_IR_TYPE_NODE (ir_lookup_type);
      name = get_ir_name (args);
      leaf = get_ir_parm_var_leaf (name, DECL_IR_OFFSET (args), argtype, ir_type_node);
      if (leaf != NULL)
        {
          type_id = dbg_gen_get_TypeID (type);
          xloc = expand_location (DECL_SOURCE_LOCATION (args));
          file_id = dbg_gen_lookup_filename (xloc.file);
          size = get_type_size (type);
          if (DECL_VALUE_EXPR (args) == NULL) 
            {
              ir_addr = &(leaf->val.addr);
              seg = ir_addr->seg;
              var_leaf = NULL;
            }
          else
            {
              /* parameter is converted and stored on stack */
              tree local = DECL_VALUE_EXPR (args);
              const char *name = get_ir_name (local);
              ir_lookup_type = TREE_TYPE (local);
              argtype = map_gnu_type_to_TYPE (ir_lookup_type);
              ir_type_node = map_gnu_type_to_IR_TYPE_NODE (ir_lookup_type);
              var_leaf = get_ir_auto_var_leaf (name, DECL_IR_OFFSET (local),
                                    argtype.size, argtype.align, 0, 
                                    argtype, ir_type_node);
              ir_addr = &(var_leaf->val.addr);
              seg = ir_addr->seg;
#if 0
              /* This will increase debugability, but disabled optimization. */
              /* Set this local variable as volatile to avoid optimization
                 and make its value visible inside the debugger. */
              if (optimize == 0)
                var_leaf->is_volatile = IR_TRUE;
#endif
            }
          dbg_sym_id = dbg_func_formal_arg (dbggendata.func_scope, name, type_id,
                                            size, flags, (seg->offset + ir_addr->offset),
                                            file_id, xloc.line, 0);
          leaf->dbg_sym_id = build_ir_proc_dbgsym (dbg_sym_id);
          DECL_DBG_SYM_ID_1 (args) = dbg_sym_id.scope_id;
          DECL_DBG_SYM_ID_2 (args) = dbg_sym_id.sym_no;
          /* If var_leaf does not have dbg_sym, set it to the dbg_sym of leaf 
           so that cg will update it. */
          if (var_leaf && leaf != var_leaf)
            var_leaf->dbg_sym_id = leaf->dbg_sym_id;
        }
    }
  
  return;
}

/* Generate scope information in IR at optimization level 0.  */
bool
dbg_gen_generate_scopes (void)
{
  return flag_tm_mode || dbggendata.on;
}

#include "gt-tree-ir-debug.h"
