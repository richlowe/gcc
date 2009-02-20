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

#ifndef __gcc_ir_common__
#define __gcc_ir_common__

#define CONSZ long long
#define OFFSZ long

#define IR_MAJOR_VERS 9
#define IR_MINOR_VERS 0
#define IR_DEV_VERS   0

typedef long long IR_OFFSZ;

typedef enum {
        IR_ERR=0, IR_ENTRYDEF, IR_EXITUSE, IR_ENTRY, IR_FPARAM,
        IR_PLUS,IR_MINUS,IR_MULT,IR_DIV,IR_REMAINDER, IR_AND, IR_OR, IR_XOR, IR_NOT,
        IR_LSHIFT, IR_RSHIFT, IR_SCALL, IR_FCALL, IR_EQ, IR_NE, IR_LE, IR_LT, IR_GE,
        IR_GT, IR_CONV, IR_COMPL, IR_NEG, IR_ADDROF, IR_IFETCH, IR_ISTORE, IR_GOTO,
        IR_CBRANCH, IR_SWITCH, IR_REPEAT, IR_ASSIGN,
        IR_PASS, IR_STMT, IR_LABELDEF, IR_INDIRGOTO, IR_FVAL, IR_LABELREF,
        IR_PARAM, IR_PCONV, IR_FOREFF,
	IR_BFEXT, IR_BFINS, IR_IBFINS,
	IR_DOALL, IR_MCALL, IR_DOACROSS, IR_ORDER, IR_ENDORDER,
	IR_ARRAYLOC, IR_SUBINDEX, IR_BOUNDS, IR_DIMINFO, 
	IR_MIN, IR_MAX,
	IR_PBRANCH,
	IR_BECOME,
	IR_PRAGMA,
	IR_DARRAYLOC,
	IR_PARRAYLOC,
	IR_PSUBINDEX,
	IR_QGT,
	IR_QGE,
	IR_QLT,
	IR_QLE,
	IR_QGL,
	IR_QUO,
	IR_FPCLASSIFY,
	IR_ISFINITE,
	IR_ISINF,
	IR_ISNAN,
	IR_ISNORMAL,
	IR_SIGNBIT,
	IR_PREFETCH,
        IR_BEGIN_SCOPE,
        IR_END_SCOPE,
        IR_MEM_BARRIER,
        IR_ASM_OUTPUT,
        IR_ASM_INPUT,
        IR_ASM_ADDRESS,
        IR_ASM_CLOBBER,
        IR_ASM_STMT,
	IR_LAST_OP /* Please dont add any ops after this */
} IR_OP;

typedef enum param_mode {
   PM_INOUT=0,  /* the parameter can be read or written by the call */
   PM_IN,       /* the parameter can only be read */
   PM_OUT,      /* the parameter must be written, w/o upward exposed use */
   PM_NOUSE     /* the parameter is not used (neither read nor written) */
} PARAM_MODE;
 

typedef unsigned int BOOLEAN;
#define IR_FALSE 0
#define IR_TRUE 1

typedef enum {VAR_LEAF=1, ADDR_CONST_LEAF=2, CONST_LEAF=3} LEAF_CLASS;
typedef enum {ISBLOCK=1, ISLEAF, ISTRIPLE, ISLOOPINFO, ISPPGCELL,
		ISTYPE} TAG;
typedef enum {EXT_FUNC, INTR_FUNC, SUPPORT_FUNC} FUNC_DESCR;
typedef enum {UNOPTIMIZED, OPTIMIZED, MPENABLED} FILE_STATUS;
typedef enum {C, FORTRAN, PASCAL, MODULA2, CDOUBLEPLUS, FORTRAN90, JAVA} LANG;

typedef enum {REG_SEG, STG_SEG} SEGCONTENT;
typedef enum {USER_SEG, BUILTIN_SEG} SEGBUILTIN;
typedef enum {LCLSTG_SEG, EXTSTG_SEG} SEGSTGTYPE;

typedef enum {ARG_SEG, BSS_SEG, DATA_SEG, AUTO_SEG, TEMP_SEG,
	      DREG_SEG, FREG_SEG, HEAP_SEG} SEGCLASS;

typedef enum {ALIAS_ANY, ALIAS_BASIC, ALIAS_WEAK, ALIAS_LAYOUT,
              ALIAS_STRICT, ALIAS_C_STD, ALIAS_STRONG} ALIAS_LEVEL;

/*
 * C++ team suggests adding some aliases to inline_field as follows
 */
typedef enum inline_field {
    NO_RESTRICTION=0,
    LOW_PRIORITY_INLINE_CALL=0,    /* alias to NO_RESTRICTION */
    INLINE_CALL=1,
    MUST_INLINE_CALL=1,            /* alias to INLINE_CALL */
    DO_NOT_INLINE_CALL=2,
    HIGH_PRIORITY_INLINE_CALL=3
} INLINE_FIELD;

typedef enum pfm_location {
    PFM_UNKNOWN = 0,
    PFM_L3_MEMORY = 1,
    PFM_L2_MEMORY = 2,
    PFM_MEMORY = 3,
    PFM_L3_CACHE = 4,
    PFM_L2_CACHE = 5,
    PFM_L1_CACHE = 6,
    PFM_L0_CACHE = 7
} PFM_LOCATION;

typedef enum ir_ld_scope {
	IR_GLOBAL_LD_SCOPE = 0,
	IR_SYMBOLIC_LD_SCOPE,
	IR_HIDDEN_LD_SCOPE
} IR_LD_SCOPE;

typedef unsigned int TWORD;

typedef enum {
    SEGEXT_GLOBAL = 0,
    SEGEXT_FILE_LOCAL,
    SEGEXT_FUNC_LOCAL
} seg_access_t;

struct	segdescr_st	{
	__extension__ SEGCLASS class:	8;		/* segment class */
	__extension__ SEGCONTENT content: 8;		/* registers or storage */
	__extension__ SEGBUILTIN builtin: 8;		/* built in or user defined */
	__extension__ SEGSTGTYPE external: 4;		/* external or local storage */
        BOOLEAN    ext_nosave : 1;      /* if an external segment has
                                         * "nosave" attribute, i.e., the
                                         * global can be privatized in
                                         * any accessed procedures. */
        __extension__ seg_access_t ext_access : 3;    /* the access scope of an
                                                       * external segment. */
};

typedef struct segment {
	char *name;
	struct segdescr_st descr;
	short base;
	unsigned int align:8;
	BOOLEAN visited:1;
	BOOLEAN noalias:1;
	BOOLEAN readonly:1;
        BOOLEAN need_far_access:1; /* Data resides in .ldata,.lrodata or .lbss */
	int segno;
	IR_OFFSZ offset;
	IR_OFFSZ len;
	struct segment *next_seg;
	struct list *leaves;
} SEGMENT;

/* segment numbers for builtin segments : these segments are always present
** but may be empty
*/
# define ARG_SEGNO 0
# define BSS_SMALL_SEGNO 1
# define BSS_LARGE_SEGNO 2
# define DATA_SEGNO 3
# define AUTO_SEGNO 4
# define HEAP_SEGNO 5
# define DREG_SEGNO 6
# define FREG_SEGNO 7

# define N_BUILTIN_SEG (FREG_SEGNO + 1)

typedef struct address {
	IR_OFFSZ offset;
	IR_OFFSZ length;
	struct segment *seg;
	int labelno;
	short alignment;
} ir_ADDRESS;

#define NOBASEREG -1
#define EXIT_LABELNO -1

union	const_u {
	char *cp;	/* Character constants */
	CONSZ i;
	char *fp[2];	/* Strings for float real and complex consts */
	char *bytep[2];	/* bytes for binary constants (init only) */
};

typedef struct list {
	struct list *next;
	union list_u *datap;	/* union list_u defined by application */
} LIST;

/*
 * IR type structure.   For most types, the size and alignment fields
 * indicate the type's size and its alignment requirements, both quantities
 * in units of bytes.  (alignment of a structured variable depends on the
 * types of its members.)
 */
typedef struct irtype {
	TWORD	tword;		/* PCC type encoding */
	unsigned align;		/* type alignment */
	IR_OFFSZ size;		/* type size */
} TYPE;

/*
 *
 */
typedef struct ir_type_node {
        __extension__ TAG     tag:4;          /* =ISTYPE */
        int	tid:8;          /* PCC or IR types */
        __extension__ ALIAS_LEVEL     alias_level:4;
        int     align:8;        /* 1<<align */
        int is_base_class:1;
        int is_wrapper_struct:1;
        int is_final:1;
        int is_vptr:1;
        __extension__ IR_LD_SCOPE ld_scope:2;
        int     unused:2;
        int     typeno;
        const char    *label;         /* field and enum name */
        struct  ir_type_node *parent;
	struct  DbgSymID *dbg_sym_id;	/* for struct member references */
        union {
            struct {
                IR_OFFSZ size;
                IR_OFFSZ offset;
                char    *tag_name;
                LIST    *children;
            } s;        /* struct, union, enum */
            struct {
                IR_OFFSZ size;
                IR_OFFSZ offset;
                struct ir_type_node *element_type;
                LIST    *dimensions;
            } a;        /* array */
            struct {
                char    *func_name;
                struct ir_type_node *return_type;
                LIST    *params;
            } f;        /* function */
            struct {
                IR_OFFSZ size;
                IR_OFFSZ offset;
            } b;        /* base type */
	    struct {
                IR_OFFSZ size;
                IR_OFFSZ offset;
		struct ir_type_node *bf_type;
                int      bit_width:8;
                int      bit_offset:8;
	    } bf;	/* bit field */
            struct {
                IR_OFFSZ size;
                IR_OFFSZ offset;
                struct ir_type_node *designator;
                int vptr_skew;
            } p;        /* pointer type */
            struct {
                CONSZ lower;
                CONSZ upper;
            } d;        /* array dimension */
            struct {
                CONSZ enumv;
            } e;        /* enum member */
        } t;
        struct ir_type_node *next;
} IR_TYPE_NODE;

typedef	struct s_block S_BLOCK;

typedef struct block {
	__extension__ TAG tag:4;
	BOOLEAN is_ext_entry : 1; /* is the block an external entry point */
	BOOLEAN entry_is_global: 1;	/* if so, is the entry point global */
        BOOLEAN unrolled: 1;      /* true iff block was unrolled */
	BOOLEAN modified: 1;	  /* true if block was modified */
	BOOLEAN is_rarely_executed:1; /* if true outline this block */
        int type:8;             /* Type associated with entry point */
        int unused:15;
	int blockno;
	struct triple *last_triple;
	char *entryname;
	int labelno;
	struct block *next;		/* allocation defined order */
	BOOLEAN visited;
	struct list *pred, *succ;
	int fregmask;			/* FPU registers live on block exit */
	int  profile_id;		/* id for collecting profiling data */
	float counter;		        /* number of times a block is executed */
	float conf_level;         	/* confidence that we have in the counter value */
	struct list *edge_succ; 	/* pointer to a list of struct succ_edge */
} ir_BLOCK;

typedef struct prof_edge {
        ir_BLOCK *succ_bp;
        float  edge_counter;
        float  edge_conf_level;		/* confidence level */
} PROF_EDGE;

typedef union leaf_value {
	struct address addr;
	struct constant {
		BOOLEAN isbinary;
		union const_u c;
	} cnst;
} LEAF_VALUE;


typedef struct leaf {
	__extension__ TAG tag:4;
	__extension__ LEAF_CLASS class:4;
	__extension__ FUNC_DESCR func_descr:4;	   /* attributes of known functions */
	BOOLEAN unknown_control_flow:1;	   /* if called, flow graph is incorrect */
	BOOLEAN makes_regs_inconsistent:1; /* if called, ... */
        BOOLEAN uplevel_addressed:1;	   /* for Pascal & Modula-2 */
        BOOLEAN is_volatile:1;	           /* = IR_TRUE if volatile memory_reference */
        BOOLEAN is_unshared:1;
	BOOLEAN is_const:1;
	BOOLEAN is_va_alist:1;	/* leaf represents beginning of
				 * vararg/stdarg part of formal
				 * parameter list.  Called routine
				 * must build argument array on stack
				 * starting at this leaf's address.
				 */
	    /*
	     * must_store_arg == TRUE if an incoming argument must be
	     * stored in its ARG_SEG location as part of the routine prologue.
	     */
	    BOOLEAN must_store_arg:1;
	    BOOLEAN pure_func:1;	   /* function  don't have side effect */
	    BOOLEAN in_taskcommon_block:1;
            BOOLEAN is_indirect:1;
	    int unused:5;
	/* For compatibility put new BOOLEAN flags here & decrement unused */
	__extension__ IR_LD_SCOPE ld_scope:2;
	BOOLEAN is_restrict:1;
	BOOLEAN throw_nothing:1; /* the function call will not throw exceptions */
	int  leafno;
	TYPE	type;
	LEAF_VALUE val;
	BOOLEAN	visited; /* used as a flag and/or pointer to auxiliary information*/
	const char *pass1_id;
	IR_TYPE_NODE	*typep;
	struct leaf *next_leaf;
	struct list *overlap;
	struct list *neighbors;
	int  pointerno;
	int  elvarno;
	struct leaf *addressed_leaf;	/* object addressed by ADDR_CONST */
	BOOLEAN no_reg;	/* indicate this leaf can not be in the register */
	struct DbgSymID *dbg_sym_id;	/* debug symbol handle */
} LEAF;

typedef enum ir_param_info_t {	/* formal param type info at call sites */
	IrParamDefault=0,	/* none: same as before V9 abi */
	IrParamIsDeclared,	/* param type info available at call site */
	IrParamIsEllipsis,	/* no param type info; stdarg.h "..." */ 
	IrParamNotDeclared	/* no param decl in scope at call site */
} IR_PARAM_INFO;

enum cpp_access_info {	
	cpp_default_access = 0,
	cpp_rodata_ref = 1,
	cpp_vtptr_ref = 2
};

typedef struct triple {
	__extension__ TAG tag:4;
	BOOLEAN is_stcall:1;	/* FCALL triple returns structured result */
	BOOLEAN is_tailcall:1;	/* = IR_TRUE if call is tail-recursive */
	BOOLEAN chk_ovflo:1;	/* = IR_TRUE if overflow checking enabled */
        BOOLEAN is_volatile:1;  /* = IR_TRUE if volatile memory reference */
	__extension__ IR_OP	op:8;
	unsigned  bfoffset:6;	/* for BIT_OPs: bit field offset [0..63] */
	unsigned  bfwidth:7;	/* for BIT_OPs: bit field width [1..64]  */
        BOOLEAN movable:1;      /* true iff triple is moveable during store 
                                   buffering optimization */
	BOOLEAN no_dalign:1;    /* true => override -dalign flag */
	BOOLEAN rodata_ref:1;   /* true => the ifetch is a RO data ref
				 * will be used for vtables etc. */
	int tripleno;
	TYPE	type;
        __extension__ INLINE_FIELD in_line:3; /* user control over inlining */
	BOOLEAN  pf_load:1;     /* true => this ref should be prefetched */
	BOOLEAN  pf_cov:1;	/* true => other prefetch "covers" this ref */
	__extension__ PFM_LOCATION pf_params:3;   /* other info for the prefetch */
	unsigned param_float_map:8; /* ((map>>i)&1)==1 iff word(i) is float */
	__extension__ IR_PARAM_INFO param_info:2; /* formal param type info at call site */
	BOOLEAN  is_rarely_executed:1;  /* indicates that this triple is rarely
					 * executed
					 */
	BOOLEAN never_returns:1; /* the corresponding call never returns */
	BOOLEAN allocates_new_memory:1;
	BOOLEAN never_reads_globals:1;
	BOOLEAN never_writes_globals:1;
	BOOLEAN may_cause_exception:1;
	BOOLEAN dont_tailcall:1;
	BOOLEAN reserved_tailcall:1;
	BOOLEAN speculative:1; /* speculative op if true */
	__extension__ PARAM_MODE param_mode:2; 	/* parameter mode */
	unsigned   access_info:2;	/* language dependent memory access info */
	BOOLEAN no_overflow:1; /* cannot overflow, or else result is undefined. */
        BOOLEAN pf_rw:1;        /* true => Write-After-Read pref candidates */
        BOOLEAN non_temporal:1; /* this bit is for annotating an istore triple,
                                 * indicating that the istore will write to to
                                 * memory directly, bypass all caches.
                                 */
        BOOLEAN tm_callable:1;  /* tm_callable/tm_only call */
        BOOLEAN tm_atomic_block:1;      /* __tm_atomic_block */
        unsigned unused:28;     /* sizeof(TRIPLE) == (k * sizeof(double)) */
        unsigned prof_id;	/* for profile feedback */

	struct triple *tprev, *tnext;
	union node_u *left,*right;
	IR_TYPE_NODE	*typep;
	BOOLEAN	visited;	/* flag/ptr used repeatedly by various phases */
	struct list *can_access;/* list of leaves used/defined by side effects*/
	struct ln_sl *line_num; /* Line number information */
          IR_TYPE_NODE * ldst_ir_type; /* SGCC only fields */
	  TYPE ldst_type;              /* SGCC only fields */
} TRIPLE;


/*
 * OPERAND contains those fields which MUST be
 * identical in all variants of the IR_NODE union.
 */
typedef struct operand {
	__extension__ TAG tag:4; 
	int /*overloaded*/: 28;
	int number;  /* align with blockno, tripleno, leafno */
	TYPE type;
} OPERAND;

typedef union node_u {
	struct leaf leaf;
	struct triple triple;
	struct block block;
	struct operand operand;
} IR_NODE;

typedef enum {
   FUNC_KIND_ORIG=0, /* func passed into backend */
   FUNC_KIND_MFUNC,  /* mfunc, created by iropt */
   FUNC_KIND_CLONE   /* clone, created by iropt */
} func_kind_t;

#define LCAST(list,structname) ((structname*)((list)->datap))
#define LNULL   ((LIST*) NULL)
#define TNULL   ((TRIPLE*) NULL)
#define LFOR(list,head)\
	for((list)=(head); (list);\
		((list)=((list)->next==(head) ? LNULL : (list)->next)))

#define TFOR(triple,head)\
	for((triple)=(head); (triple);\
		((triple)=((triple)->tnext==(head) ? TNULL : (triple)->tnext)))

/*
 * append an item to a circular list and update the
 * list 'tail' pointer.  The effect is that the circular
 * list stores elements in FIFO order.
 */
#define LAPPEND(after,item) {\
	register LIST *tmp1;\
	if((after) != LNULL) {\
		tmp1 = (item)->next;\
		(item)->next = (after)->next; \
		(after)->next = tmp1;\
	} (after) = (item);\
}

#define TAPPEND(after,item) {\
	register TRIPLE *tmp1, *tmp2;\
	if((after) != TNULL) {\
		tmp1 = (item)->tnext;\
		tmp2 = (after)->tnext;\
		(after)->tnext = tmp1;\
		(item)->tnext = tmp2; \
		tmp1->tprev = (after);\
		tmp2->tprev  = (item);\
	} (after) = (item);\
}

extern ir_BLOCK *first_block;
extern LEAF *first_leaf;

#ifndef __loopinfo__
#define __loopinfo__

/* A "loopinfo" holds information about one loop
 * in the source.
 */
/* RATIONALE:  This structure might duplicate information found elsewhere
 *   but it is hard for the backend to collect this information.
 *   Correct code (but not necessarily optimal code) can be generated without
 *   reference to these structures.
 *   Sometimes the information is not present in IR triples:  for example, only if the
 *   source DO statement (FORTRAN 77) generates executable code will a STMT
 *   triple be generated with the filename and line number of the DO.
 *   Otherwise the first STMT triple after the DO corresponds to the first
 *   executable statement in the loop body.
 */

typedef enum {
        LT_DO,           /* fortran style do loop with an index variable */
        LT_FOR,          /* for loop */
        LT_WHILE,        /* while loop */
        LT_DOALL,        /* doall (pragma) loop */
        LT_DOSERIAL,     /* doserial (pragma) loop */
        LT_DOSERIALNEST, /* doserialstar (pragma) loop nest */
        LT_CRAY_DOALL,   /* Cray style doall (pragma) loop */
        LT_UNKNOWN       /* none of the above */
} loop_t;

typedef enum {
        IR_ST_NONE,      /* no scheduling type specified */
        ST_STATIC,       /* static scheduling type */
        ST_SELF,         /* self scheduling type */
        ST_GSS,          /* guided self scheduling type */
        ST_FACTORING,    /* factoring scheduling type */
        ST_AFFINITY,     /* affinity scheduling type */
        ST_OMP_STATIC,   /* OpenMP STATIC */
        ST_OMP_GUIDED,   /* OpenMP GUIDED */ 
        ST_OMP_DYNAMIC,  /* OpenMP DYNAMIC */
        ST_OMP_RUNTIME,  /* OpenMP RUNTIME */
        ST_NUMA,         /* NUMA scheduling */
        ST_OMP_AUTO      /* OpenMP AUTO */
} sched_t;

typedef enum {
	NUMA_DATA_AFFINITY
} numa_sched_t;

typedef enum {
	NUMA_BLOCK,
	NUMA_CYCLIC
} numa_data_dist_t;

typedef enum {
        ASSUME_NONE= 0,     /* assumed_trip_count not set */
        ASSUME_LOW,         /* assumed_trip_count with low probability */
        ASSUME_HIGH,        /* assumed_trip_count with high probability */
        ASSUME_SURE         /* assumed trip_count with probability 1.0 */
} ASSUME_KIND;

typedef struct loopinfo {
        __extension__ TAG      tag:4;
        BOOLEAN  storeback_all :1; /* if true, storeback all local variables in
                                      this loop */
        BOOLEAN  pipe_loop:1;      /* pipelinable loop? */
        BOOLEAN  no_mem_dep:1;     /* any mem deps? */
	__extension__ ASSUME_KIND assume_kind:2; /* probability of assumed_trip_count */

        int      unused:23;

        int loopno;
        struct leaf *looplabel;   /* CONST_LEAF representing the labeled
                                   * location in the code of the top of the
                                   * loop body.  At the bottom of the
                                   * loop an IR_CBRANCH is expected to reference
                                   * the same label.
                                   */
        struct leaf *loopfilename;/* CONST_LEAF representing the source filename
                                   * for the first line of the loop.
                                   */
        struct leaf *looplineno;  /* CONST_LEAF representing the source line
                                   * number of the first line of the loop, e.g. 
                                   * in FORTRAN 77
                                   * the line on which "DO" appears.
                                   */
        union node_u *loopindex;   /* IR_NODE representing the loop index
                                   * variable, if language semantics prohibit
                                   * changing the value of this variable.
                                   * The NULL value is permitted.
                                   */
        loop_t   loop_type;        /* loop type for this loop */
	int      unroll_factor;    /* unroll factor for this loop */
        LEAF    *ncpus;            /* number of cpus for this loop */
        sched_t  schedtype;        /* scheduling strategy for this loop */
        LEAF    *chunksize;        /* size of each scheduled chunk for this
                                      loop */
        LEAF    *numchunks;        /* number of scheduled chunks for this
                                      loop */
        LIST    *local;            /* list of local variables in this loop */
        LIST    *shared;           /* list of shared variables in this loop */
        LIST    *byvalue;          /* list of read-only shared variables in this
                                      loop */
        LIST    *storeback;        /* list of variables requiring storeback in
                                      this loop */
        LIST    *reduction;        /* list of reduction variables in this
                                      loop */
	int      dep_dist;         /* dependence distance */

        LIST    *misc_list;

	CONSZ assumed_trip_count;  /* trip count from ASSUME/ASSERT pragma */

	LIST	*pragmas;	   /* pragmas associated with this loop */

} LOOPINFO;

/**************************************************/
/*             BEGIN OF PRAGMAINFO                */
/**************************************************/

/*
 * pragmainfo is used to pass OpenMP pragma information into iropt.
 * See /set/misc/docs/interfaces/IR/ir_openmp.txt for details.
 *
 */
typedef enum {
    OMP_E_PARALLEL,        /* OpenMP parallel region */
    OMP_E_PARDO,           /* OpenMP parallel DO/FOR */
    OMP_E_PARSECTIONS,     /* OpenMP parallel sections */
    OMP_E_DO,              /* OpenMP DO/FOR */
    OMP_E_SECTIONS,        /* OpenMP sections */
    OMP_E_SINGLE,          /* OpenMP single */
    OMP_E_CRITICAL,        /* OpenMP critical section */
    OMP_E_FLUSH,           /* OpenMP flush, a single pragma. */
    OMP_E_TASK             /* OpenMP task */
} pragmaEntry_t;

/*
 * Data scope
 */
typedef enum {
    SCOPE_NONE,
    SCOPE_PRIVATE,
    SCOPE_SHARED,
    SCOPE_REDUCTION
} dscope_t;

/*
 * Pragma/directive information
 */
typedef struct pragmainfo {
     BOOLEAN            is_end_nowait:1;
     BOOLEAN            has_order:1;
     BOOLEAN            is_task_untied:1;
     int                unused:29;

     /* Entry type and location information */
     pragmaEntry_t      type;
     TRIPLE             *begin_triple, *end_triple;

     /* For reporting, error checking, etc. */
     LEAF               *filename;
     int                begin_lineno, end_lineno;

     /* Number of threads in the team executing parallel construct. */ 
     LEAF               *num_threads; 

     union {
        struct {
           /*
            *       Data Scope
            * Each list is a list of VAR_LEAF. Note the reduction
            * list has a special structure and is defined later.
            */
           dscope_t           default_scope;
           LIST               *omp_private;
           LIST               *shared;
           LIST               *reduction;
           LIST               *firstprivate;
           LIST               *lastprivate;
           LIST               *copyin;
           LIST               *copyprivate;

           LIST               *autoscope;
           LIST               *auto_private;
           LIST               *auto_shared;
           LIST               *auto_reduction;
           LIST               *auto_firstprivate;
           LIST               *auto_lastprivate;

           LEAF               *if_clause;    /* a temp leaf (VAR or CONST) */

           /* Scheduling information -- for DO/FOR only */
           sched_t            schedtype;
           LEAF               *chunksize;         /* a temp leaf */

           numa_sched_t        numa_schedtype;
           numa_data_dist_t    numa_data_dist;
           LEAF               *numa_extent;       /* a temp leaf */
           LEAF               *numa_r1;           /* a temp leaf */
           LEAF               *numa_r2;           /* a temp leaf */
           LEAF               *numa_block_size;   /* a temp leaf */

           /* Collapse parameter -- for DO/FOR only.
            * Specifies how many perfectly nested loops should be 
            * collapsed before doing the parallelization.
            */

           int                collapse;

        } s;

        LEAF               *cs_lock_leaf;    /* global VAR_LEAF for CS */
                                             /* Null for unnamed CS */

        LIST               *flush;           /* FLUSH pragma, a list of 
                                                VAR_LEAF */
     } u;

     LIST               *misc_list;

} PRAGMAINFO;

/* 
 * Reduction list in the PRAGMAINFO table
 *
 *    LIST *reduction has a special structure because reduction clause
 *    is of form REDUCTION ( {operator|intrinsic} : list).
 *
 *    First, operators and intrinsics are represented by constant leaves
 *    whose values are of type int and is defined by reduct_op given below.
 *
 *    Second, because 'list' is a list of VAR_LEAF, a mixed list of
 *    CONST_LEAF and VAR_LEAF can represent multiple reduction clauses:
 *
 *                    -----   -----   -----   -----   -----   -----
 *      reduction --->| C |-->| V |-->| V |-->| C |-->| V |-->| V |--> NIL
 *                    -----   -----   -----   -----   -----   -----
 *                      ^                       ^
 *                      |                       |
 *                    First reduction clause    Second reduction clause
 *
 *                    Note: C: CONST_LEAF of type int, representing an
 *                             reduction operator. Its val.cnst.c.i is
 *                             defined by type reduct_op.
 *                          V: VAR_LEAF representing a variable.
 *
 *
 *    For example, REDUCTION (+: a, b) REDUCTION (.OR.: AM) can be
 *        represented as follows:
 *
 *                      ------   -----   -----   ---------   ------
 *        reduction --->| '+'|-->| a |-->| b |-->| '.OR' |-->| AM |
 *                      ------   -----   -----   ---------   ------
 */
typedef enum {
    REDU_PLUS,
    REDU_MINUS,
    REDU_TIMES,
    REDU_AND,
    REDU_OR,
    REDU_EQV,
    REDU_NEQV,
    /* intrinsic functions */
    REDU_MAX,
    REDU_MIN,
    REDU_IAND,      /* bit AND */
    REDU_IOR,       /* bit OR  */
    REDU_IEOR       /* bit exclusive OR */
} reduct_op;

/*
 *  Value of the left operand of IR_PRAGMA in IR_OP.
 *
 *    left.val.cnst.c.i is of type pragma_t as defined below:
 */
typedef enum {
    /* Control Constructs */
    OMP_PARALLEL_BEGIN,     OMP_PARALLEL_END,
    OMP_PARDO_BEGIN,        OMP_PARDO_END,
    OMP_PARSECTIONS_BEGIN,  OMP_PARSECTIONS_END,

    /* Worksharing constructs */
    OMP_DO_BEGIN,           OMP_DO_END,
    OMP_SECTIONS_BEGIN,     OMP_SECTIONS_END,
    OMP_IR_SECTION,
    OMP_SINGLE_BEGIN,       OMP_SINGLE_END,
    
    /* Synchronization Constructs */ 
    OMP_MASTER_BEGIN,       OMP_MASTER_END,
    OMP_CRITICAL_BEGIN,     OMP_CRITICAL_END,
    OMP_ORDER_BEGIN,        OMP_ORDER_END,
    OMP_ATOMIC_BEGIN,       OMP_ATOMIC_END,
    OMP_BARRIER,
    OMP_FLUSH,
    OMP_IR_SECTION_END,

    /* Worksharing OpenMP 3.0 */
    OMP_TASK_BEGIN,         OMP_TASK_END,
    OMP_IR_TASKWAIT,

    PRAGMA_ALIAS_TYPE=100,
    PRAGMA_ALIAS_POINTER,
    PRAGMA_NOALIAS_TYPE,
    PRAGMA_NOALIAS_POINTER,
    PRAGMA_MAY_POINT_TO,
    PRAGMA_MAY_NOT_POINT_TO,
    PRAGMA_ALIAS_LEVEL,
 
    /* C99 STDC pragmas */
    STDC_CX_LIMITED_RANGE=200,
    STDC_FENV_ACCESS,
    STDC_FP_CONTRACT
} pragma_t;

/*
 * For more information on pragmainfo table, 
 *     see /set/misc/docs/interfaces/IR/ir_openmp.txt.
 */
/**************************************************/
/*               END OF PRAGMAINFO                */
/**************************************************/

#endif /*__loopinfo__*/

#endif /*__gcc_ir_common__*/


