/* cr 6582875 */
/* { dg-do compile } */

typedef unsigned int size_t;

typedef enum { FALSE, TRUE } boolean;
static boolean BraceFormat = FALSE;
typedef struct sOptionValues {
    boolean if0;
    boolean kindLong;
} optionValues;

extern const optionValues Option;

typedef struct sConditionalInfo {
    boolean ignoreAllBranches;
    boolean singleBranch;
    boolean branchChosen;
    boolean ignoring;
} conditionalInfo;

typedef struct sVString {
    size_t length;
    size_t size;
    char * buffer;
} vString;
enum eCppLimits {
    MaxCppNestingLevel = 20,
    MaxDirectiveName = 10
};

typedef struct sCppState {
    int ungetch, ungetch2;
    boolean resolveRequired;
    struct sDirective {
 enum eState {
     DRCTV_NONE,
     DRCTV_DEFINE,
     DRCTV_HASH,
     DRCTV_IF,
     DRCTV_LINE,
     DRCTV_UNDEF
 } state;
 boolean accept;
 vString * name;
 unsigned int nestLevel;
 conditionalInfo ifdef[MaxCppNestingLevel];
    } directive;
} cppState;

static cppState Cpp = {
    '\0', '\0',
    FALSE,
    {
 DRCTV_NONE,
 FALSE,
 0,
 0,
 { {FALSE,FALSE,FALSE,FALSE} }
    }
};

static conditionalInfo *currentConditional (void);
static boolean isIgnore (void);
static boolean pushConditional (const boolean firstBranchChosen);
static boolean directiveIf (const int c);
static boolean handleDirective (const int c);

static conditionalInfo *currentConditional()
{
    return &Cpp.directive.ifdef[Cpp.directive.nestLevel];
}

static boolean isIgnore()
{
    return Cpp.directive.ifdef[Cpp.directive.nestLevel].ignoring;
}

static boolean pushConditional( firstBranchChosen )
    const boolean firstBranchChosen;
{
    const boolean ignoreAllBranches = isIgnore();
    boolean ignoreBranch = FALSE;

    if (Cpp.directive.nestLevel < (unsigned int)MaxCppNestingLevel - 1)
    {
 conditionalInfo *ifdef;

 ++Cpp.directive.nestLevel;
 ifdef = currentConditional();
 ifdef->ignoreAllBranches= ignoreAllBranches;
 ifdef->singleBranch = Cpp.resolveRequired;
 ifdef->branchChosen = firstBranchChosen;
 ifdef->ignoring = (boolean)(ignoreAllBranches || (
        ! firstBranchChosen && ! BraceFormat &&
        (ifdef->singleBranch || !Option.if0)));
 ignoreBranch = ifdef->ignoring;
    }
    return ignoreBranch;
}

static boolean directiveIf( c )
    const int c;
{
    const boolean ignore = pushConditional((boolean)(c != '0'));

    Cpp.directive.state = DRCTV_NONE;

    return ignore;
}

static boolean handleDirective( c)
    const int c;
{
    boolean ignore = FALSE;

    switch (Cpp.directive.state)
    {
 case DRCTV_NONE: ignore = isIgnore(); break;
 case DRCTV_IF: ignore = directiveIf(c); break;
    }
    return ignore;
}

int cppGetc(int c)
{
  return handleDirective(c);
}
