/* cr 6507787 (nightly build failed due to iropt's lcond_elim.c problem) */
/* { dg-do run } */

extern void abort(void) __attribute__((__noreturn__));

enum reg_class { NO_REGS, FPCC_REGS, I64_REGS, GENERAL_REGS, FP_REGS,
   EXTRA_FP_REGS, GENERAL_OR_FP_REGS, GENERAL_OR_EXTRA_FP_REGS,
   ALL_REGS, LIM_REG_CLASSES };

struct rtx_def;
typedef struct rtx_def *rtx;

struct reload
{
  rtx in;
  rtx out;
  unsigned int optional:1;
};

struct rtx_def {};

int n_reloads;
struct reload rld[(2 * 30 * (2 + 1))];

static void combine_reloads (void);

int earlyclobber_operand_p (rtx insn) {abort();return 0;}

static void
combine_reloads (void)
{
  int i;
  int output_reload = -1;

  for (i = 0; i < n_reloads; i++)
    if (rld[i].out != 0)
      {
        if (output_reload >= 0)
          return;
        output_reload = i;
      }

  if (output_reload < 0 || rld[output_reload].optional)
    return;

  if (rld[output_reload].in != 0)
    return;

  if (earlyclobber_operand_p (rld[output_reload].out))
    return;
}

int this_alternative[30];

int var, var2, var3, var4, var5, bad;

int
find_reloads ()
{
  if ((( var3
        ? ( var 
           ? NO_REGS : var2 
           ? GENERAL_REGS : ((enum reg_class) this_alternative[0])) 
        : ((enum reg_class) this_alternative[0])) == NO_REGS))
    bad = 1;

  combine_reloads ();
  return 0;
}

int
main()
{
  n_reloads = 0;
  rld[-1].optional = 0;
  rld[-1].in = 0;
  find_reloads ();
  return 0;
}

