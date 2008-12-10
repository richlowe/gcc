/* Check for "noreturn" warning in main. */
/* { dg-do compile } */
/* { dg-options "-O2 -frtl-backend -Wmissing-noreturn -ffreestanding" } */
extern void exit (int) __attribute__ ((__noreturn__));

int
main (void)
{ /* { dg-warning "function might be possible candidate for attribute 'noreturn'" "warn for main" } */
  exit (0);
}
