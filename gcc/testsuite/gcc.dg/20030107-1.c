/* { dg-do compile } */
/* { dg-options "-fprofile-arcs" } */
/* { dg-excess-errors "assuming `-O1' option" } */

extern void bar(void) __attribute__((noreturn));
int foo (void) { bar(); }

/* { dg-final { cleanup-coverage-files } } */
