/* { dg-do compile } */
/* { dg-options "-O2 -frtl-backend -fprofile-generate -fprofile-use" } */

unsigned test (unsigned a, unsigned b)
{
  return a / b;
} /* { dg-message "note: \[^\n\]*execution counts estimated" } */

/* { dg-final { cleanup-coverage-files } } */
