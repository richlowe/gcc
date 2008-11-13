/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O3" } */

double x;

template <typename T>
T *foo(void)
{
  return (T *)&x; /* { dg-bogus "strict-aliasing" } */
}

template double *foo<double>(void);

