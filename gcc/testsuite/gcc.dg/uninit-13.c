/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

typedef _Complex float C;
C foo()
{
  C f;
  __imag__ f = 0;	/* { g-warning "is used" "unconditional" } */ /* todo in iropt */
  return f;
}
