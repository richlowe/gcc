/* { dg-do compile } */
/* { dg-options "-fstrict-overflow -O2 -Wstrict-overflow=4" } */

/* Source: Ian Lance Taylor.  */

int
foo (int j)
{
  int i;
  int sum = 0;

  for (i = 1; i < j; i += i)
    sum += i / 16; /* { g-warning "assuming signed overflow does not occur" "" } */ /* todo in iropt */
  return sum;
}
