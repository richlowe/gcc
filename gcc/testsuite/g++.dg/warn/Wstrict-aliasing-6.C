/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing=2 -O3" } */

int foo ()
{
  char buf[8];
  return *((int *)buf); /* { dg-warning "strict-aliasing" } */
}

