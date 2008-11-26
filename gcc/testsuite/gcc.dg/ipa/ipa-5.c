/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */

/* Float & short constants.  */

#include <stdio.h>
int g (float b, short c)
{
  return c + (int)b;
}
int f (float a)
{
  /* a is modified.  */
  if (a++ > 0)
    g (a, 3);
}
int main ()
{
  f (7.6);
  return 0;	
}


