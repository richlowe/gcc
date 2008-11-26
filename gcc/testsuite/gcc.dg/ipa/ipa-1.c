/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */

#include <stdio.h>
int g (int b, int c)
{
  printf ("%d %d\n", b, c);
}
int f (int a)
{
  /* Second parameter of g gets different values.  */
  if (a > 0)
    g (a, 3);
  else
    g (a, 5); 	
}
int main ()
{
  f (7);
  return 0;	
}




