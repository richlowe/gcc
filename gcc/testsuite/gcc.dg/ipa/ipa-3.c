/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fno-early-inlining"  } */
/* { dg-skip-if "PR 25442" { "*-*-*" } { "-fpic" "-fPIC" } { "" } } */


/* Double constants.  */

#include <stdio.h>
int g (double b, double c)
{
  return (int)(b+c);  
}
int f (double a)
{
  if (a > 0)
    g (a, 3.1);
  else
    g (a, 3.1); 	
}
int main ()
{
  f (7.44);
  return 0;	
}


