/* { dg-do compile } */
/* { dg-options "" } */

int &f(int *a)
{
  return *a;
}

/* There should be no cast as pointer and references are
   considered the same type. */
