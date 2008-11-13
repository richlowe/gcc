/* { dg-do compile } */
/* { dg-options "" } */

int *f(int *b)
{
  int * a = new int[104];
  *a = 1;
  if (a == 0)
    return b;
  return a;
}

