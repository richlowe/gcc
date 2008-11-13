/* { dg-do compile } */
/* { dg-options "" } */

int f(int *a)
{
  int &b = *a;
  b = 0;
  return *a;
}
/* There should be only one dereferencing of a. */
