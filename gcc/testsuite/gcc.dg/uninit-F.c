/* Test we do warn about initializing variable with self in the initialization. */
/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

int f()
{
  int i = i + 1; /* TODO: warn in iropt. was: dg-warning "i" "uninitialized variable warning" */
  return i;
}
