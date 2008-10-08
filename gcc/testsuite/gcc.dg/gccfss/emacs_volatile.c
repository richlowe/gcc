/* cr 6479871 */ 
/* { dg-do compile } */
void foo()
{
  volatile int a;
  if (a) {}
}
