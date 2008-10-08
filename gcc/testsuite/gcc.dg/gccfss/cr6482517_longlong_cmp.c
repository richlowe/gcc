/* cr 6482517 */
/* { dg-do run } */
long long Xx    = 0;
long long Yy    = 2;
long long Zz    = 5;
extern void abort();
int main()
{
  long long a;
  a = 3;
  if (Yy < a < Zz); else abort ();
  if (Zz > Yy || Zz > Yy); else abort ();
  if (Zz != Zz && Yy > Xx || Xx < a); else abort ();
  if (Xx < a || Yy > Xx && Zz != Zz); else abort ();
  return 0;
}

