/* { dg-do run } */

extern void abort();

int foo (long long *a, long long *b) __attribute__ ((noinline));
int foo (long long *a, long long *b)
{
  return a != b;
}

int bar (double *a, double *b) __attribute__ ((noinline));
int bar (double *a, double *b)
{
  return a != b;
}

int call (long long ll, double d)
{
  if (foo (&ll, &ll)) abort();
  if (bar (&d, &d)) abort();
  return 0;
}

int main()
{
  volatile long long ll;
  volatile double d;
  return call (ll, d);
}
    
