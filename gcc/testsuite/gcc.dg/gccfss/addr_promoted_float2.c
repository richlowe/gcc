/* cr 6477774 */
/* { dg-do run } */

extern void abort();
void problemfunction();
void dummyfunction();

int main()
{
  float x;
  x = 5.0;
  problemfunction(x);
  return 0;
}

void problemfunction(f)
    float f;
{
  if (f != 5.0) abort();
  dummyfunction (&f);
}

void dummyfunction(pf)
    float *pf;
{
  if (*pf != 5.0) abort();
}

