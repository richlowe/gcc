/* cr 6476417 */
/* { dg-do run } */

extern void abort ();

float f = 1;
float *fp = &f;

int ac = 0;

void check () __attribute__ ((noinline));
void check ()
{
  if (ac == 0 && *fp != 1)
      abort ();
  else if (ac == 1 && *fp != 3)
      abort ();
  else if (ac == 2 && *fp != 2)
      abort ();
  ac ++;
}

void foo () __attribute__ ((noinline));

void foo (fff2) float fff2;
{
  if (ac == 0)
    {
      fp = &fff2;
      check ();
      fff2 = 3;
      fp = &fff2;
      check ();
    }
  else
    {
      fp = &fff2;
      check ();
    }
}

int main ()
{
  foo (f);
  foo (f + 1);
  return 0;
}

