/* { dg-do compile } */
/* { dg-options "-O3 -frtl-backend -fdump-tree-forwprop1" } */

#include <new>

template <class T>
struct Vec
{
  Vec()
  {
    for (int i=0; i<3; ++i)
      new (&a[i]) T(0);
  }
  T a[3];
};

double foo (void)
{
  Vec<double> v;
  return v.a[2];
}

/* { dg-final { cleanup-tree-dump "forwprop1" } } */
