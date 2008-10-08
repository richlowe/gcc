/* { dg-do run } */
/* cr 6578077 */

extern "C" void abort();
struct C
{
  int i;
  
  C (int v) {i=v;}
  C () {i=0;}
  
  C& operator *=(C &a)
    {
      i *= a.i;
      return *this;
    }
  int operator != (int v)
    { return i != v;}
};
  
inline C operator*(C &a, C&b)
    {
      C r = a;
      r *= b;
      return r;
    }

int main(void)
{
  C a[2], roots;

  roots = 1;
  a[0] = 2;
  a[0] = a[0]*roots;
  a[0] = roots * a[0];
  if (a[0] != 2)
    abort();
  return 0;
}
