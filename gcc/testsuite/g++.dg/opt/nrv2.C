// Test for the named return value optimization, this time with inlining.
// { dg-do run }
// { dg-options "" }

int c;
int d;

struct A
{
  A() { ++c; }
  A(const A&) { ++c; };
  ~A() { ++d; }
};

inline A f ()
{
  A a;
  return a;
}

int main ()
{
  {
    A a = f ();
  }

  return !(c == 1 && c == d);
}
