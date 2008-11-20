// Testcase by Volker Reichelt <reichelt@gcc.gnu.org>

// { dg-do compile }
// { dg-options "-O3 -ffast-math" }

struct A
{
  ~A();
};

double& foo();

inline void bar (double d) { foo() /= d; }

void baz()
{
  A a;
  bar(2);
}

