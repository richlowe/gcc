/* { dg-do compile } */
/* { dg-options "" } */

struct Foo {
  Foo() : s(1) {}
  int s;
};
void foo(Foo&);
void bar(void)
{
  Foo x[4];
  foo(x[0]);
}

