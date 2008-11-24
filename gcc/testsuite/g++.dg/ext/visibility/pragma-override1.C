/* Test that #pragma GCC visibility does not override class member specific settings. */
/* { dg-do compile } */
/* { dg-require-visibility "internal" } */
 /* gcc2ir emits .hidden instead  { scan-assembler "\\.internal.*Foo.methodEv" } } */
/* { dg-final { scan-assembler "\\.hidden.*Foo.methodEv" } } */

#pragma GCC visibility push(hidden)
class __attribute__ ((visibility ("internal"))) Foo
{
  void method();
};
#pragma GCC visibility pop

void Foo::method() { }
