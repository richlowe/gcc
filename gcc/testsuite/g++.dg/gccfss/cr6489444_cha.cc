/* { dg-do run } */
/* { dg-options "-W2,-Awhole" } */

extern "C" void abort();

namespace foo {
    struct A {
        virtual int foo();
    };
    int A::foo() {return 10;}
};
namespace bar
{
    struct A {
        virtual int foo();
    };
    int A::foo() {return 20;}
};
int main () 
{
  bar::A *a = new bar::A();
  if (a->foo() != 20)
    abort();
  delete a;
  return 0;
}

