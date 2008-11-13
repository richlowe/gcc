// Test that typeid sees through references even when optimizing.
// { dg-do run }
// { dg-options "" }

#include <typeinfo>

struct A
{
  virtual ~A() { }
};

class B : public A { };

int main ()
{
  B b;
  A &aref = b;

  return typeid (aref) != typeid (b);
}
