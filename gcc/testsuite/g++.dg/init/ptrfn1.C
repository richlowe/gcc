// PR c++/29016
// { dg-options "" }

class A;
class B
{
  typedef void (*C[5]) (A *);
  static  C D;
  static void E (A*) {}
};
B::C B::D={E};

