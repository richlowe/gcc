// PR c++/13314
// { dg-options "" }

struct A { template <int> struct B; };
struct A::B {}; // { dg-error "" }
A::B<0> b; // { dg-error "" }
