/* { dg-do compile } */
/* { dg-options "" } */

/* We were not getting the offset of a in B and a in C::B correct, 
   causing an abort.  */
struct A { A(); };

struct B : A
{
    A a;
};

struct C : B { };

C c;
