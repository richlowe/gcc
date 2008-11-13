// { dg-do compile }
// { dg-options "" }

struct A
{
    A();
    virtual A foo() const;
};

void bar()
{
    const A& a=A();
    a.foo();
}
