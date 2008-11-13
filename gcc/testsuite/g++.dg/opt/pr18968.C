// { dg-do compile }
// { dg-options "" }
struct X
{
  int i;
};
struct Y : virtual X {};
struct Z : Y {};
struct A
{
  Z* p;
  A();
};
A::A() : p(0)
{
  ((X*)(Y*)p)->i++;
}

