// PR c++/26534
// { dg-do run } 
// { dg-options "-w" }
 
struct X
{
  unsigned a:4;
};

unsigned i;

int main()
{
  struct X x = { 63u };
  i = x.a;
  if (i != 15)
    return 1;
}
