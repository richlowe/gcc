// PR c++/13392
// { dg-do compile }
// { dg-options "" }

extern "C" void abort (void);
struct X { ~X () throw() {} };
bool foo (X s = X ()) { return false; }
void bar ()
{
  __builtin_expect (foo () && true, 1) ? 0 : (abort (), 0);
}
