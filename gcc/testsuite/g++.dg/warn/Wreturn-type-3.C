// PR middle-end/19583
// { dg-options "-Wreturn-type" }

struct E{};

inline int bar() throw(E)
{
  return 0;
}

void foo ()
{
  bar();
}
