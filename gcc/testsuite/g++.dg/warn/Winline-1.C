// { dg-options "-O3 -Winline" }

static inline int foo(int x);

int main()
{
  return(foo(17));
}

inline int foo(int x) {  return(x);  }
