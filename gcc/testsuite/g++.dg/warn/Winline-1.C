// { dg-options "-Winline" }

static inline int foo(int x);

int main()
{
  return(foo(17));
}

inline int foo(int x) {  return(x);  }
