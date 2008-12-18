// { dg-options "-std=gnu++0x -O1 -Wd,-w" }
// { dg-do compile }
template<typename... Args>
int f(const Args&...);

template<typename T> void g(T) { }

void h()
{
  g(&f<int, float>);
}

// { dg-final { scan-assembler "_Z1gIPFiRKiRKfEEvT_"} }
