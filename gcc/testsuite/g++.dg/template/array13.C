// PR c++/20208
// { dg-do run }
// { dg-options "" }

extern "C" void abort();

template <typename T>
inline void *Foo (T arg) { return &arg[0]; }

int main () {
  int bry[2];
  if (Foo<int[2]>(bry) != bry)
    abort();
}
