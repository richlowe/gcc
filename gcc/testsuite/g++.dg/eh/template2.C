// { dg-do compile }
// { dg-options "" }

template<class T> struct O {
  O(T *p) : q(p) { }
  T *q;
};
struct W {
  virtual ~W();
};
struct S : public W {
  S (int *);
};
W *bar(int);
S::S (int *x)
{
  for (int *p = x; *p; p++)
    O<W> i (bar (*p));
}
