// PR c++/33709
// { dg-do compile }
// { dg-options "-O3" }

class S {
  virtual void foo ();
};
struct T {
  S *s;
  void bar (unsigned x) { s = (new S[1]) - x; }
};
