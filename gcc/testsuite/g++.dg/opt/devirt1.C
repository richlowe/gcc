// { dg-do compile }
// { dg-options "" }
// { g-final //TODO { scan-assembler "xyzzy" } }

struct S { S(); virtual void xyzzy(); };
inline void foo(S *s) { s->xyzzy(); }
void bar() { S s; foo(&s); }
