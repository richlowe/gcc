/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing -fstrict-aliasing" } */


struct S {
  int i;
  float f;
};

int foo () {
  struct S s;
  s.i = 7;
  float* f = &s.f;  /* { dg-bogus "float included in struct S" } */
  *f = 2.0;
  return s.i + (int)s.f;
}
