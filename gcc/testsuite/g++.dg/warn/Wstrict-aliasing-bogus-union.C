/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing -fstrict-aliasing" } */


union U {
  int i;
  float f;
};

float foo () {
  union U u;
  float* f = &u.f;  /* { dg-bogus "unions are holy in GCC" } */
  u.i = 2;
  return *f;
}
