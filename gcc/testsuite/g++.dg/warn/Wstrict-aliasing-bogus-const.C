/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing -fstrict-aliasing" } */

int foo(int* p) {
  const int& q = *p;  /* { dg-bogus "const vs. non-const" } */
  *p = 1;
  return q;
}

