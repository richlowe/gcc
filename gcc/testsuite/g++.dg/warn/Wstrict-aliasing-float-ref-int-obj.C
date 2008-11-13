/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing -fstrict-aliasing" } */


int foo() {
  int x;
  float& q = reinterpret_cast<float&> (x);  /* { dg-warning "type-punn" } */
  q = 1.0;
  return x;
}
