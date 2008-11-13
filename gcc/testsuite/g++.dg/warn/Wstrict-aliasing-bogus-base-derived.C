/* { dg-do compile } */
/* { dg-options "-Wstrict-aliasing -fstrict-aliasing" } */


class base {
 public:
  int x;
};

class derived: public base {
 public:
  int y;
};

base foo () {
  derived d;
  base* pb = &d;  /* { dg-bogus "base vs. derived" } */
  pb->x = 1;

  return d;
}
