/* { dg-do compile } */
/* Test that -Warray-bounds is enabled by -Wall */
/* { dg-options "-O2 -Wall" } */

int a[10];

int* f(void) {

    a[-1] = 0;             /* { g-warning "array subscript" } */ /* todo in iropt */

    return a;
}

