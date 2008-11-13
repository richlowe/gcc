/* { dg-do compile } */
/* { dg-options "-Wall -frtl-backend" } */

extern void function(void * x);

struct A {
    long x;
    char d[0];
};


void test(A * a) {
    function((char *)a - 4); /* { dg-bogus "below array bounds" } */
}

