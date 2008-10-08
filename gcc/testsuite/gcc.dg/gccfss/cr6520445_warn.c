/* cr 6520445 incorrect warning */
/* { dg-do compile } */

struct S1 {int i;};
struct S2 {int i;};

union U{
    struct S1 a;
    struct S2 b;
};

struct S{
    union U u;
};
void foo (void *);

struct S volatile ss1[10];
volatile struct S ss2[10];
struct S * volatile ss3;
volatile struct S * ss4;

void bar (int i)
{
  volatile int vv;
  foo (&ss1[i].u); /* { dg-warning "discards qualifiers" } */
  foo (&ss2[i].u); /* { dg-warning "discards qualifiers" } */
  foo (&ss3[i].u); /* { dg-bogus "warning" "discards qualifiers" } */
  foo (&ss4[i].u); /* { dg-warning "discards qualifiers" } */
  foo (&vv); /* { dg-warning "discards qualifiers" } */
}
