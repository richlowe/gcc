/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

inline int foo (int i)
{
    if (i) return 1; /* { g-warning "is used uninitialized" } */ /* todo in iropt */
    return 0;
}

void baz();

void bar()
{
    int j;           /* { g-message "was declared here" } */
    for (; foo(j); ++j)
        baz();
}
