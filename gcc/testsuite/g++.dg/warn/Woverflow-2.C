/* { dg-do compile } */
/* { dg-options "-Woverflow" } */

#include <limits.h>

int foo = INT_MAX + 1;  /* { dg-warning "integer overflow" } */

