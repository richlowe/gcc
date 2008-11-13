/* { dg-do compile } */
/* { dg-options "" } */

#include <limits.h>

int foo = INT_MAX + 1;  /* { dg-warning "integer overflow" } */

