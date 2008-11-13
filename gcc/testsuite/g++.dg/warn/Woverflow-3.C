/* { dg-do compile } */
/* { dg-options "-Wno-overflow" } */

#include <limits.h>

int foo = INT_MAX + 1;

