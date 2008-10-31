/* { dg-do compile } */
/* { dg-options "-O -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "DW_AT_const_value" { xfail sparc*-*-* } } } */

extern void x();
static void (*f)() = x;
