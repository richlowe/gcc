/* Test -msave-args */
/* { dg-do compile { target i?86-*-solaris2.* } } */
/* { dg-options "-m64 -msave-args" } */
/* { dg-final { scan-assembler "movq\t%rsi, -32\\(%rbp\\)" } } */

int
foo(int argc, char **argv)
{
    return (1);
}
