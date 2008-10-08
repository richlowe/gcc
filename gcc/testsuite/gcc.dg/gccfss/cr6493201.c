/* cr 6493201 internal compiler error: in ir_decref */
/* { dg-do compile } */

long vp;

int foo ()
{
  return (*((int(*)[]) vp))[0];
}
