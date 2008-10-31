/* { dg-do compile } */
/* { dg-options "-O2 -Wuninitialized" } */

int sys_msgctl (void)
{
  struct { int mode; } setbuf;  /* TODO: warn in iropt. was: dg-warning "'setbuf\.mode' is used" */
  return setbuf.mode;
}
