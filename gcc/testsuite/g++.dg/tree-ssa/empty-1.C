/* { dg-do compile } */
/* { dg-options "-fdump-tree-optimized" } */

struct S {};
S bar (const S &a)
{
  S s;
  s = a;
  return s;
}

/* Test whether memcpy call has been optimized out.  */
/* { dg-final { cleanup-tree-dump "optimized" } } */
