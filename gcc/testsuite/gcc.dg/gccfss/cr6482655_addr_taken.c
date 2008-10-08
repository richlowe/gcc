/* cr 6482655 */ 
/* { dg-do compile } */

struct S{
   int var;
};

void *foo(int i, void * b)
{
 register struct S * ar = (struct S*)b;
 return &ar[i].var;
}
