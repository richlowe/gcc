/*cr 6511316 ISV:ruby-1.8.1 -O3 caused the build*/
/* { dg-do run } */

#include <stdlib.h>

extern void abort();
typedef struct RNode {
    unsigned long flags;
} NODE;
#define R_CAST(st)   (struct st*)

#define CHAR_BIT 8
#define FL_USHIFT    11

#define RNODE(obj)  (R_CAST(RNode)(obj))

#define NODE_LSHIFT (FL_USHIFT+8)

#define NODE_LMASK  (((long)1<<(sizeof(NODE*)*CHAR_BIT-NODE_LSHIFT))-1)
#define nd_line(n) ((unsigned int)(((RNODE(n))->flags>>NODE_LSHIFT)&NODE_LMASK))
#define nd_set_line(n,l) \
    RNODE(n)->flags=((RNODE(n)->flags&~(-1<<NODE_LSHIFT))|(((l)&NODE_LMASK)<<NODE_LSHIFT))

int main(void)
{
  NODE *node, *orig;
  node = (NODE *)malloc(sizeof(NODE));
  orig = (NODE *)malloc(sizeof(NODE));

  node->flags = orig->flags =0xfffff;
  nd_set_line(node, nd_line(orig));
  if (node->flags != 0xfffff)
    abort ();
  return 0;
}

