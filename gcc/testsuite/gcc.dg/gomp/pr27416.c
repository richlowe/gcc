/* PR middle-end/27416 */
/* { dg-do compile } */

void
foo (void)
{
  int i = 0, j = 0;
#pragma omp for firstprivate (j) /* { g-error "is private in outer context" } */
  for (i = 0; i < 10; i++)
    j++;
}

int
bar (void)
{
  int i, j;
#pragma omp for lastprivate (j)	/* { g-error "is private in outer context" } */
  for (i = 0; i < 10; i++)
    j = i;
  return j;
}

int
baz (void)
{
  int i, j = 0;
#pragma omp for reduction (+:j)	/* { g-error "is private in outer context" } */
  for (i = 0; i < 10; i++)
    j++;
  return j;
}
