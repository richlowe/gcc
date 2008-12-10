// PR middle-end/34694
// { dg-do compile }
// { dg-options "-O -fopenmp -Wall" }

int i;

void
foo ()
{
#pragma omp parallel
  {
    int j;	// { g-warning "note: 'j' was declared here" }
    i = j;	// { g-warning "is used uninitialized" }
  }
}
