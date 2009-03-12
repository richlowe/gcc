// { dg-do run { target native } }
// { dg-options "-fprofile-arcs -ftest-coverage" }
/* { dg-warning "assuming `-O1' option" } */
void
test_swap(int& x, int& y) throw()
{
  int tmp = x;
  x = y;
  y = tmp;
}

main()
{
  int i = 5;
  int j = 7;
  test_swap(i, j);
}

/* { dg-final { cleanup-coverage-files } } */
