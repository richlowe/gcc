// Test that the nothrow optimization works properly.
// { dg-do compile }
// { dg-options "-fdump-tree-optimized" }

extern void blah() throw();

int i, j, k;

int main()
{
  try
    {
      ++i;
      blah();
      ++j;
    }
  catch (...)
    {
      return -42;
    }
}

// The catch block should be optimized away.
// { dg-final { cleanup-tree-dump "optimized" } }
