// { dg-do compile }
// { dg-options "" }

extern void foo (void);
int c;
void foo (int n)
{
  int j = 0;
  try
    {
      for(;;)
        {
          foo ();
          if (j ++ == n)
            break;
          foo ();
        }
    }
  catch (...)
    {
      c = j;
    }
}
