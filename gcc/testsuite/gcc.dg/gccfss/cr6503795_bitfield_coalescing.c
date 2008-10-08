/* cr6503795 isv:gcc-3.4.1:internal compiler error: in expand_call_inline w/ -O2 */
/* { dg-do run } */

extern void abort();
struct c_scope
{
  _Bool function_body : 1;
  _Bool keep : 1;
};

void
foo (struct c_scope * p)
{
  p->function_body = 1;
  p->keep = 1;
}

int main()
{
  volatile struct c_scope c;
  foo ((struct c_scope*)&c);
  if (c.function_body != 1 || c.keep != 1)
    abort ();
  return 0;
}
