/* cr 6477785 */ 
/* { dg-do run } */

struct S {
  int x;
};
typedef struct S A;

int main()
{
  int i = 123;

  switch ((A){i}.x);

  for ((int[]){i}; (int[]){i}; (int[]){i}) break;

  while ((A){i}.x + (A){i}.x + (A){i}.x) break;

  return 0;
}

