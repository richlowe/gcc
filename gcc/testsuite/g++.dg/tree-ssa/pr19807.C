/* { dg-do compile } */
/* { dg-options "" } */

int a[4];
int *x, *y, *z;

void foo(void)
{
	x = &a[3] - 1;
	y = &a[1] + 1;
	z = 1 + &a[1];
}

void bar(int i)
{
	x = &a[i] - 1;
	y = &a[i] + 1;
	z = 1 + &a[i];
}

