/* cr 6481447 */
/* { dg-do run } */

extern void abort();
typedef struct parameters {
    int vectors;
    int delay;
    int comp;
    int format;
    int version;
} parameters;

int test1(int val, parameters *s) __attribute__ ((noinline));
int test2(int val, parameters *s) __attribute__ ((noinline));

int test1(int val, parameters *s)
{
    s->format = 200;

    switch(val){
    case 0x10000000:
        s->version= 0;
        s->vectors=0;
        s->delay=1;
        break;
    case 0x10002000:
        s->version= 3;
        s->vectors=1;
        s->delay=1;
        s->comp=1;
        break;
    case 0x10003000:
        s->version= 3;
        s->vectors=1;
        s->delay=1;
        break;
    case 0x10003001:
        s->version= 3;
        s->vectors=0;
        s->delay=1;
        break;
    case 0x20001000:


    case 0x20100000 ... 0x2019ffff:

        s->delay=1;
        break;


    case 0x20200002 ... 0x202fffff:

    case 0x30202002:
    case 0x30203002:
        s->delay=0;
        break;
    default:
        s->delay=9;
    }

    return 0;
}

int test2(int val, parameters *s)
{
    s->format = 200;
    volatile int * ptr = &val;

    switch(*ptr){
    case 0x30202002:
    case 0x30203002:
        s->delay=0;
        break;
    case 0x2019ffff+1:
    case 0x202fffff+1:
        s->delay=9;
        break;

    case 0x20100000 ... 0x2019ffff:

        s->delay=1;
        break;


    case 0x20200002 ... 0x202fffff:
        s->delay=0;

    }

    return 0;
}

void foo(int (*test)(int val, parameters *s))
{
  volatile int var;
  struct parameters s;
  var = 0x20100000;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 1) abort();
  
  var = 0x20100001;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 1) abort();
  
  var = 0x2019fffe;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 1) abort();
  
  var = 0x2019ffff;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 1) abort();
  
  var = 0x20200002;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 0) abort();
  
  var = 0x20200003;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 0) abort();
  
  var = 0x202fffff;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 0) abort();
  
  var = 0x30202002;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 0) abort();
  
  var = 0x30203002;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 0) abort();
  
  var = 0x2019ffff + 1;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 9) abort();
  
  var = 0x202fffff + 1;
  s.delay = 10;
  test (var, &s);
  if (s.delay != 9) abort();
}

int main()
{
  foo (test1);
  foo (test2);
  return 0;
}
