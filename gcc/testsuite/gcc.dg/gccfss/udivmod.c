/* test inline asm support */
/* { dg-do run } */

typedef int QItype __attribute__ ((mode (QI)));
typedef unsigned int UQItype __attribute__ ((mode (QI)));
typedef int HItype __attribute__ ((mode (HI)));
typedef unsigned int UHItype __attribute__ ((mode (HI)));


typedef int SItype __attribute__ ((mode (SI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));


typedef int DItype __attribute__ ((mode (DI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));

typedef float SFtype __attribute__ ((mode (SF)));


typedef float DFtype __attribute__ ((mode (DF)));








typedef int word_type __attribute__ ((mode (__word__)));

extern DItype __muldi3 (DItype, DItype);
extern DItype __divdi3 (DItype, DItype);
extern UDItype __udivdi3 (UDItype, UDItype);
extern UDItype __umoddi3 (UDItype, UDItype);
extern DItype __moddi3 (DItype, DItype);

extern DItype __negdi2 (DItype);


extern DItype __lshrdi3 (DItype, word_type);
extern DItype __ashldi3 (DItype, word_type);
extern DItype __ashrdi3 (DItype, word_type);







extern word_type __cmpdi2 (DItype, DItype);
extern word_type __ucmpdi2 (DItype, DItype);

extern SItype __absvsi2 (SItype);
extern SItype __addvsi3 (SItype, SItype);
extern SItype __subvsi3 (SItype, SItype);
extern SItype __mulvsi3 (SItype, SItype);
extern SItype __negvsi2 (SItype);
extern DItype __absvdi2 (DItype);
extern DItype __addvdi3 (DItype, DItype);
extern DItype __subvdi3 (DItype, DItype);
extern DItype __mulvdi3 (DItype, DItype);
extern DItype __negvdi2 (DItype);

extern DItype __fixsfdi (SFtype);
extern SFtype __floatdisf (DItype);
extern USItype __fixunssfsi (SFtype);
extern DItype __fixunssfdi (SFtype);
extern SFtype __powisf2 (SFtype, SItype);


extern DItype __fixdfdi (DFtype);
extern DFtype __floatdidf (DItype);
extern USItype __fixunsdfsi (DFtype);
extern DItype __fixunsdfdi (DFtype);
extern DFtype __powidf2 (DFtype, SItype);



extern const UQItype __clz_tab[] ;




  struct DWstruct {SItype high, low;};

typedef union
{
  struct DWstruct s;
  DItype ll;
} DWunion;
UDItype
my__udivmoddi4 (UDItype n, UDItype d, UDItype *rp)
{
  const DWunion nn = {.ll = n};
  const DWunion dd = {.ll = d};
  DWunion rr;
  USItype d0, d1, n0, n1, n2;
  USItype q0, q1;
  USItype b, bm;

  d0 = dd.s.low;
  d1 = dd.s.high;
  n0 = nn.s.low;
  n1 = nn.s.high;


  if (d1 == 0)
    {
      if (d0 > n1)
 {


   __asm__ ("! Inlined udiv_qrnnd\n" "	mov	32,%%g1\n" "	subcc	%1,%2,%%g0\n" "1:	bcs	5f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	sub	%1,%2,%1	! this kills msb of n\n" "	addx	%1,%1,%1	! so this can't give carry\n" "	subcc	%%g1,1,%%g1\n" "2:	bne	1b\n" "	 subcc	%1,%2,%%g0\n" "	bcs	3f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	b	3f\n" "	 sub	%1,%2,%1	! this kills msb of n\n" "4:	sub	%1,%2,%1\n" "5:	addxcc	%1,%1,%1\n" "	bcc	2b\n" "	 subcc	%%g1,1,%%g1\n" "! Got carry from n.  Subtract next step to cancel this carry.\n" "	bne	4b\n" "	 addcc	%0,%0,%0	! shift n1n0 and a 0-bit in lsb\n" "	sub	%1,%2,%1\n" "3:	xnor	%0,0,%0\n" "	! End of inline udiv_qrnnd" : "=&r" ((USItype) (q0)), "=&r" ((USItype) (n0)) : "r" ((USItype) (d0)), "1" ((USItype) (n1)), "0" ((USItype) (n0)) : "g1" , "cc");
   q1 = 0;


 }
      else
 {


   if (d0 == 0)
     d0 = 1 / d0;

   __asm__ ("! Inlined udiv_qrnnd\n" "	mov	32,%%g1\n" "	subcc	%1,%2,%%g0\n" "1:	bcs	5f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	sub	%1,%2,%1	! this kills msb of n\n" "	addx	%1,%1,%1	! so this can't give carry\n" "	subcc	%%g1,1,%%g1\n" "2:	bne	1b\n" "	 subcc	%1,%2,%%g0\n" "	bcs	3f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	b	3f\n" "	 sub	%1,%2,%1	! this kills msb of n\n" "4:	sub	%1,%2,%1\n" "5:	addxcc	%1,%1,%1\n" "	bcc	2b\n" "	 subcc	%%g1,1,%%g1\n" "! Got carry from n.  Subtract next step to cancel this carry.\n" "	bne	4b\n" "	 addcc	%0,%0,%0	! shift n1n0 and a 0-bit in lsb\n" "	sub	%1,%2,%1\n" "3:	xnor	%0,0,%0\n" "	! End of inline udiv_qrnnd" : "=&r" ((USItype) (q1)), "=&r" ((USItype) (n1)) : "r" ((USItype) (d0)), "1" ((USItype) (0)), "0" ((USItype) (n1)) : "g1" , "cc");
   __asm__ ("! Inlined udiv_qrnnd\n" "	mov	32,%%g1\n" "	subcc	%1,%2,%%g0\n" "1:	bcs	5f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	sub	%1,%2,%1	! this kills msb of n\n" "	addx	%1,%1,%1	! so this can't give carry\n" "	subcc	%%g1,1,%%g1\n" "2:	bne	1b\n" "	 subcc	%1,%2,%%g0\n" "	bcs	3f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	b	3f\n" "	 sub	%1,%2,%1	! this kills msb of n\n" "4:	sub	%1,%2,%1\n" "5:	addxcc	%1,%1,%1\n" "	bcc	2b\n" "	 subcc	%%g1,1,%%g1\n" "! Got carry from n.  Subtract next step to cancel this carry.\n" "	bne	4b\n" "	 addcc	%0,%0,%0	! shift n1n0 and a 0-bit in lsb\n" "	sub	%1,%2,%1\n" "3:	xnor	%0,0,%0\n" "	! End of inline udiv_qrnnd" : "=&r" ((USItype) (q0)), "=&r" ((USItype) (n0)) : "r" ((USItype) (d0)), "1" ((USItype) (n1)), "0" ((USItype) (n0)) : "g1" , "cc");


 }

      if (rp != 0)
 {
   rr.s.low = n0;
   rr.s.high = 0;
   *rp = rr.ll;
 }
    }

  else
    {
      if (d1 > n1)
 {


   q0 = 0;
   q1 = 0;


   if (rp != 0)
     {
       rr.s.low = n0;
       rr.s.high = n1;
       *rp = rr.ll;
     }
 }
      else
 {


   do { USItype __xr = (d1); USItype __a; if ((4 * 8) <= 32) { __a = __xr < ((USItype)1<<2*((4 * 8) / 4)) ? (__xr < ((USItype)1<<((4 * 8) / 4)) ? 0 : ((4 * 8) / 4)) : (__xr < ((USItype)1<<3*((4 * 8) / 4)) ? 2*((4 * 8) / 4) : 3*((4 * 8) / 4)); } else { for (__a = (4 * 8) - 8; __a > 0; __a -= 8) if (((__xr >> __a) & 0xff) != 0) break; } (bm) = (4 * 8) - (__clz_tab[__xr >> __a] + __a); } while (0);
   if (bm == 0)
     {

       if (n1 > d1 || n0 >= d0)
  {
    q0 = 1;
    __asm__ ("subcc %r4,%5,%1\n\tsubx %r2,%3,%0" : "=r" ((USItype) (n1)), "=&r" ((USItype) (n0)) : "rJ" ((USItype) (n1)), "rI" ((USItype) (d1)), "rJ" ((USItype) (n0)), "rI" ((USItype) (d0)) : "cc");
  }
       else
  q0 = 0;

       q1 = 0;

       if (rp != 0)
  {
    rr.s.low = n0;
    rr.s.high = n1;
    *rp = rr.ll;
  }
     }
   else
     {
       USItype m1, m0;


       b = (4 * 8) - bm;

       d1 = (d1 << bm) | (d0 >> b);
       d0 = d0 << bm;
       n2 = n1 >> b;
       n1 = (n1 << bm) | (n0 >> b);
       n0 = n0 << bm;

       __asm__ ("! Inlined udiv_qrnnd\n" "	mov	32,%%g1\n" "	subcc	%1,%2,%%g0\n" "1:	bcs	5f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	sub	%1,%2,%1	! this kills msb of n\n" "	addx	%1,%1,%1	! so this can't give carry\n" "	subcc	%%g1,1,%%g1\n" "2:	bne	1b\n" "	 subcc	%1,%2,%%g0\n" "	bcs	3f\n" "	 addxcc %0,%0,%0	! shift n1n0 and a q-bit in lsb\n" "	b	3f\n" "	 sub	%1,%2,%1	! this kills msb of n\n" "4:	sub	%1,%2,%1\n" "5:	addxcc	%1,%1,%1\n" "	bcc	2b\n" "	 subcc	%%g1,1,%%g1\n" "! Got carry from n.  Subtract next step to cancel this carry.\n" "	bne	4b\n" "	 addcc	%0,%0,%0	! shift n1n0 and a 0-bit in lsb\n" "	sub	%1,%2,%1\n" "3:	xnor	%0,0,%0\n" "	! End of inline udiv_qrnnd" : "=&r" ((USItype) (q0)), "=&r" ((USItype) (n1)) : "r" ((USItype) (d1)), "1" ((USItype) (n2)), "0" ((USItype) (n1)) : "g1" , "cc");
       __asm__ ("! Inlined umul_ppmm\n" "	wr	%%g0,%2,%%y	! SPARC has 0-3 delay insn after a wr\n""	sra	%3,31,%%o5	! Don't move this insn\n" "	and	%2,%%o5,%%o5	! Don't move this insn\n" "	andcc	%%g0,0,%%g1	! Don't move this insn\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,%3,%%g1\n" "	mulscc	%%g1,0,%%g1\n" "	add	%%g1,%%o5,%0\n" "	rd	%%y,%1" : "=r" ((USItype) (m1)), "=r" ((USItype) (m0)) : "%rI" ((USItype) (q0)), "r" ((USItype) (d0)) : "g1", "o5" , "cc");

       if (m1 > n1 || (m1 == n1 && m0 > n0))
  {
    q0--;
    __asm__ ("subcc %r4,%5,%1\n\tsubx %r2,%3,%0" : "=r" ((USItype) (m1)), "=&r" ((USItype) (m0)) : "rJ" ((USItype) (m1)), "rI" ((USItype) (d1)), "rJ" ((USItype) (m0)), "rI" ((USItype) (d0)) : "cc");
  }

       q1 = 0;


       if (rp != 0)
  {
    __asm__ ("subcc %r4,%5,%1\n\tsubx %r2,%3,%0" : "=r" ((USItype) (n1)), "=&r" ((USItype) (n0)) : "rJ" ((USItype) (n1)), "rI" ((USItype) (m1)), "rJ" ((USItype) (n0)), "rI" ((USItype) (m0)) : "cc");
    rr.s.low = (n1 << b) | (n0 >> bm);
    rr.s.high = n1 >> bm;
    *rp = rr.ll;
  }
     }
 }
    }

  const DWunion ww = {{.low = q0, .high = q1}};
  return ww.ll;
}
long long
xlrandom ()
{
  long long x;
  unsigned a;
  int bits = 64;
  unsigned b;

  do
    {
      a = random ();
      b = (a & 15) + 1;
      x <<= b;				/* shift up 1-16 steps */
      a = (a >> 18) & 1;
      if (a)
	x |= (unsigned) (1 << b) - 1;
      bits -= b;
    }
  while (bits >= 0);
  return x;
}


extern int printf (const char *, ...);
extern void abort ();

main ()
{
  int i;
  UDItype n, d, q, r, rr;

  for (i = 0; i <= 1000000; i++)
    {
      n = xlrandom ();
      d = xlrandom ();
      if (d == 0)
	continue;

      q = my__udivmoddi4 (n, d, &r);

      rr = n - q * d;
      if (rr != r || r >= d)
	{
	  printf ("Testing udivmoddi4: failure after %d iterations\n", i);
	  printf ("n=%lX%08lX\n", (unsigned) (n >> 32), (unsigned) n);
	  printf ("d=%lX%08lX\n", (unsigned) (d >> 32), (unsigned) d);
	  printf ("q=%lX%08lX\n", (unsigned) (q >> 32), (unsigned) q);
	  printf ("r=%lX%08lX\n", (unsigned) (r >> 32), (unsigned) r);
	  printf ("rr=%lX%08lX\n", (unsigned) (rr >> 32), (unsigned) rr);
	  abort ();
	}
    }

  return 0;
}
