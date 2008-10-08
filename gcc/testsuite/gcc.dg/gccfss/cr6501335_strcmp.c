/* cr 6501335 inline strcmp bug & libstdc++ testcase cw-16.cc fail to execute */
/* { dg-do run } */

typedef long size_t;
extern void abort();
extern size_t strlen (const char*);

int foo (const char * p)
{
  return __extension__ ({ 
  size_t __s1_len, __s2_len; 
  (__builtin_constant_p (p) 
   && __builtin_constant_p ("st") 
   && (__s1_len = strlen (p), __s2_len = strlen ("st"), 
       (!((size_t)(const void *)((p) + 1) 
	  - (size_t)(const void *)(p) == 1) 
	|| __s1_len >= 4) 
       && (!((size_t)(const void *)(("st") + 1) 
	     - (size_t)(const void *)("st") == 1) 
	   || __s2_len >= 4)) 
   ? __builtin_strcmp (p, "st") 
   : (__builtin_constant_p (p) 
      && ((size_t)(const void *)((p) + 1) - (size_t)(const void *)(p) == 1) 
      && (__s1_len = strlen (p), __s1_len < 4) 
      ? (__builtin_constant_p ("st") 
	 && ((size_t)(const void *)(("st") + 1) 
	     - (size_t)(const void *)("st") == 1) 
	 ? __builtin_strcmp (p, "st") 
	 : (__extension__ ({ 
	__const unsigned char *__s2 = (__const unsigned char *) (__const char *) ("st"); 
	register int __result = (((__const unsigned char *) (__const char *) (p))[0] - __s2[0]); 
	if (__s1_len > 0 && __result == 0) 
	{ __result = (((__const unsigned char *) (__const char *) (p))[1] - __s2[1]); 
	if (__s1_len > 1 && __result == 0) 
	{ __result = (((__const unsigned char *) (__const char *) (p))[2] - __s2[2]); 
	if (__s1_len > 2 && __result == 0) 
	__result = (((__const unsigned char *) (__const char *) (p))[3] - __s2[3]); 
	} 
	} __result; 
	}))) 
      : (__builtin_constant_p ("st") 
	 && ((size_t)(const void *)(("st") + 1) 
	     - (size_t)(const void *)("st") == 1) 
	 && (__s2_len = strlen ("st"), __s2_len < 4) 
	 ? (__builtin_constant_p (p) 
	    && ((size_t)(const void *)((p) + 1) - (size_t)(const void *)(p) == 1) 
	    ? __builtin_strcmp (p, "st") 
	    : (__extension__ ({ 
   __const unsigned char *__s1 = (__const unsigned char *) (__const char *) (p); 
   register int __result = __s1[0] - ((__const unsigned char *) (__const char *) ("st"))[0]; 
   if (__s2_len > 0 && __result == 0) 
   { __result = (__s1[1] - ((__const unsigned char *) (__const char *) ("st"))[1]); 
   if (__s2_len > 1 && __result == 0) 
   { __result = (__s1[2] - ((__const unsigned char *) (__const char *) ("st"))[2]); 
   if (__s2_len > 2 && __result == 0) 
   __result = (__s1[3] - ((__const unsigned char *) (__const char *) ("st"))[3]); 
   } 
   } __result; 
   }))) 
	 : __builtin_strcmp (p, "st")))); });
}

int main()
{
  if (foo ("st") != 0)
    abort ();
  if (foo ("sk") != -9)
    abort ();
  if (foo ("k") != -8)
    abort ();
  if (foo ("s") != -116)
    abort ();
  return 0;
}
