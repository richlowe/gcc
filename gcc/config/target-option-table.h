/* Operating system specific defines to be used when targeting GCC for any
   Solaris 2 system.
   Copyright 2002, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* Modified by Sun Microsystems 2008 */

/* Handle the SunStudio options that start with 'f',
   or contain ':' [which can't handle in spec] */
/* look in gccspec.c and g++spec.c for language specific 
   ones expansions such as -fast */
/* If you have -option=foo and -option=foobar then put
   -option=foobar before -option=foo in the following table */

#ifdef __linux
#define LINUX_TARGET_OPTION_TRANSLATE_TABLE \
{ "-m64", "-m64 -mcmodel=medlow" },
#else
#define LINUX_TARGET_OPTION_TRANSLATE_TABLE
#endif

#ifdef CROSS_COMPILE
#define NATIVE_OPTION_TRANSLATE_TABLE \
  { "-native", "-xtarget=generic" }, \
  { "-xarch=native64", "-Zarch=v9 -Zarchm64=v9 -Zm=64" },\
  { "-xarch=native", "-Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -Zm=32" },\
  { "-xtarget=native64", "-Zarch=generic -Zarchm32=generic -Zarchm64=v9 -xchip=generic -xcache=generic -Zm=64"}, \
  { "-xtarget=native", "-Zarch=generic -Zarchm32=generic -Zarchm64=v9 -xchip=generic -xcache=generic -Zm=32"}, 
#else
#define NATIVE_OPTION_TRANSLATE_TABLE \
  { "-native", "-xtarget=native" }, \
  { "-xarch=native64", "-xarch=native64 -Zm=64"}, \
  { "-xarch=native", "-xarch=native -Zm=32"}, \
  { "-xtarget=native64", "-xtarget=native64 -Zm=64"}, \
  { "-xtarget=native", "-xtarget=native -Zm=32"},
#endif

#define SUN_TARGET_OPTION_TRANSLATE_TABLE \
  { "-dalign", "-xmemalign=8s" }, \
  { "-ffast-math", "-ffast-math -Zfsimple=2"}, \
  { "-fgot", "-Zignore"}, \
  { "-fma=none", "-Zfma=none"}, \
  { "-fma=fused", "-Zfma=fused"}, \
  { "-fno-fast-math", "-fno-fast-math -Zfsimple=0"}, \
  { "-fno-got", "-Zfno-got"}, \
  { "-fpic", "-xcode=pic13" }, \
  { "-fPIC", "-xcode=pic32" }, \
  { "-fprofile-arcs", "-Zfprofile-arcs -xprofile=collect=a.out" },\
  { "-fprofile-generate", "-Zfprofile-generate -xprofile=collect=a.out" },\
  { "-fprofile-values", "-Zfprofile-values"}, \
  { "-fbranch-probabilities", "-Zfbranch-probabilities -xprofile=use=a.out" },\
  { "-fprofile-use", "-Zfprofile-use -xprofile=use=a.out" },\
  { "-fsimple", "-ffast-math -Zfsimple=1" }, \
  { "-fsimple=0", "-fno-fast-math -Zfsimple=0" }, \
  { "-fsimple=1", "-ffast-math -Zfsimple=1" }, \
  { "-fsimple=2", "-ffast-math -Zfsimple=2" }, \
  { "-fns", "-Zfns=yes" }, \
  { "-fnonstd", "-Zfns=yes -Zftrap=common"}, \
  { "-fvpt", "-Zfvpt"}, \
  { "-KPIC", "-xcode=pic32" }, \
  { "-Kpic", "-xcode=pic13" }, \
  { "-mapp-regs", "-mapp-regs -xregs=appl" }, \
  { "-mno-app-regs", "-mno-app-regs -xregs=no%appl" }, \
  { "-mfpu", "-mfpu -xregs=float" }, \
  { "-mno-fpu", "-mno-fpu -xregs=no%float" }, \
  { "-mcpu=v7", "-mcpu=v7 -Zarch=v7 -Zarchm32=v7 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=cypress", "-mcpu=cypress -Zarch=v7 -Zarchm32=v7 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=v8", "-mcpu=v8 -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=supersparc", "-mcpu=supersparc -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=sparclite", "-mcpu=sparclite -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=f930", "-mcpu=f930 -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=f934", "-mcpu=f934 -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=hypersparc", "-mcpu=hypersparc -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=sparclite86x", "-mcpu=sparclite86x -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=sparclet", "-mcpu=sparclet -Zarch=v8 -Zarchm32=v8 -xchip=generic -xcache=generic"}, \
  { "-mcpu=tsc701", "-mcpu=tsc701 -Zarch=v8 -Zarchm32=v8 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-mcpu=v9", "-mcpu=v9 -Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -xchip=generic -xcache=generic"}, \
  { "-mcpu=ultrasparc2i", "-mcpu=ultrasparc2i -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2 -xcache=16/32/1:512/64/1"}, \
  { "-mcpu=ultrasparc2e", "-mcpu=ultrasparc2e -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2 -xcache=16/32/1:256/64/4"}, \
  { "-mcpu=ultrasparc2", "-mcpu=ultrasparc2 -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2 -xcache=16/32/1:512/64/1"}, \
  { "-mcpu=ultrasparc3iplus", "-mcpu=ultrasparc3iplus -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3iplus -xcache=64/32/4:4096/64/4"}, \
  { "-mcpu=ultrasparc3i", "-mcpu=ultrasparc3i -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3i -xcache=64/32/4:1024/64/4"}, \
  { "-mcpu=ultrasparc3cu", "-mcpu=ultrasparc3cu -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3cu -xcache=64/32/4:8192/512/2"}, \
  { "-mcpu=ultrasparc3", "-mcpu=ultrasparc3 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3 -xcache=64/32/4:8192/512/2"}, \
  { "-mcpu=ultrasparc4plus", "-mcpu=ultrasparc4plus -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra4plus -xcache=64/32/4:2048/64/4:32768/64/4"}, \
  { "-mcpu=panther", "-mcpu=ultrasparc4 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra4 -xcache=64/32/4:8192/128/2"}, \
  { "-mcpu=ultrasparc4", "-mcpu=ultrasparc4 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra4 -xcache=64/32/4:8192/128/2"}, \
  { "-mcpu=ultrasparc", "-mcpu=ultrasparc -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra -xcache=16/32/1:512/64/1"}, \
  { "-mcpu=niagara", "-mcpu=niagara -Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -xchip=ultraT1 -xcache=8/16/4/4:3072/64/12/32"}, \
  { "-mcpu=ultraT1", "-mcpu=ultraT1 -Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -xchip=ultraT1 -xcache=8/16/4/4:3072/64/12/32"}, \
  { "-mcpu=niagara2", "-mcpu=ultraT2 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultraT2 -xcache=8/16/4:4096/64/16"}, \
  { "-mcpu=ultraT2", "-mcpu=ultraT2 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultraT2 -xcache=8/16/4:4096/64/16"}, \
  { "-mcpu=ultraT2plus", "-mcpu=ultraT2plus -Zarch=sparcvis2 -Zarchm32=sparcvis2 -Zarchm64=v9b -xchip=ultraT2plus -xcache=8/16/4:4096/64/16"}, \
  { "-mcpu=sparc64vii", "-mcpu=sparc64vii -Zarch=v8plusd -Zarchm32=v8plusd -Zarchm64=v9d -xchip=sparc64vii -xcache=128/64/2:5120/64/10"}, \
  { "-mcpu=sparc64vi", "-mcpu=sparc64vi -Zarch=v8plusc -Zarchm32=v8plusc -Zarchm64=v9c -xchip=sparc64vi -xcache=128/64/2:5120/256/4"}, \
  { "-mcypress", "-mcpu=cypress -mno-vis -Zarch=v7 -Zarchm32=v7 -xchip=generic -xcache=generic"}, \
  { "-mf930", "-mcpu=f930 -mno-vis -Zarch=v8 -Zarchm32=v8 -xchip=generic -xcache=generic"}, \
  { "-mf934", "-mcpu=f934 -mno-vis -Zarch=v8 -Zarchm32=v8 -xchip=generic -xcache=generic"}, \
  { "-msparclite", "-mcpu=sparclite -mno-vis -Zarch=v8 -Zarchm32=v8 -xchip=generic -xcache=generic"}, \
  { "-msupersparc", "-mcpu=supersparc -mno-vis -Zarch=v8 -Zarchm32=v8 -xchip=generic -xcache=generic"}, \
  { "-mtune=v7", "-mtune=v7 -xchip=generic" }, \
  { "-mtune=v8", "-mtune=v8 -xchip=generic" }, \
  { "-mtune=cypress", "-mtune=cypress -xchip=generic" }, \
  { "-mtune=supersparc", "-mtune=supersparc -xchip=generic" }, \
  { "-mtune=sparclite", "-mtune=sparclite -xchip=generic" }, \
  { "-mtune=f930", "-mtune=f930 -xchip=generic" }, \
  { "-mtune=f934", "-mtune=f934 -xchip=generic" }, \
  { "-mtune=hypersparc", "-mtune=hypersparc -xchip=generic" }, \
  { "-mtune=sparclite86x", "-mtune=sparelite86x -xchip=generic" }, \
  { "-mtune=sparclet", "-mtune=sparclet -xchip=generic" }, \
  { "-mtune=tsc701", "-mtune=tsc701 -xchip=generic" }, \
  { "-mtune=v9", "-mtune=v9 -xchip=generic" }, \
  { "-mtune=ultrasparc2i", "-mtune=ultrasparc2i -xchip=ultra2i" }, \
  { "-mtune=ultrasparc2e", "-mtune=ultrasparc2e -xchip=ultra2e" }, \
  { "-mtune=ultrasparc2", "-mtune=ultrasparc2 -xchip=ultra2" }, \
  { "-mtune=ultrasparc3cu", "-mtune=ultrasparc3cu -xchip=ultra3cu" }, \
  { "-mtune=ultrasparc3i", "-mtune=ultrasparc3i -xchip=ultra3i" }, \
  { "-mtune=ultrasparc3", "-mtune=ultrasparc3 -xchip=ultra3" }, \
  { "-mtune=ultrasparc4plus", "-mtune=ultrasparc4plus -xchip=ultra4plus" }, \
  { "-mtune=panther", "-mtune=ultrasparc4 -xchip=ultra4" }, \
  { "-mtune=ultrasparc4", "-mtune=ultrasparc4 -xchip=ultra4" }, \
  { "-mtune=ultrasparc", "-mtune=ultrasparc -xchip=ultra" }, \
  { "-mtune=niagara", "-mtune=niagara -xchip=ultraT1" }, \
  { "-mtune=niagara2", "-mtune=niagara2 -xchip=ultraT2" }, \
  { "-mtune=ultraT1", "-mtune=ultraT1 -xchip=ultraT1" }, \
  { "-mtune=ultraT2", "-mtune=ultraT2 -xchip=ultraT2" }, \
  { "-mtune=ultraT2plus", "-mtune=ultraT2plus -xchip=ultraT2plus" }, \
  { "-mtune=sparc64vii", "-mtune=sparc64vii -xchip=sparc64vii" },\
  { "-mtune=sparc64vi", "-mtune=sparc64vi -xchip=sparc64vi" },\
  { "-mv8", "-mcpu=v8 -Zarch=v8 -Zarchm32=v8 -Zarchm64=v9 -mno-vis -xchip=generic -xcache=generic"}, \
  { "-time", "-time -Ztime" }, \
  { "-xannotate", "-xannotate=yes" }, \
  { "-xarch=sparcima", "-Zarch=v8plusd -Zarchm32=v8plusd -Zarchm64=v9d -Zm=32" },\
  { "-xarch=sparcfmaf", "-Zarch=v8plusc -Zarchm32=v8plusc -Zarchm64=v9c -Zm=32" },\
  { "-xarch=sparcvis2", "-Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -Zm=32" },\
  { "-xarch=sparcvis3", "-Zarch=v8plusd -Zarchm32=v8plusd -Zarchm64=v9d -Zm=32" },\
  { "-xarch=sparcvis", "-Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -Zm=32" },\
  { "-xarch=sparc", "-Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -Zm=32" },\
  { "-xarch=v7", "-Zarch=v7 -Zarchm32=v7 -Zm=32" },\
  { "-xarch=v8", "-Zarch=v8 -Zarchm32=v8 -Zm=32" },\
  { "-xarch=v8a", "-Zarch=v8a -Zarchm32=v8a -Zm=32" },\
  { "-xarch=v8plusa", "-Zarch=v8plusa -Zarchm32=v8plusa -Zm=32" },\
  { "-xarch=v8plusb", "-Zarch=v8plusb -Zarchm32=v8plusb -Zm=32" },\
  { "-xarch=v8plusc", "-Zarch=v8plusc -Zarchm32=v8plusc -Zm=32" },\
  { "-xarch=v8plusd", "-Zarch=v8plusd -Zarchm32=v8plusd -Zm=32" },\
  { "-xarch=v8plus", "-Zarch=v8plus -Zarchm32=v8plus -Zm=32" },\
  { "-xarch=v9a", "-Zarch=v9a -Zarchm64=v9a -Zm=64" },\
  { "-xarch=v9b", "-Zarch=v9b -Zarchm64=v9b -Zm=64" },\
  { "-xarch=v9c", "-Zarch=v9c -Zarchm64=v9c -Zm=64" },\
  { "-xarch=v9d", "-Zarch=v9d -Zarchm64=v9d -Zm=64" },\
  { "-xarch=v9", "-Zarch=v9 -Zarchm64=v9 -Zm=64" },\
  { "-xarch=generic64", "-Zarch=v9 -Zarchm64=v9 -Zm=64" },\
  { "-xarch=generic", "-Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -Zm=32" },\
  { "-xbuiltin", "-xbuiltin=%all" }, \
  { "-xdepend", "-xdepend=yes" }, \
  { "-xipo", "-xipo=1" }, \
  { "-xinline=", "-xinline=@none" } /* add @none so lack of func wont cause problems with other matchin */, \
  { "-xinline=%auto", "-xinline=@auto" } /* want to match on %auto but speclang won't allow it */, \
  { "-xmemalign", "-xmemalign=1i" }, \
  { "-xparallel", "-xautopar -xdepend=yes -xexplicitpar" }, \
  { "-xprefetch", "-xprefetch=auto,explicit" }, \
  { "-xprofile=collect", "-xprofile=collect=a.out" }, \
  { "-xprofile=use", "-xprofile=use=a.out" }, \
  { "-xsunir-backend", "-Zsunir-backend" }, \
  { "-xtarget=sparc64vii", "-mcpu=sparc64vii -Zarch=v8plusd -Zarchm32=v8plusd -Zarchm64=v9d -xchip=sparc64vii -xcache=128/64/2:5120/64/10"}, \
  { "-xtarget=sparc64vi", "-mcpu=sparc64vi -Zarch=v8plusc -Zarchm32=v8plusc -Zarchm64=v9c -xchip=sparc64vi -xcache=128/64/2:5120/256/4"}, \
  { "-xtarget=ultra", "-mcpu=ultrasparc -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra -xcache=16/32/1:512/64/1"}, \
  { "-xtarget=ultra2", "-mcpu=ultrasparc2 -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2 -xcache=16/32/1:512/64/1"}, \
  { "-xtarget=ultra2i", "-mcpu=ultrasparc2i -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2i -xcache=16/32/1:512/64/1"}, \
  { "-xtarget=ultra2e", "-mcpu=ultrasparc2e -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2e -xcache=16/32/1:256/64/4"}, \
  { "-xtarget=ultra3", "-mcpu=ultrasparc3 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3 -xcache=64/32/4:8192/512/2"}, \
  { "-xtarget=ultra3cu", "-mcpu=ultrasparc3cu -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3cu -xcache=64/32/4:8192/512/2"}, \
  { "-xtarget=ultra3i", "-mcpu=ultrasparc3i -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3i -xcache=64/32/4:1024/64/4"}, \
  { "-xtarget=ultra4", "-mcpu=ultrasparc4 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra4 -xcache=64/32/4:8192/128/2"}, \
  { "-xtarget=gemini", "-mcpu=gemini -Zarch=v8plusa -Zarchm32=v8plusa -Zarchm64=v9a -xchip=ultra2 -xcache=16/32/1:512/64/4"}, \
  { "-xtarget=panther", "-mcpu=ultrasparc4 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra4 -xcache=64/32/4:8192/128/2"}, \
  { "-xtarget=ultra4plus", "-mcpu=ultrasparc4plus -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra4plus -xcache=64/32/4:2048/64/4:32768/64/4"}, \
  { "-xtarget=ultra3iplus", "-mcpu=ultrasparc3iplus -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultra3iplus -xcache=64/32/4:4096/64/4"}, \
  { "-xtarget=ultraT1", "-mcpu=ultraT1 -Zarch=v8plus -Zarchm32=v8plus -Zarchm64=v9 -xchip=ultraT1 -xcache=8/16/4/4:3072/64/12/32"}, \
  { "-xtarget=ultraT2", "-mcpu=ultraT2 -Zarch=v8plusb -Zarchm32=v8plusb -Zarchm64=v9b -xchip=ultraT2 -xcache=8/16/4:4096/64/16"}, \
  { "-xtarget=ultraT2plus", "-mcpu=ultraT2plus -Zarch=sparcvis2 -Zarchm32=sparcvis2 -Zarchm64=v9b -xchip=ultraT2plus -xcache=8/16/4:4096/64/16"}, \
  { "-xtarget=generic", "-Zarch=generic -Zarchm32=generic -Zarchm64=v9 -xchip=generic -xcache=generic -Zm=32"}, \
  { "-xtarget=generic64", "-Zarch=generic -Zarchm32=generic -Zarchm64=v9 -xchip=generic -xcache=generic -Zm=64"}, \
  { "-xvector", "-xvector=yes"}, \
  { "-xvis=no", "-mno-vis"}, \
  { "-xvis=yes", "-mvis"}, \
  { "-xvis", "-mvis"}, \
  { "-Wd,-fast_phase_1", "-Zipo_fast_phase_1"}, \
  { "-Wd,-pec", "-xpec"}, \
  { "-Wd,-xsafe=unboundsym", "-xsafe=unboundsym"}, \
  { "-Wd,-w", "-Zquietdriver" }

#define TARGET_OPTION_TRANSLATE_TABLE \
	      NATIVE_OPTION_TRANSLATE_TABLE \
              LINUX_TARGET_OPTION_TRANSLATE_TABLE \
              SUN_TARGET_OPTION_TRANSLATE_TABLE 
