/	.asciz  "@(#)cpu_type.s 1.10 07/06/06 SMI"
/	Copyright (c) 1993 by Sun Microsystems, Inc.

/    This function is implemented based on the following documentation:
/
/       Intel Processor Identification and the CPUID Instruction 
/       (Application note 485, rev. 31, Sept 2006, Document number 241618-031)
/
/       AMD CPUID Specification, Publication #25481, rev. 2.18, Jan 2006
/
/ 	return struct cpu_type_info:
/			int family;		  ;/ offset: 0
/			int model;		  ;/ offset: 4
/			int stepping;		  ;/ offset: 8
/			int basic_features_ecx;   ;/ offset: 12 
/			int basic_features_edx;   ;/ offset: 16
/			int ext_features_ecx;     ;/ offset: 20
/			int ext_features_edx;     ;/ offset: 24


	.set	EFL_AC, 0x40000
	.set	EFL_ID, 0x200000

	.type	x86cpu_type,@function
	.text
	.globl	x86cpu_type
	.align	4
x86cpu_type:
	pushl	%ebp
	movl	%esp,%ebp
	pushl	%ebx

/ Initialize return structure
	movl	8(%ebp), %ebx
	movl	$0, (%ebx)
	movl	$0, 4(%ebx)
	movl	$0, 8(%ebx)
	movl	$0, 12(%ebx)
	movl	$0, 16(%ebx)
	movl	$0, 20(%ebx)
	movl	$0, 24(%ebx)

/ Check if it is 80386
	pushfl
	popl	%eax		;/ get EFLAGS in eax

	pushl	%eax		;/ save EFLAGS, to restore them before exit

	movl	%eax, %ecx
	xorl	$EFL_AC, %eax	;/ flip AC bit in flags

	pushl	%eax		;/ save modified EFLAGS
	popfl

	pushfl			;/ get modified EFLAGS
	popl	%eax

	cmpl	%eax, %ecx	;/ EFLAGS is not changed -> this is 80386
	je	cpu_is_386

/ Check if it is 80486
	movl	%ecx, %eax	;/ get original EFLAGS in eax
	xorl	$EFL_ID, %eax	;/ flip ID bit

	pushl	%eax		;/ save modified EFLAGS
	popfl

	pushfl			;/ get modified EFLAGS
	popl	%eax

	cmpl	%eax, %ecx	;/ EFLAGS is not changed -> this is 80486
	je	cpu_is_486

/ We have a processor which supports the cpuid instruction, 
/ we use it to identify the processor

/ Check if function 1 of cpuid is supported 
	xorl	%eax,%eax
	cpuid
	orl	%eax,%eax 	;/ Max function of cpuid is 0, 
	jz	exit_point	;/ function 1 is not supported

	movl	$1,%eax
	cpuid

/ Save ecx and edx to the return stuct
	movl	8(%ebp), %ebx
	movl    %ecx, 12(%ebx)
	movl    %edx, 16(%ebx)	
	
/ Determine cpu stepping: it's bits 0-3 of eax
	pushl	%eax
	andl	$0xf, %eax
	movl	%eax, 8(%ebx)
	popl	%eax

/ Determine cpu family: 
/ F = Extended Family + Family
/ F = CPUID(1).EAX[27:20] + CPUID(1).EAX[11:8]
	pushl	%eax
	movl	%eax, %edx 
	sarl	$8,%eax
	andl	$0xf,%eax	
        sarl	$20, %edx
	andl    $0xff, %edx	
	addl    %edx, %eax
	movl	%eax, (%ebx)

/ Determine cpu model:
/ M = (Extended Model << 4) + Model
/ M = (CPUID(1).EAX[19:16] << 4) + CPUID(1).EAX[7:4]
	popl 	%eax
	movl	%eax, %edx
	sarl 	$4, %eax
	andl	$0xf, %eax
	sarl 	$12, %edx
	andl 	$0xf0, %edx
	orl 	%edx, %eax
	movl	%eax, 4(%ebx)

/ Determine extended feature flags

/ Check if function 0x80000001 is available
	movl	$0x80000000, %eax
	cpuid
	cmpl	$0x80000001, %eax
	jb	exit_point

/ Save function 0x80000001 returned value in edx
	movl	$0x80000001, %eax
	cpuid
	movl	8(%ebp), %ebx
	movl	%ecx, 20(%ebx)
	movl	%edx, 24(%ebx)
	jmp	exit_point

cpu_is_386:
	movl	$3, (%ebx)
	jmp	exit_point

cpu_is_486:
	movl	$4, (%ebx)
	jmp	exit_point

exit_point:
	popfl
	popl	%ebx
	movl 	8(%ebp),%eax
	movl	%ebp, %esp
	popl	%ebp
	ret
	.size	x86cpu_type,.-x86cpu_type

/ FUNCTION:
/ unsigned char* x86_cpu_cache_info(unsigned* count)
/ PURPOSE:
/   This routine retrieves cache info (the list of descriptors)  for x86 processor.
/ INPUT:
/   count - pointer to integer where to put descriptors count
/ OUTPUT:
/   If processor supports cache determination through cpuid output value is pointer to 
/   static array containing *count cache descriptors. Otherwise return value is NULL.
/ GLOBAL INPUT:
/   None.
/ SIDE EFFECTS:
/   None.
	.globl	x86_cpu_cache_info
	.type	x86_cpu_cache_info,@function
	.align	16
x86_cpu_cache_info:
	pushl	%ebp
	movl	%esp,%ebp
	pushl	%ebx

	xorl	%eax,%eax
	cpuid
	cmpl	$2,%eax
	jb	1f
	
/ lower 8 bits of %eax contains a value that indentifies the number of times
/ the cpuid has to be executed to obtain complete information about 
/ processor's cache configuration

	movl	$2, %eax	;/ get cache configuration
	cpuid
/ NOTE:	 for future processors cpuid instruction may need to be run 
/ more then once to get complete cache information.
/ Since this code can process one iteration only go to exit if more 
/ then one iteration required
	cmpb	$1, %al
	jne	1f

	.comm	cputype_cacheinfo_x86, 16, 4
	andl	$0xffffff00, %eax ;/ clear lower 8 bits
	movl	%eax, cputype_cacheinfo_x86
	movl	%ebx, cputype_cacheinfo_x86 + 4
	movl	%ecx, cputype_cacheinfo_x86 + 8
	movl	%edx, cputype_cacheinfo_x86 + 12

	movl	8(%ebp),%eax	;/ int* count
	movl	$16, (%eax)
	leal	cputype_cacheinfo_x86, %eax
	jmp	2f

1:	xorl	%ecx, %ecx
	movl	8(%ebp),%eax	;/ int* count
	movl	%ecx, (%eax)
	movl	%ecx, %eax

2:	popl	%ebx
	movl	%ebp,%esp
	popl	%ebp
	ret
	.size	x86_cpu_cache_info,.-x86_cpu_cache_info


/ FUNCTION:
/ const char* x86_cpu_vendor(void)
/ PURPOSE:
/   This routine retrieves vendor info for x86 processor.
/ INPUT:
/   None
/ OUTPUT:
/   Value is pointer to static array containing vendor name.
/ GLOBAL INPUT:
/   None.
/ SIDE EFFECTS:
/   None.

    .type	x86_cpu_vendor,@function
    .text
    .globl	x86_cpu_vendor
    .align	4
x86_cpu_vendor:
    pushl	%ebp
    movl	%esp,%ebp
    pushl	%ebx

    .comm   cpu_vendor, 16, 4
    xorl	%eax, %eax
    cpuid
    movl	%ebx, cpu_vendor
    movl	%edx, cpu_vendor + 4
    movl	%ecx, cpu_vendor + 8
    leal	cpu_vendor, %eax

    popl	%ebx
    movl	%ebp,%esp
    popl	%ebp
    ret

    .align  4
    .size   x86_cpu_vendor,.-x86_cpu_vendor


/ FUNCTION:
/ unsigned char* amd64_cpu_cache_info()
/ PURPOSE:
/   This routine retrieves cache info (the list of descriptors)  for AMD64 processor.
/ INPUT:
/   None
/ OUTPUT:
/   Is pointer to static array containing L1 and L2 cache descriptors.
/   L1:
/       cacheinfo[0] - Line Size (bytes)
/       cacheinfo[1] - Lines Per Tag
/       cacheinfo[2] - Associativity
/       cacheinfo[3] - Size (Kbytes)
/   L2:
/       cacheinfo[4] - Line Size (bytes)
/       cacheinfo[5] - Bits [0-3] Lines Per Tag
/                           [4-7] Associativity
/       cacheinfo[6-7] - Size (Kbytes)
/
/ GLOBAL INPUT:
/   None.
/ SIDE EFFECTS:
/   None.
    .globl	amd64_cpu_cache_info
    .type	amd64_cpu_cache_info,@function
    .align	4
amd64_cpu_cache_info:
    pushl	%ebp
    movl	%esp,%ebp
    pushl	%ebx

    .comm	cputype_cacheinfo_amd64, 8, 4
    movl	$0x80000005, %eax	;/ get L1 cache configuration
    cpuid
    movl	%ecx, cputype_cacheinfo_amd64
    movl	$0x80000006, %eax	;/ get L2 cache configuration
    cpuid
    movl	%ecx, cputype_cacheinfo_amd64 + 4
    leal	cputype_cacheinfo_amd64, %eax

    popl	%ebx
    movl	%ebp,%esp
    popl	%ebp
    ret
    .size	amd64_cpu_cache_info,.-amd64_cpu_cache_info

