/* $Id$ */
/* modified for use with FPC and */
/* exception handling support by Pierre Muller */
/* Copyright (C) 1998 DJ Delorie, see COPYING.DJ for details */
/* Copyright (C) 1997 DJ Delorie, see COPYING.DJ for details */
/* Copyright (C) 1996 DJ Delorie, see COPYING.DJ for details */
/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
#include <libc/stubs.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dpmi.h>
#include <crt0.h>
#include <go32.h>
#include <signal.h>
#include <setjmp.h>
#include /* <debug/dbgcom.h> */ "dbgcom.h"
#include <sys/exceptn.h>
#include <stubinfo.h>
#include <sys/farptr.h>
#include <sys/fsext.h>
#include <io.h>

extern char __libdbg_ident_string[];
static char *id = __libdbg_ident_string;

#define MEM_HANDLE_COUNT	256
#define DESCRIPTOR_COUNT	128
#define DOS_DESCRIPTOR_COUNT	128
#define DPMI_EXCEPTION_COUNT     18

#define USE_FSEXT
#define CLOSE_UNREGISTERED_FILES

#ifdef FPC
#undef CLOSE_UNREGISTERED_FILES
#endif
/* debug splitted into 3 parts */
#define DEBUG_ALL_DBGCOM

#ifdef DEBUG_ALL_DBGCOM
/* general debug infos */
#define DEBUG_DBGCOM
/* files open/close infos */
#define DEBUG_DBGCOM_FILES
/* exceptions infos */
#define DEBUG_EXCEPTIONS
#endif /* DEBUG_ALL_DBGCOM */

long mem_handles[MEM_HANDLE_COUNT];
unsigned short descriptors[DESCRIPTOR_COUNT];
unsigned short dos_descriptors[DOS_DESCRIPTOR_COUNT];

/* these all need to be static because
  ss can be different from ds in dbgsig !! */
static int excep_stack[1000];
static int errcode,cs,eflags,eip,ss,esp,ret_cs,ret_eip;
static void *cur_pos;
static int child_exception_level;


/* ARGSUSED */
char **
__crt0_glob_function(char *foo)
{
  id = 0;
  return 0;
}

ExternalDebuggerInfo edi;
TSS a_tss;
FPUEnvironment fpue;

static jmp_buf jumper;

static int my_ds,my_cs,app_cs,app_exit_cs,app_ds;
static jmp_buf load_state;

static int nset, breakhandle[4];

static __dpmi_paddr our_handler[DPMI_EXCEPTION_COUNT],app_handler[DPMI_EXCEPTION_COUNT];

#ifdef DEBUG_EXCEPTIONS
typedef
  struct {
   short excp_cs;
   int   excp_eip;
   short excp_nb;
   } texcp_info;

static texcp_info excp_info[20];
static int excp_index;
static int excp_count;
static int redir_excp_count;
#endif

static void store_fpu_env (void)
{
  unsigned char tag;
  int i;
  int valid_nb = 0;
  long double d;
  asm("fstenv %0" : "=g" (fpue));
  tag = (fpue.status & FPU_TOP_MASK) >> FPU_TOP_SHIFT;
  for (i=0;i<8;i++)
    {
    /* tag is a array of 8 2 bits that contain info about FPU registers
       st(0) is register(tag) and st(1) is register (tag-1) ... */
     fpue.isvalid[i] = ((fpue.tag >> ((((8+tag)+i) & 7) << 1)) & 3) != 3;
    }
  /* to store the fpu registers we use fstpt and push them again after */
  /* this code assumes that the only valid regs are
     from top to base, is this allways true ?
     for me it is the very definition of a stack */
  for (i=0;i<8;i++)
    if (fpue.isvalid[i])
      {
       asm("fstpt %0" : "=g" (d));
       fpue.st[i]=d;
      }
  for (i=7;i>=0;i--)
    if (fpue.isvalid[i])
      {
       d=fpue.st[i];
       asm("fldt %0" :  : "g" (d));
      }
}

static void restore_fpu_env (void)
{
  int i;
  long double d;
  /* to store the fpu registers we use fstpt and push them again after */
  for (i=0;i<8;i++)
    if (fpue.isvalid[i])
      {
       asm("fstpt %0" : "=g" (d));
      }
  for (i=7;i>=0;i--)
    if (fpue.isvalid[i])
      {
       d=fpue.st[i];
       asm("fldt %0" :  : "g" (d));
      }
  asm("fldenv %0" :  : "g" (fpue));
}

static int _DPMIsetBreak(unsigned short sizetype, unsigned vaddr)
{
	int handle;

	asm volatile(						       "\n\
	    movw   %1,%%dx						\n\
	    movl   %2,%%ecx						\n\
	    movl   %%ecx,%%ebx						\n\
	    shrl   $16,%%ebx						\n\
	    movw   $0x0b00,%%ax						\n\
	    int    $0x31						\n\
	    jnc    3f							\n\
	    xorl   %%ebx,%%ebx						\n\
	    decl   %%ebx						\n\
	    jmp    1f							\n\
3:          movzwl %%bx,%%ebx						\n\
1:	    movl   %%ebx,%0						\n\
	    "
	    : "=g" (handle)			/* outputs */
	    : "g" (sizetype), "g"  (vaddr)	/* inputs */
	    : "ax", "bx", "cx", "dx"		/* regs used */
	);
	return handle;
}

static int _DPMIcancelBreak(int handle)
{
	unsigned state;

	asm volatile(						       "\n\
	    movl   %1,%%ebx						\n\
	    movw   $0x0b02,%%ax						\n\
	    int    $0x31						\n\
	    jnc    2f							\n\
	    xorl   %%eax,%%eax                                          \n\
2:	    andl   $1,%%eax						\n\
	    pushl  %%eax						\n\
	    movw   $0x0b01,%%ax						\n\
	    int    $0x31						\n\
	    popl   %0							\n\
	    "
	    : "=g" (state)			/* outputs */
	    : "g"  (handle)			/* inputs */
	    : "ax", "bx"			/* regs used */
	);
	return state;
}

/* Can't be static because called in asm below; -O3 inlines if static */
void _set_break_DPMI(void);
void _set_break_DPMI(void)
{
  int i;
  unsigned extract;
  unsigned short sizetype;
  unsigned long vbase;
  
  if(__dpmi_get_segment_base_address(__djgpp_app_DS, &vbase) == -1)
    return;
  extract = edi.dr[7] >> 16;
  nset = 0;

  for(i=0;i<4;i++)
    if( (edi.dr[7] >> (i*2))&3 ) {		/* enabled? */
      sizetype = (extract >> (i*4)) & 3;    /* extract the type */
      if(sizetype == 3) sizetype = 2;       /* convert for DPMI brain damage */
      sizetype = (sizetype << 8) + ((extract >> (i*4+2)) & 3) + 1; /* & size */
      breakhandle[i] = _DPMIsetBreak(sizetype, edi.dr[i]+vbase);
      if(breakhandle[i] == -1)
        printf("Error allocating DPMI breakpoint at address 0x%08lx\n",edi.dr[i]);
      else
        nset++;
    } else
      breakhandle[i] = -1;
  return;
}

/* Can't be static because called in asm below; -O3 inlines if static */
void _clear_break_DPMI(void);
void _clear_break_DPMI(void)
{
  int i,bt;

  if(!nset) {
    edi.dr[6] = 0;
    return;
  }

  bt = 0;
  for(i=3;i>=0;i--) {
    bt = bt << 1;                             /* Shift for next bit */
    if(breakhandle[i] != -1)
      bt |= _DPMIcancelBreak(breakhandle[i]);  /* Set low bit if active */
  }

  edi.dr[6] = bt;
}

static __dpmi_paddr old_i31,old_i21,user_i31,user_i21;
static int user_int_set = 0;
static __dpmi_paddr my_i9,user_i9,my_i8,user_i8;

static void hook_dpmi(void)
{
  int i;
  __dpmi_paddr new_int;
  extern void i21_hook(void),i31_hook(void);

  __dpmi_get_protected_mode_interrupt_vector(0x21, &old_i21);
  __dpmi_get_protected_mode_interrupt_vector(0x31, &old_i31);
  /* Save our current interrupt vectors for the keyboard and the timer */
  __dpmi_get_protected_mode_interrupt_vector(0x09, &my_i9);
  __dpmi_get_protected_mode_interrupt_vector(0x08, &my_i8);

  asm("mov %%cs,%0" : "=g" (new_int.selector) );
  new_int.offset32 = (unsigned long)i21_hook;
  __dpmi_set_protected_mode_interrupt_vector(0x21, &new_int);
  new_int.offset32 = (unsigned long)i31_hook;
  __dpmi_set_protected_mode_interrupt_vector(0x31, &new_int);

  /* If we have called already unhook_dpmi, the user interrupt
     vectors for the keyboard and the timer are valid. */
  if (user_int_set)
  {
    __dpmi_set_protected_mode_interrupt_vector(0x09, &user_i9);
    __dpmi_set_protected_mode_interrupt_vector(0x08, &user_i8);
    __dpmi_set_protected_mode_interrupt_vector(0x21, &user_i21);
    __dpmi_set_protected_mode_interrupt_vector(0x31, &user_i31);
  }
  for (i=0;i<DPMI_EXCEPTION_COUNT;i++)
    {
      __dpmi_get_processor_exception_handler_vector(i,&our_handler[i]);
      /*
        if (app_handler[i].offset32 && app_handler[i].selector)
        __dpmi_set_processor_exception_handler_vector(i,&app_handler[i]);
      */
    }
}

/* Set an exception handler */
/* stores it into app_handler if selector is app_cs  */

asm("\n\
        .text                                                           \n\
        .align  2,0x90                                                  \n\
_change_exception_handler:                                              \n\
        pushl   %eax                                                    \n\
        push    %es                                                     \n\
        push    %ds                                                     \n\
        .byte   0x2e                                                    \n\
        movw    _my_ds,%ax                                              \n\
        movw    %ax,%ds                                                 \n\
        movw    %ax,%es                                                 \n\
        movzbl %bl,%eax                                                 \n\
        imull  $8,%eax                                                  \n\
        addl  $_app_handler,%eax  /* only retain handlers */            \n\
        cmpw   _app_cs,%cx /* for the main app     */                   \n\
        jne    _not_in_current_app                                      \n\
        movl  %ecx,4(%eax)                                              \n\
        movl  %edx,(%eax)                                               \n\
_not_in_current_app:                                                    \n\
        subl  $_app_handler,%eax /* allways restore our handler */      \n\
        addl  $_our_handler,%eax                                         \n\
        movl  4(%eax),%ecx                                              \n\
        movl (%eax),%edx                                                \n\
        pop   %ds                                                       \n\
        pop   %es                                                       \n\
        popl  %eax                                                      \n\
        ret                                                             \n"
);

/* Get an exception handler */
/* (returns app_handler if set disabled because creates problems (PM))  */

asm("\n\
        .text                                                           \n\
        .align  2,0x90                                                  \n\
_get_exception_handler:                                                 \n\
        .byte 0x2e                                                      \n\
        ljmp _old_i31                                                   \n\
        ");

/* this creates problems ! WHY ??
asm("\n\
        .text                                                           \n\
        .align  2,0x90                                                  \n\
_get_exception_handler:                                                 \n\
        pushl   %eax                                                    \n\
        push    %es                                                     \n\
        push    %ds                                                     \n\
        .byte   0x2e                                                    \n\
        movw    _my_ds,%ax                                              \n\
        movw    %ax,%ds                                                 \n\
        movw    %ax,%es                                                 \n\
        movzbl %bl,%eax                                                 \n\
        imull  $8,%eax                                                  \n\
        addl  $_app_handler,%eax   only retain handlers             \n\
        cmpw  $0,4(%eax)           for the main app                 \n\
        je    _app_exception_not_set                                    \n\
        cmpl  $0,(%eax)                                                 \n\
        je    _app_exception_not_set                                    \n\
        movl  (%eax),%edx                                               \n\
        movw  4(%eax),%cx                                               \n\
        pop    %ds                                                      \n\
        pop    %es                                                      \n\
        popl   %eax                                                     \n\
        ret                                                             \n\
_app_exception_not_set:                                                 \n\
        pop   %ds                                                       \n\
        pop   %es                                                       \n\
        popl  %eax                                                      \n\
        .byte 0x2e                                                      \n\
        ljmp _old_i31                                                   \n\
        ret                                                             \n"
); */

/* Change a handle in the list: EAX is the old handle, EDX is the new */
/* for changing a value, we need our ds, because cs has no write access */
asm(						       			"\n\
	.text								\n\
	.align  2,0x90							\n\
_change_handle:								\n\
	pushl	%ecx							\n\
	xorl	%ecx,%ecx						\n\
CL1:									\n\
	.byte	0x2e 							\n\
	cmpl	%eax,_mem_handles(,%ecx,4)				\n\
	jne	CL2							\n\
	push	%ds							\n\
	.byte	0x2e							\n\
	movw	_my_ds,%ax						\n\
	movw	%ax,%ds							\n\
	movl	%edx,_mem_handles(,%ecx,4)				\n\
	pop	%ds							\n\
	popl	%ecx							\n\
	ret								\n\
CL2:									\n\
	incl	%ecx							\n\
	cmpl	$256,%ecx	/* MEM_HANDLE_COUNT */			\n\
	jl	CL1							\n\
	popl	%ecx							\n\
	ret								\n"
);

/* Change a descriptor in the list: AX is the old, DX is the new */
/* for changing a value, we need our ds, because cs has no write access */
asm(						       			"\n\
	.text								\n\
	.align  2,0x90							\n\
_change_descriptor:							\n\
	pushl	%ecx							\n\
	pushl	%eax							\n\
	xorl	%ecx,%ecx						\n\
CL3:									\n\
	.byte	0x2e 							\n\
	cmpw	%ax,_descriptors(,%ecx,2)				\n\
	jne	CL4							\n\
	push	%ds							\n\
	.byte	0x2e							\n\
	movw	_my_ds,%ax						\n\
	movw	%ax,%ds							\n\
	movw	%dx,_descriptors(,%ecx,2)				\n\
	pop	%ds							\n\
	popl	%eax							\n\
	popl	%ecx							\n\
	ret								\n\
CL4:									\n\
	incl	%ecx							\n\
	cmpl	$128,%ecx	/* DESCRIPTOR_COUNT */			\n\
	jl	CL3							\n\
	popl	%eax							\n\
	popl	%ecx							\n\
	ret								\n"
);

/* Add descriptors to the list: AX is the first, CX is the count */
asm(									"\n\
	.text								\n\
	.align  2,0x90							\n\
_add_descriptors:							\n\
	pushl	%edx							\n\
	pushl	%ecx							\n\
	pushl	%ebx							\n\
	pushl	%eax							\n\
	movw	$0x0003,%ax						\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	movw	%ax,%bx							\n\
	popl	%eax							\n\
	pushl	%eax							\n\
	movw	%ax,%dx							\n\
	xorw	%ax,%ax							\n\
CL5:									\n\
	call	_change_descriptor					\n\
	addw	%bx,%dx							\n\
	loop	CL5							\n\
	popl	%eax							\n\
	popl	%ebx							\n\
	popl	%ecx							\n\
	popl	%edx							\n\
	ret								\n"
);

/* Change a dos_descriptor in the list: AX is the old, DX is the new */
/* for changing a value, we need our ds, because cs has no write access */
asm(						       			"\n\
	.text								\n\
	.align  2,0x90							\n\
_change_dos_descriptor:							\n\
	pushl	%eax							\n\
	pushl	%ecx							\n\
	xorl	%ecx,%ecx						\n\
CL6:									\n\
	.byte	0x2e 							\n\
	cmpw	%ax,_dos_descriptors(,%ecx,2)				\n\
	jne	CL7							\n\
	push	%ds							\n\
	.byte	0x2e							\n\
	movw	_my_ds,%ax						\n\
	movw	%ax,%ds							\n\
	movw	%dx,_dos_descriptors(,%ecx,2)				\n\
	pop	%ds							\n\
	popl	%ecx							\n\
	popl	%eax							\n\
	ret								\n\
CL7:									\n\
	incl	%ecx							\n\
	cmpl	$128,%ecx	/* DOS_DESCRIPTOR_COUNT */		\n\
	jl	CL6							\n\
	popl	%ecx							\n\
	popl	%eax							\n\
	ret								\n"
);

/* BUGS: We ignore the exception handlers for the child process, so signals
   do not work.  We also disable the hooking of the numeric coprocessor
   HW interrupt. */

/* Watch set selector base, if it is __djgpp_app_DS then reset breakpoints */

/* Watch the following DPMI-functions: (added by RH)  (some by PM)

  0x0000   : __dpmi_allocate_ldt_descriptors
  0x0001   : __dpmi_free_ldt_descriptor
  0x0007   : __dpmi_set_selector_base_address
             hooked because the hardware breakpoints need to be reset
             if we change the base address of __djgpp_app_DS
  0x000A   : __dpmi_create_alias_descriptor
  
  0x0100   : __dpmi_allocate_dos_memory
  0x0101   : __dpmi_free_dos_memory
  
  0x0202   : __dpmi_get_processor_exception_handler_vector
  0x0203   : __dpmi_set_processor_exception_handler_vector
  0x0205   : __dpmi_set_protected_mode_interrupt
             (int 0x09 and 0x75 rejected)
             Ctrl-C will thus create a SIGINT in the top level
             program but cont will directly return to
             the code before interruption no matter at which level)
  0x0210   : __dpmi_get_extended_exception_handler_vector_pm
  0x0212   : __dpmi_set_extended_exception_handler_vector_pm
  
  0x0501   : __dpmi_allocate_memory
  0x0501   : __dpmi_free_memory
  0x0503   : __dpmi_resize_memory
  
*/ 
asm(		        		       			        "\n\
	.text								\n\
	.align  2,0x90							\n\
        .globl  _dbgcom_hook_i31                                        \n\
_dbgcom_hook_i31:                                                       \n\
_i31_hook:								\n\
	cmpw	$0x0000,%ax						\n\
	je	Lc31_alloc_descriptors					\n\
	cmpw	$0x0001,%ax						\n\
	je	Lc31_free_descriptor					\n\
	cmpw	$0x0007,%ax						\n\
	je	Lc31_set_selector_base_address				\n\
	cmpw	$0x000A,%ax						\n\
	je	Lc31_create_alias_descriptor				\n\
	cmpw	$0x0100,%ax						\n\
	je	Lc31_allocate_dos_memory				\n\
	cmpw	$0x0101,%ax						\n\
	je	Lc31_free_dos_memory					\n\
        cmpw    $0x0202,%ax                                             \n\
        je      _get_exception_handler                                  \n\
        cmpw    $0x0203,%ax                                             \n\
        je      Lc31_set_exception_handler                              \n\
	cmpw	$0x0205,%ax						\n\
	je	Lc31_set_protected_mode_interrupt			\n\
        cmpw    $0x0210,%ax                                             \n\
        je      _get_exception_handler                                  \n\
        cmpw    $0x0212,%ax                                             \n\
        je      Lc31_set_exception_handler                              \n\
	cmpw	$0x0501,%ax						\n\
	je	Lc31_alloc_mem						\n\
	cmpw	$0x0502,%ax						\n\
	je	Lc31_free_mem						\n\
	cmpw	$0x0503,%ax						\n\
	je	Lc31_resize_mem						\n\
L_jmp_to_old_i31:                                                       \n\
        .byte	0x2e							\n\
	ljmp	_old_i31						\n\
Lc31_set_flags_and_iret:                                                \n\
        pushl	%eax				                        \n\
	pushf								\n\
	popl	%eax		/* store the right flags for iret */	\n\
	movl	%eax,12(%esp)						\n\
	popl	%eax							\n\
Lc31_iret:                                                              \n\
        iret								\n\
Lc31_set_selector_base_address:                                         \n\
	.byte	0x2e							\n\
	cmpw	___djgpp_app_DS,%bx					\n\
	jne	L_jmp_to_old_i31					\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	call	___djgpp_save_interrupt_regs				\n\
	call	__clear_break_DPMI					\n\
	call	__set_break_DPMI					\n\
	movl	___djgpp_exception_state_ptr,%eax			\n\
	pushl	(%eax)							\n\
	pushl	%eax							\n\
	call	_longjmp						\n\
Lc31_set_protected_mode_interrupt:                                      \n\
	cmpb	$0x75,%bl						\n\
	je	Lc31_iret						\n\
	cmpb	$0x09,%bl                                             	\n\
	je	Lc31_iret						\n\
	jmp	L_jmp_to_old_i31					\n\
Lc31_alloc_mem:								\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	jc	Lc31_set_flags_and_iret					\n\
	pushf								\n\
	pushl	%edx							\n\
	pushw	%si							\n\
	pushw	%di							\n\
	popl	%edx							\n\
	xorl	%eax,%eax						\n\
	call	_change_handle						\n\
	popl	%edx							\n\
	popf								\n\
	clc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_free_mem:								\n\
	pushw	%si							\n\
	pushw	%di							\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	jc	Lc31_resize_mem_error					\n\
	popl	%eax							\n\
	xorl	%edx,%edx						\n\
	call	_change_handle						\n\
	xorl	%eax,%eax						\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_resize_mem:							\n\
	pushw	%si							\n\
	pushw	%di							\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	jnc	Lc31_resize_mem_ok					\n\
Lc31_resize_mem_error:							\n\
	addl	$4,%esp							\n\
	stc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_resize_mem_ok:							\n\
	popl	%eax							\n\
	pushw	%si							\n\
	pushw	%di							\n\
	popl	%edx							\n\
	call	_change_handle						\n\
	xorl	%eax,%eax						\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_alloc_descriptors:							\n\
	pushl	%ecx							\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	popl	%ecx							\n\
	jc	Lc31_set_flags_and_iret					\n\
	call	_add_descriptors					\n\
	clc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_free_descriptor:							\n\
	pushl	%ebx							\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	popl	%eax							\n\
	jc	Lc31_set_flags_and_iret					\n\
	xorw	%dx,%dx							\n\
	call	_change_descriptor					\n\
	clc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_create_alias_descriptor:						\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	jc	Lc31_set_flags_and_iret					\n\
	pushl	%eax							\n\
	movw	%ax,%dx							\n\
	xorw	%ax,%ax							\n\
	call	_change_descriptor					\n\
	popl	%eax							\n\
	clc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_allocate_dos_memory:						\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	jc	Lc31_set_flags_and_iret					\n\
	pushl	%eax							\n\
	xorl	%eax,%eax						\n\
	call	_change_dos_descriptor					\n\
	popl	%eax							\n\
	clc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_free_dos_memory:							\n\
	pushl	%edx							\n\
	pushf								\n\
	.byte	0x2e							\n\
	lcall	_old_i31						\n\
	popl	%eax							\n\
	jc	Lc31_set_flags_and_iret					\n\
	xorw	%dx,%dx							\n\
	call	_change_dos_descriptor					\n\
	clc								\n\
	jmp	Lc31_set_flags_and_iret					\n\
Lc31_set_exception_handler:                                             \n\
        pushl  %eax                                                     \n\
        pushl  %ebx                                                     \n\
        pushl  %ecx                                                     \n\
        pushl  %edx                                                     \n\
        pushf                                                           \n\
        .byte  0x2e                                                     \n\
        lcall   _old_i31                                                \n\
        jc   Lc31_set_flags_and_iret                                    \n\
        popl   %edx                                                     \n\
        popl   %ecx                                                     \n\
        popl   %ebx                                                     \n\
        popl   %eax                                                     \n\
        call   _change_exception_handler                                \n\
        pushf                                                           \n\
        .byte  0x2e                                                     \n\
        lcall   _old_i31                                                \n\
        jmp Lc31_set_flags_and_iret                                     \n\
        .align  2,0x90                                                  \n\
        .globl  _dbgcom_hook_i21                                        \n\
_dbgcom_hook_i21:                                                       \n\
_i21_hook:								\n\
	cmpb	$0x4c,%ah						\n\
	je	Lc21							\n\
Lc21_jmp_to_old:                                                        \n\
        .byte	0x2e							\n\
	ljmp	_old_i21						\n\
Lc21:	push	%eax							\n\
	movl	8(%esp),%eax						\n\
	cs								\n\
	cmpw	_app_exit_cs,%ax					\n\
	pop	%eax							\n\
	jne	Lc21_jmp_to_old						\n\
	call	___djgpp_save_interrupt_regs				\n\
	movl	___djgpp_exception_state_ptr,%esi			\n\
	movl	$0x21,56(%esi)						\n\
	movl	$_load_state,%edi					\n\
	movl	$43,%ecx						\n\
	rep								\n\
	movsl								\n\
	pushl	$1							\n\
	pushl	$_jumper						\n\
	call	_longjmp						\n\
	"
	);

/*	movw	%cs:__go32_info_block+26, %fs				\n\
	.byte	0x64							\n\
	movw	$0x7021,0xb0f00						\n\ */

/* simple code to return from an exception */
asm (  ".text
       .align 2,0x90
_exception_return:       /* remove errorcode from stack */
       addl $4,%esp
       lret
    ");
    
static void unhook_dpmi(void)
{
  int i;
  /* save app i31 and i21 if changed */
  __dpmi_get_protected_mode_interrupt_vector(0x31, &user_i31);
  __dpmi_get_protected_mode_interrupt_vector(0x21, &user_i21);
  /* restore i31 and i21 */
  __dpmi_set_protected_mode_interrupt_vector(0x31, &old_i31);
  __dpmi_set_protected_mode_interrupt_vector(0x21, &old_i21);

  /* Save the interrupt vectors for the keyboard and the the
     time, because the debuggee may have changed it. */
  __dpmi_get_protected_mode_interrupt_vector(0x09, &user_i9);
  __dpmi_get_protected_mode_interrupt_vector(0x08, &user_i8);

  /* And remember it for hook_dpmi */
  user_int_set = 1;

  /* Now restore our interrupt vectors */
  __dpmi_set_protected_mode_interrupt_vector(0x09, &my_i9);
  __dpmi_set_protected_mode_interrupt_vector(0x08, &my_i8);
  for (i=0;i<DPMI_EXCEPTION_COUNT;i++)
    {
      if (i!=2)
        __dpmi_set_processor_exception_handler_vector(i,&our_handler[i]);
    }
}

static void dbgsig(int sig)
{
#ifdef DEBUG_EXCEPTIONS
    excp_info[excp_index].excp_eip=__djgpp_exception_state->__eip;
    excp_info[excp_index].excp_cs=__djgpp_exception_state->__cs;
    excp_info[excp_index].excp_nb=__djgpp_exception_state->__signum;
    excp_index++;
    excp_count++;
    if (excp_index==20)
      excp_index=0;
#endif
  if(__djgpp_exception_state->__cs == app_cs /* || sig == SIGTRAP */)
  {
    *load_state = *__djgpp_exception_state;     /* exception was in other process */
    longjmp(jumper, 1);
  }
  else
  {
    extern int invalid_sel_addr(short sel, unsigned a, unsigned len, char for_write);
    
    int signum =  __djgpp_exception_state->__signum;
  if (
     (app_handler[signum].offset32) &&
     (app_handler[signum].selector) &&
      !invalid_sel_addr(app_handler[signum].selector,
        app_handler[signum].offset32,1,0) &&
     ((app_handler[signum].offset32 !=
      our_handler[signum].offset32) ||
     (app_handler[signum].selector !=
      our_handler[signum].selector)))
    {
    extern void exception_return(void);
#ifdef DEBUG_EXCEPTIONS
    redir_excp_count++;
#endif
    *load_state = *__djgpp_exception_state;     /* exception was in other process */
    eip = load_state->__eip;
    cs  = load_state->__cs;
    esp = load_state->__esp;
    /* reset the debug trace bit */
    /* we should only do this under certain specific conditions
       but which ones ?? */
    eflags = load_state->__eflags;
    /* if (signum!=1)
     eflags &= 0xfffffeff; */
    /* we don't want to step inside the exception_table code */
    load_state->__eflags &= 0xfffffeff;
    ss  = load_state->__ss;
    errcode = load_state->__sigmask;
    load_state->__eip=app_handler[signum].offset32;
    load_state->__cs=app_handler[signum].selector;
    /* use our own exception stack */
    child_exception_level++;
    memset(&excep_stack,0xAB,sizeof(excep_stack));
    cur_pos = &excep_stack[1000-40];
    cur_pos -= 8*4;
    load_state->__ss = my_ds;
    load_state->__esp= (int) cur_pos;
    /* where to return */
    ret_cs = my_cs;
    ret_eip = (int) &exception_return;
    memcpy(cur_pos,&ret_eip,4);
    cur_pos+=4;
    memcpy(cur_pos,&ret_cs,4);
    cur_pos+=4;
    memcpy(cur_pos,&errcode,4);
    cur_pos+=4;
    memcpy(cur_pos,&eip,4);
    cur_pos+=4;
    memcpy(cur_pos,&cs,4);
    cur_pos+=4;
    memcpy(cur_pos,&eflags,4);
    cur_pos+=4;
    memcpy(cur_pos,&esp,4);
    cur_pos+=4;
    memcpy(cur_pos,&ss,4);
    cur_pos+=4;
    longjmp(load_state, load_state->__eax);
    }
   /*  else
     {
      *load_state = *__djgpp_exception_state;
      longjmp(jumper, 1);
     }  */
  }
}

void run_child(void)
{
  load_state->__cs = a_tss.tss_cs;
  load_state->__ss = a_tss.tss_ss;
  load_state->__ds = a_tss.tss_ds;
  load_state->__es = a_tss.tss_es;
  load_state->__fs = a_tss.tss_fs;
  load_state->__gs = a_tss.tss_gs;
  load_state->__eip = a_tss.tss_eip;
  load_state->__eflags = a_tss.tss_eflags;
  load_state->__eax = a_tss.tss_eax;
  load_state->__ebx = a_tss.tss_ebx;
  load_state->__ecx = a_tss.tss_ecx;
  load_state->__edx = a_tss.tss_edx;
  load_state->__esp = a_tss.tss_esp;
  load_state->__ebp = a_tss.tss_ebp;
  load_state->__esi = a_tss.tss_esi;
  load_state->__edi = a_tss.tss_edi;
  if(!setjmp(jumper)){
    /* jump to tss */
    _set_break_DPMI();
    hook_dpmi();
    restore_fpu_env();
    longjmp(load_state, load_state->__eax);
    /* we never return here, execption routine will longjump */
  }
  /* exception routine:  save state, copy to tss, return */
  a_tss.tss_cs = load_state->__cs;
  a_tss.tss_ss = load_state->__ss;
  a_tss.tss_ds = load_state->__ds;
  a_tss.tss_es = load_state->__es;
  a_tss.tss_fs = load_state->__fs;
  a_tss.tss_gs = load_state->__gs;
  a_tss.tss_eip = load_state->__eip;
  a_tss.tss_esp = load_state->__esp;
  a_tss.tss_eflags = load_state->__eflags;
  a_tss.tss_eax = load_state->__eax;
  a_tss.tss_ebx = load_state->__ebx;
  a_tss.tss_ecx = load_state->__ecx;
  a_tss.tss_edx = load_state->__edx;
  a_tss.tss_esi = load_state->__esi;
  a_tss.tss_edi = load_state->__edi;
  a_tss.tss_ebp = load_state->__ebp;
  a_tss.tss_irqn = load_state->__signum;
  a_tss.tss_error = load_state->__sigmask;
  store_fpu_env();
  unhook_dpmi();
  _clear_break_DPMI();
}

static int invalid_addr(unsigned a, unsigned len)
{
  /* Here we assume expand up writable code.  We could check the rights to
     be sure, but that's a waste unless *_child routines fixed to know about
     different selectors. */

  unsigned limit;
  limit = __dpmi_get_segment_limit(__djgpp_app_DS);
  if(4096 <= a             /* First page is used for NULL pointer detection. */
  && a <= limit            /* To guard against limit < len. */
  && a - 1 <= limit - len  /* To guard against limit <= a + len - 1. */
     )
    return 0;
/*  printf("Invalid access to child, address %#x length %#x  limit: %#x\n", a, len, limit);
  if (can_longjmp)
    longjmp(debugger_jmpbuf, 1); */
  return 1;
}

int read_child(unsigned child_addr, void *buf, unsigned len)
{
  if (invalid_addr(child_addr, len))
    return 1;
  movedata(__djgpp_app_DS, child_addr, my_ds, (int)buf, len);
  return 0;
}

int write_child(unsigned child_addr, void *buf, unsigned len)
{
  if (invalid_addr(child_addr, len))
    return 1;
  movedata(my_ds, (int)buf, __djgpp_app_DS, child_addr, len);
  return 0;
}

int invalid_sel_addr(short sel, unsigned a, unsigned len, char for_write)
{
  /* Here we assume expand up writable code.  We could check the rights to
     be sure, but that's a waste unless *_child routines fixed to know about
     different selectors. */

  unsigned limit;
  char read_allowed = 0;
  char write_allowed = 0;
  
  asm("
      movw  %2,%%ax
      verr  %%ax
      jnz   .Ldoes_not_has_read_right
      movb  $1,%0
.Ldoes_not_has_read_right:
      verrw %%ax
      jnz   .Ldoes_not_has_write_right
      movb  $1,%1
.Ldoes_not_has_write_right: "
     : "=g" (read_allowed), "=g" (write_allowed)
     : "g" (sel)
     );
  if (for_write)
    {
     if (!write_allowed)
       return 1;
    }
  else
    if (!read_allowed)
      return 1;
      
  limit = __dpmi_get_segment_limit(sel);
  /* some selectors don't have zero page protection
     like the protected interrupt stack */
  if(/*a >= 4096 && */ (a+len-1) <= limit)
    return 0;
/*  printf("Invalid access to child, address %#x length %#x  limit: %#x\n", a, len, limit);
  if (can_longjmp)
    longjmp(debugger_jmpbuf, 1); */
  return 1;
}

int read_sel_addr(unsigned child_addr, void *buf, unsigned len, unsigned sel)
{
  /* first clear memory */
  memcpy(buf,0,len);
  if (invalid_sel_addr(sel, child_addr, len, 0))
    return 1;
  movedata(sel, child_addr, my_ds, (int)buf, len);
  return 0;
}

int write_sel_addr(unsigned sel, unsigned child_addr, void *buf, unsigned len)
{
  if (invalid_sel_addr(sel, child_addr, len, 1))
    return 1;
  movedata(my_ds, (int)buf, sel, child_addr, len);
  return 0;
}

static _GO32_StubInfo si;

static void (*oldTRAP)(int);
static void (*oldSEGV)(int);
static void (*oldFPE)(int);
static void (*oldINT)(int);

void edi_init(jmp_buf start_state)
{
  int i;
  my_ds = 0;
  asm("mov %%ds,%0" : "=g" (my_ds) );
  my_cs = 0;
  asm("mov %%cs,%0" : "=g" (my_cs) );

  for (i=0;i<DPMI_EXCEPTION_COUNT;i++)
    {
      app_handler[i].offset32 = 0;
      app_handler[i].selector = 0;
    }
  *load_state = *start_state;
  a_tss.tss_cs = load_state->__cs;
  a_tss.tss_ss = load_state->__ss;
  a_tss.tss_ds = load_state->__ds;
  a_tss.tss_es = load_state->__es;
  a_tss.tss_fs = load_state->__fs;
  a_tss.tss_gs = load_state->__gs;
  a_tss.tss_eip = load_state->__eip;
  a_tss.tss_esp = load_state->__esp;
  a_tss.tss_eflags = load_state->__eflags;

  __djgpp_app_DS = a_tss.tss_ds;
  app_cs = a_tss.tss_cs;
  edi.app_base = 0;
  memset(&fpue,0,sizeof(fpue));
  /* Save all the changed signal handlers */
  oldTRAP = signal(SIGTRAP, dbgsig);
  oldSEGV = signal(SIGSEGV, dbgsig);
  oldFPE = signal(SIGFPE, dbgsig);
  oldINT = signal(SIGINT, dbgsig);
  movedata(a_tss.tss_fs,0,my_ds,(unsigned)&si,sizeof(si));
  memset(mem_handles,0,sizeof(mem_handles));
  mem_handles[0] = si.memory_handle;
  memset(descriptors,0,sizeof(descriptors));
  descriptors[0] = si.cs_selector;
  descriptors[1] = si.ds_selector;
  descriptors[2] = __djgpp_app_DS;
  descriptors[3] = app_cs; 
  app_exit_cs=si.cs_selector;
  memset(dos_descriptors,0,sizeof(dos_descriptors));
  dos_descriptors[0] = _farpeekw(si.psp_selector,0x2c);
  dos_descriptors[1] = si.psp_selector; 
}

static void close_handles(void); /* Forward declaration */

void cleanup_client(void)
{
  int i;

  /* restore __djgpp_app_DS for Ctrl-C !! */
  __djgpp_app_DS = __djgpp_our_DS;
#ifdef DEBUG_EXCEPTIONS
    fprintf(stderr,"excp_count = %d\n",excp_count);
    fprintf(stderr,"redir_excp_count = %d\n",redir_excp_count);
    fprintf(stderr,"excp_index = %d\n",excp_index);
    fprintf(stderr,"app_cs %04x\tapp_ds %04x\n",app_cs,app_ds);
    fprintf(stderr,"my_cs %04x\tmy_ds %04x\n",my_cs,my_ds);
    for (i=0;i<excp_count;i++)
      {
       fprintf(stderr," excep %04x:%08x\tsignal %08x\n",
         excp_info[i].excp_cs,
         excp_info[i].excp_eip,
         excp_info[i].excp_nb);
         excp_info[i].excp_eip=0;
         excp_info[i].excp_cs=0;
         excp_info[i].excp_nb=0;
      }
    excp_count=0;
    redir_excp_count=0;
    excp_index=0;
  for (i=0;i<DPMI_EXCEPTION_COUNT;i++)
    {
      fprintf(stderr," app %d handler = %04x:%08lx\n",
              i,app_handler[i].selector,app_handler[i].offset32);
    }
  for (i=0;i<DESCRIPTOR_COUNT;i++)
  {
    if (descriptors[i])
    {
      fprintf(stderr,"used descriptor: %08x\n",descriptors[i]);
    }
  }
#endif
  for (i=0;i<DPMI_EXCEPTION_COUNT;i++)
    {
      app_handler[i].offset32 = 0;
      app_handler[i].selector = 0;
    }
  /* Set the flag, that the user interrupt vectors are no longer valid */
  user_int_set = 0;

  memset(&fpue,0,sizeof(fpue));
  /* Close all handles, which may be left open */
  close_handles();
  for (i=0;i<DOS_DESCRIPTOR_COUNT;i++)
  {
    if (dos_descriptors[i])
    {
#ifdef DEBUG_DBGCOM
      fprintf(stderr,"free dos memory: %08x\n",dos_descriptors[i]);
#endif
      __dpmi_free_dos_memory(dos_descriptors[i]);
    }
  }
  for (i=0;i<MEM_HANDLE_COUNT;i++)
  {
    if (mem_handles[i])
    {
#ifdef DEBUG_DBGCOM
      fprintf(stderr,"free mem : %08lx\n",mem_handles[i]);
#endif
      __dpmi_free_memory(mem_handles[i]);
    }
  }
  for (i=0;i<DESCRIPTOR_COUNT;i++)
  {
    if (descriptors[i])
    {
#ifdef DEBUG_DBGCOM
      fprintf(stderr,"free descriptor: %08x\n",descriptors[i]);
#endif
      __dpmi_free_ldt_descriptor(descriptors[i]);
    }
  }
  /* Restore all changed signal handlers */
  signal(SIGTRAP, oldTRAP);
  signal(SIGSEGV, oldSEGV);
  signal(SIGFPE, oldFPE);
  signal(SIGINT, oldINT);
}

#ifndef USE_FSEXT

static void close_handles(void)
{
}

#else /* USE_FSEXT */
/*
   Now the FSEXT function for watching files being opened. This is needed,
   because the debuggee can open files which are not closed and if you
   do this multiple times, the limit of max opened files is reached.

   The watching is done by the FSEXT function by hooking the _open(),
   _creat() and _close() calls from the libc functions. The only things
   which are added is recording files which are opened (and closed) by
   the debugger. When cleanup_client() is called, this list is compared
   with actual open files and every file, which was not seen by dbg_fsext()
   is closed.

   This technique does not work correctly when the debugger uses the lowest
   routines for opening/creating/closing files which are
   _dos_open(), _dos_creat(), _dos_creatnew() and _dos_close().
*/

static unsigned char handles[256];
static int in_dbg_fsext = 0;

static void close_handles(void)
{
  __dpmi_regs r;
  int psp_la;
  int jft_ofs;
  int jft_count;
  int handle;

  /* Get our PSP address.  */
  r.x.ax = 0x6200;
  __dpmi_int (0x21, &r);
  psp_la = ( (int)r.x.bx ) << 4;

  /* Get the offset of the JFT table by (seg << 4) + offset */
  jft_ofs = (_farpeekw(_dos_ds, psp_la + 0x36) << 4) +
            _farpeekw(_dos_ds, psp_la + 0x34);

  /* Number of used entries in the JFT table */
  jft_count = _farpeekw(_dos_ds, psp_la + 0x32);

  /* Disable the fsext function */
  in_dbg_fsext++;

  for (handle=0;handle<jft_count;handle++)
  {
    if (_farpeekb(_dos_ds,jft_ofs++) != 0xff /* it is an opened handle */
        && handles[handle] == 0xff /* but not recorded by the fsext function */
       )
    { /* it was opened by the debuggee */
#ifdef CLOSE_UNREGISTERED_FILES
#ifdef DEBUG_DBGCOM_FILES
      fprintf(stderr,"closing %d\n",handle);
#endif
      _close(handle);
#else  /* not CLOSE_UNREGISTERED_FILES */
#ifdef DEBUG_DBGCOM_FILES
      fprintf(stderr,"unknown open file %d\n",handle);
#endif
#endif /* CLOSE_UNREGISTERED_FILES */
    }
  }

  /* Enable the fsext function */
  in_dbg_fsext--;
}


static int dbg_fsext(__FSEXT_Fnumber _function_number,
                      int *_rv, va_list _args)
{
  int attrib,oflag,retval = 0,handle;
  const char *filename;
  /* We are called from this function */
  if (in_dbg_fsext) return 0;
  switch (_function_number)
  {
    default:
      return 0;
    case __FSEXT_creat:
      filename = va_arg(_args,const char *);
      attrib = va_arg(_args,int);
      in_dbg_fsext++;
      retval = _creat(filename,attrib);
#ifdef DEBUG_DBGCOM_FILES
      fprintf(stderr,"_creat(%s) => %d\n",filename,retval);
#endif
      in_dbg_fsext--;
      if (retval != -1)
      {
        handles[retval] = retval;
        __FSEXT_set_function(retval,dbg_fsext);
      }
      break;
    case __FSEXT_open:
      filename = va_arg(_args,const char *);
      oflag = va_arg(_args,int);
      in_dbg_fsext++;
      retval = _open(filename,oflag);
#ifdef DEBUG_DBGCOM_FILES
      fprintf(stderr,"_open(%s) => %d\n",filename,retval);
#endif
      in_dbg_fsext--;
      if (retval != -1)
      {
        handles[retval] = retval;
        __FSEXT_set_function(retval,dbg_fsext);
      }
      break;
    case __FSEXT_close:
      handle = va_arg(_args,int);
      in_dbg_fsext++;
#ifdef DEBUG_DBGCOM_FILES
      fprintf(stderr,"_close(%d)\n",handle);
#endif
      retval = _close(handle);
      in_dbg_fsext--;
      if (retval == 0)
      {
        handles[handle] = 0xff;
        __FSEXT_set_function(handle,NULL);
      }
      break;
  }
  *_rv = retval;
  return 1;
}

/* With attribute constructor to be called automaticaly before main */

static void __attribute__((__constructor__))
_init_dbg_fsext(void)
{
  __dpmi_regs r;
  int psp_la;
  int jft_ofs;
  int jft_count;

  /* Get our PSP address.  */
  r.x.ax = 0x6200;
  __dpmi_int (0x21, &r);
  psp_la = ( (int)r.x.bx ) << 4;

  /* Get the offset of the JFT table by (seg << 4) + offset */
  jft_ofs = (_farpeekw(_dos_ds, psp_la + 0x36) << 4) +
            _farpeekw(_dos_ds, psp_la + 0x34);

  /* Number of used entries in the JFT table */
  jft_count = _farpeekw(_dos_ds, psp_la + 0x32);

  /* Add the handler for opening/creating files */
  __FSEXT_add_open_handler(dbg_fsext);

  /* Initialize all the handles to 0xff */
  memset(handles,0xff,sizeof(handles));

  /* Get a copy of all already opened handles */
  movedata(_dos_ds,jft_ofs,_my_ds(),(int)handles,jft_count);

  /* enable the fsext function */
  in_dbg_fsext = 0;
}
#endif /* def USE_FSEXT */

/* 
  $Log$
  Revision 1.4  1998/12/23 10:55:29  pierre
    + added FPU support for go32v2
    * now can both handle extended with ten or twelve bytes
    + added fpue structure for storing FPU environment and stack
      (in dbgcom.h available for all debuggers

  Revision 1.3  1998/12/21 11:03:01  pierre
   * problems of FSEXT_dbg solved
 
*/