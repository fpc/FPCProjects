/* Copyright (C) 1996 DJ Delorie, see COPYING.DJ for details */
/* Copyright (C) 1995 DJ Delorie, see COPYING.DJ for details */
#ifndef __dj_include_debug_dbgcom_h_
#define __dj_include_debug_dbgcom_h_

#ifdef __cplusplus
extern "C" {
#endif

#ifndef __dj_ENFORCE_ANSI_FREESTANDING

#ifndef __STRICT_ANSI__

#ifndef _POSIX_SOURCE

#include <setjmp.h>
#include <debug/tss.h>

typedef struct {
  unsigned long app_base;	/* linear base address of application */
  unsigned long dr[8];		/* debug registers, set when a_tss runs */
} ExternalDebuggerInfo;

extern ExternalDebuggerInfo edi;

/* structure of FPU state                       */
/* 14 bytes for FPU env                         */
/* plus 8*10 bytes from the FPU stack           */
/* r[8] is the array as defined in intel docs   */
/* st0 is r[top]                                */

#define FPU_TOP_MASK 0x3800
#define FPU_TOP_SHIFT 11

typedef struct {
  unsigned short control,res1,status,res2,tag,res3;
  unsigned long instofs;
  unsigned short instsel,opcode;
  unsigned long operandofs;
  unsigned short operandsel,res4;
  /* added to allow use of fnsave instruction
     works also for MMX instructions */
  long double r[8];
  long double st[8];
  /* this is for fpu stack so
     is_valid[1] means st1 is valid on stack
     its location is r[1+top] */
  char isvalid[8];
  char top,is_mmx;
} FPUEnvironment;

extern FPUEnvironment fpue;

void run_child(void);
int read_child(unsigned child_addr, void *buf, unsigned len);
int write_child(unsigned child_addr, void *buf, unsigned len);
void edi_init(jmp_buf start_state);
void cleanup_client(void);

#endif /* !_POSIX_SOURCE */
#endif /* !__STRICT_ANSI__ */
#endif /* !__dj_ENFORCE_ANSI_FREESTANDING */

#ifndef __dj_ENFORCE_FUNCTION_CALLS
#endif /* !__dj_ENFORCE_FUNCTION_CALLS */

#ifdef __cplusplus
}
#endif

#endif /* !__dj_include_debug_dbgcom_h_ */
