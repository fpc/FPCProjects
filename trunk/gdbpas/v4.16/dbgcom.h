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

/* structure of FPU env */
/* plus space from the FPU stack */

#define FPU_TOP_MASK 0x3800
#define FPU_TOP_SHIFT 11

typedef struct {
  unsigned short control,res1,status,res2,tag,res3;
  unsigned long instofs;
  unsigned short instsel,opcode;
  unsigned long operandofs;
  unsigned short operandsel,res4;
  char isvalid[8];
  long double st[8];
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
