/* Target-vector operations for controlling go32 processes, for GDB.
   Copyright 1994 Free Software Foundation, Inc.
   Contributed by DJ Delorie.
   Modified for V2 by CW Sandmann.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <fcntl.h>

#include "defs.h"
#include "frame.h"  /* required by inferior.h */
#include "inferior.h"
#include "target.h"
#include "wait.h"
#include "gdbcore.h"
#include "command.h"

extern char **environ;

/* Forward declaration */
extern struct target_ops go32_ops;

/* ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
   º  Go32's external debugger interface routines			º
   ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼ */

#define SOME_PID 42

static int prog_has_started = 0;

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <debug/v2load.h>
#include <debug/dbgcom.h>

void cleanup_client();

#define r_ofs(x) ((int)(&(((TSS *)0)->x)))
static struct {
  int tss_ofs;
  int size;
} regno_mapping[] = {
  r_ofs(tss_eax), 4,
  r_ofs(tss_ecx), 4,
  r_ofs(tss_edx), 4,
  r_ofs(tss_ebx), 4,
  r_ofs(tss_esp), 4,
  r_ofs(tss_ebp), 4,
  r_ofs(tss_esi), 4,
  r_ofs(tss_edi), 4,
  r_ofs(tss_eip), 4,
  r_ofs(tss_eflags), 4,
  r_ofs(tss_cs), 2,
  r_ofs(tss_ss), 2,
  r_ofs(tss_ds), 2,
  r_ofs(tss_es), 2,
  r_ofs(tss_fs), 2,
  r_ofs(tss_gs), 2
};

static struct {
  int go32_sig;
  int gdb_sig;
} sig_map[] = {
  0, TARGET_SIGNAL_FPE,
  1, TARGET_SIGNAL_TRAP,
  2, TARGET_SIGNAL_UNKNOWN,
  3, TARGET_SIGNAL_TRAP,
  4, TARGET_SIGNAL_FPE,
  5, TARGET_SIGNAL_SEGV,
  6, TARGET_SIGNAL_ILL,
  7, TARGET_SIGNAL_FPE,
  8, TARGET_SIGNAL_SEGV,
  9, TARGET_SIGNAL_SEGV,
  10, TARGET_SIGNAL_BUS,
  11, TARGET_SIGNAL_SEGV,
  12, TARGET_SIGNAL_SEGV,
  13, TARGET_SIGNAL_SEGV,
  14, TARGET_SIGNAL_SEGV,
  16, TARGET_SIGNAL_FPE,
  31, TARGET_SIGNAL_ILL,
0x75, TARGET_SIGNAL_FPE,
0x79, TARGET_SIGNAL_INT,
0x1b, TARGET_SIGNAL_INT,
  -1,-1
};

void
init_go32_extdebug(int *argc, char ***argvx)
{
  int i;
  char **argv;
  char cmdline[128];
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_open(char *name, int from_tty)
{
  printf_unfiltered("Use the `run' command to run go32 programs\n");
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void go32_close(int quitting)
{
  /*printf_unfiltered("go32_close called\n");*/
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_attach(char *args, int from_tty)
{
  printf_unfiltered("Use the `run' command to run go32 programs\n");
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_detach(char *args, int from_tty)
{
  /*printf_unfiltered("go32_detach called\n");*/
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static int resume_is_step;

static void
go32_resume(int pid, int step, enum target_signal siggnal)
{
#if 0
  printf_unfiltered("go32_resume called\n");
#endif
  resume_is_step = step;
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static int
go32_wait(int pid, struct target_waitstatus *status)
{
#if 0
  printf_unfiltered("go32_wait %d\n", pid);
#endif
  if (resume_is_step)
    a_tss.tss_eflags |= 0x0100;
  else
    a_tss.tss_eflags &= 0xfeff;
  run_child();
  if (a_tss.tss_irqn == 0x21)
  {
    status->kind = TARGET_WAITKIND_EXITED;
    status->value.integer = a_tss.tss_eax & 0xff;
  }
  else
  {
    int i;
    status->value.sig = TARGET_SIGNAL_UNKNOWN;
    for (i=0; sig_map[i].go32_sig != -1; i++)
      if (a_tss.tss_irqn == sig_map[i].go32_sig)
      {
        status->value.sig = sig_map[i].gdb_sig;
        break;
      }
    status->kind = TARGET_WAITKIND_STOPPED;
  }
  return SOME_PID;
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_fetch_registers(int regno)
{
/*JHW*/
  int end_reg=regno+1;	/*just one reg initially*/
#if 0
  printf_unfiltered("go32_fetch_registers called\n");
#endif
  if (regno < 0) {	/*do the all registers*/
     regno=0;	/*start at first register*/
     end_reg=sizeof(regno_mapping)/sizeof(regno_mapping[0]);	/*# regs in table*/
  }
  for (; regno<end_reg; regno++) {
     switch (regno_mapping[regno].size)
     {
       case 4:
       case 2:
         supply_register(regno, (char *)&a_tss + regno_mapping[regno].tss_ofs);
         break;
      default:	/*unknown register size*/
         printf_unfiltered("Invalid register size %d bytes in go32_fetch_register()", regno_mapping[regno].size);
         exit(1);
     }
  }
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void store_register(int regno)
{
  char *rp = (char *)&a_tss + regno_mapping[regno].tss_ofs;
  int v = *(int *)(&registers[REGISTER_BYTE(regno)]);
  switch (regno_mapping[regno].size)
  {
    case 4:
      *(int *)rp = v;
      break;
    case 2:
      *(short *)rp = v;
      break;
   default:	/*unknown register size*/
      printf_unfiltered("Invalid register size %d bytes in store_register()", regno_mapping[regno].size);
      exit(1);
  }
}

static void
go32_store_registers(int regno)
{
#if 0
  printf_unfiltered("go32_store_registers called\n");
#endif
  if (regno >= 0)
    store_register(regno);
  else
  {
    int r;
    for (r=0; r<sizeof(regno_mapping)/sizeof(regno_mapping[0]); r++)
      store_register(r);
  }
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_prepare_to_store(void)
{
#if 0
  printf_unfiltered("go32_prepare_to_store called\n");
#endif
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static int
go32_xfer_memory(CORE_ADDR memaddr, char *myaddr, int len, int write,
		 struct target_ops *target)
{
  int res;
#if 0
  printf_unfiltered("go32_xfer_memory %x %x %d %d\n", memaddr, myaddr, len, write);
#endif
  if (write)
    res=write_child(memaddr, myaddr, len);
  else
    res=read_child(memaddr, myaddr, len);
  if (!res)
    return len;
  else
    return -1;
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_files_info(struct target_ops *target)
{
  printf_unfiltered("You are running a DJGPP V2 program\n");
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void go32_stop()
{
  /*printf_unfiltered("go32_stop called\n");*/
  normal_stop();
  cleanup_client();
  inferior_pid = 0;
  prog_has_started = 0;
}

static void
go32_kill_inferior(void)
{
  /*printf_unfiltered("go32_kill_inferior called\n");*/
  go32_stop();
  unpush_target(&go32_ops);
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

extern char **environ;

static void
go32_create_inferior(char *exec_file, char *args, char **env)
{
  jmp_buf start_state;
  char *cmdline;
  char **env_save = environ;
  /*printf_unfiltered("go32_create_inferior called\n");*/
  if (prog_has_started)
  {
    go32_kill_inferior();
  }

  cmdline = (char *)alloca(strlen(args)+4);
  cmdline[0] = strlen(args);
  strcpy(cmdline+1, args);
  cmdline[strlen(args)+1] = 13;

  environ = env;

  if (v2loadimage(exec_file, cmdline, start_state))
  {
    environ = env_save;
    printf_unfiltered("Load failed for image %s\n", exec_file);
    exit(1);
  }
  environ = env_save;

  edi_init(start_state);

  inferior_pid = SOME_PID;
  push_target(&go32_ops);
  clear_proceed_status();
  insert_breakpoints();
  proceed((CORE_ADDR) -1, TARGET_SIGNAL_0, 0);
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void
go32_mourn_inferior(void)
{
  /*printf_filtered("go32_mourn_inferior called\n");*/
  go32_kill_inferior();
  generic_mourn_inferior();
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static int go32_can_run(void)
{
  return 1;
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

void go32_sel_info(int sel)
{
  char read_allowed = 0;
  char write_allowed = 0;
  unsigned long limit,vbase;
  
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
  if (!read_allowed && !write_allowed)
    {
      printf_unfiltered("Invalid selector %04x\n", sel);
      return;
    }
  if (read_allowed)
    printf_unfiltered("R");
  else
    printf_unfiltered(" ");
  if (write_allowed)
    printf_unfiltered("W ");
  else
    printf_unfiltered("  ");
  __dpmi_get_segment_base_address(sel, &vbase);
  limit=__dpmi_get_segment_limit(sel);
  printf_unfiltered(" base %08x\t limit %08x\n",vbase,limit);
}

/* ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ */

static void ignore(void) {}
static void ignore2(char *a,int b) {}

/* ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ */

struct target_ops go32_ops = {
  "go32",			/* to_shortname */
  "go32 target process",	/* to_longname */
  "Program loaded by go32, when gdb is used as an external debugger",	/* to_doc */
  go32_open,			/* to_open */
  go32_close,			/* to_close */
  go32_attach,			/* to_attach */
  go32_detach, 			/* to_detach */
  go32_resume,			/* to_resume */
  go32_wait,			/* to_wait */
  go32_fetch_registers,		/* to_fetch_registers */
  go32_store_registers,		/* to_store_registers */
  go32_prepare_to_store,	/* to_prepare_to_store */
  go32_xfer_memory,		/* to_xfer_memory */
  go32_files_info,		/* to_files_info */
  memory_insert_breakpoint,	/* to_insert_breakpoint */
  memory_remove_breakpoint,	/* to_remove_breakpoint */
  ignore,			/* to_terminal_init */
  ignore,	 		/* to_terminal_inferior */
  ignore,			/* to_terminal_ours_for_output */
  ignore,			/* to_terminal_ours */
  ignore2,			/* to_terminal_info */
  go32_kill_inferior,		/* to_kill */
  0,				/* to_load */
  0,				/* to_lookup_symbol */
  go32_create_inferior,		/* to_create_inferior */
  go32_mourn_inferior,		/* to_mourn_inferior */
  go32_can_run,			/* to_can_run */
  0, 				/* to_notice_signals */
  0,				/* to_thread_alive */
  go32_stop,			/* to_stop */
  process_stratum,		/* to_stratum */
  0,				/* to_next */
  1,				/* to_has_all_memory */
  1,				/* to_has_memory */
  1,				/* to_has_stack */
  1,				/* to_has_registers */
  1,				/* to_has_execution */
  0,				/* sections */
  0,				/* sections_end */
  OPS_MAGIC			/* to_magic */
};

void
_initialize_inftarg ()
{
  add_target (&go32_ops);
}

#ifdef TARGET_HAS_HARDWARE_WATCHPOINTS

#define DR_STATUS 6
#define DR_CONTROL 7
#define DR_ENABLE_SIZE 2
#define DR_LOCAL_ENABLE_SHIFT 0
#define DR_GLOBAL_ENABLE_SHIFT 1
#define DR_LOCAL_SLOWDOWN 0x100
#define DR_GLOBAL_SLOWDOWN 0x200
#define DR_CONTROL_SHIFT 16
#define DR_CONTROL_SIZE 4
#define DR_RW_READ 0x3
#define DR_RW_WRITE 0x1
#define DR_CONTROL_MASK 0xf
#define DR_ENABLE_MASK 0x3
#define DR_LEN_1 0x0
#define DR_LEN_2 0x4
#define DR_LEN_4 0xc

#define D_REGS edi.dr
#define CONTROL D_REGS[DR_CONTROL]
#define STATUS D_REGS[DR_STATUS]

#define IS_REG_FREE(index) \
  (!(CONTROL & (3 << (DR_ENABLE_SIZE * index))))
#define LOCAL_ENABLE_REG(index) \
  (CONTROL |= (1 << (DR_LOCAL_ENABLE_SHIFT + DR_ENABLE_SIZE * index)))
#define GLOBAL_ENABLE_REG(index) \
  (CONTROL |= (1 << (DR_GLOBAL_ENABLE_SHIFT + DR_ENABLE_SIZE * index)))
#define DISABLE_REG(index) \
  (CONTROL &= ~(3 << (DR_ENABLE_SIZE * index)))
#define SET_LOCAL_EXACT() \
  (CONTROL |= DR_LOCAL_SLOWDOWN)
#define SET_GLOBAL_EXACT() \
  (CONTROL |= DR_GLOBAL_SLOWDOWN)
#define SET_BREAK(index,address) \
  do {\
    CONTROL &= ~(DR_CONTROL_MASK << (DR_CONTROL_SHIFT + DR_CONTROL_SIZE * index));\
    D_REGS[index] = address;\
  } while(0)
#define SET_WATCH(index,address,rw,len) \
  do {\
    SET_BREAK(index,address);\
    CONTROL |= (len | rw) << (DR_CONTROL_SHIFT + DR_CONTROL_SIZE * index);\
  } while (0)
  
#define WATCH_HIT(index) \
  (\
   (STATUS & (1 << index)) && \
   (CONTROL & (DR_CONTROL_MASK << (DR_CONTROL_SHIFT + DR_CONTROL_SIZE * index)))\
  )

static int
go32_insert_aligned_watchpoint PARAMS ((int, CORE_ADDR, CORE_ADDR, int,
					   int));

static int
go32_insert_nonaligned_watchpoint PARAMS ((int, CORE_ADDR, CORE_ADDR, int,
					   int));

/* Insert a watchpoint.  */

#define SHOW_DR(text) \
do {\
  fprintf(stderr,"%08x %08x ",edi.dr[7],edi.dr[6]);\
  fprintf(stderr,"%08x %08x ",edi.dr[0],edi.dr[1]);\
  fprintf(stderr,"%08x %08x ",edi.dr[2],edi.dr[3]);\
  fprintf(stderr,"(%s)\n",#text);\
} while (0)

#define _SHOW_DR(x) do {} while(0)

int
go32_insert_watchpoint (pid, addr, len, rw)
     int pid;
     CORE_ADDR addr;
     int len;
     int rw;
{
  int ret = go32_insert_aligned_watchpoint (pid, addr, addr, len, rw);
_SHOW_DR(insert_watch);
  return ret;
}

static int
go32_insert_aligned_watchpoint (pid, waddr, addr, len, rw)
     int pid;
     CORE_ADDR waddr;
     CORE_ADDR addr;
     int len;
     int rw;
{
  int i;
  int read_write_bits, len_bits;
  
  /* Look for a free debug register.  */
  for (i = 0; i <= 3; i++)
    {
      if (IS_REG_FREE(i))
	break;
    }

  /* No more debug registers!  */
  if (i > 3)
    return -1;

  read_write_bits = ((rw & 1) ? DR_RW_READ : 0) | ((rw & 2) ? DR_RW_WRITE : 0);

  if (len == 1)
    len_bits = DR_LEN_1;
  else if (len == 2)
    {
      if (addr % 2)
	return go32_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
      len_bits = DR_LEN_2;
    }

  else if (len == 4)
    {
      if (addr % 4)
	return go32_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
      len_bits = DR_LEN_4;
    }
  else
    return go32_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw);
  
  SET_WATCH(i,addr,read_write_bits,len_bits);
  LOCAL_ENABLE_REG(i);
  SET_LOCAL_EXACT();
}

static int
go32_insert_nonaligned_watchpoint (pid, waddr, addr, len, rw)
     int pid;
     CORE_ADDR waddr;
     CORE_ADDR addr;
     int len;
     int rw;
{
  int align;
  int size;
  int rv;

  static int size_try_array[16] = {
    1, 1, 1, 1,			/* trying size one */
    2, 1, 2, 1,			/* trying size two */
    2, 1, 2, 1,			/* trying size three */
    4, 1, 2, 1			/* trying size four */
  };

  rv = 0;
  while (len > 0)
    {
      align = addr % 4;
      /* Four is the maximum length for 386.  */
      size = (len > 4) ? 3 : len - 1;
      size = size_try_array[size * 4 + align];

      rv = go32_insert_aligned_watchpoint (pid, waddr, addr, size, rw);
      if (rv)
	{
	  go32_remove_watchpoint (pid, waddr, size);
	  return rv;
	}
      addr += size;
      len -= size;
    }
  return rv;
}

/* Remove a watchpoint.  */

int
go32_remove_watchpoint (pid, addr, len)
     int pid;
     CORE_ADDR addr;
     int len;
{
  int i;
  for (i = 0; i <= 3; i++)
    {
      if (D_REGS[i] == addr)
	{
	  DISABLE_REG(i);
	}
    }
_SHOW_DR(remove_watch);
  return 0;
}

/* Check if stopped by a watchpoint.  */

CORE_ADDR
go32_stopped_by_watchpoint (pid)
    int pid;
{
  int i,ret=0;
  int status;

  status = edi.dr[DR_STATUS];
_SHOW_DR(stopped_by);
  for (i = 0; i <= 3; i++)
    {
      if (WATCH_HIT(i))
      {
_SHOW_DR(HIT);
	ret = D_REGS[i];
      }
    }
/* this is a hack to GDB. If we stopped at a hardware breakpoint,
   the stop_pc must incremented by DECR_PC_AFTER_BREAK. I tried everything
   with the DECR_PC_AFTER_HW_BREAK, but nothing works. */
  if (STATUS && !ret)
    stop_pc += DECR_PC_AFTER_BREAK;
  STATUS = 0;
  return ret;
}

/* Remove a breakpoint.  */

int
go32_remove_hw_breakpoint (addr, shadow)
     CORE_ADDR addr;
     CORE_ADDR shadow;
{
  int i;
  for (i = 0; i <= 3; i++)
    {
      if (D_REGS[i] == addr)
	{
	  DISABLE_REG(i);
	}
    }
_SHOW_DR(remove_hw);
  return 0;
}

int
go32_insert_hw_breakpoint (addr, shadow)
    CORE_ADDR addr;
    CORE_ADDR shadow;
{
  int i;
  int read_write_bits, len_bits;
  int free_debug_register;
  int register_number;
  
  /* Look for a free debug register.  */
  for (i = 0; i <= 3; i++)
    {
      if (IS_REG_FREE(i))
	break;
    }

  /* No more debug registers!  */
  if (i > 3)
    return -1;

  SET_BREAK(i,addr);
  LOCAL_ENABLE_REG(i);
_SHOW_DR(insert_hw);
  return 0;
}

#endif /* TARGET_HAS_HARDWARE_WATCHPOINTS */

