{******************************************************************************}
{                                                       	               }
{ NT Bug Codes API interface Unit for Object Pascal                            }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: bugcodes.h, released June 2000. The original Pascal    }
{ code is: BugCodes.pas, released December 2000. The initial developer of the  }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{								               }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{								               }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{ 								               }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{ 								               }
{******************************************************************************}

unit JwaBugCodes;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "bugcodes.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

{$I WINDEFINES.INC}

//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//


//
// Define the severity codes
//

const

//
// MessageId: APC_INDEX_MISMATCH
//
// MessageText:
//
//  APC_INDEX_MISMATCH
//

  APC_INDEX_MISMATCH = ULONG($00000001);

//
// MessageId: DEVICE_QUEUE_NOT_BUSY
//
// MessageText:
//
//  DEVICE_QUEUE_NOT_BUSY
//

  DEVICE_QUEUE_NOT_BUSY = ULONG($00000002);

//
// MessageId: INVALID_AFFINITY_SET
//
// MessageText:
//
//  INVALID_AFFINITY_SET
//

  INVALID_AFFINITY_SET = ULONG($00000003);

//
// MessageId: INVALID_DATA_ACCESS_TRAP
//
// MessageText:
//
//  INVALID_DATA_ACCESS_TRAP
//

  INVALID_DATA_ACCESS_TRAP = ULONG($00000004);

//
// MessageId: INVALID_PROCESS_ATTACH_ATTEMPT
//
// MessageText:
//
//  INVALID_PROCESS_ATTACH_ATTEMPT
//

  INVALID_PROCESS_ATTACH_ATTEMPT = ULONG($00000005);

//
// MessageId: INVALID_PROCESS_DETACH_ATTEMPT
//
// MessageText:
//
//  INVALID_PROCESS_DETACH_ATTEMPT
//

  INVALID_PROCESS_DETACH_ATTEMPT = ULONG($00000006);

//
// MessageId: INVALID_SOFTWARE_INTERRUPT
//
// MessageText:
//
//  INVALID_SOFTWARE_INTERRUPT
//

  INVALID_SOFTWARE_INTERRUPT = ULONG($00000007);

//
// MessageId: IRQL_NOT_DISPATCH_LEVEL
//
// MessageText:
//
//  IRQL_NOT_DISPATCH_LEVEL
//

  IRQL_NOT_DISPATCH_LEVEL = ULONG($00000008);

//
// MessageId: IRQL_NOT_GREATER_OR_EQUAL
//
// MessageText:
//
//  IRQL_NOT_GREATER_OR_EQUAL
//

  IRQL_NOT_GREATER_OR_EQUAL = ULONG($00000009);

//
// MessageId: IRQL_NOT_LESS_OR_EQUAL
//
// MessageText:
//
//  IRQL_NOT_LESS_OR_EQUAL
//

  IRQL_NOT_LESS_OR_EQUAL = ULONG($0000000A);

//
// MessageId: NO_EXCEPTION_HANDLING_SUPPORT
//
// MessageText:
//
//  NO_EXCEPTION_HANDLING_SUPPORT
//

  NO_EXCEPTION_HANDLING_SUPPORT = ULONG($0000000B);

//
// MessageId: MAXIMUM_WAIT_OBJECTS_EXCEEDED
//
// MessageText:
//
//  MAXIMUM_WAIT_OBJECTS_EXCEEDED
//

  MAXIMUM_WAIT_OBJECTS_EXCEEDED = ULONG($0000000C);

//
// MessageId: MUTEX_LEVEL_NUMBER_VIOLATION
//
// MessageText:
//
//  MUTEX_LEVEL_NUMBER_VIOLATION
//

  MUTEX_LEVEL_NUMBER_VIOLATION = ULONG($0000000D);

//
// MessageId: NO_USER_MODE_CONTEXT
//
// MessageText:
//
//  NO_USER_MODE_CONTEXT
//

  NO_USER_MODE_CONTEXT = ULONG($0000000E);

//
// MessageId: SPIN_LOCK_ALREADY_OWNED
//
// MessageText:
//
//  SPIN_LOCK_ALREADY_OWNED
//

  SPIN_LOCK_ALREADY_OWNED = ULONG($0000000F);

//
// MessageId: SPIN_LOCK_NOT_OWNED
//
// MessageText:
//
//  SPIN_LOCK_NOT_OWNED
//

  SPIN_LOCK_NOT_OWNED = ULONG($00000010);

//
// MessageId: THREAD_NOT_MUTEX_OWNER
//
// MessageText:
//
//  THREAD_NOT_MUTEX_OWNER
//

  THREAD_NOT_MUTEX_OWNER = ULONG($00000011);

//
// MessageId: TRAP_CAUSE_UNKNOWN
//
// MessageText:
//
//  TRAP_CAUSE_UNKNOWN
//

  TRAP_CAUSE_UNKNOWN = ULONG($00000012);

//
// MessageId: EMPTY_THREAD_REAPER_LIST
//
// MessageText:
//
//  EMPTY_THREAD_REAPER_LIST
//

  EMPTY_THREAD_REAPER_LIST = ULONG($00000013);

//
// MessageId: CREATE_DELETE_LOCK_NOT_LOCKED
//
// MessageText:
//
//  CREATE_DELETE_LOCK_NOT_LOCKED
//

  CREATE_DELETE_LOCK_NOT_LOCKED = ULONG($00000014);

//
// MessageId: LAST_CHANCE_CALLED_FROM_KMODE
//
// MessageText:
//
//  LAST_CHANCE_CALLED_FROM_KMODE
//

  LAST_CHANCE_CALLED_FROM_KMODE = ULONG($00000015);

//
// MessageId: CID_HANDLE_CREATION
//
// MessageText:
//
//  CID_HANDLE_CREATION
//

  CID_HANDLE_CREATION = ULONG($00000016);

//
// MessageId: CID_HANDLE_DELETION
//
// MessageText:
//
//  CID_HANDLE_DELETION
//

  CID_HANDLE_DELETION = ULONG($00000017);

//
// MessageId: REFERENCE_BY_POINTER
//
// MessageText:
//
//  REFERENCE_BY_POINTER
//

  REFERENCE_BY_POINTER = ULONG($00000018);

//
// MessageId: BAD_POOL_HEADER
//
// MessageText:
//
//  BAD_POOL_HEADER
//

  BAD_POOL_HEADER = ULONG($00000019);

//
// MessageId: MEMORY_MANAGEMENT
//
// MessageText:
//
//  MEMORY_MANAGEMENT
//

  MEMORY_MANAGEMENT = ULONG($0000001A);

//
// MessageId: PFN_SHARE_COUNT
//
// MessageText:
//
//  PFN_SHARE_COUNT
//

  PFN_SHARE_COUNT = ULONG($0000001B);

//
// MessageId: PFN_REFERENCE_COUNT
//
// MessageText:
//
//  PFN_REFERENCE_COUNT
//

  PFN_REFERENCE_COUNT = ULONG($0000001C);

//
// MessageId: NO_SPIN_LOCK_AVAILABLE
//
// MessageText:
//
//  NO_SPIN_LOCK_AVAILABLE
//

  NO_SPIN_LOCK_AVAILABLE = ULONG($0000001D);

//
// MessageId: KMODE_EXCEPTION_NOT_HANDLED
//
// MessageText:
//
//  KMODE_EXCEPTION_NOT_HANDLED
//

  KMODE_EXCEPTION_NOT_HANDLED = ULONG($0000001E);

//
// MessageId: SHARED_RESOURCE_CONV_ERROR
//
// MessageText:
//
//  SHARED_RESOURCE_CONV_ERROR
//

  SHARED_RESOURCE_CONV_ERROR = ULONG($0000001F);

//
// MessageId: KERNEL_APC_PENDING_DURING_EXIT
//
// MessageText:
//
//  KERNEL_APC_PENDING_DURING_EXIT
//

  KERNEL_APC_PENDING_DURING_EXIT = ULONG($00000020);

//
// MessageId: QUOTA_UNDERFLOW
//
// MessageText:
//
//  QUOTA_UNDERFLOW
//

  QUOTA_UNDERFLOW = ULONG($00000021);

//
// MessageId: FILE_SYSTEM
//
// MessageText:
//
//  FILE_SYSTEM
//

  FILE_SYSTEM = ULONG($00000022);

//
// MessageId: FAT_FILE_SYSTEM
//
// MessageText:
//
//  FAT_FILE_SYSTEM
//

  FAT_FILE_SYSTEM = ULONG($00000023);

//
// MessageId: NTFS_FILE_SYSTEM
//
// MessageText:
//
//  NTFS_FILE_SYSTEM
//

  NTFS_FILE_SYSTEM = ULONG($00000024);

//
// MessageId: NPFS_FILE_SYSTEM
//
// MessageText:
//
//  NPFS_FILE_SYSTEM
//

  NPFS_FILE_SYSTEM = ULONG($00000025);

//
// MessageId: CDFS_FILE_SYSTEM
//
// MessageText:
//
//  CDFS_FILE_SYSTEM
//

  CDFS_FILE_SYSTEM = ULONG($00000026);

//
// MessageId: RDR_FILE_SYSTEM
//
// MessageText:
//
//  RDR_FILE_SYSTEM
//

  RDR_FILE_SYSTEM = ULONG($00000027);

//
// MessageId: CORRUPT_ACCESS_TOKEN
//
// MessageText:
//
//  CORRUPT_ACCESS_TOKEN
//

  CORRUPT_ACCESS_TOKEN = ULONG($00000028);

//
// MessageId: SECURITY_SYSTEM
//
// MessageText:
//
//  SECURITY_SYSTEM
//

  SECURITY_SYSTEM = ULONG($00000029);

//
// MessageId: INCONSISTENT_IRP
//
// MessageText:
//
//  INCONSISTENT_IRP
//

  INCONSISTENT_IRP = ULONG($0000002A);

//
// MessageId: PANIC_STACK_SWITCH
//
// MessageText:
//
//  PANIC_STACK_SWITCH
//

  PANIC_STACK_SWITCH = ULONG($0000002B);

//
// MessageId: PORT_DRIVER_INTERNAL
//
// MessageText:
//
//  PORT_DRIVER_INTERNAL
//

  PORT_DRIVER_INTERNAL = ULONG($0000002C);

//
// MessageId: SCSI_DISK_DRIVER_INTERNAL
//
// MessageText:
//
//  SCSI_DISK_DRIVER_INTERNAL
//

  SCSI_DISK_DRIVER_INTERNAL = ULONG($0000002D);

//
// MessageId: DATA_BUS_ERROR
//
// MessageText:
//
//  DATA_BUS_ERROR
//

  DATA_BUS_ERROR = ULONG($0000002E);

//
// MessageId: INSTRUCTION_BUS_ERROR
//
// MessageText:
//
//  INSTRUCTION_BUS_ERROR
//

  INSTRUCTION_BUS_ERROR = ULONG($0000002F);

//
// MessageId: SET_OF_INVALID_CONTEXT
//
// MessageText:
//
//  SET_OF_INVALID_CONTEXT
//

  SET_OF_INVALID_CONTEXT = ULONG($00000030);

//
// MessageId: PHASE0_INITIALIZATION_FAILED
//
// MessageText:
//
//  PHASE0_INITIALIZATION_FAILED
//

  PHASE0_INITIALIZATION_FAILED = ULONG($00000031);

//
// MessageId: PHASE1_INITIALIZATION_FAILED
//
// MessageText:
//
//  PHASE1_INITIALIZATION_FAILED
//

  PHASE1_INITIALIZATION_FAILED = ULONG($00000032);

//
// MessageId: UNEXPECTED_INITIALIZATION_CALL
//
// MessageText:
//
//  UNEXPECTED_INITIALIZATION_CALL
//

  UNEXPECTED_INITIALIZATION_CALL = ULONG($00000033);

//
// MessageId: CACHE_MANAGER
//
// MessageText:
//
//  CACHE_MANAGER
//

  CACHE_MANAGER = ULONG($00000034);

//
// MessageId: NO_MORE_IRP_STACK_LOCATIONS
//
// MessageText:
//
//  NO_MORE_IRP_STACK_LOCATIONS
//

  NO_MORE_IRP_STACK_LOCATIONS = ULONG($00000035);

//
// MessageId: DEVICE_REFERENCE_COUNT_NOT_ZERO
//
// MessageText:
//
//  DEVICE_REFERENCE_COUNT_NOT_ZERO
//

  DEVICE_REFERENCE_COUNT_NOT_ZERO = ULONG($00000036);

//
// MessageId: FLOPPY_INTERNAL_ERROR
//
// MessageText:
//
//  FLOPPY_INTERNAL_ERROR
//

  FLOPPY_INTERNAL_ERROR = ULONG($00000037);

//
// MessageId: SERIAL_DRIVER_INTERNAL
//
// MessageText:
//
//  SERIAL_DRIVER_INTERNAL
//

  SERIAL_DRIVER_INTERNAL = ULONG($00000038);

//
// MessageId: SYSTEM_EXIT_OWNED_MUTEX
//
// MessageText:
//
//  SYSTEM_EXIT_OWNED_MUTEX
//

  SYSTEM_EXIT_OWNED_MUTEX = ULONG($00000039);

//
// MessageId: SYSTEM_UNWIND_PREVIOUS_USER
//
// MessageText:
//
//  SYSTEM_UNWIND_PREVIOUS_USER
//

  SYSTEM_UNWIND_PREVIOUS_USER = ULONG($0000003A);

//
// MessageId: SYSTEM_SERVICE_EXCEPTION
//
// MessageText:
//
//  SYSTEM_SERVICE_EXCEPTION
//

  SYSTEM_SERVICE_EXCEPTION = ULONG($0000003B);

//
// MessageId: INTERRUPT_UNWIND_ATTEMPTED
//
// MessageText:
//
//  INTERRUPT_UNWIND_ATTEMPTED
//

  INTERRUPT_UNWIND_ATTEMPTED = ULONG($0000003C);

//
// MessageId: INTERRUPT_EXCEPTION_NOT_HANDLED
//
// MessageText:
//
//  INTERRUPT_EXCEPTION_NOT_HANDLED
//

  INTERRUPT_EXCEPTION_NOT_HANDLED = ULONG($0000003D);

//
// MessageId: MULTIPROCESSOR_CONFIGURATION_NOT_SUPPORTED
//
// MessageText:
//
//  MULTIPROCESSOR_CONFIGURATION_NOT_SUPPORTED
//

  MULTIPROCESSOR_CONFIGURATION_NOT_SUPPORTED = ULONG($0000003E);

//
// MessageId: NO_MORE_SYSTEM_PTES
//
// MessageText:
//
//  NO_MORE_SYSTEM_PTES
//

  NO_MORE_SYSTEM_PTES = ULONG($0000003F);

//
// MessageId: TARGET_MDL_TOO_SMALL
//
// MessageText:
//
//  TARGET_MDL_TOO_SMALL
//

  TARGET_MDL_TOO_SMALL = ULONG($00000040);

//
// MessageId: MUST_SUCCEED_POOL_EMPTY
//
// MessageText:
//
//  MUST_SUCCEED_POOL_EMPTY
//

  MUST_SUCCEED_POOL_EMPTY = ULONG($00000041);

//
// MessageId: ATDISK_DRIVER_INTERNAL
//
// MessageText:
//
//  ATDISK_DRIVER_INTERNAL
//

  ATDISK_DRIVER_INTERNAL = ULONG($00000042);

//
// MessageId: NO_SUCH_PARTITION
//
// MessageText:
//
//  NO_SUCH_PARTITION
//

  NO_SUCH_PARTITION = ULONG($00000043);

//
// MessageId: MULTIPLE_IRP_COMPLETE_REQUESTS
//
// MessageText:
//
//  MULTIPLE_IRP_COMPLETE_REQUESTS
//

  MULTIPLE_IRP_COMPLETE_REQUESTS = ULONG($00000044);

//
// MessageId: INSUFFICIENT_SYSTEM_MAP_REGS
//
// MessageText:
//
//  INSUFFICIENT_SYSTEM_MAP_REGS
//

  INSUFFICIENT_SYSTEM_MAP_REGS = ULONG($00000045);

//
// MessageId: DEREF_UNKNOWN_LOGON_SESSION
//
// MessageText:
//
//  DEREF_UNKNOWN_LOGON_SESSION
//

  DEREF_UNKNOWN_LOGON_SESSION = ULONG($00000046);

//
// MessageId: REF_UNKNOWN_LOGON_SESSION
//
// MessageText:
//
//  REF_UNKNOWN_LOGON_SESSION
//

  REF_UNKNOWN_LOGON_SESSION = ULONG($00000047);

//
// MessageId: CANCEL_STATE_IN_COMPLETED_IRP
//
// MessageText:
//
//  CANCEL_STATE_IN_COMPLETED_IRP
//

  CANCEL_STATE_IN_COMPLETED_IRP = ULONG($00000048);

//
// MessageId: PAGE_FAULT_WITH_INTERRUPTS_OFF
//
// MessageText:
//
//  PAGE_FAULT_WITH_INTERRUPTS_OFF
//

  PAGE_FAULT_WITH_INTERRUPTS_OFF = ULONG($00000049);

//
// MessageId: IRQL_GT_ZERO_AT_SYSTEM_SERVICE
//
// MessageText:
//
//  IRQL_GT_ZERO_AT_SYSTEM_SERVICE
//

  IRQL_GT_ZERO_AT_SYSTEM_SERVICE = ULONG($0000004A);

//
// MessageId: STREAMS_INTERNAL_ERROR
//
// MessageText:
//
//  STREAMS_INTERNAL_ERROR
//

  STREAMS_INTERNAL_ERROR = ULONG($0000004B);

//
// MessageId: FATAL_UNHANDLED_HARD_ERROR
//
// MessageText:
//
//  FATAL_UNHANDLED_HARD_ERROR
//

  FATAL_UNHANDLED_HARD_ERROR = ULONG($0000004C);

//
// MessageId: NO_PAGES_AVAILABLE
//
// MessageText:
//
//  NO_PAGES_AVAILABLE
//

  NO_PAGES_AVAILABLE = ULONG($0000004D);

//
// MessageId: PFN_LIST_CORRUPT
//
// MessageText:
//
//  PFN_LIST_CORRUPT
//

  PFN_LIST_CORRUPT = ULONG($0000004E);

//
// MessageId: NDIS_INTERNAL_ERROR
//
// MessageText:
//
//  NDIS_INTERNAL_ERROR
//

  NDIS_INTERNAL_ERROR = ULONG($0000004F);

//
// MessageId: PAGE_FAULT_IN_NONPAGED_AREA
//
// MessageText:
//
//  PAGE_FAULT_IN_NONPAGED_AREA
//

  PAGE_FAULT_IN_NONPAGED_AREA = ULONG($00000050);

//
// MessageId: REGISTRY_ERROR
//
// MessageText:
//
//  REGISTRY_ERROR
//

  REGISTRY_ERROR = ULONG($00000051);

//
// MessageId: MAILSLOT_FILE_SYSTEM
//
// MessageText:
//
//  MAILSLOT_FILE_SYSTEM
//

  MAILSLOT_FILE_SYSTEM = ULONG($00000052);

//
// MessageId: NO_BOOT_DEVICE
//
// MessageText:
//
//  NO_BOOT_DEVICE
//

  NO_BOOT_DEVICE = ULONG($00000053);

//
// MessageId: LM_SERVER_INTERNAL_ERROR
//
// MessageText:
//
//  LM_SERVER_INTERNAL_ERROR
//

  LM_SERVER_INTERNAL_ERROR = ULONG($00000054);

//
// MessageId: DATA_COHERENCY_EXCEPTION
//
// MessageText:
//
//  DATA_COHERENCY_EXCEPTION
//

  DATA_COHERENCY_EXCEPTION = ULONG($00000055);

//
// MessageId: INSTRUCTION_COHERENCY_EXCEPTION
//
// MessageText:
//
//  INSTRUCTION_COHERENCY_EXCEPTION
//

  INSTRUCTION_COHERENCY_EXCEPTION = ULONG($00000056);

//
// MessageId: XNS_INTERNAL_ERROR
//
// MessageText:
//
//  XNS_INTERNAL_ERROR
//

  XNS_INTERNAL_ERROR = ULONG($00000057);

//
// MessageId: FTDISK_INTERNAL_ERROR
//
// MessageText:
//
//  FTDISK_INTERNAL_ERROR
//

  FTDISK_INTERNAL_ERROR = ULONG($00000058);

//
// MessageId: PINBALL_FILE_SYSTEM
//
// MessageText:
//
//  PINBALL_FILE_SYSTEM
//

  PINBALL_FILE_SYSTEM = ULONG($00000059);

//
// MessageId: CRITICAL_SERVICE_FAILED
//
// MessageText:
//
//  CRITICAL_SERVICE_FAILED
//

  CRITICAL_SERVICE_FAILED = ULONG($0000005A);

//
// MessageId: SET_ENV_VAR_FAILED
//
// MessageText:
//
//  SET_ENV_VAR_FAILED
//

  SET_ENV_VAR_FAILED = ULONG($0000005B);

//
// MessageId: HAL_INITIALIZATION_FAILED
//
// MessageText:
//
//  HAL_INITIALIZATION_FAILED
//

  HAL_INITIALIZATION_FAILED = ULONG($0000005C);

//
// MessageId: UNSUPPORTED_PROCESSOR
//
// MessageText:
//
//  UNSUPPORTED_PROCESSOR
//

  UNSUPPORTED_PROCESSOR = ULONG($0000005D);

//
// MessageId: OBJECT_INITIALIZATION_FAILED
//
// MessageText:
//
//  OBJECT_INITIALIZATION_FAILED
//

  OBJECT_INITIALIZATION_FAILED = ULONG($0000005E);

//
// MessageId: SECURITY_INITIALIZATION_FAILED
//
// MessageText:
//
//  SECURITY_INITIALIZATION_FAILED
//

  SECURITY_INITIALIZATION_FAILED = ULONG($0000005F);

//
// MessageId: PROCESS_INITIALIZATION_FAILED
//
// MessageText:
//
//  PROCESS_INITIALIZATION_FAILED
//

  PROCESS_INITIALIZATION_FAILED = ULONG($00000060);

//
// MessageId: HAL1_INITIALIZATION_FAILED
//
// MessageText:
//
//  HAL1_INITIALIZATION_FAILED
//

  HAL1_INITIALIZATION_FAILED = ULONG($00000061);

//
// MessageId: OBJECT1_INITIALIZATION_FAILED
//
// MessageText:
//
//  OBJECT1_INITIALIZATION_FAILED
//

  OBJECT1_INITIALIZATION_FAILED = ULONG($00000062);

//
// MessageId: SECURITY1_INITIALIZATION_FAILED
//
// MessageText:
//
//  SECURITY1_INITIALIZATION_FAILED
//

  SECURITY1_INITIALIZATION_FAILED = ULONG($00000063);

//
// MessageId: SYMBOLIC_INITIALIZATION_FAILED
//
// MessageText:
//
//  SYMBOLIC_INITIALIZATION_FAILED
//

  SYMBOLIC_INITIALIZATION_FAILED = ULONG($00000064);

//
// MessageId: MEMORY1_INITIALIZATION_FAILED
//
// MessageText:
//
//  MEMORY1_INITIALIZATION_FAILED
//

  MEMORY1_INITIALIZATION_FAILED = ULONG($00000065);

//
// MessageId: CACHE_INITIALIZATION_FAILED
//
// MessageText:
//
//  CACHE_INITIALIZATION_FAILED
//

  CACHE_INITIALIZATION_FAILED = ULONG($00000066);

//
// MessageId: CONFIG_INITIALIZATION_FAILED
//
// MessageText:
//
//  CONFIG_INITIALIZATION_FAILED
//

  CONFIG_INITIALIZATION_FAILED = ULONG($00000067);

//
// MessageId: FILE_INITIALIZATION_FAILED
//
// MessageText:
//
//  FILE_INITIALIZATION_FAILED
//

  FILE_INITIALIZATION_FAILED = ULONG($00000068);

//
// MessageId: IO1_INITIALIZATION_FAILED
//
// MessageText:
//
//  IO1_INITIALIZATION_FAILED
//

  IO1_INITIALIZATION_FAILED = ULONG($00000069);

//
// MessageId: LPC_INITIALIZATION_FAILED
//
// MessageText:
//
//  LPC_INITIALIZATION_FAILED
//

  LPC_INITIALIZATION_FAILED = ULONG($0000006A);

//
// MessageId: PROCESS1_INITIALIZATION_FAILED
//
// MessageText:
//
//  PROCESS1_INITIALIZATION_FAILED
//

  PROCESS1_INITIALIZATION_FAILED = ULONG($0000006B);

//
// MessageId: REFMON_INITIALIZATION_FAILED
//
// MessageText:
//
//  REFMON_INITIALIZATION_FAILED
//

  REFMON_INITIALIZATION_FAILED = ULONG($0000006C);

//
// MessageId: SESSION1_INITIALIZATION_FAILED
//
// MessageText:
//
//  SESSION1_INITIALIZATION_FAILED
//

  SESSION1_INITIALIZATION_FAILED = ULONG($0000006D);

//
// MessageId: SESSION2_INITIALIZATION_FAILED
//
// MessageText:
//
//  SESSION2_INITIALIZATION_FAILED
//

  SESSION2_INITIALIZATION_FAILED = ULONG($0000006E);

//
// MessageId: SESSION3_INITIALIZATION_FAILED
//
// MessageText:
//
//  SESSION3_INITIALIZATION_FAILED
//

  SESSION3_INITIALIZATION_FAILED = ULONG($0000006F);

//
// MessageId: SESSION4_INITIALIZATION_FAILED
//
// MessageText:
//
//  SESSION4_INITIALIZATION_FAILED
//

  SESSION4_INITIALIZATION_FAILED = ULONG($00000070);

//
// MessageId: SESSION5_INITIALIZATION_FAILED
//
// MessageText:
//
//  SESSION5_INITIALIZATION_FAILED
//

  SESSION5_INITIALIZATION_FAILED = ULONG($00000071);

//
// MessageId: ASSIGN_DRIVE_LETTERS_FAILED
//
// MessageText:
//
//  ASSIGN_DRIVE_LETTERS_FAILED
//

  ASSIGN_DRIVE_LETTERS_FAILED = ULONG($00000072);

//
// MessageId: CONFIG_LIST_FAILED
//
// MessageText:
//
//  CONFIG_LIST_FAILED
//

  CONFIG_LIST_FAILED = ULONG($00000073);

//
// MessageId: BAD_SYSTEM_CONFIG_INFO
//
// MessageText:
//
//  BAD_SYSTEM_CONFIG_INFO
//

  BAD_SYSTEM_CONFIG_INFO = ULONG($00000074);

//
// MessageId: CANNOT_WRITE_CONFIGURATION
//
// MessageText:
//
//  CANNOT_WRITE_CONFIGURATION
//

  CANNOT_WRITE_CONFIGURATION = ULONG($00000075);

//
// MessageId: PROCESS_HAS_LOCKED_PAGES
//
// MessageText:
//
//  PROCESS_HAS_LOCKED_PAGES
//

  PROCESS_HAS_LOCKED_PAGES = ULONG($00000076);

//
// MessageId: KERNEL_STACK_INPAGE_ERROR
//
// MessageText:
//
//  KERNEL_STACK_INPAGE_ERROR
//

  KERNEL_STACK_INPAGE_ERROR = ULONG($00000077);

//
// MessageId: PHASE0_EXCEPTION
//
// MessageText:
//
//  PHASE0_EXCEPTION
//

  PHASE0_EXCEPTION = ULONG($00000078);

//
// MessageId: MISMATCHED_HAL
//
// MessageText:
//
//  Mismatched kernel and hal image.
//

  MISMATCHED_HAL = ULONG($00000079);

//
// MessageId: KERNEL_DATA_INPAGE_ERROR
//
// MessageText:
//
//  KERNEL_DATA_INPAGE_ERROR
//

  KERNEL_DATA_INPAGE_ERROR = ULONG($0000007A);

//
// MessageId: INACCESSIBLE_BOOT_DEVICE
//
// MessageText:
//
//  INACCESSIBLE_BOOT_DEVICE
//

  INACCESSIBLE_BOOT_DEVICE = ULONG($0000007B);

//
// MessageId: BUGCODE_PSS_MESSAGE
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Check to make sure any new hardware or software is properly installed.
//  If this is a new installation, ask your hardware or software manufacturer
//  for any Windows 2000 updates you might need.
//  
//  If problems continue, disable or remove any newly installed hardware
//  or software. Disable BIOS memory options such as caching or shadowing.
//  If you need to use Safe Mode to remove or disable components, restart
//  your computer, press F8 to select Advanced Startup Options, and then
//  select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE = ULONG($0000007C);

//
// MessageId: INSTALL_MORE_MEMORY
//
// MessageText:
//
//  INSTALL_MORE_MEMORY
//

  INSTALL_MORE_MEMORY = ULONG($0000007D);

//
// MessageId: WINDOWS_NT_BANNER
//
// MessageText:
//
//  Microsoft (R) Windows 2000 (R) Version %hs (Build %u%hs)
//

  WINDOWS_NT_BANNER = ULONG($4000007E);

//
// MessageId: UNEXPECTED_KERNEL_MODE_TRAP
//
// MessageText:
//
//  UNEXPECTED_KERNEL_MODE_TRAP
//

  UNEXPECTED_KERNEL_MODE_TRAP = ULONG($0000007F);

//
// MessageId: NMI_HARDWARE_FAILURE
//
// MessageText:
//
//  Hardware malfunction.
//

  NMI_HARDWARE_FAILURE = ULONG($00000080);

//
// MessageId: SPIN_LOCK_INIT_FAILURE
//
// MessageText:
//
//  SPIN_LOCK_INIT_FAILURE
//

  SPIN_LOCK_INIT_FAILURE = ULONG($00000081);

//
// MessageId: DFS_FILE_SYSTEM
//
// MessageText:
//
//  DFS_FILE_SYSTEM
//

  DFS_FILE_SYSTEM = ULONG($00000082);

//
// MessageId: OFS_FILE_SYSTEM
//
// MessageText:
//
//  OFS_FILE_SYSTEM
//

  OFS_FILE_SYSTEM = ULONG($00000083);

//
// MessageId: RECOM_DRIVER
//
// MessageText:
//
//  RECOM_DRIVER
//

  RECOM_DRIVER = ULONG($00000084);

//
// MessageId: SETUP_FAILURE
//
// MessageText:
//
//  SETUP_FAILURE
//

  SETUP_FAILURE = ULONG($00000085);

//
// MessageId: AUDIT_FAILURE
//
// MessageText:
//
//  Audit attempt has failed.
//

  AUDIT_FAILURE = ULONG($00000086);

//
// MessageId: WINDOWS_NT_CSD_STRING
//
// MessageText:
//
//  Service Pack
//

  WINDOWS_NT_CSD_STRING = ULONG($40000087);

//
// MessageId: WINDOWS_NT_INFO_STRING
//
// MessageText:
//
//  %u System Processor [%u MB Memory] %Z
//

  WINDOWS_NT_INFO_STRING = ULONG($40000088);

//
// MessageId: WINDOWS_NT_MP_STRING
//
// MessageText:
//
//  MultiProcessor Kernel
//

  WINDOWS_NT_MP_STRING = ULONG($40000089);

//
// MessageId: THREAD_TERMINATE_HELD_MUTEX
//
// MessageText:
//
//  A kernel thread terminated while holding a mutex
//

  THREAD_TERMINATE_HELD_MUTEX = ULONG($4000008A);

//
// MessageId: MBR_CHECKSUM_MISMATCH
//
// MessageText:
//
//  This system may be infected with a virus.
//

  MBR_CHECKSUM_MISMATCH = ULONG($0000008B);

//
// MessageId: BUGCODE_PSS_CRASH_INIT
//
// MessageText:
//
//  Beginning dump of physical memory
//

  BUGCODE_PSS_CRASH_INIT = ULONG($0000008C);

//
// MessageId: BUGCODE_PSS_CRASH_PROGRESS
//
// MessageText:
//
//  Dumping physical memory to disk
//

  BUGCODE_PSS_CRASH_PROGRESS = ULONG($0000008D);

//
// MessageId: BUGCODE_PSS_CRASH_DONE
//
// MessageText:
//
//  Physical memory dump complete. Contact your system administrator or
//  technical support group.
//

  BUGCODE_PSS_CRASH_DONE = ULONG($0000008E);

//
// MessageId: PP0_INITIALIZATION_FAILED
//
// MessageText:
//
//  PP0_INITIALIZATION_FAILED
//

  PP0_INITIALIZATION_FAILED = ULONG($0000008F);

//
// MessageId: PP1_INITIALIZATION_FAILED
//
// MessageText:
//
//  PP1_INITIALIZATION_FAILED
//

  PP1_INITIALIZATION_FAILED = ULONG($00000090);

//
// MessageId: WIN32K_INIT_OR_RIT_FAILURE
//
// MessageText:
//
//  WIN32K_INIT_OR_RIT_FAILURE
//

  WIN32K_INIT_OR_RIT_FAILURE = ULONG($00000091);

//
// MessageId: UP_DRIVER_ON_MP_SYSTEM
//
// MessageText:
//
//  UP_DRIVER_ON_MP_SYSTEM
//

  UP_DRIVER_ON_MP_SYSTEM = ULONG($00000092);

//
// MessageId: INVALID_KERNEL_HANDLE
//
// MessageText:
//
//  INVALID_KERNEL_HANDLE
//

  INVALID_KERNEL_HANDLE = ULONG($00000093);

//
// MessageId: KERNEL_STACK_LOCKED_AT_EXIT
//
// MessageText:
//
//  KERNEL_STACK_LOCKED_AT_EXIT
//

  KERNEL_STACK_LOCKED_AT_EXIT = ULONG($00000094);

//
// MessageId: PNP_INTERNAL_ERROR
//
// MessageText:
//
//  PNP_INTERNAL_ERROR
//

  PNP_INTERNAL_ERROR = ULONG($00000095);

//
// MessageId: INVALID_WORK_QUEUE_ITEM
//
// MessageText:
//
//  INVALID_WORK_QUEUE_ITEM
//

  INVALID_WORK_QUEUE_ITEM = ULONG($00000096);

//
// MessageId: BOUND_IMAGE_UNSUPPORTED
//
// MessageText:
//
//  BOUND_IMAGE_UNSUPPORTED
//

  BOUND_IMAGE_UNSUPPORTED = ULONG($00000097);

//
// MessageId: END_OF_NT_EVALUATION_PERIOD
//
// MessageText:
//
//  END_OF_NT_EVALUATION_PERIOD
//

  END_OF_NT_EVALUATION_PERIOD = ULONG($00000098);

//
// MessageId: INVALID_REGION_OR_SEGMENT
//
// MessageText:
//
//  INVALID_REGION_OR_SEGMENT
//

  INVALID_REGION_OR_SEGMENT = ULONG($00000099);

//
// MessageId: SYSTEM_LICENSE_VIOLATION
//
// MessageText:
//
//  SYSTEM_LICENSE_VIOLATION
//

  SYSTEM_LICENSE_VIOLATION = ULONG($0000009A);

//
// MessageId: UDFS_FILE_SYSTEM
//
// MessageText:
//
//  UDFS_FILE_SYSTEM
//

  UDFS_FILE_SYSTEM = ULONG($0000009B);

//
// MessageId: MACHINE_CHECK_EXCEPTION
//
// MessageText:
//
//  MACHINE_CHECK_EXCEPTION
//

  MACHINE_CHECK_EXCEPTION = ULONG($0000009C);

//
// MessageId: WINDOWS_NT_INFO_STRING_PLURAL
//
// MessageText:
//
//  %u System Processors [%u MB Memory] %Z
//

  WINDOWS_NT_INFO_STRING_PLURAL = ULONG($4000009D);

//
// MessageId: WINDOWS_NT_RC_STRING
//
// MessageText:
//
//  RC
//

  WINDOWS_NT_RC_STRING = ULONG($4000009E);

//
// MessageId: DRIVER_POWER_STATE_FAILURE
//
// MessageText:
//
//  DRIVER_POWER_STATE_FAILURE
//

  DRIVER_POWER_STATE_FAILURE = ULONG($0000009F);

//
// MessageId: INTERNAL_POWER_ERROR
//
// MessageText:
//
//  INTERNAL_POWER_ERROR
//

  INTERNAL_POWER_ERROR = ULONG($000000A0);

//
// MessageId: PCI_BUS_DRIVER_INTERNAL
//
// MessageText:
//
//  Inconsistency detected in the PCI Bus driver's internal structures.
//

  PCI_BUS_DRIVER_INTERNAL = ULONG($000000A1);

//
// MessageId: MEMORY_IMAGE_CORRUPT
//
// MessageText:
//
//  A CRC check on the memory range has failed
//

  MEMORY_IMAGE_CORRUPT = ULONG($000000A2);

//
// MessageId: ACPI_DRIVER_INTERNAL
//
// MessageText:
//
//  ACPI_DRIVER_INTERNAL
//

  ACPI_DRIVER_INTERNAL = ULONG($000000A3);

//
// MessageId: CNSS_FILE_SYSTEM_FILTER
//
// MessageText:
//
//  Internal inconsistency while representing
//  Ntfs Structured Storage as a DOCFILE.
//

  CNSS_FILE_SYSTEM_FILTER = ULONG($000000A4);

//
// MessageId: ACPI_BIOS_ERROR
//
// MessageText:
//
//  The ACPI BIOS in this system is not fully compliant with the ACPI 
//  specification. Please read the README.TXT for possible workarounds.  You
//  can also contact your system's manufacturer for an updated BIOS, or visit
//  http://www.hardware-update.com to see if a new BIOS is available.  
//

  ACPI_BIOS_ERROR = ULONG($000000A5);

//
// MessageId: FP_EMULATION_ERROR
//
// MessageText:
//
//  FP_EMULATION_ERROR
//

  FP_EMULATION_ERROR = ULONG($000000A6);

//
// MessageId: BAD_EXHANDLE
//
// MessageText:
//
//  BAD_EXHANDLE
//

  BAD_EXHANDLE = ULONG($000000A7);

//
// MessageId: BOOTING_IN_SAFEMODE_MINIMAL
//
// MessageText:
//
//  The system is booting in safemode - Minimal Services
//

  BOOTING_IN_SAFEMODE_MINIMAL = ULONG($000000A8);

//
// MessageId: BOOTING_IN_SAFEMODE_NETWORK
//
// MessageText:
//
//  The system is booting in safemode - Minimal Services with Network
//

  BOOTING_IN_SAFEMODE_NETWORK = ULONG($000000A9);

//
// MessageId: BOOTING_IN_SAFEMODE_DSREPAIR
//
// MessageText:
//
//  The system is booting in safemode - Directory Services Repair
//

  BOOTING_IN_SAFEMODE_DSREPAIR = ULONG($000000AA);

//
// MessageId: SESSION_HAS_VALID_POOL_ON_EXIT
//
// MessageText:
//
//  SESSION_HAS_VALID_POOL_ON_EXIT
//

  SESSION_HAS_VALID_POOL_ON_EXIT = ULONG($000000AB);

//
// MessageId: HAL_MEMORY_ALLOCATION
//
// MessageText:
//
//  Allocate from NonPaged Pool failed for a HAL critical allocation.
//

  HAL_MEMORY_ALLOCATION = ULONG($000000AC);

//
// MessageId: BUGCODE_PSS_MESSAGE_A
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Check to make sure any new hardware or software is properly installed.
//  If this is a new installation, ask your hardware or software manufacturer
//  for any Windows 2000 updates you might need.
//  
//  If problems continue, disable or remove any newly installed hardware
//  or software. Disable BIOS memory options such as caching or shadowing.
//  Check your hard drive to make sure it is properly configured and
//  terminated. If you need to use Safe Mode to remove or disable components,
//  restart your computer, press F8 to select Advanced Startup Options,
//  and then select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_A = ULONG($000000AD);

//
// MessageId: BUGCODE_PSS_MESSAGE_1E
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Check to be sure you have adequate disk space. If a driver is
//  identified in the Stop message, disable the driver or check
//  with the manufacturer for driver updates. Try changing video
//  adapters.
//  
//  Check with your hardware vendor for any BIOS updates. Disable
//  BIOS memory options such as caching or shadowing. If you need
//  to use Safe Mode to remove or disable components, restart your
//  computer, press F8 to select Advanced Startup Options, and then
//  select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_1E = ULONG($000000AE);

//
// MessageId: BUGCODE_PSS_MESSAGE_23
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Disable or uninstall any anti-virus, disk defragmentation
//  or backup utilities. Check your hard drive configuration,
//  and check for any updated drivers. Run CHKDSK /F to check
//  for hard drive corruption, and then restart your computer.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_23 = ULONG($000000AF);

//
// MessageId: BUGCODE_PSS_MESSAGE_2E
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Run system diagnostics supplied by your hardware manufacturer.
//  In particular, run a memory check, and check for faulty or
//  mismatched memory. Try changing video adapters.
//  
//  Check with your hardware vendor for any BIOS updates. Disable
//  BIOS memory options such as caching or shadowing. If you need
//  to use Safe Mode to remove or disable components, restart your
//  computer, press F8 to select Advanced Startup Options, and then
//  select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_2E = ULONG($000000B0);

//
// MessageId: BUGCODE_PSS_MESSAGE_3F
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Remove any recently installed software including backup
//  utilities or disk-intensive applications.
//  
//  If you need to use Safe Mode to remove or disable components,
//  restart your computer, press F8 to select Advanced Startup
//  Options, and then select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_3F = ULONG($000000B1);

//
// MessageId: BUGCODE_PSS_MESSAGE_7B
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Check for viruses on your computer. Remove any newly installed
//  hard drives or hard drive controllers. Check your hard drive
//  to make sure it is properly configured and terminated.
//  Run CHKDSK /F to check for hard drive corruption, and then
//  restart your computer.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_7B = ULONG($000000B2);

//
// MessageId: BUGCODE_PSS_MESSAGE_7F
//
// MessageText:
//
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Run a system diagnostic utility supplied by your hardware manufacturer.
//  In particular, run a memory check, and check for faulty or mismatched
//  memory. Try changing video adapters.
//  
//  Disable or remove any newly installed hardware and drivers. Disable or
//  remove any newly installed software. If you need to use Safe Mode to
//  remove or disable components, restart your computer, press F8 to select
//  Advanced Startup Options, and then select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  BUGCODE_PSS_MESSAGE_7F = ULONG($000000B3);

//
// MessageId: VIDEO_DRIVER_INIT_FAILURE
//
// MessageText:
//
//  The video driver failed to initialize
//

  VIDEO_DRIVER_INIT_FAILURE = ULONG($000000B4);

//
// MessageId: BOOTLOG_LOADED
//
// MessageText:
//
//  Loaded driver
//

  BOOTLOG_LOADED = ULONG($000000B5);

//
// MessageId: BOOTLOG_NOT_LOADED
//
// MessageText:
//
//  Did not load driver
//

  BOOTLOG_NOT_LOADED = ULONG($000000B6);

//
// MessageId: BOOTLOG_ENABLED
//
// MessageText:
//
//  Boot Logging Enabled
//

  BOOTLOG_ENABLED = ULONG($000000B7);

//
// MessageId: ATTEMPTED_SWITCH_FROM_DPC
//
// MessageText:
//
//  A wait operation, attach process, or yield was attempted from a DPC routine.
//

  ATTEMPTED_SWITCH_FROM_DPC = ULONG($000000B8);

//
// MessageId: CHIPSET_DETECTED_ERROR
//
// MessageText:
//
//  A parity error in the system memory or I/O system was detected.
//

  CHIPSET_DETECTED_ERROR = ULONG($000000B9);

//
// MessageId: SESSION_HAS_VALID_VIEWS_ON_EXIT
//
// MessageText:
//
//  SESSION_HAS_VALID_VIEWS_ON_EXIT
//

  SESSION_HAS_VALID_VIEWS_ON_EXIT = ULONG($000000BA);

//
// MessageId: NETWORK_BOOT_INITIALIZATION_FAILED
//
// MessageText:
//
//  An initialization failure occurred while attempting to boot from the network.
//

  NETWORK_BOOT_INITIALIZATION_FAILED = ULONG($000000BB);

//
// MessageId: NETWORK_BOOT_DUPLICATE_ADDRESS
//
// MessageText:
//
//  A duplicate IP address was assigned to this machine while attempting to
//  boot from the network.
//

  NETWORK_BOOT_DUPLICATE_ADDRESS = ULONG($000000BC);

//
// MessageId: INVALID_HIBERNATED_STATE
//
// MessageText:
//
//  The hibernated memory image does not match the current hardware configuration.
//

  INVALID_HIBERNATED_STATE = ULONG($000000BD);

//
// MessageId: ATTEMPTED_WRITE_TO_READONLY_MEMORY
//
// MessageText:
//
//  An attempt was made to write to read-only memory.
//

  ATTEMPTED_WRITE_TO_READONLY_MEMORY = ULONG($000000BE);

//
// MessageId: MUTEX_ALREADY_OWNED
//
// MessageText:
//
//  MUTEX_ALREADY_OWNED
//

  MUTEX_ALREADY_OWNED = ULONG($000000BF);

//
// MessageId: PCI_CONFIG_SPACE_ACCESS_FAILURE
//
// MessageText:
//
//  An attempt to access PCI configuration space failed.
//

  PCI_CONFIG_SPACE_ACCESS_FAILURE = ULONG($000000C0);

//
// MessageId: SPECIAL_POOL_DETECTED_MEMORY_CORRUPTION
//
// MessageText:
//
//  SPECIAL_POOL_DETECTED_MEMORY_CORRUPTION
//

  SPECIAL_POOL_DETECTED_MEMORY_CORRUPTION = ULONG($000000C1);

//
// MessageId: BAD_POOL_CALLER
//
// MessageText:
//
//  BAD_POOL_CALLER
//

  BAD_POOL_CALLER = ULONG($000000C2);

//
// MessageId: BUGCODE_PSS_MESSAGE_SIGNATURE
//
// MessageText:
//
//  
//  A system file that is owned by Windows 2000 was replaced by an application
//  running on your system.  The operating system detected this and tried to
//  verify the validity of the file's signature.  The operating system found that
//  the file signature is not valid and put the original, correct file back
//  so that your operating system will continue to function properly.
//

  BUGCODE_PSS_MESSAGE_SIGNATURE = ULONG($000000C3);

//
// MessageId: DRIVER_VERIFIER_DETECTED_VIOLATION
//
// MessageText:
//
//  
//  A device driver attempting to corrupt the system has been caught.
//  The faulty driver currently on the kernel stack must be replaced
//  with a working version.
//

  DRIVER_VERIFIER_DETECTED_VIOLATION = ULONG($000000C4);

//
// MessageId: DRIVER_CORRUPTED_EXPOOL
//
// MessageText:
//
//  
//  A device driver has corrupted the executive memory pool.
//  
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Check to make sure any new hardware or software is properly installed.
//  If this is a new installation, ask your hardware or software manufacturer
//  for any Windows 2000 updates you might need.
//  
//  Run the driver verifier against any new (or suspect) drivers.
//  If that doesn't reveal the corrupting driver, try enabling special pool.
//  Both of these features are intended to catch the corruption at an earlier
//  point where the offending driver can be identified.
//  
//  If you need to use Safe Mode to remove or disable components,
//  restart your computer, press F8 to select Advanced Startup Options,
//  and then select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  DRIVER_CORRUPTED_EXPOOL = ULONG($000000C5);

//
// MessageId: DRIVER_CAUGHT_MODIFYING_FREED_POOL
//
// MessageText:
//
//  
//  A device driver attempting to corrupt the system has been caught.
//  The faulty driver currently on the kernel stack must be replaced
//  with a working version.
//

  DRIVER_CAUGHT_MODIFYING_FREED_POOL = ULONG($000000C6);

//
// MessageId: TIMER_OR_DPC_INVALID
//
// MessageText:
//
//  
//  A kernel timer or DPC was found in memory which must not contain such
//  items.  Usually this is memory being freed.  This is usually caused by
//  a device driver that has not cleaned up properly before freeing memory.
//

  TIMER_OR_DPC_INVALID = ULONG($000000C7);

//
// MessageId: IRQL_UNEXPECTED_VALUE
//
// MessageText:
//
//  
//  The processor's IRQL is not valid for the currently executing context.
//  This is a software error condition and is usually caused by a device
//  driver changing IRQL and not restoring it to its previous value when
//  it has finished its task.
//

  IRQL_UNEXPECTED_VALUE = ULONG($000000C8);

//
// MessageId: DRIVER_VERIFIER_IOMANAGER_VIOLATION
//
// MessageText:
//
//  
//  The IO manager has detected a violation by a driver that is being verified.
//  The faulty driver that is being verified must be debugged and
//  replaced with a working version.
//

  DRIVER_VERIFIER_IOMANAGER_VIOLATION = ULONG($000000C9);

//
// MessageId: PNP_DETECTED_FATAL_ERROR
//
// MessageText:
//
//  
//  Plug and Play detected an error most likely caused by a faulty driver.
//

  PNP_DETECTED_FATAL_ERROR = ULONG($000000CA);

//
// MessageId: DRIVER_LEFT_LOCKED_PAGES_IN_PROCESS
//
// MessageText:
//
//  DRIVER_LEFT_LOCKED_PAGES_IN_PROCESS
//

  DRIVER_LEFT_LOCKED_PAGES_IN_PROCESS = ULONG($000000CB);

//
// MessageId: PAGE_FAULT_IN_FREED_SPECIAL_POOL
//
// MessageText:
//
//  
//  The system is attempting to access memory after it has been freed.
//  This usually indicates a system-driver synchronization issue.
//

  PAGE_FAULT_IN_FREED_SPECIAL_POOL = ULONG($000000CC);

//
// MessageId: PAGE_FAULT_BEYOND_END_OF_ALLOCATION
//
// MessageText:
//
//  
//  The system is attempting to access memory beyond the end of the allocation.
//  This usually indicates a system-driver synchronization issue.
//

  PAGE_FAULT_BEYOND_END_OF_ALLOCATION = ULONG($000000CD);

//
// MessageId: DRIVER_UNLOADED_WITHOUT_CANCELLING_PENDING_OPERATIONS
//
// MessageText:
//
//  DRIVER_UNLOADED_WITHOUT_CANCELLING_PENDING_OPERATIONS
//

  DRIVER_UNLOADED_WITHOUT_CANCELLING_PENDING_OPERATIONS = ULONG($000000CE);

//
// MessageId: TERMINAL_SERVER_DRIVER_MADE_INCORRECT_MEMORY_REFERENCE
//
// MessageText:
//
//  TERMINAL_SERVER_DRIVER_MADE_INCORRECT_MEMORY_REFERENCE
//

  TERMINAL_SERVER_DRIVER_MADE_INCORRECT_MEMORY_REFERENCE = ULONG($000000CF);

//
// MessageId: DRIVER_CORRUPTED_MMPOOL
//
// MessageText:
//
//  
//  A device driver has corrupted the system memory management pool.
//  
//  If this is the first time you've seen this Stop error screen,
//  restart your computer. If this screen appears again, follow
//  these steps:
//  
//  Check to make sure any new hardware or software is properly installed.
//  If this is a new installation, ask your hardware or software manufacturer
//  for any Windows 2000 updates you might need.
//  
//  Run the driver verifier against any new (or suspect) drivers.
//  If that doesn't reveal the corrupting driver, try enabling special pool.
//  Both of these features are intended to catch the corruption at an earlier
//  point where the offending driver can be identified.
//  
//  If you need to use Safe Mode to remove or disable components,
//  restart your computer, press F8 to select Advanced Startup Options,
//  and then select Safe Mode.
//  
//  Refer to your Getting Started manual for more information on
//  troubleshooting Stop errors.
//

  DRIVER_CORRUPTED_MMPOOL = ULONG($000000D0);

//
// MessageId: DRIVER_IRQL_NOT_LESS_OR_EQUAL
//
// MessageText:
//
//  DRIVER_IRQL_NOT_LESS_OR_EQUAL
//

  DRIVER_IRQL_NOT_LESS_OR_EQUAL = ULONG($000000D1);

//
// MessageId: BUGCODE_ID_DRIVER
//
// MessageText:
//
//  This driver may be at fault :
//

  BUGCODE_ID_DRIVER = ULONG($000000D2);

//
// MessageId: DRIVER_PORTION_MUST_BE_NONPAGED
//
// MessageText:
//
//  The driver mistakenly marked a part of it's image pagable instead of nonpagable.
//

  DRIVER_PORTION_MUST_BE_NONPAGED = ULONG($000000D3);

//
// MessageId: SYSTEM_SCAN_AT_RAISED_IRQL_CAUGHT_IMPROPER_DRIVER_UNLOAD
//
// MessageText:
//
//  The driver unloaded without cancelling pending operations.
//

  SYSTEM_SCAN_AT_RAISED_IRQL_CAUGHT_IMPROPER_DRIVER_UNLOAD = ULONG($000000D4);

//
// MessageId: DRIVER_PAGE_FAULT_IN_FREED_SPECIAL_POOL
//
// MessageText:
//
//  
//  The driver is attempting to access memory after it has been freed.
//

  DRIVER_PAGE_FAULT_IN_FREED_SPECIAL_POOL = ULONG($000000D5);

//
// MessageId: DRIVER_PAGE_FAULT_BEYOND_END_OF_ALLOCATION
//
// MessageText:
//
//  
//  The driver is attempting to access memory beyond the end of the allocation.
//

  DRIVER_PAGE_FAULT_BEYOND_END_OF_ALLOCATION = ULONG($000000D6);

//
// MessageId: DRIVER_UNMAPPING_INVALID_VIEW
//
// MessageText:
//
//  
//  The driver is attempting to unmap an invalid memory address.
//

  DRIVER_UNMAPPING_INVALID_VIEW = ULONG($000000D7);

//
// MessageId: DRIVER_USED_EXCESSIVE_PTES
//
// MessageText:
//
//  
//  The driver has used an excessive number of system PTEs.
//

  DRIVER_USED_EXCESSIVE_PTES = ULONG($000000D8);

//
// MessageId: LOCKED_PAGES_TRACKER_CORRUPTION
//
// MessageText:
//
//  
//  The driver is corrupting the locked pages tracking structures.
//

  LOCKED_PAGES_TRACKER_CORRUPTION = ULONG($000000D9);

//
// MessageId: SYSTEM_PTE_MISUSE
//
// MessageText:
//
//  
//  The driver is mismanaging system PTEs.
//

  SYSTEM_PTE_MISUSE = ULONG($000000DA);

//
// MessageId: DRIVER_CORRUPTED_SYSPTES
//
// MessageText:
//
//  
//  A driver has corrupted the memory management system PTEs.
//

  DRIVER_CORRUPTED_SYSPTES = ULONG($000000DB);

//
// MessageId: DRIVER_INVALID_STACK_ACCESS
//
// MessageText:
//
//  
//  A driver accessed a stack address that lies below the current stack pointer
//  of the stack's thread.
//

  DRIVER_INVALID_STACK_ACCESS = ULONG($000000DC);

//
// MessageId: BUGCODE_PSS_MESSAGE_A5
//
// MessageText:
//
//  
//  The BIOS in this system is not fully ACPI compliant.  Please contact your
//  system vendor or visit http://www.hardware-update.com for an updated BIOS.  
//  If you are unable to obtain an updated BIOS or the latest BIOS supplied by 
//  your vendor is not ACPI compliant, you can turn off ACPI mode during text 
//  mode setup.  To do this, simply press the F7 key when you are prompted to 
//  install storage drivers.  The system will not notify you that the F7 key 
//  was pressed - it will silently disable ACPI and allow you to continue 
//  your installation.
//

  BUGCODE_PSS_MESSAGE_A5 = ULONG($000000DD);

//
// MessageId: POOL_CORRUPTION_IN_FILE_AREA
//
// MessageText:
//
//  
//  A driver corrupted pool memory used for holding pages destined for disk.
//

  POOL_CORRUPTION_IN_FILE_AREA = ULONG($000000DE);

//
// MessageId: HARDWARE_PROFILE_UNDOCKED_STRING
//
// MessageText:
//
//  Undocked Profile
//

  HARDWARE_PROFILE_UNDOCKED_STRING = ULONG($40010001);

//
// MessageId: HARDWARE_PROFILE_DOCKED_STRING
//
// MessageText:
//
//  Docked Profile
//

  HARDWARE_PROFILE_DOCKED_STRING = ULONG($40010002);

//
// MessageId: HARDWARE_PROFILE_UNKNOWN_STRING
//
// MessageText:
//
//  Profile
//

  HARDWARE_PROFILE_UNKNOWN_STRING = ULONG($40010003);

//
// MessageId: IMPERSONATING_WORKER_THREAD
//
// MessageText:
//
//  
//  A worker thread is impersonating another process. The work item forgot to
//  disable impersonation before it returned.
//

  IMPERSONATING_WORKER_THREAD = ULONG($000000DF);

//
// MessageId: ACPI_BIOS_FATAL_ERROR
//
// MessageText:
//
//  
//  Your computer (BIOS) has reported that a component in your system is faulty and
//  has prevented Windows from operating.  You can determine which component is
//  faulty by running the diagnostic disk or tool that came with your computer.
//  
//  If you do not have this tool, you must contact your system vendor and report
//  this error message to them.  They will be able to assist you in correcting this
//  hardware problem thereby allowing Windows to operate.
//

  ACPI_BIOS_FATAL_ERROR = ULONG($000000E0);

//
// MessageId: WORKER_THREAD_RETURNED_AT_BAD_IRQL
//
// MessageText:
//
//  WORKER_THREAD_RETURNED_AT_BAD_IRQL
//

  WORKER_THREAD_RETURNED_AT_BAD_IRQL = ULONG($000000E1);

//
// MessageId: MANUALLY_INITIATED_CRASH
//
// MessageText:
//
//  
//  The end-user manually generated the crashdump.
//

  MANUALLY_INITIATED_CRASH = ULONG($000000E2);

//
// MessageId: RESOURCE_NOT_OWNED
//
// MessageText:
//
//  
//  A thread tried to release a resource it did not own.
//

  RESOURCE_NOT_OWNED = ULONG($000000E3);

//
// MessageId: WORKER_INVALID
//
// MessageText:
//
//  
//  A executive worker item was found in memory which must not contain such
//  items.  Usually this is memory being freed.  This is usually caused by
//  a device driver that has not cleaned up properly before freeing memory.
//

  WORKER_INVALID = ULONG($000000E4);

implementation

end.
