{******************************************************************************}
{                                                       	               }
{ Windows Base Services API interface Unit for Object Pascal                   }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winbase.h, released August 2001. The original Pascal   }
{ code is: WinBase.pas, released December 2000. The initial developer of the   }
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

unit JwaWinBase;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinBase.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

{$STACKFRAMES ON}

interface

uses
  {$IFDEF USE_DELPHI_TYPES}Windows,{$ENDIF}
  JwaNtStatus, JwaWinNT, JwaWinType;

const
  INVALID_HANDLE_VALUE     = HANDLE(-1);
  INVALID_FILE_SIZE        = DWORD($FFFFFFFF);
  INVALID_SET_FILE_POINTER = DWORD(-1);
  INVALID_FILE_ATTRIBUTES  = DWORD(-1);

  FILE_BEGIN   = 0;
  FILE_CURRENT = 1;
  FILE_END     = 2;

  TIME_ZONE_ID_INVALID = DWORD($FFFFFFFF);

  WAIT_FAILED   = DWORD($FFFFFFFF);
  WAIT_OBJECT_0 = STATUS_WAIT_0 + 0;

  WAIT_ABANDONED   = STATUS_ABANDONED_WAIT_0 + 0;
  WAIT_ABANDONED_0 = STATUS_ABANDONED_WAIT_0 + 0;

  WAIT_IO_COMPLETION                 = STATUS_USER_APC;
  STILL_ACTIVE                       = STATUS_PENDING;
  EXCEPTION_ACCESS_VIOLATION         = STATUS_ACCESS_VIOLATION;
  EXCEPTION_DATATYPE_MISALIGNMENT    = STATUS_DATATYPE_MISALIGNMENT;
  EXCEPTION_BREAKPOINT               = STATUS_BREAKPOINT;
  EXCEPTION_SINGLE_STEP              = STATUS_SINGLE_STEP;
  EXCEPTION_ARRAY_BOUNDS_EXCEEDED    = STATUS_ARRAY_BOUNDS_EXCEEDED;
  EXCEPTION_FLT_DENORMAL_OPERAND     = STATUS_FLOAT_DENORMAL_OPERAND;
  EXCEPTION_FLT_DIVIDE_BY_ZERO       = STATUS_FLOAT_DIVIDE_BY_ZERO;
  EXCEPTION_FLT_INEXACT_RESULT       = STATUS_FLOAT_INEXACT_RESULT;
  EXCEPTION_FLT_INVALID_OPERATION    = STATUS_FLOAT_INVALID_OPERATION;
  EXCEPTION_FLT_OVERFLOW             = STATUS_FLOAT_OVERFLOW;
  EXCEPTION_FLT_STACK_CHECK          = STATUS_FLOAT_STACK_CHECK;
  EXCEPTION_FLT_UNDERFLOW            = STATUS_FLOAT_UNDERFLOW;
  EXCEPTION_INT_DIVIDE_BY_ZERO       = STATUS_INTEGER_DIVIDE_BY_ZERO;
  EXCEPTION_INT_OVERFLOW             = STATUS_INTEGER_OVERFLOW;
  EXCEPTION_PRIV_INSTRUCTION         = STATUS_PRIVILEGED_INSTRUCTION;
  EXCEPTION_IN_PAGE_ERROR            = STATUS_IN_PAGE_ERROR;
  EXCEPTION_ILLEGAL_INSTRUCTION      = STATUS_ILLEGAL_INSTRUCTION;
  EXCEPTION_NONCONTINUABLE_EXCEPTION = STATUS_NONCONTINUABLE_EXCEPTION;
  EXCEPTION_STACK_OVERFLOW           = STATUS_STACK_OVERFLOW;
  EXCEPTION_INVALID_DISPOSITION      = STATUS_INVALID_DISPOSITION;
  EXCEPTION_GUARD_PAGE               = STATUS_GUARD_PAGE_VIOLATION;
  EXCEPTION_INVALID_HANDLE           = STATUS_INVALID_HANDLE;
  EXCEPTION_POSSIBLE_DEADLOCK        = STATUS_POSSIBLE_DEADLOCK;
  CONTROL_C_EXIT                     = STATUS_CONTROL_C_EXIT;

procedure MoveMemory(Destination, Source: PVOID; Length: SIZE_T);
procedure CopyMemory(Destination, Source: PVOID; Length: SIZE_T);
procedure FillMemory(Destination: PVOID; Length: SIZE_T; Fill: BYTE);
procedure ZeroMemory(Destination: PVOID; Length: SIZE_T);
//#define SecureZeroMemory RtlSecureZeroMemory TODO

//
// File creation flags must start at the high end since they
// are combined with the attributes
//

const
  FILE_FLAG_WRITE_THROUGH      = DWORD($80000000);
  FILE_FLAG_OVERLAPPED         = $40000000;
  FILE_FLAG_NO_BUFFERING       = $20000000;
  FILE_FLAG_RANDOM_ACCESS      = $10000000;
  FILE_FLAG_SEQUENTIAL_SCAN    = $08000000;
  FILE_FLAG_DELETE_ON_CLOSE    = $04000000;
  FILE_FLAG_BACKUP_SEMANTICS   = $02000000;
  FILE_FLAG_POSIX_SEMANTICS    = $01000000;
  FILE_FLAG_OPEN_REPARSE_POINT = $00200000;
  FILE_FLAG_OPEN_NO_RECALL     = $00100000;
  FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;

  CREATE_NEW        = 1;
  CREATE_ALWAYS     = 2;
  OPEN_EXISTING     = 3;
  OPEN_ALWAYS       = 4;
  TRUNCATE_EXISTING = 5;

//
// Define possible return codes from the CopyFileEx callback routine
//

  PROGRESS_CONTINUE = 0;
  PROGRESS_CANCEL   = 1;
  PROGRESS_STOP     = 2;
  PROGRESS_QUIET    = 3;

//
// Define CopyFileEx callback routine state change values
//

  CALLBACK_CHUNK_FINISHED = $00000000;
  CALLBACK_STREAM_SWITCH  = $00000001;

//
// Define CopyFileEx option flags
//

  COPY_FILE_FAIL_IF_EXISTS        = $00000001;
  COPY_FILE_RESTARTABLE           = $00000002;
  COPY_FILE_OPEN_SOURCE_FOR_WRITE = $00000004;
  COPY_FILE_ALLOW_DECRYPTED_DESTINATION = $00000008;

//
// Define ReplaceFile option flags
//

  REPLACEFILE_WRITE_THROUGH       = $00000001;
  REPLACEFILE_IGNORE_MERGE_ERRORS = $00000002;

//
// Define the NamedPipe definitions
//


//
// Define the dwOpenMode values for CreateNamedPipe
//

  PIPE_ACCESS_INBOUND  = $00000001;
  PIPE_ACCESS_OUTBOUND = $00000002;
  PIPE_ACCESS_DUPLEX   = $00000003;

//
// Define the Named Pipe End flags for GetNamedPipeInfo
//

  PIPE_CLIENT_END = $00000000;
  PIPE_SERVER_END = $00000001;

//
// Define the dwPipeMode values for CreateNamedPipe
//

  PIPE_WAIT             = $00000000;
  PIPE_NOWAIT           = $00000001;
  PIPE_READMODE_BYTE    = $00000000;
  PIPE_READMODE_MESSAGE = $00000002;
  PIPE_TYPE_BYTE        = $00000000;
  PIPE_TYPE_MESSAGE     = $00000004;

//
// Define the well known values for CreateNamedPipe nMaxInstances
//

  PIPE_UNLIMITED_INSTANCES = 255;

//
// Define the Security Quality of Service bits to be passed
// into CreateFile
//

  SECURITY_ANONYMOUS      = (Ord(SecurityAnonymous) shl 16);
  SECURITY_IDENTIFICATION = (Ord(SecurityIdentification) shl 16);
  SECURITY_IMPERSONATION  = (Ord(SecurityImpersonation) shl 16);
  SECURITY_DELEGATION     = (Ord(SecurityDelegation) shl 16);

  SECURITY_CONTEXT_TRACKING = $00040000;
  SECURITY_EFFECTIVE_ONLY   = $00080000;

  SECURITY_SQOS_PRESENT     = $00100000;
  SECURITY_VALID_SQOS_FLAGS = $001F0000;

//
//  File structures
//

type
  LPOVERLAPPED = ^OVERLAPPED;
  _OVERLAPPED = record
    Internal: ULONG_PTR;
    InternalHigh: ULONG_PTR;
    Union: record
    case Integer of
      0: (
        Offset: DWORD;
        OffsetHigh: DWORD);
      1: (
        Pointer: PVOID);
    end;
    hEvent: HANDLE;
  end;
  OVERLAPPED = _OVERLAPPED;
  TOverlapped = OVERLAPPED;
  POverlapped = LPOVERLAPPED;

  PSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
  _SECURITY_ATTRIBUTES = record
    nLength: DWORD;
    lpSecurityDescriptor: LPVOID;
    bInheritHandle: BOOL;
  end;
  SECURITY_ATTRIBUTES = _SECURITY_ATTRIBUTES;
  LPSECURITY_ATTRIBUTES = ^SECURITY_ATTRIBUTES;
  TSecurityAttributes = SECURITY_ATTRIBUTES;
  PSecurityAttributes = PSECURITY_ATTRIBUTES;

  PPROCESS_INFORMATION = ^PROCESS_INFORMATION;
  _PROCESS_INFORMATION = record
    hProcess: HANDLE;
    hThread: HANDLE;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
  end;
  PROCESS_INFORMATION = _PROCESS_INFORMATION;
  LPPROCESS_INFORMATION = ^PROCESS_INFORMATION;
  TProcessInformation = PROCESS_INFORMATION;
  PProcessInformation = PPROCESS_INFORMATION;

//
//  File System time stamps are represented with the following structure:
//

  {$IFNDEF _FILETIME_}
  {$DEFINE _FILETIME_}
  LPFILETIME = ^FILETIME;
  _FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;
  FILETIME = _FILETIME;
  TFileTime = FILETIME;
  PFileTime = LPFILETIME;
  {$ENDIF}

//
// System time is represented with the following structure:
//

  LPSYSTEMTIME = ^SYSTEMTIME;
{$IFDEF USE_DELPHI_TYPES}
  _SYSTEMTIME = Windows._SYSTEMTIME;
  SYSTEMTIME = Windows.SYSTEMTIME;
  TSystemTime = Windows.TSystemTime;
  PSystemtime = Windows.PSystemTime;
{$ELSE}
  _SYSTEMTIME = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  SYSTEMTIME = _SYSTEMTIME;
  TSystemTime = SYSTEMTIME;
  PSystemTime = LPSYSTEMTIME;
{$ENDIF}

  PTHREAD_START_ROUTINE = function (lpThreadParameter: LPVOID): DWORD; stdcall;
  LPTHREAD_START_ROUTINE = PTHREAD_START_ROUTINE;
  TThreadStartRoutine = PTHREAD_START_ROUTINE;

  PFIBER_START_ROUTINE = procedure (lpFiberParameter: LPVOID); stdcall;
  LPFIBER_START_ROUTINE = PFIBER_START_ROUTINE;
  TFiberStartRoutine = PFIBER_START_ROUTINE;

  CRITICAL_SECTION = RTL_CRITICAL_SECTION;
  PCRITICAL_SECTION = PRTL_CRITICAL_SECTION;
  LPCRITICAL_SECTION = PRTL_CRITICAL_SECTION;
  TCriticalSection = CRITICAL_SECTION;
  PCriticalSection = PCRITICAL_SECTION;

  CRITICAL_SECTION_DEBUG = RTL_CRITICAL_SECTION_DEBUG;
  PCRITICAL_SECTION_DEBUG = PRTL_CRITICAL_SECTION_DEBUG;
  LPCRITICAL_SECTION_DEBUG = PRTL_CRITICAL_SECTION_DEBUG;
  TCriticalSectionDebug = CRITICAL_SECTION_DEBUG;
  PCriticalSectionDebug = PCRITICAL_SECTION_DEBUG;  

  LPLDT_ENTRY = PLDT_ENTRY;
  PLdtEntry = LPLDT_ENTRY;

const
  MUTEX_MODIFY_STATE = MUTANT_QUERY_STATE;
  MUTEX_ALL_ACCESS   = MUTANT_ALL_ACCESS;

//
// Serial provider type.
//

  SP_SERIALCOMM = DWORD($00000001);

//
// Provider SubTypes
//

  PST_UNSPECIFIED    = DWORD($00000000);
  PST_RS232          = DWORD($00000001);
  PST_PARALLELPORT   = DWORD($00000002);
  PST_RS422          = DWORD($00000003);
  PST_RS423          = DWORD($00000004);
  PST_RS449          = DWORD($00000005);
  PST_MODEM          = DWORD($00000006);
  PST_FAX            = DWORD($00000021);
  PST_SCANNER        = DWORD($00000022);
  PST_NETWORK_BRIDGE = DWORD($00000100);
  PST_LAT            = DWORD($00000101);
  PST_TCPIP_TELNET   = DWORD($00000102);
  PST_X25            = DWORD($00000103);

//
// Provider capabilities flags.
//

  PCF_DTRDSR        = DWORD($0001);
  PCF_RTSCTS        = DWORD($0002);
  PCF_RLSD          = DWORD($0004);
  PCF_PARITY_CHECK  = DWORD($0008);
  PCF_XONXOFF       = DWORD($0010);
  PCF_SETXCHAR      = DWORD($0020);
  PCF_TOTALTIMEOUTS = DWORD($0040);
  PCF_INTTIMEOUTS   = DWORD($0080);
  PCF_SPECIALCHARS  = DWORD($0100);
  PCF_16BITMODE     = DWORD($0200);

//
// Comm provider settable parameters.
//

  SP_PARITY       = DWORD($0001);
  SP_BAUD         = DWORD($0002);
  SP_DATABITS     = DWORD($0004);
  SP_STOPBITS     = DWORD($0008);
  SP_HANDSHAKING  = DWORD($0010);
  SP_PARITY_CHECK = DWORD($0020);
  SP_RLSD         = DWORD($0040);

//
// Settable baud rates in the provider.
//

  BAUD_075    = DWORD($00000001);
  BAUD_110    = DWORD($00000002);
  BAUD_134_5  = DWORD($00000004);
  BAUD_150    = DWORD($00000008);
  BAUD_300    = DWORD($00000010);
  BAUD_600    = DWORD($00000020);
  BAUD_1200   = DWORD($00000040);
  BAUD_1800   = DWORD($00000080);
  BAUD_2400   = DWORD($00000100);
  BAUD_4800   = DWORD($00000200);
  BAUD_7200   = DWORD($00000400);
  BAUD_9600   = DWORD($00000800);
  BAUD_14400  = DWORD($00001000);
  BAUD_19200  = DWORD($00002000);
  BAUD_38400  = DWORD($00004000);
  BAUD_56K    = DWORD($00008000);
  BAUD_128K   = DWORD($00010000);
  BAUD_115200 = DWORD($00020000);
  BAUD_57600  = DWORD($00040000);
  BAUD_USER   = DWORD($10000000);

//
// Settable Data Bits
//

  DATABITS_5   = WORD($0001);
  DATABITS_6   = WORD($0002);
  DATABITS_7   = WORD($0004);
  DATABITS_8   = WORD($0008);
  DATABITS_16  = WORD($0010);
  DATABITS_16X = WORD($0020);

//
// Settable Stop and Parity bits.
//

  STOPBITS_10  = WORD($0001);
  STOPBITS_15  = WORD($0002);
  STOPBITS_20  = WORD($0004);
  PARITY_NONE  = WORD($0100);
  PARITY_ODD   = WORD($0200);
  PARITY_EVEN  = WORD($0400);
  PARITY_MARK  = WORD($0800);
  PARITY_SPACE = WORD($1000);

type
  LPCOMMPROP = ^COMMPROP;
  _COMMPROP = record
    wPacketLength: Word;
    wPacketVersion: Word;
    dwServiceMask: DWORD;
    dwReserved1: DWORD;
    dwMaxTxQueue: DWORD;
    dwMaxRxQueue: DWORD;
    dwMaxBaud: DWORD;
    dwProvSubType: DWORD;
    dwProvCapabilities: DWORD;
    dwSettableParams: DWORD;
    dwSettableBaud: DWORD;
    wSettableData: Word;
    wSettableStopParity: Word;
    dwCurrentTxQueue: DWORD;
    dwCurrentRxQueue: DWORD;
    dwProvSpec1: DWORD;
    dwProvSpec2: DWORD;
    wcProvChar: array [0..0] of WCHAR;
  end;
  COMMPROP = _COMMPROP;
  TCommProp = COMMPROP;
  PCommProp = LPCOMMPROP;

//
// Set dwProvSpec1 to COMMPROP_INITIALIZED to indicate that wPacketLength
// is valid before a call to GetCommProperties().
//

const
  COMMPROP_INITIALIZED = DWORD($E73CF52E);

//_COMSTAT Flags (bitfield)

  COMSTAT_CTS_HOLD   = 1 shl 0;
  COMSTAT_DSR_HOLD   = 1 shl 1;
  COMSTAT_RLSD_HOLD  = 1 shl 2;
  COMSTAT_XOFF_HOLD  = 1 shl 3;
  COMSTAT_XOFF_SENT  = 1 shl 4;
  COMSTAT_F_EOF      = 1 shl 5;
  COMSTAT_F_TXIM     = 1 shl 6;

type
  LPCOMSTAT = ^COMSTAT;
  _COMSTAT = record
    Flags: DWORD;
    cbInQue: DWORD;
    cbOutQue: DWORD;
  end;
  COMSTAT = _COMSTAT;
  TComstat = COMSTAT;
  PComstat = LPCOMSTAT;

//
// DTR Control Flow Values.
//

const
  DTR_CONTROL_DISABLE   = $00;
  DTR_CONTROL_ENABLE    = $01;
  DTR_CONTROL_HANDSHAKE = $02;

//
// RTS Control Flow Values
//

  RTS_CONTROL_DISABLE   = $00;
  RTS_CONTROL_ENABLE    = $01;
  RTS_CONTROL_HANDSHAKE = $02;
  RTS_CONTROL_TOGGLE    = $03;

// _DCB.Flags

const
  fDcbBinary = 1 shl 0;           // Binary Mode (skip EOF check)
  fDcbParity = 1 shl 1;           // Enable parity checking
  fDcbOutxCtsFlow = 1 shl 2;      // CTS handshaking on output
  fDcbOutxDsrFlow = 1 shl 3;      // DSR handshaking on output
  fDcbDtrControl = $0030;         // DTR Flow control
  fDcbDsrSensitivity = 1 shl 6;   // DSR Sensitivity
  fDcbTXContinueOnXoff = 1 shl 7; // Continue TX when Xoff sent
  fDcbOutX = 1 shl 8;             // Enable output X-ON/X-OFF
  fDcbInX = 1 shl 9;              // Enable input X-ON/X-OFF
  fDcbErrorChar = 1 shl 10;       // Enable Err Replacement
  fDcbNull = 1 shl 11;            // Enable Null stripping
  fDcbRtsControl = $3000;         // Rts Flow control
  fAbortOnError = 1 shl 14;       // Abort all reads and writes on Error

type
  LPDCB = ^DCB;
  _DCB = record
    DCBlength: DWORD;      // sizeof(DCB)
    BaudRate: DWORD;       // Baudrate at which running
    Flags: DWORD;          // See constants above
    wReserved: WORD;       // Not currently used
    XonLim: WORD;          // Transmit X-ON threshold
    XoffLim: WORD;         // Transmit X-OFF threshold
    ByteSize: BYTE;        // Number of bits/byte, 4-8
    Parity: BYTE;          // 0-4=None,Odd,Even,Mark,Space
    StopBits: BYTE;        // 0,1,2 = 1, 1.5, 2
    XonChar: Char;         // Tx and Rx X-ON character
    XoffChar: Char;        // Tx and Rx X-OFF character
    ErrorChar: Char;       // Error replacement char
    EofChar: Char;         // End of Input character
    EvtChar: Char;         // Received Event character
    wReserved1: WORD;      // Fill for now.
  end;
  DCB = _DCB;
  TDCB = DCB;
  PDCB = LPDCB;

  LPCOMMTIMEOUTS = ^COMMTIMEOUTS;
  _COMMTIMEOUTS = record
    ReadIntervalTimeout: DWORD;         // Maximum time between read chars.
    ReadTotalTimeoutMultiplier: DWORD;  // Multiplier of characters.
    ReadTotalTimeoutConstant: DWORD;    // Constant in milliseconds.
    WriteTotalTimeoutMultiplier: DWORD; // Multiplier of characters.
    WriteTotalTimeoutConstant: DWORD;   // Constant in milliseconds.
  end;
  COMMTIMEOUTS = _COMMTIMEOUTS;
  TCommTimeouts = COMMTIMEOUTS;
  PCommTimeouts = LPCOMMTIMEOUTS;

  LPCOMMCONFIG = ^COMMCONFIG;
  _COMMCONFIG = record
    dwSize: DWORD;            // Size of the entire struct
    wVersion: Word;           // version of the structure
    wReserved: Word;          // alignment
    dcb: DCB;                 // device control block
    dwProviderSubType: DWORD; // ordinal value for identifying
                              // provider-defined data structure format
    dwProviderOffset: DWORD;  // Specifies the offset of provider specific
                              // data field in bytes from the start
    dwProviderSize: DWORD;    // size of the provider-specific data field
    wcProviderData: array [0..0] of WCHAR; // provider-specific data
  end;
  COMMCONFIG = _COMMCONFIG;
  TCommConfig = COMMCONFIG;
  PCommConfig = LPCOMMCONFIG;

  LPSYSTEM_INFO = ^SYSTEM_INFO;
  _SYSTEM_INFO = record
    case Integer of
    0: (
      dwOemId: DWORD); // absolete, do not use
    1: (
      wProcessorArchitecture: WORD;
      wReserved: WORD;
      dwPageSize: DWORD;
      lpMinimumApplicationAddress: LPVOID;
      lpMaximumApplicationAddress: LPVOID;
      dwActiveProcessorMask: DWORD_PTR;
      dwNumberOfProcessors: DWORD;
      dwProcessorType: DWORD;
      dwAllocationGranularity: DWORD;
      wProcessorLevel: WORD;
      wProcessorRevision: WORD);
  end;
  SYSTEM_INFO = _SYSTEM_INFO;
  TSystemInfo = SYSTEM_INFO;
  PSystemInfo = LPSYSTEM_INFO;

//
//

function FreeModule(hLibModule: HMODULE): BOOL;
function MakeProcInstance(lpProc: FARPROC; hInstance: HINSTANCE): FARPROC;
procedure FreeProcInstance(lpProc: FARPROC);

// Global Memory Flags

const
  GMEM_FIXED          = $0000;
  GMEM_MOVEABLE       = $0002;
  GMEM_NOCOMPACT      = $0010;
  GMEM_NODISCARD      = $0020;
  GMEM_ZEROINIT       = $0040;
  GMEM_MODIFY         = $0080;
  GMEM_DISCARDABLE    = $0100;
  GMEM_NOT_BANKED     = $1000;
  GMEM_SHARE          = $2000;
  GMEM_DDESHARE       = $2000;
  GMEM_NOTIFY         = $4000;
  GMEM_LOWER          = GMEM_NOT_BANKED;
  GMEM_VALID_FLAGS    = $7F72;
  GMEM_INVALID_HANDLE = $8000;

  GHND = (GMEM_MOVEABLE or GMEM_ZEROINIT);
  GPTR = (GMEM_FIXED or GMEM_ZEROINIT);

function GlobalLRUNewest(h: HANDLE): HANDLE;
function GlobalLRUOldest(h: HANDLE): HANDLE;
function GlobalDiscard(h: HANDLE): HANDLE;

// Flags returned by GlobalFlags (in addition to GMEM_DISCARDABLE)

const
  GMEM_DISCARDED = $4000;
  GMEM_LOCKCOUNT = $00FF;

type
  LPMEMORYSTATUS = ^MEMORYSTATUS;
  _MEMORYSTATUS = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    dwTotalPhys: SIZE_T;
    dwAvailPhys: SIZE_T;
    dwTotalPageFile: SIZE_T;
    dwAvailPageFile: SIZE_T;
    dwTotalVirtual: SIZE_T;
    dwAvailVirtual: SIZE_T;
  end;
  MEMORYSTATUS = _MEMORYSTATUS;
  TMemoryStatus = MEMORYSTATUS;
  PMemoryStatus = LPMEMORYSTATUS;

// Local Memory Flags

const
  LMEM_FIXED          = $0000;
  LMEM_MOVEABLE       = $0002;
  LMEM_NOCOMPACT      = $0010;
  LMEM_NODISCARD      = $0020;
  LMEM_ZEROINIT       = $0040;
  LMEM_MODIFY         = $0080;
  LMEM_DISCARDABLE    = $0F00;
  LMEM_VALID_FLAGS    = $0F72;
  LMEM_INVALID_HANDLE = $8000;

  LHND = (LMEM_MOVEABLE or LMEM_ZEROINIT);
  LPTR = (LMEM_FIXED or LMEM_ZEROINIT);

  NONZEROLHND = (LMEM_MOVEABLE);
  NONZEROLPTR = (LMEM_FIXED);

function LocalDiscard(h: HLOCAL): HLOCAL;

// Flags returned by LocalFlags (in addition to LMEM_DISCARDABLE)

const
  LMEM_DISCARDED = $4000;
  LMEM_LOCKCOUNT = $00FF;

//
// dwCreationFlag values
//

  DEBUG_PROCESS           = $00000001;
  DEBUG_ONLY_THIS_PROCESS = $00000002;

  CREATE_SUSPENDED = $00000004;

  DETACHED_PROCESS = $00000008;

  CREATE_NEW_CONSOLE = $00000010;

  NORMAL_PRIORITY_CLASS   = $00000020;
  IDLE_PRIORITY_CLASS     = $00000040;
  HIGH_PRIORITY_CLASS     = $00000080;
  REALTIME_PRIORITY_CLASS = $00000100;

  CREATE_NEW_PROCESS_GROUP   = $00000200;
  CREATE_UNICODE_ENVIRONMENT = $00000400;

  CREATE_SEPARATE_WOW_VDM = $00000800;
  CREATE_SHARED_WOW_VDM   = $00001000;
  CREATE_FORCEDOS         = $00002000;

  BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
  STACK_SIZE_PARAM_IS_A_RESERVATION = $00010000;

  CREATE_BREAKAWAY_FROM_JOB = $01000000;
  CREATE_PRESERVE_CODE_AUTHZ_LEVEL = $02000000;

  CREATE_DEFAULT_ERROR_MODE = $04000000;
  CREATE_NO_WINDOW          = $08000000;

  PROFILE_USER   = $10000000;
  PROFILE_KERNEL = $20000000;
  PROFILE_SERVER = $40000000;

  CREATE_IGNORE_SYSTEM_DEFAULT = DWORD($80000000);

  THREAD_PRIORITY_LOWEST       = THREAD_BASE_PRIORITY_MIN;
  THREAD_PRIORITY_BELOW_NORMAL = (THREAD_PRIORITY_LOWEST+1);
  THREAD_PRIORITY_NORMAL       = 0;
  THREAD_PRIORITY_HIGHEST      = THREAD_BASE_PRIORITY_MAX;
  THREAD_PRIORITY_ABOVE_NORMAL = (THREAD_PRIORITY_HIGHEST-1);
  THREAD_PRIORITY_ERROR_RETURN = (MAXLONG);

  THREAD_PRIORITY_TIME_CRITICAL = THREAD_BASE_PRIORITY_LOWRT;
  THREAD_PRIORITY_IDLE          = THREAD_BASE_PRIORITY_IDLE;

//
// Debug APIs
//

  EXCEPTION_DEBUG_EVENT      = 1;
  CREATE_THREAD_DEBUG_EVENT  = 2;
  CREATE_PROCESS_DEBUG_EVENT = 3;
  EXIT_THREAD_DEBUG_EVENT    = 4;
  EXIT_PROCESS_DEBUG_EVENT   = 5;
  LOAD_DLL_DEBUG_EVENT       = 6;
  UNLOAD_DLL_DEBUG_EVENT     = 7;
  OUTPUT_DEBUG_STRING_EVENT  = 8;
  RIP_EVENT                  = 9;

type
  LPEXCEPTION_DEBUG_INFO = ^EXCEPTION_DEBUG_INFO;
  _EXCEPTION_DEBUG_INFO = record
    ExceptionRecord: EXCEPTION_RECORD;
    dwFirstChance: DWORD;
  end;
  EXCEPTION_DEBUG_INFO = _EXCEPTION_DEBUG_INFO;
  TExceptionDebugInfo = EXCEPTION_DEBUG_INFO;
  PExceptionDebugInfo = LPEXCEPTION_DEBUG_INFO;

  LPCREATE_THREAD_DEBUG_INFO = ^CREATE_THREAD_DEBUG_INFO;
  _CREATE_THREAD_DEBUG_INFO = record
    hThread: HANDLE;
    lpThreadLocalBase: LPVOID;
    lpStartAddress: LPTHREAD_START_ROUTINE;
  end;
  CREATE_THREAD_DEBUG_INFO = _CREATE_THREAD_DEBUG_INFO;
  TCreateThreadDebugInfo = CREATE_THREAD_DEBUG_INFO;
  PCreateThreadDebugInfo = LPCREATE_THREAD_DEBUG_INFO;

  LPCREATE_PROCESS_DEBUG_INFO = ^CREATE_PROCESS_DEBUG_INFO;
  _CREATE_PROCESS_DEBUG_INFO = record
    hFile: HANDLE;
    hProcess: HANDLE;
    hThread: HANDLE;
    lpBaseOfImage: LPVOID;
    dwDebugInfoFileOffset: DWORD;
    nDebugInfoSize: DWORD;
    lpThreadLocalBase: LPVOID;
    lpStartAddress: LPTHREAD_START_ROUTINE;
    lpImageName: LPVOID;
    fUnicode: Word;
  end;
  CREATE_PROCESS_DEBUG_INFO = _CREATE_PROCESS_DEBUG_INFO;
  TCreateProcessDebugInfo = CREATE_PROCESS_DEBUG_INFO;
  PCreateProcessDebugInfo = LPCREATE_PROCESS_DEBUG_INFO;

  LPEXIT_THREAD_DEBUG_INFO = ^EXIT_THREAD_DEBUG_INFO;
  _EXIT_THREAD_DEBUG_INFO = record
    dwExitCode: DWORD;
  end;
  EXIT_THREAD_DEBUG_INFO = _EXIT_THREAD_DEBUG_INFO;
  TExitThreadDebugInfo = EXIT_THREAD_DEBUG_INFO;
  PExitThreadDebugInfo = LPEXIT_THREAD_DEBUG_INFO;

  LPEXIT_PROCESS_DEBUG_INFO = ^EXIT_PROCESS_DEBUG_INFO;
  _EXIT_PROCESS_DEBUG_INFO = record
    dwExitCode: DWORD;
  end;
  EXIT_PROCESS_DEBUG_INFO = _EXIT_PROCESS_DEBUG_INFO;
  TExitProcessDebugInfo = EXIT_PROCESS_DEBUG_INFO;
  PExitProcessDebugInfo = LPEXIT_PROCESS_DEBUG_INFO;

  LPLOAD_DLL_DEBUG_INFO = ^LOAD_DLL_DEBUG_INFO;
  _LOAD_DLL_DEBUG_INFO = record
    hFile: HANDLE;
    lpBaseOfDll: LPVOID;
    dwDebugInfoFileOffset: DWORD;
    nDebugInfoSize: DWORD;
    lpImageName: LPVOID;
    fUnicode: Word;
  end;
  LOAD_DLL_DEBUG_INFO = _LOAD_DLL_DEBUG_INFO;
  TLoadDllDebugInfo = LOAD_DLL_DEBUG_INFO;
  PLoadDllDebugInfo = LPLOAD_DLL_DEBUG_INFO;

  LPUNLOAD_DLL_DEBUG_INFO = ^UNLOAD_DLL_DEBUG_INFO;
  _UNLOAD_DLL_DEBUG_INFO = record
    lpBaseOfDll: LPVOID;
  end;
  UNLOAD_DLL_DEBUG_INFO = _UNLOAD_DLL_DEBUG_INFO;
  TUnloadDllDebugInfo = UNLOAD_DLL_DEBUG_INFO;
  PUnloadDllDebugInfo = LPUNLOAD_DLL_DEBUG_INFO;

  LPOUTPUT_DEBUG_STRING_INFO = ^OUTPUT_DEBUG_STRING_INFO;
  _OUTPUT_DEBUG_STRING_INFO = record
    lpDebugStringData: LPSTR;
    fUnicode: Word;
    nDebugStringLength: Word;
  end;
  OUTPUT_DEBUG_STRING_INFO = _OUTPUT_DEBUG_STRING_INFO;
  TOutputDebugStringInfo = OUTPUT_DEBUG_STRING_INFO;
  POutputDebugStringInfo = LPOUTPUT_DEBUG_STRING_INFO;

  LPRIP_INFO = ^RIP_INFO;
  _RIP_INFO = record
    dwError: DWORD;
    dwType: DWORD;
  end;
  RIP_INFO = _RIP_INFO;
  TRipInfo = RIP_INFO;
  PRipInfo = LPRIP_INFO;

  LPDEBUG_EVENT = ^DEBUG_EVENT;
  _DEBUG_EVENT = record
    dwDebugEventCode: DWORD;
    dwProcessId: DWORD;
    dwThreadId: DWORD;
    case Integer of
      0: (Exception: EXCEPTION_DEBUG_INFO);
      1: (CreateThread: CREATE_THREAD_DEBUG_INFO);
      2: (CreateProcessInfo: CREATE_PROCESS_DEBUG_INFO);
      3: (ExitThread: EXIT_THREAD_DEBUG_INFO);
      4: (ExitProcess: EXIT_PROCESS_DEBUG_INFO);
      5: (LoadDll: LOAD_DLL_DEBUG_INFO);
      6: (UnloadDll: UNLOAD_DLL_DEBUG_INFO);
      7: (DebugString: OUTPUT_DEBUG_STRING_INFO);
      8: (RipInfo: RIP_INFO);
  end;
  DEBUG_EVENT = _DEBUG_EVENT;
  TDebugEvent = DEBUG_EVENT;
  PDebugEvent = LPDEBUG_EVENT;

  LPCONTEXT = PCONTEXT;
  LPEXCEPTION_RECORD = PEXCEPTION_RECORD;
  LPEXCEPTION_POINTERS = PEXCEPTION_POINTERS;

const
  DRIVE_UNKNOWN     = 0;
  DRIVE_NO_ROOT_DIR = 1;
  DRIVE_REMOVABLE   = 2;
  DRIVE_FIXED       = 3;
  DRIVE_REMOTE      = 4;
  DRIVE_CDROM       = 5;
  DRIVE_RAMDISK     = 6;

function GetFreeSpace(w: WORD): DWORD;

const
  FILE_TYPE_UNKNOWN = $0000;
  FILE_TYPE_DISK    = $0001;
  FILE_TYPE_CHAR    = $0002;
  FILE_TYPE_PIPE    = $0003;
  FILE_TYPE_REMOTE  = $8000;

  STD_INPUT_HANDLE  = DWORD(-10);
  STD_OUTPUT_HANDLE = DWORD(-11);
  STD_ERROR_HANDLE  = DWORD(-12);

  NOPARITY    = 0;
  ODDPARITY   = 1;
  EVENPARITY  = 2;
  MARKPARITY  = 3;
  SPACEPARITY = 4;

  ONESTOPBIT   = 0;
  ONE5STOPBITS = 1;
  TWOSTOPBITS  = 2;

  IGNORE   = 0;                // Ignore signal
  INFINITE = DWORD($FFFFFFFF); // Infinite timeout

//
// Baud rates at which the communication device operates
//

  CBR_110    = 110;
  CBR_300    = 300;
  CBR_600    = 600;
  CBR_1200   = 1200;
  CBR_2400   = 2400;
  CBR_4800   = 4800;
  CBR_9600   = 9600;
  CBR_14400  = 14400;
  CBR_19200  = 19200;
  CBR_38400  = 38400;
  CBR_56000  = 56000;
  CBR_57600  = 57600;
  CBR_115200 = 115200;
  CBR_128000 = 128000;
  CBR_256000 = 256000;

//
// Error Flags
//

  CE_RXOVER   = $0001; // Receive Queue overflow
  CE_OVERRUN  = $0002; // Receive Overrun Error
  CE_RXPARITY = $0004; // Receive Parity Error
  CE_FRAME    = $0008; // Receive Framing error
  CE_BREAK    = $0010; // Break Detected
  CE_TXFULL   = $0100; // TX Queue is full
  CE_PTO      = $0200; // LPTx Timeout
  CE_IOE      = $0400; // LPTx I/O Error
  CE_DNS      = $0800; // LPTx Device not selected
  CE_OOP      = $1000; // LPTx Out-Of-Paper
  CE_MODE     = $8000; // Requested mode unsupported

  IE_BADID    = DWORD(-1); // Invalid or unsupported id
  IE_OPEN     = DWORD(-2); // Device Already Open
  IE_NOPEN    = DWORD(-3); // Device Not Open
  IE_MEMORY   = DWORD(-4); // Unable to allocate queues
  IE_DEFAULT  = DWORD(-5); // Error in default parameters
  IE_HARDWARE = DWORD(-10); // Hardware Not Present
  IE_BYTESIZE = DWORD(-11); // Illegal Byte Size
  IE_BAUDRATE = DWORD(-12); // Unsupported BaudRate

//
// Events
//

  EV_RXCHAR   = $0001; // Any Character received
  EV_RXFLAG   = $0002; // Received certain character
  EV_TXEMPTY  = $0004; // Transmitt Queue Empty
  EV_CTS      = $0008; // CTS changed state
  EV_DSR      = $0010; // DSR changed state
  EV_RLSD     = $0020; // RLSD changed state
  EV_BREAK    = $0040; // BREAK received
  EV_ERR      = $0080; // Line status error occurred
  EV_RING     = $0100; // Ring signal detected
  EV_PERR     = $0200; // Printer error occured
  EV_RX80FULL = $0400; // Receive buffer is 80 percent full
  EV_EVENT1   = $0800; // Provider specific event 1
  EV_EVENT2   = $1000; // Provider specific event 2

//
// Escape Functions
//

  SETXOFF  = 1; // Simulate XOFF received
  SETXON   = 2; // Simulate XON received
  SETRTS   = 3; // Set RTS high
  CLRRTS   = 4; // Set RTS low
  SETDTR   = 5; // Set DTR high
  CLRDTR   = 6; // Set DTR low
  RESETDEV = 7; // Reset device if possible
  SETBREAK = 8; // Set the device break line.
  CLRBREAK = 9; // Clear the device break line.

//
// PURGE function flags.
//

  PURGE_TXABORT = $0001; // Kill the pending/current writes to the comm port.
  PURGE_RXABORT = $0002; // Kill the pending/current reads to the comm port.
  PURGE_TXCLEAR = $0004; // Kill the transmit queue if there.
  PURGE_RXCLEAR = $0008; // Kill the typeahead buffer if there.

  LPTx = $80; // Set if ID is for LPT device

//
// Modem Status Flags
//

  MS_CTS_ON  = DWORD($0010);
  MS_DSR_ON  = DWORD($0020);
  MS_RING_ON = DWORD($0040);
  MS_RLSD_ON = DWORD($0080);

//
// WaitSoundState() Constants
//

  S_QUEUEEMPTY   = 0;
  S_THRESHOLD    = 1;
  S_ALLTHRESHOLD = 2;

//
// Accent Modes
//

  S_NORMAL   = 0;
  S_LEGATO   = 1;
  S_STACCATO = 2;

//
// SetSoundNoise() Sources
//

  S_PERIOD512   = 0; // Freq = N/512 high pitch, less coarse hiss
  S_PERIOD1024  = 1; // Freq = N/1024
  S_PERIOD2048  = 2; // Freq = N/2048 low pitch, more coarse hiss
  S_PERIODVOICE = 3; // Source is frequency from voice channel (3)
  S_WHITE512    = 4; // Freq = N/512 high pitch, less coarse hiss
  S_WHITE1024   = 5; // Freq = N/1024
  S_WHITE2048   = 6; // Freq = N/2048 low pitch, more coarse hiss
  S_WHITEVOICE  = 7; // Source is frequency from voice channel (3)

  S_SERDVNA = DWORD(-1); // Device not available
  S_SEROFM  = DWORD(-2); // Out of memory
  S_SERMACT = DWORD(-3); // Music active
  S_SERQFUL = DWORD(-4); // Queue full
  S_SERBDNT = DWORD(-5); // Invalid note
  S_SERDLN  = DWORD(-6); // Invalid note length
  S_SERDCC  = DWORD(-7); // Invalid note count
  S_SERDTP  = DWORD(-8); // Invalid tempo
  S_SERDVL  = DWORD(-9); // Invalid volume
  S_SERDMD  = DWORD(-10); // Invalid mode
  S_SERDSH  = DWORD(-11); // Invalid shape
  S_SERDPT  = DWORD(-12); // Invalid pitch
  S_SERDFQ  = DWORD(-13); // Invalid frequency
  S_SERDDR  = DWORD(-14); // Invalid duration
  S_SERDSR  = DWORD(-15); // Invalid source
  S_SERDST  = DWORD(-16); // Invalid state

  NMPWAIT_WAIT_FOREVER     = DWORD($ffffffff);
  NMPWAIT_NOWAIT           = $00000001;
  NMPWAIT_USE_DEFAULT_WAIT = $00000000;

  FS_CASE_IS_PRESERVED      = FILE_CASE_PRESERVED_NAMES;
  FS_CASE_SENSITIVE         = FILE_CASE_SENSITIVE_SEARCH;
  FS_UNICODE_STORED_ON_DISK = FILE_UNICODE_ON_DISK;
  FS_PERSISTENT_ACLS        = FILE_PERSISTENT_ACLS;
  FS_VOL_IS_COMPRESSED      = FILE_VOLUME_IS_COMPRESSED;
  FS_FILE_COMPRESSION       = FILE_FILE_COMPRESSION;
  FS_FILE_ENCRYPTION        = FILE_SUPPORTS_ENCRYPTION;

  FILE_MAP_COPY       = SECTION_QUERY;
  FILE_MAP_WRITE      = SECTION_MAP_WRITE;
  FILE_MAP_READ       = SECTION_MAP_READ;
  FILE_MAP_ALL_ACCESS = SECTION_ALL_ACCESS;

  OF_READ             = $00000000;
  OF_WRITE            = $00000001;
  OF_READWRITE        = $00000002;
  OF_SHARE_COMPAT     = $00000000;
  OF_SHARE_EXCLUSIVE  = $00000010;
  OF_SHARE_DENY_WRITE = $00000020;
  OF_SHARE_DENY_READ  = $00000030;
  OF_SHARE_DENY_NONE  = $00000040;
  OF_PARSE            = $00000100;
  OF_DELETE           = $00000200;
  OF_VERIFY           = $00000400;
  OF_CANCEL           = $00000800;
  OF_CREATE           = $00001000;
  OF_PROMPT           = $00002000;
  OF_EXIST            = $00004000;
  OF_REOPEN           = $00008000;

  OFS_MAXPATHNAME = 128;

type
  LPOFSTRUCT = ^OFSTRUCT;
  _OFSTRUCT = record
    cBytes: Byte;
    fFixedDisk: Byte;
    nErrCode: Word;
    Reserved1: Word;
    Reserved2: Word;
    szPathName: array [0..OFS_MAXPATHNAME - 1] of CHAR;
  end;
  OFSTRUCT = _OFSTRUCT;
  TOfStruct = OFSTRUCT;
  POfStruct = LPOFSTRUCT;

//
// The Risc compilers support intrinsic functions for interlocked
// increment, decrement, and exchange.
//

function InterlockedIncrement(var lpAddend: LONG): LONG; stdcall;

function InterlockedDecrement(var lpAddend: LONG): LONG; stdcall;

function InterlockedExchange(var Target: LONG; Value: LONG): LONG; stdcall;

function InterlockedExchangePointer(var Target: PVOID; Value: PVOID): PVOID;

function InterlockedExchangeAdd(var Addend: LONG; Value: LONG): LONG; stdcall;

function InterlockedCompareExchange(var Destination: LONG; Exchange: LONG;
  Comperand: LONG): LONG; stdcall;

function InterlockedCompareExchangePointer(var Destination: PVOID;
  Exchange, Comperand: PVOID): PVOID;

{
#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedDecrementAcquire InterlockedDecrement
#define InterlockedDecrementRelease InterlockedDecrement
#define InterlockedIncrementAcquire InterlockedIncrement
#define InterlockedIncrementRelease InterlockedIncrement
#define InterlockedCompareExchangeAcquire InterlockedCompareExchange
#define InterlockedCompareExchangeRelease InterlockedCompareExchange
#define InterlockedCompareExchangeAcquire64 InterlockedCompareExchange64
#define InterlockedCompareExchangeRelease64 InterlockedCompareExchange64
}

procedure InitializeSListHead(ListHead: PSLIST_HEADER); stdcall;

function InterlockedPopEntrySList(ListHead: PSLIST_HEADER): PSLIST_ENTRY; stdcall;

function InterlockedPushEntrySList(ListHead: PSLIST_HEADER; ListEntry: PSLIST_ENTRY): PSLIST_ENTRY; stdcall;

function InterlockedFlushSList(ListHead: PSLIST_HEADER): PSLIST_ENTRY; stdcall;

function QueryDepthSList(ListHead: PSLIST_HEADER): USHORT; stdcall;

function FreeResource(hResData: HGLOBAL): BOOL; stdcall;

function LockResource(hResData: HGLOBAL): LPVOID; stdcall;

function UnlockResource(hResData: HANDLE): BOOL;

const
  MAXINTATOM = $C000;
  INVALID_ATOM = ATOM(0);

type
  MAKEINTATOMA = PAnsiChar;
  MAKEINTATOMW = PWideChar;
{$IFDEF UNICODE}
  MAKEINTATOM = MAKEINTATOMW;
{$ELSE}
  MAKEINTATOM = MAKEINTATOMA;
{$ENDIF}

function FreeLibrary(hLibModule: HMODULE): BOOL; stdcall;

procedure FreeLibraryAndExitThread(hLibModule: HMODULE; dwExitCode: DWORD); stdcall;

function DisableThreadLibraryCalls(hLibModule: HMODULE): BOOL; stdcall;

function GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;

function GetVersion: DWORD; stdcall;

function GlobalAlloc(uFlags: UINT; dwBytes: SIZE_T): HGLOBAL; stdcall;

function GlobalReAlloc(hMem: HGLOBAL; dwBytes: SIZE_T; uFlags: UINT): HGLOBAL; stdcall;

function GlobalSize(hMem: HGLOBAL): SIZE_T; stdcall;

function GlobalFlags(hMem: HGLOBAL): UINT; stdcall;

function GlobalLock(hMem: HGLOBAL): LPVOID; stdcall;

//!!!MWH My version  win31 = DWORD WINAPI GlobalHandle(UINT)

function GlobalHandle(pMem: LPCVOID): HGLOBAL; stdcall;

function GlobalUnlock(hMem: HGLOBAL): BOOL; stdcall;

function GlobalFree(hMem: HGLOBAL): HGLOBAL; stdcall;

function GlobalCompact(dwMinFree: DWORD): SIZE_T; stdcall;

procedure GlobalFix(hMem: HGLOBAL); stdcall;

procedure GlobalUnfix(hMem: HGLOBAL); stdcall;

function GlobalWire(hMem: HGLOBAL): LPVOID; stdcall;

function GlobalUnWire(hMem: HGLOBAL): BOOL; stdcall;

procedure GlobalMemoryStatus(var lpBuffer: MEMORYSTATUS); stdcall;

type
  LPMEMORYSTATUSEX = ^MEMORYSTATUSEX;
  _MEMORYSTATUSEX = record
    dwLength: DWORD;
    dwMemoryLoad: DWORD;
    ullTotalPhys: DWORDLONG;
    ullAvailPhys: DWORDLONG;
    ullTotalPageFile: DWORDLONG;
    ullAvailPageFile: DWORDLONG;
    ullTotalVirtual: DWORDLONG;
    ullAvailVirtual: DWORDLONG;
    ullAvailExtendedVirtual: DWORDLONG;
  end;
  MEMORYSTATUSEX = _MEMORYSTATUSEX;
  TMemoryStatusEx = MEMORYSTATUSEX;
  PMemoryStatusEx = LPMEMORYSTATUSEX;

function GlobalMemoryStatusEx(var lpBuffer: MEMORYSTATUSEX): BOOL; stdcall;

function LocalAlloc(uFlags: UINT; uBytes: SIZE_T): HLOCAL; stdcall;

function LocalReAlloc(hMem: HLOCAL; uBytes: SIZE_T; uFlags: UINT): HLOCAL; stdcall;

function LocalLock(hMem: HLOCAL): LPVOID; stdcall;

function LocalHandle(pMem: LPCVOID): HLOCAL; stdcall;

function LocalUnlock(hMem: HLOCAL): BOOL; stdcall;

function LocalSize(hMem: HLOCAL): SIZE_T; stdcall;

function LocalFlags(hMem: HLOCAL): UINT; stdcall;

function LocalFree(hMem: HLOCAL): HLOCAL; stdcall;

function LocalShrink(hMem: HLOCAL; cbNewSize: UINT): SIZE_T; stdcall;

function LocalCompact(uMinFree: UINT): SIZE_T; stdcall;

function FlushInstructionCache(hProcess: HANDLE; lpBaseAddress: LPCVOID;
  dwSize: DWORD): BOOL; stdcall;

function VirtualAlloc(lpAddress: LPVOID; dwSize: SIZE_T; flAllocationType: DWORD;
  flProtect: DWORD): LPVOID; stdcall;

function VirtualFree(lpAddress: LPVOID; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; stdcall;

function VirtualProtect(lpAddress: LPVOID; dwSize: SIZE_T; flNewProtect: DWORD;
  lpflOldProtect: PDWORD): BOOL; stdcall;

function VirtualQuery(lpAddress: LPCVOID; var lpBuffer: MEMORY_BASIC_INFORMATION;
  dwLength: DWORD): DWORD; stdcall;

function VirtualAllocEx(hProcess: HANDLE; lpAddress: LPVOID; dwSize: SIZE_T;
  flAllocationType: DWORD; flProtect: DWORD): LPVOID; stdcall;

function GetWriteWatch(dwFlags: DWORD; lpBaseAddress: PVOID;
  dwRegionSize: SIZE_T; var lpAddresses: PVOID; var lpdwCount: ULONG_PTR;
  var lpdwGranularity: ULONG): UINT; stdcall;

function ResetWriteWatch(lpBaseAddress: LPVOID; dwRegionSize: SIZE_T): UINT; stdcall;

function GetLargePageMinimum: SIZE_T; stdcall;

function VirtualFreeEx(hProcess: HANDLE; lpAddress: LPVOID; dwSize: SIZE_T;
  dwFreeType: DWORD): BOOL; stdcall;

function VirtualProtectEx(hProcess: HANDLE; lpAddress: LPVOID; dwSize: SIZE_T;
  flNewProtect: DWORD; lpflOldProtect: PDWORD): BOOL; stdcall;

function VirtualQueryEx(hProcess: HANDLE; lpAddress: LPCVOID;
  var lpBuffer: MEMORY_BASIC_INFORMATION; dwLength: DWORD): DWORD; stdcall;

function HeapCreate(flOptions: DWORD; dwInitialSize: SIZE_T;
  dwMaximumSize: SIZE_T): HANDLE; stdcall;

function HeapDestroy(hHeap: HANDLE): BOOL; stdcall;

function HeapAlloc(hHeap: HANDLE; dwFlags: DWORD; dwBytes: SIZE_T): LPVOID; stdcall;

function HeapReAlloc(hHeap: HANDLE; dwFlags: DWORD; lpMem: LPVOID; dwBytes: SIZE_T): LPVOID; stdcall;

function HeapFree(hHeap: HANDLE; dwFlags: DWORD; lpMem: LPVOID): BOOL; stdcall;

function HeapSize(hHeap: HANDLE; dwFlags: DWORD; lpMem: LPCVOID): SIZE_T; stdcall;

function HeapValidate(hHeap: HANDLE; dwFlags: DWORD; lpMem: LPCVOID): BOOL; stdcall;

function HeapCompact(hHeap: HANDLE; dwFlags: DWORD): SIZE_T; stdcall;

function GetProcessHeap: HANDLE; stdcall;

function GetProcessHeaps(NumberOfHeaps: DWORD; var ProcessHeaps: HANDLE): DWORD; stdcall;

type
  PPROCESS_HEAP_ENTRY = ^PROCESS_HEAP_ENTRY;
  _PROCESS_HEAP_ENTRY = record
    lpData: PVOID;
    cbData: DWORD;
    cbOverhead: BYTE;
    iRegionIndex: BYTE;
    wFlags: WORD;
    case Integer of
      0: (
        hMem: HANDLE;
        dwReserved: array [0..2] of DWORD);
      1: (
        dwComittedSize: DWORD;
        dwUnComittedSize: DWORD;
        lpFirstBlock: LPVOID;
        lpLastBlock: LPVOID);
  end;
  PROCESS_HEAP_ENTRY = _PROCESS_HEAP_ENTRY;
  LPPROCESS_HEAP_ENTRY = ^PROCESS_HEAP_ENTRY;
  TProcessHeapEntry = PROCESS_HEAP_ENTRY;
  PProcessHeapEntry = PPROCESS_HEAP_ENTRY;

const
  PROCESS_HEAP_REGION            = $0001;
  PROCESS_HEAP_UNCOMMITTED_RANGE = $0002;
  PROCESS_HEAP_ENTRY_BUSY        = $0004;
  PROCESS_HEAP_ENTRY_MOVEABLE    = $0010;
  PROCESS_HEAP_ENTRY_DDESHARE    = $0020;

function HeapLock(hHeap: HANDLE): BOOL; stdcall;

function HeapUnlock(hHeap: HANDLE): BOOL; stdcall;

function HeapWalk(hHeap: HANDLE; var lpEntry: PROCESS_HEAP_ENTRY): BOOL; stdcall;

function HeapSetInformation(HeapHandle: HANDLE; HeapInformationClass: HEAP_INFORMATION_CLASS;
  HeapInformation: PVOID; HeapInformationLength: SIZE_T): BOOL; stdcall;

function HeapQueryInformation(HeapHandle: HANDLE; HeapInformationClass: HEAP_INFORMATION_CLASS;
  HeapInformation: PVOID; HeapInformationLength: SIZE_T; ReturnLength: PSIZE_T): BOOL; stdcall;

// GetBinaryType return values.

const
  SCS_32BIT_BINARY = 0;
  SCS_DOS_BINARY   = 1;
  SCS_WOW_BINARY   = 2;
  SCS_PIF_BINARY   = 3;
  SCS_POSIX_BINARY = 4;
  SCS_OS216_BINARY = 5;
  SCS_64BIT_BINARY = 6;

  SCS_THIS_PLATFORM_BINARY = SCS_32BIT_BINARY;

function GetBinaryTypeA(lpApplicationName: LPCSTR; var lpBinaryType: DWORD): BOOL; stdcall;
function GetBinaryTypeW(lpApplicationName: LPCWSTR; var lpBinaryType: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetBinaryType(lpApplicationName: LPCWSTR; var lpBinaryType: DWORD): BOOL; stdcall;
{$ELSE}
function GetBinaryType(lpApplicationName: LPCSTR; var lpBinaryType: DWORD): BOOL; stdcall;
{$ENDIF}

function GetShortPathNameA(lpszLongPath: LPCSTR; lpszShortPath: LPSTR;
  cchBuffer: DWORD): DWORD; stdcall;
function GetShortPathNameW(lpszLongPath: LPCWSTR; lpszShortPath: LPWSTR;
  cchBuffer: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetShortPathName(lpszLongPath: LPCWSTR; lpszShortPath: LPWSTR;
  cchBuffer: DWORD): DWORD; stdcall;
{$ELSE}
function GetShortPathName(lpszLongPath: LPCSTR; lpszShortPath: LPSTR;
  cchBuffer: DWORD): DWORD; stdcall;
{$ENDIF}

function GetLongPathNameA(lpszShortPath: LPCSTR; lpszLongPath: LPSTR;
  cchBuffer: DWORD): DWORD; stdcall;
function GetLongPathNameW(lpszShortPath: LPCWSTR; lpszLongPath: LPWSTR;
  cchBuffer: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetLongPathName(lpszShortPath: LPCWSTR; lpszLongPath: LPWSTR;
  cchBuffer: DWORD): DWORD; stdcall;
{$ELSE}
function GetLongPathName(lpszShortPath: LPCSTR; lpszLongPath: LPSTR;
  cchBuffer: DWORD): DWORD; stdcall;
{$ENDIF}

function GetProcessAffinityMask(hProcess: HANDLE;
  var lpProcessAffinityMask, lpSystemAffinityMask: DWORD_PTR): BOOL; stdcall;

function SetProcessAffinityMask(hProcess: HANDLE;
  dwProcessAffinityMask: DWORD_PTR): BOOL; stdcall;

function GetProcessHandleCount(hProcess: HANDLE; out pdwHandleCount: DWORD): BOOL; stdcall;

function GetProcessTimes(hProcess: HANDLE; var lpCreationTime, lpExitTime,
  lpKernelTime, lpUserTime: FILETIME): BOOL; stdcall;

function GetProcessIoCounters(hProcess: HANDLE; var lpIoCounters: IO_COUNTERS): BOOL; stdcall;

function GetProcessWorkingSetSize(hProcess: HANDLE;
  var lpMinimumWorkingSetSize, lpMaximumWorkingSetSize: SIZE_T): BOOL; stdcall;

function GetProcessWorkingSetSizeEx(hProcess: HANDLE; out lpMinimumWorkingSetSize,  lpMaximumWorkingSetSize: SIZE_T; out Flags: DWORD): BOOL; stdcall;

function SetProcessWorkingSetSize(hProcess: HANDLE; dwMinimumWorkingSetSize,
  dwMaximumWorkingSetSize: SIZE_T): BOOL; stdcall;

function SetProcessWorkingSetSizeEx(hProcess: HANDLE; dwMinimumWorkingSetSize, dwMaximumWorkingSetSize: SIZE_T; Flags: DWORD): BOOL; stdcall;

function OpenProcess(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  dwProcessId: DWORD): HANDLE; stdcall;

function GetCurrentProcess: HANDLE; stdcall;

function GetCurrentProcessId: DWORD; stdcall;

procedure ExitProcess(uExitCode: UINT); stdcall;

function TerminateProcess(hProcess: HANDLE; uExitCode: UINT): BOOL; stdcall;

function GetExitCodeProcess(hProcess: HANDLE; var lpExitCode: DWORD): BOOL; stdcall;

procedure FatalExit(ExitCode: Integer); stdcall;

{$IFNDEF UNICODE}
function GetEnvironmentStrings: LPSTR; stdcall;
{$ENDIF}

function GetEnvironmentStringsW: LPWSTR; stdcall;

{$IFDEF UNICODE}
function GetEnvironmentStrings: LPWSTR; stdcall;
{$ELSE}
function GetEnvironmentStringsA: LPSTR; stdcall;
{$ENDIF}

function SetEnvironmentStringsA(NewEnvironment: LPSTR): BOOL; stdcall;

function SetEnvironmentStringsW(NewEnvironment: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetEnvironmentStrings(NewEnvironment: LPWSTR): BOOL; stdcall;
{$ELSE}
function SetEnvironmentStrings(NewEnvironment: LPSTR): BOOL; stdcall;
{$ENDIF}

function FreeEnvironmentStringsA(pstr: LPSTR): BOOL; stdcall;
function FreeEnvironmentStringsW(pstr: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function FreeEnvironmentStrings(pstr: LPWSTR): BOOL; stdcall;
{$ELSE}
function FreeEnvironmentStrings(pstr: LPSTR): BOOL; stdcall;
{$ENDIF}

procedure RaiseException(dwExceptionCode: DWORD; dwExceptionFlags: DWORD;
  nNumberOfArguments: DWORD; lpArguments: PULONG_PTR); stdcall;

function UnhandledExceptionFilter(ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;

type
  PTOP_LEVEL_EXCEPTION_FILTER = function (ExceptionInfo: PEXCEPTION_POINTERS): LONG; stdcall;
  LPTOP_LEVEL_EXCEPTION_FILTER = PTOP_LEVEL_EXCEPTION_FILTER;
  TTopLevelExceptionFilter = PTOP_LEVEL_EXCEPTION_FILTER;

function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter: LPTOP_LEVEL_EXCEPTION_FILTER): LPTOP_LEVEL_EXCEPTION_FILTER; stdcall;

//
// Fiber creation flags
//

const
  FIBER_FLAG_FLOAT_SWITCH = $1;     // context switch floating point

function CreateFiber(dwStackSize: DWORD; lpStartAddress: LPFIBER_START_ROUTINE;
  lpParameter: LPVOID): LPVOID; stdcall;

function CreateFiberEx(dwStackCommitSize, dwStackReserveSize: SIZE_T; dwFlags: DWORD;
  lpStartAddress: LPFIBER_START_ROUTINE; lpParameter: LPVOID): LPVOID; stdcall;

procedure DeleteFiber(lpFiber: LPVOID); stdcall;

function ConvertThreadToFiber(lpParameter: LPVOID): LPVOID; stdcall;

function ConvertThreadToFiberEx(lpParameter: LPVOID; dwFlags: DWORD): LPVOID; stdcall;

function ConvertFiberToThread: BOOL; stdcall;

procedure SwitchToFiber(lpFiber: LPVOID); stdcall;

function SwitchToThread: BOOL; stdcall;

function CreateThread(lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  dwStackSize: DWORD; lpStartAddress: LPTHREAD_START_ROUTINE; lpParameter: LPVOID;
  dwCreationFlags: DWORD; lpThreadId: LPDWORD): HANDLE; stdcall;

function CreateRemoteThread(hProcess: HANDLE;
  lpThreadAttributes: LPSECURITY_ATTRIBUTES; dwStackSize: DWORD;
  lpStartAddress: LPTHREAD_START_ROUTINE; lpParameter: LPVOID;
  dwCreationFlags: DWORD; lpThreadId: LPDWORD): HANDLE; stdcall;

function GetCurrentThread: HANDLE; stdcall;

function GetCurrentThreadId: DWORD; stdcall;

function GetProcessIdOfThread(Thread: HANDLE): DWORD; stdcall;

function GetThreadId(Thread: HANDLE): DWORD; stdcall;

function GetProcessId(Process: HANDLE): DWORD; stdcall;

function GetCurrentProcessorNumber: DWORD; stdcall;

function SetThreadAffinityMask(hThread: HANDLE;
  dwThreadAffinityMask: DWORD_PTR): DWORD_PTR; stdcall;

function SetThreadIdealProcessor(hThread: HANDLE; dwIdealProcessor: DWORD): DWORD; stdcall;

function SetProcessPriorityBoost(hProcess: HANDLE;
  bDisablePriorityBoost: BOOL): BOOL; stdcall;

function GetProcessPriorityBoost(hProcess: HANDLE;
  var pDisablePriorityBoost: BOOL): BOOL; stdcall;

function RequestWakeupLatency(latency: LATENCY_TIME): BOOL; stdcall;

function IsSystemResumeAutomatic: BOOL; stdcall;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  dwThreadId: DWORD): HANDLE; stdcall;

function SetThreadPriority(hThread: HANDLE; nPriority: Integer): BOOL; stdcall;

function SetThreadPriorityBoost(hThread: HANDLE; bDisablePriorityBoost: BOOL): BOOL; stdcall;

function GetThreadPriorityBoost(hThread: HANDLE;
  var pDisablePriorityBoost: BOOL): BOOL; stdcall;

function GetThreadPriority(hThread: HANDLE): Integer; stdcall;

function GetThreadTimes(hThread: HANDLE; var lpCreationTime, lpExitTime,
  lpKernelTime, lpUserTime: FILETIME): BOOL; stdcall;

function GetThreadIOPendingFlag(hThread: HANDLE; out lpIOIsPending: BOOL): BOOL; stdcall;

procedure ExitThread(dwExitCode: DWORD); stdcall;

function TerminateThread(hThread: HANDLE; dwExitCode: DWORD): BOOL; stdcall;

function GetExitCodeThread(hThread: HANDLE; var lpExitCode: DWORD): BOOL; stdcall;

function GetThreadSelectorEntry(hThread: HANDLE; dwSelector: DWORD;
  var lpSelectorEntry: LDT_ENTRY): BOOL; stdcall;

function SetThreadExecutionState(esFlags: EXECUTION_STATE): EXECUTION_STATE; stdcall;

function GetLastError: DWORD; stdcall;

procedure SetLastError(dwErrCode: DWORD); stdcall;

procedure RestoreLastError(dwErrCode: DWORD); stdcall;

type
   PRESTORE_LAST_ERROR = procedure (dwErrCode: DWORD); stdcall;

const
  RESTORE_LAST_ERROR_NAME_A = 'RestoreLastError';
  RESTORE_LAST_ERROR_NAME_W = WideString('RestoreLastError');
  RESTORE_LAST_ERROR_NAME   = __TEXT('RestoreLastError');

function HasOverlappedIoCompleted(const lpOverlapped: OVERLAPPED): BOOL;

function GetOverlappedResult(hFile: HANDLE; const lpOverlapped: OVERLAPPED;
  var lpNumberOfBytesTransferred: DWORD; bWait: BOOL): BOOL; stdcall;

function CreateIoCompletionPort(FileHandle: HANDLE; ExistingCompletionPort: HANDLE;
  CompletionKey: ULONG_PTR; NumberOfConcurrentThreads: DWORD): HANDLE; stdcall;

function GetQueuedCompletionStatus(CompletionPort: HANDLE;
  var lpNumberOfBytesTransferred: DWORD; var lpCompletionKey: ULONG_PTR;
  var lpOverlapped: LPOVERLAPPED; dwMilliseconds: DWORD): BOOL; stdcall;

function PostQueuedCompletionStatus(CompletionPort: HANDLE;
  dwNumberOfBytesTransferred: DWORD; dwCompletionKey: ULONG_PTR;
  lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

const
  SEM_FAILCRITICALERRORS     = $0001;
  SEM_NOGPFAULTERRORBOX      = $0002;
  SEM_NOALIGNMENTFAULTEXCEPT = $0004;
  SEM_NOOPENFILEERRORBOX     = $8000;

function SetErrorMode(uMode: UINT): UINT; stdcall;

function ReadProcessMemory(hProcess: HANDLE; lpBaseAddress: LPCVOID;
  lpBuffer: LPVOID; nSize: DWORD; lpNumberOfBytesRead: LPDWORD): BOOL; stdcall;

function WriteProcessMemory(hProcess: HANDLE; lpBaseAddress: LPVOID;
  lpBuffer: LPVOID; nSize: DWORD; lpNumberOfBytesWritten: LPDWORD): BOOL; stdcall;

function GetThreadContext(hThread: HANDLE; var lpContext: CONTEXT): BOOL; stdcall;

function SetThreadContext(hThread: HANDLE; const lpContext: CONTEXT): BOOL; stdcall;

function SuspendThread(hThread: HANDLE): DWORD; stdcall;

function ResumeThread(hThread: HANDLE): DWORD; stdcall;

type
  PAPCFUNC = procedure (dwParam: ULONG_PTR); stdcall;
  TApcFunc = PAPCFUNC;

function QueueUserAPC(pfnAPC: PAPCFUNC; hThread: HANDLE; dwData: ULONG_PTR): DWORD; stdcall;

function IsDebuggerPresent: BOOL; stdcall;

function CheckRemoteDebuggerPresent(hProcess: HANDLE; out pbDebuggerPresent: BOOL): BOOL; stdcall;

procedure DebugBreak; stdcall;

function WaitForDebugEvent(var lpDebugEvent: DEBUG_EVENT; dwMilliseconds: DWORD): BOOL; stdcall;

function ContinueDebugEvent(dwProcessId: DWORD; dwThreadId: DWORD;
  dwContinueStatus: DWORD): BOOL; stdcall;

function DebugActiveProcess(dwProcessId: DWORD): BOOL; stdcall;

function DebugActiveProcessStop(dwProcessId: DWORD): BOOL; stdcall;

function DebugSetProcessKillOnExit(KillOnExit: BOOL): BOOL; stdcall;

function DebugBreakProcess(Process: HANDLE): BOOL; stdcall;

procedure InitializeCriticalSection(var lpCriticalSection: CRITICAL_SECTION); stdcall;

procedure EnterCriticalSection(var lpCriticalSection: CRITICAL_SECTION); stdcall;

procedure LeaveCriticalSection(var lpCriticalSection: CRITICAL_SECTION); stdcall;

function InitializeCriticalSectionAndSpinCount(var lpCriticalSection: CRITICAL_SECTION;
  dwSpinCount: DWORD): BOOL; stdcall;

function SetCriticalSectionSpinCount(var lpCriticalSection: CRITICAL_SECTION;
  dwSpinCount: DWORD): DWORD; stdcall;

function TryEnterCriticalSection(var lpCriticalSection: CRITICAL_SECTION): BOOL; stdcall;

procedure DeleteCriticalSection(var lpCriticalSection: CRITICAL_SECTION); stdcall;

function SetEvent(hEvent: HANDLE): BOOL; stdcall;

function ResetEvent(hEvent: HANDLE): BOOL; stdcall;

function PulseEvent(hEvent: HANDLE): BOOL; stdcall;

function ReleaseSemaphore(hSemaphore: HANDLE; lReleaseCount: LONG;
  lpPreviousCount: LPLONG): BOOL; stdcall;

function ReleaseMutex(hMutex: HANDLE): BOOL; stdcall;

function WaitForSingleObject(hHandle: HANDLE; dwMilliseconds: DWORD): DWORD; stdcall;

function WaitForMultipleObjects(nCount: DWORD; lpHandles: PHANDLE; bWaitAll: BOOL;
  dwMilliseconds: DWORD): DWORD; stdcall;

procedure Sleep(dwMilliseconds: DWORD); stdcall;

function LoadResource(hModule: HMODULE; hResInfo: HRSRC): HGLOBAL; stdcall;

function SizeofResource(hModule: HMODULE; hResInfo: HRSRC): DWORD; stdcall;

function GlobalDeleteAtom(nAtom: ATOM): ATOM; stdcall;

function InitAtomTable(nSize: DWORD): BOOL; stdcall;

function DeleteAtom(nAtom: ATOM): ATOM; stdcall;

function SetHandleCount(uNumber: UINT): UINT; stdcall;

function GetLogicalDrives: DWORD; stdcall;

function LockFile(hFile: HANDLE; dwFileOffsetLow: DWORD; dwFileOffsetHigh: DWORD;
  nNumberOfBytesToLockLow: DWORD; nNumberOfBytesToLockHigh: DWORD): BOOL; stdcall;

function UnlockFile(hFile: HANDLE; dwFileOffsetLow: DWORD; dwFileOffsetHigh: DWORD;
  nNumberOfBytesToUnlockLow: DWORD; nNumberOfBytesToUnlockHigh: DWORD): BOOL; stdcall;

function LockFileEx(hFile: HANDLE; dwFlags: DWORD; dwReserved: DWORD;
  nNumberOfBytesToLockLow: DWORD; nNumberOfBytesToLockHigh: DWORD;
  const lpOverlapped: OVERLAPPED): BOOL; stdcall;

const
  LOCKFILE_FAIL_IMMEDIATELY = $00000001;
  LOCKFILE_EXCLUSIVE_LOCK   = $00000002;

function UnlockFileEx(hFile: HANDLE; dwReserved: DWORD;
  nNumberOfBytesToUnlockLow: DWORD; nNumberOfBytesToUnlockHigh: DWORD;
  const lpOverlapped: OVERLAPPED): BOOL; stdcall;

type
  PBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;
  _BY_HANDLE_FILE_INFORMATION = record
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    dwVolumeSerialNumber: DWORD;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    nNumberOfLinks: DWORD;
    nFileIndexHigh: DWORD;
    nFileIndexLow: DWORD;
  end;
  BY_HANDLE_FILE_INFORMATION = _BY_HANDLE_FILE_INFORMATION;
  LPBY_HANDLE_FILE_INFORMATION = ^BY_HANDLE_FILE_INFORMATION;
  TByHandleFileInformation = BY_HANDLE_FILE_INFORMATION;
  PByHandleFileInformation = PBY_HANDLE_FILE_INFORMATION;

function GetFileInformationByHandle(hFile: HANDLE;
  var lpFileInformation: BY_HANDLE_FILE_INFORMATION): BOOL; stdcall;

function GetFileType(hFile: HANDLE): DWORD; stdcall;

function GetFileSize(hFile: HANDLE; lpFileSizeHigh: LPDWORD): DWORD; stdcall;

function GetFileSizeEx(hFile: HANDLE; var lpFileSize: LARGE_INTEGER): BOOL; stdcall;

function GetStdHandle(nStdHandle: DWORD): HANDLE; stdcall;

function SetStdHandle(nStdHandle: DWORD; hHandle: HANDLE): BOOL; stdcall;

function WriteFile(hFile: HANDLE; lpBuffer: LPCVOID; nNumberOfBytesToWrite: DWORD;
  lpNumberOfBytesWritten: LPDWORD; lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function ReadFile(hFile: HANDLE; lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  lpNumberOfBytesRead: LPDWORD; lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function FlushFileBuffers(hFile: HANDLE): BOOL; stdcall;

function DeviceIoControl(hDevice: HANDLE; dwIoControlCode: DWORD;
  lpInBuffer: LPVOID; nInBufferSize: DWORD; lpOutBuffer: LPVOID;
  nOutBufferSize: DWORD; lpBytesReturned: LPDWORD;
  lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function RequestDeviceWakeup(hDevice: HANDLE): BOOL; stdcall;

function CancelDeviceWakeupRequest(hDevice: HANDLE): BOOL; stdcall;

function GetDevicePowerState(hDevice: HANDLE; var pfOn: BOOL): BOOL; stdcall;

function SetMessageWaitingIndicator(hMsgIndicator: HANDLE; ulMsgCount: ULONG): BOOL; stdcall;

function SetEndOfFile(hFile: HANDLE): BOOL; stdcall;

function SetFilePointer(hFile: HANDLE; lDistanceToMove: LONG;
  lpDistanceToMoveHigh: PLONG; dwMoveMethod: DWORD): DWORD; stdcall;

function SetFilePointerEx(hFile: HANDLE; liDistanceToMove: LARGE_INTEGER;
  lpNewFilePointer: PLARGE_INTEGER; dwMoveMethod: DWORD): BOOL; stdcall;

function FindClose(hFindFile: HANDLE): BOOL; stdcall;

function GetFileTime(hFile: HANDLE; lpCreationTime, lpLastAccessTime,
  lpLastWriteTime: PFILETIME): BOOL; stdcall;

function SetFileTime(hFile: HANDLE; lpCreationTime, lpLastAccessTime,
  lpLastWriteTime: PFILETIME): BOOL; stdcall;

function SetFileValidData(hFile: HANDLE; ValidDataLength: LONGLONG): BOOL; stdcall;

function SetFileShortNameA(hFile: HANDLE; lpShortName: LPCSTR): BOOL; stdcall;
function SetFileShortNameW(hFile: HANDLE; lpShortName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetFileShortName(hFile: HANDLE; lpShortName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetFileShortName(hFile: HANDLE; lpShortName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function CloseHandle(hObject: HANDLE): BOOL; stdcall;

function DuplicateHandle(hSourceProcessHandle: HANDLE; hSourceHandle: HANDLE;
  hTargetProcessHandle: HANDLE; lpTargetHandle: LPHANDLE;
  dwDesiredAccess: DWORD; bInheritHandle: BOOL; dwOptions: DWORD): BOOL; stdcall;

function GetHandleInformation(hObject: HANDLE; var lpdwFlags: DWORD): BOOL; stdcall;

function SetHandleInformation(hObject: HANDLE; dwMask: DWORD; dwFlags: DWORD): BOOL; stdcall;

const
  HANDLE_FLAG_INHERIT            = $00000001;
  HANDLE_FLAG_PROTECT_FROM_CLOSE = $00000002;

  HINSTANCE_ERROR                = 32;

function LoadModule(lpModuleName: LPCSTR; lpParameterBlock: LPVOID): DWORD; stdcall;

function WinExec(lpCmdLine: LPCSTR; uCmdShow: UINT): UINT; stdcall;

function ClearCommBreak(hFile: HANDLE): BOOL; stdcall;

function ClearCommError(hFile: HANDLE; var lpErrors: DWORD;
  lpStat: LPCOMSTAT): BOOL; stdcall;

function SetupComm(hFile: HANDLE; dwInQueue: DWORD; dwOutQueue: DWORD): BOOL; stdcall;

function EscapeCommFunction(hFile: HANDLE; dwFunc: DWORD): BOOL; stdcall;

function GetCommConfig(hCommDev: HANDLE; var lpCC: COMMCONFIG;
  var lpdwSize: DWORD): BOOL; stdcall;

function GetCommMask(hFile: HANDLE; var lpEvtMask: DWORD): BOOL; stdcall;

function GetCommProperties(hFile: HANDLE; var lpCommProp: COMMPROP): BOOL; stdcall;

function GetCommModemStatus(hFile: HANDLE; var lpModemStat: DWORD): BOOL; stdcall;

function GetCommState(hFile: HANDLE; var lpDCB: DCB): BOOL; stdcall;

function GetCommTimeouts(hFile: HANDLE; var lpCommTimeouts: COMMTIMEOUTS): BOOL; stdcall;

function PurgeComm(hFile: HANDLE; dwFlags: DWORD): BOOL; stdcall;

function SetCommBreak(hFile: HANDLE): BOOL; stdcall;

function SetCommConfig(hCommDev: HANDLE; const lpCC: COMMCONFIG; dwSize: DWORD): BOOL; stdcall;

function SetCommMask(hFile: HANDLE; dwEvtMask: DWORD): BOOL; stdcall;

function SetCommState(hFile: HANDLE; const lpDCB: DCB): BOOL; stdcall;

function SetCommTimeouts(hFile: HANDLE; const lpCommTimeouts: COMMTIMEOUTS): BOOL; stdcall;

function TransmitCommChar(hFile: HANDLE; cChar: Char): BOOL; stdcall;

function WaitCommEvent(hFile: HANDLE; var lpEvtMask: DWORD;
  lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function SetTapePosition(hDevice: HANDLE; dwPositionMethod, dwPartition,
  dwOffsetLow, dwOffsetHigh: DWORD; bImmediate: BOOL): DWORD; stdcall;

function GetTapePosition(hDevice: HANDLE; dwPositionType: DWORD;
  var lpdwPartition, lpdwOffsetLow: LPDWORD; lpdwOffsetHigh: LPDWORD): DWORD; stdcall;

function PrepareTape(hDevice: HANDLE; dwOperation: DWORD; bImmediate: BOOL): DWORD; stdcall;

function EraseTape(hDevice: HANDLE; dwEraseType: DWORD; bImmediate: BOOL): DWORD; stdcall;

function CreateTapePartition(hDevice: HANDLE; dwPartitionMethod: DWORD;
  dwCount: DWORD; dwSize: DWORD): DWORD; stdcall;

function WriteTapemark(hDevice: HANDLE; dwTapemarkType: DWORD;
  dwTapemarkCount: DWORD; bImmediate: BOOL): DWORD; stdcall;

function GetTapeStatus(hDevice: HANDLE): DWORD; stdcall;

function GetTapeParameters(hDevice: HANDLE; dwOperation: DWORD;
  var lpdwSize: DWORD; lpTapeInformation: LPVOID): DWORD; stdcall;

const
  GET_TAPE_MEDIA_INFORMATION = 0;
  GET_TAPE_DRIVE_INFORMATION = 1;

function SetTapeParameters(hDevice: HANDLE; dwOperation: DWORD; lpTapeInformation: LPVOID): DWORD; stdcall;

const
  SET_TAPE_MEDIA_INFORMATION = 0;
  SET_TAPE_DRIVE_INFORMATION = 1;

function Beep(dwFreq: DWORD; dwDuration: DWORD): BOOL; stdcall;

function MulDiv(nNumber, nNumerator, nDenominator: Integer): Integer; stdcall;

procedure GetSystemTime(var lpSystemTime: SYSTEMTIME); stdcall;

procedure GetSystemTimeAsFileTime(var lpSystemTimeAsFileTime: FILETIME); stdcall;

function SetSystemTime(var lpSystemTime: SYSTEMTIME): BOOL; stdcall;

procedure GetLocalTime(var lpSystemTime: SYSTEMTIME); stdcall;

function SetLocalTime(var lpSystemTime: SYSTEMTIME): BOOL; stdcall;

procedure GetSystemInfo(var lpSystemInfo: SYSTEM_INFO); stdcall;

function GetSystemRegistryQuota(out pdwQuotaAllowed, pdwQuotaUsed: DWORD): BOOL; stdcall;

function GetSystemTimes(lpIdleTime, lpKernelTime, lpUserTime: LPFILETIME): BOOL; stdcall;

procedure GetNativeSystemInfo(lpSystemInfo: LPSYSTEM_INFO); stdcall;

function IsProcessorFeaturePresent(ProcessorFeature: DWORD): BOOL; stdcall;

type
  PTIME_ZONE_INFORMATION = ^TIME_ZONE_INFORMATION;
  _TIME_ZONE_INFORMATION = record
    Bias: LONG;
    StandardName: array [0..31] of WCHAR;
    StandardDate: SYSTEMTIME;
    StandardBias: LONG;
    DaylightName: array [0..31] of WCHAR;
    DaylightDate: SYSTEMTIME;
    DaylightBias: LONG;
  end;
  TIME_ZONE_INFORMATION = _TIME_ZONE_INFORMATION;
  LPTIME_ZONE_INFORMATION = ^TIME_ZONE_INFORMATION;
  TTimeZoneInformation = TIME_ZONE_INFORMATION;
  PTimeZoneInformation = PTIME_ZONE_INFORMATION;

function SystemTimeToTzSpecificLocalTime(lpTimeZoneInformation: LPTIME_ZONE_INFORMATION;
  var lpUniversalTime, lpLocalTime: SYSTEMTIME): BOOL; stdcall;

function TzSpecificLocalTimeToSystemTime(const lpTimeZoneInformation: TIME_ZONE_INFORMATION;
  const lpLocalTime: SYSTEMTIME; var lpUniversalTime: SYSTEMTIME): BOOL; stdcall;

function GetTimeZoneInformation(var lpTimeZoneInformation: TIME_ZONE_INFORMATION): DWORD; stdcall;

function SetTimeZoneInformation(const lpTimeZoneInformation: TIME_ZONE_INFORMATION): BOOL; stdcall;

//
// Routines to convert back and forth between system time and file time
//

function SystemTimeToFileTime(const lpSystemTime: SYSTEMTIME; var lpFileTime: FILETIME): BOOL; stdcall;

function FileTimeToLocalFileTime(const lpFileTime: FILETIME; var lpLocalFileTime: FILETIME): BOOL; stdcall;

function LocalFileTimeToFileTime(const lpLocalFileTime: FILETIME; var lpFileTime: FILETIME): BOOL; stdcall;

function FileTimeToSystemTime(const lpFileTime: FILETIME; var lpSystemTime: SYSTEMTIME): BOOL; stdcall;

function CompareFileTime(const lpFileTime1, lpFileTime2: FILETIME): LONG; stdcall;

function FileTimeToDosDateTime(const lpFileTime: FILETIME; var lpFatDate,
  lpFatTime: WORD): BOOL; stdcall;

function DosDateTimeToFileTime(wFatDate, wFatTime: WORD; var lpFileTime: FILETIME): BOOL; stdcall;

function GetTickCount: DWORD; stdcall;

function SetSystemTimeAdjustment(dwTimeAdjustment: DWORD;
  bTimeAdjustmentDisabled: BOOL): BOOL; stdcall;

function GetSystemTimeAdjustment(var lpTimeAdjustment, lpTimeIncrement: DWORD;
  var lpTimeAdjustmentDisabled: BOOL): BOOL; stdcall;

function FormatMessageA(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD;
  dwLanguageId: DWORD; lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD; stdcall;
function FormatMessageW(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD;
  dwLanguageId: DWORD; lpBuffer: LPWSTR; nSize: DWORD; Arguments: Pointer): DWORD; stdcall;

{$IFDEF UNICODE}
function FormatMessage(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD;
  dwLanguageId: DWORD; lpBuffer: LPWSTR; nSize: DWORD; Arguments: Pointer): DWORD; stdcall;
{$ELSE}
function FormatMessage(dwFlags: DWORD; lpSource: LPCVOID; dwMessageId: DWORD;
  dwLanguageId: DWORD; lpBuffer: LPSTR; nSize: DWORD; Arguments: Pointer): DWORD; stdcall;
{$ENDIF}

const
  FORMAT_MESSAGE_ALLOCATE_BUFFER = $00000100;
  FORMAT_MESSAGE_IGNORE_INSERTS  = $00000200;
  FORMAT_MESSAGE_FROM_STRING     = $00000400;
  FORMAT_MESSAGE_FROM_HMODULE    = $00000800;
  FORMAT_MESSAGE_FROM_SYSTEM     = $00001000;
  FORMAT_MESSAGE_ARGUMENT_ARRAY  = $00002000;
  FORMAT_MESSAGE_MAX_WIDTH_MASK  = $000000FF;

function CreatePipe(var hReadPipe, hWritePipe: HANDLE;
  lpPipeAttributes: LPSECURITY_ATTRIBUTES; nSize: DWORD): BOOL; stdcall;

function ConnectNamedPipe(hNamedPipe: HANDLE; lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function DisconnectNamedPipe(hNamedPipe: HANDLE): BOOL; stdcall;

function SetNamedPipeHandleState(hNamedPipe: HANDLE; var lpMode: DWORD;
  lpMaxCollectionCount: LPDWORD; lpCollectDataTimeout: LPDWORD): BOOL; stdcall;

function GetNamedPipeInfo(hNamedPipe: HANDLE; lpFlags, lpOutBufferSize,
  lpInBufferSize, lpMaxInstances: LPDWORD): BOOL; stdcall;

function PeekNamedPipe(hNamedPipe: HANDLE; lpBuffer: LPVOID; nBufferSize: DWORD;
  lpBytesRead, lpTotalBytesAvail, lpBytesLeftThisMessage: LPDWORD): BOOL; stdcall;

function TransactNamedPipe(hNamedPipe: HANDLE; lpInBuffer: LPVOID;
  nInBufferSize: DWORD; lpOutBuffer: LPVOID; nOutBufferSize: DWORD;
  lpBytesRead: LPDWORD; lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function CreateMailslotA(lpName: LPCSTR; nMaxMessageSize, lReadTimeout: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;
function CreateMailslotW(lpName: LPCWSTR; nMaxMessageSize, lReadTimeout: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateMailslot(lpName: LPCWSTR; nMaxMessageSize, lReadTimeout: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;
{$ELSE}
function CreateMailslot(lpName: LPCSTR; nMaxMessageSize, lReadTimeout: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;
{$ENDIF}

function GetMailslotInfo(hMailslot: HANDLE; lpMaxMessageSize, lpNextSize,
  lpMessageCount, lpReadTimeout: LPDWORD): BOOL; stdcall;

function SetMailslotInfo(hMailslot: HANDLE; lReadTimeout: DWORD): BOOL; stdcall;

function MapViewOfFile(hFileMappingObject: HANDLE; dwDesiredAccess: DWORD;
  dwFileOffsetHigh, dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: SIZE_T): LPVOID; stdcall;

function FlushViewOfFile(lpBaseAddress: LPCVOID; dwNumberOfBytesToFlush: SIZE_T): BOOL; stdcall;

function UnmapViewOfFile(lpBaseAddress: LPCVOID): BOOL; stdcall;

//
// File Encryption API
//

function EncryptFileA(lpFileName: LPCSTR): BOOL; stdcall;
function EncryptFileW(lpFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EncryptFile(lpFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function EncryptFile(lpFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function DecryptFileA(lpFileName: LPCSTR; dwReserved: DWORD): BOOL; stdcall;
function DecryptFileW(lpFileName: LPCWSTR; dwReserved: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function DecryptFile(lpFileName: LPCWSTR; dwReserved: DWORD): BOOL; stdcall;
{$ELSE}
function DecryptFile(lpFileName: LPCSTR; dwReserved: DWORD): BOOL; stdcall;
{$ENDIF}

//
//  Encryption Status Value
//

const
  FILE_ENCRYPTABLE        = 0;
  FILE_IS_ENCRYPTED       = 1;
  FILE_SYSTEM_ATTR        = 2;
  FILE_ROOT_DIR           = 3;
  FILE_SYSTEM_DIR         = 4;
  FILE_UNKNOWN            = 5;
  FILE_SYSTEM_NOT_SUPPORT = 6;
  FILE_USER_DISALLOWED    = 7;
  FILE_READ_ONLY          = 8;
  FILE_DIR_DISALLOWED     = 9;

function FileEncryptionStatusA(lpFileName: LPCSTR; var lpStatus: DWORD): BOOL; stdcall;
function FileEncryptionStatusW(lpFileName: LPCWSTR; var lpStatus: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function FileEncryptionStatus(lpFileName: LPCWSTR; var lpStatus: DWORD): BOOL; stdcall;
{$ELSE}
function FileEncryptionStatus(lpFileName: LPCSTR; var lpStatus: DWORD): BOOL; stdcall;
{$ENDIF}

//
// Currently defined recovery flags
//

const
  EFS_USE_RECOVERY_KEYS = ($1);

type
  PFE_EXPORT_FUNC = function (pbData: PBYTE; pvCallbackContext: PVOID;
    ulLength: ULONG): DWORD; stdcall;

  PFE_IMPORT_FUNC = function (pbData: PBYTE; pvCallbackContext: PVOID;
    ulLength: PULONG): DWORD; stdcall;

//
//  OpenRaw flag values
//

const
  CREATE_FOR_IMPORT = (1);
  CREATE_FOR_DIR    = (2);
  OVERWRITE_HIDDEN  = (4);

function OpenEncryptedFileRawA(lpFileName: LPCSTR; ulFlags: ULONG;
  pvContext: PVOID): DWORD; stdcall;
function OpenEncryptedFileRawW(lpFileName: LPCWSTR; ulFlags: ULONG;
  pvContext: PVOID): DWORD; stdcall;

{$IFDEF UNICODE}
function OpenEncryptedFileRaw(lpFileName: LPCWSTR; ulFlags: ULONG;
  pvContext: PVOID): DWORD; stdcall;
{$ELSE}
function OpenEncryptedFileRaw(lpFileName: LPCSTR; ulFlags: ULONG;
  pvContext: PVOID): DWORD; stdcall;
{$ENDIF}

function ReadEncryptedFileRaw(pfExportCallback: PFE_EXPORT_FUNC;
  pvCallbackContext: PVOID; pvContext: PVOID): DWORD; stdcall;

function WriteEncryptedFileRaw(pfImportCallback: PFE_IMPORT_FUNC;
  pvCallbackContext: PVOID; pvContext: PVOID): DWORD; stdcall;

procedure CloseEncryptedFileRaw(pvContext: PVOID); stdcall;

//
// _l Compat Functions
//

function lstrcmpA(lpString1, lpString2: LPCSTR): Integer; stdcall;
function lstrcmpW(lpString1, lpString2: LPCWSTR): Integer; stdcall;

{$IFDEF UNICODE}
function lstrcmp(lpString1, lpString2: LPCWSTR): Integer; stdcall;
{$ELSE}
function lstrcmp(lpString1, lpString2: LPCSTR): Integer; stdcall;
{$ENDIF}

function lstrcmpiA(lpString1, lpString2: LPCSTR): Integer; stdcall;
function lstrcmpiW(lpString1, lpString2: LPCWSTR): Integer; stdcall;

{$IFDEF UNICODE}
function lstrcmpi(lpString1, lpString2: LPCWSTR): Integer; stdcall;
{$ELSE}
function lstrcmpi(lpString1, lpString2: LPCSTR): Integer; stdcall;
{$ENDIF}

function lstrcpynA(lpString1: LPSTR; lpString2: LPCSTR; iMaxLength: Integer): LPSTR; stdcall;
function lstrcpynW(lpString1: LPWSTR; lpString2: LPCWSTR; iMaxLength: Integer): LPWSTR; stdcall;

{$IFDEF UNICODE}
function lstrcpyn(lpString1: LPWSTR; lpString2: LPCWSTR; iMaxLength: Integer): LPWSTR; stdcall;
{$ELSE}
function lstrcpyn(lpString1: LPSTR; lpString2: LPCSTR; iMaxLength: Integer): LPSTR; stdcall;
{$ENDIF}

function lstrcpyA(lpString1: LPSTR; lpString2: LPCSTR): LPSTR; stdcall;
function lstrcpyW(lpString1: LPWSTR; lpString2: LPCWSTR): LPWSTR; stdcall;

{$IFDEF UNICODE}
function lstrcpy(lpString1: LPWSTR; lpString2: LPCWSTR): LPWSTR; stdcall;
{$ELSE}
function lstrcpy(lpString1: LPSTR; lpString2: LPCSTR): LPSTR; stdcall;
{$ENDIF}

function lstrcatA(lpString1: LPSTR; lpString2: LPCSTR): LPSTR; stdcall;
function lstrcatW(lpString1: LPWSTR; lpString2: LPCWSTR): LPWSTR; stdcall;

{$IFDEF UNICODE}
function lstrcat(lpString1: LPWSTR; lpString2: LPCWSTR): LPWSTR; stdcall;
{$ELSE}
function lstrcat(lpString1: LPSTR; lpString2: LPCSTR): LPSTR; stdcall;
{$ENDIF}

function lstrlenA(lpString: LPCSTR): Integer; stdcall;
function lstrlenW(lpString: LPCWSTR): Integer; stdcall;

{$IFDEF UNICODE}
function lstrlen(lpString: LPCWSTR): Integer; stdcall;
{$ELSE}
function lstrlen(lpString: LPCSTR): Integer; stdcall;
{$ENDIF}

function OpenFile(lpFileName: LPCSTR; var lpReOpenBuff: OFSTRUCT; uStyle: UINT): HFILE; stdcall;

function _lopen(lpPathName: LPCSTR; iReadWrite: Integer): HFILE; stdcall;

function _lcreat(lpPathName: LPCSTR; iAttribute: Integer): HFILE; stdcall;

function _lread(hFile: HFILE; lpBuffer: LPVOID; uBytes: UINT): UINT; stdcall;

function _lwrite(hFile: HFILE; lpBuffer: LPCSTR; uBytes: UINT): UINT; stdcall;

function _hread(hFile: HFILE; lpBuffer: LPVOID; lBytes: Longint): Longint; stdcall;

function _hwrite(hFile: HFILE; lpBuffer: LPCSTR; lBytes: Longint): Longint; stdcall;

function _lclose(hFile: HFILE): HFILE; stdcall;

function _llseek(hFile: HFILE; lOffset: LONG; iOrigin: Integer): LONG; stdcall;

function IsTextUnicode(lpBuffer: LPVOID; cb: Integer; lpi: LPINT): BOOL; stdcall;

type
  PFLS_CALLBACK_FUNCTION = procedure (lpFlsData: PVOID); stdcall;
  TFlsCallbackFunction = PFLS_CALLBACK_FUNCTION;

const
  FLS_OUT_OF_INDEXES = DWORD($FFFFFFFF);

function FlsAlloc(lpCallback: PFLS_CALLBACK_FUNCTION): DWORD; stdcall;

function FlsGetValue(dwFlsIndex: DWORD): DWORD; stdcall;

function FlsSetValue(dwFlsIndex: DWORD; lpFlsData: PVOID): DWORD; stdcall;

function FlsFree(dwFlsIndex: DWORD): DWORD; stdcall;

const
  TLS_OUT_OF_INDEXES = DWORD($FFFFFFFF);

function TlsAlloc: DWORD; stdcall;

function TlsGetValue(dwTlsIndex: DWORD): LPVOID; stdcall;

function TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: LPVOID): BOOL; stdcall;

function TlsFree(dwTlsIndex: DWORD): BOOL; stdcall;

type
  LPOVERLAPPED_COMPLETION_ROUTINE = procedure (dwErrorCode: DWORD;
    dwNumberOfBytesTransfered: DWORD; lpOverlapped: LPOVERLAPPED); stdcall;
  TOverlappedCompletionRoutine = LPOVERLAPPED_COMPLETION_ROUTINE;

function SleepEx(dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;

function WaitForSingleObjectEx(hHandle: HANDLE; dwMilliseconds: DWORD;
  bAlertable: BOOL): DWORD; stdcall;

function WaitForMultipleObjectsEx(nCount: DWORD; lpHandles: PHANDLE;
  bWaitAll: BOOL; dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;

function SignalObjectAndWait(hObjectToSignal: HANDLE; hObjectToWaitOn: HANDLE;
  dwMilliseconds: DWORD; bAlertable: BOOL): DWORD; stdcall;

function ReadFileEx(hFile: HANDLE; lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  lpOverlapped: LPOVERLAPPED; lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL; stdcall;

function WriteFileEx(hFile: HANDLE; lpBuffer: LPCVOID; nNumberOfBytesToWrite: DWORD;
  lpOverlapped: LPOVERLAPPED; lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL; stdcall;

function BackupRead(hFile: HANDLE; lpBuffer: LPBYTE;
  nNumberOfBytesToRead: DWORD; var lpNumberOfBytesRead: DWORD;
  bAbort, bProcessSecurity: BOOL; var lpContext: LPVOID): BOOL; stdcall;

function BackupSeek(hFile: HANDLE; dwLowBytesToSeek, dwHighBytesToSeek: DWORD;
  var lpdwLowByteSeeked, lpdwHighByteSeeked: DWORD; var lpContext: LPVOID): BOOL; stdcall;

function BackupWrite(hFile: HANDLE; lpBuffer: LPBYTE; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; bAbort, bProcessSecurity: BOOL;
  var lpContext: LPVOID): BOOL; stdcall;

//
//  Stream id structure
//

type
  LPWIN32_STREAM_ID = ^WIN32_STREAM_ID;
  _WIN32_STREAM_ID = record
    dwStreamId: DWORD;
    dwStreamAttributes: DWORD;
    Size: TLargeInteger;
    dwStreamNameSize: DWORD;
    cStreamName: array [0..ANYSIZE_ARRAY - 1] of WCHAR;
  end;
  WIN32_STREAM_ID = _WIN32_STREAM_ID;
  TWin32StreamId = WIN32_STREAM_ID;
  PWin32StreamId = LPWIN32_STREAM_ID;

//
//  Stream Ids
//

const
  BACKUP_INVALID        = $00000000;
  BACKUP_DATA           = $00000001;
  BACKUP_EA_DATA        = $00000002;
  BACKUP_SECURITY_DATA  = $00000003;
  BACKUP_ALTERNATE_DATA = $00000004;
  BACKUP_LINK           = $00000005;
  BACKUP_PROPERTY_DATA  = $00000006;
  BACKUP_OBJECT_ID      = $00000007;
  BACKUP_REPARSE_DATA   = $00000008;
  BACKUP_SPARSE_BLOCK   = $00000009;

//
//  Stream Attributes
//

const
  STREAM_NORMAL_ATTRIBUTE    = $00000000;
  STREAM_MODIFIED_WHEN_READ  = $00000001;
  STREAM_CONTAINS_SECURITY   = $00000002;
  STREAM_CONTAINS_PROPERTIES = $00000004;
  STREAM_SPARSE_ATTRIBUTE    = $00000008;

function ReadFileScatter(hFile: HANDLE; aSegmentArray: PFILE_SEGMENT_ELEMENT;
  nNumberOfBytesToRead: DWORD; lpReserved: LPDWORD; lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

function WriteFileGather(hFile: HANDLE; aSegmentArray: PFILE_SEGMENT_ELEMENT;
  nNumberOfBytesToWrite: DWORD; lpReserved: LPDWORD; lpOverlapped: LPOVERLAPPED): BOOL; stdcall;

//
// Dual Mode API below this line. Dual Mode Structures also included.
//

const
  STARTF_USESHOWWINDOW    = $00000001;
  STARTF_USESIZE          = $00000002;
  STARTF_USEPOSITION      = $00000004;
  STARTF_USECOUNTCHARS    = $00000008;
  STARTF_USEFILLATTRIBUTE = $00000010;
  STARTF_RUNFULLSCREEN    = $00000020; // ignored for non-x86 platforms
  STARTF_FORCEONFEEDBACK  = $00000040;
  STARTF_FORCEOFFFEEDBACK = $00000080;
  STARTF_USESTDHANDLES    = $00000100;

  STARTF_USEHOTKEY = $00000200;

type
  LPSTARTUPINFOA = ^STARTUPINFOA;
  _STARTUPINFOA = record
    cb: DWORD;
    lpReserved: LPSTR;
    lpDesktop: LPSTR;
    lpTitle: LPSTR;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: WORD;
    cbReserved2: WORD;
    lpReserved2: LPBYTE;
    hStdInput: HANDLE;
    hStdOutput: HANDLE;
    hStdError: HANDLE;
  end;
  STARTUPINFOA = _STARTUPINFOA;
  TStartupInfoA = STARTUPINFOA;
  PStartupInfoA = LPSTARTUPINFOA;

  LPSTARTUPINFOW = ^STARTUPINFOW;
  _STARTUPINFOW = record
    cb: DWORD;
    lpReserved: LPWSTR;
    lpDesktop: LPWSTR;
    lpTitle: LPWSTR;
    dwX: DWORD;
    dwY: DWORD;
    dwXSize: DWORD;
    dwYSize: DWORD;
    dwXCountChars: DWORD;
    dwYCountChars: DWORD;
    dwFillAttribute: DWORD;
    dwFlags: DWORD;
    wShowWindow: WORD;
    cbReserved2: WORD;
    lpReserved2: LPBYTE;
    hStdInput: HANDLE;
    hStdOutput: HANDLE;
    hStdError: HANDLE;
  end;
  STARTUPINFOW = _STARTUPINFOW;
  TStartupInfoW = STARTUPINFOW;
  PStartupInfoW = LPSTARTUPINFOW;

{$IFDEF UNICODE}
  STARTUPINFO = STARTUPINFOW;
  LPSTARTUPINFO = LPSTARTUPINFOW;
  TStartupInfo = TStartupInfoW;
  PStartupInfo = PStartupInfoW;
{$ELSE}
  STARTUPINFO = STARTUPINFOA;
  LPSTARTUPINFO = LPSTARTUPINFOA;
  TStartupInfo = TStartupInfoA;
  PStartupInfo = PStartupInfoA;
{$ENDIF}

const
  SHUTDOWN_NORETRY = $00000001;

type
  PWIN32_FIND_DATAA = ^WIN32_FIND_DATAA;
  _WIN32_FIND_DATAA = record
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array [0..MAX_PATH - 1] of CHAR;
    cAlternateFileName: array [0..13] of CHAR;
  end;
  WIN32_FIND_DATAA = _WIN32_FIND_DATAA;
  LPWIN32_FIND_DATAA = ^WIN32_FIND_DATAA;
  TWin32FindDataA = WIN32_FIND_DATAA;
  PWin32FindDataA = PWIN32_FIND_DATAA;

  PWIN32_FIND_DATAW = ^WIN32_FIND_DATAW;
  _WIN32_FIND_DATAW = record
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array [0..MAX_PATH - 1] of WCHAR;
    cAlternateFileName: array [0..13] of WCHAR;
  end;
  WIN32_FIND_DATAW = _WIN32_FIND_DATAW;
  LPWIN32_FIND_DATAW = ^WIN32_FIND_DATAW;
  TWin32FindDataW = WIN32_FIND_DATAW;
  PWin32FindDataW = PWIN32_FIND_DATAW;

{$IFDEF UNICODE}
  WIN32_FIND_DATA = WIN32_FIND_DATAW;
  PWIN32_FIND_DATA = PWIN32_FIND_DATAW;
  LPWIN32_FIND_DATA = LPWIN32_FIND_DATAW;
  TWin32FindData = TWin32FindDataW;
  PWin32FindData = PWin32FindDataW;
{$ELSE}
  WIN32_FIND_DATA = WIN32_FIND_DATAA;
  PWIN32_FIND_DATA = PWIN32_FIND_DATAA;
  LPWIN32_FIND_DATA = LPWIN32_FIND_DATAA;
  TWin32FindData = TWin32FindDataA;
  PWin32FindData = PWin32FindDataA;
{$ENDIF}

  LPWIN32_FILE_ATTRIBUTE_DATA = ^WIN32_FILE_ATTRIBUTE_DATA;
  _WIN32_FILE_ATTRIBUTE_DATA = record
    dwFileAttributes: DWORD;
    ftCreationTime: FILETIME;
    ftLastAccessTime: FILETIME;
    ftLastWriteTime: FILETIME;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
  end;
  WIN32_FILE_ATTRIBUTE_DATA = _WIN32_FILE_ATTRIBUTE_DATA;
  TWin32FileAttributeData = WIN32_FILE_ATTRIBUTE_DATA;
  PWin32FileAttributeData = LPWIN32_FILE_ATTRIBUTE_DATA;

function CreateMutexA(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCSTR): HANDLE;
function CreateMutexW(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCWSTR): HANDLE;

{$IFDEF UNICODE}
function CreateMutex(lpMutexAttributes: LPSECURITY_ATTRIBUTES;
  bInitialOwner: BOOL; lpName: LPCWSTR): HANDLE;
{$ELSE}
function CreateMutex(lpMutexAttributes: LPSECURITY_ATTRIBUTES;
  bInitialOwner: BOOL; lpName: LPCSTR): HANDLE;
{$ENDIF}

function OpenMutexA(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall;
function OpenMutexW(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenMutex(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenMutex(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function CreateEventA(lpEventAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset, bInitialState: BOOL; lpName: LPCSTR): HANDLE; stdcall;
function CreateEventW(lpEventAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset, bInitialState: BOOL; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateEvent(lpEventAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset, bInitialState: BOOL; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function CreateEvent(lpEventAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset, bInitialState: BOOL; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function OpenEventA(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall;
function OpenEventW(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenEvent(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenEvent(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function CreateSemaphoreA(lpSemaphoreAttributes: LPSECURITY_ATTRIBUTES;
  lInitialCount, lMaximumCount: LONG; lpName: LPCSTR): HANDLE; stdcall;
function CreateSemaphoreW(lpSemaphoreAttributes: LPSECURITY_ATTRIBUTES;
  lInitialCount, lMaximumCount: LONG; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateSemaphore(lpSemaphoreAttributes: LPSECURITY_ATTRIBUTES;
  lInitialCount, lMaximumCount: LONG; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function CreateSemaphore(lpSemaphoreAttributes: LPSECURITY_ATTRIBUTES;
  lInitialCount, lMaximumCount: LONG; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function OpenSemaphoreA(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCSTR): HANDLE; stdcall;
function OpenSemaphoreW(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenSemaphore(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenSemaphore(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

type
  PTIMERAPCROUTINE = procedure (lpArgToCompletionRoutine: LPVOID;
    dwTimerLowValue, dwTimerHighValue: DWORD); stdcall;
  TTimerApcRoutine = PTIMERAPCROUTINE;

function CreateWaitableTimerA(lpTimerAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset: BOOL; lpTimerName: LPCSTR): HANDLE; stdcall;
function CreateWaitableTimerW(lpTimerAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset: BOOL; lpTimerName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateWaitableTimer(lpTimerAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset: BOOL; lpTimerName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function CreateWaitableTimer(lpTimerAttributes: LPSECURITY_ATTRIBUTES;
  bManualReset: BOOL; lpTimerName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function OpenWaitableTimerA(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpTimerName: LPCSTR): HANDLE; stdcall;
function OpenWaitableTimerW(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpTimerName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenWaitableTimer(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpTimerName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenWaitableTimer(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpTimerName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function SetWaitableTimer(hTimer: HANDLE; var lpDueTime: LARGE_INTEGER;
  lPeriod: LONG; pfnCompletionRoutine: PTIMERAPCROUTINE;
  lpArgToCompletionRoutine: LPVOID; fResume: BOOL): BOOL; stdcall;

function CancelWaitableTimer(hTimer: HANDLE): BOOL; stdcall;

function CreateFileMappingA(hFile: HANDLE; lpFileMappingAttributes: LPSECURITY_ATTRIBUTES;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: LPCSTR): HANDLE; stdcall;
function CreateFileMappingW(hFile: HANDLE; lpFileMappingAttributes: LPSECURITY_ATTRIBUTES;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateFileMapping(hFile: HANDLE; lpFileMappingAttributes: LPSECURITY_ATTRIBUTES;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function CreateFileMapping(hFile: HANDLE; lpFileMappingAttributes: LPSECURITY_ATTRIBUTES;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function OpenFileMappingA(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCSTR): HANDLE; stdcall;
function OpenFileMappingW(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenFileMapping(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenFileMapping(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function GetLogicalDriveStringsA(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
function GetLogicalDriveStringsW(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetLogicalDriveStrings(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetLogicalDriveStrings(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
{$ENDIF}

type
  _MEMORY_RESOURCE_NOTIFICATION_TYPE = (
    LowMemoryResourceNotification,
    HighMemoryResourceNotification);
  MEMORY_RESOURCE_NOTIFICATION_TYPE = _MEMORY_RESOURCE_NOTIFICATION_TYPE;
  TMemoryResourceNotification = MEMORY_RESOURCE_NOTIFICATION_TYPE;

function CreateMemoryResourceNotification(NotificationType: MEMORY_RESOURCE_NOTIFICATION_TYPE): HANDLE; stdcall;

function QueryMemoryResourceNotification(ResourceNotificationHandle: HANDLE;
  ResourceState: BOOL): BOOL; stdcall;

function LoadLibraryA(lpLibFileName: LPCSTR): HMODULE; stdcall;
function LoadLibraryW(lpLibFileName: LPCWSTR): HMODULE; stdcall;

{$IFDEF UNICODE}
function LoadLibrary(lpLibFileName: LPCWSTR): HMODULE; stdcall;
{$ELSE}
function LoadLibrary(lpLibFileName: LPCSTR): HMODULE; stdcall;
{$ENDIF}

function LoadLibraryExA(lpLibFileName: LPCSTR; hFile: HANDLE; dwFlags: DWORD): HMODULE; stdcall;
function LoadLibraryExW(lpLibFileName: LPCWSTR; hFile: HANDLE; dwFlags: DWORD): HMODULE; stdcall;

{$IFDEF UNICODE}
function LoadLibraryEx(lpLibFileName: LPCWSTR; hFile: HANDLE; dwFlags: DWORD): HMODULE; stdcall;
{$ELSE}
function LoadLibraryEx(lpLibFileName: LPCSTR; hFile: HANDLE; dwFlags: DWORD): HMODULE; stdcall;
{$ENDIF}

const
  DONT_RESOLVE_DLL_REFERENCES   = $00000001;
  LOAD_LIBRARY_AS_DATAFILE      = $00000002;
  LOAD_WITH_ALTERED_SEARCH_PATH = $00000008;
  LOAD_IGNORE_CODE_AUTHZ_LEVEL  = $00000010;

function GetModuleFileNameA(hModule: HMODULE; lpFilename: LPSTR; nSize: DWORD): DWORD; stdcall;
function GetModuleFileNameW(hModule: HMODULE; lpFilename: LPWSTR; nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetModuleFileName(hModule: HMODULE; lpFilename: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$ELSE}
function GetModuleFileName(hModule: HMODULE; lpFilename: LPSTR; nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function GetModuleHandleA(lpModuleName: LPCSTR): HMODULE; stdcall;
function GetModuleHandleW(lpModuleName: LPCWSTR): HMODULE; stdcall;

{$IFDEF UNICODE}
function GetModuleHandle(lpModuleName: LPCWSTR): HMODULE; stdcall;
{$ELSE}
function GetModuleHandle(lpModuleName: LPCSTR): HMODULE; stdcall;
{$ENDIF}

const
  GET_MODULE_HANDLE_EX_FLAG_PIN                = ($00000001);
  GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT = ($00000002);
  GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS       = ($00000004);

type
  PGET_MODULE_HANDLE_EXA = function (dwFlags: DWORD; lpModuleName: LPCSTR; var phModule: HMODULE): BOOL; stdcall;
  PGET_MODULE_HANDLE_EXW = function (dwFlags: DWORD; lpModuleName: LPCWSTR; var phModule: HMODULE): BOOL; stdcall;

{$IFDEF UNICODE}
  PGET_MODULE_HANDLE_EX = PGET_MODULE_HANDLE_EXW;
{$ELSE}
  PGET_MODULE_HANDLE_EX = PGET_MODULE_HANDLE_EXA;
{$ENDIF}

function GetModuleHandleExA(dwFlags: DWORD; lpModuleName: LPCSTR; var phModule: HMODULE): BOOL; stdcall;
function GetModuleHandleExW(dwFlags: DWORD; lpModuleName: LPCWSTR; var phModule: HMODULE): BOOL; stdcall;

{$IFDEF UNICODE}
function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: LPCWSTR; var phModule: HMODULE): BOOL; stdcall;
{$ELSE}
function GetModuleHandleEx(dwFlags: DWORD; lpModuleName: LPCSTR; var phModule: HMODULE): BOOL; stdcall;
{$ENDIF}

function NeedCurrentDirectoryForExePathA(ExeName: LPCSTR): BOOL; stdcall;
function NeedCurrentDirectoryForExePathW(ExeName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function NeedCurrentDirectoryForExePath(ExeName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function NeedCurrentDirectoryForExePath(ExeName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function CreateProcessA(lpApplicationName: LPCSTR; lpCommandLine: LPSTR;
  lpProcessAttributes, lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID;
  lpCurrentDirectory: LPCSTR; const lpStartupInfo: STARTUPINFOA;
  var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;
function CreateProcessW(lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  lpProcessAttributes, lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID;
  lpCurrentDirectory: LPCWSTR; const lpStartupInfo: STARTUPINFOW;
  var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;

{$IFDEF UNICODE}
function CreateProcess(lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  lpProcessAttributes, lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID;
  lpCurrentDirectory: LPCWSTR; const lpStartupInfo: STARTUPINFOW;
  var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;
{$ELSE}
function CreateProcess(lpApplicationName: LPCSTR; lpCommandLine: LPSTR;
  lpProcessAttributes, lpThreadAttributes: LPSECURITY_ATTRIBUTES;
  bInheritHandles: BOOL; dwCreationFlags: DWORD; lpEnvironment: LPVOID;
  lpCurrentDirectory: LPCSTR; const lpStartupInfo: STARTUPINFOA;
  var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;
{$ENDIF}

function SetProcessShutdownParameters(dwLevel, dwFlags: DWORD): BOOL; stdcall;

function GetProcessShutdownParameters(var lpdwLevel, lpdwFlags: DWORD): BOOL; stdcall;

function GetProcessVersion(ProcessId: DWORD): DWORD; stdcall;

procedure FatalAppExitA(uAction: UINT; lpMessageText: LPCSTR); stdcall;
procedure FatalAppExitW(uAction: UINT; lpMessageText: LPCWSTR); stdcall;

{$IFDEF UNICODE}
procedure FatalAppExit(uAction: UINT; lpMessageText: LPCWSTR); stdcall;
{$ELSE}
procedure FatalAppExit(uAction: UINT; lpMessageText: LPCSTR); stdcall;
{$ENDIF}

procedure GetStartupInfoA(var lpStartupInfo: STARTUPINFOA); stdcall;
procedure GetStartupInfoW(var lpStartupInfo: STARTUPINFOW); stdcall;

{$IFDEF UNICODE}
procedure GetStartupInfo(var lpStartupInfo: STARTUPINFOW); stdcall;
{$ELSE}
procedure GetStartupInfo(var lpStartupInfo: STARTUPINFOA); stdcall;
{$ENDIF}

function GetCommandLineA: LPSTR; stdcall;
function GetCommandLineW: LPWSTR; stdcall;

{$IFDEF UNICODE}
function GetCommandLine: LPWSTR; stdcall;
{$ELSE}
function GetCommandLine: LPSTR; stdcall;
{$ENDIF}

function GetEnvironmentVariableA(lpName: LPCSTR; lpBuffer: LPSTR; nSize: DWORD): DWORD; stdcall;
function GetEnvironmentVariableW(lpName: LPCWSTR; lpBuffer: LPWSTR; nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetEnvironmentVariable(lpName: LPCWSTR; lpBuffer: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$ELSE}
function GetEnvironmentVariable(lpName: LPCSTR; lpBuffer: LPSTR; nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function SetEnvironmentVariableA(lpName, lpValue: LPCSTR): BOOL; stdcall;
function SetEnvironmentVariableW(lpName, lpValue: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetEnvironmentVariable(lpName, lpValue: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetEnvironmentVariable(lpName, lpValue: LPCSTR): BOOL; stdcall;
{$ENDIF}

function ExpandEnvironmentStringsA(lpSrc: LPCSTR; lpDst: LPSTR; nSize: DWORD): DWORD; stdcall;
function ExpandEnvironmentStringsW(lpSrc: LPCWSTR; lpDst: LPWSTR; nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function ExpandEnvironmentStrings(lpSrc: LPCWSTR; lpDst: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$ELSE}
function ExpandEnvironmentStrings(lpSrc: LPCSTR; lpDst: LPSTR; nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function GetFirmwareEnvironmentVariableA(lpName, lpGuid: LPCSTR; pBuffer: PVOID;
  nSize: DWORD): DWORD; stdcall;
function GetFirmwareEnvironmentVariableW(lpName, lpGuid: LPCWSTR; pBuffer: PVOID;
  nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetFirmwareEnvironmentVariable(lpName, lpGuid: LPCWSTR; pBuffer: PVOID;
  nSize: DWORD): DWORD; stdcall;
{$ELSE}
function GetFirmwareEnvironmentVariable(lpName, lpGuid: LPCSTR; pBuffer: PVOID;
  nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function SetFirmwareEnvironmentVariableA(lpName, lpGuid: LPCSTR; pValue: PVOID;
  nSize: DWORD): BOOL; stdcall;
function SetFirmwareEnvironmentVariableW(lpName, lpGuid: LPCWSTR; pValue: PVOID;
  nSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function SetFirmwareEnvironmentVariable(lpName, lpGuid: LPCWSTR; pValue: PVOID;
  nSize: DWORD): BOOL; stdcall;
{$ELSE}
function SetFirmwareEnvironmentVariable(lpName, lpGuid: LPCSTR; pValue: PVOID;
  nSize: DWORD): BOOL; stdcall;
{$ENDIF}

procedure OutputDebugStringA(lpOutputString: LPCSTR); stdcall;
procedure OutputDebugStringW(lpOutputString: LPCWSTR); stdcall;

{$IFDEF UNICODE}
procedure OutputDebugString(lpOutputString: LPCWSTR); stdcall;
{$ELSE}
procedure OutputDebugString(lpOutputString: LPCSTR); stdcall;
{$ENDIF}

function FindResourceA(hModule: HMODULE; lpName, lpType: LPCSTR): HRSRC; stdcall;
function FindResourceW(hModule: HMODULE; lpName, lpType: LPCWSTR): HRSRC; stdcall;

{$IFDEF UNICODE}
function FindResource(hModule: HMODULE; lpName, lpType: LPCWSTR): HRSRC; stdcall;
{$ELSE}
function FindResource(hModule: HMODULE; lpName, lpType: LPCSTR): HRSRC; stdcall;
{$ENDIF}

function FindResourceExA(hModule: HMODULE; lpType, lpName: LPCSTR; wLanguage: WORD): HRSRC; stdcall;
function FindResourceExW(hModule: HMODULE; lpType, lpName: LPCWSTR; wLanguage: WORD): HRSRC; stdcall;

{$IFDEF UNICODE}
function FindResourceEx(hModule: HMODULE; lpType, lpName: LPCWSTR; wLanguage: WORD): HRSRC; stdcall;
{$ELSE}
function FindResourceEx(hModule: HMODULE; lpType, lpName: LPCSTR; wLanguage: WORD): HRSRC; stdcall;
{$ENDIF}

type
  ENUMRESTYPEPROCA = function (hModule: HMODULE; lpType: LPSTR; lParam: LONG_PTR): BOOL; stdcall;
  ENUMRESTYPEPROCW = function (hModule: HMODULE; lpType: LPWSTR; lParam: LONG_PTR): BOOL; stdcall;
  TEnumResTypeProcA = ENUMRESTYPEPROCA;
  TEnumResTypeProcW = ENUMRESTYPEPROCW;

{$IFDEF UNICODE}
  ENUMRESTYPEPROC = function (hModule: HMODULE; lpType: LPWSTR; lParam: LONG_PTR): BOOL; stdcall;
  TEnumResTypeProc = ENUMRESTYPEPROCW;
{$ELSE}
  ENUMRESTYPEPROC = function (hModule: HMODULE; lpType: LPSTR; lParam: LONG_PTR): BOOL; stdcall;
  TEnumResTypeProc = ENUMRESTYPEPROCA;
{$ENDIF}

  ENUMRESNAMEPROCA = function (hModule: HMODULE; lpType: LPCSTR; lpName: LPSTR;
    lParam: LONG_PTR): BOOL; stdcall;
  ENUMRESNAMEPROCW = function (hModule: HMODULE; lpType: LPCWSTR; lpName: LPWSTR;
    lParam: LONG_PTR): BOOL; stdcall;
  TEnumResNameProcA = ENUMRESNAMEPROCA;
  TEnumResNameProcW = ENUMRESNAMEPROCW;

{$IFDEF UNICODE}
  ENUMRESNAMEPROC = function (hModule: HMODULE; lpType: LPCWSTR; lpName: LPWSTR;
    lParam: LONG_PTR): BOOL; stdcall;
  TEnumResNameProc = ENUMRESNAMEPROCW;
{$ELSE}
  ENUMRESNAMEPROC = function (hModule: HMODULE; lpType: LPCSTR; lpName: LPSTR;
    lParam: LONG_PTR): BOOL; stdcall;
  TEnumResNameProc = ENUMRESNAMEPROCA;
{$ENDIF}

  ENUMRESLANGPROCA = function (hModule: HMODULE; lpType, lpName: LPCSTR;
    wLanguage: WORD; lParam: LONG_PTR): BOOL; stdcall;
  ENUMRESLANGPROCW = function (hModule: HMODULE; lpType, lpName: LPCWSTR;
    wLanguage: WORD; lParam: LONG_PTR): BOOL; stdcall;
  TEnumResLangProcA = ENUMRESLANGPROCA;
  TEnumResLangProcW = ENUMRESLANGPROCW;

{$IFDEF UNICODE}
  ENUMRESLANGPROC = function (hModule: HMODULE; lpType, lpName: LPCWSTR;
    wLanguage: WORD; lParam: LONG_PTR): BOOL; stdcall;
  TEnumResLangProc = ENUMRESLANGPROCW;
{$ELSE}
  ENUMRESLANGPROC = function (hModule: HMODULE; lpType, lpName: LPCSTR;
    wLanguage: WORD; lParam: LONG_PTR): BOOL; stdcall;
  TEnumResLangProc = ENUMRESLANGPROCA;
{$ENDIF}

function EnumResourceTypesA(hModule: HMODULE; lpEnumFunc: ENUMRESTYPEPROCA;
  lParam: LONG_PTR): BOOL; stdcall;
function EnumResourceTypesW(hModule: HMODULE; lpEnumFunc: ENUMRESTYPEPROCW;
  lParam: LONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumResourceTypes(hModule: HMODULE; lpEnumFunc: ENUMRESTYPEPROCW;
  lParam: LONG_PTR): BOOL; stdcall;
{$ELSE}
function EnumResourceTypes(hModule: HMODULE; lpEnumFunc: ENUMRESTYPEPROCA;
  lParam: LONG_PTR): BOOL; stdcall;
{$ENDIF}

function EnumResourceNamesA(hModule: HMODULE; lpType: LPCSTR;
  lpEnumFunc: ENUMRESNAMEPROCA; lParam: LONG_PTR): BOOL; stdcall;
function EnumResourceNamesW(hModule: HMODULE; lpType: LPCWSTR;
  lpEnumFunc: ENUMRESNAMEPROCW; lParam: LONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumResourceNames(hModule: HMODULE; lpType: LPCWSTR;
  lpEnumFunc: ENUMRESNAMEPROCW; lParam: LONG_PTR): BOOL; stdcall;
{$ELSE}
function EnumResourceNames(hModule: HMODULE; lpType: LPCSTR;
  lpEnumFunc: ENUMRESNAMEPROCA; lParam: LONG_PTR): BOOL; stdcall;
{$ENDIF}

function EnumResourceLanguagesA(hModule: HMODULE; lpType, lpName: LPCSTR;
  lpEnumFunc: ENUMRESLANGPROCA; lParam: LONG_PTR): BOOL; stdcall;
function EnumResourceLanguagesW(hModule: HMODULE; lpType, lpName: LPCWSTR;
  lpEnumFunc: ENUMRESLANGPROCW; lParam: LONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumResourceLanguages(hModule: HMODULE; lpType, lpName: LPCWSTR;
  lpEnumFunc: ENUMRESLANGPROCW; lParam: LONG_PTR): BOOL; stdcall;
{$ELSE}
function EnumResourceLanguages(hModule: HMODULE; lpType, lpName: LPCSTR;
  lpEnumFunc: ENUMRESLANGPROCA; lParam: LONG_PTR): BOOL; stdcall;
{$ENDIF}

function BeginUpdateResourceA(pFileName: LPCSTR; bDeleteExistingResources: BOOL): HANDLE; stdcall;
function BeginUpdateResourceW(pFileName: LPCWSTR; bDeleteExistingResources: BOOL): HANDLE; stdcall;

{$IFDEF UNICODE}
function BeginUpdateResource(pFileName: LPCWSTR; bDeleteExistingResources: BOOL): HANDLE; stdcall;
{$ELSE}
function BeginUpdateResource(pFileName: LPCSTR; bDeleteExistingResources: BOOL): HANDLE; stdcall;
{$ENDIF}

function UpdateResourceA(hUpdate: HANDLE; lpType, lpName: LPCSTR;
  wLanguage: WORD; lpData: LPVOID; cbData: DWORD): BOOL; stdcall;
function UpdateResourceW(hUpdate: HANDLE; lpType, lpName: LPCWSTR;
  wLanguage: WORD; lpData: LPVOID; cbData: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function UpdateResource(hUpdate: HANDLE; lpType, lpName: LPCWSTR;
  wLanguage: WORD; lpData: LPVOID; cbData: DWORD): BOOL; stdcall;
{$ELSE}
function UpdateResource(hUpdate: HANDLE; lpType, lpName: LPCSTR;
  wLanguage: WORD; lpData: LPVOID; cbData: DWORD): BOOL; stdcall;
{$ENDIF}

function EndUpdateResourceA(hUpdate: HANDLE; fDiscard: BOOL): BOOL; stdcall;
function EndUpdateResourceW(hUpdate: HANDLE; fDiscard: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function EndUpdateResource(hUpdate: HANDLE; fDiscard: BOOL): BOOL; stdcall;
{$ELSE}
function EndUpdateResource(hUpdate: HANDLE; fDiscard: BOOL): BOOL; stdcall;
{$ENDIF}

function GlobalAddAtomA(lpString: LPCSTR): ATOM; stdcall;
function GlobalAddAtomW(lpString: LPCWSTR): ATOM; stdcall;

{$IFDEF UNICODE}
function GlobalAddAtom(lpString: LPCWSTR): ATOM; stdcall;
{$ELSE}
function GlobalAddAtom(lpString: LPCSTR): ATOM; stdcall;
{$ENDIF}

function GlobalFindAtomA(lpString: LPCSTR): ATOM; stdcall;
function GlobalFindAtomW(lpString: LPCWSTR): ATOM; stdcall;

{$IFDEF UNICODE}
function GlobalFindAtom(lpString: LPCWSTR): ATOM; stdcall;
{$ELSE}
function GlobalFindAtom(lpString: LPCSTR): ATOM; stdcall;
{$ENDIF}

function GlobalGetAtomNameA(nAtom: ATOM; lpBuffer: LPSTR; nSize: Integer): UINT; stdcall;
function GlobalGetAtomNameW(nAtom: ATOM; lpBuffer: LPWSTR; nSize: Integer): UINT; stdcall;

{$IFDEF UNICODE}
function GlobalGetAtomName(nAtom: ATOM; lpBuffer: LPWSTR; nSize: Integer): UINT; stdcall;
{$ELSE}
function GlobalGetAtomName(nAtom: ATOM; lpBuffer: LPSTR; nSize: Integer): UINT; stdcall;
{$ENDIF}

function AddAtomA(lpString: LPCSTR): ATOM; stdcall;
function AddAtomW(lpString: LPCWSTR): ATOM; stdcall;

{$IFDEF UNICODE}
function AddAtom(lpString: LPCWSTR): ATOM; stdcall;
{$ELSE}
function AddAtom(lpString: LPCSTR): ATOM; stdcall;
{$ENDIF}

function FindAtomA(lpString: LPCSTR): ATOM; stdcall;
function FindAtomW(lpString: LPCWSTR): ATOM; stdcall;

{$IFDEF UNICODE}
function FindAtom(lpString: LPCWSTR): ATOM; stdcall;
{$ELSE}
function FindAtom(lpString: LPCSTR): ATOM; stdcall;
{$ENDIF}

function GetAtomNameA(nAtom: ATOM; lpBuffer: LPSTR; nSize: Integer): UINT; stdcall;
function GetAtomNameW(nAtom: ATOM; lpBuffer: LPWSTR; nSize: Integer): UINT; stdcall;

{$IFDEF UNICODE}
function GetAtomName(nAtom: ATOM; lpBuffer: LPWSTR; nSize: Integer): UINT; stdcall;
{$ELSE}
function GetAtomName(nAtom: ATOM; lpBuffer: LPSTR; nSize: Integer): UINT; stdcall;
{$ENDIF}

function GetProfileIntA(lpAppName, lpKeyName: LPCSTR; nDefault: Integer): UINT; stdcall;
function GetProfileIntW(lpAppName, lpKeyName: LPCWSTR; nDefault: Integer): UINT; stdcall;

{$IFDEF UNICODE}
function GetProfileInt(lpAppName, lpKeyName: LPCWSTR; nDefault: Integer): UINT; stdcall;
{$ELSE}
function GetProfileInt(lpAppName, lpKeyName: LPCSTR; nDefault: Integer): UINT; stdcall;
{$ENDIF}

function GetProfileStringA(lpAppName, lpKeyName, lpDefault: LPCSTR;
  lpReturnedString: LPSTR; nSize: DWORD): DWORD; stdcall;
function GetProfileStringW(lpAppName, lpKeyName, lpDefault: LPCWSTR;
  lpReturnedString: LPWSTR; nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetProfileString(lpAppName, lpKeyName, lpDefault: LPCWSTR;
  lpReturnedString: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$ELSE}
function GetProfileString(lpAppName, lpKeyName, lpDefault: LPCSTR;
  lpReturnedString: LPSTR; nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function WriteProfileStringA(lpAppName, lpKeyName, lpString: LPCSTR): BOOL; stdcall;
function WriteProfileStringW(lpAppName, lpKeyName, lpString: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function WriteProfileString(lpAppName, lpKeyName, lpString: LPCWSTR): BOOL; stdcall;
{$ELSE}
function WriteProfileString(lpAppName, lpKeyName, lpString: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetProfileSectionA(lpAppName: LPCSTR; lpReturnedString: LPSTR;
  nSize: DWORD): DWORD; stdcall;
function GetProfileSectionW(lpAppName: LPCWSTR; lpReturnedString: LPWSTR;
  nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetProfileSection(lpAppName: LPCWSTR; lpReturnedString: LPWSTR;
  nSize: DWORD): DWORD; stdcall;
{$ELSE}
function GetProfileSection(lpAppName: LPCSTR; lpReturnedString: LPSTR;
  nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function WriteProfileSectionA(lpAppName, lpString: LPCSTR): BOOL; stdcall;
function WriteProfileSectionW(lpAppName, lpString: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function WriteProfileSection(lpAppName, lpString: LPCWSTR): BOOL; stdcall;
{$ELSE}
function WriteProfileSection(lpAppName, lpString: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetPrivateProfileIntA(lpAppName, lpKeyName: LPCSTR; nDefault: Integer;
  lpFileName: LPCSTR): UINT; stdcall;
function GetPrivateProfileIntW(lpAppName, lpKeyName: LPCWSTR; nDefault: Integer;
  lpFileName: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function GetPrivateProfileInt(lpAppName, lpKeyName: LPCWSTR; nDefault: Integer;
  lpFileName: LPCWSTR): UINT; stdcall;
{$ELSE}
function GetPrivateProfileInt(lpAppName, lpKeyName: LPCSTR; nDefault: Integer;
  lpFileName: LPCSTR): UINT; stdcall;
{$ENDIF}

function GetPrivateProfileStringA(lpAppName, lpKeyName, lpDefault: LPCSTR;
  lpReturnedString: LPSTR; nSize: DWORD; lpFileName: LPCSTR): DWORD; stdcall;
function GetPrivateProfileStringW(lpAppName, lpKeyName, lpDefault: LPCWSTR;
  lpReturnedString: LPWSTR; nSize: DWORD; lpFileName: LPCWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetPrivateProfileString(lpAppName, lpKeyName, lpDefault: LPCWSTR;
  lpReturnedString: LPWSTR; nSize: DWORD; lpFileName: LPCWSTR): DWORD; stdcall;
{$ELSE}
function GetPrivateProfileString(lpAppName, lpKeyName, lpDefault: LPCSTR;
  lpReturnedString: LPSTR; nSize: DWORD; lpFileName: LPCSTR): DWORD; stdcall;
{$ENDIF}

function WritePrivateProfileStringA(lpAppName, lpKeyName, lpString,
  lpFileName: LPCSTR): BOOL; stdcall;
function WritePrivateProfileStringW(lpAppName, lpKeyName, lpString,
  lpFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function WritePrivateProfileString(lpAppName, lpKeyName, lpString,
  lpFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function WritePrivateProfileString(lpAppName, lpKeyName, lpString,
  lpFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetPrivateProfileSectionA(lpAppName: LPCSTR; lpReturnedString: LPSTR;
  nSize: DWORD; lpFileName: LPCSTR): DWORD; stdcall;
function GetPrivateProfileSectionW(lpAppName: LPCWSTR; lpReturnedString: LPWSTR;
  nSize: DWORD; lpFileName: LPCWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetPrivateProfileSection(lpAppName: LPCWSTR; lpReturnedString: LPWSTR;
  nSize: DWORD; lpFileName: LPCWSTR): DWORD; stdcall;
{$ELSE}
function GetPrivateProfileSection(lpAppName: LPCSTR; lpReturnedString: LPSTR;
  nSize: DWORD; lpFileName: LPCSTR): DWORD; stdcall;
{$ENDIF}

function WritePrivateProfileSectionA(lpAppName, lpString, lpFileName: LPCSTR): BOOL; stdcall;
function WritePrivateProfileSectionW(lpAppName, lpString, lpFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function WritePrivateProfileSection(lpAppName, lpString, lpFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function WritePrivateProfileSection(lpAppName, lpString, lpFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetPrivateProfileSectionNamesA(lpszReturnBuffer: LPSTR; nSize: DWORD;
  lpFileName: LPCSTR): DWORD; stdcall;
function GetPrivateProfileSectionNamesW(lpszReturnBuffer: LPWSTR; nSize: DWORD;
  lpFileName: LPCWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetPrivateProfileSectionNames(lpszReturnBuffer: LPWSTR; nSize: DWORD;
  lpFileName: LPCWSTR): DWORD; stdcall;
{$ELSE}
function GetPrivateProfileSectionNames(lpszReturnBuffer: LPSTR; nSize: DWORD;
  lpFileName: LPCSTR): DWORD; stdcall;
{$ENDIF}

function GetPrivateProfileStructA(lpszSection, lpszKey: LPCSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCSTR): BOOL; stdcall;
function GetPrivateProfileStructW(lpszSection, lpszKey: LPCWSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function GetPrivateProfileStruct(lpszSection, lpszKey: LPCWSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCWSTR): BOOL; stdcall;
{$ELSE}
function GetPrivateProfileStruct(lpszSection, lpszKey: LPCSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCSTR): BOOL; stdcall;
{$ENDIF}

function WritePrivateProfileStructA(lpszSection, lpszKey: LPCSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCSTR): BOOL; stdcall;
function WritePrivateProfileStructW(lpszSection, lpszKey: LPCWSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function WritePrivateProfileStruct(lpszSection, lpszKey: LPCWSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCWSTR): BOOL; stdcall;
{$ELSE}
function WritePrivateProfileStruct(lpszSection, lpszKey: LPCSTR; lpStruct: LPVOID;
  uSizeStruct: UINT; szFile: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetDriveTypeA(lpRootPathName: LPCSTR): UINT; stdcall;
function GetDriveTypeW(lpRootPathName: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function GetDriveType(lpRootPathName: LPCWSTR): UINT; stdcall;
{$ELSE}
function GetDriveType(lpRootPathName: LPCSTR): UINT; stdcall;
{$ENDIF}

function GetSystemDirectoryA(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
function GetSystemDirectoryW(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function GetSystemDirectory(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;
{$ELSE}
function GetSystemDirectory(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
{$ENDIF}

function GetTempPathA(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
function GetTempPathW(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetTempPath(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetTempPath(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
{$ENDIF}

function GetTempFileNameA(lpPathName, lpPrefixString: LPCSTR; uUnique: UINT;
  lpTempFileName: LPSTR): UINT; stdcall;
function GetTempFileNameW(lpPathName, lpPrefixString: LPCWSTR; uUnique: UINT;
  lpTempFileName: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function GetTempFileName(lpPathName, lpPrefixString: LPCWSTR; uUnique: UINT;
  lpTempFileName: LPWSTR): UINT; stdcall;
{$ELSE}
function GetTempFileName(lpPathName, lpPrefixString: LPCSTR; uUnique: UINT;
  lpTempFileName: LPSTR): UINT; stdcall;
{$ENDIF}

function GetWindowsDirectoryA(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
function GetWindowsDirectoryW(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function GetWindowsDirectory(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;
{$ELSE}
function GetWindowsDirectory(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
{$ENDIF}

function GetSystemWindowsDirectoryA(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
function GetSystemWindowsDirectoryW(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function GetSystemWindowsDirectory(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;
{$ELSE}
function GetSystemWindowsDirectory(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
{$ENDIF}

function GetSystemWow64DirectoryA(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
function GetSystemWow64DirectoryW(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function GetSystemWow64Directory(lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;
{$ELSE}
function GetSystemWow64Directory(lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
{$ENDIF}

function Wow64EnableWow64FsRedirection(Wow64FsEnableRedirection: BOOL): BOOL; stdcall;

//
// for GetProcAddress
//

type
  PGET_SYSTEM_WOW64_DIRECTORY_A = function (lpBuffer: LPSTR; uSize: UINT): UINT; stdcall;
  PGET_SYSTEM_WOW64_DIRECTORY_W = function (lpBuffer: LPWSTR; uSize: UINT): UINT; stdcall;

//
// GetProcAddress only accepts GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A,
// GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A, GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A.
// The others are if you want to use the strings in some other way.
//

const
  GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A = 'GetSystemWow64DirectoryA';
  GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W = WideString('GetSystemWow64DirectoryA');
  GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T = __TEXT('GetSystemWow64DirectoryA');
  GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A = 'GetSystemWow64DirectoryW';
  GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W = WideString('GetSystemWow64DirectoryW');
  GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T = __TEXT('GetSystemWow64DirectoryW');

{$IFDEF UNICODE}
  GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A = GET_SYSTEM_WOW64_DIRECTORY_NAME_W_A;
  GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W = GET_SYSTEM_WOW64_DIRECTORY_NAME_W_W;
  GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T = GET_SYSTEM_WOW64_DIRECTORY_NAME_W_T;
{$ELSE}
  GET_SYSTEM_WOW64_DIRECTORY_NAME_T_A = GET_SYSTEM_WOW64_DIRECTORY_NAME_A_A;
  GET_SYSTEM_WOW64_DIRECTORY_NAME_T_W = GET_SYSTEM_WOW64_DIRECTORY_NAME_A_W;
  GET_SYSTEM_WOW64_DIRECTORY_NAME_T_T = GET_SYSTEM_WOW64_DIRECTORY_NAME_A_T;
{$ENDIF}

function SetCurrentDirectoryA(lpPathName: LPCSTR): BOOL; stdcall;
function SetCurrentDirectoryW(lpPathName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetCurrentDirectory(lpPathName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetCurrentDirectory(lpPathName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetCurrentDirectoryA(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
function GetCurrentDirectoryW(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetCurrentDirectory(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetCurrentDirectory(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
{$ENDIF}

//#if _WIN32_WINNT >= 0x0502

function SetDllDirectoryA(lpPathName: LPCSTR): BOOL; stdcall;
function SetDllDirectoryW(lpPathName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetDllDirectory(lpPathName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetDllDirectory(lpPathName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetDllDirectoryA(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
function GetDllDirectoryW(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetDllDirectory(nBufferLength: DWORD; lpBuffer: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetDllDirectory(nBufferLength: DWORD; lpBuffer: LPSTR): DWORD; stdcall;
{$ENDIF}

//#endif // _WIN32_WINNT >= 0x0502

function GetDiskFreeSpaceA(lpRootPathName: LPCSTR; var lpSectorsPerCluster,
  lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters: DWORD): BOOL; stdcall;
function GetDiskFreeSpaceW(lpRootPathName: LPCWSTR; var lpSectorsPerCluster,
  lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetDiskFreeSpace(lpRootPathName: LPCWSTR; var lpSectorsPerCluster,
  lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters: DWORD): BOOL; stdcall;
{$ELSE}
function GetDiskFreeSpace(lpRootPathName: LPCSTR; lpSectorsPerCluster,
  lpBytesPerSector, lpNumberOfFreeClusters, lpTotalNumberOfClusters: DWORD): BOOL; stdcall;
{$ENDIF}

function GetDiskFreeSpaceExA(lpDirectoryName: LPCSTR; var lpFreeBytesAvailableToCaller,
  lpTotalNumberOfBytes: ULARGE_INTEGER; lpTotalNumberOfFreeBytes: PULARGE_INTEGER): BOOL; stdcall;
function GetDiskFreeSpaceExW(lpDirectoryName: LPCWSTR; var lpFreeBytesAvailableToCaller,
  lpTotalNumberOfBytes: ULARGE_INTEGER; lpTotalNumberOfFreeBytes: PULARGE_INTEGER): BOOL; stdcall;

{$IFDEF UNICODE}
function GetDiskFreeSpaceEx(lpDirectoryName: LPCWSTR; var lpFreeBytesAvailableToCaller,
  lpTotalNumberOfBytes: ULARGE_INTEGER; lpTotalNumberOfFreeBytes: PULARGE_INTEGER): BOOL; stdcall;
{$ELSE}
function GetDiskFreeSpaceEx(lpDirectoryName: LPCSTR; var lpFreeBytesAvailableToCaller,
  lpTotalNumberOfBytes: ULARGE_INTEGER; lpTotalNumberOfFreeBytes: PULARGE_INTEGER): BOOL; stdcall;
{$ENDIF}

function CreateDirectoryA(lpPathName: LPCSTR; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
function CreateDirectoryW(lpPathName: LPCWSTR; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;

{$IFDEF UNICODE}
function CreateDirectory(lpPathName: LPCWSTR; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
{$ELSE}
function CreateDirectory(lpPathName: LPCSTR; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
{$ENDIF}

function CreateDirectoryExA(lpTemplateDirectory: LPCSTR; lpNewDirectory: LPCSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
function CreateDirectoryExW(lpTemplateDirectory: LPCWSTR; lpNewDirectory: LPCWSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;

{$IFDEF UNICODE}
function CreateDirectoryEx(lpTemplateDirectory: LPCWSTR; lpNewDirectory: LPCWSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
{$ELSE}
function CreateDirectoryEx(lpTemplateDirectory: LPCSTR; lpNewDirectory: LPCSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
{$ENDIF}

function RemoveDirectoryA(lpPathName: LPCSTR): BOOL; stdcall;
function RemoveDirectoryW(lpPathName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function RemoveDirectory(lpPathName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function RemoveDirectory(lpPathName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetFullPathNameA(lpFileName: LPCSTR; nBufferLength: DWORD;
  lpBuffer: LPSTR; var lpFilePart: LPSTR): DWORD; stdcall;
function GetFullPathNameW(lpFileName: LPCWSTR; nBufferLength: DWORD;
  lpBuffer: LPWSTR; var lpFilePart: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetFullPathName(lpFileName: LPCWSTR; nBufferLength: DWORD;
  lpBuffer: LPWSTR; var lpFilePart: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetFullPathName(lpFileName: LPCSTR; nBufferLength: DWORD;
  lpBuffer: LPSTR; var lpFilePart: LPSTR): DWORD; stdcall;
{$ENDIF}

const
  DDD_RAW_TARGET_PATH       = $00000001;
  DDD_REMOVE_DEFINITION     = $00000002;
  DDD_EXACT_MATCH_ON_REMOVE = $00000004;
  DDD_NO_BROADCAST_SYSTEM   = $00000008;
  DDD_LUID_BROADCAST_DRIVE  = $00000010;

function DefineDosDeviceA(dwFlags: DWORD; lpDeviceName, lpTargetPath: LPCSTR): BOOL; stdcall;
function DefineDosDeviceW(dwFlags: DWORD; lpDeviceName, lpTargetPath: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function DefineDosDevice(dwFlags: DWORD; lpDeviceName, lpTargetPath: LPCWSTR): BOOL; stdcall;
{$ELSE}
function DefineDosDevice(dwFlags: DWORD; lpDeviceName, lpTargetPath: LPCSTR): BOOL; stdcall;
{$ENDIF}

function QueryDosDeviceA(lpDeviceName, lpTargetPath: LPSTR; ucchMax: DWORD): DWORD; stdcall;
function QueryDosDeviceW(lpDeviceName, lpTargetPath: LPWSTR; ucchMax: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function QueryDosDevice(lpDeviceName, lpTargetPath: LPWSTR; ucchMax: DWORD): DWORD; stdcall;
{$ELSE}
function QueryDosDevice(lpDeviceName, lpTargetPath: LPSTR; ucchMax: DWORD): DWORD; stdcall;
{$ENDIF}

function CreateFileA(lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreationDisposition: DWORD;
  dwFlagsAndAttributes: DWORD; hTemplateFile: HANDLE): HANDLE; stdcall;
function CreateFileW(lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreationDisposition: DWORD;
  dwFlagsAndAttributes: DWORD; hTemplateFile: HANDLE): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateFile(lpFileName: LPCWSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreationDisposition: DWORD;
  dwFlagsAndAttributes: DWORD; hTemplateFile: HANDLE): HANDLE; stdcall;
{$ELSE}
function CreateFile(lpFileName: LPCSTR; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES; dwCreationDisposition: DWORD;
  dwFlagsAndAttributes: DWORD; hTemplateFile: HANDLE): HANDLE; stdcall;
{$ENDIF}

function ReOpenFile(hOriginalFile: HANDLE; dwDesiredAccess, dwShareMode, dwFlagsAndAttributes: DWORD): HANDLE; stdcall;

function SetFileAttributesA(lpFileName: LPCSTR; dwFileAttributes: DWORD): BOOL; stdcall;
function SetFileAttributesW(lpFileName: LPCWSTR; dwFileAttributes: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function SetFileAttributes(lpFileName: LPCWSTR; dwFileAttributes: DWORD): BOOL; stdcall;
{$ELSE}
function SetFileAttributes(lpFileName: LPCSTR; dwFileAttributes: DWORD): BOOL; stdcall;
{$ENDIF}

function GetFileAttributesA(lpFileName: LPCSTR): DWORD; stdcall;
function GetFileAttributesW(lpFileName: LPCWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetFileAttributes(lpFileName: LPCWSTR): DWORD; stdcall;
{$ELSE}
function GetFileAttributes(lpFileName: LPCSTR): DWORD; stdcall;
{$ENDIF}

type
  _GET_FILEEX_INFO_LEVELS = (GetFileExInfoStandard, GetFileExMaxInfoLevel);
  GET_FILEEX_INFO_LEVELS = _GET_FILEEX_INFO_LEVELS;
  TGetFileExInfoLevels = GET_FILEEX_INFO_LEVELS;

function GetFileAttributesExA(lpFileName: LPCSTR;
  fInfoLevelId: GET_FILEEX_INFO_LEVELS; lpFileInformation: LPVOID): BOOL; stdcall;
function GetFileAttributesExW(lpFileName: LPCWSTR;
  fInfoLevelId: GET_FILEEX_INFO_LEVELS; lpFileInformation: LPVOID): BOOL; stdcall;

{$IFDEF UNICODE}
function GetFileAttributesEx(lpFileName: LPCWSTR;
  fInfoLevelId: GET_FILEEX_INFO_LEVELS; lpFileInformation: LPVOID): BOOL; stdcall;
{$ELSE}
function GetFileAttributesEx(lpFileName: LPCSTR;
  fInfoLevelId: GET_FILEEX_INFO_LEVELS; lpFileInformation: LPVOID): BOOL; stdcall;
{$ENDIF}

function GetCompressedFileSizeA(lpFileName: LPCSTR; lpFileSizeHigh: LPDWORD): DWORD; stdcall;
function GetCompressedFileSizeW(lpFileName: LPCWSTR; lpFileSizeHigh: LPDWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetCompressedFileSize(lpFileName: LPCWSTR; lpFileSizeHigh: LPDWORD): DWORD; stdcall;
{$ELSE}
function GetCompressedFileSize(lpFileName: LPCSTR; lpFileSizeHigh: LPDWORD): DWORD; stdcall;
{$ENDIF}

function DeleteFileA(lpFileName: LPCSTR): BOOL; stdcall;
function DeleteFileW(lpFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function DeleteFile(lpFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function DeleteFile(lpFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

(* todo
WINBASEAPI
BOOL
WINAPI
CheckNameLegalDOS8Dot3A(
    IN LPCSTR lpName,
    OUT LPSTR lpOemName OPTIONAL,
    IN DWORD OemNameSize OPTIONAL,
    OUT PBOOL pbNameContainsSpaces OPTIONAL,
    OUT PBOOL pbNameLegal
    );
WINBASEAPI
BOOL
WINAPI
CheckNameLegalDOS8Dot3W(
    IN LPCWSTR lpName,
    OUT LPSTR lpOemName OPTIONAL,
    IN DWORD OemNameSize OPTIONAL,
    OUT PBOOL pbNameContainsSpaces OPTIONAL,
    OUT PBOOL pbNameLegal
    );
#ifdef UNICODE
#define CheckNameLegalDOS8Dot3  CheckNameLegalDOS8Dot3W
#else
#define CheckNameLegalDOS8Dot3  CheckNameLegalDOS8Dot3A
#endif // !UNICODE
*)

type
  _FINDEX_INFO_LEVELS = (FindExInfoStandard, FindExInfoMaxInfoLevel);
  FINDEX_INFO_LEVELS = _FINDEX_INFO_LEVELS;
  TFindExInfoLevels = FINDEX_INFO_LEVELS;

  _FINDEX_SEARCH_OPS = (
    FindExSearchNameMatch,
    FindExSearchLimitToDirectories,
    FindExSearchLimitToDevices,
    FindExSearchMaxSearchOp);
  FINDEX_SEARCH_OPS = _FINDEX_SEARCH_OPS;
  TFindExSearchOps = FINDEX_SEARCH_OPS;

const
  FIND_FIRST_EX_CASE_SENSITIVE = $00000001;

function FindFirstFileExA(lpFileName: LPCSTR; fInfoLevelId: FINDEX_INFO_LEVELS;
  lpFindFileData: LPVOID; fSearchOp: FINDEX_SEARCH_OPS; lpSearchFilter: LPVOID;
  dwAdditionalFlags: DWORD): HANDLE; stdcall;
function FindFirstFileExW(lpFileName: LPCWSTR; fInfoLevelId: FINDEX_INFO_LEVELS;
  lpFindFileData: LPVOID; fSearchOp: FINDEX_SEARCH_OPS; lpSearchFilter: LPVOID;
  dwAdditionalFlags: DWORD): HANDLE; stdcall;

{$IFDEF UNICODE}
function FindFirstFileEx(lpFileName: LPCWSTR; fInfoLevelId: FINDEX_INFO_LEVELS;
  lpFindFileData: LPVOID; fSearchOp: FINDEX_SEARCH_OPS; lpSearchFilter: LPVOID;
  dwAdditionalFlags: DWORD): HANDLE; stdcall;
{$ELSE}
function FindFirstFileEx(lpFileName: LPCSTR; fInfoLevelId: FINDEX_INFO_LEVELS;
  lpFindFileData: LPVOID; fSearchOp: FINDEX_SEARCH_OPS; lpSearchFilter: LPVOID;
  dwAdditionalFlags: DWORD): HANDLE; stdcall;
{$ENDIF}

function FindFirstFileA(lpFileName: LPCSTR; var lpFindFileData: WIN32_FIND_DATAA): HANDLE; stdcall;
function FindFirstFileW(lpFileName: LPCWSTR; var lpFindFileData: WIN32_FIND_DATAW): HANDLE; stdcall;

{$IFDEF UNICODE}
function FindFirstFile(lpFileName: LPCWSTR; var lpFindFileData: WIN32_FIND_DATAW): HANDLE; stdcall;
{$ELSE}
function FindFirstFile(lpFileName: LPCSTR; var lpFindFileData: WIN32_FIND_DATAA): HANDLE; stdcall;
{$ENDIF}

function FindNextFileA(hFindFile: HANDLE; var FindFileData: WIN32_FIND_DATAA): BOOL; stdcall;
function FindNextFileW(hFindFile: HANDLE; var lpFindFileData: WIN32_FIND_DATAW): BOOL; stdcall;

{$IFDEF UNICODE}
function FindNextFile(hFindFile: HANDLE; var lpFindFileData: WIN32_FIND_DATAW): BOOL; stdcall;
{$ELSE}
function FindNextFile(hFindFile: HANDLE; var lpFindFileData: WIN32_FIND_DATAA): BOOL; stdcall;
{$ENDIF}

function SearchPathA(lpPath, lpFileName, lpExtension: LPCSTR; nBufferLength: DWORD;
  lpBuffer: LPSTR; var lpFilePart: LPSTR): DWORD; stdcall;
function SearchPathW(lpPath, lpFileName, lpExtension: LPCWSTR; nBufferLength: DWORD;
  lpBuffer: LPWSTR; var lpFilePart: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function SearchPath(lpPath, lpFileName, lpExtension: LPCWSTR; nBufferLength: DWORD;
  lpBuffer: LPWSTR; var lpFilePart: LPWSTR): DWORD; stdcall;
{$ELSE}
function SearchPath(lpPath, lpFileName, lpExtension: LPCSTR; nBufferLength: DWORD;
  lpBuffer: LPSTR; var lpFilePart: LPSTR): DWORD; stdcall;
{$ENDIF}

function CopyFileA(lpExistingFileName, lpNewFileName: LPCSTR; bFailIfExists: BOOL): BOOL; stdcall;
function CopyFileW(lpExistingFileName, lpNewFileName: LPCWSTR; bFailIfExists: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function CopyFile(lpExistingFileName, lpNewFileName: LPCWSTR; bFailIfExists: BOOL): BOOL; stdcall;
{$ELSE}
function CopyFile(lpExistingFileName, lpNewFileName: LPCSTR; bFailIfExists: BOOL): BOOL; stdcall;
{$ENDIF}

type
  LPPROGRESS_ROUTINE = function (
    TotalFileSize: LARGE_INTEGER;
    TotalBytesTransferred: LARGE_INTEGER;
    StreamSize: LARGE_INTEGER;
    StreamBytesTransferred: LARGE_INTEGER;
    dwStreamNumber: DWORD;
    dwCallbackReason: DWORD;
    hSourceFile: HANDLE;
    hDestinationFile: HANDLE;
    lpData: LPVOID): DWORD; stdcall;
  TProgressRoutine = LPPROGRESS_ROUTINE;

function CopyFileExA(lpExistingFileName, lpNewFileName: LPCSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; var pbCancel: BOOL;
  dwCopyFlags: DWORD): BOOL; stdcall;
function CopyFileExW(lpExistingFileName, lpNewFileName: LPCWSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; var pbCancel: BOOL;
  dwCopyFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CopyFileEx(lpExistingFileName, lpNewFileName: LPCWSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; var pbCancel: BOOL;
  dwCopyFlags: DWORD): BOOL; stdcall;
{$ELSE}
function CopyFileEx(lpExistingFileName, lpNewFileName: LPCSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; var pbCancel: BOOL;
  dwCopyFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function MoveFileA(lpExistingFileName, lpNewFileName: LPCSTR): BOOL; stdcall;
function MoveFileW(lpExistingFileName, lpNewFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function MoveFile(lpExistingFileName, lpNewFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function MoveFile(lpExistingFileName, lpNewFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function MoveFileExA(lpExistingFileName, lpNewFileName: LPCSTR; dwFlags: DWORD): BOOL; stdcall;
function MoveFileExW(lpExistingFileName, lpNewFileName: LPCWSTR; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function MoveFileEx(lpExistingFileName, lpNewFileName: LPCWSTR; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function MoveFileEx(lpExistingFileName, lpNewFileName: LPCSTR; dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function MoveFileWithProgressA(lpExistingFileName, lpNewFileName: LPCSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; dwFlags: DWORD): BOOL; stdcall;
function MoveFileWithProgressW(lpExistingFileName, lpNewFileName: LPCWSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function MoveFileWithProgress(lpExistingFileName, lpNewFileName: LPCWSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function MoveFileWithProgress(lpExistingFileName, lpNewFileName: LPCSTR;
  lpProgressRoutine: LPPROGRESS_ROUTINE; lpData: LPVOID; dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

const
  MOVEFILE_REPLACE_EXISTING      = $00000001;
  MOVEFILE_COPY_ALLOWED          = $00000002;
  MOVEFILE_DELAY_UNTIL_REBOOT    = $00000004;
  MOVEFILE_WRITE_THROUGH         = $00000008;
  MOVEFILE_CREATE_HARDLINK       = $00000010;
  MOVEFILE_FAIL_IF_NOT_TRACKABLE = $00000020;

function ReplaceFileA(lpReplacedFileName, lpReplacementFileName,
  lpBackupFileName: LPCSTR; dwReplaceFlags: DWORD; lpExclude: LPVOID;
  lpReserved: LPVOID): BOOL; stdcall;
function ReplaceFileW(lpReplacedFileName, lpReplacementFileName,
  lpBackupFileName: LPCWSTR; dwReplaceFlags: DWORD; lpExclude: LPVOID;
  lpReserved: LPVOID): BOOL; stdcall;

{$IFDEF UNICODE}
function ReplaceFile(lpReplacedFileName, lpReplacementFileName,
  lpBackupFileName: LPCWSTR; dwReplaceFlags: DWORD; lpExclude: LPVOID;
  lpReserved: LPVOID): BOOL; stdcall;
{$ELSE}
function ReplaceFile(lpReplacedFileName, lpReplacementFileName,
  lpBackupFileName: LPCSTR; dwReplaceFlags: DWORD; lpExclude: LPVOID;
  lpReserved: LPVOID): BOOL; stdcall;
{$ENDIF}

//
// API call to create hard links.
//

function CreateHardLinkA(lpFileName, lpExistingFileName: LPCSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
function CreateHardLinkW(lpFileName, lpExistingFileName: LPCWSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;

{$IFDEF UNICODE}
function CreateHardLink(lpFileName, lpExistingFileName: LPCWSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
{$ELSE}
function CreateHardLink(lpFileName, lpExistingFileName: LPCSTR;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): BOOL; stdcall;
{$ENDIF}

//#if (_WIN32_WINNT >= 0x0501)

//
// API call to enumerate for streams within a file
//

type
  _STREAM_INFO_LEVELS = (FindStreamInfoStandard, FindStreamInfoMaxInfoLevel);
  STREAM_INFO_LEVELS = _STREAM_INFO_LEVELS;
  TStreamInfoLevels = STREAM_INFO_LEVELS;

  _WIN32_FIND_STREAM_DATA = record
    StreamSize: LARGE_INTEGER;
    cStreamName: array [0..MAX_PATH + 35] of WCHAR;
  end;
  WIN32_FIND_STREAM_DATA = _WIN32_FIND_STREAM_DATA;
  PWIN32_FIND_STREAM_DATA = ^WIN32_FIND_STREAM_DATA;
  TWin32FindStreamData = WIN32_FIND_STREAM_DATA;
  PWin32FindStreamData = PWIN32_FIND_STREAM_DATA;

function FindFirstStreamW(lpFileName: LPCWSTR; InfoLevel: STREAM_INFO_LEVELS; lpFindStreamData: LPVOID; dwFlags: DWORD): HANDLE; stdcall;

function FindNextStreamW(hFindStream: HANDLE; lpFindStreamData: LPVOID): BOOL; stdcall;

//#endif // (_WIN32_WINNT >= 0x0500)

function CreateNamedPipeA(lpName: LPCSTR; dwOpenMode, dwPipeMode, nMaxInstances,
  nOutBufferSize, nInBufferSize, nDefaultTimeOut: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;
function CreateNamedPipeW(lpName: LPCWSTR; dwOpenMode, dwPipeMode, nMaxInstances,
  nOutBufferSize, nInBufferSize, nDefaultTimeOut: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateNamedPipe(lpName: LPCWSTR; dwOpenMode, dwPipeMode, nMaxInstances,
  nOutBufferSize, nInBufferSize, nDefaultTimeOut: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;
{$ELSE}
function CreateNamedPipe(lpName: LPCSTR; dwOpenMode, dwPipeMode, nMaxInstances,
  nOutBufferSize, nInBufferSize, nDefaultTimeOut: DWORD;
  lpSecurityAttributes: LPSECURITY_ATTRIBUTES): HANDLE; stdcall;
{$ENDIF}

function GetNamedPipeHandleStateA(hNamedPipe: HANDLE; lpState, lpCurInstances,
  lpMaxCollectionCount, lpCollectDataTimeout: LPDWORD; lpUserName: LPSTR;
  nMaxUserNameSize: DWORD): BOOL; stdcall;
function GetNamedPipeHandleStateW(hNamedPipe: HANDLE; lpState, lpCurInstances,
  lpMaxCollectionCount, lpCollectDataTimeout: LPDWORD; lpUserName: LPWSTR;
  nMaxUserNameSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetNamedPipeHandleState(hNamedPipe: HANDLE; lpState, lpCurInstances,
  lpMaxCollectionCount, lpCollectDataTimeout: LPDWORD; lpUserName: LPWSTR;
  nMaxUserNameSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetNamedPipeHandleState(hNamedPipe: HANDLE; lpState, lpCurInstances,
  lpMaxCollectionCount, lpCollectDataTimeout: LPDWORD; lpUserName: LPSTR;
  nMaxUserNameSize: DWORD): BOOL; stdcall;
{$ENDIF}

function CallNamedPipeA(lpNamedPipeName: LPCSTR; lpInBuffer: LPVOID;
  nInBufferSize: DWORD; lpOutBuffer: LPVOID; nOutBufferSize: DWORD;
  var lpBytesRead: DWORD; nTimeOut: DWORD): BOOL; stdcall;
function CallNamedPipeW(lpNamedPipeName: LPCWSTR; lpInBuffer: LPVOID;
  nInBufferSize: DWORD; lpOutBuffer: LPVOID; nOutBufferSize: DWORD;
  var lpBytesRead: DWORD; nTimeOut: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CallNamedPipe(lpNamedPipeName: LPCWSTR; lpInBuffer: LPVOID;
  nInBufferSize: DWORD; lpOutBuffer: LPVOID; nOutBufferSize: DWORD;
  var lpBytesRead: DWORD; nTimeOut: DWORD): BOOL; stdcall;
{$ELSE}
function CallNamedPipe(lpNamedPipeName: LPCSTR; lpInBuffer: LPVOID;
  nInBufferSize: DWORD; lpOutBuffer: LPVOID; nOutBufferSize: DWORD;
  var lpBytesRead: DWORD; nTimeOut: DWORD): BOOL; stdcall;
{$ENDIF}

function WaitNamedPipeA(lpNamedPipeName: LPCSTR; nTimeOut: DWORD): BOOL; stdcall;
function WaitNamedPipeW(lpNamedPipeName: LPCWSTR; nTimeOut: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function WaitNamedPipe(lpNamedPipeName: LPCWSTR; nTimeOut: DWORD): BOOL; stdcall;
{$ELSE}
function WaitNamedPipe(lpNamedPipeName: LPCSTR; nTimeOut: DWORD): BOOL; stdcall;
{$ENDIF}

function SetVolumeLabelA(lpRootPathName, lpVolumeName: LPCSTR): BOOL; stdcall;
function SetVolumeLabelW(lpRootPathName, lpVolumeName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetVolumeLabel(lpRootPathName, lpVolumeName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetVolumeLabel(lpRootPathName, lpVolumeName: LPCSTR): BOOL; stdcall;
{$ENDIF}

procedure SetFileApisToOEM; stdcall;

procedure SetFileApisToANSI; stdcall;

function AreFileApisANSI: BOOL; stdcall;

function GetVolumeInformationA(lpRootPathName: LPCSTR; lpVolumeNameBuffer: LPSTR;
  nVolumeNameSize: DWORD; lpVolumeSerialNumber: LPDWORD;
  var lpMaximumComponentLength, lpFileSystemFlags: DWORD;
  lpFileSystemNameBuffer: LPSTR; nFileSystemNameSize: DWORD): BOOL; stdcall;
function GetVolumeInformationW(lpRootPathName: LPCWSTR; lpVolumeNameBuffer: LPWSTR;
  nVolumeNameSize: DWORD; lpVolumeSerialNumber: LPDWORD;
  var lpMaximumComponentLength, lpFileSystemFlags: DWORD;
  lpFileSystemNameBuffer: LPWSTR; nFileSystemNameSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetVolumeInformation(lpRootPathName: LPCWSTR; lpVolumeNameBuffer: LPWSTR;
  nVolumeNameSize: DWORD; lpVolumeSerialNumber: LPDWORD;
  var lpMaximumComponentLength, lpFileSystemFlags: DWORD;
  lpFileSystemNameBuffer: LPWSTR; nFileSystemNameSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetVolumeInformation(lpRootPathName: LPCSTR; lpVolumeNameBuffer: LPSTR;
  nVolumeNameSize: DWORD; lpVolumeSerialNumber: LPDWORD;
  var lpMaximumComponentLength, lpFileSystemFlags: DWORD;
  lpFileSystemNameBuffer: LPSTR; nFileSystemNameSize: DWORD): BOOL; stdcall;
{$ENDIF}

function CancelIo(hFile: HANDLE): BOOL; stdcall;

//
// Event logging APIs
//

function ClearEventLogA(hEventLog: HANDLE; lpBackupFileName: LPCSTR): BOOL; stdcall;
function ClearEventLogW(hEventLog: HANDLE; lpBackupFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function ClearEventLog(hEventLog: HANDLE; lpBackupFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function ClearEventLog(hEventLog: HANDLE; lpBackupFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function BackupEventLogA(hEventLog: HANDLE; lpBackupFileName: LPCSTR): BOOL; stdcall;
function BackupEventLogW(hEventLog: HANDLE; lpBackupFileName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function BackupEventLog(hEventLog: HANDLE; lpBackupFileName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function BackupEventLog(hEventLog: HANDLE; lpBackupFileName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function CloseEventLog(hEventLog: HANDLE): BOOL; stdcall;

function DeregisterEventSource(hEventLog: HANDLE): BOOL; stdcall;

function NotifyChangeEventLog(hEventLog, hEvent: HANDLE): BOOL; stdcall;

function GetNumberOfEventLogRecords(hEventLog: HANDLE; var NumberOfRecords: DWORD): BOOL; stdcall;

function GetOldestEventLogRecord(hEventLog: HANDLE; var OldestRecord: DWORD): BOOL; stdcall;

function OpenEventLogA(lpUNCServerName, lpSourceName: LPCSTR): HANDLE; stdcall;
function OpenEventLogW(lpUNCServerName, lpSourceName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenEventLog(lpUNCServerName, lpSourceName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenEventLog(lpUNCServerName, lpSourceName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function RegisterEventSourceA(lpUNCServerName, lpSourceName: LPCSTR): HANDLE; stdcall;
function RegisterEventSourceW(lpUNCServerName, lpSourceName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function RegisterEventSource(lpUNCServerName, lpSourceName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function RegisterEventSource(lpUNCServerName, lpSourceName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function OpenBackupEventLogA(lpUNCServerName, lpFileName: LPCSTR): HANDLE; stdcall;
function OpenBackupEventLogW(lpUNCServerName, lpFileName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenBackupEventLog(lpUNCServerName, lpFileName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenBackupEventLog(lpUNCServerName, lpFileName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function ReadEventLogA(hEventLog: HANDLE; dwReadFlags, dwRecordOffset: DWORD;
  lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  var pnBytesRead, pnMinNumberOfBytesNeeded: DWORD): BOOL; stdcall;
function ReadEventLogW(hEventLog: HANDLE; dwReadFlags, dwRecordOffset: DWORD;
  lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  var pnBytesRead, pnMinNumberOfBytesNeeded: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function ReadEventLog(hEventLog: HANDLE; dwReadFlags, dwRecordOffset: DWORD;
  lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  var pnBytesRead, pnMinNumberOfBytesNeeded: DWORD): BOOL; stdcall;
{$ELSE}
function ReadEventLog(hEventLog: HANDLE; dwReadFlags, dwRecordOffset: DWORD;
  lpBuffer: LPVOID; nNumberOfBytesToRead: DWORD;
  var pnBytesRead, pnMinNumberOfBytesNeeded: DWORD): BOOL; stdcall;
{$ENDIF}

function ReportEventA(hEventLog: HANDLE; wType, wCategory: WORD; dwEventID: DWORD;
  lpUserSid: PSID; wNumStrings: WORD; dwDataSize: DWORD; lpStrings: LPCSTR;
  lpRawData: LPVOID): BOOL; stdcall;
function ReportEventW(hEventLog: HANDLE; wType, wCategory: WORD; dwEventID: DWORD;
  lpUserSid: PSID; wNumStrings: WORD; dwDataSize: DWORD; lpStrings: LPCWSTR;
  lpRawData: LPVOID): BOOL; stdcall;

{$IFDEF UNICODE}
function ReportEvent(hEventLog: HANDLE; wType, wCategory: WORD; dwEventID: DWORD;
  lpUserSid: PSID; wNumStrings: WORD; dwDataSize: DWORD; lpStrings: LPCWSTR;
  lpRawData: LPVOID): BOOL; stdcall;
{$ELSE}
function ReportEvent(hEventLog: HANDLE; wType, wCategory: WORD; dwEventID: DWORD;
  lpUserSid: PSID; wNumStrings: WORD; dwDataSize: DWORD; lpStrings: LPCSTR;
  lpRawData: LPVOID): BOOL; stdcall;
{$ENDIF}

const
  EVENTLOG_FULL_INFO = 0;

type
  LPEVENTLOG_FULL_INFORMATION = ^EVENTLOG_FULL_INFORMATION;
  _EVENTLOG_FULL_INFORMATION = record
    dwFull: DWORD;
  end;
  EVENTLOG_FULL_INFORMATION = _EVENTLOG_FULL_INFORMATION;
  TEventlogFullInformation = EVENTLOG_FULL_INFORMATION;
  PEventlogFullInformation = LPEVENTLOG_FULL_INFORMATION;

function GetEventLogInformation(hEventLog: HANDLE; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; cbBufSize: DWORD; var pcbBytesNeeded: DWORD): BOOL; stdcall;

//
// Security APIs
//

function DuplicateToken(ExistingTokenHandle: HANDLE;
  ImpersonationLevel: SECURITY_IMPERSONATION_LEVEL; DuplicateTokenHandle: PHANDLE): BOOL; stdcall;

function GetKernelObjectSecurity(Handle: HANDLE;
  RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;

function ImpersonateNamedPipeClient(hNamedPipe: HANDLE): BOOL; stdcall;

function ImpersonateSelf(ImpersonationLevel: SECURITY_IMPERSONATION_LEVEL): BOOL; stdcall;

function RevertToSelf : BOOL; stdcall;

function SetThreadToken(Thread: PHANDLE; Token: HANDLE): BOOL; stdcall;

function AccessCheck(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  ClientToken: HANDLE; DesiredAccess: DWORD; const GenericMapping: GENERIC_MAPPING;
  var PrivilegeSet: PRIVILEGE_SET; var PrivilegeSetLength,
  GrantedAccess: DWORD; var AccessStatus: BOOL): BOOL; stdcall;

function AccessCheckByType(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  PrincipalSelfSid: PSID; ClientToken: HANDLE; DesiredAccess: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; var PrivilegeSet: PRIVILEGE_SET;
  var PrivilegeSetLength, GrantedAccess: DWORD; var AccessStatus: BOOL): BOOL; stdcall;

function AccessCheckByTypeResultList(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  PrincipalSelfSid: PSID; ClientToken: HANDLE; DesiredAccess: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; var PrivilegeSet: PRIVILEGE_SET;
  var PrivilegeSetLength, GrantedAccessList: DWORD;
  var AccessStatusList: DWORD): BOOL; stdcall;

function OpenProcessToken(ProcessHandle: HANDLE; DesiredAccess: DWORD;
  var TokenHandle: HANDLE): BOOL; stdcall;

function OpenThreadToken(ThreadHandle: HANDLE; DesiredAccess: DWORD;
  OpenAsSelf: BOOL; var TokenHandle: HANDLE): BOOL; stdcall;

function GetTokenInformation(TokenHandle: HANDLE;
  TokenInformationClass: TOKEN_INFORMATION_CLASS; TokenInformation: LPVOID;
  TokenInformationLength: DWORD; var ReturnLength: DWORD): BOOL; stdcall;

function SetTokenInformation(TokenHandle: HANDLE;
  TokenInformationClass: TOKEN_INFORMATION_CLASS; TokenInformation: LPVOID;
  TokenInformationLength: DWORD): BOOL; stdcall;

function AdjustTokenPrivileges(TokenHandle: HANDLE; DisableAllPrivileges: BOOL;
  NewState: PTOKEN_PRIVILEGES; BufferLength: DWORD;
  PreviousState: PTOKEN_PRIVILEGES; ReturnLength: LPDWORD): BOOL; stdcall;

function AdjustTokenGroups(TokenHandle: HANDLE; ResetToDefault: BOOL;
  NewState: PTOKEN_GROUPS; BufferLength: DWORD; PreviousState: PTOKEN_GROUPS;
  ReturnLength: PDWORD): BOOL; stdcall;

function PrivilegeCheck(ClientToken: HANDLE;
  RequiredPrivileges: PPRIVILEGE_SET; var pfResult: BOOL): BOOL; stdcall;

function AccessCheckAndAuditAlarmA(SubsystemName: LPCSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  DesiredAccess: DWORD; const GenericMapping: GENERIC_MAPPING;
  ObjectCreation: BOOL; var GrantedAccess: DWORD;
  var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall;
function AccessCheckAndAuditAlarmW(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPWSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  DesiredAccess: DWORD; const GenericMapping: GENERIC_MAPPING;
  ObjectCreation: BOOL; var GrantedAccess: DWORD;
  var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function AccessCheckAndAuditAlarm(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPWSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  DesiredAccess: DWORD; const GenericMapping: GENERIC_MAPPING;
  ObjectCreation: BOOL; var GrantedAccess: DWORD;
  var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function AccessCheckAndAuditAlarm(SubsystemName: LPCSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  DesiredAccess: DWORD; const GenericMapping: GENERIC_MAPPING;
  ObjectCreation: BOOL; var GrantedAccess: DWORD;
  var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function AccessCheckByTypeAndAuditAlarmA(SubsystemName: LPCSTR; HandleId: LPVOID;
  ObjectTypeName: LPCSTR; ObjectName: LPCSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  PrincipalSelfSid: PSID; DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE;
  Flags: DWORD; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOL; GrantedAccess: LPDWORD;
  AccessStatus: LPBOOL; pfGenerateOnClose: LPBOOL): BOOL; stdcall;
function AccessCheckByTypeAndAuditAlarmW(SubsystemName: LPCWSTR; HandleId: LPVOID;
ObjectTypeName: LPCWSTR; ObjectName: LPCWSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
PrincipalSelfSid: PSID; DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE;
Flags: DWORD; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOL; GrantedAccess: LPDWORD;
AccessStatus: LPBOOL; pfGenerateOnClose: LPBOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function AccessCheckByTypeAndAuditAlarm(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPCWSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  PrincipalSelfSid: PSID; DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE;
  Flags: DWORD; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess: DWORD; var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function AccessCheckByTypeAndAuditAlarm(SubsystemName: LPCSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPCSTR; SecurityDescriptor: PSECURITY_DESCRIPTOR;
  PrincipalSelfSid: PSID; DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE;
  Flags: DWORD; ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess: DWORD; var AccessStatus, pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function AccessCheckByTypeResultListAndAuditAlarmA(SubsystemName: LPCSTR;
  HandleId: LPVOID; ObjectTypeName, ObjectName: LPCSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;
function AccessCheckByTypeResultListAndAuditAlarmW(SubsystemName: LPCWSTR;
  HandleId: LPVOID; ObjectTypeName, ObjectName: LPCWSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function AccessCheckByTypeResultListAndAuditAlarm(SubsystemName: LPCWSTR;
  HandleId: LPVOID; ObjectTypeName, ObjectName: LPCWSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarm(SubsystemName: LPCSTR;
  HandleId: LPVOID; ObjectTypeName, ObjectName: LPCSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function AccessCheckByTypeResultListAndAuditAlarmByHandleA(SubsystemName: LPCSTR;
  HandleId: LPVOID; ClientToken: HANDLE; ObjectTypeName, ObjectName: LPCSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;
function AccessCheckByTypeResultListAndAuditAlarmByHandleW(SubsystemName: LPCWSTR;
  HandleId: LPVOID; ClientToken: HANDLE; ObjectTypeName, ObjectName: LPCWSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function AccessCheckByTypeResultListAndAuditAlarmByHandle(SubsystemName: LPCWSTR;
  HandleId: LPVOID; ClientToken: HANDLE; ObjectTypeName, ObjectName: LPCWSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: PGENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmByHandle(SubsystemName: LPCSTR;
  HandleId: LPVOID; ClientToken: HANDLE; ObjectTypeName, ObjectName: LPCSTR;
  SecurityDescriptor: PSECURITY_DESCRIPTOR; PrincipalSelfSid: PSID;
  DesiredAccess: DWORD; AuditType: AUDIT_EVENT_TYPE; Flags: DWORD;
  ObjectTypeList: POBJECT_TYPE_LIST; ObjectTypeListLength: DWORD;
  const GenericMapping: GENERIC_MAPPING; ObjectCreation: BOOL;
  var GrantedAccess, AccessStatusList: DWORD; var pfGenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function ObjectOpenAuditAlarmA(SubsystemName: LPCSTR; HandleId: LPVOID;
  ObjectTypeName: LPSTR; ObjectName: LPSTR; pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  ClientToken: HANDLE; DesiredAccess, GrantedAccess: DWORD;
  Privileges: PPRIVILEGE_SET; ObjectCreation: BOOL; AccessGranted: BOOL;
  var GenerateOnClose: BOOL): BOOL; stdcall;
function ObjectOpenAuditAlarmW(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ObjectTypeName, ObjectName: LPWSTR; pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  ClientToken: HANDLE; DesiredAccess: DWORD; GrantedAccess: DWORD;
  Privileges: PPRIVILEGE_SET; ObjectCreation: BOOL; AccessGranted: BOOL;
  var GenerateOnClose: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function ObjectOpenAuditAlarm(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ObjectTypeName: LPWSTR; ObjectName: LPWSTR; pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  ClientToken: HANDLE; DesiredAccess: DWORD; GrantedAccess: DWORD;
  Privileges: PPRIVILEGE_SET; ObjectCreation: BOOL; AccessGranted: BOOL;
  var GenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function ObjectOpenAuditAlarm(SubsystemName: LPCSTR; HandleId: LPVOID;
  ObjectTypeName: LPSTR; ObjectName: LPSTR; pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  ClientToken: HANDLE; DesiredAccess: DWORD; GrantedAccess: DWORD;
  Privileges: PPRIVILEGE_SET; ObjectCreation: BOOL; AccessGranted: BOOL;
  var GenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function ObjectPrivilegeAuditAlarmA(SubsystemName: LPCSTR; HandleId: LPVOID;
  ClientToken: HANDLE; DesiredAccess: DWORD; const Privileges: PRIVILEGE_SET;
  AccessGranted: BOOL): BOOL; stdcall;
function ObjectPrivilegeAuditAlarmW(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ClientToken: HANDLE; DesiredAccess: DWORD; const Privileges: PRIVILEGE_SET;
  AccessGranted: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function ObjectPrivilegeAuditAlarm(SubsystemName: LPCWSTR; HandleId: LPVOID;
  ClientToken: HANDLE; DesiredAccess: DWORD; const Privileges: PRIVILEGE_SET;
  AccessGranted: BOOL): BOOL; stdcall;
{$ELSE}
function ObjectPrivilegeAuditAlarm(SubsystemName: LPCSTR; HandleId: LPVOID;
  ClientToken: HANDLE; DesiredAccess: DWORD; const Privileges: PRIVILEGE_SET;
  AccessGranted: BOOL): BOOL; stdcall;
{$ENDIF}

function ObjectCloseAuditAlarmA(SubsystemName: LPCSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;
function ObjectCloseAuditAlarmW(SubsystemName: LPCWSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function ObjectCloseAuditAlarm(SubsystemName: LPCWSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function ObjectCloseAuditAlarm(SubsystemName: LPCSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function ObjectDeleteAuditAlarmA(SubsystemName: LPCSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;
function ObjectDeleteAuditAlarmW(SubsystemName: LPCWSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function ObjectDeleteAuditAlarm(SubsystemName: LPCWSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;
{$ELSE}
function ObjectDeleteAuditAlarm(SubsystemName: LPCSTR; HandleId: LPVOID;
  GenerateOnClose: BOOL): BOOL; stdcall;
{$ENDIF}

function PrivilegedServiceAuditAlarmA(SubsystemName, ServiceName: LPCSTR;
  ClientToken: HANDLE; const Privileges: PRIVILEGE_SET; AccessGranted: BOOL): BOOL; stdcall;
function PrivilegedServiceAuditAlarmW(SubsystemName, ServiceName: LPCWSTR;
  ClientToken: HANDLE; const Privileges: PRIVILEGE_SET; AccessGranted: BOOL): BOOL; stdcall;

{$IFDEF UNICODE}
function PrivilegedServiceAuditAlarm(SubsystemName, ServiceName: LPCWSTR;
  ClientToken: HANDLE; const Privileges: PRIVILEGE_SET; AccessGranted: BOOL): BOOL; stdcall;
{$ELSE}
function PrivilegedServiceAuditAlarm(SubsystemName, ServiceName: LPCSTR;
  ClientToken: HANDLE; const Privileges: PRIVILEGE_SET; AccessGranted: BOOL): BOOL; stdcall;
{$ENDIF}

function IsWellKnownSid(pSid: PSID; WellKnownSidType: WELL_KNOWN_SID_TYPE): BOOL; stdcall;

function CreateWellKnownSid(WellKnownSidType: WELL_KNOWN_SID_TYPE; DomainSid: PSID;
  pSid: PSID; var cbSid: DWORD): BOOL; stdcall;

function EqualDomainSid(pSid1, pSid2: PSID; pfEqual: PBOOL): BOOL; stdcall;

function GetWindowsAccountDomainSid(pSid, ppDomainSid: PSID; var cbSid: DWORD): BOOL; stdcall;

function IsValidSid(pSid: PSID): BOOL; stdcall;

function EqualSid(pSid1, pSid2: PSID): BOOL; stdcall;

function EqualPrefixSid(pSid1, pSid2: PSID): BOOL; stdcall;

function GetSidLengthRequired(nSubAuthorityCount: UCHAR): DWORD; stdcall;

function AllocateAndInitializeSid(pIdentifierAuthority: PSID_IDENTIFIER_AUTHORITY;
  SubAuthorityCount: BYTE; nSubAuthority0, nSubAuthority1, nSubAuthority2,
  nSubAuthority3, nSubAuthority4, nSubAuthority5, nSubAuthority6,
  nSubAuthority7: DWORD; var pSid: PSID): BOOL; stdcall;

function FreeSid(pSid: PSID): PVOID; stdcall;

function InitializeSid(Sid: PSID; pIdentifierAuthority: PSID_IDENTIFIER_AUTHORITY;
  nSubAuthorityCount: BYTE): BOOL; stdcall;

function GetSidIdentifierAuthority(pSid: PSID): PSID_IDENTIFIER_AUTHORITY; stdcall;

function GetSidSubAuthority(pSid: PSID; nSubAuthority: DWORD): PDWORD; stdcall;

function GetSidSubAuthorityCount(pSid: PSID): PUCHAR; stdcall;

function GetLengthSid(pSid: PSID): DWORD; stdcall;

function CopySid(nDestinationSidLength: DWORD; pDestinationSid: PSID;
  pSourceSid: PSID): BOOL; stdcall;

function AreAllAccessesGranted(GrantedAccess, DesiredAccess: DWORD): BOOL; stdcall;

function AreAnyAccessesGranted(GrantedAccess, DesiredAccess: DWORD): BOOL; stdcall;

procedure MapGenericMask(var AccessMask: DWORD; var GenericMapping: GENERIC_MAPPING); stdcall;

function IsValidAcl(pAcl: PACL): BOOL; stdcall;

function InitializeAcl(pAcl: PACL; nAclLength: DWORD; dwAclRevision: DWORD): BOOL; stdcall;

function GetAclInformation(pAcl: PACL; pAclInformation: LPVOID;
  nAclInformationLength: DWORD; dwAclInformationClass: ACL_INFORMATION_CLASS): BOOL; stdcall;

function SetAclInformation(pAcl: PACL; pAclInformation: LPVOID;
  nAclInformationLength: DWORD; dwAclInformationClass: ACL_INFORMATION_CLASS): BOOL; stdcall;

function AddAce(pAcl: PACL; dwAceRevision: DWORD; dwStartingAceIndex: DWORD;
  pAceList: LPVOID; nAceListLength: DWORD): BOOL; stdcall;

function DeleteAce(pAcl: PACL; dwAceIndex: DWORD): BOOL; stdcall;

function GetAce(pAcl: PACL; dwAceIndex: DWORD; var pAce: LPVOID): BOOL; stdcall;

function AddAccessAllowedAce(pAcl: PACL; dwAceRevision, AccessMask: DWORD; pSid: PSID): BOOL; stdcall;

function AddAccessAllowedAceEx(pAcl: PACL; dwAceRevision, AceFlags,
  AccessMask: DWORD; pSid: PSID): BOOL; stdcall;

function AddAccessDeniedAce(pAcl: PACL; dwAceRevision, AccessMask: DWORD; pSid: PSID): BOOL; stdcall;

function AddAccessDeniedAceEx(pAcl: PACL; dwAceRevision, AceFlags: DWORD;
  AccessMask: DWORD; pSid: PSID): BOOL; stdcall;

function AddAuditAccessAce(pAcl: PACL; dwAceRevision, dwAccessMask: DWORD;
  pSid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL; stdcall;

function AddAuditAccessAceEx(pAcl: PACL; dwAceRevision, AceFlags,
  dwAccessMask: DWORD; pSid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL; stdcall;

function AddAccessAllowedObjectAce(pAcl: PACL; dwAceRevision, AceFlags,
  AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: LPGUID; pSid: PSID): BOOL; stdcall;

function AddAccessDeniedObjectAce(pAcl: PACL; dwAceRevision, AceFlags,
  AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: LPGUID; pSid: PSID): BOOL; stdcall;

function AddAuditAccessObjectAce(pAcl: PACL; dwAceRevision, AceFlags,
  AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: LPGUID; pSid: PSID;
  bAuditSuccess, bAuditFailure: BOOL): BOOL; stdcall;

function FindFirstFreeAce(pAcl: PACL; var pAce: LPVOID): BOOL; stdcall;

function InitializeSecurityDescriptor(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  dwRevision: DWORD): BOOL; stdcall;

function IsValidSecurityDescriptor(pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;

function GetSecurityDescriptorLength(pSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;

function GetSecurityDescriptorControl(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var pControl: SECURITY_DESCRIPTOR_CONTROL; var lpdwRevision: DWORD): BOOL; stdcall;

function SetSecurityDescriptorControl(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  ControlBitsOfInterest, ControlBitsToSet: SECURITY_DESCRIPTOR_CONTROL): BOOL; stdcall;

function SetSecurityDescriptorDacl(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  bDaclPresent: BOOL; pDacl: PACL; bDaclDefaulted: BOOL): BOOL; stdcall;

function GetSecurityDescriptorDacl(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var lpbDaclPresent: BOOL; var pDacl: PACL; var lpbDaclDefaulted: BOOL): BOOL; stdcall;

function SetSecurityDescriptorSacl(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  bSaclPresent: BOOL; pSacl: PACL; bSaclDefaulted: BOOL): BOOL; stdcall;

function GetSecurityDescriptorSacl(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var lpbSaclPresent: BOOL; var pSacl: PACL; var lpbSaclDefaulted: BOOL): BOOL; stdcall;

function SetSecurityDescriptorOwner(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  pOwner: PSID; bOwnerDefaulted: BOOL): BOOL; stdcall;

function GetSecurityDescriptorOwner(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var pOwner: PSID; lpbOwnerDefaulted: PBOOL): BOOL; stdcall;

function SetSecurityDescriptorGroup(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  pGroup: PSID; bGroupDefaulted: BOOL): BOOL; stdcall;

function GetSecurityDescriptorGroup(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var pGroup: PSID; lpbGroupDefaulted: PBOOL): BOOL; stdcall;

function SetSecurityDescriptorRMControl(SecurityDescriptor: PSECURITY_DESCRIPTOR;
  RMControl: PUCHAR): DWORD; stdcall;

function GetSecurityDescriptorRMControl(SecurityDescriptor: PSECURITY_DESCRIPTOR;
  var RMControl: UCHAR): DWORD; stdcall;

function CreatePrivateObjectSecurity(ParentDescriptor, CreatorDescriptor: PSECURITY_DESCRIPTOR;
  var NewDescriptor: PSECURITY_DESCRIPTOR; IsDirectoryObject: BOOL; Token: HANDLE;
  const GenericMapping: GENERIC_MAPPING): BOOL; stdcall;

function ConvertToAutoInheritPrivateObjectSecurity(ParentDescriptor,
  CurrentSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var NewSecurityDescriptor: PSECURITY_DESCRIPTOR; ObjectType: LPGUID;
  IsDirectoryObject: ByteBool; const GenericMapping: GENERIC_MAPPING): BOOL; stdcall;

function CreatePrivateObjectSecurityEx(ParentDescriptor,
  CreatorDescriptor: PSECURITY_DESCRIPTOR;
  var NewDescriptor: PSECURITY_DESCRIPTOR; ObjectType: LPGUID;
  IsContainerObject: BOOL; AutoInheritFlags: ULONG; Token: HANDLE;
  const GenericMapping: GENERIC_MAPPING): BOOL; stdcall;

function SetPrivateObjectSecurity(SecurityInformation: SECURITY_INFORMATION;
  ModificationDescriptor: PSECURITY_DESCRIPTOR;
  var ObjectsSecurityDescriptor: PSECURITY_DESCRIPTOR;
  const GenericMapping: GENERIC_MAPPING; Token: HANDLE): BOOL; stdcall;

function SetPrivateObjectSecurityEx(SecurityInformation: SECURITY_INFORMATION;
  ModificationDescriptor: PSECURITY_DESCRIPTOR;
  var ObjectsSecurityDescriptor: PSECURITY_DESCRIPTOR; AutoInheritFlags: ULONG;
  const GenericMapping: GENERIC_MAPPING; Token: HANDLE): BOOL; stdcall;

function GetPrivateObjectSecurity(ObjectDescriptor: PSECURITY_DESCRIPTOR;
  SecurityInformation: SECURITY_INFORMATION;
  ResultantDescriptor: PSECURITY_DESCRIPTOR; DescriptorLength: DWORD;
  var ReturnLength: DWORD): BOOL; stdcall;

function DestroyPrivateObjectSecurity(var ObjectDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;

function MakeSelfRelativeSD(pAbsoluteSecurityDescriptor: PSECURITY_DESCRIPTOR;
  pSelfRelativeSecurityDescriptor: PSECURITY_DESCRIPTOR; var lpdwBufferLength: DWORD): BOOL; stdcall;

function MakeAbsoluteSD(pSelfRelativeSecurityDescriptor: PSECURITY_DESCRIPTOR;
  pAbsoluteSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var lpdwAbsoluteSecurityDescriptorSize: DWORD; pDacl: PACL;
  var lpdwDaclSize: DWORD; pSacl: PACL; var lpdwSaclSize: DWORD; pOwner: PSID;
  var lpdwOwnerSize: DWORD; pPrimaryGroup: PSID; var lpdwPrimaryGroupSize: DWORD): BOOL; stdcall;

function MakeAbsoluteSD2(pSelfRelativeSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var lpdwBufferSize: DWORD): BOOL; stdcall;

function SetFileSecurityA(lpFileName: LPCSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;
function SetFileSecurityW(lpFileName: LPCWSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetFileSecurity(lpFileName: LPCWSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;
{$ELSE}
function SetFileSecurity(lpFileName: LPCSTR; SecurityInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;
{$ENDIF}

function GetFileSecurityA(lpFileName: LPCSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;
function GetFileSecurityW(lpFileName: LPCWSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetFileSecurity(lpFileName: LPCWSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$ELSE}
function GetFileSecurity(lpFileName: LPCSTR; RequestedInformation: SECURITY_INFORMATION;
  pSecurityDescriptor: PSECURITY_DESCRIPTOR; nLength: DWORD;
  var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$ENDIF}

function SetKernelObjectSecurity(Handle: HANDLE; SecurityInformation: SECURITY_INFORMATION;
  SecurityDescriptor: PSECURITY_DESCRIPTOR): BOOL; stdcall;

function FindFirstChangeNotificationA(lpPathName: LPCSTR; bWatchSubtree: Cardinal;
  dwNotifyFilter: DWORD): HANDLE; stdcall;
function FindFirstChangeNotificationW(lpPathName: LPCWSTR; bWatchSubtree: Cardinal;
  dwNotifyFilter: DWORD): HANDLE; stdcall;

{$IFDEF UNICODE}
function FindFirstChangeNotification(lpPathName: LPCWSTR; bWatchSubtree: Cardinal;
  dwNotifyFilter: DWORD): HANDLE; stdcall;
{$ELSE}
function FindFirstChangeNotification(lpPathName: LPCSTR; bWatchSubtree: Cardinal;
  dwNotifyFilter: DWORD): HANDLE; stdcall;
{$ENDIF}

function FindNextChangeNotification(hChangeHandle: HANDLE): BOOL; stdcall;

function FindCloseChangeNotification(hChangeHandle: HANDLE): BOOL; stdcall;

function ReadDirectoryChangesW(hDirectory: HANDLE; lpBuffer: LPVOID;
  nBufferLength: DWORD; bWatchSubtree: BOOL; dwNotifyFilter: DWORD;
  lpBytesReturned: LPDWORD; lpOverlapped: LPOVERLAPPED;
  lpCompletionRoutine: LPOVERLAPPED_COMPLETION_ROUTINE): BOOL; stdcall;

function VirtualLock(lpAddress: LPVOID; dwSize: SIZE_T): BOOL; stdcall;

function VirtualUnlock(lpAddress: LPVOID; dwSize: SIZE_T): BOOL; stdcall;

function MapViewOfFileEx(hFileMappingObject: HANDLE; dwDesiredAccess: DWORD;
  dwFileOffsetHigh: DWORD; dwFileOffsetLow: DWORD; dwNumberOfBytesToMap: SIZE_T;
  lpBaseAddress: LPVOID): LPVOID; stdcall;

function SetPriorityClass(hProcess: HANDLE; dwPriorityClass: DWORD): BOOL; stdcall;

function GetPriorityClass(hProcess: HANDLE): DWORD; stdcall;

function IsBadReadPtr(lp: LPVOID; ucb: UINT_PTR): BOOL; stdcall;

function IsBadWritePtr(lp: LPVOID; ucb: UINT_PTR): BOOL; stdcall;

function IsBadHugeReadPtr(lp: LPVOID; ucb: UINT_PTR): BOOL; stdcall;

function IsBadHugeWritePtr(lp: LPVOID; ucb: UINT_PTR): BOOL; stdcall;

function IsBadCodePtr(lpfn: FARPROC): BOOL; stdcall;

function IsBadStringPtrA(lpsz: LPCSTR; ucchMax: UINT_PTR): BOOL; stdcall;
function IsBadStringPtrW(lpsz: LPCWSTR; ucchMax: UINT_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function IsBadStringPtr(lpsz: LPCWSTR; ucchMax: UINT_PTR): BOOL; stdcall;
{$ELSE}
function IsBadStringPtr(lpsz: LPCSTR; ucchMax: UINT_PTR): BOOL; stdcall;
{$ENDIF}

function LookupAccountSidA(lpSystemName: LPCSTR; Sid: PSID; Name: LPSTR;
  var cchName: DWORD; ReferencedDomainName: LPSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;
function LookupAccountSidW(lpSystemName: LPCWSTR; Sid: PSID; Name: LPWSTR;
  var cchName: DWORD; ReferencedDomainName: LPWSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;

{$IFDEF UNICODE}
function LookupAccountSid(lpSystemName: LPCWSTR; Sid: PSID; Name: LPWSTR;
  var cchName: DWORD; ReferencedDomainName: LPWSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;
{$ELSE}
function LookupAccountSid(lpSystemName: LPCSTR; Sid: PSID; Name: LPSTR;
  var cchName: DWORD; ReferencedDomainName: LPSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;
{$ENDIF}

function LookupAccountNameA(lpSystemName, lpAccountName: LPCSTR; Sid: PSID;
  var cbSid: DWORD; ReferencedDomainName: LPSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;
function LookupAccountNameW(lpSystemName, lpAccountName: LPCWSTR; Sid: PSID;
  var cbSid: DWORD; ReferencedDomainName: LPWSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;

{$IFDEF UNICODE}
function LookupAccountName(lpSystemName: LPCWSTR; lpAccountName: LPCWSTR; Sid: PSID;
  var cbSid: DWORD; ReferencedDomainName: LPWSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;
{$ELSE}
function LookupAccountName(lpSystemName: LPCSTR; lpAccountName: LPCSTR; Sid: PSID;
  var cbSid: DWORD; ReferencedDomainName: LPSTR; var cchReferencedDomainName: DWORD;
  var peUse: SID_NAME_USE): BOOL; stdcall;
{$ENDIF}

function LookupPrivilegeValueA(lpSystemName, lpName: LPCSTR; var lpLuid: LUID): BOOL; stdcall;
function LookupPrivilegeValueW(lpSystemName, lpName: LPCWSTR; var lpLuid: LUID): BOOL; stdcall;

{$IFDEF UNICODE}
function LookupPrivilegeValue(lpSystemName, lpName: LPCWSTR; var lpLuid: LUID): BOOL; stdcall;
{$ELSE}
function LookupPrivilegeValue(lpSystemName, lpName: LPCSTR; var lpLuid: LUID): BOOL; stdcall;
{$ENDIF}

function LookupPrivilegeNameA(lpSystemName: LPCSTR; const lpLuid: LUID;
  lpName: LPSTR; var cbName: DWORD): BOOL; stdcall;
function LookupPrivilegeNameW(lpSystemName: LPCWSTR; const lpLuid: LUID;
  lpName: LPWSTR; var cbName: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function LookupPrivilegeName(lpSystemName: LPCWSTR; const lpLuid: LUID;
  lpName: LPWSTR; var cbName: DWORD): BOOL; stdcall;
{$ELSE}
function LookupPrivilegeName(lpSystemName: LPCSTR; const lpLuid: LUID;
  lpName: LPSTR; var cbName: DWORD): BOOL; stdcall;
{$ENDIF}

function LookupPrivilegeDisplayNameA(lpSystemName, lpName: LPCSTR;
  lpDisplayName: LPSTR; var cbDisplayName, lpLanguageId: DWORD): BOOL; stdcall;
function LookupPrivilegeDisplayNameW(lpSystemName, lpName: LPCWSTR;
  lpDisplayName: LPWSTR; var cbDisplayName, lpLanguageId: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function LookupPrivilegeDisplayName(lpSystemName, lpName: LPCWSTR;
  lpDisplayName: LPWSTR; var cbDisplayName, lpLanguageId: DWORD): BOOL; stdcall;
{$ELSE}
function LookupPrivilegeDisplayName(lpSystemName, lpName: LPCSTR;
  lpDisplayName: LPSTR; var cbDisplayName, lpLanguageId: DWORD): BOOL; stdcall;
{$ENDIF}

function AllocateLocallyUniqueId(var Luid: LUID): BOOL; stdcall;

function BuildCommDCBA(lpDef: LPCSTR; var lpDCB: DCB): BOOL; stdcall;
function BuildCommDCBW(lpDef: LPCWSTR; var lpDCB: DCB): BOOL; stdcall;

{$IFDEF UNICODE}
function BuildCommDCB(lpDef: LPCWSTR; var lpDCB: DCB): BOOL; stdcall;
{$ELSE}
function BuildCommDCB(lpDef: LPCSTR; var lpDCB: DCB): BOOL; stdcall;
{$ENDIF}

function BuildCommDCBAndTimeoutsA(lpDef: LPCSTR; var lpDCB: DCB;
  var lpCommTimeouts: COMMTIMEOUTS): BOOL; stdcall;
function BuildCommDCBAndTimeoutsW(lpDef: LPCWSTR; var lpDCB: DCB;
  var lpCommTimeouts: COMMTIMEOUTS): BOOL; stdcall;

{$IFDEF UNICODE}
function BuildCommDCBAndTimeouts(lpDef: LPCWSTR; var lpDCB: DCB;
  var lpCommTimeouts: COMMTIMEOUTS): BOOL; stdcall;
{$ELSE}
function BuildCommDCBAndTimeouts(lpDef: LPCSTR; var lpDCB: DCB;
  var lpCommTimeouts: COMMTIMEOUTS): BOOL; stdcall;
{$ENDIF}

function CommConfigDialogA(lpszName: LPCSTR; hWnd: HWND; var lpCC: COMMCONFIG): BOOL; stdcall;
function CommConfigDialogW(lpszName: LPCWSTR; hWnd: HWND; var lpCC: COMMCONFIG): BOOL; stdcall;

{$IFDEF UNICODE}
function CommConfigDialog(lpszName: LPCWSTR; hWnd: HWND; var lpCC: COMMCONFIG): BOOL; stdcall;
{$ELSE}
function CommConfigDialog(lpszName: LPCSTR; hWnd: HWND; var lpCC: COMMCONFIG): BOOL; stdcall;
{$ENDIF}

function GetDefaultCommConfigA(lpszName: LPCSTR; var lpCC: COMMCONFIG;
  var lpdwSize: DWORD): BOOL; stdcall;
function GetDefaultCommConfigW(lpszName: LPCWSTR; var lpCC: COMMCONFIG;
  var lpdwSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetDefaultCommConfig(lpszName: LPCWSTR; var lpCC: COMMCONFIG;
  var lpdwSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetDefaultCommConfig(lpszName: LPCSTR; var lpCC: COMMCONFIG;
  var lpdwSize: DWORD): BOOL; stdcall;
{$ENDIF}

function SetDefaultCommConfigA(lpszName: LPCSTR; const lpCC: COMMCONFIG;
  dwSize: DWORD): BOOL; stdcall;
function SetDefaultCommConfigW(lpszName: LPCWSTR; const lpCC: COMMCONFIG;
  dwSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function SetDefaultCommConfig(lpszName: LPCWSTR; const lpCC: COMMCONFIG;
  dwSize: DWORD): BOOL; stdcall;
{$ELSE}
function SetDefaultCommConfig(lpszName: LPCSTR; const lpCC: COMMCONFIG;
  dwSize: DWORD): BOOL; stdcall;
{$ENDIF}

const
  MAX_COMPUTERNAME_LENGTH = 15;

function GetComputerNameA(lpBuffer: LPSTR; var nSize: DWORD): BOOL; stdcall;
function GetComputerNameW(lpBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetComputerName(lpBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetComputerName(lpBuffer: LPSTR; var nSize: DWORD): BOOL; stdcall;
{$ENDIF}

function SetComputerNameA(lpComputerName: LPCSTR): BOOL; stdcall;
function SetComputerNameW(lpComputerName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetComputerName(lpComputerName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetComputerName(lpComputerName: LPCSTR): BOOL; stdcall;
{$ENDIF}

type
  _COMPUTER_NAME_FORMAT = (
    ComputerNameNetBIOS,
    ComputerNameDnsHostname,
    ComputerNameDnsDomain,
    ComputerNameDnsFullyQualified,
    ComputerNamePhysicalNetBIOS,
    ComputerNamePhysicalDnsHostname,
    ComputerNamePhysicalDnsDomain,
    ComputerNamePhysicalDnsFullyQualified,
    ComputerNameMax);
  COMPUTER_NAME_FORMAT = _COMPUTER_NAME_FORMAT;
  TComputerNameFormat = COMPUTER_NAME_FORMAT;

function GetComputerNameExA(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPSTR;
  var nSize: DWORD): BOOL; stdcall;
function GetComputerNameExW(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPWSTR;
  var nSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetComputerNameEx(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPWSTR;
  varnSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetComputerNameEx(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPSTR;
  var nSize: DWORD): BOOL; stdcall;
{$ENDIF}

function SetComputerNameExA(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPCSTR): BOOL; stdcall;
function SetComputerNameExW(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetComputerNameEx(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetComputerNameEx(NameType: COMPUTER_NAME_FORMAT; lpBuffer: LPCSTR): BOOL; stdcall;
{$ENDIF}

function DnsHostnameToComputerNameA(Hostname, ComputerName: LPSTR; var nSize: DWORD): BOOL; stdcall;
function DnsHostnameToComputerNameW(Hostname, ComputerName: LPWSTR; var nSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function DnsHostnameToComputerName(Hostname, ComputerName: LPWSTR; var nSize: DWORD): BOOL; stdcall;
{$ELSE}
function DnsHostnameToComputerName(Hostname, ComputerName: LPSTR; var nSize: DWORD): BOOL; stdcall;
{$ENDIF}

function GetUserNameA(lpBuffer: LPSTR; var nSize: DWORD): BOOL; stdcall;
function GetUserNameW(lpBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetUserName(lpBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;
{$ELSE}
function GetUserName(lpBuffer: LPSTR; var nSize: DWORD): BOOL; stdcall;
{$ENDIF}

//
// Logon Support APIs
//

const
  LOGON32_LOGON_INTERACTIVE       = 2;
  LOGON32_LOGON_NETWORK           = 3;
  LOGON32_LOGON_BATCH             = 4;
  LOGON32_LOGON_SERVICE           = 5;
  LOGON32_LOGON_UNLOCK            = 7;
  LOGON32_LOGON_NETWORK_CLEARTEXT = 8;
  LOGON32_LOGON_NEW_CREDENTIALS   = 9;

  LOGON32_PROVIDER_DEFAULT = 0;
  LOGON32_PROVIDER_WINNT35 = 1;
  LOGON32_PROVIDER_WINNT40 = 2;
  LOGON32_PROVIDER_WINNT50 = 3;

function LogonUserA(lpszUsername, lpszDomain, lpszPassword: LPCSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE): BOOL; stdcall;
function LogonUserW(lpszUsername, lpszDomain, lpszPassword: LPCWSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE): BOOL; stdcall;

{$IFDEF UNICODE}
function LogonUser(lpszUsername, lpszDomain, lpszPassword: LPCWSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE): BOOL; stdcall;
{$ELSE}
function LogonUser(lpszUsername, lpszDomain, lpszPassword: LPCSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE): BOOL; stdcall;
{$ENDIF}

function LogonUserExA(lpszUsername, lpszDomain, lpszPassword: LPCSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE; ppLogonSid: PPSID;
  ppProfileBuffer: PPVOID; pdwProfileLength: LPDWORD; pQuotaLimits: PQUOTA_LIMITS): BOOL; stdcall;
function LogonUserExW(lpszUsername, lpszDomain, lpszPassword: LPCWSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE; ppLogonSid: PPSID;
  ppProfileBuffer: PPVOID; pdwProfileLength: LPDWORD; pQuotaLimits: PQUOTA_LIMITS): BOOL; stdcall;

{$IFDEF UNICODE}
function LogonUserEx(lpszUsername, lpszDomain, lpszPassword: LPCWSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE; ppLogonSid: PPSID;
  ppProfileBuffer: PPVOID; pdwProfileLength: LPDWORD; pQuotaLimits: PQUOTA_LIMITS): BOOL; stdcall;
{$ELSE}
function LogonUserEx(lpszUsername, lpszDomain, lpszPassword: LPCSTR;
  dwLogonType, dwLogonProvider: DWORD; var phToken: HANDLE; ppLogonSid: PPSID;
  ppProfileBuffer: PPVOID; pdwProfileLength: LPDWORD; pQuotaLimits: PQUOTA_LIMITS): BOOL; stdcall;
{$ENDIF}

function ImpersonateLoggedOnUser(hToken: HANDLE): BOOL; stdcall;

function CreateProcessAsUserA(hToken: HANDLE; lpApplicationName: LPCSTR;
  lpCommandLine: LPSTR; lpProcessAttributes: LPSECURITY_ATTRIBUTES;
  lpThreadAttributes: LPSECURITY_ATTRIBUTES; bInheritHandles: BOOL;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCSTR;
  const lpStartupInfo: STARTUPINFOA; var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;
function CreateProcessAsUserW(hToken: HANDLE; lpApplicationName: LPCWSTR;
  lpCommandLine: LPWSTR; lpProcessAttributes: LPSECURITY_ATTRIBUTES;
  lpThreadAttributes: LPSECURITY_ATTRIBUTES; bInheritHandles: BOOL;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR;
  const lpStartupInfo: STARTUPINFOW; var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;

{$IFDEF UNICODE}
function CreateProcessAsUser(hToken: HANDLE; lpApplicationName: LPCWSTR;
  lpCommandLine: LPWSTR; lpProcessAttributes: LPSECURITY_ATTRIBUTES;
  lpThreadAttributes: LPSECURITY_ATTRIBUTES; bInheritHandles: BOOL;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR;
  const lpStartupInfo: STARTUPINFOW; var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;
{$ELSE}
function CreateProcessAsUser(hToken: HANDLE; lpApplicationName: LPCSTR;
  lpCommandLine: LPSTR; lpProcessAttributes: LPSECURITY_ATTRIBUTES;
  lpThreadAttributes: LPSECURITY_ATTRIBUTES; bInheritHandles: BOOL;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCSTR;
  const lpStartupInfo: STARTUPINFOA; var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;
{$ENDIF}

//
// LogonFlags
//

const
  LOGON_WITH_PROFILE         = $00000001;
  LOGON_NETCREDENTIALS_ONLY  = $00000002;
  LOGON_ZERO_PASSWORD_BUFFER = DWORD($80000000);

function CreateProcessWithLogonW(lpUsername, lpDomain, lpPassword: LPCWSTR;
  dwLogonFlags: DWORD; lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR;
  const lpStartupInfo: STARTUPINFOW; var lpProcessInformation: PROCESS_INFORMATION): BOOL; stdcall;

function CreateProcessWithTokenW(hToken: HANDLE; dwLogonFlags: DWORD; lpApplicationName: LPCWSTR; lpCommandLine: LPWSTR;
  dwCreationFlags: DWORD; lpEnvironment: LPVOID; lpCurrentDirectory: LPCWSTR; lpStartupInfo: LPSTARTUPINFOW;
  lpProcessInformation: LPPROCESS_INFORMATION): BOOL; stdcall;

function ImpersonateAnonymousToken(ThreadHandle: HANDLE): BOOL; stdcall;

function DuplicateTokenEx(hExistingToken: HANDLE; dwDesiredAccess: DWORD;
  lpTokenAttributes: LPSECURITY_ATTRIBUTES; ImpersonationLevel: SECURITY_IMPERSONATION_LEVEL;
  TokenType: TOKEN_TYPE; var phNewToken: HANDLE): BOOL; stdcall;

function CreateRestrictedToken(ExistingTokenHandle: HANDLE; Flags: DWORD;
  DisableSidCount: DWORD; SidsToDisable: PSID_AND_ATTRIBUTES;
  DeletePrivilegeCount: DWORD; PrivilegesToDelete: PLUID_AND_ATTRIBUTES;
  RestrictedSidCount: DWORD; SidsToRestrict: PSID_AND_ATTRIBUTES;
  var NewTokenHandle: HANDLE): BOOL; stdcall;

function IsTokenRestricted(TokenHandle: HANDLE): BOOL; stdcall;

function CheckTokenMembership(TokenHandle: HANDLE; SidToCheck: PSID; var IsMember: BOOL): BOOL; stdcall;

function IsTokenUntrusted(TokenHandle: HANDLE): BOOL; stdcall;

//
// Thread pool API's
//

type
  WAITORTIMERCALLBACK = WAITORTIMERCALLBACKFUNC;
  TWaitOrTimerCallback = WAITORTIMERCALLBACKFUNC;

function RegisterWaitForSingleObject(var phNewWaitObject: HANDLE; hObject: HANDLE;
  Callback: WAITORTIMERCALLBACK; Context: PVOID; dwMilliseconds, dwFlags: ULONG): BOOL; stdcall;

function RegisterWaitForSingleObjectEx(hObject: HANDLE;
  Callback: WAITORTIMERCALLBACK; Context: PVOID; dwMilliseconds, dwFlags: ULONG): HANDLE; stdcall;

function UnregisterWait(WaitHandle: HANDLE): BOOL; stdcall;

function UnregisterWaitEx(WaitHandle, CompletionEvent: HANDLE): BOOL; stdcall;

function QueueUserWorkItem(Function_: LPTHREAD_START_ROUTINE; Context: PVOID;
  Flags: ULONG): BOOL; stdcall;

function BindIoCompletionCallback(FileHandle: HANDLE;
  Function_: LPOVERLAPPED_COMPLETION_ROUTINE; Flags: ULONG): BOOL; stdcall;

function CreateTimerQueue: HANDLE; stdcall;

function CreateTimerQueueTimer(var phNewTimer: HANDLE; TimerQueue: HANDLE;
  Callback: WAITORTIMERCALLBACK; Parameter: PVOID; DueTime, Period: DWORD;
  Flags: ULONG): BOOL; stdcall;

function ChangeTimerQueueTimer(TimerQueue, Timer: HANDLE; DueTime, Period: ULONG): BOOL; stdcall;

function DeleteTimerQueueTimer(TimerQueue, Timer, CompletionEvent: HANDLE): BOOL; stdcall;

function DeleteTimerQueueEx(TimerQueue, CompletionEvent: HANDLE): BOOL; stdcall;

function SetTimerQueueTimer(TimerQueue, Callback: WAITORTIMERCALLBACK;
  Parameter: PVOID; DueTime, Period: DWORD; PreferIo: BOOL): HANDLE; stdcall;

function CancelTimerQueueTimer(TimerQueue, Timer: HANDLE): BOOL; stdcall;

function DeleteTimerQueue(TimerQueue: HANDLE): BOOL; stdcall;

//
// Plug-and-Play API's
//

const
  HW_PROFILE_GUIDLEN = 39; // 36-characters plus NULL terminator
  MAX_PROFILE_LEN    = 80;

  DOCKINFO_UNDOCKED      = ($1);
  DOCKINFO_DOCKED        = ($2);
  DOCKINFO_USER_SUPPLIED = ($4);
  DOCKINFO_USER_UNDOCKED = (DOCKINFO_USER_SUPPLIED or DOCKINFO_UNDOCKED);
  DOCKINFO_USER_DOCKED   = (DOCKINFO_USER_SUPPLIED or DOCKINFO_DOCKED);

type
  LPHW_PROFILE_INFOA = ^HW_PROFILE_INFOA;
  tagHW_PROFILE_INFOA = record
    dwDockInfo: DWORD;
    szHwProfileGuid: array [0..HW_PROFILE_GUIDLEN - 1] of CHAR;
    szHwProfileName: array [0..MAX_PROFILE_LEN - 1] of CHAR;
  end;
  HW_PROFILE_INFOA = tagHW_PROFILE_INFOA;
  THWProfileInfoA = HW_PROFILE_INFOA;
  PHWProfileInfoA = LPHW_PROFILE_INFOA;

  LPHW_PROFILE_INFOW = ^HW_PROFILE_INFOW;
  tagHW_PROFILE_INFOW = record
    dwDockInfo: DWORD;
    szHwProfileGuid: array [0..HW_PROFILE_GUIDLEN - 1] of WCHAR;
    szHwProfileName: array [0..MAX_PROFILE_LEN - 1] of WCHAR;
  end;
  HW_PROFILE_INFOW = tagHW_PROFILE_INFOW;
  THWProfileInfoW = HW_PROFILE_INFOW;
  PHWProfileInfoW = LPHW_PROFILE_INFOW;

{$IFDEF UNICODE}
  HW_PROFILE_INFO = HW_PROFILE_INFOW;
  LPHW_PROFILE_INFO = LPHW_PROFILE_INFOW;
  THWProfileInfo = THWProfileInfoW;
  PHWProfileInfo = PHWProfileInfoW;
{$ELSE}
  HW_PROFILE_INFO = HW_PROFILE_INFOA;
  LPHW_PROFILE_INFO = LPHW_PROFILE_INFOA;
  THWProfileInfo = THWProfileInfoA;
  PHWProfileInfo = PHWProfileInfoA;
{$ENDIF}

function GetCurrentHwProfileA(var lpHwProfileInfo: HW_PROFILE_INFOA): BOOL; stdcall;
function GetCurrentHwProfileW(var lpHwProfileInfo: HW_PROFILE_INFOW): BOOL; stdcall;

{$IFDEF UNICODE}
function GetCurrentHwProfile(var lpHwProfileInfo: HW_PROFILE_INFOW): BOOL; stdcall;
{$ELSE}
function GetCurrentHwProfile(var lpHwProfileInfo: HW_PROFILE_INFOA): BOOL; stdcall;
{$ENDIF}

//
// Performance counter API's
//

function QueryPerformanceCounter(var lpPerformanceCount: LARGE_INTEGER): BOOL; stdcall;

function QueryPerformanceFrequency(var lpFrequency: LARGE_INTEGER): BOOL; stdcall;

function GetVersionExA(lpVersionInformation: LPOSVERSIONINFOA): BOOL; stdcall;
function GetVersionExW(lpVersionInformation: LPOSVERSIONINFOW): BOOL; stdcall;

{$IFDEF UNICODE}
function GetVersionEx(lpVersionInformation: LPOSVERSIONINFOW): BOOL; stdcall;
{$ELSE}
function GetVersionEx(lpVersionInformation: LPOSVERSIONINFOA): BOOL; stdcall;
{$ENDIF}

function VerifyVersionInfoA(var lpVersionInformation: OSVERSIONINFOEXA;
  dwTypeMask: DWORD; dwlConditionMask: DWORDLONG): BOOL; stdcall;
function VerifyVersionInfoW(var lpVersionInformation: OSVERSIONINFOEXW;
  dwTypeMask: DWORD; dwlConditionMask: DWORDLONG): BOOL; stdcall;

{$IFDEF UNICODE}
function VerifyVersionInfo(var lpVersionInformation: OSVERSIONINFOEXW;
  dwTypeMask: DWORD; dwlConditionMask: DWORDLONG): BOOL; stdcall;
{$ELSE}
function VerifyVersionInfo(var lpVersionInformation: OSVERSIONINFOEXA;
  dwTypeMask: DWORD; dwlConditionMask: DWORDLONG): BOOL; stdcall;
{$ENDIF}

// DOS and OS/2 Compatible Error Code definitions returned by the Win32 Base
// API functions.
//

// #include <winerror.h>

// Abnormal termination codes

const
  TC_NORMAL  = 0;
  TC_HARDERR = 1;
  TC_GP_TRAP = 2;
  TC_SIGNAL  = 3;

//
// Power Management APIs
//

  AC_LINE_OFFLINE      = $00;
  AC_LINE_ONLINE       = $01;
  AC_LINE_BACKUP_POWER = $02;
  AC_LINE_UNKNOWN      = $FF;

  BATTERY_FLAG_HIGH       = $01;
  BATTERY_FLAG_LOW        = $02;
  BATTERY_FLAG_CRITICAL   = $04;
  BATTERY_FLAG_CHARGING   = $08;
  BATTERY_FLAG_NO_BATTERY = $80;
  BATTERY_FLAG_UNKNOWN    = $FF;

  BATTERY_PERCENTAGE_UNKNOWN = $FF;

  BATTERY_LIFE_UNKNOWN = DWORD($FFFFFFFF);

type
  LPSYSTEM_POWER_STATUS = ^SYSTEM_POWER_STATUS;
  _SYSTEM_POWER_STATUS = record
    ACLineStatus: BYTE;
    BatteryFlag: BYTE;
    BatteryLifePercent: BYTE;
    Reserved1: BYTE;
    BatteryLifeTime: DWORD;
    BatteryFullLifeTime: DWORD;
  end;
  SYSTEM_POWER_STATUS = _SYSTEM_POWER_STATUS;
  TSystemPowerStatus = SYSTEM_POWER_STATUS;
  PSystemPowerStatus = LPSYSTEM_POWER_STATUS;

function GetSystemPowerStatus(var lpSystemPowerStatus: SYSTEM_POWER_STATUS): BOOL; stdcall;

function SetSystemPowerState(fSuspend, fForce: BOOL): BOOL; stdcall;

//
// Very Large Memory API Subset
//

function AllocateUserPhysicalPages(hProcess: HANDLE; var NumberOfPages: ULONG_PTR;
  PageArray: PULONG_PTR): BOOL; stdcall;

function FreeUserPhysicalPages(hProcess: HANDLE; var NumberOfPages: ULONG_PTR;
  PageArray: PULONG_PTR): BOOL; stdcall;

function MapUserPhysicalPages(VirtualAddress: PVOID; NumberOfPages: ULONG_PTR;
  PageArray: PULONG_PTR): BOOL; stdcall;

function MapUserPhysicalPagesScatter(VirtualAddresses: PVOID; NumberOfPages: ULONG_PTR;
  PageArray: PULONG_PTR): BOOL; stdcall;

function CreateJobObjectA(lpJobAttributes: LPSECURITY_ATTRIBUTES; lpName: LPCSTR): HANDLE; stdcall;
function CreateJobObjectW(lpJobAttributes: LPSECURITY_ATTRIBUTES; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateJobObject(lpJobAttributes: LPSECURITY_ATTRIBUTES; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function CreateJobObject(lpJobAttributes: LPSECURITY_ATTRIBUTES; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function OpenJobObjectA(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall;
function OpenJobObjectW(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function OpenJobObject(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function OpenJobObject(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function AssignProcessToJobObject(hJob, hProcess: HANDLE): BOOL; stdcall;

function TerminateJobObject(hJob: HANDLE; uExitCode: UINT): BOOL; stdcall;

function QueryInformationJobObject(hJob: HANDLE; JobObjectInformationClass: JOBOBJECTINFOCLASS;
  lpJobObjectInformation: LPVOID; cbJobObjectInformationLength: DWORD;
  lpReturnLength: LPDWORD): BOOL; stdcall;

function SetInformationJobObject(hJob: HANDLE; JobObjectInformationClass: JOBOBJECTINFOCLASS;
  lpJobObjectInformation: LPVOID; cbJobObjectInformationLength: DWORD): BOOL; stdcall;

function IsProcessInJob(ProcessHandle, JobHandle: HANDLE; var Result_: BOOL): BOOL; stdcall;

function CreateJobSet(NumJob: ULONG; UserJobSet: PJOB_SET_ARRAY; Flags: ULONG): BOOL; stdcall;

function AddVectoredExceptionHandler(FirstHandler: ULONG;
  VectoredHandler: PVECTORED_EXCEPTION_HANDLER): PVOID; stdcall;

function RemoveVectoredExceptionHandler(VectoredHandlerHandle: PVOID): ULONG; stdcall;

//
// New Volume Mount Point API.
//

function FindFirstVolumeA(lpszVolumeName: LPSTR; cchBufferLength: DWORD): HANDLE; stdcall;
function FindFirstVolumeW(lpszVolumeName: LPWSTR; cchBufferLength: DWORD): HANDLE; stdcall;

{$IFDEF UNICODE}
function FindFirstVolume(lpszVolumeName: LPWSTR; cchBufferLength: DWORD): HANDLE; stdcall;
{$ELSE}
function FindFirstVolume(lpszVolumeName: LPSTR; cchBufferLength: DWORD): HANDLE; stdcall;
{$ENDIF}

function FindNextVolumeA(hFindVolume: HANDLE; lpszVolumeName: LPSTR;
  cchBufferLength: DWORD): BOOL; stdcall;
function FindNextVolumeW(hFindVolume: HANDLE; lpszVolumeName: LPWSTR;
  cchBufferLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function FindNextVolume(hFindVolume: HANDLE; lpszVolumeName: LPWSTR;
  cchBufferLength: DWORD): BOOL; stdcall;
{$ELSE}
function FindNextVolume(hFindVolume: HANDLE; lpszVolumeName: LPSTR;
  cchBufferLength: DWORD): BOOL; stdcall;
{$ENDIF}

function FindVolumeClose(hFindVolume: HANDLE): BOOL; stdcall;

function FindFirstVolumeMountPointA(lpszRootPathName: LPCSTR;
  lpszVolumeMountPoint: LPSTR; cchBufferLength: DWORD): HANDLE; stdcall;
function FindFirstVolumeMountPointW(lpszRootPathName: LPCWSTR;
  lpszVolumeMountPoint: LPWSTR; cchBufferLength: DWORD): HANDLE; stdcall;

{$IFDEF UNICODE}
function FindFirstVolumeMountPoint(lpszRootPathName: LPCWSTR;
  lpszVolumeMountPoint: LPWSTR; cchBufferLength: DWORD): HANDLE; stdcall;
{$ELSE}
function FindFirstVolumeMountPoint(lpszRootPathName: LPCSTR;
  lpszVolumeMountPoint: LPSTR; cchBufferLength: DWORD): HANDLE; stdcall;
{$ENDIF}

function FindNextVolumeMountPointA(hFindVolumeMountPoint: HANDLE;
  lpszVolumeMountPoint: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
function FindNextVolumeMountPointW(hFindVolumeMountPoint: HANDLE;
  lpszVolumeMountPoint: LPWSTR; cchBufferLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function FindNextVolumeMountPoint(hFindVolumeMountPoint: HANDLE;
  lpszVolumeMountPoint: LPWSTR; cchBufferLength: DWORD): BOOL; stdcall;
{$ELSE}
function FindNextVolumeMountPoint(hFindVolumeMountPoint: HANDLE;
  lpszVolumeMountPoint: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
{$ENDIF}

function FindVolumeMountPointClose(hFindVolumeMountPoint: HANDLE): BOOL; stdcall;

function SetVolumeMountPointA(lpszVolumeMountPoint, lpszVolumeName: LPCSTR): BOOL; stdcall;
function SetVolumeMountPointW(lpszVolumeMountPoint, lpszVolumeName: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetVolumeMountPoint(lpszVolumeMountPoint, lpszVolumeName: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetVolumeMountPoint(lpszVolumeMountPoint, lpszVolumeName: LPCSTR): BOOL; stdcall;
{$ENDIF}

function DeleteVolumeMountPointA(lpszVolumeMountPoint: LPCSTR): BOOL; stdcall;
function DeleteVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCWSTR): BOOL; stdcall;
{$ELSE}
function DeleteVolumeMountPoint(lpszVolumeMountPoint: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetVolumeNameForVolumeMountPointA(lpszVolumeMountPoint: LPCSTR;
  lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
function GetVolumeNameForVolumeMountPointW(lpszVolumeMountPoint: LPCWSTR;
  lpszVolumeName: LPWSTR; cchBufferLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: LPCWSTR;
  lpszVolumeName: LPWSTR; cchBufferLength: DWORD): BOOL; stdcall;
{$ELSE}
function GetVolumeNameForVolumeMountPoint(lpszVolumeMountPoint: LPCSTR;
  lpszVolumeName: LPSTR; cchBufferLength: DWORD): BOOL; stdcall;
{$ENDIF}

function GetVolumePathNameA(lpszFileName: LPCSTR; lpszVolumePathName: LPSTR;
  cchBufferLength: DWORD): BOOL; stdcall;
function GetVolumePathNameW(lpszFileName: LPCWSTR; lpszVolumePathName: LPWSTR;
  cchBufferLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetVolumePathName(lpszFileName: LPCWSTR; lpszVolumePathName: LPWSTR;
  cchBufferLength: DWORD): BOOL; stdcall;
{$ELSE}
function GetVolumePathName(lpszFileName: LPCSTR; lpszVolumePathName: LPSTR;
  cchBufferLength: DWORD): BOOL; stdcall;
{$ENDIF}

function GetVolumePathNamesForVolumeNameA(lpszVolumeName, lpszVolumePathNames: LPCSTR;
  cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall;
function GetVolumePathNamesForVolumeNameW(lpszVolumeName, lpszVolumePathNames: LPCWSTR;
  cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetVolumePathNamesForVolumeName(lpszVolumeName, lpszVolumePathNames: LPCWSTR;
  cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall;
{$ELSE}
function GetVolumePathNamesForVolumeName(lpszVolumeName, lpszVolumePathNames: LPCSTR;
  cchBufferLength: DWORD; var lpcchReturnLength: DWORD): BOOL; stdcall;
{$ENDIF}

const
  ACTCTX_FLAG_PROCESSOR_ARCHITECTURE_VALID  = ($00000001);
  ACTCTX_FLAG_LANGID_VALID                  = ($00000002);
  ACTCTX_FLAG_ASSEMBLY_DIRECTORY_VALID      = ($00000004);
  ACTCTX_FLAG_RESOURCE_NAME_VALID           = ($00000008);
  ACTCTX_FLAG_SET_PROCESS_DEFAULT           = ($00000010);
  ACTCTX_FLAG_APPLICATION_NAME_VALID        = ($00000020);
  ACTCTX_FLAG_SOURCE_IS_ASSEMBLYREF         = ($00000040);
  ACTCTX_FLAG_HMODULE_VALID                 = ($00000080);

type
  tagACTCTXA = record
    cbSize: ULONG;
    dwFlags: DWORD;
    lpSource: LPCSTR;
    wProcessorArchitecture: USHORT;
    wLangId: LANGID;
    lpAssemblyDirectory: LPCSTR;
    lpResourceName: LPCSTR;
    lpApplicationName: LPCSTR;
    hModule: HMODULE;
  end;
  ACTCTXA = tagACTCTXA;
  PACTCTXA = ^ACTCTXA;
  TActCtxA = ACTCTXA;

  tagACTCTXW = record
    cbSize: ULONG;
    dwFlags: DWORD;
    lpSource: LPCWSTR;
    wProcessorArchitecture: USHORT;
    wLangId: LANGID;
    lpAssemblyDirectory: LPCWSTR;
    lpResourceName: LPCWSTR;
    lpApplicationName: LPCWSTR;
    hModule: HMODULE;
  end;
  ACTCTXW = tagACTCTXW;
  PACTCTXW = ^ACTCTXW;
  TActCtxW = ACTCTXW;

{$IFDEF UNICODE}
  ACTCTX = ACTCTXW;
  PACTCTX = PACTCTXW;
  TActCtx = TActCtxW;
{$ELSE}
  ACTCTX = ACTCTXA;
  PACTCTX = PACTCTXA;
  TActCtx = TActCtxA;  
{$ENDIF}

function CreateActCtxA(var pActCtx: ACTCTXA): HANDLE; stdcall;
function CreateActCtxW(var pActCtx: ACTCTXW): HANDLE; stdcall;

{$IFDEF UNICODE}
function CreateActCtx(var pActCtx: ACTCTXW): HANDLE; stdcall;
{$ELSE}
function CreateActCtx(var pActCtx: ACTCTXA): HANDLE; stdcall;
{$ENDIF}

procedure AddRefActCtx(hActCtx: HANDLE); stdcall;

procedure ReleaseActCtx(hActCtx: HANDLE); stdcall;

function ZombifyActCtx(hActCtx: HANDLE): BOOL; stdcall;

function ActivateActCtx(hActCtx: HANDLE; var lpCookie: ULONG_PTR): BOOL; stdcall;

const
  DEACTIVATE_ACTCTX_FLAG_FORCE_EARLY_DEACTIVATION = ($00000001);

function DeactivateActCtx(dwFlags: DWORD; ulCookie: ULONG_PTR): BOOL; stdcall;

function GetCurrentActCtx(var lphActCtx: HANDLE): BOOL; stdcall;

type
  tagACTCTX_SECTION_KEYED_DATA_2600 = record
    cbSize: ULONG;
    ulDataFormatVersion: ULONG;
    lpData: PVOID;
    ulLength: ULONG;
    lpSectionGlobalData: PVOID;
    ulSectionGlobalDataLength: ULONG;
    lpSectionBase: PVOID;
    ulSectionTotalLength: ULONG;
    hActCtx: HANDLE;
    ulAssemblyRosterIndex: ULONG;
  end;
  ACTCTX_SECTION_KEYED_DATA_2600 = tagACTCTX_SECTION_KEYED_DATA_2600;
  PACTCTX_SECTION_KEYED_DATA_2600 = ^ACTCTX_SECTION_KEYED_DATA_2600;
  PCACTCTX_SECTION_KEYED_DATA_2600 = ^ACTCTX_SECTION_KEYED_DATA_2600;
  TActCtxSectionKeyedData2600 = ACTCTX_SECTION_KEYED_DATA_2600;
  PActCtxSectionKeyedData2600 = PACTCTX_SECTION_KEYED_DATA_2600;

  tagACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA = record
    lpInformation: PVOID;
    lpSectionBase: PVOID;
    ulSectionLength: ULONG;
    lpSectionGlobalDataBase: PVOID;
    ulSectionGlobalDataLength: ULONG;
  end;
  ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA = tagACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
  PACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA = ^ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
  PCACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA = ^ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
  TActCtxSectionKeyedDataAssemblyMetadata = ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
  PActCtxSectionKeyedDataAssemblyMetadata = PACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
  
  tagACTCTX_SECTION_KEYED_DATA = record
    cbSize: ULONG;
    ulDataFormatVersion: ULONG;
    lpData: PVOID;
    ulLength: ULONG;
    lpSectionGlobalData: PVOID;
    ulSectionGlobalDataLength: ULONG;
    lpSectionBase: PVOID;
    ulSectionTotalLength: ULONG;
    hActCtx: HANDLE;
    ulAssemblyRosterIndex: ULONG;
    // 2600 stops here
    ulFlags: ULONG;
    AssemblyMetadata: ACTCTX_SECTION_KEYED_DATA_ASSEMBLY_METADATA;
  end;
  ACTCTX_SECTION_KEYED_DATA = tagACTCTX_SECTION_KEYED_DATA;
  PACTCTX_SECTION_KEYED_DATA = ^ACTCTX_SECTION_KEYED_DATA;
  PCACTCTX_SECTION_KEYED_DATA = ^ACTCTX_SECTION_KEYED_DATA;
  TActCtxSectionKeyedData = ACTCTX_SECTION_KEYED_DATA;
  PActCtxSectionKeyedData = PACTCTX_SECTION_KEYED_DATA;

const
  FIND_ACTCTX_SECTION_KEY_RETURN_HACTCTX = ($00000001);
  FIND_ACTCTX_SECTION_KEY_RETURN_FLAGS   = ($00000002);
  FIND_ACTCTX_SECTION_KEY_RETURN_ASSEMBLY_METADATA = ($00000004);

function FindActCtxSectionStringA(dwFlags: DWORD; const lpExtensionGuid: TGUID;
  ulSectionId: ULONG; lpStringToFind: LPCSTR; ReturnedData: PACTCTX_SECTION_KEYED_DATA): BOOL; stdcall;
function FindActCtxSectionStringW(dwFlags: DWORD; const lpExtensionGuid: TGUID;
  ulSectionId: ULONG; lpStringToFind: LPCWSTR; ReturnedData: PACTCTX_SECTION_KEYED_DATA): BOOL; stdcall;

{$IFDEF UNICODE}
function FindActCtxSectionString(dwFlags: DWORD; const lpExtensionGuid: TGUID;
  ulSectionId: ULONG; lpStringToFind: LPCWSTR; ReturnedData: PACTCTX_SECTION_KEYED_DATA): BOOL; stdcall;
{$ELSE}
function FindActCtxSectionString(dwFlags: DWORD; const lpExtensionGuid: TGUID;
  ulSectionId: ULONG; lpStringToFind: LPCSTR; ReturnedData: PACTCTX_SECTION_KEYED_DATA): BOOL; stdcall;
{$ENDIF}

function FindActCtxSectionGuid(dwFlags: DWORD; const lpExtensionGuid: TGUID;
  ulSectionId: ULONG; const lpGuidToFind: TGUID; ReturnedData: PACTCTX_SECTION_KEYED_DATA): BOOL; stdcall;

type
  _ACTIVATION_CONTEXT_BASIC_INFORMATION = record
    hActCtx: HANDLE;
    dwFlags: DWORD;
  end;
  ACTIVATION_CONTEXT_BASIC_INFORMATION = _ACTIVATION_CONTEXT_BASIC_INFORMATION;
  PACTIVATION_CONTEXT_BASIC_INFORMATION = ^ACTIVATION_CONTEXT_BASIC_INFORMATION;
  TActivationContextBasicInformation = ACTIVATION_CONTEXT_BASIC_INFORMATION;
  PActivationContextBasicInformation = PACTIVATION_CONTEXT_BASIC_INFORMATION;

  PCACTIVATION_CONTEXT_BASIC_INFORMATION = ^_ACTIVATION_CONTEXT_BASIC_INFORMATION;

const
  ACTIVATION_CONTEXT_BASIC_INFORMATION_DEFINED = 1;

  QUERY_ACTCTX_FLAG_USE_ACTIVE_ACTCTX = ($00000004);
  QUERY_ACTCTX_FLAG_ACTCTX_IS_HMODULE = ($00000008);
  QUERY_ACTCTX_FLAG_ACTCTX_IS_ADDRESS = ($00000010);
  QUERY_ACTCTX_FLAG_NO_ADDREF         = ($80000000);

//
// switch (ulInfoClass)
//
//  case ActivationContextBasicInformation:
//    pvSubInstance == NULL
//    pvBuffer is of type PACTIVATION_CONTEXT_BASIC_INFORMATION
//
//  case ActivationContextDetailedInformation:
//    pvSubInstance == NULL
//    pvBuffer is of type PACTIVATION_CONTEXT_DETAILED_INFORMATION
//
//  case AssemblyDetailedInformationInActivationContext:
//    pvSubInstance is of type PULONG
//      *pvSubInstance < ACTIVATION_CONTEXT_DETAILED_INFORMATION::ulAssemblyCount
//    pvBuffer is of type PACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION
//
//  case FileInformationInAssemblyOfAssemblyInActivationContext:
//    pvSubInstance is of type PACTIVATION_CONTEXT_QUERY_INDEX
//      pvSubInstance->ulAssemblyIndex < ACTIVATION_CONTEXT_DETAILED_INFORMATION::ulAssemblyCount
//      pvSubInstance->ulFileIndexInAssembly < ACTIVATION_CONTEXT_ASSEMBLY_DETAILED_INFORMATION::ulFileCount
//    pvBuffer is of type PASSEMBLY_FILE_DETAILED_INFORMATION
//
// String are placed after the structs.
//
  
function QueryActCtxW(dwFlags: DWORD; hActCtx: HANDLE; pvSubInstance: PVOID;
  ulInfoClass: ULONG; pvBuffer: PVOID; cbBuffer: SIZE_T;
  pcbWrittenOrRequired: PSIZE_T): BOOL; stdcall;

type
  PQUERYACTCTXW_FUNC = function (dwFlags: DWORD; hActCtx: HANDLE;
    pvSubInstance: PVOID; ulInfoClass: ULONG; pvBuffer: PVOID; cbBuffer: SIZE_T;
    pcbWrittenOrRequired: PSIZE_T): BOOL; stdcall;

function ProcessIdToSessionId(dwProcessId: DWORD; var pSessionId: DWORD): BOOL; stdcall;

function WTSGetActiveConsoleSessionId: DWORD; stdcall;

function IsWow64Process(hProcess: HANDLE; var Wow64Process: BOOL): BOOL; stdcall;

function GetLogicalProcessorInformation(Buffer: PSYSTEM_LOGICAL_PROCESSOR_INFORMATION; ReturnedLength: PDWORD): BOOL; stdcall;

//
// NUMA Information routines.
//

function GetNumaHighestNodeNumber(var HighestNodeNumber: ULONG): BOOL; stdcall;

function GetNumaProcessorNode(Processor: UCHAR; var NodeNumber: UCHAR): BOOL; stdcall;

function GetNumaNodeProcessorMask(Node: UCHAR; ProcessorMask: ULONGLONG): BOOL; stdcall;

function GetNumaAvailableMemoryNode(Node: UCHAR; var AvailableBytes: ULONGLONG): BOOL; stdcall;

implementation

const
  kernel32 = 'kernel32.dll';
  advapi32 = 'advapi32.dll';

procedure MoveMemory(Destination, Source: PVOID; Length: SIZE_T);
begin
  Move(Source^, Destination^, Length);
end;

procedure CopyMemory(Destination, Source: PVOID; Length: SIZE_T);
begin
  Move(Source^, Destination^, Length);
end;

procedure FillMemory(Destination: PVOID; Length: SIZE_T; Fill: BYTE);
begin
  FillChar(Destination^, Length, Fill);
end;

procedure ZeroMemory(Destination: PVOID; Length: SIZE_T);
begin
  FillChar(Destination^, Length, 0);
end;

function FreeModule(hLibModule: HMODULE): BOOL;
begin
  Result := FreeLibrary(hLibModule);
end;

function MakeProcInstance(lpProc: FARPROC; hInstance: HINSTANCE): FARPROC;
begin
  Result := lpProc;
end;

procedure FreeProcInstance(lpProc: FARPROC);
begin
  { nothing }
end;

function GlobalLRUNewest(h: HANDLE): HANDLE;
begin
  Result := H;
end;

function GlobalLRUOldest(h: HANDLE): HANDLE;
begin
  Result := H;
end;

function GlobalDiscard(h: HANDLE): HANDLE;
begin
 Result := GlobalReAlloc(h, 0, GMEM_MOVEABLE);
end;

function LocalDiscard(h: HLOCAL): HLOCAL;
begin
  Result := LocalReAlloc(h, 0, LMEM_MOVEABLE);
end;

function GetFreeSpace(w: WORD): DWORD;
begin
  Result := $100000;
end;

function InterlockedExchangePointer(var Target: PVOID; Value: PVOID): PVOID;
begin
  Result := PVOID(InterlockedExchange(LONG(Target), LONG(Value)));
end;

function InterlockedCompareExchangePointer(var Destination: PVOID; Exchange, Comperand: PVOID): PVOID;
begin
  Result := PVOID(InterlockedCompareExchange(LONG(Destination), LONG(Exchange), LONG(Comperand)));
end;

function UnlockResource(hResData: HANDLE): BOOL;
begin
  Result := False;
end;

function HasOverlappedIoCompleted(const lpOverlapped: OVERLAPPED): BOOL;
begin
  Result := DWORD(lpOverlapped.Internal) <> STATUS_PENDING;
end;


{$IFDEF DYNAMIC_LINK}
var
  _InterlockedIncrement: Pointer;

function InterlockedIncrement;
begin
  GetProcedureAddress(_InterlockedIncrement, kernel32, 'InterlockedIncrement');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedIncrement]
  end;
end;
{$ELSE}
function InterlockedIncrement; external kernel32 name 'InterlockedIncrement';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedDecrement: Pointer;

function InterlockedDecrement;
begin
  GetProcedureAddress(_InterlockedDecrement, kernel32, 'InterlockedDecrement');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedDecrement]
  end;
end;
{$ELSE}
function InterlockedDecrement; external kernel32 name 'InterlockedDecrement';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedExchange: Pointer;

function InterlockedExchange;
begin
  GetProcedureAddress(_InterlockedExchange, kernel32, 'InterlockedExchange');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedExchange]
  end;
end;
{$ELSE}
function InterlockedExchange; external kernel32 name 'InterlockedExchange';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedExchangeAdd: Pointer;

function InterlockedExchangeAdd;
begin
  GetProcedureAddress(_InterlockedExchangeAdd, kernel32, 'InterlockedExchangeAdd');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedExchangeAdd]
  end;
end;
{$ELSE}
function InterlockedExchangeAdd; external kernel32 name 'InterlockedExchangeAdd';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedCompareExchange: Pointer;

function InterlockedCompareExchange;
begin
  GetProcedureAddress(_InterlockedCompareExchange, kernel32, 'InterlockedCompareExchange');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedCompareExchange]
  end;
end;
{$ELSE}
function InterlockedCompareExchange; external kernel32 name 'InterlockedCompareExchange';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitializeSListHead: Pointer;

procedure InitializeSListHead;
begin
  GetProcedureAddress(_InitializeSListHead, kernel32, 'InitializeSListHead');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitializeSListHead]
  end;
end;
{$ELSE}
procedure InitializeSListHead; external kernel32 name 'InitializeSListHead';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedPopEntrySList: Pointer;

function InterlockedPopEntrySList;
begin
  GetProcedureAddress(_InterlockedPopEntrySList, kernel32, 'InterlockedPopEntrySList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedPopEntrySList]
  end;
end;
{$ELSE}
function InterlockedPopEntrySList; external kernel32 name 'InterlockedPopEntrySList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedPushEntrySList: Pointer;

function InterlockedPushEntrySList;
begin
  GetProcedureAddress(_InterlockedPushEntrySList, kernel32, 'InterlockedPushEntrySList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedPushEntrySList]
  end;
end;
{$ELSE}
function InterlockedPushEntrySList; external kernel32 name 'InterlockedPushEntrySList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InterlockedFlushSList: Pointer;

function InterlockedFlushSList;
begin
  GetProcedureAddress(_InterlockedFlushSList, kernel32, 'InterlockedFlushSList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InterlockedFlushSList]
  end;
end;
{$ELSE}
function InterlockedFlushSList; external kernel32 name 'InterlockedFlushSList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueryDepthSList: Pointer;

function QueryDepthSList;
begin
  GetProcedureAddress(_QueryDepthSList, kernel32, 'QueryDepthSList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryDepthSList]
  end;
end;
{$ELSE}
function QueryDepthSList; external kernel32 name 'QueryDepthSList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeResource: Pointer;

function FreeResource;
begin
  GetProcedureAddress(_FreeResource, kernel32, 'FreeResource');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeResource]
  end;
end;
{$ELSE}
function FreeResource; external kernel32 name 'FreeResource';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LockResource: Pointer;

function LockResource;
begin
  GetProcedureAddress(_LockResource, kernel32, 'LockResource');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LockResource]
  end;
end;
{$ELSE}
function LockResource; external kernel32 name 'LockResource';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeLibrary: Pointer;

function FreeLibrary;
begin
  GetProcedureAddress(_FreeLibrary, kernel32, 'FreeLibrary');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeLibrary]
  end;
end;
{$ELSE}
function FreeLibrary; external kernel32 name 'FreeLibrary';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeLibraryAndExitThread: Pointer;

procedure FreeLibraryAndExitThread;
begin
  GetProcedureAddress(_FreeLibraryAndExitThread, kernel32, 'FreeLibraryAndExitThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeLibraryAndExitThread]
  end;
end;
{$ELSE}
procedure FreeLibraryAndExitThread; external kernel32 name 'FreeLibraryAndExitThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DisableThreadLibraryCalls: Pointer;

function DisableThreadLibraryCalls;
begin
  GetProcedureAddress(_DisableThreadLibraryCalls, kernel32, 'DisableThreadLibraryCalls');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DisableThreadLibraryCalls]
  end;
end;
{$ELSE}
function DisableThreadLibraryCalls; external kernel32 name 'DisableThreadLibraryCalls';
{$ENDIF DYNAMIC_LINK}

// MVB TODO Dynamic linking for GetProcAddress doesn't make much sense, does it? Same for LoadLibrary.

{$IFDEF DYNAMIC_LINK}
var
  _GetProcAddress: Pointer;

function GetProcAddress;
begin
  GetProcedureAddress(_GetProcAddress, kernel32, 'GetProcAddress');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcAddress]
  end;
end;
{$ELSE}
function GetProcAddress; external kernel32 name 'GetProcAddress';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVersion: Pointer;

function GetVersion;
begin
  GetProcedureAddress(_GetVersion, kernel32, 'GetVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVersion]
  end;
end;
{$ELSE}
function GetVersion; external kernel32 name 'GetVersion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalAlloc: Pointer;

function GlobalAlloc;
begin
  GetProcedureAddress(_GlobalAlloc, kernel32, 'GlobalAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalAlloc]
  end;
end;
{$ELSE}
function GlobalAlloc; external kernel32 name 'GlobalAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalReAlloc: Pointer;

function GlobalReAlloc;
begin
  GetProcedureAddress(_GlobalReAlloc, kernel32, 'GlobalReAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalReAlloc]
  end;
end;
{$ELSE}
function GlobalReAlloc; external kernel32 name 'GlobalReAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalSize: Pointer;

function GlobalSize;
begin
  GetProcedureAddress(_GlobalSize, kernel32, 'GlobalSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalSize]
  end;
end;
{$ELSE}
function GlobalSize; external kernel32 name 'GlobalSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFlags: Pointer;

function GlobalFlags;
begin
  GetProcedureAddress(_GlobalFlags, kernel32, 'GlobalFlags');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFlags]
  end;
end;
{$ELSE}
function GlobalFlags; external kernel32 name 'GlobalFlags';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalLock: Pointer;

function GlobalLock;
begin
  GetProcedureAddress(_GlobalLock, kernel32, 'GlobalLock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalLock]
  end;
end;
{$ELSE}
function GlobalLock; external kernel32 name 'GlobalLock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalHandle: Pointer;

function GlobalHandle;
begin
  GetProcedureAddress(_GlobalHandle, kernel32, 'GlobalHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalHandle]
  end;
end;
{$ELSE}
function GlobalHandle; external kernel32 name 'GlobalHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalUnlock: Pointer;

function GlobalUnlock;
begin
  GetProcedureAddress(_GlobalUnlock, kernel32, 'GlobalUnlock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalUnlock]
  end;
end;
{$ELSE}
function GlobalUnlock; external kernel32 name 'GlobalUnlock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFree: Pointer;

function GlobalFree;
begin
  GetProcedureAddress(_GlobalFree, kernel32, 'GlobalFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFree]
  end;
end;
{$ELSE}
function GlobalFree; external kernel32 name 'GlobalFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalCompact: Pointer;

function GlobalCompact;
begin
  GetProcedureAddress(_GlobalCompact, kernel32, 'GlobalCompact');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalCompact]
  end;
end;
{$ELSE}
function GlobalCompact; external kernel32 name 'GlobalCompact';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFix: Pointer;

procedure GlobalFix;
begin
  GetProcedureAddress(_GlobalFix, kernel32, 'GlobalFix');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFix]
  end;
end;
{$ELSE}
procedure GlobalFix; external kernel32 name 'GlobalFix';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalUnfix: Pointer;

procedure GlobalUnfix;
begin
  GetProcedureAddress(_GlobalUnfix, kernel32, 'GlobalUnfix');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalUnfix]
  end;
end;
{$ELSE}
procedure GlobalUnfix; external kernel32 name 'GlobalUnfix';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalWire: Pointer;

function GlobalWire;
begin
  GetProcedureAddress(_GlobalWire, kernel32, 'GlobalWire');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalWire]
  end;
end;
{$ELSE}
function GlobalWire; external kernel32 name 'GlobalWire';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalUnWire: Pointer;

function GlobalUnWire;
begin
  GetProcedureAddress(_GlobalUnWire, kernel32, 'GlobalUnWire');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalUnWire]
  end;
end;
{$ELSE}
function GlobalUnWire; external kernel32 name 'GlobalUnWire';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalMemoryStatus: Pointer;

procedure GlobalMemoryStatus;
begin
  GetProcedureAddress(_GlobalMemoryStatus, kernel32, 'GlobalMemoryStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalMemoryStatus]
  end;
end;
{$ELSE}
procedure GlobalMemoryStatus; external kernel32 name 'GlobalMemoryStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalMemoryStatusEx: Pointer;

function GlobalMemoryStatusEx;
begin
  GetProcedureAddress(_GlobalMemoryStatusEx, kernel32, 'GlobalMemoryStatusEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalMemoryStatusEx]
  end;
end;
{$ELSE}
function GlobalMemoryStatusEx; external kernel32 name 'GlobalMemoryStatusEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalAlloc: Pointer;

function LocalAlloc;
begin
  GetProcedureAddress(_LocalAlloc, kernel32, 'LocalAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalAlloc]
  end;
end;
{$ELSE}
function LocalAlloc; external kernel32 name 'LocalAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalReAlloc: Pointer;

function LocalReAlloc;
begin
  GetProcedureAddress(_LocalReAlloc, kernel32, 'LocalReAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalReAlloc]
  end;
end;
{$ELSE}
function LocalReAlloc; external kernel32 name 'LocalReAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalLock: Pointer;

function LocalLock;
begin
  GetProcedureAddress(_LocalLock, kernel32, 'LocalLock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalLock]
  end;
end;
{$ELSE}
function LocalLock; external kernel32 name 'LocalLock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalHandle: Pointer;

function LocalHandle;
begin
  GetProcedureAddress(_LocalHandle, kernel32, 'LocalHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalHandle]
  end;
end;
{$ELSE}
function LocalHandle; external kernel32 name 'LocalHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalUnlock: Pointer;

function LocalUnlock;
begin
  GetProcedureAddress(_LocalUnlock, kernel32, 'LocalUnlock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalUnlock]
  end;
end;
{$ELSE}
function LocalUnlock; external kernel32 name 'LocalUnlock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalSize: Pointer;

function LocalSize;
begin
  GetProcedureAddress(_LocalSize, kernel32, 'LocalSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalSize]
  end;
end;
{$ELSE}
function LocalSize; external kernel32 name 'LocalSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalFlags: Pointer;

function LocalFlags;
begin
  GetProcedureAddress(_LocalFlags, kernel32, 'LocalFlags');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalFlags]
  end;
end;
{$ELSE}
function LocalFlags; external kernel32 name 'LocalFlags';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalFree: Pointer;

function LocalFree;
begin
  GetProcedureAddress(_LocalFree, kernel32, 'LocalFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalFree]
  end;
end;
{$ELSE}
function LocalFree; external kernel32 name 'LocalFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalShrink: Pointer;

function LocalShrink;
begin
  GetProcedureAddress(_LocalShrink, kernel32, 'LocalShrink');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalShrink]
  end;
end;
{$ELSE}
function LocalShrink; external kernel32 name 'LocalShrink';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalCompact: Pointer;

function LocalCompact;
begin
  GetProcedureAddress(_LocalCompact, kernel32, 'LocalCompact');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalCompact]
  end;
end;
{$ELSE}
function LocalCompact; external kernel32 name 'LocalCompact';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlushInstructionCache: Pointer;

function FlushInstructionCache;
begin
  GetProcedureAddress(_FlushInstructionCache, kernel32, 'FlushInstructionCache');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlushInstructionCache]
  end;
end;
{$ELSE}
function FlushInstructionCache; external kernel32 name 'FlushInstructionCache';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualAlloc: Pointer;

function VirtualAlloc;
begin
  GetProcedureAddress(_VirtualAlloc, kernel32, 'VirtualAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualAlloc]
  end;
end;
{$ELSE}
function VirtualAlloc; external kernel32 name 'VirtualAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualFree: Pointer;

function VirtualFree;
begin
  GetProcedureAddress(_VirtualFree, kernel32, 'VirtualFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualFree]
  end;
end;
{$ELSE}
function VirtualFree; external kernel32 name 'VirtualFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualProtect: Pointer;

function VirtualProtect;
begin
  GetProcedureAddress(_VirtualProtect, kernel32, 'VirtualProtect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualProtect]
  end;
end;
{$ELSE}
function VirtualProtect; external kernel32 name 'VirtualProtect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualQuery: Pointer;

function VirtualQuery;
begin
  GetProcedureAddress(_VirtualQuery, kernel32, 'VirtualQuery');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualQuery]
  end;
end;
{$ELSE}
function VirtualQuery; external kernel32 name 'VirtualQuery';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualAllocEx: Pointer;

function VirtualAllocEx;
begin
  GetProcedureAddress(_VirtualAllocEx, kernel32, 'VirtualAllocEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualAllocEx]
  end;
end;
{$ELSE}
function VirtualAllocEx; external kernel32 name 'VirtualAllocEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWriteWatch: Pointer;

function GetWriteWatch;
begin
  GetProcedureAddress(_GetWriteWatch, kernel32, 'GetWriteWatch');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWriteWatch]
  end;
end;
{$ELSE}
function GetWriteWatch; external kernel32 name 'GetWriteWatch';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ResetWriteWatch: Pointer;

function ResetWriteWatch;
begin
  GetProcedureAddress(_ResetWriteWatch, kernel32, 'ResetWriteWatch');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResetWriteWatch]
  end;
end;
{$ELSE}
function ResetWriteWatch; external kernel32 name 'ResetWriteWatch';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLargePageMinimum: Pointer;

function GetLargePageMinimum;
begin
  GetProcedureAddress(_GetLargePageMinimum, kernel32, 'GetLargePageMinimum');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLargePageMinimum]
  end;
end;
{$ELSE}
function GetLargePageMinimum; external kernel32 name 'GetLargePageMinimum';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualFreeEx: Pointer;

function VirtualFreeEx;
begin
  GetProcedureAddress(_VirtualFreeEx, kernel32, 'VirtualFreeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualFreeEx]
  end;
end;
{$ELSE}
function VirtualFreeEx; external kernel32 name 'VirtualFreeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualProtectEx: Pointer;

function VirtualProtectEx;
begin
  GetProcedureAddress(_VirtualProtectEx, kernel32, 'VirtualProtectEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualProtectEx]
  end;
end;
{$ELSE}
function VirtualProtectEx; external kernel32 name 'VirtualProtectEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualQueryEx: Pointer;

function VirtualQueryEx;
begin
  GetProcedureAddress(_VirtualQueryEx, kernel32, 'VirtualQueryEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualQueryEx]
  end;
end;
{$ELSE}
function VirtualQueryEx; external kernel32 name 'VirtualQueryEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapCreate: Pointer;

function HeapCreate;
begin
  GetProcedureAddress(_HeapCreate, kernel32, 'HeapCreate');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapCreate]
  end;
end;
{$ELSE}
function HeapCreate; external kernel32 name 'HeapCreate';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapDestroy: Pointer;

function HeapDestroy;
begin
  GetProcedureAddress(_HeapDestroy, kernel32, 'HeapDestroy');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapDestroy]
  end;
end;
{$ELSE}
function HeapDestroy; external kernel32 name 'HeapDestroy';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapAlloc: Pointer;

function HeapAlloc;
begin
  GetProcedureAddress(_HeapAlloc, kernel32, 'HeapAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapAlloc]
  end;
end;
{$ELSE}
function HeapAlloc; external kernel32 name 'HeapAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapReAlloc: Pointer;

function HeapReAlloc;
begin
  GetProcedureAddress(_HeapReAlloc, kernel32, 'HeapReAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapReAlloc]
  end;
end;
{$ELSE}
function HeapReAlloc; external kernel32 name 'HeapReAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapFree: Pointer;

function HeapFree;
begin
  GetProcedureAddress(_HeapFree, kernel32, 'HeapFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapFree]
  end;
end;
{$ELSE}
function HeapFree; external kernel32 name 'HeapFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapSize: Pointer;

function HeapSize;
begin
  GetProcedureAddress(_HeapSize, kernel32, 'HeapSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapSize]
  end;
end;
{$ELSE}
function HeapSize; external kernel32 name 'HeapSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapValidate: Pointer;

function HeapValidate;
begin
  GetProcedureAddress(_HeapValidate, kernel32, 'HeapValidate');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapValidate]
  end;
end;
{$ELSE}
function HeapValidate; external kernel32 name 'HeapValidate';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapCompact: Pointer;

function HeapCompact;
begin
  GetProcedureAddress(_HeapCompact, kernel32, 'HeapCompact');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapCompact]
  end;
end;
{$ELSE}
function HeapCompact; external kernel32 name 'HeapCompact';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessHeap: Pointer;

function GetProcessHeap;
begin
  GetProcedureAddress(_GetProcessHeap, kernel32, 'GetProcessHeap');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessHeap]
  end;
end;
{$ELSE}
function GetProcessHeap; external kernel32 name 'GetProcessHeap';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessHeaps: Pointer;

function GetProcessHeaps;
begin
  GetProcedureAddress(_GetProcessHeaps, kernel32, 'GetProcessHeaps');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessHeaps]
  end;
end;
{$ELSE}
function GetProcessHeaps; external kernel32 name 'GetProcessHeaps';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapLock: Pointer;

function HeapLock;
begin
  GetProcedureAddress(_HeapLock, kernel32, 'HeapLock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapLock]
  end;
end;
{$ELSE}
function HeapLock; external kernel32 name 'HeapLock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapUnlock: Pointer;

function HeapUnlock;
begin
  GetProcedureAddress(_HeapUnlock, kernel32, 'HeapUnlock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapUnlock]
  end;
end;
{$ELSE}
function HeapUnlock; external kernel32 name 'HeapUnlock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapWalk: Pointer;

function HeapWalk;
begin
  GetProcedureAddress(_HeapWalk, kernel32, 'HeapWalk');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapWalk]
  end;
end;
{$ELSE}
function HeapWalk; external kernel32 name 'HeapWalk';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapSetInformation: Pointer;

function HeapSetInformation;
begin
  GetProcedureAddress(_HeapSetInformation, kernel32, 'HeapSetInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapSetInformation]
  end;
end;
{$ELSE}
function HeapSetInformation; external kernel32 name 'HeapSetInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HeapQueryInformation: Pointer;

function HeapQueryInformation;
begin
  GetProcedureAddress(_HeapQueryInformation, kernel32, 'HeapQueryInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HeapQueryInformation]
  end;
end;
{$ELSE}
function HeapQueryInformation; external kernel32 name 'HeapQueryInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBinaryTypeA: Pointer;

function GetBinaryTypeA;
begin
  GetProcedureAddress(_GetBinaryTypeA, kernel32, 'GetBinaryTypeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBinaryTypeA]
  end;
end;
{$ELSE}
function GetBinaryTypeA; external kernel32 name 'GetBinaryTypeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetBinaryTypeW: Pointer;

function GetBinaryTypeW;
begin
  GetProcedureAddress(_GetBinaryTypeW, kernel32, 'GetBinaryTypeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBinaryTypeW]
  end;
end;
{$ELSE}
function GetBinaryTypeW; external kernel32 name 'GetBinaryTypeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetBinaryType: Pointer;

function GetBinaryType;
begin
  GetProcedureAddress(_GetBinaryType, kernel32, 'GetBinaryTypeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBinaryType]
  end;
end;
{$ELSE}
function GetBinaryType; external kernel32 name 'GetBinaryTypeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetBinaryType: Pointer;

function GetBinaryType;
begin
  GetProcedureAddress(_GetBinaryType, kernel32, 'GetBinaryTypeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetBinaryType]
  end;
end;
{$ELSE}
function GetBinaryType; external kernel32 name 'GetBinaryTypeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetShortPathNameA: Pointer;

function GetShortPathNameA;
begin
  GetProcedureAddress(_GetShortPathNameA, kernel32, 'GetShortPathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetShortPathNameA]
  end;
end;
{$ELSE}
function GetShortPathNameA; external kernel32 name 'GetShortPathNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetShortPathNameW: Pointer;

function GetShortPathNameW;
begin
  GetProcedureAddress(_GetShortPathNameW, kernel32, 'GetShortPathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetShortPathNameW]
  end;
end;
{$ELSE}
function GetShortPathNameW; external kernel32 name 'GetShortPathNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetShortPathName: Pointer;

function GetShortPathName;
begin
  GetProcedureAddress(_GetShortPathName, kernel32, 'GetShortPathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetShortPathName]
  end;
end;
{$ELSE}
function GetShortPathName; external kernel32 name 'GetShortPathNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetShortPathName: Pointer;

function GetShortPathName;
begin
  GetProcedureAddress(_GetShortPathName, kernel32, 'GetShortPathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetShortPathName]
  end;
end;
{$ELSE}
function GetShortPathName; external kernel32 name 'GetShortPathNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetLongPathNameA: Pointer;

function GetLongPathNameA;
begin
  GetProcedureAddress(_GetLongPathNameA, kernel32, 'GetLongPathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLongPathNameA]
  end;
end;
{$ELSE}
function GetLongPathNameA; external kernel32 name 'GetLongPathNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLongPathNameW: Pointer;

function GetLongPathNameW;
begin
  GetProcedureAddress(_GetLongPathNameW, kernel32, 'GetLongPathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLongPathNameW]
  end;
end;
{$ELSE}
function GetLongPathNameW; external kernel32 name 'GetLongPathNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLongPathName: Pointer;

function GetLongPathName;
begin
  GetProcedureAddress(_GetLongPathName, kernel32, 'GetLongPathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLongPathName]
  end;
end;
{$ELSE}
function GetLongPathName; external kernel32 name 'GetLongPathNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLongPathName: Pointer;

function GetLongPathName;
begin
  GetProcedureAddress(_GetLongPathName, kernel32, 'GetLongPathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLongPathName]
  end;
end;
{$ELSE}
function GetLongPathName; external kernel32 name 'GetLongPathNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessAffinityMask: Pointer;

function GetProcessAffinityMask;
begin
  GetProcedureAddress(_GetProcessAffinityMask, kernel32, 'GetProcessAffinityMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessAffinityMask]
  end;
end;
{$ELSE}
function GetProcessAffinityMask; external kernel32 name 'GetProcessAffinityMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetProcessAffinityMask: Pointer;

function SetProcessAffinityMask;
begin
  GetProcedureAddress(_SetProcessAffinityMask, kernel32, 'SetProcessAffinityMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessAffinityMask]
  end;
end;
{$ELSE}
function SetProcessAffinityMask; external kernel32 name 'SetProcessAffinityMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessHandleCount: Pointer;

function GetProcessHandleCount;
begin
  GetProcedureAddress(_GetProcessHandleCount, kernel32, 'GetProcessHandleCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessHandleCount]
  end;
end;
{$ELSE}
function GetProcessHandleCount; external kernel32 name 'GetProcessHandleCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessTimes: Pointer;

function GetProcessTimes;
begin
  GetProcedureAddress(_GetProcessTimes, kernel32, 'GetProcessTimes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessTimes]
  end;
end;
{$ELSE}
function GetProcessTimes; external kernel32 name 'GetProcessTimes';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessIoCounters: Pointer;

function GetProcessIoCounters;
begin
  GetProcedureAddress(_GetProcessIoCounters, kernel32, 'GetProcessIoCounters');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessIoCounters]
  end;
end;
{$ELSE}
function GetProcessIoCounters; external kernel32 name 'GetProcessIoCounters';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessWorkingSetSize: Pointer;

function GetProcessWorkingSetSize;
begin
  GetProcedureAddress(_GetProcessWorkingSetSize, kernel32, 'GetProcessWorkingSetSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessWorkingSetSize]
  end;
end;
{$ELSE}
function GetProcessWorkingSetSize; external kernel32 name 'GetProcessWorkingSetSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessWorkingSetSizeEx: Pointer;

function GetProcessWorkingSetSizeEx;
begin
  GetProcedureAddress(_GetProcessWorkingSetSizeEx, kernel32, 'GetProcessWorkingSetSizeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessWorkingSetSizeEx]
  end;
end;
{$ELSE}
function GetProcessWorkingSetSizeEx; external kernel32 name 'GetProcessWorkingSetSizeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetProcessWorkingSetSize: Pointer;

function SetProcessWorkingSetSize;
begin
  GetProcedureAddress(_SetProcessWorkingSetSize, kernel32, 'SetProcessWorkingSetSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessWorkingSetSize]
  end;
end;
{$ELSE}
function SetProcessWorkingSetSize; external kernel32 name 'SetProcessWorkingSetSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetProcessWorkingSetSizeEx: Pointer;

function SetProcessWorkingSetSizeEx;
begin
  GetProcedureAddress(_SetProcessWorkingSetSizeEx, kernel32, 'SetProcessWorkingSetSizeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessWorkingSetSizeEx]
  end;
end;
{$ELSE}
function SetProcessWorkingSetSizeEx; external kernel32 name 'SetProcessWorkingSetSizeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenProcess: Pointer;

function OpenProcess;
begin
  GetProcedureAddress(_OpenProcess, kernel32, 'OpenProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenProcess]
  end;
end;
{$ELSE}
function OpenProcess; external kernel32 name 'OpenProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentProcess: Pointer;

function GetCurrentProcess;
begin
  GetProcedureAddress(_GetCurrentProcess, kernel32, 'GetCurrentProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentProcess]
  end;
end;
{$ELSE}
function GetCurrentProcess; external kernel32 name 'GetCurrentProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentProcessId: Pointer;

function GetCurrentProcessId;
begin
  GetProcedureAddress(_GetCurrentProcessId, kernel32, 'GetCurrentProcessId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentProcessId]
  end;
end;
{$ELSE}
function GetCurrentProcessId; external kernel32 name 'GetCurrentProcessId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExitProcess: Pointer;

procedure ExitProcess;
begin
  GetProcedureAddress(_ExitProcess, kernel32, 'ExitProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExitProcess]
  end;
end;
{$ELSE}
procedure ExitProcess; external kernel32 name 'ExitProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TerminateProcess: Pointer;

function TerminateProcess;
begin
  GetProcedureAddress(_TerminateProcess, kernel32, 'TerminateProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TerminateProcess]
  end;
end;
{$ELSE}
function TerminateProcess; external kernel32 name 'TerminateProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetExitCodeProcess: Pointer;

function GetExitCodeProcess;
begin
  GetProcedureAddress(_GetExitCodeProcess, kernel32, 'GetExitCodeProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetExitCodeProcess]
  end;
end;
{$ELSE}
function GetExitCodeProcess; external kernel32 name 'GetExitCodeProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FatalExit: Pointer;

procedure FatalExit;
begin
  GetProcedureAddress(_FatalExit, kernel32, 'FatalExit');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FatalExit]
  end;
end;
{$ELSE}
procedure FatalExit; external kernel32 name 'FatalExit';
{$ENDIF DYNAMIC_LINK}
{$IFNDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentStrings: Pointer;

function GetEnvironmentStrings;
begin
  GetProcedureAddress(_GetEnvironmentStrings, kernel32, 'GetEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentStrings]
  end;
end;
{$ELSE}
function GetEnvironmentStrings; external kernel32 name 'GetEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentStringsW: Pointer;

function GetEnvironmentStringsW;
begin
  GetProcedureAddress(_GetEnvironmentStringsW, kernel32, 'GetEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentStringsW]
  end;
end;
{$ELSE}
function GetEnvironmentStringsW; external kernel32 name 'GetEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentStrings: Pointer;

function GetEnvironmentStrings;
begin
  GetProcedureAddress(_GetEnvironmentStrings, kernel32, 'GetEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentStrings]
  end;
end;
{$ELSE}
function GetEnvironmentStrings; external kernel32 name 'GetEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentStringsA: Pointer;

function GetEnvironmentStringsA;
begin
  GetProcedureAddress(_GetEnvironmentStringsA, kernel32, 'GetEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentStringsA]
  end;
end;
{$ELSE}
function GetEnvironmentStringsA; external kernel32 name 'GetEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentStringsA: Pointer;

function SetEnvironmentStringsA;
begin
  GetProcedureAddress(_SetEnvironmentStringsA, kernel32, 'SetEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentStringsA]
  end;
end;
{$ELSE}
function SetEnvironmentStringsA; external kernel32 name 'SetEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentStringsW: Pointer;

function SetEnvironmentStringsW;
begin
  GetProcedureAddress(_SetEnvironmentStringsW, kernel32, 'SetEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentStringsW]
  end;
end;
{$ELSE}
function SetEnvironmentStringsW; external kernel32 name 'SetEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}
{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentStrings: Pointer;

function SetEnvironmentStrings;
begin
  GetProcedureAddress(_SetEnvironmentStrings, kernel32, 'SetEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentStrings]
  end;
end;
{$ELSE}
function SetEnvironmentStrings; external kernel32 name 'SetEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}

{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentStrings: Pointer;

function SetEnvironmentStrings;
begin
  GetProcedureAddress(_SetEnvironmentStrings, kernel32, 'SetEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentStrings]
  end;
end;
{$ELSE}
function SetEnvironmentStrings; external kernel32 name 'SetEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FreeEnvironmentStringsA: Pointer;

function FreeEnvironmentStringsA;
begin
  GetProcedureAddress(_FreeEnvironmentStringsA, kernel32, 'FreeEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeEnvironmentStringsA]
  end;
end;
{$ELSE}
function FreeEnvironmentStringsA; external kernel32 name 'FreeEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeEnvironmentStringsW: Pointer;

function FreeEnvironmentStringsW;
begin
  GetProcedureAddress(_FreeEnvironmentStringsW, kernel32, 'FreeEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeEnvironmentStringsW]
  end;
end;
{$ELSE}
function FreeEnvironmentStringsW; external kernel32 name 'FreeEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FreeEnvironmentStrings: Pointer;

function FreeEnvironmentStrings;
begin
  GetProcedureAddress(_FreeEnvironmentStrings, kernel32, 'FreeEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeEnvironmentStrings]
  end;
end;
{$ELSE}
function FreeEnvironmentStrings; external kernel32 name 'FreeEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FreeEnvironmentStrings: Pointer;

function FreeEnvironmentStrings;
begin
  GetProcedureAddress(_FreeEnvironmentStrings, kernel32, 'FreeEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeEnvironmentStrings]
  end;
end;
{$ELSE}
function FreeEnvironmentStrings; external kernel32 name 'FreeEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RaiseException: Pointer;

procedure RaiseException;
begin
  GetProcedureAddress(_RaiseException, kernel32, 'RaiseException');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RaiseException]
  end;
end;
{$ELSE}
procedure RaiseException; external kernel32 name 'RaiseException';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnhandledExceptionFilter: Pointer;

function UnhandledExceptionFilter;
begin
  GetProcedureAddress(_UnhandledExceptionFilter, kernel32, 'UnhandledExceptionFilter');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnhandledExceptionFilter]
  end;
end;
{$ELSE}
function UnhandledExceptionFilter; external kernel32 name 'UnhandledExceptionFilter';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetUnhandledExceptionFilter: Pointer;

function SetUnhandledExceptionFilter;
begin
  GetProcedureAddress(_SetUnhandledExceptionFilter, kernel32, 'SetUnhandledExceptionFilter');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUnhandledExceptionFilter]
  end;
end;
{$ELSE}
function SetUnhandledExceptionFilter; external kernel32 name 'SetUnhandledExceptionFilter';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFiber: Pointer;

function CreateFiber;
begin
  GetProcedureAddress(_CreateFiber, kernel32, 'CreateFiber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFiber]
  end;
end;
{$ELSE}
function CreateFiber; external kernel32 name 'CreateFiber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFiberEx: Pointer;

function CreateFiberEx;
begin
  GetProcedureAddress(_CreateFiberEx, kernel32, 'CreateFiberEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFiberEx]
  end;
end;
{$ELSE}
function CreateFiberEx; external kernel32 name 'CreateFiberEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteFiber: Pointer;

procedure DeleteFiber;
begin
  GetProcedureAddress(_DeleteFiber, kernel32, 'DeleteFiber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteFiber]
  end;
end;
{$ELSE}
procedure DeleteFiber; external kernel32 name 'DeleteFiber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertThreadToFiber: Pointer;

function ConvertThreadToFiber;
begin
  GetProcedureAddress(_ConvertThreadToFiber, kernel32, 'ConvertThreadToFiber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertThreadToFiber]
  end;
end;
{$ELSE}
function ConvertThreadToFiber; external kernel32 name 'ConvertThreadToFiber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertThreadToFiberEx: Pointer;

function ConvertThreadToFiberEx;
begin
  GetProcedureAddress(_ConvertThreadToFiberEx, kernel32, 'ConvertThreadToFiberEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertThreadToFiberEx]
  end;
end;
{$ELSE}
function ConvertThreadToFiberEx; external kernel32 name 'ConvertThreadToFiberEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertFiberToThread: Pointer;

function ConvertFiberToThread;
begin
  GetProcedureAddress(_ConvertFiberToThread, kernel32, 'ConvertFiberToThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertFiberToThread]
  end;
end;
{$ELSE}
function ConvertFiberToThread; external kernel32 name 'ConvertFiberToThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SwitchToFiber: Pointer;

procedure SwitchToFiber;
begin
  GetProcedureAddress(_SwitchToFiber, kernel32, 'SwitchToFiber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SwitchToFiber]
  end;
end;
{$ELSE}
procedure SwitchToFiber; external kernel32 name 'SwitchToFiber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SwitchToThread: Pointer;

function SwitchToThread;
begin
  GetProcedureAddress(_SwitchToThread, kernel32, 'SwitchToThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SwitchToThread]
  end;
end;
{$ELSE}
function SwitchToThread; external kernel32 name 'SwitchToThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateThread: Pointer;

function CreateThread;
begin
  GetProcedureAddress(_CreateThread, kernel32, 'CreateThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateThread]
  end;
end;
{$ELSE}
function CreateThread; external kernel32 name 'CreateThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateRemoteThread: Pointer;

function CreateRemoteThread;
begin
  GetProcedureAddress(_CreateRemoteThread, kernel32, 'CreateRemoteThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateRemoteThread]
  end;
end;
{$ELSE}
function CreateRemoteThread; external kernel32 name 'CreateRemoteThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentThread: Pointer;

function GetCurrentThread;
begin
  GetProcedureAddress(_GetCurrentThread, kernel32, 'GetCurrentThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentThread]
  end;
end;
{$ELSE}
function GetCurrentThread; external kernel32 name 'GetCurrentThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentThreadId: Pointer;

function GetCurrentThreadId;
begin
  GetProcedureAddress(_GetCurrentThreadId, kernel32, 'GetCurrentThreadId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentThreadId]
  end;
end;
{$ELSE}
function GetCurrentThreadId; external kernel32 name 'GetCurrentThreadId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessIdOfThread: Pointer;

function GetProcessIdOfThread;
begin
  GetProcedureAddress(_GetProcessIdOfThread, kernel32, 'GetProcessIdOfThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessIdOfThread]
  end;
end;
{$ELSE}
function GetProcessIdOfThread; external kernel32 name 'GetProcessIdOfThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadId: Pointer;

function GetThreadId;
begin
  GetProcedureAddress(_GetThreadId, kernel32, 'GetThreadId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadId]
  end;
end;
{$ELSE}
function GetThreadId; external kernel32 name 'GetThreadId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessId: Pointer;

function GetProcessId;
begin
  GetProcedureAddress(_GetProcessId, kernel32, 'GetProcessId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessId]
  end;
end;
{$ELSE}
function GetProcessId; external kernel32 name 'GetProcessId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentProcessorNumber: Pointer;

function GetCurrentProcessorNumber;
begin
  GetProcedureAddress(_GetCurrentProcessorNumber, kernel32, 'GetCurrentProcessorNumber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentProcessorNumber]
  end;
end;
{$ELSE}
function GetCurrentProcessorNumber; external kernel32 name 'GetCurrentProcessorNumber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadAffinityMask: Pointer;

function SetThreadAffinityMask;
begin
  GetProcedureAddress(_SetThreadAffinityMask, kernel32, 'SetThreadAffinityMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadAffinityMask]
  end;
end;
{$ELSE}
function SetThreadAffinityMask; external kernel32 name 'SetThreadAffinityMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadIdealProcessor: Pointer;

function SetThreadIdealProcessor;
begin
  GetProcedureAddress(_SetThreadIdealProcessor, kernel32, 'SetThreadIdealProcessor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadIdealProcessor]
  end;
end;
{$ELSE}
function SetThreadIdealProcessor; external kernel32 name 'SetThreadIdealProcessor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetProcessPriorityBoost: Pointer;

function SetProcessPriorityBoost;
begin
  GetProcedureAddress(_SetProcessPriorityBoost, kernel32, 'SetProcessPriorityBoost');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessPriorityBoost]
  end;
end;
{$ELSE}
function SetProcessPriorityBoost; external kernel32 name 'SetProcessPriorityBoost';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessPriorityBoost: Pointer;

function GetProcessPriorityBoost;
begin
  GetProcedureAddress(_GetProcessPriorityBoost, kernel32, 'GetProcessPriorityBoost');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessPriorityBoost]
  end;
end;
{$ELSE}
function GetProcessPriorityBoost; external kernel32 name 'GetProcessPriorityBoost';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RequestWakeupLatency: Pointer;

function RequestWakeupLatency;
begin
  GetProcedureAddress(_RequestWakeupLatency, kernel32, 'RequestWakeupLatency');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RequestWakeupLatency]
  end;
end;
{$ELSE}
function RequestWakeupLatency; external kernel32 name 'RequestWakeupLatency';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsSystemResumeAutomatic: Pointer;

function IsSystemResumeAutomatic;
begin
  GetProcedureAddress(_IsSystemResumeAutomatic, kernel32, 'IsSystemResumeAutomatic');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsSystemResumeAutomatic]
  end;
end;
{$ELSE}
function IsSystemResumeAutomatic; external kernel32 name 'IsSystemResumeAutomatic';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenThread: Pointer;

function OpenThread;
begin
  GetProcedureAddress(_OpenThread, kernel32, 'OpenThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenThread]
  end;
end;
{$ELSE}
function OpenThread; external kernel32 name 'OpenThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadPriority: Pointer;

function SetThreadPriority;
begin
  GetProcedureAddress(_SetThreadPriority, kernel32, 'SetThreadPriority');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadPriority]
  end;
end;
{$ELSE}
function SetThreadPriority; external kernel32 name 'SetThreadPriority';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadPriorityBoost: Pointer;

function SetThreadPriorityBoost;
begin
  GetProcedureAddress(_SetThreadPriorityBoost, kernel32, 'SetThreadPriorityBoost');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadPriorityBoost]
  end;
end;
{$ELSE}
function SetThreadPriorityBoost; external kernel32 name 'SetThreadPriorityBoost';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadPriorityBoost: Pointer;

function GetThreadPriorityBoost;
begin
  GetProcedureAddress(_GetThreadPriorityBoost, kernel32, 'GetThreadPriorityBoost');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadPriorityBoost]
  end;
end;
{$ELSE}
function GetThreadPriorityBoost; external kernel32 name 'GetThreadPriorityBoost';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadPriority: Pointer;

function GetThreadPriority;
begin
  GetProcedureAddress(_GetThreadPriority, kernel32, 'GetThreadPriority');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadPriority]
  end;
end;
{$ELSE}
function GetThreadPriority; external kernel32 name 'GetThreadPriority';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadTimes: Pointer;

function GetThreadTimes;
begin
  GetProcedureAddress(_GetThreadTimes, kernel32, 'GetThreadTimes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadTimes]
  end;
end;
{$ELSE}
function GetThreadTimes; external kernel32 name 'GetThreadTimes';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadIOPendingFlag: Pointer;

function GetThreadIOPendingFlag;
begin
  GetProcedureAddress(_GetThreadIOPendingFlag, kernel32, 'GetThreadIOPendingFlag');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadIOPendingFlag]
  end;
end;
{$ELSE}
function GetThreadIOPendingFlag; external kernel32 name 'GetThreadIOPendingFlag';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExitThread: Pointer;

procedure ExitThread;
begin
  GetProcedureAddress(_ExitThread, kernel32, 'ExitThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExitThread]
  end;
end;
{$ELSE}
procedure ExitThread; external kernel32 name 'ExitThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TerminateThread: Pointer;

function TerminateThread;
begin
  GetProcedureAddress(_TerminateThread, kernel32, 'TerminateThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TerminateThread]
  end;
end;
{$ELSE}
function TerminateThread; external kernel32 name 'TerminateThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetExitCodeThread: Pointer;

function GetExitCodeThread;
begin
  GetProcedureAddress(_GetExitCodeThread, kernel32, 'GetExitCodeThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetExitCodeThread]
  end;
end;
{$ELSE}
function GetExitCodeThread; external kernel32 name 'GetExitCodeThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadSelectorEntry: Pointer;

function GetThreadSelectorEntry;
begin
  GetProcedureAddress(_GetThreadSelectorEntry, kernel32, 'GetThreadSelectorEntry');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadSelectorEntry]
  end;
end;
{$ELSE}
function GetThreadSelectorEntry; external kernel32 name 'GetThreadSelectorEntry';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadExecutionState: Pointer;

function SetThreadExecutionState;
begin
  GetProcedureAddress(_SetThreadExecutionState, kernel32, 'SetThreadExecutionState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadExecutionState]
  end;
end;
{$ELSE}
function SetThreadExecutionState; external kernel32 name 'SetThreadExecutionState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLastError: Pointer;

function GetLastError;
begin
  GetProcedureAddress(_GetLastError, kernel32, 'GetLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLastError]
  end;
end;
{$ELSE}
function GetLastError; external kernel32 name 'GetLastError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetLastError: Pointer;

procedure SetLastError;
begin
  GetProcedureAddress(_SetLastError, kernel32, 'SetLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLastError]
  end;
end;
{$ELSE}
procedure SetLastError; external kernel32 name 'SetLastError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RestoreLastError: Pointer;

procedure RestoreLastError;
begin
  GetProcedureAddress(_RestoreLastError, kernel32, 'RestoreLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RestoreLastError]
  end;
end;
{$ELSE}
procedure RestoreLastError; external kernel32 name 'RestoreLastError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetOverlappedResult: Pointer;

function GetOverlappedResult;
begin
  GetProcedureAddress(_GetOverlappedResult, kernel32, 'GetOverlappedResult');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOverlappedResult]
  end;
end;
{$ELSE}
function GetOverlappedResult; external kernel32 name 'GetOverlappedResult';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIoCompletionPort: Pointer;

function CreateIoCompletionPort;
begin
  GetProcedureAddress(_CreateIoCompletionPort, kernel32, 'CreateIoCompletionPort');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIoCompletionPort]
  end;
end;
{$ELSE}
function CreateIoCompletionPort; external kernel32 name 'CreateIoCompletionPort';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetQueuedCompletionStatus: Pointer;

function GetQueuedCompletionStatus;
begin
  GetProcedureAddress(_GetQueuedCompletionStatus, kernel32, 'GetQueuedCompletionStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetQueuedCompletionStatus]
  end;
end;
{$ELSE}
function GetQueuedCompletionStatus; external kernel32 name 'GetQueuedCompletionStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PostQueuedCompletionStatus: Pointer;

function PostQueuedCompletionStatus;
begin
  GetProcedureAddress(_PostQueuedCompletionStatus, kernel32, 'PostQueuedCompletionStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostQueuedCompletionStatus]
  end;
end;
{$ELSE}
function PostQueuedCompletionStatus; external kernel32 name 'PostQueuedCompletionStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetErrorMode: Pointer;

function SetErrorMode;
begin
  GetProcedureAddress(_SetErrorMode, kernel32, 'SetErrorMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetErrorMode]
  end;
end;
{$ELSE}
function SetErrorMode; external kernel32 name 'SetErrorMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadProcessMemory: Pointer;

function ReadProcessMemory;
begin
  GetProcedureAddress(_ReadProcessMemory, kernel32, 'ReadProcessMemory');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadProcessMemory]
  end;
end;
{$ELSE}
function ReadProcessMemory; external kernel32 name 'ReadProcessMemory';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProcessMemory: Pointer;

function WriteProcessMemory;
begin
  GetProcedureAddress(_WriteProcessMemory, kernel32, 'WriteProcessMemory');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProcessMemory]
  end;
end;
{$ELSE}
function WriteProcessMemory; external kernel32 name 'WriteProcessMemory';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadContext: Pointer;

function GetThreadContext;
begin
  GetProcedureAddress(_GetThreadContext, kernel32, 'GetThreadContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadContext]
  end;
end;
{$ELSE}
function GetThreadContext; external kernel32 name 'GetThreadContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadContext: Pointer;

function SetThreadContext;
begin
  GetProcedureAddress(_SetThreadContext, kernel32, 'SetThreadContext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadContext]
  end;
end;
{$ELSE}
function SetThreadContext; external kernel32 name 'SetThreadContext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SuspendThread: Pointer;

function SuspendThread;
begin
  GetProcedureAddress(_SuspendThread, kernel32, 'SuspendThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SuspendThread]
  end;
end;
{$ELSE}
function SuspendThread; external kernel32 name 'SuspendThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ResumeThread: Pointer;

function ResumeThread;
begin
  GetProcedureAddress(_ResumeThread, kernel32, 'ResumeThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResumeThread]
  end;
end;
{$ELSE}
function ResumeThread; external kernel32 name 'ResumeThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueueUserAPC: Pointer;

function QueueUserAPC;
begin
  GetProcedureAddress(_QueueUserAPC, kernel32, 'QueueUserAPC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueueUserAPC]
  end;
end;
{$ELSE}
function QueueUserAPC; external kernel32 name 'QueueUserAPC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsDebuggerPresent: Pointer;

function IsDebuggerPresent;
begin
  GetProcedureAddress(_IsDebuggerPresent, kernel32, 'IsDebuggerPresent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDebuggerPresent]
  end;
end;
{$ELSE}
function IsDebuggerPresent; external kernel32 name 'IsDebuggerPresent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CheckRemoteDebuggerPresent: Pointer;

function CheckRemoteDebuggerPresent;
begin
  GetProcedureAddress(_CheckRemoteDebuggerPresent, kernel32, 'CheckRemoteDebuggerPresent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckRemoteDebuggerPresent]
  end;
end;
{$ELSE}
function CheckRemoteDebuggerPresent; external kernel32 name 'CheckRemoteDebuggerPresent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DebugBreak: Pointer;

procedure DebugBreak;
begin
  GetProcedureAddress(_DebugBreak, kernel32, 'DebugBreak');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DebugBreak]
  end;
end;
{$ELSE}
procedure DebugBreak; external kernel32 name 'DebugBreak';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitForDebugEvent: Pointer;

function WaitForDebugEvent;
begin
  GetProcedureAddress(_WaitForDebugEvent, kernel32, 'WaitForDebugEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitForDebugEvent]
  end;
end;
{$ELSE}
function WaitForDebugEvent; external kernel32 name 'WaitForDebugEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ContinueDebugEvent: Pointer;

function ContinueDebugEvent;
begin
  GetProcedureAddress(_ContinueDebugEvent, kernel32, 'ContinueDebugEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ContinueDebugEvent]
  end;
end;
{$ELSE}
function ContinueDebugEvent; external kernel32 name 'ContinueDebugEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DebugActiveProcess: Pointer;

function DebugActiveProcess;
begin
  GetProcedureAddress(_DebugActiveProcess, kernel32, 'DebugActiveProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DebugActiveProcess]
  end;
end;
{$ELSE}
function DebugActiveProcess; external kernel32 name 'DebugActiveProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DebugActiveProcessStop: Pointer;

function DebugActiveProcessStop;
begin
  GetProcedureAddress(_DebugActiveProcessStop, kernel32, 'DebugActiveProcessStop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DebugActiveProcessStop]
  end;
end;
{$ELSE}
function DebugActiveProcessStop; external kernel32 name 'DebugActiveProcessStop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DebugSetProcessKillOnExit: Pointer;

function DebugSetProcessKillOnExit;
begin
  GetProcedureAddress(_DebugSetProcessKillOnExit, kernel32, 'DebugSetProcessKillOnExit');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DebugSetProcessKillOnExit]
  end;
end;
{$ELSE}
function DebugSetProcessKillOnExit; external kernel32 name 'DebugSetProcessKillOnExit';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DebugBreakProcess: Pointer;

function DebugBreakProcess;
begin
  GetProcedureAddress(_DebugBreakProcess, kernel32, 'DebugBreakProcess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DebugBreakProcess]
  end;
end;
{$ELSE}
function DebugBreakProcess; external kernel32 name 'DebugBreakProcess';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitializeCriticalSection: Pointer;

procedure InitializeCriticalSection;
begin
  GetProcedureAddress(_InitializeCriticalSection, kernel32, 'InitializeCriticalSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitializeCriticalSection]
  end;
end;
{$ELSE}
procedure InitializeCriticalSection; external kernel32 name 'InitializeCriticalSection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnterCriticalSection: Pointer;

procedure EnterCriticalSection;
begin
  GetProcedureAddress(_EnterCriticalSection, kernel32, 'EnterCriticalSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnterCriticalSection]
  end;
end;
{$ELSE}
procedure EnterCriticalSection; external kernel32 name 'EnterCriticalSection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LeaveCriticalSection: Pointer;

procedure LeaveCriticalSection;
begin
  GetProcedureAddress(_LeaveCriticalSection, kernel32, 'LeaveCriticalSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LeaveCriticalSection]
  end;
end;
{$ELSE}
procedure LeaveCriticalSection; external kernel32 name 'LeaveCriticalSection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitCritSectAndSpinCount: Pointer;

function InitializeCriticalSectionAndSpinCount;
begin
  GetProcedureAddress(_InitCritSectAndSpinCount, kernel32, 'InitializeCriticalSectionAndSpinCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitCritSectAndSpinCount]
  end;
end;
{$ELSE}
function InitializeCriticalSectionAndSpinCount; external kernel32 name 'InitializeCriticalSectionAndSpinCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCriticalSectionSpinCount: Pointer;

function SetCriticalSectionSpinCount;
begin
  GetProcedureAddress(_SetCriticalSectionSpinCount, kernel32, 'SetCriticalSectionSpinCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCriticalSectionSpinCount]
  end;
end;
{$ELSE}
function SetCriticalSectionSpinCount; external kernel32 name 'SetCriticalSectionSpinCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TryEnterCriticalSection: Pointer;

function TryEnterCriticalSection;
begin
  GetProcedureAddress(_TryEnterCriticalSection, kernel32, 'TryEnterCriticalSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TryEnterCriticalSection]
  end;
end;
{$ELSE}
function TryEnterCriticalSection; external kernel32 name 'TryEnterCriticalSection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteCriticalSection: Pointer;

procedure DeleteCriticalSection;
begin
  GetProcedureAddress(_DeleteCriticalSection, kernel32, 'DeleteCriticalSection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteCriticalSection]
  end;
end;
{$ELSE}
procedure DeleteCriticalSection; external kernel32 name 'DeleteCriticalSection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetEvent: Pointer;

function SetEvent;
begin
  GetProcedureAddress(_SetEvent, kernel32, 'SetEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEvent]
  end;
end;
{$ELSE}
function SetEvent; external kernel32 name 'SetEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ResetEvent: Pointer;

function ResetEvent;
begin
  GetProcedureAddress(_ResetEvent, kernel32, 'ResetEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ResetEvent]
  end;
end;
{$ELSE}
function ResetEvent; external kernel32 name 'ResetEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PulseEvent: Pointer;

function PulseEvent;
begin
  GetProcedureAddress(_PulseEvent, kernel32, 'PulseEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PulseEvent]
  end;
end;
{$ELSE}
function PulseEvent; external kernel32 name 'PulseEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReleaseSemaphore: Pointer;

function ReleaseSemaphore;
begin
  GetProcedureAddress(_ReleaseSemaphore, kernel32, 'ReleaseSemaphore');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReleaseSemaphore]
  end;
end;
{$ELSE}
function ReleaseSemaphore; external kernel32 name 'ReleaseSemaphore';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReleaseMutex: Pointer;

function ReleaseMutex;
begin
  GetProcedureAddress(_ReleaseMutex, kernel32, 'ReleaseMutex');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReleaseMutex]
  end;
end;
{$ELSE}
function ReleaseMutex; external kernel32 name 'ReleaseMutex';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitForSingleObject: Pointer;

function WaitForSingleObject;
begin
  GetProcedureAddress(_WaitForSingleObject, kernel32, 'WaitForSingleObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitForSingleObject]
  end;
end;
{$ELSE}
function WaitForSingleObject; external kernel32 name 'WaitForSingleObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitForMultipleObjects: Pointer;

function WaitForMultipleObjects;
begin
  GetProcedureAddress(_WaitForMultipleObjects, kernel32, 'WaitForMultipleObjects');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitForMultipleObjects]
  end;
end;
{$ELSE}
function WaitForMultipleObjects; external kernel32 name 'WaitForMultipleObjects';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Sleep: Pointer;

procedure Sleep;
begin
  GetProcedureAddress(_Sleep, kernel32, 'Sleep');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Sleep]
  end;
end;
{$ELSE}
procedure Sleep; external kernel32 name 'Sleep';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadResource: Pointer;

function LoadResource;
begin
  GetProcedureAddress(_LoadResource, kernel32, 'LoadResource');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadResource]
  end;
end;
{$ELSE}
function LoadResource; external kernel32 name 'LoadResource';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SizeofResource: Pointer;

function SizeofResource;
begin
  GetProcedureAddress(_SizeofResource, kernel32, 'SizeofResource');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SizeofResource]
  end;
end;
{$ELSE}
function SizeofResource; external kernel32 name 'SizeofResource';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalDeleteAtom: Pointer;

function GlobalDeleteAtom;
begin
  GetProcedureAddress(_GlobalDeleteAtom, kernel32, 'GlobalDeleteAtom');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalDeleteAtom]
  end;
end;
{$ELSE}
function GlobalDeleteAtom; external kernel32 name 'GlobalDeleteAtom';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitAtomTable: Pointer;

function InitAtomTable;
begin
  GetProcedureAddress(_InitAtomTable, kernel32, 'InitAtomTable');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitAtomTable]
  end;
end;
{$ELSE}
function InitAtomTable; external kernel32 name 'InitAtomTable';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteAtom: Pointer;

function DeleteAtom;
begin
  GetProcedureAddress(_DeleteAtom, kernel32, 'DeleteAtom');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteAtom]
  end;
end;
{$ELSE}
function DeleteAtom; external kernel32 name 'DeleteAtom';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetHandleCount: Pointer;

function SetHandleCount;
begin
  GetProcedureAddress(_SetHandleCount, kernel32, 'SetHandleCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetHandleCount]
  end;
end;
{$ELSE}
function SetHandleCount; external kernel32 name 'SetHandleCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogicalDrives: Pointer;

function GetLogicalDrives;
begin
  GetProcedureAddress(_GetLogicalDrives, kernel32, 'GetLogicalDrives');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogicalDrives]
  end;
end;
{$ELSE}
function GetLogicalDrives; external kernel32 name 'GetLogicalDrives';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LockFile: Pointer;

function LockFile;
begin
  GetProcedureAddress(_LockFile, kernel32, 'LockFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LockFile]
  end;
end;
{$ELSE}
function LockFile; external kernel32 name 'LockFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnlockFile: Pointer;

function UnlockFile;
begin
  GetProcedureAddress(_UnlockFile, kernel32, 'UnlockFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnlockFile]
  end;
end;
{$ELSE}
function UnlockFile; external kernel32 name 'UnlockFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LockFileEx: Pointer;

function LockFileEx;
begin
  GetProcedureAddress(_LockFileEx, kernel32, 'LockFileEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LockFileEx]
  end;
end;
{$ELSE}
function LockFileEx; external kernel32 name 'LockFileEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnlockFileEx: Pointer;

function UnlockFileEx;
begin
  GetProcedureAddress(_UnlockFileEx, kernel32, 'UnlockFileEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnlockFileEx]
  end;
end;
{$ELSE}
function UnlockFileEx; external kernel32 name 'UnlockFileEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileInformationByHandle: Pointer;

function GetFileInformationByHandle;
begin
  GetProcedureAddress(_GetFileInformationByHandle, kernel32, 'GetFileInformationByHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileInformationByHandle]
  end;
end;
{$ELSE}
function GetFileInformationByHandle; external kernel32 name 'GetFileInformationByHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileType: Pointer;

function GetFileType;
begin
  GetProcedureAddress(_GetFileType, kernel32, 'GetFileType');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileType]
  end;
end;
{$ELSE}
function GetFileType; external kernel32 name 'GetFileType';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileSize: Pointer;

function GetFileSize;
begin
  GetProcedureAddress(_GetFileSize, kernel32, 'GetFileSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileSize]
  end;
end;
{$ELSE}
function GetFileSize; external kernel32 name 'GetFileSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileSizeEx: Pointer;

function GetFileSizeEx;
begin
  GetProcedureAddress(_GetFileSizeEx, kernel32, 'GetFileSizeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileSizeEx]
  end;
end;
{$ELSE}
function GetFileSizeEx; external kernel32 name 'GetFileSizeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStdHandle: Pointer;

function GetStdHandle;
begin
  GetProcedureAddress(_GetStdHandle, kernel32, 'GetStdHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStdHandle]
  end;
end;
{$ELSE}
function GetStdHandle; external kernel32 name 'GetStdHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetStdHandle: Pointer;

function SetStdHandle;
begin
  GetProcedureAddress(_SetStdHandle, kernel32, 'SetStdHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetStdHandle]
  end;
end;
{$ELSE}
function SetStdHandle; external kernel32 name 'SetStdHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteFile: Pointer;

function WriteFile;
begin
  GetProcedureAddress(_WriteFile, kernel32, 'WriteFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteFile]
  end;
end;
{$ELSE}
function WriteFile; external kernel32 name 'WriteFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadFile: Pointer;

function ReadFile;
begin
  GetProcedureAddress(_ReadFile, kernel32, 'ReadFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadFile]
  end;
end;
{$ELSE}
function ReadFile; external kernel32 name 'ReadFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlushFileBuffers: Pointer;

function FlushFileBuffers;
begin
  GetProcedureAddress(_FlushFileBuffers, kernel32, 'FlushFileBuffers');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlushFileBuffers]
  end;
end;
{$ELSE}
function FlushFileBuffers; external kernel32 name 'FlushFileBuffers';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeviceIoControl: Pointer;

function DeviceIoControl;
begin
  GetProcedureAddress(_DeviceIoControl, kernel32, 'DeviceIoControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeviceIoControl]
  end;
end;
{$ELSE}
function DeviceIoControl; external kernel32 name 'DeviceIoControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RequestDeviceWakeup: Pointer;

function RequestDeviceWakeup;
begin
  GetProcedureAddress(_RequestDeviceWakeup, kernel32, 'RequestDeviceWakeup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RequestDeviceWakeup]
  end;
end;
{$ELSE}
function RequestDeviceWakeup; external kernel32 name 'RequestDeviceWakeup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CancelDeviceWakeupRequest: Pointer;

function CancelDeviceWakeupRequest;
begin
  GetProcedureAddress(_CancelDeviceWakeupRequest, kernel32, 'CancelDeviceWakeupRequest');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CancelDeviceWakeupRequest]
  end;
end;
{$ELSE}
function CancelDeviceWakeupRequest; external kernel32 name 'CancelDeviceWakeupRequest';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDevicePowerState: Pointer;

function GetDevicePowerState;
begin
  GetProcedureAddress(_GetDevicePowerState, kernel32, 'GetDevicePowerState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDevicePowerState]
  end;
end;
{$ELSE}
function GetDevicePowerState; external kernel32 name 'GetDevicePowerState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMessageWaitingIndicator: Pointer;

function SetMessageWaitingIndicator;
begin
  GetProcedureAddress(_SetMessageWaitingIndicator, kernel32, 'SetMessageWaitingIndicator');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMessageWaitingIndicator]
  end;
end;
{$ELSE}
function SetMessageWaitingIndicator; external kernel32 name 'SetMessageWaitingIndicator';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetEndOfFile: Pointer;

function SetEndOfFile;
begin
  GetProcedureAddress(_SetEndOfFile, kernel32, 'SetEndOfFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEndOfFile]
  end;
end;
{$ELSE}
function SetEndOfFile; external kernel32 name 'SetEndOfFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFilePointer: Pointer;

function SetFilePointer;
begin
  GetProcedureAddress(_SetFilePointer, kernel32, 'SetFilePointer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFilePointer]
  end;
end;
{$ELSE}
function SetFilePointer; external kernel32 name 'SetFilePointer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFilePointerEx: Pointer;

function SetFilePointerEx;
begin
  GetProcedureAddress(_SetFilePointerEx, kernel32, 'SetFilePointerEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFilePointerEx]
  end;
end;
{$ELSE}
function SetFilePointerEx; external kernel32 name 'SetFilePointerEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindClose: Pointer;

function FindClose;
begin
  GetProcedureAddress(_FindClose, kernel32, 'FindClose');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindClose]
  end;
end;
{$ELSE}
function FindClose; external kernel32 name 'FindClose';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileTime: Pointer;

function GetFileTime;
begin
  GetProcedureAddress(_GetFileTime, kernel32, 'GetFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileTime]
  end;
end;
{$ELSE}
function GetFileTime; external kernel32 name 'GetFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileTime: Pointer;

function SetFileTime;
begin
  GetProcedureAddress(_SetFileTime, kernel32, 'SetFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileTime]
  end;
end;
{$ELSE}
function SetFileTime; external kernel32 name 'SetFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileValidData: Pointer;

function SetFileValidData;
begin
  GetProcedureAddress(_SetFileValidData, kernel32, 'SetFileValidData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileValidData]
  end;
end;
{$ELSE}
function SetFileValidData; external kernel32 name 'SetFileValidData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileShortNameA: Pointer;

function SetFileShortNameA;
begin
  GetProcedureAddress(_SetFileShortNameA, kernel32, 'SetFileShortNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileShortNameA]
  end;
end;
{$ELSE}
function SetFileShortNameA; external kernel32 name 'SetFileShortNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileShortNameW: Pointer;

function SetFileShortNameW;
begin
  GetProcedureAddress(_SetFileShortNameW, kernel32, 'SetFileShortNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileShortNameW]
  end;
end;
{$ELSE}
function SetFileShortNameW; external kernel32 name 'SetFileShortNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileShortName: Pointer;

function SetFileShortName;
begin
  GetProcedureAddress(_SetFileShortName, kernel32, 'SetFileShortNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileShortName]
  end;
end;
{$ELSE}
function SetFileShortName; external kernel32 name 'SetFileShortNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileShortName: Pointer;

function SetFileShortName;
begin
  GetProcedureAddress(_SetFileShortName, kernel32, 'SetFileShortNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileShortName]
  end;
end;
{$ELSE}
function SetFileShortName; external kernel32 name 'SetFileShortNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CloseHandle: Pointer;

function CloseHandle;
begin
  GetProcedureAddress(_CloseHandle, kernel32, 'CloseHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseHandle]
  end;
end;
{$ELSE}
function CloseHandle; external kernel32 name 'CloseHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DuplicateHandle: Pointer;

function DuplicateHandle;
begin
  GetProcedureAddress(_DuplicateHandle, kernel32, 'DuplicateHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DuplicateHandle]
  end;
end;
{$ELSE}
function DuplicateHandle; external kernel32 name 'DuplicateHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetHandleInformation: Pointer;

function GetHandleInformation;
begin
  GetProcedureAddress(_GetHandleInformation, kernel32, 'GetHandleInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetHandleInformation]
  end;
end;
{$ELSE}
function GetHandleInformation; external kernel32 name 'GetHandleInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetHandleInformation: Pointer;

function SetHandleInformation;
begin
  GetProcedureAddress(_SetHandleInformation, kernel32, 'SetHandleInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetHandleInformation]
  end;
end;
{$ELSE}
function SetHandleInformation; external kernel32 name 'SetHandleInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadModule: Pointer;

function LoadModule;
begin
  GetProcedureAddress(_LoadModule, kernel32, 'LoadModule');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadModule]
  end;
end;
{$ELSE}
function LoadModule; external kernel32 name 'LoadModule';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WinExec: Pointer;

function WinExec;
begin
  GetProcedureAddress(_WinExec, kernel32, 'WinExec');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WinExec]
  end;
end;
{$ELSE}
function WinExec; external kernel32 name 'WinExec';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ClearCommBreak: Pointer;

function ClearCommBreak;
begin
  GetProcedureAddress(_ClearCommBreak, kernel32, 'ClearCommBreak');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClearCommBreak]
  end;
end;
{$ELSE}
function ClearCommBreak; external kernel32 name 'ClearCommBreak';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ClearCommError: Pointer;

function ClearCommError;
begin
  GetProcedureAddress(_ClearCommError, kernel32, 'ClearCommError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClearCommError]
  end;
end;
{$ELSE}
function ClearCommError; external kernel32 name 'ClearCommError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetupComm: Pointer;

function SetupComm;
begin
  GetProcedureAddress(_SetupComm, kernel32, 'SetupComm');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetupComm]
  end;
end;
{$ELSE}
function SetupComm; external kernel32 name 'SetupComm';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EscapeCommFunction: Pointer;

function EscapeCommFunction;
begin
  GetProcedureAddress(_EscapeCommFunction, kernel32, 'EscapeCommFunction');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EscapeCommFunction]
  end;
end;
{$ELSE}
function EscapeCommFunction; external kernel32 name 'EscapeCommFunction';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommConfig: Pointer;

function GetCommConfig;
begin
  GetProcedureAddress(_GetCommConfig, kernel32, 'GetCommConfig');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommConfig]
  end;
end;
{$ELSE}
function GetCommConfig; external kernel32 name 'GetCommConfig';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommMask: Pointer;

function GetCommMask;
begin
  GetProcedureAddress(_GetCommMask, kernel32, 'GetCommMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommMask]
  end;
end;
{$ELSE}
function GetCommMask; external kernel32 name 'GetCommMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommProperties: Pointer;

function GetCommProperties;
begin
  GetProcedureAddress(_GetCommProperties, kernel32, 'GetCommProperties');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommProperties]
  end;
end;
{$ELSE}
function GetCommProperties; external kernel32 name 'GetCommProperties';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommModemStatus: Pointer;

function GetCommModemStatus;
begin
  GetProcedureAddress(_GetCommModemStatus, kernel32, 'GetCommModemStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommModemStatus]
  end;
end;
{$ELSE}
function GetCommModemStatus; external kernel32 name 'GetCommModemStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommState: Pointer;

function GetCommState;
begin
  GetProcedureAddress(_GetCommState, kernel32, 'GetCommState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommState]
  end;
end;
{$ELSE}
function GetCommState; external kernel32 name 'GetCommState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommTimeouts: Pointer;

function GetCommTimeouts;
begin
  GetProcedureAddress(_GetCommTimeouts, kernel32, 'GetCommTimeouts');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommTimeouts]
  end;
end;
{$ELSE}
function GetCommTimeouts; external kernel32 name 'GetCommTimeouts';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PurgeComm: Pointer;

function PurgeComm;
begin
  GetProcedureAddress(_PurgeComm, kernel32, 'PurgeComm');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PurgeComm]
  end;
end;
{$ELSE}
function PurgeComm; external kernel32 name 'PurgeComm';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCommBreak: Pointer;

function SetCommBreak;
begin
  GetProcedureAddress(_SetCommBreak, kernel32, 'SetCommBreak');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCommBreak]
  end;
end;
{$ELSE}
function SetCommBreak; external kernel32 name 'SetCommBreak';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCommConfig: Pointer;

function SetCommConfig;
begin
  GetProcedureAddress(_SetCommConfig, kernel32, 'SetCommConfig');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCommConfig]
  end;
end;
{$ELSE}
function SetCommConfig; external kernel32 name 'SetCommConfig';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCommMask: Pointer;

function SetCommMask;
begin
  GetProcedureAddress(_SetCommMask, kernel32, 'SetCommMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCommMask]
  end;
end;
{$ELSE}
function SetCommMask; external kernel32 name 'SetCommMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCommState: Pointer;

function SetCommState;
begin
  GetProcedureAddress(_SetCommState, kernel32, 'SetCommState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCommState]
  end;
end;
{$ELSE}
function SetCommState; external kernel32 name 'SetCommState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCommTimeouts: Pointer;

function SetCommTimeouts;
begin
  GetProcedureAddress(_SetCommTimeouts, kernel32, 'SetCommTimeouts');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCommTimeouts]
  end;
end;
{$ELSE}
function SetCommTimeouts; external kernel32 name 'SetCommTimeouts';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TransmitCommChar: Pointer;

function TransmitCommChar;
begin
  GetProcedureAddress(_TransmitCommChar, kernel32, 'TransmitCommChar');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TransmitCommChar]
  end;
end;
{$ELSE}
function TransmitCommChar; external kernel32 name 'TransmitCommChar';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitCommEvent: Pointer;

function WaitCommEvent;
begin
  GetProcedureAddress(_WaitCommEvent, kernel32, 'WaitCommEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitCommEvent]
  end;
end;
{$ELSE}
function WaitCommEvent; external kernel32 name 'WaitCommEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTapePosition: Pointer;

function SetTapePosition;
begin
  GetProcedureAddress(_SetTapePosition, kernel32, 'SetTapePosition');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTapePosition]
  end;
end;
{$ELSE}
function SetTapePosition; external kernel32 name 'SetTapePosition';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTapePosition: Pointer;

function GetTapePosition;
begin
  GetProcedureAddress(_GetTapePosition, kernel32, 'GetTapePosition');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTapePosition]
  end;
end;
{$ELSE}
function GetTapePosition; external kernel32 name 'GetTapePosition';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PrepareTape: Pointer;

function PrepareTape;
begin
  GetProcedureAddress(_PrepareTape, kernel32, 'PrepareTape');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrepareTape]
  end;
end;
{$ELSE}
function PrepareTape; external kernel32 name 'PrepareTape';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EraseTape: Pointer;

function EraseTape;
begin
  GetProcedureAddress(_EraseTape, kernel32, 'EraseTape');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EraseTape]
  end;
end;
{$ELSE}
function EraseTape; external kernel32 name 'EraseTape';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateTapePartition: Pointer;

function CreateTapePartition;
begin
  GetProcedureAddress(_CreateTapePartition, kernel32, 'CreateTapePartition');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateTapePartition]
  end;
end;
{$ELSE}
function CreateTapePartition; external kernel32 name 'CreateTapePartition';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteTapemark: Pointer;

function WriteTapemark;
begin
  GetProcedureAddress(_WriteTapemark, kernel32, 'WriteTapemark');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteTapemark]
  end;
end;
{$ELSE}
function WriteTapemark; external kernel32 name 'WriteTapemark';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTapeStatus: Pointer;

function GetTapeStatus;
begin
  GetProcedureAddress(_GetTapeStatus, kernel32, 'GetTapeStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTapeStatus]
  end;
end;
{$ELSE}
function GetTapeStatus; external kernel32 name 'GetTapeStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTapeParameters: Pointer;

function GetTapeParameters;
begin
  GetProcedureAddress(_GetTapeParameters, kernel32, 'GetTapeParameters');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTapeParameters]
  end;
end;
{$ELSE}
function GetTapeParameters; external kernel32 name 'GetTapeParameters';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTapeParameters: Pointer;

function SetTapeParameters;
begin
  GetProcedureAddress(_SetTapeParameters, kernel32, 'SetTapeParameters');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTapeParameters]
  end;
end;
{$ELSE}
function SetTapeParameters; external kernel32 name 'SetTapeParameters';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _Beep: Pointer;

function Beep;
begin
  GetProcedureAddress(_Beep, kernel32, 'Beep');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Beep]
  end;
end;
{$ELSE}
function Beep; external kernel32 name 'Beep';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MulDiv: Pointer;

function MulDiv;
begin
  GetProcedureAddress(_MulDiv, kernel32, 'MulDiv');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MulDiv]
  end;
end;
{$ELSE}
function MulDiv; external kernel32 name 'MulDiv';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemTime: Pointer;

procedure GetSystemTime;
begin
  GetProcedureAddress(_GetSystemTime, kernel32, 'GetSystemTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemTime]
  end;
end;
{$ELSE}
procedure GetSystemTime; external kernel32 name 'GetSystemTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemTimeAsFileTime: Pointer;

procedure GetSystemTimeAsFileTime;
begin
  GetProcedureAddress(_GetSystemTimeAsFileTime, kernel32, 'GetSystemTimeAsFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemTimeAsFileTime]
  end;
end;
{$ELSE}
procedure GetSystemTimeAsFileTime; external kernel32 name 'GetSystemTimeAsFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSystemTime: Pointer;

function SetSystemTime;
begin
  GetProcedureAddress(_SetSystemTime, kernel32, 'SetSystemTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSystemTime]
  end;
end;
{$ELSE}
function SetSystemTime; external kernel32 name 'SetSystemTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLocalTime: Pointer;

procedure GetLocalTime;
begin
  GetProcedureAddress(_GetLocalTime, kernel32, 'GetLocalTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLocalTime]
  end;
end;
{$ELSE}
procedure GetLocalTime; external kernel32 name 'GetLocalTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetLocalTime: Pointer;

function SetLocalTime;
begin
  GetProcedureAddress(_SetLocalTime, kernel32, 'SetLocalTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLocalTime]
  end;
end;
{$ELSE}
function SetLocalTime; external kernel32 name 'SetLocalTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemInfo: Pointer;

procedure GetSystemInfo;
begin
  GetProcedureAddress(_GetSystemInfo, kernel32, 'GetSystemInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemInfo]
  end;
end;
{$ELSE}
procedure GetSystemInfo; external kernel32 name 'GetSystemInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemRegistryQuota: Pointer;

function GetSystemRegistryQuota;
begin
  GetProcedureAddress(_GetSystemRegistryQuota, kernel32, 'GetSystemRegistryQuota');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemRegistryQuota]
  end;
end;
{$ELSE}
function GetSystemRegistryQuota; external kernel32 name 'GetSystemRegistryQuota';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemTimes: Pointer;

function GetSystemTimes;
begin
  GetProcedureAddress(_GetSystemTimes, kernel32, 'GetSystemTimes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemTimes]
  end;
end;
{$ELSE}
function GetSystemTimes; external kernel32 name 'GetSystemTimes';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _GetNativeSystemInfo: Pointer;

procedure GetNativeSystemInfo;
begin
  GetProcedureAddress(_GetNativeSystemInfo, kernel32, 'GetNativeSystemInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNativeSystemInfo]
  end;
end;
{$ELSE}
procedure GetNativeSystemInfo; external kernel32 name 'GetNativeSystemInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsProcessorFeaturePresent: Pointer;

function IsProcessorFeaturePresent;
begin
  GetProcedureAddress(_IsProcessorFeaturePresent, kernel32, 'IsProcessorFeaturePresent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsProcessorFeaturePresent]
  end;
end;
{$ELSE}
function IsProcessorFeaturePresent; external kernel32 name 'IsProcessorFeaturePresent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SystemTimeToTzSpecificLocalTime: Pointer;

function SystemTimeToTzSpecificLocalTime;
begin
  GetProcedureAddress(_SystemTimeToTzSpecificLocalTime, kernel32, 'SystemTimeToTzSpecificLocalTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SystemTimeToTzSpecificLocalTime]
  end;
end;
{$ELSE}
function SystemTimeToTzSpecificLocalTime; external kernel32 name 'SystemTimeToTzSpecificLocalTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TzSpecificLocalTimeToSystemTime: Pointer;

function TzSpecificLocalTimeToSystemTime;
begin
  GetProcedureAddress(_TzSpecificLocalTimeToSystemTime, kernel32, 'TzSpecificLocalTimeToSystemTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TzSpecificLocalTimeToSystemTime]
  end;
end;
{$ELSE}
function TzSpecificLocalTimeToSystemTime; external kernel32 name 'TzSpecificLocalTimeToSystemTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTimeZoneInformation: Pointer;

function GetTimeZoneInformation;
begin
  GetProcedureAddress(_GetTimeZoneInformation, kernel32, 'GetTimeZoneInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTimeZoneInformation]
  end;
end;
{$ELSE}
function GetTimeZoneInformation; external kernel32 name 'GetTimeZoneInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTimeZoneInformation: Pointer;

function SetTimeZoneInformation;
begin
  GetProcedureAddress(_SetTimeZoneInformation, kernel32, 'SetTimeZoneInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTimeZoneInformation]
  end;
end;
{$ELSE}
function SetTimeZoneInformation; external kernel32 name 'SetTimeZoneInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SystemTimeToFileTime: Pointer;

function SystemTimeToFileTime;
begin
  GetProcedureAddress(_SystemTimeToFileTime, kernel32, 'SystemTimeToFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SystemTimeToFileTime]
  end;
end;
{$ELSE}
function SystemTimeToFileTime; external kernel32 name 'SystemTimeToFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FileTimeToLocalFileTime: Pointer;

function FileTimeToLocalFileTime;
begin
  GetProcedureAddress(_FileTimeToLocalFileTime, kernel32, 'FileTimeToLocalFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileTimeToLocalFileTime]
  end;
end;
{$ELSE}
function FileTimeToLocalFileTime; external kernel32 name 'FileTimeToLocalFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocalFileTimeToFileTime: Pointer;

function LocalFileTimeToFileTime;
begin
  GetProcedureAddress(_LocalFileTimeToFileTime, kernel32, 'LocalFileTimeToFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocalFileTimeToFileTime]
  end;
end;
{$ELSE}
function LocalFileTimeToFileTime; external kernel32 name 'LocalFileTimeToFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FileTimeToSystemTime: Pointer;

function FileTimeToSystemTime;
begin
  GetProcedureAddress(_FileTimeToSystemTime, kernel32, 'FileTimeToSystemTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileTimeToSystemTime]
  end;
end;
{$ELSE}
function FileTimeToSystemTime; external kernel32 name 'FileTimeToSystemTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CompareFileTime: Pointer;

function CompareFileTime;
begin
  GetProcedureAddress(_CompareFileTime, kernel32, 'CompareFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CompareFileTime]
  end;
end;
{$ELSE}
function CompareFileTime; external kernel32 name 'CompareFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FileTimeToDosDateTime: Pointer;

function FileTimeToDosDateTime;
begin
  GetProcedureAddress(_FileTimeToDosDateTime, kernel32, 'FileTimeToDosDateTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileTimeToDosDateTime]
  end;
end;
{$ELSE}
function FileTimeToDosDateTime; external kernel32 name 'FileTimeToDosDateTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DosDateTimeToFileTime: Pointer;

function DosDateTimeToFileTime;
begin
  GetProcedureAddress(_DosDateTimeToFileTime, kernel32, 'DosDateTimeToFileTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DosDateTimeToFileTime]
  end;
end;
{$ELSE}
function DosDateTimeToFileTime; external kernel32 name 'DosDateTimeToFileTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTickCount: Pointer;

function GetTickCount;
begin
  GetProcedureAddress(_GetTickCount, kernel32, 'GetTickCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTickCount]
  end;
end;
{$ELSE}
function GetTickCount; external kernel32 name 'GetTickCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSystemTimeAdjustment: Pointer;

function SetSystemTimeAdjustment;
begin
  GetProcedureAddress(_SetSystemTimeAdjustment, kernel32, 'SetSystemTimeAdjustment');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSystemTimeAdjustment]
  end;
end;
{$ELSE}
function SetSystemTimeAdjustment; external kernel32 name 'SetSystemTimeAdjustment';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemTimeAdjustment: Pointer;

function GetSystemTimeAdjustment;
begin
  GetProcedureAddress(_GetSystemTimeAdjustment, kernel32, 'GetSystemTimeAdjustment');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemTimeAdjustment]
  end;
end;
{$ELSE}
function GetSystemTimeAdjustment; external kernel32 name 'GetSystemTimeAdjustment';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FormatMessageA: Pointer;

function FormatMessageA;
begin
  GetProcedureAddress(_FormatMessageA, kernel32, 'FormatMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FormatMessageA]
  end;
end;
{$ELSE}
function FormatMessageA; external kernel32 name 'FormatMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FormatMessageW: Pointer;

function FormatMessageW;
begin
  GetProcedureAddress(_FormatMessageW, kernel32, 'FormatMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FormatMessageW]
  end;
end;
{$ELSE}
function FormatMessageW; external kernel32 name 'FormatMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FormatMessage: Pointer;

function FormatMessage;
begin
  GetProcedureAddress(_FormatMessage, kernel32, 'FormatMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FormatMessage]
  end;
end;
{$ELSE}
function FormatMessage; external kernel32 name 'FormatMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FormatMessage: Pointer;

function FormatMessage;
begin
  GetProcedureAddress(_FormatMessage, kernel32, 'FormatMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FormatMessage]
  end;
end;
{$ELSE}
function FormatMessage; external kernel32 name 'FormatMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePipe: Pointer;

function CreatePipe;
begin
  GetProcedureAddress(_CreatePipe, kernel32, 'CreatePipe');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePipe]
  end;
end;
{$ELSE}
function CreatePipe; external kernel32 name 'CreatePipe';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConnectNamedPipe: Pointer;

function ConnectNamedPipe;
begin
  GetProcedureAddress(_ConnectNamedPipe, kernel32, 'ConnectNamedPipe');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConnectNamedPipe]
  end;
end;
{$ELSE}
function ConnectNamedPipe; external kernel32 name 'ConnectNamedPipe';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DisconnectNamedPipe: Pointer;

function DisconnectNamedPipe;
begin
  GetProcedureAddress(_DisconnectNamedPipe, kernel32, 'DisconnectNamedPipe');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DisconnectNamedPipe]
  end;
end;
{$ELSE}
function DisconnectNamedPipe; external kernel32 name 'DisconnectNamedPipe';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetNamedPipeHandleState: Pointer;

function SetNamedPipeHandleState;
begin
  GetProcedureAddress(_SetNamedPipeHandleState, kernel32, 'SetNamedPipeHandleState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetNamedPipeHandleState]
  end;
end;
{$ELSE}
function SetNamedPipeHandleState; external kernel32 name 'SetNamedPipeHandleState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNamedPipeInfo: Pointer;

function GetNamedPipeInfo;
begin
  GetProcedureAddress(_GetNamedPipeInfo, kernel32, 'GetNamedPipeInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNamedPipeInfo]
  end;
end;
{$ELSE}
function GetNamedPipeInfo; external kernel32 name 'GetNamedPipeInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PeekNamedPipe: Pointer;

function PeekNamedPipe;
begin
  GetProcedureAddress(_PeekNamedPipe, kernel32, 'PeekNamedPipe');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekNamedPipe]
  end;
end;
{$ELSE}
function PeekNamedPipe; external kernel32 name 'PeekNamedPipe';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TransactNamedPipe: Pointer;

function TransactNamedPipe;
begin
  GetProcedureAddress(_TransactNamedPipe, kernel32, 'TransactNamedPipe');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TransactNamedPipe]
  end;
end;
{$ELSE}
function TransactNamedPipe; external kernel32 name 'TransactNamedPipe';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMailslotA: Pointer;

function CreateMailslotA;
begin
  GetProcedureAddress(_CreateMailslotA, kernel32, 'CreateMailslotA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMailslotA]
  end;
end;
{$ELSE}
function CreateMailslotA; external kernel32 name 'CreateMailslotA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMailslotW: Pointer;

function CreateMailslotW;
begin
  GetProcedureAddress(_CreateMailslotW, kernel32, 'CreateMailslotW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMailslotW]
  end;
end;
{$ELSE}
function CreateMailslotW; external kernel32 name 'CreateMailslotW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMailslot: Pointer;

function CreateMailslot;
begin
  GetProcedureAddress(_CreateMailslot, kernel32, 'CreateMailslotW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMailslot]
  end;
end;
{$ELSE}
function CreateMailslot; external kernel32 name 'CreateMailslotW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMailslot: Pointer;

function CreateMailslot;
begin
  GetProcedureAddress(_CreateMailslot, kernel32, 'CreateMailslotA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMailslot]
  end;
end;
{$ELSE}
function CreateMailslot; external kernel32 name 'CreateMailslotA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetMailslotInfo: Pointer;

function GetMailslotInfo;
begin
  GetProcedureAddress(_GetMailslotInfo, kernel32, 'GetMailslotInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMailslotInfo]
  end;
end;
{$ELSE}
function GetMailslotInfo; external kernel32 name 'GetMailslotInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMailslotInfo: Pointer;

function SetMailslotInfo;
begin
  GetProcedureAddress(_SetMailslotInfo, kernel32, 'SetMailslotInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMailslotInfo]
  end;
end;
{$ELSE}
function SetMailslotInfo; external kernel32 name 'SetMailslotInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapViewOfFile: Pointer;

function MapViewOfFile;
begin
  GetProcedureAddress(_MapViewOfFile, kernel32, 'MapViewOfFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapViewOfFile]
  end;
end;
{$ELSE}
function MapViewOfFile; external kernel32 name 'MapViewOfFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlushViewOfFile: Pointer;

function FlushViewOfFile;
begin
  GetProcedureAddress(_FlushViewOfFile, kernel32, 'FlushViewOfFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlushViewOfFile]
  end;
end;
{$ELSE}
function FlushViewOfFile; external kernel32 name 'FlushViewOfFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnmapViewOfFile: Pointer;

function UnmapViewOfFile;
begin
  GetProcedureAddress(_UnmapViewOfFile, kernel32, 'UnmapViewOfFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnmapViewOfFile]
  end;
end;
{$ELSE}
function UnmapViewOfFile; external kernel32 name 'UnmapViewOfFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EncryptFileA: Pointer;

function EncryptFileA;
begin
  GetProcedureAddress(_EncryptFileA, advapi32, 'EncryptFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EncryptFileA]
  end;
end;
{$ELSE}
function EncryptFileA; external advapi32 name 'EncryptFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EncryptFileW: Pointer;

function EncryptFileW;
begin
  GetProcedureAddress(_EncryptFileW, advapi32, 'EncryptFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EncryptFileW]
  end;
end;
{$ELSE}
function EncryptFileW; external advapi32 name 'EncryptFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EncryptFile: Pointer;

function EncryptFile;
begin
  GetProcedureAddress(_EncryptFile, advapi32, 'EncryptFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EncryptFile]
  end;
end;
{$ELSE}
function EncryptFile; external advapi32 name 'EncryptFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EncryptFile: Pointer;

function EncryptFile;
begin
  GetProcedureAddress(_EncryptFile, advapi32, 'EncryptFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EncryptFile]
  end;
end;
{$ELSE}
function EncryptFile; external advapi32 name 'EncryptFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DecryptFileA: Pointer;

function DecryptFileA;
begin
  GetProcedureAddress(_DecryptFileA, advapi32, 'DecryptFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DecryptFileA]
  end;
end;
{$ELSE}
function DecryptFileA; external advapi32 name 'DecryptFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DecryptFileW: Pointer;

function DecryptFileW;
begin
  GetProcedureAddress(_DecryptFileW, advapi32, 'DecryptFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DecryptFileW]
  end;
end;
{$ELSE}
function DecryptFileW; external advapi32 name 'DecryptFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DecryptFile: Pointer;

function DecryptFile;
begin
  GetProcedureAddress(_DecryptFile, advapi32, 'DecryptFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DecryptFile]
  end;
end;
{$ELSE}
function DecryptFile; external advapi32 name 'DecryptFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DecryptFile: Pointer;

function DecryptFile;
begin
  GetProcedureAddress(_DecryptFile, advapi32, 'DecryptFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DecryptFile]
  end;
end;
{$ELSE}
function DecryptFile; external advapi32 name 'DecryptFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FileEncryptionStatusA: Pointer;

function FileEncryptionStatusA;
begin
  GetProcedureAddress(_FileEncryptionStatusA, advapi32, 'FileEncryptionStatusA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileEncryptionStatusA]
  end;
end;
{$ELSE}
function FileEncryptionStatusA; external advapi32 name 'FileEncryptionStatusA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FileEncryptionStatusW: Pointer;

function FileEncryptionStatusW;
begin
  GetProcedureAddress(_FileEncryptionStatusW, advapi32, 'FileEncryptionStatusW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileEncryptionStatusW]
  end;
end;
{$ELSE}
function FileEncryptionStatusW; external advapi32 name 'FileEncryptionStatusW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FileEncryptionStatus: Pointer;

function FileEncryptionStatus;
begin
  GetProcedureAddress(_FileEncryptionStatus, advapi32, 'FileEncryptionStatusW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileEncryptionStatus]
  end;
end;
{$ELSE}
function FileEncryptionStatus; external advapi32 name 'FileEncryptionStatusW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FileEncryptionStatus: Pointer;

function FileEncryptionStatus;
begin
  GetProcedureAddress(_FileEncryptionStatus, advapi32, 'FileEncryptionStatusA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FileEncryptionStatus]
  end;
end;
{$ELSE}
function FileEncryptionStatus; external advapi32 name 'FileEncryptionStatusA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEncryptedFileRawA: Pointer;

function OpenEncryptedFileRawA;
begin
  GetProcedureAddress(_OpenEncryptedFileRawA, advapi32, 'OpenEncryptedFileRawA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEncryptedFileRawA]
  end;
end;
{$ELSE}
function OpenEncryptedFileRawA; external advapi32 name 'OpenEncryptedFileRawA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEncryptedFileRawW: Pointer;

function OpenEncryptedFileRawW;
begin
  GetProcedureAddress(_OpenEncryptedFileRawW, advapi32, 'OpenEncryptedFileRawW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEncryptedFileRawW]
  end;
end;
{$ELSE}
function OpenEncryptedFileRawW; external advapi32 name 'OpenEncryptedFileRawW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEncryptedFileRaw: Pointer;

function OpenEncryptedFileRaw;
begin
  GetProcedureAddress(_OpenEncryptedFileRaw, advapi32, 'OpenEncryptedFileRawW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEncryptedFileRaw]
  end;
end;
{$ELSE}
function OpenEncryptedFileRaw; external advapi32 name 'OpenEncryptedFileRawW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEncryptedFileRaw: Pointer;

function OpenEncryptedFileRaw;
begin
  GetProcedureAddress(_OpenEncryptedFileRaw, advapi32, 'OpenEncryptedFileRawA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEncryptedFileRaw]
  end;
end;
{$ELSE}
function OpenEncryptedFileRaw; external advapi32 name 'OpenEncryptedFileRawA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ReadEncryptedFileRaw: Pointer;

function ReadEncryptedFileRaw;
begin
  GetProcedureAddress(_ReadEncryptedFileRaw, advapi32, 'ReadEncryptedFileRaw');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadEncryptedFileRaw]
  end;
end;
{$ELSE}
function ReadEncryptedFileRaw; external advapi32 name 'ReadEncryptedFileRaw';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteEncryptedFileRaw: Pointer;

function WriteEncryptedFileRaw;
begin
  GetProcedureAddress(_WriteEncryptedFileRaw, advapi32, 'WriteEncryptedFileRaw');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteEncryptedFileRaw]
  end;
end;
{$ELSE}
function WriteEncryptedFileRaw; external advapi32 name 'WriteEncryptedFileRaw';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseEncryptedFileRaw: Pointer;

procedure CloseEncryptedFileRaw;
begin
  GetProcedureAddress(_CloseEncryptedFileRaw, advapi32, 'CloseEncryptedFileRaw');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseEncryptedFileRaw]
  end;
end;
{$ELSE}
procedure CloseEncryptedFileRaw; external advapi32 name 'CloseEncryptedFileRaw';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmpA: Pointer;

function lstrcmpA;
begin
  GetProcedureAddress(_lstrcmpA, kernel32, 'lstrcmpA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmpA]
  end;
end;
{$ELSE}
function lstrcmpA; external kernel32 name 'lstrcmpA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmpW: Pointer;

function lstrcmpW;
begin
  GetProcedureAddress(_lstrcmpW, kernel32, 'lstrcmpW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmpW]
  end;
end;
{$ELSE}
function lstrcmpW; external kernel32 name 'lstrcmpW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmp: Pointer;

function lstrcmp;
begin
  GetProcedureAddress(_lstrcmp, kernel32, 'lstrcmpW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmp]
  end;
end;
{$ELSE}
function lstrcmp; external kernel32 name 'lstrcmpW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmp: Pointer;

function lstrcmp;
begin
  GetProcedureAddress(_lstrcmp, kernel32, 'lstrcmpA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmp]
  end;
end;
{$ELSE}
function lstrcmp; external kernel32 name 'lstrcmpA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmpiA: Pointer;

function lstrcmpiA;
begin
  GetProcedureAddress(_lstrcmpiA, kernel32, 'lstrcmpiA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmpiA]
  end;
end;
{$ELSE}
function lstrcmpiA; external kernel32 name 'lstrcmpiA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmpiW: Pointer;

function lstrcmpiW;
begin
  GetProcedureAddress(_lstrcmpiW, kernel32, 'lstrcmpiW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmpiW]
  end;
end;
{$ELSE}
function lstrcmpiW; external kernel32 name 'lstrcmpiW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmpi: Pointer;

function lstrcmpi;
begin
  GetProcedureAddress(_lstrcmpi, kernel32, 'lstrcmpiW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmpi]
  end;
end;
{$ELSE}
function lstrcmpi; external kernel32 name 'lstrcmpiW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcmpi: Pointer;

function lstrcmpi;
begin
  GetProcedureAddress(_lstrcmpi, kernel32, 'lstrcmpiA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcmpi]
  end;
end;
{$ELSE}
function lstrcmpi; external kernel32 name 'lstrcmpiA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpynA: Pointer;

function lstrcpynA;
begin
  GetProcedureAddress(_lstrcpynA, kernel32, 'lstrcpynA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpynA]
  end;
end;
{$ELSE}
function lstrcpynA; external kernel32 name 'lstrcpynA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpynW: Pointer;

function lstrcpynW;
begin
  GetProcedureAddress(_lstrcpynW, kernel32, 'lstrcpynW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpynW]
  end;
end;
{$ELSE}
function lstrcpynW; external kernel32 name 'lstrcpynW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpyn: Pointer;

function lstrcpyn;
begin
  GetProcedureAddress(_lstrcpyn, kernel32, 'lstrcpynW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpyn]
  end;
end;
{$ELSE}
function lstrcpyn; external kernel32 name 'lstrcpynW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpyn: Pointer;

function lstrcpyn;
begin
  GetProcedureAddress(_lstrcpyn, kernel32, 'lstrcpynA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpyn]
  end;
end;
{$ELSE}
function lstrcpyn; external kernel32 name 'lstrcpynA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpyA: Pointer;

function lstrcpyA;
begin
  GetProcedureAddress(_lstrcpyA, kernel32, 'lstrcpyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpyA]
  end;
end;
{$ELSE}
function lstrcpyA; external kernel32 name 'lstrcpyA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpyW: Pointer;

function lstrcpyW;
begin
  GetProcedureAddress(_lstrcpyW, kernel32, 'lstrcpyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpyW]
  end;
end;
{$ELSE}
function lstrcpyW; external kernel32 name 'lstrcpyW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpy: Pointer;

function lstrcpy;
begin
  GetProcedureAddress(_lstrcpy, kernel32, 'lstrcpyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpy]
  end;
end;
{$ELSE}
function lstrcpy; external kernel32 name 'lstrcpyW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcpy: Pointer;

function lstrcpy;
begin
  GetProcedureAddress(_lstrcpy, kernel32, 'lstrcpyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcpy]
  end;
end;
{$ELSE}
function lstrcpy; external kernel32 name 'lstrcpyA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcatA: Pointer;

function lstrcatA;
begin
  GetProcedureAddress(_lstrcatA, kernel32, 'lstrcatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcatA]
  end;
end;
{$ELSE}
function lstrcatA; external kernel32 name 'lstrcatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcatW: Pointer;

function lstrcatW;
begin
  GetProcedureAddress(_lstrcatW, kernel32, 'lstrcatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcatW]
  end;
end;
{$ELSE}
function lstrcatW; external kernel32 name 'lstrcatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcat: Pointer;

function lstrcat;
begin
  GetProcedureAddress(_lstrcat, kernel32, 'lstrcatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcat]
  end;
end;
{$ELSE}
function lstrcat; external kernel32 name 'lstrcatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrcat: Pointer;

function lstrcat;
begin
  GetProcedureAddress(_lstrcat, kernel32, 'lstrcatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrcat]
  end;
end;
{$ELSE}
function lstrcat; external kernel32 name 'lstrcatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _lstrlenA: Pointer;

function lstrlenA;
begin
  GetProcedureAddress(_lstrlenA, kernel32, 'lstrlenA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrlenA]
  end;
end;
{$ELSE}
function lstrlenA; external kernel32 name 'lstrlenA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _lstrlenW: Pointer;

function lstrlenW;
begin
  GetProcedureAddress(_lstrlenW, kernel32, 'lstrlenW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrlenW]
  end;
end;
{$ELSE}
function lstrlenW; external kernel32 name 'lstrlenW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrlen: Pointer;

function lstrlen;
begin
  GetProcedureAddress(_lstrlen, kernel32, 'lstrlenW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrlen]
  end;
end;
{$ELSE}
function lstrlen; external kernel32 name 'lstrlenW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _lstrlen: Pointer;

function lstrlen;
begin
  GetProcedureAddress(_lstrlen, kernel32, 'lstrlenA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_lstrlen]
  end;
end;
{$ELSE}
function lstrlen; external kernel32 name 'lstrlenA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenFile: Pointer;

function OpenFile;
begin
  GetProcedureAddress(_OpenFile, kernel32, 'OpenFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenFile]
  end;
end;
{$ELSE}
function OpenFile; external kernel32 name 'OpenFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __lopen: Pointer;

function _lopen;
begin
  GetProcedureAddress(__lopen, kernel32, '_lopen');
  asm
    mov esp, ebp
    pop ebp
    jmp [__lopen]
  end;
end;
{$ELSE}
function _lopen; external kernel32 name '_lopen';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __lcreat: Pointer;

function _lcreat;
begin
  GetProcedureAddress(__lcreat, kernel32, '_lcreat');
  asm
    mov esp, ebp
    pop ebp
    jmp [__lcreat]
  end;
end;
{$ELSE}
function _lcreat; external kernel32 name '_lcreat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __lread: Pointer;

function _lread;
begin
  GetProcedureAddress(__lread, kernel32, '_lread');
  asm
    mov esp, ebp
    pop ebp
    jmp [__lread]
  end;
end;
{$ELSE}
function _lread; external kernel32 name '_lread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __lwrite: Pointer;

function _lwrite;
begin
  GetProcedureAddress(__lwrite, kernel32, '_lwrite');
  asm
    mov esp, ebp
    pop ebp
    jmp [__lwrite]
  end;
end;
{$ELSE}
function _lwrite; external kernel32 name '_lwrite';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __hread: Pointer;

function _hread;
begin
  GetProcedureAddress(__hread, kernel32, '_hread');
  asm
    mov esp, ebp
    pop ebp
    jmp [__hread]
  end;
end;
{$ELSE}
function _hread; external kernel32 name '_hread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __hwrite: Pointer;

function _hwrite;
begin
  GetProcedureAddress(__hwrite, kernel32, '_hwrite');
  asm
    mov esp, ebp
    pop ebp
    jmp [__hwrite]
  end;
end;
{$ELSE}
function _hwrite; external kernel32 name '_hwrite';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __lclose: Pointer;

function _lclose;
begin
  GetProcedureAddress(__lclose, kernel32, '_lclose');
  asm
    mov esp, ebp
    pop ebp
    jmp [__lclose]
  end;
end;
{$ELSE}
function _lclose; external kernel32 name '_lclose';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  __llseek: Pointer;

function _llseek;
begin
  GetProcedureAddress(__llseek, kernel32, '_llseek');
  asm
    mov esp, ebp
    pop ebp
    jmp [__llseek]
  end;
end;
{$ELSE}
function _llseek; external kernel32 name '_llseek';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsTextUnicode: Pointer;

function IsTextUnicode;
begin
  GetProcedureAddress(_IsTextUnicode, advapi32, 'IsTextUnicode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsTextUnicode]
  end;
end;
{$ELSE}
function IsTextUnicode; external advapi32 name 'IsTextUnicode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlsAlloc: Pointer;

function FlsAlloc;
begin
  GetProcedureAddress(_FlsAlloc, kernel32, 'FlsAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlsAlloc]
  end;
end;
{$ELSE}
function FlsAlloc; external kernel32 name 'FlsAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlsGetValue: Pointer;

function FlsGetValue;
begin
  GetProcedureAddress(_FlsGetValue, kernel32, 'FlsGetValue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlsGetValue]
  end;
end;
{$ELSE}
function FlsGetValue; external kernel32 name 'FlsGetValue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlsSetValue: Pointer;

function FlsSetValue;
begin
  GetProcedureAddress(_FlsSetValue, kernel32, 'FlsSetValue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlsSetValue]
  end;
end;
{$ELSE}
function FlsSetValue; external kernel32 name 'FlsSetValue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlsFree: Pointer;

function FlsFree;
begin
  GetProcedureAddress(_FlsFree, kernel32, 'FlsFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlsFree]
  end;
end;
{$ELSE}
function FlsFree; external kernel32 name 'FlsFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TlsAlloc: Pointer;

function TlsAlloc;
begin
  GetProcedureAddress(_TlsAlloc, kernel32, 'TlsAlloc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TlsAlloc]
  end;
end;
{$ELSE}
function TlsAlloc; external kernel32 name 'TlsAlloc';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TlsGetValue: Pointer;

function TlsGetValue;
begin
  GetProcedureAddress(_TlsGetValue, kernel32, 'TlsGetValue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TlsGetValue]
  end;
end;
{$ELSE}
function TlsGetValue; external kernel32 name 'TlsGetValue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TlsSetValue: Pointer;

function TlsSetValue;
begin
  GetProcedureAddress(_TlsSetValue, kernel32, 'TlsSetValue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TlsSetValue]
  end;
end;
{$ELSE}
function TlsSetValue; external kernel32 name 'TlsSetValue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TlsFree: Pointer;

function TlsFree;
begin
  GetProcedureAddress(_TlsFree, kernel32, 'TlsFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TlsFree]
  end;
end;
{$ELSE}
function TlsFree; external kernel32 name 'TlsFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SleepEx: Pointer;

function SleepEx;
begin
  GetProcedureAddress(_SleepEx, kernel32, 'SleepEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SleepEx]
  end;
end;
{$ELSE}
function SleepEx; external kernel32 name 'SleepEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitForSingleObjectEx: Pointer;

function WaitForSingleObjectEx;
begin
  GetProcedureAddress(_WaitForSingleObjectEx, kernel32, 'WaitForSingleObjectEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitForSingleObjectEx]
  end;
end;
{$ELSE}
function WaitForSingleObjectEx; external kernel32 name 'WaitForSingleObjectEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitForMultipleObjectsEx: Pointer;

function WaitForMultipleObjectsEx;
begin
  GetProcedureAddress(_WaitForMultipleObjectsEx, kernel32, 'WaitForMultipleObjectsEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitForMultipleObjectsEx]
  end;
end;
{$ELSE}
function WaitForMultipleObjectsEx; external kernel32 name 'WaitForMultipleObjectsEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SignalObjectAndWait: Pointer;

function SignalObjectAndWait;
begin
  GetProcedureAddress(_SignalObjectAndWait, kernel32, 'SignalObjectAndWait');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SignalObjectAndWait]
  end;
end;
{$ELSE}
function SignalObjectAndWait; external kernel32 name 'SignalObjectAndWait';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadFileEx: Pointer;

function ReadFileEx;
begin
  GetProcedureAddress(_ReadFileEx, kernel32, 'ReadFileEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadFileEx]
  end;
end;
{$ELSE}
function ReadFileEx; external kernel32 name 'ReadFileEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteFileEx: Pointer;

function WriteFileEx;
begin
  GetProcedureAddress(_WriteFileEx, kernel32, 'WriteFileEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteFileEx]
  end;
end;
{$ELSE}
function WriteFileEx; external kernel32 name 'WriteFileEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BackupRead: Pointer;

function BackupRead;
begin
  GetProcedureAddress(_BackupRead, kernel32, 'BackupRead');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupRead]
  end;
end;
{$ELSE}
function BackupRead; external kernel32 name 'BackupRead';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BackupSeek: Pointer;

function BackupSeek;
begin
  GetProcedureAddress(_BackupSeek, kernel32, 'BackupSeek');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupSeek]
  end;
end;
{$ELSE}
function BackupSeek; external kernel32 name 'BackupSeek';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BackupWrite: Pointer;

function BackupWrite;
begin
  GetProcedureAddress(_BackupWrite, kernel32, 'BackupWrite');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupWrite]
  end;
end;
{$ELSE}
function BackupWrite; external kernel32 name 'BackupWrite';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadFileScatter: Pointer;

function ReadFileScatter;
begin
  GetProcedureAddress(_ReadFileScatter, kernel32, 'ReadFileScatter');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadFileScatter]
  end;
end;
{$ELSE}
function ReadFileScatter; external kernel32 name 'ReadFileScatter';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteFileGather: Pointer;

function WriteFileGather;
begin
  GetProcedureAddress(_WriteFileGather, kernel32, 'WriteFileGather');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteFileGather]
  end;
end;
{$ELSE}
function WriteFileGather; external kernel32 name 'WriteFileGather';
{$ENDIF DYNAMIC_LINK}

{ MVB:
  The implementation of CreateMutex only interpretes bInitialOwner as True if
  it's ordinal value is 1, all other values are interpreted as False. Delphi
  compiles Longbool(True) as $FFFFFFFF which is consequently interpreted as
  False. Changing the bInitalOwner parameter type to Boolean fixes the problem
  (Boolean(True) = 1) but that would be implementation specific and might break
  in the future, though unlikely. Hence the CreateMutex function here which
  explicitly passes LongBool(1) instead of LongBool(True). }

type
  TCreateMutexA = function (lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: LongBool; lpName: LPCSTR): HANDLE; stdcall;
  TCreateMutexW = function (lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: LongBool; lpName: LPCWSTR): HANDLE; stdcall;

var
  _CreateMutexA: Pointer;
  _CreateMutexW: Pointer;

function CreateMutexA(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCSTR): HANDLE;
begin
  GetProcedureAddress(_CreateMutexA, kernel32, 'CreateMutexA');
  if bInitialOwner then
    Result := TCreateMutexA(_CreateMutexA)(lpMutexAttributes, LongBool(1), lpName)
  else
    Result := TCreateMutexA(_CreateMutexA)(lpMutexAttributes, LongBool(0), lpName)
end;

function CreateMutexW(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCWSTR): HANDLE;
begin
  GetProcedureAddress(_CreateMutexW, kernel32, 'CreateMutexW');
  if bInitialOwner then
    Result := TCreateMutexW(_CreateMutexW)(lpMutexAttributes, LongBool(1), lpName)
  else
    Result := TCreateMutexW(_CreateMutexW)(lpMutexAttributes, LongBool(0), lpName)
end;

{$IFDEF UNICODE}
function CreateMutex(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCWSTR): HANDLE;
begin
  GetProcedureAddress(_CreateMutexW, kernel32, 'CreateMutexW');
  if bInitialOwner then
    Result := TCreateMutexW(_CreateMutexW)(lpMutexAttributes, LongBool(1), lpName)
  else
    Result := TCreateMutexW(_CreateMutexW)(lpMutexAttributes, LongBool(0), lpName)
end;
{$ELSE}
function CreateMutex(lpMutexAttributes: LPSECURITY_ATTRIBUTES; bInitialOwner: BOOL; lpName: LPCSTR): HANDLE;
begin
  GetProcedureAddress(_CreateMutexA, kernel32, 'CreateMutexA');
  if bInitialOwner then
    Result := TCreateMutexA(_CreateMutexA)(lpMutexAttributes, LongBool(1), lpName)
  else
    Result := TCreateMutexA(_CreateMutexA)(lpMutexAttributes, LongBool(0), lpName)
end;
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _OpenMutexA: Pointer;

function OpenMutexA;
begin
  GetProcedureAddress(_OpenMutexA, kernel32, 'OpenMutexA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenMutexA]
  end;
end;
{$ELSE}
function OpenMutexA; external kernel32 name 'OpenMutexA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenMutexW: Pointer;

function OpenMutexW;
begin
  GetProcedureAddress(_OpenMutexW, kernel32, 'OpenMutexW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenMutexW]
  end;
end;
{$ELSE}
function OpenMutexW; external kernel32 name 'OpenMutexW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenMutex: Pointer;

function OpenMutex;
begin
  GetProcedureAddress(_OpenMutex, kernel32, 'OpenMutexW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenMutex]
  end;
end;
{$ELSE}
function OpenMutex; external kernel32 name 'OpenMutexW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenMutex: Pointer;

function OpenMutex;
begin
  GetProcedureAddress(_OpenMutex, kernel32, 'OpenMutexA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenMutex]
  end;
end;
{$ELSE}
function OpenMutex; external kernel32 name 'OpenMutexA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEventA: Pointer;

function CreateEventA;
begin
  GetProcedureAddress(_CreateEventA, kernel32, 'CreateEventA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEventA]
  end;
end;
{$ELSE}
function CreateEventA; external kernel32 name 'CreateEventA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEventW: Pointer;

function CreateEventW;
begin
  GetProcedureAddress(_CreateEventW, kernel32, 'CreateEventW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEventW]
  end;
end;
{$ELSE}
function CreateEventW; external kernel32 name 'CreateEventW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEvent: Pointer;

function CreateEvent;
begin
  GetProcedureAddress(_CreateEvent, kernel32, 'CreateEventW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEvent]
  end;
end;
{$ELSE}
function CreateEvent; external kernel32 name 'CreateEventW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateEvent: Pointer;

function CreateEvent;
begin
  GetProcedureAddress(_CreateEvent, kernel32, 'CreateEventA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateEvent]
  end;
end;
{$ELSE}
function CreateEvent; external kernel32 name 'CreateEventA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEventA: Pointer;

function OpenEventA;
begin
  GetProcedureAddress(_OpenEventA, kernel32, 'OpenEventA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEventA]
  end;
end;
{$ELSE}
function OpenEventA; external kernel32 name 'OpenEventA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEventW: Pointer;

function OpenEventW;
begin
  GetProcedureAddress(_OpenEventW, kernel32, 'OpenEventW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEventW]
  end;
end;
{$ELSE}
function OpenEventW; external kernel32 name 'OpenEventW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEvent: Pointer;

function OpenEvent;
begin
  GetProcedureAddress(_OpenEvent, kernel32, 'OpenEventW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEvent]
  end;
end;
{$ELSE}
function OpenEvent; external kernel32 name 'OpenEventW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEvent: Pointer;

function OpenEvent;
begin
  GetProcedureAddress(_OpenEvent, kernel32, 'OpenEventA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEvent]
  end;
end;
{$ELSE}
function OpenEvent; external kernel32 name 'OpenEventA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateSemaphoreA: Pointer;

function CreateSemaphoreA;
begin
  GetProcedureAddress(_CreateSemaphoreA, kernel32, 'CreateSemaphoreA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateSemaphoreA]
  end;
end;
{$ELSE}
function CreateSemaphoreA; external kernel32 name 'CreateSemaphoreA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateSemaphoreW: Pointer;

function CreateSemaphoreW;
begin
  GetProcedureAddress(_CreateSemaphoreW, kernel32, 'CreateSemaphoreW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateSemaphoreW]
  end;
end;
{$ELSE}
function CreateSemaphoreW; external kernel32 name 'CreateSemaphoreW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateSemaphore: Pointer;

function CreateSemaphore;
begin
  GetProcedureAddress(_CreateSemaphore, kernel32, 'CreateSemaphoreW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateSemaphore]
  end;
end;
{$ELSE}
function CreateSemaphore; external kernel32 name 'CreateSemaphoreW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateSemaphore: Pointer;

function CreateSemaphore;
begin
  GetProcedureAddress(_CreateSemaphore, kernel32, 'CreateSemaphoreA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateSemaphore]
  end;
end;
{$ELSE}
function CreateSemaphore; external kernel32 name 'CreateSemaphoreA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenSemaphoreA: Pointer;

function OpenSemaphoreA;
begin
  GetProcedureAddress(_OpenSemaphoreA, kernel32, 'OpenSemaphoreA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenSemaphoreA]
  end;
end;
{$ELSE}
function OpenSemaphoreA; external kernel32 name 'OpenSemaphoreA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenSemaphoreW: Pointer;

function OpenSemaphoreW;
begin
  GetProcedureAddress(_OpenSemaphoreW, kernel32, 'OpenSemaphoreW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenSemaphoreW]
  end;
end;
{$ELSE}
function OpenSemaphoreW; external kernel32 name 'OpenSemaphoreW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenSemaphore: Pointer;

function OpenSemaphore;
begin
  GetProcedureAddress(_OpenSemaphore, kernel32, 'OpenSemaphoreW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenSemaphore]
  end;
end;
{$ELSE}
function OpenSemaphore; external kernel32 name 'OpenSemaphoreW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenSemaphore: Pointer;

function OpenSemaphore;
begin
  GetProcedureAddress(_OpenSemaphore, kernel32, 'OpenSemaphoreA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenSemaphore]
  end;
end;
{$ELSE}
function OpenSemaphore; external kernel32 name 'OpenSemaphoreA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWaitableTimerA: Pointer;

function CreateWaitableTimerA;
begin
  GetProcedureAddress(_CreateWaitableTimerA, kernel32, 'CreateWaitableTimerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWaitableTimerA]
  end;
end;
{$ELSE}
function CreateWaitableTimerA; external kernel32 name 'CreateWaitableTimerA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWaitableTimerW: Pointer;

function CreateWaitableTimerW;
begin
  GetProcedureAddress(_CreateWaitableTimerW, kernel32, 'CreateWaitableTimerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWaitableTimerW]
  end;
end;
{$ELSE}
function CreateWaitableTimerW; external kernel32 name 'CreateWaitableTimerW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWaitableTimer: Pointer;

function CreateWaitableTimer;
begin
  GetProcedureAddress(_CreateWaitableTimer, kernel32, 'CreateWaitableTimerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWaitableTimer]
  end;
end;
{$ELSE}
function CreateWaitableTimer; external kernel32 name 'CreateWaitableTimerW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWaitableTimer: Pointer;

function CreateWaitableTimer;
begin
  GetProcedureAddress(_CreateWaitableTimer, kernel32, 'CreateWaitableTimerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWaitableTimer]
  end;
end;
{$ELSE}
function CreateWaitableTimer; external kernel32 name 'CreateWaitableTimerA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWaitableTimerA: Pointer;

function OpenWaitableTimerA;
begin
  GetProcedureAddress(_OpenWaitableTimerA, kernel32, 'OpenWaitableTimerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWaitableTimerA]
  end;
end;
{$ELSE}
function OpenWaitableTimerA; external kernel32 name 'OpenWaitableTimerA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWaitableTimerW: Pointer;

function OpenWaitableTimerW;
begin
  GetProcedureAddress(_OpenWaitableTimerW, kernel32, 'OpenWaitableTimerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWaitableTimerW]
  end;
end;
{$ELSE}
function OpenWaitableTimerW; external kernel32 name 'OpenWaitableTimerW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWaitableTimer: Pointer;

function OpenWaitableTimer;
begin
  GetProcedureAddress(_OpenWaitableTimer, kernel32, 'OpenWaitableTimerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWaitableTimer]
  end;
end;
{$ELSE}
function OpenWaitableTimer; external kernel32 name 'OpenWaitableTimerW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWaitableTimer: Pointer;

function OpenWaitableTimer;
begin
  GetProcedureAddress(_OpenWaitableTimer, kernel32, 'OpenWaitableTimerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWaitableTimer]
  end;
end;
{$ELSE}
function OpenWaitableTimer; external kernel32 name 'OpenWaitableTimerA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetWaitableTimer: Pointer;

function SetWaitableTimer;
begin
  GetProcedureAddress(_SetWaitableTimer, kernel32, 'SetWaitableTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWaitableTimer]
  end;
end;
{$ELSE}
function SetWaitableTimer; external kernel32 name 'SetWaitableTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CancelWaitableTimer: Pointer;

function CancelWaitableTimer;
begin
  GetProcedureAddress(_CancelWaitableTimer, kernel32, 'CancelWaitableTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CancelWaitableTimer]
  end;
end;
{$ELSE}
function CancelWaitableTimer; external kernel32 name 'CancelWaitableTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFileMappingA: Pointer;

function CreateFileMappingA;
begin
  GetProcedureAddress(_CreateFileMappingA, kernel32, 'CreateFileMappingA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFileMappingA]
  end;
end;
{$ELSE}
function CreateFileMappingA; external kernel32 name 'CreateFileMappingA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFileMappingW: Pointer;

function CreateFileMappingW;
begin
  GetProcedureAddress(_CreateFileMappingW, kernel32, 'CreateFileMappingW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFileMappingW]
  end;
end;
{$ELSE}
function CreateFileMappingW; external kernel32 name 'CreateFileMappingW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFileMapping: Pointer;

function CreateFileMapping;
begin
  GetProcedureAddress(_CreateFileMapping, kernel32, 'CreateFileMappingW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFileMapping]
  end;
end;
{$ELSE}
function CreateFileMapping; external kernel32 name 'CreateFileMappingW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFileMapping: Pointer;

function CreateFileMapping;
begin
  GetProcedureAddress(_CreateFileMapping, kernel32, 'CreateFileMappingA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFileMapping]
  end;
end;
{$ELSE}
function CreateFileMapping; external kernel32 name 'CreateFileMappingA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenFileMappingA: Pointer;

function OpenFileMappingA;
begin
  GetProcedureAddress(_OpenFileMappingA, kernel32, 'OpenFileMappingA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenFileMappingA]
  end;
end;
{$ELSE}
function OpenFileMappingA; external kernel32 name 'OpenFileMappingA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenFileMappingW: Pointer;

function OpenFileMappingW;
begin
  GetProcedureAddress(_OpenFileMappingW, kernel32, 'OpenFileMappingW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenFileMappingW]
  end;
end;
{$ELSE}
function OpenFileMappingW; external kernel32 name 'OpenFileMappingW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenFileMapping: Pointer;

function OpenFileMapping;
begin
  GetProcedureAddress(_OpenFileMapping, kernel32, 'OpenFileMappingW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenFileMapping]
  end;
end;
{$ELSE}
function OpenFileMapping; external kernel32 name 'OpenFileMappingW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenFileMapping: Pointer;

function OpenFileMapping;
begin
  GetProcedureAddress(_OpenFileMapping, kernel32, 'OpenFileMappingA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenFileMapping]
  end;
end;
{$ELSE}
function OpenFileMapping; external kernel32 name 'OpenFileMappingA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogicalDriveStringsA: Pointer;

function GetLogicalDriveStringsA;
begin
  GetProcedureAddress(_GetLogicalDriveStringsA, kernel32, 'GetLogicalDriveStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogicalDriveStringsA]
  end;
end;
{$ELSE}
function GetLogicalDriveStringsA; external kernel32 name 'GetLogicalDriveStringsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogicalDriveStringsW: Pointer;

function GetLogicalDriveStringsW;
begin
  GetProcedureAddress(_GetLogicalDriveStringsW, kernel32, 'GetLogicalDriveStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogicalDriveStringsW]
  end;
end;
{$ELSE}
function GetLogicalDriveStringsW; external kernel32 name 'GetLogicalDriveStringsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogicalDriveStrings: Pointer;

function GetLogicalDriveStrings;
begin
  GetProcedureAddress(_GetLogicalDriveStrings, kernel32, 'GetLogicalDriveStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogicalDriveStrings]
  end;
end;
{$ELSE}
function GetLogicalDriveStrings; external kernel32 name 'GetLogicalDriveStringsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogicalDriveStrings: Pointer;

function GetLogicalDriveStrings;
begin
  GetProcedureAddress(_GetLogicalDriveStrings, kernel32, 'GetLogicalDriveStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogicalDriveStrings]
  end;
end;
{$ELSE}
function GetLogicalDriveStrings; external kernel32 name 'GetLogicalDriveStringsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMemResNotification: Pointer;

function CreateMemoryResourceNotification;
begin
  GetProcedureAddress(_CreateMemResNotification, kernel32, 'CreateMemoryResourceNotification');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMemResNotification]
  end;
end;
{$ELSE}
function CreateMemoryResourceNotification; external kernel32 name 'CreateMemoryResourceNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueryMemoryResourceNotification: Pointer;

function QueryMemoryResourceNotification;
begin
  GetProcedureAddress(_QueryMemoryResourceNotification, kernel32, 'QueryMemoryResourceNotification');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryMemoryResourceNotification]
  end;
end;
{$ELSE}
function QueryMemoryResourceNotification; external kernel32 name 'QueryMemoryResourceNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibraryA: Pointer;

function LoadLibraryA;
begin
  GetProcedureAddress(_LoadLibraryA, kernel32, 'LoadLibraryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryA]
  end;
end;
{$ELSE}
function LoadLibraryA; external kernel32 name 'LoadLibraryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibraryW: Pointer;

function LoadLibraryW;
begin
  GetProcedureAddress(_LoadLibraryW, kernel32, 'LoadLibraryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryW]
  end;
end;
{$ELSE}
function LoadLibraryW; external kernel32 name 'LoadLibraryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibrary: Pointer;

function LoadLibrary;
begin
  GetProcedureAddress(_LoadLibrary, kernel32, 'LoadLibraryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibrary]
  end;
end;
{$ELSE}
function LoadLibrary; external kernel32 name 'LoadLibraryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibrary: Pointer;

function LoadLibrary;
begin
  GetProcedureAddress(_LoadLibrary, kernel32, 'LoadLibraryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibrary]
  end;
end;
{$ELSE}
function LoadLibrary; external kernel32 name 'LoadLibraryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibraryExA: Pointer;

function LoadLibraryExA;
begin
  GetProcedureAddress(_LoadLibraryExA, kernel32, 'LoadLibraryExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryExA]
  end;
end;
{$ELSE}
function LoadLibraryExA; external kernel32 name 'LoadLibraryExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibraryExW: Pointer;

function LoadLibraryExW;
begin
  GetProcedureAddress(_LoadLibraryExW, kernel32, 'LoadLibraryExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryExW]
  end;
end;
{$ELSE}
function LoadLibraryExW; external kernel32 name 'LoadLibraryExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibraryEx: Pointer;

function LoadLibraryEx;
begin
  GetProcedureAddress(_LoadLibraryEx, kernel32, 'LoadLibraryExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryEx]
  end;
end;
{$ELSE}
function LoadLibraryEx; external kernel32 name 'LoadLibraryExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadLibraryEx: Pointer;

function LoadLibraryEx;
begin
  GetProcedureAddress(_LoadLibraryEx, kernel32, 'LoadLibraryExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadLibraryEx]
  end;
end;
{$ELSE}
function LoadLibraryEx; external kernel32 name 'LoadLibraryExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleFileNameA: Pointer;

function GetModuleFileNameA;
begin
  GetProcedureAddress(_GetModuleFileNameA, kernel32, 'GetModuleFileNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleFileNameA]
  end;
end;
{$ELSE}
function GetModuleFileNameA; external kernel32 name 'GetModuleFileNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleFileNameW: Pointer;

function GetModuleFileNameW;
begin
  GetProcedureAddress(_GetModuleFileNameW, kernel32, 'GetModuleFileNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleFileNameW]
  end;
end;
{$ELSE}
function GetModuleFileNameW; external kernel32 name 'GetModuleFileNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleFileName: Pointer;

function GetModuleFileName;
begin
  GetProcedureAddress(_GetModuleFileName, kernel32, 'GetModuleFileNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleFileName]
  end;
end;
{$ELSE}
function GetModuleFileName; external kernel32 name 'GetModuleFileNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleFileName: Pointer;

function GetModuleFileName;
begin
  GetProcedureAddress(_GetModuleFileName, kernel32, 'GetModuleFileNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleFileName]
  end;
end;
{$ELSE}
function GetModuleFileName; external kernel32 name 'GetModuleFileNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandleA: Pointer;

function GetModuleHandleA;
begin
  GetProcedureAddress(_GetModuleHandleA, kernel32, 'GetModuleHandleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandleA]
  end;
end;
{$ELSE}
function GetModuleHandleA; external kernel32 name 'GetModuleHandleA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandleW: Pointer;

function GetModuleHandleW;
begin
  GetProcedureAddress(_GetModuleHandleW, kernel32, 'GetModuleHandleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandleW]
  end;
end;
{$ELSE}
function GetModuleHandleW; external kernel32 name 'GetModuleHandleW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandle: Pointer;

function GetModuleHandle;
begin
  GetProcedureAddress(_GetModuleHandle, kernel32, 'GetModuleHandleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandle]
  end;
end;
{$ELSE}
function GetModuleHandle; external kernel32 name 'GetModuleHandleW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandle: Pointer;

function GetModuleHandle;
begin
  GetProcedureAddress(_GetModuleHandle, kernel32, 'GetModuleHandleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandle]
  end;
end;
{$ELSE}
function GetModuleHandle; external kernel32 name 'GetModuleHandleA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessA: Pointer;

function CreateProcessA;
begin
  GetProcedureAddress(_CreateProcessA, kernel32, 'CreateProcessA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessA]
  end;
end;
{$ELSE}
function CreateProcessA; external kernel32 name 'CreateProcessA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessW: Pointer;

function CreateProcessW;
begin
  GetProcedureAddress(_CreateProcessW, kernel32, 'CreateProcessW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessW]
  end;
end;
{$ELSE}
function CreateProcessW; external kernel32 name 'CreateProcessW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcess: Pointer;

function CreateProcess;
begin
  GetProcedureAddress(_CreateProcess, kernel32, 'CreateProcessW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcess]
  end;
end;
{$ELSE}
function CreateProcess; external kernel32 name 'CreateProcessW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcess: Pointer;

function CreateProcess;
begin
  GetProcedureAddress(_CreateProcess, kernel32, 'CreateProcessA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcess]
  end;
end;
{$ELSE}
function CreateProcess; external kernel32 name 'CreateProcessA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandleExA: Pointer;

function GetModuleHandleExA;
begin
  GetProcedureAddress(_GetModuleHandleExA, kernel32, 'GetModuleHandleExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandleExA]
  end;
end;
{$ELSE}
function GetModuleHandleExA; external kernel32 name 'GetModuleHandleExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandleExW: Pointer;

function GetModuleHandleExW;
begin
  GetProcedureAddress(_GetModuleHandleExW, kernel32, 'GetModuleHandleExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandleExW]
  end;
end;
{$ELSE}
function GetModuleHandleExW; external kernel32 name 'GetModuleHandleExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandleEx: Pointer;

function GetModuleHandleEx;
begin
  GetProcedureAddress(_GetModuleHandleEx, kernel32, 'GetModuleHandleExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandleEx]
  end;
end;
{$ELSE}
function GetModuleHandleEx; external kernel32 name 'GetModuleHandleExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetModuleHandleEx: Pointer;

function GetModuleHandleEx;
begin
  GetProcedureAddress(_GetModuleHandleEx, kernel32, 'GetModuleHandleExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetModuleHandleEx]
  end;
end;
{$ELSE}
function GetModuleHandleEx; external kernel32 name 'GetModuleHandleExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _NeedCurrentDirectoryForExePathA: Pointer;

function NeedCurrentDirectoryForExePathA;
begin
  GetProcedureAddress(_NeedCurrentDirectoryForExePathA, kernel32, 'NeedCurrentDirectoryForExePathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NeedCurrentDirectoryForExePathA]
  end;
end;
{$ELSE}
function NeedCurrentDirectoryForExePathA; external kernel32 name 'NeedCurrentDirectoryForExePathA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NeedCurrentDirectoryForExePathW: Pointer;

function NeedCurrentDirectoryForExePathW;
begin
  GetProcedureAddress(_NeedCurrentDirectoryForExePathW, kernel32, 'NeedCurrentDirectoryForExePathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NeedCurrentDirectoryForExePathW]
  end;
end;
{$ELSE}
function NeedCurrentDirectoryForExePathW; external kernel32 name 'NeedCurrentDirectoryForExePathW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _NeedCurrentDirectoryForExePath: Pointer;

function NeedCurrentDirectoryForExePath;
begin
  GetProcedureAddress(_NeedCurrentDirectoryForExePath, kernel32, 'NeedCurrentDirectoryForExePathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NeedCurrentDirectoryForExePath]
  end;
end;
{$ELSE}
function NeedCurrentDirectoryForExePath; external kernel32 name 'NeedCurrentDirectoryForExePathW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _NeedCurrentDirectoryForExePath: Pointer;

function NeedCurrentDirectoryForExePath;
begin
  GetProcedureAddress(_NeedCurrentDirectoryForExePath, kernel32, 'NeedCurrentDirectoryForExePathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NeedCurrentDirectoryForExePath]
  end;
end;
{$ELSE}
function NeedCurrentDirectoryForExePath; external kernel32 name 'NeedCurrentDirectoryForExePathA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _SetProcessShutdownParameters: Pointer;

function SetProcessShutdownParameters;
begin
  GetProcedureAddress(_SetProcessShutdownParameters, kernel32, 'SetProcessShutdownParameters');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessShutdownParameters]
  end;
end;
{$ELSE}
function SetProcessShutdownParameters; external kernel32 name 'SetProcessShutdownParameters';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessShutdownParameters: Pointer;

function GetProcessShutdownParameters;
begin
  GetProcedureAddress(_GetProcessShutdownParameters, kernel32, 'GetProcessShutdownParameters');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessShutdownParameters]
  end;
end;
{$ELSE}
function GetProcessShutdownParameters; external kernel32 name 'GetProcessShutdownParameters';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessVersion: Pointer;

function GetProcessVersion;
begin
  GetProcedureAddress(_GetProcessVersion, kernel32, 'GetProcessVersion');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessVersion]
  end;
end;
{$ELSE}
function GetProcessVersion; external kernel32 name 'GetProcessVersion';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FatalAppExitA: Pointer;

procedure FatalAppExitA;
begin
  GetProcedureAddress(_FatalAppExitA, kernel32, 'FatalAppExitA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FatalAppExitA]
  end;
end;
{$ELSE}
procedure FatalAppExitA; external kernel32 name 'FatalAppExitA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FatalAppExitW: Pointer;

procedure FatalAppExitW;
begin
  GetProcedureAddress(_FatalAppExitW, kernel32, 'FatalAppExitW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FatalAppExitW]
  end;
end;
{$ELSE}
procedure FatalAppExitW; external kernel32 name 'FatalAppExitW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FatalAppExit: Pointer;

procedure FatalAppExit;
begin
  GetProcedureAddress(_FatalAppExit, kernel32, 'FatalAppExitW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FatalAppExit]
  end;
end;
{$ELSE}
procedure FatalAppExit; external kernel32 name 'FatalAppExitW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FatalAppExit: Pointer;

procedure FatalAppExit;
begin
  GetProcedureAddress(_FatalAppExit, kernel32, 'FatalAppExitA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FatalAppExit]
  end;
end;
{$ELSE}
procedure FatalAppExit; external kernel32 name 'FatalAppExitA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetStartupInfoA: Pointer;

procedure GetStartupInfoA;
begin
  GetProcedureAddress(_GetStartupInfoA, kernel32, 'GetStartupInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStartupInfoA]
  end;
end;
{$ELSE}
procedure GetStartupInfoA; external kernel32 name 'GetStartupInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetStartupInfoW: Pointer;

procedure GetStartupInfoW;
begin
  GetProcedureAddress(_GetStartupInfoW, kernel32, 'GetStartupInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStartupInfoW]
  end;
end;
{$ELSE}
procedure GetStartupInfoW; external kernel32 name 'GetStartupInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetStartupInfo: Pointer;

procedure GetStartupInfo;
begin
  GetProcedureAddress(_GetStartupInfo, kernel32, 'GetStartupInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStartupInfo]
  end;
end;
{$ELSE}
procedure GetStartupInfo; external kernel32 name 'GetStartupInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetStartupInfo: Pointer;

procedure GetStartupInfo;
begin
  GetProcedureAddress(_GetStartupInfo, kernel32, 'GetStartupInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetStartupInfo]
  end;
end;
{$ELSE}
procedure GetStartupInfo; external kernel32 name 'GetStartupInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommandLineA: Pointer;

function GetCommandLineA;
begin
  GetProcedureAddress(_GetCommandLineA, kernel32, 'GetCommandLineA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommandLineA]
  end;
end;
{$ELSE}
function GetCommandLineA; external kernel32 name 'GetCommandLineA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommandLineW: Pointer;

function GetCommandLineW;
begin
  GetProcedureAddress(_GetCommandLineW, kernel32, 'GetCommandLineW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommandLineW]
  end;
end;
{$ELSE}
function GetCommandLineW; external kernel32 name 'GetCommandLineW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommandLine: Pointer;

function GetCommandLine;
begin
  GetProcedureAddress(_GetCommandLine, kernel32, 'GetCommandLineW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommandLine]
  end;
end;
{$ELSE}
function GetCommandLine; external kernel32 name 'GetCommandLineW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCommandLine: Pointer;

function GetCommandLine;
begin
  GetProcedureAddress(_GetCommandLine, kernel32, 'GetCommandLineA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCommandLine]
  end;
end;
{$ELSE}
function GetCommandLine; external kernel32 name 'GetCommandLineA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentVariableA: Pointer;

function GetEnvironmentVariableA;
begin
  GetProcedureAddress(_GetEnvironmentVariableA, kernel32, 'GetEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentVariableA]
  end;
end;
{$ELSE}
function GetEnvironmentVariableA; external kernel32 name 'GetEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentVariableW: Pointer;

function GetEnvironmentVariableW;
begin
  GetProcedureAddress(_GetEnvironmentVariableW, kernel32, 'GetEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentVariableW]
  end;
end;
{$ELSE}
function GetEnvironmentVariableW; external kernel32 name 'GetEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentVariable: Pointer;

function GetEnvironmentVariable;
begin
  GetProcedureAddress(_GetEnvironmentVariable, kernel32, 'GetEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentVariable]
  end;
end;
{$ELSE}
function GetEnvironmentVariable; external kernel32 name 'GetEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetEnvironmentVariable: Pointer;

function GetEnvironmentVariable;
begin
  GetProcedureAddress(_GetEnvironmentVariable, kernel32, 'GetEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEnvironmentVariable]
  end;
end;
{$ELSE}
function GetEnvironmentVariable; external kernel32 name 'GetEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentVariableA: Pointer;

function SetEnvironmentVariableA;
begin
  GetProcedureAddress(_SetEnvironmentVariableA, kernel32, 'SetEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentVariableA]
  end;
end;
{$ELSE}
function SetEnvironmentVariableA; external kernel32 name 'SetEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentVariableW: Pointer;

function SetEnvironmentVariableW;
begin
  GetProcedureAddress(_SetEnvironmentVariableW, kernel32, 'SetEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentVariableW]
  end;
end;
{$ELSE}
function SetEnvironmentVariableW; external kernel32 name 'SetEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentVariable: Pointer;

function SetEnvironmentVariable;
begin
  GetProcedureAddress(_SetEnvironmentVariable, kernel32, 'SetEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentVariable]
  end;
end;
{$ELSE}
function SetEnvironmentVariable; external kernel32 name 'SetEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetEnvironmentVariable: Pointer;

function SetEnvironmentVariable;
begin
  GetProcedureAddress(_SetEnvironmentVariable, kernel32, 'SetEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetEnvironmentVariable]
  end;
end;
{$ELSE}
function SetEnvironmentVariable; external kernel32 name 'SetEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ExpandEnvironmentStringsA: Pointer;

function ExpandEnvironmentStringsA;
begin
  GetProcedureAddress(_ExpandEnvironmentStringsA, kernel32, 'ExpandEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExpandEnvironmentStringsA]
  end;
end;
{$ELSE}
function ExpandEnvironmentStringsA; external kernel32 name 'ExpandEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExpandEnvironmentStringsW: Pointer;

function ExpandEnvironmentStringsW;
begin
  GetProcedureAddress(_ExpandEnvironmentStringsW, kernel32, 'ExpandEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExpandEnvironmentStringsW]
  end;
end;
{$ELSE}
function ExpandEnvironmentStringsW; external kernel32 name 'ExpandEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ExpandEnvironmentStrings: Pointer;

function ExpandEnvironmentStrings;
begin
  GetProcedureAddress(_ExpandEnvironmentStrings, kernel32, 'ExpandEnvironmentStringsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExpandEnvironmentStrings]
  end;
end;
{$ELSE}
function ExpandEnvironmentStrings; external kernel32 name 'ExpandEnvironmentStringsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ExpandEnvironmentStrings: Pointer;

function ExpandEnvironmentStrings;
begin
  GetProcedureAddress(_ExpandEnvironmentStrings, kernel32, 'ExpandEnvironmentStringsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExpandEnvironmentStrings]
  end;
end;
{$ELSE}
function ExpandEnvironmentStrings; external kernel32 name 'ExpandEnvironmentStringsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetFirmwareEnvironmentVariableA: Pointer;

function GetFirmwareEnvironmentVariableA;
begin
  GetProcedureAddress(_GetFirmwareEnvironmentVariableA, kernel32, 'GetFirmwareEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFirmwareEnvironmentVariableA]
  end;
end;
{$ELSE}
function GetFirmwareEnvironmentVariableA; external kernel32 name 'GetFirmwareEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFirmwareEnvironmentVariableW: Pointer;

function GetFirmwareEnvironmentVariableW;
begin
  GetProcedureAddress(_GetFirmwareEnvironmentVariableW, kernel32, 'GetFirmwareEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFirmwareEnvironmentVariableW]
  end;
end;
{$ELSE}
function GetFirmwareEnvironmentVariableW; external kernel32 name 'GetFirmwareEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFirmwareEnvironmentVariable: Pointer;

function GetFirmwareEnvironmentVariable;
begin
  GetProcedureAddress(_GetFirmwareEnvironmentVariable, kernel32, 'GetFirmwareEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFirmwareEnvironmentVariable]
  end;
end;
{$ELSE}
function GetFirmwareEnvironmentVariable; external kernel32 name 'GetFirmwareEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFirmwareEnvironmentVariable: Pointer;

function GetFirmwareEnvironmentVariable;
begin
  GetProcedureAddress(_GetFirmwareEnvironmentVariable, kernel32, 'GetFirmwareEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFirmwareEnvironmentVariable]
  end;
end;
{$ELSE}
function GetFirmwareEnvironmentVariable; external kernel32 name 'GetFirmwareEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetFirmwareEnvironmentVariableA: Pointer;

function SetFirmwareEnvironmentVariableA;
begin
  GetProcedureAddress(_SetFirmwareEnvironmentVariableA, kernel32, 'SetFirmwareEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFirmwareEnvironmentVariableA]
  end;
end;
{$ELSE}
function SetFirmwareEnvironmentVariableA; external kernel32 name 'SetFirmwareEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFirmwareEnvironmentVariableW: Pointer;

function SetFirmwareEnvironmentVariableW;
begin
  GetProcedureAddress(_SetFirmwareEnvironmentVariableW, kernel32, 'SetFirmwareEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFirmwareEnvironmentVariableW]
  end;
end;
{$ELSE}
function SetFirmwareEnvironmentVariableW; external kernel32 name 'SetFirmwareEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFirmwareEnvironmentVariable: Pointer;

function SetFirmwareEnvironmentVariable;
begin
  GetProcedureAddress(_SetFirmwareEnvironmentVariable, kernel32, 'SetFirmwareEnvironmentVariableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFirmwareEnvironmentVariable]
  end;
end;
{$ELSE}
function SetFirmwareEnvironmentVariable; external kernel32 name 'SetFirmwareEnvironmentVariableW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFirmwareEnvironmentVariable: Pointer;

function SetFirmwareEnvironmentVariable;
begin
  GetProcedureAddress(_SetFirmwareEnvironmentVariable, kernel32, 'SetFirmwareEnvironmentVariableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFirmwareEnvironmentVariable]
  end;
end;
{$ELSE}
function SetFirmwareEnvironmentVariable; external kernel32 name 'SetFirmwareEnvironmentVariableA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _OutputDebugStringA: Pointer;

procedure OutputDebugStringA;
begin
  GetProcedureAddress(_OutputDebugStringA, kernel32, 'OutputDebugStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OutputDebugStringA]
  end;
end;
{$ELSE}
procedure OutputDebugStringA; external kernel32 name 'OutputDebugStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OutputDebugStringW: Pointer;

procedure OutputDebugStringW;
begin
  GetProcedureAddress(_OutputDebugStringW, kernel32, 'OutputDebugStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OutputDebugStringW]
  end;
end;
{$ELSE}
procedure OutputDebugStringW; external kernel32 name 'OutputDebugStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OutputDebugString: Pointer;

procedure OutputDebugString;
begin
  GetProcedureAddress(_OutputDebugString, kernel32, 'OutputDebugStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OutputDebugString]
  end;
end;
{$ELSE}
procedure OutputDebugString; external kernel32 name 'OutputDebugStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OutputDebugString: Pointer;

procedure OutputDebugString;
begin
  GetProcedureAddress(_OutputDebugString, kernel32, 'OutputDebugStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OutputDebugString]
  end;
end;
{$ELSE}
procedure OutputDebugString; external kernel32 name 'OutputDebugStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindResourceA: Pointer;

function FindResourceA;
begin
  GetProcedureAddress(_FindResourceA, kernel32, 'FindResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResourceA]
  end;
end;
{$ELSE}
function FindResourceA; external kernel32 name 'FindResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindResourceW: Pointer;

function FindResourceW;
begin
  GetProcedureAddress(_FindResourceW, kernel32, 'FindResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResourceW]
  end;
end;
{$ELSE}
function FindResourceW; external kernel32 name 'FindResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindResource: Pointer;

function FindResource;
begin
  GetProcedureAddress(_FindResource, kernel32, 'FindResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResource]
  end;
end;
{$ELSE}
function FindResource; external kernel32 name 'FindResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindResource: Pointer;

function FindResource;
begin
  GetProcedureAddress(_FindResource, kernel32, 'FindResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResource]
  end;
end;
{$ELSE}
function FindResource; external kernel32 name 'FindResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindResourceExA: Pointer;

function FindResourceExA;
begin
  GetProcedureAddress(_FindResourceExA, kernel32, 'FindResourceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResourceExA]
  end;
end;
{$ELSE}
function FindResourceExA; external kernel32 name 'FindResourceExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindResourceExW: Pointer;

function FindResourceExW;
begin
  GetProcedureAddress(_FindResourceExW, kernel32, 'FindResourceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResourceExW]
  end;
end;
{$ELSE}
function FindResourceExW; external kernel32 name 'FindResourceExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindResourceEx: Pointer;

function FindResourceEx;
begin
  GetProcedureAddress(_FindResourceEx, kernel32, 'FindResourceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResourceEx]
  end;
end;
{$ELSE}
function FindResourceEx; external kernel32 name 'FindResourceExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindResourceEx: Pointer;

function FindResourceEx;
begin
  GetProcedureAddress(_FindResourceEx, kernel32, 'FindResourceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindResourceEx]
  end;
end;
{$ELSE}
function FindResourceEx; external kernel32 name 'FindResourceExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceTypesA: Pointer;

function EnumResourceTypesA;
begin
  GetProcedureAddress(_EnumResourceTypesA, kernel32, 'EnumResourceTypesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceTypesA]
  end;
end;
{$ELSE}
function EnumResourceTypesA; external kernel32 name 'EnumResourceTypesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceTypesW: Pointer;

function EnumResourceTypesW;
begin
  GetProcedureAddress(_EnumResourceTypesW, kernel32, 'EnumResourceTypesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceTypesW]
  end;
end;
{$ELSE}
function EnumResourceTypesW; external kernel32 name 'EnumResourceTypesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceTypes: Pointer;

function EnumResourceTypes;
begin
  GetProcedureAddress(_EnumResourceTypes, kernel32, 'EnumResourceTypesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceTypes]
  end;
end;
{$ELSE}
function EnumResourceTypes; external kernel32 name 'EnumResourceTypesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceTypes: Pointer;

function EnumResourceTypes;
begin
  GetProcedureAddress(_EnumResourceTypes, kernel32, 'EnumResourceTypesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceTypes]
  end;
end;
{$ELSE}
function EnumResourceTypes; external kernel32 name 'EnumResourceTypesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceNamesA: Pointer;

function EnumResourceNamesA;
begin
  GetProcedureAddress(_EnumResourceNamesA, kernel32, 'EnumResourceNamesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceNamesA]
  end;
end;
{$ELSE}
function EnumResourceNamesA; external kernel32 name 'EnumResourceNamesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceNamesW: Pointer;

function EnumResourceNamesW;
begin
  GetProcedureAddress(_EnumResourceNamesW, kernel32, 'EnumResourceNamesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceNamesW]
  end;
end;
{$ELSE}
function EnumResourceNamesW; external kernel32 name 'EnumResourceNamesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceNames: Pointer;

function EnumResourceNames;
begin
  GetProcedureAddress(_EnumResourceNames, kernel32, 'EnumResourceNamesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceNames]
  end;
end;
{$ELSE}
function EnumResourceNames; external kernel32 name 'EnumResourceNamesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceNames: Pointer;

function EnumResourceNames;
begin
  GetProcedureAddress(_EnumResourceNames, kernel32, 'EnumResourceNamesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceNames]
  end;
end;
{$ELSE}
function EnumResourceNames; external kernel32 name 'EnumResourceNamesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceLanguagesA: Pointer;

function EnumResourceLanguagesA;
begin
  GetProcedureAddress(_EnumResourceLanguagesA, kernel32, 'EnumResourceLanguagesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceLanguagesA]
  end;
end;
{$ELSE}
function EnumResourceLanguagesA; external kernel32 name 'EnumResourceLanguagesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceLanguagesW: Pointer;

function EnumResourceLanguagesW;
begin
  GetProcedureAddress(_EnumResourceLanguagesW, kernel32, 'EnumResourceLanguagesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceLanguagesW]
  end;
end;
{$ELSE}
function EnumResourceLanguagesW; external kernel32 name 'EnumResourceLanguagesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceLanguages: Pointer;

function EnumResourceLanguages;
begin
  GetProcedureAddress(_EnumResourceLanguages, kernel32, 'EnumResourceLanguagesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceLanguages]
  end;
end;
{$ELSE}
function EnumResourceLanguages; external kernel32 name 'EnumResourceLanguagesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumResourceLanguages: Pointer;

function EnumResourceLanguages;
begin
  GetProcedureAddress(_EnumResourceLanguages, kernel32, 'EnumResourceLanguagesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumResourceLanguages]
  end;
end;
{$ELSE}
function EnumResourceLanguages; external kernel32 name 'EnumResourceLanguagesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _BeginUpdateResourceA: Pointer;

function BeginUpdateResourceA;
begin
  GetProcedureAddress(_BeginUpdateResourceA, kernel32, 'BeginUpdateResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginUpdateResourceA]
  end;
end;
{$ELSE}
function BeginUpdateResourceA; external kernel32 name 'BeginUpdateResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BeginUpdateResourceW: Pointer;

function BeginUpdateResourceW;
begin
  GetProcedureAddress(_BeginUpdateResourceW, kernel32, 'BeginUpdateResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginUpdateResourceW]
  end;
end;
{$ELSE}
function BeginUpdateResourceW; external kernel32 name 'BeginUpdateResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _BeginUpdateResource: Pointer;

function BeginUpdateResource;
begin
  GetProcedureAddress(_BeginUpdateResource, kernel32, 'BeginUpdateResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginUpdateResource]
  end;
end;
{$ELSE}
function BeginUpdateResource; external kernel32 name 'BeginUpdateResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _BeginUpdateResource: Pointer;

function BeginUpdateResource;
begin
  GetProcedureAddress(_BeginUpdateResource, kernel32, 'BeginUpdateResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginUpdateResource]
  end;
end;
{$ELSE}
function BeginUpdateResource; external kernel32 name 'BeginUpdateResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateResourceA: Pointer;

function UpdateResourceA;
begin
  GetProcedureAddress(_UpdateResourceA, kernel32, 'UpdateResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateResourceA]
  end;
end;
{$ELSE}
function UpdateResourceA; external kernel32 name 'UpdateResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateResourceW: Pointer;

function UpdateResourceW;
begin
  GetProcedureAddress(_UpdateResourceW, kernel32, 'UpdateResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateResourceW]
  end;
end;
{$ELSE}
function UpdateResourceW; external kernel32 name 'UpdateResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateResource: Pointer;

function UpdateResource;
begin
  GetProcedureAddress(_UpdateResource, kernel32, 'UpdateResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateResource]
  end;
end;
{$ELSE}
function UpdateResource; external kernel32 name 'UpdateResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateResource: Pointer;

function UpdateResource;
begin
  GetProcedureAddress(_UpdateResource, kernel32, 'UpdateResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateResource]
  end;
end;
{$ELSE}
function UpdateResource; external kernel32 name 'UpdateResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EndUpdateResourceA: Pointer;

function EndUpdateResourceA;
begin
  GetProcedureAddress(_EndUpdateResourceA, kernel32, 'EndUpdateResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndUpdateResourceA]
  end;
end;
{$ELSE}
function EndUpdateResourceA; external kernel32 name 'EndUpdateResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndUpdateResourceW: Pointer;

function EndUpdateResourceW;
begin
  GetProcedureAddress(_EndUpdateResourceW, kernel32, 'EndUpdateResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndUpdateResourceW]
  end;
end;
{$ELSE}
function EndUpdateResourceW; external kernel32 name 'EndUpdateResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EndUpdateResource: Pointer;

function EndUpdateResource;
begin
  GetProcedureAddress(_EndUpdateResource, kernel32, 'EndUpdateResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndUpdateResource]
  end;
end;
{$ELSE}
function EndUpdateResource; external kernel32 name 'EndUpdateResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EndUpdateResource: Pointer;

function EndUpdateResource;
begin
  GetProcedureAddress(_EndUpdateResource, kernel32, 'EndUpdateResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndUpdateResource]
  end;
end;
{$ELSE}
function EndUpdateResource; external kernel32 name 'EndUpdateResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalAddAtomA: Pointer;

function GlobalAddAtomA;
begin
  GetProcedureAddress(_GlobalAddAtomA, kernel32, 'GlobalAddAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalAddAtomA]
  end;
end;
{$ELSE}
function GlobalAddAtomA; external kernel32 name 'GlobalAddAtomA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalAddAtomW: Pointer;

function GlobalAddAtomW;
begin
  GetProcedureAddress(_GlobalAddAtomW, kernel32, 'GlobalAddAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalAddAtomW]
  end;
end;
{$ELSE}
function GlobalAddAtomW; external kernel32 name 'GlobalAddAtomW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalAddAtom: Pointer;

function GlobalAddAtom;
begin
  GetProcedureAddress(_GlobalAddAtom, kernel32, 'GlobalAddAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalAddAtom]
  end;
end;
{$ELSE}
function GlobalAddAtom; external kernel32 name 'GlobalAddAtomW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalAddAtom: Pointer;

function GlobalAddAtom;
begin
  GetProcedureAddress(_GlobalAddAtom, kernel32, 'GlobalAddAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalAddAtom]
  end;
end;
{$ELSE}
function GlobalAddAtom; external kernel32 name 'GlobalAddAtomA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFindAtomA: Pointer;

function GlobalFindAtomA;
begin
  GetProcedureAddress(_GlobalFindAtomA, kernel32, 'GlobalFindAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFindAtomA]
  end;
end;
{$ELSE}
function GlobalFindAtomA; external kernel32 name 'GlobalFindAtomA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFindAtomW: Pointer;

function GlobalFindAtomW;
begin
  GetProcedureAddress(_GlobalFindAtomW, kernel32, 'GlobalFindAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFindAtomW]
  end;
end;
{$ELSE}
function GlobalFindAtomW; external kernel32 name 'GlobalFindAtomW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFindAtom: Pointer;

function GlobalFindAtom;
begin
  GetProcedureAddress(_GlobalFindAtom, kernel32, 'GlobalFindAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFindAtom]
  end;
end;
{$ELSE}
function GlobalFindAtom; external kernel32 name 'GlobalFindAtomW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalFindAtom: Pointer;

function GlobalFindAtom;
begin
  GetProcedureAddress(_GlobalFindAtom, kernel32, 'GlobalFindAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalFindAtom]
  end;
end;
{$ELSE}
function GlobalFindAtom; external kernel32 name 'GlobalFindAtomA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalGetAtomNameA: Pointer;

function GlobalGetAtomNameA;
begin
  GetProcedureAddress(_GlobalGetAtomNameA, kernel32, 'GlobalGetAtomNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalGetAtomNameA]
  end;
end;
{$ELSE}
function GlobalGetAtomNameA; external kernel32 name 'GlobalGetAtomNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalGetAtomNameW: Pointer;

function GlobalGetAtomNameW;
begin
  GetProcedureAddress(_GlobalGetAtomNameW, kernel32, 'GlobalGetAtomNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalGetAtomNameW]
  end;
end;
{$ELSE}
function GlobalGetAtomNameW; external kernel32 name 'GlobalGetAtomNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalGetAtomName: Pointer;

function GlobalGetAtomName;
begin
  GetProcedureAddress(_GlobalGetAtomName, kernel32, 'GlobalGetAtomNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalGetAtomName]
  end;
end;
{$ELSE}
function GlobalGetAtomName; external kernel32 name 'GlobalGetAtomNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GlobalGetAtomName: Pointer;

function GlobalGetAtomName;
begin
  GetProcedureAddress(_GlobalGetAtomName, kernel32, 'GlobalGetAtomNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GlobalGetAtomName]
  end;
end;
{$ELSE}
function GlobalGetAtomName; external kernel32 name 'GlobalGetAtomNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AddAtomA: Pointer;

function AddAtomA;
begin
  GetProcedureAddress(_AddAtomA, kernel32, 'AddAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAtomA]
  end;
end;
{$ELSE}
function AddAtomA; external kernel32 name 'AddAtomA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAtomW: Pointer;

function AddAtomW;
begin
  GetProcedureAddress(_AddAtomW, kernel32, 'AddAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAtomW]
  end;
end;
{$ELSE}
function AddAtomW; external kernel32 name 'AddAtomW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AddAtom: Pointer;

function AddAtom;
begin
  GetProcedureAddress(_AddAtom, kernel32, 'AddAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAtom]
  end;
end;
{$ELSE}
function AddAtom; external kernel32 name 'AddAtomW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AddAtom: Pointer;

function AddAtom;
begin
  GetProcedureAddress(_AddAtom, kernel32, 'AddAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAtom]
  end;
end;
{$ELSE}
function AddAtom; external kernel32 name 'AddAtomA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindAtomA: Pointer;

function FindAtomA;
begin
  GetProcedureAddress(_FindAtomA, kernel32, 'FindAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindAtomA]
  end;
end;
{$ELSE}
function FindAtomA; external kernel32 name 'FindAtomA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindAtomW: Pointer;

function FindAtomW;
begin
  GetProcedureAddress(_FindAtomW, kernel32, 'FindAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindAtomW]
  end;
end;
{$ELSE}
function FindAtomW; external kernel32 name 'FindAtomW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindAtom: Pointer;

function FindAtom;
begin
  GetProcedureAddress(_FindAtom, kernel32, 'FindAtomW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindAtom]
  end;
end;
{$ELSE}
function FindAtom; external kernel32 name 'FindAtomW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindAtom: Pointer;

function FindAtom;
begin
  GetProcedureAddress(_FindAtom, kernel32, 'FindAtomA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindAtom]
  end;
end;
{$ELSE}
function FindAtom; external kernel32 name 'FindAtomA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetAtomNameA: Pointer;

function GetAtomNameA;
begin
  GetProcedureAddress(_GetAtomNameA, kernel32, 'GetAtomNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAtomNameA]
  end;
end;
{$ELSE}
function GetAtomNameA; external kernel32 name 'GetAtomNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAtomNameW: Pointer;

function GetAtomNameW;
begin
  GetProcedureAddress(_GetAtomNameW, kernel32, 'GetAtomNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAtomNameW]
  end;
end;
{$ELSE}
function GetAtomNameW; external kernel32 name 'GetAtomNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetAtomName: Pointer;

function GetAtomName;
begin
  GetProcedureAddress(_GetAtomName, kernel32, 'GetAtomNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAtomName]
  end;
end;
{$ELSE}
function GetAtomName; external kernel32 name 'GetAtomNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetAtomName: Pointer;

function GetAtomName;
begin
  GetProcedureAddress(_GetAtomName, kernel32, 'GetAtomNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAtomName]
  end;
end;
{$ELSE}
function GetAtomName; external kernel32 name 'GetAtomNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileIntA: Pointer;

function GetProfileIntA;
begin
  GetProcedureAddress(_GetProfileIntA, kernel32, 'GetProfileIntA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileIntA]
  end;
end;
{$ELSE}
function GetProfileIntA; external kernel32 name 'GetProfileIntA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileIntW: Pointer;

function GetProfileIntW;
begin
  GetProcedureAddress(_GetProfileIntW, kernel32, 'GetProfileIntW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileIntW]
  end;
end;
{$ELSE}
function GetProfileIntW; external kernel32 name 'GetProfileIntW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileInt: Pointer;

function GetProfileInt;
begin
  GetProcedureAddress(_GetProfileInt, kernel32, 'GetProfileIntW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileInt]
  end;
end;
{$ELSE}
function GetProfileInt; external kernel32 name 'GetProfileIntW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileInt: Pointer;

function GetProfileInt;
begin
  GetProcedureAddress(_GetProfileInt, kernel32, 'GetProfileIntA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileInt]
  end;
end;
{$ELSE}
function GetProfileInt; external kernel32 name 'GetProfileIntA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileStringA: Pointer;

function GetProfileStringA;
begin
  GetProcedureAddress(_GetProfileStringA, kernel32, 'GetProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileStringA]
  end;
end;
{$ELSE}
function GetProfileStringA; external kernel32 name 'GetProfileStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileStringW: Pointer;

function GetProfileStringW;
begin
  GetProcedureAddress(_GetProfileStringW, kernel32, 'GetProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileStringW]
  end;
end;
{$ELSE}
function GetProfileStringW; external kernel32 name 'GetProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileString: Pointer;

function GetProfileString;
begin
  GetProcedureAddress(_GetProfileString, kernel32, 'GetProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileString]
  end;
end;
{$ELSE}
function GetProfileString; external kernel32 name 'GetProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileString: Pointer;

function GetProfileString;
begin
  GetProcedureAddress(_GetProfileString, kernel32, 'GetProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileString]
  end;
end;
{$ELSE}
function GetProfileString; external kernel32 name 'GetProfileStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileStringA: Pointer;

function WriteProfileStringA;
begin
  GetProcedureAddress(_WriteProfileStringA, kernel32, 'WriteProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileStringA]
  end;
end;
{$ELSE}
function WriteProfileStringA; external kernel32 name 'WriteProfileStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileStringW: Pointer;

function WriteProfileStringW;
begin
  GetProcedureAddress(_WriteProfileStringW, kernel32, 'WriteProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileStringW]
  end;
end;
{$ELSE}
function WriteProfileStringW; external kernel32 name 'WriteProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileString: Pointer;

function WriteProfileString;
begin
  GetProcedureAddress(_WriteProfileString, kernel32, 'WriteProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileString]
  end;
end;
{$ELSE}
function WriteProfileString; external kernel32 name 'WriteProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileString: Pointer;

function WriteProfileString;
begin
  GetProcedureAddress(_WriteProfileString, kernel32, 'WriteProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileString]
  end;
end;
{$ELSE}
function WriteProfileString; external kernel32 name 'WriteProfileStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileSectionA: Pointer;

function GetProfileSectionA;
begin
  GetProcedureAddress(_GetProfileSectionA, kernel32, 'GetProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileSectionA]
  end;
end;
{$ELSE}
function GetProfileSectionA; external kernel32 name 'GetProfileSectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileSectionW: Pointer;

function GetProfileSectionW;
begin
  GetProcedureAddress(_GetProfileSectionW, kernel32, 'GetProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileSectionW]
  end;
end;
{$ELSE}
function GetProfileSectionW; external kernel32 name 'GetProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileSection: Pointer;

function GetProfileSection;
begin
  GetProcedureAddress(_GetProfileSection, kernel32, 'GetProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileSection]
  end;
end;
{$ELSE}
function GetProfileSection; external kernel32 name 'GetProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProfileSection: Pointer;

function GetProfileSection;
begin
  GetProcedureAddress(_GetProfileSection, kernel32, 'GetProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProfileSection]
  end;
end;
{$ELSE}
function GetProfileSection; external kernel32 name 'GetProfileSectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileSectionA: Pointer;

function WriteProfileSectionA;
begin
  GetProcedureAddress(_WriteProfileSectionA, kernel32, 'WriteProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileSectionA]
  end;
end;
{$ELSE}
function WriteProfileSectionA; external kernel32 name 'WriteProfileSectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileSectionW: Pointer;

function WriteProfileSectionW;
begin
  GetProcedureAddress(_WriteProfileSectionW, kernel32, 'WriteProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileSectionW]
  end;
end;
{$ELSE}
function WriteProfileSectionW; external kernel32 name 'WriteProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileSection: Pointer;

function WriteProfileSection;
begin
  GetProcedureAddress(_WriteProfileSection, kernel32, 'WriteProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileSection]
  end;
end;
{$ELSE}
function WriteProfileSection; external kernel32 name 'WriteProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteProfileSection: Pointer;

function WriteProfileSection;
begin
  GetProcedureAddress(_WriteProfileSection, kernel32, 'WriteProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteProfileSection]
  end;
end;
{$ELSE}
function WriteProfileSection; external kernel32 name 'WriteProfileSectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileIntA: Pointer;

function GetPrivateProfileIntA;
begin
  GetProcedureAddress(_GetPrivateProfileIntA, kernel32, 'GetPrivateProfileIntA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileIntA]
  end;
end;
{$ELSE}
function GetPrivateProfileIntA; external kernel32 name 'GetPrivateProfileIntA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileIntW: Pointer;

function GetPrivateProfileIntW;
begin
  GetProcedureAddress(_GetPrivateProfileIntW, kernel32, 'GetPrivateProfileIntW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileIntW]
  end;
end;
{$ELSE}
function GetPrivateProfileIntW; external kernel32 name 'GetPrivateProfileIntW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileInt: Pointer;

function GetPrivateProfileInt;
begin
  GetProcedureAddress(_GetPrivateProfileInt, kernel32, 'GetPrivateProfileIntW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileInt]
  end;
end;
{$ELSE}
function GetPrivateProfileInt; external kernel32 name 'GetPrivateProfileIntW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileInt: Pointer;

function GetPrivateProfileInt;
begin
  GetProcedureAddress(_GetPrivateProfileInt, kernel32, 'GetPrivateProfileIntA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileInt]
  end;
end;
{$ELSE}
function GetPrivateProfileInt; external kernel32 name 'GetPrivateProfileIntA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileStringA: Pointer;

function GetPrivateProfileStringA;
begin
  GetProcedureAddress(_GetPrivateProfileStringA, kernel32, 'GetPrivateProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileStringA]
  end;
end;
{$ELSE}
function GetPrivateProfileStringA; external kernel32 name 'GetPrivateProfileStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileStringW: Pointer;

function GetPrivateProfileStringW;
begin
  GetProcedureAddress(_GetPrivateProfileStringW, kernel32, 'GetPrivateProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileStringW]
  end;
end;
{$ELSE}
function GetPrivateProfileStringW; external kernel32 name 'GetPrivateProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileString: Pointer;

function GetPrivateProfileString;
begin
  GetProcedureAddress(_GetPrivateProfileString, kernel32, 'GetPrivateProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileString]
  end;
end;
{$ELSE}
function GetPrivateProfileString; external kernel32 name 'GetPrivateProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileString: Pointer;

function GetPrivateProfileString;
begin
  GetProcedureAddress(_GetPrivateProfileString, kernel32, 'GetPrivateProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileString]
  end;
end;
{$ELSE}
function GetPrivateProfileString; external kernel32 name 'GetPrivateProfileStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileStringA: Pointer;

function WritePrivateProfileStringA;
begin
  GetProcedureAddress(_WritePrivateProfileStringA, kernel32, 'WritePrivateProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileStringA]
  end;
end;
{$ELSE}
function WritePrivateProfileStringA; external kernel32 name 'WritePrivateProfileStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileStringW: Pointer;

function WritePrivateProfileStringW;
begin
  GetProcedureAddress(_WritePrivateProfileStringW, kernel32, 'WritePrivateProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileStringW]
  end;
end;
{$ELSE}
function WritePrivateProfileStringW; external kernel32 name 'WritePrivateProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileString: Pointer;

function WritePrivateProfileString;
begin
  GetProcedureAddress(_WritePrivateProfileString, kernel32, 'WritePrivateProfileStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileString]
  end;
end;
{$ELSE}
function WritePrivateProfileString; external kernel32 name 'WritePrivateProfileStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileString: Pointer;

function WritePrivateProfileString;
begin
  GetProcedureAddress(_WritePrivateProfileString, kernel32, 'WritePrivateProfileStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileString]
  end;
end;
{$ELSE}
function WritePrivateProfileString; external kernel32 name 'WritePrivateProfileStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSectionA: Pointer;

function GetPrivateProfileSectionA;
begin
  GetProcedureAddress(_GetPrivateProfileSectionA, kernel32, 'GetPrivateProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSectionA]
  end;
end;
{$ELSE}
function GetPrivateProfileSectionA; external kernel32 name 'GetPrivateProfileSectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSectionW: Pointer;

function GetPrivateProfileSectionW;
begin
  GetProcedureAddress(_GetPrivateProfileSectionW, kernel32, 'GetPrivateProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSectionW]
  end;
end;
{$ELSE}
function GetPrivateProfileSectionW; external kernel32 name 'GetPrivateProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSection: Pointer;

function GetPrivateProfileSection;
begin
  GetProcedureAddress(_GetPrivateProfileSection, kernel32, 'GetPrivateProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSection]
  end;
end;
{$ELSE}
function GetPrivateProfileSection; external kernel32 name 'GetPrivateProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSection: Pointer;

function GetPrivateProfileSection;
begin
  GetProcedureAddress(_GetPrivateProfileSection, kernel32, 'GetPrivateProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSection]
  end;
end;
{$ELSE}
function GetPrivateProfileSection; external kernel32 name 'GetPrivateProfileSectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileSectionA: Pointer;

function WritePrivateProfileSectionA;
begin
  GetProcedureAddress(_WritePrivateProfileSectionA, kernel32, 'WritePrivateProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileSectionA]
  end;
end;
{$ELSE}
function WritePrivateProfileSectionA; external kernel32 name 'WritePrivateProfileSectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileSectionW: Pointer;

function WritePrivateProfileSectionW;
begin
  GetProcedureAddress(_WritePrivateProfileSectionW, kernel32, 'WritePrivateProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileSectionW]
  end;
end;
{$ELSE}
function WritePrivateProfileSectionW; external kernel32 name 'WritePrivateProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileSection: Pointer;

function WritePrivateProfileSection;
begin
  GetProcedureAddress(_WritePrivateProfileSection, kernel32, 'WritePrivateProfileSectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileSection]
  end;
end;
{$ELSE}
function WritePrivateProfileSection; external kernel32 name 'WritePrivateProfileSectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileSection: Pointer;

function WritePrivateProfileSection;
begin
  GetProcedureAddress(_WritePrivateProfileSection, kernel32, 'WritePrivateProfileSectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileSection]
  end;
end;
{$ELSE}
function WritePrivateProfileSection; external kernel32 name 'WritePrivateProfileSectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSectionNamesA: Pointer;

function GetPrivateProfileSectionNamesA;
begin
  GetProcedureAddress(_GetPrivateProfileSectionNamesA, kernel32, 'GetPrivateProfileSectionNamesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSectionNamesA]
  end;
end;
{$ELSE}
function GetPrivateProfileSectionNamesA; external kernel32 name 'GetPrivateProfileSectionNamesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSectionNamesW: Pointer;

function GetPrivateProfileSectionNamesW;
begin
  GetProcedureAddress(_GetPrivateProfileSectionNamesW, kernel32, 'GetPrivateProfileSectionNamesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSectionNamesW]
  end;
end;
{$ELSE}
function GetPrivateProfileSectionNamesW; external kernel32 name 'GetPrivateProfileSectionNamesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSectionNames: Pointer;

function GetPrivateProfileSectionNames;
begin
  GetProcedureAddress(_GetPrivateProfileSectionNames, kernel32, 'GetPrivateProfileSectionNamesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSectionNames]
  end;
end;
{$ELSE}
function GetPrivateProfileSectionNames; external kernel32 name 'GetPrivateProfileSectionNamesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileSectionNames: Pointer;

function GetPrivateProfileSectionNames;
begin
  GetProcedureAddress(_GetPrivateProfileSectionNames, kernel32, 'GetPrivateProfileSectionNamesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileSectionNames]
  end;
end;
{$ELSE}
function GetPrivateProfileSectionNames; external kernel32 name 'GetPrivateProfileSectionNamesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileStructA: Pointer;

function GetPrivateProfileStructA;
begin
  GetProcedureAddress(_GetPrivateProfileStructA, kernel32, 'GetPrivateProfileStructA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileStructA]
  end;
end;
{$ELSE}
function GetPrivateProfileStructA; external kernel32 name 'GetPrivateProfileStructA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileStructW: Pointer;

function GetPrivateProfileStructW;
begin
  GetProcedureAddress(_GetPrivateProfileStructW, kernel32, 'GetPrivateProfileStructW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileStructW]
  end;
end;
{$ELSE}
function GetPrivateProfileStructW; external kernel32 name 'GetPrivateProfileStructW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileStruct: Pointer;

function GetPrivateProfileStruct;
begin
  GetProcedureAddress(_GetPrivateProfileStruct, kernel32, 'GetPrivateProfileStructW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileStruct]
  end;
end;
{$ELSE}
function GetPrivateProfileStruct; external kernel32 name 'GetPrivateProfileStructW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateProfileStruct: Pointer;

function GetPrivateProfileStruct;
begin
  GetProcedureAddress(_GetPrivateProfileStruct, kernel32, 'GetPrivateProfileStructA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateProfileStruct]
  end;
end;
{$ELSE}
function GetPrivateProfileStruct; external kernel32 name 'GetPrivateProfileStructA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileStructA: Pointer;

function WritePrivateProfileStructA;
begin
  GetProcedureAddress(_WritePrivateProfileStructA, kernel32, 'WritePrivateProfileStructA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileStructA]
  end;
end;
{$ELSE}
function WritePrivateProfileStructA; external kernel32 name 'WritePrivateProfileStructA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileStructW: Pointer;

function WritePrivateProfileStructW;
begin
  GetProcedureAddress(_WritePrivateProfileStructW, kernel32, 'WritePrivateProfileStructW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileStructW]
  end;
end;
{$ELSE}
function WritePrivateProfileStructW; external kernel32 name 'WritePrivateProfileStructW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileStruct: Pointer;

function WritePrivateProfileStruct;
begin
  GetProcedureAddress(_WritePrivateProfileStruct, kernel32, 'WritePrivateProfileStructW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileStruct]
  end;
end;
{$ELSE}
function WritePrivateProfileStruct; external kernel32 name 'WritePrivateProfileStructW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WritePrivateProfileStruct: Pointer;

function WritePrivateProfileStruct;
begin
  GetProcedureAddress(_WritePrivateProfileStruct, kernel32, 'WritePrivateProfileStructA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WritePrivateProfileStruct]
  end;
end;
{$ELSE}
function WritePrivateProfileStruct; external kernel32 name 'WritePrivateProfileStructA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDriveTypeA: Pointer;

function GetDriveTypeA;
begin
  GetProcedureAddress(_GetDriveTypeA, kernel32, 'GetDriveTypeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDriveTypeA]
  end;
end;
{$ELSE}
function GetDriveTypeA; external kernel32 name 'GetDriveTypeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDriveTypeW: Pointer;

function GetDriveTypeW;
begin
  GetProcedureAddress(_GetDriveTypeW, kernel32, 'GetDriveTypeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDriveTypeW]
  end;
end;
{$ELSE}
function GetDriveTypeW; external kernel32 name 'GetDriveTypeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDriveType: Pointer;

function GetDriveType;
begin
  GetProcedureAddress(_GetDriveType, kernel32, 'GetDriveTypeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDriveType]
  end;
end;
{$ELSE}
function GetDriveType; external kernel32 name 'GetDriveTypeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDriveType: Pointer;

function GetDriveType;
begin
  GetProcedureAddress(_GetDriveType, kernel32, 'GetDriveTypeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDriveType]
  end;
end;
{$ELSE}
function GetDriveType; external kernel32 name 'GetDriveTypeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDirectoryA: Pointer;

function GetSystemDirectoryA;
begin
  GetProcedureAddress(_GetSystemDirectoryA, kernel32, 'GetSystemDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDirectoryA]
  end;
end;
{$ELSE}
function GetSystemDirectoryA; external kernel32 name 'GetSystemDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDirectoryW: Pointer;

function GetSystemDirectoryW;
begin
  GetProcedureAddress(_GetSystemDirectoryW, kernel32, 'GetSystemDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDirectoryW]
  end;
end;
{$ELSE}
function GetSystemDirectoryW; external kernel32 name 'GetSystemDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDirectory: Pointer;

function GetSystemDirectory;
begin
  GetProcedureAddress(_GetSystemDirectory, kernel32, 'GetSystemDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDirectory]
  end;
end;
{$ELSE}
function GetSystemDirectory; external kernel32 name 'GetSystemDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemDirectory: Pointer;

function GetSystemDirectory;
begin
  GetProcedureAddress(_GetSystemDirectory, kernel32, 'GetSystemDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemDirectory]
  end;
end;
{$ELSE}
function GetSystemDirectory; external kernel32 name 'GetSystemDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempPathA: Pointer;

function GetTempPathA;
begin
  GetProcedureAddress(_GetTempPathA, kernel32, 'GetTempPathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempPathA]
  end;
end;
{$ELSE}
function GetTempPathA; external kernel32 name 'GetTempPathA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempPathW: Pointer;

function GetTempPathW;
begin
  GetProcedureAddress(_GetTempPathW, kernel32, 'GetTempPathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempPathW]
  end;
end;
{$ELSE}
function GetTempPathW; external kernel32 name 'GetTempPathW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempPath: Pointer;

function GetTempPath;
begin
  GetProcedureAddress(_GetTempPath, kernel32, 'GetTempPathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempPath]
  end;
end;
{$ELSE}
function GetTempPath; external kernel32 name 'GetTempPathW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempPath: Pointer;

function GetTempPath;
begin
  GetProcedureAddress(_GetTempPath, kernel32, 'GetTempPathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempPath]
  end;
end;
{$ELSE}
function GetTempPath; external kernel32 name 'GetTempPathA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempFileNameA: Pointer;

function GetTempFileNameA;
begin
  GetProcedureAddress(_GetTempFileNameA, kernel32, 'GetTempFileNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempFileNameA]
  end;
end;
{$ELSE}
function GetTempFileNameA; external kernel32 name 'GetTempFileNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempFileNameW: Pointer;

function GetTempFileNameW;
begin
  GetProcedureAddress(_GetTempFileNameW, kernel32, 'GetTempFileNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempFileNameW]
  end;
end;
{$ELSE}
function GetTempFileNameW; external kernel32 name 'GetTempFileNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempFileName: Pointer;

function GetTempFileName;
begin
  GetProcedureAddress(_GetTempFileName, kernel32, 'GetTempFileNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempFileName]
  end;
end;
{$ELSE}
function GetTempFileName; external kernel32 name 'GetTempFileNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTempFileName: Pointer;

function GetTempFileName;
begin
  GetProcedureAddress(_GetTempFileName, kernel32, 'GetTempFileNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTempFileName]
  end;
end;
{$ELSE}
function GetTempFileName; external kernel32 name 'GetTempFileNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowsDirectoryA: Pointer;

function GetWindowsDirectoryA;
begin
  GetProcedureAddress(_GetWindowsDirectoryA, kernel32, 'GetWindowsDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowsDirectoryA]
  end;
end;
{$ELSE}
function GetWindowsDirectoryA; external kernel32 name 'GetWindowsDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowsDirectoryW: Pointer;

function GetWindowsDirectoryW;
begin
  GetProcedureAddress(_GetWindowsDirectoryW, kernel32, 'GetWindowsDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowsDirectoryW]
  end;
end;
{$ELSE}
function GetWindowsDirectoryW; external kernel32 name 'GetWindowsDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowsDirectory: Pointer;

function GetWindowsDirectory;
begin
  GetProcedureAddress(_GetWindowsDirectory, kernel32, 'GetWindowsDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowsDirectory]
  end;
end;
{$ELSE}
function GetWindowsDirectory; external kernel32 name 'GetWindowsDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowsDirectory: Pointer;

function GetWindowsDirectory;
begin
  GetProcedureAddress(_GetWindowsDirectory, kernel32, 'GetWindowsDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowsDirectory]
  end;
end;
{$ELSE}
function GetWindowsDirectory; external kernel32 name 'GetWindowsDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWindowsDirectoryA: Pointer;

function GetSystemWindowsDirectoryA;
begin
  GetProcedureAddress(_GetSystemWindowsDirectoryA, kernel32, 'GetSystemWindowsDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWindowsDirectoryA]
  end;
end;
{$ELSE}
function GetSystemWindowsDirectoryA; external kernel32 name 'GetSystemWindowsDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWindowsDirectoryW: Pointer;

function GetSystemWindowsDirectoryW;
begin
  GetProcedureAddress(_GetSystemWindowsDirectoryW, kernel32, 'GetSystemWindowsDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWindowsDirectoryW]
  end;
end;
{$ELSE}
function GetSystemWindowsDirectoryW; external kernel32 name 'GetSystemWindowsDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWindowsDirectory: Pointer;

function GetSystemWindowsDirectory;
begin
  GetProcedureAddress(_GetSystemWindowsDirectory, kernel32, 'GetSystemWindowsDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWindowsDirectory]
  end;
end;
{$ELSE}
function GetSystemWindowsDirectory; external kernel32 name 'GetSystemWindowsDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWindowsDirectory: Pointer;

function GetSystemWindowsDirectory;
begin
  GetProcedureAddress(_GetSystemWindowsDirectory, kernel32, 'GetSystemWindowsDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWindowsDirectory]
  end;
end;
{$ELSE}
function GetSystemWindowsDirectory; external kernel32 name 'GetSystemWindowsDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWow64DirectoryA: Pointer;

function GetSystemWow64DirectoryA;
begin
  GetProcedureAddress(_GetSystemWow64DirectoryA, kernel32, 'GetSystemWow64DirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWow64DirectoryA]
  end;
end;
{$ELSE}
function GetSystemWow64DirectoryA; external kernel32 name 'GetSystemWow64DirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWow64DirectoryW: Pointer;

function GetSystemWow64DirectoryW;
begin
  GetProcedureAddress(_GetSystemWow64DirectoryW, kernel32, 'GetSystemWow64DirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWow64DirectoryW]
  end;
end;
{$ELSE}
function GetSystemWow64DirectoryW; external kernel32 name 'GetSystemWow64DirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWow64Directory: Pointer;

function GetSystemWow64Directory;
begin
  GetProcedureAddress(_GetSystemWow64Directory, kernel32, 'GetSystemWow64DirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWow64Directory]
  end;
end;
{$ELSE}
function GetSystemWow64Directory; external kernel32 name 'GetSystemWow64DirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemWow64Directory: Pointer;

function GetSystemWow64Directory;
begin
  GetProcedureAddress(_GetSystemWow64Directory, kernel32, 'GetSystemWow64DirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemWow64Directory]
  end;
end;
{$ELSE}
function GetSystemWow64Directory; external kernel32 name 'GetSystemWow64DirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _Wow64EnableWow64FsRedirection: Pointer;

function Wow64EnableWow64FsRedirection;
begin
  GetProcedureAddress(_Wow64EnableWow64FsRedirection, kernel32, 'Wow64EnableWow64FsRedirection');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Wow64EnableWow64FsRedirection]
  end;
end;
{$ELSE}
function Wow64EnableWow64FsRedirection; external kernel32 name 'Wow64EnableWow64FsRedirection';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCurrentDirectoryA: Pointer;

function SetCurrentDirectoryA;
begin
  GetProcedureAddress(_SetCurrentDirectoryA, kernel32, 'SetCurrentDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCurrentDirectoryA]
  end;
end;
{$ELSE}
function SetCurrentDirectoryA; external kernel32 name 'SetCurrentDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCurrentDirectoryW: Pointer;

function SetCurrentDirectoryW;
begin
  GetProcedureAddress(_SetCurrentDirectoryW, kernel32, 'SetCurrentDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCurrentDirectoryW]
  end;
end;
{$ELSE}
function SetCurrentDirectoryW; external kernel32 name 'SetCurrentDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetCurrentDirectory: Pointer;

function SetCurrentDirectory;
begin
  GetProcedureAddress(_SetCurrentDirectory, kernel32, 'SetCurrentDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCurrentDirectory]
  end;
end;
{$ELSE}
function SetCurrentDirectory; external kernel32 name 'SetCurrentDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetCurrentDirectory: Pointer;

function SetCurrentDirectory;
begin
  GetProcedureAddress(_SetCurrentDirectory, kernel32, 'SetCurrentDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCurrentDirectory]
  end;
end;
{$ELSE}
function SetCurrentDirectory; external kernel32 name 'SetCurrentDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentDirectoryA: Pointer;

function GetCurrentDirectoryA;
begin
  GetProcedureAddress(_GetCurrentDirectoryA, kernel32, 'GetCurrentDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentDirectoryA]
  end;
end;
{$ELSE}
function GetCurrentDirectoryA; external kernel32 name 'GetCurrentDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentDirectoryW: Pointer;

function GetCurrentDirectoryW;
begin
  GetProcedureAddress(_GetCurrentDirectoryW, kernel32, 'GetCurrentDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentDirectoryW]
  end;
end;
{$ELSE}
function GetCurrentDirectoryW; external kernel32 name 'GetCurrentDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentDirectory: Pointer;

function GetCurrentDirectory;
begin
  GetProcedureAddress(_GetCurrentDirectory, kernel32, 'GetCurrentDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentDirectory]
  end;
end;
{$ELSE}
function GetCurrentDirectory; external kernel32 name 'GetCurrentDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentDirectory: Pointer;

function GetCurrentDirectory;
begin
  GetProcedureAddress(_GetCurrentDirectory, kernel32, 'GetCurrentDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentDirectory]
  end;
end;
{$ELSE}
function GetCurrentDirectory; external kernel32 name 'GetCurrentDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetDllDirectoryA: Pointer;

function SetDllDirectoryA;
begin
  GetProcedureAddress(_SetDllDirectoryA, kernel32, 'SetDllDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDllDirectoryA]
  end;
end;
{$ELSE}
function SetDllDirectoryA; external kernel32 name 'SetDllDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDllDirectoryW: Pointer;

function SetDllDirectoryW;
begin
  GetProcedureAddress(_SetDllDirectoryW, kernel32, 'SetDllDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDllDirectoryW]
  end;
end;
{$ELSE}
function SetDllDirectoryW; external kernel32 name 'SetDllDirectoryW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetDllDirectory: Pointer;

function SetDllDirectory;
begin
  GetProcedureAddress(_SetDllDirectory, kernel32, 'SetDllDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDllDirectory]
  end;
end;
{$ELSE}
function SetDllDirectory; external kernel32 name 'SetDllDirectoryW';
{$ENDIF DYNAMIC_LINK}

{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetDllDirectory: Pointer;

function SetDllDirectory;
begin
  GetProcedureAddress(_SetDllDirectory, kernel32, 'SetDllDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDllDirectory]
  end;
end;
{$ELSE}
function SetDllDirectory; external kernel32 name 'SetDllDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDllDirectoryA: Pointer;

function GetDllDirectoryA;
begin
  GetProcedureAddress(_GetDllDirectoryA, kernel32, 'GetDllDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDllDirectoryA]
  end;
end;
{$ELSE}
function GetDllDirectoryA; external kernel32 name 'GetDllDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDllDirectoryW: Pointer;

function GetDllDirectoryW;
begin
  GetProcedureAddress(_GetDllDirectoryW, kernel32, 'GetDllDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDllDirectoryW]
  end;
end;
{$ELSE}
function GetDllDirectoryW; external kernel32 name 'GetDllDirectoryW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDllDirectory: Pointer;

function GetDllDirectory;
begin
  GetProcedureAddress(_GetDllDirectory, kernel32, 'GetDllDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDllDirectory]
  end;
end;
{$ELSE}
function GetDllDirectory; external kernel32 name 'GetDllDirectoryW';
{$ENDIF DYNAMIC_LINK}

{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDllDirectory: Pointer;

function GetDllDirectory;
begin
  GetProcedureAddress(_GetDllDirectory, kernel32, 'GetDllDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDllDirectory]
  end;
end;
{$ELSE}
function GetDllDirectory; external kernel32 name 'GetDllDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$ENDIF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpaceA: Pointer;

function GetDiskFreeSpaceA;
begin
  GetProcedureAddress(_GetDiskFreeSpaceA, kernel32, 'GetDiskFreeSpaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpaceA]
  end;
end;
{$ELSE}
function GetDiskFreeSpaceA; external kernel32 name 'GetDiskFreeSpaceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpaceW: Pointer;

function GetDiskFreeSpaceW;
begin
  GetProcedureAddress(_GetDiskFreeSpaceW, kernel32, 'GetDiskFreeSpaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpaceW]
  end;
end;
{$ELSE}
function GetDiskFreeSpaceW; external kernel32 name 'GetDiskFreeSpaceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpace: Pointer;

function GetDiskFreeSpace;
begin
  GetProcedureAddress(_GetDiskFreeSpace, kernel32, 'GetDiskFreeSpaceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpace]
  end;
end;
{$ELSE}
function GetDiskFreeSpace; external kernel32 name 'GetDiskFreeSpaceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpace: Pointer;

function GetDiskFreeSpace;
begin
  GetProcedureAddress(_GetDiskFreeSpace, kernel32, 'GetDiskFreeSpaceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpace]
  end;
end;
{$ELSE}
function GetDiskFreeSpace; external kernel32 name 'GetDiskFreeSpaceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpaceExA: Pointer;

function GetDiskFreeSpaceExA;
begin
  GetProcedureAddress(_GetDiskFreeSpaceExA, kernel32, 'GetDiskFreeSpaceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpaceExA]
  end;
end;
{$ELSE}
function GetDiskFreeSpaceExA; external kernel32 name 'GetDiskFreeSpaceExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpaceExW: Pointer;

function GetDiskFreeSpaceExW;
begin
  GetProcedureAddress(_GetDiskFreeSpaceExW, kernel32, 'GetDiskFreeSpaceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpaceExW]
  end;
end;
{$ELSE}
function GetDiskFreeSpaceExW; external kernel32 name 'GetDiskFreeSpaceExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpaceEx: Pointer;

function GetDiskFreeSpaceEx;
begin
  GetProcedureAddress(_GetDiskFreeSpaceEx, kernel32, 'GetDiskFreeSpaceExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpaceEx]
  end;
end;
{$ELSE}
function GetDiskFreeSpaceEx; external kernel32 name 'GetDiskFreeSpaceExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDiskFreeSpaceEx: Pointer;

function GetDiskFreeSpaceEx;
begin
  GetProcedureAddress(_GetDiskFreeSpaceEx, kernel32, 'GetDiskFreeSpaceExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDiskFreeSpaceEx]
  end;
end;
{$ELSE}
function GetDiskFreeSpaceEx; external kernel32 name 'GetDiskFreeSpaceExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectoryA: Pointer;

function CreateDirectoryA;
begin
  GetProcedureAddress(_CreateDirectoryA, kernel32, 'CreateDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectoryA]
  end;
end;
{$ELSE}
function CreateDirectoryA; external kernel32 name 'CreateDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectoryW: Pointer;

function CreateDirectoryW;
begin
  GetProcedureAddress(_CreateDirectoryW, kernel32, 'CreateDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectoryW]
  end;
end;
{$ELSE}
function CreateDirectoryW; external kernel32 name 'CreateDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectory: Pointer;

function CreateDirectory;
begin
  GetProcedureAddress(_CreateDirectory, kernel32, 'CreateDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectory]
  end;
end;
{$ELSE}
function CreateDirectory; external kernel32 name 'CreateDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectory: Pointer;

function CreateDirectory;
begin
  GetProcedureAddress(_CreateDirectory, kernel32, 'CreateDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectory]
  end;
end;
{$ELSE}
function CreateDirectory; external kernel32 name 'CreateDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectoryExA: Pointer;

function CreateDirectoryExA;
begin
  GetProcedureAddress(_CreateDirectoryExA, kernel32, 'CreateDirectoryExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectoryExA]
  end;
end;
{$ELSE}
function CreateDirectoryExA; external kernel32 name 'CreateDirectoryExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectoryExW: Pointer;

function CreateDirectoryExW;
begin
  GetProcedureAddress(_CreateDirectoryExW, kernel32, 'CreateDirectoryExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectoryExW]
  end;
end;
{$ELSE}
function CreateDirectoryExW; external kernel32 name 'CreateDirectoryExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectoryEx: Pointer;

function CreateDirectoryEx;
begin
  GetProcedureAddress(_CreateDirectoryEx, kernel32, 'CreateDirectoryExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectoryEx]
  end;
end;
{$ELSE}
function CreateDirectoryEx; external kernel32 name 'CreateDirectoryExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDirectoryEx: Pointer;

function CreateDirectoryEx;
begin
  GetProcedureAddress(_CreateDirectoryEx, kernel32, 'CreateDirectoryExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDirectoryEx]
  end;
end;
{$ELSE}
function CreateDirectoryEx; external kernel32 name 'CreateDirectoryExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveDirectoryA: Pointer;

function RemoveDirectoryA;
begin
  GetProcedureAddress(_RemoveDirectoryA, kernel32, 'RemoveDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveDirectoryA]
  end;
end;
{$ELSE}
function RemoveDirectoryA; external kernel32 name 'RemoveDirectoryA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveDirectoryW: Pointer;

function RemoveDirectoryW;
begin
  GetProcedureAddress(_RemoveDirectoryW, kernel32, 'RemoveDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveDirectoryW]
  end;
end;
{$ELSE}
function RemoveDirectoryW; external kernel32 name 'RemoveDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveDirectory: Pointer;

function RemoveDirectory;
begin
  GetProcedureAddress(_RemoveDirectory, kernel32, 'RemoveDirectoryW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveDirectory]
  end;
end;
{$ELSE}
function RemoveDirectory; external kernel32 name 'RemoveDirectoryW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveDirectory: Pointer;

function RemoveDirectory;
begin
  GetProcedureAddress(_RemoveDirectory, kernel32, 'RemoveDirectoryA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveDirectory]
  end;
end;
{$ELSE}
function RemoveDirectory; external kernel32 name 'RemoveDirectoryA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetFullPathNameA: Pointer;

function GetFullPathNameA;
begin
  GetProcedureAddress(_GetFullPathNameA, kernel32, 'GetFullPathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFullPathNameA]
  end;
end;
{$ELSE}
function GetFullPathNameA; external kernel32 name 'GetFullPathNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFullPathNameW: Pointer;

function GetFullPathNameW;
begin
  GetProcedureAddress(_GetFullPathNameW, kernel32, 'GetFullPathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFullPathNameW]
  end;
end;
{$ELSE}
function GetFullPathNameW; external kernel32 name 'GetFullPathNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFullPathName: Pointer;

function GetFullPathName;
begin
  GetProcedureAddress(_GetFullPathName, kernel32, 'GetFullPathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFullPathName]
  end;
end;
{$ELSE}
function GetFullPathName; external kernel32 name 'GetFullPathNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFullPathName: Pointer;

function GetFullPathName;
begin
  GetProcedureAddress(_GetFullPathName, kernel32, 'GetFullPathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFullPathName]
  end;
end;
{$ELSE}
function GetFullPathName; external kernel32 name 'GetFullPathNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DefineDosDeviceA: Pointer;

function DefineDosDeviceA;
begin
  GetProcedureAddress(_DefineDosDeviceA, kernel32, 'DefineDosDeviceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefineDosDeviceA]
  end;
end;
{$ELSE}
function DefineDosDeviceA; external kernel32 name 'DefineDosDeviceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefineDosDeviceW: Pointer;

function DefineDosDeviceW;
begin
  GetProcedureAddress(_DefineDosDeviceW, kernel32, 'DefineDosDeviceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefineDosDeviceW]
  end;
end;
{$ELSE}
function DefineDosDeviceW; external kernel32 name 'DefineDosDeviceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DefineDosDevice: Pointer;

function DefineDosDevice;
begin
  GetProcedureAddress(_DefineDosDevice, kernel32, 'DefineDosDeviceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefineDosDevice]
  end;
end;
{$ELSE}
function DefineDosDevice; external kernel32 name 'DefineDosDeviceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DefineDosDevice: Pointer;

function DefineDosDevice;
begin
  GetProcedureAddress(_DefineDosDevice, kernel32, 'DefineDosDeviceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefineDosDevice]
  end;
end;
{$ELSE}
function DefineDosDevice; external kernel32 name 'DefineDosDeviceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _QueryDosDeviceA: Pointer;

function QueryDosDeviceA;
begin
  GetProcedureAddress(_QueryDosDeviceA, kernel32, 'QueryDosDeviceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryDosDeviceA]
  end;
end;
{$ELSE}
function QueryDosDeviceA; external kernel32 name 'QueryDosDeviceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueryDosDeviceW: Pointer;

function QueryDosDeviceW;
begin
  GetProcedureAddress(_QueryDosDeviceW, kernel32, 'QueryDosDeviceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryDosDeviceW]
  end;
end;
{$ELSE}
function QueryDosDeviceW; external kernel32 name 'QueryDosDeviceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _QueryDosDevice: Pointer;

function QueryDosDevice;
begin
  GetProcedureAddress(_QueryDosDevice, kernel32, 'QueryDosDeviceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryDosDevice]
  end;
end;
{$ELSE}
function QueryDosDevice; external kernel32 name 'QueryDosDeviceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _QueryDosDevice: Pointer;

function QueryDosDevice;
begin
  GetProcedureAddress(_QueryDosDevice, kernel32, 'QueryDosDeviceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryDosDevice]
  end;
end;
{$ELSE}
function QueryDosDevice; external kernel32 name 'QueryDosDeviceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFileA: Pointer;

function CreateFileA;
begin
  GetProcedureAddress(_CreateFileA, kernel32, 'CreateFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFileA]
  end;
end;
{$ELSE}
function CreateFileA; external kernel32 name 'CreateFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFileW: Pointer;

function CreateFileW;
begin
  GetProcedureAddress(_CreateFileW, kernel32, 'CreateFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFileW]
  end;
end;
{$ELSE}
function CreateFileW; external kernel32 name 'CreateFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFile: Pointer;

function CreateFile;
begin
  GetProcedureAddress(_CreateFile, kernel32, 'CreateFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFile]
  end;
end;
{$ELSE}
function CreateFile; external kernel32 name 'CreateFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateFile: Pointer;

function CreateFile;
begin
  GetProcedureAddress(_CreateFile, kernel32, 'CreateFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateFile]
  end;
end;
{$ELSE}
function CreateFile; external kernel32 name 'CreateFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ReOpenFile: Pointer;

function ReOpenFile;
begin
  GetProcedureAddress(_ReOpenFile, kernel32, 'ReOpenFile');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReOpenFile]
  end;
end;
{$ELSE}
function ReOpenFile; external kernel32 name 'ReOpenFile';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileAttributesA: Pointer;

function SetFileAttributesA;
begin
  GetProcedureAddress(_SetFileAttributesA, kernel32, 'SetFileAttributesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileAttributesA]
  end;
end;
{$ELSE}
function SetFileAttributesA; external kernel32 name 'SetFileAttributesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileAttributesW: Pointer;

function SetFileAttributesW;
begin
  GetProcedureAddress(_SetFileAttributesW, kernel32, 'SetFileAttributesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileAttributesW]
  end;
end;
{$ELSE}
function SetFileAttributesW; external kernel32 name 'SetFileAttributesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileAttributes: Pointer;

function SetFileAttributes;
begin
  GetProcedureAddress(_SetFileAttributes, kernel32, 'SetFileAttributesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileAttributes]
  end;
end;
{$ELSE}
function SetFileAttributes; external kernel32 name 'SetFileAttributesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileAttributes: Pointer;

function SetFileAttributes;
begin
  GetProcedureAddress(_SetFileAttributes, kernel32, 'SetFileAttributesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileAttributes]
  end;
end;
{$ELSE}
function SetFileAttributes; external kernel32 name 'SetFileAttributesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributesA: Pointer;

function GetFileAttributesA;
begin
  GetProcedureAddress(_GetFileAttributesA, kernel32, 'GetFileAttributesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributesA]
  end;
end;
{$ELSE}
function GetFileAttributesA; external kernel32 name 'GetFileAttributesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributesW: Pointer;

function GetFileAttributesW;
begin
  GetProcedureAddress(_GetFileAttributesW, kernel32, 'GetFileAttributesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributesW]
  end;
end;
{$ELSE}
function GetFileAttributesW; external kernel32 name 'GetFileAttributesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributes: Pointer;

function GetFileAttributes;
begin
  GetProcedureAddress(_GetFileAttributes, kernel32, 'GetFileAttributesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributes]
  end;
end;
{$ELSE}
function GetFileAttributes; external kernel32 name 'GetFileAttributesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributes: Pointer;

function GetFileAttributes;
begin
  GetProcedureAddress(_GetFileAttributes, kernel32, 'GetFileAttributesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributes]
  end;
end;
{$ELSE}
function GetFileAttributes; external kernel32 name 'GetFileAttributesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributesExA: Pointer;

function GetFileAttributesExA;
begin
  GetProcedureAddress(_GetFileAttributesExA, kernel32, 'GetFileAttributesExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributesExA]
  end;
end;
{$ELSE}
function GetFileAttributesExA; external kernel32 name 'GetFileAttributesExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributesExW: Pointer;

function GetFileAttributesExW;
begin
  GetProcedureAddress(_GetFileAttributesExW, kernel32, 'GetFileAttributesExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributesExW]
  end;
end;
{$ELSE}
function GetFileAttributesExW; external kernel32 name 'GetFileAttributesExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributesEx: Pointer;

function GetFileAttributesEx;
begin
  GetProcedureAddress(_GetFileAttributesEx, kernel32, 'GetFileAttributesExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributesEx]
  end;
end;
{$ELSE}
function GetFileAttributesEx; external kernel32 name 'GetFileAttributesExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileAttributesEx: Pointer;

function GetFileAttributesEx;
begin
  GetProcedureAddress(_GetFileAttributesEx, kernel32, 'GetFileAttributesExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileAttributesEx]
  end;
end;
{$ELSE}
function GetFileAttributesEx; external kernel32 name 'GetFileAttributesExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCompressedFileSizeA: Pointer;

function GetCompressedFileSizeA;
begin
  GetProcedureAddress(_GetCompressedFileSizeA, kernel32, 'GetCompressedFileSizeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCompressedFileSizeA]
  end;
end;
{$ELSE}
function GetCompressedFileSizeA; external kernel32 name 'GetCompressedFileSizeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCompressedFileSizeW: Pointer;

function GetCompressedFileSizeW;
begin
  GetProcedureAddress(_GetCompressedFileSizeW, kernel32, 'GetCompressedFileSizeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCompressedFileSizeW]
  end;
end;
{$ELSE}
function GetCompressedFileSizeW; external kernel32 name 'GetCompressedFileSizeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCompressedFileSize: Pointer;

function GetCompressedFileSize;
begin
  GetProcedureAddress(_GetCompressedFileSize, kernel32, 'GetCompressedFileSizeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCompressedFileSize]
  end;
end;
{$ELSE}
function GetCompressedFileSize; external kernel32 name 'GetCompressedFileSizeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCompressedFileSize: Pointer;

function GetCompressedFileSize;
begin
  GetProcedureAddress(_GetCompressedFileSize, kernel32, 'GetCompressedFileSizeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCompressedFileSize]
  end;
end;
{$ELSE}
function GetCompressedFileSize; external kernel32 name 'GetCompressedFileSizeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteFileA: Pointer;

function DeleteFileA;
begin
  GetProcedureAddress(_DeleteFileA, kernel32, 'DeleteFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteFileA]
  end;
end;
{$ELSE}
function DeleteFileA; external kernel32 name 'DeleteFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteFileW: Pointer;

function DeleteFileW;
begin
  GetProcedureAddress(_DeleteFileW, kernel32, 'DeleteFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteFileW]
  end;
end;
{$ELSE}
function DeleteFileW; external kernel32 name 'DeleteFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteFile: Pointer;

function DeleteFile;
begin
  GetProcedureAddress(_DeleteFile, kernel32, 'DeleteFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteFile]
  end;
end;
{$ELSE}
function DeleteFile; external kernel32 name 'DeleteFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteFile: Pointer;

function DeleteFile;
begin
  GetProcedureAddress(_DeleteFile, kernel32, 'DeleteFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteFile]
  end;
end;
{$ELSE}
function DeleteFile; external kernel32 name 'DeleteFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFileExA: Pointer;

function FindFirstFileExA;
begin
  GetProcedureAddress(_FindFirstFileExA, kernel32, 'FindFirstFileExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFileExA]
  end;
end;
{$ELSE}
function FindFirstFileExA; external kernel32 name 'FindFirstFileExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFileExW: Pointer;

function FindFirstFileExW;
begin
  GetProcedureAddress(_FindFirstFileExW, kernel32, 'FindFirstFileExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFileExW]
  end;
end;
{$ELSE}
function FindFirstFileExW; external kernel32 name 'FindFirstFileExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFileEx: Pointer;

function FindFirstFileEx;
begin
  GetProcedureAddress(_FindFirstFileEx, kernel32, 'FindFirstFileExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFileEx]
  end;
end;
{$ELSE}
function FindFirstFileEx; external kernel32 name 'FindFirstFileExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFileEx: Pointer;

function FindFirstFileEx;
begin
  GetProcedureAddress(_FindFirstFileEx, kernel32, 'FindFirstFileExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFileEx]
  end;
end;
{$ELSE}
function FindFirstFileEx; external kernel32 name 'FindFirstFileExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFileA: Pointer;

function FindFirstFileA;
begin
  GetProcedureAddress(_FindFirstFileA, kernel32, 'FindFirstFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFileA]
  end;
end;
{$ELSE}
function FindFirstFileA; external kernel32 name 'FindFirstFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFileW: Pointer;

function FindFirstFileW;
begin
  GetProcedureAddress(_FindFirstFileW, kernel32, 'FindFirstFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFileW]
  end;
end;
{$ELSE}
function FindFirstFileW; external kernel32 name 'FindFirstFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFile: Pointer;

function FindFirstFile;
begin
  GetProcedureAddress(_FindFirstFile, kernel32, 'FindFirstFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFile]
  end;
end;
{$ELSE}
function FindFirstFile; external kernel32 name 'FindFirstFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFile: Pointer;

function FindFirstFile;
begin
  GetProcedureAddress(_FindFirstFile, kernel32, 'FindFirstFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFile]
  end;
end;
{$ELSE}
function FindFirstFile; external kernel32 name 'FindFirstFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextFileA: Pointer;

function FindNextFileA;
begin
  GetProcedureAddress(_FindNextFileA, kernel32, 'FindNextFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextFileA]
  end;
end;
{$ELSE}
function FindNextFileA; external kernel32 name 'FindNextFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextFileW: Pointer;

function FindNextFileW;
begin
  GetProcedureAddress(_FindNextFileW, kernel32, 'FindNextFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextFileW]
  end;
end;
{$ELSE}
function FindNextFileW; external kernel32 name 'FindNextFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextFile: Pointer;

function FindNextFile;
begin
  GetProcedureAddress(_FindNextFile, kernel32, 'FindNextFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextFile]
  end;
end;
{$ELSE}
function FindNextFile; external kernel32 name 'FindNextFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextFile: Pointer;

function FindNextFile;
begin
  GetProcedureAddress(_FindNextFile, kernel32, 'FindNextFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextFile]
  end;
end;
{$ELSE}
function FindNextFile; external kernel32 name 'FindNextFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SearchPathA: Pointer;

function SearchPathA;
begin
  GetProcedureAddress(_SearchPathA, kernel32, 'SearchPathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SearchPathA]
  end;
end;
{$ELSE}
function SearchPathA; external kernel32 name 'SearchPathA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SearchPathW: Pointer;

function SearchPathW;
begin
  GetProcedureAddress(_SearchPathW, kernel32, 'SearchPathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SearchPathW]
  end;
end;
{$ELSE}
function SearchPathW; external kernel32 name 'SearchPathW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SearchPath: Pointer;

function SearchPath;
begin
  GetProcedureAddress(_SearchPath, kernel32, 'SearchPathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SearchPath]
  end;
end;
{$ELSE}
function SearchPath; external kernel32 name 'SearchPathW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SearchPath: Pointer;

function SearchPath;
begin
  GetProcedureAddress(_SearchPath, kernel32, 'SearchPathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SearchPath]
  end;
end;
{$ELSE}
function SearchPath; external kernel32 name 'SearchPathA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFileA: Pointer;

function CopyFileA;
begin
  GetProcedureAddress(_CopyFileA, kernel32, 'CopyFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFileA]
  end;
end;
{$ELSE}
function CopyFileA; external kernel32 name 'CopyFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFileW: Pointer;

function CopyFileW;
begin
  GetProcedureAddress(_CopyFileW, kernel32, 'CopyFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFileW]
  end;
end;
{$ELSE}
function CopyFileW; external kernel32 name 'CopyFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFile: Pointer;

function CopyFile;
begin
  GetProcedureAddress(_CopyFile, kernel32, 'CopyFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFile]
  end;
end;
{$ELSE}
function CopyFile; external kernel32 name 'CopyFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFile: Pointer;

function CopyFile;
begin
  GetProcedureAddress(_CopyFile, kernel32, 'CopyFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFile]
  end;
end;
{$ELSE}
function CopyFile; external kernel32 name 'CopyFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFileExA: Pointer;

function CopyFileExA;
begin
  GetProcedureAddress(_CopyFileExA, kernel32, 'CopyFileExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFileExA]
  end;
end;
{$ELSE}
function CopyFileExA; external kernel32 name 'CopyFileExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFileExW: Pointer;

function CopyFileExW;
begin
  GetProcedureAddress(_CopyFileExW, kernel32, 'CopyFileExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFileExW]
  end;
end;
{$ELSE}
function CopyFileExW; external kernel32 name 'CopyFileExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFileEx: Pointer;

function CopyFileEx;
begin
  GetProcedureAddress(_CopyFileEx, kernel32, 'CopyFileExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFileEx]
  end;
end;
{$ELSE}
function CopyFileEx; external kernel32 name 'CopyFileExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyFileEx: Pointer;

function CopyFileEx;
begin
  GetProcedureAddress(_CopyFileEx, kernel32, 'CopyFileExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyFileEx]
  end;
end;
{$ELSE}
function CopyFileEx; external kernel32 name 'CopyFileExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileA: Pointer;

function MoveFileA;
begin
  GetProcedureAddress(_MoveFileA, kernel32, 'MoveFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileA]
  end;
end;
{$ELSE}
function MoveFileA; external kernel32 name 'MoveFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileW: Pointer;

function MoveFileW;
begin
  GetProcedureAddress(_MoveFileW, kernel32, 'MoveFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileW]
  end;
end;
{$ELSE}
function MoveFileW; external kernel32 name 'MoveFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFile: Pointer;

function MoveFile;
begin
  GetProcedureAddress(_MoveFile, kernel32, 'MoveFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFile]
  end;
end;
{$ELSE}
function MoveFile; external kernel32 name 'MoveFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFile: Pointer;

function MoveFile;
begin
  GetProcedureAddress(_MoveFile, kernel32, 'MoveFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFile]
  end;
end;
{$ELSE}
function MoveFile; external kernel32 name 'MoveFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileExA: Pointer;

function MoveFileExA;
begin
  GetProcedureAddress(_MoveFileExA, kernel32, 'MoveFileExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileExA]
  end;
end;
{$ELSE}
function MoveFileExA; external kernel32 name 'MoveFileExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileExW: Pointer;

function MoveFileExW;
begin
  GetProcedureAddress(_MoveFileExW, kernel32, 'MoveFileExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileExW]
  end;
end;
{$ELSE}
function MoveFileExW; external kernel32 name 'MoveFileExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileEx: Pointer;

function MoveFileEx;
begin
  GetProcedureAddress(_MoveFileEx, kernel32, 'MoveFileExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileEx]
  end;
end;
{$ELSE}
function MoveFileEx; external kernel32 name 'MoveFileExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileEx: Pointer;

function MoveFileEx;
begin
  GetProcedureAddress(_MoveFileEx, kernel32, 'MoveFileExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileEx]
  end;
end;
{$ELSE}
function MoveFileEx; external kernel32 name 'MoveFileExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileWithProgressA: Pointer;

function MoveFileWithProgressA;
begin
  GetProcedureAddress(_MoveFileWithProgressA, kernel32, 'MoveFileWithProgressA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileWithProgressA]
  end;
end;
{$ELSE}
function MoveFileWithProgressA; external kernel32 name 'MoveFileWithProgressA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileWithProgressW: Pointer;

function MoveFileWithProgressW;
begin
  GetProcedureAddress(_MoveFileWithProgressW, kernel32, 'MoveFileWithProgressW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileWithProgressW]
  end;
end;
{$ELSE}
function MoveFileWithProgressW; external kernel32 name 'MoveFileWithProgressW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileWithProgress: Pointer;

function MoveFileWithProgress;
begin
  GetProcedureAddress(_MoveFileWithProgress, kernel32, 'MoveFileWithProgressW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileWithProgress]
  end;
end;
{$ELSE}
function MoveFileWithProgress; external kernel32 name 'MoveFileWithProgressW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MoveFileWithProgress: Pointer;

function MoveFileWithProgress;
begin
  GetProcedureAddress(_MoveFileWithProgress, kernel32, 'MoveFileWithProgressA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveFileWithProgress]
  end;
end;
{$ELSE}
function MoveFileWithProgress; external kernel32 name 'MoveFileWithProgressA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ReplaceFileA: Pointer;

function ReplaceFileA;
begin
  GetProcedureAddress(_ReplaceFileA, kernel32, 'ReplaceFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReplaceFileA]
  end;
end;
{$ELSE}
function ReplaceFileA; external kernel32 name 'ReplaceFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReplaceFileW: Pointer;

function ReplaceFileW;
begin
  GetProcedureAddress(_ReplaceFileW, kernel32, 'ReplaceFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReplaceFileW]
  end;
end;
{$ELSE}
function ReplaceFileW; external kernel32 name 'ReplaceFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReplaceFile: Pointer;

function ReplaceFile;
begin
  GetProcedureAddress(_ReplaceFile, kernel32, 'ReplaceFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReplaceFile]
  end;
end;
{$ELSE}
function ReplaceFile; external kernel32 name 'ReplaceFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReplaceFile: Pointer;

function ReplaceFile;
begin
  GetProcedureAddress(_ReplaceFile, kernel32, 'ReplaceFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReplaceFile]
  end;
end;
{$ELSE}
function ReplaceFile; external kernel32 name 'ReplaceFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateHardLinkA: Pointer;

function CreateHardLinkA;
begin
  GetProcedureAddress(_CreateHardLinkA, kernel32, 'CreateHardLinkA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateHardLinkA]
  end;
end;
{$ELSE}
function CreateHardLinkA; external kernel32 name 'CreateHardLinkA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateHardLinkW: Pointer;

function CreateHardLinkW;
begin
  GetProcedureAddress(_CreateHardLinkW, kernel32, 'CreateHardLinkW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateHardLinkW]
  end;
end;
{$ELSE}
function CreateHardLinkW; external kernel32 name 'CreateHardLinkW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateHardLink: Pointer;

function CreateHardLink;
begin
  GetProcedureAddress(_CreateHardLink, kernel32, 'CreateHardLinkW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateHardLink]
  end;
end;
{$ELSE}
function CreateHardLink; external kernel32 name 'CreateHardLinkW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateHardLink: Pointer;

function CreateHardLink;
begin
  GetProcedureAddress(_CreateHardLink, kernel32, 'CreateHardLinkA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateHardLink]
  end;
end;
{$ELSE}
function CreateHardLink; external kernel32 name 'CreateHardLinkA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstStreamW: Pointer;

function FindFirstStreamW;
begin
  GetProcedureAddress(_FindFirstStreamW, kernel32, 'FindFirstStreamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstStreamW]
  end;
end;
{$ELSE}
function FindFirstStreamW; external kernel32 name 'FindFirstStreamW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextStreamW: Pointer;

function FindNextStreamW;
begin
  GetProcedureAddress(_FindNextStreamW, kernel32, 'FindNextStreamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextStreamW]
  end;
end;
{$ELSE}
function FindNextStreamW; external kernel32 name 'FindNextStreamW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateNamedPipeA: Pointer;

function CreateNamedPipeA;
begin
  GetProcedureAddress(_CreateNamedPipeA, kernel32, 'CreateNamedPipeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateNamedPipeA]
  end;
end;
{$ELSE}
function CreateNamedPipeA; external kernel32 name 'CreateNamedPipeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateNamedPipeW: Pointer;

function CreateNamedPipeW;
begin
  GetProcedureAddress(_CreateNamedPipeW, kernel32, 'CreateNamedPipeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateNamedPipeW]
  end;
end;
{$ELSE}
function CreateNamedPipeW; external kernel32 name 'CreateNamedPipeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateNamedPipe: Pointer;

function CreateNamedPipe;
begin
  GetProcedureAddress(_CreateNamedPipe, kernel32, 'CreateNamedPipeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateNamedPipe]
  end;
end;
{$ELSE}
function CreateNamedPipe; external kernel32 name 'CreateNamedPipeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateNamedPipe: Pointer;

function CreateNamedPipe;
begin
  GetProcedureAddress(_CreateNamedPipe, kernel32, 'CreateNamedPipeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateNamedPipe]
  end;
end;
{$ELSE}
function CreateNamedPipe; external kernel32 name 'CreateNamedPipeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetNamedPipeHandleStateA: Pointer;

function GetNamedPipeHandleStateA;
begin
  GetProcedureAddress(_GetNamedPipeHandleStateA, kernel32, 'GetNamedPipeHandleStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNamedPipeHandleStateA]
  end;
end;
{$ELSE}
function GetNamedPipeHandleStateA; external kernel32 name 'GetNamedPipeHandleStateA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNamedPipeHandleStateW: Pointer;

function GetNamedPipeHandleStateW;
begin
  GetProcedureAddress(_GetNamedPipeHandleStateW, kernel32, 'GetNamedPipeHandleStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNamedPipeHandleStateW]
  end;
end;
{$ELSE}
function GetNamedPipeHandleStateW; external kernel32 name 'GetNamedPipeHandleStateW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetNamedPipeHandleState: Pointer;

function GetNamedPipeHandleState;
begin
  GetProcedureAddress(_GetNamedPipeHandleState, kernel32, 'GetNamedPipeHandleStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNamedPipeHandleState]
  end;
end;
{$ELSE}
function GetNamedPipeHandleState; external kernel32 name 'GetNamedPipeHandleStateW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetNamedPipeHandleState: Pointer;

function GetNamedPipeHandleState;
begin
  GetProcedureAddress(_GetNamedPipeHandleState, kernel32, 'GetNamedPipeHandleStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNamedPipeHandleState]
  end;
end;
{$ELSE}
function GetNamedPipeHandleState; external kernel32 name 'GetNamedPipeHandleStateA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CallNamedPipeA: Pointer;

function CallNamedPipeA;
begin
  GetProcedureAddress(_CallNamedPipeA, kernel32, 'CallNamedPipeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallNamedPipeA]
  end;
end;
{$ELSE}
function CallNamedPipeA; external kernel32 name 'CallNamedPipeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CallNamedPipeW: Pointer;

function CallNamedPipeW;
begin
  GetProcedureAddress(_CallNamedPipeW, kernel32, 'CallNamedPipeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallNamedPipeW]
  end;
end;
{$ELSE}
function CallNamedPipeW; external kernel32 name 'CallNamedPipeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CallNamedPipe: Pointer;

function CallNamedPipe;
begin
  GetProcedureAddress(_CallNamedPipe, kernel32, 'CallNamedPipeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallNamedPipe]
  end;
end;
{$ELSE}
function CallNamedPipe; external kernel32 name 'CallNamedPipeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CallNamedPipe: Pointer;

function CallNamedPipe;
begin
  GetProcedureAddress(_CallNamedPipe, kernel32, 'CallNamedPipeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallNamedPipe]
  end;
end;
{$ELSE}
function CallNamedPipe; external kernel32 name 'CallNamedPipeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WaitNamedPipeA: Pointer;

function WaitNamedPipeA;
begin
  GetProcedureAddress(_WaitNamedPipeA, kernel32, 'WaitNamedPipeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitNamedPipeA]
  end;
end;
{$ELSE}
function WaitNamedPipeA; external kernel32 name 'WaitNamedPipeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitNamedPipeW: Pointer;

function WaitNamedPipeW;
begin
  GetProcedureAddress(_WaitNamedPipeW, kernel32, 'WaitNamedPipeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitNamedPipeW]
  end;
end;
{$ELSE}
function WaitNamedPipeW; external kernel32 name 'WaitNamedPipeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WaitNamedPipe: Pointer;

function WaitNamedPipe;
begin
  GetProcedureAddress(_WaitNamedPipe, kernel32, 'WaitNamedPipeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitNamedPipe]
  end;
end;
{$ELSE}
function WaitNamedPipe; external kernel32 name 'WaitNamedPipeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WaitNamedPipe: Pointer;

function WaitNamedPipe;
begin
  GetProcedureAddress(_WaitNamedPipe, kernel32, 'WaitNamedPipeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitNamedPipe]
  end;
end;
{$ELSE}
function WaitNamedPipe; external kernel32 name 'WaitNamedPipeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeLabelA: Pointer;

function SetVolumeLabelA;
begin
  GetProcedureAddress(_SetVolumeLabelA, kernel32, 'SetVolumeLabelA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeLabelA]
  end;
end;
{$ELSE}
function SetVolumeLabelA; external kernel32 name 'SetVolumeLabelA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeLabelW: Pointer;

function SetVolumeLabelW;
begin
  GetProcedureAddress(_SetVolumeLabelW, kernel32, 'SetVolumeLabelW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeLabelW]
  end;
end;
{$ELSE}
function SetVolumeLabelW; external kernel32 name 'SetVolumeLabelW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeLabel: Pointer;

function SetVolumeLabel;
begin
  GetProcedureAddress(_SetVolumeLabel, kernel32, 'SetVolumeLabelW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeLabel]
  end;
end;
{$ELSE}
function SetVolumeLabel; external kernel32 name 'SetVolumeLabelW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeLabel: Pointer;

function SetVolumeLabel;
begin
  GetProcedureAddress(_SetVolumeLabel, kernel32, 'SetVolumeLabelA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeLabel]
  end;
end;
{$ELSE}
function SetVolumeLabel; external kernel32 name 'SetVolumeLabelA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileApisToOEM: Pointer;

procedure SetFileApisToOEM;
begin
  GetProcedureAddress(_SetFileApisToOEM, kernel32, 'SetFileApisToOEM');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileApisToOEM]
  end;
end;
{$ELSE}
procedure SetFileApisToOEM; external kernel32 name 'SetFileApisToOEM';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileApisToANSI: Pointer;

procedure SetFileApisToANSI;
begin
  GetProcedureAddress(_SetFileApisToANSI, kernel32, 'SetFileApisToANSI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileApisToANSI]
  end;
end;
{$ELSE}
procedure SetFileApisToANSI; external kernel32 name 'SetFileApisToANSI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AreFileApisANSI: Pointer;

function AreFileApisANSI;
begin
  GetProcedureAddress(_AreFileApisANSI, kernel32, 'AreFileApisANSI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AreFileApisANSI]
  end;
end;
{$ELSE}
function AreFileApisANSI; external kernel32 name 'AreFileApisANSI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeInformationA: Pointer;

function GetVolumeInformationA;
begin
  GetProcedureAddress(_GetVolumeInformationA, kernel32, 'GetVolumeInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeInformationA]
  end;
end;
{$ELSE}
function GetVolumeInformationA; external kernel32 name 'GetVolumeInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeInformationW: Pointer;

function GetVolumeInformationW;
begin
  GetProcedureAddress(_GetVolumeInformationW, kernel32, 'GetVolumeInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeInformationW]
  end;
end;
{$ELSE}
function GetVolumeInformationW; external kernel32 name 'GetVolumeInformationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeInformation: Pointer;

function GetVolumeInformation;
begin
  GetProcedureAddress(_GetVolumeInformation, kernel32, 'GetVolumeInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeInformation]
  end;
end;
{$ELSE}
function GetVolumeInformation; external kernel32 name 'GetVolumeInformationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeInformation: Pointer;

function GetVolumeInformation;
begin
  GetProcedureAddress(_GetVolumeInformation, kernel32, 'GetVolumeInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeInformation]
  end;
end;
{$ELSE}
function GetVolumeInformation; external kernel32 name 'GetVolumeInformationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CancelIo: Pointer;

function CancelIo;
begin
  GetProcedureAddress(_CancelIo, kernel32, 'CancelIo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CancelIo]
  end;
end;
{$ELSE}
function CancelIo; external kernel32 name 'CancelIo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ClearEventLogA: Pointer;

function ClearEventLogA;
begin
  GetProcedureAddress(_ClearEventLogA, advapi32, 'ClearEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClearEventLogA]
  end;
end;
{$ELSE}
function ClearEventLogA; external advapi32 name 'ClearEventLogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ClearEventLogW: Pointer;

function ClearEventLogW;
begin
  GetProcedureAddress(_ClearEventLogW, advapi32, 'ClearEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClearEventLogW]
  end;
end;
{$ELSE}
function ClearEventLogW; external advapi32 name 'ClearEventLogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ClearEventLog: Pointer;

function ClearEventLog;
begin
  GetProcedureAddress(_ClearEventLog, advapi32, 'ClearEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClearEventLog]
  end;
end;
{$ELSE}
function ClearEventLog; external advapi32 name 'ClearEventLogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ClearEventLog: Pointer;

function ClearEventLog;
begin
  GetProcedureAddress(_ClearEventLog, advapi32, 'ClearEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClearEventLog]
  end;
end;
{$ELSE}
function ClearEventLog; external advapi32 name 'ClearEventLogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _BackupEventLogA: Pointer;

function BackupEventLogA;
begin
  GetProcedureAddress(_BackupEventLogA, advapi32, 'BackupEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupEventLogA]
  end;
end;
{$ELSE}
function BackupEventLogA; external advapi32 name 'BackupEventLogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BackupEventLogW: Pointer;

function BackupEventLogW;
begin
  GetProcedureAddress(_BackupEventLogW, advapi32, 'BackupEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupEventLogW]
  end;
end;
{$ELSE}
function BackupEventLogW; external advapi32 name 'BackupEventLogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _BackupEventLog: Pointer;

function BackupEventLog;
begin
  GetProcedureAddress(_BackupEventLog, advapi32, 'BackupEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupEventLog]
  end;
end;
{$ELSE}
function BackupEventLog; external advapi32 name 'BackupEventLogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _BackupEventLog: Pointer;

function BackupEventLog;
begin
  GetProcedureAddress(_BackupEventLog, advapi32, 'BackupEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BackupEventLog]
  end;
end;
{$ELSE}
function BackupEventLog; external advapi32 name 'BackupEventLogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CloseEventLog: Pointer;

function CloseEventLog;
begin
  GetProcedureAddress(_CloseEventLog, advapi32, 'CloseEventLog');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseEventLog]
  end;
end;
{$ELSE}
function CloseEventLog; external advapi32 name 'CloseEventLog';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeregisterEventSource: Pointer;

function DeregisterEventSource;
begin
  GetProcedureAddress(_DeregisterEventSource, advapi32, 'DeregisterEventSource');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeregisterEventSource]
  end;
end;
{$ELSE}
function DeregisterEventSource; external advapi32 name 'DeregisterEventSource';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NotifyChangeEventLog: Pointer;

function NotifyChangeEventLog;
begin
  GetProcedureAddress(_NotifyChangeEventLog, advapi32, 'NotifyChangeEventLog');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NotifyChangeEventLog]
  end;
end;
{$ELSE}
function NotifyChangeEventLog; external advapi32 name 'NotifyChangeEventLog';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberOfEventLogRecords: Pointer;

function GetNumberOfEventLogRecords;
begin
  GetProcedureAddress(_GetNumberOfEventLogRecords, advapi32, 'GetNumberOfEventLogRecords');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberOfEventLogRecords]
  end;
end;
{$ELSE}
function GetNumberOfEventLogRecords; external advapi32 name 'GetNumberOfEventLogRecords';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetOldestEventLogRecord: Pointer;

function GetOldestEventLogRecord;
begin
  GetProcedureAddress(_GetOldestEventLogRecord, advapi32, 'GetOldestEventLogRecord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOldestEventLogRecord]
  end;
end;
{$ELSE}
function GetOldestEventLogRecord; external advapi32 name 'GetOldestEventLogRecord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEventLogA: Pointer;

function OpenEventLogA;
begin
  GetProcedureAddress(_OpenEventLogA, advapi32, 'OpenEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEventLogA]
  end;
end;
{$ELSE}
function OpenEventLogA; external advapi32 name 'OpenEventLogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEventLogW: Pointer;

function OpenEventLogW;
begin
  GetProcedureAddress(_OpenEventLogW, advapi32, 'OpenEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEventLogW]
  end;
end;
{$ELSE}
function OpenEventLogW; external advapi32 name 'OpenEventLogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEventLog: Pointer;

function OpenEventLog;
begin
  GetProcedureAddress(_OpenEventLog, advapi32, 'OpenEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEventLog]
  end;
end;
{$ELSE}
function OpenEventLog; external advapi32 name 'OpenEventLogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenEventLog: Pointer;

function OpenEventLog;
begin
  GetProcedureAddress(_OpenEventLog, advapi32, 'OpenEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenEventLog]
  end;
end;
{$ELSE}
function OpenEventLog; external advapi32 name 'OpenEventLogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterEventSourceA: Pointer;

function RegisterEventSourceA;
begin
  GetProcedureAddress(_RegisterEventSourceA, advapi32, 'RegisterEventSourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterEventSourceA]
  end;
end;
{$ELSE}
function RegisterEventSourceA; external advapi32 name 'RegisterEventSourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterEventSourceW: Pointer;

function RegisterEventSourceW;
begin
  GetProcedureAddress(_RegisterEventSourceW, advapi32, 'RegisterEventSourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterEventSourceW]
  end;
end;
{$ELSE}
function RegisterEventSourceW; external advapi32 name 'RegisterEventSourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterEventSource: Pointer;

function RegisterEventSource;
begin
  GetProcedureAddress(_RegisterEventSource, advapi32, 'RegisterEventSourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterEventSource]
  end;
end;
{$ELSE}
function RegisterEventSource; external advapi32 name 'RegisterEventSourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterEventSource: Pointer;

function RegisterEventSource;
begin
  GetProcedureAddress(_RegisterEventSource, advapi32, 'RegisterEventSourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterEventSource]
  end;
end;
{$ELSE}
function RegisterEventSource; external advapi32 name 'RegisterEventSourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenBackupEventLogA: Pointer;

function OpenBackupEventLogA;
begin
  GetProcedureAddress(_OpenBackupEventLogA, advapi32, 'OpenBackupEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenBackupEventLogA]
  end;
end;
{$ELSE}
function OpenBackupEventLogA; external advapi32 name 'OpenBackupEventLogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenBackupEventLogW: Pointer;

function OpenBackupEventLogW;
begin
  GetProcedureAddress(_OpenBackupEventLogW, advapi32, 'OpenBackupEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenBackupEventLogW]
  end;
end;
{$ELSE}
function OpenBackupEventLogW; external advapi32 name 'OpenBackupEventLogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenBackupEventLog: Pointer;

function OpenBackupEventLog;
begin
  GetProcedureAddress(_OpenBackupEventLog, advapi32, 'OpenBackupEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenBackupEventLog]
  end;
end;
{$ELSE}
function OpenBackupEventLog; external advapi32 name 'OpenBackupEventLogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenBackupEventLog: Pointer;

function OpenBackupEventLog;
begin
  GetProcedureAddress(_OpenBackupEventLog, advapi32, 'OpenBackupEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenBackupEventLog]
  end;
end;
{$ELSE}
function OpenBackupEventLog; external advapi32 name 'OpenBackupEventLogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ReadEventLogA: Pointer;

function ReadEventLogA;
begin
  GetProcedureAddress(_ReadEventLogA, advapi32, 'ReadEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadEventLogA]
  end;
end;
{$ELSE}
function ReadEventLogA; external advapi32 name 'ReadEventLogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadEventLogW: Pointer;

function ReadEventLogW;
begin
  GetProcedureAddress(_ReadEventLogW, advapi32, 'ReadEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadEventLogW]
  end;
end;
{$ELSE}
function ReadEventLogW; external advapi32 name 'ReadEventLogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadEventLog: Pointer;

function ReadEventLog;
begin
  GetProcedureAddress(_ReadEventLog, advapi32, 'ReadEventLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadEventLog]
  end;
end;
{$ELSE}
function ReadEventLog; external advapi32 name 'ReadEventLogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadEventLog: Pointer;

function ReadEventLog;
begin
  GetProcedureAddress(_ReadEventLog, advapi32, 'ReadEventLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadEventLog]
  end;
end;
{$ELSE}
function ReadEventLog; external advapi32 name 'ReadEventLogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ReportEventA: Pointer;

function ReportEventA;
begin
  GetProcedureAddress(_ReportEventA, advapi32, 'ReportEventA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReportEventA]
  end;
end;
{$ELSE}
function ReportEventA; external advapi32 name 'ReportEventA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReportEventW: Pointer;

function ReportEventW;
begin
  GetProcedureAddress(_ReportEventW, advapi32, 'ReportEventW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReportEventW]
  end;
end;
{$ELSE}
function ReportEventW; external advapi32 name 'ReportEventW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReportEvent: Pointer;

function ReportEvent;
begin
  GetProcedureAddress(_ReportEvent, advapi32, 'ReportEventW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReportEvent]
  end;
end;
{$ELSE}
function ReportEvent; external advapi32 name 'ReportEventW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReportEvent: Pointer;

function ReportEvent;
begin
  GetProcedureAddress(_ReportEvent, advapi32, 'ReportEventA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReportEvent]
  end;
end;
{$ELSE}
function ReportEvent; external advapi32 name 'ReportEventA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetEventLogInformation: Pointer;

function GetEventLogInformation;
begin
  GetProcedureAddress(_GetEventLogInformation, advapi32, 'GetEventLogInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetEventLogInformation]
  end;
end;
{$ELSE}
function GetEventLogInformation; external advapi32 name 'GetEventLogInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DuplicateToken: Pointer;

function DuplicateToken;
begin
  GetProcedureAddress(_DuplicateToken, advapi32, 'DuplicateToken');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DuplicateToken]
  end;
end;
{$ELSE}
function DuplicateToken; external advapi32 name 'DuplicateToken';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKernelObjectSecurity: Pointer;

function GetKernelObjectSecurity;
begin
  GetProcedureAddress(_GetKernelObjectSecurity, advapi32, 'GetKernelObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKernelObjectSecurity]
  end;
end;
{$ELSE}
function GetKernelObjectSecurity; external advapi32 name 'GetKernelObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ImpersonateNamedPipeClient: Pointer;

function ImpersonateNamedPipeClient;
begin
  GetProcedureAddress(_ImpersonateNamedPipeClient, advapi32, 'ImpersonateNamedPipeClient');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImpersonateNamedPipeClient]
  end;
end;
{$ELSE}
function ImpersonateNamedPipeClient; external advapi32 name 'ImpersonateNamedPipeClient';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ImpersonateSelf: Pointer;

function ImpersonateSelf;
begin
  GetProcedureAddress(_ImpersonateSelf, advapi32, 'ImpersonateSelf');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImpersonateSelf]
  end;
end;
{$ELSE}
function ImpersonateSelf; external advapi32 name 'ImpersonateSelf';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RevertToSelf: Pointer;

function RevertToSelf;
begin
  GetProcedureAddress(_RevertToSelf, advapi32, 'RevertToSelf');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RevertToSelf]
  end;
end;
{$ELSE}
function RevertToSelf; external advapi32 name 'RevertToSelf';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadToken: Pointer;

function SetThreadToken;
begin
  GetProcedureAddress(_SetThreadToken, advapi32, 'SetThreadToken');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadToken]
  end;
end;
{$ELSE}
function SetThreadToken; external advapi32 name 'SetThreadToken';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheck: Pointer;

function AccessCheck;
begin
  GetProcedureAddress(_AccessCheck, advapi32, 'AccessCheck');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheck]
  end;
end;
{$ELSE}
function AccessCheck; external advapi32 name 'AccessCheck';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByType: Pointer;

function AccessCheckByType;
begin
  GetProcedureAddress(_AccessCheckByType, advapi32, 'AccessCheckByType');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByType]
  end;
end;
{$ELSE}
function AccessCheckByType; external advapi32 name 'AccessCheckByType';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeResultList: Pointer;

function AccessCheckByTypeResultList;
begin
  GetProcedureAddress(_AccessCheckByTypeResultList, advapi32, 'AccessCheckByTypeResultList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeResultList]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultList; external advapi32 name 'AccessCheckByTypeResultList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenProcessToken: Pointer;

function OpenProcessToken;
begin
  GetProcedureAddress(_OpenProcessToken, advapi32, 'OpenProcessToken');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenProcessToken]
  end;
end;
{$ELSE}
function OpenProcessToken; external advapi32 name 'OpenProcessToken';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenThreadToken: Pointer;

function OpenThreadToken;
begin
  GetProcedureAddress(_OpenThreadToken, advapi32, 'OpenThreadToken');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenThreadToken]
  end;
end;
{$ELSE}
function OpenThreadToken; external advapi32 name 'OpenThreadToken';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTokenInformation: Pointer;

function GetTokenInformation;
begin
  GetProcedureAddress(_GetTokenInformation, advapi32, 'GetTokenInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTokenInformation]
  end;
end;
{$ELSE}
function GetTokenInformation; external advapi32 name 'GetTokenInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTokenInformation: Pointer;

function SetTokenInformation;
begin
  GetProcedureAddress(_SetTokenInformation, advapi32, 'SetTokenInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTokenInformation]
  end;
end;
{$ELSE}
function SetTokenInformation; external advapi32 name 'SetTokenInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AdjustTokenPrivileges: Pointer;

function AdjustTokenPrivileges;
begin
  GetProcedureAddress(_AdjustTokenPrivileges, advapi32, 'AdjustTokenPrivileges');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdjustTokenPrivileges]
  end;
end;
{$ELSE}
function AdjustTokenPrivileges; external advapi32 name 'AdjustTokenPrivileges';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AdjustTokenGroups: Pointer;

function AdjustTokenGroups;
begin
  GetProcedureAddress(_AdjustTokenGroups, advapi32, 'AdjustTokenGroups');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdjustTokenGroups]
  end;
end;
{$ELSE}
function AdjustTokenGroups; external advapi32 name 'AdjustTokenGroups';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PrivilegeCheck: Pointer;

function PrivilegeCheck;
begin
  GetProcedureAddress(_PrivilegeCheck, advapi32, 'PrivilegeCheck');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivilegeCheck]
  end;
end;
{$ELSE}
function PrivilegeCheck; external advapi32 name 'PrivilegeCheck';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckAndAuditAlarmA: Pointer;

function AccessCheckAndAuditAlarmA;
begin
  GetProcedureAddress(_AccessCheckAndAuditAlarmA, advapi32, 'AccessCheckAndAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckAndAuditAlarmA]
  end;
end;
{$ELSE}
function AccessCheckAndAuditAlarmA; external advapi32 name 'AccessCheckAndAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckAndAuditAlarmW: Pointer;

function AccessCheckAndAuditAlarmW;
begin
  GetProcedureAddress(_AccessCheckAndAuditAlarmW, advapi32, 'AccessCheckAndAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckAndAuditAlarmW]
  end;
end;
{$ELSE}
function AccessCheckAndAuditAlarmW; external advapi32 name 'AccessCheckAndAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckAndAuditAlarm: Pointer;

function AccessCheckAndAuditAlarm;
begin
  GetProcedureAddress(_AccessCheckAndAuditAlarm, advapi32, 'AccessCheckAndAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckAndAuditAlarm]
  end;
end;
{$ELSE}
function AccessCheckAndAuditAlarm; external advapi32 name 'AccessCheckAndAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckAndAuditAlarm: Pointer;

function AccessCheckAndAuditAlarm;
begin
  GetProcedureAddress(_AccessCheckAndAuditAlarm, advapi32, 'AccessCheckAndAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckAndAuditAlarm]
  end;
end;
{$ELSE}
function AccessCheckAndAuditAlarm; external advapi32 name 'AccessCheckAndAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeAndAuditAlarmA: Pointer;

function AccessCheckByTypeAndAuditAlarmA;
begin
  GetProcedureAddress(_AccessCheckByTypeAndAuditAlarmA, advapi32, 'AccessCheckByTypeAndAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeAndAuditAlarmA]
  end;
end;
{$ELSE}
function AccessCheckByTypeAndAuditAlarmA; external advapi32 name 'AccessCheckByTypeAndAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeAndAuditAlarmW: Pointer;

function AccessCheckByTypeAndAuditAlarmW;
begin
  GetProcedureAddress(_AccessCheckByTypeAndAuditAlarmW, advapi32, 'AccessCheckByTypeAndAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeAndAuditAlarmW]
  end;
end;
{$ELSE}
function AccessCheckByTypeAndAuditAlarmW; external advapi32 name 'AccessCheckByTypeAndAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeAndAuditAlarm: Pointer;

function AccessCheckByTypeAndAuditAlarm;
begin
  GetProcedureAddress(_AccessCheckByTypeAndAuditAlarm, advapi32, 'AccessCheckByTypeAndAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeAndAuditAlarm]
  end;
end;
{$ELSE}
function AccessCheckByTypeAndAuditAlarm; external advapi32 name 'AccessCheckByTypeAndAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeAndAuditAlarm: Pointer;

function AccessCheckByTypeAndAuditAlarm;
begin
  GetProcedureAddress(_AccessCheckByTypeAndAuditAlarm, advapi32, 'AccessCheckByTypeAndAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeAndAuditAlarm]
  end;
end;
{$ELSE}
function AccessCheckByTypeAndAuditAlarm; external advapi32 name 'AccessCheckByTypeAndAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeResultListAndA: Pointer;

function AccessCheckByTypeResultListAndAuditAlarmA;
begin
  GetProcedureAddress(_AccessCheckByTypeResultListAndA, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeResultListAndA]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmA; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeResultListAndW: Pointer;

function AccessCheckByTypeResultListAndAuditAlarmW;
begin
  GetProcedureAddress(_AccessCheckByTypeResultListAndW, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeResultListAndW]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmW; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeResultListAnd: Pointer;

function AccessCheckByTypeResultListAndAuditAlarm;
begin
  GetProcedureAddress(_AccessCheckByTypeResultListAnd, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeResultListAnd]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarm; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTypeResultListAnd: Pointer;

function AccessCheckByTypeResultListAndAuditAlarm;
begin
  GetProcedureAddress(_AccessCheckByTypeResultListAnd, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTypeResultListAnd]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarm; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTRLAndAAByHA: Pointer;

function AccessCheckByTypeResultListAndAuditAlarmByHandleA;
begin
  GetProcedureAddress(_AccessCheckByTRLAndAAByHA, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmByHandleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTRLAndAAByHA]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmByHandleA; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmByHandleA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTRLAndAAByHW: Pointer;

function AccessCheckByTypeResultListAndAuditAlarmByHandleW;
begin
  GetProcedureAddress(_AccessCheckByTRLAndAAByHW, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmByHandleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTRLAndAAByHW]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmByHandleW; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmByHandleW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTRLAndAAByH: Pointer;

function AccessCheckByTypeResultListAndAuditAlarmByHandle;
begin
  GetProcedureAddress(_AccessCheckByTRLAndAAByH, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmByHandleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTRLAndAAByH]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmByHandle; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmByHandleW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AccessCheckByTRLAndAAByH: Pointer;

function AccessCheckByTypeResultListAndAuditAlarmByHandle;
begin
  GetProcedureAddress(_AccessCheckByTRLAndAAByH, advapi32, 'AccessCheckByTypeResultListAndAuditAlarmByHandleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AccessCheckByTRLAndAAByH]
  end;
end;
{$ELSE}
function AccessCheckByTypeResultListAndAuditAlarmByHandle; external advapi32 name 'AccessCheckByTypeResultListAndAuditAlarmByHandleA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectOpenAuditAlarmA: Pointer;

function ObjectOpenAuditAlarmA;
begin
  GetProcedureAddress(_ObjectOpenAuditAlarmA, advapi32, 'ObjectOpenAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectOpenAuditAlarmA]
  end;
end;
{$ELSE}
function ObjectOpenAuditAlarmA; external advapi32 name 'ObjectOpenAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectOpenAuditAlarmW: Pointer;

function ObjectOpenAuditAlarmW;
begin
  GetProcedureAddress(_ObjectOpenAuditAlarmW, advapi32, 'ObjectOpenAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectOpenAuditAlarmW]
  end;
end;
{$ELSE}
function ObjectOpenAuditAlarmW; external advapi32 name 'ObjectOpenAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectOpenAuditAlarm: Pointer;

function ObjectOpenAuditAlarm;
begin
  GetProcedureAddress(_ObjectOpenAuditAlarm, advapi32, 'ObjectOpenAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectOpenAuditAlarm]
  end;
end;
{$ELSE}
function ObjectOpenAuditAlarm; external advapi32 name 'ObjectOpenAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectOpenAuditAlarm: Pointer;

function ObjectOpenAuditAlarm;
begin
  GetProcedureAddress(_ObjectOpenAuditAlarm, advapi32, 'ObjectOpenAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectOpenAuditAlarm]
  end;
end;
{$ELSE}
function ObjectOpenAuditAlarm; external advapi32 name 'ObjectOpenAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectPrivilegeAuditAlarmA: Pointer;

function ObjectPrivilegeAuditAlarmA;
begin
  GetProcedureAddress(_ObjectPrivilegeAuditAlarmA, advapi32, 'ObjectPrivilegeAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectPrivilegeAuditAlarmA]
  end;
end;
{$ELSE}
function ObjectPrivilegeAuditAlarmA; external advapi32 name 'ObjectPrivilegeAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectPrivilegeAuditAlarmW: Pointer;

function ObjectPrivilegeAuditAlarmW;
begin
  GetProcedureAddress(_ObjectPrivilegeAuditAlarmW, advapi32, 'ObjectPrivilegeAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectPrivilegeAuditAlarmW]
  end;
end;
{$ELSE}
function ObjectPrivilegeAuditAlarmW; external advapi32 name 'ObjectPrivilegeAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectPrivilegeAuditAlarm: Pointer;

function ObjectPrivilegeAuditAlarm;
begin
  GetProcedureAddress(_ObjectPrivilegeAuditAlarm, advapi32, 'ObjectPrivilegeAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectPrivilegeAuditAlarm]
  end;
end;
{$ELSE}
function ObjectPrivilegeAuditAlarm; external advapi32 name 'ObjectPrivilegeAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectPrivilegeAuditAlarm: Pointer;

function ObjectPrivilegeAuditAlarm;
begin
  GetProcedureAddress(_ObjectPrivilegeAuditAlarm, advapi32, 'ObjectPrivilegeAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectPrivilegeAuditAlarm]
  end;
end;
{$ELSE}
function ObjectPrivilegeAuditAlarm; external advapi32 name 'ObjectPrivilegeAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectCloseAuditAlarmA: Pointer;

function ObjectCloseAuditAlarmA;
begin
  GetProcedureAddress(_ObjectCloseAuditAlarmA, advapi32, 'ObjectCloseAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectCloseAuditAlarmA]
  end;
end;
{$ELSE}
function ObjectCloseAuditAlarmA; external advapi32 name 'ObjectCloseAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectCloseAuditAlarmW: Pointer;

function ObjectCloseAuditAlarmW;
begin
  GetProcedureAddress(_ObjectCloseAuditAlarmW, advapi32, 'ObjectCloseAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectCloseAuditAlarmW]
  end;
end;
{$ELSE}
function ObjectCloseAuditAlarmW; external advapi32 name 'ObjectCloseAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectCloseAuditAlarm: Pointer;

function ObjectCloseAuditAlarm;
begin
  GetProcedureAddress(_ObjectCloseAuditAlarm, advapi32, 'ObjectCloseAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectCloseAuditAlarm]
  end;
end;
{$ELSE}
function ObjectCloseAuditAlarm; external advapi32 name 'ObjectCloseAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectCloseAuditAlarm: Pointer;

function ObjectCloseAuditAlarm;
begin
  GetProcedureAddress(_ObjectCloseAuditAlarm, advapi32, 'ObjectCloseAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectCloseAuditAlarm]
  end;
end;
{$ELSE}
function ObjectCloseAuditAlarm; external advapi32 name 'ObjectCloseAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectDeleteAuditAlarmA: Pointer;

function ObjectDeleteAuditAlarmA;
begin
  GetProcedureAddress(_ObjectDeleteAuditAlarmA, advapi32, 'ObjectDeleteAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectDeleteAuditAlarmA]
  end;
end;
{$ELSE}
function ObjectDeleteAuditAlarmA; external advapi32 name 'ObjectDeleteAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectDeleteAuditAlarmW: Pointer;

function ObjectDeleteAuditAlarmW;
begin
  GetProcedureAddress(_ObjectDeleteAuditAlarmW, advapi32, 'ObjectDeleteAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectDeleteAuditAlarmW]
  end;
end;
{$ELSE}
function ObjectDeleteAuditAlarmW; external advapi32 name 'ObjectDeleteAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectDeleteAuditAlarm: Pointer;

function ObjectDeleteAuditAlarm;
begin
  GetProcedureAddress(_ObjectDeleteAuditAlarm, advapi32, 'ObjectDeleteAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectDeleteAuditAlarm]
  end;
end;
{$ELSE}
function ObjectDeleteAuditAlarm; external advapi32 name 'ObjectDeleteAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ObjectDeleteAuditAlarm: Pointer;

function ObjectDeleteAuditAlarm;
begin
  GetProcedureAddress(_ObjectDeleteAuditAlarm, advapi32, 'ObjectDeleteAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ObjectDeleteAuditAlarm]
  end;
end;
{$ELSE}
function ObjectDeleteAuditAlarm; external advapi32 name 'ObjectDeleteAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _PrivilegedServiceAuditAlarmA: Pointer;

function PrivilegedServiceAuditAlarmA;
begin
  GetProcedureAddress(_PrivilegedServiceAuditAlarmA, advapi32, 'PrivilegedServiceAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivilegedServiceAuditAlarmA]
  end;
end;
{$ELSE}
function PrivilegedServiceAuditAlarmA; external advapi32 name 'PrivilegedServiceAuditAlarmA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PrivilegedServiceAuditAlarmW: Pointer;

function PrivilegedServiceAuditAlarmW;
begin
  GetProcedureAddress(_PrivilegedServiceAuditAlarmW, advapi32, 'PrivilegedServiceAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivilegedServiceAuditAlarmW]
  end;
end;
{$ELSE}
function PrivilegedServiceAuditAlarmW; external advapi32 name 'PrivilegedServiceAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PrivilegedServiceAuditAlarm: Pointer;

function PrivilegedServiceAuditAlarm;
begin
  GetProcedureAddress(_PrivilegedServiceAuditAlarm, advapi32, 'PrivilegedServiceAuditAlarmW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivilegedServiceAuditAlarm]
  end;
end;
{$ELSE}
function PrivilegedServiceAuditAlarm; external advapi32 name 'PrivilegedServiceAuditAlarmW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PrivilegedServiceAuditAlarm: Pointer;

function PrivilegedServiceAuditAlarm;
begin
  GetProcedureAddress(_PrivilegedServiceAuditAlarm, advapi32, 'PrivilegedServiceAuditAlarmA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivilegedServiceAuditAlarm]
  end;
end;
{$ELSE}
function PrivilegedServiceAuditAlarm; external advapi32 name 'PrivilegedServiceAuditAlarmA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsWellKnownSid: Pointer;

function IsWellKnownSid;
begin
  GetProcedureAddress(_IsWellKnownSid, advapi32, 'IsWellKnownSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWellKnownSid]
  end;
end;
{$ELSE}
function IsWellKnownSid; external advapi32 name 'IsWellKnownSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWellKnownSid: Pointer;

function CreateWellKnownSid;
begin
  GetProcedureAddress(_CreateWellKnownSid, advapi32, 'CreateWellKnownSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWellKnownSid]
  end;
end;
{$ELSE}
function CreateWellKnownSid; external advapi32 name 'CreateWellKnownSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EqualDomainSid: Pointer;

function EqualDomainSid;
begin
  GetProcedureAddress(_EqualDomainSid, advapi32, 'EqualDomainSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EqualDomainSid]
  end;
end;
{$ELSE}
function EqualDomainSid; external advapi32 name 'EqualDomainSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowsAccountDomainSid: Pointer;

function GetWindowsAccountDomainSid;
begin
  GetProcedureAddress(_GetWindowsAccountDomainSid, advapi32, 'GetWindowsAccountDomainSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowsAccountDomainSid]
  end;
end;
{$ELSE}
function GetWindowsAccountDomainSid; external advapi32 name 'GetWindowsAccountDomainSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsValidSid: Pointer;

function IsValidSid;
begin
  GetProcedureAddress(_IsValidSid, advapi32, 'IsValidSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsValidSid]
  end;
end;
{$ELSE}
function IsValidSid; external advapi32 name 'IsValidSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EqualSid: Pointer;

function EqualSid;
begin
  GetProcedureAddress(_EqualSid, advapi32, 'EqualSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EqualSid]
  end;
end;
{$ELSE}
function EqualSid; external advapi32 name 'EqualSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EqualPrefixSid: Pointer;

function EqualPrefixSid;
begin
  GetProcedureAddress(_EqualPrefixSid, advapi32, 'EqualPrefixSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EqualPrefixSid]
  end;
end;
{$ELSE}
function EqualPrefixSid; external advapi32 name 'EqualPrefixSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSidLengthRequired: Pointer;

function GetSidLengthRequired;
begin
  GetProcedureAddress(_GetSidLengthRequired, advapi32, 'GetSidLengthRequired');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSidLengthRequired]
  end;
end;
{$ELSE}
function GetSidLengthRequired; external advapi32 name 'GetSidLengthRequired';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AllocateAndInitializeSid: Pointer;

function AllocateAndInitializeSid;
begin
  GetProcedureAddress(_AllocateAndInitializeSid, advapi32, 'AllocateAndInitializeSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllocateAndInitializeSid]
  end;
end;
{$ELSE}
function AllocateAndInitializeSid; external advapi32 name 'AllocateAndInitializeSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeSid: Pointer;

function FreeSid;
begin
  GetProcedureAddress(_FreeSid, advapi32, 'FreeSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeSid]
  end;
end;
{$ELSE}
function FreeSid; external advapi32 name 'FreeSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitializeSid: Pointer;

function InitializeSid;
begin
  GetProcedureAddress(_InitializeSid, advapi32, 'InitializeSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitializeSid]
  end;
end;
{$ELSE}
function InitializeSid; external advapi32 name 'InitializeSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSidIdentifierAuthority: Pointer;

function GetSidIdentifierAuthority;
begin
  GetProcedureAddress(_GetSidIdentifierAuthority, advapi32, 'GetSidIdentifierAuthority');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSidIdentifierAuthority]
  end;
end;
{$ELSE}
function GetSidIdentifierAuthority; external advapi32 name 'GetSidIdentifierAuthority';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSidSubAuthority: Pointer;

function GetSidSubAuthority;
begin
  GetProcedureAddress(_GetSidSubAuthority, advapi32, 'GetSidSubAuthority');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSidSubAuthority]
  end;
end;
{$ELSE}
function GetSidSubAuthority; external advapi32 name 'GetSidSubAuthority';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSidSubAuthorityCount: Pointer;

function GetSidSubAuthorityCount;
begin
  GetProcedureAddress(_GetSidSubAuthorityCount, advapi32, 'GetSidSubAuthorityCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSidSubAuthorityCount]
  end;
end;
{$ELSE}
function GetSidSubAuthorityCount; external advapi32 name 'GetSidSubAuthorityCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLengthSid: Pointer;

function GetLengthSid;
begin
  GetProcedureAddress(_GetLengthSid, advapi32, 'GetLengthSid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLengthSid]
  end;
end;
{$ELSE}
function GetLengthSid; external advapi32 name 'GetLengthSid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopySid: Pointer;

function CopySid;
begin
  GetProcedureAddress(_CopySid, advapi32, 'CopySid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopySid]
  end;
end;
{$ELSE}
function CopySid; external advapi32 name 'CopySid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AreAllAccessesGranted: Pointer;

function AreAllAccessesGranted;
begin
  GetProcedureAddress(_AreAllAccessesGranted, advapi32, 'AreAllAccessesGranted');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AreAllAccessesGranted]
  end;
end;
{$ELSE}
function AreAllAccessesGranted; external advapi32 name 'AreAllAccessesGranted';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AreAnyAccessesGranted: Pointer;

function AreAnyAccessesGranted;
begin
  GetProcedureAddress(_AreAnyAccessesGranted, advapi32, 'AreAnyAccessesGranted');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AreAnyAccessesGranted]
  end;
end;
{$ELSE}
function AreAnyAccessesGranted; external advapi32 name 'AreAnyAccessesGranted';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapGenericMask: Pointer;

procedure MapGenericMask;
begin
  GetProcedureAddress(_MapGenericMask, advapi32, 'MapGenericMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapGenericMask]
  end;
end;
{$ELSE}
procedure MapGenericMask; external advapi32 name 'MapGenericMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsValidAcl: Pointer;

function IsValidAcl;
begin
  GetProcedureAddress(_IsValidAcl, advapi32, 'IsValidAcl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsValidAcl]
  end;
end;
{$ELSE}
function IsValidAcl; external advapi32 name 'IsValidAcl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitializeAcl: Pointer;

function InitializeAcl;
begin
  GetProcedureAddress(_InitializeAcl, advapi32, 'InitializeAcl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitializeAcl]
  end;
end;
{$ELSE}
function InitializeAcl; external advapi32 name 'InitializeAcl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAclInformation: Pointer;

function GetAclInformation;
begin
  GetProcedureAddress(_GetAclInformation, advapi32, 'GetAclInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAclInformation]
  end;
end;
{$ELSE}
function GetAclInformation; external advapi32 name 'GetAclInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetAclInformation: Pointer;

function SetAclInformation;
begin
  GetProcedureAddress(_SetAclInformation, advapi32, 'SetAclInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetAclInformation]
  end;
end;
{$ELSE}
function SetAclInformation; external advapi32 name 'SetAclInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAce: Pointer;

function AddAce;
begin
  GetProcedureAddress(_AddAce, advapi32, 'AddAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAce]
  end;
end;
{$ELSE}
function AddAce; external advapi32 name 'AddAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteAce: Pointer;

function DeleteAce;
begin
  GetProcedureAddress(_DeleteAce, advapi32, 'DeleteAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteAce]
  end;
end;
{$ELSE}
function DeleteAce; external advapi32 name 'DeleteAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAce: Pointer;

function GetAce;
begin
  GetProcedureAddress(_GetAce, advapi32, 'GetAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAce]
  end;
end;
{$ELSE}
function GetAce; external advapi32 name 'GetAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAccessAllowedAce: Pointer;

function AddAccessAllowedAce;
begin
  GetProcedureAddress(_AddAccessAllowedAce, advapi32, 'AddAccessAllowedAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAccessAllowedAce]
  end;
end;
{$ELSE}
function AddAccessAllowedAce; external advapi32 name 'AddAccessAllowedAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAccessAllowedAceEx: Pointer;

function AddAccessAllowedAceEx;
begin
  GetProcedureAddress(_AddAccessAllowedAceEx, advapi32, 'AddAccessAllowedAceEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAccessAllowedAceEx]
  end;
end;
{$ELSE}
function AddAccessAllowedAceEx; external advapi32 name 'AddAccessAllowedAceEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAccessDeniedAce: Pointer;

function AddAccessDeniedAce;
begin
  GetProcedureAddress(_AddAccessDeniedAce, advapi32, 'AddAccessDeniedAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAccessDeniedAce]
  end;
end;
{$ELSE}
function AddAccessDeniedAce; external advapi32 name 'AddAccessDeniedAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAccessDeniedAceEx: Pointer;

function AddAccessDeniedAceEx;
begin
  GetProcedureAddress(_AddAccessDeniedAceEx, advapi32, 'AddAccessDeniedAceEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAccessDeniedAceEx]
  end;
end;
{$ELSE}
function AddAccessDeniedAceEx; external advapi32 name 'AddAccessDeniedAceEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAuditAccessAce: Pointer;

function AddAuditAccessAce;
begin
  GetProcedureAddress(_AddAuditAccessAce, advapi32, 'AddAuditAccessAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAuditAccessAce]
  end;
end;
{$ELSE}
function AddAuditAccessAce; external advapi32 name 'AddAuditAccessAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAuditAccessAceEx: Pointer;

function AddAuditAccessAceEx;
begin
  GetProcedureAddress(_AddAuditAccessAceEx, advapi32, 'AddAuditAccessAceEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAuditAccessAceEx]
  end;
end;
{$ELSE}
function AddAuditAccessAceEx; external advapi32 name 'AddAuditAccessAceEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAccessAllowedObjectAce: Pointer;

function AddAccessAllowedObjectAce;
begin
  GetProcedureAddress(_AddAccessAllowedObjectAce, advapi32, 'AddAccessAllowedObjectAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAccessAllowedObjectAce]
  end;
end;
{$ELSE}
function AddAccessAllowedObjectAce; external advapi32 name 'AddAccessAllowedObjectAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAccessDeniedObjectAce: Pointer;

function AddAccessDeniedObjectAce;
begin
  GetProcedureAddress(_AddAccessDeniedObjectAce, advapi32, 'AddAccessDeniedObjectAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAccessDeniedObjectAce]
  end;
end;
{$ELSE}
function AddAccessDeniedObjectAce; external advapi32 name 'AddAccessDeniedObjectAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddAuditAccessObjectAce: Pointer;

function AddAuditAccessObjectAce;
begin
  GetProcedureAddress(_AddAuditAccessObjectAce, advapi32, 'AddAuditAccessObjectAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddAuditAccessObjectAce]
  end;
end;
{$ELSE}
function AddAuditAccessObjectAce; external advapi32 name 'AddAuditAccessObjectAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstFreeAce: Pointer;

function FindFirstFreeAce;
begin
  GetProcedureAddress(_FindFirstFreeAce, advapi32, 'FindFirstFreeAce');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstFreeAce]
  end;
end;
{$ELSE}
function FindFirstFreeAce; external advapi32 name 'FindFirstFreeAce';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InitializeSecurityDescriptor: Pointer;

function InitializeSecurityDescriptor;
begin
  GetProcedureAddress(_InitializeSecurityDescriptor, advapi32, 'InitializeSecurityDescriptor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InitializeSecurityDescriptor]
  end;
end;
{$ELSE}
function InitializeSecurityDescriptor; external advapi32 name 'InitializeSecurityDescriptor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsValidSecurityDescriptor: Pointer;

function IsValidSecurityDescriptor;
begin
  GetProcedureAddress(_IsValidSecurityDescriptor, advapi32, 'IsValidSecurityDescriptor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsValidSecurityDescriptor]
  end;
end;
{$ELSE}
function IsValidSecurityDescriptor; external advapi32 name 'IsValidSecurityDescriptor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorLength: Pointer;

function GetSecurityDescriptorLength;
begin
  GetProcedureAddress(_GetSecurityDescriptorLength, advapi32, 'GetSecurityDescriptorLength');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorLength]
  end;
end;
{$ELSE}
function GetSecurityDescriptorLength; external advapi32 name 'GetSecurityDescriptorLength';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorControl: Pointer;

function GetSecurityDescriptorControl;
begin
  GetProcedureAddress(_GetSecurityDescriptorControl, advapi32, 'GetSecurityDescriptorControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorControl]
  end;
end;
{$ELSE}
function GetSecurityDescriptorControl; external advapi32 name 'GetSecurityDescriptorControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSecurityDescriptorControl: Pointer;

function SetSecurityDescriptorControl;
begin
  GetProcedureAddress(_SetSecurityDescriptorControl, advapi32, 'SetSecurityDescriptorControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSecurityDescriptorControl]
  end;
end;
{$ELSE}
function SetSecurityDescriptorControl; external advapi32 name 'SetSecurityDescriptorControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSecurityDescriptorDacl: Pointer;

function SetSecurityDescriptorDacl;
begin
  GetProcedureAddress(_SetSecurityDescriptorDacl, advapi32, 'SetSecurityDescriptorDacl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSecurityDescriptorDacl]
  end;
end;
{$ELSE}
function SetSecurityDescriptorDacl; external advapi32 name 'SetSecurityDescriptorDacl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorDacl: Pointer;

function GetSecurityDescriptorDacl;
begin
  GetProcedureAddress(_GetSecurityDescriptorDacl, advapi32, 'GetSecurityDescriptorDacl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorDacl]
  end;
end;
{$ELSE}
function GetSecurityDescriptorDacl; external advapi32 name 'GetSecurityDescriptorDacl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSecurityDescriptorSacl: Pointer;

function SetSecurityDescriptorSacl;
begin
  GetProcedureAddress(_SetSecurityDescriptorSacl, advapi32, 'SetSecurityDescriptorSacl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSecurityDescriptorSacl]
  end;
end;
{$ELSE}
function SetSecurityDescriptorSacl; external advapi32 name 'SetSecurityDescriptorSacl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorSacl: Pointer;

function GetSecurityDescriptorSacl;
begin
  GetProcedureAddress(_GetSecurityDescriptorSacl, advapi32, 'GetSecurityDescriptorSacl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorSacl]
  end;
end;
{$ELSE}
function GetSecurityDescriptorSacl; external advapi32 name 'GetSecurityDescriptorSacl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSecurityDescriptorOwner: Pointer;

function SetSecurityDescriptorOwner;
begin
  GetProcedureAddress(_SetSecurityDescriptorOwner, advapi32, 'SetSecurityDescriptorOwner');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSecurityDescriptorOwner]
  end;
end;
{$ELSE}
function SetSecurityDescriptorOwner; external advapi32 name 'SetSecurityDescriptorOwner';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorOwner: Pointer;

function GetSecurityDescriptorOwner;
begin
  GetProcedureAddress(_GetSecurityDescriptorOwner, advapi32, 'GetSecurityDescriptorOwner');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorOwner]
  end;
end;
{$ELSE}
function GetSecurityDescriptorOwner; external advapi32 name 'GetSecurityDescriptorOwner';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSecurityDescriptorGroup: Pointer;

function SetSecurityDescriptorGroup;
begin
  GetProcedureAddress(_SetSecurityDescriptorGroup, advapi32, 'SetSecurityDescriptorGroup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSecurityDescriptorGroup]
  end;
end;
{$ELSE}
function SetSecurityDescriptorGroup; external advapi32 name 'SetSecurityDescriptorGroup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorGroup: Pointer;

function GetSecurityDescriptorGroup;
begin
  GetProcedureAddress(_GetSecurityDescriptorGroup, advapi32, 'GetSecurityDescriptorGroup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorGroup]
  end;
end;
{$ELSE}
function GetSecurityDescriptorGroup; external advapi32 name 'GetSecurityDescriptorGroup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSecurityDescriptorRMControl: Pointer;

function SetSecurityDescriptorRMControl;
begin
  GetProcedureAddress(_SetSecurityDescriptorRMControl, advapi32, 'SetSecurityDescriptorRMControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSecurityDescriptorRMControl]
  end;
end;
{$ELSE}
function SetSecurityDescriptorRMControl; external advapi32 name 'SetSecurityDescriptorRMControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSecurityDescriptorRMControl: Pointer;

function GetSecurityDescriptorRMControl;
begin
  GetProcedureAddress(_GetSecurityDescriptorRMControl, advapi32, 'GetSecurityDescriptorRMControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSecurityDescriptorRMControl]
  end;
end;
{$ELSE}
function GetSecurityDescriptorRMControl; external advapi32 name 'GetSecurityDescriptorRMControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePrivateObjectSecurity: Pointer;

function CreatePrivateObjectSecurity;
begin
  GetProcedureAddress(_CreatePrivateObjectSecurity, advapi32, 'CreatePrivateObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePrivateObjectSecurity]
  end;
end;
{$ELSE}
function CreatePrivateObjectSecurity; external advapi32 name 'CreatePrivateObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertToAutoInheritPrObjSec: Pointer;

function ConvertToAutoInheritPrivateObjectSecurity;
begin
  GetProcedureAddress(_ConvertToAutoInheritPrObjSec, advapi32, 'ConvertToAutoInheritPrivateObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertToAutoInheritPrObjSec]
  end;
end;
{$ELSE}
function ConvertToAutoInheritPrivateObjectSecurity; external advapi32 name 'ConvertToAutoInheritPrivateObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePrivateObjectSecurityEx: Pointer;

function CreatePrivateObjectSecurityEx;
begin
  GetProcedureAddress(_CreatePrivateObjectSecurityEx, advapi32, 'CreatePrivateObjectSecurityEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePrivateObjectSecurityEx]
  end;
end;
{$ELSE}
function CreatePrivateObjectSecurityEx; external advapi32 name 'CreatePrivateObjectSecurityEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPrivateObjectSecurity: Pointer;

function SetPrivateObjectSecurity;
begin
  GetProcedureAddress(_SetPrivateObjectSecurity, advapi32, 'SetPrivateObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPrivateObjectSecurity]
  end;
end;
{$ELSE}
function SetPrivateObjectSecurity; external advapi32 name 'SetPrivateObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPrivateObjectSecurityEx: Pointer;

function SetPrivateObjectSecurityEx;
begin
  GetProcedureAddress(_SetPrivateObjectSecurityEx, advapi32, 'SetPrivateObjectSecurityEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPrivateObjectSecurityEx]
  end;
end;
{$ELSE}
function SetPrivateObjectSecurityEx; external advapi32 name 'SetPrivateObjectSecurityEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPrivateObjectSecurity: Pointer;

function GetPrivateObjectSecurity;
begin
  GetProcedureAddress(_GetPrivateObjectSecurity, advapi32, 'GetPrivateObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPrivateObjectSecurity]
  end;
end;
{$ELSE}
function GetPrivateObjectSecurity; external advapi32 name 'GetPrivateObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyPrivateObjectSecurity: Pointer;

function DestroyPrivateObjectSecurity;
begin
  GetProcedureAddress(_DestroyPrivateObjectSecurity, advapi32, 'DestroyPrivateObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyPrivateObjectSecurity]
  end;
end;
{$ELSE}
function DestroyPrivateObjectSecurity; external advapi32 name 'DestroyPrivateObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MakeSelfRelativeSD: Pointer;

function MakeSelfRelativeSD;
begin
  GetProcedureAddress(_MakeSelfRelativeSD, advapi32, 'MakeSelfRelativeSD');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MakeSelfRelativeSD]
  end;
end;
{$ELSE}
function MakeSelfRelativeSD; external advapi32 name 'MakeSelfRelativeSD';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MakeAbsoluteSD: Pointer;

function MakeAbsoluteSD;
begin
  GetProcedureAddress(_MakeAbsoluteSD, advapi32, 'MakeAbsoluteSD');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MakeAbsoluteSD]
  end;
end;
{$ELSE}
function MakeAbsoluteSD; external advapi32 name 'MakeAbsoluteSD';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MakeAbsoluteSD2: Pointer;

function MakeAbsoluteSD2;
begin
  GetProcedureAddress(_MakeAbsoluteSD2, advapi32, 'MakeAbsoluteSD2');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MakeAbsoluteSD2]
  end;
end;
{$ELSE}
function MakeAbsoluteSD2; external advapi32 name 'MakeAbsoluteSD2';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileSecurityA: Pointer;

function SetFileSecurityA;
begin
  GetProcedureAddress(_SetFileSecurityA, advapi32, 'SetFileSecurityA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileSecurityA]
  end;
end;
{$ELSE}
function SetFileSecurityA; external advapi32 name 'SetFileSecurityA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileSecurityW: Pointer;

function SetFileSecurityW;
begin
  GetProcedureAddress(_SetFileSecurityW, advapi32, 'SetFileSecurityW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileSecurityW]
  end;
end;
{$ELSE}
function SetFileSecurityW; external advapi32 name 'SetFileSecurityW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileSecurity: Pointer;

function SetFileSecurity;
begin
  GetProcedureAddress(_SetFileSecurity, advapi32, 'SetFileSecurityW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileSecurity]
  end;
end;
{$ELSE}
function SetFileSecurity; external advapi32 name 'SetFileSecurityW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetFileSecurity: Pointer;

function SetFileSecurity;
begin
  GetProcedureAddress(_SetFileSecurity, advapi32, 'SetFileSecurityA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFileSecurity]
  end;
end;
{$ELSE}
function SetFileSecurity; external advapi32 name 'SetFileSecurityA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileSecurityA: Pointer;

function GetFileSecurityA;
begin
  GetProcedureAddress(_GetFileSecurityA, advapi32, 'GetFileSecurityA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileSecurityA]
  end;
end;
{$ELSE}
function GetFileSecurityA; external advapi32 name 'GetFileSecurityA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileSecurityW: Pointer;

function GetFileSecurityW;
begin
  GetProcedureAddress(_GetFileSecurityW, advapi32, 'GetFileSecurityW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileSecurityW]
  end;
end;
{$ELSE}
function GetFileSecurityW; external advapi32 name 'GetFileSecurityW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileSecurity: Pointer;

function GetFileSecurity;
begin
  GetProcedureAddress(_GetFileSecurity, advapi32, 'GetFileSecurityW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileSecurity]
  end;
end;
{$ELSE}
function GetFileSecurity; external advapi32 name 'GetFileSecurityW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileSecurity: Pointer;

function GetFileSecurity;
begin
  GetProcedureAddress(_GetFileSecurity, advapi32, 'GetFileSecurityA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileSecurity]
  end;
end;
{$ELSE}
function GetFileSecurity; external advapi32 name 'GetFileSecurityA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetKernelObjectSecurity: Pointer;

function SetKernelObjectSecurity;
begin
  GetProcedureAddress(_SetKernelObjectSecurity, advapi32, 'SetKernelObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetKernelObjectSecurity]
  end;
end;
{$ELSE}
function SetKernelObjectSecurity; external advapi32 name 'SetKernelObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstChangeNotificationA: Pointer;

function FindFirstChangeNotificationA;
begin
  GetProcedureAddress(_FindFirstChangeNotificationA, kernel32, 'FindFirstChangeNotificationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstChangeNotificationA]
  end;
end;
{$ELSE}
function FindFirstChangeNotificationA; external kernel32 name 'FindFirstChangeNotificationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstChangeNotificationW: Pointer;

function FindFirstChangeNotificationW;
begin
  GetProcedureAddress(_FindFirstChangeNotificationW, kernel32, 'FindFirstChangeNotificationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstChangeNotificationW]
  end;
end;
{$ELSE}
function FindFirstChangeNotificationW; external kernel32 name 'FindFirstChangeNotificationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstChangeNotification: Pointer;

function FindFirstChangeNotification;
begin
  GetProcedureAddress(_FindFirstChangeNotification, kernel32, 'FindFirstChangeNotificationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstChangeNotification]
  end;
end;
{$ELSE}
function FindFirstChangeNotification; external kernel32 name 'FindFirstChangeNotificationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstChangeNotification: Pointer;

function FindFirstChangeNotification;
begin
  GetProcedureAddress(_FindFirstChangeNotification, kernel32, 'FindFirstChangeNotificationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstChangeNotification]
  end;
end;
{$ELSE}
function FindFirstChangeNotification; external kernel32 name 'FindFirstChangeNotificationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextChangeNotification: Pointer;

function FindNextChangeNotification;
begin
  GetProcedureAddress(_FindNextChangeNotification, kernel32, 'FindNextChangeNotification');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextChangeNotification]
  end;
end;
{$ELSE}
function FindNextChangeNotification; external kernel32 name 'FindNextChangeNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindCloseChangeNotification: Pointer;

function FindCloseChangeNotification;
begin
  GetProcedureAddress(_FindCloseChangeNotification, kernel32, 'FindCloseChangeNotification');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindCloseChangeNotification]
  end;
end;
{$ELSE}
function FindCloseChangeNotification; external kernel32 name 'FindCloseChangeNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadDirectoryChangesW: Pointer;

function ReadDirectoryChangesW;
begin
  GetProcedureAddress(_ReadDirectoryChangesW, kernel32, 'ReadDirectoryChangesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadDirectoryChangesW]
  end;
end;
{$ELSE}
function ReadDirectoryChangesW; external kernel32 name 'ReadDirectoryChangesW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualLock: Pointer;

function VirtualLock;
begin
  GetProcedureAddress(_VirtualLock, kernel32, 'VirtualLock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualLock]
  end;
end;
{$ELSE}
function VirtualLock; external kernel32 name 'VirtualLock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VirtualUnlock: Pointer;

function VirtualUnlock;
begin
  GetProcedureAddress(_VirtualUnlock, kernel32, 'VirtualUnlock');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VirtualUnlock]
  end;
end;
{$ELSE}
function VirtualUnlock; external kernel32 name 'VirtualUnlock';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapViewOfFileEx: Pointer;

function MapViewOfFileEx;
begin
  GetProcedureAddress(_MapViewOfFileEx, kernel32, 'MapViewOfFileEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapViewOfFileEx]
  end;
end;
{$ELSE}
function MapViewOfFileEx; external kernel32 name 'MapViewOfFileEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPriorityClass: Pointer;

function SetPriorityClass;
begin
  GetProcedureAddress(_SetPriorityClass, kernel32, 'SetPriorityClass');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPriorityClass]
  end;
end;
{$ELSE}
function SetPriorityClass; external kernel32 name 'SetPriorityClass';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPriorityClass: Pointer;

function GetPriorityClass;
begin
  GetProcedureAddress(_GetPriorityClass, kernel32, 'GetPriorityClass');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPriorityClass]
  end;
end;
{$ELSE}
function GetPriorityClass; external kernel32 name 'GetPriorityClass';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadReadPtr: Pointer;

function IsBadReadPtr;
begin
  GetProcedureAddress(_IsBadReadPtr, kernel32, 'IsBadReadPtr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadReadPtr]
  end;
end;
{$ELSE}
function IsBadReadPtr; external kernel32 name 'IsBadReadPtr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadWritePtr: Pointer;

function IsBadWritePtr;
begin
  GetProcedureAddress(_IsBadWritePtr, kernel32, 'IsBadWritePtr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadWritePtr]
  end;
end;
{$ELSE}
function IsBadWritePtr; external kernel32 name 'IsBadWritePtr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadHugeReadPtr: Pointer;

function IsBadHugeReadPtr;
begin
  GetProcedureAddress(_IsBadHugeReadPtr, kernel32, 'IsBadHugeReadPtr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadHugeReadPtr]
  end;
end;
{$ELSE}
function IsBadHugeReadPtr; external kernel32 name 'IsBadHugeReadPtr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadHugeWritePtr: Pointer;

function IsBadHugeWritePtr;
begin
  GetProcedureAddress(_IsBadHugeWritePtr, kernel32, 'IsBadHugeWritePtr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadHugeWritePtr]
  end;
end;
{$ELSE}
function IsBadHugeWritePtr; external kernel32 name 'IsBadHugeWritePtr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadCodePtr: Pointer;

function IsBadCodePtr;
begin
  GetProcedureAddress(_IsBadCodePtr, kernel32, 'IsBadCodePtr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadCodePtr]
  end;
end;
{$ELSE}
function IsBadCodePtr; external kernel32 name 'IsBadCodePtr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadStringPtrA: Pointer;

function IsBadStringPtrA;
begin
  GetProcedureAddress(_IsBadStringPtrA, kernel32, 'IsBadStringPtrA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadStringPtrA]
  end;
end;
{$ELSE}
function IsBadStringPtrA; external kernel32 name 'IsBadStringPtrA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadStringPtrW: Pointer;

function IsBadStringPtrW;
begin
  GetProcedureAddress(_IsBadStringPtrW, kernel32, 'IsBadStringPtrW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadStringPtrW]
  end;
end;
{$ELSE}
function IsBadStringPtrW; external kernel32 name 'IsBadStringPtrW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadStringPtr: Pointer;

function IsBadStringPtr;
begin
  GetProcedureAddress(_IsBadStringPtr, kernel32, 'IsBadStringPtrW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadStringPtr]
  end;
end;
{$ELSE}
function IsBadStringPtr; external kernel32 name 'IsBadStringPtrW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _IsBadStringPtr: Pointer;

function IsBadStringPtr;
begin
  GetProcedureAddress(_IsBadStringPtr, kernel32, 'IsBadStringPtrA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsBadStringPtr]
  end;
end;
{$ELSE}
function IsBadStringPtr; external kernel32 name 'IsBadStringPtrA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountSidA: Pointer;

function LookupAccountSidA;
begin
  GetProcedureAddress(_LookupAccountSidA, advapi32, 'LookupAccountSidA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountSidA]
  end;
end;
{$ELSE}
function LookupAccountSidA; external advapi32 name 'LookupAccountSidA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountSidW: Pointer;

function LookupAccountSidW;
begin
  GetProcedureAddress(_LookupAccountSidW, advapi32, 'LookupAccountSidW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountSidW]
  end;
end;
{$ELSE}
function LookupAccountSidW; external advapi32 name 'LookupAccountSidW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountSid: Pointer;

function LookupAccountSid;
begin
  GetProcedureAddress(_LookupAccountSid, advapi32, 'LookupAccountSidW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountSid]
  end;
end;
{$ELSE}
function LookupAccountSid; external advapi32 name 'LookupAccountSidW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountSid: Pointer;

function LookupAccountSid;
begin
  GetProcedureAddress(_LookupAccountSid, advapi32, 'LookupAccountSidA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountSid]
  end;
end;
{$ELSE}
function LookupAccountSid; external advapi32 name 'LookupAccountSidA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountNameA: Pointer;

function LookupAccountNameA;
begin
  GetProcedureAddress(_LookupAccountNameA, advapi32, 'LookupAccountNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountNameA]
  end;
end;
{$ELSE}
function LookupAccountNameA; external advapi32 name 'LookupAccountNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountNameW: Pointer;

function LookupAccountNameW;
begin
  GetProcedureAddress(_LookupAccountNameW, advapi32, 'LookupAccountNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountNameW]
  end;
end;
{$ELSE}
function LookupAccountNameW; external advapi32 name 'LookupAccountNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountName: Pointer;

function LookupAccountName;
begin
  GetProcedureAddress(_LookupAccountName, advapi32, 'LookupAccountNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountName]
  end;
end;
{$ELSE}
function LookupAccountName; external advapi32 name 'LookupAccountNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupAccountName: Pointer;

function LookupAccountName;
begin
  GetProcedureAddress(_LookupAccountName, advapi32, 'LookupAccountNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupAccountName]
  end;
end;
{$ELSE}
function LookupAccountName; external advapi32 name 'LookupAccountNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeValueA: Pointer;

function LookupPrivilegeValueA;
begin
  GetProcedureAddress(_LookupPrivilegeValueA, advapi32, 'LookupPrivilegeValueA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeValueA]
  end;
end;
{$ELSE}
function LookupPrivilegeValueA; external advapi32 name 'LookupPrivilegeValueA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeValueW: Pointer;

function LookupPrivilegeValueW;
begin
  GetProcedureAddress(_LookupPrivilegeValueW, advapi32, 'LookupPrivilegeValueW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeValueW]
  end;
end;
{$ELSE}
function LookupPrivilegeValueW; external advapi32 name 'LookupPrivilegeValueW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeValue: Pointer;

function LookupPrivilegeValue;
begin
  GetProcedureAddress(_LookupPrivilegeValue, advapi32, 'LookupPrivilegeValueW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeValue]
  end;
end;
{$ELSE}
function LookupPrivilegeValue; external advapi32 name 'LookupPrivilegeValueW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeValue: Pointer;

function LookupPrivilegeValue;
begin
  GetProcedureAddress(_LookupPrivilegeValue, advapi32, 'LookupPrivilegeValueA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeValue]
  end;
end;
{$ELSE}
function LookupPrivilegeValue; external advapi32 name 'LookupPrivilegeValueA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeNameA: Pointer;

function LookupPrivilegeNameA;
begin
  GetProcedureAddress(_LookupPrivilegeNameA, advapi32, 'LookupPrivilegeNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeNameA]
  end;
end;
{$ELSE}
function LookupPrivilegeNameA; external advapi32 name 'LookupPrivilegeNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeNameW: Pointer;

function LookupPrivilegeNameW;
begin
  GetProcedureAddress(_LookupPrivilegeNameW, advapi32, 'LookupPrivilegeNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeNameW]
  end;
end;
{$ELSE}
function LookupPrivilegeNameW; external advapi32 name 'LookupPrivilegeNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeName: Pointer;

function LookupPrivilegeName;
begin
  GetProcedureAddress(_LookupPrivilegeName, advapi32, 'LookupPrivilegeNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeName]
  end;
end;
{$ELSE}
function LookupPrivilegeName; external advapi32 name 'LookupPrivilegeNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeName: Pointer;

function LookupPrivilegeName;
begin
  GetProcedureAddress(_LookupPrivilegeName, advapi32, 'LookupPrivilegeNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeName]
  end;
end;
{$ELSE}
function LookupPrivilegeName; external advapi32 name 'LookupPrivilegeNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeDisplayNameA: Pointer;

function LookupPrivilegeDisplayNameA;
begin
  GetProcedureAddress(_LookupPrivilegeDisplayNameA, advapi32, 'LookupPrivilegeDisplayNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeDisplayNameA]
  end;
end;
{$ELSE}
function LookupPrivilegeDisplayNameA; external advapi32 name 'LookupPrivilegeDisplayNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeDisplayNameW: Pointer;

function LookupPrivilegeDisplayNameW;
begin
  GetProcedureAddress(_LookupPrivilegeDisplayNameW, advapi32, 'LookupPrivilegeDisplayNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeDisplayNameW]
  end;
end;
{$ELSE}
function LookupPrivilegeDisplayNameW; external advapi32 name 'LookupPrivilegeDisplayNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeDisplayName: Pointer;

function LookupPrivilegeDisplayName;
begin
  GetProcedureAddress(_LookupPrivilegeDisplayName, advapi32, 'LookupPrivilegeDisplayNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeDisplayName]
  end;
end;
{$ELSE}
function LookupPrivilegeDisplayName; external advapi32 name 'LookupPrivilegeDisplayNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LookupPrivilegeDisplayName: Pointer;

function LookupPrivilegeDisplayName;
begin
  GetProcedureAddress(_LookupPrivilegeDisplayName, advapi32, 'LookupPrivilegeDisplayNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupPrivilegeDisplayName]
  end;
end;
{$ELSE}
function LookupPrivilegeDisplayName; external advapi32 name 'LookupPrivilegeDisplayNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AllocateLocallyUniqueId: Pointer;

function AllocateLocallyUniqueId;
begin
  GetProcedureAddress(_AllocateLocallyUniqueId, advapi32, 'AllocateLocallyUniqueId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllocateLocallyUniqueId]
  end;
end;
{$ELSE}
function AllocateLocallyUniqueId; external advapi32 name 'AllocateLocallyUniqueId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCBA: Pointer;

function BuildCommDCBA;
begin
  GetProcedureAddress(_BuildCommDCBA, kernel32, 'BuildCommDCBA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCBA]
  end;
end;
{$ELSE}
function BuildCommDCBA; external kernel32 name 'BuildCommDCBA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCBW: Pointer;

function BuildCommDCBW;
begin
  GetProcedureAddress(_BuildCommDCBW, kernel32, 'BuildCommDCBW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCBW]
  end;
end;
{$ELSE}
function BuildCommDCBW; external kernel32 name 'BuildCommDCBW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCB: Pointer;

function BuildCommDCB;
begin
  GetProcedureAddress(_BuildCommDCB, kernel32, 'BuildCommDCBW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCB]
  end;
end;
{$ELSE}
function BuildCommDCB; external kernel32 name 'BuildCommDCBW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCB: Pointer;

function BuildCommDCB;
begin
  GetProcedureAddress(_BuildCommDCB, kernel32, 'BuildCommDCBA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCB]
  end;
end;
{$ELSE}
function BuildCommDCB; external kernel32 name 'BuildCommDCBA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCBAndTimeoutsA: Pointer;

function BuildCommDCBAndTimeoutsA;
begin
  GetProcedureAddress(_BuildCommDCBAndTimeoutsA, kernel32, 'BuildCommDCBAndTimeoutsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCBAndTimeoutsA]
  end;
end;
{$ELSE}
function BuildCommDCBAndTimeoutsA; external kernel32 name 'BuildCommDCBAndTimeoutsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCBAndTimeoutsW: Pointer;

function BuildCommDCBAndTimeoutsW;
begin
  GetProcedureAddress(_BuildCommDCBAndTimeoutsW, kernel32, 'BuildCommDCBAndTimeoutsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCBAndTimeoutsW]
  end;
end;
{$ELSE}
function BuildCommDCBAndTimeoutsW; external kernel32 name 'BuildCommDCBAndTimeoutsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCBAndTimeouts: Pointer;

function BuildCommDCBAndTimeouts;
begin
  GetProcedureAddress(_BuildCommDCBAndTimeouts, kernel32, 'BuildCommDCBAndTimeoutsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCBAndTimeouts]
  end;
end;
{$ELSE}
function BuildCommDCBAndTimeouts; external kernel32 name 'BuildCommDCBAndTimeoutsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _BuildCommDCBAndTimeouts: Pointer;

function BuildCommDCBAndTimeouts;
begin
  GetProcedureAddress(_BuildCommDCBAndTimeouts, kernel32, 'BuildCommDCBAndTimeoutsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BuildCommDCBAndTimeouts]
  end;
end;
{$ELSE}
function BuildCommDCBAndTimeouts; external kernel32 name 'BuildCommDCBAndTimeoutsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CommConfigDialogA: Pointer;

function CommConfigDialogA;
begin
  GetProcedureAddress(_CommConfigDialogA, kernel32, 'CommConfigDialogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CommConfigDialogA]
  end;
end;
{$ELSE}
function CommConfigDialogA; external kernel32 name 'CommConfigDialogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CommConfigDialogW: Pointer;

function CommConfigDialogW;
begin
  GetProcedureAddress(_CommConfigDialogW, kernel32, 'CommConfigDialogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CommConfigDialogW]
  end;
end;
{$ELSE}
function CommConfigDialogW; external kernel32 name 'CommConfigDialogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CommConfigDialog: Pointer;

function CommConfigDialog;
begin
  GetProcedureAddress(_CommConfigDialog, kernel32, 'CommConfigDialogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CommConfigDialog]
  end;
end;
{$ELSE}
function CommConfigDialog; external kernel32 name 'CommConfigDialogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CommConfigDialog: Pointer;

function CommConfigDialog;
begin
  GetProcedureAddress(_CommConfigDialog, kernel32, 'CommConfigDialogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CommConfigDialog]
  end;
end;
{$ELSE}
function CommConfigDialog; external kernel32 name 'CommConfigDialogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDefaultCommConfigA: Pointer;

function GetDefaultCommConfigA;
begin
  GetProcedureAddress(_GetDefaultCommConfigA, kernel32, 'GetDefaultCommConfigA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDefaultCommConfigA]
  end;
end;
{$ELSE}
function GetDefaultCommConfigA; external kernel32 name 'GetDefaultCommConfigA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDefaultCommConfigW: Pointer;

function GetDefaultCommConfigW;
begin
  GetProcedureAddress(_GetDefaultCommConfigW, kernel32, 'GetDefaultCommConfigW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDefaultCommConfigW]
  end;
end;
{$ELSE}
function GetDefaultCommConfigW; external kernel32 name 'GetDefaultCommConfigW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDefaultCommConfig: Pointer;

function GetDefaultCommConfig;
begin
  GetProcedureAddress(_GetDefaultCommConfig, kernel32, 'GetDefaultCommConfigW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDefaultCommConfig]
  end;
end;
{$ELSE}
function GetDefaultCommConfig; external kernel32 name 'GetDefaultCommConfigW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDefaultCommConfig: Pointer;

function GetDefaultCommConfig;
begin
  GetProcedureAddress(_GetDefaultCommConfig, kernel32, 'GetDefaultCommConfigA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDefaultCommConfig]
  end;
end;
{$ELSE}
function GetDefaultCommConfig; external kernel32 name 'GetDefaultCommConfigA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetDefaultCommConfigA: Pointer;

function SetDefaultCommConfigA;
begin
  GetProcedureAddress(_SetDefaultCommConfigA, kernel32, 'SetDefaultCommConfigA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDefaultCommConfigA]
  end;
end;
{$ELSE}
function SetDefaultCommConfigA; external kernel32 name 'SetDefaultCommConfigA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDefaultCommConfigW: Pointer;

function SetDefaultCommConfigW;
begin
  GetProcedureAddress(_SetDefaultCommConfigW, kernel32, 'SetDefaultCommConfigW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDefaultCommConfigW]
  end;
end;
{$ELSE}
function SetDefaultCommConfigW; external kernel32 name 'SetDefaultCommConfigW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetDefaultCommConfig: Pointer;

function SetDefaultCommConfig;
begin
  GetProcedureAddress(_SetDefaultCommConfig, kernel32, 'SetDefaultCommConfigW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDefaultCommConfig]
  end;
end;
{$ELSE}
function SetDefaultCommConfig; external kernel32 name 'SetDefaultCommConfigW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetDefaultCommConfig: Pointer;

function SetDefaultCommConfig;
begin
  GetProcedureAddress(_SetDefaultCommConfig, kernel32, 'SetDefaultCommConfigA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDefaultCommConfig]
  end;
end;
{$ELSE}
function SetDefaultCommConfig; external kernel32 name 'SetDefaultCommConfigA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerNameA: Pointer;

function GetComputerNameA;
begin
  GetProcedureAddress(_GetComputerNameA, kernel32, 'GetComputerNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerNameA]
  end;
end;
{$ELSE}
function GetComputerNameA; external kernel32 name 'GetComputerNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerNameW: Pointer;

function GetComputerNameW;
begin
  GetProcedureAddress(_GetComputerNameW, kernel32, 'GetComputerNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerNameW]
  end;
end;
{$ELSE}
function GetComputerNameW; external kernel32 name 'GetComputerNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerName: Pointer;

function GetComputerName;
begin
  GetProcedureAddress(_GetComputerName, kernel32, 'GetComputerNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerName]
  end;
end;
{$ELSE}
function GetComputerName; external kernel32 name 'GetComputerNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerName: Pointer;

function GetComputerName;
begin
  GetProcedureAddress(_GetComputerName, kernel32, 'GetComputerNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerName]
  end;
end;
{$ELSE}
function GetComputerName; external kernel32 name 'GetComputerNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerNameA: Pointer;

function SetComputerNameA;
begin
  GetProcedureAddress(_SetComputerNameA, kernel32, 'SetComputerNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerNameA]
  end;
end;
{$ELSE}
function SetComputerNameA; external kernel32 name 'SetComputerNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerNameW: Pointer;

function SetComputerNameW;
begin
  GetProcedureAddress(_SetComputerNameW, kernel32, 'SetComputerNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerNameW]
  end;
end;
{$ELSE}
function SetComputerNameW; external kernel32 name 'SetComputerNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerName: Pointer;

function SetComputerName;
begin
  GetProcedureAddress(_SetComputerName, kernel32, 'SetComputerNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerName]
  end;
end;
{$ELSE}
function SetComputerName; external kernel32 name 'SetComputerNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerName: Pointer;

function SetComputerName;
begin
  GetProcedureAddress(_SetComputerName, kernel32, 'SetComputerNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerName]
  end;
end;
{$ELSE}
function SetComputerName; external kernel32 name 'SetComputerNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerNameExA: Pointer;

function GetComputerNameExA;
begin
  GetProcedureAddress(_GetComputerNameExA, kernel32, 'GetComputerNameExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerNameExA]
  end;
end;
{$ELSE}
function GetComputerNameExA; external kernel32 name 'GetComputerNameExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerNameExW: Pointer;

function GetComputerNameExW;
begin
  GetProcedureAddress(_GetComputerNameExW, kernel32, 'GetComputerNameExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerNameExW]
  end;
end;
{$ELSE}
function GetComputerNameExW; external kernel32 name 'GetComputerNameExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerNameEx: Pointer;

function GetComputerNameEx;
begin
  GetProcedureAddress(_GetComputerNameEx, kernel32, 'GetComputerNameExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerNameEx]
  end;
end;
{$ELSE}
function GetComputerNameEx; external kernel32 name 'GetComputerNameExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetComputerNameEx: Pointer;

function GetComputerNameEx;
begin
  GetProcedureAddress(_GetComputerNameEx, kernel32, 'GetComputerNameExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComputerNameEx]
  end;
end;
{$ELSE}
function GetComputerNameEx; external kernel32 name 'GetComputerNameExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerNameExA: Pointer;

function SetComputerNameExA;
begin
  GetProcedureAddress(_SetComputerNameExA, kernel32, 'SetComputerNameExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerNameExA]
  end;
end;
{$ELSE}
function SetComputerNameExA; external kernel32 name 'SetComputerNameExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerNameExW: Pointer;

function SetComputerNameExW;
begin
  GetProcedureAddress(_SetComputerNameExW, kernel32, 'SetComputerNameExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerNameExW]
  end;
end;
{$ELSE}
function SetComputerNameExW; external kernel32 name 'SetComputerNameExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerNameEx: Pointer;

function SetComputerNameEx;
begin
  GetProcedureAddress(_SetComputerNameEx, kernel32, 'SetComputerNameExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerNameEx]
  end;
end;
{$ELSE}
function SetComputerNameEx; external kernel32 name 'SetComputerNameExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetComputerNameEx: Pointer;

function SetComputerNameEx;
begin
  GetProcedureAddress(_SetComputerNameEx, kernel32, 'SetComputerNameExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetComputerNameEx]
  end;
end;
{$ELSE}
function SetComputerNameEx; external kernel32 name 'SetComputerNameExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _DnsHostnameToComputerNameA: Pointer;

function DnsHostnameToComputerNameA;
begin
  GetProcedureAddress(_DnsHostnameToComputerNameA, kernel32, 'DnsHostnameToComputerNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DnsHostnameToComputerNameA]
  end;
end;
{$ELSE}
function DnsHostnameToComputerNameA; external kernel32 name 'DnsHostnameToComputerNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DnsHostnameToComputerNameW: Pointer;

function DnsHostnameToComputerNameW;
begin
  GetProcedureAddress(_DnsHostnameToComputerNameW, kernel32, 'DnsHostnameToComputerNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DnsHostnameToComputerNameW]
  end;
end;
{$ELSE}
function DnsHostnameToComputerNameW; external kernel32 name 'DnsHostnameToComputerNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DnsHostnameToComputerName: Pointer;

function DnsHostnameToComputerName;
begin
  GetProcedureAddress(_DnsHostnameToComputerName, kernel32, 'DnsHostnameToComputerNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DnsHostnameToComputerName]
  end;
end;
{$ELSE}
function DnsHostnameToComputerName; external kernel32 name 'DnsHostnameToComputerNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DnsHostnameToComputerName: Pointer;

function DnsHostnameToComputerName;
begin
  GetProcedureAddress(_DnsHostnameToComputerName, kernel32, 'DnsHostnameToComputerNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DnsHostnameToComputerName]
  end;
end;
{$ELSE}
function DnsHostnameToComputerName; external kernel32 name 'DnsHostnameToComputerNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserNameA: Pointer;

function GetUserNameA;
begin
  GetProcedureAddress(_GetUserNameA, advapi32, 'GetUserNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserNameA]
  end;
end;
{$ELSE}
function GetUserNameA; external advapi32 name 'GetUserNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserNameW: Pointer;

function GetUserNameW;
begin
  GetProcedureAddress(_GetUserNameW, advapi32, 'GetUserNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserNameW]
  end;
end;
{$ELSE}
function GetUserNameW; external advapi32 name 'GetUserNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserName: Pointer;

function GetUserName;
begin
  GetProcedureAddress(_GetUserName, advapi32, 'GetUserNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserName]
  end;
end;
{$ELSE}
function GetUserName; external advapi32 name 'GetUserNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserName: Pointer;

function GetUserName;
begin
  GetProcedureAddress(_GetUserName, advapi32, 'GetUserNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserName]
  end;
end;
{$ELSE}
function GetUserName; external advapi32 name 'GetUserNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUserA: Pointer;

function LogonUserA;
begin
  GetProcedureAddress(_LogonUserA, advapi32, 'LogonUserA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUserA]
  end;
end;
{$ELSE}
function LogonUserA; external advapi32 name 'LogonUserA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUserW: Pointer;

function LogonUserW;
begin
  GetProcedureAddress(_LogonUserW, advapi32, 'LogonUserW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUserW]
  end;
end;
{$ELSE}
function LogonUserW; external advapi32 name 'LogonUserW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUser: Pointer;

function LogonUser;
begin
  GetProcedureAddress(_LogonUser, advapi32, 'LogonUserW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUser]
  end;
end;
{$ELSE}
function LogonUser; external advapi32 name 'LogonUserW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUser: Pointer;

function LogonUser;
begin
  GetProcedureAddress(_LogonUser, advapi32, 'LogonUserA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUser]
  end;
end;
{$ELSE}
function LogonUser; external advapi32 name 'LogonUserA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUserExA: Pointer;

function LogonUserExA;
begin
  GetProcedureAddress(_LogonUserExA, advapi32, 'LogonUserExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUserExA]
  end;
end;
{$ELSE}
function LogonUserExA; external advapi32 name 'LogonUserExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUserExW: Pointer;

function LogonUserExW;
begin
  GetProcedureAddress(_LogonUserExW, advapi32, 'LogonUserExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUserExW]
  end;
end;
{$ELSE}
function LogonUserExW; external advapi32 name 'LogonUserExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUserEx: Pointer;

function LogonUserEx;
begin
  GetProcedureAddress(_LogonUserEx, advapi32, 'LogonUserExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUserEx]
  end;
end;
{$ELSE}
function LogonUserEx; external advapi32 name 'LogonUserExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LogonUserEx: Pointer;

function LogonUserEx;
begin
  GetProcedureAddress(_LogonUserEx, advapi32, 'LogonUserExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LogonUserEx]
  end;
end;
{$ELSE}
function LogonUserEx; external advapi32 name 'LogonUserExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ImpersonateLoggedOnUser: Pointer;

function ImpersonateLoggedOnUser;
begin
  GetProcedureAddress(_ImpersonateLoggedOnUser, advapi32, 'ImpersonateLoggedOnUser');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImpersonateLoggedOnUser]
  end;
end;
{$ELSE}
function ImpersonateLoggedOnUser; external advapi32 name 'ImpersonateLoggedOnUser';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessAsUserA: Pointer;

function CreateProcessAsUserA;
begin
  GetProcedureAddress(_CreateProcessAsUserA, advapi32, 'CreateProcessAsUserA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessAsUserA]
  end;
end;
{$ELSE}
function CreateProcessAsUserA; external advapi32 name 'CreateProcessAsUserA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessAsUserW: Pointer;

function CreateProcessAsUserW;
begin
  GetProcedureAddress(_CreateProcessAsUserW, advapi32, 'CreateProcessAsUserW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessAsUserW]
  end;
end;
{$ELSE}
function CreateProcessAsUserW; external advapi32 name 'CreateProcessAsUserW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessAsUser: Pointer;

function CreateProcessAsUser;
begin
  GetProcedureAddress(_CreateProcessAsUser, advapi32, 'CreateProcessAsUserW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessAsUser]
  end;
end;
{$ELSE}
function CreateProcessAsUser; external advapi32 name 'CreateProcessAsUserW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessAsUser: Pointer;

function CreateProcessAsUser;
begin
  GetProcedureAddress(_CreateProcessAsUser, advapi32, 'CreateProcessAsUserA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessAsUser]
  end;
end;
{$ELSE}
function CreateProcessAsUser; external advapi32 name 'CreateProcessAsUserA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessWithLogonW: Pointer;

function CreateProcessWithLogonW;
begin
  GetProcedureAddress(_CreateProcessWithLogonW, advapi32, 'CreateProcessWithLogonW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessWithLogonW]
  end;
end;
{$ELSE}
function CreateProcessWithLogonW; external advapi32 name 'CreateProcessWithLogonW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateProcessWithTokenW: Pointer;

function CreateProcessWithTokenW;
begin
  GetProcedureAddress(_CreateProcessWithTokenW, advapi32, 'CreateProcessWithTokenW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateProcessWithTokenW]
  end;
end;
{$ELSE}
function CreateProcessWithTokenW; external advapi32 name 'CreateProcessWithTokenW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ImpersonateAnonymousToken: Pointer;

function ImpersonateAnonymousToken;
begin
  GetProcedureAddress(_ImpersonateAnonymousToken, advapi32, 'ImpersonateAnonymousToken');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ImpersonateAnonymousToken]
  end;
end;
{$ELSE}
function ImpersonateAnonymousToken; external advapi32 name 'ImpersonateAnonymousToken';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DuplicateTokenEx: Pointer;

function DuplicateTokenEx;
begin
  GetProcedureAddress(_DuplicateTokenEx, advapi32, 'DuplicateTokenEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DuplicateTokenEx]
  end;
end;
{$ELSE}
function DuplicateTokenEx; external advapi32 name 'DuplicateTokenEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateRestrictedToken: Pointer;

function CreateRestrictedToken;
begin
  GetProcedureAddress(_CreateRestrictedToken, advapi32, 'CreateRestrictedToken');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateRestrictedToken]
  end;
end;
{$ELSE}
function CreateRestrictedToken; external advapi32 name 'CreateRestrictedToken';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsTokenRestricted: Pointer;

function IsTokenRestricted;
begin
  GetProcedureAddress(_IsTokenRestricted, advapi32, 'IsTokenRestricted');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsTokenRestricted]
  end;
end;
{$ELSE}
function IsTokenRestricted; external advapi32 name 'IsTokenRestricted';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CheckTokenMembership: Pointer;

function CheckTokenMembership;
begin
  GetProcedureAddress(_CheckTokenMembership, advapi32, 'CheckTokenMembership');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckTokenMembership]
  end;
end;
{$ELSE}
function CheckTokenMembership; external advapi32 name 'CheckTokenMembership';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsTokenUntrusted: Pointer;

function IsTokenUntrusted;
begin
  GetProcedureAddress(_IsTokenUntrusted, advapi32, 'IsTokenUntrusted');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsTokenUntrusted]
  end;
end;
{$ELSE}
function IsTokenUntrusted; external advapi32 name 'IsTokenUntrusted';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterWaitForSingleObject: Pointer;

function RegisterWaitForSingleObject;
begin
  GetProcedureAddress(_RegisterWaitForSingleObject, kernel32, 'RegisterWaitForSingleObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterWaitForSingleObject]
  end;
end;
{$ELSE}
function RegisterWaitForSingleObject; external kernel32 name 'RegisterWaitForSingleObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterWaitForSingleObjectEx: Pointer;

function RegisterWaitForSingleObjectEx;
begin
  GetProcedureAddress(_RegisterWaitForSingleObjectEx, kernel32, 'RegisterWaitForSingleObjectEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterWaitForSingleObjectEx]
  end;
end;
{$ELSE}
function RegisterWaitForSingleObjectEx; external kernel32 name 'RegisterWaitForSingleObjectEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterWait: Pointer;

function UnregisterWait;
begin
  GetProcedureAddress(_UnregisterWait, kernel32, 'UnregisterWait');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterWait]
  end;
end;
{$ELSE}
function UnregisterWait; external kernel32 name 'UnregisterWait';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterWaitEx: Pointer;

function UnregisterWaitEx;
begin
  GetProcedureAddress(_UnregisterWaitEx, kernel32, 'UnregisterWaitEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterWaitEx]
  end;
end;
{$ELSE}
function UnregisterWaitEx; external kernel32 name 'UnregisterWaitEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueueUserWorkItem: Pointer;

function QueueUserWorkItem;
begin
  GetProcedureAddress(_QueueUserWorkItem, kernel32, 'QueueUserWorkItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueueUserWorkItem]
  end;
end;
{$ELSE}
function QueueUserWorkItem; external kernel32 name 'QueueUserWorkItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BindIoCompletionCallback: Pointer;

function BindIoCompletionCallback;
begin
  GetProcedureAddress(_BindIoCompletionCallback, kernel32, 'BindIoCompletionCallback');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BindIoCompletionCallback]
  end;
end;
{$ELSE}
function BindIoCompletionCallback; external kernel32 name 'BindIoCompletionCallback';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateTimerQueue: Pointer;

function CreateTimerQueue;
begin
  GetProcedureAddress(_CreateTimerQueue, kernel32, 'CreateTimerQueue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateTimerQueue]
  end;
end;
{$ELSE}
function CreateTimerQueue; external kernel32 name 'CreateTimerQueue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateTimerQueueTimer: Pointer;

function CreateTimerQueueTimer;
begin
  GetProcedureAddress(_CreateTimerQueueTimer, kernel32, 'CreateTimerQueueTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateTimerQueueTimer]
  end;
end;
{$ELSE}
function CreateTimerQueueTimer; external kernel32 name 'CreateTimerQueueTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeTimerQueueTimer: Pointer;

function ChangeTimerQueueTimer;
begin
  GetProcedureAddress(_ChangeTimerQueueTimer, kernel32, 'ChangeTimerQueueTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeTimerQueueTimer]
  end;
end;
{$ELSE}
function ChangeTimerQueueTimer; external kernel32 name 'ChangeTimerQueueTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteTimerQueueTimer: Pointer;

function DeleteTimerQueueTimer;
begin
  GetProcedureAddress(_DeleteTimerQueueTimer, kernel32, 'DeleteTimerQueueTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteTimerQueueTimer]
  end;
end;
{$ELSE}
function DeleteTimerQueueTimer; external kernel32 name 'DeleteTimerQueueTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteTimerQueueEx: Pointer;

function DeleteTimerQueueEx;
begin
  GetProcedureAddress(_DeleteTimerQueueEx, kernel32, 'DeleteTimerQueueEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteTimerQueueEx]
  end;
end;
{$ELSE}
function DeleteTimerQueueEx; external kernel32 name 'DeleteTimerQueueEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTimerQueueTimer: Pointer;

function SetTimerQueueTimer;
begin
  GetProcedureAddress(_SetTimerQueueTimer, kernel32, 'SetTimerQueueTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTimerQueueTimer]
  end;
end;
{$ELSE}
function SetTimerQueueTimer; external kernel32 name 'SetTimerQueueTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CancelTimerQueueTimer: Pointer;

function CancelTimerQueueTimer;
begin
  GetProcedureAddress(_CancelTimerQueueTimer, kernel32, 'CancelTimerQueueTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CancelTimerQueueTimer]
  end;
end;
{$ELSE}
function CancelTimerQueueTimer; external kernel32 name 'CancelTimerQueueTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteTimerQueue: Pointer;

function DeleteTimerQueue;
begin
  GetProcedureAddress(_DeleteTimerQueue, kernel32, 'DeleteTimerQueue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteTimerQueue]
  end;
end;
{$ELSE}
function DeleteTimerQueue; external kernel32 name 'DeleteTimerQueue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentHwProfileA: Pointer;

function GetCurrentHwProfileA;
begin
  GetProcedureAddress(_GetCurrentHwProfileA, advapi32, 'GetCurrentHwProfileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentHwProfileA]
  end;
end;
{$ELSE}
function GetCurrentHwProfileA; external advapi32 name 'GetCurrentHwProfileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentHwProfileW: Pointer;

function GetCurrentHwProfileW;
begin
  GetProcedureAddress(_GetCurrentHwProfileW, advapi32, 'GetCurrentHwProfileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentHwProfileW]
  end;
end;
{$ELSE}
function GetCurrentHwProfileW; external advapi32 name 'GetCurrentHwProfileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentHwProfile: Pointer;

function GetCurrentHwProfile;
begin
  GetProcedureAddress(_GetCurrentHwProfile, advapi32, 'GetCurrentHwProfileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentHwProfile]
  end;
end;
{$ELSE}
function GetCurrentHwProfile; external advapi32 name 'GetCurrentHwProfileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentHwProfile: Pointer;

function GetCurrentHwProfile;
begin
  GetProcedureAddress(_GetCurrentHwProfile, advapi32, 'GetCurrentHwProfileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentHwProfile]
  end;
end;
{$ELSE}
function GetCurrentHwProfile; external advapi32 name 'GetCurrentHwProfileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _QueryPerformanceCounter: Pointer;

function QueryPerformanceCounter;
begin
  GetProcedureAddress(_QueryPerformanceCounter, kernel32, 'QueryPerformanceCounter');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryPerformanceCounter]
  end;
end;
{$ELSE}
function QueryPerformanceCounter; external kernel32 name 'QueryPerformanceCounter';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueryPerformanceFrequency: Pointer;

function QueryPerformanceFrequency;
begin
  GetProcedureAddress(_QueryPerformanceFrequency, kernel32, 'QueryPerformanceFrequency');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryPerformanceFrequency]
  end;
end;
{$ELSE}
function QueryPerformanceFrequency; external kernel32 name 'QueryPerformanceFrequency';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVersionExA: Pointer;

function GetVersionExA;
begin
  GetProcedureAddress(_GetVersionExA, kernel32, 'GetVersionExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVersionExA]
  end;
end;
{$ELSE}
function GetVersionExA; external kernel32 name 'GetVersionExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVersionExW: Pointer;

function GetVersionExW;
begin
  GetProcedureAddress(_GetVersionExW, kernel32, 'GetVersionExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVersionExW]
  end;
end;
{$ELSE}
function GetVersionExW; external kernel32 name 'GetVersionExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVersionEx: Pointer;

function GetVersionEx;
begin
  GetProcedureAddress(_GetVersionEx, kernel32, 'GetVersionExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVersionEx]
  end;
end;
{$ELSE}
function GetVersionEx; external kernel32 name 'GetVersionExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVersionEx: Pointer;

function GetVersionEx;
begin
  GetProcedureAddress(_GetVersionEx, kernel32, 'GetVersionExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVersionEx]
  end;
end;
{$ELSE}
function GetVersionEx; external kernel32 name 'GetVersionExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _VerifyVersionInfoA: Pointer;

function VerifyVersionInfoA;
begin
  GetProcedureAddress(_VerifyVersionInfoA, kernel32, 'VerifyVersionInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerifyVersionInfoA]
  end;
end;
{$ELSE}
function VerifyVersionInfoA; external kernel32 name 'VerifyVersionInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VerifyVersionInfoW: Pointer;

function VerifyVersionInfoW;
begin
  GetProcedureAddress(_VerifyVersionInfoW, kernel32, 'VerifyVersionInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerifyVersionInfoW]
  end;
end;
{$ELSE}
function VerifyVersionInfoW; external kernel32 name 'VerifyVersionInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VerifyVersionInfo: Pointer;

function VerifyVersionInfo;
begin
  GetProcedureAddress(_VerifyVersionInfo, kernel32, 'VerifyVersionInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerifyVersionInfo]
  end;
end;
{$ELSE}
function VerifyVersionInfo; external kernel32 name 'VerifyVersionInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VerifyVersionInfo: Pointer;

function VerifyVersionInfo;
begin
  GetProcedureAddress(_VerifyVersionInfo, kernel32, 'VerifyVersionInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerifyVersionInfo]
  end;
end;
{$ELSE}
function VerifyVersionInfo; external kernel32 name 'VerifyVersionInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemPowerStatus: Pointer;

function GetSystemPowerStatus;
begin
  GetProcedureAddress(_GetSystemPowerStatus, kernel32, 'GetSystemPowerStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemPowerStatus]
  end;
end;
{$ELSE}
function GetSystemPowerStatus; external kernel32 name 'GetSystemPowerStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSystemPowerState: Pointer;

function SetSystemPowerState;
begin
  GetProcedureAddress(_SetSystemPowerState, kernel32, 'SetSystemPowerState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSystemPowerState]
  end;
end;
{$ELSE}
function SetSystemPowerState; external kernel32 name 'SetSystemPowerState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AllocateUserPhysicalPages: Pointer;

function AllocateUserPhysicalPages;
begin
  GetProcedureAddress(_AllocateUserPhysicalPages, kernel32, 'AllocateUserPhysicalPages');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllocateUserPhysicalPages]
  end;
end;
{$ELSE}
function AllocateUserPhysicalPages; external kernel32 name 'AllocateUserPhysicalPages';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeUserPhysicalPages: Pointer;

function FreeUserPhysicalPages;
begin
  GetProcedureAddress(_FreeUserPhysicalPages, kernel32, 'FreeUserPhysicalPages');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeUserPhysicalPages]
  end;
end;
{$ELSE}
function FreeUserPhysicalPages; external kernel32 name 'FreeUserPhysicalPages';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapUserPhysicalPages: Pointer;

function MapUserPhysicalPages;
begin
  GetProcedureAddress(_MapUserPhysicalPages, kernel32, 'MapUserPhysicalPages');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapUserPhysicalPages]
  end;
end;
{$ELSE}
function MapUserPhysicalPages; external kernel32 name 'MapUserPhysicalPages';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapUserPhysicalPagesScatter: Pointer;

function MapUserPhysicalPagesScatter;
begin
  GetProcedureAddress(_MapUserPhysicalPagesScatter, kernel32, 'MapUserPhysicalPagesScatter');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapUserPhysicalPagesScatter]
  end;
end;
{$ELSE}
function MapUserPhysicalPagesScatter; external kernel32 name 'MapUserPhysicalPagesScatter';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateJobObjectA: Pointer;

function CreateJobObjectA;
begin
  GetProcedureAddress(_CreateJobObjectA, kernel32, 'CreateJobObjectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateJobObjectA]
  end;
end;
{$ELSE}
function CreateJobObjectA; external kernel32 name 'CreateJobObjectA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateJobObjectW: Pointer;

function CreateJobObjectW;
begin
  GetProcedureAddress(_CreateJobObjectW, kernel32, 'CreateJobObjectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateJobObjectW]
  end;
end;
{$ELSE}
function CreateJobObjectW; external kernel32 name 'CreateJobObjectW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateJobObject: Pointer;

function CreateJobObject;
begin
  GetProcedureAddress(_CreateJobObject, kernel32, 'CreateJobObjectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateJobObject]
  end;
end;
{$ELSE}
function CreateJobObject; external kernel32 name 'CreateJobObjectW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateJobObject: Pointer;

function CreateJobObject;
begin
  GetProcedureAddress(_CreateJobObject, kernel32, 'CreateJobObjectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateJobObject]
  end;
end;
{$ELSE}
function CreateJobObject; external kernel32 name 'CreateJobObjectA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenJobObjectA: Pointer;

function OpenJobObjectA;
begin
  GetProcedureAddress(_OpenJobObjectA, kernel32, 'OpenJobObjectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenJobObjectA]
  end;
end;
{$ELSE}
function OpenJobObjectA; external kernel32 name 'OpenJobObjectA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenJobObjectW: Pointer;

function OpenJobObjectW;
begin
  GetProcedureAddress(_OpenJobObjectW, kernel32, 'OpenJobObjectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenJobObjectW]
  end;
end;
{$ELSE}
function OpenJobObjectW; external kernel32 name 'OpenJobObjectW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenJobObject: Pointer;

function OpenJobObject;
begin
  GetProcedureAddress(_OpenJobObject, kernel32, 'OpenJobObjectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenJobObject]
  end;
end;
{$ELSE}
function OpenJobObject; external kernel32 name 'OpenJobObjectW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenJobObject: Pointer;

function OpenJobObject;
begin
  GetProcedureAddress(_OpenJobObject, kernel32, 'OpenJobObjectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenJobObject]
  end;
end;
{$ELSE}
function OpenJobObject; external kernel32 name 'OpenJobObjectA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AssignProcessToJobObject: Pointer;

function AssignProcessToJobObject;
begin
  GetProcedureAddress(_AssignProcessToJobObject, kernel32, 'AssignProcessToJobObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AssignProcessToJobObject]
  end;
end;
{$ELSE}
function AssignProcessToJobObject; external kernel32 name 'AssignProcessToJobObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TerminateJobObject: Pointer;

function TerminateJobObject;
begin
  GetProcedureAddress(_TerminateJobObject, kernel32, 'TerminateJobObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TerminateJobObject]
  end;
end;
{$ELSE}
function TerminateJobObject; external kernel32 name 'TerminateJobObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueryInformationJobObject: Pointer;

function QueryInformationJobObject;
begin
  GetProcedureAddress(_QueryInformationJobObject, kernel32, 'QueryInformationJobObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryInformationJobObject]
  end;
end;
{$ELSE}
function QueryInformationJobObject; external kernel32 name 'QueryInformationJobObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetInformationJobObject: Pointer;

function SetInformationJobObject;
begin
  GetProcedureAddress(_SetInformationJobObject, kernel32, 'SetInformationJobObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetInformationJobObject]
  end;
end;
{$ELSE}
function SetInformationJobObject; external kernel32 name 'SetInformationJobObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsProcessInJob: Pointer;

function IsProcessInJob;
begin
  GetProcedureAddress(_IsProcessInJob, kernel32, 'IsProcessInJob');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsProcessInJob]
  end;
end;
{$ELSE}
function IsProcessInJob; external kernel32 name 'IsProcessInJob';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateJobSet: Pointer;

function CreateJobSet;
begin
  GetProcedureAddress(_CreateJobSet, kernel32, 'CreateJobSet');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateJobSet]
  end;
end;
{$ELSE}
function CreateJobSet; external kernel32 name 'CreateJobSet';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddVectoredExceptionHandler: Pointer;

function AddVectoredExceptionHandler;
begin
  GetProcedureAddress(_AddVectoredExceptionHandler, kernel32, 'AddVectoredExceptionHandler');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddVectoredExceptionHandler]
  end;
end;
{$ELSE}
function AddVectoredExceptionHandler; external kernel32 name 'AddVectoredExceptionHandler';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveVectoredExceptionHandler: Pointer;

function RemoveVectoredExceptionHandler;
begin
  GetProcedureAddress(_RemoveVectoredExceptionHandler, kernel32, 'RemoveVectoredExceptionHandler');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveVectoredExceptionHandler]
  end;
end;
{$ELSE}
function RemoveVectoredExceptionHandler; external kernel32 name 'RemoveVectoredExceptionHandler';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolumeA: Pointer;

function FindFirstVolumeA;
begin
  GetProcedureAddress(_FindFirstVolumeA, kernel32, 'FindFirstVolumeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolumeA]
  end;
end;
{$ELSE}
function FindFirstVolumeA; external kernel32 name 'FindFirstVolumeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolumeW: Pointer;

function FindFirstVolumeW;
begin
  GetProcedureAddress(_FindFirstVolumeW, kernel32, 'FindFirstVolumeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolumeW]
  end;
end;
{$ELSE}
function FindFirstVolumeW; external kernel32 name 'FindFirstVolumeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolume: Pointer;

function FindFirstVolume;
begin
  GetProcedureAddress(_FindFirstVolume, kernel32, 'FindFirstVolumeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolume]
  end;
end;
{$ELSE}
function FindFirstVolume; external kernel32 name 'FindFirstVolumeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolume: Pointer;

function FindFirstVolume;
begin
  GetProcedureAddress(_FindFirstVolume, kernel32, 'FindFirstVolumeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolume]
  end;
end;
{$ELSE}
function FindFirstVolume; external kernel32 name 'FindFirstVolumeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolumeA: Pointer;

function FindNextVolumeA;
begin
  GetProcedureAddress(_FindNextVolumeA, kernel32, 'FindNextVolumeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolumeA]
  end;
end;
{$ELSE}
function FindNextVolumeA; external kernel32 name 'FindNextVolumeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolumeW: Pointer;

function FindNextVolumeW;
begin
  GetProcedureAddress(_FindNextVolumeW, kernel32, 'FindNextVolumeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolumeW]
  end;
end;
{$ELSE}
function FindNextVolumeW; external kernel32 name 'FindNextVolumeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolume: Pointer;

function FindNextVolume;
begin
  GetProcedureAddress(_FindNextVolume, kernel32, 'FindNextVolumeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolume]
  end;
end;
{$ELSE}
function FindNextVolume; external kernel32 name 'FindNextVolumeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolume: Pointer;

function FindNextVolume;
begin
  GetProcedureAddress(_FindNextVolume, kernel32, 'FindNextVolumeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolume]
  end;
end;
{$ELSE}
function FindNextVolume; external kernel32 name 'FindNextVolumeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindVolumeClose: Pointer;

function FindVolumeClose;
begin
  GetProcedureAddress(_FindVolumeClose, kernel32, 'FindVolumeClose');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindVolumeClose]
  end;
end;
{$ELSE}
function FindVolumeClose; external kernel32 name 'FindVolumeClose';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolumeMountPointA: Pointer;

function FindFirstVolumeMountPointA;
begin
  GetProcedureAddress(_FindFirstVolumeMountPointA, kernel32, 'FindFirstVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolumeMountPointA]
  end;
end;
{$ELSE}
function FindFirstVolumeMountPointA; external kernel32 name 'FindFirstVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolumeMountPointW: Pointer;

function FindFirstVolumeMountPointW;
begin
  GetProcedureAddress(_FindFirstVolumeMountPointW, kernel32, 'FindFirstVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolumeMountPointW]
  end;
end;
{$ELSE}
function FindFirstVolumeMountPointW; external kernel32 name 'FindFirstVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolumeMountPoint: Pointer;

function FindFirstVolumeMountPoint;
begin
  GetProcedureAddress(_FindFirstVolumeMountPoint, kernel32, 'FindFirstVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolumeMountPoint]
  end;
end;
{$ELSE}
function FindFirstVolumeMountPoint; external kernel32 name 'FindFirstVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindFirstVolumeMountPoint: Pointer;

function FindFirstVolumeMountPoint;
begin
  GetProcedureAddress(_FindFirstVolumeMountPoint, kernel32, 'FindFirstVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindFirstVolumeMountPoint]
  end;
end;
{$ELSE}
function FindFirstVolumeMountPoint; external kernel32 name 'FindFirstVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolumeMountPointA: Pointer;

function FindNextVolumeMountPointA;
begin
  GetProcedureAddress(_FindNextVolumeMountPointA, kernel32, 'FindNextVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolumeMountPointA]
  end;
end;
{$ELSE}
function FindNextVolumeMountPointA; external kernel32 name 'FindNextVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolumeMountPointW: Pointer;

function FindNextVolumeMountPointW;
begin
  GetProcedureAddress(_FindNextVolumeMountPointW, kernel32, 'FindNextVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolumeMountPointW]
  end;
end;
{$ELSE}
function FindNextVolumeMountPointW; external kernel32 name 'FindNextVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolumeMountPoint: Pointer;

function FindNextVolumeMountPoint;
begin
  GetProcedureAddress(_FindNextVolumeMountPoint, kernel32, 'FindNextVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolumeMountPoint]
  end;
end;
{$ELSE}
function FindNextVolumeMountPoint; external kernel32 name 'FindNextVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindNextVolumeMountPoint: Pointer;

function FindNextVolumeMountPoint;
begin
  GetProcedureAddress(_FindNextVolumeMountPoint, kernel32, 'FindNextVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindNextVolumeMountPoint]
  end;
end;
{$ELSE}
function FindNextVolumeMountPoint; external kernel32 name 'FindNextVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindVolumeMountPointClose: Pointer;

function FindVolumeMountPointClose;
begin
  GetProcedureAddress(_FindVolumeMountPointClose, kernel32, 'FindVolumeMountPointClose');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindVolumeMountPointClose]
  end;
end;
{$ELSE}
function FindVolumeMountPointClose; external kernel32 name 'FindVolumeMountPointClose';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeMountPointA: Pointer;

function SetVolumeMountPointA;
begin
  GetProcedureAddress(_SetVolumeMountPointA, kernel32, 'SetVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeMountPointA]
  end;
end;
{$ELSE}
function SetVolumeMountPointA; external kernel32 name 'SetVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeMountPointW: Pointer;

function SetVolumeMountPointW;
begin
  GetProcedureAddress(_SetVolumeMountPointW, kernel32, 'SetVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeMountPointW]
  end;
end;
{$ELSE}
function SetVolumeMountPointW; external kernel32 name 'SetVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeMountPoint: Pointer;

function SetVolumeMountPoint;
begin
  GetProcedureAddress(_SetVolumeMountPoint, kernel32, 'SetVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeMountPoint]
  end;
end;
{$ELSE}
function SetVolumeMountPoint; external kernel32 name 'SetVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetVolumeMountPoint: Pointer;

function SetVolumeMountPoint;
begin
  GetProcedureAddress(_SetVolumeMountPoint, kernel32, 'SetVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetVolumeMountPoint]
  end;
end;
{$ELSE}
function SetVolumeMountPoint; external kernel32 name 'SetVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteVolumeMountPointA: Pointer;

function DeleteVolumeMountPointA;
begin
  GetProcedureAddress(_DeleteVolumeMountPointA, kernel32, 'DeleteVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteVolumeMountPointA]
  end;
end;
{$ELSE}
function DeleteVolumeMountPointA; external kernel32 name 'DeleteVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteVolumeMountPointW: Pointer;

function DeleteVolumeMountPointW;
begin
  GetProcedureAddress(_DeleteVolumeMountPointW, kernel32, 'DeleteVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteVolumeMountPointW]
  end;
end;
{$ELSE}
function DeleteVolumeMountPointW; external kernel32 name 'DeleteVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteVolumeMountPoint: Pointer;

function DeleteVolumeMountPoint;
begin
  GetProcedureAddress(_DeleteVolumeMountPoint, kernel32, 'DeleteVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteVolumeMountPoint]
  end;
end;
{$ELSE}
function DeleteVolumeMountPoint; external kernel32 name 'DeleteVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteVolumeMountPoint: Pointer;

function DeleteVolumeMountPoint;
begin
  GetProcedureAddress(_DeleteVolumeMountPoint, kernel32, 'DeleteVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteVolumeMountPoint]
  end;
end;
{$ELSE}
function DeleteVolumeMountPoint; external kernel32 name 'DeleteVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeNameForVolMountPointA: Pointer;

function GetVolumeNameForVolumeMountPointA;
begin
  GetProcedureAddress(_GetVolumeNameForVolMountPointA, kernel32, 'GetVolumeNameForVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeNameForVolMountPointA]
  end;
end;
{$ELSE}
function GetVolumeNameForVolumeMountPointA; external kernel32 name 'GetVolumeNameForVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeNameForVolMountPointW: Pointer;

function GetVolumeNameForVolumeMountPointW;
begin
  GetProcedureAddress(_GetVolumeNameForVolMountPointW, kernel32, 'GetVolumeNameForVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeNameForVolMountPointW]
  end;
end;
{$ELSE}
function GetVolumeNameForVolumeMountPointW; external kernel32 name 'GetVolumeNameForVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeNameForVolMountPoint: Pointer;

function GetVolumeNameForVolumeMountPoint;
begin
  GetProcedureAddress(_GetVolumeNameForVolMountPoint, kernel32, 'GetVolumeNameForVolumeMountPointW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeNameForVolMountPoint]
  end;
end;
{$ELSE}
function GetVolumeNameForVolumeMountPoint; external kernel32 name 'GetVolumeNameForVolumeMountPointW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumeNameForVolMountPoint: Pointer;

function GetVolumeNameForVolumeMountPoint;
begin
  GetProcedureAddress(_GetVolumeNameForVolMountPoint, kernel32, 'GetVolumeNameForVolumeMountPointA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumeNameForVolMountPoint]
  end;
end;
{$ELSE}
function GetVolumeNameForVolumeMountPoint; external kernel32 name 'GetVolumeNameForVolumeMountPointA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathNameA: Pointer;

function GetVolumePathNameA;
begin
  GetProcedureAddress(_GetVolumePathNameA, kernel32, 'GetVolumePathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathNameA]
  end;
end;
{$ELSE}
function GetVolumePathNameA; external kernel32 name 'GetVolumePathNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathNameW: Pointer;

function GetVolumePathNameW;
begin
  GetProcedureAddress(_GetVolumePathNameW, kernel32, 'GetVolumePathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathNameW]
  end;
end;
{$ELSE}
function GetVolumePathNameW; external kernel32 name 'GetVolumePathNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathName: Pointer;

function GetVolumePathName;
begin
  GetProcedureAddress(_GetVolumePathName, kernel32, 'GetVolumePathNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathName]
  end;
end;
{$ELSE}
function GetVolumePathName; external kernel32 name 'GetVolumePathNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathName: Pointer;

function GetVolumePathName;
begin
  GetProcedureAddress(_GetVolumePathName, kernel32, 'GetVolumePathNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathName]
  end;
end;
{$ELSE}
function GetVolumePathName; external kernel32 name 'GetVolumePathNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathNamesForVolNameA: Pointer;

function GetVolumePathNamesForVolumeNameA;
begin
  GetProcedureAddress(_GetVolumePathNamesForVolNameA, kernel32, 'GetVolumePathNamesForVolumeNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathNamesForVolNameA]
  end;
end;
{$ELSE}
function GetVolumePathNamesForVolumeNameA; external kernel32 name 'GetVolumePathNamesForVolumeNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathNamesForVolNameW: Pointer;

function GetVolumePathNamesForVolumeNameW;
begin
  GetProcedureAddress(_GetVolumePathNamesForVolNameW, kernel32, 'GetVolumePathNamesForVolumeNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathNamesForVolNameW]
  end;
end;
{$ELSE}
function GetVolumePathNamesForVolumeNameW; external kernel32 name 'GetVolumePathNamesForVolumeNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathNamesForVolName: Pointer;

function GetVolumePathNamesForVolumeName;
begin
  GetProcedureAddress(_GetVolumePathNamesForVolName, kernel32, 'GetVolumePathNamesForVolumeNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathNamesForVolName]
  end;
end;
{$ELSE}
function GetVolumePathNamesForVolumeName; external kernel32 name 'GetVolumePathNamesForVolumeNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetVolumePathNamesForVolName: Pointer;

function GetVolumePathNamesForVolumeName;
begin
  GetProcedureAddress(_GetVolumePathNamesForVolName, kernel32, 'GetVolumePathNamesForVolumeNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetVolumePathNamesForVolName]
  end;
end;
{$ELSE}
function GetVolumePathNamesForVolumeName; external kernel32 name 'GetVolumePathNamesForVolumeNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateActCtxA: Pointer;

function CreateActCtxA;
begin
  GetProcedureAddress(_CreateActCtxA, kernel32, 'CreateActCtxA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateActCtxA]
  end;
end;
{$ELSE}
function CreateActCtxA; external kernel32 name 'CreateActCtxA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateActCtxW: Pointer;

function CreateActCtxW;
begin
  GetProcedureAddress(_CreateActCtxW, kernel32, 'CreateActCtxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateActCtxW]
  end;
end;
{$ELSE}
function CreateActCtxW; external kernel32 name 'CreateActCtxW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateActCtx: Pointer;

function CreateActCtx;
begin
  GetProcedureAddress(_CreateActCtx, kernel32, 'CreateActCtxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateActCtx]
  end;
end;
{$ELSE}
function CreateActCtx; external kernel32 name 'CreateActCtxW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateActCtx: Pointer;

function CreateActCtx;
begin
  GetProcedureAddress(_CreateActCtx, kernel32, 'CreateActCtxA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateActCtx]
  end;
end;
{$ELSE}
function CreateActCtx; external kernel32 name 'CreateActCtxA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AddRefActCtx: Pointer;

procedure AddRefActCtx;
begin
  GetProcedureAddress(_AddRefActCtx, kernel32, 'AddRefActCtx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddRefActCtx]
  end;
end;
{$ELSE}
procedure AddRefActCtx; external kernel32 name 'AddRefActCtx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReleaseActCtx: Pointer;

procedure ReleaseActCtx;
begin
  GetProcedureAddress(_ReleaseActCtx, kernel32, 'ReleaseActCtx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReleaseActCtx]
  end;
end;
{$ELSE}
procedure ReleaseActCtx; external kernel32 name 'ReleaseActCtx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ZombifyActCtx: Pointer;

function ZombifyActCtx;
begin
  GetProcedureAddress(_ZombifyActCtx, kernel32, 'ZombifyActCtx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ZombifyActCtx]
  end;
end;
{$ELSE}
function ZombifyActCtx; external kernel32 name 'ZombifyActCtx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ActivateActCtx: Pointer;

function ActivateActCtx;
begin
  GetProcedureAddress(_ActivateActCtx, kernel32, 'ActivateActCtx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ActivateActCtx]
  end;
end;
{$ELSE}
function ActivateActCtx; external kernel32 name 'ActivateActCtx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeactivateActCtx: Pointer;

function DeactivateActCtx;
begin
  GetProcedureAddress(_DeactivateActCtx, kernel32, 'DeactivateActCtx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeactivateActCtx]
  end;
end;
{$ELSE}
function DeactivateActCtx; external kernel32 name 'DeactivateActCtx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentActCtx: Pointer;

function GetCurrentActCtx;
begin
  GetProcedureAddress(_GetCurrentActCtx, kernel32, 'GetCurrentActCtx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentActCtx]
  end;
end;
{$ELSE}
function GetCurrentActCtx; external kernel32 name 'GetCurrentActCtx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindActCtxSectionStringA: Pointer;

function FindActCtxSectionStringA;
begin
  GetProcedureAddress(_FindActCtxSectionStringA, kernel32, 'FindActCtxSectionStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindActCtxSectionStringA]
  end;
end;
{$ELSE}
function FindActCtxSectionStringA; external kernel32 name 'FindActCtxSectionStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindActCtxSectionStringW: Pointer;

function FindActCtxSectionStringW;
begin
  GetProcedureAddress(_FindActCtxSectionStringW, kernel32, 'FindActCtxSectionStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindActCtxSectionStringW]
  end;
end;
{$ELSE}
function FindActCtxSectionStringW; external kernel32 name 'FindActCtxSectionStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindActCtxSectionString: Pointer;

function FindActCtxSectionString;
begin
  GetProcedureAddress(_FindActCtxSectionString, kernel32, 'FindActCtxSectionStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindActCtxSectionString]
  end;
end;
{$ELSE}
function FindActCtxSectionString; external kernel32 name 'FindActCtxSectionStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindActCtxSectionString: Pointer;

function FindActCtxSectionString;
begin
  GetProcedureAddress(_FindActCtxSectionString, kernel32, 'FindActCtxSectionStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindActCtxSectionString]
  end;
end;
{$ELSE}
function FindActCtxSectionString; external kernel32 name 'FindActCtxSectionStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindActCtxSectionGuid: Pointer;

function FindActCtxSectionGuid;
begin
  GetProcedureAddress(_FindActCtxSectionGuid, kernel32, 'FindActCtxSectionGuid');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindActCtxSectionGuid]
  end;
end;
{$ELSE}
function FindActCtxSectionGuid; external kernel32 name 'FindActCtxSectionGuid';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _QueryActCtxW: Pointer;

function QueryActCtxW;
begin
  GetProcedureAddress(_QueryActCtxW, kernel32, 'QueryActCtxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_QueryActCtxW]
  end;
end;
{$ELSE}
function QueryActCtxW; external kernel32 name 'QueryActCtxW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ProcessIdToSessionId: Pointer;

function ProcessIdToSessionId;
begin
  GetProcedureAddress(_ProcessIdToSessionId, kernel32, 'ProcessIdToSessionId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ProcessIdToSessionId]
  end;
end;
{$ELSE}
function ProcessIdToSessionId; external kernel32 name 'ProcessIdToSessionId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WTSGetActiveConsoleSessionId: Pointer;

function WTSGetActiveConsoleSessionId;
begin
  GetProcedureAddress(_WTSGetActiveConsoleSessionId, kernel32, 'WTSGetActiveConsoleSessionId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WTSGetActiveConsoleSessionId]
  end;
end;
{$ELSE}
function WTSGetActiveConsoleSessionId; external kernel32 name 'WTSGetActiveConsoleSessionId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsWow64Process: Pointer;

function IsWow64Process;
begin
  GetProcedureAddress(_IsWow64Process, kernel32, 'IsWow64Process');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWow64Process]
  end;
end;
{$ELSE}
function IsWow64Process; external kernel32 name 'IsWow64Process';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLogicalProcessorInformation: Pointer;

function GetLogicalProcessorInformation;
begin
  GetProcedureAddress(_GetLogicalProcessorInformation, kernel32, 'GetLogicalProcessorInformation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLogicalProcessorInformation]
  end;
end;
{$ELSE}
function GetLogicalProcessorInformation; external kernel32 name 'GetLogicalProcessorInformation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumaHighestNodeNumber: Pointer;

function GetNumaHighestNodeNumber;
begin
  GetProcedureAddress(_GetNumaHighestNodeNumber, kernel32, 'GetNumaHighestNodeNumber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumaHighestNodeNumber]
  end;
end;
{$ELSE}
function GetNumaHighestNodeNumber; external kernel32 name 'GetNumaHighestNodeNumber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumaProcessorNode: Pointer;

function GetNumaProcessorNode;
begin
  GetProcedureAddress(_GetNumaProcessorNode, kernel32, 'GetNumaProcessorNode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumaProcessorNode]
  end;
end;
{$ELSE}
function GetNumaProcessorNode; external kernel32 name 'GetNumaProcessorNode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumaNodeProcessorMask: Pointer;

function GetNumaNodeProcessorMask;
begin
  GetProcedureAddress(_GetNumaNodeProcessorMask, kernel32, 'GetNumaNodeProcessorMask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumaNodeProcessorMask]
  end;
end;
{$ELSE}
function GetNumaNodeProcessorMask; external kernel32 name 'GetNumaNodeProcessorMask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumaAvailableMemoryNode: Pointer;

function GetNumaAvailableMemoryNode;
begin
  GetProcedureAddress(_GetNumaAvailableMemoryNode, kernel32, 'GetNumaAvailableMemoryNode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumaAvailableMemoryNode]
  end;
end;
{$ELSE}
function GetNumaAvailableMemoryNode; external kernel32 name 'GetNumaAvailableMemoryNode';
{$ENDIF DYNAMIC_LINK}

end.
