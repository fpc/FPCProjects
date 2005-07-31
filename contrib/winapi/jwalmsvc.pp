{******************************************************************************}
{                                                       	               }
{ Lan Manager Service API interface Unit for Object Pascal                     }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: lmsvc.h, released November 2001. The original Pascal   }
{ code is: LmSvc.pas, released Februari 2002. The initial developer of the     }
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

unit JwaLmSvc;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "lmsvc.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaLmCons, JwaWinType;

//
//  Data Structures
//

type
  LPSERVICE_INFO_0 = ^SERVICE_INFO_0;
  PSERVICE_INFO_0 = ^SERVICE_INFO_0;
  _SERVICE_INFO_0 = record
    svci0_name: LPWSTR;
  end;
  SERVICE_INFO_0 = _SERVICE_INFO_0;
  TServiceInfo0 = SERVICE_INFO_0;
  PServiceInfo0 = PSERVICE_INFO_0;

  LPSERVICE_INFO_1 = ^SERVICE_INFO_1;
  PSERVICE_INFO_1 = ^SERVICE_INFO_1;
  _SERVICE_INFO_1 = record
    svci1_name: LPWSTR;
    svci1_status: DWORD;
    svci1_code: DWORD;
    svci1_pid: DWORD;
  end;
  SERVICE_INFO_1 = _SERVICE_INFO_1;
  TServiceInfo1 = SERVICE_INFO_1;
  PServiceInfo1 = PSERVICE_INFO_1;

  LPSERVICE_INFO_2 = ^SERVICE_INFO_2;
  PSERVICE_INFO_2 = ^SERVICE_INFO_2;
  _SERVICE_INFO_2 = record
    svci2_name: LPWSTR;
    svci2_status: DWORD;
    svci2_code: DWORD;
    svci2_pid: DWORD;
    svci2_text: LPWSTR;
    svci2_specific_error: DWORD;
    svci2_display_name: LPWSTR;
  end;
  SERVICE_INFO_2 = _SERVICE_INFO_2;
  TServiceInfo2 = SERVICE_INFO_2;
  PServiceInfo2 = PSERVICE_INFO_2;

//
// Function Prototypes
//

function NetServiceControl(servername, service: LPCWSTR; opcode: DWORD; arg: DWORD; var bufptr: LPBYTE): NET_API_STATUS; stdcall;

function NetServiceEnum(servername: LPCWSTR; level: DWORD; var bufptr: LPBYTE; prefmaxlen: DWORD; entriesread, totalentries, resume_handle: LPDWORD): NET_API_STATUS; stdcall;

function NetServiceGetInfo(servername, service: LPCWSTR; level: DWORD; var bufptr: LPBYTE): NET_API_STATUS; stdcall;

function NetServiceInstall(servername, service: LPCWSTR; argc: DWORD; argv: LPCWSTR; var bufptr: LPBYTE): NET_API_STATUS; stdcall;

//
// Special Values and Constants
//

//
//  Bitmask and bit values for svci1_status, and svci2_status
//  fields.  For each "subfield", there is a mask defined,
//  and a number of constants representing the value
//  obtained by doing (status & mask).
//

// Bits 0,1 -- general status

const
  SERVICE_INSTALL_STATE     = $03;
  SERVICE_UNINSTALLED       = $00;
  SERVICE_INSTALL_PENDING   = $01;
  SERVICE_UNINSTALL_PENDING = $02;
  SERVICE_INSTALLED         = $03;

// Bits 2,3 -- paused/active status

  SERVICE_PAUSE_STATE           = $0C;
  LM20_SERVICE_ACTIVE           = $00;
  LM20_SERVICE_CONTINUE_PENDING = $04;
  LM20_SERVICE_PAUSE_PENDING    = $08;
  LM20_SERVICE_PAUSED           = $0C;

// Bit 4 -- uninstallable indication

  SERVICE_NOT_UNINSTALLABLE = $00;
  SERVICE_UNINSTALLABLE     = $10;

// Bit 5 -- pausable indication

  SERVICE_NOT_PAUSABLE = $00;
  SERVICE_PAUSABLE     = $20;

// Workstation service only:
// Bits 8,9,10 -- redirection paused/active

  SERVICE_REDIR_PAUSED       = $700;
  SERVICE_REDIR_DISK_PAUSED  = $100;
  SERVICE_REDIR_PRINT_PAUSED = $200;
  SERVICE_REDIR_COMM_PAUSED  = $400;

//
//  Additional standard LAN Manager for MS-DOS services
//

  SERVICE_DOS_ENCRYPTION = WideString('ENCRYPT');

//
//  NetServiceControl opcodes.
//

  SERVICE_CTRL_INTERROGATE = 0;
  SERVICE_CTRL_PAUSE       = 1;
  SERVICE_CTRL_CONTINUE    = 2;
  SERVICE_CTRL_UNINSTALL   = 3;

//
//  Workstation service only:  Bits used in the "arg" parameter
//  to NetServiceControl in conjunction with the opcode
//  SERVICE_CTRL_PAUSE or SERVICE_CTRL_CONTINUE, to pause or
//  continue redirection.
//

  SERVICE_CTRL_REDIR_DISK  = $1;
  SERVICE_CTRL_REDIR_PRINT = $2;
  SERVICE_CTRL_REDIR_COMM  = $4;

//
//  Values for svci1_code, and svci2_code when status
//  of the service is SERVICE_INSTALL_PENDING or
//  SERVICE_UNINSTALL_PENDING.
//  A service can optionally provide a hint to the installer
//  that the install is proceeding and how long to wait
//  (in 0.1 second increments) before querying status again.
//

  SERVICE_IP_NO_HINT  = $0;
  SERVICE_CCP_NO_HINT = $0;

  SERVICE_IP_QUERY_HINT  = $10000;
  SERVICE_CCP_QUERY_HINT = $10000;

//
// Mask for install proceeding checkpoint number
//

  SERVICE_IP_CHKPT_NUM  = $0FF;
  SERVICE_CCP_CHKPT_NUM = $0FF;

//
// Mask for wait time hint before querying again
//

  SERVICE_IP_WAIT_TIME  = $0FF00;
  SERVICE_CCP_WAIT_TIME = $0FF00;

//
// Shift count for building wait time _code values
//

  SERVICE_IP_WAITTIME_SHIFT   = 8;
  SERVICE_NTIP_WAITTIME_SHIFT = 12;

//
// Mask used for upper and lower portions of wait hint time.
//

  UPPER_HINT_MASK     = $0000FF00;
  LOWER_HINT_MASK     = $000000FF;
  UPPER_GET_HINT_MASK = $0FF00000;
  LOWER_GET_HINT_MASK = $0000FF00;
  SERVICE_NT_MAXTIME  = $0000FFFF;
  SERVICE_RESRV_MASK  = $0001FFFF;
  SERVICE_MAXTIME     = $000000FF;

//
//  SERVICE_BASE is the base of service error codes,
//  chosen to avoid conflict with OS, redirector,
//  netapi, and errlog codes.
//
// Don't change the comments following the manifest constants without
// understanding how mapmsg works.
//

  SERVICE_BASE       = 3050;
  SERVICE_UIC_NORMAL = 0;

{*
 *  Uninstall codes, to be used in high byte of 'code' on final NetStatus,
 *  which sets the status to UNINSTALLED.
 *}

  SERVICE_UIC_BADPARMVAL = (SERVICE_BASE + 1);

{*
 * The Registry or the information you just typed includes an illegal
 * value for "%1".
 *}

  SERVICE_UIC_MISSPARM = (SERVICE_BASE + 2);

{*
 * The required parameter was not provided on the command
 * line or in the configuration file.
 *}

  SERVICE_UIC_UNKPARM = (SERVICE_BASE + 3);

{*
 * LAN Manager does not recognize "%1" as a valid option.
 *}

  SERVICE_UIC_RESOURCE = (SERVICE_BASE + 4);

{*
 * A request for resource could not be satisfied.
 *}

  SERVICE_UIC_CONFIG = (SERVICE_BASE + 5);

{*
 * A problem exists with the system configuration.
 *}

  SERVICE_UIC_SYSTEM = (SERVICE_BASE + 6);

{*
 * A system error has occurred.
 *}

  SERVICE_UIC_INTERNAL = (SERVICE_BASE + 7);

{*
 * An internal consistency error has occurred.
 *}

  SERVICE_UIC_AMBIGPARM = (SERVICE_BASE + 8);

{*
 * The configuration file or the command line has an ambiguous option.
 *}

  SERVICE_UIC_DUPPARM = (SERVICE_BASE + 9);

{*
 * The configuration file or the command line has a duplicate parameter.
 *}

  SERVICE_UIC_KILL = (SERVICE_BASE + 10);

{*
 * The service did not respond to control and was stopped with
 * the DosKillProc function.
 *}

  SERVICE_UIC_EXEC = (SERVICE_BASE + 11);

{*
 * An error occurred when attempting to run the service program.
 *}

  SERVICE_UIC_SUBSERV = (SERVICE_BASE + 12);

{*
 * The sub-service failed to start.
 *}

  SERVICE_UIC_CONFLPARM = (SERVICE_BASE + 13);

{*
 * There is a conflict in the value or use of these options: %1.
 *}

  SERVICE_UIC_FILE = (SERVICE_BASE + 14);

{*
 * There is a problem with the file.
 *}



//
//  The modifiers
//

//
// General:
//

  SERVICE_UIC_M_NULL = 0;

//
//  RESOURCE:
//

  SERVICE_UIC_M_MEMORY    = (SERVICE_BASE + 20); // memory
  SERVICE_UIC_M_DISK      = (SERVICE_BASE + 21); // disk space
  SERVICE_UIC_M_THREADS   = (SERVICE_BASE + 22); // thread
  SERVICE_UIC_M_PROCESSES = (SERVICE_BASE + 23); // process

//
//  CONFIG:
//

//
// Security failure
//

  SERVICE_UIC_M_SECURITY = (SERVICE_BASE + 24);

{* Security Failure. %0 *}

  SERVICE_UIC_M_LANROOT = (SERVICE_BASE + 25);

{*
 * Bad or missing LAN Manager root directory.
 *}

  SERVICE_UIC_M_REDIR = (SERVICE_BASE + 26);

{*
 * The network software is not installed.
 *}

  SERVICE_UIC_M_SERVER = (SERVICE_BASE + 27);

{*
 * The server is not started.
 *}

  SERVICE_UIC_M_SEC_FILE_ERR = (SERVICE_BASE + 28);

{*
 * The server cannot access the user accounts database (NET.ACC).
 *}

  SERVICE_UIC_M_FILES = (SERVICE_BASE + 29);

{*
 * Incompatible files are installed in the LANMAN tree.
 *}

  SERVICE_UIC_M_LOGS = (SERVICE_BASE + 30);

{*
 * The LANMAN\LOGS directory is invalid.
 *}

  SERVICE_UIC_M_LANGROUP = (SERVICE_BASE + 31);

{*
 * The domain specified could not be used.
 *}

  SERVICE_UIC_M_MSGNAME = (SERVICE_BASE + 32);

{*
 * The computer name is being used as a message alias on another computer.
 *}

  SERVICE_UIC_M_ANNOUNCE = (SERVICE_BASE + 33);

{*
 * The announcement of the server name failed.
 *}

  SERVICE_UIC_M_UAS = (SERVICE_BASE + 34);

{*
 * The user accounts database is not configured correctly.
 *}

  SERVICE_UIC_M_SERVER_SEC_ERR = (SERVICE_BASE + 35);

{*
 * The server is not running with user-level security.
 *}

  SERVICE_UIC_M_WKSTA = (SERVICE_BASE + 37);

{*
 * The workstation is not configured properly.
 *}

  SERVICE_UIC_M_ERRLOG = (SERVICE_BASE + 38);

{*
 * View your error log for details.
 *}

  SERVICE_UIC_M_FILE_UW = (SERVICE_BASE + 39);

{*
 * Unable to write to this file.
 *}

  SERVICE_UIC_M_ADDPAK = (SERVICE_BASE + 40);

{*
 * ADDPAK file is corrupted.  Delete LANMAN\NETPROG\ADDPAK.SER
 * and reapply all ADDPAKs.
 *}

  SERVICE_UIC_M_LAZY = (SERVICE_BASE + 41);

{*
 * The LM386 server cannot be started because CACHE.EXE is not running.
 *}

  SERVICE_UIC_M_UAS_MACHINE_ACCT = (SERVICE_BASE + 42);

{*
 * There is no account for this computer in the security database.
 *}

  SERVICE_UIC_M_UAS_SERVERS_NMEMB = (SERVICE_BASE + 43);

{*
 * This computer is not a member of the group SERVERS.
 *}

  SERVICE_UIC_M_UAS_SERVERS_NOGRP = (SERVICE_BASE + 44);

{*
 * The group SERVERS is not present in the local security database.
 *}

  SERVICE_UIC_M_UAS_INVALID_ROLE = (SERVICE_BASE + 45);

{*
 * This computer is configured as a member of a workgroup, not as
 * a member of a domain. The Netlogon service does not need to run in this
 * configuration.
 *}

  SERVICE_UIC_M_NETLOGON_NO_DC = (SERVICE_BASE + 46);

{*
 * The primary Domain Controller for this domain could not be located.
 *}

  SERVICE_UIC_M_NETLOGON_DC_CFLCT = (SERVICE_BASE + 47);

{*
 * This computer is configured to be the primary domain controller of its domain.
 * However, the computer %1 is currently claiming to be the primary domain controller
 * of the domain.
 *}

  SERVICE_UIC_M_NETLOGON_AUTH = (SERVICE_BASE + 48);

{*
 * The service failed to authenticate with the primary domain controller.
 *}

  SERVICE_UIC_M_UAS_PROLOG = (SERVICE_BASE + 49);

{*
 * There is a problem with the security database creation date or serial number.
 *}


  SERVICE2_BASE = 5600;

{* new SEVICE_UIC messages go here *}

  SERVICE_UIC_M_NETLOGON_MPATH = (SERVICE2_BASE + 0);

{*
 * Could not share the User or Script path.
 *}

  SERVICE_UIC_M_LSA_MACHINE_ACCT = (SERVICE2_BASE + 1);

{*
 * The password for this computer is not found in the local security
 * database.
 *}

  SERVICE_UIC_M_DATABASE_ERROR = (SERVICE2_BASE + 2);

{*
 * An internal error occurred while accessing the computer's
 * local or network security database.
 *}


//
//  End modifiers
//

//
// Commonly used Macros:
//

function SERVICE_IP_CODE(tt, nn: LONG): LONG;

function SERVICE_CCP_CODE(tt, nn: LONG): LONG;

function SERVICE_UIC_CODE(cc, mm: LONG): LONG;

//
// This macro takes a wait hint (tt) which can have a maximum value of
// 0xFFFF and puts it into the service status code field.
// 0x0FF1FFnn  (where nn is the checkpoint information).
//

function SERVICE_NT_CCP_CODE(tt, nn: LONG): LONG;

//
// This macro takes a status code field, and strips out the wait hint
// from the upper and lower sections.
// 0x0FF1FFnn results in 0x0000FFFF.
//

function SERVICE_NT_WAIT_GET(code: DWORD): DWORD;

implementation


{$IFDEF DYNAMIC_LINK}
var
  _NetServiceControl: Pointer;

function NetServiceControl;
begin
  GetProcedureAddress(_NetServiceControl, netapi32, 'NetServiceControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetServiceControl]
  end;
end;
{$ELSE}
function NetServiceControl; external netapi32 name 'NetServiceControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NetServiceEnum: Pointer;

function NetServiceEnum;
begin
  GetProcedureAddress(_NetServiceEnum, netapi32, 'NetServiceEnum');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetServiceEnum]
  end;
end;
{$ELSE}
function NetServiceEnum; external netapi32 name 'NetServiceEnum';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NetServiceGetInfo: Pointer;

function NetServiceGetInfo;
begin
  GetProcedureAddress(_NetServiceGetInfo, netapi32, 'NetServiceGetInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetServiceGetInfo]
  end;
end;
{$ELSE}
function NetServiceGetInfo; external netapi32 name 'NetServiceGetInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NetServiceInstall: Pointer;

function NetServiceInstall;
begin
  GetProcedureAddress(_NetServiceInstall, netapi32, 'NetServiceInstall');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetServiceInstall]
  end;
end;
{$ELSE}
function NetServiceInstall; external netapi32 name 'NetServiceInstall';
{$ENDIF DYNAMIC_LINK}

// #define SERVICE_IP_CODE(tt,nn) ((long)SERVICE_IP_QUERY_HINT|(long)(nn|(tt<<SERVICE_IP_WAITTIME_SHIFT)))

function SERVICE_IP_CODE(tt, nn: LONG): LONG;
begin
  Result := SERVICE_IP_QUERY_HINT or (nn or (tt shl SERVICE_IP_WAITTIME_SHIFT));
end;

// #define SERVICE_CCP_CODE(tt,nn) ((long)SERVICE_CCP_QUERY_HINT|(long)(nn|(tt<<SERVICE_IP_WAITTIME_SHIFT)))

function SERVICE_CCP_CODE(tt, nn: LONG): LONG;
begin
  Result := SERVICE_CCP_QUERY_HINT or (nn or (tt shl SERVICE_IP_WAITTIME_SHIFT));
end;

// #define SERVICE_UIC_CODE(cc,mm) ((long)(((long)cc<<16)|(long)(unsigned short)mm))

function SERVICE_UIC_CODE(cc, mm: LONG): LONG;
begin
  Result := (cc shl 16) or WORD(mm);
end;

// #define SERVICE_NT_CCP_CODE(tt,nn)  \
//   (  \
//     ((long)SERVICE_CCP_QUERY_HINT)   | \
//     ((long)(nn))   | \
//     (((tt)&LOWER_HINT_MASK) << SERVICE_IP_WAITTIME_SHIFT)   | \
//     (((tt)&UPPER_HINT_MASK) << SERVICE_NTIP_WAITTIME_SHIFT)   \
//   )

function SERVICE_NT_CCP_CODE(tt, nn: LongInt): LongInt;
begin
  Result := SERVICE_CCP_QUERY_HINT or nn or ((tt and LOWER_HINT_MASK) shl SERVICE_IP_WAITTIME_SHIFT) or ((tt and UPPER_HINT_MASK) shl SERVICE_NTIP_WAITTIME_SHIFT);
end;

// #define SERVICE_NT_WAIT_GET(code) \
//     (   \
//       (((code) & UPPER_GET_HINT_MASK) >> SERVICE_NTIP_WAITTIME_SHIFT)  |  \
//       (((code) & LOWER_GET_HINT_MASK) >> SERVICE_IP_WAITTIME_SHIFT)  \
//     )

function SERVICE_NT_WAIT_GET(code: DWORD): DWORD;
begin
  Result := ((code and UPPER_GET_HINT_MASK) shr SERVICE_NTIP_WAITTIME_SHIFT) or ((code and LOWER_GET_HINT_MASK) shr SERVICE_IP_WAITTIME_SHIFT);
end;

end.
