{******************************************************************************}
{                                                       	               }
{ Equates for WM_DEVICECHANGE and BroadcastSystemMessage for Object Pascal     }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: dbt.h, released June 2000. The original Pascal         }
{ code is: Dbt.pas, released December 2000. The initial developer of the       }
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

unit JwaDbt;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "dbt.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType, JwaWinUser;

//
// BroadcastSpecialMessage constants.
//

const
  WM_DEVICECHANGE = $0219;

(*
 * Broadcast message and receipient flags.
 *
 * Note that there is a third "flag". If the wParam has:
 *
 * bit 15 on:   lparam is a pointer and bit 14 is meaningfull.
 * bit 15 off:  lparam is just a UNLONG data type.
 *
 * bit 14 on:   lparam is a pointer to an ASCIIZ string.
 * bit 14 off:  lparam is a pointer to a binary struture starting with
 *              a dword describing the length of the structure.
 *)

  BSF_QUERY              = $00000001;
  BSF_IGNORECURRENTTASK  = $00000002;     // Meaningless for VxDs
  BSF_FLUSHDISK          = $00000004;     // Shouldn't be used by VxDs
  BSF_NOHANG             = $00000008;
  BSF_POSTMESSAGE        = $00000010;
  BSF_FORCEIFHUNG        = $00000020;
  BSF_NOTIMEOUTIFNOTHUNG = $00000040;
  BSF_MSGSRV32ISOK       = $80000000;     // Called synchronously from PM API
  BSF_MSGSRV32ISOK_BIT   = 31;            // Called synchronously from PM API

  BSM_ALLCOMPONENTS      = $00000000;
  BSM_VXDS               = $00000001;
  BSM_NETDRIVER          = $00000002;
  BSM_INSTALLABLEDRIVERS = $00000004;
  BSM_APPLICATIONS       = $00000008;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_APPYBEGIN
 * lParam  = (not used)
 *
 *      'Appy-time is now available.  This message is itself sent
 *      at 'Appy-time.
 *
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_APPYEND
 * lParam  = (not used)
 *
 *      'Appy-time is no longer available.  This message is *NOT* sent
 *      at 'Appy-time.  (It cannot be, because 'Appy-time is gone.)
 *
 * NOTE!  It is possible for DBT_APPYBEGIN and DBT_APPYEND to be sent
 * multiple times during a single Windows session.  Each appearance of
 * 'Appy-time is bracketed by these two messages, but 'Appy-time may
 * momentarily become unavailable during otherwise normal Windows
 * processing.  The current status of 'Appy-time availability can always
 * be obtained from a call to _SHELL_QueryAppyTimeAvailable.
 *)

  DBT_APPYBEGIN = $0000;
  DBT_APPYEND   = $0001;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_DEVNODES_CHANGED
 * lParam  = 0
 *
 *      send when configmg finished a process tree batch. Some devnodes
 *      may have been added or removed. This is used by ring3 people which
 *      need to be refreshed whenever any devnode changed occur (like
 *      device manager). People specific to certain devices should use
 *      DBT_DEVICE* instead.
 *)

  DBT_DEVNODES_CHANGED = $0007;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_QUERYCHANGECONFIG
 * lParam  = 0
 *
 *      sent to ask if a config change is allowed
 *)

  DBT_QUERYCHANGECONFIG = $0017;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_CONFIGCHANGED
 * lParam  = 0
 *
 *      sent when a config has changed
 *)

  DBT_CONFIGCHANGED = $0018;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_CONFIGCHANGECANCELED
 * lParam  = 0
 *
 *      someone cancelled the config change
 *)

  DBT_CONFIGCHANGECANCELED = $0019;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_MONITORCHANGE
 * lParam  = new resolution to use (LOWORD=x, HIWORD=y)
 *           if 0, use the default res for current config
 *
 *      this message is sent when the display monitor has changed
 *      and the system should change the display mode to match it.
 *)

  DBT_MONITORCHANGE = $001B;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_SHELLLOGGEDON
 * lParam  = 0
 *
 *      The shell has finished login on: VxD can now do Shell_EXEC.
 *)

  DBT_SHELLLOGGEDON = $0020;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_CONFIGMGAPI
 * lParam  = CONFIGMG API Packet
 *
 *      CONFIGMG ring 3 call.
 *)

  DBT_CONFIGMGAPI32 = $0022;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_VXDINITCOMPLETE
 * lParam  = 0
 *
 *      CONFIGMG ring 3 call.
 *)

  DBT_VXDINITCOMPLETE = $0023;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_VOLLOCK*
 * lParam  = pointer to VolLockBroadcast structure described below
 *
 *      Messages issued by IFSMGR for volume locking purposes on WM_DEVICECHANGE.
 *      All these messages pass a pointer to a struct which has no pointers.
 *)

  DBT_VOLLOCKQUERYLOCK    = $8041;
  DBT_VOLLOCKLOCKTAKEN    = $8042;
  DBT_VOLLOCKLOCKFAILED   = $8043;
  DBT_VOLLOCKQUERYUNLOCK  = $8044;
  DBT_VOLLOCKLOCKRELEASED = $8045;
  DBT_VOLLOCKUNLOCKFAILED = $8046;

(*
 * Device broadcast header
 *)

type
  _DEV_BROADCAST_HDR = record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
  end;
  DEV_BROADCAST_HDR = _DEV_BROADCAST_HDR;
  PDEV_BROADCAST_HDR = ^DEV_BROADCAST_HDR;
  TDevBroadcastHdr = DEV_BROADCAST_HDR;
  PDevBroadcastHdr = PDEV_BROADCAST_HDR;

(*
 * Structure for volume lock broadcast
 *)

  VolLockBroadcast = record
    vlb_dbh: DEV_BROADCAST_HDR;
    vlb_owner: DWORD;   // thread on which lock request is being issued
    vlb_perms: BYTE;    // lock permission flags defined below
    vlb_lockType: BYTE; // type of lock
    vlb_drive: BYTE;    // drive on which lock is issued
    vlb_flags: BYTE;    // miscellaneous flags
  end;
  TVollockbroadcast = VolLockBroadcast;
  PVollockbroadcast = ^VolLockBroadcast;

(*
 * Values for vlb_perms
 *)

const
  LOCKP_ALLOW_WRITES      = $01; // Bit 0 set - allow writes
  LOCKP_FAIL_WRITES       = $00; // Bit 0 clear - fail writes
  LOCKP_FAIL_MEM_MAPPING  = $02; // Bit 1 set - fail memory mappings
  LOCKP_ALLOW_MEM_MAPPING = $00; // Bit 1 clear - allow memory mappings
  LOCKP_USER_MASK         = $03; // Mask for user lock flags
  LOCKP_LOCK_FOR_FORMAT   = $04; // Level 0 lock for format

(*
 * Values for vlb_flags
 *)

  LOCKF_LOGICAL_LOCK  = $00; // Bit 0 clear - logical lock
  LOCKF_PHYSICAL_LOCK = $01; // Bit 0 set - physical lock

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_NODISKSPACE
 * lParam  = drive number of drive that is out of disk space (1-based)
 *
 * Message issued by IFS manager when it detects that a drive is run out of
 * free space.
 *)

  DBT_NO_DISK_SPACE = $0047;

(*
 * Message = WM_DEVICECHANGE
 * wParam  = DBT_LOW_DISK_SPACE
 * lParam  = drive number of drive that is low on disk space (1-based)
 *
 * Message issued by VFAT when it detects that a drive it has mounted
 * has the remaning free space below a threshold specified by the
 * registry or by a disk space management application.
 * The broadcast is issued by VFAT ONLY when space is either allocated
 * or freed by VFAT.
 *)

  DBT_LOW_DISK_SPACE = $0048;

  DBT_CONFIGMGPRIVATE = $7FFF;

(*
 * The following messages are for WM_DEVICECHANGE. The immediate list
 * is for the wParam. ALL THESE MESSAGES PASS A POINTER TO A STRUCT
 * STARTING WITH A DWORD SIZE AND HAVING NO POINTER IN THE STRUCT.
 *
 *)

  DBT_DEVICEARRIVAL           = $8000; // system detected a new device
  DBT_DEVICEQUERYREMOVE       = $8001; // wants to remove, may fail
  DBT_DEVICEQUERYREMOVEFAILED = $8002; // removal aborted
  DBT_DEVICEREMOVEPENDING     = $8003; // about to remove, still avail.
  DBT_DEVICEREMOVECOMPLETE    = $8004; // device is gone
  DBT_DEVICETYPESPECIFIC      = $8005; // type specific event
  DBT_CUSTOMEVENT             = $8006; // user-defined event

  DBT_DEVTYP_OEM     = $00000000; // oem-defined device type
  DBT_DEVTYP_DEVNODE = $00000001; // devnode number
  DBT_DEVTYP_VOLUME  = $00000002; // logical volume
  DBT_DEVTYP_PORT    = $00000003; // serial, parallel
  DBT_DEVTYP_NET     = $00000004; // network resource

  DBT_DEVTYP_DEVICEINTERFACE = $00000005; // device interface class
  DBT_DEVTYP_HANDLE          = $00000006; // file system handle

type
  _DEV_BROADCAST_HEADER = record
    dbcd_size: DWORD;
    dbcd_devicetype: DWORD;
    dbcd_reserved: DWORD;
  end;
  TDevBroadcastHeader = _DEV_BROADCAST_HEADER;
  PDevBroadcastHeader = ^_DEV_BROADCAST_HEADER;

  PDEV_BROADCAST_OEM = ^DEV_BROADCAST_OEM;
  _DEV_BROADCAST_OEM = record
    dbco_size: DWORD;
    dbco_devicetype: DWORD;
    dbco_reserved: DWORD;
    dbco_identifier: DWORD;
    dbco_suppfunc: DWORD;
  end;
  DEV_BROADCAST_OEM = _DEV_BROADCAST_OEM;
  TDevBroadcastOem = DEV_BROADCAST_OEM;
  PDevBroadcastOem = PDEV_BROADCAST_OEM;

  PDEV_BROADCAST_DEVNODE = ^DEV_BROADCAST_DEVNODE;
  _DEV_BROADCAST_DEVNODE = record
    dbcd_size: DWORD;
    dbcd_devicetype: DWORD;
    dbcd_reserved: DWORD;
    dbcd_devnode: DWORD;
  end;
  DEV_BROADCAST_DEVNODE = _DEV_BROADCAST_DEVNODE;
  TDevBroadcastDevNode = DEV_BROADCAST_DEVNODE;
  PDevBroadcastDevNode = PDEV_BROADCAST_DEVNODE;

  PDEV_BROADCAST_VOLUME = ^DEV_BROADCAST_VOLUME;
  _DEV_BROADCAST_VOLUME = record
    dbcv_size: DWORD;
    dbcv_devicetype: DWORD;
    dbcv_reserved: DWORD;
    dbcv_unitmask: DWORD;
    dbcv_flags: WORD;
  end;
  DEV_BROADCAST_VOLUME = _DEV_BROADCAST_VOLUME;
  TDevBroadcastVolume = DEV_BROADCAST_VOLUME;
  PDevBroadcastVolume = PDEV_BROADCAST_VOLUME;

const
  DBTF_MEDIA = $0001; // media comings and goings
  DBTF_NET   = $0002; // network volume

type
  PDEV_BROADCAST_PORT_A = ^DEV_BROADCAST_PORT_A;
  _DEV_BROADCAST_PORT_A = record
    dbcp_size: DWORD;
    dbcp_devicetype: DWORD;
    dbcp_reserved: DWORD;
    dbcp_name: array [0..0] of Char;
  end;
  DEV_BROADCAST_PORT_A = _DEV_BROADCAST_PORT_A;
  TDevBroadcastPortA = DEV_BROADCAST_PORT_A;
  PDevBroadcastPortA = PDEV_BROADCAST_PORT_A;

  PDEV_BROADCAST_PORT_W = ^DEV_BROADCAST_PORT_W;
  _DEV_BROADCAST_PORT_W = record
    dbcp_size: DWORD;
    dbcp_devicetype: DWORD;
    dbcp_reserved: DWORD;
    dbcp_name: array [0..0] of WideChar;
  end;
  DEV_BROADCAST_PORT_W = _DEV_BROADCAST_PORT_W;
  TDevBroadcastPortW = DEV_BROADCAST_PORT_W;
  PDevBroadcastPortW = PDEV_BROADCAST_PORT_W;

{$IFDEF UNICODE}
  DEV_BROADCAST_PORT = DEV_BROADCAST_PORT_W;
  PDEV_BROADCAST_PORT = PDEV_BROADCAST_PORT_W;
  TDevBroadcastPort = TDevBroadcastPortW;
  PDevBroadcastPort = PDevBroadcastPortW;
{$ELSE}
  DEV_BROADCAST_PORT = DEV_BROADCAST_PORT_A;
  PDEV_BROADCAST_PORT = PDEV_BROADCAST_PORT_A;
  TDevBroadcastPort = TDevBroadcastPortA;
  PDevBroadcastPort = PDevBroadcastPortA;
{$ENDIF}

  PDEV_BROADCAST_NET = ^DEV_BROADCAST_NET;
  _DEV_BROADCAST_NET = record
    dbcn_size: DWORD;
    dbcn_devicetype: DWORD;
    dbcn_reserved: DWORD;
    dbcn_resource: DWORD;
    dbcn_flags: DWORD;
  end;
  DEV_BROADCAST_NET = _DEV_BROADCAST_NET;
  TDevBroadcastNet = DEV_BROADCAST_NET;
  PDevBroadcastNet = PDEV_BROADCAST_NET;

  PDEV_BROADCAST_DEVICEINTERFACE_A = ^DEV_BROADCAST_DEVICEINTERFACE_A;
  _DEV_BROADCAST_DEVICEINTERFACE_A = record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: GUID;
    dbcc_name: array [0..0] of char;
  end;
  DEV_BROADCAST_DEVICEINTERFACE_A = _DEV_BROADCAST_DEVICEINTERFACE_A;
  TDevBroadcastDeviceInterfaceA = DEV_BROADCAST_DEVICEINTERFACE_A;
  PDevBroadcastDeviceInterfaceA = PDEV_BROADCAST_DEVICEINTERFACE_A;

  PDEV_BROADCAST_DEVICEINTERFACE_W = ^DEV_BROADCAST_DEVICEINTERFACE_W;
  _DEV_BROADCAST_DEVICEINTERFACE_W = record
    dbcc_size: DWORD;
    dbcc_devicetype: DWORD;
    dbcc_reserved: DWORD;
    dbcc_classguid: GUID;
    dbcc_name: array [0..0] of wchar_t;
  end;
  DEV_BROADCAST_DEVICEINTERFACE_W = _DEV_BROADCAST_DEVICEINTERFACE_W;
  TDevBroadcastDeviceInterfaceW = DEV_BROADCAST_DEVICEINTERFACE_W;
  PDevBroadcastDeviceInterfaceW = PDEV_BROADCAST_DEVICEINTERFACE_W;

{$IFDEF UNICODE}
  DEV_BROADCAST_DEVICEINTERFACE = DEV_BROADCAST_DEVICEINTERFACE_W;
  PDEV_BROADCAST_DEVICEINTERFACE = PDEV_BROADCAST_DEVICEINTERFACE_W;
  TDevBroadcastDeviceInterface = TDevBroadcastDeviceInterfaceW;
  PDevBroadcastDeviceInterface = PDevBroadcastDeviceInterfaceW;
{$ELSE}
  DEV_BROADCAST_DEVICEINTERFACE = DEV_BROADCAST_DEVICEINTERFACE_A;
  PDEV_BROADCAST_DEVICEINTERFACE = PDEV_BROADCAST_DEVICEINTERFACE_A;
  TDevBroadcastDeviceInterface = TDevBroadcastDeviceInterfaceA;
  PDevBroadcastDeviceInterface = PDevBroadcastDeviceInterfaceA;
{$ENDIF}

  PDEV_BROADCAST_HANDLE = ^DEV_BROADCAST_HANDLE;
  _DEV_BROADCAST_HANDLE = record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
    dbch_handle: HANDLE;         // file handle used in call to RegisterDeviceNotification
    dbch_hdevnotify: HDEVNOTIFY; // returned from RegisterDeviceNotification
    //
    // The following 3 fields are only valid if wParam is DBT_CUSTOMEVENT.
    //
    dbch_eventguid: GUID;
    dbch_nameoffset: LONG;           // offset (bytes) of variable-length string buffer (-1 if none)
    dbch_data: array [0..0] of BYTE; // variable-sized buffer, potentially containing binary and/or text data
  end;
  DEV_BROADCAST_HANDLE = _DEV_BROADCAST_HANDLE;
  TDevBroadcastHandle = DEV_BROADCAST_HANDLE;
  PDevBroadcastHandle = PDEV_BROADCAST_HANDLE;

//
// Define 32-bit and 64-bit versions of the DEV_BROADCAST_HANDLE structure
// for WOW64.  These must be kept in sync with the above structure.
//

  PDEV_BROADCAST_HANDLE32 = ^DEV_BROADCAST_HANDLE32;
  _DEV_BROADCAST_HANDLE32 = record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
    dbch_handle: ULONG32;
    dbch_hdevnotify: ULONG32;
    dbch_eventguid: GUID;
    dbch_nameoffset: LONG;
    dbch_data: array [0..0] of BYTE;
  end;
  DEV_BROADCAST_HANDLE32 = _DEV_BROADCAST_HANDLE32;
  TDevBroadcastHandle32 = DEV_BROADCAST_HANDLE32;
  PDevBroadcastHandle32 = PDEV_BROADCAST_HANDLE32;

  PDEV_BROADCAST_HANDLE64 = ^DEV_BROADCAST_HANDLE64;
  _DEV_BROADCAST_HANDLE64 = record
    dbch_size: DWORD;
    dbch_devicetype: DWORD;
    dbch_reserved: DWORD;
    dbch_handle: ULONG64;
    dbch_hdevnotify: ULONG64;
    dbch_eventguid: GUID;
    dbch_nameoffset: LONG;
    dbch_data: array [0..0] of BYTE;
  end;
  DEV_BROADCAST_HANDLE64 = _DEV_BROADCAST_HANDLE64;
  TDevBroadcastHandle64 = DEV_BROADCAST_HANDLE64;
  PDevBroadcastHandle64 = PDEV_BROADCAST_HANDLE64;

const
  DBTF_RESOURCE = $00000001; // network resource
  DBTF_XPORT    = $00000002; // new transport coming or going
  DBTF_SLOWNET  = $00000004; // new incoming transport is slow
                             // (dbcn_resource undefined for now)

  DBT_VPOWERDAPI = $8100; // VPOWERD API for Win95

(*
 *  User-defined message types all use wParam = 0xFFFF with the
 *  lParam a pointer to the structure below.
 *
 *  dbud_dbh - DEV_BROADCAST_HEADER must be filled in as usual.
 *
 *  dbud_szName contains a case-sensitive ASCIIZ name which names the
 *  message.  The message name consists of the vendor name, a backslash,
 *  then arbitrary user-defined ASCIIZ text.  For example:
 *
 *      "WidgetWare\QueryScannerShutdown"
 *      "WidgetWare\Video Q39S\AdapterReady"
 *
 *  After the ASCIIZ name, arbitrary information may be provided.
 *  Make sure that dbud_dbh.dbch_size is big enough to encompass
 *  all the data.  And remember that nothing in the structure may
 *  contain pointers.
 *)

  DBT_USERDEFINED = $FFFF;

type
  PDEV_BROADCAST_USERDEFINED = ^DEV_BROADCAST_USERDEFINED;
  _DEV_BROADCAST_USERDEFINED = record
    dbud_dbh: DEV_BROADCAST_HDR;
    dbud_szName: array [0..0] of Char;
    //  BYTE        dbud_rgbUserDefined[];*/ /* User-defined contents */
  end;
  DEV_BROADCAST_USERDEFINED = _DEV_BROADCAST_USERDEFINED;
  TDevBroadcastUserDefined = DEV_BROADCAST_USERDEFINED;
  PDevBroadcastUserDefined = PDEV_BROADCAST_USERDEFINED;

implementation

end.
