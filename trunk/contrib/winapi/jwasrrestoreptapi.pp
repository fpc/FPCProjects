{******************************************************************************}
{                                                       	               }
{ Windows System Restore API interface Unit for Object Pascal                  }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: srrestoreptapi.h, released Match 2003. The original    }
{ Pascal code is: SrRestorePtApi.pas, released December 2000.                  }
{ The initial developer of the Pascal code is Marcel van Brakel                }
{ (brakelm@chello.nl).                                                         }
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

unit JwaSrRestorePtApi;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "SrRestorePtApi.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinNT, JwaWinType;

interface

//
// Type of Event
//

const
  MIN_EVENT               	   = 100;
  BEGIN_SYSTEM_CHANGE     	   = 100;
  END_SYSTEM_CHANGE       	   = 101;
  BEGIN_NESTED_SYSTEM_CHANGE       = 102; // for Whistler only - use this to prevent nested restore pts
  END_NESTED_SYSTEM_CHANGE         = 103; // for Whistler only - use this to prevent nested restore pts
  MAX_EVENT               	   = 103;

//
// Type of Restore Points
//

  MIN_RPT                = 0;
  APPLICATION_INSTALL    = 0;
  APPLICATION_UNINSTALL  = 1;
  DESKTOP_SETTING        = 2;    // Not implemented
  ACCESSIBILITY_SETTING  = 3;    // Not implemented
  OE_SETTING             = 4;    // Not implemented
  APPLICATION_RUN        = 5;    // Not implemented
  RESTORE                = 6;
  CHECKPOINT             = 7;
  WINDOWS_SHUTDOWN       = 8;    // Not implemented
  WINDOWS_BOOT           = 9;    // Not implemented
  DEVICE_DRIVER_INSTALL  = 10;
  FIRSTRUN               = 11;
  MODIFY_SETTINGS        = 12;
  CANCELLED_OPERATION    = 13;   // Only valid for END_SYSTEM_CHANGE
  BACKUP_RECOVERY	 = 14;
  MAX_RPT                = 14;

  MAX_DESC               = 64;
  MAX_DESC_W		 = 256;  // longer for Whistler

//
// for Millennium compatibility
//

//#pragma pack(push, srrestoreptapi_include)
//#pragma pack(1)

//
// Restore point information
//

type
  _RESTOREPTINFOA = packed record
    dwEventType: DWORD;                // Type of Event - Begin or End
    dwRestorePtType: DWORD;            // Type of Restore Point - App install/uninstall
    llSequenceNumber: Int64;           // Sequence Number - 0 for begin
    szDescription: array [0..MAX_DESC - 1] of Char;    // Description - Name of Application / Operation
  end;
  RESTOREPOINTINFOA = _RESTOREPTINFOA;
  PRESTOREPOINTINFOA = ^RESTOREPOINTINFOA;
  TRestorePointInfoA = RESTOREPOINTINFOA;

  _RESTOREPTINFOW = packed record
    dwEventType: DWORD;                // Type of Event - Begin or End
    dwRestorePtType: DWORD;            // Type of Restore Point - App install/uninstall
    llSequenceNumber: Int64;           // Sequence Number - 0 for begin
    szDescription: array [0..MAX_DESCW - 1] of WideChar;    // Description - Name of Application / Operation
  end;
  RESTOREPOINTINFOW = _RESTOREPTINFOW;
  PRESTOREPOINTINFOW = ^RESTOREPOINTINFOW;
  TRestorePointInfoW = RESTOREPOINTINFOW;

//
// Status returned by System Restore
//

  _SMGRSTATUS =  packed record
    nStatus: DWORD;            // Status returned by State Manager Process
    llSequenceNumber: Int64;   // Sequence Number for the restore point
  end;
  STATEMGRSTATUS = _STATEMGRSTATUS;
  PSTATEMGRSTATUS = ^STATEMGRSTATUS;
  TSMgrStatus = STATEMGRSTATUS;

//#pragma pack(pop, srrestoreptapi_include)

//
// RPC call to set a restore point
//
// Return value  TRUE if the call was a success
//               FALSE if the call failed
//
// If pSmgrStatus nStatus field is set as follows
//
// ERROR_SUCCESS              If the call succeeded (return value will be TRUE)
//
// ERROR_TIMEOUT              If the call timed out due to a wait on a mutex for
//                            for setting restore points.
//
// ERROR_INVALID_DATA         If the cancel restore point is called with an invalid
//                            sequence number
//
// ERROR_INTERNAL_ERROR       If there are internal failures.
//
// ERROR_BAD_ENVIRONMENT      If the API is called in SafeMode
//
// ERROR_SERVICE_DISABLED     If SystemRestore is Disabled.
//
// ERROR_DISK_FULL 			  If System Restore is frozen (Windows Whistler only)
//
// ERROR_ALREADY_EXISTS       If this is a nested restore point

function SRSetRestorePointA(pRestorePtSpec: PRESTOREPOINTINFOA; pSMgrStatus: PSTATEMGRSTATUS): BOOL stdcall;

function SRSetRestorePointW(pRestorePtSpec: PRESTOREPOINTINFOW; pSMgrStatus: PSTATEMGRSTATUS): BOOL stdcall;

function SRRemoveRestorePoint(dwRPNum: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}

type
  RESTOREPOINTINFO = RESTOREPOINTINFOW;
  PRESTOREPOINTINFO = PRESTOREPOINTINFOW;
  TRestorePointInfo = TRestorePointInfoW;

function SRSetRestorePoint(pRestorePtSpec: PRESTOREPOINTINFOA; pSMgrStatus: PSTATEMGRSTATUS): BOOL stdcall;

{$ELSE}

type
  RESTOREPOINTINFO = RESTOREPOINTINFOW;
  PRESTOREPOINTINFO = PRESTOREPOINTINFOW;
  TRestorePointInfo = TRestorePointInfoW;

function SRSetRestorePoint(pRestorePtSpec: PRESTOREPOINTINFOW; pSMgrStatus: PSTATEMGRSTATUS): BOOL stdcall;

{$ENDIF}

implementation

const
  srclient = 'srclient.dll';

function SRSetRestorePointA; external srclient name 'SRSetRestorePointA';
function SRSetRestorePointW; external srclient name 'SRSetRestorePointW';
function SRRemoveRestorePoint; external srclient name 'SRRemoveRestorePoint';
{$IFDEF UNICODE}
function SRSetRestorePoint; external srclient name 'SRSetRestorePointW';
{$ELSE}
function SRSetRestorePoint; external srclient name 'SRSetRestorePointA';
{$ENDIF}

end.
