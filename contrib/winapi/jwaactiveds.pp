{******************************************************************************}
{                                                       	               }
{ Active Directory Services API interface Unit for Object Pascal               }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: activeds.h, released June 2000. The original Pascal    }
{ code is: ActiveDS.pas, released December 2000. The initial developer of the  }
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
{ For more information about the LGPL: http://www.gn.org/copyleft/lesser.html }
{ 						    u		               }
{******************************************************************************}

unit JwaActiveDS;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "activeds.h"'}
{$HPPEMIT ''}
{$HPPEMIT 'typedef GUID REFIID'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  ActiveX {TODO}, JwaAdsTLB, JwaWinNT, JwaWinType, JwaWinUser;

type
  REFIID = GUID;
  {$NODEFINE REFIID}

type
  // imports of a type library sometimes are missing a few decls, these are just
  // a few of them to make this file compile at all. I really should do all of
  // them one day.

  PADSVALUE = ^_adsvalue;
  PADS_ATTR_INFO = ^_ads_attr_info;

//  Contents:   Master include file for Ole Ds
//
//  Notes:      All Ole Ds client applications must include this file. This
//              provides access to the primary Ole Ds interfaces, the error
//              codes, and function prototypes for the Ole Ds helper apis.

//
// Interface definitions and well known GUIDS for Ole Ds
//

//#include "iads.h"  >> AdsTLB from activeds.dll

//
// Helper function prototypes for Ole Ds
//

//#include "adshlp.h"

function ADsGetObject(lpszPathName: LPCWSTR; const riid: REFIID; out ppObject: Pointer): HRESULT; stdcall;

function ADsBuildEnumerator(pADsContainer: IADsContainer; out ppEnumVariant: IEnumVARIANT): HRESULT; stdcall;

function ADsFreeEnumerator(var pEnumVariant: IEnumVARIANT): HRESULT;

function ADsEnumerateNext(pEnumVariant: IEnumVARIANT; cElements: ULONG;
  var pvar: OleVariant; var pcElementsFetched: ULONG): HRESULT; stdcall;

function ADsBuildVarArrayStr(lppPathNames: LPWSTR; dwPathNames: DWORD;
  var pVar: OleVariant): HRESULT; stdcall;

function ADsBuildVarArrayInt(lpdwObjectTypes: LPDWORD; dwObjectTypes: DWORD;
  var pVar: OleVariant): HRESULT; stdcall;

function ADsOpenObject(lpszPathName, lpszUserName, lpszPassword: LPCWSTR;
  dwReserved: DWORD; const riid: REFIID; out ppObject: Pointer): HRESULT; stdcall;

//
// Helper functions for extended error support
//

function ADsGetLastError(var lpError: DWORD; lpErrorBuf: LPWSTR;
  dwErrorBufLen: DWORD; lpNameBuf: LPWSTR; dwNameBufLen: DWORD): HRESULT; stdcall;

procedure ADsSetLastError(dwErr: DWORD; pszError, pszProvider: LPCWSTR); stdcall;

//procedure ADsFreeAllErrorRecords; stdcall;
//{$EXTERNALSYM ADsFreeAllErrorRecords}

function AllocADsMem(cb: DWORD): LPVOID; stdcall;

function FreeADsMem(pMem: LPVOID): BOOL; stdcall;

function ReallocADsMem(pOldMem: LPVOID; cbOld, cbNew: DWORD): LPVOID; stdcall;

function AllocADsStr(pStr: LPCWSTR): LPWSTR; stdcall;

function FreeADsStr(pStr: LPWSTR): BOOL; stdcall;

function ReallocADsStr(var ppStr: LPWSTR; pStr: LPWSTR): BOOL; stdcall;

function ADsEncodeBinaryData(pbSrcData: PBYTE; dwSrcLen: DWORD;
  var ppszDestData: LPWSTR): HRESULT; stdcall;

function ADsDecodeBinaryData(szSrcData: LPCWSTR; var ppbDestData: PBYTE;
  var pdwDestLen: ULONG): HRESULT; stdcall;

function PropVariantToAdsType(var pVariant: OleVariant; dwNumVariant: DWORD;
  var ppAdsValues: PADSVALUE; pdwNumValues: PDWORD): HRESULT; stdcall;

function AdsTypeToPropVariant(pAdsValues: PADSVALUE; dwNumValues: DWORD;
  var pVariant: OleVariant): HRESULT; stdcall;

procedure AdsFreeAdsValues(pAdsValues: PADSVALUE; dwNumValues: DWORD); stdcall;

//
// Error codes for Ole Ds - generated from ..\..\errmsg
//

//#include "adserr.h"

// ---------------------- HRESULT value definitions -----------------
//
// HRESULT definitions
//

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

const
  FACILITY_WINDOWS  = 8;
  FACILITY_STORAGE  = 3;
  FACILITY_RPC      = 1;
  FACILITY_SSPI     = 9;
  FACILITY_WIN32    = 7;
  FACILITY_CONTROL  = 10;
  FACILITY_NULL     = 0;
  FACILITY_ITF      = 4;
  FACILITY_DISPATCH = 2;

//
// Define the severity codes
//


//
// MessageId: E_ADS_BAD_PATHNAME
//
// MessageText:
//
//  An invalid Active Directory pathname was passed
//

  E_ADS_BAD_PATHNAME               = HRESULT($80005000);

//
// MessageId: E_ADS_INVALID_DOMAIN_OBJECT
//
// MessageText:
//
//  An unknown Active Directory domain object was requested
//

  E_ADS_INVALID_DOMAIN_OBJECT      = HRESULT($80005001);

//
// MessageId: E_ADS_INVALID_USER_OBJECT
//
// MessageText:
//
//  An unknown Active Directory user object was requested
//

  E_ADS_INVALID_USER_OBJECT        = HRESULT($80005002);

//
// MessageId: E_ADS_INVALID_COMPUTER_OBJECT
//
// MessageText:
//
//  An unknown Active Directory computer object was requested
//

  E_ADS_INVALID_COMPUTER_OBJECT    = HRESULT($80005003);

//
// MessageId: E_ADS_UNKNOWN_OBJECT
//
// MessageText:
//
//  An unknown Active Directory object was requested
//

  E_ADS_UNKNOWN_OBJECT             = HRESULT($80005004);

//
// MessageId: E_ADS_PROPERTY_NOT_SET
//
// MessageText:
//
//  The specified Active Directory property was not set
//

  E_ADS_PROPERTY_NOT_SET           = HRESULT($80005005);

//
// MessageId: E_ADS_PROPERTY_NOT_SUPPORTED
//
// MessageText:
//
//  The specified Active Directory property is not supported
//

  E_ADS_PROPERTY_NOT_SUPPORTED     = HRESULT($80005006);

//
// MessageId: E_ADS_PROPERTY_INVALID
//
// MessageText:
//
//  The specified Active Directory property is invalid
//

  E_ADS_PROPERTY_INVALID           = HRESULT($80005007);

//
// MessageId: E_ADS_BAD_PARAMETER
//
// MessageText:
//
//  One or more input parameters are invalid
//

  E_ADS_BAD_PARAMETER              = HRESULT($80005008);

//
// MessageId: E_ADS_OBJECT_UNBOUND
//
// MessageText:
//
//  The specified Active Directory object is not bound to a remote resource
//

  E_ADS_OBJECT_UNBOUND             = HRESULT($80005009);

//
// MessageId: E_ADS_PROPERTY_NOT_MODIFIED
//
// MessageText:
//
//  The specified Active Directory object has not been modified
//

  E_ADS_PROPERTY_NOT_MODIFIED      = HRESULT($8000500A);

//
// MessageId: E_ADS_PROPERTY_MODIFIED
//
// MessageText:
//
//  The specified Active Directory object has not been modified
//

  E_ADS_PROPERTY_MODIFIED          = HRESULT($8000500B);

//
// MessageId: E_ADS_CANT_CONVERT_DATATYPE
//
// MessageText:
//
//  The Active Directory datatype cannot be converted to/from a native DS datatype
//

  E_ADS_CANT_CONVERT_DATATYPE      = HRESULT($8000500C);

//
// MessageId: E_ADS_PROPERTY_NOT_FOUND
//
// MessageText:
//
//  The Active Directory property cannot be found in the cache.
//

  E_ADS_PROPERTY_NOT_FOUND         = HRESULT($8000500D);

//
// MessageId: E_ADS_OBJECT_EXISTS
//
// MessageText:
//
//  The Active Directory object exists.
//

  E_ADS_OBJECT_EXISTS              = HRESULT($8000500E);

//
// MessageId: E_ADS_SCHEMA_VIOLATION
//
// MessageText:
//
//  The attempted action violates the DS schema rules.
//

  E_ADS_SCHEMA_VIOLATION           = HRESULT($8000500F);

//
// MessageId: E_ADS_COLUMN_NOT_SET
//
// MessageText:
//
//  The specified column in the Active Directory was not set.
//

  E_ADS_COLUMN_NOT_SET             = HRESULT($80005010);

//
// MessageId: S_ADS_ERRORSOCCURRED
//
// MessageText:
//
//  One or more errors occurred
//

  S_ADS_ERRORSOCCURRED             = HRESULT($00005011);

//
// MessageId: S_ADS_NOMORE_ROWS
//
// MessageText:
//
//  No more rows to be obatained by the search result.
//

  S_ADS_NOMORE_ROWS                = HRESULT($00005012);

//
// MessageId: S_ADS_NOMORE_COLUMNS
//
// MessageText:
//
//  No more columns to be obatained for the current row.
//

  S_ADS_NOMORE_COLUMNS             = HRESULT($00005013);

//
// MessageId: E_ADS_INVALID_FILTER
//
// MessageText:
//
//  The search filter specified is invalid
//

  E_ADS_INVALID_FILTER             = HRESULT($80005014);

//
// Globally accessible GUIDS
//

//#include "adsiid.h" -> adstlb from activeds.dll

//
// Status codes for ads objects
//

//#include "adssts.h"

const
  ADS_PRINTER_PAUSED            = $00000001;
  ADS_PRINTER_PENDING_DELETION  = $00000002;
  ADS_PRINTER_ERROR             = $00000003;
  ADS_PRINTER_PAPER_JAM         = $00000004;
  ADS_PRINTER_PAPER_OUT         = $00000005;
  ADS_PRINTER_MANUAL_FEED       = $00000006;
  ADS_PRINTER_PAPER_PROBLEM     = $00000007;
  ADS_PRINTER_OFFLINE           = $00000008;
  ADS_PRINTER_IO_ACTIVE         = $00000100;
  ADS_PRINTER_BUSY              = $00000200;
  ADS_PRINTER_PRINTING          = $00000400;
  ADS_PRINTER_OUTPUT_BIN_FULL   = $00000800;
  ADS_PRINTER_NOT_AVAILABLE     = $00001000;
  ADS_PRINTER_WAITING           = $00002000;
  ADS_PRINTER_PROCESSING        = $00004000;
  ADS_PRINTER_INITIALIZING      = $00008000;
  ADS_PRINTER_WARMING_UP        = $00010000;
  ADS_PRINTER_TONER_LOW         = $00020000;
  ADS_PRINTER_NO_TONER          = $00040000;
  ADS_PRINTER_PAGE_PUNT         = $00080000;
  ADS_PRINTER_USER_INTERVENTION = $00100000;
  ADS_PRINTER_OUT_OF_MEMORY     = $00200000;
  ADS_PRINTER_DOOR_OPEN         = $00400000;
  ADS_PRINTER_SERVER_UNKNOWN    = $00800000;
  ADS_PRINTER_POWER_SAVE        = $01000000;

//
// job status values
//

  ADS_JOB_PAUSED   = $00000001;
  ADS_JOB_ERROR    = $00000002;
  ADS_JOB_DELETING = $00000004;
  ADS_JOB_SPOOLING = $00000008;
  ADS_JOB_PRINTING = $00000010;
  ADS_JOB_OFFLINE  = $00000020;
  ADS_JOB_PAPEROUT = $00000040;
  ADS_JOB_PRINTED  = $00000080;
  ADS_JOB_DELETED  = $00000100;

//
// service status values
//

  ADS_SERVICE_STOPPED          = $00000001;
  ADS_SERVICE_START_PENDING	   = $00000002;
  ADS_SERVICE_STOP_PENDING     = $00000003;
  ADS_SERVICE_RUNNING	         = $00000004;
  ADS_SERVICE_CONTINUE_PENDING = $00000005;
  ADS_SERVICE_PAUSE_PENDING	   = $00000006;
  ADS_SERVICE_PAUSED           = $00000007;
  ADS_SERVICE_ERROR            = $00000008;

//---------------------------------------------------------------------

//
// Service Type Valid Values
//

  ADS_SERVICE_OWN_PROCESS        = $00000010;
  ADS_SERVICE_SHARE_PROCESS      = $00000020;
  ADS_SERVICE_KERNEL_DRIVER      = $00000001;
  ADS_SERVICE_FILE_SYSTEM_DRIVER = $00000002;

//
// Start Type Valid Values
//

  ADS_SERVICE_BOOT_START   = SERVICE_BOOT_START;
  ADS_SERVICE_SYSTEM_START = SERVICE_SYSTEM_START;
  ADS_SERVICE_AUTO_START   = SERVICE_AUTO_START;
  ADS_SERVICE_DEMAND_START = SERVICE_DEMAND_START;
  ADS_SERVICE_DISABLED     = SERVICE_DISABLED;

//
// Error Control Values
//

  ADS_SERVICE_ERROR_IGNORE   = 0;
  ADS_SERVICE_ERROR_NORMAL   = 1;
  ADS_SERVICE_ERROR_SEVERE   = 2;
  ADS_SERVICE_ERROR_CRITICAL = 3;

//
// Schema class names and other schema related definitions
//

//#include "adsnms.h"

const
  NAMESPACE_CLASS_NAME        = 'Namespace';
  COUNTRY_CLASS_NAME          = 'Country';
  LOCALITY_CLASS_NAME         = 'Locality';
  ORGANIZATION_CLASS_NAME     = 'Organization';
  ORGANIZATIONUNIT_CLASS_NAME = 'Organizational Unit';
  DOMAIN_CLASS_NAME           = 'Domain';
  COMPUTER_CLASS_NAME         = 'Computer';
  USER_CLASS_NAME             = 'User';
  GROUP_CLASS_NAME            = 'Group';
  GLOBALGROUP_CLASS_NAME      = 'GlobalGroup';
  LOCALGROUP_CLASS_NAME       = 'LocalGroup';
  SERVICE_CLASS_NAME          = 'Service';
  FILESERVICE_CLASS_NAME      = 'FileService';
  SESSION_CLASS_NAME          = 'Session';
  RESOURCE_CLASS_NAME         = 'Resource';
  FILESHARE_CLASS_NAME        = 'FileShare';
  PRINTER_CLASS_NAME          = 'PrintQueue';
  PRINTJOB_CLASS_NAME         = 'PrintJob';
  SCHEMA_CLASS_NAME           = 'Schema';
  CLASS_CLASS_NAME            = 'Class';
  PROPERTY_CLASS_NAME         = 'Property';
  SYNTAX_CLASS_NAME           = 'Syntax';
  ROOTDSE_CLASS_NAME          = 'RootDSE';

  NO_SCHEMA                    = '';
  DOMAIN_SCHEMA_NAME           = 'Domain';
  COMPUTER_SCHEMA_NAME         = 'Computer';
  USER_SCHEMA_NAME             = 'User';
  GROUP_SCHEMA_NAME            = 'Group';
  GLOBALGROUP_SCHEMA_NAME      = 'GlobalGroup';
  LOCALGROUP_SCHEMA_NAME       = 'LocalGroup';
  SERVICE_SCHEMA_NAME          = 'Service';
  PRINTER_SCHEMA_NAME          = 'PrintQueue';
  PRINTJOB_SCHEMA_NAME         = 'PrintJob';
  FILESERVICE_SCHEMA_NAME      = 'FileService';
  SESSION_SCHEMA_NAME          = 'Session';
  RESOURCE_SCHEMA_NAME         = 'Resource';
  FILESHARE_SCHEMA_NAME        = 'FileShare';
  FPNW_FILESERVICE_SCHEMA_NAME = 'FPNWFileService';
  FPNW_SESSION_SCHEMA_NAME     = 'FPNWSession';
  FPNW_RESOURCE_SCHEMA_NAME    = 'FPNWResource';
  FPNW_FILESHARE_SCHEMA_NAME   = 'FPNWFileShare';

//
// Definitions in the OLE DB provider for ADSI
//

//#include "adsdb.h"

//
// printer status values
//

// Most of the constants have been moved into an enum in adstype.h and
// are available publicly in iads.h. This file has been left here so that
// old references to adsdb.h do not break compiles.

const
  DBPROPFLAGS_ADSISEARCH        = $0000C000;

//#include "adsprop.h"

//  Windows NT Active Directory Service Property Pages
//
//  Contents:   Functions and definitions used in the creation of AD property
//              sheets.

const
  WM_ADSPROP_NOTIFY_PAGEINIT   = (WM_USER + 1101); // where LPARAM is the PADSPROPINITPARAMS pointer.
  WM_ADSPROP_NOTIFY_PAGEHWND   = (WM_USER + 1102); // where WPARAM => page's HWND
  WM_ADSPROP_NOTIFY_CHANGE     = (WM_USER + 1103); // used to send a change notification to a parent sheet
  WM_ADSPROP_NOTIFY_APPLY      = (WM_USER + 1104); // pages send this to the notification object.
  WM_ADSPROP_NOTIFY_SETFOCUS   = (WM_USER + 1105); // used internally by the notification object.
  WM_ADSPROP_NOTIFY_FOREGROUND = (WM_USER + 1106); // used internally by the notification object.
  WM_ADSPROP_NOTIFY_EXIT       = (WM_USER + 1107); // sent on page release

//+----------------------------------------------------------------------------
//
//  Structure:  ADSPROPINITPARAMS
//
//  Usage:      Used to pass page initialization information to new pages from
//              the notify object.
//
//-----------------------------------------------------------------------------

type
  PADSPROPINITPARAMS = ^ADSPROPINITPARAMS;
  _ADSPROPINITPARAMS = record
    dwSize: DWORD;            // Set this to the size of the struct.
    dwFlags: DWORD;           // Reserved for future use.
    hr: HRESULT;              // If this is non-zero, then the others
    pDsObj: IDirectoryObject; // should be ignored.
    pwzCN: LPWSTR;
    pWritableAttrs: PADS_ATTR_INFO;
  end;
  ADSPROPINITPARAMS = _ADSPROPINITPARAMS;
  TAdsPropInitParams = ADSPROPINITPARAMS;

//+----------------------------------------------------------------------------
//
//  Function:   ADsPropCreateNotifyObj
//
//  Synopsis:   Checks to see if the notification window/object exists for this
//              sheet instance and if not creates it.
//
//  Arguments:  [pAppThdDataObj] - the unmarshalled data object pointer.
//              [pwzADsObjName]  - object path name.
//              [phNotifyObj]    - to return the notificion window handle.
//
//  Returns:    HRESULTs.
//
//-----------------------------------------------------------------------------

function ADsPropCreateNotifyObj(pAppThdDataObj: Pointer; {LPDATAOBJECT}
  pwzADsObjName: PWSTR; var phNotifyObj: HWND): HRESULT; stdcall;

//+----------------------------------------------------------------------------
//
//  Function:   ADsPropGetInitInfo
//
//  Synopsis:   Pages call this at their init time to retreive DS object info.
//
//  Arguments:  [hNotifyObj]  - the notificion window handle.
//              [pInitParams] - struct filled in with DS object info. This
//                              struct must be allocated by the caller before
//                              the call.
//
//  Returns:    FALSE if the notify window has gone away for some reason or
//              if the parameters are invalid.
//
//  Notes:      This call results in the sending of the
//              WM_ADSPROP_NOTIFY_PAGEINIT message to the notify window.
//              pInitParams->pWritableAttrs can be NULL if there are no
//              writable attributes.
//
//-----------------------------------------------------------------------------

function ADsPropGetInitInfo(hNotifyObj: HWND; pInitParams: PADSPROPINITPARAMS): BOOL; stdcall;

//+----------------------------------------------------------------------------
//
//  Function:   ADsPropSetHwnd
//
//  Synopsis:   Pages call this at their dialog init time to send their hwnd.
//
//  Arguments:  [hNotifyObj]  - the notificion window handle.
//              [hPage]       - the page's window handle.
//
//  Returns:    FALSE if the notify window has gone away for some reason.
//
//  Notes:      Sends the WM_ADSPROP_NOTIFY_PAGEHWND message to the notify
//              window.
//
//-----------------------------------------------------------------------------

function ADsPropSetHwnd(hNotifyObj: HWND; hPage: HWND): BOOL; stdcall;

//+----------------------------------------------------------------------------
//
//  function:   ADsPropCheckIfWritable
//
//  Synopsis:   See if the attribute is writable by checking if it is in
//              the allowedAttributesEffective array.
//
//  Arguments:  [pwzAttr]        - the attribute name.
//              [pWritableAttrs] - the array of writable attributes.
//
//  Returns:    FALSE if the attribute name is not found in the writable-attrs
//              array or if the array pointer is NULL.
//
//-----------------------------------------------------------------------------

function ADsPropCheckIfWritable(pwzAttr: PWSTR; pWritableAttrs: PADS_ATTR_INFO): BOOL; stdcall;

implementation

const
  adslib = 'activeds.dll';
  dsprop = 'dsprop.dll';

// adshlp.h


{$IFDEF DYNAMIC_LINK}
var
  _ADsGetObject: Pointer;

function ADsGetObject;
begin
  GetProcedureAddress(_ADsGetObject, adslib, 'ADsGetObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsGetObject]
  end;
end;
{$ELSE}
function ADsGetObject; external adslib name 'ADsGetObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsBuildEnumerator: Pointer;

function ADsBuildEnumerator;
begin
  GetProcedureAddress(_ADsBuildEnumerator, adslib, 'ADsBuildEnumerator');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsBuildEnumerator]
  end;
end;
{$ELSE}
function ADsBuildEnumerator; external adslib name 'ADsBuildEnumerator';
{$ENDIF DYNAMIC_LINK}

function _ADsFreeEnumerator(pEnumVariant: IEnumVARIANT): HRESULT; stdcall; external adslib name 'ADsFreeEnumerator';

function ADsFreeEnumerator(var pEnumVariant: IEnumVARIANT): HRESULT;
begin
  Result := _ADsFreeEnumerator(pEnumVariant);
  // ADsFreeEnumerator doesn't set pEnumVariant to nil causing Delphi to call
  // Release() again when pEnumVariant leaves scope. Result would be an access
  // violation, explicitly setting the interface to nil prevents this.
  if Result = 0{S_OK} then Pointer(pEnumVariant) := nil;
end;


{$IFDEF DYNAMIC_LINK}
var
  _ADsEnumerateNext: Pointer;

function ADsEnumerateNext;
begin
  GetProcedureAddress(_ADsEnumerateNext, adslib, 'ADsEnumerateNext');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsEnumerateNext]
  end;
end;
{$ELSE}
function ADsEnumerateNext; external adslib name 'ADsEnumerateNext';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsBuildVarArrayStr: Pointer;

function ADsBuildVarArrayStr;
begin
  GetProcedureAddress(_ADsBuildVarArrayStr, adslib, 'ADsBuildVarArrayStr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsBuildVarArrayStr]
  end;
end;
{$ELSE}
function ADsBuildVarArrayStr; external adslib name 'ADsBuildVarArrayStr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsBuildVarArrayInt: Pointer;

function ADsBuildVarArrayInt;
begin
  GetProcedureAddress(_ADsBuildVarArrayInt, adslib, 'ADsBuildVarArrayInt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsBuildVarArrayInt]
  end;
end;
{$ELSE}
function ADsBuildVarArrayInt; external adslib name 'ADsBuildVarArrayInt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsOpenObject: Pointer;

function ADsOpenObject;
begin
  GetProcedureAddress(_ADsOpenObject, adslib, 'ADsOpenObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsOpenObject]
  end;
end;
{$ELSE}
function ADsOpenObject; external adslib name 'ADsOpenObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsGetLastError: Pointer;

function ADsGetLastError;
begin
  GetProcedureAddress(_ADsGetLastError, adslib, 'ADsGetLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsGetLastError]
  end;
end;
{$ELSE}
function ADsGetLastError; external adslib name 'ADsGetLastError';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsSetLastError: Pointer;

procedure ADsSetLastError;
begin
  GetProcedureAddress(_ADsSetLastError, adslib, 'ADsSetLastError');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsSetLastError]
  end;
end;
{$ELSE}
procedure ADsSetLastError; external adslib name 'ADsSetLastError';
{$ENDIF DYNAMIC_LINK}
//procedure ADsFreeAllErrorRecords

{$IFDEF DYNAMIC_LINK}
var
  _AllocADsMem: Pointer;

function AllocADsMem;
begin
  GetProcedureAddress(_AllocADsMem, adslib, 'AllocADsMem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllocADsMem]
  end;
end;
{$ELSE}
function AllocADsMem; external adslib name 'AllocADsMem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeADsMem: Pointer;

function FreeADsMem;
begin
  GetProcedureAddress(_FreeADsMem, adslib, 'FreeADsMem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeADsMem]
  end;
end;
{$ELSE}
function FreeADsMem; external adslib name 'FreeADsMem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReallocADsMem: Pointer;

function ReallocADsMem;
begin
  GetProcedureAddress(_ReallocADsMem, adslib, 'ReallocADsMem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReallocADsMem]
  end;
end;
{$ELSE}
function ReallocADsMem; external adslib name 'ReallocADsMem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AllocADsStr: Pointer;

function AllocADsStr;
begin
  GetProcedureAddress(_AllocADsStr, adslib, 'AllocADsStr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllocADsStr]
  end;
end;
{$ELSE}
function AllocADsStr; external adslib name 'AllocADsStr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeADsStr: Pointer;

function FreeADsStr;
begin
  GetProcedureAddress(_FreeADsStr, adslib, 'FreeADsStr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeADsStr]
  end;
end;
{$ELSE}
function FreeADsStr; external adslib name 'FreeADsStr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReallocADsStr: Pointer;

function ReallocADsStr;
begin
  GetProcedureAddress(_ReallocADsStr, adslib, 'ReallocADsStr');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReallocADsStr]
  end;
end;
{$ELSE}
function ReallocADsStr; external adslib name 'ReallocADsStr';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsEncodeBinaryData: Pointer;

function ADsEncodeBinaryData;
begin
  GetProcedureAddress(_ADsEncodeBinaryData, adslib, 'ADsEncodeBinaryData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsEncodeBinaryData]
  end;
end;
{$ELSE}
function ADsEncodeBinaryData; external adslib name 'ADsEncodeBinaryData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsDecodeBinaryData: Pointer;

function ADsDecodeBinaryData;
begin
  GetProcedureAddress(_ADsDecodeBinaryData, adslib, 'ADsDecodeBinaryData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsDecodeBinaryData]
  end;
end;
{$ELSE}
function ADsDecodeBinaryData; external adslib name 'ADsDecodeBinaryData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PropVariantToAdsType: Pointer;

function PropVariantToAdsType;
begin
  GetProcedureAddress(_PropVariantToAdsType, adslib, 'PropVariantToAdsType');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PropVariantToAdsType]
  end;
end;
{$ELSE}
function PropVariantToAdsType; external adslib name 'PropVariantToAdsType';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AdsTypeToPropVariant: Pointer;

function AdsTypeToPropVariant;
begin
  GetProcedureAddress(_AdsTypeToPropVariant, adslib, 'AdsTypeToPropVariant');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdsTypeToPropVariant]
  end;
end;
{$ELSE}
function AdsTypeToPropVariant; external adslib name 'AdsTypeToPropVariant';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AdsFreeAdsValues: Pointer;

procedure AdsFreeAdsValues;
begin
  GetProcedureAddress(_AdsFreeAdsValues, adslib, 'AdsFreeAdsValues');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdsFreeAdsValues]
  end;
end;
{$ELSE}
procedure AdsFreeAdsValues; external adslib name 'AdsFreeAdsValues';
{$ENDIF DYNAMIC_LINK}

// adsprop.h


{$IFDEF DYNAMIC_LINK}
var
  _ADsPropCreateNotifyObj: Pointer;

function ADsPropCreateNotifyObj;
begin
  GetProcedureAddress(_ADsPropCreateNotifyObj, dsprop, 'ADsPropCreateNotifyObj');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsPropCreateNotifyObj]
  end;
end;
{$ELSE}
function ADsPropCreateNotifyObj; external dsprop name 'ADsPropCreateNotifyObj';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsPropGetInitInfo: Pointer;

function ADsPropGetInitInfo;
begin
  GetProcedureAddress(_ADsPropGetInitInfo, dsprop, 'ADsPropGetInitInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsPropGetInitInfo]
  end;
end;
{$ELSE}
function ADsPropGetInitInfo; external dsprop name 'ADsPropGetInitInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsPropSetHwnd: Pointer;

function ADsPropSetHwnd;
begin
  GetProcedureAddress(_ADsPropSetHwnd, dsprop, 'ADsPropSetHwnd');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsPropSetHwnd]
  end;
end;
{$ELSE}
function ADsPropSetHwnd; external dsprop name 'ADsPropSetHwnd';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ADsPropCheckIfWritable: Pointer;

function ADsPropCheckIfWritable;
begin
  GetProcedureAddress(_ADsPropCheckIfWritable, dsprop, 'ADsPropCheckIfWritable');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ADsPropCheckIfWritable]
  end;
end;
{$ELSE}
function ADsPropCheckIfWritable; external dsprop name 'ADsPropCheckIfWritable';
{$ENDIF DYNAMIC_LINK}

end.
