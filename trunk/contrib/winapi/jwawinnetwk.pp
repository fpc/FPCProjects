{******************************************************************************}
{                                                       	               }
{ Windows Networking API interface Unit for Object Pascal                      }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winnetwk.h, released June 2000. The original Pascal    }
{ code is: WinNetWk.pas, released December 2000. The initial developer of the  }
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

unit JwaWinNetWk;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinNetWk.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinError, JwaWinType;

//
// Network types
//

const
  WNNC_NET_MSNET       = $00010000;
  WNNC_NET_LANMAN      = $00020000;
  WNNC_NET_NETWARE     = $00030000;
  WNNC_NET_VINES       = $00040000;
  WNNC_NET_10NET       = $00050000;
  WNNC_NET_LOCUS       = $00060000;
  WNNC_NET_SUN_PC_NFS  = $00070000;
  WNNC_NET_LANSTEP     = $00080000;
  WNNC_NET_9TILES      = $00090000;
  WNNC_NET_LANTASTIC   = $000A0000;
  WNNC_NET_AS400       = $000B0000;
  WNNC_NET_FTP_NFS     = $000C0000;
  WNNC_NET_PATHWORKS   = $000D0000;
  WNNC_NET_LIFENET     = $000E0000;
  WNNC_NET_POWERLAN    = $000F0000;
  WNNC_NET_BWNFS       = $00100000;
  WNNC_NET_COGENT      = $00110000;
  WNNC_NET_FARALLON    = $00120000;
  WNNC_NET_APPLETALK   = $00130000;
  WNNC_NET_INTERGRAPH  = $00140000;
  WNNC_NET_SYMFONET    = $00150000;
  WNNC_NET_CLEARCASE   = $00160000;
  WNNC_NET_FRONTIER    = $00170000;
  WNNC_NET_BMC         = $00180000;
  WNNC_NET_DCE         = $00190000;
  WNNC_NET_AVID        = $001A0000;
  WNNC_NET_DOCUSPACE   = $001B0000;
  WNNC_NET_MANGOSOFT   = $001C0000;
  WNNC_NET_SERNET      = $001D0000;
  WNNC_NET_RIVERFRONT1 = $001E0000;
  WNNC_NET_RIVERFRONT2 = $001F0000;
  WNNC_NET_DECORB      = $00200000;
  WNNC_NET_PROTSTOR    = $00210000;
  WNNC_NET_FJ_REDIR    = $00220000;
  WNNC_NET_DISTINCT    = $00230000;
  WNNC_NET_TWINS       = $00240000;
  WNNC_NET_RDR2SAMPLE  = $00250000;
  WNNC_NET_CSC         = $00260000;
  WNNC_NET_3IN1        = $00270000;
  WNNC_NET_EXTENDNET   = $00290000;
  WNNC_NET_STAC        = $002A0000;
  WNNC_NET_FOXBAT      = $002B0000;
  WNNC_NET_YAHOO       = $002C0000;
  WNNC_NET_EXIFS       = $002D0000;
  WNNC_NET_DAV         = $002E0000;
  WNNC_NET_KNOWARE     = $002F0000;
  WNNC_NET_OBJECT_DIRE = $00300000;
  WNNC_NET_MASFAX      = $00310000;
  WNNC_NET_HOB_NFS     = $00320000;
  WNNC_NET_SHIVA       = $00330000;
  WNNC_NET_IBMAL       = $00340000;
  WNNC_NET_LOCK        = $00350000;
  WNNC_NET_TERMSRV     = $00360000;
  WNNC_NET_SRT         = $00370000;
  WNNC_NET_QUINCY      = $00380000;

  WNNC_CRED_MANAGER = DWORD($FFFF0000);

//
//  Network Resources.
//

  RESOURCE_CONNECTED  = $00000001;
  RESOURCE_GLOBALNET  = $00000002;
  RESOURCE_REMEMBERED = $00000003;
  RESOURCE_RECENT     = $00000004;
  RESOURCE_CONTEXT    = $00000005;

  RESOURCETYPE_ANY      = $00000000;
  RESOURCETYPE_DISK     = $00000001;
  RESOURCETYPE_PRINT    = $00000002;
  RESOURCETYPE_RESERVED = $00000008;
  RESOURCETYPE_UNKNOWN  = DWORD($FFFFFFFF);

  RESOURCEUSAGE_CONNECTABLE   = $00000001;
  RESOURCEUSAGE_CONTAINER     = $00000002;
  RESOURCEUSAGE_NOLOCALDEVICE = $00000004;
  RESOURCEUSAGE_SIBLING       = $00000008;
  RESOURCEUSAGE_ATTACHED      = $00000010;
  RESOURCEUSAGE_ALL           = (RESOURCEUSAGE_CONNECTABLE or
    RESOURCEUSAGE_CONTAINER or RESOURCEUSAGE_ATTACHED);
  RESOURCEUSAGE_RESERVED      = DWORD($80000000);

  RESOURCEDISPLAYTYPE_GENERIC      = $00000000;
  RESOURCEDISPLAYTYPE_DOMAIN       = $00000001;
  RESOURCEDISPLAYTYPE_SERVER       = $00000002;
  RESOURCEDISPLAYTYPE_SHARE        = $00000003;
  RESOURCEDISPLAYTYPE_FILE         = $00000004;
  RESOURCEDISPLAYTYPE_GROUP        = $00000005;
  RESOURCEDISPLAYTYPE_NETWORK      = $00000006;
  RESOURCEDISPLAYTYPE_ROOT         = $00000007;
  RESOURCEDISPLAYTYPE_SHAREADMIN   = $00000008;
  RESOURCEDISPLAYTYPE_DIRECTORY    = $00000009;
  RESOURCEDISPLAYTYPE_TREE         = $0000000A;
  RESOURCEDISPLAYTYPE_NDSCONTAINER = $0000000B;

type
  LPNETRESOURCEA = ^NETRESOURCEA;
  _NETRESOURCEA = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: LPSTR;
    lpRemoteName: LPSTR;
    lpComment: LPSTR;
    lpProvider: LPSTR;
  end;
  NETRESOURCEA = _NETRESOURCEA;
  TNetResourceA = NETRESOURCEA;
  PNetResourceA = LPNETRESOURCEA;

  LPNETRESOURCEW = ^NETRESOURCEW;
  _NETRESOURCEW = record
    dwScope: DWORD;
    dwType: DWORD;
    dwDisplayType: DWORD;
    dwUsage: DWORD;
    lpLocalName: LPWSTR;
    lpRemoteName: LPWSTR;
    lpComment: LPWSTR;
    lpProvider: LPWSTR;
  end;
  NETRESOURCEW = _NETRESOURCEW;
  TNetResourceW = NETRESOURCEW;
  PNetResourceW = LPNETRESOURCEW;

{$IFDEF UNICODE}
  NETRESOURCE = NETRESOURCEW;
  LPNETRESOURCE = LPNETRESOURCEW;
  TNetResource = TNetResourceW;
  PNetResource = PNetResourceW;
{$ELSE}
  NETRESOURCE = NETRESOURCEA;
  LPNETRESOURCE = LPNETRESOURCEA;
  TNetResource = TNetResourceA;
  PNetResource = PNetResourceA;
{$ENDIF}

//
//  Network Connections.
//

const
  NETPROPERTY_PERSISTENT = 1;

  CONNECT_UPDATE_PROFILE = $00000001;
  CONNECT_UPDATE_RECENT  = $00000002;
  CONNECT_TEMPORARY      = $00000004;
  CONNECT_INTERACTIVE    = $00000008;
  CONNECT_PROMPT         = $00000010;
  CONNECT_NEED_DRIVE     = $00000020;
  CONNECT_REFCOUNT       = $00000040;
  CONNECT_REDIRECT       = $00000080;
  CONNECT_LOCALDRIVE     = $00000100;
  CONNECT_CURRENT_MEDIA  = $00000200;
  CONNECT_DEFERRED       = $00000400;
  CONNECT_RESERVED       = DWORD($FF000000);
  CONNECT_COMMANDLINE    = $00000800;
  CONNECT_CMD_SAVECRED   = $00001000;

function WNetAddConnectionA(lpRemoteName, lpPassword, lpLocalName: LPCSTR): DWORD; stdcall;
function WNetAddConnectionW(lpRemoteName, lpPassword, lpLocalName: LPCWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetAddConnection(lpRemoteName, lpPassword, lpLocalName: LPCWSTR): DWORD; stdcall;
{$ELSE}
function WNetAddConnection(lpRemoteName, lpPassword, lpLocalName: LPCSTR): DWORD; stdcall;
{$ENDIF}

function WNetAddConnection2A(const lpNetResource: NETRESOURCEA; lpPassword: LPCSTR;
  lpUserName: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
function WNetAddConnection2W(const lpNetResource: NETRESOURCEW; lpPassword: LPCWSTR;
  lpUserName: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetAddConnection2(const lpNetResource: NETRESOURCEW; lpPassword: LPCWSTR;
  lpUserName: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;
{$ELSE}
function WNetAddConnection2(const lpNetResource: NETRESOURCEA; lpPassword: LPCSTR;
  lpUserName: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
{$ENDIF}

function WNetAddConnection3A(hwndOwner: HWND; const lpNetResource: NETRESOURCEA;
  lpPassword: LPCSTR; lpUserName: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
function WNetAddConnection3W(hwndOwner: HWND; const lpNetResource: NETRESOURCEW;
  lpPassword: LPCWSTR; lpUserName: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetAddConnection3(hwndOwner: HWND; const lpNetResource: LPNETRESOURCEW;
  lpPassword: LPCWSTR; lpUserName: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;
{$ELSE}
function WNetAddConnection3(hwndOwner: HWND; const lpNetResource: NETRESOURCEA;
  lpPassword: LPCSTR; lpUserName: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
{$ENDIF}

function WNetCancelConnectionA(lpName: LPCSTR; fForce: BOOL): DWORD; stdcall;
function WNetCancelConnectionW(lpName: LPCWSTR; fForce: BOOL): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetCancelConnection(lpName: LPCWSTR; fForce: BOOL): DWORD; stdcall;
{$ELSE}
function WNetCancelConnection(lpName: LPCSTR; fForce: BOOL): DWORD; stdcall;
{$ENDIF}

function WNetCancelConnection2A(lpName: LPCSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;
function WNetCancelConnection2W(lpName: LPCWSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetCancelConnection2(lpName: LPCWSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;
{$ELSE}
function WNetCancelConnection2(lpName: LPCSTR; dwFlags: DWORD; fForce: BOOL): DWORD; stdcall;
{$ENDIF}

function WNetGetConnectionA(lpLocalName, lpRemoteName: LPSTR; var lpnLength: DWORD): DWORD; stdcall;
function WNetGetConnectionW(lpLocalName, lpRemoteName: LPWSTR; var lpnLength: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetConnection(lpLocalName, lpRemoteName: LPWSTR; var lpnLength: DWORD): DWORD; stdcall;
{$ELSE}
function WNetGetConnection(lpLocalName, lpRemoteName: LPSTR; var lpnLength: DWORD): DWORD; stdcall;
{$ENDIF}

function WNetRestoreConnectionA(hwndParent: HWND; lpDevice: LPCSTR): DWORD; stdcall;
function WNetRestoreConnectionW(hwndParent: HWND; lpDevice: LPCWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetRestoreConnection(hwndParent: HWND; lpDevice: LPCWSTR): DWORD; stdcall;
{$ELSE}
function WNetRestoreConnection(hwndParent: HWND; lpDevice: LPCSTR): DWORD; stdcall;
{$ENDIF}

function WNetUseConnectionA(hwndOwner: HWND; const lpNetResource: NETRESOURCEA;
  lpPassword, lpUserID: LPCSTR; dwFlags: DWORD; lpAccessName: LPSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;
function WNetUseConnectionW(hwndOwner: HWND; const lpNetResource: NETRESOURCEW;
  lpPassword, lpUserID: LPCWSTR; dwFlags: DWORD; lpAccessName: LPWSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetUseConnection(hwndOwner: HWND; const lpNetResource: NETRESOURCEW;
  lpPassword, lpUserID: LPCWSTR; dwFlags: DWORD; lpAccessName: LPWSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;
{$ELSE}
function WNetUseConnection(hwndOwner: HWND; const lpNetResource: NETRESOURCEA;
  lpPassword, lpUserID: LPCSTR; dwFlags: DWORD; lpAccessName: LPSTR;
  var lpBufferSize, lpResult: DWORD): DWORD; stdcall;
{$ENDIF}

//
//  Network Connection Dialogs.
//

function WNetConnectionDialog(hwnd: HWND; dwType: DWORD): DWORD; stdcall;

function WNetDisconnectDialog(hwnd: HWND; dwType: DWORD): DWORD; stdcall;

type
  LPCONNECTDLGSTRUCTA = ^CONNECTDLGSTRUCTA;
  _CONNECTDLGSTRUCTA = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpConnRes: LPNETRESOURCEA; // Requested Resource info
    dwFlags: DWORD; // flags (see below)
    dwDevNum: DWORD; // number of devices connected to
  end;
  CONNECTDLGSTRUCTA = _CONNECTDLGSTRUCTA;
  TConnectDlgStructA = CONNECTDLGSTRUCTA;
  PConnectDlgStructA = LPCONNECTDLGSTRUCTA;

  LPCONNECTDLGSTRUCTW = ^CONNECTDLGSTRUCTW;
  _CONNECTDLGSTRUCTW = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpConnRes: LPNETRESOURCEW; // Requested Resource info
    dwFlags: DWORD; // flags (see below)
    dwDevNum: DWORD; // number of devices connected to
  end;
  CONNECTDLGSTRUCTW = _CONNECTDLGSTRUCTW;
  TConnectDlgStructW = CONNECTDLGSTRUCTW;
  PConnectDlgStructW = LPCONNECTDLGSTRUCTW;

{$IFDEF UNICODE}
  CONNECTDLGSTRUCT = CONNECTDLGSTRUCTW;
  LPCONNECTDLGSTRUCT = LPCONNECTDLGSTRUCTW;
  TConnectDlgStruct = TConnectDlgStructW;
  PConnectDlgStruct = PConnectDlgStructW;
{$ELSE}
  CONNECTDLGSTRUCT = CONNECTDLGSTRUCTA;
  LPCONNECTDLGSTRUCT = LPCONNECTDLGSTRUCTA;
  TConnectDlgStruct = TConnectDlgStructA;
  PConnectDlgStruct = PConnectDlgStructA;
{$ENDIF}

const
  CONNDLG_RO_PATH    = $00000001; // Resource path should be read-only
  CONNDLG_CONN_POINT = $00000002; // Netware -style movable connection point enabled
  CONNDLG_USE_MRU    = $00000004; // Use MRU combobox
  CONNDLG_HIDE_BOX   = $00000008; // Hide persistent connect checkbox

//
// NOTE:  Set at most ONE of the below flags.  If neither flag is set,
//        then the persistence is set to whatever the user chose during
//        a previous connection
//

  CONNDLG_PERSIST     = $00000010; // Force persistent connection
  CONNDLG_NOT_PERSIST = $00000020; // Force connection NOT persistent

function WNetConnectionDialog1A(var lpConnDlgStruct: CONNECTDLGSTRUCTA): DWORD; stdcall;
function WNetConnectionDialog1W(var lpConnDlgStruct: CONNECTDLGSTRUCTW): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetConnectionDialog1(var lpConnDlgStruct: CONNECTDLGSTRUCTW): DWORD; stdcall;
{$ELSE}
function WNetConnectionDialog1(var lpConnDlgStruct: CONNECTDLGSTRUCTA): DWORD; stdcall;
{$ENDIF}

type
  LPDISCDLGSTRUCTA = ^DISCDLGSTRUCTA;
  _DISCDLGSTRUCTA = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpLocalName: LPSTR; // local device name
    lpRemoteName: LPSTR; // network resource name
    dwFlags: DWORD; // flags
  end;
  DISCDLGSTRUCTA = _DISCDLGSTRUCTA;
  TDiscDlgStructA = DISCDLGSTRUCTA;
  PDiscDlgStructA = LPDISCDLGSTRUCTA;

  LPDISCDLGSTRUCTW = ^DISCDLGSTRUCTW;
  _DISCDLGSTRUCTW = record
    cbStructure: DWORD; // size of this structure in bytes
    hwndOwner: HWND; // owner window for the dialog
    lpLocalName: LPWSTR; // local device name
    lpRemoteName: LPWSTR; // network resource name
    dwFlags: DWORD; // flags
  end;
  DISCDLGSTRUCTW = _DISCDLGSTRUCTW;
  TDiscDlgStructW = DISCDLGSTRUCTW;
  PDiscDlgStructW = LPDISCDLGSTRUCTW;

{$IFDEF UNICODE}
  DISCDLGSTRUCT = DISCDLGSTRUCTW;
  LPDISCDLGSTRUCT = LPDISCDLGSTRUCTW;
  TDiscDlgStruct = TDiscDlgStructW;
  PDiscDlgStruct = PDiscDlgStructW;
{$ELSE}
  DISCDLGSTRUCT = DISCDLGSTRUCTA;
  LPDISCDLGSTRUCT = LPDISCDLGSTRUCTA;
  TDiscDlgStruct = TDiscDlgStructA;
  PDiscDlgStruct = PDiscDlgStructA;
{$ENDIF}

const
  DISC_UPDATE_PROFILE = $00000001;
  DISC_NO_FORCE       = $00000040;

function WNetDisconnectDialog1A(const lpConnDlgStruct: DISCDLGSTRUCTA): DWORD; stdcall;
function WNetDisconnectDialog1W(const lpConnDlgStruct: DISCDLGSTRUCTW): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetDisconnectDialog1(const lpConnDlgStruct: DISCDLGSTRUCTW): DWORD; stdcall;
{$ELSE}
function WNetDisconnectDialog1(const lpConnDlgStruct: DISCDLGSTRUCTA): DWORD; stdcall;
{$ENDIF}

//
//  Network Browsing.
//

function WNetOpenEnumA(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCEA;
  var lphEnum: HANDLE): DWORD; stdcall;
function WNetOpenEnumW(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCEW;
  var lphEnum: HANDLE): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetOpenEnum(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCEW;
  var lphEnum: HANDLE): DWORD; stdcall;
{$ELSE}
function WNetOpenEnum(dwScope, dwType, dwUsage: DWORD; lpNetResource: LPNETRESOURCEA;
  var lphEnum: HANDLE): DWORD; stdcall;
{$ENDIF}

function WNetEnumResourceA(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;
function WNetEnumResourceW(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetEnumResource(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$ELSE}
function WNetEnumResource(hEnum: HANDLE; var lpcCount: DWORD; lpBuffer: LPVOID;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$ENDIF}

function WNetCloseEnum(hEnum: HANDLE): DWORD; stdcall;

function WNetGetResourceParentA(const lpNetResource: NETRESOURCEA;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;
function WNetGetResourceParentW(const lpNetResource: NETRESOURCEW;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetResourceParent(const lpNetResource: NETRESOURCEW;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;
{$ELSE}
function WNetGetResourceParent(const lpNetResource: NETRESOURCEA;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD): DWORD; stdcall;
{$ENDIF}

function WNetGetResourceInformationA(const lpNetResource: NETRESOURCEA;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPSTR): DWORD; stdcall;
function WNetGetResourceInformationW(const lpNetResource: NETRESOURCEW;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetResourceInformation(const lpNetResource: NETRESOURCEW;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPWSTR): DWORD; stdcall;
{$ELSE}
function WNetGetResourceInformation(const lpNetResource: NETRESOURCEA;
  lpBuffer: LPVOID; var lpcbBuffer: DWORD; var lplpSystem: LPSTR): DWORD; stdcall;
{$ENDIF}

//
//  Universal Naming.
//

const
  UNIVERSAL_NAME_INFO_LEVEL = $00000001;
  REMOTE_NAME_INFO_LEVEL    = $00000002;

type
  LPUNIVERSAL_NAME_INFOA = ^UNIVERSAL_NAME_INFOA;
  _UNIVERSAL_NAME_INFOA = record
    lpUniversalName: LPSTR;
  end;
  UNIVERSAL_NAME_INFOA = _UNIVERSAL_NAME_INFOA;
  TUniversalNameInfoA = UNIVERSAL_NAME_INFOA;
  PUniversalNameInfoA = LPUNIVERSAL_NAME_INFOA;

  LPUNIVERSAL_NAME_INFOW = ^UNIVERSAL_NAME_INFOW;
  _UNIVERSAL_NAME_INFOW = record
    lpUniversalName: LPWSTR;
  end;
  UNIVERSAL_NAME_INFOW = _UNIVERSAL_NAME_INFOW;
  TUniversalNameInfoW = UNIVERSAL_NAME_INFOW;
  PUniversalNameInfoW = LPUNIVERSAL_NAME_INFOW;

{$IFDEF UNICODE}
  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOW;
  LPUNIVERSAL_NAME_INFO = LPUNIVERSAL_NAME_INFOW;
  TUniversalNameInfo = TUniversalNameInfoW;
  PUniversalNameInfo = PUniversalNameInfoW;
{$ELSE}
  UNIVERSAL_NAME_INFO = UNIVERSAL_NAME_INFOA;
  LPUNIVERSAL_NAME_INFO = LPUNIVERSAL_NAME_INFOA;
  TUniversalNameInfo = TUniversalNameInfoA;
  PUniversalNameInfo = PUniversalNameInfoA;
{$ENDIF}

  LPREMOTE_NAME_INFOA = ^REMOTE_NAME_INFOA;
  _REMOTE_NAME_INFOA = record
    lpUniversalName: LPSTR;
    lpConnectionName: LPSTR;
    lpRemainingPath: LPSTR;
  end;
  REMOTE_NAME_INFOA = _REMOTE_NAME_INFOA;
  TRemoteNameInfoA = REMOTE_NAME_INFOA;
  PRemoteNameInfoA = LPREMOTE_NAME_INFOA;

  LPREMOTE_NAME_INFOW = ^REMOTE_NAME_INFOW;
  _REMOTE_NAME_INFOW = record
    lpUniversalName: LPWSTR;
    lpConnectionName: LPWSTR;
    lpRemainingPath: LPWSTR;
  end;
  REMOTE_NAME_INFOW = _REMOTE_NAME_INFOW;
  TRemoteNameInfoW = REMOTE_NAME_INFOW;
  PRemoteNameInfoW = LPREMOTE_NAME_INFOW;

{$IFDEF UNICODE}
  REMOTE_NAME_INFO = REMOTE_NAME_INFOW;
  LPREMOTE_NAME_INFO = LPREMOTE_NAME_INFOW;
  TRemoteNameInfo = TRemoteNameInfoW;
  PRemoteNameInfo = PRemoteNameInfoW;
{$ELSE}
  REMOTE_NAME_INFO = REMOTE_NAME_INFOA;
  LPREMOTE_NAME_INFO = LPREMOTE_NAME_INFOA;
  TRemoteNameInfo = TRemoteNameInfoA;
  PRemoteNameInfo = PRemoteNameInfoA;
{$ENDIF}

function WNetGetUniversalNameA(lpLocalPath: LPCSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;
function WNetGetUniversalNameW(lpLocalPath: LPCWSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetUniversalName(lpLocalPath: LPCWSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;
{$ELSE}
function WNetGetUniversalName(lpLocalPath: LPCSTR; dwInfoLevel: DWORD;
  lpBuffer: LPVOID; var lpBufferSize: DWORD): DWORD; stdcall;
{$ENDIF}

//
//  Authentication and Logon/Logoff.
//

function WNetGetUserA(lpName: LPCSTR; lpUserName: LPSTR; var lpnLength: DWORD): DWORD; stdcall;
function WNetGetUserW(lpName: LPCWSTR; lpUserName: LPWSTR; var lpnLength: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetUser(lpName: LPCWSTR; lpUserName: LPWSTR; var lpnLength: DWORD): DWORD; stdcall;
{$ELSE}
function WNetGetUser(lpName: LPCSTR; lpUserName: LPSTR; var lpnLength: DWORD): DWORD; stdcall;
{$ENDIF}

//
// Other.
//

const
  WNFMT_MULTILINE   = $01;
  WNFMT_ABBREVIATED = $02;
  WNFMT_INENUM      = $10;
  WNFMT_CONNECTION  = $20;

function WNetGetProviderNameA(dwNetType: DWORD; lpProviderName: LPSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;
function WNetGetProviderNameW(dwNetType: DWORD; lpProviderName: LPWSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetProviderName(dwNetType: DWORD; lpProviderName: LPWSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$ELSE}
function WNetGetProviderName(dwNetType: DWORD; lpProviderName: LPSTR;
  var lpBufferSize: DWORD): DWORD; stdcall;
{$ENDIF}

type
  LPNETINFOSTRUCT = ^NETINFOSTRUCT;
  _NETINFOSTRUCT = record
    cbStructure: DWORD;
    dwProviderVersion: DWORD;
    dwStatus: DWORD;
    dwCharacteristics: DWORD;
    dwHandle: ULONG_PTR;
    wNetType: WORD;
    dwPrinters: DWORD;
    dwDrives: DWORD;
  end;
  NETINFOSTRUCT = _NETINFOSTRUCT;
  TNetInfoStruct = NETINFOSTRUCT;
  PNetInfoStruct = LPNETINFOSTRUCT;

const
  NETINFO_DLL16      = $00000001; // Provider running as 16 bit Winnet Driver
  NETINFO_DISKRED    = $00000004; // Provider requires disk redirections to connect
  NETINFO_PRINTERRED = $00000008; // Provider requires printer redirections to connect

function WNetGetNetworkInformationA(lpProvider: LPCSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;
function WNetGetNetworkInformationW(lpProvider: LPCWSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetNetworkInformation(lpProvider: LPCWSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;
{$ELSE}
function WNetGetNetworkInformation(lpProvider: LPCSTR;
  var lpNetInfoStruct: NETINFOSTRUCT): DWORD; stdcall;
{$ENDIF}

//
//  User Profiles.
//

type
  PFNGETPROFILEPATHA = function (pszUsername: LPCSTR; pszBuffer: LPSTR;
    cbBuffer: UINT): UINT; stdcall;

  PFNGETPROFILEPATHW = function (pszUsername: LPCWSTR; pszBuffer: LPWSTR;
    cbBuffer: UINT): UINT; stdcall;

{$IFDEF UNICODE}
  PFNGETPROFILEPATH = PFNGETPROFILEPATHW;
{$ELSE}
  PFNGETPROFILEPATH = PFNGETPROFILEPATHA;
{$ENDIF}

  PFNRECONCILEPROFILEA = function (pszCentralFile, pszLocalFile: LPCSTR;
    dwFlags: DWORD): UINT; stdcall;

  PFNRECONCILEPROFILEW = function (pszCentralFile, pszLocalFile: LPCWSTR;
    dwFlags: DWORD): UINT; stdcall;


{$IFDEF UNICODE}
  PFNRECONCILEPROFILE = PFNRECONCILEPROFILEW;
{$ELSE}
  PFNRECONCILEPROFILE = PFNRECONCILEPROFILEA;
{$ENDIF}

const
  RP_LOGON   = $01; // if set, do for logon, else for logoff
  RP_INIFILE = $02; // if set, reconcile .INI file, else reg. hive

//
//  Policies.
//

type
  PFNPROCESSPOLICIESA = function (hwnd: HWND; pszPath, pszUsername,
    pszComputerName: LPCSTR; dwFlags: DWORD): BOOL; stdcall;

  PFNPROCESSPOLICIESW = function (hwnd: HWND; pszPath, pszUsername,
    pszComputerName: LPCWSTR; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
  PFNPROCESSPOLICIES = PFNPROCESSPOLICIESW;
{$ELSE}
  PFNPROCESSPOLICIES = PFNPROCESSPOLICIESA;
{$ENDIF}

const
  PP_DISPLAYERRORS = $01; // if set, display error messages, else fail silently if error

//
//  Error handling.
//

function WNetGetLastErrorA(var lpError: DWORD; lpErrorBuf: LPSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPSTR; nNameBufSize: DWORD): DWORD; stdcall;
function WNetGetLastErrorW(var lpError: DWORD; lpErrorBuf: LPWSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPWSTR; nNameBufSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function WNetGetLastError(var lpError: DWORD; lpErrorBuf: LPWSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPWSTR; nNameBufSize: DWORD): DWORD; stdcall;
{$ELSE}
function WNetGetLastError(var lpError: DWORD; lpErrorBuf: LPSTR;
  nErrorBufSize: DWORD; lpNameBuf: LPSTR; nNameBufSize: DWORD): DWORD; stdcall;
{$ENDIF}

//
//  STATUS CODES
//

// General

const
  WN_SUCCESS          = NO_ERROR;
  WN_NO_ERROR         = NO_ERROR;
  WN_NOT_SUPPORTED    = ERROR_NOT_SUPPORTED;
  WN_CANCEL           = ERROR_CANCELLED;
  WN_RETRY            = ERROR_RETRY;
  WN_NET_ERROR        = ERROR_UNEXP_NET_ERR;
  WN_MORE_DATA        = ERROR_MORE_DATA;
  WN_BAD_POINTER      = ERROR_INVALID_ADDRESS;
  WN_BAD_VALUE        = ERROR_INVALID_PARAMETER;
  WN_BAD_USER         = ERROR_BAD_USERNAME;
  WN_BAD_PASSWORD     = ERROR_INVALID_PASSWORD;
  WN_ACCESS_DENIED    = ERROR_ACCESS_DENIED;
  WN_FUNCTION_BUSY    = ERROR_BUSY;
  WN_WINDOWS_ERROR    = ERROR_UNEXP_NET_ERR;
  WN_OUT_OF_MEMORY    = ERROR_NOT_ENOUGH_MEMORY;
  WN_NO_NETWORK       = ERROR_NO_NETWORK;
  WN_EXTENDED_ERROR   = ERROR_EXTENDED_ERROR;
  WN_BAD_LEVEL        = ERROR_INVALID_LEVEL;
  WN_BAD_HANDLE       = ERROR_INVALID_HANDLE;
  WN_NOT_INITIALIZING = ERROR_ALREADY_INITIALIZED;
  WN_NO_MORE_DEVICES  = ERROR_NO_MORE_DEVICES;

// Connection

  WN_NOT_CONNECTED             = ERROR_NOT_CONNECTED;
  WN_OPEN_FILES                = ERROR_OPEN_FILES;
  WN_DEVICE_IN_USE             = ERROR_DEVICE_IN_USE;
  WN_BAD_NETNAME               = ERROR_BAD_NET_NAME;
  WN_BAD_LOCALNAME             = ERROR_BAD_DEVICE;
  WN_ALREADY_CONNECTED         = ERROR_ALREADY_ASSIGNED;
  WN_DEVICE_ERROR              = ERROR_GEN_FAILURE;
  WN_CONNECTION_CLOSED         = ERROR_CONNECTION_UNAVAIL;
  WN_NO_NET_OR_BAD_PATH        = ERROR_NO_NET_OR_BAD_PATH;
  WN_BAD_PROVIDER              = ERROR_BAD_PROVIDER;
  WN_CANNOT_OPEN_PROFILE       = ERROR_CANNOT_OPEN_PROFILE;
  WN_BAD_PROFILE               = ERROR_BAD_PROFILE;
  WN_BAD_DEV_TYPE              = ERROR_BAD_DEV_TYPE;
  WN_DEVICE_ALREADY_REMEMBERED = ERROR_DEVICE_ALREADY_REMEMBERED;
  WN_CONNECTED_OTHER_PASSWORD  = ERROR_CONNECTED_OTHER_PASSWORD;
  WN_CONNECTED_OTHER_PASSWORD_DEFAULT = ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT;

// Enumeration

  WN_NO_MORE_ENTRIES = ERROR_NO_MORE_ITEMS;
  WN_NOT_CONTAINER   = ERROR_NOT_CONTAINER;

// Authentication

  WN_NOT_AUTHENTICATED = ERROR_NOT_AUTHENTICATED;
  WN_NOT_LOGGED_ON     = ERROR_NOT_LOGGED_ON;
  WN_NOT_VALIDATED     = ERROR_NO_LOGON_SERVERS;

//
//  For Shell
//

type
  LPNETCONNECTINFOSTRUCT = ^NETCONNECTINFOSTRUCT;
  _NETCONNECTINFOSTRUCT = record
    cbStructure: DWORD;
    dwFlags: DWORD;
    dwSpeed: DWORD;
    dwDelay: DWORD;
    dwOptDataSize: DWORD;
  end;
  NETCONNECTINFOSTRUCT = _NETCONNECTINFOSTRUCT;
  TNetConnectInfoStruct = NETCONNECTINFOSTRUCT;
  PNetConnectInfoStruct = LPNETCONNECTINFOSTRUCT;

const
  WNCON_FORNETCARD = $00000001;
  WNCON_NOTROUTED  = $00000002;
  WNCON_SLOWLINK   = $00000004;
  WNCON_DYNAMIC    = $00000008;

function MultinetGetConnectionPerformanceA(const lpNetResource: NETRESOURCEA;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;
function MultinetGetConnectionPerformanceW(const lpNetResource: NETRESOURCEW;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;

{$IFDEF UNICODE}
function MultinetGetConnectionPerformance(const lpNetResource: NETRESOURCEW;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;
{$ELSE}
function MultinetGetConnectionPerformance(const lpNetResource: NETRESOURCEA;
  var lpNetConnectInfoStruct: NETCONNECTINFOSTRUCT): DWORD; stdcall;
{$ENDIF}

implementation

const
  mpr = 'mpr.dll';


{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnectionA: Pointer;

function WNetAddConnectionA;
begin
  GetProcedureAddress(_WNetAddConnectionA, mpr, 'WNetAddConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnectionA]
  end;
end;
{$ELSE}
function WNetAddConnectionA; external mpr name 'WNetAddConnectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnectionW: Pointer;

function WNetAddConnectionW;
begin
  GetProcedureAddress(_WNetAddConnectionW, mpr, 'WNetAddConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnectionW]
  end;
end;
{$ELSE}
function WNetAddConnectionW; external mpr name 'WNetAddConnectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection: Pointer;

function WNetAddConnection;
begin
  GetProcedureAddress(_WNetAddConnection, mpr, 'WNetAddConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection]
  end;
end;
{$ELSE}
function WNetAddConnection; external mpr name 'WNetAddConnectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection: Pointer;

function WNetAddConnection;
begin
  GetProcedureAddress(_WNetAddConnection, mpr, 'WNetAddConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection]
  end;
end;
{$ELSE}
function WNetAddConnection; external mpr name 'WNetAddConnectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection2A: Pointer;

function WNetAddConnection2A;
begin
  GetProcedureAddress(_WNetAddConnection2A, mpr, 'WNetAddConnection2A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection2A]
  end;
end;
{$ELSE}
function WNetAddConnection2A; external mpr name 'WNetAddConnection2A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection2W: Pointer;

function WNetAddConnection2W;
begin
  GetProcedureAddress(_WNetAddConnection2W, mpr, 'WNetAddConnection2W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection2W]
  end;
end;
{$ELSE}
function WNetAddConnection2W; external mpr name 'WNetAddConnection2W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection2: Pointer;

function WNetAddConnection2;
begin
  GetProcedureAddress(_WNetAddConnection2, mpr, 'WNetAddConnection2W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection2]
  end;
end;
{$ELSE}
function WNetAddConnection2; external mpr name 'WNetAddConnection2W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection2: Pointer;

function WNetAddConnection2;
begin
  GetProcedureAddress(_WNetAddConnection2, mpr, 'WNetAddConnection2A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection2]
  end;
end;
{$ELSE}
function WNetAddConnection2; external mpr name 'WNetAddConnection2A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection3A: Pointer;

function WNetAddConnection3A;
begin
  GetProcedureAddress(_WNetAddConnection3A, mpr, 'WNetAddConnection3A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection3A]
  end;
end;
{$ELSE}
function WNetAddConnection3A; external mpr name 'WNetAddConnection3A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection3W: Pointer;

function WNetAddConnection3W;
begin
  GetProcedureAddress(_WNetAddConnection3W, mpr, 'WNetAddConnection3W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection3W]
  end;
end;
{$ELSE}
function WNetAddConnection3W; external mpr name 'WNetAddConnection3W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection3: Pointer;

function WNetAddConnection3;
begin
  GetProcedureAddress(_WNetAddConnection3, mpr, 'WNetAddConnection3W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection3]
  end;
end;
{$ELSE}
function WNetAddConnection3; external mpr name 'WNetAddConnection3W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetAddConnection3: Pointer;

function WNetAddConnection3;
begin
  GetProcedureAddress(_WNetAddConnection3, mpr, 'WNetAddConnection3A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetAddConnection3]
  end;
end;
{$ELSE}
function WNetAddConnection3; external mpr name 'WNetAddConnection3A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnectionA: Pointer;

function WNetCancelConnectionA;
begin
  GetProcedureAddress(_WNetCancelConnectionA, mpr, 'WNetCancelConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnectionA]
  end;
end;
{$ELSE}
function WNetCancelConnectionA; external mpr name 'WNetCancelConnectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnectionW: Pointer;

function WNetCancelConnectionW;
begin
  GetProcedureAddress(_WNetCancelConnectionW, mpr, 'WNetCancelConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnectionW]
  end;
end;
{$ELSE}
function WNetCancelConnectionW; external mpr name 'WNetCancelConnectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnection: Pointer;

function WNetCancelConnection;
begin
  GetProcedureAddress(_WNetCancelConnection, mpr, 'WNetCancelConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnection]
  end;
end;
{$ELSE}
function WNetCancelConnection; external mpr name 'WNetCancelConnectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnection: Pointer;

function WNetCancelConnection;
begin
  GetProcedureAddress(_WNetCancelConnection, mpr, 'WNetCancelConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnection]
  end;
end;
{$ELSE}
function WNetCancelConnection; external mpr name 'WNetCancelConnectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnection2A: Pointer;

function WNetCancelConnection2A;
begin
  GetProcedureAddress(_WNetCancelConnection2A, mpr, 'WNetCancelConnection2A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnection2A]
  end;
end;
{$ELSE}
function WNetCancelConnection2A; external mpr name 'WNetCancelConnection2A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnection2W: Pointer;

function WNetCancelConnection2W;
begin
  GetProcedureAddress(_WNetCancelConnection2W, mpr, 'WNetCancelConnection2W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnection2W]
  end;
end;
{$ELSE}
function WNetCancelConnection2W; external mpr name 'WNetCancelConnection2W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnection2: Pointer;

function WNetCancelConnection2;
begin
  GetProcedureAddress(_WNetCancelConnection2, mpr, 'WNetCancelConnection2W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnection2]
  end;
end;
{$ELSE}
function WNetCancelConnection2; external mpr name 'WNetCancelConnection2W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCancelConnection2: Pointer;

function WNetCancelConnection2;
begin
  GetProcedureAddress(_WNetCancelConnection2, mpr, 'WNetCancelConnection2A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCancelConnection2]
  end;
end;
{$ELSE}
function WNetCancelConnection2; external mpr name 'WNetCancelConnection2A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetConnectionA: Pointer;

function WNetGetConnectionA;
begin
  GetProcedureAddress(_WNetGetConnectionA, mpr, 'WNetGetConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetConnectionA]
  end;
end;
{$ELSE}
function WNetGetConnectionA; external mpr name 'WNetGetConnectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetConnectionW: Pointer;

function WNetGetConnectionW;
begin
  GetProcedureAddress(_WNetGetConnectionW, mpr, 'WNetGetConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetConnectionW]
  end;
end;
{$ELSE}
function WNetGetConnectionW; external mpr name 'WNetGetConnectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetConnection: Pointer;

function WNetGetConnection;
begin
  GetProcedureAddress(_WNetGetConnection, mpr, 'WNetGetConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetConnection]
  end;
end;
{$ELSE}
function WNetGetConnection; external mpr name 'WNetGetConnectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetConnection: Pointer;

function WNetGetConnection;
begin
  GetProcedureAddress(_WNetGetConnection, mpr, 'WNetGetConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetConnection]
  end;
end;
{$ELSE}
function WNetGetConnection; external mpr name 'WNetGetConnectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetRestoreConnectionA: Pointer;

function WNetRestoreConnectionA;
begin
  GetProcedureAddress(_WNetRestoreConnectionA, mpr, 'WNetRestoreConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetRestoreConnectionA]
  end;
end;
{$ELSE}
function WNetRestoreConnectionA; external mpr name 'WNetRestoreConnectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetRestoreConnectionW: Pointer;

function WNetRestoreConnectionW;
begin
  GetProcedureAddress(_WNetRestoreConnectionW, mpr, 'WNetRestoreConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetRestoreConnectionW]
  end;
end;
{$ELSE}
function WNetRestoreConnectionW; external mpr name 'WNetRestoreConnectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetRestoreConnection: Pointer;

function WNetRestoreConnection;
begin
  GetProcedureAddress(_WNetRestoreConnection, mpr, 'WNetRestoreConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetRestoreConnection]
  end;
end;
{$ELSE}
function WNetRestoreConnection; external mpr name 'WNetRestoreConnectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetRestoreConnection: Pointer;

function WNetRestoreConnection;
begin
  GetProcedureAddress(_WNetRestoreConnection, mpr, 'WNetRestoreConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetRestoreConnection]
  end;
end;
{$ELSE}
function WNetRestoreConnection; external mpr name 'WNetRestoreConnectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetUseConnectionA: Pointer;

function WNetUseConnectionA;
begin
  GetProcedureAddress(_WNetUseConnectionA, mpr, 'WNetUseConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetUseConnectionA]
  end;
end;
{$ELSE}
function WNetUseConnectionA; external mpr name 'WNetUseConnectionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetUseConnectionW: Pointer;

function WNetUseConnectionW;
begin
  GetProcedureAddress(_WNetUseConnectionW, mpr, 'WNetUseConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetUseConnectionW]
  end;
end;
{$ELSE}
function WNetUseConnectionW; external mpr name 'WNetUseConnectionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetUseConnection: Pointer;

function WNetUseConnection;
begin
  GetProcedureAddress(_WNetUseConnection, mpr, 'WNetUseConnectionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetUseConnection]
  end;
end;
{$ELSE}
function WNetUseConnection; external mpr name 'WNetUseConnectionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetUseConnection: Pointer;

function WNetUseConnection;
begin
  GetProcedureAddress(_WNetUseConnection, mpr, 'WNetUseConnectionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetUseConnection]
  end;
end;
{$ELSE}
function WNetUseConnection; external mpr name 'WNetUseConnectionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetConnectionDialog: Pointer;

function WNetConnectionDialog;
begin
  GetProcedureAddress(_WNetConnectionDialog, mpr, 'WNetConnectionDialog');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetConnectionDialog]
  end;
end;
{$ELSE}
function WNetConnectionDialog; external mpr name 'WNetConnectionDialog';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetDisconnectDialog: Pointer;

function WNetDisconnectDialog;
begin
  GetProcedureAddress(_WNetDisconnectDialog, mpr, 'WNetDisconnectDialog');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetDisconnectDialog]
  end;
end;
{$ELSE}
function WNetDisconnectDialog; external mpr name 'WNetDisconnectDialog';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetConnectionDialog1A: Pointer;

function WNetConnectionDialog1A;
begin
  GetProcedureAddress(_WNetConnectionDialog1A, mpr, 'WNetConnectionDialog1A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetConnectionDialog1A]
  end;
end;
{$ELSE}
function WNetConnectionDialog1A; external mpr name 'WNetConnectionDialog1A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetConnectionDialog1W: Pointer;

function WNetConnectionDialog1W;
begin
  GetProcedureAddress(_WNetConnectionDialog1W, mpr, 'WNetConnectionDialog1W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetConnectionDialog1W]
  end;
end;
{$ELSE}
function WNetConnectionDialog1W; external mpr name 'WNetConnectionDialog1W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetConnectionDialog1: Pointer;

function WNetConnectionDialog1;
begin
  GetProcedureAddress(_WNetConnectionDialog1, mpr, 'WNetConnectionDialog1W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetConnectionDialog1]
  end;
end;
{$ELSE}
function WNetConnectionDialog1; external mpr name 'WNetConnectionDialog1W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetConnectionDialog1: Pointer;

function WNetConnectionDialog1;
begin
  GetProcedureAddress(_WNetConnectionDialog1, mpr, 'WNetConnectionDialog1A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetConnectionDialog1]
  end;
end;
{$ELSE}
function WNetConnectionDialog1; external mpr name 'WNetConnectionDialog1A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetDisconnectDialog1A: Pointer;

function WNetDisconnectDialog1A;
begin
  GetProcedureAddress(_WNetDisconnectDialog1A, mpr, 'WNetDisconnectDialog1A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetDisconnectDialog1A]
  end;
end;
{$ELSE}
function WNetDisconnectDialog1A; external mpr name 'WNetDisconnectDialog1A';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetDisconnectDialog1W: Pointer;

function WNetDisconnectDialog1W;
begin
  GetProcedureAddress(_WNetDisconnectDialog1W, mpr, 'WNetDisconnectDialog1W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetDisconnectDialog1W]
  end;
end;
{$ELSE}
function WNetDisconnectDialog1W; external mpr name 'WNetDisconnectDialog1W';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetDisconnectDialog1: Pointer;

function WNetDisconnectDialog1;
begin
  GetProcedureAddress(_WNetDisconnectDialog1, mpr, 'WNetDisconnectDialog1W');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetDisconnectDialog1]
  end;
end;
{$ELSE}
function WNetDisconnectDialog1; external mpr name 'WNetDisconnectDialog1W';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetDisconnectDialog1: Pointer;

function WNetDisconnectDialog1;
begin
  GetProcedureAddress(_WNetDisconnectDialog1, mpr, 'WNetDisconnectDialog1A');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetDisconnectDialog1]
  end;
end;
{$ELSE}
function WNetDisconnectDialog1; external mpr name 'WNetDisconnectDialog1A';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetOpenEnumA: Pointer;

function WNetOpenEnumA;
begin
  GetProcedureAddress(_WNetOpenEnumA, mpr, 'WNetOpenEnumA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetOpenEnumA]
  end;
end;
{$ELSE}
function WNetOpenEnumA; external mpr name 'WNetOpenEnumA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetOpenEnumW: Pointer;

function WNetOpenEnumW;
begin
  GetProcedureAddress(_WNetOpenEnumW, mpr, 'WNetOpenEnumW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetOpenEnumW]
  end;
end;
{$ELSE}
function WNetOpenEnumW; external mpr name 'WNetOpenEnumW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetOpenEnum: Pointer;

function WNetOpenEnum;
begin
  GetProcedureAddress(_WNetOpenEnum, mpr, 'WNetOpenEnumW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetOpenEnum]
  end;
end;
{$ELSE}
function WNetOpenEnum; external mpr name 'WNetOpenEnumW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetOpenEnum: Pointer;

function WNetOpenEnum;
begin
  GetProcedureAddress(_WNetOpenEnum, mpr, 'WNetOpenEnumA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetOpenEnum]
  end;
end;
{$ELSE}
function WNetOpenEnum; external mpr name 'WNetOpenEnumA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetEnumResourceA: Pointer;

function WNetEnumResourceA;
begin
  GetProcedureAddress(_WNetEnumResourceA, mpr, 'WNetEnumResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetEnumResourceA]
  end;
end;
{$ELSE}
function WNetEnumResourceA; external mpr name 'WNetEnumResourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetEnumResourceW: Pointer;

function WNetEnumResourceW;
begin
  GetProcedureAddress(_WNetEnumResourceW, mpr, 'WNetEnumResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetEnumResourceW]
  end;
end;
{$ELSE}
function WNetEnumResourceW; external mpr name 'WNetEnumResourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetEnumResource: Pointer;

function WNetEnumResource;
begin
  GetProcedureAddress(_WNetEnumResource, mpr, 'WNetEnumResourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetEnumResource]
  end;
end;
{$ELSE}
function WNetEnumResource; external mpr name 'WNetEnumResourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetEnumResource: Pointer;

function WNetEnumResource;
begin
  GetProcedureAddress(_WNetEnumResource, mpr, 'WNetEnumResourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetEnumResource]
  end;
end;
{$ELSE}
function WNetEnumResource; external mpr name 'WNetEnumResourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetCloseEnum: Pointer;

function WNetCloseEnum;
begin
  GetProcedureAddress(_WNetCloseEnum, mpr, 'WNetCloseEnum');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetCloseEnum]
  end;
end;
{$ELSE}
function WNetCloseEnum; external mpr name 'WNetCloseEnum';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceParentA: Pointer;

function WNetGetResourceParentA;
begin
  GetProcedureAddress(_WNetGetResourceParentA, mpr, 'WNetGetResourceParentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceParentA]
  end;
end;
{$ELSE}
function WNetGetResourceParentA; external mpr name 'WNetGetResourceParentA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceParentW: Pointer;

function WNetGetResourceParentW;
begin
  GetProcedureAddress(_WNetGetResourceParentW, mpr, 'WNetGetResourceParentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceParentW]
  end;
end;
{$ELSE}
function WNetGetResourceParentW; external mpr name 'WNetGetResourceParentW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceParent: Pointer;

function WNetGetResourceParent;
begin
  GetProcedureAddress(_WNetGetResourceParent, mpr, 'WNetGetResourceParentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceParent]
  end;
end;
{$ELSE}
function WNetGetResourceParent; external mpr name 'WNetGetResourceParentW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceParent: Pointer;

function WNetGetResourceParent;
begin
  GetProcedureAddress(_WNetGetResourceParent, mpr, 'WNetGetResourceParentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceParent]
  end;
end;
{$ELSE}
function WNetGetResourceParent; external mpr name 'WNetGetResourceParentA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceInformationA: Pointer;

function WNetGetResourceInformationA;
begin
  GetProcedureAddress(_WNetGetResourceInformationA, mpr, 'WNetGetResourceInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceInformationA]
  end;
end;
{$ELSE}
function WNetGetResourceInformationA; external mpr name 'WNetGetResourceInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceInformationW: Pointer;

function WNetGetResourceInformationW;
begin
  GetProcedureAddress(_WNetGetResourceInformationW, mpr, 'WNetGetResourceInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceInformationW]
  end;
end;
{$ELSE}
function WNetGetResourceInformationW; external mpr name 'WNetGetResourceInformationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceInformation: Pointer;

function WNetGetResourceInformation;
begin
  GetProcedureAddress(_WNetGetResourceInformation, mpr, 'WNetGetResourceInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceInformation]
  end;
end;
{$ELSE}
function WNetGetResourceInformation; external mpr name 'WNetGetResourceInformationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetResourceInformation: Pointer;

function WNetGetResourceInformation;
begin
  GetProcedureAddress(_WNetGetResourceInformation, mpr, 'WNetGetResourceInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetResourceInformation]
  end;
end;
{$ELSE}
function WNetGetResourceInformation; external mpr name 'WNetGetResourceInformationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUniversalNameA: Pointer;

function WNetGetUniversalNameA;
begin
  GetProcedureAddress(_WNetGetUniversalNameA, mpr, 'WNetGetUniversalNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUniversalNameA]
  end;
end;
{$ELSE}
function WNetGetUniversalNameA; external mpr name 'WNetGetUniversalNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUniversalNameW: Pointer;

function WNetGetUniversalNameW;
begin
  GetProcedureAddress(_WNetGetUniversalNameW, mpr, 'WNetGetUniversalNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUniversalNameW]
  end;
end;
{$ELSE}
function WNetGetUniversalNameW; external mpr name 'WNetGetUniversalNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUniversalName: Pointer;

function WNetGetUniversalName;
begin
  GetProcedureAddress(_WNetGetUniversalName, mpr, 'WNetGetUniversalNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUniversalName]
  end;
end;
{$ELSE}
function WNetGetUniversalName; external mpr name 'WNetGetUniversalNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUniversalName: Pointer;

function WNetGetUniversalName;
begin
  GetProcedureAddress(_WNetGetUniversalName, mpr, 'WNetGetUniversalNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUniversalName]
  end;
end;
{$ELSE}
function WNetGetUniversalName; external mpr name 'WNetGetUniversalNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUserA: Pointer;

function WNetGetUserA;
begin
  GetProcedureAddress(_WNetGetUserA, mpr, 'WNetGetUserA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUserA]
  end;
end;
{$ELSE}
function WNetGetUserA; external mpr name 'WNetGetUserA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUserW: Pointer;

function WNetGetUserW;
begin
  GetProcedureAddress(_WNetGetUserW, mpr, 'WNetGetUserW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUserW]
  end;
end;
{$ELSE}
function WNetGetUserW; external mpr name 'WNetGetUserW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUser: Pointer;

function WNetGetUser;
begin
  GetProcedureAddress(_WNetGetUser, mpr, 'WNetGetUserW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUser]
  end;
end;
{$ELSE}
function WNetGetUser; external mpr name 'WNetGetUserW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetUser: Pointer;

function WNetGetUser;
begin
  GetProcedureAddress(_WNetGetUser, mpr, 'WNetGetUserA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetUser]
  end;
end;
{$ELSE}
function WNetGetUser; external mpr name 'WNetGetUserA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetProviderNameA: Pointer;

function WNetGetProviderNameA;
begin
  GetProcedureAddress(_WNetGetProviderNameA, mpr, 'WNetGetProviderNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetProviderNameA]
  end;
end;
{$ELSE}
function WNetGetProviderNameA; external mpr name 'WNetGetProviderNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetProviderNameW: Pointer;

function WNetGetProviderNameW;
begin
  GetProcedureAddress(_WNetGetProviderNameW, mpr, 'WNetGetProviderNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetProviderNameW]
  end;
end;
{$ELSE}
function WNetGetProviderNameW; external mpr name 'WNetGetProviderNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetProviderName: Pointer;

function WNetGetProviderName;
begin
  GetProcedureAddress(_WNetGetProviderName, mpr, 'WNetGetProviderNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetProviderName]
  end;
end;
{$ELSE}
function WNetGetProviderName; external mpr name 'WNetGetProviderNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetProviderName: Pointer;

function WNetGetProviderName;
begin
  GetProcedureAddress(_WNetGetProviderName, mpr, 'WNetGetProviderNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetProviderName]
  end;
end;
{$ELSE}
function WNetGetProviderName; external mpr name 'WNetGetProviderNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetNetworkInformationA: Pointer;

function WNetGetNetworkInformationA;
begin
  GetProcedureAddress(_WNetGetNetworkInformationA, mpr, 'WNetGetNetworkInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetNetworkInformationA]
  end;
end;
{$ELSE}
function WNetGetNetworkInformationA; external mpr name 'WNetGetNetworkInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetNetworkInformationW: Pointer;

function WNetGetNetworkInformationW;
begin
  GetProcedureAddress(_WNetGetNetworkInformationW, mpr, 'WNetGetNetworkInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetNetworkInformationW]
  end;
end;
{$ELSE}
function WNetGetNetworkInformationW; external mpr name 'WNetGetNetworkInformationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetNetworkInformation: Pointer;

function WNetGetNetworkInformation;
begin
  GetProcedureAddress(_WNetGetNetworkInformation, mpr, 'WNetGetNetworkInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetNetworkInformation]
  end;
end;
{$ELSE}
function WNetGetNetworkInformation; external mpr name 'WNetGetNetworkInformationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetNetworkInformation: Pointer;

function WNetGetNetworkInformation;
begin
  GetProcedureAddress(_WNetGetNetworkInformation, mpr, 'WNetGetNetworkInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetNetworkInformation]
  end;
end;
{$ELSE}
function WNetGetNetworkInformation; external mpr name 'WNetGetNetworkInformationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetLastErrorA: Pointer;

function WNetGetLastErrorA;
begin
  GetProcedureAddress(_WNetGetLastErrorA, mpr, 'WNetGetLastErrorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetLastErrorA]
  end;
end;
{$ELSE}
function WNetGetLastErrorA; external mpr name 'WNetGetLastErrorA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetLastErrorW: Pointer;

function WNetGetLastErrorW;
begin
  GetProcedureAddress(_WNetGetLastErrorW, mpr, 'WNetGetLastErrorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetLastErrorW]
  end;
end;
{$ELSE}
function WNetGetLastErrorW; external mpr name 'WNetGetLastErrorW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetLastError: Pointer;

function WNetGetLastError;
begin
  GetProcedureAddress(_WNetGetLastError, mpr, 'WNetGetLastErrorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetLastError]
  end;
end;
{$ELSE}
function WNetGetLastError; external mpr name 'WNetGetLastErrorW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WNetGetLastError: Pointer;

function WNetGetLastError;
begin
  GetProcedureAddress(_WNetGetLastError, mpr, 'WNetGetLastErrorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WNetGetLastError]
  end;
end;
{$ELSE}
function WNetGetLastError; external mpr name 'WNetGetLastErrorA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MultinetGetConnectionPerfA: Pointer;

function MultinetGetConnectionPerformanceA;
begin
  GetProcedureAddress(_MultinetGetConnectionPerfA, mpr, 'MultinetGetConnectionPerformanceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MultinetGetConnectionPerfA]
  end;
end;
{$ELSE}
function MultinetGetConnectionPerformanceA; external mpr name 'MultinetGetConnectionPerformanceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MultinetGetConnectionPerfW: Pointer;

function MultinetGetConnectionPerformanceW;
begin
  GetProcedureAddress(_MultinetGetConnectionPerfW, mpr, 'MultinetGetConnectionPerformanceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MultinetGetConnectionPerfW]
  end;
end;
{$ELSE}
function MultinetGetConnectionPerformanceW; external mpr name 'MultinetGetConnectionPerformanceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MultinetGetConnectionPerf: Pointer;

function MultinetGetConnectionPerformance;
begin
  GetProcedureAddress(_MultinetGetConnectionPerf, mpr, 'MultinetGetConnectionPerformanceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MultinetGetConnectionPerf]
  end;
end;
{$ELSE}
function MultinetGetConnectionPerformance; external mpr name 'MultinetGetConnectionPerformanceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MultinetGetConnectionPerf: Pointer;

function MultinetGetConnectionPerformance;
begin
  GetProcedureAddress(_MultinetGetConnectionPerf, mpr, 'MultinetGetConnectionPerformanceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MultinetGetConnectionPerf]
  end;
end;
{$ELSE}
function MultinetGetConnectionPerformance; external mpr name 'MultinetGetConnectionPerformanceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

end.
