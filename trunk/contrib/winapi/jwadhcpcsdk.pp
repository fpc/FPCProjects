{******************************************************************************}
{                                                       	               }
{ DHCP Client API interface unit for Object Pascal                             }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: dhcpcsdk.h, released June 2000. The original Pascal    }
{ code is: DhcpCSdk.pas, released December 2000. The initial developer of the  }
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

unit JwaDhcpCSdk;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "dhcpcsdk.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

//
// DHCP Standard Options.
//

const
  OPTION_PAD                 = 0;
  OPTION_SUBNET_MASK         = 1;
  OPTION_TIME_OFFSET         = 2;
  OPTION_ROUTER_ADDRESS      = 3;
  OPTION_TIME_SERVERS        = 4;
  OPTION_IEN116_NAME_SERVERS = 5;
  OPTION_DOMAIN_NAME_SERVERS = 6;
  OPTION_LOG_SERVERS         = 7;
  OPTION_COOKIE_SERVERS      = 8;
  OPTION_LPR_SERVERS         = 9;
  OPTION_IMPRESS_SERVERS     = 10;
  OPTION_RLP_SERVERS         = 11;
  OPTION_HOST_NAME           = 12;
  OPTION_BOOT_FILE_SIZE      = 13;
  OPTION_MERIT_DUMP_FILE     = 14;
  OPTION_DOMAIN_NAME         = 15;
  OPTION_SWAP_SERVER         = 16;
  OPTION_ROOT_DISK           = 17;
  OPTION_EXTENSIONS_PATH     = 18;

//
// IP layer parameters - per host
//

  OPTION_BE_A_ROUTER              = 19;
  OPTION_NON_LOCAL_SOURCE_ROUTING = 20;
  OPTION_POLICY_FILTER_FOR_NLSR   = 21;
  OPTION_MAX_REASSEMBLY_SIZE      = 22;
  OPTION_DEFAULT_TTL              = 23;
  OPTION_PMTU_AGING_TIMEOUT       = 24;
  OPTION_PMTU_PLATEAU_TABLE       = 25;

//
// Link layer parameters - per interface.
//

  OPTION_MTU                      = 26;
  OPTION_ALL_SUBNETS_MTU          = 27;
  OPTION_BROADCAST_ADDRESS        = 28;
  OPTION_PERFORM_MASK_DISCOVERY   = 29;
  OPTION_BE_A_MASK_SUPPLIER       = 30;
  OPTION_PERFORM_ROUTER_DISCOVERY = 31;
  OPTION_ROUTER_SOLICITATION_ADDR = 32;
  OPTION_STATIC_ROUTES            = 33;
  OPTION_TRAILERS                 = 34;
  OPTION_ARP_CACHE_TIMEOUT        = 35;
  OPTION_ETHERNET_ENCAPSULATION   = 36;

//
// TCP Paramters - per host
//

  OPTION_TTL                  = 37;
  OPTION_KEEP_ALIVE_INTERVAL  = 38;
  OPTION_KEEP_ALIVE_DATA_SIZE = 39;

//
// Application Layer Parameters
//

  OPTION_NETWORK_INFO_SERVICE_DOM = 40;
  OPTION_NETWORK_INFO_SERVERS     = 41;
  OPTION_NETWORK_TIME_SERVERS     = 42;

//
// Vender specific information option
//

  OPTION_VENDOR_SPEC_INFO = 43;

//
// NetBIOS over TCP/IP Name server option
//

  OPTION_NETBIOS_NAME_SERVER     = 44;
  OPTION_NETBIOS_DATAGRAM_SERVER = 45;
  OPTION_NETBIOS_NODE_TYPE       = 46;
  OPTION_NETBIOS_SCOPE_OPTION    = 47;

//
// X Window System Options.
//

  OPTION_XWINDOW_FONT_SERVER     = 48;
  OPTION_XWINDOW_DISPLAY_MANAGER = 49;

//
// Other extensions
//

  OPTION_REQUESTED_ADDRESS      = 50;
  OPTION_LEASE_TIME             = 51;
  OPTION_OK_TO_OVERLAY          = 52;
  OPTION_MESSAGE_TYPE           = 53;
  OPTION_SERVER_IDENTIFIER      = 54;
  OPTION_PARAMETER_REQUEST_LIST = 55;
  OPTION_MESSAGE                = 56;
  OPTION_MESSAGE_LENGTH         = 57;
  OPTION_RENEWAL_TIME           = 58; // T1
  OPTION_REBIND_TIME            = 59; // T2
  OPTION_CLIENT_CLASS_INFO      = 60;
  OPTION_CLIENT_ID              = 61;

  OPTION_TFTP_SERVER_NAME = 66;
  OPTION_BOOTFILE_NAME    = 67;

  OPTION_END = 255;

type
  _DHCPAPI_PARAMS = record
    Flags: ULONG;      // for future use
    OptionId: ULONG;   // what option is this?
    IsVendor: BOOL;    // is this vendor specific?
    Data: LPBYTE;      // the actual data
    nBytesData: DWORD; // how many bytes of data are there in Data?
  end;
  DHCPAPI_PARAMS = _DHCPAPI_PARAMS;
  LPDHCPAPI_PARAMS = ^DHCPAPI_PARAMS;
  PDHCPAPI_PARAMS = ^DHCPAPI_PARAMS;
  TDhcpApiParams = DHCPAPI_PARAMS;
  PDhcpApiParams = PDHCPAPI_PARAMS;

  DHCPCAPI_PARAMS = DHCPAPI_PARAMS;
  PDHCPCAPI_PARAMS = ^DHCPCAPI_PARAMS;
  LPDHCPCAPI_PARAMS = ^DHCPCAPI_PARAMS;
  TDhcpCApiParams = DHCPCAPI_PARAMS;
  PDhcpCApiParams = PDHCPCAPI_PARAMS;

  _DHCPCAPI_PARAMS_ARRAY = record
    nParams: ULONG;            // size of array
    Params: PDHCPCAPI_PARAMS;  // actual array
  end;
  DHCPCAPI_PARAMS_ARRAY = _DHCPCAPI_PARAMS_ARRAY;
  LPDHCPCAPI_PARAMS_ARRAY = ^DHCPCAPI_PARAMS_ARRAY;
  PDHCPCAPI_PARAMS_ARRAY = ^DHCPCAPI_PARAMS_ARRAY;
  TDhcpcApiParamsArray = DHCPCAPI_PARAMS_ARRAY;
  PDhcpcApiParamsArray = PDHCPCAPI_PARAMS_ARRAY;

  _DHCPCAPI_CLASSID = record
    Flags: ULONG;      // must be zero currently.
    Data: LPBYTE;      // classid binary data.
    nBytesData: ULONG; // how many bytes of data are there?
  end;
  DHCPCAPI_CLASSID = _DHCPCAPI_CLASSID;
  LPDHCPCAPI_CLASSID = ^DHCPCAPI_CLASSID;
  PDHCPCAPI_CLASSID = ^DHCPCAPI_CLASSID;
  TDhcpcApiClassId = DHCPCAPI_CLASSID;
  PDhcpcApiClassId = PDHCPCAPI_CLASSID;

const
  DHCPCAPI_REQUEST_PERSISTENT   = $01; // request this options "permanently"
  DHCPCAPI_REQUEST_SYNCHRONOUS  = $02; // request and block on it
  DHCPCAPI_REQUEST_ASYNCHRONOUS = $04; // request and return, set event on completion
  DHCPCAPI_REQUEST_CANCEL       = $08; // cancel request
  DHCPCAPI_REQUEST_MASK         = $0F; // allowed flags..

function DhcpCApiInitialize(var Version: DWORD): DWORD; stdcall;

procedure DhcpCApiCleanup; stdcall;

function DhcpRequestParams(Flags: DWORD; Reserved: LPVOID; AdapterName: LPWSTR;
  ClassId: PDHCPCAPI_CLASSID; SendParams, RecdParams: DHCPCAPI_PARAMS_ARRAY;
  Buffer: LPBYTE; pSize: LPDWORD; RequestIdStr: LPWSTR): DWORD; stdcall;

function DhcpUndoRequestParams(Flags: DWORD; Reserved: LPVOID; AdapterName: LPWSTR;
  RequestIdStr: LPWSTR): DWORD; stdcall;

const
  DHCPCAPI_REGISTER_HANDLE_EVENT = $01; // handle returned is to an event

function DhcpRegisterParamChange(Flags: DWORD; Reserved: LPVOID; AdapterName: LPWSTR;
  ClassId: PDHCPCAPI_CLASSID; Params: DHCPCAPI_PARAMS_ARRAY; Handle: LPVOID): DWORD; stdcall;

const
  DHCPCAPI_DEREGISTER_HANDLE_EVENT = $01; // de-register handle that is an event

function DhcpDeRegisterParamChange(Flags: DWORD; Reserved, Event: LPVOID): DWORD; stdcall;

function DhcpRemoveDNSRegistrations: DWORD; stdcall;

implementation

const
  dhcpapi = 'dhcpcsvc.dll';


{$IFDEF DYNAMIC_LINK}
var
  _DhcpCApiInitialize: Pointer;

function DhcpCApiInitialize;
begin
  GetProcedureAddress(_DhcpCApiInitialize, dhcpapi, 'DhcpCApiInitialize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpCApiInitialize]
  end;
end;
{$ELSE}
function DhcpCApiInitialize; external dhcpapi name 'DhcpCApiInitialize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DhcpCApiCleanup: Pointer;

procedure DhcpCApiCleanup;
begin
  GetProcedureAddress(_DhcpCApiCleanup, dhcpapi, 'DhcpCApiCleanup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpCApiCleanup]
  end;
end;
{$ELSE}
procedure DhcpCApiCleanup; external dhcpapi name 'DhcpCApiCleanup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DhcpRequestParams: Pointer;

function DhcpRequestParams;
begin
  GetProcedureAddress(_DhcpRequestParams, dhcpapi, 'DhcpRequestParams');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpRequestParams]
  end;
end;
{$ELSE}
function DhcpRequestParams; external dhcpapi name 'DhcpRequestParams';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DhcpUndoRequestParams: Pointer;

function DhcpUndoRequestParams;
begin
  GetProcedureAddress(_DhcpUndoRequestParams, dhcpapi, 'DhcpUndoRequestParams');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpUndoRequestParams]
  end;
end;
{$ELSE}
function DhcpUndoRequestParams; external dhcpapi name 'DhcpUndoRequestParams';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DhcpRegisterParamChange: Pointer;

function DhcpRegisterParamChange;
begin
  GetProcedureAddress(_DhcpRegisterParamChange, dhcpapi, 'DhcpRegisterParamChange');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpRegisterParamChange]
  end;
end;
{$ELSE}
function DhcpRegisterParamChange; external dhcpapi name 'DhcpRegisterParamChange';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DhcpDeRegisterParamChange: Pointer;

function DhcpDeRegisterParamChange;
begin
  GetProcedureAddress(_DhcpDeRegisterParamChange, dhcpapi, 'DhcpDeRegisterParamChange');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpDeRegisterParamChange]
  end;
end;
{$ELSE}
function DhcpDeRegisterParamChange; external dhcpapi name 'DhcpDeRegisterParamChange';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DhcpRemoveDNSRegistrations: Pointer;

function DhcpRemoveDNSRegistrations;
begin
  GetProcedureAddress(_DhcpRemoveDNSRegistrations, dhcpapi, 'DhcpRemoveDNSRegistrations');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DhcpRemoveDNSRegistrations]
  end;
end;
{$ELSE}
function DhcpRemoveDNSRegistrations; external dhcpapi name 'DhcpRemoveDNSRegistrations';
{$ENDIF DYNAMIC_LINK}

end.
