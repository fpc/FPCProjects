{******************************************************************************}
{                                                       	               }
{ Winsock2 DecNET API interface Unit for Object Pascal                         }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: ws2dnet.h, released June 2000. The original Pascal     }
{ code is: WS2dnet.pas, released December 2000. The initial developer of the   }
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

unit JwaWS2dnet;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "ws2dnet.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinSock2;

//************************************************************************
//  Winsock V2.0  DECnet definitions		File: WS2DNET.H
//************************************************************************

//
//  DECnet WinSock Definitions
//

const
  DNPROTO_NSP = 1; // DECnet NSP transport protocol
  DNPROTO_RAW = 255;

  DN_MAXADDL   = 20; // maximum DECnet address length
  DN_ADDL      = 2; // DECnet NSP address length
  DN_MAXOPTL   = 16; // Maximum DECnet optional data length
  DN_MAXOBJL   = 16; // Maximum DECnet object name length
  DN_MAXACCL   = 39; // Maximum DECnet access string length
  DN_MAXALIASL = 128; // Maximum DECnet alias string length
  DN_MAXNODEL  = 7; // Maximum DECnet Phase IV node string length

// DECnet Extension Function Identifiers

  WS2API_DECNET_dnet_addr     = 1;
  WS2API_DECNET_dnet_eof      = 2;
  WS2API_DECNET_dnet_getacc   = 3;
  WS2API_DECNET_dnet_getalias = 4;
  WS2API_DECNET_dnet_htoa     = 5;
  WS2API_DECNET_dnet_ntoa     = 6;
  WS2API_DECNET_getnodeadd    = 7;
  WS2API_DECNET_getnodebyaddr = 8;
  WS2API_DECNET_getnodebyname = 9;
  WS2API_DECNET_getnodename   = 10;
  WS2API_DECNET_MAX           = 10;

//
//  DECnet address structure
//

type
  dn_naddr = packed record
    a_len: Word;		               // length of address
    a_addr: array [0..DN_MAXADDL - 1] of Byte; // address as bytes
  end;
  DNNADDR = dn_naddr;
  LPDNNADDR = ^DNNADDR;
  TDNNAddr = DNNADDR;
  PDNNAddr = LPDNNADDR;  

//
//  DECnet socket address structure
//

  sockaddr_dn = packed record
    sdn_family: Word;	    // AF_DECnet
    sdn_flags: Byte;	    // flags
    sdn_objnum: Byte;	    // object number
    sdn_objnamel: Word;	// size of object name
    sdn_objname: array [0..DN_MAXOBJL - 1] of Char;	// object name
    sdn_add: dn_naddr;	    // node address
  end;
  SOCKADDRDN = sockaddr_dn;
  LPSOCKADDRDN = ^SOCKADDRDN;
  TSockAddrDN = SOCKADDRDN;
  PSockAddrDN = LPSOCKADDRDN;

//  Common DECnet object numbers (used in sockaddr_dn)

const
  DNOBJECT_FAL	= 17;		//    = file access listener;
  DNOBJECT_NICE	= 19;		//   = network management;
  DNOBJECT_DTERM  = 23;		//  = remote terminals;
  DNOBJECT_MIRROR = 25;		// = loopback mirror;
  DNOBJECT_EVR = 26;		//    = event receiver;
  DNOBJECT_MAIL11 = 27;		// = Mail-11 service;
  DNOBJECT_PHONE = 29;		//  = phone utility;
  DNOBJECT_CTERM = 42;		//  = command terminals;
  DNOBJECT_DTR = 63;		//    = data test receiver;

//
//  DECnet node structure
//

type
  nodeent_f = packed record
    n_name: PChar;      // name of node
    n_addrtype: Word;	// node address type
    n_length: Word;	// address length
    n_addr: PChar;	// address
    n_params: PChar;	// node parameters
    n_reserved: array [0..15] of Byte;	// Reserved
  end;
  NODEENTF = nodeent_f;
  LPNODEENTF = ^NODEENTF;
  TNodeEntF = NODEENTF;
  PNodeEntF = LPNODEENTF;

//
//  DECnet set/get DSO_CONDATA, DSO_DISDATA (optional data) structure
//

  optdata_dn = packed record
    opt_status: Word;	          // extended status return
    opt_optl: Word;	          // user data length
    opt_data: array [0..DN_MAXOPTL - 1] of Byte; // user data
  end;
  OPTDATADN = optdata_dn;
  LPOPTDATADN = ^OPTDATADN;
  TOptDataDN = OPTDATADN;
  POptDataDN = LPOPTDATADN;

//
//  DECnet set/get DSO_CONACCESS access (control data) structure
//

  accessdata_dn = packed record
    acc_accl: Word;	            // account string length
    acc_acc: array [0..DN_MAXACCL] of Byte;	// account string
    acc_passl: Word;	            // password string length
    acc_pass: array [0..DN_MAXACCL] of Byte;	// password string
    acc_userl: Word;	            // user string length
    acc_user: array [0..DN_MAXACCL] of Byte;	// user string
  end;
  ACCESSDATADN = accessdata_dn;
  LPACCESSDATADN = ^ACCESSDATADN;
  TAccessDataDN = ACCESSDATADN;
  PAccessDataDN = LPACCESSDATADN;  

//
//  DECnet call data structure (concatenated access and optional data)
//

  calldata_dn = packed record
    optdata_dn: optdata_dn;
    accessdata_dn: accessdata_dn;
  end;
  CALLDATADN = calldata_dn;
  LPCALLDATADN = ^CALLDATADN;
  TCallDataDN = CALLDATADN;
  PCallDataDN = LPCALLDATADN;  

//
//  DECnet incoming access control structure
//

  dnet_accent = packed record
    dac_status: Byte;      // Reserved
    dac_type: Byte;        // DN_NONE, etc.
    dac_username: array [0..DN_MAXACCL] of Char;
    dac_password: array [0..DN_MAXACCL] of Char;
  end;
  DNETACCENT = dnet_accent;
  LPDNETACCENT = ^DNETACCENT;
  TDNetAccent = DNETACCENT;
  PDNetAccent = LPDNETACCENT;  

const
  DN_NONE = $00;
  DN_RO   = $01;
  DN_WO   = $02;
  DN_RW   = $03;

// DECnet logical link information structure

type
  linkinfo_dn = packed record
    idn_segsize: Word;   // segment size for link
    idn_linkstate: Byte; // logical link state
  end;
  LINKINFODN = linkinfo_dn;
  LPLINKINFODN = ^LINKINFODN;
  TLinkInfoDN = LINKINFODN;
  PLinkInfoDN = LPLINKINFODN;

const
  SO_LINKINFO      = 7; // set/get link information
  LL_INACTIVE      = 0; // logical link inactive
  LL_CONNECTING    = 1; // logical link connecting
  LL_RUNNING       = 2; // logical link running
  LL_DISCONNECTING = 3; // logical link disconnecting

//*********************************************************************
//  DECnet WinSock 2 extended function prototypes
//*********************************************************************

(*
MVB TODO: Extension functions must be dynamically queried for using WSAIoctl.

function dnet_addr(cp: PChar): PDNAddr; stdcall;
function dnet_eof(s: SOCKET): Integer; stdcall;
function dnet_getacc(const nacc: TDNetAccent): PDNetAccent; stdcall;
function dnet_getalias(P: PChar): PChar; stdcall;
function dnet_htoa(const add: TDNNAddr): PChar; stdcall;
function dnet_ntoa(const add: TDNNAddr): PChar; stdcall;
function getnodeadd: PDNNAddr; stdcall;
function getnodebyaddr(addr: PChar; len, type_: Integer): PNodeEntF; stdcall;
function getnodebyname(name: PChar): PNodeEntF; stdcall;
function getnodename: PChar; stdcall;
*)

// typedefs for C++ compatability

type
  LPDNETADDR = function (cp: PChar): PDNNAddr; stdcall;
  TDNetAddr = LPDNETADDR;
  LPDNETEOF = function (s: TSocket): Integer; stdcall;
  TDNetEOF = LPDNETEOF;
  LPDNETGETACC = function (const nacc: TDNetAccent): PDNetAccent; stdcall;
  TDNetGetAcc = LPDNETGETACC;
  LPDNETGETALIAS = function (P: PChar): PChar; stdcall;
  TDNetGetAlias = LPDNETGETALIAS;
  LPDNETHTOA = function (const add: TDNNAddr): PChar; stdcall;
  TDNetHToA = LPDNETHTOA;
  LPDNETNTOA = function (const add: TDNNAddr): PChar; stdcall;
  TDNetNToA = LPDNETNTOA;
  LPGETNODEADD = function: PDNNAddr; stdcall;
  TGetNodeAdd = LPGETNODEADD;
  LPGETNODEBYADDR = function (addr: PChar; len, type_: Integer): PNodeEntF; stdcall;
  TGetNodeByAddr = LPGETNODEBYADDR;
  LPGETNODEBYNAME = function (name: PChar): PNodeEntF; stdcall;
  TGetNodeByName = LPGETNODEBYNAME;
  LPGETNODENAME = function: PChar; stdcall;
  TGetNodeName = LPGETNODENAME;

implementation

end.
