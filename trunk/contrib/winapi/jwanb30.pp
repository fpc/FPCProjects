{******************************************************************************}
{                                                       	               }
{ Portable Netbios 3.0 API interface Unit for Object Pascal                    }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: Nb30.h, released June 2000. The original Pascal        }
{ code is: JwaN30.pas, released December 2000. The initial developer of the    }
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

unit JwaNb30;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "Nb30.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

(****************************************************************
 *                                                              *
 *              Data structure templates                        *
 *                                                              *
 ****************************************************************)

const
  NCBNAMSZ = 16;  // absolute length of a net name
  MAX_LANA = 254; // lana's in range 0 to MAX_LANA inclusive

//
// Network Control Block
//

type
  PNCB = ^NCB;

  TNcbPost = procedure (P: PNCB); stdcall;

  _NCB = record
    ncb_command: UCHAR;  // command code
    ncb_retcode: UCHAR;  // return code
    ncb_lsn: UCHAR;      // local session number
    ncb_num: UCHAR;      // number of our network name
    ncb_buffer: PChar;   // address of message buffer
    ncb_length: Word;    // size of message buffer
    ncb_callname: array [0..NCBNAMSZ - 1] of Char; // blank-padded name of remote
    ncb_name: array [0..NCBNAMSZ - 1] of Char;     // our blank-padded netname
    ncb_rto: UCHAR;      // rcv timeout/retry count
    ncb_sto: UCHAR;      // send timeout/sys timeout
    ncb_post: TNcbPost;  // POST routine address
    ncb_lana_num: UCHAR; // lana (adapter) number
    ncb_cmd_cplt: UCHAR; // 0xff => commmand pending
    {$IFDEF _WIN64}
    ncb_reserve: array [0..17] of Char; // reserved, used by BIOS
    {$ELSE}
    ncb_reserve: array [0..9] of Char;  // reserved, used by BIOS
    {$ENDIF}
    ncb_event: HANDLE;   // HANDLE to Win32 event which
                         // will be set to the signalled
                         // state when an ASYNCH command
                         // completes
  end;
  NCB = _NCB;
  TNcb = NCB;

//
//  Structure returned to the NCB command NCBASTAT is ADAPTER_STATUS followed
//  by an array of NAME_BUFFER structures.
//

  _ADAPTER_STATUS = record
    adapter_address: array [0..5] of UCHAR;
    rev_major: UCHAR;
    reserved0: UCHAR;
    adapter_type: UCHAR;
    rev_minor: UCHAR;
    duration: WORD;
    frmr_recv: WORD;
    frmr_xmit: WORD;
    iframe_recv_err: WORD;
    xmit_aborts: WORD;
    xmit_success: DWORD;
    recv_success: DWORD;
    iframe_xmit_err: WORD;
    recv_buff_unavail: WORD;
    t1_timeouts: WORD;
    ti_timeouts: WORD;
    reserved1: DWORD;
    free_ncbs: WORD;
    max_cfg_ncbs: WORD;
    max_ncbs: WORD;
    xmit_buf_unavail: WORD;
    max_dgram_size: WORD;
    pending_sess: WORD;
    max_cfg_sess: WORD;
    max_sess: WORD;
    max_sess_pkt_size: WORD;
    name_count: WORD;
  end;
  ADAPTER_STATUS = _ADAPTER_STATUS;
  PADAPTER_STATUS = ^ADAPTER_STATUS;
  TAdapterStatus = ADAPTER_STATUS;
  PAdapterStatus = PADAPTER_STATUS;

  _NAME_BUFFER = record
    name: array [0..NCBNAMSZ - 1] of Char;
    name_num: UCHAR;
    name_flags: UCHAR;
  end;
  NAME_BUFFER = _NAME_BUFFER;
  PNAME_BUFFER = ^NAME_BUFFER;
  TNameBuffer = NAME_BUFFER;
  PNameBuffer = PNAME_BUFFER;

//  values for name_flags bits.

const
  NAME_FLAGS_MASK = $87;

  GROUP_NAME  = $80;
  UNIQUE_NAME = $00;

  REGISTERING     = $00;
  REGISTERED      = $04;
  DEREGISTERED    = $05;
  DUPLICATE       = $06;
  DUPLICATE_DEREG = $07;

//
//  Structure returned to the NCB command NCBSSTAT is SESSION_HEADER followed
//  by an array of SESSION_BUFFER structures. If the NCB_NAME starts with an
//  asterisk then an array of these structures is returned containing the
//  status for all names.
//

type
  _SESSION_HEADER = record
    sess_name: UCHAR;
    num_sess: UCHAR;
    rcv_dg_outstanding: UCHAR;
    rcv_any_outstanding: UCHAR;
  end;
  SESSION_HEADER = _SESSION_HEADER;
  PSESSION_HEADER = ^SESSION_HEADER;
  TSessionHeader = SESSION_HEADER;
  PSessionHeader = PSESSION_HEADER;

  _SESSION_BUFFER = record
    lsn: UCHAR;
    state: UCHAR;
    local_name: array [0..NCBNAMSZ - 1] of UCHAR;
    remote_name: array [0..NCBNAMSZ - 1] of UCHAR;
    rcvs_outstanding: UCHAR;
    sends_outstanding: UCHAR;
  end;
  SESSION_BUFFER = _SESSION_BUFFER;
  PSESSION_BUFFER = ^SESSION_BUFFER;
  TSessionBuffer = SESSION_BUFFER;
  PSessionBuffer = PSESSION_BUFFER;

//  Values for state

const
  LISTEN_OUTSTANDING  = $01;
  CALL_PENDING        = $02;
  SESSION_ESTABLISHED = $03;
  HANGUP_PENDING      = $04;
  HANGUP_COMPLETE     = $05;
  SESSION_ABORTED     = $06;

//
//  Structure returned to the NCB command NCBENUM.
//
//  On a system containing lana's 0, 2 and 3, a structure with
//  length =3, lana[0]=0, lana[1]=2 and lana[2]=3 will be returned.
//

type
  _LANA_ENUM = record
    length: UCHAR; // Number of valid entries in lana[]
    lana: array [0..MAX_LANA] of UCHAR;
  end;
  LANA_ENUM = _LANA_ENUM;
  PLANA_ENUM = ^LANA_ENUM;
  TLanaEnum = LANA_ENUM;
  PLanaEnum = PLANA_ENUM;

//
//  Structure returned to the NCB command NCBFINDNAME is FIND_NAME_HEADER followed
//  by an array of FIND_NAME_BUFFER structures.
//

type
  _FIND_NAME_HEADER = record
    node_count: WORD;
    reserved: UCHAR;
    unique_group: UCHAR;
  end;
  FIND_NAME_HEADER = _FIND_NAME_HEADER;
  PFIND_NAME_HEADER = ^FIND_NAME_HEADER;
  TFindNameHeader = FIND_NAME_HEADER;
  PFindNameHeader = PFIND_NAME_HEADER;

  _FIND_NAME_BUFFER = record
    length: UCHAR;
    access_control: UCHAR;
    frame_control: UCHAR;
    destination_addr: array [0..5] of UCHAR;
    source_addr: array [0..5] of UCHAR;
    routing_info: array [0..17] of UCHAR;
  end;
  FIND_NAME_BUFFER = _FIND_NAME_BUFFER;
  PFIND_NAME_BUFFER = ^FIND_NAME_BUFFER;
  TFindNameBuffer = FIND_NAME_BUFFER;
  PFindNameBuffer = PFIND_NAME_BUFFER;

//
//  Structure provided with NCBACTION. The purpose of NCBACTION is to provide
//  transport specific extensions to netbios.
//

  _ACTION_HEADER = record
    transport_id: ULONG;
    action_code: USHORT;
    reserved: USHORT;
  end;
  ACTION_HEADER = _ACTION_HEADER;
  PACTION_HEADER = ^ACTION_HEADER;
  TActionHeader = ACTION_HEADER;
  PActionHeader = PACTION_HEADER;

//  Values for transport_id

const
  ALL_TRANSPORTS = 'M'#0#0#0;
  MS_NBF         = 'MNBF';

(****************************************************************
 *                                                              *
 *              Special values and constants                    *
 *                                                              *
 ****************************************************************)

//
//      NCB Command codes
//

const
  NCBCALL        = $10; // NCB CALL
  NCBLISTEN      = $11; // NCB LISTEN
  NCBHANGUP      = $12; // NCB HANG UP
  NCBSEND        = $14; // NCB SEND
  NCBRECV        = $15; // NCB RECEIVE
  NCBRECVANY     = $16; // NCB RECEIVE ANY
  NCBCHAINSEND   = $17; // NCB CHAIN SEND
  NCBDGSEND      = $20; // NCB SEND DATAGRAM
  NCBDGRECV      = $21; // NCB RECEIVE DATAGRAM
  NCBDGSENDBC    = $22; // NCB SEND BROADCAST DATAGRAM
  NCBDGRECVBC    = $23; // NCB RECEIVE BROADCAST DATAGRAM
  NCBADDNAME     = $30; // NCB ADD NAME
  NCBDELNAME     = $31; // NCB DELETE NAME
  NCBRESET       = $32; // NCB RESET
  NCBASTAT       = $33; // NCB ADAPTER STATUS
  NCBSSTAT       = $34; // NCB SESSION STATUS
  NCBCANCEL      = $35; // NCB CANCEL
  NCBADDGRNAME   = $36; // NCB ADD GROUP NAME
  NCBENUM        = $37; // NCB ENUMERATE LANA NUMBERS
  NCBUNLINK      = $70; // NCB UNLINK
  NCBSENDNA      = $71; // NCB SEND NO ACK
  NCBCHAINSENDNA = $72; // NCB CHAIN SEND NO ACK
  NCBLANSTALERT  = $73; // NCB LAN STATUS ALERT
  NCBACTION      = $77; // NCB ACTION
  NCBFINDNAME    = $78; // NCB FIND NAME
  NCBTRACE       = $79; // NCB TRACE

  ASYNCH = $80; // high bit set == asynchronous

//
//      NCB Return codes
//

  NRC_GOODRET = $00; // good return also returned when ASYNCH request accepted
  NRC_BUFLEN      = $01; // illegal buffer length
  NRC_ILLCMD      = $03; // illegal command
  NRC_CMDTMO      = $05; // command timed out
  NRC_INCOMP      = $06; // message incomplete, issue another command
  NRC_BADDR       = $07; // illegal buffer address
  NRC_SNUMOUT     = $08; // session number out of range
  NRC_NORES       = $09; // no resource available
  NRC_SCLOSED     = $0a; // session closed
  NRC_CMDCAN      = $0b; // command cancelled
  NRC_DUPNAME     = $0d; // duplicate name
  NRC_NAMTFUL     = $0e; // name table full
  NRC_ACTSES      = $0f; // no deletions, name has active sessions
  NRC_LOCTFUL     = $11; // local session table full
  NRC_REMTFUL     = $12; // remote session table full
  NRC_ILLNN       = $13; // illegal name number
  NRC_NOCALL      = $14; // no callname
  NRC_NOWILD      = $15; // cannot put * in NCB_NAME
  NRC_INUSE       = $16; // name in use on remote adapter
  NRC_NAMERR      = $17; // name deleted
  NRC_SABORT      = $18; // session ended abnormally
  NRC_NAMCONF     = $19; // name conflict detected
  NRC_IFBUSY      = $21; // interface busy, IRET before retrying
  NRC_TOOMANY     = $22; // too many commands outstanding, retry later
  NRC_BRIDGE      = $23; // ncb_lana_num field invalid
  NRC_CANOCCR     = $24; // command completed while cancel occurring
  NRC_CANCEL      = $26; // command not valid to cancel
  NRC_DUPENV      = $30; // name defined by anther local process
  NRC_ENVNOTDEF   = $34; // environment undefined. RESET required
  NRC_OSRESNOTAV  = $35; // required OS resources exhausted
  NRC_MAXAPPS     = $36; // max number of applications exceeded
  NRC_NOSAPS      = $37; // no saps available for netbios
  NRC_NORESOURCES = $38; // requested resources are not available
  NRC_INVADDRESS  = $39; // invalid ncb address or length > segment
  NRC_INVDDID     = $3B; // invalid NCB DDID
  NRC_LOCKFAIL    = $3C; // lock of user area failed
  NRC_OPENERR     = $3f; // NETBIOS not loaded
  NRC_SYSTEM      = $40; // system error

  NRC_PENDING = $ff; // asynchronous command is not yet finished

(****************************************************************
 *                                                              *
 *              main user entry point for NetBIOS 3.0           *
 *                                                              *
 * Usage: result = Netbios( pncb );                             *
 ****************************************************************)

function Netbios(pncb: PNCB): UCHAR; stdcall;

(****************************************************************
 *                                                              *
 *              Prefix for callback routines                    *
 *                                                              *
 * Usage in a declaration: NCB_POST MyPostRoutine( PNCB pncb ); *
 ****************************************************************)

// #define NCB_POST void CALLBACK

implementation


{$IFDEF DYNAMIC_LINK}
var
  _Netbios: Pointer;

function Netbios;
begin
  GetProcedureAddress(_Netbios, 'netapi32.dll', 'Netbios');
  asm
    mov esp, ebp
    pop ebp
    jmp [_Netbios]
  end;
end;
{$ELSE}
function Netbios; external 'netapi32.dll' name 'Netbios';
{$ENDIF DYNAMIC_LINK}

end.
