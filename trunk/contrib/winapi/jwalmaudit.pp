{******************************************************************************}
{                                                       	               }
{ Lan Manager Audit API interface Unit for Object Pascal                       }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: lmaudit.h, released November 2001. The original Pascal }
{ code is: LmAudit.pas, released Februari 2002. The initial developer of the   }
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

unit JwaLmAudit;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "lmaudit.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaLmCons, JwaWinType;

type
  _HLOG = record
    time: DWORD;
    last_flags: DWORD;
    offset: DWORD;
    rec_offset: DWORD;
  end;
  HLOG = _HLOG;
  LPHLOG = ^HLOG;
  PHLOG = ^HLOG;

const
  LOGFLAGS_FORWARD  = 0;
  LOGFLAGS_BACKWARD = $1;
  LOGFLAGS_SEEK     = $2;

//
// Function Prototypes - Audit
//

function NetAuditClear(server, backupfile, service: LPCWSTR): NET_API_STATUS; stdcall;

function NetAuditRead(server, service: LPCWSTR; auditloghandle: LPHLOG; offset: DWORD; reserved1: LPDWORD; reserved2, offsetflag: DWORD; var bufptr: LPBYTE; prefmaxlen: DWORD; bytesread, totalavailable: LPDWORD): NET_API_STATUS; stdcall;

function NetAuditWrite(type_: DWORD; buf: LPBYTE; numbytes: DWORD; service: LPCWSTR; reserved: LPBYTE): NET_API_STATUS; stdcall;

//
// Data Structures - Audit
//

type
  _AUDIT_ENTRY = record
    ae_len: DWORD;
    ae_reserved: DWORD;
    ae_time: DWORD;
    ae_type: DWORD;
    ae_data_offset: DWORD; // Offset from beginning address of audit_entry
    ae_data_size: DWORD; // byte count of ae_data area (not incl pad).
  end;
  AUDIT_ENTRY = _AUDIT_ENTRY;
  LPAUDIT_ENTRY = ^AUDIT_ENTRY;
  PAUDIT_ENTRY = ^AUDIT_ENTRY;
  TAuditEntry = AUDIT_ENTRY;
  PAuditEntry = PAUDIT_ENTRY;

{$DEFINE REVISED_AUDIT_ENTRY_STRUCT}

  _AE_SRVSTATUS = record
    ae_sv_status: DWORD	;
  end;
  //AE_SRVSTATUS = _AE_SRVSTATUS;
  //{$EXTERNALSYM AE_SRVSTATUS}
  LPAE_SRVSTATUS = ^_AE_SRVSTATUS;
  PAE_SRVSTATUS = ^_AE_SRVSTATUS;
  TAeSrvStatus = _AE_SRVSTATUS;
  PAeSrvStatus = PAE_SRVSTATUS;

  _AE_SESSLOGON = record
    ae_so_compname: DWORD;
    ae_so_username: DWORD;
    ae_so_privilege: DWORD;
  end;
  //AE_SESSLOGON = _AE_SESSLOGON;
  //{$EXTERNALSYM AE_SESSLOGON}
  LPAE_SESSLOGON = ^_AE_SESSLOGON;
  PAE_SESSLOGON = ^_AE_SESSLOGON;
  TAeSessLogon = _AE_SESSLOGON;
  PAeSessLogon = PAE_SESSLOGON;

  _AE_SESSLOGOFF = record
    ae_sf_compname: DWORD;
    ae_sf_username: DWORD;
    ae_sf_reason: DWORD;
  end;
  //AE_SESSLOGOFF = _AE_SESSLOGOFF;
  //{$EXTERNALSYM AE_SESSLOGOFF}
  LPAE_SESSLOGOFF = ^_AE_SESSLOGOFF;
  PAE_SESSLOGOFF = ^_AE_SESSLOGOFF;
  TAeSessLogoff = _AE_SESSLOGOFF;
  PAeSessLogoff = PAE_SESSLOGOFF;

  _AE_SESSPWERR = record
    ae_sp_compname: DWORD;
    ae_sp_username: DWORD;
  end;
  //AE_SESSPWERR = _AE_SESSPWERR;
  //{$EXTERNALSYM AE_SESSPWERR}
  LPAE_SESSPWERR = ^_AE_SESSPWERR;
  PAE_SESSPWERR = ^_AE_SESSPWERR;
  TAeSessPwerr = _AE_SESSPWERR;
  PAeSessPwerr = PAE_SESSPWERR;

  _AE_CONNSTART = record
    ae_ct_compname: DWORD;
    ae_ct_username: DWORD;
    ae_ct_netname: DWORD;
    ae_ct_connid: DWORD;
  end;
  //AE_CONNSTART = _AE_CONNSTART;
  //{$EXTERNALSYM AE_CONNSTART}
  LPAE_CONNSTART = ^_AE_CONNSTART;
  PAE_CONNSTART = ^_AE_CONNSTART;
  TAeConnStart = _AE_CONNSTART;
  PAeConnStart = PAE_CONNSTART;

  _AE_CONNSTOP = record
    ae_cp_compname: DWORD;
    ae_cp_username: DWORD;
    ae_cp_netname: DWORD;
    ae_cp_connid: DWORD;
    ae_cp_reason: DWORD;
  end;
  //AE_CONNSTOP = _AE_CONNSTOP;
  //{$EXTERNALSYM AE_CONNSTOP}
  LPAE_CONNSTOP = ^_AE_CONNSTOP;
  PAE_CONNSTOP = ^_AE_CONNSTOP;
  TAeConnStop = _AE_CONNSTOP;
  PAeConnStop = PAE_CONNSTOP;

  _AE_CONNREJ = record
    ae_cr_compname: DWORD;
    ae_cr_username: DWORD;
    ae_cr_netname: DWORD;
    ae_cr_reason: DWORD;
  end;
  //AE_CONNREJ = _AE_CONNREJ;
  //{$EXTERNALSYM AE_CONNREJ}
  LPAE_CONNREJ = ^_AE_CONNREJ;
  PAE_CONNREJ = ^_AE_CONNREJ;
  TAeConnRej = _AE_CONNREJ;
  PAeConnRej = PAE_CONNREJ;

  _AE_RESACCESS = record
    ae_ra_compname: DWORD;
    ae_ra_username: DWORD;
    ae_ra_resname: DWORD;
    ae_ra_operation: DWORD;
    ae_ra_returncode: DWORD;
    ae_ra_restype: DWORD;
    ae_ra_fileid: DWORD;
  end;
  //AE_RESACCESS = _AE_RESACCESS;
  //{$EXTERNALSYM AE_RESACCESS}
  LPAE_RESACCESS = ^_AE_RESACCESS;
  PAE_RESACCESS = ^_AE_RESACCESS;
  TAeResAccess = _AE_RESACCESS;
  PAeResAccess = PAE_RESACCESS;

  _AE_RESACCESSREJ = record
    ae_rr_compname: DWORD;
    ae_rr_username: DWORD;
    ae_rr_resname: DWORD;
    ae_rr_operation: DWORD;
  end;
  //AE_RESACCESSREJ = _AE_RESACCESSREJ;
  //{$EXTERNALSYM AE_RESACCESSREJ}
  LPAE_RESACCESSREJ = ^_AE_RESACCESSREJ;
  PAE_RESACCESSREJ = ^_AE_RESACCESSREJ;
  TAeResAccessRej = _AE_RESACCESSREJ;
  PAeResAccessRej = PAE_RESACCESSREJ;

  _AE_CLOSEFILE = record
    ae_cf_compname: DWORD;
    ae_cf_username: DWORD;
    ae_cf_resname: DWORD;
    ae_cf_fileid: DWORD;
    ae_cf_duration: DWORD;
    ae_cf_reason: DWORD;
  end;
  //AE_CLOSEFILE = _AE_CLOSEFILE;
  //{$EXTERNALSYM AE_CLOSEFILE}
  LPAE_CLOSEFILE = ^_AE_CLOSEFILE;
  PAE_CLOSEFILE = ^_AE_CLOSEFILE;
  TAeCloseFile = _AE_CLOSEFILE;
  PAeCloseFile = PAE_CLOSEFILE;

  _AE_SERVICESTAT = record
    ae_ss_compname: DWORD;
    ae_ss_username: DWORD;
    ae_ss_svcname: DWORD;
    ae_ss_status: DWORD;
    ae_ss_code: DWORD;
    ae_ss_text: DWORD;
    ae_ss_returnval: DWORD;
  end;
  //AE_SERVICESTAT = _AE_SERVICESTAT;
  //{$EXTERNALSYM AE_SERVICESTAT}
  LPAE_SERVICESTAT = ^_AE_SERVICESTAT;
  PAE_SERVICESTAT = ^_AE_SERVICESTAT;
  TAeServiceStat = _AE_SERVICESTAT;
  PAeServiceStat = PAE_SERVICESTAT;

  _AE_ACLMOD = record
    ae_am_compname: DWORD;
    ae_am_username: DWORD;
    ae_am_resname: DWORD;
    ae_am_action: DWORD;
    ae_am_datalen: DWORD;
  end;
  //AE_ACLMOD = _AE_ACLMOD;
  //{$EXTERNALSYM AE_ACLMOD}
  LPAE_ACLMOD = ^_AE_ACLMOD;
  PAE_ACLMOD = ^_AE_ACLMOD;
  TAeAclMod = _AE_ACLMOD;
  PAeAclMod = PAE_ACLMOD;

  _AE_UASMOD = record
    ae_um_compname: DWORD;
    ae_um_username: DWORD;
    ae_um_resname: DWORD;
    ae_um_rectype: DWORD;
    ae_um_action: DWORD;
    ae_um_datalen: DWORD;
  end;
  //AE_UASMOD = _AE_UASMOD;
  //{$EXTERNALSYM AE_UASMOD}
  LPAE_UASMOD = ^_AE_UASMOD;
  PAE_UASMOD = ^_AE_UASMOD;
  TAeUasMod = _AE_UASMOD;
  PAeUasMod = PAE_UASMOD;

  _AE_NETLOGON = record
    ae_no_compname: DWORD;
    ae_no_username: DWORD;
    ae_no_privilege: DWORD;
    ae_no_authflags: DWORD;
  end;
  //AE_NETLOGON = _AE_NETLOGON;
  //{$EXTERNALSYM AE_NETLOGON}
  LPAE_NETLOGON = ^_AE_NETLOGON;
  PAE_NETLOGON = ^_AE_NETLOGON;
  TAeNetLogon = _AE_NETLOGON;
  PAeNetLogon = PAE_NETLOGON;

  _AE_NETLOGOFF = record
    ae_nf_compname: DWORD;
    ae_nf_username: DWORD;
    ae_nf_reserved1: DWORD;
    ae_nf_reserved2: DWORD;
  end;
  //AE_NETLOGOFF = _AE_NETLOGOFF;
  //{$EXTERNALSYM AE_NETLOGOFF}
  LPAE_NETLOGOFF = ^_AE_NETLOGOFF;
  PAE_NETLOGOFF = ^_AE_NETLOGOFF;
  TAeNetLogoff = _AE_NETLOGOFF;
  PAeNetLogoff = PAE_NETLOGOFF;

  _AE_ACCLIM = record
    ae_al_compname: DWORD;
    ae_al_username: DWORD;
    ae_al_resname: DWORD;
    ae_al_limit: DWORD;
  end;
  //AE_ACCLIM = _AE_ACCLIM;
  //{$EXTERNALSYM AE_ACCLIM}
  LPAE_ACCLIM = ^_AE_ACCLIM;
  PAE_ACCLIM = ^_AE_ACCLIM;
  TAeAccLim = _AE_ACCLIM;
  PAeAccLim = PAE_ACCLIM;

const
  ACTION_LOCKOUT     = 0;
  ACTION_ADMINUNLOCK = 1;

type
  _AE_LOCKOUT = record
    ae_lk_compname: DWORD; // Ptr to computername of client.
    ae_lk_username: DWORD; // Ptr to username of client (NULL
    //  if same as computername).
    ae_lk_action: DWORD; // Action taken on account:
    // 0 means locked out, 1 means not.
    ae_lk_bad_pw_count: DWORD; // Bad password count at the time
    // of lockout.
  end;
  //AE_LOCKOUT = _AE_LOCKOUT;
  //{$EXTERNALSYM AE_LOCKOUT}
  LPAE_LOCKOUT = ^_AE_LOCKOUT;
  PAE_LOCKOUT = ^_AE_LOCKOUT;
  TAeLockout = _AE_LOCKOUT;
  PAeLockout = PAE_LOCKOUT;

  _AE_GENERIC = record
    ae_ge_msgfile: DWORD;
    ae_ge_msgnum: DWORD;
    ae_ge_params: DWORD;
    ae_ge_param1: DWORD;
    ae_ge_param2: DWORD;
    ae_ge_param3: DWORD;
    ae_ge_param4: DWORD;
    ae_ge_param5: DWORD;
    ae_ge_param6: DWORD;
    ae_ge_param7: DWORD;
    ae_ge_param8: DWORD;
    ae_ge_param9: DWORD;
  end;
  //AE_GENERIC = _AE_GENERIC;
  //{$EXTERNALSYM AE_GENERIC}
  LPAE_GENERIC = ^_AE_GENERIC;
  PAE_GENERIC = ^_AE_GENERIC;
  TAeGeneric = _AE_GENERIC;
  PAeGeneric = PAE_GENERIC;

//
// Special Values and Constants - Audit
//

//
// 	Audit entry types (field ae_type in audit_entry).
//

const
  AE_SRVSTATUS    = 0;
  AE_SESSLOGON    = 1;
  AE_SESSLOGOFF   = 2;
  AE_SESSPWERR    = 3;
  AE_CONNSTART    = 4;
  AE_CONNSTOP     = 5;
  AE_CONNREJ      = 6;
  AE_RESACCESS    = 7;
  AE_RESACCESSREJ = 8;
  AE_CLOSEFILE    = 9;
  AE_SERVICESTAT  = 11;
  AE_ACLMOD       = 12;
  AE_UASMOD       = 13;
  AE_NETLOGON     = 14;
  AE_NETLOGOFF    = 15;
  AE_NETLOGDENIED = 16;
  AE_ACCLIMITEXCD = 17;
  AE_RESACCESS2   = 18;
  AE_ACLMODFAIL   = 19;
  AE_LOCKOUT      = 20;
  AE_GENERIC_TYPE = 21;

//
//	Values for ae_ss_status field of ae_srvstatus.
//

  AE_SRVSTART  = 0;
  AE_SRVPAUSED = 1;
  AE_SRVCONT   = 2;
  AE_SRVSTOP   = 3;

//
// 	Values for ae_so_privilege field of ae_sesslogon.
//

  AE_GUEST = 0;
  AE_USER  = 1;
  AE_ADMIN = 2;

//
//	Values for various ae_XX_reason fields.
//

  AE_NORMAL        = 0;
  AE_USERLIMIT     = 0;
  AE_GENERAL       = 0;
  AE_ERROR         = 1;
  AE_SESSDIS       = 1;
  AE_BADPW         = 1;
  AE_AUTODIS       = 2;
  AE_UNSHARE       = 2;
  AE_ADMINPRIVREQD = 2;
  AE_ADMINDIS      = 3;
  AE_NOACCESSPERM  = 3;
  AE_ACCRESTRICT   = 4;

  AE_NORMAL_CLOSE = 0;
  AE_SES_CLOSE    = 1;
  AE_ADMIN_CLOSE  = 2;

//
// Values for xx_subreason fields.
//

  AE_LIM_UNKNOWN     = 0;
  AE_LIM_LOGONHOURS  = 1;
  AE_LIM_EXPIRED     = 2;
  AE_LIM_INVAL_WKSTA = 3;
  AE_LIM_DISABLED    = 4;
  AE_LIM_DELETED     = 5;

//
// Values for xx_action fields
//

  AE_MOD    = 0;
  AE_DELETE = 1;
  AE_ADD    = 2;

//
// Types of UAS record for um_rectype field
//

  AE_UAS_USER   = 0;
  AE_UAS_GROUP  = 1;
  AE_UAS_MODALS = 2;

//
// Bitmasks for auditing events
//
// The parentheses around the hex constants broke h_to_inc
// and have been purged from the face of the earth.
//

  SVAUD_SERVICE       = $1;
  SVAUD_GOODSESSLOGON = $6;
  SVAUD_BADSESSLOGON  = $18;
  SVAUD_SESSLOGON     = (SVAUD_GOODSESSLOGON or SVAUD_BADSESSLOGON);
  SVAUD_GOODNETLOGON  = $60;
  SVAUD_BADNETLOGON   = $180;
  SVAUD_NETLOGON      = (SVAUD_GOODNETLOGON or SVAUD_BADNETLOGON);
  SVAUD_LOGON         = (SVAUD_NETLOGON or SVAUD_SESSLOGON);
  SVAUD_GOODUSE       = $600;
  SVAUD_BADUSE        = $1800;
  SVAUD_USE           = (SVAUD_GOODUSE or SVAUD_BADUSE);
  SVAUD_USERLIST      = $2000;
  SVAUD_PERMISSIONS   = $4000;
  SVAUD_RESOURCE      = $8000;
  SVAUD_LOGONLIM      = $00010000;

//
// Resource access audit bitmasks.
//

  AA_AUDIT_ALL = $0001;
  AA_A_OWNER   = $0004;
  AA_CLOSE     = $0008;
  AA_S_OPEN    = $0010;
  AA_S_WRITE   = $0020;
  AA_S_CREATE  = $0020;
  AA_S_DELETE  = $0040;
  AA_S_ACL     = $0080;
  AA_S_ALL     = ( AA_S_OPEN or AA_S_WRITE or AA_S_DELETE or AA_S_ACL);
  AA_F_OPEN    = $0100;
  AA_F_WRITE   = $0200;
  AA_F_CREATE  = $0200;
  AA_F_DELETE  = $0400;
  AA_F_ACL     = $0800;
  AA_F_ALL     = ( AA_F_OPEN or AA_F_WRITE or AA_F_DELETE or AA_F_ACL);

// Pinball-specific

  AA_A_OPEN   = $1000;
  AA_A_WRITE  = $2000;
  AA_A_CREATE = $2000;
  AA_A_DELETE = $4000;
  AA_A_ACL    = $8000;
  AA_A_ALL    = (AA_F_OPEN or AA_F_WRITE or AA_F_DELETE or AA_F_ACL);

implementation


{$IFDEF DYNAMIC_LINK}
var
  _NetAuditClear: Pointer;

function NetAuditClear;
begin
  GetProcedureAddress(_NetAuditClear, netapi32, 'NetAuditClear');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetAuditClear]
  end;
end;
{$ELSE}
function NetAuditClear; external netapi32 name 'NetAuditClear';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NetAuditRead: Pointer;

function NetAuditRead;
begin
  GetProcedureAddress(_NetAuditRead, netapi32, 'NetAuditRead');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetAuditRead]
  end;
end;
{$ELSE}
function NetAuditRead; external netapi32 name 'NetAuditRead';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NetAuditWrite: Pointer;

function NetAuditWrite;
begin
  GetProcedureAddress(_NetAuditWrite, netapi32, 'NetAuditWrite');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NetAuditWrite]
  end;
end;
{$ELSE}
function NetAuditWrite; external netapi32 name 'NetAuditWrite';
{$ENDIF DYNAMIC_LINK}

end.

