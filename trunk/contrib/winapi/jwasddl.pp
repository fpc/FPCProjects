{******************************************************************************}
{                                                       	               }
{ Security Descriptor Definition Language API interface Unit for Object Pascal }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: sddl.h, released June 2000. The original Pascal        }
{ code is: Sddl.pas, released December 2000. The initial developer of the      }
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

unit JwaSddl;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "sddl.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinNT, JwaWinType;

//
// SDDL Version information
//

const
  SDDL_REVISION_1 = 1;
  SDDL_REVISION   = SDDL_REVISION_1;

//
// SDDL Component tags
//

  SDDL_OWNER = 'O'; // Owner tag
  SDDL_GROUP = 'G'; // Group tag
  SDDL_DACL  = 'D'; // DACL tag
  SDDL_SACL  = 'S'; // SACL tag

//
// SDDL Security descriptor controls
//

  SDDL_PROTECTED        = 'P'; // DACL or SACL Protected
  SDDL_AUTO_INHERIT_REQ = 'AR'; // Auto inherit request
  SDDL_AUTO_INHERITED   = 'AI'; // DACL/SACL are auto inherited

//
// SDDL Ace types
//

  SDDL_ACCESS_ALLOWED        = 'A'; // Access allowed
  SDDL_ACCESS_DENIED         = 'D'; // Access denied
  SDDL_OBJECT_ACCESS_ALLOWED = 'OA'; // Object access allowed
  SDDL_OBJECT_ACCESS_DENIED  = 'OD'; // Object access denied
  SDDL_AUDIT                 = 'AU'; // Audit
  SDDL_ALARM                 = 'AL'; // Alarm
  SDDL_OBJECT_AUDIT          = 'OU'; // Object audit
  SDDL_OBJECT_ALARM          = 'OL'; // Object alarm

//
// SDDL Ace flags
//

  SDDL_CONTAINER_INHERIT = 'CI'; // Container inherit
  SDDL_OBJECT_INHERIT    = 'OI'; // Object inherit
  SDDL_NO_PROPAGATE      = 'NP'; // Inherit no propagate
  SDDL_INHERIT_ONLY      = 'IO'; // Inherit only
  SDDL_INHERITED         = 'ID'; // Inherited
  SDDL_AUDIT_SUCCESS     = 'SA'; // Audit success
  SDDL_AUDIT_FAILURE     = 'FA'; // Audit failure

//
// SDDL Rights
//

  SDDL_READ_PROPERTY   = 'RP';
  SDDL_WRITE_PROPERTY  = 'WP';
  SDDL_CREATE_CHILD    = 'CC';
  SDDL_DELETE_CHILD    = 'DC';
  SDDL_LIST_CHILDREN   = 'LC';
  SDDL_SELF_WRITE      = 'SW';
  SDDL_LIST_OBJECT     = 'LO';
  SDDL_DELETE_TREE     = 'DT';
  SDDL_CONTROL_ACCESS  = 'CR';
  SDDL_READ_CONTROL    = 'RC';
  SDDL_WRITE_DAC       = 'WD';
  SDDL_WRITE_OWNER     = 'WO';
  SDDL_STANDARD_DELETE = 'SD';
  SDDL_GENERIC_ALL     = 'GA';
  SDDL_GENERIC_READ    = 'GR';
  SDDL_GENERIC_WRITE   = 'GW';
  SDDL_GENERIC_EXECUTE = 'GX';
  SDDL_FILE_ALL        = 'FA';
  SDDL_FILE_READ       = 'FR';
  SDDL_FILE_WRITE      = 'FW';
  SDDL_FILE_EXECUTE    = 'FX';
  SDDL_KEY_ALL         = 'KA';
  SDDL_KEY_READ        = 'KR';
  SDDL_KEY_WRITE       = 'KW';
  SDDL_KEY_EXECUTE     = 'KX';

//
// SDDL User alias max size
//      - currently, upto two supported eg. "DA"
//      - modify this if more WCHARs need to be there in future e.g. "DAX"
//

  SDDL_ALIAS_SIZE = 2;

//
// SDDL User aliases
//

  SDDL_DOMAIN_ADMINISTRATORS         = 'DA'; // Domain admins
  SDDL_DOMAIN_GUESTS                 = 'DG'; // Domain guests
  SDDL_DOMAIN_USERS                  = 'DU'; // Domain users
  SDDL_ENTERPRISE_DOMAIN_CONTROLLERS = 'ED'; // Enterprise domain controllers
  SDDL_DOMAIN_DOMAIN_CONTROLLERS     = 'DD'; // Domain domain controllers
  SDDL_DOMAIN_COMPUTERS              = 'DC'; // Domain computers
  SDDL_BUILTIN_ADMINISTRATORS        = 'BA'; // Builtin (local ) administrators
  SDDL_BUILTIN_GUESTS                = 'BG'; // Builtin (local ) guests
  SDDL_BUILTIN_USERS                 = 'BU'; // Builtin (local ) users
  SDDL_LOCAL_ADMIN                   = 'LA'; // Local administrator account
  SDDL_LOCAL_GUEST                   = 'LG'; // Local group account
  SDDL_ACCOUNT_OPERATORS             = 'AO'; // Account operators
  SDDL_BACKUP_OPERATORS              = 'BO'; // Backup operators
  SDDL_PRINTER_OPERATORS             = 'PO'; // Printer operators
  SDDL_SERVER_OPERATORS              = 'SO'; // Server operators
  SDDL_AUTHENTICATED_USERS           = 'AU'; // Authenticated users
  SDDL_PERSONAL_SELF                 = 'PS'; // Personal self
  SDDL_CREATOR_OWNER                 = 'CO'; // Creator owner
  SDDL_CREATOR_GROUP                 = 'CG'; // Creator group
  SDDL_LOCAL_SYSTEM                  = 'SY'; // Local system
  SDDL_POWER_USERS                   = 'PU'; // Power users
  SDDL_EVERYONE                      = 'WD'; // Everyone ( World )
  SDDL_REPLICATOR                    = 'RE'; // Replicator
  SDDL_INTERACTIVE                   = 'IU'; // Interactive logon user
  SDDL_NETWORK                       = 'NU'; // Nework logon user
  SDDL_SERVICE                       = 'SU'; // Service logon user
  SDDL_RESTRICTED_CODE               = 'RC'; // Restricted code
  SDDL_ANONYMOUS                     = 'AN'; // Anonymous Logon
  SDDL_SCHEMA_ADMINISTRATORS         = 'SA'; // Schema Administrators
  SDDL_CERT_SERV_ADMINISTRATORS      = 'CA'; // Certificate Server Administrators
  SDDL_RAS_SERVERS                   = 'RS'; // RAS servers group
  SDDL_ENTERPRISE_ADMINS             = 'EA'; // Enterprise administrators
  SDDL_GROUP_POLICY_ADMINS           = 'PA'; // Group Policy administrators
  SDDL_ALIAS_PREW2KCOMPACC           = 'RU'; // alias to allow previous windows 2000
  SDDL_LOCAL_SERVICE                 = 'LS'; // Local service account (for services)
  SDDL_NETWORK_SERVICE               = 'NS'; // Network service account (for services)
  SDDL_REMOTE_DESKTOP                = 'RD'; // Remote desktop users (for terminal server)
  SDDL_NETWORK_CONFIGURATION_OPS     = 'NO'; // Network configuration operators ( to manage configuration of networking features)
  SDDL_PERFMON_USERS                 = 'MU'; // Performance Monitor Users
  SDDL_PERFLOG_USERS                 = 'LU'; // Performance Log Users

//
// SDDL Seperators - character version
//

  SDDL_SEPERATORC   = ';';
  SDDL_DELIMINATORC = ':';
  SDDL_ACE_BEGINC   = '(';
  SDDL_ACE_ENDC     = ')';

//
// SDDL Seperators - string version
//

  SDDL_SEPERATOR   = ';';
  SDDL_DELIMINATOR = ':';
  SDDL_ACE_BEGIN   = '(';
  SDDL_ACE_END     = ')';

function ConvertSidToStringSidA(Sid: PSID; var StringSid: LPSTR): BOOL; stdcall;
function ConvertSidToStringSidW(Sid: PSID; var StringSid: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function ConvertSidToStringSid(Sid: PSID; var StringSid: LPWSTR): BOOL; stdcall;
{$ELSE}
function ConvertSidToStringSid(Sid: PSID; var StringSid: LPSTR): BOOL; stdcall;
{$ENDIF}

function ConvertStringSidToSidA(StringSid: LPCSTR; var Sid: PSID): BOOL; stdcall;
function ConvertStringSidToSidW(StringSid: LPCWSTR; var Sid: PSID): BOOL; stdcall;

{$IFDEF UNICODE}
function ConvertStringSidToSid(StringSid: LPCWSTR; var Sid: PSID): BOOL; stdcall;
{$ELSE}
function ConvertStringSidToSid(StringSid: LPCSTR; var Sid: PSID): BOOL; stdcall;
{$ENDIF}

function ConvertStringSecurityDescriptorToSecurityDescriptorA(StringSecurityDescriptor: LPCSTR;
  StringSDRevision: DWORD; var SecurityDescriptor: PSECURITY_DESCRIPTOR;
  SecurityDescriptorSize: PULONG): BOOL; stdcall;
function ConvertStringSecurityDescriptorToSecurityDescriptorW(StringSecurityDescriptor: LPCWSTR;
  StringSDRevision: DWORD; var SecurityDescriptor: PSECURITY_DESCRIPTOR;
  SecurityDescriptorSize: PULONG): BOOL; stdcall;

{$IFDEF UNICODE}
function ConvertStringSecurityDescriptorToSecurityDescriptor(StringSecurityDescriptor: LPCWSTR;
  StringSDRevision: DWORD; var SecurityDescriptor: PSECURITY_DESCRIPTOR;
  SecurityDescriptorSize: PULONG): BOOL; stdcall;
{$ELSE}
function ConvertStringSecurityDescriptorToSecurityDescriptor(StringSecurityDescriptor: LPCSTR;
  StringSDRevision: DWORD; var SecurityDescriptor: PSECURITY_DESCRIPTOR;
  SecurityDescriptorSize: PULONG): BOOL; stdcall;
{$ENDIF}

function ConvertSecurityDescriptorToStringSecurityDescriptorA(
  SecurityDescriptor: PSECURITY_DESCRIPTOR; RequestedStringSDRevision: DWORD;
  SecurityInformation: SECURITY_INFORMATION; var StringSecurityDescriptor: LPSTR;
  StringSecurityDescriptorLen: PULONG): BOOL; stdcall;
function ConvertSecurityDescriptorToStringSecurityDescriptorW(
  SecurityDescriptor: PSECURITY_DESCRIPTOR; RequestedStringSDRevision: DWORD;
  SecurityInformation: SECURITY_INFORMATION; var StringSecurityDescriptor: LPWSTR;
  StringSecurityDescriptorLen: PULONG): BOOL; stdcall;

{$IFDEF UNICODE}
function ConvertSecurityDescriptorToStringSecurityDescriptor(
  SecurityDescriptor: PSECURITY_DESCRIPTOR; RequestedStringSDRevision: DWORD;
  SecurityInformation: SECURITY_INFORMATION; var StringSecurityDescriptor: LPWSTR;
  StringSecurityDescriptorLen: PULONG): BOOL; stdcall;
{$ELSE}
function ConvertSecurityDescriptorToStringSecurityDescriptor(
  SecurityDescriptor: PSECURITY_DESCRIPTOR; RequestedStringSDRevision: DWORD;
  SecurityInformation: SECURITY_INFORMATION; var StringSecurityDescriptor: LPSTR;
  StringSecurityDescriptorLen: PULONG): BOOL; stdcall;
{$ENDIF}

implementation

const
  advapi32 = 'advapi32.dll';


{$IFDEF DYNAMIC_LINK}
var
  _ConvertSidToStringSidA: Pointer;

function ConvertSidToStringSidA;
begin
  GetProcedureAddress(_ConvertSidToStringSidA, advapi32, 'ConvertSidToStringSidA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertSidToStringSidA]
  end;
end;
{$ELSE}
function ConvertSidToStringSidA; external advapi32 name 'ConvertSidToStringSidA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertSidToStringSidW: Pointer;

function ConvertSidToStringSidW;
begin
  GetProcedureAddress(_ConvertSidToStringSidW, advapi32, 'ConvertSidToStringSidW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertSidToStringSidW]
  end;
end;
{$ELSE}
function ConvertSidToStringSidW; external advapi32 name 'ConvertSidToStringSidW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertSidToStringSid: Pointer;

function ConvertSidToStringSid;
begin
  GetProcedureAddress(_ConvertSidToStringSid, advapi32, 'ConvertSidToStringSidW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertSidToStringSid]
  end;
end;
{$ELSE}
function ConvertSidToStringSid; external advapi32 name 'ConvertSidToStringSidW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertSidToStringSid: Pointer;

function ConvertSidToStringSid;
begin
  GetProcedureAddress(_ConvertSidToStringSid, advapi32, 'ConvertSidToStringSidA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertSidToStringSid]
  end;
end;
{$ELSE}
function ConvertSidToStringSid; external advapi32 name 'ConvertSidToStringSidA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertStringSidToSidA: Pointer;

function ConvertStringSidToSidA;
begin
  GetProcedureAddress(_ConvertStringSidToSidA, advapi32, 'ConvertStringSidToSidA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertStringSidToSidA]
  end;
end;
{$ELSE}
function ConvertStringSidToSidA; external advapi32 name 'ConvertStringSidToSidA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertStringSidToSidW: Pointer;

function ConvertStringSidToSidW;
begin
  GetProcedureAddress(_ConvertStringSidToSidW, advapi32, 'ConvertStringSidToSidW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertStringSidToSidW]
  end;
end;
{$ELSE}
function ConvertStringSidToSidW; external advapi32 name 'ConvertStringSidToSidW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertStringSidToSid: Pointer;

function ConvertStringSidToSid;
begin
  GetProcedureAddress(_ConvertStringSidToSid, advapi32, 'ConvertStringSidToSidW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertStringSidToSid]
  end;
end;
{$ELSE}
function ConvertStringSidToSid; external advapi32 name 'ConvertStringSidToSidW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvertStringSidToSid: Pointer;

function ConvertStringSidToSid;
begin
  GetProcedureAddress(_ConvertStringSidToSid, advapi32, 'ConvertStringSidToSidA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvertStringSidToSid]
  end;
end;
{$ELSE}
function ConvertStringSidToSid; external advapi32 name 'ConvertStringSidToSidA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ConvStrSecDescrToSecDescrA: Pointer;

function ConvertStringSecurityDescriptorToSecurityDescriptorA;
begin
  GetProcedureAddress(_ConvStrSecDescrToSecDescrA, advapi32, 'ConvertStringSecurityDescriptorToSecurityDescriptorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvStrSecDescrToSecDescrA]
  end;
end;
{$ELSE}
function ConvertStringSecurityDescriptorToSecurityDescriptorA; external advapi32 name 'ConvertStringSecurityDescriptorToSecurityDescriptorA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvStrSecDescrToSecDescrW: Pointer;

function ConvertStringSecurityDescriptorToSecurityDescriptorW;
begin
  GetProcedureAddress(_ConvStrSecDescrToSecDescrW, advapi32, 'ConvertStringSecurityDescriptorToSecurityDescriptorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvStrSecDescrToSecDescrW]
  end;
end;
{$ELSE}
function ConvertStringSecurityDescriptorToSecurityDescriptorW; external advapi32 name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvStrSecDescrToSecDescr: Pointer;

function ConvertStringSecurityDescriptorToSecurityDescriptor;
begin
  GetProcedureAddress(_ConvStrSecDescrToSecDescr, advapi32, 'ConvertStringSecurityDescriptorToSecurityDescriptorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvStrSecDescrToSecDescr]
  end;
end;
{$ELSE}
function ConvertStringSecurityDescriptorToSecurityDescriptor; external advapi32 name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvStrSecDescrToSecDescr: Pointer;

function ConvertStringSecurityDescriptorToSecurityDescriptor;
begin
  GetProcedureAddress(_ConvStrSecDescrToSecDescr, advapi32, 'ConvertStringSecurityDescriptorToSecurityDescriptorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvStrSecDescrToSecDescr]
  end;
end;
{$ELSE}
function ConvertStringSecurityDescriptorToSecurityDescriptor; external advapi32 name 'ConvertStringSecurityDescriptorToSecurityDescriptorA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ConvSecDescrToStrSecDescrA: Pointer;

function ConvertSecurityDescriptorToStringSecurityDescriptorA;
begin
  GetProcedureAddress(_ConvSecDescrToStrSecDescrA, advapi32, 'ConvertSecurityDescriptorToStringSecurityDescriptorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvSecDescrToStrSecDescrA]
  end;
end;
{$ELSE}
function ConvertSecurityDescriptorToStringSecurityDescriptorA; external advapi32 name 'ConvertSecurityDescriptorToStringSecurityDescriptorA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ConvSecDescrToStrSecDescrW: Pointer;

function ConvertSecurityDescriptorToStringSecurityDescriptorW;
begin
  GetProcedureAddress(_ConvSecDescrToStrSecDescrW, advapi32, 'ConvertSecurityDescriptorToStringSecurityDescriptorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvSecDescrToStrSecDescrW]
  end;
end;
{$ELSE}
function ConvertSecurityDescriptorToStringSecurityDescriptorW; external advapi32 name 'ConvertSecurityDescriptorToStringSecurityDescriptorW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvSecDescrToStrSecDescr: Pointer;

function ConvertSecurityDescriptorToStringSecurityDescriptor;
begin
  GetProcedureAddress(_ConvSecDescrToStrSecDescr, advapi32, 'ConvertSecurityDescriptorToStringSecurityDescriptorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvSecDescrToStrSecDescr]
  end;
end;
{$ELSE}
function ConvertSecurityDescriptorToStringSecurityDescriptor; external advapi32 name 'ConvertSecurityDescriptorToStringSecurityDescriptorW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ConvSecDescrToStrSecDescr: Pointer;

function ConvertSecurityDescriptorToStringSecurityDescriptor;
begin
  GetProcedureAddress(_ConvSecDescrToStrSecDescr, advapi32, 'ConvertSecurityDescriptorToStringSecurityDescriptorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ConvSecDescrToStrSecDescr]
  end;
end;
{$ELSE}
function ConvertSecurityDescriptorToStringSecurityDescriptor; external advapi32 name 'ConvertSecurityDescriptorToStringSecurityDescriptorA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

end.
