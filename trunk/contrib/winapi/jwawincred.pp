{******************************************************************************}
{                                                       	               }
{ Credentials Manager API interface Unit for Object Pascal                     }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: wincred.h, released November 2001. The original Pascal }
{ code is: WinCred.pas, released March 2002. The initial developer of the      }
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

unit JwaWinCred;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "wincred.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaLmCons, JwaWinBase, JwaWinError, JwaWinType, JwaNtSecApi;

type
  PCtxtHandle = PSecHandle;

//-----------------------------------------------------------------------------
// Macros
//-----------------------------------------------------------------------------

//
// Macro to determine whether CredUIPromptForCredentials should be called upon a failed
//      authentication attempt.
//
// Implemented as a macro so that the caller can delay load credui.dll only if this
//      macro returns TRUE.
//
// Include only status codes that imply the username/password are wrong or that the
//      password is expired.  In the former case, asking for a another username or password
//      is appropriate.  In the later case, we put up a different dialog asking the
//      user to change the password on the server.
//
// Don't include status codes such as ERROR_ACCOUNT_DISABLED, ERROR_ACCOUNT_RESTRICTION,
//      ERROR_ACCOUNT_LOCKED_OUT, ERROR_ACCOUNT_EXPIRED, ERROR_LOGON_TYPE_NOT_GRANTED.
//      For those, the user isn't going to have another account so prompting him
//      won't help.
//
// STATUS_DOWNGRADE_DETECTED is included to handle the case where a corporate laptop
//      is brought to another LAN.  A downgrade attack will indeed be detected,
//      but we want to popup UI to allow the user to connect to resources in the
//      other LAN.
//
// Don't use the CREDUIP_* macros directly.  Their definition is private to credui.dll.
//

// Don't require ntstatus.h

const
  STATUS_LOGON_FAILURE          = NTSTATUS($C000006D); // ntsubauth
  STATUS_WRONG_PASSWORD         = NTSTATUS($C000006A); // ntsubauth
  STATUS_PASSWORD_EXPIRED       = NTSTATUS($C0000071); // ntsubauth
  STATUS_PASSWORD_MUST_CHANGE   = NTSTATUS($C0000224); // ntsubauth
  STATUS_ACCESS_DENIED          = NTSTATUS($C0000022);
  STATUS_DOWNGRADE_DETECTED     = NTSTATUS($C0000388);
  STATUS_AUTHENTICATION_FIREWALL_FAILED = NTSTATUS($C0000413);
  STATUS_ACCOUNT_DISABLED       = NTSTATUS($C0000072);   // ntsubauth
  STATUS_ACCOUNT_RESTRICTION    = NTSTATUS($C000006E);   // ntsubauth
  STATUS_ACCOUNT_LOCKED_OUT     = NTSTATUS($C0000234);   // ntsubauth
  STATUS_ACCOUNT_EXPIRED        = NTSTATUS($C0000193);   // ntsubauth
  STATUS_LOGON_TYPE_NOT_GRANTED = NTSTATUS($C000015B);

// Don't require lmerr.h

  NERR_BASE            = 2100;
  NERR_PasswordExpired = (NERR_BASE+142); // The password of this user has expired.

function CREDUIP_IS_USER_PASSWORD_ERROR(_Status: NTSTATUS): BOOL;

function CREDUIP_IS_DOWNGRADE_ERROR(_Status: NTSTATUS): BOOL;

function CREDUIP_IS_EXPIRED_ERROR(_Status: NTSTATUS): BOOL;

function CREDUI_IS_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;

function CREDUI_NO_PROMPT_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;

//-----------------------------------------------------------------------------
// Structures
//-----------------------------------------------------------------------------

//
// Credential Attribute
//

const

// Maximum length of the various credential string fields (in characters)

  CRED_MAX_STRING_LENGTH = 256;

// Maximum length of the UserName field.  The worst case is <User>@<DnsDomain>

  CRED_MAX_USERNAME_LENGTH = (256+1+256);

// Maximum length of the TargetName field for CRED_TYPE_GENERIC (in characters)

  CRED_MAX_GENERIC_TARGET_NAME_LENGTH = 32767;

// Maximum length of the TargetName field for CRED_TYPE_DOMAIN_* (in characters)
//      Largest one is <DfsRoot>\<DfsShare>

  CRED_MAX_DOMAIN_TARGET_NAME_LENGTH = (256+1+80);

// Maximum size of the Credential Attribute Value field (in bytes)

  CRED_MAX_VALUE_SIZE = 256;

// Maximum number of attributes per credential

  CRED_MAX_ATTRIBUTES = 64;

type
  PCREDENTIAL_ATTRIBUTEA = ^CREDENTIAL_ATTRIBUTEA;
  _CREDENTIAL_ATTRIBUTEA = record
    Keyword: LPSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  CREDENTIAL_ATTRIBUTEA = _CREDENTIAL_ATTRIBUTEA;
  TCredentialAttributeA = CREDENTIAL_ATTRIBUTEA;
  PCredentialAttributeA = PCREDENTIAL_ATTRIBUTEA;

  PCREDENTIAL_ATTRIBUTEW = ^CREDENTIAL_ATTRIBUTEW;
  _CREDENTIAL_ATTRIBUTEW = record
    Keyword: LPWSTR;
    Flags: DWORD;
    ValueSize: DWORD;
    Value: LPBYTE;
  end;
  CREDENTIAL_ATTRIBUTEW = _CREDENTIAL_ATTRIBUTEW;
  TCredentialAttributeW = CREDENTIAL_ATTRIBUTEW;
  PCredentialAttributeW = PCREDENTIAL_ATTRIBUTEW;

{$IFDEF UNICODE}
  CREDENTIAL_ATTRIBUTE = CREDENTIAL_ATTRIBUTEW;
  PCREDENTIAL_ATTRIBUTE = PCREDENTIAL_ATTRIBUTEW;
  TCredentialAttribute = TCredentialAttributeW;
  PCredentialAttribute = PCredentialAttributeW;
{$ELSE}
  CREDENTIAL_ATTRIBUTE = CREDENTIAL_ATTRIBUTEA;
  PCREDENTIAL_ATTRIBUTE = PCREDENTIAL_ATTRIBUTEA;
  TCredentialAttribute = TCredentialAttributeA;
  PCredentialAttribute = PCredentialAttributeA;
{$ENDIF}

//
// Special values of the TargetName field
//

const
  CRED_SESSION_WILDCARD_NAME_W      = WideString('*Session');
  CRED_SESSION_WILDCARD_NAME_A      = '*Session';
  CRED_SESSION_WILDCARD_NAME_LENGTH = SizeOf(CRED_SESSION_WILDCARD_NAME_A) - 1;

{$IFDEF UNICODE}
  CRED_SESSION_WILDCARD_NAME = CRED_SESSION_WILDCARD_NAME_W;
{$ELSE}
  CRED_SESSION_WILDCARD_NAME = CRED_SESSION_WILDCARD_NAME_A;
{$ENDIF}

//
// Values of the Credential Flags field.
//

const
  CRED_FLAGS_PASSWORD_FOR_CERT  = $0001;
  CRED_FLAGS_PROMPT_NOW         = $0002;
  CRED_FLAGS_USERNAME_TARGET    = $0004;
  CRED_FLAGS_OWF_CRED_BLOB      = $0008;
  CRED_FLAGS_VALID_FLAGS        = $000F;  // Mask of all valid flags

//
// Values of the Credential Type field.
//

  CRED_TYPE_GENERIC                 = 1;
  CRED_TYPE_DOMAIN_PASSWORD         = 2;
  CRED_TYPE_DOMAIN_CERTIFICATE      = 3;
  CRED_TYPE_DOMAIN_VISIBLE_PASSWORD = 4;
  CRED_TYPE_MAXIMUM                 = 5; // Maximum supported cred type
  CRED_TYPE_MAXIMUM_EX              = (CRED_TYPE_MAXIMUM+1000);  // Allow new applications to run on old OSes

//
// Maximum size of the CredBlob field (in bytes)
//

  CRED_MAX_CREDENTIAL_BLOB_SIZE = 512;

//
// Values of the Credential Persist field
//

  CRED_PERSIST_NONE          = 0;
  CRED_PERSIST_SESSION       = 1;
  CRED_PERSIST_LOCAL_MACHINE = 2;
  CRED_PERSIST_ENTERPRISE    = 3;

//
// A credential
//

type
  PCREDENTIALA = ^CREDENTIALA;
  _CREDENTIALA = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPSTR;
    Comment: LPSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEA;
    TargetAlias: LPSTR;
    UserName: LPSTR;
  end;
  CREDENTIALA = _CREDENTIALA;
  TCredentialA = CREDENTIALA;

  PCREDENTIALW = ^CREDENTIALW;
  _CREDENTIALW = record
    Flags: DWORD;
    Type_: DWORD;
    TargetName: LPWSTR;
    Comment: LPWSTR;
    LastWritten: FILETIME;
    CredentialBlobSize: DWORD;
    CredentialBlob: LPBYTE;
    Persist: DWORD;
    AttributeCount: DWORD;
    Attributes: PCREDENTIAL_ATTRIBUTEW;
    TargetAlias: LPWSTR;
    UserName: LPWSTR;
  end;
  CREDENTIALW = _CREDENTIALW;
  TCredentialW = CREDENTIALW;

{$IFDEF UNICODE}
  CREDENTIAL = CREDENTIALW;
  PCREDENTIAL = PCREDENTIALW;
  TCredential = TCredentialW;
{$ELSE}
  CREDENTIAL = CREDENTIALA;
  PCREDENTIAL = PCREDENTIALA;
  TCredential = TCredentialA;
{$ENDIF}

//
// Value of the Flags field in CREDENTIAL_TARGET_INFORMATION
//

const
  CRED_TI_SERVER_FORMAT_UNKNOWN  = $0001; // Don't know if server name is DNS or netbios format
  CRED_TI_DOMAIN_FORMAT_UNKNOWN  = $0002; // Don't know if domain name is DNS or netbios format
  CRED_TI_ONLY_PASSWORD_REQUIRED = $0004; // Server only requires a password and not a username
  CRED_TI_USERNAME_TARGET        = $0008; // TargetName is username
  CRED_TI_CREATE_EXPLICIT_CRED   = $0010; // When creating a cred, create one named TargetInfo->TargetName
  CRED_TI_WORKGROUP_MEMBER       = $0020; // Indicates the machine is a member of a workgroup
  CRED_TI_VALID_FLAGS            = $003F;

//
// A credential target
//

type
  PCREDENTIAL_TARGET_INFORMATIONA = ^CREDENTIAL_TARGET_INFORMATIONA;
  _CREDENTIAL_TARGET_INFORMATIONA = record
    TargetName: LPSTR;
    NetbiosServerName: LPSTR;
    DnsServerName: LPSTR;
    NetbiosDomainName: LPSTR;
    DnsDomainName: LPSTR;
    DnsTreeName: LPSTR;
    PackageName: LPSTR;
    Flags: ULONG;
    CredTypeCount: DWORD;
    CredTypes: LPDWORD;
  end;
  CREDENTIAL_TARGET_INFORMATIONA = _CREDENTIAL_TARGET_INFORMATIONA;
  TCredentialTargetInformationA = CREDENTIAL_TARGET_INFORMATIONA;
  PCredentialTargetInformationA = PCREDENTIAL_TARGET_INFORMATIONA;

  PCREDENTIAL_TARGET_INFORMATIONW = ^CREDENTIAL_TARGET_INFORMATIONW;
  _CREDENTIAL_TARGET_INFORMATIONW = record
    TargetName: LPWSTR;
    NetbiosServerName: LPWSTR;
    DnsServerName: LPWSTR;
    NetbiosDomainName: LPWSTR;
    DnsDomainName: LPWSTR;
    DnsTreeName: LPWSTR;
    PackageName: LPWSTR;
    Flags: ULONG;
    CredTypeCount: DWORD;
    CredTypes: LPDWORD;
  end;
  CREDENTIAL_TARGET_INFORMATIONW = _CREDENTIAL_TARGET_INFORMATIONW;
  TCredentialTargetInformationW = CREDENTIAL_TARGET_INFORMATIONW;
  PCredentialTargetInformationW = PCREDENTIAL_TARGET_INFORMATIONW;

{$IFDEF UNICODE}
  CREDENTIAL_TARGET_INFORMATION = CREDENTIAL_TARGET_INFORMATIONW;
  PCREDENTIAL_TARGET_INFORMATION = PCREDENTIAL_TARGET_INFORMATIONW;
  TCredentialTargetInformation = TCredentialTargetInformationW;
  PCredentialTargetInformation = PCredentialTargetInformationW;
{$ELSE}
  CREDENTIAL_TARGET_INFORMATION = CREDENTIAL_TARGET_INFORMATIONA;
  PCREDENTIAL_TARGET_INFORMATION = PCREDENTIAL_TARGET_INFORMATIONA;
  TCredentialTargetInformation = TCredentialTargetInformationA;
  PCredentialTargetInformation = PCredentialTargetInformationA;
{$ENDIF}

//
// Certificate credential information
//
// The cbSize should be the size of the structure, sizeof(CERT_CREDENTIAL_INFO),
// rgbHashofCert is the hash of the cert which is to be used as the credential.
//

const
  CERT_HASH_LENGTH = 20; // SHA1 hashes are used for cert hashes

type
  PCERT_CREDENTIAL_INFO = ^CERT_CREDENTIAL_INFO;
  _CERT_CREDENTIAL_INFO = record
    cbSize: ULONG;
    rgbHashOfCert: array [0..CERT_HASH_LENGTH - 1] of UCHAR;
  end;
  CERT_CREDENTIAL_INFO = _CERT_CREDENTIAL_INFO;
  TCertCredentialInfo = CERT_CREDENTIAL_INFO;
  PCertCredentialInfo = PCERT_CREDENTIAL_INFO;

//
// Username Target credential information
//
// This credential can be pass to LsaLogonUser to ask it to find a credential with a
// TargetName of UserName.
//

  PUSERNAME_TARGET_CREDENTIAL_INFO = ^USERNAME_TARGET_CREDENTIAL_INFO;
  _USERNAME_TARGET_CREDENTIAL_INFO = record
    UserName: LPWSTR;
  end;
  USERNAME_TARGET_CREDENTIAL_INFO = _USERNAME_TARGET_CREDENTIAL_INFO;
  TUsernameTargetCredentialInfo = USERNAME_TARGET_CREDENTIAL_INFO;
  PUsernameTargetCredentialInfo = PUSERNAME_TARGET_CREDENTIAL_INFO;

//
// Credential type for credential marshaling routines
//

  _CRED_MARSHAL_TYPE = DWORD;
  CRED_MARSHAL_TYPE = _CRED_MARSHAL_TYPE;
  PCRED_MARSHAL_TYPE = ^CRED_MARSHAL_TYPE;
  TCredMarshalType = CRED_MARSHAL_TYPE;
  PCredMarshalType = PCRED_MARSHAL_TYPE;

const
  CertCredential = 1;
  UsernameTargetCredential = 2;

//
// Credential UI info
//

type
  PCREDUI_INFOA = ^CREDUI_INFOA;
  _CREDUI_INFOA = record
    cbSize: DWORD;
    hwndParent: HWND;
    pszMessageText: PCSTR;
    pszCaptionText: PCSTR;
    hbmBanner: HBITMAP;
  end;
  CREDUI_INFOA = _CREDUI_INFOA;
  TCredUIInfoA = CREDUI_INFOA;
  PCredUIInfoA = PCREDUI_INFOA;

  PCREDUI_INFOW = ^CREDUI_INFOW;
  _CREDUI_INFOW = record
    cbSize: DWORD;
    hwndParent: HWND;
    pszMessageText: LPCWSTR;
    pszCaptionText: LPCWSTR;
    hbmBanner: HBITMAP;
  end;
  CREDUI_INFOW = _CREDUI_INFOW;
  TCredUIInfoW = CREDUI_INFOW;
  PCredUIInfoW = PCREDUI_INFOW;

{$IFDEF UNICODE}
  CREDUI_INFO = CREDUI_INFOW;
  PCREDUI_INFO = PCREDUI_INFOW;
  TCredUIInfo = TCredUIInfoW;
  PCredUIInfo = PCredUIInfoW;
{$ELSE}
  CREDUI_INFO = CREDUI_INFOA;
  PCREDUI_INFO = PCREDUI_INFOA;
  TCredUIInfo = TCredUIInfoA;
  PCredUIInfo = PCredUIInfoA;
{$ENDIF}

//-----------------------------------------------------------------------------
// Values
//-----------------------------------------------------------------------------

// String length limits:

const
  CREDUI_MAX_MESSAGE_LENGTH        = 32767;
  CREDUI_MAX_CAPTION_LENGTH        = 128;
  CREDUI_MAX_GENERIC_TARGET_LENGTH = CRED_MAX_GENERIC_TARGET_NAME_LENGTH;
  CREDUI_MAX_DOMAIN_TARGET_LENGTH  = CRED_MAX_DOMAIN_TARGET_NAME_LENGTH;
  CREDUI_MAX_USERNAME_LENGTH       = CRED_MAX_USERNAME_LENGTH;
  CREDUI_MAX_PASSWORD_LENGTH       = (CRED_MAX_CREDENTIAL_BLOB_SIZE div 2);

//
// Flags for CredUIPromptForCredentials and/or CredUICmdLinePromptForCredentials
//

  CREDUI_FLAGS_INCORRECT_PASSWORD          = $00001; // indicates the username is valid, but password is not
  CREDUI_FLAGS_DO_NOT_PERSIST              = $00002; // Do not show "Save" checkbox, and do not persist credentials
  CREDUI_FLAGS_REQUEST_ADMINISTRATOR       = $00004; // Populate list box with admin accounts
  CREDUI_FLAGS_EXCLUDE_CERTIFICATES        = $00008; // do not include certificates in the drop list
  CREDUI_FLAGS_REQUIRE_CERTIFICATE         = $00010;
  CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX         = $00040;
  CREDUI_FLAGS_ALWAYS_SHOW_UI              = $00080;
  CREDUI_FLAGS_REQUIRE_SMARTCARD           = $00100;
  CREDUI_FLAGS_PASSWORD_ONLY_OK            = $00200;
  CREDUI_FLAGS_VALIDATE_USERNAME           = $00400;
  CREDUI_FLAGS_COMPLETE_USERNAME           = $00800;
  CREDUI_FLAGS_PERSIST                     = $01000; // Do not show "Save" checkbox, but persist credentials anyway
  CREDUI_FLAGS_SERVER_CREDENTIAL           = $04000;
  CREDUI_FLAGS_EXPECT_CONFIRMATION         = $20000; // do not persist unless caller later confirms credential via CredUIConfirmCredential() api
  CREDUI_FLAGS_GENERIC_CREDENTIALS         = $40000; // Credential is a generic credential
  CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS = $80000; // Credential has a username as the target
  CREDUI_FLAGS_KEEP_USERNAME               = $100000; // don't allow the user to change the supplied username

//
// Mask of flags valid for CredUIPromptForCredentials
//

  CREDUI_FLAGS_PROMPT_VALID =
    CREDUI_FLAGS_INCORRECT_PASSWORD or
    CREDUI_FLAGS_DO_NOT_PERSIST or
    CREDUI_FLAGS_REQUEST_ADMINISTRATOR or
    CREDUI_FLAGS_EXCLUDE_CERTIFICATES or
    CREDUI_FLAGS_REQUIRE_CERTIFICATE or
    CREDUI_FLAGS_SHOW_SAVE_CHECK_BOX or
    CREDUI_FLAGS_ALWAYS_SHOW_UI or
    CREDUI_FLAGS_REQUIRE_SMARTCARD or
    CREDUI_FLAGS_PASSWORD_ONLY_OK or
    CREDUI_FLAGS_VALIDATE_USERNAME or
    CREDUI_FLAGS_COMPLETE_USERNAME or
    CREDUI_FLAGS_PERSIST or
    CREDUI_FLAGS_SERVER_CREDENTIAL or
    CREDUI_FLAGS_EXPECT_CONFIRMATION or
    CREDUI_FLAGS_GENERIC_CREDENTIALS or
    CREDUI_FLAGS_USERNAME_TARGET_CREDENTIALS or
    CREDUI_FLAGS_KEEP_USERNAME;

//-----------------------------------------------------------------------------
// Functions
//-----------------------------------------------------------------------------

//
// Values of flags to CredWrite and CredWriteDomainCredentials
//

const
  CRED_PRESERVE_CREDENTIAL_BLOB = $1;

function CredWriteW(Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;

function CredWriteA(Credential: PCREDENTIALA; Flags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CredWrite(Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;
{$ELSE}
function CredWrite(Credential: PCREDENTIALA; Flags: DWORD): BOOL; stdcall;
{$ENDIF}

function CredReadW(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;

function CredReadA(TargetName: LPCSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;

{$IFDEF UNICODE}
function CredRead(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
{$ELSE}
function CredRead(TargetName: LPCSTR; Type_: DWORD; Flags: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
{$ENDIF}

function CredEnumerateW(Filter: LPCWSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;

function CredEnumerateA(Filter: LPCSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;

{$IFDEF UNICODE}
function CredEnumerate(Filter: LPCWSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
{$ELSE}
function CredEnumerate(Filter: LPCSTR; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
{$ENDIF}

function CredWriteDomainCredentialsW(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW; Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;

function CredWriteDomainCredentialsA(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA; Credential: PCREDENTIALA; Flags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CredWriteDomainCredentials(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW; Credential: PCREDENTIALW; Flags: DWORD): BOOL; stdcall;
{$ELSE}
function CredWriteDomainCredentials(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA; Credential: PCREDENTIALA; Flags: DWORD): BOOL; stdcall;
{$ENDIF}

//
// Values of flags to CredReadDomainCredentials
//

const
  CRED_CACHE_TARGET_INFORMATION = $1;

function CredReadDomainCredentialsW(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;

function CredReadDomainCredentialsA(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;

{$IFDEF UNICODE}
function CredReadDomainCredentials(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALW): BOOL; stdcall;
{$ELSE}
function CredReadDomainCredentials(TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA; Flags: DWORD; var Count: DWORD; var Credential: PCREDENTIALA): BOOL; stdcall;
{$ENDIF}

function CredDeleteW(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;

function CredDeleteA(TargetName: LPCSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CredDelete(TargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$ELSE}
function CredDelete(TargetName: LPCSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$ENDIF}

function CredRenameW(OldTargetName: LPCWSTR; NewTargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;

function CredRenameA(OldTargetName: LPCSTR; NewTargetName: LPCSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CredRename(OldTargetName: LPCWSTR; NewTargetName: LPCWSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$ELSE}
function CredRename(OldTargetName: LPCSTR; NewTargetName: LPCSTR; Type_: DWORD; Flags: DWORD): BOOL; stdcall;
{$ENDIF}

//
// Values of flags to CredGetTargetInfo
//

const
  CRED_ALLOW_NAME_RESOLUTION = $1;

function CredGetTargetInfoW(TargetName: LPCWSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW): BOOL; stdcall;

function CredGetTargetInfoA(TargetName: LPCSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA): BOOL; stdcall;

{$IFDEF UNICODE}
function CredGetTargetInfo(TargetName: LPCWSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATIONW): BOOL; stdcall;
{$ELSE}
function CredGetTargetInfo(TargetName: LPCSTR; Flags: DWORD; var TargetInfo: PCREDENTIAL_TARGET_INFORMATIONA): BOOL; stdcall;
{$ENDIF}

function CredMarshalCredentialW(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; var MarshaledCredential: LPWSTR): BOOL; stdcall;

function CredMarshalCredentialA(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; MarshaledCredential: LPSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function CredMarshalCredential(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; var MarshaledCredential: LPWSTR): BOOL; stdcall;
{$ELSE}
function CredMarshalCredential(CredType: CRED_MARSHAL_TYPE; Credential: PVOID; MarshaledCredential: LPSTR): BOOL; stdcall;
{$ENDIF}

function CredUnmarshalCredentialW(MarshaledCredential: LPCWSTR; CredType: PCRED_MARSHAL_TYPE; var Credential: PVOID): BOOL; stdcall;

function CredUnmarshalCredentialA(MarshaledCredential: LPCSTR; CredType: PCRED_MARSHAL_TYPE; Credential: PVOID): BOOL; stdcall;

{$IFDEF UNICODE}
function CredUnmarshalCredential(MarshaledCredential: LPCWSTR; CredType: PCRED_MARSHAL_TYPE; var Credential: PVOID): BOOL; stdcall;
{$ELSE}
function CredUnmarshalCredential(MarshaledCredential: LPCSTR; CredType: PCRED_MARSHAL_TYPE; Credential: PVOID): BOOL; stdcall;
{$ENDIF}

function CredIsMarshaledCredentialW(MarshaledCredential: LPCWSTR): BOOL; stdcall;

function CredIsMarshaledCredentialA(MarshaledCredential: LPCSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function CredIsMarshaledCredential(MarshaledCredential: LPCWSTR): BOOL; stdcall;
{$ELSE}
function CredIsMarshaledCredential(MarshaledCredential: LPCSTR): BOOL; stdcall;
{$ENDIF}

function CredGetSessionTypes(MaximumPersistCount: DWORD; MaximumPersist: LPDWORD): BOOL; stdcall;

procedure CredFree(Buffer: PVOID); stdcall;

function CredUIPromptForCredentialsW(pUiInfo: PCREDUI_INFOW; pszTargetName: LPCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PWSTR; ulUserNameBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;

function CredUIPromptForCredentialsA(pUiInfo: PCREDUI_INFOA; pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PSTR; ulUserNameBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function CredUIPromptForCredentials(pUiInfo: PCREDUI_INFOW; pszTargetName: LPCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PWSTR; ulUserNameBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$ELSE}
function CredUIPromptForCredentials(pUiInfo: PCREDUI_INFOA; pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD; pszUserName: PSTR; ulUserNameBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG; var save: BOOL; dwFlags: DWORD): DWORD; stdcall;
{$ENDIF}

function CredUIParseUserNameW(pszUserName: LPCWSTR; pszUser: PWSTR; ulUserBufferSize: ULONG; pszDomain: PWSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;

function CredUIParseUserNameA(pszUserName: PCSTR; pszUser: PSTR; ulUserBufferSize: ULONG; pszDomain: PSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;

{$IFDEF UNICODE}
function CredUIParseUserName(pszUserName: LPCWSTR; pszUser: PWSTR; ulUserBufferSize: ULONG; pszDomain: PWSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$ELSE}
function CredUIParseUserName(pszUserName: PCSTR; pszUser: PSTR; ulUserBufferSize: ULONG; pszDomain: PSTR; ulDomainBufferSize: ULONG): DWORD; stdcall;
{$ENDIF}

function CredUICmdLinePromptForCredentialsW(pszTargetName: LPCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PWSTR; ulUserBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;

function CredUICmdLinePromptForCredentialsA(pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PSTR; ulUserBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function CredUICmdLinePromptForCredentials(pszTargetName: LPCWSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PWSTR; ulUserBufferSize: ULONG; pszPassword: PWSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$ELSE}
function CredUICmdLinePromptForCredentials(pszTargetName: PCSTR; pContext: PCtxtHandle; dwAuthError: DWORD; UserName: PSTR; ulUserBufferSize: ULONG; pszPassword: PSTR; ulPasswordBufferSize: ULONG; pfSave: PBOOL; dwFlags: DWORD): DWORD; stdcall;
{$ENDIF}

//
// Call this API with bConfirm set to TRUE to confirm that the credential (previously created
// via CredUIGetCredentials or CredUIPromptForCredentials worked, or with bConfirm set to FALSE
// to indicate it didn't

function CredUIConfirmCredentialsW(pszTargetName: LPCWSTR; bConfirm: BOOL): DWORD; stdcall;

function CredUIConfirmCredentialsA(pszTargetName: PCSTR; bConfirm: BOOL): DWORD; stdcall;

{$IFDEF UNICODE}
function CredUIConfirmCredentials(pszTargetName: LPCWSTR; bConfirm: BOOL): DWORD; stdcall;
{$ELSE}
function CredUIConfirmCredentials(pszTargetName: PCSTR; bConfirm: BOOL): DWORD; stdcall;
{$ENDIF}

function CredUIStoreSSOCredW(pszRealm, pszUsername, pszPassword: LPCWSTR; bPersist: BOOL): DWORD; stdcall;

function CredUIReadSSOCredW(pszRealm: LPCWSTR; out ppszUsername: PWSTR): DWORD; stdcall;

implementation

function CREDUIP_IS_USER_PASSWORD_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (DWORD(_Status) = ERROR_LOGON_FAILURE) or
    (_Status = HRESULT_FROM_WIN32(ERROR_LOGON_FAILURE)) or
    (_Status = STATUS_LOGON_FAILURE) or
    (_Status = HRESULT_FROM_NT(STATUS_LOGON_FAILURE)) or
    (DWORD(_Status) = ERROR_ACCESS_DENIED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCESS_DENIED)) or
    (_Status = STATUS_ACCESS_DENIED) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCESS_DENIED)) or
    (DWORD(_Status) = ERROR_INVALID_PASSWORD) or
    (_Status = HRESULT_FROM_WIN32(ERROR_INVALID_PASSWORD)) or
    (_Status = STATUS_WRONG_PASSWORD) or
    (_Status = HRESULT_FROM_NT(STATUS_WRONG_PASSWORD)) or
    (_Status = SEC_E_NO_CREDENTIALS) or
    (_Status = SEC_E_LOGON_DENIED);
end;

function CREDUIP_IS_DOWNGRADE_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (DWORD(_Status) = ERROR_DOWNGRADE_DETECTED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_DOWNGRADE_DETECTED)) or
    (_Status = STATUS_DOWNGRADE_DETECTED) or
    (_Status = HRESULT_FROM_NT(STATUS_DOWNGRADE_DETECTED))
end;

function CREDUIP_IS_EXPIRED_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (DWORD(_Status) = ERROR_PASSWORD_EXPIRED) or
    (_Status = HRESULT_FROM_WIN32( ERROR_PASSWORD_EXPIRED)) or
    (_Status = STATUS_PASSWORD_EXPIRED) or
    (_Status = HRESULT_FROM_NT( STATUS_PASSWORD_EXPIRED)) or
    (DWORD(_Status) = ERROR_PASSWORD_MUST_CHANGE) or
    (_Status = HRESULT_FROM_WIN32( ERROR_PASSWORD_MUST_CHANGE)) or
    (_Status = STATUS_PASSWORD_MUST_CHANGE) or
    (_Status = HRESULT_FROM_NT( STATUS_PASSWORD_MUST_CHANGE)) or
    (_Status = NERR_PasswordExpired) or
    (_Status = HRESULT_FROM_WIN32(NERR_PasswordExpired));
end;

function CREDUI_IS_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result := CREDUIP_IS_USER_PASSWORD_ERROR(_Status) or CREDUIP_IS_DOWNGRADE_ERROR(_Status) or CREDUIP_IS_EXPIRED_ERROR(_Status);
end;

function CREDUI_NO_PROMPT_AUTHENTICATION_ERROR(_Status: NTSTATUS): BOOL;
begin
  Result :=
    (_Status = 1935{TODO Temp until WinError updated ERROR_AUTHENTICATION_FIREWALL_FAILED}) or
    (_Status = HRESULT_FROM_WIN32(1935{TODO ERROR_AUTHENTICATION_FIREWALL_FAILED})) or
    (_Status = STATUS_AUTHENTICATION_FIREWALL_FAILED) or
    (_Status = HRESULT_FROM_NT(STATUS_AUTHENTICATION_FIREWALL_FAILED)) or
    (DWORD(_Status) = ERROR_ACCOUNT_DISABLED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_DISABLED)) or
    (_Status = STATUS_ACCOUNT_DISABLED) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_DISABLED)) or
    (DWORD(_Status) = ERROR_ACCOUNT_RESTRICTION) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_RESTRICTION)) or
    (_Status = STATUS_ACCOUNT_RESTRICTION) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_RESTRICTION)) or
    (DWORD(_Status) = ERROR_ACCOUNT_LOCKED_OUT) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_LOCKED_OUT)) or
    (_Status = STATUS_ACCOUNT_LOCKED_OUT) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_LOCKED_OUT)) or
    (DWORD(_Status) = ERROR_ACCOUNT_EXPIRED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_ACCOUNT_EXPIRED)) or
    (_Status = STATUS_ACCOUNT_EXPIRED) or
    (_Status = HRESULT_FROM_NT(STATUS_ACCOUNT_EXPIRED)) or
    (DWORD(_Status) = ERROR_LOGON_TYPE_NOT_GRANTED) or
    (_Status = HRESULT_FROM_WIN32(ERROR_LOGON_TYPE_NOT_GRANTED)) or
    (_Status = STATUS_LOGON_TYPE_NOT_GRANTED) or
    (_Status = HRESULT_FROM_NT(STATUS_LOGON_TYPE_NOT_GRANTED));
end;

const
  credapi = 'advapi32.dll';
  credui = 'credui.dll';

{$IFDEF DYNAMIC_LINK}
var
  _CredWriteW: Pointer;

function CredWriteW;
begin
  GetProcedureAddress(_CredWriteW, credapi, 'CredWriteW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWriteW]
  end;
end;
{$ELSE}
function CredWriteW; external credapi name 'CredWriteW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredWriteA: Pointer;

function CredWriteA;
begin
  GetProcedureAddress(_CredWriteA, credapi, 'CredWriteA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWriteA]
  end;
end;
{$ELSE}
function CredWriteA; external credapi name 'CredWriteA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredWrite: Pointer;

function CredWrite;
begin
  GetProcedureAddress(_CredWrite, credapi, 'CredWriteW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWrite]
  end;
end;
{$ELSE}
function CredWrite; external credapi name 'CredWriteW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredWrite: Pointer;

function CredWrite;
begin
  GetProcedureAddress(_CredWrite, credapi, 'CredWriteA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWrite]
  end;
end;
{$ELSE}
function CredWrite; external credapi name 'CredWriteA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredReadW: Pointer;

function CredReadW;
begin
  GetProcedureAddress(_CredReadW, credapi, 'CredReadW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredReadW]
  end;
end;
{$ELSE}
function CredReadW; external credapi name 'CredReadW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredReadA: Pointer;

function CredReadA;
begin
  GetProcedureAddress(_CredReadA, credapi, 'CredReadA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredReadA]
  end;
end;
{$ELSE}
function CredReadA; external credapi name 'CredReadA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredRead: Pointer;

function CredRead;
begin
  GetProcedureAddress(_CredRead, credapi, 'CredReadW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredRead]
  end;
end;
{$ELSE}
function CredRead; external credapi name 'CredReadW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredRead: Pointer;

function CredRead;
begin
  GetProcedureAddress(_CredRead, credapi, 'CredReadA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredRead]
  end;
end;
{$ELSE}
function CredRead; external credapi name 'CredReadA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredEnumerateW: Pointer;

function CredEnumerateW;
begin
  GetProcedureAddress(_CredEnumerateW, credapi, 'CredEnumerateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredEnumerateW]
  end;
end;
{$ELSE}
function CredEnumerateW; external credapi name 'CredEnumerateW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredEnumerateA: Pointer;

function CredEnumerateA;
begin
  GetProcedureAddress(_CredEnumerateA, credapi, 'CredEnumerateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredEnumerateA]
  end;
end;
{$ELSE}
function CredEnumerateA; external credapi name 'CredEnumerateA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredEnumerate: Pointer;

function CredEnumerate;
begin
  GetProcedureAddress(_CredEnumerate, credapi, 'CredEnumerateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredEnumerate]
  end;
end;
{$ELSE}
function CredEnumerate; external credapi name 'CredEnumerateW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredEnumerate: Pointer;

function CredEnumerate;
begin
  GetProcedureAddress(_CredEnumerate, credapi, 'CredEnumerateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredEnumerate]
  end;
end;
{$ELSE}
function CredEnumerate; external credapi name 'CredEnumerateA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredWriteDomainCredentialsW: Pointer;

function CredWriteDomainCredentialsW;
begin
  GetProcedureAddress(_CredWriteDomainCredentialsW, credapi, 'CredWriteDomainCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWriteDomainCredentialsW]
  end;
end;
{$ELSE}
function CredWriteDomainCredentialsW; external credapi name 'CredWriteDomainCredentialsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredWriteDomainCredentialsA: Pointer;

function CredWriteDomainCredentialsA;
begin
  GetProcedureAddress(_CredWriteDomainCredentialsA, credapi, 'CredWriteDomainCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWriteDomainCredentialsA]
  end;
end;
{$ELSE}
function CredWriteDomainCredentialsA; external credapi name 'CredWriteDomainCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredWriteDomainCredentials: Pointer;

function CredWriteDomainCredentials;
begin
  GetProcedureAddress(_CredWriteDomainCredentials, credapi, 'CredWriteDomainCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWriteDomainCredentials]
  end;
end;
{$ELSE}
function CredWriteDomainCredentials; external credapi name 'CredWriteDomainCredentialsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredWriteDomainCredentials: Pointer;

function CredWriteDomainCredentials;
begin
  GetProcedureAddress(_CredWriteDomainCredentials, credapi, 'CredWriteDomainCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredWriteDomainCredentials]
  end;
end;
{$ELSE}
function CredWriteDomainCredentials; external credapi name 'CredWriteDomainCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredReadDomainCredentialsW: Pointer;

function CredReadDomainCredentialsW;
begin
  GetProcedureAddress(_CredReadDomainCredentialsW, credapi, 'CredReadDomainCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredReadDomainCredentialsW]
  end;
end;
{$ELSE}
function CredReadDomainCredentialsW; external credapi name 'CredReadDomainCredentialsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredReadDomainCredentialsA: Pointer;

function CredReadDomainCredentialsA;
begin
  GetProcedureAddress(_CredReadDomainCredentialsA, credapi, 'CredReadDomainCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredReadDomainCredentialsA]
  end;
end;
{$ELSE}
function CredReadDomainCredentialsA; external credapi name 'CredReadDomainCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredReadDomainCredentials: Pointer;

function CredReadDomainCredentials;
begin
  GetProcedureAddress(_CredReadDomainCredentials, credapi, 'CredReadDomainCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredReadDomainCredentials]
  end;
end;
{$ELSE}
function CredReadDomainCredentials; external credapi name 'CredReadDomainCredentialsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredReadDomainCredentials: Pointer;

function CredReadDomainCredentials;
begin
  GetProcedureAddress(_CredReadDomainCredentials, credapi, 'CredReadDomainCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredReadDomainCredentials]
  end;
end;
{$ELSE}
function CredReadDomainCredentials; external credapi name 'CredReadDomainCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredDeleteW: Pointer;

function CredDeleteW;
begin
  GetProcedureAddress(_CredDeleteW, credapi, 'CredDeleteW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredDeleteW]
  end;
end;
{$ELSE}
function CredDeleteW; external credapi name 'CredDeleteW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredDeleteA: Pointer;

function CredDeleteA;
begin
  GetProcedureAddress(_CredDeleteA, credapi, 'CredDeleteA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredDeleteA]
  end;
end;
{$ELSE}
function CredDeleteA; external credapi name 'CredDeleteA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredDelete: Pointer;

function CredDelete;
begin
  GetProcedureAddress(_CredDelete, credapi, 'CredDeleteW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredDelete]
  end;
end;
{$ELSE}
function CredDelete; external credapi name 'CredDeleteW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredDelete: Pointer;

function CredDelete;
begin
  GetProcedureAddress(_CredDelete, credapi, 'CredDeleteA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredDelete]
  end;
end;
{$ELSE}
function CredDelete; external credapi name 'CredDeleteA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredRenameW: Pointer;

function CredRenameW;
begin
  GetProcedureAddress(_CredRenameW, credapi, 'CredRenameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredRenameW]
  end;
end;
{$ELSE}
function CredRenameW; external credapi name 'CredRenameW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredRenameA: Pointer;

function CredRenameA;
begin
  GetProcedureAddress(_CredRenameA, credapi, 'CredRenameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredRenameA]
  end;
end;
{$ELSE}
function CredRenameA; external credapi name 'CredRenameA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredRename: Pointer;

function CredRename;
begin
  GetProcedureAddress(_CredRename, credapi, 'CredRenameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredRename]
  end;
end;
{$ELSE}
function CredRename; external credapi name 'CredRenameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredRename: Pointer;

function CredRename;
begin
  GetProcedureAddress(_CredRename, credapi, 'CredRenameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredRename]
  end;
end;
{$ELSE}
function CredRename; external credapi name 'CredRenameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredGetTargetInfoW: Pointer;

function CredGetTargetInfoW;
begin
  GetProcedureAddress(_CredGetTargetInfoW, credapi, 'CredGetTargetInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredGetTargetInfoW]
  end;
end;
{$ELSE}
function CredGetTargetInfoW; external credapi name 'CredGetTargetInfoW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredGetTargetInfoA: Pointer;

function CredGetTargetInfoA;
begin
  GetProcedureAddress(_CredGetTargetInfoA, credapi, 'CredGetTargetInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredGetTargetInfoA]
  end;
end;
{$ELSE}
function CredGetTargetInfoA; external credapi name 'CredGetTargetInfoA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredGetTargetInfo: Pointer;

function CredGetTargetInfo;
begin
  GetProcedureAddress(_CredGetTargetInfo, credapi, 'CredGetTargetInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredGetTargetInfo]
  end;
end;
{$ELSE}
function CredGetTargetInfo; external credapi name 'CredGetTargetInfoA';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredGetTargetInfo: Pointer;

function CredGetTargetInfo;
begin
  GetProcedureAddress(_CredGetTargetInfo, credapi, 'CredGetTargetInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredGetTargetInfo]
  end;
end;
{$ELSE}
function CredGetTargetInfo; external credapi name 'CredGetTargetInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredMarshalCredentialW: Pointer;

function CredMarshalCredentialW;
begin
  GetProcedureAddress(_CredMarshalCredentialW, credapi, 'CredMarshalCredentialW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredMarshalCredentialW]
  end;
end;
{$ELSE}
function CredMarshalCredentialW; external credapi name 'CredMarshalCredentialW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredMarshalCredentialA: Pointer;

function CredMarshalCredentialA;
begin
  GetProcedureAddress(_CredMarshalCredentialA, credapi, 'CredMarshalCredentialA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredMarshalCredentialA]
  end;
end;
{$ELSE}
function CredMarshalCredentialA; external credapi name 'CredMarshalCredentialA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredMarshalCredential: Pointer;

function CredMarshalCredential;
begin
  GetProcedureAddress(_CredMarshalCredential, credapi, 'CredMarshalCredentialW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredMarshalCredential]
  end;
end;
{$ELSE}
function CredMarshalCredential; external credapi name 'CredMarshalCredentialW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredMarshalCredential: Pointer;

function CredMarshalCredential;
begin
  GetProcedureAddress(_CredMarshalCredential, credapi, 'CredMarshalCredentialA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredMarshalCredential]
  end;
end;
{$ELSE}
function CredMarshalCredential; external credapi name 'CredMarshalCredentialA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredUnmarshalCredentialW: Pointer;

function CredUnmarshalCredentialW;
begin
  GetProcedureAddress(_CredUnmarshalCredentialW, credapi, 'CredUnmarshalCredentialW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUnmarshalCredentialW]
  end;
end;
{$ELSE}
function CredUnmarshalCredentialW; external credapi name 'CredUnmarshalCredentialW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUnmarshalCredentialA: Pointer;

function CredUnmarshalCredentialA;
begin
  GetProcedureAddress(_CredUnmarshalCredentialA, credapi, 'CredUnmarshalCredentialA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUnmarshalCredentialA]
  end;
end;
{$ELSE}
function CredUnmarshalCredentialA; external credapi name 'CredUnmarshalCredentialA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUnmarshalCredential: Pointer;

function CredUnmarshalCredential;
begin
  GetProcedureAddress(_CredUnmarshalCredential, credapi, 'CredUnmarshalCredentialW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUnmarshalCredential]
  end;
end;
{$ELSE}
function CredUnmarshalCredential; external credapi name 'CredUnmarshalCredentialW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUnmarshalCredential: Pointer;

function CredUnmarshalCredential;
begin
  GetProcedureAddress(_CredUnmarshalCredential, credapi, 'CredUnmarshalCredentialA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUnmarshalCredential]
  end;
end;
{$ELSE}
function CredUnmarshalCredential; external credapi name 'CredUnmarshalCredentialA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredIsMarshaledCredentialW: Pointer;

function CredIsMarshaledCredentialW;
begin
  GetProcedureAddress(_CredIsMarshaledCredentialW, credapi, 'CredIsMarshaledCredentialW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredIsMarshaledCredentialW]
  end;
end;
{$ELSE}
function CredIsMarshaledCredentialW; external credapi name 'CredIsMarshaledCredentialW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredIsMarshaledCredentialA: Pointer;

function CredIsMarshaledCredentialA;
begin
  GetProcedureAddress(_CredIsMarshaledCredentialA, credapi, 'CredIsMarshaledCredentialA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredIsMarshaledCredentialA]
  end;
end;
{$ELSE}
function CredIsMarshaledCredentialA; external credapi name 'CredIsMarshaledCredentialA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredIsMarshaledCredential: Pointer;

function CredIsMarshaledCredential;
begin
  GetProcedureAddress(_CredIsMarshaledCredential, credapi, 'CredIsMarshaledCredentialW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredIsMarshaledCredential]
  end;
end;
{$ELSE}
function CredIsMarshaledCredential; external credapi name 'CredIsMarshaledCredentialW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredIsMarshaledCredential: Pointer;

function CredIsMarshaledCredential;
begin
  GetProcedureAddress(_CredIsMarshaledCredential, credapi, 'CredIsMarshaledCredentialA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredIsMarshaledCredential]
  end;
end;
{$ELSE}
function CredIsMarshaledCredential; external credapi name 'CredIsMarshaledCredentialA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredGetSessionTypes: Pointer;

function CredGetSessionTypes;
begin
  GetProcedureAddress(_CredGetSessionTypes, credapi, 'CredGetSessionTypes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredGetSessionTypes]
  end;
end;
{$ELSE}
function CredGetSessionTypes; external credapi name 'CredGetSessionTypes';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredFree: Pointer;

procedure CredFree;
begin
  GetProcedureAddress(_CredFree, credapi, 'CredFree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredFree]
  end;
end;
{$ELSE}
procedure CredFree; external credapi name 'CredFree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIPromptForCredentialsW: Pointer;

function CredUIPromptForCredentialsW;
begin
  GetProcedureAddress(_CredUIPromptForCredentialsW, credui, 'CredUIPromptForCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIPromptForCredentialsW]
  end;
end;
{$ELSE}
function CredUIPromptForCredentialsW; external credui name 'CredUIPromptForCredentialsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIPromptForCredentialsA: Pointer;

function CredUIPromptForCredentialsA;
begin
  GetProcedureAddress(_CredUIPromptForCredentialsA, credui, 'CredUIPromptForCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIPromptForCredentialsA]
  end;
end;
{$ELSE}
function CredUIPromptForCredentialsA; external credui name 'CredUIPromptForCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIPromptForCredentials: Pointer;

function CredUIPromptForCredentials;
begin
  GetProcedureAddress(_CredUIPromptForCredentials, credui, 'CredUIPromptForCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIPromptForCredentials]
  end;
end;
{$ELSE}
function CredUIPromptForCredentials; external credui name 'CredUIPromptForCredentialsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIPromptForCredentials: Pointer;

function CredUIPromptForCredentials;
begin
  GetProcedureAddress(_CredUIPromptForCredentials, credui, 'CredUIPromptForCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIPromptForCredentials]
  end;
end;
{$ELSE}
function CredUIPromptForCredentials; external credui name 'CredUIPromptForCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIParseUserNameW: Pointer;

function CredUIParseUserNameW;
begin
  GetProcedureAddress(_CredUIParseUserNameW, credui, 'CredUIParseUserNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIParseUserNameW]
  end;
end;
{$ELSE}
function CredUIParseUserNameW; external credui name 'CredUIParseUserNameW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIParseUserNameA: Pointer;

function CredUIParseUserNameA;
begin
  GetProcedureAddress(_CredUIParseUserNameA, credui, 'CredUIParseUserNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIParseUserNameA]
  end;
end;
{$ELSE}
function CredUIParseUserNameA; external credui name 'CredUIParseUserNameA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIParseUserName: Pointer;

function CredUIParseUserName;
begin
  GetProcedureAddress(_CredUIParseUserName, credui, 'CredUIParseUserNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIParseUserName]
  end;
end;
{$ELSE}
function CredUIParseUserName; external credui name 'CredUIParseUserNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIParseUserName: Pointer;

function CredUIParseUserName;
begin
  GetProcedureAddress(_CredUIParseUserName, credui, 'CredUIParseUserNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIParseUserName]
  end;
end;
{$ELSE}
function CredUIParseUserName; external credui name 'CredUIParseUserNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredUICmdLinePromptForCredentialsW: Pointer;

function CredUICmdLinePromptForCredentialsW;
begin
  GetProcedureAddress(_CredUICmdLinePromptForCredentialsW, credui, 'CredUICmdLinePromptForCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUICmdLinePromptForCredentialsW]
  end;
end;
{$ELSE}
function CredUICmdLinePromptForCredentialsW; external credui name 'CredUICmdLinePromptForCredentialsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUICmdLinePromptForCredentialsA: Pointer;

function CredUICmdLinePromptForCredentialsA;
begin
  GetProcedureAddress(_CredUICmdLinePromptForCredentialsA, credui, 'CredUICmdLinePromptForCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUICmdLinePromptForCredentialsA]
  end;
end;
{$ELSE}
function CredUICmdLinePromptForCredentialsA; external credui name 'CredUICmdLinePromptForCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUICmdLinePromptForCredentials: Pointer;

function CredUICmdLinePromptForCredentials;
begin
  GetProcedureAddress(_CredUICmdLinePromptForCredentials, credui, 'CredUICmdLinePromptForCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUICmdLinePromptForCredentials]
  end;
end;
{$ELSE}
function CredUICmdLinePromptForCredentials; external credui name 'CredUICmdLinePromptForCredentialsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUICmdLinePromptForCredentials: Pointer;

function CredUICmdLinePromptForCredentials;
begin
  GetProcedureAddress(_CredUICmdLinePromptForCredentials, credui, 'CredUICmdLinePromptForCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUICmdLinePromptForCredentials]
  end;
end;
{$ELSE}
function CredUICmdLinePromptForCredentials; external credui name 'CredUICmdLinePromptForCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIConfirmCredentialsW: Pointer;

function CredUIConfirmCredentialsW;
begin
  GetProcedureAddress(_CredUIConfirmCredentialsW, credui, 'CredUIConfirmCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIConfirmCredentialsW]
  end;
end;
{$ELSE}
function CredUIConfirmCredentialsW; external credui name 'CredUIConfirmCredentialsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIConfirmCredentialsA: Pointer;

function CredUIConfirmCredentialsA;
begin
  GetProcedureAddress(_CredUIConfirmCredentialsA, credui, 'CredUIConfirmCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIConfirmCredentialsA]
  end;
end;
{$ELSE}
function CredUIConfirmCredentialsA; external credui name 'CredUIConfirmCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIConfirmCredentials: Pointer;

function CredUIConfirmCredentials;
begin
  GetProcedureAddress(_CredUIConfirmCredentials, credui, 'CredUIConfirmCredentialsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIConfirmCredentials]
  end;
end;
{$ELSE}
function CredUIConfirmCredentials; external credui name 'CredUIConfirmCredentialsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIConfirmCredentials: Pointer;

function CredUIConfirmCredentials;
begin
  GetProcedureAddress(_CredUIConfirmCredentials, credui, 'CredUIConfirmCredentialsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIConfirmCredentials]
  end;
end;
{$ELSE}
function CredUIConfirmCredentials; external credui name 'CredUIConfirmCredentialsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIStoreSSOCredW: Pointer;

function CredUIStoreSSOCredW;
begin
  GetProcedureAddress(_CredUIStoreSSOCredW, credui, 'CredUIStoreSSOCredW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIStoreSSOCredW]
  end;
end;
{$ELSE}
function CredUIStoreSSOCredW; external credui name 'CredUIStoreSSOCredW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CredUIReadSSOCredW: Pointer;

function CredUIReadSSOCredW;
begin
  GetProcedureAddress(_CredUIReadSSOCredW, credui, 'CredUIReadSSOCredW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CredUIReadSSOCredW]
  end;
end;
{$ELSE}
function CredUIReadSSOCredW; external credui name 'CredUIReadSSOCredW';
{$ENDIF DYNAMIC_LINK}

end.
