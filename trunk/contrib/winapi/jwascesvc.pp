{******************************************************************************}
{                                                       	               }
{ Security Service Attachements API interface Unit for Object Pascal           }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: scesvc.h, released June 2000. The original Pascal      }
{ code is: SceSvc.pas, released December 2000. The initial developer of the    }
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

unit JwaSceSvc;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "scesvc.h"'}
{$HPPEMIT ''}
{$HPPEMIT 'typedef SCESVC_HANDLE *PSCESVC_HANDLE'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

type
  SCESTATUS = DWORD;

const
  SCESTATUS_SUCCESS             = 0;
  SCESTATUS_INVALID_PARAMETER   = 1;
  SCESTATUS_RECORD_NOT_FOUND    = 2;
  SCESTATUS_INVALID_DATA        = 3;
  SCESTATUS_OBJECT_EXIST        = 4;
  SCESTATUS_BUFFER_TOO_SMALL    = 5;
  SCESTATUS_PROFILE_NOT_FOUND   = 6;
  SCESTATUS_BAD_FORMAT          = 7;
  SCESTATUS_NOT_ENOUGH_RESOURCE = 8;
  SCESTATUS_ACCESS_DENIED       = 9;
  SCESTATUS_CANT_DELETE         = 10;
  SCESTATUS_PREFIX_OVERFLOW     = 11;
  SCESTATUS_OTHER_ERROR         = 12;
  SCESTATUS_ALREADY_RUNNING     = 13;
  SCESTATUS_SERVICE_NOT_SUPPORT = 14;
  SCESTATUS_MOD_NOT_FOUND       = 15;
  SCESTATUS_EXCEPTION_IN_SERVER = 16;
  SCESTATUS_NO_TEMPLATE_GIVEN   = 17;
  SCESTATUS_NO_MAPPING          = 18;
  SCESTATUS_TRUST_FAIL          = 19;

type
  PSCESVC_CONFIGURATION_LINE = ^SCESVC_CONFIGURATION_LINE;
  _SCESVC_CONFIGURATION_LINE_ = record
    Key: LPTSTR;
    Value: LPTSTR;
    ValueLen: DWORD; // number of bytes
  end;
  SCESVC_CONFIGURATION_LINE = _SCESVC_CONFIGURATION_LINE_;
  TScesvcConfigurationLine = SCESVC_CONFIGURATION_LINE;
  PScesvcConfigurationLine = PSCESVC_CONFIGURATION_LINE;

  PSCESVC_CONFIGURATION_INFO = ^SCESVC_CONFIGURATION_INFO;
  _SCESVC_CONFIGURATION_INFO_ = record
    Count: DWORD;
    Lines: PSCESVC_CONFIGURATION_INFO;
  end;
  SCESVC_CONFIGURATION_INFO = _SCESVC_CONFIGURATION_INFO_;
  TScesvcConfigurationInfo = SCESVC_CONFIGURATION_INFO;
  PScesvcConfigurationInfo = PSCESVC_CONFIGURATION_INFO;

  SCE_HANDLE = PVOID;
  SCE_ENUMERATION_CONTEXT = ULONG;
  PSCE_ENUMERATION_CONTEXT = ^SCE_ENUMERATION_CONTEXT;

  _SCESVC_INFO_TYPE = (
    SceSvcConfigurationInfo,
    SceSvcMergedPolicyInfo,
    SceSvcAnalysisInfo,
    SceSvcInternalUse); // !!!do not use this type!!!
  SCESVC_INFO_TYPE = _SCESVC_INFO_TYPE;
  TScesvcInfoType = _SCESVC_INFO_TYPE;

const
  SCE_ROOT_PATH = 'Software\Microsoft\Windows NT\CurrentVersion\SeCEdit';

  SCE_ROOT_SERVICE_PATH = SCE_ROOT_PATH + '\SvcEngs';

type
  SCESVC_HANDLE = PVOID;

  PSCESVC_HANDLE = ^SCESVC_HANDLE;
  {$NODEFINE PSCESVC_HANDLE}

  PSCESVC_ANALYSIS_LINE = ^SCESVC_ANALYSIS_LINE;
  _SCESVC_ANALYSIS_LINE_ = record
    Key: LPTSTR;
    Value: PBYTE;
    ValueLen: DWORD; // number of bytes
  end;
  SCESVC_ANALYSIS_LINE = _SCESVC_ANALYSIS_LINE_;
  TScesvcAnalysisLine = SCESVC_ANALYSIS_LINE;
  PScesvcAnalysisLine = PSCESVC_ANALYSIS_LINE;

  PSCESVC_ANALYSIS_INFO = ^SCESVC_ANALYSIS_INFO;
  _SCESVC_ANALYSIS_INFO_ = record
    Count: DWORD;
    Lines: PSCESVC_ANALYSIS_LINE;
  end;
  SCESVC_ANALYSIS_INFO = _SCESVC_ANALYSIS_INFO_;
  TScesvcAnalysisInfo = SCESVC_ANALYSIS_INFO;
  PScesvcAnalysisInfo = PSCESVC_ANALYSIS_INFO;

const
  SCESVC_ENUMERATION_MAX = 100;

type
  PFSCE_QUERY_INFO = function (sceHandle: SCE_HANDLE; sceType: SCESVC_INFO_TYPE;
    lpPrefix: LPTSTR; bExact: BOOL; ppvInfo: PPVOID;
    var psceEnumHandle: SCE_ENUMERATION_CONTEXT): SCESTATUS; stdcall;

  PFSCE_SET_INFO = function (sceHandle: SCE_HANDLE; sceType: SCESVC_INFO_TYPE;
    lpPrefix: LPTSTR; bExact: BOOL; pvInfo: PVOID): SCESTATUS; stdcall;

  PFSCE_FREE_INFO = function (pvServiceInfo: PVOID): SCESTATUS; stdcall;

const
  SCE_LOG_LEVEL_ALWAYS = 0;
  SCE_LOG_LEVEL_ERROR  = 1;
  SCE_LOG_LEVEL_DETAIL = 2;
  SCE_LOG_LEVEL_DEBUG  = 3;

type
  PFSCE_LOG_INFO = function (ErrLevel: Integer; Win32rc: DWORD; pErrFmt: LPTSTR {...}): SCESTATUS; stdcall;

  PSCESVC_CALLBACK_INFO = ^SCESVC_CALLBACK_INFO;
  _SCESVC_CALLBACK_INFO_ = record
    sceHandle: SCE_HANDLE;
    pfQueryInfo: PFSCE_QUERY_INFO;
    pfSetInfo: PFSCE_SET_INFO;
    pfFreeInfo: PFSCE_FREE_INFO;
    pfLogInfo: PFSCE_LOG_INFO;
  end;
  SCESVC_CALLBACK_INFO = _SCESVC_CALLBACK_INFO_;
  TScesvcCallbackInfo = SCESVC_CALLBACK_INFO;
  PScesvcCallbackInfo = PSCESVC_CALLBACK_INFO;

  PF_ConfigAnalyzeService = function (pSceCbInfo: PSCESVC_CALLBACK_INFO): SCESTATUS; stdcall;

  PF_UpdateService = function (pSceCbInfo: PSCESVC_CALLBACK_INFO;
    ServiceInfo: PSCESVC_CONFIGURATION_INFO): SCESTATUS; stdcall;

//
// headers for extension snap-ins
// only define this for NT5
//

const
  struuidNodetypeSceTemplateServices = '{24a7f717-1f0c-11d1-affb-00c04fb984f9}';
  lstruuidNodetypeSceTemplateServices = '{24a7f717-1f0c-11d1-affb-00c04fb984f9}';
  cNodetypeSceTemplateServices: GUID = (
    D1:$24a7f717; D2:$1f0c; D3:$11d1; D4:($af, $fb, $0, $c0, $4f, $b9, $84, $f9));
  struuidNodetypeSceAnalysisServices = '{678050c7-1ff8-11d1-affb-00c04fb984f9}';
  lstruuidNodetypeSceAnalysisServices = '{678050c7-1ff8-11d1-affb-00c04fb984f9}';
  cNodetypeSceAnalysisServices: GUID = (
    D1:$678050c7; D2:$1ff8; D3:$11d1; D4:($af, $fb, $0, $c0, $4f, $b9, $84, $f9));
  struuidNodetypeSceEventLog = '{2ce06698-4bf3-11d1-8c30-00c04fb984f9}';
  lstruuidNodetypeSceEventLog = '{2ce06698-4bf3-11d1-8c30-00c04fb984f9}';
  cNodetypeSceEventLog: GUID = (
    D1:$2ce06698; D2:$4bf3; D3:$11d1; D4:($8c, $30, $0, $c0, $4f, $b9, $84, $f9));

type
  LPSCESVC_CONFIGURATION_INFO = ^PSCESVC_CONFIGURATION_INFO;
  LPSCESVC_ANALYSIS_INFO = ^PSCESVC_ANALYSIS_INFO;

const
  CCF_SCESVC_ATTACHMENT       = 'CCF_SCESVC_ATTACHMENT';
  CCF_SCESVC_ATTACHMENT_DATA  = 'CCF_SCESVC_ATTACHMENT_DATA';

//
// implemented by service attachment
//

const
  IID_ISceSvcAttachmentPersistInfo: GUID = (
    D1:$6d90e0d0; D2:$200d; D3:$11d1; D4:($af, $fb, $0, $c0, $4f, $b9, $84, $f9));

  SID_ISceSvcAttachmentPersistInfo = '{6d90e0d0-200d-11d1-affb-00c04fb984f9}';

type
  ISceSvcAttachmentPersistInfo = interface (IUnknown)
    [SID_ISceSvcAttachmentPersistInfo]
    function Save(lpTemplateName: LPTSTR; scesvcHandle: PSCESVC_HANDLE;
      ppvData: PPVOID; pbOverwriteAll: PBOOL): HRESULT; stdcall;
    function IsDirty(lpTemplateName: LPTSTR): HRESULT; stdcall;
    function FreeBuffer(pvData: PVOID): HRESULT; stdcall;
  end;

  LPSCESVCATTACHMENTPERSISTINFO = ISceSvcAttachmentPersistInfo;

//
// implemented by SecEdit
//

const
  IID_ISceSvcAttachmentData: GUID = (
    D1:$17c35fde; D2:$200d; D3:$11d1; D4:($af, $fb, $0, $c0, $4f, $b9, $84, $f9));

  SID_ISceSvcAttachmentData = '{17c35fde-200d-11d1-affb-00c04fb984f9}';

type
  ISceSvcAttachmentData = interface (IUnknown)
    [SID_ISceSvcAttachmentData]
    function GetData(scesvcHandle: SCESVC_HANDLE; sceType: SCESVC_INFO_TYPE;
      ppvData: PPVOID; psceEnumHandle: PSCE_ENUMERATION_CONTEXT): HRESULT; stdcall;
    function Initialize(lpServiceName, lpTemplateName: LPCTSTR;
      lpSceSvcPersistInfo: LPSCESVCATTACHMENTPERSISTINFO;
      pscesvcHandle: PSCESVC_HANDLE): HRESULT; stdcall;
    function FreeBuffer(pvData: PVOID): HRESULT; stdcall;
    function CloseHandle(scesvcHandle: SCESVC_HANDLE): HRESULT; stdcall;
  end;

  LPSCESVCATTACHMENTDATA = ISceSvcAttachmentData;

implementation

end.
