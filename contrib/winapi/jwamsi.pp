{******************************************************************************}
{                                                       	               }
{ Windows Installer API interface Unit for Object Pascal                       }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: msi.h, released June 2000. The original Pascal         }
{ code is: Msi.pas, released June 2001. The initial developer of the           }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Contributors: Steve Moss (spm@coco.co.uk)                                    }
{                                                                              }
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

unit JwaMsi;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "msi.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType, JwaWinCrypt { for PCCERT_CONTEXT };

type // TODO
  LPVOID = Pointer;
  PHWND = ^HWND;

{$DEFINE WIN32_NT_GREATER_EQUAL_0500}

{$IFDEF WIN32_WINNT_GREATER_EQUAL_0501}
  {$DEFINE _WIN32_MSI_200}
  {$DEFINE _WIN32_MSI_GREATER_EQUAL_110} // not in original!!  
{$ELSE}
  {$IFDEF WIN32_NT_GREATER_EQUAL_0500}
    {$DEFINE WIN32_MSI_110}
  {$ELSE}
    {$DEFINE WIN32_MSI_100}
  {$ENDIF WIN32_NT_GREATER_EQUAL_0500}
{$ENDIF WIN32_WINNT_GREATER_EQUAL_0501}



(*****************************************************************************\
*                                                                             *
* msi.h - - Interface for external access to Installer Service                *
*                                                                             *
* Version 1.0 - 1.2                                                           *
*                                                                             *
* NOTES:  All buffers sizes are TCHAR count, null included only on input      *
*         Return argument pointers may be null if not interested in value     *
*                                                                             *
* Copyright (c) 1999-2000, Microsoft Corp.      All rights reserved.          *
*                                                                             *
\*****************************************************************************)

// --------------------------------------------------------------------------
// Installer generic handle definitions
// --------------------------------------------------------------------------

type
  MSIHANDLE = DWORD;     // abstract generic handle, 0 == no handle
  TMsiHandle = MSIHANDLE;

// Close a open handle of any type
// All handles obtained from API calls must be closed when no longer needed
// Normally succeeds, returning TRUE.

function MsiCloseHandle(hAny: MSIHANDLE): UINT; stdcall;

// Close all handles open in the process, a diagnostic call
// This should NOT be used as a cleanup mechanism -- use PMSIHANDLE class
// Can be called at termination to assure that all handles have been closed
// Returns 0 if all handles have been close, else number of open handles

function MsiCloseAllHandles: UINT; stdcall;

// Install message type for callback is a combination of the following:
//  A message box style:      MB_*, where MB_OK is the default
//  A message box icon type:  MB_ICON*, where no icon is the default
//  A default button:         MB_DEFBUTTON?, where MB_DEFBUTTON1 is the default
//  One of the following install message types, no default

const
  INSTALLMESSAGE_FATALEXIT      = $00000000; // premature termination, possibly fatal OOM
  INSTALLMESSAGE_ERROR          = $01000000; // formatted error message
  INSTALLMESSAGE_WARNING        = $02000000; // formatted warning message
  INSTALLMESSAGE_USER           = $03000000; // user request message
  INSTALLMESSAGE_INFO           = $04000000; // informative message for log
  INSTALLMESSAGE_FILESINUSE     = $05000000; // list of files in use that need to be replaced
  INSTALLMESSAGE_RESOLVESOURCE  = $06000000; // request to determine a valid source location
  INSTALLMESSAGE_OUTOFDISKSPACE = $07000000; // insufficient disk space message
  INSTALLMESSAGE_ACTIONSTART    = $08000000; // start of action: action name & description
  INSTALLMESSAGE_ACTIONDATA     = $09000000; // formatted data associated with individual action item
  INSTALLMESSAGE_PROGRESS       = $0A000000; // progress gauge info: units so far, total
  INSTALLMESSAGE_COMMONDATA     = $0B000000; // product info for dialog: language Id, dialog caption
  INSTALLMESSAGE_INITIALIZE     = $0C000000; // sent prior to UI initialization, no string data
  INSTALLMESSAGE_TERMINATE      = $0D000000; // sent after UI termination, no string data
  INSTALLMESSAGE_SHOWDIALOG     = $0E000000; // sent prior to display or authored dialog or wizard

type
  INSTALLMESSAGE = Longint;
  TInstallMessage = INSTALLMESSAGE;

// external error handler supplied to installation API functions

type
  INSTALLUI_HANDLERA = function (pvContext: LPVOID; iMessageType: UINT; szMessage: LPCSTR): Integer; stdcall;
  TInstallUIHandlerA = INSTALLUI_HANDLERA;
  INSTALLUI_HANDLERW = function (pvContext: LPVOID; iMessageType: UINT; szMessage: LPCWSTR): Integer; stdcall;
  TInstallUIHandlerW = INSTALLUI_HANDLERW;

{$IFDEF UNICODE}
  INSTALLUI_HANDLER = INSTALLUI_HANDLERW;
  TInstallUIHandler = TInstallUIHandlerW;
{$ELSE}
  INSTALLUI_HANDLER = INSTALLUI_HANDLERA;
  TInstallUIHandler = TInstallUIHandlerA;  
{$ENDIF}

const
  INSTALLUILEVEL_NOCHANGE = 0;    // UI level is unchanged
  INSTALLUILEVEL_DEFAULT  = 1;    // default UI is used
  INSTALLUILEVEL_NONE     = 2;    // completely silent installation
  INSTALLUILEVEL_BASIC    = 3;    // simple progress and error handling
  INSTALLUILEVEL_REDUCED  = 4;    // authored UI, wizard dialogs suppressed
  INSTALLUILEVEL_FULL     = 5;    // authored UI with wizards, progress, errors
  INSTALLUILEVEL_ENDDIALOG    = $80; // display success/failure dialog at end of install
  INSTALLUILEVEL_PROGRESSONLY = $40; // display only progress dialog
  INSTALLUILEVEL_HIDECANCEL   = $20; // do not display the cancel button in basic UI
  INSTALLUILEVEL_SOURCERESONLY = $100; // force display of source resolution even if quiet

type
  INSTALLUILEVEL = Longint;
  TInstallUILevel = INSTALLUILEVEL;

const
  INSTALLSTATE_NOTUSED      = -7;  // component disabled
  INSTALLSTATE_BADCONFIG    = -6;  // configuration data corrupt
  INSTALLSTATE_INCOMPLETE   = -5;  // installation suspended or in progress
  INSTALLSTATE_SOURCEABSENT = -4;  // run from source, source is unavailable
  INSTALLSTATE_MOREDATA     = -3;  // return buffer overflow
  INSTALLSTATE_INVALIDARG   = -2;  // invalid function argument
  INSTALLSTATE_UNKNOWN      = -1;  // unrecognized product or feature
  INSTALLSTATE_BROKEN       =  0;  // broken
  INSTALLSTATE_ADVERTISED   =  1;  // advertised feature
  INSTALLSTATE_REMOVED      =  1;  // component being removed (action state, not settable)
  INSTALLSTATE_ABSENT       =  2;  // uninstalled (or action state absent but clients remain)
  INSTALLSTATE_LOCAL        =  3;  // installed on local drive
  INSTALLSTATE_SOURCE       =  4;  // run from source, CD or net
  INSTALLSTATE_DEFAULT      =  5;  // use default, local or source

type
  INSTALLSTATE = Longint;
  TInstallState = INSTALLSTATE;

const
  USERINFOSTATE_MOREDATA   = -3;  // return buffer overflow
  USERINFOSTATE_INVALIDARG = -2;  // invalid function argument
  USERINFOSTATE_UNKNOWN    = -1;  // unrecognized product
  USERINFOSTATE_ABSENT     =  0;  // user info and PID not initialized
  USERINFOSTATE_PRESENT    =  1;  // user info and PID initialized

type
  USERINFOSTATE = DWORD;
  TUserInfoState = USERINFOSTATE;

const
  INSTALLLEVEL_DEFAULT = 0;      // install authored default
  INSTALLLEVEL_MINIMUM = 1;      // install only required features
  INSTALLLEVEL_MAXIMUM = $FFFF;  // install all features

type
  INSTALLLEVEL = DWORD;                   // intermediate levels dependent on authoring
  TInstallLevel = INSTALLLEVEL;

const
  REINSTALLMODE_REPAIR           = $00000001;  // Reserved bit - currently ignored
  REINSTALLMODE_FILEMISSING      = $00000002;  // Reinstall only if file is missing
  REINSTALLMODE_FILEOLDERVERSION = $00000004;  // Reinstall if file is missing, or older version
  REINSTALLMODE_FILEEQUALVERSION = $00000008;  // Reinstall if file is missing, or equal or older version
  REINSTALLMODE_FILEEXACT        = $00000010;  // Reinstall if file is missing, or not exact version
  REINSTALLMODE_FILEVERIFY       = $00000020;  // checksum executables, reinstall if missing or corrupt
  REINSTALLMODE_FILEREPLACE      = $00000040;  // Reinstall all files, regardless of version
  REINSTALLMODE_MACHINEDATA      = $00000080;  // insure required machine reg entries
  REINSTALLMODE_USERDATA         = $00000100;  // insure required user reg entries
  REINSTALLMODE_SHORTCUT         = $00000200;  // validate shortcuts items
  REINSTALLMODE_PACKAGE          = $00000400;  // use re-cache source install package

type
  REINSTALLMODE = DWORD;
  TReinstallMode = REINSTALLMODE;

// bit flags for use with MsiEnableLog and MsiSetExternalUI

const
  INSTALLLOGMODE_FATALEXIT      = (1 shl (INSTALLMESSAGE_FATALEXIT      shr 24));
  INSTALLLOGMODE_ERROR          = (1 shl (INSTALLMESSAGE_ERROR          shr 24));
  INSTALLLOGMODE_WARNING        = (1 shl (INSTALLMESSAGE_WARNING        shr 24));
  INSTALLLOGMODE_USER           = (1 shl (INSTALLMESSAGE_USER           shr 24));
  INSTALLLOGMODE_INFO           = (1 shl (INSTALLMESSAGE_INFO           shr 24));
  INSTALLLOGMODE_RESOLVESOURCE  = (1 shl (INSTALLMESSAGE_RESOLVESOURCE  shr 24));
  INSTALLLOGMODE_OUTOFDISKSPACE = (1 shl (INSTALLMESSAGE_OUTOFDISKSPACE shr 24));
  INSTALLLOGMODE_ACTIONSTART    = (1 shl (INSTALLMESSAGE_ACTIONSTART    shr 24));
  INSTALLLOGMODE_ACTIONDATA     = (1 shl (INSTALLMESSAGE_ACTIONDATA     shr 24));
  INSTALLLOGMODE_COMMONDATA     = (1 shl (INSTALLMESSAGE_COMMONDATA     shr 24));
  INSTALLLOGMODE_PROPERTYDUMP   = (1 shl (INSTALLMESSAGE_PROGRESS       shr 24)); // log only
  INSTALLLOGMODE_VERBOSE        = (1 shl (INSTALLMESSAGE_INITIALIZE     shr 24)); // log only
  INSTALLLOGMODE_EXTRADEBUG     = (1 shl (INSTALLMESSAGE_TERMINATE      shr 24)); // log only
  INSTALLLOGMODE_PROGRESS       = (1 shl (INSTALLMESSAGE_PROGRESS       shr 24)); // external handler only
  INSTALLLOGMODE_INITIALIZE     = (1 shl (INSTALLMESSAGE_INITIALIZE     shr 24)); // external handler only
  INSTALLLOGMODE_TERMINATE      = (1 shl (INSTALLMESSAGE_TERMINATE      shr 24)); // external handler only
  INSTALLLOGMODE_SHOWDIALOG     = (1 shl (INSTALLMESSAGE_SHOWDIALOG     shr 24)); // external handler only

type
  INSTALLLOGMODE = DWORD;
  TInstallLogMode = INSTALLLOGMODE;

const
  INSTALLLOGATTRIBUTES_APPEND            = (1 shl 0);
  INSTALLLOGATTRIBUTES_FLUSHEACHLINE     = (1 shl 1);

type
  INSTALLLOGATTRIBUTES = DWORD;
  TInstallLogAttributes = INSTALLLOGATTRIBUTES;

const
  INSTALLFEATUREATTRIBUTE_FAVORLOCAL             = 1 shl 0;
  INSTALLFEATUREATTRIBUTE_FAVORSOURCE            = 1 shl 1;
  INSTALLFEATUREATTRIBUTE_FOLLOWPARENT           = 1 shl 2;
  INSTALLFEATUREATTRIBUTE_FAVORADVERTISE         = 1 shl 3;
  INSTALLFEATUREATTRIBUTE_DISALLOWADVERTISE      = 1 shl 4;
  INSTALLFEATUREATTRIBUTE_NOUNSUPPORTEDADVERTISE = 1 shl 5;

type
  INSTALLFEATUREATTRIBUTE = DWORD;
  TInstallFeatureAttribute = INSTALLFEATUREATTRIBUTE;

const
  INSTALLMODE_NOSOURCERESOLUTION   = -3;  // skip source resolution
  INSTALLMODE_NODETECTION          = -2;  // skip detection
  INSTALLMODE_EXISTING             = -1;  // provide, if available
  INSTALLMODE_DEFAULT              =  0;  // install, if absent

type
  INSTALLMODE = DWORD;
  TInstallMode = INSTALLMODE;

const
  MAX_FEATURE_CHARS = 38;   // maximum chars in feature name (same as string GUID)

// Product info attributes: advertised information

  INSTALLPROPERTY_PACKAGENAME    = __TEXT('PackageName');
  INSTALLPROPERTY_TRANSFORMS     = __TEXT('Transforms');
  INSTALLPROPERTY_LANGUAGE       = __TEXT('Language');
  INSTALLPROPERTY_PRODUCTNAME    = __TEXT('ProductName');
  INSTALLPROPERTY_ASSIGNMENTTYPE = __TEXT('AssignmentType');
//#if (_WIN32_MSI >= 150)
  INSTALLPROPERTY_INSTANCETYPE   = __TEXT('InstanceType');
//#endif //(_WIN32_MSI >= 150)

  INSTALLPROPERTY_PACKAGECODE    = __TEXT('PackageCode');
  INSTALLPROPERTY_VERSION        = __TEXT('Version');
  INSTALLPROPERTY_PRODUCTICON    = __TEXT('ProductIcon');

// Product info attributes: installed information

  INSTALLPROPERTY_INSTALLEDPRODUCTNAME = __TEXT('InstalledProductName');
  INSTALLPROPERTY_VERSIONSTRING        = __TEXT('VersionString');
  INSTALLPROPERTY_HELPLINK             = __TEXT('HelpLink');
  INSTALLPROPERTY_HELPTELEPHONE        = __TEXT('HelpTelephone');
  INSTALLPROPERTY_INSTALLLOCATION      = __TEXT('InstallLocation');
  INSTALLPROPERTY_INSTALLSOURCE        = __TEXT('InstallSource');
  INSTALLPROPERTY_INSTALLDATE          = __TEXT('InstallDate');
  INSTALLPROPERTY_PUBLISHER            = __TEXT('Publisher');
  INSTALLPROPERTY_LOCALPACKAGE         = __TEXT('LocalPackage');
  INSTALLPROPERTY_URLINFOABOUT         = __TEXT('URLInfoAbout');
  INSTALLPROPERTY_URLUPDATEINFO        = __TEXT('URLUpdateInfo');
  INSTALLPROPERTY_VERSIONMINOR         = __TEXT('VersionMinor');
  INSTALLPROPERTY_VERSIONMAJOR         = __TEXT('VersionMajor');

const
  SCRIPTFLAGS_CACHEINFO                = $00000001;   // set if the icons need to be created/ removed
  SCRIPTFLAGS_SHORTCUTS                = $00000004;   // set if the shortcuts needs to be created/ deleted
  SCRIPTFLAGS_MACHINEASSIGN            = $00000008;   // set if product to be assigned to machine
  SCRIPTFLAGS_REGDATA_CNFGINFO         = $00000020;   // set if the product cnfg mgmt. registry data needs to be written/ removed
  SCRIPTFLAGS_VALIDATE_TRANSFORMS_LIST = $00000040;
{$IFDEF _WIN32_MSI_GREATER_EQUAL_110}
  SCRIPTFLAGS_REGDATA_CLASSINFO        = $00000080;   // set if COM classes related app info needs to be  created/ deleted
  SCRIPTFLAGS_REGDATA_EXTENSIONINFO    = $00000100;   // set if extension related app info needs to be  created/ deleted
  SCRIPTFLAGS_REGDATA_APPINFO          = SCRIPTFLAGS_REGDATA_CLASSINFO or SCRIPTFLAGS_REGDATA_EXTENSIONINFO; // for source level backward compatibility
{$ELSE} // _WIN32_MSI >= 110
  SCRIPTFLAGS_REGDATA_APPINFO          = $00000010;
{$ENDIF}
  SCRIPTFLAGS_REGDATA                  = SCRIPTFLAGS_REGDATA_APPINFO or SCRIPTFLAGS_REGDATA_CNFGINFO;// for source level backward compatibility

type
  SCRIPTFLAGS = Longint;
  TScriptFlags = SCRIPTFLAGS;

const
  ADVERTISEFLAGS_MACHINEASSIGN   = 0;   // set if the product is to be machine assigned
  ADVERTISEFLAGS_USERASSIGN      = 1;   // set if the product is to be user assigned

type
  ADVERTISEFLAGS = Longint;
  TAdvertiseFlags = ADVERTISEFLAGS;

const
  INSTALLTYPE_DEFAULT            = 0;   // set to indicate default behavior
  INSTALLTYPE_NETWORK_IMAGE      = 1;   // set to indicate network install
  INSTALLTYPE_SINGLE_INSTANCE    = 2;   // set to indicate a particular instance

type
  INSTALLTYPE = DWORD;
  TInstallType = INSTALLTYPE;

type
  _MSIFILEHASHINFO = record
    dwFileHashInfoSize: ULONG;
    dwData: array [0..3] of ULONG;
  end;
  MSIFILEHASHINFO = _MSIFILEHASHINFO;
  PMSIFILEHASHINFO = ^MSIFILEHASHINFO;
  TMsiFileHashInfo = MSIFILEHASHINFO;

const
  MSIARCHITECTUREFLAGS_X86   = $00000001; // set if creating the script for i386 platform
  MSIARCHITECTUREFLAGS_IA64  = $00000002; // set if creating the script for IA64 platform
  MSIARCHITECTUREFLAGS_AMD64 = $00000004; // set if creating the script for AMD64 platform

type
  MSIARCHITECTUREFLAGS = DWORD;
  TMsiArchitectureFlags = MSIARCHITECTUREFLAGS;

const
  MSIOPENPACKAGEFLAGS_IGNOREMACHINESTATE = $00000001; // ignore the machine state when creating the engine

type
  MSIOPENPACKAGEFLAGS = DWORD;
  TMsiOpenPackageFlags = MSIOPENPACKAGEFLAGS;

const
  MSIADVERTISEOPTIONFLAGS_INSTANCE = $00000001; // set if advertising a new instance

type
  tagMSIADVERTISEOPTIONFLAGS = DWORD;
  MSIADVERTISEOPTIONFLAGS = tagMSIADVERTISEOPTIONFLAGS;
  TMsiAdvertiseOptionFlags = MSIADVERTISEOPTIONFLAGS;

// --------------------------------------------------------------------------
// Functions to set the UI handling and logging. The UI will be used for error,
// progress, and log messages for all subsequent calls to Installer Service
// API functions that require UI.
// --------------------------------------------------------------------------

// Enable internal UI

function MsiSetInternalUI(dwUILevel: INSTALLUILEVEL; phWnd: PHWND): INSTALLUILEVEL; stdcall;

// Enable external UI handling, returns any previous handler or NULL if none.
// Messages are designated with a combination of bits from INSTALLLOGMODE enum.

function MsiSetExternalUIA(puiHandler: INSTALLUI_HANDLERA; dwMessageFilter: DWORD;
  pvContext: LPVOID): INSTALLUI_HANDLERA; stdcall;
function MsiSetExternalUIW(puiHandler: INSTALLUI_HANDLERW; dwMessageFilter: DWORD;
  pvContext: LPVOID): INSTALLUI_HANDLERW; stdcall;

{$IFDEF UNICODE}
function MsiSetExternalUI(puiHandler: INSTALLUI_HANDLERW; dwMessageFilter: DWORD;
  pvContext: LPVOID): INSTALLUI_HANDLERW; stdcall;
{$ELSE}
function MsiSetExternalUI(puiHandler: INSTALLUI_HANDLERA; dwMessageFilter: DWORD;
  pvContext: LPVOID): INSTALLUI_HANDLERA; stdcall;
{$ENDIF}

// Enable logging to a file for all install sessions for the client process,
// with control over which log messages are passed to the specified log file.
// Messages are designated with a combination of bits from INSTALLLOGMODE enum.

function MsiEnableLogA(dwLogMode: DWORD; szLogFile: LPCSTR; dwLogAttributes: DWORD): UINT; stdcall;
function MsiEnableLogW(dwLogMode: DWORD; szLogFile: LPCWSTR; dwLogAttributes: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnableLog(dwLogMode: DWORD; szLogFile: LPCWSTR; dwLogAttributes: DWORD): UINT; stdcall;
{$ELSE}
function MsiEnableLog(dwLogMode: DWORD; szLogFile: LPCSTR; dwLogAttributes: DWORD): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to query and configure a product as a whole.
// --------------------------------------------------------------------------

// Return the installed state for a product

function MsiQueryProductStateA(szProduct: LPCSTR): INSTALLSTATE; stdcall;
function MsiQueryProductStateW(szProduct: LPCWSTR): INSTALLSTATE; stdcall;

{$IFDEF UNICODE}
function MsiQueryProductState(szProduct: LPCWSTR): INSTALLSTATE; stdcall;
{$ELSE}
function MsiQueryProductState(szProduct: LPCSTR): INSTALLSTATE; stdcall;
{$ENDIF}

// Return product info

function MsiGetProductInfoA(szProduct: LPCSTR; szAttribute: LPCSTR;
  lpValueBuf: LPSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
function MsiGetProductInfoW(szProduct: LPCWSTR; szAttribute: LPCWSTR;
  lpValueBuf: LPWSTR; pcchValueBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetProductInfo(szProduct: LPCWSTR; szAttribute: LPCWSTR;
  lpValueBuf: LPWSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiGetProductInfo(szProduct: LPCSTR; szAttribute: LPCSTR;
  lpValueBuf: LPSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Install a new product.
// Either may be NULL, but the DATABASE property must be specfied

function MsiInstallProductA(szPackagePath: LPCSTR; szCommandLine: LPCSTR): UINT; stdcall;
function MsiInstallProductW(szPackagePath: LPCWSTR; szCommandLine: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiInstallProduct(szPackagePath: LPCWSTR; szCommandLine: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiInstallProduct(szPackagePath: LPCSTR; szCommandLine: LPCSTR): UINT; stdcall;
{$ENDIF}

// Install/uninstall an advertised or installed product
// No action if installed and INSTALLSTATE_DEFAULT specified

function MsiConfigureProductA(szProduct: LPCSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE): UINT; stdcall;
function MsiConfigureProductW(szProduct: LPCWSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE): UINT; stdcall;

{$IFDEF UNICODE}
function MsiConfigureProduct(szProduct: LPCWSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE): UINT; stdcall;
{$ELSE}
function MsiConfigureProduct(szProduct: LPCSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE): UINT; stdcall;
{$ENDIF}

// Install/uninstall an advertised or installed product
// No action if installed and INSTALLSTATE_DEFAULT specified

function MsiConfigureProductExA(szProduct: LPCSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE; szCommandLine: LPCSTR): UINT; stdcall;
function MsiConfigureProductExW(szProduct: LPCWSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE; szCommandLine: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiConfigureProductEx(szProduct: LPCWSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE; szCommandLine: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiConfigureProductEx(szProduct: LPCSTR; iInstallLevel: Integer;
  eInstallState: INSTALLSTATE; szCommandLine: LPCSTR): UINT; stdcall;
{$ENDIF}

// Reinstall product, used to validate or correct problems

function MsiReinstallProductA(szProduct: LPCSTR; szReinstallMode: DWORD): UINT; stdcall;
function MsiReinstallProductW(szProduct: LPCWSTR; szReinstallMode: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiReinstallProduct(szProduct: LPCWSTR; szReinstallMode: DWORD): UINT; stdcall;
{$ELSE}
function MsiReinstallProduct(szProduct: LPCSTR; szReinstallMode: DWORD): UINT; stdcall;
{$ENDIF}

// Output reg and shortcut info to script file for specified architecture for Assign or Publish
// If dwPlatform is 0, then the script is created based on the current platform (behavior of MsiAdvertiseProduct)
// If dwOptions includes MSIADVERTISEOPTIONFLAGS_INSTANCE, then a new instance is advertised. Use of
//    this option requires that szTransforms include the instance transform that changes the product code

function MsiAdvertiseProductExA(szPackagePath, szScriptfilePath, szTransforms: LPCSTR; lgidLanguage: LANGID;
  dwPlatform, dwOptions: DWORD): UINT; stdcall;
function MsiAdvertiseProductExW(szPackagePath, szScriptfilePath, szTransforms: LPCWSTR; lgidLanguage: LANGID;
  dwPlatform, dwOptions: DWORD): UINT; stdcall;
{$IFDEF UNICODE}
function MsiAdvertiseProductEx(szPackagePath, szScriptfilePath, szTransforms: LPCWSTR; lgidLanguage: LANGID;
  dwPlatform, dwOptions: DWORD): UINT; stdcall;
{$ELSE}
function MsiAdvertiseProductEx(szPackagePath, szScriptfilePath, szTransforms: LPCSTR; lgidLanguage: LANGID;
  dwPlatform, dwOptions: DWORD): UINT; stdcall;
{$ENDIF}

// Output reg and shortcut info to script file for Assign or Publish

function MsiAdvertiseProductA(szPackagePath, szScriptfilePath, szTransforms: LPCSTR; lgidLanguage: LANGID): UINT; stdcall;
function MsiAdvertiseProductW(szPackagePath, szScriptfilePath, szTransforms: LPCWSTR; lgidLanguage: LANGID): UINT; stdcall;
{$IFDEF UNICODE}
function MsiAdvertiseProduct(szPackagePath, szScriptfilePath, szTransforms: LPCWSTR; lgidLanguage: LANGID): UINT; stdcall;
{$ELSE}
function MsiAdvertiseProduct(szPackagePath, szScriptfilePath, szTransforms: LPCSTR; lgidLanguage: LANGID): UINT; stdcall;
{$ENDIF}

// Process advertise script file into supplied locations
// If an icon folder is specified, icon files will be placed there
// If an registry key is specified, registry data will be mapped under it
// If fShortcuts is TRUE, shortcuts will be created. If a special folder is
//    returned by SHGetSpecialFolderLocation(?), it will hold the shortcuts.
// if fRemoveItems is TRUE, items that are present will be removed

function MsiProcessAdvertiseScriptA(szScriptFile, szIconFolder: LPCSTR; hRegData: HKEY; fShortcuts, fRemoveItems: BOOL): UINT; stdcall;
function MsiProcessAdvertiseScriptW(szScriptFile, szIconFolder: LPCWSTR; hRegData: HKEY; fShortcuts, fRemoveItems: BOOL): UINT; stdcall;
{$IFDEF UNICODE}
function MsiProcessAdvertiseScript(szScriptFile, szIconFolder: LPCWSTR; hRegData: HKEY; fShortcuts, fRemoveItems: BOOL): UINT; stdcall;
{$ELSE}
function MsiProcessAdvertiseScript(szScriptFile, szIconFolder: LPCSTR; hRegData: HKEY; fShortcuts, fRemoveItems: BOOL): UINT; stdcall;
{$ENDIF}

// Process advertise script file using the supplied dwFlags control flags
// if fRemoveItems is TRUE, items that are present will be removed

function MsiAdvertiseScriptA(szScriptFile: LPCSTR; dwFlags: DWORD; phRegData: PHKEY; fRemoveItems: BOOL): UINT; stdcall;
function MsiAdvertiseScriptW(szScriptFile: LPCWSTR; dwFlags: DWORD; phRegData: PHKEY; fRemoveItems: BOOL): UINT; stdcall;
{$IFDEF UNICODE}
function MsiAdvertiseScript(szScriptFile: LPCWSTR; dwFlags: DWORD; phRegData: PHKEY; fRemoveItems: BOOL): UINT; stdcall;
{$ELSE}
function MsiAdvertiseScript(szScriptFile: LPCSTR; dwFlags: DWORD; phRegData: PHKEY; fRemoveItems: BOOL): UINT; stdcall;
{$ENDIF}

// Return product info from an installer script file:
//   product code, language, version, readable name, path to package
// Returns TRUE is success, FALSE if szScriptFile is not a valid script file

function MsiGetProductInfoFromScriptA(szScriptFile: LPCSTR; lpProductBuf39: LPSTR; plgidLanguage: PLANGID; pdwVersion: LPDWORD;
  lpNameBuf: LPSTR; pcchNameBuf: LPDWORD; lpPackageBuf: LPSTR; pcchPackageBuf: LPDWORD): UINT; stdcall;
function MsiGetProductInfoFromScriptW(szScriptFile: LPCWSTR; lpProductBuf39: LPWSTR; plgidLanguage: PLANGID; pdwVersion: LPDWORD;
  lpNameBuf: LPWSTR; pcchNameBuf: LPDWORD; lpPackageBuf: LPWSTR; pcchPackageBuf: LPDWORD): UINT; stdcall;
{$IFDEF UNICODE}
function MsiGetProductInfoFromScript(szScriptFile: LPCWSTR; lpProductBuf39: LPWSTR; plgidLanguage: PLANGID; pdwVersion: LPDWORD;
  lpNameBuf: LPWSTR; pcchNameBuf: LPDWORD; lpPackageBuf: LPWSTR; pcchPackageBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiGetProductInfoFromScript(szScriptFile: LPCSTR; lpProductBuf39: LPSTR; plgidLanguage: PLANGID; pdwVersion: LPDWORD;
  lpNameBuf: LPSTR; pcchNameBuf: LPDWORD; lpPackageBuf: LPSTR; pcchPackageBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Return the product code for a registered component, called once by apps

function MsiGetProductCodeA(szComponent: LPCSTR; lpBuf39: LPSTR): UINT; stdcall;
function MsiGetProductCodeW(szComponent: LPCWSTR; lpBuf39: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetProductCode(szComponent: LPCWSTR; lpBuf39: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiGetProductCode(szComponent: LPCSTR; lpBuf39: LPSTR): UINT; stdcall;
{$ENDIF}

// Return the registered user information for an installed product

function MsiGetUserInfoA(szProduct: LPCSTR; lpUserNameBuf: LPSTR;
  var pcchUserNameBuf: DWORD; lpOrgNameBuf: LPSTR; var pcchOrgNameBuf: DWORD;
  lpSerialBuf: LPSTR; var pcchSerialBuf: DWORD): USERINFOSTATE; stdcall;
function MsiGetUserInfoW(szProduct: LPCWSTR; lpUserNameBuf: LPWSTR;
  var pcchUserNameBuf: DWORD; lpOrgNameBuf: LPWSTR; var pcchOrgNameBuf: DWORD;
  lpSerialBuf: LPWSTR; var pcchSerialBuf: DWORD): USERINFOSTATE; stdcall;

{$IFDEF UNICODE}
function MsiGetUserInfo(szProduct: LPCWSTR; lpUserNameBuf: LPWSTR;
  var pcchUserNameBuf: DWORD; lpOrgNameBuf: LPWSTR; var pcchOrgNameBuf: DWORD;
  lpSerialBuf: LPWSTR; var pcchSerialBuf: DWORD): USERINFOSTATE; stdcall;
{$ELSE}
function MsiGetUserInfo(szProduct: LPCSTR; lpUserNameBuf: LPSTR;
  var pcchUserNameBuf: DWORD; lpOrgNameBuf: LPSTR; var pcchOrgNameBuf: DWORD;
  lpSerialBuf: LPSTR; var pcchSerialBuf: DWORD): USERINFOSTATE; stdcall;
{$ENDIF}

// Obtain and store user info and PID from installation wizard (first run)

function MsiCollectUserInfoA(szProduct: LPCSTR): UINT; stdcall;
function MsiCollectUserInfoW(szProduct: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiCollectUserInfo(szProduct: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiCollectUserInfo(szProduct: LPCSTR): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to patch existing products
// --------------------------------------------------------------------------

// Patch all possible installed products.

function MsiApplyPatchA(szPatchPackage: LPCSTR; szInstallPackage: LPCSTR;
  eInstallType: INSTALLTYPE; szCommandLine: LPCSTR): UINT; stdcall;
function MsiApplyPatchW(szPatchPackage: LPCWSTR; szInstallPackage: LPCWSTR;
  eInstallType: INSTALLTYPE; szCommandLine: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiApplyPatch(szPatchPackage: LPCWSTR; szInstallPackage: LPCWSTR;
  eInstallType: INSTALLTYPE; szCommandLine: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiApplyPatch(szPatchPackage: LPCSTR; szInstallPackage: LPCSTR;
  eInstallType: INSTALLTYPE; szCommandLine: LPCSTR): UINT; stdcall;
{$ENDIF}

// Return patch info

function MsiGetPatchInfoA(szPatch: LPCSTR; szAttribute: LPCSTR;
  lpValueBuf: LPSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
function MsiGetPatchInfoW(szPatch: LPCWSTR; szAttribute: LPCWSTR;
  lpValueBuf: LPWSTR; pcchValueBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetPatchInfo(szPatch: LPCWSTR; szAttribute: LPCWSTR;
  lpValueBuf: LPWSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiGetPatchInfo(szPatch: LPCSTR; szAttribute: LPCSTR;
  lpValueBuf: LPSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Enumerate all patches for a product

function MsiEnumPatchesA(szProduct: LPCSTR; iPatchIndex: DWORD; lpPatchBuf: LPSTR;
  lpTransformsBuf: LPSTR; var pcchTransformsBuf: DWORD): UINT; stdcall;
function MsiEnumPatchesW(szProduct: LPCWSTR; iPatchIndex: DWORD; lpPatchBuf: LPWSTR;
  lpTransformsBuf: LPWSTR; var pcchTransformsBuf: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumPatches(szProduct: LPCWSTR; iPatchIndex: DWORD; lpPatchBuf: LPWSTR;
  lpTransformsBuf: LPWSTR; var pcchTransformsBuf: DWORD): UINT; stdcall;
{$ELSE}
function MsiEnumPatches(szProduct: LPCSTR; iPatchIndex: DWORD; lpPatchBuf: LPSTR;
  lpTransformsBuf: LPSTR; var pcchTransformsBuf: DWORD): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to query and configure a feature within a product.
// --------------------------------------------------------------------------

// Return the installed state for a product feature

function MsiQueryFeatureStateA(szProduct: LPCSTR; szFeature: LPCSTR): INSTALLSTATE; stdcall;
function MsiQueryFeatureStateW(szProduct: LPCWSTR; szFeature: LPCWSTR): INSTALLSTATE; stdcall;

{$IFDEF UNICODE}
function MsiQueryFeatureState(szProduct: LPCWSTR; szFeature: LPCWSTR): INSTALLSTATE; stdcall;
{$ELSE}
function MsiQueryFeatureState(szProduct: LPCSTR; szFeature: LPCSTR): INSTALLSTATE; stdcall;
{$ENDIF}

// Indicate intent to use a product feature, increments usage count
// Prompts for CD if not loaded, does not install feature

function MsiUseFeatureA(szProduct: LPCSTR; szFeature: LPCSTR): INSTALLSTATE; stdcall;
function MsiUseFeatureW(szProduct: LPCWSTR; szFeature: LPCWSTR): INSTALLSTATE; stdcall;

{$IFDEF UNICODE}
function MsiUseFeature(szProduct: LPCWSTR; szFeature: LPCWSTR): INSTALLSTATE; stdcall;
{$ELSE}
function MsiUseFeature(szProduct: LPCSTR; szFeature: LPCSTR): INSTALLSTATE; stdcall;
{$ENDIF}

// Indicate intent to use a product feature, increments usage count
// Prompts for CD if not loaded, does not install feature
// Allows for bypassing component detection where performance is critical

function MsiUseFeatureExA(szProduct: LPCSTR; szFeature: LPCSTR;
  dwInstallMode: DWORD; dwReserved: DWORD): INSTALLSTATE; stdcall;
function MsiUseFeatureExW(szProduct: LPCWSTR; szFeature: LPCWSTR; dwInstallMode: DWORD;
  dwReserved: DWORD): INSTALLSTATE; stdcall;

{$IFDEF UNICODE}
function MsiUseFeatureEx(szProduct: LPCWSTR; szFeature: LPCWSTR;
  dwInstallMode: DWORD; dwReserved: DWORD): INSTALLSTATE; stdcall;
{$ELSE}
function MsiUseFeatureEx(szProduct: LPCSTR; szFeature: LPCSTR;
  dwInstallMode: DWORD; dwReserved: DWORD): INSTALLSTATE; stdcall;
{$ENDIF}

// Return the usage metrics for a product feature

function MsiGetFeatureUsageA(szProduct: LPCSTR; szFeature: LPCSTR;
  var pdwUseCount, pwDateUsed: WORD): UINT; stdcall;
function MsiGetFeatureUsageW(szProduct: LPCWSTR; szFeature: LPCWSTR;
  var pdwUseCount, pwDateUsed: WORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetFeatureUsage(szProduct: LPCWSTR; szFeature: LPCWSTR;
  var pdwUseCount, pwDateUsed: WORD): UINT; stdcall;
{$ELSE}
function MsiGetFeatureUsage(szProduct: LPCSTR; szFeature: LPCSTR;
  var pdwUseCount, pwDateUsed: WORD): UINT; stdcall;
{$ENDIF}

// Force the installed state for a product feature

function MsiConfigureFeatureA(szProduct, szFeature: LPCSTR; eInstallState: INSTALLSTATE): UINT; stdcall;
function MsiConfigureFeatureW(szProduct, szFeature: LPCWSTR; eInstallState: INSTALLSTATE): UINT; stdcall;

{$IFDEF UNICODE}
function MsiConfigureFeature(szProduct, szFeature: LPCWSTR; eInstallState: INSTALLSTATE): UINT; stdcall;
{$ELSE}
function MsiConfigureFeature(szProduct, szFeature: LPCSTR; eInstallState: INSTALLSTATE): UINT; stdcall;
{$ENDIF}

// Reinstall feature, used to validate or correct problems

function MsiReinstallFeatureA(szProduct, szFeature: LPCSTR; dwReinstallMode: DWORD): UINT; stdcall;
function MsiReinstallFeatureW(szProduct, szFeature: LPCWSTR; dwReinstallMode: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiReinstallFeature(szProduct, szFeature: LPCWSTR; dwReinstallMode: DWORD): UINT; stdcall;
{$ELSE}
function MsiReinstallFeature(szProduct, szFeature: LPCSTR; dwReinstallMode: DWORD): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to return a path to a particular component.
// The state of the feature being used should have been checked previously.
// --------------------------------------------------------------------------

// Return full component path, performing any necessary installation
// calls MsiQueryFeatureState to detect that all components are installed
// then calls MsiConfigureFeature if any of its components are uninstalled
// then calls MsiLocateComponent to obtain the path the its key file

function MsiProvideComponentA(szProduct: LPCSTR; szFeature: LPCSTR; szComponent: LPCSTR;
  dwInstallMode: DWORD; lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
function MsiProvideComponentW(szProduct: LPCWSTR; szFeature: LPCWSTR; szComponent: LPCWSTR;
  dwInstallMode: DWORD; lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiProvideComponent(szProduct: LPCWSTR; szFeature: LPCWSTR; szComponent: LPCWSTR;
  dwInstallMode: DWORD; lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiProvideComponent(szProduct: LPCSTR; szFeature: LPCSTR; szComponent: LPCSTR;
  dwInstallMode: DWORD; lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Return full component path for a qualified component, performing any necessary installation.
// Prompts for source if necessary and increments the usage count for the feature.

function MsiProvideQualifiedComponentA(szCategory: LPCSTR; szQualifier: LPCSTR;
  dwInstallMode: DWORD; lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
function MsiProvideQualifiedComponentW(szCategory: LPCWSTR; szQualifier: LPCWSTR;
  dwInstallMode: DWORD; lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiProvideQualifiedComponent(szCategory: LPCWSTR; szQualifier: LPCWSTR;
  dwInstallMode: DWORD; lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiProvideQualifiedComponent(szCategory: LPCSTR; szQualifier: LPCSTR;
  dwInstallMode: DWORD; lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Return full component path for a qualified component, performing any necessary installation.
// Prompts for source if necessary and increments the usage count for the feature.
// The szProduct parameter specifies the product to match that has published the qualified
// component. If null, this API works the same as MsiProvideQualifiedComponent.

function MsiProvideQualifiedComponentExA(szCategory: LPCSTR; szQualifier: LPCSTR;
  dwInstallMode: DWORD; szProduct: LPCSTR; dwUnused1: DWORD; dwUnused2: DWORD;
  lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
function MsiProvideQualifiedComponentExW(szCategory: LPCWSTR; szQualifier: LPCWSTR;
  dwInstallMode: DWORD; szProduct: LPCWSTR; dwUnused1: DWORD; dwUnused2: DWORD;
  lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiProvideQualifiedComponentEx(szCategory: LPCWSTR; szQualifier: LPCWSTR;
  dwInstallMode: DWORD; szProduct: LPCWSTR; dwUnused1: DWORD; dwUnused2: DWORD;
  lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiProvideQualifiedComponentEx(szCategory: LPCSTR; szQualifier: LPCSTR;
  dwInstallMode: DWORD; szProduct: LPCSTR; dwUnused1: DWORD; dwUnused2: DWORD;
  lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Return full path to an installed component

function MsiGetComponentPathA(szProduct: LPCSTR; szComponent: LPCSTR;
  lpPathBuf: LPSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;
function MsiGetComponentPathW(szProduct: LPCWSTR; szComponent: LPCWSTR;
  lpPathBuf: LPWSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;

{$IFDEF UNICODE}
function MsiGetComponentPath(szProduct: LPCWSTR; szComponent: LPCWSTR;
  lpPathBuf: LPWSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;
{$ELSE}
function MsiGetComponentPath(szProduct: LPCSTR; szComponent: LPCSTR;
  lpPathBuf: LPSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;
{$ENDIF}

const
  MSIASSEMBLYINFO_NETASSEMBLY   = 0; // Net assemblies
  MSIASSEMBLYINFO_WIN32ASSEMBLY = 1; // Win32 assemblies

// Return full component path for an assembly installed via the WI, performing any necessary installation.
// Prompts for source if necessary and increments the usage count for the feature.
// The szAssemblyName parameter specifies the stringized assembly name.
// The szAppContext is the full path to the .cfg file or the app exe to which the assembly being requested
// has been privatised to, which is null for global assemblies

function MsiProvideAssemblyA(szAssemblyName, szAppContext: LPCSTR; dwInstallMode, dwAssemblyInfo: DWORD;
  lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
function MsiProvideAssemblyW(szAssemblyName, szAppContext: LPCWSTR; dwInstallMode, dwAssemblyInfo: DWORD;
  lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$IFDEF UNICODE}
function MsiProvideAssembly(szAssemblyName, szAppContext: LPCWSTR; dwInstallMode, dwAssemblyInfo: DWORD;
  lpPathBuf: LPWSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiProvideAssembly(szAssemblyName, szAppContext: LPCSTR; dwInstallMode, dwAssemblyInfo: DWORD;
  lpPathBuf: LPSTR; pcchPathBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to iterate registered products, features, and components.
// As with reg keys, they accept a 0-based index into the enumeration.
// --------------------------------------------------------------------------

// Enumerate the registered products, either installed or advertised

function MsiEnumProductsA(iProductIndex: DWORD; lpProductBuf: LPSTR): UINT; stdcall;
function MsiEnumProductsW(iProductIndex: DWORD; lpProductBuf: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumProducts(iProductIndex: DWORD; lpProductBuf: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiEnumProducts(iProductIndex: DWORD; lpProductBuf: LPSTR): UINT; stdcall;
{$ENDIF}

{$IFDEF WIN32_MSI_110}

// Enumerate products with given upgrade code

function MsiEnumRelatedProductsA(lpUpgradeCode: LPCSTR; dwReserved: DWORD;
  iProductIndex: DWORD; lpProductBuf: LPSTR): UINT; stdcall;
function MsiEnumRelatedProductsW(lpUpgradeCode: LPCWSTR; dwReserved: DWORD;
  iProductIndex: DWORD; lpProductBuf: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumRelatedProducts(lpUpgradeCode: LPCWSTR; dwReserved: DWORD;
  iProductIndex: DWORD; lpProductBuf: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiEnumRelatedProducts(lpUpgradeCode: LPCSTR; dwReserved: DWORD;
  iProductIndex: DWORD; lpProductBuf: LPSTR): UINT; stdcall;
{$ENDIF}

{$ENDIF WIN32_MSI_110}

// Enumerate the advertised features for a given product.
// If parent is not required, supplying NULL will improve performance.

function MsiEnumFeaturesA(szProduct: LPCSTR; iFeatureIndex: DWORD;
  lpFeatureBuf: LPSTR; lpParentBuf: LPSTR): UINT; stdcall;
function MsiEnumFeaturesW(szProduct: LPCWSTR; iFeatureIndex: DWORD;
  lpFeatureBuf: LPWSTR; lpParentBuf: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumFeatures(szProduct: LPCWSTR; iFeatureIndex: DWORD;
  lpFeatureBuf: LPWSTR; lpParentBuf: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiEnumFeatures(szProduct: LPCSTR; iFeatureIndex: DWORD;
  lpFeatureBuf: LPSTR; lpParentBuf: LPSTR): UINT; stdcall;
{$ENDIF}

// Enumerate the installed components for all products

function MsiEnumComponentsA(iComponentIndex: DWORD; lpComponentBuf: LPSTR): UINT; stdcall;
function MsiEnumComponentsW(iComponentIndex: DWORD; lpComponentBuf: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumComponents(iComponentIndex: DWORD; lpComponentBuf: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiEnumComponents(iComponentIndex: DWORD; lpComponentBuf: LPSTR): UINT; stdcall;
{$ENDIF}

// Enumerate the client products for a component

function MsiEnumClientsA(szComponent: LPCSTR; iProductIndex: DWORD; lpProductBuf: LPSTR): UINT; stdcall;
function MsiEnumClientsW(szComponent: LPCWSTR; iProductIndex: DWORD; lpProductBuf: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumClients(szComponent: LPCWSTR; iProductIndex: DWORD; lpProductBuf: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiEnumClients(szComponent: LPCSTR; iProductIndex: DWORD; lpProductBuf: LPSTR): UINT; stdcall;
{$ENDIF}

// Enumerate the qualifiers for an advertised component.

function MsiEnumComponentQualifiersA(szComponent: LPCSTR; iIndex: DWORD;
  lpQualifierBuf: LPSTR; var pcchQualifierBuf: DWORD; lpApplicationDataBuf: LPSTR;
  pcchApplicationDataBuf: LPDWORD): UINT; stdcall;
function MsiEnumComponentQualifiersW(szComponent: LPCWSTR; iIndex: DWORD;
  lpQualifierBuf: LPWSTR; var pcchQualifierBuf: DWORD; lpApplicationDataBuf: LPWSTR;
  pcchApplicationDataBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiEnumComponentQualifiers(szComponent: LPCWSTR; iIndex: DWORD;
  lpQualifierBuf: LPWSTR; var pcchQualifierBuf: DWORD; lpApplicationDataBuf: LPWSTR;
  pcchApplicationDataBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiEnumComponentQualifiers(szComponent: LPCSTR; iIndex: DWORD;
  lpQualifierBuf: LPSTR; var pcchQualifierBuf: DWORD; lpApplicationDataBuf: LPSTR;
  pcchApplicationDataBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to obtain product or package information.
// --------------------------------------------------------------------------

// Open the installation for a product to obtain detailed information

function MsiOpenProductA(szProduct: LPCSTR; var hProduct: MSIHANDLE): UINT; stdcall;
function MsiOpenProductW(szProduct: LPCWSTR; var hProduct: MSIHANDLE): UINT; stdcall;

{$IFDEF UNICODE}
function MsiOpenProduct(szProduct: LPCWSTR; var hProduct: MSIHANDLE): UINT; stdcall;
{$ELSE}
function MsiOpenProduct(szProduct: LPCSTR; var hProduct: MSIHANDLE): UINT; stdcall;
{$ENDIF}

// Open a product package in order to access product properties

function MsiOpenPackageA(szPackagePath: LPCSTR; var hProduct: MSIHANDLE): UINT; stdcall;
function MsiOpenPackageW(szPackagePath: LPCWSTR; var hProduct: MSIHANDLE): UINT; stdcall;

{$IFDEF UNICODE}
function MsiOpenPackage(szPackagePath: LPCWSTR; var hProduct: MSIHANDLE): UINT; stdcall;
{$ELSE}
function MsiOpenPackage(szPackagePath: LPCSTR; var hProduct: MSIHANDLE): UINT; stdcall;
{$ENDIF}

// Open a product package in order to access product properties
// Option to create a "safe" engine that does not look at machine state
//  and does not allow for modification of machine state

function MsiOpenPackageExA(szPackagePath: LPCSTR; dwOptions: DWORD; var hProduct: MSIHANDLE): UINT; stdcall;
function MsiOpenPackageExW(szPackagePath: LPCWSTR; dwOptions: DWORD; var hProduct: MSIHANDLE): UINT; stdcall;
{$IFDEF UNICODE}
function MsiOpenPackageEx(szPackagePath: LPCWSTR; dwOptions: DWORD; var hProduct: MSIHANDLE): UINT; stdcall;
{$ELSE}
function MsiOpenPackageEx(szPackagePath: LPCSTR; dwOptions: DWORD; var hProduct: MSIHANDLE): UINT; stdcall;
{$ENDIF}


// Provide the value for an installation property.

function MsiGetProductPropertyA(hProduct: MSIHANDLE; szProperty: LPCSTR;
  lpValueBuf: LPSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
function MsiGetProductPropertyW(hProduct: MSIHANDLE; szProperty: LPCWSTR;
  lpValueBuf: LPWSTR; pcchValueBuf: LPDWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetProductProperty(hProduct: MSIHANDLE; szProperty: LPCWSTR;
  lpValueBuf: LPWSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
{$ELSE}
function MsiGetProductProperty(hProduct: MSIHANDLE; szProperty: LPCSTR;
  lpValueBuf: LPSTR; pcchValueBuf: LPDWORD): UINT; stdcall;
{$ENDIF}

// Determine whether a file is a package
// Returns ERROR_SUCCESS if file is a package.

function MsiVerifyPackageA(szPackagePath: LPCSTR): UINT; stdcall;
function MsiVerifyPackageW(szPackagePath: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiVerifyPackage(szPackagePath: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiVerifyPackage(szPackagePath: LPCSTR): UINT; stdcall;
{$ENDIF}

// Provide descriptive information for product feature: title and description.
// Returns the install level for the feature, or -1 if feature is unknown.
//   0 = feature is not available on this machine
//   1 = highest priority, feature installed if parent is installed
//  >1 = decreasing priority, feature installation based on InstallLevel property

function MsiGetFeatureInfoA(hProduct: MSIHANDLE; szFeature: LPCSTR; var lpAttributes: DWORD;
  lpTitleBuf: LPSTR; var pcchTitleBuf: DWORD; lpHelpBuf: LPSTR; var pcchHelpBuf: DWORD): UINT; stdcall;
function MsiGetFeatureInfoW(hProduct: MSIHANDLE; szFeature: LPCWSTR; var lpAttributes: DWORD;
  lpTitleBuf: LPWSTR; var pcchTitleBuf: DWORD; lpHelpBuf: LPWSTR; var pcchHelpBuf: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetFeatureInfo(hProduct: MSIHANDLE; szFeature: LPCWSTR; var lpAttributes: DWORD;
  lpTitleBuf: LPWSTR; var pcchTitleBuf: DWORD; lpHelpBuf: LPWSTR; var pcchHelpBuf: DWORD): UINT; stdcall;
{$ELSE}
function MsiGetFeatureInfo(hProduct: MSIHANDLE; szFeature: LPCSTR; var lpAttributes: DWORD;
  lpTitleBuf: LPSTR; var pcchTitleBuf: DWORD; lpHelpBuf: LPSTR; var pcchHelpBuf: DWORD): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Functions to access or install missing components and files.
// These should be used as a last resort.
// --------------------------------------------------------------------------

// Install a component unexpectedly missing, provided only for error recovery
// This would typically occur due to failue to establish feature availability
// The product feature having the smallest incremental cost is installed

function MsiInstallMissingComponentA(szProduct: LPCSTR; szComponent: LPCSTR;
  eInstallState: INSTALLSTATE): UINT; stdcall;
function MsiInstallMissingComponentW(szProduct: LPCWSTR; szComponent: LPCWSTR;
  eInstallState: INSTALLSTATE): UINT; stdcall;

{$IFDEF UNICODE}
function MsiInstallMissingComponent(szProduct: LPCWSTR; szComponent: LPCWSTR;
  eInstallState: INSTALLSTATE): UINT; stdcall;
{$ELSE}
function MsiInstallMissingComponent(szProduct: LPCSTR; szComponent: LPCSTR;
  eInstallState: INSTALLSTATE): UINT; stdcall;
{$ENDIF}

// Install a file unexpectedly missing, provided only for error recovery
// This would typically occur due to failue to establish feature availability
// The missing component is determined from the product's File table, then
// the product feature having the smallest incremental cost is installed

function MsiInstallMissingFileA(szProduct: LPCSTR; szFile: LPCSTR): UINT; stdcall;
function MsiInstallMissingFileW(szProduct: LPCWSTR; szFile: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiInstallMissingFile(szProduct: LPCWSTR; szFile: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiInstallMissingFile(szProduct: LPCSTR; szFile: LPCSTR): UINT; stdcall;
{$ENDIF}

// Return full path to an installed component without a product code
// This function attempts to determine the product using MsiGetProductCode
// but is not guaranteed to find the correct product for the caller.
// MsiGetComponentPath should always be called when possible.

function MsiLocateComponentA(szComponent: LPCSTR; lpPathBuf: LPSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;
function MsiLocateComponentW(szComponent: LPCWSTR; lpPathBuf: LPWSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;

{$IFDEF UNICODE}
function MsiLocateComponent(szComponent: LPCWSTR; lpPathBuf: LPWSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;
{$ELSE}
function MsiLocateComponent(szComponent: LPCSTR; lpPathBuf: LPSTR; pcchBuf: LPDWORD): INSTALLSTATE; stdcall;
{$ENDIF}

{$IFDEF WIN32_MSI_110}

// --------------------------------------------------------------------------
// Functions used to manage the list of valid sources.
// --------------------------------------------------------------------------

// Opens the list of sources for the specified user's install of the product
// and removes all network sources from the list. A NULL or empty value for
// the user name indicates the per-machine install.

function MsiSourceListClearAllA(szProduct: LPCSTR; szUserName: LPCSTR; dwReserved: DWORD): UINT; stdcall;
function MsiSourceListClearAllW(szProduct: LPCWSTR; szUserName: LPCWSTR; dwReserved: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiSourceListClearAll(szProduct: LPCWSTR; szUserName: LPCWSTR; dwReserved: DWORD): UINT; stdcall;
{$ELSE}
function MsiSourceListClearAll(szProduct: LPCSTR; szUserName: LPCSTR; dwReserved: DWORD): UINT; stdcall;
{$ENDIF}

// Opens the list of sources for the specified user's install of the product
// and adds the provided source as a new network source. A NULL or empty
// value for the user name indicates the per-machine install.

function MsiSourceListAddSourceA(szProduct: LPCSTR; szUserName: LPCSTR;
  dwReserved: DWORD; szSource: LPCSTR): UINT; stdcall;
function MsiSourceListAddSourceW(szProduct: LPCWSTR; szUserName: LPCWSTR;
  dwReserved: DWORD; szSource: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiSourceListAddSource(szProduct: LPCWSTR; szUserName: LPCWSTR;
  dwReserved: DWORD; szSource: LPCWSTR): UINT; stdcall;
{$ELSE}
function MsiSourceListAddSource(szProduct: LPCSTR; szUserName: LPCSTR;
  dwReserved: DWORD; szSource: LPCSTR): UINT; stdcall;
{$ENDIF}

// Forces the installer to reevaluate the list of sources the next time that
// the specified product needs a source.

function MsiSourceListForceResolutionA(szProduct, szUserName: LPCSTR; dwReserved: DWORD): UINT; stdcall;
function MsiSourceListForceResolutionW(szProduct, szUserName: LPCWSTR; dwReserved: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiSourceListForceResolution(szProduct, szUserName: LPCWSTR; dwReserved: DWORD): UINT; stdcall;
{$ELSE}
function MsiSourceListForceResolution(szProduct, szUserName: LPCSTR; dwReserved: DWORD): UINT; stdcall;
{$ENDIF}

{$ENDIF WIN32_MSI_110}

// --------------------------------------------------------------------------
// Utility functions
// --------------------------------------------------------------------------

// Give the version string and language for a specified file

function MsiGetFileVersionA(szFilePath: LPCSTR; lpVersionBuf: LPSTR;
  var pcchVersionBuf: DWORD; lpLangBuf: LPSTR; var pcchLangBuf: DWORD): UINT; stdcall;
function MsiGetFileVersionW(szFilePath: LPCWSTR; lpVersionBuf: LPWSTR;
  var pcchVersionBuf: DWORD; lpLangBuf: LPWSTR; var pcchLangBuf: DWORD): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetFileVersion(szFilePath: LPCWSTR; lpVersionBuf: LPWSTR;
  var pcchVersionBuf: DWORD; lpLangBuf: LPWSTR; var pcchLangBuf: DWORD): UINT; stdcall;
{$ELSE}
function MsiGetFileVersion(szFilePath: LPCSTR; lpVersionBuf: LPSTR;
  var pcchVersionBuf: DWORD; lpLangBuf: LPSTR; var pcchLangBuf: DWORD): UINT; stdcall;
{$ENDIF}

function MsiGetFileHashA(szFilePath: LPCSTR; dwOptions: DWORD; pHash: PMSIFILEHASHINFO): UINT; stdcall;
function MsiGetFileHashW(szFilePath: LPCWSTR; dwOptions: DWORD; pHash: PMSIFILEHASHINFO): UINT; stdcall;
{$IFDEF UNICODE}
function MsiGetFileHash(szFilePath: LPCWSTR; dwOptions: DWORD; pHash: PMSIFILEHASHINFO): UINT; stdcall;
{$ELSE}
function MsiGetFileHash(szFilePath: LPCSTR; dwOptions: DWORD; pHash: PMSIFILEHASHINFO): UINT; stdcall;
{$ENDIF}

function MsiGetFileSignatureInformationA(szSignedObjectPath: LPCSTR; dwFlags: DWORD; var ppcCertContext: PCCERT_CONTEXT;
  pbHashData: LPBYTE; pcbHashData: LPDWORD): HRESULT; stdcall;
function MsiGetFileSignatureInformationW(szSignedObjectPath: LPCWSTR; dwFlags: DWORD; var ppcCertContext: PCCERT_CONTEXT;
  pbHashData: LPBYTE; pcbHashData: LPDWORD): HRESULT; stdcall;
{$IFDEF UNICODE}
function MsiGetFileSignatureInformation(szSignedObjectPath: LPCWSTR; dwFlags: DWORD; var ppcCertContext: PCCERT_CONTEXT;
  pbHashData: LPBYTE; pcbHashData: LPDWORD): HRESULT; stdcall;
{$ELSE}
function MsiGetFileSignatureInformation(szSignedObjectPath: LPCSTR; dwFlags: DWORD; var ppcCertContext: PCCERT_CONTEXT;
  pbHashData: LPBYTE; pcbHashData: LPDWORD): HRESULT; stdcall;
{$ENDIF}

// By default, when only requesting the certificate context, an invalid hash
// in the digital signature is not a fatal error.  Set this flag in the dwFlags
// parameter to make the TRUST_E_BAD_DIGEST error fatal.

const
  MSI_INVALID_HASH_IS_FATAL = $1;

{$IFDEF WIN32_MSI_110}

// examine a shortcut, and retrieve its descriptor information
// if available.

function MsiGetShortcutTargetA(szShortcutPath: LPCSTR; szProductCode: LPSTR;
  szFeatureId: LPSTR; szComponentCode: LPSTR): UINT; stdcall;
function MsiGetShortcutTargetW(szShortcutPath: LPCWSTR; szProductCode: LPWSTR;
  szFeatureId: LPWSTR; szComponentCode: LPWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function MsiGetShortcutTarget(szShortcutPath: LPCWSTR; szProductCode: LPWSTR;
  szFeatureId: LPWSTR; szComponentCode: LPWSTR): UINT; stdcall;
{$ELSE}
function MsiGetShortcutTarget(szShortcutPath: LPCSTR; szProductCode: LPSTR;
  szFeatureId: LPSTR; szComponentCode: LPSTR): UINT; stdcall;
{$ENDIF}

{$ENDIF WIN32_MSI_110}

// checks to see if a product is managed
// checks per-machine if called from system context, per-user if from
// user context

function MsiIsProductElevatedA(szProduct: LPCSTR; var pfElevated: BOOL): UINT; stdcall;
function MsiIsProductElevatedW(szProduct: LPCWSTR; var pfElevated: BOOL): UINT; stdcall;

{$IFDEF UNICODE}
function MsiIsProductElevated(szProduct: LPCWSTR; var pfElevated: BOOL): UINT; stdcall;
{$ELSE}
function MsiIsProductElevated(szProduct: LPCSTR; var pfElevated: BOOL): UINT; stdcall;
{$ENDIF}

// --------------------------------------------------------------------------
// Error codes for installer access functions - until merged to winerr.h
// --------------------------------------------------------------------------

const
  ERROR_INSTALL_USEREXIT      = 1602; // User cancel installation.
  ERROR_INSTALL_FAILURE       = 1603; // Fatal error during installation.
  ERROR_INSTALL_SUSPEND       = 1604; // Installation suspended, incomplete.
  ERROR_UNKNOWN_PRODUCT       = 1605; // This action is only valid for products that are currently installed.
  ERROR_UNKNOWN_FEATURE       = 1606; // Feature ID not registered.
  ERROR_UNKNOWN_COMPONENT     = 1607; // Component ID not registered.
  ERROR_UNKNOWN_PROPERTY      = 1608; // Unknown property.
  ERROR_INVALID_HANDLE_STATE  = 1609; // Handle is in an invalid state.
  ERROR_BAD_CONFIGURATION     = 1610; // The configuration data for this product is corrupt.  Contact your support personnel.
  ERROR_INDEX_ABSENT          = 1611; // Component qualifier not present.
  ERROR_INSTALL_SOURCE_ABSENT = 1612; // The installation source for this product is not available.  Verify that the source exists and that you can access it.
  ERROR_PRODUCT_UNINSTALLED   = 1614; // Product is uninstalled.
  ERROR_BAD_QUERY_SYNTAX      = 1615; // SQL query syntax invalid or unsupported.
  ERROR_INVALID_FIELD         = 1616; // Record field does not exist.

  ERROR_INSTALL_SERVICE_FAILURE      = 1601; // The Windows Installer service could not be accessed.  Contact your support personnel to verify that the Windows Installer service is properly registered.
  ERROR_INSTALL_PACKAGE_VERSION      = 1613; // This installation package cannot be installed by the Windows Installer service.  You must install a Windows service pack that contains a newer version of the Windows Installer service.
  ERROR_INSTALL_ALREADY_RUNNING      = 1618; // Another installation is already in progress.  Complete that installation before proceeding with this install.
  ERROR_INSTALL_PACKAGE_OPEN_FAILED  = 1619; // This installation package could not be opened.  Verify that the package exists and that you can access it, or contact the application vendor to verify that this is a valid Windows Installer package.
  ERROR_INSTALL_PACKAGE_INVALID      = 1620; // This installation package could not be opened.  Contact the application vendor to verify that this is a valid Windows Installer package.
  ERROR_INSTALL_UI_FAILURE           = 1621; // There was an error starting the Windows Installer service user interface.  Contact your support personnel.
  ERROR_INSTALL_LOG_FAILURE          = 1622; // Error opening installation log file.  Verify that the specified log file location exists and is writable.
  ERROR_INSTALL_LANGUAGE_UNSUPPORTED = 1623; // This language of this installation package is not supported by your system.
  ERROR_INSTALL_PACKAGE_REJECTED     = 1625; // The system administrator has set policies to prevent this installation.

  ERROR_FUNCTION_NOT_CALLED = 1626; // Function could not be executed.
  ERROR_FUNCTION_FAILED     = 1627; // Function failed during execution.
  ERROR_INVALID_TABLE       = 1628; // Invalid or unknown table specified.
  ERROR_DATATYPE_MISMATCH   = 1629; // Data supplied is of wrong type.
  ERROR_UNSUPPORTED_TYPE    = 1630; // Data of this type is not supported.
  ERROR_CREATE_FAILED       = 1631; // The Windows Installer service failed to start.  Contact your support personnel.

  ERROR_INSTALL_TEMP_UNWRITABLE = 1632; // The Temp folder is on a drive that is full or is inaccessible. Free up space on the drive or verify that you have write permission on the Temp folder.

  ERROR_INSTALL_PLATFORM_UNSUPPORTED = 1633; // This installation package is not supported by this processor type. Contact your product vendor.

  ERROR_INSTALL_NOTUSED = 1634; // Component not used on this machine

  ERROR_INSTALL_TRANSFORM_FAILURE = 1624; // Error applying transforms.  Verify that the specified transform paths are valid.

  ERROR_PATCH_PACKAGE_OPEN_FAILED = 1635; // This patch package could not be opened.  Verify that the patch package exists and that you can access it, or contact the application vendor to verify that this is a valid Windows Installer patch package.
  ERROR_PATCH_PACKAGE_INVALID     = 1636; // This patch package could not be opened.  Contact the application vendor to verify that this is a valid Windows Installer patch package.
  ERROR_PATCH_PACKAGE_UNSUPPORTED = 1637; // This patch package cannot be processed by the Windows Installer service.  You must install a Windows service pack that contains a newer version of the Windows Installer service.

  ERROR_PRODUCT_VERSION = 1638; // Another version of this product is already installed.  Installation of this version cannot continue.  To configure or remove the existing version of this product, use Add/Remove Programs on the Control Panel.

  ERROR_INVALID_COMMAND_LINE = 1639; // Invalid command line argument.  Consult the Windows Installer SDK for detailed command line help.

// The following three error codes are not returned from MSI version 1.0

  ERROR_INSTALL_REMOTE_DISALLOWED = 1640; // Configuration of this product is not permitted from remote sessions. Contact your administrator.

  ERROR_SUCCESS_REBOOT_INITIATED = 1641; // The requested operation completed successfully.  The system will be restarted so the changes can take effect.

  ERROR_PATCH_TARGET_NOT_FOUND = 1642; // The upgrade patch cannot be installed by the Windows Installer service because the program to be upgraded may be missing, or the upgrade patch may update a different version of the program. Verify that the program to be upgraded exists on your computer and that you have the correct upgrade patch.

// The following two error codes are not returned from MSI version 1.0, 1.1. or 1.2

  ERROR_PATCH_PACKAGE_REJECTED      = 1643; // The patch package is not permitted by software restriction policy.
  ERROR_INSTALL_TRANSFORM_REJECTED  = 1644; // One or more customizations are not permitted by software restriction policy.

// The following error code is returned only from MSI post version 2.0

// LOCALIZE BEGIN:

//#ifndef ERROR_INSTALL_REMOTE_PROHIBITED
  ERROR_INSTALL_REMOTE_PROHIBITED   = 1645; // The Windows Installer does not permit installation from a Remote Desktop Connection.

//#endif

// LOCALIZE END

implementation

const
  msilib = 'msi.dll';


{$IFDEF DYNAMIC_LINK}
var
  _MsiCloseHandle: Pointer;

function MsiCloseHandle;
begin
  GetProcedureAddress(_MsiCloseHandle, msilib, 'MsiCloseHandle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiCloseHandle]
  end;
end;
{$ELSE}
function MsiCloseHandle; external msilib name 'MsiCloseHandle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiCloseAllHandles: Pointer;

function MsiCloseAllHandles;
begin
  GetProcedureAddress(_MsiCloseAllHandles, msilib, 'MsiCloseAllHandles');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiCloseAllHandles]
  end;
end;
{$ELSE}
function MsiCloseAllHandles; external msilib name 'MsiCloseAllHandles';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSetInternalUI: Pointer;

function MsiSetInternalUI;
begin
  GetProcedureAddress(_MsiSetInternalUI, msilib, 'MsiSetInternalUI');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSetInternalUI]
  end;
end;
{$ELSE}
function MsiSetInternalUI; external msilib name 'MsiSetInternalUI';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSetExternalUIA: Pointer;

function MsiSetExternalUIA;
begin
  GetProcedureAddress(_MsiSetExternalUIA, msilib, 'MsiSetExternalUIA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSetExternalUIA]
  end;
end;
{$ELSE}
function MsiSetExternalUIA; external msilib name 'MsiSetExternalUIA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSetExternalUIW: Pointer;

function MsiSetExternalUIW;
begin
  GetProcedureAddress(_MsiSetExternalUIW, msilib, 'MsiSetExternalUIW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSetExternalUIW]
  end;
end;
{$ELSE}
function MsiSetExternalUIW; external msilib name 'MsiSetExternalUIW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSetExternalUI: Pointer;

function MsiSetExternalUI;
begin
  GetProcedureAddress(_MsiSetExternalUI, msilib, 'MsiSetExternalUIW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSetExternalUI]
  end;
end;
{$ELSE}
function MsiSetExternalUI; external msilib name 'MsiSetExternalUIW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSetExternalUI: Pointer;

function MsiSetExternalUI;
begin
  GetProcedureAddress(_MsiSetExternalUI, msilib, 'MsiSetExternalUIA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSetExternalUI]
  end;
end;
{$ELSE}
function MsiSetExternalUI; external msilib name 'MsiSetExternalUIA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnableLogA: Pointer;

function MsiEnableLogA;
begin
  GetProcedureAddress(_MsiEnableLogA, msilib, 'MsiEnableLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnableLogA]
  end;
end;
{$ELSE}
function MsiEnableLogA; external msilib name 'MsiEnableLogA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnableLogW: Pointer;

function MsiEnableLogW;
begin
  GetProcedureAddress(_MsiEnableLogW, msilib, 'MsiEnableLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnableLogW]
  end;
end;
{$ELSE}
function MsiEnableLogW; external msilib name 'MsiEnableLogW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnableLog: Pointer;

function MsiEnableLog;
begin
  GetProcedureAddress(_MsiEnableLog, msilib, 'MsiEnableLogW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnableLog]
  end;
end;
{$ELSE}
function MsiEnableLog; external msilib name 'MsiEnableLogW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnableLog: Pointer;

function MsiEnableLog;
begin
  GetProcedureAddress(_MsiEnableLog, msilib, 'MsiEnableLogA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnableLog]
  end;
end;
{$ELSE}
function MsiEnableLog; external msilib name 'MsiEnableLogA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryProductStateA: Pointer;

function MsiQueryProductStateA;
begin
  GetProcedureAddress(_MsiQueryProductStateA, msilib, 'MsiQueryProductStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryProductStateA]
  end;
end;
{$ELSE}
function MsiQueryProductStateA; external msilib name 'MsiQueryProductStateA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryProductStateW: Pointer;

function MsiQueryProductStateW;
begin
  GetProcedureAddress(_MsiQueryProductStateW, msilib, 'MsiQueryProductStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryProductStateW]
  end;
end;
{$ELSE}
function MsiQueryProductStateW; external msilib name 'MsiQueryProductStateW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryProductState: Pointer;

function MsiQueryProductState;
begin
  GetProcedureAddress(_MsiQueryProductState, msilib, 'MsiQueryProductStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryProductState]
  end;
end;
{$ELSE}
function MsiQueryProductState; external msilib name 'MsiQueryProductStateW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryProductState: Pointer;

function MsiQueryProductState;
begin
  GetProcedureAddress(_MsiQueryProductState, msilib, 'MsiQueryProductStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryProductState]
  end;
end;
{$ELSE}
function MsiQueryProductState; external msilib name 'MsiQueryProductStateA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfoA: Pointer;

function MsiGetProductInfoA;
begin
  GetProcedureAddress(_MsiGetProductInfoA, msilib, 'MsiGetProductInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfoA]
  end;
end;
{$ELSE}
function MsiGetProductInfoA; external msilib name 'MsiGetProductInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfoW: Pointer;

function MsiGetProductInfoW;
begin
  GetProcedureAddress(_MsiGetProductInfoW, msilib, 'MsiGetProductInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfoW]
  end;
end;
{$ELSE}
function MsiGetProductInfoW; external msilib name 'MsiGetProductInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfo: Pointer;

function MsiGetProductInfo;
begin
  GetProcedureAddress(_MsiGetProductInfo, msilib, 'MsiGetProductInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfo]
  end;
end;
{$ELSE}
function MsiGetProductInfo; external msilib name 'MsiGetProductInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfo: Pointer;

function MsiGetProductInfo;
begin
  GetProcedureAddress(_MsiGetProductInfo, msilib, 'MsiGetProductInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfo]
  end;
end;
{$ELSE}
function MsiGetProductInfo; external msilib name 'MsiGetProductInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallProductA: Pointer;

function MsiInstallProductA;
begin
  GetProcedureAddress(_MsiInstallProductA, msilib, 'MsiInstallProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallProductA]
  end;
end;
{$ELSE}
function MsiInstallProductA; external msilib name 'MsiInstallProductA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallProductW: Pointer;

function MsiInstallProductW;
begin
  GetProcedureAddress(_MsiInstallProductW, msilib, 'MsiInstallProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallProductW]
  end;
end;
{$ELSE}
function MsiInstallProductW; external msilib name 'MsiInstallProductW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallProduct: Pointer;

function MsiInstallProduct;
begin
  GetProcedureAddress(_MsiInstallProduct, msilib, 'MsiInstallProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallProduct]
  end;
end;
{$ELSE}
function MsiInstallProduct; external msilib name 'MsiInstallProductW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallProduct: Pointer;

function MsiInstallProduct;
begin
  GetProcedureAddress(_MsiInstallProduct, msilib, 'MsiInstallProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallProduct]
  end;
end;
{$ELSE}
function MsiInstallProduct; external msilib name 'MsiInstallProductA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProductA: Pointer;

function MsiConfigureProductA;
begin
  GetProcedureAddress(_MsiConfigureProductA, msilib, 'MsiConfigureProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProductA]
  end;
end;
{$ELSE}
function MsiConfigureProductA; external msilib name 'MsiConfigureProductA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProductW: Pointer;

function MsiConfigureProductW;
begin
  GetProcedureAddress(_MsiConfigureProductW, msilib, 'MsiConfigureProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProductW]
  end;
end;
{$ELSE}
function MsiConfigureProductW; external msilib name 'MsiConfigureProductW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProduct: Pointer;

function MsiConfigureProduct;
begin
  GetProcedureAddress(_MsiConfigureProduct, msilib, 'MsiConfigureProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProduct]
  end;
end;
{$ELSE}
function MsiConfigureProduct; external msilib name 'MsiConfigureProductW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProduct: Pointer;

function MsiConfigureProduct;
begin
  GetProcedureAddress(_MsiConfigureProduct, msilib, 'MsiConfigureProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProduct]
  end;
end;
{$ELSE}
function MsiConfigureProduct; external msilib name 'MsiConfigureProductA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProductExA: Pointer;

function MsiConfigureProductExA;
begin
  GetProcedureAddress(_MsiConfigureProductExA, msilib, 'MsiConfigureProductExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProductExA]
  end;
end;
{$ELSE}
function MsiConfigureProductExA; external msilib name 'MsiConfigureProductExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProductExW: Pointer;

function MsiConfigureProductExW;
begin
  GetProcedureAddress(_MsiConfigureProductExW, msilib, 'MsiConfigureProductExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProductExW]
  end;
end;
{$ELSE}
function MsiConfigureProductExW; external msilib name 'MsiConfigureProductExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProductEx: Pointer;

function MsiConfigureProductEx;
begin
  GetProcedureAddress(_MsiConfigureProductEx, msilib, 'MsiConfigureProductExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProductEx]
  end;
end;
{$ELSE}
function MsiConfigureProductEx; external msilib name 'MsiConfigureProductExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureProductEx: Pointer;

function MsiConfigureProductEx;
begin
  GetProcedureAddress(_MsiConfigureProductEx, msilib, 'MsiConfigureProductExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureProductEx]
  end;
end;
{$ELSE}
function MsiConfigureProductEx; external msilib name 'MsiConfigureProductExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallProductA: Pointer;

function MsiReinstallProductA;
begin
  GetProcedureAddress(_MsiReinstallProductA, msilib, 'MsiReinstallProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallProductA]
  end;
end;
{$ELSE}
function MsiReinstallProductA; external msilib name 'MsiReinstallProductA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallProductW: Pointer;

function MsiReinstallProductW;
begin
  GetProcedureAddress(_MsiReinstallProductW, msilib, 'MsiReinstallProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallProductW]
  end;
end;
{$ELSE}
function MsiReinstallProductW; external msilib name 'MsiReinstallProductW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallProduct: Pointer;

function MsiReinstallProduct;
begin
  GetProcedureAddress(_MsiReinstallProduct, msilib, 'MsiReinstallProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallProduct]
  end;
end;
{$ELSE}
function MsiReinstallProduct; external msilib name 'MsiReinstallProductW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallProduct: Pointer;

function MsiReinstallProduct;
begin
  GetProcedureAddress(_MsiReinstallProduct, msilib, 'MsiReinstallProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallProduct]
  end;
end;
{$ELSE}
function MsiReinstallProduct; external msilib name 'MsiReinstallProductA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProductExA: Pointer;

function MsiAdvertiseProductExA;
begin
  GetProcedureAddress(_MsiAdvertiseProductExA, msilib, 'MsiAdvertiseProductExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProductExA]
  end;
end;
{$ELSE}
function MsiAdvertiseProductExA; external msilib name 'MsiAdvertiseProductExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProductExW: Pointer;

function MsiAdvertiseProductExW;
begin
  GetProcedureAddress(_MsiAdvertiseProductExW, msilib, 'MsiAdvertiseProductExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProductExW]
  end;
end;
{$ELSE}
function MsiAdvertiseProductExW; external msilib name 'MsiAdvertiseProductExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProductEx: Pointer;

function MsiAdvertiseProductEx;
begin
  GetProcedureAddress(_MsiAdvertiseProductEx, msilib, 'MsiAdvertiseProductExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProductEx]
  end;
end;
{$ELSE}
function MsiAdvertiseProductEx; external msilib name 'MsiAdvertiseProductExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProductEx: Pointer;

function MsiAdvertiseProductEx;
begin
  GetProcedureAddress(_MsiAdvertiseProductEx, msilib, 'MsiAdvertiseProductExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProductEx]
  end;
end;
{$ELSE}
function MsiAdvertiseProductEx; external msilib name 'MsiAdvertiseProductExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProductA: Pointer;

function MsiAdvertiseProductA;
begin
  GetProcedureAddress(_MsiAdvertiseProductA, msilib, 'MsiAdvertiseProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProductA]
  end;
end;
{$ELSE}
function MsiAdvertiseProductA; external msilib name 'MsiAdvertiseProductA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProductW: Pointer;

function MsiAdvertiseProductW;
begin
  GetProcedureAddress(_MsiAdvertiseProductW, msilib, 'MsiAdvertiseProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProductW]
  end;
end;
{$ELSE}
function MsiAdvertiseProductW; external msilib name 'MsiAdvertiseProductW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProduct: Pointer;

function MsiAdvertiseProduct;
begin
  GetProcedureAddress(_MsiAdvertiseProduct, msilib, 'MsiAdvertiseProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProduct]
  end;
end;
{$ELSE}
function MsiAdvertiseProduct; external msilib name 'MsiAdvertiseProductW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseProduct: Pointer;

function MsiAdvertiseProduct;
begin
  GetProcedureAddress(_MsiAdvertiseProduct, msilib, 'MsiAdvertiseProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseProduct]
  end;
end;
{$ELSE}
function MsiAdvertiseProduct; external msilib name 'MsiAdvertiseProductA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProcessAdvertiseScriptA: Pointer;

function MsiProcessAdvertiseScriptA;
begin
  GetProcedureAddress(_MsiProcessAdvertiseScriptA, msilib, 'MsiProcessAdvertiseScriptA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProcessAdvertiseScriptA]
  end;
end;
{$ELSE}
function MsiProcessAdvertiseScriptA; external msilib name 'MsiProcessAdvertiseScriptA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProcessAdvertiseScriptW: Pointer;

function MsiProcessAdvertiseScriptW;
begin
  GetProcedureAddress(_MsiProcessAdvertiseScriptW, msilib, 'MsiProcessAdvertiseScriptW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProcessAdvertiseScriptW]
  end;
end;
{$ELSE}
function MsiProcessAdvertiseScriptW; external msilib name 'MsiProcessAdvertiseScriptW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProcessAdvertiseScript: Pointer;

function MsiProcessAdvertiseScript;
begin
  GetProcedureAddress(_MsiProcessAdvertiseScript, msilib, 'MsiProcessAdvertiseScriptW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProcessAdvertiseScript]
  end;
end;
{$ELSE}
function MsiProcessAdvertiseScript; external msilib name 'MsiProcessAdvertiseScriptW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProcessAdvertiseScript: Pointer;

function MsiProcessAdvertiseScript;
begin
  GetProcedureAddress(_MsiProcessAdvertiseScript, msilib, 'MsiProcessAdvertiseScriptA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProcessAdvertiseScript]
  end;
end;
{$ELSE}
function MsiProcessAdvertiseScript; external msilib name 'MsiProcessAdvertiseScriptA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseScriptA: Pointer;

function MsiAdvertiseScriptA;
begin
  GetProcedureAddress(_MsiAdvertiseScriptA, msilib, 'MsiAdvertiseScriptA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseScriptA]
  end;
end;
{$ELSE}
function MsiAdvertiseScriptA; external msilib name 'MsiAdvertiseScriptA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseScriptW: Pointer;

function MsiAdvertiseScriptW;
begin
  GetProcedureAddress(_MsiAdvertiseScriptW, msilib, 'MsiAdvertiseScriptW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseScriptW]
  end;
end;
{$ELSE}
function MsiAdvertiseScriptW; external msilib name 'MsiAdvertiseScriptW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseScript: Pointer;

function MsiAdvertiseScript;
begin
  GetProcedureAddress(_MsiAdvertiseScript, msilib, 'MsiAdvertiseScriptW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseScript]
  end;
end;
{$ELSE}
function MsiAdvertiseScript; external msilib name 'MsiAdvertiseScriptW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiAdvertiseScript: Pointer;

function MsiAdvertiseScript;
begin
  GetProcedureAddress(_MsiAdvertiseScript, msilib, 'MsiAdvertiseScriptA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiAdvertiseScript]
  end;
end;
{$ELSE}
function MsiAdvertiseScript; external msilib name 'MsiAdvertiseScriptA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfoFromScriptA: Pointer;

function MsiGetProductInfoFromScriptA;
begin
  GetProcedureAddress(_MsiGetProductInfoFromScriptA, msilib, 'MsiGetProductInfoFromScriptA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfoFromScriptA]
  end;
end;
{$ELSE}
function MsiGetProductInfoFromScriptA; external msilib name 'MsiGetProductInfoFromScriptA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfoFromScriptW: Pointer;

function MsiGetProductInfoFromScriptW;
begin
  GetProcedureAddress(_MsiGetProductInfoFromScriptW, msilib, 'MsiGetProductInfoFromScriptW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfoFromScriptW]
  end;
end;
{$ELSE}
function MsiGetProductInfoFromScriptW; external msilib name 'MsiGetProductInfoFromScriptW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfoFromScript: Pointer;

function MsiGetProductInfoFromScript;
begin
  GetProcedureAddress(_MsiGetProductInfoFromScript, msilib, 'MsiGetProductInfoFromScriptW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfoFromScript]
  end;
end;
{$ELSE}
function MsiGetProductInfoFromScript; external msilib name 'MsiGetProductInfoFromScriptW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductInfoFromScript: Pointer;

function MsiGetProductInfoFromScript;
begin
  GetProcedureAddress(_MsiGetProductInfoFromScript, msilib, 'MsiGetProductInfoFromScriptA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductInfoFromScript]
  end;
end;
{$ELSE}
function MsiGetProductInfoFromScript; external msilib name 'MsiGetProductInfoFromScriptA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductCodeA: Pointer;

function MsiGetProductCodeA;
begin
  GetProcedureAddress(_MsiGetProductCodeA, msilib, 'MsiGetProductCodeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductCodeA]
  end;
end;
{$ELSE}
function MsiGetProductCodeA; external msilib name 'MsiGetProductCodeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductCodeW: Pointer;

function MsiGetProductCodeW;
begin
  GetProcedureAddress(_MsiGetProductCodeW, msilib, 'MsiGetProductCodeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductCodeW]
  end;
end;
{$ELSE}
function MsiGetProductCodeW; external msilib name 'MsiGetProductCodeW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductCode: Pointer;

function MsiGetProductCode;
begin
  GetProcedureAddress(_MsiGetProductCode, msilib, 'MsiGetProductCodeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductCode]
  end;
end;
{$ELSE}
function MsiGetProductCode; external msilib name 'MsiGetProductCodeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductCode: Pointer;

function MsiGetProductCode;
begin
  GetProcedureAddress(_MsiGetProductCode, msilib, 'MsiGetProductCodeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductCode]
  end;
end;
{$ELSE}
function MsiGetProductCode; external msilib name 'MsiGetProductCodeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetUserInfoA: Pointer;

function MsiGetUserInfoA;
begin
  GetProcedureAddress(_MsiGetUserInfoA, msilib, 'MsiGetUserInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetUserInfoA]
  end;
end;
{$ELSE}
function MsiGetUserInfoA; external msilib name 'MsiGetUserInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetUserInfoW: Pointer;

function MsiGetUserInfoW;
begin
  GetProcedureAddress(_MsiGetUserInfoW, msilib, 'MsiGetUserInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetUserInfoW]
  end;
end;
{$ELSE}
function MsiGetUserInfoW; external msilib name 'MsiGetUserInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetUserInfo: Pointer;

function MsiGetUserInfo;
begin
  GetProcedureAddress(_MsiGetUserInfo, msilib, 'MsiGetUserInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetUserInfo]
  end;
end;
{$ELSE}
function MsiGetUserInfo; external msilib name 'MsiGetUserInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetUserInfo: Pointer;

function MsiGetUserInfo;
begin
  GetProcedureAddress(_MsiGetUserInfo, msilib, 'MsiGetUserInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetUserInfo]
  end;
end;
{$ELSE}
function MsiGetUserInfo; external msilib name 'MsiGetUserInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiCollectUserInfoA: Pointer;

function MsiCollectUserInfoA;
begin
  GetProcedureAddress(_MsiCollectUserInfoA, msilib, 'MsiCollectUserInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiCollectUserInfoA]
  end;
end;
{$ELSE}
function MsiCollectUserInfoA; external msilib name 'MsiCollectUserInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiCollectUserInfoW: Pointer;

function MsiCollectUserInfoW;
begin
  GetProcedureAddress(_MsiCollectUserInfoW, msilib, 'MsiCollectUserInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiCollectUserInfoW]
  end;
end;
{$ELSE}
function MsiCollectUserInfoW; external msilib name 'MsiCollectUserInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiCollectUserInfo: Pointer;

function MsiCollectUserInfo;
begin
  GetProcedureAddress(_MsiCollectUserInfo, msilib, 'MsiCollectUserInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiCollectUserInfo]
  end;
end;
{$ELSE}
function MsiCollectUserInfo; external msilib name 'MsiCollectUserInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiCollectUserInfo: Pointer;

function MsiCollectUserInfo;
begin
  GetProcedureAddress(_MsiCollectUserInfo, msilib, 'MsiCollectUserInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiCollectUserInfo]
  end;
end;
{$ELSE}
function MsiCollectUserInfo; external msilib name 'MsiCollectUserInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiApplyPatchA: Pointer;

function MsiApplyPatchA;
begin
  GetProcedureAddress(_MsiApplyPatchA, msilib, 'MsiApplyPatchA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiApplyPatchA]
  end;
end;
{$ELSE}
function MsiApplyPatchA; external msilib name 'MsiApplyPatchA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiApplyPatchW: Pointer;

function MsiApplyPatchW;
begin
  GetProcedureAddress(_MsiApplyPatchW, msilib, 'MsiApplyPatchW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiApplyPatchW]
  end;
end;
{$ELSE}
function MsiApplyPatchW; external msilib name 'MsiApplyPatchW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiApplyPatch: Pointer;

function MsiApplyPatch;
begin
  GetProcedureAddress(_MsiApplyPatch, msilib, 'MsiApplyPatchW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiApplyPatch]
  end;
end;
{$ELSE}
function MsiApplyPatch; external msilib name 'MsiApplyPatchW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiApplyPatch: Pointer;

function MsiApplyPatch;
begin
  GetProcedureAddress(_MsiApplyPatch, msilib, 'MsiApplyPatchA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiApplyPatch]
  end;
end;
{$ELSE}
function MsiApplyPatch; external msilib name 'MsiApplyPatchA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetPatchInfoA: Pointer;

function MsiGetPatchInfoA;
begin
  GetProcedureAddress(_MsiGetPatchInfoA, msilib, 'MsiGetPatchInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetPatchInfoA]
  end;
end;
{$ELSE}
function MsiGetPatchInfoA; external msilib name 'MsiGetPatchInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetPatchInfoW: Pointer;

function MsiGetPatchInfoW;
begin
  GetProcedureAddress(_MsiGetPatchInfoW, msilib, 'MsiGetPatchInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetPatchInfoW]
  end;
end;
{$ELSE}
function MsiGetPatchInfoW; external msilib name 'MsiGetPatchInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetPatchInfo: Pointer;

function MsiGetPatchInfo;
begin
  GetProcedureAddress(_MsiGetPatchInfo, msilib, 'MsiGetPatchInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetPatchInfo]
  end;
end;
{$ELSE}
function MsiGetPatchInfo; external msilib name 'MsiGetPatchInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetPatchInfo: Pointer;

function MsiGetPatchInfo;
begin
  GetProcedureAddress(_MsiGetPatchInfo, msilib, 'MsiGetPatchInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetPatchInfo]
  end;
end;
{$ELSE}
function MsiGetPatchInfo; external msilib name 'MsiGetPatchInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumPatchesA: Pointer;

function MsiEnumPatchesA;
begin
  GetProcedureAddress(_MsiEnumPatchesA, msilib, 'MsiEnumPatchesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumPatchesA]
  end;
end;
{$ELSE}
function MsiEnumPatchesA; external msilib name 'MsiEnumPatchesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumPatchesW: Pointer;

function MsiEnumPatchesW;
begin
  GetProcedureAddress(_MsiEnumPatchesW, msilib, 'MsiEnumPatchesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumPatchesW]
  end;
end;
{$ELSE}
function MsiEnumPatchesW; external msilib name 'MsiEnumPatchesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumPatches: Pointer;

function MsiEnumPatches;
begin
  GetProcedureAddress(_MsiEnumPatches, msilib, 'MsiEnumPatchesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumPatches]
  end;
end;
{$ELSE}
function MsiEnumPatches; external msilib name 'MsiEnumPatchesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumPatches: Pointer;

function MsiEnumPatches;
begin
  GetProcedureAddress(_MsiEnumPatches, msilib, 'MsiEnumPatchesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumPatches]
  end;
end;
{$ELSE}
function MsiEnumPatches; external msilib name 'MsiEnumPatchesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryFeatureStateA: Pointer;

function MsiQueryFeatureStateA;
begin
  GetProcedureAddress(_MsiQueryFeatureStateA, msilib, 'MsiQueryFeatureStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryFeatureStateA]
  end;
end;
{$ELSE}
function MsiQueryFeatureStateA; external msilib name 'MsiQueryFeatureStateA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryFeatureStateW: Pointer;

function MsiQueryFeatureStateW;
begin
  GetProcedureAddress(_MsiQueryFeatureStateW, msilib, 'MsiQueryFeatureStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryFeatureStateW]
  end;
end;
{$ELSE}
function MsiQueryFeatureStateW; external msilib name 'MsiQueryFeatureStateW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryFeatureState: Pointer;

function MsiQueryFeatureState;
begin
  GetProcedureAddress(_MsiQueryFeatureState, msilib, 'MsiQueryFeatureStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryFeatureState]
  end;
end;
{$ELSE}
function MsiQueryFeatureState; external msilib name 'MsiQueryFeatureStateW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiQueryFeatureState: Pointer;

function MsiQueryFeatureState;
begin
  GetProcedureAddress(_MsiQueryFeatureState, msilib, 'MsiQueryFeatureStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiQueryFeatureState]
  end;
end;
{$ELSE}
function MsiQueryFeatureState; external msilib name 'MsiQueryFeatureStateA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeatureA: Pointer;

function MsiUseFeatureA;
begin
  GetProcedureAddress(_MsiUseFeatureA, msilib, 'MsiUseFeatureA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeatureA]
  end;
end;
{$ELSE}
function MsiUseFeatureA; external msilib name 'MsiUseFeatureA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeatureW: Pointer;

function MsiUseFeatureW;
begin
  GetProcedureAddress(_MsiUseFeatureW, msilib, 'MsiUseFeatureW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeatureW]
  end;
end;
{$ELSE}
function MsiUseFeatureW; external msilib name 'MsiUseFeatureW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeature: Pointer;

function MsiUseFeature;
begin
  GetProcedureAddress(_MsiUseFeature, msilib, 'MsiUseFeatureW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeature]
  end;
end;
{$ELSE}
function MsiUseFeature; external msilib name 'MsiUseFeatureW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeature: Pointer;

function MsiUseFeature;
begin
  GetProcedureAddress(_MsiUseFeature, msilib, 'MsiUseFeatureA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeature]
  end;
end;
{$ELSE}
function MsiUseFeature; external msilib name 'MsiUseFeatureA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeatureExA: Pointer;

function MsiUseFeatureExA;
begin
  GetProcedureAddress(_MsiUseFeatureExA, msilib, 'MsiUseFeatureExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeatureExA]
  end;
end;
{$ELSE}
function MsiUseFeatureExA; external msilib name 'MsiUseFeatureExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeatureExW: Pointer;

function MsiUseFeatureExW;
begin
  GetProcedureAddress(_MsiUseFeatureExW, msilib, 'MsiUseFeatureExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeatureExW]
  end;
end;
{$ELSE}
function MsiUseFeatureExW; external msilib name 'MsiUseFeatureExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeatureEx: Pointer;

function MsiUseFeatureEx;
begin
  GetProcedureAddress(_MsiUseFeatureEx, msilib, 'MsiUseFeatureExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeatureEx]
  end;
end;
{$ELSE}
function MsiUseFeatureEx; external msilib name 'MsiUseFeatureExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiUseFeatureEx: Pointer;

function MsiUseFeatureEx;
begin
  GetProcedureAddress(_MsiUseFeatureEx, msilib, 'MsiUseFeatureExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiUseFeatureEx]
  end;
end;
{$ELSE}
function MsiUseFeatureEx; external msilib name 'MsiUseFeatureExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureUsageA: Pointer;

function MsiGetFeatureUsageA;
begin
  GetProcedureAddress(_MsiGetFeatureUsageA, msilib, 'MsiGetFeatureUsageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureUsageA]
  end;
end;
{$ELSE}
function MsiGetFeatureUsageA; external msilib name 'MsiGetFeatureUsageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureUsageW: Pointer;

function MsiGetFeatureUsageW;
begin
  GetProcedureAddress(_MsiGetFeatureUsageW, msilib, 'MsiGetFeatureUsageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureUsageW]
  end;
end;
{$ELSE}
function MsiGetFeatureUsageW; external msilib name 'MsiGetFeatureUsageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureUsage: Pointer;

function MsiGetFeatureUsage;
begin
  GetProcedureAddress(_MsiGetFeatureUsage, msilib, 'MsiGetFeatureUsageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureUsage]
  end;
end;
{$ELSE}
function MsiGetFeatureUsage; external msilib name 'MsiGetFeatureUsageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureUsage: Pointer;

function MsiGetFeatureUsage;
begin
  GetProcedureAddress(_MsiGetFeatureUsage, msilib, 'MsiGetFeatureUsageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureUsage]
  end;
end;
{$ELSE}
function MsiGetFeatureUsage; external msilib name 'MsiGetFeatureUsageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureFeatureA: Pointer;

function MsiConfigureFeatureA;
begin
  GetProcedureAddress(_MsiConfigureFeatureA, msilib, 'MsiConfigureFeatureA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureFeatureA]
  end;
end;
{$ELSE}
function MsiConfigureFeatureA; external msilib name 'MsiConfigureFeatureA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureFeatureW: Pointer;

function MsiConfigureFeatureW;
begin
  GetProcedureAddress(_MsiConfigureFeatureW, msilib, 'MsiConfigureFeatureW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureFeatureW]
  end;
end;
{$ELSE}
function MsiConfigureFeatureW; external msilib name 'MsiConfigureFeatureW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureFeature: Pointer;

function MsiConfigureFeature;
begin
  GetProcedureAddress(_MsiConfigureFeature, msilib, 'MsiConfigureFeatureW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureFeature]
  end;
end;
{$ELSE}
function MsiConfigureFeature; external msilib name 'MsiConfigureFeatureW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiConfigureFeature: Pointer;

function MsiConfigureFeature;
begin
  GetProcedureAddress(_MsiConfigureFeature, msilib, 'MsiConfigureFeatureA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiConfigureFeature]
  end;
end;
{$ELSE}
function MsiConfigureFeature; external msilib name 'MsiConfigureFeatureA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallFeatureA: Pointer;

function MsiReinstallFeatureA;
begin
  GetProcedureAddress(_MsiReinstallFeatureA, msilib, 'MsiReinstallFeatureA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallFeatureA]
  end;
end;
{$ELSE}
function MsiReinstallFeatureA; external msilib name 'MsiReinstallFeatureA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallFeatureW: Pointer;

function MsiReinstallFeatureW;
begin
  GetProcedureAddress(_MsiReinstallFeatureW, msilib, 'MsiReinstallFeatureW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallFeatureW]
  end;
end;
{$ELSE}
function MsiReinstallFeatureW; external msilib name 'MsiReinstallFeatureW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallFeature: Pointer;

function MsiReinstallFeature;
begin
  GetProcedureAddress(_MsiReinstallFeature, msilib, 'MsiReinstallFeatureW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallFeature]
  end;
end;
{$ELSE}
function MsiReinstallFeature; external msilib name 'MsiReinstallFeatureW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiReinstallFeature: Pointer;

function MsiReinstallFeature;
begin
  GetProcedureAddress(_MsiReinstallFeature, msilib, 'MsiReinstallFeatureA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiReinstallFeature]
  end;
end;
{$ELSE}
function MsiReinstallFeature; external msilib name 'MsiReinstallFeatureA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideComponentA: Pointer;

function MsiProvideComponentA;
begin
  GetProcedureAddress(_MsiProvideComponentA, msilib, 'MsiProvideComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideComponentA]
  end;
end;
{$ELSE}
function MsiProvideComponentA; external msilib name 'MsiProvideComponentA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideComponentW: Pointer;

function MsiProvideComponentW;
begin
  GetProcedureAddress(_MsiProvideComponentW, msilib, 'MsiProvideComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideComponentW]
  end;
end;
{$ELSE}
function MsiProvideComponentW; external msilib name 'MsiProvideComponentW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideComponent: Pointer;

function MsiProvideComponent;
begin
  GetProcedureAddress(_MsiProvideComponent, msilib, 'MsiProvideComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideComponent]
  end;
end;
{$ELSE}
function MsiProvideComponent; external msilib name 'MsiProvideComponentW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideComponent: Pointer;

function MsiProvideComponent;
begin
  GetProcedureAddress(_MsiProvideComponent, msilib, 'MsiProvideComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideComponent]
  end;
end;
{$ELSE}
function MsiProvideComponent; external msilib name 'MsiProvideComponentA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponentA: Pointer;

function MsiProvideQualifiedComponentA;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponentA, msilib, 'MsiProvideQualifiedComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponentA]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponentA; external msilib name 'MsiProvideQualifiedComponentA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponentW: Pointer;

function MsiProvideQualifiedComponentW;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponentW, msilib, 'MsiProvideQualifiedComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponentW]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponentW; external msilib name 'MsiProvideQualifiedComponentW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponent: Pointer;

function MsiProvideQualifiedComponent;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponent, msilib, 'MsiProvideQualifiedComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponent]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponent; external msilib name 'MsiProvideQualifiedComponentW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponent: Pointer;

function MsiProvideQualifiedComponent;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponent, msilib, 'MsiProvideQualifiedComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponent]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponent; external msilib name 'MsiProvideQualifiedComponentA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponentExA: Pointer;

function MsiProvideQualifiedComponentExA;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponentExA, msilib, 'MsiProvideQualifiedComponentExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponentExA]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponentExA; external msilib name 'MsiProvideQualifiedComponentExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponentExW: Pointer;

function MsiProvideQualifiedComponentExW;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponentExW, msilib, 'MsiProvideQualifiedComponentExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponentExW]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponentExW; external msilib name 'MsiProvideQualifiedComponentExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponentEx: Pointer;

function MsiProvideQualifiedComponentEx;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponentEx, msilib, 'MsiProvideQualifiedComponentExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponentEx]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponentEx; external msilib name 'MsiProvideQualifiedComponentExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideQualifiedComponentEx: Pointer;

function MsiProvideQualifiedComponentEx;
begin
  GetProcedureAddress(_MsiProvideQualifiedComponentEx, msilib, 'MsiProvideQualifiedComponentExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideQualifiedComponentEx]
  end;
end;
{$ELSE}
function MsiProvideQualifiedComponentEx; external msilib name 'MsiProvideQualifiedComponentExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetComponentPathA: Pointer;

function MsiGetComponentPathA;
begin
  GetProcedureAddress(_MsiGetComponentPathA, msilib, 'MsiGetComponentPathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetComponentPathA]
  end;
end;
{$ELSE}
function MsiGetComponentPathA; external msilib name 'MsiGetComponentPathA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetComponentPathW: Pointer;

function MsiGetComponentPathW;
begin
  GetProcedureAddress(_MsiGetComponentPathW, msilib, 'MsiGetComponentPathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetComponentPathW]
  end;
end;
{$ELSE}
function MsiGetComponentPathW; external msilib name 'MsiGetComponentPathW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetComponentPath: Pointer;

function MsiGetComponentPath;
begin
  GetProcedureAddress(_MsiGetComponentPath, msilib, 'MsiGetComponentPathW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetComponentPath]
  end;
end;
{$ELSE}
function MsiGetComponentPath; external msilib name 'MsiGetComponentPathW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetComponentPath: Pointer;

function MsiGetComponentPath;
begin
  GetProcedureAddress(_MsiGetComponentPath, msilib, 'MsiGetComponentPathA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetComponentPath]
  end;
end;
{$ELSE}
function MsiGetComponentPath; external msilib name 'MsiGetComponentPathA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideAssemblyA: Pointer;

function MsiProvideAssemblyA;
begin
  GetProcedureAddress(_MsiProvideAssemblyA, msilib, 'MsiProvideAssemblyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideAssemblyA]
  end;
end;
{$ELSE}
function MsiProvideAssemblyA; external msilib name 'MsiProvideAssemblyA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideAssemblyW: Pointer;

function MsiProvideAssemblyW;
begin
  GetProcedureAddress(_MsiProvideAssemblyW, msilib, 'MsiProvideAssemblyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideAssemblyW]
  end;
end;
{$ELSE}
function MsiProvideAssemblyW; external msilib name 'MsiProvideAssemblyW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideAssembly: Pointer;

function MsiProvideAssembly;
begin
  GetProcedureAddress(_MsiProvideAssembly, msilib, 'MsiProvideAssemblyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideAssembly]
  end;
end;
{$ELSE}
function MsiProvideAssembly; external msilib name 'MsiProvideAssemblyW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiProvideAssembly: Pointer;

function MsiProvideAssembly;
begin
  GetProcedureAddress(_MsiProvideAssembly, msilib, 'MsiProvideAssemblyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiProvideAssembly]
  end;
end;
{$ELSE}
function MsiProvideAssembly; external msilib name 'MsiProvideAssemblyA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumProductsA: Pointer;

function MsiEnumProductsA;
begin
  GetProcedureAddress(_MsiEnumProductsA, msilib, 'MsiEnumProductsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumProductsA]
  end;
end;
{$ELSE}
function MsiEnumProductsA; external msilib name 'MsiEnumProductsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumProductsW: Pointer;

function MsiEnumProductsW;
begin
  GetProcedureAddress(_MsiEnumProductsW, msilib, 'MsiEnumProductsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumProductsW]
  end;
end;
{$ELSE}
function MsiEnumProductsW; external msilib name 'MsiEnumProductsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumProducts: Pointer;

function MsiEnumProducts;
begin
  GetProcedureAddress(_MsiEnumProducts, msilib, 'MsiEnumProductsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumProducts]
  end;
end;
{$ELSE}
function MsiEnumProducts; external msilib name 'MsiEnumProductsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumProducts: Pointer;

function MsiEnumProducts;
begin
  GetProcedureAddress(_MsiEnumProducts, msilib, 'MsiEnumProductsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumProducts]
  end;
end;
{$ELSE}
function MsiEnumProducts; external msilib name 'MsiEnumProductsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF WIN32_MSI_110}


{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumRelatedProductsA: Pointer;

function MsiEnumRelatedProductsA;
begin
  GetProcedureAddress(_MsiEnumRelatedProductsA, msilib, 'MsiEnumRelatedProductsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumRelatedProductsA]
  end;
end;
{$ELSE}
function MsiEnumRelatedProductsA; external msilib name 'MsiEnumRelatedProductsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumRelatedProductsW: Pointer;

function MsiEnumRelatedProductsW;
begin
  GetProcedureAddress(_MsiEnumRelatedProductsW, msilib, 'MsiEnumRelatedProductsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumRelatedProductsW]
  end;
end;
{$ELSE}
function MsiEnumRelatedProductsW; external msilib name 'MsiEnumRelatedProductsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumRelatedProducts: Pointer;

function MsiEnumRelatedProducts;
begin
  GetProcedureAddress(_MsiEnumRelatedProducts, msilib, 'MsiEnumRelatedProductsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumRelatedProducts]
  end;
end;
{$ELSE}
function MsiEnumRelatedProducts; external msilib name 'MsiEnumRelatedProductsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumRelatedProducts: Pointer;

function MsiEnumRelatedProducts;
begin
  GetProcedureAddress(_MsiEnumRelatedProducts, msilib, 'MsiEnumRelatedProductsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumRelatedProducts]
  end;
end;
{$ELSE}
function MsiEnumRelatedProducts; external msilib name 'MsiEnumRelatedProductsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$ENDIF WIN32_MSI_110}


{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumFeaturesA: Pointer;

function MsiEnumFeaturesA;
begin
  GetProcedureAddress(_MsiEnumFeaturesA, msilib, 'MsiEnumFeaturesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumFeaturesA]
  end;
end;
{$ELSE}
function MsiEnumFeaturesA; external msilib name 'MsiEnumFeaturesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumFeaturesW: Pointer;

function MsiEnumFeaturesW;
begin
  GetProcedureAddress(_MsiEnumFeaturesW, msilib, 'MsiEnumFeaturesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumFeaturesW]
  end;
end;
{$ELSE}
function MsiEnumFeaturesW; external msilib name 'MsiEnumFeaturesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumFeatures: Pointer;

function MsiEnumFeatures;
begin
  GetProcedureAddress(_MsiEnumFeatures, msilib, 'MsiEnumFeaturesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumFeatures]
  end;
end;
{$ELSE}
function MsiEnumFeatures; external msilib name 'MsiEnumFeaturesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumFeatures: Pointer;

function MsiEnumFeatures;
begin
  GetProcedureAddress(_MsiEnumFeatures, msilib, 'MsiEnumFeaturesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumFeatures]
  end;
end;
{$ELSE}
function MsiEnumFeatures; external msilib name 'MsiEnumFeaturesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponentsA: Pointer;

function MsiEnumComponentsA;
begin
  GetProcedureAddress(_MsiEnumComponentsA, msilib, 'MsiEnumComponentsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponentsA]
  end;
end;
{$ELSE}
function MsiEnumComponentsA; external msilib name 'MsiEnumComponentsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponentsW: Pointer;

function MsiEnumComponentsW;
begin
  GetProcedureAddress(_MsiEnumComponentsW, msilib, 'MsiEnumComponentsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponentsW]
  end;
end;
{$ELSE}
function MsiEnumComponentsW; external msilib name 'MsiEnumComponentsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponents: Pointer;

function MsiEnumComponents;
begin
  GetProcedureAddress(_MsiEnumComponents, msilib, 'MsiEnumComponentsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponents]
  end;
end;
{$ELSE}
function MsiEnumComponents; external msilib name 'MsiEnumComponentsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponents: Pointer;

function MsiEnumComponents;
begin
  GetProcedureAddress(_MsiEnumComponents, msilib, 'MsiEnumComponentsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponents]
  end;
end;
{$ELSE}
function MsiEnumComponents; external msilib name 'MsiEnumComponentsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumClientsA: Pointer;

function MsiEnumClientsA;
begin
  GetProcedureAddress(_MsiEnumClientsA, msilib, 'MsiEnumClientsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumClientsA]
  end;
end;
{$ELSE}
function MsiEnumClientsA; external msilib name 'MsiEnumClientsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumClientsW: Pointer;

function MsiEnumClientsW;
begin
  GetProcedureAddress(_MsiEnumClientsW, msilib, 'MsiEnumClientsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumClientsW]
  end;
end;
{$ELSE}
function MsiEnumClientsW; external msilib name 'MsiEnumClientsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumClients: Pointer;

function MsiEnumClients;
begin
  GetProcedureAddress(_MsiEnumClients, msilib, 'MsiEnumClientsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumClients]
  end;
end;
{$ELSE}
function MsiEnumClients; external msilib name 'MsiEnumClientsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumClients: Pointer;

function MsiEnumClients;
begin
  GetProcedureAddress(_MsiEnumClients, msilib, 'MsiEnumClientsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumClients]
  end;
end;
{$ELSE}
function MsiEnumClients; external msilib name 'MsiEnumClientsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponentQualifiersA: Pointer;

function MsiEnumComponentQualifiersA;
begin
  GetProcedureAddress(_MsiEnumComponentQualifiersA, msilib, 'MsiEnumComponentQualifiersA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponentQualifiersA]
  end;
end;
{$ELSE}
function MsiEnumComponentQualifiersA; external msilib name 'MsiEnumComponentQualifiersA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponentQualifiersW: Pointer;

function MsiEnumComponentQualifiersW;
begin
  GetProcedureAddress(_MsiEnumComponentQualifiersW, msilib, 'MsiEnumComponentQualifiersW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponentQualifiersW]
  end;
end;
{$ELSE}
function MsiEnumComponentQualifiersW; external msilib name 'MsiEnumComponentQualifiersW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponentQualifiers: Pointer;

function MsiEnumComponentQualifiers;
begin
  GetProcedureAddress(_MsiEnumComponentQualifiers, msilib, 'MsiEnumComponentQualifiersW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponentQualifiers]
  end;
end;
{$ELSE}
function MsiEnumComponentQualifiers; external msilib name 'MsiEnumComponentQualifiersW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiEnumComponentQualifiers: Pointer;

function MsiEnumComponentQualifiers;
begin
  GetProcedureAddress(_MsiEnumComponentQualifiers, msilib, 'MsiEnumComponentQualifiersA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiEnumComponentQualifiers]
  end;
end;
{$ELSE}
function MsiEnumComponentQualifiers; external msilib name 'MsiEnumComponentQualifiersA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenProductA: Pointer;

function MsiOpenProductA;
begin
  GetProcedureAddress(_MsiOpenProductA, msilib, 'MsiOpenProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenProductA]
  end;
end;
{$ELSE}
function MsiOpenProductA; external msilib name 'MsiOpenProductA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenProductW: Pointer;

function MsiOpenProductW;
begin
  GetProcedureAddress(_MsiOpenProductW, msilib, 'MsiOpenProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenProductW]
  end;
end;
{$ELSE}
function MsiOpenProductW; external msilib name 'MsiOpenProductW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenProduct: Pointer;

function MsiOpenProduct;
begin
  GetProcedureAddress(_MsiOpenProduct, msilib, 'MsiOpenProductW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenProduct]
  end;
end;
{$ELSE}
function MsiOpenProduct; external msilib name 'MsiOpenProductW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenProduct: Pointer;

function MsiOpenProduct;
begin
  GetProcedureAddress(_MsiOpenProduct, msilib, 'MsiOpenProductA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenProduct]
  end;
end;
{$ELSE}
function MsiOpenProduct; external msilib name 'MsiOpenProductA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackageA: Pointer;

function MsiOpenPackageA;
begin
  GetProcedureAddress(_MsiOpenPackageA, msilib, 'MsiOpenPackageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackageA]
  end;
end;
{$ELSE}
function MsiOpenPackageA; external msilib name 'MsiOpenPackageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackageW: Pointer;

function MsiOpenPackageW;
begin
  GetProcedureAddress(_MsiOpenPackageW, msilib, 'MsiOpenPackageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackageW]
  end;
end;
{$ELSE}
function MsiOpenPackageW; external msilib name 'MsiOpenPackageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackage: Pointer;

function MsiOpenPackage;
begin
  GetProcedureAddress(_MsiOpenPackage, msilib, 'MsiOpenPackageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackage]
  end;
end;
{$ELSE}
function MsiOpenPackage; external msilib name 'MsiOpenPackageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackage: Pointer;

function MsiOpenPackage;
begin
  GetProcedureAddress(_MsiOpenPackage, msilib, 'MsiOpenPackageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackage]
  end;
end;
{$ELSE}
function MsiOpenPackage; external msilib name 'MsiOpenPackageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackageExA: Pointer;

function MsiOpenPackageExA;
begin
  GetProcedureAddress(_MsiOpenPackageExA, msilib, 'MsiOpenPackageExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackageExA]
  end;
end;
{$ELSE}
function MsiOpenPackageExA; external msilib name 'MsiOpenPackageExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackageExW: Pointer;

function MsiOpenPackageExW;
begin
  GetProcedureAddress(_MsiOpenPackageExW, msilib, 'MsiOpenPackageExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackageExW]
  end;
end;
{$ELSE}
function MsiOpenPackageExW; external msilib name 'MsiOpenPackageExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackageEx: Pointer;

function MsiOpenPackageEx;
begin
  GetProcedureAddress(_MsiOpenPackageEx, msilib, 'MsiOpenPackageExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackageEx]
  end;
end;
{$ELSE}
function MsiOpenPackageEx; external msilib name 'MsiOpenPackageExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiOpenPackageEx: Pointer;

function MsiOpenPackageEx;
begin
  GetProcedureAddress(_MsiOpenPackageEx, msilib, 'MsiOpenPackageExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiOpenPackageEx]
  end;
end;
{$ELSE}
function MsiOpenPackageEx; external msilib name 'MsiOpenPackageExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductPropertyA: Pointer;

function MsiGetProductPropertyA;
begin
  GetProcedureAddress(_MsiGetProductPropertyA, msilib, 'MsiGetProductPropertyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductPropertyA]
  end;
end;
{$ELSE}
function MsiGetProductPropertyA; external msilib name 'MsiGetProductPropertyA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductPropertyW: Pointer;

function MsiGetProductPropertyW;
begin
  GetProcedureAddress(_MsiGetProductPropertyW, msilib, 'MsiGetProductPropertyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductPropertyW]
  end;
end;
{$ELSE}
function MsiGetProductPropertyW; external msilib name 'MsiGetProductPropertyW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductProperty: Pointer;

function MsiGetProductProperty;
begin
  GetProcedureAddress(_MsiGetProductProperty, msilib, 'MsiGetProductPropertyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductProperty]
  end;
end;
{$ELSE}
function MsiGetProductProperty; external msilib name 'MsiGetProductPropertyW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetProductProperty: Pointer;

function MsiGetProductProperty;
begin
  GetProcedureAddress(_MsiGetProductProperty, msilib, 'MsiGetProductPropertyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetProductProperty]
  end;
end;
{$ELSE}
function MsiGetProductProperty; external msilib name 'MsiGetProductPropertyA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiVerifyPackageA: Pointer;

function MsiVerifyPackageA;
begin
  GetProcedureAddress(_MsiVerifyPackageA, msilib, 'MsiVerifyPackageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiVerifyPackageA]
  end;
end;
{$ELSE}
function MsiVerifyPackageA; external msilib name 'MsiVerifyPackageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiVerifyPackageW: Pointer;

function MsiVerifyPackageW;
begin
  GetProcedureAddress(_MsiVerifyPackageW, msilib, 'MsiVerifyPackageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiVerifyPackageW]
  end;
end;
{$ELSE}
function MsiVerifyPackageW; external msilib name 'MsiVerifyPackageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiVerifyPackage: Pointer;

function MsiVerifyPackage;
begin
  GetProcedureAddress(_MsiVerifyPackage, msilib, 'MsiVerifyPackageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiVerifyPackage]
  end;
end;
{$ELSE}
function MsiVerifyPackage; external msilib name 'MsiVerifyPackageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiVerifyPackage: Pointer;

function MsiVerifyPackage;
begin
  GetProcedureAddress(_MsiVerifyPackage, msilib, 'MsiVerifyPackageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiVerifyPackage]
  end;
end;
{$ELSE}
function MsiVerifyPackage; external msilib name 'MsiVerifyPackageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureInfoA: Pointer;

function MsiGetFeatureInfoA;
begin
  GetProcedureAddress(_MsiGetFeatureInfoA, msilib, 'MsiGetFeatureInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureInfoA]
  end;
end;
{$ELSE}
function MsiGetFeatureInfoA; external msilib name 'MsiGetFeatureInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureInfoW: Pointer;

function MsiGetFeatureInfoW;
begin
  GetProcedureAddress(_MsiGetFeatureInfoW, msilib, 'MsiGetFeatureInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureInfoW]
  end;
end;
{$ELSE}
function MsiGetFeatureInfoW; external msilib name 'MsiGetFeatureInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureInfo: Pointer;

function MsiGetFeatureInfo;
begin
  GetProcedureAddress(_MsiGetFeatureInfo, msilib, 'MsiGetFeatureInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureInfo]
  end;
end;
{$ELSE}
function MsiGetFeatureInfo; external msilib name 'MsiGetFeatureInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFeatureInfo: Pointer;

function MsiGetFeatureInfo;
begin
  GetProcedureAddress(_MsiGetFeatureInfo, msilib, 'MsiGetFeatureInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFeatureInfo]
  end;
end;
{$ELSE}
function MsiGetFeatureInfo; external msilib name 'MsiGetFeatureInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingComponentA: Pointer;

function MsiInstallMissingComponentA;
begin
  GetProcedureAddress(_MsiInstallMissingComponentA, msilib, 'MsiInstallMissingComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingComponentA]
  end;
end;
{$ELSE}
function MsiInstallMissingComponentA; external msilib name 'MsiInstallMissingComponentA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingComponentW: Pointer;

function MsiInstallMissingComponentW;
begin
  GetProcedureAddress(_MsiInstallMissingComponentW, msilib, 'MsiInstallMissingComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingComponentW]
  end;
end;
{$ELSE}
function MsiInstallMissingComponentW; external msilib name 'MsiInstallMissingComponentW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingComponent: Pointer;

function MsiInstallMissingComponent;
begin
  GetProcedureAddress(_MsiInstallMissingComponent, msilib, 'MsiInstallMissingComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingComponent]
  end;
end;
{$ELSE}
function MsiInstallMissingComponent; external msilib name 'MsiInstallMissingComponentW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingComponent: Pointer;

function MsiInstallMissingComponent;
begin
  GetProcedureAddress(_MsiInstallMissingComponent, msilib, 'MsiInstallMissingComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingComponent]
  end;
end;
{$ELSE}
function MsiInstallMissingComponent; external msilib name 'MsiInstallMissingComponentA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingFileA: Pointer;

function MsiInstallMissingFileA;
begin
  GetProcedureAddress(_MsiInstallMissingFileA, msilib, 'MsiInstallMissingFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingFileA]
  end;
end;
{$ELSE}
function MsiInstallMissingFileA; external msilib name 'MsiInstallMissingFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingFileW: Pointer;

function MsiInstallMissingFileW;
begin
  GetProcedureAddress(_MsiInstallMissingFileW, msilib, 'MsiInstallMissingFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingFileW]
  end;
end;
{$ELSE}
function MsiInstallMissingFileW; external msilib name 'MsiInstallMissingFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingFile: Pointer;

function MsiInstallMissingFile;
begin
  GetProcedureAddress(_MsiInstallMissingFile, msilib, 'MsiInstallMissingFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingFile]
  end;
end;
{$ELSE}
function MsiInstallMissingFile; external msilib name 'MsiInstallMissingFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiInstallMissingFile: Pointer;

function MsiInstallMissingFile;
begin
  GetProcedureAddress(_MsiInstallMissingFile, msilib, 'MsiInstallMissingFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiInstallMissingFile]
  end;
end;
{$ELSE}
function MsiInstallMissingFile; external msilib name 'MsiInstallMissingFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiLocateComponentA: Pointer;

function MsiLocateComponentA;
begin
  GetProcedureAddress(_MsiLocateComponentA, msilib, 'MsiLocateComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiLocateComponentA]
  end;
end;
{$ELSE}
function MsiLocateComponentA; external msilib name 'MsiLocateComponentA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiLocateComponentW: Pointer;

function MsiLocateComponentW;
begin
  GetProcedureAddress(_MsiLocateComponentW, msilib, 'MsiLocateComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiLocateComponentW]
  end;
end;
{$ELSE}
function MsiLocateComponentW; external msilib name 'MsiLocateComponentW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiLocateComponent: Pointer;

function MsiLocateComponent;
begin
  GetProcedureAddress(_MsiLocateComponent, msilib, 'MsiLocateComponentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiLocateComponent]
  end;
end;
{$ELSE}
function MsiLocateComponent; external msilib name 'MsiLocateComponentW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiLocateComponent: Pointer;

function MsiLocateComponent;
begin
  GetProcedureAddress(_MsiLocateComponent, msilib, 'MsiLocateComponentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiLocateComponent]
  end;
end;
{$ELSE}
function MsiLocateComponent; external msilib name 'MsiLocateComponentA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF WIN32_MSI_110}


{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListClearAllA: Pointer;

function MsiSourceListClearAllA;
begin
  GetProcedureAddress(_MsiSourceListClearAllA, msilib, 'MsiSourceListClearAllA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListClearAllA]
  end;
end;
{$ELSE}
function MsiSourceListClearAllA; external msilib name 'MsiSourceListClearAllA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListClearAllW: Pointer;

function MsiSourceListClearAllW;
begin
  GetProcedureAddress(_MsiSourceListClearAllW, msilib, 'MsiSourceListClearAllW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListClearAllW]
  end;
end;
{$ELSE}
function MsiSourceListClearAllW; external msilib name 'MsiSourceListClearAllW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListClearAll: Pointer;

function MsiSourceListClearAll;
begin
  GetProcedureAddress(_MsiSourceListClearAll, msilib, 'MsiSourceListClearAllW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListClearAll]
  end;
end;
{$ELSE}
function MsiSourceListClearAll; external msilib name 'MsiSourceListClearAllW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListClearAll: Pointer;

function MsiSourceListClearAll;
begin
  GetProcedureAddress(_MsiSourceListClearAll, msilib, 'MsiSourceListClearAllA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListClearAll]
  end;
end;
{$ELSE}
function MsiSourceListClearAll; external msilib name 'MsiSourceListClearAllA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListAddSourceA: Pointer;

function MsiSourceListAddSourceA;
begin
  GetProcedureAddress(_MsiSourceListAddSourceA, msilib, 'MsiSourceListAddSourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListAddSourceA]
  end;
end;
{$ELSE}
function MsiSourceListAddSourceA; external msilib name 'MsiSourceListAddSourceA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListAddSourceW: Pointer;

function MsiSourceListAddSourceW;
begin
  GetProcedureAddress(_MsiSourceListAddSourceW, msilib, 'MsiSourceListAddSourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListAddSourceW]
  end;
end;
{$ELSE}
function MsiSourceListAddSourceW; external msilib name 'MsiSourceListAddSourceW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListAddSource: Pointer;

function MsiSourceListAddSource;
begin
  GetProcedureAddress(_MsiSourceListAddSource, msilib, 'MsiSourceListAddSourceW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListAddSource]
  end;
end;
{$ELSE}
function MsiSourceListAddSource; external msilib name 'MsiSourceListAddSourceW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListAddSource: Pointer;

function MsiSourceListAddSource;
begin
  GetProcedureAddress(_MsiSourceListAddSource, msilib, 'MsiSourceListAddSourceA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListAddSource]
  end;
end;
{$ELSE}
function MsiSourceListAddSource; external msilib name 'MsiSourceListAddSourceA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListForceResolutionA: Pointer;

function MsiSourceListForceResolutionA;
begin
  GetProcedureAddress(_MsiSourceListForceResolutionA, msilib, 'MsiSourceListForceResolutionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListForceResolutionA]
  end;
end;
{$ELSE}
function MsiSourceListForceResolutionA; external msilib name 'MsiSourceListForceResolutionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListForceResolutionW: Pointer;

function MsiSourceListForceResolutionW;
begin
  GetProcedureAddress(_MsiSourceListForceResolutionW, msilib, 'MsiSourceListForceResolutionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListForceResolutionW]
  end;
end;
{$ELSE}
function MsiSourceListForceResolutionW; external msilib name 'MsiSourceListForceResolutionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListForceResolution: Pointer;

function MsiSourceListForceResolution;
begin
  GetProcedureAddress(_MsiSourceListForceResolution, msilib, 'MsiSourceListForceResolutionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListForceResolution]
  end;
end;
{$ELSE}
function MsiSourceListForceResolution; external msilib name 'MsiSourceListForceResolutionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiSourceListForceResolution: Pointer;

function MsiSourceListForceResolution;
begin
  GetProcedureAddress(_MsiSourceListForceResolution, msilib, 'MsiSourceListForceResolutionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiSourceListForceResolution]
  end;
end;
{$ELSE}
function MsiSourceListForceResolution; external msilib name 'MsiSourceListForceResolutionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$ENDIF WIN32_MSI_110}


{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileVersionA: Pointer;

function MsiGetFileVersionA;
begin
  GetProcedureAddress(_MsiGetFileVersionA, msilib, 'MsiGetFileVersionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileVersionA]
  end;
end;
{$ELSE}
function MsiGetFileVersionA; external msilib name 'MsiGetFileVersionA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileVersionW: Pointer;

function MsiGetFileVersionW;
begin
  GetProcedureAddress(_MsiGetFileVersionW, msilib, 'MsiGetFileVersionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileVersionW]
  end;
end;
{$ELSE}
function MsiGetFileVersionW; external msilib name 'MsiGetFileVersionW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileVersion: Pointer;

function MsiGetFileVersion;
begin
  GetProcedureAddress(_MsiGetFileVersion, msilib, 'MsiGetFileVersionW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileVersion]
  end;
end;
{$ELSE}
function MsiGetFileVersion; external msilib name 'MsiGetFileVersionW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileVersion: Pointer;

function MsiGetFileVersion;
begin
  GetProcedureAddress(_MsiGetFileVersion, msilib, 'MsiGetFileVersionA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileVersion]
  end;
end;
{$ELSE}
function MsiGetFileVersion; external msilib name 'MsiGetFileVersionA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileHashA: Pointer;

function MsiGetFileHashA;
begin
  GetProcedureAddress(_MsiGetFileHashA, msilib, 'MsiGetFileHashA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileHashA]
  end;
end;
{$ELSE}
function MsiGetFileHashA; external msilib name 'MsiGetFileHashA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileHashW: Pointer;

function MsiGetFileHashW;
begin
  GetProcedureAddress(_MsiGetFileHashW, msilib, 'MsiGetFileHashW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileHashW]
  end;
end;
{$ELSE}
function MsiGetFileHashW; external msilib name 'MsiGetFileHashW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileHash: Pointer;

function MsiGetFileHash;
begin
  GetProcedureAddress(_MsiGetFileHash, msilib, 'MsiGetFileHashW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileHash]
  end;
end;
{$ELSE}
function MsiGetFileHash; external msilib name 'MsiGetFileHashW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileHash: Pointer;

function MsiGetFileHash;
begin
  GetProcedureAddress(_MsiGetFileHash, msilib, 'MsiGetFileHashA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileHash]
  end;
end;
{$ELSE}
function MsiGetFileHash; external msilib name 'MsiGetFileHashA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileSignatureInformationA: Pointer;

function MsiGetFileSignatureInformationA;
begin
  GetProcedureAddress(_MsiGetFileSignatureInformationA, msilib, 'MsiGetFileSignatureInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileSignatureInformationA]
  end;
end;
{$ELSE}
function MsiGetFileSignatureInformationA; external msilib name 'MsiGetFileSignatureInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileSignatureInformationW: Pointer;

function MsiGetFileSignatureInformationW;
begin
  GetProcedureAddress(_MsiGetFileSignatureInformationW, msilib, 'MsiGetFileSignatureInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileSignatureInformationW]
  end;
end;
{$ELSE}
function MsiGetFileSignatureInformationW; external msilib name 'MsiGetFileSignatureInformationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileSignatureInformation: Pointer;

function MsiGetFileSignatureInformation;
begin
  GetProcedureAddress(_MsiGetFileSignatureInformation, msilib, 'MsiGetFileSignatureInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileSignatureInformation]
  end;
end;
{$ELSE}
function MsiGetFileSignatureInformation; external msilib name 'MsiGetFileSignatureInformationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetFileSignatureInformation: Pointer;

function MsiGetFileSignatureInformation;
begin
  GetProcedureAddress(_MsiGetFileSignatureInformation, msilib, 'MsiGetFileSignatureInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetFileSignatureInformation]
  end;
end;
{$ELSE}
function MsiGetFileSignatureInformation; external msilib name 'MsiGetFileSignatureInformationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF WIN32_MSI_110}


{$IFDEF DYNAMIC_LINK}
var
  _MsiGetShortcutTargetA: Pointer;

function MsiGetShortcutTargetA;
begin
  GetProcedureAddress(_MsiGetShortcutTargetA, msilib, 'MsiGetShortcutTargetA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetShortcutTargetA]
  end;
end;
{$ELSE}
function MsiGetShortcutTargetA; external msilib name 'MsiGetShortcutTargetA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetShortcutTargetW: Pointer;

function MsiGetShortcutTargetW;
begin
  GetProcedureAddress(_MsiGetShortcutTargetW, msilib, 'MsiGetShortcutTargetW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetShortcutTargetW]
  end;
end;
{$ELSE}
function MsiGetShortcutTargetW; external msilib name 'MsiGetShortcutTargetW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetShortcutTarget: Pointer;

function MsiGetShortcutTarget;
begin
  GetProcedureAddress(_MsiGetShortcutTarget, msilib, 'MsiGetShortcutTargetW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetShortcutTarget]
  end;
end;
{$ELSE}
function MsiGetShortcutTarget; external msilib name 'MsiGetShortcutTargetW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiGetShortcutTarget: Pointer;

function MsiGetShortcutTarget;
begin
  GetProcedureAddress(_MsiGetShortcutTarget, msilib, 'MsiGetShortcutTargetA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiGetShortcutTarget]
  end;
end;
{$ELSE}
function MsiGetShortcutTarget; external msilib name 'MsiGetShortcutTargetA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _MsiIsProductElevatedA: Pointer;

function MsiIsProductElevatedA;
begin
  GetProcedureAddress(_MsiIsProductElevatedA, msilib, 'MsiIsProductElevatedA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiIsProductElevatedA]
  end;
end;
{$ELSE}
function MsiIsProductElevatedA; external msilib name 'MsiIsProductElevatedA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsiIsProductElevatedW: Pointer;

function MsiIsProductElevatedW;
begin
  GetProcedureAddress(_MsiIsProductElevatedW, msilib, 'MsiIsProductElevatedW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiIsProductElevatedW]
  end;
end;
{$ELSE}
function MsiIsProductElevatedW; external msilib name 'MsiIsProductElevatedW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiIsProductElevated: Pointer;

function MsiIsProductElevated;
begin
  GetProcedureAddress(_MsiIsProductElevated, msilib, 'MsiIsProductElevatedW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiIsProductElevated]
  end;
end;
{$ELSE}
function MsiIsProductElevated; external msilib name 'MsiIsProductElevatedW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MsiIsProductElevated: Pointer;

function MsiIsProductElevated;
begin
  GetProcedureAddress(_MsiIsProductElevated, msilib, 'MsiIsProductElevatedA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsiIsProductElevated]
  end;
end;
{$ELSE}
function MsiIsProductElevated; external msilib name 'MsiIsProductElevatedA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


end.
