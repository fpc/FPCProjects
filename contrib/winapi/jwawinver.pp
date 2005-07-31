{******************************************************************************}
{                                                       	               }
{ Windows Version API interface Unit for Object Pascal                         }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winver.h, released June 2000. The original Pascal      }
{ code is: WinVer.pas, released December 2000. The initial developer of the    }
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

unit JwaWinVer;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinVer.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinUser, JwaWinType; 

const

//  RT_VERSION = MAKEINTRESOURCE(16);

// ----- Symbols -----

  VS_FILE_INFO    = RT_VERSION;
  VS_VERSION_INFO = 1;
  VS_USER_DEFINED = 100;

// ----- VS_VERSION.dwFileFlags -----

  VS_FFI_SIGNATURE     = $FEEF04BD;
  VS_FFI_STRUCVERSION  = $00010000;
  VS_FFI_FILEFLAGSMASK = $0000003F;

// ----- VS_VERSION.dwFileFlags -----

  VS_FF_DEBUG        = $00000001;
  VS_FF_PRERELEASE   = $00000002;
  VS_FF_PATCHED      = $00000004;
  VS_FF_PRIVATEBUILD = $00000008;
  VS_FF_INFOINFERRED = $00000010;
  VS_FF_SPECIALBUILD = $00000020;

// ----- VS_VERSION.dwFileOS -----

  VOS_UNKNOWN = $00000000;
  VOS_DOS     = $00010000;
  VOS_OS216   = $00020000;
  VOS_OS232   = $00030000;
  VOS_NT      = $00040000;
  VOS_WINCE   = $00050000;

  VOS__BASE      = $00000000;
  VOS__WINDOWS16 = $00000001;
  VOS__PM16      = $00000002;
  VOS__PM32      = $00000003;
  VOS__WINDOWS32 = $00000004;

  VOS_DOS_WINDOWS16 = $00010001;
  VOS_DOS_WINDOWS32 = $00010004;
  VOS_OS216_PM16    = $00020002;
  VOS_OS232_PM32    = $00030003;
  VOS_NT_WINDOWS32  = $00040004;

// ----- VS_VERSION.dwFileType -----

  VFT_UNKNOWN    = $00000000;
  VFT_APP        = $00000001;
  VFT_DLL        = $00000002;
  VFT_DRV        = $00000003;
  VFT_FONT       = $00000004;
  VFT_VXD        = $00000005;
  VFT_STATIC_LIB = $00000007;

// ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_DRV -----

  VFT2_UNKNOWN         = $00000000;
  VFT2_DRV_PRINTER     = $00000001;
  VFT2_DRV_KEYBOARD    = $00000002;
  VFT2_DRV_LANGUAGE    = $00000003;
  VFT2_DRV_DISPLAY     = $00000004;
  VFT2_DRV_MOUSE       = $00000005;
  VFT2_DRV_NETWORK     = $00000006;
  VFT2_DRV_SYSTEM      = $00000007;
  VFT2_DRV_INSTALLABLE = $00000008;
  VFT2_DRV_SOUND       = $00000009;
  VFT2_DRV_COMM        = $0000000A;
  VFT2_DRV_INPUTMETHOD = $0000000B;
  VFT2_DRV_VERSIONED_PRINTER = $0000000C;

// ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_FONT -----

  VFT2_FONT_RASTER   = $00000001;
  VFT2_FONT_VECTOR   = $00000002;
  VFT2_FONT_TRUETYPE = $00000003;

// ----- VerFindFile() flags -----

  VFFF_ISSHAREDFILE = $0001;

  VFF_CURNEDEST     = $0001;
  VFF_FILEINUSE     = $0002;
  VFF_BUFFTOOSMALL  = $0004;

// ----- VerInstallFile() flags -----

  VIFF_FORCEINSTALL  = $0001;
  VIFF_DONTDELETEOLD = $0002;

  VIF_TEMPFILE = $00000001;
  VIF_MISMATCH = $00000002;
  VIF_SRCOLD   = $00000004;

  VIF_DIFFLANG   = $00000008;
  VIF_DIFFCODEPG = $00000010;
  VIF_DIFFTYPE   = $00000020;

  VIF_WRITEPROT        = $00000040;
  VIF_FILEINUSE        = $00000080;
  VIF_OUTOFSPACE       = $00000100;
  VIF_ACCESSVIOLATION  = $00000200;
  VIF_SHARINGVIOLATION = $00000400;
  VIF_CANNOTCREATE     = $00000800;
  VIF_CANNOTDELETE     = $00001000;
  VIF_CANNOTRENAME     = $00002000;
  VIF_CANNOTDELETECUR  = $00004000;
  VIF_OUTOFMEMORY      = $00008000;

  VIF_CANNOTREADSRC = $00010000;
  VIF_CANNOTREADDST = $00020000;

  VIF_BUFFTOOSMALL      = $00040000;
  VIF_CANNOTLOADLZ32    = $00080000;
  VIF_CANNOTLOADCABINET = $00100000;

// ----- Types and structures -----

type
{$IFDEF USEDELPHI5}
  PVSFixedFileInfo = Windows.PVSFixedFileInfo;
  tagVS_FIXEDFILEINFO = Windows.tagVS_FIXEDFILEINFO;
  VS_FIXEDFILEINFO = Windows.VS_FIXEDFILEINFO;
  TVSFixedFileInfo = Windows.TVSFixedFileInfo;
{$ELSE}
  PVSFixedFileInfo = ^VS_FIXEDFILEINFO;
  tagVS_FIXEDFILEINFO = record
    dwSignature: DWORD;        // e.g. 0xfeef04bd
    dwStrucVersion: DWORD;     // e.g. 0x00000042 = "0.42"
    dwFileVersionMS: DWORD;    // e.g. 0x00030075 = "3.75"
    dwFileVersionLS: DWORD;    // e.g. 0x00000031 = "0.31"
    dwProductVersionMS: DWORD; // e.g. 0x00030010 = "3.10"
    dwProductVersionLS: DWORD; // e.g. 0x00000031 = "0.31"
    dwFileFlagsMask: DWORD;    // = 0x3F for version "0.42"
    dwFileFlags: DWORD;        // e.g. VFF_DEBUG | VFF_PRERELEASE
    dwFileOS: DWORD;           // e.g. VOS_DOS_WINDOWS16
    dwFileType: DWORD;         // e.g. VFT_DRIVER
    dwFileSubtype: DWORD;      // e.g. VFT2_DRV_KEYBOARD
    dwFileDateMS: DWORD;       // e.g. 0
    dwFileDateLS: DWORD;       // e.g. 0
  end;
  VS_FIXEDFILEINFO = tagVS_FIXEDFILEINFO;
  TVSFixedFileInfo = VS_FIXEDFILEINFO;
{$ENDIF}

// ----- Function prototypes -----

function VerFindFileA(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPSTR; var lpuCurDirLen: UINT; szDestDir: LPSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;
function VerFindFileW(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPWSTR; var lpuCurDirLen: UINT; szDestDir: LPWSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;

{$IFDEF UNICODE}
function VerFindFile(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPWSTR; var lpuCurDirLen: UINT; szDestDir: LPWSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;
{$ELSE}
function VerFindFile(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPSTR; var lpuCurDirLen: UINT; szDestDir: LPSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;
{$ENDIF}

function VerInstallFileA(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
function VerInstallFileW(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPWSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;

{$IFDEF UNICODE}
function VerInstallFile(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPWSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
{$ELSE}
function VerInstallFile(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
{$ENDIF}

// Returns size of version info in bytes

function GetFileVersionInfoSizeA(lptstrFilename: LPCSTR; var lpdwHandle: DWORD): DWORD; stdcall;
function GetFileVersionInfoSizeW(lptstrFilename: LPCWSTR; var lpdwHandle: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetFileVersionInfoSize(lptstrFilename: LPCWSTR; var lpdwHandle: DWORD): DWORD; stdcall;
{$ELSE}
function GetFileVersionInfoSize(lptstrFilename: LPCSTR; var lpdwHandle: DWORD): DWORD; stdcall;
{$ENDIF}

// Read version info into buffer

function GetFileVersionInfoA(lptstrFilename: LPCSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;
function GetFileVersionInfoW(lptstrFilename: LPCWSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;

{$IFDEF UNICODE}
function GetFileVersionInfo(lptstrFilename: LPCWSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;
{$ELSE}
function GetFileVersionInfo(lptstrFilename: LPCSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;
{$ENDIF}

function VerLanguageNameA(wLang: DWORD; szLang: LPSTR; nSize: DWORD): DWORD; stdcall;
function VerLanguageNameW(wLang: DWORD; szLang: LPWSTR; nSize: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function VerLanguageName(wLang: DWORD; szLang: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$ELSE}
function VerLanguageName(wLang: DWORD; szLang: LPSTR; nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function VerQueryValueA(pBlock: LPVOID; lpSubBlock: LPSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;
function VerQueryValueW(pBlock: LPVOID; lpSubBlock: LPWSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function VerQueryValue(pBlock: LPVOID; lpSubBlock: LPWSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;
{$ELSE}
function VerQueryValue(pBlock: LPVOID; lpSubBlock: LPSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;
{$ENDIF}

implementation

const
  Version = 'version.dll';


{$IFDEF DYNAMIC_LINK}
var
  _VerFindFileA: Pointer;

function VerFindFileA;
begin
  GetProcedureAddress(_VerFindFileA, Version, 'VerFindFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerFindFileA]
  end;
end;
{$ELSE}
function VerFindFileA; external Version name 'VerFindFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VerFindFileW: Pointer;

function VerFindFileW;
begin
  GetProcedureAddress(_VerFindFileW, Version, 'VerFindFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerFindFileW]
  end;
end;
{$ELSE}
function VerFindFileW; external Version name 'VerFindFileW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VerFindFile: Pointer;

function VerFindFile;
begin
  GetProcedureAddress(_VerFindFile, Version, 'VerFindFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerFindFile]
  end;
end;
{$ELSE}
function VerFindFile; external Version name 'VerFindFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VerFindFile: Pointer;

function VerFindFile;
begin
  GetProcedureAddress(_VerFindFile, Version, 'VerFindFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerFindFile]
  end;
end;
{$ELSE}
function VerFindFile; external Version name 'VerFindFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _VerInstallFileA: Pointer;

function VerInstallFileA;
begin
  GetProcedureAddress(_VerInstallFileA, Version, 'VerInstallFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerInstallFileA]
  end;
end;
{$ELSE}
function VerInstallFileA; external Version name 'VerInstallFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VerInstallFileW: Pointer;

function VerInstallFileW;
begin
  GetProcedureAddress(_VerInstallFileW, Version, 'VerInstallFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerInstallFileW]
  end;
end;
{$ELSE}
function VerInstallFileW; external Version name 'VerInstallFileW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VerInstallFile: Pointer;

function VerInstallFile;
begin
  GetProcedureAddress(_VerInstallFile, Version, 'VerInstallFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerInstallFile]
  end;
end;
{$ELSE}
function VerInstallFile; external Version name 'VerInstallFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VerInstallFile: Pointer;

function VerInstallFile;
begin
  GetProcedureAddress(_VerInstallFile, Version, 'VerInstallFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerInstallFile]
  end;
end;
{$ELSE}
function VerInstallFile; external Version name 'VerInstallFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfoSizeA: Pointer;

function GetFileVersionInfoSizeA;
begin
  GetProcedureAddress(_GetFileVersionInfoSizeA, Version, 'GetFileVersionInfoSizeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfoSizeA]
  end;
end;
{$ELSE}
function GetFileVersionInfoSizeA; external Version name 'GetFileVersionInfoSizeA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfoSizeW: Pointer;

function GetFileVersionInfoSizeW;
begin
  GetProcedureAddress(_GetFileVersionInfoSizeW, Version, 'GetFileVersionInfoSizeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfoSizeW]
  end;
end;
{$ELSE}
function GetFileVersionInfoSizeW; external Version name 'GetFileVersionInfoSizeW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfoSize: Pointer;

function GetFileVersionInfoSize;
begin
  GetProcedureAddress(_GetFileVersionInfoSize, Version, 'GetFileVersionInfoSizeW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfoSize]
  end;
end;
{$ELSE}
function GetFileVersionInfoSize; external Version name 'GetFileVersionInfoSizeW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfoSize: Pointer;

function GetFileVersionInfoSize;
begin
  GetProcedureAddress(_GetFileVersionInfoSize, Version, 'GetFileVersionInfoSizeA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfoSize]
  end;
end;
{$ELSE}
function GetFileVersionInfoSize; external Version name 'GetFileVersionInfoSizeA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfoA: Pointer;

function GetFileVersionInfoA;
begin
  GetProcedureAddress(_GetFileVersionInfoA, Version, 'GetFileVersionInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfoA]
  end;
end;
{$ELSE}
function GetFileVersionInfoA; external Version name 'GetFileVersionInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfoW: Pointer;

function GetFileVersionInfoW;
begin
  GetProcedureAddress(_GetFileVersionInfoW, Version, 'GetFileVersionInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfoW]
  end;
end;
{$ELSE}
function GetFileVersionInfoW; external Version name 'GetFileVersionInfoW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfo: Pointer;

function GetFileVersionInfo;
begin
  GetProcedureAddress(_GetFileVersionInfo, Version, 'GetFileVersionInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfo]
  end;
end;
{$ELSE}
function GetFileVersionInfo; external Version name 'GetFileVersionInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetFileVersionInfo: Pointer;

function GetFileVersionInfo;
begin
  GetProcedureAddress(_GetFileVersionInfo, Version, 'GetFileVersionInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFileVersionInfo]
  end;
end;
{$ELSE}
function GetFileVersionInfo; external Version name 'GetFileVersionInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _VerLanguageNameA: Pointer;

function VerLanguageNameA;
begin
  GetProcedureAddress(_VerLanguageNameA, Version, 'VerLanguageNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerLanguageNameA]
  end;
end;
{$ELSE}
function VerLanguageNameA; external Version name 'VerLanguageNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VerLanguageNameW: Pointer;

function VerLanguageNameW;
begin
  GetProcedureAddress(_VerLanguageNameW, Version, 'VerLanguageNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerLanguageNameW]
  end;
end;
{$ELSE}
function VerLanguageNameW; external Version name 'VerLanguageNameW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VerLanguageName: Pointer;

function VerLanguageName;
begin
  GetProcedureAddress(_VerLanguageName, Version, 'VerLanguageNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerLanguageName]
  end;
end;
{$ELSE}
function VerLanguageName; external Version name 'VerLanguageNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VerLanguageName: Pointer;

function VerLanguageName;
begin
  GetProcedureAddress(_VerLanguageName, Version, 'VerLanguageNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerLanguageName]
  end;
end;
{$ELSE}
function VerLanguageName; external Version name 'VerLanguageNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _VerQueryValueA: Pointer;

function VerQueryValueA;
begin
  GetProcedureAddress(_VerQueryValueA, Version, 'VerQueryValueA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerQueryValueA]
  end;
end;
{$ELSE}
function VerQueryValueA; external Version name 'VerQueryValueA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VerQueryValueW: Pointer;

function VerQueryValueW;
begin
  GetProcedureAddress(_VerQueryValueW, Version, 'VerQueryValueW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerQueryValueW]
  end;
end;
{$ELSE}
function VerQueryValueW; external Version name 'VerQueryValueW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VerQueryValue: Pointer;

function VerQueryValue;
begin
  GetProcedureAddress(_VerQueryValue, Version, 'VerQueryValueW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerQueryValue]
  end;
end;
{$ELSE}
function VerQueryValue; external Version name 'VerQueryValueW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VerQueryValue: Pointer;

function VerQueryValue;
begin
  GetProcedureAddress(_VerQueryValue, Version, 'VerQueryValueA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VerQueryValue]
  end;
end;
{$ELSE}
function VerQueryValue; external Version name 'VerQueryValueA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

end.
