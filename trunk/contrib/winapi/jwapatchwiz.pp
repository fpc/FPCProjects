{******************************************************************************}
{                                                       	               }
{ MSI Patch Wizard API interface Unit for Object Pascal                        }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: patchwiz.h, released August 2001. The original Pascal  }
{ code is: PatchWiz.pas, released December 2001. The initial developer of the  }
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

unit JwaPatchWiz;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "PatchWiz.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

  (* PATCHWIZ.H - public header file for PATCHWIZ.DLL *)

(*
**	UINT WINAPI UiCreatePatchPackage ( LPTSTR szPcpPath,
**		LPTSTR szPatchPath, LPTSTR szLogPath, HWND hwndStatus,
**		LPTSTR szTempFolder, BOOL fRemoveTempFolderIfPresent );
**
**	Arguments:
**	  szPcpPath - full absolute path to Windows Installer database
**		(PCP file) that contains appropriate tables of input-data for
**		Patch creation process such as Properties and TargetImages.
**	  szPatchPath - optional, full absolute path to Patching Package
**		file (MSP file) to create and stuff with output.  If this
**		NULL or an empty string, the api will try to use
**		Properties.Value where Properties.Name = PatchOutputPath
**		from the PCP file.
**	  szLogPath - optional, full absolute path to text log file to
**		append to.  Caller should truncate file if wanted.
**	  hwndStatus - optional, window handle to display status text.
**		More details to come later.
**	  szTempFolder - optional location to use for temp files.
**		Default is %TEMP%\~pcw_tmp.tmp\.
**	  fRemoveTempFolderIfPresent - remove temp folder (and all its
**		contents) if present.  If FALSE and folder is present, api
**		will fail.
**		
**	Return Values: ERROR_SUCCESS, plus ERROR_PCW_* that follow.
*)

const
  ERROR_PCW_BASE = DWORD($C00E5101);

  ERROR_PCW_PCP_DOESNT_EXIST              = (ERROR_PCW_BASE + $00);
  ERROR_PCW_PCP_BAD_FORMAT                = (ERROR_PCW_BASE + $01);
  ERROR_PCW_CANT_CREATE_TEMP_FOLDER       = (ERROR_PCW_BASE + $02);
  ERROR_PCW_MISSING_PATCH_PATH            = (ERROR_PCW_BASE + $03);
  ERROR_PCW_CANT_OVERWRITE_PATCH          = (ERROR_PCW_BASE + $04);
  ERROR_PCW_CANT_CREATE_PATCH_FILE        = (ERROR_PCW_BASE + $05);
  ERROR_PCW_MISSING_PATCH_GUID            = (ERROR_PCW_BASE + $06);
  ERROR_PCW_BAD_PATCH_GUID                = (ERROR_PCW_BASE + $07);
  ERROR_PCW_BAD_GUIDS_TO_REPLACE          = (ERROR_PCW_BASE + $08);
  ERROR_PCW_BAD_TARGET_PRODUCT_CODE_LIST  = (ERROR_PCW_BASE + $09);
  ERROR_PCW_NO_UPGRADED_IMAGES_TO_PATCH   = (ERROR_PCW_BASE + $0a);
  //#define ERROR_PCW_BAD_API_PATCHING_OPTION_FLAGS  (ERROR_PCW_BASE + 0x0b) -- obsolete
  ERROR_PCW_BAD_API_PATCHING_SYMBOL_FLAGS = (ERROR_PCW_BASE + $0c);
  ERROR_PCW_OODS_COPYING_MSI              = (ERROR_PCW_BASE + $0d);
  ERROR_PCW_UPGRADED_IMAGE_NAME_TOO_LONG  = (ERROR_PCW_BASE + $0e);
  ERROR_PCW_BAD_UPGRADED_IMAGE_NAME       = (ERROR_PCW_BASE + $0f);

  ERROR_PCW_DUP_UPGRADED_IMAGE_NAME       = (ERROR_PCW_BASE + $10);
  ERROR_PCW_UPGRADED_IMAGE_PATH_TOO_LONG  = (ERROR_PCW_BASE + $11);
  ERROR_PCW_UPGRADED_IMAGE_PATH_EMPTY     = (ERROR_PCW_BASE + $12);
  ERROR_PCW_UPGRADED_IMAGE_PATH_NOT_EXIST = (ERROR_PCW_BASE + $13);
  ERROR_PCW_UPGRADED_IMAGE_PATH_NOT_MSI   = (ERROR_PCW_BASE + $14);
  ERROR_PCW_UPGRADED_IMAGE_COMPRESSED     = (ERROR_PCW_BASE + $15);
  ERROR_PCW_TARGET_IMAGE_NAME_TOO_LONG    = (ERROR_PCW_BASE + $16);
  ERROR_PCW_BAD_TARGET_IMAGE_NAME         = (ERROR_PCW_BASE + $17);
  ERROR_PCW_DUP_TARGET_IMAGE_NAME         = (ERROR_PCW_BASE + $18);
  ERROR_PCW_TARGET_IMAGE_PATH_TOO_LONG    = (ERROR_PCW_BASE + $19);
  ERROR_PCW_TARGET_IMAGE_PATH_EMPTY       = (ERROR_PCW_BASE + $1a);
  ERROR_PCW_TARGET_IMAGE_PATH_NOT_EXIST   = (ERROR_PCW_BASE + $1b);
  ERROR_PCW_TARGET_IMAGE_PATH_NOT_MSI     = (ERROR_PCW_BASE + $1c);
  ERROR_PCW_TARGET_IMAGE_COMPRESSED       = (ERROR_PCW_BASE + $1d);
  ERROR_PCW_TARGET_BAD_PROD_VALIDATE      = (ERROR_PCW_BASE + $1e);
  ERROR_PCW_TARGET_BAD_PROD_CODE_VAL      = (ERROR_PCW_BASE + $1f);

  ERROR_PCW_UPGRADED_MISSING_SRC_FILES       = (ERROR_PCW_BASE + $20);
  ERROR_PCW_TARGET_MISSING_SRC_FILES         = (ERROR_PCW_BASE + $21);
  ERROR_PCW_IMAGE_FAMILY_NAME_TOO_LONG       = (ERROR_PCW_BASE + $22);
  ERROR_PCW_BAD_IMAGE_FAMILY_NAME            = (ERROR_PCW_BASE + $23);
  ERROR_PCW_DUP_IMAGE_FAMILY_NAME            = (ERROR_PCW_BASE + $24);
  ERROR_PCW_BAD_IMAGE_FAMILY_SRC_PROP        = (ERROR_PCW_BASE + $25);
  ERROR_PCW_UFILEDATA_LONG_FILE_TABLE_KEY    = (ERROR_PCW_BASE + $26);
  ERROR_PCW_UFILEDATA_BLANK_FILE_TABLE_KEY   = (ERROR_PCW_BASE + $27);
  ERROR_PCW_UFILEDATA_MISSING_FILE_TABLE_KEY = (ERROR_PCW_BASE + $28);
  ERROR_PCW_EXTFILE_LONG_FILE_TABLE_KEY      = (ERROR_PCW_BASE + $29);
  ERROR_PCW_EXTFILE_BLANK_FILE_TABLE_KEY     = (ERROR_PCW_BASE + $2a);
  ERROR_PCW_EXTFILE_BAD_FAMILY_FIELD         = (ERROR_PCW_BASE + $2b);
  ERROR_PCW_EXTFILE_LONG_PATH_TO_FILE        = (ERROR_PCW_BASE + $2c);
  ERROR_PCW_EXTFILE_BLANK_PATH_TO_FILE       = (ERROR_PCW_BASE + $2d);
  ERROR_PCW_EXTFILE_MISSING_FILE             = (ERROR_PCW_BASE + $2e);
//#define ERROR_PCW_FILERANGE_LONG_FILE_TABLE_KEY       (ERROR_PCW_BASE + 0x2f) -- obsolete

//#define ERROR_PCW_FILERANGE_BLANK_FILE_TABLE_KEY      (ERROR_PCW_BASE + 0x30) -- obsolete
//#define ERROR_PCW_FILERANGE_MISSING_FILE_TABLE_KEY    (ERROR_PCW_BASE + 0x31) -- obsolete
//#define ERROR_PCW_FILERANGE_LONG_PATH_TO_FILE         (ERROR_PCW_BASE + 0x32) -- obsolete
//#define ERROR_PCW_FILERANGE_MISSING_FILE              (ERROR_PCW_BASE + 0x33) -- obsolete
//#define ERROR_PCW_FILERANGE_INVALID_OFFSET            (ERROR_PCW_BASE + 0x34) -- obsolete
//#define ERROR_PCW_FILERANGE_INVALID_SIZE              (ERROR_PCW_BASE + 0x35) -- obsolete
//#define ERROR_PCW_FILERANGE_INVALID_RETAIN            (ERROR_PCW_BASE + 0x36) -- obsolete
//#define ERROR_PCW_BAD_MEDIA_SRC_PROP_NAME             (ERROR_PCW_BASE + 0x37) -- obsolete
//#define ERROR_PCW_BAD_MEDIA_DISK_ID                   (ERROR_PCW_BASE + 0x38) -- obsolete
  ERROR_PCW_BAD_FILE_SEQUENCE_START       = (ERROR_PCW_BASE + $39);
  ERROR_PCW_CANT_COPY_FILE_TO_TEMP_FOLDER = (ERROR_PCW_BASE + $3a);
  ERROR_PCW_CANT_CREATE_ONE_PATCH_FILE    = (ERROR_PCW_BASE + $3b);
  ERROR_PCW_BAD_IMAGE_FAMILY_DISKID       = (ERROR_PCW_BASE + $3c);
  ERROR_PCW_BAD_IMAGE_FAMILY_FILESEQSTART = (ERROR_PCW_BASE + $3d);
  ERROR_PCW_BAD_UPGRADED_IMAGE_FAMILY     = (ERROR_PCW_BASE + $3e);
  ERROR_PCW_BAD_TARGET_IMAGE_UPGRADED     = (ERROR_PCW_BASE + $3f);

  ERROR_PCW_DUP_TARGET_IMAGE_PACKCODE    = (ERROR_PCW_BASE + $40);
  ERROR_PCW_UFILEDATA_BAD_UPGRADED_FIELD = (ERROR_PCW_BASE + $41);
  ERROR_PCW_MISMATCHED_PRODUCT_CODES     = (ERROR_PCW_BASE + $42);
  ERROR_PCW_MISMATCHED_PRODUCT_VERSIONS  = (ERROR_PCW_BASE + $43);
  ERROR_PCW_CANNOT_WRITE_DDF             = (ERROR_PCW_BASE + $44);
  ERROR_PCW_CANNOT_RUN_MAKECAB           = (ERROR_PCW_BASE + $45);
//#define ERROR_PCW_CANNOT_CREATE_STORAGE               (ERROR_PCW_BASE + 0x46) -- obsolete
//#define ERROR_PCW_CANNOT_CREATE_STREAM                (ERROR_PCW_BASE + 0x47) -- obsolete
//#define ERROR_PCW_CANNOT_WRITE_STREAM                 (ERROR_PCW_BASE + 0x48) -- obsolete
//#define ERROR_PCW_CANNOT_READ_CABINET                 (ERROR_PCW_BASE + 0x49) -- obsolete
  ERROR_PCW_WRITE_SUMMARY_PROPERTIES           = (ERROR_PCW_BASE + $4a);
  ERROR_PCW_TFILEDATA_LONG_FILE_TABLE_KEY      = (ERROR_PCW_BASE + $4b);
  ERROR_PCW_TFILEDATA_BLANK_FILE_TABLE_KEY     = (ERROR_PCW_BASE + $4c);
  ERROR_PCW_TFILEDATA_MISSING_FILE_TABLE_KEY   = (ERROR_PCW_BASE + $4d);
  ERROR_PCW_TFILEDATA_BAD_TARGET_FIELD         = (ERROR_PCW_BASE + $4e);
  ERROR_PCW_UPGRADED_IMAGE_PATCH_PATH_TOO_LONG = (ERROR_PCW_BASE + $4f);

  ERROR_PCW_UPGRADED_IMAGE_PATCH_PATH_NOT_EXIST = (ERROR_PCW_BASE + $50);
  ERROR_PCW_UPGRADED_IMAGE_PATCH_PATH_NOT_MSI   = (ERROR_PCW_BASE + $51);
  ERROR_PCW_DUP_UPGRADED_IMAGE_PACKCODE         = (ERROR_PCW_BASE + $52);
  ERROR_PCW_UFILEIGNORE_BAD_UPGRADED_FIELD      = (ERROR_PCW_BASE + $53);
  ERROR_PCW_UFILEIGNORE_LONG_FILE_TABLE_KEY     = (ERROR_PCW_BASE + $54);
  ERROR_PCW_UFILEIGNORE_BLANK_FILE_TABLE_KEY    = (ERROR_PCW_BASE + $55);
  ERROR_PCW_UFILEIGNORE_BAD_FILE_TABLE_KEY      = (ERROR_PCW_BASE + $56);
  ERROR_PCW_FAMILY_RANGE_NAME_TOO_LONG          = (ERROR_PCW_BASE + $57);
  ERROR_PCW_BAD_FAMILY_RANGE_NAME               = (ERROR_PCW_BASE + $58);
  ERROR_PCW_FAMILY_RANGE_LONG_FILE_TABLE_KEY    = (ERROR_PCW_BASE + $59);
  ERROR_PCW_FAMILY_RANGE_BLANK_FILE_TABLE_KEY   = (ERROR_PCW_BASE + $5a);
  ERROR_PCW_FAMILY_RANGE_LONG_RETAIN_OFFSETS    = (ERROR_PCW_BASE + $5b);
  ERROR_PCW_FAMILY_RANGE_BLANK_RETAIN_OFFSETS   = (ERROR_PCW_BASE + $5c);
  ERROR_PCW_FAMILY_RANGE_BAD_RETAIN_OFFSETS     = (ERROR_PCW_BASE + $5d);
  ERROR_PCW_FAMILY_RANGE_LONG_RETAIN_LENGTHS    = (ERROR_PCW_BASE + $5e);
  ERROR_PCW_FAMILY_RANGE_BLANK_RETAIN_LENGTHS   = (ERROR_PCW_BASE + $5f);

  ERROR_PCW_FAMILY_RANGE_BAD_RETAIN_LENGTHS = (ERROR_PCW_BASE + $60);
  ERROR_PCW_FAMILY_RANGE_COUNT_MISMATCH     = (ERROR_PCW_BASE + $61);
  ERROR_PCW_EXTFILE_LONG_IGNORE_OFFSETS     = (ERROR_PCW_BASE + $62);
  ERROR_PCW_EXTFILE_BAD_IGNORE_OFFSETS      = (ERROR_PCW_BASE + $63);
  ERROR_PCW_EXTFILE_LONG_IGNORE_LENGTHS     = (ERROR_PCW_BASE + $64);
  ERROR_PCW_EXTFILE_BAD_IGNORE_LENGTHS      = (ERROR_PCW_BASE + $65);
  ERROR_PCW_EXTFILE_IGNORE_COUNT_MISMATCH   = (ERROR_PCW_BASE + $66);
  ERROR_PCW_EXTFILE_LONG_RETAIN_OFFSETS     = (ERROR_PCW_BASE + $67);
  ERROR_PCW_EXTFILE_BAD_RETAIN_OFFSETS      = (ERROR_PCW_BASE + $68);
//#define ERROR_PCW_EXTFILE_RETAIN_COUNT_MISMATCH       (ERROR_PCW_BASE + 0x69) -- obsolete
  ERROR_PCW_TFILEDATA_LONG_IGNORE_OFFSETS   = (ERROR_PCW_BASE + $6a);
  ERROR_PCW_TFILEDATA_BAD_IGNORE_OFFSETS    = (ERROR_PCW_BASE + $6b);
  ERROR_PCW_TFILEDATA_LONG_IGNORE_LENGTHS   = (ERROR_PCW_BASE + $6c);
  ERROR_PCW_TFILEDATA_BAD_IGNORE_LENGTHS    = (ERROR_PCW_BASE + $6d);
  ERROR_PCW_TFILEDATA_IGNORE_COUNT_MISMATCH = (ERROR_PCW_BASE + $6e);
  ERROR_PCW_TFILEDATA_LONG_RETAIN_OFFSETS   = (ERROR_PCW_BASE + $6f);

  ERROR_PCW_TFILEDATA_BAD_RETAIN_OFFSETS = (ERROR_PCW_BASE + $70);
//#define ERROR_PCW_TFILEDATA_RETAIN_COUNT_MISMATCH     (ERROR_PCW_BASE + 0x71) -- obsolete
  ERROR_PCW_CANT_GENERATE_TRANSFORM            = (ERROR_PCW_BASE + $72);
  ERROR_PCW_CANT_CREATE_SUMMARY_INFO           = (ERROR_PCW_BASE + $73);
  ERROR_PCW_CANT_GENERATE_TRANSFORM_POUND      = (ERROR_PCW_BASE + $74);
  ERROR_PCW_CANT_CREATE_SUMMARY_INFO_POUND     = (ERROR_PCW_BASE + $75);
  ERROR_PCW_BAD_UPGRADED_IMAGE_PRODUCT_CODE    = (ERROR_PCW_BASE + $76);
  ERROR_PCW_BAD_UPGRADED_IMAGE_PRODUCT_VERSION = (ERROR_PCW_BASE + $77);
  ERROR_PCW_BAD_UPGRADED_IMAGE_UPGRADE_CODE    = (ERROR_PCW_BASE + $78);
  ERROR_PCW_BAD_TARGET_IMAGE_PRODUCT_CODE      = (ERROR_PCW_BASE + $79);
  ERROR_PCW_BAD_TARGET_IMAGE_PRODUCT_VERSION   = (ERROR_PCW_BASE + $7a);
  ERROR_PCW_BAD_TARGET_IMAGE_UPGRADE_CODE      = (ERROR_PCW_BASE + $7b);
  ERROR_PCW_MATCHED_PRODUCT_VERSIONS           = (ERROR_PCW_BASE + $7c);
  ERROR_PCW_NEXTxd                             = (ERROR_PCW_BASE + $7d);
  ERROR_PCW_NEXTxe                             = (ERROR_PCW_BASE + $7e);
  ERROR_PCW_NEXTxf                             = (ERROR_PCW_BASE + $7f);

(*
#define ERROR_PCW_NEXTx0           (ERROR_PCW_BASE + 0x80)
#define ERROR_PCW_NEXTx1           (ERROR_PCW_BASE + 0x81)
#define ERROR_PCW_NEXTx2           (ERROR_PCW_BASE + 0x82)
#define ERROR_PCW_NEXTx3           (ERROR_PCW_BASE + 0x83)
#define ERROR_PCW_NEXTx4           (ERROR_PCW_BASE + 0x84)
#define ERROR_PCW_NEXTx5           (ERROR_PCW_BASE + 0x85)
#define ERROR_PCW_NEXTx6           (ERROR_PCW_BASE + 0x86)
#define ERROR_PCW_NEXTx7           (ERROR_PCW_BASE + 0x87)
#define ERROR_PCW_NEXTx8           (ERROR_PCW_BASE + 0x88)
#define ERROR_PCW_NEXTx9           (ERROR_PCW_BASE + 0x89)
#define ERROR_PCW_NEXTxa           (ERROR_PCW_BASE + 0x8a)
#define ERROR_PCW_NEXTxb           (ERROR_PCW_BASE + 0x8b)
#define ERROR_PCW_NEXTxc           (ERROR_PCW_BASE + 0x8c)
#define ERROR_PCW_NEXTxd           (ERROR_PCW_BASE + 0x8d)
#define ERROR_PCW_NEXTxe           (ERROR_PCW_BASE + 0x8e)
#define ERROR_PCW_NEXTxf           (ERROR_PCW_BASE + 0x8f)
*)


//  Control IDs for hwndStatus child Text controls; title is required

  IDC_STATUS_TITLE = ($1cf0);
  IDC_STATUS_DATA1 = ($1cf1);
  IDC_STATUS_DATA2 = ($1cf2);

function UiCreatePatchPackageA(szaPcpPath, szaPatchPath, szaLogPath: LPSTR; hwndStatus: HWND; szaTempFolder: LPSTR; fRemoveTempFolderIfPresent: BOOL): UINT; stdcall;
function UiCreatePatchPackageW(szwPcpPath, szwPatchPath, szwLogPath: LPWSTR; hwndStatus: HWND; szwTempFolder: LPWSTR; fRemoveTempFolderIfPresent: BOOL): UINT; stdcall;

{$IFDEF UNICODE}
function UiCreatePatchPackage(szwPcpPath, szwPatchPath, szwLogPath: LPWSTR; hwndStatus: HWND; szwTempFolder: LPWSTR; fRemoveTempFolderIfPresent: BOOL): UINT; stdcall;
{$ELSE}
function UiCreatePatchPackage(szaPcpPath, szaPatchPath, szaLogPath: LPSTR; hwndStatus: HWND; szaTempFolder: LPSTR; fRemoveTempFolderIfPresent: BOOL): UINT; stdcall;
{$ENDIF}

implementation

const
  patchwiz = 'patchwiz.dll'; // mvb Installed in Samples\SysMgmt\Msi\Patching


{$IFDEF DYNAMIC_LINK}
var
  _UiCreatePatchPackageA: Pointer;

function UiCreatePatchPackageA;
begin
  GetProcedureAddress(_UiCreatePatchPackageA, patchwiz, 'UiCreatePatchPackageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UiCreatePatchPackageA]
  end;
end;
{$ELSE}
function UiCreatePatchPackageA; external patchwiz name 'UiCreatePatchPackageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UiCreatePatchPackageW: Pointer;

function UiCreatePatchPackageW;
begin
  GetProcedureAddress(_UiCreatePatchPackageW, patchwiz, 'UiCreatePatchPackageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UiCreatePatchPackageW]
  end;
end;
{$ELSE}
function UiCreatePatchPackageW; external patchwiz name 'UiCreatePatchPackageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _UiCreatePatchPackage: Pointer;

function UiCreatePatchPackage;
begin
  GetProcedureAddress(_UiCreatePatchPackage, patchwiz, 'UiCreatePatchPackageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UiCreatePatchPackage]
  end;
end;
{$ELSE}
function UiCreatePatchPackage; external patchwiz name 'UiCreatePatchPackageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _UiCreatePatchPackage: Pointer;

function UiCreatePatchPackage;
begin
  GetProcedureAddress(_UiCreatePatchPackage, patchwiz, 'UiCreatePatchPackageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UiCreatePatchPackage]
  end;
end;
{$ELSE}
function UiCreatePatchPackage; external patchwiz name 'UiCreatePatchPackageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

end.
