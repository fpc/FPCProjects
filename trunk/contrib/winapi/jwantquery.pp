{******************************************************************************}
{                                                       	               }
{  Indezing Service Query API interface Unit for Object Pascal                 }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: nyquery.h, released August 2001. The original Pascal   }
{ code is: NtQuery.pas, released December 2000. The initial developer of the   }
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

unit JwaNtQuery;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "ntquery.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  ActiveX, ComObj, {TODO} JwaWinType;

type
  // TODO STUBS  see CmdTree.h (cmdtree.idl)
  IFilter = Pointer;
  IUnkown = Pointer;
  ICommand = Pointer;
  PPWCHAR = ^PWCHAR;
  REFIID = TGUID; // also in ActiveDS
  DBID = Pointer;
  DBCOMMANDTREE = Pointer;
  PDBCOMMANDTREE = Pointer;
  DBCOMMANDOP = Pointer;

//
// Use this path for the null catalog, one that doesn't have an index.
// Use it to search for properties of files that are not indexed.
//

const
  CINULLCATALOG = '::_noindex_::';

//
// Use this path to connect to the server for administration work
// (i.e. DocStoreAdmin.) No catalog is associated with the connection
//

  CIADMIN = '::_nodocstore_::';

//
// Minimal support for persistent handlers.
//

function LoadIFilter(pwcsPath: PWCHAR; pUnkOuter: IUnknown; out ppIUnk: IFilter): HRESULT; stdcall;

const
  LIFF_LOAD_DEFINED_FILTER                   = 1;
  LIFF_IMPLEMENT_TEXT_FILTER_FALLBACK_POLICY = 2;
  LIFF_FORCE_TEXT_FILTER_FALLBACK            = 3;

function LoadIFilterEx(pwcsPath: PWCHAR; dwFlags: DWORD; const riid: REFIID; out ppIUnk: IUnknown): HRESULT; stdcall;

function BindIFilterFromStorage(pStg: IStorage; pUnkOuter: IUnknown; out ppIUnk: IFilter): HRESULT; stdcall;

function BindIFilterFromStream(pStm: IStream; pUnkOuter: IUnkown; out ppIUnk: IFilter): HRESULT; stdcall;

function LocateCatalogsW(pwszScope: PWCHAR; iBmk: ULONG; pwszMachine: PWCHAR;
  var pccMachine: ULONG; pwszCat: PWCHAR; var pccCat: ULONG): HRESULT; stdcall;

//
// For calling from VB
//

function LocateCatalogsA(pwszScope: PCHAR; iBmk: ULONG; pwszMachine: PCHAR;
  var pccMachine: ULONG; pwszCat: PCHAR; var pccCat: ULONG): HRESULT; stdcall;

{$IFDEF UNICODE}
function LocateCatalogs(pwszScope: PWCHAR; iBmk: ULONG; pwszMachine: PWCHAR;
  var pccMachine: ULONG; pwszCat: PWCHAR; var pccCat: ULONG): HRESULT; stdcall;
{$ELSE}
function LocateCatalogs(pwszScope: PCHAR; iBmk: ULONG; pwszMachine: PCHAR;
  var pccMachine: ULONG; pwszCat: PCHAR; var pccCat: ULONG): HRESULT; stdcall;
{$ENDIF}

// The Index Server Data Source Object CLSID

const
  CLSID_INDEX_SERVER_DSO: TGUID = (
    D1:$F9AE8980; D2:$7E52; D3:$11d0; D4:($89,$64,$00,$C0,$4F,$D6,$11,$D7));

// The storage property set

  PSGUID_STORAGE: TGUID = (
    D1:$b725f130; D2:$47ef; D3:$101a; D4:($a5,$f1,$02,$60,$8c,$9e,$eb,$ac));

//#define PID_STG_DICTIONARY            ((PROPID) 0x00000000) //reserved
//#define PID_STG_CODEPAGE              ((PROPID) 0x00000001) //reserved

  PID_STG_DIRECTORY      = PROPID($00000002);
  PID_STG_CLASSID        = PROPID($00000003);
  PID_STG_STORAGETYPE    = PROPID($00000004);
  PID_STG_VOLUME_ID      = PROPID($00000005);
  PID_STG_PARENT_WORKID  = PROPID($00000006);
  PID_STG_SECONDARYSTORE = PROPID($00000007);
  PID_STG_FILEINDEX      = PROPID($00000008);
  PID_STG_LASTCHANGEUSN  = PROPID($00000009);
  PID_STG_NAME           = PROPID($0000000a);
  PID_STG_PATH           = PROPID($0000000b);
  PID_STG_SIZE           = PROPID($0000000c);
  PID_STG_ATTRIBUTES     = PROPID($0000000d);
  PID_STG_WRITETIME      = PROPID($0000000e);
  PID_STG_CREATETIME     = PROPID($0000000f);
  PID_STG_ACCESSTIME     = PROPID($00000010);
  PID_STG_CHANGETIME     = PROPID($00000011);
  PID_STG_CONTENTS       = PROPID($00000013);
  PID_STG_SHORTNAME      = PROPID($00000014);
  PID_STG_MAX            = PID_STG_SHORTNAME;
  CSTORAGEPROPERTY       = $15;

// File System Content Index Framework property set

  DBPROPSET_FSCIFRMWRK_EXT: TGUID = (
    D1:$A9BD1526; D2:$6A80; D3:$11D0; D4:($8C,$9D,$00,$20,$AF,$1D,$74,$0E));

  DBPROP_CI_CATALOG_NAME   = 2;
  DBPROP_CI_INCLUDE_SCOPES = 3;
  DBPROP_CI_DEPTHS         = 4; // obsolete
  DBPROP_CI_SCOPE_FLAGS    = 4;
  DBPROP_CI_EXCLUDE_SCOPES = 5;
  DBPROP_CI_SECURITY_ID    = 6;
  DBPROP_CI_QUERY_TYPE     = 7;

// Query Extension property set

  DBPROPSET_QUERYEXT: TGUID = (
    D1:$A7AC77ED; D2:$F8D7; D3:$11CE; D4:($A7,$98,$00,$20,$F8,$00,$80,$25));

  DBPROP_USECONTENTINDEX         = 2;
  DBPROP_DEFERNONINDEXEDTRIMMING = 3;
  DBPROP_USEEXTENDEDDBTYPES      = 4;
  DBPROP_FIRSTROWS               = 7;

// Content Index Framework Core property set

  DBPROPSET_CIFRMWRKCORE_EXT: TGUID = (
    D1:$afafaca5; D2:$b5d1; D3:$11d0; D4:($8c,$62,$00,$c0,$4f,$c2,$db,$8d));

  DBPROP_MACHINE      = 2;
  DBPROP_CLIENT_CLSID = 3;

// MSIDXS Rowset property set

  DBPROPSET_MSIDXS_ROWSETEXT: TGUID = (
    D1:$aa6ee6b0; D2:$e828; D3:$11d0; D4:($b2,$3e,$00,$aa,$00,$47,$fc,$01));

  MSIDXSPROP_ROWSETQUERYSTATUS     = 2;
  MSIDXSPROP_COMMAND_LOCALE_STRING = 3;
  MSIDXSPROP_QUERY_RESTRICTION     = 4;

//
// Query status values returned by MSIDXSPROP_ROWSETQUERYSTATUS
//
// Bits   Effect
// -----  -----------------------------------------------------
// 00-02  Fill Status: How data is being updated, if at all.
// 03-15  Bitfield query reliability: How accurate the result is

  STAT_BUSY    = (0);
  STAT_ERROR   = ($1);
  STAT_DONE    = ($2);
  STAT_REFRESH = ($3);

function QUERY_FILL_STATUS(x: DWORD): DWORD;

const
  STAT_PARTIAL_SCOPE            = ($8);
  STAT_NOISE_WORDS              = ($10);
  STAT_CONTENT_OUT_OF_DATE      = ($20);
  STAT_REFRESH_INCOMPLETE       = ($40);
  STAT_CONTENT_QUERY_INCOMPLETE = ($80);
  STAT_TIME_LIMIT_EXCEEDED      = ($100);
  STAT_SHARING_VIOLATION        = ($200);

function QUERY_RELIABILITY_STATUS(x: DWORD): DWORD;

// Scope flags

const
  QUERY_SHALLOW       = 0;
  QUERY_DEEP          = 1;
  QUERY_PHYSICAL_PATH = 0;
  QUERY_VIRTUAL_PATH  = 2;

// query property set (PSGUID_QUERY) properties not defined in oledb.h

  PROPID_QUERY_WORKID       = 5;
  PROPID_QUERY_UNFILTERED   = 7;
  PROPID_QUERY_VIRTUALPATH  = 9;
  PROPID_QUERY_LASTSEENTIME = 10;

//
// Change or get the current state of a catalog specified.
//

  CICAT_STOPPED    = $1;
  CICAT_READONLY   = $2;
  CICAT_WRITABLE   = $4;
  CICAT_NO_QUERY   = $8;
  CICAT_GET_STATE  = $10;
  CICAT_ALL_OPENED = $20;

function SetCatalogState(pwcsCat, pwcsMachine: PWCHAR; dwNewState: DWORD;
  var pdwOldState: DWORD): HRESULT; stdcall;

//
// Query catalog state
//

const
  CI_STATE_SHADOW_MERGE          = $0001; // Index is performing a shadow merge
  CI_STATE_MASTER_MERGE          = $0002; // Index is performing a master merge
  CI_STATE_CONTENT_SCAN_REQUIRED = $0004; // Index is likely corrupt, and a rescan is required
  CI_STATE_ANNEALING_MERGE       = $0008; // Index is performing an annealing (optimizing) merge
  CI_STATE_SCANNING              = $0010; // Scans are in-progress
  CI_STATE_RECOVERING            = $0020; // Index metadata is being recovered
  CI_STATE_INDEX_MIGRATION_MERGE = $0040; // Reserved for future use
  CI_STATE_LOW_MEMORY            = $0080; // Indexing is paused due to low memory availability
  CI_STATE_HIGH_IO               = $0100; // Indexing is paused due to a high rate of I/O
  CI_STATE_MASTER_MERGE_PAUSED   = $0200; // Master merge is paused
  CI_STATE_READ_ONLY             = $0400; // Indexing has been manually paused (read-only)
  CI_STATE_BATTERY_POWER         = $0800; // Indexing is paused to conserve battery life
  CI_STATE_USER_ACTIVE           = $1000; // Indexing is paused due to high user activity (keyboard/mouse)
  CI_STATE_STARTING              = $2000; // Index is still starting up
  CI_STATE_READING_USNS          = $4000; // USNs on NTFS volumes are being processed

//#include <pshpack4.h>

type
  _CI_STATE = record
    cbStruct: DWORD;
    cWordList: DWORD;
    cPersistentIndex: DWORD;
    cQueries: DWORD;
    cDocuments: DWORD;
    cFreshTest: DWORD;
    dwMergeProgress: DWORD;
    eState: DWORD;
    cFilteredDocuments: DWORD;
    cTotalDocuments: DWORD;
    cPendingScans: DWORD;
    dwIndexSize: DWORD;
    cUniqueKeys: DWORD;
    cSecQDocuments: DWORD;
    dwPropCacheSize: DWORD;
  end;
  CI_STATE = _CI_STATE;
  TCiState = CI_STATE;
  PCiState = ^CI_STATE;

//#include <poppack.h>

function CIState(pwcsCat, pwcsMachine: PWCHAR; var pCiState: CI_STATE): HRESULT; stdcall;

//
// Create an ICommand, specifying scopes, catalogs, and machines
//

function CIMakeICommand(out ppCommand: ICommand; cScope: ULONG; aDepths: LPDWORD;
  awcsScope, awcsCatalogs, awcsMachine: PPWCHAR): HRESULT; stdcall;

//
// Create an ICommand, specifying a catalog and machine
//

function CICreateCommand(out ppCommand: IUnknown; pUnkOuter: IUnknown;
  const riid: REFIID; pwcsCatalog, pwcsMachine: PWCHAR): HRESULT; stdcall;

type
  tagCIPROPERTYDEF = record
    wcsFriendlyName: LPWSTR;
    dbType: DWORD;
    dbCol: DBID;
  end;
  CIPROPERTYDEF = tagCIPROPERTYDEF;
  TCiPropertyDef = CIPROPERTYDEF;
  PCiPropertyDef = ^CIPROPERTYDEF;

//
// Values for ulDialect in CITextToSelectTreeEx and CITextToFullTreeEx
//

const
  ISQLANG_V1 = 1; // Same as the non-Ex versions
  ISQLANG_V2 = 2;

//
// Convert pwszRestriction in Triplish to a command tree.
//

function CITextToSelectTree(pwszRestriction: PWCHAR; var ppTree: PDBCOMMANDTREE;
  cProperties: ULONG; pProperties: PCIPROPERTYDEF; LocaleID: LCID): HRESULT; stdcall;

function CITextToSelectTreeEx(pwszRestriction: PWCHAR; ulDialect: ULONG;
  var ppTree: PDBCOMMANDTREE; cProperties: ULONG; pProperties: PCIPROPERTYDEF;
  LocaleID: LCID): HRESULT; stdcall;

//
// Convert pwszRestriction in Triplish, project columns, sort columns
// and grouping columns to a command tree.
//

function CITextToFullTree(pwszRestriction, pwszColumns, pwszSortColumns,
  pwszGroupings: PWCHAR; var ppTree: PDBCOMMANDTREE; cProperties: ULONG;
  pProperties: PCIPROPERTYDEF; LocaleID: LCID): HRESULT; stdcall;

function CITextToFullTreeEx(pwszRestriction: PWCHAR; ulDialect: ULONG;
  pwszColumns, pwszSortColumns, pwszGroupings: PWCHAR; var ppTree: PDBCOMMANDTREE;
  cProperties: ULONG; pProperties: PCIPROPERTYDEF; LocaleID: LCID): HRESULT; stdcall;

//
// Build a simple restriction node.
//

function CIBuildQueryNode(wcsProperty: PWCHAR; dbOperator: DBCOMMANDOP;
  const pvarPropertyValue: PROPVARIANT; var ppTree: PDBCOMMANDTREE;
  cProperties: ULONG; pProperty: PCIPROPERTYDEF; LocaleID: LCID): HRESULT; stdcall;

//
// Build a restriction tree from an existing tree (could be empty) and a newly added node/tree.
//

function CIBuildQueryTree(pExistingTree: PDBCOMMANDTREE; dbBoolOp: DBCOMMANDOP;
  cSiblings: ULONG; ppSibsToCombine: PDBCOMMANDTREE; var ppTree: DBCOMMANDTREE): HRESULT; stdcall;

//
// Convert restriction tree, project columns, sort columns
// and grouping columns to a command tree.
//

function CIRestrictionToFullTree(const pTree: DBCOMMANDTREE; pwszColumns,
  pwszSortColumns, pwszGroupings: PWCHAR; var ppTree: PDBCOMMANDTREE;
  cProperties: ULONG; pReserved: PCIPROPERTYDEF; LocaleID: LCID): HRESULT; stdcall;

implementation

function QUERY_FILL_STATUS(x: DWORD): DWORD;
begin
  Result := x and $7;
end;

function QUERY_RELIABILITY_STATUS(x: DWORD): DWORD;
begin
  Result := x and $FFF8;
end;

const
  query = 'query.dll';

{$IFDEF DYNAMIC_LINK}
var
  _LoadIFilterEx: Pointer;

function LoadIFilterEx;
begin
  GetProcedureAddress(_LoadIFilterEx, query, 'LoadIFilterEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadIFilterEx]
  end;
end;
{$ELSE}
function LoadIFilterEx; external query name 'LoadIFilterEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadIFilter: Pointer;

function LoadIFilter;
begin
  GetProcedureAddress(_LoadIFilter, query, 'LoadIFilter');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadIFilter]
  end;
end;
{$ELSE}
function LoadIFilter; external query name 'LoadIFilter';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BindIFilterFromStorage: Pointer;

function BindIFilterFromStorage;
begin
  GetProcedureAddress(_BindIFilterFromStorage, query, 'BindIFilterFromStorage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BindIFilterFromStorage]
  end;
end;
{$ELSE}
function BindIFilterFromStorage; external query name 'BindIFilterFromStorage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BindIFilterFromStream: Pointer;

function BindIFilterFromStream;
begin
  GetProcedureAddress(_BindIFilterFromStream, query, 'BindIFilterFromStream');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BindIFilterFromStream]
  end;
end;
{$ELSE}
function BindIFilterFromStream; external query name 'BindIFilterFromStream';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocateCatalogsW: Pointer;

function LocateCatalogsW;
begin
  GetProcedureAddress(_LocateCatalogsW, query, 'LocateCatalogsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocateCatalogsW]
  end;
end;
{$ELSE}
function LocateCatalogsW; external query name 'LocateCatalogsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LocateCatalogsA: Pointer;

function LocateCatalogsA;
begin
  GetProcedureAddress(_LocateCatalogsA, query, 'LocateCatalogsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocateCatalogsA]
  end;
end;
{$ELSE}
function LocateCatalogsA; external query name 'LocateCatalogsA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LocateCatalogs: Pointer;

function LocateCatalogs;
begin
  GetProcedureAddress(_LocateCatalogs, query, 'LocateCatalogsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocateCatalogs]
  end;
end;
{$ELSE}
function LocateCatalogs; external query name 'LocateCatalogsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LocateCatalogs: Pointer;

function LocateCatalogs;
begin
  GetProcedureAddress(_LocateCatalogs, query, 'LocateCatalogsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LocateCatalogs]
  end;
end;
{$ELSE}
function LocateCatalogs; external query name 'LocateCatalogsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetCatalogState: Pointer;

function SetCatalogState;
begin
  GetProcedureAddress(_SetCatalogState, query, 'SetCatalogState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCatalogState]
  end;
end;
{$ELSE}
function SetCatalogState; external query name 'SetCatalogState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CIState: Pointer;

function CIState;
begin
  GetProcedureAddress(_CIState, query, 'CIState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CIState]
  end;
end;
{$ELSE}
function CIState; external query name 'CIState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CIMakeICommand: Pointer;

function CIMakeICommand;
begin
  GetProcedureAddress(_CIMakeICommand, query, 'CIMakeICommand');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CIMakeICommand]
  end;
end;
{$ELSE}
function CIMakeICommand; external query name 'CIMakeICommand';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CICreateCommand: Pointer;

function CICreateCommand;
begin
  GetProcedureAddress(_CICreateCommand, query, 'CICreateCommand');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CICreateCommand]
  end;
end;
{$ELSE}
function CICreateCommand; external query name 'CICreateCommand';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CITextToSelectTree: Pointer;

function CITextToSelectTree;
begin
  GetProcedureAddress(_CITextToSelectTree, query, 'CITextToSelectTree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CITextToSelectTree]
  end;
end;
{$ELSE}
function CITextToSelectTree; external query name 'CITextToSelectTree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CITextToSelectTreeEx: Pointer;

function CITextToSelectTreeEx;
begin
  GetProcedureAddress(_CITextToSelectTreeEx, query, 'CITextToSelectTreeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CITextToSelectTreeEx]
  end;
end;
{$ELSE}
function CITextToSelectTreeEx; external query name 'CITextToSelectTreeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CITextToFullTree: Pointer;

function CITextToFullTree;
begin
  GetProcedureAddress(_CITextToFullTree, query, 'CITextToSelectTreeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CITextToFullTree]
  end;
end;
{$ELSE}
function CITextToFullTree; external query name 'CITextToSelectTreeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CITextToFullTreeEx: Pointer;

function CITextToFullTreeEx;
begin
  GetProcedureAddress(_CITextToFullTreeEx, query, 'CITextToFullTreeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CITextToFullTreeEx]
  end;
end;
{$ELSE}
function CITextToFullTreeEx; external query name 'CITextToFullTreeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CIBuildQueryNode: Pointer;

function CIBuildQueryNode;
begin
  GetProcedureAddress(_CIBuildQueryNode, query, 'CIBuildQueryNode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CIBuildQueryNode]
  end;
end;
{$ELSE}
function CIBuildQueryNode; external query name 'CIBuildQueryNode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CIBuildQueryTree: Pointer;

function CIBuildQueryTree;
begin
  GetProcedureAddress(_CIBuildQueryTree, query, 'CIBuildQueryTree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CIBuildQueryTree]
  end;
end;
{$ELSE}
function CIBuildQueryTree; external query name 'CIBuildQueryTree';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CIRestrictionToFullTree: Pointer;

function CIRestrictionToFullTree;
begin
  GetProcedureAddress(_CIRestrictionToFullTree, query, 'CIRestrictionToFullTree');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CIRestrictionToFullTree]
  end;
end;
{$ELSE}
function CIRestrictionToFullTree; external query name 'CIRestrictionToFullTree';
{$ENDIF DYNAMIC_LINK}

end.
