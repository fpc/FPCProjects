{******************************************************************************}
{                                                       	               }
{ IME Component API interface Unit for Object Pascal                           }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: ime.h, released June 2000. The original Pascal         }
{ code is: Ime.pas, released December 2000. The initial developer of the       }
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

unit JwaIme;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "Ime.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

const
  IME_MAXPROCESS = 32;

function SendIMEMessageExA(hWnd: HWND; lParam: LPARAM): LRESULT; stdcall;
function SendIMEMessageExW(hWnd: HWND; lParam: LPARAM): LRESULT; stdcall;
{$IFDEF UNICODE}
function SendIMEMessageEx(hWnd: HWND; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function SendIMEMessageEx(hWnd: HWND; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

//
// IMESTRUCT structure for SendIMEMessageEx
//

type
  tagIMESTRUCT = record
    fnc: UINT;       // function code
    wParam: WPARAM;  // word parameter
    wCount: UINT;    // word counter
    dchSource: UINT; // offset to Source from top of memory object
    dchDest: UINT;   // offset to Desrination from top of memory object
    lParam1: LPARAM;
    lParam2: LPARAM;
    lParam3: LPARAM;
  end;
  IMESTRUCT = tagIMESTRUCT;
  LPIMESTRUCT = ^IMESTRUCT;
  PIMESTRUCT = ^IMESTRUCT;
  NPIMESTRUCT = ^IMESTRUCT;
  TImeStruct = IMESTRUCT;

const
  CP_HWND   = 0;
  CP_OPEN   = 1;
  CP_DIRECT = 2;
  CP_LEVEL  = 3;

//
//      Virtual Keys
//

  VK_DBE_ALPHANUMERIC           = $0f0;
  VK_DBE_KATAKANA               = $0f1;
  VK_DBE_HIRAGANA               = $0f2;
  VK_DBE_SBCSCHAR               = $0f3;
  VK_DBE_DBCSCHAR               = $0f4;
  VK_DBE_ROMAN                  = $0f5;
  VK_DBE_NOROMAN                = $0f6;
  VK_DBE_ENTERWORDREGISTERMODE  = $0f7;
  VK_DBE_ENTERIMECONFIGMODE     = $0f8;
  VK_DBE_FLUSHSTRING            = $0f9;
  VK_DBE_CODEINPUT              = $0fa;
  VK_DBE_NOCODEINPUT            = $0fb;
  VK_DBE_DETERMINESTRING        = $0fc;
  VK_DBE_ENTERDLGCONVERSIONMODE = $0fd;

//
//     switch for wParam of IME_SETCONVERSIONWINDOW
//

  MCW_DEFAULT  = $00;
  MCW_RECT     = $01;
  MCW_WINDOW   = $02;
  MCW_SCREEN   = $04;
  MCW_VERTICAL = $08;
  MCW_HIDDEN   = $10;

//
//    switch for wParam of IME_SETCONVERSIONMODE
//       and IME_GETCONVERSIONMODE
//

  IME_MODE_ALPHANUMERIC = $0001;

{$IFDEF KOREA}    // BeomOh - 9/29/92
  IME_MODE_SBCSCHAR = $0002;
{$ELSE}
  IME_MODE_SBCSCHAR = $0008;
{$ENDIF}

  IME_MODE_KATAKANA     = $0002;
  IME_MODE_HIRAGANA     = $0004;
  IME_MODE_HANJACONVERT = $0004;
  IME_MODE_DBCSCHAR     = $0010;
  IME_MODE_ROMAN        = $0020;
  IME_MODE_NOROMAN      = $0040;
  IME_MODE_CODEINPUT    = $0080;
  IME_MODE_NOCODEINPUT  = $0100;

//
//

//
//     IME APIs
//

  IME_GETIMECAPS          = $03;
  IME_SETOPEN             = $04;
  IME_GETOPEN             = $05;
  IME_GETVERSION          = $07;
  IME_SETCONVERSIONWINDOW = $08;
  IME_MOVEIMEWINDOW       = IME_SETCONVERSIONWINDOW; // KOREA only
  IME_SETCONVERSIONMODE   = $10;

  IME_GETCONVERSIONMODE     = $11;
  IME_SET_MODE              = $12; // KOREA only
  IME_SENDVKEY              = $13;
  IME_ENTERWORDREGISTERMODE = $18;
  IME_SETCONVERSIONFONTEX   = $19;

//
// IME_CODECONVERT subfunctions
//

  IME_BANJAtoJUNJA = $13; // KOREA only
  IME_JUNJAtoBANJA = $14; // KOREA only
  IME_JOHABtoKS    = $15; // KOREA only
  IME_KStoJOHAB    = $16; // KOREA only

//
// IME_AUTOMATA subfunctions
//

  IMEA_INIT = $01; // KOREA only
  IMEA_NEXT = $02; // KOREA only
  IMEA_PREV = $03; // KOREA only

//
// IME_HANJAMODE subfunctions
//

  IME_REQUEST_CONVERT = $01; // KOREA only
  IME_ENABLE_CONVERT  = $02; // KOREA only

//
// IME_MOVEIMEWINDOW subfunctions
//

  INTERIM_WINDOW = $00; // KOREA only
  MODE_WINDOW    = $01; // KOREA only
  HANJA_WINDOW   = $02; // KOREA only

//
//    error code
//

  IME_RS_ERROR       = $01; // genetal error
  IME_RS_NOIME       = $02; // IME is not installed
  IME_RS_TOOLONG     = $05; // given string is too long
  IME_RS_ILLEGAL     = $06; // illegal charactor(s) is string
  IME_RS_NOTFOUND    = $07; // no (more) candidate
  IME_RS_NOROOM      = $0a; // no disk/memory space
  IME_RS_DISKERROR   = $0e; // disk I/O error
  IME_RS_INVALID     = $11; // Win3.1/NT
  IME_RS_NEST        = $12; // called nested
  IME_RS_SYSTEMMODAL = $13; // called when system mode

//
//   report messge from IME to WinApps
//

  WM_IME_REPORT = $0280;

//
//   report message parameter for WM_IME_REPORT
//

  IR_STRINGSTART   = $100;
  IR_STRINGEND     = $101;
  IR_OPENCONVERT   = $120;
  IR_CHANGECONVERT = $121;
  IR_CLOSECONVERT  = $122;
  IR_FULLCONVERT   = $123;
  IR_IMESELECT     = $130;
  IR_STRING        = $140;
  IR_DBCSCHAR      = $160;
  IR_UNDETERMINE   = $170;
  IR_STRINGEX      = $180; // New for 3.1
  IR_MODEINFO      = $190;

//#define WM_CONVERTREQUESTEX     0x0109

  WM_WNT_CONVERTREQUESTEX = $0109; // WM_CONVERTREQUESTEX: 109 for NT, 108 for OT
  WM_CONVERTREQUEST       = $010A;
  WM_CONVERTRESULT        = $010B;
  WM_INTERIM              = $010C;

  WM_IMEKEYDOWN = $290;
  WM_IMEKEYUP   = $291;


//
// UNDETERMINESTRING structure for IR_UNDETERMINE
//

type
  tagUNDETERMINESTRUCT = record
    dwSize: DWORD;
    uDefIMESize: UINT;
    uDefIMEPos: UINT;
    uUndetTextLen: UINT;
    uUndetTextPos: UINT;
    uUndetAttrPos: UINT;
    uCursorPos: UINT;
    uDeltaStart: UINT;
    uDetermineTextLen: UINT;
    uDetermineTextPos: UINT;
    uDetermineDelimPos: UINT;
    uYomiTextLen: UINT;
    uYomiTextPos: UINT;
    uYomiDelimPos: UINT;
  end;
  UNDETERMINESTRUCT = tagUNDETERMINESTRUCT;
  LPUNDETERMINESTRUCT = ^UNDETERMINESTRUCT;
  PUNDETERMINESTRUCT = ^UNDETERMINESTRUCT;
  NPUNDETERMINESTRUCT = ^UNDETERMINESTRUCT;
  TUndetermineStruct = UNDETERMINESTRUCT;

  tagSTRINGEXSTRUCT = record
    dwSize: DWORD;
    uDeterminePos: UINT;
    uDetermineDelimPos: UINT;
    uYomiPos: UINT;
    uYomiDelimPos: UINT;
  end;
  STRINGEXSTRUCT = tagSTRINGEXSTRUCT;
  LPSTRINGEXSTRUCT = ^STRINGEXSTRUCT;
  PSTRINGEXSTRUCT = ^STRINGEXSTRUCT;
  NPSTRINGEXSTRUCT = ^STRINGEXSTRUCT;
  TStringexStruct = STRINGEXSTRUCT;

implementation

const
  imelib = 'user32.dll';


{$IFDEF DYNAMIC_LINK}
var
  _SendIMEMessageExA: Pointer;

function SendIMEMessageExA;
begin
  GetProcedureAddress(_SendIMEMessageExA, imelib, 'SendIMEMessageExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendIMEMessageExA]
  end;
end;
{$ELSE}
function SendIMEMessageExA; external imelib name 'SendIMEMessageExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendIMEMessageExW: Pointer;

function SendIMEMessageExW;
begin
  GetProcedureAddress(_SendIMEMessageExW, imelib, 'SendIMEMessageExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendIMEMessageExW]
  end;
end;
{$ELSE}
function SendIMEMessageExW; external imelib name 'SendIMEMessageExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SendIMEMessageEx: Pointer;

function SendIMEMessageEx;
begin
  GetProcedureAddress(_SendIMEMessageEx, imelib, 'SendIMEMessageExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendIMEMessageEx]
  end;
end;
{$ELSE}
function SendIMEMessageEx; external imelib name 'SendIMEMessageExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SendIMEMessageEx: Pointer;

function SendIMEMessageEx;
begin
  GetProcedureAddress(_SendIMEMessageEx, imelib, 'SendIMEMessageExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendIMEMessageEx]
  end;
end;
{$ELSE}
function SendIMEMessageEx; external imelib name 'SendIMEMessageExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

end.
