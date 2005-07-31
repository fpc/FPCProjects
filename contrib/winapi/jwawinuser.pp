{******************************************************************************}
{                                                       	               }
{ Windows User API interface Unit for Object Pascal                            }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winuser.h, released June 2000. The original Pascal     }
{ code is: WinUser.pas, released December 2000. The initial developer of the   }
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

unit JwaWinUser;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinUser.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinBase, JwaWinGDI, JwaWinNT, JwaWinType;

const
  UINT_MAX = UINT($FFFFFFFF); // from limits.h TODO

type
  HDWP = HANDLE;

  MENUTEMPLATEA = Pointer;
  MENUTEMPLATEW = Pointer;

{$IFDEF UNICODE}
  MENUTEMPLATE = MENUTEMPLATEW;
{$ELSE}
  MENUTEMPLATE = MENUTEMPLATEA;
{$ENDIF}

  LPMENUTEMPLATEA = PVOID;
  LPMENUTEMPLATEW = PVOID;
{$IFDEF UNICODE}
  LPMENUTEMPLATE = LPMENUTEMPLATEW;
{$ELSE}
  LPMENUTEMPLATE = LPMENUTEMPLATEA;
{$ENDIF}

  WNDPROC = function (hWnd: HWND; uMsg: UINT; lParam: WPARAM; wParam: LPARAM): LRESULT;

  DLGPROC = function (hwndDlg: HWND; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): INT_PTR; stdcall;
  TIMERPROC = procedure (hwnd: HWND; uMsg: UINT; idEvent: UINT_PTR; dwTime: DWORD); stdcall;
  GRAYSTRINGPROC = function (hdc: HDC; lpData: LPARAM; cchData: Integer): BOOL; stdcall;
  WNDENUMPROC = function (hwnd: HWND; lParam: LPARAM): BOOL; stdcall;
  HOOKPROC = function (nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  SENDASYNCPROC = procedure (hwnd: HWND; uMsg: UINT; dwData: ULONG_PTR; lResult: LRESULT); stdcall;

  PROPENUMPROCA = function (hwnd: HWND; lpszString: LPCSTR; hData: HANDLE): BOOL; stdcall;
  PROPENUMPROCW = function (hwnd: HWND; lpszString: LPCWSTR; hData: HANDLE): BOOL; stdcall;

  PROPENUMPROCEXA = function (hwnd: HWND; lpszString: LPSTR; hData: HANDLE; dwData: ULONG_PTR): BOOL; stdcall;
  PROPENUMPROCEXW = function (hwnd: HWND; lpszString: LPWSTR; hData: HANDLE; dwData: ULONG_PTR): BOOL; stdcall;

  EDITWORDBREAKPROCA = function (lpch: LPSTR; ichCurrent: Integer; cch, code: Integer): Integer; stdcall;
  EDITWORDBREAKPROCW = function (lpch: LPWSTR; ichCurrent: Integer; cch, code: Integer): Integer; stdcall;

  DRAWSTATEPROC = function (hdc: HDC; lData: LPARAM; wData: WPARAM; cx, cy: Integer): BOOL; stdcall;

{$IFDEF UNICODE}

  PROPENUMPROC = PROPENUMPROCW;
  PROPENUMPROCEX = PROPENUMPROCEXW;
  EDITWORDBREAKPROC = EDITWORDBREAKPROCW;

{$ELSE}

  PROPENUMPROC = PROPENUMPROCA;
  PROPENUMPROCEX = PROPENUMPROCEXA;
  EDITWORDBREAKPROC = EDITWORDBREAKPROCA;

{$ENDIF}

  NAMEENUMPROCA = function (lpstr: LPSTR; lParam: LPARAM): BOOL; stdcall;
  NAMEENUMPROCW = function (lpstr: LPWSTR; lParam: LPARAM): BOOL; stdcall;

  WINSTAENUMPROCA = NAMEENUMPROCA;
  DESKTOPENUMPROCA = NAMEENUMPROCA;
  WINSTAENUMPROCW = NAMEENUMPROCW;
  DESKTOPENUMPROCW = NAMEENUMPROCW;

{$IFDEF UNICODE}

  WINSTAENUMPROC = WINSTAENUMPROCW;
  DESKTOPENUMPROC = DESKTOPENUMPROCW;

{$ELSE}

  WINSTAENUMPROC = WINSTAENUMPROCA;
  DESKTOPENUMPROC = DESKTOPENUMPROCA;

{$ENDIF}

function IS_INTRESOURCE(wInteger: WORD): BOOL;

type
  MAKEINTRESOURCEA = LPSTR;
  MAKEINTRESOURCEW = LPWSTR;
{$IFDEF UNICODE}
  MAKEINTRESOURCE = MAKEINTRESOURCEW;
{$ELSE}
  MAKEINTRESOURCE = MAKEINTRESOURCEA;
{$ENDIF}

//
// Predefined Resource Types
//

const
  RT_CURSOR       = MAKEINTRESOURCE(1);
  RT_BITMAP       = MAKEINTRESOURCE(2);
  RT_ICON         = MAKEINTRESOURCE(3);
  RT_MENU         = MAKEINTRESOURCE(4);
  RT_DIALOG       = MAKEINTRESOURCE(5);
  RT_STRING       = MAKEINTRESOURCE(6);
  RT_FONTDIR      = MAKEINTRESOURCE(7);
  RT_FONT         = MAKEINTRESOURCE(8);
  RT_ACCELERATOR  = MAKEINTRESOURCE(9);
  RT_RCDATA       = MAKEINTRESOURCE(10);
  RT_MESSAGETABLE = MAKEINTRESOURCE(11);

  DIFFERENCE = 11;

  RT_GROUP_CURSOR = MAKEINTRESOURCE(ULONG_PTR(RT_CURSOR) + DIFFERENCE);
  RT_GROUP_ICON = MAKEINTRESOURCE(ULONG_PTR(RT_ICON) + DIFFERENCE);
  RT_VERSION    = MAKEINTRESOURCE(16);
  RT_DLGINCLUDE = MAKEINTRESOURCE(17);
  RT_PLUGPLAY   = MAKEINTRESOURCE(19);
  RT_VXD        = MAKEINTRESOURCE(20);
  RT_ANICURSOR  = MAKEINTRESOURCE(21);
  RT_ANIICON    = MAKEINTRESOURCE(22);
  RT_HTML       = MAKEINTRESOURCE(23);
  RT_MANIFEST   = MAKEINTRESOURCE(24);
  CREATEPROCESS_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(1);
  ISOLATIONAWARE_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(2);
  ISOLATIONAWARE_NOSTATICIMPORT_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(3);
  MINIMUM_RESERVED_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(1{inclusive});
  MAXIMUM_RESERVED_MANIFEST_RESOURCE_ID = MAKEINTRESOURCE(16{inclusive});

type
  va_list = PChar;

function wvsprintfA(Output: LPSTR; Format: LPCSTR; arglist: va_list): Integer; stdcall;
function wvsprintfW(Output: LPWSTR; Format: LPCWSTR; arglist: va_list): Integer; stdcall;

{$IFDEF UNICODE}
function wvsprintf(Output: LPWSTR; Format: LPCWSTR; arglist: va_list): Integer; stdcall;
{$ELSE}
function wvsprintf(Output: LPSTR; Format: LPCSTR; arglist: va_list): Integer; stdcall;
{$ENDIF}

function wsprintfA(Output: LPSTR; Format: LPCSTR): Integer; stdcall;
function wsprintfW(Output: LPWSTR; Format: LPCWSTR): Integer; stdcall;

{$IFDEF UNICODE}
function wsprintf(Output: LPWSTR; Format: LPCWSTR): Integer; stdcall;
{$ELSE}
function wsprintf(Output: LPSTR; Format: LPCSTR): Integer; stdcall;
{$ENDIF}

//
// SPI_SETDESKWALLPAPER defined constants
//

const
  SETWALLPAPER_DEFAULT = LPWSTR(-1);

//
// Scroll Bar Constants
//

  SB_HORZ = 0;
  SB_VERT = 1;
  SB_CTL  = 2;
  SB_BOTH = 3;

//
// Scroll Bar Commands
//

  SB_LINEUP        = 0;
  SB_LINELEFT      = 0;
  SB_LINEDOWN      = 1;
  SB_LINERIGHT     = 1;
  SB_PAGEUP        = 2;
  SB_PAGELEFT      = 2;
  SB_PAGEDOWN      = 3;
  SB_PAGERIGHT     = 3;
  SB_THUMBPOSITION = 4;
  SB_THUMBTRACK    = 5;
  SB_TOP           = 6;
  SB_LEFT          = 6;
  SB_BOTTOM        = 7;
  SB_RIGHT         = 7;
  SB_ENDSCROLL     = 8;

//
// ShowWindow() Commands
//

  SW_HIDE            = 0;
  SW_SHOWNORMAL      = 1;
  SW_NORMAL          = 1;
  SW_SHOWMINIMIZED   = 2;
  SW_SHOWMAXIMIZED   = 3;
  SW_MAXIMIZE        = 3;
  SW_SHOWNOACTIVATE  = 4;
  SW_SHOW            = 5;
  SW_MINIMIZE        = 6;
  SW_SHOWMINNOACTIVE = 7;
  SW_SHOWNA          = 8;
  SW_RESTORE         = 9;
  SW_SHOWDEFAULT     = 10;
  SW_FORCEMINIMIZE   = 11;
  SW_MAX             = 11;

//
// Old ShowWindow() Commands
//

  HIDE_WINDOW         = 0;
  SHOW_OPENWINDOW     = 1;
  SHOW_ICONWINDOW     = 2;
  SHOW_FULLSCREEN     = 3;
  SHOW_OPENNOACTIVATE = 4;

//
// Identifiers for the WM_SHOWWINDOW message
//

  SW_PARENTCLOSING = 1;
  SW_OTHERZOOM     = 2;
  SW_PARENTOPENING = 3;
  SW_OTHERUNZOOM   = 4;

//
// AnimateWindow() Commands
//

  AW_HOR_POSITIVE = $00000001;
  AW_HOR_NEGATIVE = $00000002;
  AW_VER_POSITIVE = $00000004;
  AW_VER_NEGATIVE = $00000008;
  AW_CENTER       = $00000010;
  AW_HIDE         = $00010000;
  AW_ACTIVATE     = $00020000;
  AW_SLIDE        = $00040000;
  AW_BLEND        = $00080000;

//
// WM_KEYUP/DOWN/CHAR HIWORD(lParam) flags
//

  KF_EXTENDED = $0100;
  KF_DLGMODE  = $0800;
  KF_MENUMODE = $1000;
  KF_ALTDOWN  = $2000;
  KF_REPEAT   = $4000;
  KF_UP       = $8000;

//
// Virtual Keys, Standard Set
//

  VK_LBUTTON = $01;
  VK_RBUTTON = $02;
  VK_CANCEL  = $03;
  VK_MBUTTON = $04; // NOT contiguous with L & RBUTTON

  VK_XBUTTON1 = $05; // NOT contiguous with L & RBUTTON
  VK_XBUTTON2 = $06; // NOT contiguous with L & RBUTTON

//
// 0x07 : unassigned
//

  VK_BACK = $08;
  VK_TAB  = $09;

//
// 0x0A - 0x0B : reserved
//

  VK_CLEAR  = $0C;
  VK_RETURN = $0D;

  VK_SHIFT   = $10;
  VK_CONTROL = $11;
  VK_MENU    = $12;
  VK_PAUSE   = $13;
  VK_CAPITAL = $14;

  VK_KANA    = $15;
  VK_HANGEUL = $15; // old name - should be here for compatibility
  VK_HANGUL  = $15;
  VK_JUNJA   = $17;
  VK_FINAL   = $18;
  VK_HANJA   = $19;
  VK_KANJI   = $19;

  VK_ESCAPE = $1B;

  VK_CONVERT    = $1C;
  VK_NONCONVERT = $1D;
  VK_ACCEPT     = $1E;
  VK_MODECHANGE = $1F;

  VK_SPACE    = $20;
  VK_PRIOR    = $21;
  VK_NEXT     = $22;
  VK_END      = $23;
  VK_HOME     = $24;
  VK_LEFT     = $25;
  VK_UP       = $26;
  VK_RIGHT    = $27;
  VK_DOWN     = $28;
  VK_SELECT   = $29;
  VK_PRINT    = $2A;
  VK_EXECUTE  = $2B;
  VK_SNAPSHOT = $2C;
  VK_INSERT   = $2D;
  VK_DELETE   = $2E;
  VK_HELP     = $2F;

//
// VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
// 0x40 : unassigned
// VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
//

  VK_LWIN = $5B;
  VK_RWIN = $5C;
  VK_APPS = $5D;

//
// 0x5E : reserved
//

  VK_SLEEP = $5F;

  VK_NUMPAD0   = $60;
  VK_NUMPAD1   = $61;
  VK_NUMPAD2   = $62;
  VK_NUMPAD3   = $63;
  VK_NUMPAD4   = $64;
  VK_NUMPAD5   = $65;
  VK_NUMPAD6   = $66;
  VK_NUMPAD7   = $67;
  VK_NUMPAD8   = $68;
  VK_NUMPAD9   = $69;
  VK_MULTIPLY  = $6A;
  VK_ADD       = $6B;
  VK_SEPARATOR = $6C;
  VK_SUBTRACT  = $6D;
  VK_DECIMAL   = $6E;
  VK_DIVIDE    = $6F;
  VK_F1        = $70;
  VK_F2        = $71;
  VK_F3        = $72;
  VK_F4        = $73;
  VK_F5        = $74;
  VK_F6        = $75;
  VK_F7        = $76;
  VK_F8        = $77;
  VK_F9        = $78;
  VK_F10       = $79;
  VK_F11       = $7A;
  VK_F12       = $7B;
  VK_F13       = $7C;
  VK_F14       = $7D;
  VK_F15       = $7E;
  VK_F16       = $7F;
  VK_F17       = $80;
  VK_F18       = $81;
  VK_F19       = $82;
  VK_F20       = $83;
  VK_F21       = $84;
  VK_F22       = $85;
  VK_F23       = $86;
  VK_F24       = $87;

//
// 0x88 - 0x8F : unassigned
//

  VK_NUMLOCK = $90;
  VK_SCROLL  = $91;

//
// NEC PC-9800 kbd definitions
//

  VK_OEM_NEC_EQUAL = $92; // '=' key on numpad

//
// Fujitsu/OASYS kbd definitions
//

  VK_OEM_FJ_JISHO   = $92; // 'Dictionary' key
  VK_OEM_FJ_MASSHOU = $93; // 'Unregister word' key
  VK_OEM_FJ_TOUROKU = $94; // 'Register word' key
  VK_OEM_FJ_LOYA    = $95; // 'Left OYAYUBI' key
  VK_OEM_FJ_ROYA    = $96; // 'Right OYAYUBI' key

//
// 0x97 - 0x9F : unassigned
//

//
// VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
// Used only as parameters to GetAsyncKeyState() and GetKeyState().
// No other API or message will distinguish left and right keys in this way.
//

  VK_LSHIFT   = $A0;
  VK_RSHIFT   = $A1;
  VK_LCONTROL = $A2;
  VK_RCONTROL = $A3;
  VK_LMENU    = $A4;
  VK_RMENU    = $A5;

  VK_BROWSER_BACK      = $A6;
  VK_BROWSER_FORWARD   = $A7;
  VK_BROWSER_REFRESH   = $A8;
  VK_BROWSER_STOP      = $A9;
  VK_BROWSER_SEARCH    = $AA;
  VK_BROWSER_FAVORITES = $AB;
  VK_BROWSER_HOME      = $AC;

  VK_VOLUME_MUTE         = $AD;
  VK_VOLUME_DOWN         = $AE;
  VK_VOLUME_UP           = $AF;
  VK_MEDIA_NEXT_TRACK    = $B0;
  VK_MEDIA_PREV_TRACK    = $B1;
  VK_MEDIA_STOP          = $B2;
  VK_MEDIA_PLAY_PAUSE    = $B3;
  VK_LAUNCH_MAIL         = $B4;
  VK_LAUNCH_MEDIA_SELECT = $B5;
  VK_LAUNCH_APP1         = $B6;
  VK_LAUNCH_APP2         = $B7;

//
// 0xB8 - 0xB9 : reserved
//

  VK_OEM_1      = $BA; // ';:' for US
  VK_OEM_PLUS   = $BB; // '+' any country
  VK_OEM_COMMA  = $BC; // ',' any country
  VK_OEM_MINUS  = $BD; // '-' any country
  VK_OEM_PERIOD = $BE; // '.' any country
  VK_OEM_2      = $BF; // '/?' for US
  VK_OEM_3      = $C0; // '`~' for US

//
// 0xC1 - 0xD7 : reserved
//

//
// 0xD8 - 0xDA : unassigned
//

  VK_OEM_4 = $DB; // '[{' for US
  VK_OEM_5 = $DC; // '\|' for US
  VK_OEM_6 = $DD; // ']}' for US
  VK_OEM_7 = $DE; // ''"' for US
  VK_OEM_8 = $DF;

//
// 0xE0 : reserved
//

//
// Various extended or enhanced keyboards
//

  VK_OEM_AX   = $E1; // 'AX' key on Japanese AX kbd
  VK_OEM_102  = $E2; // "<>" or "\|" on RT 102-key kbd.
  VK_ICO_HELP = $E3; // Help key on ICO
  VK_ICO_00   = $E4; // 00 key on ICO

  VK_PROCESSKEY = $E5;

  VK_ICO_CLEAR = $E6;

  VK_PACKET = $E7;

//
// 0xE8 : unassigned
//

//
// Nokia/Ericsson definitions
//

  VK_OEM_RESET   = $E9;
  VK_OEM_JUMP    = $EA;
  VK_OEM_PA1     = $EB;
  VK_OEM_PA2     = $EC;
  VK_OEM_PA3     = $ED;
  VK_OEM_WSCTRL  = $EE;
  VK_OEM_CUSEL   = $EF;
  VK_OEM_ATTN    = $F0;
  VK_OEM_FINISH  = $F1;
  VK_OEM_COPY    = $F2;
  VK_OEM_AUTO    = $F3;
  VK_OEM_ENLW    = $F4;
  VK_OEM_BACKTAB = $F5;

  VK_ATTN      = $F6;
  VK_CRSEL     = $F7;
  VK_EXSEL     = $F8;
  VK_EREOF     = $F9;
  VK_PLAY      = $FA;
  VK_ZOOM      = $FB;
  VK_NONAME    = $FC;
  VK_PA1       = $FD;
  VK_OEM_CLEAR = $FE;

//
// 0xFF : reserved
//


//
// SetWindowsHook() codes
//

  WH_MIN             = DWORD(-1);
  WH_MSGFILTER       = DWORD(-1);
  WH_JOURNALRECORD   = 0;
  WH_JOURNALPLAYBACK = 1;
  WH_KEYBOARD        = 2;
  WH_GETMESSAGE      = 3;
  WH_CALLWNDPROC     = 4;
  WH_CBT             = 5;
  WH_SYSMSGFILTER    = 6;
  WH_MOUSE           = 7;
  WH_HARDWARE        = 8;
  WH_DEBUG           = 9;
  WH_SHELL           = 10;
  WH_FOREGROUNDIDLE  = 11;
  WH_CALLWNDPROCRET  = 12;

  WH_KEYBOARD_LL = 13;
  WH_MOUSE_LL    = 14;

{$IFDEF WINVER_0400_UP}
  {$IFDEF WINNT_0400_UP}
  WH_MAX = 14;
  {$ELSE}
  WH_MAX = 12;
  {$ENDIF}
{$ELSE}
  WH_MAX = 11;
{$ENDIF}

  WH_MINHOOK = WH_MIN;
  WH_MAXHOOK = WH_MAX;

//
// Hook Codes
//

  HC_ACTION      = 0;
  HC_GETNEXT     = 1;
  HC_SKIP        = 2;
  HC_NOREMOVE    = 3;
  HC_NOREM       = HC_NOREMOVE;
  HC_SYSMODALON  = 4;
  HC_SYSMODALOFF = 5;

//
// CBT Hook Codes
//

  HCBT_MOVESIZE     = 0;
  HCBT_MINMAX       = 1;
  HCBT_QS           = 2;
  HCBT_CREATEWND    = 3;
  HCBT_DESTROYWND   = 4;
  HCBT_ACTIVATE     = 5;
  HCBT_CLICKSKIPPED = 6;
  HCBT_KEYSKIPPED   = 7;
  HCBT_SYSCOMMAND   = 8;
  HCBT_SETFOCUS     = 9;

//
// HCBT_ACTIVATE structure pointed to by lParam
//

type
  LPCBTACTIVATESTRUCT = ^CBTACTIVATESTRUCT;
  tagCBTACTIVATESTRUCT = record
    fMouse: BOOL;
    hWndActive: HWND;
  end;
  CBTACTIVATESTRUCT = tagCBTACTIVATESTRUCT;
  TCbtActivateStruct = CBTACTIVATESTRUCT;
  PCbtActivateStruct = LPCBTACTIVATESTRUCT;

//
// WTSSESSION_NOTIFICATION struct pointed by lParam, for WM_WTSSESSION_CHANGE
//

  tagWTSSESSION_NOTIFICATION = record
    cbSize: DWORD;
    dwSessionId: DWORD;
  end;
  WTSSESSION_NOTIFICATION = tagWTSSESSION_NOTIFICATION;
  PWTSSESSION_NOTIFICATION = ^WTSSESSION_NOTIFICATION;
  TWtsSessionNotification = WTSSESSION_NOTIFICATION;
  PWtsSessionNotification = PWTSSESSION_NOTIFICATION;

//
// codes passed in WPARAM for WM_WTSSESSION_CHANGE
//

const
  WTS_CONSOLE_CONNECT     = $1;
  WTS_CONSOLE_DISCONNECT  = $2;
  WTS_REMOTE_CONNECT      = $3;
  WTS_REMOTE_DISCONNECT   = $4;
  WTS_SESSION_LOGON       = $5;
  WTS_SESSION_LOGOFF      = $6;
  WTS_SESSION_LOCK        = $7;
  WTS_SESSION_UNLOCK      = $8;
  WTS_SESSION_REMOTE_CONTROL = $9;

//
// WH_MSGFILTER Filter Proc Codes
//

const
  MSGF_DIALOGBOX  = 0;
  MSGF_MESSAGEBOX = 1;
  MSGF_MENU       = 2;
  MSGF_SCROLLBAR  = 5;
  MSGF_NEXTWINDOW = 6;
  MSGF_MAX        = 8; // unused
  MSGF_USER       = 4096;

//
// Shell support
//

  HSHELL_WINDOWCREATED       = 1;
  HSHELL_WINDOWDESTROYED     = 2;
  HSHELL_ACTIVATESHELLWINDOW = 3;

  HSHELL_WINDOWACTIVATED = 4;
  HSHELL_GETMINRECT      = 5;
  HSHELL_REDRAW          = 6;
  HSHELL_TASKMAN         = 7;
  HSHELL_LANGUAGE        = 8;
  HSHELL_SYSMENU            = 9;
  HSHELL_ENDTASK            = 10;
  HSHELL_ACCESSIBILITYSTATE = 11;
  HSHELL_APPCOMMAND      = 12;

  HSHELL_WINDOWREPLACED  = 13;
  HSHELL_WINDOWREPLACING = 14;

  HSHELL_HIGHBIT          = $8000;
  HSHELL_FLASH            = (HSHELL_REDRAW or HSHELL_HIGHBIT);
  HSHELL_RUDEAPPACTIVATED = (HSHELL_WINDOWACTIVATED or HSHELL_HIGHBIT);

// wparam for HSHELL_ACCESSIBILITYSTATE//

  ACCESS_STICKYKEYS = $0001;
  ACCESS_FILTERKEYS = $0002;
  ACCESS_MOUSEKEYS  = $0003;

// cmd for HSHELL_APPCOMMAND and WM_APPCOMMAND//

  APPCOMMAND_BROWSER_BACKWARD    = 1;
  APPCOMMAND_BROWSER_FORWARD     = 2;
  APPCOMMAND_BROWSER_REFRESH     = 3;
  APPCOMMAND_BROWSER_STOP        = 4;
  APPCOMMAND_BROWSER_SEARCH      = 5;
  APPCOMMAND_BROWSER_FAVORITES   = 6;
  APPCOMMAND_BROWSER_HOME        = 7;
  APPCOMMAND_VOLUME_MUTE         = 8;
  APPCOMMAND_VOLUME_DOWN         = 9;
  APPCOMMAND_VOLUME_UP           = 10;
  APPCOMMAND_MEDIA_NEXTTRACK     = 11;
  APPCOMMAND_MEDIA_PREVIOUSTRACK = 12;
  APPCOMMAND_MEDIA_STOP          = 13;
  APPCOMMAND_MEDIA_PLAY_PAUSE    = 14;
  APPCOMMAND_LAUNCH_MAIL         = 15;
  APPCOMMAND_LAUNCH_MEDIA_SELECT = 16;
  APPCOMMAND_LAUNCH_APP1         = 17;
  APPCOMMAND_LAUNCH_APP2         = 18;
  APPCOMMAND_BASS_DOWN           = 19;
  APPCOMMAND_BASS_BOOST          = 20;
  APPCOMMAND_BASS_UP             = 21;
  APPCOMMAND_TREBLE_DOWN         = 22;
  APPCOMMAND_TREBLE_UP           = 23;
  APPCOMMAND_MICROPHONE_VOLUME_MUTE = 24;
  APPCOMMAND_MICROPHONE_VOLUME_DOWN = 25;
  APPCOMMAND_MICROPHONE_VOLUME_UP   = 26;
  APPCOMMAND_HELP                   = 27;
  APPCOMMAND_FIND                   = 28;
  APPCOMMAND_NEW                    = 29;
  APPCOMMAND_OPEN                   = 30;
  APPCOMMAND_CLOSE                  = 31;
  APPCOMMAND_SAVE                   = 32;
  APPCOMMAND_PRINT                  = 33;
  APPCOMMAND_UNDO                   = 34;
  APPCOMMAND_REDO                   = 35;
  APPCOMMAND_COPY                   = 36;
  APPCOMMAND_CUT                    = 37;
  APPCOMMAND_PASTE                  = 38;
  APPCOMMAND_REPLY_TO_MAIL          = 39;
  APPCOMMAND_FORWARD_MAIL           = 40;
  APPCOMMAND_SEND_MAIL              = 41;
  APPCOMMAND_SPELL_CHECK            = 42;
  APPCOMMAND_DICTATE_OR_COMMAND_CONTROL_TOGGLE = 43;
  APPCOMMAND_MIC_ON_OFF_TOGGLE      = 44;
  APPCOMMAND_CORRECTION_LIST        = 45;
  APPCOMMAND_MEDIA_PLAY             = 46;
  APPCOMMAND_MEDIA_PAUSE            = 47;
  APPCOMMAND_MEDIA_RECORD           = 48;
  APPCOMMAND_MEDIA_FAST_FORWARD     = 49;
  APPCOMMAND_MEDIA_REWIND           = 50;
  APPCOMMAND_MEDIA_CHANNEL_UP       = 51;
  APPCOMMAND_MEDIA_CHANNEL_DOWN     = 52;

  FAPPCOMMAND_MOUSE = $8000;
  FAPPCOMMAND_KEY   = 0;
  FAPPCOMMAND_OEM   = $1000;
  FAPPCOMMAND_MASK  = $F000;

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Shortint;

function GET_DEVICE_LPARAM(lParam: LPARAM): WORD;

function GET_MOUSEORKEY_LPARAM(lParam: LPARAM): WORD;

function GET_FLAGS_LPARAM(lParam: LPARAM): Integer;

function GET_KEYSTATE_LPARAM(lParam: LPARAM): Integer;

type
  SHELLHOOKINFO = record
    hwnd: HWND;
    rc: RECT;
  end;
  LPSHELLHOOKINFO = ^SHELLHOOKINFO;
  TShellHookInfo = SHELLHOOKINFO;
  PShellHookInfo = LPSHELLHOOKINFO;

//
// Message Structure used in Journaling
//

type
  LPEVENTMSG = ^EVENTMSG;
  tagEVENTMSG = record
    message_: UINT;
    paramL: UINT;
    paramH: UINT;
    time: DWORD;
    hwnd: HWND;
  end;
  EVENTMSG = tagEVENTMSG;
  LPEVENTMSGMSG = ^EVENTMSG;
  PEVENTMSGMSG = ^EVENTMSG;
  NPEVENTMSG = ^EVENTMSG;
  NPEVENTMSGMSG = ^EVENTMSG;
  TEventMsg = EVENTMSG;
  PEventMsg = LPEVENTMSG;

//
// Message structure used by WH_CALLWNDPROC
//

  LPCWPSTRUCT = ^CWPSTRUCT;
  tagCWPSTRUCT = record
    lParam: LPARAM;
    wParam: WPARAM;
    message: UINT;
    hwnd: HWND;
  end;
  CWPSTRUCT = tagCWPSTRUCT;
  NPCWPSTRUCT = ^CWPSTRUCT;
  TCwpStruct = CWPSTRUCT;
  PCwpStruct = LPCWPSTRUCT;

//
// Message structure used by WH_CALLWNDPROCRET
//

  LPCWPRETSTRUCT = ^CWPRETSTRUCT;
  tagCWPRETSTRUCT = record
    lResult: LRESULT;
    lParam: LPARAM;
    wParam: WPARAM;
    message: UINT;
    hwnd: HWND;
  end;
  CWPRETSTRUCT = tagCWPRETSTRUCT;
  NPCWPRETSTRUCT = ^CWPRETSTRUCT;
  TCwpRetStruct = CWPRETSTRUCT;
  PCwpRetStruct = LPCWPRETSTRUCT;

//
// Low level hook flags
//

const
  LLKHF_EXTENDED = (KF_EXTENDED shr 8);
  LLKHF_INJECTED = $00000010;
  LLKHF_ALTDOWN  = (KF_ALTDOWN shr 8);
  LLKHF_UP       = (KF_UP shr 8);

  LLMHF_INJECTED = $00000001;

//
// Structure used by WH_KEYBOARD_LL
//

type
  LPKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;
  tagKBDLLHOOKSTRUCT = record
    vkCode: DWORD;
    scanCode: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  KBDLLHOOKSTRUCT = tagKBDLLHOOKSTRUCT;
  TKbDllHookStruct = KBDLLHOOKSTRUCT;
  PKbDllHookStruct = LPKBDLLHOOKSTRUCT;

//
// Structure used by WH_MOUSE_LL
//

  LPMSLLHOOKSTRUCT = ^MSLLHOOKSTRUCT;
  tagMSLLHOOKSTRUCT = record
    pt: POINT;
    mouseData: DWORD;
    flags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  MSLLHOOKSTRUCT = tagMSLLHOOKSTRUCT;
  TMsllHookStruct = MSLLHOOKSTRUCT;
  PMsllHookStruct = LPMSLLHOOKSTRUCT;

//
// Structure used by WH_DEBUG
//

  LPDEBUGHOOKINFO = ^DEBUGHOOKINFO;
  tagDEBUGHOOKINFO = record
    idThread: DWORD;
    idThreadInstaller: DWORD;
    lParam: LPARAM;
    wParam: WPARAM;
    code: Integer;
  end;
  DEBUGHOOKINFO = tagDEBUGHOOKINFO;
  NPDEBUGHOOKINFO = ^DEBUGHOOKINFO;
  TDebugHookInfo = DEBUGHOOKINFO;
  PDebugHookInfo = LPDEBUGHOOKINFO;

//
// Structure used by WH_MOUSE
//

  LPMOUSEHOOKSTRUCT = ^MOUSEHOOKSTRUCT;
  tagMOUSEHOOKSTRUCT = record
    pt: POINT;
    hwnd: HWND;
    wHitTestCode: UINT;
    dwExtraInfo: ULONG_PTR;
  end;
  MOUSEHOOKSTRUCT = tagMOUSEHOOKSTRUCT;
  TMouseHookStruct = MOUSEHOOKSTRUCT;
  PMouseHookStruct = LPMOUSEHOOKSTRUCT;

  LPMOUSEHOOKSTRUCTEX = ^MOUSEHOOKSTRUCTEX;
  tagMOUSEHOOKSTRUCTEX = record
    mhs: MOUSEHOOKSTRUCT;
    mouseData: DWORD;
  end;
  MOUSEHOOKSTRUCTEX = tagMOUSEHOOKSTRUCTEX;
  TMouseHookStructEx = MOUSEHOOKSTRUCTEX;
  PMouseHookStructEx = LPMOUSEHOOKSTRUCTEX;

//
// Structure used by WH_HARDWARE
//

  LPHARDWAREHOOKSTRUCT = ^HARDWAREHOOKSTRUCT;
  tagHARDWAREHOOKSTRUCT = record
    hwnd: HWND;
    message: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
  end;
  HARDWAREHOOKSTRUCT = tagHARDWAREHOOKSTRUCT;
  THardwareHookStruct = HARDWAREHOOKSTRUCT;
  PHardwareHookStruct = LPHARDWAREHOOKSTRUCT;

//
// Keyboard Layout API
//

const
  HKL_PREV = 0;
  HKL_NEXT = 1;

  KLF_ACTIVATE      = $00000001;
  KLF_SUBSTITUTE_OK = $00000002;
  KLF_REORDER       = $00000008;
  KLF_REPLACELANG   = $00000010;
  KLF_NOTELLSHELL   = $00000080;
  KLF_SETFORPROCESS = $00000100;
  KLF_SHIFTLOCK     = $00010000;
  KLF_RESET         = $40000000;

//
// Bits in wParam of WM_INPUTLANGCHANGEREQUEST message
//

  INPUTLANGCHANGE_SYSCHARSET = $0001;
  INPUTLANGCHANGE_FORWARD    = $0002;
  INPUTLANGCHANGE_BACKWARD   = $0004;

//
// Size of KeyboardLayoutName (number of characters), including nul terminator
//

  KL_NAMELENGTH = 9;

function LoadKeyboardLayoutA(pwszKLID: LPCSTR; Flags: UINT): HKL; stdcall;
function LoadKeyboardLayoutW(pwszKLID: LPCWSTR; Flags: UINT): HKL; stdcall;

{$IFDEF UNICODE}
function LoadKeyboardLayout(pwszKLID: LPCWSTR; Flags: UINT): HKL; stdcall;
{$ELSE}
function LoadKeyboardLayout(pwszKLID: LPCSTR; Flags: UINT): HKL; stdcall;
{$ENDIF}

{$IFDEF WINVER_0400_GREATER}
function ActivateKeyboardLayout(hkl: HKL; Flags: UINT): HKL; stdcall;
{$ELSE}
function ActivateKeyboardLayout(hkl: HKL; Flags: UINT): BOOL; stdcall;
{$ENDIF}

function ToUnicodeEx(wVirtKey, wScanCode: UINT; lpKeyState: PBYTE;
  pwszBuff: LPWSTR; cchBuff: Integer; wFlags: UINT; dwhkl: HKL): Integer; stdcall;

function UnloadKeyboardLayout(hkl: HKL): BOOL; stdcall;

function GetKeyboardLayoutNameA(pwszKLID: LPSTR): BOOL; stdcall;
function GetKeyboardLayoutNameW(pwszKLID: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function GetKeyboardLayoutName(pwszKLID: LPWSTR): BOOL; stdcall;
{$ELSE}
function GetKeyboardLayoutName(pwszKLID: LPSTR): BOOL; stdcall;
{$ENDIF}

function GetKeyboardLayoutList(nBuff: Integer; lpList: PHKL): Integer; stdcall;

function GetKeyboardLayout(idThread: DWORD): HKL; stdcall;

type
  LPMOUSEMOVEPOINT = ^MOUSEMOVEPOINT;
  tagMOUSEMOVEPOINT = record
    x: Integer;
    y: Integer;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  MOUSEMOVEPOINT = tagMOUSEMOVEPOINT;
  TMouseMovePoint = MOUSEMOVEPOINT;
  PMouseMovePoint = LPMOUSEMOVEPOINT;

//
// Values for resolution parameter of GetMouseMovePointsEx
//

const
  GMMP_USE_DISPLAY_POINTS         = 1;
  GMMP_USE_HIGH_RESOLUTION_POINTS = 2;

function GetMouseMovePointsEx(cbSize: UINT; lppt, lpptBuf: LPMOUSEMOVEPOINT;
  nBufPoints: Integer; resolution: DWORD): Integer; stdcall;

//
// Desktop-specific access flags
//

const
  DESKTOP_READOBJECTS     = $0001;
  DESKTOP_CREATEWINDOW    = $0002;
  DESKTOP_CREATEMENU      = $0004;
  DESKTOP_HOOKCONTROL     = $0008;
  DESKTOP_JOURNALRECORD   = $0010;
  DESKTOP_JOURNALPLAYBACK = $0020;
  DESKTOP_ENUMERATE       = $0040;
  DESKTOP_WRITEOBJECTS    = $0080;
  DESKTOP_SWITCHDESKTOP   = $0100;

//
// Desktop-specific control flags
//

  DF_ALLOWOTHERACCOUNTHOOK = $0001;

function CreateDesktopA(lpszDesktop, lpszDevice: LPCSTR; pDevmode: LPDEVMODEA;
  dwFlags: DWORD; dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HDESK; stdcall;
function CreateDesktopW(lpszDesktop, lpszDevice: LPCWSTR; pDevmode: LPDEVMODEW;
  dwFlags: DWORD; dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HDESK; stdcall;

{$IFDEF UNICODE}
function CreateDesktop(lpszDesktop, lpszDevice: LPCWSTR; pDevmode: LPDEVMODEW;
  dwFlags: DWORD; dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HDESK; stdcall;
{$ELSE}
function CreateDesktop(lpszDesktop, lpszDevice: LPCSTR; pDevmode: LPDEVMODEA;
  dwFlags: DWORD; dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HDESK; stdcall;
{$ENDIF}

function OpenDesktopA(lpszDesktop: LPCSTR; dwFlags: DWORD; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HDESK; stdcall;
function OpenDesktopW(lpszDesktop: LPCWSTR; dwFlags: DWORD; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HDESK; stdcall;

{$IFDEF UNICODE}
function OpenDesktop(lpszDesktop: LPCWSTR; dwFlags: DWORD; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HDESK; stdcall;
{$ELSE}
function OpenDesktop(lpszDesktop: LPCSTR; dwFlags: DWORD; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HDESK; stdcall;
{$ENDIF}

function OpenInputDesktop(dwFlags: DWORD; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HDESK; stdcall;

function EnumDesktopsA(hwinsta: HWINSTA; lpEnumFunc: DESKTOPENUMPROCA;
  lParam: LPARAM): BOOL; stdcall;
function EnumDesktopsW(hwinsta: HWINSTA; lpEnumFunc: DESKTOPENUMPROCW;
  lParam: LPARAM): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumDesktops(hwinsta: HWINSTA; lpEnumFunc: DESKTOPENUMPROCW;
  lParam: LPARAM): BOOL; stdcall;
{$ELSE}
function EnumDesktops(hwinsta: HWINSTA; lpEnumFunc: DESKTOPENUMPROCA;
  lParam: LPARAM): BOOL; stdcall;
{$ENDIF}

function EnumDesktopWindows(hDesktop: HDESK; lpfn: WNDENUMPROC; lParam: LPARAM): BOOL; stdcall;

function SwitchDesktop(hDesktop: HDESK): BOOL; stdcall;

function SetThreadDesktop(hDesktop: HDESK): BOOL; stdcall;

function CloseDesktop(hDesktop: HDESK): BOOL; stdcall;

function GetThreadDesktop(dwThreadId: DWORD): HDESK; stdcall;

//
// Windowstation-specific access flags
//

const
  WINSTA_ENUMDESKTOPS      = $0001;
  WINSTA_READATTRIBUTES    = $0002;
  WINSTA_ACCESSCLIPBOARD   = $0004;
  WINSTA_CREATEDESKTOP     = $0008;
  WINSTA_WRITEATTRIBUTES   = $0010;
  WINSTA_ACCESSGLOBALATOMS = $0020;
  WINSTA_EXITWINDOWS       = $0040;
  WINSTA_ENUMERATE         = $0100;
  WINSTA_READSCREEN        = $0200;

  WINSTA_ALL_ACCESS        = (WINSTA_ENUMDESKTOPS or WINSTA_READATTRIBUTES or WINSTA_ACCESSCLIPBOARD or
                              WINSTA_CREATEDESKTOP or WINSTA_WRITEATTRIBUTES or WINSTA_ACCESSGLOBALATOMS or
                              WINSTA_EXITWINDOWS or WINSTA_ENUMERATE or WINSTA_READSCREEN);

//
// Windowstation creation flags.
//

  CWF_CREATE_ONLY          = $0001;

//
// Windowstation-specific attribute flags
//

  WSF_VISIBLE = $0001;

function CreateWindowStationA(lpwinsta: LPCSTR; dwFlags: DWORD;
  dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HWINSTA; stdcall;
function CreateWindowStationW(lpwinsta: LPCWSTR; dwFlags: DWORD;
  dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HWINSTA; stdcall;

{$IFDEF UNICODE}
function CreateWindowStation(lpwinsta: LPCWSTR; dwFlags: DWORD;
  dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HWINSTA; stdcall;
{$ELSE}
function CreateWindowStation(lpwinsta: LPCSTR; dwFlags: DWORD;
  dwDesiredAccess: ACCESS_MASK; lpsa: LPSECURITY_ATTRIBUTES): HWINSTA; stdcall;
{$ENDIF}

function OpenWindowStationA(lpszWinSta: LPCSTR; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HWINSTA; stdcall;
function OpenWindowStationW(lpszWinSta: LPCWSTR; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HWINSTA; stdcall;

{$IFDEF UNICODE}
function OpenWindowStation(lpszWinSta: LPCWSTR; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HWINSTA; stdcall;
{$ELSE}
function OpenWindowStation(lpszWinSta: LPCSTR; fInherit: BOOL;
  dwDesiredAccess: ACCESS_MASK): HWINSTA; stdcall;
{$ENDIF}

function EnumWindowStationsA(lpEnumFunc: WINSTAENUMPROCA; lParam: LPARAM): BOOL; stdcall;
function EnumWindowStationsW(lpEnumFunc: WINSTAENUMPROCW; lParam: LPARAM): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumWindowStations(lpEnumFunc: WINSTAENUMPROCW; lParam: LPARAM): BOOL; stdcall;
{$ELSE}
function EnumWindowStations(lpEnumFunc: WINSTAENUMPROCA; lParam: LPARAM): BOOL; stdcall;
{$ENDIF}

function CloseWindowStation(hWinSta: HWINSTA): BOOL; stdcall;

function SetProcessWindowStation(hWinSta: HWINSTA): BOOL; stdcall;

function GetProcessWindowStation: HWINSTA; stdcall;

function SetUserObjectSecurity(hObj: HANDLE; var pSIRequested: SECURITY_INFORMATION;
  pSID: PSECURITY_DESCRIPTOR): BOOL; stdcall;

function GetUserObjectSecurity(hObj: HANDLE; var pSIRequested: SECURITY_INFORMATION;
  pSID: PSECURITY_DESCRIPTOR; nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;

const
  UOI_FLAGS    = 1;
  UOI_NAME     = 2;
  UOI_TYPE     = 3;
  UOI_USER_SID = 4;

type
  PUSEROBJECTFLAGS = ^USEROBJECTFLAGS;
  tagUSEROBJECTFLAGS = record
    fInherit: BOOL;
    fReserved: BOOL;
    dwFlags: DWORD;
  end;
  USEROBJECTFLAGS = tagUSEROBJECTFLAGS;
  TUserObjectFlags = USEROBJECTFLAGS;

function GetUserObjectInformationA(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
function GetUserObjectInformationW(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function GetUserObjectInformation(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$ELSE}
function GetUserObjectInformation(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD; var lpnLengthNeeded: DWORD): BOOL; stdcall;
{$ENDIF}

function SetUserObjectInformationA(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD): BOOL; stdcall;
function SetUserObjectInformationW(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function SetUserObjectInformation(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD): BOOL; stdcall;
{$ELSE}
function SetUserObjectInformation(hObj: HANDLE; nIndex: Integer; pvInfo: PVOID;
  nLength: DWORD): BOOL; stdcall;
{$ENDIF}

type
  LPWNDCLASSEXA = ^WNDCLASSEXA;
  tagWNDCLASSEXA = record
    cbSize: UINT;
    // Win 3.x
    style: UINT;
    lpfnWndProc: WNDPROC;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINSTANCE;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: LPCSTR;
    lpszClassName: LPCSTR;
    // Win 4.0
    hIconSm: HICON;
  end;
  WNDCLASSEXA = tagWNDCLASSEXA;
  NPWNDCLASSEXA = ^WNDCLASSEXA;
  TWndClassExA = WNDCLASSEXA;
  PWndClassExA = LPWNDCLASSEXA;

  LPWNDCLASSEXW = ^WNDCLASSEXW;
  tagWNDCLASSEXW = record
    cbSize: UINT;
    // Win 3.x
    style: UINT;
    lpfnWndProc: WNDPROC;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINSTANCE;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: LPCWSTR;
    lpszClassName: LPCWSTR;
    // Win 4.0
    hIconSm: HICON;
  end;
  WNDCLASSEXW = tagWNDCLASSEXW;
  NPWNDCLASSEXW = ^WNDCLASSEXW;
  TWndClassExW = WNDCLASSEXW;
  PWndClassExW = LPWNDCLASSEXW;

{$IFDEF UNICODE}
  WNDCLASSEX = WNDCLASSEXW;
  NPWNDCLASSEX = NPWNDCLASSEXW;
  LPWNDCLASSEX = LPWNDCLASSEXW;
  TWndClassEx = TWndClassExW;
  PWndClassEx = PWndClassExW;
{$ELSE}
  WNDCLASSEX = WNDCLASSEXA;
  NPWNDCLASSEX = NPWNDCLASSEXA;
  LPWNDCLASSEX = LPWNDCLASSEXA;
  TWndClassEx = TWndClassExA;
  PWndClassEx = PWndClassExA;
{$ENDIF}

  LPWNDCLASSA = ^WNDCLASSA;
  tagWNDCLASSA = record
    style: UINT;
    lpfnWndProc: WNDPROC;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINSTANCE;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: LPCSTR;
    lpszClassName: LPCSTR;
  end;
  WNDCLASSA = tagWNDCLASSA;
  NPWNDCLASSA = ^WNDCLASSA;
  TWndClassA = WNDCLASSA;
  PWndClassA = LPWNDCLASSA;

  LPWNDCLASSW = ^WNDCLASSW;
  tagWNDCLASSW = record
    style: UINT;
    lpfnWndProc: WNDPROC;
    cbClsExtra: Integer;
    cbWndExtra: Integer;
    hInstance: HINSTANCE;
    hIcon: HICON;
    hCursor: HCURSOR;
    hbrBackground: HBRUSH;
    lpszMenuName: LPCWSTR;
    lpszClassName: LPCWSTR;
  end;
  WNDCLASSW = tagWNDCLASSW;
  NPWNDCLASSW = ^WNDCLASSW;
  TWndClassW = WNDCLASSW;
  PWndClassW = LPWNDCLASSW;

{$IFDEF UNICODE}
  WNDCLASS = WNDCLASSW;
  NPWNDCLASS = NPWNDCLASSW;
  LPWNDCLASS = LPWNDCLASSW;
  TWndClass = TWndClassW;
  PWndClass = PWndClassW;
{$ELSE}
  WNDCLASS = WNDCLASSA;
  NPWNDCLASS = NPWNDCLASSA;
  LPWNDCLASS = LPWNDCLASSA;
  TWndClass = TWndClassA;
  PWndClass = PWndClassA;
{$ENDIF}

function IsHungAppWindow(hwnd: HWND): BOOL; stdcall;

procedure DisableProcessWindowsGhosting; stdcall;

//
// Message structure
//

type
  LPMSG = ^MSG;
  tagMSG = record
    hwnd: HWND;
    message: UINT;
    wParam: WPARAM;
    lParam: LPARAM;
    time: DWORD;
    pt: POINT;
  end;
  MSG = tagMSG;
  NPMSG = ^MSG;
  TMsg = MSG;
  PMsg = LPMSG;

function MAKEWPARAM(wLow, wHigh: WORD): WPARAM;

function MAKELPARAM(wLow, wHigh: WORD): LPARAM;

function MAKELRESULT(wLow, wHigh: WORD): LRESULT;

//
// Window field offsets for GetWindowLong()
//

const
  GWL_WNDPROC    = -4;
  GWL_HINSTANCE  = -6;
  GWL_HWNDPARENT = -8;
  GWL_STYLE      = -16;
  GWL_EXSTYLE    = -20;
  GWL_USERDATA   = -21;
  GWL_ID         = -12;

  GWLP_WNDPROC    = -4;
  GWLP_HINSTANCE  = -6;
  GWLP_HWNDPARENT = -8;
  GWLP_USERDATA   = -21;
  GWLP_ID         = -12;

//
// Class field offsets for GetClassLong()
//

  GCL_MENUNAME      = DWORD(-8);
  GCL_HBRBACKGROUND = DWORD(-10);
  GCL_HCURSOR       = DWORD(-12);
  GCL_HICON         = DWORD(-14);
  GCL_HMODULE       = DWORD(-16);
  GCL_CBWNDEXTRA    = DWORD(-18);
  GCL_CBCLSEXTRA    = DWORD(-20);
  GCL_WNDPROC       = DWORD(-24);
  GCL_STYLE         = DWORD(-26);
  GCW_ATOM          = DWORD(-32);

  GCL_HICONSM = DWORD(-34);

  GCLP_MENUNAME      = DWORD(-8);
  GCLP_HBRBACKGROUND = DWORD(-10);
  GCLP_HCURSOR       = DWORD(-12);
  GCLP_HICON         = DWORD(-14);
  GCLP_HMODULE       = DWORD(-16);
  GCLP_WNDPROC       = DWORD(-24);
  GCLP_HICONSM       = DWORD(-34);

//
// Window Messages
//

  WM_NULL    = $0000;
  WM_CREATE  = $0001;
  WM_DESTROY = $0002;
  WM_MOVE    = $0003;
  WM_SIZE    = $0005;

  WM_ACTIVATE = $0006;

//
// WM_ACTIVATE state values
//

  WA_INACTIVE    = 0;
  WA_ACTIVE      = 1;
  WA_CLICKACTIVE = 2;

  WM_SETFOCUS        = $0007;
  WM_KILLFOCUS       = $0008;
  WM_ENABLE          = $000A;
  WM_SETREDRAW       = $000B;
  WM_SETTEXT         = $000C;
  WM_GETTEXT         = $000D;
  WM_GETTEXTLENGTH   = $000E;
  WM_PAINT           = $000F;
  WM_CLOSE           = $0010;
  WM_QUERYENDSESSION = $0011;
  WM_QUERYOPEN       = $0013;
  WM_ENDSESSION      = $0016;
  WM_QUIT            = $0012;
  WM_ERASEBKGND      = $0014;
  WM_SYSCOLORCHANGE  = $0015;
  WM_SHOWWINDOW      = $0018;
  WM_WININICHANGE    = $001A;
  WM_SETTINGCHANGE   = WM_WININICHANGE;

  WM_DEVMODECHANGE = $001B;
  WM_ACTIVATEAPP   = $001C;
  WM_FONTCHANGE    = $001D;
  WM_TIMECHANGE    = $001E;
  WM_CANCELMODE    = $001F;
  WM_SETCURSOR     = $0020;
  WM_MOUSEACTIVATE = $0021;
  WM_CHILDACTIVATE = $0022;
  WM_QUEUESYNC     = $0023;

  WM_GETMINMAXINFO = $0024;

//
// Struct pointed to by WM_GETMINMAXINFO lParam
//

type
  LPMINMAXINFO = ^MINMAXINFO;
  tagMINMAXINFO = record
    ptReserved: POINT;
    ptMaxSize: POINT;
    ptMaxPosition: POINT;
    ptMinTrackSize: POINT;
    ptMaxTrackSize: POINT;
  end;
  MINMAXINFO = tagMINMAXINFO;
  TMinMaxInfo = MINMAXINFO;
  PMinMaxInfo = LPMINMAXINFO;

const
  WM_PAINTICON         = $0026;
  WM_ICONERASEBKGND    = $0027;
  WM_NEXTDLGCTL        = $0028;
  WM_SPOOLERSTATUS     = $002A;
  WM_DRAWITEM          = $002B;
  WM_MEASUREITEM       = $002C;
  WM_DELETEITEM        = $002D;
  WM_VKEYTOITEM        = $002E;
  WM_CHARTOITEM        = $002F;
  WM_SETFONT           = $0030;
  WM_GETFONT           = $0031;
  WM_SETHOTKEY         = $0032;
  WM_GETHOTKEY         = $0033;
  WM_QUERYDRAGICON     = $0037;
  WM_COMPAREITEM       = $0039;
  WM_GETOBJECT         = $003D;
  WM_COMPACTING        = $0041;
  WM_COMMNOTIFY        = $0044; // no longer suported
  WM_WINDOWPOSCHANGING = $0046;
  WM_WINDOWPOSCHANGED  = $0047;

  WM_POWER = $0048;

//
// wParam for WM_POWER window message and DRV_POWER driver notification
//

  PWR_OK             = 1;
  PWR_FAIL           = DWORD(-1);
  PWR_SUSPENDREQUEST = 1;
  PWR_SUSPENDRESUME  = 2;
  PWR_CRITICALRESUME = 3;

  WM_COPYDATA      = $004A;
  WM_CANCELJOURNAL = $004B;

//
// lParam of WM_COPYDATA message points to...
//

type
  PCOPYDATASTRUCT = ^COPYDATASTRUCT;
  tagCOPYDATASTRUCT = record
    dwData: ULONG_PTR;
    cbData: DWORD;
    lpData: PVOID;
  end;
  COPYDATASTRUCT = tagCOPYDATASTRUCT;
  TCopyDataStruct = COPYDATASTRUCT;

  LPMDINEXTMENU = ^MDINEXTMENU;
  tagMDINEXTMENU = record
    hmenuIn: HMENU;
    hmenuNext: HMENU;
    hwndNext: HWND;
  end;
  MDINEXTMENU = tagMDINEXTMENU;
  TMdiNextMenu = MDINEXTMENU;
  PMdiNextMenu = LPMDINEXTMENU;

const
  WM_NOTIFY                 = $004E;
  WM_INPUTLANGCHANGEREQUEST = $0050;
  WM_INPUTLANGCHANGE        = $0051;
  WM_TCARD                  = $0052;
  WM_HELP                   = $0053;
  WM_USERCHANGED            = $0054;
  WM_NOTIFYFORMAT           = $0055;

  NFR_ANSI    = 1;
  NFR_UNICODE = 2;
  NF_QUERY    = 3;
  NF_REQUERY  = 4;

  WM_CONTEXTMENU   = $007B;
  WM_STYLECHANGING = $007C;
  WM_STYLECHANGED  = $007D;
  WM_DISPLAYCHANGE = $007E;
  WM_GETICON       = $007F;
  WM_SETICON       = $0080;

  WM_NCCREATE        = $0081;
  WM_NCDESTROY       = $0082;
  WM_NCCALCSIZE      = $0083;
  WM_NCHITTEST       = $0084;
  WM_NCPAINT         = $0085;
  WM_NCACTIVATE      = $0086;
  WM_GETDLGCODE      = $0087;
  WM_SYNCPAINT       = $0088;
  WM_NCMOUSEMOVE     = $00A0;
  WM_NCLBUTTONDOWN   = $00A1;
  WM_NCLBUTTONUP     = $00A2;
  WM_NCLBUTTONDBLCLK = $00A3;
  WM_NCRBUTTONDOWN   = $00A4;
  WM_NCRBUTTONUP     = $00A5;
  WM_NCRBUTTONDBLCLK = $00A6;
  WM_NCMBUTTONDOWN   = $00A7;
  WM_NCMBUTTONUP     = $00A8;
  WM_NCMBUTTONDBLCLK = $00A9;

  WM_NCXBUTTONDOWN   = $00AB;
  WM_NCXBUTTONUP     = $00AC;
  WM_NCXBUTTONDBLCLK = $00AD;

  WM_INPUT           = $00FF;

  WM_KEYFIRST    = $0100;
  WM_KEYDOWN     = $0100;
  WM_KEYUP       = $0101;
  WM_CHAR        = $0102;
  WM_DEADCHAR    = $0103;
  WM_SYSKEYDOWN  = $0104;
  WM_SYSKEYUP    = $0105;
  WM_SYSCHAR     = $0106;
  WM_SYSDEADCHAR = $0107;
{$IFDEF WIN32_WINNT_GREATER_EQUAL_0501}
  WM_UNICHAR     = $0109;
  WM_KEYLAST     = $0109;
  UNICODE_NOCHAR = $FFFF;
{$ELSE}
  WM_KEYLAST     = $0108;
{$ENDIF}

  WM_IME_STARTCOMPOSITION = $010D;
  WM_IME_ENDCOMPOSITION   = $010E;
  WM_IME_COMPOSITION      = $010F;
  WM_IME_KEYLAST          = $010F;

  WM_INITDIALOG      = $0110;
  WM_COMMAND         = $0111;
  WM_SYSCOMMAND      = $0112;
  WM_TIMER           = $0113;
  WM_HSCROLL         = $0114;
  WM_VSCROLL         = $0115;
  WM_INITMENU        = $0116;
  WM_INITMENUPOPUP   = $0117;
  WM_MENUSELECT      = $011F;
  WM_MENUCHAR        = $0120;
  WM_ENTERIDLE       = $0121;
  WM_MENURBUTTONUP   = $0122;
  WM_MENUDRAG        = $0123;
  WM_MENUGETOBJECT   = $0124;
  WM_UNINITMENUPOPUP = $0125;
  WM_MENUCOMMAND     = $0126;

  WM_CHANGEUISTATE = $0127;
  WM_UPDATEUISTATE = $0128;
  WM_QUERYUISTATE  = $0129;

//
// LOWORD(wParam) values in WM_*UISTATE*
//

  UIS_SET        = 1;
  UIS_CLEAR      = 2;
  UIS_INITIALIZE = 3;

//
// HIWORD(wParam) values in WM_*UISTATE*
//

  UISF_HIDEFOCUS = $1;
  UISF_HIDEACCEL = $2;
  UISF_ACTIVE    = $4;

  WM_CTLCOLORMSGBOX    = $0132;
  WM_CTLCOLOREDIT      = $0133;
  WM_CTLCOLORLISTBOX   = $0134;
  WM_CTLCOLORBTN       = $0135;
  WM_CTLCOLORDLG       = $0136;
  WM_CTLCOLORSCROLLBAR = $0137;
  WM_CTLCOLORSTATIC    = $0138;
  MN_GETHMENU          = $01E1;

  WM_MOUSEFIRST    = $0200;
  WM_MOUSEMOVE     = $0200;
  WM_LBUTTONDOWN   = $0201;
  WM_LBUTTONUP     = $0202;
  WM_LBUTTONDBLCLK = $0203;
  WM_RBUTTONDOWN   = $0204;
  WM_RBUTTONUP     = $0205;
  WM_RBUTTONDBLCLK = $0206;
  WM_MBUTTONDOWN   = $0207;
  WM_MBUTTONUP     = $0208;
  WM_MBUTTONDBLCLK = $0209;
  WM_MOUSEWHEEL    = $020A;
  WM_XBUTTONDOWN   = $020B;
  WM_XBUTTONUP     = $020C;
  WM_XBUTTONDBLCLK = $020D;

{$IFDEF WINNT_0500_GREATER}
  WM_MOUSELAST = $020D;
{$ELSE}
  {$IFDEF WINNT_0400_GREATER}
  WM_MOUSELAST = $020A;
  {$ELSE}
    {$IFDEF WINDOWS_0400_GREATER}
  WM_MOUSELAST = $020A;
    {$ELSE}
  WM_MOUSELAST = $0209;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

// Value for rolling one detent//

  WHEEL_DELTA                    = 120;

function GET_WHEEL_DELTA_WPARAM(wParam: WPARAM): SHORT;

// Setting to scroll one page for SPI_GET/SETWHEELSCROLLLINES//

const
  WHEEL_PAGESCROLL = UINT_MAX;

function GET_KEYSTATE_WPARAM(wParam: WPARAM): Integer;
function GET_NCHITTEST_WPARAM(wParam: WPARAM): Shortint;
function GET_XBUTTON_WPARAM(wParam: WPARAM): Integer;

// XButton values are WORD flags//

const
  XBUTTON1 = $0001;
  XBUTTON2 = $0002;

// Were there to be an XBUTTON3, it's value would be 0x0004//

  WM_PARENTNOTIFY  = $0210;
  WM_ENTERMENULOOP = $0211;
  WM_EXITMENULOOP  = $0212;

  WM_NEXTMENU       = $0213;
  WM_SIZING         = $0214;
  WM_CAPTURECHANGED = $0215;
  WM_MOVING         = $0216;

  WM_POWERBROADCAST = $0218;

  PBT_APMQUERYSUSPEND = $0000;
  PBT_APMQUERYSTANDBY = $0001;

  PBT_APMQUERYSUSPENDFAILED = $0002;
  PBT_APMQUERYSTANDBYFAILED = $0003;

  PBT_APMSUSPEND = $0004;
  PBT_APMSTANDBY = $0005;

  PBT_APMRESUMECRITICAL = $0006;
  PBT_APMRESUMESUSPEND  = $0007;
  PBT_APMRESUMESTANDBY  = $0008;

  PBTF_APMRESUMEFROMFAILURE = $00000001;

  PBT_APMBATTERYLOW        = $0009;
  PBT_APMPOWERSTATUSCHANGE = $000A;

  PBT_APMOEMEVENT = $000B;
  PBT_APMRESUMEAUTOMATIC = $0012;

  WM_DEVICECHANGE = $0219;

  WM_MDICREATE      = $0220;
  WM_MDIDESTROY     = $0221;
  WM_MDIACTIVATE    = $0222;
  WM_MDIRESTORE     = $0223;
  WM_MDINEXT        = $0224;
  WM_MDIMAXIMIZE    = $0225;
  WM_MDITILE        = $0226;
  WM_MDICASCADE     = $0227;
  WM_MDIICONARRANGE = $0228;
  WM_MDIGETACTIVE   = $0229;

  WM_MDISETMENU     = $0230;
  WM_ENTERSIZEMOVE  = $0231;
  WM_EXITSIZEMOVE   = $0232;
  WM_DROPFILES      = $0233;
  WM_MDIREFRESHMENU = $0234;

  WM_IME_SETCONTEXT      = $0281;
  WM_IME_NOTIFY          = $0282;
  WM_IME_CONTROL         = $0283;
  WM_IME_COMPOSITIONFULL = $0284;
  WM_IME_SELECT          = $0285;
  WM_IME_CHAR            = $0286;
  WM_IME_REQUEST         = $0288;
  WM_IME_KEYDOWN         = $0290;
  WM_IME_KEYUP           = $0291;

  WM_MOUSEHOVER   = $02A1;
  WM_MOUSELEAVE   = $02A3;
  WM_NCMOUSEHOVER = $02A0;
  WM_NCMOUSELEAVE = $02A2;

  WM_WTSSESSION_CHANGE = $02B1;

  WM_TABLET_FIRST      = $02c0;
  WM_TABLET_LAST       = $02df;

  WM_CUT               = $0300;
  WM_COPY              = $0301;
  WM_PASTE             = $0302;
  WM_CLEAR             = $0303;
  WM_UNDO              = $0304;
  WM_RENDERFORMAT      = $0305;
  WM_RENDERALLFORMATS  = $0306;
  WM_DESTROYCLIPBOARD  = $0307;
  WM_DRAWCLIPBOARD     = $0308;
  WM_PAINTCLIPBOARD    = $0309;
  WM_VSCROLLCLIPBOARD  = $030A;
  WM_SIZECLIPBOARD     = $030B;
  WM_ASKCBFORMATNAME   = $030C;
  WM_CHANGECBCHAIN     = $030D;
  WM_HSCROLLCLIPBOARD  = $030E;
  WM_QUERYNEWPALETTE   = $030F;
  WM_PALETTEISCHANGING = $0310;
  WM_PALETTECHANGED    = $0311;
  WM_HOTKEY            = $0312;

  WM_PRINT       = $0317;
  WM_PRINTCLIENT = $0318;

  WM_APPCOMMAND = $0319;

  WM_THEMECHANGED  = $031A;

  WM_HANDHELDFIRST = $0358;
  WM_HANDHELDLAST  = $035F;

  WM_AFXFIRST = $0360;
  WM_AFXLAST  = $037F;

  WM_PENWINFIRST = $0380;
  WM_PENWINLAST  = $038F;

  WM_APP = $8000;

//
// NOTE: All Message Numbers below 0x0400 are RESERVED.
//
// Private Window Messages Start Here:
//

  WM_USER = $0400;

//  wParam for WM_SIZING message

  WMSZ_LEFT        = 1;
  WMSZ_RIGHT       = 2;
  WMSZ_TOP         = 3;
  WMSZ_TOPLEFT     = 4;
  WMSZ_TOPRIGHT    = 5;
  WMSZ_BOTTOM      = 6;
  WMSZ_BOTTOMLEFT  = 7;
  WMSZ_BOTTOMRIGHT = 8;

//
// WM_NCHITTEST and MOUSEHOOKSTRUCT Mouse Position Codes
//

  HTERROR       = DWORD(-2);
  HTTRANSPARENT = DWORD(-1);
  HTNOWHERE     = 0;
  HTCLIENT      = 1;
  HTCAPTION     = 2;
  HTSYSMENU     = 3;
  HTGROWBOX     = 4;
  HTSIZE        = HTGROWBOX;
  HTMENU        = 5;
  HTHSCROLL     = 6;
  HTVSCROLL     = 7;
  HTMINBUTTON   = 8;
  HTMAXBUTTON   = 9;
  HTLEFT        = 10;
  HTRIGHT       = 11;
  HTTOP         = 12;
  HTTOPLEFT     = 13;
  HTTOPRIGHT    = 14;
  HTBOTTOM      = 15;
  HTBOTTOMLEFT  = 16;
  HTBOTTOMRIGHT = 17;
  HTBORDER      = 18;
  HTREDUCE      = HTMINBUTTON;
  HTZOOM        = HTMAXBUTTON;
  HTSIZEFIRST   = HTLEFT;
  HTSIZELAST    = HTBOTTOMRIGHT;
  HTOBJECT      = 19;
  HTCLOSE       = 20;
  HTHELP        = 21;

//
// SendMessageTimeout values
//

  SMTO_NORMAL             = $0000;
  SMTO_BLOCK              = $0001;
  SMTO_ABORTIFHUNG        = $0002;
  SMTO_NOTIMEOUTIFNOTHUNG = $0008;

//
// WM_MOUSEACTIVATE Return Codes
//

  MA_ACTIVATE         = 1;
  MA_ACTIVATEANDEAT   = 2;
  MA_NOACTIVATE       = 3;
  MA_NOACTIVATEANDEAT = 4;

//
// WM_SETICON / WM_GETICON Type Codes
//

  ICON_SMALL  = 0;
  ICON_BIG    = 1;
  ICON_SMALL2 = 2;

function RegisterWindowMessageA(lpString: LPCSTR): UINT; stdcall;
function RegisterWindowMessageW(lpString: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function RegisterWindowMessage(lpString: LPCWSTR): UINT; stdcall;
{$ELSE}
function RegisterWindowMessage(lpString: LPCSTR): UINT; stdcall;
{$ENDIF}

//
// WM_SIZE message wParam values
//

const
  SIZE_RESTORED  = 0;
  SIZE_MINIMIZED = 1;
  SIZE_MAXIMIZED = 2;
  SIZE_MAXSHOW   = 3;
  SIZE_MAXHIDE   = 4;

//
// Obsolete constant names
//

  SIZENORMAL     = SIZE_RESTORED;
  SIZEICONIC     = SIZE_MINIMIZED;
  SIZEFULLSCREEN = SIZE_MAXIMIZED;
  SIZEZOOMSHOW   = SIZE_MAXSHOW;
  SIZEZOOMHIDE   = SIZE_MAXHIDE;

//
// WM_WINDOWPOSCHANGING/CHANGED struct pointed to by lParam
//

type
  LPWINDOWPOS = ^WINDOWPOS;
  tagWINDOWPOS = record
    hwnd: HWND;
    hwndInsertAfter: HWND;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    flags: UINT;
  end;
  WINDOWPOS = tagWINDOWPOS;
  TWindowPos = WINDOWPOS;
  PWindowPos = LPWINDOWPOS;

//
// WM_NCCALCSIZE parameter structure
//

  LPNCCALCSIZE_PARAMS = ^NCCALCSIZE_PARAMS;
  NCCALCSIZE_PARAMS = record
    rgrc: array [0..2] of RECT;
    lppos: PWINDOWPOS;
  end;
  TNcCalcSizeParams = NCCALCSIZE_PARAMS;
  PNcCalcSizeParams = LPNCCALCSIZE_PARAMS;

//
// WM_NCCALCSIZE "window valid rect" return values
//

const
  WVR_ALIGNTOP    = $0010;
  WVR_ALIGNLEFT   = $0020;
  WVR_ALIGNBOTTOM = $0040;
  WVR_ALIGNRIGHT  = $0080;
  WVR_HREDRAW     = $0100;
  WVR_VREDRAW     = $0200;
  WVR_REDRAW      = (WVR_HREDRAW or WVR_VREDRAW);
  WVR_VALIDRECTS  = $0400;

//
// Key State Masks for Mouse Messages
//

  MK_LBUTTON  = $0001;
  MK_RBUTTON  = $0002;
  MK_SHIFT    = $0004;
  MK_CONTROL  = $0008;
  MK_MBUTTON  = $0010;
  MK_XBUTTON1 = $0020;
  MK_XBUTTON2 = $0040;

  TME_HOVER     = $00000001;
  TME_LEAVE     = $00000002;
  TME_NONCLIENT = $00000010;
  TME_QUERY     = $40000000;
  TME_CANCEL    = $80000000;

  HOVER_DEFAULT = $FFFFFFFF;

type
  LPTRACKMOUSEEVENT = ^_TRACKMOUSEEVENT;
  _TRACKMOUSEEVENT = record
    cbSize: DWORD;
    dwFlags: DWORD;
    hwndTrack: HWND;
    dwHoverTime: DWORD;
  end;
  //TRACKMOUSEEVENT = _TRACKMOUSEEVENT;
  //{$EXTERNALSYM TRACKMOUSEEVENT}
  TTrackMouseEvent = _TRACKMOUSEEVENT;
  PTrackMouseEvent = LPTRACKMOUSEEVENT;

function TrackMouseEvent(var lpEventTrack: TTrackMouseEvent): BOOL; stdcall;

//
// Window Styles
//

const
  WS_OVERLAPPED   = $00000000;
  WS_POPUP        = $80000000;
  WS_CHILD        = $40000000;
  WS_MINIMIZE     = $20000000;
  WS_VISIBLE      = $10000000;
  WS_DISABLED     = $08000000;
  WS_CLIPSIBLINGS = $04000000;
  WS_CLIPCHILDREN = $02000000;
  WS_MAXIMIZE     = $01000000;
  WS_CAPTION      = $00C00000; // WS_BORDER | WS_DLGFRAME
  WS_BORDER       = $00800000;
  WS_DLGFRAME     = $00400000;
  WS_VSCROLL      = $00200000;
  WS_HSCROLL      = $00100000;
  WS_SYSMENU      = $00080000;
  WS_THICKFRAME   = $00040000;
  WS_GROUP        = $00020000;
  WS_TABSTOP      = $00010000;

  WS_MINIMIZEBOX = $00020000;
  WS_MAXIMIZEBOX = $00010000;

  WS_TILED       = WS_OVERLAPPED;
  WS_ICONIC      = WS_MINIMIZE;
  WS_SIZEBOX     = WS_THICKFRAME;

//
// Common Window Styles
//

  WS_OVERLAPPEDWINDOW = (WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU or
                         WS_THICKFRAME or WS_MINIMIZEBOX or WS_MAXIMIZEBOX);

  WS_POPUPWINDOW = (WS_POPUP or WS_BORDER or WS_SYSMENU);

  WS_CHILDWINDOW = (WS_CHILD);

  WS_TILEDWINDOW = WS_OVERLAPPEDWINDOW;

//
// Extended Window Styles
//

  WS_EX_DLGMODALFRAME  = $00000001;
  WS_EX_NOPARENTNOTIFY = $00000004;
  WS_EX_TOPMOST        = $00000008;
  WS_EX_ACCEPTFILES    = $00000010;
  WS_EX_TRANSPARENT    = $00000020;
  WS_EX_MDICHILD       = $00000040;
  WS_EX_TOOLWINDOW     = $00000080;
  WS_EX_WINDOWEDGE     = $00000100;
  WS_EX_CLIENTEDGE     = $00000200;
  WS_EX_CONTEXTHELP    = $00000400;

  WS_EX_RIGHT          = $00001000;
  WS_EX_LEFT           = $00000000;
  WS_EX_RTLREADING     = $00002000;
  WS_EX_LTRREADING     = $00000000;
  WS_EX_LEFTSCROLLBAR  = $00004000;
  WS_EX_RIGHTSCROLLBAR = $00000000;
  WS_EX_CONTROLPARENT  = $00010000;
  WS_EX_STATICEDGE     = $00020000;
  WS_EX_APPWINDOW      = $00040000;

  WS_EX_OVERLAPPEDWINDOW = (WS_EX_WINDOWEDGE or WS_EX_CLIENTEDGE);
  WS_EX_PALETTEWINDOW    = (WS_EX_WINDOWEDGE or WS_EX_TOOLWINDOW or WS_EX_TOPMOST);

  WS_EX_LAYERED = $00080000;

  WS_EX_NOINHERITLAYOUT = $00100000; // Disable inheritence of mirroring by children
  WS_EX_LAYOUTRTL       = $00400000; // Right to left mirroring

  WS_EX_COMPOSITED = $02000000;
  WS_EX_NOACTIVATE = $08000000;

//
// Class styles
//

  CS_VREDRAW         = $0001;
  CS_HREDRAW         = $0002;
  CS_DBLCLKS         = $0008;
  CS_OWNDC           = $0020;
  CS_CLASSDC         = $0040;
  CS_PARENTDC        = $0080;
  CS_NOCLOSE         = $0200;
  CS_SAVEBITS        = $0800;
  CS_BYTEALIGNCLIENT = $1000;
  CS_BYTEALIGNWINDOW = $2000;
  CS_GLOBALCLASS     = $4000;

  CS_IME = $00010000;
  CS_DROPSHADOW = $00020000;

// WM_PRINT flags//

  PRF_CHECKVISIBLE = $00000001;
  PRF_NONCLIENT    = $00000002;
  PRF_CLIENT       = $00000004;
  PRF_ERASEBKGND   = $00000008;
  PRF_CHILDREN     = $00000010;
  PRF_OWNED        = $00000020;

// 3D border styles//

  BDR_RAISEDOUTER = $0001;
  BDR_SUNKENOUTER = $0002;
  BDR_RAISEDINNER = $0004;
  BDR_SUNKENINNER = $0008;

  BDR_OUTER  = (BDR_RAISEDOUTER or BDR_SUNKENOUTER);
  BDR_INNER  = (BDR_RAISEDINNER or BDR_SUNKENINNER);
  BDR_RAISED = (BDR_RAISEDOUTER or BDR_RAISEDINNER);
  BDR_SUNKEN = (BDR_SUNKENOUTER or BDR_SUNKENINNER);

  EDGE_RAISED = (BDR_RAISEDOUTER or BDR_RAISEDINNER);
  EDGE_SUNKEN = (BDR_SUNKENOUTER or BDR_SUNKENINNER);
  EDGE_ETCHED = (BDR_SUNKENOUTER or BDR_RAISEDINNER);
  EDGE_BUMP   = (BDR_RAISEDOUTER or BDR_SUNKENINNER);

// Border flags//

  BF_LEFT   = $0001;
  BF_TOP    = $0002;
  BF_RIGHT  = $0004;
  BF_BOTTOM = $0008;

  BF_TOPLEFT     = (BF_TOP or BF_LEFT);
  BF_TOPRIGHT    = (BF_TOP or BF_RIGHT);
  BF_BOTTOMLEFT  = (BF_BOTTOM or BF_LEFT);
  BF_BOTTOMRIGHT = (BF_BOTTOM or BF_RIGHT);
  BF_RECT        = (BF_LEFT or BF_TOP or BF_RIGHT or BF_BOTTOM);

  BF_DIAGONAL = $0010;

// For diagonal lines, the BF_RECT flags specify the end point of the
// vector bounded by the rectangle parameter.

  BF_DIAGONAL_ENDTOPRIGHT    = (BF_DIAGONAL or BF_TOP or BF_RIGHT);
  BF_DIAGONAL_ENDTOPLEFT     = (BF_DIAGONAL or BF_TOP or BF_LEFT);
  BF_DIAGONAL_ENDBOTTOMLEFT  = (BF_DIAGONAL or BF_BOTTOM or BF_LEFT);
  BF_DIAGONAL_ENDBOTTOMRIGHT = (BF_DIAGONAL or BF_BOTTOM or BF_RIGHT);

  BF_MIDDLE = $0800; // Fill in the middle
  BF_SOFT   = $1000; // For softer buttons
  BF_ADJUST = $2000; // Calculate the space left over
  BF_FLAT   = $4000; // For flat rather than 3D borders
  BF_MONO   = $8000; // For monochrome borders

function DrawEdge(hdc: HDC; var qrc: RECT; edge, grfFlags: UINT): BOOL; stdcall;

// flags for DrawFrameControl//

const
  DFC_CAPTION   = 1;
  DFC_MENU      = 2;
  DFC_SCROLL    = 3;
  DFC_BUTTON    = 4;
  DFC_POPUPMENU = 5;

  DFCS_CAPTIONCLOSE   = $0000;
  DFCS_CAPTIONMIN     = $0001;
  DFCS_CAPTIONMAX     = $0002;
  DFCS_CAPTIONRESTORE = $0003;
  DFCS_CAPTIONHELP    = $0004;

  DFCS_MENUARROW           = $0000;
  DFCS_MENUCHECK           = $0001;
  DFCS_MENUBULLET          = $0002;
  DFCS_MENUARROWRIGHT      = $0004;
  DFCS_SCROLLUP            = $0000;
  DFCS_SCROLLDOWN          = $0001;
  DFCS_SCROLLLEFT          = $0002;
  DFCS_SCROLLRIGHT         = $0003;
  DFCS_SCROLLCOMBOBOX      = $0005;
  DFCS_SCROLLSIZEGRIP      = $0008;
  DFCS_SCROLLSIZEGRIPRIGHT = $0010;

  DFCS_BUTTONCHECK      = $0000;
  DFCS_BUTTONRADIOIMAGE = $0001;
  DFCS_BUTTONRADIOMASK  = $0002;
  DFCS_BUTTONRADIO      = $0004;
  DFCS_BUTTON3STATE     = $0008;
  DFCS_BUTTONPUSH       = $0010;

  DFCS_INACTIVE = $0100;
  DFCS_PUSHED   = $0200;
  DFCS_CHECKED  = $0400;

  DFCS_TRANSPARENT = $0800;
  DFCS_HOT         = $1000;

  DFCS_ADJUSTRECT = $2000;
  DFCS_FLAT       = $4000;
  DFCS_MONO       = $8000;

function DrawFrameControl(hdc: HDC; const lprc: RECT; uType, uState: UINT): BOOL; stdcall;

// flags for DrawCaption//

const
  DC_ACTIVE   = $0001;
  DC_SMALLCAP = $0002;
  DC_ICON     = $0004;
  DC_TEXT     = $0008;
  DC_INBUTTON = $0010;
  DC_GRADIENT = $0020;
  DC_BUTTONS  = $1000;

function DrawCaption(hwnd: HWND; hdc: HDC; const lprc: RECT; uFlags: UINT): BOOL; stdcall;

const
  IDANI_OPEN = 1;
  IDANI_CAPTION = 3;

function DrawAnimatedRects(hwnd: HWND; idAni: Integer; const lprcFrom, lprcTo: RECT): BOOL; stdcall;

//
// Predefined Clipboard Formats
//

const
  CF_TEXT         = 1;
  CF_BITMAP       = 2;
  CF_METAFILEPICT = 3;
  CF_SYLK         = 4;
  CF_DIF          = 5;
  CF_TIFF         = 6;
  CF_OEMTEXT      = 7;
  CF_DIB          = 8;
  CF_PALETTE      = 9;
  CF_PENDATA      = 10;
  CF_RIFF         = 11;
  CF_WAVE         = 12;
  CF_UNICODETEXT  = 13;
  CF_ENHMETAFILE  = 14;
  CF_HDROP        = 15;
  CF_LOCALE       = 16;
  CF_DIBV5        = 17;

{$IFDEF WINVER_0500_GREATER}
  CF_MAX = 18;
{$ELSE}
  {$IFDEF WINVER_0400_GREATER}
  CF_MAX = 17;
  {$ELSE}
  CF_MAX = 15;
  {$ENDIF}
{$ENDIF}

  CF_OWNERDISPLAY    = $0080;
  CF_DSPTEXT         = $0081;
  CF_DSPBITMAP       = $0082;
  CF_DSPMETAFILEPICT = $0083;
  CF_DSPENHMETAFILE  = $008E;

//
// "Private" formats don't get GlobalFree()'d
//

  CF_PRIVATEFIRST = $0200;
  CF_PRIVATELAST  = $02FF;

//
// "GDIOBJ" formats do get DeleteObject()'d
//

  CF_GDIOBJFIRST = $0300;
  CF_GDIOBJLAST  = $03FF;

//
// Defines for the fVirt field of the Accelerator table structure.
//

  FVIRTKEY  = TRUE; // Assumed to be == TRUE
  FNOINVERT = $02;
  FSHIFT    = $04;
  FCONTROL  = $08;
  FALT      = $10;

type
  LPACCEL = ^ACCEL;
  tagACCEL = record
    fVirt: BYTE; // Also called the flags field//
    key: WORD;
    cmd: WORD;
  end;
  ACCEL = tagACCEL;
  TAccel = ACCEL;
  PAccel = LPACCEL;

  LPPAINTSTRUCT = ^PAINTSTRUCT;
  tagPAINTSTRUCT = record
    hdc: HDC;
    fErase: BOOL;
    rcPaint: RECT;
    fRestore: BOOL;
    fIncUpdate: BOOL;
    rgbReserved: array [0..31] of BYTE;
  end;
  PAINTSTRUCT = tagPAINTSTRUCT;
  NPPAINTSTRUCT = ^PAINTSTRUCT;
  TPaintStruct = PAINTSTRUCT;
  PPaintStruct = LPPAINTSTRUCT;

  LPCREATESTRUCTA = ^CREATESTRUCTA;
  tagCREATESTRUCTA = record
    lpCreateParams: LPVOID;
    hInstance: HINSTANCE;
    hMenu: HMENU;
    hwndParent: HWND;
    cy: Integer;
    cx: Integer;
    y: Integer;
    x: Integer;
    style: LONG;
    lpszName: LPCSTR;
    lpszClass: LPCSTR;
    dwExStyle: DWORD;
  end;
  CREATESTRUCTA = tagCREATESTRUCTA;
  TCreateStructA = CREATESTRUCTA;
  PCreateStructA = LPCREATESTRUCTA;

  LPCREATESTRUCTW = ^CREATESTRUCTW;
  tagCREATESTRUCTW = record
    lpCreateParams: LPVOID;
    hInstance: HINSTANCE;
    hMenu: HMENU;
    hwndParent: HWND;
    cy: Integer;
    cx: Integer;
    y: Integer;
    x: Integer;
    style: LONG;
    lpszName: LPCWSTR;
    lpszClass: LPCWSTR;
    dwExStyle: DWORD;
  end;
  CREATESTRUCTW = tagCREATESTRUCTW;
  TCreateStructW = CREATESTRUCTW;
  PCreateStructW = LPCREATESTRUCTW;

{$IFDEF UNICODE}
  CREATESTRUCT = CREATESTRUCTW;
  LPCREATESTRUCT = LPCREATESTRUCTW;
  TCreateStruct = TCreateStructW;
  PCreateStruct = PCreateStructW;
{$ELSE}
  CREATESTRUCT = CREATESTRUCTA;
  LPCREATESTRUCT = LPCREATESTRUCTA;
  TCreateStruct = TCreateStructA;
  PCreateStruct = PCreateStructA;
{$ENDIF}

//
// HCBT_CREATEWND parameters pointed to by lParam
//

type
  LPCBT_CREATEWNDA = ^CBT_CREATEWNDA;
  tagCBT_CREATEWNDA = record
    lpcs: LPCREATESTRUCTA;
    hwndInsertAfter: HWND;
  end;
  CBT_CREATEWNDA = tagCBT_CREATEWNDA;
  TCbtCreateWndA = CBT_CREATEWNDA;
  PCbtCreateWndA = LPCBT_CREATEWNDA;

//
// HCBT_CREATEWND parameters pointed to by lParam
//

  LPCBT_CREATEWNDW = ^CBT_CREATEWNDW;
  tagCBT_CREATEWNDW = record
    lpcs: LPCREATESTRUCTW;
    hwndInsertAfter: HWND;
  end;
  CBT_CREATEWNDW = tagCBT_CREATEWNDW;
  TCbtCreateWndW = CBT_CREATEWNDW;
  PCbtCreateWndW = LPCBT_CREATEWNDW;

{$IFDEF UNICODE}
  CBT_CREATEWND = CBT_CREATEWNDW;
  LPCBT_CREATEWND = LPCBT_CREATEWNDW;
{$ELSE}
  CBT_CREATEWND = CBT_CREATEWNDA;
  LPCBT_CREATEWND = LPCBT_CREATEWNDA;
{$ENDIF}

  LPWINDOWPLACEMENT = ^WINDOWPLACEMENT;
  tagWINDOWPLACEMENT = record
    length: UINT;
    flags: UINT;
    showCmd: UINT;
    ptMinPosition: POINT;
    ptMaxPosition: POINT;
    rcNormalPosition: RECT;
  end;
  WINDOWPLACEMENT = tagWINDOWPLACEMENT;
  TWindowPlacement = WINDOWPLACEMENT;
  PWindowPlacement = LPWINDOWPLACEMENT;

const
  WPF_SETMINPOSITION       = $0001;
  WPF_RESTORETOMAXIMIZED   = $0002;
  WPF_ASYNCWINDOWPLACEMENT = $0004;

type
  LPNMHDR = ^NMHDR;
  tagNMHDR = record
    hwndFrom: HWND;
    idFrom: UINT_PTR;
    code: UINT; // NM_ code
  end;
  NMHDR = tagNMHDR;
  TNmHdr = NMHDR;
  PNmHdr = LPNMHDR;

  LPSTYLESTRUCT = ^STYLESTRUCT;
  tagSTYLESTRUCT = record
    styleOld: DWORD;
    styleNew: DWORD;
  end;
  STYLESTRUCT = tagSTYLESTRUCT;
  TStyleStruct = STYLESTRUCT;
  PStyleStruct = LPSTYLESTRUCT;

//
// Owner draw control types
//

const
  ODT_MENU     = 1;
  ODT_LISTBOX  = 2;
  ODT_COMBOBOX = 3;
  ODT_BUTTON   = 4;
  ODT_STATIC   = 5;

//
// Owner draw actions
//

  ODA_DRAWENTIRE = $0001;
  ODA_SELECT     = $0002;
  ODA_FOCUS      = $0004;

//
// Owner draw state
//

  ODS_SELECTED     = $0001;
  ODS_GRAYED       = $0002;
  ODS_DISABLED     = $0004;
  ODS_CHECKED      = $0008;
  ODS_FOCUS        = $0010;
  ODS_DEFAULT      = $0020;
  ODS_COMBOBOXEDIT = $1000;
  ODS_HOTLIGHT     = $0040;
  ODS_INACTIVE     = $0080;
  ODS_NOACCEL      = $0100;
  ODS_NOFOCUSRECT  = $0200;

//
// MEASUREITEMSTRUCT for ownerdraw
//

type
  LPMEASUREITEMSTRUCT = ^MEASUREITEMSTRUCT;
  tagMEASUREITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    itemID: UINT;
    itemWidth: UINT;
    itemHeight: UINT;
    itemData: ULONG_PTR;
  end;
  MEASUREITEMSTRUCT = tagMEASUREITEMSTRUCT;
  TMeasureItemStruct = MEASUREITEMSTRUCT;
  PMeasureItemStruct = LPMEASUREITEMSTRUCT;

//
// DRAWITEMSTRUCT for ownerdraw
//

  LPDRAWITEMSTRUCT = ^DRAWITEMSTRUCT;
  tagDRAWITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    itemID: UINT;
    itemAction: UINT;
    itemState: UINT;
    hwndItem: HWND;
    hDC: HDC;
    rcItem: RECT;
    itemData: ULONG_PTR;
  end;
  DRAWITEMSTRUCT = tagDRAWITEMSTRUCT;
  TDrawItemStruct = DRAWITEMSTRUCT;
  PDrawItemStruct = LPDRAWITEMSTRUCT;

//
// DELETEITEMSTRUCT for ownerdraw
//

  LPDELETEITEMSTRUCT = ^DELETEITEMSTRUCT;
  tagDELETEITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    itemID: UINT;
    hwndItem: HWND;
    itemData: ULONG_PTR;
  end;
  DELETEITEMSTRUCT = tagDELETEITEMSTRUCT;
  TDeleteItemStruct = DELETEITEMSTRUCT;
  PDeleteItemStruct = LPDELETEITEMSTRUCT;

//
// COMPAREITEMSTUCT for ownerdraw sorting
//

  LPCOMPAREITEMSTRUCT = ^COMPAREITEMSTRUCT;
  tagCOMPAREITEMSTRUCT = record
    CtlType: UINT;
    CtlID: UINT;
    hwndItem: HWND;
    itemID1: UINT;
    itemData1: ULONG_PTR;
    itemID2: UINT;
    itemData2: ULONG_PTR;
    dwLocaleId: DWORD;
  end;
  COMPAREITEMSTRUCT = tagCOMPAREITEMSTRUCT;
  TCompareItemStruct = COMPAREITEMSTRUCT;
  PCompareItemStruct = LPCOMPAREITEMSTRUCT;

//
// Message Function Templates
//

function GetMessageA(lpMsg: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;
function GetMessageW(lpMsg: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function GetMessage(lpMsg: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;
{$ELSE}
function GetMessage(lpMsg: LPMSG; hWnd: HWND; wMsgFilterMin, wMsgFilterMax: UINT): BOOL; stdcall;
{$ENDIF}

function TranslateMessage(lpMsg: LPMSG): BOOL; stdcall;

function DispatchMessageA(lpMsg: LPMSG): LRESULT; stdcall;
function DispatchMessageW(lpMsg: LPMSG): LRESULT; stdcall;

{$IFDEF UNICODE}
function DispatchMessage(lpMsg: LPMSG): LRESULT; stdcall;
{$ELSE}
function DispatchMessage(lpMsg: LPMSG): LRESULT; stdcall;
{$ENDIF}

function SetMessageQueue(cMessagesMax: Integer): BOOL; stdcall;

function PeekMessageA(var lpMsg: MSG; hWnd: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;
function PeekMessageW(var lpMsg: MSG; hWnd: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function PeekMessage(var lpMsg: MSG; hWnd: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;
{$ELSE}
function PeekMessage(var lpMsg: MSG; hWnd: HWND;
  wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL; stdcall;
{$ENDIF}

//
// Queue status flags for GetQueueStatus() and MsgWaitForMultipleObjects()
//

const
  QS_KEY            = $0001;
  QS_MOUSEMOVE      = $0002;
  QS_MOUSEBUTTON    = $0004;
  QS_POSTMESSAGE    = $0008;
  QS_TIMER          = $0010;
  QS_PAINT          = $0020;
  QS_SENDMESSAGE    = $0040;
  QS_HOTKEY         = $0080;
  QS_ALLPOSTMESSAGE = $0100;
  QS_RAWINPUT       = $0400;

  QS_MOUSE = (QS_MOUSEMOVE or QS_MOUSEBUTTON);

  QS_INPUT = (QS_MOUSE or QS_KEY {$IFDEF WINDOWSXP}or QS_RAWINPUT{$ENDIF});

  QS_ALLEVENTS = (QS_INPUT or QS_POSTMESSAGE or QS_TIMER or QS_PAINT or QS_HOTKEY);

  QS_ALLINPUT = (QS_INPUT or QS_POSTMESSAGE or QS_TIMER or QS_PAINT or
    QS_HOTKEY or QS_SENDMESSAGE);

//
// PeekMessage() Options
//

const
  PM_NOREMOVE       = $0000;
  PM_REMOVE         = $0001;
  PM_NOYIELD        = $0002;
  PM_QS_INPUT       = (QS_INPUT shl 16);
  PM_QS_POSTMESSAGE = ((QS_POSTMESSAGE or QS_HOTKEY or QS_TIMER) shl 16);
  PM_QS_PAINT       = (QS_PAINT shl 16);
  PM_QS_SENDMESSAGE = (QS_SENDMESSAGE shl 16);

function RegisterHotKey(hWnd: HWND; id: Integer; fsModifiers, vk: UINT): BOOL; stdcall;

function UnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;

const
  MOD_ALT     = $0001;
  MOD_CONTROL = $0002;
  MOD_SHIFT   = $0004;
  MOD_WIN     = $0008;

  IDHOT_SNAPWINDOW  = DWORD(-1); // SHIFT-PRINTSCRN
  IDHOT_SNAPDESKTOP = DWORD(-2); // PRINTSCRN

const
  ENDSESSION_LOGOFF = DWORD($80000000);

  EWX_LOGOFF      = 0;
  EWX_SHUTDOWN    = $00000001;
  EWX_REBOOT      = $00000002;
  EWX_FORCE       = $00000004;
  EWX_POWEROFF    = $00000008;
  EWX_FORCEIFHUNG = $00000010;

function ExitWindows(dwReserved: DWORD; uREserved: UINT): BOOL;

function ExitWindowsEx(uFlags: UINT; dwReserved: DWORD): BOOL; stdcall;

function SwapMouseButton(fSwap: BOOL): BOOL; stdcall;

function GetMessagePos: DWORD; stdcall;

function GetMessageTime: LONG; stdcall;

function GetMessageExtraInfo: LPARAM; stdcall;

function SetMessageExtraInfo(lParam: LPARAM): LPARAM; stdcall;

function SendMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function SendMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function SendMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

function SendMessageTimeoutA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  fuFlags, uTimeout: UINT; var lpdwResult: DWORD_PTR): LRESULT; stdcall;
function SendMessageTimeoutW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  fuFlags, uTimeout: UINT; var lpdwResult: DWORD_PTR): LRESULT; stdcall;

{$IFDEF UNICODE}
function SendMessageTimeout(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  fuFlags, uTimeout: UINT; var lpdwResult: DWORD_PTR): LRESULT; stdcall;
{$ELSE}
function SendMessageTimeout(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  fuFlags, uTimeout: UINT; var lpdwResult: DWORD_PTR): LRESULT; stdcall;
{$ENDIF}

function SendNotifyMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
function SendNotifyMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;

{$IFDEF UNICODE}
function SendNotifyMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
{$ELSE}
function SendNotifyMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
{$ENDIF}

function SendMessageCallbackA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  lpResultCallBack: SENDASYNCPROC; dwData: ULONG_PTR): BOOL; stdcall;
function SendMessageCallbackW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  lpResultCallBack: SENDASYNCPROC; dwData: ULONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SendMessageCallback(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  lpResultCallBack: SENDASYNCPROC; dwData: ULONG_PTR): BOOL; stdcall;
{$ELSE}
function SendMessageCallback(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM;
  lpResultCallBack: SENDASYNCPROC; dwData: ULONG_PTR): BOOL; stdcall;
{$ENDIF}

type
  BSMINFO = record
    cbSize: UINT;
    hdesk: HDESK;
    hwnd: HWND;
    luid: LUID;
  end;
  PBSMINFO = ^BSMINFO;
  TBsmInfo = BSMINFO;

function BroadcastSystemMessageExA(dwFlags: DWORD; lpwRecipients: LPDWORD; uiMessage: UINT;
  wParam: WPARAM; lParam: LPARAM; pBSMInfo: PBSMINFO): Longint; stdcall;
function BroadcastSystemMessageExW(dwFlags: DWORD; lpwRecipients: LPDWORD; uiMessage: UINT;
  wParam: WPARAM; lParam: LPARAM; pBSMInfo: PBSMINFO): Longint; stdcall;

{$IFDEF UNICODE}
function BroadcastSystemMessageEx(dwFlags: DWORD; lpwRecipients: LPDWORD; uiMessage: UINT;
  wParam: WPARAM; lParam: LPARAM; pBSMInfo: PBSMINFO): Longint; stdcall;
{$ELSE}
function BroadcastSystemMessageEx(dwFlags: DWORD; lpwRecipients: LPDWORD; uiMessage: UINT;
  wParam: WPARAM; lParam: LPARAM; pBSMInfo: PBSMINFO): Longint; stdcall;
{$ENDIF}

function BroadcastSystemMessageA(dwFlags: DWORD; lpdwRecipients: LPDWORD;
  uiMessage: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;
function BroadcastSystemMessageW(dwFlags: DWORD; lpdwRecipients: LPDWORD;
  uiMessage: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;

{$IFDEF UNICODE}
function BroadcastSystemMessage(dwFlags: DWORD; lpdwRecipients: LPDWORD;
  uiMessage: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;
{$ELSE}
function BroadcastSystemMessage(dwFlags: DWORD; lpdwRecipients: LPDWORD;
  uiMessage: UINT; wParam: WPARAM; lParam: LPARAM): Longint; stdcall;
{$ENDIF}

//Broadcast Special Message Recipient list

const
  BSM_ALLCOMPONENTS      = $00000000;
  BSM_VXDS               = $00000001;
  BSM_NETDRIVER          = $00000002;
  BSM_INSTALLABLEDRIVERS = $00000004;
  BSM_APPLICATIONS       = $00000008;
  BSM_ALLDESKTOPS        = $00000010;

//Broadcast Special Message Flags

  BSF_QUERY              = $00000001;
  BSF_IGNORECURRENTTASK  = $00000002;
  BSF_FLUSHDISK          = $00000004;
  BSF_NOHANG             = $00000008;
  BSF_POSTMESSAGE        = $00000010;
  BSF_FORCEIFHUNG        = $00000020;
  BSF_NOTIMEOUTIFNOTHUNG = $00000040;
  BSF_ALLOWSFW           = $00000080;
  BSF_SENDNOTIFYMESSAGE  = $00000100;

  BSF_RETURNHDESK        = $00000200;
  BSF_LUID               = $00000400;

  BROADCAST_QUERY_DENY = $424D5144; // Return this value to deny a query.

// RegisterDeviceNotification

type
  HDEVNOTIFY = PVOID;
  PHDEVNOTIFY = ^HDEVNOTIFY;

const
  DEVICE_NOTIFY_WINDOW_HANDLE  = $00000000;
  DEVICE_NOTIFY_SERVICE_HANDLE = $00000001;
  DEVICE_NOTIFY_ALL_INTERFACE_CLASSES = $00000004;

function RegisterDeviceNotificationA(hRecipient: HANDLE; NotificationFilter: LPVOID;
  Flags: DWORD): HDEVNOTIFY; stdcall;
function RegisterDeviceNotificationW(hRecipient: HANDLE; NotificationFilter: LPVOID;
  Flags: DWORD): HDEVNOTIFY; stdcall;

{$IFDEF UNICODE}
function RegisterDeviceNotification(hRecipient: HANDLE; NotificationFilter: LPVOID;
  Flags: DWORD): HDEVNOTIFY; stdcall;
{$ELSE}
function RegisterDeviceNotification(hRecipient: HANDLE; NotificationFilter: LPVOID;
  Flags: DWORD): HDEVNOTIFY; stdcall;
{$ENDIF}

function UnregisterDeviceNotification(Handle: HDEVNOTIFY): BOOL; stdcall;

function PostMessageA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
function PostMessageW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;

{$IFDEF UNICODE}
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
{$ELSE}
function PostMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
{$ENDIF}

function PostThreadMessageA(idThread: DWORD; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
function PostThreadMessageW(idThread: DWORD; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;

{$IFDEF UNICODE}
function PostThreadMessage(idThread: DWORD; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
{$ELSE}
function PostThreadMessage(idThread: DWORD; Msg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL; stdcall;
{$ENDIF}

function PostAppMessageA(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;

function PostAppMessageW(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;

{$IFDEF UNICODE}
function PostAppMessage(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
{$ELSE}
function PostAppMessage(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
{$ENDIF}

//
// Special HWND value for use with PostMessage() and SendMessage()
//

const
  HWND_BROADCAST = HWND($ffff);

  HWND_MESSAGE = HWND(-3);

function AttachThreadInput(idAttach, idAttachTo: DWORD; fAttach: BOOL): BOOL; stdcall;

function ReplyMessage(lResult: LRESULT): BOOL; stdcall;

function WaitMessage: BOOL; stdcall;

function WaitForInputIdle(hProcess: HANDLE; dwMilliseconds: DWORD): DWORD; stdcall;

function DefWindowProcA(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function DefWindowProcW(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function DefWindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

procedure PostQuitMessage(nExitCode: Integer); stdcall;

function CallWindowProcA(lpPrevWndFunc: WNDPROC; hWnd: HWND; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function CallWindowProcW(lpPrevWndFunc: WNDPROC; hWnd: HWND; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function CallWindowProc(lpPrevWndFunc: WNDPROC; hWnd: HWND; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function CallWindowProc(lpPrevWndFunc: WNDPROC; hWnd: HWND; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

function InSendMessage: BOOL; stdcall;

function InSendMessageEx(lpReserved: LPVOID): DWORD; stdcall;

//
// InSendMessageEx return value
//

const
  ISMEX_NOSEND   = $00000000;
  ISMEX_SEND     = $00000001;
  ISMEX_NOTIFY   = $00000002;
  ISMEX_CALLBACK = $00000004;
  ISMEX_REPLIED  = $00000008;

function GetDoubleClickTime: UINT; stdcall;

function SetDoubleClickTime(uInterval: UINT): BOOL; stdcall;

function RegisterClassA(const lpWndClass: WNDCLASSA): ATOM; stdcall;
function RegisterClassW(const lpWndClass: WNDCLASSW): ATOM; stdcall;

{$IFDEF UNICODE}
function RegisterClass(const lpWndClass: WNDCLASSW): ATOM; stdcall;
{$ELSE}
function RegisterClass(const lpWndClass: WNDCLASSA): ATOM; stdcall;
{$ENDIF}

function UnregisterClassA(lpClassName: LPCSTR; hInstance: HINSTANCE): BOOL; stdcall;
function UnregisterClassW(lpClassName: LPCWSTR; hInstance: HINSTANCE): BOOL; stdcall;

{$IFDEF UNICODE}
function UnregisterClass(lpClassName: LPCWSTR; hInstance: HINSTANCE): BOOL; stdcall;
{$ELSE}
function UnregisterClass(lpClassName: LPCSTR; hInstance: HINSTANCE): BOOL; stdcall;
{$ENDIF}

function GetClassInfoA(hInstance: HINSTANCE; lpClassName: LPCSTR;
  var lpWndClass: WNDCLASSA): BOOL; stdcall;
function GetClassInfoW(hInstance: HINSTANCE; lpClassName: LPCWSTR;
  var lpWndClass: WNDCLASSW): BOOL; stdcall;

{$IFDEF UNICODE}
function GetClassInfo(hInstance: HINSTANCE; lpClassName: LPCWSTR;
  var lpWndClass: WNDCLASSW): BOOL; stdcall;
{$ELSE}
function GetClassInfo(hInstance: HINSTANCE; lpClassName: LPCSTR;
  var lpWndClass: WNDCLASSA): BOOL; stdcall;
{$ENDIF}

function RegisterClassExA(const lpwcx: WNDCLASSEXA): ATOM; stdcall;
function RegisterClassExW(const lpwcx: WNDCLASSEXW): ATOM; stdcall;

{$IFDEF UNICODE}
function RegisterClassEx(const lpwcx: WNDCLASSEXW): ATOM; stdcall;
{$ELSE}
function RegisterClassEx(const lpwcx: WNDCLASSEXA): ATOM; stdcall;
{$ENDIF}

function GetClassInfoExA(hinst: HINSTANCE; lpszClass: LPCSTR; var lpwcx: WNDCLASSEXA): BOOL; stdcall;
function GetClassInfoExW(hinst: HINSTANCE; lpszClass: LPCWSTR; var lpwcx: WNDCLASSEXW): BOOL; stdcall;

{$IFDEF UNICODE}
function GetClassInfoEx(hinst: HINSTANCE; lpszClass: LPCWSTR; var lpwcx: WNDCLASSEXW): BOOL; stdcall;
{$ELSE}
function GetClassInfoEx(hinst: HINSTANCE; lpszClass: LPCSTR; var lpwcx: WNDCLASSEXA): BOOL; stdcall;
{$ENDIF}

const
  CW_USEDEFAULT = Integer($80000000);

//
// Special value for CreateWindow, et al.
//

const
  HWND_DESKTOP = HWND(0);

type
  PREGISTERCLASSNAMEW = function (p: LPCWSTR): LongBool; stdcall;

function CreateWindowExA(dwExStyle: DWORD; lpClassName, lpWindowName: LPCSTR;
  dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND;
  hMenu: HMENU; hInstance: HINSTANCE; lpParam: LPVOID): HWND; stdcall;
function CreateWindowExW(dwExStyle: DWORD; lpClassName, lpWindowName: LPCWSTR;
  dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND;
  hMenu: HMENU; hInstance: HINSTANCE; lpParam: LPVOID): HWND; stdcall;

{$IFDEF UNICODE}
function CreateWindowEx(dwExStyle: DWORD; lpClassName, lpWindowName: LPCWSTR;
  dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND;
  hMenu: HMENU; hInstance: HINSTANCE; lpParam: LPVOID): HWND; stdcall;
{$ELSE}
function CreateWindowEx(dwExStyle: DWORD; lpClassName, lpWindowName: LPCSTR;
  dwStyle: DWORD; X, Y, nWidth, nHeight: Integer; hWndParent: HWND;
  hMenu: HMENU; hInstance: HINSTANCE; lpParam: LPVOID): HWND; stdcall;
{$ENDIF}

function CreateWindowA(lpClassName, lpWindowName: LPCSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;

function CreateWindowW(lpClassName, lpWindowName: LPCWSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;

{$IFDEF UNICODE}
function CreateWindow(lpClassName, lpWindowName: LPCWSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;
{$ELSE}
function CreateWindow(lpClassName, lpWindowName: LPCSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;
{$ENDIF}

function IsWindow(hWnd: HWND): BOOL; stdcall;

function IsMenu(hMenu: HMENU): BOOL; stdcall;

function IsChild(hWndParent, hWnd: HWND): BOOL; stdcall;

function DestroyWindow(hWnd: HWND): BOOL; stdcall;

function ShowWindow(hWnd: HWND; nCmdShow: Integer): BOOL; stdcall;

function AnimateWindow(hWnd: HWND; dwTime, dwFlags: DWORD): BOOL; stdcall;

function UpdateLayeredWindow(hWnd: HWND; hdcDst: HDC; pptDst: LPPOINT;
  psize: LPSIZE; hdcSrc: HDC; pptSrc: LPPOINT; crKey: COLORREF;
  pblend: LPBLENDFUNCTION; dwFlags: DWORD): BOOL; stdcall;

function GetLayeredWindowAttributes(hwnd: HWND; pcrKey: LPCOLORREF; pbAlpha: LPBYTE;
  pdwFlags: LPWORD): BOOL; stdcall;

const
  PW_CLIENTONLY = $00000001;

function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL; stdcall;

function SetLayeredWindowAttributes(hwnd: HWND; crKey: COLORREF; bAlpha: BYTE;
  dwFlags: DWORD): BOOL; stdcall;

const
  LWA_COLORKEY = $00000001;
  LWA_ALPHA    = $00000002;

  ULW_COLORKEY = $00000001;
  ULW_ALPHA    = $00000002;
  ULW_OPAQUE   = $00000004;

function ShowWindowAsync(hWnd: HWND; nCmdShow: Integer): BOOL; stdcall;

function FlashWindow(hWnd: HWND; bInvert: BOOL): BOOL; stdcall;

type
  PFLASH_INFO = ^FLASH_INFO;
  FLASH_INFO = record
    cbSize: UINT;
    hwnd: HWND;
    dwFlags: DWORD;
    uCount: UINT;
    dwTimeout: DWORD;
  end;
  TFlashInfo = FLASH_INFO;
  PFlashInfo = PFLASH_INFO;

function FlashWindowEx(var pfwi: FLASH_INFO): BOOL; stdcall;

const
  FLASHW_STOP      = 0;
  FLASHW_CAPTION   = $00000001;
  FLASHW_TRAY      = $00000002;
  FLASHW_ALL       = (FLASHW_CAPTION or FLASHW_TRAY);
  FLASHW_TIMER     = $00000004;
  FLASHW_TIMERNOFG = $0000000C;

function ShowOwnedPopups(hWnd: HWND; fShow: BOOL): BOOL; stdcall;

function OpenIcon(hWnd: HWND): BOOL; stdcall;

function CloseWindow(hWnd: HWND): BOOL; stdcall;

function MoveWindow(hWnd: HWND; X, Y, nWidth, nHeight: Integer; bRepaint: BOOL): BOOL; stdcall;

function SetWindowPos(hWnd, hWndInsertAfter: HWND; X, Y, cx, cy: Integer;
  uFlags: UINT): BOOL; stdcall;

function GetWindowPlacement(hWnd: HWND; var lpwndpl: WINDOWPLACEMENT): BOOL; stdcall;

function SetWindowPlacement(hWnd: HWND; const lpwndpl: WINDOWPLACEMENT): BOOL; stdcall;

function BeginDeferWindowPos(nNumWindows: Integer): HDWP; stdcall;

function DeferWindowPos(hWinPosInfo: HDWP; hWnd, hWndInsertAfter: HWND;
  x, y, cx, cy: Integer; uFlags: UINT): HDWP; stdcall;

function EndDeferWindowPos(hWinPosInfo: HDWP): BOOL; stdcall;

function IsWindowVisible(hWnd: HWND): BOOL; stdcall;

function IsIconic(hWnd: HWND): BOOL; stdcall;

function AnyPopup: BOOL; stdcall;

function BringWindowToTop(hWnd: HWND): BOOL; stdcall;

function IsZoomed(hWnd: HWND): BOOL; stdcall;

//
// SetWindowPos Flags
//

const
  SWP_NOSIZE         = $0001;
  SWP_NOMOVE         = $0002;
  SWP_NOZORDER       = $0004;
  SWP_NOREDRAW       = $0008;
  SWP_NOACTIVATE     = $0010;
  SWP_FRAMECHANGED   = $0020; // The frame changed: send WM_NCCALCSIZE
  SWP_SHOWWINDOW     = $0040;
  SWP_HIDEWINDOW     = $0080;
  SWP_NOCOPYBITS     = $0100;
  SWP_NOOWNERZORDER  = $0200; // Don't do owner Z ordering
  SWP_NOSENDCHANGING = $0400; // Don't send WM_WINDOWPOSCHANGING

  SWP_DRAWFRAME    = SWP_FRAMECHANGED;
  SWP_NOREPOSITION = SWP_NOOWNERZORDER;

  SWP_DEFERERASE     = $2000;
  SWP_ASYNCWINDOWPOS = $4000;

  HWND_TOP       = HWND(0);
  HWND_BOTTOM    = HWND(1);
  HWND_TOPMOST   = HWND(-1);
  HWND_NOTOPMOST = HWND(-2);

//
// WARNING:
// The following structures must NOT be DWORD padded because they are
// followed by strings, etc that do not have to be DWORD aligned.
//

// #include <pshpack2.h>

//
// original NT 32 bit dialog template:
//

type
  DLGTEMPLATE = packed record
    style: DWORD;
    dwExtendedStyle: DWORD;
    cdit: WORD;
    x: short;
    y: short;
    cx: short;
    cy: short;
  end;
  TDlgTemplate = DLGTEMPLATE;

  LPDLGTEMPLATEA = ^DLGTEMPLATE;
  LPDLGTEMPLATEW = ^DLGTEMPLATE;

  LPCDLGTEMPLATEA = ^DLGTEMPLATE;
  LPCDLGTEMPLATEW = ^DLGTEMPLATE;

{$IFDEF UNICODE}
  LPDLGTEMPLATE = LPDLGTEMPLATEW;
  LPCDLGTEMPLATE = LPCDLGTEMPLATEW;
{$ELSE}
  LPDLGTEMPLATE = LPDLGTEMPLATEA;
  LPCDLGTEMPLATE = LPCDLGTEMPLATEA;
{$ENDIF}

//
// 32 bit Dialog item template.
//

  DLGITEMTEMPLATE = packed record
    style: DWORD;
    dwExtendedStyle: DWORD;
    x: short;
    y: short;
    cx: short;
    cy: short;
    id: WORD;
  end;
  TDlgItemTemplate = DLGITEMTEMPLATE;

  PDLGITEMTEMPLATEA = ^DLGITEMTEMPLATE;
  PDLGITEMTEMPLATEW = ^DLGITEMTEMPLATE;

  LPDLGITEMTEMPLATEA = ^DLGITEMTEMPLATE;
  LPDLGITEMTEMPLATEW = ^DLGITEMTEMPLATE;

{$IFDEF UNICODE}
  PDLGITEMTEMPLATE = PDLGITEMTEMPLATEW;
  LPDLGITEMTEMPLATE = PDLGITEMTEMPLATEW;
{$ELSE}
  PDLGITEMTEMPLATE = PDLGITEMTEMPLATEA;
  LPDLGITEMTEMPLATE = PDLGITEMTEMPLATEA;
{$ENDIF}

// #include <poppack.h> // Resume normal packing//

function CreateDialogParamA(hInstance: HINSTANCE; lpTemplateName: LPCSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;
function CreateDialogParamW(hInstance: HINSTANCE; lpTemplateName: LPCWSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;

{$IFDEF UNICODE}
function CreateDialogParam(hInstance: HINSTANCE; lpTemplateName: LPCWSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;
{$ELSE}
function CreateDialogParam(hInstance: HINSTANCE; lpTemplateName: LPCSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;
{$ENDIF}

function CreateDialogIndirectParamA(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;
function CreateDialogIndirectParamW(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;

{$IFDEF UNICODE}
function CreateDialogIndirectParam(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;
{$ELSE}
function CreateDialogIndirectParam(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): HWND; stdcall;
{$ENDIF}

function CreateDialogA(hInstance: HINSTANCE; lpName: LPCSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): HWND;
function CreateDialogW(hInstance: HINSTANCE; lpName: LPCWSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): HWND;

{$IFDEF UNICODE}
function CreateDialog(hInstance: HINSTANCE; lpName: LPCWSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): HWND;
{$ELSE}
function CreateDialog(hInstance: HINSTANCE; lpName: LPCSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): HWND;
{$ENDIF}

function CreateDialogIndirectA(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
function CreateDialogIndirectW(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;

{$IFDEF UNICODE}
function CreateDialogIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
{$ELSE}
function CreateDialogIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
{$ENDIF}

function DialogBoxParamA(hInstance: HINSTANCE; lpTemplateName: LPCSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): INT_PTR; stdcall;
function DialogBoxParamW(hInstance: HINSTANCE; lpTemplateName: LPCWSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): INT_PTR; stdcall;

{$IFDEF UNICODE}
function DialogBoxParam(hInstance: HINSTANCE; lpTemplateName: LPCWSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): INT_PTR; stdcall;
{$ELSE}
function DialogBoxParam(hInstance: HINSTANCE; lpTemplateName: LPCSTR;
  hWndParent: HWND; lpDialogFunc: DLGPROC; dwInitParam: LPARAM): INT_PTR; stdcall;
{$ENDIF}

function DialogBoxIndirectParamA(hInstance: HINSTANCE;
  const hDialogTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC;
  dwInitParam: LPARAM): INT_PTR; stdcall;
function DialogBoxIndirectParamW(hInstance: HINSTANCE;
  const hDialogTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC;
  dwInitParam: LPARAM): INT_PTR; stdcall;

{$IFDEF UNICODE}
function DialogBoxIndirectParam(hInstance: HINSTANCE;
  const hDialogTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC;
  dwInitParam: LPARAM): INT_PTR; stdcall;
{$ELSE}
function DialogBoxIndirectParam(hInstance: HINSTANCE;
  const hDialogTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC;
  dwInitParam: LPARAM): INT_PTR; stdcall;
{$ENDIF}

function DialogBoxA(hInstance: HINSTANCE; lpTemplate: LPCSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): INT_PTR;

function DialogBoxW(hInstance: HINSTANCE; lpTemplate: LPCWSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): INT_PTR;

{$IFDEF UNICODE}
function DialogBox(hInstance: HINSTANCE; lpTemplate: LPCWSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): INT_PTR;
{$ELSE}
function DialogBox(hInstance: HINSTANCE; lpTemplate: LPCSTR; hWndParent: HWND;
  lpDialogFunc: DLGPROC): INT_PTR;
{$ENDIF}

function DialogBoxIndirectA(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;

function DialogBoxIndirectW(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;

{$IFDEF UNICODE}
function DialogBoxIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
{$ELSE}
function DialogBoxIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
{$ENDIF}

function EndDialog(hDlg: HWND; nResult: INT_PTR): BOOL; stdcall;

function GetDlgItem(hDlg: HWND; nIDDlgItem: Integer): HWND; stdcall;

function SetDlgItemInt(hDlg: HWND; nIDDlgItem: Integer; uValue: UINT; bSigned: BOOL): BOOL; stdcall;

function GetDlgItemInt(hDlg: HWND; nIDDlgItem: Integer; lpTranslated: LPBOOL;
  bSigned: BOOL): UINT; stdcall;

function SetDlgItemTextA(hDlg: HWND; nIDDlgItem: Integer; lpString: LPCSTR): BOOL; stdcall;
function SetDlgItemTextW(hDlg: HWND; nIDDlgItem: Integer; lpString: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetDlgItemText(hDlg: HWND; nIDDlgItem: Integer; lpString: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetDlgItemText(hDlg: HWND; nIDDlgItem: Integer; lpString: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetDlgItemTextA(hDlg: HWND; nIDDlgItem: Integer; lpString: LPSTR;
  nMaxCount: Integer): UINT; stdcall;
function GetDlgItemTextW(hDlg: HWND; nIDDlgItem: Integer; lpString: LPWSTR;
  nMaxCount: Integer): UINT; stdcall;

{$IFDEF UNICODE}
function GetDlgItemText(hDlg: HWND; nIDDlgItem: Integer; lpString: LPWSTR;
  nMaxCount: Integer): UINT; stdcall;
{$ELSE}
function GetDlgItemText(hDlg: HWND; nIDDlgItem: Integer; lpString: LPSTR;
  nMaxCount: Integer): UINT; stdcall;
{$ENDIF}

function CheckDlgButton(hDlg: HWND; nIDButton: Integer; uCheck: UINT): BOOL; stdcall;

function CheckRadioButton(hDlg: HWND; nIDFirstButton, nIDLastButton: Integer;
  nIDCheckButton: Integer): BOOL; stdcall;

function IsDlgButtonChecked(hDlg: HWND; nIDButton: Integer): UINT; stdcall;

function SendDlgItemMessageA(hDlg: HWND; nIDDlgItem: Integer; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function SendDlgItemMessageW(hDlg: HWND; nIDDlgItem: Integer; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function SendDlgItemMessage(hDlg: HWND; nIDDlgItem: Integer; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function SendDlgItemMessage(hDlg: HWND; nIDDlgItem: Integer; Msg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

function GetNextDlgGroupItem(hDlg: HWND; hCtl: HWND; bPrevious: BOOL): HWND; stdcall;

function GetNextDlgTabItem(hDlg: HWND; hCtl: HWND; bPrevious: BOOL): HWND; stdcall;

function GetDlgCtrlID(hWnd: HWND): Integer; stdcall;

function GetDialogBaseUnits: Longint; stdcall;

function DefDlgProcA(hDlg: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function DefDlgProcW(hDlg: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function DefDlgProc(hDlg: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function DefDlgProc(hDlg: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

//
// Window extra byted needed for private dialog classes.
//

const
  DLGWINDOWEXTRA = 30;

function CallMsgFilterA(lpMsg: LPMSG; nCode: Integer): BOOL; stdcall;
function CallMsgFilterW(lpMsg: LPMSG; nCode: Integer): BOOL; stdcall;

{$IFDEF UNICODE}
function CallMsgFilter(lpMsg: LPMSG; nCode: Integer): BOOL; stdcall;
{$ELSE}
function CallMsgFilter(lpMsg: LPMSG; nCode: Integer): BOOL; stdcall;
{$ENDIF}

//
// Clipboard Manager Functions
//

function OpenClipboard(hWndNewOwner: HWND): BOOL; stdcall;

function CloseClipboard: BOOL; stdcall;

function GetClipboardSequenceNumber: DWORD; stdcall;

function GetClipboardOwner: HWND; stdcall;

function SetClipboardViewer(hWndNewViewer: HWND): HWND; stdcall;

function GetClipboardViewer: HWND; stdcall;

function ChangeClipboardChain(hWndRemove, hWndNewNext: HWND): BOOL; stdcall;

function SetClipboardData(uFormat: UINT; hMem: HANDLE): HANDLE; stdcall;

function GetClipboardData(uFormat: UINT): HANDLE; stdcall;

function RegisterClipboardFormatA(lpszFormat: LPCSTR): UINT; stdcall;
function RegisterClipboardFormatW(lpszFormat: LPCWSTR): UINT; stdcall;

{$IFDEF UNICODE}
function RegisterClipboardFormat(lpszFormat: LPCWSTR): UINT; stdcall;
{$ELSE}
function RegisterClipboardFormat(lpszFormat: LPCSTR): UINT; stdcall;
{$ENDIF}

function CountClipboardFormats: Integer; stdcall;

function EnumClipboardFormats(format: UINT): UINT; stdcall;

function GetClipboardFormatNameA(format: UINT; lpszFormatName: LPSTR;
  cchMaxCount: Integer): Integer; stdcall;
function GetClipboardFormatNameW(format: UINT; lpszFormatName: LPWSTR;
  cchMaxCount: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetClipboardFormatName(format: UINT; lpszFormatName: LPWSTR;
  cchMaxCount: Integer): Integer; stdcall;
{$ELSE}
function GetClipboardFormatName(format: UINT; lpszFormatName: LPSTR;
  cchMaxCount: Integer): Integer; stdcall;
{$ENDIF}

function EmptyClipboard: BOOL; stdcall;

function IsClipboardFormatAvailable(format: UINT): BOOL; stdcall;

function GetPriorityClipboardFormat(paFormatPriorityList: PUINT; cFormats: Integer): Integer; stdcall;

function GetOpenClipboardWindow: HWND; stdcall;

//
// Character Translation Routines
//

function CharToOemA(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL; stdcall;
function CharToOemW(lpszSrc: LPCWSTR; lpszDst: LPSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function CharToOem(lpszSrc: LPCWSTR; lpszDst: LPSTR): BOOL; stdcall;
{$ELSE}
function CharToOem(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL; stdcall;
{$ENDIF}

function OemToCharA(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL; stdcall;
function OemToCharW(lpszSrc: LPCSTR; lpszDst: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function OemToChar(lpszSrc: LPCSTR; lpszDst: LPWSTR): BOOL; stdcall;
{$ELSE}
function OemToChar(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL; stdcall;
{$ENDIF}

function CharToOemBuffA(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall;
function CharToOemBuffW(lpszSrc: LPCWSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function CharToOemBuff(lpszSrc: LPCWSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall;
{$ELSE}
function CharToOemBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall;
{$ENDIF}

function OemToCharBuffA(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall;
function OemToCharBuffW(lpszSrc: LPCSTR; lpszDst: LPWSTR; cchDstLength: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function OemToCharBuff(lpszSrc: LPCSTR; lpszDst: LPWSTR; cchDstLength: DWORD): BOOL; stdcall;
{$ELSE}
function OemToCharBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL; stdcall;
{$ENDIF}

function CharUpperA(lpsz: LPSTR): LPSTR; stdcall;
function CharUpperW(lpsz: LPWSTR): LPWSTR; stdcall;

{$IFDEF UNICODE}
function CharUpper(lpsz: LPWSTR): LPWSTR; stdcall;
{$ELSE}
function CharUpper(lpsz: LPSTR): LPSTR; stdcall;
{$ENDIF}

function CharUpperBuffA(lpsz: LPSTR; cchLength: DWORD): DWORD; stdcall;
function CharUpperBuffW(lpsz: LPWSTR; cchLength: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function CharUpperBuff(lpsz: LPWSTR; cchLength: DWORD): DWORD; stdcall;
{$ELSE}
function CharUpperBuff(lpsz: LPSTR; cchLength: DWORD): DWORD; stdcall;
{$ENDIF}

function CharLowerA(lpsz: LPSTR): LPSTR; stdcall;
function CharLowerW(lpsz: LPWSTR): LPWSTR; stdcall;

{$IFDEF UNICODE}
function CharLower(lpsz: LPWSTR): LPWSTR; stdcall;
{$ELSE}
function CharLower(lpsz: LPSTR): LPSTR; stdcall;
{$ENDIF}

function CharLowerBuffA(lpsz: LPSTR; cchLength: DWORD): DWORD; stdcall;
function CharLowerBuffW(lpsz: LPWSTR; cchLength: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function CharLowerBuff(lpsz: LPWSTR; cchLength: DWORD): DWORD; stdcall;
{$ELSE}
function CharLowerBuff(lpsz: LPSTR; cchLength: DWORD): DWORD; stdcall;
{$ENDIF}

function CharNextA(lpsz: LPCSTR): LPSTR; stdcall;
function CharNextW(lpsz: LPCWSTR): LPWSTR; stdcall;

{$IFDEF UNICODE}
function CharNext(lpsz: LPCWSTR): LPWSTR; stdcall;
{$ELSE}
function CharNext(lpsz: LPCSTR): LPSTR; stdcall;
{$ENDIF}

function CharPrevA(lpszStart: LPCSTR; lpszCurrent: LPCSTR): LPSTR; stdcall;
function CharPrevW(lpszStart: LPCWSTR; lpszCurrent: LPCWSTR): LPWSTR; stdcall;

{$IFDEF UNICODE}
function CharPrev(lpszStart: LPCWSTR; lpszCurrent: LPCWSTR): LPWSTR; stdcall;
{$ELSE}
function CharPrev(lpszStart: LPCSTR; lpszCurrent: LPCSTR): LPSTR; stdcall;
{$ENDIF}

function CharNextExA(CodePage: WORD; lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR; stdcall;

function CharPrevExA(CodePage: WORD; lpStart, lpCurrentChar: LPCSTR; dwFlags: DWORD): LPSTR; stdcall;

//
// Compatibility defines for character translation routines
//

function AnsiToOem(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL;

function OemToAnsi(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL;

function AnsiToOemBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL;

function OemToAnsiBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL;

function AnsiUpper(lpsz: LPSTR): LPSTR;

function AnsiUpperBuff(lpsz: LPSTR; cchLength: DWORD): DWORD;

function AnsiLower(lpsz: LPSTR): LPSTR;

function AnsiLowerBuff(lpsz: LPSTR; cchLength: DWORD): DWORD;

function AnsiNext(lpsz: LPCSTR): LPSTR;

function AnsiPrev(lpszStart: LPCSTR; lpszCurrent: LPCSTR): LPSTR;

//
// Language dependent Routines
//

function IsCharAlphaA(ch: CHAR): BOOL; stdcall;
function IsCharAlphaW(ch: WCHAR): BOOL; stdcall;

{$IFDEF UNICODE}
function IsCharAlpha(ch: WCHAR): BOOL; stdcall;
{$ELSE}
function IsCharAlpha(ch: CHAR): BOOL; stdcall;
{$ENDIF}

function IsCharAlphaNumericA(ch: CHAR): BOOL; stdcall;
function IsCharAlphaNumericW(ch: WCHAR): BOOL; stdcall;

{$IFDEF UNICODE}
function IsCharAlphaNumeric(ch: WCHAR): BOOL; stdcall;
{$ELSE}
function IsCharAlphaNumeric(ch: CHAR): BOOL; stdcall;
{$ENDIF}

function IsCharUpperA(ch: CHAR): BOOL; stdcall;
function IsCharUpperW(ch: WCHAR): BOOL; stdcall;

{$IFDEF UNICODE}
function IsCharUpper(ch: WCHAR): BOOL; stdcall;
{$ELSE}
function IsCharUpper(ch: CHAR): BOOL; stdcall;
{$ENDIF}

function IsCharLowerA(ch: CHAR): BOOL; stdcall;
function IsCharLowerW(ch: WCHAR): BOOL; stdcall;

{$IFDEF UNICODE}
function IsCharLower(ch: WCHAR): BOOL; stdcall;
{$ELSE}
function IsCharLower(ch: CHAR): BOOL; stdcall;
{$ENDIF}

function SetFocus(hWnd: HWND): HWND; stdcall;

function GetActiveWindow: HWND; stdcall;

function GetFocus: HWND; stdcall;

function GetKBCodePage: UINT; stdcall;

function GetKeyState(nVirtKey: Integer): SHORT; stdcall;

function GetAsyncKeyState(vKey: Integer): SHORT; stdcall;

function GetKeyboardState(lpKeyState: LPBYTE): BOOL; stdcall;

function SetKeyboardState(lpKeyState: LPBYTE): BOOL; stdcall;

function GetKeyNameTextA(lParam: LONG; lpString: LPSTR; nSize: Integer): Integer; stdcall;
function GetKeyNameTextW(lParam: LONG; lpString: LPWSTR; nSize: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetKeyNameText(lParam: LONG; lpString: LPWSTR; nSize: Integer): Integer; stdcall;
{$ELSE}
function GetKeyNameText(lParam: LONG; lpString: LPSTR; nSize: Integer): Integer; stdcall;
{$ENDIF}

function GetKeyboardType(nTypeFlag: Integer): Integer; stdcall;

function ToAscii(uVirtKey, uScanCode: UINT; lpKeyState: PBYTE; lpChar: LPWORD;
  uFlags: UINT): Integer; stdcall;

function ToAsciiEx(uVirtKey, uScanCode: UINT; lpKeyState: PBYTE; lpChar: LPWORD;
  uFlags: UINT; dwhkl: HKL): Integer; stdcall;

function ToUnicode(wVirtKey, wScanCode: UINT; lpKeyState: PBYTE; pwszBuff: LPWSTR;
  cchBuff: Integer; wFlags: UINT): Integer; stdcall;

function OemKeyScan(wOemChar: WORD): DWORD; stdcall;

function VkKeyScanA(ch: CHAR): SHORT; stdcall;
function VkKeyScanW(ch: WCHAR): SHORT; stdcall;

{$IFDEF UNICODE}
function VkKeyScan(ch: WCHAR): SHORT; stdcall;
{$ELSE}
function VkKeyScan(ch: CHAR): SHORT; stdcall;
{$ENDIF}

function VkKeyScanExA(ch: CHAR; dwhkl: HKL): SHORT; stdcall;
function VkKeyScanExW(ch: WCHAR; dwhkl: HKL): SHORT; stdcall;

{$IFDEF UNICODE}
function VkKeyScanEx(ch: WCHAR; dwhkl: HKL): SHORT; stdcall;
{$ELSE}
function VkKeyScanEx(ch: CHAR; dwhkl: HKL): SHORT; stdcall;
{$ENDIF}

const
  KEYEVENTF_EXTENDEDKEY = $0001;
  KEYEVENTF_KEYUP       = $0002;

  KEYEVENTF_UNICODE  = $0004;
  KEYEVENTF_SCANCODE = $0008;

procedure keybd_event(bVk, bScan: BYTE; dwFlags: DWORD; dwExtraInfo: ULONG_PTR); stdcall;

const
  MOUSEEVENTF_MOVE        = $0001; // mouse move
  MOUSEEVENTF_LEFTDOWN    = $0002; // left button down
  MOUSEEVENTF_LEFTUP      = $0004; // left button up
  MOUSEEVENTF_RIGHTDOWN   = $0008; // right button down
  MOUSEEVENTF_RIGHTUP     = $0010; // right button up
  MOUSEEVENTF_MIDDLEDOWN  = $0020; // middle button down
  MOUSEEVENTF_MIDDLEUP    = $0040; // middle button up
  MOUSEEVENTF_XDOWN       = $0080; // x button down
  MOUSEEVENTF_XUP         = $0100; // x button down
  MOUSEEVENTF_WHEEL       = $0800; // wheel button rolled
  MOUSEEVENTF_VIRTUALDESK = $4000; // map to entire virtual desktop
  MOUSEEVENTF_ABSOLUTE    = $8000; // absolute move

procedure mouse_event(dwFlags, dx, dy, dwData: DWORD; dwExtraInfo: ULONG_PTR); stdcall;

type
  LPMOUSEINPUT = ^MOUSEINPUT;
  tagMOUSEINPUT = record
    dx: LONG;
    dy: LONG;
    mouseData: DWORD;
    dwFlags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  MOUSEINPUT = tagMOUSEINPUT;
  TMouseInput = MOUSEINPUT;
  PMouseInput = LPMOUSEINPUT;

  LPKEYBDINPUT = ^KEYBDINPUT;
  tagKEYBDINPUT = record
    wVk: WORD;
    wScan: WORD;
    dwFlags: DWORD;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;
  KEYBDINPUT = tagKEYBDINPUT;
  TKeybdinput = KEYBDINPUT;
  PKeybdInput = LPKEYBDINPUT;

  LPHARDWAREINPUT = ^HARDWAREINPUT;
  tagHARDWAREINPUT = record
    uMsg: DWORD;
    wParamL: WORD;
    wParamH: WORD;
  end;
  HARDWAREINPUT = tagHARDWAREINPUT;
  THardwareInput = HARDWAREINPUT;
  PHardwareInput = LPHARDWAREINPUT;

const
  INPUT_MOUSE    = 0;
  INPUT_KEYBOARD = 1;
  INPUT_HARDWARE = 2;

type
  LPINPUT = ^INPUT;
  tagINPUT = record
    type_: DWORD;
    case Integer of
      0: (mi: MOUSEINPUT);
      1: (ki: KEYBDINPUT);
      2: (hi: HARDWAREINPUT);
  end;
  INPUT = tagINPUT;
  TInput = INPUT;
  PInput = LPINPUT;

function SendInput(cInputs: UINT; pInputs: LPINPUT; cbSize: Integer): UINT; stdcall;

type
  PLASTINPUTINFO = ^LASTINPUTINFO;
  tagLASTINPUTINFO = record
    cbSize: UINT;
    dwTime: DWORD;
  end;
  LASTINPUTINFO = tagLASTINPUTINFO;
  TLastInputInfo = LASTINPUTINFO;

function GetLastInputInfo(var plii: LASTINPUTINFO): BOOL; stdcall;

function MapVirtualKeyA(uCode, uMapType: UINT): UINT; stdcall;
function MapVirtualKeyW(uCode, uMapType: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function MapVirtualKey(uCode, uMapType: UINT): UINT; stdcall;
{$ELSE}
function MapVirtualKey(uCode, uMapType: UINT): UINT; stdcall;
{$ENDIF}

function MapVirtualKeyExA(uCode, uMapType: UINT; dwhkl: HKL): UINT; stdcall;
function MapVirtualKeyExW(uCode, uMapType: UINT; dwhkl: HKL): UINT; stdcall;

{$IFDEF UNICODE}
function MapVirtualKeyEx(uCode, uMapType: UINT; dwhkl: HKL): UINT; stdcall;
{$ELSE}
function MapVirtualKeyEx(uCode, uMapType: UINT; dwhkl: HKL): UINT; stdcall;
{$ENDIF}

function GetInputState: BOOL; stdcall;

function GetQueueStatus(flags: UINT): DWORD; stdcall;

function GetCapture: HWND; stdcall;

function SetCapture(hWnd: HWND): HWND; stdcall;

function ReleaseCapture: BOOL; stdcall;

function MsgWaitForMultipleObjects(nCount: DWORD; pHandles: PHANDLE;
  fWaitAll: BOOL; dwMilliseconds: DWORD; dwWakeMask: DWORD): DWORD; stdcall;

function MsgWaitForMultipleObjectsEx(nCount: DWORD; pHandles: PHANDLE;
  dwMilliseconds: DWORD; dwWakeMask: DWORD; dwFlags: DWORD): DWORD; stdcall;

const
  MWMO_WAITALL        = $0001;
  MWMO_ALERTABLE      = $0002;
  MWMO_INPUTAVAILABLE = $0004;

//
// Windows Functions
//

function SetTimer(hWnd: HWND; nIDEvent: UINT_PTR; uElapse: UINT;
  lpTimerFunc: TIMERPROC): UINT_PTR; stdcall;

function KillTimer(hWnd: HWND; uIDEvent: UINT_PTR): BOOL; stdcall;

function IsWindowUnicode(hWnd: HWND): BOOL; stdcall;

function EnableWindow(hWnd: HWND; bEnable: BOOL): BOOL; stdcall;

function IsWindowEnabled(hWnd: HWND): BOOL; stdcall;

function LoadAcceleratorsA(hInstance: HINSTANCE; lpTableName: LPCSTR): HACCEL; stdcall;
function LoadAcceleratorsW(hInstance: HINSTANCE; lpTableName: LPCWSTR): HACCEL; stdcall;

{$IFDEF UNICODE}
function LoadAccelerators(hInstance: HINSTANCE; lpTableName: LPCWSTR): HACCEL; stdcall;
{$ELSE}
function LoadAccelerators(hInstance: HINSTANCE; lpTableName: LPCSTR): HACCEL; stdcall;
{$ENDIF}

function CreateAcceleratorTableA(lpaccl: LPACCEL; cEntries: Integer): HACCEL; stdcall;
function CreateAcceleratorTableW(lpaccl: LPACCEL; cEntries: Integer): HACCEL; stdcall;

{$IFDEF UNICODE}
function CreateAcceleratorTable(lpaccl: LPACCEL; cEntries: Integer): HACCEL; stdcall;
{$ELSE}
function CreateAcceleratorTable(lpaccl: LPACCEL; cEntries: Integer): HACCEL; stdcall;
{$ENDIF}

function DestroyAcceleratorTable(hAccel: HACCEL): BOOL; stdcall;

function CopyAcceleratorTableA(hAccelSrc: HACCEL; lpAccelDst: LPACCEL;
  cAccelEntries: Integer): Integer; stdcall;
function CopyAcceleratorTableW(hAccelSrc: HACCEL; lpAccelDst: LPACCEL;
  cAccelEntries: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function CopyAcceleratorTable(hAccelSrc: HACCEL; lpAccelDst: LPACCEL;
  cAccelEntries: Integer): Integer; stdcall;
{$ELSE}
function CopyAcceleratorTable(hAccelSrc: HACCEL; lpAccelDst: LPACCEL;
  cAccelEntries: Integer): Integer; stdcall;
{$ENDIF}

function TranslateAcceleratorA(hWnd: HWND; hAccTable: HACCEL; lpMsg: LPMSG): Integer; stdcall;
function TranslateAcceleratorW(hWnd: HWND; hAccTable: HACCEL; lpMsg: LPMSG): Integer; stdcall;

{$IFDEF UNICODE}
function TranslateAccelerator(hWnd: HWND; hAccTable: HACCEL; lpMsg: LPMSG): Integer; stdcall;
{$ELSE}
function TranslateAccelerator(hWnd: HWND; hAccTable: HACCEL; lpMsg: LPMSG): Integer; stdcall;
{$ENDIF}

//
// GetSystemMetrics() codes
//

const
  SM_CXSCREEN          = 0;
  SM_CYSCREEN          = 1;
  SM_CXVSCROLL         = 2;
  SM_CYHSCROLL         = 3;
  SM_CYCAPTION         = 4;
  SM_CXBORDER          = 5;
  SM_CYBORDER          = 6;
  SM_CXDLGFRAME        = 7;
  SM_CYDLGFRAME        = 8;
  SM_CYVTHUMB          = 9;
  SM_CXHTHUMB          = 10;
  SM_CXICON            = 11;
  SM_CYICON            = 12;
  SM_CXCURSOR          = 13;
  SM_CYCURSOR          = 14;
  SM_CYMENU            = 15;
  SM_CXFULLSCREEN      = 16;
  SM_CYFULLSCREEN      = 17;
  SM_CYKANJIWINDOW     = 18;
  SM_MOUSEPRESENT      = 19;
  SM_CYVSCROLL         = 20;
  SM_CXHSCROLL         = 21;
  SM_DEBUG             = 22;
  SM_SWAPBUTTON        = 23;
  SM_RESERVED1         = 24;
  SM_RESERVED2         = 25;
  SM_RESERVED3         = 26;
  SM_RESERVED4         = 27;
  SM_CXMIN             = 28;
  SM_CYMIN             = 29;
  SM_CXSIZE            = 30;
  SM_CYSIZE            = 31;
  SM_CXFRAME           = 32;
  SM_CYFRAME           = 33;
  SM_CXMINTRACK        = 34;
  SM_CYMINTRACK        = 35;
  SM_CXDOUBLECLK       = 36;
  SM_CYDOUBLECLK       = 37;
  SM_CXICONSPACING     = 38;
  SM_CYICONSPACING     = 39;
  SM_MENUDROPALIGNMENT = 40;
  SM_PENWINDOWS        = 41;
  SM_DBCSENABLED       = 42;
  SM_CMOUSEBUTTONS     = 43;

  SM_CXFIXEDFRAME = SM_CXDLGFRAME; // ;win40 name change
  SM_CYFIXEDFRAME = SM_CYDLGFRAME; // ;win40 name change
  SM_CXSIZEFRAME  = SM_CXFRAME; // ;win40 name change
  SM_CYSIZEFRAME  = SM_CYFRAME; // ;win40 name change

  SM_SECURE       = 44;
  SM_CXEDGE       = 45;
  SM_CYEDGE       = 46;
  SM_CXMINSPACING = 47;
  SM_CYMINSPACING = 48;
  SM_CXSMICON     = 49;
  SM_CYSMICON     = 50;
  SM_CYSMCAPTION  = 51;
  SM_CXSMSIZE     = 52;
  SM_CYSMSIZE     = 53;
  SM_CXMENUSIZE   = 54;
  SM_CYMENUSIZE   = 55;
  SM_ARRANGE      = 56;
  SM_CXMINIMIZED  = 57;
  SM_CYMINIMIZED  = 58;
  SM_CXMAXTRACK   = 59;
  SM_CYMAXTRACK   = 60;
  SM_CXMAXIMIZED  = 61;
  SM_CYMAXIMIZED  = 62;
  SM_NETWORK      = 63;
  SM_CLEANBOOT    = 67;
  SM_CXDRAG       = 68;
  SM_CYDRAG       = 69;
  SM_SHOWSOUNDS = 70;
  SM_CXMENUCHECK    = 71; // Use instead of GetMenuCheckMarkDimensions()!
  SM_CYMENUCHECK    = 72;
  SM_SLOWMACHINE    = 73;
  SM_MIDEASTENABLED = 74;

  SM_MOUSEWHEELPRESENT = 75;
  SM_XVIRTUALSCREEN    = 76;
  SM_YVIRTUALSCREEN    = 77;
  SM_CXVIRTUALSCREEN   = 78;
  SM_CYVIRTUALSCREEN   = 79;
  SM_CMONITORS         = 80;
  SM_SAMEDISPLAYFORMAT = 81;
  SM_IMMENABLED        = 82;
  SM_CXFOCUSBORDER     = 83;
  SM_CYFOCUSBORDER     = 84;

//#if(_WIN32_WINNT >= 0x0501)

  SM_TABLETPC          = 86;
  SM_MEDIACENTER       = 87;

//#endif /* _WIN32_WINNT >= 0x0501 */

{ TODO
#if (WINVER < 0x0500) && (!defined(_WIN32_WINNT) || (_WIN32_WINNT < 0x0400))
#define SM_CMETRICS             76
#elif WINVER == 0x500
#define SM_CMETRICS             83
#else
#define SM_CMETRICS             88
#endif
}

const
  SM_CMETRICS = 76;

  SM_REMOTESESSION = $1000;
  SM_SHUTTINGDOWN  = $2000;
//#if(WINVER >= 0x0501)
  SM_REMOTECONTROL = $2001;
//#endif /* WINVER >= 0x0501 */

function GetSystemMetrics(nIndex: Integer): Integer; stdcall;

function LoadMenuA(hInstance: HINSTANCE; lpMenuName: LPCSTR): HMENU; stdcall;
function LoadMenuW(hInstance: HINSTANCE; lpMenuName: LPCWSTR): HMENU; stdcall;

{$IFDEF UNICODE}
function LoadMenu(hInstance: HINSTANCE; lpMenuName: LPCWSTR): HMENU; stdcall;
{$ELSE}
function LoadMenu(hInstance: HINSTANCE; lpMenuName: LPCSTR): HMENU; stdcall;
{$ENDIF}

function LoadMenuIndirectA(lpMenuTemplate: LPMENUTEMPLATEA): HMENU; stdcall;
function LoadMenuIndirectW(lpMenuTemplate: LPMENUTEMPLATEW): HMENU; stdcall;

{$IFDEF UNICODE}
function LoadMenuIndirect(lpMenuTemplate: LPMENUTEMPLATEW): HMENU; stdcall;
{$ELSE}
function LoadMenuIndirect(lpMenuTemplate: LPMENUTEMPLATEA): HMENU; stdcall;
{$ENDIF}

function GetMenu(hWnd: HWND): HMENU; stdcall;

function SetMenu(hWnd: HWND; hMenu: HMENU): BOOL; stdcall;

function ChangeMenuA(hMenu: HMENU; cmd: UINT; lpszNewItem: LPCSTR;
  cmdInsert: UINT; flags: UINT): BOOL; stdcall;
function ChangeMenuW(hMenu: HMENU; cmd: UINT; lpszNewItem: LPCWSTR;
  cmdInsert: UINT; flags: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function ChangeMenu(hMenu: HMENU; cmd: UINT; lpszNewItem: LPCWSTR;
  cmdInsert: UINT; flags: UINT): BOOL; stdcall;
{$ELSE}
function ChangeMenu(hMenu: HMENU; cmd: UINT; lpszNewItem: LPCSTR;
  cmdInsert: UINT; flags: UINT): BOOL; stdcall;
{$ENDIF}

function HiliteMenuItem(hWnd: HWND; hMenu: HMENU; uIDHiliteItem: UINT; uHilite: UINT): BOOL; stdcall;

function GetMenuStringA(hMenu: HMENU; uIDItem: UINT; lpString: LPSTR;
  nMaxCount: Integer; uFlag: UINT): Integer; stdcall;
function GetMenuStringW(hMenu: HMENU; uIDItem: UINT; lpString: LPWSTR;
  nMaxCount: Integer; uFlag: UINT): Integer; stdcall;

{$IFDEF UNICODE}
function GetMenuString(hMenu: HMENU; uIDItem: UINT; lpString: LPWSTR;
  nMaxCount: Integer; uFlag: UINT): Integer; stdcall;
{$ELSE}
function GetMenuString(hMenu: HMENU; uIDItem: UINT; lpString: LPSTR;
  nMaxCount: Integer; uFlag: UINT): Integer; stdcall;
{$ENDIF}

function GetMenuState(hMenu: HMENU; uId, uFlags: UINT): UINT; stdcall;

function DrawMenuBar(hWnd: HWND): BOOL; stdcall;

const
  PMB_ACTIVE = $00000001;
  
function GetSystemMenu(hWnd: HWND; bRevert: BOOL): HMENU; stdcall;

function CreateMenu: HMENU; stdcall;

function CreatePopupMenu: HMENU; stdcall;

function DestroyMenu(hMenu: HMENU): BOOL; stdcall;

function CheckMenuItem(hMenu: HMENU; uIDCheckItem, uCheck: UINT): DWORD; stdcall;

function EnableMenuItem(hMenu: HMENU; uIDEnableItem, uEnable: UINT): BOOL; stdcall;

function GetSubMenu(hMenu: HMENU; nPos: Integer): HMENU; stdcall;

function GetMenuItemID(hMenu: HMENU; nPos: Integer): UINT; stdcall;

function GetMenuItemCount(hMenu: HMENU): Integer; stdcall;

function InsertMenuA(hMenu: HMENU; uPosition, uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCSTR): BOOL; stdcall;
function InsertMenuW(hMenu: HMENU; uPosition, uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function InsertMenu(hMenu: HMENU; uPosition, uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCWSTR): BOOL; stdcall;
{$ELSE}
function InsertMenu(hMenu: HMENU; uPosition, uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCSTR): BOOL; stdcall;
{$ENDIF}

function AppendMenuA(hMenu: HMENU; uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCSTR): BOOL; stdcall;
function AppendMenuW(hMenu: HMENU; uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function AppendMenu(hMenu: HMENU; uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCWSTR): BOOL; stdcall;
{$ELSE}
function AppendMenu(hMenu: HMENU; uFlags: UINT; uIDNewItem: UINT_PTR;
  lpNewItem: LPCSTR): BOOL; stdcall;
{$ENDIF}

function ModifyMenuA(hMnu: HMENU; uPosition: UINT; uFlags: UINT;
  uIDNewItem: UINT_PTR; lpNewItem: LPCSTR): BOOL; stdcall;
function ModifyMenuW(hMnu: HMENU; uPosition: UINT; uFlags: UINT;
  uIDNewItem: UINT_PTR; lpNewItem: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function ModifyMenu(hMnu: HMENU; uPosition: UINT; uFlags: UINT;
  uIDNewItem: UINT_PTR; lpNewItem: LPCWSTR): BOOL; stdcall;
{$ELSE}
function ModifyMenu(hMnu: HMENU; uPosition: UINT; uFlags: UINT;
  uIDNewItem: UINT_PTR; lpNewItem: LPCSTR): BOOL; stdcall;
{$ENDIF}

function RemoveMenu(hMenu: HMENU; uPosition: UINT; uFlags: UINT): BOOL; stdcall;

function DeleteMenu(hMenu: HMENU; uPosition: UINT; uFlags: UINT): BOOL; stdcall;

function SetMenuItemBitmaps(hMenu: HMENU; uPosition: UINT; uFlags: UINT;
  hBitmapUnchecked: HBITMAP; hBitmapChecked: HBITMAP): BOOL; stdcall;

function GetMenuCheckMarkDimensions: LONG; stdcall;

function TrackPopupMenu(hMenu: HMENU; uFlags: UINT; x: Integer; y: Integer;
  nReserved: Integer; hWnd: HWND; prcRect: LPRECT): BOOL; stdcall;

// return codes for WM_MENUCHAR//

const
  MNC_IGNORE  = 0;
  MNC_CLOSE   = 1;
  MNC_EXECUTE = 2;
  MNC_SELECT  = 3;

type
  LPTPMPARAMS = ^TPMPARAMS;
  tagTPMPARAMS = record
    cbSize: UINT;    // Size of structure
    rcExclude: RECT; // Screen coordinates of rectangle to exclude when positioning
  end;
  TPMPARAMS = tagTPMPARAMS;
  TTPMParams = TPMPARAMS;
  PTPMParams = LPTPMPARAMS;

function TrackPopupMenuEx(hmenu: HMENU; fuflags: UINT; x, y: Integer;
  hwnd: HWND; lptpm: LPTPMPARAMS): BOOL; stdcall;

const
  MNS_NOCHECK     = $80000000;
  MNS_MODELESS    = $40000000;
  MNS_DRAGDROP    = $20000000;
  MNS_AUTODISMISS = $10000000;
  MNS_NOTIFYBYPOS = $08000000;
  MNS_CHECKORBMP  = $04000000;

  MIM_MAXHEIGHT       = $00000001;
  MIM_BACKGROUND      = $00000002;
  MIM_HELPID          = $00000004;
  MIM_MENUDATA        = $00000008;
  MIM_STYLE           = $00000010;
  MIM_APPLYTOSUBMENUS = $80000000;

type
  LPMENUINFO = ^MENUINFO;
  tagMENUINFO = record
    cbSize: DWORD;
    fMask: DWORD;
    dwStyle: DWORD;
    cyMax: UINT;
    hbrBack: HBRUSH;
    dwContextHelpID: DWORD;
    dwMenuData: ULONG_PTR;
  end;
  MENUINFO = tagMENUINFO;
  TMenuinfo = MENUINFO;
  PMenuinfo = LPMENUINFO;

  LPCMENUINFO = ^MENUINFO;

function GetMenuInfo(hmenu: HMENU; var lpcmi: MENUINFO): BOOL; stdcall;

function SetMenuInfo(hemnu: HMENU; const lpcmi: MENUINFO): BOOL; stdcall;

function EndMenu: BOOL; stdcall;

//
// WM_MENUDRAG return values.
//

const
  MND_CONTINUE = 0;
  MND_ENDMENU  = 1;

type
  PMENUGETOBJECTINFO = ^MENUGETOBJECTINFO;
  tagMENUGETOBJECTINFO = record
    dwFlags: DWORD;
    uPos: UINT;
    hmenu: HMENU;
    riid: PVOID;
    pvObj: PVOID;
  end;
  MENUGETOBJECTINFO = tagMENUGETOBJECTINFO;
  TMenuGetObjectInfo = MENUGETOBJECTINFO;

//
// MENUGETOBJECTINFO dwFlags values
//

const
  MNGOF_TOPGAP    = $00000001;
  MNGOF_BOTTOMGAP = $00000002;

//
// WM_MENUGETOBJECT return values
//

  MNGO_NOINTERFACE = $00000000;
  MNGO_NOERROR     = $00000001;

  MIIM_STATE      = $00000001;
  MIIM_ID         = $00000002;
  MIIM_SUBMENU    = $00000004;
  MIIM_CHECKMARKS = $00000008;
  MIIM_TYPE       = $00000010;
  MIIM_DATA       = $00000020;

  MIIM_STRING = $00000040;
  MIIM_BITMAP = $00000080;
  MIIM_FTYPE  = $00000100;

  HBMMENU_CALLBACK        = HBITMAP(-1);
  HBMMENU_SYSTEM          = HBITMAP(1);
  HBMMENU_MBAR_RESTORE    = HBITMAP(2);
  HBMMENU_MBAR_MINIMIZE   = HBITMAP(3);
  HBMMENU_MBAR_CLOSE      = HBITMAP(5);
  HBMMENU_MBAR_CLOSE_D    = HBITMAP(6);
  HBMMENU_MBAR_MINIMIZE_D = HBITMAP(7);
  HBMMENU_POPUP_CLOSE     = HBITMAP(8);
  HBMMENU_POPUP_RESTORE   = HBITMAP(9);
  HBMMENU_POPUP_MAXIMIZE  = HBITMAP(10);
  HBMMENU_POPUP_MINIMIZE  = HBITMAP(11);

type
  LPMENUITEMINFOA = ^MENUITEMINFOA;
  tagMENUITEMINFOA = record
    cbSize: UINT;
    fMask: UINT;
    fType: UINT;            // used if MIIM_TYPE (4.0) or MIIM_FTYPE (>4.0)
    fState: UINT;           // used if MIIM_STATE
    wID: UINT;              // used if MIIM_ID
    hSubMenu: HMENU;        // used if MIIM_SUBMENU
    hbmpChecked: HBITMAP;   // used if MIIM_CHECKMARKS
    hbmpUnchecked: HBITMAP; // used if MIIM_CHECKMARKS
    dwItemData: ULONG_PTR;  // used if MIIM_DATA
    dwTypeData: LPSTR;      // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    cch: UINT;              // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    {$IFDEF WINVER_0500_GREATER}
    hbmpItem: HBITMAP;      // used if MIIM_BITMAP
    {$ENDIF}
  end;
  MENUITEMINFOA = tagMENUITEMINFOA;
  TMenuItemInfoA = MENUITEMINFOA;
  PMenuItemInfoA = LPMENUITEMINFOA;

  LPMENUITEMINFOW = ^MENUITEMINFOW;
  tagMENUITEMINFOW = record
    cbSize: UINT;
    fMask: UINT;
    fType: UINT;            // used if MIIM_TYPE (4.0) or MIIM_FTYPE (>4.0)
    fState: UINT;           // used if MIIM_STATE
    wID: UINT;              // used if MIIM_ID
    hSubMenu: HMENU;        // used if MIIM_SUBMENU
    hbmpChecked: HBITMAP;   // used if MIIM_CHECKMARKS
    hbmpUnchecked: HBITMAP; // used if MIIM_CHECKMARKS
    dwItemData: ULONG_PTR;  // used if MIIM_DATA
    dwTypeData: LPWSTR;     // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    cch: UINT;              // used if MIIM_TYPE (4.0) or MIIM_STRING (>4.0)
    {$IFDEF WINVER_0500_GREATER}
    hbmpItem: HBITMAP;      // used if MIIM_BITMAP
    {$ENDIF}
  end;
  MENUITEMINFOW = tagMENUITEMINFOW;
  TMenuItemInfoW = MENUITEMINFOW;
  PMenuItemInfoW = LPMENUITEMINFOW;

  LPCMENUITEMINFOA = ^MENUITEMINFOA;
  LPCMENUITEMINFOW = ^MENUITEMINFOW;

{$IFDEF UNICODE}
  MENUITEMINFO = MENUITEMINFOW;
  LPMENUITEMINFO = LPMENUITEMINFOW;
  TMenuItemInfo = TMenuItemInfoW;
  PMenuItemInfo = PMenuItemInfoW;
  LPCMENUITEMINFO = LPCMENUITEMINFOW;
{$ELSE}
  MENUITEMINFO = MENUITEMINFOA;
  LPMENUITEMINFO = LPMENUITEMINFOA;
  TMenuItemInfo = TMenuItemInfoA;
  PMenuItemInfo = PMenuItemInfoA;
  LPCMENUITEMINFO = LPCMENUITEMINFOA;
{$ENDIF}

function InsertMenuItemA(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOA): BOOL; stdcall;
function InsertMenuItemW(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOW): BOOL; stdcall;

{$IFDEF UNICODE}
function InsertMenuItem(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOW): BOOL; stdcall;
{$ELSE}
function InsertMenuItem(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOA): BOOL; stdcall;
{$ENDIF}

function GetMenuItemInfoA(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  var lpmii: MENUITEMINFOA): BOOL; stdcall;
function GetMenuItemInfoW(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  var lpmii: MENUITEMINFOW): BOOL; stdcall;

{$IFDEF UNICODE}
function GetMenuItemInfo(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  var lpmii: MENUITEMINFOW): BOOL; stdcall;
{$ELSE}
function GetMenuItemInfo(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  var lpmii: MENUITEMINFOA): BOOL; stdcall;
{$ENDIF}

function SetMenuItemInfoA(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOA): BOOL; stdcall;
function SetMenuItemInfoW(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOW): BOOL; stdcall;

{$IFDEF UNICODE}
function SetMenuItemInfo(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOW): BOOL; stdcall;
{$ELSE}
function SetMenuItemInfo(hMenu: HMENU; uItem: UINT; fByPosition: BOOL;
  const lpmii: MENUITEMINFOA): BOOL; stdcall;
{$ENDIF}

const
  GMDI_USEDISABLED  = $0001;
  GMDI_GOINTOPOPUPS = $0002;

function GetMenuDefaultItem(hMenu: HMENU; fByPos, gmdiFlags: UINT): UINT; stdcall;

function SetMenuDefaultItem(hMenu: HMENU; uItem, fByPos: UINT): BOOL; stdcall;

function GetMenuItemRect(hWnd: HWND; hMenu: HMENU; uItem: UINT; var lprcItem: RECT): BOOL; stdcall;

function MenuItemFromPoint(hWnd: HWND; hMenu: HMENU; ptScreen: POINT): Integer; stdcall;

//
// Flags for TrackPopupMenu
//

const
  TPM_LEFTBUTTON   = $0000;
  TPM_RIGHTBUTTON  = $0002;
  TPM_LEFTALIGN    = $0000;
  TPM_CENTERALIGN  = $0004;
  TPM_RIGHTALIGN   = $0008;
  TPM_TOPALIGN     = $0000;
  TPM_VCENTERALIGN = $0010;
  TPM_BOTTOMALIGN  = $0020;

  TPM_HORIZONTAL      = $0000; // Horz alignment matters more
  TPM_VERTICAL        = $0040; // Vert alignment matters more
  TPM_NONOTIFY        = $0080; // Don't send any notification msgs
  TPM_RETURNCMD       = $0100;
  TPM_RECURSE         = $0001;
  TPM_HORPOSANIMATION = $0400;
  TPM_HORNEGANIMATION = $0800;
  TPM_VERPOSANIMATION = $1000;
  TPM_VERNEGANIMATION = $2000;
  TPM_NOANIMATION     = $4000;
  TPM_LAYOUTRTL       = $8000;
  
//
// Drag-and-drop support
// Obsolete - use OLE instead
//

type
  LPDROPSTRUCT = ^DROPSTRUCT;
  tagDROPSTRUCT = record
    hwndSource: HWND;
    hwndSink: HWND;
    wFmt: DWORD;
    dwData: ULONG_PTR;
    ptDrop: POINT;
    dwControlData: DWORD;
  end;
  DROPSTRUCT = tagDROPSTRUCT;
  TDropStruct = DROPSTRUCT;
  PDropStruct = LPDROPSTRUCT;

const
  DOF_EXECUTABLE = $8001; // wFmt flags
  DOF_DOCUMENT   = $8002;
  DOF_DIRECTORY  = $8003;
  DOF_MULTIPLE   = $8004;
  DOF_PROGMAN    = $0001;
  DOF_SHELLDATA  = $0002;

  DO_DROPFILE  = $454C4946;
  DO_PRINTFILE = $544E5250;

function DragObject(hwnd1, hwnd2: HWND; i: UINT; u: ULONG_PTR; hcursor: HCURSOR): DWORD; stdcall;

function DragDetect(hwnd: HWND; pt: POINT): BOOL; stdcall;

function DrawIcon(hDC: HDC; X, Y: Integer; hIcon: HICON): BOOL; stdcall;

//
// DrawText() Format Flags
//

const
  DT_TOP             = $00000000;
  DT_LEFT            = $00000000;
  DT_CENTER          = $00000001;
  DT_RIGHT           = $00000002;
  DT_VCENTER         = $00000004;
  DT_BOTTOM          = $00000008;
  DT_WORDBREAK       = $00000010;
  DT_SINGLELINE      = $00000020;
  DT_EXPANDTABS      = $00000040;
  DT_TABSTOP         = $00000080;
  DT_NOCLIP          = $00000100;
  DT_EXTERNALLEADING = $00000200;
  DT_CALCRECT        = $00000400;
  DT_NOPREFIX        = $00000800;
  DT_INTERNAL        = $00001000;

  DT_EDITCONTROL          = $00002000;
  DT_PATH_ELLIPSIS        = $00004000;
  DT_END_ELLIPSIS         = $00008000;
  DT_MODIFYSTRING         = $00010000;
  DT_RTLREADING           = $00020000;
  DT_WORD_ELLIPSIS        = $00040000;
  DT_NOFULLWIDTHCHARBREAK = $00080000;
  DT_HIDEPREFIX           = $00100000;
  DT_PREFIXONLY           = $00200000;

type
  LPDRAWTEXTPARAMS = ^DRAWTEXTPARAMS;
  tagDRAWTEXTPARAMS = record
    cbSize: UINT;
    iTabLength: Integer;
    iLeftMargin: Integer;
    iRightMargin: Integer;
    uiLengthDrawn: UINT;
  end;
  DRAWTEXTPARAMS = tagDRAWTEXTPARAMS;
  TDrawTextParams = DRAWTEXTPARAMS;
  PDrawTextParams = LPDRAWTEXTPARAMS;

function DrawTextA(hDC: HDC; lpString: LPCSTR; nCount: Integer;
  var lpRect: RECT; uFormat: UINT): Integer; stdcall;
function DrawTextW(hDC: HDC; lpString: LPCWSTR; nCount: Integer;
  var lpRect: RECT; uFormat: UINT): Integer; stdcall;

{$IFDEF UNICODE}
function DrawText(hDC: HDC; lpString: LPCWSTR; nCount: Integer;
  var lpRect: RECT; uFormat: UINT): Integer; stdcall;
{$ELSE}
function DrawText(hDC: HDC; lpString: LPCSTR; nCount: Integer;
  var lpRect: RECT; uFormat: UINT): Integer; stdcall;
{$ENDIF}

function DrawTextExA(hDc: HDC; lpchText: LPSTR; cchText: Integer;
  var lprc: RECT; dwDTFormat: UINT; lpDTParams: LPDRAWTEXTPARAMS): Integer; stdcall;
function DrawTextExW(hDc: HDC; lpchText: LPWSTR; cchText: Integer;
  var lprc: RECT; dwDTFormat: UINT; lpDTParams: LPDRAWTEXTPARAMS): Integer; stdcall;

{$IFDEF UNICODE}
function DrawTextEx(hDc: HDC; lpchText: LPWSTR; cchText: Integer;
  var lprc: LPRECT; dwDTFormat: UINT; lpDTParams: LPDRAWTEXTPARAMS): Integer; stdcall;
{$ELSE}
function DrawTextEx(hDc: HDC; lpchText: LPSTR; cchText: Integer;
  var lprc: RECT; dwDTFormat: UINT; lpDTParams: LPDRAWTEXTPARAMS): Integer; stdcall;
{$ENDIF}

function GrayStringA(hDC: HDC; hBrush: HBRUSH; lpOutputFunc: GRAYSTRINGPROC;
  lpData: LPARAM; nCount, X, Y, nWidth, nHeight: Integer): BOOL; stdcall;
function GrayStringW(hDC: HDC; hBrush: HBRUSH; lpOutputFunc: GRAYSTRINGPROC;
  lpData: LPARAM; nCount, X, Y, nWidth, nHeight: Integer): BOOL; stdcall;

{$IFDEF UNICODE}
function GrayString(hDC: HDC; hBrush: HBRUSH; lpOutputFunc: GRAYSTRINGPROC;
  lpData: LPARAM; nCount, X, Y, nWidth, nHeight: Integer): BOOL; stdcall;
{$ELSE}
function GrayString(hDC: HDC; hBrush: HBRUSH; lpOutputFunc: GRAYSTRINGPROC;
  lpData: LPARAM; nCount, X, Y, nWidth, nHeight: Integer): BOOL; stdcall;
{$ENDIF}


// Monolithic state-drawing routine//
// Image type//

const
  DST_COMPLEX    = $0000;
  DST_TEXT       = $0001;
  DST_PREFIXTEXT = $0002;
  DST_ICON       = $0003;
  DST_BITMAP     = $0004;

// State type//

  DSS_NORMAL     = $0000;
  DSS_UNION      = $0010; // Gray string appearance
  DSS_DISABLED   = $0020;
  DSS_MONO       = $0080;
  DSS_HIDEPREFIX = $0200;
  DSS_PREFIXONLY = $0400;
  DSS_RIGHT      = $8000;

function DrawStateA(hdc: HDC; hbr: HBRUSH; lputputFunc: DRAWSTATEPROC;
  lData: LPARAM; wData: WPARAM; x, y, cx, cy: Integer; fuFlags: UINT): BOOL; stdcall;
function DrawStateW(hdc: HDC; hbr: HBRUSH; lputputFunc: DRAWSTATEPROC;
  lData: LPARAM; wData: WPARAM; x, y, cx, cy: Integer; fuFlags: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function DrawState(hdc: HDC; hbr: HBRUSH; lputputFunc: DRAWSTATEPROC;
  lData: LPARAM; wData: WPARAM; x, y, cx, cy: Integer; fuFlags: UINT): BOOL; stdcall;
{$ELSE}
function DrawState(hdc: HDC; hbr: HBRUSH; lputputFunc: DRAWSTATEPROC;
  lData: LPARAM; wData: WPARAM; x, y, cx, cy: Integer; fuFlags: UINT): BOOL; stdcall;
{$ENDIF}

function TabbedTextOutA(hDC: HDC; X, Y: Integer; lpString: LPCSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT; nTabOrigin: Integer): LONG; stdcall;
function TabbedTextOutW(hDC: HDC; X, Y: Integer; lpString: LPCWSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT; nTabOrigin: Integer): LONG; stdcall;

{$IFDEF UNICODE}
function TabbedTextOut(hDC: HDC; X, Y: Integer; lpString: LPCWSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT; nTabOrigin: Integer): LONG; stdcall;
{$ELSE}
function TabbedTextOut(hDC: HDC; X, Y: Integer; lpString: LPCSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT; nTabOrigin: Integer): LONG; stdcall;
{$ENDIF}

function GetTabbedTextExtentA(hDC: HDC; lpString: LPCSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT): DWORD; stdcall;
function GetTabbedTextExtentW(hDC: HDC; lpString: LPCWSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT): DWORD; stdcall;

{$IFDEF UNICODE}
function GetTabbedTextExtent(hDC: HDC; lpString: LPCWSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT): DWORD; stdcall;
{$ELSE}
function GetTabbedTextExtent(hDC: HDC; lpString: LPCSTR; nCount,
  nTabPositions: Integer; lpnTabStopPositions: LPINT): DWORD; stdcall;
{$ENDIF}

function UpdateWindow(hWnd: HWND): BOOL; stdcall;

function SetActiveWindow(hWnd: HWND): HWND; stdcall;

function GetForegroundWindow: HWND; stdcall;

function PaintDesktop(hdc: HDC): BOOL; stdcall;

procedure SwitchToThisWindow(hwnd: HWND; fUnknown: BOOL); stdcall;

function SetForegroundWindow(hWnd: HWND): BOOL; stdcall;

function AllowSetForegroundWindow(dwProcessId: DWORD): BOOL; stdcall;

const
  ASFW_ANY = DWORD(-1);

function LockSetForegroundWindow(uLockCode: UINT): BOOL; stdcall;

const
  LSFW_LOCK   = 1;
  LSFW_UNLOCK = 2;

function WindowFromDC(hDC: HDC): HWND; stdcall;

function GetDC(hWnd: HWND): HDC; stdcall;

function GetDCEx(hWnd: HWND; hrgnClip: HRGN; flags: DWORD): HDC; stdcall;

//
// GetDCEx() flags
//

const
  DCX_WINDOW           = $00000001;
  DCX_CACHE            = $00000002;
  DCX_NORESETATTRS     = $00000004;
  DCX_CLIPCHILDREN     = $00000008;
  DCX_CLIPSIBLINGS     = $00000010;
  DCX_PARENTCLIP       = $00000020;
  DCX_EXCLUDERGN       = $00000040;
  DCX_INTERSECTRGN     = $00000080;
  DCX_EXCLUDEUPDATE    = $00000100;
  DCX_INTERSECTUPDATE  = $00000200;
  DCX_LOCKWINDOWUPDATE = $00000400;

  DCX_VALIDATE = $00200000;

function GetWindowDC(hWnd: HWND): HDC; stdcall;

function ReleaseDC(hWnd: HWND; hDC: HDC): Integer; stdcall;

function BeginPaint(hWnd: HWND; var lpPaint: PAINTSTRUCT): HDC; stdcall;

function EndPaint(hWnd: HWND; const lpPaint: PAINTSTRUCT): BOOL; stdcall;

function GetUpdateRect(hWnd: HWND; var lpRect: RECT; bErase: BOOL): BOOL; stdcall;

function GetUpdateRgn(hWnd: HWND; hRgn: HRGN; bErase: BOOL): Integer; stdcall;

function SetWindowRgn(hWnd: HWND; hRgn: HRGN; bRedraw: BOOL): Integer; stdcall;

function GetWindowRgn(hWnd: HWND; hRgn: HRGN): Integer; stdcall;

function GetWindowRgnBox(hWnd: HWND; var lprc: RECT): Integer; stdcall;

function ExcludeUpdateRgn(hDC: HDC; hWnd: HWND): Integer; stdcall;

function InvalidateRect(hWnd: HWND; lpRect: LPRECT; bErase: BOOL): BOOL; stdcall;

function ValidateRect(hWnd: HWND; lpRect: LPRECT): BOOL; stdcall;

function InvalidateRgn(hWnd: HWND; hRgn: HRGN; bErase: BOOL): BOOL; stdcall;

function ValidateRgn(hWnd: HWND; hRgn: HRGN): BOOL; stdcall;

function RedrawWindow(hWnd: HWND; lprcUpdate: LPRECT; hrgnUpdate: HRGN; flags: UINT): BOOL; stdcall;

//
// RedrawWindow() flags
//

const
  RDW_INVALIDATE    = $0001;
  RDW_INTERNALPAINT = $0002;
  RDW_ERASE         = $0004;

  RDW_VALIDATE        = $0008;
  RDW_NOINTERNALPAINT = $0010;
  RDW_NOERASE         = $0020;

  RDW_NOCHILDREN  = $0040;
  RDW_ALLCHILDREN = $0080;

  RDW_UPDATENOW = $0100;
  RDW_ERASENOW  = $0200;

  RDW_FRAME   = $0400;
  RDW_NOFRAME = $0800;

//
// LockWindowUpdate API
//

function LockWindowUpdate(hWndLock: HWND): BOOL; stdcall;

function ScrollWindow(hWnd: HWND; XAmount, YAmount: Integer; lpRect, lpClipRect: LPRECT): BOOL; stdcall;

function ScrollDC(hDC: HDC; dx, dy: Integer; lprcScroll, lprcClip: LPRECT;
  hrgnUpdate: HRGN; lprcUpdate: LPRECT): BOOL; stdcall;

function ScrollWindowEx(hWnd: HWND; dx, dy: Integer; prcScroll, prcClip: LPRECT;
  hrgnUpdate: HRGN; prcUpdate: LPRECT; flags: UINT): Integer; stdcall;

const
  SW_SCROLLCHILDREN = $0001; // Scroll children within *lprcScroll.
  SW_INVALIDATE     = $0002; // Invalidate after scrolling
  SW_ERASE          = $0004; // If SW_INVALIDATE, don't send WM_ERASEBACKGROUND
  SW_SMOOTHSCROLL   = $0010; // Use smooth scrolling

function SetScrollPos(hWnd: HWND; nBar, nPos: Integer; bRedraw: BOOL): Integer; stdcall;

function GetScrollPos(hWnd: HWND; nBar: Integer): Integer; stdcall;

function SetScrollRange(hWnd: HWND; nBar, nMinPos, nMaxPos: Integer; bRedraw: BOOL): BOOL; stdcall;

function GetScrollRange(hWnd: HWND; nBar: Integer; var lpMinPos, lpMaxPos: Integer): BOOL; stdcall;

function ShowScrollBar(hWnd: HWND; wBar: Integer; bShow: BOOL): BOOL; stdcall;

function EnableScrollBar(hWnd: HWND; wSBflags, wArrows: UINT): BOOL; stdcall;

//
// EnableScrollBar() flags
//

const
  ESB_ENABLE_BOTH  = $0000;
  ESB_DISABLE_BOTH = $0003;

  ESB_DISABLE_LEFT  = $0001;
  ESB_DISABLE_RIGHT = $0002;

  ESB_DISABLE_UP   = $0001;
  ESB_DISABLE_DOWN = $0002;

  ESB_DISABLE_LTUP = ESB_DISABLE_LEFT;
  ESB_DISABLE_RTDN = ESB_DISABLE_RIGHT;

function SetPropA(hWnd: HWND; lpString: LPCSTR; hData: HANDLE): BOOL; stdcall;
function SetPropW(hWnd: HWND; lpString: LPCWSTR; hData: HANDLE): BOOL; stdcall;

{$IFDEF UNICODE}
function SetProp(hWnd: HWND; lpString: LPCWSTR; hData: HANDLE): BOOL; stdcall;
{$ELSE}
function SetProp(hWnd: HWND; lpString: LPCSTR; hData: HANDLE): BOOL; stdcall;
{$ENDIF}

function GetPropA(hWnd: HWND; lpString: LPCSTR): HANDLE; stdcall;
function GetPropW(hWnd: HWND; lpString: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function GetProp(hWnd: HWND; lpString: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function GetProp(hWnd: HWND; lpString: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function RemovePropA(hWnd: HWND; lpString: LPCSTR): HANDLE; stdcall;
function RemovePropW(hWnd: HWND; lpString: LPCWSTR): HANDLE; stdcall;

{$IFDEF UNICODE}
function RemoveProp(hWnd: HWND; lpString: LPCWSTR): HANDLE; stdcall;
{$ELSE}
function RemoveProp(hWnd: HWND; lpString: LPCSTR): HANDLE; stdcall;
{$ENDIF}

function EnumPropsExA(hWnd: HWND; lpEnumFunc: PROPENUMPROCEXA; lParam: LPARAM): Integer; stdcall;
function EnumPropsExW(hWnd: HWND; lpEnumFunc: PROPENUMPROCEXW; lParam: LPARAM): Integer; stdcall;

{$IFDEF UNICODE}
function EnumPropsEx(hWnd: HWND; lpEnumFunc: PROPENUMPROCEXW; lParam: LPARAM): Integer; stdcall;
{$ELSE}
function EnumPropsEx(hWnd: HWND; lpEnumFunc: PROPENUMPROCEXA; lParam: LPARAM): Integer; stdcall;
{$ENDIF}

function EnumPropsA(hWnd: HWND; lpEnumFunc: PROPENUMPROCA): Integer; stdcall;
function EnumPropsW(hWnd: HWND; lpEnumFunc: PROPENUMPROCW): Integer; stdcall;

{$IFDEF UNICODE}
function EnumProps(hWnd: HWND; lpEnumFunc: PROPENUMPROCW): Integer; stdcall;
{$ELSE}
function EnumProps(hWnd: HWND; lpEnumFunc: PROPENUMPROCA): Integer; stdcall;
{$ENDIF}

function SetWindowTextA(hWnd: HWND; lpString: LPCSTR): BOOL; stdcall;
function SetWindowTextW(hWnd: HWND; lpString: LPCWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function SetWindowText(hWnd: HWND; lpString: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetWindowText(hWnd: HWND; lpString: LPCSTR): BOOL; stdcall;
{$ENDIF}

function GetWindowTextA(hWnd: HWND; lpString: LPSTR; nMaxCount: Integer): Integer; stdcall;
function GetWindowTextW(hWnd: HWND; lpString: LPWSTR; nMaxCount: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetWindowText(hWnd: HWND; lpString: LPWSTR; nMaxCount: Integer): Integer; stdcall;
{$ELSE}
function GetWindowText(hWnd: HWND; lpString: LPSTR; nMaxCount: Integer): Integer; stdcall;
{$ENDIF}

function GetWindowTextLengthA(hWnd: HWND): Integer; stdcall;
function GetWindowTextLengthW(hWnd: HWND): Integer; stdcall;

{$IFDEF UNICODE}
function GetWindowTextLength(hWnd: HWND): Integer; stdcall;
{$ELSE}
function GetWindowTextLength(hWnd: HWND): Integer; stdcall;
{$ENDIF}

function GetClientRect(hWnd: HWND; var lpRect: RECT): BOOL; stdcall;

function GetWindowRect(hWnd: HWND; var lpRect: RECT): BOOL; stdcall;

function AdjustWindowRect(var lpRect: RECT; dwStyle: DWORD; bMenu: BOOL): BOOL; stdcall;

function AdjustWindowRectEx(var lpRect: RECT; dwStyle: DWORD;
  bMenu: BOOL; dwExStyle: DWORD): BOOL; stdcall;

const
  HELPINFO_WINDOW   = $0001;
  HELPINFO_MENUITEM = $0002;

type
  LPHELPINFO = ^HELPINFO;
  tagHELPINFO = record      // Structure pointed to by lParam of WM_HELP//
    cbSize: UINT;           // Size in bytes of this struct //
    iContextType: Integer;  // Either HELPINFO_WINDOW or HELPINFO_MENUITEM//
    iCtrlId: Integer;       // Control Id or a Menu item Id.//
    hItemHandle: HANDLE;    // hWnd of control or hMenu.    //
    dwContextId: DWORD_PTR; // Context Id associated with this item//
    MousePos: POINT;        // Mouse Position in screen co-ordinates//
  end;
  HELPINFO = tagHELPINFO;
  THelpInfo = HELPINFO;
  PHelpInfo = LPHELPINFO;

function SetWindowContextHelpId(hwnd: HWND; dwContextHelpId: DWORD): BOOL; stdcall;

function GetWindowContextHelpId(hwnd: HWND): DWORD; stdcall;

function SetMenuContextHelpId(hmenu: HMENU; dwContextHelpId: DWORD): BOOL; stdcall;

function GetMenuContextHelpId(hmenu: HMENU): DWORD; stdcall;

//
// MessageBox() Flags
//

const
  MB_OK                = $00000000;
  MB_OKCANCEL          = $00000001;
  MB_ABORTRETRYIGNORE  = $00000002;
  MB_YESNOCANCEL       = $00000003;
  MB_YESNO             = $00000004;
  MB_RETRYCANCEL       = $00000005;
  MB_CANCELTRYCONTINUE = $00000006;

  MB_ICONHAND        = $00000010;
  MB_ICONQUESTION    = $00000020;
  MB_ICONEXCLAMATION = $00000030;
  MB_ICONASTERISK    = $00000040;

  MB_USERICON    = $00000080;
  MB_ICONWARNING = MB_ICONEXCLAMATION;
  MB_ICONERROR   = MB_ICONHAND;

  MB_ICONINFORMATION = MB_ICONASTERISK;
  MB_ICONSTOP        = MB_ICONHAND;

  MB_DEFBUTTON1 = $00000000;
  MB_DEFBUTTON2 = $00000100;
  MB_DEFBUTTON3 = $00000200;
  MB_DEFBUTTON4 = $00000300;

  MB_APPLMODAL   = $00000000;
  MB_SYSTEMMODAL = $00001000;
  MB_TASKMODAL   = $00002000;
  MB_HELP        = $00004000; // Help Button

  MB_NOFOCUS              = $00008000;
  MB_SETFOREGROUND        = $00010000;
  MB_DEFAULT_DESKTOP_ONLY = $00020000;

  MB_TOPMOST    = $00040000;
  MB_RIGHT      = $00080000;
  MB_RTLREADING = $00100000;

const
{$IFDEF WINNT}
  {$IFDEF WINNT_0400_GREATER}
  MB_SERVICE_NOTIFICATION = $00200000;
  {$ELSE}
  MB_SERVICE_NOTIFICATION = $00040000;
  {$ENDIF}
{$ELSE}
  MB_SERVICE_NOTIFICATION_NT3X = $00040000;
{$ENDIF}

  MB_TYPEMASK = $0000000F;
  MB_ICONMASK = $000000F0;
  MB_DEFMASK  = $00000F00;
  MB_MODEMASK = $00003000;
  MB_MISCMASK = $0000C000;

function MessageBoxA(hWnd: HWND; lpText, lpCaption: LPCSTR; uType: UINT): Integer; stdcall;
function MessageBoxW(hWnd: HWND; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;

{$IFDEF UNICODE}
function MessageBox(hWnd: HWND; lpText, lpCaption: LPCWSTR; uType: UINT): Integer; stdcall;
{$ELSE}
function MessageBox(hWnd: HWND; lpText, lpCaption: LPCSTR; uType: UINT): Integer; stdcall;
{$ENDIF}

function MessageBoxExA(hWnd: HWND; lpText, lpCaption: LPCSTR; uType: UINT;
  wLanguageId: WORD): Integer; stdcall;
function MessageBoxExW(hWnd: HWND; lpText, lpCaption: LPCWSTR; uType: UINT;
  wLanguageId: WORD): Integer; stdcall;

{$IFDEF UNICODE}
function MessageBoxEx(hWnd: HWND; lpText, lpCaption: LPCWSTR; uType: UINT;
  wLanguageId: WORD): Integer; stdcall;
{$ELSE}
function MessageBoxEx(hWnd: HWND; lpText, lpCaption: LPCSTR; uType: UINT;
  wLanguageId: WORD): Integer; stdcall;
{$ENDIF}

type
  MSGBOXCALLBACK = procedure (var lpHelpInfo: HELPINFO); stdcall;
  TMsgBoxCallback = MSGBOXCALLBACK;

  LPMSGBOXPARAMSA = ^MSGBOXPARAMSA;
  tagMSGBOXPARAMSA = record
    cbSize: UINT;
    hwndOwner: HWND;
    hInstance: HINSTANCE;
    lpszText: LPCSTR;
    lpszCaption: LPCSTR;
    dwStyle: DWORD;
    lpszIcon: LPCSTR;
    dwContextHelpId: DWORD_PTR;
    lpfnMsgBoxCallback: MSGBOXCALLBACK;
    dwLanguageId: DWORD;
  end;
  MSGBOXPARAMSA = tagMSGBOXPARAMSA;
  TMsgBoxParamsA = MSGBOXPARAMSA;
  PMsgBoxParamsA = LPMSGBOXPARAMSA;

  LPMSGBOXPARAMSW = ^MSGBOXPARAMSW;
  tagMSGBOXPARAMSW = record
    cbSize: UINT;
    hwndOwner: HWND;
    hInstance: HINSTANCE;
    lpszText: LPCWSTR;
    lpszCaption: LPCWSTR;
    dwStyle: DWORD;
    lpszIcon: LPCWSTR;
    dwContextHelpId: DWORD_PTR;
    lpfnMsgBoxCallback: MSGBOXCALLBACK;
    dwLanguageId: DWORD;
  end;
  MSGBOXPARAMSW = tagMSGBOXPARAMSW;
  TMsgBoxParamsW = MSGBOXPARAMSW;
  PMsgBoxParamsW = LPMSGBOXPARAMSW;

{$IFDEF UNICODE}
  MSGBOXPARAMS = MSGBOXPARAMSW;
  LPMSGBOXPARAMS = LPMSGBOXPARAMSW;
  TMsgBoxParams = TMsgBoxParamsW;
  PMsgBoxParams = PMsgBoxParamsW;
{$ELSE}
  MSGBOXPARAMS = MSGBOXPARAMSA;
  LPMSGBOXPARAMS = LPMSGBOXPARAMSA;
  TMsgBoxParams = TMsgBoxParamsA;
  PMsgBoxParams = PMsgBoxParamsA;
{$ENDIF}

function MessageBoxIndirectA(const lpMsgBoxParams: MSGBOXPARAMSA): Integer; stdcall;
function MessageBoxIndirectW(const lpMsgBoxParams: MSGBOXPARAMSW): Integer; stdcall;

{$IFDEF UNICODE}
function MessageBoxIndirect(const lpMsgBoxParams: MSGBOXPARAMSW): Integer; stdcall;
{$ELSE}
function MessageBoxIndirect(const lpMsgBoxParams: MSGBOXPARAMSA): Integer; stdcall;
{$ENDIF}

function MessageBeep(uType: UINT): BOOL; stdcall;

function ShowCursor(bShow: BOOL): Integer; stdcall;

function SetCursorPos(X, Y: Integer): BOOL; stdcall;

function SetCursor(hCursor: HCURSOR): HCURSOR; stdcall;

function GetCursorPos(var lpPoint: POINT): BOOL; stdcall;

function ClipCursor(lpRect: LPRECT): BOOL; stdcall;

function GetClipCursor(var lpRect: RECT): BOOL; stdcall;

function GetCursor: HCURSOR; stdcall;

function CreateCaret(hWnd: HWND; hBitmap: HBITMAP; nWidth, nHeight: Integer): BOOL; stdcall;

function GetCaretBlinkTime: UINT; stdcall;

function SetCaretBlinkTime(uMSeconds: UINT): BOOL; stdcall;

function DestroyCaret: BOOL; stdcall;

function HideCaret(hWnd: HWND): BOOL; stdcall;

function ShowCaret(hWnd: HWND): BOOL; stdcall;

function SetCaretPos(X, Y: Integer): BOOL; stdcall;

function GetCaretPos(var lpPoint: POINT): BOOL; stdcall;

function ClientToScreen(hWnd: HWND; var lpPoint: POINT): BOOL; stdcall;

function ScreenToClient(hWnd: HWND; var lpPoint: POINT): BOOL; stdcall;

function MapWindowPoints(hWndFrom, hWndTo: HWND; lpPoints: LPPOINT; cPoints: UINT): Integer; stdcall;

function WindowFromPoint(Point: POINT): HWND; stdcall;

function ChildWindowFromPoint(hWndParent: HWND; Point: POINT): HWND; stdcall;

const
  CWP_ALL             = $0000;
  CWP_SKIPINVISIBLE   = $0001;
  CWP_SKIPDISABLED    = $0002;
  CWP_SKIPTRANSPARENT = $0004;

function ChildWindowFromPointEx(hwndParent: HWND; pt: POINT; uFlags: UINT): HWND; stdcall;

//
// Color Types
//

const
  CTLCOLOR_MSGBOX    = 0;
  CTLCOLOR_EDIT      = 1;
  CTLCOLOR_LISTBOX   = 2;
  CTLCOLOR_BTN       = 3;
  CTLCOLOR_DLG       = 4;
  CTLCOLOR_SCROLLBAR = 5;
  CTLCOLOR_STATIC    = 6;
  CTLCOLOR_MAX       = 7;

  COLOR_SCROLLBAR           = 0;
  COLOR_BACKGROUND          = 1;
  COLOR_ACTIVECAPTION       = 2;
  COLOR_INACTIVECAPTION     = 3;
  COLOR_MENU                = 4;
  COLOR_WINDOW              = 5;
  COLOR_WINDOWFRAME         = 6;
  COLOR_MENUTEXT            = 7;
  COLOR_WINDOWTEXT          = 8;
  COLOR_CAPTIONTEXT         = 9;
  COLOR_ACTIVEBORDER        = 10;
  COLOR_INACTIVEBORDER      = 11;
  COLOR_APPWORKSPACE        = 12;
  COLOR_HIGHLIGHT           = 13;
  COLOR_HIGHLIGHTTEXT       = 14;
  COLOR_BTNFACE             = 15;
  COLOR_BTNSHADOW           = 16;
  COLOR_GRAYTEXT            = 17;
  COLOR_BTNTEXT             = 18;
  COLOR_INACTIVECAPTIONTEXT = 19;
  COLOR_BTNHIGHLIGHT        = 20;

  COLOR_3DDKSHADOW = 21;
  COLOR_3DLIGHT    = 22;
  COLOR_INFOTEXT   = 23;
  COLOR_INFOBK     = 24;

  COLOR_HOTLIGHT                = 26;
  COLOR_GRADIENTACTIVECAPTION   = 27;
  COLOR_GRADIENTINACTIVECAPTION = 28;
  COLOR_MENUHILIGHT             = 29;
  COLOR_MENUBAR                 = 30;

  COLOR_DESKTOP     = COLOR_BACKGROUND;
  COLOR_3DFACE      = COLOR_BTNFACE;
  COLOR_3DSHADOW    = COLOR_BTNSHADOW;
  COLOR_3DHIGHLIGHT = COLOR_BTNHIGHLIGHT;
  COLOR_3DHILIGHT   = COLOR_BTNHIGHLIGHT;
  COLOR_BTNHILIGHT  = COLOR_BTNHIGHLIGHT;

function GetSysColor(nIndex: Integer): DWORD; stdcall;

function GetSysColorBrush(nIndex: Integer): HBRUSH; stdcall;

function SetSysColors(cElements: Integer; lpaElements: LPINT;
  lpaRgbValues: LPCOLORREF): BOOL; stdcall;

function DrawFocusRect(hDC: HDC; const lprc: RECT): BOOL; stdcall;

function FillRect(hDC: HDC; const lprc: RECT; hbr: HBRUSH): Integer; stdcall;

function FrameRect(hDC: HDC; const lprc: RECT; hbr: HBRUSH): Integer; stdcall;

function InvertRect(hDC: HDC; const lprc: RECT): BOOL; stdcall;

function SetRect(var lprc: RECT; xLeft, yTop, xRight, yBottom: Integer): BOOL; stdcall;

function SetRectEmpty(var lprc: RECT): BOOL; stdcall;

function CopyRect(var lprcDst: RECT; const lprcSrc: RECT): BOOL; stdcall;

function InflateRect(var lprc: RECT; dx, dy: Integer): BOOL; stdcall;

function IntersectRect(var lprcDst: RECT; const lprcSrc1, lprcSrc2: RECT): BOOL; stdcall;

function UnionRect(var lprcDst: RECT; const lprcSrc1, lprcSrc2: RECT): BOOL; stdcall;

function SubtractRect(var lprcDst: RECT; const lprcSrc1, lprcSrc2: RECT): BOOL; stdcall;

function OffsetRect(var lprc: RECT; dx, dy: Integer): BOOL; stdcall;

function IsRectEmpty(const lprc: RECT): BOOL; stdcall;

function EqualRect(const lprc1, lprc2: RECT): BOOL; stdcall;

function PtInRect(const lprc: RECT; pt: POINT): BOOL; stdcall;

function GetWindowWord(hWnd: HWND; nIndex: Integer): WORD; stdcall;

function SetWindowWord(hWnd: HWND; nIndex: Integer; wNewWord: WORD): WORD; stdcall;

function GetWindowLongA(hWnd: HWND; nIndex: Integer): LONG; stdcall;
function GetWindowLongW(hWnd: HWND; nIndex: Integer): LONG; stdcall;

{$IFDEF UNICODE}
function GetWindowLong(hWnd: HWND; nIndex: Integer): LONG; stdcall;
{$ELSE}
function GetWindowLong(hWnd: HWND; nIndex: Integer): LONG; stdcall;
{$ENDIF}

function SetWindowLongA(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): LONG; stdcall;
function SetWindowLongW(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): LONG; stdcall;

{$IFDEF UNICODE}
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): LONG; stdcall;
{$ELSE}
function SetWindowLong(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): LONG; stdcall;
{$ENDIF}

function GetWindowLongPtrA(hWnd: HWND; nIndex: Integer): LONG_PTR;

function GetWindowLongPtrW(hWnd: HWND; nIndex: Integer): LONG_PTR;

{$IFDEF UNICODE}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR;
{$ELSE}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR;
{$ENDIF}

function SetWindowLongPtrA(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;

function SetWindowLongPtrW(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;

{$IFDEF UNICODE}
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;
{$ELSE}
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;
{$ENDIF}

function GetClassWord(hWnd: HWND; nIndex: Integer): WORD; stdcall;

function SetClassWord(hWnd: HWND; nIndex: Integer; wNewWord: WORD): WORD; stdcall;

function GetClassLongA(hWnd: HWND; nIndex: Integer): DWORD; stdcall;
function GetClassLongW(hWnd: HWND; nIndex: Integer): DWORD; stdcall;

{$IFDEF UNICODE}
function GetClassLong(hWnd: HWND; nIndex: Integer): DWORD; stdcall;
{$ELSE}
function GetClassLong(hWnd: HWND; nIndex: Integer): DWORD; stdcall;
{$ENDIF}

function SetClassLongA(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): DWORD; stdcall;
function SetClassLongW(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): DWORD; stdcall;

{$IFDEF UNICODE}
function SetClassLong(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): DWORD; stdcall;
{$ELSE}
function SetClassLong(hWnd: HWND; nIndex: Integer; dwNewLong: LONG): DWORD; stdcall;
{$ENDIF}

function GetClassLongPtrA(hWnd: HWND; nIndex: Integer): ULONG_PTR;

function GetClassLongPtrW(hWnd: HWND; nIndex: Integer): ULONG_PTR;

{$IFDEF UNICODE}
function GetClassLongPtr(hWnd: HWND; nIndex: Integer): ULONG_PTR;
{$ELSE}
function GetClassLongPtr(hWnd: HWND; nIndex: Integer): ULONG_PTR;
{$ENDIF}

function SetClassLongPtrA(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;

function SetClassLongPtrW(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;

{$IFDEF UNICODE}
function SetClassLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;
{$ELSE}
function SetClassLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;
{$ENDIF}

function GetProcessDefaultLayout(var pdwDefaultLayout: DWORD): BOOL; stdcall;

function SetProcessDefaultLayout(dwDefaultLayout: DWORD): BOOL; stdcall;

function GetDesktopWindow: HWND; stdcall;

function GetParent(hWnd: HWND): HWND; stdcall;

function SetParent(hWndChild, hWndNewParent: HWND): HWND; stdcall;

function EnumChildWindows(hWndParent: HWND; lpEnumFunc: WNDENUMPROC; lParam: LPARAM): BOOL; stdcall;

function FindWindowA(lpClassName, lpWindowName: LPCSTR): HWND; stdcall;
function FindWindowW(lpClassName, lpWindowName: LPCWSTR): HWND; stdcall;

{$IFDEF UNICODE}
function FindWindow(lpClassName, lpWindowName: LPCWSTR): HWND; stdcall;
{$ELSE}
function FindWindow(lpClassName, lpWindowName: LPCSTR): HWND; stdcall;
{$ENDIF}

function FindWindowExA(hwndParent, hwndChildAfter: HWND; lpszClass, lpszWindow: LPCSTR): HWND; stdcall;
function FindWindowExW(hwndParent, hwndChildAfter: HWND; lpszClass, lpszWindow: LPCWSTR): HWND; stdcall;

{$IFDEF UNICODE}
function FindWindowEx(hwndParent, hwndChildAfter: HWND; lpszClass, lpszWindow: LPCWSTR): HWND; stdcall;
{$ELSE}
function FindWindowEx(hwndParent, hwndChildAfter: HWND; lpszClass, lpszWindow: LPCSTR): HWND; stdcall;
{$ENDIF}

function GetShellWindow: HWND; stdcall;

function RegisterShellHookWindow(h: HWND): BOOL; stdcall;

function DeregisterShellHookWindow(h: HWND): BOOL; stdcall;

function EnumWindows(lpEnumFunc: WNDENUMPROC; lParam: LPARAM): BOOL; stdcall;

function EnumThreadWindows(dwThreadId: DWORD; lpfn: WNDENUMPROC; lParam: LPARAM): BOOL; stdcall;

function EnumTaskWindows(hTask: HANDLE; lpfn: WNDENUMPROC; lParam: LPARAM): BOOL;

function GetClassNameA(hWnd: HWND; lpClassName: LPSTR; nMaxCount: Integer): Integer; stdcall;
function GetClassNameW(hWnd: HWND; lpClassName: LPWSTR; nMaxCount: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function GetClassName(hWnd: HWND; lpClassName: LPWSTR; nMaxCount: Integer): Integer; stdcall;
{$ELSE}
function GetClassName(hWnd: HWND; lpClassName: LPSTR; nMaxCount: Integer): Integer; stdcall;
{$ENDIF}

function GetTopWindow(hWnd: HWND): HWND; stdcall;

function GetNextWindow(hWnd: HWND; wCmd: UINT): HWND;

function GetWindowThreadProcessId(hWnd: HWND; lpdwProcessId: LPDWORD): DWORD; stdcall;

function IsGUIThread(bConvert: BOOL): BOOL; stdcall;

function GetWindowTask(hWnd: HWND): HANDLE;

function GetLastActivePopup(hWnd: HWND): HWND; stdcall;

//
// GetWindow() Constants
//

const
  GW_HWNDFIRST    = 0;
  GW_HWNDLAST     = 1;
  GW_HWNDNEXT     = 2;
  GW_HWNDPREV     = 3;
  GW_OWNER        = 4;
  GW_CHILD        = 5;
{$IFNDEF WINVER_0500_GREATER} // #if(WINVER <= 0x0400)
  GW_MAX          = 5;
{$ELSE}
  GW_ENABLEDPOPUP = 6;
  GW_MAX          = 6;
{$ENDIF}

function GetWindow(hWnd: HWND; uCmd: UINT): HWND; stdcall;

function SetWindowsHookA(nFilterType: Integer; pfnFilterProc: HOOKPROC): HHOOK; stdcall;
function SetWindowsHookW(nFilterType: Integer; pfnFilterProc: HOOKPROC): HHOOK; stdcall;

{$IFDEF UNICODE}
function SetWindowsHook(nFilterType: Integer; pfnFilterProc: HOOKPROC): HHOOK; stdcall;
{$ELSE}
function SetWindowsHook(nFilterType: Integer; pfnFilterProc: HOOKPROC): HHOOK; stdcall;
{$ENDIF}

function UnhookWindowsHook(nCode: Integer; pfnFilterProc: HOOKPROC): BOOL; stdcall;

function SetWindowsHookExA(idHook: Integer; lpfn: HOOKPROC; hmod: HINSTANCE;
  dwThreadId: DWORD): HHOOK; stdcall;
function SetWindowsHookExW(idHook: Integer; lpfn: HOOKPROC; hmod: HINSTANCE;
  dwThreadId: DWORD): HHOOK; stdcall;

{$IFDEF UNICODE}
function SetWindowsHookEx(idHook: Integer; lpfn: HOOKPROC; hmod: HINSTANCE;
  dwThreadId: DWORD): HHOOK; stdcall;
{$ELSE}
function SetWindowsHookEx(idHook: Integer; lpfn: HOOKPROC; hmod: HINSTANCE;
  dwThreadId: DWORD): HHOOK; stdcall;
{$ENDIF}

function UnhookWindowsHookEx(hhk: HHOOK): BOOL; stdcall;

function CallNextHookEx(hhk: HHOOK; nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

//
// Macros for source-level compatibility with old functions.
//

function DefHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM; phhk: LPHHOOK): LRESULT;

// ;win40  -- A lot of MF_* flags have been renamed as MFT_* and MFS_* flags//
//
// Menu flags for Add/Check/EnableMenuItem()
///)

const
  MF_INSERT = $00000000;
  MF_CHANGE = $00000080;
  MF_APPEND = $00000100;
  MF_DELETE = $00000200;
  MF_REMOVE = $00001000;

  MF_BYCOMMAND  = $00000000;
  MF_BYPOSITION = $00000400;

  MF_SEPARATOR = $00000800;

  MF_ENABLED  = $00000000;
  MF_GRAYED   = $00000001;
  MF_DISABLED = $00000002;

  MF_UNCHECKED       = $00000000;
  MF_CHECKED         = $00000008;
  MF_USECHECKBITMAPS = $00000200;

  MF_STRING    = $00000000;
  MF_BITMAP    = $00000004;
  MF_OWNERDRAW = $00000100;

  MF_POPUP        = $00000010;
  MF_MENUBARBREAK = $00000020;
  MF_MENUBREAK    = $00000040;

  MF_UNHILITE = $00000000;
  MF_HILITE   = $00000080;

  MF_DEFAULT      = $00001000;
  MF_SYSMENU      = $00002000;
  MF_HELP         = $00004000;
  MF_RIGHTJUSTIFY = $00004000;

  MF_MOUSESELECT = $00008000;
  MF_END         = $00000080; // Obsolete -- only used by old RES files

  MFT_STRING       = MF_STRING;
  MFT_BITMAP       = MF_BITMAP;
  MFT_MENUBARBREAK = MF_MENUBARBREAK;
  MFT_MENUBREAK    = MF_MENUBREAK;
  MFT_OWNERDRAW    = MF_OWNERDRAW;
  MFT_RADIOCHECK   = $00000200;
  MFT_SEPARATOR    = MF_SEPARATOR;
  MFT_RIGHTORDER   = $00002000;
  MFT_RIGHTJUSTIFY = MF_RIGHTJUSTIFY;

// Menu flags for Add/Check/EnableMenuItem()

  MFS_GRAYED    = $00000003;
  MFS_DISABLED  = MFS_GRAYED;
  MFS_CHECKED   = MF_CHECKED;
  MFS_HILITE    = MF_HILITE;
  MFS_ENABLED   = MF_ENABLED;
  MFS_UNCHECKED = MF_UNCHECKED;
  MFS_UNHILITE  = MF_UNHILITE;
  MFS_DEFAULT   = MF_DEFAULT;

function CheckMenuRadioItem(hmenu: HMENU; idFirst, idLast, idCheck, uFlags: UINT): BOOL; stdcall;

//
// Menu item resource format
//

type
  PMENUITEMTEMPLATEHEADER = ^MENUITEMTEMPLATEHEADER;
  MENUITEMTEMPLATEHEADER = record
    versionNumber: WORD;
    offset: WORD;
  end;
  TMenuItemTemplateHeader = MENUITEMTEMPLATEHEADER;

  PMENUITEMTEMPLATE = ^MENUITEMTEMPLATE; // version 0
  MENUITEMTEMPLATE = record
    mtOption: WORD;
    mtID: WORD;
    mtString: array [0..0] of WCHAR;
  end;
  TMenuItemTemplate = MENUITEMTEMPLATE;

//
// System Menu Command Values
//

const
  SC_SIZE         = $F000;
  SC_MOVE         = $F010;
  SC_MINIMIZE     = $F020;
  SC_MAXIMIZE     = $F030;
  SC_NEXTWINDOW   = $F040;
  SC_PREVWINDOW   = $F050;
  SC_CLOSE        = $F060;
  SC_VSCROLL      = $F070;
  SC_HSCROLL      = $F080;
  SC_MOUSEMENU    = $F090;
  SC_KEYMENU      = $F100;
  SC_ARRANGE      = $F110;
  SC_RESTORE      = $F120;
  SC_TASKLIST     = $F130;
  SC_SCREENSAVE   = $F140;
  SC_HOTKEY       = $F150;
  SC_DEFAULT      = $F160;
  SC_MONITORPOWER = $F170;
  SC_CONTEXTHELP  = $F180;
  SC_SEPARATOR    = $F00F;

//
// Obsolete names
//

const
  SC_ICON = SC_MINIMIZE;
  SC_ZOOM = SC_MAXIMIZE;

//
// Resource Loading Routines
//

function LoadBitmapA(hInstance: HINSTANCE; lpBitmapName: LPCSTR): HBITMAP; stdcall;
function LoadBitmapW(hInstance: HINSTANCE; lpBitmapName: LPCWSTR): HBITMAP; stdcall;

{$IFDEF UNICODE}
function LoadBitmap(hInstance: HINSTANCE; lpBitmapName: LPCWSTR): HBITMAP; stdcall;
{$ELSE}
function LoadBitmap(hInstance: HINSTANCE; lpBitmapName: LPCSTR): HBITMAP; stdcall;
{$ENDIF}

function LoadCursorA(hInstance: HINSTANCE; lpCursorName: LPCSTR): HCURSOR; stdcall;
function LoadCursorW(hInstance: HINSTANCE; lpCursorName: LPCWSTR): HCURSOR; stdcall;

{$IFDEF UNICODE}
function LoadCursor(hInstance: HINSTANCE; lpCursorName: LPCWSTR): HCURSOR; stdcall;
{$ELSE}
function LoadCursor(hInstance: HINSTANCE; lpCursorName: LPCSTR): HCURSOR; stdcall;
{$ENDIF}

function LoadCursorFromFileA(lpFileName: LPCSTR): HCURSOR; stdcall;
function LoadCursorFromFileW(lpFileName: LPCWSTR): HCURSOR; stdcall;

{$IFDEF UNICODE}
function LoadCursorFromFile(lpFileName: LPCWSTR): HCURSOR; stdcall;
{$ELSE}
function LoadCursorFromFile(lpFileName: LPCSTR): HCURSOR; stdcall;
{$ENDIF}

function CreateCursor(hInst: HINSTANCE; xHotSpot, yHotSpot, nWidth, nHeight: Integer;
  pvANDPlane: PVOID; pvXORPlane: PVOID): HCURSOR; stdcall;

function DestroyCursor(hCursor: HCURSOR): BOOL; stdcall;

function CopyCursor(pcur: HCURSOR): HCURSOR;

//
// Standard Cursor IDs
//

const
  IDC_ARROW       = MAKEINTRESOURCE(32512);
  IDC_IBEAM       = MAKEINTRESOURCE(32513);
  IDC_WAIT        = MAKEINTRESOURCE(32514);
  IDC_CROSS       = MAKEINTRESOURCE(32515);
  IDC_UPARROW     = MAKEINTRESOURCE(32516);
  IDC_SIZE        = MAKEINTRESOURCE(32640); // OBSOLETE: use IDC_SIZEALL
  IDC_ICON        = MAKEINTRESOURCE(32641); // OBSOLETE: use IDC_ARROW
  IDC_SIZENWSE    = MAKEINTRESOURCE(32642);
  IDC_SIZENESW    = MAKEINTRESOURCE(32643);
  IDC_SIZEWE      = MAKEINTRESOURCE(32644);
  IDC_SIZENS      = MAKEINTRESOURCE(32645);
  IDC_SIZEALL     = MAKEINTRESOURCE(32646);
  IDC_NO          = MAKEINTRESOURCE(32648); // not in win3.1
  IDC_HAND        = MAKEINTRESOURCE(32649);
  IDC_APPSTARTING = MAKEINTRESOURCE(32650); // not in win3.1
  IDC_HELP        = MAKEINTRESOURCE(32651);

function SetSystemCursor(hcur: HCURSOR; id: DWORD): BOOL; stdcall;

type
  PICONINFO = ^ICONINFO;
  _ICONINFO = record
    fIcon: BOOL;
    xHotspot: DWORD;
    yHotspot: DWORD;
    hbmMask: HBITMAP;
    hbmColor: HBITMAP;
  end;
  ICONINFO = _ICONINFO;
  TIconInfo = ICONINFO;

function LoadIconA(hInstance: HINSTANCE; lpIconName: LPCSTR): HICON; stdcall;
function LoadIconW(hInstance: HINSTANCE; lpIconName: LPCWSTR): HICON; stdcall;

{$IFDEF UNICODE}
function LoadIcon(hInstance: HINSTANCE; lpIconName: LPCWSTR): HICON; stdcall;
{$ELSE}
function LoadIcon(hInstance: HINSTANCE; lpIconName: LPCSTR): HICON; stdcall;
{$ENDIF}

function PrivateExtractIconsA(szFileName: LPCSTR; nIconIndex, cxIcon, cyIcon: Integer; var phicon: HICON;
  var piconid: UINT; nIcons, flags: UINT): UINT; stdcall;
function PrivateExtractIconsW(szFileName: LPCWSTR; nIconIndex, cxIcon, cyIcon: Integer; var phicon: HICON;
  var piconid: UINT; nIcons, flags: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function PrivateExtractIcons(szFileName: LPCWSTR; nIconIndex, cxIcon, cyIcon: Integer; var phicon: HICON;
  var piconid: UINT; nIcons, flags: UINT): UINT; stdcall;
{$ELSE}
function PrivateExtractIcons(szFileName: LPCSTR; nIconIndex, cxIcon, cyIcon: Integer; var phicon: HICON;
  var piconid: UINT; nIcons, flags: UINT): UINT; stdcall;
{$ENDIF}

function CreateIcon(hInstance: HINSTANCE; nWidth, nHeight: Integer; cPlanes,
  cBitsPixel: BYTE; lpbANDbits: LPBYTE; lpbXORbits: LPBYTE): HICON; stdcall;

function DestroyIcon(hIcon: HICON): BOOL; stdcall;

function LookupIconIdFromDirectory(presbits: PBYTE; fIcon: BOOL): Integer; stdcall;

function LookupIconIdFromDirectoryEx(presbits: PBYTE; fIcon: BOOL;
  cxDesired, cyDesired: Integer; Flags: UINT): Integer; stdcall;

function CreateIconFromResource(presbits: PBYTE; dwResSize: DWORD;
  fIcon: BOOL; dwVer: DWORD): HICON; stdcall;

function CreateIconFromResourceEx(presbits: PBYTE; dwResSize: DWORD; fIcon: BOOL;
  dwVer: DWORD; cxDesired, cyDesired: Integer; Flags: UINT): HICON; stdcall;

// Icon/Cursor header//

type
  LPCURSORSHAPE = ^CURSORSHAPE;
  tagCURSORSHAPE = record
    xHotSpot: Integer;
    yHotSpot: Integer;
    cx: Integer;
    cy: Integer;
    cbWidth: Integer;
    Planes: BYTE;
    BitsPixel: BYTE;
  end;
  CURSORSHAPE = tagCURSORSHAPE;
  TCursorShape = CURSORSHAPE;
  PCursorShape = LPCURSORSHAPE;

const
  IMAGE_BITMAP      = 0;
  IMAGE_ICON        = 1;
  IMAGE_CURSOR      = 2;
  IMAGE_ENHMETAFILE = 3;

  LR_DEFAULTCOLOR     = $0000;
  LR_MONOCHROME       = $0001;
  LR_COLOR            = $0002;
  LR_COPYRETURNORG    = $0004;
  LR_COPYDELETEORG    = $0008;
  LR_LOADFROMFILE     = $0010;
  LR_LOADTRANSPARENT  = $0020;
  LR_DEFAULTSIZE      = $0040;
  LR_VGACOLOR         = $0080;
  LR_LOADMAP3DCOLORS  = $1000;
  LR_CREATEDIBSECTION = $2000;
  LR_COPYFROMRESOURCE = $4000;
  LR_SHARED           = $8000;

function LoadImageA(hinst: HINSTANCE; lpszName: LPCSTR; uType: UINT;
  cxDesired, cyDesired: Integer; fuLoad: UINT): HANDLE; stdcall;
function LoadImageW(hinst: HINSTANCE; lpszName: LPCWSTR; uType: UINT;
  cxDesired, cyDesired: Integer; fuLoad: UINT): HANDLE; stdcall;

{$IFDEF UNICODE}
function LoadImage(hinst: HINSTANCE; lpszName: LPCWSTR; uType: UINT;
  cxDesired, cyDesired: Integer; fuLoad: UINT): HANDLE; stdcall;
{$ELSE}
function LoadImage(hinst: HINSTANCE; lpszName: LPCSTR; uType: UINT;
  cxDesired, cyDesired: Integer; fuLoad: UINT): HANDLE; stdcall;
{$ENDIF}

function CopyImage(hinst: HANDLE; lpszName: UINT; cxDesired, cyDesired: Integer;
  fuFlags: UINT): HANDLE; stdcall;

const
  DI_MASK        = $0001;
  DI_IMAGE       = $0002;
  DI_NORMAL      = $0003;
  DI_COMPAT      = $0004;
  DI_DEFAULTSIZE = $0008;
  DI_NOMIRROR    = $0010;

function DrawIconEx(hdc: HDC; xLeft, yTop: Integer; hIcon: HICON;
  cxWidth, cyWidth: Integer; istepIfAniCur: UINT; hbrFlickerFreeDraw: HBRUSH;
  diFlags: UINT): BOOL; stdcall;

function CreateIconIndirect(const piconinfo: ICONINFO): HICON; stdcall;

function CopyIcon(hIcon: HICON): HICON; stdcall;

function GetIconInfo(hIcon: HICON; var piconinfo: ICONINFO): BOOL; stdcall;

const
  RES_ICON   = 1;
  RES_CURSOR = 2;

//
// OEM Resource Ordinal Numbers
//

  OBM_CLOSE    = 32754;
  OBM_UPARROW  = 32753;
  OBM_DNARROW  = 32752;
  OBM_RGARROW  = 32751;
  OBM_LFARROW  = 32750;
  OBM_REDUCE   = 32749;
  OBM_ZOOM     = 32748;
  OBM_RESTORE  = 32747;
  OBM_REDUCED  = 32746;
  OBM_ZOOMD    = 32745;
  OBM_RESTORED = 32744;
  OBM_UPARROWD = 32743;
  OBM_DNARROWD = 32742;
  OBM_RGARROWD = 32741;
  OBM_LFARROWD = 32740;
  OBM_MNARROW  = 32739;
  OBM_COMBO    = 32738;
  OBM_UPARROWI = 32737;
  OBM_DNARROWI = 32736;
  OBM_RGARROWI = 32735;
  OBM_LFARROWI = 32734;

  OBM_OLD_CLOSE   = 32767;
  OBM_SIZE        = 32766;
  OBM_OLD_UPARROW = 32765;
  OBM_OLD_DNARROW = 32764;
  OBM_OLD_RGARROW = 32763;
  OBM_OLD_LFARROW = 32762;
  OBM_BTSIZE      = 32761;
  OBM_CHECK       = 32760;
  OBM_CHECKBOXES  = 32759;
  OBM_BTNCORNERS  = 32758;
  OBM_OLD_REDUCE  = 32757;
  OBM_OLD_ZOOM    = 32756;
  OBM_OLD_RESTORE = 32755;

  OCR_NORMAL      = 32512;
  OCR_IBEAM       = 32513;
  OCR_WAIT        = 32514;
  OCR_CROSS       = 32515;
  OCR_UP          = 32516;
  OCR_SIZE        = 32640; // OBSOLETE: use OCR_SIZEALL
  OCR_ICON        = 32641; // OBSOLETE: use OCR_NORMAL
  OCR_SIZENWSE    = 32642;
  OCR_SIZENESW    = 32643;
  OCR_SIZEWE      = 32644;
  OCR_SIZENS      = 32645;
  OCR_SIZEALL     = 32646;
  OCR_ICOCUR      = 32647; // OBSOLETE: use OIC_WINLOGO
  OCR_NO          = 32648;
  OCR_HAND        = 32649;
  OCR_APPSTARTING = 32650;

  OIC_SAMPLE      = 32512;
  OIC_HAND        = 32513;
  OIC_QUES        = 32514;
  OIC_BANG        = 32515;
  OIC_NOTE        = 32516;
  OIC_WINLOGO     = 32517;
  OIC_WARNING     = OIC_BANG;
  OIC_ERROR       = OIC_HAND;
  OIC_INFORMATION = OIC_NOTE;

  ORD_LANGDRIVER = 1; // The ordinal number for the entry point of language drivers.

//
// Standard Icon IDs
//

  IDI_APPLICATION = MAKEINTRESOURCE(32512);
  IDI_HAND        = MAKEINTRESOURCE(32513);
  IDI_QUESTION    = MAKEINTRESOURCE(32514);
  IDI_EXCLAMATION = MAKEINTRESOURCE(32515);
  IDI_ASTERISK    = MAKEINTRESOURCE(32516);
  IDI_WINLOGO = MAKEINTRESOURCE(32517);

  IDI_WARNING     = IDI_EXCLAMATION;
  IDI_ERROR       = IDI_HAND;
  IDI_INFORMATION = IDI_ASTERISK;

function LoadStringA(hInstance: HINSTANCE; uID: UINT; lpBuffer: LPSTR;
  nBufferMax: Integer): Integer; stdcall;
function LoadStringW(hInstance: HINSTANCE; uID: UINT; lpBuffer: LPWSTR;
  nBufferMax: Integer): Integer; stdcall;

{$IFDEF UNICODE}
function LoadString(hInstance: HINSTANCE; uID: UINT; lpBuffer: LPWSTR;
  nBufferMax: Integer): Integer; stdcall;
{$ELSE}
function LoadString(hInstance: HINSTANCE; uID: UINT; lpBuffer: LPSTR;
  nBufferMax: Integer): Integer; stdcall;
{$ENDIF}

//
// Dialog Box Command IDs
//

const
  IDOK     = 1;
  IDCANCEL = 2;
  IDABORT  = 3;
  IDRETRY  = 4;
  IDIGNORE = 5;
  IDYES    = 6;
  IDNO     = 7;
  IDCLOSE  = 8;
  IDHELP   = 9;

  IDTRYAGAIN = 10;
  IDCONTINUE = 11;

  IDTIMEOUT  = 32000;

//
// Control Manager Structures and Definitions
//

//
// Edit Control Styles
//

  ES_LEFT        = $0000;
  ES_CENTER      = $0001;
  ES_RIGHT       = $0002;
  ES_MULTILINE   = $0004;
  ES_UPPERCASE   = $0008;
  ES_LOWERCASE   = $0010;
  ES_PASSWORD    = $0020;
  ES_AUTOVSCROLL = $0040;
  ES_AUTOHSCROLL = $0080;
  ES_NOHIDESEL   = $0100;
  ES_OEMCONVERT  = $0400;
  ES_READONLY    = $0800;
  ES_WANTRETURN  = $1000;
  ES_NUMBER      = $2000;

//
// Edit Control Notification Codes
//

  EN_SETFOCUS  = $0100;
  EN_KILLFOCUS = $0200;
  EN_CHANGE    = $0300;
  EN_UPDATE    = $0400;
  EN_ERRSPACE  = $0500;
  EN_MAXTEXT   = $0501;
  EN_HSCROLL   = $0601;
  EN_VSCROLL   = $0602;

  EN_ALIGN_LTR_EC = $0700;
  EN_ALIGN_RTL_EC = $0701;

// Edit control EM_SETMARGIN parameters//

  EC_LEFTMARGIN  = $0001;
  EC_RIGHTMARGIN = $0002;
  EC_USEFONTINFO = $ffff;

// wParam of EM_GET/SETIMESTATUS //

  EMSIS_COMPOSITIONSTRING = $0001;

// lParam for EMSIS_COMPOSITIONSTRING //

  EIMES_GETCOMPSTRATONCE         = $0001;
  EIMES_CANCELCOMPSTRINFOCUS     = $0002;
  EIMES_COMPLETECOMPSTRKILLFOCUS = $0004;

//
// Edit Control Messages
//

  EM_GETSEL              = $00B0;
  EM_SETSEL              = $00B1;
  EM_GETRECT             = $00B2;
  EM_SETRECT             = $00B3;
  EM_SETRECTNP           = $00B4;
  EM_SCROLL              = $00B5;
  EM_LINESCROLL          = $00B6;
  EM_SCROLLCARET         = $00B7;
  EM_GETMODIFY           = $00B8;
  EM_SETMODIFY           = $00B9;
  EM_GETLINECOUNT        = $00BA;
  EM_LINEINDEX           = $00BB;
  EM_SETHANDLE           = $00BC;
  EM_GETHANDLE           = $00BD;
  EM_GETTHUMB            = $00BE;
  EM_LINELENGTH          = $00C1;
  EM_REPLACESEL          = $00C2;
  EM_GETLINE             = $00C4;
  EM_LIMITTEXT           = $00C5;
  EM_CANUNDO             = $00C6;
  EM_UNDO                = $00C7;
  EM_FMTLINES            = $00C8;
  EM_LINEFROMCHAR        = $00C9;
  EM_SETTABSTOPS         = $00CB;
  EM_SETPASSWORDCHAR     = $00CC;
  EM_EMPTYUNDOBUFFER     = $00CD;
  EM_GETFIRSTVISIBLELINE = $00CE;
  EM_SETREADONLY         = $00CF;
  EM_SETWORDBREAKPROC    = $00D0;
  EM_GETWORDBREAKPROC    = $00D1;
  EM_GETPASSWORDCHAR     = $00D2;
  EM_SETMARGINS          = $00D3;
  EM_GETMARGINS          = $00D4;
  EM_SETLIMITTEXT        = EM_LIMITTEXT; // ;win40 Name change
  EM_GETLIMITTEXT        = $00D5;
  EM_POSFROMCHAR         = $00D6;
  EM_CHARFROMPOS         = $00D7;

  EM_SETIMESTATUS = $00D8;
  EM_GETIMESTATUS = $00D9;

//
// EDITWORDBREAKPROC code values
//

  WB_LEFT        = 0;
  WB_RIGHT       = 1;
  WB_ISDELIMITER = 2;

//
// Button Control Styles
//

  BS_PUSHBUTTON      = $00000000;
  BS_DEFPUSHBUTTON   = $00000001;
  BS_CHECKBOX        = $00000002;
  BS_AUTOCHECKBOX    = $00000003;
  BS_RADIOBUTTON     = $00000004;
  BS_3STATE          = $00000005;
  BS_AUTO3STATE      = $00000006;
  BS_GROUPBOX        = $00000007;
  BS_USERBUTTON      = $00000008;
  BS_AUTORADIOBUTTON = $00000009;
  BS_PUSHBOX         = $0000000A;
  BS_OWNERDRAW       = $0000000B;
  BS_TYPEMASK        = $0000000F;
  BS_LEFTTEXT        = $00000020;
  BS_TEXT            = $00000000;
  BS_ICON            = $00000040;
  BS_BITMAP          = $00000080;
  BS_LEFT            = $00000100;
  BS_RIGHT           = $00000200;
  BS_CENTER          = $00000300;
  BS_TOP             = $00000400;
  BS_BOTTOM          = $00000800;
  BS_VCENTER         = $00000C00;
  BS_PUSHLIKE        = $00001000;
  BS_MULTILINE       = $00002000;
  BS_NOTIFY          = $00004000;
  BS_FLAT            = $00008000;
  BS_RIGHTBUTTON     = BS_LEFTTEXT;

//
// User Button Notification Codes
//

  BN_CLICKED       = 0;
  BN_PAINT         = 1;
  BN_HILITE        = 2;
  BN_UNHILITE      = 3;
  BN_DISABLE       = 4;
  BN_DOUBLECLICKED = 5;
  BN_PUSHED        = BN_HILITE;
  BN_UNPUSHED      = BN_UNHILITE;
  BN_DBLCLK        = BN_DOUBLECLICKED;
  BN_SETFOCUS      = 6;
  BN_KILLFOCUS     = 7;

//
// Button Control Messages
//

  BM_GETCHECK = $00F0;
  BM_SETCHECK = $00F1;
  BM_GETSTATE = $00F2;
  BM_SETSTATE = $00F3;
  BM_SETSTYLE = $00F4;
  BM_CLICK    = $00F5;
  BM_GETIMAGE = $00F6;
  BM_SETIMAGE = $00F7;

  BST_UNCHECKED     = $0000;
  BST_CHECKED       = $0001;
  BST_INDETERMINATE = $0002;
  BST_PUSHED        = $0004;
  BST_FOCUS         = $0008;

//
// Static Control Constants
//

  SS_LEFT           = $00000000;
  SS_CENTER         = $00000001;
  SS_RIGHT          = $00000002;
  SS_ICON           = $00000003;
  SS_BLACKRECT      = $00000004;
  SS_GRAYRECT       = $00000005;
  SS_WHITERECT      = $00000006;
  SS_BLACKFRAME     = $00000007;
  SS_GRAYFRAME      = $00000008;
  SS_WHITEFRAME     = $00000009;
  SS_USERITEM       = $0000000A;
  SS_SIMPLE         = $0000000B;
  SS_LEFTNOWORDWRAP = $0000000C;
  SS_OWNERDRAW      = $0000000D;
  SS_BITMAP         = $0000000E;
  SS_ENHMETAFILE    = $0000000F;
  SS_ETCHEDHORZ     = $00000010;
  SS_ETCHEDVERT     = $00000011;
  SS_ETCHEDFRAME    = $00000012;
  SS_TYPEMASK       = $0000001F;
  SS_REALSIZECONTROL = $00000040;
  SS_NOPREFIX       = $00000080; // Don't do "&" character translation
  SS_NOTIFY         = $00000100;
  SS_CENTERIMAGE    = $00000200;
  SS_RIGHTJUST      = $00000400;
  SS_REALSIZEIMAGE  = $00000800;
  SS_SUNKEN         = $00001000;
  SS_EDITCONTROL    = $00002000;
  SS_ENDELLIPSIS    = $00004000;
  SS_PATHELLIPSIS   = $00008000;
  SS_WORDELLIPSIS   = $0000C000;
  SS_ELLIPSISMASK   = $0000C000;

//
// Static Control Mesages
//

  STM_SETICON  = $0170;
  STM_GETICON  = $0171;
  STM_SETIMAGE = $0172;
  STM_GETIMAGE = $0173;
  STN_CLICKED  = 0;
  STN_DBLCLK   = 1;
  STN_ENABLE   = 2;
  STN_DISABLE  = 3;
  STM_MSGMAX   = $0174;

//
// Dialog window class
//

  WC_DIALOG = (MAKEINTATOM($8002));

//
// Get/SetWindowWord/Long offsets for use with WC_DIALOG windows
//

  DWL_MSGRESULT = 0;
  DWL_DLGPROC   = 4;
  DWL_USER      = 8;

  DWLP_MSGRESULT = 0;
  DWLP_DLGPROC   = DWLP_MSGRESULT + SizeOf(LRESULT);
  DWLP_USER      = DWLP_DLGPROC + SizeOf(DLGPROC);

//
// Dialog Manager Routines
//

function IsDialogMessageA(hDlg: HWND; const lpMsg: MSG): BOOL; stdcall;
function IsDialogMessageW(hDlg: HWND; const lpMsg: MSG): BOOL; stdcall;

{$IFDEF UNICODE}
function IsDialogMessage(hDlg: HWND; const lpMsg: MSG): BOOL; stdcall;
{$ELSE}
function IsDialogMessage(hDlg: HWND; const lpMsg: MSG): BOOL; stdcall;
{$ENDIF}

function MapDialogRect(hDlg: HWND; var lpRect: RECT): BOOL; stdcall;

function DlgDirListA(hDlg: HWND; lpPathSpec: LPSTR; nIDListBox: Integer;
  nIDStaticPath: Integer; uFileType: UINT): Integer; stdcall;
function DlgDirListW(hDlg: HWND; lpPathSpec: LPWSTR; nIDListBox: Integer;
  nIDStaticPath: Integer; uFileType: UINT): Integer; stdcall;

{$IFDEF UNICODE}
function DlgDirList(hDlg: HWND; lpPathSpec: LPWSTR; nIDListBox: Integer;
  nIDStaticPath: Integer; uFileType: UINT): Integer; stdcall;
{$ELSE}
function DlgDirList(hDlg: HWND; lpPathSpec: LPSTR; nIDListBox: Integer;
  nIDStaticPath: Integer; uFileType: UINT): Integer; stdcall;
{$ENDIF}

//
// DlgDirList, DlgDirListComboBox flags values
//

const
  DDL_READWRITE = $0000;
  DDL_READONLY  = $0001;
  DDL_HIDDEN    = $0002;
  DDL_SYSTEM    = $0004;
  DDL_DIRECTORY = $0010;
  DDL_ARCHIVE   = $0020;

  DDL_POSTMSGS  = $2000;
  DDL_DRIVES    = $4000;
  DDL_EXCLUSIVE = $8000;

function DlgDirSelectExA(hDlg: HWND; lpString: LPSTR; nCount, nIDListBox: Integer): BOOL; stdcall;
function DlgDirSelectExW(hDlg: HWND; lpString: LPWSTR; nCount, nIDListBox: Integer): BOOL; stdcall;

{$IFDEF UNICODE}
function DlgDirSelectEx(hDlg: HWND; lpString: LPWSTR; nCount, nIDListBox: Integer): BOOL; stdcall;
{$ELSE}
function DlgDirSelectEx(hDlg: HWND; lpString: LPSTR; nCount, nIDListBox: Integer): BOOL; stdcall;
{$ENDIF}

function DlgDirListComboBoxA(hDlg: HWND; lpPathSpec: LPSTR; nIDComboBox: Integer;
  nIDStaticPath: Integer; uFiletype: UINT): Integer; stdcall;
function DlgDirListComboBoxW(hDlg: HWND; lpPathSpec: LPWSTR; nIDComboBox: Integer;
  nIDStaticPath: Integer; uFiletype: UINT): Integer; stdcall;

{$IFDEF UNICODE}
function DlgDirListComboBox(hDlg: HWND; lpPathSpec: LPWSTR; nIDComboBox: Integer;
  nIDStaticPath: Integer; uFiletype: UINT): Integer; stdcall;
{$ELSE}
function DlgDirListComboBox(hDlg: HWND; lpPathSpec: LPSTR; nIDComboBox: Integer;
  nIDStaticPath: Integer; uFiletype: UINT): Integer; stdcall;
{$ENDIF}

function DlgDirSelectComboBoxExA(hDlg: HWND; lpString: LPSTR; nCount: Integer;
  nIDComboBox: Integer): BOOL; stdcall;
function DlgDirSelectComboBoxExW(hDlg: HWND; lpString: LPWSTR; nCount: Integer;
  nIDComboBox: Integer): BOOL; stdcall;

{$IFDEF UNICODE}
function DlgDirSelectComboBoxEx(hDlg: HWND; lpString: LPWSTR; nCount: Integer;
  nIDComboBox: Integer): BOOL; stdcall;
{$ELSE}
function DlgDirSelectComboBoxEx(hDlg: HWND; lpString: LPSTR; nCount: Integer;
  nIDComboBox: Integer): BOOL; stdcall;
{$ENDIF}

//
// Dialog Styles
//

const
  DS_ABSALIGN      = $01;
  DS_SYSMODAL      = $02;
  DS_LOCALEDIT     = $20; // Edit items get Local storage.
  DS_SETFONT       = $40; // User specified font for Dlg controls
  DS_MODALFRAME    = $80; // Can be combined with WS_CAPTION
  DS_NOIDLEMSG     = $100; // WM_ENTERIDLE message will not be sent
  DS_SETFOREGROUND = $200; // not in win3.1

  DS_3DLOOK       = $0004;
  DS_FIXEDSYS     = $0008;
  DS_NOFAILCREATE = $0010;
  DS_CONTROL      = $0400;
  DS_CENTER       = $0800;
  DS_CENTERMOUSE  = $1000;
  DS_CONTEXTHELP  = $2000;

  DS_SHELLFONT = (DS_SETFONT or DS_FIXEDSYS);

//#if(_WIN32_WCE >= 0x0500)
  DS_USEPIXELS = $8000;
//#endif

  DM_GETDEFID = (WM_USER+0);
  DM_SETDEFID = (WM_USER+1);

  DM_REPOSITION = (WM_USER+2);

//
// Returned in HIWORD() of DM_GETDEFID result if msg is supported
//

  DC_HASDEFID = $534B;

//
// Dialog Codes
//

  DLGC_WANTARROWS      = $0001; // Control wants arrow keys
  DLGC_WANTTAB         = $0002; // Control wants tab keys
  DLGC_WANTALLKEYS     = $0004; // Control wants all keys
  DLGC_WANTMESSAGE     = $0004; // Pass message to control
  DLGC_HASSETSEL       = $0008; // Understands EM_SETSEL message
  DLGC_DEFPUSHBUTTON   = $0010; // Default pushbutton
  DLGC_UNDEFPUSHBUTTON = $0020; // Non-default pushbutton
  DLGC_RADIOBUTTON     = $0040; // Radio button
  DLGC_WANTCHARS       = $0080; // Want WM_CHAR messages
  DLGC_STATIC          = $0100; // Static item: don't include
  DLGC_BUTTON          = $2000; // Button item: can be checked

  LB_CTLCODE = 0;

//
// Listbox Return Values
//

  LB_OKAY     = 0;
  LB_ERR      = DWORD(-1);
  LB_ERRSPACE = DWORD(-2);

//
//  The idStaticPath parameter to DlgDirList can have the following values
//  ORed if the list box should show other details of the files along with
//  the name of the files;
//
// all other details also will be returned

//
// Listbox Notification Codes
//

  LBN_ERRSPACE  = DWORD(-2);
  LBN_SELCHANGE = 1;
  LBN_DBLCLK    = 2;
  LBN_SELCANCEL = 3;
  LBN_SETFOCUS  = 4;
  LBN_KILLFOCUS = 5;

//
// Listbox messages
//

  LB_ADDSTRING           = $0180;
  LB_INSERTSTRING        = $0181;
  LB_DELETESTRING        = $0182;
  LB_SELITEMRANGEEX      = $0183;
  LB_RESETCONTENT        = $0184;
  LB_SETSEL              = $0185;
  LB_SETCURSEL           = $0186;
  LB_GETSEL              = $0187;
  LB_GETCURSEL           = $0188;
  LB_GETTEXT             = $0189;
  LB_GETTEXTLEN          = $018A;
  LB_GETCOUNT            = $018B;
  LB_SELECTSTRING        = $018C;
  LB_DIR                 = $018D;
  LB_GETTOPINDEX         = $018E;
  LB_FINDSTRING          = $018F;
  LB_GETSELCOUNT         = $0190;
  LB_GETSELITEMS         = $0191;
  LB_SETTABSTOPS         = $0192;
  LB_GETHORIZONTALEXTENT = $0193;
  LB_SETHORIZONTALEXTENT = $0194;
  LB_SETCOLUMNWIDTH      = $0195;
  LB_ADDFILE             = $0196;
  LB_SETTOPINDEX         = $0197;
  LB_GETITEMRECT         = $0198;
  LB_GETITEMDATA         = $0199;
  LB_SETITEMDATA         = $019A;
  LB_SELITEMRANGE        = $019B;
  LB_SETANCHORINDEX      = $019C;
  LB_GETANCHORINDEX      = $019D;
  LB_SETCARETINDEX       = $019E;
  LB_GETCARETINDEX       = $019F;
  LB_SETITEMHEIGHT       = $01A0;
  LB_GETITEMHEIGHT       = $01A1;
  LB_FINDSTRINGEXACT     = $01A2;
  LB_SETLOCALE           = $01A5;
  LB_GETLOCALE           = $01A6;
  LB_SETCOUNT            = $01A7;
  LB_INITSTORAGE   = $01A8;
  LB_ITEMFROMPOINT = $01A9;
  LB_MULTIPLEADDSTRING = $01B1;
  LB_GETLISTBOXINFO    = $01B2;

{$IFDEF WINVER_0400_GREATER}
  LB_MSGMAX = $01B3;
{$ELSE}
{$IFDEF WINVER_0400_GREATER}
  LB_MSGMAX = $01B0;
{$ELSE}
  LB_MSGMAX = $01A8;
{$ENDIF}
{$ENDIF}

//
// Listbox Styles
//

  LBS_NOTIFY            = $0001;
  LBS_SORT              = $0002;
  LBS_NOREDRAW          = $0004;
  LBS_MULTIPLESEL       = $0008;
  LBS_OWNERDRAWFIXED    = $0010;
  LBS_OWNERDRAWVARIABLE = $0020;
  LBS_HASSTRINGS        = $0040;
  LBS_USETABSTOPS       = $0080;
  LBS_NOINTEGRALHEIGHT  = $0100;
  LBS_MULTICOLUMN       = $0200;
  LBS_WANTKEYBOARDINPUT = $0400;
  LBS_EXTENDEDSEL       = $0800;
  LBS_DISABLENOSCROLL   = $1000;
  LBS_NODATA            = $2000;
  LBS_NOSEL             = $4000;
  LBS_COMBOBOX          = $8000;
  
  LBS_STANDARD          = (LBS_NOTIFY or LBS_SORT or WS_VSCROLL or WS_BORDER);

//
// Combo Box return Values
//

  CB_OKAY     = 0;
  CB_ERR      = DWORD(-1);
  CB_ERRSPACE = DWORD(-2);

//
// Combo Box Notification Codes
//

  CBN_ERRSPACE     = DWORD(-1);
  CBN_SELCHANGE    = 1;
  CBN_DBLCLK       = 2;
  CBN_SETFOCUS     = 3;
  CBN_KILLFOCUS    = 4;
  CBN_EDITCHANGE   = 5;
  CBN_EDITUPDATE   = 6;
  CBN_DROPDOWN     = 7;
  CBN_CLOSEUP      = 8;
  CBN_SELENDOK     = 9;
  CBN_SELENDCANCEL = 10;

//
// Combo Box styles
//

  CBS_SIMPLE            = $0001;
  CBS_DROPDOWN          = $0002;
  CBS_DROPDOWNLIST      = $0003;
  CBS_OWNERDRAWFIXED    = $0010;
  CBS_OWNERDRAWVARIABLE = $0020;
  CBS_AUTOHSCROLL       = $0040;
  CBS_OEMCONVERT        = $0080;
  CBS_SORT              = $0100;
  CBS_HASSTRINGS        = $0200;
  CBS_NOINTEGRALHEIGHT  = $0400;
  CBS_DISABLENOSCROLL   = $0800;
  CBS_UPPERCASE         = $2000;
  CBS_LOWERCASE         = $4000;

//
// Combo Box messages
//

  CB_GETEDITSEL            = $0140;
  CB_LIMITTEXT             = $0141;
  CB_SETEDITSEL            = $0142;
  CB_ADDSTRING             = $0143;
  CB_DELETESTRING          = $0144;
  CB_DIR                   = $0145;
  CB_GETCOUNT              = $0146;
  CB_GETCURSEL             = $0147;
  CB_GETLBTEXT             = $0148;
  CB_GETLBTEXTLEN          = $0149;
  CB_INSERTSTRING          = $014A;
  CB_RESETCONTENT          = $014B;
  CB_FINDSTRING            = $014C;
  CB_SELECTSTRING          = $014D;
  CB_SETCURSEL             = $014E;
  CB_SHOWDROPDOWN          = $014F;
  CB_GETITEMDATA           = $0150;
  CB_SETITEMDATA           = $0151;
  CB_GETDROPPEDCONTROLRECT = $0152;
  CB_SETITEMHEIGHT         = $0153;
  CB_GETITEMHEIGHT         = $0154;
  CB_SETEXTENDEDUI         = $0155;
  CB_GETEXTENDEDUI         = $0156;
  CB_GETDROPPEDSTATE       = $0157;
  CB_FINDSTRINGEXACT       = $0158;
  CB_SETLOCALE             = $0159;
  CB_GETLOCALE             = $015A;
  CB_GETTOPINDEX           = $015b;
  CB_SETTOPINDEX           = $015c;
  CB_GETHORIZONTALEXTENT   = $015d;
  CB_SETHORIZONTALEXTENT   = $015e;
  CB_GETDROPPEDWIDTH       = $015f;
  CB_SETDROPPEDWIDTH       = $0160;
  CB_INITSTORAGE           = $0161;
  CB_MULTIPLEADDSTRING     = $0163;
  CB_GETCOMBOBOXINFO       = $0164;

{$IFDEF WINVER_0400_GREATER}
  CB_MSGMAX = $0165;
{$ELSE}
{$IFDEF WINVER_0400_GREATER}
  CB_MSGMAX = $0162;
{$ELSE}
  CB_MSGMAX = $015B;
{$ENDIF}
{$ENDIF}

//
// Scroll Bar Styles
//

  SBS_HORZ                    = $0000;
  SBS_VERT                    = $0001;
  SBS_TOPALIGN                = $0002;
  SBS_LEFTALIGN               = $0002;
  SBS_BOTTOMALIGN             = $0004;
  SBS_RIGHTALIGN              = $0004;
  SBS_SIZEBOXTOPLEFTALIGN     = $0002;
  SBS_SIZEBOXBOTTOMRIGHTALIGN = $0004;
  SBS_SIZEBOX                 = $0008;
  SBS_SIZEGRIP                = $0010;

//
// Scroll bar messages
//

  SBM_SETPOS         = $00E0; // not in win3.1
  SBM_GETPOS         = $00E1; // not in win3.1
  SBM_SETRANGE       = $00E2; // not in win3.1
  SBM_SETRANGEREDRAW = $00E6; // not in win3.1
  SBM_GETRANGE       = $00E3; // not in win3.1
  SBM_ENABLE_ARROWS  = $00E4; // not in win3.1
  SBM_SETSCROLLINFO  = $00E9;
  SBM_GETSCROLLINFO  = $00EA;
  
  SBM_GETSCROLLBARINFO = $00EB;

  SIF_RANGE           = $0001;
  SIF_PAGE            = $0002;
  SIF_POS             = $0004;
  SIF_DISABLENOSCROLL = $0008;
  SIF_TRACKPOS        = $0010;
  SIF_ALL             = (SIF_RANGE or SIF_PAGE or SIF_POS or SIF_TRACKPOS);

type
  LPSCROLLINFO = ^SCROLLINFO;
  tagSCROLLINFO = record
    cbSize: UINT;
    fMask: UINT;
    nMin: Integer;
    nMax: Integer;
    nPage: UINT;
    nPos: Integer;
    nTrackPos: Integer;
  end;
  SCROLLINFO = tagSCROLLINFO;
  TScrollInfo = SCROLLINFO;
  PScrollInfo = LPSCROLLINFO;

function SetScrollInfo(hwnd: HWND; fnBar: Integer; const lpsi: SCROLLINFO;
  fRedraw: BOOL): Integer; stdcall;

function GetScrollInfo(hwnd: HWND; fnBar: Integer; var lpsi: SCROLLINFO): BOOL; stdcall;

//
// MDI client style bits
//

const
  MDIS_ALLCHILDSTYLES = $0001;

//
// wParam Flags for WM_MDITILE and WM_MDICASCADE messages.
//

const
  MDITILE_VERTICAL     = $0000; // not in win3.1
  MDITILE_HORIZONTAL   = $0001; // not in win3.1
  MDITILE_SKIPDISABLED = $0002; // not in win3.1
  MDITILE_ZORDER       = $0004;

type
  LPMDICREATESTRUCTA = ^MDICREATESTRUCTA;
  tagMDICREATESTRUCTA = record
    szClass: LPCSTR;
    szTitle: LPCSTR;
    hOwner: HANDLE;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    style: DWORD;
    lParam: LPARAM; // app-defined stuff//
  end;
  MDICREATESTRUCTA = tagMDICREATESTRUCTA;
  TMdiCreateStructA = MDICREATESTRUCTA;
  PMdiCreateStructA = LPMDICREATESTRUCTA;

  LPMDICREATESTRUCTW = ^MDICREATESTRUCTW;
  tagMDICREATESTRUCTW = record
    szClass: LPCWSTR;
    szTitle: LPCWSTR;
    hOwner: HANDLE;
    x: Integer;
    y: Integer;
    cx: Integer;
    cy: Integer;
    style: DWORD;
    lParam: LPARAM; // app-defined stuff//
  end;
  MDICREATESTRUCTW = tagMDICREATESTRUCTW;
  TMdiCreateStructW = MDICREATESTRUCTW;
  PMdiCreateStructW = LPMDICREATESTRUCTW;

{$IFDEF UNICODE}
  MDICREATESTRUCT = MDICREATESTRUCTW;
  LPMDICREATESTRUCT = LPMDICREATESTRUCTW;
  TMdiCreateStruct = TMdiCreateStructW;
  PMdiCreateStruct = PMdiCreateStructW;
{$ELSE}
  MDICREATESTRUCT = MDICREATESTRUCTA;
  LPMDICREATESTRUCT = LPMDICREATESTRUCTA;
  TMdiCreateStruct = TMdiCreateStructA;
  PMdiCreateStruct = PMdiCreateStructA;
{$ENDIF}

  LPCLIENTCREATESTRUCT = ^CLIENTCREATESTRUCT;
  tagCLIENTCREATESTRUCT = record
    hWindowMenu: HANDLE;
    idFirstChild: UINT;
  end;
  CLIENTCREATESTRUCT = tagCLIENTCREATESTRUCT;
  TClientCreateStruct = CLIENTCREATESTRUCT;
  PClientCreateStruct = LPCLIENTCREATESTRUCT;

function DefFrameProcA(hWnd: HWND; hWndMDIClient: HWND; uMsg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function DefFrameProcW(hWnd: HWND; hWndMDIClient: HWND; uMsg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function DefFrameProc(hWnd: HWND; hWndMDIClient: HWND; uMsg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function DefFrameProc(hWnd: HWND; hWndMDIClient: HWND; uMsg: UINT;
  wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

function DefMDIChildProcA(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
function DefMDIChildProcW(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;

{$IFDEF UNICODE}
function DefMDIChildProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function DefMDIChildProc(hWnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}

function TranslateMDISysAccel(hWndClient: HWND; const lpMsg: MSG): BOOL; stdcall;

function ArrangeIconicWindows(hWnd: HWND): UINT; stdcall;

function CreateMDIWindowA(lpClassName, lpWindowName: LPCSTR; dwStyle: DWORD;
  X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hInstance: HINSTANCE;
  lParam: LPARAM): HWND; stdcall;
function CreateMDIWindowW(lpClassName, lpWindowName: LPCWSTR; dwStyle: DWORD;
  X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hInstance: HINSTANCE;
  lParam: LPARAM): HWND; stdcall;

{$IFDEF UNICODE}
function CreateMDIWindow(lpClassName, lpWindowName: LPCWSTR; dwStyle: DWORD;
  X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hInstance: HINSTANCE;
  lParam: LPARAM): HWND; stdcall;
{$ELSE}
function CreateMDIWindow(lpClassName, lpWindowName: LPCSTR; dwStyle: DWORD;
  X, Y, nWidth, nHeight: Integer; hWndParent: HWND; hInstance: HINSTANCE;
  lParam: LPARAM): HWND; stdcall;
{$ENDIF}

function TileWindows(hwndParent: HWND; wHow: UINT; lpRect: LPRECT; cKids: UINT;
  hwnd: HWND; lpKids: LPHWND): WORD; stdcall;

function CascadeWindows(hwndParent: HWND; wHow: UINT; lpRect: LPRECT;
  cKids: UINT; lpKids: LPHWND): WORD; stdcall;

//***** Help support ********************************************************/

type
  HELPPOLY = DWORD;

  LPMULTIKEYHELPA = ^MULTIKEYHELPA;
  tagMULTIKEYHELPA = record
    mkSize: DWORD;
    mkKeylist: CHAR;
    szKeyphrase: array [0..0] of CHAR;
  end;
  MULTIKEYHELPA = tagMULTIKEYHELPA;
  TMultiKeyHelpA = MULTIKEYHELPA;
  PMultiKeyHelpA = LPMULTIKEYHELPA;

  LPMULTIKEYHELPW = ^MULTIKEYHELPW;
  tagMULTIKEYHELPW = record
    mkSize: DWORD;
    mkKeylist: WCHAR;
    szKeyphrase: array [0..0] of WCHAR;
  end;
  MULTIKEYHELPW = tagMULTIKEYHELPW;
  TMultiKeyHelpW = MULTIKEYHELPW;
  PMultiKeyHelpW = LPMULTIKEYHELPW;

{$IFDEF UNICODE}
  MULTIKEYHELP = MULTIKEYHELPW;
  LPMULTIKEYHELP = LPMULTIKEYHELPW;
  TMultiKeyHelp = TMultiKeyHelpW;
  PMultiKeyHelp = PMultiKeyHelpW;
{$ELSE}
  MULTIKEYHELP = MULTIKEYHELPA;
  LPMULTIKEYHELP = LPMULTIKEYHELPA;
  TMultiKeyHelp = TMultiKeyHelpA;
  PMultiKeyHelp = PMultiKeyHelpA;
{$ENDIF}

  LPHELPWININFOA = ^HELPWININFOA;
  tagHELPWININFOA = record
    wStructSize: Integer;
    x: Integer;
    y: Integer;
    dx: Integer;
    dy: Integer;
    wMax: Integer;
    rgchMember: array [0..1] of CHAR;
  end;
  HELPWININFOA = tagHELPWININFOA;
  THelpWinInfoA = HELPWININFOA;
  PHelpWinInfoA = LPHELPWININFOA;

  LPHELPWININFOW = ^HELPWININFOW;
  tagHELPWININFOW = record
    wStructSize: Integer;
    x: Integer;
    y: Integer;
    dx: Integer;
    dy: Integer;
    wMax: Integer;
    rgchMember: array [0..1] of WCHAR;
  end;
  HELPWININFOW = tagHELPWININFOW;
  THelpWinInfoW = HELPWININFOW;
  PHelpWinInfoW = LPHELPWININFOW;

{$IFDEF UNICODE}
  HELPWININFO = HELPWININFOW;
  LPHELPWININFO = LPHELPWININFOW;
  THelpWinInfo = THelpWinInfoW;
  PHelpWinInfo = PHelpWinInfoW;
{$ELSE}
  HELPWININFO = HELPWININFOA;
  LPHELPWININFO = LPHELPWININFOA;
  THelpWinInfo = THelpWinInfoA;
  PHelpWinInfo = PHelpWinInfoA;
{$ENDIF}

//
// Commands to pass to WinHelp()
//

const
  HELP_CONTEXT      = $0001; // Display topic in ulTopic
  HELP_QUIT         = $0002; // Terminate help
  HELP_INDEX        = $0003; // Display index
  HELP_CONTENTS     = $0003;
  HELP_HELPONHELP   = $0004; // Display help on using help
  HELP_SETINDEX     = $0005; // Set current Index for multi index help
  HELP_SETCONTENTS  = $0005;
  HELP_CONTEXTPOPUP = $0008;
  HELP_FORCEFILE    = $0009;
  HELP_KEY          = $0101; // Display topic for keyword in offabData
  HELP_COMMAND      = $0102;
  HELP_PARTIALKEY   = $0105;
  HELP_MULTIKEY     = $0201;
  HELP_SETWINPOS    = $0203;

  HELP_CONTEXTMENU  = $000a;
  HELP_FINDER       = $000b;
  HELP_WM_HELP      = $000c;
  HELP_SETPOPUP_POS = $000d;

  HELP_TCARD              = $8000;
  HELP_TCARD_DATA         = $0010;
  HELP_TCARD_OTHER_CALLER = $0011;

// These are in winhelp.h in Win95.

  IDH_NO_HELP             = 28440;
  IDH_MISSING_CONTEXT     = 28441; // Control doesn't have matching help context
  IDH_GENERIC_HELP_BUTTON = 28442; // Property sheet help button
  IDH_OK                  = 28443;
  IDH_CANCEL              = 28444;
  IDH_HELP                = 28445;

function WinHelpA(hWndMain: HWND; lpszHelp: LPCSTR; uCommand: UINT; dwData: ULONG_PTR): BOOL; stdcall;
function WinHelpW(hWndMain: HWND; lpszHelp: LPCWSTR; uCommand: UINT; dwData: ULONG_PTR): BOOL; stdcall;

{$IFDEF UNICODE}
function WinHelp(hWndMain: HWND; lpszHelp: LPCWSTR; uCommand: UINT; dwData: ULONG_PTR): BOOL; stdcall;
{$ELSE}
function WinHelp(hWndMain: HWND; lpszHelp: LPCSTR; uCommand: UINT; dwData: ULONG_PTR): BOOL; stdcall;
{$ENDIF}

const
  GR_GDIOBJECTS  = 0; // Count of GDI objects
  GR_USEROBJECTS = 1; // Count of USER objects

function GetGuiResources(hProcess: HANDLE; uiFlags: DWORD): DWORD; stdcall;

//
// Parameter for SystemParametersInfo()
//

const
  SPI_GETBEEP               = 1;
  SPI_SETBEEP               = 2;
  SPI_GETMOUSE              = 3;
  SPI_SETMOUSE              = 4;
  SPI_GETBORDER             = 5;
  SPI_SETBORDER             = 6;
  SPI_GETKEYBOARDSPEED      = 10;
  SPI_SETKEYBOARDSPEED      = 11;
  SPI_LANGDRIVER            = 12;
  SPI_ICONHORIZONTALSPACING = 13;
  SPI_GETSCREENSAVETIMEOUT  = 14;
  SPI_SETSCREENSAVETIMEOUT  = 15;
  SPI_GETSCREENSAVEACTIVE   = 16;
  SPI_SETSCREENSAVEACTIVE   = 17;
  SPI_GETGRIDGRANULARITY    = 18;
  SPI_SETGRIDGRANULARITY    = 19;
  SPI_SETDESKWALLPAPER      = 20;
  SPI_SETDESKPATTERN        = 21;
  SPI_GETKEYBOARDDELAY      = 22;
  SPI_SETKEYBOARDDELAY      = 23;
  SPI_ICONVERTICALSPACING   = 24;
  SPI_GETICONTITLEWRAP      = 25;
  SPI_SETICONTITLEWRAP      = 26;
  SPI_GETMENUDROPALIGNMENT  = 27;
  SPI_SETMENUDROPALIGNMENT  = 28;
  SPI_SETDOUBLECLKWIDTH     = 29;
  SPI_SETDOUBLECLKHEIGHT    = 30;
  SPI_GETICONTITLELOGFONT   = 31;
  SPI_SETDOUBLECLICKTIME    = 32;
  SPI_SETMOUSEBUTTONSWAP    = 33;
  SPI_SETICONTITLELOGFONT   = 34;
  SPI_GETFASTTASKSWITCH     = 35;
  SPI_SETFASTTASKSWITCH     = 36;
  SPI_SETDRAGFULLWINDOWS    = 37;
  SPI_GETDRAGFULLWINDOWS    = 38;
  SPI_GETNONCLIENTMETRICS   = 41;
  SPI_SETNONCLIENTMETRICS   = 42;
  SPI_GETMINIMIZEDMETRICS   = 43;
  SPI_SETMINIMIZEDMETRICS   = 44;
  SPI_GETICONMETRICS        = 45;
  SPI_SETICONMETRICS        = 46;
  SPI_SETWORKAREA           = 47;
  SPI_GETWORKAREA           = 48;
  SPI_SETPENWINDOWS         = 49;

  SPI_GETHIGHCONTRAST       = 66;
  SPI_SETHIGHCONTRAST       = 67;
  SPI_GETKEYBOARDPREF       = 68;
  SPI_SETKEYBOARDPREF       = 69;
  SPI_GETSCREENREADER       = 70;
  SPI_SETSCREENREADER       = 71;
  SPI_GETANIMATION          = 72;
  SPI_SETANIMATION          = 73;
  SPI_GETFONTSMOOTHING      = 74;
  SPI_SETFONTSMOOTHING      = 75;
  SPI_SETDRAGWIDTH          = 76;
  SPI_SETDRAGHEIGHT         = 77;
  SPI_SETHANDHELD           = 78;
  SPI_GETLOWPOWERTIMEOUT    = 79;
  SPI_GETPOWEROFFTIMEOUT    = 80;
  SPI_SETLOWPOWERTIMEOUT    = 81;
  SPI_SETPOWEROFFTIMEOUT    = 82;
  SPI_GETLOWPOWERACTIVE     = 83;
  SPI_GETPOWEROFFACTIVE     = 84;
  SPI_SETLOWPOWERACTIVE     = 85;
  SPI_SETPOWEROFFACTIVE     = 86;
  SPI_SETCURSORS            = 87;
  SPI_SETICONS              = 88;
  SPI_GETDEFAULTINPUTLANG   = 89;
  SPI_SETDEFAULTINPUTLANG   = 90;
  SPI_SETLANGTOGGLE         = 91;
  SPI_GETWINDOWSEXTENSION   = 92;
  SPI_SETMOUSETRAILS        = 93;
  SPI_GETMOUSETRAILS        = 94;
  SPI_SETSCREENSAVERRUNNING = 97;
  SPI_SCREENSAVERRUNNING    = SPI_SETSCREENSAVERRUNNING;
  SPI_GETFILTERKEYS         = 50;
  SPI_SETFILTERKEYS         = 51;
  SPI_GETTOGGLEKEYS         = 52;
  SPI_SETTOGGLEKEYS         = 53;
  SPI_GETMOUSEKEYS          = 54;
  SPI_SETMOUSEKEYS          = 55;
  SPI_GETSHOWSOUNDS         = 56;
  SPI_SETSHOWSOUNDS         = 57;
  SPI_GETSTICKYKEYS         = 58;
  SPI_SETSTICKYKEYS         = 59;
  SPI_GETACCESSTIMEOUT      = 60;
  SPI_SETACCESSTIMEOUT      = 61;
  SPI_GETSERIALKEYS         = 62;
  SPI_SETSERIALKEYS         = 63;
  SPI_GETSOUNDSENTRY        = 64;
  SPI_SETSOUNDSENTRY        = 65;
  SPI_GETSNAPTODEFBUTTON    = 95;
  SPI_SETSNAPTODEFBUTTON    = 96;
  SPI_GETMOUSEHOVERWIDTH    = 98;
  SPI_SETMOUSEHOVERWIDTH    = 99;
  SPI_GETMOUSEHOVERHEIGHT   = 100;
  SPI_SETMOUSEHOVERHEIGHT   = 101;
  SPI_GETMOUSEHOVERTIME     = 102;
  SPI_SETMOUSEHOVERTIME     = 103;
  SPI_GETWHEELSCROLLLINES   = 104;
  SPI_SETWHEELSCROLLLINES   = 105;
  SPI_GETMENUSHOWDELAY      = 106;
  SPI_SETMENUSHOWDELAY      = 107;

  SPI_GETSHOWIMEUI = 110;
  SPI_SETSHOWIMEUI = 111;

  SPI_GETMOUSESPEED         = 112;
  SPI_SETMOUSESPEED         = 113;
  SPI_GETSCREENSAVERRUNNING = 114;
  SPI_GETDESKWALLPAPER      = 115;

  SPI_GETACTIVEWINDOWTRACKING   = $1000;
  SPI_SETACTIVEWINDOWTRACKING   = $1001;
  SPI_GETMENUANIMATION          = $1002;
  SPI_SETMENUANIMATION          = $1003;
  SPI_GETCOMBOBOXANIMATION      = $1004;
  SPI_SETCOMBOBOXANIMATION      = $1005;
  SPI_GETLISTBOXSMOOTHSCROLLING = $1006;
  SPI_SETLISTBOXSMOOTHSCROLLING = $1007;
  SPI_GETGRADIENTCAPTIONS       = $1008;
  SPI_SETGRADIENTCAPTIONS       = $1009;
  SPI_GETKEYBOARDCUES           = $100A;
  SPI_SETKEYBOARDCUES           = $100B;
  SPI_GETMENUUNDERLINES         = SPI_GETKEYBOARDCUES;
  SPI_SETMENUUNDERLINES         = SPI_SETKEYBOARDCUES;
  SPI_GETACTIVEWNDTRKZORDER     = $100C;
  SPI_SETACTIVEWNDTRKZORDER     = $100D;
  SPI_GETHOTTRACKING            = $100E;
  SPI_SETHOTTRACKING            = $100F;
  SPI_GETMENUFADE               = $1012;
  SPI_SETMENUFADE               = $1013;
  SPI_GETSELECTIONFADE          = $1014;
  SPI_SETSELECTIONFADE          = $1015;
  SPI_GETTOOLTIPANIMATION       = $1016;
  SPI_SETTOOLTIPANIMATION       = $1017;
  SPI_GETTOOLTIPFADE            = $1018;
  SPI_SETTOOLTIPFADE            = $1019;
  SPI_GETCURSORSHADOW           = $101A;
  SPI_SETCURSORSHADOW           = $101B;

  SPI_GETMOUSESONAR             = $101C;
  SPI_SETMOUSESONAR             = $101D;
  SPI_GETMOUSECLICKLOCK         = $101E;
  SPI_SETMOUSECLICKLOCK         = $101F;
  SPI_GETMOUSEVANISH            = $1020;
  SPI_SETMOUSEVANISH            = $1021;
  SPI_GETFLATMENU               = $1022;
  SPI_SETFLATMENU               = $1023;
  SPI_GETDROPSHADOW             = $1024;
  SPI_SETDROPSHADOW             = $1025;
  SPI_GETBLOCKSENDINPUTRESETS   = $1026;
  SPI_SETBLOCKSENDINPUTRESETS   = $1027;

  SPI_GETUIEFFECTS = $103E;
  SPI_SETUIEFFECTS = $103F;

  SPI_GETFOREGROUNDLOCKTIMEOUT = $2000;
  SPI_SETFOREGROUNDLOCKTIMEOUT = $2001;
  SPI_GETACTIVEWNDTRKTIMEOUT   = $2002;
  SPI_SETACTIVEWNDTRKTIMEOUT   = $2003;
  SPI_GETFOREGROUNDFLASHCOUNT  = $2004;
  SPI_SETFOREGROUNDFLASHCOUNT  = $2005;
  SPI_GETCARETWIDTH            = $2006;
  SPI_SETCARETWIDTH            = $2007;

  SPI_GETMOUSECLICKLOCKTIME    = $2008;
  SPI_SETMOUSECLICKLOCKTIME    = $2009;
  SPI_GETFONTSMOOTHINGTYPE     = $200A;
  SPI_SETFONTSMOOTHINGTYPE     = $200B;

// constants for SPI_GETFONTSMOOTHINGTYPE and SPI_SETFONTSMOOTHINGTYPE

  FE_FONTSMOOTHINGSTANDARD     = $0001;
  FE_FONTSMOOTHINGCLEARTYPE    = $0002;
  FE_FONTSMOOTHINGDOCKING      = $8000;

  SPI_GETFONTSMOOTHINGCONTRAST = $200C;
  SPI_SETFONTSMOOTHINGCONTRAST = $200D;

  SPI_GETFOCUSBORDERWIDTH      = $200E;
  SPI_SETFOCUSBORDERWIDTH      = $200F;
  SPI_GETFOCUSBORDERHEIGHT     = $2010;
  SPI_SETFOCUSBORDERHEIGHT     = $2011;

  SPI_GETFONTSMOOTHINGORIENTATION = $2012;
  SPI_SETFONTSMOOTHINGORIENTATION = $2013;

// constants for SPI_GETFONTSMOOTHINGORIENTATION and SPI_SETFONTSMOOTHINGORIENTATION:

  FE_FONTSMOOTHINGORIENTATIONBGR = $0000;
  FE_FONTSMOOTHINGORIENTATIONRGB = $0001;

//
// Flags
//

  SPIF_UPDATEINIFILE    = $0001;
  SPIF_SENDWININICHANGE = $0002;
  SPIF_SENDCHANGE       = SPIF_SENDWININICHANGE;

  METRICS_USEDEFAULT = DWORD(-1);

type
  LPNONCLIENTMETRICSA = ^NONCLIENTMETRICSA;
  tagNONCLIENTMETRICSA = record
    cbSize: UINT;
    iBorderWidth: Integer;
    iScrollWidth: Integer;
    iScrollHeight: Integer;
    iCaptionWidth: Integer;
    iCaptionHeight: Integer;
    lfCaptionFont: LOGFONTA;
    iSmCaptionWidth: Integer;
    iSmCaptionHeight: Integer;
    lfSmCaptionFont: LOGFONTA;
    iMenuWidth: Integer;
    iMenuHeight: Integer;
    lfMenuFont: LOGFONTA;
    lfStatusFont: LOGFONTA;
    lfMessageFont: LOGFONTA;
  end;
  NONCLIENTMETRICSA = tagNONCLIENTMETRICSA;
  TNonClientMetricsA = NONCLIENTMETRICSA;
  PNonClientMetricsA = LPNONCLIENTMETRICSA;

  LPNONCLIENTMETRICSW = ^NONCLIENTMETRICSW;
  tagNONCLIENTMETRICSW = record
    cbSize: UINT;
    iBorderWidth: Integer;
    iScrollWidth: Integer;
    iScrollHeight: Integer;
    iCaptionWidth: Integer;
    iCaptionHeight: Integer;
    lfCaptionFont: LOGFONTW;
    iSmCaptionWidth: Integer;
    iSmCaptionHeight: Integer;
    lfSmCaptionFont: LOGFONTW;
    iMenuWidth: Integer;
    iMenuHeight: Integer;
    lfMenuFont: LOGFONTW;
    lfStatusFont: LOGFONTW;
    lfMessageFont: LOGFONTW;
  end;
  NONCLIENTMETRICSW = tagNONCLIENTMETRICSW;
  TNonClientMetricsW = NONCLIENTMETRICSW;
  PNonClientMetricsW = LPNONCLIENTMETRICSW;

{$IFDEF UNICODE}
  NONCLIENTMETRICS = NONCLIENTMETRICSW;
  LPNONCLIENTMETRICS = LPNONCLIENTMETRICSW;
  TNonClientMetrics = TNonClientMetricsW;
  PNonClientMetrics = PNonClientMetricsW;
{$ELSE}
  NONCLIENTMETRICS = NONCLIENTMETRICSA;
  LPNONCLIENTMETRICS = LPNONCLIENTMETRICSA;
  TNonClientMetrics = TNonClientMetricsA;
  PNonClientMetrics = PNonClientMetricsA;
{$ENDIF}

const
  ARW_BOTTOMLEFT  = $0000;
  ARW_BOTTOMRIGHT = $0001;
  ARW_TOPLEFT     = $0002;
  ARW_TOPRIGHT    = $0003;
  ARW_STARTMASK   = $0003;
  ARW_STARTRIGHT  = $0001;
  ARW_STARTTOP    = $0002;

  ARW_LEFT  = $0000;
  ARW_RIGHT = $0000;
  ARW_UP    = $0004;
  ARW_DOWN  = $0004;
  ARW_HIDE  = $0008;

type
  LPMINIMIZEDMETRICS = ^MINIMIZEDMETRICS;
  tagMINIMIZEDMETRICS = record
    cbSize: UINT;
    iWidth: Integer;
    iHorzGap: Integer;
    iVertGap: Integer;
    iArrange: Integer;
  end;
  MINIMIZEDMETRICS = tagMINIMIZEDMETRICS;
  TMinimizedMetrics = MINIMIZEDMETRICS;
  PMinimizedMetrics = LPMINIMIZEDMETRICS;

  LPICONMETRICSA = ^ICONMETRICSA;
  tagICONMETRICSA = record
    cbSize: UINT;
    iHorzSpacing: Integer;
    iVertSpacing: Integer;
    iTitleWrap: Integer;
    lfFont: LOGFONTA;
  end;
  ICONMETRICSA = tagICONMETRICSA;
  TIconMetricsA = ICONMETRICSA;
  PIconMetricsA = LPICONMETRICSA;

  LPICONMETRICSW = ^ICONMETRICSW;
  tagICONMETRICSW = record
    cbSize: UINT;
    iHorzSpacing: Integer;
    iVertSpacing: Integer;
    iTitleWrap: Integer;
    lfFont: LOGFONTW;
  end;
  ICONMETRICSW = tagICONMETRICSW;
  TIconMetricsW = ICONMETRICSW;
  PIconMetricsW = LPICONMETRICSW;

{$IFDEF UNICODE}
  ICONMETRICS = ICONMETRICSW;
  LPICONMETRICS = LPICONMETRICSW;
  TIconMetrics = TIconMetricsW;
  PIconMetrics = PIconMetricsW;
{$ELSE}
  ICONMETRICS = ICONMETRICSA;
  LPICONMETRICS = LPICONMETRICSA;
  TIconMetrics = TIconMetricsA;
  PIconMetrics = PIconMetricsA;
{$ENDIF}

  LPANIMATIONINFO = ^ANIMATIONINFO;
  tagANIMATIONINFO = record
    cbSize: UINT;
    iMinAnimate: Integer;
  end;
  ANIMATIONINFO = tagANIMATIONINFO;
  TAnimationInfo = ANIMATIONINFO;
  PAnimationInfo = LPANIMATIONINFO;

  LPSERIALKEYSA = ^SERIALKEYSA;
  tagSERIALKEYSA = record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszActivePort: LPSTR;
    lpszPort: LPSTR;
    iBaudRate: UINT;
    iPortState: UINT;
    iActive: UINT;
  end;
  SERIALKEYSA = tagSERIALKEYSA;
  TSerialKeysA = SERIALKEYSA;
  PSerialKeysA = LPSERIALKEYSA;

  LPSERIALKEYSW = ^SERIALKEYSW;
  tagSERIALKEYSW = record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszActivePort: LPWSTR;
    lpszPort: LPWSTR;
    iBaudRate: UINT;
    iPortState: UINT;
    iActive: UINT;
  end;
  SERIALKEYSW = tagSERIALKEYSW;
  TSerialKeysW = SERIALKEYSW;
  PSerialKeysW = LPSERIALKEYSW;

{$IFDEF UNICODE}
  SERIALKEYS = SERIALKEYSW;
  LPSERIALKEYS = LPSERIALKEYSW;
  TSerialKeys = TSerialKeysW;
  PSerialKeys = PSerialKeysW;
{$ELSE}
  SERIALKEYS = SERIALKEYSA;
  LPSERIALKEYS = LPSERIALKEYSA;
  TSerialKeys = TSerialKeysA;
  PSerialKeys = PSerialKeysA;
{$ENDIF}

// flags for SERIALKEYS dwFlags field//

const
  SERKF_SERIALKEYSON = $00000001;
  SERKF_AVAILABLE    = $00000002;
  SERKF_INDICATOR    = $00000004;

type
  LPHIGHCONTRASTA = ^HIGHCONTRASTA;
  tagHIGHCONTRASTA = record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszDefaultScheme: LPSTR;
  end;
  HIGHCONTRASTA = tagHIGHCONTRASTA;
  THighContrastA = HIGHCONTRASTA;
  PHighContrastA = LPHIGHCONTRASTA;

  LPHIGHCONTRASTW = ^HIGHCONTRASTW;
  tagHIGHCONTRASTW = record
    cbSize: UINT;
    dwFlags: DWORD;
    lpszDefaultScheme: LPWSTR;
  end;
  HIGHCONTRASTW = tagHIGHCONTRASTW;
  THighContrastW = HIGHCONTRASTW;
  PHighContrastW = LPHIGHCONTRASTW;

{$IFDEF UNICODE}
  HIGHCONTRAST = HIGHCONTRASTW;
  LPHIGHCONTRAST = LPHIGHCONTRASTW;
  THighContrast = THighContrastW;
  PHighContrast = PHighContrastW;  
{$ELSE}
  HIGHCONTRAST = HIGHCONTRASTA;
  LPHIGHCONTRAST = LPHIGHCONTRASTA;
  THighContrast = THighContrastA;
  PHighContrast = PHighContrastA;
{$ENDIF}

// flags for HIGHCONTRAST dwFlags field//

const
  HCF_HIGHCONTRASTON  = $00000001;
  HCF_AVAILABLE       = $00000002;
  HCF_HOTKEYACTIVE    = $00000004;
  HCF_CONFIRMHOTKEY   = $00000008;
  HCF_HOTKEYSOUND     = $00000010;
  HCF_INDICATOR       = $00000020;
  HCF_HOTKEYAVAILABLE = $00000040;

// Flags for ChangeDisplaySettings//

  CDS_UPDATEREGISTRY  = $00000001;
  CDS_TEST            = $00000002;
  CDS_FULLSCREEN      = $00000004;
  CDS_GLOBAL          = $00000008;
  CDS_SET_PRIMARY     = $00000010;
  CDS_VIDEOPARAMETERS = $00000020;
  CDS_RESET           = $40000000;
  CDS_NORESET         = $10000000;

// #include <tvout.h>

// Return values for ChangeDisplaySettings

  DISP_CHANGE_SUCCESSFUL = 0;
  DISP_CHANGE_RESTART    = 1;
  DISP_CHANGE_FAILED     = DWORD(-1);
  DISP_CHANGE_BADMODE    = DWORD(-2);
  DISP_CHANGE_NOTUPDATED = DWORD(-3);
  DISP_CHANGE_BADFLAGS   = DWORD(-4);
  DISP_CHANGE_BADPARAM   = DWORD(-5);
  DISP_CHANGE_BADDUALVIEW = DWORD(-6);

function ChangeDisplaySettingsA(lpDevMode: LPDEVMODEA; dwFlags: DWORD): LONG; stdcall;
function ChangeDisplaySettingsW(lpDevMode: LPDEVMODEW; dwFlags: DWORD): LONG; stdcall;

{$IFDEF UNICODE}
function ChangeDisplaySettings(lpDevMode: LPDEVMODEW; dwFlags: DWORD): LONG; stdcall;
{$ELSE}
function ChangeDisplaySettings(lpDevMode: LPDEVMODEA; dwFlags: DWORD): LONG; stdcall;
{$ENDIF}

function ChangeDisplaySettingsExA(lpszDeviceName: LPCSTR; lpDevMode: LPDEVMODEA;
  hwnd: HWND; dwflags: DWORD; lParam: LPVOID): LONG; stdcall;
function ChangeDisplaySettingsExW(lpszDeviceName: LPCWSTR; lpDevMode: LPDEVMODEW;
  hwnd: HWND; dwflags: DWORD; lParam: LPVOID): LONG; stdcall;

{$IFDEF UNICODE}
function ChangeDisplaySettingsEx(lpszDeviceName: LPCWSTR; lpDevMode: LPDEVMODEW;
  hwnd: HWND; dwflags: DWORD; lParam: LPVOID): LONG; stdcall;
{$ELSE}
function ChangeDisplaySettingsEx(lpszDeviceName: LPCSTR; lpDevMode: LPDEVMODEA;
  hwnd: HWND; dwflags: DWORD; lParam: LPVOID): LONG; stdcall;
{$ENDIF}

const
  ENUM_CURRENT_SETTINGS  = DWORD(-1);
  ENUM_REGISTRY_SETTINGS = DWORD(-2);

function EnumDisplaySettingsA(lpszDeviceName: LPCSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEA): BOOL; stdcall;
function EnumDisplaySettingsW(lpszDeviceName: LPCWSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEW): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumDisplaySettings(lpszDeviceName: LPCWSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEW): BOOL; stdcall;
{$ELSE}
function EnumDisplaySettings(lpszDeviceName: LPCSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEA): BOOL; stdcall;
{$ENDIF}

function EnumDisplaySettingsExA(lpszDeviceName: LPCSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEA; dwFlags: DWORD): BOOL; stdcall;
function EnumDisplaySettingsExW(lpszDeviceName: LPCWSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEW; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumDisplaySettingsEx(lpszDeviceName: LPCWSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEW; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumDisplaySettingsEx(lpszDeviceName: LPCSTR; iModeNum: DWORD;
  var lpDevMode: DEVMODEA; dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

// Flags for EnumDisplaySettingsEx//

const
  EDS_RAWMODE = $00000002;

function EnumDisplayDevicesA(lpDevice: LPCSTR; iDevNum: DWORD;
  var lpDisplayDevice: DISPLAY_DEVICEA; dwFlags: DWORD): BOOL; stdcall;
function EnumDisplayDevicesW(lpDevice: LPCWSTR; iDevNum: DWORD;
  var lpDisplayDevice: DISPLAY_DEVICEW; dwFlags: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function EnumDisplayDevices(lpDevice: LPCWSTR; iDevNum: DWORD;
  var lpDisplayDevice: DISPLAY_DEVICEW; dwFlags: DWORD): BOOL; stdcall;
{$ELSE}
function EnumDisplayDevices(lpDevice: LPCSTR; iDevNum: DWORD;
  var lpDisplayDevice: DISPLAY_DEVICEA; dwFlags: DWORD): BOOL; stdcall;
{$ENDIF}

function SystemParametersInfoA(uiAction: UINT; uiParam: UINT;
  pvParam: PVOID; fWinIni: UINT): BOOL; stdcall;
function SystemParametersInfoW(uiAction: UINT; uiParam: UINT;
  pvParam: PVOID; fWinIni: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function SystemParametersInfo(uiAction: UINT; uiParam: UINT;
  pvParam: PVOID; fWinIni: UINT): BOOL; stdcall;
{$ELSE}
function SystemParametersInfo(uiAction: UINT; uiParam: UINT;
  pvParam: PVOID; fWinIni: UINT): BOOL; stdcall;
{$ENDIF}

//
// Accessibility support
//

type
  LPFILTERKEYS = ^FILTERKEYS;
  tagFILTERKEYS = record
    cbSize: UINT;
    dwFlags: DWORD;
    iWaitMSec: DWORD;   // Acceptance Delay
    iDelayMSec: DWORD;  // Delay Until Repeat
    iRepeatMSec: DWORD; // Repeat Rate
    iBounceMSec: DWORD; // Debounce Time
  end;
  FILTERKEYS = tagFILTERKEYS;
  TFilterKeys = FILTERKEYS;
  PFilterKeys = LPFILTERKEYS;

//
// FILTERKEYS dwFlags field
//

const
  FKF_FILTERKEYSON  = $00000001;
  FKF_AVAILABLE     = $00000002;
  FKF_HOTKEYACTIVE  = $00000004;
  FKF_CONFIRMHOTKEY = $00000008;
  FKF_HOTKEYSOUND   = $00000010;
  FKF_INDICATOR     = $00000020;
  FKF_CLICKON       = $00000040;

type
  LPSTICKYKEYS = ^STICKYKEYS;
  tagSTICKYKEYS = record
    cbSize: UINT;
    dwFlags: DWORD;
  end;
  STICKYKEYS = tagSTICKYKEYS;
  TStickyKeys = STICKYKEYS;
  PStickyKeys = LPSTICKYKEYS;

//
// STICKYKEYS dwFlags field
//

const
  SKF_STICKYKEYSON    = $00000001;
  SKF_AVAILABLE       = $00000002;
  SKF_HOTKEYACTIVE    = $00000004;
  SKF_CONFIRMHOTKEY   = $00000008;
  SKF_HOTKEYSOUND     = $00000010;
  SKF_INDICATOR       = $00000020;
  SKF_AUDIBLEFEEDBACK = $00000040;
  SKF_TRISTATE        = $00000080;
  SKF_TWOKEYSOFF      = $00000100;
  SKF_LALTLATCHED     = $10000000;
  SKF_LCTLLATCHED     = $04000000;
  SKF_LSHIFTLATCHED   = $01000000;
  SKF_RALTLATCHED     = $20000000;
  SKF_RCTLLATCHED     = $08000000;
  SKF_RSHIFTLATCHED   = $02000000;
  SKF_LWINLATCHED     = $40000000;
  SKF_RWINLATCHED     = $80000000;
  SKF_LALTLOCKED      = $00100000;
  SKF_LCTLLOCKED      = $00040000;
  SKF_LSHIFTLOCKED    = $00010000;
  SKF_RALTLOCKED      = $00200000;
  SKF_RCTLLOCKED      = $00080000;
  SKF_RSHIFTLOCKED    = $00020000;
  SKF_LWINLOCKED      = $00400000;
  SKF_RWINLOCKED      = $00800000;

type
  LPMOUSEKEYS = ^MOUSEKEYS;
  tagMOUSEKEYS = record
    cbSize: UINT;
    dwFlags: DWORD;
    iMaxSpeed: DWORD;
    iTimeToMaxSpeed: DWORD;
    iCtrlSpeed: DWORD;
    dwReserved1: DWORD;
    dwReserved2: DWORD;
  end;
  MOUSEKEYS = tagMOUSEKEYS;
  TMouseKeys = MOUSEKEYS;
  PMouseKeys = LPMOUSEKEYS;

//
// MOUSEKEYS dwFlags field
//

const
  MKF_MOUSEKEYSON     = $00000001;
  MKF_AVAILABLE       = $00000002;
  MKF_HOTKEYACTIVE    = $00000004;
  MKF_CONFIRMHOTKEY   = $00000008;
  MKF_HOTKEYSOUND     = $00000010;
  MKF_INDICATOR       = $00000020;
  MKF_MODIFIERS       = $00000040;
  MKF_REPLACENUMBERS  = $00000080;
  MKF_LEFTBUTTONSEL   = $10000000;
  MKF_RIGHTBUTTONSEL  = $20000000;
  MKF_LEFTBUTTONDOWN  = $01000000;
  MKF_RIGHTBUTTONDOWN = $02000000;
  MKF_MOUSEMODE       = $80000000;

type
  LPACCESSTIMEOUT = ^ACCESSTIMEOUT;
  tagACCESSTIMEOUT = record
    cbSize: UINT;
    dwFlags: DWORD;
    iTimeOutMSec: DWORD;
  end;
  ACCESSTIMEOUT = tagACCESSTIMEOUT;
  TAccessTimeout = ACCESSTIMEOUT;
  PAccessTimeout = LPACCESSTIMEOUT;

//
// ACCESSTIMEOUT dwFlags field
//

const
  ATF_TIMEOUTON     = $00000001;
  ATF_ONOFFFEEDBACK = $00000002;

// values for SOUNDSENTRY iFSGrafEffect field//

  SSGF_NONE    = 0;
  SSGF_DISPLAY = 3;

// values for SOUNDSENTRY iFSTextEffect field//

  SSTF_NONE    = 0;
  SSTF_CHARS   = 1;
  SSTF_BORDER  = 2;
  SSTF_DISPLAY = 3;

// values for SOUNDSENTRY iWindowsEffect field//

  SSWF_NONE    = 0;
  SSWF_TITLE   = 1;
  SSWF_WINDOW  = 2;
  SSWF_DISPLAY = 3;
  SSWF_CUSTOM  = 4;

type
  LPSOUNDSENTRYA = ^SOUNDSENTRYA;
  tagSOUNDSENTRYA = record
    cbSize: UINT;
    dwFlags: DWORD;
    iFSTextEffect: DWORD;
    iFSTextEffectMSec: DWORD;
    iFSTextEffectColorBits: DWORD;
    iFSGrafEffect: DWORD;
    iFSGrafEffectMSec: DWORD;
    iFSGrafEffectColor: DWORD;
    iWindowsEffect: DWORD;
    iWindowsEffectMSec: DWORD;
    lpszWindowsEffectDLL: LPSTR;
    iWindowsEffectOrdinal: DWORD;
  end;
  SOUNDSENTRYA = tagSOUNDSENTRYA;
  TSoundsEntryA = SOUNDSENTRYA;
  PSoundsEntryA = LPSOUNDSENTRYA;

  LPSOUNDSENTRYW = ^SOUNDSENTRYW;
  tagSOUNDSENTRYW = record
    cbSize: UINT;
    dwFlags: DWORD;
    iFSTextEffect: DWORD;
    iFSTextEffectMSec: DWORD;
    iFSTextEffectColorBits: DWORD;
    iFSGrafEffect: DWORD;
    iFSGrafEffectMSec: DWORD;
    iFSGrafEffectColor: DWORD;
    iWindowsEffect: DWORD;
    iWindowsEffectMSec: DWORD;
    lpszWindowsEffectDLL: LPWSTR;
    iWindowsEffectOrdinal: DWORD;
  end;
  SOUNDSENTRYW = tagSOUNDSENTRYW;
  TSoundsEntryW = SOUNDSENTRYW;
  PSoundsEntryW = LPSOUNDSENTRYW;

{$IFDEF UNICODE}
  SOUNDSENTRY = SOUNDSENTRYW;
  LPSOUNDSENTRY = LPSOUNDSENTRYW;
  TSoundsEntry = TSoundsEntryW;
  PSoundsEntry = PSoundsEntryW;
{$ELSE}
  SOUNDSENTRY = SOUNDSENTRYA;
  LPSOUNDSENTRY = LPSOUNDSENTRYA;
  TSoundsEntry = TSoundsEntryA;
  PSoundsEntry = PSoundsEntryA;
{$ENDIF}

//
// SOUNDSENTRY dwFlags field
//

const
  SSF_SOUNDSENTRYON = $00000001;
  SSF_AVAILABLE     = $00000002;
  SSF_INDICATOR     = $00000004;

type
  LPTOGGLEKEYS = ^TOGGLEKEYS;
  tagTOGGLEKEYS = record
    cbSize: UINT;
    dwFlags: DWORD;
  end;
  TOGGLEKEYS = tagTOGGLEKEYS;
  TToggleKeys = TOGGLEKEYS;
  PToggleKeys = LPTOGGLEKEYS;

//
// TOGGLEKEYS dwFlags field
//

const
  TKF_TOGGLEKEYSON  = $00000001;
  TKF_AVAILABLE     = $00000002;
  TKF_HOTKEYACTIVE  = $00000004;
  TKF_CONFIRMHOTKEY = $00000008;
  TKF_HOTKEYSOUND   = $00000010;
  TKF_INDICATOR     = $00000020;

//
// Set debug level
//

procedure SetDebugErrorLevel(dwLevel: DWORD); stdcall;

//
// SetLastErrorEx() types.
//

const
  SLE_ERROR      = $00000001;
  SLE_MINORERROR = $00000002;
  SLE_WARNING    = $00000003;

procedure SetLastErrorEx(dwErrCode, dwType: DWORD); stdcall;

function InternalGetWindowText(hWnd: HWND; lpString: LPWSTR; nMaxCount: Integer): Integer; stdcall;

function EndTask(hWnd: HWND; fShutDown, fForce: BOOL): BOOL; stdcall;

//
// Multimonitor API.
//

const
  MONITOR_DEFAULTTONULL    = $00000000;
  MONITOR_DEFAULTTOPRIMARY = $00000001;
  MONITOR_DEFAULTTONEAREST = $00000002;

function MonitorFromPoint(pt: POINT; dwFlags: DWORD): HMONITOR; stdcall;

function MonitorFromRect(const lprc: RECT; dwFlags: DWORD): HMONITOR; stdcall;

function MonitorFromWindow(hwnd: HWND; dwFlags: DWORD): HMONITOR; stdcall;

const
  MONITORINFOF_PRIMARY = $00000001;

  CCHDEVICENAME = 32;

type
  LPMONITORINFO = ^MONITORINFO;
  tagMONITORINFO = record
    cbSize: DWORD;
    rcMonitor: RECT;
    rcWork: RECT;
    dwFlags: DWORD;
  end;
  MONITORINFO = tagMONITORINFO;
  TMonitorInfo = MONITORINFO;
  PMonitorInfo = LPMONITORINFO;

  LPMONITORINFOEXA = ^MONITORINFOEXA;
  tagMONITORINFOEXA = record
    MonitorInfo: MONITORINFO;
    szDevice: array [0..CCHDEVICENAME - 1] of CHAR;
  end;
  MONITORINFOEXA = tagMONITORINFOEXA;
  TMonitorinfoexa = MONITORINFOEXA;
  PMonitorInfoExA = LPMONITORINFOEXA;

  LPMONITORINFOEXW = ^MONITORINFOEXW;
  tagMONITORINFOEXW = record
    MonitorInfo: MONITORINFO;
    szDevice: array [0..CCHDEVICENAME - 1] of WCHAR;
  end;
  MONITORINFOEXW = tagMONITORINFOEXW;
  TMonitorInfoExW = MONITORINFOEXW;
  PMonitorInfoExW = LPMONITORINFOEXW;

{$IFDEF UNICODE}
  MONITORINFOEX = MONITORINFOEXW;
  LPMONITORINFOEX = LPMONITORINFOEXW;
  TMonitorInfoEx = TMonitorInfoExW;
  PMonitorInfoEx = PMonitorInfoExW;
{$ELSE}
  MONITORINFOEX = MONITORINFOEXA;
  LPMONITORINFOEX = LPMONITORINFOEXA;
  TMonitorInfoEx = TMonitorInfoExA;
  PMonitorInfoEx = PMonitorInfoExA;
{$ENDIF}

function GetMonitorInfoA(hMonitor: HMONITOR; pmi: LPMONITORINFO): BOOL; stdcall;
function GetMonitorInfoW(hMonitor: HMONITOR; lpmi: LPMONITORINFO): BOOL; stdcall;

{$IFDEF UNICODE}
function GetMonitorInfo(hMonitor: HMONITOR; lpmi: LPMONITORINFO): BOOL; stdcall;
{$ELSE}
function GetMonitorInfo(hMonitor: HMONITOR; lpmi: LPMONITORINFO): BOOL; stdcall;
{$ENDIF}

type
  MONITORENUMPROC = function (hMonitor: HMONITOR; hdcMonitor: HDC;
    lprcMonitor: LPRECT; dwData: LPARAM): BOOL; stdcall;
  TMonitorEnumProc = MONITORENUMPROC;

function EnumDisplayMonitors(hdc: HDC; lprcClip: LPCRECT;
  lpfnEnum: MONITORENUMPROC; dwData: LPARAM): BOOL; stdcall;

//
// WinEvents - Active Accessibility hooks
//

procedure NotifyWinEvent(event: DWORD; hwnd: HWND; idObject: LONG; idChild: LONG); stdcall;

type
  WINEVENTPROC = procedure (hWinEventHook: HWINEVENTHOOK; event: DWORD; hwnd: HWND;
    idObject, idChild: LONG; idEventThread, dwmsEventTime: DWORD); stdcall;
  TWinEventProc = WINEVENTPROC;

function SetWinEventHook(eventMin: DWORD; eventMax: DWORD;
  hmodWinEventProc: HMODULE; pfnWinEventProc: WINEVENTPROC; idProcess: DWORD;
  idThread: DWORD; dwFlags: DWORD): HWINEVENTHOOK; stdcall;

function IsWinEventHookInstalled(event: DWORD): BOOL; stdcall;

//
// dwFlags for SetWinEventHook
//

const
  WINEVENT_OUTOFCONTEXT   = $0000; // Events are ASYNC
  WINEVENT_SKIPOWNTHREAD  = $0001; // Don't call back for events on installer's thread
  WINEVENT_SKIPOWNPROCESS = $0002; // Don't call back for events on installer's process
  WINEVENT_INCONTEXT      = $0004; // Events are SYNC, this causes your dll to be injected into every process

function UnhookWinEvent(hWinEventHook: HWINEVENTHOOK): BOOL; stdcall;

//
// idObject values for WinEventProc and NotifyWinEvent
//

//
// hwnd + idObject can be used with OLEACC.DLL's OleGetObjectFromWindow()
// to get an interface pointer to the container.  indexChild is the item
// within the container in question.  Setup a VARIANT with vt VT_I4 and
// lVal the indexChild and pass that in to all methods.  Then you
// are raring to go.
//


//
// Common object IDs (cookies, only for sending WM_GETOBJECT to get at the
// thing in question).  Positive IDs are reserved for apps (app specific),
// negative IDs are system things and are global, 0 means "just little old
// me".
//

const
  CHILDID_SELF      = 0;
  INDEXID_OBJECT    = 0;
  INDEXID_CONTAINER = 0;

//
// Reserved IDs for system objects
//

const
  OBJID_WINDOW            = DWORD($00000000);
  OBJID_SYSMENU           = DWORD($FFFFFFFF);
  OBJID_TITLEBAR          = DWORD($FFFFFFFE);
  OBJID_MENU              = DWORD($FFFFFFFD);
  OBJID_CLIENT            = DWORD($FFFFFFFC);
  OBJID_VSCROLL           = DWORD($FFFFFFFB);
  OBJID_HSCROLL           = DWORD($FFFFFFFA);
  OBJID_SIZEGRIP          = DWORD($FFFFFFF9);
  OBJID_CARET             = DWORD($FFFFFFF8);
  OBJID_CURSOR            = DWORD($FFFFFFF7);
  OBJID_ALERT             = DWORD($FFFFFFF6);
  OBJID_SOUND             = DWORD($FFFFFFF5);
  OBJID_QUERYCLASSNAMEIDX = DWORD($FFFFFFF4);
  OBJID_NATIVEOM          = DWORD($FFFFFFF0);

//
// EVENT DEFINITION
//

  EVENT_MIN = $00000001;
  EVENT_MAX = $7FFFFFFF;

//
//  EVENT_SYSTEM_SOUND
//  Sent when a sound is played.  Currently nothing is generating this, we
//  this event when a system sound (for menus, etc) is played.  Apps
//  generate this, if accessible, when a private sound is played.  For
//  example, if Mail plays a "New Mail" sound.
//
//  System Sounds:
//  (Generated by PlaySoundEvent in USER itself)
//      hwnd            is NULL
//      idObject        is OBJID_SOUND
//      idChild         is sound child ID if one
//  App Sounds:
//  (PlaySoundEvent won't generate notification; up to app)
//      hwnd + idObject gets interface pointer to Sound object
//      idChild identifies the sound in question
//  are going to be cleaning up the SOUNDSENTRY feature in the control panel
//  and will use this at that time.  Applications implementing WinEvents
//  are perfectly welcome to use it.  Clients of IAccessible* will simply
//  turn around and get back a non-visual object that describes the sound.
//

  EVENT_SYSTEM_SOUND = $0001;

//
// EVENT_SYSTEM_ALERT
// System Alerts:
// (Generated by MessageBox() calls for example)
//      hwnd            is hwndMessageBox
//      idObject        is OBJID_ALERT
// App Alerts:
// (Generated whenever)
//      hwnd+idObject gets interface pointer to Alert
//

  EVENT_SYSTEM_ALERT = $0002;

//
// EVENT_SYSTEM_FOREGROUND
// Sent when the foreground (active) window changes, even if it is changing
// to another window in the same thread as the previous one.
//      hwnd            is hwndNewForeground
//      idObject        is OBJID_WINDOW
//      idChild    is INDEXID_OBJECT
//

  EVENT_SYSTEM_FOREGROUND = $0003;

//
// Menu
//      hwnd            is window (top level window or popup menu window)
//      idObject        is ID of control (OBJID_MENU, OBJID_SYSMENU, OBJID_SELF for popup)
//      idChild         is CHILDID_SELF

// EVENT_SYSTEM_MENUSTART
// EVENT_SYSTEM_MENUEND
// For MENUSTART, hwnd+idObject+idChild refers to the control with the menu bar,
//  or the control bringing up the context menu.

// Sent when entering into and leaving from menu mode (system, app bar, and
// track popups).
//

  EVENT_SYSTEM_MENUSTART = $0004;
  EVENT_SYSTEM_MENUEND   = $0005;

//
// EVENT_SYSTEM_MENUPOPUPSTART
// EVENT_SYSTEM_MENUPOPUPEND
// Sent when a menu popup comes up and just before it is taken down.  Note
// that for a call to TrackPopupMenu(), a client will see EVENT_SYSTEM_MENUSTART
// followed almost immediately by EVENT_SYSTEM_MENUPOPUPSTART for the popup
// being shown.

// For MENUPOPUP, hwnd+idObject+idChild refers to the NEW popup coming up, not the
// parent item which is hierarchical.  You can get the parent menu/popup by
// asking for the accParent object.
//

  EVENT_SYSTEM_MENUPOPUPSTART = $0006;
  EVENT_SYSTEM_MENUPOPUPEND   = $0007;

//
// EVENT_SYSTEM_CAPTURESTART
// EVENT_SYSTEM_CAPTUREEND
// Sent when a window takes the capture and releases the capture.
//

  EVENT_SYSTEM_CAPTURESTART = $0008;
  EVENT_SYSTEM_CAPTUREEND   = $0009;

//
// Move Size
// EVENT_SYSTEM_MOVESIZESTART
// EVENT_SYSTEM_MOVESIZEEND
// Sent when a window enters and leaves move-size dragging mode.
//

  EVENT_SYSTEM_MOVESIZESTART = $000A;
  EVENT_SYSTEM_MOVESIZEEND   = $000B;

//
// Context Help
// EVENT_SYSTEM_CONTEXTHELPSTART
// EVENT_SYSTEM_CONTEXTHELPEND
// Sent when a window enters and leaves context sensitive help mode.
//

  EVENT_SYSTEM_CONTEXTHELPSTART = $000C;
  EVENT_SYSTEM_CONTEXTHELPEND   = $000D;

//
// Drag & Drop
// EVENT_SYSTEM_DRAGDROPSTART
// EVENT_SYSTEM_DRAGDROPEND
// Send the START notification just before going into drag&drop loop.  Send
// the END notification just after canceling out.
// Note that it is up to apps and OLE to generate this, since the system
// doesn't know.  Like EVENT_SYSTEM_SOUND, it will be a while before this
// is prevalent.
//

  EVENT_SYSTEM_DRAGDROPSTART = $000E;
  EVENT_SYSTEM_DRAGDROPEND   = $000F;

//
// Dialog
// Send the START notification right after the dialog is completely
//  initialized and visible.  Send the END right before the dialog
//  is hidden and goes away.
// EVENT_SYSTEM_DIALOGSTART
// EVENT_SYSTEM_DIALOGEND
//

  EVENT_SYSTEM_DIALOGSTART = $0010;
  EVENT_SYSTEM_DIALOGEND   = $0011;

//
// EVENT_SYSTEM_SCROLLING
// EVENT_SYSTEM_SCROLLINGSTART
// EVENT_SYSTEM_SCROLLINGEND
// Sent when beginning and ending the tracking of a scrollbar in a window,
// and also for scrollbar controls.
//

  EVENT_SYSTEM_SCROLLINGSTART = $0012;
  EVENT_SYSTEM_SCROLLINGEND   = $0013;

//
// Alt-Tab Window
// Send the START notification right after the switch window is initialized
// and visible.  Send the END right before it is hidden and goes away.
// EVENT_SYSTEM_SWITCHSTART
// EVENT_SYSTEM_SWITCHEND
//

  EVENT_SYSTEM_SWITCHSTART = $0014;
  EVENT_SYSTEM_SWITCHEND   = $0015;

//
// EVENT_SYSTEM_MINIMIZESTART
// EVENT_SYSTEM_MINIMIZEEND
// Sent when a window minimizes and just before it restores.
//

  EVENT_SYSTEM_MINIMIZESTART = $0016;
  EVENT_SYSTEM_MINIMIZEEND   = $0017;

  EVENT_CONSOLE_CARET             = $4001;
  EVENT_CONSOLE_UPDATE_REGION     = $4002;
  EVENT_CONSOLE_UPDATE_SIMPLE     = $4003;
  EVENT_CONSOLE_UPDATE_SCROLL     = $4004;
  EVENT_CONSOLE_LAYOUT            = $4005;
  EVENT_CONSOLE_START_APPLICATION = $4006;
  EVENT_CONSOLE_END_APPLICATION   = $4007;

//
// Flags for EVENT_CONSOLE_START/END_APPLICATION.
//

  CONSOLE_APPLICATION_16BIT       = $0001;

//
// Flags for EVENT_CONSOLE_CARET
//

  CONSOLE_CARET_SELECTION         = $0001;
  CONSOLE_CARET_VISIBLE           = $0002;

//
// Object events

// The system AND apps generate these.  The system generates these for
// real windows.  Apps generate these for objects within their window which
// act like a separate control, e.g. an item in a list view.

// When the system generate them, dwParam2 is always WMOBJID_SELF.  When
// apps generate them, apps put the has-meaning-to-the-app-only ID value
// in dwParam2.
// For all events, if you want detailed accessibility information, callers
// should
//      * Call AccessibleObjectFromWindow() with the hwnd, idObject parameters
//          of the event, and IID_IAccessible as the REFIID, to get back an
//          IAccessible* to talk to
//      * Initialize and fill in a VARIANT as VT_I4 with lVal the idChild
//          parameter of the event.
//      * If idChild isn't zero, call get_accChild() in the container to see
//          if the child is an object in its own right.  If so, you will get
//          back an IDispatch* object for the child.  You should release the
//          parent, and call QueryInterface() on the child object to get its
//          IAccessible*.  Then you talk directly to the child.  Otherwise,
//          if get_accChild() returns you nothing, you should continue to
//          use the child VARIANT.  You will ask the container for the properties
//          of the child identified by the VARIANT.  In other words, the
//          child in this case is accessible but not a full-blown object.
//          Like a button on a titlebar which is 'small' and has no children.
//

//
// For all EVENT_OBJECT events,
//      hwnd is the dude to Send the WM_GETOBJECT message to (unless NULL,
//          see above for system things)
//      idObject is the ID of the object that can resolve any queries a
//          client might have.  It's a way to deal with windowless controls,
//          controls that are just drawn on the screen in some larger parent
//          window (like SDM), or standard frame elements of a window.
//      idChild is the piece inside of the object that is affected.  This
//          allows clients to access things that are too small to have full
//          blown objects in their own right.  Like the thumb of a scrollbar.
//          The hwnd/idObject pair gets you to the container, the dude you
//          probably want to talk to most of the time anyway.  The idChild
//          can then be passed into the acc properties to get the name/value
//          of it as needed.

// Example #1:
//      System propagating a listbox selection change
//      EVENT_OBJECT_SELECTION
//          hwnd == listbox hwnd
//          idObject == OBJID_WINDOW
//          idChild == new selected item, or CHILDID_SELF if
//              nothing now selected within container.
//      Word '97 propagating a listbox selection change
//          hwnd == SDM window
//          idObject == SDM ID to get at listbox 'control'
//          idChild == new selected item, or CHILDID_SELF if
//              nothing

// Example #2:
//      System propagating a menu item selection on the menu bar
//      EVENT_OBJECT_SELECTION
//          hwnd == top level window
//          idObject == OBJID_MENU
//          idChild == ID of child menu bar item selected
// *
// Example #3:
//      System propagating a dropdown coming off of said menu bar item
//      EVENT_OBJECT_CREATE
//          hwnd == popup item
//          idObject == OBJID_WINDOW
//          idChild == CHILDID_SELF
//
// Example #4:
//
// For EVENT_OBJECT_REORDER, the object referred to by hwnd/idObject is the
// PARENT container in which the zorder is occurring.  This is because if
// one child is zordering, all of them are changing their relative zorder.
//

  EVENT_OBJECT_CREATE  = $8000; // hwnd + ID + idChild is created item
  EVENT_OBJECT_DESTROY = $8001; // hwnd + ID + idChild is destroyed item
  EVENT_OBJECT_SHOW    = $8002; // hwnd + ID + idChild is shown item
  EVENT_OBJECT_HIDE    = $8003; // hwnd + ID + idChild is hidden item
  EVENT_OBJECT_REORDER = $8004; // hwnd + ID + idChild is parent of zordering children

//
// NOTE:
// Minimize the number of notifications!
//
// When you are hiding a parent object, obviously all child objects are no
// longer visible on screen.  They still have the same "visible" status,
// but are not truly visible.  Hence do not send HIDE notifications for the
// children also.  One implies all.  The same goes for SHOW.
//

  EVENT_OBJECT_FOCUS           = $8005; // hwnd + ID + idChild is focused item
  EVENT_OBJECT_SELECTION       = $8006; // hwnd + ID + idChild is selected item (if only one), or idChild is OBJID_WINDOW if complex
  EVENT_OBJECT_SELECTIONADD    = $8007; // hwnd + ID + idChild is item added
  EVENT_OBJECT_SELECTIONREMOVE = $8008; // hwnd + ID + idChild is item removed
  EVENT_OBJECT_SELECTIONWITHIN = $8009; // hwnd + ID + idChild is parent of changed selected items

//
// NOTES:
// There is only one "focused" child item in a parent.  This is the place
// keystrokes are going at a given moment.  Hence only send a notification
// about where the NEW focus is going.  A NEW item getting the focus already
// implies that the OLD item is losing it.
//
// SELECTION however can be multiple.  Hence the different SELECTION
// notifications.  Here's when to use each:
//
// (1) Send a SELECTION notification in the simple single selection
//     case (like the focus) when the item with the selection is
//     merely moving to a different item within a container.  hwnd + ID
//     is the container control, idChildItem is the new child with the
//     selection.
//
// (2) Send a SELECTIONADD notification when a new item has simply been added
//     to the selection within a container.  This is appropriate when the
//     number of newly selected items is very small.  hwnd + ID is the
//     container control, idChildItem is the new child added to the selection.
//
// (3) Send a SELECTIONREMOVE notification when a new item has simply been
//     removed from the selection within a container.  This is appropriate
//     when the number of newly selected items is very small, just like
//     SELECTIONADD.  hwnd + ID is the container control, idChildItem is the
//     new child removed from the selection.
//
// (4) Send a SELECTIONWITHIN notification when the selected items within a
//     control have changed substantially.  Rather than propagate a large
//     number of changes to reflect removal for some items, addition of
//     others, just tell somebody who cares that a lot happened.  It will
//     be faster an easier for somebody watching to just turn around and
//     query the container control what the new bunch of selected items
//     are.
//

  EVENT_OBJECT_STATECHANGE = $800A; // hwnd + ID + idChild is item w/ state change

//
// Examples of when to send an EVENT_OBJECT_STATECHANGE include
//      * It is being enabled/disabled (USER does for windows)
//      * It is being pressed/released (USER does for buttons)
//      * It is being checked/unchecked (USER does for radio/check buttons)
//

  EVENT_OBJECT_LOCATIONCHANGE = $800B; // hwnd + ID + idChild is moved/sized item

//
// Note:
// A LOCATIONCHANGE is not sent for every child object when the parent
// changes shape/moves.  Send one notification for the topmost object
// that is changing.  For example, if the user resizes a top level window,
// USER will generate a LOCATIONCHANGE for it, but not for the menu bar,
// title bar, scrollbars, etc.  that are also changing shape/moving.
//
// In other words, it only generates LOCATIONCHANGE notifications for
// real windows that are moving/sizing.  It will not generate a LOCATIONCHANGE
// for every non-floating child window when the parent moves (the children are
// logically moving also on screen, but not relative to the parent).
//
// Now, if the app itself resizes child windows as a result of being
// sized, USER will generate LOCATIONCHANGEs for those dudes also because
// it doesn't know better.
//
// Note also that USER will generate LOCATIONCHANGE notifications for two
// non-window sys objects:
//      (1) System caret
//      (2) Cursor
//

  EVENT_OBJECT_NAMECHANGE        = $800C; // hwnd + ID + idChild is item w/ name change
  EVENT_OBJECT_DESCRIPTIONCHANGE = $800D; // hwnd + ID + idChild is item w/ desc change
  EVENT_OBJECT_VALUECHANGE       = $800E; // hwnd + ID + idChild is item w/ value change
  EVENT_OBJECT_PARENTCHANGE      = $800F; // hwnd + ID + idChild is item w/ new parent
  EVENT_OBJECT_HELPCHANGE        = $8010; // hwnd + ID + idChild is item w/ help change
  EVENT_OBJECT_DEFACTIONCHANGE   = $8011; // hwnd + ID + idChild is item w/ def action change
  EVENT_OBJECT_ACCELERATORCHANGE = $8012; // hwnd + ID + idChild is item w/ keybd accel change

//
// Child IDs
//

//
// System Sounds (idChild of system SOUND notification)
//

  SOUND_SYSTEM_STARTUP     = 1;
  SOUND_SYSTEM_SHUTDOWN    = 2;
  SOUND_SYSTEM_BEEP        = 3;
  SOUND_SYSTEM_ERROR       = 4;
  SOUND_SYSTEM_QUESTION    = 5;
  SOUND_SYSTEM_WARNING     = 6;
  SOUND_SYSTEM_INFORMATION = 7;
  SOUND_SYSTEM_MAXIMIZE    = 8;
  SOUND_SYSTEM_MINIMIZE    = 9;
  SOUND_SYSTEM_RESTOREUP   = 10;
  SOUND_SYSTEM_RESTOREDOWN = 11;
  SOUND_SYSTEM_APPSTART    = 12;
  SOUND_SYSTEM_FAULT       = 13;
  SOUND_SYSTEM_APPEND      = 14;
  SOUND_SYSTEM_MENUCOMMAND = 15;
  SOUND_SYSTEM_MENUPOPUP   = 16;
  CSOUND_SYSTEM            = 16;

//
// System Alerts (indexChild of system ALERT notification)
//

  ALERT_SYSTEM_INFORMATIONAL = 1; // MB_INFORMATION
  ALERT_SYSTEM_WARNING       = 2; // MB_WARNING
  ALERT_SYSTEM_ERROR         = 3; // MB_ERROR
  ALERT_SYSTEM_QUERY         = 4; // MB_QUESTION
  ALERT_SYSTEM_CRITICAL      = 5; // HardSysErrBox
  CALERT_SYSTEM              = 6;

type
  LPGUITHREADINFO = ^GUITHREADINFO;
  tagGUITHREADINFO = record
    cbSize: DWORD;
    flags: DWORD;
    hwndActive: HWND;
    hwndFocus: HWND;
    hwndCapture: HWND;
    hwndMenuOwner: HWND;
    hwndMoveSize: HWND;
    hwndCaret: HWND;
    rcCaret: RECT;
  end;
  GUITHREADINFO = tagGUITHREADINFO;
  TGuiThreadInfo = GUITHREADINFO;
  PGuiThreadInfo = LPGUITHREADINFO;

const
  GUI_CARETBLINKING  = $00000001;
  GUI_INMOVESIZE     = $00000002;
  GUI_INMENUMODE     = $00000004;
  GUI_SYSTEMMENUMODE = $00000008;
  GUI_POPUPMENUMODE  = $00000010;
  GUI_16BITTASK      = $00000020;

function GetGUIThreadInfo(idThread: DWORD; var pgui: GUITHREADINFO): BOOL; stdcall;

function GetWindowModuleFileNameA(hwnd: HWND; pszFileName: LPSTR; cchFileNameMax: UINT): UINT; stdcall;
function GetWindowModuleFileNameW(hwnd: HWND; pszFileName: LPWSTR; cchFileNameMax: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function GetWindowModuleFileName(hwnd: HWND; pszFileName: LPWSTR; cchFileNameMax: UINT): UINT; stdcall;
{$ELSE}
function GetWindowModuleFileName(hwnd: HWND; pszFileName: LPSTR; cchFileNameMax: UINT): UINT; stdcall;
{$ENDIF}

const
  STATE_SYSTEM_UNAVAILABLE     = $00000001; // Disabled
  STATE_SYSTEM_SELECTED        = $00000002;
  STATE_SYSTEM_FOCUSED         = $00000004;
  STATE_SYSTEM_PRESSED         = $00000008;
  STATE_SYSTEM_CHECKED         = $00000010;
  STATE_SYSTEM_MIXED           = $00000020; // 3-state checkbox or toolbar button
  STATE_SYSTEM_INDETERMINATE   = STATE_SYSTEM_MIXED;
  STATE_SYSTEM_READONLY        = $00000040;
  STATE_SYSTEM_HOTTRACKED      = $00000080;
  STATE_SYSTEM_DEFAULT         = $00000100;
  STATE_SYSTEM_EXPANDED        = $00000200;
  STATE_SYSTEM_COLLAPSED       = $00000400;
  STATE_SYSTEM_BUSY            = $00000800;
  STATE_SYSTEM_FLOATING        = $00001000; // Children "owned" not "contained" by parent
  STATE_SYSTEM_MARQUEED        = $00002000;
  STATE_SYSTEM_ANIMATED        = $00004000;
  STATE_SYSTEM_INVISIBLE       = $00008000;
  STATE_SYSTEM_OFFSCREEN       = $00010000;
  STATE_SYSTEM_SIZEABLE        = $00020000;
  STATE_SYSTEM_MOVEABLE        = $00040000;
  STATE_SYSTEM_SELFVOICING     = $00080000;
  STATE_SYSTEM_FOCUSABLE       = $00100000;
  STATE_SYSTEM_SELECTABLE      = $00200000;
  STATE_SYSTEM_LINKED          = $00400000;
  STATE_SYSTEM_TRAVERSED       = $00800000;
  STATE_SYSTEM_MULTISELECTABLE = $01000000; // Supports multiple selection
  STATE_SYSTEM_EXTSELECTABLE   = $02000000; // Supports extended selection
  STATE_SYSTEM_ALERT_LOW       = $04000000; // This information is of low priority
  STATE_SYSTEM_ALERT_MEDIUM    = $08000000; // This information is of medium priority
  STATE_SYSTEM_ALERT_HIGH      = $10000000; // This information is of high priority
  STATE_SYSTEM_PROTECTED       = $20000000; // access to this is restricted
  STATE_SYSTEM_VALID           = $3FFFFFFF;

  CCHILDREN_TITLEBAR  = 5;
  CCHILDREN_SCROLLBAR = 5;

//
// Information about the global cursor.
//

type
  LPCURSORINFO = ^CURSORINFO;
  tagCURSORINFO = record
    cbSize: DWORD;
    flags: DWORD;
    hCursor: HCURSOR;
    ptScreenPos: POINT;
  end;
  CURSORINFO = tagCURSORINFO;
  TCursorInfo = CURSORINFO;
  PCursorInfo = LPCURSORINFO;

const
  CURSOR_SHOWING = $00000001;

function GetCursorInfo(var pci: CURSORINFO): BOOL; stdcall;

//
// Window information snapshot
//

type
  LPWINDOWINFO = ^WINDOWINFO;
  tagWINDOWINFO = record
    cbSize: DWORD;
    rcWindow: RECT;
    rcClient: RECT;
    dwStyle: DWORD;
    dwExStyle: DWORD;
    dwWindowStatus: DWORD;
    cxWindowBorders: UINT;
    cyWindowBorders: UINT;
    atomWindowType: ATOM;
    wCreatorVersion: WORD;
  end;
  WINDOWINFO = tagWINDOWINFO;
  TWindowInfo = WINDOWINFO;
  PWindowInfo = LPWINDOWINFO;

const
  WS_ACTIVECAPTION = $0001;

function GetWindowInfo(hwnd: HWND; var pwi: WINDOWINFO): BOOL; stdcall;

//
// Titlebar information.
//

type
  LPTITLEBARINFO = ^TITLEBARINFO;
  tagTITLEBARINFO = record
    cbSize: DWORD;
    rcTitleBar: RECT;
    rgstate: array [0..CCHILDREN_TITLEBAR] of DWORD;
  end;
  TITLEBARINFO = tagTITLEBARINFO;
  TTitleBarInfo = TITLEBARINFO;
  PTitleBarInfo = LPTITLEBARINFO;

function GetTitleBarInfo(hwnd: HWND; var pti: TITLEBARINFO): BOOL; stdcall;

//
// Menubar information
//

type
  LPMENUBARINFO = ^MENUBARINFO;
  tagMENUBARINFO = record
    cbSize: DWORD;
    rcBar: RECT;          // rect of bar, popup, item
    hMenu: HMENU;         // real menu handle of bar, popup
    hwndMenu: HWND;       // hwnd of item submenu if one
    Flags: DWORD;
    // BOOL  fBarFocused:1;  // bar, popup has the focus
    // BOOL  fFocused:1;     // item has the focus
  end;
  MENUBARINFO = tagMENUBARINFO;
  TMenuBarInfo = MENUBARINFO;
  PMenuBarInfo = LPMENUBARINFO;

function GetMenuBarInfo(hwnd: HWND; idObject: LONG; idItem: LONG;
  var pmbi: MENUBARINFO): BOOL; stdcall;

//
// Scrollbar information
//

type
  LPSCROLLBARINFO = ^SCROLLBARINFO;
  tagSCROLLBARINFO = record
    cbSize: DWORD;
    rcScrollBar: RECT;
    dxyLineButton: Integer;
    xyThumbTop: Integer;
    xyThumbBottom: Integer;
    reserved: Integer;
    rgstate: array [0..CCHILDREN_SCROLLBAR] of DWORD;
  end;
  SCROLLBARINFO = tagSCROLLBARINFO;
  TScrollBarInfo = SCROLLBARINFO;
  PScrollBarInfo = LPSCROLLBARINFO;

function GetScrollBarInfo(hwnd: HWND; idObject: LONG; var psbi: SCROLLBARINFO): BOOL; stdcall;

//
// Combobox information
//

type
  LPCOMBOBOXINFO = ^COMBOBOXINFO;
  tagCOMBOBOXINFO = record
    cbSize: DWORD;
    rcItem: RECT;
    rcButton: RECT;
    stateButton: DWORD;
    hwndCombo: HWND;
    hwndItem: HWND;
    hwndList: HWND;
  end;
  COMBOBOXINFO = tagCOMBOBOXINFO;
  TComboBoxInfo = COMBOBOXINFO;
  PComboBoxInfo = LPCOMBOBOXINFO;

function GetComboBoxInfo(hwndCombo: HWND; var pcbi: COMBOBOXINFO): BOOL; stdcall;

//
// The "real" ancestor window
//

const
  GA_PARENT    = 1;
  GA_ROOT      = 2;
  GA_ROOTOWNER = 3;

function GetAncestor(hwnd: HWND; gaFlags: UINT): HWND; stdcall;

//
// This gets the REAL child window at the point.  If it is in the dead
// space of a group box, it will try a sibling behind it.  But static
// fields will get returned.  In other words, it is kind of a cross between
// ChildWindowFromPointEx and WindowFromPoint.
//

function RealChildWindowFromPoint(hwndParent: HWND; ptParentClientCoords: POINT): HWND; stdcall;

//
// This gets the name of the window TYPE, not class.  This allows us to
// recognize ThunderButton32 et al.
//

function RealGetWindowClassA(hwnd: HWND; pszType: LPSTR; cchType: UINT): UINT; stdcall;

//
// This gets the name of the window TYPE, not class.  This allows us to
// recognize ThunderButton32 et al.
//

function RealGetWindowClassW(hwnd: HWND; pszType: LPWSTR; cchType: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function RealGetWindowClass(hwnd: HWND; pszType: LPWSTR; cchType: UINT): UINT; stdcall;
{$ELSE}
function RealGetWindowClass(hwnd: HWND; pszType: LPSTR; cchType: UINT): UINT; stdcall;
{$ENDIF}

//
// Alt-Tab Switch window information.
//

type
  LPALTTABINFO = ^ALTTABINFO;
  tagALTTABINFO = record
    cbSize: DWORD;
    cItems: Integer;
    cColumns: Integer;
    cRows: Integer;
    iColFocus: Integer;
    iRowFocus: Integer;
    cxItem: Integer;
    cyItem: Integer;
    ptStart: POINT;
  end;
  ALTTABINFO = tagALTTABINFO;
  TAltTabInfo = ALTTABINFO;
  PAltTabInfo = LPALTTABINFO;

function GetAltTabInfoA(hwnd: HWND; iItem: Integer; var pati: ALTTABINFO;
  pszItemText: LPSTR; cchItemText: UINT): BOOL; stdcall;
function GetAltTabInfoW(hwnd: HWND; iItem: Integer; var pati: ALTTABINFO;
  pszItemText: LPWSTR; cchItemText: UINT): BOOL; stdcall;

{$IFDEF UNICODE}
function GetAltTabInfo(hwnd: HWND; iItem: Integer; var pati: ALTTABINFO;
  pszItemText: LPWSTR; cchItemText: UINT): BOOL; stdcall;
{$ELSE}
function GetAltTabInfo(hwnd: HWND; iItem: Integer; var pati: ALTTABINFO;
  pszItemText: LPSTR; cchItemText: UINT): BOOL; stdcall;
{$ENDIF}

//
// Listbox information.
// Returns the number of items per row.
//

function GetListBoxInfo(hwnd: HWND): DWORD; stdcall;

function LockWorkStation: BOOL; stdcall;

function UserHandleGrantAccess(hUserHandle, hJob: HANDLE; bGrant: BOOL): BOOL; stdcall;

//
// Raw Input Messages.
//

type
  HRAWINPUT = HANDLE;

//
// WM_INPUT wParam
//

//
// Use this macro to get the input code from wParam.
//

function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): DWORD;

//
// The input is in the regular message flow,
// the app is required to call DefWindowProc
// so that the system can perform clean ups.
//

const
  RIM_INPUT       = 0;

//
// The input is sink only. The app is expected
// to behave nicely.
//

  RIM_INPUTSINK   = 1;

//
// Raw Input data header
//

type
  tagRAWINPUTHEADER = record
    dwType: DWORD;
    dwSize: DWORD;
    hDevice: HANDLE;
    wParam: WPARAM;
  end;
  RAWINPUTHEADER = tagRAWINPUTHEADER;
  PRAWINPUTHEADER = ^RAWINPUTHEADER;
  LPRAWINPUTHEADER = ^RAWINPUTHEADER;
  TRawInputHeader = RAWINPUTHEADER;

//
// Type of the raw input
//

const
  RIM_TYPEMOUSE      = 0;
  RIM_TYPEKEYBOARD   = 1;
  RIM_TYPEHID        = 2;

//
// Raw format of the mouse input
//

type
  tagRAWMOUSE = record
    //
    // Indicator flags.
    //
    usFlags: USHORT;

    //
    // The transition state of the mouse buttons.
    //

    union: record
    case Integer of
      0: (
        ulButtons: ULONG);
      1: (
        usButtonFlags: USHORT;
        usButtonData: USHORT);
    end;

    //
    // The raw state of the mouse buttons.
    //
    ulRawButtons: ULONG;

    //
    // The signed relative or absolute motion in the X direction.
    //
    lLastX: LONG;

    //
    // The signed relative or absolute motion in the Y direction.
    //
    lLastY: LONG;

    //
    // Device-specific additional information for the event.
    //
    ulExtraInformation: ULONG;
  end;
  RAWMOUSE = tagRAWMOUSE;
  PRAWMOUSE = ^RAWMOUSE;
  LPRAWMOUSE = ^RAWMOUSE;
  TRawMouse = RAWMOUSE;

//
// Define the mouse button state indicators.
//

const
  RI_MOUSE_LEFT_BUTTON_DOWN   = $0001; // Left Button changed to down.
  RI_MOUSE_LEFT_BUTTON_UP     = $0002; // Left Button changed to up.
  RI_MOUSE_RIGHT_BUTTON_DOWN  = $0004; // Right Button changed to down.
  RI_MOUSE_RIGHT_BUTTON_UP    = $0008; // Right Button changed to up.
  RI_MOUSE_MIDDLE_BUTTON_DOWN = $0010; // Middle Button changed to down.
  RI_MOUSE_MIDDLE_BUTTON_UP   = $0020; // Middle Button changed to up.

  RI_MOUSE_BUTTON_1_DOWN = RI_MOUSE_LEFT_BUTTON_DOWN;
  RI_MOUSE_BUTTON_1_UP   = RI_MOUSE_LEFT_BUTTON_UP;
  RI_MOUSE_BUTTON_2_DOWN = RI_MOUSE_RIGHT_BUTTON_DOWN;
  RI_MOUSE_BUTTON_2_UP   = RI_MOUSE_RIGHT_BUTTON_UP;
  RI_MOUSE_BUTTON_3_DOWN = RI_MOUSE_MIDDLE_BUTTON_DOWN;
  RI_MOUSE_BUTTON_3_UP   = RI_MOUSE_MIDDLE_BUTTON_UP;

  RI_MOUSE_BUTTON_4_DOWN = $0040;
  RI_MOUSE_BUTTON_4_UP   = $0080;
  RI_MOUSE_BUTTON_5_DOWN = $0100;
  RI_MOUSE_BUTTON_5_UP   = $0200;

//
// If usButtonFlags has RI_MOUSE_WHEEL, the wheel delta is stored in usButtonData.
// Take it as a signed value.
//

  RI_MOUSE_WHEEL = $0400;

//
// Define the mouse indicator flags.
//

  MOUSE_MOVE_RELATIVE      = 0;
  MOUSE_MOVE_ABSOLUTE      = 1;
  MOUSE_VIRTUAL_DESKTOP    = $02; // the coordinates are mapped to the virtual desktop
  MOUSE_ATTRIBUTES_CHANGED = $04; // requery for mouse attributes

//
// Raw format of the keyboard input
//

type
  tagRAWKEYBOARD = record
    //
    // The "make" scan code (key depression).
    //
    MakeCode: USHORT;

    //
    // The flags field indicates a "break" (key release) and other
    // miscellaneous scan code information defined in ntddkbd.h.
    //
    Flags: USHORT;

    Reserved: USHORT;

    //
    // Windows message compatible information
    //
    VKey: USHORT;
    Message: UINT;

    //
    // Device-specific additional information for the event.
    //
    ExtraInformation: ULONG;
  end;
  RAWKEYBOARD = tagRAWKEYBOARD;
  PRAWKEYBOARD = ^RAWKEYBOARD;
  LPRAWKEYBOARD = ^RAWKEYBOARD;
  TRawKeyBoard = RAWKEYBOARD;

//
// Define the keyboard overrun MakeCode.
//

const
  KEYBOARD_OVERRUN_MAKE_CODE = $FF;

//
// Define the keyboard input data Flags.
//

  RI_KEY_MAKE            = 0;
  RI_KEY_BREAK           = 1;
  RI_KEY_E0              = 2;
  RI_KEY_E1              = 4;
  RI_KEY_TERMSRV_SET_LED = 8;
  RI_KEY_TERMSRV_SHADOW  = $10;

//
// Raw format of the input from Human Input Devices
//

type
  tagRAWHID = record
    dwSizeHid: DWORD;    // byte size of each report
    dwCount: DWORD;      // number of input packed
    bRawData: array [0..0] of BYTE;
  end;
  RAWHID = tagRAWHID;
  PRAWHID = ^RAWHID;
  LPRAWHID = ^RAWHID;
  TRawHid = RAWHID;

//
// RAWINPUT data structure.
//

  tagRAWINPUT = record
    header: RAWINPUTHEADER;
    case Integer of
      0: (mouse: RAWMOUSE);
      1: (keyboard: RAWKEYBOARD);
      2: (hid: RAWHID);
  end;
  RAWINPUT = tagRAWINPUT;
  PRAWINPUT = ^RAWINPUT;
  LPRAWINPUT = ^RAWINPUT;
  TRawInput = RAWINPUT;

function RAWINPUT_ALIGN(x: Pointer): Pointer;

function NEXTRAWINPUTBLOCK(ptr: PRawInput): PRawInput;

//
// Flags for GetRawInputData
//

const
  RID_INPUT  = $10000003;
  RID_HEADER = $10000005;

function GetRawInputData(hRawInput: HRAWINPUT; uiCommand: UINT; pData: LPVOID;
  var pcbSize: UINT; cbSizeHeader: UINT): UINT; stdcall;

//
// Raw Input Device Information
//

const
  RIDI_PREPARSEDDATA = $20000005;
  RIDI_DEVICENAME    = $20000007; // the return valus is the character length, not the byte size
  RIDI_DEVICEINFO    = $2000000b;

type
  PRID_DEVICE_INFO_MOUSE = ^RID_DEVICE_INFO_MOUSE;
  tagRID_DEVICE_INFO_MOUSE = record
    dwId: DWORD;
    dwNumberOfButtons: DWORD;
    dwSampleRate: DWORD;
  end;
  RID_DEVICE_INFO_MOUSE = tagRID_DEVICE_INFO_MOUSE;
  TRidDeviceInfoMouse = RID_DEVICE_INFO_MOUSE;
  PRidDeviceInfoMouse = PRID_DEVICE_INFO_MOUSE;

  PRID_DEVICE_INFO_KEYBOARD = ^RID_DEVICE_INFO_KEYBOARD;
  tagRID_DEVICE_INFO_KEYBOARD = record
    dwType: DWORD;
    dwSubType: DWORD;
    dwKeyboardMode: DWORD;
    dwNumberOfFunctionKeys: DWORD;
    dwNumberOfIndicators: DWORD;
    dwNumberOfKeysTotal: DWORD;
  end;
  RID_DEVICE_INFO_KEYBOARD = tagRID_DEVICE_INFO_KEYBOARD;
  TRidDeviceInfoKeyboard = RID_DEVICE_INFO_KEYBOARD;
  PRidDeviceInfoKeyboard = PRID_DEVICE_INFO_KEYBOARD;

  PRID_DEVICE_INFO_HID = ^RID_DEVICE_INFO_HID;
  tagRID_DEVICE_INFO_HID = record
    dwVendorId: DWORD;
    dwProductId: DWORD;
    dwVersionNumber: DWORD;
    //
    // Top level collection UsagePage and Usage
    //
    usUsagePage: USHORT;
    usUsage: USHORT;
  end;
  RID_DEVICE_INFO_HID = tagRID_DEVICE_INFO_HID;
  TRidDeviceInfoHid = RID_DEVICE_INFO_HID;
  PRidDeviceInfoHid = PRID_DEVICE_INFO_HID;

  tagRID_DEVICE_INFO = record
    cbSize: DWORD;
    dwType: DWORD;
    case Integer of
    0: (mouse: RID_DEVICE_INFO_MOUSE);
    1: (keyboard: RID_DEVICE_INFO_KEYBOARD);
    2: (hid: RID_DEVICE_INFO_HID);
  end;
  RID_DEVICE_INFO = tagRID_DEVICE_INFO;
  PRID_DEVICE_INFO = ^RID_DEVICE_INFO;
  LPRID_DEVICE_INFO = ^RID_DEVICE_INFO;
  TRidDeviceInfo = RID_DEVICE_INFO;
  PRidDeviceInfo = PRID_DEVICE_INFO;

function GetRawInputDeviceInfoA(hDevice: HANDLE; uiCommand: UINT; pData: LPVOID;
  var pcbSize: UINT): UINT; stdcall;
function GetRawInputDeviceInfoW(hDevice: HANDLE; uiCommand: UINT; pData: LPVOID;
  var pcbSize: UINT): UINT; stdcall;

{$IFDEF UNICODE}
function GetRawInputDeviceInfo(hDevice: HANDLE; uiCommand: UINT; pData: LPVOID;
  var pcbSize: UINT): UINT; stdcall;
{$ELSE}
function GetRawInputDeviceInfo(hDevice: HANDLE; uiCommand: UINT; pData: LPVOID;
  var pcbSize: UINT): UINT; stdcall;
{$ENDIF}

//
// Raw Input Bulk Read: GetRawInputBuffer
//

function GetRawInputBuffer(pData: PRAWINPUT; var pcbSize: UINT; cbSizeHeader: UINT): UINT; stdcall;

//
// Raw Input request APIs
//

type
  LPRAWINPUTDEVICE = ^RAWINPUTDEVICE;
  PRAWINPUTDEVICE = ^RAWINPUTDEVICE;
  tagRAWINPUTDEVICE = record
    usUsagePage: USHORT; // Toplevel collection UsagePage
    usUsage: USHORT;     // Toplevel collection Usage
    dwFlags: DWORD;
    hwndTarget: HWND;    // Target hwnd. NULL = follows keyboard focus
  end;
  RAWINPUTDEVICE = tagRAWINPUTDEVICE;
  TRawInputDevice = RAWINPUTDEVICE;

const
  RIDEV_REMOVE       = $00000001;
  RIDEV_EXCLUDE      = $00000010;
  RIDEV_PAGEONLY     = $00000020;
  RIDEV_NOLEGACY     = $00000030;
  RIDEV_INPUTSINK    = $00000100;
  RIDEV_CAPTUREMOUSE = $00000200; // effective when mouse nolegacy is specified, otherwise it would be an error
  RIDEV_NOHOTKEYS    = $00000200; // effective for keyboard.
  RIDEV_APPKEYS      = $00000400;  // effective for keyboard.
  RIDEV_EXMODEMASK   = $000000F0;

function RIDEV_EXMODE(mode: DWORD): DWORD;

function RegisterRawInputDevices(pRawInputDevices: PRAWINPUTDEVICE;
  uiNumDevices: UINT; cbSize: UINT): BOOL; stdcall;

function GetRegisteredRawInputDevices(pRawInputDevices: PRAWINPUTDEVICE;
  var puiNumDevices: UINT; cbSize: UINT): UINT; stdcall;

type
  PRAWINPUTDEVICELIST = ^RAWINPUTDEVICELIST;
  tagRAWINPUTDEVICELIST = record
    hDevice: HANDLE;
    dwType: DWORD;
  end;
  RAWINPUTDEVICELIST = tagRAWINPUTDEVICELIST;
  TRawInputDeviceList = RAWINPUTDEVICELIST;

function GetRawInputDeviceList(pRawInputDeviceList: PRAWINPUTDEVICELIST; var puiNumDevices: UINT;
  cbSize: UINT): UINT; stdcall;

function DefRawInputProc(paRawInput: PRAWINPUT; nInput: Integer; cbSizeHeader: UINT): LRESULT; stdcall;

implementation

const
  user32 = 'user32.dll';

function IS_INTRESOURCE(wInteger: WORD): BOOL;
begin
  Result := (ULONG_PTR(wInteger) shr 16) = 0;
end;

function GET_WHEEL_DELTA_WPARAM(wParam: WPARAM): SHORT;
begin
  Result := SHORT(HIWORD(wParam));
end;

function GET_KEYSTATE_WPARAM(wParam: WPARAM): Integer;
begin
  Result := LOWORD(wParam);
end;

function GET_NCHITTEST_WPARAM(wParam: WPARAM): Shortint;
begin
  Result := LOWORD(wParam);
end;

function GET_XBUTTON_WPARAM(wParam: WPARAM): Integer;
begin
  Result := HIWORD(wParam);
end;

function GET_APPCOMMAND_LPARAM(lParam: LPARAM): Shortint;
begin
  Result := Shortint(HIWORD(lParam) and not FAPPCOMMAND_MASK);
end;

function GET_DEVICE_LPARAM(lParam: LPARAM): WORD;
begin
  Result := WORD(HIWORD(lParam) and FAPPCOMMAND_MASK);
end;

function GET_MOUSEORKEY_LPARAM(lParam: LPARAM): WORD;
begin
  Result := GET_DEVICE_LPARAM(lParam);
end;

function GET_FLAGS_LPARAM(lParam: LPARAM): Integer;
begin
  Result := LOWORD(lParam);
end;

function GET_KEYSTATE_LPARAM(lParam: LPARAM): Integer;
begin
  Result := GET_FLAGS_LPARAM(lParam);
end;

function MAKEWPARAM(wLow, wHigh: WORD): WPARAM;
begin
  Result := WPARAM(DWORD(MAKELONG(wLow, wHigh)));
end;

function MAKELPARAM(wLow, wHigh: WORD): LPARAM;
begin
  Result := LPARAM(DWORD(MAKELONG(wLow, wHigh)));
end;

function MAKELRESULT(wLow, wHigh: WORD): LRESULT;
begin
  Result := LRESULT(DWORD(MAKELONG(wLow, wHigh)));
end;

function ExitWindows(dwReserved: DWORD; uREserved: UINT): BOOL;
begin
  Result := ExitWindowsEx(EWX_LOGOFF, $FFFFFFFF);
end;

function PostAppMessageA(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
begin
  Result := PostThreadMessageA(idThread, wMsg, wParam, lParam);
end;

function PostAppMessageW(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
begin
  Result := PostThreadMessageW(idThread, wMsg, wParam, lParam);
end;

{$IFDEF UNICODE}
function PostAppMessage(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
begin
  Result := PostThreadMessageW(idThread, wMsg, wParam, lParam);
end;
{$ELSE}
function PostAppMessage(idThread: DWORD; wMsg: UINT; wParam: WPARAM; lParam: LPARAM): BOOL;
begin
  Result := PostThreadMessageA(idThread, wMsg, wParam, lParam);
end;
{$ENDIF}

function CreateWindowA(lpClassName: LPCSTR; lpWindowName: LPCSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;
begin
  Result := CreateWindowExA(0, lpClassName, lpWindowName, dwStyle, x, y,
    nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end;

function CreateWindowW(lpClassName: LPCWSTR; lpWindowName: LPCWSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;
begin
  Result := CreateWindowExW(0, lpClassName, lpWindowName, dwStyle, x, y,
    nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end;

{$IFDEF UNICODE}
function CreateWindow(lpClassName: LPCWSTR; lpWindowName: LPCWSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;
begin
  Result := CreateWindowExW(0, lpClassName, lpWindowName, dwStyle, x, y,
    nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end;
{$ELSE}
function CreateWindow(lpClassName: LPCSTR; lpWindowName: LPCSTR; dwStyle: DWORD;
  x, y, nWidth, nHeight: Integer; hWndParent: HWND; hMenu: HMENU;
  hInstance: HINSTANCE; lpParam: LPVOID): HWND;
begin
  Result := CreateWindowExA(0, lpClassName, lpWindowName, dwStyle, x, y,
    nWidth, nHeight, hWndParent, hMenu, hInstance, lpParam);
end;
{$ENDIF}

function CreateDialogA(hInstance: HINSTANCE; lpName: LPCSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogParamA(hInstance, lpName, hWndParent, lpDialogFunc, 0);
end;

function CreateDialogW(hInstance: HINSTANCE; lpName: LPCWSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogParamW(hInstance, lpName, hWndParent, lpDialogFunc, 0);
end;

{$IFDEF UNICODE}
function CreateDialog(hInstance: HINSTANCE; lpName: LPCWSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogParamW(hInstance, lpName, hWndParent, lpDialogFunc, 0);
end;
{$ELSE}
function CreateDialog(hInstance: HINSTANCE; lpName: LPCSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogParamA(hInstance, lpName, hWndParent, lpDialogFunc, 0);
end;
{$ENDIF}

function CreateDialogIndirectA(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogIndirectParamA(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;

function CreateDialogIndirectW(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogIndirectParamW(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;

{$IFDEF UNICODE}
function CreateDialogIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogIndirectParamW(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;
{$ELSE}
function CreateDialogIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE;
  hWndParent: HWND; lpDialogFunc: DLGPROC): HWND;
begin
  Result := CreateDialogIndirectParamA(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;
{$ENDIF}

function DialogBoxA(hInstance: HINSTANCE; lpTemplate: LPCSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxParamA(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;

function DialogBoxW(hInstance: HINSTANCE; lpTemplate: LPCWSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxParamW(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;

{$IFDEF UNICODE}
function DialogBox(hInstance: HINSTANCE; lpTemplate: LPCWSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxParamW(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;
{$ELSE}
function DialogBox(hInstance: HINSTANCE; lpTemplate: LPCSTR; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxParamA(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;
{$ENDIF}

function DialogBoxIndirectA(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxIndirectParamA(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;

function DialogBoxIndirectW(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxIndirectParamW(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;

{$IFDEF UNICODE}
function DialogBoxIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxIndirectParamW(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;
{$ELSE}
function DialogBoxIndirect(hInstance: HINSTANCE; const lpTemplate: DLGTEMPLATE; hWndParent: HWND; lpDialogFunc: DLGPROC): INT_PTR;
begin
  Result := DialogBoxIndirectParamA(hInstance, lpTemplate, hWndParent, lpDialogFunc, 0);
end;
{$ENDIF}

function AnsiToOem(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL;
begin
  Result := CharToOemA(lpszSrc, lpszDst);
end;

function OemToAnsi(lpszSrc: LPCSTR; lpszDst: LPSTR): BOOL;
begin
  Result := OemToCharA(lpszSrc, lpszDst);
end;

function AnsiToOemBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL;
begin
  Result := CharToOemBuffA(lpszSrc, lpszDst, cchDstLength);
end;

function OemToAnsiBuff(lpszSrc: LPCSTR; lpszDst: LPSTR; cchDstLength: DWORD): BOOL;
begin
  Result := OemToCharBuffA(lpszSrc, lpszDst, cchDstLength);
end;

function AnsiUpper(lpsz: LPSTR): LPSTR;
begin
  Result := CharUpperA(lpsz);
end;

function AnsiUpperBuff(lpsz: LPSTR; cchLength: DWORD): DWORD;
begin
  Result := CharUpperBuffA(lpsz, cchLength);
end;

function AnsiLower(lpsz: LPSTR): LPSTR;
begin
  Result := CharLowerA(lpsz);
end;

function AnsiLowerBuff(lpsz: LPSTR; cchLength: DWORD): DWORD;
begin
  Result := CharLowerBuffA(lpsz, cchLength);
end;

function AnsiNext(lpsz: LPCSTR): LPSTR;
begin
  Result := CharNextA(lpsz);
end;

function AnsiPrev(lpszStart: LPCSTR; lpszCurrent: LPCSTR): LPSTR;
begin
  Result := CharPrevA(lpszStart, lpszCurrent);
end;

function GetWindowLongPtrA(hWnd: HWND; nIndex: Integer): LONG_PTR;
begin
  Result := GetWindowLongA(hWnd, nIndex);
end;

function GetWindowLongPtrW(hWnd: HWND; nIndex: Integer): LONG_PTR;
begin
  Result := GetWindowLongW(hWnd, nIndex);
end;

{$IFDEF UNICODE}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR;
begin
  Result := GetWindowLongW(hWnd, nIndex);
end;
{$ELSE}
function GetWindowLongPtr(hWnd: HWND; nIndex: Integer): LONG_PTR;
begin
  Result := GetWindowLongA(hWnd, nIndex);
end;
{$ENDIF}

function SetWindowLongPtrA(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;
begin
  Result := SetWindowLongA(hWnd, nIndex, dwNewLong);
end;

function SetWindowLongPtrW(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;
begin
  Result := SetWindowLongW(hWnd, nIndex, dwNewLong);
end;

{$IFDEF UNICODE}
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;
begin
  Result := SetWindowLongW(hWnd, nIndex, dwNewLong);
end;
{$ELSE}
function SetWindowLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: LONG_PTR): LONG_PTR;
begin
  Result := SetWindowLongA(hWnd, nIndex, dwNewLong);
end;
{$ENDIF}

function GetClassLongPtrA(hWnd: HWND; nIndex: Integer): ULONG_PTR;
begin
  Result := GetClassLongA(hWnd, nIndex);
end;

function GetClassLongPtrW(hWnd: HWND; nIndex: Integer): ULONG_PTR;
begin
  Result := GetClassLongW(hWnd, nIndex);
end;

{$IFDEF UNICODE}
function GetClassLongPtr(hWnd: HWND; nIndex: Integer): ULONG_PTR;
begin
  Result := GetClassLongW(hWnd, nIndex);
end;
{$ELSE}
function GetClassLongPtr(hWnd: HWND; nIndex: Integer): ULONG_PTR;
begin
  Result := GetClassLongA(hWnd, nIndex);
end;
{$ENDIF}

function SetClassLongPtrA(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;
begin
  Result := SetClassLongA(hWnd, nIndex, dwNewLong);
end;

function SetClassLongPtrW(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;
begin
  Result := SetClassLongW(hWnd, nIndex, dwNewLong);
end;

{$IFDEF UNICODE}
function SetClassLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;
begin
  Result := SetClassLongW(hWnd, nIndex, dwNewLong);
end;
{$ELSE}
function SetClassLongPtr(hWnd: HWND; nIndex: Integer; dwNewLong: ULONG_PTR): ULONG_PTR;
begin
  Result := SetClassLongA(hWnd, nIndex, dwNewLong);
end;
{$ENDIF}

function EnumTaskWindows(hTask: HANDLE; lpfn: WNDENUMPROC; lParam: LPARAM): BOOL;
begin
  Result := EnumThreadWindows(ULONG(hTask), lpfn, lParam);
end;

function GetNextWindow(hWnd: HWND; wCmd: UINT): HWND;
begin
  Result := GetWindow(hWnd, wCmd);
end;

function GetWindowTask(hWnd: HWND): HANDLE;
begin
  Result := HANDLE(DWORD_PTR(GetWindowThreadProcessId(hWnd, nil)));
end;

function DefHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM; phhk: LPHHOOK): LRESULT;
begin
  Result := CallNextHookEx(HHOOK(phhk^), nCode, wParam, lParam);
end;

function CopyCursor(pcur: HCURSOR): HCURSOR;
begin
 Result := HCURSOR(CopyIcon(HICON(pcur)));
end;


{$IFDEF DYNAMIC_LINK}
var
  _IsHungAppWindow: Pointer;

function IsHungAppWindow;
begin
  GetProcedureAddress(_IsHungAppWindow, user32, 'IsHungAppWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsHungAppWindow]
  end;
end;
{$ELSE}
function IsHungAppWindow; external user32 name 'IsHungAppWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DisableProcessWindowsGhosting: Pointer;

procedure DisableProcessWindowsGhosting;
begin
  GetProcedureAddress(_DisableProcessWindowsGhosting, user32, 'DisableProcessWindowsGhosting');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DisableProcessWindowsGhosting]
  end;
end;
{$ELSE}
procedure DisableProcessWindowsGhosting; external user32 name 'DisableProcessWindowsGhosting';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wvsprintfA: Pointer;

function wvsprintfA;
begin
  GetProcedureAddress(_wvsprintfA, user32, 'wvsprintfA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wvsprintfA]
  end;
end;
{$ELSE}
function wvsprintfA; external user32 name 'wvsprintfA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wvsprintfW: Pointer;

function wvsprintfW;
begin
  GetProcedureAddress(_wvsprintfW, user32, 'wvsprintfW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wvsprintfW]
  end;
end;
{$ELSE}
function wvsprintfW; external user32 name 'wvsprintfW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _wvsprintf: Pointer;

function wvsprintf;
begin
  GetProcedureAddress(_wvsprintf, user32, 'wvsprintfW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wvsprintf]
  end;
end;
{$ELSE}
function wvsprintf; external user32 name 'wvsprintfW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _wvsprintf: Pointer;

function wvsprintf;
begin
  GetProcedureAddress(_wvsprintf, user32, 'wvsprintfA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wvsprintf]
  end;
end;
{$ELSE}
function wvsprintf; external user32 name 'wvsprintfA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _wsprintfA: Pointer;

function wsprintfA;
begin
  GetProcedureAddress(_wsprintfA, user32, 'wsprintfA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wsprintfA]
  end;
end;
{$ELSE}
function wsprintfA; external user32 name 'wsprintfA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _wsprintfW: Pointer;

function wsprintfW;
begin
  GetProcedureAddress(_wsprintfW, user32, 'wsprintfW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wsprintfW]
  end;
end;
{$ELSE}
function wsprintfW; external user32 name 'wsprintfW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _wsprintf: Pointer;

function wsprintf;
begin
  GetProcedureAddress(_wsprintf, user32, 'wsprintfW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wsprintf]
  end;
end;
{$ELSE}
function wsprintf; external user32 name 'wsprintfW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _wsprintf: Pointer;

function wsprintf;
begin
  GetProcedureAddress(_wsprintf, user32, 'wsprintfA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_wsprintf]
  end;
end;
{$ELSE}
function wsprintf; external user32 name 'wsprintfA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LoadKeyboardLayoutA: Pointer;

function LoadKeyboardLayoutA;
begin
  GetProcedureAddress(_LoadKeyboardLayoutA, user32, 'LoadKeyboardLayoutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadKeyboardLayoutA]
  end;
end;
{$ELSE}
function LoadKeyboardLayoutA; external user32 name 'LoadKeyboardLayoutA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadKeyboardLayoutW: Pointer;

function LoadKeyboardLayoutW;
begin
  GetProcedureAddress(_LoadKeyboardLayoutW, user32, 'LoadKeyboardLayoutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadKeyboardLayoutW]
  end;
end;
{$ELSE}
function LoadKeyboardLayoutW; external user32 name 'LoadKeyboardLayoutW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadKeyboardLayout: Pointer;

function LoadKeyboardLayout;
begin
  GetProcedureAddress(_LoadKeyboardLayout, user32, 'LoadKeyboardLayoutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadKeyboardLayout]
  end;
end;
{$ELSE}
function LoadKeyboardLayout; external user32 name 'LoadKeyboardLayoutW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadKeyboardLayout: Pointer;

function LoadKeyboardLayout;
begin
  GetProcedureAddress(_LoadKeyboardLayout, user32, 'LoadKeyboardLayoutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadKeyboardLayout]
  end;
end;
{$ELSE}
function LoadKeyboardLayout; external user32 name 'LoadKeyboardLayoutA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ActivateKeyboardLayout: Pointer;

function ActivateKeyboardLayout;
begin
  GetProcedureAddress(_ActivateKeyboardLayout, user32, 'ActivateKeyboardLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ActivateKeyboardLayout]
  end;
end;
{$ELSE}
function ActivateKeyboardLayout; external user32 name 'ActivateKeyboardLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ToUnicodeEx: Pointer;

function ToUnicodeEx;
begin
  GetProcedureAddress(_ToUnicodeEx, user32, 'ToUnicodeEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ToUnicodeEx]
  end;
end;
{$ELSE}
function ToUnicodeEx; external user32 name 'ToUnicodeEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnloadKeyboardLayout: Pointer;

function UnloadKeyboardLayout;
begin
  GetProcedureAddress(_UnloadKeyboardLayout, user32, 'UnloadKeyboardLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnloadKeyboardLayout]
  end;
end;
{$ELSE}
function UnloadKeyboardLayout; external user32 name 'UnloadKeyboardLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardLayoutNameA: Pointer;

function GetKeyboardLayoutNameA;
begin
  GetProcedureAddress(_GetKeyboardLayoutNameA, user32, 'GetKeyboardLayoutNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardLayoutNameA]
  end;
end;
{$ELSE}
function GetKeyboardLayoutNameA; external user32 name 'GetKeyboardLayoutNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardLayoutNameW: Pointer;

function GetKeyboardLayoutNameW;
begin
  GetProcedureAddress(_GetKeyboardLayoutNameW, user32, 'GetKeyboardLayoutNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardLayoutNameW]
  end;
end;
{$ELSE}
function GetKeyboardLayoutNameW; external user32 name 'GetKeyboardLayoutNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardLayoutName: Pointer;

function GetKeyboardLayoutName;
begin
  GetProcedureAddress(_GetKeyboardLayoutName, user32, 'GetKeyboardLayoutNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardLayoutName]
  end;
end;
{$ELSE}
function GetKeyboardLayoutName; external user32 name 'GetKeyboardLayoutNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardLayoutName: Pointer;

function GetKeyboardLayoutName;
begin
  GetProcedureAddress(_GetKeyboardLayoutName, user32, 'GetKeyboardLayoutNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardLayoutName]
  end;
end;
{$ELSE}
function GetKeyboardLayoutName; external user32 name 'GetKeyboardLayoutNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardLayoutList: Pointer;

function GetKeyboardLayoutList;
begin
  GetProcedureAddress(_GetKeyboardLayoutList, user32, 'GetKeyboardLayoutList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardLayoutList]
  end;
end;
{$ELSE}
function GetKeyboardLayoutList; external user32 name 'GetKeyboardLayoutList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardLayout: Pointer;

function GetKeyboardLayout;
begin
  GetProcedureAddress(_GetKeyboardLayout, user32, 'GetKeyboardLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardLayout]
  end;
end;
{$ELSE}
function GetKeyboardLayout; external user32 name 'GetKeyboardLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMouseMovePointsEx: Pointer;

function GetMouseMovePointsEx;
begin
  GetProcedureAddress(_GetMouseMovePointsEx, user32, 'GetMouseMovePointsEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMouseMovePointsEx]
  end;
end;
{$ELSE}
function GetMouseMovePointsEx; external user32 name 'GetMouseMovePointsEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDesktopA: Pointer;

function CreateDesktopA;
begin
  GetProcedureAddress(_CreateDesktopA, user32, 'CreateDesktopA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDesktopA]
  end;
end;
{$ELSE}
function CreateDesktopA; external user32 name 'CreateDesktopA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDesktopW: Pointer;

function CreateDesktopW;
begin
  GetProcedureAddress(_CreateDesktopW, user32, 'CreateDesktopW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDesktopW]
  end;
end;
{$ELSE}
function CreateDesktopW; external user32 name 'CreateDesktopW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDesktop: Pointer;

function CreateDesktop;
begin
  GetProcedureAddress(_CreateDesktop, user32, 'CreateDesktopW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDesktop]
  end;
end;
{$ELSE}
function CreateDesktop; external user32 name 'CreateDesktopW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDesktop: Pointer;

function CreateDesktop;
begin
  GetProcedureAddress(_CreateDesktop, user32, 'CreateDesktopA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDesktop]
  end;
end;
{$ELSE}
function CreateDesktop; external user32 name 'CreateDesktopA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenDesktopA: Pointer;

function OpenDesktopA;
begin
  GetProcedureAddress(_OpenDesktopA, user32, 'OpenDesktopA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenDesktopA]
  end;
end;
{$ELSE}
function OpenDesktopA; external user32 name 'OpenDesktopA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenDesktopW: Pointer;

function OpenDesktopW;
begin
  GetProcedureAddress(_OpenDesktopW, user32, 'OpenDesktopW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenDesktopW]
  end;
end;
{$ELSE}
function OpenDesktopW; external user32 name 'OpenDesktopW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenDesktop: Pointer;

function OpenDesktop;
begin
  GetProcedureAddress(_OpenDesktop, user32, 'OpenDesktopW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenDesktop]
  end;
end;
{$ELSE}
function OpenDesktop; external user32 name 'OpenDesktopW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenDesktop: Pointer;

function OpenDesktop;
begin
  GetProcedureAddress(_OpenDesktop, user32, 'OpenDesktopA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenDesktop]
  end;
end;
{$ELSE}
function OpenDesktop; external user32 name 'OpenDesktopA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenInputDesktop: Pointer;

function OpenInputDesktop;
begin
  GetProcedureAddress(_OpenInputDesktop, user32, 'OpenInputDesktop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenInputDesktop]
  end;
end;
{$ELSE}
function OpenInputDesktop; external user32 name 'OpenInputDesktop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDesktopsA: Pointer;

function EnumDesktopsA;
begin
  GetProcedureAddress(_EnumDesktopsA, user32, 'EnumDesktopsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDesktopsA]
  end;
end;
{$ELSE}
function EnumDesktopsA; external user32 name 'EnumDesktopsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDesktopsW: Pointer;

function EnumDesktopsW;
begin
  GetProcedureAddress(_EnumDesktopsW, user32, 'EnumDesktopsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDesktopsW]
  end;
end;
{$ELSE}
function EnumDesktopsW; external user32 name 'EnumDesktopsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDesktops: Pointer;

function EnumDesktops;
begin
  GetProcedureAddress(_EnumDesktops, user32, 'EnumDesktopsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDesktops]
  end;
end;
{$ELSE}
function EnumDesktops; external user32 name 'EnumDesktopsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDesktops: Pointer;

function EnumDesktops;
begin
  GetProcedureAddress(_EnumDesktops, user32, 'EnumDesktopsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDesktops]
  end;
end;
{$ELSE}
function EnumDesktops; external user32 name 'EnumDesktopsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDesktopWindows: Pointer;

function EnumDesktopWindows;
begin
  GetProcedureAddress(_EnumDesktopWindows, user32, 'EnumDesktopWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDesktopWindows]
  end;
end;
{$ELSE}
function EnumDesktopWindows; external user32 name 'EnumDesktopWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SwitchDesktop: Pointer;

function SwitchDesktop;
begin
  GetProcedureAddress(_SwitchDesktop, user32, 'SwitchDesktop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SwitchDesktop]
  end;
end;
{$ELSE}
function SwitchDesktop; external user32 name 'SwitchDesktop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetThreadDesktop: Pointer;

function SetThreadDesktop;
begin
  GetProcedureAddress(_SetThreadDesktop, user32, 'SetThreadDesktop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetThreadDesktop]
  end;
end;
{$ELSE}
function SetThreadDesktop; external user32 name 'SetThreadDesktop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseDesktop: Pointer;

function CloseDesktop;
begin
  GetProcedureAddress(_CloseDesktop, user32, 'CloseDesktop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseDesktop]
  end;
end;
{$ELSE}
function CloseDesktop; external user32 name 'CloseDesktop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetThreadDesktop: Pointer;

function GetThreadDesktop;
begin
  GetProcedureAddress(_GetThreadDesktop, user32, 'GetThreadDesktop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetThreadDesktop]
  end;
end;
{$ELSE}
function GetThreadDesktop; external user32 name 'GetThreadDesktop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowStationA: Pointer;

function CreateWindowStationA;
begin
  GetProcedureAddress(_CreateWindowStationA, user32, 'CreateWindowStationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowStationA]
  end;
end;
{$ELSE}
function CreateWindowStationA; external user32 name 'CreateWindowStationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowStationW: Pointer;

function CreateWindowStationW;
begin
  GetProcedureAddress(_CreateWindowStationW, user32, 'CreateWindowStationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowStationW]
  end;
end;
{$ELSE}
function CreateWindowStationW; external user32 name 'CreateWindowStationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowStation: Pointer;

function CreateWindowStation;
begin
  GetProcedureAddress(_CreateWindowStation, user32, 'CreateWindowStationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowStation]
  end;
end;
{$ELSE}
function CreateWindowStation; external user32 name 'CreateWindowStationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowStation: Pointer;

function CreateWindowStation;
begin
  GetProcedureAddress(_CreateWindowStation, user32, 'CreateWindowStationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowStation]
  end;
end;
{$ELSE}
function CreateWindowStation; external user32 name 'CreateWindowStationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWindowStationA: Pointer;

function OpenWindowStationA;
begin
  GetProcedureAddress(_OpenWindowStationA, user32, 'OpenWindowStationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWindowStationA]
  end;
end;
{$ELSE}
function OpenWindowStationA; external user32 name 'OpenWindowStationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWindowStationW: Pointer;

function OpenWindowStationW;
begin
  GetProcedureAddress(_OpenWindowStationW, user32, 'OpenWindowStationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWindowStationW]
  end;
end;
{$ELSE}
function OpenWindowStationW; external user32 name 'OpenWindowStationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWindowStation: Pointer;

function OpenWindowStation;
begin
  GetProcedureAddress(_OpenWindowStation, user32, 'OpenWindowStationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWindowStation]
  end;
end;
{$ELSE}
function OpenWindowStation; external user32 name 'OpenWindowStationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OpenWindowStation: Pointer;

function OpenWindowStation;
begin
  GetProcedureAddress(_OpenWindowStation, user32, 'OpenWindowStationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenWindowStation]
  end;
end;
{$ELSE}
function OpenWindowStation; external user32 name 'OpenWindowStationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumWindowStationsA: Pointer;

function EnumWindowStationsA;
begin
  GetProcedureAddress(_EnumWindowStationsA, user32, 'EnumWindowStationsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumWindowStationsA]
  end;
end;
{$ELSE}
function EnumWindowStationsA; external user32 name 'EnumWindowStationsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumWindowStationsW: Pointer;

function EnumWindowStationsW;
begin
  GetProcedureAddress(_EnumWindowStationsW, user32, 'EnumWindowStationsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumWindowStationsW]
  end;
end;
{$ELSE}
function EnumWindowStationsW; external user32 name 'EnumWindowStationsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumWindowStations: Pointer;

function EnumWindowStations;
begin
  GetProcedureAddress(_EnumWindowStations, user32, 'EnumWindowStationsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumWindowStations]
  end;
end;
{$ELSE}
function EnumWindowStations; external user32 name 'EnumWindowStationsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumWindowStations: Pointer;

function EnumWindowStations;
begin
  GetProcedureAddress(_EnumWindowStations, user32, 'EnumWindowStationsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumWindowStations]
  end;
end;
{$ELSE}
function EnumWindowStations; external user32 name 'EnumWindowStationsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CloseWindowStation: Pointer;

function CloseWindowStation;
begin
  GetProcedureAddress(_CloseWindowStation, user32, 'CloseWindowStation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseWindowStation]
  end;
end;
{$ELSE}
function CloseWindowStation; external user32 name 'CloseWindowStation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetProcessWindowStation: Pointer;

function SetProcessWindowStation;
begin
  GetProcedureAddress(_SetProcessWindowStation, user32, 'SetProcessWindowStation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessWindowStation]
  end;
end;
{$ELSE}
function SetProcessWindowStation; external user32 name 'SetProcessWindowStation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessWindowStation: Pointer;

function GetProcessWindowStation;
begin
  GetProcedureAddress(_GetProcessWindowStation, user32, 'GetProcessWindowStation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessWindowStation]
  end;
end;
{$ELSE}
function GetProcessWindowStation; external user32 name 'GetProcessWindowStation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetUserObjectSecurity: Pointer;

function SetUserObjectSecurity;
begin
  GetProcedureAddress(_SetUserObjectSecurity, user32, 'SetUserObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUserObjectSecurity]
  end;
end;
{$ELSE}
function SetUserObjectSecurity; external user32 name 'SetUserObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserObjectSecurity: Pointer;

function GetUserObjectSecurity;
begin
  GetProcedureAddress(_GetUserObjectSecurity, user32, 'GetUserObjectSecurity');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserObjectSecurity]
  end;
end;
{$ELSE}
function GetUserObjectSecurity; external user32 name 'GetUserObjectSecurity';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserObjectInformationA: Pointer;

function GetUserObjectInformationA;
begin
  GetProcedureAddress(_GetUserObjectInformationA, user32, 'GetUserObjectInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserObjectInformationA]
  end;
end;
{$ELSE}
function GetUserObjectInformationA; external user32 name 'GetUserObjectInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserObjectInformationW: Pointer;

function GetUserObjectInformationW;
begin
  GetProcedureAddress(_GetUserObjectInformationW, user32, 'GetUserObjectInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserObjectInformationW]
  end;
end;
{$ELSE}
function GetUserObjectInformationW; external user32 name 'GetUserObjectInformationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserObjectInformation: Pointer;

function GetUserObjectInformation;
begin
  GetProcedureAddress(_GetUserObjectInformation, user32, 'GetUserObjectInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserObjectInformation]
  end;
end;
{$ELSE}
function GetUserObjectInformation; external user32 name 'GetUserObjectInformationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetUserObjectInformation: Pointer;

function GetUserObjectInformation;
begin
  GetProcedureAddress(_GetUserObjectInformation, user32, 'GetUserObjectInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUserObjectInformation]
  end;
end;
{$ELSE}
function GetUserObjectInformation; external user32 name 'GetUserObjectInformationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetUserObjectInformationA: Pointer;

function SetUserObjectInformationA;
begin
  GetProcedureAddress(_SetUserObjectInformationA, user32, 'SetUserObjectInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUserObjectInformationA]
  end;
end;
{$ELSE}
function SetUserObjectInformationA; external user32 name 'SetUserObjectInformationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetUserObjectInformationW: Pointer;

function SetUserObjectInformationW;
begin
  GetProcedureAddress(_SetUserObjectInformationW, user32, 'SetUserObjectInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUserObjectInformationW]
  end;
end;
{$ELSE}
function SetUserObjectInformationW; external user32 name 'SetUserObjectInformationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetUserObjectInformation: Pointer;

function SetUserObjectInformation;
begin
  GetProcedureAddress(_SetUserObjectInformation, user32, 'SetUserObjectInformationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUserObjectInformation]
  end;
end;
{$ELSE}
function SetUserObjectInformation; external user32 name 'SetUserObjectInformationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetUserObjectInformation: Pointer;

function SetUserObjectInformation;
begin
  GetProcedureAddress(_SetUserObjectInformation, user32, 'SetUserObjectInformationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetUserObjectInformation]
  end;
end;
{$ELSE}
function SetUserObjectInformation; external user32 name 'SetUserObjectInformationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterWindowMessageA: Pointer;

function RegisterWindowMessageA;
begin
  GetProcedureAddress(_RegisterWindowMessageA, user32, 'RegisterWindowMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterWindowMessageA]
  end;
end;
{$ELSE}
function RegisterWindowMessageA; external user32 name 'RegisterWindowMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterWindowMessageW: Pointer;

function RegisterWindowMessageW;
begin
  GetProcedureAddress(_RegisterWindowMessageW, user32, 'RegisterWindowMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterWindowMessageW]
  end;
end;
{$ELSE}
function RegisterWindowMessageW; external user32 name 'RegisterWindowMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterWindowMessage: Pointer;

function RegisterWindowMessage;
begin
  GetProcedureAddress(_RegisterWindowMessage, user32, 'RegisterWindowMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterWindowMessage]
  end;
end;
{$ELSE}
function RegisterWindowMessage; external user32 name 'RegisterWindowMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterWindowMessage: Pointer;

function RegisterWindowMessage;
begin
  GetProcedureAddress(_RegisterWindowMessage, user32, 'RegisterWindowMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterWindowMessage]
  end;
end;
{$ELSE}
function RegisterWindowMessage; external user32 name 'RegisterWindowMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  __TrackMouseEvent: Pointer;

function TrackMouseEvent;
begin
  GetProcedureAddress(__TrackMouseEvent, user32, 'TrackMouseEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [__TrackMouseEvent]
  end;
end;
{$ELSE}
function TrackMouseEvent; external user32 name 'TrackMouseEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawEdge: Pointer;

function DrawEdge;
begin
  GetProcedureAddress(_DrawEdge, user32, 'DrawEdge');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawEdge]
  end;
end;
{$ELSE}
function DrawEdge; external user32 name 'DrawEdge';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawFrameControl: Pointer;

function DrawFrameControl;
begin
  GetProcedureAddress(_DrawFrameControl, user32, 'DrawFrameControl');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawFrameControl]
  end;
end;
{$ELSE}
function DrawFrameControl; external user32 name 'DrawFrameControl';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawCaption: Pointer;

function DrawCaption;
begin
  GetProcedureAddress(_DrawCaption, user32, 'DrawCaption');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawCaption]
  end;
end;
{$ELSE}
function DrawCaption; external user32 name 'DrawCaption';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawAnimatedRects: Pointer;

function DrawAnimatedRects;
begin
  GetProcedureAddress(_DrawAnimatedRects, user32, 'DrawAnimatedRects');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawAnimatedRects]
  end;
end;
{$ELSE}
function DrawAnimatedRects; external user32 name 'DrawAnimatedRects';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessageA: Pointer;

function GetMessageA;
begin
  GetProcedureAddress(_GetMessageA, user32, 'GetMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessageA]
  end;
end;
{$ELSE}
function GetMessageA; external user32 name 'GetMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessageW: Pointer;

function GetMessageW;
begin
  GetProcedureAddress(_GetMessageW, user32, 'GetMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessageW]
  end;
end;
{$ELSE}
function GetMessageW; external user32 name 'GetMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessage: Pointer;

function GetMessage;
begin
  GetProcedureAddress(_GetMessage, user32, 'GetMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessage]
  end;
end;
{$ELSE}
function GetMessage; external user32 name 'GetMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessage: Pointer;

function GetMessage;
begin
  GetProcedureAddress(_GetMessage, user32, 'GetMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessage]
  end;
end;
{$ELSE}
function GetMessage; external user32 name 'GetMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateMessage: Pointer;

function TranslateMessage;
begin
  GetProcedureAddress(_TranslateMessage, user32, 'TranslateMessage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateMessage]
  end;
end;
{$ELSE}
function TranslateMessage; external user32 name 'TranslateMessage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DispatchMessageA: Pointer;

function DispatchMessageA;
begin
  GetProcedureAddress(_DispatchMessageA, user32, 'DispatchMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DispatchMessageA]
  end;
end;
{$ELSE}
function DispatchMessageA; external user32 name 'DispatchMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DispatchMessageW: Pointer;

function DispatchMessageW;
begin
  GetProcedureAddress(_DispatchMessageW, user32, 'DispatchMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DispatchMessageW]
  end;
end;
{$ELSE}
function DispatchMessageW; external user32 name 'DispatchMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DispatchMessage: Pointer;

function DispatchMessage;
begin
  GetProcedureAddress(_DispatchMessage, user32, 'DispatchMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DispatchMessage]
  end;
end;
{$ELSE}
function DispatchMessage; external user32 name 'DispatchMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DispatchMessage: Pointer;

function DispatchMessage;
begin
  GetProcedureAddress(_DispatchMessage, user32, 'DispatchMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DispatchMessage]
  end;
end;
{$ELSE}
function DispatchMessage; external user32 name 'DispatchMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetMessageQueue: Pointer;

function SetMessageQueue;
begin
  GetProcedureAddress(_SetMessageQueue, user32, 'SetMessageQueue');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMessageQueue]
  end;
end;
{$ELSE}
function SetMessageQueue; external user32 name 'SetMessageQueue';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PeekMessageA: Pointer;

function PeekMessageA;
begin
  GetProcedureAddress(_PeekMessageA, user32, 'PeekMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekMessageA]
  end;
end;
{$ELSE}
function PeekMessageA; external user32 name 'PeekMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PeekMessageW: Pointer;

function PeekMessageW;
begin
  GetProcedureAddress(_PeekMessageW, user32, 'PeekMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekMessageW]
  end;
end;
{$ELSE}
function PeekMessageW; external user32 name 'PeekMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PeekMessage: Pointer;

function PeekMessage;
begin
  GetProcedureAddress(_PeekMessage, user32, 'PeekMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekMessage]
  end;
end;
{$ELSE}
function PeekMessage; external user32 name 'PeekMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PeekMessage: Pointer;

function PeekMessage;
begin
  GetProcedureAddress(_PeekMessage, user32, 'PeekMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekMessage]
  end;
end;
{$ELSE}
function PeekMessage; external user32 name 'PeekMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterHotKey: Pointer;

function RegisterHotKey;
begin
  GetProcedureAddress(_RegisterHotKey, user32, 'RegisterHotKey');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterHotKey]
  end;
end;
{$ELSE}
function RegisterHotKey; external user32 name 'RegisterHotKey';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterHotKey: Pointer;

function UnregisterHotKey;
begin
  GetProcedureAddress(_UnregisterHotKey, user32, 'UnregisterHotKey');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterHotKey]
  end;
end;
{$ELSE}
function UnregisterHotKey; external user32 name 'UnregisterHotKey';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExitWindowsEx: Pointer;

function ExitWindowsEx;
begin
  GetProcedureAddress(_ExitWindowsEx, user32, 'ExitWindowsEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExitWindowsEx]
  end;
end;
{$ELSE}
function ExitWindowsEx; external user32 name 'ExitWindowsEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SwapMouseButton: Pointer;

function SwapMouseButton;
begin
  GetProcedureAddress(_SwapMouseButton, user32, 'SwapMouseButton');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SwapMouseButton]
  end;
end;
{$ELSE}
function SwapMouseButton; external user32 name 'SwapMouseButton';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessagePos: Pointer;

function GetMessagePos;
begin
  GetProcedureAddress(_GetMessagePos, user32, 'GetMessagePos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessagePos]
  end;
end;
{$ELSE}
function GetMessagePos; external user32 name 'GetMessagePos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessageTime: Pointer;

function GetMessageTime;
begin
  GetProcedureAddress(_GetMessageTime, user32, 'GetMessageTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessageTime]
  end;
end;
{$ELSE}
function GetMessageTime; external user32 name 'GetMessageTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMessageExtraInfo: Pointer;

function GetMessageExtraInfo;
begin
  GetProcedureAddress(_GetMessageExtraInfo, user32, 'GetMessageExtraInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMessageExtraInfo]
  end;
end;
{$ELSE}
function GetMessageExtraInfo; external user32 name 'GetMessageExtraInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMessageExtraInfo: Pointer;

function SetMessageExtraInfo;
begin
  GetProcedureAddress(_SetMessageExtraInfo, user32, 'SetMessageExtraInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMessageExtraInfo]
  end;
end;
{$ELSE}
function SetMessageExtraInfo; external user32 name 'SetMessageExtraInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageA: Pointer;

function SendMessageA;
begin
  GetProcedureAddress(_SendMessageA, user32, 'SendMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageA]
  end;
end;
{$ELSE}
function SendMessageA; external user32 name 'SendMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageW: Pointer;

function SendMessageW;
begin
  GetProcedureAddress(_SendMessageW, user32, 'SendMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageW]
  end;
end;
{$ELSE}
function SendMessageW; external user32 name 'SendMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessage: Pointer;

function SendMessage;
begin
  GetProcedureAddress(_SendMessage, user32, 'SendMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessage]
  end;
end;
{$ELSE}
function SendMessage; external user32 name 'SendMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessage: Pointer;

function SendMessage;
begin
  GetProcedureAddress(_SendMessage, user32, 'SendMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessage]
  end;
end;
{$ELSE}
function SendMessage; external user32 name 'SendMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageTimeoutA: Pointer;

function SendMessageTimeoutA;
begin
  GetProcedureAddress(_SendMessageTimeoutA, user32, 'SendMessageTimeoutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageTimeoutA]
  end;
end;
{$ELSE}
function SendMessageTimeoutA; external user32 name 'SendMessageTimeoutA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageTimeoutW: Pointer;

function SendMessageTimeoutW;
begin
  GetProcedureAddress(_SendMessageTimeoutW, user32, 'SendMessageTimeoutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageTimeoutW]
  end;
end;
{$ELSE}
function SendMessageTimeoutW; external user32 name 'SendMessageTimeoutW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageTimeout: Pointer;

function SendMessageTimeout;
begin
  GetProcedureAddress(_SendMessageTimeout, user32, 'SendMessageTimeoutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageTimeout]
  end;
end;
{$ELSE}
function SendMessageTimeout; external user32 name 'SendMessageTimeoutW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageTimeout: Pointer;

function SendMessageTimeout;
begin
  GetProcedureAddress(_SendMessageTimeout, user32, 'SendMessageTimeoutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageTimeout]
  end;
end;
{$ELSE}
function SendMessageTimeout; external user32 name 'SendMessageTimeoutA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SendNotifyMessageA: Pointer;

function SendNotifyMessageA;
begin
  GetProcedureAddress(_SendNotifyMessageA, user32, 'SendNotifyMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendNotifyMessageA]
  end;
end;
{$ELSE}
function SendNotifyMessageA; external user32 name 'SendNotifyMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendNotifyMessageW: Pointer;

function SendNotifyMessageW;
begin
  GetProcedureAddress(_SendNotifyMessageW, user32, 'SendNotifyMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendNotifyMessageW]
  end;
end;
{$ELSE}
function SendNotifyMessageW; external user32 name 'SendNotifyMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SendNotifyMessage: Pointer;

function SendNotifyMessage;
begin
  GetProcedureAddress(_SendNotifyMessage, user32, 'SendNotifyMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendNotifyMessage]
  end;
end;
{$ELSE}
function SendNotifyMessage; external user32 name 'SendNotifyMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SendNotifyMessage: Pointer;

function SendNotifyMessage;
begin
  GetProcedureAddress(_SendNotifyMessage, user32, 'SendNotifyMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendNotifyMessage]
  end;
end;
{$ELSE}
function SendNotifyMessage; external user32 name 'SendNotifyMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageCallbackA: Pointer;

function SendMessageCallbackA;
begin
  GetProcedureAddress(_SendMessageCallbackA, user32, 'SendMessageCallbackA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageCallbackA]
  end;
end;
{$ELSE}
function SendMessageCallbackA; external user32 name 'SendMessageCallbackA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageCallbackW: Pointer;

function SendMessageCallbackW;
begin
  GetProcedureAddress(_SendMessageCallbackW, user32, 'SendMessageCallbackW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageCallbackW]
  end;
end;
{$ELSE}
function SendMessageCallbackW; external user32 name 'SendMessageCallbackW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageCallback: Pointer;

function SendMessageCallback;
begin
  GetProcedureAddress(_SendMessageCallback, user32, 'SendMessageCallbackW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageCallback]
  end;
end;
{$ELSE}
function SendMessageCallback; external user32 name 'SendMessageCallbackW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SendMessageCallback: Pointer;

function SendMessageCallback;
begin
  GetProcedureAddress(_SendMessageCallback, user32, 'SendMessageCallbackA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendMessageCallback]
  end;
end;
{$ELSE}
function SendMessageCallback; external user32 name 'SendMessageCallbackA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessageExA: Pointer;

function BroadcastSystemMessageExA;
begin
  GetProcedureAddress(_BroadcastSystemMessageExA, user32, 'BroadcastSystemMessageExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessageExA]
  end;
end;
{$ELSE}
function BroadcastSystemMessageExA; external user32 name 'BroadcastSystemMessageExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessageExW: Pointer;

function BroadcastSystemMessageExW;
begin
  GetProcedureAddress(_BroadcastSystemMessageExW, user32, 'BroadcastSystemMessageExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessageExW]
  end;
end;
{$ELSE}
function BroadcastSystemMessageExW; external user32 name 'BroadcastSystemMessageExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessageEx: Pointer;

function BroadcastSystemMessageEx;
begin
  GetProcedureAddress(_BroadcastSystemMessageEx, user32, 'BroadcastSystemMessageExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessageEx]
  end;
end;
{$ELSE}
function BroadcastSystemMessageEx; external user32 name 'BroadcastSystemMessageExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessageEx: Pointer;

function BroadcastSystemMessageEx;
begin
  GetProcedureAddress(_BroadcastSystemMessageEx, user32, 'BroadcastSystemMessageExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessageEx]
  end;
end;
{$ELSE}
function BroadcastSystemMessageEx; external user32 name 'BroadcastSystemMessageExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessageA: Pointer;

function BroadcastSystemMessageA;
begin
  GetProcedureAddress(_BroadcastSystemMessageA, user32, 'BroadcastSystemMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessageA]
  end;
end;
{$ELSE}
function BroadcastSystemMessageA; external user32 name 'BroadcastSystemMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessageW: Pointer;

function BroadcastSystemMessageW;
begin
  GetProcedureAddress(_BroadcastSystemMessageW, user32, 'BroadcastSystemMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessageW]
  end;
end;
{$ELSE}
function BroadcastSystemMessageW; external user32 name 'BroadcastSystemMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessage: Pointer;

function BroadcastSystemMessage;
begin
  GetProcedureAddress(_BroadcastSystemMessage, user32, 'BroadcastSystemMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessage]
  end;
end;
{$ELSE}
function BroadcastSystemMessage; external user32 name 'BroadcastSystemMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _BroadcastSystemMessage: Pointer;

function BroadcastSystemMessage;
begin
  GetProcedureAddress(_BroadcastSystemMessage, user32, 'BroadcastSystemMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BroadcastSystemMessage]
  end;
end;
{$ELSE}
function BroadcastSystemMessage; external user32 name 'BroadcastSystemMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterDeviceNotificationA: Pointer;

function RegisterDeviceNotificationA;
begin
  GetProcedureAddress(_RegisterDeviceNotificationA, user32, 'RegisterDeviceNotificationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterDeviceNotificationA]
  end;
end;
{$ELSE}
function RegisterDeviceNotificationA; external user32 name 'RegisterDeviceNotificationA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterDeviceNotificationW: Pointer;

function RegisterDeviceNotificationW;
begin
  GetProcedureAddress(_RegisterDeviceNotificationW, user32, 'RegisterDeviceNotificationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterDeviceNotificationW]
  end;
end;
{$ELSE}
function RegisterDeviceNotificationW; external user32 name 'RegisterDeviceNotificationW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterDeviceNotification: Pointer;

function RegisterDeviceNotification;
begin
  GetProcedureAddress(_RegisterDeviceNotification, user32, 'RegisterDeviceNotificationW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterDeviceNotification]
  end;
end;
{$ELSE}
function RegisterDeviceNotification; external user32 name 'RegisterDeviceNotificationW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterDeviceNotification: Pointer;

function RegisterDeviceNotification;
begin
  GetProcedureAddress(_RegisterDeviceNotification, user32, 'RegisterDeviceNotificationA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterDeviceNotification]
  end;
end;
{$ELSE}
function RegisterDeviceNotification; external user32 name 'RegisterDeviceNotificationA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterDeviceNotification: Pointer;

function UnregisterDeviceNotification;
begin
  GetProcedureAddress(_UnregisterDeviceNotification, user32, 'UnregisterDeviceNotification');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterDeviceNotification]
  end;
end;
{$ELSE}
function UnregisterDeviceNotification; external user32 name 'UnregisterDeviceNotification';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PostMessageA: Pointer;

function PostMessageA;
begin
  GetProcedureAddress(_PostMessageA, user32, 'PostMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostMessageA]
  end;
end;
{$ELSE}
function PostMessageA; external user32 name 'PostMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PostMessageW: Pointer;

function PostMessageW;
begin
  GetProcedureAddress(_PostMessageW, user32, 'PostMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostMessageW]
  end;
end;
{$ELSE}
function PostMessageW; external user32 name 'PostMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PostMessage: Pointer;

function PostMessage;
begin
  GetProcedureAddress(_PostMessage, user32, 'PostMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostMessage]
  end;
end;
{$ELSE}
function PostMessage; external user32 name 'PostMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PostMessage: Pointer;

function PostMessage;
begin
  GetProcedureAddress(_PostMessage, user32, 'PostMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostMessage]
  end;
end;
{$ELSE}
function PostMessage; external user32 name 'PostMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _PostThreadMessageA: Pointer;

function PostThreadMessageA;
begin
  GetProcedureAddress(_PostThreadMessageA, user32, 'PostThreadMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostThreadMessageA]
  end;
end;
{$ELSE}
function PostThreadMessageA; external user32 name 'PostThreadMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PostThreadMessageW: Pointer;

function PostThreadMessageW;
begin
  GetProcedureAddress(_PostThreadMessageW, user32, 'PostThreadMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostThreadMessageW]
  end;
end;
{$ELSE}
function PostThreadMessageW; external user32 name 'PostThreadMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PostThreadMessage: Pointer;

function PostThreadMessage;
begin
  GetProcedureAddress(_PostThreadMessage, user32, 'PostThreadMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostThreadMessage]
  end;
end;
{$ELSE}
function PostThreadMessage; external user32 name 'PostThreadMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PostThreadMessage: Pointer;

function PostThreadMessage;
begin
  GetProcedureAddress(_PostThreadMessage, user32, 'PostThreadMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostThreadMessage]
  end;
end;
{$ELSE}
function PostThreadMessage; external user32 name 'PostThreadMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AttachThreadInput: Pointer;

function AttachThreadInput;
begin
  GetProcedureAddress(_AttachThreadInput, user32, 'AttachThreadInput');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AttachThreadInput]
  end;
end;
{$ELSE}
function AttachThreadInput; external user32 name 'AttachThreadInput';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReplyMessage: Pointer;

function ReplyMessage;
begin
  GetProcedureAddress(_ReplyMessage, user32, 'ReplyMessage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReplyMessage]
  end;
end;
{$ELSE}
function ReplyMessage; external user32 name 'ReplyMessage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitMessage: Pointer;

function WaitMessage;
begin
  GetProcedureAddress(_WaitMessage, user32, 'WaitMessage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitMessage]
  end;
end;
{$ELSE}
function WaitMessage; external user32 name 'WaitMessage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WaitForInputIdle: Pointer;

function WaitForInputIdle;
begin
  GetProcedureAddress(_WaitForInputIdle, user32, 'WaitForInputIdle');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WaitForInputIdle]
  end;
end;
{$ELSE}
function WaitForInputIdle; external user32 name 'WaitForInputIdle';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefWindowProcA: Pointer;

function DefWindowProcA;
begin
  GetProcedureAddress(_DefWindowProcA, user32, 'DefWindowProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefWindowProcA]
  end;
end;
{$ELSE}
function DefWindowProcA; external user32 name 'DefWindowProcA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefWindowProcW: Pointer;

function DefWindowProcW;
begin
  GetProcedureAddress(_DefWindowProcW, user32, 'DefWindowProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefWindowProcW]
  end;
end;
{$ELSE}
function DefWindowProcW; external user32 name 'DefWindowProcW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DefWindowProc: Pointer;

function DefWindowProc;
begin
  GetProcedureAddress(_DefWindowProc, user32, 'DefWindowProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefWindowProc]
  end;
end;
{$ELSE}
function DefWindowProc; external user32 name 'DefWindowProcW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DefWindowProc: Pointer;

function DefWindowProc;
begin
  GetProcedureAddress(_DefWindowProc, user32, 'DefWindowProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefWindowProc]
  end;
end;
{$ELSE}
function DefWindowProc; external user32 name 'DefWindowProcA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _PostQuitMessage: Pointer;

procedure PostQuitMessage;
begin
  GetProcedureAddress(_PostQuitMessage, user32, 'PostQuitMessage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PostQuitMessage]
  end;
end;
{$ELSE}
procedure PostQuitMessage; external user32 name 'PostQuitMessage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CallWindowProcA: Pointer;

function CallWindowProcA;
begin
  GetProcedureAddress(_CallWindowProcA, user32, 'CallWindowProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallWindowProcA]
  end;
end;
{$ELSE}
function CallWindowProcA; external user32 name 'CallWindowProcA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CallWindowProcW: Pointer;

function CallWindowProcW;
begin
  GetProcedureAddress(_CallWindowProcW, user32, 'CallWindowProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallWindowProcW]
  end;
end;
{$ELSE}
function CallWindowProcW; external user32 name 'CallWindowProcW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CallWindowProc: Pointer;

function CallWindowProc;
begin
  GetProcedureAddress(_CallWindowProc, user32, 'CallWindowProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallWindowProc]
  end;
end;
{$ELSE}
function CallWindowProc; external user32 name 'CallWindowProcW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CallWindowProc: Pointer;

function CallWindowProc;
begin
  GetProcedureAddress(_CallWindowProc, user32, 'CallWindowProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallWindowProc]
  end;
end;
{$ELSE}
function CallWindowProc; external user32 name 'CallWindowProcA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _InSendMessage: Pointer;

function InSendMessage;
begin
  GetProcedureAddress(_InSendMessage, user32, 'InSendMessage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InSendMessage]
  end;
end;
{$ELSE}
function InSendMessage; external user32 name 'InSendMessage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InSendMessageEx: Pointer;

function InSendMessageEx;
begin
  GetProcedureAddress(_InSendMessageEx, user32, 'InSendMessageEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InSendMessageEx]
  end;
end;
{$ELSE}
function InSendMessageEx; external user32 name 'InSendMessageEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDoubleClickTime: Pointer;

function GetDoubleClickTime;
begin
  GetProcedureAddress(_GetDoubleClickTime, user32, 'GetDoubleClickTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDoubleClickTime]
  end;
end;
{$ELSE}
function GetDoubleClickTime; external user32 name 'GetDoubleClickTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDoubleClickTime: Pointer;

function SetDoubleClickTime;
begin
  GetProcedureAddress(_SetDoubleClickTime, user32, 'SetDoubleClickTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDoubleClickTime]
  end;
end;
{$ELSE}
function SetDoubleClickTime; external user32 name 'SetDoubleClickTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClassA: Pointer;

function RegisterClassA;
begin
  GetProcedureAddress(_RegisterClassA, user32, 'RegisterClassA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClassA]
  end;
end;
{$ELSE}
function RegisterClassA; external user32 name 'RegisterClassA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClassW: Pointer;

function RegisterClassW;
begin
  GetProcedureAddress(_RegisterClassW, user32, 'RegisterClassW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClassW]
  end;
end;
{$ELSE}
function RegisterClassW; external user32 name 'RegisterClassW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClass: Pointer;

function RegisterClass;
begin
  GetProcedureAddress(_RegisterClass, user32, 'RegisterClassW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClass]
  end;
end;
{$ELSE}
function RegisterClass; external user32 name 'RegisterClassW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClass: Pointer;

function RegisterClass;
begin
  GetProcedureAddress(_RegisterClass, user32, 'RegisterClassA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClass]
  end;
end;
{$ELSE}
function RegisterClass; external user32 name 'RegisterClassA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterClassA: Pointer;

function UnregisterClassA;
begin
  GetProcedureAddress(_UnregisterClassA, user32, 'UnregisterClassA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterClassA]
  end;
end;
{$ELSE}
function UnregisterClassA; external user32 name 'UnregisterClassA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterClassW: Pointer;

function UnregisterClassW;
begin
  GetProcedureAddress(_UnregisterClassW, user32, 'UnregisterClassW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterClassW]
  end;
end;
{$ELSE}
function UnregisterClassW; external user32 name 'UnregisterClassW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterClass: Pointer;

function UnregisterClass;
begin
  GetProcedureAddress(_UnregisterClass, user32, 'UnregisterClassW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterClass]
  end;
end;
{$ELSE}
function UnregisterClass; external user32 name 'UnregisterClassW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _UnregisterClass: Pointer;

function UnregisterClass;
begin
  GetProcedureAddress(_UnregisterClass, user32, 'UnregisterClassA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnregisterClass]
  end;
end;
{$ELSE}
function UnregisterClass; external user32 name 'UnregisterClassA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfoA: Pointer;

function GetClassInfoA;
begin
  GetProcedureAddress(_GetClassInfoA, user32, 'GetClassInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfoA]
  end;
end;
{$ELSE}
function GetClassInfoA; external user32 name 'GetClassInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfoW: Pointer;

function GetClassInfoW;
begin
  GetProcedureAddress(_GetClassInfoW, user32, 'GetClassInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfoW]
  end;
end;
{$ELSE}
function GetClassInfoW; external user32 name 'GetClassInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfo: Pointer;

function GetClassInfo;
begin
  GetProcedureAddress(_GetClassInfo, user32, 'GetClassInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfo]
  end;
end;
{$ELSE}
function GetClassInfo; external user32 name 'GetClassInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfo: Pointer;

function GetClassInfo;
begin
  GetProcedureAddress(_GetClassInfo, user32, 'GetClassInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfo]
  end;
end;
{$ELSE}
function GetClassInfo; external user32 name 'GetClassInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClassExA: Pointer;

function RegisterClassExA;
begin
  GetProcedureAddress(_RegisterClassExA, user32, 'RegisterClassExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClassExA]
  end;
end;
{$ELSE}
function RegisterClassExA; external user32 name 'RegisterClassExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClassExW: Pointer;

function RegisterClassExW;
begin
  GetProcedureAddress(_RegisterClassExW, user32, 'RegisterClassExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClassExW]
  end;
end;
{$ELSE}
function RegisterClassExW; external user32 name 'RegisterClassExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClassEx: Pointer;

function RegisterClassEx;
begin
  GetProcedureAddress(_RegisterClassEx, user32, 'RegisterClassExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClassEx]
  end;
end;
{$ELSE}
function RegisterClassEx; external user32 name 'RegisterClassExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClassEx: Pointer;

function RegisterClassEx;
begin
  GetProcedureAddress(_RegisterClassEx, user32, 'RegisterClassExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClassEx]
  end;
end;
{$ELSE}
function RegisterClassEx; external user32 name 'RegisterClassExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfoExA: Pointer;

function GetClassInfoExA;
begin
  GetProcedureAddress(_GetClassInfoExA, user32, 'GetClassInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfoExA]
  end;
end;
{$ELSE}
function GetClassInfoExA; external user32 name 'GetClassInfoExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfoExW: Pointer;

function GetClassInfoExW;
begin
  GetProcedureAddress(_GetClassInfoExW, user32, 'GetClassInfoExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfoExW]
  end;
end;
{$ELSE}
function GetClassInfoExW; external user32 name 'GetClassInfoExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfoEx: Pointer;

function GetClassInfoEx;
begin
  GetProcedureAddress(_GetClassInfoEx, user32, 'GetClassInfoExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfoEx]
  end;
end;
{$ELSE}
function GetClassInfoEx; external user32 name 'GetClassInfoExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassInfoEx: Pointer;

function GetClassInfoEx;
begin
  GetProcedureAddress(_GetClassInfoEx, user32, 'GetClassInfoExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassInfoEx]
  end;
end;
{$ELSE}
function GetClassInfoEx; external user32 name 'GetClassInfoExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowExA: Pointer;

function CreateWindowExA;
begin
  GetProcedureAddress(_CreateWindowExA, user32, 'CreateWindowExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowExA]
  end;
end;
{$ELSE}
function CreateWindowExA; external user32 name 'CreateWindowExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowExW: Pointer;

function CreateWindowExW;
begin
  GetProcedureAddress(_CreateWindowExW, user32, 'CreateWindowExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowExW]
  end;
end;
{$ELSE}
function CreateWindowExW; external user32 name 'CreateWindowExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowEx: Pointer;

function CreateWindowEx;
begin
  GetProcedureAddress(_CreateWindowEx, user32, 'CreateWindowExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowEx]
  end;
end;
{$ELSE}
function CreateWindowEx; external user32 name 'CreateWindowExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateWindowEx: Pointer;

function CreateWindowEx;
begin
  GetProcedureAddress(_CreateWindowEx, user32, 'CreateWindowExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateWindowEx]
  end;
end;
{$ELSE}
function CreateWindowEx; external user32 name 'CreateWindowExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsWindow: Pointer;

function IsWindow;
begin
  GetProcedureAddress(_IsWindow, user32, 'IsWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWindow]
  end;
end;
{$ELSE}
function IsWindow; external user32 name 'IsWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsMenu: Pointer;

function IsMenu;
begin
  GetProcedureAddress(_IsMenu, user32, 'IsMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsMenu]
  end;
end;
{$ELSE}
function IsMenu; external user32 name 'IsMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsChild: Pointer;

function IsChild;
begin
  GetProcedureAddress(_IsChild, user32, 'IsChild');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsChild]
  end;
end;
{$ELSE}
function IsChild; external user32 name 'IsChild';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyWindow: Pointer;

function DestroyWindow;
begin
  GetProcedureAddress(_DestroyWindow, user32, 'DestroyWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyWindow]
  end;
end;
{$ELSE}
function DestroyWindow; external user32 name 'DestroyWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ShowWindow: Pointer;

function ShowWindow;
begin
  GetProcedureAddress(_ShowWindow, user32, 'ShowWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ShowWindow]
  end;
end;
{$ELSE}
function ShowWindow; external user32 name 'ShowWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AnimateWindow: Pointer;

function AnimateWindow;
begin
  GetProcedureAddress(_AnimateWindow, user32, 'AnimateWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AnimateWindow]
  end;
end;
{$ELSE}
function AnimateWindow; external user32 name 'AnimateWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateLayeredWindow: Pointer;

function UpdateLayeredWindow;
begin
  GetProcedureAddress(_UpdateLayeredWindow, user32, 'UpdateLayeredWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateLayeredWindow]
  end;
end;
{$ELSE}
function UpdateLayeredWindow; external user32 name 'UpdateLayeredWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLayeredWindowAttributes: Pointer;

function GetLayeredWindowAttributes;
begin
  GetProcedureAddress(_GetLayeredWindowAttributes, user32, 'GetLayeredWindowAttributes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLayeredWindowAttributes]
  end;
end;
{$ELSE}
function GetLayeredWindowAttributes; external user32 name 'GetLayeredWindowAttributes';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PrintWindow: Pointer;

function PrintWindow;
begin
  GetProcedureAddress(_PrintWindow, user32, 'PrintWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrintWindow]
  end;
end;
{$ELSE}
function PrintWindow; external user32 name 'PrintWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetLayeredWindowAttributes: Pointer;

function SetLayeredWindowAttributes;
begin
  GetProcedureAddress(_SetLayeredWindowAttributes, user32, 'SetLayeredWindowAttributes');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLayeredWindowAttributes]
  end;
end;
{$ELSE}
function SetLayeredWindowAttributes; external user32 name 'SetLayeredWindowAttributes';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ShowWindowAsync: Pointer;

function ShowWindowAsync;
begin
  GetProcedureAddress(_ShowWindowAsync, user32, 'ShowWindowAsync');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ShowWindowAsync]
  end;
end;
{$ELSE}
function ShowWindowAsync; external user32 name 'ShowWindowAsync';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlashWindow: Pointer;

function FlashWindow;
begin
  GetProcedureAddress(_FlashWindow, user32, 'FlashWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlashWindow]
  end;
end;
{$ELSE}
function FlashWindow; external user32 name 'FlashWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlashWindowEx: Pointer;

function FlashWindowEx;
begin
  GetProcedureAddress(_FlashWindowEx, user32, 'FlashWindowEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlashWindowEx]
  end;
end;
{$ELSE}
function FlashWindowEx; external user32 name 'FlashWindowEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ShowOwnedPopups: Pointer;

function ShowOwnedPopups;
begin
  GetProcedureAddress(_ShowOwnedPopups, user32, 'ShowOwnedPopups');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ShowOwnedPopups]
  end;
end;
{$ELSE}
function ShowOwnedPopups; external user32 name 'ShowOwnedPopups';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OpenIcon: Pointer;

function OpenIcon;
begin
  GetProcedureAddress(_OpenIcon, user32, 'OpenIcon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenIcon]
  end;
end;
{$ELSE}
function OpenIcon; external user32 name 'OpenIcon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseWindow: Pointer;

function CloseWindow;
begin
  GetProcedureAddress(_CloseWindow, user32, 'CloseWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseWindow]
  end;
end;
{$ELSE}
function CloseWindow; external user32 name 'CloseWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MoveWindow: Pointer;

function MoveWindow;
begin
  GetProcedureAddress(_MoveWindow, user32, 'MoveWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MoveWindow]
  end;
end;
{$ELSE}
function MoveWindow; external user32 name 'MoveWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowPos: Pointer;

function SetWindowPos;
begin
  GetProcedureAddress(_SetWindowPos, user32, 'SetWindowPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowPos]
  end;
end;
{$ELSE}
function SetWindowPos; external user32 name 'SetWindowPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowPlacement: Pointer;

function GetWindowPlacement;
begin
  GetProcedureAddress(_GetWindowPlacement, user32, 'GetWindowPlacement');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowPlacement]
  end;
end;
{$ELSE}
function GetWindowPlacement; external user32 name 'GetWindowPlacement';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowPlacement: Pointer;

function SetWindowPlacement;
begin
  GetProcedureAddress(_SetWindowPlacement, user32, 'SetWindowPlacement');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowPlacement]
  end;
end;
{$ELSE}
function SetWindowPlacement; external user32 name 'SetWindowPlacement';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BeginDeferWindowPos: Pointer;

function BeginDeferWindowPos;
begin
  GetProcedureAddress(_BeginDeferWindowPos, user32, 'BeginDeferWindowPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginDeferWindowPos]
  end;
end;
{$ELSE}
function BeginDeferWindowPos; external user32 name 'BeginDeferWindowPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeferWindowPos: Pointer;

function DeferWindowPos;
begin
  GetProcedureAddress(_DeferWindowPos, user32, 'DeferWindowPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeferWindowPos]
  end;
end;
{$ELSE}
function DeferWindowPos; external user32 name 'DeferWindowPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndDeferWindowPos: Pointer;

function EndDeferWindowPos;
begin
  GetProcedureAddress(_EndDeferWindowPos, user32, 'EndDeferWindowPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndDeferWindowPos]
  end;
end;
{$ELSE}
function EndDeferWindowPos; external user32 name 'EndDeferWindowPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsWindowVisible: Pointer;

function IsWindowVisible;
begin
  GetProcedureAddress(_IsWindowVisible, user32, 'IsWindowVisible');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWindowVisible]
  end;
end;
{$ELSE}
function IsWindowVisible; external user32 name 'IsWindowVisible';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsIconic: Pointer;

function IsIconic;
begin
  GetProcedureAddress(_IsIconic, user32, 'IsIconic');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsIconic]
  end;
end;
{$ELSE}
function IsIconic; external user32 name 'IsIconic';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AnyPopup: Pointer;

function AnyPopup;
begin
  GetProcedureAddress(_AnyPopup, user32, 'AnyPopup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AnyPopup]
  end;
end;
{$ELSE}
function AnyPopup; external user32 name 'AnyPopup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BringWindowToTop: Pointer;

function BringWindowToTop;
begin
  GetProcedureAddress(_BringWindowToTop, user32, 'BringWindowToTop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BringWindowToTop]
  end;
end;
{$ELSE}
function BringWindowToTop; external user32 name 'BringWindowToTop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsZoomed: Pointer;

function IsZoomed;
begin
  GetProcedureAddress(_IsZoomed, user32, 'IsZoomed');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsZoomed]
  end;
end;
{$ELSE}
function IsZoomed; external user32 name 'IsZoomed';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogParamA: Pointer;

function CreateDialogParamA;
begin
  GetProcedureAddress(_CreateDialogParamA, user32, 'CreateDialogParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogParamA]
  end;
end;
{$ELSE}
function CreateDialogParamA; external user32 name 'CreateDialogParamA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogParamW: Pointer;

function CreateDialogParamW;
begin
  GetProcedureAddress(_CreateDialogParamW, user32, 'CreateDialogParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogParamW]
  end;
end;
{$ELSE}
function CreateDialogParamW; external user32 name 'CreateDialogParamW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogParam: Pointer;

function CreateDialogParam;
begin
  GetProcedureAddress(_CreateDialogParam, user32, 'CreateDialogParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogParam]
  end;
end;
{$ELSE}
function CreateDialogParam; external user32 name 'CreateDialogParamW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogParam: Pointer;

function CreateDialogParam;
begin
  GetProcedureAddress(_CreateDialogParam, user32, 'CreateDialogParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogParam]
  end;
end;
{$ELSE}
function CreateDialogParam; external user32 name 'CreateDialogParamA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogIndirectParamA: Pointer;

function CreateDialogIndirectParamA;
begin
  GetProcedureAddress(_CreateDialogIndirectParamA, user32, 'CreateDialogIndirectParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogIndirectParamA]
  end;
end;
{$ELSE}
function CreateDialogIndirectParamA; external user32 name 'CreateDialogIndirectParamA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogIndirectParamW: Pointer;

function CreateDialogIndirectParamW;
begin
  GetProcedureAddress(_CreateDialogIndirectParamW, user32, 'CreateDialogIndirectParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogIndirectParamW]
  end;
end;
{$ELSE}
function CreateDialogIndirectParamW; external user32 name 'CreateDialogIndirectParamW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogIndirectParam: Pointer;

function CreateDialogIndirectParam;
begin
  GetProcedureAddress(_CreateDialogIndirectParam, user32, 'CreateDialogIndirectParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogIndirectParam]
  end;
end;
{$ELSE}
function CreateDialogIndirectParam; external user32 name 'CreateDialogIndirectParamW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateDialogIndirectParam: Pointer;

function CreateDialogIndirectParam;
begin
  GetProcedureAddress(_CreateDialogIndirectParam, user32, 'CreateDialogIndirectParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateDialogIndirectParam]
  end;
end;
{$ELSE}
function CreateDialogIndirectParam; external user32 name 'CreateDialogIndirectParamA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxParamA: Pointer;

function DialogBoxParamA;
begin
  GetProcedureAddress(_DialogBoxParamA, user32, 'DialogBoxParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxParamA]
  end;
end;
{$ELSE}
function DialogBoxParamA; external user32 name 'DialogBoxParamA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxParamW: Pointer;

function DialogBoxParamW;
begin
  GetProcedureAddress(_DialogBoxParamW, user32, 'DialogBoxParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxParamW]
  end;
end;
{$ELSE}
function DialogBoxParamW; external user32 name 'DialogBoxParamW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxParam: Pointer;

function DialogBoxParam;
begin
  GetProcedureAddress(_DialogBoxParam, user32, 'DialogBoxParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxParam]
  end;
end;
{$ELSE}
function DialogBoxParam; external user32 name 'DialogBoxParamW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxParam: Pointer;

function DialogBoxParam;
begin
  GetProcedureAddress(_DialogBoxParam, user32, 'DialogBoxParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxParam]
  end;
end;
{$ELSE}
function DialogBoxParam; external user32 name 'DialogBoxParamA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxIndirectParamA: Pointer;

function DialogBoxIndirectParamA;
begin
  GetProcedureAddress(_DialogBoxIndirectParamA, user32, 'DialogBoxIndirectParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxIndirectParamA]
  end;
end;
{$ELSE}
function DialogBoxIndirectParamA; external user32 name 'DialogBoxIndirectParamA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxIndirectParamW: Pointer;

function DialogBoxIndirectParamW;
begin
  GetProcedureAddress(_DialogBoxIndirectParamW, user32, 'DialogBoxIndirectParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxIndirectParamW]
  end;
end;
{$ELSE}
function DialogBoxIndirectParamW; external user32 name 'DialogBoxIndirectParamW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxIndirectParam: Pointer;

function DialogBoxIndirectParam;
begin
  GetProcedureAddress(_DialogBoxIndirectParam, user32, 'DialogBoxIndirectParamW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxIndirectParam]
  end;
end;
{$ELSE}
function DialogBoxIndirectParam; external user32 name 'DialogBoxIndirectParamW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DialogBoxIndirectParam: Pointer;

function DialogBoxIndirectParam;
begin
  GetProcedureAddress(_DialogBoxIndirectParam, user32, 'DialogBoxIndirectParamA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DialogBoxIndirectParam]
  end;
end;
{$ELSE}
function DialogBoxIndirectParam; external user32 name 'DialogBoxIndirectParamA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EndDialog: Pointer;

function EndDialog;
begin
  GetProcedureAddress(_EndDialog, user32, 'EndDialog');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndDialog]
  end;
end;
{$ELSE}
function EndDialog; external user32 name 'EndDialog';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgItem: Pointer;

function GetDlgItem;
begin
  GetProcedureAddress(_GetDlgItem, user32, 'GetDlgItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgItem]
  end;
end;
{$ELSE}
function GetDlgItem; external user32 name 'GetDlgItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDlgItemInt: Pointer;

function SetDlgItemInt;
begin
  GetProcedureAddress(_SetDlgItemInt, user32, 'SetDlgItemInt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDlgItemInt]
  end;
end;
{$ELSE}
function SetDlgItemInt; external user32 name 'SetDlgItemInt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgItemInt: Pointer;

function GetDlgItemInt;
begin
  GetProcedureAddress(_GetDlgItemInt, user32, 'GetDlgItemInt');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgItemInt]
  end;
end;
{$ELSE}
function GetDlgItemInt; external user32 name 'GetDlgItemInt';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDlgItemTextA: Pointer;

function SetDlgItemTextA;
begin
  GetProcedureAddress(_SetDlgItemTextA, user32, 'SetDlgItemTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDlgItemTextA]
  end;
end;
{$ELSE}
function SetDlgItemTextA; external user32 name 'SetDlgItemTextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetDlgItemTextW: Pointer;

function SetDlgItemTextW;
begin
  GetProcedureAddress(_SetDlgItemTextW, user32, 'SetDlgItemTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDlgItemTextW]
  end;
end;
{$ELSE}
function SetDlgItemTextW; external user32 name 'SetDlgItemTextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetDlgItemText: Pointer;

function SetDlgItemText;
begin
  GetProcedureAddress(_SetDlgItemText, user32, 'SetDlgItemTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDlgItemText]
  end;
end;
{$ELSE}
function SetDlgItemText; external user32 name 'SetDlgItemTextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetDlgItemText: Pointer;

function SetDlgItemText;
begin
  GetProcedureAddress(_SetDlgItemText, user32, 'SetDlgItemTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDlgItemText]
  end;
end;
{$ELSE}
function SetDlgItemText; external user32 name 'SetDlgItemTextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgItemTextA: Pointer;

function GetDlgItemTextA;
begin
  GetProcedureAddress(_GetDlgItemTextA, user32, 'GetDlgItemTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgItemTextA]
  end;
end;
{$ELSE}
function GetDlgItemTextA; external user32 name 'GetDlgItemTextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgItemTextW: Pointer;

function GetDlgItemTextW;
begin
  GetProcedureAddress(_GetDlgItemTextW, user32, 'GetDlgItemTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgItemTextW]
  end;
end;
{$ELSE}
function GetDlgItemTextW; external user32 name 'GetDlgItemTextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgItemText: Pointer;

function GetDlgItemText;
begin
  GetProcedureAddress(_GetDlgItemText, user32, 'GetDlgItemTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgItemText]
  end;
end;
{$ELSE}
function GetDlgItemText; external user32 name 'GetDlgItemTextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgItemText: Pointer;

function GetDlgItemText;
begin
  GetProcedureAddress(_GetDlgItemText, user32, 'GetDlgItemTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgItemText]
  end;
end;
{$ELSE}
function GetDlgItemText; external user32 name 'GetDlgItemTextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CheckDlgButton: Pointer;

function CheckDlgButton;
begin
  GetProcedureAddress(_CheckDlgButton, user32, 'CheckDlgButton');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckDlgButton]
  end;
end;
{$ELSE}
function CheckDlgButton; external user32 name 'CheckDlgButton';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CheckRadioButton: Pointer;

function CheckRadioButton;
begin
  GetProcedureAddress(_CheckRadioButton, user32, 'CheckRadioButton');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckRadioButton]
  end;
end;
{$ELSE}
function CheckRadioButton; external user32 name 'CheckRadioButton';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsDlgButtonChecked: Pointer;

function IsDlgButtonChecked;
begin
  GetProcedureAddress(_IsDlgButtonChecked, user32, 'IsDlgButtonChecked');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDlgButtonChecked]
  end;
end;
{$ELSE}
function IsDlgButtonChecked; external user32 name 'IsDlgButtonChecked';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendDlgItemMessageA: Pointer;

function SendDlgItemMessageA;
begin
  GetProcedureAddress(_SendDlgItemMessageA, user32, 'SendDlgItemMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendDlgItemMessageA]
  end;
end;
{$ELSE}
function SendDlgItemMessageA; external user32 name 'SendDlgItemMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendDlgItemMessageW: Pointer;

function SendDlgItemMessageW;
begin
  GetProcedureAddress(_SendDlgItemMessageW, user32, 'SendDlgItemMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendDlgItemMessageW]
  end;
end;
{$ELSE}
function SendDlgItemMessageW; external user32 name 'SendDlgItemMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SendDlgItemMessage: Pointer;

function SendDlgItemMessage;
begin
  GetProcedureAddress(_SendDlgItemMessage, user32, 'SendDlgItemMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendDlgItemMessage]
  end;
end;
{$ELSE}
function SendDlgItemMessage; external user32 name 'SendDlgItemMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SendDlgItemMessage: Pointer;

function SendDlgItemMessage;
begin
  GetProcedureAddress(_SendDlgItemMessage, user32, 'SendDlgItemMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendDlgItemMessage]
  end;
end;
{$ELSE}
function SendDlgItemMessage; external user32 name 'SendDlgItemMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetNextDlgGroupItem: Pointer;

function GetNextDlgGroupItem;
begin
  GetProcedureAddress(_GetNextDlgGroupItem, user32, 'GetNextDlgGroupItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNextDlgGroupItem]
  end;
end;
{$ELSE}
function GetNextDlgGroupItem; external user32 name 'GetNextDlgGroupItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNextDlgTabItem: Pointer;

function GetNextDlgTabItem;
begin
  GetProcedureAddress(_GetNextDlgTabItem, user32, 'GetNextDlgTabItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNextDlgTabItem]
  end;
end;
{$ELSE}
function GetNextDlgTabItem; external user32 name 'GetNextDlgTabItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDlgCtrlID: Pointer;

function GetDlgCtrlID;
begin
  GetProcedureAddress(_GetDlgCtrlID, user32, 'GetDlgCtrlID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDlgCtrlID]
  end;
end;
{$ELSE}
function GetDlgCtrlID; external user32 name 'GetDlgCtrlID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDialogBaseUnits: Pointer;

function GetDialogBaseUnits;
begin
  GetProcedureAddress(_GetDialogBaseUnits, user32, 'GetDialogBaseUnits');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDialogBaseUnits]
  end;
end;
{$ELSE}
function GetDialogBaseUnits; external user32 name 'GetDialogBaseUnits';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefDlgProcA: Pointer;

function DefDlgProcA;
begin
  GetProcedureAddress(_DefDlgProcA, user32, 'DefDlgProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefDlgProcA]
  end;
end;
{$ELSE}
function DefDlgProcA; external user32 name 'DefDlgProcA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefDlgProcW: Pointer;

function DefDlgProcW;
begin
  GetProcedureAddress(_DefDlgProcW, user32, 'DefDlgProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefDlgProcW]
  end;
end;
{$ELSE}
function DefDlgProcW; external user32 name 'DefDlgProcW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DefDlgProc: Pointer;

function DefDlgProc;
begin
  GetProcedureAddress(_DefDlgProc, user32, 'DefDlgProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefDlgProc]
  end;
end;
{$ELSE}
function DefDlgProc; external user32 name 'DefDlgProcW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DefDlgProc: Pointer;

function DefDlgProc;
begin
  GetProcedureAddress(_DefDlgProc, user32, 'DefDlgProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefDlgProc]
  end;
end;
{$ELSE}
function DefDlgProc; external user32 name 'DefDlgProcA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CallMsgFilterA: Pointer;

function CallMsgFilterA;
begin
  GetProcedureAddress(_CallMsgFilterA, user32, 'CallMsgFilterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallMsgFilterA]
  end;
end;
{$ELSE}
function CallMsgFilterA; external user32 name 'CallMsgFilterA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CallMsgFilterW: Pointer;

function CallMsgFilterW;
begin
  GetProcedureAddress(_CallMsgFilterW, user32, 'CallMsgFilterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallMsgFilterW]
  end;
end;
{$ELSE}
function CallMsgFilterW; external user32 name 'CallMsgFilterW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CallMsgFilter: Pointer;

function CallMsgFilter;
begin
  GetProcedureAddress(_CallMsgFilter, user32, 'CallMsgFilterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallMsgFilter]
  end;
end;
{$ELSE}
function CallMsgFilter; external user32 name 'CallMsgFilterW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CallMsgFilter: Pointer;

function CallMsgFilter;
begin
  GetProcedureAddress(_CallMsgFilter, user32, 'CallMsgFilterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallMsgFilter]
  end;
end;
{$ELSE}
function CallMsgFilter; external user32 name 'CallMsgFilterA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OpenClipboard: Pointer;

function OpenClipboard;
begin
  GetProcedureAddress(_OpenClipboard, user32, 'OpenClipboard');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OpenClipboard]
  end;
end;
{$ELSE}
function OpenClipboard; external user32 name 'OpenClipboard';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CloseClipboard: Pointer;

function CloseClipboard;
begin
  GetProcedureAddress(_CloseClipboard, user32, 'CloseClipboard');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CloseClipboard]
  end;
end;
{$ELSE}
function CloseClipboard; external user32 name 'CloseClipboard';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardSequenceNumber: Pointer;

function GetClipboardSequenceNumber;
begin
  GetProcedureAddress(_GetClipboardSequenceNumber, user32, 'GetClipboardSequenceNumber');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardSequenceNumber]
  end;
end;
{$ELSE}
function GetClipboardSequenceNumber; external user32 name 'GetClipboardSequenceNumber';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardOwner: Pointer;

function GetClipboardOwner;
begin
  GetProcedureAddress(_GetClipboardOwner, user32, 'GetClipboardOwner');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardOwner]
  end;
end;
{$ELSE}
function GetClipboardOwner; external user32 name 'GetClipboardOwner';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetClipboardViewer: Pointer;

function SetClipboardViewer;
begin
  GetProcedureAddress(_SetClipboardViewer, user32, 'SetClipboardViewer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClipboardViewer]
  end;
end;
{$ELSE}
function SetClipboardViewer; external user32 name 'SetClipboardViewer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardViewer: Pointer;

function GetClipboardViewer;
begin
  GetProcedureAddress(_GetClipboardViewer, user32, 'GetClipboardViewer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardViewer]
  end;
end;
{$ELSE}
function GetClipboardViewer; external user32 name 'GetClipboardViewer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeClipboardChain: Pointer;

function ChangeClipboardChain;
begin
  GetProcedureAddress(_ChangeClipboardChain, user32, 'ChangeClipboardChain');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeClipboardChain]
  end;
end;
{$ELSE}
function ChangeClipboardChain; external user32 name 'ChangeClipboardChain';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetClipboardData: Pointer;

function SetClipboardData;
begin
  GetProcedureAddress(_SetClipboardData, user32, 'SetClipboardData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClipboardData]
  end;
end;
{$ELSE}
function SetClipboardData; external user32 name 'SetClipboardData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardData: Pointer;

function GetClipboardData;
begin
  GetProcedureAddress(_GetClipboardData, user32, 'GetClipboardData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardData]
  end;
end;
{$ELSE}
function GetClipboardData; external user32 name 'GetClipboardData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClipboardFormatA: Pointer;

function RegisterClipboardFormatA;
begin
  GetProcedureAddress(_RegisterClipboardFormatA, user32, 'RegisterClipboardFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClipboardFormatA]
  end;
end;
{$ELSE}
function RegisterClipboardFormatA; external user32 name 'RegisterClipboardFormatA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClipboardFormatW: Pointer;

function RegisterClipboardFormatW;
begin
  GetProcedureAddress(_RegisterClipboardFormatW, user32, 'RegisterClipboardFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClipboardFormatW]
  end;
end;
{$ELSE}
function RegisterClipboardFormatW; external user32 name 'RegisterClipboardFormatW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClipboardFormat: Pointer;

function RegisterClipboardFormat;
begin
  GetProcedureAddress(_RegisterClipboardFormat, user32, 'RegisterClipboardFormatW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClipboardFormat]
  end;
end;
{$ELSE}
function RegisterClipboardFormat; external user32 name 'RegisterClipboardFormatW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterClipboardFormat: Pointer;

function RegisterClipboardFormat;
begin
  GetProcedureAddress(_RegisterClipboardFormat, user32, 'RegisterClipboardFormatA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterClipboardFormat]
  end;
end;
{$ELSE}
function RegisterClipboardFormat; external user32 name 'RegisterClipboardFormatA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CountClipboardFormats: Pointer;

function CountClipboardFormats;
begin
  GetProcedureAddress(_CountClipboardFormats, user32, 'CountClipboardFormats');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CountClipboardFormats]
  end;
end;
{$ELSE}
function CountClipboardFormats; external user32 name 'CountClipboardFormats';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumClipboardFormats: Pointer;

function EnumClipboardFormats;
begin
  GetProcedureAddress(_EnumClipboardFormats, user32, 'EnumClipboardFormats');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumClipboardFormats]
  end;
end;
{$ELSE}
function EnumClipboardFormats; external user32 name 'EnumClipboardFormats';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardFormatNameA: Pointer;

function GetClipboardFormatNameA;
begin
  GetProcedureAddress(_GetClipboardFormatNameA, user32, 'GetClipboardFormatNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardFormatNameA]
  end;
end;
{$ELSE}
function GetClipboardFormatNameA; external user32 name 'GetClipboardFormatNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardFormatNameW: Pointer;

function GetClipboardFormatNameW;
begin
  GetProcedureAddress(_GetClipboardFormatNameW, user32, 'GetClipboardFormatNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardFormatNameW]
  end;
end;
{$ELSE}
function GetClipboardFormatNameW; external user32 name 'GetClipboardFormatNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardFormatName: Pointer;

function GetClipboardFormatName;
begin
  GetProcedureAddress(_GetClipboardFormatName, user32, 'GetClipboardFormatNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardFormatName]
  end;
end;
{$ELSE}
function GetClipboardFormatName; external user32 name 'GetClipboardFormatNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipboardFormatName: Pointer;

function GetClipboardFormatName;
begin
  GetProcedureAddress(_GetClipboardFormatName, user32, 'GetClipboardFormatNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipboardFormatName]
  end;
end;
{$ELSE}
function GetClipboardFormatName; external user32 name 'GetClipboardFormatNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EmptyClipboard: Pointer;

function EmptyClipboard;
begin
  GetProcedureAddress(_EmptyClipboard, user32, 'EmptyClipboard');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EmptyClipboard]
  end;
end;
{$ELSE}
function EmptyClipboard; external user32 name 'EmptyClipboard';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsClipboardFormatAvailable: Pointer;

function IsClipboardFormatAvailable;
begin
  GetProcedureAddress(_IsClipboardFormatAvailable, user32, 'IsClipboardFormatAvailable');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsClipboardFormatAvailable]
  end;
end;
{$ELSE}
function IsClipboardFormatAvailable; external user32 name 'IsClipboardFormatAvailable';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPriorityClipboardFormat: Pointer;

function GetPriorityClipboardFormat;
begin
  GetProcedureAddress(_GetPriorityClipboardFormat, user32, 'GetPriorityClipboardFormat');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPriorityClipboardFormat]
  end;
end;
{$ELSE}
function GetPriorityClipboardFormat; external user32 name 'GetPriorityClipboardFormat';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetOpenClipboardWindow: Pointer;

function GetOpenClipboardWindow;
begin
  GetProcedureAddress(_GetOpenClipboardWindow, user32, 'GetOpenClipboardWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetOpenClipboardWindow]
  end;
end;
{$ELSE}
function GetOpenClipboardWindow; external user32 name 'GetOpenClipboardWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOemA: Pointer;

function CharToOemA;
begin
  GetProcedureAddress(_CharToOemA, user32, 'CharToOemA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOemA]
  end;
end;
{$ELSE}
function CharToOemA; external user32 name 'CharToOemA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOemW: Pointer;

function CharToOemW;
begin
  GetProcedureAddress(_CharToOemW, user32, 'CharToOemW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOemW]
  end;
end;
{$ELSE}
function CharToOemW; external user32 name 'CharToOemW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOem: Pointer;

function CharToOem;
begin
  GetProcedureAddress(_CharToOem, user32, 'CharToOemW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOem]
  end;
end;
{$ELSE}
function CharToOem; external user32 name 'CharToOemW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOem: Pointer;

function CharToOem;
begin
  GetProcedureAddress(_CharToOem, user32, 'CharToOemA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOem]
  end;
end;
{$ELSE}
function CharToOem; external user32 name 'CharToOemA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OemToCharA: Pointer;

function OemToCharA;
begin
  GetProcedureAddress(_OemToCharA, user32, 'OemToCharA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToCharA]
  end;
end;
{$ELSE}
function OemToCharA; external user32 name 'OemToCharA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OemToCharW: Pointer;

function OemToCharW;
begin
  GetProcedureAddress(_OemToCharW, user32, 'OemToCharW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToCharW]
  end;
end;
{$ELSE}
function OemToCharW; external user32 name 'OemToCharW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OemToChar: Pointer;

function OemToChar;
begin
  GetProcedureAddress(_OemToChar, user32, 'OemToCharW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToChar]
  end;
end;
{$ELSE}
function OemToChar; external user32 name 'OemToCharW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OemToChar: Pointer;

function OemToChar;
begin
  GetProcedureAddress(_OemToChar, user32, 'OemToCharA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToChar]
  end;
end;
{$ELSE}
function OemToChar; external user32 name 'OemToCharA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOemBuffA: Pointer;

function CharToOemBuffA;
begin
  GetProcedureAddress(_CharToOemBuffA, user32, 'CharToOemBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOemBuffA]
  end;
end;
{$ELSE}
function CharToOemBuffA; external user32 name 'CharToOemBuffA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOemBuffW: Pointer;

function CharToOemBuffW;
begin
  GetProcedureAddress(_CharToOemBuffW, user32, 'CharToOemBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOemBuffW]
  end;
end;
{$ELSE}
function CharToOemBuffW; external user32 name 'CharToOemBuffW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOemBuff: Pointer;

function CharToOemBuff;
begin
  GetProcedureAddress(_CharToOemBuff, user32, 'CharToOemBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOemBuff]
  end;
end;
{$ELSE}
function CharToOemBuff; external user32 name 'CharToOemBuffW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharToOemBuff: Pointer;

function CharToOemBuff;
begin
  GetProcedureAddress(_CharToOemBuff, user32, 'CharToOemBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharToOemBuff]
  end;
end;
{$ELSE}
function CharToOemBuff; external user32 name 'CharToOemBuffA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _OemToCharBuffA: Pointer;

function OemToCharBuffA;
begin
  GetProcedureAddress(_OemToCharBuffA, user32, 'OemToCharBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToCharBuffA]
  end;
end;
{$ELSE}
function OemToCharBuffA; external user32 name 'OemToCharBuffA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OemToCharBuffW: Pointer;

function OemToCharBuffW;
begin
  GetProcedureAddress(_OemToCharBuffW, user32, 'OemToCharBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToCharBuffW]
  end;
end;
{$ELSE}
function OemToCharBuffW; external user32 name 'OemToCharBuffW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _OemToCharBuff: Pointer;

function OemToCharBuff;
begin
  GetProcedureAddress(_OemToCharBuff, user32, 'OemToCharBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToCharBuff]
  end;
end;
{$ELSE}
function OemToCharBuff; external user32 name 'OemToCharBuffW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _OemToCharBuff: Pointer;

function OemToCharBuff;
begin
  GetProcedureAddress(_OemToCharBuff, user32, 'OemToCharBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemToCharBuff]
  end;
end;
{$ELSE}
function OemToCharBuff; external user32 name 'OemToCharBuffA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpperA: Pointer;

function CharUpperA;
begin
  GetProcedureAddress(_CharUpperA, user32, 'CharUpperA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpperA]
  end;
end;
{$ELSE}
function CharUpperA; external user32 name 'CharUpperA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpperW: Pointer;

function CharUpperW;
begin
  GetProcedureAddress(_CharUpperW, user32, 'CharUpperW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpperW]
  end;
end;
{$ELSE}
function CharUpperW; external user32 name 'CharUpperW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpper: Pointer;

function CharUpper;
begin
  GetProcedureAddress(_CharUpper, user32, 'CharUpperW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpper]
  end;
end;
{$ELSE}
function CharUpper; external user32 name 'CharUpperW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpper: Pointer;

function CharUpper;
begin
  GetProcedureAddress(_CharUpper, user32, 'CharUpperA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpper]
  end;
end;
{$ELSE}
function CharUpper; external user32 name 'CharUpperA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpperBuffA: Pointer;

function CharUpperBuffA;
begin
  GetProcedureAddress(_CharUpperBuffA, user32, 'CharUpperBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpperBuffA]
  end;
end;
{$ELSE}
function CharUpperBuffA; external user32 name 'CharUpperBuffA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpperBuffW: Pointer;

function CharUpperBuffW;
begin
  GetProcedureAddress(_CharUpperBuffW, user32, 'CharUpperBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpperBuffW]
  end;
end;
{$ELSE}
function CharUpperBuffW; external user32 name 'CharUpperBuffW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpperBuff: Pointer;

function CharUpperBuff;
begin
  GetProcedureAddress(_CharUpperBuff, user32, 'CharUpperBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpperBuff]
  end;
end;
{$ELSE}
function CharUpperBuff; external user32 name 'CharUpperBuffW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharUpperBuff: Pointer;

function CharUpperBuff;
begin
  GetProcedureAddress(_CharUpperBuff, user32, 'CharUpperBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharUpperBuff]
  end;
end;
{$ELSE}
function CharUpperBuff; external user32 name 'CharUpperBuffA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharLowerA: Pointer;

function CharLowerA;
begin
  GetProcedureAddress(_CharLowerA, user32, 'CharLowerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLowerA]
  end;
end;
{$ELSE}
function CharLowerA; external user32 name 'CharLowerA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharLowerW: Pointer;

function CharLowerW;
begin
  GetProcedureAddress(_CharLowerW, user32, 'CharLowerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLowerW]
  end;
end;
{$ELSE}
function CharLowerW; external user32 name 'CharLowerW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharLower: Pointer;

function CharLower;
begin
  GetProcedureAddress(_CharLower, user32, 'CharLowerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLower]
  end;
end;
{$ELSE}
function CharLower; external user32 name 'CharLowerW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharLower: Pointer;

function CharLower;
begin
  GetProcedureAddress(_CharLower, user32, 'CharLowerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLower]
  end;
end;
{$ELSE}
function CharLower; external user32 name 'CharLowerA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharLowerBuffA: Pointer;

function CharLowerBuffA;
begin
  GetProcedureAddress(_CharLowerBuffA, user32, 'CharLowerBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLowerBuffA]
  end;
end;
{$ELSE}
function CharLowerBuffA; external user32 name 'CharLowerBuffA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharLowerBuffW: Pointer;

function CharLowerBuffW;
begin
  GetProcedureAddress(_CharLowerBuffW, user32, 'CharLowerBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLowerBuffW]
  end;
end;
{$ELSE}
function CharLowerBuffW; external user32 name 'CharLowerBuffW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharLowerBuff: Pointer;

function CharLowerBuff;
begin
  GetProcedureAddress(_CharLowerBuff, user32, 'CharLowerBuffW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLowerBuff]
  end;
end;
{$ELSE}
function CharLowerBuff; external user32 name 'CharLowerBuffW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharLowerBuff: Pointer;

function CharLowerBuff;
begin
  GetProcedureAddress(_CharLowerBuff, user32, 'CharLowerBuffA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharLowerBuff]
  end;
end;
{$ELSE}
function CharLowerBuff; external user32 name 'CharLowerBuffA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharNextA: Pointer;

function CharNextA;
begin
  GetProcedureAddress(_CharNextA, user32, 'CharNextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharNextA]
  end;
end;
{$ELSE}
function CharNextA; external user32 name 'CharNextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharNextW: Pointer;

function CharNextW;
begin
  GetProcedureAddress(_CharNextW, user32, 'CharNextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharNextW]
  end;
end;
{$ELSE}
function CharNextW; external user32 name 'CharNextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharNext: Pointer;

function CharNext;
begin
  GetProcedureAddress(_CharNext, user32, 'CharNextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharNext]
  end;
end;
{$ELSE}
function CharNext; external user32 name 'CharNextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharNext: Pointer;

function CharNext;
begin
  GetProcedureAddress(_CharNext, user32, 'CharNextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharNext]
  end;
end;
{$ELSE}
function CharNext; external user32 name 'CharNextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharPrevA: Pointer;

function CharPrevA;
begin
  GetProcedureAddress(_CharPrevA, user32, 'CharPrevA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharPrevA]
  end;
end;
{$ELSE}
function CharPrevA; external user32 name 'CharPrevA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharPrevW: Pointer;

function CharPrevW;
begin
  GetProcedureAddress(_CharPrevW, user32, 'CharPrevW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharPrevW]
  end;
end;
{$ELSE}
function CharPrevW; external user32 name 'CharPrevW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CharPrev: Pointer;

function CharPrev;
begin
  GetProcedureAddress(_CharPrev, user32, 'CharPrevW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharPrev]
  end;
end;
{$ELSE}
function CharPrev; external user32 name 'CharPrevW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CharPrev: Pointer;

function CharPrev;
begin
  GetProcedureAddress(_CharPrev, user32, 'CharPrevA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharPrev]
  end;
end;
{$ELSE}
function CharPrev; external user32 name 'CharPrevA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CharNextExA: Pointer;

function CharNextExA;
begin
  GetProcedureAddress(_CharNextExA, user32, 'CharNextExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharNextExA]
  end;
end;
{$ELSE}
function CharNextExA; external user32 name 'CharNextExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CharPrevExA: Pointer;

function CharPrevExA;
begin
  GetProcedureAddress(_CharPrevExA, user32, 'CharPrevExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CharPrevExA]
  end;
end;
{$ELSE}
function CharPrevExA; external user32 name 'CharPrevExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlphaA: Pointer;

function IsCharAlphaA;
begin
  GetProcedureAddress(_IsCharAlphaA, user32, 'IsCharAlphaA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlphaA]
  end;
end;
{$ELSE}
function IsCharAlphaA; external user32 name 'IsCharAlphaA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlphaW: Pointer;

function IsCharAlphaW;
begin
  GetProcedureAddress(_IsCharAlphaW, user32, 'IsCharAlphaW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlphaW]
  end;
end;
{$ELSE}
function IsCharAlphaW; external user32 name 'IsCharAlphaW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlpha: Pointer;

function IsCharAlpha;
begin
  GetProcedureAddress(_IsCharAlpha, user32, 'IsCharAlphaW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlpha]
  end;
end;
{$ELSE}
function IsCharAlpha; external user32 name 'IsCharAlphaW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlpha: Pointer;

function IsCharAlpha;
begin
  GetProcedureAddress(_IsCharAlpha, user32, 'IsCharAlphaA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlpha]
  end;
end;
{$ELSE}
function IsCharAlpha; external user32 name 'IsCharAlphaA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlphaNumericA: Pointer;

function IsCharAlphaNumericA;
begin
  GetProcedureAddress(_IsCharAlphaNumericA, user32, 'IsCharAlphaNumericA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlphaNumericA]
  end;
end;
{$ELSE}
function IsCharAlphaNumericA; external user32 name 'IsCharAlphaNumericA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlphaNumericW: Pointer;

function IsCharAlphaNumericW;
begin
  GetProcedureAddress(_IsCharAlphaNumericW, user32, 'IsCharAlphaNumericW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlphaNumericW]
  end;
end;
{$ELSE}
function IsCharAlphaNumericW; external user32 name 'IsCharAlphaNumericW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlphaNumeric: Pointer;

function IsCharAlphaNumeric;
begin
  GetProcedureAddress(_IsCharAlphaNumeric, user32, 'IsCharAlphaNumericW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlphaNumeric]
  end;
end;
{$ELSE}
function IsCharAlphaNumeric; external user32 name 'IsCharAlphaNumericW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharAlphaNumeric: Pointer;

function IsCharAlphaNumeric;
begin
  GetProcedureAddress(_IsCharAlphaNumeric, user32, 'IsCharAlphaNumericA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharAlphaNumeric]
  end;
end;
{$ELSE}
function IsCharAlphaNumeric; external user32 name 'IsCharAlphaNumericA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharUpperA: Pointer;

function IsCharUpperA;
begin
  GetProcedureAddress(_IsCharUpperA, user32, 'IsCharUpperA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharUpperA]
  end;
end;
{$ELSE}
function IsCharUpperA; external user32 name 'IsCharUpperA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharUpperW: Pointer;

function IsCharUpperW;
begin
  GetProcedureAddress(_IsCharUpperW, user32, 'IsCharUpperW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharUpperW]
  end;
end;
{$ELSE}
function IsCharUpperW; external user32 name 'IsCharUpperW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharUpper: Pointer;

function IsCharUpper;
begin
  GetProcedureAddress(_IsCharUpper, user32, 'IsCharUpperW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharUpper]
  end;
end;
{$ELSE}
function IsCharUpper; external user32 name 'IsCharUpperW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharUpper: Pointer;

function IsCharUpper;
begin
  GetProcedureAddress(_IsCharUpper, user32, 'IsCharUpperA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharUpper]
  end;
end;
{$ELSE}
function IsCharUpper; external user32 name 'IsCharUpperA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharLowerA: Pointer;

function IsCharLowerA;
begin
  GetProcedureAddress(_IsCharLowerA, user32, 'IsCharLowerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharLowerA]
  end;
end;
{$ELSE}
function IsCharLowerA; external user32 name 'IsCharLowerA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharLowerW: Pointer;

function IsCharLowerW;
begin
  GetProcedureAddress(_IsCharLowerW, user32, 'IsCharLowerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharLowerW]
  end;
end;
{$ELSE}
function IsCharLowerW; external user32 name 'IsCharLowerW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharLower: Pointer;

function IsCharLower;
begin
  GetProcedureAddress(_IsCharLower, user32, 'IsCharLowerW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharLower]
  end;
end;
{$ELSE}
function IsCharLower; external user32 name 'IsCharLowerW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _IsCharLower: Pointer;

function IsCharLower;
begin
  GetProcedureAddress(_IsCharLower, user32, 'IsCharLowerA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsCharLower]
  end;
end;
{$ELSE}
function IsCharLower; external user32 name 'IsCharLowerA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetFocus: Pointer;

function SetFocus;
begin
  GetProcedureAddress(_SetFocus, user32, 'SetFocus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetFocus]
  end;
end;
{$ELSE}
function SetFocus; external user32 name 'SetFocus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetActiveWindow: Pointer;

function GetActiveWindow;
begin
  GetProcedureAddress(_GetActiveWindow, user32, 'GetActiveWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetActiveWindow]
  end;
end;
{$ELSE}
function GetActiveWindow; external user32 name 'GetActiveWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetFocus: Pointer;

function GetFocus;
begin
  GetProcedureAddress(_GetFocus, user32, 'GetFocus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetFocus]
  end;
end;
{$ELSE}
function GetFocus; external user32 name 'GetFocus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKBCodePage: Pointer;

function GetKBCodePage;
begin
  GetProcedureAddress(_GetKBCodePage, user32, 'GetKBCodePage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKBCodePage]
  end;
end;
{$ELSE}
function GetKBCodePage; external user32 name 'GetKBCodePage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyState: Pointer;

function GetKeyState;
begin
  GetProcedureAddress(_GetKeyState, user32, 'GetKeyState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyState]
  end;
end;
{$ELSE}
function GetKeyState; external user32 name 'GetKeyState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAsyncKeyState: Pointer;

function GetAsyncKeyState;
begin
  GetProcedureAddress(_GetAsyncKeyState, user32, 'GetAsyncKeyState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAsyncKeyState]
  end;
end;
{$ELSE}
function GetAsyncKeyState; external user32 name 'GetAsyncKeyState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardState: Pointer;

function GetKeyboardState;
begin
  GetProcedureAddress(_GetKeyboardState, user32, 'GetKeyboardState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardState]
  end;
end;
{$ELSE}
function GetKeyboardState; external user32 name 'GetKeyboardState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetKeyboardState: Pointer;

function SetKeyboardState;
begin
  GetProcedureAddress(_SetKeyboardState, user32, 'SetKeyboardState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetKeyboardState]
  end;
end;
{$ELSE}
function SetKeyboardState; external user32 name 'SetKeyboardState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyNameTextA: Pointer;

function GetKeyNameTextA;
begin
  GetProcedureAddress(_GetKeyNameTextA, user32, 'GetKeyNameTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyNameTextA]
  end;
end;
{$ELSE}
function GetKeyNameTextA; external user32 name 'GetKeyNameTextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyNameTextW: Pointer;

function GetKeyNameTextW;
begin
  GetProcedureAddress(_GetKeyNameTextW, user32, 'GetKeyNameTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyNameTextW]
  end;
end;
{$ELSE}
function GetKeyNameTextW; external user32 name 'GetKeyNameTextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyNameText: Pointer;

function GetKeyNameText;
begin
  GetProcedureAddress(_GetKeyNameText, user32, 'GetKeyNameTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyNameText]
  end;
end;
{$ELSE}
function GetKeyNameText; external user32 name 'GetKeyNameTextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyNameText: Pointer;

function GetKeyNameText;
begin
  GetProcedureAddress(_GetKeyNameText, user32, 'GetKeyNameTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyNameText]
  end;
end;
{$ELSE}
function GetKeyNameText; external user32 name 'GetKeyNameTextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetKeyboardType: Pointer;

function GetKeyboardType;
begin
  GetProcedureAddress(_GetKeyboardType, user32, 'GetKeyboardType');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetKeyboardType]
  end;
end;
{$ELSE}
function GetKeyboardType; external user32 name 'GetKeyboardType';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ToAscii: Pointer;

function ToAscii;
begin
  GetProcedureAddress(_ToAscii, user32, 'ToAscii');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ToAscii]
  end;
end;
{$ELSE}
function ToAscii; external user32 name 'ToAscii';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ToAsciiEx: Pointer;

function ToAsciiEx;
begin
  GetProcedureAddress(_ToAsciiEx, user32, 'ToAsciiEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ToAsciiEx]
  end;
end;
{$ELSE}
function ToAsciiEx; external user32 name 'ToAsciiEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ToUnicode: Pointer;

function ToUnicode;
begin
  GetProcedureAddress(_ToUnicode, user32, 'ToUnicode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ToUnicode]
  end;
end;
{$ELSE}
function ToUnicode; external user32 name 'ToUnicode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OemKeyScan: Pointer;

function OemKeyScan;
begin
  GetProcedureAddress(_OemKeyScan, user32, 'OemKeyScan');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OemKeyScan]
  end;
end;
{$ELSE}
function OemKeyScan; external user32 name 'OemKeyScan';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScanA: Pointer;

function VkKeyScanA;
begin
  GetProcedureAddress(_VkKeyScanA, user32, 'VkKeyScanA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScanA]
  end;
end;
{$ELSE}
function VkKeyScanA; external user32 name 'VkKeyScanA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScanW: Pointer;

function VkKeyScanW;
begin
  GetProcedureAddress(_VkKeyScanW, user32, 'VkKeyScanW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScanW]
  end;
end;
{$ELSE}
function VkKeyScanW; external user32 name 'VkKeyScanW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScan: Pointer;

function VkKeyScan;
begin
  GetProcedureAddress(_VkKeyScan, user32, 'VkKeyScanW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScan]
  end;
end;
{$ELSE}
function VkKeyScan; external user32 name 'VkKeyScanW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScan: Pointer;

function VkKeyScan;
begin
  GetProcedureAddress(_VkKeyScan, user32, 'VkKeyScanA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScan]
  end;
end;
{$ELSE}
function VkKeyScan; external user32 name 'VkKeyScanA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScanExA: Pointer;

function VkKeyScanExA;
begin
  GetProcedureAddress(_VkKeyScanExA, user32, 'VkKeyScanExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScanExA]
  end;
end;
{$ELSE}
function VkKeyScanExA; external user32 name 'VkKeyScanExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScanExW: Pointer;

function VkKeyScanExW;
begin
  GetProcedureAddress(_VkKeyScanExW, user32, 'VkKeyScanExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScanExW]
  end;
end;
{$ELSE}
function VkKeyScanExW; external user32 name 'VkKeyScanExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScanEx: Pointer;

function VkKeyScanEx;
begin
  GetProcedureAddress(_VkKeyScanEx, user32, 'VkKeyScanExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScanEx]
  end;
end;
{$ELSE}
function VkKeyScanEx; external user32 name 'VkKeyScanExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _VkKeyScanEx: Pointer;

function VkKeyScanEx;
begin
  GetProcedureAddress(_VkKeyScanEx, user32, 'VkKeyScanExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_VkKeyScanEx]
  end;
end;
{$ELSE}
function VkKeyScanEx; external user32 name 'VkKeyScanExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _keybd_event: Pointer;

procedure keybd_event;
begin
  GetProcedureAddress(_keybd_event, user32, 'keybd_event');
  asm
    mov esp, ebp
    pop ebp
    jmp [_keybd_event]
  end;
end;
{$ELSE}
procedure keybd_event; external user32 name 'keybd_event';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _mouse_event: Pointer;

procedure mouse_event;
begin
  GetProcedureAddress(_mouse_event, user32, 'mouse_event');
  asm
    mov esp, ebp
    pop ebp
    jmp [_mouse_event]
  end;
end;
{$ELSE}
procedure mouse_event; external user32 name 'mouse_event';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SendInput: Pointer;

function SendInput;
begin
  GetProcedureAddress(_SendInput, user32, 'SendInput');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SendInput]
  end;
end;
{$ELSE}
function SendInput; external user32 name 'SendInput';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLastInputInfo: Pointer;

function GetLastInputInfo;
begin
  GetProcedureAddress(_GetLastInputInfo, user32, 'GetLastInputInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLastInputInfo]
  end;
end;
{$ELSE}
function GetLastInputInfo; external user32 name 'GetLastInputInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKeyA: Pointer;

function MapVirtualKeyA;
begin
  GetProcedureAddress(_MapVirtualKeyA, user32, 'MapVirtualKeyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKeyA]
  end;
end;
{$ELSE}
function MapVirtualKeyA; external user32 name 'MapVirtualKeyA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKeyW: Pointer;

function MapVirtualKeyW;
begin
  GetProcedureAddress(_MapVirtualKeyW, user32, 'MapVirtualKeyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKeyW]
  end;
end;
{$ELSE}
function MapVirtualKeyW; external user32 name 'MapVirtualKeyW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKey: Pointer;

function MapVirtualKey;
begin
  GetProcedureAddress(_MapVirtualKey, user32, 'MapVirtualKeyW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKey]
  end;
end;
{$ELSE}
function MapVirtualKey; external user32 name 'MapVirtualKeyW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKey: Pointer;

function MapVirtualKey;
begin
  GetProcedureAddress(_MapVirtualKey, user32, 'MapVirtualKeyA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKey]
  end;
end;
{$ELSE}
function MapVirtualKey; external user32 name 'MapVirtualKeyA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKeyExA: Pointer;

function MapVirtualKeyExA;
begin
  GetProcedureAddress(_MapVirtualKeyExA, user32, 'MapVirtualKeyExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKeyExA]
  end;
end;
{$ELSE}
function MapVirtualKeyExA; external user32 name 'MapVirtualKeyExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKeyExW: Pointer;

function MapVirtualKeyExW;
begin
  GetProcedureAddress(_MapVirtualKeyExW, user32, 'MapVirtualKeyExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKeyExW]
  end;
end;
{$ELSE}
function MapVirtualKeyExW; external user32 name 'MapVirtualKeyExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKeyEx: Pointer;

function MapVirtualKeyEx;
begin
  GetProcedureAddress(_MapVirtualKeyEx, user32, 'MapVirtualKeyExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKeyEx]
  end;
end;
{$ELSE}
function MapVirtualKeyEx; external user32 name 'MapVirtualKeyExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MapVirtualKeyEx: Pointer;

function MapVirtualKeyEx;
begin
  GetProcedureAddress(_MapVirtualKeyEx, user32, 'MapVirtualKeyExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapVirtualKeyEx]
  end;
end;
{$ELSE}
function MapVirtualKeyEx; external user32 name 'MapVirtualKeyExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetInputState: Pointer;

function GetInputState;
begin
  GetProcedureAddress(_GetInputState, user32, 'GetInputState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetInputState]
  end;
end;
{$ELSE}
function GetInputState; external user32 name 'GetInputState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetQueueStatus: Pointer;

function GetQueueStatus;
begin
  GetProcedureAddress(_GetQueueStatus, user32, 'GetQueueStatus');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetQueueStatus]
  end;
end;
{$ELSE}
function GetQueueStatus; external user32 name 'GetQueueStatus';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCapture: Pointer;

function GetCapture;
begin
  GetProcedureAddress(_GetCapture, user32, 'GetCapture');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCapture]
  end;
end;
{$ELSE}
function GetCapture; external user32 name 'GetCapture';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCapture: Pointer;

function SetCapture;
begin
  GetProcedureAddress(_SetCapture, user32, 'SetCapture');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCapture]
  end;
end;
{$ELSE}
function SetCapture; external user32 name 'SetCapture';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReleaseCapture: Pointer;

function ReleaseCapture;
begin
  GetProcedureAddress(_ReleaseCapture, user32, 'ReleaseCapture');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReleaseCapture]
  end;
end;
{$ELSE}
function ReleaseCapture; external user32 name 'ReleaseCapture';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsgWaitForMultipleObjects: Pointer;

function MsgWaitForMultipleObjects;
begin
  GetProcedureAddress(_MsgWaitForMultipleObjects, user32, 'MsgWaitForMultipleObjects');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsgWaitForMultipleObjects]
  end;
end;
{$ELSE}
function MsgWaitForMultipleObjects; external user32 name 'MsgWaitForMultipleObjects';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MsgWaitForMultipleObjectsEx: Pointer;

function MsgWaitForMultipleObjectsEx;
begin
  GetProcedureAddress(_MsgWaitForMultipleObjectsEx, user32, 'MsgWaitForMultipleObjectsEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MsgWaitForMultipleObjectsEx]
  end;
end;
{$ELSE}
function MsgWaitForMultipleObjectsEx; external user32 name 'MsgWaitForMultipleObjectsEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetTimer: Pointer;

function SetTimer;
begin
  GetProcedureAddress(_SetTimer, user32, 'SetTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetTimer]
  end;
end;
{$ELSE}
function SetTimer; external user32 name 'SetTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _KillTimer: Pointer;

function KillTimer;
begin
  GetProcedureAddress(_KillTimer, user32, 'KillTimer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_KillTimer]
  end;
end;
{$ELSE}
function KillTimer; external user32 name 'KillTimer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsWindowUnicode: Pointer;

function IsWindowUnicode;
begin
  GetProcedureAddress(_IsWindowUnicode, user32, 'IsWindowUnicode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWindowUnicode]
  end;
end;
{$ELSE}
function IsWindowUnicode; external user32 name 'IsWindowUnicode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnableWindow: Pointer;

function EnableWindow;
begin
  GetProcedureAddress(_EnableWindow, user32, 'EnableWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnableWindow]
  end;
end;
{$ELSE}
function EnableWindow; external user32 name 'EnableWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsWindowEnabled: Pointer;

function IsWindowEnabled;
begin
  GetProcedureAddress(_IsWindowEnabled, user32, 'IsWindowEnabled');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWindowEnabled]
  end;
end;
{$ELSE}
function IsWindowEnabled; external user32 name 'IsWindowEnabled';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadAcceleratorsA: Pointer;

function LoadAcceleratorsA;
begin
  GetProcedureAddress(_LoadAcceleratorsA, user32, 'LoadAcceleratorsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadAcceleratorsA]
  end;
end;
{$ELSE}
function LoadAcceleratorsA; external user32 name 'LoadAcceleratorsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadAcceleratorsW: Pointer;

function LoadAcceleratorsW;
begin
  GetProcedureAddress(_LoadAcceleratorsW, user32, 'LoadAcceleratorsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadAcceleratorsW]
  end;
end;
{$ELSE}
function LoadAcceleratorsW; external user32 name 'LoadAcceleratorsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadAccelerators: Pointer;

function LoadAccelerators;
begin
  GetProcedureAddress(_LoadAccelerators, user32, 'LoadAcceleratorsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadAccelerators]
  end;
end;
{$ELSE}
function LoadAccelerators; external user32 name 'LoadAcceleratorsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadAccelerators: Pointer;

function LoadAccelerators;
begin
  GetProcedureAddress(_LoadAccelerators, user32, 'LoadAcceleratorsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadAccelerators]
  end;
end;
{$ELSE}
function LoadAccelerators; external user32 name 'LoadAcceleratorsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateAcceleratorTableA: Pointer;

function CreateAcceleratorTableA;
begin
  GetProcedureAddress(_CreateAcceleratorTableA, user32, 'CreateAcceleratorTableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateAcceleratorTableA]
  end;
end;
{$ELSE}
function CreateAcceleratorTableA; external user32 name 'CreateAcceleratorTableA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateAcceleratorTableW: Pointer;

function CreateAcceleratorTableW;
begin
  GetProcedureAddress(_CreateAcceleratorTableW, user32, 'CreateAcceleratorTableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateAcceleratorTableW]
  end;
end;
{$ELSE}
function CreateAcceleratorTableW; external user32 name 'CreateAcceleratorTableW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateAcceleratorTable: Pointer;

function CreateAcceleratorTable;
begin
  GetProcedureAddress(_CreateAcceleratorTable, user32, 'CreateAcceleratorTableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateAcceleratorTable]
  end;
end;
{$ELSE}
function CreateAcceleratorTable; external user32 name 'CreateAcceleratorTableW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateAcceleratorTable: Pointer;

function CreateAcceleratorTable;
begin
  GetProcedureAddress(_CreateAcceleratorTable, user32, 'CreateAcceleratorTableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateAcceleratorTable]
  end;
end;
{$ELSE}
function CreateAcceleratorTable; external user32 name 'CreateAcceleratorTableA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyAcceleratorTable: Pointer;

function DestroyAcceleratorTable;
begin
  GetProcedureAddress(_DestroyAcceleratorTable, user32, 'DestroyAcceleratorTable');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyAcceleratorTable]
  end;
end;
{$ELSE}
function DestroyAcceleratorTable; external user32 name 'DestroyAcceleratorTable';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyAcceleratorTableA: Pointer;

function CopyAcceleratorTableA;
begin
  GetProcedureAddress(_CopyAcceleratorTableA, user32, 'CopyAcceleratorTableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyAcceleratorTableA]
  end;
end;
{$ELSE}
function CopyAcceleratorTableA; external user32 name 'CopyAcceleratorTableA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyAcceleratorTableW: Pointer;

function CopyAcceleratorTableW;
begin
  GetProcedureAddress(_CopyAcceleratorTableW, user32, 'CopyAcceleratorTableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyAcceleratorTableW]
  end;
end;
{$ELSE}
function CopyAcceleratorTableW; external user32 name 'CopyAcceleratorTableW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyAcceleratorTable: Pointer;

function CopyAcceleratorTable;
begin
  GetProcedureAddress(_CopyAcceleratorTable, user32, 'CopyAcceleratorTableW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyAcceleratorTable]
  end;
end;
{$ELSE}
function CopyAcceleratorTable; external user32 name 'CopyAcceleratorTableW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CopyAcceleratorTable: Pointer;

function CopyAcceleratorTable;
begin
  GetProcedureAddress(_CopyAcceleratorTable, user32, 'CopyAcceleratorTableA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyAcceleratorTable]
  end;
end;
{$ELSE}
function CopyAcceleratorTable; external user32 name 'CopyAcceleratorTableA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateAcceleratorA: Pointer;

function TranslateAcceleratorA;
begin
  GetProcedureAddress(_TranslateAcceleratorA, user32, 'TranslateAcceleratorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateAcceleratorA]
  end;
end;
{$ELSE}
function TranslateAcceleratorA; external user32 name 'TranslateAcceleratorA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateAcceleratorW: Pointer;

function TranslateAcceleratorW;
begin
  GetProcedureAddress(_TranslateAcceleratorW, user32, 'TranslateAcceleratorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateAcceleratorW]
  end;
end;
{$ELSE}
function TranslateAcceleratorW; external user32 name 'TranslateAcceleratorW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateAccelerator: Pointer;

function TranslateAccelerator;
begin
  GetProcedureAddress(_TranslateAccelerator, user32, 'TranslateAcceleratorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateAccelerator]
  end;
end;
{$ELSE}
function TranslateAccelerator; external user32 name 'TranslateAcceleratorW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateAccelerator: Pointer;

function TranslateAccelerator;
begin
  GetProcedureAddress(_TranslateAccelerator, user32, 'TranslateAcceleratorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateAccelerator]
  end;
end;
{$ELSE}
function TranslateAccelerator; external user32 name 'TranslateAcceleratorA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemMetrics: Pointer;

function GetSystemMetrics;
begin
  GetProcedureAddress(_GetSystemMetrics, user32, 'GetSystemMetrics');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemMetrics]
  end;
end;
{$ELSE}
function GetSystemMetrics; external user32 name 'GetSystemMetrics';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenuA: Pointer;

function LoadMenuA;
begin
  GetProcedureAddress(_LoadMenuA, user32, 'LoadMenuAA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenuA]
  end;
end;
{$ELSE}
function LoadMenuA; external user32 name 'LoadMenuAA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenuW: Pointer;

function LoadMenuW;
begin
  GetProcedureAddress(_LoadMenuW, user32, 'LoadMenuWA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenuW]
  end;
end;
{$ELSE}
function LoadMenuW; external user32 name 'LoadMenuWA';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenu: Pointer;

function LoadMenu;
begin
  GetProcedureAddress(_LoadMenu, user32, 'LoadMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenu]
  end;
end;
{$ELSE}
function LoadMenu; external user32 name 'LoadMenuW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenu: Pointer;

function LoadMenu;
begin
  GetProcedureAddress(_LoadMenu, user32, 'LoadMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenu]
  end;
end;
{$ELSE}
function LoadMenu; external user32 name 'LoadMenuA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenuIndirectA: Pointer;

function LoadMenuIndirectA;
begin
  GetProcedureAddress(_LoadMenuIndirectA, user32, 'LoadMenuIndirectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenuIndirectA]
  end;
end;
{$ELSE}
function LoadMenuIndirectA; external user32 name 'LoadMenuIndirectA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenuIndirectW: Pointer;

function LoadMenuIndirectW;
begin
  GetProcedureAddress(_LoadMenuIndirectW, user32, 'LoadMenuIndirectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenuIndirectW]
  end;
end;
{$ELSE}
function LoadMenuIndirectW; external user32 name 'LoadMenuIndirectW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenuIndirect: Pointer;

function LoadMenuIndirect;
begin
  GetProcedureAddress(_LoadMenuIndirect, user32, 'LoadMenuIndirectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenuIndirect]
  end;
end;
{$ELSE}
function LoadMenuIndirect; external user32 name 'LoadMenuIndirectW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadMenuIndirect: Pointer;

function LoadMenuIndirect;
begin
  GetProcedureAddress(_LoadMenuIndirect, user32, 'LoadMenuIndirectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadMenuIndirect]
  end;
end;
{$ELSE}
function LoadMenuIndirect; external user32 name 'LoadMenuIndirectA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenu: Pointer;

function GetMenu;
begin
  GetProcedureAddress(_GetMenu, user32, 'GetMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenu]
  end;
end;
{$ELSE}
function GetMenu; external user32 name 'GetMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenu: Pointer;

function SetMenu;
begin
  GetProcedureAddress(_SetMenu, user32, 'SetMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenu]
  end;
end;
{$ELSE}
function SetMenu; external user32 name 'SetMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeMenuA: Pointer;

function ChangeMenuA;
begin
  GetProcedureAddress(_ChangeMenuA, user32, 'ChangeMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeMenuA]
  end;
end;
{$ELSE}
function ChangeMenuA; external user32 name 'ChangeMenuA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeMenuW: Pointer;

function ChangeMenuW;
begin
  GetProcedureAddress(_ChangeMenuW, user32, 'ChangeMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeMenuW]
  end;
end;
{$ELSE}
function ChangeMenuW; external user32 name 'ChangeMenuW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeMenu: Pointer;

function ChangeMenu;
begin
  GetProcedureAddress(_ChangeMenu, user32, 'ChangeMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeMenu]
  end;
end;
{$ELSE}
function ChangeMenu; external user32 name 'ChangeMenuW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeMenu: Pointer;

function ChangeMenu;
begin
  GetProcedureAddress(_ChangeMenu, user32, 'ChangeMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeMenu]
  end;
end;
{$ELSE}
function ChangeMenu; external user32 name 'ChangeMenuA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _HiliteMenuItem: Pointer;

function HiliteMenuItem;
begin
  GetProcedureAddress(_HiliteMenuItem, user32, 'HiliteMenuItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HiliteMenuItem]
  end;
end;
{$ELSE}
function HiliteMenuItem; external user32 name 'HiliteMenuItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuStringA: Pointer;

function GetMenuStringA;
begin
  GetProcedureAddress(_GetMenuStringA, user32, 'GetMenuStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuStringA]
  end;
end;
{$ELSE}
function GetMenuStringA; external user32 name 'GetMenuStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuStringW: Pointer;

function GetMenuStringW;
begin
  GetProcedureAddress(_GetMenuStringW, user32, 'GetMenuStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuStringW]
  end;
end;
{$ELSE}
function GetMenuStringW; external user32 name 'GetMenuStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuString: Pointer;

function GetMenuString;
begin
  GetProcedureAddress(_GetMenuString, user32, 'GetMenuStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuString]
  end;
end;
{$ELSE}
function GetMenuString; external user32 name 'GetMenuStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuString: Pointer;

function GetMenuString;
begin
  GetProcedureAddress(_GetMenuString, user32, 'GetMenuStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuString]
  end;
end;
{$ELSE}
function GetMenuString; external user32 name 'GetMenuStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuState: Pointer;

function GetMenuState;
begin
  GetProcedureAddress(_GetMenuState, user32, 'GetMenuState');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuState]
  end;
end;
{$ELSE}
function GetMenuState; external user32 name 'GetMenuState';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawMenuBar: Pointer;

function DrawMenuBar;
begin
  GetProcedureAddress(_DrawMenuBar, user32, 'DrawMenuBar');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawMenuBar]
  end;
end;
{$ELSE}
function DrawMenuBar; external user32 name 'DrawMenuBar';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSystemMenu: Pointer;

function GetSystemMenu;
begin
  GetProcedureAddress(_GetSystemMenu, user32, 'GetSystemMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSystemMenu]
  end;
end;
{$ELSE}
function GetSystemMenu; external user32 name 'GetSystemMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMenu: Pointer;

function CreateMenu;
begin
  GetProcedureAddress(_CreateMenu, user32, 'CreateMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMenu]
  end;
end;
{$ELSE}
function CreateMenu; external user32 name 'CreateMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreatePopupMenu: Pointer;

function CreatePopupMenu;
begin
  GetProcedureAddress(_CreatePopupMenu, user32, 'CreatePopupMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreatePopupMenu]
  end;
end;
{$ELSE}
function CreatePopupMenu; external user32 name 'CreatePopupMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyMenu: Pointer;

function DestroyMenu;
begin
  GetProcedureAddress(_DestroyMenu, user32, 'DestroyMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyMenu]
  end;
end;
{$ELSE}
function DestroyMenu; external user32 name 'DestroyMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CheckMenuItem: Pointer;

function CheckMenuItem;
begin
  GetProcedureAddress(_CheckMenuItem, user32, 'CheckMenuItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckMenuItem]
  end;
end;
{$ELSE}
function CheckMenuItem; external user32 name 'CheckMenuItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnableMenuItem: Pointer;

function EnableMenuItem;
begin
  GetProcedureAddress(_EnableMenuItem, user32, 'EnableMenuItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnableMenuItem]
  end;
end;
{$ELSE}
function EnableMenuItem; external user32 name 'EnableMenuItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSubMenu: Pointer;

function GetSubMenu;
begin
  GetProcedureAddress(_GetSubMenu, user32, 'GetSubMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSubMenu]
  end;
end;
{$ELSE}
function GetSubMenu; external user32 name 'GetSubMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemID: Pointer;

function GetMenuItemID;
begin
  GetProcedureAddress(_GetMenuItemID, user32, 'GetMenuItemID');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemID]
  end;
end;
{$ELSE}
function GetMenuItemID; external user32 name 'GetMenuItemID';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemCount: Pointer;

function GetMenuItemCount;
begin
  GetProcedureAddress(_GetMenuItemCount, user32, 'GetMenuItemCount');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemCount]
  end;
end;
{$ELSE}
function GetMenuItemCount; external user32 name 'GetMenuItemCount';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenuA: Pointer;

function InsertMenuA;
begin
  GetProcedureAddress(_InsertMenuA, user32, 'InsertMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenuA]
  end;
end;
{$ELSE}
function InsertMenuA; external user32 name 'InsertMenuA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenuW: Pointer;

function InsertMenuW;
begin
  GetProcedureAddress(_InsertMenuW, user32, 'InsertMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenuW]
  end;
end;
{$ELSE}
function InsertMenuW; external user32 name 'InsertMenuW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenu: Pointer;

function InsertMenu;
begin
  GetProcedureAddress(_InsertMenu, user32, 'InsertMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenu]
  end;
end;
{$ELSE}
function InsertMenu; external user32 name 'InsertMenuW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenu: Pointer;

function InsertMenu;
begin
  GetProcedureAddress(_InsertMenu, user32, 'InsertMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenu]
  end;
end;
{$ELSE}
function InsertMenu; external user32 name 'InsertMenuA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _AppendMenuA: Pointer;

function AppendMenuA;
begin
  GetProcedureAddress(_AppendMenuA, user32, 'AppendMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AppendMenuA]
  end;
end;
{$ELSE}
function AppendMenuA; external user32 name 'AppendMenuA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AppendMenuW: Pointer;

function AppendMenuW;
begin
  GetProcedureAddress(_AppendMenuW, user32, 'AppendMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AppendMenuW]
  end;
end;
{$ELSE}
function AppendMenuW; external user32 name 'AppendMenuW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AppendMenu: Pointer;

function AppendMenu;
begin
  GetProcedureAddress(_AppendMenu, user32, 'AppendMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AppendMenu]
  end;
end;
{$ELSE}
function AppendMenu; external user32 name 'AppendMenuW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AppendMenu: Pointer;

function AppendMenu;
begin
  GetProcedureAddress(_AppendMenu, user32, 'AppendMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AppendMenu]
  end;
end;
{$ELSE}
function AppendMenu; external user32 name 'AppendMenuA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ModifyMenuA: Pointer;

function ModifyMenuA;
begin
  GetProcedureAddress(_ModifyMenuA, user32, 'ModifyMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ModifyMenuA]
  end;
end;
{$ELSE}
function ModifyMenuA; external user32 name 'ModifyMenuA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ModifyMenuW: Pointer;

function ModifyMenuW;
begin
  GetProcedureAddress(_ModifyMenuW, user32, 'ModifyMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ModifyMenuW]
  end;
end;
{$ELSE}
function ModifyMenuW; external user32 name 'ModifyMenuW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ModifyMenu: Pointer;

function ModifyMenu;
begin
  GetProcedureAddress(_ModifyMenu, user32, 'ModifyMenuW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ModifyMenu]
  end;
end;
{$ELSE}
function ModifyMenu; external user32 name 'ModifyMenuW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ModifyMenu: Pointer;

function ModifyMenu;
begin
  GetProcedureAddress(_ModifyMenu, user32, 'ModifyMenuA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ModifyMenu]
  end;
end;
{$ELSE}
function ModifyMenu; external user32 name 'ModifyMenuA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveMenu: Pointer;

function RemoveMenu;
begin
  GetProcedureAddress(_RemoveMenu, user32, 'RemoveMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveMenu]
  end;
end;
{$ELSE}
function RemoveMenu; external user32 name 'RemoveMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeleteMenu: Pointer;

function DeleteMenu;
begin
  GetProcedureAddress(_DeleteMenu, user32, 'DeleteMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeleteMenu]
  end;
end;
{$ELSE}
function DeleteMenu; external user32 name 'DeleteMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuItemBitmaps: Pointer;

function SetMenuItemBitmaps;
begin
  GetProcedureAddress(_SetMenuItemBitmaps, user32, 'SetMenuItemBitmaps');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuItemBitmaps]
  end;
end;
{$ELSE}
function SetMenuItemBitmaps; external user32 name 'SetMenuItemBitmaps';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuCheckMarkDimensions: Pointer;

function GetMenuCheckMarkDimensions;
begin
  GetProcedureAddress(_GetMenuCheckMarkDimensions, user32, 'GetMenuCheckMarkDimensions');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuCheckMarkDimensions]
  end;
end;
{$ELSE}
function GetMenuCheckMarkDimensions; external user32 name 'GetMenuCheckMarkDimensions';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TrackPopupMenu: Pointer;

function TrackPopupMenu;
begin
  GetProcedureAddress(_TrackPopupMenu, user32, 'TrackPopupMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TrackPopupMenu]
  end;
end;
{$ELSE}
function TrackPopupMenu; external user32 name 'TrackPopupMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TrackPopupMenuEx: Pointer;

function TrackPopupMenuEx;
begin
  GetProcedureAddress(_TrackPopupMenuEx, user32, 'TrackPopupMenuEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TrackPopupMenuEx]
  end;
end;
{$ELSE}
function TrackPopupMenuEx; external user32 name 'TrackPopupMenuEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuInfo: Pointer;

function GetMenuInfo;
begin
  GetProcedureAddress(_GetMenuInfo, user32, 'GetMenuInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuInfo]
  end;
end;
{$ELSE}
function GetMenuInfo; external user32 name 'GetMenuInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuInfo: Pointer;

function SetMenuInfo;
begin
  GetProcedureAddress(_SetMenuInfo, user32, 'SetMenuInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuInfo]
  end;
end;
{$ELSE}
function SetMenuInfo; external user32 name 'SetMenuInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndMenu: Pointer;

function EndMenu;
begin
  GetProcedureAddress(_EndMenu, user32, 'EndMenu');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndMenu]
  end;
end;
{$ELSE}
function EndMenu; external user32 name 'EndMenu';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenuItemA: Pointer;

function InsertMenuItemA;
begin
  GetProcedureAddress(_InsertMenuItemA, user32, 'InsertMenuItemA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenuItemA]
  end;
end;
{$ELSE}
function InsertMenuItemA; external user32 name 'InsertMenuItemA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenuItemW: Pointer;

function InsertMenuItemW;
begin
  GetProcedureAddress(_InsertMenuItemW, user32, 'InsertMenuItemW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenuItemW]
  end;
end;
{$ELSE}
function InsertMenuItemW; external user32 name 'InsertMenuItemW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenuItem: Pointer;

function InsertMenuItem;
begin
  GetProcedureAddress(_InsertMenuItem, user32, 'InsertMenuItemW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenuItem]
  end;
end;
{$ELSE}
function InsertMenuItem; external user32 name 'InsertMenuItemW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _InsertMenuItem: Pointer;

function InsertMenuItem;
begin
  GetProcedureAddress(_InsertMenuItem, user32, 'InsertMenuItemA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InsertMenuItem]
  end;
end;
{$ELSE}
function InsertMenuItem; external user32 name 'InsertMenuItemA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemInfoA: Pointer;

function GetMenuItemInfoA;
begin
  GetProcedureAddress(_GetMenuItemInfoA, user32, 'GetMenuItemInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemInfoA]
  end;
end;
{$ELSE}
function GetMenuItemInfoA; external user32 name 'GetMenuItemInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemInfoW: Pointer;

function GetMenuItemInfoW;
begin
  GetProcedureAddress(_GetMenuItemInfoW, user32, 'GetMenuItemInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemInfoW]
  end;
end;
{$ELSE}
function GetMenuItemInfoW; external user32 name 'GetMenuItemInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemInfo: Pointer;

function GetMenuItemInfo;
begin
  GetProcedureAddress(_GetMenuItemInfo, user32, 'GetMenuItemInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemInfo]
  end;
end;
{$ELSE}
function GetMenuItemInfo; external user32 name 'GetMenuItemInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemInfo: Pointer;

function GetMenuItemInfo;
begin
  GetProcedureAddress(_GetMenuItemInfo, user32, 'GetMenuItemInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemInfo]
  end;
end;
{$ELSE}
function GetMenuItemInfo; external user32 name 'GetMenuItemInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuItemInfoA: Pointer;

function SetMenuItemInfoA;
begin
  GetProcedureAddress(_SetMenuItemInfoA, user32, 'SetMenuItemInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuItemInfoA]
  end;
end;
{$ELSE}
function SetMenuItemInfoA; external user32 name 'SetMenuItemInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuItemInfoW: Pointer;

function SetMenuItemInfoW;
begin
  GetProcedureAddress(_SetMenuItemInfoW, user32, 'SetMenuItemInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuItemInfoW]
  end;
end;
{$ELSE}
function SetMenuItemInfoW; external user32 name 'SetMenuItemInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuItemInfo: Pointer;

function SetMenuItemInfo;
begin
  GetProcedureAddress(_SetMenuItemInfo, user32, 'SetMenuItemInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuItemInfo]
  end;
end;
{$ELSE}
function SetMenuItemInfo; external user32 name 'SetMenuItemInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuItemInfo: Pointer;

function SetMenuItemInfo;
begin
  GetProcedureAddress(_SetMenuItemInfo, user32, 'SetMenuItemInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuItemInfo]
  end;
end;
{$ELSE}
function SetMenuItemInfo; external user32 name 'SetMenuItemInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuDefaultItem: Pointer;

function GetMenuDefaultItem;
begin
  GetProcedureAddress(_GetMenuDefaultItem, user32, 'GetMenuDefaultItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuDefaultItem]
  end;
end;
{$ELSE}
function GetMenuDefaultItem; external user32 name 'GetMenuDefaultItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuDefaultItem: Pointer;

function SetMenuDefaultItem;
begin
  GetProcedureAddress(_SetMenuDefaultItem, user32, 'SetMenuDefaultItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuDefaultItem]
  end;
end;
{$ELSE}
function SetMenuDefaultItem; external user32 name 'SetMenuDefaultItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuItemRect: Pointer;

function GetMenuItemRect;
begin
  GetProcedureAddress(_GetMenuItemRect, user32, 'GetMenuItemRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuItemRect]
  end;
end;
{$ELSE}
function GetMenuItemRect; external user32 name 'GetMenuItemRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MenuItemFromPoint: Pointer;

function MenuItemFromPoint;
begin
  GetProcedureAddress(_MenuItemFromPoint, user32, 'MenuItemFromPoint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MenuItemFromPoint]
  end;
end;
{$ELSE}
function MenuItemFromPoint; external user32 name 'MenuItemFromPoint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DragObject: Pointer;

function DragObject;
begin
  GetProcedureAddress(_DragObject, user32, 'DragObject');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DragObject]
  end;
end;
{$ELSE}
function DragObject; external user32 name 'DragObject';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DragDetect: Pointer;

function DragDetect;
begin
  GetProcedureAddress(_DragDetect, user32, 'DragDetect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DragDetect]
  end;
end;
{$ELSE}
function DragDetect; external user32 name 'DragDetect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawIcon: Pointer;

function DrawIcon;
begin
  GetProcedureAddress(_DrawIcon, user32, 'DrawIcon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawIcon]
  end;
end;
{$ELSE}
function DrawIcon; external user32 name 'DrawIcon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawTextA: Pointer;

function DrawTextA;
begin
  GetProcedureAddress(_DrawTextA, user32, 'DrawTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawTextA]
  end;
end;
{$ELSE}
function DrawTextA; external user32 name 'DrawTextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawTextW: Pointer;

function DrawTextW;
begin
  GetProcedureAddress(_DrawTextW, user32, 'DrawTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawTextW]
  end;
end;
{$ELSE}
function DrawTextW; external user32 name 'DrawTextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DrawText: Pointer;

function DrawText;
begin
  GetProcedureAddress(_DrawText, user32, 'DrawTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawText]
  end;
end;
{$ELSE}
function DrawText; external user32 name 'DrawTextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DrawText: Pointer;

function DrawText;
begin
  GetProcedureAddress(_DrawText, user32, 'DrawTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawText]
  end;
end;
{$ELSE}
function DrawText; external user32 name 'DrawTextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DrawTextExA: Pointer;

function DrawTextExA;
begin
  GetProcedureAddress(_DrawTextExA, user32, 'DrawTextExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawTextExA]
  end;
end;
{$ELSE}
function DrawTextExA; external user32 name 'DrawTextExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawTextExW: Pointer;

function DrawTextExW;
begin
  GetProcedureAddress(_DrawTextExW, user32, 'DrawTextExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawTextExW]
  end;
end;
{$ELSE}
function DrawTextExW; external user32 name 'DrawTextExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DrawTextEx: Pointer;

function DrawTextEx;
begin
  GetProcedureAddress(_DrawTextEx, user32, 'DrawTextExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawTextEx]
  end;
end;
{$ELSE}
function DrawTextEx; external user32 name 'DrawTextExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DrawTextEx: Pointer;

function DrawTextEx;
begin
  GetProcedureAddress(_DrawTextEx, user32, 'DrawTextExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawTextEx]
  end;
end;
{$ELSE}
function DrawTextEx; external user32 name 'DrawTextExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GrayStringA: Pointer;

function GrayStringA;
begin
  GetProcedureAddress(_GrayStringA, user32, 'GrayStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GrayStringA]
  end;
end;
{$ELSE}
function GrayStringA; external user32 name 'GrayStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GrayStringW: Pointer;

function GrayStringW;
begin
  GetProcedureAddress(_GrayStringW, user32, 'GrayStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GrayStringW]
  end;
end;
{$ELSE}
function GrayStringW; external user32 name 'GrayStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GrayString: Pointer;

function GrayString;
begin
  GetProcedureAddress(_GrayString, user32, 'GrayStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GrayString]
  end;
end;
{$ELSE}
function GrayString; external user32 name 'GrayStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GrayString: Pointer;

function GrayString;
begin
  GetProcedureAddress(_GrayString, user32, 'GrayStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GrayString]
  end;
end;
{$ELSE}
function GrayString; external user32 name 'GrayStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DrawStateA: Pointer;

function DrawStateA;
begin
  GetProcedureAddress(_DrawStateA, user32, 'DrawStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawStateA]
  end;
end;
{$ELSE}
function DrawStateA; external user32 name 'DrawStateA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawStateW: Pointer;

function DrawStateW;
begin
  GetProcedureAddress(_DrawStateW, user32, 'DrawStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawStateW]
  end;
end;
{$ELSE}
function DrawStateW; external user32 name 'DrawStateW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DrawState: Pointer;

function DrawState;
begin
  GetProcedureAddress(_DrawState, user32, 'DrawStateW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawState]
  end;
end;
{$ELSE}
function DrawState; external user32 name 'DrawStateW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DrawState: Pointer;

function DrawState;
begin
  GetProcedureAddress(_DrawState, user32, 'DrawStateA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawState]
  end;
end;
{$ELSE}
function DrawState; external user32 name 'DrawStateA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _TabbedTextOutA: Pointer;

function TabbedTextOutA;
begin
  GetProcedureAddress(_TabbedTextOutA, user32, 'TabbedTextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TabbedTextOutA]
  end;
end;
{$ELSE}
function TabbedTextOutA; external user32 name 'TabbedTextOutA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _TabbedTextOutW: Pointer;

function TabbedTextOutW;
begin
  GetProcedureAddress(_TabbedTextOutW, user32, 'TabbedTextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TabbedTextOutW]
  end;
end;
{$ELSE}
function TabbedTextOutW; external user32 name 'TabbedTextOutW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _TabbedTextOut: Pointer;

function TabbedTextOut;
begin
  GetProcedureAddress(_TabbedTextOut, user32, 'TabbedTextOutW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TabbedTextOut]
  end;
end;
{$ELSE}
function TabbedTextOut; external user32 name 'TabbedTextOutW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _TabbedTextOut: Pointer;

function TabbedTextOut;
begin
  GetProcedureAddress(_TabbedTextOut, user32, 'TabbedTextOutA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TabbedTextOut]
  end;
end;
{$ELSE}
function TabbedTextOut; external user32 name 'TabbedTextOutA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTabbedTextExtentA: Pointer;

function GetTabbedTextExtentA;
begin
  GetProcedureAddress(_GetTabbedTextExtentA, user32, 'GetTabbedTextExtentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTabbedTextExtentA]
  end;
end;
{$ELSE}
function GetTabbedTextExtentA; external user32 name 'GetTabbedTextExtentA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTabbedTextExtentW: Pointer;

function GetTabbedTextExtentW;
begin
  GetProcedureAddress(_GetTabbedTextExtentW, user32, 'GetTabbedTextExtentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTabbedTextExtentW]
  end;
end;
{$ELSE}
function GetTabbedTextExtentW; external user32 name 'GetTabbedTextExtentW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTabbedTextExtent: Pointer;

function GetTabbedTextExtent;
begin
  GetProcedureAddress(_GetTabbedTextExtent, user32, 'GetTabbedTextExtentW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTabbedTextExtent]
  end;
end;
{$ELSE}
function GetTabbedTextExtent; external user32 name 'GetTabbedTextExtentW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetTabbedTextExtent: Pointer;

function GetTabbedTextExtent;
begin
  GetProcedureAddress(_GetTabbedTextExtent, user32, 'GetTabbedTextExtentA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTabbedTextExtent]
  end;
end;
{$ELSE}
function GetTabbedTextExtent; external user32 name 'GetTabbedTextExtentA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UpdateWindow: Pointer;

function UpdateWindow;
begin
  GetProcedureAddress(_UpdateWindow, user32, 'UpdateWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UpdateWindow]
  end;
end;
{$ELSE}
function UpdateWindow; external user32 name 'UpdateWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetActiveWindow: Pointer;

function SetActiveWindow;
begin
  GetProcedureAddress(_SetActiveWindow, user32, 'SetActiveWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetActiveWindow]
  end;
end;
{$ELSE}
function SetActiveWindow; external user32 name 'SetActiveWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetForegroundWindow: Pointer;

function GetForegroundWindow;
begin
  GetProcedureAddress(_GetForegroundWindow, user32, 'GetForegroundWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetForegroundWindow]
  end;
end;
{$ELSE}
function GetForegroundWindow; external user32 name 'GetForegroundWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PaintDesktop: Pointer;

function PaintDesktop;
begin
  GetProcedureAddress(_PaintDesktop, user32, 'PaintDesktop');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PaintDesktop]
  end;
end;
{$ELSE}
function PaintDesktop; external user32 name 'PaintDesktop';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SwitchToThisWindow: Pointer;

procedure SwitchToThisWindow;
begin
  GetProcedureAddress(_SwitchToThisWindow, user32, 'SwitchToThisWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SwitchToThisWindow]
  end;
end;
{$ELSE}
procedure SwitchToThisWindow; external user32 name 'SwitchToThisWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetForegroundWindow: Pointer;

function SetForegroundWindow;
begin
  GetProcedureAddress(_SetForegroundWindow, user32, 'SetForegroundWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetForegroundWindow]
  end;
end;
{$ELSE}
function SetForegroundWindow; external user32 name 'SetForegroundWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AllowSetForegroundWindow: Pointer;

function AllowSetForegroundWindow;
begin
  GetProcedureAddress(_AllowSetForegroundWindow, user32, 'AllowSetForegroundWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllowSetForegroundWindow]
  end;
end;
{$ELSE}
function AllowSetForegroundWindow; external user32 name 'AllowSetForegroundWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LockSetForegroundWindow: Pointer;

function LockSetForegroundWindow;
begin
  GetProcedureAddress(_LockSetForegroundWindow, user32, 'LockSetForegroundWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LockSetForegroundWindow]
  end;
end;
{$ELSE}
function LockSetForegroundWindow; external user32 name 'LockSetForegroundWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WindowFromDC: Pointer;

function WindowFromDC;
begin
  GetProcedureAddress(_WindowFromDC, user32, 'WindowFromDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WindowFromDC]
  end;
end;
{$ELSE}
function WindowFromDC; external user32 name 'WindowFromDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDC: Pointer;

function GetDC;
begin
  GetProcedureAddress(_GetDC, user32, 'GetDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDC]
  end;
end;
{$ELSE}
function GetDC; external user32 name 'GetDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDCEx: Pointer;

function GetDCEx;
begin
  GetProcedureAddress(_GetDCEx, user32, 'GetDCEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDCEx]
  end;
end;
{$ELSE}
function GetDCEx; external user32 name 'GetDCEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowDC: Pointer;

function GetWindowDC;
begin
  GetProcedureAddress(_GetWindowDC, user32, 'GetWindowDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowDC]
  end;
end;
{$ELSE}
function GetWindowDC; external user32 name 'GetWindowDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReleaseDC: Pointer;

function ReleaseDC;
begin
  GetProcedureAddress(_ReleaseDC, user32, 'ReleaseDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReleaseDC]
  end;
end;
{$ELSE}
function ReleaseDC; external user32 name 'ReleaseDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _BeginPaint: Pointer;

function BeginPaint;
begin
  GetProcedureAddress(_BeginPaint, user32, 'BeginPaint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_BeginPaint]
  end;
end;
{$ELSE}
function BeginPaint; external user32 name 'BeginPaint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndPaint: Pointer;

function EndPaint;
begin
  GetProcedureAddress(_EndPaint, user32, 'EndPaint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndPaint]
  end;
end;
{$ELSE}
function EndPaint; external user32 name 'EndPaint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUpdateRect: Pointer;

function GetUpdateRect;
begin
  GetProcedureAddress(_GetUpdateRect, user32, 'GetUpdateRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUpdateRect]
  end;
end;
{$ELSE}
function GetUpdateRect; external user32 name 'GetUpdateRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetUpdateRgn: Pointer;

function GetUpdateRgn;
begin
  GetProcedureAddress(_GetUpdateRgn, user32, 'GetUpdateRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetUpdateRgn]
  end;
end;
{$ELSE}
function GetUpdateRgn; external user32 name 'GetUpdateRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowRgn: Pointer;

function SetWindowRgn;
begin
  GetProcedureAddress(_SetWindowRgn, user32, 'SetWindowRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowRgn]
  end;
end;
{$ELSE}
function SetWindowRgn; external user32 name 'SetWindowRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowRgn: Pointer;

function GetWindowRgn;
begin
  GetProcedureAddress(_GetWindowRgn, user32, 'GetWindowRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowRgn]
  end;
end;
{$ELSE}
function GetWindowRgn; external user32 name 'GetWindowRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowRgnBox: Pointer;

function GetWindowRgnBox;
begin
  GetProcedureAddress(_GetWindowRgnBox, user32, 'GetWindowRgnBox');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowRgnBox]
  end;
end;
{$ELSE}
function GetWindowRgnBox; external user32 name 'GetWindowRgnBox';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ExcludeUpdateRgn: Pointer;

function ExcludeUpdateRgn;
begin
  GetProcedureAddress(_ExcludeUpdateRgn, user32, 'ExcludeUpdateRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ExcludeUpdateRgn]
  end;
end;
{$ELSE}
function ExcludeUpdateRgn; external user32 name 'ExcludeUpdateRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InvalidateRect: Pointer;

function InvalidateRect;
begin
  GetProcedureAddress(_InvalidateRect, user32, 'InvalidateRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InvalidateRect]
  end;
end;
{$ELSE}
function InvalidateRect; external user32 name 'InvalidateRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ValidateRect: Pointer;

function ValidateRect;
begin
  GetProcedureAddress(_ValidateRect, user32, 'ValidateRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ValidateRect]
  end;
end;
{$ELSE}
function ValidateRect; external user32 name 'ValidateRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InvalidateRgn: Pointer;

function InvalidateRgn;
begin
  GetProcedureAddress(_InvalidateRgn, user32, 'InvalidateRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InvalidateRgn]
  end;
end;
{$ELSE}
function InvalidateRgn; external user32 name 'InvalidateRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ValidateRgn: Pointer;

function ValidateRgn;
begin
  GetProcedureAddress(_ValidateRgn, user32, 'ValidateRgn');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ValidateRgn]
  end;
end;
{$ELSE}
function ValidateRgn; external user32 name 'ValidateRgn';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RedrawWindow: Pointer;

function RedrawWindow;
begin
  GetProcedureAddress(_RedrawWindow, user32, 'RedrawWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RedrawWindow]
  end;
end;
{$ELSE}
function RedrawWindow; external user32 name 'RedrawWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LockWindowUpdate: Pointer;

function LockWindowUpdate;
begin
  GetProcedureAddress(_LockWindowUpdate, user32, 'LockWindowUpdate');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LockWindowUpdate]
  end;
end;
{$ELSE}
function LockWindowUpdate; external user32 name 'LockWindowUpdate';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScrollWindow: Pointer;

function ScrollWindow;
begin
  GetProcedureAddress(_ScrollWindow, user32, 'ScrollWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollWindow]
  end;
end;
{$ELSE}
function ScrollWindow; external user32 name 'ScrollWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScrollDC: Pointer;

function ScrollDC;
begin
  GetProcedureAddress(_ScrollDC, user32, 'ScrollDC');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollDC]
  end;
end;
{$ELSE}
function ScrollDC; external user32 name 'ScrollDC';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScrollWindowEx: Pointer;

function ScrollWindowEx;
begin
  GetProcedureAddress(_ScrollWindowEx, user32, 'ScrollWindowEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollWindowEx]
  end;
end;
{$ELSE}
function ScrollWindowEx; external user32 name 'ScrollWindowEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetScrollPos: Pointer;

function SetScrollPos;
begin
  GetProcedureAddress(_SetScrollPos, user32, 'SetScrollPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetScrollPos]
  end;
end;
{$ELSE}
function SetScrollPos; external user32 name 'SetScrollPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetScrollPos: Pointer;

function GetScrollPos;
begin
  GetProcedureAddress(_GetScrollPos, user32, 'GetScrollPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetScrollPos]
  end;
end;
{$ELSE}
function GetScrollPos; external user32 name 'GetScrollPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetScrollRange: Pointer;

function SetScrollRange;
begin
  GetProcedureAddress(_SetScrollRange, user32, 'SetScrollRange');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetScrollRange]
  end;
end;
{$ELSE}
function SetScrollRange; external user32 name 'SetScrollRange';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetScrollRange: Pointer;

function GetScrollRange;
begin
  GetProcedureAddress(_GetScrollRange, user32, 'GetScrollRange');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetScrollRange]
  end;
end;
{$ELSE}
function GetScrollRange; external user32 name 'GetScrollRange';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ShowScrollBar: Pointer;

function ShowScrollBar;
begin
  GetProcedureAddress(_ShowScrollBar, user32, 'ShowScrollBar');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ShowScrollBar]
  end;
end;
{$ELSE}
function ShowScrollBar; external user32 name 'ShowScrollBar';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnableScrollBar: Pointer;

function EnableScrollBar;
begin
  GetProcedureAddress(_EnableScrollBar, user32, 'EnableScrollBar');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnableScrollBar]
  end;
end;
{$ELSE}
function EnableScrollBar; external user32 name 'EnableScrollBar';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPropA: Pointer;

function SetPropA;
begin
  GetProcedureAddress(_SetPropA, user32, 'SetPropA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPropA]
  end;
end;
{$ELSE}
function SetPropA; external user32 name 'SetPropA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetPropW: Pointer;

function SetPropW;
begin
  GetProcedureAddress(_SetPropW, user32, 'SetPropW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetPropW]
  end;
end;
{$ELSE}
function SetPropW; external user32 name 'SetPropW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetProp: Pointer;

function SetProp;
begin
  GetProcedureAddress(_SetProp, user32, 'SetPropW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProp]
  end;
end;
{$ELSE}
function SetProp; external user32 name 'SetPropW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetProp: Pointer;

function SetProp;
begin
  GetProcedureAddress(_SetProp, user32, 'SetPropA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProp]
  end;
end;
{$ELSE}
function SetProp; external user32 name 'SetPropA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetPropA: Pointer;

function GetPropA;
begin
  GetProcedureAddress(_GetPropA, user32, 'GetPropA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPropA]
  end;
end;
{$ELSE}
function GetPropA; external user32 name 'GetPropA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetPropW: Pointer;

function GetPropW;
begin
  GetProcedureAddress(_GetPropW, user32, 'GetPropW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetPropW]
  end;
end;
{$ELSE}
function GetPropW; external user32 name 'GetPropW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProp: Pointer;

function GetProp;
begin
  GetProcedureAddress(_GetProp, user32, 'GetPropW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProp]
  end;
end;
{$ELSE}
function GetProp; external user32 name 'GetPropW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetProp: Pointer;

function GetProp;
begin
  GetProcedureAddress(_GetProp, user32, 'GetPropA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProp]
  end;
end;
{$ELSE}
function GetProp; external user32 name 'GetPropA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _RemovePropA: Pointer;

function RemovePropA;
begin
  GetProcedureAddress(_RemovePropA, user32, 'RemovePropA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemovePropA]
  end;
end;
{$ELSE}
function RemovePropA; external user32 name 'RemovePropA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RemovePropW: Pointer;

function RemovePropW;
begin
  GetProcedureAddress(_RemovePropW, user32, 'RemovePropW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemovePropW]
  end;
end;
{$ELSE}
function RemovePropW; external user32 name 'RemovePropW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveProp: Pointer;

function RemoveProp;
begin
  GetProcedureAddress(_RemoveProp, user32, 'RemovePropW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveProp]
  end;
end;
{$ELSE}
function RemoveProp; external user32 name 'RemovePropW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RemoveProp: Pointer;

function RemoveProp;
begin
  GetProcedureAddress(_RemoveProp, user32, 'RemovePropA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RemoveProp]
  end;
end;
{$ELSE}
function RemoveProp; external user32 name 'RemovePropA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumPropsExA: Pointer;

function EnumPropsExA;
begin
  GetProcedureAddress(_EnumPropsExA, user32, 'EnumPropsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPropsExA]
  end;
end;
{$ELSE}
function EnumPropsExA; external user32 name 'EnumPropsExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumPropsExW: Pointer;

function EnumPropsExW;
begin
  GetProcedureAddress(_EnumPropsExW, user32, 'EnumPropsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPropsExW]
  end;
end;
{$ELSE}
function EnumPropsExW; external user32 name 'EnumPropsExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumPropsEx: Pointer;

function EnumPropsEx;
begin
  GetProcedureAddress(_EnumPropsEx, user32, 'EnumPropsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPropsEx]
  end;
end;
{$ELSE}
function EnumPropsEx; external user32 name 'EnumPropsExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumPropsEx: Pointer;

function EnumPropsEx;
begin
  GetProcedureAddress(_EnumPropsEx, user32, 'EnumPropsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPropsEx]
  end;
end;
{$ELSE}
function EnumPropsEx; external user32 name 'EnumPropsExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumPropsA: Pointer;

function EnumPropsA;
begin
  GetProcedureAddress(_EnumPropsA, user32, 'EnumPropsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPropsA]
  end;
end;
{$ELSE}
function EnumPropsA; external user32 name 'EnumPropsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumPropsW: Pointer;

function EnumPropsW;
begin
  GetProcedureAddress(_EnumPropsW, user32, 'EnumPropsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumPropsW]
  end;
end;
{$ELSE}
function EnumPropsW; external user32 name 'EnumPropsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumProps: Pointer;

function EnumProps;
begin
  GetProcedureAddress(_EnumProps, user32, 'EnumPropsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumProps]
  end;
end;
{$ELSE}
function EnumProps; external user32 name 'EnumPropsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumProps: Pointer;

function EnumProps;
begin
  GetProcedureAddress(_EnumProps, user32, 'EnumPropsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumProps]
  end;
end;
{$ELSE}
function EnumProps; external user32 name 'EnumPropsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowTextA: Pointer;

function SetWindowTextA;
begin
  GetProcedureAddress(_SetWindowTextA, user32, 'SetWindowTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowTextA]
  end;
end;
{$ELSE}
function SetWindowTextA; external user32 name 'SetWindowTextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowTextW: Pointer;

function SetWindowTextW;
begin
  GetProcedureAddress(_SetWindowTextW, user32, 'SetWindowTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowTextW]
  end;
end;
{$ELSE}
function SetWindowTextW; external user32 name 'SetWindowTextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowText: Pointer;

function SetWindowText;
begin
  GetProcedureAddress(_SetWindowText, user32, 'SetWindowTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowText]
  end;
end;
{$ELSE}
function SetWindowText; external user32 name 'SetWindowTextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowText: Pointer;

function SetWindowText;
begin
  GetProcedureAddress(_SetWindowText, user32, 'SetWindowTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowText]
  end;
end;
{$ELSE}
function SetWindowText; external user32 name 'SetWindowTextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowTextA: Pointer;

function GetWindowTextA;
begin
  GetProcedureAddress(_GetWindowTextA, user32, 'GetWindowTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowTextA]
  end;
end;
{$ELSE}
function GetWindowTextA; external user32 name 'GetWindowTextA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowTextW: Pointer;

function GetWindowTextW;
begin
  GetProcedureAddress(_GetWindowTextW, user32, 'GetWindowTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowTextW]
  end;
end;
{$ELSE}
function GetWindowTextW; external user32 name 'GetWindowTextW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowText: Pointer;

function GetWindowText;
begin
  GetProcedureAddress(_GetWindowText, user32, 'GetWindowTextW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowText]
  end;
end;
{$ELSE}
function GetWindowText; external user32 name 'GetWindowTextW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowText: Pointer;

function GetWindowText;
begin
  GetProcedureAddress(_GetWindowText, user32, 'GetWindowTextA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowText]
  end;
end;
{$ELSE}
function GetWindowText; external user32 name 'GetWindowTextA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowTextLengthA: Pointer;

function GetWindowTextLengthA;
begin
  GetProcedureAddress(_GetWindowTextLengthA, user32, 'GetWindowTextLengthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowTextLengthA]
  end;
end;
{$ELSE}
function GetWindowTextLengthA; external user32 name 'GetWindowTextLengthA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowTextLengthW: Pointer;

function GetWindowTextLengthW;
begin
  GetProcedureAddress(_GetWindowTextLengthW, user32, 'GetWindowTextLengthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowTextLengthW]
  end;
end;
{$ELSE}
function GetWindowTextLengthW; external user32 name 'GetWindowTextLengthW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowTextLength: Pointer;

function GetWindowTextLength;
begin
  GetProcedureAddress(_GetWindowTextLength, user32, 'GetWindowTextLengthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowTextLength]
  end;
end;
{$ELSE}
function GetWindowTextLength; external user32 name 'GetWindowTextLengthW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowTextLength: Pointer;

function GetWindowTextLength;
begin
  GetProcedureAddress(_GetWindowTextLength, user32, 'GetWindowTextLengthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowTextLength]
  end;
end;
{$ELSE}
function GetWindowTextLength; external user32 name 'GetWindowTextLengthA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetClientRect: Pointer;

function GetClientRect;
begin
  GetProcedureAddress(_GetClientRect, user32, 'GetClientRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClientRect]
  end;
end;
{$ELSE}
function GetClientRect; external user32 name 'GetClientRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowRect: Pointer;

function GetWindowRect;
begin
  GetProcedureAddress(_GetWindowRect, user32, 'GetWindowRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowRect]
  end;
end;
{$ELSE}
function GetWindowRect; external user32 name 'GetWindowRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AdjustWindowRect: Pointer;

function AdjustWindowRect;
begin
  GetProcedureAddress(_AdjustWindowRect, user32, 'AdjustWindowRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdjustWindowRect]
  end;
end;
{$ELSE}
function AdjustWindowRect; external user32 name 'AdjustWindowRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AdjustWindowRectEx: Pointer;

function AdjustWindowRectEx;
begin
  GetProcedureAddress(_AdjustWindowRectEx, user32, 'AdjustWindowRectEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AdjustWindowRectEx]
  end;
end;
{$ELSE}
function AdjustWindowRectEx; external user32 name 'AdjustWindowRectEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowContextHelpId: Pointer;

function SetWindowContextHelpId;
begin
  GetProcedureAddress(_SetWindowContextHelpId, user32, 'SetWindowContextHelpId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowContextHelpId]
  end;
end;
{$ELSE}
function SetWindowContextHelpId; external user32 name 'SetWindowContextHelpId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowContextHelpId: Pointer;

function GetWindowContextHelpId;
begin
  GetProcedureAddress(_GetWindowContextHelpId, user32, 'GetWindowContextHelpId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowContextHelpId]
  end;
end;
{$ELSE}
function GetWindowContextHelpId; external user32 name 'GetWindowContextHelpId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetMenuContextHelpId: Pointer;

function SetMenuContextHelpId;
begin
  GetProcedureAddress(_SetMenuContextHelpId, user32, 'SetMenuContextHelpId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetMenuContextHelpId]
  end;
end;
{$ELSE}
function SetMenuContextHelpId; external user32 name 'SetMenuContextHelpId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuContextHelpId: Pointer;

function GetMenuContextHelpId;
begin
  GetProcedureAddress(_GetMenuContextHelpId, user32, 'GetMenuContextHelpId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuContextHelpId]
  end;
end;
{$ELSE}
function GetMenuContextHelpId; external user32 name 'GetMenuContextHelpId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxA: Pointer;

function MessageBoxA;
begin
  GetProcedureAddress(_MessageBoxA, user32, 'MessageBoxA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxA]
  end;
end;
{$ELSE}
function MessageBoxA; external user32 name 'MessageBoxA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxW: Pointer;

function MessageBoxW;
begin
  GetProcedureAddress(_MessageBoxW, user32, 'MessageBoxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxW]
  end;
end;
{$ELSE}
function MessageBoxW; external user32 name 'MessageBoxW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBox: Pointer;

function MessageBox;
begin
  GetProcedureAddress(_MessageBox, user32, 'MessageBoxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBox]
  end;
end;
{$ELSE}
function MessageBox; external user32 name 'MessageBoxW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBox: Pointer;

function MessageBox;
begin
  GetProcedureAddress(_MessageBox, user32, 'MessageBoxA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBox]
  end;
end;
{$ELSE}
function MessageBox; external user32 name 'MessageBoxA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxExA: Pointer;

function MessageBoxExA;
begin
  GetProcedureAddress(_MessageBoxExA, user32, 'MessageBoxExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxExA]
  end;
end;
{$ELSE}
function MessageBoxExA; external user32 name 'MessageBoxExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxExW: Pointer;

function MessageBoxExW;
begin
  GetProcedureAddress(_MessageBoxExW, user32, 'MessageBoxExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxExW]
  end;
end;
{$ELSE}
function MessageBoxExW; external user32 name 'MessageBoxExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxEx: Pointer;

function MessageBoxEx;
begin
  GetProcedureAddress(_MessageBoxEx, user32, 'MessageBoxExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxEx]
  end;
end;
{$ELSE}
function MessageBoxEx; external user32 name 'MessageBoxExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxEx: Pointer;

function MessageBoxEx;
begin
  GetProcedureAddress(_MessageBoxEx, user32, 'MessageBoxExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxEx]
  end;
end;
{$ELSE}
function MessageBoxEx; external user32 name 'MessageBoxExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxIndirectA: Pointer;

function MessageBoxIndirectA;
begin
  GetProcedureAddress(_MessageBoxIndirectA, user32, 'MessageBoxIndirectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxIndirectA]
  end;
end;
{$ELSE}
function MessageBoxIndirectA; external user32 name 'MessageBoxIndirectA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxIndirectW: Pointer;

function MessageBoxIndirectW;
begin
  GetProcedureAddress(_MessageBoxIndirectW, user32, 'MessageBoxIndirectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxIndirectW]
  end;
end;
{$ELSE}
function MessageBoxIndirectW; external user32 name 'MessageBoxIndirectW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxIndirect: Pointer;

function MessageBoxIndirect;
begin
  GetProcedureAddress(_MessageBoxIndirect, user32, 'MessageBoxIndirectW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxIndirect]
  end;
end;
{$ELSE}
function MessageBoxIndirect; external user32 name 'MessageBoxIndirectW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBoxIndirect: Pointer;

function MessageBoxIndirect;
begin
  GetProcedureAddress(_MessageBoxIndirect, user32, 'MessageBoxIndirectA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBoxIndirect]
  end;
end;
{$ELSE}
function MessageBoxIndirect; external user32 name 'MessageBoxIndirectA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MessageBeep: Pointer;

function MessageBeep;
begin
  GetProcedureAddress(_MessageBeep, user32, 'MessageBeep');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MessageBeep]
  end;
end;
{$ELSE}
function MessageBeep; external user32 name 'MessageBeep';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ShowCursor: Pointer;

function ShowCursor;
begin
  GetProcedureAddress(_ShowCursor, user32, 'ShowCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ShowCursor]
  end;
end;
{$ELSE}
function ShowCursor; external user32 name 'ShowCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCursorPos: Pointer;

function SetCursorPos;
begin
  GetProcedureAddress(_SetCursorPos, user32, 'SetCursorPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCursorPos]
  end;
end;
{$ELSE}
function SetCursorPos; external user32 name 'SetCursorPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCursor: Pointer;

function SetCursor;
begin
  GetProcedureAddress(_SetCursor, user32, 'SetCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCursor]
  end;
end;
{$ELSE}
function SetCursor; external user32 name 'SetCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCursorPos: Pointer;

function GetCursorPos;
begin
  GetProcedureAddress(_GetCursorPos, user32, 'GetCursorPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCursorPos]
  end;
end;
{$ELSE}
function GetCursorPos; external user32 name 'GetCursorPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ClipCursor: Pointer;

function ClipCursor;
begin
  GetProcedureAddress(_ClipCursor, user32, 'ClipCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClipCursor]
  end;
end;
{$ELSE}
function ClipCursor; external user32 name 'ClipCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClipCursor: Pointer;

function GetClipCursor;
begin
  GetProcedureAddress(_GetClipCursor, user32, 'GetClipCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClipCursor]
  end;
end;
{$ELSE}
function GetClipCursor; external user32 name 'GetClipCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCursor: Pointer;

function GetCursor;
begin
  GetProcedureAddress(_GetCursor, user32, 'GetCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCursor]
  end;
end;
{$ELSE}
function GetCursor; external user32 name 'GetCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateCaret: Pointer;

function CreateCaret;
begin
  GetProcedureAddress(_CreateCaret, user32, 'CreateCaret');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateCaret]
  end;
end;
{$ELSE}
function CreateCaret; external user32 name 'CreateCaret';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCaretBlinkTime: Pointer;

function GetCaretBlinkTime;
begin
  GetProcedureAddress(_GetCaretBlinkTime, user32, 'GetCaretBlinkTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCaretBlinkTime]
  end;
end;
{$ELSE}
function GetCaretBlinkTime; external user32 name 'GetCaretBlinkTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCaretBlinkTime: Pointer;

function SetCaretBlinkTime;
begin
  GetProcedureAddress(_SetCaretBlinkTime, user32, 'SetCaretBlinkTime');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCaretBlinkTime]
  end;
end;
{$ELSE}
function SetCaretBlinkTime; external user32 name 'SetCaretBlinkTime';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyCaret: Pointer;

function DestroyCaret;
begin
  GetProcedureAddress(_DestroyCaret, user32, 'DestroyCaret');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyCaret]
  end;
end;
{$ELSE}
function DestroyCaret; external user32 name 'DestroyCaret';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _HideCaret: Pointer;

function HideCaret;
begin
  GetProcedureAddress(_HideCaret, user32, 'HideCaret');
  asm
    mov esp, ebp
    pop ebp
    jmp [_HideCaret]
  end;
end;
{$ELSE}
function HideCaret; external user32 name 'HideCaret';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ShowCaret: Pointer;

function ShowCaret;
begin
  GetProcedureAddress(_ShowCaret, user32, 'ShowCaret');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ShowCaret]
  end;
end;
{$ELSE}
function ShowCaret; external user32 name 'ShowCaret';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetCaretPos: Pointer;

function SetCaretPos;
begin
  GetProcedureAddress(_SetCaretPos, user32, 'SetCaretPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetCaretPos]
  end;
end;
{$ELSE}
function SetCaretPos; external user32 name 'SetCaretPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCaretPos: Pointer;

function GetCaretPos;
begin
  GetProcedureAddress(_GetCaretPos, user32, 'GetCaretPos');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCaretPos]
  end;
end;
{$ELSE}
function GetCaretPos; external user32 name 'GetCaretPos';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ClientToScreen: Pointer;

function ClientToScreen;
begin
  GetProcedureAddress(_ClientToScreen, user32, 'ClientToScreen');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ClientToScreen]
  end;
end;
{$ELSE}
function ClientToScreen; external user32 name 'ClientToScreen';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScreenToClient: Pointer;

function ScreenToClient;
begin
  GetProcedureAddress(_ScreenToClient, user32, 'ScreenToClient');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScreenToClient]
  end;
end;
{$ELSE}
function ScreenToClient; external user32 name 'ScreenToClient';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MapWindowPoints: Pointer;

function MapWindowPoints;
begin
  GetProcedureAddress(_MapWindowPoints, user32, 'MapWindowPoints');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapWindowPoints]
  end;
end;
{$ELSE}
function MapWindowPoints; external user32 name 'MapWindowPoints';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WindowFromPoint: Pointer;

function WindowFromPoint;
begin
  GetProcedureAddress(_WindowFromPoint, user32, 'WindowFromPoint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WindowFromPoint]
  end;
end;
{$ELSE}
function WindowFromPoint; external user32 name 'WindowFromPoint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChildWindowFromPoint: Pointer;

function ChildWindowFromPoint;
begin
  GetProcedureAddress(_ChildWindowFromPoint, user32, 'ChildWindowFromPoint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChildWindowFromPoint]
  end;
end;
{$ELSE}
function ChildWindowFromPoint; external user32 name 'ChildWindowFromPoint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChildWindowFromPointEx: Pointer;

function ChildWindowFromPointEx;
begin
  GetProcedureAddress(_ChildWindowFromPointEx, user32, 'ChildWindowFromPointEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChildWindowFromPointEx]
  end;
end;
{$ELSE}
function ChildWindowFromPointEx; external user32 name 'ChildWindowFromPointEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSysColor: Pointer;

function GetSysColor;
begin
  GetProcedureAddress(_GetSysColor, user32, 'GetSysColor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSysColor]
  end;
end;
{$ELSE}
function GetSysColor; external user32 name 'GetSysColor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetSysColorBrush: Pointer;

function GetSysColorBrush;
begin
  GetProcedureAddress(_GetSysColorBrush, user32, 'GetSysColorBrush');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetSysColorBrush]
  end;
end;
{$ELSE}
function GetSysColorBrush; external user32 name 'GetSysColorBrush';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSysColors: Pointer;

function SetSysColors;
begin
  GetProcedureAddress(_SetSysColors, user32, 'SetSysColors');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSysColors]
  end;
end;
{$ELSE}
function SetSysColors; external user32 name 'SetSysColors';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawFocusRect: Pointer;

function DrawFocusRect;
begin
  GetProcedureAddress(_DrawFocusRect, user32, 'DrawFocusRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawFocusRect]
  end;
end;
{$ELSE}
function DrawFocusRect; external user32 name 'DrawFocusRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FillRect: Pointer;

function FillRect;
begin
  GetProcedureAddress(_FillRect, user32, 'FillRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillRect]
  end;
end;
{$ELSE}
function FillRect; external user32 name 'FillRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FrameRect: Pointer;

function FrameRect;
begin
  GetProcedureAddress(_FrameRect, user32, 'FrameRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FrameRect]
  end;
end;
{$ELSE}
function FrameRect; external user32 name 'FrameRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InvertRect: Pointer;

function InvertRect;
begin
  GetProcedureAddress(_InvertRect, user32, 'InvertRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InvertRect]
  end;
end;
{$ELSE}
function InvertRect; external user32 name 'InvertRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetRect: Pointer;

function SetRect;
begin
  GetProcedureAddress(_SetRect, user32, 'SetRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetRect]
  end;
end;
{$ELSE}
function SetRect; external user32 name 'SetRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetRectEmpty: Pointer;

function SetRectEmpty;
begin
  GetProcedureAddress(_SetRectEmpty, user32, 'SetRectEmpty');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetRectEmpty]
  end;
end;
{$ELSE}
function SetRectEmpty; external user32 name 'SetRectEmpty';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyRect: Pointer;

function CopyRect;
begin
  GetProcedureAddress(_CopyRect, user32, 'CopyRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyRect]
  end;
end;
{$ELSE}
function CopyRect; external user32 name 'CopyRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InflateRect: Pointer;

function InflateRect;
begin
  GetProcedureAddress(_InflateRect, user32, 'InflateRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InflateRect]
  end;
end;
{$ELSE}
function InflateRect; external user32 name 'InflateRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IntersectRect: Pointer;

function IntersectRect;
begin
  GetProcedureAddress(_IntersectRect, user32, 'IntersectRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IntersectRect]
  end;
end;
{$ELSE}
function IntersectRect; external user32 name 'IntersectRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnionRect: Pointer;

function UnionRect;
begin
  GetProcedureAddress(_UnionRect, user32, 'UnionRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnionRect]
  end;
end;
{$ELSE}
function UnionRect; external user32 name 'UnionRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SubtractRect: Pointer;

function SubtractRect;
begin
  GetProcedureAddress(_SubtractRect, user32, 'SubtractRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SubtractRect]
  end;
end;
{$ELSE}
function SubtractRect; external user32 name 'SubtractRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _OffsetRect: Pointer;

function OffsetRect;
begin
  GetProcedureAddress(_OffsetRect, user32, 'OffsetRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_OffsetRect]
  end;
end;
{$ELSE}
function OffsetRect; external user32 name 'OffsetRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsRectEmpty: Pointer;

function IsRectEmpty;
begin
  GetProcedureAddress(_IsRectEmpty, user32, 'IsRectEmpty');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsRectEmpty]
  end;
end;
{$ELSE}
function IsRectEmpty; external user32 name 'IsRectEmpty';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EqualRect: Pointer;

function EqualRect;
begin
  GetProcedureAddress(_EqualRect, user32, 'EqualRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EqualRect]
  end;
end;
{$ELSE}
function EqualRect; external user32 name 'EqualRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PtInRect: Pointer;

function PtInRect;
begin
  GetProcedureAddress(_PtInRect, user32, 'PtInRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PtInRect]
  end;
end;
{$ELSE}
function PtInRect; external user32 name 'PtInRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowWord: Pointer;

function GetWindowWord;
begin
  GetProcedureAddress(_GetWindowWord, user32, 'GetWindowWord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowWord]
  end;
end;
{$ELSE}
function GetWindowWord; external user32 name 'GetWindowWord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowWord: Pointer;

function SetWindowWord;
begin
  GetProcedureAddress(_SetWindowWord, user32, 'SetWindowWord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowWord]
  end;
end;
{$ELSE}
function SetWindowWord; external user32 name 'SetWindowWord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowLongA: Pointer;

function GetWindowLongA;
begin
  GetProcedureAddress(_GetWindowLongA, user32, 'GetWindowLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowLongA]
  end;
end;
{$ELSE}
function GetWindowLongA; external user32 name 'GetWindowLongA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowLongW: Pointer;

function GetWindowLongW;
begin
  GetProcedureAddress(_GetWindowLongW, user32, 'GetWindowLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowLongW]
  end;
end;
{$ELSE}
function GetWindowLongW; external user32 name 'GetWindowLongW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowLong: Pointer;

function GetWindowLong;
begin
  GetProcedureAddress(_GetWindowLong, user32, 'GetWindowLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowLong]
  end;
end;
{$ELSE}
function GetWindowLong; external user32 name 'GetWindowLongW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowLong: Pointer;

function GetWindowLong;
begin
  GetProcedureAddress(_GetWindowLong, user32, 'GetWindowLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowLong]
  end;
end;
{$ELSE}
function GetWindowLong; external user32 name 'GetWindowLongA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowLongA: Pointer;

function SetWindowLongA;
begin
  GetProcedureAddress(_SetWindowLongA, user32, 'SetWindowLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowLongA]
  end;
end;
{$ELSE}
function SetWindowLongA; external user32 name 'SetWindowLongA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowLongW: Pointer;

function SetWindowLongW;
begin
  GetProcedureAddress(_SetWindowLongW, user32, 'SetWindowLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowLongW]
  end;
end;
{$ELSE}
function SetWindowLongW; external user32 name 'SetWindowLongW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowLong: Pointer;

function SetWindowLong;
begin
  GetProcedureAddress(_SetWindowLong, user32, 'SetWindowLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowLong]
  end;
end;
{$ELSE}
function SetWindowLong; external user32 name 'SetWindowLongW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowLong: Pointer;

function SetWindowLong;
begin
  GetProcedureAddress(_SetWindowLong, user32, 'SetWindowLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowLong]
  end;
end;
{$ELSE}
function SetWindowLong; external user32 name 'SetWindowLongA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassWord: Pointer;

function GetClassWord;
begin
  GetProcedureAddress(_GetClassWord, user32, 'GetClassWord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassWord]
  end;
end;
{$ELSE}
function GetClassWord; external user32 name 'GetClassWord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetClassWord: Pointer;

function SetClassWord;
begin
  GetProcedureAddress(_SetClassWord, user32, 'SetClassWord');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClassWord]
  end;
end;
{$ELSE}
function SetClassWord; external user32 name 'SetClassWord';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassLongA: Pointer;

function GetClassLongA;
begin
  GetProcedureAddress(_GetClassLongA, user32, 'GetClassLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassLongA]
  end;
end;
{$ELSE}
function GetClassLongA; external user32 name 'GetClassLongA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassLongW: Pointer;

function GetClassLongW;
begin
  GetProcedureAddress(_GetClassLongW, user32, 'GetClassLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassLongW]
  end;
end;
{$ELSE}
function GetClassLongW; external user32 name 'GetClassLongW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassLong: Pointer;

function GetClassLong;
begin
  GetProcedureAddress(_GetClassLong, user32, 'GetClassLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassLong]
  end;
end;
{$ELSE}
function GetClassLong; external user32 name 'GetClassLongW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassLong: Pointer;

function GetClassLong;
begin
  GetProcedureAddress(_GetClassLong, user32, 'GetClassLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassLong]
  end;
end;
{$ELSE}
function GetClassLong; external user32 name 'GetClassLongA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetClassLongA: Pointer;

function SetClassLongA;
begin
  GetProcedureAddress(_SetClassLongA, user32, 'SetClassLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClassLongA]
  end;
end;
{$ELSE}
function SetClassLongA; external user32 name 'SetClassLongA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetClassLongW: Pointer;

function SetClassLongW;
begin
  GetProcedureAddress(_SetClassLongW, user32, 'SetClassLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClassLongW]
  end;
end;
{$ELSE}
function SetClassLongW; external user32 name 'SetClassLongW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetClassLong: Pointer;

function SetClassLong;
begin
  GetProcedureAddress(_SetClassLong, user32, 'SetClassLongW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClassLong]
  end;
end;
{$ELSE}
function SetClassLong; external user32 name 'SetClassLongW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetClassLong: Pointer;

function SetClassLong;
begin
  GetProcedureAddress(_SetClassLong, user32, 'SetClassLongA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetClassLong]
  end;
end;
{$ELSE}
function SetClassLong; external user32 name 'SetClassLongA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetProcessDefaultLayout: Pointer;

function GetProcessDefaultLayout;
begin
  GetProcedureAddress(_GetProcessDefaultLayout, user32, 'GetProcessDefaultLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetProcessDefaultLayout]
  end;
end;
{$ELSE}
function GetProcessDefaultLayout; external user32 name 'GetProcessDefaultLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetProcessDefaultLayout: Pointer;

function SetProcessDefaultLayout;
begin
  GetProcedureAddress(_SetProcessDefaultLayout, user32, 'SetProcessDefaultLayout');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetProcessDefaultLayout]
  end;
end;
{$ELSE}
function SetProcessDefaultLayout; external user32 name 'SetProcessDefaultLayout';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetDesktopWindow: Pointer;

function GetDesktopWindow;
begin
  GetProcedureAddress(_GetDesktopWindow, user32, 'GetDesktopWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetDesktopWindow]
  end;
end;
{$ELSE}
function GetDesktopWindow; external user32 name 'GetDesktopWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetParent: Pointer;

function GetParent;
begin
  GetProcedureAddress(_GetParent, user32, 'GetParent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetParent]
  end;
end;
{$ELSE}
function GetParent; external user32 name 'GetParent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetParent: Pointer;

function SetParent;
begin
  GetProcedureAddress(_SetParent, user32, 'SetParent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetParent]
  end;
end;
{$ELSE}
function SetParent; external user32 name 'SetParent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumChildWindows: Pointer;

function EnumChildWindows;
begin
  GetProcedureAddress(_EnumChildWindows, user32, 'EnumChildWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumChildWindows]
  end;
end;
{$ELSE}
function EnumChildWindows; external user32 name 'EnumChildWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindowA: Pointer;

function FindWindowA;
begin
  GetProcedureAddress(_FindWindowA, user32, 'FindWindowA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindowA]
  end;
end;
{$ELSE}
function FindWindowA; external user32 name 'FindWindowA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindowW: Pointer;

function FindWindowW;
begin
  GetProcedureAddress(_FindWindowW, user32, 'FindWindowW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindowW]
  end;
end;
{$ELSE}
function FindWindowW; external user32 name 'FindWindowW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindow: Pointer;

function FindWindow;
begin
  GetProcedureAddress(_FindWindow, user32, 'FindWindowW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindow]
  end;
end;
{$ELSE}
function FindWindow; external user32 name 'FindWindowW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindow: Pointer;

function FindWindow;
begin
  GetProcedureAddress(_FindWindow, user32, 'FindWindowA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindow]
  end;
end;
{$ELSE}
function FindWindow; external user32 name 'FindWindowA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindowExA: Pointer;

function FindWindowExA;
begin
  GetProcedureAddress(_FindWindowExA, user32, 'FindWindowExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindowExA]
  end;
end;
{$ELSE}
function FindWindowExA; external user32 name 'FindWindowExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindowExW: Pointer;

function FindWindowExW;
begin
  GetProcedureAddress(_FindWindowExW, user32, 'FindWindowExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindowExW]
  end;
end;
{$ELSE}
function FindWindowExW; external user32 name 'FindWindowExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindowEx: Pointer;

function FindWindowEx;
begin
  GetProcedureAddress(_FindWindowEx, user32, 'FindWindowExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindowEx]
  end;
end;
{$ELSE}
function FindWindowEx; external user32 name 'FindWindowExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FindWindowEx: Pointer;

function FindWindowEx;
begin
  GetProcedureAddress(_FindWindowEx, user32, 'FindWindowExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FindWindowEx]
  end;
end;
{$ELSE}
function FindWindowEx; external user32 name 'FindWindowExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetShellWindow: Pointer;

function GetShellWindow;
begin
  GetProcedureAddress(_GetShellWindow, user32, 'GetShellWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetShellWindow]
  end;
end;
{$ELSE}
function GetShellWindow; external user32 name 'GetShellWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RegisterShellHookWindow: Pointer;

function RegisterShellHookWindow;
begin
  GetProcedureAddress(_RegisterShellHookWindow, user32, 'RegisterShellHookWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterShellHookWindow]
  end;
end;
{$ELSE}
function RegisterShellHookWindow; external user32 name 'RegisterShellHookWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DeregisterShellHookWindow: Pointer;

function DeregisterShellHookWindow;
begin
  GetProcedureAddress(_DeregisterShellHookWindow, user32, 'DeregisterShellHookWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DeregisterShellHookWindow]
  end;
end;
{$ELSE}
function DeregisterShellHookWindow; external user32 name 'DeregisterShellHookWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumWindows: Pointer;

function EnumWindows;
begin
  GetProcedureAddress(_EnumWindows, user32, 'EnumWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumWindows]
  end;
end;
{$ELSE}
function EnumWindows; external user32 name 'EnumWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumThreadWindows: Pointer;

function EnumThreadWindows;
begin
  GetProcedureAddress(_EnumThreadWindows, user32, 'EnumThreadWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumThreadWindows]
  end;
end;
{$ELSE}
function EnumThreadWindows; external user32 name 'EnumThreadWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassNameA: Pointer;

function GetClassNameA;
begin
  GetProcedureAddress(_GetClassNameA, user32, 'GetClassNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassNameA]
  end;
end;
{$ELSE}
function GetClassNameA; external user32 name 'GetClassNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassNameW: Pointer;

function GetClassNameW;
begin
  GetProcedureAddress(_GetClassNameW, user32, 'GetClassNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassNameW]
  end;
end;
{$ELSE}
function GetClassNameW; external user32 name 'GetClassNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassName: Pointer;

function GetClassName;
begin
  GetProcedureAddress(_GetClassName, user32, 'GetClassNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassName]
  end;
end;
{$ELSE}
function GetClassName; external user32 name 'GetClassNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetClassName: Pointer;

function GetClassName;
begin
  GetProcedureAddress(_GetClassName, user32, 'GetClassNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetClassName]
  end;
end;
{$ELSE}
function GetClassName; external user32 name 'GetClassNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetTopWindow: Pointer;

function GetTopWindow;
begin
  GetProcedureAddress(_GetTopWindow, user32, 'GetTopWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTopWindow]
  end;
end;
{$ELSE}
function GetTopWindow; external user32 name 'GetTopWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowThreadProcessId: Pointer;

function GetWindowThreadProcessId;
begin
  GetProcedureAddress(_GetWindowThreadProcessId, user32, 'GetWindowThreadProcessId');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowThreadProcessId]
  end;
end;
{$ELSE}
function GetWindowThreadProcessId; external user32 name 'GetWindowThreadProcessId';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsGUIThread: Pointer;

function IsGUIThread;
begin
  GetProcedureAddress(_IsGUIThread, user32, 'IsGUIThread');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsGUIThread]
  end;
end;
{$ELSE}
function IsGUIThread; external user32 name 'IsGUIThread';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLastActivePopup: Pointer;

function GetLastActivePopup;
begin
  GetProcedureAddress(_GetLastActivePopup, user32, 'GetLastActivePopup');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLastActivePopup]
  end;
end;
{$ELSE}
function GetLastActivePopup; external user32 name 'GetLastActivePopup';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindow: Pointer;

function GetWindow;
begin
  GetProcedureAddress(_GetWindow, user32, 'GetWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindow]
  end;
end;
{$ELSE}
function GetWindow; external user32 name 'GetWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHookA: Pointer;

function SetWindowsHookA;
begin
  GetProcedureAddress(_SetWindowsHookA, user32, 'SetWindowsHookA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHookA]
  end;
end;
{$ELSE}
function SetWindowsHookA; external user32 name 'SetWindowsHookA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHookW: Pointer;

function SetWindowsHookW;
begin
  GetProcedureAddress(_SetWindowsHookW, user32, 'SetWindowsHookW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHookW]
  end;
end;
{$ELSE}
function SetWindowsHookW; external user32 name 'SetWindowsHookW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHook: Pointer;

function SetWindowsHook;
begin
  GetProcedureAddress(_SetWindowsHook, user32, 'SetWindowsHookW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHook]
  end;
end;
{$ELSE}
function SetWindowsHook; external user32 name 'SetWindowsHookW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHook: Pointer;

function SetWindowsHook;
begin
  GetProcedureAddress(_SetWindowsHook, user32, 'SetWindowsHookA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHook]
  end;
end;
{$ELSE}
function SetWindowsHook; external user32 name 'SetWindowsHookA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UnhookWindowsHook: Pointer;

function UnhookWindowsHook;
begin
  GetProcedureAddress(_UnhookWindowsHook, user32, 'UnhookWindowsHook');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnhookWindowsHook]
  end;
end;
{$ELSE}
function UnhookWindowsHook; external user32 name 'UnhookWindowsHook';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHookExA: Pointer;

function SetWindowsHookExA;
begin
  GetProcedureAddress(_SetWindowsHookExA, user32, 'SetWindowsHookExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHookExA]
  end;
end;
{$ELSE}
function SetWindowsHookExA; external user32 name 'SetWindowsHookExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHookExW: Pointer;

function SetWindowsHookExW;
begin
  GetProcedureAddress(_SetWindowsHookExW, user32, 'SetWindowsHookExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHookExW]
  end;
end;
{$ELSE}
function SetWindowsHookExW; external user32 name 'SetWindowsHookExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHookEx: Pointer;

function SetWindowsHookEx;
begin
  GetProcedureAddress(_SetWindowsHookEx, user32, 'SetWindowsHookExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHookEx]
  end;
end;
{$ELSE}
function SetWindowsHookEx; external user32 name 'SetWindowsHookExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetWindowsHookEx: Pointer;

function SetWindowsHookEx;
begin
  GetProcedureAddress(_SetWindowsHookEx, user32, 'SetWindowsHookExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWindowsHookEx]
  end;
end;
{$ELSE}
function SetWindowsHookEx; external user32 name 'SetWindowsHookExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _UnhookWindowsHookEx: Pointer;

function UnhookWindowsHookEx;
begin
  GetProcedureAddress(_UnhookWindowsHookEx, user32, 'UnhookWindowsHookEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnhookWindowsHookEx]
  end;
end;
{$ELSE}
function UnhookWindowsHookEx; external user32 name 'UnhookWindowsHookEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CallNextHookEx: Pointer;

function CallNextHookEx;
begin
  GetProcedureAddress(_CallNextHookEx, user32, 'CallNextHookEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CallNextHookEx]
  end;
end;
{$ELSE}
function CallNextHookEx; external user32 name 'CallNextHookEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CheckMenuRadioItem: Pointer;

function CheckMenuRadioItem;
begin
  GetProcedureAddress(_CheckMenuRadioItem, user32, 'CheckMenuRadioItem');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CheckMenuRadioItem]
  end;
end;
{$ELSE}
function CheckMenuRadioItem; external user32 name 'CheckMenuRadioItem';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadBitmapA: Pointer;

function LoadBitmapA;
begin
  GetProcedureAddress(_LoadBitmapA, user32, 'LoadBitmapA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadBitmapA]
  end;
end;
{$ELSE}
function LoadBitmapA; external user32 name 'LoadBitmapA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadBitmapW: Pointer;

function LoadBitmapW;
begin
  GetProcedureAddress(_LoadBitmapW, user32, 'LoadBitmapW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadBitmapW]
  end;
end;
{$ELSE}
function LoadBitmapW; external user32 name 'LoadBitmapW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadBitmap: Pointer;

function LoadBitmap;
begin
  GetProcedureAddress(_LoadBitmap, user32, 'LoadBitmapW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadBitmap]
  end;
end;
{$ELSE}
function LoadBitmap; external user32 name 'LoadBitmapW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadBitmap: Pointer;

function LoadBitmap;
begin
  GetProcedureAddress(_LoadBitmap, user32, 'LoadBitmapA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadBitmap]
  end;
end;
{$ELSE}
function LoadBitmap; external user32 name 'LoadBitmapA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursorA: Pointer;

function LoadCursorA;
begin
  GetProcedureAddress(_LoadCursorA, user32, 'LoadCursorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursorA]
  end;
end;
{$ELSE}
function LoadCursorA; external user32 name 'LoadCursorA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursorW: Pointer;

function LoadCursorW;
begin
  GetProcedureAddress(_LoadCursorW, user32, 'LoadCursorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursorW]
  end;
end;
{$ELSE}
function LoadCursorW; external user32 name 'LoadCursorW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursor: Pointer;

function LoadCursor;
begin
  GetProcedureAddress(_LoadCursor, user32, 'LoadCursorW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursor]
  end;
end;
{$ELSE}
function LoadCursor; external user32 name 'LoadCursorW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursor: Pointer;

function LoadCursor;
begin
  GetProcedureAddress(_LoadCursor, user32, 'LoadCursorA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursor]
  end;
end;
{$ELSE}
function LoadCursor; external user32 name 'LoadCursorA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursorFromFileA: Pointer;

function LoadCursorFromFileA;
begin
  GetProcedureAddress(_LoadCursorFromFileA, user32, 'LoadCursorFromFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursorFromFileA]
  end;
end;
{$ELSE}
function LoadCursorFromFileA; external user32 name 'LoadCursorFromFileA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursorFromFileW: Pointer;

function LoadCursorFromFileW;
begin
  GetProcedureAddress(_LoadCursorFromFileW, user32, 'LoadCursorFromFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursorFromFileW]
  end;
end;
{$ELSE}
function LoadCursorFromFileW; external user32 name 'LoadCursorFromFileW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursorFromFile: Pointer;

function LoadCursorFromFile;
begin
  GetProcedureAddress(_LoadCursorFromFile, user32, 'LoadCursorFromFileW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursorFromFile]
  end;
end;
{$ELSE}
function LoadCursorFromFile; external user32 name 'LoadCursorFromFileW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadCursorFromFile: Pointer;

function LoadCursorFromFile;
begin
  GetProcedureAddress(_LoadCursorFromFile, user32, 'LoadCursorFromFileA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadCursorFromFile]
  end;
end;
{$ELSE}
function LoadCursorFromFile; external user32 name 'LoadCursorFromFileA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CreateCursor: Pointer;

function CreateCursor;
begin
  GetProcedureAddress(_CreateCursor, user32, 'CreateCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateCursor]
  end;
end;
{$ELSE}
function CreateCursor; external user32 name 'CreateCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyCursor: Pointer;

function DestroyCursor;
begin
  GetProcedureAddress(_DestroyCursor, user32, 'DestroyCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyCursor]
  end;
end;
{$ELSE}
function DestroyCursor; external user32 name 'DestroyCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetSystemCursor: Pointer;

function SetSystemCursor;
begin
  GetProcedureAddress(_SetSystemCursor, user32, 'SetSystemCursor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetSystemCursor]
  end;
end;
{$ELSE}
function SetSystemCursor; external user32 name 'SetSystemCursor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadIconA: Pointer;

function LoadIconA;
begin
  GetProcedureAddress(_LoadIconA, user32, 'LoadIconA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadIconA]
  end;
end;
{$ELSE}
function LoadIconA; external user32 name 'LoadIconA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadIconW: Pointer;

function LoadIconW;
begin
  GetProcedureAddress(_LoadIconW, user32, 'LoadIconW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadIconW]
  end;
end;
{$ELSE}
function LoadIconW; external user32 name 'LoadIconW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadIcon: Pointer;

function LoadIcon;
begin
  GetProcedureAddress(_LoadIcon, user32, 'LoadIconW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadIcon]
  end;
end;
{$ELSE}
function LoadIcon; external user32 name 'LoadIconW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadIcon: Pointer;

function LoadIcon;
begin
  GetProcedureAddress(_LoadIcon, user32, 'LoadIconA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadIcon]
  end;
end;
{$ELSE}
function LoadIcon; external user32 name 'LoadIconA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _PrivateExtractIconsA: Pointer;

function PrivateExtractIconsA;
begin
  GetProcedureAddress(_PrivateExtractIconsA, user32, 'PrivateExtractIconsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivateExtractIconsA]
  end;
end;
{$ELSE}
function PrivateExtractIconsA; external user32 name 'PrivateExtractIconsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PrivateExtractIconsW: Pointer;

function PrivateExtractIconsW;
begin
  GetProcedureAddress(_PrivateExtractIconsW, user32, 'PrivateExtractIconsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivateExtractIconsW]
  end;
end;
{$ELSE}
function PrivateExtractIconsW; external user32 name 'PrivateExtractIconsW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PrivateExtractIcons: Pointer;

function PrivateExtractIcons;
begin
  GetProcedureAddress(_PrivateExtractIcons, user32, 'PrivateExtractIconsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivateExtractIcons]
  end;
end;
{$ELSE}
function PrivateExtractIcons; external user32 name 'PrivateExtractIconsW';
{$ENDIF DYNAMIC_LINK}

{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PrivateExtractIcons: Pointer;

function PrivateExtractIcons;
begin
  GetProcedureAddress(_PrivateExtractIcons, user32, 'PrivateExtractIconsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PrivateExtractIcons]
  end;
end;
{$ELSE}
function PrivateExtractIcons; external user32 name 'PrivateExtractIconsA';
{$ENDIF DYNAMIC_LINK}

{$ENDIF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIcon: Pointer;

function CreateIcon;
begin
  GetProcedureAddress(_CreateIcon, user32, 'CreateIcon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIcon]
  end;
end;
{$ELSE}
function CreateIcon; external user32 name 'CreateIcon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DestroyIcon: Pointer;

function DestroyIcon;
begin
  GetProcedureAddress(_DestroyIcon, user32, 'DestroyIcon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DestroyIcon]
  end;
end;
{$ELSE}
function DestroyIcon; external user32 name 'DestroyIcon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupIconIdFromDirectory: Pointer;

function LookupIconIdFromDirectory;
begin
  GetProcedureAddress(_LookupIconIdFromDirectory, user32, 'LookupIconIdFromDirectory');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupIconIdFromDirectory]
  end;
end;
{$ELSE}
function LookupIconIdFromDirectory; external user32 name 'LookupIconIdFromDirectory';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LookupIconIdFromDirectoryEx: Pointer;

function LookupIconIdFromDirectoryEx;
begin
  GetProcedureAddress(_LookupIconIdFromDirectoryEx, user32, 'LookupIconIdFromDirectoryEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LookupIconIdFromDirectoryEx]
  end;
end;
{$ELSE}
function LookupIconIdFromDirectoryEx; external user32 name 'LookupIconIdFromDirectoryEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIconFromResource: Pointer;

function CreateIconFromResource;
begin
  GetProcedureAddress(_CreateIconFromResource, user32, 'CreateIconFromResource');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIconFromResource]
  end;
end;
{$ELSE}
function CreateIconFromResource; external user32 name 'CreateIconFromResource';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIconFromResourceEx: Pointer;

function CreateIconFromResourceEx;
begin
  GetProcedureAddress(_CreateIconFromResourceEx, user32, 'CreateIconFromResourceEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIconFromResourceEx]
  end;
end;
{$ELSE}
function CreateIconFromResourceEx; external user32 name 'CreateIconFromResourceEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadImageA: Pointer;

function LoadImageA;
begin
  GetProcedureAddress(_LoadImageA, user32, 'LoadImageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadImageA]
  end;
end;
{$ELSE}
function LoadImageA; external user32 name 'LoadImageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadImageW: Pointer;

function LoadImageW;
begin
  GetProcedureAddress(_LoadImageW, user32, 'LoadImageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadImageW]
  end;
end;
{$ELSE}
function LoadImageW; external user32 name 'LoadImageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadImage: Pointer;

function LoadImage;
begin
  GetProcedureAddress(_LoadImage, user32, 'LoadImageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadImage]
  end;
end;
{$ELSE}
function LoadImage; external user32 name 'LoadImageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadImage: Pointer;

function LoadImage;
begin
  GetProcedureAddress(_LoadImage, user32, 'LoadImageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadImage]
  end;
end;
{$ELSE}
function LoadImage; external user32 name 'LoadImageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _CopyImage: Pointer;

function CopyImage;
begin
  GetProcedureAddress(_CopyImage, user32, 'CopyImage');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyImage]
  end;
end;
{$ELSE}
function CopyImage; external user32 name 'CopyImage';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DrawIconEx: Pointer;

function DrawIconEx;
begin
  GetProcedureAddress(_DrawIconEx, user32, 'DrawIconEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DrawIconEx]
  end;
end;
{$ELSE}
function DrawIconEx; external user32 name 'DrawIconEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateIconIndirect: Pointer;

function CreateIconIndirect;
begin
  GetProcedureAddress(_CreateIconIndirect, user32, 'CreateIconIndirect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateIconIndirect]
  end;
end;
{$ELSE}
function CreateIconIndirect; external user32 name 'CreateIconIndirect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CopyIcon: Pointer;

function CopyIcon;
begin
  GetProcedureAddress(_CopyIcon, user32, 'CopyIcon');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CopyIcon]
  end;
end;
{$ELSE}
function CopyIcon; external user32 name 'CopyIcon';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetIconInfo: Pointer;

function GetIconInfo;
begin
  GetProcedureAddress(_GetIconInfo, user32, 'GetIconInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetIconInfo]
  end;
end;
{$ELSE}
function GetIconInfo; external user32 name 'GetIconInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadStringA: Pointer;

function LoadStringA;
begin
  GetProcedureAddress(_LoadStringA, user32, 'LoadStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadStringA]
  end;
end;
{$ELSE}
function LoadStringA; external user32 name 'LoadStringA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LoadStringW: Pointer;

function LoadStringW;
begin
  GetProcedureAddress(_LoadStringW, user32, 'LoadStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadStringW]
  end;
end;
{$ELSE}
function LoadStringW; external user32 name 'LoadStringW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadString: Pointer;

function LoadString;
begin
  GetProcedureAddress(_LoadString, user32, 'LoadStringW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadString]
  end;
end;
{$ELSE}
function LoadString; external user32 name 'LoadStringW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _LoadString: Pointer;

function LoadString;
begin
  GetProcedureAddress(_LoadString, user32, 'LoadStringA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LoadString]
  end;
end;
{$ELSE}
function LoadString; external user32 name 'LoadStringA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _IsDialogMessageA: Pointer;

function IsDialogMessageA;
begin
  GetProcedureAddress(_IsDialogMessageA, user32, 'IsDialogMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDialogMessageA]
  end;
end;
{$ELSE}
function IsDialogMessageA; external user32 name 'IsDialogMessageA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsDialogMessageW: Pointer;

function IsDialogMessageW;
begin
  GetProcedureAddress(_IsDialogMessageW, user32, 'IsDialogMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDialogMessageW]
  end;
end;
{$ELSE}
function IsDialogMessageW; external user32 name 'IsDialogMessageW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _IsDialogMessage: Pointer;

function IsDialogMessage;
begin
  GetProcedureAddress(_IsDialogMessage, user32, 'IsDialogMessageW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDialogMessage]
  end;
end;
{$ELSE}
function IsDialogMessage; external user32 name 'IsDialogMessageW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _IsDialogMessage: Pointer;

function IsDialogMessage;
begin
  GetProcedureAddress(_IsDialogMessage, user32, 'IsDialogMessageA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsDialogMessage]
  end;
end;
{$ELSE}
function IsDialogMessage; external user32 name 'IsDialogMessageA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _MapDialogRect: Pointer;

function MapDialogRect;
begin
  GetProcedureAddress(_MapDialogRect, user32, 'MapDialogRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MapDialogRect]
  end;
end;
{$ELSE}
function MapDialogRect; external user32 name 'MapDialogRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirListA: Pointer;

function DlgDirListA;
begin
  GetProcedureAddress(_DlgDirListA, user32, 'DlgDirListA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirListA]
  end;
end;
{$ELSE}
function DlgDirListA; external user32 name 'DlgDirListA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirListW: Pointer;

function DlgDirListW;
begin
  GetProcedureAddress(_DlgDirListW, user32, 'DlgDirListW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirListW]
  end;
end;
{$ELSE}
function DlgDirListW; external user32 name 'DlgDirListW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirList: Pointer;

function DlgDirList;
begin
  GetProcedureAddress(_DlgDirList, user32, 'DlgDirListW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirList]
  end;
end;
{$ELSE}
function DlgDirList; external user32 name 'DlgDirListW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirList: Pointer;

function DlgDirList;
begin
  GetProcedureAddress(_DlgDirList, user32, 'DlgDirListA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirList]
  end;
end;
{$ELSE}
function DlgDirList; external user32 name 'DlgDirListA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectExA: Pointer;

function DlgDirSelectExA;
begin
  GetProcedureAddress(_DlgDirSelectExA, user32, 'DlgDirSelectExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectExA]
  end;
end;
{$ELSE}
function DlgDirSelectExA; external user32 name 'DlgDirSelectExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectExW: Pointer;

function DlgDirSelectExW;
begin
  GetProcedureAddress(_DlgDirSelectExW, user32, 'DlgDirSelectExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectExW]
  end;
end;
{$ELSE}
function DlgDirSelectExW; external user32 name 'DlgDirSelectExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectEx: Pointer;

function DlgDirSelectEx;
begin
  GetProcedureAddress(_DlgDirSelectEx, user32, 'DlgDirSelectExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectEx]
  end;
end;
{$ELSE}
function DlgDirSelectEx; external user32 name 'DlgDirSelectExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectEx: Pointer;

function DlgDirSelectEx;
begin
  GetProcedureAddress(_DlgDirSelectEx, user32, 'DlgDirSelectExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectEx]
  end;
end;
{$ELSE}
function DlgDirSelectEx; external user32 name 'DlgDirSelectExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirListComboBoxA: Pointer;

function DlgDirListComboBoxA;
begin
  GetProcedureAddress(_DlgDirListComboBoxA, user32, 'DlgDirListComboBoxA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirListComboBoxA]
  end;
end;
{$ELSE}
function DlgDirListComboBoxA; external user32 name 'DlgDirListComboBoxA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirListComboBoxW: Pointer;

function DlgDirListComboBoxW;
begin
  GetProcedureAddress(_DlgDirListComboBoxW, user32, 'DlgDirListComboBoxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirListComboBoxW]
  end;
end;
{$ELSE}
function DlgDirListComboBoxW; external user32 name 'DlgDirListComboBoxW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirListComboBox: Pointer;

function DlgDirListComboBox;
begin
  GetProcedureAddress(_DlgDirListComboBox, user32, 'DlgDirListComboBoxW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirListComboBox]
  end;
end;
{$ELSE}
function DlgDirListComboBox; external user32 name 'DlgDirListComboBoxW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirListComboBox: Pointer;

function DlgDirListComboBox;
begin
  GetProcedureAddress(_DlgDirListComboBox, user32, 'DlgDirListComboBoxA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirListComboBox]
  end;
end;
{$ELSE}
function DlgDirListComboBox; external user32 name 'DlgDirListComboBoxA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectComboBoxExA: Pointer;

function DlgDirSelectComboBoxExA;
begin
  GetProcedureAddress(_DlgDirSelectComboBoxExA, user32, 'DlgDirSelectComboBoxExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectComboBoxExA]
  end;
end;
{$ELSE}
function DlgDirSelectComboBoxExA; external user32 name 'DlgDirSelectComboBoxExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectComboBoxExW: Pointer;

function DlgDirSelectComboBoxExW;
begin
  GetProcedureAddress(_DlgDirSelectComboBoxExW, user32, 'DlgDirSelectComboBoxExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectComboBoxExW]
  end;
end;
{$ELSE}
function DlgDirSelectComboBoxExW; external user32 name 'DlgDirSelectComboBoxExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectComboBoxEx: Pointer;

function DlgDirSelectComboBoxEx;
begin
  GetProcedureAddress(_DlgDirSelectComboBoxEx, user32, 'DlgDirSelectComboBoxExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectComboBoxEx]
  end;
end;
{$ELSE}
function DlgDirSelectComboBoxEx; external user32 name 'DlgDirSelectComboBoxExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DlgDirSelectComboBoxEx: Pointer;

function DlgDirSelectComboBoxEx;
begin
  GetProcedureAddress(_DlgDirSelectComboBoxEx, user32, 'DlgDirSelectComboBoxExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DlgDirSelectComboBoxEx]
  end;
end;
{$ELSE}
function DlgDirSelectComboBoxEx; external user32 name 'DlgDirSelectComboBoxExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetScrollInfo: Pointer;

function SetScrollInfo;
begin
  GetProcedureAddress(_SetScrollInfo, user32, 'SetScrollInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetScrollInfo]
  end;
end;
{$ELSE}
function SetScrollInfo; external user32 name 'SetScrollInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetScrollInfo: Pointer;

function GetScrollInfo;
begin
  GetProcedureAddress(_GetScrollInfo, user32, 'GetScrollInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetScrollInfo]
  end;
end;
{$ELSE}
function GetScrollInfo; external user32 name 'GetScrollInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefFrameProcA: Pointer;

function DefFrameProcA;
begin
  GetProcedureAddress(_DefFrameProcA, user32, 'DefFrameProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefFrameProcA]
  end;
end;
{$ELSE}
function DefFrameProcA; external user32 name 'DefFrameProcA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefFrameProcW: Pointer;

function DefFrameProcW;
begin
  GetProcedureAddress(_DefFrameProcW, user32, 'DefFrameProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefFrameProcW]
  end;
end;
{$ELSE}
function DefFrameProcW; external user32 name 'DefFrameProcW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DefFrameProc: Pointer;

function DefFrameProc;
begin
  GetProcedureAddress(_DefFrameProc, user32, 'DefFrameProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefFrameProc]
  end;
end;
{$ELSE}
function DefFrameProc; external user32 name 'DefFrameProcW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DefFrameProc: Pointer;

function DefFrameProc;
begin
  GetProcedureAddress(_DefFrameProc, user32, 'DefFrameProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefFrameProc]
  end;
end;
{$ELSE}
function DefFrameProc; external user32 name 'DefFrameProcA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _DefMDIChildProcA: Pointer;

function DefMDIChildProcA;
begin
  GetProcedureAddress(_DefMDIChildProcA, user32, 'DefMDIChildProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefMDIChildProcA]
  end;
end;
{$ELSE}
function DefMDIChildProcA; external user32 name 'DefMDIChildProcA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefMDIChildProcW: Pointer;

function DefMDIChildProcW;
begin
  GetProcedureAddress(_DefMDIChildProcW, user32, 'DefMDIChildProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefMDIChildProcW]
  end;
end;
{$ELSE}
function DefMDIChildProcW; external user32 name 'DefMDIChildProcW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _DefMDIChildProc: Pointer;

function DefMDIChildProc;
begin
  GetProcedureAddress(_DefMDIChildProc, user32, 'DefMDIChildProcW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefMDIChildProc]
  end;
end;
{$ELSE}
function DefMDIChildProc; external user32 name 'DefMDIChildProcW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _DefMDIChildProc: Pointer;

function DefMDIChildProc;
begin
  GetProcedureAddress(_DefMDIChildProc, user32, 'DefMDIChildProcA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefMDIChildProc]
  end;
end;
{$ELSE}
function DefMDIChildProc; external user32 name 'DefMDIChildProcA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _TranslateMDISysAccel: Pointer;

function TranslateMDISysAccel;
begin
  GetProcedureAddress(_TranslateMDISysAccel, user32, 'TranslateMDISysAccel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TranslateMDISysAccel]
  end;
end;
{$ELSE}
function TranslateMDISysAccel; external user32 name 'TranslateMDISysAccel';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ArrangeIconicWindows: Pointer;

function ArrangeIconicWindows;
begin
  GetProcedureAddress(_ArrangeIconicWindows, user32, 'ArrangeIconicWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ArrangeIconicWindows]
  end;
end;
{$ELSE}
function ArrangeIconicWindows; external user32 name 'ArrangeIconicWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMDIWindowA: Pointer;

function CreateMDIWindowA;
begin
  GetProcedureAddress(_CreateMDIWindowA, user32, 'CreateMDIWindowA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMDIWindowA]
  end;
end;
{$ELSE}
function CreateMDIWindowA; external user32 name 'CreateMDIWindowA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMDIWindowW: Pointer;

function CreateMDIWindowW;
begin
  GetProcedureAddress(_CreateMDIWindowW, user32, 'CreateMDIWindowW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMDIWindowW]
  end;
end;
{$ELSE}
function CreateMDIWindowW; external user32 name 'CreateMDIWindowW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMDIWindow: Pointer;

function CreateMDIWindow;
begin
  GetProcedureAddress(_CreateMDIWindow, user32, 'CreateMDIWindowW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMDIWindow]
  end;
end;
{$ELSE}
function CreateMDIWindow; external user32 name 'CreateMDIWindowW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _CreateMDIWindow: Pointer;

function CreateMDIWindow;
begin
  GetProcedureAddress(_CreateMDIWindow, user32, 'CreateMDIWindowA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateMDIWindow]
  end;
end;
{$ELSE}
function CreateMDIWindow; external user32 name 'CreateMDIWindowA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _TileWindows: Pointer;

function TileWindows;
begin
  GetProcedureAddress(_TileWindows, user32, 'TileWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_TileWindows]
  end;
end;
{$ELSE}
function TileWindows; external user32 name 'TileWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _CascadeWindows: Pointer;

function CascadeWindows;
begin
  GetProcedureAddress(_CascadeWindows, user32, 'CascadeWindows');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CascadeWindows]
  end;
end;
{$ELSE}
function CascadeWindows; external user32 name 'CascadeWindows';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WinHelpA: Pointer;

function WinHelpA;
begin
  GetProcedureAddress(_WinHelpA, user32, 'WinHelpA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WinHelpA]
  end;
end;
{$ELSE}
function WinHelpA; external user32 name 'WinHelpA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WinHelpW: Pointer;

function WinHelpW;
begin
  GetProcedureAddress(_WinHelpW, user32, 'WinHelpW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WinHelpW]
  end;
end;
{$ELSE}
function WinHelpW; external user32 name 'WinHelpW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WinHelp: Pointer;

function WinHelp;
begin
  GetProcedureAddress(_WinHelp, user32, 'WinHelpW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WinHelp]
  end;
end;
{$ELSE}
function WinHelp; external user32 name 'WinHelpW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WinHelp: Pointer;

function WinHelp;
begin
  GetProcedureAddress(_WinHelp, user32, 'WinHelpA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WinHelp]
  end;
end;
{$ELSE}
function WinHelp; external user32 name 'WinHelpA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetGuiResources: Pointer;

function GetGuiResources;
begin
  GetProcedureAddress(_GetGuiResources, user32, 'GetGuiResources');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGuiResources]
  end;
end;
{$ELSE}
function GetGuiResources; external user32 name 'GetGuiResources';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettingsA: Pointer;

function ChangeDisplaySettingsA;
begin
  GetProcedureAddress(_ChangeDisplaySettingsA, user32, 'ChangeDisplaySettingsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettingsA]
  end;
end;
{$ELSE}
function ChangeDisplaySettingsA; external user32 name 'ChangeDisplaySettingsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettingsW: Pointer;

function ChangeDisplaySettingsW;
begin
  GetProcedureAddress(_ChangeDisplaySettingsW, user32, 'ChangeDisplaySettingsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettingsW]
  end;
end;
{$ELSE}
function ChangeDisplaySettingsW; external user32 name 'ChangeDisplaySettingsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettings: Pointer;

function ChangeDisplaySettings;
begin
  GetProcedureAddress(_ChangeDisplaySettings, user32, 'ChangeDisplaySettingsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettings]
  end;
end;
{$ELSE}
function ChangeDisplaySettings; external user32 name 'ChangeDisplaySettingsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettings: Pointer;

function ChangeDisplaySettings;
begin
  GetProcedureAddress(_ChangeDisplaySettings, user32, 'ChangeDisplaySettingsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettings]
  end;
end;
{$ELSE}
function ChangeDisplaySettings; external user32 name 'ChangeDisplaySettingsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettingsExA: Pointer;

function ChangeDisplaySettingsExA;
begin
  GetProcedureAddress(_ChangeDisplaySettingsExA, user32, 'ChangeDisplaySettingsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettingsExA]
  end;
end;
{$ELSE}
function ChangeDisplaySettingsExA; external user32 name 'ChangeDisplaySettingsExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettingsExW: Pointer;

function ChangeDisplaySettingsExW;
begin
  GetProcedureAddress(_ChangeDisplaySettingsExW, user32, 'ChangeDisplaySettingsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettingsExW]
  end;
end;
{$ELSE}
function ChangeDisplaySettingsExW; external user32 name 'ChangeDisplaySettingsExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettingsEx: Pointer;

function ChangeDisplaySettingsEx;
begin
  GetProcedureAddress(_ChangeDisplaySettingsEx, user32, 'ChangeDisplaySettingsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettingsEx]
  end;
end;
{$ELSE}
function ChangeDisplaySettingsEx; external user32 name 'ChangeDisplaySettingsExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ChangeDisplaySettingsEx: Pointer;

function ChangeDisplaySettingsEx;
begin
  GetProcedureAddress(_ChangeDisplaySettingsEx, user32, 'ChangeDisplaySettingsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ChangeDisplaySettingsEx]
  end;
end;
{$ELSE}
function ChangeDisplaySettingsEx; external user32 name 'ChangeDisplaySettingsExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettingsA: Pointer;

function EnumDisplaySettingsA;
begin
  GetProcedureAddress(_EnumDisplaySettingsA, user32, 'EnumDisplaySettingsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettingsA]
  end;
end;
{$ELSE}
function EnumDisplaySettingsA; external user32 name 'EnumDisplaySettingsA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettingsW: Pointer;

function EnumDisplaySettingsW;
begin
  GetProcedureAddress(_EnumDisplaySettingsW, user32, 'EnumDisplaySettingsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettingsW]
  end;
end;
{$ELSE}
function EnumDisplaySettingsW; external user32 name 'EnumDisplaySettingsW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettings: Pointer;

function EnumDisplaySettings;
begin
  GetProcedureAddress(_EnumDisplaySettings, user32, 'EnumDisplaySettingsW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettings]
  end;
end;
{$ELSE}
function EnumDisplaySettings; external user32 name 'EnumDisplaySettingsW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettings: Pointer;

function EnumDisplaySettings;
begin
  GetProcedureAddress(_EnumDisplaySettings, user32, 'EnumDisplaySettingsA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettings]
  end;
end;
{$ELSE}
function EnumDisplaySettings; external user32 name 'EnumDisplaySettingsA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettingsExA: Pointer;

function EnumDisplaySettingsExA;
begin
  GetProcedureAddress(_EnumDisplaySettingsExA, user32, 'EnumDisplaySettingsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettingsExA]
  end;
end;
{$ELSE}
function EnumDisplaySettingsExA; external user32 name 'EnumDisplaySettingsExA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettingsExW: Pointer;

function EnumDisplaySettingsExW;
begin
  GetProcedureAddress(_EnumDisplaySettingsExW, user32, 'EnumDisplaySettingsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettingsExW]
  end;
end;
{$ELSE}
function EnumDisplaySettingsExW; external user32 name 'EnumDisplaySettingsExW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettingsEx: Pointer;

function EnumDisplaySettingsEx;
begin
  GetProcedureAddress(_EnumDisplaySettingsEx, user32, 'EnumDisplaySettingsExW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettingsEx]
  end;
end;
{$ELSE}
function EnumDisplaySettingsEx; external user32 name 'EnumDisplaySettingsExW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplaySettingsEx: Pointer;

function EnumDisplaySettingsEx;
begin
  GetProcedureAddress(_EnumDisplaySettingsEx, user32, 'EnumDisplaySettingsExA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplaySettingsEx]
  end;
end;
{$ELSE}
function EnumDisplaySettingsEx; external user32 name 'EnumDisplaySettingsExA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplayDevicesA: Pointer;

function EnumDisplayDevicesA;
begin
  GetProcedureAddress(_EnumDisplayDevicesA, user32, 'EnumDisplayDevicesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplayDevicesA]
  end;
end;
{$ELSE}
function EnumDisplayDevicesA; external user32 name 'EnumDisplayDevicesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplayDevicesW: Pointer;

function EnumDisplayDevicesW;
begin
  GetProcedureAddress(_EnumDisplayDevicesW, user32, 'EnumDisplayDevicesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplayDevicesW]
  end;
end;
{$ELSE}
function EnumDisplayDevicesW; external user32 name 'EnumDisplayDevicesW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplayDevices: Pointer;

function EnumDisplayDevices;
begin
  GetProcedureAddress(_EnumDisplayDevices, user32, 'EnumDisplayDevicesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplayDevices]
  end;
end;
{$ELSE}
function EnumDisplayDevices; external user32 name 'EnumDisplayDevicesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplayDevices: Pointer;

function EnumDisplayDevices;
begin
  GetProcedureAddress(_EnumDisplayDevices, user32, 'EnumDisplayDevicesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplayDevices]
  end;
end;
{$ELSE}
function EnumDisplayDevices; external user32 name 'EnumDisplayDevicesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SystemParametersInfoA: Pointer;

function SystemParametersInfoA;
begin
  GetProcedureAddress(_SystemParametersInfoA, user32, 'SystemParametersInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SystemParametersInfoA]
  end;
end;
{$ELSE}
function SystemParametersInfoA; external user32 name 'SystemParametersInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SystemParametersInfoW: Pointer;

function SystemParametersInfoW;
begin
  GetProcedureAddress(_SystemParametersInfoW, user32, 'SystemParametersInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SystemParametersInfoW]
  end;
end;
{$ELSE}
function SystemParametersInfoW; external user32 name 'SystemParametersInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SystemParametersInfo: Pointer;

function SystemParametersInfo;
begin
  GetProcedureAddress(_SystemParametersInfo, user32, 'SystemParametersInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SystemParametersInfo]
  end;
end;
{$ELSE}
function SystemParametersInfo; external user32 name 'SystemParametersInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SystemParametersInfo: Pointer;

function SystemParametersInfo;
begin
  GetProcedureAddress(_SystemParametersInfo, user32, 'SystemParametersInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SystemParametersInfo]
  end;
end;
{$ELSE}
function SystemParametersInfo; external user32 name 'SystemParametersInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _SetDebugErrorLevel: Pointer;

procedure SetDebugErrorLevel;
begin
  GetProcedureAddress(_SetDebugErrorLevel, user32, 'SetDebugErrorLevel');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetDebugErrorLevel]
  end;
end;
{$ELSE}
procedure SetDebugErrorLevel; external user32 name 'SetDebugErrorLevel';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetLastErrorEx: Pointer;

procedure SetLastErrorEx;
begin
  GetProcedureAddress(_SetLastErrorEx, user32, 'SetLastErrorEx');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetLastErrorEx]
  end;
end;
{$ELSE}
procedure SetLastErrorEx; external user32 name 'SetLastErrorEx';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _InternalGetWindowText: Pointer;

function InternalGetWindowText;
begin
  GetProcedureAddress(_InternalGetWindowText, user32, 'InternalGetWindowText');
  asm
    mov esp, ebp
    pop ebp
    jmp [_InternalGetWindowText]
  end;
end;
{$ELSE}
function InternalGetWindowText; external user32 name 'InternalGetWindowText';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _EndTask: Pointer;

function EndTask;
begin
  GetProcedureAddress(_EndTask, user32, 'EndTask');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EndTask]
  end;
end;
{$ELSE}
function EndTask; external user32 name 'EndTask';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MonitorFromPoint: Pointer;

function MonitorFromPoint;
begin
  GetProcedureAddress(_MonitorFromPoint, user32, 'MonitorFromPoint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MonitorFromPoint]
  end;
end;
{$ELSE}
function MonitorFromPoint; external user32 name 'MonitorFromPoint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MonitorFromRect: Pointer;

function MonitorFromRect;
begin
  GetProcedureAddress(_MonitorFromRect, user32, 'MonitorFromRect');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MonitorFromRect]
  end;
end;
{$ELSE}
function MonitorFromRect; external user32 name 'MonitorFromRect';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _MonitorFromWindow: Pointer;

function MonitorFromWindow;
begin
  GetProcedureAddress(_MonitorFromWindow, user32, 'MonitorFromWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_MonitorFromWindow]
  end;
end;
{$ELSE}
function MonitorFromWindow; external user32 name 'MonitorFromWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMonitorInfoA: Pointer;

function GetMonitorInfoA;
begin
  GetProcedureAddress(_GetMonitorInfoA, user32, 'GetMonitorInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMonitorInfoA]
  end;
end;
{$ELSE}
function GetMonitorInfoA; external user32 name 'GetMonitorInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMonitorInfoW: Pointer;

function GetMonitorInfoW;
begin
  GetProcedureAddress(_GetMonitorInfoW, user32, 'GetMonitorInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMonitorInfoW]
  end;
end;
{$ELSE}
function GetMonitorInfoW; external user32 name 'GetMonitorInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMonitorInfo: Pointer;

function GetMonitorInfo;
begin
  GetProcedureAddress(_GetMonitorInfo, user32, 'GetMonitorInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMonitorInfo]
  end;
end;
{$ELSE}
function GetMonitorInfo; external user32 name 'GetMonitorInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetMonitorInfo: Pointer;

function GetMonitorInfo;
begin
  GetProcedureAddress(_GetMonitorInfo, user32, 'GetMonitorInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMonitorInfo]
  end;
end;
{$ELSE}
function GetMonitorInfo; external user32 name 'GetMonitorInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _EnumDisplayMonitors: Pointer;

function EnumDisplayMonitors;
begin
  GetProcedureAddress(_EnumDisplayMonitors, user32, 'EnumDisplayMonitors');
  asm
    mov esp, ebp
    pop ebp
    jmp [_EnumDisplayMonitors]
  end;
end;
{$ELSE}
function EnumDisplayMonitors; external user32 name 'EnumDisplayMonitors';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _NotifyWinEvent: Pointer;

procedure NotifyWinEvent;
begin
  GetProcedureAddress(_NotifyWinEvent, user32, 'NotifyWinEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_NotifyWinEvent]
  end;
end;
{$ELSE}
procedure NotifyWinEvent; external user32 name 'NotifyWinEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetWinEventHook: Pointer;

function SetWinEventHook;
begin
  GetProcedureAddress(_SetWinEventHook, user32, 'SetWinEventHook');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetWinEventHook]
  end;
end;
{$ELSE}
function SetWinEventHook; external user32 name 'SetWinEventHook';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _IsWinEventHookInstalled: Pointer;

function IsWinEventHookInstalled;
begin
  GetProcedureAddress(_IsWinEventHookInstalled, user32, 'IsWinEventHookInstalled');
  asm
    mov esp, ebp
    pop ebp
    jmp [_IsWinEventHookInstalled]
  end;
end;
{$ELSE}
function IsWinEventHookInstalled; external user32 name 'IsWinEventHookInstalled';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UnhookWinEvent: Pointer;

function UnhookWinEvent;
begin
  GetProcedureAddress(_UnhookWinEvent, user32, 'UnhookWinEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UnhookWinEvent]
  end;
end;
{$ELSE}
function UnhookWinEvent; external user32 name 'UnhookWinEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetGUIThreadInfo: Pointer;

function GetGUIThreadInfo;
begin
  GetProcedureAddress(_GetGUIThreadInfo, user32, 'GetGUIThreadInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetGUIThreadInfo]
  end;
end;
{$ELSE}
function GetGUIThreadInfo; external user32 name 'GetGUIThreadInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowModuleFileNameA: Pointer;

function GetWindowModuleFileNameA;
begin
  GetProcedureAddress(_GetWindowModuleFileNameA, user32, 'GetWindowModuleFileNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowModuleFileNameA]
  end;
end;
{$ELSE}
function GetWindowModuleFileNameA; external user32 name 'GetWindowModuleFileNameA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowModuleFileNameW: Pointer;

function GetWindowModuleFileNameW;
begin
  GetProcedureAddress(_GetWindowModuleFileNameW, user32, 'GetWindowModuleFileNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowModuleFileNameW]
  end;
end;
{$ELSE}
function GetWindowModuleFileNameW; external user32 name 'GetWindowModuleFileNameW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowModuleFileName: Pointer;

function GetWindowModuleFileName;
begin
  GetProcedureAddress(_GetWindowModuleFileName, user32, 'GetWindowModuleFileNameW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowModuleFileName]
  end;
end;
{$ELSE}
function GetWindowModuleFileName; external user32 name 'GetWindowModuleFileNameW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowModuleFileName: Pointer;

function GetWindowModuleFileName;
begin
  GetProcedureAddress(_GetWindowModuleFileName, user32, 'GetWindowModuleFileNameA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowModuleFileName]
  end;
end;
{$ELSE}
function GetWindowModuleFileName; external user32 name 'GetWindowModuleFileNameA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetCursorInfo: Pointer;

function GetCursorInfo;
begin
  GetProcedureAddress(_GetCursorInfo, user32, 'GetCursorInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCursorInfo]
  end;
end;
{$ELSE}
function GetCursorInfo; external user32 name 'GetCursorInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetWindowInfo: Pointer;

function GetWindowInfo;
begin
  GetProcedureAddress(_GetWindowInfo, user32, 'GetWindowInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetWindowInfo]
  end;
end;
{$ELSE}
function GetWindowInfo; external user32 name 'GetWindowInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetTitleBarInfo: Pointer;

function GetTitleBarInfo;
begin
  GetProcedureAddress(_GetTitleBarInfo, user32, 'GetTitleBarInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetTitleBarInfo]
  end;
end;
{$ELSE}
function GetTitleBarInfo; external user32 name 'GetTitleBarInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetMenuBarInfo: Pointer;

function GetMenuBarInfo;
begin
  GetProcedureAddress(_GetMenuBarInfo, user32, 'GetMenuBarInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetMenuBarInfo]
  end;
end;
{$ELSE}
function GetMenuBarInfo; external user32 name 'GetMenuBarInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetScrollBarInfo: Pointer;

function GetScrollBarInfo;
begin
  GetProcedureAddress(_GetScrollBarInfo, user32, 'GetScrollBarInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetScrollBarInfo]
  end;
end;
{$ELSE}
function GetScrollBarInfo; external user32 name 'GetScrollBarInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetComboBoxInfo: Pointer;

function GetComboBoxInfo;
begin
  GetProcedureAddress(_GetComboBoxInfo, user32, 'GetComboBoxInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetComboBoxInfo]
  end;
end;
{$ELSE}
function GetComboBoxInfo; external user32 name 'GetComboBoxInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAncestor: Pointer;

function GetAncestor;
begin
  GetProcedureAddress(_GetAncestor, user32, 'GetAncestor');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAncestor]
  end;
end;
{$ELSE}
function GetAncestor; external user32 name 'GetAncestor';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RealChildWindowFromPoint: Pointer;

function RealChildWindowFromPoint;
begin
  GetProcedureAddress(_RealChildWindowFromPoint, user32, 'RealChildWindowFromPoint');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RealChildWindowFromPoint]
  end;
end;
{$ELSE}
function RealChildWindowFromPoint; external user32 name 'RealChildWindowFromPoint';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RealGetWindowClassA: Pointer;

function RealGetWindowClassA;
begin
  GetProcedureAddress(_RealGetWindowClassA, user32, 'RealGetWindowClassA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RealGetWindowClassA]
  end;
end;
{$ELSE}
function RealGetWindowClassA; external user32 name 'RealGetWindowClassA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _RealGetWindowClassW: Pointer;

function RealGetWindowClassW;
begin
  GetProcedureAddress(_RealGetWindowClassW, user32, 'RealGetWindowClassW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RealGetWindowClassW]
  end;
end;
{$ELSE}
function RealGetWindowClassW; external user32 name 'RealGetWindowClassW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _RealGetWindowClass: Pointer;

function RealGetWindowClass;
begin
  GetProcedureAddress(_RealGetWindowClass, user32, 'RealGetWindowClassW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RealGetWindowClass]
  end;
end;
{$ELSE}
function RealGetWindowClass; external user32 name 'RealGetWindowClassW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _RealGetWindowClass: Pointer;

function RealGetWindowClass;
begin
  GetProcedureAddress(_RealGetWindowClass, user32, 'RealGetWindowClassA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RealGetWindowClass]
  end;
end;
{$ELSE}
function RealGetWindowClass; external user32 name 'RealGetWindowClassA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetAltTabInfoA: Pointer;

function GetAltTabInfoA;
begin
  GetProcedureAddress(_GetAltTabInfoA, user32, 'GetAltTabInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAltTabInfoA]
  end;
end;
{$ELSE}
function GetAltTabInfoA; external user32 name 'GetAltTabInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetAltTabInfoW: Pointer;

function GetAltTabInfoW;
begin
  GetProcedureAddress(_GetAltTabInfoW, user32, 'GetAltTabInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAltTabInfoW]
  end;
end;
{$ELSE}
function GetAltTabInfoW; external user32 name 'GetAltTabInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetAltTabInfo: Pointer;

function GetAltTabInfo;
begin
  GetProcedureAddress(_GetAltTabInfo, user32, 'GetAltTabInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAltTabInfo]
  end;
end;
{$ELSE}
function GetAltTabInfo; external user32 name 'GetAltTabInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetAltTabInfo: Pointer;

function GetAltTabInfo;
begin
  GetProcedureAddress(_GetAltTabInfo, user32, 'GetAltTabInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetAltTabInfo]
  end;
end;
{$ELSE}
function GetAltTabInfo; external user32 name 'GetAltTabInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetListBoxInfo: Pointer;

function GetListBoxInfo;
begin
  GetProcedureAddress(_GetListBoxInfo, user32, 'GetListBoxInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetListBoxInfo]
  end;
end;
{$ELSE}
function GetListBoxInfo; external user32 name 'GetListBoxInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _LockWorkStation: Pointer;

function LockWorkStation;
begin
  GetProcedureAddress(_LockWorkStation, user32, 'LockWorkStation');
  asm
    mov esp, ebp
    pop ebp
    jmp [_LockWorkStation]
  end;
end;
{$ELSE}
function LockWorkStation; external user32 name 'LockWorkStation';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _UserHandleGrantAccess: Pointer;

function UserHandleGrantAccess;
begin
  GetProcedureAddress(_UserHandleGrantAccess, user32, 'UserHandleGrantAccess');
  asm
    mov esp, ebp
    pop ebp
    jmp [_UserHandleGrantAccess]
  end;
end;
{$ELSE}
function UserHandleGrantAccess; external user32 name 'UserHandleGrantAccess';
{$ENDIF DYNAMIC_LINK}

function GET_RAWINPUT_CODE_WPARAM(wParam: WPARAM): DWORD;
begin
  Result := wParam and $ff;
end;

function RAWINPUT_ALIGN(x: Pointer): Pointer;
begin
  Result := Pointer((Integer(x) + SizeOf(DWORD) - 1) and not (SizeOf(DWORD) - 1));
end;

function NEXTRAWINPUTBLOCK(ptr: PRawInput): PRawInput;
begin
  Result := PRAWINPUT(DWORD(RAWINPUT_ALIGN(ptr)) + ptr^.header.dwSize);
end;


{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputData: Pointer;

function GetRawInputData;
begin
  GetProcedureAddress(_GetRawInputData, user32, 'GetRawInputData');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputData]
  end;
end;
{$ELSE}
function GetRawInputData; external user32 name 'GetRawInputData';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfoA: Pointer;

function GetRawInputDeviceInfoA;
begin
  GetProcedureAddress(_GetRawInputDeviceInfoA, user32, 'GetRawInputDeviceInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfoA]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfoA; external user32 name 'GetRawInputDeviceInfoA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfoW: Pointer;

function GetRawInputDeviceInfoW;
begin
  GetProcedureAddress(_GetRawInputDeviceInfoW, user32, 'GetRawInputDeviceInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfoW]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfoW; external user32 name 'GetRawInputDeviceInfoW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfo: Pointer;

function GetRawInputDeviceInfo;
begin
  GetProcedureAddress(_GetRawInputDeviceInfo, user32, 'GetRawInputDeviceInfoW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfo]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfo; external user32 name 'GetRawInputDeviceInfoW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceInfo: Pointer;

function GetRawInputDeviceInfo;
begin
  GetProcedureAddress(_GetRawInputDeviceInfo, user32, 'GetRawInputDeviceInfoA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceInfo]
  end;
end;
{$ELSE}
function GetRawInputDeviceInfo; external user32 name 'GetRawInputDeviceInfoA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputBuffer: Pointer;

function GetRawInputBuffer;
begin
  GetProcedureAddress(_GetRawInputBuffer, user32, 'GetRawInputBuffer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputBuffer]
  end;
end;
{$ELSE}
function GetRawInputBuffer; external user32 name 'GetRawInputBuffer';
{$ENDIF DYNAMIC_LINK}

function RIDEV_EXMODE(mode: DWORD): DWORD;
begin
  Result := mode and RIDEV_EXMODEMASK;
end;


{$IFDEF DYNAMIC_LINK}
var
  _RegisterRawInputDevices: Pointer;

function RegisterRawInputDevices;
begin
  GetProcedureAddress(_RegisterRawInputDevices, user32, 'RegisterRawInputDevices');
  asm
    mov esp, ebp
    pop ebp
    jmp [_RegisterRawInputDevices]
  end;
end;
{$ELSE}
function RegisterRawInputDevices; external user32 name 'RegisterRawInputDevices';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRegisteredRawInputDevices: Pointer;

function GetRegisteredRawInputDevices;
begin
  GetProcedureAddress(_GetRegisteredRawInputDevices, user32, 'GetRegisteredRawInputDevices');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRegisteredRawInputDevices]
  end;
end;
{$ELSE}
function GetRegisteredRawInputDevices; external user32 name 'GetRegisteredRawInputDevices';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetRawInputDeviceList: Pointer;

function GetRawInputDeviceList;
begin
  GetProcedureAddress(_GetRawInputDeviceList, user32, 'GetRawInputDeviceList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetRawInputDeviceList]
  end;
end;
{$ELSE}
function GetRawInputDeviceList; external user32 name 'GetRawInputDeviceList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _DefRawInputProc: Pointer;

function DefRawInputProc;
begin
  GetProcedureAddress(_DefRawInputProc, user32, 'DefRawInputProc');
  asm
    mov esp, ebp
    pop ebp
    jmp [_DefRawInputProc]
  end;
end;
{$ELSE}
function DefRawInputProc; external user32 name 'DefRawInputProc';
{$ENDIF DYNAMIC_LINK}

end.

