{******************************************************************************}
{                                                       	               }
{ Console Applications API interface Unit for Object Pascal                    }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: wincon.h, released June 2000. The original Pascal      }
{ code is: WinCon.pas, released December 2000. The initial developer of the    }
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

unit JwaWinCon;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinCon.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinBase, JwaWinType;

type
  PCOORD = ^COORD;
  _COORD = record
    X: SHORT;
    Y: SHORT;
  end;
  COORD = _COORD;
  TCoord = _COORD;

  PSMALL_RECT = ^SMALL_RECT;
  _SMALL_RECT = record
    Left: SHORT;
    Top: SHORT;
    Right: SHORT;
    Bottom: SHORT;
  end;
  SMALL_RECT = _SMALL_RECT;
  TSmallRect = SMALL_RECT;
  PSmallRect = PSMALL_RECT;

  TCharUnion = record
    case Integer of
      0: (UnicodeChar: WCHAR);
      1: (AsciiChar: CHAR);
  end;

  PKEY_EVENT_RECORD = ^KEY_EVENT_RECORD;
  _KEY_EVENT_RECORD = record
    bKeyDown: BOOL;
    wRepeatCount: WORD;
    wVirtualKeyCode: WORD;
    wVirtualScanCode: WORD;
    uChar: TCharUnion;
    dwControlKeyState: DWORD;
  end;
  KEY_EVENT_RECORD = _KEY_EVENT_RECORD;
  TKeyEventRecord = KEY_EVENT_RECORD;
  PKeyEventRecord = PKEY_EVENT_RECORD;

//
// ControlKeyState flags
//

const
  RIGHT_ALT_PRESSED  = $0001; // the right alt key is pressed.
  LEFT_ALT_PRESSED   = $0002; // the left alt key is pressed.
  RIGHT_CTRL_PRESSED = $0004; // the right ctrl key is pressed.
  LEFT_CTRL_PRESSED  = $0008; // the left ctrl key is pressed.
  SHIFT_PRESSED      = $0010; // the shift key is pressed.
  NUMLOCK_ON         = $0020; // the numlock light is on.
  SCROLLLOCK_ON      = $0040; // the scrolllock light is on.
  CAPSLOCK_ON        = $0080; // the capslock light is on.
  ENHANCED_KEY       = $0100; // the key is enhanced.
  NLS_DBCSCHAR       = $00010000; // DBCS for JPN: SBCS/DBCS mode.
  NLS_ALPHANUMERIC   = $00000000; // DBCS for JPN: Alphanumeric mode.
  NLS_KATAKANA       = $00020000; // DBCS for JPN: Katakana mode.
  NLS_HIRAGANA       = $00040000; // DBCS for JPN: Hiragana mode.
  NLS_ROMAN          = $00400000; // DBCS for JPN: Roman/Noroman mode.
  NLS_IME_CONVERSION = $00800000; // DBCS for JPN: IME conversion.
  NLS_IME_DISABLE    = $20000000; // DBCS for JPN: IME enable/disable.

type
  PMOUSE_EVENT_RECORD = ^MOUSE_EVENT_RECORD;
  _MOUSE_EVENT_RECORD = record
    dwMousePosition: COORD;
    dwButtonState: DWORD;
    dwControlKeyState: DWORD;
    dwEventFlags: DWORD;
  end;
  MOUSE_EVENT_RECORD = _MOUSE_EVENT_RECORD;
  TMouseEventRecord = MOUSE_EVENT_RECORD;
  PMouseEventRecord = PMOUSE_EVENT_RECORD;

//
// ButtonState flags
//

const
  FROM_LEFT_1ST_BUTTON_PRESSED = $0001;
  RIGHTMOST_BUTTON_PRESSED     = $0002;
  FROM_LEFT_2ND_BUTTON_PRESSED = $0004;
  FROM_LEFT_3RD_BUTTON_PRESSED = $0008;
  FROM_LEFT_4TH_BUTTON_PRESSED = $0010;

//
// EventFlags
//

  MOUSE_MOVED   = $0001;
  DOUBLE_CLICK  = $0002;
  MOUSE_WHEELED = $0004;

type
  PWINDOW_BUFFER_SIZE_RECORD = ^WINDOW_BUFFER_SIZE_RECORD;
  _WINDOW_BUFFER_SIZE_RECORD = record
    dwSize: COORD;
  end;
  WINDOW_BUFFER_SIZE_RECORD = _WINDOW_BUFFER_SIZE_RECORD;
  TWindowBufferSizeRecord = WINDOW_BUFFER_SIZE_RECORD;
  PWindowBufferSizeRecord = PWINDOW_BUFFER_SIZE_RECORD;

  PMENU_EVENT_RECORD = ^MENU_EVENT_RECORD;
  _MENU_EVENT_RECORD = record
    dwCommandId: UINT;
  end;
  MENU_EVENT_RECORD = _MENU_EVENT_RECORD;
  TMenuEventRecord = MENU_EVENT_RECORD;
  PMenuEventRecord = PMENU_EVENT_RECORD;

  PFOCUS_EVENT_RECORD = ^FOCUS_EVENT_RECORD;
  _FOCUS_EVENT_RECORD = record
    bSetFocus: BOOL;
  end;
  FOCUS_EVENT_RECORD = _FOCUS_EVENT_RECORD;
  TFocusEventRecord = FOCUS_EVENT_RECORD;
  PFocusEventRecord = PFOCUS_EVENT_RECORD;

  PINPUT_RECORD = ^INPUT_RECORD;
  _INPUT_RECORD = record
    EventType: WORD;
    case Integer of
      0: (KeyEvent: KEY_EVENT_RECORD);
      1: (MouseEvent: MOUSE_EVENT_RECORD);
      2: (WindowBufferSizeEvent: WINDOW_BUFFER_SIZE_RECORD);
      3: (MenuEvent: MENU_EVENT_RECORD);
      4: (FocusEvent: FOCUS_EVENT_RECORD);
  end;
  INPUT_RECORD = _INPUT_RECORD;
  TInputRecord = INPUT_RECORD;
  PInputRecord = PINPUT_RECORD;

//
//  EventType flags:
//

const
  KEY_EVENT                = $0001; // Event contains key event record
  MOUSE_EVENT              = $0002; // Event contains mouse event record
  WINDOW_BUFFER_SIZE_EVENT = $0004; // Event contains window change event record
  MENU_EVENT               = $0008; // Event contains menu event record
  FOCUS_EVENT              = $0010; // event contains focus change

type
  PCHAR_INFO = ^CHAR_INFO;
  _CHAR_INFO = record
    uChar: TCharUnion;
    Attributes: WORD;
  end;
  CHAR_INFO = _CHAR_INFO;
  TCharInfo = CHAR_INFO;
  PCharInfo = PCHAR_INFO;

//
// Attributes flags:
//

const
  FOREGROUND_BLUE            = $0001; // text color contains blue.
  FOREGROUND_GREEN           = $0002; // text color contains green.
  FOREGROUND_RED             = $0004; // text color contains red.
  FOREGROUND_INTENSITY       = $0008; // text color is intensified.
  BACKGROUND_BLUE            = $0010; // background color contains blue.
  BACKGROUND_GREEN           = $0020; // background color contains green.
  BACKGROUND_RED             = $0040; // background color contains red.
  BACKGROUND_INTENSITY       = $0080; // background color is intensified.
  COMMON_LVB_LEADING_BYTE    = $0100; // Leading Byte of DBCS
  COMMON_LVB_TRAILING_BYTE   = $0200; // Trailing Byte of DBCS
  COMMON_LVB_GRID_HORIZONTAL = $0400; // DBCS: Grid attribute: top horizontal.
  COMMON_LVB_GRID_LVERTICAL  = $0800; // DBCS: Grid attribute: left vertical.
  COMMON_LVB_GRID_RVERTICAL  = $1000; // DBCS: Grid attribute: right vertical.
  COMMON_LVB_REVERSE_VIDEO   = $4000; // DBCS: Reverse fore/back ground attribute.
  COMMON_LVB_UNDERSCORE      = $8000; // DBCS: Underscore.

  COMMON_LVB_SBCSDBCS        = $0300; // SBCS or DBCS flag.

type
  PCONSOLE_SCREEN_BUFFER_INFO = ^CONSOLE_SCREEN_BUFFER_INFO;
  _CONSOLE_SCREEN_BUFFER_INFO = record
    dwSize: COORD;
    dwCursorPosition: COORD;
    wAttributes: WORD;
    srWindow: SMALL_RECT;
    dwMaximumWindowSize: COORD;
  end;
  CONSOLE_SCREEN_BUFFER_INFO = _CONSOLE_SCREEN_BUFFER_INFO;
  TConsoleScreenBufferInfo = CONSOLE_SCREEN_BUFFER_INFO;
  PConsoleScreenBufferInfo = PCONSOLE_SCREEN_BUFFER_INFO;

  PCONSOLE_CURSOR_INFO = ^CONSOLE_CURSOR_INFO;
  _CONSOLE_CURSOR_INFO = record
    dwSize: DWORD;
    bVisible: BOOL;
  end;
  CONSOLE_CURSOR_INFO = _CONSOLE_CURSOR_INFO;
  TConsoleCursorInfo = CONSOLE_CURSOR_INFO;
  PConsoleCursorInfo = PCONSOLE_CURSOR_INFO;

  _CONSOLE_FONT_INFO = record
    nFont: DWORD;
    dwFontSize: COORD;
  end;
  CONSOLE_FONT_INFO = _CONSOLE_FONT_INFO;
  PCONSOLE_FONT_INFO = ^CONSOLE_FONT_INFO;
  TConsoleFontInfo = CONSOLE_FONT_INFO;
  PConsoleFontInfo = PCONSOLE_FONT_INFO;

  _CONSOLE_SELECTION_INFO = record
    dwFlags: DWORD;
    dwSelectionAnchor: COORD;
    srSelection: SMALL_RECT;
  end;
  CONSOLE_SELECTION_INFO = _CONSOLE_SELECTION_INFO;
  PCONSOLE_SELECTION_INFO = ^CONSOLE_SELECTION_INFO;
  TConsoleSelectionInfo = CONSOLE_SELECTION_INFO;
  PConsoleSelectionInfo = PCONSOLE_SELECTION_INFO;  

//
// Selection flags
//

const
  CONSOLE_NO_SELECTION          = $0000;
  CONSOLE_SELECTION_IN_PROGRESS = $0001;   // selection has begun
  CONSOLE_SELECTION_NOT_EMPTY   = $0002;   // non-null select rectangle
  CONSOLE_MOUSE_SELECTION       = $0004;   // selecting with mouse
  CONSOLE_MOUSE_DOWN            = $0008;   // mouse is down

//
// typedef for ctrl-c handler routines
//

type
  PHANDLER_ROUTINE = function (CtrlType: DWORD): BOOL; stdcall;
  THandlerRoutine = PHANDLER_ROUTINE;

const
  CTRL_C_EVENT        = 0;
  CTRL_BREAK_EVENT    = 1;
  CTRL_CLOSE_EVENT    = 2;
  // 3 is reserved!
  // 4 is reserved!
  CTRL_LOGOFF_EVENT   = 5;
  CTRL_SHUTDOWN_EVENT = 6;

//
//  Input Mode flags:
//

  ENABLE_PROCESSED_INPUT = $0001;
  ENABLE_LINE_INPUT      = $0002;
  ENABLE_ECHO_INPUT      = $0004;
  ENABLE_WINDOW_INPUT    = $0008;
  ENABLE_MOUSE_INPUT     = $0010;

//
// Output Mode flags:
//

  ENABLE_PROCESSED_OUTPUT   = $0001;
  ENABLE_WRAP_AT_EOL_OUTPUT = $0002;

//
// direct API definitions.
//

function PeekConsoleInputA(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
function PeekConsoleInputW(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function PeekConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$ELSE}
function PeekConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$ENDIF}

function ReadConsoleInputA(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
function ReadConsoleInputW(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function ReadConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$ELSE}
function ReadConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsRead: DWORD): BOOL; stdcall;
{$ENDIF}

function WriteConsoleInputA(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;
function WriteConsoleInputW(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function WriteConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;
{$ELSE}
function WriteConsoleInput(hConsoleInput: HANDLE; lpBuffer: PINPUT_RECORD;
  nLength: DWORD; var lpNumberOfEventsWritten: DWORD): BOOL; stdcall;
{$ENDIF}

function ReadConsoleOutputA(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;
function ReadConsoleOutputW(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;

{$IFDEF UNICODE}
function ReadConsoleOutput(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;
{$ELSE}
function ReadConsoleOutput(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpReadRegion: SMALL_RECT): BOOL; stdcall;
{$ENDIF}

function WriteConsoleOutputA(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;
function WriteConsoleOutputW(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;

{$IFDEF UNICODE}
function WriteConsoleOutput(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;
{$ELSE}
function WriteConsoleOutput(hConsoleOutput: HANDLE; lpBuffer: PCHAR_INFO;
  dwBufferSize: COORD; dwBufferCoord: COORD;
  var lpWriteRegion: SMALL_RECT): BOOL; stdcall;
{$ENDIF}

function ReadConsoleOutputCharacterA(hConsoleOutput: HANDLE; lpCharacter: LPSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;
function ReadConsoleOutputCharacterW(hConsoleOutput: HANDLE; lpCharacter: LPWSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function ReadConsoleOutputCharacter(hConsoleOutput: HANDLE; lpCharacter: LPWSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;
{$ELSE}
function ReadConsoleOutputCharacter(hConsoleOutput: HANDLE; lpCharacter: LPSTR;
  nLength: DWORD; dwReadCoord: COORD; var lpNumberOfCharsRead: DWORD): BOOL; stdcall;
{$ENDIF}

function ReadConsoleOutputAttribute(hConsoleOutput: HANDLE;
  var lpAttribute: DWORD; nLength: DWORD; dwReadCoord: COORD;
  var lpNumberOfAttrsRead: DWORD): BOOL; stdcall;

function WriteConsoleOutputCharacterA(hConsoleOutput: HANDLE;
  lpCharacter: LPCSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
function WriteConsoleOutputCharacterW(hConsoleOutput: HANDLE;
  lpCharacter: LPCWSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function WriteConsoleOutputCharacter(hConsoleOutput: HANDLE;
  lpCharacter: LPCWSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$ELSE}
function WriteConsoleOutputCharacter(hConsoleOutput: HANDLE;
  lpCharacter: LPCSTR; nLength: DWORD; dwWriteCoord: COORD;
  var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$ENDIF}

function WriteConsoleOutputAttribute(hConsoleOutput: HANDLE; lpAttribute: PWORD;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfAttrsWritten: DWORD): BOOL; stdcall;

function FillConsoleOutputCharacterA(hConsoleOutput: HANDLE; cCharacter: CHAR;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
function FillConsoleOutputCharacterW(hConsoleOutput: HANDLE; cCharacter: WCHAR;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;

{$IFDEF UNICODE}
function FillConsoleOutputCharacter(hConsoleOutput: HANDLE; cCharacter: WCHAR;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$ELSE}
function FillConsoleOutputCharacter(hConsoleOutput: HANDLE; cCharacter: CHAR;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfCharsWritten: DWORD): BOOL; stdcall;
{$ENDIF}

function FillConsoleOutputAttribute(hConsoleOutput: HANDLE; wAttribute: WORD;
  nLength: DWORD; dwWriteCoord: COORD; var lpNumberOfAttrsWritten: DWORD): BOOL; stdcall;
function GetConsoleMode(hConsoleHandle: HANDLE; var lpMode: DWORD): BOOL; stdcall;
function GetNumberOfConsoleInputEvents(hConsoleInput: HANDLE;
  var lpNumberOfEvents: DWORD): BOOL; stdcall;
function GetConsoleScreenBufferInfo(hConsoleOutput: HANDLE;
  var lpConsoleScreenBufferInfo: CONSOLE_SCREEN_BUFFER_INFO): BOOL; stdcall;
function GetLargestConsoleWindowSize(hConsoleOutput: HANDLE): COORD; stdcall;
function GetConsoleCursorInfo(hConsoleOutput: HANDLE;
  var lpConsoleCursorInfo: CONSOLE_CURSOR_INFO): BOOL; stdcall;
function GetCurrentConsoleFont(hConsoleOutput: HANDLE; bMaximumWindow: BOOL;
  var lpConsoleCurrentFont: CONSOLE_FONT_INFO): BOOL; stdcall;
function GetConsoleFontSize(hConsoleOutput: HANDLE; nFont: DWORD): COORD; stdcall;
function GetConsoleSelectionInfo(var lpConsoleSelectionInfo: CONSOLE_SELECTION_INFO): BOOL; stdcall;
function GetNumberOfConsoleMouseButtons(var lpNumberOfMouseButtons: DWORD): BOOL; stdcall;
function SetConsoleMode(hConsoleHandle: HANDLE; dwMode: DWORD): BOOL; stdcall;
function SetConsoleActiveScreenBuffer(hConsoleOutput: HANDLE): BOOL; stdcall;
function FlushConsoleInputBuffer(hConsoleInput: HANDLE): BOOL; stdcall;
function SetConsoleScreenBufferSize(hConsoleOutput: HANDLE; dwSize: COORD): BOOL; stdcall;
function SetConsoleCursorPosition(hConsoleOutput: HANDLE; dwCursorPosition: COORD): BOOL; stdcall;
function SetConsoleCursorInfo(hConsoleOutput: HANDLE;
  var lpConsoleCursorInfo: CONSOLE_CURSOR_INFO): BOOL; stdcall;

function ScrollConsoleScreenBufferA(hConsoleOutput: HANDLE;
  const lpScrollRectangle: SMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
function ScrollConsoleScreenBufferW(hConsoleOutput: HANDLE;
  const lpScrollRectangle: PSMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
{$IFDEF UNICODE}
function ScrollConsoleScreenBuffer(hConsoleOutput: HANDLE;
  const lpScrollRectangle: PSMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
{$ELSE}
function ScrollConsoleScreenBuffer(hConsoleOutput: HANDLE;
  const lpScrollRectangle: SMALL_RECT; lpClipRectangle: PSMALL_RECT;
  dwDestinationOrigin: COORD; const lpFill: CHAR_INFO): BOOL; stdcall;
{$ENDIF}

function SetConsoleWindowInfo(hConsoleOutput: HANDLE; bAbsolute: BOOL;
  const lpConsoleWindow: SMALL_RECT): BOOL; stdcall;
function SetConsoleTextAttribute(hConsoleOutput: HANDLE; wAttributes: WORD): BOOL; stdcall;
function SetConsoleCtrlHandler(HandlerRoutine: PHANDLER_ROUTINE; Add: BOOL): BOOL; stdcall;
function GenerateConsoleCtrlEvent(dwCtrlEvent: DWORD; dwProcessGroupId: DWORD): BOOL; stdcall;
function AllocConsole: BOOL; stdcall;
function FreeConsole: BOOL; stdcall;
function AttachConsole(dwProcessId: DWORD): BOOL; stdcall;

const
  ATTACH_PARENT_PROCESS = DWORD(-1);

function GetConsoleTitleA(lpConsoleTitle: LPSTR; nSize: DWORD): DWORD; stdcall;
function GetConsoleTitleW(lpConsoleTitle: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$IFDEF UNICODE}
function GetConsoleTitle(lpConsoleTitle: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$ELSE}
function GetConsoleTitle(lpConsoleTitle: LPSTR; nSize: DWORD): DWORD; stdcall;
{$ENDIF}

function SetConsoleTitleA(lpConsoleTitle: LPCSTR): BOOL; stdcall;
function SetConsoleTitleW(lpConsoleTitle: LPCWSTR): BOOL; stdcall;
{$IFDEF UNICODE}
function SetConsoleTitle(lpConsoleTitle: LPCWSTR): BOOL; stdcall;
{$ELSE}
function SetConsoleTitle(lpConsoleTitle: LPCSTR): BOOL; stdcall;
{$ENDIF}

function ReadConsoleA(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
function ReadConsoleW(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$IFDEF UNICODE}
function ReadConsole(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$ELSE}
function ReadConsole(hConsoleInput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToRead: DWORD; var lpNumberOfCharsRead: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$ENDIF}

function WriteConsoleA(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
function WriteConsoleW(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$IFDEF UNICODE}
function WriteConsole(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$ELSE}
function WriteConsole(hConsoleOutput: HANDLE; lpBuffer: LPVOID;
  nNumberOfCharsToWrite: DWORD; var lpNumberOfCharsWritten: DWORD;
  lpReserved: LPVOID): BOOL; stdcall;
{$ENDIF}

const
  CONSOLE_TEXTMODE_BUFFER = 1;

function CreateConsoleScreenBuffer(dwDesiredAccess: DWORD; dwShareMode: DWORD;
  lpSecurityAttributes: PSECURITY_ATTRIBUTES; dwFlags: DWORD;
  lpScreenBufferData: LPVOID): HANDLE; stdcall;
function GetConsoleCP: UINT; stdcall;
function SetConsoleCP(wCodePageID: UINT): BOOL; stdcall;
function GetConsoleOutputCP: UINT; stdcall;
function SetConsoleOutputCP(wCodePageID: UINT): BOOL; stdcall;

const
  CONSOLE_FULLSCREEN = 1;            // fullscreen console
  CONSOLE_FULLSCREEN_HARDWARE = 2;   // console owns the hardware

function GetConsoleDisplayMode(var lpModeFlags: DWORD): BOOL; stdcall;

function GetConsoleWindow: HWND; stdcall;

function GetConsoleProcessList(var lpdwProcessList: LPDWORD; dwProcessCount: DWORD): DWORD; stdcall;

//
// Aliasing apis.
//

function AddConsoleAliasA(Source, Target, ExeName: LPSTR): BOOL; stdcall;
function AddConsoleAliasW(Source, Target, ExeName: LPWSTR): BOOL; stdcall;

{$IFDEF UNICODE}
function AddConsoleAlias(Source, Target, ExeName: LPWSTR): BOOL; stdcall;
{$ELSE}
function AddConsoleAlias(Source, Target, ExeName: LPSTR): BOOL; stdcall;
{$ENDIF}

function GetConsoleAliasA(Source, TargetBuffer: LPSTR; TargetBufferLength: DWORD; ExeName: LPSTR): DWORD; stdcall;
function GetConsoleAliasW(Source, TargetBuffer: LPWSTR; TargetBufferLength: DWORD; ExeName: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetConsoleAlias(Source, TargetBuffer: LPWSTR; TargetBufferLength: DWORD; ExeName: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetConsoleAlias(Source, TargetBuffer: LPSTR; TargetBufferLength: DWORD; ExeName: LPSTR): DWORD; stdcall;
{$ENDIF}

function GetConsoleAliasesLengthA(ExeName: LPSTR): DWORD; stdcall;
function GetConsoleAliasesLengthW(ExeName: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetConsoleAliasesLength(ExeName: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetConsoleAliasesLength(ExeName: LPSTR): DWORD; stdcall;
{$ENDIF}

function GetConsoleAliasExesLengthA: DWORD; stdcall;
function GetConsoleAliasExesLengthW: DWORD; stdcall;

{$IFDEF UNICODE}
function GetConsoleAliasExesLength: DWORD; stdcall;
{$ELSE}
function GetConsoleAliasExesLength: DWORD; stdcall;
{$ENDIF}

function GetConsoleAliasesA(AliasBuffer: LPSTR; AliasBufferLength: DWORD; ExeName: LPSTR): DWORD; stdcall;
function GetConsoleAliasesW(AliasBuffer: LPWSTR; AliasBufferLength: DWORD; ExeName: LPWSTR): DWORD; stdcall;

{$IFDEF UNICODE}
function GetConsoleAliases(AliasBuffer: LPWSTR; AliasBufferLength: DWORD; ExeName: LPWSTR): DWORD; stdcall;
{$ELSE}
function GetConsoleAliases(AliasBuffer: LPSTR; AliasBufferLength: DWORD; ExeName: LPSTR): DWORD; stdcall;
{$ENDIF}

function GetConsoleAliasExesA(ExeNameBuffer: LPSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;
function GetConsoleAliasExesW(ExeNameBuffer: LPWSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;

{$IFDEF UNICODE}
function GetConsoleAliasExes(ExeNameBuffer: LPWSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;
{$ELSE}
function GetConsoleAliasExes(ExeNameBuffer: LPSTR; ExeNameBufferLength: DWORD): DWORD; stdcall;
{$ENDIF}

implementation

const
  kernel32 = 'kernel32.dll';


{$IFDEF DYNAMIC_LINK}
var
  _PeekConsoleInputA: Pointer;

function PeekConsoleInputA;
begin
  GetProcedureAddress(_PeekConsoleInputA, kernel32, 'PeekConsoleInputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekConsoleInputA]
  end;
end;
{$ELSE}
function PeekConsoleInputA; external kernel32 name 'PeekConsoleInputA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _PeekConsoleInputW: Pointer;

function PeekConsoleInputW;
begin
  GetProcedureAddress(_PeekConsoleInputW, kernel32, 'PeekConsoleInputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekConsoleInputW]
  end;
end;
{$ELSE}
function PeekConsoleInputW; external kernel32 name 'PeekConsoleInputW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _PeekConsoleInput: Pointer;

function PeekConsoleInput;
begin
  GetProcedureAddress(_PeekConsoleInput, kernel32, 'PeekConsoleInputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekConsoleInput]
  end;
end;
{$ELSE}
function PeekConsoleInput; external kernel32 name 'PeekConsoleInputW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _PeekConsoleInput: Pointer;

function PeekConsoleInput;
begin
  GetProcedureAddress(_PeekConsoleInput, kernel32, 'PeekConsoleInputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_PeekConsoleInput]
  end;
end;
{$ELSE}
function PeekConsoleInput; external kernel32 name 'PeekConsoleInputA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleInputA: Pointer;

function ReadConsoleInputA;
begin
  GetProcedureAddress(_ReadConsoleInputA, kernel32, 'ReadConsoleInputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleInputA]
  end;
end;
{$ELSE}
function ReadConsoleInputA; external kernel32 name 'ReadConsoleInputA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleInputW: Pointer;

function ReadConsoleInputW;
begin
  GetProcedureAddress(_ReadConsoleInputW, kernel32, 'ReadConsoleInputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleInputW]
  end;
end;
{$ELSE}
function ReadConsoleInputW; external kernel32 name 'ReadConsoleInputW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleInput: Pointer;

function ReadConsoleInput;
begin
  GetProcedureAddress(_ReadConsoleInput, kernel32, 'ReadConsoleInputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleInput]
  end;
end;
{$ELSE}
function ReadConsoleInput; external kernel32 name 'ReadConsoleInputW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleInput: Pointer;

function ReadConsoleInput;
begin
  GetProcedureAddress(_ReadConsoleInput, kernel32, 'ReadConsoleInputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleInput]
  end;
end;
{$ELSE}
function ReadConsoleInput; external kernel32 name 'ReadConsoleInputA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleInputA: Pointer;

function WriteConsoleInputA;
begin
  GetProcedureAddress(_WriteConsoleInputA, kernel32, 'WriteConsoleInputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleInputA]
  end;
end;
{$ELSE}
function WriteConsoleInputA; external kernel32 name 'WriteConsoleInputA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleInputW: Pointer;

function WriteConsoleInputW;
begin
  GetProcedureAddress(_WriteConsoleInputW, kernel32, 'WriteConsoleInputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleInputW]
  end;
end;
{$ELSE}
function WriteConsoleInputW; external kernel32 name 'WriteConsoleInputW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleInput: Pointer;

function WriteConsoleInput;
begin
  GetProcedureAddress(_WriteConsoleInput, kernel32, 'WriteConsoleInputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleInput]
  end;
end;
{$ELSE}
function WriteConsoleInput; external kernel32 name 'WriteConsoleInputW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleInput: Pointer;

function WriteConsoleInput;
begin
  GetProcedureAddress(_WriteConsoleInput, kernel32, 'WriteConsoleInputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleInput]
  end;
end;
{$ELSE}
function WriteConsoleInput; external kernel32 name 'WriteConsoleInputA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputA: Pointer;

function ReadConsoleOutputA;
begin
  GetProcedureAddress(_ReadConsoleOutputA, kernel32, 'ReadConsoleOutputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputA]
  end;
end;
{$ELSE}
function ReadConsoleOutputA; external kernel32 name 'ReadConsoleOutputA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputW: Pointer;

function ReadConsoleOutputW;
begin
  GetProcedureAddress(_ReadConsoleOutputW, kernel32, 'ReadConsoleOutputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputW]
  end;
end;
{$ELSE}
function ReadConsoleOutputW; external kernel32 name 'ReadConsoleOutputW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutput: Pointer;

function ReadConsoleOutput;
begin
  GetProcedureAddress(_ReadConsoleOutput, kernel32, 'ReadConsoleOutputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutput]
  end;
end;
{$ELSE}
function ReadConsoleOutput; external kernel32 name 'ReadConsoleOutputW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutput: Pointer;

function ReadConsoleOutput;
begin
  GetProcedureAddress(_ReadConsoleOutput, kernel32, 'ReadConsoleOutputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutput]
  end;
end;
{$ELSE}
function ReadConsoleOutput; external kernel32 name 'ReadConsoleOutputA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputA: Pointer;

function WriteConsoleOutputA;
begin
  GetProcedureAddress(_WriteConsoleOutputA, kernel32, 'WriteConsoleOutputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputA]
  end;
end;
{$ELSE}
function WriteConsoleOutputA; external kernel32 name 'WriteConsoleOutputA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputW: Pointer;

function WriteConsoleOutputW;
begin
  GetProcedureAddress(_WriteConsoleOutputW, kernel32, 'WriteConsoleOutputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputW]
  end;
end;
{$ELSE}
function WriteConsoleOutputW; external kernel32 name 'WriteConsoleOutputW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutput: Pointer;

function WriteConsoleOutput;
begin
  GetProcedureAddress(_WriteConsoleOutput, kernel32, 'WriteConsoleOutputW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutput]
  end;
end;
{$ELSE}
function WriteConsoleOutput; external kernel32 name 'WriteConsoleOutputW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutput: Pointer;

function WriteConsoleOutput;
begin
  GetProcedureAddress(_WriteConsoleOutput, kernel32, 'WriteConsoleOutputA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutput]
  end;
end;
{$ELSE}
function WriteConsoleOutput; external kernel32 name 'WriteConsoleOutputA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputCharacterA: Pointer;

function ReadConsoleOutputCharacterA;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacterA, kernel32, 'ReadConsoleOutputCharacterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputCharacterA]
  end;
end;
{$ELSE}
function ReadConsoleOutputCharacterA; external kernel32 name 'ReadConsoleOutputCharacterA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputCharacterW: Pointer;

function ReadConsoleOutputCharacterW;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacterW, kernel32, 'ReadConsoleOutputCharacterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputCharacterW]
  end;
end;
{$ELSE}
function ReadConsoleOutputCharacterW; external kernel32 name 'ReadConsoleOutputCharacterW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputCharacter: Pointer;

function ReadConsoleOutputCharacter;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacter, kernel32, 'ReadConsoleOutputCharacterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputCharacter]
  end;
end;
{$ELSE}
function ReadConsoleOutputCharacter; external kernel32 name 'ReadConsoleOutputCharacterW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputCharacter: Pointer;

function ReadConsoleOutputCharacter;
begin
  GetProcedureAddress(_ReadConsoleOutputCharacter, kernel32, 'ReadConsoleOutputCharacterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputCharacter]
  end;
end;
{$ELSE}
function ReadConsoleOutputCharacter; external kernel32 name 'ReadConsoleOutputCharacterA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleOutputAttribute: Pointer;

function ReadConsoleOutputAttribute;
begin
  GetProcedureAddress(_ReadConsoleOutputAttribute, kernel32, 'ReadConsoleOutputAttribute');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleOutputAttribute]
  end;
end;
{$ELSE}
function ReadConsoleOutputAttribute; external kernel32 name 'ReadConsoleOutputAttribute';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputCharacterA: Pointer;

function WriteConsoleOutputCharacterA;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacterA, kernel32, 'WriteConsoleOutputCharacterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputCharacterA]
  end;
end;
{$ELSE}
function WriteConsoleOutputCharacterA; external kernel32 name 'WriteConsoleOutputCharacterA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputCharacterW: Pointer;

function WriteConsoleOutputCharacterW;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacterW, kernel32, 'WriteConsoleOutputCharacterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputCharacterW]
  end;
end;
{$ELSE}
function WriteConsoleOutputCharacterW; external kernel32 name 'WriteConsoleOutputCharacterW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputCharacter: Pointer;

function WriteConsoleOutputCharacter;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacter, kernel32, 'WriteConsoleOutputCharacterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputCharacter]
  end;
end;
{$ELSE}
function WriteConsoleOutputCharacter; external kernel32 name 'WriteConsoleOutputCharacterW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputCharacter: Pointer;

function WriteConsoleOutputCharacter;
begin
  GetProcedureAddress(_WriteConsoleOutputCharacter, kernel32, 'WriteConsoleOutputCharacterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputCharacter]
  end;
end;
{$ELSE}
function WriteConsoleOutputCharacter; external kernel32 name 'WriteConsoleOutputCharacterA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleOutputAttribute: Pointer;

function WriteConsoleOutputAttribute;
begin
  GetProcedureAddress(_WriteConsoleOutputAttribute, kernel32, 'WriteConsoleOutputAttribute');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleOutputAttribute]
  end;
end;
{$ELSE}
function WriteConsoleOutputAttribute; external kernel32 name 'WriteConsoleOutputAttribute';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _FillConsoleOutputCharacterA: Pointer;

function FillConsoleOutputCharacterA;
begin
  GetProcedureAddress(_FillConsoleOutputCharacterA, kernel32, 'FillConsoleOutputCharacterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillConsoleOutputCharacterA]
  end;
end;
{$ELSE}
function FillConsoleOutputCharacterA; external kernel32 name 'FillConsoleOutputCharacterA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FillConsoleOutputCharacterW: Pointer;

function FillConsoleOutputCharacterW;
begin
  GetProcedureAddress(_FillConsoleOutputCharacterW, kernel32, 'FillConsoleOutputCharacterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillConsoleOutputCharacterW]
  end;
end;
{$ELSE}
function FillConsoleOutputCharacterW; external kernel32 name 'FillConsoleOutputCharacterW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _FillConsoleOutputCharacter: Pointer;

function FillConsoleOutputCharacter;
begin
  GetProcedureAddress(_FillConsoleOutputCharacter, kernel32, 'FillConsoleOutputCharacterW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillConsoleOutputCharacter]
  end;
end;
{$ELSE}
function FillConsoleOutputCharacter; external kernel32 name 'FillConsoleOutputCharacterW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _FillConsoleOutputCharacter: Pointer;

function FillConsoleOutputCharacter;
begin
  GetProcedureAddress(_FillConsoleOutputCharacter, kernel32, 'FillConsoleOutputCharacterA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillConsoleOutputCharacter]
  end;
end;
{$ELSE}
function FillConsoleOutputCharacter; external kernel32 name 'FillConsoleOutputCharacterA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _FillConsoleOutputAttribute: Pointer;

function FillConsoleOutputAttribute;
begin
  GetProcedureAddress(_FillConsoleOutputAttribute, kernel32, 'FillConsoleOutputAttribute');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FillConsoleOutputAttribute]
  end;
end;
{$ELSE}
function FillConsoleOutputAttribute; external kernel32 name 'FillConsoleOutputAttribute';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleMode: Pointer;

function GetConsoleMode;
begin
  GetProcedureAddress(_GetConsoleMode, kernel32, 'GetConsoleMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleMode]
  end;
end;
{$ELSE}
function GetConsoleMode; external kernel32 name 'GetConsoleMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberOfConsoleInputEvents: Pointer;

function GetNumberOfConsoleInputEvents;
begin
  GetProcedureAddress(_GetNumberOfConsoleInputEvents, kernel32, 'GetNumberOfConsoleInputEvents');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberOfConsoleInputEvents]
  end;
end;
{$ELSE}
function GetNumberOfConsoleInputEvents; external kernel32 name 'GetNumberOfConsoleInputEvents';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleScreenBufferInfo: Pointer;

function GetConsoleScreenBufferInfo;
begin
  GetProcedureAddress(_GetConsoleScreenBufferInfo, kernel32, 'GetConsoleScreenBufferInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleScreenBufferInfo]
  end;
end;
{$ELSE}
function GetConsoleScreenBufferInfo; external kernel32 name 'GetConsoleScreenBufferInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetLargestConsoleWindowSize: Pointer;

function GetLargestConsoleWindowSize;
begin
  GetProcedureAddress(_GetLargestConsoleWindowSize, kernel32, 'GetLargestConsoleWindowSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetLargestConsoleWindowSize]
  end;
end;
{$ELSE}
function GetLargestConsoleWindowSize; external kernel32 name 'GetLargestConsoleWindowSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleCursorInfo: Pointer;

function GetConsoleCursorInfo;
begin
  GetProcedureAddress(_GetConsoleCursorInfo, kernel32, 'GetConsoleCursorInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleCursorInfo]
  end;
end;
{$ELSE}
function GetConsoleCursorInfo; external kernel32 name 'GetConsoleCursorInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetCurrentConsoleFont: Pointer;

function GetCurrentConsoleFont;
begin
  GetProcedureAddress(_GetCurrentConsoleFont, kernel32, 'GetCurrentConsoleFont');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetCurrentConsoleFont]
  end;
end;
{$ELSE}
function GetCurrentConsoleFont; external kernel32 name 'GetCurrentConsoleFont';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleFontSize: Pointer;

function GetConsoleFontSize;
begin
  GetProcedureAddress(_GetConsoleFontSize, kernel32, 'GetConsoleFontSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleFontSize]
  end;
end;
{$ELSE}
function GetConsoleFontSize; external kernel32 name 'GetConsoleFontSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleSelectionInfo: Pointer;

function GetConsoleSelectionInfo;
begin
  GetProcedureAddress(_GetConsoleSelectionInfo, kernel32, 'GetConsoleSelectionInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleSelectionInfo]
  end;
end;
{$ELSE}
function GetConsoleSelectionInfo; external kernel32 name 'GetConsoleSelectionInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetNumberOfConsoleMouseButtons: Pointer;

function GetNumberOfConsoleMouseButtons;
begin
  GetProcedureAddress(_GetNumberOfConsoleMouseButtons, kernel32, 'GetNumberOfConsoleMouseButtons');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetNumberOfConsoleMouseButtons]
  end;
end;
{$ELSE}
function GetNumberOfConsoleMouseButtons; external kernel32 name 'GetNumberOfConsoleMouseButtons';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleMode: Pointer;

function SetConsoleMode;
begin
  GetProcedureAddress(_SetConsoleMode, kernel32, 'SetConsoleMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleMode]
  end;
end;
{$ELSE}
function SetConsoleMode; external kernel32 name 'SetConsoleMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleActiveScreenBuffer: Pointer;

function SetConsoleActiveScreenBuffer;
begin
  GetProcedureAddress(_SetConsoleActiveScreenBuffer, kernel32, 'SetConsoleActiveScreenBuffer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleActiveScreenBuffer]
  end;
end;
{$ELSE}
function SetConsoleActiveScreenBuffer; external kernel32 name 'SetConsoleActiveScreenBuffer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FlushConsoleInputBuffer: Pointer;

function FlushConsoleInputBuffer;
begin
  GetProcedureAddress(_FlushConsoleInputBuffer, kernel32, 'FlushConsoleInputBuffer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FlushConsoleInputBuffer]
  end;
end;
{$ELSE}
function FlushConsoleInputBuffer; external kernel32 name 'FlushConsoleInputBuffer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleScreenBufferSize: Pointer;

function SetConsoleScreenBufferSize;
begin
  GetProcedureAddress(_SetConsoleScreenBufferSize, kernel32, 'SetConsoleScreenBufferSize');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleScreenBufferSize]
  end;
end;
{$ELSE}
function SetConsoleScreenBufferSize; external kernel32 name 'SetConsoleScreenBufferSize';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleCursorPosition: Pointer;

function SetConsoleCursorPosition;
begin
  GetProcedureAddress(_SetConsoleCursorPosition, kernel32, 'SetConsoleCursorPosition');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleCursorPosition]
  end;
end;
{$ELSE}
function SetConsoleCursorPosition; external kernel32 name 'SetConsoleCursorPosition';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleCursorInfo: Pointer;

function SetConsoleCursorInfo;
begin
  GetProcedureAddress(_SetConsoleCursorInfo, kernel32, 'SetConsoleCursorInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleCursorInfo]
  end;
end;
{$ELSE}
function SetConsoleCursorInfo; external kernel32 name 'SetConsoleCursorInfo';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _ScrollConsoleScreenBufferA: Pointer;

function ScrollConsoleScreenBufferA;
begin
  GetProcedureAddress(_ScrollConsoleScreenBufferA, kernel32, 'ScrollConsoleScreenBufferA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollConsoleScreenBufferA]
  end;
end;
{$ELSE}
function ScrollConsoleScreenBufferA; external kernel32 name 'ScrollConsoleScreenBufferA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ScrollConsoleScreenBufferW: Pointer;

function ScrollConsoleScreenBufferW;
begin
  GetProcedureAddress(_ScrollConsoleScreenBufferW, kernel32, 'ScrollConsoleScreenBufferW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollConsoleScreenBufferW]
  end;
end;
{$ELSE}
function ScrollConsoleScreenBufferW; external kernel32 name 'ScrollConsoleScreenBufferW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ScrollConsoleScreenBuffer: Pointer;

function ScrollConsoleScreenBuffer;
begin
  GetProcedureAddress(_ScrollConsoleScreenBuffer, kernel32, 'ScrollConsoleScreenBufferW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollConsoleScreenBuffer]
  end;
end;
{$ELSE}
function ScrollConsoleScreenBuffer; external kernel32 name 'ScrollConsoleScreenBufferW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ScrollConsoleScreenBuffer: Pointer;

function ScrollConsoleScreenBuffer;
begin
  GetProcedureAddress(_ScrollConsoleScreenBuffer, kernel32, 'ScrollConsoleScreenBufferA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ScrollConsoleScreenBuffer]
  end;
end;
{$ELSE}
function ScrollConsoleScreenBuffer; external kernel32 name 'ScrollConsoleScreenBufferA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleWindowInfo: Pointer;

function SetConsoleWindowInfo;
begin
  GetProcedureAddress(_SetConsoleWindowInfo, kernel32, 'SetConsoleWindowInfo');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleWindowInfo]
  end;
end;
{$ELSE}
function SetConsoleWindowInfo; external kernel32 name 'SetConsoleWindowInfo';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleTextAttribute: Pointer;

function SetConsoleTextAttribute;
begin
  GetProcedureAddress(_SetConsoleTextAttribute, kernel32, 'SetConsoleTextAttribute');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleTextAttribute]
  end;
end;
{$ELSE}
function SetConsoleTextAttribute; external kernel32 name 'SetConsoleTextAttribute';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleCtrlHandler: Pointer;

function SetConsoleCtrlHandler;
begin
  GetProcedureAddress(_SetConsoleCtrlHandler, kernel32, 'SetConsoleCtrlHandler');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleCtrlHandler]
  end;
end;
{$ELSE}
function SetConsoleCtrlHandler; external kernel32 name 'SetConsoleCtrlHandler';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GenerateConsoleCtrlEvent: Pointer;

function GenerateConsoleCtrlEvent;
begin
  GetProcedureAddress(_GenerateConsoleCtrlEvent, kernel32, 'GenerateConsoleCtrlEvent');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GenerateConsoleCtrlEvent]
  end;
end;
{$ELSE}
function GenerateConsoleCtrlEvent; external kernel32 name 'GenerateConsoleCtrlEvent';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AllocConsole: Pointer;

function AllocConsole;
begin
  GetProcedureAddress(_AllocConsole, kernel32, 'AllocConsole');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AllocConsole]
  end;
end;
{$ELSE}
function AllocConsole; external kernel32 name 'AllocConsole';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _FreeConsole: Pointer;

function FreeConsole;
begin
  GetProcedureAddress(_FreeConsole, kernel32, 'FreeConsole');
  asm
    mov esp, ebp
    pop ebp
    jmp [_FreeConsole]
  end;
end;
{$ELSE}
function FreeConsole; external kernel32 name 'FreeConsole';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AttachConsole: Pointer;

function AttachConsole;
begin
  GetProcedureAddress(_AttachConsole, kernel32, 'AttachConsole');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AttachConsole]
  end;
end;
{$ELSE}
function AttachConsole; external kernel32 name 'AttachConsole';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleTitleA: Pointer;

function GetConsoleTitleA;
begin
  GetProcedureAddress(_GetConsoleTitleA, kernel32, 'GetConsoleTitleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleTitleA]
  end;
end;
{$ELSE}
function GetConsoleTitleA; external kernel32 name 'GetConsoleTitleA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleTitleW: Pointer;

function GetConsoleTitleW;
begin
  GetProcedureAddress(_GetConsoleTitleW, kernel32, 'GetConsoleTitleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleTitleW]
  end;
end;
{$ELSE}
function GetConsoleTitleW; external kernel32 name 'GetConsoleTitleW';
{$ENDIF DYNAMIC_LINK}
{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleTitle: Pointer;

function GetConsoleTitle;
begin
  GetProcedureAddress(_GetConsoleTitle, kernel32, 'GetConsoleTitleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleTitle]
  end;
end;
{$ELSE}
function GetConsoleTitle; external kernel32 name 'GetConsoleTitleW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleTitle: Pointer;

function GetConsoleTitle;
begin
  GetProcedureAddress(_GetConsoleTitle, kernel32, 'GetConsoleTitleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleTitle]
  end;
end;
{$ELSE}
function GetConsoleTitle; external kernel32 name 'GetConsoleTitleA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleTitleA: Pointer;

function SetConsoleTitleA;
begin
  GetProcedureAddress(_SetConsoleTitleA, kernel32, 'SetConsoleTitleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleTitleA]
  end;
end;
{$ELSE}
function SetConsoleTitleA; external kernel32 name 'SetConsoleTitleA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleTitleW: Pointer;

function SetConsoleTitleW;
begin
  GetProcedureAddress(_SetConsoleTitleW, kernel32, 'SetConsoleTitleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleTitleW]
  end;
end;
{$ELSE}
function SetConsoleTitleW; external kernel32 name 'SetConsoleTitleW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleTitle: Pointer;

function SetConsoleTitle;
begin
  GetProcedureAddress(_SetConsoleTitle, kernel32, 'SetConsoleTitleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleTitle]
  end;
end;
{$ELSE}
function SetConsoleTitle; external kernel32 name 'SetConsoleTitleW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleTitle: Pointer;

function SetConsoleTitle;
begin
  GetProcedureAddress(_SetConsoleTitle, kernel32, 'SetConsoleTitleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleTitle]
  end;
end;
{$ELSE}
function SetConsoleTitle; external kernel32 name 'SetConsoleTitleA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleA: Pointer;

function ReadConsoleA;
begin
  GetProcedureAddress(_ReadConsoleA, kernel32, 'ReadConsoleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleA]
  end;
end;
{$ELSE}
function ReadConsoleA; external kernel32 name 'ReadConsoleA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsoleW: Pointer;

function ReadConsoleW;
begin
  GetProcedureAddress(_ReadConsoleW, kernel32, 'ReadConsoleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsoleW]
  end;
end;
{$ELSE}
function ReadConsoleW; external kernel32 name 'ReadConsoleW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsole: Pointer;

function ReadConsole;
begin
  GetProcedureAddress(_ReadConsole, kernel32, 'ReadConsoleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsole]
  end;
end;
{$ELSE}
function ReadConsole; external kernel32 name 'ReadConsoleW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _ReadConsole: Pointer;

function ReadConsole;
begin
  GetProcedureAddress(_ReadConsole, kernel32, 'ReadConsoleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_ReadConsole]
  end;
end;
{$ELSE}
function ReadConsole; external kernel32 name 'ReadConsoleA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleA: Pointer;

function WriteConsoleA;
begin
  GetProcedureAddress(_WriteConsoleA, kernel32, 'WriteConsoleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleA]
  end;
end;
{$ELSE}
function WriteConsoleA; external kernel32 name 'WriteConsoleA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsoleW: Pointer;

function WriteConsoleW;
begin
  GetProcedureAddress(_WriteConsoleW, kernel32, 'WriteConsoleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsoleW]
  end;
end;
{$ELSE}
function WriteConsoleW; external kernel32 name 'WriteConsoleW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsole: Pointer;

function WriteConsole;
begin
  GetProcedureAddress(_WriteConsole, kernel32, 'WriteConsoleW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsole]
  end;
end;
{$ELSE}
function WriteConsole; external kernel32 name 'WriteConsoleW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _WriteConsole: Pointer;

function WriteConsole;
begin
  GetProcedureAddress(_WriteConsole, kernel32, 'WriteConsoleA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WriteConsole]
  end;
end;
{$ELSE}
function WriteConsole; external kernel32 name 'WriteConsoleA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


{$IFDEF DYNAMIC_LINK}
var
  _CreateConsoleScreenBuffer: Pointer;

function CreateConsoleScreenBuffer;
begin
  GetProcedureAddress(_CreateConsoleScreenBuffer, kernel32, 'CreateConsoleScreenBuffer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_CreateConsoleScreenBuffer]
  end;
end;
{$ELSE}
function CreateConsoleScreenBuffer; external kernel32 name 'CreateConsoleScreenBuffer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleCP: Pointer;

function GetConsoleCP;
begin
  GetProcedureAddress(_GetConsoleCP, kernel32, 'GetConsoleCP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleCP]
  end;
end;
{$ELSE}
function GetConsoleCP; external kernel32 name 'GetConsoleCP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleCP: Pointer;

function SetConsoleCP;
begin
  GetProcedureAddress(_SetConsoleCP, kernel32, 'SetConsoleCP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleCP]
  end;
end;
{$ELSE}
function SetConsoleCP; external kernel32 name 'SetConsoleCP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleOutputCP: Pointer;

function GetConsoleOutputCP;
begin
  GetProcedureAddress(_GetConsoleOutputCP, kernel32, 'GetConsoleOutputCP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleOutputCP]
  end;
end;
{$ELSE}
function GetConsoleOutputCP; external kernel32 name 'GetConsoleOutputCP';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _SetConsoleOutputCP: Pointer;

function SetConsoleOutputCP;
begin
  GetProcedureAddress(_SetConsoleOutputCP, kernel32, 'SetConsoleOutputCP');
  asm
    mov esp, ebp
    pop ebp
    jmp [_SetConsoleOutputCP]
  end;
end;
{$ELSE}
function SetConsoleOutputCP; external kernel32 name 'SetConsoleOutputCP';
{$ENDIF DYNAMIC_LINK}


{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleDisplayMode: Pointer;

function GetConsoleDisplayMode;
begin
  GetProcedureAddress(_GetConsoleDisplayMode, kernel32, 'GetConsoleDisplayMode');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleDisplayMode]
  end;
end;
{$ELSE}
function GetConsoleDisplayMode; external kernel32 name 'GetConsoleDisplayMode';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleWindow: Pointer;

function GetConsoleWindow;
begin
  GetProcedureAddress(_GetConsoleWindow, kernel32, 'GetConsoleWindow');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleWindow]
  end;
end;
{$ELSE}
function GetConsoleWindow; external kernel32 name 'GetConsoleWindow';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleProcessList: Pointer;

function GetConsoleProcessList;
begin
  GetProcedureAddress(_GetConsoleProcessList, kernel32, 'GetConsoleProcessList');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleProcessList]
  end;
end;
{$ELSE}
function GetConsoleProcessList; external kernel32 name 'GetConsoleProcessList';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddConsoleAliasA: Pointer;

function AddConsoleAliasA;
begin
  GetProcedureAddress(_AddConsoleAliasA, kernel32, 'AddConsoleAliasA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddConsoleAliasA]
  end;
end;
{$ELSE}
function AddConsoleAliasA; external kernel32 name 'AddConsoleAliasA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _AddConsoleAliasW: Pointer;

function AddConsoleAliasW;
begin
  GetProcedureAddress(_AddConsoleAliasW, kernel32, 'AddConsoleAliasW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddConsoleAliasW]
  end;
end;
{$ELSE}
function AddConsoleAliasW; external kernel32 name 'AddConsoleAliasW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _AddConsoleAlias: Pointer;

function AddConsoleAlias;
begin
  GetProcedureAddress(_AddConsoleAlias, kernel32, 'AddConsoleAliasW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddConsoleAlias]
  end;
end;
{$ELSE}
function AddConsoleAlias; external kernel32 name 'AddConsoleAliasW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _AddConsoleAlias: Pointer;

function AddConsoleAlias;
begin
  GetProcedureAddress(_AddConsoleAlias, kernel32, 'AddConsoleAliasA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_AddConsoleAlias]
  end;
end;
{$ELSE}
function AddConsoleAlias; external kernel32 name 'AddConsoleAliasA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasA: Pointer;

function GetConsoleAliasA;
begin
  GetProcedureAddress(_GetConsoleAliasA, kernel32, 'GetConsoleAliasA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasA]
  end;
end;
{$ELSE}
function GetConsoleAliasA; external kernel32 name 'GetConsoleAliasA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasW: Pointer;

function GetConsoleAliasW;
begin
  GetProcedureAddress(_GetConsoleAliasW, kernel32, 'GetConsoleAliasW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasW]
  end;
end;
{$ELSE}
function GetConsoleAliasW; external kernel32 name 'GetConsoleAliasW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAlias: Pointer;

function GetConsoleAlias;
begin
  GetProcedureAddress(_GetConsoleAlias, kernel32, 'GetConsoleAliasW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAlias]
  end;
end;
{$ELSE}
function GetConsoleAlias; external kernel32 name 'GetConsoleAliasW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAlias: Pointer;

function GetConsoleAlias;
begin
  GetProcedureAddress(_GetConsoleAlias, kernel32, 'GetConsoleAliasA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAlias]
  end;
end;
{$ELSE}
function GetConsoleAlias; external kernel32 name 'GetConsoleAliasA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasesLengthA: Pointer;

function GetConsoleAliasesLengthA;
begin
  GetProcedureAddress(_GetConsoleAliasesLengthA, kernel32, 'GetConsoleAliasesLengthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasesLengthA]
  end;
end;
{$ELSE}
function GetConsoleAliasesLengthA; external kernel32 name 'GetConsoleAliasesLengthA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasesLengthW: Pointer;

function GetConsoleAliasesLengthW;
begin
  GetProcedureAddress(_GetConsoleAliasesLengthW, kernel32, 'GetConsoleAliasesLengthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasesLengthW]
  end;
end;
{$ELSE}
function GetConsoleAliasesLengthW; external kernel32 name 'GetConsoleAliasesLengthW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasesLength: Pointer;

function GetConsoleAliasesLength;
begin
  GetProcedureAddress(_GetConsoleAliasesLength, kernel32, 'GetConsoleAliasesLengthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasesLength]
  end;
end;
{$ELSE}
function GetConsoleAliasesLength; external kernel32 name 'GetConsoleAliasesLengthW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasesLength: Pointer;

function GetConsoleAliasesLength;
begin
  GetProcedureAddress(_GetConsoleAliasesLength, kernel32, 'GetConsoleAliasesLengthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasesLength]
  end;
end;
{$ELSE}
function GetConsoleAliasesLength; external kernel32 name 'GetConsoleAliasesLengthA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExesLengthA: Pointer;

function GetConsoleAliasExesLengthA;
begin
  GetProcedureAddress(_GetConsoleAliasExesLengthA, kernel32, 'GetConsoleAliasExesLengthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExesLengthA]
  end;
end;
{$ELSE}
function GetConsoleAliasExesLengthA; external kernel32 name 'GetConsoleAliasExesLengthA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExesLengthW: Pointer;

function GetConsoleAliasExesLengthW;
begin
  GetProcedureAddress(_GetConsoleAliasExesLengthW, kernel32, 'GetConsoleAliasExesLengthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExesLengthW]
  end;
end;
{$ELSE}
function GetConsoleAliasExesLengthW; external kernel32 name 'GetConsoleAliasExesLengthW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExesLength: Pointer;

function GetConsoleAliasExesLength;
begin
  GetProcedureAddress(_GetConsoleAliasExesLength, kernel32, 'GetConsoleAliasExesLengthW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExesLength]
  end;
end;
{$ELSE}
function GetConsoleAliasExesLength; external kernel32 name 'GetConsoleAliasExesLengthW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExesLength: Pointer;

function GetConsoleAliasExesLength;
begin
  GetProcedureAddress(_GetConsoleAliasExesLength, kernel32, 'GetConsoleAliasExesLengthA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExesLength]
  end;
end;
{$ELSE}
function GetConsoleAliasExesLength; external kernel32 name 'GetConsoleAliasExesLengthA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasesA: Pointer;

function GetConsoleAliasesA;
begin
  GetProcedureAddress(_GetConsoleAliasesA, kernel32, 'GetConsoleAliasesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasesA]
  end;
end;
{$ELSE}
function GetConsoleAliasesA; external kernel32 name 'GetConsoleAliasesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasesW: Pointer;

function GetConsoleAliasesW;
begin
  GetProcedureAddress(_GetConsoleAliasesW, kernel32, 'GetConsoleAliasesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasesW]
  end;
end;
{$ELSE}
function GetConsoleAliasesW; external kernel32 name 'GetConsoleAliasesW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliases: Pointer;

function GetConsoleAliases;
begin
  GetProcedureAddress(_GetConsoleAliases, kernel32, 'GetConsoleAliasesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliases]
  end;
end;
{$ELSE}
function GetConsoleAliases; external kernel32 name 'GetConsoleAliasesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliases: Pointer;

function GetConsoleAliases;
begin
  GetProcedureAddress(_GetConsoleAliases, kernel32, 'GetConsoleAliasesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliases]
  end;
end;
{$ELSE}
function GetConsoleAliases; external kernel32 name 'GetConsoleAliasesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExesA: Pointer;

function GetConsoleAliasExesA;
begin
  GetProcedureAddress(_GetConsoleAliasExesA, kernel32, 'GetConsoleAliasExesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExesA]
  end;
end;
{$ELSE}
function GetConsoleAliasExesA; external kernel32 name 'GetConsoleAliasExesA';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExesW: Pointer;

function GetConsoleAliasExesW;
begin
  GetProcedureAddress(_GetConsoleAliasExesW, kernel32, 'GetConsoleAliasExesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExesW]
  end;
end;
{$ELSE}
function GetConsoleAliasExesW; external kernel32 name 'GetConsoleAliasExesW';
{$ENDIF DYNAMIC_LINK}

{$IFDEF UNICODE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExes: Pointer;

function GetConsoleAliasExes;
begin
  GetProcedureAddress(_GetConsoleAliasExes, kernel32, 'GetConsoleAliasExesW');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExes]
  end;
end;
{$ELSE}
function GetConsoleAliasExes; external kernel32 name 'GetConsoleAliasExesW';
{$ENDIF DYNAMIC_LINK}
{$ELSE}

{$IFDEF DYNAMIC_LINK}
var
  _GetConsoleAliasExes: Pointer;

function GetConsoleAliasExes;
begin
  GetProcedureAddress(_GetConsoleAliasExes, kernel32, 'GetConsoleAliasExesA');
  asm
    mov esp, ebp
    pop ebp
    jmp [_GetConsoleAliasExes]
  end;
end;
{$ELSE}
function GetConsoleAliasExes; external kernel32 name 'GetConsoleAliasExesA';
{$ENDIF DYNAMIC_LINK}
{$ENDIF}


end.
