{******************************************************************************}
{                                                       	               }
{ 32 bit Generic Thunks API interface Unit for Object Pascal                   }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: wownt32.h, released June 2000. The original Pascal     }
{ code is: WowNT32.pas, released December 2000. The initial developer of the   }
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

unit JwaWowNT32;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "wownt32.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

//
// 16:16 -> 0:32 Pointer translation.
//
// WOWGetVDMPointer will convert the passed in 16-bit address
// to the equivalent 32-bit flat pointer.  If fProtectedMode
// is TRUE, the function treats the upper 16 bits as a selector
// in the local descriptor table.  If fProtectedMode is FALSE,
// the upper 16 bits are treated as a real-mode segment value.
// In either case the lower 16 bits are treated as the offset.
//
// The return value is NULL if the selector is invalid.
//
// NOTE:  Limit checking is not performed in the retail build
// of Windows NT.  It is performed in the checked (debug) build
// of WOW32.DLL, which will cause NULL to be returned when the
// limit is exceeded by the supplied offset.
//

function WOWGetVDMPointer(vp, dwBytes: DWORD; fProtectedMode: BOOL): LPVOID; stdcall;

//
// The following two functions are here for compatibility with
// Windows 95.  On Win95, the global heap can be rearranged,
// invalidating flat pointers returned by WOWGetVDMPointer, while
// a thunk is executing.  On Windows NT, the 16-bit VDM is completely
// halted while a thunk executes, so the only way the heap will
// be rearranged is if a callback is made to Win16 code.
//
// The Win95 versions of these functions call GlobalFix to
// lock down a segment's flat address, and GlobalUnfix to
// release the segment.
//
// The Windows NT implementations of these functions do *not*
// call GlobalFix/GlobalUnfix on the segment, because there
// will not be any heap motion unless a callback occurs.
// If your thunk does callback to the 16-bit side, be sure
// to discard flat pointers and call WOWGetVDMPointer again
// to be sure the flat address is correct.
//

function WOWGetVDMPointerFix(vp, dwBytes: DWORD; fProtectedMode: BOOL): LPVOID; stdcall;
procedure WOWGetVDMPointerUnfix(vp: DWORD); stdcall;

//
// Win16 memory management.
//
// These functions can be used to manage memory in the Win16
// heap.  The following four functions are identical to their
// Win16 counterparts, except that they are called from Win32
// code.
//

function WOWGlobalAlloc16(wFlags: WORD; cb: DWORD): WORD; stdcall;
function WOWGlobalFree16(hMem: WORD): WORD; stdcall;
function WOWGlobalLock16(hMem: WORD): DWORD; stdcall;
function WOWGlobalUnlock16(hMem: WORD): BOOL; stdcall;

//
// The following three functions combine two common operations in
// one switch to 16-bit mode.
//

function WOWGlobalAllocLock16(wFlags: WORD; cb: DWORD; var phMem: WORD): DWORD; stdcall;
function WOWGlobalUnlockFree16(vpMem: DWORD): WORD; stdcall;
function WOWGlobalLockSize16(hMem: WORD; pcb: PDWORD): DWORD; stdcall;

//
// Yielding the Win16 nonpreemptive scheduler
//
// The following two functions are provided for Win32 code called
// via Generic Thunks which needs to yield the Win16 scheduler so
// that tasks in that VDM can execute while the thunk waits for
// something to complete.  These two functions are functionally
// identical to calling back to 16-bit code which calls Yield or
// DirectedYield.
//

procedure WOWYield16; stdcall;
procedure WOWDirectedYield16(htask16: WORD); stdcall;

//
// 16 <--> 32 Handle mapping functions.
//
// NOTE:  While some of these functions perform trivial
// conversions, these functions must be used to maintain
// compatibility with future versions of Windows NT which
// may require different handle mapping.
//

type
  _WOW_HANDLE_TYPE = (
    WOW_TYPE_HWND,
    WOW_TYPE_HMENU,
    WOW_TYPE_HDWP,
    WOW_TYPE_HDROP,
    WOW_TYPE_HDC,
    WOW_TYPE_HFONT,
    WOW_TYPE_HMETAFILE,
    WOW_TYPE_HRGN,
    WOW_TYPE_HBITMAP,
    WOW_TYPE_HBRUSH,
    WOW_TYPE_HPALETTE,
    WOW_TYPE_HPEN,
    WOW_TYPE_HACCEL,
    WOW_TYPE_HTASK,
    WOW_TYPE_FULLHWND);
  WOW_HANDLE_TYPE = _WOW_HANDLE_TYPE;
  TWowHandleType = WOW_HANDLE_TYPE;

function WOWHandle32(Handle: WORD; Type_: WOW_HANDLE_TYPE): HANDLE; stdcall;
function WOWHandle16(Handle: HANDLE; Type_: WOW_HANDLE_TYPE): WORD; stdcall;

function HWND_32(h16: WORD): HANDLE;
function HMENU_32(h16: WORD): HANDLE;
function HDWP_32(h16: WORD): HANDLE;
function HDROP_32(h16: WORD): HANDLE;
function HDC_32(h16: WORD): HANDLE;
function HFONT_32(h16: WORD): HANDLE;
function HMETAFILE_32(h16: WORD): HANDLE;
function HRGN_32(h16: WORD): HANDLE;
function HBITMAP_32(h16: WORD): HANDLE;
function HBRUSH_32(h16: WORD): HANDLE;
function HPALETTE_32(h16: WORD): HANDLE;
function HPEN_32(h16: WORD): HANDLE;
function HACCEL_32(h16: WORD): HANDLE;
function HTASK_32(h16: WORD): HANDLE;
function FULLHWND_32(h16: WORD): HANDLE;
function HWND_16(h32: HANDLE): WORD;
function HMENU_16(h32: HANDLE): WORD;
function HDWP_16(h32: HANDLE): WORD;
function HDROP_16(h32: HANDLE): WORD;
function HDC_16(h32: HANDLE): WORD;
function HFONT_16(h32: HANDLE): WORD;
function HMETAFILE_16(h32: HANDLE): WORD;
function HRGN_16(h32: HANDLE): WORD;
function HBITMAP_16(h32: HANDLE): WORD;
function HBRUSH_16(h32: HANDLE): WORD;
function HPALETTE_16(h32: HANDLE): WORD;
function HPEN_16(h32: HANDLE): WORD;
function HACCEL_16(h32: HANDLE): WORD;
function HTASK_16(h32: HANDLE): WORD;

//
// Generic Callbacks.
//
// WOWCallback16 can be used in Win32 code called
// from 16-bit (such as by using Generic Thunks) to call back to
// the 16-bit side.  The function called must be declared similarly
// to the following:
//
// LONG FAR PASCAL CallbackRoutine(DWORD dwParam);
//
// If you are passing a pointer, declare the parameter as such:
//
// LONG FAR PASCAL CallbackRoutine(VOID FAR *vp);
//
// NOTE: If you are passing a pointer, you'll need to get the
// pointer using WOWGlobalAlloc16 or WOWGlobalAllocLock16
//
// If the function called returns a WORD instead of a DWORD, the
// upper 16 bits of the return value is undefined.  Similarly, if
// the function called has no return value, the entire return value
// is undefined.
//
// WOWCallback16Ex allows any combination of arguments up to
// WCB16_MAX_CBARGS bytes total to be passed to the 16-bit routine.
// cbArgs is used to properly clean up the 16-bit stack after calling
// the routine.  Regardless of the value of cbArgs, WCB16_MAX_CBARGS
// bytes will always be copied from pArgs to the 16-bit stack.  If
// pArgs is less than WCB16_MAX_CBARGS bytes from the end of a page,
// and the next page is inaccessible, WOWCallback16Ex will incur an
// access violation.
//
// If cbArgs is larger than the WCB16_MAX_ARGS which the running
// system supports, the function returns FALSE and GetLastError
// returns ERROR_INVALID_PARAMETER.  Otherwise the function
// returns TRUE and the DWORD pointed to by pdwRetCode contains
// the return code from the callback routine.  If the callback
// routine returns a WORD, the HIWORD of the return code is
// undefined and should be ignored using LOWORD(dwRetCode).
//
// WOWCallback16Ex can call routines using the PASCAL and CDECL
// calling conventions.  The default is to use the PASCAL
// calling convention.  To use CDECL, pass WCB16_CDECL in the
// dwFlags parameter.
//
// The arguments pointed to by pArgs must be in the correct
// order for the callback routine's calling convention.
// To call the PASCAL routine SetWindowText,
//
// LONG FAR PASCAL SetWindowText(HWND hwnd, LPCSTR lpsz);
//
// pArgs would point to an array of words:
//
// WORD SetWindowTextArgs[] = {OFFSETOF(lpsz), SELECTOROF(lpsz), hwnd};
//
// In other words, the arguments are placed in the array in reverse
// order with the least significant word first for DWORDs and offset
// first for FAR pointers.
//
// To call the CDECL routine wsprintf, for example
//
// LPSTR lpszFormat = "%d %s";
// int _cdecl wsprintf(lpsz, lpszFormat, nValue. lpszString);
//
// pArgs would point to the array:
//
// WORD wsprintfArgs[] = {OFFSETOF(lpsz), SELECTOROF(lpsz),
//                        OFFSETOF(lpszFormat), SELECTOROF(lpszFormat),
//                        nValue,
//                        OFFSETOF(lpszString), SELECTOROF(lpszString)};
//
// In other words, the arguments are placed in the array in the order
// listed in the function prototype with the least significant word
// first for DWORDs and offset first for FAR pointers.
//

function WOWCallback16(vpfn16: DWORD; dwParam: DWORD): DWORD; stdcall;

const
  WCB16_MAX_CBARGS = 16;

  WCB16_PASCAL     = $0;
  WCB16_CDECL      = $1;

function WOWCallback16Ex(vpfn16, dwFlags, cbArgs: DWORD; pArgs: PVOID; pdwRetCode: PDWORD): BOOL; stdcall;

implementation

const
  wow32lib = 'wow32.dll';


{$IFDEF DYNAMIC_LINK}
var
  _WOWGetVDMPointer: Pointer;

function WOWGetVDMPointer;
begin
  GetProcedureAddress(_WOWGetVDMPointer, wow32lib, 'WOWGetVDMPointer');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGetVDMPointer]
  end;
end;
{$ELSE}
function WOWGetVDMPointer; external wow32lib name 'WOWGetVDMPointer';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGetVDMPointerFix: Pointer;

function WOWGetVDMPointerFix;
begin
  GetProcedureAddress(_WOWGetVDMPointerFix, wow32lib, 'WOWGetVDMPointerFix');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGetVDMPointerFix]
  end;
end;
{$ELSE}
function WOWGetVDMPointerFix; external wow32lib name 'WOWGetVDMPointerFix';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGetVDMPointerUnfix: Pointer;

procedure WOWGetVDMPointerUnfix;
begin
  GetProcedureAddress(_WOWGetVDMPointerUnfix, wow32lib, 'WOWGetVDMPointerUnfix');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGetVDMPointerUnfix]
  end;
end;
{$ELSE}
procedure WOWGetVDMPointerUnfix; external wow32lib name 'WOWGetVDMPointerUnfix';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalAlloc16: Pointer;

function WOWGlobalAlloc16;
begin
  GetProcedureAddress(_WOWGlobalAlloc16, wow32lib, 'WOWGlobalAlloc16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalAlloc16]
  end;
end;
{$ELSE}
function WOWGlobalAlloc16; external wow32lib name 'WOWGlobalAlloc16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalFree16: Pointer;

function WOWGlobalFree16;
begin
  GetProcedureAddress(_WOWGlobalFree16, wow32lib, 'WOWGlobalFree16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalFree16]
  end;
end;
{$ELSE}
function WOWGlobalFree16; external wow32lib name 'WOWGlobalFree16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalLock16: Pointer;

function WOWGlobalLock16;
begin
  GetProcedureAddress(_WOWGlobalLock16, wow32lib, 'WOWGlobalLock16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalLock16]
  end;
end;
{$ELSE}
function WOWGlobalLock16; external wow32lib name 'WOWGlobalLock16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalUnlock16: Pointer;

function WOWGlobalUnlock16;
begin
  GetProcedureAddress(_WOWGlobalUnlock16, wow32lib, 'WOWGlobalUnlock16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalUnlock16]
  end;
end;
{$ELSE}
function WOWGlobalUnlock16; external wow32lib name 'WOWGlobalUnlock16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalAllocLock16: Pointer;

function WOWGlobalAllocLock16;
begin
  GetProcedureAddress(_WOWGlobalAllocLock16, wow32lib, 'WOWGlobalAllocLock16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalAllocLock16]
  end;
end;
{$ELSE}
function WOWGlobalAllocLock16; external wow32lib name 'WOWGlobalAllocLock16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalUnlockFree16: Pointer;

function WOWGlobalUnlockFree16;
begin
  GetProcedureAddress(_WOWGlobalUnlockFree16, wow32lib, 'WOWGlobalUnlockFree16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalUnlockFree16]
  end;
end;
{$ELSE}
function WOWGlobalUnlockFree16; external wow32lib name 'WOWGlobalUnlockFree16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWGlobalLockSize16: Pointer;

function WOWGlobalLockSize16;
begin
  GetProcedureAddress(_WOWGlobalLockSize16, wow32lib, 'WOWGlobalLockSize16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWGlobalLockSize16]
  end;
end;
{$ELSE}
function WOWGlobalLockSize16; external wow32lib name 'WOWGlobalLockSize16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWYield16: Pointer;

procedure WOWYield16;
begin
  GetProcedureAddress(_WOWYield16, wow32lib, 'WOWYield16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWYield16]
  end;
end;
{$ELSE}
procedure WOWYield16; external wow32lib name 'WOWYield16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWDirectedYield16: Pointer;

procedure WOWDirectedYield16;
begin
  GetProcedureAddress(_WOWDirectedYield16, wow32lib, 'WOWDirectedYield16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWDirectedYield16]
  end;
end;
{$ELSE}
procedure WOWDirectedYield16; external wow32lib name 'WOWDirectedYield16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWHandle32: Pointer;

function WOWHandle32;
begin
  GetProcedureAddress(_WOWHandle32, wow32lib, 'WOWHandle32');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWHandle32]
  end;
end;
{$ELSE}
function WOWHandle32; external wow32lib name 'WOWHandle32';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWHandle16: Pointer;

function WOWHandle16;
begin
  GetProcedureAddress(_WOWHandle16, wow32lib, 'WOWHandle16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWHandle16]
  end;
end;
{$ELSE}
function WOWHandle16; external wow32lib name 'WOWHandle16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWCallback16: Pointer;

function WOWCallback16;
begin
  GetProcedureAddress(_WOWCallback16, wow32lib, 'WOWCallback16');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWCallback16]
  end;
end;
{$ELSE}
function WOWCallback16; external wow32lib name 'WOWCallback16';
{$ENDIF DYNAMIC_LINK}

{$IFDEF DYNAMIC_LINK}
var
  _WOWCallback16Ex: Pointer;

function WOWCallback16Ex;
begin
  GetProcedureAddress(_WOWCallback16Ex, wow32lib, 'WOWCallback16Ex');
  asm
    mov esp, ebp
    pop ebp
    jmp [_WOWCallback16Ex]
  end;
end;
{$ELSE}
function WOWCallback16Ex; external wow32lib name 'WOWCallback16Ex';
{$ENDIF DYNAMIC_LINK}

function HWND_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HWND);
end;

function HMENU_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HMENU);
end;

function HDWP_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HDWP);
end;

function HDROP_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HDROP);
end;

function HDC_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HDC);
end;

function HFONT_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HFONT);
end;

function HMETAFILE_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HMETAFILE);
end;

function HRGN_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HRGN);
end;

function HBITMAP_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HBITMAP);
end;

function HBRUSH_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HBRUSH);
end;

function HPALETTE_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HPALETTE);
end;

function HPEN_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HPEN);
end;

function HACCEL_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HACCEL);
end;

function HTASK_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_HTASK);
end;

function FULLHWND_32(h16: WORD): HANDLE;
begin
  Result := WOWHandle32(h16, WOW_TYPE_FULLHWND);
end;

function HWND_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HWND);
end;

function HMENU_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HMENU);
end;

function HDWP_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HDWP);
end;

function HDROP_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HDROP);
end;

function HDC_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HDC);
end;

function HFONT_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HFONT);
end;

function HMETAFILE_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HMETAFILE);
end;

function HRGN_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HRGN);
end;

function HBITMAP_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HBITMAP);
end;

function HBRUSH_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HBRUSH);
end;

function HPALETTE_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HPALETTE);
end;

function HPEN_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HPEN);
end;

function HACCEL_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HACCEL);
end;

function HTASK_16(h32: HANDLE): WORD;
begin
  Result := WOWHandle16(h32, WOW_TYPE_HTASK);
end;

end.
