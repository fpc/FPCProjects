(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{*                    TSORT2.DPR 1.05                    *}
{*                     Test Program                      *}
{*********************************************************}

{$R-,S-,X+,I+}
program TSort2;

{$IFDEF Win32}
 {$APPTYPE CONSOLE}
{$ENDIF}

{$I STDEFINE.INC}

uses
  stbase,
  stsort,
  sysutils,
{$IFDEF OS32}
  TstUtil;
{$ELSE}
  wintypes,
  winprocs,
  wincrt;
{$ENDIF}

const
  NumRecs = 300;
  FillSize = 99;
type
  TRec = record
    Key : LongInt;
    Data : array[1..FillSize] of byte;
  end;

var
  A : array[1..NumRecs] of TRec;
  I, J, T1, T2, LastKey : LongInt;

{$IFDEF VirtualPascal}
function gettickcount : longint;
begin
  gettickcount := VPUtils.GetTimemSec;
end;
{$ENDIF}

function LongIntLess(const El1, El2) : Integer; far;
begin
  Result := TRec(El1).Key-TRec(El2).Key;
  {raise EAbort.Create('user abort');}
end;

begin
  for I := 1 to NumRecs do begin
    A[I].Key := Random(10000);
    FillChar(A[I].Data, FillSize, Lo(A[I].Key));
  end;

  T1 := GetTickCount;
  ArraySort(A, SizeOf(TRec), NumRecs, LongIntLess);
  T2 := GetTickCount;
  writeln('Time ', T2-T1);

  LastKey := 0;
  for I := 1 to NumRecs do begin
    if A[I].Key < LastKey then begin
      writeln('sort error!', I);
      halt;
    end;
    LastKey := A[I].Key;
    for J := 1 to FillSize do
      if A[I].Data[J] <> Lo(LastKey) then begin
        writeln('data swap error!', I);
        halt;
      end;
  end;

  writeln('Done');
  readln;
end.

