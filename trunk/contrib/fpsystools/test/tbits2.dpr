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
{*                    TBITS2.DPR 1.05                    *}
{*                     Test Program                      *}
{*********************************************************}

program TBits2;

{$I STDEFINE.INC}
{$R-,S-,X+}

{$IFDEF Win32}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  stbase,
  stbits
{$IFNDEF OS32}
  , wincrt
{$ENDIF}
  ;

const
  BitMax = 10000;
var
  BS : TStBits;
  Errors : LongInt;

type
  TModRec = record
    ModVal : Word;
    Tst : LongInt;
  end;
  PModRec = ^TModRec;

function TestModBit(Container : TStBits;
                    N : LongInt;
                    OtherData : Pointer) : Boolean; far;
begin
  with PModRec(OtherData)^ do
    if N mod ModVal <> 0 then begin
      WriteLn('Iterate returned bogus element ', N);
      inc(Errors);
    end else
      inc(Tst);
  Result := True;
end;

procedure TestMod(ModVal : Word);
var
  B : LongInt;
  Cnt : LongInt;
  V : Boolean;
  TMR : TModRec;
begin
  WriteLn;
  WriteLn('Clearing bits one by one');
  BS.SetBits;
  for B := 0 to BitMax do
    BS[B] := False;

  WriteLn('Setting bits mod ', ModVal);
  Cnt := 0;
  for B := 0 to BitMax do
    if B mod ModVal = 0 then begin
      BS[B] := True;
      inc(Cnt);
    end;

  WriteLn('Testing set bit count');
  if BS.Count <> Cnt then begin
    WriteLn('Count returned bogus count ', BS.Count);
    inc(Errors);
  end;

  WriteLn('Testing all bits');
  for B := 0 to BitMax do begin
    V := BS[B];
    if B mod ModVal = 0 then begin
      if not V then begin
        WriteLn('Bit ', B, ' is not set');
        inc(Errors);
      end;
    end else begin
      if V then begin
        WriteLn('Bit ', B, ' is set');
        inc(Errors);
      end;
    end;
  end;

  WriteLn('Iterating, up, set');
  TMR.ModVal := ModVal;
  TMR.Tst := 0;
  BS.Iterate(TestModBit, True, True, @TMR);
  if TMR.Tst <> Cnt then begin
    WriteLn('Iterate missed some bits ', Cnt-TMR.Tst);
    inc(Errors);
  end;

  WriteLn('Iterating, down, set');
  TMR.ModVal := ModVal;
  TMR.Tst := 0;
  BS.Iterate(TestModBit, True, False, @TMR);
  if TMR.Tst <> Cnt then begin
    WriteLn('Iterate missed some bits ', Cnt-TMR.Tst);
    inc(Errors);
  end;
end;

begin
  Errors := 0;

  BS := TStBits.Create(BitMax);

  {Test when setting every 9th bit}
  TestMod(9);

  {Test when setting every 3rd bit}
  TestMod(3);

  {Test when setting every 127th bit}
  TestMod(127);

  {Test when setting every bit}
  TestMod(1);

  BS.Free;

  WriteLn;
  WriteLn(Errors, ' errors detected');
  readln;
end.
