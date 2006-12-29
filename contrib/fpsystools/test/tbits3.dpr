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
{*                   TBITS3.DPR 1.05                     *}
{*                     Test Program                      *}
{*********************************************************}

program TBits3;

{$I STDEFINE.INC}
{$R-,S-,X+}

{$IFDEF Win32}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  stbase,
  stbits
  ;

const
  {A range of bit counts that crosses a 16-bit segment boundary}
  BitMaxStart = 65530*8*2;
  BitMaxStop  = 65540*8*2;
  BitMaxDelta = 1;

var
  BitMax : LongInt;
  BS, BS0 : TStBits;

begin
  BitMax := BitMaxStart;

  {Create a bitset with all bits set}
  BS := TStBits.Create(BitMax);
  BS.SetBits;

  while BitMax < BitMaxStop do begin
    write(BitMax);

    {Create a fresh zero-filled bitset}
    BS0 := TStBits.Create(BitMax);

    {Force a recount of the bits in BS}
    BS.OrBits(BS0);

    BS0.Free;

    if BS.Count <> BitMaxStart+1 then begin
      writeln(' Error!');
      halt;
    end;
    writeln;

    {Resize the test set}
    inc(BitMax, BitMaxDelta);
    BS.Max := BitMax;
  end;

  BS.Free;
  readln;
end.
