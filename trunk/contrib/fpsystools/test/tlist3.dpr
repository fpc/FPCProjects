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
{*                    TLIST3.DPR 1.05                    *}
{*                     Test Program                      *}
{*********************************************************}

program TList3;

{$IFDEF Win32}
 {$APPTYPE CONSOLE}
{$ENDIF}

{$I STDEFINE.INC}

uses
  sysutils, stbase, stlist
{$IFNDEF OS32}
  , wincrt
{$ENDIF}
  ;
const
  Nodes = 100;
  Tests = 1000;
var
  list : TStlist;
  i, j : longint;

begin
  list := TStlist.create(TStlistnode);

  for I := 0 to Nodes-1 do
    list.append(pointer(I));

  for J := 1 to Tests do begin
    I := random(Nodes);
    if longint(list[I].Data) <> I then begin
      writeln('error!');
      halt;
    end;
  end;

  list.destroy;
  WriteLn('Done');
  readln;
end.
