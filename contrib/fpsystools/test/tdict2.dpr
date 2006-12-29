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
{*                   TDICT2.DPR 2.00                     *}
{*                     Test Program                      *}
{*********************************************************}

program TDict2;

{$I STDEFINE.INC}

{$IFDEF Win32}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
{$IFDEF Win32}
  windows,
{$ENDIF}
  sysutils, stbase, stdict
{$IFNDEF os32}
  , wincrt
{$ENDIF}
  ;
const
  Nodes = 10000;
  NodeLen = 32;
  TableSize = 10039;
var
  dict : TStdictionary;
  i, total : longint;
  bincnt : array[0..99] of integer;

function RandomStr : string;
var
  i : integer;
begin
{$IFDEF os32}
  SetLength(Result, NodeLen);
{$ELSE}
  Result[0] := Char(NodeLen);
{$ENDIF}
  for I := 1 to NodeLen do
    Result[I] := char(byte('a')+random(26));
end;

begin
  WriteLn('Creating dictionary, adding ', Nodes, ' random nodes...');
  dict := TStDictionary.Create(TableSize);
  dict.Hash := AnsiELFHashText;

  randseed := 0;
  for i := 1 to Nodes do
    dict.add(RandomStr, pointer(i));

  randseed := 0;
  if not dict.Exists(RandomStr, pointer(i)) then
    WriteLn('error');

  {display the distribution in the hash table}
  fillchar(bincnt, sizeof(bincnt), 0);
  for i := 0 to TableSize-1 do
    inc(bincnt[dict.BinCount(i)]);
  total := 0;
  for i := 0 to 99 do
    if bincnt[i] <> 0 then begin
      writeln(i:5, ' ', bincnt[i]);
      inc(total, i*bincnt[i]);
    end;
  writeln('total ', total);

  dict.free;
  readln;
end.

