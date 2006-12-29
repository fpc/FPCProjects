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
{*                   TDICT3.DPR 2.00                     *}
{*                     Test Program                      *}
{*********************************************************}

program TDict3;

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
  Nodes = 10;
  NodeLen = 32;
var
  strings : array[1..Nodes] of string;
  dict : TStdictionary;
  i, j : longint;

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
  dict := TStDictionary.Create(1000);
  dict.Hash := AnsiELFHashText;

  randseed := 0;
  for i := 1 to Nodes do
    strings[i] := RandomStr;

  for i := 1 to Nodes do
    dict.add(strings[i], pointer(i));

  for i := 1 to Nodes do
    if not dict.Exists(strings[i], pointer(j)) then
      WriteLn('not found error on node ', i)
    else if i <> j then
      WriteLn('data error on node ', i);

  writeln('setting hashsize to 2000');
  dict.hashsize := 2000;
  writeln('new count is ', dict.count);

  for i := 1 to Nodes do
    if not dict.Exists(strings[i], pointer(j)) then
      WriteLn('not found error on node ', i)
    else if i <> j then
      WriteLn('data error on node ', i);

  writeln('setting hashsize to 3000');
  dict.hashsize := 3000;
  writeln('new count is ', dict.count);

  for i := 1 to Nodes do
    if not dict.Exists(strings[i], pointer(j)) then
      WriteLn('not found error on node ', i)
    else if i <> j then
      WriteLn('data error on node ', i);

  for i := 1 to Nodes do
    strings[i] := '';

  dict.free;
  writeln('Done');
  readln;
end.


