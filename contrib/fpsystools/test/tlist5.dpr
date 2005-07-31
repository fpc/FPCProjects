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
{*                    TLIST5.DPR 1.05                    *}
{*                     Test Program                      *}
{*********************************************************}

program TList5;

{$IFDEF Win32}
 {$APPTYPE CONSOLE}
{$ENDIF}

uses
  sysutils, stbase, stlist
{$IFNDEF Win32}
  , wincrt
{$ENDIF}
  ;
const
  Nodes = 10;
  Tests = 1000;
type
  TMyNodeData = class
    MyInt : Integer;
    MyStr : string;
    constructor Create(I : Integer; const S : string);
  end;

  TMyNode = class(TStListNode)
    destructor Destroy; override;
  end;

var
  List : TStlist;
  i, j : longint;

constructor TMyNodeData.Create(I : Integer; const S : string);
begin
  MyInt := I;
  MyStr := S;
end;

destructor TMyNode.Destroy;
begin
  writeln('destroying ', TMyNodeData(Data).MyInt);
  TMyNodeData(Data).Free;
  inherited Destroy;
end;

begin
  list := TStlist.create(TMyNode);

  for I := 0 to Nodes-1 do
    list.append(TMyNodeData.Create(I, inttostr(I)));

  for J := 1 to Tests do begin
    I := random(Nodes);
    if longint(TMyNodeData(list[I].Data).MyInt) <> I then begin
      writeln('error!');
      halt;
    end;
  end;

  list.free;
  writeln('ok!');
  readln;
end.
