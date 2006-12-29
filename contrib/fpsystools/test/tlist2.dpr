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
{*                    TLIST2.DPR 1.05                    *}
{*                     Test Program                      *}
{*********************************************************}
program TList2;

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
  Nodes = 20;
  NodeLen = 5;
var
  list : TStlist;
  i : longint;
  compares : longint;
  p : string;
  prev : pstring;
  checkorder : boolean;

function SCompare(Data1, Data2 : Pointer) : Integer; far;
begin
  inc(compares);
  SCompare := CompareStr(pstring(Data1)^, pstring(Data2)^);
end;

procedure SDispose(Data : Pointer); far;
begin
  disposestr(pstring(data));
end;

function RandomStr : string;
var
  i : integer;
  Dest : PChar;
begin
{$IFDEF OS32}
  SetLength(Result, NodeLen);
  Dest := Pointer(Result);
  for I := 1 to NodeLen do begin
    Dest^ := char(byte('a')+random(26));
    inc(Dest);
  end;
{$ELSE}
  for I := 1 to NodeLen do
    Result[I] := char(byte('a')+random(26));
  Result[0] := Char(NodeLen);
{$ENDIF}
end;

function ShowNode(Container : TStContainer;
                  Node : TStNode;
                  OtherData : Pointer) : Boolean; far;
begin
  if Nodes <= 100 then
    WriteLn(pstring(Node.Data)^);
  if CheckOrder then begin
   if prev <> nil then
     if pstring(Node.Data)^ < Prev^ then begin
       writeln('error!');
       writeln(pstring(TStList(Container).Prev(TStListNode(Node)).Data)^);
       writeln(pstring(Node.Data)^);
       ShowNode := False;
       exit;
     end;
   prev := pstring(Node.Data);
  end;
  ShowNode := True;
end;

function NewStr(const S: string): PString;
begin
  if S = '' then Result := NullStr else
  begin
    New(Result);
    Result^ := S;
  end;
end;

begin
  list := TStlist.create(TStlistnode);

  list.Compare := SCompare;
  list.DisposeData := SDispose;

  compares := 0;
  for I := 1 to Nodes do begin
    P := RandomStr;
    list.append(newstr(p));
  end;

  prev := nil;
  checkorder := false;
  list.iterate(ShowNode, True, nil);
  writeln(compares);
  WriteLn;

  compares := 0;
  list.sort;

  prev := nil;
  checkorder := true;
  list.iterate(ShowNode, True, nil);
  writeln(compares);

  list.destroy;
  writeln('Done');
  readln;
end.

