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
{*                    TLIST.DPR 1.05                     *}
{*                     Test Program                      *}
{*********************************************************}

program TList;

{$IFDEF Win32}
 {$APPTYPE CONSOLE}
{$ENDIF}

{$I STDEFINE.INC}

uses
  sysutils, classes, stbase, stlist
{$IFNDEF OS32}
  , wincrt
{$ENDIF}
  ;
const
  Nodes = 10;
  NodeLen = 5;
type
  TNodeName = string[NodeLen];
  PNodeName = ^TNodeName;
var
  list : TStlist;
  i : longint;
  compares : longint;
  prev : TNodeName;
  checkorder : boolean;

function SCompare(Data1, Data2 : Pointer) : Integer; far;
begin
  inc(compares);
  if compares mod 10000 = 0 then
    writeln(compares);
  SCompare := CompareStr(PNodeName(Data1)^, PNodeName(Data2)^);
end;

procedure SDispose(Data : Pointer); far;
begin
  FreeMem(Data, NodeLen+1);
end;

function RandomStr : PNodeName;
var
  i : integer;
  S : TNodeName;
begin
  S[0] := Char(NodeLen);
  for I := 1 to NodeLen do
    S[I] := char(byte('a')+random(26));
  GetMem(Result, NodeLen+1);
  Result^ := S;
end;

function ShowNode(Container : TStContainer;
                  Node : TStNode;
                  OtherData : Pointer) : Boolean; far;
begin
  if Nodes <= 100 then
    WriteLn(PNodeName(Node.Data)^);
  if CheckOrder then begin
   if PNodeName(Node.Data)^ < Prev then begin
     writeln('error!');
     writeln(PNodeName(TStList(Container).Prev(TStListNode(Node)).Data)^);
     writeln(PNodeName(Node.Data)^);
     ShowNode := False;
     exit;
   end;
   prev := PNodeName(Node.Data)^;
  end;
  ShowNode := True;
end;

procedure StoreNode(Writer : TWriter; Data : pointer); far;
  begin
    Writer.WriteString(PNodeName(Data)^);
  end;

function LoadNode(Reader : TReader) : pointer; far;
  var
    PS: PNodeName;
  begin
    New(PS);
    PS^ := Reader.ReadString;
    Result := PS;
  end;

begin
  list := TStlist.create(TStlistnode);

  list.Compare := SCompare;
  list.DisposeData := SDispose;

  compares := 0;
  for I := 1 to Nodes do
    list.append(randomstr);

  prev := '';
  checkorder := false;
  list.iterate(ShowNode, True, nil);
  writeln(compares);
  WriteLn;

  compares := 0;
  list.sort;

  prev := '';
  checkorder := true;
  list.iterate(ShowNode, True, nil);
  writeln(compares);

  list.StoreData := StoreNode;
  list.StoreToFile('Test.stm');
  list.destroy;

  RegisterClasses([TStList, TStListNode]);
  list := TStList.Create(TStListNode);
  list.Compare := SCompare;
  list.DisposeData := SDispose;
  list.LoadData := LoadNode;
  list.LoadFromFile('Test.stm');

  prev := '';
  checkorder := true;
  list.iterate(ShowNode, True, nil);
  list.destroy;

  WriteLn('Done');
  readln;
end.
