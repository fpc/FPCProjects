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
{*                    TDQUE.DPR 1.05                     *}
{*                     Test Program                      *}
{*********************************************************}

program TDQue;

{$I STDEFINE.INC}

{$IFDEF Win32}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  stbase,
  stlist,
  stdque
{$IFNDEF OS32}
  , wincrt
{$ENDIF}
  ;

const
  Nodes = 20;
var
  DQ : TStDQue;
  I, J : LongInt;

begin
  DQ := TStDQue.Create(TStListNode);

  for I := 1 to Nodes do
    DQ.PushTail(Pointer(I));

  {queue operation}
  for I := 1 to Nodes do begin
    DQ.PeekHead(Pointer(J));
    DQ.PopHead;
    writeln(J);
  end;

(*
  {stack operation}
  for I := 1 to Nodes do begin
    DQ.PeekTail(Pointer(J));
    DQ.PopTail;
    writeln(J);
  end;
*)

  DQ.Free;
  readln;
end.
