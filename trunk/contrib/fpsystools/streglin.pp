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
 *   Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
 *     Initially adapted for use with Lazarus and FPC - 12-6-2003
 *
 * ***** END LICENSE BLOCK ***** *)

 (* Changes

 Tom Lisjac <vlx@users.sourceforge.net> http://theseus.sf.net
   Initially adapted for use with Lazarus and FPC - 12-6-2003
   All changes can be found by searching for: ////TL
   Unresolved issues/problems/needed checks can be found by searching for the
   above change string and 1 or more ! suffixes. The more ! the worse the issue.
   17-6-2003 status: compiled ok with minor changes... ready for testing

 *)

{*********************************************************}
{* TurboPower SysTools for Kylix: StRegLin.pas 1.01      *}
{*********************************************************}
{* SysTools: Component Registration                      *}
{*********************************************************}

{$I StDefine.inc}

unit StRegLin;

interface

uses
  Classes;

procedure Register;

implementation
////TL! {$R *.res}
uses
  StRegEx,
  StToHTML,
  StExpr,
  StNVBits,
  StNVColl,
  StNVCont,
  StNVDQ,
  StNVDict,
  StNVLAry,
  StNVLMat,
  StNVList,
  StNVSCol,
  StNVTree;
  ////TL ,StBarC,
  ////TL StBarPN;

procedure Register;
begin
  RegisterComponents('SysTools',
    [TStExpression,
{$ifdef GUI}
     TStExpressionEdit,
{$endif}
     TStRegEx,
     TStFileToHTML
     //,TStBarCode,
     //TStPNBarCode
     ]);

  {non-visual container class components}
  RegisterComponents('SysTools (CC)',
    [TStNVBits,
     TStNVCollection,
     TStNVDictionary,
     TStNVDQue,
     TStNVLArray,
     TStNVList,
     TStNVLMatrix,
     TStNVSortedCollection,
     TStNVTree]);
end;

end.
