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
   17-6-2003 status: compiled ok with no changes... ready for testing

 *)

{*********************************************************}
{* TurboPower SysTools for Kylix:StNVSCol.pas 1.01       *}
{*********************************************************}
{* SysTools: non visual component for TStSortedCollection*}
{*********************************************************}

{$I StDefine.inc}

unit StNVSCol;

interface

uses
  Classes,
  StBase, StColl, StNVCont;

type
  TStNVSortedCollection = class(TStNVContainerBase)
  {.Z+}
  protected {private}
    {property variables}
    FContainer    : TStSortedCollection; {instance of the container}
    FDuplicates   : Boolean;
    FPageElements : Integer;

    {property methods}
    procedure SetDuplicates(Value : Boolean);
    procedure SetPageElements(Value : Integer);

    {internal methods}
    procedure RecreateContainer;

  protected
    function GetOnCompare : TStCompareEvent;
      override;
    function GetOnDisposeData : TStDisposeDataEvent;
      override;
    function GetOnLoadData : TStLoadDataEvent;
      override;
    function GetOnStoreData : TStStoreDataEvent;
      override;
    procedure SetOnCompare(Value : TStCompareEvent);
      override;
    procedure SetOnDisposeData(Value : TStDisposeDataEvent);
      override;
    procedure SetOnLoadData(Value : TStLoadDataEvent);
      override;
    procedure SetOnStoreData(Value : TStStoreDataEvent);
      override;

  public
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
  {.Z-}

    property Container : TStSortedCollection
      read FContainer;

  published
    property Duplicates : Boolean
      read FDuplicates
      write SetDuplicates;

    property PageElements : Integer
      read FPageElements
      write SetPageElements;

    property OnCompare;
    property OnDisposeData;
    property OnLoadData;
    property OnStoreData;
  end;


implementation

{*** TStNVSortedCollection ***}

constructor TStNVSortedCollection.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  {defaults}
  FPageElements := 1000;

  if Classes.GetClass(TStSortedCollection.ClassName) = nil then
    RegisterClass(TStSortedCollection);

  FContainer := TStSortedCollection.Create(FPageElements);
end;

destructor TStNVSortedCollection.Destroy;
begin
  FContainer.Free;
  FContainer := nil;

  inherited Destroy;
end;

function TStNVSortedCollection.GetOnCompare : TStCompareEvent;
begin
  Result := FContainer.OnCompare;
end;

function TStNVSortedCollection.GetOnDisposeData : TStDisposeDataEvent;
begin
  Result := FContainer.OnDisposeData;
end;

function TStNVSortedCollection.GetOnLoadData : TStLoadDataEvent;
begin
  Result := FContainer.OnLoadData;
end;

function TStNVSortedCollection.GetOnStoreData : TStStoreDataEvent;
begin
  Result := FContainer.OnStoreData;
end;

procedure TStNVSortedCollection.RecreateContainer;
var
  HoldOnCompare     : TStCompareEvent;
  HoldOnDisposeData : TStDisposeDataEvent;
  HoldOnLoadData    : TStLoadDataEvent;
  HoldOnStoreData   : TStStoreDataEvent;
begin
  HoldOnCompare := FContainer.OnCompare;
  HoldOnDisposeData := FContainer.OnDisposeData;
  HoldOnLoadData := FContainer.OnLoadData;
  HoldOnStoreData := FContainer.OnStoreData;
  FContainer.Free;
  FContainer := TStSortedCollection.Create(FPageElements);
  FContainer.OnCompare := HoldOnCompare;
  FContainer.OnDisposeData := HoldOnDisposeData;
  FContainer.OnLoadData := HoldOnLoadData;
  FContainer.OnStoreData := HoldOnStoreData;
end;

procedure TStNVSortedCollection.SetOnCompare(Value : TStCompareEvent);
begin
  FContainer.OnCompare := Value;
end;

procedure TStNVSortedCollection.SetOnDisposeData(Value : TStDisposeDataEvent);
begin
  FContainer.OnDisposeData := Value;
end;

procedure TStNVSortedCollection.SetOnLoadData(Value : TStLoadDataEvent);
begin
  FContainer.OnLoadData := Value;
end;

procedure TStNVSortedCollection.SetOnStoreData(Value : TStStoreDataEvent);
begin
  FContainer.OnStoreData := Value;
end;

procedure TStNVSortedCollection.SetDuplicates(Value : Boolean);
begin
  FDuplicates := Value;
  FContainer.Duplicates := FDuplicates;
end;

procedure TStNVSortedCollection.SetPageElements(Value : Integer);
begin
  FPageElements := Value;
  RecreateContainer;
  FContainer.Duplicates := FDuplicates;
end;


end.
