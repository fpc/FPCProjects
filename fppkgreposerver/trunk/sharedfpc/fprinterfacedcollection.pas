unit fprInterfacedCollection;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  { TfprInterfacedCollectionItem }

  TfprInterfacedCollectionItem= class(TCollectionItem, IInterface)
  private
    FOwnerInterface: IInterface;
  protected
    { IInterface }
    function _AddRef: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: Longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} IID: TGUID; out Obj): HResult; virtual; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    procedure AfterConstruction; override;
  end;

implementation

{ TfprInterfacedCollectionItem }

function TfprInterfacedCollectionItem._AddRef: Longint; cdecl;
begin
  if assigned(FOwnerInterface) then
    Result:=FOwnerInterface._AddRef
  else
    Result:=-1;
end;

function TfprInterfacedCollectionItem._Release: Longint; cdecl;
begin
  if assigned(FOwnerInterface) then
    Result:=FOwnerInterface._Release
  else
    Result:=-1;
end;

function TfprInterfacedCollectionItem.QueryInterface(constref IID: TGUID; out Obj): HResult; cdecl;
begin
  if GetInterface(IID, Obj) then
    Result:=0
  else
    Result:=HResult($80004002);
end;

procedure TfprInterfacedCollectionItem.AfterConstruction;
var TheOwner: TPersistent;
begin
  inherited;
  TheOwner:=GetOwner;
  if assigned(TheOwner) then
    TheOwner.GetInterface(IUnknown,FOwnerInterface);
end;

end.

