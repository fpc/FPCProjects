{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// Notebook widget which contains all views

{$MODE objfpc}
{$H+}

unit view_notebook;

interface
uses Classes, KCL, vw_generic;

type

  TViewNotebook = class(TNotebook)
  protected
    PageInfos: TCollection;
    procedure ViewModified(View: TGenericView);
    function FGetCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function  AddView(View: TGenericView): Integer;
    function  GetView(index: Integer): TGenericView;
    procedure CloseView(index: Integer);
    property  Count: Integer read FGetCount;
  end;


procedure Register;


implementation

uses SysUtils;

type
  TPageInfo = class(TCollectionItem)
  protected
    View: TGenericView;
    PgIndex: Integer;
  end;


constructor TViewNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PageInfos := TCollection.Create(TPageInfo);
end;

function GetViewTitle(View: TGenericView): String;
var
  s: String;
  i: Integer;
begin
  // Cut path from filename for page title
  s := View.FileName;
  for i := Length(s) downto 1 do
    if (s[i] = '/') or (s[i] = '\') then begin
      s := Copy(s, i + 1, Length(s));
      break;
    end;
  Result := s;
end;

function TViewNotebook.AddView(View: TGenericView): Integer;
var
  child: TGenericView;
  i, PageNumber: Integer;
  found: Boolean;
  PageInfo: TPageInfo;
begin
  PageNumber := 1;
  found := False;
  for i := 0 to PageInfos.Count - 1 do begin
    child := TPageInfo(PageInfos.Items[i]).View;
    if child.PageNumber <> PageNumber then begin
      found := True;
      break;
    end;
    Inc(PageNumber);
  end;
  if not found then PageNumber := PageInfos.Count + 1;
  View.PageNumber := PageNumber;

  Result := PageInfos.Count;
  PageInfo := TPageInfo(PageInfos.Add);
  PageInfo.View := View;
  PageInfo.PgIndex := AddPage('[' + IntToStr(PageNumber) + '] ' +
    GetViewTitle(View), View.MainSubwindow);
  View.OnModified := @ViewModified;
  View.MainSubwindow.FinishCreation;
end;

function TViewNotebook.GetView(index: Integer): TGenericView;
begin
  Result := TPageInfo(PageInfos.Items[index]).View;
end;

procedure TViewNotebook.CloseView(index: Integer);
begin
  RemovePage(index);
  PageInfos.Items[index].Free;
end;

function TViewNotebook.FGetCount: Integer;
begin
  Result := PageInfos.Count;
end;

procedure TViewNotebook.ViewModified(View: TGenericView);
var
  i: Integer;
  PageInfo: TPageInfo;
  s: String;
begin
  for i := 0 to PageInfos.Count - 1 do begin
    PageInfo := TPageInfo(PageInfos.Items[i]);
    if PageInfo.View = View then begin
      s := '[' + IntToStr(View.PageNumber) + '] ' + GetViewTitle(View);
      if View.IsModified then
        s := s + ' *';
      TNotebookPage(Page[PageInfo.PgIndex]).Text := s;
      break;
    end;
  end;
end;


procedure Register;
var
  Components: array[0..0] of TComponentClass;
begin
  Components[0]  := TViewNotebook;
  RegisterComponents('Kassandra', Components);
end;

end.


{
  $Log$
  Revision 1.1  1999/12/30 21:32:52  sg
  Initial revision

}
