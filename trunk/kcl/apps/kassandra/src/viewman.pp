{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999 - 2000  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// View Manager: Notebook widget which contains all views

{$MODE objfpc}
{$H+}

unit ViewMan;

interface
uses Classes, KCL;

type

  TGenericView = class;

  TViewManager = class(TNotebook)
  protected
    PageInfos: TCollection;
    procedure ViewModified(View: TGenericView);
    function  GetCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  AddView(View: TGenericView): Integer;
    function  GetView(index: Integer): TGenericView;
    procedure CloseView(index: Integer);
    property  Count: Integer read GetCount;
  end;


  TOnViewModified = procedure(AView: TGenericView) of object;

  TGenericView = class
  protected
    manager: TViewManager;
    FMainSubwindow: TWidget;
    FFilename: String;
    FIsModified: Boolean;
    FOnModified: TOnViewModified;
    procedure SetFileName(NewFileName: String);
    procedure SetIsModified(NewIsModified: Boolean);
  public
    constructor Create(AManager: TViewManager);
    property MainSubwindow: TWidget read FMainSubwindow;
    property FileName: String read FFileName write SetFileName;
    PageNumber: Integer;
    HasDefaultName: Boolean;		// Set if filename is not really set
    property IsModified: Boolean read FIsModified write SetIsModified;
    procedure Save; virtual;
    property OnModified: TOnViewModified read FOnModified write FOnModified;
  end;


procedure Register;

// ===================================================================
// ===================================================================
implementation

uses SysUtils;

type
  TPageInfo = class(TCollectionItem)
  protected
    View: TGenericView;
    PgIndex: Integer;
  end;


// -------------------------------------------------------------------
//   TViewManager
// -------------------------------------------------------------------

constructor TViewManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  PageInfos := TCollection.Create(TPageInfo);
end;

destructor TViewManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to PageInfos.Count - 1 do
    TPageInfo(PageInfos.Items[i]).View.Free;
  PageInfos.Free;
  inherited Destroy;
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

function TViewManager.AddView(View: TGenericView): Integer;
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
  PageInfo.PgIndex := AddPage('&' + IntToStr(PageNumber) + ' ' +
    GetViewTitle(View), View.MainSubwindow);
  View.OnModified := @ViewModified;
  View.MainSubwindow.FinishCreation;
end;
 
function TViewManager.GetView(index: Integer): TGenericView;
begin
  Result := TPageInfo(PageInfos.Items[index]).View;
end;

procedure TViewManager.CloseView(index: Integer);
begin
  RemovePage(index);
  TPageInfo(PageInfos.Items[index]).View.Free;
  PageInfos.Items[index].Free;
end;

function TViewManager.GetCount: Integer;
begin
  Result := PageInfos.Count;
end;

procedure TViewManager.ViewModified(View: TGenericView);
var
  i: Integer;
  PageInfo: TPageInfo;
  s: String;
begin
  for i := 0 to PageInfos.Count - 1 do begin
    PageInfo := TPageInfo(PageInfos.Items[i]);
    if PageInfo.View = View then begin
      s := '&' + IntToStr(View.PageNumber) + ' ' + GetViewTitle(View);
      if View.IsModified then
        s := s + ' *';
      TNotebookPage(Page[PageInfo.PgIndex]).Text := s;
      break;
    end;
  end;
end;


// -------------------------------------------------------------------
//   TGenericView
// -------------------------------------------------------------------

constructor TGenericView.Create(AManager: TViewManager);
begin
  inherited Create;
  manager := AManager;
end;

procedure TGenericView.SetFileName(NewFileName: String);
begin
  if NewFileName <> FFileName then begin
    FFileName := newFileName;
    if Assigned(OnModified) then
      OnModified(Self);
  end;
end;

procedure TGenericView.SetIsModified(NewIsModified: Boolean);
begin
  if NewIsModified <> FIsModified then begin
    FIsModified := NewIsModified;
    if Assigned(OnModified) then
      OnModified(Self);
  end;
end;

procedure TGenericView.Save;
begin
  IsModified := False;
end;


// -------------------------------------------------------------------
//   Miscellaneous
// -------------------------------------------------------------------

procedure Register;
var
  Components: array[0..0] of TComponentClass;
begin
  Components[0] := TViewManager;
  RegisterComponents('Kassandra', Components);
end;

end.


{
  $Log$
  Revision 1.3  2000/02/10 18:25:18  sg
  * Changed method names (removed "F" at start)
  * Added destructor to remove memory leaks...

  Revision 1.2  2000/01/24 00:34:18  sg
  * The page title label now underlines the page number (instead of "[1]" etc.)

  Revision 1.1  2000/01/05 19:26:34  sg
  * First version (composited of the old 'view_notebook.pp' and 'vw_generic.pp')

  Revision 1.1.1.1  1999/12/30 21:32:52  sg
  Initial import

}
