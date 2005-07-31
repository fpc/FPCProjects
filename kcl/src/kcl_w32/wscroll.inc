{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// ===================================================================
//   ScrollBox widget implementation for W32
// ===================================================================
  

type
  TWin32Range = class(TRange)
  private
  protected
    ScrollBox: TScrollBox;
    SBar: integer;
    function  GetMaxValue: Integer; override;
    procedure SetMaxValue(NewMaxValue: Integer); override;
    function  GetCurValue: Integer; override;
    procedure SetCurValue(NewCurValue: Integer); override;
    function  GetPageSize: Integer; override;
    procedure SetPageSize(NewPageValue: Integer); override;
  public
    constructor Create(AScrollBox: TScrollBox; isVert: boolean);
    function GetInfo(mask: integer; var info: ScrollInfo): boolean;
    procedure SetInfo(mask: integer; var info: ScrollInfo);
  end;     


procedure TScrollbox_UpdateContent(sbox: TScrollbox);
var 
  r: windows.rect;
  w, h: integer;
begin
  with sbox do
  begin
    GetClientRect(Handle.Window, @r);
    HorzRange.PageSize := r.right;
    VertRange.PageSize := r.bottom;
    w := HorzRange.MaxValue-HorzRange.PageSize+1;
    h := VertRange.MaxValue-VertRange.PageSize+1;
    SetWindowPos(Content.Handle.Window, -1, 
      -HorzRange.CurValue, -VertRange.CurValue, w, h, swp_nozorder);

    Content.OnSizeChanged;

    if (w<r.right)or(h<r.bottom) then
    begin
      GetWindowRect(Handle.Window, @r);
      RedrawWindow(Handle.Window, r, 0, rdw_InternalPaint or 
          rdw_Erase or rdw_Frame or rdw_Invalidate or rdw_UpdateNow);
    end;  
  end;
end;


constructor TWin32Range.Create(AScrollBox: TScrollBox; isVert: boolean);
begin
  inherited Create;
  ScrollBox := AScrollBox;
  if isVert then SBar := sb_Vert
  else SBar := sb_Horz;
end;

function TWin32Range.GetInfo(mask: integer; var info: ScrollInfo): boolean;
begin
  info.cbSize := SizeOf(Scrollinfo);
  info.fMask := mask;
  result := GetScrollInfo(Scrollbox.Handle.Window, sbar, @info);
end;

procedure TWin32Range.SetInfo(mask: integer; var info: ScrollInfo);
begin
  info.cbSize := SizeOf(Scrollinfo);
  info.fMask := mask;
  SetScrollInfo(Scrollbox.Handle.Window, sbar, @info, true);
end;

function TWin32Range.GetMaxValue: Integer;
var 
  info: ScrollInfo;
begin
  if GetInfo(sif_Range, info) then result := info.nMax + PageSize
  else result := -1;
end;

procedure TWin32Range.SetMaxValue(NewMaxValue: Integer);
var 
  info: ScrollInfo;
begin
  info.nMin := 0;
  info.nMax := NewMaxValue;{-PageSize;}
  SetInfo(sif_Range, info);
  TScrollbox_UpdateContent(Scrollbox);
end;

function TWin32Range.GetCurValue: Integer;
var 
  info: ScrollInfo;
begin
  if GetInfo(sif_Pos, info) then result := info.nPos
  else result := -1;
end;

procedure TWin32Range.SetCurValue(NewCurValue: Integer);
var 
  info: ScrollInfo;
begin
  info.nPos := NewCurValue;
  SetInfo(sif_Pos, info);
  TScrollbox_UpdateContent(Scrollbox);
end;

function TWin32Range.GetPageSize: Integer;
var 
  info: ScrollInfo;
begin
  if GetInfo(sif_Page, info) then result := info.nPage
  else result := -1;
end;

procedure TWin32Range.SetPageSize(NewPageValue: Integer);
var 
  info: ScrollInfo;
begin
  info.nPage := NewPageValue;
  SetInfo(sif_Page, info);
end;


function TScrollbox_ProcessMessages(Widget: TWidget; AMessage, WParam, LParam: Longint): Longint; 
var
  range: TRange;
begin
  result := 0;
  case aMessage of
    WM_VScroll, WM_HScroll:
    begin
      if amessage=wm_vscroll then range := TScrollbox(Widget).VertRange
      else range := TScrollbox(Widget).HorzRange;

      case loword(wparam) of

        sb_ThumbPosition, sb_ThumbTrack:
            Range.CurValue := hiword(wparam);

        sb_PageDown:
            Range.CurValue := Range.CurValue + Range.PageSize;

        sb_PageUp:
            Range.CurValue := Range.CurValue - Range.PageSize;

        sb_Bottom:
            Range.CurValue := Range.MaxValue;

        sb_Top:
            Range.CurValue := 0;

        sb_LineDown:
            Range.CurValue := Range.CurValue + 1;

        sb_LineUp:
            Range.CurValue := Range.CurValue - 1;

      end;

    end;
  end;
end;

constructor TScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHorzRange := TWin32Range.Create(self, false);
  FVertRange := TWin32Range.Create(self, true); 
end;

procedure TScrollBox.SetContent(AContent: TWidget);
begin
  FContent := AContent;
  FContent.FParent := Self;
end;

procedure TScrollBox.OnFinishCreation;
var wnd: WndClass;
begin
  if CreationState = wcsCreationDone then exit;
  FCreationState := wcsCreationInProgress;

  CreateWin32(self, 'KCLWIDGET', WS_EX_CONTROLPARENT or ws_ex_clientedge, WS_CHILD or WS_TABSTOP or ws_vscroll or ws_hscroll);
  Handle.ProcessMessages := @TScrollbox_ProcessMessages;

  if FContent<>nil then FContent.FinishCreation;
        
  FHorzRange.MaxValue := 200;
  FHorzRange.PageSize := 100;
  FVertRange.MaxValue := 200;
  FVertRange.PageSize := 100;   

  inherited OnFinishCreation;
end;

procedure TScrollBox.OnSetSize;
begin
    inherited OnSetSize;
    if FContent<>nil then TScrollbox_UpdateContent(self);
end;

procedure TScrollBox.OnSizeChanged;
begin
    inherited OnSizeChanged;
    if FContent<>nil then TScrollbox_UpdateContent(self);
end;

procedure TScrollBox.OnRecalcLayout;
begin
  if FContent<>nil then TScrollbox_UpdateContent(self);
end;



constructor THorzScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FRange := TWin32Range.Create;
end;

procedure THorzScrollBar.OnFinishCreation;
begin
  FCreationState := wcsCreationInProgress;
  CreateWin32(self, 'SCROLLBAR', 0, WS_CHILD or WS_TABSTOP or SBS_HORZ);
  inherited OnFinishCreation;
end;


constructor TVertScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  FRange := TWin32Range.Create;
end;

procedure TVertScrollBar.OnFinishCreation;
begin
  FCreationState := wcsCreationInProgress;
  CreateWin32(self, 'SCROLLBAR', 0, WS_CHILD or WS_TABSTOP or SBS_VERT);
  // *** Scroll bar not yet functional
  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:01  sg
  Initial revision

}
