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
//   Notebook widget implementation for Win32
// ===================================================================

procedure InsertTabPage(ctrl: HWnd; idx: integer; text: string);
var
  item: TC_ITEM;
begin
  with item do
  begin
    mask := TCIF_Text;
    pszText := PChar(Text);
    cchTextMax := length(Text);
  end;
  TabCtrl_InsertItem(ctrl, idx, item);
end;

procedure TNotebook_RedrawContent(nbook: TNotebook);    
var winrect: _Rect;
begin
  with nbook do
  begin
    if CurPage<>nil then
      if CurPage.Content<>nil then 
      begin
        GetWindowRect(CurPage.Content.Handle.Window, @winrect);
        RedrawWindow(CurPage.Content.Handle.Window, winrect, 0, rdw_InternalPaint or 
          rdw_Erase or rdw_Frame or rdw_Invalidate or rdw_UpdateNow);
      end;
  end;
end;

procedure UpdateTabContentSize(wnd: HWnd; apage: TNotebookPage);
var 
  r: _rect;
begin
  if apage.content<>nil then
  begin
    GetClientRect(wnd, @r);
    TabCtrl_AdjustRect(wnd, false, r);
    SetWindowPos(apage.content.Handle.Window, -1, r.left, r.top, r.right-r.left, r.bottom-r.top, swp_nozorder);
  end;
end;  

procedure TNotebook_UpdateVisible(nbook: TNotebook);         
var 
  i: integer;
begin
  with nbook do
  begin
    for i:=0 to FPages.count-1 do
      if Page[i].Content<>nil then
      begin
        if i=CurPageIndex then 
        begin
          UpdateTabContentSize(nbook.handle.Window, page[i]);
          ShowWindow(Page[i].Content.Handle.Window, sw_Show);
        end
        else ShowWindow(Page[i].Content.Handle.Window, sw_Hide);
      end;
  end;
end;


function TNotebook_ProcessMessages(Widget: TWidget; AMessage, WParam, LParam: Longint): Longint; 
var 
  nbook: TNotebook;
  n: ^NMHDR;
begin
  result := 0;
  nbook := TNotebook(widget);
  case AMessage of
   wm_Notify:
    begin
      n := pointer(lparam);
      case n^.code of
        TCN_SelChange: 
        begin
          TNotebook_UpdateVisible(nbook);
          if Assigned(nbook.FOnPageChange) then nbook.FOnPageChange(nbook, 0, nbook.CurPageIndex);
        end;
      end;
    end;   
  end;
end;


procedure TNotebookPage.SetText(const AText: String);
var
  item: TC_ITEM;
begin
  FText := AText;
  with item do
  begin
    mask := TCIF_Text;
    pszText := PChar(AText);
    cchTextMax := length(AText);
  end;
  TabCtrl_SetItem(TNotebook(priv.notebook).Handle.Window, Index, item);
  TNotebook_RedrawContent(TNotebook(priv.notebook));
end;

procedure TNotebookPage.SetContent(AContent: TWidget);
begin
  FContent := AContent;
end;


// -------------------------------------------------------------------
//   Notebook
// -------------------------------------------------------------------

constructor TNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TCollection.Create(TNotebookPage);
end;

function TNotebook.GetPage(Aindex: Integer): TNotebookPage;
begin
  Result := TNotebookPage(FPages.Items[Aindex]);
end;

function TNotebook.GetCurPage: TNotebookPage;
begin
    if CurPageIndex>-1 then
        Result := Page[CurPageIndex]
    else
        Result := nil;
end;

function TNotebook.GetCurPageIndex: Integer;
begin
  Result := TabCtrl_GetCurSel(Handle.Window);
end;

procedure TNotebook.SetCurPageIndex(AIndex: Integer);
begin
    TabCtrl_SetCurSel(Handle.Window, AIndex);
    TNotebook_UpdateVisible(self);
end;

procedure TNotebook.OnFinishCreation;
var
  i: integer;
begin
  if CreationState = wcsCreationDone then exit;
  FCreationState := wcsCreationInProgress;

  CreateWin32(self, wc_TabControl, 0, ws_Child);
  Handle.ProcessMessages := @TNotebook_ProcessMessages;

  for i:=0 to FPages.count-1 do
  begin
    InsertTabPage(Handle.Window, i, page[i].Text);
    Page[i].Priv.Notebook := self;
    if page[i].Content<>nil then page[i].Content.FinishCreation;
  end;
  TNotebook_UpdateVisible(self);

  inherited OnFinishCreation;
end;

function TNotebook.AddPage(AText: String; AWidget: TWidget): Integer;
var
  NewPage: TNotebookPage;
begin
  Result := FPages.Count;
  if AWidget<>nil then AWidget.FParent := Self;
  NewPage := TNotebookPage(FPages.Add);
  NewPage.FText := AText;
  NewPage.FContent := AWidget;
  NewPage.Priv.Notebook := self;
  if CreationState=wcsCreationDone then 
  begin
    InsertTabPage(Handle.Window, result, AText);
    AWidget.FinishCreation;
    TNotebook_UpdateVisible(self);
  end;
end;

procedure TNotebook.RemovePage(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= FPages.Count) then exit;

  //if anyone can tell me, why I have to free the content _before_
  //I delete the tab, don't hesitate...

  TNotebookPage(FPages.Items[AIndex]).Content.Free;
  FPages.Items[AIndex].Free;   

  SendMessage(Handle.Window, tcm_DeleteItem, AIndex, 0);

  //---

  Dec(AIndex);
  if AIndex<0 then AIndex:=0;
  CurPageIndex := AIndex;   
end;

procedure TNotebook.OnSizeChanged;
begin
  inherited OnSizeChanged;
  if CurPageIndex>-1 then UpdateTabContentSize(Handle.Window, Page[CurPageIndex]);
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:07  sg
  Initial revision

}
