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

var
  globalMenuDown: boolean;


function FindMenuItem(menuitem: TMenuItem; ID: word): TMenuItem;
var 
  i: integer;      
  item: TMenuItem;
begin
  if menuitem.FHandle.ID = ID then 
  begin
    result := menuitem;
    exit;
  end
  else if menuitem is TMenu then
  begin
    for i:=0 to TMenu(menuitem).FItems.count-1 do
    begin
      item := FindMenuItem(TMenuItem(TMenu(menuitem).FItems[i]), ID);
      if item<>nil then
      begin
        result := item;
        exit;
      end;
    end;
  end;
  result := nil;
end;

procedure TMenuBar_DrawContent(AWidget: TMenuBar; ACanvas: TCanvas; const ARect: TRect);
var 
  item: TMenuItem;
  x, i, old, idx, tw: integer;
begin
with AWidget, acanvas do
  begin
    color := GetSysColor(COLOR_MENU);
    fillrect(arect);
    idx := longint(AWidget.Handle.Tag);
    old := round(font.height);
    font.height := 10;
    color := GetSysColor(COLOR_MENUTEXT);
    x := 6;
    for i:=0 to FItems.count-1 do
    begin
      item := TMenuItem(FItems.Items[i]);
      tw := font.GetTextWidth(item.Text);
      if i=idx then 
      begin
        color := colGray;
        line(x-5,1,x-5, FHeight-1);
        line(x-5,1,x+tw+5, 1);
        color := colWhite;
        line(x+tw+5, 1, x+tw+5, FHeight);
        line(x-4, FHeight-1, x+tw+5, FHeight-1);
        color := GetSysColor(COLOR_MENUTEXT);
        Text(x+1, 4, item.Text);
      end
      else Text(x, 3, item.Text);
      x := x + tw +12;
    end;
    font.height := old;
  end;
end;

procedure TMenuBar_ButtonPressed(AWidget: TMenuBar; AX, AY: integer; AModifiers: TModifiers);
var 
  r, r2: Windows.rect;
  afont: TFont;
  x,i,idx,tw: integer;
  item: TMenuItem;
begin
  with awidget do
  begin
     GetWindowRect(Handle.Window, @r);
     x := 0;
     idx := -1;
     aFont := TFont.Create;
     aFont.height := 10;
     for i:=0 to FItems.count-1 do
     begin
        item := TMenuItem(FItems.Items[i]);
        tw := afont.GetTextWidth(item.Text)+12;
        if (ax>x)and(ax<x+tw) then 
        begin
          idx := i;
          break;
        end;
        x := x+ tw;
     end;
     aFont.Free;
     if idx>=0 then
     begin
       Handle.Tag := pointer(idx);
       Redraw;
       TrackPopupMenu(TMenuItem(FItems.Items[idx]).FHandle.ID, TPM_LEFTALIGN, r.left+x, r.top+FHeight, 0, Handle.Window, r2);
     end;
  end;
end;

function TMenuBar_ProcessMessages(widget: TWidget; AMessage, WParam, LParam: Longint): Longint; 
var
  menubar: TMenubar;
  item: TMenuItem;
  i: integer;
  Canvas: TW32Canvas;
begin
  result := 0;
  menubar := TMenubar(widget);
  case AMessage of

    wm_Command:
      begin
        globalMenuDown := false;
        for i:=0 to menubar.FItems.count-1 do
        begin
          item := FindMenuItem(TMenuItem(menubar.FItems[i]), loWord(wparam));
          if item<>nil then
          begin
            if Assigned(item.OnClick) then item.OnClick(item);
            exit;
          end;
        end;
      end;

    wm_ExitMenuLoop:
      if wparam = 1 then 
      begin
        globalMenuDown := false;
        menubar.Handle.Tag := Pointer(-1);
        menubar.Redraw;
      end;

    wm_Paint:
        begin
          Canvas := TW32Canvas.Create(widget);
          TMenuBar_DrawContent(menubar, Canvas, Canvas.RedrawRect);
          Canvas.Free;
          Exit;
        end;      

    wm_LButtonDown:
        begin
          TMenubar_ButtonPressed(menubar, loWord(lparam), hiWord(lparam), [modLButton]);
          globalMenuDown := true;
          exit;
        end;

    //not working, because trackpopupmenu captures the mouse:
 {   wm_MouseMove:
      if globalMenuDown then
        TMenubar_ButtonPressed(menubar, loWord(lparam), hiWord(lparam), []);
        }
  end;   
end;


// -------------------------------------------------------------------
//   TMenuItem
// -------------------------------------------------------------------

constructor TMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TMenuItem.SetText(const AText: String);
var 
  menuinfo: MenuItemInfo;
begin
  FText := AText;
  with menuinfo do
  begin
    cbSize := SizeOf(MenuitemInfo);
    fMask := miim_Type;
    if FText='-' then fType := mft_Separator
    else fType := mft_String;
    if FText<>'-' then
    begin
      dwTypeData := PChar(FText);
      cch := length(FText)+1;
    end;
  end;
  SetMenuItemInfo(FHandle.Menu, FHandle.ID, false, @menuinfo);
end;

procedure TMenuItem.FSetGray(AGray: Boolean);
begin
  if AGray <> FGray then 
  begin
    FGray := AGray;
    if FHandle.Menu<>0 then
    begin
        if FGray then EnableMenuItem(FHandle.Menu, FHandle.ID, mf_ByCommand or mf_Grayed)
        else EnableMenuItem(FHandle.Menu, FHandle.ID, mf_ByCommand or mf_Enabled);
    end;
  end;
end;

procedure TMenuItem.OnFinishCreation;
var 
  gr: integer;
begin
  if FGray then gr := mf_Grayed
  else gr := 0;
  if FText = '-' then AppendMenu(FHandle.Menu, mf_Separator, 0, nil)
  else AppendMenu(FHandle.Menu, mf_String or gr, FHandle.ID, PChar(FText));
end;


// -------------------------------------------------------------------
//   TMenu
// -------------------------------------------------------------------

constructor TMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FHandle.Menu := 0;
end;

procedure TMenu.OnFinishCreation;
var
  i: Integer;
  item: TMenuItem;
  gr: integer;
begin
  if FGray then gr := mf_Grayed
  else gr := 0;
  FHandle.ID := CreateMenu;
  AppendMenu(FHandle.Menu, mf_Popup or mf_String or gr, FHandle.ID, PChar(FText));
  for i := 0 to FItems.Count - 1 do 
  begin
    item := TMenuItem(FItems.Items[i]);
    item.FHandle.Menu := FHandle.ID;
    item.FHandle.ID := 1000+i;
    item.OnFinishCreation;
  end;  
end;

procedure TMenu.AddItem(AItem: TMenuItem);
begin
  FItems.Add(AItem);
  if FHandle.Menu<>0 then 
  begin
    AItem.FHandle.Menu := FHandle.Menu;
    Aitem.FHandle.ID := FItems.count-1;
    AItem.OnFinishCreation;
  end;
end;


// -------------------------------------------------------------------
//   TMenuBar
// -------------------------------------------------------------------

constructor TMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FHeight := 20;
end;

procedure TMenuBar.AddItem(AItem: TMenuItem);
begin
  FItems.Add(AItem);
  if Handle.Window<>0 then 
  begin
    AItem.FHandle.Menu := Handle.Window;
    Aitem.FHandle.ID := 1000+FItems.count-1;
    AItem.OnFinishCreation;
  end;
end;

procedure TMenuBar.OnFinishCreation;
var
  i: Integer;
  item: TMenuItem;
  p: TComponent;
begin
  FCreationState := wcsCreationInProgress;

  for i := 0 to FItems.Count - 1 do 
  begin
    item := TMenuItem(FItems.Items[i]);
    item.FHandle.ID := 1000+i;
    item.FHandle.Menu := CreatePopupMenu;
    item.OnFinishCreation;
  end;       

  CreateWin32(self, 'KCLWIDGET', 0, ws_Child);
  Handle.Tag := pointer(-1);
  Handle.ProcessMessages := @TMenubar_ProcessMessages;

  inherited OnFinishCreation;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:58  sg
  Initial revision

}
