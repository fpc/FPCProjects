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
//   Win32 Splitter Widget Implementation
// ===================================================================

type
  TSplitterExtra = class
    PropValue: Single;
    DividerSize: Longint;
  end;

var
  _cursorofs: integer;

procedure TSplitter_UpdatePanes(splitter: TSplitter);
var
  p: Longint;
begin
  with splitter do
    if (Pane1<>nil) and (Pane2<>nil) and (Handle.Tag<>nil) then
      with TSplitterExtra(Handle.Tag) do
      begin
        if Orientation = soHorizontal then
        begin
          case ResizePolicy of
            srProportional: p := round(PropValue*FHeight) - dividerSize div 2;
            srFixedPane1:   p := Pane1.Height;
            srFixedPane2:   p := Height - Pane2.Height - dividerSize;
          end;
          SetWindowPos(Pane1.Handle.Window, -1, 0, 0, FWidth, p, swp_NoZOrder);
          SetWindowPos(Pane2.Handle.Window, -1, 0, p + dividerSize, FWidth, FHeight - p - dividerSize, swp_NoZOrder);
        end
        else
        begin
          case ResizePolicy of
            srProportional: p := round(PropValue*FWidth) - dividerSize div 2;
            srFixedPane1:   p := Pane1.Width;
            srFixedPane2:   p := Width - Pane2.Width - dividerSize;
          end;
          SetWindowPos(Pane1.Handle.Window, -1, 0, 0, p, FHeight,  swp_NoZOrder);
          SetWindowPos(Pane2.Handle.Window, -1, p + dividerSize, 0, FWidth - p - dividerSize, FHeight, swp_NoZOrder);
        end;
      end;
end;

procedure TSplitter_SetCursor(splitter: TSplitter);
begin
  if splitter.Orientation = soHorizontal then 
  begin
    SetCursor(_syscurSizeNS);
  end
  else 
    SetCursor(_syscurSizeWE);
end;

function TSplitter_ProcessMessages(widget: TWidget; AMessage, WParam, LParam: Longint): Longint; 
var
  splitter: TSplitter;

function MouseOverDivider: boolean;
begin
  result := false;
  with splitter do
  begin
    if Orientation=soHorizontal then
    begin
      if (hiWord(LParam)>Pane1.Height)and
        (hiWord(LParam)<Pane1.Height+TSplitterExtra(Handle.Tag).DividerSize) then
          result := true;
    end
    else
      if (loWord(LParam)>Pane1.Width)and
        (loWord(LParam)<Pane1.Width+TSplitterExtra(Handle.Tag).DividerSize) then
          result := true;
  end;
end;

begin
  result := 0;
  splitter := TSplitter(widget);
  with splitter do
    if (Pane1<>nil)and(Pane2<>nil) then
      case AMessage of

        wm_LButtonDown:
          if MouseOverDivider then
          begin
            SetCapture(Handle.Window);
            if Orientation = soHorizontal then _cursorofs := hiWord(LParam) - Pane1.Height
            else _cursorofs := loWord(LParam) - Pane1.Width;
            TSplitter_SetCursor(splitter);
          end;

        wm_MouseMove:
          begin
            if (WParam = mk_LButton)and(GetCapture=Handle.Window) then
            begin  
              if Orientation = soHorizontal then
              begin
                case ResizePolicy of
                  srProportional: TSplitterExtra(Handle.Tag).PropValue := (hiWord(LParam)-_cursorofs)/Height;
                  srFixedPane1: Pane1.Height := hiWord(LParam) - _cursorofs;
                  srFixedPane2: Pane2.Height := Height - hiWord(LParam) - _cursorofs;
                end;
              end
              else
              begin
                case ResizePolicy of
                  srProportional: TSplitterExtra(Handle.Tag).PropValue := (loWord(LParam)-_cursorofs)/Width;
                  srFixedPane1: Pane1.Width := loWord(LParam) - _cursorofs;
                  srFixedPane2: Pane2.Width := Width - loWord(LParam) - _cursorofs;
                end;
              end;
              TSplitter_UpdatePanes(splitter);
            end;
            if MouseOverDivider then
            begin
              TSplitter_SetCursor(splitter);
              result := 1;
            end;
          end;

        wm_LButtonUp:
          begin
           ReleaseCapture;
          end;

        wm_Destroy:
          if Handle.Tag<>nil then 
            TSplitterExtra(Handle.Tag).Free;

      end;
end;

procedure TSplitter.SetPane1(APane1: TWidget);
begin
  FPane1 := APane1;
  FPane1.FParent := self;
  if FCreationState >= wcsCreationInProgress then 
    FPane1.FinishCreation;
end;

procedure TSplitter.SetPane2(APane2: TWidget);
begin
  FPane2 := APane2;
  FPane2.FParent := self;
  if FCreationState >= wcsCreationInProgress then 
    FPane2.FinishCreation;
end;

procedure TSplitter.SetOrientation(AOrientation: TSplitterOrientation);
begin
  FOrientation := AOrientation;
end;

procedure TSplitter.SetResizePolicy(APolicy: TSplitterResizePolicy);
begin
  FResizePolicy := APolicy;
end;

procedure TSplitter.OnFinishCreation;
var
 wclass: WNDCLASS;
begin
  FCreationState := wcsCreationInProgress;

  CreateWin32(self, 'KCLWIDGET', 0, ws_Child);

  Handle.Tag := TSplitterExtra.Create; 
  with TSplitterExtra(Handle.Tag) do
  begin
    DividerSize := 5;
    PropValue := 0.5;
  end;

  Handle.ProcessMessages := @TSplitter_ProcessMessages;

  if Assigned(FPane1) then SetPane1(FPane1);
  if Assigned(FPane2) then SetPane2(FPane2);

  TSplitter_UpdatePanes(self);

  inherited OnFinishCreation;
end;

procedure TSplitter.OnSizeChanged; 
begin
  inherited OnSizeChanged;
  TSplitter_UpdatePanes(self);
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:00  sg
  Initial revision

}
