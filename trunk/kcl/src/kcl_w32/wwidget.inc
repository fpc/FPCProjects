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
//   Widget implementation for Win32
// ===================================================================


function VKeyToModifier(vkey: integer): TModifiers;
begin
  result := [];
  case vkey of
    vk_Shift        : result := [modShift];
    vk_Control      : result := [modCtrl];
    vk_Menu         : result := [modAlt];
    vk_Capital      : result := [modCaps];
    vk_NumLock      : result := [modNum];
    vk_Scroll       : result := [modScroll];
    vk_LButton      : result := [modLButton];
    vk_RButton      : result := [modRButton];
    vk_MButton      : result := [modMButton];
  end;
end;

function VKeyToKeyCode(vkey: integer): longword;
begin
  result := 0;
  case vkey of
    vk_left   : result := keyLeft;
    vk_right  : result := keyRight;
    vk_up     : result := keyUp;
    vk_down   : result := keyDown;
    vk_Escape : result := keyEscape;
    vk_Prior  : result := keyPageUp;
    vk_Next   : result := keyPageDown;
    vk_Home   : result := keyHome;
    vk_End    : result := keyEnd;
    vk_Insert : result := keyInsert;
    vk_Tab    : result := keyTab;
    vk_Return : result := keyReturn;
    vk_Back   : result := keyBackspace;
    vk_Delete : result := keyDelete;
    vk_F1     : result := keyF1;
    vk_F2     : result := keyF2;
    vk_F3     : result := keyF3;
    vk_F4     : result := keyF4;
    vk_F5     : result := keyF5;
    vk_F6     : result := keyF6;
    vk_F7     : result := keyF7;
    vk_F8     : result := keyF8;
    vk_F9     : result := keyF9;
    vk_F10    : result := keyF10;
    vk_F11    : result := keyF11;
    vk_F12    : result := keyF12;
    vk_F13    : result := keyF13;
    vk_F14    : result := keyF14;
    vk_F15    : result := keyF15;
    vk_F16    : result := keyF16;
    vk_F17    : result := keyF17;
    vk_F18    : result := keyF18;
    vk_F19    : result := keyF19;
    vk_F20    : result := keyF20;
    vk_F21    : result := keyF21;
    vk_F22    : result := keyF22;
    vk_F23    : result := keyF23;
    vk_F24    : result := keyF24;
  end;
end;

procedure ProcessWidgetMessages(widget: TWidget; AMessage, WParam, LParam: Longint);
var
  minmax: ^MinMaxInfo;
  n: ^NMHDR;
  from: hWnd;
  r: Windows.Rect;
  child: TWidget;
  pproc: ProcessMessagesProc;

begin
  with widget do

  case AMessage of

    wm_Command, wm_Notify:  //send them back to the sender, so they can send signals like 'clicked'
      begin
        if AMessage = wm_Command then from := lParam
        else
        begin
          n := pointer(lparam);
          from := n^.hwndFrom;
        end;
        if from<>0 then
        begin
          child := TWidget(GetProp(from, PChar('KCL')));
          if child<>nil then
            if child.handle.ProcessMessages<>nil then 
            begin
                pproc := ProcessMessagesProc(child.handle.ProcessMessages);
                pproc(child, AMessage, WParam, LParam);
            end;
        end;
      end;

    wm_MouseMove: SetCursor(_syscurDefault);

    wm_GetMinMaxInfo:
      begin
        minmax := pointer(lparam);
        with minmax^ do
        begin
          ptMinTrackSize.x := FMinWidth;
          ptMinTrackSize.y := FMinHeight;
          if FMaxWidth>-1 then ptMaxTrackSize.x := FMaxWidth;
          if FMaxHeight>-1 then ptMaxTrackSize.y := FMaxHeight;
        end;    
      end;     

    wm_Size:
      begin
        GetWindowRect(Handle.Window, @r);
        FWidth := r.right - r.left;
        FHeight := r.bottom - r.top;
        OnSizeChanged;
      end;                 

    wm_SetFocus:
      if IsSignalUsed(@FOnFocusIn) then FOnFocusIn(widget);

    wm_KillFocus:
      if IsSignalUsed(@FOnFocusOut) then FOnFocusOut(widget);   

  end;

end;


function WidgetProc(Window: HWnd; AMessage, WParam, LParam: Longint): Longint; stdcall; export;
var
   widget: TWidget;
   proc: ProcessMessagesProc;
begin   
  widget := TWidget(GetProp(Window, PChar('KCL')));

  if widget<>nil then
    with widget do
      if handle.stdproc<>nil then
      begin
        if handle.ProcessMessages<>nil then 
        begin
            proc := ProcessMessagesProc(handle.ProcessMessages);
            result := proc(widget, AMessage, WParam, LParam);
            if result<>0 then exit;
        end;

        ProcessWidgetMessages(widget, AMessage, WParam, LParam);

        result := CallWindowProc(WndProc(handle.stdproc), Window, AMessage, WParam, LParam);
        exit;
      end
      else
      begin
        writeln('!!ALARM!! no stdproc!');
        halt(1);
      end;

  writeln('!!ALARM!! no widget!');
  halt(1);
end;


procedure FillStdWndClass(var wnd: WndClass);
begin
  with wnd do
  begin
    Style := cs_DBLCLKS or cs_hRedraw or cs_vRedraw;
    lpfnWndProc := WndProc(@DefWindowProc);
    cbClsExtra := 0;
    cbWndExtra := 4;
    hInstance := system.MainInstance;
    hIcon := LoadIcon(0, idi_Application);
    hCursor := 0;
    hbrBackground := COLOR_APPWORKSPACE;
    lpszMenuName := nil;
    lpszClassName := PChar('KCLWIDGET');
  end;
end;

procedure CreateWin32(widget: TWidget; winclassname: string; exstyle, winstyle: longint);
var 
  hParent: hWnd;
begin
  if widget<>nil then
  with widget do
  begin
    //get parent:
    hParent := 0;
    if parent<>nil then 
        if parent is TWidget then hParent := TWidget(parent).Handle.Window;

    //create window:
    if winclassname='STATUSBAR' then
        Handle.Window := CreateStatusWindow(winstyle, PChar(FText), hParent, 101)
    else
        Handle.Window := CreateWindowEx(exstyle, PChar(winclassname), PChar(FText), winstyle,
                        CW_USEDEFAULT,0,width,height, hParent, 0, MainInstance, nil);

    //save us in the props list:
    SetProp(Handle.Window, PChar('KCL'), longint(widget));

    //all widgets start with the "good-looking" m$sansserif font:
    PostMessage(Handle.Window, wm_SetFont, _systemFont.FHandle.Font, 1);

    //all windows have the same proc:
    if handle.window>0 then
        handle.stdproc :=pointer(SetWindowLong(Handle.Window, GWL_WNDPROC, longint(@WidgetProc)));

  end;
end;


// -------------------------------------------------------------------
//   TWidget signal/slot and property handling
// -------------------------------------------------------------------

procedure TWidget.SetText(const AText: String);
begin
  FText := AText;
  SetWindowText(Handle.Window, PChar(AText));
end;
       
procedure TWidget.SetVisible(AVisible: boolean);
begin
  if AVisible<>FVisible then
  begin
    FVisible := AVisible;
    if FCreationState >= wcsCreationInProgress then
      if FVisible then ShowWindow(Handle.Window, sw_Show)
      else ShowWindow(Handle.Window, sw_Hide);
  end;
end;

procedure TWidget.SetHint(const AHint: String);
begin
  FHint := AHint;
end;


// -------------------------------------------------------------------
//   TWidget protected methods
// -------------------------------------------------------------------

procedure TWidget.FinishCreation;
begin
  if FCreationState = wcsCreationDone then exit;
  if (FParent <> nil) and FParent.InheritsFrom(TWidget) and
    (TWidget(FParent).FCreationState < wcsCreationInProgress) then exit;

  OnFinishCreation;

  if not (sizeAuto in FSizeFlags) then
    OnMinMaxSizeChanged;
end;

procedure TWidget.OnFinishCreation;
begin
  OnSetSize;

  ShowWindow(Handle.Window, sw_Show);   // Show widget

  if FCreationState <> wcsCreationInProgress then
    WriteLn('TWidget.OnFinishCreation: CreationState is not ' +
      'wcsCreationInProgress! Class=', self.ClassName);
          
  FCreationState := wcsCreationDone;
end;

procedure TWidget.OnRecalcLayout; begin end;

procedure TWidget.OnSetSize;
begin
  SetWindowPos(Handle.Window, -1, 0, 0, FWidth, FHeight, SWP_NOZORDER or SWP_NOMOVE);
end;

procedure TWidget.OnSizeChanged;
begin
  OnMinMaxSizeChanged;       
end;

procedure TWidget.OnMinMaxSizeChanged;
begin
end;


// -------------------------------------------------------------------
//   TWidget public methods
// -------------------------------------------------------------------


constructor TWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSizeFlags := [sizeAuto];
  FMinWidth := 0;
  FMinHeight := 0;
  FMaxWidth := InfiniteSize;
  FMaxHeight := InfiniteSize;
  //gut?:
  FWidth:=50;
  FHeight:=50;
  Handle.ProcessMessages := nil;
end;

destructor TWidget.Destroy;
begin
  inherited Destroy;
  DestroyWindow(Handle.Window);
end;       

procedure TWidget.SetFocus;
begin
  if Handle.Window <> -1 then Windows.SetFocus(Handle.Window);
end;

function TWidget.HasFocus: Boolean;
begin
  result := GetFocus = Handle.Window;
end;

procedure TWidget.SetSize(AWidth, AHeight: Longint);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then exit;
  FWidth := AWidth;
  FHeight := AHeight;
  if CreationState >= wcsCreationInProgress then OnSetSize;
end;

procedure TWidget.SetDefaultSize(AWidth, AHeight: Longint);
begin
  FSizeFlags := FSizeFlags - [sizeAuto];
  SetSize(AWidth, AHeight);
end;

procedure TWidget.SetFixedSize(AWidth, AHeight: Longint);
begin
  FSizeFlags := [sizeFixed];
  FMinWidth := AWidth;
  FMinHeight := AHeight;
  FMaxWidth := AWidth;
  FMaxHeight := AHeight;
  WriteLn('TWidget.SetFixedSize for ', ClassName, ': ', AWidth, 'x', AHeight);
  SetSize(AWidth, AHeight);
end;

procedure TWidget.Redraw;
var 
  clientrect: _Rect;
begin
  GetClientRect(Handle.Window, @clientrect);
  InvalidateRect(Handle.Window, clientrect, false);
end;

procedure TWidget.Redraw(const Rect: TRect);
var 
  r: _Rect;
begin
  r := TRectToRect(rect);
  InvalidateRect(Handle.Window, r, true);   
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:03  sg
  Initial revision

}
