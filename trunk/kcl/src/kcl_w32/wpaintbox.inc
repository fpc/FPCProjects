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


var _curmodifier: TModifiers;

function TPaintbox_ProcessMessages(widget: TWidget; AMessage, WParam, LParam: Longint): Longint; 
var
  paintbox: TCustomPaintbox;
  m: TModifiers;
  aKeyCode: longWord;
  canvas: TW32Canvas;

begin
  result := 0;
  paintbox := TCustomPaintbox(widget);
  with paintbox do

  case AMessage of

    wm_Paint:
      if Assigned(OnPaint) then
      begin
        Canvas := TW32Canvas.Create(widget);
        if BlockDrawingCounter = 0 then OnPaint(widget, Canvas, Canvas.RedrawRect);
        Canvas.Free;
      end;       

    wm_KeyDown, wm_SysKeyDown:
      begin
        m :=  VKeyToModifier(wparam);
        akeycode := VKeyToKeyCode(wparam);

        //check extended key (altgr)
        if ((lparam shr 24) and 1 <> 0)and(modCtrl in _curmodifier)and(akeycode=0) 
          then _curmodifier := [modAltGr]
        else _curmodifier := _curmodifier + m;

//if akeycode = keyBackspace then writeln('[<-](1)');

        if akeycode<>0 then
          if Assigned(OnKey) then 
            OnKey(widget, #0, akeycode, _curmodifier);     
      end;

    wm_KeyUp, wm_SysKeyUp:
    begin
       m := VKeyToModifier(wparam);
        if ((lparam shr 24) and 1 <> 0)and(modAltGr in _curmodifier) then m := [modAltGr];
        _curmodifier := _curmodifier - m;
    end;

    wm_Char:
      if Assigned(OnKey) then 
      begin
        if modCtrl in _curmodifier then wparam := wparam + 64; //why that?

        if wparam <> keyBackspace then 
          OnKey(widget, char(wparam), wparam, _curmodifier); 
      end;

    wm_LButtonDown, wm_MButtonDown, wm_RButtonDown:
      begin
        case AMessage of
          wm_LButtonDown: _curmodifier := _curmodifier + [modLButton];
          wm_MButtonDown: _curmodifier := _curmodifier + [modMButton]
          else _curmodifier := _curmodifier + [modRButton];
        end;
        if Assigned(FOnMouseButtonDown) then 
          FOnMouseButtonDown(widget, loWord(lparam), hiWord(lparam), _curmodifier);
      end;

    wm_LButtonUp, wm_MButtonUp, wm_RButtonUp:
      begin
        if Assigned(FOnMouseButtonUp) then 
          FOnMouseButtonUp(widget, loWord(lparam), hiWord(lparam), _curmodifier);
        case AMessage of
          wm_LButtonUp: _curmodifier := _curmodifier - [modLButton];
          wm_MButtonUp: _curmodifier := _curmodifier - [modMButton]
          else _curmodifier := _curmodifier - [modRButton];
        end;
      end;

    wm_MouseMove:
      begin
        if Assigned(FOnMouseMove) then
          FOnMouseMove(widget, loWord(lparam), hiWord(lparam), _curmodifier);
      end;

  end;   
end;

procedure TPaintBox.OnFinishCreation;
begin
  FCreationState := wcsCreationInProgress;
  CreateWin32(self, 'KCLWIDGET', 0, ws_Child);
  Handle.ProcessMessages := @TPaintbox_ProcessMessages;
  inherited OnFinishCreation;
end;

procedure TPaintBox.OnSetSize;
begin   
  inherited OnSetSize;
end;


{
  $Log$
  Revision 1.1  1999/12/30 21:34:07  sg
  Initial revision

}
