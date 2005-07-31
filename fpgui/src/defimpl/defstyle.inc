{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Default style implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$IFNDEF Has_DefaultStyle_GetGUIColor}

function TDefaultStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  case Color of
    clScrollBar: Result := rgbaLtGray;
//    clScrollBar: Result := rgbaDkWhite;
    clMenu: Result := rgbaLtGray;
    clWindow: Result := rgbaWhite;
    clMenuText: Result := rgbaBlack;
    clWindowText: Result := rgbaBlack;
    clAppWorkSpace: Result := rgbaGray;
    clHighlight: Result := rgbaDkBlue;
    clHighlightText: Result := rgbaWhite;
    cl3DFace: Result := rgbaLtGray;
    cl3DShadow: Result := rgbaGray;
    clGrayText: Result := rgbaGray;
    clBtnText: Result := rgbaBlack;
    cl3DHighlight: Result := rgbaWhite;
    cl3DDkShadow: Result := rgbaBlack;
    cl3DLight: Result := rgbaDkWhite;
    clInfoText: Result := rgbaBlack;
    clInfoBk: Result := rgbaLtYellow;
    else Result := rgbaWhite;
  end;
end;

{$ENDIF}


{$IFNDEF Has_DefaultStyle_DrawFocusRect}

procedure TDefaultStyle.DrawFocusRect(Canvas: TGfxCanvas; const ARect: TRect);
begin
  // !!!: Disabled for Linux as long a a certain nVidia X-Server has bugs with this...
{  Canvas.SetColor(rgbaBlack);
  Canvas.SetLineStyle(lsDot);
  Canvas.DrawRect(ARect);
  Canvas.SetLineStyle(lsSolid);}
  Canvas.SetColor(rgbaGray);
  Canvas.DrawRect(ARect);
end;

{$ENDIF}


{
  $Log$
  Revision 1.1  2001/02/12 08:49:46  sg
  * First version

}
