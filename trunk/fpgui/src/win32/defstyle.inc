{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Default style implementation for Win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$DEFINE Has_DefaultStyle_GetGUIColor}

function TDefaultStyle.GetGUIColor(Color: TColor): TGfxColor;
begin
  Color := Windows.GetSysColor(Color and $ffff);
  Result.Red := (Color and $ff) * 257;
  Result.Green := ((Color shr 8) and $ff) * 257;
  Result.Blue := ((Color shr 16) and $ff) * 257;
  Result.Alpha := 0;
end;


{$DEFINE Has_DefaultStyle_DrawFocusRect}

procedure TDefaultStyle.DrawFocusRect(Canvas: TGfxCanvas; const ARect: TRect);
var
  Rect: Windows.TRect;
begin
  if Canvas.InheritsFrom(TGDICanvas) then
  begin
    Rect := RectToWinRect(Canvas.Transform(ARect));
    Windows.DrawFocusRect(TGDICanvas(Canvas).Handle, Rect)
  end else
  begin
    Canvas.SetColor(rgbaGray);
    Canvas.DrawRect(ARect);
  end;
end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:45  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2001/02/12 08:49:46  sg
  * First version

}
