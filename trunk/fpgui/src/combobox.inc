{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Combobox widget implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TCustomComboBox
// ===================================================================

// -------------------------------------------------------------------
//   TArrowButton
// -------------------------------------------------------------------

type
  TArrowButton = class(TGenericButton)
  protected
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
  end;

procedure TArrowButton.Paint(Canvas: TGfxCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawComboBoxArrow(Canvas, Rect(0, 0, Width, Height),
    (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;

procedure TArrowButton.CalcSizes;
begin
  FMinSize := Style.GetComboBoxArrowSize;
end;


// -------------------------------------------------------------------
//   TCustomComboBox
// -------------------------------------------------------------------

constructor TCustomComboBox.Create(AOwner: TComponent);
var
  l: TLabel;
begin
  inherited Create(AOwner);
  FCanExpandWidth := True;
  WidgetStyle := WidgetStyle + [wsCaptureMouse, wsClickable, wsOpaque];

  l := TLabel.Create(Self);
  l.CanExpandWidth := True;
  l.Text := 'Combobox';
  l.Parent := Self;

  FButton := TArrowButton.Create(Self);
  FButton.Embedded := True;
  FButton.CanExpandWidth := False;
  FButton.OnClick := @ButtonClick;
  FButton.Parent := Self;
end;

procedure TCustomComboBox.Paint(Canvas: TGfxCanvas);
begin
  Style.DrawEditBox(Canvas, Rect(0, 0, Width, Height));
end;

procedure TCustomComboBox.CalcSizes;
begin
  with Style.GetEditBoxBorders do
    FMinSize := Size(FButton.MinSize.cx,
      Max(FindForm.Wnd.Canvas.FontCellHeight, FButton.MinSize.cy)) +
      TopLeft + BottomRight;
end;

procedure TCustomComboBox.Resized;
begin
  with Style.GetEditBoxBorders do
    FButton.SetBounds(Point(Width - Right - FButton.MinSize.cx, Top),
      FButton.MinSize);
end;

function TCustomComboBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(FButton) or
    inherited DistributeEvent(Event);
end;

procedure TCustomComboBox.ButtonClick(Sender: TObject);
var
  l: TCustomListBox;
  i: Integer;
begin
  FDropDown := TCustomForm.Create(Self);
  FDropDown.WindowType := wtBorderlessPopup;
  FDropDown.SetEmbeddedParent(FindForm);
  FDropDown.SetPosition(ClientToScreen(Point(0, Height)));
  FDropDown.OnDeactivate := @DropDownDeactivate;
  FDropDown.OnDestroy := @DropDownDestroy;

  l := TCustomListBox.Create(FDropDown);
  for i := 1 to 20 do
    l.Items.Add(Format('Item #%d...', [i]));
  l.Parent := FDropDown;
  l.HotTrack := True;
  l.OnClick := @DropDownDeactivate;

  FDropDown.Show;
end;

procedure TCustomComboBox.DropDownDeactivate(Sender: TObject);
begin
  FDropDown.Close;
end;

procedure TCustomComboBox.DropDownDestroy(Sender: TObject);
begin
  FDropDown := nil;
end;


{
  $Log$
  Revision 1.5  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.4  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.3  2001/01/18 15:36:48  sg
  * The drop-down window is now parented

  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
