{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
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
    procedure EvRecalcLayout; override;
  end;

procedure TArrowButton.Paint(Canvas: TGfxCanvas);
begin
  inherited Paint(Canvas);
  Style.DrawComboBoxArrow(Canvas, Rect(0, 0, Width, Height),
    (wsClicked in WidgetState) and (wsMouseInside in WidgetState),
    wsEnabled in WidgetState);
end;

procedure TArrowButton.EvRecalcLayout;
begin
  with Style.GetComboBoxArrowSize do
  begin
    MinW := cx;
    MinH := cy;
  end;
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

function TCustomComboBox.ProcessEvent(Event: TEventObj): Boolean;
begin
  if Event is TResizeEventObj then
    Result := EvResize(TResizeEventObj(Event))
  else
    Result := inherited ProcessEvent(Event);
end;

function TCustomComboBox.DistributeEvent(Event: TEventObj): Boolean;
begin
  Result := Event.SendToChild(FButton) or
    inherited DistributeEvent(Event);
end;

function TCustomComboBox.EvResize(Event: TResizeEventObj): Boolean;
begin
  with Style.GetEditBoxBorders do
    FButton.SetBounds(Width - Right - FButton.MinW, Top,
      FButton.MinW, FButton.MinH);
  Result := False;
end;

procedure TCustomComboBox.EvRecalcLayout;
begin
  with Style.GetEditBoxBorders do
  begin
    MinW := FButton.MinW + Left + Right;
    MinH := Top + Bottom +
      Max(FindForm.Wnd.Canvas.FontCellHeight, FButton.MinH);
  end;
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
  Revision 1.4  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.3  2001/01/18 15:36:48  sg
  * The drop-down window is now parented

  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
