{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Application class implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TApplication
// ===================================================================

constructor TApplication.Create;
begin
  inherited Create(nil);
  FDisplay := TDefDisplay.Create;
  FForms := TList.Create;
end;

destructor TApplication.Destroy;
begin
  FForms.Free;
  // !!!: Only auto-created styles should be destroyed, not user-provided
  if Assigned(FDefaultStyle) then
    FDefaultStyle.Free;
  if Assigned(FDisplay) then
    FDisplay.Free;
  inherited Destroy;
end;

procedure TApplication.SetupXMLStreaming(const AXMLFilename: String);
begin
  ReadXMLFile(XMLDoc, AXMLFilename);
end;

procedure TApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
type
  PForm = ^TCustomForm;
var
  form: PForm;
  Filename: String;
  TextStream, BinStream: TStream;
begin
  form := @Reference;
  form^ := TCustomForm(InstanceClass.Create(Self));

  Filename := LowerCase(Copy(InstanceClass.ClassName, 2, 255)) + '.frm';
  WriteLn('Default streaming: ', Filename);

  TextStream := TFileStream.Create(Filename, fmOpenRead);
  BinStream := TMemoryStream.Create;
  ObjectTextToBinary(TextStream, BinStream);
  TextStream.Free;

  BinStream.Position := 0;
  BinStream.ReadComponent(Form^);
  BinStream.Free;

  Form^.Show;
end;

procedure TApplication.AddForm(AForm: TCustomForm);
begin
  FForms.Add(AForm);
  AForm.Show;
end;

procedure TApplication.Run;
begin
  FDisplay.Run;
end;

function TApplication.GetDefaultStyle: TStyle;
begin
  if not Assigned(FDefaultStyle) then
    FDefaultStyle := TDefaultStyle.Create(Display);
  Result := FDefaultStyle;
end;

procedure TApplication.SetTitle(const ATitle: String);
begin
  if ATitle <> FTitle then
  begin
    FTitle := ATitle;
    // !!!: Change title of all forms with Text='' to FTitle
  end;
end;

{!!!: Remove this ASAP
function TApplication.XMLReadFormProc(Instance: TComponent): Boolean;
var
  Node: TDOMNode;
  Reader: TXMLStreamingReader;
begin
  Result := False;
  if not Assigned(XMLDoc) then
    exit;
  Node := XMLDoc.DocumentElement.FirstChild;

  // Search the correct serialisation node
  while True do
  begin
    if not Assigned(Node) then
      exit;
    if (Node.NodeType = ELEMENT_NODE) and (Node.NodeName = 'component') and
      (UpperCase(TDOMElement(Node).GetAttribute('class')) = Instance.ClassName) then
      break;
    Node := Node.NextSibling;
  end;
  Reader := TXMLStreamingReader.Create(Node as TDOMElement);
  try
    Reader.ReadRootComponent(Instance);
  finally
    Reader.Free;
  end;
  Result := True;
end;}



{
  $Log$
  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
