{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999 - 2000  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// View class for generic text editor with syntax highlighting capabilities

{$MODE objfpc}
{$M+,H+}

unit Vw_SHText;

interface
uses ViewMan, doc_text, SHEdit, KCLSHEdit;

type

  TSHTextEditClass = class of TSHTextEdit;

  TSHTextView = class(TGenericView)
  protected
    Editor: TSHTextEdit;
    Widget: TKCLSHWidget;
    FDoc: TTextDoc;
    procedure ModifiedChanged(Sender: TObject);
    constructor Create(AManager: TViewManager; ADoc: TTextDoc;
      AEditClass: TSHTextEditClass);
  public
    constructor Create(AManager: TViewManager; ADoc: TTextDoc);
    procedure Save; override;
  end;

  TSHPasView = class(TSHTextView)
  public
    constructor Create(AManager: TViewManager; ADoc: TTextDoc);
  end;

  TSHXMLView = class(TSHTextView)
  public
    constructor Create(AManager: TViewManager; ADoc: TTextDoc);
  end;


implementation

uses KCL, sh_pas, sh_xml;


constructor TSHTextView.Create(AManager: TViewManager; ADoc: TTextDoc);
begin
  Self.Create(AManager, ADoc, TSHTextEdit);
end;

constructor TSHTextView.Create(AManager: TViewManager; ADoc: TTextDoc;
  AEditClass: TSHTextEditClass);
begin
  inherited Create(AManager);
  FDoc := ADoc;
  Widget := TKCLSHWidget.Create(manager);
  FMainSubwindow := Widget;

  // Editor widget setup
  Widget.SetupEditor(ADoc, AEditClass);
  Editor := Widget.Editor;
  Widget.DrawVBar := True;
  Widget.LeftIndent := 24;
  Widget.DefMaxTextWidth := 80;
  Editor.OnModifiedChange := @ModifiedChanged;
end;

procedure TSHTextView.Save;
begin
  FDoc.SaveToFile(FFileName);
  inherited Save;
end;

procedure TSHTextView.ModifiedChanged(Sender: TObject);
begin
  IsModified := FDoc.Modified;
end;



constructor TSHPasView.Create(AManager: TViewManager; ADoc: TTextDoc);
var
  e: TSHPasEdit;
begin
  inherited Create(AManager, ADoc, TSHPasEdit);
  e := Editor as TSHPasEdit;
  e.shSymbol     := Widget.AddSHStyle('Symbol',        colBrown,       colDefault, fsNormal);
  e.shKeyword    := Widget.AddSHStyle('Keyword',       colBlack,       colDefault, fsBold);
  e.shComment    := Widget.AddSHStyle('Comment',       colDarkCyan,    colDefault, fsItalics);
  e.shDirective  := Widget.AddSHStyle('Directive',     colDarkYellow,  colDefault, fsItalics);
  e.shNumbers    := Widget.AddSHStyle('Numbers',       colDarkMagenta, colDefault, fsNormal);
  e.shCharacters := Widget.AddSHStyle('Characters',    colDarkBlue,    colDefault, fsNormal);
  e.shStrings    := Widget.AddSHStyle('Strings',       colBlue,        colDefault, fsNormal);
  e.shAssembler  := Widget.AddSHStyle('Assembler',     colDarkGreen,   colDefault, fsNormal);
end;

constructor TSHXMLView.Create(AManager: TViewManager; ADoc: TTextDoc);
var
  e: TSHXMLEdit;
begin
  inherited Create(AManager, ADoc, TSHXMLEdit);
  e := Editor as TSHXMLEdit;

  e.shTag        := Widget.AddSHStyle('Tag',           colBlack,       colDefault, fsBold);
  e.shTagName    := Widget.AddSHStyle('Tag Name',      colBlack,       colDefault, fsBold);
  e.shDefTagName := Widget.AddSHStyle('Definition Tag Name', colDarkGreen, colDefault, fsBold);
  e.shArgName    := Widget.AddSHStyle('Argument Name', colBrown,       colDefault, fsNormal);
  e.shString     := Widget.AddSHStyle('String',        colBlue,        colDefault, fsNormal);
  e.shReference  := Widget.AddSHStyle('Reference',     colDarkMagenta, colDefault, fsNormal);
  e.shInvalid    := Widget.AddSHStyle('Invalid',       colRed,         colDefault, fsNormal);
  e.shComment    := Widget.AddSHStyle('Comment',       colDarkCyan,    colDefault, fsItalics);
  e.shCDATA      := Widget.AddSHStyle('CDATA',         colDarkGreen,   colDefault, fsNormal);
end;


end.


{
  $Log$
  Revision 1.3  2000/02/10 18:26:19  sg
  * Files are now saved with TTextDocument.SaveToFile

  Revision 1.2  2000/01/05 19:27:26  sg
  * Support for view manager
  * Widgets now set their owner correctly

  Revision 1.1.1.1  1999/12/30 21:22:31  sg
  Initial import

}
