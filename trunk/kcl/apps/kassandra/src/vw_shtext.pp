{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// View class for generic text editor with syntax highlighting capabilities

{$MODE objfpc}
{$M+,H+}

unit vw_shtext;

interface
uses vw_generic, doc_text, SHEdit, KCLSHEdit;

type

  TSHTextEditClass = class of TSHTextEdit;

  TSHTextView = class(TGenericView)
  protected
    LeftIndent, DefMaxTextWidth: Integer;
    Editor: TSHTextEdit;
    Widget: TKCLSHWidget;
    FDoc: TTextDoc;
    procedure ModifiedChanged(Sender: TObject);
    constructor Create(ADoc: TTextDoc; AEditClass: TSHTextEditClass);
  public
    constructor Create(ADoc: TTextDoc);
    procedure Save; override;
  end;

  TSHPasView = class(TSHTextView)
  public
    constructor Create(ADoc: TTextDoc);
  end;

  TSHXMLView = class(TSHTextView)
  public
    constructor Create(ADoc: TTextDoc);
  end;


implementation

uses KCL, sh_pas, sh_xml;


constructor TSHTextView.Create(ADoc: TTextDoc);
begin
  Self.Create(ADoc, TSHTextEdit);
end;

constructor TSHTextView.Create(ADoc: TTextDoc; AEditClass: TSHTextEditClass);
begin
  inherited Create;
  FDoc := ADoc;
  Widget := TKCLSHWidget.Create(nil);
  Widget.SetupEditor(ADoc, AEditClass);
  Widget.DrawVBar := True;
  Widget.LeftIndent := 24;
  Editor := Widget.Editor;
  FMainSubwindow := Widget;

  // Editor widget setup
  LeftIndent := 24;
  DefMaxTextWidth := 80;
  Editor.OnModifiedChange := @ModifiedChanged;
end;

procedure TSHTextView.Save;
var
  f: Text;
  i: Integer;
begin
  AssignFile(f, FFileName);
  Rewrite(f);
  for i := 0 to Editor.Doc.LineCount - 1 do
    WriteLn(f, Editor.Doc.LineText[i]);
  Close(f);
  inherited Save;
end;

procedure TSHTextView.ModifiedChanged(Sender: TObject);
begin
  IsModified := FDoc.Modified;
end;



constructor TSHPasView.Create(ADoc: TTextDoc);
var
  e: TSHPasEdit;
begin
  inherited Create(ADoc, TSHPasEdit);
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

constructor TSHXMLView.Create(ADoc: TTextDoc);
var
  e: TSHXMLEdit;
begin
  inherited Create(ADoc, TSHXMLEdit);
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
  Revision 1.1  1999/12/30 21:22:31  sg
  Initial revision

}
