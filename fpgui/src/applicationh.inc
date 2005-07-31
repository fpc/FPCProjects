{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Application class declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TApplication = class(TComponent)
  private
    FDisplay: TGfxDisplay;
    FForms: TList;
  protected
    FDefaultStyle: TStyle;
    FTitle: String;
    XMLDoc: TXMLDocument;
    function GetDefaultStyle: TStyle;
    procedure SetTitle(const ATitle: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetupXMLStreaming(const AXMLFilename: String);
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure AddForm(AForm: TCustomForm);
    procedure Run;
    property Display: TGfxDisplay read FDisplay;
    property DefaultStyle: TStyle read GetDefaultStyle;
    property Title: String read FTitle write SetTitle;
  end;


{
  $Log$
  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
