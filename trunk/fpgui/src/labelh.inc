{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Label widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomLabel = class(TWidget)
  private
    procedure EvRecalcLayout; override;
    procedure SetAlignment(AAlignment: TAlignment);
  protected
    FAlignment: TAlignment;
    procedure Paint(Canvas: TGfxCanvas); override;
    property Alignment: TAlignment read FAlignment write SetAlignment;
  end;

  TLabel = class(TCustomLabel)
  published
    property CanExpandWidth;
    property Enabled;
    property Text;
    property Alignment default taLeftJustify;
  end;


{
  $Log$
  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
