{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Separator widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomSeparator = class(TWidget)
  private
    procedure SetOrientation(AOrientation: TOrientation);
    procedure SetSpacing(ASpacing: Integer);
  protected
    FOrientation: TOrientation;
    FSpacing: Integer;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
    property Orientation: TOrientation read FOrientation write SetOrientation default Horizontal;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TSeparator = class(TCustomSeparator)
  published
    property Enabled;
    property Orientation;
    property Spacing;
  end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:45  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
