{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Checkbox widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomCheckbox = class(TWidget)
  private
    procedure SetChecked(AChecked: Boolean);
  protected
    FChecked: Boolean;
    FLabelPos: TPoint;
    procedure Click; override;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
    property Checked: Boolean read FChecked write SetChecked;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCheckbox = class(TCustomCheckbox)
  published
    property Enabled;
    property Checked;
    property Text;
    property OnClick;
  end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
