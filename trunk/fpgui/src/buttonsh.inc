{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Button widgets declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TGenericButton = class(TWidget)
  protected
    FEmbedded: Boolean;
    procedure Paint(Canvas: TGfxCanvas); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Embedded: Boolean read FEmbedded write FEmbedded default False;
  end;

  TCustomButton = class(TGenericButton)
  protected
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure EvRecalcLayout; override;
  end;

  TButton = class(TCustomButton)
  published
    property CanExpandWidth;
    property Enabled;
    property Text;
    property OnClick;
  end;

{
  TCustomImageButton = class(TGenericButton)
  protected
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure EvRecalcLayout; override;
  end;

  TImageButton = class(TCustomImageButton)
  published
    property Enabled;
    property Image;
    property OnClick;
  end;
}


{
  $Log$
  Revision 1.2  2001/01/11 23:30:11  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
