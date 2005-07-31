{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   TImageList
// ===================================================================

type

  TImageList = class(TKCLComponent)
  protected
    FCount: Integer;
    FWidth, FHeight: Integer;
    priv: TImageList_private;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(AImage, AMask: TBitmap);
    procedure AddMasked(AImage: TBitmap; AMaskColor: TColor);
    property Count: Integer read FCount;
    property Width: Integer read FWidth write FWidth default 16;
    property Height: Integer read FHeight write FHeight default 15;
  end;


{
  $Log$
  Revision 1.1  2000/01/24 00:02:21  sg
  * First version

}
