{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Base Classes for Forms
// ===================================================================

  TGenericForm = class(TWidget)
  protected
    FOnClose: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    function  CanClose: Boolean; virtual;
    procedure Show; virtual; abstract;
  published
    property OnClose: TNotifyEvent read FOnClose;
  end;


  TForm = class(TGenericForm)
  protected
    FContent: TWidget;
    procedure SetContent(AContent: TWidget); // ###virtual;
    procedure OnFinishCreation; override;
    procedure OnSetSize; override;
    procedure OnSizeChanged; override;
  public
    procedure Close; virtual;
    procedure Show; override;
  published
    property Content: TWidget read FContent write SetContent stored True;
    property Text;
  end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:19  sg
  Initial revision

}
