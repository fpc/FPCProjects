{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Edit widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TCustomEdit = class(TWidget)
  private
    FOnChange: TNotifyEvent;
    procedure SetPasswordChar(APasswordChar: Char);
    procedure SetCursorPos(ACursorPos: Integer);
  protected
    FPasswordChar: Char;
    FCursorPos: Integer;
    procedure Paint(Canvas: TGfxCanvas); override;
    procedure CalcSizes; override;
    procedure EvKeyPressed(Key: Word; Shift: TShiftState); override;
    procedure EvKeyChar(KeyChar: Char); override;
    procedure EvTextChanged; override;
    property CanExpandWidth default True;
    property Cursor default crIBeam;
    property PasswordChar: Char read FPasswordChar write SetPasswordChar default #0;
    property CursorPos: Integer read FCursorPos write SetCursorPos;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEdit = class(TCustomEdit)
  published
    property CanExpandWidth;
    property Enabled;
    property PasswordChar;
    property Text;
    property OnChange;
  end;


{
  $Log$
  Revision 1.4  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.3  2001/02/09 20:50:20  sg
  * The usual bunch of improvements and bug fixes...

  Revision 1.2  2001/01/11 23:30:12  sg
  *** empty log message ***

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
