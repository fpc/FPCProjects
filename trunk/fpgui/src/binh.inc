{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Bin widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TBinWidget = class(TContainerWidget)
  protected
    FChild: TWidget;
    procedure SetChild(AChild: TWidget);
    function GetChildCount: Integer; override;
    function GetChild(Index: Integer): TWidget; override;
    procedure CalcSizes; override;
  public // !!!: temporarily
    property Child: TWidget read FChild write SetChild;
    // really public :)
    function ContainsChild(AChild: TWidget): Boolean; override;
    procedure InsertChild(AChild: TWidget); override;
    procedure RemoveChild(AChild: TWidget); override;
  end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:43  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2000/12/23 23:20:14  sg
  * First public CVS version...

}
