{
    $Id$

    fpGUI  -  Free Pascal Graphical User Interface
    Copyright (C) 2000 - 2001 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    Container widget declarations

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


  TContainerWidget = class(TWidget)
  protected
    function DistributeEvent(Event: TEventObj): Boolean; override;
    function GetChildCount: Integer; virtual; abstract;
    function GetChild(Index: Integer): TWidget; virtual; abstract;
    property ChildCount: Integer read GetChildCount;
    property Children[Index: Integer]: TWidget read GetChild;
  public
    function GetChildAt(APoint: TPoint): TWidget;
    function ContainsChild(AChild: TWidget): Boolean; dynamic; abstract;
    procedure InsertChild(AChild: TWidget); dynamic; abstract;
    procedure RemoveChild(AChild: TWidget); dynamic; abstract;
  end;


{
  $Log$
  Revision 1.2  2001/02/14 23:14:44  sg
  * Adaptions to fpGFX changes; added much more usage of TSize and TPoint
    here as well
  * Heavy improvements of the layouting algorithm

  Revision 1.1  2000/12/23 23:20:15  sg
  * First public CVS version...

}
