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
//   TKCLComponent
// ===================================================================

type

  TEventClass = class of TEventObj;

  // Return False to suppress further processing of this event
  TEventFilterFn = function(Event: TEventObj): Boolean of object;

  PEventFilter = ^TEventFilter;
  TEventFilter = record
    EventClass: TEventClass;
    FilterFn: TEventFilterFn;
  end;

  TKCLComponent = class(TComponent)
  protected
    FilterList: TList;
  public
    destructor Destroy; override;
    procedure AddEventFilter(EventClass: TEventClass; FilterFn: TEventFilterFn);
    function  CallEventFilters(Event: TEventObj): Boolean;
  end;


{
  $Log$
  Revision 1.2  2000/01/24 00:03:45  sg
  * TEventFilterFn is now a function of object, and not a normal function
    (this should have been the case since the beginning, but I forgot it...)
  * Add TEventFilter and PEventFilter

  Revision 1.1.1.1  1999/12/30 21:33:22  sg
  Initial import

}
