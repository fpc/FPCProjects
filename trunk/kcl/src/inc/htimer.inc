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
//   Definition of TTimer
// ===================================================================

type

  TTimer = class(TKCLComponent)
  protected
    Handle: TTimerHandle;
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;

    procedure Update;
    procedure SetEnabled(AEnabled: Boolean); dynamic;
    procedure SetInterval(AInterval: Cardinal); dynamic;
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;


{
  $Log$
  Revision 1.2  2000/01/24 00:08:49  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:17  sg
  Initial import

}
