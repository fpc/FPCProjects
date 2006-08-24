{ lNetComponents v0.4

  CopyRight (C) 2004-2006 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit LCLNet;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase, LCLIntf, lNet, lEvents;
  
type

  { TLCLSocket }

  TLCLSocket = class(TLSocket)
   protected
    FEventHandle: PEventHandler;
    FWinObject: THandle;   // for win32
    FFlags: DWord;
    FReferenced: Boolean;
    function CanSend: Boolean; override;
    function CanReceive: Boolean; override;
    procedure SetNonBlock; override;
    procedure RegisterHandler(const aFlags: DWord);

    procedure HandleEvents(aData: PtrInt; aFlags: DWord);
   public
    constructor Create; override;
    procedure Disconnect; override;
    procedure Free; override;
  end;
  
implementation

{$ifdef win32}
  {$i lclsocketwin32.inc}
{$else}
  {$i lclsocketgtk.inc}
{$endif}

procedure TLCLSocket.Free;
begin
  FDispose:=True;
  if not FReferenced then
    inherited Free;
end;

end.

