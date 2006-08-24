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


unit lNetComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLNet, lNet, lEvents, lFTP, lSMTP;

type
  TLSocket = lNet.TLSocket;
  TLConnection = lNet.TLConnection;
  TLNetComponent = TLConnection;
  TLTcp = lNet.TLTcp;
  TLUdp = lNet.TLUdp;
  TLFTPClient = lFTP.TLFTPClient;
  TLSMTPClient = lSMTP.TLSMTPClient;

  TLErrorProc = procedure(const msg: string; aSocket: TLSocket) of object;
  TLProc = procedure(aSocket: TLSocket) of object;

  TLFTPClientProgressCallback = procedure (Sender: TLFTPClient; const Bytes: Integer) of object;
  TLFTPClientCallback = procedure (Sender: TLFTPClient) of object;

  TLSMTPClientCallback = procedure (Sender: TLSMTPClient) of object;

  { TLTCPComponent }

  TLTCPComponent = class(TLTcp)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property OnReceive;
    property OnError;
    property OnDisconnect;
    property OnConnect;
    property OnAccept;
  end;
  
  { TLUDPComponent }

  TLUDPComponent = class(TLUdp)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property OnReceive;
    property OnError;
  end;

  { TLFTPClientComponent }

  TLFTPClientComponent = class(TLFTPClient)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property OnConnect;
    property OnSent;
    property OnReceive;
    property OnControl;
    property OnError;
    property PipeLine;
    property StartPort;
    property UsePORT;
  end;
  
  { TLSMTPCientComponent }

  TLSMTPClientComponent = class(TLSMTPClient)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property OnConnect;
    property OnReceive;
    property OnDisconnect;
    property OnError;
    property PipeLine;
  end;

implementation

var
  LCLEventer: TLEventer;

{ TLTCPComponent }

constructor TLTCPComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SocketClass:=TLCLSocket;
  Eventer:=LCLEventer;
end;

{ TLUDPComponent }

constructor TLUDPComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SocketClass:=TLCLSocket;
  Eventer:=LCLEventer;
end;

{ TLFTPClientComponent }

constructor TLFTPClientComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SocketClass:=TLCLSocket;
  ControlConnection.Connection.Eventer:=LCLEventer;
  DataConnection.Eventer:=LCLEventer;
end;

{ TLSMTPCientComponent }

constructor TLSMTPClientComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  SocketClass:=TLCLSocket;
  Eventer:=LCLEventer;
end;

initialization
  LCLEventer:=TLEventer.Create;
  
finalization
  LCLEventer.Free; // it IS required because refcount is +1

end.
