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
  LCLNet, lNet, lEvents, lTelnet, lFTP, lSMTP, lHTTP;
  
type
  TLSocket = lNet.TLSocket;
  TLConnection = lNet.TLConnection;
  TLNetComponent = TLConnection;
  
  TLTcp = lNet.TLTcp;
  TLUdp = lNet.TLUdp;
  
  TLFTP = lFTP.TLFTP;
  TLFTPClient = lFTP.TLFTPClient;
  
  TLSMTP = lSMTP.TLSMTP;
  TLSMTPClient = lSMTP.TLSMTPClient;
  
  TLHTTPClientSocket = lHTTP.TLHTTPClientSocket;
  TLHTTPClient = lHTTP.TLHTTPClient;
  TLHTTPMethod = lHTTP.TLHTTPMethod;

  TLSocketErrorEvent = procedure(const msg: string; aSocket: TLSocket) of object;
  TLSocketEvent = procedure(aSocket: TLSocket) of object;
  
  TLTelnetClientEvent = procedure (Sender: TLTelnetClient) of object;
  TLTelnetClientErrorEvent = procedure (const msg: string;
                                        Sender: TLTelnetClient) of object;

  TLFTPClientProgressCallback = procedure (Sender: TLFTPClient;
                                           const Bytes: Integer) of object;
  TLFTPClientCallback = procedure (Sender: TLFTPClient) of object;
  TLFTPClientStatusCallback = procedure (Sender: TLFTPClient;
                                   const aStatus: TLFTPStatus) of object;
  TLFTPClientErrorCallback = procedure (const msg: string;
                                   Sender: TLFTPClient) of object;

  TLSMTPClientCallback = procedure (Sender: TLSMTPClient) of object;
  TLSMTPClientErrorCallback = procedure (const msg: string;
                                    Sender: TLSMTPClient) of object;

  TLInputEvent = function(ASocket: TLHTTPClientSocket; ABuffer: pchar; ASize: dword): dword of object;
  TLCanWriteEvent = procedure(ASocket: TLHTTPClientSocket; var OutputEof: TWriteBlockStatus) of object;
  TLHTTPClientProc = procedure(ASocket: TLHTTPClientSocket) of object;
  
  TLSMTPStatus = lSMTP.TLSMTPStatus;
  TLSMTPStatusSet = lSMTP.TLSMTPStatusSet;

  { TLTCPComponent }

  TLTCPComponent = class(TLTcp)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Host;
    property Port;
    property OnReceive;
    property OnError;
    property OnDisconnect;
    property OnConnect;
    property OnAccept;
    property Timeout;
  end;
  
  { TLUDPComponent }

  TLUDPComponent = class(TLUdp)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Host;
    property Port;
    property OnReceive;
    property OnError;
    property Timeout;
  end;
  
  { TLTelnetClientComponent }
  
  TLTelnetClientComponent = class(TLTelnetClient)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Host;
    property Port;
    property OnConnect;
    property OnDisconnect;
    property OnReceive;
    property OnError;
    property Timeout;
    property LocalEcho;
  end;

  { TLFTPClientComponent }

  TLFTPClientComponent = class(TLFTPClient)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Host;
    property Port;
    property OnConnect;
    property OnSent;
    property OnReceive;
    property OnControl;
    property OnError;
    property OnSuccess;
    property OnFailure;
    property StatusSet;
    property PipeLine;
    property StartPort;
    property TransferMethod;
    property Timeout;
  end;
  
  { TLSMTPCientComponent }

  TLSMTPClientComponent = class(TLSMTPClient)
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Host;
    property Port;
    property OnConnect;
    property OnReceive;
    property OnDisconnect;
    property OnError;
    property OnSuccess;
    property OnFailure;
    property StatusSet;
    property PipeLine;
    property Timeout;
  end;
  
  { TLHTTPClientComponent }

  TLHTTPClientComponent = class(TLHTTPClient)
   public
    constructor Create(aOwner: TComponent); override;
   published
    property Host;
    property Method;
    property Port;
    property URI;
    property OnCanWrite;
    property OnDoneInput;
    property OnInput;
    property OnProcessHeaders;
    property OnDisconnect;
    property OnError;
    property Timeout;
  end;

implementation

var
  LCLEventer: TLCLEventer;

{ TLTCPComponent }

constructor TLTCPComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Eventer := LCLEventer;
end;

{ TLUDPComponent }

constructor TLUDPComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Eventer :=LCLEventer;
end;

{ TLTelnetClientComponent }

constructor TLTelnetClientComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Connection.Eventer := LCLEventer;
end;

{ TLFTPClientComponent }

constructor TLFTPClientComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  ControlConnection.Connection.Eventer := LCLEventer;
  DataConnection.Eventer := LCLEventer;
end;

{ TLSMTPCientComponent }

constructor TLSMTPClientComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Eventer := LCLEventer;
end;

{ TLHTTPClientComponent }

constructor TLHTTPClientComponent.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Eventer := LCLEventer;
end;

initialization
  LCLEventer:=TLCLEventer.Create;
  
finalization
  LCLEventer.Free; // it IS required because refcount is +1

end.
