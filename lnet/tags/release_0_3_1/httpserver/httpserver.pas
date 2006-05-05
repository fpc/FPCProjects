unit httpserver;

{$mode objfpc}{$h+}

interface

uses
  classes, lnet, httpsocket;

type
  THTTPServer = class(TLTcp)
  private
  public
    constructor Create(AOwner: TComponent); override;

    procedure ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum); override;
    procedure HandleReceive(aSocket: TLSocket);
    procedure HandleSend(aSocket: TLSocket);
  end;

implementation

constructor THTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, THTTPSocket);

  BlockTime := 500;
  OnCanSend := @HandleSend;
  OnReceive := @HandleReceive;
end;

procedure THTTPServer.ControlAction(var aSocket: TLSocket; const anAction: TLActionEnum);
begin
  inherited;

end;

procedure THTTPServer.HandleReceive(aSocket: TLSocket);
begin
  THTTPSocket(aSocket).HandleReceive;
end;

procedure THTTPServer.HandleSend(aSocket: TLSocket);
begin
  THTTPSocket(aSocket).WriteBlock;
end;

end.
