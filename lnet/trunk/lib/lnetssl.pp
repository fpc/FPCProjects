unit lNetSSL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, cTypes, OpenSSL,
  lNet, lEvents;
  
type
  TLMethodSSL = (msSSLv2or3, msSSLv2, msSSLv3, msTLSv1);
  TLStatusSSL = (ssNone, ssConnect, ssShutdown);

  TLPasswordCB = function(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;

  { TLSSLSocket }

  TLSSLSocket = class(TLSocket)
   protected
    FSSL: PSSL;
    FSSLContext: PSSL_CTX;
    FStatusSSL: TLStatusSSL;
    function GetConnected: Boolean; override;

    function DoSend(const aData; const aSize: Integer): Integer; override;
    function DoGet(out aData; const aSize: Integer): Integer; override;
    
    function HandleResult(const aResult, aOp: Integer): Integer; override;

    function SetActiveSSL(const AValue: Boolean): Boolean;
    procedure SetConnecting(const AValue: Boolean);

    procedure ConnectEvent(aSSLContext: PSSL_CTX);
    procedure ConnectSSL;
    procedure ShutdownSSL;

    procedure LogError(const msg: string; const ernum: Integer); override;
   public
    destructor Destroy; override;
    
    function SetFeature(const aFeature: Byte; const TurnOn: Boolean = True): Boolean; override;

    procedure Disconnect; override;
   public
    property StatusSSL: TLStatusSSL read FStatusSSL;
  end;

  { TLSSLSession }

  TLSSLSession = class(TLSession)
   protected
    FSSLActive: Boolean;
    FSSLContext: PSSL_CTX;
    FPassword: string;
    FCAFile: string;
    FKeyFile: string;
    FMethod: TLMethodSSL;
    FPasswordCallback: TLPasswordCB;
    
    function CanCreateContext: Boolean;
    
    procedure SetCAFile(const AValue: string);
    procedure SetKeyFile(const AValue: string);
    procedure SetPassword(const AValue: string);
    procedure SetMethod(const AValue: TLMethodSSL);
    procedure SetPasswordCallback(const AValue: TLPasswordCB);
    
    procedure CreateSSLContext;
   public
    constructor Create(aOwner: TComponent); override;
    
    procedure RegisterWithComponent(aConnection: TLConnection); override;
    
    procedure InitHandle(aHandle: TLHandle); override;
    
    procedure ConnectEvent(aHandle: TLHandle); override;
    procedure ReceiveEvent(aHandle: TLHandle); override;
    procedure AcceptEvent(aHandle: TLHandle); override;
    function HandleSSLConnection(aSocket: TLSSLSocket): Boolean;
   public
    property Password: string read FPassword write SetPassword;
    property CAFile: string read FCAFile write SetCAFile;
    property KeyFile: string read FKeyFile write SetKeyFile;
    property Method: TLMethodSSL read FMethod write SetMethod;
    property PasswordCallback: TLPasswordCB read FPasswordCallback write SetPasswordCallback;
    property SSLContext: PSSL_CTX read FSSLContext;
    property SSLActive: Boolean read FSSLActive write FSSLActive;
  end;
  
  function IsSSLBlockError(const anError: Longint): Boolean; inline;
  
implementation

uses
  lCommon;

function PasswordCB(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;
var
  S: TLSSLSession;
begin
  S := TLSSLSession(userdata);
  
  if num < Length(S.Password) + 1 then
    Exit(0);

  Move(S.Password[1], buf[0], Length(S.Password));
  Result := Length(S.Password);
end;

function IsSSLBlockError(const anError: Longint): Boolean; inline;
begin
  Result := (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_READ);
end;

{ TLSSLSocket }

function TLSSLSocket.SetActiveSSL(const AValue: Boolean): Boolean;
begin
  Result := False;
  
  if (LSOCKF_SSLACTIVE in FFeatures) = AValue then Exit(True);
  case aValue of
    True  : FFeatures := FFeatures + [LSOCKF_SSLACTIVE];
    False : FFeatures := FFeatures - [LSOCKF_SSLACTIVE];
  end;
  
  if aValue and FConnected then
    ConnectEvent(FSSLContext);
    
  if not aValue then begin
    if Connected then
      ShutdownSSL
    else if FStatusSSL = ssConnect then
      raise Exception.Create('Switching SSL mode on socket during SSL handshake is not supported');
  end;
  
  Result := True;
end;

procedure TLSSLSocket.SetConnecting(const AValue: Boolean);
begin
  FConnecting := AValue;
end;

function TLSSLSocket.GetConnected: Boolean;
begin
  Result := Assigned(FSSL) and (FStatusSSL = ssNone);
end;

function TLSSLSocket.DoSend(const aData; const aSize: Integer): Integer;
begin
  if LSOCKF_SSLACTIVE in FFeatures then
    Result := SSLWrite(FSSL, @aData, aSize)
  else
    Result := inherited DoSend(aData, aSize);
end;

function TLSSLSocket.DoGet(out aData; const aSize: Integer): Integer;
begin
  if LSOCKF_SSLACTIVE in FFeatures then
    Result := SSLRead(FSSL, @aData, aSize)
  else
    Result := inherited DoGet(aData, aSize);
end;

function TLSSLSocket.HandleResult(const aResult, aOp: Integer): Integer;
const
  GSStr: array[0..1] of string = ('SSLWrite', 'SSLRead');
var
  LastError: cInt;
begin
  if not LSOCKF_SSLACTIVE in FFeatures then
    Exit(inherited HandleResult(aResult, aOp));
    
  Result := aResult;
  if Result <= 0 then begin
    LastError := SslGetError(FSSL, Result);
    if IsSSLBlockError(LastError) then case aOp of
      0: begin
           FFeatures := FFeatures - [LSOCKF_CANSEND];
           IgnoreWrite := False;
         end;
      1: begin
           FFeatures := FFeatures - [LSOCKF_CANRECEIVE];
           IgnoreRead := False;
         end;
    end else
      Bail(GSStr[aOp] + ' error', LastError);
    Result := 0;
  end;
end;

procedure TLSSLSocket.ConnectEvent(aSSLContext: PSSL_CTX);
begin
  if Assigned(FSSL) then
    SslFree(FSSL);

  FSSLContext := aSSLContext;

  FSSL := SSLNew(FSSLContext);
  if not Assigned(FSSL) then begin
    Bail('SSLNew error', -1);
    Exit;
  end;

  if SslSetFd(FSSL, FHandle) = 0 then begin
    FSSL := nil;
    Bail('SSL setFD error', -1);
    Exit;
  end;

  ConnectSSL;
end;

procedure TLSSLSocket.LogError(const msg: string; const ernum: Integer);
var
  s: string;
begin
  if not LSOCKF_SSLACTIVE in FFeatures then
    inherited LogError(msg, ernum)
  else if Assigned(FOnError) then begin
    if ernum > 0 then begin
      SetLength(s, 1024);
      ErrErrorString(ernum, s, Length(s));
      FOnError(Self, msg + ': ' + s);
    end else
      FOnError(Self, msg);
  end;
end;

destructor TLSSLSocket.Destroy;
begin
  inherited Destroy;
  SslFree(FSSL);
end;

function TLSSLSocket.SetFeature(const aFeature: Byte; const TurnOn: Boolean
  ): Boolean;
begin
  case aFeature of
    LSOCKF_SSLACTIVE: Result := SetActiveSSL(TurnOn);
  else
    Result := inherited SetFeature(aFeature, TurnOn);
  end;
end;

procedure TLSSLSocket.ConnectSSL;
var
  c: cInt;
begin
  c := SSLConnect(FSSL);
  if c <= 0 then begin
    case SslGetError(FSSL, c) of
      SSL_ERROR_WANT_READ  : begin // make sure we're watching for reads and flag status
                               FStatusSSL := ssConnect;
                               FFeatures := FFeatures - [LSOCKF_CANRECEIVE];
                               IgnoreRead := False;
                             end;
      SSL_ERROR_WANT_WRITE : begin // make sure we're watching for writes and flag status
                               FStatusSSL := ssConnect;
                               FFeatures := FFeatures - [LSOCKF_CANSEND];
                               IgnoreWrite := False;
                             end;
    else
      begin
        Bail('SSL connect error', -1);
        Exit;
      end;
    end;
  end else
    FStatusSSL := ssNone;
end;

procedure TLSSLSocket.ShutdownSSL;
var
  n, c: Integer;
begin
  c := 0;
  if Assigned(FSSL) then begin
    FStatusSSL := ssNone; // for now
    n := SSLShutdown(FSSL); // don't care for now, unless it fails badly
    if n <= 0 then begin
      n := SslGetError(FSSL, n);
      case n of
        SSL_ERROR_WANT_READ,
        SSL_ERROR_WANT_WRITE,
        SSL_ERROR_SYSCALL     : begin end; // ignore
      else
        Bail('SSL shutdown error', n);
      end;
    end;
  end;
end;

procedure TLSSLSocket.Disconnect;
begin
  if LSOCKF_SSLACTIVE in FFeatures then begin
    FStatusSSL := ssShutdown;
    ShutdownSSL;
  end;
  
  inherited Disconnect;
end;

{ TLSSLSession }

function TLSSLSession.CanCreateContext: Boolean;
begin
  Result := (Length(FCAFile) > 0) and (Length(FPassword) > 0) and (Length(FKeyFile) > 0);
end;

procedure TLSSLSession.SetCAFile(const AValue: string);
begin
  if aValue = FCAFile then Exit;
  if FActive then
    raise Exception.Create('Cannot change certificate file on active session');
  FCAFile := aValue;
  
  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSSLSession.SetKeyFile(const AValue: string);
begin
  if aValue = FKeyFile then Exit;
  if FActive then
    raise Exception.Create('Cannot change key file on active session');
  FKeyFile := aValue;
  
  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSSLSession.SetPassword(const AValue: string);
begin
  if aValue = FPassword then Exit;
  if FActive then
    raise Exception.Create('Cannot change password on active session');
  FPassword := aValue;

  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSSLSession.SetMethod(const AValue: TLMethodSSL);
begin
  if aValue = FMethod then Exit;
  if FActive then
    raise Exception.Create('Cannot change SSL method on active session');
  FMethod := aValue;
  
  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSSLSession.SetPasswordCallback(const AValue: TLPasswordCB);
begin
  if aValue = FPasswordCallback then Exit;
  if FActive then
    raise Exception.Create('Cannot change SSL password callback on active session');
  FPasswordCallback := aValue;

  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSSLSession.CreateSSLContext;
var
  aMethod: PSSL_METHOD;
begin
  if not CanCreateContext then
    raise Exception.Create('Cannot create SSL context without password, keyfile and cafile set');

  if FActive then
    raise Exception.Create('Cannot create SSL context on an already active session');
    
  if Assigned(FSSLContext) then
    SSLCTXFree(FSSLContext);

  case FMethod of
    msSSLv2or3 : aMethod := SslMethodV23;
    msSSLv2    : aMethod := SslMethodV2;
    msSSLv3    : aMethod := SslMethodV3;
    msTLSv1    : aMethod := SslMethodTLSV1;
  end;

  FSSLContext := SSLCTXNew(aMethod);
  if not Assigned(FSSLContext) then
    raise Exception.Create('Error creating SSL CTX: SSLCTXNew');

  if SslCtxUseCertificateChainFile(FSSLContext, FKeyFile) = 0 then
    raise Exception.Create('Error creating SSL CTX: SslCtxUseCertificateChainFile');

  SslCtxSetDefaultPasswdCb(FSSLContext, FPasswordCallback);
  SslCtxSetDefaultPasswdCbUserdata(FSSLContext, Self);
  if SSLCTXUsePrivateKeyFile(FSSLContext, FKeyfile, SSL_FILETYPE_PEM) = 0 then
    raise Exception.Create('Error creating SSL CTX: SSLCTXUsePrivateKeyFile');

  if SSLCTXLoadVerifyLocations(FSSLContext, FCAFile, pChar(nil)) = 0 then
    raise Exception.Create('Error creating SSL CTX: SSLCTXLoadVerifyLocations');
end;

constructor TLSSLSession.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPasswordCallback := @PasswordCB;
  FSSLActive := True;
end;

procedure TLSSLSession.RegisterWithComponent(aConnection: TLConnection);
begin
  inherited RegisterWithComponent(aConnection);
  
  if not aConnection.SocketClass.InheritsFrom(TLSSLSocket) then
    aConnection.SocketClass := TLSSLSocket;
end;

procedure TLSSLSession.InitHandle(aHandle: TLHandle);
begin
  inherited;
  if FSSLActive then
    TLSSLSocket(aHandle).SetFeature(LSOCKF_SSLACTIVE);
end;

procedure TLSSLSession.ConnectEvent(aHandle: TLHandle);
begin
  if not (LSOCKF_SSLACTIVE in TLSSLSocket(aHandle).Features) then
    inherited ConnectEvent(aHandle)
  else if HandleSSLConnection(TLSSLSocket(aHandle)) then
    CallConnectEvent(aHandle);
end;

procedure TLSSLSession.ReceiveEvent(aHandle: TLHandle);
begin
  if not (LSOCKF_SSLACTIVE in TLSSLSocket(aHandle).Features) then
    inherited ReceiveEvent(aHandle)
  else if TLSSLSocket(aHandle).FConnecting then begin
    if HandleSSLConnection(TLSSLSocket(aHandle)) then
    case LSOCKF_SERVERSOCKET in TLSSLSocket(aHandle).Features of
      True  : CallAcceptEvent(aHandle);
      False : CallConnectEvent(aHandle);
    end;
  end else
    CallReceiveEvent(aHandle);
end;

procedure TLSSLSession.AcceptEvent(aHandle: TLHandle);
begin
  if not (LSOCKF_SSLACTIVE in TLSSLSocket(aHandle).Features) then
    inherited AcceptEvent(aHandle)
  else if HandleSSLConnection(TLSSLSocket(aHandle)) then
    CallAcceptEvent(aHandle);
end;

function TLSSLSession.HandleSSLConnection(aSocket: TLSSLSocket): Boolean;
begin
  Result := False;
  FActive := True;
  
  if not Assigned(FSSLContext) then
    if not CanCreateContext then
      raise Exception.Create('Context is not/can not be created on connect or accept event')
    else
      CreateSSLContext;

  aSocket.SetConnecting(True);
  case aSocket.FStatusSSL of
    ssNone     : aSocket.ConnectEvent(FSSLContext);
    ssConnect  : aSocket.ConnectSSL;
    ssShutdown : raise Exception.Create('Got ConnectEvent or AcceptEvent on socket with ssShutdown status');
  end;
  
  if aSocket.FStatusSSL = ssNone then begin
    aSocket.SetConnecting(False);
    Result := True;
  end;
end;

initialization
  SslLibraryInit;
  SslLoadErrorStrings;

finalization
  DestroySSLInterface;

end.

