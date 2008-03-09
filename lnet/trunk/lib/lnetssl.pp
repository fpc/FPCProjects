unit lNetSSL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, cTypes, OpenSSL,
  lNet, lEvents;
  
type
  TLMethodSSL = (msSSLv2or3, msSSLv2, msSSLv3, msTLSv1);
  TLStatusSSL = (ssNone, ssConnect, ssShutdown);

  TLPasswordCB = function(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;

  { TLSocketSSL }

  TLSocketSSL = class(TLSocketTCP)
   protected
    FSSL: PSSL;
    FSSLContext: PSSL_CTX;
    FStatusSSL: TLStatusSSL;
    FActiveSSL: Boolean;
    function GetConnected: Boolean; override;
    procedure SetActiveSSL(const AValue: Boolean);

    procedure ConnectEvent(aSSLContext: PSSL_CTX);
    procedure ConnectSSL;
    procedure ShutdownSSL;

    procedure LogError(const msg: string; const ernum: Integer); override;
   public
    constructor Create; override;
    destructor Destroy; override;
    function Send(const aData; const aSize: Integer): Integer; override;
    function Get(out aData; const aSize: Integer): Integer; override;

    procedure Disconnect; override;
   public
    property ActiveSSL: Boolean read FActiveSSL write SetActiveSSL;
  end;
  
  { TLSessionSSL }
  
  TLSessionSSL = class(TLSession)
   protected
    FActiveSSL: Boolean;
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
    constructor Create;
    
    procedure RegisterWithComponent(aComponent: TLComponent); override;
    
    procedure InitHandle(aHandle: TLHandle); override;
    
    procedure ConnectEvent(aHandle: TLHandle; const aOnConnect: TLHandleEvent); override;
    procedure AcceptEvent(aHandle: TLHandle; const aOnAccept: TLHandleEvent); override;
    procedure ReceiveEvent(aHandle: TLHandle; const aOnReceive: TLHandleEvent); override;
    procedure SendEvent(aHandle: TLHandle; const aOnSend: TLHandleEvent); override;
   public
    property Password: string read FPassword write SetPassword;
    property CAFile: string read FCAFile write SetCAFile;
    property KeyFile: string read FKeyFile write SetKeyFile;
    property Method: TLMethodSSL read FMethod write SetMethod;
    property PasswordCallback: TLPasswordCB read FPasswordCallback write SetPasswordCallback;
    property SSLContext: PSSL_CTX read FSSLContext;
    property ActiveSSL: Boolean read FActiveSSL write FActiveSSL;
  end;
  
  function IsSSLBlockError(const anError: Longint): Boolean; inline;
  
implementation

uses
  lCommon;

function PasswordCB(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;
var
  S: TLSessionSSL;
begin
  S := TLSessionSSL(userdata);
  
  if num < Length(S.Password) + 1 then
    Exit(0);

  Move(S.Password[1], buf[0], Length(S.Password));
  Result := Length(S.Password);
end;

function IsSSLBlockError(const anError: Longint): Boolean; inline;
begin
  Result := (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_READ);
end;

{ TLSocketSSL }

procedure TLSocketSSL.SetActiveSSL(const AValue: Boolean);
begin
  if FActiveSSL = AValue then Exit;
  FActiveSSL := AValue;
  
  if aValue and FConnected then
    ConnectEvent(FSSLContext);
    
  if not aValue and Connected then
    ShutdownSSL;
end;

function TLSocketSSL.GetConnected: Boolean;
begin
  if FActiveSSL then
    Result := inherited GetConnected
  else
    Result := FConnected and Assigned(FSSL) and (FStatusSSL = ssNone)
end;

procedure TLSocketSSL.ConnectEvent(aSSLContext: PSSL_CTX);
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

procedure TLSocketSSL.LogError(const msg: string; const ernum: Integer);
var
  s: string;
begin
  if not FActiveSSL then
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

constructor TLSocketSSL.Create;
begin
  inherited Create;
  FActiveSSL := True;
end;

destructor TLSocketSSL.Destroy;
begin
  SslFree(FSSL);
  inherited Destroy;
end;

function TLSocketSSL.Send(const aData; const aSize: Integer): Integer;
var
  LastError: Longint;
begin
  if not FActiveSSL then
    Exit(inherited Send(aData, aSize));

  Result := 0;

  if not FServerSocket then begin
    if aSize <= 0 then begin
      Bail('Send error: wrong size (Size <= 0)', -1);
      Exit(0);
    end;

    if CanSend then begin
      Result := SSLWrite(FSSL, @aData, aSize);

      if Result <= 0 then begin // TODO: SSL error handling
        LastError := SslGetError(FSSL, Result);
        if IsSSLBlockError(LastError) then begin
          FCanSend := False;
          IgnoreWrite := False;
        end else
          Bail('SSLWrite error', LastError);
        Result := 0;
      end;
    end;
  end;
end;


function TLSocketSSL.Get(out aData; const aSize: Integer): Integer;
var
  LastError: Longint;
begin
  if not FActiveSSL then
    Exit(inherited Get(aData, aSize));

  Result := 0;

  if CanReceive then begin
    Result := SSLRead(FSSL, @aData, aSize);

    if Result = 0 then
      Disconnect;

    if Result <= 0 then begin
      LastError := SslGetError(FSSL, Result);
      if IsSSLBlockError(LastError) then begin
        FCanReceive := False;
        IgnoreRead := False;
      end else
        Bail('SSLRead Error', LastError);
      Result := 0;
    end;
  end;
end;

procedure TLSocketSSL.ConnectSSL;
var
  c: cInt;
begin
  c := SSLConnect(FSSL);
  if c <= 0 then begin
    case SslGetError(FSSL, c) of
      SSL_ERROR_WANT_READ  : begin // make sure we're watching for reads and flag status
                               FStatusSSL := ssConnect;
                               FCanReceive := False;
                               IgnoreRead := False;
                             end;
      SSL_ERROR_WANT_WRITE : begin // make sure we're watching for writes and flag status
                               FStatusSSL := ssConnect;
                               FCanSend := False;
                               IgnoreWrite := False;
                             end;
    else
      begin
        Bail('SSL connect error', -1);
        Exit;
      end;
    end;
  end else begin
    FStatusSSL := ssNone;
    FSession.CallConnectEvent(Self);
  end;
end;

procedure TLSocketSSL.ShutdownSSL;
var
  n, c: Integer;
begin
  c := 0;
  if Assigned(FSSL) then begin
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

procedure TLSocketSSL.Disconnect;
begin
  if FActiveSSL then begin
    FStatusSSL := ssShutdown;
    ShutdownSSL;
  end;
  
  inherited Disconnect;
end;

{ TLSessionSSL }

function TLSessionSSL.CanCreateContext: Boolean;
begin
  Result := (Length(FCAFile) > 0) and (Length(FPassword) > 0) and (Length(FKeyFile) > 0);
end;

procedure TLSessionSSL.SetCAFile(const AValue: string);
begin
  if aValue = FCAFile then Exit;
  if FActive then
    raise Exception.Create('Cannot change certificate file on active session');
  FCAFile := aValue;
  
  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSessionSSL.SetKeyFile(const AValue: string);
begin
  if aValue = FKeyFile then Exit;
  if FActive then
    raise Exception.Create('Cannot change key file on active session');
  FKeyFile := aValue;
  
  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSessionSSL.SetPassword(const AValue: string);
begin
  if aValue = FPassword then Exit;
  if FActive then
    raise Exception.Create('Cannot change password on active session');
  FPassword := aValue;

  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSessionSSL.SetMethod(const AValue: TLMethodSSL);
begin
  if aValue = FMethod then Exit;
  if FActive then
    raise Exception.Create('Cannot change SSL method on active session');
  FMethod := aValue;
  
  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSessionSSL.SetPasswordCallback(const AValue: TLPasswordCB);
begin
  if aValue = FPasswordCallback then Exit;
  if FActive then
    raise Exception.Create('Cannot change SSL password callback on active session');
  FPasswordCallback := aValue;

  if CanCreateContext then
    CreateSSLContext;
end;

procedure TLSessionSSL.CreateSSLContext;
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

constructor TLSessionSSL.Create;
begin
  FPasswordCallback := @PasswordCB;
end;

procedure TLSessionSSL.RegisterWithComponent(aComponent: TLComponent);
begin
  inherited RegisterWithComponent(aComponent);
  aComponent.SocketClass := TLSocketSSL;
end;

procedure TLSessionSSL.InitHandle(aHandle: TLHandle);
begin
  TLSocketSSL(aHandle).FActiveSSL := FActiveSSL;
end;

procedure TLSessionSSL.ConnectEvent(aHandle: TLHandle;
  const aOnConnect: TLHandleEvent);
begin
  if TLSocketSSL(aHandle).ActiveSSL then
    inherited ConnectEvent(aHandle, aOnConnect)
  else begin
    FActive := True;
    FOnConnect := aOnConnect;

    if not Assigned(FSSLContext) then
      if not CanCreateContext then
        raise Exception.Create('Context is not/can not be created on connect event')
      else
        CreateSSLContext;

    TLSocketSSL(aHandle).ConnectEvent(FSSLContext);
  end;
end;

procedure TLSessionSSL.AcceptEvent(aHandle: TLHandle;
  const aOnAccept: TLHandleEvent);
begin
  if TLSocketSSL(aHandle).ActiveSSL then
    inherited AcceptEvent(aHandle, aOnAccept)
  else begin
    FActive := True;
    FOnConnect := aOnAccept;

    if not Assigned(FSSLContext) then
      if not CanCreateContext then
        raise Exception.Create('Context is not/can not be created on connect event')
      else
        CreateSSLContext;

    TLSocketSSL(aHandle).ConnectEvent(FSSLContext);
  end;
end;

procedure TLSessionSSL.ReceiveEvent(aHandle: TLHandle; const aOnReceive: TLHandleEvent);
begin
  if TLSocketSSL(aHandle).ActiveSSL then
    inherited ReceiveEvent(aHandle, aOnReceive)
  else begin
    FActive := True;
    FOnReceive := aOnReceive;

    case TLSocketSSL(aHandle).FStatusSSL of
      ssNone     : CallReceiveEvent(aHandle);
      ssConnect  : TLSocketSSL(aHandle).ConnectSSL;
      ssShutdown : begin end; // TODO: finish shutdown eventizing
    end;
  end;
end;

procedure TLSessionSSL.SendEvent(aHandle: TLHandle; const aOnSend: TLHandleEvent);
begin
  if TLSocketSSL(aHandle).ActiveSSL then
    inherited SendEvent(aHandle, aOnSend)
  else begin
    FActive := True;
    FOnSend := aOnSend;

    case TLSocketSSL(aHandle).FStatusSSL of
      ssNone     : CallSendEvent(aHandle);
      ssConnect  : TLSocketSSL(aHandle).ConnectSSL;
      ssShutdown : begin end; // TODO: finish shutdown eventizing
    end;
  end;
end;

initialization
  SslLibraryInit;
  SslLoadErrorStrings;

finalization
  DestroySSLInterface;

end.

