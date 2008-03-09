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
    FStatusSSL: TLStatusSSL;

    procedure ConnectEvent(aSSLContext: PSSL_CTX);
    procedure ConnectSSL;

    procedure LogError(const msg: string; const ernum: Integer); override;
   public
    function Send(const aData; const aSize: Integer): Integer; override;
    function Get(out aData; const aSize: Integer): Integer; override;

    procedure Disconnect; override;
  end;
  
  { TLSessionSSL }
  
  TLSessionSSL = class(TLSession)
   protected
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
    
    procedure ConnectEvent(aHandle: TLHandle; const aOnConnect: TLHandleEvent); override;
    procedure ReceiveEvent(aHandle: TLHandle; const aOnReceive: TLHandleEvent); override;
    procedure SendEvent(aHandle: TLHandle; const aOnSend: TLHandleEvent); override;
   public
    property Password: string read FPassword write SetPassword;
    property CAFile: string read FCAFile write SetCAFile;
    property KeyFile: string read FKeyFile write SetKeyFile;
    property Method: TLMethodSSL read FMethod write SetMethod;
    property PasswordCallback: TLPasswordCB read FPasswordCallback write SetPasswordCallback;
    property SSLContext: PSSL_CTX read FSSLContext;
  end;
  
  function IsSSLBlockError(const anError: Longint): Boolean; inline;
  
implementation

function PasswordCB(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;
var
  S: TLSessionSSL;
begin
  Writeln('Password callback executing...');
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

procedure TLSocketSSL.ConnectEvent(aSSLContext: PSSL_CTX);
begin
  if Assigned(FSSL) then
    SslFree(FSSL);
    
  FSSL := SSLNew(aSSLContext);
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
  if Assigned(FOnError) then
    if ernum > 0 then begin
      ErrErrorString(ernum, s, 1024);
      FOnError(Self, msg + s);
    end else
      FOnError(Self, msg);
end;

function TLSocketSSL.Send(const aData; const aSize: Integer): Integer;
var
  LastError: Longint;
begin
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
          Bail('SSL_Write error', LastError);
        Result := 0;
      end;
    end;
  end;
end;


function TLSocketSSL.Get(out aData; const aSize: Integer): Integer;
var
  LastError: Longint;
begin
  Result := 0;

  if CanReceive then begin
    Result := SSLRead(FSSL, @aData, aSize);

    if Result = 0 then
      Disconnect;

    if Result <= 0 then begin
      LastError := SslGetError(FSSL, Result);
      if IsSSLBlockError(LastError) then begin
        FCanReceive  :=  False;
        IgnoreRead  :=  False;
      end else
        Bail('Receive Error', LastError);
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

procedure TLSocketSSL.Disconnect;
var
  n, c: Integer;
begin
  {$WARNING Handle non-blocking shutdown here via TLEventer somehow}
  c := 0;
  if Assigned(FSSL) then
    repeat
      n := SSLShutdown(FSSL);
      case n of
        -1 : Bail('SSL shutdown error', -1); // TODO: handle non-blocking shutdown here via TLEventer somehow
         0 : case SslGetError(FSSL, n) of
               SSL_ERROR_WANT_READ  : Sleep(1);
               SSL_ERROR_WANT_WRITE : Sleep(1);
               SSL_ERROR_SYSCALL    : Sleep(1);
             end;
      end;
      Inc(c);
    until (n > 0) or (n < 0) or (c > 2);
    
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

  SslCtxSetDefaultPasswdCb(FSSLContext, @FPasswordCallback);
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

procedure TLSessionSSL.ConnectEvent(aHandle: TLHandle;
  const aOnConnect: TLHandleEvent);
begin
  FActive := True;
  FOnConnect := aOnConnect;

  if not Assigned(FSSLContext) then
    if not CanCreateContext then
      raise Exception.Create('Context is not/can not be created on connect event')
    else
      CreateSSLContext;
      
  TLSocketSSL(aHandle).ConnectEvent(FSSLContext);
end;

procedure TLSessionSSL.ReceiveEvent(aHandle: TLHandle; const aOnReceive: TLHandleEvent);
begin
  FActive := True;
  FOnReceive := aOnReceive;
  
  case TLSocketSSL(aHandle).FStatusSSL of
    ssNone     : CallReceiveEvent(aHandle);
    ssConnect  : TLSocketSSL(aHandle).ConnectSSL;
    ssShutdown : begin end; // TODO: finish shutdown eventizing
  end;
end;

procedure TLSessionSSL.SendEvent(aHandle: TLHandle; const aOnSend: TLHandleEvent);
begin
  FActive := True;
  FOnSend := aOnSend;
  
  case TLSocketSSL(aHandle).FStatusSSL of
    ssNone     : CallSendEvent(aHandle);
    ssConnect  : TLSocketSSL(aHandle).ConnectSSL;
    ssShutdown : begin end; // TODO: finish shutdown eventizing
  end;
end;

initialization
  SslLibraryInit;
  SslLoadErrorStrings;

finalization
  DestroySSLInterface;

end.

