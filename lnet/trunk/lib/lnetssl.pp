unit lNetSSL;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, cTypes, OpenSSL,
  lNet, lEvents;
  
type
  TLStatusSSL = (ssNone, ssConnect, ssShutdown);

  { TLSocketSSL }

  TLSocketSSL = class(TLSocketTCP)
   protected
    FSSL: PSSL;
    FStatusSSL: TLStatusSSL;
    FOnWritePriv: TLHandleEvent;
    FOnReadPriv: TLHandleEvent;
    FConnectionEstablishedCB: TLHandleEvent;
    
    procedure OnReadInternal(aHandle: TLHandle);
    procedure OnWriteInternal(aHandle: TLHandle);

    procedure SetOnRead(const AValue: TLHandleEvent); override;
    procedure SetOnWrite(const AValue: TLHandleEvent); override;

    procedure ConnectionEstablished(aEvent: TLHandleEvent); override;
    
    procedure LogError(const msg: string; const ernum: Integer); override;
   public
    constructor Create; override;
    function Send(const aData; const aSize: Integer): Integer; override;
    function Get(out aData; const aSize: Integer): Integer; override;

    procedure ConnectSSL;
    procedure Disconnect; override;
  end;
  
  function CreateSSLContext(const CAList, KeyFile, Password: string): Boolean;
  function GetSSLContext: PSSL_CTX;
  function IsSSLBlockError(const anError: Longint): Boolean; inline;
  
implementation

var
  SSLContext: PSSL_CTX;
  SSLPassword: string;
  
function PasswordCallback(buf: pChar; num, rwflag: cInt; userdata: Pointer): cInt; cdecl;
begin
  if num < Length(SSLPassword) + 1 then
    Exit(0);

  Move(SSLPassword[1], buf[0], Length(SSLPassword));
  Result := Length(SSLPassword);
end;

function CreateSSLContext(const CAList, KeyFile, Password: string): Boolean;
var
  Method: PSSL_METHOD;
begin
  Result := False;
  SSLPassword := Password;
  if Assigned(SSLContext) then
    SSLCTXFree(SSLContext);
  
  {* Create our context*}
  Method := SslMethodV23;
  SSLContext := SSLCTXNew(Method);
  
  {* Load our keys and certificates*}
  if SslCtxUseCertificateChainFile(SSLContext, KeyFile) = 0 then
    Exit;

  SslCtxSetDefaultPasswdCb(SSLContext, @PasswordCallback);
  if SSLCTXUsePrivateKeyFile(SSLContext, Keyfile, SSL_FILETYPE_PEM) = 0 then
    Exit;

  {* Load the CAs we trust*}
  if SSLCTXLoadVerifyLocations(SSLContext, CAList, pChar(nil)) = 0 then
    Exit;
    
  Result := True;
end;

function GetSSLContext: PSSL_CTX;
begin
  Result := SSLContext;
end;

function IsSSLBlockError(const anError: Longint): Boolean; inline;
begin
  Result := (anError = SSL_ERROR_WANT_READ) or (anError = SSL_ERROR_WANT_READ);
end;

{ TLSocketSSL }

procedure TLSocketSSL.OnReadInternal(aHandle: TLHandle);
begin
  case FStatusSSL of
    ssNone     : if Assigned(FOnReadPriv) then FOnReadPriv(aHandle);
    ssConnect  : ConnectSSL;
    ssShutdown : begin end; // TODO: finish shutdown eventizing
  end;
end;

procedure TLSocketSSL.OnWriteInternal(aHandle: TLHandle);
begin
  case FStatusSSL of
    ssNone     : if Assigned(FOnWritePriv) then FOnWritePriv(aHandle);
    ssConnect  : ConnectSSL;
    ssShutdown : begin end; // TODO: finish shutdown eventizing
  end;
end;

procedure TLSocketSSL.SetOnRead(const AValue: TLHandleEvent);
begin
  FOnReadPriv := aValue;
end;

procedure TLSocketSSL.SetOnWrite(const AValue: TLHandleEvent);
begin
  FOnWritePriv := aValue;
end;

procedure TLSocketSSL.ConnectionEstablished(aEvent: TLHandleEvent);
begin
  if FServerSocket then begin // server socket behaves just like non-SSL
    inherited ConnectionEstablished(aEvent);
    Exit;
  end;
  
  FConnectionEstablishedCB := aEvent;
  FSSL := SSLNew(SSLContext);
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

constructor TLSocketSSL.Create;
begin
  inherited Create;
  
  FOnWrite := @OnWriteInternal;
  FOnRead := @OnReadInternal;
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
    inherited ConnectionEstablished(FConnectionEstablishedCB);
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

initialization
  SslLibraryInit;
  SslLoadErrorStrings;

finalization
  if Assigned(SSLContext) then
    SslCtxFree(SSLContext);
  DestroySSLInterface;

end.

