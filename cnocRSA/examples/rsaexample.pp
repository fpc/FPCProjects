program rsaexample;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  strutils,
  FmtBCD,
  CustApp,
  cnocRSA;

type

  { TRSAEncryptionExample }

  TRSAEncryptionExample = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure Verify();
    procedure Sign();
    procedure GenerateKey(PEMFormat: Boolean; Bits, Exp: Integer);
    procedure SaveKey(RSAKey: TcnocRSAPublicKey; PEMFormat: Boolean);
    procedure ComposePublicKey(PEMFormat: Boolean);
    procedure ComposePrivateKey(PEMFormat: Boolean);
    function ObtainPublicKey: TcnocRSAPublicKey;
    function ObtainPrivateKey: TcnocRSAPrivateKey;
    function GetFileContent(AFileName: string): TBytes;
    function GetContent: TBytes;
    function GetSignature: TBytes;
    procedure SetBigNumParam(BN: TcnocRSABignum; Value: string);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteHelp; virtual;
  end;

{ TRSAEncryptionExample }

procedure TRSAEncryptionExample.DoRun;
var
  ErrorMsg: string;
  CommandList: TStrings;
  Command: string;
begin
  // quick check parameters
  CommandList := TStringList.Create;
  try
    ErrorMsg := CheckOptions('hn:e:c:s:o:d:p:q:1:2:f:', ['help', 'number:', 'exponent:', 'content:', 'signature:', 'output:', 'privateexp:', 'prime1:', 'prime2:', 'exponent1:', 'exponent2:', 'coefficient:'], nil, CommandList);
    if CommandList.Count <> 1 then
      begin
      WriteLn('No command given');
      WriteHelp;
      Terminate;
      Exit;
      end
    else
      Command := CommandList.Strings[0];
  finally
    CommandList.Free;
  end;

  if ErrorMsg <> '' then
    begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
    end;

  // parse parameters
  if HasOption('h', 'help') or (command = 'help') then
    begin
    WriteHelp;
    Terminate;
    Exit;
    end;

  case Command of
    'verify': Verify;
    'composepublicpemkey' : ComposePublicKey(True);
    'composepublicderkey' : ComposePublicKey(False);
    'composeprivatepemkey': ComposePrivateKey(True);
    'composeprivatederkey': ComposePrivateKey(False);
    'generatepemkey'      : GenerateKey(True, 1024, 65535);
    'generatederkey'      : GenerateKey(False, 1024, 65535);
    'sign'                : Sign;
  else
    writeln('Unknown command '''+ Command +'''. Try ''help''.');
  end;

  // stop program loop
  Terminate;
end;

procedure TRSAEncryptionExample.Verify();
var
  RSAKey: TcnocRSAPublicKey;
begin
  RSAKey := ObtainPublicKey;
  try
    if RSAKey.Verify(GetContent, GetSignature) then
      WriteLn('Signature OK')
    else
      WriteLn('Invalid signature');
  finally
    RSAKey.Free;
  end;
end;

procedure TRSAEncryptionExample.Sign();
var
  RSAKey: TcnocRSAPrivateKey;
  Signature: TBytes;
  FS: TFileStream;
begin
  if not HasOption('o', 'output') then
    raise Exception.Create('Missing obligatory ''output'' parameter');
  RSAKey := ObtainPrivateKey;
  try
    Signature := RSAKey.Sign(GetContent);
    FS := TFileStream.Create(GetOptionValue('o', 'output'), fmCreate);
    try
      FS.WriteBuffer(Signature[0], length(Signature));
    finally
      FS.Free;
    end;
  finally
    RSAKey.Free;
  end;
end;

procedure TRSAEncryptionExample.GenerateKey(PEMFormat: Boolean; Bits, Exp: Integer);
var
  RSAKey: TcnocRSAPrivateKey;
begin
  RSAKey := TcnocRSAPrivateKey.Create;
  try
    RSAKey.Generate(Bits, Exp);
    SaveKey(RSAKey, PEMFormat);
  finally
    RSAKey.free;
  end;
end;

procedure TRSAEncryptionExample.SaveKey(RSAKey: TcnocRSAPublicKey; PEMFormat: Boolean);
var
  FS: TFileStream;
begin
  if not HasOption('o', 'output') then
    raise Exception.Create('Missing obligatory ''output'' parameter');
  FS := TFileStream.Create(GetOptionValue('o', 'output'), fmCreate);
  try
    if PEMFormat then
      RSAKey.SaveToPEMStream(FS)
    else
      RSAKey.SaveToDERStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TRSAEncryptionExample.ComposePublicKey(PEMFormat: Boolean);
var
  RSAKey: TcnocRSAPublicKey;
begin
  RSAKey := ObtainPublicKey;
  try
    SaveKey(RSAKey, PEMFormat);
  finally
    RSAKey.Free;
  end;
end;

procedure TRSAEncryptionExample.ComposePrivateKey(PEMFormat: Boolean);
var
  RSAKey: TcnocRSAPrivateKey;
begin
  RSAKey := ObtainPrivateKey;
  try
    SaveKey(RSAKey, PEMFormat);
  finally
    RSAKey.Free;
  end;
end;

function TRSAEncryptionExample.ObtainPublicKey: TcnocRSAPublicKey;
begin
  Result := TcnocRSAPublicKey.Create;
  try
    if HasOption('e','exponent') then
      begin
      SetBigNumParam(Result.e, GetOptionValue('e', 'exponent'));
      if not HasOption('n','number') then
        raise Exception.Create('Exponent-parameter without number-parameter');
      SetBigNumParam(Result.n, GetOptionValue('n', 'number'));
      end
    else
      begin
      raise Exception.Create('No key given (number and exponent parameter)');
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TRSAEncryptionExample.ObtainPrivateKey: TcnocRSAPrivateKey;
begin
  Result := TcnocRSAPrivateKey.Create;
  try
    if HasOption('e','exponent') then
      begin
      SetBigNumParam(Result.e, GetOptionValue('e', 'exponent'));
      if (not HasOption('n', 'number')) or
         (not HasOption('d', 'privateexponent') ) or
         (not HasOption('p', 'prime1') ) or
         (not HasOption('1', 'exponent1') ) or
         (not HasOption('2', 'exponent2') ) or
         (not HasOption('f', 'coefficient') ) then
        raise Exception.Create('Not all parameters necessary for creating a private key are given');
      SetBigNumParam(Result.n, GetOptionValue('n', 'number'));
      SetBigNumParam(Result.d, GetOptionValue('d', 'privateexp'));
      SetBigNumParam(Result.p, GetOptionValue('p', 'prime1'));
      SetBigNumParam(Result.q, GetOptionValue('q', 'prime2'));
      SetBigNumParam(Result.dmp1, GetOptionValue('1', 'exponent1'));
      SetBigNumParam(Result.dmq1, GetOptionValue('2', 'exponent2'));
      SetBigNumParam(Result.iqmp, GetOptionValue('f', 'coefficient'));
      end
    else
      begin
      raise Exception.Create('No key given (number and exponent parameter)');
      end;
  except
    Result.Free;
    raise;
  end;
end;

function TRSAEncryptionExample.GetFileContent(AFileName: string): TBytes;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(Result, FS.Size);
    FS.ReadBuffer(Result[0], FS.Size);
  finally
    FS.Free;
  end;
end;

function TRSAEncryptionExample.GetContent: TBytes;
begin
  if HasOption('c', 'content') then
    Result := GetFileContent(GetOptionValue('c','content'))
  else
    raise Exception.Create('Missing content');
end;

function TRSAEncryptionExample.GetSignature: TBytes;
begin
  if HasOption('s', 'signature') then
    Result := GetFileContent(GetOptionValue('s','signature'))
  else
    raise Exception.Create('Missing signature');
end;

procedure TRSAEncryptionExample.SetBigNumParam(BN: TcnocRSABignum; Value: string);
var
  i: Int64;
  ii: Integer;
  b: tBCD;
  bt: TBytes;
begin
  if Value='' then
    raise Exception.Create('Empty string is not a valid number');
  if SameText(copy(Value, 1, 2), '0x') then
    begin
    SetLength(bt, (length(Value)-2) div 2);
    for i := 1 to Length(bt) do
      begin
      ii := Hex2Dec(Copy(Value, (i*2)+1, 2));
      bt[i-1] := ii;
      end;
    BN.AsBinaryUnsigned := bt;
    end
  else if TryStrToInt64(Value, i) then
    BN.AsInt64 := i
  else if TryStrToBCD(Value, b) then
    BN.AsBCD := b
  else if (pos('-', Value) > 0) or (pos('_', Value) > 0) then
    BN.AsBase64UrlEncoded := Value
  else
    BN.AsBase64Encoded := Value
end;

constructor TRSAEncryptionExample.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

procedure TRSAEncryptionExample.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' [command] <options> ');
  writeln('');
  writeln('Possible commands:');
  writeln('  help                  Write this help and exit');
  writeln('  verify                Verify RSA-signature');
  writeln('  composepublicderkey   Compose a public key (DER-format)');
  writeln('  composepublicpemkey   Compose a public key (PEM-format)');
  writeln('  composeprivatederkey  Compose a private key (DER-format)');
  writeln('  composeprivatepemkey  Compose a private key (PEM-format)');
  writeln('  generatepemkey        Generate a new key (PEM-format)');
  writeln('  generatederkey        Generate a new key (PEM-format)');
  writeln('');
  writeln('Possible options:');
  writeln('  -h                    Write this help and exit');
  writeln('  --content=<value>     The name of the file with content to sign/verify');
  writeln('    -c <value>');
  writeln('  --output=<value>      The name of the file for the output');
  writeln('    -o <value>');
  writeln('  --signature=<value>   The name of the file with signature to verify');
  writeln('    -s <value>');
  writeln('');
  writeln('Options to define the different components of a key:');
  writeln('(formatted as decimal, hexadecimal, base64- or base64url-encoded)');
  writeln('  --number=<value>      Specify the number/modulus-part of a RSA key');
  writeln('    -n <value>');
  writeln('  --exponent=<value>    Specify the exponent-part of a RSA key');
  writeln('    -e <value>');
  writeln('  --privateexp=<value>  Specify the private-exponent-part of a RSA-private key');
  writeln('    -d <value>');
  writeln('  --prime1=<value>      Specify the prime1-part of a RSA-private key');
  writeln('    -p <value>');
  writeln('  --prime2=<value>      Specify the prime2-part of a RSA-private key');
  writeln('    -q <value>');
  writeln('  --exponent1=<value>   Specify the exponent-1/dmp1-part of a RSA-private key');
  writeln('    -1 <value>');
  writeln('  --exponent2=<value>   Specify the exponent-2/dmq1-part of a RSA-private key');
  writeln('    -2 <value>');
  writeln('  --coefficient=<value> Specify the coefficient-part of a RSA-private key');
  writeln('    -f <value>');

  writeln('');
  writeln('examples:');
  writeln('  ', ExtractFileName(ExeName), ' composepublicpemkey -o out.pem -e AQAB -n 0xffaa3355');
  writeln('  ', ExtractFileName(ExeName), ' verify -s sigfile -c contfile -e 65535 -n uw4EOCj7U1re');
end;

var
  Application: TRSAEncryptionExample;
begin
  Application := TRSAEncryptionExample.Create(nil);
  Application.Title := 'RSA Encryption Example';
  Application.Run;
  Application.Free;
end.

