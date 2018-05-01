unit cnocRSA;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  classes,
  strutils,
  ctypes,
  base64,
  fpjwt,
  cnocASN1,
  cnocASN1Encoder,
  cnocASN1Element,
  openssl;

type

  { TcnocRSABignum }

  TcnocRSABignum = class(TcnocASN1IntegerElement)
  private
    function GetAsBase64Encoded: string;
    procedure SetAsBase64Encoded(AValue: string);

    procedure SetAsBase64UrlEncoded(AValue: string);
    function GetAsBase64UrlEncoded: string;
  public
    function Clone: TcnocRSABignum;
    property AsBase64Encoded: string read GetAsBase64Encoded write SetAsBase64Encoded;
    property AsBase64UrlEncoded: string read GetAsBase64UrlEncoded write SetAsBase64UrlEncoded;
  end;

  { TcnocRSAPublicKey }

  TcnocRSAPublicKey = class(IcnocASN1EncodableClass)
  private
    Fn: TcnocRSABignum;
    Fe: TcnocRSABignum;
    FObjIdentifierSequence: TcnocASN1SequenceElement;
    FBitStr: TcnocASN1GenericElement;
    procedure CreateASN1Structures(AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
  protected
    function GetASN1HeaderInfo(
      out AClass: TcnocASN1Class; out ATag: Integer; out AnEncoding: TcnocASN1Encoding; out ALength: QWord;
      AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean; virtual;
    procedure StreamASN1Content(
      AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format); virtual;
    function GetPEMDescription: string; virtual;
    function GetOpenSSLKey: PEVP_PKEY; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SaveToDERStream(AStream: TStream); virtual;
    procedure SaveToPEMStream(AStream: TStream); virtual;

    function Verify(Content, Signature: TBytes): Boolean;

    property n    : TcnocRSABignum read Fn write Fn;       // public modulus
    property e    : TcnocRSABignum read Fe write Fe;       // public exponent
  end;

  { TcnocRSAPrivateKey }

  TcnocRSAPrivateKey = class(TcnocRSAPublicKey)
  private
    FVer: TcnocASN1IntegerElement;
    Fd: TcnocRSABignum;
    Fdmp1: TcnocRSABignum;
    Fdmq1: TcnocRSABignum;
    Fiqmp: TcnocRSABignum;
    Fp: TcnocRSABignum;
    Fq: TcnocRSABignum;
  protected
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format); override;
    function GetPEMDescription: string; override;
    function GetOpenSSLKey: PEVP_PKEY; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Sign(Content: TBytes): TBytes;

    property Ver  : TcnocASN1IntegerElement read FVer write FVer; // Version
    property d    : TcnocRSABignum read Fd write Fd;       // private exponent
    property p    : TcnocRSABignum read Fp write Fp;       // secret prime factor
    property q    : TcnocRSABignum read Fq write Fq;       // secret prime factor
    property dmp1 : TcnocRSABignum read Fdmp1 write Fdmp1; // d mod (p-1)
    property dmq1 : TcnocRSABignum read Fdmq1 write Fdmq1; // d mod (q-1)
    property iqmp : TcnocRSABignum read Fiqmp write Fiqmp; // q^-1 mod p
  end;

  { TcnocLineWrapStream }

  TcnocLineWrapStream = class(TOwnerStream)
  private
    FLineWidth: Integer;
    procedure SetLineWidth(AValue: Integer);
  protected
    BytesProcessed: Integer;
  public
    Constructor Create(ASource : TStream);
    function Write(const Buffer; Count: Longint): Longint; override;
    property LineWidth: Integer read FLineWidth write SetLineWidth;
  end;

implementation

{ TcnocLineWrapStream }

procedure TcnocLineWrapStream.SetLineWidth(AValue: Integer);
begin
  if AValue = 0 then
    raise Exception.Create('Can not use a line-width of 0');
  FLineWidth := AValue;
end;

Constructor TcnocLineWrapStream.Create(ASource: TStream);
begin
  inherited Create(ASource);
  //  FLineWidth := 64;
  FLineWidth := 64;
end;

function TcnocLineWrapStream.Write(const Buffer; Count: Longint): Longint;
var
  p: Integer;
  b, WriteBytes: Integer;
  LineEnd: char;
  ptr: pchar;
begin
  LineEnd := #10;
  p := 0;
  ptr := @Buffer;
  repeat

  if (BytesProcessed > 0) and (BytesProcessed mod FLineWidth = 0) then
    begin
    Source.Write(LineEnd, 1);
    BytesProcessed := 0;
    end;

  if (BytesProcessed + Count) > FLineWidth then
    WriteBytes := FLineWidth - BytesProcessed
  else
    WriteBytes := Count;

  b := Source.Write(ptr^, WriteBytes);
  p := p + b;
  ptr := ptr + p;
  dec(Count, b);
  inc(BytesProcessed, b);

  if b < WriteBytes then
    break;

  until Count = 0;
  result := p;
end;

{ TcnocRSAPrivateKey }

function TcnocRSAPrivateKey.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format): Boolean;
begin
  AClass := cacUniversal;
  ATag := 16;
  AnEncoding := caeConstructed;
  ALength :=
    AnEncoder.GetElementLength(Ver, AFormat, True) +
    AnEncoder.GetElementLength(n, AFormat, True) +
    AnEncoder.GetElementLength(e, AFormat, True) +
    AnEncoder.GetElementLength(d, AFormat, True) +
    AnEncoder.GetElementLength(p, AFormat, True) +
    AnEncoder.GetElementLength(q, AFormat, True) +
    AnEncoder.GetElementLength(dmp1, AFormat, True) +
    AnEncoder.GetElementLength(dmq1, AFormat, True) +
    AnEncoder.GetElementLength(iqmp, AFormat, True);
end;

procedure TcnocRSAPrivateKey.StreamASN1Content(AContentStream: TStream;
  AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, Ver);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, n);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, e);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, d);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, p);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, q);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, dmp1);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, dmq1);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, iqmp);
end;

function TcnocRSAPrivateKey.GetPEMDescription: string;
begin
  Result := 'RSA PRIVATE KEY';
end;

function TcnocRSAPrivateKey.GetOpenSSLKey: PEVP_PKEY;
var
  MemStream: TMemoryStream;
  ptr: pointer;
  s: string;
begin
  MemStream := TMemoryStream.Create;
   try
     SaveToDERStream(MemStream);
     ptr := MemStream.Memory;
     Result := d2i_AutoPrivateKey(nil, @ptr, MemStream.Size);
     if not assigned(Result) then
       begin
       setlength(s,200);
       ErrErrorString(ErrGetError, s, 200);
       raise Exception.CreateFmt('Invalid key: %s', [s]);
       end;
   finally
     MemStream.Free;
   end;
end;

constructor TcnocRSAPrivateKey.Create;
begin
  inherited Create;
  FVer := TcnocASN1IntegerElement.Create;
  Fd := TcnocRSABignum.Create;
  Fdmp1 := TcnocRSABignum.Create;
  Fdmq1 := TcnocRSABignum.Create;
  Fiqmp := TcnocRSABignum.Create;
  Fp := TcnocRSABignum.Create;
  Fq := TcnocRSABignum.Create;

  Fd.AsInt64 := -1;
  Fdmp1.AsInt64 := -1;
  Fdmq1.AsInt64 := -1;
  Fiqmp.AsInt64 := -1;
  Fp.AsInt64 := -1;
  Fq.AsInt64 := -1;
end;

destructor TcnocRSAPrivateKey.Destroy;
begin
  Fd.Free;
  Fdmp1.Free;
  Fdmq1.Free;
  Fiqmp.Free;
  Fp.Free;
  Fq.Free;
  FVer.Free;
  inherited Destroy;
end;

function TcnocRSAPrivateKey.Sign(Content: TBytes): TBytes;
var
  mdctx: PEVP_MD_CTX;
  key: PEVP_PKEY;
  siglen: csize_t;
begin
  Key := GetOpenSSLKey;

  mdctx := EVP_MD_CTX_create;
  try
    if not Assigned(mdctx) then
      raise Exception.Create('Failed to create Message Digest Context');

    if EVP_DigestSignInit(mdctx, Nil, EVP_sha256, Nil, key) <> 1 then
      raise Exception.Create('Failed to initialise the Digest-Sign operation');

    if EVP_DigestSignUpdate(mdctx, @Content[0], Length(Content)) <> 1 then
      raise Exception.Create('Failed to process the content');

    if EVP_DigestSignFinal(mdctx, nil, @siglen) <> 1 then
      raise Exception.Create('Failed to retrieve the signature length');

    SetLength(Result, siglen);
    if EVP_DigestSignFinal(mdctx, @Result[0], @siglen) <> 1 then
      raise Exception.Create('Failed to sign the content');

    SetLength(Result, siglen);
  finally
    EVP_MD_CTX_destroy(mdctx);
  end;
end;

{ TcnocRSAPublicKey }

procedure TcnocRSAPublicKey.CreateASN1Structures(AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format);
var
  ObjIdentifier: TcnocASN1ObjectIdentifierElement;
  KeySequence: TcnocASN1SequenceElement;
  ObjIdentifierArr: TBoundArray;
  B: TBytes;
begin
  FObjIdentifierSequence.Free;
  FBitStr.Free;

  FObjIdentifierSequence := TcnocASN1SequenceElement.Create;
  try
    FObjIdentifierSequence.FixedLength := True;
    ObjIdentifier := TcnocASN1ObjectIdentifierElement.Create;
    FObjIdentifierSequence.Items.Add(ObjIdentifier);
    SetLength(ObjIdentifierArr, 7);
    ObjIdentifierArr[0] := 1;
    ObjIdentifierArr[1] := 2;
    ObjIdentifierArr[2] := 840;
    ObjIdentifierArr[3] := 113549;
    ObjIdentifierArr[4] := 1;
    ObjIdentifierArr[5] := 1;
    ObjIdentifierArr[6] := 1;
    ObjIdentifier.Values := ObjIdentifierArr;
    FObjIdentifierSequence.Items.Add(TcnocASN1NullElement.Create);
  except
    FObjIdentifierSequence.Free;
    raise;
  end;

  FBitStr := TcnocASN1GenericElement.Create;
  try
    FBitStr.TagNr := 3;
    FBitStr.ASN1Class := cacUniversal;
    FBitStr.Encoding := caePrimitive;
    SetLength(B, 1);
    FBitStr.Content := B;
    KeySequence := TcnocASN1SequenceElement.Create;
    KeySequence.FixedLength := True;
    FBitStr.Items.Add(KeySequence);
    KeySequence.Items.Add(n.Clone);
    KeySequence.Items.Add(e.Clone);

    FBitStr.ContentLength := AnEncoder.GetElementLength(KeySequence, AFormat, True) + 1;
  except
    FBitStr.Free;
    raise;
  end;
end;

function TcnocRSAPublicKey.GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
  AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
  AFormat: TcnocASN1Format): Boolean;
begin
  CreateASN1Structures(AnEncoder, AFormat);
  AClass := cacUniversal;
  ATag := 16;
  AnEncoding := caeConstructed;
  ALength := AnEncoder.GetElementLength(FObjIdentifierSequence, AFormat, True) + AnEncoder.GetElementLength(FBitStr, AFormat, True);
  Result := True;
end;

procedure TcnocRSAPublicKey.StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format);
begin
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, FObjIdentifierSequence);
  AnEncoder.SaveObjectToStream(AContentStream, AFormat, FBitStr);
end;

function TcnocRSAPublicKey.GetPEMDescription: string;
begin
  Result := 'PUBLIC KEY';
end;

function TcnocRSAPublicKey.GetOpenSSLKey: PEVP_PKEY;
var
  MemStream: TMemoryStream;
  ptr: pointer;
  s: string;
begin
  MemStream := TMemoryStream.Create;
   try
     SaveToDERStream(MemStream);
     ptr := MemStream.Memory;
     Result := d2i_PubKey(nil, @ptr, MemStream.Size);
     if not assigned(Result) then
       begin
       setlength(s,200);
       ErrErrorString(ErrGetError, s, 200);
       raise Exception.CreateFmt('Failed to get key: %s', [s]);
       end;
   finally
     MemStream.Free;
   end;
end;

constructor TcnocRSAPublicKey.Create;
begin
  Fn := TcnocRSABignum.Create;
  Fe := TcnocRSABignum.Create;

  Fn.AsInt64 := -1;
  Fe.AsInt64 := -1;
end;

destructor TcnocRSAPublicKey.Destroy;
begin
  Fn.Free;
  Fe.Free;
  FObjIdentifierSequence.Free;
  FBitStr.Free;
  inherited Destroy;
end;

procedure TcnocRSAPublicKey.SaveToDERStream(AStream: TStream);
var
  ASN1Encoder: TcnocASN1BEREncoder;
begin
  ASN1Encoder := TcnocASN1BEREncoder.Create;
  try
    ASN1Encoder.SaveObjectToStream(AStream, cafDER, Self);
  finally
    ASN1Encoder.Free;
  end;
end;

procedure TcnocRSAPublicKey.SaveToPEMStream(AStream: TStream);
var
  Base64Stream: TBase64EncodingStream;
  LineWrapStream: TcnocLineWrapStream;
  s: String;
begin
  s := '-----BEGIN ' + GetPEMDescription + '-----' + sLineBreak;
  AStream.WriteBuffer(s[1], Length(s));

  LineWrapStream := TcnocLineWrapStream.Create(AStream);
  try
    Base64Stream := TBase64EncodingStream.Create(LineWrapStream);
    try
      SaveToDERStream(Base64Stream);
    finally
      Base64Stream.Free;
    end;
  finally
    LineWrapStream.Free;
  end;

  s := sLineBreak + '-----END ' + GetPEMDescription + '-----' + sLineBreak;
  AStream.WriteBuffer(s[1], Length(s));
end;

function TcnocRSAPublicKey.Verify(Content, Signature: TBytes): Boolean;
var
  mdctx: PEVP_MD_CTX;
  key: PEVP_PKEY;
begin
  Key := GetOpenSSLKey;
  mdctx := EVP_MD_CTX_create;
  try
    if not Assigned(mdctx) then
      raise Exception.Create('Failed to create Message Digest Context');

    if EVP_DigestVerifyInit(mdctx, Nil, EVP_sha256, Nil, key) <> 1 then
      raise Exception.Create('Failed to initialise the Digest-Verify operation');

    if EVP_DigestVerifyUpdate(mdctx, @Content[0], Length(Content)) <> 1 then
      raise Exception.Create('Failed to process the content');

    Result := EVP_DigestVerifyFinal(mdctx, @Signature[0], Length(Signature)) = 1;
  finally
    EVP_MD_CTX_destroy(mdctx);
  end;
end;

{ TcnocRSABignum }

function TcnocRSABignum.GetAsBase64Encoded: string;
var
  OutputStream: TStringStream;
  EncodingStream: TBase64EncodingStream;
  Data: TBytes;
begin
  OutputStream := TStringStream.Create('');
  try
    EncodingStream := TBase64EncodingStream.Create(OutputStream);
    try
      Data := AsBinaryUnsigned;
      EncodingStream.Write(Data[0], Length(Data));
    finally
      EncodingStream.Free;
    end;
    Result := OutputStream.DataString;
  finally
    OutputStream.Free;
  end;
end;

function TcnocRSABignum.GetAsBase64UrlEncoded: string;
begin
  Result := TBaseJWT.Base64ToBase64URL(AsBase64Encoded);
end;

function TcnocRSABignum.Clone: TcnocRSABignum;
begin
  Result := TcnocRSABignum.Create;
  case FUsedFormat of
    sfBCD: Result.AsBCD := AsBCD;
    sfInt: Result.AsInt64 := AsInt64;
    sfBytes: Result.AsBinarySigned := AsBinarySigned;
  end;
  Result.FEncoding := FEncoding;
  Result.FContentLength := FContentLength;
end;

procedure TcnocRSABignum.SetAsBase64Encoded(AValue: string);
var
  Instream  : TStringStream;
  Outstream : TBytesStream;
  Decoder   : TBase64DecodingStream;
  Data     : TBytes;
begin
  Instream:=TStringStream.Create(AValue);
  try
    Outstream:=TBytesStream.Create([]);
    try
      Decoder:=TBase64DecodingStream.Create(Instream,bdmStrict);
      try
         Outstream.CopyFrom(Decoder,Decoder.Size);
         Data:=Outstream.Bytes;
         SetLength(Data, Outstream.Size);
         AsBinaryUnsigned := Data;
      finally
        Decoder.Free;
      end;
    finally
     Outstream.Free;
     end;
  finally
    Instream.Free;
    end;
end;

procedure TcnocRSABignum.SetAsBase64UrlEncoded(AValue: string);
begin
  AsBase64Encoded := TBaseJWT.Base64URLToBase64(AValue);
end;

end.

