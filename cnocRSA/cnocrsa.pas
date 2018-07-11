unit cnocRSA;

{$mode objfpc}{$H+}

interface

uses
  sysutils,
  classes,
  ctypes,
  base64,
  fpjwt,
  cnocASN1,
  cnocASN1Encoder,
  cnocASN1TagFactory,
  cnocASN1Decoder,
  cnocASN1Element,
  openssl,
  FmtBCD;

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
    function Gete: TcnocRSABignum;
    function Getn: TcnocRSABignum;
  protected
    function GetASN1HeaderInfo(
      out AClass: TcnocASN1Class; out ATag: Integer; out AnEncoding: TcnocASN1Encoding; out ALength: QWord;
      AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format): Boolean; virtual;
    procedure StreamASN1Content(
      AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder; AFormat: TcnocASN1Format); virtual;
    function GetPEMDescription: string; virtual;
    function GetOpenSSLKey: PEVP_PKEY; virtual;
    procedure RaiseOpenSSLError(Msg: string);
  public
    destructor Destroy; override;

    procedure SaveToDERStream(AStream: TStream); virtual;
    procedure SaveToPEMStream(AStream: TStream); virtual;

    procedure LoadFromDERStream(AStream: TStream); virtual;
    procedure Assign(ASource: TcnocRSAPublicKey); virtual;

    function Verify(Content, Signature: TBytes): Boolean;

    property n    : TcnocRSABignum read Getn;       // public modulus
    property e    : TcnocRSABignum read Gete;       // public exponent
  end;

  { TcnocRSAPrivateKey }

  TcnocRSAPrivateKey = class(TcnocRSAPublicKey, IcnocASN1DecodableClass)
  private
    FVer: TcnocASN1IntegerElement;
    Fd: TcnocRSABignum;
    Fdmp1: TcnocRSABignum;
    Fdmq1: TcnocRSABignum;
    Fiqmp: TcnocRSABignum;
    Fp: TcnocRSABignum;
    Fq: TcnocRSABignum;

    function Getd: TcnocRSABignum;
    function Getdmp1: TcnocRSABignum;
    function Getdmq1: TcnocRSABignum;
    function Getiqmp: TcnocRSABignum;
    function Getp: TcnocRSABignum;
    function Getq: TcnocRSABignum;
    function GetVer: TcnocASN1IntegerElement;
  protected
    function GetASN1HeaderInfo(out AClass: TcnocASN1Class; out ATag: Integer; out
      AnEncoding: TcnocASN1Encoding; out ALength: QWord; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format): Boolean; override;
    procedure StreamASN1Content(AContentStream: TStream; AnEncoder: TcnocASN1BEREncoder;
      AFormat: TcnocASN1Format); override;
    procedure ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer; AnEncoding: TcnocASN1Encoding;
      ALength: Integer; ADecoder: TcnocASN1BERDecoder; AContentStream: TStream;
      AFormat: TcnocASN1Format);
    function GetPEMDescription: string; override;
    function GetOpenSSLKey: PEVP_PKEY; override;
  public
    destructor Destroy; override;

    procedure Assign(ASource: TcnocRSAPublicKey); override;

    function Sign(Content: TBytes): TBytes;
    procedure Generate(Bits: cint; Exp: PtrInt);

    property Ver  : TcnocASN1IntegerElement read GetVer;     // Version
    property d    : TcnocRSABignum read Getd;                // private exponent
    property p    : TcnocRSABignum read Getp;                // secret prime factor
    property q    : TcnocRSABignum read Getq;                // secret prime factor
    property dmp1 : TcnocRSABignum read Getdmp1;             // d mod (p-1)
    property dmq1 : TcnocRSABignum read Getdmq1;             // d mod (q-1)
    property iqmp : TcnocRSABignum read Getiqmp;             // q^-1 mod p
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

function TcnocRSAPrivateKey.Getd: TcnocRSABignum;
begin
  if not Assigned(Fd) then
    begin
    Fd := TcnocRSABignum.Create;
    Fd.AsInt64 := -1;
    end;
  Result := Fd;
end;

function TcnocRSAPrivateKey.Getdmp1: TcnocRSABignum;
begin
  if not Assigned(Fdmp1) then
    begin
    Fdmp1 := TcnocRSABignum.Create;
    Fdmp1.AsInt64 := -1;
    end;
  Result := Fdmp1;
end;

function TcnocRSAPrivateKey.Getdmq1: TcnocRSABignum;
begin
  if not Assigned(Fdmq1) then
    begin
    Fdmq1 := TcnocRSABignum.Create;
    Fdmq1.AsInt64 := -1;
    end;
  Result := Fdmq1;
end;

function TcnocRSAPrivateKey.Getiqmp: TcnocRSABignum;
begin
  if not Assigned(Fiqmp) then
    begin
    Fiqmp := TcnocRSABignum.Create;
    Fiqmp.AsInt64 := -1;
    end;
  Result := Fiqmp;
end;

function TcnocRSAPrivateKey.Getp: TcnocRSABignum;
begin
  if not Assigned(Fp) then
    begin
    Fp := TcnocRSABignum.Create;
    Fp.AsInt64 := -1;
    end;
  Result := Fp;
end;

function TcnocRSAPrivateKey.Getq: TcnocRSABignum;
begin
  if not Assigned(Fq) then
    begin
    Fq := TcnocRSABignum.Create;
    Fq.AsInt64 := -1;
    end;
  Result := Fq;
end;

function TcnocRSAPrivateKey.GetVer: TcnocASN1IntegerElement;
begin
  if not Assigned(FVer) then
    FVer := TcnocASN1IntegerElement.Create;
  Result := FVer;
end;

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
  Result := True;
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

procedure TcnocRSAPrivateKey.ParseASN1Content(AClass: TcnocASN1Class; ATag: Integer;
  AnEncoding: TcnocASN1Encoding; ALength: Integer; ADecoder: TcnocASN1BERDecoder;
  AContentStream: TStream; AFormat: TcnocASN1Format);
var
  Items: TcnocASN1ElementList;
begin
  Items := TcnocASN1ElementList.Create(True);
  try
    ADecoder.LoadFromStream(AContentStream, AFormat, ALength, Items);
    Ver.Assign(Items[0] as TcnocASN1CustomElement);
    n.Assign(Items[1] as TcnocASN1CustomElement);
    e.Assign(Items[2] as TcnocASN1CustomElement);
    d.Assign(Items[3] as TcnocASN1CustomElement);
    p.Assign(Items[4] as TcnocASN1CustomElement);
    q.Assign(Items[5] as TcnocASN1CustomElement);
    dmp1.Assign(Items[6] as TcnocASN1CustomElement);
    dmq1.Assign(Items[7] as TcnocASN1CustomElement);
    iqmp.Assign(Items[8] as TcnocASN1CustomElement);
  finally
    Items.Free;
  end;
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
       RaiseOpenSSLError('Invalid key:');
   finally
     MemStream.Free;
   end;
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

procedure TcnocRSAPrivateKey.Assign(ASource: TcnocRSAPublicKey);
var
  PKSource: TcnocRSAPrivateKey;
begin
  if ASource is TcnocRSAPrivateKey then
    begin
    PKSource := TcnocRSAPrivateKey(ASource);
    inherited Assign(PKSource);
    Ver.Assign(PKSource.Ver);
    d.Assign(PKSource.d);
    p.Assign(PKSource.p);
    q.Assign(PKSource.q);
    dmp1.Assign(PKSource.dmp1);
    dmq1.Assign(PKSource.dmq1);
    iqmp.Assign(PKSource.iqmp);
    end
  else
    raise Exception.Create('Can not assign a public key to a private key.');
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

function TcnocRSAPublicKey.Gete: TcnocRSABignum;
begin
  if not Assigned(Fe) then
    begin
    Fe := TcnocRSABignum.Create;
    Fe.AsInt64 := -1;
    end;
  Result := Fe;
end;

function TcnocRSAPublicKey.Getn: TcnocRSABignum;
begin
  if not Assigned(Fn) then
    begin
    Fn := TcnocRSABignum.Create;
    Fn.AsInt64 := -1;
    end;
  Result := Fn;
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

procedure TcnocRSAPublicKey.RaiseOpenSSLError(Msg: string);
var
  ErrBuf: string;
begin
  SetLength(ErrBuf, 200);
  ErrErrorString(ErrGetError, ErrBuf, Length(ErrBuf));
  raise Exception.Create(Msg + ' ' + ErrBuf);
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
       RaiseOpenSSLError('Failed to get key:');
   finally
     MemStream.Free;
   end;
end;

destructor TcnocRSAPublicKey.Destroy;
begin
  Fn.Free;
  Fe.Free;
  FObjIdentifierSequence.Free;
  FBitStr.Free;
  inherited Destroy;
end;

procedure TcnocRSAPrivateKey.Generate(Bits: cint; Exp: PtrInt);
var
  Rsa: PRSA;
  i: Integer;
  RsaDER: TBytes;
  ExpBigNum: PBIGNUM;
  pt: Pointer;
  DERStream: TBytesStream;
begin
  if RAND_status <> 1 then
    raise Exception.Create('OpenSSL did not obtain enough random data.');
  Rsa := RSA_new;
  try
    if not Assigned(Rsa) then
      RaiseOpenSSLError('OpenSSL');

    ExpBigNum := BN_new;
    try
      if not Assigned(ExpBigNum) then
        RaiseOpenSSLError('OpenSSL');

      if BN_set_word(ExpBigNum, Exp) <> 1 then
        RaiseOpenSSLError('OpenSSL');

      if cint(RSA_generate_key_ex(Rsa, Bits, ExpBigNum, nil)) <> 1 then
        RaiseOpenSSLError('OpenSSL');
    finally
      BN_free(ExpBigNum);
    end;

    // Store in DER-format
    pt := nil;
    i := i2d_RSAPrivateKey(Rsa, @pbyte(pt));
    if i < 1 then
      RaiseOpenSSLError('OpenSSL');
    SetLength(RsaDER, i);

    pt := @RsaDER[0];
    if i2d_RSAPrivateKey(Rsa, @pbyte(pt)) < i then
      RaiseOpenSSLError('OpenSSL');

    // Fill this class with the data from the stored DER-data
    DERStream := TBytesStream.Create(RsaDER);
    try
      LoadFromDERStream(DERStream);
    finally
      DERStream.Free;
    end;
  finally
    RSA_free(Rsa);
  end;
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

procedure TcnocRSAPublicKey.LoadFromDERStream(AStream: TStream);
var
  ASN1Decoder: TcnocASN1BERDecoder;
  Key: TcnocRSAPublicKey;
begin
  ASN1Decoder := TcnocASN1BERDecoder.Create;
  try
    ASN1Decoder.TagFactory := TcnocASN1TagFactory.Create;
    try
      ASN1Decoder.TagFactory.RegisterASN1Type(cacUniversal, 16, ClassType);
      ASN1Decoder.TagFactory.RegisterASN1Type(TcnocASN1IntegerElement.GetClass, TcnocASN1IntegerElement.GetTagNr, TcnocASN1IntegerElement);
      Key := ASN1Decoder.LoadElementFromStream(AStream, cafDER) as TcnocRSAPublicKey;
      try
        Assign(Key);
      finally
        Key.Free;
      end;
    finally
      ASN1Decoder.TagFactory.Free;
    end;
  finally
    ASN1Decoder.Free;
  end;
end;

procedure TcnocRSAPublicKey.Assign(ASource: TcnocRSAPublicKey);
begin
  n.Assign(ASource.n);
  e.Assign(ASource.e);
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

