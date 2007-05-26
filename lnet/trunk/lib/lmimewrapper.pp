unit lMimeWrapper;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Contnrs, lMimeStreams;
  
const
  MIME_VERSION = 'MIME-version: 1.0' + CRLF;

type
  TMimeEncoding = (me8bit, meBase64);
  TMimeDisposition = (mdInline, mdAttachment);

  { TMimeSection }

  TMimeSection = class
   protected
    FContentType: string;
    FEncoding: TMimeEncoding;
    FActivated: Boolean;
    FDescription: string;
    FDisposition: TMimeDisposition;
    FBuffer: string;
    FEncodingStream: TStream;
    FOutputStream: TStream;
    FLocalStream: TBogusStream;
    function GetSize: Integer; virtual; abstract;
    procedure SetDescription(const AValue: string);
    procedure SetDisposition(const AValue: TMimeDisposition);
    procedure SetEncoding(const AValue: TMimeEncoding);
    procedure CreateEncodingStream; virtual;
    function GetHeader: string; virtual;
    function ReadBuffer(const aSize: Integer): string;
    procedure FillBuffer(const aSize: Integer); virtual; abstract;
   public
    constructor Create(aOutputStream: TStream);
    destructor Destroy; override;
    function Read(const aSize: Integer): Integer;
    procedure Reset; virtual;
   public
    property ContentType: string read FContentType write FContentType;
    property Encoding: TMimeEncoding read FEncoding write SetEncoding;
    property Disposition: TMimeDisposition read FDisposition write SetDisposition;
    property Description: string read FDescription write SetDescription;
    property Header: string read GetHeader;
    property Size: Integer read GetSize;
  end;
  
  { TMimeTextSection }

  TMimeTextSection = class(TMimeSection)
   protected
    FOriginalData: string;
    FData: string;
    function GetSize: Integer; override;
    procedure SetData(const AValue: string);
    function GetCharset: string;
    procedure SetCharset(const AValue: string);
    procedure FillBuffer(const aSize: Integer); override;
   public
    constructor Create(aOutputStream: TStream; const aText: string);
    procedure Reset; override;
   public
    property Charset: string read GetCharset write SetCharset;
    property Text: string read FData write SetData;
  end;
  
  { TMimeStreamSection }

  TMimeStreamSection = class(TMimeSection)
   protected
    FStream: TStream;
    FOwnsStreams: Boolean;
    FOriginalPosition: Int64;
    function GetSize: Integer; override;
    procedure SetStream(aValue: TStream);
    procedure FillBuffer(const aSize: Integer); override;
   public
    constructor Create(aOutputStream: TStream; aStream: TStream);
    destructor Destroy; override;
    procedure Reset; override;
   public
    property Stream: TStream read FStream write SetStream;
    property OwnsStreams: Boolean read FOwnsStreams write FOwnsStreams;
  end;
  
  { TMimeFileSection }

  TMimeFileSection = class(TMimeStreamSection)
   protected
    FFileName: string;
    procedure SetFileName(const AValue: string);
    procedure SetContentType(const aFileName: string);
    function GetHeader: string; override;
   public
    constructor Create(aOutputStream: TStream; const aFileName: string);
    property FileName: string read FFileName write SetFileName;
  end;

  { TMimeStream }

  TMimeStream = class(TStream)
   protected
    FSections: TFPObjectList;
    FOutputStream: TMimeOutputStream;
    FBoundary: string;
    FActiveSection: Integer;
    FCalledRead: Boolean;
    FCalledWrite: Boolean;
    function GetSize: Int64; override;
    function GetCount: Integer;
    function GetBoundary: string;
    function GetSections(i: Integer): TMimeSection;
    procedure SetSections(i: Integer; const AValue: TMimeSection);
    procedure ActivateFirstSection;
    procedure ActivateNextSection;
    procedure DoRead(const aSize: Integer);
   public
    constructor Create;
    destructor Destroy; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure AddTextSection(const aText: string; const aCharSet: string = 'UTF-8');
    procedure AddFileSection(const aFileName: string);
    procedure AddStreamSection(aStream: TStream; const FreeStream: Boolean = False);
    procedure Delete(const i: Integer);
    procedure Remove(aSection: TMimeSection);
    procedure Reset;
   public
    property Sections[i: Integer]: TMimeSection read GetSections write SetSections; default;
    property Count: Integer read GetCount;
    property Boundary: string read FBoundary;
  end;
  
  { EAlreadyActivatedException }

  EAlreadyActivatedException = class(Exception)
   public
    constructor Create;
  end;
  
  { EAlreadyCalledReadException }

  EAlreadyCalledReadException = class(Exception)
   public
    constructor Create;
  end;

  { EAlreadyCalledWriteException }

  EAlreadyCalledWriteException = class(Exception)
   public
    constructor Create;
  end;
  
implementation

uses
  Base64;
  
function EncodingToStr(const Encoding: TMimeEncoding): string;
begin
  Result := '';
  case Encoding of
    me8bit   : Result := '8bit';
    meBase64 : Result := 'base64';
  end;
end;

function DispositionToStr(const Disposition: TMimeDisposition): string;
begin
  Result := '';
  case Disposition of
    mdInline     : Result := 'inline';
    mdAttachment : Result := 'attachment';
  end;
end;

{ TMimeSection }

procedure TMimeSection.SetDescription(const AValue: string);
begin
  if not FActivated then
    FDescription := AValue;
end;

procedure TMimeSection.SetDisposition(const AValue: TMimeDisposition);
begin
  if not FActivated then
    FDisposition := AValue;
end;

procedure TMimeSection.SetEncoding(const AValue: TMimeEncoding);
begin
  if not FActivated then begin
    FEncoding := aValue;
    if Assigned(FEncodingStream) then
      FEncodingStream.Free;
    
    CreateEncodingStream;
  end;
end;

procedure TMimeSection.CreateEncodingStream;
begin
  case FEncoding of
    me8bit   : FEncodingStream := nil;
    meBase64 : FEncodingStream := TBase64EncodingStream.Create(FLocalStream);
  end;
end;

function TMimeSection.GetHeader: string;
begin
  Result := 'Content-Type: ' + FContentType + CRLF;
  Result := Result + 'Content-Transfer-Encoding: ' + EncodingToStr(FEncoding) + CRLF;
  Result := Result + 'Content-Disposition: ' + DispositionToStr(FDisposition) + CRLF;

  if Length(FDescription) > 0 then
    Result := Result + 'Content-Description: ' + FDescription + CRLF;
    
  Result := Result + CRLF;
end;

function TMimeSection.ReadBuffer(const aSize: Integer): string;
begin
  Result := '';

  if aSize >= Length(FBuffer) then
    FillBuffer(aSize);
    
  Result := Copy(FBuffer, 1, aSize);
end;

constructor TMimeSection.Create(aOutputStream: TStream);
begin
  FOutputStream := aOutputStream;
  FEncodingStream := nil;
  FLocalStream := TBogusStream.Create;
end;

destructor TMimeSection.Destroy;
begin
  if Assigned(FEncodingStream) then
    FEncodingStream.Free;
    
  FLocalStream.Free;

  inherited Destroy;
end;

function TMimeSection.Read(const aSize: Integer): Integer;
var
  s: string;
begin
  Result := 0;

  if aSize <= 0 then
    Exit;

  if not FActivated then begin
    FActivated := True;
    FBuffer := GetHeader;
  end;
  
  if Length(FBuffer) < aSize then
    FillBuffer(aSize);
    
  s := ReadBuffer(aSize);
  if Length(s) >= aSize then begin
    Result := FOutputStream.Write(s[1], aSize);
    Delete(FBuffer, 1, Result);
  end else begin
    Result := FOutputStream.Write(s[1], Length(s));
    Delete(FBuffer, 1, Result);
  end;
end;

procedure TMimeSection.Reset;
begin
  FActivated := False;
  FBuffer := '';
  FLocalStream.Reset;
  SetEncoding(FEncoding);
end;

{ TMimeTextSection }

procedure TMimeTextSection.SetCharset(const aValue: string);
begin
  if not FActivated then begin
    if Length(aValue) > 0 then
      FContentType := 'text/plain; charset="' + aValue + '"'
    else
      FContentType := 'text/plain';
  end;
end;

procedure TMimeTextSection.FillBuffer(const aSize: Integer);
var
  s: string;
  n: Integer;
begin
  s := Copy(FData, 1, aSize);
  
  if Length(s) <= 0 then
    Exit;
    
  n := aSize;

  if Assigned(FEncodingStream) then begin
    n := FEncodingStream.Write(s[1], Length(s));
    Delete(FData, 1, n);

    if Length(FData) = 0 then begin
      FEncodingStream.Free; // to fill in the last bit
      CreateEncodingStream;
      FLocalStream.Write(CRLF[1], Length(CRLF));
    end;
    
    SetLength(s, FLocalStream.Size);
    SetLength(s, FLocalStream.Read(s[1], Length(s)));
  end else begin
    Delete(FData, 1, n);
    if Length(FData) = 0 then
      s := s + CRLF;
  end;

  FBuffer := FBuffer + s;
end;

function TMimeTextSection.GetSize: Integer;
begin
  if FActivated then
    Result := Length(FBuffer) + Length(FData)
  else
    Result := Length(FBuffer) + Length(GetHeader) + Length(FData);
end;

procedure TMimeTextSection.SetData(const AValue: string);
begin
  if not FActivated then begin
    FOriginalData := aValue;
    FData := aValue;
  end;
end;

function TMimeTextSection.GetCharset: string;
var
  n: Integer;
begin
  Result := '';

  n := Pos('=', FContentType);
  if n > 0 then
    Result := StringReplace(Copy(FContentType, n + 1, Length(FContentType)),
                            '"', '', [rfReplaceAll]);
end;

constructor TMimeTextSection.Create(aOutputStream: TStream; const aText: string);
begin
  inherited Create(aOutputStream);

  FContentType := 'text/plain; charset="UTF-8"';
  FOriginalData := aText;
  FData := FOriginalData;
end;

procedure TMimeTextSection.Reset;
begin
  inherited Reset;
  FData := FOriginalData;
end;

{ TMimeStreamSection }

function TMimeStreamSection.GetSize: Integer;
begin
  if FActivated then
    Result := Length(FBuffer) + FStream.Size - FStream.Position
  else
    Result := Length(FBuffer) + Length(GetHeader) + FStream.Size - FStream.Position;
end;

procedure TMimeStreamSection.SetStream(aValue: TStream);
begin
  if Assigned(FStream)
  and FOwnsStreams then begin
    FStream.Free;
    FStream := nil;
  end;
  
  FStream := aValue;
  FOriginalPosition := FStream.Position;
end;

procedure TMimeStreamSection.FillBuffer(const aSize: Integer);
var
  s: string;
  n: Integer;
begin
  SetLength(s, aSize);
  SetLength(s, FStream.Read(s[1], aSize));
  
  if Length(s) <= 0 then
    Exit;
  
  if Assigned(FEncodingStream) then begin
    n := FEncodingStream.Write(s[1], Length(s));
    
    if n < Length(s) then
      FStream.Position := FStream.Position - (n - Length(s));
      
    if FStream.Size - FStream.Position = 0 then begin
      FEncodingStream.Free; // to fill in the last bit
      CreateEncodingStream;
      FLocalStream.Write(CRLF[1], Length(CRLF));
    end;
      
    SetLength(s, FLocalStream.Size);
    SetLength(s, FLocalStream.Read(s[1], FLocalStream.Size));
  end else if FStream.Size - FStream.Position = 0 then
    s := s + CRLF;

  FBuffer := FBuffer + s;
end;

constructor TMimeStreamSection.Create(aOutputStream: TStream; aStream: TStream);
begin
  inherited Create(aOutputStream);
  
  FDisposition := mdAttachment;
  FStream := aStream;
  FOriginalPosition := FStream.Position;
  FContentType := 'application/octet-stream';
end;

destructor TMimeStreamSection.Destroy;
begin
  if FOwnsStreams then
    FStream.Free;

  inherited Destroy;
end;

procedure TMimeStreamSection.Reset;
begin
  inherited Reset;
  FStream.Position := FOriginalPosition;
end;

{ TMimeStream }

function TMimeStream.GetSize: Int64;
var
  i: Integer;
begin
  Result := 0;
  
  if FActiveSection > -2 then
    for i := 0 to Count - 1 do
      Result := Result + TMimeSection(FSections[i]).Size;
    
  Result := Result + FOutputStream.Size;
end;

function TMimeStream.GetCount: Integer;
begin
  Result := FSections.Count;
end;

function TMimeStream.GetBoundary: string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to 25 + Random(15) do
    Result := Result + Char(Random(Ord('9') - Ord('0') + 1) + Ord('0'));
end;

function TMimeStream.GetSections(i: Integer): TMimeSection;
begin
  Result := nil;
  
  if  (i >= 0)
  and (i < FSections.Count) then
    Result := TMimeSection(FSections[i]);
end;

procedure TMimeStream.SetSections(i: Integer; const AValue: TMimeSection);
begin
  if  (i >= 0)
  and (i < FSections.Count) then
    FSections[i] := aValue;
end;

procedure TMimeStream.ActivateFirstSection;
const
  MIME_HEADER = 'Content-type: multipart/mixed; boundary="';
var
  s: string;
begin
  if FActiveSection = -1 then
    if FSections.Count > 0 then begin
      FOutputStream.Write(MIME_VERSION[1], Length(MIME_VERSION));
      FActiveSection := 0;
      
      if FSections.Count > 1 then begin
        s := MIME_HEADER + FBoundary + '"' + CRLF + CRLF +
             'This is a multi-part message in MIME format.' + CRLF +
             '--' + FBoundary + CRLF;
             
        FOutputStream.Write(s[1], Length(s));
      end;
    end;
end;

procedure TMimeStream.ActivateNextSection;
var
  s: string;
begin
  Inc(FActiveSection);
  
  if FSections.Count > 1 then begin
    if FActiveSection >= FSections.Count then
      s := '--' + FBoundary + '--' + CRLF
    else
      s := '--' + FBoundary + CRLF;
      
    FOutputStream.Write(s[1], Length(s));
  end;

  if FActiveSection >= FSections.Count then
    FActiveSection := -2;
end;

procedure TMimeStream.DoRead(const aSize: Integer);
var
  n: Integer;
begin
  ActivateFirstSection;
  
  if FActiveSection < 0 then
    Exit;
    
  TMimeSection(FSections[FActiveSection]).Read(aSize);
  
  if TMimeSection(FSections[FActiveSection]).Size = 0 then
    ActivateNextSection;
end;

constructor TMimeStream.Create;
begin
  Randomize;
  
  FActiveSection := -1;
  FBoundary := GetBoundary;
  FSections := TFPObjectList.Create(True);
  FOutputStream := TMimeOutputStream.Create(@DoRead);
end;

destructor TMimeStream.Destroy;
begin
  FSections.Free;
  FOutputStream.Free;

  inherited Destroy;
end;

function TMimeStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := Offset;
end;

function TMimeStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := Offset;
end;

function TMimeStream.Read(var Buffer; Count: Longint): Longint;
begin
  if FCalledWrite then
    raise EAlreadyCalledWriteException.Create;

  FCalledRead := True;
  Result := FOutputStream.Read(Buffer, Count);
end;

function TMimeStream.Write(const Buffer; Count: Longint): Longint;
begin
  if FCalledRead then
    raise EAlreadyCalledReadException.Create;

  Result := 0;
  FCalledWrite := True;
  raise Exception.Create('Not yet implemented');
end;

procedure TMimeStream.AddTextSection(const aText: string; const aCharSet: string = 'UTF-8');
var
  s: TMimeTextSection;
begin
  if FActiveSection >= 0 then
    raise EAlreadyActivatedException.Create;
    
  s := TMimeTextSection.Create(FOutputStream, aText);
  
  s.Charset := aCharSet;
  FSections.Add(s);
end;

procedure TMimeStream.AddFileSection(const aFileName: string);
begin
  if FActiveSection >= 0 then
    raise EAlreadyActivatedException.Create;

  FSections.Add(TMimeFileSection.Create(FOutputStream, aFileName));
end;

procedure TMimeStream.AddStreamSection(aStream: TStream; const FreeStream: Boolean
  );
var
  s: TMimeStreamSection;
begin
  if FActiveSection >= 0 then
    raise EAlreadyActivatedException.Create;

  s := TMimeStreamSection.Create(FOutputStream, aStream);
  if FreeStream then
    s.OwnsStreams := True;
  FSections.Add(s);
end;

procedure TMimeStream.Delete(const i: Integer);
begin
  if (i >= 0) and (i < Count) then
    FSections.Delete(i);
end;

procedure TMimeStream.Remove(aSection: TMimeSection);
begin
  FSections.Remove(aSection);
end;

procedure TMimeStream.Reset;
var
  i: Integer;
begin
  FCalledRead := False;
  FCalledWrite := False;
  
  for i := 0 to FSections.Count - 1 do
    TMimeSection(FSections[i]).Reset;
    
  FOutputStream.Reset;
  FActiveSection := -1;
end;

{ TMimeFileSection }

procedure TMimeFileSection.SetFileName(const AValue: string);
begin
  if not FActivated then begin
    FFileName := aValue;
    Stream := TFileStream.Create(aValue, fmOpenRead);
    SetContentType(aValue);
  end;
end;

procedure TMimeFileSection.SetContentType(const aFileName: string);
var
  s: string;
begin
  s := StringReplace(ExtractFileExt(aFileName), '.', '', [rfReplaceAll]);

  if (s = 'txt')
  or (s = 'pas')
  or (s = 'pp')
  or (s = 'pl')
  or (s = 'cpp')
  or (s = 'cc')
  or (s = 'h')
  or (s = 'c++') then FContentType := 'text/plain';
  
  if s = 'html' then FContentType := 'text/html';
  if s = 'css' then FContentType := 'text/css';
  
  if s = 'png' then FContentType := 'image/x-png';
  if s = 'xpm' then FContentType := 'image/x-pixmap';
  if s = 'xbm' then FContentType := 'image/x-bitmap';
  if (s = 'tif')
  or (s = 'tiff') then FContentType := 'image/tiff';
  if s = 'mng' then FContentType := 'image/x-mng';
  if s = 'gif' then FContentType := 'image/gif';
  if s = 'rgb' then FContentType := 'image/rgb';
  if (s = 'jpg')
  or (s = 'jpeg') then FContentType := 'image/jpeg';
  if s = 'bmp' then FContentType := 'image/x-ms-bmp';
    
  if s = 'wav' then FContentType := 'audio/x-wav';
  if s = 'mp3' then FContentType := 'audio/x-mp3';
  if s = 'ogg' then FContentType := 'audio/x-ogg';
  if s = 'avi' then FContentType := 'video/x-msvideo';
  if (s = 'qt')
  or (s = 'mov') then FContentType := 'video/quicktime';
  if (s = 'mpg')
  or (s = 'mpeg') then FContentType := 'video/mpeg';
  
  if s = 'pdf' then FContentType := 'application/pdf';
  if s = 'rtf' then FContentType := 'application/rtf';
  if s = 'tex' then FContentType := 'application/x-tex';
  if s = 'latex' then FContentType := 'application/x-latex';
  if s = 'doc' then FContentType := 'application/msword';
  if s = 'gz' then FContentType := 'application/x-gzip';
  if s = 'zip' then FContentType := 'application/zip';
  if s = '7z' then FContentType := 'application/x-7zip';
  if s = 'rar' then FContentType := 'application/rar';
  if s = 'tar' then FContentType := 'application/x-tar';
  if s = 'arj' then FContentType := 'application/arj';
end;

function TMimeFileSection.GetHeader: string;
begin
  Result := 'Content-Type: ' + FContentType + CRLF;
  Result := Result + 'Content-Transfer-Encoding: ' + EncodingToStr(FEncoding) + CRLF;
  Result := Result + 'Content-Disposition: ' + DispositionToStr(FDisposition) +
            '; filename="' + FFileName + '"' + CRLF;

  if Length(FDescription) > 0 then
    Result := Result + 'Content-Description: ' + FDescription + CRLF;
    
  Result := Result + CRLF;
end;

constructor TMimeFileSection.Create(aOutputStream: TStream; const aFileName: string);
begin
  inherited Create(aOutputStream, TFileStream.Create(aFileName, fmOpenRead));

  SetContentType(aFileName);
  FDescription := ExtractFileName(aFileName);
  Encoding := meBase64;
  FFileName := ExtractFileName(aFileName);
  FOwnsStreams := True;
end;

{ EAlreadyActivatedException }

constructor EAlreadyActivatedException.Create;
begin
  inherited Create('The stream or section has already been activated (by Read() or Write())');
end;

{ EAlreadyCalledReadException }

constructor EAlreadyCalledReadException.Create;
begin
  inherited Create('The stream has already been used for reading');
end;

{ EAlreadyCalledWriteException }

constructor EAlreadyCalledWriteException.Create;
begin
  inherited Create('The stream has already been used for writing');
end;

end.

