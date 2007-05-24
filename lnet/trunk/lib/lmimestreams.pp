unit lMimeStreams;

{$mode objfpc}{$H+}

interface

uses
  Classes;
  
type
  TStreamNotificationEvent = procedure(const aSize: Integer) of object;

  { TMimeOutputStream }

  TMimeOutputStream = class(TStream)
   protected
    FInputData: string;
    FLastCRLF: Integer;
    FNotificationEvent: TStreamNotificationEvent;
    function GetSize: Int64; override;
    procedure AddInputData(const s: string);
   public
    constructor Create(aNotificationEvent: TStreamNotificationEvent);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;

  { TBogusStream }

  TBogusStream = class(TStream)
   protected
    FData: string;
    function GetSize: Int64; override;
   public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
  end;
  
implementation

uses
  Math;

type
  TByteArray = array of Byte;

{ TMimeOutputStream }

function TMimeOutputStream.GetSize: Int64;
begin
  Result := Length(FInputData);
end;

procedure TMimeOutputStream.AddInputData(const s: string);

{  function RightPos(const What, Where: string): Integer;
  var
    i, j: Integer;
  begin
    Result := 0;

    j := Length(What);
    for i := Length(Where) downto 1 do
      if Where[i] = What[j] then begin
        Dec(j);
        if j = 0 then Exit(i);
      end else
        j := Length(What);
  end;

var
  n: Integer;}
begin
{  n := RightPos(#13#10, s);
  if n > 0 then
    Inc(FLastCRLF, (Length(FInputData) - FLastCRLF) + n);}

  FInputData := FInputData + s;

{  while Length(FInputData) - FLastCRLF >= 74 do begin
    Insert(#13#10, FInputData, FLastCRLF + 75);
    Inc(FLastCRLF, 77);
  end;}
end;

constructor TMimeOutputStream.Create(aNotificationEvent: TStreamNotificationEvent);
begin
  inherited Create;
  
  FNotificationEvent := aNotificationEvent;
  FLastCRLF := 1;
end;

function TMimeOutputStream.Read(var Buffer; Count: Longint): Longint;
var
  i: Integer;
begin
  if Assigned(FNotificationEvent) then
    FNotificationEvent(Count);

  Result := Min(Count, Length(FInputData));
  
  Move(FInputData[1], Buffer, Result);
  Delete(FInputData, 1, Result);

  Dec(FLastCRLF, Result);
end;

function TMimeOutputStream.Write(const Buffer; Count: Longint): Longint;
var
  s: string;
begin
  SetLength(s, Count);
  Move(Buffer, s[1], Count);
  AddInputData(s);
  Result := Count;
end;

function TMimeOutputStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := Offset;
end;

function TMimeOutputStream.Seek(const Offset: Int64; Origin: TSeekOrigin
  ): Int64;
begin
  Result := Offset;
end;

{ TBogusStream }

function TBogusStream.GetSize: Int64;
begin
  Result := Length(FData);
end;

function TBogusStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Min(Count, Length(FData));
  
  Move(FData[1], Buffer, Result);
  Delete(FData, 1, Result);
end;

function TBogusStream.Write(const Buffer; Count: Longint): Longint;
var
  l: Integer;
begin
  l := Length(FData);
  Result := Count;
  SetLength(FData, l + Count);
  Inc(l);
  Move(Buffer, FData[l], Count);
end;

function TBogusStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := Offset;
end;

function TBogusStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := Offset;
end;

end.

