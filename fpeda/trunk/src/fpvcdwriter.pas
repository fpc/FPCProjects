{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2014 by Mazen NEIFER of the Free Pascal development team.

    VCD (Value Change Dump) file writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fpVcdWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, Variants;

type
  TSignalValue = Variant;
  TSignalId = '!'..'~';
  TSignalDef = record
    Name: string;
    Width: Byte;
    Value: TSignalValue
  end;

  { TVcdWriter }

  TVcdWriter = class(TObject)
  private
    FStream: TStream;
    FFreeStreamOnClose: Boolean;
    FModLevel: Integer;
    function WriteLn(const s: string): Boolean;
    function WriteLn(SignalId: TSignalId):Boolean;
    function WriteLn(SignalIdx: Byte): Boolean;
  protected
    FTime: UInt64;
    FSignals: array of TSignalDef;
    function SignalId2Idx(Id: TSignalId): Integer;
    function SignalIdx2Id(Idx: Byte): TSignalId;
    function SetTime(Time: UInt64): Boolean;
    function FlushHeader: Boolean;
  public
    Date: string;
    Comment: string;
    TimeScale: string;
    destructor Destroy; override;
    function BeginModuleDef(aName: string): Boolean;
    function EndModuleDef: Boolean;
    function AddSignal(const AName: string; AWidth: Integer; AValue: TSignalValue): Char;
    function SetSignal(Id: TSignalId; AValue: TSignalValue; Time: Uint64): Boolean;
    function CloseFile: Boolean;
    function StartDumping: Boolean;
    function StoreToFile(const FileName: string): Boolean;
    function StoreToStream(AStream: TStream): Boolean;
  end;

implementation

uses
  SysUtils;

{ TWaveWriter }

function TVcdWriter.WriteLn(const s: string): Boolean;
begin
  Result := FStream.Write(s[1], Length(s)) = Length(s);
  Result := Result and (FStream.Write(string(LineEnding)[1], Length(LineEnding)) = Length(LineEnding));
end;

function TVcdWriter.WriteLn(SignalId: TSignalId): Boolean;
begin
  Result := WriteLn(SignalId2Idx(SignalId));
end;

function TVcdWriter.WriteLn(SignalIdx: Byte): Boolean;
var
  V: string;
begin
  with FSignals[SignalIdx] do begin
    try
       V := binStr(UInt64(Value), Width);
    except
       V := Value;
    end;
    if Width > 1 then begin
      V := 'b' + V + ' ';
    end;
  end;
 Result := WriteLn(Format('%s%s', [V, SignalIdx2Id(SignalIdx)]))
end;

function TVcdWriter.SignalId2Idx(Id: TSignalId): Integer;
begin
  Result := Ord(Id) - Ord(Low(Id));
end;

function TVcdWriter.SignalIdx2Id(Idx: Byte): TSignalId;
begin
  Result := Chr(Ord(Low(Result)) + Idx);
end;

destructor TVcdWriter.Destroy;
begin
  CLoseFile;
  inherited Destroy;
end;

function TVcdWriter.BeginModuleDef(aName: string): Boolean;
var
  Tabs: string;
begin
  Tabs := StringOfChar(#9, FModLevel);
  WriteLn(Tabs + '$scope module ' + aName + ' $end');
  FModLevel += 1;
  Result := True;
end;

function TVcdWriter.EndModuleDef: Boolean;
var
  Tabs: string;
begin
  Result := FModLevel > 0;
  if Result then begin
    FModLevel -= 1;
    Tabs := StringOfChar(#9, FModLevel);
    WriteLn(Tabs + '$upscope $end');
  end;
end;

function TVcdWriter.AddSignal(const AName: string; AWidth: Integer; AValue: TSignalValue): Char;
var
  Idx: Integer;
begin
  if FModLevel <= 0 then begin
    Exit(#0);
  end;
  Idx := Length(FSignals);
  SetLength(FSignals, Idx + 1);
  Result := SignalIdx2Id(Idx);
  with FSignals[Idx] do begin
    Name := AName;
    Width := AWidth;
    Value := AValue;
    WriteLn(Format('$var wire %d %s %s $end', [Width, Result, Name]));
  end;
end;

function TVcdWriter.SetTime(Time: UInt64): Boolean;
begin
  Result := Time >= FTime;
  if Time > FTime then begin
    WriteLn(Format('#%d', [FTime]));
    FTime := Time;
  end;
end;

function TVcdWriter.FlushHeader: Boolean;
var
  s: string;
begin
  if Date = '' then begin
    DateTimeToString(s, 'dddd yyyy-mm-dd HH:MM:SS', Now);
  end else begin
    s := Date
  end;
  WriteLn('$date ' + s + ' $end');
  WriteLn('$version ' + 'fpVcdWriter 0.1' + ' $end');
  WriteLn('$date ' + Comment + ' $end');
  if TimeScale = ''then begin
    s := '1ps';
  end else begin
    s := TimeScale;
  end;
  Result := WriteLn('$timescale ' + s + ' $end');
end;

function TVcdWriter.SetSignal(Id: TSignalId; AValue: TSignalValue; Time: Uint64): Boolean;
var
  Idx: Integer;
begin
  Idx := SignalId2Idx(Id);
  Result := Idx < Length(FSignals);
  if Result then with FSignals[Idx] do begin
    if(VarType(AValue) <> VarType(Value)) or (AValue <> Value) then begin
      SetTime(Time);
      Value := AValue;
      WriteLn(Idx);
    end;
  end;
end;

function TVcdWriter.CloseFile: Boolean;
begin
  Result := True;
  if not Assigned(fStream) then begin
    Exit(True);
  end;
  if FFreeStreamOnClose then begin
    fStream.Free;
  end;
end;

function TVcdWriter.StartDumping: Boolean;
var
  SignalIdx: Integer;
begin
  WriteLn('$enddefinitions $end');
  WriteLn('$dumpvars');
  for SignalIdx := Low(FSignals) to High(FSignals) do begin
    WriteLn(SignalIdx);
  end;
  WriteLn('$end');
  Result := True;
end;

function TVcdWriter.StoreToFile(const FileName: string):Boolean;
begin
  CLoseFile;
  fStream := TFileStream.Create(FileName, fmCreate + fmOpenWrite + fmShareDenyWrite);
  if Assigned(fStream) then begin
    Result := StoreToStream(fStream);
    FFreeStreamOnClose := True;
  end else begin
    Result := False;
  end;
end;

function TVcdWriter.StoreToStream(AStream:TStream):Boolean;
begin
  Result := Assigned(AStream);
  if Result then begin
    fStream := AStream;
    FFreeStreamOnClose := False;
    Result := FlushHeader;
  end;
end;

end.

