{ Utility routines for HTTP server component

  Copyright (C) 2006 Micha Nelissen

  This library is Free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
  
  This license has been modified. See file LICENSE.ADDON for more information.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit lHTTPUtil;

{$mode objfpc}{$h+}

interface

uses
  sysutils, 
{$ifdef UNIX}
  unixutil,
{$endif}
  strutils;

const
  HTTPDateFormat: string = 'ddd, dd mmm yyyy hh:nn:ss';

function TZSeconds: integer;
function GMTToLocalTime(ADateTime: TDateTime): TDateTime;
function LocalTimeToGMT(ADateTime: TDateTime): TDateTime;
function TryHTTPDateStrToDateTime(ADateStr: pchar; var ADest: TDateTime): boolean;

function SeparatePath(var InPath: string; out ExtraPath: string): boolean;

implementation

{$ifdef MSWINDOWS}

uses
  windows;

function TZSeconds: integer;
var
  lInfo: Windows.TIME_ZONE_INFORMATION;
begin
  { lInfo.Bias is in minutes }
  if Windows.GetTimeZoneInformation(@lInfo) <> $FFFFFFFF then
    Result := lInfo.Bias * 60
  else
    Result := 0;
end;

{$else}

function TZSeconds: integer; inline;
begin
  Result := unixutil.TZSeconds;
end;

{$endif}

function GMTToLocalTime(ADateTime: TDateTime): TDateTime;
begin
  Result := ADateTime + (TZSeconds*1000/MSecsPerDay);
end;

function LocalTimeToGMT(ADateTime: TDateTime): TDateTime;
begin
  Result := ADateTime - (TZSeconds*1000/MSecsPerDay);
end;

function TryHTTPDateStrToDateTime(ADateStr: pchar; var ADest: TDateTime): boolean;
var
  lYear, lMonth, lDay: word;
  lTime: array[0..2] of word;
  I, lCode: integer;
begin
  if StrLen(ADateStr) < Length(HTTPDateFormat)+4 then exit(false);
  { skip redundant short day string }
  Inc(ADateStr, 5);
  { day }
  if ADateStr[2] = ' ' then
    ADateStr[2] := #0
  else 
    exit(false);
  Val(ADateStr, lDay, lCode);
  if lCode <> 0 then exit(false);
  Inc(ADateStr, 3);
  { month }
  lMonth := 1;
  repeat
    if CompareMem(ADateStr, @ShortMonthNames[lMonth][1], 3) then break;
    inc(lMonth);
    if lMonth = 13 then exit(false);
  until false;
  Inc(ADateStr, 4);
  { year }
  if ADateStr[4] = ' ' then
    ADateStr[4] := #0
  else
    exit(false);
  Val(ADateStr, lYear, lCode);
  if lCode <> 0 then exit(false);
  Inc(ADateStr, 5);
  { hour, minute, second }
  for I := 0 to 2 do
  begin
    ADateStr[2] := #0;
    Val(ADateStr, lTime[I], lCode);
    Inc(ADateStr, 3);
    if lCode <> 0 then exit(false);
  end;
  ADest := EncodeDate(lYear, lMonth, lDay) + EncodeTime(lTime[0], lTime[1], lTime[2], 0);
  Result := true;
end;

function SeparatePath(var InPath: string; out ExtraPath: string): boolean;
var
  lFullPath: string;
  lPos: integer;
begin
  ExtraPath := '';
  if Length(InPath) <= 2 then exit(false);
  lFullPath := InPath;
  if InPath[Length(InPath)] = '/' then
    SetLength(InPath, Length(InPath)-1);
  Result := false;
  repeat
    if FileExists(InPath) then
    begin
      ExtraPath := Copy(lFullPath, Length(InPath)+1, Length(lFullPath)-Length(InPath));
      exit(true);
    end;
    lPos := RPos('/', InPath);
    if lPos > 0 then
      SetLength(InPath, lPos-1)
    else
      break;
  until false;
end;

end.
