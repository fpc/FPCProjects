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

unit httputil;

{$mode objfpc}{$h+}

interface

uses
  sysutils, unixutil, strutils;

{$ifdef MSWINDOWS}
function TZSeconds: integer;
{$endif}
function GMTToLocalTime(ADateTime: TDateTime): TDateTime;
function LocalTimeToGMT(ADateTime: TDateTime): TDateTime;

function SeperatePath(var InPath: string; out ExtraPath: string): boolean;

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

{$endif}

function GMTToLocalTime(ADateTime: TDateTime): TDateTime;
begin
  Result := ADateTime + (TZSeconds*1000/MSecsPerDay);
end;

function LocalTimeToGMT(ADateTime: TDateTime): TDateTime;
begin
  Result := ADateTime - (TZSeconds*1000/MSecsPerDay);
end;

function SeperatePath(var InPath: string; out ExtraPath: string): boolean;
var
  lInfo: TSearchRec;
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
    if FindFirst(InPath, faAnyFile, lInfo) = 0 then
    begin
      Result := true;
      ExtraPath := Copy(lFullPath, Length(InPath)+1, Length(lFullPath)-Length(InPath));
    end;
    FindClose(lInfo);
    if Result then break;
    lPos := RPos('/', InPath);
    if lPos > 0 then
      SetLength(InPath, lPos-1)
    else
      break;
  until false;
end;

end.
