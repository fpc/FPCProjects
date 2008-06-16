unit StringUtils;

{$mode objfpc}{$H+}

{$i ../include/baseinc.inc}

interface

uses
  SysUtils {$ifndef noDB} ,
{$IfDef LinkDynamically}
  postgres3dyn;
{$Else}
  postgres3;
{$EndIf}

{$else}
;
{$endif}

function RemoveOtherChars (const S : String; AllowedChars : TSysCharSet) : String; {$IFDEF USEINLINE} inline;{$ENDIF}
//Remove chars that does not exists on the charset

function SQLEscape (const S : string) : string;
//This function make a string good for database.

function FilterHtml(const s: string): string;
//This function convert chars that are used in html and convert them to char codes.

function FilterHtmlWeak(const s: string): string;
//This function convert chars that are used in html and convert them to char codes.

function RestoreHtmlWeak(const s: string): string;
//This function reverts cleaning

function return_Number_sanitize (const Number : String) : String;
//Remove any char that is not number

function return_Channel_sanitize (const ChannelName : String) : String;
//This function remove any illeagl chars for Channel name according the RFC.

function return_Nick_sanitize (const Nick : String) : String;
//This function remove any illeagl chars for nick name according the RFC

function return_Decimal_sanitize (const Decimal: string): string;
//This functions remove any non-decimal chars

function return_datetimestamp_sanitize (const DateTimeStamp : String) : String;
//This function remove any illeagl chars for DateTimeStamp

function SQLStrToDateTimeStr(const SQLStr: string): string;
//This function converts datetime string returned by SQL(timestamp.asstring) into a TDateTime compatible string

function CreateDiffStr(const DT1, DT2: TDateTime): string;
//This function creates a string diff for TDateTime diff with 'years months' etc. It only fills parts which are not 0.

implementation

function RemoveOtherChars (const S : String; AllowedChars : TSysCharSet) : String; {$IFDEF USEINLINE} inline;{$ENDIF}
var
 i  : integer;
 
begin
  Result := S;
	if Length (Result) <= 0 then
	  exit;
{ todo: move this code to regex }
  for i := 0 to 255 do //run on the full ascii table .. 
	 begin
	   if not (chr(i) in AllowedChars) then //Does the char exists on the list ?
		   Result := StringReplace (Result, chr(i), '', [rfReplaceAll]); //nop, lets remove it :)
	 end;
end;

function SQLEscape (const S : string) : string;
begin
  {$ifndef noDB}
  if Length(S) > 0 then begin
    SetLength(Result, Length(S) * 2 + 1);
    SetLength(Result, PQescapeString(pChar(Result), pChar(S), Length(S)));
  end;
  {$else}
  Result := s;
  {$endif}
end;

function FilterHtml(const s: string): string;
begin
  Result:=s;
  if Length(Result) > 0 then begin
{
< = &lt;
> = &gt;
" = &quot;
" = &#34;
' = &#39;
\x8b = &#8249;
\x9b = &#8250;
\012 = &#10;
\015 = &#13;
& = &amp;
}
    Result := StringReplace (Result, '&',      '&amp;',   [rfReplaceAll]); // Keep this the first or the rest of the changes will scrow up :)
    Result := StringReplace (Result, '<',      '&lt;',    [rfReplaceAll]);
    Result := StringReplace (Result, '>',      '&gt;',    [rfReplaceAll]);
    Result := StringReplace (Result, '"',      '&quot;',  [rfReplaceAll]);
    Result := StringReplace (Result, #39,      '&#39;',   [rfReplaceAll]);
    //Result := StringReplace (Result, #$8b,     '&#8249;', [rfReplaceAll]); <- Extended ASCII code always different on each code page !
    //Result := StringReplace (Result, #$9b,     '&#8250;', [rfReplaceAll]); <- Extended ASCII code always different on each code page !
    Result := StringReplace (Result, chr(&12), '&#10;',   [rfReplaceAll]);
    Result := StringReplace (Result, chr(&15), '&#13;',   [rfReplaceAll]);
  end;
end;

function FilterHtmlWeak(const s: string): string;
begin
  Result:=s;
  if Length(Result) > 0 then begin
{
< = &lt;
> = &gt;
" = &quot;
" = &#34;
' = &#39;
\x8b = &#8249;
\x9b = &#8250;
\012 = &#10;
\015 = &#13;
& = &amp;
}
    Result := StringReplace (Result, '&',      '&amp;',   [rfReplaceAll]); // Keep this the first or the rest of the changes will scrow up :)
    Result := StringReplace (Result, '<',      '&lt;',    [rfReplaceAll]);
    Result := StringReplace (Result, '>',      '&gt;',    [rfReplaceAll]);
    Result := StringReplace (Result, '"',      '&quot;',  [rfReplaceAll]);
    Result := StringReplace (Result, #39,      '&#39;',   [rfReplaceAll]);
    //Result := StringReplace (Result, #$8b,     '&#8249;', [rfReplaceAll]); <- Extended ASCII code always different on each code page !
    //Result := StringReplace (Result, #$9b,     '&#8250;', [rfReplaceAll]); <- Extended ASCII code always different on each code page !
{    Result := StringReplace (Result, chr(&12), '&#10;',   [rfReplaceAll]);
    Result := StringReplace (Result, chr(&15), '&#13;',   [rfReplaceAll]);}
  end;
end;

function RestoreHtmlWeak(const s: string): string;
begin
  Result:=s;
  if Length(Result) > 0 then begin
    Result := StringReplace (Result, '&amp;',      '&',   [rfReplaceAll]); // Keep this the first or the rest of the changes will scrow up :)
    Result := StringReplace (Result, '&lt;',      '<',    [rfReplaceAll]);
    Result := StringReplace (Result, '&gt;',      '>',    [rfReplaceAll]);
    Result := StringReplace (Result, '&quot;',      '"',  [rfReplaceAll]);
    Result := StringReplace (Result, '&#39;',      #39,   [rfReplaceAll]);
    Result := StringReplace (Result, '&#8249;',     #$8b, [rfReplaceAll]);
    Result := StringReplace (Result, '&#8250;',     #$9b, [rfReplaceAll]);
  end;
end;

function return_Channel_sanitize (const ChannelName : String) : String;
begin
 Result := ChannelName;
 if (Length (Result) > 0) then
   begin
		 Result := StringReplace (Result, #0, '', [rfReplaceAll]);  //Remove Null terminator chars
                 Result := StringReplace (Result, #32, '', [rfReplaceAll]); //Remove spaces.
                 Result := StringReplace (Result, #7, '', [rfReplaceAll]);  //Remove BELL
		 Result := StringReplace (Result, #13, '', [rfReplaceAll]); //Remove CR
		 Result := StringReplace (Result, #10, '',  [rfReplaceAll]); //Remove LF
		 Result := StringReplace (Result, ',', '', [rfReplaceAll]); //Remove Comma
	 end;
end;


function return_Number_sanitize (const Number : String) : String;
begin
  Result := Number;
	if Length (Result) > 0 then
	  Result := RemoveOtherChars (Result, ['0'..'9']);
	
end;

function return_Nick_sanitize (const Nick : String) : String;
begin
  Result := Nick;
	if Length (Result) > 0 then
	  Result := RemoveOtherChars (Result, ['0'..'9', 'a'..'z', 'A'..'Z', '-', '[', ']', '\', '`', '^', '{', '}']);

end;

function return_Decimal_sanitize (const Decimal: string): string;
var
  i: Integer;
begin
  i:=1;
  Result:=Decimal;
  if Length(Result) > 0 then
    while i <= Length(Result) do begin
      if not (Result[i] in ['0'..'9']) then
        Delete(Result, i, 1)
      else
        Inc(i);
    end;
  if Length(Result) = 0 then
    Result:='0';
end;

function return_datetimestamp_sanitize (const DateTimeStamp : String) : String;
begin
  Result:=DateTimeStamp;
	if Length (Result) > 0 then
	  Result := RemoveOtherChars (Result, ['0'..'9', '-', #32, ':']);
end;

function SQLStrToDateTimeStr(const SQLStr: string): string;
var
  s: string;
begin
  Result:='';
  s:=SQLStr;
  System.Delete(s, Length(s) - 5, 5);
  Result:=Copy(s, 9, 2) + '-' + Copy(s, 6, 2) + '-' + Copy(s, 3, 2) + Copy(s, 11, Length(s));
  System.Delete(Result, Length(Result) - 2, 3);
end;

function CreateDiffStr(const DT1, DT2: TDateTime): string;

  function MultiString(const x: Word): string;
  begin
    if x > 1 then Result:='s' else Result:='';
  end;

var
  Year, Day, Hour, Minute: Integer;

  DiffTIme: Integer;
begin
  Result := '';
  DiffTime  := Abs(DateTimeToTimeStamp(DT1).Time - DateTimeToTimeStamp(DT2).Time);

  Day       := Abs(DateTimeToTimeStamp(DT1).Date - DateTimeToTimeStamp(DT2).Date);

  {
    imagine this diff: DT1 = (01, 01, 0001 23:59) DT2 = (02, 01, 0001 00:01)
    The result would be DTResult = (1 day 23 hours and 58 minutes)
    So what we need to get 2 minute here is (and the rule should work generaly):
    if day diff is 1 and first time is greater than second time, dec day and
    subtract time from 24 hours.
  }
  if (Day > 0) and (DateTimeToTimeStamp(DT1).Time > DateTimeToTimeStamp(DT2).Time) then begin
    Dec(Day);
    DiffTime := Abs((24 * 1000 * 60 * 60) - DiffTime);
  end;

  Year      := Day div 365; // let's assume noone is gone long enough for leap year to make effect
  Day       := Day mod 365;
  Hour      := ((DiffTime div 1000) div 60) div 60;
  Minute    := ((DiffTime div 1000) div 60) mod 60;

  if Year > 0 then Result:=Result + IntToStr(Year) + ' year' + MultiString(Year) + ' ';
  if Day > 0 then Result:=Result + IntToStr(Day) + ' day' + MultiString(Day) + ' ';
  if Hour > 0 then Result:=Result + IntToStr(Hour) + ' hour' + MultiString(Hour) + ' ';
  if Minute > 0 then Result:=Result + IntToStr(Minute) + ' minute' + MultiString(Minute) + ' ';
  if Length(Result) = 0 then Result:='Less than a minute ';
end;

end.

