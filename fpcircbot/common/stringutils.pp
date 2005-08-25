{$mode objfpc}{$H+}
unit stringutils;

interface
uses SysUtils;

function RemoveOtherChars (const S : String; AllowedChars : TSysCharSet) : String; {$IFDEF USEINLINE} inline;{$ENDIF}
//Remove chars that does not exists on the charset

function SQLEscape (const S : string) : string;
//This function make a string good for database.

function FilterHtml(const s: string): string;
//This function convert chars that are used in html and convert them to char codes.

function return_Number_sanitize (const Number : String) : String;
//Remove any char that is not number

function return_Channel_sanitize (const ChannelName : String) : String;
//This function remove any illeagl chars for Channel name according the RFC.

function return_Nick_sanitize (const Nick : String) : String;
//This function remove any illeagl chars for nick name according the RFC

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
  Result := S;
	
	if (Length(Result) > 0) then
	  begin
      Result := StringReplace(Result, #39, #39#39, [rfReplaceAll]); {Place '' on every '.}
    	{If we are using unicode, then we should add it here as well}
			Result := StringReplace (Result, '\', '\\', [rfReplaceAll]); {Escape every \ sign -> place it as the first escaping case.}
			Result := StringReplace (Result, ':', '\:', [rfReplaceAll]); {Escape every : sign}
			Result := StringReplace (Result, '%', '\%', [rfReplaceAll]); {Escape every % sign}
			Result := StringReplace (Result, '"', '\"', [rfReplaceAll]); {Escape every " sign}
	  end;
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
    Result := StringReplace (Result, '&', '&amp;', [rfReplaceAll]); // Keep this the first or the rest of the changes will scrow up :)
    Result := StringReplace (Result, '<', '&lt;', [rfReplaceAll]);
    Result := StringReplace (Result, '>', '&gt;', [rfReplaceAll]);
    Result := StringReplace (Result, '"', '&quot;', [rfReplaceAll]);
{    Result := StringReplace (Result, #34, '&#34;', [rfReplaceAll]); // This is the same char as quote}
    Result := StringReplace (Result, #39, '&#39;', [rfReplaceAll]);
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

function return_datetimestamp_sanitize (const DateTimeStamp : String) : String;
begin
  Result := DateTimeStamp;
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
  Result:='';
  Writeln('Doing diff between: ', DateTimeToStr(DT1), ' | ', DateTimeToStr(DT2));
  DiffTime  := Abs(DateTimeToTimeStamp(DT1).Time - DateTimeToTimeStamp(DT2).Time);

  Day       := Abs(DateTimeToTimeStamp(DT1).Date - DateTimeToTimeStamp(DT2).Date);

  {
    imagine this diff: DT1 = (01, 01, 0001 23:59) DT2 = (02, 01, 0001 00:01)
    The result would be DTResult = (1 day 23 hours and 58 minutes)
    So what we need to get 2 minute here is (and the rule should work generaly):
    if day is decreasable by 1 do so, and if so then subtract time from 24 hours.
  }
  if Day > 0 then begin
    Dec(Day);
    DiffTime := Abs((24 * 1000 * 60 * 60) - DiffTime);
  end;

  Year      := Day div 356; // let's assume noone is gone long enough for leap year to make effect
  Hour      := ((DiffTime div 1000) div 60) div 60;
  Minute    := ((DiffTime div 1000) div 60) mod 60;

  if Year > 0 then Result:=Result + IntToStr(Year) + ' year' + MultiString(Year) + ' ';
  if Day > 0 then Result:=Result + IntToStr(Day) + ' day' + MultiString(Day) + ' ';;
  if Hour > 0 then Result:=Result + IntToStr(Hour) + ' hour' + MultiString(Hour) + ' ';
  if Minute > 0 then Result:=Result + IntToStr(Minute) + ' minute' + MultiString(Minute) + ' ';
end;

end.

