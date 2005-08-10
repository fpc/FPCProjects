{$mode objfpc}{$H+}
unit stringutils;

interface

function QuoteString (const S : string) : string;
//This function make a string good for database.

function FilterHtml(const s: string): string;
//This function convert chars that are used in html and convert them to char codes.

implementation
uses SysUtils;

function QuoteString (const S : string) : string;
begin
  Result := S;
	
	if (Length(Result) > 0) then
    Result := StringReplace(Result, #39, #39#39, [rfReplaceAll]); {Place '' on every '.}
	{If we are using unicode, then we should add it here as well}
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
}
    Result:=StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
    Result:=StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
    Result:=StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
    Result:=StringReplace(Result, #34, '&#34;', [rfReplaceAll]);
    Result:=StringReplace(Result, #39, '&#39;', [rfReplaceAll]);
  end;
end;


end.

