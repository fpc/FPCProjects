{ Routines to convert pascal file/string code to html highlighted file/string
  Released under NRCOL License (public domain).

}

unit PasHiliter;  {$ifdef fpc}  {$mode objfpc} {$h+} {$endif}

interface
uses
  ChrStream, PasTokenize, compactsysutils, multitype, pcharutils, tokentypes;

type
  PByteArray = ^TByteArray;
  TByteArray = array[0..maxint -1] of byte;
  astr = ansistring;

procedure RedirectStdOut(Outputfile: astr);
procedure RestoreStdOut;
procedure WriteHTMLHeader; overload;
procedure WriteHTMLFooter; overload;
procedure WriteHTMLHeader(var fh: text); overload;
procedure WriteHTMLFooter(var fh: text); overload;


procedure PasFileToStdOutHTM(const filename: astr);
function PasFileToHtmStr(const filename: astr): astr;
function PasStrToHtmStr(const input: astr): astr;


var
  MainWrite: procedure(s: astr);
  MainWriteLn: procedure(s: astr);

implementation

{$ifdef win32}
const
  CRLF = #13#10;
{$endif}

{$ifdef unix}
const
  CRLF = #10;
{$endif}

type
  TWriterProc = procedure(input: astr; var multi: TMultiType);


procedure RedirectStdOut(Outputfile: astr);
begin
  AssignFile(output, OutputFile);
  rewrite(output);
end;

procedure RestoreStdOut;
begin
  CloseFile(output);
  Assign(output,'');
  rewrite(output);
end;

var
  ChrStrm: PChrStrm;
  PasParser: PPasParser;

procedure EscapeHTML(var s: astr);
begin
  if pos('<', s) > 0 then
  begin
    s:= stringreplace(s, '<', '&lt;', [rfReplaceAll]);
  end;

  if pos('>', s) > 0 then
  begin
    s:= stringreplace(s, '>', '&gt;', [rfReplaceAll]);
  end;
end;

procedure WriteHTMLHeader;
begin
  MainWrite('<html><head>'+
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">'+
        '<title></title>' +
        '<link rel="stylesheet" type="text/css" href="style.css">'+ '</head>'+
        '<body>');
end;

procedure WriteHTMLHeader(var fh: text);
begin
  Writeln(fh, '<html><head>'+
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">'+
        '<title></title>' +
        '<link rel="stylesheet" type="text/css" href="style.css">'+ '</head>'+
        '<body>');
end;

procedure WriteHTMLFooter;
begin
  mainwrite('</body></html>');
end;

procedure WriteHTMLFooter(var fh: text);
begin
  writeln(fh, '</body></html>');
end;


{ Turns pascal into highlighted HTML using CSS classes. Does not write full
  html page, i.e. html header, body and footer, or PRE tag. Do that separately }
procedure PasToHtm(AddText: TWriterProc; var result: TMultiType);
var
  s: astr;
  token: TPasToken;
  i: integer;
begin
//  result:= nil;
  s:= '';
  repeat
    PasParser^.GetToken(token, s, PasParser, ChrStrm);

    if (s = #10) then
      addtext(#10, result);

    if (s = #13) then
      addtext(#13, result);

    if (s = ' ') then
      addtext(' ', result);

    if token = ptKeyword then
      addtext(('<span class="Keyword">' + s + '</span>'), result);

    if token = ptInvalidToken then
      addtext(('<span class="InvalidToken">' + s + '</span>'), result);

    if token = ptIdentifier then
      addtext(('<span class="Identifier">' + s + '</span>'), result);

    if token = ptString then
    begin
      EscapeHTML(s);
      addtext(('<span class="String">' + s + '</span>'), result);
    end;

    if token = ptHexNumber then
      addtext(('<span class="HexNumber">' + s + '</span>'), result);

    if token = ptNumber then
      addtext(('<span class="Number">' + s + '</span>'), result);

    if token = ptComment then
    begin
      EscapeHTML(s);
      addtext(('<span class="Comment">' + s + '</span>'), result);
    end;

    if token = ptDirective then
      addtext(('<span class="Directive">' + s + '</span>'), result);

    if token = ptRange then
      addtext(('<span class="Range">' + s + '</span>'), result);

    case token of
      ptComma, ptSemicolon, ptColon, ptPeriod, ptEquals, ptPlus, ptMinus,
      ptMultiply, ptNotEquals, ptLess, ptLessEqual, ptGreater, ptGreaterEqual,
      ptAssign, ptOpenParen, ptCloseParen, ptOpenBracket, ptCloseBracket,
      ptDivide:
      begin
        addtext(('<span class="Symbol">' + s + '</span>'), result);
      end;
    end;

    if token = ptCaret then
      addtext(('<span class="Other">' + s + '</span>'), result);

    if token = ptHash then
      addtext(('<span class="Other">' + s + '</span>'), result);

    if token = ptAddress then
      addtext(('<span class="Address">' + s + '</span>'), result);

    if (token =  ptWhitespace) and (s = ' ') then
      addtext(' ', result);

    if token = ptEndOfFile then
    begin
      addtext('<!------------------------------------------------------>' + CRLF, result);
      addtext('<!-- End of File --------------------------------------->' + CRLF, result);
      addtext('<!------------------------------------------------------>' + CRLF, result);
    end;
    if token = ptInvalidToken then addtext('<!------invalid token found----------------------------->' + CRLF, result);
  until (token = ptEndOfFile) or (token = ptInvalidToken); // done


end;

var tempstr: astr;
{ usage: InputFile: the file you want to parse, which contains pascal code }
procedure PasFileToHtm(InputFile: astr; Addtext: TWriterProc; var rslt: TMultiType);
begin
  ChrStrm:= NewChrFileStrm1(inputfile);
  PasParser:= NewPasParser(ChrStrm);
  PasToHtm(AddText, rslt);
  FreePasParser(PasParser);
  FreeChrFileStrm(ChrStrm);
end;

{ takes input string containing pascal code, converts to html  }
procedure PasStrToHtm(Input: astr; Addtext: TWriterProc; var rslt: TMultiType);
begin
  // create char stream 
  ChrStrm:= NewChrStrStrm(input);
  // create pascal parser 
  PasParser:= NewPasParser(ChrStrm);
  // absract pastohtm function accepts our rslt string as input.. RSLT is like a TVarRec
  PasToHtm(AddText, rslt);
  // free parser and char stream
  FreePasParser(PasParser);
  FreeChrStrStrm(ChrStrm);
end;

{-- WRITER METHODS ------------------------------------------------------------}

{ append a string to another string (var param allows repetitive concats to the 
  same string over and over again }
procedure StringWriter(s: astr; var rslt: tmultitype);
begin
  rslt.aString:= rslt.aString + s; //concat string
  // TODO: this could be optimized.. this concat is slow when dealing with large data
end;

{ write to stdout }
procedure StdOutWriter(s: astr; var rslt: tmultitype);
begin
  system.write(s); // write msg to stdout
end;

{ write to an existing file }
procedure FileHandleWriter(s: astr; var rslt: tmultitype);
begin
  system.write(rslt.atextfile, s); // write msg to open file
end;

{------------------------------------------------------------------------------}


{-- HTML CONVERTER METHODS ----------------------------------------------------}
{ Takes a pascal input file, outputs a highlighted pascal snippet in html to
  STDOUT }
procedure PasFileToStdOutHTM(const filename: astr);
var
  dummy: TMultiType; // nil parameter
begin
  PasFileToHtm(filename, {$ifdef fpc}@{$endif}StdOutWriter, dummy);
end;

{ takes a pascal input file, converts to highlighted pascal string in html }
function PasFileToHtmStr(const filename: astr): astr;
var
  tmp: tmultitype;
begin
  result:= '';
  tmp.aString:= result;  tmp.mtype:= mtString;
  PasFileToHtm(filename, {$ifdef fpc}@{$endif}StringWriter,  tmp);
  result:= tmp.astring; 
end;

{ takes a pascal input string, converts to a highlighted html string }
function PasStrToHtmStr(const input: astr): astr;
var
  tmp: tmultitype;
begin
  result:= '';
  tmp.aString:= result;  tmp.mtype:= mtString;
  PasStrToHtm(input, {$ifdef fpc}@{$endif}StringWriter, tmp);
  result:= tmp.aString;
end;

{ TO DO
function PasStrToHtmFile(const input: string; outfile: string);
begin
end

function PasFileHtmFile(const FNameIn: string; FNameOut: string);
begin
end

}
{------------------------------------------------------------------------------}




end.

