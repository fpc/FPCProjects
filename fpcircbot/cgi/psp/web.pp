{$H+}{$MODE OBJFPC}
unit web;
{
 *******************************************************************************
 *                          -== Pascal Server Pages ==-                        *
 *******************************************************************************
 *                                 Main Web Unit                               *
 *******************************************************************************
 * See the Pascal Server Pages Documentation for more information.             *
 *******************************************************************************
 *   Written by Vladimir Sibirov a.k.a. Trustmaster                            *
 *   http: //www.psp.furtopia.org                                              *
 *   mailto: psp@furtopia.org                                                  *
 *******************************************************************************
 * Copyright (c) 2003-2004 by Pascal Server Pages development team.            *
 * See the Pascal Server Pages License for more information.                   *
 *******************************************************************************
 }
{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface
uses httpenc, substrings, sds, fileshare, env;

{==============================================================================}
{================================ Constants ===================================}
{==============================================================================}

const PSP_INI_PATH = 'psp.ini';
// Sets path to PSP Configuration file.

    ID_CHARS = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_[]';
// Available characters in CGI vars identifiers

    PW_CHARS = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_';
// Characters for password generation

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

// Contains a pair:  variable and value
type datarec = record
   name, value: string;
   end;

// Type to store all recieved data	
cgiData = array of datarec;

// Pointer to cgiData
PcgiData = ^cgiData;

// Type of uploaded file
upfile = record
    name, filename, value, content_type: string;
    size: longint;
end;

// Type to store all uploaded files			 
fileData = array of upfile;

// Pointer to fileData
PfileData = ^fileData;

// Pointer to string. For very long strings.
PString = ^string;

// Line type for Multipart/Form-Data handling functions
MP_Line = array[1..6] of string;

// Multipart/Form-Data form type
MP_Form = array of string;

// Pointer to MP_Form
MP_PForm = ^MP_Form;

{==============================================================================}
{================================ Variables ===================================}
{==============================================================================}	

var cgi: cgiData;
// Constructing this variable by defaults to store incoming data in it
    upf: fileData;
// Constructing this variable by defaults to store uploaded files in it

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{------------------------------------------------------------------------------}
{============================== Web_Config Unit ===============================}
{------------------------------------------------------------------------------}

function Web_GetConfig(fileName, confName: string): string;
// Reads value of config "name" from configuration file

{------------------------------------------------------------------------------}
{================================= Web Unit ===================================}
{------------------------------------------------------------------------------}

procedure Web_GetData;
// Recieves GET,  POST and COOKIE data and stores it in cgi. Recieves Uploaded files and stores them in upf.

procedure Web_Header;
// Outputs default HTTP header (text/html)

procedure Web_Header(addHeader: string);	
// Outputs custom HTTP header (text/html)

function Web_VarExists(name: string): boolean;
// Checks if the variable is recieved

function Web_GetVar(name: string): string;
// Gets value of recieved variable

procedure Web_SetVar(name, value: string);
// Puts new pair name=value to CGI variables or rewrites existing variable

function Web_UnsetVar(name: string): boolean;
// Removes the variable if it is set

function Web_FormatStr(str: string): string;
// Returns formatted version of str including CGI variables

procedure Web_Out(str: string);
// Formatted string output

procedure Web_OutLn(str: string);
// Formatted string output. Ends with end-of-line.

procedure Web_FileOut(fileName: string);
// Outputs a text file

procedure Web_TemplateOut(fileName: string);
// Formatted file output

procedure Web_ResourceOut(fileName: string);
// Outputs resource file
{------------------------------------------------------------------------------}
{=========================== Environment Variables ============================}
{------------------------------------------------------------------------------}

function Web_GetEnv(name: string): string;
// Returns value of environment variable

{------------------------------------------------------------------------------}
{=============================== Cookie Functions =============================}
{------------------------------------------------------------------------------}

procedure Web_SetCookie(name, value: string);
// Sets a cookie

procedure Web_SetCookie(name, value, path, domain, expires: string);
// Sets a cookie with custom parameters

procedure Web_DeleteCookie(name: string);
// Destroys cookie which was set by usual Web_SetCookie

procedure Web_DeleteCookie(name, path, domain: string);
// Destroys cookie which was set by Web_SetCookie with custom parameters.
// parameters must be the same

{------------------------------------------------------------------------------}
{============================== Web_Filters Unit ==============================}
{------------------------------------------------------------------------------}

function Web_RandPass(len: integer): string;
// Generates a random string of alphanumeric and '_' characters with special
// length

function Web_DisableTags(svar: string): string;
// Replaces all special charecters with their HTML analogs

function Web_Nl2Br(svar: string): string;
// Replaces all "end-of-line" symbols with <br> tags


function Web_XORCrypt(str: string; key: byte): string;
// Generates crypted string using XOR algoritm
// The great hint of XOR:  use ecrypted value as str and the same key
//and you'll get the original value!


function Web_XORHash(str, key: string): string;
// Calculates hash using simple XOR crypting algoritm

{------------------------------------------------------------------------------}
{============================= Session Functions ==============================}
{------------------------------------------------------------------------------}

function Web_SessionStart: boolean;
// Recieves session data and puts it into CGI

function Web_SessionRegister(name, value: string): boolean;
// Adds new pair "varable=value" to the session

function Web_SessionUnregister(name: string): boolean;
// Deletes pair "variable=value" from the session

function Web_SessionDestroy: boolean;
// Destroys user's session

{------------------------------------------------------------------------------}
{================================= File Uploads ===============================}
{------------------------------------------------------------------------------}

function Web_IsUploaded(varName: string): boolean;
// Checks if a file for this file input field is uploaded

function Web_UploadedFileSize(varName: string): longint;
// Returns size of uploaded file

function Web_UploadedFileType(varName: string): string;
// Returns content type of uploaded file

function Web_UploadedFileName(varName: string): string;
// Returns original name of uploaded file

procedure Web_SaveUploadedFile(varName, fullPath: string);
// Saves uploaded file on disk

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{------------------------------------------------------------------------------}
{============================ Web_Config Unit =================================}
{*******************************************************************************
 * Supports easy configuration file format. Use if your program needs          *
 *configuration files.                                                         *
 *******************************************************************************
}

{--[ Web_GetConfig ]-----------------------------------------------------------}
// Reads value of config "name" from configuration file

function Web_GetConfig(fileName, confName: string): string;
var fh: text;
    buff: string;
    conf: array[1..2] of string;
    c: char;
    len, i, cnt: integer;
    spaces: boolean;
begin
    assign(fh, fileName);
    reset(fh);
    while not eof(fh) do
        begin
            readln(fh, buff);
            len := length(buff);
            conf[1] := '';
            conf[2] := '';
            spaces := false;
            cnt := 1;
            for i := 1 to len do
                begin
                    c := buff[i];
                    if (c<>'=') and (c<>'#') and (c<>' ') and (c<>'"') then conf[cnt] := conf[cnt] + c;
                    if c='=' then cnt := 2;
                    if (c='"') then
                        begin
                            if spaces then spaces := false
                            else spaces := true;
			end;
                    if (c=' ') and spaces then conf[cnt] := conf[cnt] + c;
                    if c='#' then break;
                end;
            if conf[1]=confName then break;
        end;
    close(fh);
    result := conf[2];
end;

{------------------------------------------------------------------------------}
{================================= Web Unit ===================================}
{*******************************************************************************
 * This unit contains some basic functions useful in CGI programming           *
 *******************************************************************************
}

{--[ Web_PutVars ]--------------------------------------------------------------}
// Auxilliary procedure. Sorts varible names and values and puts them to cgi

procedure Web_PutVars(data: PString; cookie: boolean);
var isName, isLit: boolean;
    ptr, len: longint;
    s, sep: string;
begin
    if cookie then sep := ';' else sep := '&';
    SetLength(cgi, length(cgi) + 1);
    isName := true;
    ptr := 1;
    len := length(data^);
    while ptr <= len do
        begin
            isLit := false;
            s := copy(data^, ptr, 1);
            inc(ptr);
            if s = '\' then
                begin
                    isLit := true;
                    s := copy(data^, ptr, 1);
                    inc(ptr);
                end;
            if isLit or ((s<>'=') and (s<>sep)) then
            with cgi[length(cgi) - 1] do
            if isName then name := name + s else value := value + s
            else
                begin
                    if s = sep then
                        begin
                            SetLength(cgi, length(cgi) + 1);
                            if cookie then inc(ptr);
                            isName := true;
                        end
                    else
                        isName := false;
                end;
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_PutMultipart ]--------------------------------------------------------}
// Auxilliary procedure. Processes "multipart/form-data" forms

// Simple functions used by it

procedure MP_FormSplit(var data: PString; boundary: string; var form: MP_PForm);
// Splits the form into items
var separator: string;
    ptr, len, len2: longint;
begin
    separator := '--' + boundary + #13 + #10;
    len2 := length(separator);
    // Cutting off last boundary
    len := pos('--' + boundary + '--', data^);
    data^ := copy(data^, 1, len-1);
    // Cutting off first boundary
    delete(data^, 1, len2);
    while len > 0 do
        begin
            len := length(data^);
            ptr := pos(separator, data^);
            if ptr <> 0 then
                begin
                    // Not last item
                    SetLength(form^, length(form^) + 1);
                    form^[length(form^) - 1] := copy(data^, 1, ptr - 2);
                    // Cutting this item and next boundary
                    delete(data^, 1, ptr + len2 - 1);
                end
            else
                begin
                    // Last item
                    SetLength(form^, length(form^) + 1);
                    form^[length(form^) - 1] := copy(data^, 1, len-1);
                    break;
                end;
        end;
end;

function MP_GetLine(data: PString; var ptr: longint): string;
// Extracts current line beginning from ptr and ending with #13#10
var s: string;
begin
    result := '';
    repeat
        s := copy(data^, ptr, 1);
        if (s <> #13) and (s <> #10) then result := result + s;
        inc(ptr);
    until (s = #13) or (s = #10);
    inc(ptr);
end;

function MP_SplitLine(line: string): MP_Line;
// Splits string by space. Max. result = 6 strings.
var cnt, elem, len: integer;
    s: string;
    quoted: boolean;
begin
    result[1] := '';
    result[2] := '';
    result[3] := '';
    result[4] := '';
    result[5] := '';
    result[6] := '';
    elem := 1;
    len := length(line);
    quoted := false;
    cnt := 1;
    for cnt := 1 to len do
        begin
            s := copy(line, cnt, 1);
            if (s='"') and (not quoted) then quoted := true;
            if (s='"') and quoted then quoted := false;
            if (s<>' ') and (s<>'=') and (s<>';') and (s<>'"') and (s<>':') then result[elem] := result[elem] + s;
            if ((s=' ') or (s=';') or (s=':') or (s='=')) and quoted then result[elem] := result[elem] + s;
            if ((s=';') or (s='=') or (s=':')) and (not quoted) then inc(elem);
        end;
end;

function MP_GetBoundary(content_type: string): string;
// Extracts data boundary from content-type string
var len: integer;
begin
    len := pos('=', Content_Type);
    result := copy(content_type, len + 1, length(content_type)-len);
    if pos('"', result)=1 then result := copy(result, 2, length(result) - 2);
end;

procedure Web_PutMultipart(data: PString; content_type: string);
var cnt, ptr, tmp, len, dpos: longint;
    buff, boundary: string;
    line: MP_Line;
    form: MP_PForm;
begin
    New(form);
    boundary := MP_GetBoundary(content_type);
    MP_FormSplit(data, boundary, form);
    for cnt := 0 to (length(form^) - 1) do
        begin
            ptr := 1;
            len := length(form^[cnt]);
            dpos := pos(#13 + #10 + #13 + #10, form^[cnt]) + 4;
            // Getting first line
            buff := MP_GetLine(@(form^[cnt]), ptr);
            // Splitting into words
            line := MP_SplitLine(buff);
            // Is it file or variable?
            if pos('filename', buff) <> 0 then
                begin
                    // It is a file
                    SetLength(upf, length(upf) + 1);
                    upf[length(upf) - 1].name := line[4];	
                    upf[length(upf) - 1].filename := line[6];
                    // Getting content type
                    buff := MP_GetLine(@(form^[cnt]), ptr);
                    line := MP_SplitLine(buff);
                    upf[length(upf) - 1].content_type := line[2];			
                    // Getting value till the end
                    upf[length(upf) - 1].size := len - dpos;
                    upf[length(upf) - 1].value := copy(form^[cnt], dpos, upf[length(upf) - 1].size);
                end
            else
                begin
                    // It is a variable
                    SetLength(cgi, length(cgi) + 1);
                    cgi[length(cgi) - 1].name := line[4];	
                    // Getting value till the end
                    tmp := len - dpos;
                    cgi[length(cgi) - 1].value := copy(form^[cnt], dpos, tmp);
                end;
        end;
    Dispose(form);
end;

{------------------------------------------------------------------------------}

{--[ Web_GetData ]-------------------------------------------------------------}
// Recieves GET,  POST and COOKIE data and Uploaded Files.

procedure Web_GetData;
var cnt, cont_len, upl_max_size: longint;
    data, method, content_type: string;
    cookies, apache: boolean;
begin
	// Checking if server environment is set
	if (Web_GetEnv('REQUEST_METHOD') = '') and (Web_GetEnv('CONTENT_TYPE') = '') then exit;
    // Getting configuration from ini file
    if Web_GetConfig(PSP_INI_PATH, 'cookies')='off' then cookies := false else cookies := true;
    if Web_GetConfig(PSP_INI_PATH, 'apache_compat')='off' then apache := false else apache := true;
    val(Web_GetConfig(PSP_INI_PATH, 'upload_max_size'), upl_max_size);
    upl_max_size := upl_max_size*1048576;
    // Getting request method
    method := upcase(Web_GetEnv('REQUEST_METHOD'));
    // First recieving POST data... (Works on ALL platforms with NO limits)
    if method = 'POST' then
        begin
            content_type := Web_GetEnv('CONTENT_TYPE');
            // Here is some code for Apache webservers
            if apache then
                begin
                    cont_len := 0;
                    data := '';
                    val(Web_GetEnv('CONTENT_LENGTH'), cont_len);
                    if cont_len > upl_max_size then cont_len := upl_max_size;
                    SetLength(data, cont_len);
                    for cnt := 1 to cont_len do read(data[cnt]);
                end
            // Here is POST code for other webservers
            else
                begin
                    // It HAS to work on MS IIS but however it doesn't!
                    // If you find the reason - share it with PSP community! 
                end;
            // Dumping got data to cgi
            if pos('application/x-www-form-urlencoded', lowercase(content_type)) > 0 then Web_PutVars(@data, false);
            // Dumping variables to cgi and uploaded files to upf
            if pos('multipart/form-data', lowercase(content_type)) > 0 then Web_PutMultipart(@data, content_type);
            data := '';
            // End POST method
        end;
    //Recieving GET variables using getenv('QUERY_STRING'). (ALL Platforms)
    if method = 'GET' then
        begin
            data := Web_GetEnv('QUERY_STRING');
            Web_PutVars(@data, false);
            // End GET method
        end;
    //Recieving Cookie just same way as variables sent through GET but using HTTP_COOKIE env. variable. (ALL Platforms)
    if cookies then
        begin
            data := Web_GetEnv('HTTP_COOKIE');
            Web_PutVars(@data, true);
        end;
    for cnt := 0 to (length(cgi) - 1) do
        begin
            cgi[cnt].value := HTTP_Decode(cgi[cnt].value);
            cgi[cnt].name := HTTP_Decode(cgi[cnt].name);
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_Header ]--------------------------------------------------------------}
//OutPuts default HTTP Header.

procedure Web_Header;
var charset: string;
begin
    if Web_GetConfig(PSP_INI_PATH, 'cache') = 'off' then writeln('Pragma: no-cache');
    charset := Web_GetConfig(PSP_INI_PATH, 'charset');
    if charset='off' then writeln('Content-Type: text/html')
    else writeln('Content-Type: text/html; charset=' + charset);
    writeln;
end;

//OutPuts HTTP Header. addHeader:  additional header line.

procedure Web_Header(addHeader: string);
var charset: string;
begin
    if Web_GetConfig(PSP_INI_PATH, 'cache') = 'off' then writeln('Pragma: no-cache');
    if addHeader<>'' then writeln(addHeader);
    charset := Web_GetConfig(PSP_INI_PATH, 'charset');
    if charset='off' then writeln('Content-Type: text/html')
    else writeln('Content-Type: text/html; charset=' + charset);
    writeln;
end;

{------------------------------------------------------------------------------}

{--[ Web_VarExitsts ]----------------------------------------------------------}
// Checks if the variable is recieved

function Web_VarExists(name: string): boolean;
var i: integer;
begin
    result := false;
    for i := 0 to (length(cgi) - 1) do
    if cgi[i].name = name then result := true;
end;

{------------------------------------------------------------------------------}

{--[ Web_GetVar ]--------------------------------------------------------------}
// Gets value of recieved variable

function Web_GetVar(name: string): string;
var i: integer;
begin
    result := '';
    for i := 0 to (length(cgi) - 1) do
    if cgi[i].name = name then result := cgi[i].value;
end;

{------------------------------------------------------------------------------}

{--[ Web_SetVar ]--------------------------------------------------------------}
// Puts new pair name=value to CGI or rewrites existing variable

procedure Web_SetVar(name, value: string);
var i: integer;
begin
    if Web_VarExists(name) then
        begin
            for i := 0 to (length(cgi) - 1) do if cgi[i].name = name then cgi[i].value := value;
        end
    else
        begin
            SetLength(cgi, length(cgi) + 1);
            cgi[length(cgi) - 1].name := name;
            cgi[length(cgi) - 1].value := value;
        end;
end;

{------------------------------------------------------------------------------}
 
{--[ Web_UnsetVar ]------------------------------------------------------------}
// Removes the variable from variable list

function Web_UnsetVar(name: string): boolean;
var i: integer;
    tcgi: cgiData;
begin
    SetLength(tcgi, 0);
    if not Web_VarExists(name) then exit(false);
    for i := 0 to (length(cgi) - 1) do if cgi[i].name <> name then
        begin
            SetLength(tcgi, length(tcgi) + 1);
            tcgi[length(tcgi) - 1] := cgi[i];
        end;
    if length(cgi) > length(tcgi) then result := true else result := false;
    cgi := tcgi;
end;

{------------------------------------------------------------------------------}


{--[ Web_FormatStr ]-----------------------------------------------------------}
// Returns formatted version of str including CGI variables

function Web_FormatStr(str: string): string;
var i, len: longint;
    c: char;
    name: string;
    isVar: boolean;
begin
    if pos('$', str) > 0 then
        begin
            result := '';
            len := length(str);
            i := 1;
            while i <= len do
                begin
                    c := str[i];
                    inc(i);
                    if c = '$' then
                        begin
                            // Parsing as CGI variable
                            name := '';
                            isVar := true;
                            // Getting variable name
                            repeat
                                c := str[i];
                                if pos(c, ID_CHARS) > 0 then
                                    begin
                                        name := name + c;
                                        inc(i);
                                    end
                                else isVar := false;
                            until isVar = false;
                            if Web_VarExists(name) then result := result + Web_GetVar(name)
                            else result := result + '$' + name;
                        end
                    else result := result + c;
                end;
        end
    else result := str;
end;
 
{------------------------------------------------------------------------------}

{--[ Web_Out ]-----------------------------------------------------------------}
// Formatted string output

procedure Web_Out(str: string);
begin
    write(Web_FormatStr(str));
end;

{------------------------------------------------------------------------------}

{--[ Web_OutLn ]---------------------------------------------------------------}
// Formatted string output. Ends with end-of-line.

procedure Web_OutLn(str: string);
begin
    writeln(Web_FormatStr(str));
end;

{------------------------------------------------------------------------------}

{--[ Web_FileOut ]-------------------------------------------------------------}
//OutPuts content of the fileName text file

procedure Web_FileOut(fileName: string);
var buff: string;
    fh: text;
begin
    assign(fh, fileName);
    reset(fh);
    while not eof(fh) do
        begin
            readln(fh, buff);
            writeln(buff);
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{--[ Web_TemplateOut ]---------------------------------------------------------}
// Formatted file output

procedure Web_TemplateOut(fileName: string);
var fh: text;
    buff: string;
begin
    assign(fh, fileName);
    reset(fh);
    while not eof(fh) do
        begin
            readln(fh, buff);
            Web_OutLn(buff);
        end;
    close(fh);
end;
 
{------------------------------------------------------------------------------}

{--[ Web_ResourceOut ]---------------------------------------------------------}
// OutPuts header and content of resource file

procedure Web_ResourceOut(fileName: string);
var fh: file of char;
    i, len, ptr: integer;
    ext, content_type: string;
    c: char;
begin
    // First scanning for last occurance of "."
    ptr := 0;
    len := length(fileName);
    for i := 1 to len do if fileName[i] = '.' then ptr := i;
    if ptr > 0 then
        begin
            // Extracting file extension and setting file type
            ext := copy(fileName, ptr + 1, len - ptr);
            content_type := 'text/plain';
            if (ext='gif') or (ext='GIF') then content_type := 'image/gif';
            if (ext='jpg') or (ext='JPG') then content_type := 'image/jpeg';
            if (ext='png') or (ext='PNG') then content_type := 'image/png';
            if (ext='zip') or (ext='ZIP') then content_type := 'application/zip';
            if (ext='gz') or (ext='GZ') or (ext='tgz') or (ext='TGZ') or (ext='bz2') or (ext='BZ2') then content_type := 'application/x-tar';
            if (ext='rar') or (ext='RAR') then content_type := 'application/x-rar-compressed';
            if (ext='mp3') or (ext='MP3') then content_type := 'audio/mpeg';
            if (ext='wav') or (ext='WAV') then content_type := 'audio/wav';
            if (ext='html') or (ext='htm') or (ext='shtml') or (ext='phtml') or (ext='xhtml') then content_type := 'text/html';
            if (ext='css') or (ext='CSS') then content_type := 'text/css';
            if (ext='js') or (ext='JS') or (ext='jsc') or (ext='JSC') then content_type := 'text/javascript';
            if (ext='exe') or (ext='EXE') then content_type := 'application/x-msdownload';
        end
    else content_type := 'application/x-download';
    // Output
    assign(fh, fileName);
    reset(fh);
    writeln('Content-Type: ', content_type);
    writeln('Content-Length: ', filesize(fh));
    writeln;
    while not eof(fh) do
        begin
            read(fh, c);
            write(c);
        end;
    close(fh);
end;
{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{=========================== Environment Variables ============================}
{*******************************************************************************
 * Few functions to get environment variables directly from web unit.          *
 *******************************************************************************
}

{--[ Web_GetEnv ]--------------------------------------------------------------}
// Returns value of the environment variable

function Web_GetEnv(name: string): string;
begin
    result := getenv(upcase(name));
end;

{------------------------------------------------------------------------------}

{------------------------------------------------------------------------------}
{=============================== Cookie Functions =============================}
{*******************************************************************************
 * These functions provide you to Set/Delete cookies.                          *
 *******************************************************************************
}

{--[ Web_SetCookie ]-----------------------------------------------------------}
// Sets a cookie

procedure Web_SetCookie(name, value: string);
begin
    writeln('Set-Cookie: ' + HTTP_Encode(name) + '=' + HTTP_Encode(value) + ';path=/cgi-bin;expires=Mon, 01 Dec 2020 12:00:00 GMT');
end;

// Sets a cookie with custom parameters

procedure Web_SetCookie(name, value, path, domain, expires: string);
begin
    writeln('Set-Cookie: ' + HTTP_Encode(name) + '=' + HTTP_Encode(value) + ';path=' + path + ';domain=' + domain + ';expires=' + expires);
end;

{------------------------------------------------------------------------------}

{--[ Web_DeleteCookie ]--------------------------------------------------------}
// Destroys cookie which was set by usual Web_SetCookie

procedure Web_DeleteCookie(name: string);
begin
    writeln('Set-Cookie: ' + HTTP_Encode(name) + '=;path=/cgi-bin;expires=Mon, 01 Dec 1998 12:00:00 GMT');
end;

// Destroys cookie which was set by Web_SetCookie with custom parameters.
// parameters must be the same

procedure Web_DeleteCookie(name, path, domain: string);
begin
    writeln('Set-Cookie: ' + HTTP_Encode(name) + '=;path=' + path + ';domain=' + domain + ';expires=Mon, 01 Dec 1998 12:00:00 GMT');
end;

{------------------------------------------------------------------------------}
{============================ Web_Filters Unit ================================}
{*******************************************************************************
 * This unit includes some functions for work with strings,                    *
 * connected with web application's rights and security                        *
 *******************************************************************************
}

{--[ Web_RandPass ]------------------------------------------------------------}
// Generates a random string of alphanumeric and '_' characters with special
// length
function Web_RandPass(len: integer): string;
var i: integer;
begin
    SetLength(result, len);
    randomize;
    for i := 1 to len do result[i] := PW_CHARS[random(62) + 1];
end;

{------------------------------------------------------------------------------}

{--[ Web_DisableTags ]---------------------------------------------------------}
//Replaces all special charecters with their HTML analogs

function Web_DisableTags(svar: string): string;
begin
    result := SubstrReplace(svar, '&', '&amp;');
    result := SubstrReplace(result, '"', '&quot;');
    result := SubstrReplace(result, '<', '&lt;');
    result := SubstrReplace(result, '>', '&gt;');
end;

{------------------------------------------------------------------------------}

{--[ Web_Nl2Br ]---------------------------------------------------------------}
// Replaces all "end-of-line" symbols with <br> tags

function Web_Nl2Br(svar: string): string;
begin
    result := SubstrReplace(svar, #10, '<br>');
end;

{------------------------------------------------------------------------------}

{--[ Web_Crypt ]---------------------------------------------------------------}
//Generates encrypted string. May be too slow.
{ This one causes too many bugs
function Web_Crypt(str, key: string;step: integer): string;
var cnt, cnt2, lim, lim2, lim3: integer;
    chs: string;
begin
    lim := length(str);
    lim2 := length(key);
    lim3 := round(lim + (lim/step) + 1);
    for cnt := 1 to lim do
        begin
            chs := copy(str, (lim + 1) - cnt, 1);
            Web_Crypt := Web_Crypt + chs;
        end;
    cnt := 1;
    while cnt <= lim3 do
        begin
            for cnt2 := 1 to lim2 do
                begin
                    if cnt >= lim3 then break;
                    chs := copy(key, cnt2, 1);
                    insert(chs, Web_Crypt, cnt);
                    cnt := cnt + step;
                end;
        end;
end;
}
{------------------------------------------------------------------------------}

{--[ Web_XORCrypt ]------------------------------------------------------------}
// Generates encrypted string using simple XOR crypting algoritm

function Web_XORCrypt(str: string; key: byte): string;
var i, len: integer;
    bt: byte;
begin
    result := '';
    len := length(str);
    for i := 1 to len do
        begin
            bt :=  ord(str[i]) xor key;
            result := result + chr(bt);
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_XORHash ]-------------------------------------------------------------}
// Calculates hash using simple XOR crypting algoritm

function Web_XORHash(str, key: string): string;
var i, len, lkey, hash: longint;
    c: char;
begin
    lkey := 0;
    len := length(key);
    for i := 1 to len do
        begin
            c := key[i];
            lkey := lkey + (ord(c) * i);
        end;
    hash := 0;
    len := length(str);
    for i := 1 to len do
        begin
            c := str[i];
            hash := hash + (ord(c) xor lkey);
        end;
    result := HexStr(hash, 8);
end;

{------------------------------------------------------------------------------}
{============================= Session Functions ==============================}
{*******************************************************************************
 * Sessions is a technology of storing private user's data directly on the     *
 * webserver. These functions provide PSP Sessions mechanism.                  *
 *******************************************************************************
}

{--[ Web_SessionStart ]--------------------------------------------------------}
// Recieves session data and puts it into CGI

function Web_SessionStart: boolean;
var path, sid, skey, session, s: string;
    cbyte: byte;
    cnt, len: integer;
    row: SDS_Row;
    bName: boolean;
begin
    SetLength(row, 3);
    path := Web_GetConfig(PSP_INI_PATH, 'session_path') + 'psp_sess.sds';
    if not FExists(path) then
        begin
            row[0] := 'id';
            row[1] := 'key';
            row[2] := 'session';
            SDS_Create(path, 3, @row);
        end;
    sid := Web_GetVar('PSPSESS');
    skey := Web_GetVar('PSPSKEY');
    if (sid = '') or (skey = '') then exit(false);
    val(Web_GetConfig(PSP_INI_PATH, 'session_crypt_byte'), cbyte);
    sid := Web_XORCrypt(sid, cbyte);
    skey := Web_XORCrypt(skey, cbyte);
    row := SDS_SelectRow(path, 'id', sid);
    if row[1] <> skey then exit(false);
    session := row[2];
    SetLength(cgi, length(cgi) + 1);
    cnt := 1;
    bName := true;
    len := length(session);
    while cnt <= len do
        begin
            s := copy(session, cnt, 1);
            inc(cnt);
            if (s <> '=') and (s <> ';') then
            with cgi[length(cgi) - 1] do
            if bName then name := name + s else value := value + s
            else
                begin
                    if s=';' then
                        begin
                            cgi[length(cgi) - 1].value := HTTP_Decode(cgi[length(cgi) - 1].value);
                            SetLength(cgi, length(cgi) + 1);
                            bName := true;
                        end
                    else
                        bName := false;
                end;
        end;
    cgi[length(cgi) - 1].value := HTTP_Decode(cgi[length(cgi) - 1].value);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ Web_SessionRegister ]-----------------------------------------------------}
// Adds new pair "varable=value" to the session

function Web_SessionRegister(name, value: string): boolean;
var path, sid, skey, session, temp, s: string;
    nSid, cnt, len: longint;
    cbyte: byte;
    row: SDS_Row;
begin
    SetLength(row, 3);
    path := Web_GetConfig(PSP_INI_PATH, 'session_path') + 'psp_sess.sds';
    if not FExists(path) then
        begin
            row[0] := 'id';
            row[1] := 'key';
            row[2] := 'session';
            SDS_Create(path, 3, @row);
        end;
    val(Web_GetConfig(PSP_INI_PATH, 'session_crypt_byte'), cbyte);
    sid := Web_GetVar('PSPSESS');
    skey := Web_GetVar('PSPSKEY');
    if (sid <> '') and (skey <> '') then
        begin
            sid := Web_XORCrypt(sid, cbyte);
            skey := Web_XORCrypt(skey, cbyte);
            s := SDS_SelectField('id', path, 'key', skey);
            if s <> sid then exit(false);
            session := SDS_SelectField('session', path, 'id', sid);
            if SubstrExists(session, HTTP_Encode(name) + '=') then
                begin
                    cnt := pos(HTTP_Encode(name), session);
                    temp := copy(session, 1, cnt - 1);
                    s := copy(session, cnt, 1);
                    inc(cnt);
                    len := length(session);
                    while (s <> ';') and (s <> #10) and (cnt <= len) do
                        begin
                            s := copy(session, cnt, 1);
                            inc(cnt);
                        end;
                    temp := temp + copy(session, cnt, len - cnt + 1);
                    session := temp;
                end;
            session := session + ';' + HTTP_Encode(name) + '=' + HTTP_Encode(value);
            SDS_UpdateField(path, 'id', sid, 'session', session);
        end
    else
        begin
            nSid := SDS_GetLastID(path);
            inc(nSid);
            str(nSid, sid);
            sid := Web_XORCrypt(sid, cbyte);
            skey := Web_RandPass(8);
            skey := Web_XORCrypt(skey, cbyte);
            Web_SetCookie('PSPSESS', sid);
            Web_SetCookie('PSPSKEY', skey);
            Web_SetVar('PSPSESS', sid);
            Web_SetVar('PSPSKEY', skey);
            row[1] := Web_XORCrypt(skey, cbyte); 
            row[2] := HTTP_Encode(name) + '=' + HTTP_Encode(value);
            SDS_Insert(path, @row);
        end;
    Web_SetVar(name, value);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ Web_SessionUnregister ]---------------------------------------------------}
// Deletes pair "variable=value" from the session

function Web_SessionUnregister(name: string): boolean;
var path, sid, skey, session, temp, s: string;
    cnt, len: longint;
    cbyte: byte;
begin
    path := Web_GetConfig(PSP_INI_PATH, 'session_path') + 'psp_sess.sds';
    val(Web_GetConfig(PSP_INI_PATH, 'session_crypt_byte'), cbyte);
    sid := Web_GetVar('PSPSESS');
    skey := Web_GetVar('PSPSKEY');
    if sid = '' then exit(false);
    if skey = '' then exit(false);
    sid := Web_XORCrypt(sid, cbyte);
    session := SDS_SelectField('session', path, 'id', sid);
    len := length(session);
    if not SubstrExists(session, HTTP_Encode(name) + '=') then exit(false);
    cnt := pos(HTTP_Encode(name), session);
    if cnt = 0 then exit(false);
    temp := copy(session, 1, cnt - 1);
    s := copy(session, cnt, 1);
    inc(cnt);
    while (s <> ';') and (s <> #10) and (cnt <= len) do
        begin
            s := copy(session, cnt, 1);
            inc(cnt);
        end;
    temp := temp + copy(session, cnt, len - cnt + 1);
    SDS_UpdateField(path, 'id', sid, 'session', temp);
    Web_UnsetVar(name);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ Web_SessionDestroy ]------------------------------------------------------}
// Destroys user's session

function Web_SessionDestroy: boolean;
var path, sid, skey: string;
    cbyte: byte;
begin
    path := Web_GetConfig(PSP_INI_PATH, 'session_path') + 'psp_sess.sds';
    val(Web_GetConfig(PSP_INI_PATH, 'session_crypt_byte'), cbyte);
    sid := Web_GetVar('PSPSESS');
    skey := Web_GetVar('PSPSKEY');
    if (sid = '') or (skey = '') then exit(false);
    sid := Web_XORCrypt(sid, cbyte);
    SDS_Delete(path, 'id', sid);
    Web_DeleteCookie('PSPSESS');
    Web_DeleteCookie('PSPSKEY');
    result := true;
end;

{------------------------------------------------------------------------------}
{================================= File Uploads ===============================}
{*******************************************************************************
 * Functions to handle form-based file uploads (RFC 1867).                     *
 *******************************************************************************
}

{--[ Web_IsUploaded ]----------------------------------------------------------}
// Checks if a file for this file input field is uploaded

function Web_IsUploaded(varName: string): boolean;
var cnt: integer;
begin
    result := false;
    for cnt := 0 to (length(upf) - 1) do if (upf[cnt].name = varName) and (upf[cnt].size > 0) then
        begin
            result := true;
            break;
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_UploadedFileSize ]----------------------------------------------------}
// Returns size of uploaded file

function Web_UploadedFileSize(varName: string): longint;
var cnt: integer;
begin
    result := 0;
    for cnt := 0 to (length(upf) - 1) do if upf[cnt].name = varName then
        begin
            result := upf[cnt].size;
            break;
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_UploadedFileType ]----------------------------------------------------}
// Returns content type of uploaded file

function Web_UploadedFileType(varName: string): string;
var cnt: integer;
begin
    for cnt := 0 to (length(upf) - 1) do if upf[cnt].name = varName then
        begin
            result := upf[cnt].content_type;
            break;
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_UploadedFileName ]----------------------------------------------------}
// Returns original name of uploaded file

function Web_UploadedFileName(varName: string): string;
var cnt: integer;
begin
    for cnt := 0 to (length(upf) - 1) do if upf[cnt].name = varName then
        begin
            result := upf[cnt].filename;
            break;
        end;
end;

{------------------------------------------------------------------------------}

{--[ Web_SaveUploadedFile ]----------------------------------------------------}
// Saves uploaded file on disk

procedure Web_SaveUploadedFile(varName, fullPath: string);
var fh: text;
    cnt: integer;
begin
    assign(fh, fullPath);
    rewrite(fh);
    for cnt := 0 to (length(upf) - 1) do if upf[cnt].name=varName then
        begin
            write(fh, upf[cnt].value);
            break;
        end;
    close(fh);
end;

{------------------------------------------------------------------------------}

{==============================================================================}
{=============================== Main Code Block ==============================}
{==============================================================================}

begin
    Web_GetData;

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.

{==============================================================================}
{==============================================================================}
