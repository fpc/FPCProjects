{$IFDEF FPC}{$H+}{$MODE OBJFPC}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
unit http;
{
 *******************************************************************************
 *              -== Pascal Server Pages unit - HTTP Client Unit ==-            *
 *******************************************************************************
 * HTTP client-side API.                                                       *
 *******************************************************************************
 * See the Pascal Server Pages Documentation for more information.             *
 *******************************************************************************
 *   Written by Vladimir Sibirov a.k.a. Trustmaster                            *
 *   http://www.psp.furtopia.org                                               *
 *   mailto:psp@furtopia.org                                                   *
 *******************************************************************************
 * Copyright (c) 2003-2005 by Pascal Server Pages development team.            *
 * See the Pascal Server Pages License for more information.                   *
 *******************************************************************************
 * [PSP 1.4.0 - 20.08.05 - Trustmaster]:                                       *
 * - finished with first full implementation of this unit, but it didn't work  *
 * on my WinXP for unknown reason.                                             *
 *******************************************************************************
}

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type HTTPConnection = Pointer;
    // Complete HTTP connection handle

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

procedure http_close(cp: HTTPConnection);
// Closes SMTP connection

function http_connect(const address: string): HTTPConnection;
// Connects to HTTP server specified by hostname:port or hostname

function http_copy(const source, dest: string): boolean;
// Copy remote file to local one
// Source must be full HTTP URL (may even contain get params), example:
// www.server.com/path/script.php?cid=256&name=example
// Get prams must be URLEncoded
// Dest is local file name accessible for writing

function http_eof(cp: HTTPConnection): boolean;
// Checks if Response document is at enf of file

function http_get(const url: string): string;
// Returns a string containing the file represented by URL
// URL must be full HTTP URL (may even contain get params), example:
// www.server.com/path/script.php?cid=256&name=example
// Get prams must be URLEncoded

function http_get_header(cp: HTTPConnection; const name: string): string;
// Returns value of server Response header

function http_readc(cp: HTTPConnection): char;
// Reads single char from Response document

function http_reads(cp: HTTPConnection): string;
// Reads a line from Response document

function http_request(cp: HTTPConnection; const method, uri: string): boolean;
// Sends HTTP request. Headers and POST data must be set before this call

function http_response_info(cp: HTTPConnection; var final_url, message: string): word;
// Fetches response result info (exact document URL, response message and code as result)

procedure http_set_header(cp: HTTPConnection; const name, value: string);
// Sets client Request header

procedure http_set_post(cp: HTTPConnection; const data: string);
// Sets client Request POST data (for POST method)
// Variables must be URLEncoded

procedure http_put_header(cp: HTTPConnection; const header: string);
// Sets client Requst header from 'Name: Value' string

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation
uses hostname, sockets, substrings;

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type THTTPHeader = record
    // Header representation
        name, value: string;
    end;
    
    THTTPHeaders = array of THTTPHeader;

    
    THTTPConnection = record
    // Complete HTTP connection record
        sock: longint; // Connected socket
        sin, sout: text; // I/O streams
        request, response: THTTPHeaders; // Request/Response headers
        code, uri: string; // Response code + message; final resolved uri (for redirect purpose)
        post: string; // Post data string
    end;

    PHTTPConnection = ^THTTPConnection;
    // Pointer to THTTPConnection
        
{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{--[ http_close ]--------------------------------------------------------------}
// Closes SMTP connection

procedure http_close(cp: HTTPConnection);
var conn: PHTTPConnection;
begin
    conn := PHTTPConnection(cp);
    close(conn^.sin);
    close(conn^.sout);
    CloseSocket(conn^.sock);
    dispose(conn);
end;

{------------------------------------------------------------------------------}

{--[ http_connect ]------------------------------------------------------------}
// Connects to HTTP server specified by hostname:port or hostname

function http_connect(const address: string): HTTPConnection;
var conn: PHTTPConnection;
    addr: TInetSockAddr;
    server: string;
    p: longint;
    port: word;
begin
    // Init
    result := nil;
    new(conn);
    // Supporting allowed address syntax
    p := pos(':', address);
    if p > 0 then
        begin
            // Splitting by :
            server := copy(address, 1, p - 1);
            val(copy(address, p + 1, length(address) - p), port);
            addr := inet_resolve(server, port);
        end
    else
        addr := inet_resolve(address, 80);
    // Checking address validity
    if addr.addr <= 0 then
        begin
            dispose(conn);
            exit(nil);
        end;
    // Performing connection
    conn^.sock := socket(AF_INET, SOCK_STREAM, 0);
    if not connect(conn^.sock, addr, conn^.sin, conn^.sout) then
        begin
            dispose(conn);
            exit(nil);
        end;
    // Descriptors init
    reset(conn^.sin);
    rewrite(conn^.sout);
    // Setting some default request headers
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := 'Accept';
    conn^.request[length(conn^.request) - 1].value := 'text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5';
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := 'Accept-Charset';
    conn^.request[length(conn^.request) - 1].value := 'windows-1252, iso-8859-1;q=0.6, *;q=0.1';
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := 'Connection';
    conn^.request[length(conn^.request) - 1].value := 'close';
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := 'Host';
    conn^.request[length(conn^.request) - 1].value := server;
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := 'User-Agent';
    conn^.request[length(conn^.request) - 1].value := 'PSP HTTP Module/1.4.0';
    // Result linking
    result := HTTPConnection(conn);
end;

{------------------------------------------------------------------------------}

{--[ http_copy ]---------------------------------------------------------------}
// Copy remote file to local one
// Source must be full HTTP URL (may even contain get params), example:
// www.server.com/path/script.php?cid=256&name=example
// Get prams must be URLEncoded
// Dest is local file name accessible for writing

function http_copy(const source, dest: string): boolean;
var fh: text;
    data: string;
begin
    // I think, char-by-char copying is not aim for copying about full http_get source
    result := false;
    data := http_get(source);
    if data = '' then exit(false);
    assign(fh, dest);
    rewrite(fh);
    write(fh, data);
    close(fh);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ http_eof ]----------------------------------------------------------------}
// Checks if Response document is at enf of file

function http_eof(cp: HTTPConnection): boolean;
var conn: PHTTPConnection;
begin
    conn := PHTTPConnection(cp);
    result := eof(conn^.sin);
end;

{------------------------------------------------------------------------------}

{--[ http_get ]----------------------------------------------------------------}
// Returns a string containing the file represented by URL
// URL must be full HTTP URL (may even contain get params), example:
// www.server.com/path/script.php?cid=256&name=example
// Get prams must be URLEncoded

function http_get(const url: string): string;
var host, uri, temp, loc: string;
    port: word;
    addr: TInetSockAddr;
    sock, p: longint;
    c: char;
    sin, sout: text;
    redir: boolean;
    nv: StrArray;
begin
    // Init
    result := '';
    // Parsing url
    host := copy(url, 1, pos('/', url) - 1);
    uri := copy(url, pos('/', url), length(url) - pos('/', url));
    if uri = '' then uri := '/';
    p := pos(':', host);
    if p > 0 then
        begin
            // Splitting by :
            val(copy(host, p + 1, length(host) - p), port);
            host := copy(host, 1, p - 1);
            addr := inet_resolve(host, port);
        end
    else
        addr := inet_resolve(host, 80);
    // Checking address validity
    if addr.addr <= 0 then exit;
    // Connecing
    sock := socket(AF_INET, SOCK_STREAM, 0);
    if not connect(sock, addr, sin, sout) then exit;
    // Descriptors init
    reset(sin);
    rewrite(sout);
    // Sending request
    writeln(sout, 'GET ' + uri + ' HTTP/1.1');
    writeln(sout, 'Accept: text/xml,application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5');
    writeln(sout, 'Connection: close');
    writeln(sout, 'Host: ' + host);
    writeln(sout, 'User-Agent: PSP HTTP Module/1.4.0');
    writeln(sout);
    flush(sout);
    // First line
    readln(sin, temp);
    val(copy(temp, 10, 3), port);
    if (port <> 200) and (port <> 301) and (port <> 302) and (port <> 303) then exit('');
    // Headers
    redir := false;
    repeat
        readln(sin, temp);
        if upcase(copy(temp, 1, 8)) = 'LOCATION' then
            begin
                nv := substr_split(temp, ':');
                loc := str_trim(nv[1]);
                redir := true;
            end;
    until temp = '';
    if redir then
        begin
            // Redirected
            close(sin);
            close(sout);
            CloseSocket(sock);
            result := http_get(loc);
        end
    else
        begin
            // Getting contents
            while not eof(sin) do
                begin
                    read(sin, c);
                    SetLength(result, length(result) + 1);
                    result[length(result)] := c;
                end;
            // Closing connection
            close(sin);
            close(sout);
            CloseSocket(sock);
        end;
end;

{------------------------------------------------------------------------------}

{--[ http_get_header ]---------------------------------------------------------}
// Returns value of server Response header

function http_get_header(cp: HTTPConnection; const name: string): string;
var conn: PHTTPConnection;
    i: longword;
begin
    conn := PHTTPConnection(cp);
    result := '';
    if length(conn^.response) > 0 then
    for i := 0 to length(conn^.response) - 1 do if upcase(conn^.response[i].name) = upcase(name) then
        begin
            result := conn^.response[i].value;
            break;
        end;
end;

{------------------------------------------------------------------------------}

{--[ http_readc ]--------------------------------------------------------------}
// Reads single char from Response document

function http_readc(cp: HTTPConnection): char;
var conn: PHTTPConnection;
begin
    conn := PHTTPConnection(cp);
    result := #0;
    if not eof(conn^.sin) then read(conn^.sin, result);
end;

{------------------------------------------------------------------------------}

{--[ http_reads ]--------------------------------------------------------------}
// Reads a line from Response document

function http_reads(cp: HTTPConnection): string;
var conn: PHTTPConnection;
begin
    conn := PHTTPConnection(cp);
    result := '';
    if not eof(conn^.sin) then readln(conn^.sin, result);
end;

{------------------------------------------------------------------------------}

{--[ http_request ]------------------------------------------------------------}
// Sends HTTP request. Headers and POST data must be set before this call

function http_request(cp: HTTPConnection; const method, uri: string): boolean;
var conn: PHTTPConnection;
    buff: string;
    nv: StrArray;
    i: longword;
begin
    conn := PHTTPConnection(cp);
    // Sending request
    // First line
    writeln(conn^.sout, upcase(method) + ' ' + uri + ' HTTP/1.1');
    // Then headers follow
    if length(conn^.request) > 0 then
    for i := 0 to length(conn^.request) - 1 do writeln(conn^.sout, conn^.request[i].name + ': ' + conn^.request[i].value);
    // Emtpy line
    writeln(conn^.sout);
    // Sending POST data
    if upcase(method) = 'POST' then writeln(conn^.sout, conn^.post);
    flush(conn^.sout);
    // Reading message
    readln(conn^.sin, conn^.code);
    // Reading headers
    repeat
        readln(conn^.sin, buff);
        if buff <> '' then
            begin
                SetLength(conn^.response, length(conn^.response) + 1);
                nv := substr_split(buff, ':');
                conn^.response[length(conn^.response) - 1].name := str_trim(nv[0]);
                conn^.response[length(conn^.response) - 1].value := str_trim(nv[1]);
            end;
    until (buff = '') or eof(conn^.sin);
    if copy(conn^.code, 10, 3) = '200' then result := true;
    // Now the redirect focus
    if length(conn^.response) > 0 then
    for i := 0 to length(conn^.response) - 1 do if upcase(conn^.response[i].name) = 'LOCATION' then
        begin
            conn^.uri := conn^.response[i].value;
            // Well, in plain HTTP it is supposed that you will reconnect yourself :(
            exit;
        end;
end;

{------------------------------------------------------------------------------}

{--[ http_response_info ]------------------------------------------------------}
// Fetches response result info (exact document URL, response message and code as result)

function http_response_info(cp: HTTPConnection; var final_url, message: string): word;
var conn: PHTTPConnection;
begin
    conn := PHTTPConnection(cp);
    final_url := conn^.uri;
    message := conn^.code;
    val(copy(conn^.code, 1, 3), result);
end;

{------------------------------------------------------------------------------}

{--[ http_set_header ]---------------------------------------------------------}
// Sets client Request header

procedure http_set_header(cp: HTTPConnection; const name, value: string);
var conn: PHTTPConnection;
    i: longword;
begin
    conn := PHTTPConnection(cp);
    // Changing value if already set
    if length(conn^.request) > 0 then
    for i := 0 to length(conn^.request) - 1 do if upcase(conn^.request[i].name) = upcase(name) then
        begin
            conn^.request[i].value := value;
            exit;
        end;
    // Or setting new header
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := name;
    conn^.request[length(conn^.request) - 1].value := value;
end;

{------------------------------------------------------------------------------}

{--[ http_set_post ]-----------------------------------------------------------}
// Sets client Request POST data (for POST method)
// Variables must be URLEncoded

procedure http_set_post(cp: HTTPConnection; const data: string);
var conn: PHTTPConnection;
    len: string;
    i: longword;
begin
    conn := PHTTPConnection(cp);
    conn^.post := data;
    str(length(data), len);
    // Changing value if already set
    if length(conn^.request) > 0 then
    for i := 0 to length(conn^.request) - 1 do if upcase(conn^.request[i].name) = upcase('Content-Length') then
        begin
            conn^.request[i].value := len;
            exit;
        end;
    // Or setting new header
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := 'Content-Length';
    conn^.request[length(conn^.request) - 1].value := len;
end;

{------------------------------------------------------------------------------}

{--[ http_put_header ]---------------------------------------------------------}
// Sets client Requst header from 'Name: Value' string

procedure http_put_header(cp: HTTPConnection; const header: string);
var conn: PHTTPConnection;
    i: longword;
    nv: StrArray;
begin
    conn := PHTTPConnection(cp);
    // Splitting into name=value pair
    nv := substr_split(header, ':');
    if length(nv) <> 2 then exit;
    nv[0] := str_trim(nv[0]);
    nv[1] := str_trim(nv[1]);
    // Changing value if already set
    if length(conn^.request) > 0 then
    for i := 0 to length(conn^.request) - 1 do if upcase(conn^.request[i].name) = upcase(nv[0]) then
        begin
            conn^.request[i].value := nv[1];
            exit;
        end;
    // Or setting new header
    SetLength(conn^.request, length(conn^.request) + 1);
    conn^.request[length(conn^.request) - 1].name := nv[0];
    conn^.request[length(conn^.request) - 1].value := nv[1];
end;

{------------------------------------------------------------------------------}

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
