{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                       Pascal Web Unit project (PWU)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 ------------------------------------------------------------------------------
  Main Web Unit
 ------------------------------------------------------------------------------
  This unit contains the main functions and procedures for web programs.

 ------------------------------------------------------------------------------
  Developer Notes
 ------------------------------------------------------------------------------
  This unit was Modified from the Pascal Server Pages project 1.4.X, mainly 
  syntax changes
  
 ------------------------------------------------------------------------------
  Developer Todo
 ------------------------------------------------------------------------------

 ------------------------------------------------------------------------------
  Credits:
 ------------------------------------------------------------------------------
  PSP Project, Vladimir Sibirov

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}


{$IFDEF FPC}{$MODE OBJFPC} {$H+}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
unit smtp;

{------------------------------------------------------------------------------}
interface
{------------------------------------------------------------------------------}

type

  // SMTP connection handle
  SMTPConnection = Pointer;

  // SMTP message handle
  SMTPMessage = Pointer;


{------------------------------------------------------------------------------}
{--------- PUBLIC PROCEDURE/FUNCTION DECLARATIONS -----------------------------}
{------------------------------------------------------------------------------}

  function smtp_attach(mp: SMTPMessage; const fname, ftype: string): boolean;

  procedure smtp_close(cp: SMTPConnection);

  function smtp_connect(const address, username, password: string): SMTPConnection;

  procedure smtp_free(mp: SMTPMessage);

  function smtp_get_header(mp: SMTPMessage; const name: string): string;

  function smtp_message(const ffrom, fto, fsubject, message: string): SMTPMessage;

  function smtp_send(cp: SMTPConnection; mp: SMTPMessage): boolean;

  procedure smtp_set_header(mp: SMTPMessage; const name, value: string);

  procedure smtp_set_text_type(mp: SMTPMessage; const ctype: string);

  procedure smtp_put_header(mp: SMTPMessage; const header: string);

  procedure smtp_TEST1;


//  END OF PUBLIC PROCEDURE/FUNCTION DECLARATIONS
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
implementation
{------------------------------------------------------------------------------}

uses
  base64enc,
  hostname,
  sockets,
  substrings;


type
  // Connected socket record
  TSMTPConnection = record
      sock: longint;
      sin, sout: text;
  end;

  // Pointer to TSMTPConnection
  PSMTPConnection = ^TSMTPConnection;

  // Header representation record
  TSMTPHeader = record
      name, value: string;
  end;
  
  TSMTPHeaders = array of TSMTPHeader;

  // Message data record
  TSMTPMessage = record
      attach, aheadr: array of string; // Attachments and their headers
      headers: TSMTPHeaders; // Message headers
      mBody, mType, mBoundary: string; // Custom data
  end;

  // Pointer to TSMTPMessage
  PSMTPMessage = ^TSMTPMessage;

        

{------------------------------------------------------------------------------}
{--------- PRIVATE PROCEDURES/FUNCTIONS ---------------------------------------}
{------------------------------------------------------------------------------}

// Random boundary generator
function build_boundary: string;
const
  BOUND_CHARSET = '1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
  BOUND_LIMIT = 20;
var
  i: longword;
begin
  result := '';
  SetLength(result, BOUND_LIMIT);
  for i := 1 to BOUND_LIMIT do
  begin
    randomize;
    result[i] := BOUND_CHARSET[random(61) + 1];
  end;
end;


// Checks if file exists
function file_exists(const fname: string): boolean;
var
  fh: file of byte;
begin
  assign(fh, fname);
  {$i-}
  reset(fh);
  {$i+}
  if ioresult = 0 then
  begin
    result := true;
    close(fh);
  end
  else result := false;
end;

//  END OF PRIVATE PROCEDURES/FUNCTIONS
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
{--------- PUBLIC PROCEDURES/FUNCTIONS ----------------------------------------}
{------------------------------------------------------------------------------}


// Adds file attachment to the message handle
function smtp_attach(mp: SMTPMessage; const fname, ftype: string): boolean;
var
  msg: PSMTPMessage;
  content_type,
  disposition,
  basename,
  temp: string;
  i,
  cnt,
  len: longword;
  fh: file of char;
  buff: array[1..4096] of char; //buffer to hold contents
begin
  msg := PSMTPMessage(mp);
  // Checking file
  if not file_exists(fname) then exit(false);
  // Checking Content-Type
  if pos('/', ftype) = 0 then
    content_type := 'application/x-unknown-content-type'
  else
    content_type := lowercase(ftype);
  // Detecting disposition
  if (pos('image', content_type) = 1) or (pos('text', content_type) = 1) then
    disposition := 'inline'
  else
    disposition := 'attachment';
  // Detecting basename
  if pos('\', fname) > 0 then
    basename := copy(fname, substr_rpos(fname, '\') + 1, length(fname) - substr_rpos(fname, '\'))
  else
    if pos('/', fname) > 0 then
      basename := copy(fname, substr_rpos(fname, '/') + 1, length(fname) - substr_rpos(fname, '/'))
  else
    basename := fname;
  // Setting message type to multipart/mixed
  if length(msg^.headers) > 0 then
    for i := 0 to length(msg^.headers) - 1 do
    begin
      if msg^.headers[i].name = 'Content-Type' then
      begin
        msg^.headers[i].value := 'multipart/mixed;' + #13+#10 +
                                 ' boundary="' + msg^.mBoundary + '"';
        break;
      end;
    end;
  // New attachment header
  SetLength(msg^.aheadr, length(msg^.aheadr) + 1);
  msg^.aheadr[length(msg^.aheadr) - 1] := '--' + msg^.mBoundary + #13+#10 +
                                          'Content-Type: ' + content_type + '; name="' + basename + '"' + #13#10 +
                                          'Content-Transfer-Encoding: base64' + #13+#10 +
                                          'Content-Disposition: ' + disposition + '; filename="' + basename + '"';
  // New attachment body
  SetLength(msg^.attach, length(msg^.attach) + 1);
  assign(fh, fname);
  reset(fh);
  temp := '';
  cnt := 0;
  while not eof(fh) do
  begin
    blockread(fh, buff, sizeof(buff), len);
    SetLength(temp, cnt + len);
    for i := 1 to len do
      temp[cnt + i] := buff[i];
    inc(cnt, len);
  end;
  close(fh);
  msg^.attach[length(msg^.attach) - 1] := base64_encode(temp);
  // Done
  result := true;
end;


// Closes SMTP connection
procedure smtp_close(cp: SMTPConnection);
var
  conn: PSMTPConnection;
begin
  conn := PSMTPConnection(cp);
  writeln(conn^.sout, 'QUIT');
  readln(conn^.sin);
  close(conn^.sin);
  close(conn^.sout);
  CloseSocket(conn^.sock);
  dispose(conn);
end;


// Connects to SMTP server specified by hostname:port or hostname
// Fill in username and password if SMTP auth is used or pass empty strings if not
function smtp_connect(const address, username, password: string): SMTPConnection;
var
  addr: TInetSockAddr;
  server: string;
  p: longint;
  port: word;
  conn: PSMTPConnection;
  selfaddr: string;
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
  end else
    addr := inet_resolve(address, 25);
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
  // We need our IP as dotted quad
  selfaddr := inet_self_addr;
  // Perform AUTH or plain HELO
  if username <> '' then
  begin
    writeln(conn^.sout, 'EHLO ' + selfaddr);
    //writeln(conn^.sout, 'EHLO localhost');
    readln(conn^.sin);
    writeln(conn^.sout, 'AUTH LOGIN');
    readln(conn^.sin);
    writeln(conn^.sout, base64_encode(username));
    readln(conn^.sin);
    writeln(conn^.sout, base64_encode(password));
    readln(conn^.sin);
  end
    else
  begin
    writeln(conn^.sout, 'HELO ' + selfaddr);
    //writeln(conn^.sout, 'HELO localhost');
    readln(conn^.sin);
  end;
  // Linking to result
  result := SMTPConnection(conn);
end;


// Frees memory occupied by message handle
procedure smtp_free(mp: SMTPMessage);
var
  msg: PSMTPMessage;
begin
  msg := PSMTPMessage(mp);
  dispose(msg);
end;


// Returns value of already assigned message header
function smtp_get_header(mp: SMTPMessage; const name: string): string;
var msg: PSMTPMessage;
    i: longword;
begin
  msg := PSMTPMessage(mp);
  result := '';
  if length(msg^.headers) > 0 then
  for i := 0 to length(msg^.headers) - 1 do if upcase(msg^.headers[i].name) = upcase(name) then
  begin
    result := msg^.headers[i].value;
    break;
  end;
end;


// Creates new SMTP message from minimal set of parameters
function smtp_message(const ffrom, fto, fsubject, message: string): SMTPMessage;
var
  msg: PSMTPMessage;
begin
  // Init
  result := nil;
  // Checking
  if (ffrom = '') or (fto = '') or (fsubject = '') or (message = '') then exit(nil);
  new(msg);
  // Setting initial headers and body
  msg^.mBody := message;
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'From';
  msg^.headers[length(msg^.headers) - 1].value := ffrom;
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'To';
  msg^.headers[length(msg^.headers) - 1].value := fto;
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'Subject';
  msg^.headers[length(msg^.headers) - 1].value := fsubject;
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'MIME-Version';
  msg^.headers[length(msg^.headers) - 1].value := '1.0';
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'Content-Type';
  msg^.headers[length(msg^.headers) - 1].value := 'text/plain; charset=us-ascii';
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'Content-Transfer-Encoding';
  msg^.headers[length(msg^.headers) - 1].value := '7bit';
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'X-Priority';
  msg^.headers[length(msg^.headers) - 1].value := '3 (Normal)';
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := 'X-Mailer';
  msg^.headers[length(msg^.headers) - 1].value := 'Pascal WU SMTP MAILER';
  // Boundary generation
  msg^.mBoundary := build_boundary;
  // Result linking
  result := SMTPMessage(msg);
end;


// Sends a message
function smtp_send(cp: SMTPConnection; mp: SMTPMessage): boolean;
var
  conn: PSMTPConnection;
  msg: PSMTPMessage;
  MFROM, RCPT, buff: string;
  i: longword;
begin
  result := false;
  // Casts
  conn := PSMTPConnection(cp);
  msg := PSMTPMessage(mp);
  // Detecting MAIL FROM and RCPT addresses
  if length(msg^.headers) = 0 then exit(false);
  for i := 0 to length(msg^.headers) - 1 do if msg^.headers[i].name = 'From' then
  begin
    MFROM := msg^.headers[i].value;
    break;
  end;
  for i := 0 to length(msg^.headers) - 1 do if msg^.headers[i].name = 'To' then
  begin
    RCPT := msg^.headers[i].value;
    break;
  end;
  if (pos('<', MFROM) > 0) and (pos('>', MFROM) > 0) then
      MFROM := copy(MFROM, pos('<', MFROM) + 1, (pos('>', MFROM) - pos('<', MFROM) - 1));
  if (pos('<', RCPT) > 0) and (pos('>', RCPT) > 0) then
      RCPT := copy(RCPT, pos('<', RCPT) + 1, (pos('>', RCPT) - pos('<', RCPT) - 1));
  // Checking Content-Transfer-Encoding
  for i := 0 to length(msg^.headers) - 1 do if msg^.headers[i].name = 'Content-Transfer-Encoding' then
  begin
    buff := msg^.headers[i].value;
    break;
  end;
  if pos('us-ascii', buff) = 0 then
  begin
    for i := 0 to length(msg^.headers) - 1 do if msg^.headers[i].name = 'Content-Transfer-Encoding' then
    begin
        msg^.headers[i].value := '8bit';
        break;
    end;
  end;
  // Sending source and recipient
  writeln(conn^.sout, 'MAIL FROM:<' + MFROM + '>');
  readln(conn^.sin);
  writeln(conn^.sout, 'RCPT TO:<' + RCPT + '>');
  readln(conn^.sin);
  // Sending headers
  writeln(conn^.sout, 'DATA');
  readln(conn^.sin);
  for i := 0 to length(msg^.headers) - 1 do writeln(conn^.sout, msg^.headers[i].name + ': ' + msg^.headers[i].value);
  // CRLF
  writeln(conn^.sout);
  // Sending message body
  if length(msg^.attach) > 0 then
  begin
    // Multipart message
    // Message text
    writeln(conn^.sout, '--' + msg^.mBoundary);
    writeln(conn^.sout, 'Content-Type: ' + msg^.mType);
    for i := 0 to length(msg^.headers) - 1 do
    begin
      if msg^.headers[i].name = 'Content-Transfer-Encoding' then
      begin
        writeln(conn^.sout, 'Content-Transfer-Encoding: ' + msg^.headers[i].value);
        break;
      end;
    end;
    writeln(conn^.sout);
    writeln(conn^.sout, msg^.mBody);
    writeln(conn^.sout);
    // Attachments
    for i := 0 to length(msg^.attach) - 1 do
    begin
      writeln(conn^.sout, msg^.aheadr[i]);
      writeln(conn^.sout);
      writeln(conn^.sout, msg^.attach[i]);
    end;
    writeln(conn^.sout);
    // Final boundary
    writeln(conn^.sout, '--' + msg^.mBoundary + '--');
  end
    else
  begin
    // Normal message
    writeln(conn^.sout, msg^.mBody);
  end;
  // Dot on empty line = DATA end
  writeln(conn^.sout, '.');
  readln(conn^.sin);
  // Done
  result := true;
end;


// Sets custom message header
procedure smtp_set_header(mp: SMTPMessage; const name, value: string);
var
  msg: PSMTPMessage;
  i: longword;
begin
  msg := PSMTPMessage(mp);
  // Changing value if already set
  if length(msg^.headers) > 0 then
  for i := 0 to length(msg^.headers) - 1 do if upcase(msg^.headers[i].name) = upcase(name) then
  begin
    msg^.headers[i].value := value;
    exit;
  end;
  // Or setting new header
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := name;
  msg^.headers[length(msg^.headers) - 1].value := value;
end;


// Sets text charset (for multipart message only)
procedure smtp_set_text_type(mp: SMTPMessage; const ctype: string);
var
  msg: PSMTPMessage;
begin
  msg := PSMTPMessage(mp);
  msg^.mType := ctype;
end;


// Puts message header as is (Name: Value)
procedure smtp_put_header(mp: SMTPMessage; const header: string);
var
  msg: PSMTPMessage;
  i: longword;
  nv: StrArray;
begin
  msg := PSMTPMessage(mp);
  // Splitting into name=value pair
  nv := substr_split(header, ':');
  if length(nv) <> 2 then exit;
  nv[0] := str_trim(nv[0]);
  nv[1] := str_trim(nv[1]);
  // Changing value if already set
  if length(msg^.headers) > 0 then
  for i := 0 to length(msg^.headers) - 1 do if upcase(msg^.headers[i].name) = upcase(nv[0]) then
  begin
    msg^.headers[i].value := nv[1];
    exit;
  end;
  // Or setting new header
  SetLength(msg^.headers, length(msg^.headers) + 1);
  msg^.headers[length(msg^.headers) - 1].name := nv[0];
  msg^.headers[length(msg^.headers) - 1].value := nv[1];
end;

//  END OF PUBLIC PROCEDURES/FUNCTIONS
{------------------------------------------------------------------------------}



end.
