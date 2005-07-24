{$H+}{$MODE OBJFPC}
unit web_mail;
{
 *******************************************************************************
 *                -== Pascal Server Pages class - Web_Mail ==-                 *
 *******************************************************************************
 * This class provides easy mail sending from PSP programs                     *             
 *******************************************************************************
 * See the Pascal Server Pages Documentation for more information.             *
 *******************************************************************************
 *   Written by Vladimir Sibirov a.k.a. Trustmaster                            *
 *   http://www.psp.furtopia.org                                               *
 *   mailto:psp@furtopia.org                                                   *
 *******************************************************************************
 * Copyright (c) 2003-2004 by Pascal Server Pages development team.            *
 * See the Pascal Server Pages License for more information.                   *
 *******************************************************************************
}

{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface

uses sockets, sysutils, classes, base64;

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function Web_SMTPMail(msgTo,msgFrom,msgBody,Username,Password:string):string;
// Mailing function which connects to SMTP.

{==============================================================================}
{=================================== Classes ==================================}
{==============================================================================}


type Web_TMail = Class(TObject)
    private
	// Fields and Variables
	FTo, FFrom, FCc, FReplyTo, FSubject, FText, FType, FCharset, FOrg, FUsername, FPassword: string;
	FPriority: integer;
	FAttach: array of string;
	AHeaders: array of string;
	MBody, MEncoding, MPriority, MBoundary, MType: string;
	MWithAttachments: boolean;
	// Private property functions
	procedure SetTo(value: string);
	function GetTo: string;
	procedure SetFrom(value: string);
	function GetFrom: string;
	procedure SetCc(value: string);
	function GetCc: string;
	procedure SetReplyTo(value: string);
	function GetReplyTo: string;
	procedure SetSubject(value: string);
	function GetSubject: string;
	procedure SetText(value: string);
	function GetText: string;
	procedure SetType(value: string);
	function GetType: string;
	procedure SetCharset(value: string);
	function GetCharset: string;
	procedure SetPriority(value: integer);
	function GetPriority: integer;
	procedure SetOrg(value: string);
	function GetOrg: string;
	procedure SetUsername(value: string);
	function GetUsername: string;
	procedure SetPassword(value: string);
	function GetPassword: string;
	// Private functions
	function BuildBoundary: string;
    public
	// Public properties
	property SendTo : string read GetTo write SetTo;
	property From : string read GetFrom write SetFrom;
	property Cc : string read GetCc write SetCc;
	property ReplyTo : string read GetReplyTo write SetReplyTo;
	property Subject : string read GetSubject write SetSubject;
	property Text : string read GetText write SetText;
	property ContentType : string read GetType write SetType;
	property Charset : string read GetCharset write SetCharset;
	property Priority : integer read GetPriority write SetPriority;
	property Organization : string read GetOrg write SetOrg;
	property Username : string read GetUsername write SetUsername;
	property Password : string read GetPassword write SetPassword;
	// Constructor
	constructor Create;
	// Public methods
	function Attach(FileName:string):boolean;
	function Send:string;
end; 

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{--[ EncodeBase64 ]------------------------------------------------------------}
// Encodes string content with Base64 Encoding.
// Written by Michael Van Canneyt

Function EncodeBase64(S: String): String;
Var
  S1,S2 : TStringStream;
begin
  S1:=TStringStream.Create(S);
  Try
    S1.Position:=0;
    S2:=TStringStream.Create('');
    Try
      With TBase64EncodingStream.Create(S2) do
        Try
          CopyFrom(S1,S1.Size);
        Finally
          Free;
        end;
      Result:=S2.DataString;  
    finally
      S2.Free;
    end;
 finally
   S1.Free;
 end;  
end;

{------------------------------------------------------------------------------}

{--[ Web_SMTPMail ]------------------------------------------------------------}
// SMTP mail sending function for Web_TMail Class

function Web_SMTPMail(msgTo, msgFrom, msgBody, Username, Password: string): string;
var sock:longint;
    SIn,SOut:text;
    addr:TInetSockAddr;
    buff:string;
begin
    result := '';
    Addr.family := AF_INET;
    Addr.port := 25 shl 8;
    Addr.addr := ((1 shl 24) or 127);
    Sock := Socket(AF_INET,SOCK_STREAM,0);
    if not connect(Sock,ADDR,SIN,SOUT) then exit;
    reset(SIn);
    rewrite(SOut);
    readln(SIn,buff);
    result := result + buff;
    if Username <> '' then
	begin
	    writeln(SOut,'EHLO localhost');
	    readln(SIn,buff);
	    result := result + buff;
	    writeln(SOut,'AUTH LOGIN');
	    readln(SIn,buff);
	    result := result + buff;
	    writeln(SOut,EncodeBase64(Username));
	    readln(SIn,buff);
	    result := result + buff;
	    writeln(SOut,EncodeBase64(Password));
	    readln(SIn,buff);
	    result := result + buff;
	end
    else
	begin
            writeln(SOut,'HELO localhost');
            readln(SIn,buff);
            result := result + buff;
	end;
    writeln(SOut,'MAIL FROM:<',msgFrom,'>');
    readln(SIn,buff);
    result := result + buff;
    writeln(SOut,'RCPT TO:<',msgTo,'>');
    readln(SIn,buff);
    result := result + buff;
    writeln(SOut,'DATA');
    readln(SIn,buff);
    result := result + buff;
    writeln(SOut,msgBody);
    writeln(SOut,'.');
    readln(SIn,buff);
    result := result + buff;
    writeln(SOut,'QUIT');
    readln(SIn,buff);
    result := result + buff;
    close(SIn);
    close(SOut);
end;

{------------------------------------------------------------------------------}

{--[ Web_TMail.Create ]---------------------------------------------------------}
// Object Constructor

constructor Web_TMail.Create;
begin
    // Assigning default values to fields
    FTo := 'nobody@localhost';
    FFrom := 'nobody@localhost';
    FCc := '';
    FReplyTo := 'nobody@localhost';
    FSubject := 'Untitled';
    FText := '';
    FType := 'text/plain';
    FCharset := 'us-ascii';
    FOrg := '';
    FPriority := 3;
    // Empty message parts
    MBody := '';
    MPriority := '';
    MType := 'text/plain';
    MEncoding := '7bit';
    MWithAttachments := false;
    // Generating message boundary
    MBoundary := BuildBoundary;
    // Setting length of dynamic arrays
    SetLength(FAttach,0);
    SetLength(AHeaders,0);
    inherited Create;
end;

{------------------------------------------------------------------------------}

{--[ Web_TMail.Destroy ]--------------------------------------------------------}
// Object Destructor

{destructor Web_TMail.Destroy;
begin
    // Automatic finalization
    inherited Destroy;
end;}

{------------------------------------------------------------------------------}

{--[ Web_TMail.SetTo ]----------------------------------------------------------}
// Sets To address

procedure Web_TMail.SetTo(value:string);
begin
    if pos('@',value)>0 then FTo := value;
end;

{--[ Web_TMail.GetTo ]----------------------------------------------------------}
// Returns To address

function Web_TMail.GetTo:string;
begin
    result := FTo;
end;

{--[ Web_TMail.SetFrom ]--------------------------------------------------------}
// Sets From address

procedure Web_TMail.SetFrom(value:string);
begin
    if pos('@',value)>0 then FFrom := value;
end;

{--[ Web_TMail.GetFrom ]--------------------------------------------------------}
// Returns From address

function Web_TMail.GetFrom:string;
begin
    result := FFrom;
end;

{--[ Web_TMail.SetCc ]----------------------------------------------------------}
// Sets Copy address

procedure Web_TMail.SetCc(value:string);
begin
    if pos('@',value)>0 then FCc := value;
end;

{--[ Web_TMail.GetCc ]----------------------------------------------------------}
// Returns Copy address

function Web_TMail.GetCc:string;
begin
    result := FCc;
end;

{--[ Web_TMail.SetReplyTo ]-----------------------------------------------------}
// Sets Reply-To address

procedure Web_TMail.SetReplyTo(value:string);
begin
    if pos('@',value)>0 then FReplyTo := value
    else FReplyTo := FFrom;
end;

{--[ Web_TMail.GetReplyTo ]-----------------------------------------------------}
// Returns Reply-To address

function Web_TMail.GetReplyTo:string;
begin
    result := FReplyTo;
end;

{--[ Web_TMail.SetSubject ]-----------------------------------------------------}
// Sets message subject

procedure Web_TMail.SetSubject(value:string);
begin
    FSubject := value;
end;

{--[ Web_TMail.GetSubject ]-----------------------------------------------------}
// Returns message subject

function Web_TMail.GetSubject:string;
begin
    result := FSubject;
end;

{--[ Web_TMail.SetText ]--------------------------------------------------------}
// Sets message text

procedure Web_TMail.SetText(value:string);
begin
    FText := value;
end;

{--[ Web_TMail.GetText ]--------------------------------------------------------}
// Returns message text

function Web_TMail.GetText:string;
begin
    result := FText;
end;

{--[ Web_TMail.SetType ]--------------------------------------------------------}
// Sets message Content-Type

procedure Web_TMail.SetType(value:string);
begin
    if pos('/',value)>0 then FType := value;
end;

{--[ Web_TMail.GetType ]--------------------------------------------------------}
// Returns message Content-Type

function Web_TMail.GetType:string;
begin
    result := FType;
end;

{--[ Web_TMail.SetCharset ]-----------------------------------------------------}
// Sets message charset

procedure Web_TMail.SetCharset(value:string);
begin
    FCharset := value;
end;

{--[ Web_TMail.GetCharset ]-----------------------------------------------------}
// Returns message charset

function Web_TMail.GetCharset:string;
begin
    result := FCharset;
end;

{--[ Web_TMail.SetPriority ]----------------------------------------------------}
// Sets message priority

procedure Web_TMail.SetPriority(value:integer);
begin
    if (value<6) and (value>0) then FPriority := value;
end;

{--[ Web_TMail.GetPriority ]----------------------------------------------------}
// Returns message priority

function Web_TMail.GetPriority:integer;
begin
    result := FPriority;
end;

{--[ Web_TMail.SetOrg ]---------------------------------------------------------}
// Sets message Organization

procedure Web_TMail.SetOrg(value:string);
begin
    FOrg := value;
end;

{--[ Web_TMail.GetOrg ]---------------------------------------------------------}
// Returns message Organization

function Web_TMail.GetOrg:string;
begin
    result := FOrg;
end;

{--[ Web_TMail.SetUsername ]---------------------------------------------------}
// Sets username for SMTP Auth

procedure Web_TMail.SetUsername(value:string);
begin
    FUsername := value;
end;

{--[ Web_TMail.GetUsername ]---------------------------------------------------}
// Gets username for SMTP Auth

function Web_TMail.GetUsername:string;
begin
    result := FUsername;
end;

{--[ Web_TMail.SetPassword ]---------------------------------------------------}
// Sets password for SMTP Auth

procedure Web_TMail.SetPassword(value:string);
begin
    FPassword := value;
end;

{--[ Web_TMail.GetPassword ]---------------------------------------------------}
// Gets username for SMTP Auth

function Web_TMail.GetPassword:string;
begin
    result := FPassword;
end;

{------------------------------------------------------------------------------}

{--[ Web_TMail.BuildBoundary ]--------------------------------------------------}
// Generates message boundary

function Web_TMail.BuildBoundary:string;
const M_BOUNDARY_LENGTH = 20;
var bchar,i:byte;
begin
    randomize;
    result := '';
    for i := 1 to M_BOUNDARY_LENGTH do
	begin
	    repeat
		bchar := random(127);
	    until chr(bchar)<>'"';
	    result := result + chr(bchar);
	end;
end;

{------------------------------------------------------------------------------}

{--[ Web_TMail.Attach ]---------------------------------------------------------}
// Attaches file to the message

function Web_TMail.Attach(FileName: string): boolean;
var basename, filetype, disposition, ext: string;
    fsize, i, item: longint;
    fh: file of char;
    c: char;
begin
    if not FileExists(FileName) then exit(false);
    ext := ExtractFileExt(FileName);
    filetype := 'application/x-unknown-content-type';
    disposition := 'attachment';
    if (ext='.txt') or (ext='.TXT') then begin filetype:='text/plain'; disposition:='inline'; end;
    if (ext='.gif') or (ext='.GIF') then begin filetype:='image/gif'; disposition:='attachment'; end;
    if (ext='.jpg') or (ext='.JPG') then begin filetype:='image/jpeg'; disposition:='attachment'; end;
    if (ext='.png') or (ext='.PNG') then begin filetype:='image/png'; disposition:='attachment'; end;
    if (ext='.zip') or (ext='.ZIP') then begin filetype:='application/zip'; disposition:='attachment'; end;
    if (ext='.gz') or (ext='.GZ') or (ext='tgz') or (ext='TGZ') or (ext='bz2') or (ext='BZ2') then begin filetype:='application/x-tar'; disposition:='attachment'; end;
    if (ext='.rar') or (ext='.RAR') then begin filetype:='application/x-rar-compressed'; disposition:='attachment'; end;
    if (ext='.mp3') or (ext='.MP3') then begin filetype:='audio/mpeg'; disposition:='attachment'; end;
    if (ext='.wav') or (ext='.WAV') then begin filetype:='audio/wav'; disposition:='attachment'; end;
    if (ext='.html') or (ext='.htm') or (ext='.shtml') or (ext='.phtml') or (ext='.xhtml') then begin filetype:='text/html'; disposition:='inline'; end;
    if (ext='.css') or (ext='.CSS') then begin filetype:='text/css'; disposition:='attachment'; end;
    if (ext='.js') or (ext='.JS') or (ext='.jsc') or (ext='.JSC') then begin filetype:='text/javascript'; disposition:='attachment'; end;
    if (ext='.exe') or (ext='.EXE') then begin filetype:='application/x-msdownload'; disposition:='attachment'; end;
    basename := ExtractFileName(FileName);
    SetLength(FAttach,length(FAttach)+1);
    SetLength(AHeaders,length(AHeaders)+1);
    item := length(AHeaders)-1;
    MType := 'multipart/mixed;'+#13+#10+' boundary="'+MBoundary+'"';
    MWithAttachments := true;
    AHeaders[item] := '--'+MBoundary+#13+#10+'Content-Type: '+filetype+'; name="'+basename+'"'+#13+#10+'Content-Transfer-Encoding: base64'+#13+#10+'Content-Disposition: '+disposition+'; filename="'+basename+'"'+#13+#10;
    assign(fh,FileName);
    reset(fh);
    fsize:=FileSize(fh);
    for i := 1 to fsize do
	begin
	    read(fh,c);
	    FAttach[item] := FAttach[item] + c;
	end;
    close(fh);
    FAttach[item] := EncodeBase64(FAttach[item]);
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ Web_TMail.Send ]----------------------------------------------------------}
// Sends this e-mail

function Web_TMail.Send:string;
var MPriority: string;
    i, len: longint;
    MFROM, RCPT: string;
begin
    if (pos('<',FFrom)>0) and (pos('>',FFrom)>0) then MFROM := copy(FFrom, pos('<',FFrom)+1, (pos('>',FFrom)-pos('<',FFrom)-1));
    if (pos('<',FTo)>0) and (pos('>',FTo)>0) then RCPT := copy(FTo, pos('<',FTo)+1, (pos('>',FTo)-pos('<',FTo)-1));
    MBody := 'From: ' + FFrom +#13+#10;
    MBody := MBody + 'X-Mailer: PSP Web Mail' +#13+#10;
    MBody := MBody + 'Reply-To: ' + FReplyTo +#13+#10;
    case FPriority of
	1: MPriority := '1 (Highest)';
	2: MPriority := '2 (High)';
	3: MPriority := '3 (Normal)';
	4: MPriority := '4 (Low)';
	5: MPriority := '5 (Lowest)';
    else
	MPriority := '3 (Normal)';
    end;
    if FCharset='us-ascii' then MEncoding := '7bit' else MEncoding := '8bit';
    MBody := MBody + 'X-Priority: ' + MPriority +#13+#10;
    MBody := MBody + 'To: ' + FTo +#13+#10;
    if FCc<>'' then MBody := MBody + 'CC: ' + FCc +#13+#10;
    MBody := MBody + 'Subject: ' + FSubject +#13+#10;
    MBody := MBody + 'MIME-Version: 1.0' +#13+#10;
    if MWithAttachments then MBody := MBody + 'Content-Type: ' + MType +#13+#10
    else MBody := MBody + 'Content-Type ' + FType + '; charset=' + FCharset +#13+#10+ 'Content-Transfer-Encoding: ' + MEncoding + #13+#10 + #13+#10;
    if MWithAttachments then
	begin
	    MBody := MBody + '--' + MBoundary +#13+#10;
	    MBody := MBody + 'Content-Type: ' + FType + '; charset=' + FCharset +#13+#10;
	    MBody := MBody + 'Content-Transfer-Encoding: ' + MEncoding +#13+#10 +#13+#10;	
	end;
    MBody := MBody + FText +#13+#10;
    if MWithAttachments then
	begin
	    len := length(FAttach)-1;
	    for i := 0 to len do
		begin
		    MBody := MBody + AHeaders[i] +#13+#10;
		    MBody := MBody + FAttach[i] +#13+#10;
		end;
	    MBody := MBody + '--' + MBoundary + '--';
	end;
    result := Web_SMTPMail(RCPT,MFROM,MBody,FUsername,FPassword);
end;

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
