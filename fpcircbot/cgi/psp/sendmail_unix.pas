{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                       Pascal Web Unit project (PWU)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 ------------------------------------------------------------------------------
  SendMail Unit
 ------------------------------------------------------------------------------

  This unit contains functions for use with SendMail. Most servers have
  SendMail installed, so using these functions is convenient. If SendMail
  is not installed, the general purpose mail functions in other units
  which connect to port 25 will work for any standard mail server.
  Since this unit currently uses AssignStream, it should only work on
  Unix/Linux servers. See SMTP unitss for windows email functions

 ------------------------------------------------------------------------------
  Developer Notes
 ------------------------------------------------------------------------------

  [ 11/OCT/2005 -L505 ]

   -created unit

 ------------------------------------------------------------------------------
  Developer Todo
 ------------------------------------------------------------------------------

 ------------------------------------------------------------------------------
  Contributors/Authors:
 ------------------------------------------------------------------------------
   Lars (L505)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}


unit SendMail_Unix;

{$mode objfpc}{$H+}

{------------------------------------------------------------------------------}
interface
{------------------------------------------------------------------------------}

uses
  pwu,
  pwuMailPrep, //mail preparation
  unix,
  sysutils;

const
  SENDMAIL_PATH ='/usr/sbin/sendmail'; //make this a PWU config file var

type
  TEMailPriority = set of (Normal, High, Low);
  
  TEMailType = set of (HTML, TextPlain);
  
  TEmailAttachment = record
    FileName: string; //name of file
    FileType: string; //content/mime type
    Disposition: set of (RegularAttach, InlineAttach); //attached separately or inline attachment
  end;
  
  TEmailAttachments = array of TEmailAttachment;

var
  PWUEmailType: TEmailType; // public/global variable that let's the user
                            // specify the email type on the fly.
                            // i.e. html emails or plain text


{------------------------------------------------------------------------------}
{--------- PUBLIC PROCEDURE/FUNCTION DECLARATIONS -----------------------------}
{------------------------------------------------------------------------------}

  function WebSendMail(EmailFrom: string;
                       EmailTo: string;
                       Subject: string;
                       MessageText:string): boolean;



  function WebSendMail_P(Priority: TEMailPriority;
                         EmailFrom: string;
                         EmailTo: string;
                         Subject: string;
                         MessageText: string): boolean;




  function WebSendMail_PA(Priority: TEMailPriority;
                          AttachedFiles: TEmailAttachments;
                          EmailFrom: string;
                          EmailTo: string;
                          Subject: string;
                          MessageText: string): boolean;




//  END OF PUBLIC PROCEDURE/FUNCTION DECLARATIONS
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
implementation
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
{--------- PRIVATE PROCEDURES/FUNCTIONS ---------------------------------------}
{------------------------------------------------------------------------------}

procedure WriteContentType(var sout: text);
begin
  if PWUEmailType = [] then //default email is plain text
    writeln(sout, 'Content-Type: text/plain');
  if PWUEmailType = [HTML] then //default email is plain text
    writeln(sout, 'Content-Type: text/html');
  if PWUEmailType = [TextPlain] then //default email is plain text
    writeln(sout, 'Content-Type: text/plain');
end;


//  END OF PRIVATE PROCEDURES/FUNCTIONS
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
{--------- PUBLIC PROCEDURES/FUNCTIONS ----------------------------------------}
{------------------------------------------------------------------------------}


// sends an email by sendmail, with default priority (normal).
// email should be written in plain text
//
// Returns true if the mail was sent for delivery attempt, false if error
// opening sendmail. True does not indicate the mail went through, because
// it does not check for bad addresses or failed deliveries
function WebSendMail(EmailFrom: string;
                     EmailTo: string;
                     Subject: string;
                     MessageText:string): boolean;
var
  SIN,
  SOUT:text;
  Args: array of string;
begin
  result:= false;
  setlength(args, 1);
  Args[0]:= ' -t ';    //tells sendmail to parse for TO, FROM, CC, BCC in text

  if AssignStream(SIN, SOUT, SENDMAIL_PATH, Args) <> -1 then
  begin
    writeln(sout, 'MIME-Version: 1.0');                // header
    writeln(sout, 'X-Priority: 3 (Normal)');           //...priority normal
    writeln(sout, 'X-Mailer: Pascal WU SendMail');
{    if PWUEmailType = [] then //default email is plain text
      writeln(sout, 'Content-Type: text/plain');
    if PWUEmailType = [HTML] then //default email is plain text
      writeln(sout, 'Content-Type: text/html');
    if PWUEmailType = [TextPlain] then //default email is plain text
      writeln(sout, 'Content-Type: text/plain');}
    WriteContentType(sout);

    writeln(sout, 'FROM: ' + EmailFrom);
    writeln(sout, 'TO: ' + EmailTo);
    writeln(sout, 'SUBJECT: ' + Subject);
    writeln(sout, MessageText);

    writeln(sout, '.'); //end connection

    result:= true;
  end else
  begin
    throwweberror('AssignStream returned error -1');
    result:= false;
  end;

  close(sin);
  close(sout);
end;


// sends an email by sendmail, with the ability to specify priority, BCC, CCC.
// Use this you need to specify many settings of your e-mail, otherwise use the
// above functions. Email message should be written in plain text
//
// Returns true if the mail was sent for delivery attempt, false if error
// opening sendmail
function WebSendMail_P(Priority: TEMailPriority;
                       EmailFrom: string;
                       EmailTo: string;
                       Subject: string;
                       MessageText: string): boolean;
var
  SIN,
  SOUT:text;
  Args: array of string;
  line: string;
begin
  result:= false;
  setlength(args, 1);
  Args[0]:= '-t';    //tells sendmail to parse for TO, FROM, CC, BCC in text

  if AssignStream(SIN, SOUT, SENDMAIL_PATH, Args) <> -1 then
  begin
    writeln(sout, 'MIME-Version: 1.0');                // header
    if priority = [normal] then
      writeln(sout, 'X-Priority: 3 (Normal)');         //...priority normal
    if priority = [low] then
      writeln(sout, 'X-Priority: 5 (Low)');            //...priority low
    if priority = [high] then
      writeln(sout, 'X-Priority: 1 (High)');           //...priority high
    writeln(sout, 'X-Mailer: Pascal WU SendMail');

//    writeln(sout, 'Content-Type: text/plain');
    WriteContentType(sout);

    writeln(sout, 'FROM: ' + EmailFrom);               // from
    writeln(sout, 'TO: ' + EmailTo);                   // to
    writeln(sout, 'SUBJECT: ' + Subject);              // subject
    writeln(sout);
    writeln(sout, MessageText);                        // message

    writeln(sout, '.'); //end connection
    result:= true;
  end else
  begin
    throwweberror('AssignStream returned error -1');
    result:= false;
  end;

  close(sin);
  close(sout);

end;


// sends an email by sendmail, with the ability to specify priority and attach
// files. Use this if you need to specify the priority and attach files.
// function. Email message should be written in plain text
//
// Returns true if the mail was sent for delivery attempt, false if error
// opening sendmail
function WebSendMail_PA(Priority: TEMailPriority;
                        AttachedFiles: TEmailAttachments;
                        EmailFrom: string;
                        EmailTo: string;
                        Subject: string;
                        MessageText: string): boolean;
var
  SIN,
  SOUT:text;
  Args: array of string;
  TheDisposition: string;
  TheAttachedFiles: TEmailAttachments;
  iLoc: integer; //local loop integer
  BoundaryDivider: string;
begin
  result:= false;
  TheAttachedFiles:= AttachedFiles;
  setlength(args, 1);
  Args[0]:= '-t';    //tells sendmail to parse for TO, FROM, CC, BCC in text

  BoundaryDivider:= '_Divider_' + PrepEmailBoundary;
  
  if AssignStream(SIN, SOUT, SENDMAIL_PATH, Args) <> -1 then
  begin
    writeln(sout, 'MIME-Version: 1.0');                // header
    writeln(sout, 'FROM: ' + EmailFrom);               // from
    writeln(sout, 'TO: ' + EmailTo);                   // to
    writeln(sout, 'SUBJECT: ' + Subject);              // subject
    if priority = [normal] then
      writeln(sout, 'X-Priority: 3 (Normal)');         //...priority normal
    if priority = [low] then
      writeln(sout, 'X-Priority: 5 (Low)');            //...priority low
    if priority = [high] then
      writeln(sout, 'X-Priority: 1 (High)');           //...priority high
    writeln(sout, 'X-Mailer: Pascal WU SendMail');
    writeln(sout, 'Content-Type: multipart/mixed;');
    writeln(sout, ' boundary="----=' + BoundaryDivider + '"');

    writeln(sout);
    writeln(sout, 'This is a multi-part message in MIME format.');
    writeln(sout);

    // message body text : write it
    writeln(sout, '------=' + BoundaryDivider);
//    writeln(sout, 'Content-Type: text/plain;');
    WriteContentType(sout);
    writeln(sout, ' charset="iso-8859-1"');
    writeln(sout, 'Content-Transfer-Encoding: 7bit');
    writeln(sout); //must have a blank line here or it won't work
    writeln(sout, MessageText);

      // attachments : include them
    if length(TheAttachedFiles) > 0 then //if there are any attached files at all
    begin

      for iLoc:= 0 to length(TheAttachedFiles) - 1 do
      begin
        if fileexists(TheAttachedFiles[iLoc].Filename) = false then
        begin
          ThrowWebError('A file that was attempted to be attached: ' + TheAttachedFiles[iLoc].Filename + ', was not found.' + #13#10 +
                        'Emailing aborted.' + #13#10 +
                        'Remove all SetLength calls if you have not attached any files.' + #13#10 +
                        'If you are attaching files, make sure SetLength specifies the number of files attached.');

          halt;
        end;

        // what type of attachment do we have
        if TheAttachedFiles[iLoc].Disposition = [] then //user didn't specify, so regular attachment
          TheDisposition:= 'attachment';
        if TheAttachedFiles[iLoc].Disposition = [RegularAttach] then
          TheDisposition:= 'attachment';
        if AttachedFiles[iLoc].Disposition = [InlineAttach] then
          TheDisposition:= 'inline';

        writeln(sout, '------=' + BoundaryDivider);
        writeln(sout, 'Content-Type: ' + TheAttachedFiles[iLoc].FileType + ';');
        writeln(sout, ' name="' + TheAttachedFiles[iLoc].FileName + '"');
        writeln(sout, 'Content-Transfer-Encoding: base64');
        writeln(sout, 'Content-Disposition: ' + TheDisposition + '; filename="' + TheAttachedFiles[iLoc].FileName + '"');
        writeln(sout);      //must have a blank line here or it won't work
        
        // write each file attachment's contents
        writeln(sout, PrepFileContents(TheAttachedFiles[iLoc].FileName));
        writeln(sout);
      end;

    end;
    writeln(sout, '------=' + BoundaryDivider);

    writeln(sout, '.'); //kill the running sendmail executable
    result:= true;
  end else
  begin
    ThrowWebError('AssignStream returned error -1');
    result:= false;
  end;

  close(sin);
  close(sout);

end;


//  END OF PUBLIC PROCEDURES/FUNCTIONS
{------------------------------------------------------------------------------}



// testing function, for internal testing, debugging, and prototyping only
function TestTestSendMail: string;
var
  SIN,
  SOUT:text;
  Args: array of string;
begin
  result:= '';
  setlength(args, 1);
  Args[0]:= 'testtest@someplace.com';

  if AssignStream(SIN, SOUT, SENDMAIL_PATH, Args) <> -1 then
  begin
    writeln(sout, 'test test test sendmail_unix test test');
    //  readln(sin);
    writeln(sout, '.'); //end connection
  end else
    ThrowWebError('AssignStream returned error -1');

  close(sin);
  close(sout);



end;




end.

