{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  PWU helper unit for gaining access to the CGI Environment variables easily

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  Notes:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   Question:
     Why not just make a general function to get any environment variable on
     the fly, such as GetCGIEnvBar('HTTP_REFERER') ?

   Answer:
     It is a lot CLEANER and more READABLE in your code to use:

       webwriteln(CGIEnvVar.Referer);

     Compared to something ugly like this:

       webwriteln(GetCGIEnvBar('HTTP_REFERER'));


     Also, hopefully there are some speed increases by doing it individually
     with CGIEnvVar versus CGIEnvVars.
     
     Note the difference between CGIEnvVar and CGIEnvVars
     
     You have a general purpose record available, which has all your variables
     held in one record called 'CGIEnvVars'. But you first must call
     GetCGIEnvVars in order to use it.

     However, with 'CGIEnvVar', you just go right ahead and use it without
     needing to call any 'get' function.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  Contributors:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    - Lars (L505)
      Website: http://z505.com
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}

unit pwuEnvVar;

{$mode objfpc}{$H+}

{------------------------------------------------------------------------------}
interface
{------------------------------------------------------------------------------}

uses
  pwu;


type
// all the cgi environment variables into one record. Can get them individually,
// or all at once
  TCGIEnvVars = record
    AuthType,
    ContentLength,
    ContentType,
    DocumentRoot,
    HttpAccept,
    PathInfo,
    PathTranslated,
    QueryString,        // The URL variables
    Referer,            // The referer URL
    RemoteAddr,         // The IP address of the visitor
    RemoteHost,
    RemoteIdent,
    RemoteUser,
    RequestMethod,      // Method of request, usually returns GET
    ScriptName,         // The current script name and path of the webpage (like SELF or paramstr(0))
    ServerProtocol,     // Protocol used, usually returns HTTP
    ServerPort,         // Port of server, usually returns port 80
    UserAgent: string;
  end;

// get cgi environment variables into a record all at once
// (not as fast/effecient as single calls below)
function GetCGIEnvVars: TCGIEnvVars;

// single calls...
function GetCGIAuthType: string;

function GetCGIContentLength: string;

function GetCGIContentType: string;

function GetCGIDocumentRoot: string;

function GetCGIHttpAccept: string;

function GetCGIPathInfo: string;

function GetCGIPathTranslated: string;

function GetCGIQueryString: string;

function GetCGIReferer: string;

function GetCGIRemoteAddr: string;

function GetCGIRemoteHost: string;

function GetCGIRemoteIdent: string;

function GetCGIRemoteUser: string;

function GetCGIRequestMethod: string;

function GetCGIScriptName: string;

function GetCGIServerProtocol: string;

function GetCGIServerPort: string;

function GetCGIUserAgent: string;


var
  // public variable so developer does not need to declare it in his program
  CGIEnvVars: TCGIEnvVars;



{------------------------------------------------------------------------------}
implementation
{------------------------------------------------------------------------------}


  
{------------------------------------------------------------------------------}
{-- PUBLIC FUNCTIONS ----------------------------------------------------------}
{------------------------------------------------------------------------------}


function GetCGIAuthType: string;
begin
  result:= pwu.GetEnvVar('AUTH_TYPE');
end;

function GetCGIContentLength: string;
begin
  result:= pwu.GetEnvVar('CONTENT_LENGTH');
end;

function GetCGIContentType: string;
begin
  result:= pwu.GetEnvVar('CONTENT_TYPE');
end;

function GetCGIDocumentRoot: string;
begin
  result:= pwu.GetEnvVar('DOCUMENT_ROOT');
end;

function GetCGIHttpAccept: string;
begin
  result:= pwu.GetEnvVar('HTTP_ACCEPT');
end;

function GetCGIPathInfo: string;
begin
  result:= pwu.GetEnvVar('PATH_INFO');
end;

function GetCGIPathTranslated: string;
begin
  result:= pwu.GetEnvVar('PATH_TRANSLATED');
end;

function GetCGIQueryString: string;
begin
  result:= pwu.GetEnvVar('QUERY_STRING');
end;

function GetCGIReferer: string;
begin
  result:= pwu.GetEnvVar('HTTP_REFERER');
end;

function GetCGIRemoteAddr: string;
begin
  result:= pwu.GetEnvVar('REMOTE_ADDR');
end;

function GetCGIRemoteHost: string;
begin
  result:= pwu.GetEnvVar('REMOTE_HOST');
end;

function GetCGIRemoteIdent: string;
begin
  result:= pwu.GetEnvVar('REMOTE_IDENT');
end;

function GetCGIRemoteUser: string;
begin
  result:= pwu.GetEnvVar('REMOTE_USER');
end;

function GetCGIRequestMethod: string;
begin
  result:= pwu.GetEnvVar('REQUEST_METHOD');
end;

function GetCGIScriptName: string;
begin
  result:= pwu.GetEnvVar('SCRIPT_NAME');
end;

function GetCGIServerProtocol: string;
begin
  result:= pwu.GetEnvVar('SERVER_PROTOCOL');
end;

function GetCGIServerPort: string;
begin
  result:= pwu.GetEnvVar('SERVER_PORT');
end;

function GetCGIUserAgent: string;
begin
  result:= pwu.GetEnvVar('HTTP_USER_AGENT');
end;



function GetCGIEnvVars: TCGIEnvVars;
begin
  result.AuthType:= pwu.GetEnvVar('AUTH_TYPE');
  result.ContentLength:= pwu.GetEnvVar('CONTENT_LENGTH');
  result.ContentType:= pwu.GetEnvVar('CONTENT_TYPE');
  result.DocumentRoot:= pwu.GetEnvVar('DOCUMENT_ROOT');
  result.HttpAccept:= pwu.GetEnvVar('HTTP_ACCEPT');
  result.PathInfo:= pwu.GetEnvVar('PATH_INFO');
  result.PathTranslated:= pwu.GetEnvVar('PATH_TRANSLATED');
  result.QueryString:= pwu.GetEnvVar('QUERY_STRING');
  result.Referer:= pwu.GetEnvVar('HTTP_REFERER');
  result.RemoteAddr:= pwu.GetEnvVar('REMOTE_ADDR');
  result.RemoteHost:= pwu.GetEnvVar('REMOTE_HOST');
  result.RemoteIdent:= pwu.GetEnvVar('REMOTE_IDENT');
  result.RequestMethod:= pwu.GetEnvVar('REQUEST_METHOD');
  result.RemoteUser:= pwu.GetEnvVar('REMOTE_USER');
  result.ScriptName:= pwu.GetEnvVar('SCRIPT_NAME');
  result.ServerPort:= pwu.GetEnvVar('SERVER_PORT');
  result.ServerProtocol:= pwu.GetEnvVar('SERVER_PROTOCOL');
  result.UserAgent:= pwu.GetEnvVar('HTTP_USER_AGENT');
end;

// END OF PUBLIC FUNCTIONS
{------------------------------------------------------------------------------}




end.

