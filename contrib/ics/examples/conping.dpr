{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This demo shows how to use the TPing object to ping any host.
Creation:     November 30, 1997
Destruction:  August 28, 2003
Version:      1.03-ugly-hack-to-consoleapp-v3.1415
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2002 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.


Updates:
Dec 13, 1997 V1.01 Use the new OnEchoRequest and OnEchoReply events.
Dec 26, 1998 V1.02 Changed event handler for new TPing version (1.10)
Nov 10, 2002 V1.03 Added Reply.Status in display when failed
                   Changed argument name from Error to Status in EchoReply
                   event (same change has in component).
Aug 28, 2003 v1.03-ugly-hack-to-consoleapp-v3.1415
                   (MarcoV) Small patch to console application
                   Currently exit by pressing "any" key not working.
                   (don't know how to do Delphi compat)

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
Program conping;
{$i icsdef.inc}

uses
  Windows, Messages, SysUtils, Classes,
  Ping;


type TPinger=Class(TComponent)
                   Private
                     Ping1    : TPing;
                     Hostname : String;
                     DnsStatus: Boolean;
                   public
                     constructor Create(AOwner:TComponent); override;
                     destructor Destroy; override;
                     procedure ResolveAndPing(Const HostToPing:String);
                     procedure Ping1Display(Sender: TObject; Icmp: TObject; Msg: String);
                     procedure Ping1DnsLookupDone(Sender: TObject; Error: Word);
                     procedure CancelButtonClick(Sender: TObject);
                     procedure Ping1EchoRequest(Sender: TObject; Icmp: TObject);
                     procedure Ping1EchoReply(Sender: TObject; Icmp: TObject; Status: Integer);
                     procedure Ping;
                     end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.ResolveAndPing(Const HostToPing:String);
begin
    HostName:=HostToPing;
    writeln('Resolving host ''' + HostToPing + '''');
    Ping1.DnsLookup(HostToPing);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.Ping1DnsLookupDone(Sender: TObject; Error: Word);

begin

    if Error <> 0 then begin
        Writeln('Unknown Host ''' + HostName + '''');
        ping1.destroy;  // release resources.
        Halt;
    end;
    DnsStatus:=True;

    writeln('Host ''' + HostName + ''' is ' + Ping1.DnsResult);

    Ping1.Address := Ping1.DnsResult;
    Ping1.Ping;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.Ping1Display(Sender: TObject; Icmp: TObject; Msg: String);
begin
    Writeln(Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.CancelButtonClick(Sender: TObject);
begin
    Ping1.CancelDnsLookup;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.Ping1EchoRequest(Sender: TObject; Icmp: TObject);
begin
    writeln('Sending ' + IntToStr(Ping1.Size) + ' bytes to ' +
                          Ping1.HostName + ' (' + Ping1.HostIP + ')');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.Ping1EchoReply(
    Sender : TObject;
    Icmp   : TObject;
    Status : Integer);
begin
    if Status <> 0 then
        { Success }
        writeln('Received ' + IntToStr(Ping1.Reply.DataSize) +
                              ' bytes from ' + Ping1.HostIP +
                              ' in ' + IntToStr(Ping1.Reply.RTT) + ' msecs')
    else
        { Failure }
        writeln('Cannot ping host (' + Ping1.HostIP + ') : ' +
                              Ping1.ErrorString +
                              '. Status = ' + IntToStr(Ping1.Reply.Status));
end;

procedure TPinger.Ping;

begin
 IF DnsStatus Then
  ping1.ping;
end;

constructor TPinger.Create(AOwner:TComponent);

begin
  inherited Create(AOwner);
  ping1:=TPing.create(self);
  Ping1.OnDnsLookupdone:=ping1dnslookupdone;
  Ping1.OnEchoRequest:=ping1echorequest;
  Ping1.OnEchoReply:=ping1echoreply;
  Hostname :='';
  DnsStatus:=false;
end;


destructor TPinger.Destroy;

begin
  If Assigned(Ping1) Then Begin
      Ping1.destroy;
      Ping1:=NIL;
  End;
  Inherited Destroy;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

var pinger     : TPinger;
    AMessage   : TMSG;
    ExitLoop   : Boolean;
    Counter    : Integer;
    HostToPing : String;

begin
  If ParamCount>0 Then
     HostToPing:=ParamStr(1)
  else begin
      WriteLn('Usage: conping <FQHN>');
      WriteLn;
      Halt;
  end;

  Pinger:=TPinger.Create(nil);
  pinger.ResolveAndPing(HostToPing);
  ExitLoop:=False;
  Counter:=0;
  while NOT ExitLoop do begin
     while PeekMessage(AMessage, 0, 0,0, PM_REMOVE)=true do
        if (AMessage.Message=WM_QUIT) or (AMessage.Message=WM_keydown) then
           ExitLoop:=true
        else
           begin
              TranslateMessage(AMessage);
              DispatchMessage(AMessage);
           end;
     Sleep(50);
     Inc(Counter);
     if counter=21 then begin   { 20*counter=1sec}
        Counter:=0;
        Pinger.ping;
      End;
 end;
 Pinger.Free;
end.

