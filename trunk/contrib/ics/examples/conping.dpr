{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This demo show how to use the TPing object to ping any host.
Creation:     November 30, 1997
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

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
Program conping;

{$i icsdef.inc}

uses
  Windows, Messages, SysUtils, Classes,
  Ping;

Const PINGMEHOST='toad.stack.nl';

type TPinger=Class(TComponent)
    Ping1: TPing;
    constructor Create(AOwner:TComponent);
    procedure GoPingSomebody(Sender: TObject);
    procedure Ping1Display(Sender: TObject; Icmp: TObject; Msg: String);
    procedure Ping1DnsLookupDone(Sender: TObject; Error: Word);
    procedure CancelButtonClick(Sender: TObject);
    procedure Ping1EchoRequest(Sender: TObject; Icmp: TObject);
    procedure Ping1EchoReply(Sender: TObject; Icmp: TObject; Status: Integer);
  end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.GoPingSomebody(Sender: TObject);
begin
//  DisplayMemo.Clear;
    writeln('Resolving host ''' + pingmehost + '''');
//    PingButton.Enabled   := FALSE;
//    CancelButton.Enabled := TRUE;

    Ping1.DnsLookup(pingmehost);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPinger.Ping1DnsLookupDone(Sender: TObject; Error: Word);

var s : String;
begin
//    CancelButton.Enabled := FALSE;
//    PingButton.Enabled   := TRUE;

    if Error <> 0 then begin
        Writeln('Unknown Host ''' + pingmehost + '''');
        Exit;
    end;

    s:=TPinger(sender).Ping1.DnsResult;
    writeln('Host ''' + pingmehost + ''' is ' + TPinger(sender).Ping1.DnsResult);

    tpinger(sender).Ping1.Address := TPinger(sender).Ping1.DnsResult;
    TPinger(sender).Ping1.Ping;
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


constructor TPinger.Create(AOwner:TComponent);

begin
  inherited Create(AOwner);
  ping1:=TPing.create(self);
  Ping1.OnDnsLookupdone:=tpinger.ping1dnslookupdone;
  Ping1.OnEchoRequest:=tpinger.ping1echorequest;
  Ping1.OnEchoReply:=tpinger.ping1echoreply;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

var pinger : TPinger;
    AMessage:TMSG;

begin
  Pinger:=TPinger.Create(nil);
  pinger.GoPingSomebody(pinger);
   while GetMessage(@AMessage, 0, 0, 0) do begin
    TranslateMessage(AMessage);
    DispatchMessage(AMessage);
   end;

    writeln('waiting, pomperdom.');

  readln;
  Pinger.Destroy;



end.

