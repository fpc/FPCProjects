{$IFDEF FPC}{$H+}{$MODE OBJFPC}{$R+}{$Q+}{$CHECKPOINTER ON}{$ENDIF}
unit hostname;
{
 *******************************************************************************
 *                -== Pascal Server Pages Unit - HostName ==-                  *
 *******************************************************************************
 * Crossplatform host name into address resolving additional unit for sockets. *
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
 * [PSP 1.4.0 - 22.08.05 - Trustmaster]:                                       *
 * - added inet_addr_aliases and inet_name_aliases functions.                  *
 *******************************************************************************
 }
 {$IFDEF WIN32}{$LINKLIB kernel32}{$ENDIF}{$IFDEF UNIX}{$LINKLIB c}{$ENDIF}
{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface
uses sockets;

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type StrArray = array of string;

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function inet_addr_aliases(const address: string): StrArray;
// Returns array of alias IP addresses for hostname

function inet_name_aliases(const address: string): StrArray;
// Returns array of alias names for hostname

function inet_resolve(const address: string; port: word): TInetSockAddr;
// Resolves string host address (dotted quad or domain name) and port into
// internet socket address

function inet_self_addr: string;
// Returns IP of the current computer as dotted quad

function inet_self_name: string;
// Returns name of the current computer

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{==============================================================================}
{================================   Types   ===================================}
{==============================================================================}

type 

{$IFDEF UNIX}
hostent = record
    { official name of host  }
    h_name: pchar;
    { alias list  }
    h_aliases: ^pchar;
    { host address type  }
    h_addrtype: integer;
    { length of address  }
    h_length: integer;
    { list of addresses  }
    h_addr_list: ^pchar;
    h_addr: pchar;
end;
{$ENDIF}
{$IFDEF WIN32}
hostent = record
    { official name of host  }
    h_name: pchar;
    { alias list  }
    h_aliases: ^pchar;
    { host address type  }
    h_addrtype: SmallInt;
    { length of address  }
    h_length: SmallInt;
    { list of addresses  }
    h_addr_list: ^pchar;
end;
{$ENDIF}

phostent = ^hostent;

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

{--[ gethosbyname/addr ]-------------------------------------------------------}
// Resolves hostname into hostent

{$IFDEF UNIX}
function gethostbyname(const name: pchar): phostent; cdecl; external 'c';
function gethostbyaddr(const addr: pchar; len, format: longint): phostent; cdecl; external 'c';
function gethostname(name: pchar; len: longint): longint; cdecl; external 'c';
{$ENDIF}
{$IFDEF WIN32}
function gethostbyaddr(const addr: pchar; len, atype: longint): phostent; stdcall; external 'wsock32';
function gethostbyname(const name: pchar): phostent; stdcall; external 'wsock32';
function gethostname(name: pchar; len: longint): longint; stdcall; external 'wsock32';
{$ENDIF}

{------------------------------------------------------------------------------}

{--[ is_quad ]-----------------------------------------------------------------}
// Checks if the string is dotted quad

function is_quad(const name: string): boolean;
const STR_DIGITS = '0123456789';
var i, len: longword;
    count: byte;
begin
    // Init
    result := false;
    // Length check ('0.0.0.0' to '255.255.255.255')
    len := length(name);
    if (len < 7) or (len > 15) then exit(false);
    count := 1;
    i := 1;
    repeat
        while (i <= len) and (name[i] <> '.') do
            begin
                if pos(name[i], STR_DIGITS) <= 0 then exit(false);
                inc(i);
            end;
        inc(i);
        inc(count);
    until (i >= len) or (count = 4);
    if (i = len + 1) and (count = 4) then result := true; 
end;

{------------------------------------------------------------------------------}

{--[ quad2addr ]---------------------------------------------------------------}
// Converts dotted quad into address

function quad2addr(var s: string): longint;
var i, p: byte;
    t: string;
begin
    result := 0;
    for i := 0 to 3 do
        begin
            p := byte(pos('.', s));
            if p = 0 then p := length(s) + 1;
            if p <= 1 then exit;
            t := copy(s, 1, p - 1);
            delete(s, 1, p);
            val(t, p);
            if (p < 0) or (p > 255) then exit;
            result := result or p shl (i * 8);
        end;
end;

{------------------------------------------------------------------------------}

{--[ intet_addr_aliases ]------------------------------------------------------}
// Returns array of alias IP addresses for hostname

function inet_addr_aliases(const address: string): StrArray;
var he: phostent;
    tmp: string;
    nwa: longint;
begin
    tmp := address;
    // Depends on address type
    if is_quad(address) then
        begin
            nwa := quad2addr(tmp);
            he := gethostbyaddr(pchar(@nwa), 4, AF_INET);
        end
    else he := gethostbyname(pchar(address));
    if he = nil then exit;
    // Then extracting list of addresses as dotted quads
    while (he^.h_addr_list^ <> #0) and (he^.h_addr_list^ <> nil) do
        begin
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := NetAddrToStr(pin_addr(he^.h_addr_list^)^);
            inc(he^.h_addr_list);
        end;
end;

{------------------------------------------------------------------------------}

{--[ inet_name_aliases ]-------------------------------------------------------}
// Returns array of alias names for hostname

function inet_name_aliases(const address: string): StrArray;
var he: phostent;
    tmp: string;
    nwa: longint;
begin
    tmp := address;
    // Depends on address type
    if is_quad(address) then
    begin
      nwa := quad2addr(tmp);
      he := gethostbyaddr(pchar(@nwa), 4, AF_INET);
    end
    else he := gethostbyname(pchar(address));
    if he = nil then exit;
    // 0 element should be h_name
    SetLength(result, 1);
    result[0] := AnsiString(he^.h_name);
    // Then extracting list of alias names
    while (he^.h_aliases^ <> #0) and (he^.h_aliases^ <> nil) do
    begin
      SetLength(result, length(result) + 1);
      result[length(result) - 1] := AnsiString(he^.h_aliases^);
      inc(he^.h_aliases);
    end;
end;

{------------------------------------------------------------------------------}

{--[ inet_resolve ]------------------------------------------------------------}
// Resolves string host address (dotted quad or domain name) and port into
// internet socket address

function inet_resolve(const address: string; port: word): TInetSockAddr;
var tmp: string;
    he: phostent;
begin
    tmp := address;
    // First initializing and assigning port
    result.family := AF_INET;
    result.port := htons(port);
    // Then depending on address value
    if is_quad(address) then result.addr := quad2addr(tmp)
    else
    begin
      he := gethostbyname(PChar(address));
      result.addr := pin_addr(he^.h_addr_list^)^.S_addr;
    end;
end;

{------------------------------------------------------------------------------}

{--[ inet_self_addr ]----------------------------------------------------------}
// Returns IP of the current computer as dotted quad

function inet_self_addr: string;
var he: phostent;
    name: pchar;
begin
    result := '';
    name := allocmem(256); // 256 chars would be enough
    // Looks enormous, but its really done this way
    if gethostname(name, 256) <> 0 then exit;
    // Then host entry
    he := gethostbyname(name);
    if he = nil then exit;
    result := NetAddrToStr(pin_addr(he^.h_addr_list^)^);
end;

{------------------------------------------------------------------------------}

{--[ inet_self_name ]----------------------------------------------------------}
// Returns name of the current computer

function inet_self_name: string;
var name: pchar;
begin
    result := '';
    name := allocmem(256); // 256 chars would be enough
    // Looks enormous, but its really done this way
    if gethostname(name, 256) <> 0 then exit;
    // Fetching result
    result := AnsiString(name);
end;

{------------------------------------------------------------------------------}

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
