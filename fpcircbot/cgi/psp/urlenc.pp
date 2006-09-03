{$IFDEF FPC}{$MODE OBJFPC} {$H+}
    {$IFDEF EXTRA_SECURE}
     {$R+}{$Q+}{$CHECKPOINTER ON}
    {$ENDIF}
{$ENDIF}

unit urlenc;
{
 *******************************************************************************
 *                 -== Pascal Server Pages Unit - URLEnc ==-                   *
 *******************************************************************************
 * Hyper Text Transfer Protocol Encoding/Decoding functions                    *
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
 * [PSP 1.4.0 - 27.05.05 - Trustmaster]:                                       *
 * - PSP2-compliancy changes in the unit (unit and function names).            *
 *******************************************************************************
 }
 
{==============================================================================}
{================================== INTERFACE =================================}
{==============================================================================}

interface

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function url_decode(const svar: string): string;
//Decodes the original values of transfered variables from HTTP-safe

function url_encode(const svar: string): string;
//Encodes variable to HTTP safe

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{==============================================================================}
{================================ Constants ===================================}
{==============================================================================}

const   HEX_TABLE = '0123456789ABCDEF';
        LAT_TABLE = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}


// Traslates Hexadecimal value of hx (2 charecters) to char
function HexToChar(const hx: string): char;
var
  cnt,
  digit,
  ascii: byte;
begin
  ascii := 0;
  for cnt := 1 to 2 do
  begin
    digit := (pos(hx[cnt], HEX_TABLE) - 1); 
    // Decimal value of this HEX digit
    if cnt = 1 then
      ascii := ascii + (digit * 16)
    else
      ascii := ascii + digit;
    // Transtated from HEX to Decimal
  end;
  result := chr(ascii);
end;


//Decodes the original values of transfered variables from HTTP-safe
function url_decode(const svar: string): string;
var
  i, len: longword;
  c: char;
begin
  result := '';
  i := 1;
  len := length(svar);
  while i <= len do 
  begin
    c := svar[i];
    if c = '%' then
    begin
      if (i + 2) <= len then
      begin
        inc(i);
        result := result + HexToChar(copy(svar, i, 2));
        i := i + 2; 
      end
        else
      begin
        i := len;
      end;
    end
      else
    begin
      if c = '+' then
        result := result + ' '
      else
        result := result + c;
        inc(i);
    end;
  end;
end;


//Encodes variable to HTTP safe
function url_encode(const svar: string): string;
var i, len: longword;
    c: char;
begin
  result := '';
  i := 1;
  len := length(svar);
  while i <= len do
  begin
    c := svar[i];
    if (pos(c, LAT_TABLE) = 0) and (c <> ' ') and (c <> '_') then
    begin
        result := result + '%' + HexStr(ord(c), 2);
        inc(i);
    end
      else
    begin
      if c = ' ' then
        result := result + '+'
      else
        result := result + c;
      inc(i);
    end
  end;
end;

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
