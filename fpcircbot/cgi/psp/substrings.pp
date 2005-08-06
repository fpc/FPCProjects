{$H+}{$MODE OBJFPC}
unit substrings;
{
 *******************************************************************************
 *                -== Pascal Server Pages unit - Substrings ==-                *
 *******************************************************************************
 * Some advanced code for work with AnsiStrings                                *
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

{==============================================================================}
{=================================== Types ====================================}
{==============================================================================}

type StrArray = array of string;
// Just the array of string

{==============================================================================}
{========================= Procedures and Functions ===========================}
{==============================================================================}

function SubstrExists(str, sub: string): boolean;
// Finds if sub exists in str

function SubstrCount(str, sub: string): integer;
// Returns number of occurances of sub in str

function SubstrReplace(str, sub, repl: string): string;
// Replaces all the sub substrings in str with repl substrings

function StrStrip(str, sub: string): string;
// Removes all occurances of sub in the string

function StrSplit(str, separator: string): StrArray;
// Splits str into array by string separator

function StrReverse(str: string): string;
// Returns reversed string

function StrTrimLeft(str: string): string;
// Removes whitespaces and tab chars from the beginning of the line

function StrTrimRight(str: string): string;
// Removes whitespaces and tab chars from the end of the line

function StrTrim(str: string): string;
// Removes whitespaces and tab chars from beginning and end of the line

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{--[ SubstrExists ]------------------------------------------------------------}
// Finds if sub exists in str

function SubstrExists(str, sub: string): boolean;
begin
    if pos(sub, str) > 0 then result := true
    else result := false;
end;

{------------------------------------------------------------------------------}

{--[ SubstrCount ]-------------------------------------------------------------}
// Returns number of occurances of sub in str

function SubstrCount(str, sub: string): integer;
var temp: string;
begin
    result := 0;
    temp := str;
    while pos(sub, str) > 0 do
        begin
            inc(result);
            Delete(temp, pos(sub, str), length(sub));
        end;
end;

{------------------------------------------------------------------------------}

{--[ SubstrReplace ]-----------------------------------------------------------}
// Replaces all the sub substrings in str with repl substrings

function SubstrReplace(str, sub, repl: string): string;
var posn, sublen, len, replen: integer;
begin
    result := str;
    posn := 1;
    sublen := length(sub);
    replen := length(repl);
    repeat
        if copy(result, posn, sublen) = sub then
            begin
                delete(result, posn, sublen);
                insert(repl, result, posn);
                posn := posn + replen;
            end
        else inc(posn);
        len := length(result);
    until posn > len;
end;

{------------------------------------------------------------------------------}

{--[ StrStrip ]----------------------------------------------------------------}
// Removes all occurances of sub in the string

function StrStrip(str, sub: string): string;
var len: integer;
begin
    result := str;
    len := length(sub);
    while pos(sub, result) > 0 do Delete(result, pos(sub, result), len);
end;

{------------------------------------------------------------------------------}

{--[ StrSplit ]----------------------------------------------------------------}
// Splits str into array by string separator

function StrSplit(str, separator: string): StrArray;
var temp: string;
    i, len: integer;
begin
    temp := str;
    len := length(separator);
    i := 1;
    // Splitting while delemiter presents in temp
    while pos(separator, temp) > 0 do
        begin
            i := pos(separator, temp);
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := Copy(temp, 1, i - 1);
            Delete(temp, 1, (i - 1) + len);
        end;
    // Just copying the last part
    SetLength(result, length(result) + 1);
    result[length(result) - 1] := temp;
end;

{------------------------------------------------------------------------------}

{--[ StrReverse ]--------------------------------------------------------------}
// Returns reversed string

function StrReverse(str: string): string;
var i, len: integer;
begin
    len := length(str);
    SetLength(result, len);
    for i :=1 to len do result[i] := str[len - i];
end;

{------------------------------------------------------------------------------}

{--[ StrTrimLeft ]-------------------------------------------------------------}
// Removes whitespaces and tab chars from the beginning of the line

function StrTrimLeft(str: string): string;
var i: integer;
begin
    i := 0;
    while (str[i + 1] = #32) or (str[i + 1] = #9) do inc(i);
    result := copy(str, i + 1, length(str) - i);
end;

{------------------------------------------------------------------------------}

{--[ StrTrimRight ]------------------------------------------------------------}
// Removes whitespaces and tab chars from the end of the line

function StrTrimRight(str: string): string;
var i, len: integer;
begin
    len := length(str);
    i := 0;
    while (str[len - i] = #32) or (str[len - i] = #9) do inc(i);
    result := copy(str, 1, len - i);
end;

{------------------------------------------------------------------------------}

{--[ StrTrim ]-----------------------------------------------------------------}
// Removes whitespaces and tab chars from beginning and end of the line

function StrTrim(str: string): string;
begin
    result := StrTrimRight(StrTrimLeft(str));
end;

{------------------------------------------------------------------------------}

{==============================================================================}
{===================================== END ====================================}
{==============================================================================}

end.
