{$H+}{$MODE OBJFPC}
unit fileshare;
{
 *******************************************************************************
 *                -== Pascal Server Pages class - FileShare ==-                *
 *******************************************************************************
 * Some functions for safe file sharing.                                       *           
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
{========================= Procedures and Functions ===========================}
{==============================================================================}
 
{$IFDEF WIN32}
procedure sleep(dwMilliseconds:DWORD); stdcall; external 'kernel32' name 'Sleep';
{$ENDIF}
{$IFDEF UNIX}
function sleep(seconds: integer): integer; cdecl; external 'c' name 'sleep';
{$ENDIF}

function FExists(fname: string): boolean;
// Checks if file exists

function FLock(fname: string): boolean;
// Temprorary locks access to the file

function FUnlock(fname: string): boolean;
// Unlocks the file

function FSuspend(fname: string): boolean;
// Suspends the execution until file is freed

{==============================================================================}
{================================ IMPLEMENTAION ===============================}
{==============================================================================}

implementation

{--[ FExists ]-------------------------------------------------------------------}
// Checks if file exists

function FExists(fname: string): boolean;
var fh: text;
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

{------------------------------------------------------------------------------}

{--[ FSuspend ]----------------------------------------------------------------}
// Suspends the execution until the file is unlocked

function FSuspend(fname: string): boolean;
begin
    while FExists(fname + '.lck') do
        begin
            {$IFDEF WIN32}
            sleep(10);
            {$ENDIF}
            {$IFDEF UNIX}
            sleep(1);
            {$ENDIF}
            // Yep, other platforms will eat 99.9% of CPU time :-(
        end;
    result := true;
end;

{------------------------------------------------------------------------------}

{--[ FLock ]-------------------------------------------------------------------}
// Temprorary locks access to the file
 
function FLock(fname: string): boolean;
var fh: text;
begin
    FSuspend(fname);
    if FExists(fname) and (not FExists(fname + '.lck')) then
        begin
            assign(fh, fname + '.lck');
            rewrite(fh);
            close(fh);
        end
    else result := false;
end;

{------------------------------------------------------------------------------}

{--[ FUnlock ]-----------------------------------------------------------------}
// Unlocks the file

function FUnlock(fname: string): boolean;
var fh: text;
begin
    if FExists(fname + '.lck') then
        begin
            assign(fh, fname + '.lck');
            erase(fh);
        end
    else result := false;
end;

{------------------------------------------------------------------------------}
 
{==============================================================================}
{===================================== END ====================================}
{==============================================================================}
 
end.