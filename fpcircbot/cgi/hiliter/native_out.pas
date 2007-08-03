{
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                                PSP/PWU 1.6.X

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

--------------------------------------------------------------------------------
 Main Web Unit
--------------------------------------------------------------------------------

  This unit contains the main procedures and additions for
  implementing Operating System Native Character Output 
  functions.   This removes one level of redirection from
  using Write/Writeln and allows webcrt to work when calling the
  webwrite function.


--------------------------------------------------------------------------------
 Developer Notes
--------------------------------------------------------------------------------

 PSP 1.6.x
 ---------

  [12/FEB/2006 - 25/FEB/2006 ]

   - Created unit to implement Native Write Function


--------------------------------------------------------------------------------
  Developer Todo
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
  Authors/Credits:
--------------------------------------------------------------------------------
  - anthonyh63 (Anthony Henry)




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
}


UNIT native_out; {$IFDEF WIN32} {$DEFINE WINDOWS} {$ENDIF}

// Implements OS native string out to STDOUT
// function for windows and unix. 

INTERFACE

{$IFDEF UNIX}
  uses baseunix;

// Under Unix STDOUT is always 1. 
  CONST
     STDIN = 0;
     STDOUT = 1;
     STDERR = 2;
{$ENDIF}

{$IFDEF WINDOWS}
  uses windows;
  var
    STDOUT: HANDLE;
{$ENDIF}


procedure NativeWrite(s : AnsiString); overload;
procedure NativeWrite(PString : PChar); overload;
{$IFDEF FPC}
  procedure NativeWrite(Buffer : PChar;  NumChars : Cardinal); overload;
{$ENDIF}

// function strlen is defined in system unit.

// function strlen(p:pchar):sizeint;external name 'FPC_PCHAR_LENGTH';

IMPLEMENTATION

{$IFNDEF FPC}
  procedure NativeWrite(s: AnsiString); overload;
  begin
    write(s);
  end;
  {
  procedure NativeWrite(Buffer: PChar;  NumChars : Cardinal); overload;
  begin

  end;
  }
  procedure NativeWrite(PString: PChar); overload;
  begin 
    write(PString);
  end;

{$ENDIF}

{$IFDEF FPC}

  {$IFDEF WINDOWS}
   var
    NumberOfChars : DWORD;
    NumWritten : LongWord;
    Idx, CharsLeft : Cardinal;
    PO : POVERLAPPED;
    TPc : PChar;
  {$ENDIF}


  procedure NativeWrite(S : AnsiString); overload;

  {$IFDEF UNIX} 
  var
   NumberOfChars : Cardinal;
  {$ENDIF}

  begin
    NumberOfChars := Length(S);

    {$IFDEF UNIX}
     fpwrite(STDOUT, PChar(S), NumberOfChars);
    {$ENDIF}
    
    {$IFDEF WINDOWS}
     po := nil;

  // If we move NativeWrite into pwumain then this can be
  // IFDEF 'ED WITH STATIC.
  // With STATIC put this call in initialization
  // It only needs to be in the function if called from DLL
  // (Win STDOUT is not alway the same handle.)
  // But it shouldn't change for one program run. 
     STDOUT := GetStdHandle(STD_OUTPUT_HANDLE);

  // Windows WriteFile function is limited to 64k characters across a Named Pipe
  // I'm assuming that is what we are doing with Apache (not sure but this won't)
  // hurt except for a small performance hit otherwise.
  // Unix fpwrite does not have that limitation
     If NumberOfChars <= 32000 then
         begin
           WriteFile(STDOUT,PChar(s)^, NumberOfChars, NumWritten, Po);
         end
      ELSE
       begin
         CharsLeft := NumberOfChars;
         Idx := 1;
         NumWritten := 0;
         repeat
           If CharsLeft <= 32000 then
              begin
                NumberofChars := CharsLeft;
              end
           else
             begin
               NumberOfChars := 32000;
             end;
           TPc := @S[Idx];
           WriteFile(STDOUT, TPc^ , NumberOfChars, NumWritten, Po);
           Dec(CharsLeft, NumWritten);
           Inc(Idx, NumWritten);
         until CharsLeft <= 0;
       end; 
    {$ENDIF}
  end;
  


  procedure NativeWrite(Buffer : PChar;  NumChars : Cardinal); overload;


  begin
    {$IFDEF UNIX}
     fpwrite(STDOUT, Buffer, NumChars);
    {$ENDIF}   // *** Thats all for UNIX :)
    
    {$IFDEF WINDOWS}
      // Windows is a little more complex 
      // because of limitations of the function
      po := nil;   // We can set the pointer to the Overlapped structure to NIL
                   // Its not needed for writing to the console. 


   // If we move NativeWrite into pwumain then this can be
   // IFDEF 'ED WITH STATIC.
   // With STATIC put this call in initialization
   // It only needs to be in the function if called from DLL
   // (Win STDOUT is not alway the same handle.)
   // But it shouldn't change for one program run. 
      STDOUT := GetStdHandle(STD_OUTPUT_HANDLE);

   // Windows WriteFile function is limited to 64k characters across a Named Pipe
   // (according to docs, didn't work reliably with over 32k) 
   // Unix fpwrite does not have that limitation
   
     If NumChars <= 32000 then
         begin
           WriteFile(STDOUT, Buffer^, NumChars, NumWritten, Po);
         end
      ELSE
       begin
         CharsLeft := NumChars;
         Idx := 1;
         NumWritten := 0;
         TPc := Buffer;
         repeat
           If CharsLeft <= 32000 then
             NumberOfChars := CharsLeft
           else
             NumberOfChars := 32000;
   
           WriteFile(STDOUT, TPc^ , NumberOfChars, NumWritten, Po);
           Dec(CharsLeft, NumWritten);
           Inc(PtrUInt(TpC), NumWritten);
         until CharsLeft <= 0;
       end; 
    {$ENDIF}
  end;





  procedure NativeWrite(PString : PChar); overload;
  begin
    {$IFDEF UNIX}
     fpwrite(STDOUT, PString, Strlen(PString));
    {$ENDIF}
    
    {$IFDEF WINDOWS}
     po := nil;

  // If we move NativeWrite into pwumain then this can be
  // IFDEF 'ED WITH STATIC.
  // With STATIC put this call in initialization
  // It only needs to be in the function if called from DLL
  // (Win STDOUT is not alway the same handle.)
  // But it shouldn't change for one program run. 
     STDOUT := GetStdHandle(STD_OUTPUT_HANDLE);
     NumberOfChars := Strlen(PString);
   Tpc := PString;
  // Windows WriteFile function is limited to 64k characters across a Named Pipe
  // I'm assuming that is what we are doing with Apache (not sure but this won't)
  // hurt except for a small performance hit otherwise.
  // Unix fpwrite does not have that limitation
     If NumberOfChars <= 32000 then
         begin
           WriteFile(STDOUT,PString^, NumberOfChars, NumWritten, Po);
         end
      ELSE
       begin
         CharsLeft := NumberOfChars;
         NumWritten := 0;
         repeat
           If CharsLeft <= 32000 then
              begin
                NumberofChars := CharsLeft;
              end
           else
             begin
               NumberOfChars := 32000;
             end;
         WriteFile(STDOUT, TPc^ , NumberOfChars, NumWritten, Po);
           Dec(CharsLeft, NumWritten);
           Inc(PtrUInt(Tpc), NumWritten);
         until CharsLeft <= 0;
       end; 
    {$ENDIF}
  end;

{$ENDIF FPC}
END.
