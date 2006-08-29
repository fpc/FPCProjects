{
    $id: dllfuncs.pp,v 1.1 2005/07/27 21:13:44 z0m3ie exp $

 **********************************************************************}

{$MODE OBJFPC}

unit dllfuncs;

interface

uses sysutils;

function LoadLibrary(Name: PChar): PtrInt;
function GetProcAddress(Lib: PtrInt; ProcName: PChar): Pointer;
function FreeLibrary(Lib: PtrInt): Boolean;
function getlastdlerror: pchar;


implementation

const
  RTLD_LAZY         = $001;
  RTLD_NOW          = $002;
  RTLD_BINDING_MASK = $003;

{$ifdef Linux}
function dlopen(Name: PChar; Flags: LongInt) : Pointer; cdecl; external 'dl';
function dlsym(Lib: Pointer; Name: PChar) : Pointer; cdecl; external 'dl';
function dlclose(Lib: Pointer): LongInt; cdecl; external 'dl';
function dlerror: pchar; cdecl; external 'dl';
{$else}
function dlopen(Name: PChar; Flags: LongInt) : Pointer; cdecl; external 'c';
function dlsym(Lib: Pointer; Name: PChar) : Pointer; cdecl; external 'c';
function dlclose(Lib: Pointer): LongInt; cdecl; external 'c';
function dlerror: pchar; cdecl; external 'c';
{$endif}

function getlastdlerror: pchar;
begin
  getlastdlerror := dlerror;
end;

function LoadLibrary(Name: PChar): PtrInt;
begin
  Result := PtrInt(dlopen(Name, RTLD_LAZY));
end;

function GetProcAddress(Lib: PtrInt; ProcName: PChar): Pointer;
begin
  Result := dlsym(Pointer(Lib), ProcName);
end;

function FreeLibrary(Lib: PtrInt): Boolean;
begin
  if Lib = 0 then
    Result := False
  else
    Result := dlClose(Pointer(Lib)) = 0;
end;

end.


{
  $Log: dllfuncs.pp,v $
  Revision 1.1  2006/01/10 20:50:46  z0m3ie
  recheckin to make shure that all is lowercase

  Revision 1.3  2006/01/09 21:02:33  z0m3ie
  *** empty log message ***

  Revision 1.1  2005/07/27 21:13:44  z0m3ie
  Initial Version

  Revision 1.1  2005/07/20 21:57:03  z0m3ie
  *** empty log message ***

  Revision 1.5  2005/02/14 17:13:21  peter
    * truncate log

}
