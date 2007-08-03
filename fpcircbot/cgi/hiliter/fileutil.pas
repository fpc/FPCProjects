(*
  Copyright (c) 2003-2006 by PSP devel team. See Artistic License for info.

  Authors/Credits: Trustmaster (Vladimir Sibirov), L505 (Lars Olson)
*)
unit fileutil;

{$IFDEF FPC}{$MODE OBJFPC}{$H+}
    {$IFDEF EXTRA_SECURE}
     {$R+}{$Q+}{$CHECKPOINTER ON}
    {$ENDIF}
{$ENDIF}

interface

function FileExists_plain(const fname: string): boolean;
function FileExists_read(const fname: string): boolean;
function FileExists_readwrite(const fname: string): boolean;
function FileError: string;

implementation

const
  { File open modes }
  fmOpenRead       = $0000;
  fmOpenWrite      = $0001;
  fmOpenReadWrite  = $0002;
  { Share modes}
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;


// Checks if file exists
function FileExists_plain(const fname: string): boolean;
var
  fh: file of byte;
begin
  result := false;
  assign(fh, fname);
  {$i-}
  reset(fh);
  {$i+}                                                   
  if ioresult = 0 then result := true;
  close(fh);
end;

// Checks if file exists read only
function FileExists_read(const fname: string): boolean;
var
  fh: file of byte;
  oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenRead; // all we need is read access
  assign(fh, fname);
  {$i-}
  reset(fh);
  {$i+}
  if ioresult = 0 then result := true;
  close(fh);
  filemode:= oldfmode;  
end;

// Checks if file exists with write access
function FileExists_readwrite(const fname: string): boolean;
var
  fh: file of byte;
  oldfmode: byte;
begin
  result := false;
  oldfmode:= filemode;
  filemode:= fmOpenReadWrite; //  we need write access
  assign(fh, fname);
  {$i-}
  reset(fh);
  {$i+}
  if ioresult = 0 then result := true;
  close(fh);
  filemode:= oldfmode;  
end;


// Returns last I/O error message
function FileError: string;
begin
  case ioresult of
      2: result := 'File not found';
      3: result := 'Path not found';
      4: result := 'Too many open files';
      5: result := 'Access denied';
      6: result := 'Invalid file handle';
      12: result := 'Invalid file-access mode';
      15: result := 'Invalid disk number';
      16: result := 'Cannot remove current directory';
      17: result := 'Cannot rename across volumes';
      100: result := 'Error when reading from disk';
      101: result := 'Error when writing to disk';
      102: result := 'File not assigned';
      103: result := 'File not open';
      104: result := 'File not opened for input';
      105: result := 'File not opened for output';
      106: result := 'Invalid number';
      150: result := 'Disk is write protected';
      151: result := 'Unknown device';
      152: result := 'Drive not ready';
      153: result := 'Unknown command';
      154: result := 'CRC check failed';
      155: result := 'Invalid drive specified';
      156: result := 'Seek error on disk';
      157: result := 'Invalid media type';
      158: result := 'Sector not found';
      159: result := 'Printer out of paper';
      160: result := 'Error when writing to device';
      161: result := 'Error when reading from device';
      162: result := 'Hardware failure';
  else
      // Emtpy line - OK
      result := '';
  end;
end;

end.
