{ =============================================================================================== }
{ FMOD Main header file. Copyright (c), Firelight Technologies Pty, Ltd. 1999-2003.               }
{ =============================================================================================== }
{
  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.70.so (in Linux) installed.

  In Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.70.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.70.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.70.so libfmod.so.
}
{ =============================================================================================== }

unit fmoderrors;

{
  $Id$
  
}
{
  $Log$
  Revision 1.2  2004/04/03 20:05:02  marco
   * new versions from Dominique. No postediting at all necessary atm

  Revision 1.1  2004/03/30 21:52:34  savage
  Moved to it's own folder.

  Revision 1.2  2004/02/14 00:56:55  savage
  Added UNIX support for FreeBSD

  Revision 1.2  2004/02/05 00:37:32  savage
  New FMod 3.71 units and updated sdlinput.pas

  Revision 1.1  2004/02/05 00:08:19  savage
  Module 1.0 release

  
}

interface

{$I jedi-sdl.inc}

uses
  fmodtypes;

function FMOD_ErrorString(ErrorCode: TFModErrors): PChar;

implementation

function FMOD_ErrorString(ErrorCode: TFModErrors): PChar;
begin
  case ErrorCode of
    FMOD_ERR_NONE:              Result := 'No errors';
    FMOD_ERR_BUSY:              Result := 'Cannot call this command after FSOUND_Init.  Call FSOUND_Close first';
    FMOD_ERR_UNINITIALIZED:     Result := 'This command failed because FSOUND_Init was not called';
    FMOD_ERR_PLAY:              Result := 'Playing the sound failed';
    FMOD_ERR_INIT:              Result := 'Error initializing output device';
    FMOD_ERR_ALLOCATED:         Result := 'The output device is already in use and cannot be reused';
    FMOD_ERR_OUTPUT_FORMAT:     Result := 'Soundcard does not support the features needed for this soundsystem (16bit stereo output)';
    FMOD_ERR_COOPERATIVELEVEL:  Result := 'Error setting cooperative level for hardware';
    FMOD_ERR_CREATEBUFFER:      Result := 'Error creating hardware sound buffer';
    FMOD_ERR_FILE_NOTFOUND:     Result := 'File not found';
    FMOD_ERR_FILE_FORMAT:       Result := 'Unknown file format';
    FMOD_ERR_FILE_BAD:          Result := 'Error loading file';
    FMOD_ERR_MEMORY:            Result := 'Not enough memory or resources';
    FMOD_ERR_VERSION:           Result := 'The version number of this file format is not supported';
    FMOD_ERR_INVALID_PARAM:     Result := 'An invalid parameter was passed to this function';
    FMOD_ERR_NO_EAX:            Result := 'Tried to use an EAX command on a non EAX enabled channel or output';
    FMOD_ERR_CHANNEL_ALLOC:     Result := 'Failed to allocate a new channel';
    FMOD_ERR_RECORD:            Result := 'Recording is not supported on this machine';
    FMOD_ERR_MEDIAPLAYER:       Result := 'Required Mediaplayer codec is not installed';
  else
    Result := 'Unknown error';
  end;
end;

end.
