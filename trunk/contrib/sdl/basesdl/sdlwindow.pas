unit sdlwindow;
{
  $Id$
  
}
{******************************************************************************}
{                                                                              }
{       Borland Delphi SDL - Simple DirectMedia Layer                          }
{                SDL Window Wrapper                                            }
{                                                                              }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Portions created by Dominique Louis are                                      }
{ Copyright (C) 2004 - 2100 Dominique Louis.                                   }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   SDL Window Wrapper                                                         }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL.dll on Windows platforms                                               }
{   libSDL-1.1.so.0 on Linux platform                                          }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{ January    31     2003 - DL : Initial creation                               }
{                                                                              }
{                                                                              }
{******************************************************************************}
{
  $Log$
  Revision 1.2  2004/04/04 15:38:28  marco
   * deleted CR char

  Revision 1.1  2004/04/03 20:09:00  marco
   * New units in this version

  Revision 1.2  2004/03/31 10:06:41  savage
  Changed so that it now compiles, but is untested.

  Revision 1.1  2004/02/05 00:08:20  savage
  Module 1.0 release

  
}

interface

{$i jedi-sdl.inc}

uses
  Classes,
  sdl,
  sdlinput;

type
  TSDLNotifyEvent =  procedure {$IFNDEF NOT_OO}of object{$ENDIF};
  TSDLResizeEvent =  procedure( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 ) {$IFNDEF NOT_OO}of object{$ENDIF};

  TSDLBaseWindow = class( TObject )
  private

  protected
    FDisplaySurface : PSDL_Surface;
    FVideoFlags : Uint32;
    FOnDestroy: TSDLNotifyEvent;
    FOnCreate: TSDLNotifyEvent;
    FOnShow: TSDLNotifyEvent;
    FOnResize: TSDLResizeEvent;
    FOnNextFrame: TSDLNotifyEvent;
    FOnRender: TSDLNotifyEvent;
    FOnClose: TSDLNotifyEvent;
    procedure DoCreate;
    procedure DoClose;
    procedure DoDestroy;
    procedure DoResize( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 );
    procedure DoShow;
    procedure DoNextFrame;
    procedure DoRender;
    procedure Render; virtual;
    procedure NextFrame; virtual;
    procedure InitialiseObjects; virtual;
    procedure RestoreObjects; virtual;
    procedure DeleteObjects; virtual; 
    property OnCreate : TSDLNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy : TSDLNotifyEvent read FOnDestroy write FOnDestroy;
    property OnClose: TSDLNotifyEvent read FOnClose write FOnClose;
    property OnShow : TSDLNotifyEvent read FOnShow write FOnShow;
    property OnResize : TSDLResizeEvent read FOnResize write FOnResize;
    property OnRender: TSDLNotifyEvent read FOnRender write FOnRender;
    property OnNextFrame: TSDLNotifyEvent read FOnNextFrame write FOnNextFrame;
  public
    InputManager : TSDLInputManager;
    Loaded : Boolean;
    Width : integer;
    Height : integer;
    BitDepth : integer;
    Finished : Boolean;
    constructor Create( aDisplaySurface : PSDL_Surface; aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 ); virtual;
    destructor Destroy; override;
    procedure InitialiseEnvironment;
    function Show : Boolean; virtual;
  end;

  TSDL2DWindow = class( TSDLBaseWindow )
  public
    constructor Create( aDisplaySurface : PSDL_Surface; aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 = SDL_DOUBLEBUF ); override;
  end;

  TSDL3DWindow = class( TSDLBaseWindow )
  public
    constructor Create( aDisplaySurface : PSDL_Surface; aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 = SDL_OPENGL ); override;
  end;



implementation

{ TSDLBaseWindow }
constructor TSDLBaseWindow.Create( aDisplaySurface : PSDL_Surface; aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 );
begin
  inherited Create;
  InputManager := TSDLInputManager.Create( [ itJoystick, itKeyBoard, itMouse ]);

  Width := aWidth;
  Height := aHeight;
  BitDepth := aBitDepth;
  FDisplaySurface := aDisplaySurface;
  DoCreate;
end;

procedure TSDLBaseWindow.DeleteObjects;
begin
  Loaded := False;
end;

destructor TSDLBaseWindow.Destroy;
begin
  DoDestroy;
  if Loaded then
      RestoreObjects;
  if InputManager <> nil then
    FreeAndNil( InputManager );
  inherited;
end;

procedure TSDLBaseWindow.DoClose;
begin
  if Assigned( FOnClose ) then
  begin
    FOnClose;
  end;
end;

procedure TSDLBaseWindow.DoCreate;
begin
  if Assigned( FOnCreate ) then
  begin
    FOnCreate;
  end;
end;

procedure TSDLBaseWindow.DoDestroy;
begin
  if Assigned( FOnDestroy ) then
  begin
    FOnDestroy;
  end;
end;

procedure TSDLBaseWindow.DoNextFrame;
begin
  {if Assigned( FNextFrame ) then
  begin
    FNextFrame;
  end;}
end;

procedure TSDLBaseWindow.DoRender;
begin
  {if FRender <> nil then
  begin
    FRender;
  end;}
end;

procedure TSDLBaseWindow.DoResize( aWidth : integer; aHeight : integer; aBitDepth : integer; aVideoFlags : Uint32 );
begin
  if Assigned( FOnResize ) then
  begin
    FOnResize( aWidth, aHeight, aBitDepth, aVideoFlags );
  end;
end;

procedure TSDLBaseWindow.DoShow;
begin
  if Assigned( FOnShow ) then
  begin
    FOnShow;
  end;
end;

procedure TSDLBaseWindow.InitialiseEnvironment;
begin
  InitialiseObjects;
  RestoreObjects;
end;

procedure TSDLBaseWindow.InitialiseObjects;
begin
  Loaded := True;
end;

procedure TSDLBaseWindow.NextFrame;
begin
  DoNextFrame;
end;

procedure TSDLBaseWindow.Render;
begin
  DoRender;
end;

procedure TSDLBaseWindow.RestoreObjects;
begin
  Loaded := false;
end;

function TSDLBaseWindow.Show : Boolean;
var
  eBaseWindowEvent : TSDL_Event;
begin
  DoShow;
  
  // repeat until Finihsed is set to something else
	while not Finished do
	begin

		// wait for an event
		while SDL_PollEvent( @eBaseWindowEvent ) > 0 do
		begin
      // check for a quit event
      case eBaseWindowEvent.type_ of
        SDL_QUITEV :
        begin
          Finished := true;
          DoClose;
        end;


        SDL_VIDEORESIZE :
        begin
          DoResize( eBaseWindowEvent.resize.w, eBaseWindowEvent.resize.h, FDisplaySurface.format.BitsPerPixel, FVideoflags );
        end;
      end;

      InputManager.UpdateInputs( eBaseWindowEvent );
    end;

    // Prepare the Next Frame
    NextFrame;

    // Display the Next Frame
    Render;
    
    // let's show the back buffer
    SDL_Flip( FDisplaySurface );
  end;

  Result := Finished;
end;

{ TSDL2DWindow }

constructor TSDL2DWindow.Create(aDisplaySurface: PSDL_Surface; aWidth,
  aHeight, aBitDepth: integer; aVideoFlags: Uint32);
begin
  inherited;

end;

{ TSDL3DWindow }

constructor TSDL3DWindow.Create(aDisplaySurface: PSDL_Surface; aWidth,
  aHeight, aBitDepth: integer; aVideoFlags: Uint32);
begin
  inherited;

end;

end.
