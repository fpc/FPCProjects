{
    $Id$

    Kassandra  -  Multiplatform Integrated Development Environment
    Copyright (C) 1999  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// Generic viewer class

{$MODE objfpc}
{$M+,H+}

unit vw_generic;

interface
uses Classes, KCL;

type

  TGenericView = class;

  TOnViewModified = procedure(AView: TGenericView) of object;

  TGenericView = class
  protected
    FMainSubwindow: TWidget;
    FFilename: String;
    FIsModified: Boolean;
    FOnModified: TOnViewModified;
    procedure FSetFileName(NewFileName: String);
    procedure FSetIsModified(NewIsModified: Boolean);
  public
    property MainSubwindow: TWidget read FMainSubwindow;
    property FileName: String read FFileName write FSetFileName;
    PageNumber: Integer;
    HasDefaultName: Boolean;		// Set if filename is not really set
    property IsModified: Boolean read FIsModified write FSetIsModified;
    procedure Save; virtual;
  published
    property OnModified: TOnViewModified read FOnModified write FOnModified;
  end;


implementation


procedure TGenericView.FSetFileName(NewFileName: String);
begin
  if NewFileName <> FFileName then begin
    FFileName := newFileName;
    if Assigned(OnModified) then
      OnModified(Self);
  end;
end;

procedure TGenericView.FSetIsModified(NewIsModified: Boolean);
begin
  if NewIsModified <> FIsModified then begin
    FIsModified := NewIsModified;
    if Assigned(OnModified) then
      OnModified(Self);
  end;
end;

procedure TGenericView.Save;
begin
  FSetIsModified(False);
end;

end.


{
  $Log$
  Revision 1.1  1999/12/30 21:22:31  sg
  Initial revision

}
