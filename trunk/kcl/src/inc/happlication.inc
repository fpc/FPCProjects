{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999 - 2000  by the KCL team
      see file AUTHORS in base directory of this distribution

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// ===================================================================
//   Definition of TApplication
// ===================================================================

type

  TCustomForm = class;

  TReadFormProc = procedure(Instance: TComponent) of object;

  TApplication = class(TKCLComponent)
  protected
    FTitle: String;
    FReadFormProc: TReadFormProc;
    IsRunning: Boolean;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    destructor Destroy; override;
    procedure Initialize;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure AddForm(AForm: TCustomForm);
    procedure Run;
    procedure Terminate;

    property Title: String read FTitle write FTitle;
    property ReadFormProc: TReadFormProc read FReadFormProc write FReadFormProc;
  end;


{
  $Log$
  Revision 1.3  2000/02/10 18:32:25  sg
  * TGenericForm is now called TCustomForm

  Revision 1.2  2000/01/24 00:08:49  sg
  * Extended copyright to year 2000...

  Revision 1.1.1.1  1999/12/30 21:33:22  sg
  Initial import

}
