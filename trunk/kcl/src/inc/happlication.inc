{
    $Id$

    KCL  -  Kassandra Component Library
    Copyright (C) 1999  by the KCL team
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

  TGenericForm = class;

  TReadFormProc = procedure(Instance: TComponent) of object;

  TApplication = class(TKCLComponent)
  protected
    FTitle: String;
    FReadFormProc: TReadFormProc;
    FForms: TList;
  public
    destructor Destroy; override;
    procedure Initialize;
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure AddForm(AForm: TGenericForm);
    procedure Run;
    procedure Terminate;

    property Title: String read FTitle write FTitle;
    property ReadFormProc: TReadFormProc read FReadFormProc write FReadFormProc;
  end;


{
  $Log$
  Revision 1.1  1999/12/30 21:33:22  sg
  Initial revision

}
