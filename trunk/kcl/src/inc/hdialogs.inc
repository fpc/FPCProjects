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
//   Dialog Box Classes
// ===================================================================

type

  TGenericDialog = class(TForm)
  public
    function Run: Boolean; virtual; abstract;
  end;


  TDialog = class(TGenericDialog)
  protected
    FOnOk, FOnCancel: TNotifyEvent;
    FResultWidget: TWidget;

    procedure Ok(AObj: TObject);
    procedure Cancel(AObj: TObject);
    // procedure SetContent(AContent: TWidget); override;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Run: Boolean; override;
    property ResultWidget: TWidget read FResultWidget;
  end;


  TStdDlgButtons =
    set of (btnOK, btnCancel, btnAbort, btnApply, btnYes, btnNo, btnHelp);

resourcestring
  btnText_Ok = 'Ok';
  btnText_Cancel = 'Cancel';
  btnText_Abort = 'Abort';
  btnText_Apply = 'Apply';
  btnText_Yes = 'Yes';
  btnText_No = 'No';
  btnText_Help = 'Help';

  dlgText_FileDialog = 'Select file';
type

  TStdBtnDialog = class(TDialog)
  protected
    FButtons: TStdDlgButtons;
    MainLayout: TDockingLayout;
    BtnLayout: TBoxLayout;
    RealContent: TWidget;
    procedure SetButtons(AButtons: TStdDlgButtons);
    procedure SetContent(AContent: TWidget); // ###override;
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Buttons: TStdDlgButtons read FButtons write SetButtons
      default [btnOk];
  end;


  TFileDialog = class(TWidget)
  protected
    ResultCode: Boolean;

    FFilename: String;
    procedure OnFinishCreation; override;
  public
    function Run: Boolean;
  published
    property Filename: String read FFilename write FFilename;
  end;


{
  $Log$
  Revision 1.2  2000/01/05 19:13:30  sg
  * Added "Abort" button for DefButton dialog
  * Added resourcestring for TFileDialog caption

  Revision 1.1.1.1  1999/12/30 21:33:20  sg
  Initial import

}
