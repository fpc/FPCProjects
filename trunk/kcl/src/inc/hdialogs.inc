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

  TGenericDialog = class(TCustomForm)
  public
    function Run: Boolean; virtual; abstract;
  end;


  TCustomDialog = class(TGenericDialog)
  protected
    FOnOk, FOnCancel: TNotifyEvent;
    FResultWidget: TWidget;

    procedure Ok(AObj: TObject);
    procedure Cancel(AObj: TObject);
    procedure OnFinishCreation; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Run: Boolean; override;
    property ResultWidget: TWidget read FResultWidget;
  end;


  TDialog = class(TCustomDialog)
  published
    property Text;
    property Content;
    property BorderWidth;
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

  TStdBtnDialog = class(TCustomDialog)
  protected
    FButtons: TStdDlgButtons;
    FDlgContent: TWidget;
    MainLayout, BtnLayout: TBoxLayout;
    Separator: TSeparator;
    procedure SetButtons(AButtons: TStdDlgButtons);
    procedure StdBtnClicked(Sender: TObject);
    procedure OnFinishCreation; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Buttons: TStdDlgButtons read FButtons write SetButtons
      default [btnOk, btnCancel];
    property Content: TWidget read FDlgContent write FDlgContent;
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
  Revision 1.3  2000/02/17 22:21:56  sg
  * Added TCustomDialog
  * Several improvements...

  Revision 1.2  2000/01/05 19:13:30  sg
  * Added "Abort" button for DefButton dialog
  * Added resourcestring for TFileDialog caption

  Revision 1.1.1.1  1999/12/30 21:33:20  sg
  Initial import

}
