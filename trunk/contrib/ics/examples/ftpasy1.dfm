�
 TFTPASYNCFORM 0{  TPF0TFtpAsyncFormFtpAsyncFormLeft_Top�Width�Height~CaptionFtpAsyncFormColor	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style OldCreateOrder	OnClose	FormCloseOnCreate
FormCreateOnShowFormShowPixelsPerInch`
TextHeight TPanel
ToolsPanelLeft Top Width�Height}AlignalTopTabOrder  TLabelLabel1LeftTopWidth2HeightCaptionHostNameFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel4Left
Top'Width2HeightCaptionUserNameFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel5Left� Top'Width1HeightCaptionPassWordFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel6Left� TopWidthHeightCaptionPortFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel2LeftTop_Width)HeightCaption	Host FileFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel3LeftTopCWidth&HeightCaptionHost DirFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabelLabel7Left� TopDWidth*HeightCaption	Local Dir  TEditHostNameEditLeftHTopWidth� HeightHintHost where the file is locatedFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontParentShowHintShowHint	TabOrder TextHostNameEdit  TEditUserNameEditLeftHTop$Width� HeightHint!User name used to log on the hostFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontParentShowHintShowHint	TabOrderTextUserNameEdit  TEditPassWordEditLeftTop$Width� HeightHint%Password used to validate user accessFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontParentShowHintShowHint	TabOrderTextPassWordEdit  TEditPortEditLeftTopWidthUHeightFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontTabOrderTextPortEdit  	TCheckBoxcbBinaryLeftxTopWidth]HeightHint#Select to use binary mode transfertCaptionBinary ModeChecked	Font.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontParentShowHintShowHint	State	cbCheckedTabOrder  TEditHostDirEditLeftHTop@Width� HeightHint2Enter the host directory where the file is locatedFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontParentShowHintShowHint	TabOrderTextHostDirEdit  TEditHostFileEditLeftHTop[Width1HeightHint:Enter local file name and path or new file name for renameFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style 
ParentFontParentShowHintShowHint	TabOrderTextHostFileEdit  TButtonAddFileButtonLeft�Top\WidthKHeightCaptionAdd FileTabOrderOnClickAddFileButtonClick  TEditLocalDirEditLeftTop@Width� HeightTabOrderTextLocalDirEdit   TPanelPanel1Left Top}Width�Height� AlignalClientCaptionPanel1TabOrder TMemoDisplayMemoLeftYTopWidth.Height� AlignalClientFont.CharsetANSI_CHARSET
Font.ColorclWindowTextFont.Height�	Font.NameCourier New
Font.Style Lines.StringsDisplayMemo 
ParentFont
ScrollBarsssBothTabOrder   TPanelPanel2Left	TopWidthPHeight� AlignalLeftTabOrder TButton
ExecButtonLeftTopWidthAHeightCaptionExecuteTabOrder OnClickExecButtonClick  TButtonRemoveButtonLeftTop0WidthAHeightCaptionRemoveTabOrderOnClickRemoveButtonClick  TButtonReplaceButtonLeftTopPWidthAHeightCaptionReplaceTabOrderOnClickReplaceButtonClick  TButtonAbortButtonLeftToppWidthAHeightCaptionAbortTabOrderOnClickAbortButtonClick   TPanelPanel3LeftTopWidthHeight� AlignalLeftCaptionPanel3TabOrder TListBoxFilesListBoxLeftTopWidthHeightoAlignalClient
ItemHeightTabOrder   TListBoxResultsListBoxLeftToppWidthHeightlAlignalBottom
ItemHeightTabOrder    
TFtpClient
FtpClient1TimeoutMultiThreadedPortftp	LocalAddr0.0.0.0DisplayFileFlagBinary		ShareModeftpShareExclusiveOptionsftpAcceptLF ConnectionType	ftpDirect	OnDisplayFtpClient1DisplayOnRequestDoneFtpClient1RequestDoneLeftTop�    