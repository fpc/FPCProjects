�
 TSENDERFORM 0
  TPF0TSenderForm
SenderFormLeft� Top� Width0HeightCCaptionSender
Font.ColorclWindowTextFont.Height�	Font.NameMS Sans Serif
Font.Style PixelsPerInch`OnClose	FormCloseOnCreate
FormCreate	OnDestroyFormDestroyOnShowFormShow
TextHeight TPanelPanel1Left Top Width(HeightEAlignalTopTabOrder  TLabelLabel1LeftTopWidthHeightCaptionServer  TLabelLabel2Left� TopWidthHeightCaptionPort  TLabelLabel3LeftTop(WidthHeightCaptionData  TLabelLabel4LeftdTopWidth#HeightCaptionRepeat  TLabelLabel5Left� TopWidth8HeightCaptionLine Length  TLabel
CountLabelLeft�Top(WidthHeightCaptionCount  TEdit
ServerEditLeft0TopWidthYHeightTabOrder Text
ServerEdit  TEditPortEditLeft� TopWidth9HeightTabOrderTextPortEdit  TEditDataEditLeft0Top$Width� HeightTabOrderTextDataEdit  TEdit
RepeatEditLeft�TopWidth%HeightTabOrderText
RepeatEdit  	TCheckBoxContCheckBoxLeft�TopWidthAHeight	AlignmenttaLeftJustifyCaption	ContinousTabOrderOnClickContCheckBoxClick  TButtonActionButtonLeft� Top$Width-HeightCaption&StartTabOrderOnClickActionButtonClick  TEdit
LengthEditLeft,TopWidth)HeightTabOrderText
LengthEdit  	TCheckBoxDisplayDataCheckBoxLeft�TopWidthQHeight	AlignmenttaLeftJustifyCaptionDisplay DataTabOrderOnClickDisplayDataCheckBoxClick  	TCheckBoxUseDataSentCheckBoxLeft�Top WidthaHeight	AlignmenttaLeftJustifyCaptionUse onDataSentTabOrderOnClickUseDataSentCheckBoxClick  TButtonPauseButtonLeft$Top$Width1HeightCaption&PauseTabOrder	VisibleOnClickPauseButtonClick  TButtonAutoStartButtonLeft\Top$Width)HeightCaption&AutoTabOrder
OnClickAutoStartButtonClick  	TCheckBoxLingerCheckBoxLeft�Top.Width0Height	AlignmenttaLeftJustifyCaptionLingerTabOrder   TMemoDisplayMemoLeft TopEWidth(Height� AlignalClientLines.StringsDisplayMemo TabOrder  TWSocketWSocket1LineMode	LineLimit   LineEnd
LineEchoLineEditPrototcp	LocalAddr0.0.0.0	LocalPort0MultiThreadedComponentOptions OnDataAvailableWSocket1DataAvailableOnSessionClosedWSocket1SessionClosedOnSessionConnectedWSocket1SessionConnectedOnDnsLookupDoneWSocket1DnsLookupDoneFlushTimeout<	SendFlagswsSendNormalLingerOnOff
wsLingerOnLingerTimeout 
SocksLevel5SocksAuthenticationsocksNoAuthenticationLeft� Top\   