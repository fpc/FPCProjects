// info
{: informations on opengl driver.<p>

      $Log: info.pas,v $
      Revision 1.1  2006/01/10 20:50:44  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.1  2006/01/09 21:02:30  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:38  z0m3ie
      - added automatical generated History from CVS

	<b>History : </b><font size=-1><ul>
      <li>08/07/04 - lr - suppress commctrl in the uses of linux		
      <li>06/07/04 - lr - display some infos for linux	
      <li>03/07/04 - lr - make change for linux
      <li>21/02/04 - eg - added extensions popup menu and hyperlink to
                          delphi3d's hardware registry
      <li>08/02/04 - nelc - added option for modal
      <li>09/09/03 - nelc - added renderer info
      <li>26/06/03 - eg - double-clicking an extension will now go to its opengl
                          registry webpage 
      <li>22/05/03 - eg - added texture units info
      <li>21/07/02 - eg - no longer modal
      <li>03/02/02 - eg - infoform registration mechanism
      <li>24/08/01 - eg - compatibility with new buffer classes
		<li>17/04/00 - eg - creation of header, minor layout changes
	</ul></font>
}
unit info;

interface

{$i GLScene.inc}

{$IFDEF MSWINDOWS}
uses
  {$ifdef fpc}lresources,{$endif}
  windows, forms, glscene, classes, controls, buttons, stdctrls, comctrls, 
  extctrls, graphics, menus;
{$endif}
{$ifdef linux}
uses
  glscene,classes,
{$ifndef fpc}
  xlib,qforms, qcontrols, qbuttons, qstdctrls, qcomctrls,
  qextctrls, qgraphics, qmenus; 
{$else}
  forms,controls,buttons,stdctrls,comctrls,extctrls,graphics,menus,lresources;
{$endif}
{$endif}


type

  TInfoForm = class(TForm)
    CloseButton: TSpeedButton;
    PageControl: TPageControl;
    Sheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Sheet2: TTabSheet;
    Sheet3: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    VendorLabel: TLabel;
    AccLabel: TLabel;
    VersionLabel: TLabel;
    CopyLabel: TLabel;
    DoubleLabel: TLabel;
    Label7: TLabel;
    StereoLabel: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    ColorLabel: TLabel;
    DepthLabel: TLabel;
    StencilLabel: TLabel;
    AuxLabel: TLabel;
    AccumLabel: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    ClipLabel: TLabel;
    EvalLabel: TLabel;
    ListLabel: TLabel;
    LightLabel: TLabel;
    Label23: TLabel;
    ModelLabel: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    NameLabel: TLabel;
    PixelLabel: TLabel;
    ProjLabel: TLabel;
    TexStackLabel: TLabel;
    TexSizeLabel: TLabel;
    Label35: TLabel;
    ViewLabel: TLabel;
    SubLabel: TLabel;
    Label37: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label18: TLabel;
    OverlayLabel: TLabel;
    UnderlayLabel: TLabel;
    Label20: TLabel;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    TexUnitsLabel: TLabel;
    Extensions: TListBox;
    Label13: TLabel;
    RendererLabel: TLabel;
    PMWebLink: TPopupMenu;
    MIRegistryLink: TMenuItem;
    MIDelphi3D: TMenuItem;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExtensionsDblClick(Sender: TObject);
    procedure ExtensionsClick(Sender: TObject);
    procedure ExtensionsKeyPress(Sender: TObject; var Key: Char);
    procedure MIDelphi3DClick(Sender: TObject);
  public
    procedure GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
  end;

implementation

uses
  opengl1x, sysutils, glcrossplatform;

{$IFNDEF FPC}
{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF}

{$R Info.res}
{$ENDIF}

// ShowInfoForm
//
procedure ShowInfoForm(aSceneBuffer : TGLSceneBuffer; Modal : boolean);
var
   infoForm: TInfoForm;
begin
   infoForm:=TInfoForm.Create(nil);
   try
      infoForm.GetInfoFrom(aSceneBuffer);
      with infoForm do if Modal then ShowModal else Show;
   except
      infoForm.Free;
      raise;
   end;
end;

// CloseButtonClick
//
procedure TInfoForm.CloseButtonClick(Sender: TObject);
begin
   Close;
end;

// GetInfoFrom
//
procedure TInfoForm.GetInfoFrom(aSceneBuffer : TGLSceneBuffer);
{$IFDEF MSWINDOWS}
const
   DRIVER_MASK = PFD_GENERIC_FORMAT or PFD_GENERIC_ACCELERATED;
{$ENDIF}
var
{$IFDEF MSWINDOWS}
   pfd            : TPixelformatDescriptor;
   pixelFormat    : Integer;
{$ENDIF}
   i              : Integer;
   ExtStr         : String;

   procedure IntLimitToLabel(const aLabel : TLabel; const aLimit : TLimitType);
   begin
      aLabel.Caption:=IntToStr(aSceneBuffer.LimitOf[aLimit]);
   end;

begin
	Caption:=Caption+' (current context in '+(aSceneBuffer.Owner as TComponent).Name+')';
	with aSceneBuffer do begin
      // common properties
      VendorLabel.Caption:=StrPas(PChar(glGetString(GL_VENDOR)));
      RendererLabel.Caption:=StrPas(PChar(glGetString(GL_RENDERER)));
      {$IFDEF MSWINDOWS}
      PixelFormat:=GetPixelFormat(Canvas.Handle);
      DescribePixelFormat(Canvas.Handle,PixelFormat,SizeOf(pfd), PFD);
      // figure out the driver type
      if (DRIVER_MASK and pfd.dwFlags) = 0 then AccLabel.Caption:='Installable Client Driver'
        else if (DRIVER_MASK and pfd.dwFlags ) = DRIVER_MASK then AccLabel.Caption:='Mini-Client Driver'
          else if (DRIVER_MASK and pfd.dwFlags) = PFD_GENERIC_FORMAT then AccLabel.Caption:='Generic Software Driver';
      {$ENDIF}
      VersionLabel.Caption:=StrPas(PChar(glGetString(GL_VERSION)));
      ExtStr:=PChar(glGetString(GL_EXTENSIONS));
      Extensions.Clear;
      while Length(ExtStr) > 0 do begin
        I:=Pos(' ',ExtStr);
        if I = 0 then I:=255;
        Extensions.Items.Add(Copy(ExtStr,1,I-1));
        Delete(ExtStr,1,I);
      end;
      {$IFDEF MSWINDOWS}
      if DoubleBuffered then begin
        DoubleLabel.Caption:='yes';
        CopyLabel.Caption:='';
        if (pfd.dwFlags and PFD_SWAP_EXCHANGE) > 0 then CopyLabel.Caption:='exchange';
        if Length(CopyLabel.Caption) > 0 then CopyLabel.Caption:=CopyLabel.Caption+', ';
        if (pfd.dwFlags and PFD_SWAP_COPY) > 0 then CopyLabel.Caption:=CopyLabel.Caption+'copy';
        if Length(CopyLabel.Caption) = 0 then CopyLabel.Caption:='no info available';
      end else begin
        DoubleLabel.Caption:='no';
        CopyLabel.Caption:='n/a';
      end;
      if (pfd.dwFlags and PFD_STEREO) > 0 then
         StereoLabel.Caption:='yes'
      else StereoLabel.Caption:='no';
      {$ENDIF}
      // buffer and pixel depths
      ColorLabel.Caption:=Format('red: %d,  green: %d,  blue: %d,  alpha: %d  bits',
                                 [LimitOf[limRedBits], LimitOf[limGreenBits],
                                  LimitOf[limBlueBits], LimitOf[limAlphaBits]]);
      DepthLabel.Caption:=Format('%d bits', [LimitOf[limDepthBits]]);
      StencilLabel.Caption:=Format('%d bits', [LimitOf[limStencilBits]]);
      AccumLabel.Caption:=Format('red: %d,  green: %d,  blue: %d,  alpha: %d  bits',
                                 [LimitOf[limAccumRedBits],LimitOf[limAccumGreenBits],
                                  LimitOf[limAccumBlueBits],LimitOf[limAccumAlphaBits]]);
      IntLimitToLabel(AuxLabel, limAuxBuffers);
      IntLimitToLabel(SubLabel, limSubpixelBits);
      {$IFDEF MSWINDOWS}
      OverlayLabel.Caption:=IntToStr(pfd.bReserved and 7);
      UnderlayLabel.Caption:=IntToStr(pfd.bReserved shr 3);
      {$ENDIF}

      // Maximum values
      IntLimitToLabel(ClipLabel, limClipPlanes);
      IntLimitToLabel(EvalLabel, limEvalOrder);
      IntLimitToLabel(LightLabel, limLights);
      IntLimitToLabel(ListLabel, limListNesting);
      IntLimitToLabel(ModelLabel, limModelViewStack);
      IntLimitToLabel(ViewLabel, limViewportDims);

      IntLimitToLabel(NameLabel, limNameStack);
      IntLimitToLabel(PixelLabel, limPixelMapTable);
      IntLimitToLabel(ProjLabel, limProjectionStack);
      IntLimitToLabel(TexSizeLabel, limTextureSize);
      IntLimitToLabel(TexStackLabel, limTextureStack);
      IntLimitToLabel(TexUnitsLabel, limNbTextureUnits);
   end;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormCreate(Sender: TObject);

// quite much code just to display a background image in the
// page control with correct windows colors

var I         : Integer;
	 NewR,
	 NewG,
    NewB      : Integer;
    r,g,b     : Byte;
    {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    X, Y      : Integer;
    {$ENDIF}
    {$ENDIF}
    BM        : TBitmap;
	 OldColors,
    NewColors  : array[Byte] of TColor;

begin
  PageControl.ActivePage:=Sheet1;
  r:=GetRValue(ColorToRGB(clBtnFace));
  g:=GetGValue(ColorToRGB(clBtnFace));
  b:=GetBValue(ColorToRGB(clBtnFace));
  BM:=TBitmap.Create;
  // translate bitmap colors from gray-ramp to a clBtnFace-ramp 
  try
    for I:=0 to 255 do
    begin
      OldColors[I]:=RGB(I,I,I);
      // 175 instead of 255 to make the image a bit lighter
      NewR:=Round(I*r/175); if NewR > r then NewR:=r;
      NewG:=Round(I*g/175); if NewG > g then NewG:=g;
      NewB:=Round(I*b/175); if NewB > b then NewB:=b;
      NewColors[I]:=RGB(NewR,NewG,NewB);
    end;
    {$IFDEF MSWINDOWS}
    {$IFNDEF FPC}
    BM.Handle:=CreateMappedRes(HInstance,'INFO_BACK',OldColors,NewColors);
	 for Y:=0 to Image1.Height div BM.Height do
      for X:=0 to Image1.Width div BM.Width do Image1.Canvas.Draw(X*BM.Width,Y*BM.Height,BM);
    {$ENDIF}{$ENDIF}
    Image2.Picture:=Image1.Picture;
    Image3.Picture:=Image1.Picture;
  finally
    BM.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure TInfoForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then Close;
end;

procedure TInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   Release;
end;

procedure TInfoForm.ExtensionsDblClick(Sender: TObject);
var
   p : Integer;
   url, buf : String;
begin
   with Extensions do begin
      if ItemIndex<0 then Exit;
      url:=Items[ItemIndex];
   end;
   p:=Pos('_', url);
   buf:=Copy(url, 1, p-1);
   url:=Copy(url, p+1, 255);
   if (buf<>'GL') and (buf<>'WGL') and (buf<>'GLX') then Exit;
   p:=Pos('_', url);
   buf:=Copy(url, 1, p-1);
   url:= 'http://oss.sgi.com/projects/ogl-sample/registry/'
        +buf+'/'+Copy(url, p+1, 255)+'.txt';
   ShowHTMLUrl(url);
end;

procedure TInfoForm.MIDelphi3DClick(Sender: TObject);
var
   url : String;
begin
   with Extensions do begin
      if ItemIndex<0 then Exit;
      url:='http://www.delphi3d.net/hardware/extsupport.php?extension='+Items[ItemIndex];
   end;
   ShowHTMLUrl(url);
end;

procedure TInfoForm.ExtensionsClick(Sender: TObject);
var
   extName : String;
begin
   if Extensions.ItemIndex<0 then
      Extensions.PopupMenu:=nil
   else begin
      Extensions.PopupMenu:=PMWebLink;
      extName:=Extensions.Items[Extensions.ItemIndex];
      MIRegistryLink.Caption:='View OpenGL Extension Registry for '+extName;
      MIDelphi3D.Caption:='View Delphi3D Hardware Registry for '+extName;
   end;
end;

procedure TInfoForm.ExtensionsKeyPress(Sender: TObject; var Key: Char);
begin
   ExtensionsClick(Sender);
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
initialization
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

   {$IFDEF FPC}{$i Info.lrs}{$ENDIF}

   RegisterInfoForm(ShowInfoForm);

end.




