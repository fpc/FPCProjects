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
//   Image list implementation for GTK+
// ===================================================================

constructor TImageList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  priv := TList.Create;
  FWidth := 16;
  FHeight := 15;
end;

destructor TImageList.Destroy;
begin
  priv.Free;
  inherited Destroy;
end;

procedure TImageList.Add(AImage, AMask: TBitmap);
var
  x: Integer;
begin
  x := 0;
  while x < AImage.Width do begin
    // ###
    Inc(x, FWidth);
  end;
end;

procedure TImageList.AddMasked(AImage: TBitmap; AMaskColor: TColor);
var
  x, mw, mx, my: Integer;
  ImgCanvas: TGdkPixmapCanvas;
  MaskData, p: PChar;
  b: Byte;
  item: PImageListItem;
begin
  BitmapWndNeeded;
  x := 0;
  while x < AImage.Width do begin
    ImgCanvas := TGdkPixmapCanvas.Create(gBitmapWnd^.window, FWidth, FHeight, -1);
    ImgCanvas.DrawBitmap(0, 0, AImage, x, 0, FWidth, FHeight);
    mw := (FWidth + 7) and not 7;
    GetMem(MaskData, mw * FHeight);
    p := MaskData;
    for my := 0 to FHeight - 1 do begin
      for mx := 0 to FWidth - 1 do begin
        if (mx and 7) = 0 then b := 0;
	if AImage.RGB[x + mx, my] <> AMaskColor then
	  b := (b shr 1) or 128
	else
	  b := b shr 1;
	if ((mx and 7) = 7) or (mx = FWidth - 1) then begin
          p[0] := Chr(b);
	  Inc(p);
	end;
      end;
    end;
    New(item);
    item^.Mask := gdk_bitmap_create_from_data(nil, MaskData, FWidth, FHeight);
    FreeMem(MaskData);
    item^.Image := ImgCanvas.pixmap;
    priv.Add(item);
    //priv.Add(gtk_pixmap_new(ImgCanvas.pixmap, mask));
    Inc(x, FWidth);
  end;
end;


{
  $Log$
  Revision 1.1  2000/01/24 00:15:23  sg
  * First version

}
