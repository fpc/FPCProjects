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
//   Splitter Widget Implementation
// ===================================================================

var
  gLastSplitterInfosValid: Boolean;
  gLastSplitterPos, gLastSplitterSize: Integer;


function TSplitter_MotionNotifyEvent(GtkWidget: PGtkWidget; event: PGdkEventMotion;
  splitter: TSplitter): Integer; cdecl;
var
  BorderSize, GutterSize, MinPos, MaxPos: Integer;
begin
  Result := 1;

  if (PGtkPaned(splitter.FHandle)^.flag0 and bm_TGtkPaned_in_drag) <> 0 then begin
    BorderSize := PGtkContainer(splitter.FHandle)^.flag0 and bm_TGtkContainer_border_width;
    GutterSize := PGtkPaned(splitter.FHandle)^.gutter_size;

    if Assigned(splitter.FPane1) then
      MinPos := splitter.FPane1.MinH
    else
      MinPos := 0;

    if Assigned(splitter.FPane1) then
      MaxPos := splitter.FHeight - splitter.FPane2.MinH - BorderSize * 2 - GutterSize
    else
      MaxPos := splitter.FHeight;

    PGtkPaned(splitter.FHandle)^.min_position := MinPos + BorderSize + GutterSize div 2;
    PGtkPaned(splitter.FHandle)^.max_position := MaxPos - BorderSize - GutterSize div 2;
  end;
end;

{procedure TSplitter_SizeAllocate(widget: PGtkWidget; alloc: PGtkAllocation;
  splitter: TSplitter); cdecl;
begin
  WriteLn('Splitter size allocated : ', alloc^.Width, 'x', alloc^.Height);

  if (splitter.FResizePolicy = srFixedPane2) and gLastSplitterInfosValid then begin
    Write('  alte Größe: ', PGtkPaned(splitter.FHandle)^.child1_size);
    PGtkPaned(splitter.FHandle)^.child1_size := gLastSplitterPos + alloc^.Height - gLastSplitterSize;
    WriteLn('  neue Größe: ', PGtkPaned(splitter.FHandle)^.child1_size);
  end;

  gLastSplitterInfosValid := False;

{  if (splitter.FSplitterPos >= 0) and
    (PGtkPaned(splitter.FHandle)^.child1_size <> splitter.FSplitterPos) then begin
    gtk_paned_set_position(PGtkPaned(splitter.FHandle), splitter.FSplitterPos);
    splitter.FSplitterPos := -1;
  end;}
end;}


procedure TSplitter.SetPane1(APane1: TWidget);
begin
  FPane1 := APane1;
  if Assigned(FPane1) then
    FPane1.FParent := Self;
  if FCreationState >= wcsCreationInProgress then begin
    if Assigned(FPane1) then begin
      FPane1.FinishCreation;
      gtk_paned_pack1(PGtkPaned(FHandle), FPane1.FHandle,
        FResizePolicy <> srFixedPane1, True);
    end; // ### else
  end;
end;

procedure TSplitter.SetPane2(APane2: TWidget);
begin
  FPane2 := APane2;
  if Assigned(FPane2) then
    FPane2.FParent := Self;
  if FCreationState >= wcsCreationInProgress then begin
    if Assigned(FPane2) then begin
      FPane2.FinishCreation;
      gtk_paned_pack2(PGtkPaned(FHandle), FPane2.FHandle,
        FResizePolicy <> srFixedPane2, True);
    end; // ### else
  end;
end;

procedure TSplitter.SetOrientation(AOrientation: TSplitterOrientation);
begin
  FOrientation := AOrientation;
end;

procedure TSplitter.SetResizePolicy(APolicy: TSplitterResizePolicy);
begin
  FResizePolicy := APolicy;
end;

procedure TSplitter.SetSplitterPos(ASplitterPos: Integer);
begin
  FSplitterPos := ASplitterPos;
end;

procedure TSplitter.OnFinishCreation;
begin
  if FOrientation = soHorizontal then
    FHandle := gtk_vpaned_new
  else
    FHandle := gtk_hpaned_new;

  gtk_signal_connect(PGtkObject(FHandle), 'motion_notify_event',
    GTK_SIGNAL_FUNC(@TSplitter_MotionNotifyEvent), Self);

//  gtk_signal_connect_after(PGtkObject(FHandle), 'size_allocate',
//    GTK_SIGNAL_FUNC(@TSplitter_SizeAllocate), Self);


  gtk_paned_set_gutter_size(PGtkPaned(FHandle), 10);

  if Assigned(FPane1) then SetPane1(FPane1);
  if Assigned(FPane2) then SetPane2(FPane2);


  inherited OnFinishCreation;
end;

procedure TSplitter.DoRecalcLayout;
var
  BorderSize, GutterSize, NewPos: Integer;
begin
  if FOrientation = soHorizontal then begin
    // We need some space fot the 'gutter' (space between the two panes)
    MinH := 10;
    DefH := MinW;
    MaxH := MinH;
    if Assigned(FPane1) then begin
      Inc(MinW, FPane1.MinW);
      Inc(MinH, FPane1.MinH);
      MaxW := Min(InfiniteSize, MaxW + FPane1.MaxW);
      MaxH := Min(InfiniteSize, MaxH + FPane1.MaxH);
      Inc(DefW, FPane1.DefW);
      Inc(DefH, FPane1.DefH);
    end;
    if Assigned(FPane2) then begin
      Inc(MinW, FPane2.MinW);
      Inc(MinH, FPane2.MinH);
      Inc(MaxW, FPane2.MaxW);
      MaxW := Min(InfiniteSize, MaxW + FPane2.MaxW);
      MaxH := Min(InfiniteSize, MaxH + FPane2.MaxH);
      Inc(DefW, FPane2.DefW);
      Inc(DefH, FPane2.DefH);
    end;
  end;

  BorderSize := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width;
  GutterSize := PGtkPaned(FHandle)^.gutter_size;

{  if FSplitterPos = -1 then begin
    if Assigned(FPane1) then
      if FOrientation = soHorizontal then
        NextPos := FHeight - BorderSize - GutterSize - FPane2.DefH
      else
        NextPos := FPane1.DefW;
    gtk_paned_set_position(PGtkPaned(FHandle), FSplitterPos);
  end;

WriteLn('TSplitter.ApplySize: ', FPane1.DefW, ' x ', FPane1.DefH,
  ' | Self: ', FWidth, 'x', FHeight, ', SplitterPos=', FSplitterPos);}

end;

procedure TSplitter.ApplySize;
{var
  BorderSize, GutterSize, NewPos: Integer;}
begin
{  gLastSplitterInfosValid := True;
  gLastSplitterPos := PGtkPaned(FHandle)^.child1_size;
  gLastSplitterSize := FHeight;


  BorderSize := PGtkContainer(FHandle)^.flag0 and bm_TGtkContainer_border_width;
  GutterSize := PGtkPaned(FHandle)^.gutter_size;


  if FSplitterPos = -1 then begin
    if Assigned(FPane1) then
      if FOrientation = soHorizontal then
        NewPos := FHeight - BorderSize - GutterSize - FPane2.DefH
      else
        NewPos := FPane1.DefW;
    gtk_paned_set_position(PGtkPaned(FHandle), FSplitterPos);
  end;

WriteLn('TSplitter.ApplySize: ', FPane1.DefW, ' x ', FPane1.DefH,
  ' | Self: ', FWidth, 'x', FHeight, ', SplitterPos=', FSplitterPos);

//  
}
  inherited ApplySize;
end;


{
  $Log$
  Revision 1.3  2000/02/10 18:47:22  sg
  * Added layout calculation (not 100% finished yet!)

  Revision 1.2  2000/01/24 00:27:43  sg
  * The "Parent" property of the panes are now set

  Revision 1.1.1.1  1999/12/30 21:33:27  sg
  Initial import

}
