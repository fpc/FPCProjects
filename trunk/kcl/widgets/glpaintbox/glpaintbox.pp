{
    $Id$

    GLPaintBox - OpenGL Paintbox Widget for KCL
    Copyright (C) 1999-2000  Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


{$MODE objfpc}

unit GLPaintBox;
interface
uses Classes, xlib, xutil, GL, KCL;
type

  TGLPaintBox = class(TPaintBox)
  protected
    xdisplay: PDisplay;
    glxcontext: GLXContext;
    function MakeCurrent: Boolean;

    FOnInitGL: TNotifyEvent;
    procedure OnFinishCreation; override;
    procedure DoRecalcLayout; override;
  public
    function  BeginGL: Boolean;
    procedure EndGL;
    procedure SwapBuffers;
  published
    property OnInitGL: TNotifyEvent read FOnInitGL write FOnInitGL;
  end;


procedure Register;


implementation
uses gdk, gtk;


function TGLPaintBox.MakeCurrent: Boolean;
begin
  if glxcontext = nil then exit(False);
  Result := glXMakeCurrent(xdisplay, PGdkWindowPrivate(Handle^.window)^.xwindow, glxcontext);
end;

function TGLPaintBox_Realize(GtkWidget: PGtkWidget;
  PaintBox: TGLPaintBox): Boolean; cdecl;
begin
  if PaintBox.MakeCurrent then begin
    glViewport(0, 0, PaintBox.Width, PaintBox.Height);
    if Assigned(PaintBox.OnInitGL) then
      PaintBox.OnInitGL(PaintBox);
    glFlush;
  end;
  Result := True;
end;

function get_xvisualinfo(visual: PGdkVisual): PXVisualInfo;
var
  dpy: PDisplay;
  vinfo_template: TXVisualInfo;
  vi: PXVisualInfo;
  nitems_return: Integer;
begin
  dpy := GDK_GET_XDISPLAY;

  vinfo_template.visual   := GDK_VISUAL_XVISUAL(PGdkVisualPrivate(visual));
  vinfo_template.visualid := XVisualIDFromVisual(vinfo_template.visual);
  vinfo_template.depth    := visual^.depth;
  vi := XGetVisualInfo(dpy, VisualIDMask or VisualDepthMask, @vinfo_template, nitems_return);

  Result := vi;
end;


procedure TGLPaintBox.OnFinishCreation;
const
  attrlist: array[0..4] of LongInt =
    (GLX_RGBA, GLX_DOUBLEBUFFER, GLX_DEPTH_SIZE, 16, 0);
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  visual: PGdkVisual;
begin

  if not GL.GLXInitialized then
    if not InitGLX then begin
      WriteLn(StdErr, '[GL Paintbox] Fatal error: Couldn''t load OpenGL GLX module!');
      exit;
    end;

  dpy := GDK_GET_XDISPLAY;

  vi := glXChooseVisual(dpy, XDefaultScreen(dpy), attrList[0]);
  if vi = nil then
    visual := nil
  else begin
    visual := gdkx_visual_get(vi^.visualid);
    XFree(vi);
    if visual = nil then exit;
  end;

  vi := get_xvisualinfo(visual);

  glxcontext := glXCreateContext(dpy, vi, nil, True);

  XFree(vi);

  if glxcontext = nil then exit;
  xdisplay := dpy;

  // use colormap and visual suitable for OpenGL rendering
  gtk_widget_push_colormap(gdk_colormap_new(visual, 1));
  gtk_widget_push_visual(visual);

  // pop back defaults
  gtk_widget_pop_visual;
  gtk_widget_pop_colormap;

  inherited OnFinishCreation;

  gtk_signal_connect(PGtkObject(Handle), 'realize',
    GTK_SIGNAL_FUNC(@TGLPaintBox_Realize), Self);
end;

procedure TGLPaintBox.DoRecalcLayout;
begin
  inherited DoRecalcLayout;
  if GTK_WIDGET_REALIZED(Handle) and MakeCurrent then begin
    glViewport(0, 0, Width, Height);
    glFlush;
  end;
end;

function TGLPaintBox.BeginGL: Boolean;
begin
  if not GTK_WIDGET_REALIZED(Handle) then exit(False);
  Result := MakeCurrent;
end;

procedure TGLPaintBox.EndGL;
begin
  glFlush;
end;

procedure TGLPaintBox.SwapBuffers;
var
  drawable: PGdkWindowPrivate;
begin
  drawable := PGdkWindowPrivate(Handle^.window);
  glXSwapBuffers(GDK_WINDOW_XDISPLAY(drawable), GDK_WINDOW_XWINDOW(drawable));
end;



procedure Register;
var
  AdvComponents: array[0..0] of TComponentClass;
begin
  AdvComponents[0]  := TGLPaintBox;
  RegisterComponents('Advanced', AdvComponents);
end;

end.


{
  $Log$
  Revision 1.4  2000/03/06 15:57:54  peter
    * updated for new layout

  Revision 1.3  2000/02/10 18:49:50  sg
  * Adapted to new layouting methods

  Revision 1.2  2000/01/26 21:20:40  peter
    * Makefile updates
    * glpaintbox fixed for Handle

  Revision 1.1  1999/12/31 19:21:42  sg
  * Initial version for the new KCL

}
