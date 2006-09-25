{ lNetComponents v0.4

  CopyRight (C) 2004-2006 Ales Katona

  This library is Free software; you can rediStribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is diStributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; withOut even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a Copy of the GNU Library General Public License
  along with This library; if not, Write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  This license has been modified. See File LICENSE.ADDON for more inFormation.
  Should you find these sources without a LICENSE File, please contact
  me at ales@chello.sk
}

unit LCLNet;

{$mode objfpc}{$H+}

interface

uses
  InterfaceBase, LCLIntf, lNet, lEvents;

type
  PLCLHandleInfo = ^TLCLHandleInfo;
  TLCLHandleInfo = record
    Handle: TLHandle;
    Flags: DWord;
    EventHandle: PEventHandler;
    WinObject: THandle;
  end;

  { TLCLEventer }

  TLCLEventer = class(TLEventer)
   protected
    procedure HandleIgnoreError(aHandle: TLHandle); override;
    procedure HandleIgnoreWrite(aHandle: TLHandle); override;
    procedure HandleIgnoreRead(aHandle: TLHandle); override;
    procedure HandleEvents(aData: PtrInt; aFlags: DWord);
   public
    function AddHandle(aHandle: TLHandle): Boolean; override;
    procedure UnplugHandle(aHandle: TLHandle); override;
  end;

implementation

{$ifdef LCLWIN32}
  {$i lclwineventer.inc}
{$endif}
{$ifdef LCLGTK}
  {$i lclgtkeventer.inc}
{$endif}
{$ifdef LCLGTK2}
  {$i lclgtkeventer.inc}
{$endif}

end.

