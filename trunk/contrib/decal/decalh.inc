{
    $Id$
    Contributed code to Decal. Same license as decal (MPL)
    Copyright (c) 2000 Ross Judson
    FPC mods (c) 2004 Marco van de Voort, member of the Free Pascal 
	development team.

    Central includefile to hide OS differences.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
  Letting code depend on Delphi version is bad, but since FPC is Delphi5
  compatible enough, so I won't bother to restructure it now.
}

{$IFDEF VER100}
{$DEFINE DELPHI3}
{$ENDIF}

{$IFDEF VER110}
{$DEFINE DELPHI3}
{$ENDIF}

{$IFDEF VER120}
{$DEFINE DELPHI4}
{$ENDIF}

{$IFDEF VER130}
{$DEFINE DELPHI5}
{$ENDIF}

{$IFDEF FPC}
{$IFDEF VER1_0}
{$USER THIS IS NOT GOING TO WORK!}
 BOMB!
{$ELSE}
{$mode Delphi}
{$goto on}
{$DEFINE DELPHI5}
{$ifdef Unix}
  {$define UseSysthrds}
{$endif}
{$ENDIF}
{$ENDIF}

// {$DEFINE DEBUG}
{$DEFINE USEPOOLS}

{$IFDEF DELPHI3}
{$ELSE}
{$DEFINE USELONGWORD}
{$ENDIF}

// can't seem to ifopt these
//{$IFOPT WARNINGS+}
{$DEFINE WARNINGSON}
//{$ENDIF}
//{$IFOPT HINTS+}
{$DEFINE HINTSON}
//{$ENDIF}

{
   $Log$
   Revision 1.2  2004/04/12 10:23:21  marco
    * regressdecal now has output

   Revision 1.1  2004/04/03 19:59:22  marco
    * some unix compiling fixes.

}