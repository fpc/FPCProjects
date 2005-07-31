
{
  Includefile used by demoes of SDL
  By FPC team (Marco van de Voort, (C) 2003)
  $Id$
}

{$ifdef FPC}
{$mode Delphi}
{$threading on}
{$else}
{$ifdef Linux}
{$define Unix}
{$endif}
{$endif}

{
  $Log$
  Revision 1.2  2004/02/15 21:36:01  marco
   * fixes to new snapshot

  Revision 1.1  2004/01/18 14:34:19  marco
   * initial versions

}