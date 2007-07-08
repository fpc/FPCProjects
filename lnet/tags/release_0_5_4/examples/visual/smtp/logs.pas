unit Logs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormLogs }

  TFormLogs = class(TForm)
    MemoLogs: TMemo;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormLogs: TFormLogs;

implementation

initialization
  {$I logs.lrs}

end.

