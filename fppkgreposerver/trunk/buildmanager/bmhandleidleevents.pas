unit bmHandleIdleEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils;

type

  { TbmIdleEventsHander }

  TbmIdleEventsHander = class
  public
    procedure HandleOnIdleEvents(Sender: TObject);
  end;


implementation

{ TbmIdleEventsHander }

procedure TbmIdleEventsHander.HandleOnIdleEvents(Sender: TObject);
begin
  try
    CheckSynchronize(0);
  except
    // log error? At least do not raise it, or else the application will terminate.
  end;
end;

end.

