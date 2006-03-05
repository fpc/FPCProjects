{% lepolleventerh.inc included by levents.pas }

{$ifdef Linux}
  PEpollEvent = ^epoll_event;
  TEpollEvent = epoll_event;
  PEpollData = ^epoll_data;
  TEpollData = epoll_data;

  { TLEpollEventer }
  
  TLEpollEventer = class(TLEventer)
   protected
    FTimeout: cInt;
    FEvents: array of TEpollEvent;
    FEpollFD: THandle;
    function GetTimeout: DWord; override;
    procedure SetTimeout(const Value: DWord); override;
    procedure Inflate;
   public
    constructor Create; override;
    destructor Destroy; override;
    function AddHandle(aHandle: TLHandle): Boolean; override;
    function CallAction: Boolean; override;
  end;

{$endif}
