Unit conkbd;

interface
{$i icsdef.inc}

Uses Sysutils, Windows, Classes,Messages;


type
    TEventKeyDownProc   = procedure (MsgRec:TMsg);   // native
    TEventCliDisconProc = procedure (var MsgRec : TMsg);
    TEventStdProc       = function  (var MsgRec : TMsg) : Integer;
    TEventCleanUpData   = procedure;


    TKbdEventRec = Record
                     EventClientDisconnect : TEventCliDisconProc;
                     EventCreate           : TEventStdProc;
                     EventKeyDown          : TEventKeyDownProc;
                     EventCleanUpData      : TEventCleanUpData;
                     EventOther            : TEventStdProc;
                     end;

    TKeyboardThread = class (TThread)
    public
        procedure Execute; override;
    end;

Procedure Initialise(kMain:HWND;Const kevents : TKbdEventRec;UseCtrlH:Boolean;CONST OtherEvents:Array of Integer);

implementation

Type TDwArr   =   Array[0..999] Of DWord;
     PDwArr   =   ^TDwArr;

Var KbdHwndMain     : HWND;
    KbdCallback     : TKbdEventRec;
    KbdLoaded       : Boolean;
    KbdCustomEvents : Integer;
    KbdEvents       : PDwArr;

function CtrlHandlerRoutine(CtrlType : DWORD) : DWORD; stdcall;
begin
    case CtrlType of
    CTRL_C_EVENT,            // User hit CTRL-C
    CTRL_BREAK_EVENT,        // User hit CTRL-BREAK
    CTRL_LOGOFF_EVENT,       // User log off his session
    CTRL_CLOSE_EVENT,        // Close signal
    CTRL_SHUTDOWN_EVENT :    // Window shutdown signal
        begin
            Result := 1;
            PostMessage(kbdhWndMain, WM_QUIT, 0, 0);
        end;
    else
        Result := 0;
    end;
end;

function MyWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    MsgRec : TMsg;
    I      : Integer;
    Found  : Boolean;
begin
    Result := 0;  // This means we handled the message
    try
        MsgRec.hwnd    := ahWnd;
        MsgRec.message := auMsg;
        MsgRec.wParam  := awParam;
        MsgRec.lParam  := alParam;

        case auMsg of

        WM_CREATE: If KbdLoaded AND Assigned(kbdCallback.EventCreate) Then
                   result:=KbdCallback.EventCreate(MsgRec);
        WM_CLOSE:
            begin
                WriteLn('Closing');
                DestroyWindow(ahWnd);
            end;
        WM_DESTROY:
            begin
                WriteLn('Destroying');
                If KbdLoaded AND Assigned(kbdCallback.EventCleanUpData) Then
                      KbdCallback.EventCleanUpData;
            end;
        WM_KEYDOWN: If KbdLoaded AND Assigned(kbdCallback.EventKeyDown) Then
                      KbdCallback.EventKeyDown(MsgRec);
        else
          Begin
            Found:=False;
            If KbdCustomEvents>0 Then
               Begin
                   i:=0;
                   Repeat
                     If (auMsg=KbdEvents^[i]) AND Assigned(kbdCallback.EventOther) Then
                       Begin
                          Found:=True;
                          Result:=KbdCallBack.EventOther(MsgRec);
                       End;
                     Inc(I);
                   Until (i=KbdCustomEvents) or Found;
               End;
            If Not Found Then
               Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
          End;
        end;
    except
        on E:Exception do
            WriteLn('Exception ' + E.ClassName + ': ' + E.Message);
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Console mode applications do not receive keyboard messages as GUI apps.   }
{ We use a thread to wait for keyboard activity and generate keyboard       }
{ messages as in a GUI application.                                         }
procedure TKeyboardThread.Execute;
var
    hConsole    : THandle;
    Status      : DWORD;
    InputBuffer : TInputRecord;
    KeyEvent    : TKeyEventRecord;
    Count       : DWORD;
begin
    hConsole := GetStdHandle(STD_INPUT_HANDLE);
    while not Terminated do begin
        Status := WaitForSingleObject(hConsole, 1000);
        if Status = WAIT_OBJECT_0 then begin
            if ReadConsoleInput(hConsole, InputBuffer, 1, Count) then begin
                if InputBuffer.EventType = KEY_EVENT then begin
{$IFDEF VER90}  { Delphi 2 }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER93}  { Bcb 1    }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER100} { Delphi 3 }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$IFDEF VER110} { Bcb 3    }
                    KeyEvent := InputBuffer.KeyEvent;
{$ELSE}
{$ENDIF}
{ Starting from Delphi 4 and Bcb4, they changed definition }
                    KeyEvent := InputBuffer.Event.KeyEvent;
{$ENDIF}
{$ENDIF}
{$ENDIF}
                    if KeyEvent.bKeyDown then begin
                        PostMessage(kbdhWndMain, WM_KEYDOWN,
                                    KeyEvent.wVirtualKeyCode,
                                    KeyEvent.wRepeatCount +
                                    (KeyEvent.wVirtualScanCode shl 16));
                    end
                    else begin
                        PostMessage(kbdhWndMain, WM_KEYUP,
                                    KeyEvent.wVirtualKeyCode,
                                    KeyEvent.wRepeatCount +
                                    (KeyEvent.wVirtualScanCode shl 16));
                    end;
                end;
            end;
        end;
    end;
end;

Procedure Initialise(kMain:HWND;Const kevents : TKbdEventRec;UseCtrlH:Boolean;CONST OtherEvents:Array of Integer);

Begin
   kbdHwndMain:=kMain;
   KbdCallback:=kevents;
   KbdLoaded:=True;
   KbdCustomEvents:=HIGH(OtherEvents);
   If KbdCustomEvents>0 Then
      Begin
         GetMem(KbdEvents,Sizeof(DWord)*KbdCustomEvents);
         Move(OtherEvents,KbdEvents^[0],SIZEOF(DWord)*KbdCustomEvents);
      End;
   If UseCtrlH Then
      SetConsoleCtrlHandler(@CtrlHandlerRoutine, TRUE);
End;

begin
 KbdLoaded:=False;
end.
