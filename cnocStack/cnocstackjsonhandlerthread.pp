unit cnocStackJSONHandlerThread;

{ CnocStack JSON handler

  Copyright (C) 2019 Joost van der Sluis (CNOC) joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  cnocStackMessageTypes,
  cnocStackHandlerThread;

type
  IcnocStackJSONRespondToMessage = interface(IcnocStackHandler) ['{30A5A36F-3EE8-481F-80E1-CD67271946F0}']
    procedure RespondToJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; out Response: TJSONData; var Handled: Boolean);
  end;

  IcnocStackJSONHandleMessage = interface(IcnocStackHandler) ['{80FDEFB8-432D-43E5-B467-8D99E72A0352}']
    procedure HandleJSONMessage(const IncomingMessage: PcnocStackMessage; const JSONData: TJSONObject; var Handled: Boolean);
  end;

  { TcnocStackJSONHandlerThread }

  TcnocStackJSONHandlerThread = class(TcnocStackHandlerThread)
  protected
    procedure DoRespondToMessage(const IncomingMessage: PcnocStackMessage; var Response: TBytes; var Handled: Boolean); override;
    procedure DoHandleMessage(const IncomingMessage: PcnocStackMessage; var Handled: Boolean); override;
  end;


implementation

{ TcnocStackJSONHandlerThread }

procedure TcnocStackJSONHandlerThread.DoRespondToMessage(const IncomingMessage: PcnocStackMessage; var Response: TBytes; var Handled: Boolean);
var
  Handler: TObject;
  RespHandler: IcnocStackRespondToMessage;
  MessHandler: IcnocStackHandleMessage;
  JSONRespHandler: IcnocStackJSONRespondToMessage;
  JSONStr: TJSONStringType;
  JSONData, JSONResponse: TJSONData;
  JSONObject: TJSONObject;
  MessageName: TJSONUnicodeStringType;
begin
  if Handled then
    Exit;
  JSONStr := IncomingMessage^.GetContentsAsAnsiString;
  JSONData := GetJSON(JSONStr);
  if Assigned(JSONData) and (JSONData.JSONType = jtObject) then
    begin
    JSONObject := TJSONObject(JSONData);
    MessageName := JSONObject.Get('name', '');
    if FHandlerMap.TryGetValue(MessageName, Handler) then
      begin
      if supports(Handler, IcnocStackJSONRespondToMessage, JSONRespHandler) then
        begin
        JSONRespHandler.RespondToJSONMessage(IncomingMessage, JSONObject, JSONResponse, Handled);
        if Assigned(JSONResponse) then
          begin
          try
            JSONStr := JSONResponse.AsJSON;
          finally
            JSONResponse.Free;
          end;
          SetLength(Response, length(JSONStr));
          move(JSONStr[1], Response[0], Length(JSONStr));
          end;
        end;
      if supports(Handler, IcnocStackRespondToMessage, RespHandler) then
        RespHandler.RespondToMessage(IncomingMessage, Response, Handled);
      if supports(Handler, IcnocStackHandleMessage, MessHandler) then
        MessHandler.HandleMessage(FBinaryClient, IncomingMessage, Handled);
      end;
    end;
end;

procedure TcnocStackJSONHandlerThread.DoHandleMessage(const IncomingMessage: PcnocStackMessage; var Handled: Boolean);
var
  Handler: TObject;
  MessHandler: IcnocStackHandleMessage;
  JSONHandler: IcnocStackJSONHandleMessage;
  JSONStr: TJSONStringType;
  JSONData, JSONResponse: TJSONData;
  JSONObject: TJSONObject;
  MessageName: TJSONUnicodeStringType;
begin
  if Handled then
    Exit;
  JSONStr := IncomingMessage^.GetContentsAsAnsiString;
  JSONData := GetJSON(JSONStr);
  if Assigned(JSONData) and (JSONData.JSONType = jtObject) then
    begin
    JSONObject := TJSONObject(JSONData);
    MessageName := JSONObject.Get('name', '');
    if FHandlerMap.TryGetValue(MessageName, Handler) then
      begin
      if supports(Handler, IcnocStackJSONHandleMessage, JSONHandler) then
        begin
        JSONHandler.HandleJSONMessage(IncomingMessage, JSONObject, Handled);
        end;
      if supports(Handler, IcnocStackHandleMessage, MessHandler) then
        MessHandler.HandleMessage(FBinaryClient, IncomingMessage, Handled);
      end;
    end;
end;

end.

