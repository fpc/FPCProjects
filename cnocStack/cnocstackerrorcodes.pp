unit cnocStackErrorCodes;

{ CnocStack's overview of error-messages

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

interface

uses
  Classes,
  SysUtils;

type
  TcnocStackErrorCodes = (
    ecNone,
    ecInvalidMessageType,
    ecException,
    ecPushMessageTimeout,
    ecPushMessageAbandoned,
    ecPushMessageError,
    ecEmptyStackName,
    ecStackDoesNotExist,
    ecNotConnected,
    ecTimeoutWhileWaitingForDirectAck,
    ecTimeoutWhileWaitingForDirectResponse,
    ecInvalidControlFlow,
    ecMissingRoutingInfo,
    ecSenderNotAvailable,
    ecErrorWhileWaitingForDirectAck,
    ecAbandonedWhileWaitingForDirectAck,
    ecAbandonedWhileWaitingForDirectResponse,
    ecErrorWhileWaitingForDirectResponse,
    ecFailedToSendData
  );

const
  cnocStackErrorMessages: array[TcnocStackErrorCodes] of string = (
    '',
    'Invalid message type',
    'Exception',
    'Timeout on pushing the message onto the stack',
    'Failed to push message onto the stack. Stack has been abandoned (shuttong down)',
    'Error while pushing message onto the stack.',
    'Empty stack name',
    'Stack does not exist',
    'Not connection with server',
    'Timeout while waiting for ack',
    'Timeout while waiting for response',
    'Received control-message in the wrong order',
    'Routing information is missing',
    'The adressee of the message does not exist',
    'Error while waiting for direct ack',
    'Shutting down while waiting for direct ack',
    'Shutting down while waiting for direct response',
    'Error while waiting for direct response',
    'Connection problem while trying to send a message to the server'
  );
  cnocStackClientErrorCodes = [
    ecNotConnected ,
    ecFailedToSendData
  ];

implementation

end.

