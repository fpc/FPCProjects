{******************************************************************************}
{                                                       	               }
{ Windows Error Codes API interface Unit for Object Pascal                     }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winerror.h, released June 2000. The original Pascal    }
{ code is: WinError.pas, released December 2000. The initial developer of the  }
{ Pascal code is Marcel van Brakel (brakelm@chello.nl).                        }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{ 								               }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{								               }
{ You may retrieve the latest version of this file at the Project JEDI home    }
{ page, located at http://delphi-jedi.org or my personal homepage located at   }
{ http://members.chello.nl/m.vanbrakel2                                        }
{								               }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{ 								               }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{ 								               }
{******************************************************************************}

unit JwaWinError;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinError.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//

const
  FACILITY_WINDOWS_CE = 24;
  FACILITY_WINDOWS = 8;
  FACILITY_URT = 19;
  FACILITY_UMI = 22;
  FACILITY_SXS = 23;
  FACILITY_STORAGE = 3;
  FACILITY_STATE_MANAGEMENT = 34;
  FACILITY_SSPI = 9;
  FACILITY_SCARD = 16;
  FACILITY_SETUPAPI = 15;
  FACILITY_SECURITY = 9;
  FACILITY_RPC = 1;
  FACILITY_WIN32 = 7;
  FACILITY_CONTROL = 10;
  FACILITY_NULL = 0;
  FACILITY_METADIRECTORY = 35;
  FACILITY_MSMQ = 14;
  FACILITY_MEDIASERVER = 13;
  FACILITY_INTERNET = 12;
  FACILITY_ITF = 4;
  FACILITY_HTTP = 25;
  FACILITY_DPLAY = 21;
  FACILITY_DISPATCH = 2;
  FACILITY_CONFIGURATION = 33;
  FACILITY_COMPLUS = 17;
  FACILITY_CERT = 11;
  FACILITY_BACKGROUNDCOPY = 32;
  FACILITY_ACS = 20;
  FACILITY_AAF = 18;


//
// Define the severity codes
//


//
// MessageId: ERROR_SUCCESS
//
// MessageText:
//
//  The operation completed successfully.
//
  ERROR_SUCCESS = DWORD(0);

  NO_ERROR = DWORD(0);  // dderror
  SEC_E_OK = HRESULT($00000000);

//
// MessageId: ERROR_INVALID_FUNCTION
//
// MessageText:
//
//  Incorrect function.
//
  ERROR_INVALID_FUNCTION = DWORD(1);  // dderror

//
// MessageId: ERROR_FILE_NOT_FOUND
//
// MessageText:
//
//  The system cannot find the file specified.
//
  ERROR_FILE_NOT_FOUND = DWORD(2);

//
// MessageId: ERROR_PATH_NOT_FOUND
//
// MessageText:
//
//  The system cannot find the path specified.
//
  ERROR_PATH_NOT_FOUND = DWORD(3);

//
// MessageId: ERROR_TOO_MANY_OPEN_FILES
//
// MessageText:
//
//  The system cannot open the file.
//
  ERROR_TOO_MANY_OPEN_FILES = DWORD(4);

//
// MessageId: ERROR_ACCESS_DENIED
//
// MessageText:
//
//  Access is denied.
//
  ERROR_ACCESS_DENIED = DWORD(5);

//
// MessageId: ERROR_INVALID_HANDLE
//
// MessageText:
//
//  The handle is invalid.
//
  ERROR_INVALID_HANDLE = DWORD(6);

//
// MessageId: ERROR_ARENA_TRASHED
//
// MessageText:
//
//  The storage control blocks were destroyed.
//
  ERROR_ARENA_TRASHED = DWORD(7);

//
// MessageId: ERROR_NOT_ENOUGH_MEMORY
//
// MessageText:
//
//  Not enough storage is available to process this command.
//
  ERROR_NOT_ENOUGH_MEMORY = DWORD(8);  // dderror

//
// MessageId: ERROR_INVALID_BLOCK
//
// MessageText:
//
//  The storage control block address is invalid.
//
  ERROR_INVALID_BLOCK = DWORD(9);

//
// MessageId: ERROR_BAD_ENVIRONMENT
//
// MessageText:
//
//  The environment is incorrect.
//
  ERROR_BAD_ENVIRONMENT = DWORD(10);

//
// MessageId: ERROR_BAD_FORMAT
//
// MessageText:
//
//  An attempt was made to load a program with an incorrect format.
//
  ERROR_BAD_FORMAT = DWORD(11);

//
// MessageId: ERROR_INVALID_ACCESS
//
// MessageText:
//
//  The access code is invalid.
//
  ERROR_INVALID_ACCESS = DWORD(12);

//
// MessageId: ERROR_INVALID_DATA
//
// MessageText:
//
//  The data is invalid.
//
  ERROR_INVALID_DATA = DWORD(13);

//
// MessageId: ERROR_OUTOFMEMORY
//
// MessageText:
//
//  Not enough storage is available to complete this operation.
//
  ERROR_OUTOFMEMORY = DWORD(14);

//
// MessageId: ERROR_INVALID_DRIVE
//
// MessageText:
//
//  The system cannot find the drive specified.
//
  ERROR_INVALID_DRIVE = DWORD(15);

//
// MessageId: ERROR_CURRENT_DIRECTORY
//
// MessageText:
//
//  The directory cannot be removed.
//
  ERROR_CURRENT_DIRECTORY = DWORD(16);

//
// MessageId: ERROR_NOT_SAME_DEVICE
//
// MessageText:
//
//  The system cannot move the file to a different disk drive.
//
  ERROR_NOT_SAME_DEVICE = DWORD(17);

//
// MessageId: ERROR_NO_MORE_FILES
//
// MessageText:
//
//  There are no more files.
//
  ERROR_NO_MORE_FILES = DWORD(18);

//
// MessageId: ERROR_WRITE_PROTECT
//
// MessageText:
//
//  The media is write protected.
//
  ERROR_WRITE_PROTECT = DWORD(19);

//
// MessageId: ERROR_BAD_UNIT
//
// MessageText:
//
//  The system cannot find the device specified.
//
  ERROR_BAD_UNIT = DWORD(20);

//
// MessageId: ERROR_NOT_READY
//
// MessageText:
//
//  The device is not ready.
//
  ERROR_NOT_READY = DWORD(21);

//
// MessageId: ERROR_BAD_COMMAND
//
// MessageText:
//
//  The device does not recognize the command.
//
  ERROR_BAD_COMMAND = DWORD(22);

//
// MessageId: ERROR_CRC
//
// MessageText:
//
//  Data error (cyclic redundancy check).
//
  ERROR_CRC = DWORD(23);

//
// MessageId: ERROR_BAD_LENGTH
//
// MessageText:
//
//  The program issued a command but the command length is incorrect.
//
  ERROR_BAD_LENGTH = DWORD(24);

//
// MessageId: ERROR_SEEK
//
// MessageText:
//
//  The drive cannot locate a specific area or track on the disk.
//
  ERROR_SEEK = DWORD(25);

//
// MessageId: ERROR_NOT_DOS_DISK
//
// MessageText:
//
//  The specified disk or diskette cannot be accessed.
//
  ERROR_NOT_DOS_DISK = DWORD(26);

//
// MessageId: ERROR_SECTOR_NOT_FOUND
//
// MessageText:
//
//  The drive cannot find the sector requested.
//
  ERROR_SECTOR_NOT_FOUND = DWORD(27);

//
// MessageId: ERROR_OUT_OF_PAPER
//
// MessageText:
//
//  The printer is out of paper.
//
  ERROR_OUT_OF_PAPER = DWORD(28);

//
// MessageId: ERROR_WRITE_FAULT
//
// MessageText:
//
//  The system cannot write to the specified device.
//
  ERROR_WRITE_FAULT = DWORD(29);

//
// MessageId: ERROR_READ_FAULT
//
// MessageText:
//
//  The system cannot read from the specified device.
//
  ERROR_READ_FAULT = DWORD(30);

//
// MessageId: ERROR_GEN_FAILURE
//
// MessageText:
//
//  A device attached to the system is not functioning.
//
  ERROR_GEN_FAILURE = DWORD(31);

//
// MessageId: ERROR_SHARING_VIOLATION
//
// MessageText:
//
//  The process cannot access the file because it is being used by another process.
//
  ERROR_SHARING_VIOLATION = DWORD(32);

//
// MessageId: ERROR_LOCK_VIOLATION
//
// MessageText:
//
//  The process cannot access the file because another process has locked a portion of the file.
//
  ERROR_LOCK_VIOLATION = DWORD(33);

//
// MessageId: ERROR_WRONG_DISK
//
// MessageText:
//
//  The wrong diskette is in the drive.
//  Insert %2 (Volume Serial Number: %3) into drive %1.
//
  ERROR_WRONG_DISK = DWORD(34);

//
// MessageId: ERROR_SHARING_BUFFER_EXCEEDED
//
// MessageText:
//
//  Too many files opened for sharing.
//
  ERROR_SHARING_BUFFER_EXCEEDED = DWORD(36);

//
// MessageId: ERROR_HANDLE_EOF
//
// MessageText:
//
//  Reached the end of the file.
//
  ERROR_HANDLE_EOF = DWORD(38);

//
// MessageId: ERROR_HANDLE_DISK_FULL
//
// MessageText:
//
//  The disk is full.
//
  ERROR_HANDLE_DISK_FULL = DWORD(39);

//
// MessageId: ERROR_NOT_SUPPORTED
//
// MessageText:
//
//  The request is not supported.
//
  ERROR_NOT_SUPPORTED = DWORD(50);

//
// MessageId: ERROR_REM_NOT_LIST
//
// MessageText:
//
//  Windows cannot find the network path. Verify that the network path is correct and the destination computer is not busy or turned off. If Windows still cannot find the network path, contact your network administrator.
//
  ERROR_REM_NOT_LIST = DWORD(51);

//
// MessageId: ERROR_DUP_NAME
//
// MessageText:
//
//  You were not connected because a duplicate name exists on the network. Go to System in Control Panel to change the computer name and try again.
//
  ERROR_DUP_NAME = DWORD(52);

//
// MessageId: ERROR_BAD_NETPATH
//
// MessageText:
//
//  The network path was not found.
//
  ERROR_BAD_NETPATH = DWORD(53);

//
// MessageId: ERROR_NETWORK_BUSY
//
// MessageText:
//
//  The network is busy.
//
  ERROR_NETWORK_BUSY = DWORD(54);

//
// MessageId: ERROR_DEV_NOT_EXIST
//
// MessageText:
//
//  The specified network resource or device is no longer available.
//
  ERROR_DEV_NOT_EXIST = DWORD(55);  // dderror

//
// MessageId: ERROR_TOO_MANY_CMDS
//
// MessageText:
//
//  The network BIOS command limit has been reached.
//
  ERROR_TOO_MANY_CMDS = DWORD(56);

//
// MessageId: ERROR_ADAP_HDW_ERR
//
// MessageText:
//
//  A network adapter hardware error occurred.
//
  ERROR_ADAP_HDW_ERR = DWORD(57);

//
// MessageId: ERROR_BAD_NET_RESP
//
// MessageText:
//
//  The specified server cannot perform the requested operation.
//
  ERROR_BAD_NET_RESP = DWORD(58);

//
// MessageId: ERROR_UNEXP_NET_ERR
//
// MessageText:
//
//  An unexpected network error occurred.
//
  ERROR_UNEXP_NET_ERR = DWORD(59);

//
// MessageId: ERROR_BAD_REM_ADAP
//
// MessageText:
//
//  The remote adapter is not compatible.
//
  ERROR_BAD_REM_ADAP = DWORD(60);

//
// MessageId: ERROR_PRINTQ_FULL
//
// MessageText:
//
//  The printer queue is full.
//
  ERROR_PRINTQ_FULL = DWORD(61);

//
// MessageId: ERROR_NO_SPOOL_SPACE
//
// MessageText:
//
//  Space to store the file waiting to be printed is not available on the server.
//
  ERROR_NO_SPOOL_SPACE = DWORD(62);

//
// MessageId: ERROR_PRINT_CANCELLED
//
// MessageText:
//
//  Your file waiting to be printed was deleted.
//
  ERROR_PRINT_CANCELLED = DWORD(63);

//
// MessageId: ERROR_NETNAME_DELETED
//
// MessageText:
//
//  The specified network name is no longer available.
//
  ERROR_NETNAME_DELETED = DWORD(64);

//
// MessageId: ERROR_NETWORK_ACCESS_DENIED
//
// MessageText:
//
//  Network access is denied.
//
  ERROR_NETWORK_ACCESS_DENIED = DWORD(65);

//
// MessageId: ERROR_BAD_DEV_TYPE
//
// MessageText:
//
//  The network resource type is not correct.
//
  ERROR_BAD_DEV_TYPE = DWORD(66);

//
// MessageId: ERROR_BAD_NET_NAME
//
// MessageText:
//
//  The network name cannot be found.
//
  ERROR_BAD_NET_NAME = DWORD(67);

//
// MessageId: ERROR_TOO_MANY_NAMES
//
// MessageText:
//
//  The name limit for the local computer network adapter card was exceeded.
//
  ERROR_TOO_MANY_NAMES = DWORD(68);

//
// MessageId: ERROR_TOO_MANY_SESS
//
// MessageText:
//
//  The network BIOS session limit was exceeded.
//
  ERROR_TOO_MANY_SESS = DWORD(69);

//
// MessageId: ERROR_SHARING_PAUSED
//
// MessageText:
//
//  The remote server has been paused or is in the process of being started.
//
  ERROR_SHARING_PAUSED = DWORD(70);

//
// MessageId: ERROR_REQ_NOT_ACCEP
//
// MessageText:
//
//  No more connections can be made to this remote computer at this time because there are already as many connections as the computer can accept.
//
  ERROR_REQ_NOT_ACCEP = DWORD(71);

//
// MessageId: ERROR_REDIR_PAUSED
//
// MessageText:
//
//  The specified printer or disk device has been paused.
//
  ERROR_REDIR_PAUSED = DWORD(72);

//
// MessageId: ERROR_FILE_EXISTS
//
// MessageText:
//
//  The file exists.
//
  ERROR_FILE_EXISTS = DWORD(80);

//
// MessageId: ERROR_CANNOT_MAKE
//
// MessageText:
//
//  The directory or file cannot be created.
//
  ERROR_CANNOT_MAKE = DWORD(82);

//
// MessageId: ERROR_FAIL_I24
//
// MessageText:
//
//  Fail on INT 24.
//
  ERROR_FAIL_I24 = DWORD(83);

//
// MessageId: ERROR_OUT_OF_STRUCTURES
//
// MessageText:
//
//  Storage to process this request is not available.
//
  ERROR_OUT_OF_STRUCTURES = DWORD(84);

//
// MessageId: ERROR_ALREADY_ASSIGNED
//
// MessageText:
//
//  The local device name is already in use.
//
  ERROR_ALREADY_ASSIGNED = DWORD(85);

//
// MessageId: ERROR_INVALID_PASSWORD
//
// MessageText:
//
//  The specified network password is not correct.
//
  ERROR_INVALID_PASSWORD = DWORD(86);

//
// MessageId: ERROR_INVALID_PARAMETER
//
// MessageText:
//
//  The parameter is incorrect.
//
  ERROR_INVALID_PARAMETER = DWORD(87);  // dderror

//
// MessageId: ERROR_NET_WRITE_FAULT
//
// MessageText:
//
//  A write fault occurred on the network.
//
  ERROR_NET_WRITE_FAULT = DWORD(88);

//
// MessageId: ERROR_NO_PROC_SLOTS
//
// MessageText:
//
//  The system cannot start another process at this time.
//
  ERROR_NO_PROC_SLOTS = DWORD(89);

//
// MessageId: ERROR_TOO_MANY_SEMAPHORES
//
// MessageText:
//
//  Cannot create another system semaphore.
//
  ERROR_TOO_MANY_SEMAPHORES = DWORD(100);

//
// MessageId: ERROR_EXCL_SEM_ALREADY_OWNED
//
// MessageText:
//
//  The exclusive semaphore is owned by another process.
//
  ERROR_EXCL_SEM_ALREADY_OWNED = DWORD(101);

//
// MessageId: ERROR_SEM_IS_SET
//
// MessageText:
//
//  The semaphore is set and cannot be closed.
//
  ERROR_SEM_IS_SET = DWORD(102);

//
// MessageId: ERROR_TOO_MANY_SEM_REQUESTS
//
// MessageText:
//
//  The semaphore cannot be set again.
//
  ERROR_TOO_MANY_SEM_REQUESTS = DWORD(103);

//
// MessageId: ERROR_INVALID_AT_INTERRUPT_TIME
//
// MessageText:
//
//  Cannot request exclusive semaphores at interrupt time.
//
  ERROR_INVALID_AT_INTERRUPT_TIME = DWORD(104);

//
// MessageId: ERROR_SEM_OWNER_DIED
//
// MessageText:
//
//  The previous ownership of this semaphore has ended.
//
  ERROR_SEM_OWNER_DIED = DWORD(105);

//
// MessageId: ERROR_SEM_USER_LIMIT
//
// MessageText:
//
//  Insert the diskette for drive %1.
//
  ERROR_SEM_USER_LIMIT = DWORD(106);

//
// MessageId: ERROR_DISK_CHANGE
//
// MessageText:
//
//  The program stopped because an alternate diskette was not inserted.
//
  ERROR_DISK_CHANGE = DWORD(107);

//
// MessageId: ERROR_DRIVE_LOCKED
//
// MessageText:
//
//  The disk is in use or locked by another process.
//
  ERROR_DRIVE_LOCKED = DWORD(108);

//
// MessageId: ERROR_BROKEN_PIPE
//
// MessageText:
//
//  The pipe has been ended.
//
  ERROR_BROKEN_PIPE = DWORD(109);

//
// MessageId: ERROR_OPEN_FAILED
//
// MessageText:
//
//  The system cannot open the device or file specified.
//
  ERROR_OPEN_FAILED = DWORD(110);

//
// MessageId: ERROR_BUFFER_OVERFLOW
//
// MessageText:
//
//  The file name is too long.
//
  ERROR_BUFFER_OVERFLOW = DWORD(111);

//
// MessageId: ERROR_DISK_FULL
//
// MessageText:
//
//  There is not enough space on the disk.
//
  ERROR_DISK_FULL = DWORD(112);

//
// MessageId: ERROR_NO_MORE_SEARCH_HANDLES
//
// MessageText:
//
//  No more internal file identifiers available.
//
  ERROR_NO_MORE_SEARCH_HANDLES = DWORD(113);

//
// MessageId: ERROR_INVALID_TARGET_HANDLE
//
// MessageText:
//
//  The target internal file identifier is incorrect.
//
  ERROR_INVALID_TARGET_HANDLE = DWORD(114);

//
// MessageId: ERROR_INVALID_CATEGORY
//
// MessageText:
//
//  The IOCTL call made by the application program is not correct.
//
  ERROR_INVALID_CATEGORY = DWORD(117);

//
// MessageId: ERROR_INVALID_VERIFY_SWITCH
//
// MessageText:
//
//  The verify-on-write switch parameter value is not correct.
//
  ERROR_INVALID_VERIFY_SWITCH = DWORD(118);

//
// MessageId: ERROR_BAD_DRIVER_LEVEL
//
// MessageText:
//
//  The system does not support the command requested.
//
  ERROR_BAD_DRIVER_LEVEL = DWORD(119);

//
// MessageId: ERROR_CALL_NOT_IMPLEMENTED
//
// MessageText:
//
//  This function is not supported on this system.
//
  ERROR_CALL_NOT_IMPLEMENTED = DWORD(120);

//
// MessageId: ERROR_SEM_TIMEOUT
//
// MessageText:
//
//  The semaphore timeout period has expired.
//
  ERROR_SEM_TIMEOUT = DWORD(121);

//
// MessageId: ERROR_INSUFFICIENT_BUFFER
//
// MessageText:
//
//  The data area passed to a system call is too small.
//
  ERROR_INSUFFICIENT_BUFFER = DWORD(122);  // dderror

//
// MessageId: ERROR_INVALID_NAME
//
// MessageText:
//
//  The filename, directory name, or volume label syntax is incorrect.
//
  ERROR_INVALID_NAME = DWORD(123);  // dderror

//
// MessageId: ERROR_INVALID_LEVEL
//
// MessageText:
//
//  The system call level is not correct.
//
  ERROR_INVALID_LEVEL = DWORD(124);

//
// MessageId: ERROR_NO_VOLUME_LABEL
//
// MessageText:
//
//  The disk has no volume label.
//
  ERROR_NO_VOLUME_LABEL = DWORD(125);

//
// MessageId: ERROR_MOD_NOT_FOUND
//
// MessageText:
//
//  The specified module could not be found.
//
  ERROR_MOD_NOT_FOUND = DWORD(126);

//
// MessageId: ERROR_PROC_NOT_FOUND
//
// MessageText:
//
//  The specified procedure could not be found.
//
  ERROR_PROC_NOT_FOUND = DWORD(127);

//
// MessageId: ERROR_WAIT_NO_CHILDREN
//
// MessageText:
//
//  There are no child processes to wait for.
//
  ERROR_WAIT_NO_CHILDREN = DWORD(128);

//
// MessageId: ERROR_CHILD_NOT_COMPLETE
//
// MessageText:
//
//  The %1 application cannot be run in Win32 mode.
//
  ERROR_CHILD_NOT_COMPLETE = DWORD(129);

//
// MessageId: ERROR_DIRECT_ACCESS_HANDLE
//
// MessageText:
//
//  Attempt to use a file handle to an open disk partition for an operation other than raw disk I/O.
//
  ERROR_DIRECT_ACCESS_HANDLE = DWORD(130);

//
// MessageId: ERROR_NEGATIVE_SEEK
//
// MessageText:
//
//  An attempt was made to move the file pointer before the beginning of the file.
//
  ERROR_NEGATIVE_SEEK = DWORD(131);

//
// MessageId: ERROR_SEEK_ON_DEVICE
//
// MessageText:
//
//  The file pointer cannot be set on the specified device or file.
//
  ERROR_SEEK_ON_DEVICE = DWORD(132);

//
// MessageId: ERROR_IS_JOIN_TARGET
//
// MessageText:
//
//  A JOIN or SUBST command cannot be used for a drive that contains previously joined drives.
//
  ERROR_IS_JOIN_TARGET = DWORD(133);

//
// MessageId: ERROR_IS_JOINED
//
// MessageText:
//
//  An attempt was made to use a JOIN or SUBST command on a drive that has already been joined.
//
  ERROR_IS_JOINED = DWORD(134);

//
// MessageId: ERROR_IS_SUBSTED
//
// MessageText:
//
//  An attempt was made to use a JOIN or SUBST command on a drive that has already been substituted.
//
  ERROR_IS_SUBSTED = DWORD(135);

//
// MessageId: ERROR_NOT_JOINED
//
// MessageText:
//
//  The system tried to delete the JOIN of a drive that is not joined.
//
  ERROR_NOT_JOINED = DWORD(136);

//
// MessageId: ERROR_NOT_SUBSTED
//
// MessageText:
//
//  The system tried to delete the substitution of a drive that is not substituted.
//
  ERROR_NOT_SUBSTED = DWORD(137);

//
// MessageId: ERROR_JOIN_TO_JOIN
//
// MessageText:
//
//  The system tried to join a drive to a directory on a joined drive.
//
  ERROR_JOIN_TO_JOIN = DWORD(138);

//
// MessageId: ERROR_SUBST_TO_SUBST
//
// MessageText:
//
//  The system tried to substitute a drive to a directory on a substituted drive.
//
  ERROR_SUBST_TO_SUBST = DWORD(139);

//
// MessageId: ERROR_JOIN_TO_SUBST
//
// MessageText:
//
//  The system tried to join a drive to a directory on a substituted drive.
//
  ERROR_JOIN_TO_SUBST = DWORD(140);

//
// MessageId: ERROR_SUBST_TO_JOIN
//
// MessageText:
//
//  The system tried to SUBST a drive to a directory on a joined drive.
//
  ERROR_SUBST_TO_JOIN = DWORD(141);

//
// MessageId: ERROR_BUSY_DRIVE
//
// MessageText:
//
//  The system cannot perform a JOIN or SUBST at this time.
//
  ERROR_BUSY_DRIVE = DWORD(142);

//
// MessageId: ERROR_SAME_DRIVE
//
// MessageText:
//
//  The system cannot join or substitute a drive to or for a directory on the same drive.
//
  ERROR_SAME_DRIVE = DWORD(143);

//
// MessageId: ERROR_DIR_NOT_ROOT
//
// MessageText:
//
//  The directory is not a subdirectory of the root directory.
//
  ERROR_DIR_NOT_ROOT = DWORD(144);

//
// MessageId: ERROR_DIR_NOT_EMPTY
//
// MessageText:
//
//  The directory is not empty.
//
  ERROR_DIR_NOT_EMPTY = DWORD(145);

//
// MessageId: ERROR_IS_SUBST_PATH
//
// MessageText:
//
//  The path specified is being used in a substitute.
//
  ERROR_IS_SUBST_PATH = DWORD(146);

//
// MessageId: ERROR_IS_JOIN_PATH
//
// MessageText:
//
//  Not enough resources are available to process this command.
//
  ERROR_IS_JOIN_PATH = DWORD(147);

//
// MessageId: ERROR_PATH_BUSY
//
// MessageText:
//
//  The path specified cannot be used at this time.
//
  ERROR_PATH_BUSY = DWORD(148);

//
// MessageId: ERROR_IS_SUBST_TARGET
//
// MessageText:
//
//  An attempt was made to join or substitute a drive for which a directory on the drive is the target of a previous substitute.
//
  ERROR_IS_SUBST_TARGET = DWORD(149);

//
// MessageId: ERROR_SYSTEM_TRACE
//
// MessageText:
//
//  System trace information was not specified in your CONFIG.SYS file, or tracing is disallowed.
//
  ERROR_SYSTEM_TRACE = DWORD(150);

//
// MessageId: ERROR_INVALID_EVENT_COUNT
//
// MessageText:
//
//  The number of specified semaphore events for DosMuxSemWait is not correct.
//
  ERROR_INVALID_EVENT_COUNT = DWORD(151);

//
// MessageId: ERROR_TOO_MANY_MUXWAITERS
//
// MessageText:
//
//  DosMuxSemWait did not execute; too many semaphores are already set.
//
  ERROR_TOO_MANY_MUXWAITERS = DWORD(152);

//
// MessageId: ERROR_INVALID_LIST_FORMAT
//
// MessageText:
//
//  The DosMuxSemWait list is not correct.
//
  ERROR_INVALID_LIST_FORMAT = DWORD(153);

//
// MessageId: ERROR_LABEL_TOO_LONG
//
// MessageText:
//
//  The volume label you entered exceeds the label character limit of the target file system.
//
  ERROR_LABEL_TOO_LONG = DWORD(154);

//
// MessageId: ERROR_TOO_MANY_TCBS
//
// MessageText:
//
//  Cannot create another thread.
//
  ERROR_TOO_MANY_TCBS = DWORD(155);

//
// MessageId: ERROR_SIGNAL_REFUSED
//
// MessageText:
//
//  The recipient process has refused the signal.
//
  ERROR_SIGNAL_REFUSED = DWORD(156);

//
// MessageId: ERROR_DISCARDED
//
// MessageText:
//
//  The segment is already discarded and cannot be locked.
//
  ERROR_DISCARDED = DWORD(157);

//
// MessageId: ERROR_NOT_LOCKED
//
// MessageText:
//
//  The segment is already unlocked.
//
  ERROR_NOT_LOCKED = DWORD(158);

//
// MessageId: ERROR_BAD_THREADID_ADDR
//
// MessageText:
//
//  The address for the thread ID is not correct.
//
  ERROR_BAD_THREADID_ADDR = DWORD(159);

//
// MessageId: ERROR_BAD_ARGUMENTS
//
// MessageText:
//
//  One or more arguments are not correct.
//
  ERROR_BAD_ARGUMENTS = DWORD(160);

//
// MessageId: ERROR_BAD_PATHNAME
//
// MessageText:
//
//  The specified path is invalid.
//
  ERROR_BAD_PATHNAME = DWORD(161);

//
// MessageId: ERROR_SIGNAL_PENDING
//
// MessageText:
//
//  A signal is already pending.
//
  ERROR_SIGNAL_PENDING = DWORD(162);

//
// MessageId: ERROR_MAX_THRDS_REACHED
//
// MessageText:
//
//  No more threads can be created in the system.
//
  ERROR_MAX_THRDS_REACHED = DWORD(164);

//
// MessageId: ERROR_LOCK_FAILED
//
// MessageText:
//
//  Unable to lock a region of a file.
//
  ERROR_LOCK_FAILED = DWORD(167);

//
// MessageId: ERROR_BUSY
//
// MessageText:
//
//  The requested resource is in use.
//
  ERROR_BUSY = DWORD(170);  // dderror

//
// MessageId: ERROR_CANCEL_VIOLATION
//
// MessageText:
//
//  A lock request was not outstanding for the supplied cancel region.
//
  ERROR_CANCEL_VIOLATION = DWORD(173);

//
// MessageId: ERROR_ATOMIC_LOCKS_NOT_SUPPORTED
//
// MessageText:
//
//  The file system does not support atomic changes to the lock type.
//
  ERROR_ATOMIC_LOCKS_NOT_SUPPORTED = DWORD(174);

//
// MessageId: ERROR_INVALID_SEGMENT_NUMBER
//
// MessageText:
//
//  The system detected a segment number that was not correct.
//
  ERROR_INVALID_SEGMENT_NUMBER = DWORD(180);

//
// MessageId: ERROR_INVALID_ORDINAL
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INVALID_ORDINAL = DWORD(182);

//
// MessageId: ERROR_ALREADY_EXISTS
//
// MessageText:
//
//  Cannot create a file when that file already exists.
//
  ERROR_ALREADY_EXISTS = DWORD(183);

//
// MessageId: ERROR_INVALID_FLAG_NUMBER
//
// MessageText:
//
//  The flag passed is not correct.
//
  ERROR_INVALID_FLAG_NUMBER = DWORD(186);

//
// MessageId: ERROR_SEM_NOT_FOUND
//
// MessageText:
//
//  The specified system semaphore name was not found.
//
  ERROR_SEM_NOT_FOUND = DWORD(187);

//
// MessageId: ERROR_INVALID_STARTING_CODESEG
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INVALID_STARTING_CODESEG = DWORD(188);

//
// MessageId: ERROR_INVALID_STACKSEG
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INVALID_STACKSEG = DWORD(189);

//
// MessageId: ERROR_INVALID_MODULETYPE
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INVALID_MODULETYPE = DWORD(190);

//
// MessageId: ERROR_INVALID_EXE_SIGNATURE
//
// MessageText:
//
//  Cannot run %1 in Win32 mode.
//
  ERROR_INVALID_EXE_SIGNATURE = DWORD(191);

//
// MessageId: ERROR_EXE_MARKED_INVALID
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_EXE_MARKED_INVALID = DWORD(192);

//
// MessageId: ERROR_BAD_EXE_FORMAT
//
// MessageText:
//
//  %1 is not a valid Win32 application.
//
  ERROR_BAD_EXE_FORMAT = DWORD(193);

//
// MessageId: ERROR_ITERATED_DATA_EXCEEDS_64k
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_ITERATED_DATA_EXCEEDS_64k = DWORD(194);

//
// MessageId: ERROR_INVALID_MINALLOCSIZE
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INVALID_MINALLOCSIZE = DWORD(195);

//
// MessageId: ERROR_DYNLINK_FROM_INVALID_RING
//
// MessageText:
//
//  The operating system cannot run this application program.
//
  ERROR_DYNLINK_FROM_INVALID_RING = DWORD(196);

//
// MessageId: ERROR_IOPL_NOT_ENABLED
//
// MessageText:
//
//  The operating system is not presently configured to run this application.
//
  ERROR_IOPL_NOT_ENABLED = DWORD(197);

//
// MessageId: ERROR_INVALID_SEGDPL
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INVALID_SEGDPL = DWORD(198);

//
// MessageId: ERROR_AUTODATASEG_EXCEEDS_64k
//
// MessageText:
//
//  The operating system cannot run this application program.
//
  ERROR_AUTODATASEG_EXCEEDS_64k = DWORD(199);

//
// MessageId: ERROR_RING2SEG_MUST_BE_MOVABLE
//
// MessageText:
//
//  The code segment cannot be greater than or equal to 64K.
//
  ERROR_RING2SEG_MUST_BE_MOVABLE = DWORD(200);

//
// MessageId: ERROR_RELOC_CHAIN_XEEDS_SEGLIM
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_RELOC_CHAIN_XEEDS_SEGLIM = DWORD(201);

//
// MessageId: ERROR_INFLOOP_IN_RELOC_CHAIN
//
// MessageText:
//
//  The operating system cannot run %1.
//
  ERROR_INFLOOP_IN_RELOC_CHAIN = DWORD(202);

//
// MessageId: ERROR_ENVVAR_NOT_FOUND
//
// MessageText:
//
//  The system could not find the environment option that was entered.
//
  ERROR_ENVVAR_NOT_FOUND = DWORD(203);

//
// MessageId: ERROR_NO_SIGNAL_SENT
//
// MessageText:
//
//  No process in the command subtree has a signal handler.
//
  ERROR_NO_SIGNAL_SENT = DWORD(205);

//
// MessageId: ERROR_FILENAME_EXCED_RANGE
//
// MessageText:
//
//  The filename or extension is too long.
//
  ERROR_FILENAME_EXCED_RANGE = DWORD(206);

//
// MessageId: ERROR_RING2_STACK_IN_USE
//
// MessageText:
//
//  The ring 2 stack is in use.
//
  ERROR_RING2_STACK_IN_USE = DWORD(207);

//
// MessageId: ERROR_META_EXPANSION_TOO_LONG
//
// MessageText:
//
//  The global filename characters, * or ?, are entered incorrectly or too many global filename characters are specified.
//
  ERROR_META_EXPANSION_TOO_LONG = DWORD(208);

//
// MessageId: ERROR_INVALID_SIGNAL_NUMBER
//
// MessageText:
//
//  The signal being posted is not correct.
//
  ERROR_INVALID_SIGNAL_NUMBER = DWORD(209);

//
// MessageId: ERROR_THREAD_1_INACTIVE
//
// MessageText:
//
//  The signal handler cannot be set.
//
  ERROR_THREAD_1_INACTIVE = DWORD(210);

//
// MessageId: ERROR_LOCKED
//
// MessageText:
//
//  The segment is locked and cannot be reallocated.
//
  ERROR_LOCKED = DWORD(212);

//
// MessageId: ERROR_TOO_MANY_MODULES
//
// MessageText:
//
//  Too many dynamic-link modules are attached to this program or dynamic-link module.
//
  ERROR_TOO_MANY_MODULES = DWORD(214);

//
// MessageId: ERROR_NESTING_NOT_ALLOWED
//
// MessageText:
//
//  Cannot nest calls to LoadModule.
//
  ERROR_NESTING_NOT_ALLOWED = DWORD(215);

//
// MessageId: ERROR_EXE_MACHINE_TYPE_MISMATCH
//
// MessageText:
//
//  The image file %1 is valid, but is for a machine type other than the current machine.
//
  ERROR_EXE_MACHINE_TYPE_MISMATCH = DWORD(216);

//
// MessageId: ERROR_EXE_CANNOT_MODIFY_SIGNED_BINARY
//
// MessageText:
//
//  The image file %1 is signed, unable to modify.
//
  ERROR_EXE_CANNOT_MODIFY_SIGNED_BINARY = DWORD(217);

//
// MessageId: ERROR_EXE_CANNOT_MODIFY_STRONG_SIGNED_BINARY
//
// MessageText:
//
//  The image file %1 is strong signed, unable to modify.
//
  ERROR_EXE_CANNOT_MODIFY_STRONG_SIGNED_BINARY = DWORD(218);

//
// MessageId: ERROR_BAD_PIPE
//
// MessageText:
//
//  The pipe state is invalid.
//
  ERROR_BAD_PIPE = DWORD(230);

//
// MessageId: ERROR_PIPE_BUSY
//
// MessageText:
//
//  All pipe instances are busy.
//
  ERROR_PIPE_BUSY = DWORD(231);

//
// MessageId: ERROR_NO_DATA
//
// MessageText:
//
//  The pipe is being closed.
//
  ERROR_NO_DATA = DWORD(232);

//
// MessageId: ERROR_PIPE_NOT_CONNECTED
//
// MessageText:
//
//  No process is on the other end of the pipe.
//
  ERROR_PIPE_NOT_CONNECTED = DWORD(233);

//
// MessageId: ERROR_MORE_DATA
//
// MessageText:
//
//  More data is available.
//
  ERROR_MORE_DATA = DWORD(234);  // dderror

//
// MessageId: ERROR_VC_DISCONNECTED
//
// MessageText:
//
//  The session was canceled.
//
  ERROR_VC_DISCONNECTED = DWORD(240);

//
// MessageId: ERROR_INVALID_EA_NAME
//
// MessageText:
//
//  The specified extended attribute name was invalid.
//
  ERROR_INVALID_EA_NAME = DWORD(254);

//
// MessageId: ERROR_EA_LIST_INCONSISTENT
//
// MessageText:
//
//  The extended attributes are inconsistent.
//
  ERROR_EA_LIST_INCONSISTENT = DWORD(255);

//
// MessageId: WAIT_TIMEOUT
//
// MessageText:
//
//  The wait operation timed out.
//
  WAIT_TIMEOUT = DWORD(258);  // dderror

//
// MessageId: ERROR_NO_MORE_ITEMS
//
// MessageText:
//
//  No more data is available.
//
  ERROR_NO_MORE_ITEMS = DWORD(259);

//
// MessageId: ERROR_CANNOT_COPY
//
// MessageText:
//
//  The copy functions cannot be used.
//
  ERROR_CANNOT_COPY = DWORD(266);

//
// MessageId: ERROR_DIRECTORY
//
// MessageText:
//
//  The directory name is invalid.
//
  ERROR_DIRECTORY = DWORD(267);

//
// MessageId: ERROR_EAS_DIDNT_FIT
//
// MessageText:
//
//  The extended attributes did not fit in the buffer.
//
  ERROR_EAS_DIDNT_FIT = DWORD(275);

//
// MessageId: ERROR_EA_FILE_CORRUPT
//
// MessageText:
//
//  The extended attribute file on the mounted file system is corrupt.
//
  ERROR_EA_FILE_CORRUPT = DWORD(276);

//
// MessageId: ERROR_EA_TABLE_FULL
//
// MessageText:
//
//  The extended attribute table file is full.
//
  ERROR_EA_TABLE_FULL = DWORD(277);

//
// MessageId: ERROR_INVALID_EA_HANDLE
//
// MessageText:
//
//  The specified extended attribute handle is invalid.
//
  ERROR_INVALID_EA_HANDLE = DWORD(278);

//
// MessageId: ERROR_EAS_NOT_SUPPORTED
//
// MessageText:
//
//  The mounted file system does not support extended attributes.
//
  ERROR_EAS_NOT_SUPPORTED = DWORD(282);

//
// MessageId: ERROR_NOT_OWNER
//
// MessageText:
//
//  Attempt to release mutex not owned by caller.
//
  ERROR_NOT_OWNER = DWORD(288);

//
// MessageId: ERROR_TOO_MANY_POSTS
//
// MessageText:
//
//  Too many posts were made to a semaphore.
//
  ERROR_TOO_MANY_POSTS = DWORD(298);

//
// MessageId: ERROR_PARTIAL_COPY
//
// MessageText:
//
//  Only part of a ReadProcessMemory or WriteProcessMemory request was completed.
//
  ERROR_PARTIAL_COPY = DWORD(299);

//
// MessageId: ERROR_OPLOCK_NOT_GRANTED
//
// MessageText:
//
//  The oplock request is denied.
//
  ERROR_OPLOCK_NOT_GRANTED = DWORD(300);

//
// MessageId: ERROR_INVALID_OPLOCK_PROTOCOL
//
// MessageText:
//
//  An invalid oplock acknowledgment was received by the system.
//
  ERROR_INVALID_OPLOCK_PROTOCOL = DWORD(301);

//
// MessageId: ERROR_DISK_TOO_FRAGMENTED
//
// MessageText:
//
//  The volume is too fragmented to complete this operation.
//
  ERROR_DISK_TOO_FRAGMENTED = DWORD(302);

//
// MessageId: ERROR_DELETE_PENDING
//
// MessageText:
//
//  The file cannot be opened because it is in the process of being deleted.
//
  ERROR_DELETE_PENDING = DWORD(303);

//
// MessageId: ERROR_MR_MID_NOT_FOUND
//
// MessageText:
//
//  The system cannot find message text for message number 0x%1 in the message file for %2.
//
  ERROR_MR_MID_NOT_FOUND = DWORD(317);

//
// MessageId: ERROR_SCOPE_NOT_FOUND
//
// MessageText:
//
//  The scope specified was not found.
//
  ERROR_SCOPE_NOT_FOUND = DWORD(318);

//
// MessageId: ERROR_INVALID_ADDRESS
//
// MessageText:
//
//  Attempt to access invalid address.
//
  ERROR_INVALID_ADDRESS = DWORD(487);

//
// MessageId: ERROR_ARITHMETIC_OVERFLOW
//
// MessageText:
//
//  Arithmetic result exceeded 32 bits.
//
  ERROR_ARITHMETIC_OVERFLOW = DWORD(534);

//
// MessageId: ERROR_PIPE_CONNECTED
//
// MessageText:
//
//  There is a process on other end of the pipe.
//
  ERROR_PIPE_CONNECTED = DWORD(535);

//
// MessageId: ERROR_PIPE_LISTENING
//
// MessageText:
//
//  Waiting for a process to open the other end of the pipe.
//
  ERROR_PIPE_LISTENING = DWORD(536);

//
// MessageId: ERROR_EA_ACCESS_DENIED
//
// MessageText:
//
//  Access to the extended attribute was denied.
//
  ERROR_EA_ACCESS_DENIED = DWORD(994);

//
// MessageId: ERROR_OPERATION_ABORTED
//
// MessageText:
//
//  The I/O operation has been aborted because of either a thread exit or an application request.
//
  ERROR_OPERATION_ABORTED = DWORD(995);

//
// MessageId: ERROR_IO_INCOMPLETE
//
// MessageText:
//
//  Overlapped I/O event is not in a signaled state.
//
  ERROR_IO_INCOMPLETE = DWORD(996);

//
// MessageId: ERROR_IO_PENDING
//
// MessageText:
//
//  Overlapped I/O operation is in progress.
//
  ERROR_IO_PENDING = DWORD(997);  // dderror

//
// MessageId: ERROR_NOACCESS
//
// MessageText:
//
//  Invalid access to memory location.
//
  ERROR_NOACCESS = DWORD(998);

//
// MessageId: ERROR_SWAPERROR
//
// MessageText:
//
//  Error performing inpage operation.
//
  ERROR_SWAPERROR = DWORD(999);

//
// MessageId: ERROR_STACK_OVERFLOW
//
// MessageText:
//
//  Recursion too deep; the stack overflowed.
//
  ERROR_STACK_OVERFLOW = DWORD(1001);

//
// MessageId: ERROR_INVALID_MESSAGE
//
// MessageText:
//
//  The window cannot act on the sent message.
//
  ERROR_INVALID_MESSAGE = DWORD(1002);

//
// MessageId: ERROR_CAN_NOT_COMPLETE
//
// MessageText:
//
//  Cannot complete this function.
//
  ERROR_CAN_NOT_COMPLETE = DWORD(1003);

//
// MessageId: ERROR_INVALID_FLAGS
//
// MessageText:
//
//  Invalid flags.
//
  ERROR_INVALID_FLAGS = DWORD(1004);

//
// MessageId: ERROR_UNRECOGNIZED_VOLUME
//
// MessageText:
//
//  The volume does not contain a recognized file system.
//  Please make sure that all required file system drivers are loaded and that the volume is not corrupted.
//
  ERROR_UNRECOGNIZED_VOLUME = DWORD(1005);

//
// MessageId: ERROR_FILE_INVALID
//
// MessageText:
//
//  The volume for a file has been externally altered so that the opened file is no longer valid.
//
  ERROR_FILE_INVALID = DWORD(1006);

//
// MessageId: ERROR_FULLSCREEN_MODE
//
// MessageText:
//
//  The requested operation cannot be performed in full-screen mode.
//
  ERROR_FULLSCREEN_MODE = DWORD(1007);

//
// MessageId: ERROR_NO_TOKEN
//
// MessageText:
//
//  An attempt was made to reference a token that does not exist.
//
  ERROR_NO_TOKEN = DWORD(1008);

//
// MessageId: ERROR_BADDB
//
// MessageText:
//
//  The configuration registry database is corrupt.
//
  ERROR_BADDB = DWORD(1009);

//
// MessageId: ERROR_BADKEY
//
// MessageText:
//
//  The configuration registry key is invalid.
//
  ERROR_BADKEY = DWORD(1010);

//
// MessageId: ERROR_CANTOPEN
//
// MessageText:
//
//  The configuration registry key could not be opened.
//
  ERROR_CANTOPEN = DWORD(1011);

//
// MessageId: ERROR_CANTREAD
//
// MessageText:
//
//  The configuration registry key could not be read.
//
  ERROR_CANTREAD = DWORD(1012);

//
// MessageId: ERROR_CANTWRITE
//
// MessageText:
//
//  The configuration registry key could not be written.
//
  ERROR_CANTWRITE = DWORD(1013);

//
// MessageId: ERROR_REGISTRY_RECOVERED
//
// MessageText:
//
//  One of the files in the registry database had to be recovered by use of a log or alternate copy. The recovery was successful.
//
  ERROR_REGISTRY_RECOVERED = DWORD(1014);

//
// MessageId: ERROR_REGISTRY_CORRUPT
//
// MessageText:
//
//  The registry is corrupted. The structure of one of the files containing registry data is corrupted, or the system's memory image of the file is corrupted, or the file could not be recovered because the alternate copy or log was absent or corrupted.
//
  ERROR_REGISTRY_CORRUPT = DWORD(1015);

//
// MessageId: ERROR_REGISTRY_IO_FAILED
//
// MessageText:
//
//  An I/O operation initiated by the registry failed unrecoverably. The registry could not read in, or write out, or flush, one of the files that contain the system's image of the registry.
//
  ERROR_REGISTRY_IO_FAILED = DWORD(1016);

//
// MessageId: ERROR_NOT_REGISTRY_FILE
//
// MessageText:
//
//  The system has attempted to load or restore a file into the registry, but the specified file is not in a registry file format.
//
  ERROR_NOT_REGISTRY_FILE = DWORD(1017);

//
// MessageId: ERROR_KEY_DELETED
//
// MessageText:
//
//  Illegal operation attempted on a registry key that has been marked for deletion.
//
  ERROR_KEY_DELETED = DWORD(1018);

//
// MessageId: ERROR_NO_LOG_SPACE
//
// MessageText:
//
//  System could not allocate the required space in a registry log.
//
  ERROR_NO_LOG_SPACE = DWORD(1019);

//
// MessageId: ERROR_KEY_HAS_CHILDREN
//
// MessageText:
//
//  Cannot create a symbolic link in a registry key that already has subkeys or values.
//
  ERROR_KEY_HAS_CHILDREN = DWORD(1020);

//
// MessageId: ERROR_CHILD_MUST_BE_VOLATILE
//
// MessageText:
//
//  Cannot create a stable subkey under a volatile parent key.
//
  ERROR_CHILD_MUST_BE_VOLATILE = DWORD(1021);

//
// MessageId: ERROR_NOTIFY_ENUM_DIR
//
// MessageText:
//
//  A notify change request is being completed and the information is not being returned in the caller's buffer. The caller now needs to enumerate the files to find the changes.
//
  ERROR_NOTIFY_ENUM_DIR = DWORD(1022);

//
// MessageId: ERROR_DEPENDENT_SERVICES_RUNNING
//
// MessageText:
//
//  A stop control has been sent to a service that other running services are dependent on.
//
  ERROR_DEPENDENT_SERVICES_RUNNING = DWORD(1051);

//
// MessageId: ERROR_INVALID_SERVICE_CONTROL
//
// MessageText:
//
//  The requested control is not valid for this service.
//
  ERROR_INVALID_SERVICE_CONTROL = DWORD(1052);

//
// MessageId: ERROR_SERVICE_REQUEST_TIMEOUT
//
// MessageText:
//
//  The service did not respond to the start or control request in a timely fashion.
//
  ERROR_SERVICE_REQUEST_TIMEOUT = DWORD(1053);

//
// MessageId: ERROR_SERVICE_NO_THREAD
//
// MessageText:
//
//  A thread could not be created for the service.
//
  ERROR_SERVICE_NO_THREAD = DWORD(1054);

//
// MessageId: ERROR_SERVICE_DATABASE_LOCKED
//
// MessageText:
//
//  The service database is locked.
//
  ERROR_SERVICE_DATABASE_LOCKED = DWORD(1055);

//
// MessageId: ERROR_SERVICE_ALREADY_RUNNING
//
// MessageText:
//
//  An instance of the service is already running.
//
  ERROR_SERVICE_ALREADY_RUNNING = DWORD(1056);

//
// MessageId: ERROR_INVALID_SERVICE_ACCOUNT
//
// MessageText:
//
//  The account name is invalid or does not exist, or the password is invalid for the account name specified.
//
  ERROR_INVALID_SERVICE_ACCOUNT = DWORD(1057);

//
// MessageId: ERROR_SERVICE_DISABLED
//
// MessageText:
//
//  The service cannot be started, either because it is disabled or because it has no enabled devices associated with it.
//
  ERROR_SERVICE_DISABLED = DWORD(1058);

//
// MessageId: ERROR_CIRCULAR_DEPENDENCY
//
// MessageText:
//
//  Circular service dependency was specified.
//
  ERROR_CIRCULAR_DEPENDENCY = DWORD(1059);

//
// MessageId: ERROR_SERVICE_DOES_NOT_EXIST
//
// MessageText:
//
//  The specified service does not exist as an installed service.
//
  ERROR_SERVICE_DOES_NOT_EXIST = DWORD(1060);

//
// MessageId: ERROR_SERVICE_CANNOT_ACCEPT_CTRL
//
// MessageText:
//
//  The service cannot accept control messages at this time.
//
  ERROR_SERVICE_CANNOT_ACCEPT_CTRL = DWORD(1061);

//
// MessageId: ERROR_SERVICE_NOT_ACTIVE
//
// MessageText:
//
//  The service has not been started.
//
  ERROR_SERVICE_NOT_ACTIVE = DWORD(1062);

//
// MessageId: ERROR_FAILED_SERVICE_CONTROLLER_CONNECT
//
// MessageText:
//
//  The service process could not connect to the service controller.
//
  ERROR_FAILED_SERVICE_CONTROLLER_CONNECT = DWORD(1063);

//
// MessageId: ERROR_EXCEPTION_IN_SERVICE
//
// MessageText:
//
//  An exception occurred in the service when handling the control request.
//
  ERROR_EXCEPTION_IN_SERVICE = DWORD(1064);

//
// MessageId: ERROR_DATABASE_DOES_NOT_EXIST
//
// MessageText:
//
//  The database specified does not exist.
//
  ERROR_DATABASE_DOES_NOT_EXIST = DWORD(1065);

//
// MessageId: ERROR_SERVICE_SPECIFIC_ERROR
//
// MessageText:
//
//  The service has returned a service-specific error code.
//
  ERROR_SERVICE_SPECIFIC_ERROR = DWORD(1066);

//
// MessageId: ERROR_PROCESS_ABORTED
//
// MessageText:
//
//  The process terminated unexpectedly.
//
  ERROR_PROCESS_ABORTED = DWORD(1067);

//
// MessageId: ERROR_SERVICE_DEPENDENCY_FAIL
//
// MessageText:
//
//  The dependency service or group failed to start.
//
  ERROR_SERVICE_DEPENDENCY_FAIL = DWORD(1068);

//
// MessageId: ERROR_SERVICE_LOGON_FAILED
//
// MessageText:
//
//  The service did not start due to a logon failure.
//
  ERROR_SERVICE_LOGON_FAILED = DWORD(1069);

//
// MessageId: ERROR_SERVICE_START_HANG
//
// MessageText:
//
//  After starting, the service hung in a start-pending state.
//
  ERROR_SERVICE_START_HANG = DWORD(1070);

//
// MessageId: ERROR_INVALID_SERVICE_LOCK
//
// MessageText:
//
//  The specified service database lock is invalid.
//
  ERROR_INVALID_SERVICE_LOCK = DWORD(1071);

//
// MessageId: ERROR_SERVICE_MARKED_FOR_DELETE
//
// MessageText:
//
//  The specified service has been marked for deletion.
//
  ERROR_SERVICE_MARKED_FOR_DELETE = DWORD(1072);

//
// MessageId: ERROR_SERVICE_EXISTS
//
// MessageText:
//
//  The specified service already exists.
//
  ERROR_SERVICE_EXISTS = DWORD(1073);

//
// MessageId: ERROR_ALREADY_RUNNING_LKG
//
// MessageText:
//
//  The system is currently running with the last-known-good configuration.
//
  ERROR_ALREADY_RUNNING_LKG = DWORD(1074);

//
// MessageId: ERROR_SERVICE_DEPENDENCY_DELETED
//
// MessageText:
//
//  The dependency service does not exist or has been marked for deletion.
//
  ERROR_SERVICE_DEPENDENCY_DELETED = DWORD(1075);

//
// MessageId: ERROR_BOOT_ALREADY_ACCEPTED
//
// MessageText:
//
//  The current boot has already been accepted for use as the last-known-good control set.
//
  ERROR_BOOT_ALREADY_ACCEPTED = DWORD(1076);

//
// MessageId: ERROR_SERVICE_NEVER_STARTED
//
// MessageText:
//
//  No attempts to start the service have been made since the last boot.
//
  ERROR_SERVICE_NEVER_STARTED = DWORD(1077);

//
// MessageId: ERROR_DUPLICATE_SERVICE_NAME
//
// MessageText:
//
//  The name is already in use as either a service name or a service display name.
//
  ERROR_DUPLICATE_SERVICE_NAME = DWORD(1078);

//
// MessageId: ERROR_DIFFERENT_SERVICE_ACCOUNT
//
// MessageText:
//
//  The account specified for this service is different from the account specified for other services running in the same process.
//
  ERROR_DIFFERENT_SERVICE_ACCOUNT = DWORD(1079);

//
// MessageId: ERROR_CANNOT_DETECT_DRIVER_FAILURE
//
// MessageText:
//
//  Failure actions can only be set for Win32 services, not for drivers.
//
  ERROR_CANNOT_DETECT_DRIVER_FAILURE = DWORD(1080);

//
// MessageId: ERROR_CANNOT_DETECT_PROCESS_ABORT
//
// MessageText:
//
//  This service runs in the same process as the service control manager.
//  Therefore, the service control manager cannot take action if this service's process terminates unexpectedly.
//
  ERROR_CANNOT_DETECT_PROCESS_ABORT = DWORD(1081);

//
// MessageId: ERROR_NO_RECOVERY_PROGRAM
//
// MessageText:
//
//  No recovery program has been configured for this service.
//
  ERROR_NO_RECOVERY_PROGRAM = DWORD(1082);

//
// MessageId: ERROR_SERVICE_NOT_IN_EXE
//
// MessageText:
//
//  The executable program that this service is configured to run in does not implement the service.
//
  ERROR_SERVICE_NOT_IN_EXE = DWORD(1083);

//
// MessageId: ERROR_NOT_SAFEBOOT_SERVICE
//
// MessageText:
//
//  This service cannot be started in Safe Mode
//
  ERROR_NOT_SAFEBOOT_SERVICE = DWORD(1084);

//
// MessageId: ERROR_END_OF_MEDIA
//
// MessageText:
//
//  The physical end of the tape has been reached.
//
  ERROR_END_OF_MEDIA = DWORD(1100);

//
// MessageId: ERROR_FILEMARK_DETECTED
//
// MessageText:
//
//  A tape access reached a filemark.
//
  ERROR_FILEMARK_DETECTED = DWORD(1101);

//
// MessageId: ERROR_BEGINNING_OF_MEDIA
//
// MessageText:
//
//  The beginning of the tape or a partition was encountered.
//
  ERROR_BEGINNING_OF_MEDIA = DWORD(1102);

//
// MessageId: ERROR_SETMARK_DETECTED
//
// MessageText:
//
//  A tape access reached the end of a set of files.
//
  ERROR_SETMARK_DETECTED = DWORD(1103);

//
// MessageId: ERROR_NO_DATA_DETECTED
//
// MessageText:
//
//  No more data is on the tape.
//
  ERROR_NO_DATA_DETECTED = DWORD(1104);

//
// MessageId: ERROR_PARTITION_FAILURE
//
// MessageText:
//
//  Tape could not be partitioned.
//
  ERROR_PARTITION_FAILURE = DWORD(1105);

//
// MessageId: ERROR_INVALID_BLOCK_LENGTH
//
// MessageText:
//
//  When accessing a new tape of a multivolume partition, the current block size is incorrect.
//
  ERROR_INVALID_BLOCK_LENGTH = DWORD(1106);

//
// MessageId: ERROR_DEVICE_NOT_PARTITIONED
//
// MessageText:
//
//  Tape partition information could not be found when loading a tape.
//
  ERROR_DEVICE_NOT_PARTITIONED = DWORD(1107);

//
// MessageId: ERROR_UNABLE_TO_LOCK_MEDIA
//
// MessageText:
//
//  Unable to lock the media eject mechanism.
//
  ERROR_UNABLE_TO_LOCK_MEDIA = DWORD(1108);

//
// MessageId: ERROR_UNABLE_TO_UNLOAD_MEDIA
//
// MessageText:
//
//  Unable to unload the media.
//
  ERROR_UNABLE_TO_UNLOAD_MEDIA = DWORD(1109);

//
// MessageId: ERROR_MEDIA_CHANGED
//
// MessageText:
//
//  The media in the drive may have changed.
//
  ERROR_MEDIA_CHANGED = DWORD(1110);

//
// MessageId: ERROR_BUS_RESET
//
// MessageText:
//
//  The I/O bus was reset.
//
  ERROR_BUS_RESET = DWORD(1111);

//
// MessageId: ERROR_NO_MEDIA_IN_DRIVE
//
// MessageText:
//
//  No media in drive.
//
  ERROR_NO_MEDIA_IN_DRIVE = DWORD(1112);

//
// MessageId: ERROR_NO_UNICODE_TRANSLATION
//
// MessageText:
//
//  No mapping for the Unicode character exists in the target multi-byte code page.
//
  ERROR_NO_UNICODE_TRANSLATION = DWORD(1113);

//
// MessageId: ERROR_DLL_INIT_FAILED
//
// MessageText:
//
//  A dynamic link library (DLL) initialization routine failed.
//
  ERROR_DLL_INIT_FAILED = DWORD(1114);

//
// MessageId: ERROR_SHUTDOWN_IN_PROGRESS
//
// MessageText:
//
//  A system shutdown is in progress.
//
  ERROR_SHUTDOWN_IN_PROGRESS = DWORD(1115);

//
// MessageId: ERROR_NO_SHUTDOWN_IN_PROGRESS
//
// MessageText:
//
//  Unable to abort the system shutdown because no shutdown was in progress.
//
  ERROR_NO_SHUTDOWN_IN_PROGRESS = DWORD(1116);

//
// MessageId: ERROR_IO_DEVICE
//
// MessageText:
//
//  The request could not be performed because of an I/O device error.
//
  ERROR_IO_DEVICE = DWORD(1117);

//
// MessageId: ERROR_SERIAL_NO_DEVICE
//
// MessageText:
//
//  No serial device was successfully initialized. The serial driver will unload.
//
  ERROR_SERIAL_NO_DEVICE = DWORD(1118);

//
// MessageId: ERROR_IRQ_BUSY
//
// MessageText:
//
//  Unable to open a device that was sharing an interrupt request (IRQ) with other devices. At least one other device that uses that IRQ was already opened.
//
  ERROR_IRQ_BUSY = DWORD(1119);

//
// MessageId: ERROR_MORE_WRITES
//
// MessageText:
//
//  A serial I/O operation was completed by another write to the serial port.
//  (The IOCTL_SERIAL_XOFF_COUNTER reached zero.)
//
  ERROR_MORE_WRITES = DWORD(1120);

//
// MessageId: ERROR_COUNTER_TIMEOUT
//
// MessageText:
//
//  A serial I/O operation completed because the timeout period expired.
//  (The IOCTL_SERIAL_XOFF_COUNTER did not reach zero.)
//
  ERROR_COUNTER_TIMEOUT = DWORD(1121);

//
// MessageId: ERROR_FLOPPY_ID_MARK_NOT_FOUND
//
// MessageText:
//
//  No ID address mark was found on the floppy disk.
//
  ERROR_FLOPPY_ID_MARK_NOT_FOUND = DWORD(1122);

//
// MessageId: ERROR_FLOPPY_WRONG_CYLINDER
//
// MessageText:
//
//  Mismatch between the floppy disk sector ID field and the floppy disk controller track address.
//
  ERROR_FLOPPY_WRONG_CYLINDER = DWORD(1123);

//
// MessageId: ERROR_FLOPPY_UNKNOWN_ERROR
//
// MessageText:
//
//  The floppy disk controller reported an error that is not recognized by the floppy disk driver.
//
  ERROR_FLOPPY_UNKNOWN_ERROR = DWORD(1124);

//
// MessageId: ERROR_FLOPPY_BAD_REGISTERS
//
// MessageText:
//
//  The floppy disk controller returned inconsistent results in its registers.
//
  ERROR_FLOPPY_BAD_REGISTERS = DWORD(1125);

//
// MessageId: ERROR_DISK_RECALIBRATE_FAILED
//
// MessageText:
//
//  While accessing the hard disk, a recalibrate operation failed, even after retries.
//
  ERROR_DISK_RECALIBRATE_FAILED = DWORD(1126);

//
// MessageId: ERROR_DISK_OPERATION_FAILED
//
// MessageText:
//
//  While accessing the hard disk, a disk operation failed even after retries.
//
  ERROR_DISK_OPERATION_FAILED = DWORD(1127);

//
// MessageId: ERROR_DISK_RESET_FAILED
//
// MessageText:
//
//  While accessing the hard disk, a disk controller reset was needed, but even that failed.
//
  ERROR_DISK_RESET_FAILED = DWORD(1128);

//
// MessageId: ERROR_EOM_OVERFLOW
//
// MessageText:
//
//  Physical end of tape encountered.
//
  ERROR_EOM_OVERFLOW = DWORD(1129);

//
// MessageId: ERROR_NOT_ENOUGH_SERVER_MEMORY
//
// MessageText:
//
//  Not enough server storage is available to process this command.
//
  ERROR_NOT_ENOUGH_SERVER_MEMORY = DWORD(1130);

//
// MessageId: ERROR_POSSIBLE_DEADLOCK
//
// MessageText:
//
//  A potential deadlock condition has been detected.
//
  ERROR_POSSIBLE_DEADLOCK = DWORD(1131);

//
// MessageId: ERROR_MAPPED_ALIGNMENT
//
// MessageText:
//
//  The base address or the file offset specified does not have the proper alignment.
//
  ERROR_MAPPED_ALIGNMENT = DWORD(1132);

//
// MessageId: ERROR_SET_POWER_STATE_VETOED
//
// MessageText:
//
//  An attempt to change the system power state was vetoed by another application or driver.
//
  ERROR_SET_POWER_STATE_VETOED = DWORD(1140);

//
// MessageId: ERROR_SET_POWER_STATE_FAILED
//
// MessageText:
//
//  The system BIOS failed an attempt to change the system power state.
//
  ERROR_SET_POWER_STATE_FAILED = DWORD(1141);

//
// MessageId: ERROR_TOO_MANY_LINKS
//
// MessageText:
//
//  An attempt was made to create more links on a file than the file system supports.
//
  ERROR_TOO_MANY_LINKS = DWORD(1142);

//
// MessageId: ERROR_OLD_WIN_VERSION
//
// MessageText:
//
//  The specified program requires a newer version of Windows.
//
  ERROR_OLD_WIN_VERSION = DWORD(1150);

//
// MessageId: ERROR_APP_WRONG_OS
//
// MessageText:
//
//  The specified program is not a Windows or MS-DOS program.
//
  ERROR_APP_WRONG_OS = DWORD(1151);

//
// MessageId: ERROR_SINGLE_INSTANCE_APP
//
// MessageText:
//
//  Cannot start more than one instance of the specified program.
//
  ERROR_SINGLE_INSTANCE_APP = DWORD(1152);

//
// MessageId: ERROR_RMODE_APP
//
// MessageText:
//
//  The specified program was written for an earlier version of Windows.
//
  ERROR_RMODE_APP = DWORD(1153);

//
// MessageId: ERROR_INVALID_DLL
//
// MessageText:
//
//  One of the library files needed to run this application is damaged.
//
  ERROR_INVALID_DLL = DWORD(1154);

//
// MessageId: ERROR_NO_ASSOCIATION
//
// MessageText:
//
//  No application is associated with the specified file for this operation.
//
  ERROR_NO_ASSOCIATION = DWORD(1155);

//
// MessageId: ERROR_DDE_FAIL
//
// MessageText:
//
//  An error occurred in sending the command to the application.
//
  ERROR_DDE_FAIL = DWORD(1156);

//
// MessageId: ERROR_DLL_NOT_FOUND
//
// MessageText:
//
//  One of the library files needed to run this application cannot be found.
//
  ERROR_DLL_NOT_FOUND = DWORD(1157);

//
// MessageId: ERROR_NO_MORE_USER_HANDLES
//
// MessageText:
//
//  The current process has used all of its system allowance of handles for Window Manager objects.
//
  ERROR_NO_MORE_USER_HANDLES = DWORD(1158);

//
// MessageId: ERROR_MESSAGE_SYNC_ONLY
//
// MessageText:
//
//  The message can be used only with synchronous operations.
//
  ERROR_MESSAGE_SYNC_ONLY = DWORD(1159);

//
// MessageId: ERROR_SOURCE_ELEMENT_EMPTY
//
// MessageText:
//
//  The indicated source element has no media.
//
  ERROR_SOURCE_ELEMENT_EMPTY = DWORD(1160);

//
// MessageId: ERROR_DESTINATION_ELEMENT_FULL
//
// MessageText:
//
//  The indicated destination element already contains media.
//
  ERROR_DESTINATION_ELEMENT_FULL = DWORD(1161);

//
// MessageId: ERROR_ILLEGAL_ELEMENT_ADDRESS
//
// MessageText:
//
//  The indicated element does not exist.
//
  ERROR_ILLEGAL_ELEMENT_ADDRESS = DWORD(1162);

//
// MessageId: ERROR_MAGAZINE_NOT_PRESENT
//
// MessageText:
//
//  The indicated element is part of a magazine that is not present.
//
  ERROR_MAGAZINE_NOT_PRESENT = DWORD(1163);

//
// MessageId: ERROR_DEVICE_REINITIALIZATION_NEEDED
//
// MessageText:
//
//  The indicated device requires reinitialization due to hardware errors.
//
  ERROR_DEVICE_REINITIALIZATION_NEEDED = DWORD(1164);  // dderror

//
// MessageId: ERROR_DEVICE_REQUIRES_CLEANING
//
// MessageText:
//
//  The device has indicated that cleaning is required before further operations are attempted.
//
  ERROR_DEVICE_REQUIRES_CLEANING = DWORD(1165);

//
// MessageId: ERROR_DEVICE_DOOR_OPEN
//
// MessageText:
//
//  The device has indicated that its door is open.
//
  ERROR_DEVICE_DOOR_OPEN = DWORD(1166);

//
// MessageId: ERROR_DEVICE_NOT_CONNECTED
//
// MessageText:
//
//  The device is not connected.
//
  ERROR_DEVICE_NOT_CONNECTED = DWORD(1167);

//
// MessageId: ERROR_NOT_FOUND
//
// MessageText:
//
//  Element not found.
//
  ERROR_NOT_FOUND = DWORD(1168);

//
// MessageId: ERROR_NO_MATCH
//
// MessageText:
//
//  There was no match for the specified key in the index.
//
  ERROR_NO_MATCH = DWORD(1169);

//
// MessageId: ERROR_SET_NOT_FOUND
//
// MessageText:
//
//  The property set specified does not exist on the object.
//
  ERROR_SET_NOT_FOUND = DWORD(1170);

//
// MessageId: ERROR_POINT_NOT_FOUND
//
// MessageText:
//
//  The point passed to GetMouseMovePoints is not in the buffer.
//
  ERROR_POINT_NOT_FOUND = DWORD(1171);

//
// MessageId: ERROR_NO_TRACKING_SERVICE
//
// MessageText:
//
//  The tracking (workstation) service is not running.
//
  ERROR_NO_TRACKING_SERVICE = DWORD(1172);

//
// MessageId: ERROR_NO_VOLUME_ID
//
// MessageText:
//
//  The Volume ID could not be found.
//
  ERROR_NO_VOLUME_ID = DWORD(1173);

//
// MessageId: ERROR_UNABLE_TO_REMOVE_REPLACED
//
// MessageText:
//
//  Unable to remove the file to be replaced.
//
  ERROR_UNABLE_TO_REMOVE_REPLACED = DWORD(1175);

//
// MessageId: ERROR_UNABLE_TO_MOVE_REPLACEMENT
//
// MessageText:
//
//  Unable to move the replacement file to the file to be replaced. The file to be replaced has retained its original name.
//
  ERROR_UNABLE_TO_MOVE_REPLACEMENT = DWORD(1176);

//
// MessageId: ERROR_UNABLE_TO_MOVE_REPLACEMENT_2
//
// MessageText:
//
//  Unable to move the replacement file to the file to be replaced. The file to be replaced has been renamed using the backup name.
//
  ERROR_UNABLE_TO_MOVE_REPLACEMENT_2 = DWORD(1177);

//
// MessageId: ERROR_JOURNAL_DELETE_IN_PROGRESS
//
// MessageText:
//
//  The volume change journal is being deleted.
//
  ERROR_JOURNAL_DELETE_IN_PROGRESS = DWORD(1178);

//
// MessageId: ERROR_JOURNAL_NOT_ACTIVE
//
// MessageText:
//
//  The volume change journal is not active.
//
  ERROR_JOURNAL_NOT_ACTIVE = DWORD(1179);

//
// MessageId: ERROR_POTENTIAL_FILE_FOUND
//
// MessageText:
//
//  A file was found, but it may not be the correct file.
//
  ERROR_POTENTIAL_FILE_FOUND = DWORD(1180);

//
// MessageId: ERROR_JOURNAL_ENTRY_DELETED
//
// MessageText:
//
//  The journal entry has been deleted from the journal.
//
  ERROR_JOURNAL_ENTRY_DELETED = DWORD(1181);

//
// MessageId: ERROR_BAD_DEVICE
//
// MessageText:
//
//  The specified device name is invalid.
//
  ERROR_BAD_DEVICE = DWORD(1200);

//
// MessageId: ERROR_CONNECTION_UNAVAIL
//
// MessageText:
//
//  The device is not currently connected but it is a remembered connection.
//
  ERROR_CONNECTION_UNAVAIL = DWORD(1201);

//
// MessageId: ERROR_DEVICE_ALREADY_REMEMBERED
//
// MessageText:
//
//  The local device name has a remembered connection to another network resource.
//
  ERROR_DEVICE_ALREADY_REMEMBERED = DWORD(1202);

//
// MessageId: ERROR_NO_NET_OR_BAD_PATH
//
// MessageText:
//
//  No network provider accepted the given network path.
//
  ERROR_NO_NET_OR_BAD_PATH = DWORD(1203);

//
// MessageId: ERROR_BAD_PROVIDER
//
// MessageText:
//
//  The specified network provider name is invalid.
//
  ERROR_BAD_PROVIDER = DWORD(1204);

//
// MessageId: ERROR_CANNOT_OPEN_PROFILE
//
// MessageText:
//
//  Unable to open the network connection profile.
//
  ERROR_CANNOT_OPEN_PROFILE = DWORD(1205);

//
// MessageId: ERROR_BAD_PROFILE
//
// MessageText:
//
//  The network connection profile is corrupted.
//
  ERROR_BAD_PROFILE = DWORD(1206);

//
// MessageId: ERROR_NOT_CONTAINER
//
// MessageText:
//
//  Cannot enumerate a noncontainer.
//
  ERROR_NOT_CONTAINER = DWORD(1207);

//
// MessageId: ERROR_EXTENDED_ERROR
//
// MessageText:
//
//  An extended error has occurred.
//
  ERROR_EXTENDED_ERROR = DWORD(1208);

//
// MessageId: ERROR_INVALID_GROUPNAME
//
// MessageText:
//
//  The format of the specified group name is invalid.
//
  ERROR_INVALID_GROUPNAME = DWORD(1209);

//
// MessageId: ERROR_INVALID_COMPUTERNAME
//
// MessageText:
//
//  The format of the specified computer name is invalid.
//
  ERROR_INVALID_COMPUTERNAME = DWORD(1210);

//
// MessageId: ERROR_INVALID_EVENTNAME
//
// MessageText:
//
//  The format of the specified event name is invalid.
//
  ERROR_INVALID_EVENTNAME = DWORD(1211);

//
// MessageId: ERROR_INVALID_DOMAINNAME
//
// MessageText:
//
//  The format of the specified domain name is invalid.
//
  ERROR_INVALID_DOMAINNAME = DWORD(1212);

//
// MessageId: ERROR_INVALID_SERVICENAME
//
// MessageText:
//
//  The format of the specified service name is invalid.
//
  ERROR_INVALID_SERVICENAME = DWORD(1213);

//
// MessageId: ERROR_INVALID_NETNAME
//
// MessageText:
//
//  The format of the specified network name is invalid.
//
  ERROR_INVALID_NETNAME = DWORD(1214);

//
// MessageId: ERROR_INVALID_SHARENAME
//
// MessageText:
//
//  The format of the specified share name is invalid.
//
  ERROR_INVALID_SHARENAME = DWORD(1215);

//
// MessageId: ERROR_INVALID_PASSWORDNAME
//
// MessageText:
//
//  The format of the specified password is invalid.
//
  ERROR_INVALID_PASSWORDNAME = DWORD(1216);

//
// MessageId: ERROR_INVALID_MESSAGENAME
//
// MessageText:
//
//  The format of the specified message name is invalid.
//
  ERROR_INVALID_MESSAGENAME = DWORD(1217);

//
// MessageId: ERROR_INVALID_MESSAGEDEST
//
// MessageText:
//
//  The format of the specified message destination is invalid.
//
  ERROR_INVALID_MESSAGEDEST = DWORD(1218);

//
// MessageId: ERROR_SESSION_CREDENTIAL_CONFLICT
//
// MessageText:
//
//  Multiple connections to a server or shared resource by the same user, using more than one user name, are not allowed. Disconnect all previous connections to the server or shared resource and try again.
//
  ERROR_SESSION_CREDENTIAL_CONFLICT = DWORD(1219);

//
// MessageId: ERROR_REMOTE_SESSION_LIMIT_EXCEEDED
//
// MessageText:
//
//  An attempt was made to establish a session to a network server, but there are already too many sessions established to that server.
//
  ERROR_REMOTE_SESSION_LIMIT_EXCEEDED = DWORD(1220);

//
// MessageId: ERROR_DUP_DOMAINNAME
//
// MessageText:
//
//  The workgroup or domain name is already in use by another computer on the network.
//
  ERROR_DUP_DOMAINNAME = DWORD(1221);

//
// MessageId: ERROR_NO_NETWORK
//
// MessageText:
//
//  The network is not present or not started.
//
  ERROR_NO_NETWORK = DWORD(1222);

//
// MessageId: ERROR_CANCELLED
//
// MessageText:
//
//  The operation was canceled by the user.
//
  ERROR_CANCELLED = DWORD(1223);

//
// MessageId: ERROR_USER_MAPPED_FILE
//
// MessageText:
//
//  The requested operation cannot be performed on a file with a user-mapped section open.
//
  ERROR_USER_MAPPED_FILE = DWORD(1224);

//
// MessageId: ERROR_CONNECTION_REFUSED
//
// MessageText:
//
//  The remote system refused the network connection.
//
  ERROR_CONNECTION_REFUSED = DWORD(1225);

//
// MessageId: ERROR_GRACEFUL_DISCONNECT
//
// MessageText:
//
//  The network connection was gracefully closed.
//
  ERROR_GRACEFUL_DISCONNECT = DWORD(1226);

//
// MessageId: ERROR_ADDRESS_ALREADY_ASSOCIATED
//
// MessageText:
//
//  The network transport endpoint already has an address associated with it.
//
  ERROR_ADDRESS_ALREADY_ASSOCIATED = DWORD(1227);

//
// MessageId: ERROR_ADDRESS_NOT_ASSOCIATED
//
// MessageText:
//
//  An address has not yet been associated with the network endpoint.
//
  ERROR_ADDRESS_NOT_ASSOCIATED = DWORD(1228);

//
// MessageId: ERROR_CONNECTION_INVALID
//
// MessageText:
//
//  An operation was attempted on a nonexistent network connection.
//
  ERROR_CONNECTION_INVALID = DWORD(1229);

//
// MessageId: ERROR_CONNECTION_ACTIVE
//
// MessageText:
//
//  An invalid operation was attempted on an active network connection.
//
  ERROR_CONNECTION_ACTIVE = DWORD(1230);

//
// MessageId: ERROR_NETWORK_UNREACHABLE
//
// MessageText:
//
//  The network location cannot be reached. For information about network troubleshooting, see Windows Help.
//
  ERROR_NETWORK_UNREACHABLE = DWORD(1231);

//
// MessageId: ERROR_HOST_UNREACHABLE
//
// MessageText:
//
//  The network location cannot be reached. For information about network troubleshooting, see Windows Help.
//
  ERROR_HOST_UNREACHABLE = DWORD(1232);

//
// MessageId: ERROR_PROTOCOL_UNREACHABLE
//
// MessageText:
//
//  The network location cannot be reached. For information about network troubleshooting, see Windows Help.
//
  ERROR_PROTOCOL_UNREACHABLE = DWORD(1233);

//
// MessageId: ERROR_PORT_UNREACHABLE
//
// MessageText:
//
//  No service is operating at the destination network endpoint on the remote system.
//
  ERROR_PORT_UNREACHABLE = DWORD(1234);

//
// MessageId: ERROR_REQUEST_ABORTED
//
// MessageText:
//
//  The request was aborted.
//
  ERROR_REQUEST_ABORTED = DWORD(1235);

//
// MessageId: ERROR_CONNECTION_ABORTED
//
// MessageText:
//
//  The network connection was aborted by the local system.
//
  ERROR_CONNECTION_ABORTED = DWORD(1236);

//
// MessageId: ERROR_RETRY
//
// MessageText:
//
//  The operation could not be completed. A retry should be performed.
//
  ERROR_RETRY = DWORD(1237);

//
// MessageId: ERROR_CONNECTION_COUNT_LIMIT
//
// MessageText:
//
//  A connection to the server could not be made because the limit on the number of concurrent connections for this account has been reached.
//
  ERROR_CONNECTION_COUNT_LIMIT = DWORD(1238);

//
// MessageId: ERROR_LOGIN_TIME_RESTRICTION
//
// MessageText:
//
//  Attempting to log in during an unauthorized time of day for this account.
//
  ERROR_LOGIN_TIME_RESTRICTION = DWORD(1239);

//
// MessageId: ERROR_LOGIN_WKSTA_RESTRICTION
//
// MessageText:
//
//  The account is not authorized to log in from this station.
//
  ERROR_LOGIN_WKSTA_RESTRICTION = DWORD(1240);

//
// MessageId: ERROR_INCORRECT_ADDRESS
//
// MessageText:
//
//  The network address could not be used for the operation requested.
//
  ERROR_INCORRECT_ADDRESS = DWORD(1241);

//
// MessageId: ERROR_ALREADY_REGISTERED
//
// MessageText:
//
//  The service is already registered.
//
  ERROR_ALREADY_REGISTERED = DWORD(1242);

//
// MessageId: ERROR_SERVICE_NOT_FOUND
//
// MessageText:
//
//  The specified service does not exist.
//
  ERROR_SERVICE_NOT_FOUND = DWORD(1243);

//
// MessageId: ERROR_NOT_AUTHENTICATED
//
// MessageText:
//
//  The operation being requested was not performed because the user has not been authenticated.
//
  ERROR_NOT_AUTHENTICATED = DWORD(1244);

//
// MessageId: ERROR_NOT_LOGGED_ON
//
// MessageText:
//
//  The operation being requested was not performed because the user has not logged on to the network.
//  The specified service does not exist.
//
  ERROR_NOT_LOGGED_ON = DWORD(1245);

//
// MessageId: ERROR_CONTINUE
//
// MessageText:
//
//  Continue with work in progress.
//
  ERROR_CONTINUE = DWORD(1246);  // dderror

//
// MessageId: ERROR_ALREADY_INITIALIZED
//
// MessageText:
//
//  An attempt was made to perform an initialization operation when initialization has already been completed.
//
  ERROR_ALREADY_INITIALIZED = DWORD(1247);

//
// MessageId: ERROR_NO_MORE_DEVICES
//
// MessageText:
//
//  No more local devices.
//
  ERROR_NO_MORE_DEVICES = DWORD(1248);  // dderror

//
// MessageId: ERROR_NO_SUCH_SITE
//
// MessageText:
//
//  The specified site does not exist.
//
  ERROR_NO_SUCH_SITE = DWORD(1249);

//
// MessageId: ERROR_DOMAIN_CONTROLLER_EXISTS
//
// MessageText:
//
//  A domain controller with the specified name already exists.
//
  ERROR_DOMAIN_CONTROLLER_EXISTS = DWORD(1250);

//
// MessageId: ERROR_ONLY_IF_CONNECTED
//
// MessageText:
//
//  This operation is supported only when you are connected to the server.
//
  ERROR_ONLY_IF_CONNECTED = DWORD(1251);

//
// MessageId: ERROR_OVERRIDE_NOCHANGES
//
// MessageText:
//
//  The group policy framework should call the extension even if there are no changes.
//
  ERROR_OVERRIDE_NOCHANGES = DWORD(1252);

//
// MessageId: ERROR_BAD_USER_PROFILE
//
// MessageText:
//
//  The specified user does not have a valid profile.
//
  ERROR_BAD_USER_PROFILE = DWORD(1253);

//
// MessageId: ERROR_NOT_SUPPORTED_ON_SBS
//
// MessageText:
//
//  This operation is not supported on a computer running Windows Server 2003 for Small Business Server
//
  ERROR_NOT_SUPPORTED_ON_SBS = DWORD(1254);

//
// MessageId: ERROR_SERVER_SHUTDOWN_IN_PROGRESS
//
// MessageText:
//
//  The server machine is shutting down.
//
  ERROR_SERVER_SHUTDOWN_IN_PROGRESS = DWORD(1255);

//
// MessageId: ERROR_HOST_DOWN
//
// MessageText:
//
//  The remote system is not available. For information about network troubleshooting, see Windows Help.
//
  ERROR_HOST_DOWN = DWORD(1256);

//
// MessageId: ERROR_NON_ACCOUNT_SID
//
// MessageText:
//
//  The security identifier provided is not from an account domain.
//
  ERROR_NON_ACCOUNT_SID = DWORD(1257);

//
// MessageId: ERROR_NON_DOMAIN_SID
//
// MessageText:
//
//  The security identifier provided does not have a domain component.
//
  ERROR_NON_DOMAIN_SID = DWORD(1258);

//
// MessageId: ERROR_APPHELP_BLOCK
//
// MessageText:
//
//  AppHelp dialog canceled thus preventing the application from starting.
//
  ERROR_APPHELP_BLOCK = DWORD(1259);

//
// MessageId: ERROR_ACCESS_DISABLED_BY_POLICY
//
// MessageText:
//
//  Windows cannot open this program because it has been prevented by a software restriction policy. For more information, open Event Viewer or contact your system administrator.
//
  ERROR_ACCESS_DISABLED_BY_POLICY = DWORD(1260);

//
// MessageId: ERROR_REG_NAT_CONSUMPTION
//
// MessageText:
//
//  A program attempt to use an invalid register value.  Normally caused by an uninitialized register. This error is Itanium specific.
//
  ERROR_REG_NAT_CONSUMPTION = DWORD(1261);

//
// MessageId: ERROR_CSCSHARE_OFFLINE
//
// MessageText:
//
//  The share is currently offline or does not exist.
//
  ERROR_CSCSHARE_OFFLINE = DWORD(1262);

//
// MessageId: ERROR_PKINIT_FAILURE
//
// MessageText:
//
//  The kerberos protocol encountered an error while validating the
//  KDC certificate during smartcard logon.  There is more information in the
//  system event log.
//
  ERROR_PKINIT_FAILURE = DWORD(1263);

//
// MessageId: ERROR_SMARTCARD_SUBSYSTEM_FAILURE
//
// MessageText:
//
//  The kerberos protocol encountered an error while attempting to utilize
//  the smartcard subsystem.
//
  ERROR_SMARTCARD_SUBSYSTEM_FAILURE = DWORD(1264);

//
// MessageId: ERROR_DOWNGRADE_DETECTED
//
// MessageText:
//
//  The system detected a possible attempt to compromise security. Please ensure that you can contact the server that authenticated you.
//
  ERROR_DOWNGRADE_DETECTED = DWORD(1265);

//
// Do not use ID's 1266 - 1270 as the symbolicNames have been moved to SEC_E_*
//
//
// MessageId: ERROR_MACHINE_LOCKED
//
// MessageText:
//
//  The machine is locked and can not be shut down without the force option.
//
  ERROR_MACHINE_LOCKED = DWORD(1271);

//
// MessageId: ERROR_CALLBACK_SUPPLIED_INVALID_DATA
//
// MessageText:
//
//  An application-defined callback gave invalid data when called.
//
  ERROR_CALLBACK_SUPPLIED_INVALID_DATA = DWORD(1273);

//
// MessageId: ERROR_SYNC_FOREGROUND_REFRESH_REQUIRED
//
// MessageText:
//
//  The group policy framework should call the extension in the synchronous foreground policy refresh.
//
  ERROR_SYNC_FOREGROUND_REFRESH_REQUIRED = DWORD(1274);

//
// MessageId: ERROR_DRIVER_BLOCKED
//
// MessageText:
//
//  This driver has been blocked from loading
//
  ERROR_DRIVER_BLOCKED = DWORD(1275);

//
// MessageId: ERROR_INVALID_IMPORT_OF_NON_DLL
//
// MessageText:
//
//  A dynamic link library (DLL) referenced a module that was neither a DLL nor the process's executable image.
//
  ERROR_INVALID_IMPORT_OF_NON_DLL = DWORD(1276);

//
// MessageId: ERROR_ACCESS_DISABLED_WEBBLADE
//
// MessageText:
//
//  Windows cannot open this program since it has been disabled.
//
  ERROR_ACCESS_DISABLED_WEBBLADE = DWORD(1277);

//
// MessageId: ERROR_ACCESS_DISABLED_WEBBLADE_TAMPER
//
// MessageText:
//
//  Windows cannot open this program because the license enforcement system has been tampered with or become corrupted.
//
  ERROR_ACCESS_DISABLED_WEBBLADE_TAMPER = DWORD(1278);

//
// MessageId: ERROR_RECOVERY_FAILURE
//
// MessageText:
//
//  A transaction recover failed.
//
  ERROR_RECOVERY_FAILURE = DWORD(1279);

//
// MessageId: ERROR_ALREADY_FIBER
//
// MessageText:
//
//  The current thread has already been converted to a fiber.
//
  ERROR_ALREADY_FIBER = DWORD(1280);

//
// MessageId: ERROR_ALREADY_THREAD
//
// MessageText:
//
//  The current thread has already been converted from a fiber.
//
  ERROR_ALREADY_THREAD = DWORD(1281);

//
// MessageId: ERROR_STACK_BUFFER_OVERRUN
//
// MessageText:
//
//  The system detected an overrun of a stack-based buffer in this application.  This
//  overrun could potentially allow a malicious user to gain control of this application.
//
  ERROR_STACK_BUFFER_OVERRUN = DWORD(1282);

//
// MessageId: ERROR_PARAMETER_QUOTA_EXCEEDED
//
// MessageText:
//
//  Data present in one of the parameters is more than the function can operate on.
//
  ERROR_PARAMETER_QUOTA_EXCEEDED = DWORD(1283);

//
// MessageId: ERROR_DEBUGGER_INACTIVE
//
// MessageText:
//
//  An attempt to do an operation on a debug object failed because the object is in the process of being deleted.
//
  ERROR_DEBUGGER_INACTIVE = DWORD(1284);

//
// MessageId: ERROR_DELAY_LOAD_FAILED
//
// MessageText:
//
//  An attempt to delay-load a .dll or get a function address in a delay-loaded .dll failed.
//
  ERROR_DELAY_LOAD_FAILED = DWORD(1285);

//
// MessageId: ERROR_VDM_DISALLOWED
//
// MessageText:
//
//  %1 is a 16-bit application. You do not have permissions to execute 16-bit applications. Check your permissions with your system administrator.
//
  ERROR_VDM_DISALLOWED = DWORD(1286);

//
// MessageId: ERROR_UNIDENTIFIED_ERROR
//
// MessageText:
//
//  Insufficient information exists to identify the cause of failure.
//
  ERROR_UNIDENTIFIED_ERROR = DWORD(1287);

///////////////////////////
//
// Add new status codes before this point unless there is a component specific section below.
//
///////////////////////////


///////////////////////////
//                       //
// Security Status Codes //
//                       //
///////////////////////////


//
// MessageId: ERROR_NOT_ALL_ASSIGNED
//
// MessageText:
//
//  Not all privileges referenced are assigned to the caller.
//
  ERROR_NOT_ALL_ASSIGNED = DWORD(1300);

//
// MessageId: ERROR_SOME_NOT_MAPPED
//
// MessageText:
//
//  Some mapping between account names and security IDs was not done.
//
  ERROR_SOME_NOT_MAPPED = DWORD(1301);

//
// MessageId: ERROR_NO_QUOTAS_FOR_ACCOUNT
//
// MessageText:
//
//  No system quota limits are specifically set for this account.
//
  ERROR_NO_QUOTAS_FOR_ACCOUNT = DWORD(1302);

//
// MessageId: ERROR_LOCAL_USER_SESSION_KEY
//
// MessageText:
//
//  No encryption key is available. A well-known encryption key was returned.
//
  ERROR_LOCAL_USER_SESSION_KEY = DWORD(1303);

//
// MessageId: ERROR_NULL_LM_PASSWORD
//
// MessageText:
//
//  The password is too complex to be converted to a LAN Manager password. The LAN Manager password returned is a NULL string.
//
  ERROR_NULL_LM_PASSWORD = DWORD(1304);

//
// MessageId: ERROR_UNKNOWN_REVISION
//
// MessageText:
//
//  The revision level is unknown.
//
  ERROR_UNKNOWN_REVISION = DWORD(1305);

//
// MessageId: ERROR_REVISION_MISMATCH
//
// MessageText:
//
//  Indicates two revision levels are incompatible.
//
  ERROR_REVISION_MISMATCH = DWORD(1306);

//
// MessageId: ERROR_INVALID_OWNER
//
// MessageText:
//
//  This security ID may not be assigned as the owner of this object.
//
  ERROR_INVALID_OWNER = DWORD(1307);

//
// MessageId: ERROR_INVALID_PRIMARY_GROUP
//
// MessageText:
//
//  This security ID may not be assigned as the primary group of an object.
//
  ERROR_INVALID_PRIMARY_GROUP = DWORD(1308);

//
// MessageId: ERROR_NO_IMPERSONATION_TOKEN
//
// MessageText:
//
//  An attempt has been made to operate on an impersonation token by a thread that is not currently impersonating a client.
//
  ERROR_NO_IMPERSONATION_TOKEN = DWORD(1309);

//
// MessageId: ERROR_CANT_DISABLE_MANDATORY
//
// MessageText:
//
//  The group may not be disabled.
//
  ERROR_CANT_DISABLE_MANDATORY = DWORD(1310);

//
// MessageId: ERROR_NO_LOGON_SERVERS
//
// MessageText:
//
//  There are currently no logon servers available to service the logon request.
//
  ERROR_NO_LOGON_SERVERS = DWORD(1311);

//
// MessageId: ERROR_NO_SUCH_LOGON_SESSION
//
// MessageText:
//
//  A specified logon session does not exist. It may already have been terminated.
//
  ERROR_NO_SUCH_LOGON_SESSION = DWORD(1312);

//
// MessageId: ERROR_NO_SUCH_PRIVILEGE
//
// MessageText:
//
//  A specified privilege does not exist.
//
  ERROR_NO_SUCH_PRIVILEGE = DWORD(1313);

//
// MessageId: ERROR_PRIVILEGE_NOT_HELD
//
// MessageText:
//
//  A required privilege is not held by the client.
//
  ERROR_PRIVILEGE_NOT_HELD = DWORD(1314);

//
// MessageId: ERROR_INVALID_ACCOUNT_NAME
//
// MessageText:
//
//  The name provided is not a properly formed account name.
//
  ERROR_INVALID_ACCOUNT_NAME = DWORD(1315);

//
// MessageId: ERROR_USER_EXISTS
//
// MessageText:
//
//  The specified user already exists.
//
  ERROR_USER_EXISTS = DWORD(1316);

//
// MessageId: ERROR_NO_SUCH_USER
//
// MessageText:
//
//  The specified user does not exist.
//
  ERROR_NO_SUCH_USER = DWORD(1317);

//
// MessageId: ERROR_GROUP_EXISTS
//
// MessageText:
//
//  The specified group already exists.
//
  ERROR_GROUP_EXISTS = DWORD(1318);

//
// MessageId: ERROR_NO_SUCH_GROUP
//
// MessageText:
//
//  The specified group does not exist.
//
  ERROR_NO_SUCH_GROUP = DWORD(1319);

//
// MessageId: ERROR_MEMBER_IN_GROUP
//
// MessageText:
//
//  Either the specified user account is already a member of the specified group, or the specified group cannot be deleted because it contains a member.
//
  ERROR_MEMBER_IN_GROUP = DWORD(1320);

//
// MessageId: ERROR_MEMBER_NOT_IN_GROUP
//
// MessageText:
//
//  The specified user account is not a member of the specified group account.
//
  ERROR_MEMBER_NOT_IN_GROUP = DWORD(1321);

//
// MessageId: ERROR_LAST_ADMIN
//
// MessageText:
//
//  The last remaining administration account cannot be disabled or deleted.
//
  ERROR_LAST_ADMIN = DWORD(1322);

//
// MessageId: ERROR_WRONG_PASSWORD
//
// MessageText:
//
//  Unable to update the password. The value provided as the current password is incorrect.
//
  ERROR_WRONG_PASSWORD = DWORD(1323);

//
// MessageId: ERROR_ILL_FORMED_PASSWORD
//
// MessageText:
//
//  Unable to update the password. The value provided for the new password contains values that are not allowed in passwords.
//
  ERROR_ILL_FORMED_PASSWORD = DWORD(1324);

//
// MessageId: ERROR_PASSWORD_RESTRICTION
//
// MessageText:
//
//  Unable to update the password. The value provided for the new password does not meet the length, complexity, or history requirement of the domain.
//
  ERROR_PASSWORD_RESTRICTION = DWORD(1325);

//
// MessageId: ERROR_LOGON_FAILURE
//
// MessageText:
//
//  Logon failure: unknown user name or bad password.
//
  ERROR_LOGON_FAILURE = DWORD(1326);

//
// MessageId: ERROR_ACCOUNT_RESTRICTION
//
// MessageText:
//
//  Logon failure: user account restriction.  Possible reasons are blank passwords not allowed, logon hour restrictions, or a policy restriction has been enforced.
//
  ERROR_ACCOUNT_RESTRICTION = DWORD(1327);

//
// MessageId: ERROR_INVALID_LOGON_HOURS
//
// MessageText:
//
//  Logon failure: account logon time restriction violation.
//
  ERROR_INVALID_LOGON_HOURS = DWORD(1328);

//
// MessageId: ERROR_INVALID_WORKSTATION
//
// MessageText:
//
//  Logon failure: user not allowed to log on to this computer.
//
  ERROR_INVALID_WORKSTATION = DWORD(1329);

//
// MessageId: ERROR_PASSWORD_EXPIRED
//
// MessageText:
//
//  Logon failure: the specified account password has expired.
//
  ERROR_PASSWORD_EXPIRED = DWORD(1330);

//
// MessageId: ERROR_ACCOUNT_DISABLED
//
// MessageText:
//
//  Logon failure: account currently disabled.
//
  ERROR_ACCOUNT_DISABLED = DWORD(1331);

//
// MessageId: ERROR_NONE_MAPPED
//
// MessageText:
//
//  No mapping between account names and security IDs was done.
//
  ERROR_NONE_MAPPED = DWORD(1332);

//
// MessageId: ERROR_TOO_MANY_LUIDS_REQUESTED
//
// MessageText:
//
//  Too many local user identifiers (LUIDs) were requested at one time.
//
  ERROR_TOO_MANY_LUIDS_REQUESTED = DWORD(1333);

//
// MessageId: ERROR_LUIDS_EXHAUSTED
//
// MessageText:
//
//  No more local user identifiers (LUIDs) are available.
//
  ERROR_LUIDS_EXHAUSTED = DWORD(1334);

//
// MessageId: ERROR_INVALID_SUB_AUTHORITY
//
// MessageText:
//
//  The subauthority part of a security ID is invalid for this particular use.
//
  ERROR_INVALID_SUB_AUTHORITY = DWORD(1335);

//
// MessageId: ERROR_INVALID_ACL
//
// MessageText:
//
//  The access control list (ACL) structure is invalid.
//
  ERROR_INVALID_ACL = DWORD(1336);

//
// MessageId: ERROR_INVALID_SID
//
// MessageText:
//
//  The security ID structure is invalid.
//
  ERROR_INVALID_SID = DWORD(1337);

//
// MessageId: ERROR_INVALID_SECURITY_DESCR
//
// MessageText:
//
//  The security descriptor structure is invalid.
//
  ERROR_INVALID_SECURITY_DESCR = DWORD(1338);

//
// MessageId: ERROR_BAD_INHERITANCE_ACL
//
// MessageText:
//
//  The inherited access control list (ACL) or access control entry (ACE) could not be built.
//
  ERROR_BAD_INHERITANCE_ACL = DWORD(1340);

//
// MessageId: ERROR_SERVER_DISABLED
//
// MessageText:
//
//  The server is currently disabled.
//
  ERROR_SERVER_DISABLED = DWORD(1341);

//
// MessageId: ERROR_SERVER_NOT_DISABLED
//
// MessageText:
//
//  The server is currently enabled.
//
  ERROR_SERVER_NOT_DISABLED = DWORD(1342);

//
// MessageId: ERROR_INVALID_ID_AUTHORITY
//
// MessageText:
//
//  The value provided was an invalid value for an identifier authority.
//
  ERROR_INVALID_ID_AUTHORITY = DWORD(1343);

//
// MessageId: ERROR_ALLOTTED_SPACE_EXCEEDED
//
// MessageText:
//
//  No more memory is available for security information updates.
//
  ERROR_ALLOTTED_SPACE_EXCEEDED = DWORD(1344);

//
// MessageId: ERROR_INVALID_GROUP_ATTRIBUTES
//
// MessageText:
//
//  The specified attributes are invalid, or incompatible with the attributes for the group as a whole.
//
  ERROR_INVALID_GROUP_ATTRIBUTES = DWORD(1345);

//
// MessageId: ERROR_BAD_IMPERSONATION_LEVEL
//
// MessageText:
//
//  Either a required impersonation level was not provided, or the provided impersonation level is invalid.
//
  ERROR_BAD_IMPERSONATION_LEVEL = DWORD(1346);

//
// MessageId: ERROR_CANT_OPEN_ANONYMOUS
//
// MessageText:
//
//  Cannot open an anonymous level security token.
//
  ERROR_CANT_OPEN_ANONYMOUS = DWORD(1347);

//
// MessageId: ERROR_BAD_VALIDATION_CLASS
//
// MessageText:
//
//  The validation information class requested was invalid.
//
  ERROR_BAD_VALIDATION_CLASS = DWORD(1348);

//
// MessageId: ERROR_BAD_TOKEN_TYPE
//
// MessageText:
//
//  The type of the token is inappropriate for its attempted use.
//
  ERROR_BAD_TOKEN_TYPE = DWORD(1349);

//
// MessageId: ERROR_NO_SECURITY_ON_OBJECT
//
// MessageText:
//
//  Unable to perform a security operation on an object that has no associated security.
//
  ERROR_NO_SECURITY_ON_OBJECT = DWORD(1350);

//
// MessageId: ERROR_CANT_ACCESS_DOMAIN_INFO
//
// MessageText:
//
//  Configuration information could not be read from the domain controller, either because the machine is unavailable, or access has been denied.
//
  ERROR_CANT_ACCESS_DOMAIN_INFO = DWORD(1351);

//
// MessageId: ERROR_INVALID_SERVER_STATE
//
// MessageText:
//
//  The security account manager (SAM) or local security authority (LSA) server was in the wrong state to perform the security operation.
//
  ERROR_INVALID_SERVER_STATE = DWORD(1352);

//
// MessageId: ERROR_INVALID_DOMAIN_STATE
//
// MessageText:
//
//  The domain was in the wrong state to perform the security operation.
//
  ERROR_INVALID_DOMAIN_STATE = DWORD(1353);

//
// MessageId: ERROR_INVALID_DOMAIN_ROLE
//
// MessageText:
//
//  This operation is only allowed for the Primary Domain Controller of the domain.
//
  ERROR_INVALID_DOMAIN_ROLE = DWORD(1354);

//
// MessageId: ERROR_NO_SUCH_DOMAIN
//
// MessageText:
//
//  The specified domain either does not exist or could not be contacted.
//
  ERROR_NO_SUCH_DOMAIN = DWORD(1355);

//
// MessageId: ERROR_DOMAIN_EXISTS
//
// MessageText:
//
//  The specified domain already exists.
//
  ERROR_DOMAIN_EXISTS = DWORD(1356);

//
// MessageId: ERROR_DOMAIN_LIMIT_EXCEEDED
//
// MessageText:
//
//  An attempt was made to exceed the limit on the number of domains per server.
//
  ERROR_DOMAIN_LIMIT_EXCEEDED = DWORD(1357);

//
// MessageId: ERROR_INTERNAL_DB_CORRUPTION
//
// MessageText:
//
//  Unable to complete the requested operation because of either a catastrophic media failure or a data structure corruption on the disk.
//
  ERROR_INTERNAL_DB_CORRUPTION = DWORD(1358);

//
// MessageId: ERROR_INTERNAL_ERROR
//
// MessageText:
//
//  An internal error occurred.
//
  ERROR_INTERNAL_ERROR = DWORD(1359);

//
// MessageId: ERROR_GENERIC_NOT_MAPPED
//
// MessageText:
//
//  Generic access types were contained in an access mask which should already be mapped to nongeneric types.
//
  ERROR_GENERIC_NOT_MAPPED = DWORD(1360);

//
// MessageId: ERROR_BAD_DESCRIPTOR_FORMAT
//
// MessageText:
//
//  A security descriptor is not in the right format (absolute or self-relative).
//
  ERROR_BAD_DESCRIPTOR_FORMAT = DWORD(1361);

//
// MessageId: ERROR_NOT_LOGON_PROCESS
//
// MessageText:
//
//  The requested action is restricted for use by logon processes only. The calling process has not registered as a logon process.
//
  ERROR_NOT_LOGON_PROCESS = DWORD(1362);

//
// MessageId: ERROR_LOGON_SESSION_EXISTS
//
// MessageText:
//
//  Cannot start a new logon session with an ID that is already in use.
//
  ERROR_LOGON_SESSION_EXISTS = DWORD(1363);

//
// MessageId: ERROR_NO_SUCH_PACKAGE
//
// MessageText:
//
//  A specified authentication package is unknown.
//
  ERROR_NO_SUCH_PACKAGE = DWORD(1364);

//
// MessageId: ERROR_BAD_LOGON_SESSION_STATE
//
// MessageText:
//
//  The logon session is not in a state that is consistent with the requested operation.
//
  ERROR_BAD_LOGON_SESSION_STATE = DWORD(1365);

//
// MessageId: ERROR_LOGON_SESSION_COLLISION
//
// MessageText:
//
//  The logon session ID is already in use.
//
  ERROR_LOGON_SESSION_COLLISION = DWORD(1366);

//
// MessageId: ERROR_INVALID_LOGON_TYPE
//
// MessageText:
//
//  A logon request contained an invalid logon type value.
//
  ERROR_INVALID_LOGON_TYPE = DWORD(1367);

//
// MessageId: ERROR_CANNOT_IMPERSONATE
//
// MessageText:
//
//  Unable to impersonate using a named pipe until data has been read from that pipe.
//
  ERROR_CANNOT_IMPERSONATE = DWORD(1368);

//
// MessageId: ERROR_RXACT_INVALID_STATE
//
// MessageText:
//
//  The transaction state of a registry subtree is incompatible with the requested operation.
//
  ERROR_RXACT_INVALID_STATE = DWORD(1369);

//
// MessageId: ERROR_RXACT_COMMIT_FAILURE
//
// MessageText:
//
//  An internal security database corruption has been encountered.
//
  ERROR_RXACT_COMMIT_FAILURE = DWORD(1370);

//
// MessageId: ERROR_SPECIAL_ACCOUNT
//
// MessageText:
//
//  Cannot perform this operation on built-in accounts.
//
  ERROR_SPECIAL_ACCOUNT = DWORD(1371);

//
// MessageId: ERROR_SPECIAL_GROUP
//
// MessageText:
//
//  Cannot perform this operation on this built-in special group.
//
  ERROR_SPECIAL_GROUP = DWORD(1372);

//
// MessageId: ERROR_SPECIAL_USER
//
// MessageText:
//
//  Cannot perform this operation on this built-in special user.
//
  ERROR_SPECIAL_USER = DWORD(1373);

//
// MessageId: ERROR_MEMBERS_PRIMARY_GROUP
//
// MessageText:
//
//  The user cannot be removed from a group because the group is currently the user's primary group.
//
  ERROR_MEMBERS_PRIMARY_GROUP = DWORD(1374);

//
// MessageId: ERROR_TOKEN_ALREADY_IN_USE
//
// MessageText:
//
//  The token is already in use as a primary token.
//
  ERROR_TOKEN_ALREADY_IN_USE = DWORD(1375);

//
// MessageId: ERROR_NO_SUCH_ALIAS
//
// MessageText:
//
//  The specified local group does not exist.
//
  ERROR_NO_SUCH_ALIAS = DWORD(1376);

//
// MessageId: ERROR_MEMBER_NOT_IN_ALIAS
//
// MessageText:
//
//  The specified account name is not a member of the local group.
//
  ERROR_MEMBER_NOT_IN_ALIAS = DWORD(1377);

//
// MessageId: ERROR_MEMBER_IN_ALIAS
//
// MessageText:
//
//  The specified account name is already a member of the local group.
//
  ERROR_MEMBER_IN_ALIAS = DWORD(1378);

//
// MessageId: ERROR_ALIAS_EXISTS
//
// MessageText:
//
//  The specified local group already exists.
//
  ERROR_ALIAS_EXISTS = DWORD(1379);

//
// MessageId: ERROR_LOGON_NOT_GRANTED
//
// MessageText:
//
//  Logon failure: the user has not been granted the requested logon type at this computer.
//
  ERROR_LOGON_NOT_GRANTED = DWORD(1380);

//
// MessageId: ERROR_TOO_MANY_SECRETS
//
// MessageText:
//
//  The maximum number of secrets that may be stored in a single system has been exceeded.
//
  ERROR_TOO_MANY_SECRETS = DWORD(1381);

//
// MessageId: ERROR_SECRET_TOO_LONG
//
// MessageText:
//
//  The length of a secret exceeds the maximum length allowed.
//
  ERROR_SECRET_TOO_LONG = DWORD(1382);

//
// MessageId: ERROR_INTERNAL_DB_ERROR
//
// MessageText:
//
//  The local security authority database contains an internal inconsistency.
//
  ERROR_INTERNAL_DB_ERROR = DWORD(1383);

//
// MessageId: ERROR_TOO_MANY_CONTEXT_IDS
//
// MessageText:
//
//  During a logon attempt, the user's security context accumulated too many security IDs.
//
  ERROR_TOO_MANY_CONTEXT_IDS = DWORD(1384);

//
// MessageId: ERROR_LOGON_TYPE_NOT_GRANTED
//
// MessageText:
//
//  Logon failure: the user has not been granted the requested logon type at this computer.
//
  ERROR_LOGON_TYPE_NOT_GRANTED = DWORD(1385);

//
// MessageId: ERROR_NT_CROSS_ENCRYPTION_REQUIRED
//
// MessageText:
//
//  A cross-encrypted password is necessary to change a user password.
//
  ERROR_NT_CROSS_ENCRYPTION_REQUIRED = DWORD(1386);

//
// MessageId: ERROR_NO_SUCH_MEMBER
//
// MessageText:
//
//  A member could not be added to or removed from the local group because the member does not exist.
//
  ERROR_NO_SUCH_MEMBER = DWORD(1387);

//
// MessageId: ERROR_INVALID_MEMBER
//
// MessageText:
//
//  A new member could not be added to a local group because the member has the wrong account type.
//
  ERROR_INVALID_MEMBER = DWORD(1388);

//
// MessageId: ERROR_TOO_MANY_SIDS
//
// MessageText:
//
//  Too many security IDs have been specified.
//
  ERROR_TOO_MANY_SIDS = DWORD(1389);

//
// MessageId: ERROR_LM_CROSS_ENCRYPTION_REQUIRED
//
// MessageText:
//
//  A cross-encrypted password is necessary to change this user password.
//
  ERROR_LM_CROSS_ENCRYPTION_REQUIRED = DWORD(1390);

//
// MessageId: ERROR_NO_INHERITANCE
//
// MessageText:
//
//  Indicates an ACL contains no inheritable components.
//
  ERROR_NO_INHERITANCE = DWORD(1391);

//
// MessageId: ERROR_FILE_CORRUPT
//
// MessageText:
//
//  The file or directory is corrupted and unreadable.
//
  ERROR_FILE_CORRUPT = DWORD(1392);

//
// MessageId: ERROR_DISK_CORRUPT
//
// MessageText:
//
//  The disk structure is corrupted and unreadable.
//
  ERROR_DISK_CORRUPT = DWORD(1393);

//
// MessageId: ERROR_NO_USER_SESSION_KEY
//
// MessageText:
//
//  There is no user session key for the specified logon session.
//
  ERROR_NO_USER_SESSION_KEY = DWORD(1394);

//
// MessageId: ERROR_LICENSE_QUOTA_EXCEEDED
//
// MessageText:
//
//  The service being accessed is licensed for a particular number of connections.
//  No more connections can be made to the service at this time because there are already as many connections as the service can accept.
//
  ERROR_LICENSE_QUOTA_EXCEEDED = DWORD(1395);

//
// MessageId: ERROR_WRONG_TARGET_NAME
//
// MessageText:
//
//  Logon Failure: The target account name is incorrect.
//
  ERROR_WRONG_TARGET_NAME = DWORD(1396);

//
// MessageId: ERROR_MUTUAL_AUTH_FAILED
//
// MessageText:
//
//  Mutual Authentication failed. The server's password is out of date at the domain controller.
//
  ERROR_MUTUAL_AUTH_FAILED = DWORD(1397);

//
// MessageId: ERROR_TIME_SKEW
//
// MessageText:
//
//  There is a time and/or date difference between the client and server.
//
  ERROR_TIME_SKEW = DWORD(1398);

//
// MessageId: ERROR_CURRENT_DOMAIN_NOT_ALLOWED
//
// MessageText:
//
//  This operation can not be performed on the current domain.
//
  ERROR_CURRENT_DOMAIN_NOT_ALLOWED = DWORD(1399);

// End of security error codes



///////////////////////////
//                       //
// WinUser Error Codes   //
//                       //
///////////////////////////


//
// MessageId: ERROR_INVALID_WINDOW_HANDLE
//
// MessageText:
//
//  Invalid window handle.
//
  ERROR_INVALID_WINDOW_HANDLE = DWORD(1400);

//
// MessageId: ERROR_INVALID_MENU_HANDLE
//
// MessageText:
//
//  Invalid menu handle.
//
  ERROR_INVALID_MENU_HANDLE = DWORD(1401);

//
// MessageId: ERROR_INVALID_CURSOR_HANDLE
//
// MessageText:
//
//  Invalid cursor handle.
//
  ERROR_INVALID_CURSOR_HANDLE = DWORD(1402);

//
// MessageId: ERROR_INVALID_ACCEL_HANDLE
//
// MessageText:
//
//  Invalid accelerator table handle.
//
  ERROR_INVALID_ACCEL_HANDLE = DWORD(1403);

//
// MessageId: ERROR_INVALID_HOOK_HANDLE
//
// MessageText:
//
//  Invalid hook handle.
//
  ERROR_INVALID_HOOK_HANDLE = DWORD(1404);

//
// MessageId: ERROR_INVALID_DWP_HANDLE
//
// MessageText:
//
//  Invalid handle to a multiple-window position structure.
//
  ERROR_INVALID_DWP_HANDLE = DWORD(1405);

//
// MessageId: ERROR_TLW_WITH_WSCHILD
//
// MessageText:
//
//  Cannot create a top-level child window.
//
  ERROR_TLW_WITH_WSCHILD = DWORD(1406);

//
// MessageId: ERROR_CANNOT_FIND_WND_CLASS
//
// MessageText:
//
//  Cannot find window class.
//
  ERROR_CANNOT_FIND_WND_CLASS = DWORD(1407);

//
// MessageId: ERROR_WINDOW_OF_OTHER_THREAD
//
// MessageText:
//
//  Invalid window; it belongs to other thread.
//
  ERROR_WINDOW_OF_OTHER_THREAD = DWORD(1408);

//
// MessageId: ERROR_HOTKEY_ALREADY_REGISTERED
//
// MessageText:
//
//  Hot key is already registered.
//
  ERROR_HOTKEY_ALREADY_REGISTERED = DWORD(1409);

//
// MessageId: ERROR_CLASS_ALREADY_EXISTS
//
// MessageText:
//
//  Class already exists.
//
  ERROR_CLASS_ALREADY_EXISTS = DWORD(1410);

//
// MessageId: ERROR_CLASS_DOES_NOT_EXIST
//
// MessageText:
//
//  Class does not exist.
//
  ERROR_CLASS_DOES_NOT_EXIST = DWORD(1411);

//
// MessageId: ERROR_CLASS_HAS_WINDOWS
//
// MessageText:
//
//  Class still has open windows.
//
  ERROR_CLASS_HAS_WINDOWS = DWORD(1412);

//
// MessageId: ERROR_INVALID_INDEX
//
// MessageText:
//
//  Invalid index.
//
  ERROR_INVALID_INDEX = DWORD(1413);

//
// MessageId: ERROR_INVALID_ICON_HANDLE
//
// MessageText:
//
//  Invalid icon handle.
//
  ERROR_INVALID_ICON_HANDLE = DWORD(1414);

//
// MessageId: ERROR_PRIVATE_DIALOG_INDEX
//
// MessageText:
//
//  Using private DIALOG window words.
//
  ERROR_PRIVATE_DIALOG_INDEX = DWORD(1415);

//
// MessageId: ERROR_LISTBOX_ID_NOT_FOUND
//
// MessageText:
//
//  The list box identifier was not found.
//
  ERROR_LISTBOX_ID_NOT_FOUND = DWORD(1416);

//
// MessageId: ERROR_NO_WILDCARD_CHARACTERS
//
// MessageText:
//
//  No wildcards were found.
//
  ERROR_NO_WILDCARD_CHARACTERS = DWORD(1417);

//
// MessageId: ERROR_CLIPBOARD_NOT_OPEN
//
// MessageText:
//
//  Thread does not have a clipboard open.
//
  ERROR_CLIPBOARD_NOT_OPEN = DWORD(1418);

//
// MessageId: ERROR_HOTKEY_NOT_REGISTERED
//
// MessageText:
//
//  Hot key is not registered.
//
  ERROR_HOTKEY_NOT_REGISTERED = DWORD(1419);

//
// MessageId: ERROR_WINDOW_NOT_DIALOG
//
// MessageText:
//
//  The window is not a valid dialog window.
//
  ERROR_WINDOW_NOT_DIALOG = DWORD(1420);

//
// MessageId: ERROR_CONTROL_ID_NOT_FOUND
//
// MessageText:
//
//  Control ID not found.
//
  ERROR_CONTROL_ID_NOT_FOUND = DWORD(1421);

//
// MessageId: ERROR_INVALID_COMBOBOX_MESSAGE
//
// MessageText:
//
//  Invalid message for a combo box because it does not have an edit control.
//
  ERROR_INVALID_COMBOBOX_MESSAGE = DWORD(1422);

//
// MessageId: ERROR_WINDOW_NOT_COMBOBOX
//
// MessageText:
//
//  The window is not a combo box.
//
  ERROR_WINDOW_NOT_COMBOBOX = DWORD(1423);

//
// MessageId: ERROR_INVALID_EDIT_HEIGHT
//
// MessageText:
//
//  Height must be less than 256.
//
  ERROR_INVALID_EDIT_HEIGHT = DWORD(1424);

//
// MessageId: ERROR_DC_NOT_FOUND
//
// MessageText:
//
//  Invalid device context (DC) handle.
//
  ERROR_DC_NOT_FOUND = DWORD(1425);

//
// MessageId: ERROR_INVALID_HOOK_FILTER
//
// MessageText:
//
//  Invalid hook procedure type.
//
  ERROR_INVALID_HOOK_FILTER = DWORD(1426);

//
// MessageId: ERROR_INVALID_FILTER_PROC
//
// MessageText:
//
//  Invalid hook procedure.
//
  ERROR_INVALID_FILTER_PROC = DWORD(1427);

//
// MessageId: ERROR_HOOK_NEEDS_HMOD
//
// MessageText:
//
//  Cannot set nonlocal hook without a module handle.
//
  ERROR_HOOK_NEEDS_HMOD = DWORD(1428);

//
// MessageId: ERROR_GLOBAL_ONLY_HOOK
//
// MessageText:
//
//  This hook procedure can only be set globally.
//
  ERROR_GLOBAL_ONLY_HOOK = DWORD(1429);

//
// MessageId: ERROR_JOURNAL_HOOK_SET
//
// MessageText:
//
//  The journal hook procedure is already installed.
//
  ERROR_JOURNAL_HOOK_SET = DWORD(1430);

//
// MessageId: ERROR_HOOK_NOT_INSTALLED
//
// MessageText:
//
//  The hook procedure is not installed.
//
  ERROR_HOOK_NOT_INSTALLED = DWORD(1431);

//
// MessageId: ERROR_INVALID_LB_MESSAGE
//
// MessageText:
//
//  Invalid message for single-selection list box.
//
  ERROR_INVALID_LB_MESSAGE = DWORD(1432);

//
// MessageId: ERROR_SETCOUNT_ON_BAD_LB
//
// MessageText:
//
//  LB_SETCOUNT sent to non-lazy list box.
//
  ERROR_SETCOUNT_ON_BAD_LB = DWORD(1433);

//
// MessageId: ERROR_LB_WITHOUT_TABSTOPS
//
// MessageText:
//
//  This list box does not support tab stops.
//
  ERROR_LB_WITHOUT_TABSTOPS = DWORD(1434);

//
// MessageId: ERROR_DESTROY_OBJECT_OF_OTHER_THREAD
//
// MessageText:
//
//  Cannot destroy object created by another thread.
//
  ERROR_DESTROY_OBJECT_OF_OTHER_THREAD = DWORD(1435);

//
// MessageId: ERROR_CHILD_WINDOW_MENU
//
// MessageText:
//
//  Child windows cannot have menus.
//
  ERROR_CHILD_WINDOW_MENU = DWORD(1436);

//
// MessageId: ERROR_NO_SYSTEM_MENU
//
// MessageText:
//
//  The window does not have a system menu.
//
  ERROR_NO_SYSTEM_MENU = DWORD(1437);

//
// MessageId: ERROR_INVALID_MSGBOX_STYLE
//
// MessageText:
//
//  Invalid message box style.
//
  ERROR_INVALID_MSGBOX_STYLE = DWORD(1438);

//
// MessageId: ERROR_INVALID_SPI_VALUE
//
// MessageText:
//
//  Invalid system-wide (SPI_*) parameter.
//
  ERROR_INVALID_SPI_VALUE = DWORD(1439);

//
// MessageId: ERROR_SCREEN_ALREADY_LOCKED
//
// MessageText:
//
//  Screen already locked.
//
  ERROR_SCREEN_ALREADY_LOCKED = DWORD(1440);

//
// MessageId: ERROR_HWNDS_HAVE_DIFF_PARENT
//
// MessageText:
//
//  All handles to windows in a multiple-window position structure must have the same parent.
//
  ERROR_HWNDS_HAVE_DIFF_PARENT = DWORD(1441);

//
// MessageId: ERROR_NOT_CHILD_WINDOW
//
// MessageText:
//
//  The window is not a child window.
//
  ERROR_NOT_CHILD_WINDOW = DWORD(1442);

//
// MessageId: ERROR_INVALID_GW_COMMAND
//
// MessageText:
//
//  Invalid GW_* command.
//
  ERROR_INVALID_GW_COMMAND = DWORD(1443);

//
// MessageId: ERROR_INVALID_THREAD_ID
//
// MessageText:
//
//  Invalid thread identifier.
//
  ERROR_INVALID_THREAD_ID = DWORD(1444);

//
// MessageId: ERROR_NON_MDICHILD_WINDOW
//
// MessageText:
//
//  Cannot process a message from a window that is not a multiple document interface (MDI) window.
//
  ERROR_NON_MDICHILD_WINDOW = DWORD(1445);

//
// MessageId: ERROR_POPUP_ALREADY_ACTIVE
//
// MessageText:
//
//  Popup menu already active.
//
  ERROR_POPUP_ALREADY_ACTIVE = DWORD(1446);

//
// MessageId: ERROR_NO_SCROLLBARS
//
// MessageText:
//
//  The window does not have scroll bars.
//
  ERROR_NO_SCROLLBARS = DWORD(1447);

//
// MessageId: ERROR_INVALID_SCROLLBAR_RANGE
//
// MessageText:
//
//  Scroll bar range cannot be greater than MAXLONG.
//
  ERROR_INVALID_SCROLLBAR_RANGE = DWORD(1448);

//
// MessageId: ERROR_INVALID_SHOWWIN_COMMAND
//
// MessageText:
//
//  Cannot show or remove the window in the way specified.
//
  ERROR_INVALID_SHOWWIN_COMMAND = DWORD(1449);

//
// MessageId: ERROR_NO_SYSTEM_RESOURCES
//
// MessageText:
//
//  Insufficient system resources exist to complete the requested service.
//
  ERROR_NO_SYSTEM_RESOURCES = DWORD(1450);

//
// MessageId: ERROR_NONPAGED_SYSTEM_RESOURCES
//
// MessageText:
//
//  Insufficient system resources exist to complete the requested service.
//
  ERROR_NONPAGED_SYSTEM_RESOURCES = DWORD(1451);

//
// MessageId: ERROR_PAGED_SYSTEM_RESOURCES
//
// MessageText:
//
//  Insufficient system resources exist to complete the requested service.
//
  ERROR_PAGED_SYSTEM_RESOURCES = DWORD(1452);

//
// MessageId: ERROR_WORKING_SET_QUOTA
//
// MessageText:
//
//  Insufficient quota to complete the requested service.
//
  ERROR_WORKING_SET_QUOTA = DWORD(1453);

//
// MessageId: ERROR_PAGEFILE_QUOTA
//
// MessageText:
//
//  Insufficient quota to complete the requested service.
//
  ERROR_PAGEFILE_QUOTA = DWORD(1454);

//
// MessageId: ERROR_COMMITMENT_LIMIT
//
// MessageText:
//
//  The paging file is too small for this operation to complete.
//
  ERROR_COMMITMENT_LIMIT = DWORD(1455);

//
// MessageId: ERROR_MENU_ITEM_NOT_FOUND
//
// MessageText:
//
//  A menu item was not found.
//
  ERROR_MENU_ITEM_NOT_FOUND = DWORD(1456);

//
// MessageId: ERROR_INVALID_KEYBOARD_HANDLE
//
// MessageText:
//
//  Invalid keyboard layout handle.
//
  ERROR_INVALID_KEYBOARD_HANDLE = DWORD(1457);

//
// MessageId: ERROR_HOOK_TYPE_NOT_ALLOWED
//
// MessageText:
//
//  Hook type not allowed.
//
  ERROR_HOOK_TYPE_NOT_ALLOWED = DWORD(1458);

//
// MessageId: ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION
//
// MessageText:
//
//  This operation requires an interactive window station.
//
  ERROR_REQUIRES_INTERACTIVE_WINDOWSTATION = DWORD(1459);

//
// MessageId: ERROR_TIMEOUT
//
// MessageText:
//
//  This operation returned because the timeout period expired.
//
  ERROR_TIMEOUT = DWORD(1460);

//
// MessageId: ERROR_INVALID_MONITOR_HANDLE
//
// MessageText:
//
//  Invalid monitor handle.
//
  ERROR_INVALID_MONITOR_HANDLE = DWORD(1461);

// End of WinUser error codes



///////////////////////////
//                       //
// Eventlog Status Codes //
//                       //
///////////////////////////


//
// MessageId: ERROR_EVENTLOG_FILE_CORRUPT
//
// MessageText:
//
//  The event log file is corrupted.
//
  ERROR_EVENTLOG_FILE_CORRUPT = DWORD(1500);

//
// MessageId: ERROR_EVENTLOG_CANT_START
//
// MessageText:
//
//  No event log file could be opened, so the event logging service did not start.
//
  ERROR_EVENTLOG_CANT_START = DWORD(1501);

//
// MessageId: ERROR_LOG_FILE_FULL
//
// MessageText:
//
//  The event log file is full.
//
  ERROR_LOG_FILE_FULL = DWORD(1502);

//
// MessageId: ERROR_EVENTLOG_FILE_CHANGED
//
// MessageText:
//
//  The event log file has changed between read operations.
//
  ERROR_EVENTLOG_FILE_CHANGED = DWORD(1503);

// End of eventlog error codes



///////////////////////////
//                       //
// MSI Error Codes       //
//                       //
///////////////////////////


//
// MessageId: ERROR_INSTALL_SERVICE_FAILURE
//
// MessageText:
//
//  The Windows Installer Service could not be accessed. This can occur if you are running Windows in safe mode, or if the Windows Installer is not correctly installed. Contact your support personnel for assistance.
//
  ERROR_INSTALL_SERVICE_FAILURE = DWORD(1601);

//
// MessageId: ERROR_INSTALL_USEREXIT
//
// MessageText:
//
//  User cancelled installation.
//
  ERROR_INSTALL_USEREXIT = DWORD(1602);

//
// MessageId: ERROR_INSTALL_FAILURE
//
// MessageText:
//
//  Fatal error during installation.
//
  ERROR_INSTALL_FAILURE = DWORD(1603);

//
// MessageId: ERROR_INSTALL_SUSPEND
//
// MessageText:
//
//  Installation suspended, incomplete.
//
  ERROR_INSTALL_SUSPEND = DWORD(1604);

//
// MessageId: ERROR_UNKNOWN_PRODUCT
//
// MessageText:
//
//  This action is only valid for products that are currently installed.
//
  ERROR_UNKNOWN_PRODUCT = DWORD(1605);

//
// MessageId: ERROR_UNKNOWN_FEATURE
//
// MessageText:
//
//  Feature ID not registered.
//
  ERROR_UNKNOWN_FEATURE = DWORD(1606);

//
// MessageId: ERROR_UNKNOWN_COMPONENT
//
// MessageText:
//
//  Component ID not registered.
//
  ERROR_UNKNOWN_COMPONENT = DWORD(1607);

//
// MessageId: ERROR_UNKNOWN_PROPERTY
//
// MessageText:
//
//  Unknown property.
//
  ERROR_UNKNOWN_PROPERTY = DWORD(1608);

//
// MessageId: ERROR_INVALID_HANDLE_STATE
//
// MessageText:
//
//  Handle is in an invalid state.
//
  ERROR_INVALID_HANDLE_STATE = DWORD(1609);

//
// MessageId: ERROR_BAD_CONFIGURATION
//
// MessageText:
//
//  The configuration data for this product is corrupt.  Contact your support personnel.
//
  ERROR_BAD_CONFIGURATION = DWORD(1610);

//
// MessageId: ERROR_INDEX_ABSENT
//
// MessageText:
//
//  Component qualifier not present.
//
  ERROR_INDEX_ABSENT = DWORD(1611);

//
// MessageId: ERROR_INSTALL_SOURCE_ABSENT
//
// MessageText:
//
//  The installation source for this product is not available.  Verify that the source exists and that you can access it.
//
  ERROR_INSTALL_SOURCE_ABSENT = DWORD(1612);

//
// MessageId: ERROR_INSTALL_PACKAGE_VERSION
//
// MessageText:
//
//  This installation package cannot be installed by the Windows Installer service.  You must install a Windows service pack that contains a newer version of the Windows Installer service.
//
  ERROR_INSTALL_PACKAGE_VERSION = DWORD(1613);

//
// MessageId: ERROR_PRODUCT_UNINSTALLED
//
// MessageText:
//
//  Product is uninstalled.
//
  ERROR_PRODUCT_UNINSTALLED = DWORD(1614);

//
// MessageId: ERROR_BAD_QUERY_SYNTAX
//
// MessageText:
//
//  SQL query syntax invalid or unsupported.
//
  ERROR_BAD_QUERY_SYNTAX = DWORD(1615);

//
// MessageId: ERROR_INVALID_FIELD
//
// MessageText:
//
//  Record field does not exist.
//
  ERROR_INVALID_FIELD = DWORD(1616);

//
// MessageId: ERROR_DEVICE_REMOVED
//
// MessageText:
//
//  The device has been removed.
//
  ERROR_DEVICE_REMOVED = DWORD(1617);

//
// MessageId: ERROR_INSTALL_ALREADY_RUNNING
//
// MessageText:
//
//  Another installation is already in progress.  Complete that installation before proceeding with this install.
//
  ERROR_INSTALL_ALREADY_RUNNING = DWORD(1618);

//
// MessageId: ERROR_INSTALL_PACKAGE_OPEN_FAILED
//
// MessageText:
//
//  This installation package could not be opened.  Verify that the package exists and that you can access it, or contact the application vendor to verify that this is a valid Windows Installer package.
//
  ERROR_INSTALL_PACKAGE_OPEN_FAILED = DWORD(1619);

//
// MessageId: ERROR_INSTALL_PACKAGE_INVALID
//
// MessageText:
//
//  This installation package could not be opened.  Contact the application vendor to verify that this is a valid Windows Installer package.
//
  ERROR_INSTALL_PACKAGE_INVALID = DWORD(1620);

//
// MessageId: ERROR_INSTALL_UI_FAILURE
//
// MessageText:
//
//  There was an error starting the Windows Installer service user interface.  Contact your support personnel.
//
  ERROR_INSTALL_UI_FAILURE = DWORD(1621);

//
// MessageId: ERROR_INSTALL_LOG_FAILURE
//
// MessageText:
//
//  Error opening installation log file. Verify that the specified log file location exists and that you can write to it.
//
  ERROR_INSTALL_LOG_FAILURE = DWORD(1622);

//
// MessageId: ERROR_INSTALL_LANGUAGE_UNSUPPORTED
//
// MessageText:
//
//  The language of this installation package is not supported by your system.
//
  ERROR_INSTALL_LANGUAGE_UNSUPPORTED = DWORD(1623);

//
// MessageId: ERROR_INSTALL_TRANSFORM_FAILURE
//
// MessageText:
//
//  Error applying transforms.  Verify that the specified transform paths are valid.
//
  ERROR_INSTALL_TRANSFORM_FAILURE = DWORD(1624);

//
// MessageId: ERROR_INSTALL_PACKAGE_REJECTED
//
// MessageText:
//
//  This installation is forbidden by system policy.  Contact your system administrator.
//
  ERROR_INSTALL_PACKAGE_REJECTED = DWORD(1625);

//
// MessageId: ERROR_FUNCTION_NOT_CALLED
//
// MessageText:
//
//  Function could not be executed.
//
  ERROR_FUNCTION_NOT_CALLED = DWORD(1626);

//
// MessageId: ERROR_FUNCTION_FAILED
//
// MessageText:
//
//  Function failed during execution.
//
  ERROR_FUNCTION_FAILED = DWORD(1627);

//
// MessageId: ERROR_INVALID_TABLE
//
// MessageText:
//
//  Invalid or unknown table specified.
//
  ERROR_INVALID_TABLE = DWORD(1628);

//
// MessageId: ERROR_DATATYPE_MISMATCH
//
// MessageText:
//
//  Data supplied is of wrong type.
//
  ERROR_DATATYPE_MISMATCH = DWORD(1629);

//
// MessageId: ERROR_UNSUPPORTED_TYPE
//
// MessageText:
//
//  Data of this type is not supported.
//
  ERROR_UNSUPPORTED_TYPE = DWORD(1630);

//
// MessageId: ERROR_CREATE_FAILED
//
// MessageText:
//
//  The Windows Installer service failed to start.  Contact your support personnel.
//
  ERROR_CREATE_FAILED = DWORD(1631);

//
// MessageId: ERROR_INSTALL_TEMP_UNWRITABLE
//
// MessageText:
//
//  The Temp folder is on a drive that is full or is inaccessible. Free up space on the drive or verify that you have write permission on the Temp folder.
//
  ERROR_INSTALL_TEMP_UNWRITABLE = DWORD(1632);

//
// MessageId: ERROR_INSTALL_PLATFORM_UNSUPPORTED
//
// MessageText:
//
//  This installation package is not supported by this processor type. Contact your product vendor.
//
  ERROR_INSTALL_PLATFORM_UNSUPPORTED = DWORD(1633);

//
// MessageId: ERROR_INSTALL_NOTUSED
//
// MessageText:
//
//  Component not used on this computer.
//
  ERROR_INSTALL_NOTUSED = DWORD(1634);

//
// MessageId: ERROR_PATCH_PACKAGE_OPEN_FAILED
//
// MessageText:
//
//  This patch package could not be opened.  Verify that the patch package exists and that you can access it, or contact the application vendor to verify that this is a valid Windows Installer patch package.
//
  ERROR_PATCH_PACKAGE_OPEN_FAILED = DWORD(1635);

//
// MessageId: ERROR_PATCH_PACKAGE_INVALID
//
// MessageText:
//
//  This patch package could not be opened.  Contact the application vendor to verify that this is a valid Windows Installer patch package.
//
  ERROR_PATCH_PACKAGE_INVALID = DWORD(1636);

//
// MessageId: ERROR_PATCH_PACKAGE_UNSUPPORTED
//
// MessageText:
//
//  This patch package cannot be processed by the Windows Installer service.  You must install a Windows service pack that contains a newer version of the Windows Installer service.
//
  ERROR_PATCH_PACKAGE_UNSUPPORTED = DWORD(1637);

//
// MessageId: ERROR_PRODUCT_VERSION
//
// MessageText:
//
//  Another version of this product is already installed.  Installation of this version cannot continue.  To configure or remove the existing version of this product, use Add/Remove Programs on the Control Panel.
//
  ERROR_PRODUCT_VERSION = DWORD(1638);

//
// MessageId: ERROR_INVALID_COMMAND_LINE
//
// MessageText:
//
//  Invalid command line argument.  Consult the Windows Installer SDK for detailed command line help.
//
  ERROR_INVALID_COMMAND_LINE = DWORD(1639);

//
// MessageId: ERROR_INSTALL_REMOTE_DISALLOWED
//
// MessageText:
//
//  Only administrators have permission to add, remove, or configure server software during a Terminal services remote session. If you want to install or configure software on the server, contact your network administrator.
//
  ERROR_INSTALL_REMOTE_DISALLOWED = DWORD(1640);

//
// MessageId: ERROR_SUCCESS_REBOOT_INITIATED
//
// MessageText:
//
//  The requested operation completed successfully.  The system will be restarted so the changes can take effect.
//
  ERROR_SUCCESS_REBOOT_INITIATED = DWORD(1641);

//
// MessageId: ERROR_PATCH_TARGET_NOT_FOUND
//
// MessageText:
//
//  The upgrade patch cannot be installed by the Windows Installer service because the program to be upgraded may be missing, or the upgrade patch may update a different version of the program. Verify that the program to be upgraded exists on your computer an
//  d that you have the correct upgrade patch.
//
  ERROR_PATCH_TARGET_NOT_FOUND = DWORD(1642);

//
// MessageId: ERROR_PATCH_PACKAGE_REJECTED
//
// MessageText:
//
//  The patch package is not permitted by software restriction policy.
//
  ERROR_PATCH_PACKAGE_REJECTED = DWORD(1643);

//
// MessageId: ERROR_INSTALL_TRANSFORM_REJECTED
//
// MessageText:
//
//  One or more customizations are not permitted by software restriction policy.
//
  ERROR_INSTALL_TRANSFORM_REJECTED = DWORD(1644);

//
// MessageId: ERROR_INSTALL_REMOTE_PROHIBITED
//
// MessageText:
//
//  The Windows Installer does not permit installation from a Remote Desktop Connection.
//
  ERROR_INSTALL_REMOTE_PROHIBITED = DWORD(1645);

// End of MSI error codes



///////////////////////////
//                       //
//   RPC Status Codes    //
//                       //
///////////////////////////


//
// MessageId: RPC_S_INVALID_STRING_BINDING
//
// MessageText:
//
//  The string binding is invalid.
//
  RPC_S_INVALID_STRING_BINDING = DWORD(1700);

//
// MessageId: RPC_S_WRONG_KIND_OF_BINDING
//
// MessageText:
//
//  The binding handle is not the correct type.
//
  RPC_S_WRONG_KIND_OF_BINDING = DWORD(1701);

//
// MessageId: RPC_S_INVALID_BINDING
//
// MessageText:
//
//  The binding handle is invalid.
//
  RPC_S_INVALID_BINDING = DWORD(1702);

//
// MessageId: RPC_S_PROTSEQ_NOT_SUPPORTED
//
// MessageText:
//
//  The RPC protocol sequence is not supported.
//
  RPC_S_PROTSEQ_NOT_SUPPORTED = DWORD(1703);

//
// MessageId: RPC_S_INVALID_RPC_PROTSEQ
//
// MessageText:
//
//  The RPC protocol sequence is invalid.
//
  RPC_S_INVALID_RPC_PROTSEQ = DWORD(1704);

//
// MessageId: RPC_S_INVALID_STRING_UUID
//
// MessageText:
//
//  The string universal unique identifier (UUID) is invalid.
//
  RPC_S_INVALID_STRING_UUID = DWORD(1705);

//
// MessageId: RPC_S_INVALID_ENDPOINT_FORMAT
//
// MessageText:
//
//  The endpoint format is invalid.
//
  RPC_S_INVALID_ENDPOINT_FORMAT = DWORD(1706);

//
// MessageId: RPC_S_INVALID_NET_ADDR
//
// MessageText:
//
//  The network address is invalid.
//
  RPC_S_INVALID_NET_ADDR = DWORD(1707);

//
// MessageId: RPC_S_NO_ENDPOINT_FOUND
//
// MessageText:
//
//  No endpoint was found.
//
  RPC_S_NO_ENDPOINT_FOUND = DWORD(1708);

//
// MessageId: RPC_S_INVALID_TIMEOUT
//
// MessageText:
//
//  The timeout value is invalid.
//
  RPC_S_INVALID_TIMEOUT = DWORD(1709);

//
// MessageId: RPC_S_OBJECT_NOT_FOUND
//
// MessageText:
//
//  The object universal unique identifier (UUID) was not found.
//
  RPC_S_OBJECT_NOT_FOUND = DWORD(1710);

//
// MessageId: RPC_S_ALREADY_REGISTERED
//
// MessageText:
//
//  The object universal unique identifier (UUID) has already been registered.
//
  RPC_S_ALREADY_REGISTERED = DWORD(1711);

//
// MessageId: RPC_S_TYPE_ALREADY_REGISTERED
//
// MessageText:
//
//  The type universal unique identifier (UUID) has already been registered.
//
  RPC_S_TYPE_ALREADY_REGISTERED = DWORD(1712);

//
// MessageId: RPC_S_ALREADY_LISTENING
//
// MessageText:
//
//  The RPC server is already listening.
//
  RPC_S_ALREADY_LISTENING = DWORD(1713);

//
// MessageId: RPC_S_NO_PROTSEQS_REGISTERED
//
// MessageText:
//
//  No protocol sequences have been registered.
//
  RPC_S_NO_PROTSEQS_REGISTERED = DWORD(1714);

//
// MessageId: RPC_S_NOT_LISTENING
//
// MessageText:
//
//  The RPC server is not listening.
//
  RPC_S_NOT_LISTENING = DWORD(1715);

//
// MessageId: RPC_S_UNKNOWN_MGR_TYPE
//
// MessageText:
//
//  The manager type is unknown.
//
  RPC_S_UNKNOWN_MGR_TYPE = DWORD(1716);

//
// MessageId: RPC_S_UNKNOWN_IF
//
// MessageText:
//
//  The interface is unknown.
//
  RPC_S_UNKNOWN_IF = DWORD(1717);

//
// MessageId: RPC_S_NO_BINDINGS
//
// MessageText:
//
//  There are no bindings.
//
  RPC_S_NO_BINDINGS = DWORD(1718);

//
// MessageId: RPC_S_NO_PROTSEQS
//
// MessageText:
//
//  There are no protocol sequences.
//
  RPC_S_NO_PROTSEQS = DWORD(1719);

//
// MessageId: RPC_S_CANT_CREATE_ENDPOINT
//
// MessageText:
//
//  The endpoint cannot be created.
//
  RPC_S_CANT_CREATE_ENDPOINT = DWORD(1720);

//
// MessageId: RPC_S_OUT_OF_RESOURCES
//
// MessageText:
//
//  Not enough resources are available to complete this operation.
//
  RPC_S_OUT_OF_RESOURCES = DWORD(1721);

//
// MessageId: RPC_S_SERVER_UNAVAILABLE
//
// MessageText:
//
//  The RPC server is unavailable.
//
  RPC_S_SERVER_UNAVAILABLE = DWORD(1722);

//
// MessageId: RPC_S_SERVER_TOO_BUSY
//
// MessageText:
//
//  The RPC server is too busy to complete this operation.
//
  RPC_S_SERVER_TOO_BUSY = DWORD(1723);

//
// MessageId: RPC_S_INVALID_NETWORK_OPTIONS
//
// MessageText:
//
//  The network options are invalid.
//
  RPC_S_INVALID_NETWORK_OPTIONS = DWORD(1724);

//
// MessageId: RPC_S_NO_CALL_ACTIVE
//
// MessageText:
//
//  There are no remote procedure calls active on this thread.
//
  RPC_S_NO_CALL_ACTIVE = DWORD(1725);

//
// MessageId: RPC_S_CALL_FAILED
//
// MessageText:
//
//  The remote procedure call failed.
//
  RPC_S_CALL_FAILED = DWORD(1726);

//
// MessageId: RPC_S_CALL_FAILED_DNE
//
// MessageText:
//
//  The remote procedure call failed and did not execute.
//
  RPC_S_CALL_FAILED_DNE = DWORD(1727);

//
// MessageId: RPC_S_PROTOCOL_ERROR
//
// MessageText:
//
//  A remote procedure call (RPC) protocol error occurred.
//
  RPC_S_PROTOCOL_ERROR = DWORD(1728);

//
// MessageId: RPC_S_UNSUPPORTED_TRANS_SYN
//
// MessageText:
//
//  The transfer syntax is not supported by the RPC server.
//
  RPC_S_UNSUPPORTED_TRANS_SYN = DWORD(1730);

//
// MessageId: RPC_S_UNSUPPORTED_TYPE
//
// MessageText:
//
//  The universal unique identifier (UUID) type is not supported.
//
  RPC_S_UNSUPPORTED_TYPE = DWORD(1732);

//
// MessageId: RPC_S_INVALID_TAG
//
// MessageText:
//
//  The tag is invalid.
//
  RPC_S_INVALID_TAG = DWORD(1733);

//
// MessageId: RPC_S_INVALID_BOUND
//
// MessageText:
//
//  The array bounds are invalid.
//
  RPC_S_INVALID_BOUND = DWORD(1734);

//
// MessageId: RPC_S_NO_ENTRY_NAME
//
// MessageText:
//
//  The binding does not contain an entry name.
//
  RPC_S_NO_ENTRY_NAME = DWORD(1735);

//
// MessageId: RPC_S_INVALID_NAME_SYNTAX
//
// MessageText:
//
//  The name syntax is invalid.
//
  RPC_S_INVALID_NAME_SYNTAX = DWORD(1736);

//
// MessageId: RPC_S_UNSUPPORTED_NAME_SYNTAX
//
// MessageText:
//
//  The name syntax is not supported.
//
  RPC_S_UNSUPPORTED_NAME_SYNTAX = DWORD(1737);

//
// MessageId: RPC_S_UUID_NO_ADDRESS
//
// MessageText:
//
//  No network address is available to use to construct a universal unique identifier (UUID).
//
  RPC_S_UUID_NO_ADDRESS = DWORD(1739);

//
// MessageId: RPC_S_DUPLICATE_ENDPOINT
//
// MessageText:
//
//  The endpoint is a duplicate.
//
  RPC_S_DUPLICATE_ENDPOINT = DWORD(1740);

//
// MessageId: RPC_S_UNKNOWN_AUTHN_TYPE
//
// MessageText:
//
//  The authentication type is unknown.
//
  RPC_S_UNKNOWN_AUTHN_TYPE = DWORD(1741);

//
// MessageId: RPC_S_MAX_CALLS_TOO_SMALL
//
// MessageText:
//
//  The maximum number of calls is too small.
//
  RPC_S_MAX_CALLS_TOO_SMALL = DWORD(1742);

//
// MessageId: RPC_S_STRING_TOO_LONG
//
// MessageText:
//
//  The string is too long.
//
  RPC_S_STRING_TOO_LONG = DWORD(1743);

//
// MessageId: RPC_S_PROTSEQ_NOT_FOUND
//
// MessageText:
//
//  The RPC protocol sequence was not found.
//
  RPC_S_PROTSEQ_NOT_FOUND = DWORD(1744);

//
// MessageId: RPC_S_PROCNUM_OUT_OF_RANGE
//
// MessageText:
//
//  The procedure number is out of range.
//
  RPC_S_PROCNUM_OUT_OF_RANGE = DWORD(1745);

//
// MessageId: RPC_S_BINDING_HAS_NO_AUTH
//
// MessageText:
//
//  The binding does not contain any authentication information.
//
  RPC_S_BINDING_HAS_NO_AUTH = DWORD(1746);

//
// MessageId: RPC_S_UNKNOWN_AUTHN_SERVICE
//
// MessageText:
//
//  The authentication service is unknown.
//
  RPC_S_UNKNOWN_AUTHN_SERVICE = DWORD(1747);

//
// MessageId: RPC_S_UNKNOWN_AUTHN_LEVEL
//
// MessageText:
//
//  The authentication level is unknown.
//
  RPC_S_UNKNOWN_AUTHN_LEVEL = DWORD(1748);

//
// MessageId: RPC_S_INVALID_AUTH_IDENTITY
//
// MessageText:
//
//  The security context is invalid.
//
  RPC_S_INVALID_AUTH_IDENTITY = DWORD(1749);

//
// MessageId: RPC_S_UNKNOWN_AUTHZ_SERVICE
//
// MessageText:
//
//  The authorization service is unknown.
//
  RPC_S_UNKNOWN_AUTHZ_SERVICE = DWORD(1750);

//
// MessageId: EPT_S_INVALID_ENTRY
//
// MessageText:
//
//  The entry is invalid.
//
  EPT_S_INVALID_ENTRY = DWORD(1751);

//
// MessageId: EPT_S_CANT_PERFORM_OP
//
// MessageText:
//
//  The server endpoint cannot perform the operation.
//
  EPT_S_CANT_PERFORM_OP = DWORD(1752);

//
// MessageId: EPT_S_NOT_REGISTERED
//
// MessageText:
//
//  There are no more endpoints available from the endpoint mapper.
//
  EPT_S_NOT_REGISTERED = DWORD(1753);

//
// MessageId: RPC_S_NOTHING_TO_EXPORT
//
// MessageText:
//
//  No interfaces have been exported.
//
  RPC_S_NOTHING_TO_EXPORT = DWORD(1754);

//
// MessageId: RPC_S_INCOMPLETE_NAME
//
// MessageText:
//
//  The entry name is incomplete.
//
  RPC_S_INCOMPLETE_NAME = DWORD(1755);

//
// MessageId: RPC_S_INVALID_VERS_OPTION
//
// MessageText:
//
//  The version option is invalid.
//
  RPC_S_INVALID_VERS_OPTION = DWORD(1756);

//
// MessageId: RPC_S_NO_MORE_MEMBERS
//
// MessageText:
//
//  There are no more members.
//
  RPC_S_NO_MORE_MEMBERS = DWORD(1757);

//
// MessageId: RPC_S_NOT_ALL_OBJS_UNEXPORTED
//
// MessageText:
//
//  There is nothing to unexport.
//
  RPC_S_NOT_ALL_OBJS_UNEXPORTED = DWORD(1758);

//
// MessageId: RPC_S_INTERFACE_NOT_FOUND
//
// MessageText:
//
//  The interface was not found.
//
  RPC_S_INTERFACE_NOT_FOUND = DWORD(1759);

//
// MessageId: RPC_S_ENTRY_ALREADY_EXISTS
//
// MessageText:
//
//  The entry already exists.
//
  RPC_S_ENTRY_ALREADY_EXISTS = DWORD(1760);

//
// MessageId: RPC_S_ENTRY_NOT_FOUND
//
// MessageText:
//
//  The entry is not found.
//
  RPC_S_ENTRY_NOT_FOUND = DWORD(1761);

//
// MessageId: RPC_S_NAME_SERVICE_UNAVAILABLE
//
// MessageText:
//
//  The name service is unavailable.
//
  RPC_S_NAME_SERVICE_UNAVAILABLE = DWORD(1762);

//
// MessageId: RPC_S_INVALID_NAF_ID
//
// MessageText:
//
//  The network address family is invalid.
//
  RPC_S_INVALID_NAF_ID = DWORD(1763);

//
// MessageId: RPC_S_CANNOT_SUPPORT
//
// MessageText:
//
//  The requested operation is not supported.
//
  RPC_S_CANNOT_SUPPORT = DWORD(1764);

//
// MessageId: RPC_S_NO_CONTEXT_AVAILABLE
//
// MessageText:
//
//  No security context is available to allow impersonation.
//
  RPC_S_NO_CONTEXT_AVAILABLE = DWORD(1765);

//
// MessageId: RPC_S_INTERNAL_ERROR
//
// MessageText:
//
//  An internal error occurred in a remote procedure call (RPC).
//
  RPC_S_INTERNAL_ERROR = DWORD(1766);

//
// MessageId: RPC_S_ZERO_DIVIDE
//
// MessageText:
//
//  The RPC server attempted an integer division by zero.
//
  RPC_S_ZERO_DIVIDE = DWORD(1767);

//
// MessageId: RPC_S_ADDRESS_ERROR
//
// MessageText:
//
//  An addressing error occurred in the RPC server.
//
  RPC_S_ADDRESS_ERROR = DWORD(1768);

//
// MessageId: RPC_S_FP_DIV_ZERO
//
// MessageText:
//
//  A floating-point operation at the RPC server caused a division by zero.
//
  RPC_S_FP_DIV_ZERO = DWORD(1769);

//
// MessageId: RPC_S_FP_UNDERFLOW
//
// MessageText:
//
//  A floating-point underflow occurred at the RPC server.
//
  RPC_S_FP_UNDERFLOW = DWORD(1770);

//
// MessageId: RPC_S_FP_OVERFLOW
//
// MessageText:
//
//  A floating-point overflow occurred at the RPC server.
//
  RPC_S_FP_OVERFLOW = DWORD(1771);

//
// MessageId: RPC_X_NO_MORE_ENTRIES
//
// MessageText:
//
//  The list of RPC servers available for the binding of auto handles has been exhausted.
//
  RPC_X_NO_MORE_ENTRIES = DWORD(1772);

//
// MessageId: RPC_X_SS_CHAR_TRANS_OPEN_FAIL
//
// MessageText:
//
//  Unable to open the character translation table file.
//
  RPC_X_SS_CHAR_TRANS_OPEN_FAIL = DWORD(1773);

//
// MessageId: RPC_X_SS_CHAR_TRANS_SHORT_FILE
//
// MessageText:
//
//  The file containing the character translation table has fewer than 512 bytes.
//
  RPC_X_SS_CHAR_TRANS_SHORT_FILE = DWORD(1774);

//
// MessageId: RPC_X_SS_IN_NULL_CONTEXT
//
// MessageText:
//
//  A null context handle was passed from the client to the host during a remote procedure call.
//
  RPC_X_SS_IN_NULL_CONTEXT = DWORD(1775);

//
// MessageId: RPC_X_SS_CONTEXT_DAMAGED
//
// MessageText:
//
//  The context handle changed during a remote procedure call.
//
  RPC_X_SS_CONTEXT_DAMAGED = DWORD(1777);

//
// MessageId: RPC_X_SS_HANDLES_MISMATCH
//
// MessageText:
//
//  The binding handles passed to a remote procedure call do not match.
//
  RPC_X_SS_HANDLES_MISMATCH = DWORD(1778);

//
// MessageId: RPC_X_SS_CANNOT_GET_CALL_HANDLE
//
// MessageText:
//
//  The stub is unable to get the remote procedure call handle.
//
  RPC_X_SS_CANNOT_GET_CALL_HANDLE = DWORD(1779);

//
// MessageId: RPC_X_NULL_REF_POINTER
//
// MessageText:
//
//  A null reference pointer was passed to the stub.
//
  RPC_X_NULL_REF_POINTER = DWORD(1780);

//
// MessageId: RPC_X_ENUM_VALUE_OUT_OF_RANGE
//
// MessageText:
//
//  The enumeration value is out of range.
//
  RPC_X_ENUM_VALUE_OUT_OF_RANGE = DWORD(1781);

//
// MessageId: RPC_X_BYTE_COUNT_TOO_SMALL
//
// MessageText:
//
//  The byte count is too small.
//
  RPC_X_BYTE_COUNT_TOO_SMALL = DWORD(1782);

//
// MessageId: RPC_X_BAD_STUB_DATA
//
// MessageText:
//
//  The stub received bad data.
//
  RPC_X_BAD_STUB_DATA = DWORD(1783);

//
// MessageId: ERROR_INVALID_USER_BUFFER
//
// MessageText:
//
//  The supplied user buffer is not valid for the requested operation.
//
  ERROR_INVALID_USER_BUFFER = DWORD(1784);

//
// MessageId: ERROR_UNRECOGNIZED_MEDIA
//
// MessageText:
//
//  The disk media is not recognized. It may not be formatted.
//
  ERROR_UNRECOGNIZED_MEDIA = DWORD(1785);

//
// MessageId: ERROR_NO_TRUST_LSA_SECRET
//
// MessageText:
//
//  The workstation does not have a trust secret.
//
  ERROR_NO_TRUST_LSA_SECRET = DWORD(1786);

//
// MessageId: ERROR_NO_TRUST_SAM_ACCOUNT
//
// MessageText:
//
//  The security database on the server does not have a computer account for this workstation trust relationship.
//
  ERROR_NO_TRUST_SAM_ACCOUNT = DWORD(1787);

//
// MessageId: ERROR_TRUSTED_DOMAIN_FAILURE
//
// MessageText:
//
//  The trust relationship between the primary domain and the trusted domain failed.
//
  ERROR_TRUSTED_DOMAIN_FAILURE = DWORD(1788);

//
// MessageId: ERROR_TRUSTED_RELATIONSHIP_FAILURE
//
// MessageText:
//
//  The trust relationship between this workstation and the primary domain failed.
//
  ERROR_TRUSTED_RELATIONSHIP_FAILURE = DWORD(1789);

//
// MessageId: ERROR_TRUST_FAILURE
//
// MessageText:
//
//  The network logon failed.
//
  ERROR_TRUST_FAILURE = DWORD(1790);

//
// MessageId: RPC_S_CALL_IN_PROGRESS
//
// MessageText:
//
//  A remote procedure call is already in progress for this thread.
//
  RPC_S_CALL_IN_PROGRESS = DWORD(1791);

//
// MessageId: ERROR_NETLOGON_NOT_STARTED
//
// MessageText:
//
//  An attempt was made to logon, but the network logon service was not started.
//
  ERROR_NETLOGON_NOT_STARTED = DWORD(1792);

//
// MessageId: ERROR_ACCOUNT_EXPIRED
//
// MessageText:
//
//  The user's account has expired.
//
  ERROR_ACCOUNT_EXPIRED = DWORD(1793);

//
// MessageId: ERROR_REDIRECTOR_HAS_OPEN_HANDLES
//
// MessageText:
//
//  The redirector is in use and cannot be unloaded.
//
  ERROR_REDIRECTOR_HAS_OPEN_HANDLES = DWORD(1794);

//
// MessageId: ERROR_PRINTER_DRIVER_ALREADY_INSTALLED
//
// MessageText:
//
//  The specified printer driver is already installed.
//
  ERROR_PRINTER_DRIVER_ALREADY_INSTALLED = DWORD(1795);

//
// MessageId: ERROR_UNKNOWN_PORT
//
// MessageText:
//
//  The specified port is unknown.
//
  ERROR_UNKNOWN_PORT = DWORD(1796);

//
// MessageId: ERROR_UNKNOWN_PRINTER_DRIVER
//
// MessageText:
//
//  The printer driver is unknown.
//
  ERROR_UNKNOWN_PRINTER_DRIVER = DWORD(1797);

//
// MessageId: ERROR_UNKNOWN_PRINTPROCESSOR
//
// MessageText:
//
//  The print processor is unknown.
//
  ERROR_UNKNOWN_PRINTPROCESSOR = DWORD(1798);

//
// MessageId: ERROR_INVALID_SEPARATOR_FILE
//
// MessageText:
//
//  The specified separator file is invalid.
//
  ERROR_INVALID_SEPARATOR_FILE = DWORD(1799);

//
// MessageId: ERROR_INVALID_PRIORITY
//
// MessageText:
//
//  The specified priority is invalid.
//
  ERROR_INVALID_PRIORITY = DWORD(1800);

//
// MessageId: ERROR_INVALID_PRINTER_NAME
//
// MessageText:
//
//  The printer name is invalid.
//
  ERROR_INVALID_PRINTER_NAME = DWORD(1801);

//
// MessageId: ERROR_PRINTER_ALREADY_EXISTS
//
// MessageText:
//
//  The printer already exists.
//
  ERROR_PRINTER_ALREADY_EXISTS = DWORD(1802);

//
// MessageId: ERROR_INVALID_PRINTER_COMMAND
//
// MessageText:
//
//  The printer command is invalid.
//
  ERROR_INVALID_PRINTER_COMMAND = DWORD(1803);

//
// MessageId: ERROR_INVALID_DATATYPE
//
// MessageText:
//
//  The specified datatype is invalid.
//
  ERROR_INVALID_DATATYPE = DWORD(1804);

//
// MessageId: ERROR_INVALID_ENVIRONMENT
//
// MessageText:
//
//  The environment specified is invalid.
//
  ERROR_INVALID_ENVIRONMENT = DWORD(1805);

//
// MessageId: RPC_S_NO_MORE_BINDINGS
//
// MessageText:
//
//  There are no more bindings.
//
  RPC_S_NO_MORE_BINDINGS = DWORD(1806);

//
// MessageId: ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT
//
// MessageText:
//
//  The account used is an interdomain trust account. Use your global user account or local user account to access this server.
//
  ERROR_NOLOGON_INTERDOMAIN_TRUST_ACCOUNT = DWORD(1807);

//
// MessageId: ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT
//
// MessageText:
//
//  The account used is a computer account. Use your global user account or local user account to access this server.
//
  ERROR_NOLOGON_WORKSTATION_TRUST_ACCOUNT = DWORD(1808);

//
// MessageId: ERROR_NOLOGON_SERVER_TRUST_ACCOUNT
//
// MessageText:
//
//  The account used is a server trust account. Use your global user account or local user account to access this server.
//
  ERROR_NOLOGON_SERVER_TRUST_ACCOUNT = DWORD(1809);

//
// MessageId: ERROR_DOMAIN_TRUST_INCONSISTENT
//
// MessageText:
//
//  The name or security ID (SID) of the domain specified is inconsistent with the trust information for that domain.
//
  ERROR_DOMAIN_TRUST_INCONSISTENT = DWORD(1810);

//
// MessageId: ERROR_SERVER_HAS_OPEN_HANDLES
//
// MessageText:
//
//  The server is in use and cannot be unloaded.
//
  ERROR_SERVER_HAS_OPEN_HANDLES = DWORD(1811);

//
// MessageId: ERROR_RESOURCE_DATA_NOT_FOUND
//
// MessageText:
//
//  The specified image file did not contain a resource section.
//
  ERROR_RESOURCE_DATA_NOT_FOUND = DWORD(1812);

//
// MessageId: ERROR_RESOURCE_TYPE_NOT_FOUND
//
// MessageText:
//
//  The specified resource type cannot be found in the image file.
//
  ERROR_RESOURCE_TYPE_NOT_FOUND = DWORD(1813);

//
// MessageId: ERROR_RESOURCE_NAME_NOT_FOUND
//
// MessageText:
//
//  The specified resource name cannot be found in the image file.
//
  ERROR_RESOURCE_NAME_NOT_FOUND = DWORD(1814);

//
// MessageId: ERROR_RESOURCE_LANG_NOT_FOUND
//
// MessageText:
//
//  The specified resource language ID cannot be found in the image file.
//
  ERROR_RESOURCE_LANG_NOT_FOUND = DWORD(1815);

//
// MessageId: ERROR_NOT_ENOUGH_QUOTA
//
// MessageText:
//
//  Not enough quota is available to process this command.
//
  ERROR_NOT_ENOUGH_QUOTA = DWORD(1816);

//
// MessageId: RPC_S_NO_INTERFACES
//
// MessageText:
//
//  No interfaces have been registered.
//
  RPC_S_NO_INTERFACES = DWORD(1817);

//
// MessageId: RPC_S_CALL_CANCELLED
//
// MessageText:
//
//  The remote procedure call was cancelled.
//
  RPC_S_CALL_CANCELLED = DWORD(1818);

//
// MessageId: RPC_S_BINDING_INCOMPLETE
//
// MessageText:
//
//  The binding handle does not contain all required information.
//
  RPC_S_BINDING_INCOMPLETE = DWORD(1819);

//
// MessageId: RPC_S_COMM_FAILURE
//
// MessageText:
//
//  A communications failure occurred during a remote procedure call.
//
  RPC_S_COMM_FAILURE = DWORD(1820);

//
// MessageId: RPC_S_UNSUPPORTED_AUTHN_LEVEL
//
// MessageText:
//
//  The requested authentication level is not supported.
//
  RPC_S_UNSUPPORTED_AUTHN_LEVEL = DWORD(1821);

//
// MessageId: RPC_S_NO_PRINC_NAME
//
// MessageText:
//
//  No principal name registered.
//
  RPC_S_NO_PRINC_NAME = DWORD(1822);

//
// MessageId: RPC_S_NOT_RPC_ERROR
//
// MessageText:
//
//  The error specified is not a valid Windows RPC error code.
//
  RPC_S_NOT_RPC_ERROR = DWORD(1823);

//
// MessageId: RPC_S_UUID_LOCAL_ONLY
//
// MessageText:
//
//  A UUID that is valid only on this computer has been allocated.
//
  RPC_S_UUID_LOCAL_ONLY = DWORD(1824);

//
// MessageId: RPC_S_SEC_PKG_ERROR
//
// MessageText:
//
//  A security package specific error occurred.
//
  RPC_S_SEC_PKG_ERROR = DWORD(1825);

//
// MessageId: RPC_S_NOT_CANCELLED
//
// MessageText:
//
//  Thread is not canceled.
//
  RPC_S_NOT_CANCELLED = DWORD(1826);

//
// MessageId: RPC_X_INVALID_ES_ACTION
//
// MessageText:
//
//  Invalid operation on the encoding/decoding handle.
//
  RPC_X_INVALID_ES_ACTION = DWORD(1827);

//
// MessageId: RPC_X_WRONG_ES_VERSION
//
// MessageText:
//
//  Incompatible version of the serializing package.
//
  RPC_X_WRONG_ES_VERSION = DWORD(1828);

//
// MessageId: RPC_X_WRONG_STUB_VERSION
//
// MessageText:
//
//  Incompatible version of the RPC stub.
//
  RPC_X_WRONG_STUB_VERSION = DWORD(1829);

//
// MessageId: RPC_X_INVALID_PIPE_OBJECT
//
// MessageText:
//
//  The RPC pipe object is invalid or corrupted.
//
  RPC_X_INVALID_PIPE_OBJECT = DWORD(1830);

//
// MessageId: RPC_X_WRONG_PIPE_ORDER
//
// MessageText:
//
//  An invalid operation was attempted on an RPC pipe object.
//
  RPC_X_WRONG_PIPE_ORDER = DWORD(1831);

//
// MessageId: RPC_X_WRONG_PIPE_VERSION
//
// MessageText:
//
//  Unsupported RPC pipe version.
//
  RPC_X_WRONG_PIPE_VERSION = DWORD(1832);

//
// MessageId: RPC_S_GROUP_MEMBER_NOT_FOUND
//
// MessageText:
//
//  The group member was not found.
//
  RPC_S_GROUP_MEMBER_NOT_FOUND = DWORD(1898);

//
// MessageId: EPT_S_CANT_CREATE
//
// MessageText:
//
//  The endpoint mapper database entry could not be created.
//
  EPT_S_CANT_CREATE = DWORD(1899);

//
// MessageId: RPC_S_INVALID_OBJECT
//
// MessageText:
//
//  The object universal unique identifier (UUID) is the nil UUID.
//
  RPC_S_INVALID_OBJECT = DWORD(1900);

//
// MessageId: ERROR_INVALID_TIME
//
// MessageText:
//
//  The specified time is invalid.
//
  ERROR_INVALID_TIME = DWORD(1901);

//
// MessageId: ERROR_INVALID_FORM_NAME
//
// MessageText:
//
//  The specified form name is invalid.
//
  ERROR_INVALID_FORM_NAME = DWORD(1902);

//
// MessageId: ERROR_INVALID_FORM_SIZE
//
// MessageText:
//
//  The specified form size is invalid.
//
  ERROR_INVALID_FORM_SIZE = DWORD(1903);

//
// MessageId: ERROR_ALREADY_WAITING
//
// MessageText:
//
//  The specified printer handle is already being waited on
//
  ERROR_ALREADY_WAITING = DWORD(1904);

//
// MessageId: ERROR_PRINTER_DELETED
//
// MessageText:
//
//  The specified printer has been deleted.
//
  ERROR_PRINTER_DELETED = DWORD(1905);

//
// MessageId: ERROR_INVALID_PRINTER_STATE
//
// MessageText:
//
//  The state of the printer is invalid.
//
  ERROR_INVALID_PRINTER_STATE = DWORD(1906);

//
// MessageId: ERROR_PASSWORD_MUST_CHANGE
//
// MessageText:
//
//  The user's password must be changed before logging on the first time.
//
  ERROR_PASSWORD_MUST_CHANGE = DWORD(1907);

//
// MessageId: ERROR_DOMAIN_CONTROLLER_NOT_FOUND
//
// MessageText:
//
//  Could not find the domain controller for this domain.
//
  ERROR_DOMAIN_CONTROLLER_NOT_FOUND = DWORD(1908);

//
// MessageId: ERROR_ACCOUNT_LOCKED_OUT
//
// MessageText:
//
//  The referenced account is currently locked out and may not be logged on to.
//
  ERROR_ACCOUNT_LOCKED_OUT = DWORD(1909);

//
// MessageId: OR_INVALID_OXID
//
// MessageText:
//
//  The object exporter specified was not found.
//
  OR_INVALID_OXID = DWORD(1910);

//
// MessageId: OR_INVALID_OID
//
// MessageText:
//
//  The object specified was not found.
//
  OR_INVALID_OID = DWORD(1911);

//
// MessageId: OR_INVALID_SET
//
// MessageText:
//
//  The object resolver set specified was not found.
//
  OR_INVALID_SET = DWORD(1912);

//
// MessageId: RPC_S_SEND_INCOMPLETE
//
// MessageText:
//
//  Some data remains to be sent in the request buffer.
//
  RPC_S_SEND_INCOMPLETE = DWORD(1913);

//
// MessageId: RPC_S_INVALID_ASYNC_HANDLE
//
// MessageText:
//
//  Invalid asynchronous remote procedure call handle.
//
  RPC_S_INVALID_ASYNC_HANDLE = DWORD(1914);

//
// MessageId: RPC_S_INVALID_ASYNC_CALL
//
// MessageText:
//
//  Invalid asynchronous RPC call handle for this operation.
//
  RPC_S_INVALID_ASYNC_CALL = DWORD(1915);

//
// MessageId: RPC_X_PIPE_CLOSED
//
// MessageText:
//
//  The RPC pipe object has already been closed.
//
  RPC_X_PIPE_CLOSED = DWORD(1916);

//
// MessageId: RPC_X_PIPE_DISCIPLINE_ERROR
//
// MessageText:
//
//  The RPC call completed before all pipes were processed.
//
  RPC_X_PIPE_DISCIPLINE_ERROR = DWORD(1917);

//
// MessageId: RPC_X_PIPE_EMPTY
//
// MessageText:
//
//  No more data is available from the RPC pipe.
//
  RPC_X_PIPE_EMPTY = DWORD(1918);

//
// MessageId: ERROR_NO_SITENAME
//
// MessageText:
//
//  No site name is available for this machine.
//
  ERROR_NO_SITENAME = DWORD(1919);

//
// MessageId: ERROR_CANT_ACCESS_FILE
//
// MessageText:
//
//  The file can not be accessed by the system.
//
  ERROR_CANT_ACCESS_FILE = DWORD(1920);

//
// MessageId: ERROR_CANT_RESOLVE_FILENAME
//
// MessageText:
//
//  The name of the file cannot be resolved by the system.
//
  ERROR_CANT_RESOLVE_FILENAME = DWORD(1921);

//
// MessageId: RPC_S_ENTRY_TYPE_MISMATCH
//
// MessageText:
//
//  The entry is not of the expected type.
//
  RPC_S_ENTRY_TYPE_MISMATCH = DWORD(1922);

//
// MessageId: RPC_S_NOT_ALL_OBJS_EXPORTED
//
// MessageText:
//
//  Not all object UUIDs could be exported to the specified entry.
//
  RPC_S_NOT_ALL_OBJS_EXPORTED = DWORD(1923);

//
// MessageId: RPC_S_INTERFACE_NOT_EXPORTED
//
// MessageText:
//
//  Interface could not be exported to the specified entry.
//
  RPC_S_INTERFACE_NOT_EXPORTED = DWORD(1924);

//
// MessageId: RPC_S_PROFILE_NOT_ADDED
//
// MessageText:
//
//  The specified profile entry could not be added.
//
  RPC_S_PROFILE_NOT_ADDED = DWORD(1925);

//
// MessageId: RPC_S_PRF_ELT_NOT_ADDED
//
// MessageText:
//
//  The specified profile element could not be added.
//
  RPC_S_PRF_ELT_NOT_ADDED = DWORD(1926);

//
// MessageId: RPC_S_PRF_ELT_NOT_REMOVED
//
// MessageText:
//
//  The specified profile element could not be removed.
//
  RPC_S_PRF_ELT_NOT_REMOVED = DWORD(1927);

//
// MessageId: RPC_S_GRP_ELT_NOT_ADDED
//
// MessageText:
//
//  The group element could not be added.
//
  RPC_S_GRP_ELT_NOT_ADDED = DWORD(1928);

//
// MessageId: RPC_S_GRP_ELT_NOT_REMOVED
//
// MessageText:
//
//  The group element could not be removed.
//
  RPC_S_GRP_ELT_NOT_REMOVED = DWORD(1929);

//
// MessageId: ERROR_KM_DRIVER_BLOCKED
//
// MessageText:
//
//  The printer driver is not compatible with a policy enabled on your computer that blocks NT 4.0 drivers.
//
  ERROR_KM_DRIVER_BLOCKED = DWORD(1930);

//
// MessageId: ERROR_CONTEXT_EXPIRED
//
// MessageText:
//
//  The context has expired and can no longer be used.
//
  ERROR_CONTEXT_EXPIRED = DWORD(1931);

//
// MessageId: ERROR_PER_USER_TRUST_QUOTA_EXCEEDED
//
// MessageText:
//
//  The current user's delegated trust creation quota has been exceeded.
//
  ERROR_PER_USER_TRUST_QUOTA_EXCEEDED = DWORD(1932);

//
// MessageId: ERROR_ALL_USER_TRUST_QUOTA_EXCEEDED
//
// MessageText:
//
//  The total delegated trust creation quota has been exceeded.
//
  ERROR_ALL_USER_TRUST_QUOTA_EXCEEDED = DWORD(1933);

//
// MessageId: ERROR_USER_DELETE_TRUST_QUOTA_EXCEEDED
//
// MessageText:
//
//  The current user's delegated trust deletion quota has been exceeded.
//
  ERROR_USER_DELETE_TRUST_QUOTA_EXCEEDED = DWORD(1934);

//
// MessageId: ERROR_AUTHENTICATION_FIREWALL_FAILED
//
// MessageText:
//
//  Logon Failure: The machine you are logging onto is protected by an authentication firewall.  The specified account is not allowed to authenticate to the machine.
//
  ERROR_AUTHENTICATION_FIREWALL_FAILED = DWORD(1935);

//
// MessageId: ERROR_REMOTE_PRINT_CONNECTIONS_BLOCKED
//
// MessageText:
//
//  Remote connections to the Print Spooler are blocked by a policy set on your machine.
//
  ERROR_REMOTE_PRINT_CONNECTIONS_BLOCKED = DWORD(1936);




///////////////////////////
//                       //
//   OpenGL Error Code   //
//                       //
///////////////////////////


//
// MessageId: ERROR_INVALID_PIXEL_FORMAT
//
// MessageText:
//
//  The pixel format is invalid.
//
  ERROR_INVALID_PIXEL_FORMAT = DWORD(2000);

//
// MessageId: ERROR_BAD_DRIVER
//
// MessageText:
//
//  The specified driver is invalid.
//
  ERROR_BAD_DRIVER = DWORD(2001);

//
// MessageId: ERROR_INVALID_WINDOW_STYLE
//
// MessageText:
//
//  The window style or class attribute is invalid for this operation.
//
  ERROR_INVALID_WINDOW_STYLE = DWORD(2002);

//
// MessageId: ERROR_METAFILE_NOT_SUPPORTED
//
// MessageText:
//
//  The requested metafile operation is not supported.
//
  ERROR_METAFILE_NOT_SUPPORTED = DWORD(2003);

//
// MessageId: ERROR_TRANSFORM_NOT_SUPPORTED
//
// MessageText:
//
//  The requested transformation operation is not supported.
//
  ERROR_TRANSFORM_NOT_SUPPORTED = DWORD(2004);

//
// MessageId: ERROR_CLIPPING_NOT_SUPPORTED
//
// MessageText:
//
//  The requested clipping operation is not supported.
//
  ERROR_CLIPPING_NOT_SUPPORTED = DWORD(2005);

// End of OpenGL error codes



///////////////////////////////////////////
//                                       //
//   Image Color Management Error Code   //
//                                       //
///////////////////////////////////////////


//
// MessageId: ERROR_INVALID_CMM
//
// MessageText:
//
//  The specified color management module is invalid.
//
  ERROR_INVALID_CMM = DWORD(2010);

//
// MessageId: ERROR_INVALID_PROFILE
//
// MessageText:
//
//  The specified color profile is invalid.
//
  ERROR_INVALID_PROFILE = DWORD(2011);

//
// MessageId: ERROR_TAG_NOT_FOUND
//
// MessageText:
//
//  The specified tag was not found.
//
  ERROR_TAG_NOT_FOUND = DWORD(2012);

//
// MessageId: ERROR_TAG_NOT_PRESENT
//
// MessageText:
//
//  A required tag is not present.
//
  ERROR_TAG_NOT_PRESENT = DWORD(2013);

//
// MessageId: ERROR_DUPLICATE_TAG
//
// MessageText:
//
//  The specified tag is already present.
//
  ERROR_DUPLICATE_TAG = DWORD(2014);

//
// MessageId: ERROR_PROFILE_NOT_ASSOCIATED_WITH_DEVICE
//
// MessageText:
//
//  The specified color profile is not associated with any device.
//
  ERROR_PROFILE_NOT_ASSOCIATED_WITH_DEVICE = DWORD(2015);

//
// MessageId: ERROR_PROFILE_NOT_FOUND
//
// MessageText:
//
//  The specified color profile was not found.
//
  ERROR_PROFILE_NOT_FOUND = DWORD(2016);

//
// MessageId: ERROR_INVALID_COLORSPACE
//
// MessageText:
//
//  The specified color space is invalid.
//
  ERROR_INVALID_COLORSPACE = DWORD(2017);

//
// MessageId: ERROR_ICM_NOT_ENABLED
//
// MessageText:
//
//  Image Color Management is not enabled.
//
  ERROR_ICM_NOT_ENABLED = DWORD(2018);

//
// MessageId: ERROR_DELETING_ICM_XFORM
//
// MessageText:
//
//  There was an error while deleting the color transform.
//
  ERROR_DELETING_ICM_XFORM = DWORD(2019);

//
// MessageId: ERROR_INVALID_TRANSFORM
//
// MessageText:
//
//  The specified color transform is invalid.
//
  ERROR_INVALID_TRANSFORM = DWORD(2020);

//
// MessageId: ERROR_COLORSPACE_MISMATCH
//
// MessageText:
//
//  The specified transform does not match the bitmap's color space.
//
  ERROR_COLORSPACE_MISMATCH = DWORD(2021);

//
// MessageId: ERROR_INVALID_COLORINDEX
//
// MessageText:
//
//  The specified named color index is not present in the profile.
//
  ERROR_INVALID_COLORINDEX = DWORD(2022);




///////////////////////////
//                       //
// Winnet32 Status Codes //
//                       //
// The range 2100 through 2999 is reserved for network status codes.
// See lmerr.h for a complete listing
///////////////////////////


//
// MessageId: ERROR_CONNECTED_OTHER_PASSWORD
//
// MessageText:
//
//  The network connection was made successfully, but the user had to be prompted for a password other than the one originally specified.
//
  ERROR_CONNECTED_OTHER_PASSWORD = DWORD(2108);

//
// MessageId: ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT
//
// MessageText:
//
//  The network connection was made successfully using default credentials.
//
  ERROR_CONNECTED_OTHER_PASSWORD_DEFAULT = DWORD(2109);

//
// MessageId: ERROR_BAD_USERNAME
//
// MessageText:
//
//  The specified username is invalid.
//
  ERROR_BAD_USERNAME = DWORD(2202);

//
// MessageId: ERROR_NOT_CONNECTED
//
// MessageText:
//
//  This network connection does not exist.
//
  ERROR_NOT_CONNECTED = DWORD(2250);

//
// MessageId: ERROR_OPEN_FILES
//
// MessageText:
//
//  This network connection has files open or requests pending.
//
  ERROR_OPEN_FILES = DWORD(2401);

//
// MessageId: ERROR_ACTIVE_CONNECTIONS
//
// MessageText:
//
//  Active connections still exist.
//
  ERROR_ACTIVE_CONNECTIONS = DWORD(2402);

//
// MessageId: ERROR_DEVICE_IN_USE
//
// MessageText:
//
//  The device is in use by an active process and cannot be disconnected.
//
  ERROR_DEVICE_IN_USE = DWORD(2404);


////////////////////////////////////
//                                //
//     Win32 Spooler Error Codes  //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_UNKNOWN_PRINT_MONITOR
//
// MessageText:
//
//  The specified print monitor is unknown.
//
  ERROR_UNKNOWN_PRINT_MONITOR = DWORD(3000);

//
// MessageId: ERROR_PRINTER_DRIVER_IN_USE
//
// MessageText:
//
//  The specified printer driver is currently in use.
//
  ERROR_PRINTER_DRIVER_IN_USE = DWORD(3001);

//
// MessageId: ERROR_SPOOL_FILE_NOT_FOUND
//
// MessageText:
//
//  The spool file was not found.
//
  ERROR_SPOOL_FILE_NOT_FOUND = DWORD(3002);

//
// MessageId: ERROR_SPL_NO_STARTDOC
//
// MessageText:
//
//  A StartDocPrinter call was not issued.
//
  ERROR_SPL_NO_STARTDOC = DWORD(3003);

//
// MessageId: ERROR_SPL_NO_ADDJOB
//
// MessageText:
//
//  An AddJob call was not issued.
//
  ERROR_SPL_NO_ADDJOB = DWORD(3004);

//
// MessageId: ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED
//
// MessageText:
//
//  The specified print processor has already been installed.
//
  ERROR_PRINT_PROCESSOR_ALREADY_INSTALLED = DWORD(3005);

//
// MessageId: ERROR_PRINT_MONITOR_ALREADY_INSTALLED
//
// MessageText:
//
//  The specified print monitor has already been installed.
//
  ERROR_PRINT_MONITOR_ALREADY_INSTALLED = DWORD(3006);

//
// MessageId: ERROR_INVALID_PRINT_MONITOR
//
// MessageText:
//
//  The specified print monitor does not have the required functions.
//
  ERROR_INVALID_PRINT_MONITOR = DWORD(3007);

//
// MessageId: ERROR_PRINT_MONITOR_IN_USE
//
// MessageText:
//
//  The specified print monitor is currently in use.
//
  ERROR_PRINT_MONITOR_IN_USE = DWORD(3008);

//
// MessageId: ERROR_PRINTER_HAS_JOBS_QUEUED
//
// MessageText:
//
//  The requested operation is not allowed when there are jobs queued to the printer.
//
  ERROR_PRINTER_HAS_JOBS_QUEUED = DWORD(3009);

//
// MessageId: ERROR_SUCCESS_REBOOT_REQUIRED
//
// MessageText:
//
//  The requested operation is successful. Changes will not be effective until the system is rebooted.
//
  ERROR_SUCCESS_REBOOT_REQUIRED = DWORD(3010);

//
// MessageId: ERROR_SUCCESS_RESTART_REQUIRED
//
// MessageText:
//
//  The requested operation is successful. Changes will not be effective until the service is restarted.
//
  ERROR_SUCCESS_RESTART_REQUIRED = DWORD(3011);

//
// MessageId: ERROR_PRINTER_NOT_FOUND
//
// MessageText:
//
//  No printers were found.
//
  ERROR_PRINTER_NOT_FOUND = DWORD(3012);

//
// MessageId: ERROR_PRINTER_DRIVER_WARNED
//
// MessageText:
//
//  The printer driver is known to be unreliable.
//
  ERROR_PRINTER_DRIVER_WARNED = DWORD(3013);

//
// MessageId: ERROR_PRINTER_DRIVER_BLOCKED
//
// MessageText:
//
//  The printer driver is known to harm the system.
//
  ERROR_PRINTER_DRIVER_BLOCKED = DWORD(3014);

////////////////////////////////////
//                                //
//     Wins Error Codes           //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_WINS_INTERNAL
//
// MessageText:
//
//  WINS encountered an error while processing the command.
//
  ERROR_WINS_INTERNAL = DWORD(4000);

//
// MessageId: ERROR_CAN_NOT_DEL_LOCAL_WINS
//
// MessageText:
//
//  The local WINS can not be deleted.
//
  ERROR_CAN_NOT_DEL_LOCAL_WINS = DWORD(4001);

//
// MessageId: ERROR_STATIC_INIT
//
// MessageText:
//
//  The importation from the file failed.
//
  ERROR_STATIC_INIT = DWORD(4002);

//
// MessageId: ERROR_INC_BACKUP
//
// MessageText:
//
//  The backup failed. Was a full backup done before?
//
  ERROR_INC_BACKUP = DWORD(4003);

//
// MessageId: ERROR_FULL_BACKUP
//
// MessageText:
//
//  The backup failed. Check the directory to which you are backing the database.
//
  ERROR_FULL_BACKUP = DWORD(4004);

//
// MessageId: ERROR_REC_NON_EXISTENT
//
// MessageText:
//
//  The name does not exist in the WINS database.
//
  ERROR_REC_NON_EXISTENT = DWORD(4005);

//
// MessageId: ERROR_RPL_NOT_ALLOWED
//
// MessageText:
//
//  Replication with a nonconfigured partner is not allowed.
//
  ERROR_RPL_NOT_ALLOWED = DWORD(4006);

////////////////////////////////////
//                                //
//     DHCP Error Codes           //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_DHCP_ADDRESS_CONFLICT
//
// MessageText:
//
//  The DHCP client has obtained an IP address that is already in use on the network. The local interface will be disabled until the DHCP client can obtain a new address.
//
  ERROR_DHCP_ADDRESS_CONFLICT = DWORD(4100);

////////////////////////////////////
//                                //
//     WMI Error Codes            //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_WMI_GUID_NOT_FOUND
//
// MessageText:
//
//  The GUID passed was not recognized as valid by a WMI data provider.
//
  ERROR_WMI_GUID_NOT_FOUND = DWORD(4200);

//
// MessageId: ERROR_WMI_INSTANCE_NOT_FOUND
//
// MessageText:
//
//  The instance name passed was not recognized as valid by a WMI data provider.
//
  ERROR_WMI_INSTANCE_NOT_FOUND = DWORD(4201);

//
// MessageId: ERROR_WMI_ITEMID_NOT_FOUND
//
// MessageText:
//
//  The data item ID passed was not recognized as valid by a WMI data provider.
//
  ERROR_WMI_ITEMID_NOT_FOUND = DWORD(4202);

//
// MessageId: ERROR_WMI_TRY_AGAIN
//
// MessageText:
//
//  The WMI request could not be completed and should be retried.
//
  ERROR_WMI_TRY_AGAIN = DWORD(4203);

//
// MessageId: ERROR_WMI_DP_NOT_FOUND
//
// MessageText:
//
//  The WMI data provider could not be located.
//
  ERROR_WMI_DP_NOT_FOUND = DWORD(4204);

//
// MessageId: ERROR_WMI_UNRESOLVED_INSTANCE_REF
//
// MessageText:
//
//  The WMI data provider references an instance set that has not been registered.
//
  ERROR_WMI_UNRESOLVED_INSTANCE_REF = DWORD(4205);

//
// MessageId: ERROR_WMI_ALREADY_ENABLED
//
// MessageText:
//
//  The WMI data block or event notification has already been enabled.
//
  ERROR_WMI_ALREADY_ENABLED = DWORD(4206);

//
// MessageId: ERROR_WMI_GUID_DISCONNECTED
//
// MessageText:
//
//  The WMI data block is no longer available.
//
  ERROR_WMI_GUID_DISCONNECTED = DWORD(4207);

//
// MessageId: ERROR_WMI_SERVER_UNAVAILABLE
//
// MessageText:
//
//  The WMI data service is not available.
//
  ERROR_WMI_SERVER_UNAVAILABLE = DWORD(4208);

//
// MessageId: ERROR_WMI_DP_FAILED
//
// MessageText:
//
//  The WMI data provider failed to carry out the request.
//
  ERROR_WMI_DP_FAILED = DWORD(4209);

//
// MessageId: ERROR_WMI_INVALID_MOF
//
// MessageText:
//
//  The WMI MOF information is not valid.
//
  ERROR_WMI_INVALID_MOF = DWORD(4210);

//
// MessageId: ERROR_WMI_INVALID_REGINFO
//
// MessageText:
//
//  The WMI registration information is not valid.
//
  ERROR_WMI_INVALID_REGINFO = DWORD(4211);

//
// MessageId: ERROR_WMI_ALREADY_DISABLED
//
// MessageText:
//
//  The WMI data block or event notification has already been disabled.
//
  ERROR_WMI_ALREADY_DISABLED = DWORD(4212);

//
// MessageId: ERROR_WMI_READ_ONLY
//
// MessageText:
//
//  The WMI data item or data block is read only.
//
  ERROR_WMI_READ_ONLY = DWORD(4213);

//
// MessageId: ERROR_WMI_SET_FAILURE
//
// MessageText:
//
//  The WMI data item or data block could not be changed.
//
  ERROR_WMI_SET_FAILURE = DWORD(4214);

//////////////////////////////////////////
//                                      //
// NT Media Services (RSM) Error Codes  //
//                                      //
//////////////////////////////////////////
//
// MessageId: ERROR_INVALID_MEDIA
//
// MessageText:
//
//  The media identifier does not represent a valid medium.
//
  ERROR_INVALID_MEDIA = DWORD(4300);

//
// MessageId: ERROR_INVALID_LIBRARY
//
// MessageText:
//
//  The library identifier does not represent a valid library.
//
  ERROR_INVALID_LIBRARY = DWORD(4301);

//
// MessageId: ERROR_INVALID_MEDIA_POOL
//
// MessageText:
//
//  The media pool identifier does not represent a valid media pool.
//
  ERROR_INVALID_MEDIA_POOL = DWORD(4302);

//
// MessageId: ERROR_DRIVE_MEDIA_MISMATCH
//
// MessageText:
//
//  The drive and medium are not compatible or exist in different libraries.
//
  ERROR_DRIVE_MEDIA_MISMATCH = DWORD(4303);

//
// MessageId: ERROR_MEDIA_OFFLINE
//
// MessageText:
//
//  The medium currently exists in an offline library and must be online to perform this operation.
//
  ERROR_MEDIA_OFFLINE = DWORD(4304);

//
// MessageId: ERROR_LIBRARY_OFFLINE
//
// MessageText:
//
//  The operation cannot be performed on an offline library.
//
  ERROR_LIBRARY_OFFLINE = DWORD(4305);

//
// MessageId: ERROR_EMPTY
//
// MessageText:
//
//  The library, drive, or media pool is empty.
//
  ERROR_EMPTY = DWORD(4306);

//
// MessageId: ERROR_NOT_EMPTY
//
// MessageText:
//
//  The library, drive, or media pool must be empty to perform this operation.
//
  ERROR_NOT_EMPTY = DWORD(4307);

//
// MessageId: ERROR_MEDIA_UNAVAILABLE
//
// MessageText:
//
//  No media is currently available in this media pool or library.
//
  ERROR_MEDIA_UNAVAILABLE = DWORD(4308);

//
// MessageId: ERROR_RESOURCE_DISABLED
//
// MessageText:
//
//  A resource required for this operation is disabled.
//
  ERROR_RESOURCE_DISABLED = DWORD(4309);

//
// MessageId: ERROR_INVALID_CLEANER
//
// MessageText:
//
//  The media identifier does not represent a valid cleaner.
//
  ERROR_INVALID_CLEANER = DWORD(4310);

//
// MessageId: ERROR_UNABLE_TO_CLEAN
//
// MessageText:
//
//  The drive cannot be cleaned or does not support cleaning.
//
  ERROR_UNABLE_TO_CLEAN = DWORD(4311);

//
// MessageId: ERROR_OBJECT_NOT_FOUND
//
// MessageText:
//
//  The object identifier does not represent a valid object.
//
  ERROR_OBJECT_NOT_FOUND = DWORD(4312);

//
// MessageId: ERROR_DATABASE_FAILURE
//
// MessageText:
//
//  Unable to read from or write to the database.
//
  ERROR_DATABASE_FAILURE = DWORD(4313);

//
// MessageId: ERROR_DATABASE_FULL
//
// MessageText:
//
//  The database is full.
//
  ERROR_DATABASE_FULL = DWORD(4314);

//
// MessageId: ERROR_MEDIA_INCOMPATIBLE
//
// MessageText:
//
//  The medium is not compatible with the device or media pool.
//
  ERROR_MEDIA_INCOMPATIBLE = DWORD(4315);

//
// MessageId: ERROR_RESOURCE_NOT_PRESENT
//
// MessageText:
//
//  The resource required for this operation does not exist.
//
  ERROR_RESOURCE_NOT_PRESENT = DWORD(4316);

//
// MessageId: ERROR_INVALID_OPERATION
//
// MessageText:
//
//  The operation identifier is not valid.
//
  ERROR_INVALID_OPERATION = DWORD(4317);

//
// MessageId: ERROR_MEDIA_NOT_AVAILABLE
//
// MessageText:
//
//  The media is not mounted or ready for use.
//
  ERROR_MEDIA_NOT_AVAILABLE = DWORD(4318);

//
// MessageId: ERROR_DEVICE_NOT_AVAILABLE
//
// MessageText:
//
//  The device is not ready for use.
//
  ERROR_DEVICE_NOT_AVAILABLE = DWORD(4319);

//
// MessageId: ERROR_REQUEST_REFUSED
//
// MessageText:
//
//  The operator or administrator has refused the request.
//
  ERROR_REQUEST_REFUSED = DWORD(4320);

//
// MessageId: ERROR_INVALID_DRIVE_OBJECT
//
// MessageText:
//
//  The drive identifier does not represent a valid drive.
//
  ERROR_INVALID_DRIVE_OBJECT = DWORD(4321);

//
// MessageId: ERROR_LIBRARY_FULL
//
// MessageText:
//
//  Library is full.  No slot is available for use.
//
  ERROR_LIBRARY_FULL = DWORD(4322);

//
// MessageId: ERROR_MEDIUM_NOT_ACCESSIBLE
//
// MessageText:
//
//  The transport cannot access the medium.
//
  ERROR_MEDIUM_NOT_ACCESSIBLE = DWORD(4323);

//
// MessageId: ERROR_UNABLE_TO_LOAD_MEDIUM
//
// MessageText:
//
//  Unable to load the medium into the drive.
//
  ERROR_UNABLE_TO_LOAD_MEDIUM = DWORD(4324);

//
// MessageId: ERROR_UNABLE_TO_INVENTORY_DRIVE
//
// MessageText:
//
//  Unable to retrieve the drive status.
//
  ERROR_UNABLE_TO_INVENTORY_DRIVE = DWORD(4325);

//
// MessageId: ERROR_UNABLE_TO_INVENTORY_SLOT
//
// MessageText:
//
//  Unable to retrieve the slot status.
//
  ERROR_UNABLE_TO_INVENTORY_SLOT = DWORD(4326);

//
// MessageId: ERROR_UNABLE_TO_INVENTORY_TRANSPORT
//
// MessageText:
//
//  Unable to retrieve status about the transport.
//
  ERROR_UNABLE_TO_INVENTORY_TRANSPORT = DWORD(4327);

//
// MessageId: ERROR_TRANSPORT_FULL
//
// MessageText:
//
//  Cannot use the transport because it is already in use.
//
  ERROR_TRANSPORT_FULL = DWORD(4328);

//
// MessageId: ERROR_CONTROLLING_IEPORT
//
// MessageText:
//
//  Unable to open or close the inject/eject port.
//
  ERROR_CONTROLLING_IEPORT = DWORD(4329);

//
// MessageId: ERROR_UNABLE_TO_EJECT_MOUNTED_MEDIA
//
// MessageText:
//
//  Unable to eject the medium because it is in a drive.
//
  ERROR_UNABLE_TO_EJECT_MOUNTED_MEDIA = DWORD(4330);

//
// MessageId: ERROR_CLEANER_SLOT_SET
//
// MessageText:
//
//  A cleaner slot is already reserved.
//
  ERROR_CLEANER_SLOT_SET = DWORD(4331);

//
// MessageId: ERROR_CLEANER_SLOT_NOT_SET
//
// MessageText:
//
//  A cleaner slot is not reserved.
//
  ERROR_CLEANER_SLOT_NOT_SET = DWORD(4332);

//
// MessageId: ERROR_CLEANER_CARTRIDGE_SPENT
//
// MessageText:
//
//  The cleaner cartridge has performed the maximum number of drive cleanings.
//
  ERROR_CLEANER_CARTRIDGE_SPENT = DWORD(4333);

//
// MessageId: ERROR_UNEXPECTED_OMID
//
// MessageText:
//
//  Unexpected on-medium identifier.
//
  ERROR_UNEXPECTED_OMID = DWORD(4334);

//
// MessageId: ERROR_CANT_DELETE_LAST_ITEM
//
// MessageText:
//
//  The last remaining item in this group or resource cannot be deleted.
//
  ERROR_CANT_DELETE_LAST_ITEM = DWORD(4335);

//
// MessageId: ERROR_MESSAGE_EXCEEDS_MAX_SIZE
//
// MessageText:
//
//  The message provided exceeds the maximum size allowed for this parameter.
//
  ERROR_MESSAGE_EXCEEDS_MAX_SIZE = DWORD(4336);

//
// MessageId: ERROR_VOLUME_CONTAINS_SYS_FILES
//
// MessageText:
//
//  The volume contains system or paging files.
//
  ERROR_VOLUME_CONTAINS_SYS_FILES = DWORD(4337);

//
// MessageId: ERROR_INDIGENOUS_TYPE
//
// MessageText:
//
//  The media type cannot be removed from this library since at least one drive in the library reports it can support this media type.
//
  ERROR_INDIGENOUS_TYPE = DWORD(4338);

//
// MessageId: ERROR_NO_SUPPORTING_DRIVES
//
// MessageText:
//
//  This offline media cannot be mounted on this system since no enabled drives are present which can be used.
//
  ERROR_NO_SUPPORTING_DRIVES = DWORD(4339);

//
// MessageId: ERROR_CLEANER_CARTRIDGE_INSTALLED
//
// MessageText:
//
//  A cleaner cartridge is present in the tape library.
//
  ERROR_CLEANER_CARTRIDGE_INSTALLED = DWORD(4340);

//
// MessageId: ERROR_IEPORT_FULL
//
// MessageText:
//
//  Cannot use the ieport because it is not empty.
//
  ERROR_IEPORT_FULL = DWORD(4341);

////////////////////////////////////////////
//                                        //
// NT Remote Storage Service Error Codes  //
//                                        //
////////////////////////////////////////////
//
// MessageId: ERROR_FILE_OFFLINE
//
// MessageText:
//
//  The remote storage service was not able to recall the file.
//
  ERROR_FILE_OFFLINE = DWORD(4350);

//
// MessageId: ERROR_REMOTE_STORAGE_NOT_ACTIVE
//
// MessageText:
//
//  The remote storage service is not operational at this time.
//
  ERROR_REMOTE_STORAGE_NOT_ACTIVE = DWORD(4351);

//
// MessageId: ERROR_REMOTE_STORAGE_MEDIA_ERROR
//
// MessageText:
//
//  The remote storage service encountered a media error.
//
  ERROR_REMOTE_STORAGE_MEDIA_ERROR = DWORD(4352);

////////////////////////////////////////////
//                                        //
// NT Reparse Points Error Codes          //
//                                        //
////////////////////////////////////////////
//
// MessageId: ERROR_NOT_A_REPARSE_POINT
//
// MessageText:
//
//  The file or directory is not a reparse point.
//
  ERROR_NOT_A_REPARSE_POINT = DWORD(4390);

//
// MessageId: ERROR_REPARSE_ATTRIBUTE_CONFLICT
//
// MessageText:
//
//  The reparse point attribute cannot be set because it conflicts with an existing attribute.
//
  ERROR_REPARSE_ATTRIBUTE_CONFLICT = DWORD(4391);

//
// MessageId: ERROR_INVALID_REPARSE_DATA
//
// MessageText:
//
//  The data present in the reparse point buffer is invalid.
//
  ERROR_INVALID_REPARSE_DATA = DWORD(4392);

//
// MessageId: ERROR_REPARSE_TAG_INVALID
//
// MessageText:
//
//  The tag present in the reparse point buffer is invalid.
//
  ERROR_REPARSE_TAG_INVALID = DWORD(4393);

//
// MessageId: ERROR_REPARSE_TAG_MISMATCH
//
// MessageText:
//
//  There is a mismatch between the tag specified in the request and the tag present in the reparse point.
//  
//
  ERROR_REPARSE_TAG_MISMATCH = DWORD(4394);

////////////////////////////////////////////
//                                        //
// NT Single Instance Store Error Codes   //
//                                        //
////////////////////////////////////////////
//
// MessageId: ERROR_VOLUME_NOT_SIS_ENABLED
//
// MessageText:
//
//  Single Instance Storage is not available on this volume.
//
  ERROR_VOLUME_NOT_SIS_ENABLED = DWORD(4500);

////////////////////////////////////
//                                //
//     Cluster Error Codes        //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_DEPENDENT_RESOURCE_EXISTS
//
// MessageText:
//
//  The cluster resource cannot be moved to another group because other resources are dependent on it.
//
  ERROR_DEPENDENT_RESOURCE_EXISTS = DWORD(5001);

//
// MessageId: ERROR_DEPENDENCY_NOT_FOUND
//
// MessageText:
//
//  The cluster resource dependency cannot be found.
//
  ERROR_DEPENDENCY_NOT_FOUND = DWORD(5002);

//
// MessageId: ERROR_DEPENDENCY_ALREADY_EXISTS
//
// MessageText:
//
//  The cluster resource cannot be made dependent on the specified resource because it is already dependent.
//
  ERROR_DEPENDENCY_ALREADY_EXISTS = DWORD(5003);

//
// MessageId: ERROR_RESOURCE_NOT_ONLINE
//
// MessageText:
//
//  The cluster resource is not online.
//
  ERROR_RESOURCE_NOT_ONLINE = DWORD(5004);

//
// MessageId: ERROR_HOST_NODE_NOT_AVAILABLE
//
// MessageText:
//
//  A cluster node is not available for this operation.
//
  ERROR_HOST_NODE_NOT_AVAILABLE = DWORD(5005);

//
// MessageId: ERROR_RESOURCE_NOT_AVAILABLE
//
// MessageText:
//
//  The cluster resource is not available.
//
  ERROR_RESOURCE_NOT_AVAILABLE = DWORD(5006);

//
// MessageId: ERROR_RESOURCE_NOT_FOUND
//
// MessageText:
//
//  The cluster resource could not be found.
//
  ERROR_RESOURCE_NOT_FOUND = DWORD(5007);

//
// MessageId: ERROR_SHUTDOWN_CLUSTER
//
// MessageText:
//
//  The cluster is being shut down.
//
  ERROR_SHUTDOWN_CLUSTER = DWORD(5008);

//
// MessageId: ERROR_CANT_EVICT_ACTIVE_NODE
//
// MessageText:
//
//  A cluster node cannot be evicted from the cluster unless the node is down or it is the last node.
//
  ERROR_CANT_EVICT_ACTIVE_NODE = DWORD(5009);

//
// MessageId: ERROR_OBJECT_ALREADY_EXISTS
//
// MessageText:
//
//  The object already exists.
//
  ERROR_OBJECT_ALREADY_EXISTS = DWORD(5010);

//
// MessageId: ERROR_OBJECT_IN_LIST
//
// MessageText:
//
//  The object is already in the list.
//
  ERROR_OBJECT_IN_LIST = DWORD(5011);

//
// MessageId: ERROR_GROUP_NOT_AVAILABLE
//
// MessageText:
//
//  The cluster group is not available for any new requests.
//
  ERROR_GROUP_NOT_AVAILABLE = DWORD(5012);

//
// MessageId: ERROR_GROUP_NOT_FOUND
//
// MessageText:
//
//  The cluster group could not be found.
//
  ERROR_GROUP_NOT_FOUND = DWORD(5013);

//
// MessageId: ERROR_GROUP_NOT_ONLINE
//
// MessageText:
//
//  The operation could not be completed because the cluster group is not online.
//
  ERROR_GROUP_NOT_ONLINE = DWORD(5014);

//
// MessageId: ERROR_HOST_NODE_NOT_RESOURCE_OWNER
//
// MessageText:
//
//  The cluster node is not the owner of the resource.
//
  ERROR_HOST_NODE_NOT_RESOURCE_OWNER = DWORD(5015);

//
// MessageId: ERROR_HOST_NODE_NOT_GROUP_OWNER
//
// MessageText:
//
//  The cluster node is not the owner of the group.
//
  ERROR_HOST_NODE_NOT_GROUP_OWNER = DWORD(5016);

//
// MessageId: ERROR_RESMON_CREATE_FAILED
//
// MessageText:
//
//  The cluster resource could not be created in the specified resource monitor.
//
  ERROR_RESMON_CREATE_FAILED = DWORD(5017);

//
// MessageId: ERROR_RESMON_ONLINE_FAILED
//
// MessageText:
//
//  The cluster resource could not be brought online by the resource monitor.
//
  ERROR_RESMON_ONLINE_FAILED = DWORD(5018);

//
// MessageId: ERROR_RESOURCE_ONLINE
//
// MessageText:
//
//  The operation could not be completed because the cluster resource is online.
//
  ERROR_RESOURCE_ONLINE = DWORD(5019);

//
// MessageId: ERROR_QUORUM_RESOURCE
//
// MessageText:
//
//  The cluster resource could not be deleted or brought offline because it is the quorum resource.
//
  ERROR_QUORUM_RESOURCE = DWORD(5020);

//
// MessageId: ERROR_NOT_QUORUM_CAPABLE
//
// MessageText:
//
//  The cluster could not make the specified resource a quorum resource because it is not capable of being a quorum resource.
//
  ERROR_NOT_QUORUM_CAPABLE = DWORD(5021);

//
// MessageId: ERROR_CLUSTER_SHUTTING_DOWN
//
// MessageText:
//
//  The cluster software is shutting down.
//
  ERROR_CLUSTER_SHUTTING_DOWN = DWORD(5022);

//
// MessageId: ERROR_INVALID_STATE
//
// MessageText:
//
//  The group or resource is not in the correct state to perform the requested operation.
//
  ERROR_INVALID_STATE = DWORD(5023);

//
// MessageId: ERROR_RESOURCE_PROPERTIES_STORED
//
// MessageText:
//
//  The properties were stored but not all changes will take effect until the next time the resource is brought online.
//
  ERROR_RESOURCE_PROPERTIES_STORED = DWORD(5024);

//
// MessageId: ERROR_NOT_QUORUM_CLASS
//
// MessageText:
//
//  The cluster could not make the specified resource a quorum resource because it does not belong to a shared storage class.
//
  ERROR_NOT_QUORUM_CLASS = DWORD(5025);

//
// MessageId: ERROR_CORE_RESOURCE
//
// MessageText:
//
//  The cluster resource could not be deleted since it is a core resource.
//
  ERROR_CORE_RESOURCE = DWORD(5026);

//
// MessageId: ERROR_QUORUM_RESOURCE_ONLINE_FAILED
//
// MessageText:
//
//  The quorum resource failed to come online.
//
  ERROR_QUORUM_RESOURCE_ONLINE_FAILED = DWORD(5027);

//
// MessageId: ERROR_QUORUMLOG_OPEN_FAILED
//
// MessageText:
//
//  The quorum log could not be created or mounted successfully.
//
  ERROR_QUORUMLOG_OPEN_FAILED = DWORD(5028);

//
// MessageId: ERROR_CLUSTERLOG_CORRUPT
//
// MessageText:
//
//  The cluster log is corrupt.
//
  ERROR_CLUSTERLOG_CORRUPT = DWORD(5029);

//
// MessageId: ERROR_CLUSTERLOG_RECORD_EXCEEDS_MAXSIZE
//
// MessageText:
//
//  The record could not be written to the cluster log since it exceeds the maximum size.
//
  ERROR_CLUSTERLOG_RECORD_EXCEEDS_MAXSIZE = DWORD(5030);

//
// MessageId: ERROR_CLUSTERLOG_EXCEEDS_MAXSIZE
//
// MessageText:
//
//  The cluster log exceeds its maximum size.
//
  ERROR_CLUSTERLOG_EXCEEDS_MAXSIZE = DWORD(5031);

//
// MessageId: ERROR_CLUSTERLOG_CHKPOINT_NOT_FOUND
//
// MessageText:
//
//  No checkpoint record was found in the cluster log.
//
  ERROR_CLUSTERLOG_CHKPOINT_NOT_FOUND = DWORD(5032);

//
// MessageId: ERROR_CLUSTERLOG_NOT_ENOUGH_SPACE
//
// MessageText:
//
//  The minimum required disk space needed for logging is not available.
//
  ERROR_CLUSTERLOG_NOT_ENOUGH_SPACE = DWORD(5033);

//
// MessageId: ERROR_QUORUM_OWNER_ALIVE
//
// MessageText:
//
//  The cluster node failed to take control of the quorum resource because the resource is owned by another active node.
//
  ERROR_QUORUM_OWNER_ALIVE = DWORD(5034);

//
// MessageId: ERROR_NETWORK_NOT_AVAILABLE
//
// MessageText:
//
//  A cluster network is not available for this operation.
//
  ERROR_NETWORK_NOT_AVAILABLE = DWORD(5035);

//
// MessageId: ERROR_NODE_NOT_AVAILABLE
//
// MessageText:
//
//  A cluster node is not available for this operation.
//
  ERROR_NODE_NOT_AVAILABLE = DWORD(5036);

//
// MessageId: ERROR_ALL_NODES_NOT_AVAILABLE
//
// MessageText:
//
//  All cluster nodes must be running to perform this operation.
//
  ERROR_ALL_NODES_NOT_AVAILABLE = DWORD(5037);

//
// MessageId: ERROR_RESOURCE_FAILED
//
// MessageText:
//
//  A cluster resource failed.
//
  ERROR_RESOURCE_FAILED = DWORD(5038);

//
// MessageId: ERROR_CLUSTER_INVALID_NODE
//
// MessageText:
//
//  The cluster node is not valid.
//
  ERROR_CLUSTER_INVALID_NODE = DWORD(5039);

//
// MessageId: ERROR_CLUSTER_NODE_EXISTS
//
// MessageText:
//
//  The cluster node already exists.
//
  ERROR_CLUSTER_NODE_EXISTS = DWORD(5040);

//
// MessageId: ERROR_CLUSTER_JOIN_IN_PROGRESS
//
// MessageText:
//
//  A node is in the process of joining the cluster.
//
  ERROR_CLUSTER_JOIN_IN_PROGRESS = DWORD(5041);

//
// MessageId: ERROR_CLUSTER_NODE_NOT_FOUND
//
// MessageText:
//
//  The cluster node was not found.
//
  ERROR_CLUSTER_NODE_NOT_FOUND = DWORD(5042);

//
// MessageId: ERROR_CLUSTER_LOCAL_NODE_NOT_FOUND
//
// MessageText:
//
//  The cluster local node information was not found.
//
  ERROR_CLUSTER_LOCAL_NODE_NOT_FOUND = DWORD(5043);

//
// MessageId: ERROR_CLUSTER_NETWORK_EXISTS
//
// MessageText:
//
//  The cluster network already exists.
//
  ERROR_CLUSTER_NETWORK_EXISTS = DWORD(5044);

//
// MessageId: ERROR_CLUSTER_NETWORK_NOT_FOUND
//
// MessageText:
//
//  The cluster network was not found.
//
  ERROR_CLUSTER_NETWORK_NOT_FOUND = DWORD(5045);

//
// MessageId: ERROR_CLUSTER_NETINTERFACE_EXISTS
//
// MessageText:
//
//  The cluster network interface already exists.
//
  ERROR_CLUSTER_NETINTERFACE_EXISTS = DWORD(5046);

//
// MessageId: ERROR_CLUSTER_NETINTERFACE_NOT_FOUND
//
// MessageText:
//
//  The cluster network interface was not found.
//
  ERROR_CLUSTER_NETINTERFACE_NOT_FOUND = DWORD(5047);

//
// MessageId: ERROR_CLUSTER_INVALID_REQUEST
//
// MessageText:
//
//  The cluster request is not valid for this object.
//
  ERROR_CLUSTER_INVALID_REQUEST = DWORD(5048);

//
// MessageId: ERROR_CLUSTER_INVALID_NETWORK_PROVIDER
//
// MessageText:
//
//  The cluster network provider is not valid.
//
  ERROR_CLUSTER_INVALID_NETWORK_PROVIDER = DWORD(5049);

//
// MessageId: ERROR_CLUSTER_NODE_DOWN
//
// MessageText:
//
//  The cluster node is down.
//
  ERROR_CLUSTER_NODE_DOWN = DWORD(5050);

//
// MessageId: ERROR_CLUSTER_NODE_UNREACHABLE
//
// MessageText:
//
//  The cluster node is not reachable.
//
  ERROR_CLUSTER_NODE_UNREACHABLE = DWORD(5051);

//
// MessageId: ERROR_CLUSTER_NODE_NOT_MEMBER
//
// MessageText:
//
//  The cluster node is not a member of the cluster.
//
  ERROR_CLUSTER_NODE_NOT_MEMBER = DWORD(5052);

//
// MessageId: ERROR_CLUSTER_JOIN_NOT_IN_PROGRESS
//
// MessageText:
//
//  A cluster join operation is not in progress.
//
  ERROR_CLUSTER_JOIN_NOT_IN_PROGRESS = DWORD(5053);

//
// MessageId: ERROR_CLUSTER_INVALID_NETWORK
//
// MessageText:
//
//  The cluster network is not valid.
//
  ERROR_CLUSTER_INVALID_NETWORK = DWORD(5054);

//
// MessageId: ERROR_CLUSTER_NODE_UP
//
// MessageText:
//
//  The cluster node is up.
//
  ERROR_CLUSTER_NODE_UP = DWORD(5056);

//
// MessageId: ERROR_CLUSTER_IPADDR_IN_USE
//
// MessageText:
//
//  The cluster IP address is already in use.
//
  ERROR_CLUSTER_IPADDR_IN_USE = DWORD(5057);

//
// MessageId: ERROR_CLUSTER_NODE_NOT_PAUSED
//
// MessageText:
//
//  The cluster node is not paused.
//
  ERROR_CLUSTER_NODE_NOT_PAUSED = DWORD(5058);

//
// MessageId: ERROR_CLUSTER_NO_SECURITY_CONTEXT
//
// MessageText:
//
//  No cluster security context is available.
//
  ERROR_CLUSTER_NO_SECURITY_CONTEXT = DWORD(5059);

//
// MessageId: ERROR_CLUSTER_NETWORK_NOT_INTERNAL
//
// MessageText:
//
//  The cluster network is not configured for internal cluster communication.
//
  ERROR_CLUSTER_NETWORK_NOT_INTERNAL = DWORD(5060);

//
// MessageId: ERROR_CLUSTER_NODE_ALREADY_UP
//
// MessageText:
//
//  The cluster node is already up.
//
  ERROR_CLUSTER_NODE_ALREADY_UP = DWORD(5061);

//
// MessageId: ERROR_CLUSTER_NODE_ALREADY_DOWN
//
// MessageText:
//
//  The cluster node is already down.
//
  ERROR_CLUSTER_NODE_ALREADY_DOWN = DWORD(5062);

//
// MessageId: ERROR_CLUSTER_NETWORK_ALREADY_ONLINE
//
// MessageText:
//
//  The cluster network is already online.
//
  ERROR_CLUSTER_NETWORK_ALREADY_ONLINE = DWORD(5063);

//
// MessageId: ERROR_CLUSTER_NETWORK_ALREADY_OFFLINE
//
// MessageText:
//
//  The cluster network is already offline.
//
  ERROR_CLUSTER_NETWORK_ALREADY_OFFLINE = DWORD(5064);

//
// MessageId: ERROR_CLUSTER_NODE_ALREADY_MEMBER
//
// MessageText:
//
//  The cluster node is already a member of the cluster.
//
  ERROR_CLUSTER_NODE_ALREADY_MEMBER = DWORD(5065);

//
// MessageId: ERROR_CLUSTER_LAST_INTERNAL_NETWORK
//
// MessageText:
//
//  The cluster network is the only one configured for internal cluster communication between two or more active cluster nodes. The internal communication capability cannot be removed from the network.
//
  ERROR_CLUSTER_LAST_INTERNAL_NETWORK = DWORD(5066);

//
// MessageId: ERROR_CLUSTER_NETWORK_HAS_DEPENDENTS
//
// MessageText:
//
//  One or more cluster resources depend on the network to provide service to clients. The client access capability cannot be removed from the network.
//
  ERROR_CLUSTER_NETWORK_HAS_DEPENDENTS = DWORD(5067);

//
// MessageId: ERROR_INVALID_OPERATION_ON_QUORUM
//
// MessageText:
//
//  This operation cannot be performed on the cluster resource as it the quorum resource. You may not bring the quorum resource offline or modify its possible owners list.
//
  ERROR_INVALID_OPERATION_ON_QUORUM = DWORD(5068);

//
// MessageId: ERROR_DEPENDENCY_NOT_ALLOWED
//
// MessageText:
//
//  The cluster quorum resource is not allowed to have any dependencies.
//
  ERROR_DEPENDENCY_NOT_ALLOWED = DWORD(5069);

//
// MessageId: ERROR_CLUSTER_NODE_PAUSED
//
// MessageText:
//
//  The cluster node is paused.
//
  ERROR_CLUSTER_NODE_PAUSED = DWORD(5070);

//
// MessageId: ERROR_NODE_CANT_HOST_RESOURCE
//
// MessageText:
//
//  The cluster resource cannot be brought online. The owner node cannot run this resource.
//
  ERROR_NODE_CANT_HOST_RESOURCE = DWORD(5071);

//
// MessageId: ERROR_CLUSTER_NODE_NOT_READY
//
// MessageText:
//
//  The cluster node is not ready to perform the requested operation.
//
  ERROR_CLUSTER_NODE_NOT_READY = DWORD(5072);

//
// MessageId: ERROR_CLUSTER_NODE_SHUTTING_DOWN
//
// MessageText:
//
//  The cluster node is shutting down.
//
  ERROR_CLUSTER_NODE_SHUTTING_DOWN = DWORD(5073);

//
// MessageId: ERROR_CLUSTER_JOIN_ABORTED
//
// MessageText:
//
//  The cluster join operation was aborted.
//
  ERROR_CLUSTER_JOIN_ABORTED = DWORD(5074);

//
// MessageId: ERROR_CLUSTER_INCOMPATIBLE_VERSIONS
//
// MessageText:
//
//  The cluster join operation failed due to incompatible software versions between the joining node and its sponsor.
//
  ERROR_CLUSTER_INCOMPATIBLE_VERSIONS = DWORD(5075);

//
// MessageId: ERROR_CLUSTER_MAXNUM_OF_RESOURCES_EXCEEDED
//
// MessageText:
//
//  This resource cannot be created because the cluster has reached the limit on the number of resources it can monitor.
//
  ERROR_CLUSTER_MAXNUM_OF_RESOURCES_EXCEEDED = DWORD(5076);

//
// MessageId: ERROR_CLUSTER_SYSTEM_CONFIG_CHANGED
//
// MessageText:
//
//  The system configuration changed during the cluster join or form operation. The join or form operation was aborted.
//
  ERROR_CLUSTER_SYSTEM_CONFIG_CHANGED = DWORD(5077);

//
// MessageId: ERROR_CLUSTER_RESOURCE_TYPE_NOT_FOUND
//
// MessageText:
//
//  The specified resource type was not found.
//
  ERROR_CLUSTER_RESOURCE_TYPE_NOT_FOUND = DWORD(5078);

//
// MessageId: ERROR_CLUSTER_RESTYPE_NOT_SUPPORTED
//
// MessageText:
//
//  The specified node does not support a resource of this type.  This may be due to version inconsistencies or due to the absence of the resource DLL on this node.
//
  ERROR_CLUSTER_RESTYPE_NOT_SUPPORTED = DWORD(5079);

//
// MessageId: ERROR_CLUSTER_RESNAME_NOT_FOUND
//
// MessageText:
//
//  The specified resource name is not supported by this resource DLL. This may be due to a bad (or changed) name supplied to the resource DLL.
//
  ERROR_CLUSTER_RESNAME_NOT_FOUND = DWORD(5080);

//
// MessageId: ERROR_CLUSTER_NO_RPC_PACKAGES_REGISTERED
//
// MessageText:
//
//  No authentication package could be registered with the RPC server.
//
  ERROR_CLUSTER_NO_RPC_PACKAGES_REGISTERED = DWORD(5081);

//
// MessageId: ERROR_CLUSTER_OWNER_NOT_IN_PREFLIST
//
// MessageText:
//
//  You cannot bring the group online because the owner of the group is not in the preferred list for the group. To change the owner node for the group, move the group.
//
  ERROR_CLUSTER_OWNER_NOT_IN_PREFLIST = DWORD(5082);

//
// MessageId: ERROR_CLUSTER_DATABASE_SEQMISMATCH
//
// MessageText:
//
//  The join operation failed because the cluster database sequence number has changed or is incompatible with the locker node. This may happen during a join operation if the cluster database was changing during the join.
//
  ERROR_CLUSTER_DATABASE_SEQMISMATCH = DWORD(5083);

//
// MessageId: ERROR_RESMON_INVALID_STATE
//
// MessageText:
//
//  The resource monitor will not allow the fail operation to be performed while the resource is in its current state. This may happen if the resource is in a pending state.
//
  ERROR_RESMON_INVALID_STATE = DWORD(5084);

//
// MessageId: ERROR_CLUSTER_GUM_NOT_LOCKER
//
// MessageText:
//
//  A non locker code got a request to reserve the lock for making global updates.
//
  ERROR_CLUSTER_GUM_NOT_LOCKER = DWORD(5085);

//
// MessageId: ERROR_QUORUM_DISK_NOT_FOUND
//
// MessageText:
//
//  The quorum disk could not be located by the cluster service.
//
  ERROR_QUORUM_DISK_NOT_FOUND = DWORD(5086);

//
// MessageId: ERROR_DATABASE_BACKUP_CORRUPT
//
// MessageText:
//
//  The backed up cluster database is possibly corrupt.
//
  ERROR_DATABASE_BACKUP_CORRUPT = DWORD(5087);

//
// MessageId: ERROR_CLUSTER_NODE_ALREADY_HAS_DFS_ROOT
//
// MessageText:
//
//  A DFS root already exists in this cluster node.
//
  ERROR_CLUSTER_NODE_ALREADY_HAS_DFS_ROOT = DWORD(5088);

//
// MessageId: ERROR_RESOURCE_PROPERTY_UNCHANGEABLE
//
// MessageText:
//
//  An attempt to modify a resource property failed because it conflicts with another existing property.
//
  ERROR_RESOURCE_PROPERTY_UNCHANGEABLE = DWORD(5089);

{
 Codes from 4300 through 5889 overlap with codes in ds\published\inc\apperr2.w.
 Do not add any more error codes in that range.
}

//
// MessageId: ERROR_CLUSTER_MEMBERSHIP_INVALID_STATE
//
// MessageText:
//
//  An operation was attempted that is incompatible with the current membership state of the node.
//
  ERROR_CLUSTER_MEMBERSHIP_INVALID_STATE = DWORD(5890);

//
// MessageId: ERROR_CLUSTER_QUORUMLOG_NOT_FOUND
//
// MessageText:
//
//  The quorum resource does not contain the quorum log.
//
  ERROR_CLUSTER_QUORUMLOG_NOT_FOUND = DWORD(5891);

//
// MessageId: ERROR_CLUSTER_MEMBERSHIP_HALT
//
// MessageText:
//
//  The membership engine requested shutdown of the cluster service on this node.
//
  ERROR_CLUSTER_MEMBERSHIP_HALT = DWORD(5892);

//
// MessageId: ERROR_CLUSTER_INSTANCE_ID_MISMATCH
//
// MessageText:
//
//  The join operation failed because the cluster instance ID of the joining node does not match the cluster instance ID of the sponsor node.
//
  ERROR_CLUSTER_INSTANCE_ID_MISMATCH = DWORD(5893);

//
// MessageId: ERROR_CLUSTER_NETWORK_NOT_FOUND_FOR_IP
//
// MessageText:
//
//  A matching network for the specified IP address could not be found. Please also specify a subnet mask and a cluster network.
//
  ERROR_CLUSTER_NETWORK_NOT_FOUND_FOR_IP = DWORD(5894);

//
// MessageId: ERROR_CLUSTER_PROPERTY_DATA_TYPE_MISMATCH
//
// MessageText:
//
//  The actual data type of the property did not match the expected data type of the property.
//
  ERROR_CLUSTER_PROPERTY_DATA_TYPE_MISMATCH = DWORD(5895);

//
// MessageId: ERROR_CLUSTER_EVICT_WITHOUT_CLEANUP
//
// MessageText:
//
//  The cluster node was evicted from the cluster successfully, but the node was not cleaned up.  Extended status information explaining why the node was not cleaned up is available.
//
  ERROR_CLUSTER_EVICT_WITHOUT_CLEANUP = DWORD(5896);

//
// MessageId: ERROR_CLUSTER_PARAMETER_MISMATCH
//
// MessageText:
//
//  Two or more parameter values specified for a resource's properties are in conflict.
//
  ERROR_CLUSTER_PARAMETER_MISMATCH = DWORD(5897);

//
// MessageId: ERROR_NODE_CANNOT_BE_CLUSTERED
//
// MessageText:
//
//  This computer cannot be made a member of a cluster.
//
  ERROR_NODE_CANNOT_BE_CLUSTERED = DWORD(5898);

//
// MessageId: ERROR_CLUSTER_WRONG_OS_VERSION
//
// MessageText:
//
//  This computer cannot be made a member of a cluster because it does not have the correct version of Windows installed.
//
  ERROR_CLUSTER_WRONG_OS_VERSION = DWORD(5899);

//
// MessageId: ERROR_CLUSTER_CANT_CREATE_DUP_CLUSTER_NAME
//
// MessageText:
//
//  A cluster cannot be created with the specified cluster name because that cluster name is already in use. Specify a different name for the cluster.
//
  ERROR_CLUSTER_CANT_CREATE_DUP_CLUSTER_NAME = DWORD(5900);

//
// MessageId: ERROR_CLUSCFG_ALREADY_COMMITTED
//
// MessageText:
//
//  The cluster configuration action has already been committed.
//
  ERROR_CLUSCFG_ALREADY_COMMITTED = DWORD(5901);

//
// MessageId: ERROR_CLUSCFG_ROLLBACK_FAILED
//
// MessageText:
//
//  The cluster configuration action could not be rolled back.
//
  ERROR_CLUSCFG_ROLLBACK_FAILED = DWORD(5902);

//
// MessageId: ERROR_CLUSCFG_SYSTEM_DISK_DRIVE_LETTER_CONFLICT
//
// MessageText:
//
//  The drive letter assigned to a system disk on one node conflicted with the drive letter assigned to a disk on another node.
//
  ERROR_CLUSCFG_SYSTEM_DISK_DRIVE_LETTER_CONFLICT = DWORD(5903);

//
// MessageId: ERROR_CLUSTER_OLD_VERSION
//
// MessageText:
//
//  One or more nodes in the cluster are running a version of Windows that does not support this operation.
//
  ERROR_CLUSTER_OLD_VERSION = DWORD(5904);

//
// MessageId: ERROR_CLUSTER_MISMATCHED_COMPUTER_ACCT_NAME
//
// MessageText:
//
//  The name of the corresponding computer account doesn't match the Network Name for this resource.
//
  ERROR_CLUSTER_MISMATCHED_COMPUTER_ACCT_NAME = DWORD(5905);

////////////////////////////////////
//                                //
//     EFS Error Codes            //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_ENCRYPTION_FAILED
//
// MessageText:
//
//  The specified file could not be encrypted.
//
  ERROR_ENCRYPTION_FAILED = DWORD(6000);

//
// MessageId: ERROR_DECRYPTION_FAILED
//
// MessageText:
//
//  The specified file could not be decrypted.
//
  ERROR_DECRYPTION_FAILED = DWORD(6001);

//
// MessageId: ERROR_FILE_ENCRYPTED
//
// MessageText:
//
//  The specified file is encrypted and the user does not have the ability to decrypt it.
//
  ERROR_FILE_ENCRYPTED = DWORD(6002);

//
// MessageId: ERROR_NO_RECOVERY_POLICY
//
// MessageText:
//
//  There is no valid encryption recovery policy configured for this system.
//
  ERROR_NO_RECOVERY_POLICY = DWORD(6003);

//
// MessageId: ERROR_NO_EFS
//
// MessageText:
//
//  The required encryption driver is not loaded for this system.
//
  ERROR_NO_EFS = DWORD(6004);

//
// MessageId: ERROR_WRONG_EFS
//
// MessageText:
//
//  The file was encrypted with a different encryption driver than is currently loaded.
//
  ERROR_WRONG_EFS = DWORD(6005);

//
// MessageId: ERROR_NO_USER_KEYS
//
// MessageText:
//
//  There are no EFS keys defined for the user.
//
  ERROR_NO_USER_KEYS = DWORD(6006);

//
// MessageId: ERROR_FILE_NOT_ENCRYPTED
//
// MessageText:
//
//  The specified file is not encrypted.
//
  ERROR_FILE_NOT_ENCRYPTED = DWORD(6007);

//
// MessageId: ERROR_NOT_EXPORT_FORMAT
//
// MessageText:
//
//  The specified file is not in the defined EFS export format.
//
  ERROR_NOT_EXPORT_FORMAT = DWORD(6008);

//
// MessageId: ERROR_FILE_READ_ONLY
//
// MessageText:
//
//  The specified file is read only.
//
  ERROR_FILE_READ_ONLY = DWORD(6009);

//
// MessageId: ERROR_DIR_EFS_DISALLOWED
//
// MessageText:
//
//  The directory has been disabled for encryption.
//
  ERROR_DIR_EFS_DISALLOWED = DWORD(6010);

//
// MessageId: ERROR_EFS_SERVER_NOT_TRUSTED
//
// MessageText:
//
//  The server is not trusted for remote encryption operation.
//
  ERROR_EFS_SERVER_NOT_TRUSTED = DWORD(6011);

//
// MessageId: ERROR_BAD_RECOVERY_POLICY
//
// MessageText:
//
//  Recovery policy configured for this system contains invalid recovery certificate.
//
  ERROR_BAD_RECOVERY_POLICY = DWORD(6012);

//
// MessageId: ERROR_EFS_ALG_BLOB_TOO_BIG
//
// MessageText:
//
//  The encryption algorithm used on the source file needs a bigger key buffer than the one on the destination file.
//
  ERROR_EFS_ALG_BLOB_TOO_BIG = DWORD(6013);

//
// MessageId: ERROR_VOLUME_NOT_SUPPORT_EFS
//
// MessageText:
//
//  The disk partition does not support file encryption.
//
  ERROR_VOLUME_NOT_SUPPORT_EFS = DWORD(6014);

//
// MessageId: ERROR_EFS_DISABLED
//
// MessageText:
//
//  This machine is disabled for file encryption.
//
  ERROR_EFS_DISABLED = DWORD(6015);

//
// MessageId: ERROR_EFS_VERSION_NOT_SUPPORT
//
// MessageText:
//
//  A newer system is required to decrypt this encrypted file.
//
  ERROR_EFS_VERSION_NOT_SUPPORT = DWORD(6016);

// This message number is for historical purposes and cannot be changed or re-used.
//
// MessageId: ERROR_NO_BROWSER_SERVERS_FOUND
//
// MessageText:
//
//  The list of servers for this workgroup is not currently available
//
  ERROR_NO_BROWSER_SERVERS_FOUND = DWORD(6118);

//////////////////////////////////////////////////////////////////
//                                                              //
// Task Scheduler Error Codes that NET START must understand    //
//                                                              //
//////////////////////////////////////////////////////////////////
//
// MessageId: SCHED_E_SERVICE_NOT_LOCALSYSTEM
//
// MessageText:
//
//  The Task Scheduler service must be configured to run in the System account to function properly.  Individual tasks may be configured to run in other accounts.
//
  SCHED_E_SERVICE_NOT_LOCALSYSTEM = DWORD(6200);

////////////////////////////////////
//                                //
// Terminal Server Error Codes    //
//                                //
////////////////////////////////////
//
// MessageId: ERROR_CTX_WINSTATION_NAME_INVALID
//
// MessageText:
//
//  The specified session name is invalid.
//
  ERROR_CTX_WINSTATION_NAME_INVALID = DWORD(7001);

//
// MessageId: ERROR_CTX_INVALID_PD
//
// MessageText:
//
//  The specified protocol driver is invalid.
//
  ERROR_CTX_INVALID_PD = DWORD(7002);

//
// MessageId: ERROR_CTX_PD_NOT_FOUND
//
// MessageText:
//
//  The specified protocol driver was not found in the system path.
//
  ERROR_CTX_PD_NOT_FOUND = DWORD(7003);

//
// MessageId: ERROR_CTX_WD_NOT_FOUND
//
// MessageText:
//
//  The specified terminal connection driver was not found in the system path.
//
  ERROR_CTX_WD_NOT_FOUND = DWORD(7004);

//
// MessageId: ERROR_CTX_CANNOT_MAKE_EVENTLOG_ENTRY
//
// MessageText:
//
//  A registry key for event logging could not be created for this session.
//
  ERROR_CTX_CANNOT_MAKE_EVENTLOG_ENTRY = DWORD(7005);

//
// MessageId: ERROR_CTX_SERVICE_NAME_COLLISION
//
// MessageText:
//
//  A service with the same name already exists on the system.
//
  ERROR_CTX_SERVICE_NAME_COLLISION = DWORD(7006);

//
// MessageId: ERROR_CTX_CLOSE_PENDING
//
// MessageText:
//
//  A close operation is pending on the session.
//
  ERROR_CTX_CLOSE_PENDING = DWORD(7007);

//
// MessageId: ERROR_CTX_NO_OUTBUF
//
// MessageText:
//
//  There are no free output buffers available.
//
  ERROR_CTX_NO_OUTBUF = DWORD(7008);

//
// MessageId: ERROR_CTX_MODEM_INF_NOT_FOUND
//
// MessageText:
//
//  The MODEM.INF file was not found.
//
  ERROR_CTX_MODEM_INF_NOT_FOUND = DWORD(7009);

//
// MessageId: ERROR_CTX_INVALID_MODEMNAME
//
// MessageText:
//
//  The modem name was not found in MODEM.INF.
//
  ERROR_CTX_INVALID_MODEMNAME = DWORD(7010);

//
// MessageId: ERROR_CTX_MODEM_RESPONSE_ERROR
//
// MessageText:
//
//  The modem did not accept the command sent to it. Verify that the configured modem name matches the attached modem.
//
  ERROR_CTX_MODEM_RESPONSE_ERROR = DWORD(7011);

//
// MessageId: ERROR_CTX_MODEM_RESPONSE_TIMEOUT
//
// MessageText:
//
//  The modem did not respond to the command sent to it. Verify that the modem is properly cabled and powered on.
//
  ERROR_CTX_MODEM_RESPONSE_TIMEOUT = DWORD(7012);

//
// MessageId: ERROR_CTX_MODEM_RESPONSE_NO_CARRIER
//
// MessageText:
//
//  Carrier detect has failed or carrier has been dropped due to disconnect.
//
  ERROR_CTX_MODEM_RESPONSE_NO_CARRIER = DWORD(7013);

//
// MessageId: ERROR_CTX_MODEM_RESPONSE_NO_DIALTONE
//
// MessageText:
//
//  Dial tone not detected within the required time. Verify that the phone cable is properly attached and functional.
//
  ERROR_CTX_MODEM_RESPONSE_NO_DIALTONE = DWORD(7014);

//
// MessageId: ERROR_CTX_MODEM_RESPONSE_BUSY
//
// MessageText:
//
//  Busy signal detected at remote site on callback.
//
  ERROR_CTX_MODEM_RESPONSE_BUSY = DWORD(7015);

//
// MessageId: ERROR_CTX_MODEM_RESPONSE_VOICE
//
// MessageText:
//
//  Voice detected at remote site on callback.
//
  ERROR_CTX_MODEM_RESPONSE_VOICE = DWORD(7016);

//
// MessageId: ERROR_CTX_TD_ERROR
//
// MessageText:
//
//  Transport driver error
//
  ERROR_CTX_TD_ERROR = DWORD(7017);

//
// MessageId: ERROR_CTX_WINSTATION_NOT_FOUND
//
// MessageText:
//
//  The specified session cannot be found.
//
  ERROR_CTX_WINSTATION_NOT_FOUND = DWORD(7022);

//
// MessageId: ERROR_CTX_WINSTATION_ALREADY_EXISTS
//
// MessageText:
//
//  The specified session name is already in use.
//
  ERROR_CTX_WINSTATION_ALREADY_EXISTS = DWORD(7023);

//
// MessageId: ERROR_CTX_WINSTATION_BUSY
//
// MessageText:
//
//  The requested operation cannot be completed because the terminal connection is currently busy processing a connect, disconnect, reset, or delete operation.
//
  ERROR_CTX_WINSTATION_BUSY = DWORD(7024);

//
// MessageId: ERROR_CTX_BAD_VIDEO_MODE
//
// MessageText:
//
//  An attempt has been made to connect to a session whose video mode is not supported by the current client.
//
  ERROR_CTX_BAD_VIDEO_MODE = DWORD(7025);

//
// MessageId: ERROR_CTX_GRAPHICS_INVALID
//
// MessageText:
//
//  The application attempted to enable DOS graphics mode.
//  DOS graphics mode is not supported.
//
  ERROR_CTX_GRAPHICS_INVALID = DWORD(7035);

//
// MessageId: ERROR_CTX_LOGON_DISABLED
//
// MessageText:
//
//  Your interactive logon privilege has been disabled.
//  Please contact your administrator.
//
  ERROR_CTX_LOGON_DISABLED = DWORD(7037);

//
// MessageId: ERROR_CTX_NOT_CONSOLE
//
// MessageText:
//
//  The requested operation can be performed only on the system console.
//  This is most often the result of a driver or system DLL requiring direct console access.
//
  ERROR_CTX_NOT_CONSOLE = DWORD(7038);

//
// MessageId: ERROR_CTX_CLIENT_QUERY_TIMEOUT
//
// MessageText:
//
//  The client failed to respond to the server connect message.
//
  ERROR_CTX_CLIENT_QUERY_TIMEOUT = DWORD(7040);

//
// MessageId: ERROR_CTX_CONSOLE_DISCONNECT
//
// MessageText:
//
//  Disconnecting the console session is not supported.
//
  ERROR_CTX_CONSOLE_DISCONNECT = DWORD(7041);

//
// MessageId: ERROR_CTX_CONSOLE_CONNECT
//
// MessageText:
//
//  Reconnecting a disconnected session to the console is not supported.
//
  ERROR_CTX_CONSOLE_CONNECT = DWORD(7042);

//
// MessageId: ERROR_CTX_SHADOW_DENIED
//
// MessageText:
//
//  The request to control another session remotely was denied.
//
  ERROR_CTX_SHADOW_DENIED = DWORD(7044);

//
// MessageId: ERROR_CTX_WINSTATION_ACCESS_DENIED
//
// MessageText:
//
//  The requested session access is denied.
//
  ERROR_CTX_WINSTATION_ACCESS_DENIED = DWORD(7045);

//
// MessageId: ERROR_CTX_INVALID_WD
//
// MessageText:
//
//  The specified terminal connection driver is invalid.
//
  ERROR_CTX_INVALID_WD = DWORD(7049);

//
// MessageId: ERROR_CTX_SHADOW_INVALID
//
// MessageText:
//
//  The requested session cannot be controlled remotely.
//  This may be because the session is disconnected or does not currently have a user logged on.
//
  ERROR_CTX_SHADOW_INVALID = DWORD(7050);

//
// MessageId: ERROR_CTX_SHADOW_DISABLED
//
// MessageText:
//
//  The requested session is not configured to allow remote control.
//
  ERROR_CTX_SHADOW_DISABLED = DWORD(7051);

//
// MessageId: ERROR_CTX_CLIENT_LICENSE_IN_USE
//
// MessageText:
//
//  Your request to connect to this Terminal Server has been rejected. Your Terminal Server client license number is currently being used by another user.
//  Please call your system administrator to obtain a unique license number.
//
  ERROR_CTX_CLIENT_LICENSE_IN_USE = DWORD(7052);

//
// MessageId: ERROR_CTX_CLIENT_LICENSE_NOT_SET
//
// MessageText:
//
//  Your request to connect to this Terminal Server has been rejected. Your Terminal Server client license number has not been entered for this copy of the Terminal Server client.
//  Please contact your system administrator.
//
  ERROR_CTX_CLIENT_LICENSE_NOT_SET = DWORD(7053);

//
// MessageId: ERROR_CTX_LICENSE_NOT_AVAILABLE
//
// MessageText:
//
//  The system has reached its licensed logon limit.
//  Please try again later.
//
  ERROR_CTX_LICENSE_NOT_AVAILABLE = DWORD(7054);

//
// MessageId: ERROR_CTX_LICENSE_CLIENT_INVALID
//
// MessageText:
//
//  The client you are using is not licensed to use this system.  Your logon request is denied.
//
  ERROR_CTX_LICENSE_CLIENT_INVALID = DWORD(7055);

//
// MessageId: ERROR_CTX_LICENSE_EXPIRED
//
// MessageText:
//
//  The system license has expired.  Your logon request is denied.
//
  ERROR_CTX_LICENSE_EXPIRED = DWORD(7056);

//
// MessageId: ERROR_CTX_SHADOW_NOT_RUNNING
//
// MessageText:
//
//  Remote control could not be terminated because the specified session is not currently being remotely controlled.
//
  ERROR_CTX_SHADOW_NOT_RUNNING = DWORD(7057);

//
// MessageId: ERROR_CTX_SHADOW_ENDED_BY_MODE_CHANGE
//
// MessageText:
//
//  The remote control of the console was terminated because the display mode was changed. Changing the display mode in a remote control session is not supported.
//
  ERROR_CTX_SHADOW_ENDED_BY_MODE_CHANGE = DWORD(7058);

//
// MessageId: ERROR_ACTIVATION_COUNT_EXCEEDED
//
// MessageText:
//
//  Activation has already been reset the maximum number of times for this installation. Your activation timer will not be cleared.
//
  ERROR_ACTIVATION_COUNT_EXCEEDED = DWORD(7059);

///////////////////////////////////////////////////
//                                                /
//             Traffic Control Error Codes        /
//                                                /
//                  7500 to  7999                 /
//                                                /
//         defined in: tcerror.h                  /
///////////////////////////////////////////////////
///////////////////////////////////////////////////
//                                                /
//             Active Directory Error Codes       /
//                                                /
//                  8000 to  8999                 /
///////////////////////////////////////////////////
// *****************
// FACILITY_FILE_REPLICATION_SERVICE
// *****************
//
// MessageId: FRS_ERR_INVALID_API_SEQUENCE
//
// MessageText:
//
//  The file replication service API was called incorrectly.
//
  FRS_ERR_INVALID_API_SEQUENCE = DWORD(8001);

//
// MessageId: FRS_ERR_STARTING_SERVICE
//
// MessageText:
//
//  The file replication service cannot be started.
//
  FRS_ERR_STARTING_SERVICE = DWORD(8002);

//
// MessageId: FRS_ERR_STOPPING_SERVICE
//
// MessageText:
//
//  The file replication service cannot be stopped.
//
  FRS_ERR_STOPPING_SERVICE = DWORD(8003);

//
// MessageId: FRS_ERR_INTERNAL_API
//
// MessageText:
//
//  The file replication service API terminated the request.
//  The event log may have more information.
//
  FRS_ERR_INTERNAL_API = DWORD(8004);

//
// MessageId: FRS_ERR_INTERNAL
//
// MessageText:
//
//  The file replication service terminated the request.
//  The event log may have more information.
//
  FRS_ERR_INTERNAL = DWORD(8005);

//
// MessageId: FRS_ERR_SERVICE_COMM
//
// MessageText:
//
//  The file replication service cannot be contacted.
//  The event log may have more information.
//
  FRS_ERR_SERVICE_COMM = DWORD(8006);

//
// MessageId: FRS_ERR_INSUFFICIENT_PRIV
//
// MessageText:
//
//  The file replication service cannot satisfy the request because the user has insufficient privileges.
//  The event log may have more information.
//
  FRS_ERR_INSUFFICIENT_PRIV = DWORD(8007);

//
// MessageId: FRS_ERR_AUTHENTICATION
//
// MessageText:
//
//  The file replication service cannot satisfy the request because authenticated RPC is not available.
//  The event log may have more information.
//
  FRS_ERR_AUTHENTICATION = DWORD(8008);

//
// MessageId: FRS_ERR_PARENT_INSUFFICIENT_PRIV
//
// MessageText:
//
//  The file replication service cannot satisfy the request because the user has insufficient privileges on the domain controller.
//  The event log may have more information.
//
  FRS_ERR_PARENT_INSUFFICIENT_PRIV = DWORD(8009);

//
// MessageId: FRS_ERR_PARENT_AUTHENTICATION
//
// MessageText:
//
//  The file replication service cannot satisfy the request because authenticated RPC is not available on the domain controller.
//  The event log may have more information.
//
  FRS_ERR_PARENT_AUTHENTICATION = DWORD(8010);

//
// MessageId: FRS_ERR_CHILD_TO_PARENT_COMM
//
// MessageText:
//
//  The file replication service cannot communicate with the file replication service on the domain controller.
//  The event log may have more information.
//
  FRS_ERR_CHILD_TO_PARENT_COMM = DWORD(8011);

//
// MessageId: FRS_ERR_PARENT_TO_CHILD_COMM
//
// MessageText:
//
//  The file replication service on the domain controller cannot communicate with the file replication service on this computer.
//  The event log may have more information.
//
  FRS_ERR_PARENT_TO_CHILD_COMM = DWORD(8012);

//
// MessageId: FRS_ERR_SYSVOL_POPULATE
//
// MessageText:
//
//  The file replication service cannot populate the system volume because of an internal error.
//  The event log may have more information.
//
  FRS_ERR_SYSVOL_POPULATE = DWORD(8013);

//
// MessageId: FRS_ERR_SYSVOL_POPULATE_TIMEOUT
//
// MessageText:
//
//  The file replication service cannot populate the system volume because of an internal timeout.
//  The event log may have more information.
//
  FRS_ERR_SYSVOL_POPULATE_TIMEOUT = DWORD(8014);

//
// MessageId: FRS_ERR_SYSVOL_IS_BUSY
//
// MessageText:
//
//  The file replication service cannot process the request. The system volume is busy with a previous request.
//
  FRS_ERR_SYSVOL_IS_BUSY = DWORD(8015);

//
// MessageId: FRS_ERR_SYSVOL_DEMOTE
//
// MessageText:
//
//  The file replication service cannot stop replicating the system volume because of an internal error.
//  The event log may have more information.
//
  FRS_ERR_SYSVOL_DEMOTE = DWORD(8016);

//
// MessageId: FRS_ERR_INVALID_SERVICE_PARAMETER
//
// MessageText:
//
//  The file replication service detected an invalid parameter.
//
  FRS_ERR_INVALID_SERVICE_PARAMETER = DWORD(8017);

// *****************
// FACILITY DIRECTORY SERVICE
// *****************
  DS_S_SUCCESS = NO_ERROR;
//
// MessageId: ERROR_DS_NOT_INSTALLED
//
// MessageText:
//
//  An error occurred while installing the directory service. For more information, see the event log.
//
  ERROR_DS_NOT_INSTALLED = DWORD(8200);

//
// MessageId: ERROR_DS_MEMBERSHIP_EVALUATED_LOCALLY
//
// MessageText:
//
//  The directory service evaluated group memberships locally.
//
  ERROR_DS_MEMBERSHIP_EVALUATED_LOCALLY = DWORD(8201);

//
// MessageId: ERROR_DS_NO_ATTRIBUTE_OR_VALUE
//
// MessageText:
//
//  The specified directory service attribute or value does not exist.
//
  ERROR_DS_NO_ATTRIBUTE_OR_VALUE = DWORD(8202);

//
// MessageId: ERROR_DS_INVALID_ATTRIBUTE_SYNTAX
//
// MessageText:
//
//  The attribute syntax specified to the directory service is invalid.
//
  ERROR_DS_INVALID_ATTRIBUTE_SYNTAX = DWORD(8203);

//
// MessageId: ERROR_DS_ATTRIBUTE_TYPE_UNDEFINED
//
// MessageText:
//
//  The attribute type specified to the directory service is not defined.
//
  ERROR_DS_ATTRIBUTE_TYPE_UNDEFINED = DWORD(8204);

//
// MessageId: ERROR_DS_ATTRIBUTE_OR_VALUE_EXISTS
//
// MessageText:
//
//  The specified directory service attribute or value already exists.
//
  ERROR_DS_ATTRIBUTE_OR_VALUE_EXISTS = DWORD(8205);

//
// MessageId: ERROR_DS_BUSY
//
// MessageText:
//
//  The directory service is busy.
//
  ERROR_DS_BUSY = DWORD(8206);

//
// MessageId: ERROR_DS_UNAVAILABLE
//
// MessageText:
//
//  The directory service is unavailable.
//
  ERROR_DS_UNAVAILABLE = DWORD(8207);

//
// MessageId: ERROR_DS_NO_RIDS_ALLOCATED
//
// MessageText:
//
//  The directory service was unable to allocate a relative identifier.
//
  ERROR_DS_NO_RIDS_ALLOCATED = DWORD(8208);

//
// MessageId: ERROR_DS_NO_MORE_RIDS
//
// MessageText:
//
//  The directory service has exhausted the pool of relative identifiers.
//
  ERROR_DS_NO_MORE_RIDS = DWORD(8209);

//
// MessageId: ERROR_DS_INCORRECT_ROLE_OWNER
//
// MessageText:
//
//  The requested operation could not be performed because the directory service is not the master for that type of operation.
//
  ERROR_DS_INCORRECT_ROLE_OWNER = DWORD(8210);

//
// MessageId: ERROR_DS_RIDMGR_INIT_ERROR
//
// MessageText:
//
//  The directory service was unable to initialize the subsystem that allocates relative identifiers.
//
  ERROR_DS_RIDMGR_INIT_ERROR = DWORD(8211);

//
// MessageId: ERROR_DS_OBJ_CLASS_VIOLATION
//
// MessageText:
//
//  The requested operation did not satisfy one or more constraints associated with the class of the object.
//
  ERROR_DS_OBJ_CLASS_VIOLATION = DWORD(8212);

//
// MessageId: ERROR_DS_CANT_ON_NON_LEAF
//
// MessageText:
//
//  The directory service can perform the requested operation only on a leaf object.
//
  ERROR_DS_CANT_ON_NON_LEAF = DWORD(8213);

//
// MessageId: ERROR_DS_CANT_ON_RDN
//
// MessageText:
//
//  The directory service cannot perform the requested operation on the RDN attribute of an object.
//
  ERROR_DS_CANT_ON_RDN = DWORD(8214);

//
// MessageId: ERROR_DS_CANT_MOD_OBJ_CLASS
//
// MessageText:
//
//  The directory service detected an attempt to modify the object class of an object.
//
  ERROR_DS_CANT_MOD_OBJ_CLASS = DWORD(8215);

//
// MessageId: ERROR_DS_CROSS_DOM_MOVE_ERROR
//
// MessageText:
//
//  The requested cross-domain move operation could not be performed.
//
  ERROR_DS_CROSS_DOM_MOVE_ERROR = DWORD(8216);

//
// MessageId: ERROR_DS_GC_NOT_AVAILABLE
//
// MessageText:
//
//  Unable to contact the global catalog server.
//
  ERROR_DS_GC_NOT_AVAILABLE = DWORD(8217);

//
// MessageId: ERROR_SHARED_POLICY
//
// MessageText:
//
//  The policy object is shared and can only be modified at the root.
//
  ERROR_SHARED_POLICY = DWORD(8218);

//
// MessageId: ERROR_POLICY_OBJECT_NOT_FOUND
//
// MessageText:
//
//  The policy object does not exist.
//
  ERROR_POLICY_OBJECT_NOT_FOUND = DWORD(8219);

//
// MessageId: ERROR_POLICY_ONLY_IN_DS
//
// MessageText:
//
//  The requested policy information is only in the directory service.
//
  ERROR_POLICY_ONLY_IN_DS = DWORD(8220);

//
// MessageId: ERROR_PROMOTION_ACTIVE
//
// MessageText:
//
//  A domain controller promotion is currently active.
//
  ERROR_PROMOTION_ACTIVE = DWORD(8221);

//
// MessageId: ERROR_NO_PROMOTION_ACTIVE
//
// MessageText:
//
//  A domain controller promotion is not currently active
//
  ERROR_NO_PROMOTION_ACTIVE = DWORD(8222);

// 8223 unused
//
// MessageId: ERROR_DS_OPERATIONS_ERROR
//
// MessageText:
//
//  An operations error occurred.
//
  ERROR_DS_OPERATIONS_ERROR = DWORD(8224);

//
// MessageId: ERROR_DS_PROTOCOL_ERROR
//
// MessageText:
//
//  A protocol error occurred.
//
  ERROR_DS_PROTOCOL_ERROR = DWORD(8225);

//
// MessageId: ERROR_DS_TIMELIMIT_EXCEEDED
//
// MessageText:
//
//  The time limit for this request was exceeded.
//
  ERROR_DS_TIMELIMIT_EXCEEDED = DWORD(8226);

//
// MessageId: ERROR_DS_SIZELIMIT_EXCEEDED
//
// MessageText:
//
//  The size limit for this request was exceeded.
//
  ERROR_DS_SIZELIMIT_EXCEEDED = DWORD(8227);

//
// MessageId: ERROR_DS_ADMIN_LIMIT_EXCEEDED
//
// MessageText:
//
//  The administrative limit for this request was exceeded.
//
  ERROR_DS_ADMIN_LIMIT_EXCEEDED = DWORD(8228);

//
// MessageId: ERROR_DS_COMPARE_FALSE
//
// MessageText:
//
//  The compare response was false.
//
  ERROR_DS_COMPARE_FALSE = DWORD(8229);

//
// MessageId: ERROR_DS_COMPARE_TRUE
//
// MessageText:
//
//  The compare response was true.
//
  ERROR_DS_COMPARE_TRUE = DWORD(8230);

//
// MessageId: ERROR_DS_AUTH_METHOD_NOT_SUPPORTED
//
// MessageText:
//
//  The requested authentication method is not supported by the server.
//
  ERROR_DS_AUTH_METHOD_NOT_SUPPORTED = DWORD(8231);

//
// MessageId: ERROR_DS_STRONG_AUTH_REQUIRED
//
// MessageText:
//
//  A more secure authentication method is required for this server.
//
  ERROR_DS_STRONG_AUTH_REQUIRED = DWORD(8232);

//
// MessageId: ERROR_DS_INAPPROPRIATE_AUTH
//
// MessageText:
//
//  Inappropriate authentication.
//
  ERROR_DS_INAPPROPRIATE_AUTH = DWORD(8233);

//
// MessageId: ERROR_DS_AUTH_UNKNOWN
//
// MessageText:
//
//  The authentication mechanism is unknown.
//
  ERROR_DS_AUTH_UNKNOWN = DWORD(8234);

//
// MessageId: ERROR_DS_REFERRAL
//
// MessageText:
//
//  A referral was returned from the server.
//
  ERROR_DS_REFERRAL = DWORD(8235);

//
// MessageId: ERROR_DS_UNAVAILABLE_CRIT_EXTENSION
//
// MessageText:
//
//  The server does not support the requested critical extension.
//
  ERROR_DS_UNAVAILABLE_CRIT_EXTENSION = DWORD(8236);

//
// MessageId: ERROR_DS_CONFIDENTIALITY_REQUIRED
//
// MessageText:
//
//  This request requires a secure connection.
//
  ERROR_DS_CONFIDENTIALITY_REQUIRED = DWORD(8237);

//
// MessageId: ERROR_DS_INAPPROPRIATE_MATCHING
//
// MessageText:
//
//  Inappropriate matching.
//
  ERROR_DS_INAPPROPRIATE_MATCHING = DWORD(8238);

//
// MessageId: ERROR_DS_CONSTRAINT_VIOLATION
//
// MessageText:
//
//  A constraint violation occurred.
//
  ERROR_DS_CONSTRAINT_VIOLATION = DWORD(8239);

//
// MessageId: ERROR_DS_NO_SUCH_OBJECT
//
// MessageText:
//
//  There is no such object on the server.
//
  ERROR_DS_NO_SUCH_OBJECT = DWORD(8240);

//
// MessageId: ERROR_DS_ALIAS_PROBLEM
//
// MessageText:
//
//  There is an alias problem.
//
  ERROR_DS_ALIAS_PROBLEM = DWORD(8241);

//
// MessageId: ERROR_DS_INVALID_DN_SYNTAX
//
// MessageText:
//
//  An invalid dn syntax has been specified.
//
  ERROR_DS_INVALID_DN_SYNTAX = DWORD(8242);

//
// MessageId: ERROR_DS_IS_LEAF
//
// MessageText:
//
//  The object is a leaf object.
//
  ERROR_DS_IS_LEAF = DWORD(8243);

//
// MessageId: ERROR_DS_ALIAS_DEREF_PROBLEM
//
// MessageText:
//
//  There is an alias dereferencing problem.
//
  ERROR_DS_ALIAS_DEREF_PROBLEM = DWORD(8244);

//
// MessageId: ERROR_DS_UNWILLING_TO_PERFORM
//
// MessageText:
//
//  The server is unwilling to process the request.
//
  ERROR_DS_UNWILLING_TO_PERFORM = DWORD(8245);

//
// MessageId: ERROR_DS_LOOP_DETECT
//
// MessageText:
//
//  A loop has been detected.
//
  ERROR_DS_LOOP_DETECT = DWORD(8246);

//
// MessageId: ERROR_DS_NAMING_VIOLATION
//
// MessageText:
//
//  There is a naming violation.
//
  ERROR_DS_NAMING_VIOLATION = DWORD(8247);

//
// MessageId: ERROR_DS_OBJECT_RESULTS_TOO_LARGE
//
// MessageText:
//
//  The result set is too large.
//
  ERROR_DS_OBJECT_RESULTS_TOO_LARGE = DWORD(8248);

//
// MessageId: ERROR_DS_AFFECTS_MULTIPLE_DSAS
//
// MessageText:
//
//  The operation affects multiple DSAs
//
  ERROR_DS_AFFECTS_MULTIPLE_DSAS = DWORD(8249);

//
// MessageId: ERROR_DS_SERVER_DOWN
//
// MessageText:
//
//  The server is not operational.
//
  ERROR_DS_SERVER_DOWN = DWORD(8250);

//
// MessageId: ERROR_DS_LOCAL_ERROR
//
// MessageText:
//
//  A local error has occurred.
//
  ERROR_DS_LOCAL_ERROR = DWORD(8251);

//
// MessageId: ERROR_DS_ENCODING_ERROR
//
// MessageText:
//
//  An encoding error has occurred.
//
  ERROR_DS_ENCODING_ERROR = DWORD(8252);

//
// MessageId: ERROR_DS_DECODING_ERROR
//
// MessageText:
//
//  A decoding error has occurred.
//
  ERROR_DS_DECODING_ERROR = DWORD(8253);

//
// MessageId: ERROR_DS_FILTER_UNKNOWN
//
// MessageText:
//
//  The search filter cannot be recognized.
//
  ERROR_DS_FILTER_UNKNOWN = DWORD(8254);

//
// MessageId: ERROR_DS_PARAM_ERROR
//
// MessageText:
//
//  One or more parameters are illegal.
//
  ERROR_DS_PARAM_ERROR = DWORD(8255);

//
// MessageId: ERROR_DS_NOT_SUPPORTED
//
// MessageText:
//
//  The specified method is not supported.
//
  ERROR_DS_NOT_SUPPORTED = DWORD(8256);

//
// MessageId: ERROR_DS_NO_RESULTS_RETURNED
//
// MessageText:
//
//  No results were returned.
//
  ERROR_DS_NO_RESULTS_RETURNED = DWORD(8257);

//
// MessageId: ERROR_DS_CONTROL_NOT_FOUND
//
// MessageText:
//
//  The specified control is not supported by the server.
//
  ERROR_DS_CONTROL_NOT_FOUND = DWORD(8258);

//
// MessageId: ERROR_DS_CLIENT_LOOP
//
// MessageText:
//
//  A referral loop was detected by the client.
//
  ERROR_DS_CLIENT_LOOP = DWORD(8259);

//
// MessageId: ERROR_DS_REFERRAL_LIMIT_EXCEEDED
//
// MessageText:
//
//  The preset referral limit was exceeded.
//
  ERROR_DS_REFERRAL_LIMIT_EXCEEDED = DWORD(8260);

//
// MessageId: ERROR_DS_SORT_CONTROL_MISSING
//
// MessageText:
//
//  The search requires a SORT control.
//
  ERROR_DS_SORT_CONTROL_MISSING = DWORD(8261);

//
// MessageId: ERROR_DS_OFFSET_RANGE_ERROR
//
// MessageText:
//
//  The search results exceed the offset range specified.
//
  ERROR_DS_OFFSET_RANGE_ERROR = DWORD(8262);

//
// MessageId: ERROR_DS_ROOT_MUST_BE_NC
//
// MessageText:
//
//  The root object must be the head of a naming context. The root object cannot have an instantiated parent.
//
  ERROR_DS_ROOT_MUST_BE_NC = DWORD(8301);

//
// MessageId: ERROR_DS_ADD_REPLICA_INHIBITED
//
// MessageText:
//
//  The add replica operation cannot be performed. The naming context must be writeable in order to create the replica.
//
  ERROR_DS_ADD_REPLICA_INHIBITED = DWORD(8302);

//
// MessageId: ERROR_DS_ATT_NOT_DEF_IN_SCHEMA
//
// MessageText:
//
//  A reference to an attribute that is not defined in the schema occurred.
//
  ERROR_DS_ATT_NOT_DEF_IN_SCHEMA = DWORD(8303);

//
// MessageId: ERROR_DS_MAX_OBJ_SIZE_EXCEEDED
//
// MessageText:
//
//  The maximum size of an object has been exceeded.
//
  ERROR_DS_MAX_OBJ_SIZE_EXCEEDED = DWORD(8304);

//
// MessageId: ERROR_DS_OBJ_STRING_NAME_EXISTS
//
// MessageText:
//
//  An attempt was made to add an object to the directory with a name that is already in use.
//
  ERROR_DS_OBJ_STRING_NAME_EXISTS = DWORD(8305);

//
// MessageId: ERROR_DS_NO_RDN_DEFINED_IN_SCHEMA
//
// MessageText:
//
//  An attempt was made to add an object of a class that does not have an RDN defined in the schema.
//
  ERROR_DS_NO_RDN_DEFINED_IN_SCHEMA = DWORD(8306);

//
// MessageId: ERROR_DS_RDN_DOESNT_MATCH_SCHEMA
//
// MessageText:
//
//  An attempt was made to add an object using an RDN that is not the RDN defined in the schema.
//
  ERROR_DS_RDN_DOESNT_MATCH_SCHEMA = DWORD(8307);

//
// MessageId: ERROR_DS_NO_REQUESTED_ATTS_FOUND
//
// MessageText:
//
//  None of the requested attributes were found on the objects.
//
  ERROR_DS_NO_REQUESTED_ATTS_FOUND = DWORD(8308);

//
// MessageId: ERROR_DS_USER_BUFFER_TO_SMALL
//
// MessageText:
//
//  The user buffer is too small.
//
  ERROR_DS_USER_BUFFER_TO_SMALL = DWORD(8309);

//
// MessageId: ERROR_DS_ATT_IS_NOT_ON_OBJ
//
// MessageText:
//
//  The attribute specified in the operation is not present on the object.
//
  ERROR_DS_ATT_IS_NOT_ON_OBJ = DWORD(8310);

//
// MessageId: ERROR_DS_ILLEGAL_MOD_OPERATION
//
// MessageText:
//
//  Illegal modify operation. Some aspect of the modification is not permitted.
//
  ERROR_DS_ILLEGAL_MOD_OPERATION = DWORD(8311);

//
// MessageId: ERROR_DS_OBJ_TOO_LARGE
//
// MessageText:
//
//  The specified object is too large.
//
  ERROR_DS_OBJ_TOO_LARGE = DWORD(8312);

//
// MessageId: ERROR_DS_BAD_INSTANCE_TYPE
//
// MessageText:
//
//  The specified instance type is not valid.
//
  ERROR_DS_BAD_INSTANCE_TYPE = DWORD(8313);

//
// MessageId: ERROR_DS_MASTERDSA_REQUIRED
//
// MessageText:
//
//  The operation must be performed at a master DSA.
//
  ERROR_DS_MASTERDSA_REQUIRED = DWORD(8314);

//
// MessageId: ERROR_DS_OBJECT_CLASS_REQUIRED
//
// MessageText:
//
//  The object class attribute must be specified.
//
  ERROR_DS_OBJECT_CLASS_REQUIRED = DWORD(8315);

//
// MessageId: ERROR_DS_MISSING_REQUIRED_ATT
//
// MessageText:
//
//  A required attribute is missing.
//
  ERROR_DS_MISSING_REQUIRED_ATT = DWORD(8316);

//
// MessageId: ERROR_DS_ATT_NOT_DEF_FOR_CLASS
//
// MessageText:
//
//  An attempt was made to modify an object to include an attribute that is not legal for its class.
//
  ERROR_DS_ATT_NOT_DEF_FOR_CLASS = DWORD(8317);

//
// MessageId: ERROR_DS_ATT_ALREADY_EXISTS
//
// MessageText:
//
//  The specified attribute is already present on the object.
//
  ERROR_DS_ATT_ALREADY_EXISTS = DWORD(8318);

// 8319 unused
//
// MessageId: ERROR_DS_CANT_ADD_ATT_VALUES
//
// MessageText:
//
//  The specified attribute is not present, or has no values.
//
  ERROR_DS_CANT_ADD_ATT_VALUES = DWORD(8320);

//
// MessageId: ERROR_DS_SINGLE_VALUE_CONSTRAINT
//
// MessageText:
//
//  Multiple values were specified for an attribute that can have only one value.
//
  ERROR_DS_SINGLE_VALUE_CONSTRAINT = DWORD(8321);

//
// MessageId: ERROR_DS_RANGE_CONSTRAINT
//
// MessageText:
//
//  A value for the attribute was not in the acceptable range of values.
//
  ERROR_DS_RANGE_CONSTRAINT = DWORD(8322);

//
// MessageId: ERROR_DS_ATT_VAL_ALREADY_EXISTS
//
// MessageText:
//
//  The specified value already exists.
//
  ERROR_DS_ATT_VAL_ALREADY_EXISTS = DWORD(8323);

//
// MessageId: ERROR_DS_CANT_REM_MISSING_ATT
//
// MessageText:
//
//  The attribute cannot be removed because it is not present on the object.
//
  ERROR_DS_CANT_REM_MISSING_ATT = DWORD(8324);

//
// MessageId: ERROR_DS_CANT_REM_MISSING_ATT_VAL
//
// MessageText:
//
//  The attribute value cannot be removed because it is not present on the object.
//
  ERROR_DS_CANT_REM_MISSING_ATT_VAL = DWORD(8325);

//
// MessageId: ERROR_DS_ROOT_CANT_BE_SUBREF
//
// MessageText:
//
//  The specified root object cannot be a subref.
//
  ERROR_DS_ROOT_CANT_BE_SUBREF = DWORD(8326);

//
// MessageId: ERROR_DS_NO_CHAINING
//
// MessageText:
//
//  Chaining is not permitted.
//
  ERROR_DS_NO_CHAINING = DWORD(8327);

//
// MessageId: ERROR_DS_NO_CHAINED_EVAL
//
// MessageText:
//
//  Chained evaluation is not permitted.
//
  ERROR_DS_NO_CHAINED_EVAL = DWORD(8328);

//
// MessageId: ERROR_DS_NO_PARENT_OBJECT
//
// MessageText:
//
//  The operation could not be performed because the object's parent is either uninstantiated or deleted.
//
  ERROR_DS_NO_PARENT_OBJECT = DWORD(8329);

//
// MessageId: ERROR_DS_PARENT_IS_AN_ALIAS
//
// MessageText:
//
//  Having a parent that is an alias is not permitted. Aliases are leaf objects.
//
  ERROR_DS_PARENT_IS_AN_ALIAS = DWORD(8330);

//
// MessageId: ERROR_DS_CANT_MIX_MASTER_AND_REPS
//
// MessageText:
//
//  The object and parent must be of the same type, either both masters or both replicas.
//
  ERROR_DS_CANT_MIX_MASTER_AND_REPS = DWORD(8331);

//
// MessageId: ERROR_DS_CHILDREN_EXIST
//
// MessageText:
//
//  The operation cannot be performed because child objects exist. This operation can only be performed on a leaf object.
//
  ERROR_DS_CHILDREN_EXIST = DWORD(8332);

//
// MessageId: ERROR_DS_OBJ_NOT_FOUND
//
// MessageText:
//
//  Directory object not found.
//
  ERROR_DS_OBJ_NOT_FOUND = DWORD(8333);

//
// MessageId: ERROR_DS_ALIASED_OBJ_MISSING
//
// MessageText:
//
//  The aliased object is missing.
//
  ERROR_DS_ALIASED_OBJ_MISSING = DWORD(8334);

//
// MessageId: ERROR_DS_BAD_NAME_SYNTAX
//
// MessageText:
//
//  The object name has bad syntax.
//
  ERROR_DS_BAD_NAME_SYNTAX = DWORD(8335);

//
// MessageId: ERROR_DS_ALIAS_POINTS_TO_ALIAS
//
// MessageText:
//
//  It is not permitted for an alias to refer to another alias.
//
  ERROR_DS_ALIAS_POINTS_TO_ALIAS = DWORD(8336);

//
// MessageId: ERROR_DS_CANT_DEREF_ALIAS
//
// MessageText:
//
//  The alias cannot be dereferenced.
//
  ERROR_DS_CANT_DEREF_ALIAS = DWORD(8337);

//
// MessageId: ERROR_DS_OUT_OF_SCOPE
//
// MessageText:
//
//  The operation is out of scope.
//
  ERROR_DS_OUT_OF_SCOPE = DWORD(8338);

//
// MessageId: ERROR_DS_OBJECT_BEING_REMOVED
//
// MessageText:
//
//  The operation cannot continue because the object is in the process of being removed.
//
  ERROR_DS_OBJECT_BEING_REMOVED = DWORD(8339);

//
// MessageId: ERROR_DS_CANT_DELETE_DSA_OBJ
//
// MessageText:
//
//  The DSA object cannot be deleted.
//
  ERROR_DS_CANT_DELETE_DSA_OBJ = DWORD(8340);

//
// MessageId: ERROR_DS_GENERIC_ERROR
//
// MessageText:
//
//  A directory service error has occurred.
//
  ERROR_DS_GENERIC_ERROR = DWORD(8341);

//
// MessageId: ERROR_DS_DSA_MUST_BE_INT_MASTER
//
// MessageText:
//
//  The operation can only be performed on an internal master DSA object.
//
  ERROR_DS_DSA_MUST_BE_INT_MASTER = DWORD(8342);

//
// MessageId: ERROR_DS_CLASS_NOT_DSA
//
// MessageText:
//
//  The object must be of class DSA.
//
  ERROR_DS_CLASS_NOT_DSA = DWORD(8343);

//
// MessageId: ERROR_DS_INSUFF_ACCESS_RIGHTS
//
// MessageText:
//
//  Insufficient access rights to perform the operation.
//
  ERROR_DS_INSUFF_ACCESS_RIGHTS = DWORD(8344);

//
// MessageId: ERROR_DS_ILLEGAL_SUPERIOR
//
// MessageText:
//
//  The object cannot be added because the parent is not on the list of possible superiors.
//
  ERROR_DS_ILLEGAL_SUPERIOR = DWORD(8345);

//
// MessageId: ERROR_DS_ATTRIBUTE_OWNED_BY_SAM
//
// MessageText:
//
//  Access to the attribute is not permitted because the attribute is owned by the Security Accounts Manager (SAM).
//
  ERROR_DS_ATTRIBUTE_OWNED_BY_SAM = DWORD(8346);

//
// MessageId: ERROR_DS_NAME_TOO_MANY_PARTS
//
// MessageText:
//
//  The name has too many parts.
//
  ERROR_DS_NAME_TOO_MANY_PARTS = DWORD(8347);

//
// MessageId: ERROR_DS_NAME_TOO_LONG
//
// MessageText:
//
//  The name is too long.
//
  ERROR_DS_NAME_TOO_LONG = DWORD(8348);

//
// MessageId: ERROR_DS_NAME_VALUE_TOO_LONG
//
// MessageText:
//
//  The name value is too long.
//
  ERROR_DS_NAME_VALUE_TOO_LONG = DWORD(8349);

//
// MessageId: ERROR_DS_NAME_UNPARSEABLE
//
// MessageText:
//
//  The directory service encountered an error parsing a name.
//
  ERROR_DS_NAME_UNPARSEABLE = DWORD(8350);

//
// MessageId: ERROR_DS_NAME_TYPE_UNKNOWN
//
// MessageText:
//
//  The directory service cannot get the attribute type for a name.
//
  ERROR_DS_NAME_TYPE_UNKNOWN = DWORD(8351);

//
// MessageId: ERROR_DS_NOT_AN_OBJECT
//
// MessageText:
//
//  The name does not identify an object; the name identifies a phantom.
//
  ERROR_DS_NOT_AN_OBJECT = DWORD(8352);

//
// MessageId: ERROR_DS_SEC_DESC_TOO_SHORT
//
// MessageText:
//
//  The security descriptor is too short.
//
  ERROR_DS_SEC_DESC_TOO_SHORT = DWORD(8353);

//
// MessageId: ERROR_DS_SEC_DESC_INVALID
//
// MessageText:
//
//  The security descriptor is invalid.
//
  ERROR_DS_SEC_DESC_INVALID = DWORD(8354);

//
// MessageId: ERROR_DS_NO_DELETED_NAME
//
// MessageText:
//
//  Failed to create name for deleted object.
//
  ERROR_DS_NO_DELETED_NAME = DWORD(8355);

//
// MessageId: ERROR_DS_SUBREF_MUST_HAVE_PARENT
//
// MessageText:
//
//  The parent of a new subref must exist.
//
  ERROR_DS_SUBREF_MUST_HAVE_PARENT = DWORD(8356);

//
// MessageId: ERROR_DS_NCNAME_MUST_BE_NC
//
// MessageText:
//
//  The object must be a naming context.
//
  ERROR_DS_NCNAME_MUST_BE_NC = DWORD(8357);

//
// MessageId: ERROR_DS_CANT_ADD_SYSTEM_ONLY
//
// MessageText:
//
//  It is not permitted to add an attribute which is owned by the system.
//
  ERROR_DS_CANT_ADD_SYSTEM_ONLY = DWORD(8358);

//
// MessageId: ERROR_DS_CLASS_MUST_BE_CONCRETE
//
// MessageText:
//
//  The class of the object must be structural; you cannot instantiate an abstract class.
//
  ERROR_DS_CLASS_MUST_BE_CONCRETE = DWORD(8359);

//
// MessageId: ERROR_DS_INVALID_DMD
//
// MessageText:
//
//  The schema object could not be found.
//
  ERROR_DS_INVALID_DMD = DWORD(8360);

//
// MessageId: ERROR_DS_OBJ_GUID_EXISTS
//
// MessageText:
//
//  A local object with this GUID (dead or alive) already exists.
//
  ERROR_DS_OBJ_GUID_EXISTS = DWORD(8361);

//
// MessageId: ERROR_DS_NOT_ON_BACKLINK
//
// MessageText:
//
//  The operation cannot be performed on a back link.
//
  ERROR_DS_NOT_ON_BACKLINK = DWORD(8362);

//
// MessageId: ERROR_DS_NO_CROSSREF_FOR_NC
//
// MessageText:
//
//  The cross reference for the specified naming context could not be found.
//
  ERROR_DS_NO_CROSSREF_FOR_NC = DWORD(8363);

//
// MessageId: ERROR_DS_SHUTTING_DOWN
//
// MessageText:
//
//  The operation could not be performed because the directory service is shutting down.
//
  ERROR_DS_SHUTTING_DOWN = DWORD(8364);

//
// MessageId: ERROR_DS_UNKNOWN_OPERATION
//
// MessageText:
//
//  The directory service request is invalid.
//
  ERROR_DS_UNKNOWN_OPERATION = DWORD(8365);

//
// MessageId: ERROR_DS_INVALID_ROLE_OWNER
//
// MessageText:
//
//  The role owner attribute could not be read.
//
  ERROR_DS_INVALID_ROLE_OWNER = DWORD(8366);

//
// MessageId: ERROR_DS_COULDNT_CONTACT_FSMO
//
// MessageText:
//
//  The requested FSMO operation failed. The current FSMO holder could not be contacted.
//
  ERROR_DS_COULDNT_CONTACT_FSMO = DWORD(8367);

//
// MessageId: ERROR_DS_CROSS_NC_DN_RENAME
//
// MessageText:
//
//  Modification of a DN across a naming context is not permitted.
//
  ERROR_DS_CROSS_NC_DN_RENAME = DWORD(8368);

//
// MessageId: ERROR_DS_CANT_MOD_SYSTEM_ONLY
//
// MessageText:
//
//  The attribute cannot be modified because it is owned by the system.
//
  ERROR_DS_CANT_MOD_SYSTEM_ONLY = DWORD(8369);

//
// MessageId: ERROR_DS_REPLICATOR_ONLY
//
// MessageText:
//
//  Only the replicator can perform this function.
//
  ERROR_DS_REPLICATOR_ONLY = DWORD(8370);

//
// MessageId: ERROR_DS_OBJ_CLASS_NOT_DEFINED
//
// MessageText:
//
//  The specified class is not defined.
//
  ERROR_DS_OBJ_CLASS_NOT_DEFINED = DWORD(8371);

//
// MessageId: ERROR_DS_OBJ_CLASS_NOT_SUBCLASS
//
// MessageText:
//
//  The specified class is not a subclass.
//
  ERROR_DS_OBJ_CLASS_NOT_SUBCLASS = DWORD(8372);

//
// MessageId: ERROR_DS_NAME_REFERENCE_INVALID
//
// MessageText:
//
//  The name reference is invalid.
//
  ERROR_DS_NAME_REFERENCE_INVALID = DWORD(8373);

//
// MessageId: ERROR_DS_CROSS_REF_EXISTS
//
// MessageText:
//
//  A cross reference already exists.
//
  ERROR_DS_CROSS_REF_EXISTS = DWORD(8374);

//
// MessageId: ERROR_DS_CANT_DEL_MASTER_CROSSREF
//
// MessageText:
//
//  It is not permitted to delete a master cross reference.
//
  ERROR_DS_CANT_DEL_MASTER_CROSSREF = DWORD(8375);

//
// MessageId: ERROR_DS_SUBTREE_NOTIFY_NOT_NC_HEAD
//
// MessageText:
//
//  Subtree notifications are only supported on NC heads.
//
  ERROR_DS_SUBTREE_NOTIFY_NOT_NC_HEAD = DWORD(8376);

//
// MessageId: ERROR_DS_NOTIFY_FILTER_TOO_COMPLEX
//
// MessageText:
//
//  Notification filter is too complex.
//
  ERROR_DS_NOTIFY_FILTER_TOO_COMPLEX = DWORD(8377);

//
// MessageId: ERROR_DS_DUP_RDN
//
// MessageText:
//
//  Schema update failed: duplicate RDN.
//
  ERROR_DS_DUP_RDN = DWORD(8378);

//
// MessageId: ERROR_DS_DUP_OID
//
// MessageText:
//
//  Schema update failed: duplicate OID.
//
  ERROR_DS_DUP_OID = DWORD(8379);

//
// MessageId: ERROR_DS_DUP_MAPI_ID
//
// MessageText:
//
//  Schema update failed: duplicate MAPI identifier.
//
  ERROR_DS_DUP_MAPI_ID = DWORD(8380);

//
// MessageId: ERROR_DS_DUP_SCHEMA_ID_GUID
//
// MessageText:
//
//  Schema update failed: duplicate schema-id GUID.
//
  ERROR_DS_DUP_SCHEMA_ID_GUID = DWORD(8381);

//
// MessageId: ERROR_DS_DUP_LDAP_DISPLAY_NAME
//
// MessageText:
//
//  Schema update failed: duplicate LDAP display name.
//
  ERROR_DS_DUP_LDAP_DISPLAY_NAME = DWORD(8382);

//
// MessageId: ERROR_DS_SEMANTIC_ATT_TEST
//
// MessageText:
//
//  Schema update failed: range-lower less than range upper.
//
  ERROR_DS_SEMANTIC_ATT_TEST = DWORD(8383);

//
// MessageId: ERROR_DS_SYNTAX_MISMATCH
//
// MessageText:
//
//  Schema update failed: syntax mismatch.
//
  ERROR_DS_SYNTAX_MISMATCH = DWORD(8384);

//
// MessageId: ERROR_DS_EXISTS_IN_MUST_HAVE
//
// MessageText:
//
//  Schema deletion failed: attribute is used in must-contain.
//
  ERROR_DS_EXISTS_IN_MUST_HAVE = DWORD(8385);

//
// MessageId: ERROR_DS_EXISTS_IN_MAY_HAVE
//
// MessageText:
//
//  Schema deletion failed: attribute is used in may-contain.
//
  ERROR_DS_EXISTS_IN_MAY_HAVE = DWORD(8386);

//
// MessageId: ERROR_DS_NONEXISTENT_MAY_HAVE
//
// MessageText:
//
//  Schema update failed: attribute in may-contain does not exist.
//
  ERROR_DS_NONEXISTENT_MAY_HAVE = DWORD(8387);

//
// MessageId: ERROR_DS_NONEXISTENT_MUST_HAVE
//
// MessageText:
//
//  Schema update failed: attribute in must-contain does not exist.
//
  ERROR_DS_NONEXISTENT_MUST_HAVE = DWORD(8388);

//
// MessageId: ERROR_DS_AUX_CLS_TEST_FAIL
//
// MessageText:
//
//  Schema update failed: class in aux-class list does not exist or is not an auxiliary class.
//
  ERROR_DS_AUX_CLS_TEST_FAIL = DWORD(8389);

//
// MessageId: ERROR_DS_NONEXISTENT_POSS_SUP
//
// MessageText:
//
//  Schema update failed: class in poss-superiors does not exist.
//
  ERROR_DS_NONEXISTENT_POSS_SUP = DWORD(8390);

//
// MessageId: ERROR_DS_SUB_CLS_TEST_FAIL
//
// MessageText:
//
//  Schema update failed: class in subclassof list does not exist or does not satisfy hierarchy rules.
//
  ERROR_DS_SUB_CLS_TEST_FAIL = DWORD(8391);

//
// MessageId: ERROR_DS_BAD_RDN_ATT_ID_SYNTAX
//
// MessageText:
//
//  Schema update failed: Rdn-Att-Id has wrong syntax.
//
  ERROR_DS_BAD_RDN_ATT_ID_SYNTAX = DWORD(8392);

//
// MessageId: ERROR_DS_EXISTS_IN_AUX_CLS
//
// MessageText:
//
//  Schema deletion failed: class is used as auxiliary class.
//
  ERROR_DS_EXISTS_IN_AUX_CLS = DWORD(8393);

//
// MessageId: ERROR_DS_EXISTS_IN_SUB_CLS
//
// MessageText:
//
//  Schema deletion failed: class is used as sub class.
//
  ERROR_DS_EXISTS_IN_SUB_CLS = DWORD(8394);

//
// MessageId: ERROR_DS_EXISTS_IN_POSS_SUP
//
// MessageText:
//
//  Schema deletion failed: class is used as poss superior.
//
  ERROR_DS_EXISTS_IN_POSS_SUP = DWORD(8395);

//
// MessageId: ERROR_DS_RECALCSCHEMA_FAILED
//
// MessageText:
//
//  Schema update failed in recalculating validation cache.
//
  ERROR_DS_RECALCSCHEMA_FAILED = DWORD(8396);

//
// MessageId: ERROR_DS_TREE_DELETE_NOT_FINISHED
//
// MessageText:
//
//  The tree deletion is not finished.  The request must be made again to continue deleting the tree.
//
  ERROR_DS_TREE_DELETE_NOT_FINISHED = DWORD(8397);

//
// MessageId: ERROR_DS_CANT_DELETE
//
// MessageText:
//
//  The requested delete operation could not be performed.
//
  ERROR_DS_CANT_DELETE = DWORD(8398);

//
// MessageId: ERROR_DS_ATT_SCHEMA_REQ_ID
//
// MessageText:
//
//  Cannot read the governs class identifier for the schema record.
//
  ERROR_DS_ATT_SCHEMA_REQ_ID = DWORD(8399);

//
// MessageId: ERROR_DS_BAD_ATT_SCHEMA_SYNTAX
//
// MessageText:
//
//  The attribute schema has bad syntax.
//
  ERROR_DS_BAD_ATT_SCHEMA_SYNTAX = DWORD(8400);

//
// MessageId: ERROR_DS_CANT_CACHE_ATT
//
// MessageText:
//
//  The attribute could not be cached.
//
  ERROR_DS_CANT_CACHE_ATT = DWORD(8401);

//
// MessageId: ERROR_DS_CANT_CACHE_CLASS
//
// MessageText:
//
//  The class could not be cached.
//
  ERROR_DS_CANT_CACHE_CLASS = DWORD(8402);

//
// MessageId: ERROR_DS_CANT_REMOVE_ATT_CACHE
//
// MessageText:
//
//  The attribute could not be removed from the cache.
//
  ERROR_DS_CANT_REMOVE_ATT_CACHE = DWORD(8403);

//
// MessageId: ERROR_DS_CANT_REMOVE_CLASS_CACHE
//
// MessageText:
//
//  The class could not be removed from the cache.
//
  ERROR_DS_CANT_REMOVE_CLASS_CACHE = DWORD(8404);

//
// MessageId: ERROR_DS_CANT_RETRIEVE_DN
//
// MessageText:
//
//  The distinguished name attribute could not be read.
//
  ERROR_DS_CANT_RETRIEVE_DN = DWORD(8405);

//
// MessageId: ERROR_DS_MISSING_SUPREF
//
// MessageText:
//
//  No superior reference has been configured for the directory service. The directory service is therefore unable to issue referrals to objects outside this forest.
//
  ERROR_DS_MISSING_SUPREF = DWORD(8406);

//
// MessageId: ERROR_DS_CANT_RETRIEVE_INSTANCE
//
// MessageText:
//
//  The instance type attribute could not be retrieved.
//
  ERROR_DS_CANT_RETRIEVE_INSTANCE = DWORD(8407);

//
// MessageId: ERROR_DS_CODE_INCONSISTENCY
//
// MessageText:
//
//  An internal error has occurred.
//
  ERROR_DS_CODE_INCONSISTENCY = DWORD(8408);

//
// MessageId: ERROR_DS_DATABASE_ERROR
//
// MessageText:
//
//  A database error has occurred.
//
  ERROR_DS_DATABASE_ERROR = DWORD(8409);

//
// MessageId: ERROR_DS_GOVERNSID_MISSING
//
// MessageText:
//
//  The attribute GOVERNSID is missing.
//
  ERROR_DS_GOVERNSID_MISSING = DWORD(8410);

//
// MessageId: ERROR_DS_MISSING_EXPECTED_ATT
//
// MessageText:
//
//  An expected attribute is missing.
//
  ERROR_DS_MISSING_EXPECTED_ATT = DWORD(8411);

//
// MessageId: ERROR_DS_NCNAME_MISSING_CR_REF
//
// MessageText:
//
//  The specified naming context is missing a cross reference.
//
  ERROR_DS_NCNAME_MISSING_CR_REF = DWORD(8412);

//
// MessageId: ERROR_DS_SECURITY_CHECKING_ERROR
//
// MessageText:
//
//  A security checking error has occurred.
//
  ERROR_DS_SECURITY_CHECKING_ERROR = DWORD(8413);

//
// MessageId: ERROR_DS_SCHEMA_NOT_LOADED
//
// MessageText:
//
//  The schema is not loaded.
//
  ERROR_DS_SCHEMA_NOT_LOADED = DWORD(8414);

//
// MessageId: ERROR_DS_SCHEMA_ALLOC_FAILED
//
// MessageText:
//
//  Schema allocation failed. Please check if the machine is running low on memory.
//
  ERROR_DS_SCHEMA_ALLOC_FAILED = DWORD(8415);

//
// MessageId: ERROR_DS_ATT_SCHEMA_REQ_SYNTAX
//
// MessageText:
//
//  Failed to obtain the required syntax for the attribute schema.
//
  ERROR_DS_ATT_SCHEMA_REQ_SYNTAX = DWORD(8416);

//
// MessageId: ERROR_DS_GCVERIFY_ERROR
//
// MessageText:
//
//  The global catalog verification failed. The global catalog is not available or does not support the operation. Some part of the directory is currently not available.
//
  ERROR_DS_GCVERIFY_ERROR = DWORD(8417);

//
// MessageId: ERROR_DS_DRA_SCHEMA_MISMATCH
//
// MessageText:
//
//  The replication operation failed because of a schema mismatch between the servers involved.
//
  ERROR_DS_DRA_SCHEMA_MISMATCH = DWORD(8418);

//
// MessageId: ERROR_DS_CANT_FIND_DSA_OBJ
//
// MessageText:
//
//  The DSA object could not be found.
//
  ERROR_DS_CANT_FIND_DSA_OBJ = DWORD(8419);

//
// MessageId: ERROR_DS_CANT_FIND_EXPECTED_NC
//
// MessageText:
//
//  The naming context could not be found.
//
  ERROR_DS_CANT_FIND_EXPECTED_NC = DWORD(8420);

//
// MessageId: ERROR_DS_CANT_FIND_NC_IN_CACHE
//
// MessageText:
//
//  The naming context could not be found in the cache.
//
  ERROR_DS_CANT_FIND_NC_IN_CACHE = DWORD(8421);

//
// MessageId: ERROR_DS_CANT_RETRIEVE_CHILD
//
// MessageText:
//
//  The child object could not be retrieved.
//
  ERROR_DS_CANT_RETRIEVE_CHILD = DWORD(8422);

//
// MessageId: ERROR_DS_SECURITY_ILLEGAL_MODIFY
//
// MessageText:
//
//  The modification was not permitted for security reasons.
//
  ERROR_DS_SECURITY_ILLEGAL_MODIFY = DWORD(8423);

//
// MessageId: ERROR_DS_CANT_REPLACE_HIDDEN_REC
//
// MessageText:
//
//  The operation cannot replace the hidden record.
//
  ERROR_DS_CANT_REPLACE_HIDDEN_REC = DWORD(8424);

//
// MessageId: ERROR_DS_BAD_HIERARCHY_FILE
//
// MessageText:
//
//  The hierarchy file is invalid.
//
  ERROR_DS_BAD_HIERARCHY_FILE = DWORD(8425);

//
// MessageId: ERROR_DS_BUILD_HIERARCHY_TABLE_FAILED
//
// MessageText:
//
//  The attempt to build the hierarchy table failed.
//
  ERROR_DS_BUILD_HIERARCHY_TABLE_FAILED = DWORD(8426);

//
// MessageId: ERROR_DS_CONFIG_PARAM_MISSING
//
// MessageText:
//
//  The directory configuration parameter is missing from the registry.
//
  ERROR_DS_CONFIG_PARAM_MISSING = DWORD(8427);

//
// MessageId: ERROR_DS_COUNTING_AB_INDICES_FAILED
//
// MessageText:
//
//  The attempt to count the address book indices failed.
//
  ERROR_DS_COUNTING_AB_INDICES_FAILED = DWORD(8428);

//
// MessageId: ERROR_DS_HIERARCHY_TABLE_MALLOC_FAILED
//
// MessageText:
//
//  The allocation of the hierarchy table failed.
//
  ERROR_DS_HIERARCHY_TABLE_MALLOC_FAILED = DWORD(8429);

//
// MessageId: ERROR_DS_INTERNAL_FAILURE
//
// MessageText:
//
//  The directory service encountered an internal failure.
//
  ERROR_DS_INTERNAL_FAILURE = DWORD(8430);

//
// MessageId: ERROR_DS_UNKNOWN_ERROR
//
// MessageText:
//
//  The directory service encountered an unknown failure.
//
  ERROR_DS_UNKNOWN_ERROR = DWORD(8431);

//
// MessageId: ERROR_DS_ROOT_REQUIRES_CLASS_TOP
//
// MessageText:
//
//  A root object requires a class of 'top'.
//
  ERROR_DS_ROOT_REQUIRES_CLASS_TOP = DWORD(8432);

//
// MessageId: ERROR_DS_REFUSING_FSMO_ROLES
//
// MessageText:
//
//  This directory server is shutting down, and cannot take ownership of new floating single-master operation roles.
//
  ERROR_DS_REFUSING_FSMO_ROLES = DWORD(8433);

//
// MessageId: ERROR_DS_MISSING_FSMO_SETTINGS
//
// MessageText:
//
//  The directory service is missing mandatory configuration information, and is unable to determine the ownership of floating single-master operation roles.
//
  ERROR_DS_MISSING_FSMO_SETTINGS = DWORD(8434);

//
// MessageId: ERROR_DS_UNABLE_TO_SURRENDER_ROLES
//
// MessageText:
//
//  The directory service was unable to transfer ownership of one or more floating single-master operation roles to other servers.
//
  ERROR_DS_UNABLE_TO_SURRENDER_ROLES = DWORD(8435);

//
// MessageId: ERROR_DS_DRA_GENERIC
//
// MessageText:
//
//  The replication operation failed.
//
  ERROR_DS_DRA_GENERIC = DWORD(8436);

//
// MessageId: ERROR_DS_DRA_INVALID_PARAMETER
//
// MessageText:
//
//  An invalid parameter was specified for this replication operation.
//
  ERROR_DS_DRA_INVALID_PARAMETER = DWORD(8437);

//
// MessageId: ERROR_DS_DRA_BUSY
//
// MessageText:
//
//  The directory service is too busy to complete the replication operation at this time.
//
  ERROR_DS_DRA_BUSY = DWORD(8438);

//
// MessageId: ERROR_DS_DRA_BAD_DN
//
// MessageText:
//
//  The distinguished name specified for this replication operation is invalid.
//
  ERROR_DS_DRA_BAD_DN = DWORD(8439);

//
// MessageId: ERROR_DS_DRA_BAD_NC
//
// MessageText:
//
//  The naming context specified for this replication operation is invalid.
//
  ERROR_DS_DRA_BAD_NC = DWORD(8440);

//
// MessageId: ERROR_DS_DRA_DN_EXISTS
//
// MessageText:
//
//  The distinguished name specified for this replication operation already exists.
//
  ERROR_DS_DRA_DN_EXISTS = DWORD(8441);

//
// MessageId: ERROR_DS_DRA_INTERNAL_ERROR
//
// MessageText:
//
//  The replication system encountered an internal error.
//
  ERROR_DS_DRA_INTERNAL_ERROR = DWORD(8442);

//
// MessageId: ERROR_DS_DRA_INCONSISTENT_DIT
//
// MessageText:
//
//  The replication operation encountered a database inconsistency.
//
  ERROR_DS_DRA_INCONSISTENT_DIT = DWORD(8443);

//
// MessageId: ERROR_DS_DRA_CONNECTION_FAILED
//
// MessageText:
//
//  The server specified for this replication operation could not be contacted.
//
  ERROR_DS_DRA_CONNECTION_FAILED = DWORD(8444);

//
// MessageId: ERROR_DS_DRA_BAD_INSTANCE_TYPE
//
// MessageText:
//
//  The replication operation encountered an object with an invalid instance type.
//
  ERROR_DS_DRA_BAD_INSTANCE_TYPE = DWORD(8445);

//
// MessageId: ERROR_DS_DRA_OUT_OF_MEM
//
// MessageText:
//
//  The replication operation failed to allocate memory.
//
  ERROR_DS_DRA_OUT_OF_MEM = DWORD(8446);

//
// MessageId: ERROR_DS_DRA_MAIL_PROBLEM
//
// MessageText:
//
//  The replication operation encountered an error with the mail system.
//
  ERROR_DS_DRA_MAIL_PROBLEM = DWORD(8447);

//
// MessageId: ERROR_DS_DRA_REF_ALREADY_EXISTS
//
// MessageText:
//
//  The replication reference information for the target server already exists.
//
  ERROR_DS_DRA_REF_ALREADY_EXISTS = DWORD(8448);

//
// MessageId: ERROR_DS_DRA_REF_NOT_FOUND
//
// MessageText:
//
//  The replication reference information for the target server does not exist.
//
  ERROR_DS_DRA_REF_NOT_FOUND = DWORD(8449);

//
// MessageId: ERROR_DS_DRA_OBJ_IS_REP_SOURCE
//
// MessageText:
//
//  The naming context cannot be removed because it is replicated to another server.
//
  ERROR_DS_DRA_OBJ_IS_REP_SOURCE = DWORD(8450);

//
// MessageId: ERROR_DS_DRA_DB_ERROR
//
// MessageText:
//
//  The replication operation encountered a database error.
//
  ERROR_DS_DRA_DB_ERROR = DWORD(8451);

//
// MessageId: ERROR_DS_DRA_NO_REPLICA
//
// MessageText:
//
//  The naming context is in the process of being removed or is not replicated from the specified server.
//
  ERROR_DS_DRA_NO_REPLICA = DWORD(8452);

//
// MessageId: ERROR_DS_DRA_ACCESS_DENIED
//
// MessageText:
//
//  Replication access was denied.
//
  ERROR_DS_DRA_ACCESS_DENIED = DWORD(8453);

//
// MessageId: ERROR_DS_DRA_NOT_SUPPORTED
//
// MessageText:
//
//  The requested operation is not supported by this version of the directory service.
//
  ERROR_DS_DRA_NOT_SUPPORTED = DWORD(8454);

//
// MessageId: ERROR_DS_DRA_RPC_CANCELLED
//
// MessageText:
//
//  The replication remote procedure call was cancelled.
//
  ERROR_DS_DRA_RPC_CANCELLED = DWORD(8455);

//
// MessageId: ERROR_DS_DRA_SOURCE_DISABLED
//
// MessageText:
//
//  The source server is currently rejecting replication requests.
//
  ERROR_DS_DRA_SOURCE_DISABLED = DWORD(8456);

//
// MessageId: ERROR_DS_DRA_SINK_DISABLED
//
// MessageText:
//
//  The destination server is currently rejecting replication requests.
//
  ERROR_DS_DRA_SINK_DISABLED = DWORD(8457);

//
// MessageId: ERROR_DS_DRA_NAME_COLLISION
//
// MessageText:
//
//  The replication operation failed due to a collision of object names.
//
  ERROR_DS_DRA_NAME_COLLISION = DWORD(8458);

//
// MessageId: ERROR_DS_DRA_SOURCE_REINSTALLED
//
// MessageText:
//
//  The replication source has been reinstalled.
//
  ERROR_DS_DRA_SOURCE_REINSTALLED = DWORD(8459);

//
// MessageId: ERROR_DS_DRA_MISSING_PARENT
//
// MessageText:
//
//  The replication operation failed because a required parent object is missing.
//
  ERROR_DS_DRA_MISSING_PARENT = DWORD(8460);

//
// MessageId: ERROR_DS_DRA_PREEMPTED
//
// MessageText:
//
//  The replication operation was preempted.
//
  ERROR_DS_DRA_PREEMPTED = DWORD(8461);

//
// MessageId: ERROR_DS_DRA_ABANDON_SYNC
//
// MessageText:
//
//  The replication synchronization attempt was abandoned because of a lack of updates.
//
  ERROR_DS_DRA_ABANDON_SYNC = DWORD(8462);

//
// MessageId: ERROR_DS_DRA_SHUTDOWN
//
// MessageText:
//
//  The replication operation was terminated because the system is shutting down.
//
  ERROR_DS_DRA_SHUTDOWN = DWORD(8463);

//
// MessageId: ERROR_DS_DRA_INCOMPATIBLE_PARTIAL_SET
//
// MessageText:
//
//  Synchronization attempt failed because the destination DC is currently waiting to synchronize new partial attributes from source. This condition is normal if a recent schema change modified the partial attribute set. The destination partial attribute set is not a subset of source partial attribute set.
//
  ERROR_DS_DRA_INCOMPATIBLE_PARTIAL_SET = DWORD(8464);

//
// MessageId: ERROR_DS_DRA_SOURCE_IS_PARTIAL_REPLICA
//
// MessageText:
//
//  The replication synchronization attempt failed because a master replica attempted to sync from a partial replica.
//
  ERROR_DS_DRA_SOURCE_IS_PARTIAL_REPLICA = DWORD(8465);

//
// MessageId: ERROR_DS_DRA_EXTN_CONNECTION_FAILED
//
// MessageText:
//
//  The server specified for this replication operation was contacted, but that server was unable to contact an additional server needed to complete the operation.
//
  ERROR_DS_DRA_EXTN_CONNECTION_FAILED = DWORD(8466);

//
// MessageId: ERROR_DS_INSTALL_SCHEMA_MISMATCH
//
// MessageText:
//
//  The version of the Active Directory schema of the source forest is not compatible with the version of Active Directory on this computer.
//
  ERROR_DS_INSTALL_SCHEMA_MISMATCH = DWORD(8467);

//
// MessageId: ERROR_DS_DUP_LINK_ID
//
// MessageText:
//
//  Schema update failed: An attribute with the same link identifier already exists.
//
  ERROR_DS_DUP_LINK_ID = DWORD(8468);

//
// MessageId: ERROR_DS_NAME_ERROR_RESOLVING
//
// MessageText:
//
//  Name translation: Generic processing error.
//
  ERROR_DS_NAME_ERROR_RESOLVING = DWORD(8469);

//
// MessageId: ERROR_DS_NAME_ERROR_NOT_FOUND
//
// MessageText:
//
//  Name translation: Could not find the name or insufficient right to see name.
//
  ERROR_DS_NAME_ERROR_NOT_FOUND = DWORD(8470);

//
// MessageId: ERROR_DS_NAME_ERROR_NOT_UNIQUE
//
// MessageText:
//
//  Name translation: Input name mapped to more than one output name.
//
  ERROR_DS_NAME_ERROR_NOT_UNIQUE = DWORD(8471);

//
// MessageId: ERROR_DS_NAME_ERROR_NO_MAPPING
//
// MessageText:
//
//  Name translation: Input name found, but not the associated output format.
//
  ERROR_DS_NAME_ERROR_NO_MAPPING = DWORD(8472);

//
// MessageId: ERROR_DS_NAME_ERROR_DOMAIN_ONLY
//
// MessageText:
//
//  Name translation: Unable to resolve completely, only the domain was found.
//
  ERROR_DS_NAME_ERROR_DOMAIN_ONLY = DWORD(8473);

//
// MessageId: ERROR_DS_NAME_ERROR_NO_SYNTACTICAL_MAPPING
//
// MessageText:
//
//  Name translation: Unable to perform purely syntactical mapping at the client without going out to the wire.
//
  ERROR_DS_NAME_ERROR_NO_SYNTACTICAL_MAPPING = DWORD(8474);

//
// MessageId: ERROR_DS_CONSTRUCTED_ATT_MOD
//
// MessageText:
//
//  Modification of a constructed attribute is not allowed.
//
  ERROR_DS_CONSTRUCTED_ATT_MOD = DWORD(8475);

//
// MessageId: ERROR_DS_WRONG_OM_OBJ_CLASS
//
// MessageText:
//
//  The OM-Object-Class specified is incorrect for an attribute with the specified syntax.
//
  ERROR_DS_WRONG_OM_OBJ_CLASS = DWORD(8476);

//
// MessageId: ERROR_DS_DRA_REPL_PENDING
//
// MessageText:
//
//  The replication request has been posted; waiting for reply.
//
  ERROR_DS_DRA_REPL_PENDING = DWORD(8477);

//
// MessageId: ERROR_DS_DS_REQUIRED
//
// MessageText:
//
//  The requested operation requires a directory service, and none was available.
//
  ERROR_DS_DS_REQUIRED = DWORD(8478);

//
// MessageId: ERROR_DS_INVALID_LDAP_DISPLAY_NAME
//
// MessageText:
//
//  The LDAP display name of the class or attribute contains non-ASCII characters.
//
  ERROR_DS_INVALID_LDAP_DISPLAY_NAME = DWORD(8479);

//
// MessageId: ERROR_DS_NON_BASE_SEARCH
//
// MessageText:
//
//  The requested search operation is only supported for base searches.
//
  ERROR_DS_NON_BASE_SEARCH = DWORD(8480);

//
// MessageId: ERROR_DS_CANT_RETRIEVE_ATTS
//
// MessageText:
//
//  The search failed to retrieve attributes from the database.
//
  ERROR_DS_CANT_RETRIEVE_ATTS = DWORD(8481);

//
// MessageId: ERROR_DS_BACKLINK_WITHOUT_LINK
//
// MessageText:
//
//  The schema update operation tried to add a backward link attribute that has no corresponding forward link.
//
  ERROR_DS_BACKLINK_WITHOUT_LINK = DWORD(8482);

//
// MessageId: ERROR_DS_EPOCH_MISMATCH
//
// MessageText:
//
//  Source and destination of a cross-domain move do not agree on the object's epoch number.  Either source or destination does not have the latest version of the object.
//
  ERROR_DS_EPOCH_MISMATCH = DWORD(8483);

//
// MessageId: ERROR_DS_SRC_NAME_MISMATCH
//
// MessageText:
//
//  Source and destination of a cross-domain move do not agree on the object's current name.  Either source or destination does not have the latest version of the object.
//
  ERROR_DS_SRC_NAME_MISMATCH = DWORD(8484);

//
// MessageId: ERROR_DS_SRC_AND_DST_NC_IDENTICAL
//
// MessageText:
//
//  Source and destination for the cross-domain move operation are identical.  Caller should use local move operation instead of cross-domain move operation.
//
  ERROR_DS_SRC_AND_DST_NC_IDENTICAL = DWORD(8485);

//
// MessageId: ERROR_DS_DST_NC_MISMATCH
//
// MessageText:
//
//  Source and destination for a cross-domain move are not in agreement on the naming contexts in the forest.  Either source or destination does not have the latest version of the Partitions container.
//
  ERROR_DS_DST_NC_MISMATCH = DWORD(8486);

//
// MessageId: ERROR_DS_NOT_AUTHORITIVE_FOR_DST_NC
//
// MessageText:
//
//  Destination of a cross-domain move is not authoritative for the destination naming context.
//
  ERROR_DS_NOT_AUTHORITIVE_FOR_DST_NC = DWORD(8487);

//
// MessageId: ERROR_DS_SRC_GUID_MISMATCH
//
// MessageText:
//
//  Source and destination of a cross-domain move do not agree on the identity of the source object.  Either source or destination does not have the latest version of the source object.
//
  ERROR_DS_SRC_GUID_MISMATCH = DWORD(8488);

//
// MessageId: ERROR_DS_CANT_MOVE_DELETED_OBJECT
//
// MessageText:
//
//  Object being moved across-domains is already known to be deleted by the destination server.  The source server does not have the latest version of the source object.
//
  ERROR_DS_CANT_MOVE_DELETED_OBJECT = DWORD(8489);

//
// MessageId: ERROR_DS_PDC_OPERATION_IN_PROGRESS
//
// MessageText:
//
//  Another operation which requires exclusive access to the PDC FSMO is already in progress.
//
  ERROR_DS_PDC_OPERATION_IN_PROGRESS = DWORD(8490);

//
// MessageId: ERROR_DS_CROSS_DOMAIN_CLEANUP_REQD
//
// MessageText:
//
//  A cross-domain move operation failed such that two versions of the moved object exist - one each in the source and destination domains.  The destination object needs to be removed to restore the system to a consistent state.
//
  ERROR_DS_CROSS_DOMAIN_CLEANUP_REQD = DWORD(8491);

//
// MessageId: ERROR_DS_ILLEGAL_XDOM_MOVE_OPERATION
//
// MessageText:
//
//  This object may not be moved across domain boundaries either because cross-domain moves for this class are disallowed, or the object has some special characteristics, e.g.: trust account or restricted RID, which prevent its move.
//
  ERROR_DS_ILLEGAL_XDOM_MOVE_OPERATION = DWORD(8492);

//
// MessageId: ERROR_DS_CANT_WITH_ACCT_GROUP_MEMBERSHPS
//
// MessageText:
//
//  Can't move objects with memberships across domain boundaries as once moved, this would violate the membership conditions of the account group.  Remove the object from any account group memberships and retry.
//
  ERROR_DS_CANT_WITH_ACCT_GROUP_MEMBERSHPS = DWORD(8493);

//
// MessageId: ERROR_DS_NC_MUST_HAVE_NC_PARENT
//
// MessageText:
//
//  A naming context head must be the immediate child of another naming context head, not of an interior node.
//
  ERROR_DS_NC_MUST_HAVE_NC_PARENT = DWORD(8494);

//
// MessageId: ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE
//
// MessageText:
//
//  The directory cannot validate the proposed naming context name because it does not hold a replica of the naming context above the proposed naming context.  Please ensure that the domain naming master role is held by a server that is configured as a global catalog server, and that the server is up to date with its replication partners. (Applies only to Windows 2000 Domain Naming masters)
//
  ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE = DWORD(8495);

//
// MessageId: ERROR_DS_DST_DOMAIN_NOT_NATIVE
//
// MessageText:
//
//  Destination domain must be in native mode.
//
  ERROR_DS_DST_DOMAIN_NOT_NATIVE = DWORD(8496);

//
// MessageId: ERROR_DS_MISSING_INFRASTRUCTURE_CONTAINER
//
// MessageText:
//
//  The operation can not be performed because the server does not have an infrastructure container in the domain of interest.
//
  ERROR_DS_MISSING_INFRASTRUCTURE_CONTAINER = DWORD(8497);

//
// MessageId: ERROR_DS_CANT_MOVE_ACCOUNT_GROUP
//
// MessageText:
//
//  Cross-domain move of non-empty account groups is not allowed.
//
  ERROR_DS_CANT_MOVE_ACCOUNT_GROUP = DWORD(8498);

//
// MessageId: ERROR_DS_CANT_MOVE_RESOURCE_GROUP
//
// MessageText:
//
//  Cross-domain move of non-empty resource groups is not allowed.
//
  ERROR_DS_CANT_MOVE_RESOURCE_GROUP = DWORD(8499);

//
// MessageId: ERROR_DS_INVALID_SEARCH_FLAG
//
// MessageText:
//
//  The search flags for the attribute are invalid. The ANR bit is valid only on attributes of Unicode or Teletex strings.
//
  ERROR_DS_INVALID_SEARCH_FLAG = DWORD(8500);

//
// MessageId: ERROR_DS_NO_TREE_DELETE_ABOVE_NC
//
// MessageText:
//
//  Tree deletions starting at an object which has an NC head as a descendant are not allowed.
//
  ERROR_DS_NO_TREE_DELETE_ABOVE_NC = DWORD(8501);

//
// MessageId: ERROR_DS_COULDNT_LOCK_TREE_FOR_DELETE
//
// MessageText:
//
//  The directory service failed to lock a tree in preparation for a tree deletion because the tree was in use.
//
  ERROR_DS_COULDNT_LOCK_TREE_FOR_DELETE = DWORD(8502);

//
// MessageId: ERROR_DS_COULDNT_IDENTIFY_OBJECTS_FOR_TREE_DELETE
//
// MessageText:
//
//  The directory service failed to identify the list of objects to delete while attempting a tree deletion.
//
  ERROR_DS_COULDNT_IDENTIFY_OBJECTS_FOR_TREE_DELETE = DWORD(8503);

//
// MessageId: ERROR_DS_SAM_INIT_FAILURE
//
// MessageText:
//
//  Security Accounts Manager initialization failed because of the following error: %1.
//  Error Status: 0x%2. Click OK to shut down the system and reboot into Directory Services Restore Mode. Check the event log for detailed information.
//
  ERROR_DS_SAM_INIT_FAILURE = DWORD(8504);

//
// MessageId: ERROR_DS_SENSITIVE_GROUP_VIOLATION
//
// MessageText:
//
//  Only an administrator can modify the membership list of an administrative group.
//
  ERROR_DS_SENSITIVE_GROUP_VIOLATION = DWORD(8505);

//
// MessageId: ERROR_DS_CANT_MOD_PRIMARYGROUPID
//
// MessageText:
//
//  Cannot change the primary group ID of a domain controller account.
//
  ERROR_DS_CANT_MOD_PRIMARYGROUPID = DWORD(8506);

//
// MessageId: ERROR_DS_ILLEGAL_BASE_SCHEMA_MOD
//
// MessageText:
//
//  An attempt is made to modify the base schema.
//
  ERROR_DS_ILLEGAL_BASE_SCHEMA_MOD = DWORD(8507);

//
// MessageId: ERROR_DS_NONSAFE_SCHEMA_CHANGE
//
// MessageText:
//
//  Adding a new mandatory attribute to an existing class, deleting a mandatory attribute from an existing class, or adding an optional attribute to the special class Top that is not a backlink attribute (directly or through inheritance, for example, by adding or deleting an auxiliary class) is not allowed.
//
  ERROR_DS_NONSAFE_SCHEMA_CHANGE = DWORD(8508);

//
// MessageId: ERROR_DS_SCHEMA_UPDATE_DISALLOWED
//
// MessageText:
//
//  Schema update is not allowed on this DC because the DC is not the schema FSMO Role Owner.
//
  ERROR_DS_SCHEMA_UPDATE_DISALLOWED = DWORD(8509);

//
// MessageId: ERROR_DS_CANT_CREATE_UNDER_SCHEMA
//
// MessageText:
//
//  An object of this class cannot be created under the schema container. You can only create attribute-schema and class-schema objects under the schema container.
//
  ERROR_DS_CANT_CREATE_UNDER_SCHEMA = DWORD(8510);

//
// MessageId: ERROR_DS_INSTALL_NO_SRC_SCH_VERSION
//
// MessageText:
//
//  The replica/child install failed to get the objectVersion attribute on the schema container on the source DC. Either the attribute is missing on the schema container or the credentials supplied do not have permission to read it.
//
  ERROR_DS_INSTALL_NO_SRC_SCH_VERSION = DWORD(8511);

//
// MessageId: ERROR_DS_INSTALL_NO_SCH_VERSION_IN_INIFILE
//
// MessageText:
//
//  The replica/child install failed to read the objectVersion attribute in the SCHEMA section of the file schema.ini in the system32 directory.
//
  ERROR_DS_INSTALL_NO_SCH_VERSION_IN_INIFILE = DWORD(8512);

//
// MessageId: ERROR_DS_INVALID_GROUP_TYPE
//
// MessageText:
//
//  The specified group type is invalid.
//
  ERROR_DS_INVALID_GROUP_TYPE = DWORD(8513);

//
// MessageId: ERROR_DS_NO_NEST_GLOBALGROUP_IN_MIXEDDOMAIN
//
// MessageText:
//
//  You cannot nest global groups in a mixed domain if the group is security-enabled.
//
  ERROR_DS_NO_NEST_GLOBALGROUP_IN_MIXEDDOMAIN = DWORD(8514);

//
// MessageId: ERROR_DS_NO_NEST_LOCALGROUP_IN_MIXEDDOMAIN
//
// MessageText:
//
//  You cannot nest local groups in a mixed domain if the group is security-enabled.
//
  ERROR_DS_NO_NEST_LOCALGROUP_IN_MIXEDDOMAIN = DWORD(8515);

//
// MessageId: ERROR_DS_GLOBAL_CANT_HAVE_LOCAL_MEMBER
//
// MessageText:
//
//  A global group cannot have a local group as a member.
//
  ERROR_DS_GLOBAL_CANT_HAVE_LOCAL_MEMBER = DWORD(8516);

//
// MessageId: ERROR_DS_GLOBAL_CANT_HAVE_UNIVERSAL_MEMBER
//
// MessageText:
//
//  A global group cannot have a universal group as a member.
//
  ERROR_DS_GLOBAL_CANT_HAVE_UNIVERSAL_MEMBER = DWORD(8517);

//
// MessageId: ERROR_DS_UNIVERSAL_CANT_HAVE_LOCAL_MEMBER
//
// MessageText:
//
//  A universal group cannot have a local group as a member.
//
  ERROR_DS_UNIVERSAL_CANT_HAVE_LOCAL_MEMBER = DWORD(8518);

//
// MessageId: ERROR_DS_GLOBAL_CANT_HAVE_CROSSDOMAIN_MEMBER
//
// MessageText:
//
//  A global group cannot have a cross-domain member.
//
  ERROR_DS_GLOBAL_CANT_HAVE_CROSSDOMAIN_MEMBER = DWORD(8519);

//
// MessageId: ERROR_DS_LOCAL_CANT_HAVE_CROSSDOMAIN_LOCAL_MEMBER
//
// MessageText:
//
//  A local group cannot have another cross domain local group as a member.
//
  ERROR_DS_LOCAL_CANT_HAVE_CROSSDOMAIN_LOCAL_MEMBER = DWORD(8520);

//
// MessageId: ERROR_DS_HAVE_PRIMARY_MEMBERS
//
// MessageText:
//
//  A group with primary members cannot change to a security-disabled group.
//
  ERROR_DS_HAVE_PRIMARY_MEMBERS = DWORD(8521);

//
// MessageId: ERROR_DS_STRING_SD_CONVERSION_FAILED
//
// MessageText:
//
//  The schema cache load failed to convert the string default SD on a class-schema object.
//
  ERROR_DS_STRING_SD_CONVERSION_FAILED = DWORD(8522);

//
// MessageId: ERROR_DS_NAMING_MASTER_GC
//
// MessageText:
//
//  Only DSAs configured to be Global Catalog servers should be allowed to hold the Domain Naming Master FSMO role. (Applies only to Windows 2000 servers)
//
  ERROR_DS_NAMING_MASTER_GC = DWORD(8523);

//
// MessageId: ERROR_DS_DNS_LOOKUP_FAILURE
//
// MessageText:
//
//  The DSA operation is unable to proceed because of a DNS lookup failure.
//
  ERROR_DS_DNS_LOOKUP_FAILURE = DWORD(8524);

//
// MessageId: ERROR_DS_COULDNT_UPDATE_SPNS
//
// MessageText:
//
//  While processing a change to the DNS Host Name for an object, the Service Principal Name values could not be kept in sync.
//
  ERROR_DS_COULDNT_UPDATE_SPNS = DWORD(8525);

//
// MessageId: ERROR_DS_CANT_RETRIEVE_SD
//
// MessageText:
//
//  The Security Descriptor attribute could not be read.
//
  ERROR_DS_CANT_RETRIEVE_SD = DWORD(8526);

//
// MessageId: ERROR_DS_KEY_NOT_UNIQUE
//
// MessageText:
//
//  The object requested was not found, but an object with that key was found.
//
  ERROR_DS_KEY_NOT_UNIQUE = DWORD(8527);

//
// MessageId: ERROR_DS_WRONG_LINKED_ATT_SYNTAX
//
// MessageText:
//
//  The syntax of the linked attribute being added is incorrect. Forward links can only have syntax 2.5.5.1, 2.5.5.7, and 2.5.5.14, and backlinks can only have syntax 2.5.5.1
//
  ERROR_DS_WRONG_LINKED_ATT_SYNTAX = DWORD(8528);

//
// MessageId: ERROR_DS_SAM_NEED_BOOTKEY_PASSWORD
//
// MessageText:
//
//  Security Account Manager needs to get the boot password.
//
  ERROR_DS_SAM_NEED_BOOTKEY_PASSWORD = DWORD(8529);

//
// MessageId: ERROR_DS_SAM_NEED_BOOTKEY_FLOPPY
//
// MessageText:
//
//  Security Account Manager needs to get the boot key from floppy disk.
//
  ERROR_DS_SAM_NEED_BOOTKEY_FLOPPY = DWORD(8530);

//
// MessageId: ERROR_DS_CANT_START
//
// MessageText:
//
//  Directory Service cannot start.
//
  ERROR_DS_CANT_START = DWORD(8531);

//
// MessageId: ERROR_DS_INIT_FAILURE
//
// MessageText:
//
//  Directory Services could not start.
//
  ERROR_DS_INIT_FAILURE = DWORD(8532);

//
// MessageId: ERROR_DS_NO_PKT_PRIVACY_ON_CONNECTION
//
// MessageText:
//
//  The connection between client and server requires packet privacy or better.
//
  ERROR_DS_NO_PKT_PRIVACY_ON_CONNECTION = DWORD(8533);

//
// MessageId: ERROR_DS_SOURCE_DOMAIN_IN_FOREST
//
// MessageText:
//
//  The source domain may not be in the same forest as destination.
//
  ERROR_DS_SOURCE_DOMAIN_IN_FOREST = DWORD(8534);

//
// MessageId: ERROR_DS_DESTINATION_DOMAIN_NOT_IN_FOREST
//
// MessageText:
//
//  The destination domain must be in the forest.
//
  ERROR_DS_DESTINATION_DOMAIN_NOT_IN_FOREST = DWORD(8535);

//
// MessageId: ERROR_DS_DESTINATION_AUDITING_NOT_ENABLED
//
// MessageText:
//
//  The operation requires that destination domain auditing be enabled.
//
  ERROR_DS_DESTINATION_AUDITING_NOT_ENABLED = DWORD(8536);

//
// MessageId: ERROR_DS_CANT_FIND_DC_FOR_SRC_DOMAIN
//
// MessageText:
//
//  The operation couldn't locate a DC for the source domain.
//
  ERROR_DS_CANT_FIND_DC_FOR_SRC_DOMAIN = DWORD(8537);

//
// MessageId: ERROR_DS_SRC_OBJ_NOT_GROUP_OR_USER
//
// MessageText:
//
//  The source object must be a group or user.
//
  ERROR_DS_SRC_OBJ_NOT_GROUP_OR_USER = DWORD(8538);

//
// MessageId: ERROR_DS_SRC_SID_EXISTS_IN_FOREST
//
// MessageText:
//
//  The source object's SID already exists in destination forest.
//
  ERROR_DS_SRC_SID_EXISTS_IN_FOREST = DWORD(8539);

//
// MessageId: ERROR_DS_SRC_AND_DST_OBJECT_CLASS_MISMATCH
//
// MessageText:
//
//  The source and destination object must be of the same type.
//
  ERROR_DS_SRC_AND_DST_OBJECT_CLASS_MISMATCH = DWORD(8540);

//
// MessageId: ERROR_SAM_INIT_FAILURE
//
// MessageText:
//
//  Security Accounts Manager initialization failed because of the following error: %1.
//  Error Status: 0x%2. Click OK to shut down the system and reboot into Safe Mode. Check the event log for detailed information.
//
  ERROR_SAM_INIT_FAILURE = DWORD(8541);

//
// MessageId: ERROR_DS_DRA_SCHEMA_INFO_SHIP
//
// MessageText:
//
//  Schema information could not be included in the replication request.
//
  ERROR_DS_DRA_SCHEMA_INFO_SHIP = DWORD(8542);

//
// MessageId: ERROR_DS_DRA_SCHEMA_CONFLICT
//
// MessageText:
//
//  The replication operation could not be completed due to a schema incompatibility.
//
  ERROR_DS_DRA_SCHEMA_CONFLICT = DWORD(8543);

//
// MessageId: ERROR_DS_DRA_EARLIER_SCHEMA_CONFLICT
//
// MessageText:
//
//  The replication operation could not be completed due to a previous schema incompatibility.
//
  ERROR_DS_DRA_EARLIER_SCHEMA_CONFLICT = DWORD(8544);

//
// MessageId: ERROR_DS_DRA_OBJ_NC_MISMATCH
//
// MessageText:
//
//  The replication update could not be applied because either the source or the destination has not yet received information regarding a recent cross-domain move operation.
//
  ERROR_DS_DRA_OBJ_NC_MISMATCH = DWORD(8545);

//
// MessageId: ERROR_DS_NC_STILL_HAS_DSAS
//
// MessageText:
//
//  The requested domain could not be deleted because there exist domain controllers that still host this domain.
//
  ERROR_DS_NC_STILL_HAS_DSAS = DWORD(8546);

//
// MessageId: ERROR_DS_GC_REQUIRED
//
// MessageText:
//
//  The requested operation can be performed only on a global catalog server.
//
  ERROR_DS_GC_REQUIRED = DWORD(8547);

//
// MessageId: ERROR_DS_LOCAL_MEMBER_OF_LOCAL_ONLY
//
// MessageText:
//
//  A local group can only be a member of other local groups in the same domain.
//
  ERROR_DS_LOCAL_MEMBER_OF_LOCAL_ONLY = DWORD(8548);

//
// MessageId: ERROR_DS_NO_FPO_IN_UNIVERSAL_GROUPS
//
// MessageText:
//
//  Foreign security principals cannot be members of universal groups.
//
  ERROR_DS_NO_FPO_IN_UNIVERSAL_GROUPS = DWORD(8549);

//
// MessageId: ERROR_DS_CANT_ADD_TO_GC
//
// MessageText:
//
//  The attribute is not allowed to be replicated to the GC because of security reasons.
//
  ERROR_DS_CANT_ADD_TO_GC = DWORD(8550);

//
// MessageId: ERROR_DS_NO_CHECKPOINT_WITH_PDC
//
// MessageText:
//
//  The checkpoint with the PDC could not be taken because there too many modifications being processed currently.
//
  ERROR_DS_NO_CHECKPOINT_WITH_PDC = DWORD(8551);

//
// MessageId: ERROR_DS_SOURCE_AUDITING_NOT_ENABLED
//
// MessageText:
//
//  The operation requires that source domain auditing be enabled.
//
  ERROR_DS_SOURCE_AUDITING_NOT_ENABLED = DWORD(8552);

//
// MessageId: ERROR_DS_CANT_CREATE_IN_NONDOMAIN_NC
//
// MessageText:
//
//  Security principal objects can only be created inside domain naming contexts.
//
  ERROR_DS_CANT_CREATE_IN_NONDOMAIN_NC = DWORD(8553);

//
// MessageId: ERROR_DS_INVALID_NAME_FOR_SPN
//
// MessageText:
//
//  A Service Principal Name (SPN) could not be constructed because the provided hostname is not in the necessary format.
//
  ERROR_DS_INVALID_NAME_FOR_SPN = DWORD(8554);

//
// MessageId: ERROR_DS_FILTER_USES_CONTRUCTED_ATTRS
//
// MessageText:
//
//  A Filter was passed that uses constructed attributes.
//
  ERROR_DS_FILTER_USES_CONTRUCTED_ATTRS = DWORD(8555);

//
// MessageId: ERROR_DS_UNICODEPWD_NOT_IN_QUOTES
//
// MessageText:
//
//  The unicodePwd attribute value must be enclosed in double quotes.
//
  ERROR_DS_UNICODEPWD_NOT_IN_QUOTES = DWORD(8556);

//
// MessageId: ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED
//
// MessageText:
//
//  Your computer could not be joined to the domain. You have exceeded the maximum number of computer accounts you are allowed to create in this domain. Contact your system administrator to have this limit reset or increased.
//
  ERROR_DS_MACHINE_ACCOUNT_QUOTA_EXCEEDED = DWORD(8557);

//
// MessageId: ERROR_DS_MUST_BE_RUN_ON_DST_DC
//
// MessageText:
//
//  For security reasons, the operation must be run on the destination DC.
//
  ERROR_DS_MUST_BE_RUN_ON_DST_DC = DWORD(8558);

//
// MessageId: ERROR_DS_SRC_DC_MUST_BE_SP4_OR_GREATER
//
// MessageText:
//
//  For security reasons, the source DC must be NT4SP4 or greater.
//
  ERROR_DS_SRC_DC_MUST_BE_SP4_OR_GREATER = DWORD(8559);

//
// MessageId: ERROR_DS_CANT_TREE_DELETE_CRITICAL_OBJ
//
// MessageText:
//
//  Critical Directory Service System objects cannot be deleted during tree delete operations.  The tree delete may have been partially performed.
//
  ERROR_DS_CANT_TREE_DELETE_CRITICAL_OBJ = DWORD(8560);

//
// MessageId: ERROR_DS_INIT_FAILURE_CONSOLE
//
// MessageText:
//
//  Directory Services could not start because of the following error: %1.
//  Error Status: 0x%2. Please click OK to shutdown the system. You can use the recovery console to diagnose the system further.
//
  ERROR_DS_INIT_FAILURE_CONSOLE = DWORD(8561);

//
// MessageId: ERROR_DS_SAM_INIT_FAILURE_CONSOLE
//
// MessageText:
//
//  Security Accounts Manager initialization failed because of the following error: %1.
//  Error Status: 0x%2. Please click OK to shutdown the system. You can use the recovery console to diagnose the system further.
//
  ERROR_DS_SAM_INIT_FAILURE_CONSOLE = DWORD(8562);

//
// MessageId: ERROR_DS_FOREST_VERSION_TOO_HIGH
//
// MessageText:
//
//  The version of the operating system installed is incompatible with the current forest functional level. You must upgrade to a new version of the operating system before this server can become a domain controller in this forest.
//
  ERROR_DS_FOREST_VERSION_TOO_HIGH = DWORD(8563);

//
// MessageId: ERROR_DS_DOMAIN_VERSION_TOO_HIGH
//
// MessageText:
//
//  The version of the operating system installed is incompatible with the current domain functional level. You must upgrade to a new version of the operating system before this server can become a domain controller in this domain.
//
  ERROR_DS_DOMAIN_VERSION_TOO_HIGH = DWORD(8564);

//
// MessageId: ERROR_DS_FOREST_VERSION_TOO_LOW
//
// MessageText:
//
//  The version of the operating system installed on this server no longer supports the current forest functional level. You must raise the forest functional level before this server can become a domain controller in this forest.
//
  ERROR_DS_FOREST_VERSION_TOO_LOW = DWORD(8565);

//
// MessageId: ERROR_DS_DOMAIN_VERSION_TOO_LOW
//
// MessageText:
//
//  The version of the operating system installed on this server no longer supports the current domain functional level. You must raise the domain functional level before this server can become a domain controller in this domain.
//
  ERROR_DS_DOMAIN_VERSION_TOO_LOW = DWORD(8566);

//
// MessageId: ERROR_DS_INCOMPATIBLE_VERSION
//
// MessageText:
//
//  The version of the operating system installed on this server is incompatible with the functional level of the domain or forest.
//
  ERROR_DS_INCOMPATIBLE_VERSION = DWORD(8567);

//
// MessageId: ERROR_DS_LOW_DSA_VERSION
//
// MessageText:
//
//  The functional level of the domain (or forest) cannot be raised to the requested value, because there exist one or more domain controllers in the domain (or forest) that are at a lower incompatible functional level.
//
  ERROR_DS_LOW_DSA_VERSION = DWORD(8568);

//
// MessageId: ERROR_DS_NO_BEHAVIOR_VERSION_IN_MIXEDDOMAIN
//
// MessageText:
//
//  The forest functional level cannot be raised to the requested value since one or more domains are still in mixed domain mode. All domains in the forest must be in native mode, for you to raise the forest functional level.
//
  ERROR_DS_NO_BEHAVIOR_VERSION_IN_MIXEDDOMAIN = DWORD(8569);

//
// MessageId: ERROR_DS_NOT_SUPPORTED_SORT_ORDER
//
// MessageText:
//
//  The sort order requested is not supported.
//
  ERROR_DS_NOT_SUPPORTED_SORT_ORDER = DWORD(8570);

//
// MessageId: ERROR_DS_NAME_NOT_UNIQUE
//
// MessageText:
//
//  The requested name already exists as a unique identifier.
//
  ERROR_DS_NAME_NOT_UNIQUE = DWORD(8571);

//
// MessageId: ERROR_DS_MACHINE_ACCOUNT_CREATED_PRENT4
//
// MessageText:
//
//  The machine account was created pre-NT4.  The account needs to be recreated.
//
  ERROR_DS_MACHINE_ACCOUNT_CREATED_PRENT4 = DWORD(8572);

//
// MessageId: ERROR_DS_OUT_OF_VERSION_STORE
//
// MessageText:
//
//  The database is out of version store.
//
  ERROR_DS_OUT_OF_VERSION_STORE = DWORD(8573);

//
// MessageId: ERROR_DS_INCOMPATIBLE_CONTROLS_USED
//
// MessageText:
//
//  Unable to continue operation because multiple conflicting controls were used.
//
  ERROR_DS_INCOMPATIBLE_CONTROLS_USED = DWORD(8574);

//
// MessageId: ERROR_DS_NO_REF_DOMAIN
//
// MessageText:
//
//  Unable to find a valid security descriptor reference domain for this partition.
//
  ERROR_DS_NO_REF_DOMAIN = DWORD(8575);

//
// MessageId: ERROR_DS_RESERVED_LINK_ID
//
// MessageText:
//
//  Schema update failed: The link identifier is reserved.
//
  ERROR_DS_RESERVED_LINK_ID = DWORD(8576);

//
// MessageId: ERROR_DS_LINK_ID_NOT_AVAILABLE
//
// MessageText:
//
//  Schema update failed: There are no link identifiers available.
//
  ERROR_DS_LINK_ID_NOT_AVAILABLE = DWORD(8577);

//
// MessageId: ERROR_DS_AG_CANT_HAVE_UNIVERSAL_MEMBER
//
// MessageText:
//
//  An account group can not have a universal group as a member.
//
  ERROR_DS_AG_CANT_HAVE_UNIVERSAL_MEMBER = DWORD(8578);

//
// MessageId: ERROR_DS_MODIFYDN_DISALLOWED_BY_INSTANCE_TYPE
//
// MessageText:
//
//  Rename or move operations on naming context heads or read-only objects are not allowed.
//
  ERROR_DS_MODIFYDN_DISALLOWED_BY_INSTANCE_TYPE = DWORD(8579);

//
// MessageId: ERROR_DS_NO_OBJECT_MOVE_IN_SCHEMA_NC
//
// MessageText:
//
//  Move operations on objects in the schema naming context are not allowed.
//
  ERROR_DS_NO_OBJECT_MOVE_IN_SCHEMA_NC = DWORD(8580);

//
// MessageId: ERROR_DS_MODIFYDN_DISALLOWED_BY_FLAG
//
// MessageText:
//
//  A system flag has been set on the object and does not allow the object to be moved or renamed.
//
  ERROR_DS_MODIFYDN_DISALLOWED_BY_FLAG = DWORD(8581);

//
// MessageId: ERROR_DS_MODIFYDN_WRONG_GRANDPARENT
//
// MessageText:
//
//  This object is not allowed to change its grandparent container. Moves are not forbidden on this object, but are restricted to sibling containers.
//
  ERROR_DS_MODIFYDN_WRONG_GRANDPARENT = DWORD(8582);

//
// MessageId: ERROR_DS_NAME_ERROR_TRUST_REFERRAL
//
// MessageText:
//
//  Unable to resolve completely, a referral to another forest is generated.
//
  ERROR_DS_NAME_ERROR_TRUST_REFERRAL = DWORD(8583);

//
// MessageId: ERROR_NOT_SUPPORTED_ON_STANDARD_SERVER
//
// MessageText:
//
//  The requested action is not supported on standard server.
//
  ERROR_NOT_SUPPORTED_ON_STANDARD_SERVER = DWORD(8584);

//
// MessageId: ERROR_DS_CANT_ACCESS_REMOTE_PART_OF_AD
//
// MessageText:
//
//  Could not access a partition of the Active Directory located on a remote server.  Make sure at least one server is running for the partition in question.
//
  ERROR_DS_CANT_ACCESS_REMOTE_PART_OF_AD = DWORD(8585);

//
// MessageId: ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE_V2
//
// MessageText:
//
//  The directory cannot validate the proposed naming context (or partition) name because it does not hold a replica nor can it contact a replica of the naming context above the proposed naming context.  Please ensure that the parent naming context is properly registered in DNS, and at least one replica of this naming context is reachable by the Domain Naming master.
//
  ERROR_DS_CR_IMPOSSIBLE_TO_VALIDATE_V2 = DWORD(8586);

//
// MessageId: ERROR_DS_THREAD_LIMIT_EXCEEDED
//
// MessageText:
//
//  The thread limit for this request was exceeded.
//
  ERROR_DS_THREAD_LIMIT_EXCEEDED = DWORD(8587);

//
// MessageId: ERROR_DS_NOT_CLOSEST
//
// MessageText:
//
//  The Global catalog server is not in the closest site.
//
  ERROR_DS_NOT_CLOSEST = DWORD(8588);

//
// MessageId: ERROR_DS_CANT_DERIVE_SPN_WITHOUT_SERVER_REF
//
// MessageText:
//
//  The DS cannot derive a service principal name (SPN) with which to mutually authenticate the target server because the corresponding server object in the local DS database has no serverReference attribute.
//
  ERROR_DS_CANT_DERIVE_SPN_WITHOUT_SERVER_REF = DWORD(8589);

//
// MessageId: ERROR_DS_SINGLE_USER_MODE_FAILED
//
// MessageText:
//
//  The Directory Service failed to enter single user mode.
//
  ERROR_DS_SINGLE_USER_MODE_FAILED = DWORD(8590);

//
// MessageId: ERROR_DS_NTDSCRIPT_SYNTAX_ERROR
//
// MessageText:
//
//  The Directory Service cannot parse the script because of a syntax error.
//
  ERROR_DS_NTDSCRIPT_SYNTAX_ERROR = DWORD(8591);

//
// MessageId: ERROR_DS_NTDSCRIPT_PROCESS_ERROR
//
// MessageText:
//
//  The Directory Service cannot process the script because of an error.
//
  ERROR_DS_NTDSCRIPT_PROCESS_ERROR = DWORD(8592);

//
// MessageId: ERROR_DS_DIFFERENT_REPL_EPOCHS
//
// MessageText:
//
//  The directory service cannot perform the requested operation because the servers
//  involved are of different replication epochs (which is usually related to a
//  domain rename that is in progress).
//
  ERROR_DS_DIFFERENT_REPL_EPOCHS = DWORD(8593);

//
// MessageId: ERROR_DS_DRS_EXTENSIONS_CHANGED
//
// MessageText:
//
//  The directory service binding must be renegotiated due to a change in the server
//  extensions information.
//
  ERROR_DS_DRS_EXTENSIONS_CHANGED = DWORD(8594);

//
// MessageId: ERROR_DS_REPLICA_SET_CHANGE_NOT_ALLOWED_ON_DISABLED_CR
//
// MessageText:
//
//  Operation not allowed on a disabled cross ref.
//
  ERROR_DS_REPLICA_SET_CHANGE_NOT_ALLOWED_ON_DISABLED_CR = DWORD(8595);

//
// MessageId: ERROR_DS_NO_MSDS_INTID
//
// MessageText:
//
//  Schema update failed: No values for msDS-IntId are available.
//
  ERROR_DS_NO_MSDS_INTID = DWORD(8596);

//
// MessageId: ERROR_DS_DUP_MSDS_INTID
//
// MessageText:
//
//  Schema update failed: Duplicate msDS-INtId. Retry the operation.
//
  ERROR_DS_DUP_MSDS_INTID = DWORD(8597);

//
// MessageId: ERROR_DS_EXISTS_IN_RDNATTID
//
// MessageText:
//
//  Schema deletion failed: attribute is used in rDNAttID.
//
  ERROR_DS_EXISTS_IN_RDNATTID = DWORD(8598);

//
// MessageId: ERROR_DS_AUTHORIZATION_FAILED
//
// MessageText:
//
//  The directory service failed to authorize the request.
//
  ERROR_DS_AUTHORIZATION_FAILED = DWORD(8599);

//
// MessageId: ERROR_DS_INVALID_SCRIPT
//
// MessageText:
//
//  The Directory Service cannot process the script because it is invalid.
//
  ERROR_DS_INVALID_SCRIPT = DWORD(8600);

//
// MessageId: ERROR_DS_REMOTE_CROSSREF_OP_FAILED
//
// MessageText:
//
//  The remote create cross reference operation failed on the Domain Naming Master FSMO.  The operation's error is in the extended data.
//
  ERROR_DS_REMOTE_CROSSREF_OP_FAILED = DWORD(8601);

//
// MessageId: ERROR_DS_CROSS_REF_BUSY
//
// MessageText:
//
//  A cross reference is in use locally with the same name.
//
  ERROR_DS_CROSS_REF_BUSY = DWORD(8602);

//
// MessageId: ERROR_DS_CANT_DERIVE_SPN_FOR_DELETED_DOMAIN
//
// MessageText:
//
//  The DS cannot derive a service principal name (SPN) with which to mutually authenticate the target server because the server's domain has been deleted from the forest.
//
  ERROR_DS_CANT_DERIVE_SPN_FOR_DELETED_DOMAIN = DWORD(8603);

//
// MessageId: ERROR_DS_CANT_DEMOTE_WITH_WRITEABLE_NC
//
// MessageText:
//
//  Writeable NCs prevent this DC from demoting.
//
  ERROR_DS_CANT_DEMOTE_WITH_WRITEABLE_NC = DWORD(8604);

//
// MessageId: ERROR_DS_DUPLICATE_ID_FOUND
//
// MessageText:
//
//  The requested object has a non-unique identifier and cannot be retrieved.
//
  ERROR_DS_DUPLICATE_ID_FOUND = DWORD(8605);

//
// MessageId: ERROR_DS_INSUFFICIENT_ATTR_TO_CREATE_OBJECT
//
// MessageText:
//
//  Insufficient attributes were given to create an object.  This object may not exist because it may have been deleted and already garbage collected.
//
  ERROR_DS_INSUFFICIENT_ATTR_TO_CREATE_OBJECT = DWORD(8606);

//
// MessageId: ERROR_DS_GROUP_CONVERSION_ERROR
//
// MessageText:
//
//  The group cannot be converted due to attribute restrictions on the requested group type.
//
  ERROR_DS_GROUP_CONVERSION_ERROR = DWORD(8607);

//
// MessageId: ERROR_DS_CANT_MOVE_APP_BASIC_GROUP
//
// MessageText:
//
//  Cross-domain move of non-empty basic application groups is not allowed.
//
  ERROR_DS_CANT_MOVE_APP_BASIC_GROUP = DWORD(8608);

//
// MessageId: ERROR_DS_CANT_MOVE_APP_QUERY_GROUP
//
// MessageText:
//
//  Cross-domain move of non-empty query based application groups is not allowed.
//
  ERROR_DS_CANT_MOVE_APP_QUERY_GROUP = DWORD(8609);

//
// MessageId: ERROR_DS_ROLE_NOT_VERIFIED
//
// MessageText:
//
//  The FSMO role ownership could not be verified because its directory partition has not replicated successfully with atleast one replication partner. 
//
  ERROR_DS_ROLE_NOT_VERIFIED = DWORD(8610);

//
// MessageId: ERROR_DS_WKO_CONTAINER_CANNOT_BE_SPECIAL
//
// MessageText:
//
//  The target container for a redirection of a well known object container cannot already be a special container.
//
  ERROR_DS_WKO_CONTAINER_CANNOT_BE_SPECIAL = DWORD(8611);

//
// MessageId: ERROR_DS_DOMAIN_RENAME_IN_PROGRESS
//
// MessageText:
//
//  The Directory Service cannot perform the requested operation because a domain rename operation is in progress.
//
  ERROR_DS_DOMAIN_RENAME_IN_PROGRESS = DWORD(8612);

//
// MessageId: ERROR_DS_EXISTING_AD_CHILD_NC
//
// MessageText:
//
//  The Active Directory detected an Active Directory child partition below the
//  requested new partition name.  The Active Directory's partition heiarchy must
//  be created in a top down method.
//
  ERROR_DS_EXISTING_AD_CHILD_NC = DWORD(8613);

//
// MessageId: ERROR_DS_REPL_LIFETIME_EXCEEDED
//
// MessageText:
//
//  The Active Directory cannot replicate with this server because the time since the last replication with this server has exceeded the tombstone lifetime.
//
  ERROR_DS_REPL_LIFETIME_EXCEEDED = DWORD(8614);

//
// MessageId: ERROR_DS_DISALLOWED_IN_SYSTEM_CONTAINER
//
// MessageText:
//
//  The requested operation is not allowed on an object under the system container.
//
  ERROR_DS_DISALLOWED_IN_SYSTEM_CONTAINER = DWORD(8615);

//
// MessageId: ERROR_DS_LDAP_SEND_QUEUE_FULL
//
// MessageText:
//
//  The LDAP servers network send queue has filled up because the client is not
//  processing the results of it's requests fast enough.  No more requests will
//  be processed until the client catches up.  If the client does not catch up
//  then it will be disconnected.
//
  ERROR_DS_LDAP_SEND_QUEUE_FULL = 8616;

//
// MessageId: ERROR_DS_DRA_OUT_SCHEDULE_WINDOW
//
// MessageText:
//
//  The scheduled replication did not take place because the system was too busy to execute the request within the schedule window.  The replication queue is overloaded. Consider reducing the number of partners or decreasing the scheduled replication frequency.
//
  ERROR_DS_DRA_OUT_SCHEDULE_WINDOW = DWORD(8617);

///////////////////////////////////////////////////
//                                                /
//     End of Active Directory Error Codes        /
//                                                /
//                  8000 to  8999                 /
///////////////////////////////////////////////////


///////////////////////////////////////////////////
//                                               //
//                  DNS Error Codes              //
//                                               //
//                   9000 to 9999                //
///////////////////////////////////////////////////

// =============================
// Facility DNS Error Messages
// =============================

//
//  DNS response codes.
//

  DNS_ERROR_RESPONSE_CODES_BASE = 9000;

  DNS_ERROR_RCODE_NO_ERROR = NO_ERROR;

  DNS_ERROR_MASK = $00002328;  // 9000 or DNS_ERROR_RESPONSE_CODES_BASE

// DNS_ERROR_RCODE_FORMAT_ERROR          0x00002329
//
// MessageId: DNS_ERROR_RCODE_FORMAT_ERROR
//
// MessageText:
//
//  DNS server unable to interpret format.
//
  DNS_ERROR_RCODE_FORMAT_ERROR = DWORD(9001);

// DNS_ERROR_RCODE_SERVER_FAILURE        0x0000232a
//
// MessageId: DNS_ERROR_RCODE_SERVER_FAILURE
//
// MessageText:
//
//  DNS server failure.
//
  DNS_ERROR_RCODE_SERVER_FAILURE = DWORD(9002);

// DNS_ERROR_RCODE_NAME_ERROR            0x0000232b
//
// MessageId: DNS_ERROR_RCODE_NAME_ERROR
//
// MessageText:
//
//  DNS name does not exist.
//
  DNS_ERROR_RCODE_NAME_ERROR = DWORD(9003);

// DNS_ERROR_RCODE_NOT_IMPLEMENTED       0x0000232c
//
// MessageId: DNS_ERROR_RCODE_NOT_IMPLEMENTED
//
// MessageText:
//
//  DNS request not supported by name server.
//
  DNS_ERROR_RCODE_NOT_IMPLEMENTED = DWORD(9004);

// DNS_ERROR_RCODE_REFUSED               0x0000232d
//
// MessageId: DNS_ERROR_RCODE_REFUSED
//
// MessageText:
//
//  DNS operation refused.
//
  DNS_ERROR_RCODE_REFUSED = DWORD(9005);

// DNS_ERROR_RCODE_YXDOMAIN              0x0000232e
//
// MessageId: DNS_ERROR_RCODE_YXDOMAIN
//
// MessageText:
//
//  DNS name that ought not exist, does exist.
//
  DNS_ERROR_RCODE_YXDOMAIN = DWORD(9006);

// DNS_ERROR_RCODE_YXRRSET               0x0000232f
//
// MessageId: DNS_ERROR_RCODE_YXRRSET
//
// MessageText:
//
//  DNS RR set that ought not exist, does exist.
//
  DNS_ERROR_RCODE_YXRRSET = DWORD(9007);

// DNS_ERROR_RCODE_NXRRSET               0x00002330
//
// MessageId: DNS_ERROR_RCODE_NXRRSET
//
// MessageText:
//
//  DNS RR set that ought to exist, does not exist.
//
  DNS_ERROR_RCODE_NXRRSET = DWORD(9008);

// DNS_ERROR_RCODE_NOTAUTH               0x00002331
//
// MessageId: DNS_ERROR_RCODE_NOTAUTH
//
// MessageText:
//
//  DNS server not authoritative for zone.
//
  DNS_ERROR_RCODE_NOTAUTH = DWORD(9009);

// DNS_ERROR_RCODE_NOTZONE               0x00002332
//
// MessageId: DNS_ERROR_RCODE_NOTZONE
//
// MessageText:
//
//  DNS name in update or prereq is not in zone.
//
  DNS_ERROR_RCODE_NOTZONE = DWORD(9010);

// DNS_ERROR_RCODE_BADSIG                0x00002338
//
// MessageId: DNS_ERROR_RCODE_BADSIG
//
// MessageText:
//
//  DNS signature failed to verify.
//
  DNS_ERROR_RCODE_BADSIG = DWORD(9016);

// DNS_ERROR_RCODE_BADKEY                0x00002339
//
// MessageId: DNS_ERROR_RCODE_BADKEY
//
// MessageText:
//
//  DNS bad key.
//
  DNS_ERROR_RCODE_BADKEY = DWORD(9017);

// DNS_ERROR_RCODE_BADTIME               0x0000233a
//
// MessageId: DNS_ERROR_RCODE_BADTIME
//
// MessageText:
//
//  DNS signature validity expired.
//
  DNS_ERROR_RCODE_BADTIME = DWORD(9018);

  DNS_ERROR_RCODE_LAST = DNS_ERROR_RCODE_BADTIME;


//
//  Packet format
//

  DNS_ERROR_PACKET_FMT_BASE = 9500;

// DNS_INFO_NO_RECORDS                   0x0000251d
//
// MessageId: DNS_INFO_NO_RECORDS
//
// MessageText:
//
//  No records found for given DNS query.
//
  DNS_INFO_NO_RECORDS = DWORD(9501);

// DNS_ERROR_BAD_PACKET                  0x0000251e
//
// MessageId: DNS_ERROR_BAD_PACKET
//
// MessageText:
//
//  Bad DNS packet.
//
  DNS_ERROR_BAD_PACKET = DWORD(9502);

// DNS_ERROR_NO_PACKET                   0x0000251f
//
// MessageId: DNS_ERROR_NO_PACKET
//
// MessageText:
//
//  No DNS packet.
//
  DNS_ERROR_NO_PACKET = DWORD(9503);

// DNS_ERROR_RCODE                       0x00002520
//
// MessageId: DNS_ERROR_RCODE
//
// MessageText:
//
//  DNS error, check rcode.
//
  DNS_ERROR_RCODE = DWORD(9504);

// DNS_ERROR_UNSECURE_PACKET             0x00002521
//
// MessageId: DNS_ERROR_UNSECURE_PACKET
//
// MessageText:
//
//  Unsecured DNS packet.
//
  DNS_ERROR_UNSECURE_PACKET = DWORD(9505);

  DNS_STATUS_PACKET_UNSECURE = DNS_ERROR_UNSECURE_PACKET;


//
//  General API errors
//

  DNS_ERROR_NO_MEMORY = ERROR_OUTOFMEMORY;
  DNS_ERROR_INVALID_NAME = ERROR_INVALID_NAME;
  DNS_ERROR_INVALID_DATA = ERROR_INVALID_DATA;

  DNS_ERROR_GENERAL_API_BASE = 9550;

// DNS_ERROR_INVALID_TYPE                0x0000254f
//
// MessageId: DNS_ERROR_INVALID_TYPE
//
// MessageText:
//
//  Invalid DNS type.
//
  DNS_ERROR_INVALID_TYPE = DWORD(9551);

// DNS_ERROR_INVALID_IP_ADDRESS          0x00002550
//
// MessageId: DNS_ERROR_INVALID_IP_ADDRESS
//
// MessageText:
//
//  Invalid IP address.
//
  DNS_ERROR_INVALID_IP_ADDRESS = DWORD(9552);

// DNS_ERROR_INVALID_PROPERTY            0x00002551
//
// MessageId: DNS_ERROR_INVALID_PROPERTY
//
// MessageText:
//
//  Invalid property.
//
  DNS_ERROR_INVALID_PROPERTY = DWORD(9553);

// DNS_ERROR_TRY_AGAIN_LATER             0x00002552
//
// MessageId: DNS_ERROR_TRY_AGAIN_LATER
//
// MessageText:
//
//  Try DNS operation again later.
//
  DNS_ERROR_TRY_AGAIN_LATER = DWORD(9554);

// DNS_ERROR_NOT_UNIQUE                  0x00002553
//
// MessageId: DNS_ERROR_NOT_UNIQUE
//
// MessageText:
//
//  Record for given name and type is not unique.
//
  DNS_ERROR_NOT_UNIQUE = DWORD(9555);

// DNS_ERROR_NON_RFC_NAME                0x00002554
//
// MessageId: DNS_ERROR_NON_RFC_NAME
//
// MessageText:
//
//  DNS name does not comply with RFC specifications.
//
  DNS_ERROR_NON_RFC_NAME = DWORD(9556);

// DNS_STATUS_FQDN                       0x00002555
//
// MessageId: DNS_STATUS_FQDN
//
// MessageText:
//
//  DNS name is a fully-qualified DNS name.
//
  DNS_STATUS_FQDN = DWORD(9557);

// DNS_STATUS_DOTTED_NAME                0x00002556
//
// MessageId: DNS_STATUS_DOTTED_NAME
//
// MessageText:
//
//  DNS name is dotted (multi-label).
//
  DNS_STATUS_DOTTED_NAME = DWORD(9558);

// DNS_STATUS_SINGLE_PART_NAME           0x00002557
//
// MessageId: DNS_STATUS_SINGLE_PART_NAME
//
// MessageText:
//
//  DNS name is a single-part name.
//
  DNS_STATUS_SINGLE_PART_NAME = DWORD(9559);

// DNS_ERROR_INVALID_NAME_CHAR           0x00002558
//
// MessageId: DNS_ERROR_INVALID_NAME_CHAR
//
// MessageText:
//
//  DNS name contains an invalid character.
//
  DNS_ERROR_INVALID_NAME_CHAR = DWORD(9560);

// DNS_ERROR_NUMERIC_NAME                0x00002559
//
// MessageId: DNS_ERROR_NUMERIC_NAME
//
// MessageText:
//
//  DNS name is entirely numeric.
//
  DNS_ERROR_NUMERIC_NAME = DWORD(9561);

// DNS_ERROR_NOT_ALLOWED_ON_ROOT_SERVER  0x0000255A
//
// MessageId: DNS_ERROR_NOT_ALLOWED_ON_ROOT_SERVER
//
// MessageText:
//
//  The operation requested is not permitted on a DNS root server.
//
  DNS_ERROR_NOT_ALLOWED_ON_ROOT_SERVER = DWORD(9562);

// DNS_ERROR_NOT_ALLOWED_UNDER_DELEGATION  0x0000255B
//
// MessageId: DNS_ERROR_NOT_ALLOWED_UNDER_DELEGATION
//
// MessageText:
//
//  The record could not be created because this part of the DNS namespace has
//  been delegated to another server.
//
  DNS_ERROR_NOT_ALLOWED_UNDER_DELEGATION = DWORD(9563);

// DNS_ERROR_CANNOT_FIND_ROOT_HINTS  0x0000255C
//
// MessageId: DNS_ERROR_CANNOT_FIND_ROOT_HINTS
//
// MessageText:
//
//  The DNS server could not find a set of root hints.
//
  DNS_ERROR_CANNOT_FIND_ROOT_HINTS = DWORD(9564);

// DNS_ERROR_INCONSISTENT_ROOT_HINTS  0x0000255D
//
// MessageId: DNS_ERROR_INCONSISTENT_ROOT_HINTS
//
// MessageText:
//
//  The DNS server found root hints but they were not consistent across
//  all adapters.
//
  DNS_ERROR_INCONSISTENT_ROOT_HINTS = DWORD(9565);


//
//  Zone errors
//

  DNS_ERROR_ZONE_BASE = 9600;

// DNS_ERROR_ZONE_DOES_NOT_EXIST         0x00002581
//
// MessageId: DNS_ERROR_ZONE_DOES_NOT_EXIST
//
// MessageText:
//
//  DNS zone does not exist.
//
  DNS_ERROR_ZONE_DOES_NOT_EXIST = DWORD(9601);

// DNS_ERROR_NO_ZONE_INFO                0x00002582
//
// MessageId: DNS_ERROR_NO_ZONE_INFO
//
// MessageText:
//
//  DNS zone information not available.
//
  DNS_ERROR_NO_ZONE_INFO = DWORD(9602);

// DNS_ERROR_INVALID_ZONE_OPERATION      0x00002583
//
// MessageId: DNS_ERROR_INVALID_ZONE_OPERATION
//
// MessageText:
//
//  Invalid operation for DNS zone.
//
  DNS_ERROR_INVALID_ZONE_OPERATION = DWORD(9603);

// DNS_ERROR_ZONE_CONFIGURATION_ERROR    0x00002584
//
// MessageId: DNS_ERROR_ZONE_CONFIGURATION_ERROR
//
// MessageText:
//
//  Invalid DNS zone configuration.
//
  DNS_ERROR_ZONE_CONFIGURATION_ERROR = DWORD(9604);

// DNS_ERROR_ZONE_HAS_NO_SOA_RECORD      0x00002585
//
// MessageId: DNS_ERROR_ZONE_HAS_NO_SOA_RECORD
//
// MessageText:
//
//  DNS zone has no start of authority (SOA) record.
//
  DNS_ERROR_ZONE_HAS_NO_SOA_RECORD = DWORD(9605);

// DNS_ERROR_ZONE_HAS_NO_NS_RECORDS      0x00002586
//
// MessageId: DNS_ERROR_ZONE_HAS_NO_NS_RECORDS
//
// MessageText:
//
//  DNS zone has no Name Server (NS) record.
//
  DNS_ERROR_ZONE_HAS_NO_NS_RECORDS = DWORD(9606);

// DNS_ERROR_ZONE_LOCKED                 0x00002587
//
// MessageId: DNS_ERROR_ZONE_LOCKED
//
// MessageText:
//
//  DNS zone is locked.
//
  DNS_ERROR_ZONE_LOCKED = DWORD(9607);

// DNS_ERROR_ZONE_CREATION_FAILED        0x00002588
//
// MessageId: DNS_ERROR_ZONE_CREATION_FAILED
//
// MessageText:
//
//  DNS zone creation failed.
//
  DNS_ERROR_ZONE_CREATION_FAILED = DWORD(9608);

// DNS_ERROR_ZONE_ALREADY_EXISTS         0x00002589
//
// MessageId: DNS_ERROR_ZONE_ALREADY_EXISTS
//
// MessageText:
//
//  DNS zone already exists.
//
  DNS_ERROR_ZONE_ALREADY_EXISTS = DWORD(9609);

// DNS_ERROR_AUTOZONE_ALREADY_EXISTS     0x0000258a
//
// MessageId: DNS_ERROR_AUTOZONE_ALREADY_EXISTS
//
// MessageText:
//
//  DNS automatic zone already exists.
//
  DNS_ERROR_AUTOZONE_ALREADY_EXISTS = DWORD(9610);

// DNS_ERROR_INVALID_ZONE_TYPE           0x0000258b
//
// MessageId: DNS_ERROR_INVALID_ZONE_TYPE
//
// MessageText:
//
//  Invalid DNS zone type.
//
  DNS_ERROR_INVALID_ZONE_TYPE = DWORD(9611);

// DNS_ERROR_SECONDARY_REQUIRES_MASTER_IP 0x0000258c
//
// MessageId: DNS_ERROR_SECONDARY_REQUIRES_MASTER_IP
//
// MessageText:
//
//  Secondary DNS zone requires master IP address.
//
  DNS_ERROR_SECONDARY_REQUIRES_MASTER_IP = DWORD(9612);

// DNS_ERROR_ZONE_NOT_SECONDARY          0x0000258d
//
// MessageId: DNS_ERROR_ZONE_NOT_SECONDARY
//
// MessageText:
//
//  DNS zone not secondary.
//
  DNS_ERROR_ZONE_NOT_SECONDARY = DWORD(9613);

// DNS_ERROR_NEED_SECONDARY_ADDRESSES    0x0000258e
//
// MessageId: DNS_ERROR_NEED_SECONDARY_ADDRESSES
//
// MessageText:
//
//  Need secondary IP address.
//
  DNS_ERROR_NEED_SECONDARY_ADDRESSES = DWORD(9614);

// DNS_ERROR_WINS_INIT_FAILED            0x0000258f
//
// MessageId: DNS_ERROR_WINS_INIT_FAILED
//
// MessageText:
//
//  WINS initialization failed.
//
  DNS_ERROR_WINS_INIT_FAILED = DWORD(9615);

// DNS_ERROR_NEED_WINS_SERVERS           0x00002590
//
// MessageId: DNS_ERROR_NEED_WINS_SERVERS
//
// MessageText:
//
//  Need WINS servers.
//
  DNS_ERROR_NEED_WINS_SERVERS = DWORD(9616);

// DNS_ERROR_NBSTAT_INIT_FAILED          0x00002591
//
// MessageId: DNS_ERROR_NBSTAT_INIT_FAILED
//
// MessageText:
//
//  NBTSTAT initialization call failed.
//
  DNS_ERROR_NBSTAT_INIT_FAILED = DWORD(9617);

// DNS_ERROR_SOA_DELETE_INVALID          0x00002592
//
// MessageId: DNS_ERROR_SOA_DELETE_INVALID
//
// MessageText:
//
//  Invalid delete of start of authority (SOA)
//
  DNS_ERROR_SOA_DELETE_INVALID = DWORD(9618);

// DNS_ERROR_FORWARDER_ALREADY_EXISTS    0x00002593
//
// MessageId: DNS_ERROR_FORWARDER_ALREADY_EXISTS
//
// MessageText:
//
//  A conditional forwarding zone already exists for that name.
//
  DNS_ERROR_FORWARDER_ALREADY_EXISTS = DWORD(9619);

// DNS_ERROR_ZONE_REQUIRES_MASTER_IP     0x00002594
//
// MessageId: DNS_ERROR_ZONE_REQUIRES_MASTER_IP
//
// MessageText:
//
//  This zone must be configured with one or more master DNS server IP addresses.
//
  DNS_ERROR_ZONE_REQUIRES_MASTER_IP = DWORD(9620);

// DNS_ERROR_ZONE_IS_SHUTDOWN            0x00002595
//
// MessageId: DNS_ERROR_ZONE_IS_SHUTDOWN
//
// MessageText:
//
//  The operation cannot be performed because this zone is shutdown.
//
  DNS_ERROR_ZONE_IS_SHUTDOWN = DWORD(9621);


//
//  Datafile errors
//

  DNS_ERROR_DATAFILE_BASE = 9650;

// DNS                                   0x000025b3
//
// MessageId: DNS_ERROR_PRIMARY_REQUIRES_DATAFILE
//
// MessageText:
//
//  Primary DNS zone requires datafile.
//
  DNS_ERROR_PRIMARY_REQUIRES_DATAFILE = DWORD(9651);

// DNS                                   0x000025b4
//
// MessageId: DNS_ERROR_INVALID_DATAFILE_NAME
//
// MessageText:
//
//  Invalid datafile name for DNS zone.
//
  DNS_ERROR_INVALID_DATAFILE_NAME = DWORD(9652);

// DNS                                   0x000025b5
//
// MessageId: DNS_ERROR_DATAFILE_OPEN_FAILURE
//
// MessageText:
//
//  Failed to open datafile for DNS zone.
//
  DNS_ERROR_DATAFILE_OPEN_FAILURE = DWORD(9653);

// DNS                                   0x000025b6
//
// MessageId: DNS_ERROR_FILE_WRITEBACK_FAILED
//
// MessageText:
//
//  Failed to write datafile for DNS zone.
//
  DNS_ERROR_FILE_WRITEBACK_FAILED = DWORD(9654);

// DNS                                   0x000025b7
//
// MessageId: DNS_ERROR_DATAFILE_PARSING
//
// MessageText:
//
//  Failure while reading datafile for DNS zone.
//
  DNS_ERROR_DATAFILE_PARSING = DWORD(9655);


//
//  Database errors
//

  DNS_ERROR_DATABASE_BASE = 9700;

// DNS_ERROR_RECORD_DOES_NOT_EXIST       0x000025e5
//
// MessageId: DNS_ERROR_RECORD_DOES_NOT_EXIST
//
// MessageText:
//
//  DNS record does not exist.
//
  DNS_ERROR_RECORD_DOES_NOT_EXIST = DWORD(9701);

// DNS_ERROR_RECORD_FORMAT               0x000025e6
//
// MessageId: DNS_ERROR_RECORD_FORMAT
//
// MessageText:
//
//  DNS record format error.
//
  DNS_ERROR_RECORD_FORMAT = DWORD(9702);

// DNS_ERROR_NODE_CREATION_FAILED        0x000025e7
//
// MessageId: DNS_ERROR_NODE_CREATION_FAILED
//
// MessageText:
//
//  Node creation failure in DNS.
//
  DNS_ERROR_NODE_CREATION_FAILED = DWORD(9703);

// DNS_ERROR_UNKNOWN_RECORD_TYPE         0x000025e8
//
// MessageId: DNS_ERROR_UNKNOWN_RECORD_TYPE
//
// MessageText:
//
//  Unknown DNS record type.
//
  DNS_ERROR_UNKNOWN_RECORD_TYPE = DWORD(9704);

// DNS_ERROR_RECORD_TIMED_OUT            0x000025e9
//
// MessageId: DNS_ERROR_RECORD_TIMED_OUT
//
// MessageText:
//
//  DNS record timed out.
//
  DNS_ERROR_RECORD_TIMED_OUT = DWORD(9705);

// DNS_ERROR_NAME_NOT_IN_ZONE            0x000025ea
//
// MessageId: DNS_ERROR_NAME_NOT_IN_ZONE
//
// MessageText:
//
//  Name not in DNS zone.
//
  DNS_ERROR_NAME_NOT_IN_ZONE = DWORD(9706);

// DNS_ERROR_CNAME_LOOP                  0x000025eb
//
// MessageId: DNS_ERROR_CNAME_LOOP
//
// MessageText:
//
//  CNAME loop detected.
//
  DNS_ERROR_CNAME_LOOP = DWORD(9707);

// DNS_ERROR_NODE_IS_CNAME               0x000025ec
//
// MessageId: DNS_ERROR_NODE_IS_CNAME
//
// MessageText:
//
//  Node is a CNAME DNS record.
//
  DNS_ERROR_NODE_IS_CNAME = DWORD(9708);

// DNS_ERROR_CNAME_COLLISION             0x000025ed
//
// MessageId: DNS_ERROR_CNAME_COLLISION
//
// MessageText:
//
//  A CNAME record already exists for given name.
//
  DNS_ERROR_CNAME_COLLISION = DWORD(9709);

// DNS_ERROR_RECORD_ONLY_AT_ZONE_ROOT    0x000025ee
//
// MessageId: DNS_ERROR_RECORD_ONLY_AT_ZONE_ROOT
//
// MessageText:
//
//  Record only at DNS zone root.
//
  DNS_ERROR_RECORD_ONLY_AT_ZONE_ROOT = DWORD(9710);

// DNS_ERROR_RECORD_ALREADY_EXISTS       0x000025ef
//
// MessageId: DNS_ERROR_RECORD_ALREADY_EXISTS
//
// MessageText:
//
//  DNS record already exists.
//
  DNS_ERROR_RECORD_ALREADY_EXISTS = DWORD(9711);

// DNS_ERROR_SECONDARY_DATA              0x000025f0
//
// MessageId: DNS_ERROR_SECONDARY_DATA
//
// MessageText:
//
//  Secondary DNS zone data error.
//
  DNS_ERROR_SECONDARY_DATA = DWORD(9712);

// DNS_ERROR_NO_CREATE_CACHE_DATA        0x000025f1
//
// MessageId: DNS_ERROR_NO_CREATE_CACHE_DATA
//
// MessageText:
//
//  Could not create DNS cache data.
//
  DNS_ERROR_NO_CREATE_CACHE_DATA = DWORD(9713);

// DNS_ERROR_NAME_DOES_NOT_EXIST         0x000025f2
//
// MessageId: DNS_ERROR_NAME_DOES_NOT_EXIST
//
// MessageText:
//
//  DNS name does not exist.
//
  DNS_ERROR_NAME_DOES_NOT_EXIST = DWORD(9714);

// DNS_WARNING_PTR_CREATE_FAILED         0x000025f3
//
// MessageId: DNS_WARNING_PTR_CREATE_FAILED
//
// MessageText:
//
//  Could not create pointer (PTR) record.
//
  DNS_WARNING_PTR_CREATE_FAILED = DWORD(9715);

// DNS_WARNING_DOMAIN_UNDELETED          0x000025f4
//
// MessageId: DNS_WARNING_DOMAIN_UNDELETED
//
// MessageText:
//
//  DNS domain was undeleted.
//
  DNS_WARNING_DOMAIN_UNDELETED = DWORD(9716);

// DNS_ERROR_DS_UNAVAILABLE              0x000025f5
//
// MessageId: DNS_ERROR_DS_UNAVAILABLE
//
// MessageText:
//
//  The directory service is unavailable.
//
  DNS_ERROR_DS_UNAVAILABLE = DWORD(9717);

// DNS_ERROR_DS_ZONE_ALREADY_EXISTS      0x000025f6
//
// MessageId: DNS_ERROR_DS_ZONE_ALREADY_EXISTS
//
// MessageText:
//
//  DNS zone already exists in the directory service.
//
  DNS_ERROR_DS_ZONE_ALREADY_EXISTS = DWORD(9718);

// DNS_ERROR_NO_BOOTFILE_IF_DS_ZONE      0x000025f7
//
// MessageId: DNS_ERROR_NO_BOOTFILE_IF_DS_ZONE
//
// MessageText:
//
//  DNS server not creating or reading the boot file for the directory service integrated DNS zone.
//
  DNS_ERROR_NO_BOOTFILE_IF_DS_ZONE = DWORD(9719);


//
//  Operation errors
//

  DNS_ERROR_OPERATION_BASE = 9750;

// DNS_INFO_AXFR_COMPLETE                0x00002617
//
// MessageId: DNS_INFO_AXFR_COMPLETE
//
// MessageText:
//
//  DNS AXFR (zone transfer) complete.
//
  DNS_INFO_AXFR_COMPLETE = DWORD(9751);

// DNS_ERROR_AXFR                        0x00002618
//
// MessageId: DNS_ERROR_AXFR
//
// MessageText:
//
//  DNS zone transfer failed.
//
  DNS_ERROR_AXFR = DWORD(9752);

// DNS_INFO_ADDED_LOCAL_WINS             0x00002619
//
// MessageId: DNS_INFO_ADDED_LOCAL_WINS
//
// MessageText:
//
//  Added local WINS server.
//
  DNS_INFO_ADDED_LOCAL_WINS = DWORD(9753);


//
//  Secure update
//

  DNS_ERROR_SECURE_BASE = 9800;

// DNS_STATUS_CONTINUE_NEEDED            0x00002649
//
// MessageId: DNS_STATUS_CONTINUE_NEEDED
//
// MessageText:
//
//  Secure update call needs to continue update request.
//
  DNS_STATUS_CONTINUE_NEEDED = DWORD(9801);


//
//  Setup errors
//

  DNS_ERROR_SETUP_BASE = 9850;

// DNS_ERROR_NO_TCPIP                    0x0000267b
//
// MessageId: DNS_ERROR_NO_TCPIP
//
// MessageText:
//
//  TCP/IP network protocol not installed.
//
  DNS_ERROR_NO_TCPIP = DWORD(9851);

// DNS_ERROR_NO_DNS_SERVERS              0x0000267c
//
// MessageId: DNS_ERROR_NO_DNS_SERVERS
//
// MessageText:
//
//  No DNS servers configured for local system.
//
  DNS_ERROR_NO_DNS_SERVERS = DWORD(9852);


//
//  Directory partition (DP) errors
//

  DNS_ERROR_DP_BASE = 9900;

// DNS_ERROR_DP_DOES_NOT_EXIST           0x000026ad
//
// MessageId: DNS_ERROR_DP_DOES_NOT_EXIST
//
// MessageText:
//
//  The specified directory partition does not exist.
//
  DNS_ERROR_DP_DOES_NOT_EXIST = DWORD(9901);

// DNS_ERROR_DP_ALREADY_EXISTS           0x000026ae
//
// MessageId: DNS_ERROR_DP_ALREADY_EXISTS
//
// MessageText:
//
//  The specified directory partition already exists.
//
  DNS_ERROR_DP_ALREADY_EXISTS = DWORD(9902);

// DNS_ERROR_DP_NOT_ENLISTED             0x000026af
//
// MessageId: DNS_ERROR_DP_NOT_ENLISTED
//
// MessageText:
//
//  This DNS server is not enlisted in the specified directory partition.
//
  DNS_ERROR_DP_NOT_ENLISTED = DWORD(9903);

// DNS_ERROR_DP_ALREADY_ENLISTED         0x000026b0
//
// MessageId: DNS_ERROR_DP_ALREADY_ENLISTED
//
// MessageText:
//
//  This DNS server is already enlisted in the specified directory partition.
//
  DNS_ERROR_DP_ALREADY_ENLISTED = DWORD(9904);

// DNS_ERROR_DP_NOT_AVAILABLE            0x000026b1
//
// MessageId: DNS_ERROR_DP_NOT_AVAILABLE
//
// MessageText:
//
//  The directory partition is not available at this time. Please wait
//  a few minutes and try again.
//
  DNS_ERROR_DP_NOT_AVAILABLE = DWORD(9905);

// DNS_ERROR_DP_FSMO_ERROR               0x000026b2
//
// MessageId: DNS_ERROR_DP_FSMO_ERROR
//
// MessageText:
//
//  The application directory partition operation failed. The domain controller
//  holding the domain naming master role is down or unable to service the
//  request or is not running Windows Server 2003.
//
  DNS_ERROR_DP_FSMO_ERROR = DWORD(9906);

///////////////////////////////////////////////////
//                                               //
//             End of DNS Error Codes            //
//                                               //
//                  9000 to 9999                 //
///////////////////////////////////////////////////


///////////////////////////////////////////////////
//                                               //
//               WinSock Error Codes             //
//                                               //
//                 10000 to 11999                //
///////////////////////////////////////////////////

//
// WinSock error codes are also defined in WinSock.h
// and WinSock2.h, hence the IFDEF
//
  WSABASEERR = 10000;
//
// MessageId: WSAEINTR
//
// MessageText:
//
//  A blocking operation was interrupted by a call to WSACancelBlockingCall.
//
  WSAEINTR = DWORD(10004);

//
// MessageId: WSAEBADF
//
// MessageText:
//
//  The file handle supplied is not valid.
//
  WSAEBADF = DWORD(10009);

//
// MessageId: WSAEACCES
//
// MessageText:
//
//  An attempt was made to access a socket in a way forbidden by its access permissions.
//
  WSAEACCES = DWORD(10013);

//
// MessageId: WSAEFAULT
//
// MessageText:
//
//  The system detected an invalid pointer address in attempting to use a pointer argument in a call.
//
  WSAEFAULT = DWORD(10014);

//
// MessageId: WSAEINVAL
//
// MessageText:
//
//  An invalid argument was supplied.
//
  WSAEINVAL = DWORD(10022);

//
// MessageId: WSAEMFILE
//
// MessageText:
//
//  Too many open sockets.
//
  WSAEMFILE = DWORD(10024);

//
// MessageId: WSAEWOULDBLOCK
//
// MessageText:
//
//  A non-blocking socket operation could not be completed immediately.
//
  WSAEWOULDBLOCK = DWORD(10035);

//
// MessageId: WSAEINPROGRESS
//
// MessageText:
//
//  A blocking operation is currently executing.
//
  WSAEINPROGRESS = DWORD(10036);

//
// MessageId: WSAEALREADY
//
// MessageText:
//
//  An operation was attempted on a non-blocking socket that already had an operation in progress.
//
  WSAEALREADY = DWORD(10037);

//
// MessageId: WSAENOTSOCK
//
// MessageText:
//
//  An operation was attempted on something that is not a socket.
//
  WSAENOTSOCK = DWORD(10038);

//
// MessageId: WSAEDESTADDRREQ
//
// MessageText:
//
//  A required address was omitted from an operation on a socket.
//
  WSAEDESTADDRREQ = DWORD(10039);

//
// MessageId: WSAEMSGSIZE
//
// MessageText:
//
//  A message sent on a datagram socket was larger than the internal message buffer or some other network limit, or the buffer used to receive a datagram into was smaller than the datagram itself.
//
  WSAEMSGSIZE = DWORD(10040);

//
// MessageId: WSAEPROTOTYPE
//
// MessageText:
//
//  A protocol was specified in the socket function call that does not support the semantics of the socket type requested.
//
  WSAEPROTOTYPE = DWORD(10041);

//
// MessageId: WSAENOPROTOOPT
//
// MessageText:
//
//  An unknown, invalid, or unsupported option or level was specified in a getsockopt or setsockopt call.
//
  WSAENOPROTOOPT = DWORD(10042);

//
// MessageId: WSAEPROTONOSUPPORT
//
// MessageText:
//
//  The requested protocol has not been configured into the system, or no implementation for it exists.
//
  WSAEPROTONOSUPPORT = DWORD(10043);

//
// MessageId: WSAESOCKTNOSUPPORT
//
// MessageText:
//
//  The support for the specified socket type does not exist in this address family.
//
  WSAESOCKTNOSUPPORT = DWORD(10044);

//
// MessageId: WSAEOPNOTSUPP
//
// MessageText:
//
//  The attempted operation is not supported for the type of object referenced.
//
  WSAEOPNOTSUPP = DWORD(10045);

//
// MessageId: WSAEPFNOSUPPORT
//
// MessageText:
//
//  The protocol family has not been configured into the system or no implementation for it exists.
//
  WSAEPFNOSUPPORT = DWORD(10046);

//
// MessageId: WSAEAFNOSUPPORT
//
// MessageText:
//
//  An address incompatible with the requested protocol was used.
//
  WSAEAFNOSUPPORT = DWORD(10047);

//
// MessageId: WSAEADDRINUSE
//
// MessageText:
//
//  Only one usage of each socket address (protocol/network address/port) is normally permitted.
//
  WSAEADDRINUSE = DWORD(10048);

//
// MessageId: WSAEADDRNOTAVAIL
//
// MessageText:
//
//  The requested address is not valid in its context.
//
  WSAEADDRNOTAVAIL = DWORD(10049);

//
// MessageId: WSAENETDOWN
//
// MessageText:
//
//  A socket operation encountered a dead network.
//
  WSAENETDOWN = DWORD(10050);

//
// MessageId: WSAENETUNREACH
//
// MessageText:
//
//  A socket operation was attempted to an unreachable network.
//
  WSAENETUNREACH = DWORD(10051);

//
// MessageId: WSAENETRESET
//
// MessageText:
//
//  The connection has been broken due to keep-alive activity detecting a failure while the operation was in progress.
//
  WSAENETRESET = DWORD(10052);

//
// MessageId: WSAECONNABORTED
//
// MessageText:
//
//  An established connection was aborted by the software in your host machine.
//
  WSAECONNABORTED = DWORD(10053);

//
// MessageId: WSAECONNRESET
//
// MessageText:
//
//  An existing connection was forcibly closed by the remote host.
//
  WSAECONNRESET = DWORD(10054);

//
// MessageId: WSAENOBUFS
//
// MessageText:
//
//  An operation on a socket could not be performed because the system lacked sufficient buffer space or because a queue was full.
//
  WSAENOBUFS = DWORD(10055);

//
// MessageId: WSAEISCONN
//
// MessageText:
//
//  A connect request was made on an already connected socket.
//
  WSAEISCONN = DWORD(10056);

//
// MessageId: WSAENOTCONN
//
// MessageText:
//
//  A request to send or receive data was disallowed because the socket is not connected and (when sending on a datagram socket using a sendto call) no address was supplied.
//
  WSAENOTCONN = DWORD(10057);

//
// MessageId: WSAESHUTDOWN
//
// MessageText:
//
//  A request to send or receive data was disallowed because the socket had already been shut down in that direction with a previous shutdown call.
//
  WSAESHUTDOWN = DWORD(10058);

//
// MessageId: WSAETOOMANYREFS
//
// MessageText:
//
//  Too many references to some kernel object.
//
  WSAETOOMANYREFS = DWORD(10059);

//
// MessageId: WSAETIMEDOUT
//
// MessageText:
//
//  A connection attempt failed because the connected party did not properly respond after a period of time, or established connection failed because connected host has failed to respond.
//
  WSAETIMEDOUT = DWORD(10060);

//
// MessageId: WSAECONNREFUSED
//
// MessageText:
//
//  No connection could be made because the target machine actively refused it.
//
  WSAECONNREFUSED = DWORD(10061);

//
// MessageId: WSAELOOP
//
// MessageText:
//
//  Cannot translate name.
//
  WSAELOOP = DWORD(10062);

//
// MessageId: WSAENAMETOOLONG
//
// MessageText:
//
//  Name component or name was too long.
//
  WSAENAMETOOLONG = DWORD(10063);

//
// MessageId: WSAEHOSTDOWN
//
// MessageText:
//
//  A socket operation failed because the destination host was down.
//
  WSAEHOSTDOWN = DWORD(10064);

//
// MessageId: WSAEHOSTUNREACH
//
// MessageText:
//
//  A socket operation was attempted to an unreachable host.
//
  WSAEHOSTUNREACH = DWORD(10065);

//
// MessageId: WSAENOTEMPTY
//
// MessageText:
//
//  Cannot remove a directory that is not empty.
//
  WSAENOTEMPTY = DWORD(10066);

//
// MessageId: WSAEPROCLIM
//
// MessageText:
//
//  A Windows Sockets implementation may have a limit on the number of applications that may use it simultaneously.
//
  WSAEPROCLIM = DWORD(10067);

//
// MessageId: WSAEUSERS
//
// MessageText:
//
//  Ran out of quota.
//
  WSAEUSERS = DWORD(10068);

//
// MessageId: WSAEDQUOT
//
// MessageText:
//
//  Ran out of disk quota.
//
  WSAEDQUOT = DWORD(10069);

//
// MessageId: WSAESTALE
//
// MessageText:
//
//  File handle reference is no longer available.
//
  WSAESTALE = DWORD(10070);

//
// MessageId: WSAEREMOTE
//
// MessageText:
//
//  Item is not available locally.
//
  WSAEREMOTE = DWORD(10071);

//
// MessageId: WSASYSNOTREADY
//
// MessageText:
//
//  WSAStartup cannot function at this time because the underlying system it uses to provide network services is currently unavailable.
//
  WSASYSNOTREADY = DWORD(10091);

//
// MessageId: WSAVERNOTSUPPORTED
//
// MessageText:
//
//  The Windows Sockets version requested is not supported.
//
  WSAVERNOTSUPPORTED = DWORD(10092);

//
// MessageId: WSANOTINITIALISED
//
// MessageText:
//
//  Either the application has not called WSAStartup, or WSAStartup failed.
//
  WSANOTINITIALISED = DWORD(10093);

//
// MessageId: WSAEDISCON
//
// MessageText:
//
//  Returned by WSARecv or WSARecvFrom to indicate the remote party has initiated a graceful shutdown sequence.
//
  WSAEDISCON = DWORD(10101);

//
// MessageId: WSAENOMORE
//
// MessageText:
//
//  No more results can be returned by WSALookupServiceNext.
//
  WSAENOMORE = DWORD(10102);

//
// MessageId: WSAECANCELLED
//
// MessageText:
//
//  A call to WSALookupServiceEnd was made while this call was still processing. The call has been canceled.
//
  WSAECANCELLED = DWORD(10103);

//
// MessageId: WSAEINVALIDPROCTABLE
//
// MessageText:
//
//  The procedure call table is invalid.
//
  WSAEINVALIDPROCTABLE = DWORD(10104);

//
// MessageId: WSAEINVALIDPROVIDER
//
// MessageText:
//
//  The requested service provider is invalid.
//
  WSAEINVALIDPROVIDER = DWORD(10105);

//
// MessageId: WSAEPROVIDERFAILEDINIT
//
// MessageText:
//
//  The requested service provider could not be loaded or initialized.
//
  WSAEPROVIDERFAILEDINIT = DWORD(10106);

//
// MessageId: WSASYSCALLFAILURE
//
// MessageText:
//
//  A system call that should never fail has failed.
//
  WSASYSCALLFAILURE = DWORD(10107);

//
// MessageId: WSASERVICE_NOT_FOUND
//
// MessageText:
//
//  No such service is known. The service cannot be found in the specified name space.
//
  WSASERVICE_NOT_FOUND = DWORD(10108);

//
// MessageId: WSATYPE_NOT_FOUND
//
// MessageText:
//
//  The specified class was not found.
//
  WSATYPE_NOT_FOUND = DWORD(10109);

//
// MessageId: WSA_E_NO_MORE
//
// MessageText:
//
//  No more results can be returned by WSALookupServiceNext.
//
  WSA_E_NO_MORE = DWORD(10110);

//
// MessageId: WSA_E_CANCELLED
//
// MessageText:
//
//  A call to WSALookupServiceEnd was made while this call was still processing. The call has been canceled.
//
  WSA_E_CANCELLED = DWORD(10111);

//
// MessageId: WSAEREFUSED
//
// MessageText:
//
//  A database query failed because it was actively refused.
//
  WSAEREFUSED = DWORD(10112);

//
// MessageId: WSAHOST_NOT_FOUND
//
// MessageText:
//
//  No such host is known.
//
  WSAHOST_NOT_FOUND = DWORD(11001);

//
// MessageId: WSATRY_AGAIN
//
// MessageText:
//
//  This is usually a temporary error during hostname resolution and means that the local server did not receive a response from an authoritative server.
//
  WSATRY_AGAIN = DWORD(11002);

//
// MessageId: WSANO_RECOVERY
//
// MessageText:
//
//  A non-recoverable error occurred during a database lookup.
//
  WSANO_RECOVERY = DWORD(11003);

//
// MessageId: WSANO_DATA
//
// MessageText:
//
//  The requested name is valid, but no data of the requested type was found.
//
  WSANO_DATA = DWORD(11004);

//
// MessageId: WSA_QOS_RECEIVERS
//
// MessageText:
//
//  At least one reserve has arrived.
//
  WSA_QOS_RECEIVERS = DWORD(11005);

//
// MessageId: WSA_QOS_SENDERS
//
// MessageText:
//
//  At least one path has arrived.
//
  WSA_QOS_SENDERS = DWORD(11006);

//
// MessageId: WSA_QOS_NO_SENDERS
//
// MessageText:
//
//  There are no senders.
//
  WSA_QOS_NO_SENDERS = DWORD(11007);

//
// MessageId: WSA_QOS_NO_RECEIVERS
//
// MessageText:
//
//  There are no receivers.
//
  WSA_QOS_NO_RECEIVERS = DWORD(11008);

//
// MessageId: WSA_QOS_REQUEST_CONFIRMED
//
// MessageText:
//
//  Reserve has been confirmed.
//
  WSA_QOS_REQUEST_CONFIRMED = DWORD(11009);

//
// MessageId: WSA_QOS_ADMISSION_FAILURE
//
// MessageText:
//
//  Error due to lack of resources.
//
  WSA_QOS_ADMISSION_FAILURE = DWORD(11010);

//
// MessageId: WSA_QOS_POLICY_FAILURE
//
// MessageText:
//
//  Rejected for administrative reasons - bad credentials.
//
  WSA_QOS_POLICY_FAILURE = DWORD(11011);

//
// MessageId: WSA_QOS_BAD_STYLE
//
// MessageText:
//
//  Unknown or conflicting style.
//
  WSA_QOS_BAD_STYLE = DWORD(11012);

//
// MessageId: WSA_QOS_BAD_OBJECT
//
// MessageText:
//
//  Problem with some part of the filterspec or providerspecific buffer in general.
//
  WSA_QOS_BAD_OBJECT = DWORD(11013);

//
// MessageId: WSA_QOS_TRAFFIC_CTRL_ERROR
//
// MessageText:
//
//  Problem with some part of the flowspec.
//
  WSA_QOS_TRAFFIC_CTRL_ERROR = DWORD(11014);

//
// MessageId: WSA_QOS_GENERIC_ERROR
//
// MessageText:
//
//  General QOS error.
//
  WSA_QOS_GENERIC_ERROR = DWORD(11015);

//
// MessageId: WSA_QOS_ESERVICETYPE
//
// MessageText:
//
//  An invalid or unrecognized service type was found in the flowspec.
//
  WSA_QOS_ESERVICETYPE = DWORD(11016);

//
// MessageId: WSA_QOS_EFLOWSPEC
//
// MessageText:
//
//  An invalid or inconsistent flowspec was found in the QOS structure.
//
  WSA_QOS_EFLOWSPEC = DWORD(11017);

//
// MessageId: WSA_QOS_EPROVSPECBUF
//
// MessageText:
//
//  Invalid QOS provider-specific buffer.
//
  WSA_QOS_EPROVSPECBUF = DWORD(11018);

//
// MessageId: WSA_QOS_EFILTERSTYLE
//
// MessageText:
//
//  An invalid QOS filter style was used.
//
  WSA_QOS_EFILTERSTYLE = DWORD(11019);

//
// MessageId: WSA_QOS_EFILTERTYPE
//
// MessageText:
//
//  An invalid QOS filter type was used.
//
  WSA_QOS_EFILTERTYPE = DWORD(11020);

//
// MessageId: WSA_QOS_EFILTERCOUNT
//
// MessageText:
//
//  An incorrect number of QOS FILTERSPECs were specified in the FLOWDESCRIPTOR.
//
  WSA_QOS_EFILTERCOUNT = DWORD(11021);

//
// MessageId: WSA_QOS_EOBJLENGTH
//
// MessageText:
//
//  An object with an invalid ObjectLength field was specified in the QOS provider-specific buffer.
//
  WSA_QOS_EOBJLENGTH = DWORD(11022);

//
// MessageId: WSA_QOS_EFLOWCOUNT
//
// MessageText:
//
//  An incorrect number of flow descriptors was specified in the QOS structure.
//
  WSA_QOS_EFLOWCOUNT = DWORD(11023);

//
// MessageId: WSA_QOS_EUNKOWNPSOBJ
//
// MessageText:
//
//  An unrecognized object was found in the QOS provider-specific buffer.
//
  WSA_QOS_EUNKOWNPSOBJ = DWORD(11024);

//
// MessageId: WSA_QOS_EPOLICYOBJ
//
// MessageText:
//
//  An invalid policy object was found in the QOS provider-specific buffer.
//
  WSA_QOS_EPOLICYOBJ = DWORD(11025);

//
// MessageId: WSA_QOS_EFLOWDESC
//
// MessageText:
//
//  An invalid QOS flow descriptor was found in the flow descriptor list.
//
  WSA_QOS_EFLOWDESC = DWORD(11026);

//
// MessageId: WSA_QOS_EPSFLOWSPEC
//
// MessageText:
//
//  An invalid or inconsistent flowspec was found in the QOS provider specific buffer.
//
  WSA_QOS_EPSFLOWSPEC = DWORD(11027);

//
// MessageId: WSA_QOS_EPSFILTERSPEC
//
// MessageText:
//
//  An invalid FILTERSPEC was found in the QOS provider-specific buffer.
//
  WSA_QOS_EPSFILTERSPEC = DWORD(11028);

//
// MessageId: WSA_QOS_ESDMODEOBJ
//
// MessageText:
//
//  An invalid shape discard mode object was found in the QOS provider specific buffer.
//
  WSA_QOS_ESDMODEOBJ = DWORD(11029);

//
// MessageId: WSA_QOS_ESHAPERATEOBJ
//
// MessageText:
//
//  An invalid shaping rate object was found in the QOS provider-specific buffer.
//
  WSA_QOS_ESHAPERATEOBJ = DWORD(11030);

//
// MessageId: WSA_QOS_RESERVED_PETYPE
//
// MessageText:
//
//  A reserved policy element was found in the QOS provider-specific buffer.
//
  WSA_QOS_RESERVED_PETYPE = DWORD(11031);

///////////////////////////////////////////////////
//                                               //
//           End of WinSock Error Codes          //
//                                               //
//                 10000 to 11999                //
///////////////////////////////////////////////////



///////////////////////////////////////////////////
//                                               //
//             Side By Side Error Codes          //
//                                               //
//                 14000 to 14999                //
///////////////////////////////////////////////////

//
// MessageId: ERROR_SXS_SECTION_NOT_FOUND
//
// MessageText:
//
//  The requested section was not present in the activation context.
//
  ERROR_SXS_SECTION_NOT_FOUND = DWORD(14000);

//
// MessageId: ERROR_SXS_CANT_GEN_ACTCTX
//
// MessageText:
//
//  This application has failed to start because the application configuration is incorrect. Reinstalling the application may fix this problem.
//
  ERROR_SXS_CANT_GEN_ACTCTX = DWORD(14001);

//
// MessageId: ERROR_SXS_INVALID_ACTCTXDATA_FORMAT
//
// MessageText:
//
//  The application binding data format is invalid.
//
  ERROR_SXS_INVALID_ACTCTXDATA_FORMAT = DWORD(14002);

//
// MessageId: ERROR_SXS_ASSEMBLY_NOT_FOUND
//
// MessageText:
//
//  The referenced assembly is not installed on your system.
//
  ERROR_SXS_ASSEMBLY_NOT_FOUND = DWORD(14003);

//
// MessageId: ERROR_SXS_MANIFEST_FORMAT_ERROR
//
// MessageText:
//
//  The manifest file does not begin with the required tag and format information.
//
  ERROR_SXS_MANIFEST_FORMAT_ERROR = DWORD(14004);

//
// MessageId: ERROR_SXS_MANIFEST_PARSE_ERROR
//
// MessageText:
//
//  The manifest file contains one or more syntax errors.
//
  ERROR_SXS_MANIFEST_PARSE_ERROR = DWORD(14005);

//
// MessageId: ERROR_SXS_ACTIVATION_CONTEXT_DISABLED
//
// MessageText:
//
//  The application attempted to activate a disabled activation context.
//
  ERROR_SXS_ACTIVATION_CONTEXT_DISABLED = DWORD(14006);

//
// MessageId: ERROR_SXS_KEY_NOT_FOUND
//
// MessageText:
//
//  The requested lookup key was not found in any active activation context.
//
  ERROR_SXS_KEY_NOT_FOUND = DWORD(14007);

//
// MessageId: ERROR_SXS_VERSION_CONFLICT
//
// MessageText:
//
//  A component version required by the application conflicts with another component version already active.
//
  ERROR_SXS_VERSION_CONFLICT = DWORD(14008);

//
// MessageId: ERROR_SXS_WRONG_SECTION_TYPE
//
// MessageText:
//
//  The type requested activation context section does not match the query API used.
//
  ERROR_SXS_WRONG_SECTION_TYPE = DWORD(14009);

//
// MessageId: ERROR_SXS_THREAD_QUERIES_DISABLED
//
// MessageText:
//
//  Lack of system resources has required isolated activation to be disabled for the current thread of execution.
//
  ERROR_SXS_THREAD_QUERIES_DISABLED = DWORD(14010);

//
// MessageId: ERROR_SXS_PROCESS_DEFAULT_ALREADY_SET
//
// MessageText:
//
//  An attempt to set the process default activation context failed because the process default activation context was already set.
//
  ERROR_SXS_PROCESS_DEFAULT_ALREADY_SET = DWORD(14011);

//
// MessageId: ERROR_SXS_UNKNOWN_ENCODING_GROUP
//
// MessageText:
//
//  The encoding group identifier specified is not recognized.
//
  ERROR_SXS_UNKNOWN_ENCODING_GROUP = DWORD(14012);

//
// MessageId: ERROR_SXS_UNKNOWN_ENCODING
//
// MessageText:
//
//  The encoding requested is not recognized.
//
  ERROR_SXS_UNKNOWN_ENCODING = DWORD(14013);

//
// MessageId: ERROR_SXS_INVALID_XML_NAMESPACE_URI
//
// MessageText:
//
//  The manifest contains a reference to an invalid URI.
//
  ERROR_SXS_INVALID_XML_NAMESPACE_URI = DWORD(14014);

//
// MessageId: ERROR_SXS_ROOT_MANIFEST_DEPENDENCY_NOT_INSTALLED
//
// MessageText:
//
//  The application manifest contains a reference to a dependent assembly which is not installed
//
  ERROR_SXS_ROOT_MANIFEST_DEPENDENCY_NOT_INSTALLED = DWORD(14015);

//
// MessageId: ERROR_SXS_LEAF_MANIFEST_DEPENDENCY_NOT_INSTALLED
//
// MessageText:
//
//  The manifest for an assembly used by the application has a reference to a dependent assembly which is not installed
//
  ERROR_SXS_LEAF_MANIFEST_DEPENDENCY_NOT_INSTALLED = DWORD(14016);

//
// MessageId: ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE
//
// MessageText:
//
//  The manifest contains an attribute for the assembly identity which is not valid.
//
  ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE = DWORD(14017);

//
// MessageId: ERROR_SXS_MANIFEST_MISSING_REQUIRED_DEFAULT_NAMESPACE
//
// MessageText:
//
//  The manifest is missing the required default namespace specification on the assembly element.
//
  ERROR_SXS_MANIFEST_MISSING_REQUIRED_DEFAULT_NAMESPACE = DWORD(14018);

//
// MessageId: ERROR_SXS_MANIFEST_INVALID_REQUIRED_DEFAULT_NAMESPACE
//
// MessageText:
//
//  The manifest has a default namespace specified on the assembly element but its value is not "urn:schemas-microsoft-com:asm.v1".
//
  ERROR_SXS_MANIFEST_INVALID_REQUIRED_DEFAULT_NAMESPACE = DWORD(14019);

//
// MessageId: ERROR_SXS_PRIVATE_MANIFEST_CROSS_PATH_WITH_REPARSE_POINT
//
// MessageText:
//
//  The private manifest probed has crossed reparse-point-associated path
//
  ERROR_SXS_PRIVATE_MANIFEST_CROSS_PATH_WITH_REPARSE_POINT = DWORD(14020);

//
// MessageId: ERROR_SXS_DUPLICATE_DLL_NAME
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest have files by the same name.
//
  ERROR_SXS_DUPLICATE_DLL_NAME = DWORD(14021);

//
// MessageId: ERROR_SXS_DUPLICATE_WINDOWCLASS_NAME
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest have window classes with the same name.
//
  ERROR_SXS_DUPLICATE_WINDOWCLASS_NAME = DWORD(14022);

//
// MessageId: ERROR_SXS_DUPLICATE_CLSID
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest have the same COM server CLSIDs.
//
  ERROR_SXS_DUPLICATE_CLSID = DWORD(14023);

//
// MessageId: ERROR_SXS_DUPLICATE_IID
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest have proxies for the same COM interface IIDs.
//
  ERROR_SXS_DUPLICATE_IID = DWORD(14024);

//
// MessageId: ERROR_SXS_DUPLICATE_TLBID
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest have the same COM type library TLBIDs.
//
  ERROR_SXS_DUPLICATE_TLBID = DWORD(14025);

//
// MessageId: ERROR_SXS_DUPLICATE_PROGID
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest have the same COM ProgIDs.
//
  ERROR_SXS_DUPLICATE_PROGID = DWORD(14026);

//
// MessageId: ERROR_SXS_DUPLICATE_ASSEMBLY_NAME
//
// MessageText:
//
//  Two or more components referenced directly or indirectly by the application manifest are different versions of the same component which is not permitted.
//
  ERROR_SXS_DUPLICATE_ASSEMBLY_NAME = DWORD(14027);

//
// MessageId: ERROR_SXS_FILE_HASH_MISMATCH
//
// MessageText:
//
//  A component's file does not match the verification information present in the
//  component manifest.
//
  ERROR_SXS_FILE_HASH_MISMATCH = DWORD(14028);

//
// MessageId: ERROR_SXS_POLICY_PARSE_ERROR
//
// MessageText:
//
//  The policy manifest contains one or more syntax errors.
//
  ERROR_SXS_POLICY_PARSE_ERROR = DWORD(14029);

//
// MessageId: ERROR_SXS_XML_E_MISSINGQUOTE
//
// MessageText:
//
//  Manifest Parse Error : A string literal was expected, but no opening quote character was found.
//
  ERROR_SXS_XML_E_MISSINGQUOTE = DWORD(14030);

//
// MessageId: ERROR_SXS_XML_E_COMMENTSYNTAX
//
// MessageText:
//
//  Manifest Parse Error : Incorrect syntax was used in a comment.
//
  ERROR_SXS_XML_E_COMMENTSYNTAX = DWORD(14031);

//
// MessageId: ERROR_SXS_XML_E_BADSTARTNAMECHAR
//
// MessageText:
//
//  Manifest Parse Error : A name was started with an invalid character.
//
  ERROR_SXS_XML_E_BADSTARTNAMECHAR = DWORD(14032);

//
// MessageId: ERROR_SXS_XML_E_BADNAMECHAR
//
// MessageText:
//
//  Manifest Parse Error : A name contained an invalid character.
//
  ERROR_SXS_XML_E_BADNAMECHAR = DWORD(14033);

//
// MessageId: ERROR_SXS_XML_E_BADCHARINSTRING
//
// MessageText:
//
//  Manifest Parse Error : A string literal contained an invalid character.
//
  ERROR_SXS_XML_E_BADCHARINSTRING = DWORD(14034);

//
// MessageId: ERROR_SXS_XML_E_XMLDECLSYNTAX
//
// MessageText:
//
//  Manifest Parse Error : Invalid syntax for an xml declaration.
//
  ERROR_SXS_XML_E_XMLDECLSYNTAX = DWORD(14035);

//
// MessageId: ERROR_SXS_XML_E_BADCHARDATA
//
// MessageText:
//
//  Manifest Parse Error : An Invalid character was found in text content.
//
  ERROR_SXS_XML_E_BADCHARDATA = DWORD(14036);

//
// MessageId: ERROR_SXS_XML_E_MISSINGWHITESPACE
//
// MessageText:
//
//  Manifest Parse Error : Required white space was missing.
//
  ERROR_SXS_XML_E_MISSINGWHITESPACE = DWORD(14037);

//
// MessageId: ERROR_SXS_XML_E_EXPECTINGTAGEND
//
// MessageText:
//
//  Manifest Parse Error : The character '>' was expected.
//
  ERROR_SXS_XML_E_EXPECTINGTAGEND = DWORD(14038);

//
// MessageId: ERROR_SXS_XML_E_MISSINGSEMICOLON
//
// MessageText:
//
//  Manifest Parse Error : A semi colon character was expected.
//
  ERROR_SXS_XML_E_MISSINGSEMICOLON = DWORD(14039);

//
// MessageId: ERROR_SXS_XML_E_UNBALANCEDPAREN
//
// MessageText:
//
//  Manifest Parse Error : Unbalanced parentheses.
//
  ERROR_SXS_XML_E_UNBALANCEDPAREN = DWORD(14040);

//
// MessageId: ERROR_SXS_XML_E_INTERNALERROR
//
// MessageText:
//
//  Manifest Parse Error : Internal error.
//
  ERROR_SXS_XML_E_INTERNALERROR = DWORD(14041);

//
// MessageId: ERROR_SXS_XML_E_UNEXPECTED_WHITESPACE
//
// MessageText:
//
//  Manifest Parse Error : Whitespace is not allowed at this location.
//
  ERROR_SXS_XML_E_UNEXPECTED_WHITESPACE = DWORD(14042);

//
// MessageId: ERROR_SXS_XML_E_INCOMPLETE_ENCODING
//
// MessageText:
//
//  Manifest Parse Error : End of file reached in invalid state for current encoding.
//
  ERROR_SXS_XML_E_INCOMPLETE_ENCODING = DWORD(14043);

//
// MessageId: ERROR_SXS_XML_E_MISSING_PAREN
//
// MessageText:
//
//  Manifest Parse Error : Missing parenthesis.
//
  ERROR_SXS_XML_E_MISSING_PAREN = DWORD(14044);

//
// MessageId: ERROR_SXS_XML_E_EXPECTINGCLOSEQUOTE
//
// MessageText:
//
//  Manifest Parse Error : A single or double closing quote character (\' or \") is missing.
//
  ERROR_SXS_XML_E_EXPECTINGCLOSEQUOTE = DWORD(14045);

//
// MessageId: ERROR_SXS_XML_E_MULTIPLE_COLONS
//
// MessageText:
//
//  Manifest Parse Error : Multiple colons are not allowed in a name.
//
  ERROR_SXS_XML_E_MULTIPLE_COLONS = DWORD(14046);

//
// MessageId: ERROR_SXS_XML_E_INVALID_DECIMAL
//
// MessageText:
//
//  Manifest Parse Error : Invalid character for decimal digit.
//
  ERROR_SXS_XML_E_INVALID_DECIMAL = DWORD(14047);

//
// MessageId: ERROR_SXS_XML_E_INVALID_HEXIDECIMAL
//
// MessageText:
//
//  Manifest Parse Error : Invalid character for hexidecimal digit.
//
  ERROR_SXS_XML_E_INVALID_HEXIDECIMAL = DWORD(14048);

//
// MessageId: ERROR_SXS_XML_E_INVALID_UNICODE
//
// MessageText:
//
//  Manifest Parse Error : Invalid unicode character value for this platform.
//
  ERROR_SXS_XML_E_INVALID_UNICODE = DWORD(14049);

//
// MessageId: ERROR_SXS_XML_E_WHITESPACEORQUESTIONMARK
//
// MessageText:
//
//  Manifest Parse Error : Expecting whitespace or '?'.
//
  ERROR_SXS_XML_E_WHITESPACEORQUESTIONMARK = DWORD(14050);

//
// MessageId: ERROR_SXS_XML_E_UNEXPECTEDENDTAG
//
// MessageText:
//
//  Manifest Parse Error : End tag was not expected at this location.
//
  ERROR_SXS_XML_E_UNEXPECTEDENDTAG = DWORD(14051);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDTAG
//
// MessageText:
//
//  Manifest Parse Error : The following tags were not closed: %1.
//
  ERROR_SXS_XML_E_UNCLOSEDTAG = DWORD(14052);

//
// MessageId: ERROR_SXS_XML_E_DUPLICATEATTRIBUTE
//
// MessageText:
//
//  Manifest Parse Error : Duplicate attribute.
//
  ERROR_SXS_XML_E_DUPLICATEATTRIBUTE = DWORD(14053);

//
// MessageId: ERROR_SXS_XML_E_MULTIPLEROOTS
//
// MessageText:
//
//  Manifest Parse Error : Only one top level element is allowed in an XML document.
//
  ERROR_SXS_XML_E_MULTIPLEROOTS = DWORD(14054);

//
// MessageId: ERROR_SXS_XML_E_INVALIDATROOTLEVEL
//
// MessageText:
//
//  Manifest Parse Error : Invalid at the top level of the document.
//
  ERROR_SXS_XML_E_INVALIDATROOTLEVEL = DWORD(14055);

//
// MessageId: ERROR_SXS_XML_E_BADXMLDECL
//
// MessageText:
//
//  Manifest Parse Error : Invalid xml declaration.
//
  ERROR_SXS_XML_E_BADXMLDECL = DWORD(14056);

//
// MessageId: ERROR_SXS_XML_E_MISSINGROOT
//
// MessageText:
//
//  Manifest Parse Error : XML document must have a top level element.
//
  ERROR_SXS_XML_E_MISSINGROOT = DWORD(14057);

//
// MessageId: ERROR_SXS_XML_E_UNEXPECTEDEOF
//
// MessageText:
//
//  Manifest Parse Error : Unexpected end of file.
//
  ERROR_SXS_XML_E_UNEXPECTEDEOF = DWORD(14058);

//
// MessageId: ERROR_SXS_XML_E_BADPEREFINSUBSET
//
// MessageText:
//
//  Manifest Parse Error : Parameter entities cannot be used inside markup declarations in an internal subset.
//
  ERROR_SXS_XML_E_BADPEREFINSUBSET = DWORD(14059);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDSTARTTAG
//
// MessageText:
//
//  Manifest Parse Error : Element was not closed.
//
  ERROR_SXS_XML_E_UNCLOSEDSTARTTAG = DWORD(14060);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDENDTAG
//
// MessageText:
//
//  Manifest Parse Error : End element was missing the character '>'.
//
  ERROR_SXS_XML_E_UNCLOSEDENDTAG = DWORD(14061);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDSTRING
//
// MessageText:
//
//  Manifest Parse Error : A string literal was not closed.
//
  ERROR_SXS_XML_E_UNCLOSEDSTRING = DWORD(14062);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDCOMMENT
//
// MessageText:
//
//  Manifest Parse Error : A comment was not closed.
//
  ERROR_SXS_XML_E_UNCLOSEDCOMMENT = DWORD(14063);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDDECL
//
// MessageText:
//
//  Manifest Parse Error : A declaration was not closed.
//
  ERROR_SXS_XML_E_UNCLOSEDDECL = DWORD(14064);

//
// MessageId: ERROR_SXS_XML_E_UNCLOSEDCDATA
//
// MessageText:
//
//  Manifest Parse Error : A CDATA section was not closed.
//
  ERROR_SXS_XML_E_UNCLOSEDCDATA = DWORD(14065);

//
// MessageId: ERROR_SXS_XML_E_RESERVEDNAMESPACE
//
// MessageText:
//
//  Manifest Parse Error : The namespace prefix is not allowed to start with the reserved string "xml".
//
  ERROR_SXS_XML_E_RESERVEDNAMESPACE = DWORD(14066);

//
// MessageId: ERROR_SXS_XML_E_INVALIDENCODING
//
// MessageText:
//
//  Manifest Parse Error : System does not support the specified encoding.
//
  ERROR_SXS_XML_E_INVALIDENCODING = DWORD(14067);

//
// MessageId: ERROR_SXS_XML_E_INVALIDSWITCH
//
// MessageText:
//
//  Manifest Parse Error : Switch from current encoding to specified encoding not supported.
//
  ERROR_SXS_XML_E_INVALIDSWITCH = DWORD(14068);

//
// MessageId: ERROR_SXS_XML_E_BADXMLCASE
//
// MessageText:
//
//  Manifest Parse Error : The name 'xml' is reserved and must be lower case.
//
  ERROR_SXS_XML_E_BADXMLCASE = DWORD(14069);

//
// MessageId: ERROR_SXS_XML_E_INVALID_STANDALONE
//
// MessageText:
//
//  Manifest Parse Error : The standalone attribute must have the value 'yes' or 'no'.
//
  ERROR_SXS_XML_E_INVALID_STANDALONE = DWORD(14070);

//
// MessageId: ERROR_SXS_XML_E_UNEXPECTED_STANDALONE
//
// MessageText:
//
//  Manifest Parse Error : The standalone attribute cannot be used in external entities.
//
  ERROR_SXS_XML_E_UNEXPECTED_STANDALONE = DWORD(14071);

//
// MessageId: ERROR_SXS_XML_E_INVALID_VERSION
//
// MessageText:
//
//  Manifest Parse Error : Invalid version number.
//
  ERROR_SXS_XML_E_INVALID_VERSION = DWORD(14072);

//
// MessageId: ERROR_SXS_XML_E_MISSINGEQUALS
//
// MessageText:
//
//  Manifest Parse Error : Missing equals sign between attribute and attribute value.
//
  ERROR_SXS_XML_E_MISSINGEQUALS = DWORD(14073);

//
// MessageId: ERROR_SXS_PROTECTION_RECOVERY_FAILED
//
// MessageText:
//
//  Assembly Protection Error : Unable to recover the specified assembly.
//
  ERROR_SXS_PROTECTION_RECOVERY_FAILED = DWORD(14074);

//
// MessageId: ERROR_SXS_PROTECTION_PUBLIC_KEY_TOO_SHORT
//
// MessageText:
//
//  Assembly Protection Error : The public key for an assembly was too short to be allowed.
//
  ERROR_SXS_PROTECTION_PUBLIC_KEY_TOO_SHORT = DWORD(14075);

//
// MessageId: ERROR_SXS_PROTECTION_CATALOG_NOT_VALID
//
// MessageText:
//
//  Assembly Protection Error : The catalog for an assembly is not valid, or does not match the assembly's manifest.
//
  ERROR_SXS_PROTECTION_CATALOG_NOT_VALID = DWORD(14076);

//
// MessageId: ERROR_SXS_UNTRANSLATABLE_HRESULT
//
// MessageText:
//
//  An HRESULT could not be translated to a corresponding Win32 error code.
//
  ERROR_SXS_UNTRANSLATABLE_HRESULT = DWORD(14077);

//
// MessageId: ERROR_SXS_PROTECTION_CATALOG_FILE_MISSING
//
// MessageText:
//
//  Assembly Protection Error : The catalog for an assembly is missing.
//
  ERROR_SXS_PROTECTION_CATALOG_FILE_MISSING = DWORD(14078);

//
// MessageId: ERROR_SXS_MISSING_ASSEMBLY_IDENTITY_ATTRIBUTE
//
// MessageText:
//
//  The supplied assembly identity is missing one or more attributes which must be present in this context.
//
  ERROR_SXS_MISSING_ASSEMBLY_IDENTITY_ATTRIBUTE = DWORD(14079);

//
// MessageId: ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE_NAME
//
// MessageText:
//
//  The supplied assembly identity has one or more attribute names that contain characters not permitted in XML names.
//
  ERROR_SXS_INVALID_ASSEMBLY_IDENTITY_ATTRIBUTE_NAME = DWORD(14080);


///////////////////////////////////////////////////
//                                               //
//           End of Side By Side Error Codes     //
//                                               //
//                 14000 to 14999                //
///////////////////////////////////////////////////



///////////////////////////////////////////////////
//                                               //
//           Start of IPSec Error codes          //
//                                               //
//                 13000 to 13999                //
///////////////////////////////////////////////////


//
// MessageId: ERROR_IPSEC_QM_POLICY_EXISTS
//
// MessageText:
//
//  The specified quick mode policy already exists.
//
  ERROR_IPSEC_QM_POLICY_EXISTS = DWORD(13000);

//
// MessageId: ERROR_IPSEC_QM_POLICY_NOT_FOUND
//
// MessageText:
//
//  The specified quick mode policy was not found.
//
  ERROR_IPSEC_QM_POLICY_NOT_FOUND = DWORD(13001);

//
// MessageId: ERROR_IPSEC_QM_POLICY_IN_USE
//
// MessageText:
//
//  The specified quick mode policy is being used.
//
  ERROR_IPSEC_QM_POLICY_IN_USE = DWORD(13002);

//
// MessageId: ERROR_IPSEC_MM_POLICY_EXISTS
//
// MessageText:
//
//  The specified main mode policy already exists.
//
  ERROR_IPSEC_MM_POLICY_EXISTS = DWORD(13003);

//
// MessageId: ERROR_IPSEC_MM_POLICY_NOT_FOUND
//
// MessageText:
//
//  The specified main mode policy was not found
//
  ERROR_IPSEC_MM_POLICY_NOT_FOUND = DWORD(13004);

//
// MessageId: ERROR_IPSEC_MM_POLICY_IN_USE
//
// MessageText:
//
//  The specified main mode policy is being used.
//
  ERROR_IPSEC_MM_POLICY_IN_USE = DWORD(13005);

//
// MessageId: ERROR_IPSEC_MM_FILTER_EXISTS
//
// MessageText:
//
//  The specified main mode filter already exists.
//
  ERROR_IPSEC_MM_FILTER_EXISTS = DWORD(13006);

//
// MessageId: ERROR_IPSEC_MM_FILTER_NOT_FOUND
//
// MessageText:
//
//  The specified main mode filter was not found.
//
  ERROR_IPSEC_MM_FILTER_NOT_FOUND = DWORD(13007);

//
// MessageId: ERROR_IPSEC_TRANSPORT_FILTER_EXISTS
//
// MessageText:
//
//  The specified transport mode filter already exists.
//
  ERROR_IPSEC_TRANSPORT_FILTER_EXISTS = DWORD(13008);

//
// MessageId: ERROR_IPSEC_TRANSPORT_FILTER_NOT_FOUND
//
// MessageText:
//
//  The specified transport mode filter does not exist.
//
  ERROR_IPSEC_TRANSPORT_FILTER_NOT_FOUND = DWORD(13009);

//
// MessageId: ERROR_IPSEC_MM_AUTH_EXISTS
//
// MessageText:
//
//  The specified main mode authentication list exists.
//
  ERROR_IPSEC_MM_AUTH_EXISTS = DWORD(13010);

//
// MessageId: ERROR_IPSEC_MM_AUTH_NOT_FOUND
//
// MessageText:
//
//  The specified main mode authentication list was not found.
//
  ERROR_IPSEC_MM_AUTH_NOT_FOUND = DWORD(13011);

//
// MessageId: ERROR_IPSEC_MM_AUTH_IN_USE
//
// MessageText:
//
//  The specified quick mode policy is being used.
//
  ERROR_IPSEC_MM_AUTH_IN_USE = DWORD(13012);

//
// MessageId: ERROR_IPSEC_DEFAULT_MM_POLICY_NOT_FOUND
//
// MessageText:
//
//  The specified main mode policy was not found.
//
  ERROR_IPSEC_DEFAULT_MM_POLICY_NOT_FOUND = DWORD(13013);

//
// MessageId: ERROR_IPSEC_DEFAULT_MM_AUTH_NOT_FOUND
//
// MessageText:
//
//  The specified quick mode policy was not found
//
  ERROR_IPSEC_DEFAULT_MM_AUTH_NOT_FOUND = DWORD(13014);

//
// MessageId: ERROR_IPSEC_DEFAULT_QM_POLICY_NOT_FOUND
//
// MessageText:
//
//  The manifest file contains one or more syntax errors.
//
  ERROR_IPSEC_DEFAULT_QM_POLICY_NOT_FOUND = DWORD(13015);

//
// MessageId: ERROR_IPSEC_TUNNEL_FILTER_EXISTS
//
// MessageText:
//
//  The application attempted to activate a disabled activation context.
//
  ERROR_IPSEC_TUNNEL_FILTER_EXISTS = DWORD(13016);

//
// MessageId: ERROR_IPSEC_TUNNEL_FILTER_NOT_FOUND
//
// MessageText:
//
//  The requested lookup key was not found in any active activation context.
//
  ERROR_IPSEC_TUNNEL_FILTER_NOT_FOUND = DWORD(13017);

//
// MessageId: ERROR_IPSEC_MM_FILTER_PENDING_DELETION
//
// MessageText:
//
//  The Main Mode filter is pending deletion.
//
  ERROR_IPSEC_MM_FILTER_PENDING_DELETION = DWORD(13018);

//
// MessageId: ERROR_IPSEC_TRANSPORT_FILTER_PENDING_DELETION
//
// MessageText:
//
//  The transport filter is pending deletion.
//
  ERROR_IPSEC_TRANSPORT_FILTER_PENDING_DELETION = DWORD(13019);

//
// MessageId: ERROR_IPSEC_TUNNEL_FILTER_PENDING_DELETION
//
// MessageText:
//
//  The tunnel filter is pending deletion.
//
  ERROR_IPSEC_TUNNEL_FILTER_PENDING_DELETION = DWORD(13020);

//
// MessageId: ERROR_IPSEC_MM_POLICY_PENDING_DELETION
//
// MessageText:
//
//  The Main Mode policy is pending deletion.
//
  ERROR_IPSEC_MM_POLICY_PENDING_DELETION = DWORD(13021);

//
// MessageId: ERROR_IPSEC_MM_AUTH_PENDING_DELETION
//
// MessageText:
//
//  The Main Mode authentication bundle is pending deletion.
//
  ERROR_IPSEC_MM_AUTH_PENDING_DELETION = DWORD(13022);

//
// MessageId: ERROR_IPSEC_QM_POLICY_PENDING_DELETION
//
// MessageText:
//
//  The Quick Mode policy is pending deletion.
//
  ERROR_IPSEC_QM_POLICY_PENDING_DELETION = DWORD(13023);

//
// MessageId: WARNING_IPSEC_MM_POLICY_PRUNED
//
// MessageText:
//
//  The Main Mode policy was successfully added, but some of the requested offers are not supported.
//
  WARNING_IPSEC_MM_POLICY_PRUNED = DWORD(13024);

//
// MessageId: WARNING_IPSEC_QM_POLICY_PRUNED
//
// MessageText:
//
//  The Quick Mode policy was successfully added, but some of the requested offers are not supported.
//
  WARNING_IPSEC_QM_POLICY_PRUNED = DWORD(13025);

//
// MessageId: ERROR_IPSEC_IKE_NEG_STATUS_BEGIN
//
// MessageText:
//
//  ERROR_IPSEC_IKE_NEG_STATUS_BEGIN
//
  ERROR_IPSEC_IKE_NEG_STATUS_BEGIN = DWORD(13800);

//
// MessageId: ERROR_IPSEC_IKE_AUTH_FAIL
//
// MessageText:
//
//  IKE authentication credentials are unacceptable
//
  ERROR_IPSEC_IKE_AUTH_FAIL = DWORD(13801);

//
// MessageId: ERROR_IPSEC_IKE_ATTRIB_FAIL
//
// MessageText:
//
//  IKE security attributes are unacceptable
//
  ERROR_IPSEC_IKE_ATTRIB_FAIL = DWORD(13802);

//
// MessageId: ERROR_IPSEC_IKE_NEGOTIATION_PENDING
//
// MessageText:
//
//  IKE Negotiation in progress
//
  ERROR_IPSEC_IKE_NEGOTIATION_PENDING = DWORD(13803);

//
// MessageId: ERROR_IPSEC_IKE_GENERAL_PROCESSING_ERROR
//
// MessageText:
//
//  General processing error
//
  ERROR_IPSEC_IKE_GENERAL_PROCESSING_ERROR = DWORD(13804);

//
// MessageId: ERROR_IPSEC_IKE_TIMED_OUT
//
// MessageText:
//
//  Negotiation timed out
//
  ERROR_IPSEC_IKE_TIMED_OUT = DWORD(13805);

//
// MessageId: ERROR_IPSEC_IKE_NO_CERT
//
// MessageText:
//
//  IKE failed to find valid machine certificate
//
  ERROR_IPSEC_IKE_NO_CERT = DWORD(13806);

//
// MessageId: ERROR_IPSEC_IKE_SA_DELETED
//
// MessageText:
//
//  IKE SA deleted by peer before establishment completed
//
  ERROR_IPSEC_IKE_SA_DELETED = DWORD(13807);

//
// MessageId: ERROR_IPSEC_IKE_SA_REAPED
//
// MessageText:
//
//  IKE SA deleted before establishment completed
//
  ERROR_IPSEC_IKE_SA_REAPED = DWORD(13808);

//
// MessageId: ERROR_IPSEC_IKE_MM_ACQUIRE_DROP
//
// MessageText:
//
//  Negotiation request sat in Queue too long
//
  ERROR_IPSEC_IKE_MM_ACQUIRE_DROP = DWORD(13809);

//
// MessageId: ERROR_IPSEC_IKE_QM_ACQUIRE_DROP
//
// MessageText:
//
//  Negotiation request sat in Queue too long
//
  ERROR_IPSEC_IKE_QM_ACQUIRE_DROP = DWORD(13810);

//
// MessageId: ERROR_IPSEC_IKE_QUEUE_DROP_MM
//
// MessageText:
//
//  Negotiation request sat in Queue too long
//
  ERROR_IPSEC_IKE_QUEUE_DROP_MM = DWORD(13811);

//
// MessageId: ERROR_IPSEC_IKE_QUEUE_DROP_NO_MM
//
// MessageText:
//
//  Negotiation request sat in Queue too long
//
  ERROR_IPSEC_IKE_QUEUE_DROP_NO_MM = DWORD(13812);

//
// MessageId: ERROR_IPSEC_IKE_DROP_NO_RESPONSE
//
// MessageText:
//
//  No response from peer
//
  ERROR_IPSEC_IKE_DROP_NO_RESPONSE = DWORD(13813);

//
// MessageId: ERROR_IPSEC_IKE_MM_DELAY_DROP
//
// MessageText:
//
//  Negotiation took too long
//
  ERROR_IPSEC_IKE_MM_DELAY_DROP = DWORD(13814);

//
// MessageId: ERROR_IPSEC_IKE_QM_DELAY_DROP
//
// MessageText:
//
//  Negotiation took too long
//
  ERROR_IPSEC_IKE_QM_DELAY_DROP = DWORD(13815);

//
// MessageId: ERROR_IPSEC_IKE_ERROR
//
// MessageText:
//
//  Unknown error occurred
//
  ERROR_IPSEC_IKE_ERROR = DWORD(13816);

//
// MessageId: ERROR_IPSEC_IKE_CRL_FAILED
//
// MessageText:
//
//  Certificate Revocation Check failed
//
  ERROR_IPSEC_IKE_CRL_FAILED = DWORD(13817);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_KEY_USAGE
//
// MessageText:
//
//  Invalid certificate key usage
//
  ERROR_IPSEC_IKE_INVALID_KEY_USAGE = DWORD(13818);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_CERT_TYPE
//
// MessageText:
//
//  Invalid certificate type
//
  ERROR_IPSEC_IKE_INVALID_CERT_TYPE = DWORD(13819);

//
// MessageId: ERROR_IPSEC_IKE_NO_PRIVATE_KEY
//
// MessageText:
//
//  No private key associated with machine certificate
//
  ERROR_IPSEC_IKE_NO_PRIVATE_KEY = DWORD(13820);

//
// MessageId: ERROR_IPSEC_IKE_DH_FAIL
//
// MessageText:
//
//  Failure in Diffie-Helman computation
//
  ERROR_IPSEC_IKE_DH_FAIL = DWORD(13822);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_HEADER
//
// MessageText:
//
//  Invalid header
//
  ERROR_IPSEC_IKE_INVALID_HEADER = DWORD(13824);

//
// MessageId: ERROR_IPSEC_IKE_NO_POLICY
//
// MessageText:
//
//  No policy configured
//
  ERROR_IPSEC_IKE_NO_POLICY = DWORD(13825);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_SIGNATURE
//
// MessageText:
//
//  Failed to verify signature
//
  ERROR_IPSEC_IKE_INVALID_SIGNATURE = DWORD(13826);

//
// MessageId: ERROR_IPSEC_IKE_KERBEROS_ERROR
//
// MessageText:
//
//  Failed to authenticate using kerberos
//
  ERROR_IPSEC_IKE_KERBEROS_ERROR = DWORD(13827);

//
// MessageId: ERROR_IPSEC_IKE_NO_PUBLIC_KEY
//
// MessageText:
//
//  Peer's certificate did not have a public key
//
  ERROR_IPSEC_IKE_NO_PUBLIC_KEY = DWORD(13828);

// These must stay as a unit.
//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR
//
// MessageText:
//
//  Error processing error payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR = DWORD(13829);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_SA
//
// MessageText:
//
//  Error processing SA payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_SA = DWORD(13830);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_PROP
//
// MessageText:
//
//  Error processing Proposal payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_PROP = DWORD(13831);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_TRANS
//
// MessageText:
//
//  Error processing Transform payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_TRANS = DWORD(13832);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_KE
//
// MessageText:
//
//  Error processing KE payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_KE = DWORD(13833);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_ID
//
// MessageText:
//
//  Error processing ID payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_ID = DWORD(13834);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_CERT
//
// MessageText:
//
//  Error processing Cert payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_CERT = DWORD(13835);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_CERT_REQ
//
// MessageText:
//
//  Error processing Certificate Request payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_CERT_REQ = DWORD(13836);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_HASH
//
// MessageText:
//
//  Error processing Hash payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_HASH = DWORD(13837);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_SIG
//
// MessageText:
//
//  Error processing Signature payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_SIG = DWORD(13838);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_NONCE
//
// MessageText:
//
//  Error processing Nonce payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_NONCE = DWORD(13839);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_NOTIFY
//
// MessageText:
//
//  Error processing Notify payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_NOTIFY = DWORD(13840);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_DELETE
//
// MessageText:
//
//  Error processing Delete Payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_DELETE = DWORD(13841);

//
// MessageId: ERROR_IPSEC_IKE_PROCESS_ERR_VENDOR
//
// MessageText:
//
//  Error processing VendorId payload
//
  ERROR_IPSEC_IKE_PROCESS_ERR_VENDOR = DWORD(13842);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_PAYLOAD
//
// MessageText:
//
//  Invalid payload received
//
  ERROR_IPSEC_IKE_INVALID_PAYLOAD = DWORD(13843);

//
// MessageId: ERROR_IPSEC_IKE_LOAD_SOFT_SA
//
// MessageText:
//
//  Soft SA loaded
//
  ERROR_IPSEC_IKE_LOAD_SOFT_SA = DWORD(13844);

//
// MessageId: ERROR_IPSEC_IKE_SOFT_SA_TORN_DOWN
//
// MessageText:
//
//  Soft SA torn down
//
  ERROR_IPSEC_IKE_SOFT_SA_TORN_DOWN = DWORD(13845);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_COOKIE
//
// MessageText:
//
//  Invalid cookie received.
//
  ERROR_IPSEC_IKE_INVALID_COOKIE = DWORD(13846);

//
// MessageId: ERROR_IPSEC_IKE_NO_PEER_CERT
//
// MessageText:
//
//  Peer failed to send valid machine certificate
//
  ERROR_IPSEC_IKE_NO_PEER_CERT = DWORD(13847);

//
// MessageId: ERROR_IPSEC_IKE_PEER_CRL_FAILED
//
// MessageText:
//
//  Certification Revocation check of peer's certificate failed
//
  ERROR_IPSEC_IKE_PEER_CRL_FAILED = DWORD(13848);

//
// MessageId: ERROR_IPSEC_IKE_POLICY_CHANGE
//
// MessageText:
//
//  New policy invalidated SAs formed with old policy
//
  ERROR_IPSEC_IKE_POLICY_CHANGE = DWORD(13849);

//
// MessageId: ERROR_IPSEC_IKE_NO_MM_POLICY
//
// MessageText:
//
//  There is no available Main Mode IKE policy.
//
  ERROR_IPSEC_IKE_NO_MM_POLICY = DWORD(13850);

//
// MessageId: ERROR_IPSEC_IKE_NOTCBPRIV
//
// MessageText:
//
//  Failed to enabled TCB privilege.
//
  ERROR_IPSEC_IKE_NOTCBPRIV = DWORD(13851);

//
// MessageId: ERROR_IPSEC_IKE_SECLOADFAIL
//
// MessageText:
//
//  Failed to load SECURITY.DLL.
//
  ERROR_IPSEC_IKE_SECLOADFAIL = DWORD(13852);

//
// MessageId: ERROR_IPSEC_IKE_FAILSSPINIT
//
// MessageText:
//
//  Failed to obtain security function table dispatch address from SSPI.
//
  ERROR_IPSEC_IKE_FAILSSPINIT = DWORD(13853);

//
// MessageId: ERROR_IPSEC_IKE_FAILQUERYSSP
//
// MessageText:
//
//  Failed to query Kerberos package to obtain max token size.
//
  ERROR_IPSEC_IKE_FAILQUERYSSP = DWORD(13854);

//
// MessageId: ERROR_IPSEC_IKE_SRVACQFAIL
//
// MessageText:
//
//  Failed to obtain Kerberos server credentials for ISAKMP/ERROR_IPSEC_IKE service.  Kerberos authentication will not function.  The most likely reason for this is lack of domain membership.  This is normal if your computer is a member of a workgroup.
//
  ERROR_IPSEC_IKE_SRVACQFAIL = DWORD(13855);

//
// MessageId: ERROR_IPSEC_IKE_SRVQUERYCRED
//
// MessageText:
//
//  Failed to determine SSPI principal name for ISAKMP/ERROR_IPSEC_IKE service (QueryCredentialsAttributes).
//
  ERROR_IPSEC_IKE_SRVQUERYCRED = DWORD(13856);

//
// MessageId: ERROR_IPSEC_IKE_GETSPIFAIL
//
// MessageText:
//
//  Failed to obtain new SPI for the inbound SA from Ipsec driver.  The most common cause for this is that the driver does not have the correct filter.  Check your policy to verify the filters.
//
  ERROR_IPSEC_IKE_GETSPIFAIL = DWORD(13857);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_FILTER
//
// MessageText:
//
//  Given filter is invalid
//
  ERROR_IPSEC_IKE_INVALID_FILTER = DWORD(13858);

//
// MessageId: ERROR_IPSEC_IKE_OUT_OF_MEMORY
//
// MessageText:
//
//  Memory allocation failed.
//
  ERROR_IPSEC_IKE_OUT_OF_MEMORY = DWORD(13859);

//
// MessageId: ERROR_IPSEC_IKE_ADD_UPDATE_KEY_FAILED
//
// MessageText:
//
//  Failed to add Security Association to IPSec Driver.  The most common cause for this is if the IKE negotiation took too long to complete.  If the problem persists, reduce the load on the faulting machine.
//
  ERROR_IPSEC_IKE_ADD_UPDATE_KEY_FAILED = DWORD(13860);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_POLICY
//
// MessageText:
//
//  Invalid policy
//
  ERROR_IPSEC_IKE_INVALID_POLICY = DWORD(13861);

//
// MessageId: ERROR_IPSEC_IKE_UNKNOWN_DOI
//
// MessageText:
//
//  Invalid DOI
//
  ERROR_IPSEC_IKE_UNKNOWN_DOI = DWORD(13862);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_SITUATION
//
// MessageText:
//
//  Invalid situation
//
  ERROR_IPSEC_IKE_INVALID_SITUATION = DWORD(13863);

//
// MessageId: ERROR_IPSEC_IKE_DH_FAILURE
//
// MessageText:
//
//  Diffie-Hellman failure
//
  ERROR_IPSEC_IKE_DH_FAILURE = DWORD(13864);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_GROUP
//
// MessageText:
//
//  Invalid Diffie-Hellman group
//
  ERROR_IPSEC_IKE_INVALID_GROUP = DWORD(13865);

//
// MessageId: ERROR_IPSEC_IKE_ENCRYPT
//
// MessageText:
//
//  Error encrypting payload
//
  ERROR_IPSEC_IKE_ENCRYPT = DWORD(13866);

//
// MessageId: ERROR_IPSEC_IKE_DECRYPT
//
// MessageText:
//
//  Error decrypting payload
//
  ERROR_IPSEC_IKE_DECRYPT = DWORD(13867);

//
// MessageId: ERROR_IPSEC_IKE_POLICY_MATCH
//
// MessageText:
//
//  Policy match error
//
  ERROR_IPSEC_IKE_POLICY_MATCH = DWORD(13868);

//
// MessageId: ERROR_IPSEC_IKE_UNSUPPORTED_ID
//
// MessageText:
//
//  Unsupported ID
//
  ERROR_IPSEC_IKE_UNSUPPORTED_ID = DWORD(13869);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_HASH
//
// MessageText:
//
//  Hash verification failed
//
  ERROR_IPSEC_IKE_INVALID_HASH = DWORD(13870);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_HASH_ALG
//
// MessageText:
//
//  Invalid hash algorithm
//
  ERROR_IPSEC_IKE_INVALID_HASH_ALG = DWORD(13871);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_HASH_SIZE
//
// MessageText:
//
//  Invalid hash size
//
  ERROR_IPSEC_IKE_INVALID_HASH_SIZE = DWORD(13872);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_ENCRYPT_ALG
//
// MessageText:
//
//  Invalid encryption algorithm
//
  ERROR_IPSEC_IKE_INVALID_ENCRYPT_ALG = DWORD(13873);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_AUTH_ALG
//
// MessageText:
//
//  Invalid authentication algorithm
//
  ERROR_IPSEC_IKE_INVALID_AUTH_ALG = DWORD(13874);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_SIG
//
// MessageText:
//
//  Invalid certificate signature
//
  ERROR_IPSEC_IKE_INVALID_SIG = DWORD(13875);

//
// MessageId: ERROR_IPSEC_IKE_LOAD_FAILED
//
// MessageText:
//
//  Load failed
//
  ERROR_IPSEC_IKE_LOAD_FAILED = DWORD(13876);

//
// MessageId: ERROR_IPSEC_IKE_RPC_DELETE
//
// MessageText:
//
//  Deleted via RPC call
//
  ERROR_IPSEC_IKE_RPC_DELETE = DWORD(13877);

//
// MessageId: ERROR_IPSEC_IKE_BENIGN_REINIT
//
// MessageText:
//
//  Temporary state created to perform reinit. This is not a real failure.
//
  ERROR_IPSEC_IKE_BENIGN_REINIT = DWORD(13878);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_RESPONDER_LIFETIME_NOTIFY
//
// MessageText:
//
//  The lifetime value received in the Responder Lifetime Notify is below the Windows 2000 configured minimum value.  Please fix the policy on the peer machine.
//
  ERROR_IPSEC_IKE_INVALID_RESPONDER_LIFETIME_NOTIFY = DWORD(13879);

//
// MessageId: ERROR_IPSEC_IKE_INVALID_CERT_KEYLEN
//
// MessageText:
//
//  Key length in certificate is too small for configured security requirements.
//
  ERROR_IPSEC_IKE_INVALID_CERT_KEYLEN = DWORD(13881);

//
// MessageId: ERROR_IPSEC_IKE_MM_LIMIT
//
// MessageText:
//
//  Max number of established MM SAs to peer exceeded.
//
  ERROR_IPSEC_IKE_MM_LIMIT = DWORD(13882);

//
// MessageId: ERROR_IPSEC_IKE_NEGOTIATION_DISABLED
//
// MessageText:
//
//  IKE received a policy that disables negotiation.
//
  ERROR_IPSEC_IKE_NEGOTIATION_DISABLED = DWORD(13883);

//
// MessageId: ERROR_IPSEC_IKE_NEG_STATUS_END
//
// MessageText:
//
//  ERROR_IPSEC_IKE_NEG_STATUS_END
//
  ERROR_IPSEC_IKE_NEG_STATUS_END = DWORD(13884);

////////////////////////////////////
//                                //
//     COM Error Codes            //
//                                //
////////////////////////////////////

//
// The return value of COM functions and methods is an HRESULT.
// This is not a handle to anything, but is merely a 32-bit value
// with several fields encoded in the value.  The parts of an
// HRESULT are shown below.
//
// Many of the macros and functions below were orginally defined to
// operate on SCODEs.  SCODEs are no longer used.  The macros are
// still present for compatibility and easy porting of Win16 code.
// Newly written code should use the HRESULT macros and functions.
//

//
//  HRESULTs are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +-+-+-+-+-+---------------------+-------------------------------+
//  |S|R|C|N|r|    Facility         |               Code            |
//  +-+-+-+-+-+---------------------+-------------------------------+
//
//  where
//
//      S - Severity - indicates success/fail
//
//          0 - Success
//          1 - Fail (COERROR)
//
//      R - reserved portion of the facility code, corresponds to NT's
//              second severity bit.
//
//      C - reserved portion of the facility code, corresponds to NT's
//              C field.
//
//      N - reserved portion of the facility code. Used to indicate a
//              mapped NT status value.
//
//      r - reserved portion of the facility code. Reserved for internal
//              use. Used to indicate HRESULT values that are not status
//              values, but are instead message ids for display strings.
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//

//
// Severity values
//

  SEVERITY_SUCCESS = 0;
  SEVERITY_ERROR = 1;


//
// Generic test for success on any status value (non-negative numbers
// indicate success).
//

function SUCCEEDED(Status: HRESULT): BOOL;

//
// and the inverse
//

function FAILED(Status: HRESULT): BOOL;

//
// Generic test for error on any status value.
//

function IS_ERROR(Status: HRESULT): BOOL;

//
// Return the code
//

function HRESULT_CODE(hr: HRESULT): DWORD;

function SCODE_CODE(sc: LONG): DWORD;

//
//  Return the facility
//

function HRESULT_FACILITY(hr: HRESULT): DWORD;

function SCODE_FACILITY(sc: LONG): DWORD;

//
//  Return the severity
//

function HRESULT_SEVERITY(hr: HRESULT): DWORD;

function SCODE_SEVERITY(sc: LONG): DWORD;

//
// Create an HRESULT value from component pieces
//

function MAKE_HRESULT(sev, fac, code: DWORD): HRESULT;

function MAKE_SCODE(sev, fac,code: DWORD): DWORD;

//
// Map a WIN32 error value into a HRESULT
// Note: This assumes that WIN32 errors fall in the range -32k to 32k.
//
// Define bits here so macros are guaranteed to work

const
  FACILITY_NT_BIT = $10000000;

// __HRESULT_FROM_WIN32 will always be a macro.
// The goal will be to enable INLINE_HRESULT_FROM_WIN32 all the time,
// but there's too much code to change to do that at this time.

function __HRESULT_FROM_WIN32(x: DWORD): HRESULT;

function HRESULT_FROM_WIN32(x: DWORD): HRESULT;

//
// Map an NT status value into a HRESULT
//

function HRESULT_FROM_NT(x: NTSTATUS): HRESULT;

// ****** OBSOLETE functions

// HRESULT functions
// As noted above, these functions are obsolete and should not be used.


// Extract the SCODE from a HRESULT

function GetScode(hr: HRESULT): DWORD;

// Convert an SCODE into an HRESULT.

function ResultFromScode(sc: DWORD): HRESULT;

// PropagateResult is a noop

function PropagateResult(hrPrevious, scBase: DWORD): HRESULT;

// ****** End of OBSOLETE functions.

// ---------------------- HRESULT value definitions -----------------
//
// HRESULT definitions
//

type
  _HRESULT_TYPEDEF_ = HRESULT;

const
  NOERROR = 0;

//
// Error definitions follow
//

//
// Codes 0x4000-0x40ff are reserved for OLE
//
//
// Error codes
//
//
// MessageId: E_UNEXPECTED
//
// MessageText:
//
//  Catastrophic failure
//
  E_UNEXPECTED = HRESULT($8000FFFF);

//
// MessageId: E_NOTIMPL
//
// MessageText:
//
//  Not implemented
//
  E_NOTIMPL = HRESULT($80004001);

//
// MessageId: E_OUTOFMEMORY
//
// MessageText:
//
//  Ran out of memory
//
  E_OUTOFMEMORY = HRESULT($8007000E);

//
// MessageId: E_INVALIDARG
//
// MessageText:
//
//  One or more arguments are invalid
//
  E_INVALIDARG = HRESULT($80070057);

//
// MessageId: E_NOINTERFACE
//
// MessageText:
//
//  No such interface supported
//
  E_NOINTERFACE = HRESULT($80004002);

//
// MessageId: E_POINTER
//
// MessageText:
//
//  Invalid pointer
//
  E_POINTER = HRESULT($80004003);

//
// MessageId: E_HANDLE
//
// MessageText:
//
//  Invalid handle
//
  E_HANDLE = HRESULT($80070006);

//
// MessageId: E_ABORT
//
// MessageText:
//
//  Operation aborted
//
  E_ABORT = HRESULT($80004004);

//
// MessageId: E_FAIL
//
// MessageText:
//
//  Unspecified error
//
  E_FAIL = HRESULT($80004005);

//
// MessageId: E_ACCESSDENIED
//
// MessageText:
//
//  General access denied error
//
  E_ACCESSDENIED = HRESULT($80070005);

//
// MessageId: E_PENDING
//
// MessageText:
//
//  The data necessary to complete this operation is not yet available.
//
  E_PENDING = HRESULT($8000000A);

//
// MessageId: CO_E_INIT_TLS
//
// MessageText:
//
//  Thread local storage failure
//
  CO_E_INIT_TLS = HRESULT($80004006);

//
// MessageId: CO_E_INIT_SHARED_ALLOCATOR
//
// MessageText:
//
//  Get shared memory allocator failure
//
  CO_E_INIT_SHARED_ALLOCATOR = HRESULT($80004007);

//
// MessageId: CO_E_INIT_MEMORY_ALLOCATOR
//
// MessageText:
//
//  Get memory allocator failure
//
  CO_E_INIT_MEMORY_ALLOCATOR = HRESULT($80004008);

//
// MessageId: CO_E_INIT_CLASS_CACHE
//
// MessageText:
//
//  Unable to initialize class cache
//
  CO_E_INIT_CLASS_CACHE = HRESULT($80004009);

//
// MessageId: CO_E_INIT_RPC_CHANNEL
//
// MessageText:
//
//  Unable to initialize RPC services
//
  CO_E_INIT_RPC_CHANNEL = HRESULT($8000400A);

//
// MessageId: CO_E_INIT_TLS_SET_CHANNEL_CONTROL
//
// MessageText:
//
//  Cannot set thread local storage channel control
//
  CO_E_INIT_TLS_SET_CHANNEL_CONTROL = HRESULT($8000400B);

//
// MessageId: CO_E_INIT_TLS_CHANNEL_CONTROL
//
// MessageText:
//
//  Could not allocate thread local storage channel control
//
  CO_E_INIT_TLS_CHANNEL_CONTROL = HRESULT($8000400C);

//
// MessageId: CO_E_INIT_UNACCEPTED_USER_ALLOCATOR
//
// MessageText:
//
//  The user supplied memory allocator is unacceptable
//
  CO_E_INIT_UNACCEPTED_USER_ALLOCATOR = HRESULT($8000400D);

//
// MessageId: CO_E_INIT_SCM_MUTEX_EXISTS
//
// MessageText:
//
//  The OLE service mutex already exists
//
  CO_E_INIT_SCM_MUTEX_EXISTS = HRESULT($8000400E);

//
// MessageId: CO_E_INIT_SCM_FILE_MAPPING_EXISTS
//
// MessageText:
//
//  The OLE service file mapping already exists
//
  CO_E_INIT_SCM_FILE_MAPPING_EXISTS = HRESULT($8000400F);

//
// MessageId: CO_E_INIT_SCM_MAP_VIEW_OF_FILE
//
// MessageText:
//
//  Unable to map view of file for OLE service
//
  CO_E_INIT_SCM_MAP_VIEW_OF_FILE = HRESULT($80004010);

//
// MessageId: CO_E_INIT_SCM_EXEC_FAILURE
//
// MessageText:
//
//  Failure attempting to launch OLE service
//
  CO_E_INIT_SCM_EXEC_FAILURE = HRESULT($80004011);

//
// MessageId: CO_E_INIT_ONLY_SINGLE_THREADED
//
// MessageText:
//
//  There was an attempt to call CoInitialize a second time while single threaded
//
  CO_E_INIT_ONLY_SINGLE_THREADED = HRESULT($80004012);

//
// MessageId: CO_E_CANT_REMOTE
//
// MessageText:
//
//  A Remote activation was necessary but was not allowed
//
  CO_E_CANT_REMOTE = HRESULT($80004013);

//
// MessageId: CO_E_BAD_SERVER_NAME
//
// MessageText:
//
//  A Remote activation was necessary but the server name provided was invalid
//
  CO_E_BAD_SERVER_NAME = HRESULT($80004014);

//
// MessageId: CO_E_WRONG_SERVER_IDENTITY
//
// MessageText:
//
//  The class is configured to run as a security id different from the caller
//
  CO_E_WRONG_SERVER_IDENTITY = HRESULT($80004015);

//
// MessageId: CO_E_OLE1DDE_DISABLED
//
// MessageText:
//
//  Use of Ole1 services requiring DDE windows is disabled
//
  CO_E_OLE1DDE_DISABLED = HRESULT($80004016);

//
// MessageId: CO_E_RUNAS_SYNTAX
//
// MessageText:
//
//  A RunAs specification must be <domain name>\<user name> or simply <user name>
//
  CO_E_RUNAS_SYNTAX = HRESULT($80004017);

//
// MessageId: CO_E_CREATEPROCESS_FAILURE
//
// MessageText:
//
//  The server process could not be started.  The pathname may be incorrect.
//
  CO_E_CREATEPROCESS_FAILURE = HRESULT($80004018);

//
// MessageId: CO_E_RUNAS_CREATEPROCESS_FAILURE
//
// MessageText:
//
//  The server process could not be started as the configured identity.  The pathname may be incorrect or unavailable.
//
  CO_E_RUNAS_CREATEPROCESS_FAILURE = HRESULT($80004019);

//
// MessageId: CO_E_RUNAS_LOGON_FAILURE
//
// MessageText:
//
//  The server process could not be started because the configured identity is incorrect.  Check the username and password.
//
  CO_E_RUNAS_LOGON_FAILURE = HRESULT($8000401A);

//
// MessageId: CO_E_LAUNCH_PERMSSION_DENIED
//
// MessageText:
//
//  The client is not allowed to launch this server.
//
  CO_E_LAUNCH_PERMSSION_DENIED = HRESULT($8000401B);

//
// MessageId: CO_E_START_SERVICE_FAILURE
//
// MessageText:
//
//  The service providing this server could not be started.
//
  CO_E_START_SERVICE_FAILURE = HRESULT($8000401C);

//
// MessageId: CO_E_REMOTE_COMMUNICATION_FAILURE
//
// MessageText:
//
//  This computer was unable to communicate with the computer providing the server.
//
  CO_E_REMOTE_COMMUNICATION_FAILURE = HRESULT($8000401D);

//
// MessageId: CO_E_SERVER_START_TIMEOUT
//
// MessageText:
//
//  The server did not respond after being launched.
//
  CO_E_SERVER_START_TIMEOUT = HRESULT($8000401E);

//
// MessageId: CO_E_CLSREG_INCONSISTENT
//
// MessageText:
//
//  The registration information for this server is inconsistent or incomplete.
//
  CO_E_CLSREG_INCONSISTENT = HRESULT($8000401F);

//
// MessageId: CO_E_IIDREG_INCONSISTENT
//
// MessageText:
//
//  The registration information for this interface is inconsistent or incomplete.
//
  CO_E_IIDREG_INCONSISTENT = HRESULT($80004020);

//
// MessageId: CO_E_NOT_SUPPORTED
//
// MessageText:
//
//  The operation attempted is not supported.
//
  CO_E_NOT_SUPPORTED = HRESULT($80004021);

//
// MessageId: CO_E_RELOAD_DLL
//
// MessageText:
//
//  A dll must be loaded.
//
  CO_E_RELOAD_DLL = HRESULT($80004022);

//
// MessageId: CO_E_MSI_ERROR
//
// MessageText:
//
//  A Microsoft Software Installer error was encountered.
//
  CO_E_MSI_ERROR = HRESULT($80004023);

//
// MessageId: CO_E_ATTEMPT_TO_CREATE_OUTSIDE_CLIENT_CONTEXT
//
// MessageText:
//
//  The specified activation could not occur in the client context as specified.
//
  CO_E_ATTEMPT_TO_CREATE_OUTSIDE_CLIENT_CONTEXT = HRESULT($80004024);

//
// MessageId: CO_E_SERVER_PAUSED
//
// MessageText:
//
//  Activations on the server are paused.
//
  CO_E_SERVER_PAUSED = HRESULT($80004025);

//
// MessageId: CO_E_SERVER_NOT_PAUSED
//
// MessageText:
//
//  Activations on the server are not paused.
//
  CO_E_SERVER_NOT_PAUSED = HRESULT($80004026);

//
// MessageId: CO_E_CLASS_DISABLED
//
// MessageText:
//
//  The component or application containing the component has been disabled.
//
  CO_E_CLASS_DISABLED = HRESULT($80004027);

//
// MessageId: CO_E_CLRNOTAVAILABLE
//
// MessageText:
//
//  The common language runtime is not available
//
  CO_E_CLRNOTAVAILABLE = HRESULT($80004028);

//
// MessageId: CO_E_ASYNC_WORK_REJECTED
//
// MessageText:
//
//  The thread-pool rejected the submitted asynchronous work.
//
  CO_E_ASYNC_WORK_REJECTED = HRESULT($80004029);

//
// MessageId: CO_E_SERVER_INIT_TIMEOUT
//
// MessageText:
//
//  The server started, but did not finish initializing in a timely fashion.
//
  CO_E_SERVER_INIT_TIMEOUT = HRESULT($8000402A);

//
// MessageId: CO_E_NO_SECCTX_IN_ACTIVATE
//
// MessageText:
//
//  Unable to complete the call since there is no COM+ security context inside IObjectControl.Activate.
//
  CO_E_NO_SECCTX_IN_ACTIVATE = HRESULT($8000402B);

//
// MessageId: CO_E_TRACKER_CONFIG
//
// MessageText:
//
//  The provided tracker configuration is invalid
//
  CO_E_TRACKER_CONFIG = HRESULT($80004030);

//
// MessageId: CO_E_THREADPOOL_CONFIG
//
// MessageText:
//
//  The provided thread pool configuration is invalid
//
  CO_E_THREADPOOL_CONFIG = HRESULT($80004031);

//
// MessageId: CO_E_SXS_CONFIG
//
// MessageText:
//
//  The provided side-by-side configuration is invalid
//
  CO_E_SXS_CONFIG = HRESULT($80004032);

//
// MessageId: CO_E_MALFORMED_SPN
//
// MessageText:
//
//  The server principal name (SPN) obtained during security negotiation is malformed.
//
  CO_E_MALFORMED_SPN = HRESULT($80004033);


//
// Success codes
//
  S_OK = HRESULT($00000000);
  S_FALSE = HRESULT($00000001);

// ******************
// FACILITY_ITF
// ******************

//
// Codes 0x0-0x01ff are reserved for the OLE group of
// interfaces.
//


//
// Generic OLE errors that may be returned by many inerfaces
//

  OLE_E_FIRST = HRESULT($80040000);
  OLE_E_LAST = HRESULT($800400FF);
  OLE_S_FIRST = HRESULT($00040000);
  OLE_S_LAST = HRESULT($000400FF);

//
// Old OLE errors
//
//
// MessageId: OLE_E_OLEVERB
//
// MessageText:
//
//  Invalid OLEVERB structure
//
  OLE_E_OLEVERB = HRESULT($80040000);

//
// MessageId: OLE_E_ADVF
//
// MessageText:
//
//  Invalid advise flags
//
  OLE_E_ADVF = HRESULT($80040001);

//
// MessageId: OLE_E_ENUM_NOMORE
//
// MessageText:
//
//  Can't enumerate any more, because the associated data is missing
//
  OLE_E_ENUM_NOMORE = HRESULT($80040002);

//
// MessageId: OLE_E_ADVISENOTSUPPORTED
//
// MessageText:
//
//  This implementation doesn't take advises
//
  OLE_E_ADVISENOTSUPPORTED = HRESULT($80040003);

//
// MessageId: OLE_E_NOCONNECTION
//
// MessageText:
//
//  There is no connection for this connection ID
//
  OLE_E_NOCONNECTION = HRESULT($80040004);

//
// MessageId: OLE_E_NOTRUNNING
//
// MessageText:
//
//  Need to run the object to perform this operation
//
  OLE_E_NOTRUNNING = HRESULT($80040005);

//
// MessageId: OLE_E_NOCACHE
//
// MessageText:
//
//  There is no cache to operate on
//
  OLE_E_NOCACHE = HRESULT($80040006);

//
// MessageId: OLE_E_BLANK
//
// MessageText:
//
//  Uninitialized object
//
  OLE_E_BLANK = HRESULT($80040007);

//
// MessageId: OLE_E_CLASSDIFF
//
// MessageText:
//
//  Linked object's source class has changed
//
  OLE_E_CLASSDIFF = HRESULT($80040008);

//
// MessageId: OLE_E_CANT_GETMONIKER
//
// MessageText:
//
//  Not able to get the moniker of the object
//
  OLE_E_CANT_GETMONIKER = HRESULT($80040009);

//
// MessageId: OLE_E_CANT_BINDTOSOURCE
//
// MessageText:
//
//  Not able to bind to the source
//
  OLE_E_CANT_BINDTOSOURCE = HRESULT($8004000A);

//
// MessageId: OLE_E_STATIC
//
// MessageText:
//
//  Object is static; operation not allowed
//
  OLE_E_STATIC = HRESULT($8004000B);

//
// MessageId: OLE_E_PROMPTSAVECANCELLED
//
// MessageText:
//
//  User canceled out of save dialog
//
  OLE_E_PROMPTSAVECANCELLED = HRESULT($8004000C);

//
// MessageId: OLE_E_INVALIDRECT
//
// MessageText:
//
//  Invalid rectangle
//
  OLE_E_INVALIDRECT = HRESULT($8004000D);

//
// MessageId: OLE_E_WRONGCOMPOBJ
//
// MessageText:
//
//  compobj.dll is too old for the ole2.dll initialized
//
  OLE_E_WRONGCOMPOBJ = HRESULT($8004000E);

//
// MessageId: OLE_E_INVALIDHWND
//
// MessageText:
//
//  Invalid window handle
//
  OLE_E_INVALIDHWND = HRESULT($8004000F);

//
// MessageId: OLE_E_NOT_INPLACEACTIVE
//
// MessageText:
//
//  Object is not in any of the inplace active states
//
  OLE_E_NOT_INPLACEACTIVE = HRESULT($80040010);

//
// MessageId: OLE_E_CANTCONVERT
//
// MessageText:
//
//  Not able to convert object
//
  OLE_E_CANTCONVERT = HRESULT($80040011);

//
// MessageId: OLE_E_NOSTORAGE
//
// MessageText:
//
//  Not able to perform the operation because object is not given storage yet
//
  OLE_E_NOSTORAGE = HRESULT($80040012);

//
// MessageId: DV_E_FORMATETC
//
// MessageText:
//
//  Invalid FORMATETC structure
//
  DV_E_FORMATETC = HRESULT($80040064);

//
// MessageId: DV_E_DVTARGETDEVICE
//
// MessageText:
//
//  Invalid DVTARGETDEVICE structure
//
  DV_E_DVTARGETDEVICE = HRESULT($80040065);

//
// MessageId: DV_E_STGMEDIUM
//
// MessageText:
//
//  Invalid STDGMEDIUM structure
//
  DV_E_STGMEDIUM = HRESULT($80040066);

//
// MessageId: DV_E_STATDATA
//
// MessageText:
//
//  Invalid STATDATA structure
//
  DV_E_STATDATA = HRESULT($80040067);

//
// MessageId: DV_E_LINDEX
//
// MessageText:
//
//  Invalid lindex
//
  DV_E_LINDEX = HRESULT($80040068);

//
// MessageId: DV_E_TYMED
//
// MessageText:
//
//  Invalid tymed
//
  DV_E_TYMED = HRESULT($80040069);

//
// MessageId: DV_E_CLIPFORMAT
//
// MessageText:
//
//  Invalid clipboard format
//
  DV_E_CLIPFORMAT = HRESULT($8004006A);

//
// MessageId: DV_E_DVASPECT
//
// MessageText:
//
//  Invalid aspect(s)
//
  DV_E_DVASPECT = HRESULT($8004006B);

//
// MessageId: DV_E_DVTARGETDEVICE_SIZE
//
// MessageText:
//
//  tdSize parameter of the DVTARGETDEVICE structure is invalid
//
  DV_E_DVTARGETDEVICE_SIZE = HRESULT($8004006C);

//
// MessageId: DV_E_NOIVIEWOBJECT
//
// MessageText:
//
//  Object doesn't support IViewObject interface
//
  DV_E_NOIVIEWOBJECT = HRESULT($8004006D);

  DRAGDROP_E_FIRST = DWORD($80040100);
  DRAGDROP_E_LAST = DWORD($8004010F);
  DRAGDROP_S_FIRST = DWORD($00040100);
  DRAGDROP_S_LAST = DWORD($0004010F);
//
// MessageId: DRAGDROP_E_NOTREGISTERED
//
// MessageText:
//
//  Trying to revoke a drop target that has not been registered
//
  DRAGDROP_E_NOTREGISTERED = HRESULT($80040100);

//
// MessageId: DRAGDROP_E_ALREADYREGISTERED
//
// MessageText:
//
//  This window has already been registered as a drop target
//
  DRAGDROP_E_ALREADYREGISTERED = HRESULT($80040101);

//
// MessageId: DRAGDROP_E_INVALIDHWND
//
// MessageText:
//
//  Invalid window handle
//
  DRAGDROP_E_INVALIDHWND = HRESULT($80040102);

  CLASSFACTORY_E_FIRST = DWORD($80040110);
  CLASSFACTORY_E_LAST = DWORD($8004011F);
  CLASSFACTORY_S_FIRST = DWORD($00040110);
  CLASSFACTORY_S_LAST = DWORD($0004011F);
//
// MessageId: CLASS_E_NOAGGREGATION
//
// MessageText:
//
//  Class does not support aggregation (or class object is remote)
//
  CLASS_E_NOAGGREGATION = HRESULT($80040110);

//
// MessageId: CLASS_E_CLASSNOTAVAILABLE
//
// MessageText:
//
//  ClassFactory cannot supply requested class
//
  CLASS_E_CLASSNOTAVAILABLE = HRESULT($80040111);

//
// MessageId: CLASS_E_NOTLICENSED
//
// MessageText:
//
//  Class is not licensed for use
//
  CLASS_E_NOTLICENSED = HRESULT($80040112);

  MARSHAL_E_FIRST = DWORD($80040120);
  MARSHAL_E_LAST = DWORD($8004012F);
  MARSHAL_S_FIRST = DWORD($00040120);
  MARSHAL_S_LAST = DWORD($0004012F);
  DATA_E_FIRST = DWORD($80040130);
  DATA_E_LAST = DWORD($8004013F);
  DATA_S_FIRST = DWORD($00040130);
  DATA_S_LAST = DWORD($0004013F);
  VIEW_E_FIRST = DWORD($80040140);
  VIEW_E_LAST = DWORD($8004014F);
  VIEW_S_FIRST = DWORD($00040140);
  VIEW_S_LAST = DWORD($0004014F);
//
// MessageId: VIEW_E_DRAW
//
// MessageText:
//
//  Error drawing view
//
  VIEW_E_DRAW = HRESULT($80040140);

  REGDB_E_FIRST = DWORD($80040150);
  REGDB_E_LAST = DWORD($8004015F);
  REGDB_S_FIRST = DWORD($00040150);
  REGDB_S_LAST = DWORD($0004015F);
//
// MessageId: REGDB_E_READREGDB
//
// MessageText:
//
//  Could not read key from registry
//
  REGDB_E_READREGDB = HRESULT($80040150);

//
// MessageId: REGDB_E_WRITEREGDB
//
// MessageText:
//
//  Could not write key to registry
//
  REGDB_E_WRITEREGDB = HRESULT($80040151);

//
// MessageId: REGDB_E_KEYMISSING
//
// MessageText:
//
//  Could not find the key in the registry
//
  REGDB_E_KEYMISSING = HRESULT($80040152);

//
// MessageId: REGDB_E_INVALIDVALUE
//
// MessageText:
//
//  Invalid value for registry
//
  REGDB_E_INVALIDVALUE = HRESULT($80040153);

//
// MessageId: REGDB_E_CLASSNOTREG
//
// MessageText:
//
//  Class not registered
//
  REGDB_E_CLASSNOTREG = HRESULT($80040154);

//
// MessageId: REGDB_E_IIDNOTREG
//
// MessageText:
//
//  Interface not registered
//
  REGDB_E_IIDNOTREG = HRESULT($80040155);

//
// MessageId: REGDB_E_BADTHREADINGMODEL
//
// MessageText:
//
//  Threading model entry is not valid
//
  REGDB_E_BADTHREADINGMODEL = HRESULT($80040156);

  CAT_E_FIRST = DWORD($80040160);
  CAT_E_LAST = DWORD($80040161);
//
// MessageId: CAT_E_CATIDNOEXIST
//
// MessageText:
//
//  CATID does not exist
//
  CAT_E_CATIDNOEXIST = HRESULT($80040160);

//
// MessageId: CAT_E_NODESCRIPTION
//
// MessageText:
//
//  Description not found
//
  CAT_E_NODESCRIPTION = HRESULT($80040161);

////////////////////////////////////
//                                //
//     Class Store Error Codes    //
//                                //
////////////////////////////////////
  CS_E_FIRST = DWORD($80040164);
  CS_E_LAST = DWORD($8004016F);
//
// MessageId: CS_E_PACKAGE_NOTFOUND
//
// MessageText:
//
//  No package in the software installation data in the Active Directory meets this criteria.
//
  CS_E_PACKAGE_NOTFOUND = HRESULT($80040164);

//
// MessageId: CS_E_NOT_DELETABLE
//
// MessageText:
//
//  Deleting this will break the referential integrity of the software installation data in the Active Directory.
//
  CS_E_NOT_DELETABLE = HRESULT($80040165);

//
// MessageId: CS_E_CLASS_NOTFOUND
//
// MessageText:
//
//  The CLSID was not found in the software installation data in the Active Directory.
//
  CS_E_CLASS_NOTFOUND = HRESULT($80040166);

//
// MessageId: CS_E_INVALID_VERSION
//
// MessageText:
//
//  The software installation data in the Active Directory is corrupt.
//
  CS_E_INVALID_VERSION = HRESULT($80040167);

//
// MessageId: CS_E_NO_CLASSSTORE
//
// MessageText:
//
//  There is no software installation data in the Active Directory.
//
  CS_E_NO_CLASSSTORE = HRESULT($80040168);

//
// MessageId: CS_E_OBJECT_NOTFOUND
//
// MessageText:
//
//  There is no software installation data object in the Active Directory.
//
  CS_E_OBJECT_NOTFOUND = HRESULT($80040169);

//
// MessageId: CS_E_OBJECT_ALREADY_EXISTS
//
// MessageText:
//
//  The software installation data object in the Active Directory already exists.
//
  CS_E_OBJECT_ALREADY_EXISTS = HRESULT($8004016A);

//
// MessageId: CS_E_INVALID_PATH
//
// MessageText:
//
//  The path to the software installation data in the Active Directory is not correct.
//
  CS_E_INVALID_PATH = HRESULT($8004016B);

//
// MessageId: CS_E_NETWORK_ERROR
//
// MessageText:
//
//  A network error interrupted the operation.
//
  CS_E_NETWORK_ERROR = HRESULT($8004016C);

//
// MessageId: CS_E_ADMIN_LIMIT_EXCEEDED
//
// MessageText:
//
//  The size of this object exceeds the maximum size set by the Administrator.
//
  CS_E_ADMIN_LIMIT_EXCEEDED = HRESULT($8004016D);

//
// MessageId: CS_E_SCHEMA_MISMATCH
//
// MessageText:
//
//  The schema for the software installation data in the Active Directory does not match the required schema.
//
  CS_E_SCHEMA_MISMATCH = HRESULT($8004016E);

//
// MessageId: CS_E_INTERNAL_ERROR
//
// MessageText:
//
//  An error occurred in the software installation data in the Active Directory.
//
  CS_E_INTERNAL_ERROR = HRESULT($8004016F);

  CACHE_E_FIRST = DWORD($80040170);
  CACHE_E_LAST = DWORD($8004017F);
  CACHE_S_FIRST = DWORD($00040170);
  CACHE_S_LAST = DWORD($0004017F);
//
// MessageId: CACHE_E_NOCACHE_UPDATED
//
// MessageText:
//
//  Cache not updated
//
  CACHE_E_NOCACHE_UPDATED = HRESULT($80040170);

  OLEOBJ_E_FIRST = DWORD($80040180);
  OLEOBJ_E_LAST = DWORD($8004018F);
  OLEOBJ_S_FIRST = DWORD($00040180);
  OLEOBJ_S_LAST = DWORD($0004018F);
//
// MessageId: OLEOBJ_E_NOVERBS
//
// MessageText:
//
//  No verbs for OLE object
//
  OLEOBJ_E_NOVERBS = HRESULT($80040180);

//
// MessageId: OLEOBJ_E_INVALIDVERB
//
// MessageText:
//
//  Invalid verb for OLE object
//
  OLEOBJ_E_INVALIDVERB = HRESULT($80040181);

  CLIENTSITE_E_FIRST = DWORD($80040190);
  CLIENTSITE_E_LAST = DWORD($8004019F);
  CLIENTSITE_S_FIRST = DWORD($00040190);
  CLIENTSITE_S_LAST = DWORD($0004019F);
//
// MessageId: INPLACE_E_NOTUNDOABLE
//
// MessageText:
//
//  Undo is not available
//
  INPLACE_E_NOTUNDOABLE = HRESULT($800401A0);

//
// MessageId: INPLACE_E_NOTOOLSPACE
//
// MessageText:
//
//  Space for tools is not available
//
  INPLACE_E_NOTOOLSPACE = HRESULT($800401A1);

  INPLACE_E_FIRST = DWORD($800401A0);
  INPLACE_E_LAST = DWORD($800401AF);
  INPLACE_S_FIRST = DWORD($000401A0);
  INPLACE_S_LAST = DWORD($000401AF);
  ENUM_E_FIRST = DWORD($800401B0);
  ENUM_E_LAST = DWORD($800401BF);
  ENUM_S_FIRST = DWORD($000401B0);
  ENUM_S_LAST = DWORD($000401BF);
  CONVERT10_E_FIRST = DWORD($800401C0);
  CONVERT10_E_LAST = DWORD($800401CF);
  CONVERT10_S_FIRST = DWORD($000401C0);
  CONVERT10_S_LAST = DWORD($000401CF);
//
// MessageId: CONVERT10_E_OLESTREAM_GET
//
// MessageText:
//
//  OLESTREAM Get method failed
//
  CONVERT10_E_OLESTREAM_GET = HRESULT($800401C0);

//
// MessageId: CONVERT10_E_OLESTREAM_PUT
//
// MessageText:
//
//  OLESTREAM Put method failed
//
  CONVERT10_E_OLESTREAM_PUT = HRESULT($800401C1);

//
// MessageId: CONVERT10_E_OLESTREAM_FMT
//
// MessageText:
//
//  Contents of the OLESTREAM not in correct format
//
  CONVERT10_E_OLESTREAM_FMT = HRESULT($800401C2);

//
// MessageId: CONVERT10_E_OLESTREAM_BITMAP_TO_DIB
//
// MessageText:
//
//  There was an error in a Windows GDI call while converting the bitmap to a DIB
//
  CONVERT10_E_OLESTREAM_BITMAP_TO_DIB = HRESULT($800401C3);

//
// MessageId: CONVERT10_E_STG_FMT
//
// MessageText:
//
//  Contents of the IStorage not in correct format
//
  CONVERT10_E_STG_FMT = HRESULT($800401C4);

//
// MessageId: CONVERT10_E_STG_NO_STD_STREAM
//
// MessageText:
//
//  Contents of IStorage is missing one of the standard streams
//
  CONVERT10_E_STG_NO_STD_STREAM = HRESULT($800401C5);

//
// MessageId: CONVERT10_E_STG_DIB_TO_BITMAP
//
// MessageText:
//
//  There was an error in a Windows GDI call while converting the DIB to a bitmap.
//  
//
  CONVERT10_E_STG_DIB_TO_BITMAP = HRESULT($800401C6);

  CLIPBRD_E_FIRST = DWORD($800401D0);
  CLIPBRD_E_LAST = DWORD($800401DF);
  CLIPBRD_S_FIRST = DWORD($000401D0);
  CLIPBRD_S_LAST = DWORD($000401DF);
//
// MessageId: CLIPBRD_E_CANT_OPEN
//
// MessageText:
//
//  OpenClipboard Failed
//
  CLIPBRD_E_CANT_OPEN = HRESULT($800401D0);

//
// MessageId: CLIPBRD_E_CANT_EMPTY
//
// MessageText:
//
//  EmptyClipboard Failed
//
  CLIPBRD_E_CANT_EMPTY = HRESULT($800401D1);

//
// MessageId: CLIPBRD_E_CANT_SET
//
// MessageText:
//
//  SetClipboard Failed
//
  CLIPBRD_E_CANT_SET = HRESULT($800401D2);

//
// MessageId: CLIPBRD_E_BAD_DATA
//
// MessageText:
//
//  Data on clipboard is invalid
//
  CLIPBRD_E_BAD_DATA = HRESULT($800401D3);

//
// MessageId: CLIPBRD_E_CANT_CLOSE
//
// MessageText:
//
//  CloseClipboard Failed
//
  CLIPBRD_E_CANT_CLOSE = HRESULT($800401D4);

  MK_E_FIRST = DWORD($800401E0);
  MK_E_LAST = DWORD($800401EF);
  MK_S_FIRST = DWORD($000401E0);
  MK_S_LAST = DWORD($000401EF);
//
// MessageId: MK_E_CONNECTMANUALLY
//
// MessageText:
//
//  Moniker needs to be connected manually
//
  MK_E_CONNECTMANUALLY = HRESULT($800401E0);

//
// MessageId: MK_E_EXCEEDEDDEADLINE
//
// MessageText:
//
//  Operation exceeded deadline
//
  MK_E_EXCEEDEDDEADLINE = HRESULT($800401E1);

//
// MessageId: MK_E_NEEDGENERIC
//
// MessageText:
//
//  Moniker needs to be generic
//
  MK_E_NEEDGENERIC = HRESULT($800401E2);

//
// MessageId: MK_E_UNAVAILABLE
//
// MessageText:
//
//  Operation unavailable
//
  MK_E_UNAVAILABLE = HRESULT($800401E3);

//
// MessageId: MK_E_SYNTAX
//
// MessageText:
//
//  Invalid syntax
//
  MK_E_SYNTAX = HRESULT($800401E4);

//
// MessageId: MK_E_NOOBJECT
//
// MessageText:
//
//  No object for moniker
//
  MK_E_NOOBJECT = HRESULT($800401E5);

//
// MessageId: MK_E_INVALIDEXTENSION
//
// MessageText:
//
//  Bad extension for file
//
  MK_E_INVALIDEXTENSION = HRESULT($800401E6);

//
// MessageId: MK_E_INTERMEDIATEINTERFACENOTSUPPORTED
//
// MessageText:
//
//  Intermediate operation failed
//
  MK_E_INTERMEDIATEINTERFACENOTSUPPORTED = HRESULT($800401E7);

//
// MessageId: MK_E_NOTBINDABLE
//
// MessageText:
//
//  Moniker is not bindable
//
  MK_E_NOTBINDABLE = HRESULT($800401E8);

//
// MessageId: MK_E_NOTBOUND
//
// MessageText:
//
//  Moniker is not bound
//
  MK_E_NOTBOUND = HRESULT($800401E9);

//
// MessageId: MK_E_CANTOPENFILE
//
// MessageText:
//
//  Moniker cannot open file
//
  MK_E_CANTOPENFILE = HRESULT($800401EA);

//
// MessageId: MK_E_MUSTBOTHERUSER
//
// MessageText:
//
//  User input required for operation to succeed
//
  MK_E_MUSTBOTHERUSER = HRESULT($800401EB);

//
// MessageId: MK_E_NOINVERSE
//
// MessageText:
//
//  Moniker class has no inverse
//
  MK_E_NOINVERSE = HRESULT($800401EC);

//
// MessageId: MK_E_NOSTORAGE
//
// MessageText:
//
//  Moniker does not refer to storage
//
  MK_E_NOSTORAGE = HRESULT($800401ED);

//
// MessageId: MK_E_NOPREFIX
//
// MessageText:
//
//  No common prefix
//
  MK_E_NOPREFIX = HRESULT($800401EE);

//
// MessageId: MK_E_ENUMERATION_FAILED
//
// MessageText:
//
//  Moniker could not be enumerated
//
  MK_E_ENUMERATION_FAILED = HRESULT($800401EF);

  CO_E_FIRST = DWORD($800401F0);
  CO_E_LAST = DWORD($800401FF);
  CO_S_FIRST = DWORD($000401F0);
  CO_S_LAST = DWORD($000401FF);
//
// MessageId: CO_E_NOTINITIALIZED
//
// MessageText:
//
//  CoInitialize has not been called.
//
  CO_E_NOTINITIALIZED = HRESULT($800401F0);

//
// MessageId: CO_E_ALREADYINITIALIZED
//
// MessageText:
//
//  CoInitialize has already been called.
//
  CO_E_ALREADYINITIALIZED = HRESULT($800401F1);

//
// MessageId: CO_E_CANTDETERMINECLASS
//
// MessageText:
//
//  Class of object cannot be determined
//
  CO_E_CANTDETERMINECLASS = HRESULT($800401F2);

//
// MessageId: CO_E_CLASSSTRING
//
// MessageText:
//
//  Invalid class string
//
  CO_E_CLASSSTRING = HRESULT($800401F3);

//
// MessageId: CO_E_IIDSTRING
//
// MessageText:
//
//  Invalid interface string
//
  CO_E_IIDSTRING = HRESULT($800401F4);

//
// MessageId: CO_E_APPNOTFOUND
//
// MessageText:
//
//  Application not found
//
  CO_E_APPNOTFOUND = HRESULT($800401F5);

//
// MessageId: CO_E_APPSINGLEUSE
//
// MessageText:
//
//  Application cannot be run more than once
//
  CO_E_APPSINGLEUSE = HRESULT($800401F6);

//
// MessageId: CO_E_ERRORINAPP
//
// MessageText:
//
//  Some error in application program
//
  CO_E_ERRORINAPP = HRESULT($800401F7);

//
// MessageId: CO_E_DLLNOTFOUND
//
// MessageText:
//
//  DLL for class not found
//
  CO_E_DLLNOTFOUND = HRESULT($800401F8);

//
// MessageId: CO_E_ERRORINDLL
//
// MessageText:
//
//  Error in the DLL
//
  CO_E_ERRORINDLL = HRESULT($800401F9);

//
// MessageId: CO_E_WRONGOSFORAPP
//
// MessageText:
//
//  Wrong OS or OS version for application
//
  CO_E_WRONGOSFORAPP = HRESULT($800401FA);

//
// MessageId: CO_E_OBJNOTREG
//
// MessageText:
//
//  Object is not registered
//
  CO_E_OBJNOTREG = HRESULT($800401FB);

//
// MessageId: CO_E_OBJISREG
//
// MessageText:
//
//  Object is already registered
//
  CO_E_OBJISREG = HRESULT($800401FC);

//
// MessageId: CO_E_OBJNOTCONNECTED
//
// MessageText:
//
//  Object is not connected to server
//
  CO_E_OBJNOTCONNECTED = HRESULT($800401FD);

//
// MessageId: CO_E_APPDIDNTREG
//
// MessageText:
//
//  Application was launched but it didn't register a class factory
//
  CO_E_APPDIDNTREG = HRESULT($800401FE);

//
// MessageId: CO_E_RELEASED
//
// MessageText:
//
//  Object has been released
//
  CO_E_RELEASED = HRESULT($800401FF);

  EVENT_E_FIRST = DWORD($80040200);
  EVENT_E_LAST = DWORD($8004021F);
  EVENT_S_FIRST = DWORD($00040200);
  EVENT_S_LAST = DWORD($0004021F);
//
// MessageId: EVENT_S_SOME_SUBSCRIBERS_FAILED
//
// MessageText:
//
//  An event was able to invoke some but not all of the subscribers
//
  EVENT_S_SOME_SUBSCRIBERS_FAILED = HRESULT($00040200);

//
// MessageId: EVENT_E_ALL_SUBSCRIBERS_FAILED
//
// MessageText:
//
//  An event was unable to invoke any of the subscribers
//
  EVENT_E_ALL_SUBSCRIBERS_FAILED = HRESULT($80040201);

//
// MessageId: EVENT_S_NOSUBSCRIBERS
//
// MessageText:
//
//  An event was delivered but there were no subscribers
//
  EVENT_S_NOSUBSCRIBERS = HRESULT($00040202);

//
// MessageId: EVENT_E_QUERYSYNTAX
//
// MessageText:
//
//  A syntax error occurred trying to evaluate a query string
//
  EVENT_E_QUERYSYNTAX = HRESULT($80040203);

//
// MessageId: EVENT_E_QUERYFIELD
//
// MessageText:
//
//  An invalid field name was used in a query string
//
  EVENT_E_QUERYFIELD = HRESULT($80040204);

//
// MessageId: EVENT_E_INTERNALEXCEPTION
//
// MessageText:
//
//  An unexpected exception was raised
//
  EVENT_E_INTERNALEXCEPTION = HRESULT($80040205);

//
// MessageId: EVENT_E_INTERNALERROR
//
// MessageText:
//
//  An unexpected internal error was detected
//
  EVENT_E_INTERNALERROR = HRESULT($80040206);

//
// MessageId: EVENT_E_INVALID_PER_USER_SID
//
// MessageText:
//
//  The owner SID on a per-user subscription doesn't exist
//
  EVENT_E_INVALID_PER_USER_SID = HRESULT($80040207);

//
// MessageId: EVENT_E_USER_EXCEPTION
//
// MessageText:
//
//  A user-supplied component or subscriber raised an exception
//
  EVENT_E_USER_EXCEPTION = HRESULT($80040208);

//
// MessageId: EVENT_E_TOO_MANY_METHODS
//
// MessageText:
//
//  An interface has too many methods to fire events from
//
  EVENT_E_TOO_MANY_METHODS = HRESULT($80040209);

//
// MessageId: EVENT_E_MISSING_EVENTCLASS
//
// MessageText:
//
//  A subscription cannot be stored unless its event class already exists
//
  EVENT_E_MISSING_EVENTCLASS = HRESULT($8004020A);

//
// MessageId: EVENT_E_NOT_ALL_REMOVED
//
// MessageText:
//
//  Not all the objects requested could be removed
//
  EVENT_E_NOT_ALL_REMOVED = HRESULT($8004020B);

//
// MessageId: EVENT_E_COMPLUS_NOT_INSTALLED
//
// MessageText:
//
//  COM+ is required for this operation, but is not installed
//
  EVENT_E_COMPLUS_NOT_INSTALLED = HRESULT($8004020C);

//
// MessageId: EVENT_E_CANT_MODIFY_OR_DELETE_UNCONFIGURED_OBJECT
//
// MessageText:
//
//  Cannot modify or delete an object that was not added using the COM+ Admin SDK
//
  EVENT_E_CANT_MODIFY_OR_DELETE_UNCONFIGURED_OBJECT = HRESULT($8004020D);

//
// MessageId: EVENT_E_CANT_MODIFY_OR_DELETE_CONFIGURED_OBJECT
//
// MessageText:
//
//  Cannot modify or delete an object that was added using the COM+ Admin SDK
//
  EVENT_E_CANT_MODIFY_OR_DELETE_CONFIGURED_OBJECT = HRESULT($8004020E);

//
// MessageId: EVENT_E_INVALID_EVENT_CLASS_PARTITION
//
// MessageText:
//
//  The event class for this subscription is in an invalid partition
//
  EVENT_E_INVALID_EVENT_CLASS_PARTITION = HRESULT($8004020F);

//
// MessageId: EVENT_E_PER_USER_SID_NOT_LOGGED_ON
//
// MessageText:
//
//  The owner of the PerUser subscription is not logged on to the system specified
//
  EVENT_E_PER_USER_SID_NOT_LOGGED_ON = HRESULT($80040210);

  XACT_E_FIRST = $8004D000;
  XACT_E_LAST = $8004D029;
  XACT_S_FIRST = $0004D000;
  XACT_S_LAST = $0004D010;
//
// MessageId: XACT_E_ALREADYOTHERSINGLEPHASE
//
// MessageText:
//
//  Another single phase resource manager has already been enlisted in this transaction.
//
  XACT_E_ALREADYOTHERSINGLEPHASE = HRESULT($8004D000);

//
// MessageId: XACT_E_CANTRETAIN
//
// MessageText:
//
//  A retaining commit or abort is not supported
//
  XACT_E_CANTRETAIN = HRESULT($8004D001);

//
// MessageId: XACT_E_COMMITFAILED
//
// MessageText:
//
//  The transaction failed to commit for an unknown reason. The transaction was aborted.
//
  XACT_E_COMMITFAILED = HRESULT($8004D002);

//
// MessageId: XACT_E_COMMITPREVENTED
//
// MessageText:
//
//  Cannot call commit on this transaction object because the calling application did not initiate the transaction.
//
  XACT_E_COMMITPREVENTED = HRESULT($8004D003);

//
// MessageId: XACT_E_HEURISTICABORT
//
// MessageText:
//
//  Instead of committing, the resource heuristically aborted.
//
  XACT_E_HEURISTICABORT = HRESULT($8004D004);

//
// MessageId: XACT_E_HEURISTICCOMMIT
//
// MessageText:
//
//  Instead of aborting, the resource heuristically committed.
//
  XACT_E_HEURISTICCOMMIT = HRESULT($8004D005);

//
// MessageId: XACT_E_HEURISTICDAMAGE
//
// MessageText:
//
//  Some of the states of the resource were committed while others were aborted, likely because of heuristic decisions.
//
  XACT_E_HEURISTICDAMAGE = HRESULT($8004D006);

//
// MessageId: XACT_E_HEURISTICDANGER
//
// MessageText:
//
//  Some of the states of the resource may have been committed while others may have been aborted, likely because of heuristic decisions.
//
  XACT_E_HEURISTICDANGER = HRESULT($8004D007);

//
// MessageId: XACT_E_ISOLATIONLEVEL
//
// MessageText:
//
//  The requested isolation level is not valid or supported.
//
  XACT_E_ISOLATIONLEVEL = HRESULT($8004D008);

//
// MessageId: XACT_E_NOASYNC
//
// MessageText:
//
//  The transaction manager doesn't support an asynchronous operation for this method.
//
  XACT_E_NOASYNC = HRESULT($8004D009);

//
// MessageId: XACT_E_NOENLIST
//
// MessageText:
//
//  Unable to enlist in the transaction.
//
  XACT_E_NOENLIST = HRESULT($8004D00A);

//
// MessageId: XACT_E_NOISORETAIN
//
// MessageText:
//
//  The requested semantics of retention of isolation across retaining commit and abort boundaries cannot be supported by this transaction implementation, or isoFlags was not equal to zero.
//
  XACT_E_NOISORETAIN = HRESULT($8004D00B);

//
// MessageId: XACT_E_NORESOURCE
//
// MessageText:
//
//  There is no resource presently associated with this enlistment
//
  XACT_E_NORESOURCE = HRESULT($8004D00C);

//
// MessageId: XACT_E_NOTCURRENT
//
// MessageText:
//
//  The transaction failed to commit due to the failure of optimistic concurrency control in at least one of the resource managers.
//
  XACT_E_NOTCURRENT = HRESULT($8004D00D);

//
// MessageId: XACT_E_NOTRANSACTION
//
// MessageText:
//
//  The transaction has already been implicitly or explicitly committed or aborted
//
  XACT_E_NOTRANSACTION = HRESULT($8004D00E);

//
// MessageId: XACT_E_NOTSUPPORTED
//
// MessageText:
//
//  An invalid combination of flags was specified
//
  XACT_E_NOTSUPPORTED = HRESULT($8004D00F);

//
// MessageId: XACT_E_UNKNOWNRMGRID
//
// MessageText:
//
//  The resource manager id is not associated with this transaction or the transaction manager.
//
  XACT_E_UNKNOWNRMGRID = HRESULT($8004D010);

//
// MessageId: XACT_E_WRONGSTATE
//
// MessageText:
//
//  This method was called in the wrong state
//
  XACT_E_WRONGSTATE = HRESULT($8004D011);

//
// MessageId: XACT_E_WRONGUOW
//
// MessageText:
//
//  The indicated unit of work does not match the unit of work expected by the resource manager.
//
  XACT_E_WRONGUOW = HRESULT($8004D012);

//
// MessageId: XACT_E_XTIONEXISTS
//
// MessageText:
//
//  An enlistment in a transaction already exists.
//
  XACT_E_XTIONEXISTS = HRESULT($8004D013);

//
// MessageId: XACT_E_NOIMPORTOBJECT
//
// MessageText:
//
//  An import object for the transaction could not be found.
//
  XACT_E_NOIMPORTOBJECT = HRESULT($8004D014);

//
// MessageId: XACT_E_INVALIDCOOKIE
//
// MessageText:
//
//  The transaction cookie is invalid.
//
  XACT_E_INVALIDCOOKIE = HRESULT($8004D015);

//
// MessageId: XACT_E_INDOUBT
//
// MessageText:
//
//  The transaction status is in doubt. A communication failure occurred, or a transaction manager or resource manager has failed
//
  XACT_E_INDOUBT = HRESULT($8004D016);

//
// MessageId: XACT_E_NOTIMEOUT
//
// MessageText:
//
//  A time-out was specified, but time-outs are not supported.
//
  XACT_E_NOTIMEOUT = HRESULT($8004D017);

//
// MessageId: XACT_E_ALREADYINPROGRESS
//
// MessageText:
//
//  The requested operation is already in progress for the transaction.
//
  XACT_E_ALREADYINPROGRESS = HRESULT($8004D018);

//
// MessageId: XACT_E_ABORTED
//
// MessageText:
//
//  The transaction has already been aborted.
//
  XACT_E_ABORTED = HRESULT($8004D019);

//
// MessageId: XACT_E_LOGFULL
//
// MessageText:
//
//  The Transaction Manager returned a log full error.
//
  XACT_E_LOGFULL = HRESULT($8004D01A);

//
// MessageId: XACT_E_TMNOTAVAILABLE
//
// MessageText:
//
//  The Transaction Manager is not available.
//
  XACT_E_TMNOTAVAILABLE = HRESULT($8004D01B);

//
// MessageId: XACT_E_CONNECTION_DOWN
//
// MessageText:
//
//  A connection with the transaction manager was lost.
//
  XACT_E_CONNECTION_DOWN = HRESULT($8004D01C);

//
// MessageId: XACT_E_CONNECTION_DENIED
//
// MessageText:
//
//  A request to establish a connection with the transaction manager was denied.
//
  XACT_E_CONNECTION_DENIED = HRESULT($8004D01D);

//
// MessageId: XACT_E_REENLISTTIMEOUT
//
// MessageText:
//
//  Resource manager reenlistment to determine transaction status timed out.
//
  XACT_E_REENLISTTIMEOUT = HRESULT($8004D01E);

//
// MessageId: XACT_E_TIP_CONNECT_FAILED
//
// MessageText:
//
//  This transaction manager failed to establish a connection with another TIP transaction manager.
//
  XACT_E_TIP_CONNECT_FAILED = HRESULT($8004D01F);

//
// MessageId: XACT_E_TIP_PROTOCOL_ERROR
//
// MessageText:
//
//  This transaction manager encountered a protocol error with another TIP transaction manager.
//
  XACT_E_TIP_PROTOCOL_ERROR = HRESULT($8004D020);

//
// MessageId: XACT_E_TIP_PULL_FAILED
//
// MessageText:
//
//  This transaction manager could not propagate a transaction from another TIP transaction manager.
//
  XACT_E_TIP_PULL_FAILED = HRESULT($8004D021);

//
// MessageId: XACT_E_DEST_TMNOTAVAILABLE
//
// MessageText:
//
//  The Transaction Manager on the destination machine is not available.
//
  XACT_E_DEST_TMNOTAVAILABLE = HRESULT($8004D022);

//
// MessageId: XACT_E_TIP_DISABLED
//
// MessageText:
//
//  The Transaction Manager has disabled its support for TIP.
//
  XACT_E_TIP_DISABLED = HRESULT($8004D023);

//
// MessageId: XACT_E_NETWORK_TX_DISABLED
//
// MessageText:
//
//  The transaction manager has disabled its support for remote/network transactions.
//
  XACT_E_NETWORK_TX_DISABLED = HRESULT($8004D024);

//
// MessageId: XACT_E_PARTNER_NETWORK_TX_DISABLED
//
// MessageText:
//
//  The partner transaction manager has disabled its support for remote/network transactions.
//
  XACT_E_PARTNER_NETWORK_TX_DISABLED = HRESULT($8004D025);

//
// MessageId: XACT_E_XA_TX_DISABLED
//
// MessageText:
//
//  The transaction manager has disabled its support for XA transactions.
//
  XACT_E_XA_TX_DISABLED = HRESULT($8004D026);

//
// MessageId: XACT_E_UNABLE_TO_READ_DTC_CONFIG
//
// MessageText:
//
//  MSDTC was unable to read its configuration information.
//
  XACT_E_UNABLE_TO_READ_DTC_CONFIG = HRESULT($8004D027);

//
// MessageId: XACT_E_UNABLE_TO_LOAD_DTC_PROXY
//
// MessageText:
//
//  MSDTC was unable to load the dtc proxy dll.
//
  XACT_E_UNABLE_TO_LOAD_DTC_PROXY = HRESULT($8004D028);

//
// MessageId: XACT_E_ABORTING
//
// MessageText:
//
//  The local transaction has aborted.
//
  XACT_E_ABORTING = HRESULT($8004D029);

//
// TXF & CRM errors start 4d080.
//
// MessageId: XACT_E_CLERKNOTFOUND
//
// MessageText:
//
//  XACT_E_CLERKNOTFOUND
//
  XACT_E_CLERKNOTFOUND = HRESULT($8004D080);

//
// MessageId: XACT_E_CLERKEXISTS
//
// MessageText:
//
//  XACT_E_CLERKEXISTS
//
  XACT_E_CLERKEXISTS = HRESULT($8004D081);

//
// MessageId: XACT_E_RECOVERYINPROGRESS
//
// MessageText:
//
//  XACT_E_RECOVERYINPROGRESS
//
  XACT_E_RECOVERYINPROGRESS = HRESULT($8004D082);

//
// MessageId: XACT_E_TRANSACTIONCLOSED
//
// MessageText:
//
//  XACT_E_TRANSACTIONCLOSED
//
  XACT_E_TRANSACTIONCLOSED = HRESULT($8004D083);

//
// MessageId: XACT_E_INVALIDLSN
//
// MessageText:
//
//  XACT_E_INVALIDLSN
//
  XACT_E_INVALIDLSN = HRESULT($8004D084);

//
// MessageId: XACT_E_REPLAYREQUEST
//
// MessageText:
//
//  XACT_E_REPLAYREQUEST
//
  XACT_E_REPLAYREQUEST = HRESULT($8004D085);

//
// OleTx Success codes.
//
//
// MessageId: XACT_S_ASYNC
//
// MessageText:
//
//  An asynchronous operation was specified. The operation has begun, but its outcome is not known yet.
//
  XACT_S_ASYNC = HRESULT($0004D000);

//
// MessageId: XACT_S_DEFECT
//
// MessageText:
//
//  XACT_S_DEFECT
//
  XACT_S_DEFECT = HRESULT($0004D001);

//
// MessageId: XACT_S_READONLY
//
// MessageText:
//
//  The method call succeeded because the transaction was read-only.
//
  XACT_S_READONLY = HRESULT($0004D002);

//
// MessageId: XACT_S_SOMENORETAIN
//
// MessageText:
//
//  The transaction was successfully aborted. However, this is a coordinated transaction, and some number of enlisted resources were aborted outright because they could not support abort-retaining semantics
//
  XACT_S_SOMENORETAIN = HRESULT($0004D003);

//
// MessageId: XACT_S_OKINFORM
//
// MessageText:
//
//  No changes were made during this call, but the sink wants another chance to look if any other sinks make further changes.
//
  XACT_S_OKINFORM = HRESULT($0004D004);

//
// MessageId: XACT_S_MADECHANGESCONTENT
//
// MessageText:
//
//  The sink is content and wishes the transaction to proceed. Changes were made to one or more resources during this call.
//
  XACT_S_MADECHANGESCONTENT = HRESULT($0004D005);

//
// MessageId: XACT_S_MADECHANGESINFORM
//
// MessageText:
//
//  The sink is for the moment and wishes the transaction to proceed, but if other changes are made following this return by other event sinks then this sink wants another chance to look
//
  XACT_S_MADECHANGESINFORM = HRESULT($0004D006);

//
// MessageId: XACT_S_ALLNORETAIN
//
// MessageText:
//
//  The transaction was successfully aborted. However, the abort was non-retaining.
//
  XACT_S_ALLNORETAIN = HRESULT($0004D007);

//
// MessageId: XACT_S_ABORTING
//
// MessageText:
//
//  An abort operation was already in progress.
//
  XACT_S_ABORTING = HRESULT($0004D008);

//
// MessageId: XACT_S_SINGLEPHASE
//
// MessageText:
//
//  The resource manager has performed a single-phase commit of the transaction.
//
  XACT_S_SINGLEPHASE = HRESULT($0004D009);

//
// MessageId: XACT_S_LOCALLY_OK
//
// MessageText:
//
//  The local transaction has not aborted.
//
  XACT_S_LOCALLY_OK = HRESULT($0004D00A);

//
// MessageId: XACT_S_LASTRESOURCEMANAGER
//
// MessageText:
//
//  The resource manager has requested to be the coordinator (last resource manager) for the transaction.
//
  XACT_S_LASTRESOURCEMANAGER = HRESULT($0004D010);

  CONTEXT_E_FIRST = DWORD($8004E000);
  CONTEXT_E_LAST = DWORD($8004E02F);
  CONTEXT_S_FIRST = DWORD($0004E000);
  CONTEXT_S_LAST = DWORD($0004E02F);
//
// MessageId: CONTEXT_E_ABORTED
//
// MessageText:
//
//  The root transaction wanted to commit, but transaction aborted
//
  CONTEXT_E_ABORTED = HRESULT($8004E002);

//
// MessageId: CONTEXT_E_ABORTING
//
// MessageText:
//
//  You made a method call on a COM+ component that has a transaction that has already aborted or in the process of aborting.
//
  CONTEXT_E_ABORTING = HRESULT($8004E003);

//
// MessageId: CONTEXT_E_NOCONTEXT
//
// MessageText:
//
//  There is no MTS object context
//
  CONTEXT_E_NOCONTEXT = HRESULT($8004E004);

//
// MessageId: CONTEXT_E_WOULD_DEADLOCK
//
// MessageText:
//
//  The component is configured to use synchronization and this method call would cause a deadlock to occur.
//
  CONTEXT_E_WOULD_DEADLOCK = HRESULT($8004E005);

//
// MessageId: CONTEXT_E_SYNCH_TIMEOUT
//
// MessageText:
//
//  The component is configured to use synchronization and a thread has timed out waiting to enter the context.
//
  CONTEXT_E_SYNCH_TIMEOUT = HRESULT($8004E006);

//
// MessageId: CONTEXT_E_OLDREF
//
// MessageText:
//
//  You made a method call on a COM+ component that has a transaction that has already committed or aborted.
//
  CONTEXT_E_OLDREF = HRESULT($8004E007);

//
// MessageId: CONTEXT_E_ROLENOTFOUND
//
// MessageText:
//
//  The specified role was not configured for the application
//
  CONTEXT_E_ROLENOTFOUND = HRESULT($8004E00C);

//
// MessageId: CONTEXT_E_TMNOTAVAILABLE
//
// MessageText:
//
//  COM+ was unable to talk to the Microsoft Distributed Transaction Coordinator
//
  CONTEXT_E_TMNOTAVAILABLE = HRESULT($8004E00F);

//
// MessageId: CO_E_ACTIVATIONFAILED
//
// MessageText:
//
//  An unexpected error occurred during COM+ Activation.
//
  CO_E_ACTIVATIONFAILED = HRESULT($8004E021);

//
// MessageId: CO_E_ACTIVATIONFAILED_EVENTLOGGED
//
// MessageText:
//
//  COM+ Activation failed. Check the event log for more information
//
  CO_E_ACTIVATIONFAILED_EVENTLOGGED = HRESULT($8004E022);

//
// MessageId: CO_E_ACTIVATIONFAILED_CATALOGERROR
//
// MessageText:
//
//  COM+ Activation failed due to a catalog or configuration error.
//
  CO_E_ACTIVATIONFAILED_CATALOGERROR = HRESULT($8004E023);

//
// MessageId: CO_E_ACTIVATIONFAILED_TIMEOUT
//
// MessageText:
//
//  COM+ activation failed because the activation could not be completed in the specified amount of time.
//
  CO_E_ACTIVATIONFAILED_TIMEOUT = HRESULT($8004E024);

//
// MessageId: CO_E_INITIALIZATIONFAILED
//
// MessageText:
//
//  COM+ Activation failed because an initialization function failed.  Check the event log for more information.
//
  CO_E_INITIALIZATIONFAILED = HRESULT($8004E025);

//
// MessageId: CONTEXT_E_NOJIT
//
// MessageText:
//
//  The requested operation requires that JIT be in the current context and it is not
//
  CONTEXT_E_NOJIT = HRESULT($8004E026);

//
// MessageId: CONTEXT_E_NOTRANSACTION
//
// MessageText:
//
//  The requested operation requires that the current context have a Transaction, and it does not
//
  CONTEXT_E_NOTRANSACTION = HRESULT($8004E027);

//
// MessageId: CO_E_THREADINGMODEL_CHANGED
//
// MessageText:
//
//  The components threading model has changed after install into a COM+ Application.  Please re-install component.
//
  CO_E_THREADINGMODEL_CHANGED = HRESULT($8004E028);

//
// MessageId: CO_E_NOIISINTRINSICS
//
// MessageText:
//
//  IIS intrinsics not available.  Start your work with IIS.
//
  CO_E_NOIISINTRINSICS = HRESULT($8004E029);

//
// MessageId: CO_E_NOCOOKIES
//
// MessageText:
//
//  An attempt to write a cookie failed.
//
  CO_E_NOCOOKIES = HRESULT($8004E02A);

//
// MessageId: CO_E_DBERROR
//
// MessageText:
//
//  An attempt to use a database generated a database specific error.
//
  CO_E_DBERROR = HRESULT($8004E02B);

//
// MessageId: CO_E_NOTPOOLED
//
// MessageText:
//
//  The COM+ component you created must use object pooling to work.
//
  CO_E_NOTPOOLED = HRESULT($8004E02C);

//
// MessageId: CO_E_NOTCONSTRUCTED
//
// MessageText:
//
//  The COM+ component you created must use object construction to work correctly.
//
  CO_E_NOTCONSTRUCTED = HRESULT($8004E02D);

//
// MessageId: CO_E_NOSYNCHRONIZATION
//
// MessageText:
//
//  The COM+ component requires synchronization, and it is not configured for it.
//
  CO_E_NOSYNCHRONIZATION = HRESULT($8004E02E);

//
// MessageId: CO_E_ISOLEVELMISMATCH
//
// MessageText:
//
//  The TxIsolation Level property for the COM+ component being created is stronger than the TxIsolationLevel for the "root" component for the transaction.  The creation failed.
//
  CO_E_ISOLEVELMISMATCH = HRESULT($8004E02F);

//
// Old OLE Success Codes
//
//
// MessageId: OLE_S_USEREG
//
// MessageText:
//
//  Use the registry database to provide the requested information
//
  OLE_S_USEREG = HRESULT($00040000);

//
// MessageId: OLE_S_STATIC
//
// MessageText:
//
//  Success, but static
//
  OLE_S_STATIC = HRESULT($00040001);

//
// MessageId: OLE_S_MAC_CLIPFORMAT
//
// MessageText:
//
//  Macintosh clipboard format
//
  OLE_S_MAC_CLIPFORMAT = HRESULT($00040002);

//
// MessageId: DRAGDROP_S_DROP
//
// MessageText:
//
//  Successful drop took place
//
  DRAGDROP_S_DROP = HRESULT($00040100);

//
// MessageId: DRAGDROP_S_CANCEL
//
// MessageText:
//
//  Drag-drop operation canceled
//
  DRAGDROP_S_CANCEL = HRESULT($00040101);

//
// MessageId: DRAGDROP_S_USEDEFAULTCURSORS
//
// MessageText:
//
//  Use the default cursor
//
  DRAGDROP_S_USEDEFAULTCURSORS = HRESULT($00040102);

//
// MessageId: DATA_S_SAMEFORMATETC
//
// MessageText:
//
//  Data has same FORMATETC
//
  DATA_S_SAMEFORMATETC = HRESULT($00040130);

//
// MessageId: VIEW_S_ALREADY_FROZEN
//
// MessageText:
//
//  View is already frozen
//
  VIEW_S_ALREADY_FROZEN = HRESULT($00040140);

//
// MessageId: CACHE_S_FORMATETC_NOTSUPPORTED
//
// MessageText:
//
//  FORMATETC not supported
//
  CACHE_S_FORMATETC_NOTSUPPORTED = HRESULT($00040170);

//
// MessageId: CACHE_S_SAMECACHE
//
// MessageText:
//
//  Same cache
//
  CACHE_S_SAMECACHE = HRESULT($00040171);

//
// MessageId: CACHE_S_SOMECACHES_NOTUPDATED
//
// MessageText:
//
//  Some cache(s) not updated
//
  CACHE_S_SOMECACHES_NOTUPDATED = HRESULT($00040172);

//
// MessageId: OLEOBJ_S_INVALIDVERB
//
// MessageText:
//
//  Invalid verb for OLE object
//
  OLEOBJ_S_INVALIDVERB = HRESULT($00040180);

//
// MessageId: OLEOBJ_S_CANNOT_DOVERB_NOW
//
// MessageText:
//
//  Verb number is valid but verb cannot be done now
//
  OLEOBJ_S_CANNOT_DOVERB_NOW = HRESULT($00040181);

//
// MessageId: OLEOBJ_S_INVALIDHWND
//
// MessageText:
//
//  Invalid window handle passed
//
  OLEOBJ_S_INVALIDHWND = HRESULT($00040182);

//
// MessageId: INPLACE_S_TRUNCATED
//
// MessageText:
//
//  Message is too long; some of it had to be truncated before displaying
//
  INPLACE_S_TRUNCATED = HRESULT($000401A0);

//
// MessageId: CONVERT10_S_NO_PRESENTATION
//
// MessageText:
//
//  Unable to convert OLESTREAM to IStorage
//
  CONVERT10_S_NO_PRESENTATION = HRESULT($000401C0);

//
// MessageId: MK_S_REDUCED_TO_SELF
//
// MessageText:
//
//  Moniker reduced to itself
//
  MK_S_REDUCED_TO_SELF = HRESULT($000401E2);

//
// MessageId: MK_S_ME
//
// MessageText:
//
//  Common prefix is this moniker
//
  MK_S_ME = HRESULT($000401E4);

//
// MessageId: MK_S_HIM
//
// MessageText:
//
//  Common prefix is input moniker
//
  MK_S_HIM = HRESULT($000401E5);

//
// MessageId: MK_S_US
//
// MessageText:
//
//  Common prefix is both monikers
//
  MK_S_US = HRESULT($000401E6);

//
// MessageId: MK_S_MONIKERALREADYREGISTERED
//
// MessageText:
//
//  Moniker is already registered in running object table
//
  MK_S_MONIKERALREADYREGISTERED = HRESULT($000401E7);

//
// Task Scheduler errors
//
//
// MessageId: SCHED_S_TASK_READY
//
// MessageText:
//
//  The task is ready to run at its next scheduled time.
//
  SCHED_S_TASK_READY = HRESULT($00041300);

//
// MessageId: SCHED_S_TASK_RUNNING
//
// MessageText:
//
//  The task is currently running.
//
  SCHED_S_TASK_RUNNING = HRESULT($00041301);

//
// MessageId: SCHED_S_TASK_DISABLED
//
// MessageText:
//
//  The task will not run at the scheduled times because it has been disabled.
//
  SCHED_S_TASK_DISABLED = HRESULT($00041302);

//
// MessageId: SCHED_S_TASK_HAS_NOT_RUN
//
// MessageText:
//
//  The task has not yet run.
//
  SCHED_S_TASK_HAS_NOT_RUN = HRESULT($00041303);

//
// MessageId: SCHED_S_TASK_NO_MORE_RUNS
//
// MessageText:
//
//  There are no more runs scheduled for this task.
//
  SCHED_S_TASK_NO_MORE_RUNS = HRESULT($00041304);

//
// MessageId: SCHED_S_TASK_NOT_SCHEDULED
//
// MessageText:
//
//  One or more of the properties that are needed to run this task on a schedule have not been set.
//
  SCHED_S_TASK_NOT_SCHEDULED = HRESULT($00041305);

//
// MessageId: SCHED_S_TASK_TERMINATED
//
// MessageText:
//
//  The last run of the task was terminated by the user.
//
  SCHED_S_TASK_TERMINATED = HRESULT($00041306);

//
// MessageId: SCHED_S_TASK_NO_VALID_TRIGGERS
//
// MessageText:
//
//  Either the task has no triggers or the existing triggers are disabled or not set.
//
  SCHED_S_TASK_NO_VALID_TRIGGERS = HRESULT($00041307);

//
// MessageId: SCHED_S_EVENT_TRIGGER
//
// MessageText:
//
//  Event triggers don't have set run times.
//
  SCHED_S_EVENT_TRIGGER = HRESULT($00041308);

//
// MessageId: SCHED_E_TRIGGER_NOT_FOUND
//
// MessageText:
//
//  Trigger not found.
//
  SCHED_E_TRIGGER_NOT_FOUND = HRESULT($80041309);

//
// MessageId: SCHED_E_TASK_NOT_READY
//
// MessageText:
//
//  One or more of the properties that are needed to run this task have not been set.
//
  SCHED_E_TASK_NOT_READY = HRESULT($8004130A);

//
// MessageId: SCHED_E_TASK_NOT_RUNNING
//
// MessageText:
//
//  There is no running instance of the task to terminate.
//
  SCHED_E_TASK_NOT_RUNNING = HRESULT($8004130B);

//
// MessageId: SCHED_E_SERVICE_NOT_INSTALLED
//
// MessageText:
//
//  The Task Scheduler Service is not installed on this computer.
//
  SCHED_E_SERVICE_NOT_INSTALLED = HRESULT($8004130C);

//
// MessageId: SCHED_E_CANNOT_OPEN_TASK
//
// MessageText:
//
//  The task object could not be opened.
//
  SCHED_E_CANNOT_OPEN_TASK = HRESULT($8004130D);

//
// MessageId: SCHED_E_INVALID_TASK
//
// MessageText:
//
//  The object is either an invalid task object or is not a task object.
//
  SCHED_E_INVALID_TASK = HRESULT($8004130E);

//
// MessageId: SCHED_E_ACCOUNT_INFORMATION_NOT_SET
//
// MessageText:
//
//  No account information could be found in the Task Scheduler security database for the task indicated.
//
  SCHED_E_ACCOUNT_INFORMATION_NOT_SET = HRESULT($8004130F);

//
// MessageId: SCHED_E_ACCOUNT_NAME_NOT_FOUND
//
// MessageText:
//
//  Unable to establish existence of the account specified.
//
  SCHED_E_ACCOUNT_NAME_NOT_FOUND = HRESULT($80041310);

//
// MessageId: SCHED_E_ACCOUNT_DBASE_CORRUPT
//
// MessageText:
//
//  Corruption was detected in the Task Scheduler security database; the database has been reset.
//
  SCHED_E_ACCOUNT_DBASE_CORRUPT = HRESULT($80041311);

//
// MessageId: SCHED_E_NO_SECURITY_SERVICES
//
// MessageText:
//
//  Task Scheduler security services are available only on Windows NT.
//
  SCHED_E_NO_SECURITY_SERVICES = HRESULT($80041312);

//
// MessageId: SCHED_E_UNKNOWN_OBJECT_VERSION
//
// MessageText:
//
//  The task object version is either unsupported or invalid.
//
  SCHED_E_UNKNOWN_OBJECT_VERSION = HRESULT($80041313);

//
// MessageId: SCHED_E_UNSUPPORTED_ACCOUNT_OPTION
//
// MessageText:
//
//  The task has been configured with an unsupported combination of account settings and run time options.
//
  SCHED_E_UNSUPPORTED_ACCOUNT_OPTION = HRESULT($80041314);

//
// MessageId: SCHED_E_SERVICE_NOT_RUNNING
//
// MessageText:
//
//  The Task Scheduler Service is not running.
//
  SCHED_E_SERVICE_NOT_RUNNING = HRESULT($80041315);

// ******************
// FACILITY_WINDOWS
// ******************
//
// Codes 0x0-0x01ff are reserved for the OLE group of
// interfaces.
//
//
// MessageId: CO_E_CLASS_CREATE_FAILED
//
// MessageText:
//
//  Attempt to create a class object failed
//
  CO_E_CLASS_CREATE_FAILED = HRESULT($80080001);

//
// MessageId: CO_E_SCM_ERROR
//
// MessageText:
//
//  OLE service could not bind object
//
  CO_E_SCM_ERROR = HRESULT($80080002);

//
// MessageId: CO_E_SCM_RPC_FAILURE
//
// MessageText:
//
//  RPC communication failed with OLE service
//
  CO_E_SCM_RPC_FAILURE = HRESULT($80080003);

//
// MessageId: CO_E_BAD_PATH
//
// MessageText:
//
//  Bad path to object
//
  CO_E_BAD_PATH = HRESULT($80080004);

//
// MessageId: CO_E_SERVER_EXEC_FAILURE
//
// MessageText:
//
//  Server execution failed
//
  CO_E_SERVER_EXEC_FAILURE = HRESULT($80080005);

//
// MessageId: CO_E_OBJSRV_RPC_FAILURE
//
// MessageText:
//
//  OLE service could not communicate with the object server
//
  CO_E_OBJSRV_RPC_FAILURE = HRESULT($80080006);

//
// MessageId: MK_E_NO_NORMALIZED
//
// MessageText:
//
//  Moniker path could not be normalized
//
  MK_E_NO_NORMALIZED = HRESULT($80080007);

//
// MessageId: CO_E_SERVER_STOPPING
//
// MessageText:
//
//  Object server is stopping when OLE service contacts it
//
  CO_E_SERVER_STOPPING = HRESULT($80080008);

//
// MessageId: MEM_E_INVALID_ROOT
//
// MessageText:
//
//  An invalid root block pointer was specified
//
  MEM_E_INVALID_ROOT = HRESULT($80080009);

//
// MessageId: MEM_E_INVALID_LINK
//
// MessageText:
//
//  An allocation chain contained an invalid link pointer
//
  MEM_E_INVALID_LINK = HRESULT($80080010);

//
// MessageId: MEM_E_INVALID_SIZE
//
// MessageText:
//
//  The requested allocation size was too large
//
  MEM_E_INVALID_SIZE = HRESULT($80080011);

//
// MessageId: CO_S_NOTALLINTERFACES
//
// MessageText:
//
//  Not all the requested interfaces were available
//
  CO_S_NOTALLINTERFACES = HRESULT($00080012);

//
// MessageId: CO_S_MACHINENAMENOTFOUND
//
// MessageText:
//
//  The specified machine name was not found in the cache.
//
  CO_S_MACHINENAMENOTFOUND = HRESULT($00080013);

// ******************
// FACILITY_DISPATCH
// ******************
//
// MessageId: DISP_E_UNKNOWNINTERFACE
//
// MessageText:
//
//  Unknown interface.
//
  DISP_E_UNKNOWNINTERFACE = HRESULT($80020001);

//
// MessageId: DISP_E_MEMBERNOTFOUND
//
// MessageText:
//
//  Member not found.
//
  DISP_E_MEMBERNOTFOUND = HRESULT($80020003);

//
// MessageId: DISP_E_PARAMNOTFOUND
//
// MessageText:
//
//  Parameter not found.
//
  DISP_E_PARAMNOTFOUND = HRESULT($80020004);

//
// MessageId: DISP_E_TYPEMISMATCH
//
// MessageText:
//
//  Type mismatch.
//
  DISP_E_TYPEMISMATCH = HRESULT($80020005);

//
// MessageId: DISP_E_UNKNOWNNAME
//
// MessageText:
//
//  Unknown name.
//
  DISP_E_UNKNOWNNAME = HRESULT($80020006);

//
// MessageId: DISP_E_NONAMEDARGS
//
// MessageText:
//
//  No named arguments.
//
  DISP_E_NONAMEDARGS = HRESULT($80020007);

//
// MessageId: DISP_E_BADVARTYPE
//
// MessageText:
//
//  Bad variable type.
//
  DISP_E_BADVARTYPE = HRESULT($80020008);

//
// MessageId: DISP_E_EXCEPTION
//
// MessageText:
//
//  Exception occurred.
//
  DISP_E_EXCEPTION = HRESULT($80020009);

//
// MessageId: DISP_E_OVERFLOW
//
// MessageText:
//
//  Out of present range.
//
  DISP_E_OVERFLOW = HRESULT($8002000A);

//
// MessageId: DISP_E_BADINDEX
//
// MessageText:
//
//  Invalid index.
//
  DISP_E_BADINDEX = HRESULT($8002000B);

//
// MessageId: DISP_E_UNKNOWNLCID
//
// MessageText:
//
//  Unknown language.
//
  DISP_E_UNKNOWNLCID = HRESULT($8002000C);

//
// MessageId: DISP_E_ARRAYISLOCKED
//
// MessageText:
//
//  Memory is locked.
//
  DISP_E_ARRAYISLOCKED = HRESULT($8002000D);

//
// MessageId: DISP_E_BADPARAMCOUNT
//
// MessageText:
//
//  Invalid number of parameters.
//
  DISP_E_BADPARAMCOUNT = HRESULT($8002000E);

//
// MessageId: DISP_E_PARAMNOTOPTIONAL
//
// MessageText:
//
//  Parameter not optional.
//
  DISP_E_PARAMNOTOPTIONAL = HRESULT($8002000F);

//
// MessageId: DISP_E_BADCALLEE
//
// MessageText:
//
//  Invalid callee.
//
  DISP_E_BADCALLEE = HRESULT($80020010);

//
// MessageId: DISP_E_NOTACOLLECTION
//
// MessageText:
//
//  Does not support a collection.
//
  DISP_E_NOTACOLLECTION = HRESULT($80020011);

//
// MessageId: DISP_E_DIVBYZERO
//
// MessageText:
//
//  Division by zero.
//
  DISP_E_DIVBYZERO = HRESULT($80020012);

//
// MessageId: DISP_E_BUFFERTOOSMALL
//
// MessageText:
//
//  Buffer too small
//
  DISP_E_BUFFERTOOSMALL = HRESULT($80020013);

//
// MessageId: TYPE_E_BUFFERTOOSMALL
//
// MessageText:
//
//  Buffer too small.
//
  TYPE_E_BUFFERTOOSMALL = HRESULT($80028016);

//
// MessageId: TYPE_E_FIELDNOTFOUND
//
// MessageText:
//
//  Field name not defined in the record.
//
  TYPE_E_FIELDNOTFOUND = HRESULT($80028017);

//
// MessageId: TYPE_E_INVDATAREAD
//
// MessageText:
//
//  Old format or invalid type library.
//
  TYPE_E_INVDATAREAD = HRESULT($80028018);

//
// MessageId: TYPE_E_UNSUPFORMAT
//
// MessageText:
//
//  Old format or invalid type library.
//
  TYPE_E_UNSUPFORMAT = HRESULT($80028019);

//
// MessageId: TYPE_E_REGISTRYACCESS
//
// MessageText:
//
//  Error accessing the OLE registry.
//
  TYPE_E_REGISTRYACCESS = HRESULT($8002801C);

//
// MessageId: TYPE_E_LIBNOTREGISTERED
//
// MessageText:
//
//  Library not registered.
//
  TYPE_E_LIBNOTREGISTERED = HRESULT($8002801D);

//
// MessageId: TYPE_E_UNDEFINEDTYPE
//
// MessageText:
//
//  Bound to unknown type.
//
  TYPE_E_UNDEFINEDTYPE = HRESULT($80028027);

//
// MessageId: TYPE_E_QUALIFIEDNAMEDISALLOWED
//
// MessageText:
//
//  Qualified name disallowed.
//
  TYPE_E_QUALIFIEDNAMEDISALLOWED = HRESULT($80028028);

//
// MessageId: TYPE_E_INVALIDSTATE
//
// MessageText:
//
//  Invalid forward reference, or reference to uncompiled type.
//
  TYPE_E_INVALIDSTATE = HRESULT($80028029);

//
// MessageId: TYPE_E_WRONGTYPEKIND
//
// MessageText:
//
//  Type mismatch.
//
  TYPE_E_WRONGTYPEKIND = HRESULT($8002802A);

//
// MessageId: TYPE_E_ELEMENTNOTFOUND
//
// MessageText:
//
//  Element not found.
//
  TYPE_E_ELEMENTNOTFOUND = HRESULT($8002802B);

//
// MessageId: TYPE_E_AMBIGUOUSNAME
//
// MessageText:
//
//  Ambiguous name.
//
  TYPE_E_AMBIGUOUSNAME = HRESULT($8002802C);

//
// MessageId: TYPE_E_NAMECONFLICT
//
// MessageText:
//
//  Name already exists in the library.
//
  TYPE_E_NAMECONFLICT = HRESULT($8002802D);

//
// MessageId: TYPE_E_UNKNOWNLCID
//
// MessageText:
//
//  Unknown LCID.
//
  TYPE_E_UNKNOWNLCID = HRESULT($8002802E);

//
// MessageId: TYPE_E_DLLFUNCTIONNOTFOUND
//
// MessageText:
//
//  Function not defined in specified DLL.
//
  TYPE_E_DLLFUNCTIONNOTFOUND = HRESULT($8002802F);

//
// MessageId: TYPE_E_BADMODULEKIND
//
// MessageText:
//
//  Wrong module kind for the operation.
//
  TYPE_E_BADMODULEKIND = HRESULT($800288BD);

//
// MessageId: TYPE_E_SIZETOOBIG
//
// MessageText:
//
//  Size may not exceed 64K.
//
  TYPE_E_SIZETOOBIG = HRESULT($800288C5);

//
// MessageId: TYPE_E_DUPLICATEID
//
// MessageText:
//
//  Duplicate ID in inheritance hierarchy.
//
  TYPE_E_DUPLICATEID = HRESULT($800288C6);

//
// MessageId: TYPE_E_INVALIDID
//
// MessageText:
//
//  Incorrect inheritance depth in standard OLE hmember.
//
  TYPE_E_INVALIDID = HRESULT($800288CF);

//
// MessageId: TYPE_E_TYPEMISMATCH
//
// MessageText:
//
//  Type mismatch.
//
  TYPE_E_TYPEMISMATCH = HRESULT($80028CA0);

//
// MessageId: TYPE_E_OUTOFBOUNDS
//
// MessageText:
//
//  Invalid number of arguments.
//
  TYPE_E_OUTOFBOUNDS = HRESULT($80028CA1);

//
// MessageId: TYPE_E_IOERROR
//
// MessageText:
//
//  I/O Error.
//
  TYPE_E_IOERROR = HRESULT($80028CA2);

//
// MessageId: TYPE_E_CANTCREATETMPFILE
//
// MessageText:
//
//  Error creating unique tmp file.
//
  TYPE_E_CANTCREATETMPFILE = HRESULT($80028CA3);

//
// MessageId: TYPE_E_CANTLOADLIBRARY
//
// MessageText:
//
//  Error loading type library/DLL.
//
  TYPE_E_CANTLOADLIBRARY = HRESULT($80029C4A);

//
// MessageId: TYPE_E_INCONSISTENTPROPFUNCS
//
// MessageText:
//
//  Inconsistent property functions.
//
  TYPE_E_INCONSISTENTPROPFUNCS = HRESULT($80029C83);

//
// MessageId: TYPE_E_CIRCULARTYPE
//
// MessageText:
//
//  Circular dependency between types/modules.
//
  TYPE_E_CIRCULARTYPE = HRESULT($80029C84);

// ******************
// FACILITY_STORAGE
// ******************
//
// MessageId: STG_E_INVALIDFUNCTION
//
// MessageText:
//
//  Unable to perform requested operation.
//
  STG_E_INVALIDFUNCTION = HRESULT($80030001);

//
// MessageId: STG_E_FILENOTFOUND
//
// MessageText:
//
//  %1 could not be found.
//
  STG_E_FILENOTFOUND = HRESULT($80030002);

//
// MessageId: STG_E_PATHNOTFOUND
//
// MessageText:
//
//  The path %1 could not be found.
//
  STG_E_PATHNOTFOUND = HRESULT($80030003);

//
// MessageId: STG_E_TOOMANYOPENFILES
//
// MessageText:
//
//  There are insufficient resources to open another file.
//
  STG_E_TOOMANYOPENFILES = HRESULT($80030004);

//
// MessageId: STG_E_ACCESSDENIED
//
// MessageText:
//
//  Access Denied.
//
  STG_E_ACCESSDENIED = HRESULT($80030005);

//
// MessageId: STG_E_INVALIDHANDLE
//
// MessageText:
//
//  Attempted an operation on an invalid object.
//
  STG_E_INVALIDHANDLE = HRESULT($80030006);

//
// MessageId: STG_E_INSUFFICIENTMEMORY
//
// MessageText:
//
//  There is insufficient memory available to complete operation.
//
  STG_E_INSUFFICIENTMEMORY = HRESULT($80030008);

//
// MessageId: STG_E_INVALIDPOINTER
//
// MessageText:
//
//  Invalid pointer error.
//
  STG_E_INVALIDPOINTER = HRESULT($80030009);

//
// MessageId: STG_E_NOMOREFILES
//
// MessageText:
//
//  There are no more entries to return.
//
  STG_E_NOMOREFILES = HRESULT($80030012);

//
// MessageId: STG_E_DISKISWRITEPROTECTED
//
// MessageText:
//
//  Disk is write-protected.
//
  STG_E_DISKISWRITEPROTECTED = HRESULT($80030013);

//
// MessageId: STG_E_SEEKERROR
//
// MessageText:
//
//  An error occurred during a seek operation.
//
  STG_E_SEEKERROR = HRESULT($80030019);

//
// MessageId: STG_E_WRITEFAULT
//
// MessageText:
//
//  A disk error occurred during a write operation.
//
  STG_E_WRITEFAULT = HRESULT($8003001D);

//
// MessageId: STG_E_READFAULT
//
// MessageText:
//
//  A disk error occurred during a read operation.
//
  STG_E_READFAULT = HRESULT($8003001E);

//
// MessageId: STG_E_SHAREVIOLATION
//
// MessageText:
//
//  A share violation has occurred.
//
  STG_E_SHAREVIOLATION = HRESULT($80030020);

//
// MessageId: STG_E_LOCKVIOLATION
//
// MessageText:
//
//  A lock violation has occurred.
//
  STG_E_LOCKVIOLATION = HRESULT($80030021);

//
// MessageId: STG_E_FILEALREADYEXISTS
//
// MessageText:
//
//  %1 already exists.
//
  STG_E_FILEALREADYEXISTS = HRESULT($80030050);

//
// MessageId: STG_E_INVALIDPARAMETER
//
// MessageText:
//
//  Invalid parameter error.
//
  STG_E_INVALIDPARAMETER = HRESULT($80030057);

//
// MessageId: STG_E_MEDIUMFULL
//
// MessageText:
//
//  There is insufficient disk space to complete operation.
//
  STG_E_MEDIUMFULL = HRESULT($80030070);

//
// MessageId: STG_E_PROPSETMISMATCHED
//
// MessageText:
//
//  Illegal write of non-simple property to simple property set.
//
  STG_E_PROPSETMISMATCHED = HRESULT($800300F0);

//
// MessageId: STG_E_ABNORMALAPIEXIT
//
// MessageText:
//
//  An API call exited abnormally.
//
  STG_E_ABNORMALAPIEXIT = HRESULT($800300FA);

//
// MessageId: STG_E_INVALIDHEADER
//
// MessageText:
//
//  The file %1 is not a valid compound file.
//
  STG_E_INVALIDHEADER = HRESULT($800300FB);

//
// MessageId: STG_E_INVALIDNAME
//
// MessageText:
//
//  The name %1 is not valid.
//
  STG_E_INVALIDNAME = HRESULT($800300FC);

//
// MessageId: STG_E_UNKNOWN
//
// MessageText:
//
//  An unexpected error occurred.
//
  STG_E_UNKNOWN = HRESULT($800300FD);

//
// MessageId: STG_E_UNIMPLEMENTEDFUNCTION
//
// MessageText:
//
//  That function is not implemented.
//
  STG_E_UNIMPLEMENTEDFUNCTION = HRESULT($800300FE);

//
// MessageId: STG_E_INVALIDFLAG
//
// MessageText:
//
//  Invalid flag error.
//
  STG_E_INVALIDFLAG = HRESULT($800300FF);

//
// MessageId: STG_E_INUSE
//
// MessageText:
//
//  Attempted to use an object that is busy.
//
  STG_E_INUSE = HRESULT($80030100);

//
// MessageId: STG_E_NOTCURRENT
//
// MessageText:
//
//  The storage has been changed since the last commit.
//
  STG_E_NOTCURRENT = HRESULT($80030101);

//
// MessageId: STG_E_REVERTED
//
// MessageText:
//
//  Attempted to use an object that has ceased to exist.
//
  STG_E_REVERTED = HRESULT($80030102);

//
// MessageId: STG_E_CANTSAVE
//
// MessageText:
//
//  Can't save.
//
  STG_E_CANTSAVE = HRESULT($80030103);

//
// MessageId: STG_E_OLDFORMAT
//
// MessageText:
//
//  The compound file %1 was produced with an incompatible version of storage.
//
  STG_E_OLDFORMAT = HRESULT($80030104);

//
// MessageId: STG_E_OLDDLL
//
// MessageText:
//
//  The compound file %1 was produced with a newer version of storage.
//
  STG_E_OLDDLL = HRESULT($80030105);

//
// MessageId: STG_E_SHAREREQUIRED
//
// MessageText:
//
//  Share.exe or equivalent is required for operation.
//
  STG_E_SHAREREQUIRED = HRESULT($80030106);

//
// MessageId: STG_E_NOTFILEBASEDSTORAGE
//
// MessageText:
//
//  Illegal operation called on non-file based storage.
//
  STG_E_NOTFILEBASEDSTORAGE = HRESULT($80030107);

//
// MessageId: STG_E_EXTANTMARSHALLINGS
//
// MessageText:
//
//  Illegal operation called on object with extant marshallings.
//
  STG_E_EXTANTMARSHALLINGS = HRESULT($80030108);

//
// MessageId: STG_E_DOCFILECORRUPT
//
// MessageText:
//
//  The docfile has been corrupted.
//
  STG_E_DOCFILECORRUPT = HRESULT($80030109);

//
// MessageId: STG_E_BADBASEADDRESS
//
// MessageText:
//
//  OLE32.DLL has been loaded at the wrong address.
//
  STG_E_BADBASEADDRESS = HRESULT($80030110);

//
// MessageId: STG_E_DOCFILETOOLARGE
//
// MessageText:
//
//  The compound file is too large for the current implementation
//
  STG_E_DOCFILETOOLARGE = HRESULT($80030111);

//
// MessageId: STG_E_NOTSIMPLEFORMAT
//
// MessageText:
//
//  The compound file was not created with the STGM_SIMPLE flag
//
  STG_E_NOTSIMPLEFORMAT = HRESULT($80030112);

//
// MessageId: STG_E_INCOMPLETE
//
// MessageText:
//
//  The file download was aborted abnormally.  The file is incomplete.
//
  STG_E_INCOMPLETE = HRESULT($80030201);

//
// MessageId: STG_E_TERMINATED
//
// MessageText:
//
//  The file download has been terminated.
//
  STG_E_TERMINATED = HRESULT($80030202);

//
// MessageId: STG_S_CONVERTED
//
// MessageText:
//
//  The underlying file was converted to compound file format.
//
  STG_S_CONVERTED = HRESULT($00030200);

//
// MessageId: STG_S_BLOCK
//
// MessageText:
//
//  The storage operation should block until more data is available.
//
  STG_S_BLOCK = HRESULT($00030201);

//
// MessageId: STG_S_RETRYNOW
//
// MessageText:
//
//  The storage operation should retry immediately.
//
  STG_S_RETRYNOW = HRESULT($00030202);

//
// MessageId: STG_S_MONITORING
//
// MessageText:
//
//  The notified event sink will not influence the storage operation.
//
  STG_S_MONITORING = HRESULT($00030203);

//
// MessageId: STG_S_MULTIPLEOPENS
//
// MessageText:
//
//  Multiple opens prevent consolidated. (commit succeeded).
//
  STG_S_MULTIPLEOPENS = HRESULT($00030204);

//
// MessageId: STG_S_CONSOLIDATIONFAILED
//
// MessageText:
//
//  Consolidation of the storage file failed. (commit succeeded).
//
  STG_S_CONSOLIDATIONFAILED = HRESULT($00030205);

//
// MessageId: STG_S_CANNOTCONSOLIDATE
//
// MessageText:
//
//  Consolidation of the storage file is inappropriate. (commit succeeded).
//
  STG_S_CANNOTCONSOLIDATE = HRESULT($00030206);

{*++

 MessageId's 0x0305 - 0x031f (inclusive) are reserved for **STORAGE**
 copy protection errors.

--*}
//
// MessageId: STG_E_STATUS_COPY_PROTECTION_FAILURE
//
// MessageText:
//
//  Generic Copy Protection Error.
//
  STG_E_STATUS_COPY_PROTECTION_FAILURE = HRESULT($80030305);

//
// MessageId: STG_E_CSS_AUTHENTICATION_FAILURE
//
// MessageText:
//
//  Copy Protection Error - DVD CSS Authentication failed.
//
  STG_E_CSS_AUTHENTICATION_FAILURE = HRESULT($80030306);

//
// MessageId: STG_E_CSS_KEY_NOT_PRESENT
//
// MessageText:
//
//  Copy Protection Error - The given sector does not have a valid CSS key.
//
  STG_E_CSS_KEY_NOT_PRESENT = HRESULT($80030307);

//
// MessageId: STG_E_CSS_KEY_NOT_ESTABLISHED
//
// MessageText:
//
//  Copy Protection Error - DVD session key not established.
//
  STG_E_CSS_KEY_NOT_ESTABLISHED = HRESULT($80030308);

//
// MessageId: STG_E_CSS_SCRAMBLED_SECTOR
//
// MessageText:
//
//  Copy Protection Error - The read failed because the sector is encrypted.
//
  STG_E_CSS_SCRAMBLED_SECTOR = HRESULT($80030309);

//
// MessageId: STG_E_CSS_REGION_MISMATCH
//
// MessageText:
//
//  Copy Protection Error - The current DVD's region does not correspond to the region setting of the drive.
//
  STG_E_CSS_REGION_MISMATCH = HRESULT($8003030A);

//
// MessageId: STG_E_RESETS_EXHAUSTED
//
// MessageText:
//
//  Copy Protection Error - The drive's region setting may be permanent or the number of user resets has been exhausted.
//
  STG_E_RESETS_EXHAUSTED = HRESULT($8003030B);

{*++

 MessageId's 0x0305 - 0x031f (inclusive) are reserved for **STORAGE**
 copy protection errors.

--*}
// ******************
// FACILITY_RPC
// ******************
//
// Codes 0x0-0x11 are propagated from 16 bit OLE.
//
//
// MessageId: RPC_E_CALL_REJECTED
//
// MessageText:
//
//  Call was rejected by callee.
//
  RPC_E_CALL_REJECTED = HRESULT($80010001);

//
// MessageId: RPC_E_CALL_CANCELED
//
// MessageText:
//
//  Call was canceled by the message filter.
//
  RPC_E_CALL_CANCELED = HRESULT($80010002);

//
// MessageId: RPC_E_CANTPOST_INSENDCALL
//
// MessageText:
//
//  The caller is dispatching an intertask SendMessage call and cannot call out via PostMessage.
//
  RPC_E_CANTPOST_INSENDCALL = HRESULT($80010003);

//
// MessageId: RPC_E_CANTCALLOUT_INASYNCCALL
//
// MessageText:
//
//  The caller is dispatching an asynchronous call and cannot make an outgoing call on behalf of this call.
//
  RPC_E_CANTCALLOUT_INASYNCCALL = HRESULT($80010004);

//
// MessageId: RPC_E_CANTCALLOUT_INEXTERNALCALL
//
// MessageText:
//
//  It is illegal to call out while inside message filter.
//
  RPC_E_CANTCALLOUT_INEXTERNALCALL = HRESULT($80010005);

//
// MessageId: RPC_E_CONNECTION_TERMINATED
//
// MessageText:
//
//  The connection terminated or is in a bogus state and cannot be used any more. Other connections are still valid.
//
  RPC_E_CONNECTION_TERMINATED = HRESULT($80010006);

//
// MessageId: RPC_E_SERVER_DIED
//
// MessageText:
//
//  The callee (server [not server application]) is not available and disappeared; all connections are invalid. The call may have executed.
//
  RPC_E_SERVER_DIED = HRESULT($80010007);

//
// MessageId: RPC_E_CLIENT_DIED
//
// MessageText:
//
//  The caller (client) disappeared while the callee (server) was processing a call.
//
  RPC_E_CLIENT_DIED = HRESULT($80010008);

//
// MessageId: RPC_E_INVALID_DATAPACKET
//
// MessageText:
//
//  The data packet with the marshalled parameter data is incorrect.
//
  RPC_E_INVALID_DATAPACKET = HRESULT($80010009);

//
// MessageId: RPC_E_CANTTRANSMIT_CALL
//
// MessageText:
//
//  The call was not transmitted properly; the message queue was full and was not emptied after yielding.
//
  RPC_E_CANTTRANSMIT_CALL = HRESULT($8001000A);

//
// MessageId: RPC_E_CLIENT_CANTMARSHAL_DATA
//
// MessageText:
//
//  The client (caller) cannot marshall the parameter data - low memory, etc.
//
  RPC_E_CLIENT_CANTMARSHAL_DATA = HRESULT($8001000B);

//
// MessageId: RPC_E_CLIENT_CANTUNMARSHAL_DATA
//
// MessageText:
//
//  The client (caller) cannot unmarshall the return data - low memory, etc.
//
  RPC_E_CLIENT_CANTUNMARSHAL_DATA = HRESULT($8001000C);

//
// MessageId: RPC_E_SERVER_CANTMARSHAL_DATA
//
// MessageText:
//
//  The server (callee) cannot marshall the return data - low memory, etc.
//
  RPC_E_SERVER_CANTMARSHAL_DATA = HRESULT($8001000D);

//
// MessageId: RPC_E_SERVER_CANTUNMARSHAL_DATA
//
// MessageText:
//
//  The server (callee) cannot unmarshall the parameter data - low memory, etc.
//
  RPC_E_SERVER_CANTUNMARSHAL_DATA = HRESULT($8001000E);

//
// MessageId: RPC_E_INVALID_DATA
//
// MessageText:
//
//  Received data is invalid; could be server or client data.
//
  RPC_E_INVALID_DATA = HRESULT($8001000F);

//
// MessageId: RPC_E_INVALID_PARAMETER
//
// MessageText:
//
//  A particular parameter is invalid and cannot be (un)marshalled.
//
  RPC_E_INVALID_PARAMETER = HRESULT($80010010);

//
// MessageId: RPC_E_CANTCALLOUT_AGAIN
//
// MessageText:
//
//  There is no second outgoing call on same channel in DDE conversation.
//
  RPC_E_CANTCALLOUT_AGAIN = HRESULT($80010011);

//
// MessageId: RPC_E_SERVER_DIED_DNE
//
// MessageText:
//
//  The callee (server [not server application]) is not available and disappeared; all connections are invalid. The call did not execute.
//
  RPC_E_SERVER_DIED_DNE = HRESULT($80010012);

//
// MessageId: RPC_E_SYS_CALL_FAILED
//
// MessageText:
//
//  System call failed.
//
  RPC_E_SYS_CALL_FAILED = HRESULT($80010100);

//
// MessageId: RPC_E_OUT_OF_RESOURCES
//
// MessageText:
//
//  Could not allocate some required resource (memory, events, ...)
//
  RPC_E_OUT_OF_RESOURCES = HRESULT($80010101);

//
// MessageId: RPC_E_ATTEMPTED_MULTITHREAD
//
// MessageText:
//
//  Attempted to make calls on more than one thread in single threaded mode.
//
  RPC_E_ATTEMPTED_MULTITHREAD = HRESULT($80010102);

//
// MessageId: RPC_E_NOT_REGISTERED
//
// MessageText:
//
//  The requested interface is not registered on the server object.
//
  RPC_E_NOT_REGISTERED = HRESULT($80010103);

//
// MessageId: RPC_E_FAULT
//
// MessageText:
//
//  RPC could not call the server or could not return the results of calling the server.
//
  RPC_E_FAULT = HRESULT($80010104);

//
// MessageId: RPC_E_SERVERFAULT
//
// MessageText:
//
//  The server threw an exception.
//
  RPC_E_SERVERFAULT = HRESULT($80010105);

//
// MessageId: RPC_E_CHANGED_MODE
//
// MessageText:
//
//  Cannot change thread mode after it is set.
//
  RPC_E_CHANGED_MODE = HRESULT($80010106);

//
// MessageId: RPC_E_INVALIDMETHOD
//
// MessageText:
//
//  The method called does not exist on the server.
//
  RPC_E_INVALIDMETHOD = HRESULT($80010107);

//
// MessageId: RPC_E_DISCONNECTED
//
// MessageText:
//
//  The object invoked has disconnected from its clients.
//
  RPC_E_DISCONNECTED = HRESULT($80010108);

//
// MessageId: RPC_E_RETRY
//
// MessageText:
//
//  The object invoked chose not to process the call now.  Try again later.
//
  RPC_E_RETRY = HRESULT($80010109);

//
// MessageId: RPC_E_SERVERCALL_RETRYLATER
//
// MessageText:
//
//  The message filter indicated that the application is busy.
//
  RPC_E_SERVERCALL_RETRYLATER = HRESULT($8001010A);

//
// MessageId: RPC_E_SERVERCALL_REJECTED
//
// MessageText:
//
//  The message filter rejected the call.
//
  RPC_E_SERVERCALL_REJECTED = HRESULT($8001010B);

//
// MessageId: RPC_E_INVALID_CALLDATA
//
// MessageText:
//
//  A call control interfaces was called with invalid data.
//
  RPC_E_INVALID_CALLDATA = HRESULT($8001010C);

//
// MessageId: RPC_E_CANTCALLOUT_ININPUTSYNCCALL
//
// MessageText:
//
//  An outgoing call cannot be made since the application is dispatching an input-synchronous call.
//
  RPC_E_CANTCALLOUT_ININPUTSYNCCALL = HRESULT($8001010D);

//
// MessageId: RPC_E_WRONG_THREAD
//
// MessageText:
//
//  The application called an interface that was marshalled for a different thread.
//
  RPC_E_WRONG_THREAD = HRESULT($8001010E);

//
// MessageId: RPC_E_THREAD_NOT_INIT
//
// MessageText:
//
//  CoInitialize has not been called on the current thread.
//
  RPC_E_THREAD_NOT_INIT = HRESULT($8001010F);

//
// MessageId: RPC_E_VERSION_MISMATCH
//
// MessageText:
//
//  The version of OLE on the client and server machines does not match.
//
  RPC_E_VERSION_MISMATCH = HRESULT($80010110);

//
// MessageId: RPC_E_INVALID_HEADER
//
// MessageText:
//
//  OLE received a packet with an invalid header.
//
  RPC_E_INVALID_HEADER = HRESULT($80010111);

//
// MessageId: RPC_E_INVALID_EXTENSION
//
// MessageText:
//
//  OLE received a packet with an invalid extension.
//
  RPC_E_INVALID_EXTENSION = HRESULT($80010112);

//
// MessageId: RPC_E_INVALID_IPID
//
// MessageText:
//
//  The requested object or interface does not exist.
//
  RPC_E_INVALID_IPID = HRESULT($80010113);

//
// MessageId: RPC_E_INVALID_OBJECT
//
// MessageText:
//
//  The requested object does not exist.
//
  RPC_E_INVALID_OBJECT = HRESULT($80010114);

//
// MessageId: RPC_S_CALLPENDING
//
// MessageText:
//
//  OLE has sent a request and is waiting for a reply.
//
  RPC_S_CALLPENDING = HRESULT($80010115);

//
// MessageId: RPC_S_WAITONTIMER
//
// MessageText:
//
//  OLE is waiting before retrying a request.
//
  RPC_S_WAITONTIMER = HRESULT($80010116);

//
// MessageId: RPC_E_CALL_COMPLETE
//
// MessageText:
//
//  Call context cannot be accessed after call completed.
//
  RPC_E_CALL_COMPLETE = HRESULT($80010117);

//
// MessageId: RPC_E_UNSECURE_CALL
//
// MessageText:
//
//  Impersonate on unsecure calls is not supported.
//
  RPC_E_UNSECURE_CALL = HRESULT($80010118);

//
// MessageId: RPC_E_TOO_LATE
//
// MessageText:
//
//  Security must be initialized before any interfaces are marshalled or unmarshalled. It cannot be changed once initialized.
//
  RPC_E_TOO_LATE = HRESULT($80010119);

//
// MessageId: RPC_E_NO_GOOD_SECURITY_PACKAGES
//
// MessageText:
//
//  No security packages are installed on this machine or the user is not logged on or there are no compatible security packages between the client and server.
//
  RPC_E_NO_GOOD_SECURITY_PACKAGES = HRESULT($8001011A);

//
// MessageId: RPC_E_ACCESS_DENIED
//
// MessageText:
//
//  Access is denied.
//
  RPC_E_ACCESS_DENIED = HRESULT($8001011B);

//
// MessageId: RPC_E_REMOTE_DISABLED
//
// MessageText:
//
//  Remote calls are not allowed for this process.
//
  RPC_E_REMOTE_DISABLED = HRESULT($8001011C);

//
// MessageId: RPC_E_INVALID_OBJREF
//
// MessageText:
//
//  The marshaled interface data packet (OBJREF) has an invalid or unknown format.
//
  RPC_E_INVALID_OBJREF = HRESULT($8001011D);

//
// MessageId: RPC_E_NO_CONTEXT
//
// MessageText:
//
//  No context is associated with this call. This happens for some custom marshalled calls and on the client side of the call.
//
  RPC_E_NO_CONTEXT = HRESULT($8001011E);

//
// MessageId: RPC_E_TIMEOUT
//
// MessageText:
//
//  This operation returned because the timeout period expired.
//
  RPC_E_TIMEOUT = HRESULT($8001011F);

//
// MessageId: RPC_E_NO_SYNC
//
// MessageText:
//
//  There are no synchronize objects to wait on.
//
  RPC_E_NO_SYNC = HRESULT($80010120);

//
// MessageId: RPC_E_FULLSIC_REQUIRED
//
// MessageText:
//
//  Full subject issuer chain SSL principal name expected from the server.
//
  RPC_E_FULLSIC_REQUIRED = HRESULT($80010121);

//
// MessageId: RPC_E_INVALID_STD_NAME
//
// MessageText:
//
//  Principal name is not a valid MSSTD name.
//
  RPC_E_INVALID_STD_NAME = HRESULT($80010122);

//
// MessageId: CO_E_FAILEDTOIMPERSONATE
//
// MessageText:
//
//  Unable to impersonate DCOM client
//
  CO_E_FAILEDTOIMPERSONATE = HRESULT($80010123);

//
// MessageId: CO_E_FAILEDTOGETSECCTX
//
// MessageText:
//
//  Unable to obtain server's security context
//
  CO_E_FAILEDTOGETSECCTX = HRESULT($80010124);

//
// MessageId: CO_E_FAILEDTOOPENTHREADTOKEN
//
// MessageText:
//
//  Unable to open the access token of the current thread
//
  CO_E_FAILEDTOOPENTHREADTOKEN = HRESULT($80010125);

//
// MessageId: CO_E_FAILEDTOGETTOKENINFO
//
// MessageText:
//
//  Unable to obtain user info from an access token
//
  CO_E_FAILEDTOGETTOKENINFO = HRESULT($80010126);

//
// MessageId: CO_E_TRUSTEEDOESNTMATCHCLIENT
//
// MessageText:
//
//  The client who called IAccessControl::IsAccessPermitted was not the trustee provided to the method
//
  CO_E_TRUSTEEDOESNTMATCHCLIENT = HRESULT($80010127);

//
// MessageId: CO_E_FAILEDTOQUERYCLIENTBLANKET
//
// MessageText:
//
//  Unable to obtain the client's security blanket
//
  CO_E_FAILEDTOQUERYCLIENTBLANKET = HRESULT($80010128);

//
// MessageId: CO_E_FAILEDTOSETDACL
//
// MessageText:
//
//  Unable to set a discretionary ACL into a security descriptor
//
  CO_E_FAILEDTOSETDACL = HRESULT($80010129);

//
// MessageId: CO_E_ACCESSCHECKFAILED
//
// MessageText:
//
//  The system function, AccessCheck, returned false
//
  CO_E_ACCESSCHECKFAILED = HRESULT($8001012A);

//
// MessageId: CO_E_NETACCESSAPIFAILED
//
// MessageText:
//
//  Either NetAccessDel or NetAccessAdd returned an error code.
//
  CO_E_NETACCESSAPIFAILED = HRESULT($8001012B);

//
// MessageId: CO_E_WRONGTRUSTEENAMESYNTAX
//
// MessageText:
//
//  One of the trustee strings provided by the user did not conform to the <Domain>\<Name> syntax and it was not the "*" string
//
  CO_E_WRONGTRUSTEENAMESYNTAX = HRESULT($8001012C);

//
// MessageId: CO_E_INVALIDSID
//
// MessageText:
//
//  One of the security identifiers provided by the user was invalid
//
  CO_E_INVALIDSID = HRESULT($8001012D);

//
// MessageId: CO_E_CONVERSIONFAILED
//
// MessageText:
//
//  Unable to convert a wide character trustee string to a multibyte trustee string
//
  CO_E_CONVERSIONFAILED = HRESULT($8001012E);

//
// MessageId: CO_E_NOMATCHINGSIDFOUND
//
// MessageText:
//
//  Unable to find a security identifier that corresponds to a trustee string provided by the user
//
  CO_E_NOMATCHINGSIDFOUND = HRESULT($8001012F);

//
// MessageId: CO_E_LOOKUPACCSIDFAILED
//
// MessageText:
//
//  The system function, LookupAccountSID, failed
//
  CO_E_LOOKUPACCSIDFAILED = HRESULT($80010130);

//
// MessageId: CO_E_NOMATCHINGNAMEFOUND
//
// MessageText:
//
//  Unable to find a trustee name that corresponds to a security identifier provided by the user
//
  CO_E_NOMATCHINGNAMEFOUND = HRESULT($80010131);

//
// MessageId: CO_E_LOOKUPACCNAMEFAILED
//
// MessageText:
//
//  The system function, LookupAccountName, failed
//
  CO_E_LOOKUPACCNAMEFAILED = HRESULT($80010132);

//
// MessageId: CO_E_SETSERLHNDLFAILED
//
// MessageText:
//
//  Unable to set or reset a serialization handle
//
  CO_E_SETSERLHNDLFAILED = HRESULT($80010133);

//
// MessageId: CO_E_FAILEDTOGETWINDIR
//
// MessageText:
//
//  Unable to obtain the Windows directory
//
  CO_E_FAILEDTOGETWINDIR = HRESULT($80010134);

//
// MessageId: CO_E_PATHTOOLONG
//
// MessageText:
//
//  Path too long
//
  CO_E_PATHTOOLONG = HRESULT($80010135);

//
// MessageId: CO_E_FAILEDTOGENUUID
//
// MessageText:
//
//  Unable to generate a uuid.
//
  CO_E_FAILEDTOGENUUID = HRESULT($80010136);

//
// MessageId: CO_E_FAILEDTOCREATEFILE
//
// MessageText:
//
//  Unable to create file
//
  CO_E_FAILEDTOCREATEFILE = HRESULT($80010137);

//
// MessageId: CO_E_FAILEDTOCLOSEHANDLE
//
// MessageText:
//
//  Unable to close a serialization handle or a file handle.
//
  CO_E_FAILEDTOCLOSEHANDLE = HRESULT($80010138);

//
// MessageId: CO_E_EXCEEDSYSACLLIMIT
//
// MessageText:
//
//  The number of ACEs in an ACL exceeds the system limit.
//
  CO_E_EXCEEDSYSACLLIMIT = HRESULT($80010139);

//
// MessageId: CO_E_ACESINWRONGORDER
//
// MessageText:
//
//  Not all the DENY_ACCESS ACEs are arranged in front of the GRANT_ACCESS ACEs in the stream.
//
  CO_E_ACESINWRONGORDER = HRESULT($8001013A);

//
// MessageId: CO_E_INCOMPATIBLESTREAMVERSION
//
// MessageText:
//
//  The version of ACL format in the stream is not supported by this implementation of IAccessControl
//
  CO_E_INCOMPATIBLESTREAMVERSION = HRESULT($8001013B);

//
// MessageId: CO_E_FAILEDTOOPENPROCESSTOKEN
//
// MessageText:
//
//  Unable to open the access token of the server process
//
  CO_E_FAILEDTOOPENPROCESSTOKEN = HRESULT($8001013C);

//
// MessageId: CO_E_DECODEFAILED
//
// MessageText:
//
//  Unable to decode the ACL in the stream provided by the user
//
  CO_E_DECODEFAILED = HRESULT($8001013D);

//
// MessageId: CO_E_ACNOTINITIALIZED
//
// MessageText:
//
//  The COM IAccessControl object is not initialized
//
  CO_E_ACNOTINITIALIZED = HRESULT($8001013F);

//
// MessageId: CO_E_CANCEL_DISABLED
//
// MessageText:
//
//  Call Cancellation is disabled
//
  CO_E_CANCEL_DISABLED = HRESULT($80010140);

//
// MessageId: RPC_E_UNEXPECTED
//
// MessageText:
//
//  An internal error occurred.
//
  RPC_E_UNEXPECTED = HRESULT($8001FFFF);



//////////////////////////////////////
//                                  //
// Additional Security Status Codes //
//                                  //
// Facility=Security                //
//                                  //
//////////////////////////////////////


//
// MessageId: ERROR_AUDITING_DISABLED
//
// MessageText:
//
//  The specified event is currently not being audited.
//
  ERROR_AUDITING_DISABLED = HRESULT($C0090001);

//
// MessageId: ERROR_ALL_SIDS_FILTERED
//
// MessageText:
//
//  The SID filtering operation removed all SIDs.
//
  ERROR_ALL_SIDS_FILTERED = HRESULT($C0090002);



/////////////////////////////////////////////
//                                         //
// end of Additional Security Status Codes //
//                                         //
/////////////////////////////////////////////



 /////////////////
 //
 //  FACILITY_SSPI
 //
 /////////////////

//
// MessageId: NTE_BAD_UID
//
// MessageText:
//
//  Bad UID.
//
  NTE_BAD_UID = HRESULT($80090001);

//
// MessageId: NTE_BAD_HASH
//
// MessageText:
//
//  Bad Hash.
//
  NTE_BAD_HASH = HRESULT($80090002);

//
// MessageId: NTE_BAD_KEY
//
// MessageText:
//
//  Bad Key.
//
  NTE_BAD_KEY = HRESULT($80090003);

//
// MessageId: NTE_BAD_LEN
//
// MessageText:
//
//  Bad Length.
//
  NTE_BAD_LEN = HRESULT($80090004);

//
// MessageId: NTE_BAD_DATA
//
// MessageText:
//
//  Bad Data.
//
  NTE_BAD_DATA = HRESULT($80090005);

//
// MessageId: NTE_BAD_SIGNATURE
//
// MessageText:
//
//  Invalid Signature.
//
  NTE_BAD_SIGNATURE = HRESULT($80090006);

//
// MessageId: NTE_BAD_VER
//
// MessageText:
//
//  Bad Version of provider.
//
  NTE_BAD_VER = HRESULT($80090007);

//
// MessageId: NTE_BAD_ALGID
//
// MessageText:
//
//  Invalid algorithm specified.
//
  NTE_BAD_ALGID = HRESULT($80090008);

//
// MessageId: NTE_BAD_FLAGS
//
// MessageText:
//
//  Invalid flags specified.
//
  NTE_BAD_FLAGS = HRESULT($80090009);

//
// MessageId: NTE_BAD_TYPE
//
// MessageText:
//
//  Invalid type specified.
//
  NTE_BAD_TYPE = HRESULT($8009000A);

//
// MessageId: NTE_BAD_KEY_STATE
//
// MessageText:
//
//  Key not valid for use in specified state.
//
  NTE_BAD_KEY_STATE = HRESULT($8009000B);

//
// MessageId: NTE_BAD_HASH_STATE
//
// MessageText:
//
//  Hash not valid for use in specified state.
//
  NTE_BAD_HASH_STATE = HRESULT($8009000C);

//
// MessageId: NTE_NO_KEY
//
// MessageText:
//
//  Key does not exist.
//
  NTE_NO_KEY = HRESULT($8009000D);

//
// MessageId: NTE_NO_MEMORY
//
// MessageText:
//
//  Insufficient memory available for the operation.
//
  NTE_NO_MEMORY = HRESULT($8009000E);

//
// MessageId: NTE_EXISTS
//
// MessageText:
//
//  Object already exists.
//
  NTE_EXISTS = HRESULT($8009000F);

//
// MessageId: NTE_PERM
//
// MessageText:
//
//  Access denied.
//
  NTE_PERM = HRESULT($80090010);

//
// MessageId: NTE_NOT_FOUND
//
// MessageText:
//
//  Object was not found.
//
  NTE_NOT_FOUND = HRESULT($80090011);

//
// MessageId: NTE_DOUBLE_ENCRYPT
//
// MessageText:
//
//  Data already encrypted.
//
  NTE_DOUBLE_ENCRYPT = HRESULT($80090012);

//
// MessageId: NTE_BAD_PROVIDER
//
// MessageText:
//
//  Invalid provider specified.
//
  NTE_BAD_PROVIDER = HRESULT($80090013);

//
// MessageId: NTE_BAD_PROV_TYPE
//
// MessageText:
//
//  Invalid provider type specified.
//
  NTE_BAD_PROV_TYPE = HRESULT($80090014);

//
// MessageId: NTE_BAD_PUBLIC_KEY
//
// MessageText:
//
//  Provider's public key is invalid.
//
  NTE_BAD_PUBLIC_KEY = HRESULT($80090015);

//
// MessageId: NTE_BAD_KEYSET
//
// MessageText:
//
//  Keyset does not exist
//
  NTE_BAD_KEYSET = HRESULT($80090016);

//
// MessageId: NTE_PROV_TYPE_NOT_DEF
//
// MessageText:
//
//  Provider type not defined.
//
  NTE_PROV_TYPE_NOT_DEF = HRESULT($80090017);

//
// MessageId: NTE_PROV_TYPE_ENTRY_BAD
//
// MessageText:
//
//  Provider type as registered is invalid.
//
  NTE_PROV_TYPE_ENTRY_BAD = HRESULT($80090018);

//
// MessageId: NTE_KEYSET_NOT_DEF
//
// MessageText:
//
//  The keyset is not defined.
//
  NTE_KEYSET_NOT_DEF = HRESULT($80090019);

//
// MessageId: NTE_KEYSET_ENTRY_BAD
//
// MessageText:
//
//  Keyset as registered is invalid.
//
  NTE_KEYSET_ENTRY_BAD = HRESULT($8009001A);

//
// MessageId: NTE_PROV_TYPE_NO_MATCH
//
// MessageText:
//
//  Provider type does not match registered value.
//
  NTE_PROV_TYPE_NO_MATCH = HRESULT($8009001B);

//
// MessageId: NTE_SIGNATURE_FILE_BAD
//
// MessageText:
//
//  The digital signature file is corrupt.
//
  NTE_SIGNATURE_FILE_BAD = HRESULT($8009001C);

//
// MessageId: NTE_PROVIDER_DLL_FAIL
//
// MessageText:
//
//  Provider DLL failed to initialize correctly.
//
  NTE_PROVIDER_DLL_FAIL = HRESULT($8009001D);

//
// MessageId: NTE_PROV_DLL_NOT_FOUND
//
// MessageText:
//
//  Provider DLL could not be found.
//
  NTE_PROV_DLL_NOT_FOUND = HRESULT($8009001E);

//
// MessageId: NTE_BAD_KEYSET_PARAM
//
// MessageText:
//
//  The Keyset parameter is invalid.
//
  NTE_BAD_KEYSET_PARAM = HRESULT($8009001F);

//
// MessageId: NTE_FAIL
//
// MessageText:
//
//  An internal error occurred.
//
  NTE_FAIL = HRESULT($80090020);

//
// MessageId: NTE_SYS_ERR
//
// MessageText:
//
//  A base error occurred.
//
  NTE_SYS_ERR = HRESULT($80090021);

//
// MessageId: NTE_SILENT_CONTEXT
//
// MessageText:
//
//  Provider could not perform the action since the context was acquired as silent.
//
  NTE_SILENT_CONTEXT = HRESULT($80090022);

//
// MessageId: NTE_TOKEN_KEYSET_STORAGE_FULL
//
// MessageText:
//
//  The security token does not have storage space available for an additional container.
//
  NTE_TOKEN_KEYSET_STORAGE_FULL = HRESULT($80090023);

//
// MessageId: NTE_TEMPORARY_PROFILE
//
// MessageText:
//
//  The profile for the user is a temporary profile.
//
  NTE_TEMPORARY_PROFILE = HRESULT($80090024);

//
// MessageId: NTE_FIXEDPARAMETER
//
// MessageText:
//
//  The key parameters could not be set because the CSP uses fixed parameters.
//
  NTE_FIXEDPARAMETER = HRESULT($80090025);

//
// MessageId: SEC_E_INSUFFICIENT_MEMORY
//
// MessageText:
//
//  Not enough memory is available to complete this request
//
  SEC_E_INSUFFICIENT_MEMORY = HRESULT($80090300);

//
// MessageId: SEC_E_INVALID_HANDLE
//
// MessageText:
//
//  The handle specified is invalid
//
  SEC_E_INVALID_HANDLE = HRESULT($80090301);

//
// MessageId: SEC_E_UNSUPPORTED_FUNCTION
//
// MessageText:
//
//  The function requested is not supported
//
  SEC_E_UNSUPPORTED_FUNCTION = HRESULT($80090302);

//
// MessageId: SEC_E_TARGET_UNKNOWN
//
// MessageText:
//
//  The specified target is unknown or unreachable
//
  SEC_E_TARGET_UNKNOWN = HRESULT($80090303);

//
// MessageId: SEC_E_INTERNAL_ERROR
//
// MessageText:
//
//  The Local Security Authority cannot be contacted
//
  SEC_E_INTERNAL_ERROR = HRESULT($80090304);

//
// MessageId: SEC_E_SECPKG_NOT_FOUND
//
// MessageText:
//
//  The requested security package does not exist
//
  SEC_E_SECPKG_NOT_FOUND = HRESULT($80090305);

//
// MessageId: SEC_E_NOT_OWNER
//
// MessageText:
//
//  The caller is not the owner of the desired credentials
//
  SEC_E_NOT_OWNER = HRESULT($80090306);

//
// MessageId: SEC_E_CANNOT_INSTALL
//
// MessageText:
//
//  The security package failed to initialize, and cannot be installed
//
  SEC_E_CANNOT_INSTALL = HRESULT($80090307);

//
// MessageId: SEC_E_INVALID_TOKEN
//
// MessageText:
//
//  The token supplied to the function is invalid
//
  SEC_E_INVALID_TOKEN = HRESULT($80090308);

//
// MessageId: SEC_E_CANNOT_PACK
//
// MessageText:
//
//  The security package is not able to marshall the logon buffer, so the logon attempt has failed
//
  SEC_E_CANNOT_PACK = HRESULT($80090309);

//
// MessageId: SEC_E_QOP_NOT_SUPPORTED
//
// MessageText:
//
//  The per-message Quality of Protection is not supported by the security package
//
  SEC_E_QOP_NOT_SUPPORTED = HRESULT($8009030A);

//
// MessageId: SEC_E_NO_IMPERSONATION
//
// MessageText:
//
//  The security context does not allow impersonation of the client
//
  SEC_E_NO_IMPERSONATION = HRESULT($8009030B);

//
// MessageId: SEC_E_LOGON_DENIED
//
// MessageText:
//
//  The logon attempt failed
//
  SEC_E_LOGON_DENIED = HRESULT($8009030C);

//
// MessageId: SEC_E_UNKNOWN_CREDENTIALS
//
// MessageText:
//
//  The credentials supplied to the package were not recognized
//
  SEC_E_UNKNOWN_CREDENTIALS = HRESULT($8009030D);

//
// MessageId: SEC_E_NO_CREDENTIALS
//
// MessageText:
//
//  No credentials are available in the security package
//
  SEC_E_NO_CREDENTIALS = HRESULT($8009030E);

//
// MessageId: SEC_E_MESSAGE_ALTERED
//
// MessageText:
//
//  The message or signature supplied for verification has been altered
//
  SEC_E_MESSAGE_ALTERED = HRESULT($8009030F);

//
// MessageId: SEC_E_OUT_OF_SEQUENCE
//
// MessageText:
//
//  The message supplied for verification is out of sequence
//
  SEC_E_OUT_OF_SEQUENCE = HRESULT($80090310);

//
// MessageId: SEC_E_NO_AUTHENTICATING_AUTHORITY
//
// MessageText:
//
//  No authority could be contacted for authentication.
//
  SEC_E_NO_AUTHENTICATING_AUTHORITY = HRESULT($80090311);

//
// MessageId: SEC_I_CONTINUE_NEEDED
//
// MessageText:
//
//  The function completed successfully, but must be called again to complete the context
//
  SEC_I_CONTINUE_NEEDED = HRESULT($00090312);

//
// MessageId: SEC_I_COMPLETE_NEEDED
//
// MessageText:
//
//  The function completed successfully, but CompleteToken must be called
//
  SEC_I_COMPLETE_NEEDED = HRESULT($00090313);

//
// MessageId: SEC_I_COMPLETE_AND_CONTINUE
//
// MessageText:
//
//  The function completed successfully, but both CompleteToken and this function must be called to complete the context
//
  SEC_I_COMPLETE_AND_CONTINUE = HRESULT($00090314);

//
// MessageId: SEC_I_LOCAL_LOGON
//
// MessageText:
//
//  The logon was completed, but no network authority was available. The logon was made using locally known information
//
  SEC_I_LOCAL_LOGON = HRESULT($00090315);

//
// MessageId: SEC_E_BAD_PKGID
//
// MessageText:
//
//  The requested security package does not exist
//
  SEC_E_BAD_PKGID = HRESULT($80090316);

//
// MessageId: SEC_E_CONTEXT_EXPIRED
//
// MessageText:
//
//  The context has expired and can no longer be used.
//
  SEC_E_CONTEXT_EXPIRED = HRESULT($80090317);

//
// MessageId: SEC_I_CONTEXT_EXPIRED
//
// MessageText:
//
//  The context has expired and can no longer be used.
//
  SEC_I_CONTEXT_EXPIRED = HRESULT($00090317);

//
// MessageId: SEC_E_INCOMPLETE_MESSAGE
//
// MessageText:
//
//  The supplied message is incomplete.  The signature was not verified.
//
  SEC_E_INCOMPLETE_MESSAGE = HRESULT($80090318);

//
// MessageId: SEC_E_INCOMPLETE_CREDENTIALS
//
// MessageText:
//
//  The credentials supplied were not complete, and could not be verified. The context could not be initialized.
//
  SEC_E_INCOMPLETE_CREDENTIALS = HRESULT($80090320);

//
// MessageId: SEC_E_BUFFER_TOO_SMALL
//
// MessageText:
//
//  The buffers supplied to a function was too small.
//
  SEC_E_BUFFER_TOO_SMALL = HRESULT($80090321);

//
// MessageId: SEC_I_INCOMPLETE_CREDENTIALS
//
// MessageText:
//
//  The credentials supplied were not complete, and could not be verified. Additional information can be returned from the context.
//
  SEC_I_INCOMPLETE_CREDENTIALS = HRESULT($00090320);

//
// MessageId: SEC_I_RENEGOTIATE
//
// MessageText:
//
//  The context data must be renegotiated with the peer.
//
  SEC_I_RENEGOTIATE = HRESULT($00090321);

//
// MessageId: SEC_E_WRONG_PRINCIPAL
//
// MessageText:
//
//  The target principal name is incorrect.
//
  SEC_E_WRONG_PRINCIPAL = HRESULT($80090322);

//
// MessageId: SEC_I_NO_LSA_CONTEXT
//
// MessageText:
//
//  There is no LSA mode context associated with this context.
//
  SEC_I_NO_LSA_CONTEXT = HRESULT($00090323);

//
// MessageId: SEC_E_TIME_SKEW
//
// MessageText:
//
//  The clocks on the client and server machines are skewed.
//
  SEC_E_TIME_SKEW = HRESULT($80090324);

//
// MessageId: SEC_E_UNTRUSTED_ROOT
//
// MessageText:
//
//  The certificate chain was issued by an authority that is not trusted.
//
  SEC_E_UNTRUSTED_ROOT = HRESULT($80090325);

//
// MessageId: SEC_E_ILLEGAL_MESSAGE
//
// MessageText:
//
//  The message received was unexpected or badly formatted.
//
  SEC_E_ILLEGAL_MESSAGE = HRESULT($80090326);

//
// MessageId: SEC_E_CERT_UNKNOWN
//
// MessageText:
//
//  An unknown error occurred while processing the certificate.
//
  SEC_E_CERT_UNKNOWN = HRESULT($80090327);

//
// MessageId: SEC_E_CERT_EXPIRED
//
// MessageText:
//
//  The received certificate has expired.
//
  SEC_E_CERT_EXPIRED = HRESULT($80090328);

//
// MessageId: SEC_E_ENCRYPT_FAILURE
//
// MessageText:
//
//  The specified data could not be encrypted.
//
  SEC_E_ENCRYPT_FAILURE = HRESULT($80090329);

//
// MessageId: SEC_E_DECRYPT_FAILURE
//
// MessageText:
//
//  The specified data could not be decrypted.
//  
//
  SEC_E_DECRYPT_FAILURE = HRESULT($80090330);

//
// MessageId: SEC_E_ALGORITHM_MISMATCH
//
// MessageText:
//
//  The client and server cannot communicate, because they do not possess a common algorithm.
//
  SEC_E_ALGORITHM_MISMATCH = HRESULT($80090331);

//
// MessageId: SEC_E_SECURITY_QOS_FAILED
//
// MessageText:
//
//  The security context could not be established due to a failure in the requested quality of service (e.g. mutual authentication or delegation).
//
  SEC_E_SECURITY_QOS_FAILED = HRESULT($80090332);

//
// MessageId: SEC_E_UNFINISHED_CONTEXT_DELETED
//
// MessageText:
//
//  A security context was deleted before the context was completed.  This is considered a logon failure.
//
  SEC_E_UNFINISHED_CONTEXT_DELETED = HRESULT($80090333);

//
// MessageId: SEC_E_NO_TGT_REPLY
//
// MessageText:
//
//  The client is trying to negotiate a context and the server requires user-to-user but didn't send a TGT reply.
//
  SEC_E_NO_TGT_REPLY = HRESULT($80090334);

//
// MessageId: SEC_E_NO_IP_ADDRESSES
//
// MessageText:
//
//  Unable to accomplish the requested task because the local machine does not have any IP addresses.
//
  SEC_E_NO_IP_ADDRESSES = HRESULT($80090335);

//
// MessageId: SEC_E_WRONG_CREDENTIAL_HANDLE
//
// MessageText:
//
//  The supplied credential handle does not match the credential associated with the security context.
//
  SEC_E_WRONG_CREDENTIAL_HANDLE = HRESULT($80090336);

//
// MessageId: SEC_E_CRYPTO_SYSTEM_INVALID
//
// MessageText:
//
//  The crypto system or checksum function is invalid because a required function is unavailable.
//
  SEC_E_CRYPTO_SYSTEM_INVALID = HRESULT($80090337);

//
// MessageId: SEC_E_MAX_REFERRALS_EXCEEDED
//
// MessageText:
//
//  The number of maximum ticket referrals has been exceeded.
//
  SEC_E_MAX_REFERRALS_EXCEEDED = HRESULT($80090338);

//
// MessageId: SEC_E_MUST_BE_KDC
//
// MessageText:
//
//  The local machine must be a Kerberos KDC (domain controller) and it is not.
//
  SEC_E_MUST_BE_KDC = HRESULT($80090339);

//
// MessageId: SEC_E_STRONG_CRYPTO_NOT_SUPPORTED
//
// MessageText:
//
//  The other end of the security negotiation is requires strong crypto but it is not supported on the local machine.
//
  SEC_E_STRONG_CRYPTO_NOT_SUPPORTED = HRESULT($8009033A);

//
// MessageId: SEC_E_TOO_MANY_PRINCIPALS
//
// MessageText:
//
//  The KDC reply contained more than one principal name.
//
  SEC_E_TOO_MANY_PRINCIPALS = HRESULT($8009033B);

//
// MessageId: SEC_E_NO_PA_DATA
//
// MessageText:
//
//  Expected to find PA data for a hint of what etype to use, but it was not found.
//
  SEC_E_NO_PA_DATA = HRESULT($8009033C);

//
// MessageId: SEC_E_PKINIT_NAME_MISMATCH
//
// MessageText:
//
//  The client certificate does not contain a valid UPN, or does not match the client name 
//  in the logon request.  Please contact your administrator.
//
  SEC_E_PKINIT_NAME_MISMATCH = HRESULT($8009033D);

//
// MessageId: SEC_E_SMARTCARD_LOGON_REQUIRED
//
// MessageText:
//
//  Smartcard logon is required and was not used.
//
  SEC_E_SMARTCARD_LOGON_REQUIRED = HRESULT($8009033E);

//
// MessageId: SEC_E_SHUTDOWN_IN_PROGRESS
//
// MessageText:
//
//  A system shutdown is in progress.
//
  SEC_E_SHUTDOWN_IN_PROGRESS = HRESULT($8009033F);

//
// MessageId: SEC_E_KDC_INVALID_REQUEST
//
// MessageText:
//
//  An invalid request was sent to the KDC.
//
  SEC_E_KDC_INVALID_REQUEST = HRESULT($80090340);

//
// MessageId: SEC_E_KDC_UNABLE_TO_REFER
//
// MessageText:
//
//  The KDC was unable to generate a referral for the service requested.
//
  SEC_E_KDC_UNABLE_TO_REFER = HRESULT($80090341);

//
// MessageId: SEC_E_KDC_UNKNOWN_ETYPE
//
// MessageText:
//
//  The encryption type requested is not supported by the KDC.
//
  SEC_E_KDC_UNKNOWN_ETYPE = HRESULT($80090342);

//
// MessageId: SEC_E_UNSUPPORTED_PREAUTH
//
// MessageText:
//
//  An unsupported preauthentication mechanism was presented to the kerberos package.
//
  SEC_E_UNSUPPORTED_PREAUTH = HRESULT($80090343);

//
// MessageId: SEC_E_DELEGATION_REQUIRED
//
// MessageText:
//
//  The requested operation requires delegation to be enabled on the machine.
//
  SEC_E_DELEGATION_REQUIRED = HRESULT($80090345);

//
// MessageId: SEC_E_BAD_BINDINGS
//
// MessageText:
//
//  Client's supplied SSPI channel bindings were incorrect.
//
  SEC_E_BAD_BINDINGS = HRESULT($80090346);

//
// MessageId: SEC_E_MULTIPLE_ACCOUNTS
//
// MessageText:
//
//  The received certificate was mapped to multiple accounts.
//
  SEC_E_MULTIPLE_ACCOUNTS = HRESULT($80090347);

//
// MessageId: SEC_E_NO_KERB_KEY
//
// MessageText:
//
//  SEC_E_NO_KERB_KEY
//
  SEC_E_NO_KERB_KEY = HRESULT($80090348);

//
// MessageId: SEC_E_CERT_WRONG_USAGE
//
// MessageText:
//
//  The certificate is not valid for the requested usage.
//
  SEC_E_CERT_WRONG_USAGE = HRESULT($80090349);

//
// MessageId: SEC_E_DOWNGRADE_DETECTED
//
// MessageText:
//
//  The system detected a possible attempt to compromise security.  Please ensure that you can contact the server that authenticated you.
//
  SEC_E_DOWNGRADE_DETECTED = HRESULT($80090350);

//
// MessageId: SEC_E_SMARTCARD_CERT_REVOKED
//
// MessageText:
//
//  The smartcard certificate used for authentication has been revoked.
//  Please contact your system administrator.  There may be additional information in the
//  event log.
//
  SEC_E_SMARTCARD_CERT_REVOKED = HRESULT($80090351);

//
// MessageId: SEC_E_ISSUING_CA_UNTRUSTED
//
// MessageText:
//
//  An untrusted certificate authority was detected While processing the
//  smartcard certificate used for authentication.  Please contact your system
//  administrator.
//
  SEC_E_ISSUING_CA_UNTRUSTED = HRESULT($80090352);

//
// MessageId: SEC_E_REVOCATION_OFFLINE_C
//
// MessageText:
//
//  The revocation status of the smartcard certificate used for
//  authentication could not be determined. Please contact your system administrator.
//
  SEC_E_REVOCATION_OFFLINE_C = HRESULT($80090353);

//
// MessageId: SEC_E_PKINIT_CLIENT_FAILURE
//
// MessageText:
//
//  The smartcard certificate used for authentication was not trusted.  Please
//  contact your system administrator.
//
  SEC_E_PKINIT_CLIENT_FAILURE = HRESULT($80090354);

//
// MessageId: SEC_E_SMARTCARD_CERT_EXPIRED
//
// MessageText:
//
//  The smartcard certificate used for authentication has expired.  Please
//  contact your system administrator.
//
  SEC_E_SMARTCARD_CERT_EXPIRED = HRESULT($80090355);

//
// MessageId: SEC_E_NO_S4U_PROT_SUPPORT
//
// MessageText:
//
//  The Kerberos subsystem encountered an error.  A service for user protocol request was made 
//  against a domain controller which does not support service for user.
//
  SEC_E_NO_S4U_PROT_SUPPORT = HRESULT($80090356);

//
// MessageId: SEC_E_CROSSREALM_DELEGATION_FAILURE
//
// MessageText:
//
//  An attempt was made by this server to make a Kerberos constrained delegation request for a target
//  outside of the server's realm.  This is not supported, and indicates a misconfiguration on this
//  server's allowed to delegate to list.  Please contact your administrator.
//
  SEC_E_CROSSREALM_DELEGATION_FAILURE = HRESULT($80090357);

//
// MessageId: SEC_E_REVOCATION_OFFLINE_KDC
//
// MessageText:
//
//  The revocation status of the domain controller certificate used for smartcard
//  authentication could not be determined.  There is additional information in the system event
//  log. Please contact your system administrator.
//
  SEC_E_REVOCATION_OFFLINE_KDC = HRESULT($80090358);

//
// MessageId: SEC_E_ISSUING_CA_UNTRUSTED_KDC
//
// MessageText:
//
//  An untrusted certificate authority was detected while processing the
//  domain controller certificate used for authentication.  There is additional information in
//  the system event log.  Please contact your system administrator.
//
  SEC_E_ISSUING_CA_UNTRUSTED_KDC = HRESULT($80090359);

//
// MessageId: SEC_E_KDC_CERT_EXPIRED
//
// MessageText:
//
//  The domain controller certificate used for smartcard logon has expired.
//  Please contact your system administrator with the contents of your system event log.
//
  SEC_E_KDC_CERT_EXPIRED = HRESULT($8009035A);

//
// MessageId: SEC_E_KDC_CERT_REVOKED
//
// MessageText:
//
//  The domain controller certificate used for smartcard logon has been revoked.
//  Please contact your system administrator with the contents of your system event log.
//
  SEC_E_KDC_CERT_REVOKED = HRESULT($8009035B);

//
// Provided for backwards compatibility
//

  SEC_E_NO_SPM = SEC_E_INTERNAL_ERROR;
  SEC_E_NOT_SUPPORTED = SEC_E_UNSUPPORTED_FUNCTION;

//
// MessageId: CRYPT_E_MSG_ERROR
//
// MessageText:
//
//  An error occurred while performing an operation on a cryptographic message.
//
  CRYPT_E_MSG_ERROR = HRESULT($80091001);

//
// MessageId: CRYPT_E_UNKNOWN_ALGO
//
// MessageText:
//
//  Unknown cryptographic algorithm.
//
  CRYPT_E_UNKNOWN_ALGO = HRESULT($80091002);

//
// MessageId: CRYPT_E_OID_FORMAT
//
// MessageText:
//
//  The object identifier is poorly formatted.
//
  CRYPT_E_OID_FORMAT = HRESULT($80091003);

//
// MessageId: CRYPT_E_INVALID_MSG_TYPE
//
// MessageText:
//
//  Invalid cryptographic message type.
//
  CRYPT_E_INVALID_MSG_TYPE = HRESULT($80091004);

//
// MessageId: CRYPT_E_UNEXPECTED_ENCODING
//
// MessageText:
//
//  Unexpected cryptographic message encoding.
//
  CRYPT_E_UNEXPECTED_ENCODING = HRESULT($80091005);

//
// MessageId: CRYPT_E_AUTH_ATTR_MISSING
//
// MessageText:
//
//  The cryptographic message does not contain an expected authenticated attribute.
//
  CRYPT_E_AUTH_ATTR_MISSING = HRESULT($80091006);

//
// MessageId: CRYPT_E_HASH_VALUE
//
// MessageText:
//
//  The hash value is not correct.
//
  CRYPT_E_HASH_VALUE = HRESULT($80091007);

//
// MessageId: CRYPT_E_INVALID_INDEX
//
// MessageText:
//
//  The index value is not valid.
//
  CRYPT_E_INVALID_INDEX = HRESULT($80091008);

//
// MessageId: CRYPT_E_ALREADY_DECRYPTED
//
// MessageText:
//
//  The content of the cryptographic message has already been decrypted.
//
  CRYPT_E_ALREADY_DECRYPTED = HRESULT($80091009);

//
// MessageId: CRYPT_E_NOT_DECRYPTED
//
// MessageText:
//
//  The content of the cryptographic message has not been decrypted yet.
//
  CRYPT_E_NOT_DECRYPTED = HRESULT($8009100A);

//
// MessageId: CRYPT_E_RECIPIENT_NOT_FOUND
//
// MessageText:
//
//  The enveloped-data message does not contain the specified recipient.
//
  CRYPT_E_RECIPIENT_NOT_FOUND = HRESULT($8009100B);

//
// MessageId: CRYPT_E_CONTROL_TYPE
//
// MessageText:
//
//  Invalid control type.
//
  CRYPT_E_CONTROL_TYPE = HRESULT($8009100C);

//
// MessageId: CRYPT_E_ISSUER_SERIALNUMBER
//
// MessageText:
//
//  Invalid issuer and/or serial number.
//
  CRYPT_E_ISSUER_SERIALNUMBER = HRESULT($8009100D);

//
// MessageId: CRYPT_E_SIGNER_NOT_FOUND
//
// MessageText:
//
//  Cannot find the original signer.
//
  CRYPT_E_SIGNER_NOT_FOUND = HRESULT($8009100E);

//
// MessageId: CRYPT_E_ATTRIBUTES_MISSING
//
// MessageText:
//
//  The cryptographic message does not contain all of the requested attributes.
//
  CRYPT_E_ATTRIBUTES_MISSING = HRESULT($8009100F);

//
// MessageId: CRYPT_E_STREAM_MSG_NOT_READY
//
// MessageText:
//
//  The streamed cryptographic message is not ready to return data.
//
  CRYPT_E_STREAM_MSG_NOT_READY = HRESULT($80091010);

//
// MessageId: CRYPT_E_STREAM_INSUFFICIENT_DATA
//
// MessageText:
//
//  The streamed cryptographic message requires more data to complete the decode operation.
//
  CRYPT_E_STREAM_INSUFFICIENT_DATA = HRESULT($80091011);

//
// MessageId: CRYPT_I_NEW_PROTECTION_REQUIRED
//
// MessageText:
//
//  The protected data needs to be re-protected.
//
  CRYPT_I_NEW_PROTECTION_REQUIRED = HRESULT($00091012);

//
// MessageId: CRYPT_E_BAD_LEN
//
// MessageText:
//
//  The length specified for the output data was insufficient.
//
  CRYPT_E_BAD_LEN = HRESULT($80092001);

//
// MessageId: CRYPT_E_BAD_ENCODE
//
// MessageText:
//
//  An error occurred during encode or decode operation.
//
  CRYPT_E_BAD_ENCODE = HRESULT($80092002);

//
// MessageId: CRYPT_E_FILE_ERROR
//
// MessageText:
//
//  An error occurred while reading or writing to a file.
//
  CRYPT_E_FILE_ERROR = HRESULT($80092003);

//
// MessageId: CRYPT_E_NOT_FOUND
//
// MessageText:
//
//  Cannot find object or property.
//
  CRYPT_E_NOT_FOUND = HRESULT($80092004);

//
// MessageId: CRYPT_E_EXISTS
//
// MessageText:
//
//  The object or property already exists.
//
  CRYPT_E_EXISTS = HRESULT($80092005);

//
// MessageId: CRYPT_E_NO_PROVIDER
//
// MessageText:
//
//  No provider was specified for the store or object.
//
  CRYPT_E_NO_PROVIDER = HRESULT($80092006);

//
// MessageId: CRYPT_E_SELF_SIGNED
//
// MessageText:
//
//  The specified certificate is self signed.
//
  CRYPT_E_SELF_SIGNED = HRESULT($80092007);

//
// MessageId: CRYPT_E_DELETED_PREV
//
// MessageText:
//
//  The previous certificate or CRL context was deleted.
//
  CRYPT_E_DELETED_PREV = HRESULT($80092008);

//
// MessageId: CRYPT_E_NO_MATCH
//
// MessageText:
//
//  Cannot find the requested object.
//
  CRYPT_E_NO_MATCH = HRESULT($80092009);

//
// MessageId: CRYPT_E_UNEXPECTED_MSG_TYPE
//
// MessageText:
//
//  The certificate does not have a property that references a private key.
//
  CRYPT_E_UNEXPECTED_MSG_TYPE = HRESULT($8009200A);

//
// MessageId: CRYPT_E_NO_KEY_PROPERTY
//
// MessageText:
//
//  Cannot find the certificate and private key for decryption.
//
  CRYPT_E_NO_KEY_PROPERTY = HRESULT($8009200B);

//
// MessageId: CRYPT_E_NO_DECRYPT_CERT
//
// MessageText:
//
//  Cannot find the certificate and private key to use for decryption.
//
  CRYPT_E_NO_DECRYPT_CERT = HRESULT($8009200C);

//
// MessageId: CRYPT_E_BAD_MSG
//
// MessageText:
//
//  Not a cryptographic message or the cryptographic message is not formatted correctly.
//
  CRYPT_E_BAD_MSG = HRESULT($8009200D);

//
// MessageId: CRYPT_E_NO_SIGNER
//
// MessageText:
//
//  The signed cryptographic message does not have a signer for the specified signer index.
//
  CRYPT_E_NO_SIGNER = HRESULT($8009200E);

//
// MessageId: CRYPT_E_PENDING_CLOSE
//
// MessageText:
//
//  Final closure is pending until additional frees or closes.
//
  CRYPT_E_PENDING_CLOSE = HRESULT($8009200F);

//
// MessageId: CRYPT_E_REVOKED
//
// MessageText:
//
//  The certificate is revoked.
//
  CRYPT_E_REVOKED = HRESULT($80092010);

//
// MessageId: CRYPT_E_NO_REVOCATION_DLL
//
// MessageText:
//
//  No Dll or exported function was found to verify revocation.
//
  CRYPT_E_NO_REVOCATION_DLL = HRESULT($80092011);

//
// MessageId: CRYPT_E_NO_REVOCATION_CHECK
//
// MessageText:
//
//  The revocation function was unable to check revocation for the certificate.
//
  CRYPT_E_NO_REVOCATION_CHECK = HRESULT($80092012);

//
// MessageId: CRYPT_E_REVOCATION_OFFLINE
//
// MessageText:
//
//  The revocation function was unable to check revocation because the revocation server was offline.
//
  CRYPT_E_REVOCATION_OFFLINE = HRESULT($80092013);

//
// MessageId: CRYPT_E_NOT_IN_REVOCATION_DATABASE
//
// MessageText:
//
//  The certificate is not in the revocation server's database.
//
  CRYPT_E_NOT_IN_REVOCATION_DATABASE = HRESULT($80092014);

//
// MessageId: CRYPT_E_INVALID_NUMERIC_STRING
//
// MessageText:
//
//  The string contains a non-numeric character.
//
  CRYPT_E_INVALID_NUMERIC_STRING = HRESULT($80092020);

//
// MessageId: CRYPT_E_INVALID_PRINTABLE_STRING
//
// MessageText:
//
//  The string contains a non-printable character.
//
  CRYPT_E_INVALID_PRINTABLE_STRING = HRESULT($80092021);

//
// MessageId: CRYPT_E_INVALID_IA5_STRING
//
// MessageText:
//
//  The string contains a character not in the 7 bit ASCII character set.
//
  CRYPT_E_INVALID_IA5_STRING = HRESULT($80092022);

//
// MessageId: CRYPT_E_INVALID_X500_STRING
//
// MessageText:
//
//  The string contains an invalid X500 name attribute key, oid, value or delimiter.
//
  CRYPT_E_INVALID_X500_STRING = HRESULT($80092023);

//
// MessageId: CRYPT_E_NOT_CHAR_STRING
//
// MessageText:
//
//  The dwValueType for the CERT_NAME_VALUE is not one of the character strings.  Most likely it is either a CERT_RDN_ENCODED_BLOB or CERT_TDN_OCTED_STRING.
//
  CRYPT_E_NOT_CHAR_STRING = HRESULT($80092024);

//
// MessageId: CRYPT_E_FILERESIZED
//
// MessageText:
//
//  The Put operation can not continue.  The file needs to be resized.  However, there is already a signature present.  A complete signing operation must be done.
//
  CRYPT_E_FILERESIZED = HRESULT($80092025);

//
// MessageId: CRYPT_E_SECURITY_SETTINGS
//
// MessageText:
//
//  The cryptographic operation failed due to a local security option setting.
//
  CRYPT_E_SECURITY_SETTINGS = HRESULT($80092026);

//
// MessageId: CRYPT_E_NO_VERIFY_USAGE_DLL
//
// MessageText:
//
//  No DLL or exported function was found to verify subject usage.
//
  CRYPT_E_NO_VERIFY_USAGE_DLL = HRESULT($80092027);

//
// MessageId: CRYPT_E_NO_VERIFY_USAGE_CHECK
//
// MessageText:
//
//  The called function was unable to do a usage check on the subject.
//
  CRYPT_E_NO_VERIFY_USAGE_CHECK = HRESULT($80092028);

//
// MessageId: CRYPT_E_VERIFY_USAGE_OFFLINE
//
// MessageText:
//
//  Since the server was offline, the called function was unable to complete the usage check.
//
  CRYPT_E_VERIFY_USAGE_OFFLINE = HRESULT($80092029);

//
// MessageId: CRYPT_E_NOT_IN_CTL
//
// MessageText:
//
//  The subject was not found in a Certificate Trust List (CTL).
//
  CRYPT_E_NOT_IN_CTL = HRESULT($8009202A);

//
// MessageId: CRYPT_E_NO_TRUSTED_SIGNER
//
// MessageText:
//
//  None of the signers of the cryptographic message or certificate trust list is trusted.
//
  CRYPT_E_NO_TRUSTED_SIGNER = HRESULT($8009202B);

//
// MessageId: CRYPT_E_MISSING_PUBKEY_PARA
//
// MessageText:
//
//  The public key's algorithm parameters are missing.
//
  CRYPT_E_MISSING_PUBKEY_PARA = HRESULT($8009202C);

//
// MessageId: CRYPT_E_OSS_ERROR
//
// MessageText:
//
//  OSS Certificate encode/decode error code base
//  
//  See asn1code.h for a definition of the OSS runtime errors. The OSS
//  error values are offset by CRYPT_E_OSS_ERROR.
//
  CRYPT_E_OSS_ERROR = HRESULT($80093000);

//
// MessageId: OSS_MORE_BUF
//
// MessageText:
//
//  OSS ASN.1 Error: Output Buffer is too small.
//
  OSS_MORE_BUF = HRESULT($80093001);

//
// MessageId: OSS_NEGATIVE_UINTEGER
//
// MessageText:
//
//  OSS ASN.1 Error: Signed integer is encoded as a unsigned integer.
//
  OSS_NEGATIVE_UINTEGER = HRESULT($80093002);

//
// MessageId: OSS_PDU_RANGE
//
// MessageText:
//
//  OSS ASN.1 Error: Unknown ASN.1 data type.
//
  OSS_PDU_RANGE = HRESULT($80093003);

//
// MessageId: OSS_MORE_INPUT
//
// MessageText:
//
//  OSS ASN.1 Error: Output buffer is too small, the decoded data has been truncated.
//
  OSS_MORE_INPUT = HRESULT($80093004);

//
// MessageId: OSS_DATA_ERROR
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_DATA_ERROR = HRESULT($80093005);

//
// MessageId: OSS_BAD_ARG
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid argument.
//
  OSS_BAD_ARG = HRESULT($80093006);

//
// MessageId: OSS_BAD_VERSION
//
// MessageText:
//
//  OSS ASN.1 Error: Encode/Decode version mismatch.
//
  OSS_BAD_VERSION = HRESULT($80093007);

//
// MessageId: OSS_OUT_MEMORY
//
// MessageText:
//
//  OSS ASN.1 Error: Out of memory.
//
  OSS_OUT_MEMORY = HRESULT($80093008);

//
// MessageId: OSS_PDU_MISMATCH
//
// MessageText:
//
//  OSS ASN.1 Error: Encode/Decode Error.
//
  OSS_PDU_MISMATCH = HRESULT($80093009);

//
// MessageId: OSS_LIMITED
//
// MessageText:
//
//  OSS ASN.1 Error: Internal Error.
//
  OSS_LIMITED = HRESULT($8009300A);

//
// MessageId: OSS_BAD_PTR
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_BAD_PTR = HRESULT($8009300B);

//
// MessageId: OSS_BAD_TIME
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_BAD_TIME = HRESULT($8009300C);

//
// MessageId: OSS_INDEFINITE_NOT_SUPPORTED
//
// MessageText:
//
//  OSS ASN.1 Error: Unsupported BER indefinite-length encoding.
//
  OSS_INDEFINITE_NOT_SUPPORTED = HRESULT($8009300D);

//
// MessageId: OSS_MEM_ERROR
//
// MessageText:
//
//  OSS ASN.1 Error: Access violation.
//
  OSS_MEM_ERROR = HRESULT($8009300E);

//
// MessageId: OSS_BAD_TABLE
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_BAD_TABLE = HRESULT($8009300F);

//
// MessageId: OSS_TOO_LONG
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_TOO_LONG = HRESULT($80093010);

//
// MessageId: OSS_CONSTRAINT_VIOLATED
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_CONSTRAINT_VIOLATED = HRESULT($80093011);

//
// MessageId: OSS_FATAL_ERROR
//
// MessageText:
//
//  OSS ASN.1 Error: Internal Error.
//
  OSS_FATAL_ERROR = HRESULT($80093012);

//
// MessageId: OSS_ACCESS_SERIALIZATION_ERROR
//
// MessageText:
//
//  OSS ASN.1 Error: Multi-threading conflict.
//
  OSS_ACCESS_SERIALIZATION_ERROR = HRESULT($80093013);

//
// MessageId: OSS_NULL_TBL
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_NULL_TBL = HRESULT($80093014);

//
// MessageId: OSS_NULL_FCN
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_NULL_FCN = HRESULT($80093015);

//
// MessageId: OSS_BAD_ENCRULES
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_BAD_ENCRULES = HRESULT($80093016);

//
// MessageId: OSS_UNAVAIL_ENCRULES
//
// MessageText:
//
//  OSS ASN.1 Error: Encode/Decode function not implemented.
//
  OSS_UNAVAIL_ENCRULES = HRESULT($80093017);

//
// MessageId: OSS_CANT_OPEN_TRACE_WINDOW
//
// MessageText:
//
//  OSS ASN.1 Error: Trace file error.
//
  OSS_CANT_OPEN_TRACE_WINDOW = HRESULT($80093018);

//
// MessageId: OSS_UNIMPLEMENTED
//
// MessageText:
//
//  OSS ASN.1 Error: Function not implemented.
//
  OSS_UNIMPLEMENTED = HRESULT($80093019);

//
// MessageId: OSS_OID_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_OID_DLL_NOT_LINKED = HRESULT($8009301A);

//
// MessageId: OSS_CANT_OPEN_TRACE_FILE
//
// MessageText:
//
//  OSS ASN.1 Error: Trace file error.
//
  OSS_CANT_OPEN_TRACE_FILE = HRESULT($8009301B);

//
// MessageId: OSS_TRACE_FILE_ALREADY_OPEN
//
// MessageText:
//
//  OSS ASN.1 Error: Trace file error.
//
  OSS_TRACE_FILE_ALREADY_OPEN = HRESULT($8009301C);

//
// MessageId: OSS_TABLE_MISMATCH
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_TABLE_MISMATCH = HRESULT($8009301D);

//
// MessageId: OSS_TYPE_NOT_SUPPORTED
//
// MessageText:
//
//  OSS ASN.1 Error: Invalid data.
//
  OSS_TYPE_NOT_SUPPORTED = HRESULT($8009301E);

//
// MessageId: OSS_REAL_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_REAL_DLL_NOT_LINKED = HRESULT($8009301F);

//
// MessageId: OSS_REAL_CODE_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_REAL_CODE_NOT_LINKED = HRESULT($80093020);

//
// MessageId: OSS_OUT_OF_RANGE
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_OUT_OF_RANGE = HRESULT($80093021);

//
// MessageId: OSS_COPIER_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_COPIER_DLL_NOT_LINKED = HRESULT($80093022);

//
// MessageId: OSS_CONSTRAINT_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_CONSTRAINT_DLL_NOT_LINKED = HRESULT($80093023);

//
// MessageId: OSS_COMPARATOR_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_COMPARATOR_DLL_NOT_LINKED = HRESULT($80093024);

//
// MessageId: OSS_COMPARATOR_CODE_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_COMPARATOR_CODE_NOT_LINKED = HRESULT($80093025);

//
// MessageId: OSS_MEM_MGR_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_MEM_MGR_DLL_NOT_LINKED = HRESULT($80093026);

//
// MessageId: OSS_PDV_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_PDV_DLL_NOT_LINKED = HRESULT($80093027);

//
// MessageId: OSS_PDV_CODE_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_PDV_CODE_NOT_LINKED = HRESULT($80093028);

//
// MessageId: OSS_API_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_API_DLL_NOT_LINKED = HRESULT($80093029);

//
// MessageId: OSS_BERDER_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_BERDER_DLL_NOT_LINKED = HRESULT($8009302A);

//
// MessageId: OSS_PER_DLL_NOT_LINKED
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_PER_DLL_NOT_LINKED = HRESULT($8009302B);

//
// MessageId: OSS_OPEN_TYPE_ERROR
//
// MessageText:
//
//  OSS ASN.1 Error: Program link error.
//
  OSS_OPEN_TYPE_ERROR = HRESULT($8009302C);

//
// MessageId: OSS_MUTEX_NOT_CREATED
//
// MessageText:
//
//  OSS ASN.1 Error: System resource error.
//
  OSS_MUTEX_NOT_CREATED = HRESULT($8009302D);

//
// MessageId: OSS_CANT_CLOSE_TRACE_FILE
//
// MessageText:
//
//  OSS ASN.1 Error: Trace file error.
//
  OSS_CANT_CLOSE_TRACE_FILE = HRESULT($8009302E);

//
// MessageId: CRYPT_E_ASN1_ERROR
//
// MessageText:
//
//  ASN1 Certificate encode/decode error code base.
//  
//  The ASN1 error values are offset by CRYPT_E_ASN1_ERROR.
//
  CRYPT_E_ASN1_ERROR = HRESULT($80093100);

//
// MessageId: CRYPT_E_ASN1_INTERNAL
//
// MessageText:
//
//  ASN1 internal encode or decode error.
//
  CRYPT_E_ASN1_INTERNAL = HRESULT($80093101);

//
// MessageId: CRYPT_E_ASN1_EOD
//
// MessageText:
//
//  ASN1 unexpected end of data.
//
  CRYPT_E_ASN1_EOD = HRESULT($80093102);

//
// MessageId: CRYPT_E_ASN1_CORRUPT
//
// MessageText:
//
//  ASN1 corrupted data.
//
  CRYPT_E_ASN1_CORRUPT = HRESULT($80093103);

//
// MessageId: CRYPT_E_ASN1_LARGE
//
// MessageText:
//
//  ASN1 value too large.
//
  CRYPT_E_ASN1_LARGE = HRESULT($80093104);

//
// MessageId: CRYPT_E_ASN1_CONSTRAINT
//
// MessageText:
//
//  ASN1 constraint violated.
//
  CRYPT_E_ASN1_CONSTRAINT = HRESULT($80093105);

//
// MessageId: CRYPT_E_ASN1_MEMORY
//
// MessageText:
//
//  ASN1 out of memory.
//
  CRYPT_E_ASN1_MEMORY = HRESULT($80093106);

//
// MessageId: CRYPT_E_ASN1_OVERFLOW
//
// MessageText:
//
//  ASN1 buffer overflow.
//
  CRYPT_E_ASN1_OVERFLOW = HRESULT($80093107);

//
// MessageId: CRYPT_E_ASN1_BADPDU
//
// MessageText:
//
//  ASN1 function not supported for this PDU.
//
  CRYPT_E_ASN1_BADPDU = HRESULT($80093108);

//
// MessageId: CRYPT_E_ASN1_BADARGS
//
// MessageText:
//
//  ASN1 bad arguments to function call.
//
  CRYPT_E_ASN1_BADARGS = HRESULT($80093109);

//
// MessageId: CRYPT_E_ASN1_BADREAL
//
// MessageText:
//
//  ASN1 bad real value.
//
  CRYPT_E_ASN1_BADREAL = HRESULT($8009310A);

//
// MessageId: CRYPT_E_ASN1_BADTAG
//
// MessageText:
//
//  ASN1 bad tag value met.
//
  CRYPT_E_ASN1_BADTAG = HRESULT($8009310B);

//
// MessageId: CRYPT_E_ASN1_CHOICE
//
// MessageText:
//
//  ASN1 bad choice value.
//
  CRYPT_E_ASN1_CHOICE = HRESULT($8009310C);

//
// MessageId: CRYPT_E_ASN1_RULE
//
// MessageText:
//
//  ASN1 bad encoding rule.
//
  CRYPT_E_ASN1_RULE = HRESULT($8009310D);

//
// MessageId: CRYPT_E_ASN1_UTF8
//
// MessageText:
//
//  ASN1 bad unicode (UTF8).
//
  CRYPT_E_ASN1_UTF8 = HRESULT($8009310E);

//
// MessageId: CRYPT_E_ASN1_PDU_TYPE
//
// MessageText:
//
//  ASN1 bad PDU type.
//
  CRYPT_E_ASN1_PDU_TYPE = HRESULT($80093133);

//
// MessageId: CRYPT_E_ASN1_NYI
//
// MessageText:
//
//  ASN1 not yet implemented.
//
  CRYPT_E_ASN1_NYI = HRESULT($80093134);

//
// MessageId: CRYPT_E_ASN1_EXTENDED
//
// MessageText:
//
//  ASN1 skipped unknown extension(s).
//
  CRYPT_E_ASN1_EXTENDED = HRESULT($80093201);

//
// MessageId: CRYPT_E_ASN1_NOEOD
//
// MessageText:
//
//  ASN1 end of data expected
//
  CRYPT_E_ASN1_NOEOD = HRESULT($80093202);

//
// MessageId: CERTSRV_E_BAD_REQUESTSUBJECT
//
// MessageText:
//
//  The request subject name is invalid or too long.
//
  CERTSRV_E_BAD_REQUESTSUBJECT = HRESULT($80094001);

//
// MessageId: CERTSRV_E_NO_REQUEST
//
// MessageText:
//
//  The request does not exist.
//
  CERTSRV_E_NO_REQUEST = HRESULT($80094002);

//
// MessageId: CERTSRV_E_BAD_REQUESTSTATUS
//
// MessageText:
//
//  The request's current status does not allow this operation.
//
  CERTSRV_E_BAD_REQUESTSTATUS = HRESULT($80094003);

//
// MessageId: CERTSRV_E_PROPERTY_EMPTY
//
// MessageText:
//
//  The requested property value is empty.
//
  CERTSRV_E_PROPERTY_EMPTY = HRESULT($80094004);

//
// MessageId: CERTSRV_E_INVALID_CA_CERTIFICATE
//
// MessageText:
//
//  The certification authority's certificate contains invalid data.
//
  CERTSRV_E_INVALID_CA_CERTIFICATE = HRESULT($80094005);

//
// MessageId: CERTSRV_E_SERVER_SUSPENDED
//
// MessageText:
//
//  Certificate service has been suspended for a database restore operation.
//
  CERTSRV_E_SERVER_SUSPENDED = HRESULT($80094006);

//
// MessageId: CERTSRV_E_ENCODING_LENGTH
//
// MessageText:
//
//  The certificate contains an encoded length that is potentially incompatible with older enrollment software.
//
  CERTSRV_E_ENCODING_LENGTH = HRESULT($80094007);

//
// MessageId: CERTSRV_E_ROLECONFLICT
//
// MessageText:
//
//  The operation is denied. The user has multiple roles assigned and the certification authority is configured to enforce role separation.
//
  CERTSRV_E_ROLECONFLICT = HRESULT($80094008);

//
// MessageId: CERTSRV_E_RESTRICTEDOFFICER
//
// MessageText:
//
//  The operation is denied. It can only be performed by a certificate manager that is allowed to manage certificates for the current requester.
//
  CERTSRV_E_RESTRICTEDOFFICER = HRESULT($80094009);

//
// MessageId: CERTSRV_E_KEY_ARCHIVAL_NOT_CONFIGURED
//
// MessageText:
//
//  Cannot archive private key.  The certification authority is not configured for key archival.
//
  CERTSRV_E_KEY_ARCHIVAL_NOT_CONFIGURED = HRESULT($8009400A);

//
// MessageId: CERTSRV_E_NO_VALID_KRA
//
// MessageText:
//
//  Cannot archive private key.  The certification authority could not verify one or more key recovery certificates.
//
  CERTSRV_E_NO_VALID_KRA = HRESULT($8009400B);

//
// MessageId: CERTSRV_E_BAD_REQUEST_KEY_ARCHIVAL
//
// MessageText:
//
//  The request is incorrectly formatted.  The encrypted private key must be in an unauthenticated attribute in an outermost signature.
//
  CERTSRV_E_BAD_REQUEST_KEY_ARCHIVAL = HRESULT($8009400C);

//
// MessageId: CERTSRV_E_NO_CAADMIN_DEFINED
//
// MessageText:
//
//  At least one security principal must have the permission to manage this CA.
//
  CERTSRV_E_NO_CAADMIN_DEFINED = HRESULT($8009400D);

//
// MessageId: CERTSRV_E_BAD_RENEWAL_CERT_ATTRIBUTE
//
// MessageText:
//
//  The request contains an invalid renewal certificate attribute.
//
  CERTSRV_E_BAD_RENEWAL_CERT_ATTRIBUTE = HRESULT($8009400E);

//
// MessageId: CERTSRV_E_NO_DB_SESSIONS
//
// MessageText:
//
//  An attempt was made to open a Certification Authority database session, but there are already too many active sessions.  The server may need to be configured to allow additional sessions.
//
  CERTSRV_E_NO_DB_SESSIONS = HRESULT($8009400F);

//
// MessageId: CERTSRV_E_ALIGNMENT_FAULT
//
// MessageText:
//
//  A memory reference caused a data alignment fault.
//
  CERTSRV_E_ALIGNMENT_FAULT = HRESULT($80094010);

//
// MessageId: CERTSRV_E_ENROLL_DENIED
//
// MessageText:
//
//  The permissions on this certification authority do not allow the current user to enroll for certificates.
//
  CERTSRV_E_ENROLL_DENIED = HRESULT($80094011);

//
// MessageId: CERTSRV_E_TEMPLATE_DENIED
//
// MessageText:
//
//  The permissions on the certificate template do not allow the current user to enroll for this type of certificate.
//
  CERTSRV_E_TEMPLATE_DENIED = HRESULT($80094012);

//
// MessageId: CERTSRV_E_DOWNLEVEL_DC_SSL_OR_UPGRADE
//
// MessageText:
//
//  The contacted domain controller cannot support signed LDAP traffic.  Update the domain controller or configure Certificate Services to use SSL for Active Directory access.
//
  CERTSRV_E_DOWNLEVEL_DC_SSL_OR_UPGRADE = HRESULT($80094013);

//
// MessageId: CERTSRV_E_UNSUPPORTED_CERT_TYPE
//
// MessageText:
//
//  The requested certificate template is not supported by this CA.
//
  CERTSRV_E_UNSUPPORTED_CERT_TYPE = HRESULT($80094800);

//
// MessageId: CERTSRV_E_NO_CERT_TYPE
//
// MessageText:
//
//  The request contains no certificate template information.
//
  CERTSRV_E_NO_CERT_TYPE = HRESULT($80094801);

//
// MessageId: CERTSRV_E_TEMPLATE_CONFLICT
//
// MessageText:
//
//  The request contains conflicting template information.
//
  CERTSRV_E_TEMPLATE_CONFLICT = HRESULT($80094802);

//
// MessageId: CERTSRV_E_SUBJECT_ALT_NAME_REQUIRED
//
// MessageText:
//
//  The request is missing a required Subject Alternate name extension.
//
  CERTSRV_E_SUBJECT_ALT_NAME_REQUIRED = HRESULT($80094803);

//
// MessageId: CERTSRV_E_ARCHIVED_KEY_REQUIRED
//
// MessageText:
//
//  The request is missing a required private key for archival by the server.
//
  CERTSRV_E_ARCHIVED_KEY_REQUIRED = HRESULT($80094804);

//
// MessageId: CERTSRV_E_SMIME_REQUIRED
//
// MessageText:
//
//  The request is missing a required SMIME capabilities extension.
//
  CERTSRV_E_SMIME_REQUIRED = HRESULT($80094805);

//
// MessageId: CERTSRV_E_BAD_RENEWAL_SUBJECT
//
// MessageText:
//
//  The request was made on behalf of a subject other than the caller.  The certificate template must be configured to require at least one signature to authorize the request.
//
  CERTSRV_E_BAD_RENEWAL_SUBJECT = HRESULT($80094806);

//
// MessageId: CERTSRV_E_BAD_TEMPLATE_VERSION
//
// MessageText:
//
//  The request template version is newer than the supported template version.
//
  CERTSRV_E_BAD_TEMPLATE_VERSION = HRESULT($80094807);

//
// MessageId: CERTSRV_E_TEMPLATE_POLICY_REQUIRED
//
// MessageText:
//
//  The template is missing a required signature policy attribute.
//
  CERTSRV_E_TEMPLATE_POLICY_REQUIRED = HRESULT($80094808);

//
// MessageId: CERTSRV_E_SIGNATURE_POLICY_REQUIRED
//
// MessageText:
//
//  The request is missing required signature policy information.
//
  CERTSRV_E_SIGNATURE_POLICY_REQUIRED = HRESULT($80094809);

//
// MessageId: CERTSRV_E_SIGNATURE_COUNT
//
// MessageText:
//
//  The request is missing one or more required signatures.
//
  CERTSRV_E_SIGNATURE_COUNT = HRESULT($8009480A);

//
// MessageId: CERTSRV_E_SIGNATURE_REJECTED
//
// MessageText:
//
//  One or more signatures did not include the required application or issuance policies.  The request is missing one or more required valid signatures.
//
  CERTSRV_E_SIGNATURE_REJECTED = HRESULT($8009480B);

//
// MessageId: CERTSRV_E_ISSUANCE_POLICY_REQUIRED
//
// MessageText:
//
//  The request is missing one or more required signature issuance policies.
//
  CERTSRV_E_ISSUANCE_POLICY_REQUIRED = HRESULT($8009480C);

//
// MessageId: CERTSRV_E_SUBJECT_UPN_REQUIRED
//
// MessageText:
//
//  The UPN is unavailable and cannot be added to the Subject Alternate name.
//
  CERTSRV_E_SUBJECT_UPN_REQUIRED = HRESULT($8009480D);

//
// MessageId: CERTSRV_E_SUBJECT_DIRECTORY_GUID_REQUIRED
//
// MessageText:
//
//  The Active Directory GUID is unavailable and cannot be added to the Subject Alternate name.
//
  CERTSRV_E_SUBJECT_DIRECTORY_GUID_REQUIRED = HRESULT($8009480E);

//
// MessageId: CERTSRV_E_SUBJECT_DNS_REQUIRED
//
// MessageText:
//
//  The DNS name is unavailable and cannot be added to the Subject Alternate name.
//
  CERTSRV_E_SUBJECT_DNS_REQUIRED = HRESULT($8009480F);

//
// MessageId: CERTSRV_E_ARCHIVED_KEY_UNEXPECTED
//
// MessageText:
//
//  The request includes a private key for archival by the server, but key archival is not enabled for the specified certificate template.
//
  CERTSRV_E_ARCHIVED_KEY_UNEXPECTED = HRESULT($80094810);

//
// MessageId: CERTSRV_E_KEY_LENGTH
//
// MessageText:
//
//  The public key does not meet the minimum size required by the specified certificate template.
//
  CERTSRV_E_KEY_LENGTH = HRESULT($80094811);

//
// MessageId: CERTSRV_E_SUBJECT_EMAIL_REQUIRED
//
// MessageText:
//
//  The EMail name is unavailable and cannot be added to the Subject or Subject Alternate name.
//
  CERTSRV_E_SUBJECT_EMAIL_REQUIRED = HRESULT($80094812);

//
// MessageId: CERTSRV_E_UNKNOWN_CERT_TYPE
//
// MessageText:
//
//  One or more certificate templates to be enabled on this certification authority could not be found.
//
  CERTSRV_E_UNKNOWN_CERT_TYPE = HRESULT($80094813);

//
// MessageId: CERTSRV_E_CERT_TYPE_OVERLAP
//
// MessageText:
//
//  The certificate template renewal period is longer than the certificate validity period.  The template should be reconfigured or the CA certificate renewed.
//
  CERTSRV_E_CERT_TYPE_OVERLAP = HRESULT($80094814);

//
// The range 0x5000-0x51ff is reserved for XENROLL errors.
//
//
// MessageId: XENROLL_E_KEY_NOT_EXPORTABLE
//
// MessageText:
//
//  The key is not exportable.
//
  XENROLL_E_KEY_NOT_EXPORTABLE = HRESULT($80095000);

//
// MessageId: XENROLL_E_CANNOT_ADD_ROOT_CERT
//
// MessageText:
//
//  You cannot add the root CA certificate into your local store.
//
  XENROLL_E_CANNOT_ADD_ROOT_CERT = HRESULT($80095001);

//
// MessageId: XENROLL_E_RESPONSE_KA_HASH_NOT_FOUND
//
// MessageText:
//
//  The key archival hash attribute was not found in the response.
//
  XENROLL_E_RESPONSE_KA_HASH_NOT_FOUND = HRESULT($80095002);

//
// MessageId: XENROLL_E_RESPONSE_UNEXPECTED_KA_HASH
//
// MessageText:
//
//  An unexpected key archival hash attribute was found in the response.
//
  XENROLL_E_RESPONSE_UNEXPECTED_KA_HASH = HRESULT($80095003);

//
// MessageId: XENROLL_E_RESPONSE_KA_HASH_MISMATCH
//
// MessageText:
//
//  There is a key archival hash mismatch between the request and the response.
//
  XENROLL_E_RESPONSE_KA_HASH_MISMATCH = HRESULT($80095004);

//
// MessageId: XENROLL_E_KEYSPEC_SMIME_MISMATCH
//
// MessageText:
//
//  Signing certificate cannot include SMIME extension.
//
  XENROLL_E_KEYSPEC_SMIME_MISMATCH = HRESULT($80095005);

//
// MessageId: TRUST_E_SYSTEM_ERROR
//
// MessageText:
//
//  A system-level error occurred while verifying trust.
//
  TRUST_E_SYSTEM_ERROR = HRESULT($80096001);

//
// MessageId: TRUST_E_NO_SIGNER_CERT
//
// MessageText:
//
//  The certificate for the signer of the message is invalid or not found.
//
  TRUST_E_NO_SIGNER_CERT = HRESULT($80096002);

//
// MessageId: TRUST_E_COUNTER_SIGNER
//
// MessageText:
//
//  One of the counter signatures was invalid.
//
  TRUST_E_COUNTER_SIGNER = HRESULT($80096003);

//
// MessageId: TRUST_E_CERT_SIGNATURE
//
// MessageText:
//
//  The signature of the certificate can not be verified.
//
  TRUST_E_CERT_SIGNATURE = HRESULT($80096004);

//
// MessageId: TRUST_E_TIME_STAMP
//
// MessageText:
//
//  The timestamp signature and/or certificate could not be verified or is malformed.
//
  TRUST_E_TIME_STAMP = HRESULT($80096005);

//
// MessageId: TRUST_E_BAD_DIGEST
//
// MessageText:
//
//  The digital signature of the object did not verify.
//
  TRUST_E_BAD_DIGEST = HRESULT($80096010);

//
// MessageId: TRUST_E_BASIC_CONSTRAINTS
//
// MessageText:
//
//  A certificate's basic constraint extension has not been observed.
//
  TRUST_E_BASIC_CONSTRAINTS = HRESULT($80096019);

//
// MessageId: TRUST_E_FINANCIAL_CRITERIA
//
// MessageText:
//
//  The certificate does not meet or contain the Authenticode(tm) financial extensions.
//
  TRUST_E_FINANCIAL_CRITERIA = HRESULT($8009601E);

//
//  Error codes for mssipotf.dll
//  Most of the error codes can only occur when an error occurs
//    during font file signing
//
//
//
// MessageId: MSSIPOTF_E_OUTOFMEMRANGE
//
// MessageText:
//
//  Tried to reference a part of the file outside the proper range.
//
  MSSIPOTF_E_OUTOFMEMRANGE = HRESULT($80097001);

//
// MessageId: MSSIPOTF_E_CANTGETOBJECT
//
// MessageText:
//
//  Could not retrieve an object from the file.
//
  MSSIPOTF_E_CANTGETOBJECT = HRESULT($80097002);

//
// MessageId: MSSIPOTF_E_NOHEADTABLE
//
// MessageText:
//
//  Could not find the head table in the file.
//
  MSSIPOTF_E_NOHEADTABLE = HRESULT($80097003);

//
// MessageId: MSSIPOTF_E_BAD_MAGICNUMBER
//
// MessageText:
//
//  The magic number in the head table is incorrect.
//
  MSSIPOTF_E_BAD_MAGICNUMBER = HRESULT($80097004);

//
// MessageId: MSSIPOTF_E_BAD_OFFSET_TABLE
//
// MessageText:
//
//  The offset table has incorrect values.
//
  MSSIPOTF_E_BAD_OFFSET_TABLE = HRESULT($80097005);

//
// MessageId: MSSIPOTF_E_TABLE_TAGORDER
//
// MessageText:
//
//  Duplicate table tags or tags out of alphabetical order.
//
  MSSIPOTF_E_TABLE_TAGORDER = HRESULT($80097006);

//
// MessageId: MSSIPOTF_E_TABLE_LONGWORD
//
// MessageText:
//
//  A table does not start on a long word boundary.
//
  MSSIPOTF_E_TABLE_LONGWORD = HRESULT($80097007);

//
// MessageId: MSSIPOTF_E_BAD_FIRST_TABLE_PLACEMENT
//
// MessageText:
//
//  First table does not appear after header information.
//
  MSSIPOTF_E_BAD_FIRST_TABLE_PLACEMENT = HRESULT($80097008);

//
// MessageId: MSSIPOTF_E_TABLES_OVERLAP
//
// MessageText:
//
//  Two or more tables overlap.
//
  MSSIPOTF_E_TABLES_OVERLAP = HRESULT($80097009);

//
// MessageId: MSSIPOTF_E_TABLE_PADBYTES
//
// MessageText:
//
//  Too many pad bytes between tables or pad bytes are not 0.
//
  MSSIPOTF_E_TABLE_PADBYTES = HRESULT($8009700A);

//
// MessageId: MSSIPOTF_E_FILETOOSMALL
//
// MessageText:
//
//  File is too small to contain the last table.
//
  MSSIPOTF_E_FILETOOSMALL = HRESULT($8009700B);

//
// MessageId: MSSIPOTF_E_TABLE_CHECKSUM
//
// MessageText:
//
//  A table checksum is incorrect.
//
  MSSIPOTF_E_TABLE_CHECKSUM = HRESULT($8009700C);

//
// MessageId: MSSIPOTF_E_FILE_CHECKSUM
//
// MessageText:
//
//  The file checksum is incorrect.
//
  MSSIPOTF_E_FILE_CHECKSUM = HRESULT($8009700D);

//
// MessageId: MSSIPOTF_E_FAILED_POLICY
//
// MessageText:
//
//  The signature does not have the correct attributes for the policy.
//
  MSSIPOTF_E_FAILED_POLICY = HRESULT($80097010);

//
// MessageId: MSSIPOTF_E_FAILED_HINTS_CHECK
//
// MessageText:
//
//  The file did not pass the hints check.
//
  MSSIPOTF_E_FAILED_HINTS_CHECK = HRESULT($80097011);

//
// MessageId: MSSIPOTF_E_NOT_OPENTYPE
//
// MessageText:
//
//  The file is not an OpenType file.
//
  MSSIPOTF_E_NOT_OPENTYPE = HRESULT($80097012);

//
// MessageId: MSSIPOTF_E_FILE
//
// MessageText:
//
//  Failed on a file operation (open, map, read, write).
//
  MSSIPOTF_E_FILE = HRESULT($80097013);

//
// MessageId: MSSIPOTF_E_CRYPT
//
// MessageText:
//
//  A call to a CryptoAPI function failed.
//
  MSSIPOTF_E_CRYPT = HRESULT($80097014);

//
// MessageId: MSSIPOTF_E_BADVERSION
//
// MessageText:
//
//  There is a bad version number in the file.
//
  MSSIPOTF_E_BADVERSION = HRESULT($80097015);

//
// MessageId: MSSIPOTF_E_DSIG_STRUCTURE
//
// MessageText:
//
//  The structure of the DSIG table is incorrect.
//
  MSSIPOTF_E_DSIG_STRUCTURE = HRESULT($80097016);

//
// MessageId: MSSIPOTF_E_PCONST_CHECK
//
// MessageText:
//
//  A check failed in a partially constant table.
//
  MSSIPOTF_E_PCONST_CHECK = HRESULT($80097017);

//
// MessageId: MSSIPOTF_E_STRUCTURE
//
// MessageText:
//
//  Some kind of structural error.
//
  MSSIPOTF_E_STRUCTURE = HRESULT($80097018);

  NTE_OP_OK = 0;

//
// Note that additional FACILITY_SSPI errors are in issperr.h
//
// ******************
// FACILITY_CERT
// ******************
//
// MessageId: TRUST_E_PROVIDER_UNKNOWN
//
// MessageText:
//
//  Unknown trust provider.
//
  TRUST_E_PROVIDER_UNKNOWN = HRESULT($800B0001);

//
// MessageId: TRUST_E_ACTION_UNKNOWN
//
// MessageText:
//
//  The trust verification action specified is not supported by the specified trust provider.
//
  TRUST_E_ACTION_UNKNOWN = HRESULT($800B0002);

//
// MessageId: TRUST_E_SUBJECT_FORM_UNKNOWN
//
// MessageText:
//
//  The form specified for the subject is not one supported or known by the specified trust provider.
//
  TRUST_E_SUBJECT_FORM_UNKNOWN = HRESULT($800B0003);

//
// MessageId: TRUST_E_SUBJECT_NOT_TRUSTED
//
// MessageText:
//
//  The subject is not trusted for the specified action.
//
  TRUST_E_SUBJECT_NOT_TRUSTED = HRESULT($800B0004);

//
// MessageId: DIGSIG_E_ENCODE
//
// MessageText:
//
//  Error due to problem in ASN.1 encoding process.
//
  DIGSIG_E_ENCODE = HRESULT($800B0005);

//
// MessageId: DIGSIG_E_DECODE
//
// MessageText:
//
//  Error due to problem in ASN.1 decoding process.
//
  DIGSIG_E_DECODE = HRESULT($800B0006);

//
// MessageId: DIGSIG_E_EXTENSIBILITY
//
// MessageText:
//
//  Reading / writing Extensions where Attributes are appropriate, and visa versa.
//
  DIGSIG_E_EXTENSIBILITY = HRESULT($800B0007);

//
// MessageId: DIGSIG_E_CRYPTO
//
// MessageText:
//
//  Unspecified cryptographic failure.
//
  DIGSIG_E_CRYPTO = HRESULT($800B0008);

//
// MessageId: PERSIST_E_SIZEDEFINITE
//
// MessageText:
//
//  The size of the data could not be determined.
//
  PERSIST_E_SIZEDEFINITE = HRESULT($800B0009);

//
// MessageId: PERSIST_E_SIZEINDEFINITE
//
// MessageText:
//
//  The size of the indefinite-sized data could not be determined.
//
  PERSIST_E_SIZEINDEFINITE = HRESULT($800B000A);

//
// MessageId: PERSIST_E_NOTSELFSIZING
//
// MessageText:
//
//  This object does not read and write self-sizing data.
//
  PERSIST_E_NOTSELFSIZING = HRESULT($800B000B);

//
// MessageId: TRUST_E_NOSIGNATURE
//
// MessageText:
//
//  No signature was present in the subject.
//
  TRUST_E_NOSIGNATURE = HRESULT($800B0100);

//
// MessageId: CERT_E_EXPIRED
//
// MessageText:
//
//  A required certificate is not within its validity period when verifying against the current system clock or the timestamp in the signed file.
//
  CERT_E_EXPIRED = HRESULT($800B0101);

//
// MessageId: CERT_E_VALIDITYPERIODNESTING
//
// MessageText:
//
//  The validity periods of the certification chain do not nest correctly.
//
  CERT_E_VALIDITYPERIODNESTING = HRESULT($800B0102);

//
// MessageId: CERT_E_ROLE
//
// MessageText:
//
//  A certificate that can only be used as an end-entity is being used as a CA or visa versa.
//
  CERT_E_ROLE = HRESULT($800B0103);

//
// MessageId: CERT_E_PATHLENCONST
//
// MessageText:
//
//  A path length constraint in the certification chain has been violated.
//
  CERT_E_PATHLENCONST = HRESULT($800B0104);

//
// MessageId: CERT_E_CRITICAL
//
// MessageText:
//
//  A certificate contains an unknown extension that is marked 'critical'.
//
  CERT_E_CRITICAL = HRESULT($800B0105);

//
// MessageId: CERT_E_PURPOSE
//
// MessageText:
//
//  A certificate being used for a purpose other than the ones specified by its CA.
//
  CERT_E_PURPOSE = HRESULT($800B0106);

//
// MessageId: CERT_E_ISSUERCHAINING
//
// MessageText:
//
//  A parent of a given certificate in fact did not issue that child certificate.
//
  CERT_E_ISSUERCHAINING = HRESULT($800B0107);

//
// MessageId: CERT_E_MALFORMED
//
// MessageText:
//
//  A certificate is missing or has an empty value for an important field, such as a subject or issuer name.
//
  CERT_E_MALFORMED = HRESULT($800B0108);

//
// MessageId: CERT_E_UNTRUSTEDROOT
//
// MessageText:
//
//  A certificate chain processed, but terminated in a root certificate which is not trusted by the trust provider.
//
  CERT_E_UNTRUSTEDROOT = HRESULT($800B0109);

//
// MessageId: CERT_E_CHAINING
//
// MessageText:
//
//  A certificate chain could not be built to a trusted root authority.
//
  CERT_E_CHAINING = HRESULT($800B010A);

//
// MessageId: TRUST_E_FAIL
//
// MessageText:
//
//  Generic trust failure.
//
  TRUST_E_FAIL = HRESULT($800B010B);

//
// MessageId: CERT_E_REVOKED
//
// MessageText:
//
//  A certificate was explicitly revoked by its issuer.
//
  CERT_E_REVOKED = HRESULT($800B010C);

//
// MessageId: CERT_E_UNTRUSTEDTESTROOT
//
// MessageText:
//
//  The certification path terminates with the test root which is not trusted with the current policy settings.
//
  CERT_E_UNTRUSTEDTESTROOT = HRESULT($800B010D);

//
// MessageId: CERT_E_REVOCATION_FAILURE
//
// MessageText:
//
//  The revocation process could not continue - the certificate(s) could not be checked.
//
  CERT_E_REVOCATION_FAILURE = HRESULT($800B010E);

//
// MessageId: CERT_E_CN_NO_MATCH
//
// MessageText:
//
//  The certificate's CN name does not match the passed value.
//
  CERT_E_CN_NO_MATCH = HRESULT($800B010F);

//
// MessageId: CERT_E_WRONG_USAGE
//
// MessageText:
//
//  The certificate is not valid for the requested usage.
//
  CERT_E_WRONG_USAGE = HRESULT($800B0110);

//
// MessageId: TRUST_E_EXPLICIT_DISTRUST
//
// MessageText:
//
//  The certificate was explicitly marked as untrusted by the user.
//
  TRUST_E_EXPLICIT_DISTRUST = HRESULT($800B0111);

//
// MessageId: CERT_E_UNTRUSTEDCA
//
// MessageText:
//
//  A certification chain processed correctly, but one of the CA certificates is not trusted by the policy provider.
//
  CERT_E_UNTRUSTEDCA = HRESULT($800B0112);

//
// MessageId: CERT_E_INVALID_POLICY
//
// MessageText:
//
//  The certificate has invalid policy.
//
  CERT_E_INVALID_POLICY = HRESULT($800B0113);

//
// MessageId: CERT_E_INVALID_NAME
//
// MessageText:
//
//  The certificate has an invalid name. The name is not included in the permitted list or is explicitly excluded.
//
  CERT_E_INVALID_NAME = HRESULT($800B0114);

// *****************
// FACILITY_SETUPAPI
// *****************
//
// Since these error codes aren't in the standard Win32 range (i.e., 0-64K), define a
// macro to map either Win32 or SetupAPI error codes into an HRESULT.
//

function HRESULT_FROM_SETUPAPI(x: DWORD): HRESULT;

//
// MessageId: SPAPI_E_EXPECTED_SECTION_NAME
//
// MessageText:
//
//  A non-empty line was encountered in the INF before the start of a section.
//

const
  SPAPI_E_EXPECTED_SECTION_NAME = HRESULT($800F0000);

//
// MessageId: SPAPI_E_BAD_SECTION_NAME_LINE
//
// MessageText:
//
//  A section name marker in the INF is not complete, or does not exist on a line by itself.
//
  SPAPI_E_BAD_SECTION_NAME_LINE = HRESULT($800F0001);

//
// MessageId: SPAPI_E_SECTION_NAME_TOO_LONG
//
// MessageText:
//
//  An INF section was encountered whose name exceeds the maximum section name length.
//
  SPAPI_E_SECTION_NAME_TOO_LONG = HRESULT($800F0002);

//
// MessageId: SPAPI_E_GENERAL_SYNTAX
//
// MessageText:
//
//  The syntax of the INF is invalid.
//
  SPAPI_E_GENERAL_SYNTAX = HRESULT($800F0003);

//
// MessageId: SPAPI_E_WRONG_INF_STYLE
//
// MessageText:
//
//  The style of the INF is different than what was requested.
//
  SPAPI_E_WRONG_INF_STYLE = HRESULT($800F0100);

//
// MessageId: SPAPI_E_SECTION_NOT_FOUND
//
// MessageText:
//
//  The required section was not found in the INF.
//
  SPAPI_E_SECTION_NOT_FOUND = HRESULT($800F0101);

//
// MessageId: SPAPI_E_LINE_NOT_FOUND
//
// MessageText:
//
//  The required line was not found in the INF.
//
  SPAPI_E_LINE_NOT_FOUND = HRESULT($800F0102);

//
// MessageId: SPAPI_E_NO_BACKUP
//
// MessageText:
//
//  The files affected by the installation of this file queue have not been backed up for uninstall.
//
  SPAPI_E_NO_BACKUP = HRESULT($800F0103);

//
// MessageId: SPAPI_E_NO_ASSOCIATED_CLASS
//
// MessageText:
//
//  The INF or the device information set or element does not have an associated install class.
//
  SPAPI_E_NO_ASSOCIATED_CLASS = HRESULT($800F0200);

//
// MessageId: SPAPI_E_CLASS_MISMATCH
//
// MessageText:
//
//  The INF or the device information set or element does not match the specified install class.
//
  SPAPI_E_CLASS_MISMATCH = HRESULT($800F0201);

//
// MessageId: SPAPI_E_DUPLICATE_FOUND
//
// MessageText:
//
//  An existing device was found that is a duplicate of the device being manually installed.
//
  SPAPI_E_DUPLICATE_FOUND = HRESULT($800F0202);

//
// MessageId: SPAPI_E_NO_DRIVER_SELECTED
//
// MessageText:
//
//  There is no driver selected for the device information set or element.
//
  SPAPI_E_NO_DRIVER_SELECTED = HRESULT($800F0203);

//
// MessageId: SPAPI_E_KEY_DOES_NOT_EXIST
//
// MessageText:
//
//  The requested device registry key does not exist.
//
  SPAPI_E_KEY_DOES_NOT_EXIST = HRESULT($800F0204);

//
// MessageId: SPAPI_E_INVALID_DEVINST_NAME
//
// MessageText:
//
//  The device instance name is invalid.
//
  SPAPI_E_INVALID_DEVINST_NAME = HRESULT($800F0205);

//
// MessageId: SPAPI_E_INVALID_CLASS
//
// MessageText:
//
//  The install class is not present or is invalid.
//
  SPAPI_E_INVALID_CLASS = HRESULT($800F0206);

//
// MessageId: SPAPI_E_DEVINST_ALREADY_EXISTS
//
// MessageText:
//
//  The device instance cannot be created because it already exists.
//
  SPAPI_E_DEVINST_ALREADY_EXISTS = HRESULT($800F0207);

//
// MessageId: SPAPI_E_DEVINFO_NOT_REGISTERED
//
// MessageText:
//
//  The operation cannot be performed on a device information element that has not been registered.
//
  SPAPI_E_DEVINFO_NOT_REGISTERED = HRESULT($800F0208);

//
// MessageId: SPAPI_E_INVALID_REG_PROPERTY
//
// MessageText:
//
//  The device property code is invalid.
//
  SPAPI_E_INVALID_REG_PROPERTY = HRESULT($800F0209);

//
// MessageId: SPAPI_E_NO_INF
//
// MessageText:
//
//  The INF from which a driver list is to be built does not exist.
//
  SPAPI_E_NO_INF = HRESULT($800F020A);

//
// MessageId: SPAPI_E_NO_SUCH_DEVINST
//
// MessageText:
//
//  The device instance does not exist in the hardware tree.
//
  SPAPI_E_NO_SUCH_DEVINST = HRESULT($800F020B);

//
// MessageId: SPAPI_E_CANT_LOAD_CLASS_ICON
//
// MessageText:
//
//  The icon representing this install class cannot be loaded.
//
  SPAPI_E_CANT_LOAD_CLASS_ICON = HRESULT($800F020C);

//
// MessageId: SPAPI_E_INVALID_CLASS_INSTALLER
//
// MessageText:
//
//  The class installer registry entry is invalid.
//
  SPAPI_E_INVALID_CLASS_INSTALLER = HRESULT($800F020D);

//
// MessageId: SPAPI_E_DI_DO_DEFAULT
//
// MessageText:
//
//  The class installer has indicated that the default action should be performed for this installation request.
//
  SPAPI_E_DI_DO_DEFAULT = HRESULT($800F020E);

//
// MessageId: SPAPI_E_DI_NOFILECOPY
//
// MessageText:
//
//  The operation does not require any files to be copied.
//
  SPAPI_E_DI_NOFILECOPY = HRESULT($800F020F);

//
// MessageId: SPAPI_E_INVALID_HWPROFILE
//
// MessageText:
//
//  The specified hardware profile does not exist.
//
  SPAPI_E_INVALID_HWPROFILE = HRESULT($800F0210);

//
// MessageId: SPAPI_E_NO_DEVICE_SELECTED
//
// MessageText:
//
//  There is no device information element currently selected for this device information set.
//
  SPAPI_E_NO_DEVICE_SELECTED = HRESULT($800F0211);

//
// MessageId: SPAPI_E_DEVINFO_LIST_LOCKED
//
// MessageText:
//
//  The operation cannot be performed because the device information set is locked.
//
  SPAPI_E_DEVINFO_LIST_LOCKED = HRESULT($800F0212);

//
// MessageId: SPAPI_E_DEVINFO_DATA_LOCKED
//
// MessageText:
//
//  The operation cannot be performed because the device information element is locked.
//
  SPAPI_E_DEVINFO_DATA_LOCKED = HRESULT($800F0213);

//
// MessageId: SPAPI_E_DI_BAD_PATH
//
// MessageText:
//
//  The specified path does not contain any applicable device INFs.
//
  SPAPI_E_DI_BAD_PATH = HRESULT($800F0214);

//
// MessageId: SPAPI_E_NO_CLASSINSTALL_PARAMS
//
// MessageText:
//
//  No class installer parameters have been set for the device information set or element.
//
  SPAPI_E_NO_CLASSINSTALL_PARAMS = HRESULT($800F0215);

//
// MessageId: SPAPI_E_FILEQUEUE_LOCKED
//
// MessageText:
//
//  The operation cannot be performed because the file queue is locked.
//
  SPAPI_E_FILEQUEUE_LOCKED = HRESULT($800F0216);

//
// MessageId: SPAPI_E_BAD_SERVICE_INSTALLSECT
//
// MessageText:
//
//  A service installation section in this INF is invalid.
//
  SPAPI_E_BAD_SERVICE_INSTALLSECT = HRESULT($800F0217);

//
// MessageId: SPAPI_E_NO_CLASS_DRIVER_LIST
//
// MessageText:
//
//  There is no class driver list for the device information element.
//
  SPAPI_E_NO_CLASS_DRIVER_LIST = HRESULT($800F0218);

//
// MessageId: SPAPI_E_NO_ASSOCIATED_SERVICE
//
// MessageText:
//
//  The installation failed because a function driver was not specified for this device instance.
//
  SPAPI_E_NO_ASSOCIATED_SERVICE = HRESULT($800F0219);

//
// MessageId: SPAPI_E_NO_DEFAULT_DEVICE_INTERFACE
//
// MessageText:
//
//  There is presently no default device interface designated for this interface class.
//
  SPAPI_E_NO_DEFAULT_DEVICE_INTERFACE = HRESULT($800F021A);

//
// MessageId: SPAPI_E_DEVICE_INTERFACE_ACTIVE
//
// MessageText:
//
//  The operation cannot be performed because the device interface is currently active.
//
  SPAPI_E_DEVICE_INTERFACE_ACTIVE = HRESULT($800F021B);

//
// MessageId: SPAPI_E_DEVICE_INTERFACE_REMOVED
//
// MessageText:
//
//  The operation cannot be performed because the device interface has been removed from the system.
//
  SPAPI_E_DEVICE_INTERFACE_REMOVED = HRESULT($800F021C);

//
// MessageId: SPAPI_E_BAD_INTERFACE_INSTALLSECT
//
// MessageText:
//
//  An interface installation section in this INF is invalid.
//
  SPAPI_E_BAD_INTERFACE_INSTALLSECT = HRESULT($800F021D);

//
// MessageId: SPAPI_E_NO_SUCH_INTERFACE_CLASS
//
// MessageText:
//
//  This interface class does not exist in the system.
//
  SPAPI_E_NO_SUCH_INTERFACE_CLASS = HRESULT($800F021E);

//
// MessageId: SPAPI_E_INVALID_REFERENCE_STRING
//
// MessageText:
//
//  The reference string supplied for this interface device is invalid.
//
  SPAPI_E_INVALID_REFERENCE_STRING = HRESULT($800F021F);

//
// MessageId: SPAPI_E_INVALID_MACHINENAME
//
// MessageText:
//
//  The specified machine name does not conform to UNC naming conventions.
//
  SPAPI_E_INVALID_MACHINENAME = HRESULT($800F0220);

//
// MessageId: SPAPI_E_REMOTE_COMM_FAILURE
//
// MessageText:
//
//  A general remote communication error occurred.
//
  SPAPI_E_REMOTE_COMM_FAILURE = HRESULT($800F0221);

//
// MessageId: SPAPI_E_MACHINE_UNAVAILABLE
//
// MessageText:
//
//  The machine selected for remote communication is not available at this time.
//
  SPAPI_E_MACHINE_UNAVAILABLE = HRESULT($800F0222);

//
// MessageId: SPAPI_E_NO_CONFIGMGR_SERVICES
//
// MessageText:
//
//  The Plug and Play service is not available on the remote machine.
//
  SPAPI_E_NO_CONFIGMGR_SERVICES = HRESULT($800F0223);

//
// MessageId: SPAPI_E_INVALID_PROPPAGE_PROVIDER
//
// MessageText:
//
//  The property page provider registry entry is invalid.
//
  SPAPI_E_INVALID_PROPPAGE_PROVIDER = HRESULT($800F0224);

//
// MessageId: SPAPI_E_NO_SUCH_DEVICE_INTERFACE
//
// MessageText:
//
//  The requested device interface is not present in the system.
//
  SPAPI_E_NO_SUCH_DEVICE_INTERFACE = HRESULT($800F0225);

//
// MessageId: SPAPI_E_DI_POSTPROCESSING_REQUIRED
//
// MessageText:
//
//  The device's co-installer has additional work to perform after installation is complete.
//
  SPAPI_E_DI_POSTPROCESSING_REQUIRED = HRESULT($800F0226);

//
// MessageId: SPAPI_E_INVALID_COINSTALLER
//
// MessageText:
//
//  The device's co-installer is invalid.
//
  SPAPI_E_INVALID_COINSTALLER = HRESULT($800F0227);

//
// MessageId: SPAPI_E_NO_COMPAT_DRIVERS
//
// MessageText:
//
//  There are no compatible drivers for this device.
//
  SPAPI_E_NO_COMPAT_DRIVERS = HRESULT($800F0228);

//
// MessageId: SPAPI_E_NO_DEVICE_ICON
//
// MessageText:
//
//  There is no icon that represents this device or device type.
//
  SPAPI_E_NO_DEVICE_ICON = HRESULT($800F0229);

//
// MessageId: SPAPI_E_INVALID_INF_LOGCONFIG
//
// MessageText:
//
//  A logical configuration specified in this INF is invalid.
//
  SPAPI_E_INVALID_INF_LOGCONFIG = HRESULT($800F022A);

//
// MessageId: SPAPI_E_DI_DONT_INSTALL
//
// MessageText:
//
//  The class installer has denied the request to install or upgrade this device.
//
  SPAPI_E_DI_DONT_INSTALL = HRESULT($800F022B);

//
// MessageId: SPAPI_E_INVALID_FILTER_DRIVER
//
// MessageText:
//
//  One of the filter drivers installed for this device is invalid.
//
  SPAPI_E_INVALID_FILTER_DRIVER = HRESULT($800F022C);

//
// MessageId: SPAPI_E_NON_WINDOWS_NT_DRIVER
//
// MessageText:
//
//  The driver selected for this device does not support Windows XP.
//
  SPAPI_E_NON_WINDOWS_NT_DRIVER = HRESULT($800F022D);

//
// MessageId: SPAPI_E_NON_WINDOWS_DRIVER
//
// MessageText:
//
//  The driver selected for this device does not support Windows.
//
  SPAPI_E_NON_WINDOWS_DRIVER = HRESULT($800F022E);

//
// MessageId: SPAPI_E_NO_CATALOG_FOR_OEM_INF
//
// MessageText:
//
//  The third-party INF does not contain digital signature information.
//
  SPAPI_E_NO_CATALOG_FOR_OEM_INF = HRESULT($800F022F);

//
// MessageId: SPAPI_E_DEVINSTALL_QUEUE_NONNATIVE
//
// MessageText:
//
//  An invalid attempt was made to use a device installation file queue for verification of digital signatures relative to other platforms.
//
  SPAPI_E_DEVINSTALL_QUEUE_NONNATIVE = HRESULT($800F0230);

//
// MessageId: SPAPI_E_NOT_DISABLEABLE
//
// MessageText:
//
//  The device cannot be disabled.
//
  SPAPI_E_NOT_DISABLEABLE = HRESULT($800F0231);

//
// MessageId: SPAPI_E_CANT_REMOVE_DEVINST
//
// MessageText:
//
//  The device could not be dynamically removed.
//
  SPAPI_E_CANT_REMOVE_DEVINST = HRESULT($800F0232);

//
// MessageId: SPAPI_E_INVALID_TARGET
//
// MessageText:
//
//  Cannot copy to specified target.
//
  SPAPI_E_INVALID_TARGET = HRESULT($800F0233);

//
// MessageId: SPAPI_E_DRIVER_NONNATIVE
//
// MessageText:
//
//  Driver is not intended for this platform.
//
  SPAPI_E_DRIVER_NONNATIVE = HRESULT($800F0234);

//
// MessageId: SPAPI_E_IN_WOW64
//
// MessageText:
//
//  Operation not allowed in WOW64.
//
  SPAPI_E_IN_WOW64 = HRESULT($800F0235);

//
// MessageId: SPAPI_E_SET_SYSTEM_RESTORE_POINT
//
// MessageText:
//
//  The operation involving unsigned file copying was rolled back, so that a system restore point could be set.
//
  SPAPI_E_SET_SYSTEM_RESTORE_POINT = HRESULT($800F0236);

//
// MessageId: SPAPI_E_INCORRECTLY_COPIED_INF
//
// MessageText:
//
//  An INF was copied into the Windows INF directory in an improper manner.
//
  SPAPI_E_INCORRECTLY_COPIED_INF = HRESULT($800F0237);

//
// MessageId: SPAPI_E_SCE_DISABLED
//
// MessageText:
//
//  The Security Configuration Editor (SCE) APIs have been disabled on this Embedded product.
//
  SPAPI_E_SCE_DISABLED = HRESULT($800F0238);

//
// MessageId: SPAPI_E_UNKNOWN_EXCEPTION
//
// MessageText:
//
//  An unknown exception was encountered.
//
  SPAPI_E_UNKNOWN_EXCEPTION = HRESULT($800F0239);

//
// MessageId: SPAPI_E_PNP_REGISTRY_ERROR
//
// MessageText:
//
//  A problem was encountered when accessing the Plug and Play registry database.
//
  SPAPI_E_PNP_REGISTRY_ERROR = HRESULT($800F023A);

//
// MessageId: SPAPI_E_REMOTE_REQUEST_UNSUPPORTED
//
// MessageText:
//
//  The requested operation is not supported for a remote machine.
//
  SPAPI_E_REMOTE_REQUEST_UNSUPPORTED = HRESULT($800F023B);

//
// MessageId: SPAPI_E_NOT_AN_INSTALLED_OEM_INF
//
// MessageText:
//
//  The specified file is not an installed OEM INF.
//
  SPAPI_E_NOT_AN_INSTALLED_OEM_INF = HRESULT($800F023C);

//
// MessageId: SPAPI_E_INF_IN_USE_BY_DEVICES
//
// MessageText:
//
//  One or more devices are presently installed using the specified INF.
//
  SPAPI_E_INF_IN_USE_BY_DEVICES = HRESULT($800F023D);

//
// MessageId: SPAPI_E_DI_FUNCTION_OBSOLETE
//
// MessageText:
//
//  The requested device install operation is obsolete.
//
  SPAPI_E_DI_FUNCTION_OBSOLETE = HRESULT($800F023E);

//
// MessageId: SPAPI_E_NO_AUTHENTICODE_CATALOG
//
// MessageText:
//
//  A file could not be verified because it does not have an associated catalog signed via Authenticode(tm).
//
  SPAPI_E_NO_AUTHENTICODE_CATALOG = HRESULT($800F023F);

//
// MessageId: SPAPI_E_AUTHENTICODE_DISALLOWED
//
// MessageText:
//
//  Authenticode(tm) signature verification is not supported for the specified INF.
//
  SPAPI_E_AUTHENTICODE_DISALLOWED = HRESULT($800F0240);

//
// MessageId: SPAPI_E_AUTHENTICODE_TRUSTED_PUBLISHER
//
// MessageText:
//
//  The INF was signed with an Authenticode(tm) catalog from a trusted publisher.
//
  SPAPI_E_AUTHENTICODE_TRUSTED_PUBLISHER = HRESULT($800F0241);

//
// MessageId: SPAPI_E_AUTHENTICODE_TRUST_NOT_ESTABLISHED
//
// MessageText:
//
//  The publisher of an Authenticode(tm) signed catalog has not yet been established as trusted.
//
  SPAPI_E_AUTHENTICODE_TRUST_NOT_ESTABLISHED = HRESULT($800F0242);

//
// MessageId: SPAPI_E_AUTHENTICODE_PUBLISHER_NOT_TRUSTED
//
// MessageText:
//
//  The publisher of an Authenticode(tm) signed catalog was not established as trusted.
//
  SPAPI_E_AUTHENTICODE_PUBLISHER_NOT_TRUSTED = HRESULT($800F0243);

//
// MessageId: SPAPI_E_SIGNATURE_OSATTRIBUTE_MISMATCH
//
// MessageText:
//
//  The software was tested for compliance with Windows Logo requirements on a different version of Windows, and may not be compatible with this version.
//
  SPAPI_E_SIGNATURE_OSATTRIBUTE_MISMATCH = HRESULT($800F0244);

//
// MessageId: SPAPI_E_ONLY_VALIDATE_VIA_AUTHENTICODE
//
// MessageText:
//
//  The file may only be validated by a catalog signed via Authenticode(tm).
//
  SPAPI_E_ONLY_VALIDATE_VIA_AUTHENTICODE = HRESULT($800F0245);

//
// MessageId: SPAPI_E_UNRECOVERABLE_STACK_OVERFLOW
//
// MessageText:
//
//  An unrecoverable stack overflow was encountered.
//
  SPAPI_E_UNRECOVERABLE_STACK_OVERFLOW = HRESULT($800F0300);

//
// MessageId: SPAPI_E_ERROR_NOT_INSTALLED
//
// MessageText:
//
//  No installed components were detected.
//
  SPAPI_E_ERROR_NOT_INSTALLED = HRESULT($800F1000);

// *****************
// FACILITY_SCARD
// *****************
//
// =============================
// Facility SCARD Error Messages
// =============================
//
  SCARD_S_SUCCESS = NO_ERROR;
//
// MessageId: SCARD_F_INTERNAL_ERROR
//
// MessageText:
//
//  An internal consistency check failed.
//
  SCARD_F_INTERNAL_ERROR = HRESULT($80100001);

//
// MessageId: SCARD_E_CANCELLED
//
// MessageText:
//
//  The action was cancelled by an SCardCancel request.
//
  SCARD_E_CANCELLED = HRESULT($80100002);

//
// MessageId: SCARD_E_INVALID_HANDLE
//
// MessageText:
//
//  The supplied handle was invalid.
//
  SCARD_E_INVALID_HANDLE = HRESULT($80100003);

//
// MessageId: SCARD_E_INVALID_PARAMETER
//
// MessageText:
//
//  One or more of the supplied parameters could not be properly interpreted.
//
  SCARD_E_INVALID_PARAMETER = HRESULT($80100004);

//
// MessageId: SCARD_E_INVALID_TARGET
//
// MessageText:
//
//  Registry startup information is missing or invalid.
//
  SCARD_E_INVALID_TARGET = HRESULT($80100005);

//
// MessageId: SCARD_E_NO_MEMORY
//
// MessageText:
//
//  Not enough memory available to complete this command.
//
  SCARD_E_NO_MEMORY = HRESULT($80100006);

//
// MessageId: SCARD_F_WAITED_TOO_LONG
//
// MessageText:
//
//  An internal consistency timer has expired.
//
  SCARD_F_WAITED_TOO_LONG = HRESULT($80100007);

//
// MessageId: SCARD_E_INSUFFICIENT_BUFFER
//
// MessageText:
//
//  The data buffer to receive returned data is too small for the returned data.
//
  SCARD_E_INSUFFICIENT_BUFFER = HRESULT($80100008);

//
// MessageId: SCARD_E_UNKNOWN_READER
//
// MessageText:
//
//  The specified reader name is not recognized.
//
  SCARD_E_UNKNOWN_READER = HRESULT($80100009);

//
// MessageId: SCARD_E_TIMEOUT
//
// MessageText:
//
//  The user-specified timeout value has expired.
//
  SCARD_E_TIMEOUT = HRESULT($8010000A);

//
// MessageId: SCARD_E_SHARING_VIOLATION
//
// MessageText:
//
//  The smart card cannot be accessed because of other connections outstanding.
//
  SCARD_E_SHARING_VIOLATION = HRESULT($8010000B);

//
// MessageId: SCARD_E_NO_SMARTCARD
//
// MessageText:
//
//  The operation requires a Smart Card, but no Smart Card is currently in the device.
//
  SCARD_E_NO_SMARTCARD = HRESULT($8010000C);

//
// MessageId: SCARD_E_UNKNOWN_CARD
//
// MessageText:
//
//  The specified smart card name is not recognized.
//
  SCARD_E_UNKNOWN_CARD = HRESULT($8010000D);

//
// MessageId: SCARD_E_CANT_DISPOSE
//
// MessageText:
//
//  The system could not dispose of the media in the requested manner.
//
  SCARD_E_CANT_DISPOSE = HRESULT($8010000E);

//
// MessageId: SCARD_E_PROTO_MISMATCH
//
// MessageText:
//
//  The requested protocols are incompatible with the protocol currently in use with the smart card.
//
  SCARD_E_PROTO_MISMATCH = HRESULT($8010000F);

//
// MessageId: SCARD_E_NOT_READY
//
// MessageText:
//
//  The reader or smart card is not ready to accept commands.
//
  SCARD_E_NOT_READY = HRESULT($80100010);

//
// MessageId: SCARD_E_INVALID_VALUE
//
// MessageText:
//
//  One or more of the supplied parameters values could not be properly interpreted.
//
  SCARD_E_INVALID_VALUE = HRESULT($80100011);

//
// MessageId: SCARD_E_SYSTEM_CANCELLED
//
// MessageText:
//
//  The action was cancelled by the system, presumably to log off or shut down.
//
  SCARD_E_SYSTEM_CANCELLED = HRESULT($80100012);

//
// MessageId: SCARD_F_COMM_ERROR
//
// MessageText:
//
//  An internal communications error has been detected.
//
  SCARD_F_COMM_ERROR = HRESULT($80100013);

//
// MessageId: SCARD_F_UNKNOWN_ERROR
//
// MessageText:
//
//  An internal error has been detected, but the source is unknown.
//
  SCARD_F_UNKNOWN_ERROR = HRESULT($80100014);

//
// MessageId: SCARD_E_INVALID_ATR
//
// MessageText:
//
//  An ATR obtained from the registry is not a valid ATR string.
//
  SCARD_E_INVALID_ATR = HRESULT($80100015);

//
// MessageId: SCARD_E_NOT_TRANSACTED
//
// MessageText:
//
//  An attempt was made to end a non-existent transaction.
//
  SCARD_E_NOT_TRANSACTED = HRESULT($80100016);

//
// MessageId: SCARD_E_READER_UNAVAILABLE
//
// MessageText:
//
//  The specified reader is not currently available for use.
//
  SCARD_E_READER_UNAVAILABLE = HRESULT($80100017);

//
// MessageId: SCARD_P_SHUTDOWN
//
// MessageText:
//
//  The operation has been aborted to allow the server application to exit.
//
  SCARD_P_SHUTDOWN = HRESULT($80100018);

//
// MessageId: SCARD_E_PCI_TOO_SMALL
//
// MessageText:
//
//  The PCI Receive buffer was too small.
//
  SCARD_E_PCI_TOO_SMALL = HRESULT($80100019);

//
// MessageId: SCARD_E_READER_UNSUPPORTED
//
// MessageText:
//
//  The reader driver does not meet minimal requirements for support.
//
  SCARD_E_READER_UNSUPPORTED = HRESULT($8010001A);

//
// MessageId: SCARD_E_DUPLICATE_READER
//
// MessageText:
//
//  The reader driver did not produce a unique reader name.
//
  SCARD_E_DUPLICATE_READER = HRESULT($8010001B);

//
// MessageId: SCARD_E_CARD_UNSUPPORTED
//
// MessageText:
//
//  The smart card does not meet minimal requirements for support.
//
  SCARD_E_CARD_UNSUPPORTED = HRESULT($8010001C);

//
// MessageId: SCARD_E_NO_SERVICE
//
// MessageText:
//
//  The Smart card resource manager is not running.
//
  SCARD_E_NO_SERVICE = HRESULT($8010001D);

//
// MessageId: SCARD_E_SERVICE_STOPPED
//
// MessageText:
//
//  The Smart card resource manager has shut down.
//
  SCARD_E_SERVICE_STOPPED = HRESULT($8010001E);

//
// MessageId: SCARD_E_UNEXPECTED
//
// MessageText:
//
//  An unexpected card error has occurred.
//
  SCARD_E_UNEXPECTED = HRESULT($8010001F);

//
// MessageId: SCARD_E_ICC_INSTALLATION
//
// MessageText:
//
//  No Primary Provider can be found for the smart card.
//
  SCARD_E_ICC_INSTALLATION = HRESULT($80100020);

//
// MessageId: SCARD_E_ICC_CREATEORDER
//
// MessageText:
//
//  The requested order of object creation is not supported.
//
  SCARD_E_ICC_CREATEORDER = HRESULT($80100021);

//
// MessageId: SCARD_E_UNSUPPORTED_FEATURE
//
// MessageText:
//
//  This smart card does not support the requested feature.
//
  SCARD_E_UNSUPPORTED_FEATURE = HRESULT($80100022);

//
// MessageId: SCARD_E_DIR_NOT_FOUND
//
// MessageText:
//
//  The identified directory does not exist in the smart card.
//
  SCARD_E_DIR_NOT_FOUND = HRESULT($80100023);

//
// MessageId: SCARD_E_FILE_NOT_FOUND
//
// MessageText:
//
//  The identified file does not exist in the smart card.
//
  SCARD_E_FILE_NOT_FOUND = HRESULT($80100024);

//
// MessageId: SCARD_E_NO_DIR
//
// MessageText:
//
//  The supplied path does not represent a smart card directory.
//
  SCARD_E_NO_DIR = HRESULT($80100025);

//
// MessageId: SCARD_E_NO_FILE
//
// MessageText:
//
//  The supplied path does not represent a smart card file.
//
  SCARD_E_NO_FILE = HRESULT($80100026);

//
// MessageId: SCARD_E_NO_ACCESS
//
// MessageText:
//
//  Access is denied to this file.
//
  SCARD_E_NO_ACCESS = HRESULT($80100027);

//
// MessageId: SCARD_E_WRITE_TOO_MANY
//
// MessageText:
//
//  The smartcard does not have enough memory to store the information.
//
  SCARD_E_WRITE_TOO_MANY = HRESULT($80100028);

//
// MessageId: SCARD_E_BAD_SEEK
//
// MessageText:
//
//  There was an error trying to set the smart card file object pointer.
//
  SCARD_E_BAD_SEEK = HRESULT($80100029);

//
// MessageId: SCARD_E_INVALID_CHV
//
// MessageText:
//
//  The supplied PIN is incorrect.
//
  SCARD_E_INVALID_CHV = HRESULT($8010002A);

//
// MessageId: SCARD_E_UNKNOWN_RES_MNG
//
// MessageText:
//
//  An unrecognized error code was returned from a layered component.
//
  SCARD_E_UNKNOWN_RES_MNG = HRESULT($8010002B);

//
// MessageId: SCARD_E_NO_SUCH_CERTIFICATE
//
// MessageText:
//
//  The requested certificate does not exist.
//
  SCARD_E_NO_SUCH_CERTIFICATE = HRESULT($8010002C);

//
// MessageId: SCARD_E_CERTIFICATE_UNAVAILABLE
//
// MessageText:
//
//  The requested certificate could not be obtained.
//
  SCARD_E_CERTIFICATE_UNAVAILABLE = HRESULT($8010002D);

//
// MessageId: SCARD_E_NO_READERS_AVAILABLE
//
// MessageText:
//
//  Cannot find a smart card reader.
//
  SCARD_E_NO_READERS_AVAILABLE = HRESULT($8010002E);

//
// MessageId: SCARD_E_COMM_DATA_LOST
//
// MessageText:
//
//  A communications error with the smart card has been detected.  Retry the operation.
//
  SCARD_E_COMM_DATA_LOST = HRESULT($8010002F);

//
// MessageId: SCARD_E_NO_KEY_CONTAINER
//
// MessageText:
//
//  The requested key container does not exist on the smart card.
//
  SCARD_E_NO_KEY_CONTAINER = HRESULT($80100030);

//
// MessageId: SCARD_E_SERVER_TOO_BUSY
//
// MessageText:
//
//  The Smart card resource manager is too busy to complete this operation.
//
  SCARD_E_SERVER_TOO_BUSY = HRESULT($80100031);

//
// These are warning codes.
//
//
// MessageId: SCARD_W_UNSUPPORTED_CARD
//
// MessageText:
//
//  The reader cannot communicate with the smart card, due to ATR configuration conflicts.
//
  SCARD_W_UNSUPPORTED_CARD = HRESULT($80100065);

//
// MessageId: SCARD_W_UNRESPONSIVE_CARD
//
// MessageText:
//
//  The smart card is not responding to a reset.
//
  SCARD_W_UNRESPONSIVE_CARD = HRESULT($80100066);

//
// MessageId: SCARD_W_UNPOWERED_CARD
//
// MessageText:
//
//  Power has been removed from the smart card, so that further communication is not possible.
//
  SCARD_W_UNPOWERED_CARD = HRESULT($80100067);

//
// MessageId: SCARD_W_RESET_CARD
//
// MessageText:
//
//  The smart card has been reset, so any shared state information is invalid.
//
  SCARD_W_RESET_CARD = HRESULT($80100068);

//
// MessageId: SCARD_W_REMOVED_CARD
//
// MessageText:
//
//  The smart card has been removed, so that further communication is not possible.
//
  SCARD_W_REMOVED_CARD = HRESULT($80100069);

//
// MessageId: SCARD_W_SECURITY_VIOLATION
//
// MessageText:
//
//  Access was denied because of a security violation.
//
  SCARD_W_SECURITY_VIOLATION = HRESULT($8010006A);

//
// MessageId: SCARD_W_WRONG_CHV
//
// MessageText:
//
//  The card cannot be accessed because the wrong PIN was presented.
//
  SCARD_W_WRONG_CHV = HRESULT($8010006B);

//
// MessageId: SCARD_W_CHV_BLOCKED
//
// MessageText:
//
//  The card cannot be accessed because the maximum number of PIN entry attempts has been reached.
//
  SCARD_W_CHV_BLOCKED = HRESULT($8010006C);

//
// MessageId: SCARD_W_EOF
//
// MessageText:
//
//  The end of the smart card file has been reached.
//
  SCARD_W_EOF = HRESULT($8010006D);

//
// MessageId: SCARD_W_CANCELLED_BY_USER
//
// MessageText:
//
//  The action was cancelled by the user.
//
  SCARD_W_CANCELLED_BY_USER = HRESULT($8010006E);

//
// MessageId: SCARD_W_CARD_NOT_AUTHENTICATED
//
// MessageText:
//
//  No PIN was presented to the smart card.
//
  SCARD_W_CARD_NOT_AUTHENTICATED = HRESULT($8010006F);

// *****************
// FACILITY_COMPLUS
// *****************
//
// ===============================
// Facility COMPLUS Error Messages
// ===============================
//
//
// The following are the subranges  within the COMPLUS facility
// 0x400 - 0x4ff               COMADMIN_E_CAT
// 0x600 - 0x6ff               COMQC errors
// 0x700 - 0x7ff               MSDTC errors
// 0x800 - 0x8ff               Other COMADMIN errors
//
// COMPLUS Admin errors
//
//
// MessageId: COMADMIN_E_OBJECTERRORS
//
// MessageText:
//
//  Errors occurred accessing one or more objects - the ErrorInfo collection may have more detail
//
  COMADMIN_E_OBJECTERRORS = HRESULT($80110401);

//
// MessageId: COMADMIN_E_OBJECTINVALID
//
// MessageText:
//
//  One or more of the object's properties are missing or invalid
//
  COMADMIN_E_OBJECTINVALID = HRESULT($80110402);

//
// MessageId: COMADMIN_E_KEYMISSING
//
// MessageText:
//
//  The object was not found in the catalog
//
  COMADMIN_E_KEYMISSING = HRESULT($80110403);

//
// MessageId: COMADMIN_E_ALREADYINSTALLED
//
// MessageText:
//
//  The object is already registered
//
  COMADMIN_E_ALREADYINSTALLED = HRESULT($80110404);

//
// MessageId: COMADMIN_E_APP_FILE_WRITEFAIL
//
// MessageText:
//
//  Error occurred writing to the application file
//
  COMADMIN_E_APP_FILE_WRITEFAIL = HRESULT($80110407);

//
// MessageId: COMADMIN_E_APP_FILE_READFAIL
//
// MessageText:
//
//  Error occurred reading the application file
//
  COMADMIN_E_APP_FILE_READFAIL = HRESULT($80110408);

//
// MessageId: COMADMIN_E_APP_FILE_VERSION
//
// MessageText:
//
//  Invalid version number in application file
//
  COMADMIN_E_APP_FILE_VERSION = HRESULT($80110409);

//
// MessageId: COMADMIN_E_BADPATH
//
// MessageText:
//
//  The file path is invalid
//
  COMADMIN_E_BADPATH = HRESULT($8011040A);

//
// MessageId: COMADMIN_E_APPLICATIONEXISTS
//
// MessageText:
//
//  The application is already installed
//
  COMADMIN_E_APPLICATIONEXISTS = HRESULT($8011040B);

//
// MessageId: COMADMIN_E_ROLEEXISTS
//
// MessageText:
//
//  The role already exists
//
  COMADMIN_E_ROLEEXISTS = HRESULT($8011040C);

//
// MessageId: COMADMIN_E_CANTCOPYFILE
//
// MessageText:
//
//  An error occurred copying the file
//
  COMADMIN_E_CANTCOPYFILE = HRESULT($8011040D);

//
// MessageId: COMADMIN_E_NOUSER
//
// MessageText:
//
//  One or more users are not valid
//
  COMADMIN_E_NOUSER = HRESULT($8011040F);

//
// MessageId: COMADMIN_E_INVALIDUSERIDS
//
// MessageText:
//
//  One or more users in the application file are not valid
//
  COMADMIN_E_INVALIDUSERIDS = HRESULT($80110410);

//
// MessageId: COMADMIN_E_NOREGISTRYCLSID
//
// MessageText:
//
//  The component's CLSID is missing or corrupt
//
  COMADMIN_E_NOREGISTRYCLSID = HRESULT($80110411);

//
// MessageId: COMADMIN_E_BADREGISTRYPROGID
//
// MessageText:
//
//  The component's progID is missing or corrupt
//
  COMADMIN_E_BADREGISTRYPROGID = HRESULT($80110412);

//
// MessageId: COMADMIN_E_AUTHENTICATIONLEVEL
//
// MessageText:
//
//  Unable to set required authentication level for update request
//
  COMADMIN_E_AUTHENTICATIONLEVEL = HRESULT($80110413);

//
// MessageId: COMADMIN_E_USERPASSWDNOTVALID
//
// MessageText:
//
//  The identity or password set on the application is not valid
//
  COMADMIN_E_USERPASSWDNOTVALID = HRESULT($80110414);

//
// MessageId: COMADMIN_E_CLSIDORIIDMISMATCH
//
// MessageText:
//
//  Application file CLSIDs or IIDs do not match corresponding DLLs
//
  COMADMIN_E_CLSIDORIIDMISMATCH = HRESULT($80110418);

//
// MessageId: COMADMIN_E_REMOTEINTERFACE
//
// MessageText:
//
//  Interface information is either missing or changed
//
  COMADMIN_E_REMOTEINTERFACE = HRESULT($80110419);

//
// MessageId: COMADMIN_E_DLLREGISTERSERVER
//
// MessageText:
//
//  DllRegisterServer failed on component install
//
  COMADMIN_E_DLLREGISTERSERVER = HRESULT($8011041A);

//
// MessageId: COMADMIN_E_NOSERVERSHARE
//
// MessageText:
//
//  No server file share available
//
  COMADMIN_E_NOSERVERSHARE = HRESULT($8011041B);

//
// MessageId: COMADMIN_E_DLLLOADFAILED
//
// MessageText:
//
//  DLL could not be loaded
//
  COMADMIN_E_DLLLOADFAILED = HRESULT($8011041D);

//
// MessageId: COMADMIN_E_BADREGISTRYLIBID
//
// MessageText:
//
//  The registered TypeLib ID is not valid
//
  COMADMIN_E_BADREGISTRYLIBID = HRESULT($8011041E);

//
// MessageId: COMADMIN_E_APPDIRNOTFOUND
//
// MessageText:
//
//  Application install directory not found
//
  COMADMIN_E_APPDIRNOTFOUND = HRESULT($8011041F);

//
// MessageId: COMADMIN_E_REGISTRARFAILED
//
// MessageText:
//
//  Errors occurred while in the component registrar
//
  COMADMIN_E_REGISTRARFAILED = HRESULT($80110423);

//
// MessageId: COMADMIN_E_COMPFILE_DOESNOTEXIST
//
// MessageText:
//
//  The file does not exist
//
  COMADMIN_E_COMPFILE_DOESNOTEXIST = HRESULT($80110424);

//
// MessageId: COMADMIN_E_COMPFILE_LOADDLLFAIL
//
// MessageText:
//
//  The DLL could not be loaded
//
  COMADMIN_E_COMPFILE_LOADDLLFAIL = HRESULT($80110425);

//
// MessageId: COMADMIN_E_COMPFILE_GETCLASSOBJ
//
// MessageText:
//
//  GetClassObject failed in the DLL
//
  COMADMIN_E_COMPFILE_GETCLASSOBJ = HRESULT($80110426);

//
// MessageId: COMADMIN_E_COMPFILE_CLASSNOTAVAIL
//
// MessageText:
//
//  The DLL does not support the components listed in the TypeLib
//
  COMADMIN_E_COMPFILE_CLASSNOTAVAIL = HRESULT($80110427);

//
// MessageId: COMADMIN_E_COMPFILE_BADTLB
//
// MessageText:
//
//  The TypeLib could not be loaded
//
  COMADMIN_E_COMPFILE_BADTLB = HRESULT($80110428);

//
// MessageId: COMADMIN_E_COMPFILE_NOTINSTALLABLE
//
// MessageText:
//
//  The file does not contain components or component information
//
  COMADMIN_E_COMPFILE_NOTINSTALLABLE = HRESULT($80110429);

//
// MessageId: COMADMIN_E_NOTCHANGEABLE
//
// MessageText:
//
//  Changes to this object and its sub-objects have been disabled
//
  COMADMIN_E_NOTCHANGEABLE = HRESULT($8011042A);

//
// MessageId: COMADMIN_E_NOTDELETEABLE
//
// MessageText:
//
//  The delete function has been disabled for this object
//
  COMADMIN_E_NOTDELETEABLE = HRESULT($8011042B);

//
// MessageId: COMADMIN_E_SESSION
//
// MessageText:
//
//  The server catalog version is not supported
//
  COMADMIN_E_SESSION = HRESULT($8011042C);

//
// MessageId: COMADMIN_E_COMP_MOVE_LOCKED
//
// MessageText:
//
//  The component move was disallowed, because the source or destination application is either a system application or currently locked against changes
//
  COMADMIN_E_COMP_MOVE_LOCKED = HRESULT($8011042D);

//
// MessageId: COMADMIN_E_COMP_MOVE_BAD_DEST
//
// MessageText:
//
//  The component move failed because the destination application no longer exists
//
  COMADMIN_E_COMP_MOVE_BAD_DEST = HRESULT($8011042E);

//
// MessageId: COMADMIN_E_REGISTERTLB
//
// MessageText:
//
//  The system was unable to register the TypeLib
//
  COMADMIN_E_REGISTERTLB = HRESULT($80110430);

//
// MessageId: COMADMIN_E_SYSTEMAPP
//
// MessageText:
//
//  This operation can not be performed on the system application
//
  COMADMIN_E_SYSTEMAPP = HRESULT($80110433);

//
// MessageId: COMADMIN_E_COMPFILE_NOREGISTRAR
//
// MessageText:
//
//  The component registrar referenced in this file is not available
//
  COMADMIN_E_COMPFILE_NOREGISTRAR = HRESULT($80110434);

//
// MessageId: COMADMIN_E_COREQCOMPINSTALLED
//
// MessageText:
//
//  A component in the same DLL is already installed
//
  COMADMIN_E_COREQCOMPINSTALLED = HRESULT($80110435);

//
// MessageId: COMADMIN_E_SERVICENOTINSTALLED
//
// MessageText:
//
//  The service is not installed
//
  COMADMIN_E_SERVICENOTINSTALLED = HRESULT($80110436);

//
// MessageId: COMADMIN_E_PROPERTYSAVEFAILED
//
// MessageText:
//
//  One or more property settings are either invalid or in conflict with each other
//
  COMADMIN_E_PROPERTYSAVEFAILED = HRESULT($80110437);

//
// MessageId: COMADMIN_E_OBJECTEXISTS
//
// MessageText:
//
//  The object you are attempting to add or rename already exists
//
  COMADMIN_E_OBJECTEXISTS = HRESULT($80110438);

//
// MessageId: COMADMIN_E_COMPONENTEXISTS
//
// MessageText:
//
//  The component already exists
//
  COMADMIN_E_COMPONENTEXISTS = HRESULT($80110439);

//
// MessageId: COMADMIN_E_REGFILE_CORRUPT
//
// MessageText:
//
//  The registration file is corrupt
//
  COMADMIN_E_REGFILE_CORRUPT = HRESULT($8011043B);

//
// MessageId: COMADMIN_E_PROPERTY_OVERFLOW
//
// MessageText:
//
//  The property value is too large
//
  COMADMIN_E_PROPERTY_OVERFLOW = HRESULT($8011043C);

//
// MessageId: COMADMIN_E_NOTINREGISTRY
//
// MessageText:
//
//  Object was not found in registry
//
  COMADMIN_E_NOTINREGISTRY = HRESULT($8011043E);

//
// MessageId: COMADMIN_E_OBJECTNOTPOOLABLE
//
// MessageText:
//
//  This object is not poolable
//
  COMADMIN_E_OBJECTNOTPOOLABLE = HRESULT($8011043F);

//
// MessageId: COMADMIN_E_APPLID_MATCHES_CLSID
//
// MessageText:
//
//  A CLSID with the same GUID as the new application ID is already installed on this machine
//
  COMADMIN_E_APPLID_MATCHES_CLSID = HRESULT($80110446);

//
// MessageId: COMADMIN_E_ROLE_DOES_NOT_EXIST
//
// MessageText:
//
//  A role assigned to a component, interface, or method did not exist in the application
//
  COMADMIN_E_ROLE_DOES_NOT_EXIST = HRESULT($80110447);

//
// MessageId: COMADMIN_E_START_APP_NEEDS_COMPONENTS
//
// MessageText:
//
//  You must have components in an application in order to start the application
//
  COMADMIN_E_START_APP_NEEDS_COMPONENTS = HRESULT($80110448);

//
// MessageId: COMADMIN_E_REQUIRES_DIFFERENT_PLATFORM
//
// MessageText:
//
//  This operation is not enabled on this platform
//
  COMADMIN_E_REQUIRES_DIFFERENT_PLATFORM = HRESULT($80110449);

//
// MessageId: COMADMIN_E_CAN_NOT_EXPORT_APP_PROXY
//
// MessageText:
//
//  Application Proxy is not exportable
//
  COMADMIN_E_CAN_NOT_EXPORT_APP_PROXY = HRESULT($8011044A);

//
// MessageId: COMADMIN_E_CAN_NOT_START_APP
//
// MessageText:
//
//  Failed to start application because it is either a library application or an application proxy
//
  COMADMIN_E_CAN_NOT_START_APP = HRESULT($8011044B);

//
// MessageId: COMADMIN_E_CAN_NOT_EXPORT_SYS_APP
//
// MessageText:
//
//  System application is not exportable
//
  COMADMIN_E_CAN_NOT_EXPORT_SYS_APP = HRESULT($8011044C);

//
// MessageId: COMADMIN_E_CANT_SUBSCRIBE_TO_COMPONENT
//
// MessageText:
//
//  Can not subscribe to this component (the component may have been imported)
//
  COMADMIN_E_CANT_SUBSCRIBE_TO_COMPONENT = HRESULT($8011044D);

//
// MessageId: COMADMIN_E_EVENTCLASS_CANT_BE_SUBSCRIBER
//
// MessageText:
//
//  An event class cannot also be a subscriber component
//
  COMADMIN_E_EVENTCLASS_CANT_BE_SUBSCRIBER = HRESULT($8011044E);

//
// MessageId: COMADMIN_E_LIB_APP_PROXY_INCOMPATIBLE
//
// MessageText:
//
//  Library applications and application proxies are incompatible
//
  COMADMIN_E_LIB_APP_PROXY_INCOMPATIBLE = HRESULT($8011044F);

//
// MessageId: COMADMIN_E_BASE_PARTITION_ONLY
//
// MessageText:
//
//  This function is valid for the base partition only
//
  COMADMIN_E_BASE_PARTITION_ONLY = HRESULT($80110450);

//
// MessageId: COMADMIN_E_START_APP_DISABLED
//
// MessageText:
//
//  You cannot start an application that has been disabled
//
  COMADMIN_E_START_APP_DISABLED = HRESULT($80110451);

//
// MessageId: COMADMIN_E_CAT_DUPLICATE_PARTITION_NAME
//
// MessageText:
//
//  The specified partition name is already in use on this computer
//
  COMADMIN_E_CAT_DUPLICATE_PARTITION_NAME = HRESULT($80110457);

//
// MessageId: COMADMIN_E_CAT_INVALID_PARTITION_NAME
//
// MessageText:
//
//  The specified partition name is invalid. Check that the name contains at least one visible character
//
  COMADMIN_E_CAT_INVALID_PARTITION_NAME = HRESULT($80110458);

//
// MessageId: COMADMIN_E_CAT_PARTITION_IN_USE
//
// MessageText:
//
//  The partition cannot be deleted because it is the default partition for one or more users
//
  COMADMIN_E_CAT_PARTITION_IN_USE = HRESULT($80110459);

//
// MessageId: COMADMIN_E_FILE_PARTITION_DUPLICATE_FILES
//
// MessageText:
//
//  The partition cannot be exported, because one or more components in the partition have the same file name
//
  COMADMIN_E_FILE_PARTITION_DUPLICATE_FILES = HRESULT($8011045A);

//
// MessageId: COMADMIN_E_CAT_IMPORTED_COMPONENTS_NOT_ALLOWED
//
// MessageText:
//
//  Applications that contain one or more imported components cannot be installed into a non-base partition
//
  COMADMIN_E_CAT_IMPORTED_COMPONENTS_NOT_ALLOWED = HRESULT($8011045B);

//
// MessageId: COMADMIN_E_AMBIGUOUS_APPLICATION_NAME
//
// MessageText:
//
//  The application name is not unique and cannot be resolved to an application id
//
  COMADMIN_E_AMBIGUOUS_APPLICATION_NAME = HRESULT($8011045C);

//
// MessageId: COMADMIN_E_AMBIGUOUS_PARTITION_NAME
//
// MessageText:
//
//  The partition name is not unique and cannot be resolved to a partition id
//
  COMADMIN_E_AMBIGUOUS_PARTITION_NAME = HRESULT($8011045D);

//
// MessageId: COMADMIN_E_REGDB_NOTINITIALIZED
//
// MessageText:
//
//  The COM+ registry database has not been initialized
//
  COMADMIN_E_REGDB_NOTINITIALIZED = HRESULT($80110472);

//
// MessageId: COMADMIN_E_REGDB_NOTOPEN
//
// MessageText:
//
//  The COM+ registry database is not open
//
  COMADMIN_E_REGDB_NOTOPEN = HRESULT($80110473);

//
// MessageId: COMADMIN_E_REGDB_SYSTEMERR
//
// MessageText:
//
//  The COM+ registry database detected a system error
//
  COMADMIN_E_REGDB_SYSTEMERR = HRESULT($80110474);

//
// MessageId: COMADMIN_E_REGDB_ALREADYRUNNING
//
// MessageText:
//
//  The COM+ registry database is already running
//
  COMADMIN_E_REGDB_ALREADYRUNNING = HRESULT($80110475);

//
// MessageId: COMADMIN_E_MIG_VERSIONNOTSUPPORTED
//
// MessageText:
//
//  This version of the COM+ registry database cannot be migrated
//
  COMADMIN_E_MIG_VERSIONNOTSUPPORTED = HRESULT($80110480);

//
// MessageId: COMADMIN_E_MIG_SCHEMANOTFOUND
//
// MessageText:
//
//  The schema version to be migrated could not be found in the COM+ registry database
//
  COMADMIN_E_MIG_SCHEMANOTFOUND = HRESULT($80110481);

//
// MessageId: COMADMIN_E_CAT_BITNESSMISMATCH
//
// MessageText:
//
//  There was a type mismatch between binaries
//
  COMADMIN_E_CAT_BITNESSMISMATCH = HRESULT($80110482);

//
// MessageId: COMADMIN_E_CAT_UNACCEPTABLEBITNESS
//
// MessageText:
//
//  A binary of unknown or invalid type was provided
//
  COMADMIN_E_CAT_UNACCEPTABLEBITNESS = HRESULT($80110483);

//
// MessageId: COMADMIN_E_CAT_WRONGAPPBITNESS
//
// MessageText:
//
//  There was a type mismatch between a binary and an application
//
  COMADMIN_E_CAT_WRONGAPPBITNESS = HRESULT($80110484);

//
// MessageId: COMADMIN_E_CAT_PAUSE_RESUME_NOT_SUPPORTED
//
// MessageText:
//
//  The application cannot be paused or resumed
//
  COMADMIN_E_CAT_PAUSE_RESUME_NOT_SUPPORTED = HRESULT($80110485);

//
// MessageId: COMADMIN_E_CAT_SERVERFAULT
//
// MessageText:
//
//  The COM+ Catalog Server threw an exception during execution
//
  COMADMIN_E_CAT_SERVERFAULT = HRESULT($80110486);

//
// COMPLUS Queued component errors
//
//
// MessageId: COMQC_E_APPLICATION_NOT_QUEUED
//
// MessageText:
//
//  Only COM+ Applications marked "queued" can be invoked using the "queue" moniker
//
  COMQC_E_APPLICATION_NOT_QUEUED = HRESULT($80110600);

//
// MessageId: COMQC_E_NO_QUEUEABLE_INTERFACES
//
// MessageText:
//
//  At least one interface must be marked "queued" in order to create a queued component instance with the "queue" moniker
//
  COMQC_E_NO_QUEUEABLE_INTERFACES = HRESULT($80110601);

//
// MessageId: COMQC_E_QUEUING_SERVICE_NOT_AVAILABLE
//
// MessageText:
//
//  MSMQ is required for the requested operation and is not installed
//
  COMQC_E_QUEUING_SERVICE_NOT_AVAILABLE = HRESULT($80110602);

//
// MessageId: COMQC_E_NO_IPERSISTSTREAM
//
// MessageText:
//
//  Unable to marshal an interface that does not support IPersistStream
//
  COMQC_E_NO_IPERSISTSTREAM = HRESULT($80110603);

//
// MessageId: COMQC_E_BAD_MESSAGE
//
// MessageText:
//
//  The message is improperly formatted or was damaged in transit
//
  COMQC_E_BAD_MESSAGE = HRESULT($80110604);

//
// MessageId: COMQC_E_UNAUTHENTICATED
//
// MessageText:
//
//  An unauthenticated message was received by an application that accepts only authenticated messages
//
  COMQC_E_UNAUTHENTICATED = HRESULT($80110605);

//
// MessageId: COMQC_E_UNTRUSTED_ENQUEUER
//
// MessageText:
//
//  The message was requeued or moved by a user not in the "QC Trusted User" role
//
  COMQC_E_UNTRUSTED_ENQUEUER = HRESULT($80110606);

//
// The range 0x700-0x7ff is reserved for MSDTC errors.
//
//
// MessageId: MSDTC_E_DUPLICATE_RESOURCE
//
// MessageText:
//
//  Cannot create a duplicate resource of type Distributed Transaction Coordinator
//
  MSDTC_E_DUPLICATE_RESOURCE = HRESULT($80110701);

//
// More COMADMIN errors from 0x8**
//
//
// MessageId: COMADMIN_E_OBJECT_PARENT_MISSING
//
// MessageText:
//
//  One of the objects being inserted or updated does not belong to a valid parent collection
//
  COMADMIN_E_OBJECT_PARENT_MISSING = HRESULT($80110808);

//
// MessageId: COMADMIN_E_OBJECT_DOES_NOT_EXIST
//
// MessageText:
//
//  One of the specified objects cannot be found
//
  COMADMIN_E_OBJECT_DOES_NOT_EXIST = HRESULT($80110809);

//
// MessageId: COMADMIN_E_APP_NOT_RUNNING
//
// MessageText:
//
//  The specified application is not currently running
//
  COMADMIN_E_APP_NOT_RUNNING = HRESULT($8011080A);

//
// MessageId: COMADMIN_E_INVALID_PARTITION
//
// MessageText:
//
//  The partition(s) specified are not valid.
//
  COMADMIN_E_INVALID_PARTITION = HRESULT($8011080B);

//
// MessageId: COMADMIN_E_SVCAPP_NOT_POOLABLE_OR_RECYCLABLE
//
// MessageText:
//
//  COM+ applications that run as NT service may not be pooled or recycled
//
  COMADMIN_E_SVCAPP_NOT_POOLABLE_OR_RECYCLABLE = HRESULT($8011080D);

//
// MessageId: COMADMIN_E_USER_IN_SET
//
// MessageText:
//
//  One or more users are already assigned to a local partition set.
//
  COMADMIN_E_USER_IN_SET = HRESULT($8011080E);

//
// MessageId: COMADMIN_E_CANTRECYCLELIBRARYAPPS
//
// MessageText:
//
//  Library applications may not be recycled.
//
  COMADMIN_E_CANTRECYCLELIBRARYAPPS = HRESULT($8011080F);

//
// MessageId: COMADMIN_E_CANTRECYCLESERVICEAPPS
//
// MessageText:
//
//  Applications running as NT services may not be recycled.
//
  COMADMIN_E_CANTRECYCLESERVICEAPPS = HRESULT($80110811);

//
// MessageId: COMADMIN_E_PROCESSALREADYRECYCLED
//
// MessageText:
//
//  The process has already been recycled.
//
  COMADMIN_E_PROCESSALREADYRECYCLED = HRESULT($80110812);

//
// MessageId: COMADMIN_E_PAUSEDPROCESSMAYNOTBERECYCLED
//
// MessageText:
//
//  A paused process may not be recycled.
//
  COMADMIN_E_PAUSEDPROCESSMAYNOTBERECYCLED = HRESULT($80110813);

//
// MessageId: COMADMIN_E_CANTMAKEINPROCSERVICE
//
// MessageText:
//
//  Library applications may not be NT services.
//
  COMADMIN_E_CANTMAKEINPROCSERVICE = HRESULT($80110814);

//
// MessageId: COMADMIN_E_PROGIDINUSEBYCLSID
//
// MessageText:
//
//  The ProgID provided to the copy operation is invalid. The ProgID is in use by another registered CLSID.
//
  COMADMIN_E_PROGIDINUSEBYCLSID = HRESULT($80110815);

//
// MessageId: COMADMIN_E_DEFAULT_PARTITION_NOT_IN_SET
//
// MessageText:
//
//  The partition specified as default is not a member of the partition set.
//
  COMADMIN_E_DEFAULT_PARTITION_NOT_IN_SET = HRESULT($80110816);

//
// MessageId: COMADMIN_E_RECYCLEDPROCESSMAYNOTBEPAUSED
//
// MessageText:
//
//  A recycled process may not be paused.
//
  COMADMIN_E_RECYCLEDPROCESSMAYNOTBEPAUSED = HRESULT($80110817);

//
// MessageId: COMADMIN_E_PARTITION_ACCESSDENIED
//
// MessageText:
//
//  Access to the specified partition is denied.
//
  COMADMIN_E_PARTITION_ACCESSDENIED = HRESULT($80110818);

//
// MessageId: COMADMIN_E_PARTITION_MSI_ONLY
//
// MessageText:
//
//  Only Application Files (*.MSI files) can be installed into partitions.
//
  COMADMIN_E_PARTITION_MSI_ONLY = HRESULT($80110819);

//
// MessageId: COMADMIN_E_LEGACYCOMPS_NOT_ALLOWED_IN_1_0_FORMAT
//
// MessageText:
//
//  Applications containing one or more legacy components may not be exported to 1.0 format.
//
  COMADMIN_E_LEGACYCOMPS_NOT_ALLOWED_IN_1_0_FORMAT = HRESULT($8011081A);

//
// MessageId: COMADMIN_E_LEGACYCOMPS_NOT_ALLOWED_IN_NONBASE_PARTITIONS
//
// MessageText:
//
//  Legacy components may not exist in non-base partitions.
//
  COMADMIN_E_LEGACYCOMPS_NOT_ALLOWED_IN_NONBASE_PARTITIONS = HRESULT($8011081B);

//
// MessageId: COMADMIN_E_COMP_MOVE_SOURCE
//
// MessageText:
//
//  A component cannot be moved (or copied) from the System Application, an application proxy or a non-changeable application
//
  COMADMIN_E_COMP_MOVE_SOURCE = HRESULT($8011081C);

//
// MessageId: COMADMIN_E_COMP_MOVE_DEST
//
// MessageText:
//
//  A component cannot be moved (or copied) to the System Application, an application proxy or a non-changeable application
//
  COMADMIN_E_COMP_MOVE_DEST = HRESULT($8011081D);

//
// MessageId: COMADMIN_E_COMP_MOVE_PRIVATE
//
// MessageText:
//
//  A private component cannot be moved (or copied) to a library application or to the base partition
//
  COMADMIN_E_COMP_MOVE_PRIVATE = HRESULT($8011081E);

//
// MessageId: COMADMIN_E_BASEPARTITION_REQUIRED_IN_SET
//
// MessageText:
//
//  The Base Application Partition exists in all partition sets and cannot be removed.
//
  COMADMIN_E_BASEPARTITION_REQUIRED_IN_SET = HRESULT($8011081F);

//
// MessageId: COMADMIN_E_CANNOT_ALIAS_EVENTCLASS
//
// MessageText:
//
//  Alas, Event Class components cannot be aliased.
//
  COMADMIN_E_CANNOT_ALIAS_EVENTCLASS = HRESULT($80110820);

//
// MessageId: COMADMIN_E_PRIVATE_ACCESSDENIED
//
// MessageText:
//
//  Access is denied because the component is private.
//
  COMADMIN_E_PRIVATE_ACCESSDENIED = HRESULT($80110821);

//
// MessageId: COMADMIN_E_SAFERINVALID
//
// MessageText:
//
//  The specified SAFER level is invalid.
//
  COMADMIN_E_SAFERINVALID = HRESULT($80110822);

//
// MessageId: COMADMIN_E_REGISTRY_ACCESSDENIED
//
// MessageText:
//
//  The specified user cannot write to the system registry
//
  COMADMIN_E_REGISTRY_ACCESSDENIED = HRESULT($80110823);

//
// MessageId: COMADMIN_E_PARTITIONS_DISABLED
//
// MessageText:
//
//  COM+ partitions are currently disabled.
//
  COMADMIN_E_PARTITIONS_DISABLED = HRESULT($80110824);

implementation

function SUCCEEDED(Status: HRESULT): BOOL;
begin
  Result := Status >= 0;
end;

function FAILED(Status: HRESULT): BOOL;
begin
  Result := Status < 0;
end;

function IS_ERROR(Status: HRESULT): BOOL;
begin
  Result := (Status shr 31) = SEVERITY_ERROR;
end;

function HRESULT_CODE(hr: HRESULT): DWORD;
begin
  Result := hr and $FFFF;
end;

function SCODE_CODE(sc: LONG): DWORD;
begin
  Result := sc and $FFFF;
end;

function HRESULT_FACILITY(hr: HRESULT): DWORD;
begin
  Result := (hr shr 16) and $1FFF;
end;

function SCODE_FACILITY(sc: LONG): DWORD;
begin
  Result := (sc shr 16) and $1FFF;
end;

function HRESULT_SEVERITY(hr: HRESULT): DWORD;
begin
  Result := (hr shr 31) and $1;
end;

function SCODE_SEVERITY(sc: LONG): DWORD;
begin
  Result := (sc shr 31) and $1;
end;

function MAKE_HRESULT(sev, fac, code: DWORD): HRESULT;
begin
  Result := HRESULT((sev shl 31) or (fac shl 16) or code);
end;

function MAKE_SCODE(sev, fac,code: DWORD): DWORD;
begin
  Result := DWORD((sev shl 31) or (fac shl 16) or code);
end;

function __HRESULT_FROM_WIN32(x: DWORD): HRESULT;
begin
  if HRESULT(x) <= 0 then
    Result := HRESULT(x)
  else
    Result := HRESULT((x and $0000FFFF) or (FACILITY_WIN32 shl 16) or $80000000);
end;

function HRESULT_FROM_WIN32(x: DWORD): HRESULT;
begin
  Result := __HRESULT_FROM_WIN32(x);
end;

function HRESULT_FROM_NT(x: NTSTATUS): HRESULT;
begin
  Result := HRESULT(x or FACILITY_NT_BIT);
end;

function HRESULT_FROM_SETUPAPI(x: DWORD): HRESULT;
begin
  if (x and (APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR)) =
    (APPLICATION_ERROR_MASK or ERROR_SEVERITY_ERROR) then
    Result := HRESULT((x and $0000FFFF) or (FACILITY_SETUPAPI shl 16) or $80000000)
  else
    Result := HRESULT_FROM_WIN32(x);
end;

function GetScode(hr: HRESULT): DWORD;
begin
  Result := DWORD(hr);
end;

function ResultFromScode(sc: DWORD): HRESULT;
begin
  Result := HRESULT(sc);
end;

function PropagateResult(hrPrevious, scBase: DWORD): HRESULT;
begin
  Result := HRESULT(scBase);
end;

end.
