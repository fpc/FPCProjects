{******************************************************************************}
{                                                       	               }
{ DS Backup/Restore Error Codes API interface Unit for Object Pascal           }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: ntdsbmsg.h, released June 2000. The original Pascal    }
{ code is: NtDsBMsg.pas, released December 2000. The initial developer of the  }
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

unit JwaNtDsBMsg;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "NtDsBMsg.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinError, JwaWinType;

//
//	Windows NT Directory Service Backup/Restore API error codes
//	Copyright (C) 1996-1999, Microsoft Corporation
//

//
//	SUCCESS
//
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
  FACILITY_SYSTEM                 = $0;
  FACILITY_NTDSB                  = $800;
  FACILITY_BACKUP                 = $7FF;

//
// Define the severity codes
//

  STATUS_SEVERITY_WARNING         = $2;
  STATUS_SEVERITY_SUCCESS         = $0;
  STATUS_SEVERITY_INFORMATIONAL   = $1;
  STATUS_SEVERITY_ERROR           = $3;


//
// MessageId: hrNone
//
// MessageText:
//
//  The operation was successful
//
  hrNone = HRESULT($00000000);

//
//	ERRORS
//
//
// MessageId: hrNyi
//
// MessageText:
//
//  The function is not yet implemented
//
  hrNyi = HRESULT($C0000001);

//
//	Backup errors
//
//
// MessageId: hrInvalidParam
//
// MessageText:
//
//  The parameter is not valid.
//
  hrInvalidParam = HRESULT($C7FF0001);

//
// MessageId: hrError
//
// MessageText:
//
//  An internal error has occurred.
//
  hrError = HRESULT($C7FF0002);

//
// MessageId: hrInvalidHandle
//
// MessageText:
//
//  The handle is not valid.
//
  hrInvalidHandle = HRESULT($C7FF0003);

//
// MessageId: hrRestoreInProgress
//
// MessageText:
//
//  The Restore process is already in progress.
//
  hrRestoreInProgress = HRESULT($C7FF0004);

//
// MessageId: hrAlreadyOpen
//
// MessageText:
//
//  The file specified is already open.
//
  hrAlreadyOpen = HRESULT($C7FF0005);

//
// MessageId: hrInvalidRecips
//
// MessageText:
//
//  The recipients are invalid.
//
  hrInvalidRecips = HRESULT($C7FF0006);

//
// MessageId: hrCouldNotConnect
//
// MessageText:
//
//  Unable to perform the backup. Either you are not connected to the specified backup server
//  or the service you are trying to backup is not running.
//
  hrCouldNotConnect = HRESULT($C7FF0007);

//
// MessageId: hrRestoreMapExists
//
// MessageText:
//
//  A restore map already exists for the specified component.  You can only specify
//  a restore map when performing a full restore.
//
  hrRestoreMapExists = HRESULT($C7FF0008);

//
// MessageId: hrIncrementalBackupDisabled
//
// MessageText:
//
//  Another application has modified the specified Windows NT Directory Service database such that any
//  subsequent backups will fail. You must perform a full backup to fix this problem.
//
  hrIncrementalBackupDisabled = HRESULT($C7FF0009);

//
// MessageId: hrLogFileNotFound
//
// MessageText:
//
//  Unable to perform an incremental backup because a required Windows NT Directory Service database log file could not be found.
//
  hrLogFileNotFound = HRESULT($C7FF000A);

//
// MessageId: hrCircularLogging
//
// MessageText:
//
//  The Windows NT Directory Service component specified is configured to use circular database logs.
//  It cannot be backed up without a full backup.
//
  hrCircularLogging = HRESULT($C7FF000B);

//
// MessageId: hrNoFullRestore
//
// MessageText:
//
//  The databases have not been restored to this machine. You cannot restore an incremental backup
//  until a full backup has been restored.
//
  hrNoFullRestore = HRESULT($C7FF000C);

//
// MessageId: hrCommunicationError
//
// MessageText:
//
//  A communications error occurred while attempting to perform a local backup.
//
  hrCommunicationError = HRESULT($C7FF000D);

//
// MessageId: hrFullBackupNotTaken
//
// MessageText:
//
//  You must perform a full backup before you can perform an incremental backup.
//
  hrFullBackupNotTaken = HRESULT($C7FF000E);

//
// MessageId: hrMissingExpiryToken
//
// MessageText:
//
//  Expiry token is missing. Cannot restore without knowing the expiry information.
//
  hrMissingExpiryToken = HRESULT($C7FF000F);

//
// MessageId: hrUnknownExpiryTokenFormat
//
// MessageText:
//
//  Expiry token is in unrecognizable format.
//
  hrUnknownExpiryTokenFormat = HRESULT($C7FF0010);

//
// MessageId: hrContentsExpired
//
// MessageText:
//
//  DS Contents in the backup copy are out of date. Try restoring with a more recent copy.
//
  hrContentsExpired = HRESULT($C7FF0011);

  hrAlreadyListening = HRESULT(RPC_S_ALREADY_LISTENING);
  
//
//	ERRORS
//
//
// SYSTEM errors
//
//
// MessageId: hrFileClose
//
// MessageText:
//
//  Unable to close the DOS file
//
  hrFileClose = HRESULT($C8000066);

//
// MessageId: hrOutOfThreads
//
// MessageText:
//
//  Unable to start a thread because there are none available.
//
  hrOutOfThreads = HRESULT($C8000067);

//
// MessageId: hrTooManyIO
//
// MessageText:
//
//  The system is busy because there are too many I/Os.
//
  hrTooManyIO = HRESULT($C8000069);

//
//	BUFFER MANAGER errors
//
//
// MessageId: hrBFNotSynchronous
//
// MessageText:
//
//  The buffer page has been evicted.
//
  hrBFNotSynchronous = HRESULT($880000C8);

//
// MessageId: hrBFPageNotFound
//
// MessageText:
//
//  Unable to find the page.
//
  hrBFPageNotFound = HRESULT($880000C9);

//
// MessageId: hrBFInUse
//
// MessageText:
//
//  Unable to abandon the buffer.
//
  hrBFInUse = HRESULT($C80000CA);

//
//	DIRECTORY MANAGER errors
//
//
// MessageId: hrPMRecDeleted
//
// MessageText:
//
//  The record has been deleted.
//
  hrPMRecDeleted = HRESULT($C800012E);

//
// MessageId: hrRemainingVersions
//
// MessageText:
//
//  There is idle work remaining.
//
  hrRemainingVersions = HRESULT($88000141);

//
//	RECORD MANAGER errors
//
//
// MessageId: hrFLDKeyTooBig
//
// MessageText:
//
//  The key was truncated because it exceeded the maximum length.
//
  hrFLDKeyTooBig = HRESULT($88000190);

//
// MessageId: hrFLDTooManySegments
//
// MessageText:
//
//  There are too many key segments.
//
  hrFLDTooManySegments = HRESULT($C8000191);

//
// MessageId: hrFLDNullKey
//
// MessageText:
//
//  The key is NULL.
//
  hrFLDNullKey = HRESULT($88000192);

//
//	LOGGING/RECOVERY errors
//
//
// MessageId: hrLogFileCorrupt
//
// MessageText:
//
//  The log file is damaged.
//
  hrLogFileCorrupt = HRESULT($C80001F5);

//
// MessageId: hrNoBackupDirectory
//
// MessageText:
//
//  No backup directory was given.
//
  hrNoBackupDirectory = HRESULT($C80001F7);

//
// MessageId: hrBackupDirectoryNotEmpty
//
// MessageText:
//
//  The backup directory is not empty.
//
  hrBackupDirectoryNotEmpty = HRESULT($C80001F8);

//
// MessageId: hrBackupInProgress
//
// MessageText:
//
//  Backup is already active.
//
  hrBackupInProgress = HRESULT($C80001F9);

//
// MessageId: hrMissingPreviousLogFile
//
// MessageText:
//
//  A log file for the checkpoint is missing.
//
  hrMissingPreviousLogFile = HRESULT($C80001FD);

//
// MessageId: hrLogWriteFail
//
// MessageText:
//
//  Unable to write to the log file.
//
  hrLogWriteFail = HRESULT($C80001FE);

//
// MessageId: hrBadLogVersion
//
// MessageText:
//
//  The version of the log file is not compatible with the version of the Windows NT Directory Service database (NTDS).
//
  hrBadLogVersion = HRESULT($C8000202);

//
// MessageId: hrInvalidLogSequence
//
// MessageText:
//
//  The time stamp in the next log does not match what was expected.
//
  hrInvalidLogSequence = HRESULT($C8000203);

//
// MessageId: hrLoggingDisabled
//
// MessageText:
//
//  The log is not active.
//
  hrLoggingDisabled = HRESULT($C8000204);

//
// MessageId: hrLogBufferTooSmall
//
// MessageText:
//
//  The log buffer is too small to be recovered.
//
  hrLogBufferTooSmall = HRESULT($C8000205);

//
// MessageId: hrLogSequenceEnd
//
// MessageText:
//
//  The maximum number of log files has been exceeded.
//
  hrLogSequenceEnd = HRESULT($C8000207);

//
// MessageId: hrNoBackup
//
// MessageText:
//
//  There is no backup in progress.
//
  hrNoBackup = HRESULT($C8000208);

//
// MessageId: hrInvalidBackupSequence
//
// MessageText:
//
//  The backup call is out of sequence.
//
  hrInvalidBackupSequence = HRESULT($C8000209);

//
// MessageId: hrBackupNotAllowedYet
//
// MessageText:
//
//  Unable to perform a backup now.
//
  hrBackupNotAllowedYet = HRESULT($C800020B);

//
// MessageId: hrDeleteBackupFileFail
//
// MessageText:
//
//  Unable to delete the backup file.
//
  hrDeleteBackupFileFail = HRESULT($C800020C);

//
// MessageId: hrMakeBackupDirectoryFail
//
// MessageText:
//
//  Unable to make a backup temporary directory.
//
  hrMakeBackupDirectoryFail = HRESULT($C800020D);

//
// MessageId: hrInvalidBackup
//
// MessageText:
//
//  An incremental backup cannot be performed when circular logging is enabled.
//
  hrInvalidBackup = HRESULT($C800020E);

//
// MessageId: hrRecoveredWithErrors
//
// MessageText:
//
//  Errors were encountered during the repair process.
//
  hrRecoveredWithErrors = HRESULT($C800020F);

//
// MessageId: hrMissingLogFile
//
// MessageText:
//
//  The current log file is missing.
//
  hrMissingLogFile = HRESULT($C8000210);

//
// MessageId: hrLogDiskFull
//
// MessageText:
//
//  The log disk is full.
//
  hrLogDiskFull = HRESULT($C8000211);

//
// MessageId: hrBadLogSignature
//
// MessageText:
//
//  A log file is damaged.
//
  hrBadLogSignature = HRESULT($C8000212);

//
// MessageId: hrBadDbSignature
//
// MessageText:
//
//  A database file is damaged.
//
  hrBadDbSignature = HRESULT($C8000213);

//
// MessageId: hrBadCheckpointSignature
//
// MessageText:
//
//  A checkpoint file is damaged.
//
  hrBadCheckpointSignature = HRESULT($C8000214);

//
// MessageId: hrCheckpointCorrupt
//
// MessageText:
//
//  A checkpoint file either could not be found or is damaged.
//
  hrCheckpointCorrupt = HRESULT($C8000215);

//
// MessageId: hrDatabaseInconsistent
//
// MessageText:
//
//  The database is damaged.
//
  hrDatabaseInconsistent = HRESULT($C8000226);

//
// MessageId: hrConsistentTimeMismatch
//
// MessageText:
//
//  There is a mismatch in the database's last consistent time.
//
  hrConsistentTimeMismatch = HRESULT($C8000227);

//
// MessageId: hrPatchFileMismatch
//
// MessageText:
//
//  The patch file is not generated from this backup.
//
  hrPatchFileMismatch = HRESULT($C8000228);

//
// MessageId: hrRestoreLogTooLow
//
// MessageText:
//
//  The starting log number is too low for the restore.
//
  hrRestoreLogTooLow = HRESULT($C8000229);

//
// MessageId: hrRestoreLogTooHigh
//
// MessageText:
//
//  The starting log number is too high for the restore.
//
  hrRestoreLogTooHigh = HRESULT($C800022A);

//
// MessageId: hrGivenLogFileHasBadSignature
//
// MessageText:
//
//  The log file downloaded from the tape is damaged.
//
  hrGivenLogFileHasBadSignature = HRESULT($C800022B);

//
// MessageId: hrGivenLogFileIsNotContiguous
//
// MessageText:
//
//  Unable to find a mandatory log file after the tape was downloaded.
//
  hrGivenLogFileIsNotContiguous = HRESULT($C800022C);

//
// MessageId: hrMissingRestoreLogFiles
//
// MessageText:
//
//  The data is not fully restored because some log files are missing.
//
  hrMissingRestoreLogFiles = HRESULT($C800022D);

//
// MessageId: hrExistingLogFileHasBadSignature
//
// MessageText:
//
//  The log file in the log file path is damaged.
//
  hrExistingLogFileHasBadSignature = HRESULT($8800022E);

//
// MessageId: hrExistingLogFileIsNotContiguous
//
// MessageText:
//
//  Unable to find a mandatory log file in the log file path.
//
  hrExistingLogFileIsNotContiguous = HRESULT($8800022F);

//
// MessageId: hrMissingFullBackup
//
// MessageText:
//
//  The database missed a previous full backup before the incremental backup.
//
  hrMissingFullBackup = HRESULT($C8000230);

//
// MessageId: hrBadBackupDatabaseSize
//
// MessageText:
//
//  The backup database size must be a multiple of 4K (4096 bytes).
//
  hrBadBackupDatabaseSize = HRESULT($C8000231);

//
// MessageId: hrTermInProgress
//
// MessageText:
//
//  The database is being shut down.
//
  hrTermInProgress = HRESULT($C80003E8);

//
// MessageId: hrFeatureNotAvailable
//
// MessageText:
//
//  The feature is not available.
//
  hrFeatureNotAvailable = HRESULT($C80003E9);

//
// MessageId: hrInvalidName
//
// MessageText:
//
//  The name is not valid.
//
  hrInvalidName = HRESULT($C80003EA);

//
// MessageId: hrInvalidParameter
//
// MessageText:
//
//  The parameter is not valid.
//
  hrInvalidParameter = HRESULT($C80003EB);

//
// MessageId: hrColumnNull
//
// MessageText:
//
//  The value of the column is null.
//
  hrColumnNull = HRESULT($880003EC);

//
// MessageId: hrBufferTruncated
//
// MessageText:
//
//  The buffer is too small for data.
//
  hrBufferTruncated = HRESULT($880003EE);

//
// MessageId: hrDatabaseAttached
//
// MessageText:
//
//  The database is already attached.
//
  hrDatabaseAttached = HRESULT($880003EF);

//
// MessageId: hrInvalidDatabaseId
//
// MessageText:
//
//  The database ID is not valid.
//
  hrInvalidDatabaseId = HRESULT($C80003F2);

//
// MessageId: hrOutOfMemory
//
// MessageText:
//
//  The computer is out of memory.
//
  hrOutOfMemory = HRESULT($C80003F3);

//
// MessageId: hrOutOfDatabaseSpace
//
// MessageText:
//
//  The database has reached the maximum size of 16 GB.
//
  hrOutOfDatabaseSpace = HRESULT($C80003F4);

//
// MessageId: hrOutOfCursors
//
// MessageText:
//
//  Out of table cursors.
//
  hrOutOfCursors = HRESULT($C80003F5);

//
// MessageId: hrOutOfBuffers
//
// MessageText:
//
//  Out of database page buffers.
//
  hrOutOfBuffers = HRESULT($C80003F6);

//
// MessageId: hrTooManyIndexes
//
// MessageText:
//
//  There are too many indexes.
//
  hrTooManyIndexes = HRESULT($C80003F7);

//
// MessageId: hrTooManyKeys
//
// MessageText:
//
//  There are too many columns in an index.
//
  hrTooManyKeys = HRESULT($C80003F8);

//
// MessageId: hrRecordDeleted
//
// MessageText:
//
//  The record has been deleted.
//
  hrRecordDeleted = HRESULT($C80003F9);

//
// MessageId: hrReadVerifyFailure
//
// MessageText:
//
//  A read verification error occurred.
//
  hrReadVerifyFailure = HRESULT($C80003FA);

//
// MessageId: hrOutOfFileHandles
//
// MessageText:
//
//  Out of file handles.
//
  hrOutOfFileHandles = HRESULT($C80003FC);

//
// MessageId: hrDiskIO
//
// MessageText:
//
//  A disk I/O error occurred.
//
  hrDiskIO = HRESULT($C80003FE);

//
// MessageId: hrInvalidPath
//
// MessageText:
//
//  The path to the file is not valid.
//
  hrInvalidPath = HRESULT($C80003FF);

//
// MessageId: hrRecordTooBig
//
// MessageText:
//
//  The record has exceeded the maximum size.
//
  hrRecordTooBig = HRESULT($C8000402);

//
// MessageId: hrTooManyOpenDatabases
//
// MessageText:
//
//  There are too many open databases.
//
  hrTooManyOpenDatabases = HRESULT($C8000403);

//
// MessageId: hrInvalidDatabase
//
// MessageText:
//
//  The file is not a database file.
//
  hrInvalidDatabase = HRESULT($C8000404);

//
// MessageId: hrNotInitialized
//
// MessageText:
//
//  The database was not yet called.
//
  hrNotInitialized = HRESULT($C8000405);

//
// MessageId: hrAlreadyInitialized
//
// MessageText:
//
//  The database was already called.
//
  hrAlreadyInitialized = HRESULT($C8000406);

//
// MessageId: hrFileAccessDenied
//
// MessageText:
//
//  Unable to access the file.
//
  hrFileAccessDenied = HRESULT($C8000408);

//
// MessageId: hrBufferTooSmall
//
// MessageText:
//
//  The buffer is too small.
//
  hrBufferTooSmall = HRESULT($C800040E);

//
// MessageId: hrSeekNotEqual
//
// MessageText:
//
//  Either SeekLE or SeekGE did not find an exact match.
//
  hrSeekNotEqual = HRESULT($8800040F);

//
// MessageId: hrTooManyColumns
//
// MessageText:
//
//  There are too many columns defined.
//
  hrTooManyColumns = HRESULT($C8000410);

//
// MessageId: hrContainerNotEmpty
//
// MessageText:
//
//  The container is not empty.
//
  hrContainerNotEmpty = HRESULT($C8000413);

//
// MessageId: hrInvalidFilename
//
// MessageText:
//
//  The filename is not valid.
//
  hrInvalidFilename = HRESULT($C8000414);

//
// MessageId: hrInvalidBookmark
//
// MessageText:
//
//  The bookmark is not valid.
//
  hrInvalidBookmark = HRESULT($C8000415);

//
// MessageId: hrColumnInUse
//
// MessageText:
//
//  The column is used in an index.
//
  hrColumnInUse = HRESULT($C8000416);

//
// MessageId: hrInvalidBufferSize
//
// MessageText:
//
//  The data buffer does not match the column size.
//
  hrInvalidBufferSize = HRESULT($C8000417);

//
// MessageId: hrColumnNotUpdatable
//
// MessageText:
//
//  Unable to set the column value.
//
  hrColumnNotUpdatable = HRESULT($C8000418);

//
// MessageId: hrIndexInUse
//
// MessageText:
//
//  The index is in use.
//
  hrIndexInUse = HRESULT($C800041B);

//
// MessageId: hrNullKeyDisallowed
//
// MessageText:
//
//  Null keys are not allowed on an index.
//
  hrNullKeyDisallowed = HRESULT($C800041D);

//
// MessageId: hrNotInTransaction
//
// MessageText:
//
//  The operation must be within a transaction.
//
  hrNotInTransaction = HRESULT($C800041E);

//
// MessageId: hrNoIdleActivity
//
// MessageText:
//
//  No idle activity occurred.
//
  hrNoIdleActivity = HRESULT($88000422);

//
// MessageId: hrTooManyActiveUsers
//
// MessageText:
//
//  There are too many active database users.
//
  hrTooManyActiveUsers = HRESULT($C8000423);

//
// MessageId: hrInvalidCountry
//
// MessageText:
//
//  The country code is either not known or is not valid.
//
  hrInvalidCountry = HRESULT($C8000425);

//
// MessageId: hrInvalidLanguageId
//
// MessageText:
//
//  The language ID is either not known or is not valid.
//
  hrInvalidLanguageId = HRESULT($C8000426);

//
// MessageId: hrInvalidCodePage
//
// MessageText:
//
//  The code page is either not known or is not valid.
//
  hrInvalidCodePage = HRESULT($C8000427);

//
// MessageId: hrNoWriteLock
//
// MessageText:
//
//  There is no write lock at transaction level 0.
//
  hrNoWriteLock = HRESULT($8800042B);

//
// MessageId: hrColumnSetNull
//
// MessageText:
//
//  The column value is set to null.
//
  hrColumnSetNull = HRESULT($8800042C);

//
// MessageId: hrVersionStoreOutOfMemory
//
// MessageText:
//
//   lMaxVerPages exceeded (XJET only)
//
  hrVersionStoreOutOfMemory = HRESULT($C800042D);

//
// MessageId: hrCurrencyStackOutOfMemory
//
// MessageText:
//
//  Out of cursors.
//
  hrCurrencyStackOutOfMemory = HRESULT($C800042E);

//
// MessageId: hrOutOfSessions
//
// MessageText:
//
//  Out of sessions.
//
  hrOutOfSessions = HRESULT($C800044D);

//
// MessageId: hrWriteConflict
//
// MessageText:
//
//  The write lock failed due to an outstanding write lock.
//
  hrWriteConflict = HRESULT($C800044E);

//
// MessageId: hrTransTooDeep
//
// MessageText:
//
//  The transactions are nested too deeply.
//
  hrTransTooDeep = HRESULT($C800044F);

//
// MessageId: hrInvalidSesid
//
// MessageText:
//
//  The session handle is not valid.
//
  hrInvalidSesid = HRESULT($C8000450);

//
// MessageId: hrSessionWriteConflict
//
// MessageText:
//
//  Another session has a private version of the page.
//
  hrSessionWriteConflict = HRESULT($C8000453);

//
// MessageId: hrInTransaction
//
// MessageText:
//
//  The operation is not allowed within a transaction.
//
  hrInTransaction = HRESULT($C8000454);

//
// MessageId: hrDatabaseDuplicate
//
// MessageText:
//
//  The database already exists.
//
  hrDatabaseDuplicate = HRESULT($C80004B1);

//
// MessageId: hrDatabaseInUse
//
// MessageText:
//
//  The database is in use.
//
  hrDatabaseInUse = HRESULT($C80004B2);

//
// MessageId: hrDatabaseNotFound
//
// MessageText:
//
//  The database does not exist.
//
  hrDatabaseNotFound = HRESULT($C80004B3);

//
// MessageId: hrDatabaseInvalidName
//
// MessageText:
//
//  The database name is not valid.
//
  hrDatabaseInvalidName = HRESULT($C80004B4);

//
// MessageId: hrDatabaseInvalidPages
//
// MessageText:
//
//  The number of pages is not valid.
//
  hrDatabaseInvalidPages = HRESULT($C80004B5);

//
// MessageId: hrDatabaseCorrupted
//
// MessageText:
//
//  The database file is either damaged or cannot be found.
//
  hrDatabaseCorrupted = HRESULT($C80004B6);

//
// MessageId: hrDatabaseLocked
//
// MessageText:
//
//  The database is locked.
//
  hrDatabaseLocked = HRESULT($C80004B7);

//
// MessageId: hrTableEmpty
//
// MessageText:
//
//  An empty table was opened.
//
  hrTableEmpty = HRESULT($88000515);

//
// MessageId: hrTableLocked
//
// MessageText:
//
//  The table is locked.
//
  hrTableLocked = HRESULT($C8000516);

//
// MessageId: hrTableDuplicate
//
// MessageText:
//
//  The table already exists.
//
  hrTableDuplicate = HRESULT($C8000517);

//
// MessageId: hrTableInUse
//
// MessageText:
//
//  Unable to lock the table because it is already in use.
//
  hrTableInUse = HRESULT($C8000518);

//
// MessageId: hrObjectNotFound
//
// MessageText:
//
//  The table or object does not exist.
//
  hrObjectNotFound = HRESULT($C8000519);

//
// MessageId: hrCannotRename
//
// MessageText:
//
//  Unable to rename the temporary file.
//
  hrCannotRename = HRESULT($C800051A);

//
// MessageId: hrDensityInvalid
//
// MessageText:
//
//  The file/index density is not valid.
//
  hrDensityInvalid = HRESULT($C800051B);

//
// MessageId: hrTableNotEmpty
//
// MessageText:
//
//  Unable to define the clustered index.
//
  hrTableNotEmpty = HRESULT($C800051C);

//
// MessageId: hrInvalidTableId
//
// MessageText:
//
//  The table ID is not valid.
//
  hrInvalidTableId = HRESULT($C800051E);

//
// MessageId: hrTooManyOpenTables
//
// MessageText:
//
//  Unable to open any more tables.
//
  hrTooManyOpenTables = HRESULT($C800051F);

//
// MessageId: hrIllegalOperation
//
// MessageText:
//
//  The operation is not supported on tables.
//
  hrIllegalOperation = HRESULT($C8000520);

//
// MessageId: hrObjectDuplicate
//
// MessageText:
//
//  The table or object name is already being used.
//
  hrObjectDuplicate = HRESULT($C8000522);

//
// MessageId: hrInvalidObject
//
// MessageText:
//
//  The object is not valid for operation.
//
  hrInvalidObject = HRESULT($C8000524);

//
// MessageId: hrIndexCantBuild
//
// MessageText:
//
//  Unable to build a clustered index.
//
  hrIndexCantBuild = HRESULT($C8000579);

//
// MessageId: hrIndexHasPrimary
//
// MessageText:
//
//  The primary index is already defined.
//
  hrIndexHasPrimary = HRESULT($C800057A);

//
// MessageId: hrIndexDuplicate
//
// MessageText:
//
//  The index is already defined.
//
  hrIndexDuplicate = HRESULT($C800057B);

//
// MessageId: hrIndexNotFound
//
// MessageText:
//
//  The index does not exist.
//
  hrIndexNotFound = HRESULT($C800057C);

//
// MessageId: hrIndexMustStay
//
// MessageText:
//
//  Unable to delete a clustered index.
//
  hrIndexMustStay = HRESULT($C800057D);

//
// MessageId: hrIndexInvalidDef
//
// MessageText:
//
//  The index definition is illegal.
//
  hrIndexInvalidDef = HRESULT($C800057E);

//
// MessageId: hrIndexHasClustered
//
// MessageText:
//
//  The clustered index is already defined.
//
  hrIndexHasClustered = HRESULT($C8000580);

//
// MessageId: hrCreateIndexFailed
//
// MessageText:
//
//  Unable to create the index because an error occurred while creating a table.
//
  hrCreateIndexFailed = HRESULT($88000581);

//
// MessageId: hrTooManyOpenIndexes
//
// MessageText:
//
//  Out of index description blocks.
//
  hrTooManyOpenIndexes = HRESULT($C8000582);

//
// MessageId: hrColumnLong
//
// MessageText:
//
//  The column value is too long.
//
  hrColumnLong = HRESULT($C80005DD);

//
// MessageId: hrColumnDoesNotFit
//
// MessageText:
//
//  The field will not fit in the record.
//
  hrColumnDoesNotFit = HRESULT($C80005DF);

//
// MessageId: hrNullInvalid
//
// MessageText:
//
//  The value cannot be null.
//
  hrNullInvalid = HRESULT($C80005E0);

//
// MessageId: hrColumnIndexed
//
// MessageText:
//
//  Unable to delete because the column is indexed.
//
  hrColumnIndexed = HRESULT($C80005E1);

//
// MessageId: hrColumnTooBig
//
// MessageText:
//
//  The length of the field exceeds the maximum length.
//
  hrColumnTooBig = HRESULT($C80005E2);

//
// MessageId: hrColumnNotFound
//
// MessageText:
//
//  Unable to find the column.
//
  hrColumnNotFound = HRESULT($C80005E3);

//
// MessageId: hrColumnDuplicate
//
// MessageText:
//
//  The field is already defined.
//
  hrColumnDuplicate = HRESULT($C80005E4);

//
// MessageId: hrColumn2ndSysMaint
//
// MessageText:
//
//  Only one auto-increment or version column is allowed per table.
//
  hrColumn2ndSysMaint = HRESULT($C80005E6);

//
// MessageId: hrInvalidColumnType
//
// MessageText:
//
//  The column data type is not valid.
//
  hrInvalidColumnType = HRESULT($C80005E7);

//
// MessageId: hrColumnMaxTruncated
//
// MessageText:
//
//  The column was truncated because it exceeded the maximum length.
//
  hrColumnMaxTruncated = HRESULT($880005E8);

//
// MessageId: hrColumnCannotIndex
//
// MessageText:
//
//  Unable to index a long value column.
//
  hrColumnCannotIndex = HRESULT($C80005E9);

//
// MessageId: hrTaggedNotNULL
//
// MessageText:
//
//  Tagged columns cannot be null.
//
  hrTaggedNotNULL = HRESULT($C80005EA);

//
// MessageId: hrNoCurrentIndex
//
// MessageText:
//
//  The entry is not valid without a current index.
//
  hrNoCurrentIndex = HRESULT($C80005EB);

//
// MessageId: hrKeyIsMade
//
// MessageText:
//
//  The key is completely made.
//
  hrKeyIsMade = HRESULT($C80005EC);

//
// MessageId: hrBadColumnId
//
// MessageText:
//
//  The column ID is not correct.
//
  hrBadColumnId = HRESULT($C80005ED);

//
// MessageId: hrBadItagSequence
//
// MessageText:
//
//  There is a bad instance identifier for a multivalued column.
//
  hrBadItagSequence = HRESULT($C80005EE);

//
// MessageId: hrCannotBeTagged
//
// MessageText:
//
//  AutoIncrement and Version cannot be multivalued.
//
  hrCannotBeTagged = HRESULT($C80005F1);

//
// MessageId: hrRecordNotFound
//
// MessageText:
//
//  Unable to find the key.
//
  hrRecordNotFound = HRESULT($C8000641);

//
// MessageId: hrNoCurrentRecord
//
// MessageText:
//
//  The currency is not on a record.
//
  hrNoCurrentRecord = HRESULT($C8000643);

//
// MessageId: hrRecordClusteredChanged
//
// MessageText:
//
//  A clustered key cannot be changed.
//
  hrRecordClusteredChanged = HRESULT($C8000644);

//
// MessageId: hrKeyDuplicate
//
// MessageText:
//
//  The key already exists.
//
  hrKeyDuplicate = HRESULT($C8000645);

//
// MessageId: hrAlreadyPrepared
//
// MessageText:
//
//  The current entry has already been copied or cleared.
//
  hrAlreadyPrepared = HRESULT($C8000647);

//
// MessageId: hrKeyNotMade
//
// MessageText:
//
//  No key was made.
//
  hrKeyNotMade = HRESULT($C8000648);

//
// MessageId: hrUpdateNotPrepared
//
// MessageText:
//
//  Update was not prepared.
//
  hrUpdateNotPrepared = HRESULT($C8000649);

//
// MessageId: hrwrnDataHasChanged
//
// MessageText:
//
//  Data has changed.
//
  hrwrnDataHasChanged = HRESULT($8800064A);

//
// MessageId: hrerrDataHasChanged
//
// MessageText:
//
//  The operation was abandoned because data has changed.
//
  hrerrDataHasChanged = HRESULT($C800064B);

//
// MessageId: hrKeyChanged
//
// MessageText:
//
//  Moved to a new key.
//
  hrKeyChanged = HRESULT($88000652);

//
// MessageId: hrTooManySorts
//
// MessageText:
//
//  There are too many sort processes.
//
  hrTooManySorts = HRESULT($C80006A5);

//
// MessageId: hrInvalidOnSort
//
// MessageText:
//
//  An operation that is not valid occurred in the sort.
//
  hrInvalidOnSort = HRESULT($C80006A6);

//
// MessageId: hrTempFileOpenError
//
// MessageText:
//
//  Unable to open the temporary file.
//
  hrTempFileOpenError = HRESULT($C800070B);

//
// MessageId: hrTooManyAttachedDatabases
//
// MessageText:
//
//  There are too many databases open.
//
  hrTooManyAttachedDatabases = HRESULT($C800070D);

//
// MessageId: hrDiskFull
//
// MessageText:
//
//  The disk is full.
//
  hrDiskFull = HRESULT($C8000710);

//
// MessageId: hrPermissionDenied
//
// MessageText:
//
//  Permission is denied.
//
  hrPermissionDenied = HRESULT($C8000711);

//
// MessageId: hrFileNotFound
//
// MessageText:
//
//  Unable to find the file.
//
  hrFileNotFound = HRESULT($C8000713);

//
// MessageId: hrFileOpenReadOnly
//
// MessageText:
//
//  The database file is read only.
//
  hrFileOpenReadOnly = HRESULT($88000715);

//
// MessageId: hrAfterInitialization
//
// MessageText:
//
//  Unable to restore after initialization.
//
  hrAfterInitialization = HRESULT($C800073A);

//
// MessageId: hrLogCorrupted
//
// MessageText:
//
//  The database log files are damaged.
//
  hrLogCorrupted = HRESULT($C800073C);

//
// MessageId: hrInvalidOperation
//
// MessageText:
//
//  The operation is not valid.
//
  hrInvalidOperation = HRESULT($C8000772);

//
// MessageId: hrAccessDenied
//
// MessageText:
//
//  Access is denied.
//
  hrAccessDenied = HRESULT($C8000773);

implementation

end.
