{******************************************************************************}
{                                                       	               }
{ I/O Control Codes API interface Unit for Object Pascal                       }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: winioctl.h, released June 2000. The original Pascal    }
{ code is: WinIoCtl.pas, released December 2000. The initial developer of the  }
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

unit JwaWinIoctl;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinIoCtl.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinNT, JwaWinType;

//
// Device interface class GUIDs.
//
// need these GUIDs outside conditional includes so that user can
//   #include <winioctl.h> in precompiled header
//   #include <initguid.h> in a single source file
//   #include <winioctl.h> in that source file a second time to instantiate the GUIDs
//

const
  GUID_DEVINTERFACE_DISK: TGUID = (
    D1:$53f56307; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_CDROM: TGUID = (
    D1:$53f56308; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_PARTITION: TGUID = (
    D1:$53f5630a; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_TAPE: TGUID = (
    D1:$53f5630b; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_WRITEONCEDISK: TGUID = (
    D1:$53f5630c; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_VOLUME: TGUID = (
    D1:$53f5630d; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_MEDIUMCHANGER: TGUID = (
    D1:$53f56310; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_FLOPPY: TGUID = (
    D1:$53f56311; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_CDCHANGER: TGUID = (
    D1:$53f56312; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_STORAGEPORT: TGUID = (
    D1:$2accfe60; D2:$c130; D3:$11d2; D4:($b0, $82, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_DEVINTERFACE_COMPORT: TGUID = (
    D1:$86e0d1e0; D2:$8089; D3:$11d0; D4:($9c, $e4, $08, $00, $3e, $30, $1f, $73));
  GUID_DEVINTERFACE_SERENUM_BUS_ENUMERATOR: TGUID = (
    D1:$4D36E978; D2:$E325; D3:$11CE; D4:($BF, $C1, $08, $00, $2B, $E1, $03, $18));

//
// Obsolete device interface class GUID names.
// (use of above GUID_DEVINTERFACE_* names is recommended).
//

  // MVB: Note that these "constants" are in reality aliases for the list above. Unfortunately you can't
  // define a GUID without using a type constant and you can't alias a type constant in Delphi...

  DiskClassGuid: TGUID = (
    D1:$53f56307; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  CdRomClassGuid: TGUID = (
    D1:$53f56308; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  PartitionClassGuid: TGUID = (
    D1:$53f5630a; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  TapeClassGuid: TGUID = (
    D1:$53f5630b; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  WriteOnceDiskClassGuid: TGUID = (
    D1:$53f5630c; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  VolumeClassGuid: TGUID = (
    D1:$53f5630d; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  MediumChangerClassGuid: TGUID = (
    D1:$53f56310; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  FloppyClassGuid: TGUID = (
    D1:$53f56311; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  CdChangerClassGuid: TGUID = (
    D1:$53f56312; D2:$b6bf; D3:$11d0; D4:($94, $f2, $00, $a0, $c9, $1e, $fb, $8b));
  StoragePortClassGuid: TGUID = (
    D1:$2accfe60; D2:$c130; D3:$11d2; D4:($b0, $82, $00, $a0, $c9, $1e, $fb, $8b));
  GUID_CLASS_COMPORT: TGUID = (
    D1:$86e0d1e0; D2:$8089; D3:$11d0; D4:($9c, $e4, $08, $00, $3e, $30, $1f, $73));
  GUID_SERENUM_BUS_ENUMERATOR: TGUID = (
    D1:$4D36E978; D2:$E325; D3:$11CE; D4:($BF, $C1, $08, $00, $2B, $E1, $03, $18));

//
// Define the various device type values.  Note that values used by Microsoft
// Corporation are in the range 0-32767, and 32768-65535 are reserved for use
// by customers.
//

type
  DEVICE_TYPE = DWORD;

const
  FILE_DEVICE_BEEP                = $00000001;
  FILE_DEVICE_CD_ROM              = $00000002;
  FILE_DEVICE_CD_ROM_FILE_SYSTEM  = $00000003;
  FILE_DEVICE_CONTROLLER          = $00000004;
  FILE_DEVICE_DATALINK            = $00000005;
  FILE_DEVICE_DFS                 = $00000006;
  FILE_DEVICE_DISK                = $00000007;
  FILE_DEVICE_DISK_FILE_SYSTEM    = $00000008;
  FILE_DEVICE_FILE_SYSTEM         = $00000009;
  FILE_DEVICE_INPORT_PORT         = $0000000a;
  FILE_DEVICE_KEYBOARD            = $0000000b;
  FILE_DEVICE_MAILSLOT            = $0000000c;
  FILE_DEVICE_MIDI_IN             = $0000000d;
  FILE_DEVICE_MIDI_OUT            = $0000000e;
  FILE_DEVICE_MOUSE               = $0000000f;
  FILE_DEVICE_MULTI_UNC_PROVIDER  = $00000010;
  FILE_DEVICE_NAMED_PIPE          = $00000011;
  FILE_DEVICE_NETWORK             = $00000012;
  FILE_DEVICE_NETWORK_BROWSER     = $00000013;
  FILE_DEVICE_NETWORK_FILE_SYSTEM = $00000014;
  FILE_DEVICE_NULL                = $00000015;
  FILE_DEVICE_PARALLEL_PORT       = $00000016;
  FILE_DEVICE_PHYSICAL_NETCARD    = $00000017;
  FILE_DEVICE_PRINTER             = $00000018;
  FILE_DEVICE_SCANNER             = $00000019;
  FILE_DEVICE_SERIAL_MOUSE_PORT   = $0000001a;
  FILE_DEVICE_SERIAL_PORT         = $0000001b;
  FILE_DEVICE_SCREEN              = $0000001c;
  FILE_DEVICE_SOUND               = $0000001d;
  FILE_DEVICE_STREAMS             = $0000001e;
  FILE_DEVICE_TAPE                = $0000001f;
  FILE_DEVICE_TAPE_FILE_SYSTEM    = $00000020;
  FILE_DEVICE_TRANSPORT           = $00000021;
  FILE_DEVICE_UNKNOWN             = $00000022;
  FILE_DEVICE_VIDEO               = $00000023;
  FILE_DEVICE_VIRTUAL_DISK        = $00000024;
  FILE_DEVICE_WAVE_IN             = $00000025;
  FILE_DEVICE_WAVE_OUT            = $00000026;
  FILE_DEVICE_8042_PORT           = $00000027;
  FILE_DEVICE_NETWORK_REDIRECTOR  = $00000028;
  FILE_DEVICE_BATTERY             = $00000029;
  FILE_DEVICE_BUS_EXTENDER        = $0000002a;
  FILE_DEVICE_MODEM               = $0000002b;
  FILE_DEVICE_VDM                 = $0000002c;
  FILE_DEVICE_MASS_STORAGE        = $0000002d;
  FILE_DEVICE_SMB                 = $0000002e;
  FILE_DEVICE_KS                  = $0000002f;
  FILE_DEVICE_CHANGER             = $00000030;
  FILE_DEVICE_SMARTCARD           = $00000031;
  FILE_DEVICE_ACPI                = $00000032;
  FILE_DEVICE_DVD                 = $00000033;
  FILE_DEVICE_FULLSCREEN_VIDEO    = $00000034;
  FILE_DEVICE_DFS_FILE_SYSTEM     = $00000035;
  FILE_DEVICE_DFS_VOLUME          = $00000036;
  FILE_DEVICE_SERENUM             = $00000037;
  FILE_DEVICE_TERMSRV             = $00000038;
  FILE_DEVICE_KSEC                = $00000039;
  FILE_DEVICE_FIPS                = $0000003A;
  FILE_DEVICE_INFINIBAND          = $0000003B;

//
// Macro definition for defining IOCTL and FSCTL function control codes.  Note
// that function codes 0-2047 are reserved for Microsoft Corporation, and
// 2048-4095 are reserved for customers.
//

function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;

//
// Macro to extract device type out of the device io control code
//

function DEVICE_TYPE_FROM_CTL_CODE(CtrlCode: DWORD): WORD;

//
// Define the method codes for how buffers are passed for I/O and FS controls
//

const
  METHOD_BUFFERED   = 0;
  METHOD_IN_DIRECT  = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER    = 3;

//
// Define some easier to comprehend aliases:
//   METHOD_DIRECT_TO_HARDWARE (writes, aka METHOD_IN_DIRECT)
//   METHOD_DIRECT_FROM_HARDWARE (reads, aka METHOD_OUT_DIRECT)
//

  METHOD_DIRECT_TO_HARDWARE     = METHOD_IN_DIRECT;
  METHOD_DIRECT_FROM_HARDWARE   = METHOD_OUT_DIRECT;

//
// Define the access check value for any access
//
//
// The FILE_READ_ACCESS and FILE_WRITE_ACCESS constants are also defined in
// ntioapi.h as FILE_READ_DATA and FILE_WRITE_DATA. The values for these
// constants *MUST* always be in sync.
//
//
// FILE_SPECIAL_ACCESS is checked by the NT I/O system the same as FILE_ANY_ACCESS.
// The file systems, however, may add additional access checks for I/O and FS controls
// that use this value.
//

const
  FILE_ANY_ACCESS     = 0;
  FILE_SPECIAL_ACCESS = FILE_ANY_ACCESS;
  FILE_READ_ACCESS    = $0001;           // file & pipe
  FILE_WRITE_ACCESS   = $0002;           // file & pipe

//
// IoControlCode values for storage devices
//

  IOCTL_STORAGE_BASE = FILE_DEVICE_MASS_STORAGE;

//
// The following device control codes are common for all class drivers.  They
// should be used in place of the older IOCTL_DISK, IOCTL_CDROM and IOCTL_TAPE
// common codes
//

const
  IOCTL_STORAGE_CHECK_VERIFY = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0200 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_CHECK_VERIFY2 = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0200 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_MEDIA_REMOVAL = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0201 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_EJECT_MEDIA = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0202 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_LOAD_MEDIA = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0203 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_LOAD_MEDIA2 = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0203 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_RESERVE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0204 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_RELEASE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0205 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_FIND_NEW_DEVICES = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0206 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_EJECTION_CONTROL = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0250 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_MCN_CONTROL = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0251 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_GET_MEDIA_TYPES = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0300 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_MEDIA_TYPES_EX = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0301 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0304 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_GET_HOTPLUG_INFO = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0305 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_SET_HOTPLUG_INFO = (
    (IOCTL_STORAGE_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0306 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_RESET_BUS = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0400 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_RESET_DEVICE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0401 shl 2) or METHOD_BUFFERED);
  IOCTL_STORAGE_BREAK_RESERVATION = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0405 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_GET_DEVICE_NUMBER = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0420 shl 2) or METHOD_BUFFERED);

  IOCTL_STORAGE_PREDICT_FAILURE = (
    (IOCTL_STORAGE_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0440 shl 2) or METHOD_BUFFERED);

//
// These ioctl codes are obsolete.  They are defined here to avoid resuing them
// and to allow class drivers to respond to them more easily.
//

  OBSOLETE_IOCTL_STORAGE_RESET_BUS = (
    (IOCTL_STORAGE_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0400 shl 2) or METHOD_BUFFERED);

  OBSOLETE_IOCTL_STORAGE_RESET_DEVICE = (
    (IOCTL_STORAGE_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0401 shl 2) or METHOD_BUFFERED);

//
// IOCTL_STORAGE_GET_HOTPLUG_INFO
//

type
  PSTORAGE_HOTPLUG_INFO = ^STORAGE_HOTPLUG_INFO;
  _STORAGE_HOTPLUG_INFO = record
    Size: DWORD; // version
    MediaRemovable: BOOLEAN; // ie. zip, jaz, cdrom, mo, etc. vs hdd
    MediaHotplug: BOOLEAN;   // ie. does the device succeed a lock even though its not lockable media?
    DeviceHotplug: BOOLEAN;  // ie. 1394, USB, etc.
    WriteCacheEnableOverride: BOOLEAN; // This field should not be relied upon because it is no longer used
  end;
  STORAGE_HOTPLUG_INFO = _STORAGE_HOTPLUG_INFO;
  TStorageHotplugInfo = STORAGE_HOTPLUG_INFO;
  PStorageHotplugInfo = PSTORAGE_HOTPLUG_INFO;

//
// IOCTL_STORAGE_GET_DEVICE_NUMBER
//
// input - none
//
// output - STORAGE_DEVICE_NUMBER structure
//          The values in the STORAGE_DEVICE_NUMBER structure are guaranteed
//          to remain unchanged until the system is rebooted.  They are not
//          guaranteed to be persistant across boots.
//

type
  PSTORAGE_DEVICE_NUMBER = ^STORAGE_DEVICE_NUMBER;
  _STORAGE_DEVICE_NUMBER = record
    //
    // The FILE_DEVICE_XXX type for this device.
    //
    DeviceType: DEVICE_TYPE;
    //
    // The number of this device
    //
    DeviceNumber: DWORD;
    //
    // If the device is partitionable, the partition number of the device.
    // Otherwise -1
    //
    PartitionNumber: DWORD;
  end;
  STORAGE_DEVICE_NUMBER = _STORAGE_DEVICE_NUMBER;
  TStorageDeviceNumber = STORAGE_DEVICE_NUMBER;
  PStorageDeviceNumber = PSTORAGE_DEVICE_NUMBER;

//
// Define the structures for scsi resets
//

  PSTORAGE_BUS_RESET_REQUEST = ^STORAGE_BUS_RESET_REQUEST;
  _STORAGE_BUS_RESET_REQUEST = record
    PathId: BYTE;
  end;
  STORAGE_BUS_RESET_REQUEST = _STORAGE_BUS_RESET_REQUEST;
  TStorageBusResetRequest = STORAGE_BUS_RESET_REQUEST;
  PStorageBusResetRequest = PSTORAGE_BUS_RESET_REQUEST;

//
// Break reservation is sent to the Adapter/FDO with the given lun information.
//

  STORAGE_BREAK_RESERVATION_REQUEST = record
    Length: DWORD;
    _unused: Byte;
    PathId: Byte;
    TargetId: Byte;
    Lun: Byte;
  end;
  PSTORAGE_BREAK_RESERVATION_REQUEST = ^STORAGE_BREAK_RESERVATION_REQUEST;
  TStorageBreakReservationRequest = STORAGE_BREAK_RESERVATION_REQUEST;
  PStorageBreakReservationRequest = PSTORAGE_BREAK_RESERVATION_REQUEST;  

//
// IOCTL_STORAGE_MEDIA_REMOVAL disables the mechanism
// on a storage device that ejects media. This function
// may or may not be supported on storage devices that
// support removable media.
//
// TRUE means prevent media from being removed.
// FALSE means allow media removal.
//

  PPREVENT_MEDIA_REMOVAL = ^PREVENT_MEDIA_REMOVAL;
  _PREVENT_MEDIA_REMOVAL = record
    PreventMediaRemoval: ByteBool;
  end;
  PREVENT_MEDIA_REMOVAL = _PREVENT_MEDIA_REMOVAL;
  TPreventMediaRemoval = PREVENT_MEDIA_REMOVAL;
  PPreventMediaRemoval = PPREVENT_MEDIA_REMOVAL;

//
//  This is the format of TARGET_DEVICE_CUSTOM_NOTIFICATION.CustomDataBuffer
//  passed to applications by the classpnp autorun code (via IoReportTargetDeviceChangeAsynchronous).
//

  _CLASS_MEDIA_CHANGE_CONTEXT = record
    MediaChangeCount: DWORD;
    NewState: DWORD;		// see MEDIA_CHANGE_DETECTION_STATE enum in classpnp.h in DDK
  end;
  CLASS_MEDIA_CHANGE_CONTEXT = _CLASS_MEDIA_CHANGE_CONTEXT;
  PCLASS_MEDIA_CHANGE_CONTEXT = ^CLASS_MEDIA_CHANGE_CONTEXT;
  TClassMediaChangeContext = CLASS_MEDIA_CHANGE_CONTEXT;
  PClassMediaChangeContext = PCLASS_MEDIA_CHANGE_CONTEXT;

  PTAPE_STATISTICS = ^TAPE_STATISTICS;
  _TAPE_STATISTICS = record
    Version: DWORD;
    Flags: DWORD;
    RecoveredWrites: LARGE_INTEGER;
    UnrecoveredWrites: LARGE_INTEGER;
    RecoveredReads: LARGE_INTEGER;
    UnrecoveredReads: LARGE_INTEGER;
    CompressionRatioReads: BYTE;
    CompressionRatioWrites: BYTE;
  end;
  TAPE_STATISTICS = _TAPE_STATISTICS;
  TTapeStatistics = TAPE_STATISTICS;
  PTapeStatistics = PTAPE_STATISTICS;

const
  RECOVERED_WRITES_VALID       = $00000001;
  UNRECOVERED_WRITES_VALID     = $00000002;
  RECOVERED_READS_VALID        = $00000004;
  UNRECOVERED_READS_VALID      = $00000008;
  WRITE_COMPRESSION_INFO_VALID = $00000010;
  READ_COMPRESSION_INFO_VALID  = $00000020;

type
  PTAPE_GET_STATISTICS = ^TAPE_GET_STATISTICS;
  _TAPE_GET_STATISTICS = record
    Operation: DWORD;
  end;
  TAPE_GET_STATISTICS = _TAPE_GET_STATISTICS;
  TTapeGetStatistics = TAPE_GET_STATISTICS;
  PTapeGetStatistics = PTAPE_GET_STATISTICS;

const
  TAPE_RETURN_STATISTICS = 0;
  TAPE_RETURN_ENV_INFO   = 1;
  TAPE_RESET_STATISTICS  = 2;

//
// IOCTL_STORAGE_GET_MEDIA_TYPES_EX will return an array of DEVICE_MEDIA_INFO
// structures, one per supported type, embedded in the GET_MEDIA_TYPES struct.
//

const

  //
  // Following are defined in ntdddisk.h in the MEDIA_TYPE enum
  //
  // Unknown,                // Format is unknown
  // F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
  // F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
  // F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
  // F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
  // F3_720_512,             // 3.5",  720KB,  512 bytes/sector
  // F5_360_512,             // 5.25", 360KB,  512 bytes/sector
  // F5_320_512,             // 5.25", 320KB,  512 bytes/sector
  // F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
  // F5_180_512,             // 5.25", 180KB,  512 bytes/sector
  // F5_160_512,             // 5.25", 160KB,  512 bytes/sector
  // RemovableMedia,         // Removable media other than floppy
  // FixedMedia,             // Fixed hard disk media
  // F3_120M_512,            // 3.5", 120M Floppy
  // F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
  // F5_640_512,             // 5.25",  640KB,  512 bytes/sector
  // F5_720_512,             // 5.25",  720KB,  512 bytes/sector
  // F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
  // F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
  // F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
  // F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
  // F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
  // F8_256_128,             // 8",     256KB,  128 bytes/sector
  // F3_200Mb_512,           // 3.5",   200M Floppy (HiFD)  
  //

  DDS_4mm            = $20;  // Tape - DAT DDS1,2,... (all vendors)
  MiniQic            = $21;  // Tape - miniQIC Tape
  Travan             = $22;  // Tape - Travan TR-1,2,3,...
  QIC                = $23;  // Tape - QIC
  MP_8mm             = $24;  // Tape - 8mm Exabyte Metal Particle
  AME_8mm            = $25;  // Tape - 8mm Exabyte Advanced Metal Evap
  AIT1_8mm           = $26;  // Tape - 8mm Sony AIT
  DLT                = $27;  // Tape - DLT Compact IIIxt, IV
  NCTP               = $28;  // Tape - Philips NCTP
  IBM_3480           = $29;  // Tape - IBM 3480
  IBM_3490E          = $2A;  // Tape - IBM 3490E
  IBM_Magstar_3590   = $2B;  // Tape - IBM Magstar 3590
  IBM_Magstar_MP     = $2C;  // Tape - IBM Magstar MP
  STK_DATA_D3        = $2D;  // Tape - STK Data D3
  SONY_DTF           = $2E;  // Tape - Sony DTF
  DV_6mm             = $2F;  // Tape - 6mm Digital Video
  DMI                = $30;  // Tape - Exabyte DMI and compatibles
  SONY_D2            = $31;  // Tape - Sony D2S and D2L
  CLEANER_CARTRIDGE  = $32;  // Cleaner - All Drive types that support Drive Cleaners
  CD_ROM             = $33;  // Opt_Disk - CD
  CD_R               = $34;  // Opt_Disk - CD-Recordable (Write Once)
  CD_RW              = $35;  // Opt_Disk - CD-Rewriteable
  DVD_ROM            = $36;  // Opt_Disk - DVD-ROM
  DVD_R              = $37;  // Opt_Disk - DVD-Recordable (Write Once)
  DVD_RW             = $38;  // Opt_Disk - DVD-Rewriteable
  MO_3_RW            = $39;  // Opt_Disk - 3.5" Rewriteable MO Disk
  MO_5_WO            = $3A;  // Opt_Disk - MO 5.25" Write Once
  MO_5_RW            = $3B;  // Opt_Disk - MO 5.25" Rewriteable (not LIMDOW)
  MO_5_LIMDOW        = $3C;  // Opt_Disk - MO 5.25" Rewriteable (LIMDOW)
  PC_5_WO            = $3D;  // Opt_Disk - Phase Change 5.25" Write Once Optical
  PC_5_RW            = $3E;  // Opt_Disk - Phase Change 5.25" Rewriteable
  PD_5_RW            = $3F;  // Opt_Disk - PhaseChange Dual Rewriteable
  ABL_5_WO           = $40;  // Opt_Disk - Ablative 5.25" Write Once Optical
  PINNACLE_APEX_5_RW = $41;  // Opt_Disk - Pinnacle Apex 4.6GB Rewriteable Optical
  SONY_12_WO         = $42;  // Opt_Disk - Sony 12" Write Once
  PHILIPS_12_WO      = $43;  // Opt_Disk - Philips/LMS 12" Write Once
  HITACHI_12_WO      = $44;  // Opt_Disk - Hitachi 12" Write Once
  CYGNET_12_WO       = $45;  // Opt_Disk - Cygnet/ATG 12" Write Once
  KODAK_14_WO        = $46;  // Opt_Disk - Kodak 14" Write Once
  MO_NFR_525         = $47;  // Opt_Disk - Near Field Recording (Terastor)
  NIKON_12_RW        = $48;  // Opt_Disk - Nikon 12" Rewriteable
  IOMEGA_ZIP         = $49;  // Mag_Disk - Iomega Zip
  IOMEGA_JAZ         = $4A;  // Mag_Disk - Iomega Jaz
  SYQUEST_EZ135      = $4B;  // Mag_Disk - Syquest EZ135
  SYQUEST_EZFLYER    = $4C;  // Mag_Disk - Syquest EzFlyer
  SYQUEST_SYJET      = $4D;  // Mag_Disk - Syquest SyJet
  AVATAR_F2          = $4E;  // Mag_Disk - 2.5" Floppy
  MP2_8mm            = $4F;  // Tape - 8mm Hitachi
  DST_S              = $50;  // Ampex DST Small Tapes
  DST_M              = $51;  // Ampex DST Medium Tapes
  DST_L              = $52;  // Ampex DST Large Tapes
  VXATape_1          = $53;  // Ecrix 8mm Tape
  VXATape_2          = $54;  // Ecrix 8mm Tape
  STK_9840           = $55;  // STK 9840
  LTO_Ultrium        = $56;  // IBM, HP, Seagate LTO Ultrium
  LTO_Accelis        = $57;  // IBM, HP, Seagate LTO Accelis
  DVD_RAM            = $58;  // Opt_Disk - DVD-RAM
  AIT_8mm            = $59;  // AIT2 or higher
  ADR_1              = $5A;  // OnStream ADR Mediatypes
  ADR_2              = $5B;
  STK_9940           = $5C;  // STK 9940

type
  STORAGE_MEDIA_TYPE = DWORD;
  PSTORAGE_MEDIA_TYPE = ^STORAGE_MEDIA_TYPE;
  TStorageMediaType = STORAGE_MEDIA_TYPE;
  PStorageMediaType = ^TStorageMediaType;

const
  MEDIA_ERASEABLE  = $00000001;
  MEDIA_WRITE_ONCE = $00000002;
  MEDIA_READ_ONLY  = $00000004;
  MEDIA_READ_WRITE = $00000008;

  MEDIA_WRITE_PROTECTED   = $00000100;
  MEDIA_CURRENTLY_MOUNTED = DWORD($80000000);

//
// Define the different storage bus types
// Bus types below 128 (0x80) are reserved for Microsoft use
//

const
  BusTypeUnknown     = 0;
  BusTypeScsi        = 1;
  BusTypeAtapi       = 2;
  BusTypeAta         = 3;
  BusType1394        = 4;
  BusTypeSsa         = 5;
  BusTypeFibre       = 6;
  BusTypeUsb         = 7;
  BusTypeRAID        = 8;
  BusTypeMaxReserved = $7F;

type
  STORAGE_BUS_TYPE = DWORD;
  PSTORAGE_BUS_TYPE = ^STORAGE_BUS_TYPE;
  TStorageBusType = STORAGE_BUS_TYPE;
  PStorageBusType = PSTORAGE_BUS_TYPE;

  TDMIDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: STORAGE_MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    NumberMediaSides: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
  end;

  TDMIRemovableDiskInfo = record
    Cylinders: LARGE_INTEGER;
    MediaType: STORAGE_MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
    NumberMediaSides: DWORD;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
  end;

  TDMITapeInfo = record
    MediaType: STORAGE_MEDIA_TYPE;
    MediaCharacteristics: DWORD; // Bitmask of MEDIA_XXX values.
    CurrentBlockSize: DWORD;
    BusType: STORAGE_BUS_TYPE;
    //
    // Bus specific information describing the medium supported.
    //
    case Integer of {BusSpecificData}
      0: ( {ScsiInformation}
        MediumType: BYTE;
        DensityCode: BYTE);
  end;

  PDEVICE_MEDIA_INFO = ^DEVICE_MEDIA_INFO;
  _DEVICE_MEDIA_INFO = record
    case Integer of
      0: (DiskInfo: TDMIDiskInfo);
      1: (RemovableDiskInfo: TDMIRemovableDiskInfo);
      2: (TapeInfo: TDMITapeInfo);
  end;
  DEVICE_MEDIA_INFO = _DEVICE_MEDIA_INFO;
  TDeviceMediaInfo = DEVICE_MEDIA_INFO;
  PDeviceMediaInfo = PDEVICE_MEDIA_INFO;

  PGET_MEDIA_TYPES = ^GET_MEDIA_TYPES;
  _GET_MEDIA_TYPES = record
    DeviceType: DWORD; // FILE_DEVICE_XXX values
    MediaInfoCount: DWORD;
    MediaInfo: array [0..0] of DEVICE_MEDIA_INFO;
  end;
  GET_MEDIA_TYPES = _GET_MEDIA_TYPES;
  TGetMediaTypes = GET_MEDIA_TYPES;
  PGetMediaTypes = PGET_MEDIA_TYPES;

//
// IOCTL_STORAGE_PREDICT_FAILURE
//
// input - none
//
// output - STORAGE_PREDICT_FAILURE structure
//          PredictFailure returns zero if no failure predicted and non zero
//                         if a failure is predicted.
//
//          VendorSpecific returns 512 bytes of vendor specific information
//                         if a failure is predicted
//

  PSTORAGE_PREDICT_FAILURE = ^STORAGE_PREDICT_FAILURE;
  _STORAGE_PREDICT_FAILURE = record
    PredictFailure: DWORD;
    VendorSpecific: array [0..511] of BYTE;
  end;
  STORAGE_PREDICT_FAILURE = _STORAGE_PREDICT_FAILURE;
  TStoragePredictFailure = STORAGE_PREDICT_FAILURE;
  PStoragePredictFailure = PSTORAGE_PREDICT_FAILURE;

//
// IoControlCode values for disk devices.
//

const
  IOCTL_DISK_BASE = FILE_DEVICE_DISK;

  IOCTL_DISK_GET_DRIVE_GEOMETRY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0000 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_PARTITION_INFO = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0001 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_PARTITION_INFO = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0002 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0003 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0004 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_VERIFY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0005 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_FORMAT_TRACKS = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0006 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_REASSIGN_BLOCKS = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0007 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_PERFORMANCE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0008 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_IS_WRITABLE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0009 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_LOGGING = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000a shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_FORMAT_TRACKS_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($000b shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_HISTOGRAM_STRUCTURE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000c shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_HISTOGRAM_DATA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000d shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_HISTOGRAM_RESET = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000e shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_REQUEST_STRUCTURE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($000f shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_REQUEST_DATA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0010 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_PERFORMANCE_OFF = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0018 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_CONTROLLER_NUMBER = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0011 shl 2) or METHOD_BUFFERED);

//
// IOCTL support for SMART drive fault prediction.
//

  SMART_GET_VERSION = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0020 shl 2) or METHOD_BUFFERED);

  SMART_SEND_DRIVE_COMMAND = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0021 shl 2) or METHOD_BUFFERED);

  SMART_RCV_DRIVE_DATA = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0022 shl 2) or METHOD_BUFFERED);

//
// New IOCTLs for GUID Partition tabled disks.
//

// 23-11-2002: various bugs reported by Carsten Grafflage corrected

  IOCTL_DISK_GET_PARTITION_INFO_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0012 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_PARTITION_INFO_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0013 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_DRIVE_LAYOUT_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0014 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_DRIVE_LAYOUT_EX = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0015 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_CREATE_DISK = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0016 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_LENGTH_INFO = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0017 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_DRIVE_GEOMETRY_EX = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0028 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_UPDATE_DRIVE_SIZE = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0032 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GROW_PARTITION = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0034 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_CACHE_INFORMATION = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0035 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SET_CACHE_INFORMATION = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0036 shl 2) or METHOD_BUFFERED);

  OBSOLETE_DISK_GET_WRITE_CACHE_STATE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0037 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_DELETE_DRIVE_LAYOUT = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0040 shl 2) or METHOD_BUFFERED);

//
// Called to flush cached information that the driver may have about this
// device's characteristics.  Not all drivers cache characteristics, and not
// cached properties can be flushed.  This simply serves as an update to the
// driver that it may want to do an expensive reexamination of the device's
// characteristics now (fixed media size, partition table, etc...)
//

  IOCTL_DISK_UPDATE_PROPERTIES = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0050 shl 2) or METHOD_BUFFERED);

//
//  Special IOCTLs needed to support PC-98 machines in Japan
//

  IOCTL_DISK_FORMAT_DRIVE = (
    (IOCTL_DISK_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($00f3 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_SENSE_DEVICE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($00f8 shl 2) or METHOD_BUFFERED);

//
// The following device control codes are common for all class drivers.  The
// functions codes defined here must match all of the other class drivers.
//
// Warning: these codes will be replaced in the future by equivalent
// IOCTL_STORAGE codes
//

  IOCTL_DISK_CHECK_VERIFY = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0200 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_MEDIA_REMOVAL = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0201 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_EJECT_MEDIA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0202 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_LOAD_MEDIA = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0203 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_RESERVE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0204 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_RELEASE = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0205 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_FIND_NEW_DEVICES = (
    (IOCTL_DISK_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0206 shl 2) or METHOD_BUFFERED);

  IOCTL_DISK_GET_MEDIA_TYPES = (
    (IOCTL_DISK_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    ($0300 shl 2) or METHOD_BUFFERED);

//
// Define the partition types returnable by known disk drivers.
//

const
  PARTITION_ENTRY_UNUSED    = $00; // Entry unused
  PARTITION_FAT_12          = $01; // 12-bit FAT entries
  PARTITION_XENIX_1         = $02; // Xenix
  PARTITION_XENIX_2         = $03; // Xenix
  PARTITION_FAT_16          = $04; // 16-bit FAT entries
  PARTITION_EXTENDED        = $05; // Extended partition entry
  PARTITION_HUGE            = $06; // Huge partition MS-DOS V4
  PARTITION_IFS             = $07; // IFS Partition
  PARTITION_OS2BOOTMGR      = $0A; // OS/2 Boot Manager/OPUS/Coherent swap
  PARTITION_FAT32           = $0B; // FAT32
  PARTITION_FAT32_XINT13    = $0C; // FAT32 using extended int13 services
  PARTITION_XINT13          = $0E; // Win95 partition using extended int13 services
  PARTITION_XINT13_EXTENDED = $0F; // Same as type 5 but uses extended int13 services
  PARTITION_PREP            = $41; // PowerPC Reference Platform (PReP) Boot Partition
  PARTITION_LDM             = $42; // Logical Disk Manager partition
  PARTITION_UNIX            = $63; // Unix

  VALID_NTFT                = $C0; // NTFT uses high order bits

//
// The high bit of the partition type code indicates that a partition
// is part of an NTFT mirror or striped array.
//

  PARTITION_NTFT = $80; // NTFT partition

//
// The following macro is used to determine which partitions should be
// assigned drive letters.
//

//++
//
// BOOLEAN
// IsRecognizedPartition(
//     IN DWORD PartitionType
//     )
//
// Routine Description:
//
//     This macro is used to determine to which partitions drive letters
//     should be assigned.
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is recognized,
//     otherwise FALSE is returned.
//
//--

function IsRecognizedPartition(PartitionType: DWORD): Boolean;

//++
//
// BOOLEAN
// IsContainerPartition(
//     IN DWORD PartitionType
//     )
//
// Routine Description:
//
//     This macro is used to determine to which partition types are actually
//     containers for other partitions (ie, extended partitions).
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is a container,
//     otherwise FALSE is returned.
//
//--

function IsContainerPartition(PartitionType: DWORD): Boolean;

//++
//
// BOOLEAN
// IsFTPartition(
//     IN DWORD PartitionType
//     )
//
// Routine Description:
//
//     This macro is used to determine if the given partition is an FT
//     partition.
//
// Arguments:
//
//     PartitionType - Supplies the type of the partition being examined.
//
// Return Value:
//
//     The return value is TRUE if the partition type is an FT partition,
//     otherwise FALSE is returned.
//
//--

function IsFTPartition(PartitionType: DWORD): Boolean;

//
// Define the media types supported by the driver.
//

type
  _MEDIA_TYPE = (
    Unknown,                // Format is unknown
    F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
    F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
    F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
    F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
    F3_720_512,             // 3.5",  720KB,  512 bytes/sector
    F5_360_512,             // 5.25", 360KB,  512 bytes/sector
    F5_320_512,             // 5.25", 320KB,  512 bytes/sector
    F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
    F5_180_512,             // 5.25", 180KB,  512 bytes/sector
    F5_160_512,             // 5.25", 160KB,  512 bytes/sector
    RemovableMedia,         // Removable media other than floppy
    FixedMedia,             // Fixed hard disk media
    F3_120M_512,            // 3.5", 120M Floppy
    F3_640_512,             // 3.5" ,  640KB,  512 bytes/sector
    F5_640_512,             // 5.25",  640KB,  512 bytes/sector
    F5_720_512,             // 5.25",  720KB,  512 bytes/sector
    F3_1Pt2_512,            // 3.5" ,  1.2Mb,  512 bytes/sector
    F3_1Pt23_1024,          // 3.5" ,  1.23Mb, 1024 bytes/sector
    F5_1Pt23_1024,          // 5.25",  1.23MB, 1024 bytes/sector
    F3_128Mb_512,           // 3.5" MO 128Mb   512 bytes/sector
    F3_230Mb_512,           // 3.5" MO 230Mb   512 bytes/sector
    F8_256_128,             // 8",     256KB,  128 bytes/sector
    F3_200Mb_512,           // 3.5",   200M Floppy (HiFD)
    F3_240M_512,            // 3.5",   240Mb Floppy (HiFD)
    F3_32M_512);            // 3.5",   32Mb Floppy
  MEDIA_TYPE = _MEDIA_TYPE;
  PMEDIA_TYPE = ^MEDIA_TYPE;
  TMediaType = MEDIA_TYPE;
  PMediaType = PMEDIA_TYPE;

//
// Define the input buffer structure for the driver, when
// it is called with IOCTL_DISK_FORMAT_TRACKS.
//

  PFORMAT_PARAMETERS = ^FORMAT_PARAMETERS;
  _FORMAT_PARAMETERS = record
    MediaType: MEDIA_TYPE;
    StartCylinderNumber: DWORD;
    EndCylinderNumber: DWORD;
    StartHeadNumber: DWORD;
    EndHeadNumber: DWORD;
  end;
  FORMAT_PARAMETERS = _FORMAT_PARAMETERS;
  TFormatParameters = FORMAT_PARAMETERS;
  PFormatParameters = PFORMAT_PARAMETERS;

//
// Define the BAD_TRACK_NUMBER type. An array of elements of this type is
// returned by the driver on IOCTL_DISK_FORMAT_TRACKS requests, to indicate
// what tracks were bad during formatting. The length of that array is
// reported in the `Information' field of the I/O Status Block.
//

  BAD_TRACK_NUMBER = WORD;
  PBAD_TRACK_NUMBER = ^WORD;

//
// Define the input buffer structure for the driver, when
// it is called with IOCTL_DISK_FORMAT_TRACKS_EX.
//

  PFORMAT_EX_PARAMETERS = ^FORMAT_EX_PARAMETERS;
  _FORMAT_EX_PARAMETERS = record
    MediaType: MEDIA_TYPE;
    StartCylinderNumber: DWORD;
    EndCylinderNumber: DWORD;
    StartHeadNumber: DWORD;
    EndHeadNumber: DWORD;
    FormatGapLength: WORD;
    SectorsPerTrack: WORD;
    SectorNumber: array [0..0] of WORD;
  end;
  FORMAT_EX_PARAMETERS = _FORMAT_EX_PARAMETERS;
  TFormatExParameters = FORMAT_EX_PARAMETERS;
  PFormatExParameters = PFORMAT_EX_PARAMETERS;

//
// The following structure is returned on an IOCTL_DISK_GET_DRIVE_GEOMETRY
// request and an array of them is returned on an IOCTL_DISK_GET_MEDIA_TYPES
// request.
//

  PDISK_GEOMETRY = ^DISK_GEOMETRY;
  _DISK_GEOMETRY = record
    Cylinders: LARGE_INTEGER;
    MediaType: MEDIA_TYPE;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;
  DISK_GEOMETRY = _DISK_GEOMETRY;
  TDiskGeometry = DISK_GEOMETRY;
  PDiskGeometry = PDISK_GEOMETRY;

//
// This wmi guid returns a DISK_GEOMETRY structure
//

const
  WMI_DISK_GEOMETRY_GUID: TGUID = (
    D1:$25007f51; D2:$57c2; D3:$11d1; D4:($a5, $28, $00, $a0, $c9, $06, $29, $10));

//
// The following structure is returned on an IOCTL_DISK_GET_PARTITION_INFO
// and an IOCTL_DISK_GET_DRIVE_LAYOUT request.  It is also used in a request
// to change the drive layout, IOCTL_DISK_SET_DRIVE_LAYOUT.
//

type
  PPARTITION_INFORMATION = ^PARTITION_INFORMATION;
  _PARTITION_INFORMATION = record
    StartingOffset: LARGE_INTEGER;
    PartitionLength: LARGE_INTEGER;
    HiddenSectors: DWORD;
    PartitionNumber: DWORD;
    PartitionType: BYTE;
    BootIndicator: ByteBool;
    RecognizedPartition: ByteBool;
    RewritePartition: ByteBool;
  end;
  PARTITION_INFORMATION = _PARTITION_INFORMATION;
  TPartitionInformation = PARTITION_INFORMATION;
  PPartitionInformation = PPARTITION_INFORMATION;

//
// The following structure is used to change the partition type of a
// specified disk partition using an IOCTL_DISK_SET_PARTITION_INFO
// request.
//

  PSET_PARTITION_INFORMATION = ^SET_PARTITION_INFORMATION;
  _SET_PARTITION_INFORMATION = record
    PartitionType: BYTE;
  end;
  SET_PARTITION_INFORMATION = _SET_PARTITION_INFORMATION;
  TSetPartitionInformation = _SET_PARTITION_INFORMATION;
  PSetPartitionInformation = PSET_PARTITION_INFORMATION;

//
// The following structures is returned on an IOCTL_DISK_GET_DRIVE_LAYOUT
// request and given as input to an IOCTL_DISK_SET_DRIVE_LAYOUT request.
//

  PDRIVE_LAYOUT_INFORMATION = ^DRIVE_LAYOUT_INFORMATION;
  _DRIVE_LAYOUT_INFORMATION = record
    PartitionCount: DWORD;
    Signature: DWORD;
    PartitionEntry: array [0..0] of PARTITION_INFORMATION;
  end;
  DRIVE_LAYOUT_INFORMATION = _DRIVE_LAYOUT_INFORMATION;
  TDriveLayoutInformation = DRIVE_LAYOUT_INFORMATION;
  PDriveLayoutInformation = PDRIVE_LAYOUT_INFORMATION;

//
// The following structure is passed in on an IOCTL_DISK_VERIFY request.
// The offset and length parameters are both given in bytes.
//

  PVERIFY_INFORMATION = ^VERIFY_INFORMATION;
  _VERIFY_INFORMATION = record
    StartingOffset: LARGE_INTEGER;
    Length: DWORD;
  end;
  VERIFY_INFORMATION = _VERIFY_INFORMATION;
  TVerifyInformation = VERIFY_INFORMATION;
  PVerifyInformation = PVERIFY_INFORMATION;

//
// The following structure is passed in on an IOCTL_DISK_REASSIGN_BLOCKS
// request.
//

  PREASSIGN_BLOCKS = ^REASSIGN_BLOCKS;
  _REASSIGN_BLOCKS = record
    Reserved: WORD;
    Count: WORD;
    BlockNumber: array [0..0] of DWORD;
  end;
  REASSIGN_BLOCKS = _REASSIGN_BLOCKS;
  TReassignBlocks = REASSIGN_BLOCKS;
  PReassignBlocks = PREASSIGN_BLOCKS;

//
// Support for GUID Partition Table (GPT) disks.
//

//
// There are currently two ways a disk can be partitioned. With a traditional
// AT-style master boot record (PARTITION_STYLE_MBR) and with a new, GPT
// partition table (PARTITION_STYLE_GPT). RAW is for an unrecognizable
// partition style. There are a very limited number of things you can
// do with a RAW partititon.
//

type
  _PARTITION_STYLE = (
    PARTITION_STYLE_MBR,
    PARTITION_STYLE_GPT,
    PARTITION_STYLE_RAW);
  PARTITION_STYLE = _PARTITION_STYLE;
  TPartitionStyle = PARTITION_STYLE;

//
// The following structure defines information in a GPT partition that is
// not common to both GPT and MBR partitions.
//

  PPARTITION_INFORMATION_GPT = ^PARTITION_INFORMATION_GPT;
  _PARTITION_INFORMATION_GPT = record
    PartitionType: GUID; // Partition type. See table 16-3.
    PartitionId: GUID; // Unique GUID for this partition.
    Attributes: DWORD64; // See table 16-4.
    Name: array [0..35] of WCHAR; // Partition Name in Unicode.
  end;
  PARTITION_INFORMATION_GPT = _PARTITION_INFORMATION_GPT;
  TPartitionInformationGpt = PARTITION_INFORMATION_GPT;
  PPartitionInformationGpt = PPARTITION_INFORMATION_GPT;

//
//  The following are GPT partition attributes applicable for any
//  partition type. These attributes are not OS-specific
//

const
  GPT_ATTRIBUTE_PLATFORM_REQUIRED = ($0000000000000001);

//
// The following are GPT partition attributes applicable when the
// PartitionType is PARTITION_BASIC_DATA_GUID.
//

  GPT_BASIC_DATA_ATTRIBUTE_NO_DRIVE_LETTER = DWORD($8000000000000000);
  GPT_BASIC_DATA_ATTRIBUTE_HIDDEN          = ($4000000000000000);
  GPT_BASIC_DATA_ATTRIBUTE_READ_ONLY       = ($1000000000000000);

//
// The following structure defines information in an MBR partition that is not
// common to both GPT and MBR partitions.
//

type
  PPARTITION_INFORMATION_MBR = ^PARTITION_INFORMATION_MBR;
  _PARTITION_INFORMATION_MBR = record
    PartitionType: BYTE;
    BootIndicator: BOOLEAN;
    RecognizedPartition: BOOLEAN;
    HiddenSectors: DWORD;
  end;
  PARTITION_INFORMATION_MBR = _PARTITION_INFORMATION_MBR;
  TPartitionInformationMbr = PARTITION_INFORMATION_MBR;
  PPartitionInformationMbr = PPARTITION_INFORMATION_MBR;

//
// The structure SET_PARTITION_INFO_EX is used with the ioctl
// IOCTL_SET_PARTITION_INFO_EX to set information about a specific
// partition. Note that for MBR partitions, you can only set the partition
// signature, whereas GPT partitions allow setting of all fields that
// you can get.
//

  SET_PARTITION_INFORMATION_MBR = SET_PARTITION_INFORMATION;
  TSetPartitionInformationMbr = SET_PARTITION_INFORMATION_MBR;
  SET_PARTITION_INFORMATION_GPT = PARTITION_INFORMATION_GPT;
  TSetPartitionInformationGpt = SET_PARTITION_INFORMATION_GPT;

  PSET_PARTITION_INFORMATION_EX = ^SET_PARTITION_INFORMATION_EX;
  _SET_PARTITION_INFORMATION_EX = record
    PartitionStyle: PARTITION_STYLE;
    case Integer of
      0: (Mbr: SET_PARTITION_INFORMATION_MBR);
      1: (Gpt: SET_PARTITION_INFORMATION_GPT);
  end;
  SET_PARTITION_INFORMATION_EX = _SET_PARTITION_INFORMATION_EX;
  TSetPartitionInformationEx = SET_PARTITION_INFORMATION_EX;
  PSetPartitionInformationEx = PSET_PARTITION_INFORMATION_EX;

//
// The structure CREATE_DISK_GPT with the ioctl IOCTL_DISK_CREATE_DISK
// to initialize an virgin disk with an empty GPT partition table.
//

  PCREATE_DISK_GPT = ^CREATE_DISK_GPT;
  _CREATE_DISK_GPT = record
    DiskId: GUID; // Unique disk id for the disk.
    MaxPartitionCount: DWORD; // Maximim number of partitions allowable.
  end;
  CREATE_DISK_GPT = _CREATE_DISK_GPT;
  TCreateDiskGpt = CREATE_DISK_GPT;
  PCreateDiskGpt = PCREATE_DISK_GPT;

//
// The structure CREATE_DISK_MBR with the ioctl IOCTL_DISK_CREATE_DISK
// to initialize an virgin disk with an empty MBR partition table.
//

  PCREATE_DISK_MBR = ^CREATE_DISK_MBR;
  _CREATE_DISK_MBR = record
    Signature: DWORD;
  end;
  CREATE_DISK_MBR = _CREATE_DISK_MBR;
  TCreateDiskMbr = CREATE_DISK_MBR;
  PCreateDiskMbr = PCREATE_DISK_MBR;

  PCREATE_DISK = ^CREATE_DISK;
  _CREATE_DISK = record
    PartitionStyle: PARTITION_STYLE;
    case Integer of
      0: (Mbr: CREATE_DISK_MBR);
      1: (Gpt: CREATE_DISK_GPT);
  end;
  CREATE_DISK = _CREATE_DISK;
  TCreateDisk = CREATE_DISK;
  PCreateDisk = PCREATE_DISK;

//
// The structure GET_LENGTH_INFORMATION is used with the ioctl
// IOCTL_DISK_GET_LENGTH_INFO to obtain the length, in bytes, of the
// disk, partition, or volume.
//

  PGET_LENGTH_INFORMATION = ^GET_LENGTH_INFORMATION;
  _GET_LENGTH_INFORMATION = record
    Length: LARGE_INTEGER;
  end;
  GET_LENGTH_INFORMATION = _GET_LENGTH_INFORMATION;
  TGetLengthInformation = GET_LENGTH_INFORMATION;
  PGetLengthInformation = PGET_LENGTH_INFORMATION;

//
// The PARTITION_INFORMATION_EX structure is used with the
// IOCTL_DISK_GET_DRIVE_LAYOUT_EX, IOCTL_DISK_SET_DRIVE_LAYOUT_EX,
// IOCTL_DISK_GET_PARTITION_INFO_EX and IOCTL_DISK_GET_PARTITION_INFO_EX calls.
//

  PPARTITION_INFORMATION_EX = ^PARTITION_INFORMATION_EX;
  _PARTITION_INFORMATION_EX = record
    PartitionStyle: PARTITION_STYLE;
    StartingOffset: LARGE_INTEGER;
    PartitionLength: LARGE_INTEGER;
    PartitionNumber: DWORD;
    RewritePartition: BOOLEAN;
    case Integer of
      0: (Mbr: PARTITION_INFORMATION_MBR);
      1: (Gpt: PARTITION_INFORMATION_GPT);
  end;
  PARTITION_INFORMATION_EX = _PARTITION_INFORMATION_EX;
  TPartitionInformationEx = PARTITION_INFORMATION_EX;
  PPartitionInformationEx = PPARTITION_INFORMATION_EX;

//
// GPT specific drive layout information.
//

  PDRIVE_LAYOUT_INFORMATION_GPT = ^DRIVE_LAYOUT_INFORMATION_GPT;
  _DRIVE_LAYOUT_INFORMATION_GPT = record
    DiskId: GUID;
    StartingUsableOffset: LARGE_INTEGER;
    UsableLength: LARGE_INTEGER;
    MaxPartitionCount: DWORD;
  end;
  DRIVE_LAYOUT_INFORMATION_GPT = _DRIVE_LAYOUT_INFORMATION_GPT;
  TDriveLayoutInformationGpt = DRIVE_LAYOUT_INFORMATION_GPT;
  PDriveLayoutInformationGpt = PDRIVE_LAYOUT_INFORMATION_GPT;

//
// MBR specific drive layout information.
//

  PDRIVE_LAYOUT_INFORMATION_MBR = ^DRIVE_LAYOUT_INFORMATION_MBR;
  _DRIVE_LAYOUT_INFORMATION_MBR = record
    Signature: DWORD;
  end;
  DRIVE_LAYOUT_INFORMATION_MBR = _DRIVE_LAYOUT_INFORMATION_MBR;
  TDriveLayoutInformationMbr = DRIVE_LAYOUT_INFORMATION_MBR;
  PDriveLayoutInformationMbr = PDRIVE_LAYOUT_INFORMATION_MBR;

//
// The structure DRIVE_LAYOUT_INFORMATION_EX is used with the
// IOCTL_SET_DRIVE_LAYOUT_EX and IOCTL_GET_DRIVE_LAYOUT_EX calls.
//

  PDRIVE_LAYOUT_INFORMATION_EX = ^DRIVE_LAYOUT_INFORMATION_EX;
  _DRIVE_LAYOUT_INFORMATION_EX = record
    PartitionStyle: DWORD;
    PartitionCount: DWORD;
    Union: record
      case Integer of
        0: (Mbr: DRIVE_LAYOUT_INFORMATION_MBR);
        1: (Gpt: DRIVE_LAYOUT_INFORMATION_GPT);
    end;
    PartitionEntry: array [0..0] of PARTITION_INFORMATION_EX;
  end;
  DRIVE_LAYOUT_INFORMATION_EX = _DRIVE_LAYOUT_INFORMATION_EX;
  TDriveLayoutInformationEx = DRIVE_LAYOUT_INFORMATION_EX;
  PDriveLayoutInformationEx = PDRIVE_LAYOUT_INFORMATION_EX;

//
// The DISK_GEOMETRY_EX structure is returned on issuing an
// IOCTL_DISK_GET_DRIVE_GEOMETRY_EX ioctl.
//

  _DETECTION_TYPE = (
    DetectNone,
    DetectInt13,
    DetectExInt13);
  DETECTION_TYPE = _DETECTION_TYPE;
  TDetectionType = DETECTION_TYPE;

  PDISK_INT13_INFO = ^DISK_INT13_INFO;
  _DISK_INT13_INFO = record
    DriveSelect: WORD;
    MaxCylinders: DWORD;
    SectorsPerTrack: WORD;
    MaxHeads: WORD;
    NumberDrives: WORD;
  end;
  DISK_INT13_INFO = _DISK_INT13_INFO;
  TDiskInt13Info = DISK_INT13_INFO;
  PDiskInt13Info = PDISK_INT13_INFO;

  PDISK_EX_INT13_INFO = ^DISK_EX_INT13_INFO;
  _DISK_EX_INT13_INFO = record
    ExBufferSize: WORD;
    ExFlags: WORD;
    ExCylinders: DWORD;
    ExHeads: DWORD;
    ExSectorsPerTrack: DWORD;
    ExSectorsPerDrive: DWORD64;
    ExSectorSize: WORD;
    ExReserved: WORD;
  end;
  DISK_EX_INT13_INFO = _DISK_EX_INT13_INFO;
  TDiskExInt13Info = DISK_EX_INT13_INFO;
  PDiskExInt13Info = PDISK_EX_INT13_INFO;

  PDISK_DETECTION_INFO = ^DISK_DETECTION_INFO;
  _DISK_DETECTION_INFO = record
    SizeOfDetectInfo: DWORD;
    DetectionType: DETECTION_TYPE;
    case Integer of
      0: (
        //
        // If DetectionType == DETECTION_INT13 then we have just the Int13
        // information.
        //
        Int13: DISK_INT13_INFO;
        //
        // If DetectionType == DETECTION_EX_INT13, then we have the
        // extended int 13 information.
        //
        ExInt13: DISK_EX_INT13_INFO); // If DetectionType == DetectExInt13
  end;
  DISK_DETECTION_INFO = _DISK_DETECTION_INFO;
  TDiskDetectionInfo = DISK_DETECTION_INFO;
  PDiskDetectionInfo = PDISK_DETECTION_INFO;

  PDISK_PARTITION_INFO = ^DISK_PARTITION_INFO;
  _DISK_PARTITION_INFO = record
    SizeOfPartitionInfo: DWORD;
    PartitionStyle: PARTITION_STYLE; // PartitionStyle = RAW, GPT or MBR
    case Integer of
      0: (                           // If PartitionStyle == MBR
        Signature: DWORD; // MBR Signature
        CheckSum: DWORD); // MBR CheckSum
      1: (                           // If PartitionStyle == GPT
        DiskId: GUID);
  end;
  DISK_PARTITION_INFO = _DISK_PARTITION_INFO;
  TDiskPartitionInfo = DISK_PARTITION_INFO;
  PDiskPartitionInfo = PDISK_PARTITION_INFO;

//
// The Geometry structure is a variable length structure composed of a
// DISK_GEOMETRY_EX structure followed by a DISK_PARTITION_INFO structure
// followed by a DISK_DETECTION_DATA structure.
//

function DiskGeometryGetPartition(Geometry: DWORD): PDISK_PARTITION_INFO;

function DiskGeometryGetDetect(Geometry: DWORD): PDISK_DETECTION_INFO;

type
  PDISK_GEOMETRY_EX = ^DISK_GEOMETRY_EX;
  _DISK_GEOMETRY_EX = record
    Geometry: DISK_GEOMETRY;    // Standard disk geometry: may be faked by driver.
    DiskSize: LARGE_INTEGER;    // Must always be correct
    Data: array [0..0] of BYTE; // Partition, Detect info
  end;
  DISK_GEOMETRY_EX = _DISK_GEOMETRY_EX;
  TDiskGeometryEx = DISK_GEOMETRY_EX;
  PDiskGeometryEx = PDISK_GEOMETRY_EX;

//
// IOCTL_DISK_CONTROLLER_NUMBER returns the controller and disk
// number for the handle.  This is used to determine if a disk
// is attached to the primary or secondary IDE controller.
//

  PDISK_CONTROLLER_NUMBER = ^DISK_CONTROLLER_NUMBER;
  _DISK_CONTROLLER_NUMBER = record
    ControllerNumber: DWORD;
    DiskNumber: DWORD;
  end;
  DISK_CONTROLLER_NUMBER = _DISK_CONTROLLER_NUMBER;
  TDiskControllerNumber = DISK_CONTROLLER_NUMBER;
  PDiskControllerNumber = PDISK_CONTROLLER_NUMBER;

//
// IOCTL_DISK_SET_CACHE allows the caller to get or set the state of the disk
// read/write caches.
//
// If the structure is provided as the input buffer for the ioctl the read &
// write caches will be enabled or disabled depending on the parameters
// provided.
//
// If the structure is provided as an output buffer for the ioctl the state
// of the read & write caches will be returned. If both input and outut buffers
// are provided the output buffer will contain the cache state BEFORE any
// changes are made
//

  DISK_CACHE_RETENTION_PRIORITY = (EqualPriority, KeepPrefetchedData, KeepReadData);
  TDiskCacheRetentionPriority = DISK_CACHE_RETENTION_PRIORITY;

  TDCIScalarPrefetch = record
    Minimum: WORD;
    Maximum: WORD;
    //
    // The maximum number of blocks which will be prefetched - useful
    // with the scalar limits to set definite upper limits.
    //
    MaximumBlocks: WORD;
  end;

  TDCIBlockPrefetch = record
    Minimum: WORD;
    Maximum: WORD;
  end;

  PDISK_CACHE_INFORMATION = ^DISK_CACHE_INFORMATION;
  _DISK_CACHE_INFORMATION = record

    //
    // on return indicates that the device is capable of saving any parameters
    // in non-volatile storage.  On send indicates that the device should
    // save the state in non-volatile storage.
    //

    ParametersSavable: ByteBool;

    //
    // Indicates whether the write and read caches are enabled.
    //

    ReadCacheEnabled: ByteBool;
    WriteCacheEnabled: ByteBool;

    //
    // Controls the likelyhood of data remaining in the cache depending on how
    // it got there.  Data cached from a READ or WRITE operation may be given
    // higher, lower or equal priority to data entered into the cache for other
    // means (like prefetch)
    //

    ReadRetentionPriority: DISK_CACHE_RETENTION_PRIORITY;
    WriteRetentionPriority: DISK_CACHE_RETENTION_PRIORITY;

    //
    // Requests for a larger number of blocks than this may have prefetching
    // disabled.  If this value is set to 0 prefetch will be disabled.
    //

    DisablePrefetchTransferLength: WORD;

    //
    // If TRUE then ScalarPrefetch (below) will be valid.  If FALSE then
    // the minimum and maximum values should be treated as a block count
    // (BlockPrefetch)
    //

    PrefetchScalar: ByteBool;

    //
    // Contains the minimum and maximum amount of data which will be
    // will be prefetched into the cache on a disk operation.  This value
    // may either be a scalar multiplier of the transfer length of the request,
    // or an abolute number of disk blocks.  PrefetchScalar (above) indicates
    // which interpretation is used.
    //

    case Integer of
      0: (ScalarPrefetch: TDCIScalarPrefetch);
      1: (BlockPrefetch: TDCIBlockPrefetch);

  end;
  DISK_CACHE_INFORMATION = _DISK_CACHE_INFORMATION;
  TDiskCacheInformation = DISK_CACHE_INFORMATION;
  PDiskCacheInformation = PDISK_CACHE_INFORMATION;

//
// IOCTL_DISK_GROW_PARTITION will update the size of a partition
// by adding sectors to the length. The number of sectors must be
// predetermined by examining PARTITION_INFORMATION.
//

  PDISK_GROW_PARTITION = ^DISK_GROW_PARTITION;
  _DISK_GROW_PARTITION = record
    PartitionNumber: DWORD;
    BytesToGrow: LARGE_INTEGER;
  end;
  DISK_GROW_PARTITION = _DISK_GROW_PARTITION;
  TDiskGrowPartition = DISK_GROW_PARTITION;
  PDiskGrowPartition = PDISK_GROW_PARTITION;

///////////////////////////////////////////////////////
//                                                   //
// The following structures define disk performance  //
// statistics: specifically the locations of all the //
// reads and writes which have occured on the disk.  //
//                                                   //
// To use these structures, you must issue an IOCTL_ //
// DISK_HIST_STRUCTURE (with a DISK_HISTOGRAM) to    //
// obtain the basic histogram information. The       //
// number of buckets which must allocated is part of //
// this structure. Allocate the required number of   //
// buckets and call an IOCTL_DISK_HIST_DATA to fill  //
// in the data                                       //
//                                                   //
///////////////////////////////////////////////////////

const
  HIST_NO_OF_BUCKETS = 24;

type
  PHISTOGRAM_BUCKET = ^HISTOGRAM_BUCKET;
  _HISTOGRAM_BUCKET = record
    Reads: DWORD;
    Writes: DWORD;
  end;
  HISTOGRAM_BUCKET = _HISTOGRAM_BUCKET;
  THistogramBucket = HISTOGRAM_BUCKET;
  PHistogramBucket = PHISTOGRAM_BUCKET;

const
  HISTOGRAM_BUCKET_SIZE = SizeOf(HISTOGRAM_BUCKET);

type
  PDISK_HISTOGRAM = ^DISK_HISTOGRAM;
  _DISK_HISTOGRAM = record
    DiskSize: LARGE_INTEGER;
    Start: LARGE_INTEGER;
    End_: LARGE_INTEGER;
    Average: LARGE_INTEGER;
    AverageRead: LARGE_INTEGER;
    AverageWrite: LARGE_INTEGER;
    Granularity: DWORD;
    Size: DWORD;
    ReadCount: DWORD;
    WriteCount: DWORD;
    Histogram: PHISTOGRAM_BUCKET;
  end;
  DISK_HISTOGRAM = _DISK_HISTOGRAM;
  TDiskHistogram = DISK_HISTOGRAM;
  PDiskHistogram = PDISK_HISTOGRAM;

const
  DISK_HISTOGRAM_SIZE = SizeOf(DISK_HISTOGRAM);

///////////////////////////////////////////////////////
//                                                   //
// The following structures define disk debugging    //
// capabilities. The IOCTLs are directed to one of   //
// the two disk filter drivers.                      //
//                                                   //
// DISKPERF is a utilty for collecting disk request  //
// statistics.                                       //
//                                                   //
// SIMBAD is a utility for injecting faults in       //
// IO requests to disks.                             //
//                                                   //
///////////////////////////////////////////////////////

//
// The following structure is exchanged on an IOCTL_DISK_GET_PERFORMANCE
// request. This ioctl collects summary disk request statistics used
// in measuring performance.
//

type
  PDISK_PERFORMANCE = ^DISK_PERFORMANCE;
  _DISK_PERFORMANCE = record
    BytesRead: LARGE_INTEGER;
    BytesWritten: LARGE_INTEGER;
    ReadTime: LARGE_INTEGER;
    WriteTime: LARGE_INTEGER;
    IdleTime: LARGE_INTEGER;
    ReadCount: DWORD;
    WriteCount: DWORD;
    QueueDepth: DWORD;
    SplitCount: DWORD;
    QueryTime: LARGE_INTEGER;
    StorageDeviceNumber: DWORD;
    StorageManagerName: array [0..7] of WCHAR;
  end;
  DISK_PERFORMANCE = _DISK_PERFORMANCE;
  TDiskPerformance = DISK_PERFORMANCE;
  PDiskPerformance = PDISK_PERFORMANCE;

//
// This structure defines the disk logging record. When disk logging
// is enabled, one of these is written to an internal buffer for each
// disk request.
//

  PDISK_RECORD = ^DISK_RECORD;
  _DISK_RECORD = record
    ByteOffset: LARGE_INTEGER;
    StartTime: LARGE_INTEGER;
    EndTime: LARGE_INTEGER;
    VirtualAddress: PVOID;
    NumberOfBytes: DWORD;
    DeviceNumber: BYTE;
    ReadRequest: ByteBool;
  end;
  DISK_RECORD = _DISK_RECORD;
  TDiskRecord = DISK_RECORD;
  PDiskRecord = PDISK_RECORD;

//
// The following structure is exchanged on an IOCTL_DISK_LOG request.
// Not all fields are valid with each function type.
//

  PDISK_LOGGING = ^DISK_LOGGING;
  _DISK_LOGGING = record
    Function_: BYTE;
    BufferAddress: PVOID;
    BufferSize: DWORD;
  end;
  DISK_LOGGING = _DISK_LOGGING;
  TDiskLogging = DISK_LOGGING;
  PDiskLogging = PDISK_LOGGING;

//
// Disk logging functions
//
// Start disk logging. Only the Function and BufferSize fields are valid.
//

const
  DISK_LOGGING_START = 0;

//
// Stop disk logging. Only the Function field is valid.
//

  DISK_LOGGING_STOP = 1;

//
// Return disk log. All fields are valid. Data will be copied from internal
// buffer to buffer specified for the number of bytes requested.
//

  DISK_LOGGING_DUMP = 2;

//
// DISK BINNING
//
// DISKPERF will keep counters for IO that falls in each of these ranges.
// The application determines the number and size of the ranges.
// Joe Lin wanted me to keep it flexible as possible, for instance, IO
// sizes are interesting in ranges like 0-4096, 4097-16384, 16385-65536, 65537+.
//

  DISK_BINNING = 3;

//
// Bin types
//

type
  _BIN_TYPES = (RequestSize, RequestLocation);
  BIN_TYPES = _BIN_TYPES;
  TBinTypes = _BIN_TYPES;

//
// Bin ranges
//

  PBIN_RANGE = ^BIN_RANGE;
  _BIN_RANGE = record
    StartValue: LARGE_INTEGER;
    Length: LARGE_INTEGER;
  end;
  BIN_RANGE = _BIN_RANGE;
  TBinRange = BIN_RANGE;
  PBinRange = PBIN_RANGE;

//
// Bin definition
//

  PPERF_BIN = ^PERF_BIN;
  _PERF_BIN = record
    NumberOfBins: DWORD;
    TypeOfBin: DWORD;
    BinsRanges: array [0..0] of BIN_RANGE;
  end;
  PERF_BIN = _PERF_BIN;
  TPerfBin = PERF_BIN;
  PPerfBin = PPERF_BIN;

//
// Bin count
//

  PBIN_COUNT = ^BIN_COUNT;
  _BIN_COUNT = record
    BinRange: BIN_RANGE;
    BinCount: DWORD;
  end;
  BIN_COUNT = _BIN_COUNT;
  TBinCount = BIN_COUNT;
  PBinCount = PBIN_COUNT;

//
// Bin results
//

  PBIN_RESULTS = ^BIN_RESULTS;
  _BIN_RESULTS = record
    NumberOfBins: DWORD;
    BinCounts: array [0..0] of BIN_COUNT;
  end;
  BIN_RESULTS = _BIN_RESULTS;
  TBinResults = BIN_RESULTS;
  PBinResults = PBIN_RESULTS;

//
// Data structures for SMART drive fault prediction.
//
// GETVERSIONINPARAMS contains the data returned from the
// Get Driver Version function.
//

//#include <pshpack1.h>

  LPGETVERSIONINPARAMS = ^GETVERSIONINPARAMS;
  _GETVERSIONINPARAMS = packed record
    bVersion: BYTE;                    // Binary driver version.
    bRevision: BYTE;                   // Binary driver revision.
    bReserved: BYTE;                   // Not used.
    bIDEDeviceMap: BYTE;               // Bit map of IDE devices.
    fCapabilities: DWORD;              // Bit mask of driver capabilities.
    dwReserved: array [0..3] of DWORD; // For future use.
  end;
  GETVERSIONINPARAMS = _GETVERSIONINPARAMS;
  TGetVersionInParams = GETVERSIONINPARAMS;
  PGetVersionInParams = LPGETVERSIONINPARAMS;

//#include <poppack.h>

//
// Bits returned in the fCapabilities member of GETVERSIONINPARAMS
//

const
  CAP_ATA_ID_CMD   = 1;         // ATA ID command supported
  CAP_ATAPI_ID_CMD = 2;         // ATAPI ID command supported
  CAP_SMART_CMD    = 4;         // SMART commannds supported

//
// IDE registers
//

//#include <pshpack1.h>

type
  LPIDEREGS = ^IDEREGS;
  _IDEREGS = packed record
    bFeaturesReg: BYTE;     // Used for specifying SMART "commands".
    bSectorCountReg: BYTE;  // IDE sector count register
    bSectorNumberReg: BYTE; // IDE sector number register
    bCylLowReg: BYTE;       // IDE low order cylinder value
    bCylHighReg: BYTE;      // IDE high order cylinder value
    bDriveHeadReg: BYTE;    // IDE drive/head register
    bCommandReg: BYTE;      // Actual IDE command.
    bReserved: BYTE;        // reserved for future use.  Must be zero.
  end;
  IDEREGS = _IDEREGS;
  TIdeRegs = IDEREGS;
  PIdeRegs = LPIDEREGS;

//#include <poppack.h>

//
// Valid values for the bCommandReg member of IDEREGS.
//

const
  ATAPI_ID_CMD = $A1;       // Returns ID sector for ATAPI.
  ID_CMD       = $EC;       // Returns ID sector for ATA.
  SMART_CMD    = $B0;       // Performs SMART cmd.
                            // Requires valid bFeaturesReg,
                            // bCylLowReg, and bCylHighReg

//
// Cylinder register defines for SMART command
//

  SMART_CYL_LOW = $4F;
  SMART_CYL_HI  = $C2;


//
// SENDCMDINPARAMS contains the input parameters for the
// Send Command to Drive function.
//

//#include <pshpack1.h>

type
  LPSENDCMDINPARAMS = ^SENDCMDINPARAMS;
  _SENDCMDINPARAMS = packed record
    cBufferSize: DWORD;   // Buffer size in bytes
    irDriveRegs: IDEREGS; // Structure with drive register values.
    bDriveNumber: BYTE;   // Physical drive number to send
                          // command to (0,1,2,3).
    bReserved: array [0..2] of BYTE;   // Reserved for future expansion.
    dwReserved: array [0..3] of DWORD; // For future use.
    bBuffer: array [0..0] of BYTE;     // Input buffer.
  end;
  SENDCMDINPARAMS = _SENDCMDINPARAMS;
  TSendCmdInParams = SENDCMDINPARAMS;
  PSendCmdInParams = LPSENDCMDINPARAMS;

//#include <poppack.h>

//
// Status returned from driver
//

//#include <pshpack1.h>

  LPDRIVERSTATUS = ^DRIVERSTATUS;
  _DRIVERSTATUS = packed record
    bDriverError: BYTE; // Error code from driver,
                        // or 0 if no error.
    bIDEError: BYTE;    // Contents of IDE Error register.
                        // Only valid when bDriverError
                        // is SMART_IDE_ERROR.
    bReserved: array [0..1] of BYTE;   // Reserved for future expansion.
    dwReserved: array [0..1] of DWORD; // Reserved for future expansion.
  end;
  DRIVERSTATUS = _DRIVERSTATUS;
  TDriverStatus = DRIVERSTATUS;
  PDriverStatus = LPDRIVERSTATUS;

//#include <poppack.h>

//
// bDriverError values
//

const
  SMART_NO_ERROR         = 0; // No error
  SMART_IDE_ERROR        = 1; // Error from IDE controller
  SMART_INVALID_FLAG     = 2; // Invalid command flag
  SMART_INVALID_COMMAND  = 3; // Invalid command byte
  SMART_INVALID_BUFFER   = 4; // Bad buffer (null, invalid addr..)
  SMART_INVALID_DRIVE    = 5; // Drive number not valid
  SMART_INVALID_IOCTL    = 6; // Invalid IOCTL
  SMART_ERROR_NO_MEM     = 7; // Could not lock user's buffer
  SMART_INVALID_REGISTER = 8; // Some IDE Register not valid
  SMART_NOT_SUPPORTED    = 9; // Invalid cmd flag set
  SMART_NO_IDE_DEVICE    = 10; // Cmd issued to device not present
                               // although drive number is valid

//
// SMART sub commands for execute offline diags
//

  SMART_OFFLINE_ROUTINE_OFFLINE   = 0;
  SMART_SHORT_SELFTEST_OFFLINE    = 1;
  SMART_EXTENDED_SELFTEST_OFFLINE = 2;
  SMART_ABORT_OFFLINE_SELFTEST    = 127;
  SMART_SHORT_SELFTEST_CAPTIVE    = 129;
  SMART_EXTENDED_SELFTEST_CAPTIVE = 130;

//#include <pshpack1.h>

type
  LPSENDCMDOUTPARAMS = ^SENDCMDOUTPARAMS;
  _SENDCMDOUTPARAMS = packed record
    cBufferSize: DWORD;            // Size of bBuffer in bytes
    DriverStatus: DRIVERSTATUS;    // Driver status structure.
    bBuffer: array [0..0] of BYTE; // Buffer of arbitrary length in which to store the data read from the                                                                                  // drive.
  end;
  SENDCMDOUTPARAMS = _SENDCMDOUTPARAMS;
  TSendCmdOutParams = SENDCMDOUTPARAMS;
  PSendCmdOutParams = LPSENDCMDOUTPARAMS;

//#include <poppack.h>

const
  READ_ATTRIBUTE_BUFFER_SIZE = 512;
  IDENTIFY_BUFFER_SIZE       = 512;
  READ_THRESHOLD_BUFFER_SIZE = 512;
  SMART_LOG_SECTOR_SIZE      = 512;

//
// Feature register defines for SMART "sub commands"
//

  READ_ATTRIBUTES             = $D0;
  READ_THRESHOLDS             = $D1;
  ENABLE_DISABLE_AUTOSAVE     = $D2;
  SAVE_ATTRIBUTE_VALUES       = $D3;
  EXECUTE_OFFLINE_DIAGS       = $D4;
  SMART_READ_LOG              = $D5;
  SMART_WRITE_LOG             = $d6;
  ENABLE_SMART                = $D8;
  DISABLE_SMART               = $D9;
  RETURN_SMART_STATUS         = $DA;
  ENABLE_DISABLE_AUTO_OFFLINE = $DB;

  IOCTL_CHANGER_BASE = FILE_DEVICE_CHANGER;

  IOCTL_CHANGER_GET_PARAMETERS = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0000 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_GET_STATUS = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0001 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_GET_PRODUCT_DATA = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0002 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_SET_ACCESS = (
    (IOCTL_CHANGER_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0004 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_GET_ELEMENT_STATUS = (
    (IOCTL_CHANGER_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($0005 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_INITIALIZE_ELEMENT_STATUS = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0006 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_SET_POSITION = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0007 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_EXCHANGE_MEDIUM = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0008 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_MOVE_MEDIUM = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($0009 shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_REINITIALIZE_TRANSPORT = (
    (IOCTL_CHANGER_BASE shl 16) or (FILE_READ_ACCESS shl 14) or
    ($000A shl 2) or METHOD_BUFFERED);

  IOCTL_CHANGER_QUERY_VOLUME_TAGS = (
    (IOCTL_CHANGER_BASE shl 16) or ((FILE_READ_ACCESS or FILE_WRITE_ACCESS) shl 14) or
    ($000B shl 2) or METHOD_BUFFERED);

  MAX_VOLUME_ID_SIZE       = 36;
  MAX_VOLUME_TEMPLATE_SIZE = 40;

  VENDOR_ID_LENGTH     = 8;
  PRODUCT_ID_LENGTH    = 16;
  REVISION_LENGTH      = 4;
  SERIAL_NUMBER_LENGTH = 32;

//
// Common structures describing elements.
//

type
  _ELEMENT_TYPE = (
    AllElements,        // As defined by SCSI
    ChangerTransport,   // As defined by SCSI
    ChangerSlot,        // As defined by SCSI
    ChangerIEPort,      // As defined by SCSI
    ChangerDrive,       // As defined by SCSI
    ChangerDoor,        // Front panel, used to access internal of cabinet.
    ChangerKeypad,      // Keypad/input on front panel.
    ChangerMaxElement); // Placeholder only. Not a valid type.
  ELEMENT_TYPE = _ELEMENT_TYPE;
  PELEMENT_TYPE = ^ELEMENT_TYPE;
  TElementType = ELEMENT_TYPE;
  PElementType = PELEMENT_TYPE;

  PCHANGER_ELEMENT = ^CHANGER_ELEMENT;
  _CHANGER_ELEMENT = record
    ElementType: ELEMENT_TYPE;
    ElementAddress: DWORD;
  end;
  CHANGER_ELEMENT = _CHANGER_ELEMENT;
  TChangerElement = CHANGER_ELEMENT;
  PChangerElement = PCHANGER_ELEMENT;

  PCHANGER_ELEMENT_LIST = ^CHANGER_ELEMENT_LIST;
  _CHANGER_ELEMENT_LIST = record
    Element: CHANGER_ELEMENT;
    NumberOfElements: DWORD;
  end;
  CHANGER_ELEMENT_LIST = _CHANGER_ELEMENT_LIST;
  TChangerElementList = CHANGER_ELEMENT_LIST;
  PChangerElementList = PCHANGER_ELEMENT_LIST;

//
// Definitions for  IOCTL_CHANGER_GET_PARAMETERS
//

//
// Definitions for Features0 of GET_CHANGER_PARAMETERS
//

const
  CHANGER_BAR_CODE_SCANNER_INSTALLED  = $00000001; // The medium-changer has a bar code scanner installed.
  CHANGER_INIT_ELEM_STAT_WITH_RANGE   = $00000002; // The medium-changer has the ability to initialize elements within a specified range.
  CHANGER_CLOSE_IEPORT                = $00000004; // The medium-changer has the ability to close the i/e port door.
  CHANGER_OPEN_IEPORT                 = $00000008; // The medium-changer can open the i/e port door.

  CHANGER_STATUS_NON_VOLATILE         = $00000010; // The medium-changer uses non-volatile memory for element status information.
  CHANGER_EXCHANGE_MEDIA              = $00000020; // The medium-changer supports exchange operations.
  CHANGER_CLEANER_SLOT                = $00000040; // The medium-changer has a fixed slot designated for cleaner cartridges.
  CHANGER_LOCK_UNLOCK                 = $00000080; // The medium-changer can be (un)secured to (allow)prevent media removal.

  CHANGER_CARTRIDGE_MAGAZINE          = $00000100; // The medium-changer uses cartridge magazines for some storage slots.
  CHANGER_MEDIUM_FLIP                 = $00000200; // The medium-changer can flip medium.
  CHANGER_POSITION_TO_ELEMENT         = $00000400; // The medium-changer can position the transport to a particular element.
  CHANGER_REPORT_IEPORT_STATE         = $00000800; // The medium-changer can determine whether media is present
                                                   // in the IE Port.

  CHANGER_STORAGE_DRIVE               = $00001000; // The medium-changer can use a drive as an independent storage element.
  CHANGER_STORAGE_IEPORT              = $00002000; // The medium-changer can use a i/e port as an independent storage element.
  CHANGER_STORAGE_SLOT                = $00004000; // The medium-changer can use a slot as an independent storage element.
  CHANGER_STORAGE_TRANSPORT           = $00008000; // The medium-changer can use a transport as an independent storage element.

  CHANGER_DRIVE_CLEANING_REQUIRED     = $00010000; // The drives controlled by the medium changer require periodic cleaning
                                                   // initiated by an application.

  CHANGER_PREDISMOUNT_EJECT_REQUIRED  = $00020000; // The medium-changer requires a drive eject command to be issued, before a changer
                                                   // move / exchange command can be issued to the drive.

  CHANGER_CLEANER_ACCESS_NOT_VALID    = $00040000; // The access bit in GES isn't valid for cleaner cartridges.
  CHANGER_PREMOUNT_EJECT_REQUIRED     = $00080000; // The medium-changer requires a drive eject command to be issued
                                                   // before a move / exchange command can be issued with the drive as src/dst.

  CHANGER_VOLUME_IDENTIFICATION       = $00100000; // The medium-changer supports volume identification.
  CHANGER_VOLUME_SEARCH               = $00200000; // The medium-changer can search for volume information.
  CHANGER_VOLUME_ASSERT               = $00400000; // The medium-changer can verify volume information.
  CHANGER_VOLUME_REPLACE              = $00800000; // The medium-changer can replace volume information.
  CHANGER_VOLUME_UNDEFINE             = $01000000; // The medium-changer can undefine volume information.

  CHANGER_SERIAL_NUMBER_VALID         = $04000000; // The serial number reported in GetProductData is valid
                                                   // and unique.

  CHANGER_DEVICE_REINITIALIZE_CAPABLE = $08000000; // The medium-changer can be issued a ChangerReinitializeUnit.
  CHANGER_KEYPAD_ENABLE_DISABLE       = $10000000; // Indicates that the keypad can be enabled/disabled.
  CHANGER_DRIVE_EMPTY_ON_DOOR_ACCESS  = $20000000; // Drives must be empty before access via the door is possible.

  CHANGER_RESERVED_BIT                = DWORD($80000000); // Will be used to indicate Features1 capability bits.

//
// Definitions for Features1 of GET_CHANGER_PARAMETERS
//

  CHANGER_PREDISMOUNT_ALIGN_TO_SLOT  = DWORD($80000001); // The transport must be prepositioned to the slot prior to ejecting the media.
  CHANGER_PREDISMOUNT_ALIGN_TO_DRIVE = DWORD($80000002); // The transport must be prepositioned to the drive prior to ejecting the media.
  CHANGER_CLEANER_AUTODISMOUNT       = DWORD($80000004); // The device will move the cleaner cartridge back into the slot when cleaning has completed.
  CHANGER_TRUE_EXCHANGE_CAPABLE      = DWORD($80000008); // Device can do src -> dest2 exchanges.
  CHANGER_SLOTS_USE_TRAYS            = DWORD($80000010); // Slots have removable trays, requiring multiple moves for inject/eject.
  CHANGER_RTN_MEDIA_TO_ORIGINAL_ADDR = DWORD($80000020); // Media must be returned to the slot from which it originated after a move to another element.
  CHANGER_CLEANER_OPS_NOT_SUPPORTED  = DWORD($80000040); // Automated cleaning operations are not supported on this device.
  CHANGER_IEPORT_USER_CONTROL_OPEN   = DWORD($80000080); // Indicates that user action is necessary to open a closed ieport.
  CHANGER_IEPORT_USER_CONTROL_CLOSE  = DWORD($80000100); // Indicates that user action is necessary to close an opened ieport.
  CHANGER_MOVE_EXTENDS_IEPORT        = DWORD($80000200); // Indicates that a move media to the ieport extends the tray.
  CHANGER_MOVE_RETRACTS_IEPORT       = DWORD($80000400); // Indicates that a move media from the ieport retracts the tray.

//
// Definitions for MoveFrom, ExchangeFrom, and PositionCapabilities
//

  CHANGER_TO_TRANSPORT = $01; // The device can carry out the operation to a transport from the specified element.
  CHANGER_TO_SLOT      = $02; // The device can carry out the operation to a slot from the specified element.
  CHANGER_TO_IEPORT    = $04; // The device can carry out the operation to an IE Port from the specified element.
  CHANGER_TO_DRIVE     = $08; // The device can carry out the operation to a drive from the specified element.

//
// Definitions for LockUnlockCapabilities
//

  LOCK_UNLOCK_IEPORT = $01; // The device can lock/unlock the ieport(s).
  LOCK_UNLOCK_DOOR   = $02; // The device can lock/unlock the door(s).
  LOCK_UNLOCK_KEYPAD = $04; // The device can lock/unlock the keypad.

type
  PGET_CHANGER_PARAMETERS = ^GET_CHANGER_PARAMETERS;
  _GET_CHANGER_PARAMETERS = record
    //
    // Size of the structure. Can be used for versioning.
    //
    Size: DWORD;
    //
    // Number of N element(s) as defined by the Element Address Page (or equivalent...).
    //
    NumberTransportElements: WORD;
    NumberStorageElements: WORD; // for data cartridges only
    NumberCleanerSlots: WORD; // for cleaner cartridges
    NumberIEElements: WORD;
    NumberDataTransferElements: WORD;
    //
    // Number of doors/front panels (allows user entry into the cabinet).
    //
    NumberOfDoors: WORD;
    //
    // The device-specific address (from user manual of the device) of the first N element. Used
    // by the UI to relate the various elements to the user.
    //
    FirstSlotNumber: WORD;
    FirstDriveNumber: WORD;
    FirstTransportNumber: WORD;
    FirstIEPortNumber: WORD;
    FirstCleanerSlotAddress: WORD;
    //
    // Indicates the capacity of each magazine, if they exist.
    //
    MagazineSize: WORD;
    //
    // Specifies the approximate number of seconds for when a cleaning should be completed.
    // Only applicable if drive cleaning is supported. See Features0.
    //
    DriveCleanTimeout: DWORD;
    //
    // See features bits, above.
    //
    Features0: DWORD;
    Features1: DWORD;
    //
    // Bitmask defining Move from N element to element. Defined by Device Capabilities Page (or equivalent).
    // AND-masking with the TO_XXX values will indicate legal destinations.
    //
    MoveFromTransport: BYTE;
    MoveFromSlot: BYTE;
    MoveFromIePort: BYTE;
    MoveFromDrive: BYTE;
    //
    // Bitmask defining Exchange from N element to element. Defined by Device Capabilities Page (or equivalent).
    // AND-masking with the TO_XXX values will indicate legal destinations.
    //
    ExchangeFromTransport: BYTE;
    ExchangeFromSlot: BYTE;
    ExchangeFromIePort: BYTE;
    ExchangeFromDrive: BYTE;
    //
    // Bitmask defining which elements are capable of lock/unlock. Valid only if
    // CHANGER_LOCK_UNLOCK is set in Features0.
    //
    LockUnlockCapabilities: BYTE;
    //
    // Bitmask defining which elements valid for positioning operations. Valid only if
    // CHANGER_POSITION_TO_ELEMENT is set in Features0.
    //
    PositionCapabilities: BYTE;
    //
    // For future expansion.
    //
    Reserved1: array [0..1] of BYTE;
    Reserved2: array [0..1] of DWORD;
  end;
  GET_CHANGER_PARAMETERS = _GET_CHANGER_PARAMETERS;
  TGetChangerParameters = GET_CHANGER_PARAMETERS;
  PGetChangerParameters = PGET_CHANGER_PARAMETERS;

//
// Definitions for IOCTL_CHANGER_GET_PRODUCT_DATA
//

  PCHANGER_PRODUCT_DATA = ^CHANGER_PRODUCT_DATA;
  _CHANGER_PRODUCT_DATA = record
    //
    // Device manufacturer's name - based on inquiry data
    //
    VendorId: array [0..VENDOR_ID_LENGTH - 1] of BYTE;
    //
    // Product identification as defined by the vendor - based on Inquiry data
    //
    ProductId: array [0..PRODUCT_ID_LENGTH - 1] of BYTE;
    //
    // Product revision as defined by the vendor.
    //
    Revision: array [0..REVISION_LENGTH - 1] of BYTE;
    //
    // Vendor unique value used to globally identify this device. Can
    // be from Vital Product Data, for example.
    //
    SerialNumber: array [0..SERIAL_NUMBER_LENGTH - 1] of BYTE;
    //
    // Indicates device type of data transports, as defined by SCSI-2.
    //
    DeviceType: BYTE;
  end;
  CHANGER_PRODUCT_DATA = _CHANGER_PRODUCT_DATA;
  TChangerProductData = CHANGER_PRODUCT_DATA;
  PChangerProductData = PCHANGER_PRODUCT_DATA;

//
// Definitions for IOCTL_CHANGER_SET_ACCESS
//

const
  LOCK_ELEMENT   = 0;
  UNLOCK_ELEMENT = 1;
  EXTEND_IEPORT  = 2;
  RETRACT_IEPORT = 3;

type
  PCHANGER_SET_ACCESS = ^CHANGER_SET_ACCESS;
  _CHANGER_SET_ACCESS = record
    //
    // Element can be ChangerIEPort, ChangerDoor, ChangerKeypad
    //
    Element: CHANGER_ELEMENT;
    //
    // See above for possible operations.
    //
    Control: DWORD;
  end;
  CHANGER_SET_ACCESS = _CHANGER_SET_ACCESS;
  TChangerSetAccess = CHANGER_SET_ACCESS;
  PChangerSetAccess = PCHANGER_SET_ACCESS;

//
// Definitions for IOCTL_CHANGER_GET_ELEMENT_STATUS
//

//
// Input buffer.
//

  PCHANGER_READ_ELEMENT_STATUS = ^CHANGER_READ_ELEMENT_STATUS;
  _CHANGER_READ_ELEMENT_STATUS = record
    //
    // List describing the elements and range on which to return information.
    //
    ElementList: CHANGER_ELEMENT_LIST;
    //
    // Indicates whether volume tag information is to be returned.
    //
    VolumeTagInfo: ByteBool;
  end;
  CHANGER_READ_ELEMENT_STATUS = _CHANGER_READ_ELEMENT_STATUS;
  TChangerReadElementStatus = CHANGER_READ_ELEMENT_STATUS;
  PChangerReadElementStatus = PCHANGER_READ_ELEMENT_STATUS;

//
// Output buffer.
//

  PCHANGER_ELEMENT_STATUS = ^CHANGER_ELEMENT_STATUS;
  _CHANGER_ELEMENT_STATUS = record
    //
    // Element to which this structure refers.
    //
    Element: CHANGER_ELEMENT;
    //
    // Address of the element from which the media was originally moved.
    // Valid if ELEMENT_STATUS_SVALID bit of Flags DWORD is set.
    // Needs to be converted to a zero-based offset from the device-unique value.
    //
    SrcElementAddress: CHANGER_ELEMENT;
    //
    // See below.
    //
    Flags: DWORD;
    //
    // See below for possible values.
    //
    ExceptionCode: DWORD;
    //
    // Scsi Target Id of this element.
    // Valid only if ELEMENT_STATUS_ID_VALID is set in Flags.
    //
    TargetId: BYTE;
    //
    // LogicalUnitNumber of this element.
    // Valid only if ELEMENT_STATUS_LUN_VALID is set in Flags.
    //
    Lun: BYTE;
    Reserved: WORD;
    //
    // Primary volume identification for the media.
    // Valid only if ELEMENT_STATUS_PVOLTAG bit is set in Flags.
    //
    PrimaryVolumeID: array [0..MAX_VOLUME_ID_SIZE - 1] of BYTE;
    //
    // Alternate volume identification for the media.
    // Valid for two-sided media only, and pertains to the id. of the inverted side.
    // Valid only if ELEMENT_STATUS_AVOLTAG bit is set in Flags.
    //
    AlternateVolumeID: array [0..MAX_VOLUME_ID_SIZE - 1] of BYTE;
  end;
  CHANGER_ELEMENT_STATUS = _CHANGER_ELEMENT_STATUS;
  TChangerElementStatus = CHANGER_ELEMENT_STATUS;
  PChangerElementStatus = PCHANGER_ELEMENT_STATUS;

  PCHANGER_ELEMENT_STATUS_EX = ^CHANGER_ELEMENT_STATUS_EX;
  _CHANGER_ELEMENT_STATUS_EX = record
    //
    // Element to which this structure refers.
    //
    Element: CHANGER_ELEMENT;
    //
    // Address of the element from which the media was originally moved.
    // Valid if ELEMENT_STATUS_SVALID bit of Flags DWORD is set.
    // Needs to be converted to a zero-based offset from the device-unique value.
    //
    SrcElementAddress: CHANGER_ELEMENT;
    //
    // See below.
    //
    Flags: DWORD;
    //
    // See below for possible values.
    //
    ExceptionCode: DWORD;
    //
    // Scsi Target Id of this element.
    // Valid only if ELEMENT_STATUS_ID_VALID is set in Flags.
    //
    TargetId: BYTE;
    //
    // LogicalUnitNumber of this element.
    // Valid only if ELEMENT_STATUS_LUN_VALID is set in Flags.
    //
    Lun: BYTE;
    Reserved: WORD;
    //
    // Primary volume identification for the media.
    // Valid only if ELEMENT_STATUS_PVOLTAG bit is set in Flags.
    //
    PrimaryVolumeID: array [0..MAX_VOLUME_ID_SIZE - 1] of BYTE;
    //
    // Alternate volume identification for the media.
    // Valid for two-sided media only, and pertains to the id. of the inverted side.
    // Valid only if ELEMENT_STATUS_AVOLTAG bit is set in Flags.
    //
    AlternateVolumeID: array [0..MAX_VOLUME_ID_SIZE - 1] of BYTE;
    //
    // Vendor ID
    //
    VendorIdentification: array [0..VENDOR_ID_LENGTH - 1] of BYTE;
    //
    // Product ID
    //
    ProductIdentification: array [0..PRODUCT_ID_LENGTH - 1] of BYTE;
    //
    // Serial number
    //
    SerialNumber: array [0..SERIAL_NUMBER_LENGTH - 1] of BYTE;
  end;
  CHANGER_ELEMENT_STATUS_EX = _CHANGER_ELEMENT_STATUS_EX;
  TChangerElementStatusEx = CHANGER_ELEMENT_STATUS_EX;
  PChangerElementStatusEx = PCHANGER_ELEMENT_STATUS_EX;

//
// Possible flag values
//

const
  ELEMENT_STATUS_FULL      = $00000001; // Element contains a unit of media.
  ELEMENT_STATUS_IMPEXP    = $00000002; // Media in i/e port was placed there by an operator.
  ELEMENT_STATUS_EXCEPT    = $00000004; // Element is in an abnormal state; check ExceptionCode field for more information.
  ELEMENT_STATUS_ACCESS    = $00000008; // Access to the i/e port from the medium changer is allowed.
  ELEMENT_STATUS_EXENAB    = $00000010; // Export of media is supported.
  ELEMENT_STATUS_INENAB    = $00000020; // Import of media is supported.

  ELEMENT_STATUS_PRODUCT_DATA = $00000040; // Serial number valid for the drive

  ELEMENT_STATUS_LUN_VALID = $00001000; // Lun information is valid.
  ELEMENT_STATUS_ID_VALID  = $00002000; // SCSI Id information is valid.
  ELEMENT_STATUS_NOT_BUS   = $00008000; // Lun and SCSI Id fields are not on same bus as medium changer.
  ELEMENT_STATUS_INVERT    = $00400000; // Media in element was inverted (valid only if ELEMENT_STATUS_SVALID bit is set)

  ELEMENT_STATUS_SVALID    = $00800000; // SourceElementAddress field and ELEMENT_STATUS_INVERT bit are valid.
  ELEMENT_STATUS_PVOLTAG   = $10000000; // Primary volume information is valid.
  ELEMENT_STATUS_AVOLTAG   = $20000000; // Alternate volume information is valid.

//
// ExceptionCode values.
//

  ERROR_LABEL_UNREADABLE    = $00000001; // Bar code scanner could not read bar code label.
  ERROR_LABEL_QUESTIONABLE  = $00000002; // Label could be invalid due to unit attention condition.
  ERROR_SLOT_NOT_PRESENT    = $00000004; // Slot is currently not addressable in the device.
  ERROR_DRIVE_NOT_INSTALLED = $00000008; // Drive is not installed.
  ERROR_TRAY_MALFUNCTION    = $00000010; // Media tray is malfunctioning/broken.
  ERROR_INIT_STATUS_NEEDED  = $00000011; // An Initialize Element Status command is needed.
  ERROR_UNHANDLED_ERROR     = DWORD($FFFFFFFF); // Unknown error condition

//
// Definitions for IOCTL_CHANGER_INITIALIZE_ELEMENT_STATUS
//

type
  PCHANGER_INITIALIZE_ELEMENT_STATUS = ^CHANGER_INITIALIZE_ELEMENT_STATUS;
  _CHANGER_INITIALIZE_ELEMENT_STATUS = record
    //
    // List describing the elements and range on which to initialize.
    //
    ElementList: CHANGER_ELEMENT_LIST;
    //
    // Indicates whether a bar code scan should be used. Only applicable if
    // CHANGER_BAR_CODE_SCANNER_INSTALLED is set in Features0 of CHANGER_GET_PARAMETERS.
    //
    BarCodeScan: ByteBool;
  end;
  CHANGER_INITIALIZE_ELEMENT_STATUS = _CHANGER_INITIALIZE_ELEMENT_STATUS;
  TChangerInitializeElementStatus = CHANGER_INITIALIZE_ELEMENT_STATUS;
  PChangerInitializeElementStatus = PCHANGER_INITIALIZE_ELEMENT_STATUS;

//
// Definitions for IOCTL_CHANGER_SET_POSITION
//

  PCHANGER_SET_POSITION = ^CHANGER_SET_POSITION;
  _CHANGER_SET_POSITION = record
    //
    // Indicates which transport to move.
    //
    Transport: CHANGER_ELEMENT;
    //
    // Indicates the final destination of the transport.
    //
    Destination: CHANGER_ELEMENT;
    //
    // Indicates whether the media currently carried by Transport, should be flipped.
    //
    Flip: ByteBool;
  end;
  CHANGER_SET_POSITION = _CHANGER_SET_POSITION;
  TChangerSetPosition = CHANGER_SET_POSITION;
  PChangerSetPosition = PCHANGER_SET_POSITION;

//
// Definitions for IOCTL_CHANGER_EXCHANGE_MEDIUM
//

  PCHANGER_EXCHANGE_MEDIUM = ^CHANGER_EXCHANGE_MEDIUM;
  _CHANGER_EXCHANGE_MEDIUM = record
    //
    // Indicates which transport to use for the exchange operation.
    //
    Transport: CHANGER_ELEMENT;
    //
    // Indicates the source for the media that is to be moved.
    //
    Source: CHANGER_ELEMENT;
    //
    // Indicates the final destination of the media originally at Source.
    //
    Destination1: CHANGER_ELEMENT;
    //
    // Indicates the destination of the media moved from Destination1.
    //
    Destination2: CHANGER_ELEMENT;
    //
    // Indicates whether the medium should be flipped.
    //
    Flip1: ByteBool;
    Flip2: ByteBool;
  end;
  CHANGER_EXCHANGE_MEDIUM = _CHANGER_EXCHANGE_MEDIUM;
  TChangerExchangeMedium = CHANGER_EXCHANGE_MEDIUM;
  PChangerExchangeMedium = PCHANGER_EXCHANGE_MEDIUM;

//
// Definitions for IOCTL_CHANGER_MOVE_MEDIUM
//

  PCHANGER_MOVE_MEDIUM = ^CHANGER_MOVE_MEDIUM;
  _CHANGER_MOVE_MEDIUM = record
    //
    // Indicates which transport to use for the move operation.
    //
    Transport: CHANGER_ELEMENT;
    //
    // Indicates the source for the media that is to be moved.
    //
    Source: CHANGER_ELEMENT;
    //
    // Indicates the destination of the media originally at Source.
    //
    Destination: CHANGER_ELEMENT;
    //
    // Indicates whether the media should be flipped.
    //
    Flip: ByteBool;
  end;
  CHANGER_MOVE_MEDIUM = _CHANGER_MOVE_MEDIUM;
  TChangerMoveMedium = CHANGER_MOVE_MEDIUM;
  PChangerMoveMedium = PCHANGER_MOVE_MEDIUM;

//
// Definitions for IOCTL_QUERY_VOLUME_TAGS
//

//
// Input buffer.
//

  PCHANGER_SEND_VOLUME_TAG_INFORMATION = ^CHANGER_SEND_VOLUME_TAG_INFORMATION;
  _CHANGER_SEND_VOLUME_TAG_INFORMATION = record
    //
    // Describes the starting element for which to return information.
    //
    StartingElement: CHANGER_ELEMENT;
    //
    // Indicates the specific action to perform. See below.
    //
    ActionCode: DWORD;
    //
    // Template used by the device to search for volume ids.
    //
    VolumeIDTemplate: array [0..MAX_VOLUME_TEMPLATE_SIZE - 1] of BYTE;
  end;
  CHANGER_SEND_VOLUME_TAG_INFORMATION = _CHANGER_SEND_VOLUME_TAG_INFORMATION;
  TChangerSendVolumeTagInformation = CHANGER_SEND_VOLUME_TAG_INFORMATION;
  PChangerSendVolumeTagInformation = PCHANGER_SEND_VOLUME_TAG_INFORMATION;

//
// Output buffer.
//

  PREAD_ELEMENT_ADDRESS_INFO = ^READ_ELEMENT_ADDRESS_INFO;
  _READ_ELEMENT_ADDRESS_INFO = record
    //
    // Number of elements matching criteria set forth by ActionCode.
    //
    NumberOfElements: DWORD;
    //
    // Array of CHANGER_ELEMENT_STATUS structures, one for each element that corresponded
    // with the information passed in with the CHANGER_SEND_VOLUME_TAG_INFORMATION structure.
    //
    ElementStatus: array [0..0] of CHANGER_ELEMENT_STATUS;
  end;
  READ_ELEMENT_ADDRESS_INFO = _READ_ELEMENT_ADDRESS_INFO;
  TReadElementAddressInfo = READ_ELEMENT_ADDRESS_INFO;
  PReadElementAddressInfo = PREAD_ELEMENT_ADDRESS_INFO;

//
// Possible ActionCode values. See Features0 of CHANGER_GET_PARAMETERS for compatibility with
// the current device.
//

const
  SEARCH_ALL         = $0; // Translate - search all defined volume tags.
  SEARCH_PRIMARY     = $1; // Translate - search only primary volume tags.
  SEARCH_ALTERNATE   = $2; // Translate - search only alternate volume tags.
  SEARCH_ALL_NO_SEQ  = $4; // Translate - search all defined volume tags but ignore sequence numbers.
  SEARCH_PRI_NO_SEQ  = $5; // Translate - search only primary volume tags but ignore sequence numbers.
  SEARCH_ALT_NO_SEQ  = $6; // Translate - search only alternate volume tags but ignore sequence numbers.

  ASSERT_PRIMARY     = $8; // Assert - as the primary volume tag - if tag now undefined.
  ASSERT_ALTERNATE   = $9; // Assert - as the alternate volume tag - if tag now undefined.

  REPLACE_PRIMARY    = $A; // Replace - the primary volume tag - current tag ignored.
  REPLACE_ALTERNATE  = $B; // Replace - the alternate volume tag - current tag ignored.

  UNDEFINE_PRIMARY   = $C; // Undefine - the primary volume tag - current tag ignored.
  UNDEFINE_ALTERNATE = $D; // Undefine - the alternate volume tag - current tag ignored.

//
// Changer diagnostic test related definitions
//

type
  _CHANGER_DEVICE_PROBLEM_TYPE = (
   DeviceProblemNone,
   DeviceProblemHardware,
   DeviceProblemCHMError,
   DeviceProblemDoorOpen,
   DeviceProblemCalibrationError,
   DeviceProblemTargetFailure,
   DeviceProblemCHMMoveError,
   DeviceProblemCHMZeroError,
   DeviceProblemCartridgeInsertError,
   DeviceProblemPositionError,
   DeviceProblemSensorError,
   DeviceProblemCartridgeEjectError,
   DeviceProblemGripperError,
   DeviceProblemDriveError);
  CHANGER_DEVICE_PROBLEM_TYPE = _CHANGER_DEVICE_PROBLEM_TYPE;
  PCHANGER_DEVICE_PROBLEM_TYPE = ^CHANGER_DEVICE_PROBLEM_TYPE;
  TChangerDeviceProblemType = CHANGER_DEVICE_PROBLEM_TYPE;
  PChangerDeviceProblemType = PCHANGER_DEVICE_PROBLEM_TYPE;

const
  IOCTL_SERIAL_LSRMST_INSERT = (
    (FILE_DEVICE_SERIAL_PORT shl 16) or (FILE_ANY_ACCESS shl 14) or
    (31 shl 2) or METHOD_BUFFERED);

  IOCTL_SERENUM_EXPOSE_HARDWARE = (
    (FILE_DEVICE_SERENUM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (128 shl 2) or METHOD_BUFFERED);

  IOCTL_SERENUM_REMOVE_HARDWARE = (
    (FILE_DEVICE_SERENUM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (129 shl 2) or METHOD_BUFFERED);

  IOCTL_SERENUM_PORT_DESC = (
    (FILE_DEVICE_SERENUM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (130 shl 2) or METHOD_BUFFERED);

  IOCTL_SERENUM_GET_PORT_NAME = (
    (FILE_DEVICE_SERENUM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (131 shl 2) or METHOD_BUFFERED);

//
// The following values follow the escape designator in the
// data stream if the LSRMST_INSERT mode has been turned on.
//

  SERIAL_LSRMST_ESCAPE = BYTE($00);

//
// Following this value is the contents of the line status
// register, and then the character in the RX hardware when
// the line status register was encountered.
//

  SERIAL_LSRMST_LSR_DATA = BYTE($01);

//
// Following this value is the contents of the line status
// register.  No error character follows
//

  SERIAL_LSRMST_LSR_NODATA = BYTE($02);

//
// Following this value is the contents of the modem status
// register.
//

  SERIAL_LSRMST_MST = BYTE($03);

//
// Bit values for FIFO Control Register
//

  SERIAL_IOC_FCR_FIFO_ENABLE      = DWORD($00000001);
  SERIAL_IOC_FCR_RCVR_RESET       = DWORD($00000002);
  SERIAL_IOC_FCR_XMIT_RESET       = DWORD($00000004);
  SERIAL_IOC_FCR_DMA_MODE         = DWORD($00000008);
  SERIAL_IOC_FCR_RES1             = DWORD($00000010);
  SERIAL_IOC_FCR_RES2             = DWORD($00000020);
  SERIAL_IOC_FCR_RCVR_TRIGGER_LSB = DWORD($00000040);
  SERIAL_IOC_FCR_RCVR_TRIGGER_MSB = DWORD($00000080);

//
// Bit values for Modem Control Register
//

  SERIAL_IOC_MCR_DTR  = DWORD($00000001);
  SERIAL_IOC_MCR_RTS  = DWORD($00000002);
  SERIAL_IOC_MCR_OUT1 = DWORD($00000004);
  SERIAL_IOC_MCR_OUT2 = DWORD($00000008);
  SERIAL_IOC_MCR_LOOP = DWORD($00000010);

//
// The following is a list of the native file system fsctls followed by
// additional network file system fsctls.  Some values have been
// decommissioned.
//

  FSCTL_REQUEST_OPLOCK_LEVEL_1 = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (0 shl 2) or METHOD_BUFFERED);

  FSCTL_REQUEST_OPLOCK_LEVEL_2 = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (1 shl 2) or METHOD_BUFFERED);

  FSCTL_REQUEST_BATCH_OPLOCK = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (2 shl 2) or METHOD_BUFFERED);

  FSCTL_OPLOCK_BREAK_ACKNOWLEDGE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (3 shl 2) or METHOD_BUFFERED);

  FSCTL_OPBATCH_ACK_CLOSE_PENDING = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (4 shl 2) or METHOD_BUFFERED);

  FSCTL_OPLOCK_BREAK_NOTIFY = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (5 shl 2) or METHOD_BUFFERED);

  FSCTL_LOCK_VOLUME = ((FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or (6 shl 2) or METHOD_BUFFERED);

  FSCTL_UNLOCK_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (7 shl 2) or METHOD_BUFFERED);

  FSCTL_DISMOUNT_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (8 shl 2) or METHOD_BUFFERED);

// decommissioned fsctl value                                              9

  FSCTL_IS_VOLUME_MOUNTED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (10 shl 2) or METHOD_BUFFERED);

  FSCTL_IS_PATHNAME_VALID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (11 shl 2) or METHOD_BUFFERED);    // PATHNAME_BUFFER,

  FSCTL_MARK_VOLUME_DIRTY = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (12 shl 2) or METHOD_BUFFERED);

// decommissioned fsctl value                                             13

  FSCTL_QUERY_RETRIEVAL_POINTERS = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (14 shl 2) or METHOD_NEITHER);

  FSCTL_GET_COMPRESSION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (15 shl 2) or METHOD_BUFFERED);

  FSCTL_SET_COMPRESSION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (16 shl 2) or METHOD_BUFFERED);

// decommissioned fsctl value                                             17
// decommissioned fsctl value                                             18

  FSCTL_MARK_AS_SYSTEM_HIVE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (19 shl 2) or METHOD_NEITHER);

  FSCTL_OPLOCK_BREAK_ACK_NO_2 = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (20 shl 2) or METHOD_BUFFERED);

  FSCTL_INVALIDATE_VOLUMES = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (21 shl 2) or METHOD_BUFFERED);

  FSCTL_QUERY_FAT_BPB = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (22 shl 2) or METHOD_BUFFERED); // FSCTL_QUERY_FAT_BPB_BUFFER

  FSCTL_REQUEST_FILTER_OPLOCK = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (23 shl 2) or METHOD_BUFFERED);

  FSCTL_FILESYSTEM_GET_STATISTICS = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (24 shl 2) or METHOD_BUFFERED); // FILESYSTEM_STATISTICS

  FSCTL_GET_NTFS_VOLUME_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (25 shl 2) or METHOD_BUFFERED);

  FSCTL_GET_NTFS_FILE_RECORD = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (26 shl 2) or METHOD_BUFFERED);

  FSCTL_GET_VOLUME_BITMAP = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (27 shl 2) or METHOD_NEITHER);

  FSCTL_GET_RETRIEVAL_POINTERS = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (28 shl 2) or METHOD_NEITHER);

  FSCTL_MOVE_FILE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (29 shl 2) or METHOD_BUFFERED);

  FSCTL_IS_VOLUME_DIRTY = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (30 shl 2) or METHOD_BUFFERED);

// decomissioned fsctl value  31
(*  FSCTL_GET_HFS_INFORMATION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (31 shl 2) or METHOD_BUFFERED);
*)

  FSCTL_ALLOW_EXTENDED_DASD_IO = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (32 shl 2) or METHOD_NEITHER);

// decommissioned fsctl value                                             33
// decommissioned fsctl value                                             34

(*
  FSCTL_READ_PROPERTY_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (33 shl 2) or METHOD_NEITHER);

  FSCTL_WRITE_PROPERTY_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (34 shl 2) or METHOD_NEITHER);
*)

  FSCTL_FIND_FILES_BY_SID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (35 shl 2) or METHOD_NEITHER);  

// decommissioned fsctl value                                             36
// decommissioned fsctl value                                             37

(*  FSCTL_DUMP_PROPERTY_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (37 shl 2) or METHOD_NEITHER);
*)

  FSCTL_SET_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (38 shl 2) or METHOD_BUFFERED);

  FSCTL_GET_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (39 shl 2) or METHOD_BUFFERED);

  FSCTL_DELETE_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (40 shl 2) or METHOD_BUFFERED);

  FSCTL_SET_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (41 shl 2) or METHOD_BUFFERED);

  FSCTL_GET_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (42 shl 2) or METHOD_BUFFERED);

  FSCTL_DELETE_REPARSE_POINT = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (43 shl 2) or METHOD_BUFFERED);

  FSCTL_ENUM_USN_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (44 shl 2) or METHOD_NEITHER);

  FSCTL_SECURITY_ID_CHECK = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (45 shl 2) or METHOD_NEITHER);

  FSCTL_READ_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (46 shl 2) or METHOD_NEITHER);

  FSCTL_SET_OBJECT_ID_EXTENDED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (47 shl 2) or METHOD_BUFFERED);

  FSCTL_CREATE_OR_GET_OBJECT_ID = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (48 shl 2) or METHOD_BUFFERED);

  FSCTL_SET_SPARSE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (49 shl 2) or METHOD_BUFFERED);

  FSCTL_SET_ZERO_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_WRITE_DATA shl 14) or
    (50 shl 2) or METHOD_BUFFERED);

  FSCTL_QUERY_ALLOCATED_RANGES = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (51 shl 2) or METHOD_NEITHER);

// decommissioned fsctl value                                             52
(*
  FSCTL_ENABLE_UPGRADE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_WRITE_DATA shl 14) or
    (52 shl 2) or METHOD_BUFFERED);
*)

  FSCTL_SET_ENCRYPTION = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (53 shl 2) or METHOD_NEITHER);

  FSCTL_ENCRYPTION_FSCTL_IO = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (54 shl 2) or METHOD_NEITHER);

  FSCTL_WRITE_RAW_ENCRYPTED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (55 shl 2) or METHOD_NEITHER);

  FSCTL_READ_RAW_ENCRYPTED = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (56 shl 2) or METHOD_NEITHER);

  FSCTL_CREATE_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (57 shl 2) or METHOD_NEITHER);

  FSCTL_READ_FILE_USN_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (58 shl 2) or METHOD_NEITHER);

  FSCTL_WRITE_USN_CLOSE_RECORD = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (59 shl 2) or METHOD_NEITHER);

  FSCTL_EXTEND_VOLUME = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (60 shl 2) or METHOD_BUFFERED);

  FSCTL_QUERY_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (61 shl 2) or METHOD_BUFFERED);

  FSCTL_DELETE_USN_JOURNAL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (62 shl 2) or METHOD_BUFFERED);

  FSCTL_MARK_HANDLE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (63 shl 2) or METHOD_BUFFERED);

  FSCTL_SIS_COPYFILE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (64 shl 2) or METHOD_BUFFERED);

  FSCTL_SIS_LINK_FILES = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (65 shl 2) or METHOD_BUFFERED);

  FSCTL_HSM_MSG = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (66 shl 2) or METHOD_BUFFERED);

// decommissioned fsctl value                                             67
(*
  FSCTL_NSS_CONTROL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_WRITE_DATA shl 14) or
    (67 shl 2) or METHOD_BUFFERED);
*)

  FSCTL_HSM_DATA = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or ((FILE_READ_DATA or FILE_WRITE_DATA) shl 14) or
    (68 shl 2) or METHOD_NEITHER);

  FSCTL_RECALL_FILE = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_ANY_ACCESS shl 14) or
    (69 shl 2) or METHOD_NEITHER);

// decommissioned fsctl value                                             70
(*
  FSCTL_NSS_RCONTROL = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (70 shl 2) or METHOD_BUFFERED);
*)

  FSCTL_READ_FROM_PLEX = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_READ_DATA shl 14) or
    (71 shl 2) or METHOD_OUT_DIRECT);

  FSCTL_FILE_PREFETCH = (
    (FILE_DEVICE_FILE_SYSTEM shl 16) or (FILE_SPECIAL_ACCESS shl 14) or
    (72 shl 2) or METHOD_BUFFERED);

//
// The following long list of structs are associated with the preceeding
// file system fsctls.
//
// Note that the structs for the property sets fsctls are not included
// in this file.  They can be found in ntfsprop.h where they should stay
// because of their dependency on objidl.h.
//

//
// Structure for FSCTL_IS_PATHNAME_VALID
//

type
  PPATHNAME_BUFFER = ^PATHNAME_BUFFER;
  _PATHNAME_BUFFER = record
    PathNameLength: DWORD;
    Name: array [0..0] of WCHAR;
  end;
  PATHNAME_BUFFER = _PATHNAME_BUFFER;
  TPathnameBuffer = PATHNAME_BUFFER;
  PPathnameBuffer = PPATHNAME_BUFFER;

//
// Structure for FSCTL_QUERY_BPB_INFO
//

  PFSCTL_QUERY_FAT_BPB_BUFFER = ^FSCTL_QUERY_FAT_BPB_BUFFER;
  _FSCTL_QUERY_FAT_BPB_BUFFER = record
    First0x24BytesOfBootSector: array [0..$23] of BYTE;
  end;
  FSCTL_QUERY_FAT_BPB_BUFFER = _FSCTL_QUERY_FAT_BPB_BUFFER;
  TFsctlQueryFatBpbBuffer = FSCTL_QUERY_FAT_BPB_BUFFER;
  PFsctlQueryFatBpbBuffer = PFSCTL_QUERY_FAT_BPB_BUFFER;

//
// Structures for FSCTL_GET_NTFS_VOLUME_DATA.
// The user must pass the basic buffer below.  Ntfs
// will return as many fields as available in the extended
// buffer which follows immediately after the VOLUME_DATA_BUFFER.
//

  PNTFS_VOLUME_DATA_BUFFER = ^NTFS_VOLUME_DATA_BUFFER;
  NTFS_VOLUME_DATA_BUFFER = record
    VolumeSerialNumber: LARGE_INTEGER;
    NumberSectors: LARGE_INTEGER;
    TotalClusters: LARGE_INTEGER;
    FreeClusters: LARGE_INTEGER;
    TotalReserved: LARGE_INTEGER;
    BytesPerSector: DWORD;
    BytesPerCluster: DWORD;
    BytesPerFileRecordSegment: DWORD;
    ClustersPerFileRecordSegment: DWORD;
    MftValidDataLength: LARGE_INTEGER;
    MftStartLcn: LARGE_INTEGER;
    Mft2StartLcn: LARGE_INTEGER;
    MftZoneStart: LARGE_INTEGER;
    MftZoneEnd: LARGE_INTEGER;
  end;
  TNtfsVolumeDataBuffer = NTFS_VOLUME_DATA_BUFFER;
  PNtfsVolumeDataBuffer = PNTFS_VOLUME_DATA_BUFFER;

  PNTFS_EXTENDED_VOLUME_DATA = ^NTFS_EXTENDED_VOLUME_DATA;
  NTFS_EXTENDED_VOLUME_DATA = record
    ByteCount: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
  end;
  TNtfsExtendedVolumeData = NTFS_EXTENDED_VOLUME_DATA;
  PNtfsExtendedVolumeData = PNTFS_EXTENDED_VOLUME_DATA;

//
// Structure for FSCTL_GET_VOLUME_BITMAP
//

  PSTARTING_LCN_INPUT_BUFFER = ^STARTING_LCN_INPUT_BUFFER;
  STARTING_LCN_INPUT_BUFFER = record
    StartingLcn: LARGE_INTEGER;
  end;
  TStartingLcnInputBuffer = STARTING_LCN_INPUT_BUFFER;
  PStartingLcnInputBuffer = PSTARTING_LCN_INPUT_BUFFER;

  PVOLUME_BITMAP_BUFFER = ^VOLUME_BITMAP_BUFFER;
  VOLUME_BITMAP_BUFFER = record
    StartingLcn: LARGE_INTEGER;
    BitmapSize: LARGE_INTEGER;
    Buffer: array [0..0] of BYTE;
  end;
  TVolumeBitmapBuffer = VOLUME_BITMAP_BUFFER;
  PVolumeBitmapBuffer = PVOLUME_BITMAP_BUFFER;

//
// Structure for FSCTL_GET_RETRIEVAL_POINTERS
//

  PSTARTING_VCN_INPUT_BUFFER = ^STARTING_VCN_INPUT_BUFFER;
  STARTING_VCN_INPUT_BUFFER = record
    StartingVcn: LARGE_INTEGER;
  end;
  TStartingVcnInputBuffer = STARTING_VCN_INPUT_BUFFER;
  PStartingVcnInputBuffer = PSTARTING_VCN_INPUT_BUFFER;

  TRPBExtends = record
    NextVcn: LARGE_INTEGER;
    Lcn: LARGE_INTEGER;
  end;

  PRETRIEVAL_POINTERS_BUFFER = ^RETRIEVAL_POINTERS_BUFFER;
  RETRIEVAL_POINTERS_BUFFER = record
    ExtentCount: DWORD;
    StartingVcn: LARGE_INTEGER;
    Extends: array [0..0] of TRPBExtends;
  end;
  TRetrievalPointersBuffer = RETRIEVAL_POINTERS_BUFFER;
  PRetrievalPointersBuffer = PRETRIEVAL_POINTERS_BUFFER;

//
// Structures for FSCTL_GET_NTFS_FILE_RECORD
//

  PNTFS_FILE_RECORD_INPUT_BUFFER = ^NTFS_FILE_RECORD_INPUT_BUFFER;
  NTFS_FILE_RECORD_INPUT_BUFFER = record
    FileReferenceNumber: LARGE_INTEGER;
  end;
  TNtfsFileRecordInputBuffer = NTFS_FILE_RECORD_INPUT_BUFFER;
  PNtfsFileRecordInputBuffer = PNTFS_FILE_RECORD_INPUT_BUFFER;

  PNTFS_FILE_RECORD_OUTPUT_BUFFER = ^NTFS_FILE_RECORD_OUTPUT_BUFFER;
  NTFS_FILE_RECORD_OUTPUT_BUFFER = record
    FileReferenceNumber: LARGE_INTEGER;
    FileRecordLength: DWORD;
    FileRecordBuffer: array [0..0] of BYTE;
  end;
  TNtfsFileRecordOutputBuffer = NTFS_FILE_RECORD_OUTPUT_BUFFER;
  PNtfsFileRecordOutputBuffer = PNTFS_FILE_RECORD_OUTPUT_BUFFER;

//
// Structure for FSCTL_MOVE_FILE
//

  PMOVE_FILE_DATA = ^MOVE_FILE_DATA;
  MOVE_FILE_DATA = record
    FileHandle: HANDLE;
    StartingVcn: LARGE_INTEGER;
    StartingLcn: LARGE_INTEGER;
    ClusterCount: DWORD;
  end;
  TMoveFileData = MOVE_FILE_DATA;
  PMoveFileData = PMOVE_FILE_DATA;

{$IFDEF _WIN64}

//
//  32/64 Bit thunking support structure
//

  _MOVE_FILE_DATA32 = record
    FileHandle: UINT32;
    StartingVcn: LARGE_INTEGER;
    StartingLcn: LARGE_INTEGER;
    ClusterCount: DWORD;
  end;
  MOVE_FILE_DATA32 = _MOVE_FILE_DATA32;
  PMOVE_FILE_DATA32 = ^MOVE_FILE_DATA32;
  TMoveFileData32 = MOVE_FILE_DATA32;
  PMoveFileData32 = PMOVE_FILE_DATA32;
  
{$ENDIF _WIN64}

//
// Structures for FSCTL_FIND_FILES_BY_SID
//

  PFIND_BY_SID_DATA = ^FIND_BY_SID_DATA;
  FIND_BY_SID_DATA = record
    Restart: DWORD;
    Sid: SID;
  end;
  TFindBySidData = FIND_BY_SID_DATA;
  PFindBySidData = PFIND_BY_SID_DATA;

  FIND_BY_SID_OUTPUT = record
    NextEntryOffset: DWORD;
    FileIndex: DWORD;
    FileNameLength: DWORD;
    FileName: array [0..0] of WCHAR;
  end;
  PFIND_BY_SID_OUTPUT = ^FIND_BY_SID_OUTPUT;
  TFindBySidOutput = FIND_BY_SID_OUTPUT;
  PFindBySidOutput = PFIND_BY_SID_OUTPUT;  

//
//  The following structures apply to Usn operations.
//

//
// Structure for FSCTL_ENUM_USN_DATA
//

  PMFT_ENUM_DATA = ^MFT_ENUM_DATA;
  MFT_ENUM_DATA = record
    StartFileReferenceNumber: DWORDLONG;
    LowUsn: USN;
    HighUsn: USN;
  end;
  TMftEnumData = MFT_ENUM_DATA;
  PMftEnumData = PMFT_ENUM_DATA;

//
// Structure for FSCTL_CREATE_USN_JOURNAL
//

  PCREATE_USN_JOURNAL_DATA = ^CREATE_USN_JOURNAL_DATA;
  CREATE_USN_JOURNAL_DATA = record
    MaximumSize: DWORDLONG;
    AllocationDelta: DWORDLONG;
  end;
  TCreateUsnJournalData = CREATE_USN_JOURNAL_DATA;
  PCreateUsnJournalData = PCREATE_USN_JOURNAL_DATA;

//
// Structure for FSCTL_READ_USN_JOURNAL
//

  PREAD_USN_JOURNAL_DATA = ^READ_USN_JOURNAL_DATA;
  READ_USN_JOURNAL_DATA = record
    StartUsn: USN;
    ReasonMask: DWORD;
    ReturnOnlyOnClose: DWORD;
    Timeout: DWORDLONG;
    BytesToWaitFor: DWORDLONG;
    UsnJournalID: DWORDLONG;
  end;
  TReadUsnJournalData = READ_USN_JOURNAL_DATA;
  PReadUsnJournalData = PREAD_USN_JOURNAL_DATA;

//
//  The initial Major.Minor version of the Usn record will be 2.0.
//  In general, the MinorVersion may be changed if fields are added
//  to this structure in such a way that the previous version of the
//  software can still correctly the fields it knows about.  The
//  MajorVersion should only be changed if the previous version of
//  any software using this structure would incorrectly handle new
//  records due to structure changes.
//
//  The first update to this will force the structure to version 2.0.
//  This will add the extended information about the source as
//  well as indicate the file name offset within the structure.
//
//  The following structure is returned with these fsctls.
//
//      FSCTL_READ_USN_JOURNAL
//      FSCTL_READ_FILE_USN_DATA
//      FSCTL_ENUM_USN_DATA
//

  PUSN_RECORD = ^USN_RECORD;
  USN_RECORD = record
    RecordLength: DWORD;
    MajorVersion: WORD;
    MinorVersion: WORD;
    FileReferenceNumber: DWORDLONG;
    ParentFileReferenceNumber: DWORDLONG;
    Usn: USN;
    TimeStamp: LARGE_INTEGER;
    Reason: DWORD;
    SourceInfo: DWORD;
    SecurityId: DWORD;
    FileAttributes: DWORD;
    FileNameLength: WORD;
    FileNameOffset: WORD;
    FileName: array [0..0] of WCHAR;
  end;
  TUsnRecord = USN_RECORD;
  PUsnRecord = PUSN_RECORD;

const
  USN_PAGE_SIZE = ($1000);

  USN_REASON_DATA_OVERWRITE        = ($00000001);
  USN_REASON_DATA_EXTEND           = ($00000002);
  USN_REASON_DATA_TRUNCATION       = ($00000004);
  USN_REASON_NAMED_DATA_OVERWRITE  = ($00000010);
  USN_REASON_NAMED_DATA_EXTEND     = ($00000020);
  USN_REASON_NAMED_DATA_TRUNCATION = ($00000040);
  USN_REASON_FILE_CREATE           = ($00000100);
  USN_REASON_FILE_DELETE           = ($00000200);
  USN_REASON_EA_CHANGE             = ($00000400);
  USN_REASON_SECURITY_CHANGE       = ($00000800);
  USN_REASON_RENAME_OLD_NAME       = ($00001000);
  USN_REASON_RENAME_NEW_NAME       = ($00002000);
  USN_REASON_INDEXABLE_CHANGE      = ($00004000);
  USN_REASON_BASIC_INFO_CHANGE     = ($00008000);
  USN_REASON_HARD_LINK_CHANGE      = ($00010000);
  USN_REASON_COMPRESSION_CHANGE    = ($00020000);
  USN_REASON_ENCRYPTION_CHANGE     = ($00040000);
  USN_REASON_OBJECT_ID_CHANGE      = ($00080000);
  USN_REASON_REPARSE_POINT_CHANGE  = ($00100000);
  USN_REASON_STREAM_CHANGE         = ($00200000);

  USN_REASON_CLOSE = DWORD($80000000);

//
//  Structure for FSCTL_QUERY_USN_JOUNAL
//

type
  PUSN_JOURNAL_DATA = ^USN_JOURNAL_DATA;
  USN_JOURNAL_DATA = record
    UsnJournalID: DWORDLONG;
    FirstUsn: USN;
    NextUsn: USN;
    LowestValidUsn: USN;
    MaxUsn: USN;
    MaximumSize: DWORDLONG;
    AllocationDelta: DWORDLONG;
  end;
  TUsnJournalData = USN_JOURNAL_DATA;
  PUsnJournalData = PUSN_JOURNAL_DATA;

//
//  Structure for FSCTL_DELETE_USN_JOURNAL
//

  PDELETE_USN_JOURNAL_DATA = ^DELETE_USN_JOURNAL_DATA;
  DELETE_USN_JOURNAL_DATA = record
    UsnJournalID: DWORDLONG;
    DeleteFlags: DWORD;
  end;
  TDeleteUsnJournalData = DELETE_USN_JOURNAL_DATA;
  PDeleteUsnJournalData = PDELETE_USN_JOURNAL_DATA;

const
  USN_DELETE_FLAG_DELETE = ($00000001);
  USN_DELETE_FLAG_NOTIFY = ($00000002);

  USN_DELETE_VALID_FLAGS = ($00000003);

//
//  Structure for FSCTL_MARK_HANDLE
//

type
  PMARK_HANDLE_INFO = ^MARK_HANDLE_INFO;
  MARK_HANDLE_INFO = record
    UsnSourceInfo: DWORD;
    VolumeHandle: HANDLE;
    HandleInfo: DWORD;
  end;
  TMarkHandleInfo = MARK_HANDLE_INFO;
  PMarkHandleInfo = PMARK_HANDLE_INFO;

{$IFDEF _WIN64}

//
//  32/64 Bit thunking support structure
//

  MARK_HANDLE_INFO32 = record
    UsnSourceInfo: DWORD;
    VolumeHandle: UINT32;
    HandleInfo: DWORD;
  end;
  PMARK_HANDLE_INFO32 = ^MARK_HANDLE_INFO32;
  TMarkHandleInfo32 = MARK_HANDLE_INFO32;
  PMarkHandleInfo32 = PMARK_HANDLE_INFO32;

{$ENDIF _WIN64}

//
//  Flags for the additional source information above.
//
//      USN_SOURCE_DATA_MANAGEMENT - Service is not modifying the external view
//          of any part of the file.  Typical case is HSM moving data to
//          and from external storage.
//
//      USN_SOURCE_AUXILIARY_DATA - Service is not modifying the external view
//          of the file with regard to the application that created this file.
//          Can be used to add private data streams to a file.
//
//      USN_SOURCE_REPLICATION_MANAGEMENT - Service is modifying a file to match
//          the contents of the same file which exists in another member of the
//          replica set.
//

const
  USN_SOURCE_DATA_MANAGEMENT        = ($00000001);
  USN_SOURCE_AUXILIARY_DATA         = ($00000002);
  USN_SOURCE_REPLICATION_MANAGEMENT = ($00000004);


//
//  Flags for the HandleInfo field above
//
//  MARK_HANDLE_PROTECT_CLUSTERS - disallow any defragmenting (FSCTL_MOVE_FILE) until the
//      the handle is closed
//

  MARK_HANDLE_PROTECT_CLUSTERS      = $00000001;

//
// Structure for FSCTL_SECURITY_ID_CHECK
//

type
  PBULK_SECURITY_TEST_DATA = ^BULK_SECURITY_TEST_DATA;
  BULK_SECURITY_TEST_DATA = record
    DesiredAccess: ACCESS_MASK;
    SecurityIds: array [0..0] of DWORD;
  end;
  TBulkSecurityTestData = BULK_SECURITY_TEST_DATA;
  PBulkSecurityTestData = PBULK_SECURITY_TEST_DATA;

//
//  Output flags for the FSCTL_IS_VOLUME_DIRTY
//

const
  VOLUME_IS_DIRTY          = ($00000001);
  VOLUME_UPGRADE_SCHEDULED = ($00000002);

//
// Structures for FSCTL_FILE_PREFETCH
//

type
  _FILE_PREFETCH = record
    Type_: DWORD;
    Count: DWORD;
    Prefetch: array [0..0] of DWORDLONG;
  end;
  FILE_PREFETCH = _FILE_PREFETCH;
  PFILE_PREFETCH = ^FILE_PREFETCH;
  TFilePrefetch = FILE_PREFETCH;
  PFilePrefetch = PFILE_PREFETCH;  

const
  FILE_PREFETCH_TYPE_FOR_CREATE = $1;
//
// Structures for FSCTL_FILESYSTEM_GET_STATISTICS
//
// Filesystem performance counters
//

type
  PFILESYSTEM_STATISTICS = ^FILESYSTEM_STATISTICS;
  _FILESYSTEM_STATISTICS = record
    FileSystemType: WORD;
    Version: WORD;                  // currently version 1
    SizeOfCompleteStructure: DWORD; // must by a mutiple of 64 bytes
    UserFileReads: DWORD;
    UserFileReadBytes: DWORD;
    UserDiskReads: DWORD;
    UserFileWrites: DWORD;
    UserFileWriteBytes: DWORD;
    UserDiskWrites: DWORD;
    MetaDataReads: DWORD;
    MetaDataReadBytes: DWORD;
    MetaDataDiskReads: DWORD;
    MetaDataWrites: DWORD;
    MetaDataWriteBytes: DWORD;
    MetaDataDiskWrites: DWORD;
    //
    //  The file system's private structure is appended here.
    //
  end;
  FILESYSTEM_STATISTICS = _FILESYSTEM_STATISTICS;
  TFilesystemStatistics = FILESYSTEM_STATISTICS;
  PFilesystemStatistics = PFILESYSTEM_STATISTICS;

// values for FS_STATISTICS.FileSystemType

const
  FILESYSTEM_STATISTICS_TYPE_NTFS = 1;
  FILESYSTEM_STATISTICS_TYPE_FAT  = 2;

//
//  File System Specific Statistics Data
//

type
  PFAT_STATISTICS = ^FAT_STATISTICS;
  _FAT_STATISTICS = record
    CreateHits: DWORD;
    SuccessfulCreates: DWORD;
    FailedCreates: DWORD;
    NonCachedReads: DWORD;
    NonCachedReadBytes: DWORD;
    NonCachedWrites: DWORD;
    NonCachedWriteBytes: DWORD;
    NonCachedDiskReads: DWORD;
    NonCachedDiskWrites: DWORD;
  end;
  FAT_STATISTICS = _FAT_STATISTICS;
  TFatStatistics = FAT_STATISTICS;
  PFatStatistics = PFAT_STATISTICS;

  // NTFS Statistics

  TMftWritesUserLevel = record
    Write: WORD;
    Create: WORD;
    SetInfo: WORD;
    Flush: WORD;
  end;

  TMft2WritesUserLevel = record
    Write: WORD;
    Create: WORD;
    SetInfo: WORD;
    Flush: WORD;
  end;

  TBitmapWritesUserLevel = record
    Write: WORD;
    Create: WORD;
    SetInfo: WORD;
  end;

  TMftBitmapWritesUserLevel = record
    Write: WORD;
    Create: WORD;
    SetInfo: WORD;
    Flush: WORD;
  end;

  TAllocate = record
    Calls: DWORD;                // number of individual calls to allocate clusters
    Clusters: DWORD;             // number of clusters allocated
    Hints: DWORD;                // number of times a hint was specified

    RunsReturned: DWORD;         // number of runs used to satisify all the requests

    HintsHonored: DWORD;         // number of times the hint was useful
    HintsClusters: DWORD;        // number of clusters allocated via the hint
    Cache: DWORD;                // number of times the cache was useful other than the hint
    CacheClusters: DWORD;        // number of clusters allocated via the cache other than the hint
    CacheMiss: DWORD;            // number of times the cache wasn't useful
    CacheMissClusters: DWORD;    // number of clusters allocated without the cache
  end;

  PNTFS_STATISTICS = ^NTFS_STATISTICS;
  _NTFS_STATISTICS  = record

    LogFileFullExceptions: DWORD;
    OtherExceptions: DWORD;

    //
    // Other meta data io's
    //

    MftReads: DWORD;
    MftReadBytes: DWORD;
    MftWrites: DWORD;
    MftWriteBytes: DWORD;
    MftWritesUserLevel: TMftWritesUserLevel;

    MftWritesFlushForLogFileFull: WORD;
    MftWritesLazyWriter: WORD;
    MftWritesUserRequest: WORD;

    Mft2Writes: DWORD;
    Mft2WriteBytes: DWORD;
    Mft2WritesUserLevel: TMft2WritesUserLevel;

    Mft2WritesFlushForLogFileFull: WORD;
    Mft2WritesLazyWriter: WORD;
    Mft2WritesUserRequest: WORD;

    RootIndexReads: DWORD;
    RootIndexReadBytes: DWORD;
    RootIndexWrites: DWORD;
    RootIndexWriteBytes: DWORD;

    BitmapReads: DWORD;
    BitmapReadBytes: DWORD;
    BitmapWrites: DWORD;
    BitmapWriteBytes: DWORD;

    BitmapWritesFlushForLogFileFull: WORD;
    BitmapWritesLazyWriter: WORD;
    BitmapWritesUserRequest: WORD;

    BitmapWritesUserLevel: TBitmapWritesUserLevel;

    MftBitmapReads: DWORD;
    MftBitmapReadBytes: DWORD;
    MftBitmapWrites: DWORD;
    MftBitmapWriteBytes: DWORD;

    MftBitmapWritesFlushForLogFileFull: WORD;
    MftBitmapWritesLazyWriter: WORD;
    MftBitmapWritesUserRequest: WORD;
    MftBitmapWritesUserLevel: TMftBitmapWritesUserLevel;

    UserIndexReads: DWORD;
    UserIndexReadBytes: DWORD;
    UserIndexWrites: DWORD;
    UserIndexWriteBytes: DWORD;

    //
    // Additions for NT 5.0
    //

    LogFileReads: DWORD;
    LogFileReadBytes: DWORD;
    LogFileWrites: DWORD;
    LogFileWriteBytes: DWORD;

    Allocate: TAllocate;
  end;
  NTFS_STATISTICS = _NTFS_STATISTICS;
  TNTFSStatistics = NTFS_STATISTICS;
  PNTFSStatistics = PNTFS_STATISTICS;

//
// Structure for FSCTL_SET_OBJECT_ID, FSCTL_GET_OBJECT_ID, and FSCTL_CREATE_OR_GET_OBJECT_ID
//

  PFILE_OBJECTID_BUFFER = ^FILE_OBJECTID_BUFFER;
  _FILE_OBJECTID_BUFFER = record

    //
    //  This is the portion of the object id that is indexed.
    //

    ObjectId: array [0..15] of BYTE;

    //
    //  This portion of the object id is not indexed, it's just
    //  some metadata for the user's benefit.
    //

    case Integer of
      0: (
        BirthVolumeId: array [0..15] of BYTE;
        BirthObjectId: array [0..15] of BYTE;
        DomainId: array [0..15] of BYTE);
      1: (
        ExtendedInfo: array [0..47] of BYTE);
  end;
  FILE_OBJECTID_BUFFER = _FILE_OBJECTID_BUFFER;
  TFileObjectIdBuffer = FILE_OBJECTID_BUFFER;
  PFileObjectIdBuffer = PFILE_OBJECTID_BUFFER;

//
// Structure for FSCTL_SET_SPARSE
//

  PFILE_SET_SPARSE_BUFFER = ^FILE_SET_SPARSE_BUFFER;
  _FILE_SET_SPARSE_BUFFER = record
    SetSparse: ByteBool;
  end;
  FILE_SET_SPARSE_BUFFER = _FILE_SET_SPARSE_BUFFER;
  TFileSetSparseBuffer = FILE_SET_SPARSE_BUFFER;
  PFileSetSparseBuffer = PFILE_SET_SPARSE_BUFFER;

//
// Structure for FSCTL_SET_ZERO_DATA
//

  PFILE_ZERO_DATA_INFORMATION = ^FILE_ZERO_DATA_INFORMATION;
  _FILE_ZERO_DATA_INFORMATION = record
    FileOffset: LARGE_INTEGER;
    BeyondFinalZero: LARGE_INTEGER;
  end;
  FILE_ZERO_DATA_INFORMATION = _FILE_ZERO_DATA_INFORMATION;
  TFileZeroDataInformation = FILE_ZERO_DATA_INFORMATION;
  PFileZeroDataInformation = PFILE_ZERO_DATA_INFORMATION;

//
// Structure for FSCTL_QUERY_ALLOCATED_RANGES
//

//
// Querying the allocated ranges requires an output buffer to store the
// allocated ranges and an input buffer to specify the range to query.
// The input buffer contains a single entry, the output buffer is an
// array of the following structure.
//

  PFILE_ALLOCATED_RANGE_BUFFER = ^FILE_ALLOCATED_RANGE_BUFFER;
  _FILE_ALLOCATED_RANGE_BUFFER = record
    FileOffset: LARGE_INTEGER;
    Length: LARGE_INTEGER;
  end;
  FILE_ALLOCATED_RANGE_BUFFER = _FILE_ALLOCATED_RANGE_BUFFER;
  TFileAllocatedRangeBuffer = FILE_ALLOCATED_RANGE_BUFFER;
  PFileAllocatedRangeBuffer = PFILE_ALLOCATED_RANGE_BUFFER;

//
// Structures for FSCTL_SET_ENCRYPTION, FSCTL_WRITE_RAW_ENCRYPTED, and FSCTL_READ_RAW_ENCRYPTED
//

//
//  The input buffer to set encryption indicates whether we are to encrypt/decrypt a file
//  or an individual stream.
//

  PENCRYPTION_BUFFER = ^ENCRYPTION_BUFFER;
  _ENCRYPTION_BUFFER = record
    EncryptionOperation: DWORD;
    Private_: array [0..0] of BYTE;
  end;
  ENCRYPTION_BUFFER = _ENCRYPTION_BUFFER;
  TEncryptionBuffer = ENCRYPTION_BUFFER;
  PEncryptionBuffer = PENCRYPTION_BUFFER;

const
  FILE_SET_ENCRYPTION      = $00000001;
  FILE_CLEAR_ENCRYPTION    = $00000002;
  STREAM_SET_ENCRYPTION    = $00000003;
  STREAM_CLEAR_ENCRYPTION  = $00000004;

  MAXIMUM_ENCRYPTION_VALUE = $00000004;

//
//  The optional output buffer to set encryption indicates that the last encrypted
//  stream in a file has been marked as decrypted.
//

type
  PDECRYPTION_STATUS_BUFFER = ^DECRYPTION_STATUS_BUFFER;
  _DECRYPTION_STATUS_BUFFER = record
    NoEncryptedStreams: ByteBool;
  end;
  DECRYPTION_STATUS_BUFFER = _DECRYPTION_STATUS_BUFFER;
  TDecryptionStatusBuffer = DECRYPTION_STATUS_BUFFER;
  PDecryptionStatusBuffer = PDECRYPTION_STATUS_BUFFER;

const
  ENCRYPTION_FORMAT_DEFAULT = ($01);

  COMPRESSION_FORMAT_SPARSE = ($4000);

//
//  Request Encrypted Data structure.  This is used to indicate
//  the range of the file to read.  It also describes the
//  output buffer used to return the data.
//

type
  PREQUEST_RAW_ENCRYPTED_DATA = ^REQUEST_RAW_ENCRYPTED_DATA;
  _REQUEST_RAW_ENCRYPTED_DATA = record
    //
    //  Requested file offset and requested length to read.
    //  The fsctl will round the starting offset down
    //  to a file system boundary.  It will also
    //  round the length up to a file system boundary.
    //
    FileOffset: LONGLONG;
    Length: DWORD;
  end;
  REQUEST_RAW_ENCRYPTED_DATA = _REQUEST_RAW_ENCRYPTED_DATA;
  TRequestRawEncryptedData = REQUEST_RAW_ENCRYPTED_DATA;
  PRequestRawEncryptedData = PREQUEST_RAW_ENCRYPTED_DATA;

//
//  Encrypted Data Information structure.  This structure
//  is used to return raw encrypted data from a file in
//  order to perform off-line recovery.  The data will be
//  encrypted or encrypted and compressed.  The off-line
//  service will need to use the encryption and compression
//  format information to recover the file data.  In the
//  event that the data is both encrypted and compressed then
//  the decryption must occur before decompression.  All
//  the data units below must be encrypted and compressed
//  with the same format.
//
//  The data will be returned in units.  The data unit size
//  will be fixed per request.  If the data is compressed
//  then the data unit size will be the compression unit size.
//
//  This structure is at the beginning of the buffer used to
//  return the encrypted data.  The actual raw bytes from
//  the file will follow this buffer.  The offset of the
//  raw bytes from the beginning of this structure is
//  specified in the REQUEST_RAW_ENCRYPTED_DATA structure
//  described above.
//

  PENCRYPTED_DATA_INFO = ^ENCRYPTED_DATA_INFO;
  _ENCRYPTED_DATA_INFO = record

    //
    //  This is the file offset for the first entry in the
    //  data block array.  The file system will round
    //  the requested start offset down to a boundary
    //  that is consistent with the format of the file.
    //

    StartingFileOffset: DWORDLONG;

    //
    //  Data offset in output buffer.  The output buffer
    //  begins with an ENCRYPTED_DATA_INFO structure.
    //  The file system will then store the raw bytes from
    //  disk beginning at the following offset within the
    //  output buffer.
    //

    OutputBufferOffset: DWORD;

    //
    //  The number of bytes being returned that are within
    //  the size of the file.  If this value is less than
    //  (NumberOfDataBlocks << DataUnitShift), it means the
    //  end of the file occurs within this transfer.  Any
    //  data beyond file size is invalid and was never
    //  passed to the encryption driver.
    //

    BytesWithinFileSize: DWORD;

    //
    //  The number of bytes being returned that are below
    //  valid data length.  If this value is less than
    //  (NumberOfDataBlocks << DataUnitShift), it means the
    //  end of the valid data occurs within this transfer.
    //  After decrypting the data from this transfer, any
    //  byte(s) beyond valid data length must be zeroed.
    //

    BytesWithinValidDataLength: DWORD;

    //
    //  Code for the compression format as defined in
    //  ntrtl.h.  Note that COMPRESSION_FORMAT_NONE
    //  and COMPRESSION_FORMAT_DEFAULT are invalid if
    //  any of the described chunks are compressed.
    //

    CompressionFormat: WORD;

    //
    //  The DataUnit is the granularity used to access the
    //  disk.  It will be the same as the compression unit
    //  size for a compressed file.  For an uncompressed
    //  file, it will be some cluster-aligned power of 2 that
    //  the file system deems convenient.  A caller should
    //  not expect that successive calls will have the
    //  same data unit shift value as the previous call.
    //
    //  Since chunks and compression units are expected to be
    //  powers of 2 in size, we express them log2.  So, for
    //  example (1 << ChunkShift) == ChunkSizeInBytes.  The
    //  ClusterShift indicates how much space must be saved
    //  to successfully compress a compression unit - each
    //  successfully compressed data unit must occupy
    //  at least one cluster less in bytes than an uncompressed
    //  data block unit.
    //

    DataUnitShift: BYTE;
    ChunkShift: BYTE;
    ClusterShift: BYTE;

    //
    //  The format for the encryption.
    //

    EncryptionFormat: BYTE;

    //
    //  This is the number of entries in the data block size
    //  array.
    //

    NumberOfDataBlocks: WORD;

    //
    //  This is an array of sizes in the data block array.  There
    //  must be one entry in this array for each data block
    //  read from disk.  The size has a different meaning
    //  depending on whether the file is compressed.
    //
    //  A size of zero always indicates that the final data consists entirely
    //  of zeroes.  There is no decryption or decompression to
    //  perform.
    //
    //  If the file is compressed then the data block size indicates
    //  whether this block is compressed.  A size equal to
    //  the block size indicates that the corresponding block did
    //  not compress.  Any other non-zero size indicates the
    //  size of the compressed data which needs to be
    //  decrypted/decompressed.
    //
    //  If the file is not compressed then the data block size
    //  indicates the amount of data within the block that
    //  needs to be decrypted.  Any other non-zero size indicates
    //  that the remaining bytes in the data unit within the file
    //  consists of zeros.  An example of this is when the
    //  the read spans the valid data length of the file.  There
    //  is no data to decrypt past the valid data length.
    //

    DataBlockSize: array [0..ANYSIZE_ARRAY - 1] of DWORD;
  end;
  ENCRYPTED_DATA_INFO = _ENCRYPTED_DATA_INFO;
  TEncryptedDataInfo = ENCRYPTED_DATA_INFO;
  PEncryptedDataInfo = PENCRYPTED_DATA_INFO;

//
//  FSCTL_READ_FROM_PLEX support
//  Request Plex Read Data structure.  This is used to indicate
//  the range of the file to read.  It also describes
//  which plex to perform the read from.
//

  _PLEX_READ_DATA_REQUEST = record

    //
    //  Requested offset and length to read.
    //  The offset can be the virtual offset (vbo) in to a file,
    //  or a volume. In the case of a file offset,
    //  the fsd will round the starting offset down
    //  to a file system boundary.  It will also
    //  round the length up to a file system boundary and
    //  enforce any other applicable limits.
    //

    ByteOffset: LARGE_INTEGER;
    ByteLength: DWORD;
    PlexNumber: DWORD;
  end;
  PLEX_READ_DATA_REQUEST = _PLEX_READ_DATA_REQUEST;
  PPLEX_READ_DATA_REQUEST = ^PLEX_READ_DATA_REQUEST;
  TPlexReadDataRequest = PLEX_READ_DATA_REQUEST;
  PPlexReadDataRequest = PPLEX_READ_DATA_REQUEST;  

//
// FSCTL_SIS_COPYFILE support
// Source and destination file names are passed in the FileNameBuffer.
// Both strings are null terminated, with the source name starting at
// the beginning of FileNameBuffer, and the destination name immediately
// following.  Length fields include terminating nulls.
//

  PSI_COPYFILE = ^SI_COPYFILE;
  _SI_COPYFILE = record
    SourceFileNameLength: DWORD;
    DestinationFileNameLength: DWORD;
    Flags: DWORD;
    FileNameBuffer: array [0..0] of WCHAR;
  end;
  SI_COPYFILE = _SI_COPYFILE;
  TSiCopyfile = SI_COPYFILE;
  PSiCopyfile = PSI_COPYFILE;

const
  COPYFILE_SIS_LINK    = $0001; // Copy only if source is SIS
  COPYFILE_SIS_REPLACE = $0002; // Replace destination if it exists, otherwise don't.
  COPYFILE_SIS_FLAGS   = $0003;

//
// These IOCTLs are handled by hard disk volumes.
//

  IOCTL_VOLUME_BASE = DWORD('V');

  IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS = (
    (IOCTL_VOLUME_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    (0 shl 2) or METHOD_BUFFERED);

  IOCTL_VOLUME_IS_CLUSTERED = (
    (IOCTL_VOLUME_BASE shl 16) or (FILE_ANY_ACCESS shl 14) or
    (12 shl 2) or METHOD_BUFFERED);

//
// Disk extent definition.
//

type
  PDISK_EXTENT = ^DISK_EXTENT;
  _DISK_EXTENT = record
    DiskNumber: DWORD;
    StartingOffset: LARGE_INTEGER;
    ExtentLength: LARGE_INTEGER;
  end;
  DISK_EXTENT = _DISK_EXTENT;
  TDiskExtent = DISK_EXTENT;
  PDiskExtent = PDISK_EXTENT;

//
// Output structure for IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS.
//

  PVOLUME_DISK_EXTENTS = ^VOLUME_DISK_EXTENTS;
  _VOLUME_DISK_EXTENTS = record
    NumberOfDiskExtents: DWORD;
    Extents: array [0..0] of DISK_EXTENT;
  end;
  VOLUME_DISK_EXTENTS = _VOLUME_DISK_EXTENTS;
  TVolumeDiskExtents = VOLUME_DISK_EXTENTS;
  PVolumeDiskExtents = PVOLUME_DISK_EXTENTS;

implementation

function CTL_CODE(DeviceType, Func, Method, Access: WORD): DWORD;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (Func shl 2) or Method;
end;

function DEVICE_TYPE_FROM_CTL_CODE(CtrlCode: DWORD): WORD;
begin
  Result := (CtrlCode and DWORD($FFFF0000)) shr 16;
end;

{
#define IsRecognizedPartition( PartitionType ) (    \
     ((PartitionType & PARTITION_NTFT) && ((PartitionType & ~0xC0) == PARTITION_FAT_12)) ||  \
     ((PartitionType & PARTITION_NTFT) && ((PartitionType & ~0xC0) == PARTITION_IFS)) ||  \
     ((PartitionType & PARTITION_NTFT) && ((PartitionType & ~0xC0) == PARTITION_HUGE)) ||  \
     ((PartitionType & PARTITION_NTFT) && ((PartitionType & ~0xC0) == PARTITION_FAT32)) ||  \
     ((PartitionType & PARTITION_NTFT) && ((PartitionType & ~0xC0) == PARTITION_FAT32_XINT13)) ||  \
     ((PartitionType & PARTITION_NTFT) && ((PartitionType & ~0xC0) == PARTITION_XINT13)) ||  \
     ((PartitionType) == PARTITION_FAT_12)          ||  \
     ((PartitionType) == PARTITION_FAT_16)          ||  \
     ((PartitionType) == PARTITION_IFS)             ||  \
     ((PartitionType) == PARTITION_HUGE)            ||  \
     ((PartitionType) == PARTITION_FAT32)           ||  \
     ((PartitionType) == PARTITION_FAT32_XINT13)    ||  \
     ((PartitionType) == PARTITION_XINT13) )
}

function IsRecognizedPartition(PartitionType: DWORD): Boolean;
begin
  Result :=
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT_12)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_IFS)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_HUGE)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT32)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_FAT32_XINT13)) or
    (((PartitionType and PARTITION_NTFT) <> 0) and ((PartitionType and not $C0) = PARTITION_XINT13)) or
    ((PartitionType) = PARTITION_FAT_12) or
    ((PartitionType) = PARTITION_FAT_16) or
    ((PartitionType) = PARTITION_IFS) or
    ((PartitionType) = PARTITION_HUGE) or
    ((PartitionType) = PARTITION_FAT32) or
    ((PartitionType) = PARTITION_FAT32_XINT13) or
    ((PartitionType) = PARTITION_XINT13);
end;

function IsContainerPartition(PartitionType: DWORD): Boolean;
begin
  Result :=
    (PartitionType = PARTITION_EXTENDED) or
    (PartitionType = PARTITION_XINT13_EXTENDED);
end;

function IsFTPartition(PartitionType: DWORD): Boolean;
begin
{     (((PartitionType)&PARTITION_NTFT) && IsRecognizedPartition(PartitionType)) }
  Result := ((PartitionType and PARTITION_NTFT) = PARTITION_NTFT) and IsRecognizedPartition(PartitionType);
end;

function DiskGeometryGetPartition(Geometry: DWORD): PDISK_PARTITION_INFO;
begin
//((PDISK_PARTITION_INFO)((Geometry)->Data))
//  Result := PDISK_PARTITION_INFO(Geometry)^.Data;
  Assert(False, 'TODO Not implemented');
  Result := nil;
end;

function DiskGeometryGetDetect(Geometry: DWORD): PDISK_DETECTION_INFO;
begin
//((PDISK_DETECTION_INFO)(((DWORD_PTR)DiskGeometryGetPartition(Geometry)+\
//DiskGeometryGetPartition(Geometry)->SizeOfPartitionInfo)))
  Result := PDISK_DETECTION_INFO(DWORD_PTR(DiskGeometryGetPartition(Geometry)) + DiskGeometryGetPartition(Geometry)^.SizeOfPartitionInfo);
end;

end.
