{******************************************************************************}
{                                                       	               }
{ Windows Installer API interface Unit for Object Pascal                       }
{                                                       	               }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{ 								               }
{ The original file is: msidefs.h, released June 2000. The original Pascal     }
{ code is: MsiDefs.pas, released June 2001. The initial developer of the       }
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

unit JwaMsiDefs;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "msidefs.h"'}
{$HPPEMIT ''}

{$I WINDEFINES.INC}

interface

uses
  JwaWinType;

//------------------------------------------------------------------------------
// INSTALLER PROPERTY DEFINITIONS
//------------------------------------------------------------------------------

const

// Required properties: set in Property table

  IPROPNAME_PRODUCTNAME     = TEXT('ProductName'); // name registered for display
  IPROPNAME_PRODUCTCODE     = TEXT('ProductCode'); // unique string GUID for product
  IPROPNAME_PRODUCTVERSION  = TEXT('ProductVersion'); // string product version
  IPROPNAME_INSTALLLANGUAGE = TEXT('ProductLanguage'); // install language of product, use to load resources
  IPROPNAME_MANUFACTURER    = TEXT('Manufacturer'); // name of manufacturer

// Customization properties: set in Property table

  IPROPNAME_UPGRADECODE       = TEXT('UpgradeCode'); // unique string GUID for product family
  IPROPNAME_PIDTEMPLATE       = TEXT('PIDTemplate'); // drives Product ID processing
  IPROPNAME_DISKPROMPT        = TEXT('DiskPrompt'); // prompt for CD
  IPROPNAME_LEFTUNIT          = TEXT('LeftUnit'); // name of unit placed to left of number instead of right
  IPROPNAME_ADMIN_PROPERTIES  = TEXT('AdminProperties'); // properties to stuff in admin package
  IPROPNAME_DEFAULTUIFONT     = TEXT('DefaultUIFont'); // the font used in the UI if no other font is specified
  IPROPNAME_ALLOWEDPROPERTIES = TEXT('SecureCustomProperties');
  IPROPNAME_ENABLEUSERCONTROL = TEXT('EnableUserControl'); // allows user to specify any public property
  IPROPNAME_HIDDEN_PROPERTIES = TEXT('MsiHiddenProperties');  // properties that should not be dumped into the log file

// Customization properties: set on command-line or in Property table

  IPROPNAME_USERNAME     = TEXT('USERNAME');
  IPROPNAME_COMPANYNAME  = TEXT('COMPANYNAME');
  IPROPNAME_PIDKEY       = TEXT('PIDKEY'); // used with PIDTemplate to form ProductID
  IPROPNAME_PATCH        = TEXT('PATCH'); // patch package to apply - SET BY INSTALLER
  IPROPNAME_TARGETDIR    = TEXT('TARGETDIR'); // target location - defaults to ROOTDRIVE
  IPROPNAME_ACTION       = TEXT('ACTION'); // top-level action to perform - default to INSTALL
  IPROPNAME_LIMITUI      = TEXT('LIMITUI'); // limit ui level to Basic
  IPROPNAME_LOGACTION    = TEXT('LOGACTION'); // log only these actions
  IPROPNAME_ALLUSERS     = TEXT('ALLUSERS'); // install for all users
  IPROPNAME_INSTALLLEVEL = TEXT('INSTALLLEVEL');
  IPROPNAME_REBOOT       = TEXT('REBOOT'); // force or suppress reboot

  IPROPNAME_REBOOTPROMPT = TEXT('REBOOTPROMPT'); // allow or suppress reboot prompt

  IPROPNAME_EXECUTEMODE          = TEXT('EXECUTEMODE'); // NONE or SCRIPT
  IPROPVALUE_EXECUTEMODE_NONE    = TEXT('NONE'); // do not update system
  IPROPVALUE_EXECUTEMODE_SCRIPT  = TEXT('SCRIPT'); // default - run script to update system
  IPROPNAME_EXECUTEACTION        = TEXT('EXECUTEACTION'); // run action on server side
  IPROPNAME_SOURCELIST           = TEXT('SOURCELIST');
  IPROPNAME_ROOTDRIVE            = TEXT('ROOTDRIVE'); // default drive to install - SET BY INSTALLER
  IPROPNAME_TRANSFORMS           = TEXT('TRANSFORMS'); // transforms to apply
  IPROPNAME_TRANSFORMSATSOURCE   = TEXT('TRANSFORMSATSOURCE'); // transforms can be found at source
  IPROPNAME_TRANSFORMSSECURE     = TEXT('TRANSFORMSSECURE'); // file transforms are secured
  IPROPNAME_SEQUENCE             = TEXT('SEQUENCE'); // sequence table to run with SEQUENCE action
  IPROPNAME_SHORTFILENAMES       = TEXT('SHORTFILENAMES'); // force short file names
  IPROPNAME_PRIMARYFOLDER        = TEXT('PRIMARYFOLDER'); // Folder on the volume the author wants costing info for
  IPROPNAME_AFTERREBOOT          = TEXT('AFTERREBOOT'); // install is after a ForceReboot triggered reboot
  IPROPNAME_NOCOMPANYNAME        = TEXT('NOCOMPANYNAME');
  IPROPNAME_NOUSERNAME           = TEXT('NOUSERNAME');
  IPROPNAME_DISABLEROLLBACK      = TEXT('DISABLEROLLBACK'); // disable rollback for this install
  IPROPNAME_AVAILABLEFREEREG     = TEXT('AVAILABLEFREEREG'); // set up the free space in the registry before commencing the install
  IPROPNAME_DISABLEADVTSHORTCUTS = TEXT('DISABLEADVTSHORTCUTS'); // disable creating darwin shortcuts even if supported
  IPROPNAME_PATCHNEWPACKAGECODE  = TEXT('PATCHNEWPACKAGECODE'); // added to property table by patch transforms - used to update
  // PackageCode of admin packages when patching admin installs
  IPROPNAME_PATCHNEWSUMMARYSUBJECT = TEXT('PATCHNEWSUMMARYSUBJECT'); // added to property table by patch transforms - used to update
  // Subject summary info property of admin packages when patching admin installs
  IPROPNAME_PATCHNEWSUMMARYCOMMENTS = TEXT('PATCHNEWSUMMARYCOMMENTS'); // added to property table by patch transforms - used to update
  // Comments summary info property of admin packages when patching admin installs
  IPROPNAME_PRODUCTLANGUAGE = TEXT('PRODUCTLANGUAGE'); // requested language, must be one in summary information list, selects language transform

  IPROPNAME_CHECKCRCS         = TEXT('MSICHECKCRCS');       // requests Darwin to check CRCs after copying, moving, patching & duplicating files.
  IPROPNAME_MSINODISABLEMEDIA = TEXT('MSINODISABLEMEDIA');  // if set, DISABLEMEDIA won't be set in the AdminProperties stream during an admin install of
// A PACKAGE WITH COMPRESSED SOURCE

// property used for URT bootstrapping

  IPROPNAME_CARRYINGNDP	                = TEXT('CARRYINGNDP');
  IPROPVALUE__CARRYINGNDP_URTREINSTALL  = TEXT('URTREINSTALL');   // reinstalling/ uninstalling core URT files
  IPROPVALUE__CARRYINGNDP_URTUPGRADE    = TEXT('URTUPGRADE');     // upgrading core URT files

// property used for multiple instance support

  IPROPNAME_MSINEWINSTANCE = TEXT('MSINEWINSTANCE');
  IPROPNAME_MSIINSTANCEGUID = TEXT('MSIINSTANCEGUID');

// properties used for URL download reduction for admins

  IPROPNAME_MSIPACKAGEDOWNLOADLOCALCOPY = TEXT('MSIPACKAGEDOWNLOADLOCALCOPY');
  IPROPNAME_MSIPATCHDOWNLOADLOCALCOPY = TEXT('MSIPATCHDOWNLOADLOCALCOPY');

// Properties used to populate Add/Remove Control Panel values

  IPROPNAME_ARPAUTHORIZEDCDFPREFIX = TEXT('ARPAUTHORIZEDCDFPREFIX');
  IPROPNAME_ARPCOMMENTS            = TEXT('ARPCOMMENTS');
  IPROPNAME_ARPCONTACT             = TEXT('ARPCONTACT');
  IPROPNAME_ARPHELPLINK            = TEXT('ARPHELPLINK');
  IPROPNAME_ARPHELPTELEPHONE       = TEXT('ARPHELPTELEPHONE');
  IPROPNAME_ARPINSTALLLOCATION     = TEXT('ARPINSTALLLOCATION');
  IPROPNAME_ARPNOMODIFY            = TEXT('ARPNOMODIFY');
  IPROPNAME_ARPNOREMOVE            = TEXT('ARPNOREMOVE');
  IPROPNAME_ARPNOREPAIR            = TEXT('ARPNOREPAIR');
  IPROPNAME_ARPREADME              = TEXT('ARPREADME');
  IPROPNAME_ARPSIZE                = TEXT('ARPSIZE');
  IPROPNAME_ARPSYSTEMCOMPONENT     = TEXT('ARPSYSTEMCOMPONENT');
  IPROPNAME_ARPURLINFOABOUT        = TEXT('ARPURLINFOABOUT');
  IPROPNAME_ARPURLUPDATEINFO       = TEXT('ARPURLUPDATEINFO');
  IPROPNAME_ARPPRODUCTICON = TEXT('ARPPRODUCTICON');

// Dynamic properties set by installer during install

  IPROPNAME_INSTALLED                    = TEXT('Installed'); // product already installed
  IPROPNAME_PRODUCTSTATE                 = TEXT('ProductState'); // state of product (installed,advertised,etc...)
  IPROPNAME_PRESELECTED                  = TEXT('Preselected'); // selections made on command line
  IPROPNAME_RESUME                       = TEXT('RESUME'); // resuming suspended install
  IPROPNAME_UPDATESTARTED                = TEXT('UpdateStarted'); // have begun to update system
  IPROPNAME_PRODUCTID                    = TEXT('ProductID'); // the complete validated Product ID
  IPROPNAME_OUTOFDISKSPACE               = TEXT('OutOfDiskSpace');
  IPROPNAME_OUTOFNORBDISKSPACE           = TEXT('OutOfNoRbDiskSpace');
  IPROPNAME_COSTINGCOMPLETE              = TEXT('CostingComplete');
  IPROPNAME_SOURCEDIR                    = TEXT('SourceDir'); // source location - SET BY INSTALLER
  IPROPNAME_REPLACEDINUSEFILES           = TEXT('ReplacedInUseFiles'); // need reboot to completely install one or more files
  IPROPNAME_PRIMARYFOLDER_PATH           = TEXT('PrimaryVolumePath');
  IPROPNAME_PRIMARYFOLDER_SPACEAVAILABLE = TEXT('PrimaryVolumeSpaceAvailable');
  IPROPNAME_PRIMARYFOLDER_SPACEREQUIRED  = TEXT('PrimaryVolumeSpaceRequired');
  IPROPNAME_PRIMARYFOLDER_SPACEREMAINING = TEXT('PrimaryVolumeSpaceRemaining');
  IPROPNAME_ISADMINPACKAGE               = TEXT('IsAdminPackage');
  IPROPNAME_ROLLBACKDISABLED             = TEXT('RollbackDisabled');
  IPROPNAME_RESTRICTEDUSERCONTROL        = TEXT('RestrictedUserControl');

// Dynamic properties evaluated upon use

  IPROPNAME_TIME     = TEXT('Time');
  IPROPNAME_DATE     = TEXT('Date');
  IPROPNAME_DATETIME = TEXT('DateTime');

// Hardware properties: set by installer at initialization

  IPROPNAME_INTEL = TEXT('Intel');
//#if (_WIN32_MSI >= 150)
  IPROPNAME_AMD64          = TEXT('AMD64');
  IPROPNAME_INTEL64        = TEXT('Intel64');
//#else // (_WIN32_MSI >= 150)
  IPROPNAME_IA64           = TEXT('IA64');
//#endif // (_WIN32_MSI >= 150)
  IPROPNAME_TEXTHEIGHT     = TEXT('TextHeight');
  IPROPNAME_SCREENX        = TEXT('ScreenX');
  IPROPNAME_SCREENY        = TEXT('ScreenY');
  IPROPNAME_CAPTIONHEIGHT  = TEXT('CaptionHeight');
  IPROPNAME_BORDERTOP      = TEXT('BorderTop');
  IPROPNAME_BORDERSIDE     = TEXT('BorderSide');
  IPROPNAME_COLORBITS      = TEXT('ColorBits');
  IPROPNAME_PHYSICALMEMORY = TEXT('PhysicalMemory');
  IPROPNAME_VIRTUALMEMORY  = TEXT('VirtualMemory');
  IPROPNAME_TEXTHEIGHT_CORRECTION = TEXT('TextHeightCorrection');

// Operating System properties: set by installer at initialization

  IPROPNAME_VERSIONNT        = TEXT('VersionNT');
  IPROPNAME_VERSION9X        = TEXT('Version9X');
  IPROPNAME_VERSIONNT64      = TEXT('VersionNT64');
  IPROPNAME_WINDOWSBUILD     = TEXT('WindowsBuild');
  IPROPNAME_SERVICEPACKLEVEL = TEXT('ServicePackLevel');
  IPROPNAME_SERVICEPACKLEVELMINOR = TEXT('ServicePackLevelMinor');
  IPROPNAME_SHAREDWINDOWS    = TEXT('SharedWindows');
  IPROPNAME_COMPUTERNAME     = TEXT('ComputerName');
  IPROPNAME_SHELLADVTSUPPORT = TEXT('ShellAdvtSupport');
  IPROPNAME_OLEADVTSUPPORT   = TEXT('OLEAdvtSupport');
  IPROPNAME_SYSTEMLANGUAGEID = TEXT('SystemLanguageID');
  IPROPNAME_TTCSUPPORT       = TEXT('TTCSupport');
  IPROPNAME_TERMSERVER       = TEXT('TerminalServer');
  IPROPNAME_REMOTEADMINTS        = TEXT('RemoteAdminTS');
  IPROPNAME_REDIRECTEDDLLSUPPORT = TEXT('RedirectedDllSupport');
  IPROPNAME_NTPRODUCTTYPE                  = TEXT('MsiNTProductType');
  IPROPNAME_NTSUITEBACKOFFICE              = TEXT('MsiNTSuiteBackOffice');
  IPROPNAME_NTSUITEDATACENTER              = TEXT('MsiNTSuiteDataCenter');
  IPROPNAME_NTSUITEENTERPRISE              = TEXT('MsiNTSuiteEnterprise');
  IPROPNAME_NTSUITESMALLBUSINESS           = TEXT('MsiNTSuiteSmallBusiness');
  IPROPNAME_NTSUITESMALLBUSINESSRESTRICTED = TEXT('MsiNTSuiteSmallBusinessRestricted');
  IPROPNAME_NTSUITEPERSONAL                = TEXT('MsiNTSuitePersonal');
  IPROPNAME_NTSUITEWEBSERVER               = TEXT('MsiNTSuiteWebServer');
  IPROPNAME_NETASSEMBLYSUPPORT             = TEXT('MsiNetAssemblySupport');
  IPROPNAME_WIN32ASSEMBLYSUPPORT           = TEXT('MsiWin32AssemblySupport');

// User properties: set by installer at initialization

  IPROPNAME_LOGONUSER      = TEXT('LogonUser');
  IPROPNAME_USERSID        = TEXT('UserSID');
  IPROPNAME_ADMINUSER      = TEXT('AdminUser');
  IPROPNAME_USERLANGUAGEID = TEXT('UserLanguageID');
  IPROPNAME_PRIVILEGED     = TEXT('Privileged');

// System folder properties: set by installer at initialization

  IPROPNAME_WINDOWS_FOLDER      = TEXT('WindowsFolder');
  IPROPNAME_SYSTEM_FOLDER       = TEXT('SystemFolder');
  IPROPNAME_SYSTEM16_FOLDER     = TEXT('System16Folder');
  IPROPNAME_WINDOWS_VOLUME      = TEXT('WindowsVolume');
  IPROPNAME_TEMP_FOLDER         = TEXT('TempFolder');
  IPROPNAME_PROGRAMFILES_FOLDER = TEXT('ProgramFilesFolder');
  IPROPNAME_COMMONFILES_FOLDER  = TEXT('CommonFilesFolder');
  IPROPNAME_SYSTEM64_FOLDER     = TEXT('System64Folder');
  IPROPNAME_PROGRAMFILES64_FOLDER = TEXT('ProgramFiles64Folder');
  IPROPNAME_COMMONFILES64_FOLDER = TEXT('CommonFiles64Folder');
  IPROPNAME_STARTMENU_FOLDER    = TEXT('StartMenuFolder');
  IPROPNAME_PROGRAMMENU_FOLDER  = TEXT('ProgramMenuFolder');
  IPROPNAME_STARTUP_FOLDER      = TEXT('StartupFolder');
  IPROPNAME_NETHOOD_FOLDER      = TEXT('NetHoodFolder');
  IPROPNAME_PERSONAL_FOLDER     = TEXT('PersonalFolder');
  IPROPNAME_SENDTO_FOLDER       = TEXT('SendToFolder');
  IPROPNAME_DESKTOP_FOLDER      = TEXT('DesktopFolder');
  IPROPNAME_TEMPLATE_FOLDER     = TEXT('TemplateFolder');
  IPROPNAME_FONTS_FOLDER        = TEXT('FontsFolder');
  IPROPNAME_FAVORITES_FOLDER    = TEXT('FavoritesFolder');
  IPROPNAME_RECENT_FOLDER       = TEXT('RecentFolder');
  IPROPNAME_APPDATA_FOLDER      = TEXT('AppDataFolder');
  IPROPNAME_PRINTHOOD_FOLDER    = TEXT('PrintHoodFolder');
  IPROPNAME_ADMINTOOLS_FOLDER    = TEXT('AdminToolsFolder');
  IPROPNAME_COMMONAPPDATA_FOLDER = TEXT('CommonAppDataFolder');
  IPROPNAME_LOCALAPPDATA_FOLDER  = TEXT('LocalAppDataFolder');
  IPROPNAME_MYPICTURES_FOLDER    = TEXT('MyPicturesFolder');

// Feature/Component installation properties: set on command-line

  IPROPNAME_FEATUREADDLOCAL   = TEXT('ADDLOCAL');
  IPROPNAME_FEATUREADDSOURCE  = TEXT('ADDSOURCE');
  IPROPNAME_FEATUREADDDEFAULT = TEXT('ADDDEFAULT');
  IPROPNAME_FEATUREREMOVE     = TEXT('REMOVE');
  IPROPNAME_FEATUREADVERTISE  = TEXT('ADVERTISE');
  IPROPVALUE_FEATURE_ALL      = TEXT('ALL');

  IPROPNAME_COMPONENTADDLOCAL   = TEXT('COMPADDLOCAL');
  IPROPNAME_COMPONENTADDSOURCE  = TEXT('COMPADDSOURCE');
  IPROPNAME_COMPONENTADDDEFAULT = TEXT('COMPADDDEFAULT');

  IPROPNAME_FILEADDLOCAL   = TEXT('FILEADDLOCAL');
  IPROPNAME_FILEADDSOURCE  = TEXT('FILEADDSOURCE');
  IPROPNAME_FILEADDDEFAULT = TEXT('FILEADDDEFAULT');

  IPROPNAME_REINSTALL          = TEXT('REINSTALL');
  IPROPNAME_REINSTALLMODE      = TEXT('REINSTALLMODE');
  IPROPNAME_PROMPTROLLBACKCOST = TEXT('PROMPTROLLBACKCOST');
  IPROPVALUE_RBCOST_PROMPT     = TEXT('P');
  IPROPVALUE_RBCOST_SILENT     = TEXT('D');
  IPROPVALUE_RBCOST_FAIL       = TEXT('F');

// Property for custom actions to communicate

  IPROPNAME_CUSTOMACTIONDATA = TEXT('CustomActionData');

//------------------------------------------------------------------------------
// TOP-LEVEL ACTION NAMES
//------------------------------------------------------------------------------

  IACTIONNAME_INSTALL         = TEXT('INSTALL');
  IACTIONNAME_ADVERTISE       = TEXT('ADVERTISE');
  IACTIONNAME_ADMIN           = TEXT('ADMIN');
  IACTIONNAME_SEQUENCE        = TEXT('SEQUENCE');
  IACTIONNAME_COLLECTUSERINFO = TEXT('CollectUserInfo');
  IACTIONNAME_FIRSTRUN        = TEXT('FirstRun');

//------------------------------------------------------------------------------
//  SummaryInformation property stream property IDs
//------------------------------------------------------------------------------

// standard property definitions, from OLE2 documentation

  PID_DICTIONARY   = 0; // integer count + array of entries
  PID_CODEPAGE     = 1; // short integer
  PID_TITLE        = 2; // string
  PID_SUBJECT      = 3; // string
  PID_AUTHOR       = 4; // string
  PID_KEYWORDS     = 5; // string
  PID_COMMENTS     = 6; // string
  PID_TEMPLATE     = 7; // string
  PID_LASTAUTHOR   = 8; // string
  PID_REVNUMBER    = 9; // string
  PID_EDITTIME     = 10; // datatime
  PID_LASTPRINTED  = 11; // datetime
  PID_CREATE_DTM   = 12; // datetime
  PID_LASTSAVE_DTM = 13; // datetime
  PID_PAGECOUNT    = 14; // integer
  PID_WORDCOUNT    = 15; // integer
  PID_CHARCOUNT    = 16; // integer
  PID_THUMBNAIL    = 17; // clipboard format + metafile/bitmap (not supported)
  PID_APPNAME      = 18; // string
  PID_SECURITY     = 19; // integer

// PIDs given specific meanings for Installer

  PID_MSIVERSION  = PID_PAGECOUNT; // integer, Installer version number (major*100+minor)
  PID_MSISOURCE   = PID_WORDCOUNT; // integer, type of file image, short/long, media/tree
  PID_MSIRESTRICT = PID_CHARCOUNT; // integer, transform restrictions

//------------------------------------------------------------------------------
// INSTALLER DATABASE INTEGER COLUMN DEFINITIONS
//------------------------------------------------------------------------------

// BBControl.Attributes
// Control.Attributes

type
  msidbControlAttributes = DWORD;

const
  msidbControlAttributesVisible            = $00000001;
  msidbControlAttributesEnabled            = $00000002;
  msidbControlAttributesSunken             = $00000004;
  msidbControlAttributesIndirect           = $00000008;
  msidbControlAttributesInteger            = $00000010;
  msidbControlAttributesRTLRO              = $00000020;
  msidbControlAttributesRightAligned       = $00000040;
  msidbControlAttributesLeftScroll         = $00000080;
  msidbControlAttributesBiDi               = msidbControlAttributesRTLRO or
    msidbControlAttributesRightAligned or msidbControlAttributesLeftScroll;

  // Text controls

  msidbControlAttributesTransparent       = $00010000;
  msidbControlAttributesNoPrefix          = $00020000;
  msidbControlAttributesNoWrap            = $00040000;
  msidbControlAttributesFormatSize        = $00080000;
  msidbControlAttributesUsersLanguage     = $00100000;

  // Edit controls

  msidbControlAttributesMultiline         = $00010000;
  msidbControlAttributesPasswordInput     = $00200000;

  // ProgressBar controls

  msidbControlAttributesProgress95        = $00010000;

  // VolumeSelectCombo and DirectoryCombo controls

  msidbControlAttributesRemovableVolume   = $00010000;
  msidbControlAttributesFixedVolume       = $00020000;
  msidbControlAttributesRemoteVolume      = $00040000;
  msidbControlAttributesCDROMVolume       = $00080000;
  msidbControlAttributesRAMDiskVolume     = $00100000;
  msidbControlAttributesFloppyVolume      = $00200000;

  // VolumeCostList controls

  msidbControlShowRollbackCost            = $00400000;

  // ListBox and ComboBox controls

  msidbControlAttributesSorted            = $00010000;
  msidbControlAttributesComboList         = $00020000;

  // picture button controls

  msidbControlAttributesImageHandle       = $00010000;
  msidbControlAttributesPushLike          = $00020000;
  msidbControlAttributesBitmap            = $00040000;
  msidbControlAttributesIcon              = $00080000;
  msidbControlAttributesFixedSize         = $00100000;
  msidbControlAttributesIconSize16        = $00200000;
  msidbControlAttributesIconSize32        = $00400000;
  msidbControlAttributesIconSize48        = $00600000;

  // RadioButton controls

  msidbControlAttributesHasBorder         = $01000000;

// CompLocator.Type
// IniLocator.Type
// RegLocator.Type

type
  msidbLocatorType = DWORD;

const
  msidbLocatorTypeDirectory = $00000000;
  msidbLocatorTypeFileName  = $00000001;
  msidbLocatorTypeRawValue  = $00000002;
  msidbLocatorType64bit     = $00000010;

// Component.Attributes

type
  msidbComponentAttributes = DWORD;

const
  msidbComponentAttributesLocalOnly          = $00000000;
  msidbComponentAttributesSourceOnly         = $00000001;
  msidbComponentAttributesOptional           = $00000002; // local or source
  msidbComponentAttributesRegistryKeyPath    = $00000004; // KeyPath is key to Registry table
  msidbComponentAttributesSharedDllRefCount  = $00000008; // increment SharedDll count
  msidbComponentAttributesPermanent          = $00000010; // never uninstall component
  msidbComponentAttributesODBCDataSource     = $00000020; // KeyFile is key to ODBCDataSource table
  msidbComponentAttributesTransitive         = $00000040; // Can transition to/from installed/uninstalled based on changing conditional
  msidbComponentAttributesNeverOverwrite     = $00000080; // dont stomp over existing component if key path exists (file/ regkey)
  msidbComponentAttributes64bit              = $00000100; // designates a 64-bit component; 32-bit if missing.

// Assembly.Attributes

type
  msidbAssemblyAttributes = DWORD;

const
  msidbAssemblyAttributesURT   = $00000000;
  msidbAssemblyAttributesWin32 = $00000001;

// CustomAction.Type

type
  msidbCustomActionType = DWORD;

const

  // executable types

  msidbCustomActionTypeDll              = $00000001;  // Target = entry point name
  msidbCustomActionTypeExe              = $00000002;  // Target = command line args
  msidbCustomActionTypeTextData         = $00000003;  // Target = text string to be formatted and set into property
  msidbCustomActionTypeJScript          = $00000005;  // Target = entry point name, null if none to call
  msidbCustomActionTypeVBScript         = $00000006;  // Target = entry point name, null if none to call
  msidbCustomActionTypeInstall          = $00000007;  // Target = property list for nested engine initialization

  // source of code

  msidbCustomActionTypeBinaryData       = $00000000;  // Source = Binary.Name, data stored in stream
  msidbCustomActionTypeSourceFile       = $00000010;  // Source = File.File, file part of installation
  msidbCustomActionTypeDirectory        = $00000020;  // Source = Directory.Directory, folder containing existing file
  msidbCustomActionTypeProperty         = $00000030;  // Source = Property.Property, full path to executable

  // return processing                  // default is syncronous execution, process return code

  msidbCustomActionTypeContinue         = $00000040;  // ignore action return status, continue running
  msidbCustomActionTypeAsync            = $00000080;  // run asynchronously

  // execution scheduling flags               // default is execute whenever sequenced

  msidbCustomActionTypeFirstSequence    = $00000100;  // skip if UI sequence already run
  msidbCustomActionTypeOncePerProcess   = $00000200;  // skip if UI sequence already run in same process
  msidbCustomActionTypeClientRepeat     = $00000300;  // run on client only if UI already run on client
  msidbCustomActionTypeInScript         = $00000400;  // queue for execution within script
  msidbCustomActionTypeRollback         = $00000100;  // in conjunction with InScript: queue in Rollback script
  msidbCustomActionTypeCommit           = $00000200;  // in conjunction with InScript: run Commit ops from script on success

  // security context flag, default to impersonate as user, valid only if InScript

  msidbCustomActionTypeNoImpersonate    = $00000800;  // no impersonation, run in system context

//#if (_WIN32_MSI >= 150)
  msidbCustomActionTypeTSAware          = $00004000;  // impersonate for per-machine installs on TS machines
//#endif // (_WIN32_MSI >= 150)

  // script requires 64bit process
  msidbCustomActionType64BitScript      = $00001000;  // script should run in 64bit process

  // don't record the contents of the Target field in the log file.
  msidbCustomActionTypeHideTarget       = $00002000;

// Dialog.Attributes

type
  msidbDialogAttributes = DWORD;

const
  msidbDialogAttributesVisible          = $00000001;
  msidbDialogAttributesModal            = $00000002;
  msidbDialogAttributesMinimize         = $00000004;
  msidbDialogAttributesSysModal         = $00000008;
  msidbDialogAttributesKeepModeless     = $00000010;
  msidbDialogAttributesTrackDiskSpace   = $00000020;
  msidbDialogAttributesUseCustomPalette = $00000040;
  msidbDialogAttributesRTLRO            = $00000080;
  msidbDialogAttributesRightAligned     = $00000100;
  msidbDialogAttributesLeftScroll       = $00000200;
  msidbDialogAttributesBiDi             = msidbDialogAttributesRTLRO or
    msidbDialogAttributesRightAligned or msidbDialogAttributesLeftScroll;
  msidbDialogAttributesError            = $00010000;

// Feature.Attributes

type
  msidbFeatureAttributes = DWORD;

const
  msidbFeatureAttributesFavorLocal             = $00000000;
  msidbFeatureAttributesFavorSource            = $00000001;
  msidbFeatureAttributesFollowParent           = $00000002;
  msidbFeatureAttributesFavorAdvertise         = $00000004;
  msidbFeatureAttributesDisallowAdvertise      = $00000008;
  msidbFeatureAttributesUIDisallowAbsent       = $00000010;
  msidbFeatureAttributesNoUnsupportedAdvertise = $00000020;

// File.Attributes

type
  msidbFileAttributes = DWORD;

const
  msidbFileAttributesReadOnly       = $00000001;
  msidbFileAttributesHidden         = $00000002;
  msidbFileAttributesSystem         = $00000004;
  msidbFileAttributesReserved0      = $00000008; // Internal use only - must be 0
  msidbFileAttributesReserved1      = $00000040; // Internal use only - must be 0
  msidbFileAttributesReserved2      = $00000080; // Internal use only - must be 0
  msidbFileAttributesReserved3      = $00000100; // Internal use only - must be 0
  msidbFileAttributesVital          = $00000200;
  msidbFileAttributesChecksum       = $00000400;
  msidbFileAttributesPatchAdded     = $00001000; // Internal use only - set by patches
  msidbFileAttributesNoncompressed  = $00002000;
  msidbFileAttributesCompressed     = $00004000;
  msidbFileAttributesReserved4      = $00008000; // Internal use only - must be 0

// IniFile.Action
// RemoveIniFile.Action

type
  msidbIniFileAction = DWORD;

const
  msidbIniFileActionAddLine    = $00000000;
  msidbIniFileActionCreateLine = $00000001;
  msidbIniFileActionRemoveLine = $00000002;
  msidbIniFileActionAddTag     = $00000003;
  msidbIniFileActionRemoveTag  = $00000004;

// MoveFile.Options

type
  msidbMoveFileOptions = DWORD;

const
  msidbMoveFileOptionsMove = $00000001;

// ODBCDataSource.Registration

type
  msidbODBCDataSourceRegistration = DWORD;

const
  msidbODBCDataSourceRegistrationPerMachine  = $00000000;
  msidbODBCDataSourceRegistrationPerUser     = $00000001;

// Class.Attributes

type
  msidbClassAttributes = DWORD;

const
  msidbClassAttributesRelativePath  = $00000001;

// Patch.Attributes

type
  msidbPatchAttributes = DWORD;

const
  msidbPatchAttributesNonVital = $00000001;

// Registry.Root
// RegLocator.Root
// RemoveRegistry.Root

type
  msidbRegistryRoot = DWORD;

const
  msidbRegistryRootClassesRoot  = 0;
  msidbRegistryRootCurrentUser  = 1;
  msidbRegistryRootLocalMachine = 2;
  msidbRegistryRootUsers        = 3;

// RemoveFile.InstallMode

type
  msidbRemoveFileInstallMode = DWORD;

const
  msidbRemoveFileInstallModeOnInstall = $00000001;
  msidbRemoveFileInstallModeOnRemove  = $00000002;
  msidbRemoveFileInstallModeOnBoth    = $00000003;

// ServiceControl.Event

type
  msidbServiceControlEvent = DWORD;

const
  msidbServiceControlEventStart             = $00000001;
  msidbServiceControlEventStop              = $00000002;
  msidbServiceControlEventDelete            = $00000008;
  msidbServiceControlEventUninstallStart    = $00000010;
  msidbServiceControlEventUninstallStop     = $00000020;
  msidbServiceControlEventUninstallDelete   = $00000080;

// ServiceInstall.ErrorControl

type
  msidbServiceInstallErrorControl = DWORD;

const
  msidbServiceInstallErrorControlVital = $00008000;

// TextStyle.StyleBits

type
  msidbTextStyleStyleBits = DWORD;

const
  msidbTextStyleStyleBitsBold         = $00000001;
  msidbTextStyleStyleBitsItalic       = $00000002;
  msidbTextStyleStyleBitsUnderline    = $00000004;
  msidbTextStyleStyleBitsStrike       = $00000008;

// Upgrade.Attributes

type
  msidbUpgradeAttributes = DWORD;

const
  msidbUpgradeAttributesMigrateFeatures     = $00000001;
  msidbUpgradeAttributesOnlyDetect          = $00000002;
  msidbUpgradeAttributesIgnoreRemoveFailure = $00000004;
  msidbUpgradeAttributesVersionMinInclusive = $00000100;
  msidbUpgradeAttributesVersionMaxInclusive = $00000200;
  msidbUpgradeAttributesLanguagesExclusive  = $00000400;

//------------------------------------------------------------------------------
// SUMMARY INFORMATION PROPERTY DEFINITIONS
//------------------------------------------------------------------------------

type
  msidbSumInfoSourceType = DWORD;

const
  msidbSumInfoSourceTypeSFN            = $00000001;  // source uses short filenames
  msidbSumInfoSourceTypeCompressed     = $00000002;  // source is compressed
  msidbSumInfoSourceTypeAdminImage     = $00000004;  // source is an admin image

implementation

end.
