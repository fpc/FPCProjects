{****************************************************************************
*                                                                           *
*                                    CPU                                    *
*                                                                           *
*                                                                           *
* Language:             FPC Pascal v0.99.12+                                *
* Environment:          MS-DOS                                              *
* Supported extenders:  GO32V2/WIN32/LINUX/OS/2                             *
*                                                                           *
* Required switches:    none                                                *
*                                                                           *
* Author:               Thomas Schatzl                                      *
* License:              LGPL, see the file COPYING.TXT, included in this    *
*                       distribution for details about the copyright.       *
* Date:                 06.11.1999                                          *
* Version:              Rel0Beta15                                          *
*                                                                           *
*      Send bug reports and feedback to tom_at_work@yline.com               *
*                                                                           *
* Description:  CPU detection and timing unit. Detects various capabilities *
*               of CPUs like speed, rating, family, manufacturer, model,    *
*               stepping and different features of them (like CPUid support,*
*               MMX support, 3Dnow! support, KNI support, eMMX support....) *
*                                                                           *
*               Additionally it provides some objects to time code parts    *
*               with an accuracy of up to 1 microsecond, depending on the   *
*               timer used.                                                 *
*                                                                           *
*               To get maximum accuracy it is recommened that you enable    *
*               some optimization switches of the compiler to minimize over-*
*               head provided by objects.                                   *
*                                                                           *
*               Full compatibility to the RTL units (CPU and MMX)           *
*                                                                           *
*                                                                           *
* For a history see the end of the file                                     *
*                                                                           *
*****************************************************************************}

unit cpu;

interface

{ constants for the capabilities field of the CPU records' capabilites field }

const
	cHAVE_FPU    = $0001;  { floating point unit }
	cHAVE_CPUID  = $0002;  { cpuid instruction supported }
	cHAVE_RDTSC  = $0004;  { rdtsc instruction supported }
	cHAVE_MMX    = $0008;  { MMX extensions available }
	cHAVE_EMMX   = $0010;  { Cyrix' extended MMX available }
	cHAVE_3DNOW  = $0020;  { AMD 3Dnow extensions supported }
	cHAVE_KNI    = $0040;  { Katmai New Instructions supported }
	cHAVE_ISSE   = $0040;  { Internet Streaming SIMD Extionsions supported }
	                       { equals KNI }
	cHAVE_SERNUM = $0080;  { PIII+ serial number available }

{ cpu family identifier constants }

const
	cFAMILY_I386     = 3; { 80386 processor class }
	cFAMILY_I486     = 4; { 80486 processor class }
	cFAMILY_PENTIUM  = 5; { Pentium (80586) class processor }
	cFAMILY_PENTIUM2 = 6; { Pentium2 (80686) class processor }
	cFAMILY_UNKNOWN  = 7; { Unkown processor class }

{ manufacturer identifier constants (sorted alphabetically) }

const
	cMFC_AMD     = 0;
	cMFC_CENTAUR = 1;
	cMFC_CYRIX   = 2;
	cMFC_INTEL   = 3;
	cMFC_NEXGEN  = 4;
	cMFC_RISE    = 5;	
	cMFC_UMC     = 6;
	cMFC_OTHER   = 7;

{ the timing frequency of the programming interval timer }

const
        TIMER_RATE : dword = $1234DD;

{ strings of different CPU brands reported by the CPUid instruction. For
  reference only.
}
                         
const
	manufacturer_idstrings : array[cMFC_AMD..cMFC_UMC] of string[12] = (
		'AuthenticAMD',
		'CentaurHauls',
		'CyrixInstead',
		'GenuineIntel',
		'NexGenDriven',
		'RiseRiseRise',
		'UMC UMC UMC ');

{ list of known official ratings of CPUs. This list MUST be sorted in
  descending CPU speed order that these ratings are correctly identified.
  To add other ratings simply insert it at a proper place and adjust the array
  size accordingly.

  You DON'T need to change the procedure which does the
  rating calculation, additional ratings are automatically detected.
  
  It's best to contact me to insert it in the official CPU-release...

  BTW: This list SHOULD NOT contain ratings of overclocked CPU's, this array
  is purely for informational purposes. Always use the clock_rate entry of the
  CPU record instead of the rating for correct speed values
}

const
	known_ratings : array [0..45] of DWord = (
		1066, 1000, 0933, 0866, 0800, 0750, 0733, 0700, 0666, 0650, 0600, 
		0550, 0533, 0500, 0475, 0466, 0450, 0433, 0400, 0380, 0366, 0350, 
		0333, 0300, 0266, 0250, 0240, 0233, 0225, 0200,	0180, 0166, 0150, 
		0133, 0120, 0100, 0090, 0075, 0066, 0050, 0040, 0033, 0030, 0025, 
		0016, 0000);

type
	serial_number = array[0..2] of DWord;

{ this record holds the various CPU characteristics detected by this unit }

type
	cpu_rec = record
		cpu_id : string[12];    { the string returned by CPUid, else 'Unknown'}
		family : Byte;          { the CPU family }
		model : Byte;           { CPU model, 0xFF means unknown }
		stepping : Byte;        { CPU stepping, 0xFF means unknown }
		manufacturer : Byte;    { the CPU manufacturer }
		capabilities : DWord;   { capabilities bit field }
		clock_rate : DWord;     { detected clock rate in Mhz }
		rating : DWord;         { detected CPU rating in Mhz }
		sernum : serial_number; { processor specific serial number }
	end;

{ default cpu record, which is filled up at startup }

var
	this_cpu : cpu_rec;

{ this procedure fills out the CPU record which is given to it as a parameter }

procedure get_cpu_information(var c : cpu_rec);

{ returns the family string }

function get_family_string(f : DWord) : string;

{ returns the manufacturers name string }

function get_mfc_string(m : DWord) : string;

{ returns the processor serial number as a string }

function get_sernum_string(ser : serial_number) : string;

{ a helper function to obtain CPU information from the this_cpu variable more
  easily. If more than one capability wants to be checked, they have to be
  OR'ed.
}

function cpu_supported(capability : DWord) : Boolean;

{ returns a unique 64 bit number based on the current time. Use this for
example to get unique id numbers for different purposes. It makes sure that a
subsequent call doesn't return the same number. Depending on the availability
it uses either the TSC or PIT value (being 'unique' enough for my purposes). }

function get_time_stamp : comp;

{ returns the current value of the internal time stamp counter. There is no
  check wheter the rdtsc instruction is supported or not, so be sure to check
  it before calling this procedure.
}

procedure get_rdtsc_value(var val : comp);

{ returns the current value of the programmable interval timer. This result is
  only accurate if you DON'T fiddle around with the timer registers.
}

procedure get_pit_value(var val : comp);

{ base timer object which runs on any platform supported by FPC pascal and
  having the DOS unit. The accuracy of this timer is 1/100 of a second. This
  resolution is only valid on DOS platforms, on others this may vary.

  You are allowed to have as much timers timing at the same time as you want.
}

type
	pTimer = ^Timer;
	Timer = object
		{ sets the number of internal ticks the timer waits until the actual
		tick count is increased. Use this to easily scale the high resolution
		timers to more convenient ones (e.g. microsecond to millisecond
		scale). However accuracy detoriates due to rounding errors with higher
		units (always < (0.5 * units internal ticks) though) }
		units : longint;
		{ if set the timer automatically resets before a delay() or waitfor()
		call (default = false) }
		autoreset : boolean;

		constructor Init;
		destructor Done;

		{ resets the timer }
		procedure Reset; virtual;
		{ starts the timer }
		procedure Start; virtual;
		{ stops the timer }
		procedure Stop; virtual;

		{ returns the time passed between (subsequent) calls to the
		Start/Stop methods }
		function Count : Comp; virtual;
		{ returns the time passed since the last Start call in addition to
		the time passed between earlier calls to the Start/Stop methods.
		If this procedure is called when the timer isn't timing the
		value returned may be undefined }
		function Lap : Comp; virtual;
		{ returns the resolution of the timer in parts of a second }
		function Resolution : Real; virtual;

		{ returns the current state of the timer }
		function IsTiming : Boolean; virtual;

		{ high precision delay procedure. The delay time given as a
		parameter is a multiple of the resolution of the timer. You MUST
		NOT let a timer itself time his own delay procedure, else un-
		predicable results may occur.
		The delayed time is automatically added to the elapsed time }
		procedure Delay(time : DWord); virtual;

		{ waits until at least time ticks elapsed since the last call to
		start(). Aborts automatically if start() wasn't invoked before this
		call. }
		procedure WaitFor(time : DWord); virtual;

		protected

		{ reads the current value of the used system timers }
		function ReadTime : Comp; virtual;

		private

		{ this flag reflects the current state of the timer }
		timing : Boolean;

		{ last start time of the timer. These two values may be in another
		scale than the times returned by the various time result methods }
		tmStart,
		{ elapsed time, for measuring subsequent Start/Stop sequences }
		tmElapsed : Comp;
	end;

	tTimer = Timer;

{ Zen timer for timing periods of up to 280+ years with a microsecond accuracy
  using the internal time stamp counter on machines supporting it.
}

	pZTimer = ^ZTimer;
	ZTimer = object(Timer)
		constructor Init;
		destructor Done;

		function Count : Comp; virtual;
		function Lap : Comp; virtual;
		function Resolution : Real; virtual;
            
		protected

		function ReadTime : Comp; virtual;

		private

		{ converts the internally used time values to microsecond scale }
		function TicksTo_usec(t : Comp) : Comp;

		{ this flag shows the method used by the ZenTimer }
		use_rdtsc : Boolean;
		{ used to convert the internally used timing scale to microsecond
		values }
		rate : DWord;
	end;

	tZTimer = ZTimer;

{ Ultra long timer object. It's accuracy is about 1/18 seconds with a timing
  period of up to one day.
  GO32V2 only
}
        {$IFDEF GO32V2}
	pULZTimer = ^ULZTimer;
	ULZTimer = object(Timer)
		constructor Init;
		destructor Done;

		function Resolution : Real; virtual;
            
		protected

		function ReadTime : Comp; virtual;
	end;

	tULZTimer = ULZTimer;
        {$ENDIF}

procedure delay(ms : DWord);

{ RTL CPU compatibility procedures
}

function cpuid_support : boolean;
function floating_point_emulation : boolean;
function cr0 : longint;

implementation

{$ASMMODE ATT}

{ used for the 1/100 second timer only }
uses
	{$IFDEF WIN32}windows, {$ENDIF}
	{$IFDEF LINUX}linux, {$ENDIF}
	{$IFDEF OS2}doscalls, {$ENDIF}
	dos;

function get_cpu_clockrate(family : Byte) : DWord; forward;
function get_cpu_rating(clock : DWord) : DWord; forward;

{ Manufacturer check
  A specific manufacturer is detected most of the time by comparing the cpuid
  strings with known ones (different are C6, here you can't rely on that the
  cpuid string is valid, because you can change it via some MSR's on your own)
}

{ CyrixInstead = 64616574 736E4978 69727943 }

function is_cyrix : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $1, %eax
	jl .LNoCyrix
	subl $0x69727943, %ebx
	subl $0x736E4978, %edx
	subl $0x64616574, %ecx
	orl %ecx, %ebx
	orl %edx, %ebx
	setzb %al
	jmp .LExitCyrix
.LNoCyrix:
	movl $0, %eax
.LExitCyrix:
end ['EAX','EBX'];

{ AuthenticAMD = 41757468 656E7469 63414D44 }

function is_amd : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $1, %eax
	jl .LNoAMD
	subl $0x68747541, %ebx
	subl $0x69746E65, %edx
	subl $0x444D4163, %ecx
	orl %ecx, %ebx
	orl %edx, %ebx
	setzb %al
	jmp .LExitAMD
.LNoAMD:
	movl $0, %eax
.LExitAMD:
end ['EAX','EBX','ECX','EDX'];

{ GenuineIntel = 47656E75 696E6549 6E74656C }

function is_intel : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $0, %eax
	je .LNoIntel
	subl $0x756E6547, %ebx
	subl $0x49656E69, %edx
	subl $0x6C65746E, %ecx
	orl %ecx, %ebx
	orl %edx, %ebx
	setzb %al
	jmp .LExitIntel
.LNoIntel:
	movl $0, %eax
.LExitIntel:
end ['EAX','EBX','ECX','EDX'];

{UMC UMC UMC = 554D4320 554D4320 554D4320}

function is_umc : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $1, %eax
	jl .LNoUMC

	subl $0x20434D55, %ebx
	subl $0x20434D55, %edx
	subl $0x20434D55, %ecx
	orl %ecx, %ebx
	orl %edx, %ebx
	setzb %al
	jmp .LExitUMC
.LNoUMC:
	movl $0, %eax
.LExitUMC:
end;

{ NexGenDriven = 4E657847 656E4472 6976656E}

function is_nexgen : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $1, %eax
	jl .LNoNexGen

	subl $0x4778654E, %ebx
	subl $0x72446E65, %edx
	subl $0x6E657669, %ecx
	orl %ecx, %ebx
	orl %edx, %ebx
	setzb %al
	jmp .LExitNexGen
.LNoNexGen:
	movl $0, %eax
.LExitNexGen:
end;

{ CentaurHauls = 43656E74 61757248 61756C73
  We actually don't use the CPUid string here, we check if CPUid function
  C000:0000 is available and supported.
}

function is_centaur : Boolean; assembler;
asm
	movl $0xC0000000, %eax
	cpuid
	subl $0xC0000000, %eax
	setzb %al
end ['EAX','EBX','ECX','EDX'];

{ RiseRiseRise = 52697365  52697365  52697365 }

function is_rise : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $1, %eax
	jl .LNoRise
	subl $0x65736952, %ebx
	subl $0x65736952, %edx
	subl $0x65736952, %ecx
	orl %ecx, %ebx
	orl %edx, %ebx
	setzb %al
	jmp .LExitRise
.LNoRise:
	movl $0, %eax
.LExitRise:
end;

{ Capabilities support check
  The different capabilites of processors are mostly detected by examining
  CPUid function 1, although some require additional work.
}

function fpu_supported : Boolean; assembler;
asm
	fninit
	movl $0x5a5a, %eax
	fnstsw %ax
	cmpw $0, %ax
	seteb %al
end ['EAX'];

{ check if the cpuid instruction is available }
function cpuid_supported : Boolean; assembler;
asm
	pushfl
	pushfl                  { Get original EFLAGS}
	popl    %eax
	movl    %eax, %ecx
	xorl    $0x200000, %eax { Flip id bit in eflags }
	pushl   %eax            { Save new eflags value on stack }
	popfl                   { Replace current eflags value }
	pushfl                  { Get new eflags }
	popl    %eax            { Store new eflags in eax }
	popfl                   { Restore original eflags }
	xorl    %ecx, %eax      { Can not toggle ID bit}
	setnzb  %al
end ['EAX','ECX'];

{ check for rdtsc support by examining bit 4 returned in %edx by cpuid
  function 1
}
function rdtsc_supported : Boolean; assembler;
asm
	movl $1, %eax
	cpuid
	testl $0x00000010, %edx
	setnzb %al
end ['EAX','EBX','ECX','EDX'];

{ check for MMX support by examining bit 23 returned in %edx by the cpuid
  function 1
}
function mmx_supported : Boolean; assembler;
asm
	xorl %eax, %eax
	cpuid
	cmpl $1, %eax
	jl .LMMXFail
	movl $1, %eax
	cpuid
	andl $0x800000, %edx
	setnzb %al
	jmp .LMMXExit
.LMMXFail:
	xorl %eax, %eax
.LMMXExit:
end ['EAX','EBX','ECX','EDX'];

{ check for eMMX support. It's obtained by checking bit 24 returned in edx
  of cpuid function 8000:0001 (functions >= 8000:0000 are only supported by
  AMD and Cyrix processors
}
function emmx_supported : Boolean; assembler;
asm
	movl $0x80000000, %eax
	cpuid
	cmpl $0x80000001, %eax
	jb .LeMMXFail
	movl $0x80000001, %eax
	cpuid
	testl $0x01000000, %edx
	setnzb %al
	jmp .LeMMXExit
.LeMMXFail:
	xorl %eax, %eax
.LeMMXExit:
end ['EAX','EBX','ECX','EDX'];

{ check for 3dnow! support, by examing bit 31 returned in edx of cpuid
  function 8000:0001 (only supported by AMD and some Cyrix processors)
}
function _3dnow_supported : Boolean; assembler;
asm
	movl $0x80000000, %eax
	cpuid
	cmpl $0x80000001, %eax
	jb .L3DnowFail
	movl $0x80000001, %eax
	cpuid
	testl $0x80000000, %edx
	setnzb %al
	jmp .L3DnowExit
.L3DnowFail:
	xorl %eax, %eax
.L3DnowExit:
end ['EAX','EBX','ECX','EDX'];

{ check for Intel's Katmai New Instructions (ISSE) set. This is bit 25 in edx
  returned by cpuid function 1.
}
function kni_supported : Boolean; assembler;
asm
	movl $1, %eax
	cpuid
	testl $0x02000000, %edx
	setnzb %al
end ['EAX','EDX'];

{ check if the 96 bit serial number is present and available. PIII+ only
}

function sernum_supported : Boolean; assembler;
asm
	movl $1, %eax
	cpuid
	testl $0x00040000, %edx
	setnzb %al
end;

{ check out stepping, model, family via CPUid instruction }

function get_stepping : Byte; assembler;
asm
	movl $1, %eax
	cpuid
	andl $0xf, %eax
end ['EAX','EBX','ECX','EDX'];

function get_model : Byte; assembler;
asm
	movl $1, %eax
	cpuid
	shrl $4, %eax
	andl $0xf, %eax
end ['EAX','EBX','ECX','EDX'];

function get_family : Byte; assembler;
asm
	movl $1, %eax
	cpuid
	shrl $8, %eax
	andl $0xf, %eax
end ['EAX','EBX','ECX','EDX'];

{ copies the vendor string returned by cpuid function 0 returned in ebx, edx,
  and ecx
}

procedure get_cpu_string(var c : string[12]); assembler;
asm
	xorl %eax, %eax
	cpuid
	movl C, %edi
	movb $12, 0(%edi)
	movl %ebx, 1(%edi)
	movl %edx, 5(%edi)
	movl %ecx, 9(%edi)
end ['EAX','EBX','ECX','EDX'];

{ returns the processors unique 96 bit serial number which is returned by
  cpuid function 1 in eax (top 32 bit) and cpuid function 3 in ecx (bottom
  32 bit) and edx (middle 32 bit) respectively
}

procedure get_sernum(var s : serial_number); assembler;
asm
	movl S, %edi
	movl $1, %eax
	cpuid
	movl %eax, 0(%edi)
	movl $3, %eax
	cpuid
	movl %edx, 4(%edi)
	movl %ecx, 8(%edi)
end ['EAX','ECX','EDX'];


{ CPU detection routines
  These routines are executed when no cpuid instruction was found.
}

function is_386 : Boolean; assembler;
asm
{ distuingish between 386/486 by examining the ability to preserve the AC bit
  of the flags register. (386 = no, 486 = yes)  }
	movw %sp, %bx        { save current stack pointer }
	andw $0xfffc, %sp    { align stack to avoid AC fault }
	pushfl
	popl %eax
	movl %eax, %ecx
	xorl $0x40000, %eax  { xor AC bit in EFLAGS }
	pushl %eax
	popfl
	pushfl
	popl %eax
	xorl %ecx, %eax      { Is AC toggled ? }
	setz %al             { if not, we have a 386 }
	andw $0xfffc, %sp    { align stack to prevent AC fault }
	pushl %ecx
	popfl                { restore original AC bit }
	movw %bx, %sp        { restore original stack pointer }
end ['EAX','EBX','ECX'];

function is_486 : Boolean; assembler;
asm
{ Distinguish between the i486 and Pentium by the ability to set the ID flag
  in the EFLAGS register. If the ID flag is set, then we can use the CPUID
  instruction to determine the final version of the chip. Otherwise we
  simply have an 80486.}
	pushfl                  { Get original EFLAGS}
	popl %eax
	movl %eax, %ecx         { save EFLAGS }
	xorl $0x200000, %eax    { Flip ID bit in EFLAGS}
	pushl %eax              { Save new EFLAGS value on stack}
	popfl                   { Replace current EFLAGS value}
	pushfl                  { Get new EFLAGS}
	popl %eax               { Store new EFLAGS in EAX}
	xorl %ecx, %eax         { Toggle ID bit}
	setz %al
	pushl %ecx              { restore old EFLAGS }
	popfl
end ['EAX','ECX'];


{ helper functions to decide between various processor types }

function is_Pentium : Boolean;
begin
	if (cpuid_supported and (get_family = 5)) then is_Pentium := true
	else is_Pentium := false;
end;

function is_PentiumPro : Boolean;
begin
	if (cpuid_supported and (get_family = 6) and (get_stepping < 2)) then is_PentiumPro := true
	else is_PentiumPro := false;
end;

function is_PentiumII : Boolean;
begin
	if (cpuid_supported and (get_family = 6) and (get_stepping > 1)) then is_PentiumII := true
	else is_PentiumII := false;
end;

{ main cpu detection routine which uses all the procedures above  }

procedure get_cpu_information(var c : cpu_rec);
begin
	with (c) do begin
		{ set default values }
		model := $ff;
		stepping := $ff;
		cpu_id := 'Unknown';
		family := cFAMILY_UNKNOWN;
		manufacturer := cMFC_OTHER;
		capabilities := 0;
		sernum[0] := 0;
		sernum[1] := 0;
		sernum[2] := 0;

		{ check capabilities first }

		{ fpu availability check }
		if (fpu_supported) then capabilities := capabilities or cHAVE_FPU;
		{ cpuid instruction check }
		if (cpuid_supported) then begin
			capabilities := capabilities or cHAVE_CPUid;

			{ get other information obtained by the cpuid instruction }
			get_cpu_string(cpu_id);
			model := get_model;
			stepping := get_stepping;
			family := get_family;

			{ RDTSC needs CPUid }
			if (rdtsc_supported) then capabilities := capabilities or cHAVE_RDTSC; 
			{ MMX support check }
			if (mmx_supported) then capabilities := capabilities or cHAVE_MMX;

			{ (e)MMX, 3Dnow! and KNI only available when cpuid is supported }
             
			{ only Cyrix' Media GXm processors may have extended MMX }
			if ((is_cyrix and is_PentiumII) and (emmx_supported)) then
				capabilities := capabilities and cHAVE_eMMX;
			{ only AMD processors with model > 0 or Cyrix GXm (Media GX) processors
			may support 3Dnow! only }
			if (((is_amd and (model > 0)) or (is_cyrix and is_PentiumII)) and (_3dnow_supported)) then
				capabilities := capabilities or cHAVE_3DNOW;
			{ KNI (ISSE) check }
			if (kni_supported) then capabilities := capabilities or cHAVE_KNI;
			{ Serial number check }
			if (sernum_supported) then begin
				capabilities := capabilities or cHAVE_SERNUM;
				get_sernum(sernum);
			end;
		end else begin
			{ get family without cpuid }
			if (is_386) then family := cFAMILY_I386 else
			if (is_486) then family := cFAMILY_I486 else
			family := cFAMILY_PENTIUM;
		end;

		{ measure clock rate of CPU }
		clock_rate := get_cpu_clockrate(family);
		{ and find suitable rating }
		rating := get_cpu_rating(clock_rate);

		{ manufacturer check only done when cpuid is supported }
		if (cpu_supported(cHAVE_CPUID)) then begin
			if (is_centaur) then manufacturer := cMFC_CENTAUR else
			if (is_amd) then manufacturer := cMFC_AMD else
			if (is_cyrix) then manufacturer := cMFC_CYRIX else
			if (is_intel) then manufacturer := cMFC_INTEL else
			if (is_nexgen) then manufacturer := cMFC_NEXGEN else
			if (is_rise) then manufacturer := cMFC_RISE else			
			if (is_umc) then manufacturer := cMFC_UMC else
			manufacturer := cMFC_OTHER;
		end;
	end;
end;

{ check capabilities field of cpu - record and return true if all conditions
  are met by the current processor
}

function cpu_supported(capability : DWord) : Boolean;
begin
	cpu_supported := ((this_cpu.capabilities and capability) = capability);
end;

{ clockrate detection
  There are two methods how speed detection is done: Either by using the
  rdtsc instruction if available or the PIT timer.

  The rdtsc speed detection is done as follows : The number of instructions
  executed in a constant time is easily deducted by using the values the
  rdtsc returns. This is done until three subsequent measure runs are within
  a tolerance of +- 1 Mhz or a specific number of runs were done. The
  frequency then calculates by dividing the highly accurate number of ticks
  elapsed in the timing interval by the length of the timing interval.

  On the other hand, when no rdtsc is available (on all 386, most 486, some
  older Pentiums and Cyrix' 6x86) the time of a loop where the tick count is
  known is run some thousand times and the time span to complete this is
  measured. This is done about 10 times and the run which used least time is
  taken for frequency calculation. The frequency of the CPU then calculates by
  dividing the calculated number of ticks the loop runs take by the time
  measured. Due to the nature of this method it is not avoidable that the
  frequency may be a bit inaccurate, especially because some newer steppings
  of 386, 486 or Pentiums are a bit faster at executing the timing loop than
  expected. Another source for errors are those turbo switches, because when
  disabled, the CPUs actually seem to execute some instructions faster than
  displayed in the opcode lists.
  (e.g. A 486DX2-66 with turbo switch enabled is measured at a speed of 67 Mhz
  which is right, but when the turbo switch is disabled, timing results
  _always_ give 38 Mhz... another example is a 486DX-33: With turbo the unit
  times for 33 Mhz, but when it is disabled, it measures 31 Mhz instead of
  30...)

  Additionally one of the above methods is executed about five times and then
  the result is an average of these runs.
}

{ returns a unique 64 bit number based on the TSC or PIT value }

function get_time_stamp : comp;
var
	v : comp;
	a, b, c, hour : word;
begin
	if (cpu_supported(cHAVE_rdtsc)) then begin
		get_rdtsc_value(result);
		repeat
                  get_rdtsc_value(v);
		until (v <> get_time_stamp);
	end else begin
		get_pit_value(v);
		gettime(a, b, c, hour);
                get_time_stamp := v + hour * (TIMER_RATE * 60 * 60);
		repeat
			get_pit_value(v);
                        v := v + hour * (TIMER_RATE * 60 * 60);
		until (v <> result);
	end;
end;

{ return the contents of the time stamp counter }

procedure get_rdtsc_value(var val : comp); assembler;
asm
	rdtsc
	movl VAL, %edi
	movl %eax, 0(%edi)
	movl %edx, 4(%edi)
end ['EAX','EDX','EDI'];

{ return the current PIT value }
{$IFDEF WIN32}
procedure get_pit_value(var val : comp);
begin
	QueryPerformanceCounter(large_integer(val));
end;
{$ENDIF}
{$IFDEF GO32V2}
procedure get_pit_value(var val : comp); assembler;
asm
	xorl %eax, %eax
	cli
	outb %al, $0x43
	movl %fs:(0x46c), %edx
	inb $0x40, %al
	.byte   0xEB,0x00,0xEB,0x00,0xEB,0x00
	movb %al, %ah
	inb $0x40, %al
	.byte   0xEB,0x00,0xEB,0x00,0xEB,0x00
	xchgb %al, %ah
	negw %ax
	movzwl %ax, %edi
	sti
	movl $0x10000, %ebx
	movl %edx, %eax
	xorl %edx, %edx
	mull %ebx
	addl %edi, %eax
	adc $0, %edx
	movl VAL, %edi
	movl %eax, 0(%edi)
	movl %edx, 4(%edi)
end ['EAX','EBX','EDX','EDI'];
{$ENDIF}
{$IFDEF LINUX}
procedure get_pit_value(var val : comp);
var
        tv : timeval;
        tz : timezone;
begin
        GetTimeOfDay(tv, tz);
        val := tv.sec * timer_rate + tv.usec;
end;
{$ENDIF}
{$IFDEF OS2}
procedure get_pit_value(var val : comp);
begin
	DosTmrQueryTime(val);
end;
{$ENDIF}

{ measure CPU speed by means of the rdtsc instruction }

function get_rdtscspeed : Longint;

const
	MIN_TRIES = 3;  { number of minumum tries }
	MAX_TRIES = 20; { number of maximume tries issued }

	TOLERANCE = 1; { the tolerance in Mhz three subsequent
                       measurement results may differ }

var
	freq : array[0..2] of comp; { storage for the 3 subsequent measurements }
	time0, time1,        { start and endtime (PIT values) }
	stamp0, stamp1,      { start and endcycles (rdtsc values) }
	cycles, ticks : comp;{ used for frequency calculation }
	total : comp;        { total amount of frequency calculated from the 3
							subsequent runs }
	dummy,
	tries : DWord;       { current number of tries }

begin
	{ initialize variables }
	for dummy := 0 to 2 do
		freq[dummy] := 0;
	tries := 0;
	time0 := 0; time1 := 0; stamp0 := 0; stamp1 := 0;
	cycles := 0; ticks := 0; total := 0;
	repeat
		{ here to avoid overflows (e.g. for FPC) }
		for dummy := 0 to 2000 do ;
		{ increment number of tries }
		tries := tries + 1;

		{ rotate frequency values in storage }
		freq[2] := freq[1];
		freq[1] := freq[0];
		{ this accounts for overhead later }
		get_pit_value(time0);
		time1 := time0;
		while ((time1-time0) < 50) do begin
			get_pit_value(time1);
			get_rdtsc_value(stamp0);
		end;

		{ again, prevent overflows (e.g. for FPC) }
		for dummy := 0 to 2000 do ;

		{ here's the actual measurement }
		time0 := time1;
		get_pit_value(time0);
		get_rdtsc_value(stamp0);
		{ time for about 5 ms }
		while ((time1-time0) < 5000) do begin
			get_pit_value(time1);
			get_rdtsc_value(stamp1);
		end;

		cycles := stamp1 - stamp0;
        ticks := ((time1 - time0) * 1000000) / TIMER_RATE;
		{ calculate frequency }
		freq[0] := cycles / ticks;
		total := freq[0] + freq[1] + freq[2];

		{ if we've made enough tries, exit }
		if (tries > MAX_TRIES) then break;
		{ loop until three subsequent measurements are within tolerance }
	until ((abs((3*freq[0])-total) <= TOLERANCE) and
		(abs((3*freq[1])-total) <= TOLERANCE) and
		(abs((3*freq[2])-total) <= TOLERANCE));
	{ return speed value }
	get_rdtscspeed := trunc(total / 3);
end;

{ measure CPU speed by means of timing the execution of known loop }

const
	NUMLOOPS = 20000;  { number of loops executed }

function get_bsfspeed(cycles : DWord) : DWord;

{ the timed loop }

procedure runBSFLoop(iter : DWord); assembler;
asm
	movl ITER, %ebx
	movl $0x80000000, %eax
.LBSFLoop:
	bsfl %eax, %ecx
	decl %ebx
	jnz .LBSFLoop
end ['EAX','EBX','ECX'];

const
	SAMPLINGS = 10;   { number of times the loop is timed at one run }

var
	t0, t1,     { start and endtime }
	lowest,     { lowest time measured }
	ticks,
	current : Comp;
	i : DWord;

begin
	{ initialize lowest to a huge value }
 // lowest := $7ffffffff;
	{ time loop }
	for i := 0 to SAMPLINGS-1 do begin
		get_pit_value(t0);
		runBSFLoop(NUMLOOPS);
		get_pit_value(t1);
		current := abs(t1 - t0);
		if (current < lowest) then lowest := current;
	end;
	{ scale the PIT frequency to microsecond values }
        ticks := ((lowest * 1000000) / TIMER_RATE);
	get_bsfspeed := round(cycles / ticks);
end;


{ the clock-rate routine which chooses between the above two methods and
  averages about some measurements
}

function get_cpu_clockrate(family : Byte) : DWord;

const
	RUNS = 5;       { the number of times timing is done }
	{ the number of cycles needed for the different CPU types for the
	non-rdtsc speed detection }
	bsf_cycles : array[cFAMILY_I386..(cFAMILY_UNKNOWN-1)] of DWord =
                 (115, 47, 43, 38);

var
	times : Longint;
	avgspeed : DWord;
	speed  : DWord;
	use_rdtsc : Boolean;

begin
	avgspeed := 0;
	use_rdtsc := cpu_supported(cHAVE_CPUID or cHAVE_RDTSC);
	for times := 1 to RUNS do begin
		if (use_rdtsc) then speed := get_rdtscspeed
		else speed := get_bsfspeed(NUMLOOPS*bsf_cycles[family]);
		avgspeed := avgspeed + speed;
	end;
	get_cpu_clockrate := (avgspeed div RUNS);
end;


{ cpu rating detection }


function get_cpu_rating(clock : DWord) : DWord;
var
	i : Longint;
	prev_abs : DWord;
begin
	prev_abs := abs(known_ratings[0]-clock); i := 1;
	while (prev_abs > abs(known_ratings[i] - clock)) do begin
		prev_abs := abs(known_ratings[i] - clock);
		i := i + 1;
	end;
	get_cpu_rating := (known_ratings[(i-1)]);
end;

{ family string output }

function get_family_string(f : DWord) : string;

      { the family string identifiers returned for the different cpu types }
const
	family_strings : array[cFAMILY_I386..cFAMILY_UNKNOWN] of string[12] = (
		'80386',
		'80486',
		'Pentium',
		'PentiumII',
		'>PentiumII');

begin
	if (f in [cFAMILY_I386..(cFAMILY_UNKNOWN-1)]) then
		get_family_string := family_strings[f]
	else
		get_family_string := family_strings[cFAMILY_UNKNOWN];
end;

function get_mfc_string(m : DWord) : string;

const
	mfc_strings : array[cMFC_AMD..cMFC_OTHER] of string[12] = (
		'AMD',
		'Centaur',
		'Cyrix',
		'Intel',
		'NexGen',
		'Rise',
		'UMC',
		'Other');

begin
	if (m in [cMFC_AMD..(cMFC_OTHER-1)]) then
		get_mfc_string := mfc_strings[m]
	else
		get_mfc_string := mfc_strings[cMFC_OTHER];
end;

function get_sernum_string(ser : serial_number) : string;
var
	i : longint;
	s : string;
begin
	s := '';
	for i := 0 to 2 do
		s := s + hexstr(hi(ser[i]), 4) + '-' + hexstr(lo(ser[i]), 4) + '-';
	delete(s, length(s), 1);
	get_sernum_string := s;
end;


{****************************************************************************}
{                                                                            }
{ Timer Object                                                               }
{                                                                            }
{ - works on any platform (uses dos unit)                                    }
{ - timing resolution of 100 ms                                              }
{                                                                            }
{****************************************************************************}

constructor Timer.Init;
begin
	autoreset := false;
	units := 1;
	Reset;
end;

destructor Timer.Done;
begin
end;

procedure Timer.Reset;
begin
	tmStart := 0;
	tmElapsed := 0;
	timing := false;
end;

procedure Timer.Start;
begin
	timing := true;
	tmStart := ReadTime;
end;

procedure Timer.Stop;
begin
	tmElapsed := tmElapsed + (ReadTime - tmStart);
	timing := false;
	tmStart := 0;
end;

function Timer.Count : Comp;
begin
	Count := tmElapsed;
end;

function Timer.Lap : Comp;
begin
	lap := tmElapsed + (readtime - tmStart)
end;

function Timer.Resolution : Real;
begin
	resolution := 1e-2;
end;

function Timer.IsTiming : Boolean;
begin
	IsTiming := timing;
end;

procedure Timer.Delay(time : DWord);
begin
	if (autoreset) then reset;
	Start;
	while (lap < time) do ;
	Stop;
end;

procedure Timer.WaitFor(time : DWord);
begin
	if (not IsTiming) then exit;
	if (autoreset) then reset;
	{ because the reset() method sets this to false.... }
	timing := true;
	while (lap < time) do ;
	stop;
end;

function Timer.ReadTime : Comp;
var hour, minute, second, sec100 : Word;
begin
	gettime(hour, minute, second, sec100);
	ReadTime := (sec100 + (second + (minute + hour * 60) * 60) * 100) div units;
end;


{****************************************************************************}
{                                                                            }
{ ZenTimer Timer Object                                                      }
{                                                                            }
{ - GO32V2 only                                                              }
{ - timing resolution of 1 microsecond                                       }
{                                                                            }
{****************************************************************************}

constructor ZTimer.Init;
begin
	use_rdtsc := cpu_supported(cHAVE_CPUID or cHAVE_RDTSC);
	if (use_rdtsc) then rate := this_cpu.clock_rate
        else rate := TIMER_RATE;
	inherited init;
end;

destructor ZTimer.Done;
begin
	inherited done;
end;

function ZTimer.Count : Comp;
begin
	Count := TicksTo_usec(tmElapsed);
end;

function ZTimer.Lap : Comp;
begin
	lap := TicksTo_usec(readtime - tmStart)
end;

function ZTimer.Resolution : Real;
begin
	resolution := 1e-6;
end;

function ZTimer.TicksTo_usec(t : Comp) : Comp;
begin
	if (use_rdtsc) then
		TicksTo_usec := t / rate
	else
		TicksTo_usec := t * 1000000 / rate;
end;

function ZTimer.ReadTime : Comp;
var
	q : Comp;
begin
	if (use_rdtsc) then
		get_RDTSC_value(q)
	else
		get_PIT_value(q);
	ReadTime := q / double(units);
end;

{****************************************************************************}
{                                                                            }
{ Ultra Long Timer Object                                                    }
{                                                                            }
{ - GO32V2 only                                                              }
{ - timing resolution of 1/18.2 second                                       }
{                                                                            }
{****************************************************************************}
{$IFDEF GO32V2}
constructor ULZTimer.Init;
begin
	inherited init;
end;

destructor ULZTimer.Done;
begin
	inherited done;
end;

function ULZTimer.Resolution : Real;
begin
        Resolution := real($10000) / real(TIMER_RATE);
end;
            
function ULZTimer.ReadTime : Comp;
var
	t : DWord;
begin
	asm
		cli
		movl %fs:(0x46C), %eax
		sti
		movl %eax, t
	end ['EAX'];
	ReadTime := t div units;
end;
{$ENDIF}

{ RTL CPU unit compatibility functions }

function cpuid_support : boolean;
begin
	cpuid_support := cpu_supported(cHAVE_CPUID);
end;

function floating_point_emulation : boolean;
begin
	floating_point_emulation := (cr0 and $4) <> 0;
end;

function cr0 : longint; assembler;
asm
	.byte 0x0f, 0x20, 0xc0 // movl cr0, %eax
end;

var
	delaytimer : pztimer;

procedure Delay(ms : DWord);
begin
	if (delaytimer <> nil) then
		delaytimer^.delay(ms);
end;

var
	oldexitproc : pointer;

procedure cpu_exitproc;
begin
	exitproc := oldexitproc;
	dispose(delaytimer, done);
end;

{$IFDEF WIN32}
var
	temp : large_integer;
{$ENDIF}

begin
	{$IFDEF WIN32}
	QueryPerformanceFrequency(temp);
	timer_rate := temp.lowpart;
	{$ENDIF}
	{$IFDEF GO32V2}
	{ brute force PIT reset }
	asm
		movl $0x34, %eax
		outb %al, $0x43
		xorl %eax, %eax
		outb %al, $0x40
		outb %al, $0x40
	end;
	{$ENDIF}
        {$IFDEF LINUX}
        timer_rate := 1000000;
        {$ENDIF}
	{$IFDEF OS2}
	DosTmrQueryFreq(timer_rate);
	{$ENDIF}

	{ get cpu info for standard cpu record }
	get_cpu_information(this_cpu);
	{$IFDEF DEBUG}
	if (cpu_supported(cHAVE_RDTSC)) then Writeln(stderr, 'CPU : Using RDTSC for microsecond timing')
	else Writeln(stderr, 'CPU : Using PIT for microsecond timing');
	Writeln(stderr);
	{$ENDIF}
	new(delaytimer, init);
	delaytimer^.autoreset := false;
	delaytimer^.units := 1000;
	oldexitproc := exitproc;
	exitproc := @cpu_exitproc;
end.

{ 13-12-1998 Initial version (start of history)
  - still dissatisfied 
	. Intel CPU detection
	. AMD CPU detection
        . NexGen CPU detection
        . UMC CPU detection
	. KNI detection
  - still not done
	. default cpu record and some funcs using it
        
  14-12-1998 Some patches applied
  
  15-12-1998 More bugfixing
  - did default cpu record and function to easily check different bits
  - fixed bug with multiple zentimers running at the same time
  - applied a fix for very fast processor concerning CPU detection
  - made Intel, AMD, NexGen, UMC detection
  - renamed capability constants a bit
  - KNI detection is still a guess
  - PPro is a PII model 0 or 1, there's no difference in family anymore
  - added 500 and 433 Mhz to known ratings

  16-12-1998 Getting mad with the comp type :(
  - CPU speed detection doesn't work with DOS 7.10 (e.g. crashes)

  17-12-1998 Final bug removal to make first official beta
  - KNI detection is right(?) now
  - added 366 Mhz to known ratings
  - now works under DOS too
  - added unit header

  18-12-1998 More bug fixing to make beta 2
  - added cpu string returned by cpuid function 0 to cpu_rec
  - fixed Cyrix detection (PII have the same behaviour which I tested for
    as all Cyrix-processors)
  - better cpu-family handling
  - KNI detection is right !!!
  - added a $ASMMODE ATT at implementation start
  - fixed bug which was introduced by a Win9x (or FPC, DPMI-extender) bug :)

  19-12-1998 Bugfixing for beta 3
  - added 333 Mhz to known ratings
  - cpu speed detection on non-rdtsc systems tested
  - some more comments in the code (lots)
  - fixed eMMX detection
  - some additional code cleanup

  20-12-1998 some more finetuning (beta4)
  - replaced the round() calls by trunc() calls to have better accuracy (?)
  - 3Dnow! detection verified
  - different manufacturer handling
  - cpu speed now measured in 1/100 Mhz

  21-12-1998 undid some work made in beta4 -> beta4a
  - cpu speed in whole Mhz again (didn't like it)
  
  01-01-1999 beta5 (because of lack of docs)
  - changed cpu rating detection code, it works now on absolute differences
  between computed clock and ratings, not comparing computed clock and rating
  directly anymore

  05-01-1999 some additions (->beta5a)
  - added some output to stderr when DEBUG is defined

  29-01-1999 added some notes (beta6)
  - DOSEmu doesn't allow RDTSC and PIT access (from AlexS@Freepage.de)
  - added some new CPU ratings (reported from AlexS@Freepage.de)
  - note: I think it's about time to write a real doc.... ;(

  06-02-1999 header modifications
  - included License notice in the header

  23-03-1999 added serial no. detection
  - added capability for processor serial number enabled
  - added sernum field in processor record
  - added string return function as Intel recommends

  26-03-1999 changed docs
  - doesn't work with 0.99.10, requires 0.99.11

  30-03-1999 cleanup
  - removed some $ASMMODE defines
  - cyrix detection code fixed again (from marcov@stack.nl)
  - new constant: cHAVE_ISSE equals cHAVE_KNI (I do however prefer KNI)

  09-05-1999 adaption to new assembler
  - start of -""-
  - fixed cyrix cpuid detection (e.g. a 'movl $0, %eax' was missing)
  - still doesn't work with latest snapshot due to compiler bug

  12-05-1999 rtl compatibility
  - added the 3 functions available with the RTL cpu unit for completedness
  - made a new mmx unit which replaces the RTL mmx unit too

  17-05-1999 extended timer object
  - autoreset flag to automatically reset the timer on finishing a delay() or
  waitfor() method
  - scale the resolution via the units variable in the object
  - waitfor() method waits until the specified amount of time elapsed
  - get_time_stamp() to get some sort of unique 64 bit number which may be used
  as id tag
  - delay() procedure replacement
  - extended CPUTEST program to show the new features of the timer objects
	
  20-08-1999 win32 version
  - finished win32 port of the cpu unit, very few to do

  26-08-1999 linux, OS/2 version
  - finished linux and OS/2 port of cpu unit, also allowing the memory unit to 
  work.
  - ULZTimer only for GO32V2
  - updated e-mail address
 
  27-08-1999 fixed linux version
  - linux.gettime overrided dos.gettime which was meant to be used.

  04-09-1999 implemented suggestions from Lee John (LeeJ@logica.com)
  - fixed wrong spelling of 'milliseconds' in example and unit
  - some problems with 25 lines display solved
  
  06-11-1999 some additions
  - added quite a few new processor clock rates (from AlexS@Freepage.de)
  - added detection for Rise Technologies' processors

}
