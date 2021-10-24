{
ARM PrimeCell PL031 Real Time Clock Driver.

Copyright (C) 2021 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\rtc\rtc-pl031.c - Copyright 2010 (c) ST-Ericsson AB
  
  QEMU - \hw\timer\pl031.c - Copyright (c) 2007 CodeSourcery
  
References
==========
 
 PL031 - http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0224b/index.html

ARM PrimeCell PL031 Real Time Clock
===================================
 
 The PL031 is a simple memory mapped Real Time Clock (RTC) device that supports a single
 32-bit time counter based on the Unix time format which starts at 1 Jan 1970 and runs
 out at 19 Jan 2038.
 
 The device also includes a single alarm value which can trigger an interrupt when the
 selected time is reached.
 
 This driver currently does not support reading or setting the alarm value.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PL031; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,RTC,SysUtils;
 
//To Do //Support for Get/Set RTC Alarm with event on Interrupt

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc} 

{==============================================================================}
const
 {PL031 specific constants}
 PL031_RTC_DESCRIPTION = 'ARM PrimeCell PL031 Real Time Clock';  {Description of PL031 device}

 PL031_MIN_TIME = TIME_TICKS_TO_1970; {Time starts at 01/01/1970 00:00:00 (MM/DD/YY HH:MM:SS)}
 PL031_MAX_TIME = 137919572470000000; {Time ends at 19/1/2038 03:14:07 (MM/DD/YY HH:MM:SS)}
 
const
 {PL031 RTC Control register bits}
 PL031_RTC_CR_EN = (1 shl 0); {If set to 1, the RTC is enabled. Once it is enabled, any writes to this bit have no effect on the RTC until a system reset. A read returns the status of the RTC}
 
 {PL031 RTC Interrupt mask set and clear register bits}
 PL031_RTC_IMSC_INTR_SET   = (1 shl 0);
 PL031_RTC_IMSC_INTR_CLEAR = (0 shl 0);
 
 {PL031 RTC Raw interrupt status register bits}
 PL031_RTC_RIS_INTR   = (1 shl 0);
 
 {PL031 RTC Masked interrupt status register bits}
 PL031_RTC_MIS_INTR   = (1 shl 0);
 
 {PL031 RTC Interrupt clear register bits}
 PL031_RTC_ICR_INTR   = (1 shl 0);
 
{==============================================================================}
type
 {PL031 specific types}
 {Layout of the PL031 registers (See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.ddi0224b/i1005653.html)}
 PPL031RTCRegisters = ^TPL031RTCRegisters;
 TPL031RTCRegisters = record
  DR:LongWord;   {Data register}
  MR:LongWord;   {Match register}
  LR:LongWord;   {Load register}
  CR:LongWord;   {Control register}
  IMSC:LongWord; {Interrupt mask set and clear register}
  RIS:LongWord;  {Raw interrupt status register}
  MIS:LongWord;  {Masked interrupt status register}
  ICR:LongWord;  {Interrupt clear register}
 end; 

 PPL031RTC = ^TPL031RTC;
 TPL031RTC = record
  {RTC Properties}
  RTC:TRTCDevice;
  {PL031 Properties}
  IRQ:LongWord;
  Registers:PPL031RTCRegisters;                                           {Device registers}
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end; 
 
{==============================================================================}
{var}
 {PL031 specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{PL031 Functions}
function PL031RTCCreate(Address:PtrUInt;const Name:String;IRQ:LongWord):PRTCDevice;

function PL031RTCDestroy(RTC:PRTCDevice):LongWord;

{==============================================================================}
{PL031 RTC Functions}
function PL031RTCStart(RTC:PRTCDevice):LongWord;
function PL031RTCStop(RTC:PRTCDevice):LongWord;

function PL031RTCGetTime(RTC:PRTCDevice):Int64;
function PL031RTCSetTime(RTC:PRTCDevice;const Time:Int64):Int64;

{==============================================================================}
{PL031 Helper Functions}
function PL031FileTimeToRTCTime(RTC:PRTCDevice;const FileTime:Int64;var RTCTime:LongWord):Boolean;
function PL031RTCTimeToFileTime(RTC:PRTCDevice;RTCTime:LongWord;var FileTime:Int64):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {PL031 specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{PL031 Functions}
function PL031RTCCreate(Address:PtrUInt;const Name:String;IRQ:LongWord):PRTCDevice;
{Create, register and start a new PL031 RTC device which can be accessed using the RTC API}
{Address: The address of the PL031 registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ: The interrupt number for the PL031}
{Return: Pointer to the new RTC device or nil if the RTC device could not be created}
var
 Status:LongWord;
 PL031RTC:PPL031RTC;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(nil,'PL031: RTC Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ=' + IntToStr(IRQ) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;
 
 {Check IRQ}
 {if IRQ = 0 then Exit;} {IRQ 0 is valid}
 
 {Create RTC}
 PL031RTC:=PPL031RTC(RTCDeviceCreateEx(SizeOf(TPL031RTC)));
 if PL031RTC <> nil then
  begin
   {Update RTC}
   {Device}
   PL031RTC.RTC.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   PL031RTC.RTC.Device.DeviceType:=RTC_TYPE_NONE;
   PL031RTC.RTC.Device.DeviceFlags:=RTC_FLAG_NONE;
   PL031RTC.RTC.Device.DeviceData:=nil;
   if Length(Name) <> 0 then PL031RTC.RTC.Device.DeviceDescription:=Name else PL031RTC.RTC.Device.DeviceDescription:=PL031_RTC_DESCRIPTION;
   {RTC}
   PL031RTC.RTC.RTCState:=RTC_STATE_DISABLED;
   PL031RTC.RTC.DeviceStart:=PL031RTCStart;
   PL031RTC.RTC.DeviceStop:=PL031RTCStop;
   PL031RTC.RTC.DeviceGetTime:=PL031RTCGetTime;
   PL031RTC.RTC.DeviceSeTTime:=PL031RTCSetTime;
   {Driver}
   PL031RTC.RTC.Properties.Flags:=PL031RTC.RTC.Device.DeviceFlags;
   PL031RTC.RTC.Properties.MinTime:=PL031_MIN_TIME;
   PL031RTC.RTC.Properties.MaxTime:=PL031_MAX_TIME;
   PL031RTC.RTC.Properties.AlarmCount:=0;
   {PL031}
   PL031RTC.IRQ:=IRQ;
   PL031RTC.Registers:=PPL031RTCRegisters(Address);
   
   {Register RTC}
   Status:=RTCDeviceRegister(@PL031RTC.RTC);
   if Status = ERROR_SUCCESS then
    begin
     {Start RTC}
     Status:=RTCDeviceStart(@PL031RTC.RTC);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PRTCDevice(PL031RTC); 
      end
     else
      begin
       if RTC_LOG_ENABLED then RTCLogError(nil,'PL031: Failed to start new RTC device: ' + ErrorToString(Status));
      end;      
    end
   else
    begin
     if RTC_LOG_ENABLED then RTCLogError(nil,'PL031: Failed to register new RTC device: ' + ErrorToString(Status));
     
     {Destroy RTC}
     RTCDeviceDestroy(@PL031RTC.RTC);
    end;
  end
 else 
  begin
   if RTC_LOG_ENABLED then RTCLogError(nil,'PL031: Failed to create new RTC device');
  end;
end;

{==============================================================================}

function PL031RTCDestroy(RTC:PRTCDevice):LongWord;
{Stop, deregister and destroy a PL031 RTC device created by this driver}
{RTC: The RTC device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'PL031: RTC Destroy');
 {$ENDIF}
 
 {Stop RTC}
 Result:=RTCDeviceStop(RTC);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister RTC}
   Result:=RTCDeviceDeregister(RTC);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy RTC}
     Result:=RTCDeviceDestroy(RTC);
     if Result <> ERROR_SUCCESS then
      begin
       if RTC_LOG_ENABLED then RTCLogError(nil,'PL031: Failed to destroy RTC device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if RTC_LOG_ENABLED then RTCLogError(nil,'PL031: Failed to deregister RTC device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if RTC_LOG_ENABLED then RTCLogError(nil,'PL031: Failed to stop RTC device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}
{==============================================================================}
{PL031 RTC Functions}
function PL031RTCStart(RTC:PRTCDevice):LongWord;
{Implementation of RTCDeviceStart API for PL031 RTC}
{Note: Not intended to be called directly by applications, use RTCDeviceStart instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'PL031: RTC Start');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Start the RTC}
 PPL031RTC(RTC).Registers.CR:=PPL031RTC(RTC).Registers.CR or PL031_RTC_CR_EN;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PL031RTCStop(RTC:PRTCDevice):LongWord;
{Implementation of RTCDeviceStop API for PL031 RTC}
{Note: Not intended to be called directly by applications, use RTCDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'PL031: RTC Stop');
 {$ENDIF}
 
 {Nothing}
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PL031RTCGetTime(RTC:PRTCDevice):Int64;
{Implementation of RTCDeviceGetTime API for PL031 RTC}
{Note: Not intended to be called directly by applications, use RTCDeviceGetTime instead}
var
 FileTime:Int64;
 RTCTime:LongWord;
begin
 {}
 Result:=0;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'PL031: RTC Get Time');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(RTC.GetCount);
 
 {Read RTC Data}
 RTCTime:=PPL031RTC(RTC).Registers.DR;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Convert to File Time}
 if PL031RTCTimeToFileTime(RTC,RTCTime,FileTime) then
  begin
   Result:=FileTime;
  end;
 
 MutexUnlock(RTC.Lock);
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'PL031:  Time=' + IntToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function PL031RTCSetTime(RTC:PRTCDevice;const Time:Int64):Int64;
{Implementation of RTCDeviceSetTime API for PL031 RTC}
{Note: Not intended to be called directly by applications, use RTCDeviceSetTime instead}
var
 RTCTime:LongWord;
begin
 {}
 Result:=0;

 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(PL031_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'PL031: RTC Set Time (Time=' + IntToStr(Time) + ')');
 {$ENDIF}
 
 {Check Time}
 if (Time < PL031_MIN_TIME) or (Time > PL031_MAX_TIME) then Exit;
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(RTC.SetCount);
 
 {Convert to RTC Time}
 if PL031FileTimeToRTCTime(RTC,Time,RTCTime) then
  begin
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Write RTC Time}
   PPL031RTC(RTC).Registers.LR:=RTCTime;
   
   Result:=Time;
  end; 
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}
{==============================================================================}
{PL031 Helper Functions}
function PL031FileTimeToRTCTime(RTC:PRTCDevice;const FileTime:Int64;var RTCTime:LongWord):Boolean;
{Convert a FileTime value in 100 nanosecond ticks since 1 January 1601 to
 the time format of the PL031 RTC which is seconds since 1 January 1970}
begin
 {}
 Result:=False;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {Check FileTime}
 if FileTime < TIME_TICKS_TO_1970 then Exit;
 
 {Convert FileTime}
 RTCTime:=(FileTime - TIME_TICKS_TO_1970) div TIME_TICKS_PER_SECOND;
 
 Result:=True;
end;

{==============================================================================}

function PL031RTCTimeToFileTime(RTC:PRTCDevice;RTCTime:LongWord;var FileTime:Int64):Boolean;
{Convert a PL031 RTC time value in seconds since 1 January 1970 to the
 FileTime format which is 100 nanosecond ticks since 1 January 1601}
begin
 {}
 Result:=False;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {Convert RTC Time}
 FileTime:=RTCTime; {Avoid 32 bit overflow}
 FileTime:=(FileTime * TIME_TICKS_PER_SECOND) + TIME_TICKS_TO_1970;
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}

{initialization}
 {Nothing}
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
