unit SimpleTSC;

{$IFDEF SimpleTSC_PurePascal}
  {$DEFINE PurePascal}
{$ENDIF}

{$IF Defined(CPUX86_64) or Defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF Defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported CPU.'}
{$IFEND}

{$IF Defined(WINDOWS) or Defined(MSWINDOWS)}
  {$DEFINE Windows}
{$ELSEIF Defined(LINUX) and Defined(FPC)}
  {$DEFINE Linux}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$ASMMODE Intel}
{$ENDIF}
{$H+}

{$IF Defined(PurePascal) and not Defined(CompTest)}
  {$MESSAGE WARN 'This unit cannot be compiled without ASM.'}
{$IFEND}

interface

uses
  SysUtils;

{===============================================================================
    Library-specific exceptions
===============================================================================}
type
  ESTSCException = class(Exception);

  ESTSCInvalidValue     = class(ESTSCException);
  ESTSCInvalidState     = class(ESTSCException);
  ESTSCIndexOutOfBounds = class(ESTSCException);

{===============================================================================
    Core functions - declaration
===============================================================================}
type
  TSTSCTimeStamp = Int64;

{
  STSC_GetTSC

  Returns value of time-stamp counter (TSC) register.
}
Function STSC_GetTSC: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCEnter

  Returns value of time-stamp counter (TSC) register while ensuring that the
  instruction reading the value (instruction RDTSC) is executed prior to
  execution of any subsequent instruction (including any memory accesses).
}
Function STSC_GetTSCEnter: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCLeave

  Returns value of time-stamp counter (TSC) register while ensuring that the
  instruction reading the value is executed only after all previous instructions
  have executed and all previous memory loads and stores are globally visible.
}
Function STSC_GetTSCLeave: TSTSCTimeStamp; register; assembler;

{
  STSC_GetTSCFence

  Returns value of time-stamp counter (TSC) register while ensuring that the
  instruction reading the value is executed only after all previous instructions
  have executed and all previous memory loads and stores are globally visible
  and, at the same time, prior to execution of any subsequent instruction
  (including any memory accesses).
}
Function STSC_GetTSCFence: TSTSCTimeStamp; register; assembler;

{
  STSC_GetDistance

  Returns distance (forward difference) between two given time stamps.
  If TimeStampThen is higher than TimeStampNow, it is assumed the lower 63 bits
  of TSC owerflowed (unlikely, but possible) and the distance is calculated as
  such (note that only SINGLE overflow event is assumed, because overflowing
  multiple times would take at least decades, if not centuries - as per Intel's
  documentation, which states that the counter should not overflow within 10
  years).
}
Function STSC_TicksBetween(TimeStampNow,TimeStampThen: TSTSCTimeStamp): TSTSCTimeStamp;

{===============================================================================
    Continuous measurement - declaration
===============================================================================}
{
  Use following types and functions for standard and continuous measurement
  (measurement of several sequential intervals).

    For example, if you want to measure three intervals, do following?

      STSC_Start(Measurement,2);
      <first_interval>
      STSC_TimePoint(Measurement,0);
      <second_interval>
      STSC_TimePoint(Measurement,1);
      <third_interval>
      STSC_End(Measurement);

    You can then get the distances (intervals length) this way:

       first ... Measurement.TimePoints[0].DistanceFromPrevious
      second ... Measurement.TimePoints[1].DistanceFromPrevious
       third ... Measurement.EndTimePoints.DistanceFromPrevious

  WARNING -  these functions have unavoidable overhead (they take some time to
             execute), so if you want to do more precise measurements, use core
             functions instead and manage the time stamps yourself.
}
type
  TSTSCTimePoint = record
    TimeStamp:            TSTSCTimeStamp;
    IsAssigned:           Boolean;
    DistanceFromStart:    TSTSCTimeStamp;
    DistanceFromPrevious: TSTSCTimeStamp;
  end;

  TSTSCMeasurement = record
    Initialized:    Boolean;
    StartTimeStamp: TSTSCTimeStamp;
    TimePoints:     array of TSTSCTimePoint;
    EndTimePoint:   TSTSCTimePoint;
  end;
  PSTSCMeasurement = ^TSTSCMeasurement;

//------------------------------------------------------------------------------
{
  STSC_Start

  Initializes Measurement structure, allocates array of time points and then
  obtains start time stamp.

  Never set TimePointCount to value below 0, doing so will raise an exception
  of class ESTSCInvalidValue.
}
procedure STSC_Start(out Measurement: TSTSCMeasurement; TimePointCount: Integer = 0);

{
  STSC_TimePoint

  Obtains time stamp for selected time point. Distances are not calculated here
  for the sake of perforamnce.

  Remember that time point indices are zero-based (tey start at zero, end at
  count - 1).

  Measurement must be intialized otherwise an exception of ESTSCInvalidState
  class is raised.
}
procedure STSC_TimePoint(var Measurement: TSTSCMeasurement; TimePointIndex: Integer);

{
  STSC_End

  Obtains ending time stamp and then finalizes the Measurement. Also calculates
  distances for endpoint and all assigned time points.

  Measurement must be intialized otherwise an exception of ESTSCInvalidState
  class is raised.  
}
procedure STSC_End(var Measurement: TSTSCMeasurement);

{===============================================================================
    Call measurement - declaration
===============================================================================}
{
  These functions and types are designed to measure short time interval it
  takes to execute a single function call, if resolution of TSC alows it.
  
  The call must fully conform to signature in prodedural type TSTSCMeasuredCall,
  otherwice the behavior is completely undefined and will most probably result
  in nasty errors or, in the worst case, memory corruption.
}
type
  TSTSCTimeStamps = record
    StartTimeStamp: TSTSCTimeStamp;
    EndTimeStamp:   TSTSCTimeStamp;
  end;
  PSTSCTimeStamps = ^TSTSCTimeStamps;

  TSTSCMeasuredCall = procedure(Param: Pointer); register;

//------------------------------------------------------------------------------
{
  STSC_MeasureCall

  Measures a time it takes to execute a function referenced by parameter Call.

  Start time stamp and end time stamp are stored in TimeStamps variable.
}
procedure STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer; out TimeStamps: TSTSCTimeStamps); overload; register; assembler;

{
  STSC_MeasureCall

  Measures a time it takes to execute a given call and returns the measured
  distance (number of ticks).
}
Function STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer): Int64; overload;

{===============================================================================
    Auxiliary functions - declaration
===============================================================================}
(*
Function STSC_GetAvailableProcessors: TSTSCProcessorSet;

Function STSC_ProcessorAvailable(ProcessorID: Integer): Boolean;

Function STSC_GetThreadAffinity: TSTSCProcessorSet;
Function STSC_SetThreadAffinity(Mask: TSTSCProcessorSet): TSTSCProcessorSet;

Function STSC_GetThreadProcessor: TSTSCProcessorSet;
Function STSC_SetThreadProcessor(ProcessorID: Integer): TSTSCProcessorSet;
*)
//Function STSC_GetThreadPriority: ;
//Function STSC_SetThreadPriority(ThreadPriority: ): ;

implementation

{===============================================================================
    Core functions - implementation
===============================================================================}

Function STSC_GetTSC: TSTSCTimeStamp;
asm
{
  RDTSC loads lower 32 bits of TSC (time-stamp counter register) into EAX and
  higher 32 bits into EDX. In 64bit mode it behaves the same and high 32 bits
  of both registers are cleared.

    NOTE - because the stamps are in-here declared as signed 64bit integers
           (unsigned is not fully supported everywhere), the highest bit (#63)
           of the result is masked (removed) to prevent problems in arithmetics
           and comparisons.

  In 32bit environment, the result is returned the same way it is loaded by
  RDTSC (lower 32bits in EAX and higher in EDX). In 64bit, the result is
  returned in RAX register (in all systems).
}
    RDTSC

    AND   EDX, $7FFFFFFF  // mask bit 63 of the result
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCEnter: TSTSCTimeStamp; register; assembler;
asm
    RDTSC
    LFENCE

    AND   EDX, $7FFFFFFF
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCLeave: TSTSCTimeStamp; register; assembler;
asm
    MFENCE
    LFENCE
    RDTSC

    AND   EDX, $7FFFFFFF
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_GetTSCFence: TSTSCTimeStamp; register; assembler;
asm
    MFENCE
    LFENCE
    RDTSC
    LFENCE

    AND   EDX, $7FFFFFFF
{$IFDEF x64}
    SHL   RDX, 32
    OR    RAX, RDX
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function STSC_TicksBetween(TimeStampNow,TimeStampThen: TSTSCTimeStamp): TSTSCTimeStamp;
begin
If TimeStampNow < TimeStampThen then
  Result := (High(Int64) - TimeStampThen) + TimeStampNow + 1{wraparound}
else
  Result := TimeStampNow - TimeStampThen;
end;

{===============================================================================
    Continuous measurement - implementation
===============================================================================}

procedure STSC_Start(out Measurement: TSTSCMeasurement; TimePointCount: Integer = 0);
begin
SetLength(Measurement.TimePoints,0);
FillChar(Measurement,SizeOf(TSTSCMeasurement),0);
If TimePointCount >= 0 then
  SetLength(Measurement.TimePoints,TimePointCount)
else
  raise ESTSCInvalidValue.CreateFmt('STSC_Start: Invalid time point count (%d).',[TimePointCount]);
Measurement.StartTimeStamp := STSC_GetTSCEnter;
Measurement.Initialized := True;
end;

//------------------------------------------------------------------------------

procedure STSC_TimePoint(var Measurement: TSTSCMeasurement; TimePointIndex: Integer);
begin
If Measurement.Initialized then
  begin
    If (TimePointIndex <= Low(Measurement.TimePoints)) and (TimePointIndex >= High(Measurement.TimePoints)) then
      begin
        Measurement.TimePoints[TimePointIndex].TimeStamp := STSC_GetTSCFence;
        Measurement.TimePoints[TimePointIndex].IsAssigned := True;
      end
    else raise ESTSCIndexOutOfBounds.CreateFmt('STSC_TimePoint: Time point index (%d) out of bounds.',[TimePointIndex]);
  end
else raise ESTSCInvalidState.Create('STSC_TimePoint: Measurement not initialized.');
end;

//------------------------------------------------------------------------------

procedure STSC_End(var Measurement: TSTSCMeasurement);
var
  i:            Integer;
  LastAssigned: Integer;
begin
If Measurement.Initialized then
  begin
    Measurement.EndTimePoint.TimeStamp := STSC_GetTSCLeave;
    Measurement.EndTimePoint.IsAssigned := True;
    // calcualte time distances for time points
    LastAssigned := -1;
    For i := Low(Measurement.TimePoints) to High(Measurement.TimePoints) do
      with Measurement.TimePoints[i] do
        If IsAssigned then
          begin
            DistanceFromStart := STSC_TicksBetween(TimeStamp,Measurement.StartTimeStamp);
            If LastAssigned >= 0 then
              DistanceFromPrevious := STSC_TicksBetween(TimeStamp,Measurement.TimePoints[LastAssigned].TimeStamp)
            else
              DistanceFromPrevious := DistanceFromStart;
            LastAssigned := i;
          end;
    // calculate distances for end point
    with Measurement.EndTimePoint do
      begin
        DistanceFromStart := STSC_TicksBetween(TimeStamp,Measurement.StartTimeStamp);
        If LastAssigned >= 0 then
          DistanceFromPrevious := STSC_TicksBetween(TimeStamp,Measurement.TimePoints[LastAssigned].TimeStamp)
        else
         DistanceFromPrevious := DistanceFromStart;
      end;
  end
else raise ESTSCInvalidState.Create('STSC_End: Measurement not initialized.');
end;

{===============================================================================
    Call measurement - implementation
===============================================================================}

procedure STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer; out TimeStamps: TSTSCTimeStamps);
asm
end;

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Function STSC_MeasureCall(Call: TSTSCMeasuredCall; CallParam: Pointer): Int64;
var
  TimeStamps: TSTSCTimeStamps;
begin
STSC_MeasureCall(Call,CallPAram,TimeStamps);
Result := STSC_TicksBetween(TimeStamps.EndTimeStamp,TimeStamps.StartTimeStamp);
end;

end.
