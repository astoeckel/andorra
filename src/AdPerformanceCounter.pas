{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Authors:  Andreas Stoeckel
* File: AdPerformanceCounter.pas
* Comment: Contains a class for FPS-Measuring
}

{Contains a class for FPS-Measuring}
unit AdPerformanceCounter;

interface

{$IFNDEF WIN32}
uses
  SysUtils, LclIntf;
{$ENDIF}

type
  {Represents the state of the performance counter.}
  TAdPerformanceCounterState = (
    psPaused, //< The performance counter is currently paused, time gap is always zero.
    psResumed,  //< You just called the "Resume" method of the performance counter. When the calculate function is called the next time, "TimeGap" is zero, then the performance counter starts to recalculate the TimeGap.
    psRunning //< The performance counter is up and running since more than two ticks.
  );

  {Class for calculating the FPS and the TimeGap.}
  TAdPerformanceCounter = class
    private
      FTimeGap: double;
      FFPS: integer;
      FInterpolate: boolean;
      FState: TAdPerformanceCounterState;
      FLastTickCount: double;
      FTempTime: double;
      FTempFPS: integer;
      FInterpolationFactor: integer;
      FMaximumTimeGap: double;
      FMaximumFrameRate: integer;
      FLastSleep: Double;
      procedure LimitFrameRate(atd: double);
    public
      {Creates a new instance of TAdPerformanceCounter.
       @param(ACreatePaused can be true, in order to create the performance counter
         in a suspended mode)}
      constructor Create(ACreatePaused:boolean=false);

      {If the performance counter was paused (suspended), resume makes the counter
       to calculate the time-between frames again.}
      procedure Resume;
      {Pauses the counter. TimeGap will always be zero.}
      procedure Pause;
      
      {Makes the counter to calculate the FPS and the time between this and the 
       last call of "Calculate".}
      procedure Calculate;    

      {Contains the current state of the performance counter.
       @seealso(TAdPerformanceCounterState)}
      property State:TAdPerformanceCounterState read FState;
      {Contains the current FPS. FPS is calculated every second by
       counting the ticks passed.}
      property FPS:integer read FFPS;
      {TimeGap is the time that passed between the last two calls of
       the "Calculate" function. Zero if the counter is paused. The result is
       in milliseconds.}
      property TimeGap:double read FTimeGap;
      {If true, the TimeGap values are interpolated in order to prevent the
       game from small hangs. May cause problems if the framerate falls below
       100 FPS.}
      property Interpolate:Boolean read FInterpolate write FInterpolate;
      {The factor the old TimeGap value is multiplied with. The weight of the
       previous TimeGap is InterpolationFactor:1}
      property InterpolationFactor:integer read FInterpolationFactor write FInterpolationFactor;
      {If TimeGap exceeds MaximumTimeGap, TimeGap will be set to this value. This
       is for preventing the game logic from e.g. collision bugs beause TimeGap
       got to big.}
      property MaximumTimeGap:double read FMaximumTimeGap write FMaximumTimeGap;
      {Use this property to set the maximum framerate that should be achieved.
       Setting this value will also help to save cpu workload, what means less
       power comsumption.
       Remember that Maximumframerate is not really percise, do not replace
       time based moving by this method.
       If maximum frame rate is activated, a "sleep" will be added to the
       "calculate" function. So if you want to use the time while the
       program sleeps, you have to use threads.
       If Maximum Frame Rate is smaller or equals zero, frame rate limitation
       will be disabled.}
      property MaximumFrameRate:integer read FMaximumFrameRate write FMaximumFrameRate;
  end;

implementation

{ TAdPerformanceCounter }

{$IFDEF WIN32}
var
  Frequency:int64 = 0;

procedure Sleep(ms: Cardinal); stdcall; external 'kernel32.dll';
function QueryPerformanceCounter(var lpPerformanceCount: int64): boolean; stdcall; external 'kernel32.dll';
function QueryPerformanceFrequency(var lpFrequency: int64): boolean; stdcall; external 'kernel32.dll';

function GetTickCount:Double;
var
  ticks:int64;
begin
  if Frequency = 0 then
    QueryPerformanceFrequency(Frequency);

  QueryPerformanceCounter(ticks);

  result := ticks * 1000 / Frequency;
end;
{$ENDIF}

constructor TAdPerformanceCounter.Create(ACreatePaused: boolean);
begin
  inherited Create;

  if ACreatePaused then
  begin
    FState := psPaused;
  end
  else
  begin
    FState := psResumed;
  end;

  FTempTime := 0;
  FLastTickCount := GetTickCount;
  FInterpolate := true;
  FInterpolationFactor := 5;
  FMaximumTimeGap := 50;
  FLastSleep := 0;
end;

procedure TAdPerformanceCounter.LimitFrameRate(atd: double);
var
  sleeptime: Double;
begin
  sleeptime := 1000 / FMaximumFrameRate - (atd - FLastSleep);
  if sleeptime > 0 then
  begin
    Sleep(trunc(sleeptime));
    FLastSleep := sleeptime;
  end else
    FLastSleep := 0;
end;

procedure TAdPerformanceCounter.Calculate;
var
  tc, td: double;
begin
  //Calculate time difference
  tc := GetTickCount;
  td := tc - FLastTickCount;

  //Limit framerate
  if FMaximumFrameRate > 0 then
    LimitFrameRate(td);

  if FState = psRunning then
  begin
    if FInterpolate then
    begin
      FTimeGap := (FTimeGap * FInterpolationFactor + (td)) / (FInterpolationFactor + 1);
    end
    else
    begin
      FTimeGap := td;
    end;
  end else
  begin
    if FState = psResumed then
    begin
      FTimeGap := 1;
      FState := psRunning;
    end
    else
    begin
      FTimeGap := 0;
    end;
  end;

  if FTimeGap > FMaximumTimeGap then
    FTimeGap := FMaximumTimeGap;

  FLastTickCount := tc;

  FTempTime := FTempTime + td;
  FTempFPS := FTempFPS + 1;
  if FTempTime > 1000 then
  begin
    FTempTime := 0;
    FFPS := FTempFPS;
    FTempFPS := 0;
  end;
end;

procedure TAdPerformanceCounter.Pause;
begin
  FState := psPaused;
end;

procedure TAdPerformanceCounter.Resume;
begin
  if FState = psPaused then
  begin
    FState := psResumed;
  end;
end;

end.
