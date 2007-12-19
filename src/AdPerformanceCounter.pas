unit AdPerformanceCounter;

interface

{$IFNDEF WIN32}
uses
  LibC;
{$ENDIF}

type
  TAdPerformanceCounterState = (psPaused, psResumed, psRunning);

  //Class for calculating the FPS and the TimeGap
  TAdPerformanceCounter = class
    private
      FTimeGap:Double;
      FFPS:integer;
      FInterpolate:boolean;
      FState:TAdPerformanceCounterState;
      FLastTickCount:LongInt;
      FTempTime:LongInt;
      FTempFPS:integer;
      FInterpolationFactor:integer;
      FMaximumTimeGap:double;
    public
      property State:TAdPerformanceCounterState read FState;
      property FPS:integer read FFPS;
      property TimeGap:double read FTimeGap;
      property Interpolate:Boolean read FInterpolate write FInterpolate;
      property InterpolationFactor:integer read FInterpolationFactor write FInterpolationFactor;
      property MaximumTimeGap:double read FMaximumTimeGap write FMaximumTimeGap;

      constructor Create(ACreatePaused:boolean=false);

      procedure Resume;
      procedure Pause;
      procedure Calculate;    
  end;

implementation

{ TAdPerformanceCounter }

{$IFNDEF WIN32}
function GetTickCount:Cardinal;
var
  tv:timeval;
begin
  GetTimeOfDay(tv, nil);
  result := int64(tv.tv_sec) * 1000 + tv.tv_usec div 1000;
end;
{$ELSE}
function GetTickCount:LongWord; external 'kernel32.dll' name 'GetTickCount';
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
  FInterpolationFactor := 10;
  FMaximumTimeGap := 50; 
end;

procedure TAdPerformanceCounter.Calculate;
var
  tc,td:LongInt;
begin
  tc := GetTickCount;
  td := tc - FLastTickCount;

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
  begin
    FTimeGap := FMaximumTimeGap;
  end;

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
