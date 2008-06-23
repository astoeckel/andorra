unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, AdTypes, AdCanvas, AdFont, AdStandardFontGenerator,
  AdPerformanceCounter, AdSetupDlg, AdSprites, AdPNG, AdContainers, Math, AdParticles,
  AdVCLFormats;

type
  TSun = class(TSprite)
    private
      FMass:Extended;
      FParticleSystem:TAdParticleSystem;
      FTime:double;
    procedure SetParticleSystem(const Value: TAdParticleSystem);
    protected
      procedure DoMove(TimeGap:double);override;
      procedure DoDraw;override;
    public
      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;

      property Mass:Extended read FMass write FMass;
      property ParticleSystem:TAdParticleSystem read FParticleSystem write SetParticleSystem;
  end;

  TPlanet = class(TSprite)
    private
      FMass:Extended;
      FVelocity:double;
      FSun:TSun;
      FAngle:double;
      FDistance:double;
      FMoveVect:TAdVector2;

      FDoTrace:boolean;
      FTrace:TAdLinkedList;
      FTracePoint:TAdPoint;
      FSpeed:Double;

      FSunDrawVect:TAdVector2;
      FMoveDrawVect:TAdVector2;

      FTrianglePoint1, FTrianglePoint2:TAdVector2;
      FTrianglePointTime:double;
      FTriangleMoveVect:TAdVector2;
      FTriangleDeltaT:double;
      FDrawTriangle:boolean;

      FImage:TAdImage;

      procedure SetAngle(AValue:double);
      procedure SetDistance(AValue:double);
      procedure SetVelocity(AValue:double);
      procedure SetDoTrace(AValue:boolean);
      procedure SetTriangleDeltaT(AValue:double);
      procedure SetPos;
      procedure FreeTraceList;
      procedure CalcPos(var AX, AY:double; var AMoveVect:TAdVector2; TimeGap:double; SetDrawVectors:boolean);
    protected
      procedure DoMove(TimeGap:double);override;
      procedure DoDraw;override;
    public
      constructor Create(AParent:TSprite);override;
      destructor Destroy;override;
      procedure Reset;

      function GetDistance: double;
      function GetAngle: double;

      property Image:TAdImage read FImage write FImage;
      property Sun:TSun read FSun write FSun;
      property Angle:double write SetAngle;
      property Velocity:double read FVelocity write SetVelocity;
      property Distance:double read FDistance write SetDistance;
      property Speed:double read FSpeed write FSpeed;
      property DoTrace:boolean read FDoTrace write SetDoTrace;
      property MoveDrawVect:TAdVector2 read FMoveDrawVect;
      property DrawTriangle:boolean read FDrawTriangle write FDrawTriangle;
      property TriangleDeltaT:double read FTriangleDeltaT write SetTriangleDeltaT;
  end;

  TMainFrm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    Sun:TSun;
    Planet:TPlanet;
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdSpriteEngine:TSpriteEngine;
    AdImageList:TAdImageList;
    Paused:boolean;
    SimulationSpeed:double;
    procedure Idle(Sender:TObject;var Done:boolean);
  end;

var
  MainFrm: TMainFrm;

const
  path = './resources/';  

implementation

{$R *.dfm}

function AEToM(value:Extended):Extended;
begin
  result := 149597870691 * value;
end;

function AEToPx(value:Extended):Extended;
begin
  result := value * MainFrm.ClientHeight / 4; 
end;

function PxToAE(value:Extended):Extended;
begin
  result := (value * 4) / MainFrm.ClientHeight
end;

procedure TMainFrm.FormCreate(Sender: TObject);
var
  AdSetupDlg:TAdSetup;
begin
  //ReportMemoryLeaksOnShutdown := true;

  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw := TAdDraw.Create(self);
  AdDraw.Options := AdDraw.Options + [aoMipmaps];

  AdSetupDlg := TAdSetup.Create(AdDraw);
  AdSetupDlg.Image := 'logo1.png';

  Paused := true;
  SimulationSpeed := 10;

  if AdSetupDlg.Execute then
  begin
    if AdDraw.Initialize then
    begin
      Application.OnIdle := Idle;

      AdSpriteEngine := TSpriteEngine.Create(AdDraw);
      AdSpriteEngine.VisibilityTest := false;

      AdImageList := TAdImageList.Create(AdDraw);
      AdImageList.LoadFromFile(path+'demo_movingplanet.ail');

      with TBackgroundSprite.Create(AdSpriteEngine) do
      begin
        Image := AdImageList.Find('stars');
        Z := -1;
      end;

      Sun := TSun.Create(AdSpriteEngine);
      Sun.ParticleSystem.LoadFromFile(path+'sun.apf');
      Sun.ParticleSystem.Texture := AdImageList.Find('particle').Texture;

      Planet := TPlanet.Create(AdSpriteEngine);
      Planet.Sun := Sun;
      Planet.Image := AdImageList.Find('earth');
      Planet.Reset;
    end
    else
    begin
      ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                  'mode or another video adapter.');
      halt;
    end;
  end
  else
  begin
    halt;
  end;
  AdSetupDlg.Free;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  AdImageList.Free;
  AdSpriteEngine.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TMainFrm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_SPACE then Paused := not Paused;
  if (Key = VK_ADD) and (SimulationSpeed < 100) then
    SimulationSpeed := SimulationSpeed + 0.1;
  if (Key = VK_SUBTRACT) and (SimulationSpeed > 0.1) then
    SimulationSpeed := SimulationSpeed - 0.1;
  if (Key = $4E) and (Sun.Mass > Power(10,29)) then
    Sun.Mass := Sun.Mass - Power(10,29);
  if (Key = $4D) then
    Sun.Mass := Sun.Mass + Power(10,29);
  if (Key = $52) then
    Planet.Reset;
  if (Key = $56) and (Planet.Velocity > 0.01) then
    Planet.Velocity := Planet.Velocity - 0.001;
  if (Key = $42) then
    Planet.Velocity := Planet.Velocity + 0.001;
  if (Key = $58) then
    Planet.Distance := Planet.Distance - 0.1;
  if (Key = $43) then
    Planet.Distance := Planet.Distance + 0.1;
  if (Key = $5A) and (Planet.TriangleDeltaT > 100) then
    Planet.TriangleDeltaT := Planet.TriangleDeltaT - 100;
  if (Key = $55) then
    Planet.TriangleDeltaT := Planet.TriangleDeltaT + 100;
  if (Key = $54) then
    Planet.DrawTriangle := not Planet.DrawTriangle;
end;

procedure TMainFrm.Idle(Sender: TObject; var Done: boolean);
var
  strs:TStringList;
  p:PAnsiChar;
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw.ClearSurface(clBlack);
    AdDraw.BeginScene;

    if Paused then
      Planet.Speed := 0
    else
      Planet.Speed := SimulationSpeed;

    AdSpriteEngine.Move(AdPerCounter.TimeGap / 1000);
    AdSpriteEngine.Draw;
    AdSpriteEngine.Dead;

    strs := TStringList.Create;

    with AdDraw.Fonts.GenerateFont('MS Sans Serif', 12, []) do
    begin
      with TypeSetter as TAdSimpleTypeSetter do
      begin
        DrawMode := [dtWordWrap, dtDoLineFeeds]
      end;

      TextOut(ClientWidth - 100,0, 'FPS: '+inttostr(AdPerCounter.FPS));
      if Paused then
        strs.Add('Press SPACE to start simulation')
      else
        strs.Add('Press R to reset simulation');

      strs.Add('Simulation speed: '+FormatFloat('#0.0',SimulationSpeed)+' [+/-]');
      strs.Add('Sun mass: '+FormatFloat('0.#####e+',Sun.Mass)+'kg [N/M]');
      strs.Add('Planet start velocity: '+FormatFloat('0.000',Planet.Velocity)+'AE/s [V/B]');
      strs.Add('Planet start distance: '+FormatFloat('0.00',Planet.Distance)+'AE [X/C]');
      strs.Add('Draw triangle: '+BoolToStr(Planet.DrawTriangle)+' [T]');
      strs.Add('Triangle delta T: '+FormatFloat('0.0',Planet.TriangleDeltaT/1000)+'s [Z/U]');

      strs.Add('Current planet sun distance: '+FormatFloat('0.00',Planet.GetDistance)+'AE');
      strs.Add('Current planet velocity: '+FormatFloat('0.00',Sqrt(sqr(Planet.MoveDrawVect.x)+sqr(Planet.MoveDrawVect.y))));

      p := strs.GetText;
      TextOut(AdRect(0, 0, ClientWidth, ClientHeight), p);
      StrDispose(p);

      strs.Free;

      TextOut(0,ClientHeight - 32,'(c) by Andreas Stöckel');
    end;

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
end;

{ TPlanet }

constructor TPlanet.Create(AParent: TSprite);
begin
  inherited;
  FMass := 5.9 * Power(10,24);
  DoTrace := true;
  FDrawTriangle := false;
  FTriangleDeltaT := 5000;
end;

destructor TPlanet.Destroy;
begin
  FreeTraceList;
  inherited;
end;

procedure TPlanet.DoDraw;
var
  i:integer;
  quad:TAdCanvasQuad;
begin
  with Engine.Surface.Canvas do
  begin
    Pen.Width := 1;

    //Draw trace
    if FDoTrace and (FTrace <> nil) and (FTrace.Count > 0) then
    begin

      i := 0;
      FTrace.StartIteration;
      while not FTrace.ReachedEnd do
      begin
        Pen.Color := Ad_ARGB(round((255 / FTrace.Count) * i),128,128,128);
        LineTo(PAdPoint(FTrace.GetCurrent)^);
        i := i + 1;
      end;

      Release;
    end;

    //Draw Triangle
    if FDrawTriangle then
    begin
      Brush.Color := Ad_ARGB(128,255,255,0);
      quad.p[0] := FTrianglePoint1;
      quad.p[1] := FTrianglePoint2;
      quad.p[2] := AdVector2(FSun.X, FSun.Y);
      quad.p[3] := AdVector2(FSun.X, FSun.Y);
      DrawQuad(quad);
      Release;
    end;

    //Draw planet
    Image.DrawRotate(Engine.Surface, round(X)-20, round(Y)-20, 40, 40, 0, 0.5, 0.5, round(GetAngle/(2*PI)*360));

    //Draw sun distance
    Pen.Color := Ad_ARGB(64,255,255,255);
    Line(AdPoint(round(FSun.X),round(FSun.Y)), AdPoint(round(X),round(Y)));

    Pen.Width := 3;

    //Draw move vector
    Pen.Color := Ad_ARGB(255,255,0,0);
    Arrow(20, 50, AdPoint(X, Y), AdPoint(X+FMoveDrawVect.x * 5, Y+FMoveDrawVect.y * 5));

    //Draw sun gravity vector
    Pen.Color := Ad_ARGB(255,0,0,255);
    Arrow(20, 50, AdPoint(X, Y), AdPoint(X+FSunDrawVect.x * 65, Y+FSunDrawVect.y * 65));
  end;
end;

procedure TPlanet.CalcPos(var AX, AY: double; var AMoveVect:TAdVector2; TimeGap: double;
  SetDrawVectors: boolean);
var
  posx,posy:Extended;
  accx, accy: Extended;
  r:Extended;
begin
  //Calculate the relative position of the planet in meters
  posx := AEToM(PxToAE(AX - FSun.X));
  posy := AEToM(PxToAE(AY - FSun.Y));

  //Calculate distance in meters
  r := Sqrt(Sqr(PosX)+Sqr(PosY));

  //Calculate acceleration
  accx := (-6.67 * Power(10, -11) * FSun.Mass) * (posx / Power(r, 3));
  accy := (-6.67 * Power(10, -11) * FSun.Mass) * (posy / Power(r, 3));

  //Calculate velocity
  AMoveVect.X := AMoveVect.X + accx * TimeGap;
  AMoveVect.Y := AMoveVect.Y + accy * TimeGap;

  if SetDrawVectors then
  begin
    FMoveDrawVect.X := AEToPx(AMoveVect.X);
    FMoveDrawVect.Y := AEToPx(AMoveVect.Y);

    FSunDrawVect.X := AEToPx(accx);
    FSunDrawVect.Y := AEToPx(accy);
  end;

  AX := AX + AEToPx(AMoveVect.X * TimeGap);
  AY := AY + AEToPx(AMoveVect.Y * TimeGap);
end;

procedure TPlanet.DoMove(TimeGap: double);
var
  p:PAdPoint;
  ax, ay:double;
begin
  inherited;

  TimeGap := TimeGap * FSpeed;

  ax := x; ay := y;
  CalcPos(ax, ay, FMoveVect, TimeGap, true);
  x := ax; y := ay;

  if (FDoTrace) and ((abs(FTracePoint.x - X) > 5) or (abs(FTracePoint.y - Y) > 5)) then
  begin
    FTracePoint := AdPoint(x,y);
    if FTrace.Count > 500 then
    begin
      FTrace.StartIteration;
      Dispose(PAdPoint(FTrace.GetCurrent));
      FTrace.Delete(0);
    end;

    New(p);
    p^ := AdPoint(x,y);
    FTrace.Add(p)
  end;

  FTrianglePointTime := FTrianglePointTime + TimeGap * Speed * 100;
  FTrianglePoint1 := AdVector2(X,Y);
  if FTrianglePointTime > FTriangleDeltaT then
  begin
    ax := FTrianglePoint2.X;
    ay := FTrianglePoint2.Y;
    CalcPos(ax, ay, FTriangleMoveVect, TimeGap, false);
    FTrianglePoint2.X := ax;
    FTrianglePoint2.Y := ay;
  end;
end;

procedure TPlanet.FreeTraceList;
var
  i: Integer;
begin
  if FTrace <> nil then
  begin
    FTrace.StartIteration;
    while not FTrace.ReachedEnd do
    begin
      Dispose(PAdPoint(FTrace.GetCurrent));
    end;
    for i := 0 to FTrace.Count - 1 do
    begin
      FTrace.Delete(0);
    end;
    
    FTrace.Free;
    FTrace := nil;
  end;
end;

function TPlanet.GetAngle: double;
begin
  result := ArcCos((X-FSun.X)/AEToPx(GetDistance));
  if (FSun.Y > Y) then
  begin
    result := 2 * pi - result;
  end;
end;

function TPlanet.GetDistance: double;
begin
  result := PxToAE(sqrt(sqr(X-FSun.X)+sqr(Y-FSun.Y)));
end;

procedure TPlanet.Reset;
begin
  FAngle := 0;
  FDistance := 1;
  FVelocity := 0.08;
  SetPos;
end;

procedure TPlanet.SetAngle(AValue: double);
begin
  FAngle := AValue;
  SetPos;
end;

procedure TPlanet.SetDistance(AValue: double);
begin
  FDistance := AValue;
  SetPos;
end;

procedure TPlanet.SetDoTrace(AValue: boolean);
begin
  if (FDoTrace <> AValue) then
  begin
    FreeTraceList;
    if AValue then
    begin
      FTrace := TAdLinkedList.Create;
    end;
  end;
  FDoTrace := avalue;
end;

procedure TPlanet.SetVelocity(AValue: double);
begin
  FVelocity := AValue;
  SetPos;
end;     

procedure TPlanet.SetPos;
begin
  X := FSun.X + cos(FAngle) * AEToPx(FDistance);
  Y := FSun.Y + sin(FAngle) * AEToPx(FDistance);
  FMoveVect := AdVector2(0,-FVelocity);

  DoTrace := not FDoTrace;
  DoTrace := not FDoTrace;

  FTrianglePoint1 := AdVector2(x,y);
  FTrianglePoint2 := AdVector2(x,y);
  FTrianglePointTime := 0;
  FTriangleMoveVect := AdVector2(0,-FVelocity);
end;

procedure TPlanet.SetTriangleDeltaT(AValue: double);
begin
  FTriangleDeltaT := AValue;
  SetPos;
end;

{ TSun }

constructor TSun.Create(AParent: TSprite);
begin
  inherited;

  FMass := 2 * Power(10,30);

  Width := 120;
  Height := 120;
  
  X := Engine.SurfaceRect.Right div 2;
  Y := Engine.SurfaceRect.Bottom div 2;

  FParticleSystem := TAdParticleSystem.Create(Engine.Surface.Parent);
end;

destructor TSun.Destroy;
begin
  FParticleSystem.Destroy;
  inherited;
end;

procedure TSun.DoDraw;
begin
  with Engine.Surface.Canvas do
  begin
    Brush.Color := Ad_ARGB(255,255,128,100);
    Brush.GradientColor := Ad_ARGB(0,255,128,0);
    Pen.Style := apNone;
    Circle(round(x), round(y), 70);
    Release;
  end;
  
  FParticleSystem.Draw(Engine.Surface, X, Y);
end;

procedure TSun.DoMove(TimeGap: double);
begin
  inherited;

  FTime := FTime + TimeGap * 1000;

  FParticleSystem.Move(TimeGap);

  if FTime > 10 then
  begin
    FParticleSystem.Emit(1, 0,0);
    FTime := FTime - 10;
  end;
end;

procedure TSun.SetParticleSystem(const Value: TAdParticleSystem);
begin
  FParticleSystem := Value;
end;

end.
