{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and 
* the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Authors:  Andreas Stoeckel
* File: AdParticles.pas
* Comment: Contains particle system classes and types.
}

{ Contains particle system classes and types. }     
unit AdParticles;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$INCLUDE andorra2d.inc}

interface

uses
  SysUtils, Classes, SyncObjs, Math,
  AdMath, AdDraws, AdPersistent, AdClasses, AdTypes, AdColorList, AdList, AdBitmap,
  AdSimpleXML;

type

  { The main, abstract particle class. TAdParticle has methods to save and
    load settings from/to XML and gives chlidren class the ability to
    create vertex/index data for drawing. Do not use TAdParticle directly.}
  TAdParticle = class;

  { A particle system is a emiter for a certain particle class. TAdParticleSystem
    takes care of creating/emiting and destroying particles. It is also able to
    load and store settings from/in XML. }
  TAdParticleSystem = class;

  { TAdParticleData is internally used by TAdParticleCalculationThread,
    TAdParticle and TAdParticleSystem to exchange vertex/index data between
    them. TAdParticleData also contains some additional data which describes
    how far data creation has processed or the bounding rectangle of the
    particle system.}
  TAdParticleData = record
    Vertices: TAdVertexArray; //< Contains the vertices of the particle system.
    Indices: TAdIndexArray; //< Contains the indices of the particle system. If the index buffer is not used, the field may be nil.
    PrimitiveCount: integer; //< Stores how many primitives should be rendered.
    VertPos: integer; //< The current vertex-array index
    IndPos: integer; //< The current index-array index
    MinX: double; //< The minimum X-Coordinate of the particle system.
    MinY: double; //< The minimum Y-Coordinate of the particle system.
    MaxX: double; //< The maximum X-Coordinate of the particle system.
    MaxY: double; //< The maximum Y-Coordinate of the particle system.
  end;

  { Pointer on TAdParticle data. Used to prevent the compiler magic to create
    a copy of TAdParticleData when exchanging between the modules. }
  PAdParticleData = ^TAdParticleData;

  { A list managing the particles. Particles are automaticly freed when they
    are deleted from the list. }
  TAdParticleList = class(TAdList)
    private
    	function GetItem(AIndex:integer):TAdParticle;
     	procedure SetItem(AIndex:integer;AItem:TAdParticle);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      { Provides access on the particle list. Remember that this method is not
        thread safe. Particle data should only be changed, when the particle
        processing thread is currently waiting. }
  	  property Items[AIndex:integer]:TAdParticle read GetItem write SetItem;default;

      { Adds a particle to the list. Remember that the list "owns" the
        particle, what means that it will automatically be freed when removing
        it from the list. This method is not thread safe. Remember not to add
        particles when the particle processing thread is not waiting. }
      procedure Add(AItem:TAdParticle);
  end;

  { The main, abstract particle class. TAdParticle has methods to save and
    load settings from/to XML and gives chlidren class the ability to
    create vertex/index data for drawing. Do not use TAdParticle directly.}
  TAdParticle = class(TAdPersistent)
    private
      FParticleSystem: TAdParticleSystem;
      FDeaded: boolean;
    protected
      property ParticleSystem: TAdParticleSystem read FParticleSystem;
    public
      { Returns the amount of vertices that is needed for one particle. }
      class function VerticesPerParticle: integer;virtual;abstract;

      { Returns the amount of indices that is needed for one particle. }
      class function IndicesPerParticle: integer;virtual;abstract;

      { Returns the mode the particle system should be drawn in. }
      class function DrawMode: TAd2DDrawMode;virtual;abstract;

      { Creates an instance of TAdParticle. }
      constructor Create(ASystem: TAdParticleSystem);virtual;

      { Calling this procedure will mark the particle as "dead". The particle
        system will remove it as soon as possible. }
      procedure Dead;virtual;

      { Initializes the particle. }
      procedure SetupMovement(AX, AY: integer);virtual;abstract;

      { Copys the properties from the given particle. }
      procedure Assign(APart: TAdParticle);virtual;abstract;

      { Calculates the position of the particle. This step is done within the
        particle calculation thread. }
      procedure Move(ATimeGap: double);virtual;abstract;

      { Stores the vertex/index data in AData. This step is done within the
        particle calculation thread.}
      procedure StoreData(AData: PAdParticleData);virtual;abstract;

      { Updates the particle system boundsrect. }
      procedure StoreMinMax(AData: PAdParticleData);virtual;abstract;

      { Returns whether the particle is marked as dead. }
      property Deaded: boolean read FDeaded;

      { Saves the particle data to a new xml node. }
      function SaveToXML(ARoot: TAdSimpleXMLElems):TAdSimpleXMLElem;virtual;

      { Loads the particle data from the given xml node. }
      procedure LoadFromXML(ARoot: TAdSimpleXMLElem);virtual;

      { Saves the particle object to any desired stream. }
      procedure SaveToStream(AStream: TStream);
      { Loads the particle object from any desired stream. }
      procedure LoadFromStream(AStream: TStream);

      { Saves the particle object to a file.}
      procedure SaveToFile(AFile: string);
      { Loads the particle object from a file.}
      procedure LoadFromFile(AFile: string);
  end;

  {Class of TAdParticle. Used for registering purposes.}
  TAdParticleClass = class of TAdParticle;

  {$M+}
  {Represents a "variation value" of TAdStdParticle. Those values have
   a start value, representing the value at the begining of the life of the particle
   and a end value that will reached at the end of the life of the particle. Variation
   represents the maximum variation in percent.}
  TAdParticleParameter = class
    private
      FStart, FStop, FVariation: double;
    public
      {Loads the value from a XML node.}
      procedure LoadFromXML(AName: string; ARoot: TAdSimpleXMLElem);
      {Saves the value to a XML node.}
      procedure SaveToXML(AName: string; ARoot: TAdSimpleXMLElem);

      {Assigns another particle parameter.}
      procedure Assign(AParam: TAdParticleParameter);
    published
      {The start value of the parameter. The parameter has this value at the
       begin of the particle life.}
      property Start: double read FStart write FStart;
      {The stop value of the parameter. The parameter has this value at the
       end of the particle life.}
      property Stop: double read FStop write FStop;
      {The variation factor for the particle parameter. In percent.}
      property Variation: double read FVariation write FVariation;
  end;
  {$M-}
  
  {A standard particle class that implements the behaviour of a base particle.
   This classes uses point sprites for drawing the particles. Operations like
   scaling or rotation are not availale in this particle class.}
  TAdStdParticle = class(TAdParticle)
    private
      FPosition: TAdVector3;
      FVelocity: TAdVector3;
      FLivedTime: double;

      FMaxLifeTime: double;
      FLifeTimeVariation: double;

      FXVelocity: TAdParticleParameter;
      FYVelocity: TAdParticleParameter;
      FZVelocity: TAdParticleParameter;

      FCreationAngle: integer;
      FCreationAngleRange: integer;
    protected
      property LivedTime: double read FLivedTime write FLivedTime;
      property Position: TAdVector3 read FPosition write FPosition;
      property Velocity: TAdVector3 read FVelocity write FVelocity;

      function CalcValue(MaxTime, TimePos, StartPos, EndPos: double): double; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
      procedure ApplyVariation(var AVal: double; AVar: double);
      procedure ApplyVelVariation(var AVel: TAdParticleParameter);
    public
      constructor Create(ASystem: TAdParticleSystem);override;
      destructor Destroy;override;

      class function VerticesPerParticle: integer;override;
      class function IndicesPerParticle: integer;override;
      class function DrawMode: TAd2DDrawMode;override;

      procedure SetupMovement(AX, AY: integer);override;
      procedure Assign(APart: TAdParticle);override; 

      procedure Move(ATimeGap: double);override;
      procedure StoreData(AData: PAdParticleData);override;
      procedure StoreMinMax(AData: PAdParticleData);override;

      function SaveToXML(ARoot: TAdSimpleXMLElems):TAdSimpleXMLElem;override;
      procedure LoadFromXML(ARoot: TAdSimpleXMLElem);override;
    published
      {Describes how fast the particle should move on the X-Axis. Unit: Pixel per second}
      property XVelocity: TAdParticleParameter read FXVelocity write FXVelocity;
      {Describes how fast the particle should move on the Y-Axis. Unit: Pixel per second}
      property YVelocity: TAdParticleParameter read FYVelocity write FYVelocity;
      {Describes how fast the particle should move on the Z-Axis. Unit: Pixel per second}
      property ZVelocity: TAdParticleParameter read FZVelocity write FZVelocity;

      {Describes how long the particle should live. Unit: seconds}
      property LifeTime: double read FMaxLifeTime write FMaxLifeTime;
      {Use this value to variate the life time. Unit: percent. Range: 0-100}
      property LifeTimeVariation: double read FLifeTimeVariation write FLifeTimeVariation;

      {The angle the particles should be emitted from. Unit: degrees. Range: 0-360}
      property CreationAngle: integer read FCreationAngle write FCreationAngle;
      {Relative to the angle given in "CreationAngle", this value sets the range
       particles are emitted from. Unit: degrees. Range: 0-360}
      property CreationAngleRange: integer read FCreationAngleRange write FCreationAngleRange;
  end;

  {A particle class extending TAdStdParticle that uses billboard sprits for
   drawing the particles and therefore supports rotation and scaling.}
  TAdBillboardParticle = class(TAdStdParticle)
    private
      FCalculatedSize: double;
      FCalculatedAngle: double;
      
      FSize: TAdParticleParameter;
      FAngle: TAdParticleParameter;

      function RotatePoint(ACenter: TAdVector3; AR, AAngle: double): TAdVector3;
    public
      constructor Create(ASystem: TAdParticleSystem);override;
      destructor Destroy;override;

      class function VerticesPerParticle: integer;override;
      class function IndicesPerParticle: integer;override;
      class function DrawMode: TAd2DDrawMode;override;

      procedure SetupMovement(AX, AY: integer);override;
      procedure Assign(APart: TAdParticle);override;

      procedure Move(ATimeGap: double);override;
      procedure StoreData(AData: PAdParticleData);override;

      function SaveToXML(ARoot: TAdSimpleXMLElems):TAdSimpleXMLElem;override;
      procedure LoadFromXML(ARoot: TAdSimpleXMLElem);override;
    published
      {The factor size of the particle.}
      property Size: TAdParticleParameter read FSize write FSize;
      {The rotation of the particle in radiant.}
      property Angle: TAdParticleParameter read FAngle write FAngle;
  end;

  TAdParticleCalculationThread = class(TThread)
    private
      FHasData: boolean;
      FStartedCalculation: boolean;

      FTimeGap: double;

      FData: TAdParticleData;

      FParticleList: TAdParticleList;
      FParticleClass: TAdParticleClass;

      FHighPerformance: boolean;
      procedure SetParticleClass(AClass: TAdParticleClass);
    protected
      procedure ResizeData(AEffectSize: integer);
      procedure ResetData;
      procedure Execute;override;
    public
      constructor Create(AParticleList: TAdParticleList; AEffectSize: integer = 100);

      function HasData: boolean;
      function GetData: PAdParticleData;

      procedure Move(ATimeGap: double);

      property HighPerformance: boolean read FHighPerformance write FHighPerformance;
      property ParticleClass: TAdParticleClass read FParticleClass write SetParticleClass;
  end;

  TAdXMLColorList = class(TAdColorList)
    public
      procedure SaveToXML(AName: string; ARoot: TAdSimpleXMLElem);
      procedure LoadFromXML(AName: string; ARoot: TAdSimpleXMLElem);
  end;

  TAdParticleSystem = class
    private
      FParent: TAdDraw;
      FMesh: TAd2dMesh;

      FParticleCalcThread: TAdParticleCalculationThread;
      FParticleList: TAdParticleList;

      FDefaultParticle: TAdParticle;
      FOwnDefaultParticle: boolean;

      FTexture: TAdCustomTexture;
      FOwnTexture: boolean;

      FHighPerformance: boolean;

      FColors: TAdXMLColorList;
      FOwnColorList: boolean;

      FTimeGap: double;
      FTime: double;
      FTmpFPS: integer;
      FPartSysFPS: integer;

      FAddParticles: TList;
      FAddParticleIndex: integer;

      FMinX, FMinY, FMaxX, FMaxY: double;

      procedure SetHighPerformance(AValue: boolean);
      procedure SetTexture(AValue: TAdCustomTexture);
      procedure SetDefaultParticle(AValue: TAdParticle);
      procedure SetColors(AValue: TAdXMLColorList);
      function GetInitialized: boolean;
      function GetBoundsRect: TAdRect;
    protected
      procedure DeleteDeaded;
      procedure Notify(ASender:TObject; AEvent:TAdSurfaceEventState);
    public
      constructor Create(AParent: TAdDraw);
      destructor Destroy; override;

      procedure Initialize;
      procedure Finalize;

      procedure Emit(ACount, AX, AY: integer);

      procedure Draw(ADest: TAdSurface; AX, AY: double; ABlendMode: TAd2dBlendMode = bmAdd);
      procedure Move(ATimeGap: double);

      procedure SaveToXML(AName: string; ARoot: TAdSimpleXMLElem);
      procedure LoadFromXML(ARoot: TAdSimpleXMLElem);

      procedure SaveToStream(AStream: TStream);
      procedure LoadFromStream(AStream: TStream);

      procedure SaveToFile(AFile: string);
      procedure LoadFromFile(AFile: string);

      procedure Assign(APartSys: TAdParticleSystem; AReferenceOnly: boolean = true);

      property Texture: TAdCustomTexture read FTexture write SetTexture;
      property Parent: TAdDraw read FParent;
      property Initialized: boolean read GetInitialized;
      property Items: TAdParticleList read FParticleList;
      property HighPerformance: boolean read FHighPerformance write SetHighPerformance;
      property Colors: TAdXMLColorList read FColors write SetColors;
      property DefaultParticle: TAdParticle read FDefaultParticle write SetDefaultParticle;
      property FPS: integer read FPartSysFPS;
      property BoundsRect: TAdRect read GetBoundsRect;
  end;

{ Registers a new particle class, which can then be used in the editor and loaded
  from XML }
procedure RegisterParticleClass(AClass: TAdParticleClass);

var
  RegisteredParticleClasses: TStringList;

implementation

procedure RegisterParticleClass(AClass: TAdParticleClass);
begin
  RegisteredParticleClasses.Add(AClass.ClassName);
  AdRegisterClass(AClass);
end;

{ TAdParticleList }

procedure TAdParticleList.Add(AItem: TAdParticle);
begin
  inherited Add(AItem);
end;

function TAdParticleList.GetItem(AIndex:integer):TAdParticle;
begin
  result := inherited Items[AIndex];
end;

procedure TAdParticleList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if ( Action = lnDeleted ) then
  begin
    TAdParticle(Ptr).Free;
  end;
  Inherited;
end;

procedure TAdParticleList.SetItem(AIndex:integer;AItem:TAdParticle);
begin
  inherited Items[AIndex] := AItem;
end;

{ TAdParticleCalculationThread }

constructor TAdParticleCalculationThread.Create(AParticleList: TAdParticleList;
  AEffectSize: integer);
begin
  inherited Create(false);

  FParticleList := AParticleList;
  FParticleClass := nil;

  ResizeData(AEffectSize);

  FStartedCalculation := true;
end;

procedure TAdParticleCalculationThread.Execute;
var
  i: integer;
begin
  while not Terminated do
  begin
    if not HighPerformance then
    begin
      while (not FStartedCalculation) and (not Terminated) do
        Sleep(1);
    end else
      while (not FStartedCalculation) and (not Terminated) do;

    if not Terminated then
    begin
      ResetData;

      try
        ResizeData(FParticleList.Count);
        if FParticleList.Count > 0 then
        begin
          for i := 0 to FParticleList.Count - 1 do
          begin
            FParticleList[i].Move(FTimeGap);
            FParticleList[i].StoreMinMax(@FData);
            FParticleList[i].StoreData(@FData);
          end;
        end
        else
        begin
          //Set the size rectangle to zero if there are no particles in the system
          FData.MinX := 0; FData.MaxX := 0;
          FData.MinY := 0; FData.MaxY := 0;
        end; 

      finally
        FHasData := true;
        FStartedCalculation := false;
      end;
    end;
  end;
end;

function TAdParticleCalculationThread.GetData: PAdParticleData;
begin
  result := @FData;
  FHasData := false;
end;

function TAdParticleCalculationThread.HasData: boolean;
begin
  result := FHasData;
end;

procedure TAdParticleCalculationThread.Move(ATimeGap: double);
begin
  if not FStartedCalculation then
  begin
    FTimeGap := ATimeGap;
    FStartedCalculation := true;
  end;
end;

procedure TAdParticleCalculationThread.ResetData;
begin
  FData.PrimitiveCount := 0;
  FData.VertPos := 0;
  FData.IndPos := 0;

  FHasData := false;
end;

procedure TAdParticleCalculationThread.ResizeData(AEffectSize: integer);
begin
  if (FParticleClass <> nil) and
     (AEffectSize * FParticleClass.VerticesPerParticle > Length(FData.Vertices)) then
  begin
    SetLength(FData.Vertices, AEffectSize * FParticleClass.VerticesPerParticle * 2);

    if FParticleClass.IndicesPerParticle <> 0 then    
      SetLength(FData.Indices, AEffectSize * FParticleClass.IndicesPerParticle * 2)
    else
      FData.Indices := nil;
  end;
end;

procedure TAdParticleCalculationThread.SetParticleClass(
  AClass: TAdParticleClass);
begin
  if AClass <> FParticleClass then
  begin
    FParticleClass := AClass;
    ResizeData(FParticleList.Count);
  end;
end;

{ TAdParticle }

constructor TAdParticle.Create(ASystem: TAdParticleSystem);
begin
  inherited Create;

  FParticleSystem := ASystem;
end;

procedure TAdParticle.Dead;
begin
  FDeaded := true;
end;

procedure TAdParticle.LoadFromXML(ARoot: TAdSimpleXMLElem);
begin
  //Nothing to do now
end;

function TAdParticle.SaveToXML(ARoot: TAdSimpleXMLElems): TAdSimpleXMLElem;
begin
  //Add new node and return it
  result := ARoot.Add(ClassName);
end;


procedure TAdParticle.SaveToFile(AFile: string);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  SaveToXML(xml.Root.Items);
  xml.SaveToFile(AFile);
  xml.Free;
end;

procedure TAdParticle.SaveToStream(AStream: TStream);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  SaveToXML(xml.Root.Items);
  xml.SaveToStream(AStream);
  xml.Free;
end;

procedure TAdParticle.LoadFromFile(AFile: string);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  xml.LoadFromFile(AFile);
  LoadFromXML(xml.Root);
  xml.Free;
end;

procedure TAdParticle.LoadFromStream(AStream: TStream);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  xml.LoadFromStream(AStream);
  LoadFromXML(xml.Root);
  xml.Free;
end;

{ TAdParticleSystem }

constructor TAdParticleSystem.Create(AParent: TAdDraw);
begin
  inherited Create;

  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);

  FOwnTexture := true;
  FTexture := TAdTexture.Create(FParent);

  FParticleList := TAdParticleList.Create;
  FAddParticles := TList.Create;

  FDefaultParticle := TAdStdParticle.Create(self);
  FOwnDefaultParticle := true;

  FParticleCalcThread := TAdParticleCalculationThread.Create(FParticleList);
  FParticleCalcThread.ParticleClass := TAdParticleClass(FDefaultParticle.ClassType);

  FOwnColorList := true;
  FColors := TAdXMLColorList.Create;
  FColors.Add(Ad_ARGB(255, 255, 255, 255));
  FColors.Add(Ad_ARGB(0  , 255, 255, 255));

  Initialize;

  HighPerformance := false;
end;

procedure TAdParticleSystem.DeleteDeaded;
var
  i: integer;
begin
  i := 0;
  
  while i < FParticleList.Count do
  begin
    if FParticleList[i].Deaded then
    begin
      FParticleList.Delete(i);
    end else
    begin
      i := i + 1;
    end;
  end;
end;

destructor TAdParticleSystem.Destroy;
begin
  FParticleCalcThread.Free;

  FParent.UnRegisterNotifyEvent(Notify);
  FParticleList.Free;

  Finalize;

  FAddParticles.Free;

  if (FOwnTexture) then
    FTexture.Free;

  if (FOwnDefaultParticle) then
    FDefaultParticle.Free;

  if (FOwnColorList) then
    FColors.Free;

  inherited;
end;

procedure TAdParticleSystem.Assign(APartSys: TAdParticleSystem; AReferenceOnly: boolean);
var
  bmp: TAdBitmap;
  ms: TMemoryStream;
begin
  if AReferenceOnly then
  begin
    DefaultParticle := APartSys.DefaultParticle;
    Colors := APartSys.Colors;
    Texture := APartSys.Texture;    
  end else
  begin
    //Copy most important particle data
    ms := TMemoryStream.Create;
    APartSys.SaveToStream(ms);
    ms.Position := 0;
    APartSys.LoadFromStream(ms);
    ms.Free;

    //Copy texture
    if APartSys.Texture is TAdTexture then
    begin
      Texture := TAdTexture.Create(FParent);
      FOwnTexture := true;

      bmp := TAdBitmap.Create;
      bmp.ReserveMemory(
        APartSys.Texture.Texture.BaseWidth,
        APartSys.Texture.Texture.BaseHeight);
      TAd2dBitmapTexture(APartSys.Texture.Texture).SaveToBitmap(bmp);

      TAd2dBitmapTexture(Texture.Texture).LoadFromBitmap(bmp, APartSys.Texture.BitDepth);
      bmp.Free;
    end else
      Texture := APartSys.Texture;
  end;

  HighPerformance := APartSys.HighPerformance;
end;

procedure TAdParticleSystem.Draw(ADest: TAdSurface; AX, AY: double; ABlendMode:
  TAd2dBlendMode);
begin
  if Initialized then
  begin
    ADest.Activate;

    FMesh.Matrix := AdMatrix_Translate(AX, AY, 0);
    FMesh.Draw(ABlendMode, FDefaultParticle.DrawMode);
  end;
end;

procedure TAdParticleSystem.Emit(ACount, AX, AY: integer);
var
  i: integer;
  part: TAdParticle;
begin
  if FDefaultParticle <> nil then
  begin
    for i := 0 to ACount - 1 do
    begin
      part := TAdParticleClass(FDefaultParticle.ClassType).Create(self);
      part.Assign(FDefaultParticle);
      part.SetupMovement(AX, AY);

      if FAddParticleIndex >= FAddParticles.Count then
        FAddParticles.Add(part)
      else
        FAddParticles.Items[FAddParticleIndex] := part;

      FAddParticleIndex := FAddParticleIndex + 1;
    end;
  end;
end;

procedure TAdParticleSystem.Finalize;
var
  i: integer;
begin
  if Initialized then
  begin
    FMesh.Free;
    FMesh := nil;

    for i := 0 to FAddParticleIndex - 1 do
      TAdParticle(FAddParticles[i]).Free;

    FAddParticles.Clear;
    FAddParticleIndex := 0;

    FPartSysFPS := 0;
    FTime := 0;
    FTmpFPS := 0;
  end;
end;

function TAdParticleSystem.GetBoundsRect: TAdRect;
begin
  result := AdRect(FMinX, FMinY, FMaxX, FMaxY);
end;

function TAdParticleSystem.GetInitialized: boolean;
begin
  result := FMesh <> nil;
end;

procedure TAdParticleSystem.Initialize;
begin
  Finalize;

  FMesh := FParent.AdAppl.CreateMesh;
  FTimeGap := 0;
end;

procedure TAdParticleSystem.Move(ATimeGap: double);
var
  PData: PAdParticleData;
  i: integer;
begin
  if Initialized then
  begin
    FTimeGap := FTimeGap + ATimeGap;

    //Wait for the calculation thread to end its calculations.
    if FParticleCalcThread.HasData then
    begin
      //Pick up data
      PData := FParticleCalcThread.GetData;

      //If some data has been created (PrimitiveCount > 0), store it in the mesh
      //structure of the graphic plugin and prepare for rendering.
      FMesh.PrimitiveCount := PData^.PrimitiveCount;
      if PData^.PrimitiveCount > 0 then
      begin
        FMesh.Vertices := PData^.Vertices;
        FMesh.Indices := PData^.Indices;

        FMesh.Texture := FTexture.Texture;
        FMesh.Matrix := AdMatrix_Identity;

        FMesh.Update;
      end;

      FParticleCalcThread.ParticleClass := TAdParticleClass(FDefaultParticle.ClassType);

      //Delete all particles that are marked as "dead". This is only possible now
      //because the calculation thread is waiting for us to call the "Move"
      //function.
      DeleteDeaded;

      //Add new particles. This is only possible now, because the calculation thread
      //is waiting for us to call the "Move" function
      for i := 0 to FAddParticleIndex - 1 do
        FParticleList.Add(FAddParticles[i]);

      FAddParticleIndex := 0;

      //Copy min-max data
      FMinX := PData^.MinX;
      FMinY := PData^.MinY;
      FMaxX := PData^.MaxX;
      FMaxY := PData^.MaxY;

      //Tell the calculation thread to go on with its calculations
      FParticleCalcThread.Move(FTimeGap);

      //Calculate particle system fps
      FTmpFPS := FTmpFPS + 1;
      FTime := FTime + FTimeGap;
      if FTime > 1 then
      begin
        FPartSysFPS := FTmpFPS;
        FTime := 0;
        FTmpFPS := 0;
      end;
      FTimeGap := 0;

    end;
  end;
end;

procedure TAdParticleSystem.Notify(ASender: TObject;
  AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialize: Initialize;
    seFinalize: Finalize;
  end;
end;

procedure TAdParticleSystem.SetColors(AValue: TAdXMLColorList);
begin
  if FOwnColorList then
  begin
    FColors.Free;
    FOwnColorList := false;
  end;

  FColors := AValue;
end;

procedure TAdParticleSystem.SetDefaultParticle(AValue: TAdParticle);
begin
  if FOwnDefaultParticle then
  begin
    FDefaultParticle.Free;
    FOwnDefaultParticle := false;
  end;

  FDefaultParticle := AValue;
end;

procedure TAdParticleSystem.SetHighPerformance(AValue: boolean);
begin
  FHighPerformance := AValue;
  FParticleCalcThread.HighPerformance := FHighPerformance;
end;

procedure TAdParticleSystem.SetTexture(AValue: TAdCustomTexture);
begin
  if (FOwnTexture) and (FMesh <> nil) then
  begin
    FTexture.Free;
    FOwnTexture := false;
  end;

  FTexture := AValue;
end;

procedure TAdParticleSystem.LoadFromFile(AFile: string);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  xml.LoadFromFile(AFile);
  LoadFromXML(xml.Root);
  xml.Free;
end;

procedure TAdParticleSystem.LoadFromStream(AStream: TStream);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  xml.LoadFromStream(AStream);
  LoadFromXML(xml.Root);
  xml.Free;
end;

procedure TAdParticleSystem.SaveToFile(AFile: string);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  SaveToXML('system', xml.Root);
  xml.SaveToFile(AFile);
  xml.Free;
end;

procedure TAdParticleSystem.SaveToStream(AStream: TStream);
var
  xml: TAdSimpleXML;
begin
  xml := TAdSimpleXML.Create;
  SaveToXML('system', xml.Root);
  xml.SaveToStream(AStream);
  xml.Free;
end;

procedure TAdParticleSystem.LoadFromXML(ARoot: TAdSimpleXMLElem);
var
  Node, PartNode: TAdSimpleXMLElem;
  classname: string;
  cref: TAdParticleClass;
begin
  Node := ARoot;
  if Node <> nil then
  begin
    classname := Node.Items.Value('particleclass', '');
    if classname <> '' then
    begin
      cref := TAdParticleClass(AdGetClass(classname));
      if cref <> nil then
      begin
        DefaultParticle := cref.Create(self);
        FOwnDefaultParticle := true;

        PartNode := Node.Items.ItemNamed[classname];
        if PartNode <> nil then
        begin
          DefaultParticle.LoadFromXML(PartNode);
        end;
      end;
    end;

    FColors.LoadFromXML('colors', Node);
  end;
end;

procedure TAdParticleSystem.SaveToXML(AName: string; ARoot: TAdSimpleXMLElem);
var
  Node: TAdSimpleXMLElem;
begin
  Node := ARoot.Items.Add(AName);
  Node.Items.Add('particleclass', FDefaultParticle.ClassName);
  FDefaultParticle.SaveToXML(Node.Items);
  FColors.SaveToXML('colors', Node);
end;

{ TAdStdParticle }

constructor TAdStdParticle.Create(ASystem: TAdParticleSystem);
begin
  inherited;

  FXVelocity := TAdParticleParameter.Create;
  FYVelocity := TAdParticleParameter.Create;
  FZVelocity := TAdParticleParameter.Create;

  FXVelocity.Start := 100;
  FXVelocity.Stop := 100;
  FXVelocity.Variation := 0;

  FYVelocity.Start := 100;
  FYVelocity.Stop := 100;
  FYVelocity.Variation := 0;

  FZVelocity.Start := 0;
  FZVelocity.Stop := 0;
  FZVelocity.Variation := 0;

  FMaxLifeTime := 1;
  FLivedTime := 0;
  FLifeTimeVariation := 0;

  FCreationAngleRange := 360;
  FCreationAngle := 0;
end;

destructor TAdStdParticle.Destroy;
begin
  FXVelocity.Free;
  FYVelocity.Free;
  FZVelocity.Free;

  inherited;
end;

function TAdStdParticle.SaveToXML(ARoot: TAdSimpleXMLElems): TAdSimpleXMLElem;
begin
  result := inherited SaveToXML(ARoot);

  with result do
  begin
    Items.Add('lifetime', LifeTime);
    Items.Add('creationangle', CreationAngle);
    Items.Add('creationanglerange', CreationAngleRange);
  end;

  XVelocity.SaveToXML('xvelocity', result);
  YVelocity.SaveToXML('yvelocity', result);
  ZVelocity.SaveToXML('zvelocity', result);
end;

procedure TAdStdParticle.LoadFromXML(ARoot: TAdSimpleXMLElem);
begin
  inherited;

  LifeTime := ARoot.Items.FloatValue('lifetime', 1);
  CreationAngle := ARoot.Items.IntValue('creationangle', 0);
  CreationAngleRange := ARoot.Items.IntValue('creationanglerange', 360);

  XVelocity.LoadFromXML('xvelocity', ARoot);
  YVelocity.LoadFromXML('yvelocity', ARoot);
  ZVelocity.LoadFromXML('zvelocity', ARoot);
end;

procedure TAdStdParticle.Assign(APart: TAdParticle);
var
  stdpart: TAdStdParticle;
begin
  if APart is TAdStdParticle then
  begin
    stdpart := TAdStdParticle(APart);

    FMaxLifeTime := stdpart.LifeTime;
    FLifeTimeVariation := stdpart.LifeTimeVariation;

    FXVelocity.Assign(stdpart.XVelocity);
    FYVelocity.Assign(stdpart.YVelocity);
    FZVelocity.Assign(stdpart.ZVelocity);

    FCreationAngle := stdpart.CreationAngle;
    FCreationAngleRange := stdpart.CreationAngleRange;
  end;
end;

procedure TAdStdParticle.ApplyVariation(var AVal: double; AVar: double);
var
  vari: double;
begin
  vari := (2*random * AVar - AVar ) / 200;

  AVal := AVal + AVal * vari;
end;

procedure TAdStdParticle.ApplyVelVariation(var AVel: TAdParticleParameter);
var
  vari: double;
begin
  vari := (random(round(AVel.Variation)) - AVel.Variation) / 100;

  AVel.Start := AVel.Start + AVel.Start * vari;
  AVel.Stop := AVel.Stop + AVel.Stop * vari;
end;


function TAdStdParticle.CalcValue(MaxTime, TimePos, StartPos, EndPos: double): double;
begin
  //How it works:
  //f(t) = m * t + b
  //m = (stoppos - startpos) / maxtime
  //b = startpos
  //==> f(t) = ((stoppos - startpos) / maxtime) * t + startpos
  
  result := ((EndPos - StartPos) / MaxTime) * TimePos + StartPos;
end;

class function TAdStdParticle.DrawMode: TAd2DDrawMode;
begin
  result := adPointSprites;
end;

class function TAdStdParticle.IndicesPerParticle: integer;
begin
  result := 0;
end;

class function TAdStdParticle.VerticesPerParticle: integer;
begin
  result := 1;
end;

procedure TAdStdParticle.Move(ATimeGap: double);
begin
  FPosition.X := FPosition.X + FVelocity.X * ATimeGap *
    CalcValue(FMaxLifeTime, FLivedTime, FXVelocity.Start, FXVelocity.Stop);

  FPosition.Y := FPosition.Y + FVelocity.Y * ATimeGap *
    CalcValue(FMaxLifeTime, FLivedTime, FYVelocity.Start, FYVelocity.Stop);

  FPosition.Z := FPosition.Z + FVelocity.Z * ATimeGap *
    CalcValue(FMaxLifeTime, FLivedTime, FZVelocity.Start, FZVelocity.Stop);

  FLivedTime := FLivedTime + ATimeGap;
  if FLivedTime > FMaxLifeTime then
    Dead;  
end;

procedure TAdStdParticle.SetupMovement(AX, AY: integer);
var
  max, min: double;
  angle: double;
begin
  //Set start position
  FPosition.X := AX;
  FPosition.Y := AY;
  FPosition.Z := 0;

  //Calculate the particle angle creation constraints
  max := (FCreationAngle + FCreationAngleRange / 2);
  min := (FCreationAngle - FCreationAngleRange / 2);
  angle := random * (max - min) + min;
  angle := angle * PI / 180;

  ApplyVelVariation(FXVelocity);
  ApplyVelVariation(FYVelocity);
  ApplyVelVariation(FZVelocity);

  ApplyVariation(FMaxLifeTime, FLifeTimeVariation);

  //Calculate particle base movement vector
  FVelocity := AdVector3(cos(angle), sin(angle), 0);
end;

procedure TAdStdParticle.StoreData(AData: PAdParticleData);
begin
  //Create one vertex with position data
  with AData^.Vertices[AData^.VertPos] do
  begin
    Position := AdVector3(FPosition.X, FPosition.Y, FPosition.Z);
    Texture := AdVector2(0, 0);
    Normal := AdVector3(0, 0, -1);
    Color := FParticleSystem.Colors.GetColor(FMaxLifeTime, FLivedTime);
  end;

  AData^.VertPos := AData^.VertPos + 1;
  AData^.PrimitiveCount := AData^.PrimitiveCount + 1;
end;

procedure TAdStdParticle.StoreMinMax(AData: PAdParticleData);
begin
  //Store min/max positions
  if (FPosition.X > AData^.MaxX) or (AData^.PrimitiveCount = 0) then
    AData^.MaxX := FPosition.X;
  if (FPosition.X < AData^.MinX) or (AData^.PrimitiveCount = 0) then
    AData^.MinX := FPosition.X;
  if (FPosition.Y > AData^.MaxY) or (AData^.PrimitiveCount = 0) then
    AData^.MaxY := FPosition.Y;
  if (FPosition.Y < AData^.MinY) or (AData^.PrimitiveCount = 0) then
    AData^.MinY := FPosition.Y;
end;

{ TAdBillboardParticle }

procedure TAdBillboardParticle.Assign(APart: TAdParticle);
var
  bbpart: TAdBillboardParticle;
begin
  inherited;
  if APart is TAdBillboardParticle then
  begin
    bbpart := TAdBillboardParticle(APart);

    FSize.Assign(bbpart.Size);
    FAngle.Assign(bbpart.Angle);
  end;
end;

constructor TAdBillboardParticle.Create(ASystem: TAdParticleSystem);
begin
  inherited;

  FSize := TAdParticleParameter.Create;
  FAngle := TAdParticleParameter.Create;

  FSize.Start := 1;
  FSize.Stop := 1;
  FSize.Variation := 0;

  FAngle.Start := 0;
  FAngle.Stop := 0;
  FAngle.Variation := 0;
end;

destructor TAdBillboardParticle.Destroy;
begin
  FSize.Free;
  FAngle.Free;
  inherited;
end;

class function TAdBillboardParticle.DrawMode: TAd2DDrawMode;
begin
  result := adTriangles;
end;

class function TAdBillboardParticle.IndicesPerParticle: integer;
begin
  result := 6;
end;

procedure TAdBillboardParticle.LoadFromXML(ARoot: TAdSimpleXMLElem);
begin
  inherited;

  FSize.LoadFromXML('size', ARoot);
  FAngle.LoadFromXML('angle', ARoot);
end;

function TAdBillboardParticle.SaveToXML(
  ARoot: TAdSimpleXMLElems): TAdSimpleXMLElem;
begin
  result := inherited SaveToXML(ARoot);

  FSize.SaveToXML('size', result);
  FAngle.SaveToXML('angle', result);
end;

procedure TAdBillboardParticle.Move(ATimeGap: double);
begin
  inherited;

  FCalculatedSize := CalcValue(LifeTime, LivedTime, FSize.Start, FSize.Stop);
  FCalculatedAngle := CalcValue(LifeTime, LivedTime, FAngle.Start, FAngle.Stop);
end;

function TAdBillboardParticle.RotatePoint(ACenter: TAdVector3; AR,
   AAngle: double): TAdVector3;
begin
  result.x := ACenter.x + cos(AAngle) * AR;
  result.y := ACenter.y + sin(AAngle) * AR;
  result.z := ACenter.z;
end;

procedure TAdBillboardParticle.SetupMovement(AX, AY: integer);
begin
  inherited;

  ApplyVelVariation(FSize);
  ApplyVelVariation(FAngle);
end;

procedure TAdBillboardParticle.StoreData(AData: PAdParticleData);
var
  i: integer;
  w, h, r: double;
begin
  //Calculate particle size
  w := FParticleSystem.Texture.Texture.Width * 0.5 * FCalculatedSize;
  h := FParticleSystem.Texture.Texture.Height * 0.5 * FCalculatedSize;

  //Draw rotated if necessary
  if IsZero(FCalculatedAngle, 0.001) then
  begin
    with Position do
    begin
      AData^.Vertices[AData^.VertPos + 0].Position :=
        AdVector3(X - w, Y - h, Z);
      AData^.Vertices[AData^.VertPos + 1].Position :=
        AdVector3(X + w, Y - h, Z);
      AData^.Vertices[AData^.VertPos + 2].Position :=
        AdVector3(X - w, Y + h, Z);
      AData^.Vertices[AData^.VertPos + 3].Position :=
        AdVector3(X + w, Y + h, Z);
    end;
  end else
  begin
    //Calculate the rotation radius
    r := sqrt(sqr(w) + sqr(h));

    AData^.Vertices[AData^.VertPos + 0].Position :=
      RotatePoint(Position, r , 5 * PI / 4 + FCalculatedAngle);
    AData^.Vertices[AData^.VertPos + 1].Position :=
      RotatePoint(Position, r, 7 * PI / 4 + FCalculatedAngle);
    AData^.Vertices[AData^.VertPos + 2].Position :=
      RotatePoint(Position, r, 3 * PI / 4 + FCalculatedAngle);
    AData^.Vertices[AData^.VertPos + 3].Position :=
      RotatePoint(Position, r , 1 * PI / 4 + FCalculatedAngle);
  end;

  //Set texture coordinates
  AData^.Vertices[AData^.VertPos + 0].Texture :=
    AdVector2(0, 0);
  AData^.Vertices[AData^.VertPos + 1].Texture :=
    AdVector2(1, 0);
  AData^.Vertices[AData^.VertPos + 2].Texture :=
    AdVector2(0, 1);
  AData^.Vertices[AData^.VertPos + 3].Texture :=
    AdVector2(1, 1);

  //Set color and normal
  for i := AData^.VertPos to AData^.VertPos + 3 do
  begin
    with AData^.Vertices[i] do
    begin
      Color := FParticleSystem.Colors.GetColor(LifeTime, LivedTime);
      Normal := AdVector3(0, 0, -1);
    end;
  end;

  //Set indices for the first triangle
  AData^.Indices[AData^.IndPos + 0] := AData^.VertPos + 0;
  AData^.Indices[AData^.IndPos + 1] := AData^.VertPos + 1;
  AData^.Indices[AData^.IndPos + 2] := AData^.VertPos + 2;

  //Set indices for the second triangle
  AData^.Indices[AData^.IndPos + 3] := AData^.VertPos + 1;
  AData^.Indices[AData^.IndPos + 4] := AData^.VertPos + 3;
  AData^.Indices[AData^.IndPos + 5] := AData^.VertPos + 2;

  AData^.VertPos := AData^.VertPos + 4;
  AData^.IndPos := AData^.IndPos + 6;
  AData^.PrimitiveCount := AData^.PrimitiveCount + 2;
end;

class function TAdBillboardParticle.VerticesPerParticle: integer;
begin
  result := 4;
end;

{ TAdColorXMLList }

procedure TAdXMLColorList.LoadFromXML(AName: string; ARoot: TAdSimpleXMLElem);
var
  i: integer;
  Node: TAdSimpleXMLElem;
begin
  Clear;

  Node := ARoot.Items.ItemNamed[AName];
  if Node <> nil then
  begin
    for i := 0 to Node.Items.Count - 1 do
    begin
      if Node.Items[i].Name = 'color' then
      begin
        Add(StringToAdColor(Node.Items[i].Value));
      end;
    end;
  end;
end;

procedure TAdXMLColorList.SaveToXML(AName: string; ARoot: TAdSimpleXMLElem);
var
  i: integer;
  Node: TAdSimpleXMLElem;
begin
  Node := ARoot.Items.Add(AName);

  for i := 0 to Count - 1 do
  begin
    Node.Items.Add('color', AdColorToString(Items[i]));
  end;
end;

{ TAdParticleParameter }

procedure TAdParticleParameter.Assign(AParam: TAdParticleParameter);
begin
  FStart := AParam.Start;
  FStop := AParam.Stop;
  FVariation := AParam.Variation;
end;

procedure TAdParticleParameter.LoadFromXML(AName: string;
  ARoot: TAdSimpleXMLElem);
var
  elem: TAdSimpleXMLElem;
begin
  elem := ARoot.Items.ItemNamed[AName];
  if elem <> nil then
  begin
    FStart := elem.Items.FloatValue('start', 0);
    FStop := elem.Items.FloatValue('stop', 0);
    FVariation := elem.Items.FloatValue('variation', 0);
  end;
end;

procedure TAdParticleParameter.SaveToXML(AName: string; ARoot: TAdSimpleXMLElem);
begin
  with ARoot.Items.Add(AName) do
  begin
    Items.Add('start', FStart);
    Items.Add('stop', FStop);
    Items.Add('variation', FVariation);
  end;
end;

initialization
  RegisteredParticleClasses := TStringList.Create;
  RegisterParticleClass(TAdStdParticle);
  RegisterParticleClass(TAdBillboardParticle);

finalization
  RegisteredParticleClasses.Free;

end.
