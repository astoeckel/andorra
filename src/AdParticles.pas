{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
* Project: Andorra 2D
* Authors:  Andreas Stoeckel
* File: AdParticles.pas
* Comment: This unit contains the particle engine
}

{ Contains the particle system engine }

unit AdParticles;

interface

uses SysUtils, Types, Classes,AdDraws, AdClasses;

type

  //The main particle class
  TAdParticle = class;

  //A list managing the particles. Deleted Particles are automaticly freed.
  TAdParticleList = class(TList)
    private
    	function GetItem(AIndex:integer):TAdParticle;
     	procedure SetItem(AIndex:integer;AItem:TAdParticle);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      //The "items" property of a list.
  	  property Items[AIndex:integer]:TAdParticle read GetItem write SetItem;default;
  end;

  //A Vector
  TAdVector = record
    X:double;//The X value
    Y:double;//The Y value
  end;

  //A list containing TAndorraColors and returning
  TAdColorList = class(TList)
    private
    	function GetItem(AIndex:integer):TAndorraColor;
    	procedure SetItem(AIndex:integer;AItem:TAndorraColor);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      //The "items" property of a list
    	property Items[AIndex:integer]:TAndorraColor read GetItem write SetItem;default;
      //Returns a mixed color value
      function GetColor(Max,Pos:double):TAndorraColor;
      //Add a color
      procedure Add(AColor:TAndorraColor);
      //Save the color list to the stream
      procedure SaveToStream(AStream:TStream);
      //Load the color list from a stream
      procedure LoadFromStream(AStream:TStream);
  end;

  //A class of a particle
  TAdParticleClass = class of TAdParticle;

  //Manages all particles.
  TAdParticleSystem = class
    private
      FTexture:TAdTexture;
      FImages:TPictureCollection;
      FDraw:TAdDraw;
      FParticles:TAdParticleList;
      FDefault:TAdParticle;
      procedure SetTexture(AValue:TAdTexture);
      function GetBoundsRect:TRect;
    protected
    public
      //Creates the particle system
      constructor Create(ADraw:TAdDraw);
      //Destroys the particle system
      destructor Destroy;override;
      //Creates an amount of particles of the specified particle class. The new particles get the settings stored in the "Default" particle.
      procedure CreateParticles(ACount:integer;AClass:TAdParticleClass;OffsetX,OffsetY:integer);virtual;
      //Draws the system at a specified position
      procedure Draw(X,Y:double);

      procedure Move(TimeGap:double);
      procedure Dead;
      procedure CreateImage(AColor:TAndorraColor);
      function GetImage(AColor:TAndorraColor):TPictureCollectionItem;
      property Texture:TAdTexture read FTexture write SetTexture;
      property Parent:TAdDraw read FDraw write FDraw;
      property Items:TAdParticleList read FParticles;
      property Images:TPictureCollection read FImages;
      property BoundsRect:TRect read GetBoundsRect;
      property DefaultParticle:TAdParticle read FDefault;
  end;

  TAdParticle = class
    private
      FX,FY:double;
      FDir:TAdVector;
      FForce:TAdVector;
      FLifeTime:double;
      FLifedTime:double;
      FLifeTimeVariation:integer;
      FSpeedVariation:integer;
      FColors:TAdColorList;
      FDeaded:boolean;
      FSystem:TAdParticleSystem;
      FLastImage:TPictureCollectionItem;
      FDrawMask:boolean;
      FColor:TAndorraColor;
      FSizeStart,FSizeEnd:double;
      FRotStart,FRotEnd:double;
      FSpeedXStart,FSpeedXEnd:double;
      FSpeedYStart,FSpeedYEnd:double;
      FCrAngle, FCrAngleOpen:integer;
      FBlendMode:TAd2DBlendMode;
      FSpeedVar:double;
      FName:ShortString;
      function GetBoundsRect:TRect;
      function GetValue(StartPos,EndPos,Max,Pos:double):double;
    protected
      function GetImage:TPictureCollectionItem;virtual;
    public
      constructor Create(ASystem:TAdParticleSystem);
      destructor Destroy;override;
      procedure DoMove(TimeGap:double);virtual;
      procedure DoDraw(AX,AY:double);virtual;
      procedure DoPreDraw(AX,AY:double);virtual;
      procedure Dead;virtual;
      procedure Assign(AParticle:TAdParticle);
      procedure SetupMovement;
      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);
      procedure SaveToFile(AFile:string);
      procedure LoadFromFile(AFile:string);
      property DrawMask:boolean read FDrawMask write FDrawMask;
      property Image:TPictureCollectionItem read GetImage;
      property Parent:TAdParticleSystem read FSystem;
      property Deaded:boolean read FDeaded;
      property Dir:TAdVector read FDir write FDir;
      property BoundsRect:TRect read GetBoundsRect;
      property LifeTime:double read FLifeTime write FLifeTime;
      property LifeTimeVariation:integer read FLifeTimeVariation write FLifeTimeVariation;
      property Colors:TAdColorList read FColors write FColors;
      property SizeStart:double read FSizeStart write FSizeStart;
      property SizeEnd:double read FSizeEnd write FSizeEnd;
      property RotStart:double read FRotStart write FRotStart;
      property RotEnd:double read FRotEnd write FRotEnd;
      property SpeedXStart:double read FSpeedXStart write FSpeedXStart;
      property SpeedYStart:double read FSpeedYStart write FSpeedYStart;
      property SpeedXEnd:double read FSpeedXEnd write FSpeedXEnd;
      property SpeedYEnd:double read FSpeedYEnd write FSpeedYEnd;
      property SpeedVariation:integer read FSpeedVariation write FSpeedVariation;
      property CreationAngle:integer read FCrAngle write FCrAngle;
      property CreationAngleOpen:integer read FCrAngleOpen write FCrAngleOpen;
      property Force:TAdVector read FForce write FForce;
      property BlendMode:TAd2DBlendMode read FBlendMode write FBlendmode;
      property Name:ShortString read FName write FName;
  end;

implementation

{ TAdParticleList }

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

{ TAdColorList }

type pAndorraColor = ^TAndorraColor;

procedure TAdColorList.Add(AColor: TAndorraColor);
var temp:pAndorraColor;
begin
  new(temp);
  temp^ := AColor;
  inherited Add(temp);
end;

function TAdColorList.GetColor(Max, Pos: double): TAndorraColor;
  function ColorBetween(C1, C2 : TAndorraColor; blend:Double):TAndorraColor;
  begin
     result.r := Round(C1.r + (C2.r-C1.r)*blend);
     result.g := Round(C1.g + (C2.g-C1.g)*blend);
     result.b := Round(C1.b + (C2.b-C1.b)*blend);
     result.a := Round(C1.a + (C2.a-C1.a)*blend);
  end;

var v1,v2:integer;
    v:single;
begin
  if pos > max then
  begin
    result := Items[count-1];
  end
  else
  begin
    if count > 0 then
    begin
      if (count > 1) and (pos > 0) then
      begin
        v := 1/(max/((count-1)*pos));
        if v > (count-1) then
        begin
          v := (count-1)
        end;
        if trunc(v) <> v then
        begin
          v1 := trunc(v);
          v2 := v1+1;
          result := ColorBetween(Items[v1],Items[v2],v-trunc(v));
        end
        else
        begin
          result := Items[round(v)];
        end;
      end
      else
      begin
        result := Items[0];
      end;
    end;
  end;
end;

function TAdColorList.GetItem(AIndex:integer):TAndorraColor;
begin
  result := TAndorraColor(inherited Items[AIndex]^);
end;

procedure TAdColorList.LoadFromStream(AStream: TStream);
var c,i:integer;
    tmp:TAndorraColor;
begin
  Clear;
  AStream.Read(c,sizeof(c));
  for i := 0 to c-1 do
  begin
    AStream.Read(tmp,SizeOf(tmp));
    Add(tmp);
  end;
end;

procedure TAdColorList.SaveToStream(AStream: TStream);
var i:integer;
    tmp:TAndorraColor;
begin
  i := Count;
  AStream.Write(i,sizeof(i));
  for i := 0 to Count - 1 do
  begin
    tmp := Items[i];
    AStream.Write(tmp,SizeOf(TAndorraColor))
  end;
end;

procedure TAdColorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if ( Action = lnDeleted ) then
  begin
    Dispose(PAndorraColor(Ptr));
  end;
  Inherited;
end;        

procedure TAdColorList.SetItem(AIndex:integer;AItem:TAndorraColor);
begin
  inherited Items[AIndex] := @AItem;
end;


{ TAdParticleSystem }

constructor TAdParticleSystem.Create(ADraw: TAdDraw);
begin
  inherited Create;
  FDraw := ADraw;
  FImages := TPictureCollection.Create(FDraw);
  FParticles := TAdParticleList.Create;
  FDefault := TAdParticle.Create(nil);
end;

procedure TAdParticleSystem.CreateImage(AColor: TAndorraColor);
begin
  if FTexture <> nil then
  begin
    with FImages.Add(AdColorToString(AColor)) do
    begin
      Texture := FTexture;
      Color := RGB(AColor.r,AColor.g,AColor.b);
      Restore;
    end;
  end;
end;

procedure TAdParticleSystem.CreateParticles(ACount: integer;
  AClass: TAdParticleClass;OffsetX,OffsetY:integer);
var
  i: Integer;
  apart:TAdParticle;
begin
  for i := 0 to ACount - 1 do
  begin
    apart := AClass.Create(self);
    FParticles.Add(apart);
    apart.FX := OffsetX;
    apart.FY := OffsetY;
    apart.Assign(FDefault);
    apart.SetupMovement;
  end;
end;

procedure TAdParticleSystem.Dead;
var i:integer;
begin
  i := 0;
  while i < FParticles.Count do
  begin
    if not FParticles[i].Deaded then
    begin
      i := i + 1;
    end
    else
    begin
      FParticles.Delete(i);
    end;
  end;
end;

destructor TAdParticleSystem.Destroy;
begin
  FImages.Free;
  FParticles.Free;
  FDefault.Free;
  inherited Destroy;
end;

procedure TAdParticleSystem.Draw(X, Y: double);
var i:integer;
begin
  for i := 0 to FParticles.Count - 1 do
  begin
    FParticles[i].DoPreDraw(X,Y);
  end;
  for i := 0 to FParticles.Count - 1 do
  begin
    FParticles[i].DoDraw(X,Y);
  end;
end;

function TAdParticleSystem.GetBoundsRect: TRect;
var i:integer;
begin
  for i := 0 to FParticles.Count - 1 do
  begin
    if (FParticles[i].GetBoundsRect.Left < result.Left) or (i = 0) then
      result.Left := FParticles[i].GetBoundsRect.Left;
    if (FParticles[i].GetBoundsRect.Right > result.Right) or (i = 0) then
      result.Right := FParticles[i].GetBoundsRect.Right;
    if (FParticles[i].GetBoundsRect.Bottom > result.Bottom) or (i = 0) then
      result.Bottom := FParticles[i].GetBoundsRect.Bottom;
    if (FParticles[i].GetBoundsRect.Top < result.Top) or (i = 0) then
      result.Top := FParticles[i].GetBoundsRect.Top;
  end;
end;

function TAdParticleSystem.GetImage(
  AColor: TAndorraColor): TPictureCollectionItem;
begin
  result := FImages.Find(AdColorToString(AColor));
end;

procedure TAdParticleSystem.Move(TimeGap: double);
var i:integer;
begin
  for i := 0 to FParticles.Count - 1 do
  begin
    FParticles[i].DoMove(TimeGap);
  end;
end;

procedure TAdParticleSystem.SetTexture(AValue: TAdTexture);
begin
  FImages.Clear;
  FTexture := AValue;
end;

{ TAdParticle }

constructor TAdParticle.Create(ASystem: TAdParticleSystem);
begin
  inherited Create;
  FSystem := ASystem;
  FColors := TAdColorList.Create;
  FColors.Add(Ad_ARGB(255,255,255,255));
  FColors.Add(Ad_ARGB(0,255,255,255));
  FLifeTime := 1;
  FLifeTimeVariation := 0;
  FDrawMask := true;
  FSizeStart := 1;
  FSizeEnd := 1;
  FRotStart := 0;
  FRotEnd := 0;
  FSpeedXStart := 100;
  FSpeedYStart := 100;
  FSpeedXEnd := 100;
  FSpeedYEnd := 100;
  FCrAngle := 0;
  FCrAngleOpen := 360;
  FForce.X := 0;
  FForce.Y := 0;
  FBlendMode := bmAdd;
end;

destructor TAdParticle.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TAdParticle.Assign(AParticle: TAdParticle);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  AParticle.SaveToStream(ms);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdParticle.Dead;
begin
  FDeaded := true;
end;

function MoveRect(ARect:TRect;X,Y:double):TRect;
begin
  result := Rect(Round(ARect.Left+X),Round(ARect.Top+Y),
                 Round(ARect.Right+X),Round(ARect.Bottom+Y));
end;

procedure TAdParticle.DoDraw(AX, AY: double);
var aimg:TPictureCollectionItem;
    arect:TRect;
    w,h:integer;
begin
  if (not FDeaded) and (BlendMode <> bmMask) then
  begin
    aimg := Image;
    if aimg <> nil then
    begin
      arect := MoveRect(GetBoundsRect,AX,AY);
      w := arect.Right-arect.Left;
      h := arect.Bottom-arect.Top;
      if BlendMode = bmAdd then
      begin
        aimg.DrawRotateAdd(FSystem.Parent,arect.Left,arect.Top,w,h,0,0.5,0.5,
                           round(GetValue(FRotStart,FRotEnd,LifeTime,FLifedTime)),
                           cut(fcolor.a));
      end;
      if BlendMode = bmAlpha then
      begin
        aimg.DrawRotateAlpha(FSystem.Parent,arect.Left,arect.Top,w,h,0,0.5,0.5,
                           round(GetValue(FRotStart,FRotEnd,LifeTime,FLifedTime)),
                           cut(fcolor.a));
      end;
    end;
  end;
end;

procedure TAdParticle.DoPreDraw(AX, AY: double);
var aimg:TPictureCollectionItem;
    arect:TRect;
    w,h:integer;
begin
  if (FDrawMask or (BlendMode = bmMask)) and not FDeaded then
  begin
    aimg := Image;
    if aimg <> nil then
    begin
      arect := MoveRect(GetBoundsRect,AX,AY);
      w := arect.Right-arect.Left;
      h := arect.Bottom-arect.Top;
      aimg.DrawRotateMask(FSystem.Parent,arect.Left,arect.Top,w,h,0,0.5,0.5,
                          round(GetValue(FRotStart,FRotEnd,LifeTime,FLifedTime)),
                          cut(fcolor.a));
    end;
  end;
end;


procedure TAdParticle.DoMove(TimeGap: double);
var sx,sy:double;
begin
  FLifedTime := FLifedTime + 1 * Timegap;
  if FLifedTime > FLifeTime then Dead;

  sx := GetValue(FSpeedXStart,FSpeedXEnd,LifeTime,FLifedTime) * Timegap;
  sy := GetValue(FSpeedYStart,FSpeedYEnd,LifeTime,FLifedTime) * Timegap;
  if FSpeedVariation = 0 then
  begin
    FX := FX+FDir.X * sx +(FForce.X*FLifedTime) * Timegap;
    FY := FY+FDir.Y * sy +(FForce.Y*FLifedTime) * Timegap;
  end
  else
  begin
    FX := FX+FDir.X * sx * FSpeedVar +(FForce.X*FLifedTime) * Timegap;
    FY := FY+FDir.Y * sy * FSpeedVar +(FForce.Y*FLifedTime) * Timegap;
  end;
end;

function TAdParticle.GetBoundsRect: TRect;
var s:double;
begin
  s := GetValue(SizeStart,SizeEnd,LifeTime,FLifedTime);
  result := Rect(round(FX-(Parent.Texture.Texture.BaseWidth)*s / 2),
                 round(FY-(Parent.Texture.Texture.BaseHeight)*s / 2),
                 round(FX+(Parent.Texture.Texture.BaseWidth)*s / 2),
                 round(FY+(Parent.Texture.Texture.BaseHeight)*s / 2));
end;

function TAdParticle.GetImage: TPictureCollectionItem;
var acolor:TAndorraColor;
begin
  acolor := FColors.GetColor(FLifeTime,FLifedTime);
  if not CompareColors(acolor,fcolor) then
  begin
    fcolor := acolor;
    result := FSystem.GetImage(acolor);
    if result = nil then
    begin
      FSystem.CreateImage(acolor);
      result := FSystem.GetImage(AColor);
    end;
    FLastImage := result;
  end
  else
  begin
    result := FLastImage;
  end;
end;

function TAdParticle.GetValue(StartPos, EndPos, Max, Pos: double): double;
begin
  result := ((EndPos-StartPos)/Max)* Pos + StartPos;
end;

procedure TAdParticle.LoadFromFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);
  ms.Free;
end;

procedure TAdParticle.LoadFromStream(AStream: TStream);
begin
  FColors.LoadFromStream(AStream);
  AStream.Read(FLifeTime,SizeOf(FLifeTime));
  AStream.Read(FLifeTimeVariation,SizeOf(FLifeTimeVariation));
  AStream.Read(FDrawMask,SizeOf(FDrawMask));
  AStream.Read(FSizeStart,SizeOf(FSizeStart));
  AStream.Read(FSizeEnd,SizeOf(FSizeEnd));
  AStream.Read(FRotStart,SizeOf(FRotStart));
  AStream.Read(FRotEnd,SizeOf(FRotEnd));
  AStream.Read(FSpeedXStart,SizeOf(FSpeedXStart));
  AStream.Read(FSpeedYStart,SizeOf(FSpeedYStart));
  AStream.Read(FSpeedXEnd,SizeOf(FSpeedXEnd));
  AStream.Read(FSpeedYEnd,SizeOf(FSpeedYEnd));
  AStream.Read(FCrAngle,SizeOf(FCrAngle));
  AStream.Read(FCrAngleOpen,SizeOf(FCrAngleOpen));
  AStream.Read(FForce,SizeOf(FForce));
  AStream.Read(FBlendMode,SizeOf(FBlendmode));
  AStream.Read(FSpeedVariation,SizeOf(FSpeedVariation));
  AStream.Read(FName,255);
end;

procedure TAdParticle.SaveToFile(AFile: string);
var ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdParticle.SaveToStream(AStream: TStream);
begin
  FColors.SaveToStream(AStream);
  AStream.Write(FLifeTime,SizeOf(FLifeTime));
  AStream.Write(FLifeTimeVariation,SizeOf(FLifeTimeVariation));
  AStream.Write(FDrawMask,SizeOf(FDrawMask));
  AStream.Write(FSizeStart,SizeOf(FSizeStart));
  AStream.Write(FSizeEnd,SizeOf(FSizeEnd));
  AStream.Write(FRotStart,SizeOf(FRotStart));
  AStream.Write(FRotEnd,SizeOf(FRotEnd));
  AStream.Write(FSpeedXStart,SizeOf(FSpeedXStart));
  AStream.Write(FSpeedYStart,SizeOf(FSpeedYStart));
  AStream.Write(FSpeedXEnd,SizeOf(FSpeedXEnd));
  AStream.Write(FSpeedYEnd,SizeOf(FSpeedYEnd));
  AStream.Write(FCrAngle,SizeOf(FCrAngle));
  AStream.Write(FCrAngleOpen,SizeOf(FCrAngleOpen));
  AStream.Write(FForce,SizeOf(FForce));
  AStream.Write(FBlendMode,SizeOf(FBlendmode));
  AStream.Write(FSpeedVariation,SizeOf(FSpeedVariation));
  AStream.Write(FName,255);
end;

procedure TAdParticle.SetupMovement;
var alpha,max,min : double;
begin
  with FDir do
  begin
    max := (CreationAngle+CreationAngleOpen / 2) * PI / 180;
    min := (CreationAngle-CreationAngleOpen / 2) * PI / 180;
    Alpha := Random * (max-min)+min;
    X := cos(Alpha);
    Y := sin(Alpha);
  end;
  if FLifeTimeVariation <> 0 then
  begin
    FLifeTime := FLifeTime + random(round(FLifeTime*FLifeTimeVariation))/100 - FLifeTime*FLifeTimeVariation/100;
  end;
  if FSpeedVariation > 100 then FSpeedVariation := 100;  
  FSpeedVar := ((100-random(FSpeedVariation))/100);
end;

end.
