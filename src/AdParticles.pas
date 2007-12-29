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
* File: AdParticles.pas
* Comment: This unit contains the particle engine
}

{ Contains the particle system engine }

unit AdParticles;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses SysUtils, AdTypes, Classes, AdDraws, AdClasses, AdList;

type

  //The main particle class
  TAdParticle = class;

  //A list managing the particles. Deleted Particles are automaticly freed.
  TAdParticleList = class(TAdList)
    private
    	function GetItem(AIndex:integer):TAdParticle;
     	procedure SetItem(AIndex:integer;AItem:TAdParticle);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);override;
    public
      //The "items" property of a list.
  	  property Items[AIndex:integer]:TAdParticle read GetItem write SetItem;default;
      //Add a particle
      procedure Add(AItem:TAdParticle);
      //Returns a particle
      function Find(AName:string):TAdParticle;
  end;

  //A Vector
  TAdVector = record
    X:double;//< The X value
    Y:double;//< The Y value
  end;

  //A list containing TAndorraColors and returning
  TAdColorList = class(TAdList)
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

  //Manages all particles in a system.
  TAdParticleSystem = class
    private
      FTexture:TAdTexture;
      FImages:TAdImageList;
      FDraw:TAdDraw;
      FParticles:TAdParticleList;
      FDefault:TAdParticle;
      procedure SetTexture(AValue:TAdTexture);
      function GetBoundsRect:TAdRect;
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

      //Moves all assigned particles
      procedure Move(TimeGap:double);     
      //Kills all particles which want to be freed
      procedure Dead;
      //Creates a new image in the systems imagelist with a specific color.
      procedure CreateImage(AColor:TAndorraColor);
      //Returns an image with a specific color or "nil" if the image isn't found.
      function GetImage(AColor:TAndorraColor):TAdImage;
      //Specifies the texture of all particles
      property Texture:TAdTexture read FTexture write SetTexture;
      //The parent sourface
      property Parent:TAdDraw read FDraw write FDraw;
      //All particles in the system
      property Items:TAdParticleList read FParticles;
      //A link to the system's image list.
      property Images:TAdImageList read FImages;
      //The relative rect of all particles
      property BoundsRect:TAdRect read GetBoundsRect;
      //A particle which settings are automaticly copied when creating new particles
      property DefaultParticle:TAdParticle read FDefault;
  end;

  //One particle
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
      FLastImage:TAdImage;
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
      function GetBoundsRect:TAdRect;
      function GetValue(StartPos,EndPos,Max,Pos:double):double;
    protected
      function GetImage:TAdImage;virtual;
    public
      //Creates an instance of the particle
      constructor Create(ASystem:TAdParticleSystem);
      //Destroys the instance of this particle
      destructor Destroy;override;
      
      //Called by the particle system, contains all steps to move a particle
      procedure DoMove(TimeGap:double);virtual;
      //Called by the particle system, draws the particle
      procedure DoDraw(AX,AY:double);virtual;
      //Called by the particle system, is called before DoDraw is called. Used to draw a background mask.
      procedure DoPreDraw(AX,AY:double);virtual;
      
      //Signs the system that this particle has to be freed.
      procedure Dead;virtual;
      
      //Copies the settings of another particle to this particle.
      procedure Assign(AParticle:TAdParticle);
      
      //Called by the particle system, setups the movement (the direction) of the particle.
      procedure SetupMovement;virtual;
      
      //Saves the particle's settings to a stream
      procedure SaveToStream(AStream:TStream);
      //Loads the particle's settings from a stream
      procedure LoadFromStream(AStream:TStream);
      //Saves the particle settings to a file
      procedure SaveToFile(AFile:string);
      //Loads the particle settings from a file
      procedure LoadFromFile(AFile:string);
      
      //Specifies whether the backgroundmask should be drawn by the DoPreDraw function.
      property DrawMask:boolean read FDrawMask write FDrawMask;
      //The image of the particle
      property Image:TAdImage read GetImage;
      //The parent particle system.
      property Parent:TAdParticleSystem read FSystem;
      //Specifies whether the particle wants to be killed.
      property Deaded:boolean read FDeaded;
      //The direction vector of the particle
      property Dir:TAdVector read FDir write FDir;
      //The relative rectangle of the particle system
      property BoundsRect:TAdRect read GetBoundsRect;
      //The lifetime of a particle
      property LifeTime:double read FLifeTime write FLifeTime;
      //The variation of the lifetime in percent.
      property LifeTimeVariation:integer read FLifeTimeVariation write FLifeTimeVariation;
      //The colors the particle has.
      property Colors:TAdColorList read FColors write FColors;
      //The start size of a particle in %/100.
      property SizeStart:double read FSizeStart write FSizeStart;
      //The end size of a particle in %/100.
      property SizeEnd:double read FSizeEnd write FSizeEnd;
      //The start rotation angle of the particle in degrees.
      property RotStart:double read FRotStart write FRotStart;
      //The end rotation angle of the particle in degrees.
      property RotEnd:double read FRotEnd write FRotEnd;
      //The start x-speed of the particle
      property SpeedXStart:double read FSpeedXStart write FSpeedXStart;
      //The start y-speed of the particle
      property SpeedYStart:double read FSpeedYStart write FSpeedYStart;
      //The end x-speed of the particle
      property SpeedXEnd:double read FSpeedXEnd write FSpeedXEnd;
      //The end y-speed of the particle
      property SpeedYEnd:double read FSpeedYEnd write FSpeedYEnd;
      //The speed's varition in percent.
      property SpeedVariation:integer read FSpeedVariation write FSpeedVariation;
      //The angle the particle can fly in in degrees from 0-360
      property CreationAngle:integer read FCrAngle write FCrAngle;
      //The angle defining the space the particle can fly in
      property CreationAngleOpen:integer read FCrAngleOpen write FCrAngleOpen;
      //A vector of a force which influences the movement of the particle
      property Force:TAdVector read FForce write FForce;
      //The blendmode the particle is drawn in
      property BlendMode:TAd2DBlendMode read FBlendMode write FBlendmode;
      //The name of the particle.
      property Name:ShortString read FName write FName;
  end;

implementation

{ TAdParticleList }

procedure TAdParticleList.Add(AItem: TAdParticle);
begin
  inherited Add(AItem);
end;

function TAdParticleList.Find(AName: string): TAdParticle;
var i:integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Name = AName then
    begin
      result := Items[i];
      break;
    end;
  end;
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
  PAndorraColor(inherited Items[AIndex])^ := AItem;
end;


{ TAdParticleSystem }

constructor TAdParticleSystem.Create(ADraw: TAdDraw);
begin
  inherited Create;
  FDraw := ADraw;
  FImages := TAdImageList.Create(FDraw);
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

function TAdParticleSystem.GetBoundsRect: TAdRect;
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

function TAdParticleSystem.GetImage(AColor: TAndorraColor): TAdImage;
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

function MoveRect(ARect:TAdRect;X,Y:double):TAdRect;
begin
  result := AdRect(Round(ARect.Left+X),Round(ARect.Top+Y),
                   Round(ARect.Right+X),Round(ARect.Bottom+Y));
end;

procedure TAdParticle.DoDraw(AX, AY: double);
var aimg:TAdImage;
    arect:TAdRect;
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
var aimg:TAdImage;
    arect:TAdRect;
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

function TAdParticle.GetBoundsRect: TAdRect;
var s:double;
begin
  s := GetValue(SizeStart,SizeEnd,LifeTime,FLifedTime);
  result := AdRect(round(FX-(Parent.Texture.Texture.BaseWidth)*s / 2),
                   round(FY-(Parent.Texture.Texture.BaseHeight)*s / 2),
                   round(FX+(Parent.Texture.Texture.BaseWidth)*s / 2),
                   round(FY+(Parent.Texture.Texture.BaseHeight)*s / 2));
end;

function TAdParticle.GetImage: TAdImage;
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
