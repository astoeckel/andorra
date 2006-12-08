{
* This program is licensed under the GNU Lesser General Public License Version 2
* You should have recieved a copy of the license with this file.
* If not, see http://www.gnu.org/licenses/lgpl.html for more informations
*
* Project: Andorra 2D
* Authors:  Andreas Stoeckel
* File: AdParticles.pas
* Comment: This unit contains the particle engine
}

{ Contains the particle system engine }

unit AdParticles;

interface

uses SysUtils, Types, Classes,AdDraws,AndorraUtils;

type

  TAdParticle = class;

  TAdParticleList = class(TList)
    private
    	function GetItem(AIndex:integer):TAdParticle;
     	procedure SetItem(AIndex:integer;AItem:TAdParticle);
    protected
    public
  	  property Items[AIndex:integer]:TAdParticle read GetItem write SetItem;default;
  end;

  TAdVector = record
    X,Y:double;
  end;

  TAdColorList = class(TList)
    private
    	function GetItem(AIndex:integer):TAndorraColor;
    	procedure SetItem(AIndex:integer;AItem:TAndorraColor);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification);     
    public
    	property Items[AIndex:integer]:TAndorraColor read GetItem write SetItem;default;
      function GetColor(Max,Pos:double):TAndorraColor;
      procedure Add(AColor:TAndorraColor);
    published
  end;

  TAdParticleClass = class of TAdParticle;

  TAdParticleSystem = class
    private
      FForce:TAdVector;
      FTexture:TAdTexture;
      FImages:TPictureCollection;
      FDraw:TAdDraw;
      FParticles:TAdParticleList;
      FDefault:TAdParticle;
      procedure SetForce(AValue:TAdVector);
      procedure SetTexture(AValue:TAdTexture);
      function GetBoundsRect:TRect;
    protected
    public
      constructor Create(ADraw:TAdDraw);
      destructor Destroy;override;
      procedure CreateParticles(ACount:integer;AClass:TAdParticleClass;OffsetX,OffsetY:integer);virtual;
      procedure Draw(X,Y:double);
      procedure Move(TimeGap:double);
      procedure Dead;
      procedure CreateImage(AColor:TAndorraColor);
      function GetImage(AColor:TAndorraColor):TPictureCollectionItem;
      property Force:TAdVector read FForce write SetForce;
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
      FColors:TAdColorList;
      FDeaded:boolean;
      FSystem:TAdParticleSystem;
      FLastImage:TPictureCollectionItem;
      FDrawMask:boolean;
      FColor:TAndorraColor;
      function GetBoundsRect:TRect;
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
      property DrawMask:boolean read FDrawMask write FDrawMask;
      property Image:TPictureCollectionItem read GetImage;
      property Parent:TAdParticleSystem read FSystem;
      property Deaded:boolean read FDeaded;
      property Dir:TAdVector read FDir write FDir;
      property BoundsRect:TRect read GetBoundsRect;
      property LifeTime:double read FLifeTime write FLifeTime;
      property Colors:TAdColorList read FColors write FColors;
  end;

implementation

{ TAdParticleList }

function TAdParticleList.GetItem(AIndex:integer):TAdParticle;
begin
  result := inherited Items[AIndex];
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

procedure TAdColorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  If ( Action = lnDeleted ) Then
    Dispose(PAndorraColor(Ptr));
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

procedure TAdParticleSystem.SetForce(AValue: TAdVector);
begin
  //
end;

procedure TAdParticleSystem.SetTexture(AValue: TAdTexture);
begin
  FImages.Clear;
  FTexture := AValue;
end;

{ TAdParticle }

procedure TAdParticle.Assign(AParticle: TAdParticle);
var i:integer;
begin
  FColors.Clear;
  for i := 0 to AParticle.Colors.Count - 1 do
  begin
    FColors.Add(AParticle.Colors[i]);
  end;
  FLifeTime := AParticle.LifeTime;
  FDrawMask := AParticle.DrawMask;
end;

constructor TAdParticle.Create(ASystem: TAdParticleSystem);
begin
  inherited Create;
  FSystem := ASystem;
  FColors := TAdColorList.Create;
  FColors.Add(Ad_ARGB(255,255,255,255));
  FColors.Add(Ad_ARGB(0,255,255,255));
  FLifeTime := 1;
  FDrawMask := true;
end;

procedure TAdParticle.Dead;
begin
  FDeaded := true;
end;

destructor TAdParticle.Destroy;
begin
  FColors.Free;
  inherited;
end;

procedure TAdParticle.DoDraw(AX, AY: double);
var aimg:TPictureCollectionItem;
begin
  if not FDeaded then
  begin
    aimg := Image;
    if aimg <> nil then
    begin
      aimg.DrawAdd(FSystem.Parent,Rect(round(AX+FX-aimg.Width/2),round(AY+FY-aimg.Height/2),
                                       round(AX+FX+aimg.Width/2),round(AY+FY+aimg.Height/2)),0,cut(fcolor.a));
    end;
  end;
end;

procedure TAdParticle.DoMove(TimeGap: double);
begin
  FLifedTime := FLifedTime + 1 * Timegap;
  if FLifedTime > FLifeTime then Dead;

  FX := FX+FDir.X * Timegap;
  FY := FY+FDir.Y * Timegap;
end;

procedure TAdParticle.DoPreDraw(AX, AY: double);
var aimg:TPictureCollectionItem;
begin
  if FDrawMask and not FDeaded then
  begin
    aimg := Image;
    if aimg <> nil then
    begin
      aimg.DrawMask(FSystem.Parent,Rect(round(AX+FX-aimg.Width/2),round(AY+FY-aimg.Height/2),
                                        round(AX+FX+aimg.Width/2),round(AY+FY+aimg.Height/2)),0,cut(fcolor.a));
    end;
  end;
end;

function TAdParticle.GetBoundsRect: TRect;
begin
  result := Rect(round(FX-Parent.Texture.BaseRect.Right / 2),
                 round(FY-Parent.Texture.BaseRect.Bottom / 2),
                 round(FX+Parent.Texture.BaseRect.Right / 2),
                 round(FY+Parent.Texture.BaseRect.Bottom / 2));
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

procedure TAdParticle.SetupMovement;
var l:double;
begin
  with FDir do
  begin
    X := 180-random(360);
    Y := 180-random(360);
    l := Sqrt(Sqr(X)+Sqr(Y));
    X := X / l*100;
    Y := Y / l*100;
  end;
end;

end.
