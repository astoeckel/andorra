{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdShapes.pas
* Comment: Contains classes for pixel check
}

unit AdShapes;

interface
uses
  AdClasses, AdTypes, AdBitmap, SysUtils, Types, Graphics, Math, Classes;

type
  TAdBinaryMap = class
    private
      FField:PByte;
      FOrgWidth:integer;
      FWidth:integer; //Width of the Field in Pixel ("Width" in Byte = FWidth div 8)
      FHeight:integer; //Height of the Field in Pixel
      FSize:integer;
      function GetLoaded:boolean;
      function GetPixel(x,y:integer):boolean;
      procedure SetPixel(x,y:integer;value:boolean);
      function GetScanline:Pointer;
    public
      constructor Create;
      destructor Destroy;override;
      procedure AssignBitmap(ABmp:TBitmap;TransparentColor:TColor);
      procedure AssignAdBitmap(ABmp:TAdBitmap);
      procedure AssignToBitmap(ABmp:TBitmap);
      procedure Clear;

      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);

      procedure ReserveMemory(AWidth,AHeight:integer);

      procedure Fill(Value:Boolean);

      procedure Invert;

      function Collision(AMap:TAdBinaryMap; AX,AY:integer):boolean;

      property Pixels[x,y:integer]:boolean read GetPixel write SetPixel;
      property Width:integer read FWidth;
      property OrgWidth:integer read FOrgWidth;
      property Height:integer read FHeight;
      property Loaded:boolean read GetLoaded;
      property Scanline:Pointer read GetScanline;
      property Size:integer read FSize;
  end;

  TAdShape = class
    private
      FBlock:boolean;
      FWidth,FHeight:integer;
    protected
      property Block:boolean read FBlock write FBlock;
    public
      constructor Create(AWidth, AHeight: integer);

      property Width:integer read FWidth write FWidth;
      property Height:integer read FHeight write FHeight;

      function CollideWithPoint(AX,AY:integer):boolean;virtual;abstract;
      function CollideWithShape(AShape:TAdShape;ASelfPos,AShapePos:TPoint):boolean;virtual;abstract;
  end;

  TAdRectShape = class(TAdShape)
   private
    public
      function CollideWithPoint(AX, AY: integer): boolean;override;
      function CollideWithShape(AShape: TAdShape; ASelfPos,
         AShapePos: TPoint): boolean;override;
  end;

  TAdBitmapShape = class(TAdShape)
    private
      FMask:TAdBinaryMap;
    protected

    public
      constructor Create(AWidth, AHeight: integer);
      destructor Destroy;override;

      function CollideWithPoint(AX, AY: integer): boolean;override;
      function CollideWithShape(AShape: TAdShape; ASelfPos,
         AShapePos: TPoint): boolean;override;

      property Mask:TAdBinaryMap read FMask;
  end;

  EAdShapeError = class(Exception);

implementation

function OverlapRect(const Rect1, Rect2: TRect): Boolean;
begin
  Result:=(Rect1.Left<Rect2.Right)and
    (Rect1.Right>Rect2.Left)and
    (Rect1.Top<Rect2.Bottom)and
    (Rect1.Bottom>Rect2.Top);
end;

{ TAdBinaryMap }

constructor TAdBinaryMap.Create;
begin
  inherited;
  FWidth := 0;
  FHeight := 0;
  FField := nil;
end;

destructor TAdBinaryMap.Destroy;
begin
  Clear;
  inherited;
end;

procedure TAdBinaryMap.Fill(Value: Boolean);
var
  v:byte;
  p1:PByte;
  i:integer;
begin
  if Value then
    v := $FF
  else
    v := $00;

  p1 := Scanline;
  for i := 0 to FSize-1 do
  begin
    p1^ := v;
    inc(p1);
  end;    
end;

procedure TAdBinaryMap.ReserveMemory(AWidth, AHeight: integer);
begin
  Clear;
  FOrgWidth := AWidth;
  FWidth := Ceil(AWidth / 8) * 8;
  FHeight := AHeight;

  FSize := (FWidth div 8) * FHeight;

  GetMem(FField, FSize);
end;

procedure TAdBinaryMap.AssignToBitmap(ABmp: TBitmap);
var
  p1,p2:PByte;
  x,y:integer;

  function FlipByte(AByte:Byte):byte;
  var
    i:integer;
  begin
    result := 0;
    for i := 0 to 7 do
    begin
      result := result or (AByte and (1 shl i) shr i) shl (7-i);
    end;
  end;

begin
  if Loaded then
  begin
    ABmp.Width := FWidth;
    ABmp.Height := FHeight;
    ABmp.PixelFormat := pf1Bit;
    p1 := Scanline;
    for y := 0 to FHeight-1 do
    begin
      p2 := ABmp.ScanLine[y];
      for x := 0 to ((FWidth-1) div 8) do
      begin
        p2^ := not FlipByte(p1^);
        inc(p1);
        inc(p2);
      end;
    end;
  end;
end;

procedure TAdBinaryMap.Clear;
begin
  if Loaded then
  begin
    FreeMem(FField, FSize);
    FField := nil;
    FWidth := 0;
    FHeight := 0;
  end;
end;

type
  TRGB = packed record
    r,g,b:byte;
  end;

  PRGB = ^TRGB;

procedure TAdBinaryMap.AssignAdBitmap(ABmp: TAdBitmap);
var
  p1:PByte;
  p2:PRGBARec;
  a,v:byte;
  x,y,j:integer;
begin
  ReserveMemory(ABmp.Width, ABmp.Height);
  p1 := Scanline;
  p2 := ABmp.ScanLine;

  for y := 0 to ABmp.Height - 1 do
  begin
    a := 0;
    v := 255;
    for x := 0 to ABmp.Width - 1 do
    begin
      if (p2^.a = 0) then
      begin
        v := v and not (1 shl a);
      end;
      inc(p2);
      a := a + 1;
      if (a = 8) then
      begin
        p1^ := v;
        inc(p1);
        a := 0;
        v := 255;
      end;
    end;
    if a <> 0 then
    begin
      for j := 0 to (8-a)-1 do
        v := v and not (1 shl (a+j));
      p1^ := v;
      inc(p1);
    end;
  end;
end;

procedure TAdBinaryMap.AssignBitmap(ABmp: TBitmap; TransparentColor: TColor);
var
  p1:PByte;
  p2:PRGB;
  a,v,r,g,b:byte;
  x,y,j:integer;
begin
  ReserveMemory(ABmp.Width, ABmp.Height);
  p1 := Scanline;

  ABmp.PixelFormat := pf24Bit;

  r := GetRValue(TransparentColor);
  g := GetGValue(TransparentColor);
  b := GetBValue(TransparentColor);

  for y := 0 to ABmp.Height - 1 do
  begin
    p2 := ABmp.ScanLine[y];
    a := 0;
    v := 255;
    for x := 0 to ABmp.Width - 1 do
    begin
      if (p2^.r = r) and (p2^.g = g) and (p2.b = b) then
      begin
        v := v and not (1 shl a);
      end;
      inc(p2);
      a := a + 1;
      if (a = 8) then
      begin
        p1^ := v;
        inc(p1);
        a := 0;
        v := 255;
      end;
    end;
    if a <> 0 then
    begin
      for j := 0 to (8-a)-1 do
        v := v and not (1 shl (a+j));
      p1^ := v;
      inc(p1);
    end;
  end;
end;

function TAdBinaryMap.GetLoaded: boolean;
begin
  result := (FField <> nil);
end;

function TAdBinaryMap.GetPixel(x, y: integer): boolean;
var
  p1:PByte;
begin
  result := false;
  if (x > -1) and (x < FOrgWidth) and (y > -1) and (y < FHeight) then
  begin
    p1 := Scanline;
    inc(p1, y * (FWidth div 8) + (x div 8));
    result := p1^ and (1 shl (x mod 8)) <> 0;
  end;
end;

procedure TAdBinaryMap.SetPixel(x, y: integer; value: boolean);
var
  p1:PByte;
begin
  if (x > -1) and (x < FOrgWidth) and (y > -1) and (y < FHeight) then
  begin
    p1 := Scanline;
    inc(p1, y * (FWidth div 8) + (x div 8));

    if value then
      p1^ := p1^ or (1 shl (x mod 8))
    else
      p1^ := p1^ and not (1 shl (x mod 8));

  end;
end;

function TAdBinaryMap.Collision(AMap: TAdBinaryMap; AX, AY: integer): boolean;
var
  x,y:integer;
  p1:PByte;
  ow,oh:integer;
begin
  result := false;

  //Speed can be incrased....

  if AMap.OrgWidth > FOrgWidth then
    ow := AMap.OrgWidth
  else
    ow := FOrgWidth;

  if AMap.Height > FHeight then
    oh := AMap.Height
  else
    oh := FHeight;

  if (ax > -ow) and (ax < ow) and (ay > -oh) and (ay < oh) then
  begin
    p1 := Scanline;
    for y := 0 to FHeight - 1 do
    begin
      for x := 0 to FWidth - 1 do
      begin
        if ((p1^ and (1 shl (x mod 8)) <> 0) and AMap.Pixels[x+ax,y+ay]) then
        begin
          result := true;
          exit;
        end;
        if (x+1) mod 8 = 0 then
        begin
          inc(p1);
        end;
      end;
    end;
  end;
end;

function TAdBinaryMap.GetScanline: Pointer;
begin
  result := FField;
end;

procedure TAdBinaryMap.Invert;
var
  p1:PByte;
  i:integer;
begin
  p1 := Scanline;
  for i := 0 to FSize-1 do
  begin
    p1^ := not p1^;
    inc(p1);
  end;
end;

procedure TAdBinaryMap.LoadFromStream(AStream: TStream);
begin
  Clear;
  AStream.Read(FWidth,SizeOf(FWidth));
  AStream.Read(FHeight,SizeOf(FHeight));
  if (FWidth <> 0) and (FHeight <> 0) then
  begin
    ReserveMemory(FWidth,FHeight);
    AStream.Read(FField^,FSize)
  end;
end;

procedure TAdBinaryMap.SaveToStream(AStream: TStream);
begin
  AStream.Write(FWidth,SizeOf(FWidth));
  AStream.Write(FHeight,SizeOf(FHeight));
  if Loaded then
  begin
    AStream.Write(FField^,FSize)
  end;
end;

{ TAdRectShape }

function TAdRectShape.CollideWithPoint(AX, AY: integer): boolean;
begin
  result := (AX >= 0) and (AY >= 0) and (AX <= Width) and (AY <= Height);
end;

function TAdRectShape.CollideWithShape(AShape: TAdShape; ASelfPos,
  AShapePos: TPoint): boolean;
begin
  //To prevent endless loops
  if Block then
    raise EAdShapeError.Create('Bad collision handler (Collision with TAdRectShape and '+AShape.ClassName+')');

  if AShape is TAdRectShape then
  begin
    Result := OverlapRect(
                Bounds(ASelfPos.X,ASelfPos.Y,Width,Height),
                Bounds(AShapePos.X,AShapePos.Y,AShape.Width,AShape.Height));
  end
  else
  begin
    Block := true;
    try
      result := AShape.CollideWithShape(self, AShapePos, ASelfPos);
    finally
      Block := false;
    end;
  end;
end;

{ TAdBitmapShape }

constructor TAdBitmapShape.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth,AHeight);
  FMask := TAdBinaryMap.Create;
  FMask.ReserveMemory(AWidth,AHeight);
  FMask.Fill(true);
end;

destructor TAdBitmapShape.Destroy;
begin
  FMask.Free;
  inherited;
end;

function TAdBitmapShape.CollideWithPoint(AX, AY: integer): boolean;
begin
  result := FMask.Pixels[AX,AY];
end;

function TAdBitmapShape.CollideWithShape(AShape: TAdShape; ASelfPos,
  AShapePos: TPoint): boolean;
var
  Field:TAdBinaryMap;
begin
  //To prevent endless loops
  if Block then
    raise EAdShapeError.Create('Bad collision handler (Collision with TAdBitmapShape and '+AShape.ClassName+')');

  if AShape is TAdRectShape then
  begin
    Field := TAdBinaryMap.Create;
    Field.ReserveMemory(AShape.Width,AShape.Height);
    Field.Fill(true);
    result := FMask.Collision(Field, ASelfPos.X - AShapePos.X, ASelfPos.Y - AShapePos.Y);
    Field.Free;
  end
  else
    if AShape is TAdBitmapShape then
    begin
      result := FMask.Collision(TAdBitmapShape(AShape).Mask, ASelfPos.X - AShapePos.X, ASelfPos.Y - AShapePos.Y);
    end
    else
    begin
      Block := true;
      try
        result := AShape.CollideWithShape(self, AShapePos, ASelfPos);
      finally
        Block := false;
      end;    
    end;  
end;


{ TAdShape }

constructor TAdShape.Create(AWidth, AHeight: integer);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FBlock := false;
end;


end.

