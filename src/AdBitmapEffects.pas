{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdBitmapEffects.pas
* Comment: This unit contains effects which may be assigned to TAdBitmap
}

{This unit contains various bitmap effects, which may be assigned to a TAd2dBitmap.}
unit AdBitmapEffects;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdTypes, AdBitmapClass;

type
  {This is the abstract bitmap effect base class. @seealso(TAdBitmapBlur)}
  TAdBitmapEffect = class
    public
      {This procedure is used to assign the effect to the specified bitmap.}
      procedure AssignEffect(Dest:TAd2dBitmap);virtual;abstract;
  end;

  {A array used by TAdBitmap blur. @seealso(TAdBitmapBlur)}
  TAdBlurMatrix = array[-100..100] of Single;
  {Class which is able to blur a bitmap using a gausian blur.
   @seealso(TAdBitmapEffect)
   @seealso(TAd2dBitmap)
   @author(This class uses code written by Phantom1, see
    (http://www.delphipraxis.net/topic14072_5x5blur+bzw+antialiasing.html))}
  TAdBitmapBlur = class(TAdBitmapEffect)
    private
      FRadius: Single;
      function CalculateMatrix(var ARadius:Single):TAdBlurMatrix;
    public
      {Assign the blur effect to the destination bitmap specified by "Dest".}
      procedure AssignEffect(Dest:TAd2dBitmap);override;
      {Set the radius of the blur effect. Accepts values between 0 and 100.}
      property Radius:Single read FRadius write FRadius;
  end;

  {A simple filter that changes the luminance of a Andorra bitmap.}
  TAdLFilter = class(TAdBitmapEffect)
    private
      FLuminanceFactor: double;
    public
      {Creates an instance of TAdLfilter}
      constructor Create;
      
      {Applies the effect to a bitmap}
      procedure AssignEffect(Dest: TAd2dBitmap);override;
      {The factor the luminance of the picture should be multiplied with.}
      property LuminanceFactor: double read FLuminanceFactor write FLuminanceFactor;
  end;

  TAdAntialiasFilter = class(TAdBitmapEffect)
    private
      FStrength: double;
      FThreshold: integer;
    public
      constructor Create;

      {Applies the effect to a bitmap}
      procedure AssignEffect(Dest: TAd2dBitmap);override;

      {The value pixels are blended by}
      property Strength: double read FStrength write FStrength;
      {The minimum alpha difference threshold}
      property Threshold: integer read FThreshold write FThreshold;
  end;

  {Makes a specific color of the bitmap transparent.}
  TAdTransparencyFilter = class(TAdBitmapEffect)
    private
      FTransparent: boolean;
      FTransparentColor: LongInt;
    public
      {Creates an instance of TAdTransparencyFilter}
      constructor Create;

      {Assigns the transparency effect to the bitmap}
      procedure AssignEffect(Dest: TAd2dBitmap);override;

      {Specifies whether the image should be made transparent. If false, the
       alpha channel will be removed.}
      property Transparent: boolean read FTransparent write FTransparent;
      {Specifies the color that should be made transparent. If this value
       is "clNone" ($1FFFFFFF) and transparent is true, nothing will be changed.}
      property TransparentColor: LongInt read FTransparentColor write FTransparentColor;      
  end;


implementation

{ TAdBitmapBlur }

type
  TFloatRGBA = packed record
    r,g,b,a:single;
  end;
  PFloatRGBA = ^TFloatRGBA;


function TAdBitmapBlur.CalculateMatrix(var ARadius: Single):TAdBlurMatrix;
var
  x: integer;
  Divisor: Single;
  MatrixRadius:byte;
begin
  if ARadius <= 0 then 
    ARadius := 1 
  else if ARadius > 99 then ARadius := 99;

  ARadius:=ARadius+1;
  MatrixRadius:=Trunc(ARadius);
  if Frac(ARadius)=0 then Dec(MatrixRadius);
  Divisor:=0;

  for x:=-MatrixRadius To MatrixRadius do
  begin
    result[x]:=ARadius-abs(x);
    Divisor:=Divisor+result[x];
  end;

  for x:=-MatrixRadius to MatrixRadius do
    result[x]:=result[x]/Divisor;

  ARadius := MatrixRadius;
end;

{Copied from a blur procedure written by Phantom1 (http://www.delphipraxis.net/topic14072_5x5blur+bzw+antialiasing.html)}
procedure TAdBitmapBlur.AssignEffect(Dest: TAd2dBitmap);
var
  Matrix:TAdBlurMatrix;
  MatrixRadius:Single;
  BmpSL: PRGBAArray;
  BmpRGB: PRGBARec;
  BmpCopy: array of array of TFloatRGBA;
  BmpCopyRGBs: PFloatRGBA;
  PixelRGBs: TFloatRGBA;
  BmpWidth, BmpHeight: Integer;
  x, y, mx: Integer;
  
begin  
  MatrixRadius := FRadius;
  Matrix := CalculateMatrix(MatrixRadius);
  
  BmpWidth := Dest.Width;
  BmpHeight := Dest.Height;

  SetLength(BmpCopy, BmpHeight, BmpWidth);
  // Alle Bildpunkte ins BmpCopy-Array schreiben und gleichzeitig HORIZONTAL blurren
  for y := 0 To Pred(BmpHeight) do
  begin
    BmpSL := Dest.ScanLine(y);
    BmpCopyRGBs := @BmpCopy[y,0];
    for x:=0 to Pred(BmpWidth) do
    begin
      FillChar(BmpCopyRGBs^, SizeOf(TFloatRGBA), 0);
      for mx := -trunc(MatrixRadius) to trunc(MatrixRadius) do
      begin
        if x + mx <= 0 then
          BmpRGB := @BmpSL^[0]  // erster Pixel
        else if x + mx >= BmpWidth then
          BmpRGB := @BmpSL^[Pred(BmpWidth)] // letzter Pixel
        else
          BmpRGB := @BmpSL^[x+mx];
        BmpCopyRGBs^.b := BmpCopyRGBs^.b+BmpRGB^.b*Matrix[mx];
        BmpCopyRGBs^.g := BmpCopyRGBs^.g+BmpRGB^.g*Matrix[mx];
        BmpCopyRGBs^.r := BmpCopyRGBs^.r+BmpRGB^.r*Matrix[mx];
        BmpCopyRGBs^.a := BmpCopyRGBs^.a+BmpRGB^.a*Matrix[mx];        
      end;
      Inc(BmpCopyRGBs);       
    end;
  end;

  // Alle Bildpunkte zurück ins Bmp-Bitmap schreiben und gleichzeitig VERTIKAL blurren
  for y := 0 to Pred(BmpHeight) do
  begin
    BmpRGB := Dest.ScanLine(y);
    for x := 0 to Pred(BmpWidth) do
    begin
      FillChar(PixelRGBs, SizeOf(TFloatRGBA), 0);
      for mx := -trunc(MatrixRadius) to trunc(MatrixRadius) do
      begin
        if y + mx <= 0 then
          BmpCopyRGBs := @BmpCopy[0, x]  // erster Pixel
        else if y + mx >= BmpHeight then
          BmpCopyRGBs := @BmpCopy[Pred(BmpHeight), x]  // letzter Pixel
        else
          BmpCopyRGBs := @BmpCopy[y + mx, x];
        PixelRGBs.b:=PixelRGBs.b+BmpCopyRGBs^.b*Matrix[mx];
        PixelRGBs.g:=PixelRGBs.g+BmpCopyRGBs^.g*Matrix[mx];
        PixelRGBs.r:=PixelRGBs.r+BmpCopyRGBs^.r*Matrix[mx];
        PixelRGBs.a:=PixelRGBs.a+BmpCopyRGBs^.a*Matrix[mx];        
      end;
      BmpRGB^.b := Round(PixelRGBs.b);
      BmpRGB^.g := Round(PixelRGBs.g);
      BmpRGB^.r := Round(PixelRGBs.r);
      BmpRGB^.a := Round(PixelRGBs.a);      
      Inc(BmpRGB);
    end;
  end;
end;

{ TAdLFilter }

procedure TAdLFilter.AssignEffect(Dest: TAd2dBitmap);
var
  x, y: integer;
  pixelptr: PRGBARec;
begin
  pixelptr := Dest.ScanLine;

  for y := 0 to Dest.Height - 1 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      pixelptr^.r := Cut(Round(pixelptr^.r * FLuminanceFactor));
      pixelptr^.g := Cut(Round(pixelptr^.g * FLuminanceFactor));
      pixelptr^.b := Cut(Round(pixelptr^.b * FLuminanceFactor));

      Inc(PixelPtr);
    end;
  end;
end;

constructor TAdLFilter.Create;
begin
  inherited;

  FLuminanceFactor := 1;
end;

{ TAdTransparencyFilter }

constructor TAdTransparencyFilter.Create;
begin
  inherited;

  FTransparent := true;
  FTransparentColor := $1FFFFFFF;
end;

procedure TAdTransparencyFilter.AssignEffect(Dest: TAd2dBitmap);
var
  i: integer;
  pixelptr: PRGBARec;
begin
  //The transparent color is "clNone", change nothing
  if FTransparent then
  begin
    if not (FTransparent and (FTransparentColor = aclNone)) then
    begin
      pixelptr := Dest.ScanLine;
      for i := 0 to (Dest.Size div 4) - 1 do
      begin
        if FTransparent and (FTransparentColor = RGB(pixelptr^.r, pixelptr^.g, pixelptr^.b)) then
          pixelptr^.a := 0
        else
          pixelptr^.a := 255;

        inc(PixelPtr);
      end;
    end;
  end else
    Dest.ClearAlphaChannel;
end;

{ TAdAntialiasFilter }

procedure TAdAntialiasFilter.AssignEffect(Dest: TAd2dBitmap);
var
  x, y: integer;
  pc1, pc2: TAndorraColor;
  src: TAd2DBitmap;
begin
  src := TAd2DBitmap.Create;
  src.ReserveMemory(Dest.Width, Dest.Height);
  Move(Dest.Scanline^, src.Scanline^, Dest.Size);

  //Blend vertically
  for y := 1 to Dest.Height - 2 do
  begin
    for x := 0 to Dest.Width - 1 do
    begin
      pc1 := src.Pixels[x, y];
      pc2 := src.Pixels[x, y-1];

      if pc1.a - pc2.a > FThreshold then
      begin
        pc1.a := round(pc1.a * (1 - FStrength));
        Dest.Pixels[x, y] := pc1;
      end;

      pc2 := src.Pixels[x, y+1];
      if pc1.a - pc2.a > FThreshold then
      begin
        pc1.a := round(pc1.a * (1 - FStrength));
        Dest.Pixels[x, y] := pc1;
      end;
    end;
  end;

  //Blend horizontally
  for y := 0 to Dest.Height - 1 do
  begin
    for x := 1 to Dest.Width - 2 do
    begin
      pc1 := src.Pixels[x, y];
      pc2 := src.Pixels[x-1, y];

      if pc1.a - pc2.a > FThreshold then
      begin
        pc1.a := round(pc1.a * (1 - FStrength));
        Dest.Pixels[x, y] := pc1;
      end;

      pc2 := src.Pixels[x+1, y];
      if pc1.a - pc2.a > FThreshold then
      begin
        pc1.a := round(pc1.a * (1 - FStrength));
        Dest.Pixels[x, y] := pc1;
      end;
    end;
  end;

  src.Free;
end;

constructor TAdAntialiasFilter.Create;
begin
  inherited;

  FStrength := 0.5;
  FThreshold := 64;
end;

end.
