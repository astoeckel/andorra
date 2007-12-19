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
* File: AdBitmapClass.pas
* Comment: This unit contains the base TAdBitmap class, which is used by the plugin dlls
}

unit AdBitmapClass;

interface

uses
  AdTypes;

type   
  TAd2DBitmap = class
    private
      FLastX, FLastY:int64;
      FCurrentPixelPointer:PAndorraColor;
      function GetPixelMemory(X, Y:integer):PAndorraColor;
      function GetPixel(X, Y:integer):TAndorraColor;
      procedure SetPixel(X, Y:integer; Value:TAndorraColor);
    protected
      FMemory:PByte;
      FWidth:integer;
      FHeight:integer;
      FSize:int64;
      procedure ClearMemory;
    public
      constructor Create;
      destructor Destroy;override;

      procedure ReserveMemory(AWidth,AHeight:integer);

      procedure ClearAlphaChannel;

      function ScanLine(AY:integer):pointer;overload;
      function ScanLine:pointer;overload;
      function Loaded:boolean;

      property Width:integer read FWidth;
      property Height:integer read FHeight;
      property Size:int64 read FSize;
      property Pixels[X, Y:integer]:TAndorraColor read GetPixel write SetPixel;
  end;

implementation

{ TAd2dBitmap }

constructor TAd2dBitmap.Create;
begin
  inherited Create;
  FWidth := 0;
  FHeight := 0;
  FSize := 0;
  FMemory := nil;
  FCurrentPixelPointer := nil;
  FLastX := -1;
  FLastY := -1;
end;

destructor TAd2dBitmap.Destroy;
begin
  ClearMemory;
  inherited;
end;

procedure TAd2dBitmap.ClearAlphaChannel;
var
  i:integer;
  mem:PCardinal;
begin
  mem := ScanLine;
  for i := 0 to (FSize div 4)-1 do
  begin
    mem^ := mem^ or $FF000000;
    inc(mem);
  end;
end;

function TAd2dBitmap.Loaded: boolean;
begin
  result := (FMemory <> nil);
end;

procedure TAd2dBitmap.ReserveMemory(AWidth, AHeight: integer);
begin
  ClearMemory;
  FSize := AWidth*AHeight*4;
  FWidth := AWidth;
  FHeight := AHeight;
  GetMem(FMemory,FSize);
end;

procedure TAd2dBitmap.ClearMemory;
begin
  if Loaded then
  begin
    FreeMem(FMemory,FSize);
    FMemory := nil;
  end;
  FSize := 0;
  FWidth := 0;
  FHeight := 0;
end;

function TAd2dBitmap.ScanLine: Pointer;
begin
  if Loaded then
  begin
    result := FMemory;
  end
  else
  begin
    result := nil;
  end;
end;

function TAd2dBitmap.ScanLine(AY: integer): Pointer;
var ptr:pByte;
begin
  if ay < Height then
  begin
    ptr := Scanline;
    inc(ptr,AY*4*FWidth);
    result := ptr;
  end
  else
  begin
    result := Scanline;
  end;
end;

function TAd2dBitmap.GetPixelMemory(X, Y: integer): PAndorraColor;
var
  dif:integer;
begin
  result := nil;
  if (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height) then
  begin
    if (FCurrentPixelPointer = nil) or (FLastX = -1) or (FLastY = -1) then
    begin
      FCurrentPixelPointer := ScanLine;
      Inc(FCurrentPixelPointer, Y * Width + X);
    end
    else
    begin
      dif := (Y - FLastY) * Height + (X - FLastX);
      if dif > 0 then
        Inc(FCurrentPixelPointer, dif);
      if dif < 0 then
        Dec(FCurrentPixelPointer, -dif);
    end;
    FLastX := X;
    FLastY := Y;
    result := FCurrentPixelPointer;
  end;
end;

function TAd2dBitmap.GetPixel(X, Y: integer): TAndorraColor;
var
  p:PAndorraColor;
begin
  p := GetPixelMemory(X, Y);
  if p <> nil then
    result := p^
  else
    result := Ad_ARGB(0, 0, 0, 0);
end;

procedure TAd2dBitmap.SetPixel(X, Y: integer; Value: TAndorraColor);
var
  p:PAndorraColor;
begin
  p := GetPixelMemory(X, Y);
  if p <> nil then
    p^ := Value;
end;

end.
