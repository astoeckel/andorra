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

{This unit contains the main bitmap class, which is used to transfer bitmap data between the host and the plugin dlls.}
unit AdBitmapClass;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdTypes;

type   
  {TAd2dBitmap is the base bitmap class, which is used to transfer bitmap data
   between the host and the plugin dlls. TAd2dBitmap is only depended to
   AdTypes.pas. It does not contain any read/write functions for streams or
   files. You can use the scanline or the fast pixel property to manipulate the
   picture.}
  TAd2DBitmap = class
    private
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
      {Creates a instance of TAd2dBitmap and initializes all used variables.}
      constructor Create;
      {Destroys the instance of TAd2dBitmap and frees used memory.}
      destructor Destroy;override;

      {Use this procedure to reserve a specific amount of memory. This is
       important when loading bitmap data from a plugin dll via
       TAd2dTexture.SaveToBitmap. }
      procedure ReserveMemory(AWidth,AHeight:integer);

      {Removes the loaded bitmap data from the memory. After calling this
       procedure the old content of the bitmap is no more existent. The memory
       occupied by the data can now be used for other purposes. Remember that
       all pointers on the bitmap data (from scanline) are invalid now.}
      procedure Clear;

      {This procedure resets the alpha channel of the bitmap to 255.}
      procedure ClearAlphaChannel;

      {Returns the pointer to the beginning of a pixel line.}
      function ScanLine(AY:integer):pointer;overload;
      {Returns the pointer to the first pixel. All pixels are written from
      top-left to bottom-right. The pixels are stored a TRGBARec.}
      function ScanLine:pointer;overload;
      {Returns wether memory for the bitmap is reserved.}
      function Loaded:boolean;

      {Returns the width of the bitmap in pixels.}
      property Width:integer read FWidth;
      {Returns the height of the bitmap in pixels.}
      property Height:integer read FHeight;
      {Returns the size of the bitmap in bytes.}
      property Size:int64 read FSize;
      {Use the pixels property to gain easy access to each pixel of the bitmap.}
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
end;

destructor TAd2dBitmap.Destroy;
begin
  ClearMemory;
  inherited;
end;

procedure TAd2DBitmap.Clear;
begin
  ClearMemory;
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
  if (FMemory = nil) or (AWidth <> FWidth) or (AHeight <> FHeight) then
  begin
    ClearMemory;
    FSize := AWidth*AHeight*4;
    FWidth := AWidth;
    FHeight := AHeight;
    GetMem(FMemory,FSize);
  end;
end;

procedure TAd2dBitmap.ClearMemory;
begin
  //Free loaded memory...
  if Loaded then
  begin
    FreeMem(FMemory, FSize);
    FMemory := nil;
  end;
  
  //Reset all properties of the bitmaps
  FSize := 0;
  FWidth := 0;
  FHeight := 0;
end;

function TAd2dBitmap.ScanLine: Pointer;
begin
  result := FMemory;
end;

function TAd2dBitmap.ScanLine(AY: integer): Pointer;
var
  ptr:PByte;
begin
  if ay < Height then
  begin
    ptr := FMemory;
    inc(ptr, AY*4*FWidth);
    result := ptr;
  end
  else
  begin
    result := Scanline;
  end;
end;

function TAd2dBitmap.GetPixelMemory(X, Y: integer): PAndorraColor;
begin
  result := nil;
  if (X >= 0) and (Y >= 0) and (X < Width) and (Y < Height) then
  begin
    result := ScanLine;
    Inc(result, Y * Width + X);
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
