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
* File: AdPNGCompressor.pas
* Comment: A PNG compressor and loader for Andorra 2D. It needs the PNG Delphi Sources which are available
           at http://pngdelphi.sourceforge.net/
}

{Contains the Andorra 2D binding to the PNGImage PNG loader and compressor. It
 needs the PNG Delphi Sources which are available at 
 http://pngdelphi.sourceforge.net/ . This unit doesn't run with lazarus! To have 
 PNG Support with Lazarus, try one of the other graphic library plugins like
 AdDevIL or AdFreeImage.
}
unit AdPNG;

interface

uses
  Graphics, Classes, AdBitmap, AdTypes, PngImage, AdVCLFormats;


type
  {A compressor to store textures in the png format}
  TAdPNGCompressor = class(TAdGraphicCompressor)
    public
      class function ID:TAdVeryShortString;override;
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
  end;

  {A loader for PNG files and TPNGObject.}
  TAdPNGFormat = class(TAdGraphicFormat)
    public
      class procedure FileExts(strs:TStrings);override;
      class function SupportsObject(AGraphic:TObject):boolean;override;
      function LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt):boolean;override;
      function Assign(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignAlphaChannel(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignAlphaChannelTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
end;

implementation

procedure GetAlpha(APNG:TPNGObject; ABMP:TAdBitmap);
var
  x,y:integer;
  sl1:PByteArray;
  sl2:PRGBARec;
begin
  sl2 := ABMP.ScanLine;
  for y := 0 to APNG.Height-1 do
  begin
    sl1 := APNG.AlphaScanline[y];
    for x := 0 to APNG.Width - 1 do
    begin
      sl2^.a := sl1[x];
      inc(sl2);
    end;
  end;
end;

procedure GetRGB(APNG:TPNGObject; ABMP:TAdBitmap; SetAlpha:boolean=false);
var
  x, y: integer;
  sl1: PRGBRec;
  sl2: PRGBARec;
begin
  sl2 := ABMP.ScanLine;
  for y := 0 to APNG.Height-1 do
  begin
    sl1 := APNG.Scanline[y];
    for x := 0 to APNG.Width - 1 do
    begin
      sl2^.r := sl1^.r;
      sl2^.g := sl1^.g;
      sl2^.b := sl1^.b;
      if SetAlpha then sl2^.a := 255;      
      inc(sl2);
      inc(sl1);
    end;
  end;
end;

procedure AddAlpha(APNG:TPNGObject;ABMP:TAdBitmap);
var
  x,y:integer;
  sl1:PByteArray;
  sl2:PRGBARec;
begin
  sl2 := ABMP.ScanLine;
  for y := 0 to APNG.Height-1 do
  begin
    sl1 := APNG.AlphaScanline[y];
    for x := 0 to APNG.Width - 1 do
    begin
      sl1[x] := sl2^.a;
      inc(sl2);
    end;
  end;
end;

procedure AddRGB(APNG:TPNGObject;ABMP:TAdBitmap);
var
  x,y:integer;
  sl1:PRGBRec;
  sl2:PRGBARec;
  bmp : TBitmap;
begin
  bmp := TBitmap.Create;
  bmp.Width := ABMP.Width;
  bmp.Height := ABMP.Height;
  bmp.PixelFormat := pf24Bit;
  
  sl2 := ABMP.ScanLine;
  for y := 0 to ABMP.Height - 1 do
  begin
    sl1 := bmp.Scanline[y];
    for x := 0 to ABMP.Width - 1 do
    begin
      sl1^.r := sl2^.r;
      sl1^.g := sl2^.g;
      sl1^.b := sl2^.b;
      inc(sl1); inc(sl2);
    end;
  end;

  APNG.Assign(bmp);

  bmp.Free;
end;


{ TPNGCompressor }

class function TAdPNGCompressor.ID: TAdVeryShortString;
begin
  result := #2+'PNG'
end;

procedure TAdPNGCompressor.Read(ABitmap:TAdBitmap; AStream:TStream);
var
  PNG:TPNGObject;
begin
  PNG := TPNGObject.Create;
  PNG.LoadFromStream(AStream);
  ABitmap.ReserveMemory(PNG.Width,PNG.Height);
  PNG.CreateAlpha;
  GetAlpha(PNG,ABitmap);
  PNG.RemoveTransparency;
  GetRGB(PNG,ABitmap);
  PNG.Free;
end;

procedure TAdPNGCompressor.Write(ABitmap:TAdBitmap; AStream:TStream);
var
  PNG:TPNGObject;
begin
  PNG := TPNGObject.Create;
  AddRGB(PNG,ABitmap);
  PNG.CreateAlpha;
  AddAlpha(PNG,ABitmap);
  PNG.CompressionLevel := 9;
  PNG.SaveToStream(AStream);
  PNG.Free;
end;

{ TPNGFormat }

class procedure TAdPNGFormat.FileExts(strs: TStrings);
begin
  strs.Add('.png')
end;

class function TAdPNGFormat.SupportsObject(AGraphic: TObject): boolean;
begin
  result := AGraphic is TPNGObject;
end; 

function TAdPNGFormat.Assign(ABitmap: TAdBitmap; AGraphic:TObject): boolean;
var
  png:TPNGObject;
begin
  result := false;
  if AGraphic is TPNGObject then
  begin
    png := TPNGObject(AGraphic);
    ABitmap.ReserveMemory(png.Width,png.Height);
    GetRGB(png,ABitmap,true);
    if png.AlphaScanline[0] <> nil then
    begin
      GetAlpha(png,ABitmap);
    end;
    result := true;
  end;
end;

function TAdPNGFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  png:TPNGObject;
begin
  result := false;
  if AGraphic is TPNGObject then
  begin
    png := TPNGObject(AGraphic);
    if png.AlphaScanline[0] <> nil then
    begin
      GetAlpha(png, ABitmap);
    end;
    result := true;
  end;
end;

function TAdPNGFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  png:TPNGObject;
begin
  result := false;
  if (AGraphic is TPNGObject) and (ABitmap.Loaded) then
  begin
    png := TPNGObject(AGraphic);
    png.CreateAlpha;
    AddAlpha(png, ABitmap);
    result := true;
  end;
end;

function TAdPNGFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  png:TPNGObject;
begin
  result := false;
  if (AGraphic is TPNGObject) and (ABitmap.Loaded) then
  begin
    png := TPNGObject(AGraphic);
    AddRGB(png, ABitmap);
    png.CreateAlpha;
    AddAlpha(png, ABitmap);
    result := true;
  end;
end;

function TAdPNGFormat.LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt): boolean;
var
  png:TPNGObject;
  bmp:TBitmap;
begin
  result := true;
  png := TPNGObject.Create;
  try
    png.LoadFromFile(AFile);
    if ATransparent then
    begin
      if png.AlphaScanline[0] <> nil then
      begin
        ABitmap.ReserveMemory(png.Width,png.Height);
        GetRGB(png,ABitmap);
        GetAlpha(png,ABitmap);
      end
      else
      begin
        bmp := TBitmap.Create;
        png.AssignTo(bmp);
        bmp.TransparentMode := tmFixed;
        bmp.Transparent := true;
        bmp.TransparentColor := ATransparentColor;
        ABitmap.Assign(bmp);
        bmp.Free;
      end;
    end
    else
    begin
      png.RemoveTransparency;
      bmp := TBitmap.Create;
      png.AssignTo(bmp);
      bmp.Transparent := false;
      ABitmap.Assign(bmp);
      bmp.Free;
    end;
  except
    result := false;
  end;
  png.Free;
end;

initialization
  RegisterGraphicCompressor(TAdPNGCompressor);
  RegisterGraphicFormat(TAdPNGFormat);

end.
