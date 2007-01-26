{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdPNGCompressor.pas
* Comment: A PNG compressor and loader for Andorra 2D. It needs the PNG Delphi Sources which are available
           at http://pngdelphi.sourceforge.net/
}

{A PNG loader and compressor. It needs the PNG Delphi Sources which are available at http://pngdelphi.sourceforge.net/}
unit AdPNG;

interface

uses PngImage,AdDraws,AdClasses,Classes,Graphics;


type
  {A compressor to store textures in the png format}
  TPNGCompressor = class(TCompressor)
    public
      //Returns the initial letters of this compressor.
      function GetInitial:TInitialLetters;override;
      //Writes the two bitmaps into a stream
      procedure Write(AStream:TStream;ABmp:TAdBitmap);override;
      //Reads the two bitmaps from the stream and copies them into ABitmap and AAlphaChannel.
      procedure Read(AStream:TStream;ABmp:TAdBitmap);override;
  end;

  {A loader for PNG files and TPNGObject.}
  TPNGFormat = class(TPictFormat)
    public
      //Fills a list with its supported graphic extension.
      procedure FileExts(strs:TStringList);override;
      //Loads the graphic from a file and stros it in a TAdBitmap.
      function LoadFromFile(AFile:string;ABmp:TAdBitmap;Transparent:boolean;TransparentColor:TColor):boolean;override;
      //Assigns an TGraphic and  stores it in a TAdBitmap
      procedure AssignGraphic(AGraphic:TGraphic;ABmp:TAdBitmap);override;
      //Returns true if this format supports the graphicclass defined in AGraphicClass
      function SupportsGraphicClass(AGraphicClass:TGraphicClass):boolean;override;
end;

implementation

procedure GetAlpha(APNG:TPNGObject;ABMP:TAdBitmap);
var x,y:integer;
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

procedure GetRGB(APNG:TPNGObject;ABMP:TAdBitmap;SetAlpha:boolean=false);
var x,y:integer;
    sl1:PRGBRec;
    sl2:PRGBARec;
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

{ TPNGCompressor }

function TPNGCompressor.GetInitial: TInitialLetters;
begin
  result := #2+'PNG'
end;

procedure TPNGCompressor.Read(AStream: TStream; ABmp:TAdBitmap);
var
  PNG:TPNGObject;
begin
  PNG := TPNGObject.Create;
  PNG.LoadFromStream(AStream);
  ABmp.ReserveMemory(PNG.Width,PNG.Height);
  GetAlpha(PNG,ABmp);
  PNG.RemoveTransparency;
  GetRGB(PNG,ABmp);
  PNG.Free;
end;

procedure TPNGCompressor.Write(AStream: TStream; ABmp:TAdBitmap);
var
  PNG:TPNGObject;
  procedure AddAlpha(APNG:TPNGObject;ABMP:TAdBitmap);
  var x,y:integer;
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
  var Bmp:TBitmap;
  begin
    Bmp := TBitmap.Create;
    ABMP.AssignToBitmap(Bmp);
    APNG.Assign(Bmp);
    Bmp.Free;
  end;
   
begin
  PNG := TPNGObject.Create;
  AddRGB(PNG,ABmp);
  PNG.CreateAlpha;
  AddAlpha(PNG,ABmp);
  PNG.CompressionLevel := 9;
  PNG.SaveToStream(AStream);
  PNG.Free;
end;

{ TPNGFormat }

procedure TPNGFormat.AssignGraphic(AGraphic: TGraphic; ABmp: TAdBitmap);
var png:TPNGObject;
begin
  if AGraphic is TPNGObject then
  begin
    png := TPNGObject(AGraphic);
    ABmp.ReserveMemory(png.Width,png.Height);
    GetRGB(png,ABMP,true);
    if png.AlphaScanline[0] <> nil then
    begin
      GetAlpha(png,ABMP);
    end;
  end;
end;

procedure TPNGFormat.FileExts(strs: TStringList);
begin
  strs.Add('.png')
end;

function TPNGFormat.LoadFromFile(AFile: string; ABmp: TAdBitmap;
  Transparent: boolean; TransparentColor: TColor): boolean;
var png:TPNGObject;
    bmp:TBitmap;
begin
  png := TPNGObject.Create;
  png.LoadFromFile(AFile);
  if Transparent then
  begin
    if png.AlphaScanline[0] <> nil then
    begin
      ABmp.ReserveMemory(png.Width,png.Height);
      GetRGB(png,ABmp);
      GetAlpha(png,ABmp);
    end
    else
    begin
      bmp := TBitmap.Create;
      png.AssignTo(bmp);
      bmp.TransparentMode := tmFixed;
      bmp.Transparent := true;
      bmp.TransparentColor := TransparentColor;
      ABmp.AssignBitmap(bmp);
      bmp.Free;
    end;
  end
  else
  begin
    png.RemoveTransparency;
    ABmp.ReserveMemory(png.Width,png.Height);
    GetRGB(png,Abmp,true);
  end;
  png.Free;
end;

function TPNGFormat.SupportsGraphicClass(AGraphicClass: TGraphicClass): boolean;
begin
  result := AGraphicClass = TPNGObject;
end;

initialization
  RegisterCompressor(TPNGCompressor);
  RegisterFormat(TPNGFormat);

end.
