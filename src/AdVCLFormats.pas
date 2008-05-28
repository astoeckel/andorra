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
* Comment: Contains loaders for registered vcl graphic formats
}

{Contains loaders for registered vcl graphic formats}
unit AdVCLFormats;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Graphics, Classes, AdBitmap, AdTypes, AdPersistent;

type
 {@exclude}
 TAdVCLFormat = class(TAdGraphicFormat)
    public
      class procedure FileExts(strs:TStrings);override;
      class function SupportsObject(AObj:TObject):boolean;override;
      function LoadFromFile(ABitmap:TAdBitmap; AFile:string;
        ATransparent:Boolean; ATransparentColor:LongInt):boolean;override;
      function Assign(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignAlphaChannel(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
      function AssignAlphaChannelTo(ABitmap:TAdBitmap; AGraphic:TObject):boolean;override;
  end;

implementation

{ TAdVCLFormat }

class function TAdVCLFormat.SupportsObject(AObj: TObject): boolean;
var
  i:integer;
begin
  result := false;
  if (AObj) is TGraphic then
  begin
    //TAdVCLFormat should only be used as an fallback - if there is another
    //format registered it may handle the object better (e.g. use a given alphachannel)
    for i := 0 to RegisteredGraphicFormats.Count-1 do
    begin
      if RegisteredGraphicFormats[i] <> ClassName then
      begin
        if TAdGraphicFormatClass(AdGetClass(RegisteredGraphicFormats[i])).SupportsObject(AObj) then
        begin
          exit;
        end;
      end;
    end;
    result := true;
  end;
end;

class procedure TAdVCLFormat.FileExts(strs: TStrings);
begin
  strs.Add('.bmp');
  strs.Add('.dib');
end;

function TAdVCLFormat.Assign(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
var
  bmp:TBitmap;
  x, y:integer;
  p1:PRGBRec;
  p2:PRGBARec;
  tr, tg, tb:byte;
begin
  result := true;
  
  if not (AGraphic is TBitmap) then
  begin
    bmp := TBitmap.Create;
    bmp.Assign(TGraphic(AGraphic));
  end
  else
  begin
    bmp := TBitmap(AGraphic);
  end;
  ABitmap.ReserveMemory(bmp.Width, bmp.Height);

  tr := GetRValue(bmp.TransparentColor);
  tg := GetGValue(bmp.TransparentColor);
  tb := GetBValue(bmp.TransparentColor);

  bmp.PixelFormat := pf24Bit;

  p2 := ABitmap.ScanLine;
  for y := 0 to bmp.Height - 1 do
  begin
    p1 := bmp.ScanLine[y];
    for x := 0 to bmp.Width - 1 do
    begin
      p2^.r := p1^.r;
      p2^.g := p1^.g;
      p2^.b := p1^.b;
      if bmp.Transparent and (p1^.r = tb) and (p1^.g = tg) and (p1^.b = tr) then  //GBR(!)
        p2^.a := 0
      else
        p2^.a := 255;
      inc(p1);
      inc(p2);
    end;
  end;

  if not (AGraphic is TBitmap) then
    bmp.Free;
end;

function TAdVCLFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  bmp:TBitmap;
  x, y:integer;
  p1:PRGBRec;
  p2:PRGBARec;
begin
  result := false;
  if ABitmap.Loaded then
  begin

    bmp := TBitmap.Create;
    bmp.Assign(TGraphic(AGraphic));

    bmp.PixelFormat := pf24Bit;
    p2 := ABitmap.ScanLine;
    for y := 0 to bmp.Height - 1 do
    begin
      p1 := bmp.ScanLine[y];
      for x := 0 to bmp.Width - 1 do
      begin
        p2^.a := (p1^.r + p1^.g + p1^.b) div 3;
        inc(p1);
        inc(p2);
      end;
    end;
    result := true;

    bmp.Free;
  end;
end;

function TAdVCLFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  bmp:TBitmap;
  x, y:integer;
  p1:PRGBRec;
  p2:PRGBARec;
begin
  result := true;
  
  if not ABitmap.Loaded then
    exit;
    
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24Bit;
  bmp.Width := ABitmap.Width;
  bmp.Height := ABitmap.Height;

  p2 := ABitmap.ScanLine;
  for y := 0 to bmp.Height - 1 do
  begin
    p1 := bmp.ScanLine[y];
    for x := 0 to bmp.Width - 1 do
    begin
      p1^.r := p2^.a;
      p1^.g := p2^.a;
      p1^.b := p2^.a;
      inc(p1);
      inc(p2);
    end;
  end;

  TGraphic(AGraphic).Assign(bmp);
  bmp.Free;
end;

function TAdVCLFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  bmp:TBitmap;
  x, y:integer;
  p1:PRGBRec;
  p2:PRGBARec;
  a:single;
begin
  result := true;
  
  bmp := TBitmap.Create;
  bmp.PixelFormat := pf24Bit;
  bmp.Width := ABitmap.Width;
  bmp.Height := ABitmap.Height;

  p2 := ABitmap.ScanLine;
  for y := 0 to bmp.Height - 1 do
  begin
    p1 := bmp.ScanLine[y];
    for x := 0 to bmp.Width - 1 do
    begin
      a := p2^.a / 255;;
      p1^.r := round((p1^.r * (1-a)) + ((p2^.r) * a));
      p1^.g := round((p1^.g * (1-a)) + ((p2^.g) * a));
      p1^.b := round((p1^.b * (1-a)) + ((p2^.b) * a));
      inc(p1);
      inc(p2);
    end;
  end;            

  TGraphic(AGraphic).Assign(bmp);
  bmp.Free;
end;

function TAdVCLFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: LongInt): boolean;
var
  pict:TPicture;
  bmp:TBitmap;
begin
  result := true;
  
  pict := TPicture.Create;
  pict.LoadFromFile(AFile);
  bmp := TBitmap.Create;
  bmp.Assign(pict.Graphic);
  bmp.Transparent := ATransparent;
  bmp.TransparentColor := ATransparentColor;
  pict.Free;

  Assign(ABitmap, bmp);

  bmp.Free;
end;

initialization
  RegisterGraphicFormat(TAdVCLFormat);

end.
