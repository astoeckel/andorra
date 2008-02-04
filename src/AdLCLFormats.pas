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
* Comment: Contains loaders for registered vcl formats
}

unit AdLCLFormats;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Interfaces, Graphics, LCLType, FPImage, IntfGraphics, Classes,
  AdBitmap, AdTypes, AdPersistent, GraphType;

type
  TAdLCLFormat = class(TAdGraphicFormat)
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

{ TAdLCLFormat }

class function TAdLCLFormat.SupportsObject(AObj: TObject): boolean;
var
  i:integer;
begin
  result := false;
  if (AObj) is TGraphic then
  begin
    //TAdLCLFormat should only be used as an fallback - if there is another
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

class procedure TAdLCLFormat.FileExts(strs: TStrings);
begin
  strs.Add('.bmp');
  strs.Add('.xpm');
  strs.Add('.png');
end;

function TAdLCLFormat.Assign(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
var
  bmp:TBitmap;
  x, y:integer;
  c1:TFPColor;
  p2:PRGBARec;
  tr, tg, tb:byte;
  
  SrcIntfImg: TLazIntfImage;
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
  
  SrcIntfImg := TLazIntfImage.Create(0,0);
  SrcIntfImg.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);

  
  ABitmap.ReserveMemory(bmp.Width, bmp.Height);

  tr := GetRValue(bmp.TransparentColor);
  tg := GetGValue(bmp.TransparentColor);
  tb := GetBValue(bmp.TransparentColor);

  p2 := ABitmap.ScanLine;
  for y := 0 to bmp.Height - 1 do
  begin
    for x := 0 to bmp.Width - 1 do
    begin
      c1 := SrcIntfImg.Colors[x,y];
      p2^.r := c1.Blue;
      p2^.g := c1.Green;
      p2^.b := c1.Red;
      if (bmp.Transparent and (p2^.r = tb) and (p2^.g = tg) and (p2^.b = tr))
         and (bmp.TransparentColor <> clNone) then
        p2^.a := 0
      else
        p2^.a := c1.Alpha;
      inc(p2);
    end;
  end;
  
  SrcIntfImg.Free;

  if not (AGraphic is TBitmap) then
    bmp.Free;
end;

function TAdLCLFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  bmp:TBitmap;
  x, y:integer;
  c1:TFPColor;
  p2:PRGBARec;
  
  SrcIntfImg:TLazIntfImage;
begin
  result := false;
  if ABitmap.Loaded then
  begin
    bmp := TBitmap.Create;
    bmp.Assign(TGraphic(AGraphic));
    
    SrcIntfImg := TLazIntfImage.Create(0, 0);
    SrcIntfImg.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);

    p2 := ABitmap.ScanLine;
    for y := 0 to bmp.Height - 1 do
    begin
      for x := 0 to bmp.Width - 1 do
      begin
        c1 := SrcIntfImg.Colors[x, y];
        p2^.a := (c1.Red + c1.Green + c1.Blue) div 3;
        inc(p2);
      end;
    end;
    result := true;

    bmp.Free;
    SrcIntfImg.Free;
  end;
end;

function TAdLCLFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  rawimg:TRawImage;
  tarinfimage:TLazIntfImage;
  hdl,mask:HBitmap;
  p1:PRGBARec;
  p2:PRGBARec;
  bmp:TBitmap;
  i:integer;
begin
  result := false;
  if not ABitmap.Loaded then exit;

  rawimg.Init;
  
  rawimg.Description.Width := ABitmap.Width;
  rawimg.Description.Height := ABitmap.Height;
  rawimg.Description.BitsPerPixel := 32;
  rawimg.Description.Depth := 32;
  rawimg.Description.Format := ricfRGBA;
  rawimg.Description.LineEnd:= rileTight;

  rawimg.Description.RedPrec := 8;
  rawimg.Description.RedShift := 16;

  rawimg.Description.GreenPrec := 8;
  rawimg.Description.GreenShift := 8;

  rawimg.Description.BluePrec := 8;
  rawimg.Description.BlueShift := 0;

  rawimg.Description.AlphaPrec := 8;
  rawimg.Description.AlphaShift := 24;
  
  rawimg.CreateData(false);
  p1 := PRGBARec(rawimg.Data);
  p2 := ABitmap.Scanline;
  
  for i := 0 to (ABitmap.Size div 4) - 1 do
  begin
    p1^.r := p2^.a;
    p1^.g := p2^.a;
    p1^.b := p2^.a;
    p1^.a := p2^.a;
    inc(p1); inc(p2);
  end;

  tarinfimage := TLazIntfImage.Create(ABitmap.Width, ABitmap.Height);
  tarinfimage.SetRAWImage(rawimg, false);

  tarinfimage.CreateBitmaps(hdl, mask);
  bmp := TBitmap.Create;
  bmp.Handle := hdl;
  bmp.MaskHandle := mask;
  TGraphic(AGraphic).Assign(bmp);
  bmp.Free;

  tarinfimage.Free;

  result := true;
end;

function TAdLCLFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
var
  rawimg:TRawImage;
  tarinfimage:TLazIntfImage;
  hdl,mask:HBitmap;
  bmp:TBitmap;
begin
  result := false;
  if not ABitmap.Loaded then exit;
  
  rawimg.Init;

  rawimg.Description.Width := ABitmap.Width;
  rawimg.Description.Height := ABitmap.Height;
  rawimg.Description.BitsPerPixel := 32;
  rawimg.Description.Depth := 32;
  rawimg.Description.Format := ricfRGBA;

  rawimg.Description.RedPrec := 8;
  rawimg.Description.RedShift := 16;

  rawimg.Description.GreenPrec := 8;
  rawimg.Description.GreenShift := 8;

  rawimg.Description.BluePrec := 8;
  rawimg.Description.BlueShift := 0;

  rawimg.Description.AlphaPrec := 8;
  rawimg.Description.AlphaShift := 24;
  
  rawimg.CreateData(false);
  Move(ABitmap.Scanline^, rawimg.Data^, abitmap.Size);

  tarinfimage := TLazIntfImage.Create(ABitmap.Width, ABitmap.Height);
  tarinfimage.SetRAWImage(rawimg, true);
  
  tarinfimage.CreateBitmaps(hdl, mask);
  bmp := TBitmap.Create;
  bmp.Handle := hdl;
  bmp.MaskHandle := mask;
  TGraphic(AGraphic).Assign(bmp);
  bmp.Free;

  tarinfimage.Free;

  result := true;
end;

function TAdLCLFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: LongInt): boolean;
var
  pict: TPicture;
  bmp: TBitmap;
  strs: TStringList;
  ext: string;
  i : integer;
begin
  result := false;
  
  //TAdLCLFormat should only be used as an fallback - if there is another
  //format registered it may handle the object better (e.g. use a given alphachannel)
  ext := Lowercase(ExtractFileExt(AFile));
  for i := 0 to RegisteredGraphicFormats.Count-1 do
  begin
    if RegisteredGraphicFormats[i] <> ClassName then
    begin
      strs := TStringList.Create;
      TAdGraphicFormatClass(AdGetClass(RegisteredGraphicFormats[i])).FileExts(strs);
      if strs.IndexOf(ext) > -1 then
      begin
        strs.Free;
        exit;
      end;
      strs.Free;
    end;
  end;

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
  RegisterGraphicFormat(TAdLCLFormat);

end.
