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
  {$IFDEF WIN32}Windows{$ELSE}Types{$ENDIF}, SysUtils, Graphics, Classes,
  AdBitmap, AdTypes, AdPersistent, AdMessages;

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

  {@exclude}
  TAdVCLBitmap = class(TGraphic)
    private
      FBitmap: TBitmap;
      FCompressor: TAdGraphicCompressorClass;
    protected
      procedure Changed(Sender: TObject); override;      
      procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
      function Equals(Graphic: TGraphic): Boolean; override;
      function GetEmpty: Boolean; override;
      function GetHeight: Integer; override;
      function GetPalette: HPALETTE; override;
      function GetTransparent: Boolean; override;
      function GetWidth: Integer; override;
      procedure SetHeight(Value: Integer); override;
      procedure SetWidth(Value: Integer); override;
    public
      constructor Create; override;
      destructor Destroy; override;

      procedure LoadFromFile(const Filename: string); override;
      procedure SaveToFile(const Filename: string); override;
      procedure LoadFromStream(Stream: TStream); override;
      procedure SaveToStream(Stream: TStream); override;
      procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
        APalette: HPALETTE); override;
      procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
        var APalette: HPALETTE); override;

      class procedure RegisterHandler;
      class procedure UnregisterHandler;
      class procedure FormatListChange(Sender: TObject);

      property Compressor: TAdGraphicCompressorClass
        read FCompressor write FCompressor;
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
  strs.Add('.ico');
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

{ TAdVCLBitmap }

procedure TAdVCLBitmap.Changed(Sender: TObject);
begin
  inherited;
  //
end;

constructor TAdVCLBitmap.Create;
begin
  inherited;

  FBitmap := TBitmap.Create;
  FBitmap.PixelFormat := pf32Bit;
  FCompressor := nil;
end;

destructor TAdVCLBitmap.Destroy;
begin
  FBitmap.Free;

  inherited;
end;

procedure TAdVCLBitmap.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
  ACanvas.StretchDraw(Rect, FBitmap);
end;

function TAdVCLBitmap.Equals(Graphic: TGraphic): Boolean;
begin
  result := false;
end;

function TAdVCLBitmap.GetEmpty: Boolean;
begin
  result := (FBitmap.Width = 0) and (FBitmap.Height = 0);
end;

function TAdVCLBitmap.GetHeight: Integer;
begin
  result := FBitmap.Height;
end;

function TAdVCLBitmap.GetWidth: Integer;
begin
  result := FBitmap.Width;
end;

function TAdVCLBitmap.GetPalette: HPALETTE;
begin
  result := 0;
end;

function TAdVCLBitmap.GetTransparent: Boolean;
begin
  result := true;
end;

procedure TAdVCLBitmap.LoadFromClipboardFormat(AFormat: Word; AData: THandle;
  APalette: HPALETTE);
var
  bmp: TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromClipboardFormat(AFormat, AData, APalette);
    bmp.PixelFormat := pf32Bit;
    FBitmap.Assign(bmp);
  finally
    bmp.Free;
  end;
end;

procedure TAdVCLBitmap.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  FBitmap.SaveToClipboardFormat(AFormat, AData, APalette);
end;

procedure TAdVCLBitmap.LoadFromFile(const Filename: string);
var
  abmp: TAdBitmap;
begin
  //Temproally unregister handlers to prevent infinite recursion
  UnregisterHandler;

  abmp := TAdBitmap.Create;
  try
    abmp.LoadGraphicFromFile(Filename, true, clNone);
    abmp.AssignTo(FBitmap);
  finally
    abmp.Free;
    RegisterHandler;
  end;
end;

procedure TAdVCLBitmap.LoadFromStream(Stream: TStream);
var
  abmp: TAdBitmap;
begin
  abmp := TAdBitmap.Create;
  try
    abmp.LoadFromStream(Stream);
    abmp.AssignTo(FBitmap);
  finally
    abmp.Free;
  end;
end;

procedure TAdVCLBitmap.SaveToFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenWrite or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAdVCLBitmap.SaveToStream(Stream: TStream);
var
  abmp: TAdBitmap;
begin
  abmp := TAdBitmap.Create;
  abmp.Assign(FBitmap);
  try
    abmp.Compressor := FCompressor;
    abmp.SaveToStream(Stream);
  finally
    abmp.Free;
  end;
end;

procedure TAdVCLBitmap.SetHeight(Value: Integer);
begin
  FBitmap.Height := Value;
end;

procedure TAdVCLBitmap.SetWidth(Value: Integer);
begin
  FBitmap.Width := Value;
end;

class procedure TAdVCLBitmap.RegisterHandler;
var
  i, j: integer;
  cls: TAdGraphicFormatClass;
  strs: TStringList;
  s: string;
begin
  strs := TStringList.Create;

  //Iterate through all registered graphic format classes
  for i := 0 to RegisteredGraphicFormats.Count - 1 do
  begin
    strs.Clear;

    //Get all available file extensions for this class
    cls := TAdGraphicFormatClass(
      AdGetClass(RegisteredGraphicFormats[i]));
    if cls <> nil then
    begin
      cls.FileExts(strs);

      for j := 0 to strs.Count - 1 do
      begin
        s := strs[j]; Delete(s, 1, 1);
        s := UpperCase(s);
        TPicture.RegisterFileFormat(s, s + ' file format', TAdVCLBitmap);
      end;
    end;
  end;

  strs.Free;
end;

class procedure TAdVCLBitmap.UnregisterHandler;
begin
  TPicture.UnregisterGraphicClass(TAdVCLBitmap);
end;

class procedure TAdVCLBitmap.FormatListChange(Sender: TObject);
begin
  //Refresh the handlers
  UnregisterHandler;
  RegisterHandler;
end;

initialization
  RegisterGraphicFormat(TAdVCLFormat);
  RegisteredGraphicFormats.OnChange := TAdVCLBitmap.FormatListChange;

end.
