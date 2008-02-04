unit AdFreeImage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, AdBitmap, AdTypes, AdPngUtils, FreeImage;

type
  TAdFreeImagePNGCompressor = class(TAdGraphicCompressor)
    public
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
      class function ID:TAdVeryShortString;override;
  end;

  TAdFreeImageFormat = class(TAdGraphicFormat)
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

{ TAdFreeImageFormat }

class procedure TAdFreeImageFormat.FileExts(strs: TStrings);
begin
  strs.Add('.bmp');
  strs.Add('.cut');
  strs.Add('.dds');
  strs.Add('.exr');
  strs.Add('.g3');
  strs.Add('.gif');
  strs.Add('.hdr');
  strs.Add('.ico');
  strs.Add('.iff');
  strs.Add('.lbm');
  strs.Add('.j2k');
  strs.Add('.j2c');
  strs.Add('.jng');
  strs.Add('.jp2');
  strs.Add('.jpg');
  strs.Add('.jif');
  strs.Add('.jpeg');
  strs.Add('.jpe');
  strs.Add('.koa');
  strs.Add('.pbm');
  strs.Add('.pcd');
  strs.Add('.pcx');
  strs.Add('.pgm');
  strs.Add('.png');
  strs.Add('.ppm');
  strs.Add('.psd');
  strs.Add('.ras');
  strs.Add('.sgi');
  strs.Add('.tga');
  strs.Add('.targa');
  strs.Add('.tif');
  strs.Add('.tiff');
  strs.Add('.wap');
  strs.Add('.wbmp');
  strs.Add('.wbm');
  strs.Add('.xbm');
  strs.Add('.xpm');
end;

class function TAdFreeImageFormat.SupportsObject(AObj: TObject): boolean;
begin
  result := false;
end;

function TAdFreeImageFormat.Assign(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdFreeImageFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdFreeImageFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdFreeImageFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdFreeImageFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: Integer): boolean;
var
  Format : FREE_IMAGE_FORMAT;
  pdib1, pdib2 : PFIBitmap;
  w, h : integer;
  y : integer;
  mem : PByte;
  typ:FREE_IMAGE_TYPE;
begin
  result := false;

  pdib1 := nil;
  pdib2 := nil;

  Format := FreeImage_GetFileType(PChar(AFile), 0);
  if Format = FIT_UNKNOWN then
    Format := FreeImage_GetFIFFromFilename(PChar(AFile));
    
  try
    pdib1 := FreeImage_Load(Format, PChar(AFile));

    typ := FreeImage_GetImageType(pdib1);

    if typ <> FIT_BITMAP then
    begin
      if typ < FIT_COMPLEX then
      begin
        pdib2 := FreeImage.FreeImage_ConvertToType(pdib1, FIT_BITMAP);
      end else
      begin
        pdib2 := FreeImage.FreeImage_TmoReinhard05(pdib1, -5);
      end;
      FreeImage_UnLoad(pdib1); pdib1 := nil;
    end else
    begin
      pdib2 := pdib1; pdib1 := nil;
    end;

    if FreeImage.FreeImage_GetBPP(pdib2) <> 32 then
    begin
      pdib1 := FreeImage_ConvertTo32Bits(pdib2);
      FreeImage_UnLoad(pdib2); pdib2 := nil;
    end else
    begin
      pdib1 := pdib2; pdib2 := nil;
    end;

    w := FreeImage_GetWidth(pdib1);
    h := FreeImage_GetHeight(pdib1);

    ABitmap.ReserveMemory(w, h);

    mem := ABitmap.Scanline;
    for y := h - 1 downto 0 do
    begin
      Move(FreeImage_GetScanline(pdib1, y)^, mem^, 4 * w);
      Inc(mem, 4 * w);
    end;
    
  finally
    if pdib1 <> nil then FreeImage_UnLoad(pdib1);
    if pdib2 <> nil then FreeImage_UnLoad(pdib2);
  end;
end;

{ TAdFreeImagePNGCompressor }

class function TAdFreeImagePNGCompressor.ID: TAdVeryShortString;
begin
  result := #2+'PNG';
end;

procedure TAdFreeImagePNGCompressor.Read(ABitmap: TAdBitmap; AStream: TStream);
var
  ms: TMemoryStream;
  w, h : integer;
  y : integer;   
  pdib1, pdib2 : PFIBitmap;
  pstream : PFIMemory;
  Format : FREE_IMAGE_FORMAT;
begin
  ms := TMemoryStream.Create;
  try
    ExtractPng(AStream, ms);
    ms.Position := 0;

    pdib2 := nil; pdib1 := nil; pstream := nil;

    try
      pstream := FreeImage_OpenMemory(ms.Memory, ms.Size - ms.Position);
      Format := FreeImage_GetFileTypeFromMemory(pstream);

      pdib1 := FreeImage_LoadFromMemory(Format, pstream);

      if FreeImage.FreeImage_GetBPP(pdib1) <> 32 then
      begin
        pdib2 := FreeImage_ConvertTo32Bits(pdib1);
        FreeImage_UnLoad(pdib1); pdib1 := nil;
      end else
      begin
        pdib2 := pdib1; pdib1 := nil;
      end;

      w := FreeImage_GetWidth(pdib2);
      h := FreeImage_GetHeight(pdib2);

      ABitmap.ReserveMemory(w, h);

      for y := 0 to h-1 do
        Move(FreeImage_GetScanline(pdib2, (h-1)-y)^, ABitmap.Scanline(y)^, 4 * w);

    finally
      if pdib1 <> nil then FreeImage_Unload(pdib1);
      if pdib2 <> nil then FreeImage_Unload(pdib2);
      if pstream <> nil then FreeImage_CloseMemory(pstream);
    end;

  finally
    ms.Free;
  end;
end;

procedure TAdFreeImagePNGCompressor.Write(ABitmap: TAdBitmap; AStream: TStream);
var
  pdib : PFIBitmap;
  pstream : PFIMemory;
  mem : System.PByte;
  size : DWORD;
  y:integer;
begin
  pdib := nil; pstream := nil;
  try
    pdib := FreeImage_Allocate(ABitmap.Width, ABitmap.Height, 32);

    for y := 0 to ABitmap.Height - 1 do
      Move(ABitmap.Scanline((ABitmap.Height-1)-y)^, FreeImage_GetScanline(pdib, y)^, 4 * ABitmap.Width);

    pstream := FreeImage_OpenMemory;
    FreeImage_SaveToMemory(FIF_PNG, pdib, pstream);
    FreeImage_AcquireMemory(pstream, mem, size);
    AStream.Write(mem^, size);
  finally
    if pdib <> nil then FreeImage_Unload(pdib);
    if pstream <> nil then FreeImage_CloseMemory(pstream);
  end;
end;

initialization
  FreeImage_Initialise;

  RegisterGraphicFormat(TAdFreeImageFormat);
  RegisterGraphicCompressor(TAdFreeImagePNGCompressor);

finalization
  FreeImage_DeInitialise;

end.
