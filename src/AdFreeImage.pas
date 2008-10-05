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
* File: AdFreeImage.pas
* Comment: Adds FreeImage support to Andorra 2D
}

{Adds FreeImage support to Andorra 2D}
unit AdFreeImage;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, AdBitmap, AdTypes, AdPngUtils, FreeImage;

type
  {@exclude}
  TAdFreeImagePNGCompressor = class(TAdGraphicCompressor)
    public
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
      class function ID:TAdVeryShortString;override;
  end;

  {@exclude}
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
  //List all file formats the can be read by FreeImage
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
  //Initialize some variables
  result := true;
  
  pdib1 := nil;
  pdib2 := nil;

  //Get the type of the file that should be loaded
  Format := FreeImage_GetFileType(PChar(AFile), 0);
  if Format = FIT_UNKNOWN then
    Format := FreeImage_GetFIFFromFilename(PChar(AFile));
    
  try
    //Load the image from the file and store the result in the pointer "pdib1"
    pdib1 := FreeImage_Load(Format, PChar(AFile));

    //Get the image type (whether it is a palette, bitmap or hdr image)
    typ := FreeImage_GetImageType(pdib1);

    if typ <> FIT_BITMAP then
    begin
      if typ < FIT_COMPLEX then
      begin
        //If the image is a palette image, convert it to a simple bitmap and store
        //the conversion result in "pdib2"
        pdib2 := FreeImage.FreeImage_ConvertToType(pdib1, FIT_BITMAP);
      end else
      begin
        //If the image is a hdr image, convert it to a simple bitmap using the
        //"Reinhard05" filter and store the conversion result in "pdib2"
        pdib2 := FreeImage.FreeImage_TmoReinhard05(pdib1, -5);
      end;
      
      //A conversion has been made, free the old, unconverted image
      FreeImage_UnLoad(pdib1); pdib1 := nil;
    end else
    begin
      //We didn't do a conversion, exchange the values of "pdib1" and "pdib2"
      pdib2 := pdib1; pdib1 := nil;
    end;

    if FreeImage.FreeImage_GetBPP(pdib2) <> 32 then
    begin
      //Convert the image to 32 Bit Depth and store the result in "pdib1"
      pdib1 := FreeImage_ConvertTo32Bits(pdib2);
      //Free the old "pdib2" resource
      FreeImage_UnLoad(pdib2); pdib2 := nil;
    end else
    begin
      //We didn't do a conversion, exchange the values of "pdib2" and "pdib1"
      pdib1 := pdib2; pdib2 := nil;
    end;

    //Get the sizes of the bitmap and copy the bitmap bottom up.
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
    //Free all unfinalized images
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
    //Extract the PNG image from the source stream
    ExtractPng(AStream, ms);
    ms.Position := 0;

    pdib2 := nil; pdib1 := nil; pstream := nil;

    try
      //Create a FreeImage stream
      pstream := FreeImage_OpenMemory(ms.Memory, ms.Size - ms.Position);
      
      //Determine the type of the image (should always be "PNG")
      Format := FreeImage_GetFileTypeFromMemory(pstream);

      //Load the image from the created stream
      pdib1 := FreeImage_LoadFromMemory(Format, pstream);

      //Get the bit depth of the image and convert it to 32 Bit if neccessary
      if FreeImage.FreeImage_GetBPP(pdib1) <> 32 then
      begin
        //Store the results in "pdib2"
        pdib2 := FreeImage_ConvertTo32Bits(pdib1);
        
        //Free the old FreeImage resource
        FreeImage_UnLoad(pdib1); pdib1 := nil;
      end else
      begin
        //We didn't change anything. Exchange the pointers in "pdib2" and "pdib1"
        pdib2 := pdib1; pdib1 := nil;
      end;

      //Get the size of the image and copy it bottom up
      w := FreeImage_GetWidth(pdib2);
      h := FreeImage_GetHeight(pdib2);

      ABitmap.ReserveMemory(w, h);

      for y := 0 to h-1 do
        Move(FreeImage_GetScanline(pdib2, (h-1)-y)^, ABitmap.Scanline(y)^, 4 * w);

    finally
      //Free all unfinalized resources
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
  //Initialize some variables
  pdib := nil; pstream := nil;
  
  try
    //Create a new free image bitmap
    pdib := FreeImage_Allocate(ABitmap.Width, ABitmap.Height, 32);

    //Copry the memory of the Andorra 2D bitmap into the FreeImage bitmap
    for y := 0 to ABitmap.Height - 1 do
      Move(ABitmap.Scanline((ABitmap.Height-1)-y)^, FreeImage_GetScanline(pdib, y)^, 4 * ABitmap.Width);

    //Create a new free image stream
    pstream := FreeImage_OpenMemory;
    
    //Save the bitmap to the stram
    FreeImage_SaveToMemory(FIF_PNG, pdib, pstream);
    //Get the pointer to the FreeImage memory stream and get its size
    FreeImage_AcquireMemory(pstream, mem, size);
    //Write the FreeImage stream data to our destination stream
    AStream.Write(mem^, size);
  finally
    //Free all unfinalized resources
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
