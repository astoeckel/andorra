{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
*Author: Andreas Stöckel
*Filename: AdDevIL.pas
*Comment: Contains Image-Loader classes and a png compressor which use the "DevIL" library
}

{Contains Image-Loader classes and a PNG compressor that use the "DevIL" library.}
unit AdDevIL;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, AdBitmap, AdTypes, AdPNGUtils, DevIL;

type
  {@exclude}
  TAdDevILPNGCompressor = class(TAdGraphicCompressor)
    public
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
      class function ID:TAdVeryShortString;override;
  end;

  {@exclude}
  TAdDevILFormat = class(TAdGraphicFormat)
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

{ TAdDevILFormat }

class function TAdDevILFormat.SupportsObject(AObj: TObject): boolean;
begin
  result := false;
end;

class procedure TAdDevILFormat.FileExts(strs: TStrings);
begin
  //List all file formats that can be read by DevIL
  strs.Add('.bmp');
  strs.Add('.cut');
  strs.Add('.dds');
  strs.Add('.gif');
  strs.Add('.ico');
  strs.Add('.cur');
  strs.Add('.jpg');
  strs.Add('.jpe');
  strs.Add('.jpeg');
  strs.Add('.lbm');
  strs.Add('.mdl');
  strs.Add('.mng');
  strs.Add('.pcd');
  strs.Add('.pcx');
  strs.Add('.pic');
  strs.Add('.pix');
  strs.Add('.png');
  strs.Add('.pbm');
  strs.Add('.pgm');
  strs.Add('.ppm');
  strs.Add('.pnm');
  strs.Add('.psd');
  strs.Add('.pxr');
  strs.Add('.sgi');
  strs.Add('.bw');
  strs.Add('.rgb');
  strs.Add('.rgba');
  strs.Add('.tga');
  strs.Add('.tif');
  strs.Add('.tiff');
  strs.Add('.wal');
  strs.Add('.xpm');
end;

function TAdDevILFormat.Assign(ABitmap: TAdBitmap; AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdDevILFormat.AssignAlphaChannel(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdDevILFormat.AssignAlphaChannelTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdDevILFormat.AssignTo(ABitmap: TAdBitmap;
  AGraphic: TObject): boolean;
begin
  result := false;
end;

function TAdDevILFormat.LoadFromFile(ABitmap: TAdBitmap; AFile: string;
  ATransparent: Boolean; ATransparentColor: Integer): boolean;
var
  w, h : integer;
  imagename:Cardinal;
begin
  result := false;

  //Generate a DevIL image
  ilGenImages(1, @imagename);
  ilBindImage(imagename);
  
  //Set the image origin to the upper left corner.
  ilEnable(IL_ORIGIN_SET);
  ilOriginFunc(IL_ORIGIN_UPPER_LEFT);  

  //Convert palette images to the BGRA32 format
  ilEnable(IL_CONV_PAL);
  ilConvertPal(IL_PAL_BGRA32);
  
  //Load the image from the stream
  if Boolean(ilLoadImage(PChar(AFile))) then
  begin
    //Convert the image to BGRA
    ilConvertImage(IL_BGRA, IL_UNSIGNED_BYTE);

    //And copy the decoded memory into an Andorra 2D bitmap
    w := ilGetInteger(IL_IMAGE_WIDTH);
    h := ilGetInteger(IL_IMAGE_HEIGHT);

    ABitmap.ReserveMemory(w, h);
    ilCopyPixels(0, 0, 0, w, h, 1,  IL_BGRA, IL_UNSIGNED_BYTE, ABitmap.ScanLine);
    result := true;
  end;

  //Delete the DevIL image resource
  ilDeleteImages(1, @imagename);
end;

{ TAdDevILPNGFormat }

class function TAdDevILPNGCompressor.ID: TAdVeryShortString;
begin
  result := #2+'PNG';
end;

procedure TAdDevILPNGCompressor.Read(ABitmap: TAdBitmap; AStream: TStream);
var
  ms: TMemoryStream;
  w, h : integer;
  imagename:Cardinal;
begin
  ms := TMemoryStream.Create;
  //Extract the PNG image from the source stream and save it to
  //a temporary memory stream
  ExtractPng(AStream, ms);

  //Generate a DevIL image
  ilGenImages(1, @imagename);
  ilBindImage(imagename);
  
  //Set the image origin to the upper left corner.
  ilEnable(IL_ORIGIN_SET);
  ilOriginFunc(IL_ORIGIN_UPPER_LEFT);
  
  //Convert images to the BGRA32 format
  ilEnable(IL_CONV_PAL);
  ilConvertPal(IL_PAL_BGRA32);
  
  //Load the image from the stream
  if Boolean(ilLoadL(IL_PNG, ms.Memory, ms.Size)) then
  begin    
    //Convert the image to BGRA
    ilConvertImage(IL_BGRA, IL_UNSIGNED_BYTE);

    //And copy the decoded memory into an Andorra 2D bitmap
    w := ilGetInteger(IL_IMAGE_WIDTH);
    h := ilGetInteger(IL_IMAGE_HEIGHT);

    ABitmap.ReserveMemory(w, h);
    ilCopyPixels(0, 0, 0, w, h, 1,  IL_BGRA, IL_UNSIGNED_BYTE, ABitmap.ScanLine);
  end;

  //Delete the DevIL image resource
  ilDeleteImages(1, @imagename);

  //Delete the temporary filestream that contained the extracted,
  //undecoded PNG Data
  ms.Free;
end;

procedure TAdDevILPNGCompressor.Write(ABitmap: TAdBitmap; AStream: TStream);
var
  mem:Pointer;
  size:int64;
  ms:TMemoryStream;
  imagename:Cardinal;
  y:integer;
  p1:PByte;
  count:integer;
  chars: TAdVeryShortString;
begin
  //Reserve some memory to store a upside down version of the bitmap in it
  mem := nil;
  GetMem(mem, ABitmap.Size);
  
  //Turn image upside down
  p1 := mem;
  for y := ABitmap.Height - 1 downto 0 do
  begin
    Move(ABitmap.Scanline(y)^, p1^, ABitmap.Width * 4);
    Inc(p1, ABitmap.Width * 4);
  end;                           
  
  //Generate a DevIL image
  ilGenImages(1, @imagename);
  ilBindImage(imagename);  

  //Load the bitmap in the DevIL system
  ilTexImage(ABitmap.Width, ABitmap.Height, 1, 4, IL_BGRA, IL_UNSIGNED_BYTE, mem);
  
  //Free the temporary memory
  FreeMem(mem, ABitmap.Size);

  //Reserve some memory to store the png plus header in it.
  size := ABitmap.Size + 128;
  GetMem(mem, size);
  FillChar(mem^, size, 0);
  
  //Save the image we generated as png to the memory
  ilSaveL(IL_PNG, mem, size);  

  //Create a memory stream
  ms := TMemoryStream.Create;
  
  //Write the png image to the stream
  ms.Write(mem^, size);
  
  ms.Position := 0;
  
  //Free the temporary memory
  FreeMem(mem, size);
  
  //Search for the end of the PNG image and remember the size of the image
  SetLength(chars, 4);
  repeat
    ms.Read(chars[1], 4);
    ms.Position := ms.Position - 3;
  until (chars = 'IEND') or (ms.Position >= ms.Size);
  
  count := ms.Position + 7;                          
  ms.Position := 0;

  //Copy the calculated amout of bytes into the destination memory stream
  AStream.CopyFrom(ms, count);
  
  //Free the temporary memory stream
  ms.Free;
  
  //Remove the image from the DevIL system
  ilDeleteImages(1, @imagename);    
end;

initialization
  ilInit();

  RegisterGraphicFormat(TAdDevILFormat);
  RegisterGraphicCompressor(TAdDevILPNGCompressor);

end.
