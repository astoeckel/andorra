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

{Contains Image-Loader classes and a png compressor that use the "DevIL" library}
unit AdDevIL;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, AdBitmap, AdTypes, AdPNGUtils, DevIL;

type
  TAdDevILPNGCompressor = class(TAdGraphicCompressor)
    public
      procedure Write(ABitmap:TAdBitmap; AStream:TStream);override;
      procedure Read(ABitmap:TAdBitmap; AStream:TStream);override;
      class function ID:TAdVeryShortString;override;
  end;

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

  ilGenImages(1, @imagename);
  ilBindImage(imagename);

  ilEnable(IL_ORIGIN_SET);
  ilOriginFunc(IL_ORIGIN_UPPER_LEFT);
  ilEnable(IL_CONV_PAL);
  ilConvertPal(IL_PAL_BGRA32);
  
  if ilLoadImage(PChar(AFile)) < IL_INVALID_ENUM then
  begin
    ilConvertImage(IL_BGRA, IL_UNSIGNED_BYTE);

    w := ilGetInteger(IL_IMAGE_WIDTH);
    h := ilGetInteger(IL_IMAGE_HEIGHT);

    ABitmap.ReserveMemory(w, h);
    ilCopyPixels(0, 0, 0, w, h, 1,  IL_BGRA, IL_UNSIGNED_BYTE, ABitmap.ScanLine);
    result := true;
  end;

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
  ExtractPng(AStream, ms);

  ilGenImages(1, @imagename);
  ilBindImage(imagename);

  ilEnable(IL_ORIGIN_SET);
  ilOriginFunc(IL_ORIGIN_UPPER_LEFT);
  ilEnable(IL_CONV_PAL);
  ilConvertPal(IL_PAL_BGRA32);

  if ilLoadL(IL_PNG, ms.Memory, ms.Size) < IL_INVALID_ENUM then
  begin    
    ilConvertImage(IL_BGRA, IL_UNSIGNED_BYTE);

    w := ilGetInteger(IL_IMAGE_WIDTH);
    h := ilGetInteger(IL_IMAGE_HEIGHT);

    ABitmap.ReserveMemory(w, h);
    ilCopyPixels(0, 0, 0, w, h, 1,  IL_BGRA, IL_UNSIGNED_BYTE, ABitmap.ScanLine);
  end;

  ilDeleteImages(1, @imagename);

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
  chars: string[4];
begin
  ms := TMemoryStream.Create;

  ilGenImages(1, @imagename);
  ilBindImage(imagename);

  mem := nil;
  GetMem(mem, ABitmap.Size);
  
  //Turn image upside down
  p1 := mem;
  for y := ABitmap.Height - 1 downto 0 do
  begin
    Move(ABitmap.Scanline(y)^, p1^, ABitmap.Width * 4);
    Inc(p1, ABitmap.Width * 4);
  end;                           

  ilTexImage(ABitmap.Width, ABitmap.Height, 1, 4, IL_BGRA, IL_UNSIGNED_BYTE, mem);
  
  FreeMem(mem, ABitmap.Size);


  size := ABitmap.Size + 128;
  GetMem(mem, size);
  FillChar(mem^, size, 0);
  ilSaveL(IL_PNG, mem, size);
  ms.Write(mem^, size);
  ms.Position := 0;
  FreeMem(mem, size);

  SetLength(chars, 4);

  repeat
    ms.Read(chars[1],4);
    ms.Position := ms.Position - 3;
  until (chars = 'IEND') or (ms.Position >= ms.Size);
  count := ms.Position + 7;                          
  ms.Position := 0;

  AStream.CopyFrom(ms, count);
  
  ms.Free;

  ilDeleteImages(1, @imagename);
end;

initialization
  ilInit();

  RegisterGraphicFormat(TAdDevILFormat);
  RegisterGraphicCompressor(TAdDevILPNGCompressor);

end.
