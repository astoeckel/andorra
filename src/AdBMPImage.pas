{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stöckel
* File: AdBMPImage.pas
* Comment: Provides an image class to load and save TGA files.
* Supported Formats to load:
* o Indexed with 24 bits per palette entry without RLE
* o RGB(A) with 16, 24 or 32 bits, without RLE
}

{Provides an image class to load and save BMP files.}
unit AdBMPImage;

interface

uses
  SysUtils, Classes,
  AdTypes, AdBitmap, AdMessages;

type
  {Exception class used within the BMP loader module.}
  EAdBMPImage = class(Exception);
  {Raised when one of the headers has an invalid size.}
  EAdBMPInvalidHeaderSize = class(EAdBMPImage);
  {Raised when the bitmap has a not supported compressor mode.}
  EAdBMPInvalidCompressorMode = class(EAdBMPImage);
  {Raised when the specified bit depth is not supported.}
  EAdBMPInvalidBitDepth = class(EAdBMPImage);

  {BMP header that describes the BMP file.}
  TAdBitmapFileHeader = packed record
    bfType: Word; //< must always be set to 'BM' to declare that this is a .bmp-file.
    bfSize: Cardinal; //< specifies the size of the file in bytes.
    bfReserved1: Word; //< must always be set to zero.
    bfReserved2: Word; //< must always be set to zero.
    bfOffBits: Cardinal; //< specifies the offset from the beginning of the file to the bitmap data.
  end;

  {BMP header that describes the BMP image dimensions.}
  TAdBitmapInfoHeader = packed record
    biSize: Cardinal; //< Length of the Bitmap Info Header used to describe the bitmap colors, compression
    biWidth: Cardinal; //< Horizontal width of bitmap in pixels.
    biHeight: Cardinal; //< Vertical height of bitmap in pixels.
    biPlanes: Word; //< Number of planes in this bitmap.
    biBitCount: Word; //< Bits per pixel used to store palette entry information. This also identifies in an indirect way the number of possible colors. 
    biCompression: Cardinal; //< Compression specifications. The following values are possible
    biSizeImage: Cardinal; //< Size of the bitmap data in bytes. This number must be rounded to the next 4 byte boundary.
    biXPelsPerMeter: Cardinal; //< Horizontal resolution expressed in pixel per meter.
    biYPelsPerMeter: Cardinal; //< Vertical resolution expressed in pixels per meter.
    biClrUsed: Cardinal; //< Number of colors used by this bitmap. For a 8-bit / pixel bitmap this will be 100h or 256.
    biClrImportant: Cardinal; //< Number of important colors. This number will be equal to the number of colors when every color is important.
  end;

  {Array that is used to represent the palette of a bitmap file.}
  TAdBitmapPalette = array of TRGBARec;

  {Class that is able to load and save Windows bitmaps}
  TAdBMPImage = class
    private
      FBMPFileHeader: TAdBitmapFileHeader;
      FBMPInfoHeader: TAdBitmapInfoHeader;
      FBMPPalette: TAdBitmapPalette;

      FBitmap: TAdBitmap;
      FOwnBitmap: boolean;

      function GetWidth: Integer;
      function GetHeight: Integer;     
      procedure SetBitmap(AValue: TAdBitmap);
      procedure Clear;

      function ReadHeader(AStream: TStream): boolean;
      function ReadBitmapData(AStream: TStream; AOffset: int64): boolean;
    public
      {Creates an instance of TAdBMPImage.}
      constructor Create;
      {Destroys the instance of TAdBMPImage.}
      destructor Destroy; override;

      {Loads the BMP image from a stream.}
      procedure LoadFromStream(AStream: TStream);
      {Loads the BMP image from a file.}
      procedure LoadFromFile(const Filename: string);
      {Saves the BMP image to a stream.}
      procedure SaveToStream(AStream: TStream);
      {Saves the BMP image to a file.}
      procedure SaveToFile(const Filename: string);

      {Returns the width of the currently loaded image.}
      property Width: integer read GetWidth;
      {Returns the height of the currently loaded image.}
      property Height: integer read GetHeight;
      {The Andorra image wherefrom the image data is written to/read from.}
      property Bitmap: TAdBitmap read FBitmap write SetBitmap;
      {Access on the bitmap info header.} 
      property Header: TAdBitmapInfoHeader read FBMPInfoHeader;
  end;

const
  //The first chars of a bmp file: "BM"
  BMPHeader = $4D42;  
  //Contains the bit depths that are supported by TAdBMPImage
  BMPBitDepths = [1, 4, 8, 16, 24, 32];
  //The compressor modes that are supported by TAdBMPImage
  BMPCompressions = [0, 3];

implementation

{ TAdBMPImage }

constructor TAdBMPImage.Create;
begin
  inherited;

  //Create bitmap that contains the bitmap data
  FOwnBitmap := true;
  FBitmap := TAdBitmap.Create;

  //Reset bitmap headers
  FillChar(FBMPFileHeader, SizeOf(FBMPFileHeader), 0);
  FillChar(FBMPInfoHeader, SizeOf(FBMPInfoHeader), 0);
end;

destructor TAdBMPImage.Destroy;
begin
  Clear;

  inherited;
end;

procedure TAdBMPImage.Clear;
begin
  if FOwnBitmap then
    FBitmap.Free;
end;

function TAdBMPImage.GetHeight: Integer;
begin
  result := FBMPInfoHeader.biHeight;  
end;

function TAdBMPImage.GetWidth: Integer;
begin
  result := FBMPInfoHeader.biWidth;
end;

procedure TAdBMPImage.LoadFromFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAdBMPImage.LoadFromStream(AStream: TStream);
var
  offs: int64;
begin
  //Store the current stream position
  offs := AStream.Position;

  if ReadHeader(AStream) then
    ReadBitmapData(AStream, offs);

  //Set the stream position
  AStream.Position := offs + FBMPFileHeader.bfSize;
end;

function TAdBMPImage.ReadHeader(AStream: TStream): boolean;
var
  offs: int64;
begin
  offs := AStream.Position;
  
  //Check whether the stream is able to read the bitmap header
  if AStream.Size - AStream.Position > SizeOf(FBMPFileHeader) then
  begin
    AStream.Read(FBMPFileHeader, SizeOf(FBMPFileHeader));
    if FBMPFileHeader.bfType = BMPHeader then
    begin
      //Check whether the stream is big enough to containt the bitmap info header
      if (AStream.Size - AStream.Position > SizeOf(FBMPInfoHeader)) and
         (FBMPInfoHeader.biSize <> SizeOf(FBMPInfoHeader)) then
      begin
        AStream.Read(FBMPInfoHeader, SizeOf(FBMPInfoHeader));

        //Jump to the end of the header
        AStream.Position := offs + SizeOf(FBMPFileHeader) + FBMPInfoHeader.biSize;

        result := true;
      end else
      begin
        raise
          EAdBMPImage.Create(MsgNoBMP);
      end;
    end else
    begin
      raise
        EAdBMPInvalidHeaderSize.Create(MsgInvalidHeaderSize);
    end;
  end else
  begin
    raise
      EAdBMPInvalidHeaderSize.Create(MsgInvalidHeaderSize);
  end;
end;

function TAdBMPImage.ReadBitmapData(AStream: TStream; AOffset: int64): boolean;
var
  x, y, i: integer;
  bytecount: cardinal;
  p32b: PRGBARec;
  vb: byte;
  vw: word;
  vrgb: TRGBRec;
  vrgba: TRGBARec;
begin
  if FBMPInfoHeader.biCompression in BMPCompressions then
  begin
    if FBMPInfoHeader.biBitCount in BMPBitDepths then
    begin
      //Read palette (if one exists)
      if FBMPInfoHeader.biBitCount < 16 then
      begin
        SetLength(FBMPPalette, 1 shl FBMPInfoHeader.biBitCount);
        AStream.Read(FBMPPalette[0], 4 * Length(FBMPPalette));
      end;

      if (Width > 0) and (Height > 0) then
      begin
        FBitmap.ReserveMemory(Width, Height);

        AStream.Position := AOffset + FBMPFileHeader.bfOffBits;

        y := 0;

        p32b := FBitmap.ScanLine;
        inc(p32b, (FBitmap.Width) * (FBitmap.Height - 1));

        while y < Height do
        begin 
          bytecount := 0;
          x := 0;

          while x < Width do
          begin
            case FBMPInfoHeader.biBitCount of
              1:
              begin
                //Read byte
                AStream.Read(vb, 1);
                bytecount := bytecount + 1;

                //Set pixels
                for i := 7 downto 0 do
                begin
                  p32b^ := FBMPPalette[(vb and (1 shl i)) shr i];
                  p32b^.a := 255;
                  inc(p32b);
                end;

                //8 Pixels were read
                x := x + 8;
              end;
              4:
              begin
                //Read byte
                AStream.Read(vb, 1);
                bytecount := bytecount + 1;

                //Set the tow pixels
                p32b^ := FBMPPalette[vb and $F0 shr 4]; p32b^.a := 255; inc(p32b);
                p32b^ := FBMPPalette[vb and $0F shr 0]; p32b^.a := 255; inc(p32b);

                //Two pixels were read
                x := x + 2;
              end;
              8:
              begin
                //Read byte
                AStream.Read(vb, 1);
                bytecount := bytecount + 1;

                p32b^ := FBMPPalette[vb];
                p32b^.a := 255;
                inc(p32b);

                x := x + 1;            
              end;
              16:
              begin
                //Read word
                AStream.Read(vw, 2);
                bytecount := bytecount + 2;

                //Bit order:
                //15 14 13 12 11 10 9  8 | 7  6  5  4  3  2  1  0
                //----------------------------------------------
                //b  b  b  b  b  g  g  g | g  g  r  r  r  r  r  x
                //1  1  1  1  1  0  0  0 | 0  0  0  0  0  0  0  0 ==> & $F800 >> 8
                //0  0  0  0  0  1  1  1 | 1  1  0  0  0  0  0  0 ==> & $07C0 >> 3
                //0  0  0  0  0  0  0  0 | 0  0  1  1  1  1  1  0 ==> & $003E << 2

                //Convert rgb16 5-5-5-1 to rgba32 8-8-8-8
                p32b^.b := (vw and $F800) shr 8;
                p32b^.r := (vw and $07C0) shr 3;
                p32b^.g := (vw and $003E) shl 2;
                p32b^.a := 255;

                inc(p32b);

                //One pixel was read
                x := x + 1;
              end;
              24:
              begin
                //Read rgb record
                AStream.Read(vrgb, 3);
                bytecount := bytecount + 3;

                p32b^.r := vrgb.r;
                p32b^.g := vrgb.g;
                p32b^.b := vrgb.b;
                p32b^.a := 255;

                inc(p32b);

                //One pixel was read
                x := x + 1;
              end;
              32:
              begin
                //Read rgba record
                if FBMPInfoHeader.biCompression = 3 then
                begin
                  AStream.Read(vrgba, 4);

                  //X8R8G8B8 - "A" was not set
                  p32b^.a := 255;
                  p32b^.r := vrgba.g;
                  p32b^.g := vrgba.b;
                  p32b^.b := vrgba.a;
                end else
                begin
                  //R8G8B8A8
                  AStream.Read(p32b^, 4);
                end;

                inc(p32b);

                //One pixel was read
                x := x + 1;
              end;
            end;
          end;

          //Go back one row in the bitmap data
          dec(p32b, (FBitmap.Width * 2));

          //Every row is aligned to a 32-Bit length
          AStream.Read(vrgba, bytecount mod 4);

          y := y + 1;
        end;
      end;

      result := true;
    end else
    begin
      raise
        EAdBMPInvalidBitDepth.Create(MsgInvalidBitDepth);
    end;
  end else
  begin
    raise
      EAdBMPInvalidCompressorMode.Create(MsgInvalidCompressor);
  end;
end;

procedure TAdBMPImage.SaveToFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAdBMPImage.SaveToStream(AStream: TStream);
var
  ptr32: PRGBARec;
  y: integer;
begin
  //Set file header data
  FBMPFileHeader.bfType := BMPHeader;
  FBMPFileHeader.bfSize := SizeOf(FBMPFileHeader) + SizeOf(FBMPInfoHeader) +
    FBitmap.Size;
  FBMPFileHeader.bfReserved1 := 0;
  FBMPFileHeader.bfReserved2 := 0;
  FBMPFileHeader.bfOffBits := SizeOf(FBMPFileHeader) + SizeOf(FBMPInfoHeader);

  //Set the info header data
  FBMPInfoHeader.biSize := SizeOf(FBMPInfoHeader);
  FBMPInfoHeader.biWidth := FBitmap.Width;
  FBMPInfoHeader.biHeight := FBitmap.Height;
  FBMPInfoHeader.biPlanes := 1;
  FBMPInfoHeader.biBitCount := 32;
  FBMPInfoHeader.biCompression := 0;
  FBMPInfoHeader.biSizeImage := FBitmap.Size;
  FBMPInfoHeader.biXPelsPerMeter := 0;
  FBMPInfoHeader.biYPelsPerMeter := 0;
  FBMPInfoHeader.biClrUsed := 0;
  FBMPInfoHeader.biClrImportant := 0;

  //Write headers
  AStream.Write(FBMPFileHeader, SizeOf(FBMPFileHeader));
  AStream.Write(FBMPInfoHeader, SizeOf(FBMPInfoHeader));

  //Go to the last scanline
  ptr32 := FBitmap.Scanline;
  inc(ptr32, FBitmap.Width * (FBitmap.Height - 1));

  for y := 0 to FBitmap.Height - 1 do
  begin
    //Write scanlines
    AStream.Write(ptr32^, FBitmap.Width * 4);
    dec(ptr32, FBitmap.Width);
  end;
end;

procedure TAdBMPImage.SetBitmap(AValue: TAdBitmap);
begin
  Clear;
  FBitmap := AValue;
  FOwnBitmap := false;
end;

end.
