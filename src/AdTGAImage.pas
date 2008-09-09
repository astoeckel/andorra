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
* Author:  Manuel Eberl
* File: AdTGAImage.pas
* Comment: Provides an image class to load and save TGA files.
* Supported Formats to load:
* o Indexed with 15, 16, 24 or 32 bits per palette entry and 8, 16, 24 or 32
*   bits per pixel, with or without RLE
* o RGB with 15, 16, 24 or 32 bits, with or without RLE
* o Monochrome with 1, 8, 16, 24 or 32 bits per pixel, with or without RLE
* (attribute bits in all formats will be stored in the output bitmap)
}

{Provides an image class to load and save TGA files.}
unit AdTGAImage;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses 
  SysUtils, Classes,
  AdTypes, AdBitmap;

const
  HIGH_COLORS: array[0..31] of Byte =
  (0, 8, 16, 25, 33, 41, 49, 58, 66, 74, 82, 90, 99, 107, 115, 123, 132,
    140, 148, 156, 165, 173, 181, 189, 197, 206, 214, 222, 230, 239, 247, 255);

  BIT_VALUES: array[0..1] of Byte = (0, 255);

type
  EAdTGAUnsupportedFormatError = class(Exception);
  EAdTGAInvalidPictureError = class(Exception);

  TAdTGAPictureType = (ttNone = 0,
    ttIndexed = 1, ttRGB = 2, ttMonochrome = 3,
    ttIndexedRLE = 9, ttRGBRLE = 10, ttMonochromeRLE = 11);

	TAdTGAHeader = packed record
    IDLength: Byte;
    Palette: Byte;
    PictureType: TAdTGAPictureType;
    PaletteStart: Word;
    PaletteLength: Word;
    PaletteBits: Byte;
    ZeroX, ZeroY: Word;
    Width, Height: Word;
    BitsPerPixel: Byte;
    Flags: Byte;
  end;

  TAdTGAPixels = array of array of TRGBARec;

  TAdTGAImage = class
  private
    FWidth, FHeight: Integer;
    FHeader: TAdTGAHeader;
    FBitmap: TAdBitmap;
    FCompressed: Boolean;
    FHasAlphaChannel: Boolean;
    procedure SetBitmap(const Value: TAdBitmap);
    procedure SetHasAlphaChannel(const Value: Boolean);
  public
    constructor Create;
  	destructor Destroy; override;

    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const Filename: string);
    function GetInfo: string;
    procedure AssignAlphaChannel(const ABitmap: TAdBitmap);
    procedure AssignAlphaChannelTo(const ABitmap: TAdBitmap);
    procedure RemoveAlphaChannel;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bitmap: TAdBitmap read FBitmap write SetBitmap;
    property Header: TAdTGAHeader read FHeader;
    property Compressed: Boolean read FCompressed write FCompressed;
    property HasAlphaChannel: Boolean read FHasAlphaChannel
    	write SetHasAlphaChannel;
  end;

implementation

function PictureTypeToStr(const PictureType: TAdTGAPictureType): string;
begin
  case PictureType of
    ttNone: Result := 'empty';
    ttIndexed, ttIndexedRLE: Result := 'indexed';
    ttRGB, ttRGBRLE: Result := 'RGB';
    ttMonochrome, ttMonochromeRLE: Result := 'monochrome';
  end;
  if PictureType in [ttIndexedRLE, ttRGBRLE, ttMonochromeRLE] then
    Result := Result + ' RLE';
end;

procedure TAdTGAImage.LoadFromStream(AStream: TStream);
type
  TPixelArray = array[0..3] of Byte;
  PPixelArray = ^TPixelArray;

var
  Pixel: array[0..3] of Byte;
  MonochromeValue: Cardinal;
  BytesPerPaletteEntry, PaletteSize, BytesPerPixel: Cardinal;
  I, J: Integer;
  index: Cardinal;
  rleheader: ShortInt;
  rlecount: Cardinal;
  BLOrigin: Boolean;
  rgba: TRGBArec;
  p: PPixelArray;

  Palette: array of TRGBARec;
  Data: TAdTGAPixels;

begin
  AStream.Read(FHeader, sizeof(TAdTGAHeader));

  if not (FHeader.BitsPerPixel in [1, 8, 15, 16, 24, 32]) or
    ((FHeader.BitsPerPixel = 1) and not
    (FHeader.PictureType in [ttMonochrome, ttMonochromeRLE])) or
    ((FHeader.BitsPerPixel = 15) and not
    (FHeader.PictureType in [ttRGB, ttRGBRLE])) or
    ((FHeader.BitsPerPixel = 8) and not
    (FHeader.PictureType in [ttIndexed, ttIndexedRLE])) or
    not (FHeader.PictureType in [ttIndexed, ttRGB, ttMonochrome,
    ttIndexedRLE, ttRGBRLE, ttMonochromeRLE]) then
  begin
    raise EAdTGAUnsupportedFormatError.CreateFmt(
      'The format %d Bit %s is not supported.',
      [FHeader.BitsPerPixel, PictureTypeToStr(FHeader.PictureType)]
      );
  end;

  if (FHeader.ZeroX <> 0) or (FHeader.ZeroY <> 0) then
    raise EAdTGAUnsupportedFormatError.Create(
      'Shifted zero points are not supported.'
      );

  if (FHeader.PictureType in [ttIndexed, ttIndexedRLE]) then
  begin
    if FHeader.Palette = 0 then
      raise EAdTGAInvalidPictureError.Create(
        'Missing palette for indexed picture.');

    if not (FHeader.PaletteBits in [15, 16, 24, 32]) then
      raise EAdTGAUnsupportedFormatError.CreateFmt(
        '%d Bit Palettes are not supported.', [FHeader.PaletteBits]
        )
  end;

  AStream.Seek(FHeader.IDLength, soCurrent);

  FWidth := FHeader.Width;
  FHeight := FHeader.Height;

  if FHeader.PaletteBits = 15 then
    BytesPerPaletteEntry := 2
  else
    BytesPerPaletteEntry := FHeader.PaletteBits div 8;
  PaletteSize := BytesPerPaletteEntry * FHeader.PaletteLength;
  if FHeader.Palette <> 0 then
  begin
    setlength(Palette, FHeader.PaletteLength);

    case FHeader.PaletteBits of
      15, 16:
        for I := 0 to FHeader.PaletteLength - 1 do
        begin
          AStream.Read(Pixel[0], 2);
          Palette[I].r := HIGH_COLORS[(Pixel[1] and $7C) shr 2];
          Palette[I].g := HIGH_COLORS[((Pixel[1] and $03) shl 3) or
            ((Pixel[0] and $E0) shr 5)];
          Palette[I].b := HIGH_COLORS[Pixel[0] and $1F];
          Palette[I].a := BIT_VALUES[Pixel[0] shr 7];
        end;

      24:
        for I := 0 to FHeader.PaletteLength - 1 do
        begin
          AStream.Read(Pixel[0], 3);
          Palette[I].r := Pixel[2];
          Palette[I].g := Pixel[1];
          Palette[I].b := Pixel[0];
          Palette[I].a := 255;
        end;

      32:
        for I := 0 to FHeader.PaletteLength - 1 do
        begin
          AStream.Read(Pixel[0], 4);
          Palette[I].r := Pixel[2];
          Palette[I].g := Pixel[1];
          Palette[I].b := Pixel[0];
          Palette[I].a := Pixel[3];
        end;
    end;
  end
  else
    AStream.Seek(PaletteSize, soCurrent);

  if FHeader.PictureType <> ttNone then
  begin
    BLOrigin := (FHeader.Flags and $10) = 0;
    BytesPerPixel := 1;
    while FHeader.BitsPerPixel > 8 * BytesPerPixel do
      inc(BytesPerPixel);
    setlength(Data, FWidth, FHeight);

    case FHeader.PictureType of

      ttIndexed:
        begin
          index := 0;
          for I := FHeight - 1 downto 0 do
            for J := 0 to FWidth - 1 do
            begin
              AStream.Read(index, BytesPerPixel);
              Data[J, I] := Palette[index - FHeader.PaletteStart];
            end;
        end;

      ttRGB:
        case FHeader.BitsPerPixel of
          15, 16:
            for I := FHeight - 1 downto 0 do
              for J := 0 to FWidth - 1 do
              begin
                AStream.Read(Pixel[0], 2);
                Data[J, I].r := HIGH_COLORS[(Pixel[1] and $7C) shr 2];
                Data[J, I].g := HIGH_COLORS[((Pixel[1] and $03) shl 3) or
                  ((Pixel[0] and $E0) shr 5)];
                Data[J, I].b := HIGH_COLORS[Pixel[0] and $1F];
                Data[J, I].a := BIT_VALUES[Pixel[0] shr 7];
              end;

          24:
            for I := FHeight - 1 downto 0 do
              for J := 0 to FWidth - 1 do
              begin
                AStream.Read(Pixel[0], 3);
                Data[J, I].r := Pixel[2];
                Data[J, I].g := Pixel[1];
                Data[J, I].b := Pixel[0];
                Data[J, I].a := 255;
              end;

          32:
            for I := FHeight - 1 downto 0 do
              for J := 0 to FWidth - 1 do
              begin
                AStream.Read(Pixel[0], 4);
                Data[J, I].r := Pixel[2];
                Data[J, I].g := Pixel[1];
                Data[J, I].b := Pixel[0];
                Data[J, I].a := Pixel[3];
              end;

        end; //Bits per pixel case

      ttMonochrome:
        begin
          Pixel[1] := 255;
          if FHeader.BitsPerPixel = 1 then
          begin
            MonochromeValue := 0;
            for I := FHeight - 1 downto 0 do
              for J := 0 to FWidth - 1 do
              begin
                AStream.Read(MonochromeValue, 1);
                MonochromeValue := MonochromeValue and 1;
                Data[J, I].r := BIT_VALUES[MonochromeValue];
                Data[J, I].g := BIT_VALUES[MonochromeValue];
                Data[J, I].b := BIT_VALUES[MonochromeValue];
                Data[J, I].a := 255;
              end;
          end
          else
            for I := FHeight - 1 downto 0 do
              for J := 0 to FWidth - 1 do
              begin
                AStream.Read(Pixel[0], BytesPerPixel);
                Data[J, I].r := Pixel[0];
                Data[J, I].g := Pixel[0];
                Data[J, I].b := Pixel[0];
                Data[J, I].a := Pixel[1];
              end;
        end;

      ttIndexedRLE:
        begin
          I := FHeight - 1;
          J := 0;
          index := 0;
          while I >= 0 do
          begin
            AStream.Read(rleheader, 1);
            if rleheader < 0 then
              AStream.Read(index, BytesPerPixel);
            rlecount := (rleheader and $7F) + 1;
            while (rlecount > 0) and (I >= 0) do
            begin
              if rleheader >= 0 then
                AStream.Read(index, BytesPerPixel);
              dec(rlecount);
              Data[J, I] := Palette[index - FHeader.PaletteStart];
              inc(J);
              if J = FWidth then
              begin
                J := 0;
                dec(I);
              end;
            end;
          end;
        end; //RLE encoded palette

      ttRGBRLE:
        begin
          I := FHeight - 1;
          J := 0;

          case FHeader.BitsPerPixel of

            15, 16:
              while I >= 0 do
              begin
                AStream.Read(rleheader, 1);
                if rleheader < 0 then
                  AStream.Read(Pixel, BytesPerPixel);
                rlecount := (rleheader and $7F) + 1;
                while (rlecount > 0) and (I >= 0) do
                begin
                  if rleheader >= 0 then
                    AStream.Read(Pixel, BytesPerPixel);
                  dec(rlecount);

                  Data[J, I].r := HIGH_COLORS[Pixel[1] and $7C];
                  Data[J, I].g := HIGH_COLORS[(Pixel[1] and $03) or
                    (Pixel[0] and $E0)];
                  Data[J, I].b := HIGH_COLORS[Pixel[0] and $1F];
                  Data[J, I].a := BIT_VALUES[Pixel[1] shr 7];

                  inc(J);
                  if J = FWidth then
                  begin
                    J := 0;
                    dec(I);
                  end;
                end; //while rlecount>0
              end; //while I>=0

            24:
              while I >= 0 do
              begin
                AStream.Read(rleheader, 1);
                if rleheader < 0 then
                  AStream.Read(Pixel, BytesPerPixel);
                rlecount := (rleheader and $7F) + 1;
                while (rlecount > 0) and (I >= 0) do
                begin
                  if rleheader >= 0 then
                    AStream.Read(Pixel, BytesPerPixel);
                  dec(rlecount);

                  Data[J, I].r := Pixel[2];
                  Data[J, I].g := Pixel[1];
                  Data[J, I].b := Pixel[0];
                  Data[J, I].a := 255;

                  inc(J);
                  if J = FWidth then
                  begin
                    J := 0;
                    dec(I);
                  end;
                end; //while rlecount>0
              end; //while I>=0

            32:
              while I >= 0 do
              begin
                AStream.Read(rleheader, 1);
                if rleheader < 0 then
                  AStream.Read(Pixel, BytesPerPixel);
                rlecount := (rleheader and $7F) + 1;
                while (rlecount > 0) and (I >= 0) do
                begin
                  if rleheader >= 0 then
                    AStream.Read(Pixel, BytesPerPixel);
                  dec(rlecount);

                  Data[J, I].r := Pixel[2];
                  Data[J, I].g := Pixel[1];
                  Data[J, I].b := Pixel[0];
                  Data[J, I].a := Pixel[3];

                  inc(J);
                  if J = FWidth then
                  begin
                    J := 0;
                    dec(I);
                  end;
                end; //while rlecount>0

              end; //while I>=0
          end; //Bits per pixel case

        end; //RLE encoded RGB

      ttMonochromeRLE:
        begin
          if FHeader.BitsPerPixel = 1 then
          begin
            I := FHeight - 1;
            J := 0;
            MonochromeValue := 0;
            while I >= 0 do
            begin
              AStream.Read(rleheader, 1);
              if rleheader < 0 then
                AStream.Read(MonochromeValue, 1);
              rlecount := (rleheader and $7F) + 1;
              while (rlecount > 0) and (I >= 0) do
              begin
                if rleheader >= 0 then
                  AStream.Read(MonochromeValue, 1);
                dec(rlecount);

                Data[J, I].r := BIT_VALUES[MonochromeValue];
                Data[J, I].g := BIT_VALUES[MonochromeValue];
                Data[J, I].b := BIT_VALUES[MonochromeValue];
                Data[J, I].a := 255;

                inc(J);
                if J = FWidth then
                begin
                  J := 0;
                  dec(I);
                end;
              end; //while rlecount>0
            end; //while I>=0
          end //case 1 bpp

          else
          begin
            I := FHeight - 1;
            J := 0;
            Pixel[1] := 255;
            while I >= 0 do
            begin
              AStream.Read(rleheader, 1);
              if rleheader < 0 then
                AStream.Read(Pixel[0], BytesPerPixel);
              rlecount := (rleheader and $7F) + 1;
              while (rlecount > 0) and (I >= 0) do
              begin
                if rleheader >= 0 then
                  AStream.Read(Pixel[0], BytesPerPixel);
                dec(rlecount);

                Data[J, I].r := Pixel[0];
                Data[J, I].g := Pixel[0];
                Data[J, I].b := Pixel[0];
                Data[J, I].a := Pixel[1];

                inc(J);
                if J = FWidth then
                begin
                  J := 0;
                  dec(I);
                end;
              end; //while rlecount>0
            end; //while I>=0
          end; //case >1bpp
        end; //RLE encoded monochrome

    end; //Image type case

    if not BLOrigin then
      for I := 0 to FWidth - 1 do
        for J := 0 to (FHeight div 2) - 1 do
        begin
          rgba := Data[I, J];
          Data[I, J] := Data[I, Height - J - 1];
          Data[I, Height - J - 1] := rgba;
        end;

  end; //Picture

  if not assigned(FBitmap) then
    FBitmap := TAdBitmap.Create;
  FBitmap.ReserveMemory(FWidth, FHeight);
  for I := 0 to FBitmap.Height - 1 do
  begin
    p := PPixelArray(FBitmap.ScanLine(I));
    for J := 0 to FBitmap.Width - 1 do
    begin
      p^[0] := Data[J, I].b;
      p^[1] := Data[J, I].g;
      p^[2] := Data[J, I].r;
      p^[3] := Data[J, I].a;
      inc(p);
    end;
  end;

  FHasAlphaChannel:=(Header.Flags and $0F)>0;

end;

procedure TAdTGAImage.RemoveAlphaChannel;
type
	TPixelArray=array[0..3] of Byte;
  PPixelArray=^TPixelArray;
var p1: PPixelArray;
  	I, J: Integer;
begin
  FHasAlphaChannel:=False;
  for I := 0 to FHeight-1 do
  begin
    p1:=FBitmap.ScanLine(I);
    for J := 0 to FWidth-1 do
    begin
      p1^[3]:=0;
      inc(p1, 4);
    end;
  end;
end;

procedure TAdTGAImage.SaveToStream(AStream: TStream);
var
  I, J, pos: Integer;
  p: PLongWord;
  LastPixel: LongWord;
  NewPackage, IsRaw: Boolean;
  Header: TAdTGAHeader;
  Buffer: array[0..127] of LongWord;
  rleheader: Byte;


  procedure ProcessRLEPixel();
  begin
		if rleheader = 128 then
    begin
      dec(rleheader);
      if not IsRaw then
        rleheader := rleheader or $80;
      NewPackage := True;
      AStream.Write(rleheader, 1);
      AStream.Write(Buffer[0], pos*4);
      IsRaw:=True;
      pos := 0;
      rleheader:=0;
    end;

    if (p^ = LastPixel) and not NewPackage then
    begin
      if IsRaw then
      begin
        if rleheader>0 then
        begin
        	dec(rleheader);
    	  	AStream.Write(rleheader, 1);
	 	    	AStream.Write(Buffer[0], pos*4);
        end;
        Buffer[0] := LastPixel;
        IsRaw := false;
        pos := 1;
        rleheader:=0;
      end;
      inc(rleheader);
    end else if (p^ <> LastPixel) and not IsRaw then
    begin
      NewPackage := True;
      rleheader:=rleheader or $80;
      AStream.Write(rleheader, 1);
      AStream.Write(Buffer[0], pos*4);
      IsRaw:=True;
      pos := 0;
      rleheader:=0;
    end else begin
      Buffer[pos] := LastPixel;
      inc(pos);
      inc(rleheader);
    end;
  end;


begin
  Header.IDLength := 0;
  Header.Palette := 0;
  if FCompressed then
		Header.PictureType := ttRGBRLE
  else
  	Header.PictureType := ttRGB;
  Header.PaletteStart := 0;
  Header.PaletteLength := 0;
  Header.PaletteBits := 0;
  Header.ZeroX := 0;
  Header.ZeroY := 0;
  Header.Width := FWidth;
  Header.Height := FHeight;
  Header.BitsPerPixel := 32;
  Header.Flags := 8;

  AStream.Write(Header, sizeof(Header));

  if FCompressed then
  begin
    pos := 0;
    rleheader := 0;
    IsRaw:=True;
    NewPackage:=True;

    for I := 0 to FHeight - 1 do
    begin
      p := FBitmap.ScanLine(Height-I-1);
	    for J := 0 to FWidth - 1 do
  	  begin
    	  if not NewPackage then
      	begin
        	ProcessRLEPixel;
	      end;
        NewPackage := False;
  	    LastPixel := p^;
    	  inc(p);
    	end;
  	end;

		if rleheader = 128 then
    begin
      dec(rleheader);
      if not IsRaw then
        rleheader := rleheader or $80;
      NewPackage := True;
      AStream.Write(rleheader, 1);
      AStream.Write(Buffer[0], pos*4);
      IsRaw:=True;
      pos := 0;
      rleheader:=0;
    end;

	  if not IsRaw then
    	rleheader:=rleheader or $80
    else
      Buffer[pos]:=LastPixel;
    inc(pos);
    AStream.Write(rleheader, 1);
    AStream.Write(Buffer[0], pos*4);

  end
  else
    for I:=0 to FHeight-1 do
      AStream.Write(FBitmap.Scanline(Height-I-1)^, FWidth*4);

end;

procedure TAdTGAImage.LoadFromFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAdTGAImage.SaveToFile(const Filename: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate, fmShareDenyWrite);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAdTGAImage.SetBitmap(const Value: TAdBitmap);
begin
  if not assigned(FBitmap) then
    FBitmap := TAdBitmap.Create;
  if FBitmap <> Value then
    FBitmap.Assign(Value);
  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;
end;

procedure TAdTGAImage.SetHasAlphaChannel(const Value: Boolean);
begin
  if not Value then
    if FHasAlphaChannel then
      RemoveAlphaChannel;
end;

procedure TAdTGAImage.AssignAlphaChannel(const ABitmap: TAdBitmap);
type
	TPixelArray=array[0..3] of Byte;
  PPixelArray=^TPixelArray;
var p1, p2: PPixelArray;
  	I, J: Integer;
begin
  FHasAlphaChannel:=True;
  for I := 0 to FHeight-1 do
  begin
    p1:=FBitmap.ScanLine(I);
    p2:=ABitmap.ScanLine(I);
    for J := 0 to FWidth-1 do
    begin
      p1^[3]:=p2^[3];
      inc(p1, 4);
      inc(p2, 4);
    end;
  end;
end;

procedure TAdTGAImage.AssignAlphaChannelTo(const ABitmap: TAdBitmap);
type
	TPixelArray=array[0..3] of Byte;
  PPixelArray=^TPixelArray;
var p1, p2: PPixelArray;
  	I, J: Integer;
begin
  for I := 0 to FHeight-1 do
  begin
    p1:=FBitmap.ScanLine(I);
    p2:=ABitmap.ScanLine(I);
    for J := 0 to FWidth-1 do
    begin
      p2^[3]:=p1^[3];
      inc(p1, 4);
      inc(p2, 4);
    end;
  end;
end;

constructor TAdTGAImage.Create;
begin
  inherited Create;
  FCompressed:=True;
end;

destructor TAdTGAImage.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TAdTGAImage.GetInfo: string;
begin
  Result := Format('%d BPP, %s', [FHeader.BitsPerPixel,
    PictureTypeToStr(FHeader.PictureType)]);
  if FHeader.Palette <> 0 then
    Result := Result + Format(', %d palette entries, %d bits per entry',
      [FHeader.PaletteLength, FHeader.PaletteBits]);
end;

end.
