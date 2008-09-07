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
* Author:  Manuel Eberl
* File: AdPNGImage.pas
* Comment: Provides an image class to load and save PNG files.
}
unit AdPNGImage;

{$DEFINE LITTLE_ENDIAN}

interface

uses SysUtils, Classes, Math, AdTypes, AdBitmap, Graphics, AdCRC, AdDeflate;

const
  HIGH_COLORS: array[0..31] of Byte =
  (0, 8, 16, 25, 33, 41, 49, 58, 66, 74, 82, 90, 99, 107, 115, 123, 132,
    140, 148, 156, 165, 173, 181, 189, 197, 206, 214, 222, 230, 239, 247, 255);

  UPSAMPLING_TABLE_1BIT: array[0..1] of Byte = (0, 255);
  UPSAMPLING_TABLE_2BIT: array[0..3] of Byte = (0, 85, 170, 255);
  UPSAMPLING_TABLE_4BIT: array[0..15] of Byte =
  	(0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 170, 187, 204, 221, 238, 255);

  BITMASKS: array[1..4] of Byte = ($01, $03, $07, $0F);

  HEADERBYTES = #$89#$50#$4E#$47#$0D#$A#$1A#$0A;

type
  EAdPNGInvalidHeaderBytes = class(Exception);
  EAdPNGInvalidFileException = class(Exception);
  EAdPNGBrokenChunk = class(Exception);
  EAdPNGUnsupportedFormatException = class(Exception);
  EAdPNGUnknownChunkException = class(Exception);


  TAdPNGChunk = packed record
    DataLength: LongWord;
    ID: array[1..4] of Char;
    Data: array of Byte;
    CRC: LongWord;
    ChunkType: String;
  end;


  TAdPNGColorType = set of (pctPalette = 0, pctColor = 1, pctAlpha = 2);
  TAdPNGCompressionMethod = (pcDeflate);
  TAdPNGFilterMethod = (pfStandard = 0);
  TAdPNGInterlaceMethod = (piNone = 0, piAdam7 = 1);
  TAdPNGFilterType =
  	(pfNone = 0, pfSub = 1, pfUp = 2, pfAverage = 3, pfPaeth = 4);


  TAdPNGHeader = packed record
    Width, Height: LongWord;
    BitDepth: Byte;
    ColorType: TAdPNGColorType;
    CompressionMethod: TAdPNGCompressionMethod;
    FilterMethod: TAdPNGFilterMethod;
    InterlaceMethod: TAdPNGInterlaceMethod;
  end;

  PAdPNGHeader = ^TAdPNGHeader;


  TAdPNGAdam7Pass = record
  	XOffset, YOffset, XSkip, YSkip: Integer;
  end;


  TAdPNGPalette = array of TRGBARec;


  TAdPNGScanline = array of Byte;


  TAdPNGSample = Byte;

  TAdPNGSamples = array of TAdPNGSample;


  TAdPNGLine = array of TRGBARec;

  TAdPNGPixels = array of array of TRGBARec;


const
	ADAM7PASSES: array[1..7] of TAdPNGAdam7Pass =
  	(
    	(XOffset: 0; YOffset: 0; XSkip: 8; YSkip: 8),
    	(XOffset: 4; YOffset: 0; XSkip: 8; YSkip: 8),
      (XOffset: 0; YOffset: 4; XSkip: 4; YSkip: 8),
      (XOffset: 2; YOffset: 0; XSkip: 4; YSkip: 4),
      (XOffset: 0; YOffset: 2; XSkip: 2; YSkip: 4),
      (XOffset: 1; YOffset: 0; XSkip: 2; YSkip: 2),
      (XOffset: 0; YOffset: 1; XSkip: 1; YSkip: 2)
    );
    

type

  TAdPNGImage = class
  private
    FWidth, FHeight: Integer;

    FHeaderChunk: TAdPNGChunk;
    FHeader: PAdPNGHeader;

    FDataStream: TMemoryStream;

    FPixels: TAdPNGPixels;
    FPalette: TAdPNGPalette;

    FBitmap: TAdBitmap;
    FCompressed: Boolean;
    FHasAlphaChannel: Boolean;
    FBytesPerPixel: Integer;
    FSamplesPerPixel: Integer;

    function ReadChunk(const AStream: TStream;
    		ReadData: Boolean = False): TAdPNGChunk;
    procedure SkipChunk(const AStream: TStream);
		function CheckChunk(const AStream: TStream;
				const AChunk: TAdPNGChunk): Boolean;
    function ChunkIsOptional(const ID: String): Boolean;

    procedure DecompressData(const Input, Output: TStream);
    procedure ProcessChunk(const AStream: TStream; const ID: String);

		function ReadSamples(const Scanline: TAdPNGScanline): TAdPNGSamples;

    procedure ReadPixels(const AStream: TStream);
    procedure ReadUninterlacedPixels(const AStream: TStream);
    procedure ReadInterlacedPixels(const AStream: TStream);

    procedure UnfilterLine(const FilterType: TAdPNGFilterType;
			var Line: TAdPNGScanline; LastLine: TAdPNGScanline);

  	function ConvertSamplesToPixels(const Samples: TAdPNGSamples): TAdPNGLine;

    procedure SetBitmap(const Value: TAdBitmap);
    procedure SetHasAlphaChannel(const Value: Boolean);

  public
    constructor Create;
  	destructor Destroy; override;

    procedure LoadFromStream(AStream: TStream);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToStream(AStream: TStream);
    procedure SaveToFile(const Filename: string);
    procedure AssignAlphaChannel(const ABitmap: TAdBitmap);
    procedure AssignAlphaChannelTo(const ABitmap: TAdBitmap);
    procedure RemoveAlphaChannel;
    procedure AssignToBitmap(const ABitmap: TBitmap);

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bitmap: TAdBitmap read FBitmap write SetBitmap;
    property Header: PAdPNGHeader read FHeader;
    property Compressed: Boolean read FCompressed write FCompressed;
    property HasAlphaChannel: Boolean read FHasAlphaChannel
    	write SetHasAlphaChannel;

  end;


implementation

procedure ConvertWord(var Value: Word);
begin
  {$IFDEF LITTLE_ENDIAN}
  Value:=(Value shl 8) or (Value shr 8);
  {$ENDIF}
end;

procedure ConvertLongWord(var Value: LongWord);
begin
  {$IFDEF LITTLE_ENDIAN}
  Value:=(Value shl 24) or ((Value and $FF0000) shr 8) or
  	((Value and $FF00) shl 8) or (Value shr 24);
  {$ENDIF}    
end;

function Paeth(const Left, Above, UpperLeft: Byte): Byte;
var p, pa, pb, pc: Integer;
begin

	p := Left+Above-UpperLeft;
	pa := abs(p - Left);
	pb := abs(p - Above);
	pc := abs(p - UpperLeft);

	if (pa <= pb) and (pa <= pc) then
    Result:=Left
  else if pb <= pc then
    Result:=Above
	else
    Result:=UpperLeft;

end;

procedure TAdPNGImage.LoadFromStream(AStream: TStream);
type
  TPixelArray = array[0..3] of Byte;
  PPixelArray = ^TPixelArray;
  
var HeaderByteBuffer: array[1..8] of Char;
    id: array[1..4] of Char;
    I, J: Integer;
    p: PPixelArray;
begin
  FPalette:=nil;
  FDataStream:=TMemoryStream.Create;

  AStream.Read(HeaderByteBuffer[1], 8);
  for I := 1 to 8 do
    if HeaderByteBuffer[I]<>HEADERBYTES[I] then
	    raise EAdPNGInvalidHeaderBytes.Create(
      		'Invalid sequence of header bytes.');

  FHeaderChunk:=ReadChunk(AStream, True);
  FHeader:=PAdPNGHeader(FHeaderChunk.Data);

  ConvertLongWord(FHeader.Width);
  ConvertLongWord(FHeader.Height);

  FWidth:=FHeader.Width;
  FHeight:=FHeader.Height;

  repeat
    AStream.Position:=AStream.Position+4;
    AStream.read(id[1], 4);
    AStream.Position:=AStream.Position-8;
    ProcessChunk(AStream, id);
  until (id='IEND') or (AStream.Position=AStream.Size);

  if id<>'IEND' then
    raise EAdPNGInvalidFileException.Create('Unexpected end of input.');

  ReadPixels(FDataStream);
  FDataStream.Free;

  if not assigned(FBitmap) then
    FBitmap := TAdBitmap.Create;
  FBitmap.ReserveMemory(FWidth, FHeight);

  for I := 0 to FBitmap.Height - 1 do
  begin
    p := PPixelArray(FBitmap.ScanLine(I));
    for J := 0 to FBitmap.Width - 1 do
    begin
      p^[0] := FPixels[J, I].b;
      p^[1] := FPixels[J, I].g;
      p^[2] := FPixels[J, I].r;
      p^[3] := FPixels[J, I].a;
      inc(p);
    end;
  end;

  setlength(FPixels, 0, 0);

end;

procedure TAdPNGImage.ProcessChunk(const AStream: TStream;
		const ID: String);
type
  TRGB = packed record
    R, G, B: Byte;
  end;
  PRGB = ^TRGB;

var
	I: Integer;
  Chunk: TAdPNGChunk;
  p: PRGB;
begin

  if id='PLTE' then
  begin
  	Chunk:=ReadChunk(AStream, True);
    setlength(FPalette, Chunk.DataLength div 3);
    p:=@Chunk.Data[0];

    for I := 0 to high(FPalette) do
    begin
      FPalette[I].r:=p^.R;
      FPalette[I].g:=p^.G;
      FPalette[I].b:=p^.B;
      FPalette[I].a:=255;
      inc(p);
    end;

  end
  else if id='IDAT' then
  begin

  	Chunk:=ReadChunk(AStream, True);
    FDataStream.Write(Chunk.Data[0], Chunk.DataLength);

  end else if id='IEND' then
  	exit
  else if ChunkIsOptional(id) then
    SkipChunk(AStream)
  else
    raise EAdPNGUnknownChunkException.CreateFmt(
    	'Unknown non-optional chunk ''%s''', [Chunk.ChunkType]);
           

end;

function TAdPNGImage.ReadChunk(const AStream: TStream;
		ReadData: Boolean = False): TAdPNGChunk;
begin
  AStream.Read(Result, 8);
  Result.ChunkType:=Result.ID;
  ConvertLongWord(Result.DataLength);

  if ReadData then
  begin
    setlength(Result.Data, Result.DataLength);
    AStream.Read(Result.Data[0], Result.DataLength);
    AStream.Read(Result.CRC, 4);
    ConvertLongWord(Result.CRC);
  end else
  begin
	  AStream.Position:=AStream.Position+Result.DataLength;
  	AStream.Read(Result.CRC, 4);
	  ConvertLongWord(Result.CRC);
  end;

 	AStream.Position:=AStream.Position-Result.DataLength-8;

  if not CheckChunk(AStream, Result) then
    raise EAdPNGBrokenChunk.CreateFmt('Invalid CRC value in chunk ''%s''.',
    	[Result.ChunkType]);

  AStream.Position:=AStream.Position+4;
end;

procedure TAdPNGImage.SkipChunk(const AStream: TStream);
var datalength: Cardinal;
begin
  AStream.Position:=AStream.Position;
  AStream.Read(datalength, 4);
  ConvertLongWord(datalength);
  AStream.Position:=AStream.Position+datalength+8;
end;

procedure TAdPNGImage.DecompressData(const Input, Output: TStream);
var decompressor: TAdDeflateDecompressor;
begin
  Input.Position:=Input.Position+2;

  decompressor:=TAdDeflateDecompressor.Create;

  try
    decompressor.Decompress(Input, Output);
  finally
    decompressor.Free;
  end;

end;

function TAdPNGImage.ReadSamples(const Scanline: TAdPNGScanline): TAdPNGSamples;

    procedure ReadSubbyteSamples(var Samples: TAdPNGSamples);
		var NextSample, ByteIndex, BitIndex: Integer;
      	ByteBuffer: Byte;
    begin
        NextSample:=0;
        ByteIndex:=0;

				while ByteIndex<length(scanline) do
	  		begin

			    ByteBuffer:=Scanline[ByteIndex];
          inc(ByteIndex);
	        BitIndex:=8;

  	  		while BitIndex>0 do
    	    begin

            if pctPalette in FHeader.ColorType then
    	        Samples[NextSample]:=(ByteBuffer shr (BitIndex-FHeader.BitDepth))
		  	          and BITMASKS[FHeader.BitDepth]
            else case FHeader.BitDepth of
	          	1: Samples[NextSample]:=UPSAMPLING_TABLE_1BIT[
              		(ByteBuffer shr (BitIndex-1))	and BITMASKS[1]];
	        	  2: Samples[NextSample]:=UPSAMPLING_TABLE_2BIT[
              		(ByteBuffer shr (BitIndex-2))	and BITMASKS[2]];
    		      4: Samples[NextSample]:=UPSAMPLING_TABLE_4BIT[
	              	(ByteBuffer shr (BitIndex-4))	and BITMASKS[4]];
	          end;

            dec(BitIndex, FHeader.BitDepth);
            inc(NextSample);

        	end;

		  	end;
    end;

var I: Integer;
begin

	setlength(Result, length(scanline)*8 div FHeader.BitDepth);

  if FHeader.BitDepth<8 then
    ReadSubbyteSamples(Result)
  else if FHeader.BitDepth=8 then
  	Result:=TAdPNGSamples(Scanline)
  else if FHeader.BitDepth=16 then
	begin

    for I := 0 to length(scanline) div 2 - 1 do
    begin
      Result[I]:=Scanline[I*2];
    end;

  end;       

end;

procedure TAdPNGImage.ReadPixels(const AStream: TStream);
var Data: TStream;
begin
  Data:=TMemoryStream.Create;
  AStream.Position:=0;
  DecompressData(AStream, Data);
  Data.Seek(0, soBeginning);

  if (FHeader.ColorType=[]) or (FHeader.ColorType=[pctPalette, pctColor]) then
    FSamplesPerPixel:=1
  else if FHeader.ColorType=[pctColor] then
    FSamplesPerPixel:=3
  else if FHeader.ColorType=[pctAlpha] then
    FSamplesPerPixel:=2
  else if FHeader.ColorType=[pctColor, pctAlpha] then
    FSamplesPerPixel:=4
  else
  	raise EAdPNGUnsupportedFormatException.Create(
    	'Specified color type not supported.');

  FBytesPerPixel:=ceil(FSamplesPerPixel*FHeader.BitDepth/8);

  setlength(FPixels, FWidth, FHeight);

  if FHeader.InterlaceMethod=piAdam7 then
    ReadInterlacedPixels(Data)
  else
    ReadUninterlacedPixels(Data);

end;

procedure TAdPNGImage.ReadUninterlacedPixels(const AStream: TStream);
var I, J, BytesPerLine: Integer;
  	FilterType: TAdPNGFilterType;
  	Scanline, LastScanline: TAdPNGScanline;
    Line: TAdPNGLine;
    LineSamples: TAdPNGSamples;
begin

  BytesPerLine:=ceil(FHeader.BitDepth*FSamplesPerPixel*FWidth/8);
  setlength(LastScanline, 0);

  for I := 0 to FHeight - 1 do
  begin
  	AStream.Read(FilterType, 1);
    setlength(Scanline, BytesPerLine);
    AStream.Read(Scanline[0], BytesPerLine);

    UnfilterLine(FilterType, Scanline, LastScanline);
    LastScanline:=Scanline;

    LineSamples:=ReadSamples(Scanline);

    Line:=ConvertSamplesToPixels(LineSamples);

    for J:=0 to FWidth-1 do
    	FPixels[J, I]:=Line[J];

  end;

end;


procedure TAdPNGImage.ReadInterlacedPixels(const AStream: TStream);
var pass: TAdPNGAdam7Pass;
  	I, J, x, y, BytesPerLine: Integer;
    FilterType: TAdPNGFilterType;
    Scanline, LastScanline: TAdPNGScanline;
    LineSamples: TAdPNGSamples;
    Line: TAdPNGLine;
begin

  setlength(LastScanline, 0);

  for I := 1 to 7 do
  begin

    pass:=ADAM7PASSES[I];

   	BytesPerLine:=
    	ceil(ceil((FWidth-pass.XOffset)/pass.XSkip) * FSamplesPerPixel *
      FHeader.BitDepth/8);

    if BytesPerLine=0 then
      continue;

    y:=pass.YOffset;

    while y<FHeight do
    begin

      J:=0;
      x:=pass.XOffset;

    	AStream.Read(FilterType, 1);
      setlength(Scanline, BytesPerLine);
	    AStream.Read(Scanline[0], BytesPerLine);

    	UnfilterLine(FilterType, Scanline, LastScanline);
  	  LastScanline:=Scanline;

	    LineSamples:=ReadSamples(Scanline);
			Line:=ConvertSamplesToPixels(LineSamples);

    	while x<FWidth do
      begin
        FPixels[x, y]:=Line[J];
        inc(x, pass.XSkip);
        inc(J);
      end;

      inc(y, pass.YSkip);

    end;

  end;

end;


function TAdPNGImage.ConvertSamplesToPixels(const Samples: TAdPNGSamples):
		TAdPNGLine;
var I, J, n: Integer;
begin
  J:=0;
  n:=length(Samples) div FSamplesPerPixel;
  setlength(result, n);

  if FHeader.ColorType=[] then
   	for I:=0 to n-1 do
    begin
 	    Result[I].r:=Samples[J];
   	  Result[I].g:=Samples[J];
     	Result[I].b:=Samples[J];
      Result[I].a:=255;
      inc(J);
 	  end

  else if FHeader.ColorType=[pctAlpha] then
    for I:=0 to n-1 do
    begin
      Result[I].r:=Samples[J];
     	Result[I].g:=Samples[J];
      Result[I].b:=Samples[J];
	    Result[I].a:=Samples[J+1];
     	inc(J, 2);
	  end

  else if FHeader.ColorType=[pctColor] then
    for I:=0 to n-1 do
 	  begin
      Result[I].r:=Samples[J];
     	Result[I].g:=Samples[J+1];
      Result[I].b:=Samples[J+2];
	    Result[I].a:=255;
 	    inc(J, 3);
   	end

  else if FHeader.ColorType=[pctColor, pctAlpha] then
    for I:=0 to n-1 do
 	  begin
      Result[I].r:=Samples[J];
     	Result[I].g:=Samples[J+1];
      Result[I].b:=Samples[J+2];
	    Result[I].a:=Samples[J+3];
 	    inc(J, 4);
   	end

  else if (FHeader.ColorType=[pctColor, pctPalette]) or
  		(FHeader.ColorType=[pctPalette]) then
    for I:=0 to n-1 do
    begin
      Result[I]:=FPalette[Samples[J]];
      inc(J, 1);
    end;

end;

procedure TAdPNGImage.UnfilterLine(const FilterType: TAdPNGFilterType;
	var Line: TAdPNGScanline; LastLine: TAdPNGScanline);
var I: Integer;
begin

    if length(LastLine)=0 then
    begin
      if FilterType = pfUp then
        exit
      else if FilterType <> pfSub then
        setlength(LastLine, length(Line));
    end;

    case FilterType of

    pfSub:
    begin
      for I := FBytesPerPixel to high(Line) do
        Line[I]:=(Line[I]+Line[I-FBytesPerPixel]) and $FF;
    end;

    pfUp:
    begin
      for I := 0 to high(Line) do
        Line[I]:=(Line[I]+LastLine[I]) and $FF;
    end;

    pfAverage:
    begin
    	for I := 0 to FBytesPerPixel - 1 do
	 	    Line[I]:=(Line[I]+LastLine[I] div 2) and $FF;

      for I := FBytesPerPixel to high(Line) do
   	    Line[I]:=(Line[I]+(Line[I-FBytesPerPixel]+LastLine[I]) div 2) and $FF;
    end;

    pfPaeth:
    begin
      for I := 0 to FBytesPerPixel - 1 do
      	Line[I]:=(Line[I]+Paeth(0, LastLine[I], 0)) and $FF;

      for I := FBytesPerPixel to high(Line) do
	      Line[I]:=(Line[I]+Paeth(Line[I-FBytesPerPixel], LastLine[I],
  	    	LastLine[I-FBytesPerPixel])) and $FF;
    end;

    end;

end;

procedure TAdPNGImage.RemoveAlphaChannel;
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

procedure TAdPNGImage.SaveToStream(AStream: TStream);
begin
	//TODO
end;

procedure TAdPNGImage.LoadFromFile(const Filename: string);
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

procedure TAdPNGImage.SaveToFile(const Filename: string);
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

procedure TAdPNGImage.SetBitmap(const Value: TAdBitmap);
begin
  if not assigned(FBitmap) then
    FBitmap := TAdBitmap.Create;
  if FBitmap <> Value then
    FBitmap.Assign(Value);
  FWidth := FBitmap.Width;
  FHeight := FBitmap.Height;
end;

procedure TAdPNGImage.SetHasAlphaChannel(const Value: Boolean);
begin
  if not Value then
    if FHasAlphaChannel then
      RemoveAlphaChannel;
end;

procedure TAdPNGImage.AssignAlphaChannel(const ABitmap: TAdBitmap);
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

procedure TAdPNGImage.AssignAlphaChannelTo(const ABitmap: TAdBitmap);
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

procedure TAdPNGImage.AssignToBitmap(const ABitmap: TBitmap);
var p1, p2: PCardinal;
  	I, J: Integer;
begin
  for I := 0 to FHeight-1 do
  begin
    p1:=FBitmap.ScanLine(I);
    p2:=ABitmap.ScanLine[I];
    for J := 0 to FWidth-1 do
    begin
      p2^:=p1^;
      inc(p1);
      inc(p2);
    end;
  end;
end;

function TAdPNGImage.CheckChunk(const AStream: TStream;
		const AChunk: TAdPNGChunk): Boolean;
var buffer: array of Byte;
begin
	setlength(buffer, AChunk.DataLength+4);
  AStream.Read(buffer[0], AChunk.DataLength+4);

  Result:=CRC32(buffer[0], AChunk.DataLength+4)=AChunk.CRC;
end;

function TAdPNGImage.ChunkIsOptional(const ID: String): Boolean;
begin
  Result:=(Ord(ID[1]) shr 5) and 1=1;
end;

constructor TAdPNGImage.Create;
begin
  inherited Create;
  FCompressed:=True;
end;

destructor TAdPNGImage.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

end.
