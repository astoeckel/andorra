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
* File: AdDeflate.pas
* Comment: Provides a compressor (TODO) and decompressor for the deflate
* compression format. (See RFC 1951 for a detailed specification)
*
* TODO:
* o Optimize
* o Add compressor (This is a lot of work!)
}

{Provides a compressor (TODO) and decompressor for the deflate compression format. (See RFC 1951 for a detailed specification)}
unit AdDeflate;

interface

uses
  SysUtils, Classes, AdDeflateConsts, AdBuffer;

const
	WINDOWSIZE = 32768; //This must be 32768 or higher according to RFC 1951

type
	EAdDeflateUnsupportedFormatError = class(Exception);
	EAdDeflateInvalidDataException = class(Exception);

  TAdDeflateCode = record
    Value, Bits: Cardinal;
  end;

  PAdDeflateCode = ^TAdDeflateCode;


  TAdDeflateCodes = array of TAdDeflateCode;

  PAdDeflateCodes = ^TAdDeflateCodes;


  TAdDeflateCodeTable = record
    MaxBits: Cardinal;
	  Codes: array of PAdDeflateCode;
  end;


	TAdDeflateDecompressor = class
  private
    FWindow: array of Byte;
    FWindowPos: Integer;

    FBitBuffer: LongWord;
    FBitsLoaded: Cardinal;

    FLengthCodeTable, FLitCodeTable, FDistCodeTable: TAdDeflateCodeTable;
    FLengthCodes, FLitCodes, FDistCodes: TAdDeflateCodes;

    function ReadBits(const AStream: TStream; ACount: Cardinal): Cardinal;
		function ReadBitsReverse(const AStream: TStream;
			ACount: Cardinal): Cardinal;
    function ReadWord(const AStream: TStream): Cardinal;
    procedure ClearBits;

    function GetFixedCode(const AStream: TStream): Cardinal;
    procedure ProcessStaticBlock(const Input, Output: TStream);
    procedure ProcessDynamicBlock(const Input, Output: TStream);

    procedure InitCodeTables;
    procedure FreeCodeTables;
  	procedure ReadCodeTables(const AStream: TStream);
		procedure CreateCodeTable(var ACodes:
			TAdDeflateCodes; var ATable: TAdDeflateCodeTable; const ACount: Cardinal);
    function ReadEncodedValue(const AStream: TStream;
    	const ATable: TAdDeflateCodeTable): Cardinal;
    procedure ReadEncodedTable(const AStream: TStream;
   	 	var ACodes: TAdDeflateCodes; var ATable: TAdDeflateCodeTable;
      const ACount: Cardinal);

    procedure Write(const Input, Output: TStream; const Size: Integer);
		procedure WriteByte(const Output: TStream; const Data: Byte);
		procedure CopyFromWindow(const Output: TStream;
			const Distance, Length: Integer);

  public
    procedure Decompress(const Input, Output: TStream);
  end;


implementation

{ TAdDeflate }

procedure TAdDeflateDecompressor.ClearBits;
begin
  FBitBuffer:=0;
  FBitsLoaded:=0;
end;

procedure TAdDeflateDecompressor.CreateCodeTable(var ACodes:
	TAdDeflateCodes; var ATable: TAdDeflateCodeTable; const ACount: Cardinal);
var I, J: Integer;
  	c, MaxBits: Cardinal;
    blcounts, nextcodes: array of Cardinal;
begin
  MaxBits:=0;

  for I := 0 to ACount-1 do
    if ACodes[I].Bits>MaxBits then
      MaxBits:=ACodes[I].Bits;

  ATable.MaxBits:=MaxBits;

  setlength(blcounts, MaxBits+1);
  setlength(nextcodes, MaxBits+1);

  for I := 0 to ACount-1 do
    inc(blcounts[ACodes[I].Bits]);

  blcounts[0]:=0;
  c:=0;

  for I := 1 to MaxBits do
  begin
    c:=(c+blcounts[I-1]) shl 1;
    nextcodes[I]:=c;
  end;

	for I := 0 to ACount-1 do
  begin
   	c:=ACodes[I].Bits;
	  if c>0 then
    begin
      ACodes[I].Value:=I;
      for J := nextcodes[c] shl (MaxBits-c) to
      	(nextcodes[c] shl (MaxBits-c)) or DEFLATE_BITMASKS[MaxBits-c] do
        ATable.Codes[J]:=@ACodes[I];
      inc(nextcodes[c]);
   	end;
  end;

end;

function TAdDeflateDecompressor.GetFixedCode(const AStream: TStream): Cardinal;
begin
  Result:=ReadBits(AStream, 6);
  if Result>=$32 then
  	Result:=(Result shl 3) and $FF or ReadBits(AStream, 3)
  else if Result>=$30 then
    Result:=((Result shl 3) and $07 or ReadBits(AStream, 2))+$0118
  else if Result>=$0C then
    Result:=((Result shl 2) or ReadBits(AStream, 2))-$30
  else
  	Result:=((Result shl 1) or ReadBits(AStream, 1))+$0100;
end;

procedure TAdDeflateDecompressor.InitCodeTables;
begin

  SetLength(FLengthCodes, DEFLATE_MAXLENGTHCODES);
  SetLength(FLitCodes, DEFLATE_MAXLITS);
  SetLength(FDistCodes, DEFLATE_MAXDISTS);

  SetLength(FLengthCodeTable.Codes, 1 shl DEFLATE_LENGTHCODE_MAXBITS);
  SetLength(FLitCodeTable.Codes, 1 shl DEFLATE_LIT_MAXBITS);
  SetLength(FDistCodeTable.Codes, 1 shl DEFLATE_DIST_MAXBITS);

end;

procedure TAdDeflateDecompressor.Decompress(const Input, Output: TStream);
var c: Cardinal;
    len, nlen: Word;
    bfinal: Boolean;
    str: TStream;
begin
  str := Input;
  //Buffer the input stream for high speed read access
  QueryBufferedStream(str);

  try
    SetLength(FWindow, WINDOWSIZE);

    InitCodeTables;

    bfinal:=False;

    while (not bfinal) and (not (str.Position >= str.Size))  do
    begin
      bfinal:=ReadBits(str, 1)=1;
      c:=ReadBits(str, 2);

      case c of
        0:
        begin
          ClearBits;
          len:=ReadWord(str);
          nlen:=ReadWord(str);
          if len<>not nlen then
            raise EAdDeflateInvalidDataException.Create(
              'Invalid block in input.');
          Write(str, Output, len);
        end;

        1: ProcessDynamicBlock(str, Output);

        2: ProcessStaticBlock(str, Output);

        3: raise EAdDeflateUnsupportedFormatError.Create(
          'Unsupported compression type.');
      end;
    end;

    FreeCodeTables;

    if not bfinal then
      raise EAdDeflateInvalidDataException.Create('Unexpected end of input.');

    if FWindowPos>0 then
      Output.Write(FWindow[0], FWindowPos);

    SetLength(FWindow, 0);
  finally
    FreeBufferedStream(str);
  end;    
end;

procedure TAdDeflateDecompressor.ProcessDynamicBlock(const Input, Output: TStream);
var c: Cardinal;
    Length, Distance: Cardinal;
begin
  ReadCodeTables(Input);

  repeat
    c:=ReadEncodedValue(Input, FLitCodeTable);

    case c of
    0..255:
      WriteByte(Output, c);

    257..285:
    begin
      Length:=DEFLATE_LENGTHS[c-257]+ReadBitsReverse(Input,
      	DEFLATE_LENGTH_EXTRABITS[c-257]);
      c:=ReadEncodedValue(Input, FDistCodeTable);
      Distance:=DEFLATE_DISTS[c]+ReadBitsReverse(Input,
      	DEFLATE_DIST_EXTRABITS[c]);
      CopyFromWindow(Output, Distance, Length);
    end;

    286, 287:
    	raise EAdDeflateUnsupportedFormatError.Create(
        	'Unsupported length/literal code.');

    end;
  until c=256;

end;

procedure TAdDeflateDecompressor.ProcessStaticBlock(const Input, Output: TStream);
var c: Cardinal;
    Length, Distance: Cardinal;
begin

  repeat
	  c:=GetFixedCode(Input);
  	case c of
	  0..255:
      WriteByte(Output, c);

    257..285:
    begin
      Length:=DEFLATE_LENGTHS[c-257]+
      	ReadBitsReverse(Input, DEFLATE_LENGTH_EXTRABITS[c-257]);

      c:=ReadBits(Input, 5);
      if c in [30, 31] then
        raise EAdDeflateUnsupportedFormatError.Create(
        	'Unsupported distance code.');
      Distance:=DEFLATE_DISTS[c]+ReadBitsReverse(
      	Input, DEFLATE_DIST_EXTRABITS[c]);

      CopyFromWindow(Output, Distance, Length);
    end;

    286, 287:
    	raise EAdDeflateUnsupportedFormatError.Create(
        	'Unsupported length/literal code.');

  	end;
  until c=256;

end;

function TAdDeflateDecompressor.ReadBits(const AStream: TStream;
	ACount: Cardinal): Cardinal;
var
  b: Byte;
  count: Cardinal;
begin
	Result := 0;
  while ACount > 0 do
  begin
    if ACount > 8 then
    	count := 8
    else
      count := ACount;

	  if count > FBitsLoaded then
  	begin
      AStream.Read(b, 1);
	    FBitBuffer:=(FBitBuffer shl 8) or DEFLATE_REVERSE[b];
  	  inc(FBitsLoaded, 8);
	  end;

  	Result:=(Result shl count) or
    	(FBitBuffer shr (FBitsLoaded-count)) and DEFLATE_BITMASKS[count];
	  dec(FBitsLoaded, count);
    dec(ACount, count);
  end;
end;

function TAdDeflateDecompressor.ReadBitsReverse(const AStream: TStream;
	ACount: Cardinal): Cardinal;
var
  b: Byte;
  count, buffer: Cardinal;
  bitsinresult: Integer;
begin
	Result:=0;
  bitsinresult:=0;
  while ACount>0 do
  begin
    if ACount>8 then
      count:=8
    else
      count:=ACount;
    if count>FBitsLoaded then
    begin
      AStream.Read(b, 1);
      FBitBuffer:=(FBitBuffer shl 8) or DEFLATE_REVERSE[b];
      inc(FBitsLoaded, 8);
    end;
    buffer:=(DEFLATE_REVERSE[FBitBuffer shr (FBitsLoaded-count) and
    	DEFLATE_BITMASKS[count]]);
    if bitsinresult-8+Integer(count)>0 then
      buffer:=buffer shl (bitsinresult-8+Integer(count))
    else
      buffer:=buffer shr (-bitsinresult+8-Integer(count));
    Result:=Result or buffer;
    inc(bitsinresult, count);
    dec(FBitsLoaded, count);
    dec(ACount, count);
  end;
end;

procedure TAdDeflateDecompressor.ReadCodeTables(const AStream: TStream);
var I: Integer;
    lit, dist, clen: Cardinal;
begin
  lit:=(DEFLATE_REVERSE[ReadBits(AStream, 5)] shr 3)+257;
  dist:=(DEFLATE_REVERSE[ReadBits(AStream, 5)] shr 3)+1;
  clen:=(DEFLATE_REVERSE[ReadBits(AStream, 4)] shr 4)+4;

  for I := 0 to clen - 1 do
  	FLengthCodes[DEFLATE_DYN_CODELENGTH_INDICES[I]].Bits:=
			DEFLATE_REVERSE[ReadBits(AStream, 3)] shr 5;

  for I := clen to DEFLATE_MAXLENGTHCODES-1 do
    FLengthCodes[DEFLATE_DYN_CODELENGTH_INDICES[I]].Bits:=0;

  CreateCodeTable(FLengthCodes, FLengthCodeTable, DEFLATE_MAXLENGTHCODES);

  ReadEncodedTable(AStream, FLitCodes, FLengthCodeTable, lit);
  ReadEncodedTable(AStream, FDistCodes, FLengthCodeTable, dist);

  CreateCodeTable(FLitCodes, FLitCodeTable, lit);
  CreateCodeTable(FDistCodes, FDistCodeTable, dist);

end;

procedure TAdDeflateDecompressor.ReadEncodedTable(const AStream: TStream;
 	var ACodes: TAdDeflateCodes; var ATable: TAdDeflateCodeTable;
  const ACount: Cardinal);

var I, c: Cardinal;

	procedure RepeatLength(const Value, Offset, BitsOfLength: Cardinal);
	var J, Times: Cardinal;
	begin
  	J:=0;
	  Times:=DEFLATE_REVERSE[ReadBits(AStream, BitsOfLength)] shr (8-BitsOfLength);
  	while (J<Times+Offset) and (I<ACount) do
	  begin
  	  ACodes[I].Bits:=Value;
    	inc(I);
	    inc(J);
  	end;
	end;

begin
  I:=0;
  while I<ACount do
  begin
    c:=ReadEncodedValue(AStream, ATable);
    case c of
    	0..15:
      begin
      	ACodes[I].Bits:=c;
        inc(I);
      end;
      16: RepeatLength(ACodes[I-1].Bits, 3, 2);
      17: RepeatLength(0, 3, 3);
      18: RepeatLength(0, 11, 7);
    end;
  end;
end;

function TAdDeflateDecompressor.ReadEncodedValue(const AStream: TStream;
    	const ATable: TAdDeflateCodeTable): Cardinal;
var index, bitsread: Cardinal;
  	code: PAdDeflateCode;
begin

  bitsread:=ATable.MaxBits;
  index:=ReadBits(AStream, bitsread);
  code:=ATable.Codes[index];
  Result:=code^.Value;
  inc(FBitsLoaded, bitsread-code^.Bits);  

end;

function TAdDeflateDecompressor.ReadWord(const AStream: TStream): Cardinal;
var
  b: Byte;
begin
  AStream.Read(b, 1);
  result := b;
  AStream.Read(b, 1);
  result := result or (b shl 8);
end;

procedure TAdDeflateDecompressor.WriteByte(const Output: TStream; const Data: Byte);
begin
  FWindow[FWindowPos]:=Data;
  inc(FWindowPos);
  if FWindowPos=WINDOWSIZE then
  begin
    Output.Write(FWindow[0], WINDOWSIZE);
    FWindowPos:=0;
  end;
end;

procedure TAdDeflateDecompressor.Write(const Input, Output: TStream;
	const Size: Integer);
var
  I, Remaining, Bytes: Integer;
  b: Byte;
begin
  Remaining:=Size;

  while Remaining>0 do
  begin
    Bytes:=Remaining;
    if Bytes + FWindowPos > WINDOWSIZE then
      Bytes := WINDOWSIZE - FWindowPos;

		for I := 0 to Bytes-1 do
    begin
      Input.Read(b, 1);
      FWindow[FWindowPos+I] := b;
    end;

    inc(FWindowPos, Bytes);

    if FWindowPos=WINDOWSIZE then
    begin
      Output.Write(FWindow[0], WINDOWSIZE);
      FWindowPos:=0;
    end;

    dec(Remaining, Bytes);

  end;
    
end;

procedure TAdDeflateDecompressor.FreeCodeTables;
begin
  setlength(FLengthCodes, 0);
  setlength(FLitCodes, 0);
  setlength(FDistCodes, 0);

  setlength(FLengthCodeTable.Codes, 0);
  setlength(FLitCodeTable.Codes, 0);
  setlength(FDistCodeTable.Codes, 0);
end;

procedure TAdDeflateDecompressor.CopyFromWindow(const Output: TStream;
	const Distance, Length: Integer);
var I, Remaining, Offset, bytes: Integer;
begin
  Offset:=(FWindowPos-Distance+WINDOWSIZE) mod WINDOWSIZE;
  assert(Offset>=0);
  Remaining:=Length;
  while Remaining>0 do
  begin
    if FWindowPos+Remaining>=WINDOWSIZE then
    begin
      bytes:=WINDOWSIZE-FWindowPos;
	    for I:=0 to bytes-1 do
 	      FWindow[FWindowPos+I]:=
  	    	FWindow[(Offset+I) mod WINDOWSIZE];
      Offset:=(Offset+bytes) mod WINDOWSIZE;
      dec(Remaining, bytes);
      FWindowPos:=0;
      Output.Write(FWindow[0], WINDOWSIZE);
    end
    else
    begin
		  for I := 0 to Remaining-1 do
		  	FWindow[FWindowPos+I]:=FWindow[(Offset+I) mod WINDOWSIZE];
  	  inc(FWindowPos, Remaining);
    	Remaining:=0;
    end;
  end;
end;

end.
