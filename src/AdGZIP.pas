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
* File: AdGZIP.pas
* Comment: Provides a class for loading and saving gzip files (see RFC 1952)
* for a detailed specification)
*
* TODO:
* o Add support to save GZIP files (before that, a compressor for deflate
*   (TAdDeflateCompressor) has to be implemented in AdDeflate.pas.
}

{Provides a class for loading and saving gzip files (see RFC 1952)}
unit AdGZIP;

interface

uses Classes, SysUtils, AdCRC, AdDeflate, AdFileClasses, AdTypes;

type
	EAdGZIPUnsupportedFormatError = class(Exception);
  EAdGZIPInvalidFileException = class(Exception);

	TAdGZIPFlags = set of (gfText, gfHCRC, gfExtra, gfName, gfComment,
  		gfReserved1, gfReserved2, gfReserved3);

  TAdGZIPExtraFlags = (xfBest=2, xfFastest=4);

	TAdGZIPHeader = packed record
    ID1, ID2, CM: Byte;
    Flags: TAdGZIPFlags;
    MTime: Cardinal;
    XFL: TAdGZIPExtraFlags;
    OS: Byte;
  end;

  TAdGZIPHeaderState = (hsValid, hsUnsupported, hsInvalid);

  PAdGZIPHeader = ^TAdGZIPHeader;

	TAdGZIPFile = class(TAdCompression)
  private
  	FHeader: TAdGZIPHeader;
    FName, FComment: String;
    class procedure ReadNullTerminatedString(
    	const AStream: TStream; var Str: String);
    class function ReadHeader(const AStream: TStream;
			const AHeader: PAdGZIPHeader = nil; const AName: PString = nil;
		  const AComment: PString = nil): TAdGZIPHeaderState;
  public
    class function GetCompressionName: String; override;
		class function ID: TAdVeryShortString; override;
    class function CanOpen(const AStream: TStream): Boolean; override;

    procedure Compress(const Input, Output: TStream); override;
    procedure Decompress(const Input, Output: TStream); override;
  end;

implementation

class function TAdGZIPFile.CanOpen(const AStream: TStream): Boolean;
begin
  Result:=ReadHeader(AStream)=hsValid;
end;

procedure TAdGZIPFile.Compress(const Input, Output: TStream);
begin
  //TODO
end;

procedure TAdGZIPFile.Decompress(const Input, Output: TStream);
var start, pos: Int64;
    isize, crc32: Cardinal;
    Buffer: Pointer;
    Decompressor: TAdDeflateDecompressor;
begin

  case ReadHeader(Input, @FHeader, @FName, @FComment) of
    hsUnsupported:
    	raise EAdGZIPUnsupportedFormatError.Create('Unsupported file format.');
    hsInvalid:
	    raise EAdGZIPInvalidFileException.Create('Invalid CRC16 value.');
  end;

  start:=Output.Position;
  decompressor:=TAdDeflateDecompressor.Create;
  try
  	decompressor.Decompress(Input, Output);
  finally
  	decompressor.Free;
  end;
  pos:=Output.Position;

  Input.Read(crc32, 4);
  Input.Read(isize, 4);

  if isize<>pos-start then
    raise EAdGZIPInvalidFileException.Create(
    	'Specified and actual size are not equal.');


  Output.Seek(start, soBeginning);
  GetMem(Buffer, pos-start);
  Output.Read(Buffer^, pos-start);
  if crc32<>AdCRC.CRC32(Buffer^, pos-start) then
  begin
    FreeMem(Buffer);
    raise EAdGZIPInvalidFileException.Create('Invalid CRC32 value.');
  end;
  FreeMem(Buffer);
  Output.Seek(pos+8, soBeginning);

end;

class function TAdGZIPFile.GetCompressionName: String;
begin
  Result:='gzip';
end;

class function TAdGZIPFile.ID: TAdVeryShortString;
begin
  Result:='GZIP';
end;

class function TAdGZIPFile.ReadHeader(const AStream: TStream;
	const AHeader: PAdGZIPHeader = nil; const AName: PString = nil;
  const AComment: PString = nil): TAdGZIPHeaderState;
var start, pos: Int64;
    Header: PAdGZIPHeader;
    wordbuf, crc16: Word;
    Buffer: Pointer;
    str: String;
begin
	start:=AStream.Position;

  if assigned(AHeader) then
    header:=AHeader
  else
    New(Header);

  AStream.Read(Header^, sizeof(TAdGZIPHeader));

  if (Header^.ID1<>31) or (Header^.ID2<>139) or (Header^.CM<>8) then
  begin
    Result:=hsUnsupported;
    exit;
  end;

  if gfExtra in Header^.Flags then
  begin
    AStream.Read(wordbuf, 2);
    AStream.Seek(wordbuf, soCurrent);
  end;

  if gfName in Header^.Flags then
  begin
  	ReadNullTerminatedString(AStream, str);
    if assigned(AName) then
    	AName^:=str;
  end;

  if gfComment in Header^.Flags then
  begin
  	ReadNullTerminatedString(AStream, str);
    if assigned(AComment) then
    	AComment^:=str;
  end;

  if gfHCRC in Header^.Flags then
  begin
    AStream.Read(crc16, 2);
	  pos:=AStream.Position;
    AStream.Seek(start, soBeginning);
	  GetMem(Buffer, Integer(pos-start-2));
    AStream.Read(Buffer^, pos-start-2);
  	if (AdCRC.CRC32(Buffer^, pos-start-2) and $FFFF)<>crc16 then
    begin
      FreeMem(Buffer);
      Result:=hsInvalid;
      exit;
    end;
	  FreeMem(Buffer);
    AStream.Seek(pos, soBeginning);
  end;
  
  Result:=hsValid;
end;

class procedure TAdGZIPFile.ReadNullTerminatedString(const AStream: TStream;
  var Str: String);
var c: Char;
  	start: Int64;
begin
  start:=AStream.Position;
  repeat
    AStream.Read(c, 1);
  until c=#0;
  setlength(Str, AStream.Position-start-1);
  AStream.Seek(start, soBeginning);  
  AStream.Read(Str[1], length(Str));
  AStream.Seek(1, soCurrent);
end;

initialization
begin
  FileControl.RegisterCompression(TAdGZIPFile);
end;

end.
