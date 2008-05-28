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
* File: AdTar.pas
* Comment: Provides a class for loading and saving (TODO) tar files.
*
* TODO:
* o Add support for saving archives
* o Add support for links and file rights (not a big deal, but not too
*   important either) 
}
unit AdTar;

interface

uses Classes, SysUtils, AdFileClasses, AdTypes;

type
	EAdTarInvalidFileException = class(Exception);
  EAdTarTooLongStringException = class(Exception);
  EAdTarUnsupportedFormatError = class(Exception);
  EAdTarFileTooBigError = class(Exception);

  TAdTarHeader = packed record
    Name: array[1..100] of Char;
    Mode: array[1..8] of Char;
    UID, GID: array[1..8] of Char;
    Size, Modified: array[1..12] of Char;
    Checksum: array[1..8] of Char;
    Link: Char;
    LinkName: array[1..100] of Char;
  end;

  PAdTarHeader = ^TAdTarHeader;

  TAdUSTARHeader = packed record
    Magic: array[1..6] of Char;
    Version: array[1..2] of Char;
    UName, GName: array[1..32] of Char;
    DevMajor, DevMinor: array[1..8] of Char;
    Prefix: array[1..155] of Char;
  end;

  PAdUSTARHeader = ^TAdUSTARHeader;

	TAdTarBall = class(TAdPackage)
  private
    class procedure StaticStrToDynStr(const str1: String; var str2: String);
    class procedure DynStrToStaticStr(const str1: String; var str2;
    	const Length: Integer);
    class procedure ZeroStr(const str; const Length: Integer);

    class function IntToOct(const Value: Cardinal): String;
		class function IntToOct64(const Value: Int64): String;
		class function OctToInt(const Str: String): Cardinal;
		class function OctToInt64(const Str: String): Int64;

    class function GetChecksum(const Buffer): Cardinal;

    procedure SaveFileToStream(AStream: TStream;
    	AFile: TAdFile; const Path: String);
    procedure SaveDirectoryToStream(AStream: TStream;
    	ADirectory: TAdDirectory; const Path: String);

  public

    class function CanOpen(const AStream: TStream): Boolean; override;
  	class function GetPackageFormatName: String; override;
		class function ID: TAdVeryShortString; override;

    procedure LoadFromStream(const AStream: TStream;
    	const AFileName: String=''); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

implementation

{ TAdTarBall }

class function TAdTarBall.CanOpen(const AStream: TStream): Boolean;
var Header: PAdTarHeader;
    Checksum: Cardinal;
    Buffer: array[0..511] of Byte;
begin
	if AStream.Size<=AStream.Position then
  begin
    Result:=True;
    exit;
  end;

	AStream.Read(Buffer[0], 512);

  Header:=PAdTarHeader(@Buffer[0]);

 	Checksum:=OctToInt(Header.Checksum);
  Result:=Checksum=GetChecksum(Buffer[0]);
end;

class procedure TAdTarBall.DynStrToStaticStr(const str1: String; var str2;
  const Length: Integer);
type
	TByteArray=array of Byte;
  PByteArray=^TByteArray;
var I: Integer;
  	p: PByteArray;
begin
  if System.length(str1)>Length then
    raise EAdTarTooLongStringException.CreateFmt(
    	'String ''%s'' too long for tar format.', [str1]);
	p:=PByteArray(@str2);
  for I := 1 to System.length(str1) do
    p^[I]:=Byte(str1[I]);
  for I := System.length(str1)+1 to Length do
    p^[I]:=0;
end;

class function TAdTarBall.GetChecksum(const Buffer): Cardinal;
var
	I: Cardinal;
begin
  Result:=0;
  for I := 0 to 511 do                         
    if (I>=148) and (I<156) then
      inc(Result, 32)
    else
      inc(Result, PByte(Cardinal(@Buffer)+I)^);
end;

class function TAdTarBall.ID: TAdVeryShortString;
begin
  Result:=#2'TAR';
end;

class function TAdTarBall.GetPackageFormatName: String;
begin
  Result:='tar';
end;

class function TAdTarBall.IntToOct(const Value: Cardinal): String;
var I, log8: Cardinal;
  	v: Int64;
begin
  log8:=0;
  v:=Value;
  while v>0 do
  begin
    inc(log8);
    v:=v shr 3;
  end;

  setlength(Result, log8);
  v:=Value;
  for I := log8 downto 1 do
  begin
    Result[I]:=chr((v and $07)+48);
    v:=v shr 3;
  end;
end;

class procedure TAdTarBall.StaticStrToDynStr(const str1: String; var str2: String);
var I: Integer;
begin
  I:=1;
 	while (I<length(str1)) and (str1[I]<>#0) do
   	inc(I);
  str2:=copy(str1, 1, I-1);
end;

class function TAdTarBall.IntToOct64(const Value: Int64): String;
var I, log8: Cardinal;
  	v: Int64;
begin
  log8:=0;
  v:=Value;
  while v>0 do
  begin
    inc(log8);
    v:=v shr 3;
  end;

  setlength(Result, log8);
  v:=Value;
  for I := log8 downto 1 do
  begin
    Result[I]:=chr((v and $07)+48);
    v:=v shr 3;
  end;
end;

procedure TAdTarBall.LoadFromStream(const AStream: TStream;
	const AFileName: String='');
var Header: PAdTarHeader;
  	USTARHeader: PAdUSTARHeader;
    IsUSTAR: Boolean;
    FilePath, FileName, Prefix: String;
    Size: Int64;
    Checksum: Cardinal;
    directory: TAdDirectory;
    Buffer: array[0..511] of Byte;
    emptyblocks: Integer;
begin
  inherited LoadFromStream(AStream, AFileName);
  emptyblocks:=0;
  FRoot:=TAdDirectory.Create('', nil, FPath+PathDelim);
  while (AStream.Position<AStream.Size) and (emptyblocks<2) do
  begin
  	AStream.Read(Buffer[0], 512);

	  Header:=@Buffer[0];
  	USTARHeader:=@Buffer[257];

    IsUSTAR:=lowercase(USTARHeader.Magic)='ustar ';

	  StaticStrToDynStr(Header.Name, FilePath);

    if FilePath='' then
    begin
      inc(emptyblocks);
      continue;
    end;

    if IsUSTAR then
    begin
		  StaticStrToDynStr(USTARHeader.Prefix, Prefix);    
  	  FilePath:=Prefix+FilePath;
    end;
  	Size:=Int64(OctToInt64(Header.Size));

    if Header.Link in ['1', '2'] then
      raise EAdTarInvalidFileException.Create('Links are not supported.');

  	Checksum:=OctToInt(Header.Checksum);
	  if Checksum<>GetChecksum(Buffer[0]) then
  	  raise EAdTarInvalidFileException.CreateFmt('Invalid checksum in file: %s',
      	[FilePath]);

    directory:=CreatePath(FilePath, FileName);
    if FileName<>'' then
    begin
	    TAdFile.Create(directory, FileName, directory.Path+FileName,
      	AStream, Size);
    	if Size>512*1024*1024 then
      	raise EAdTarFileTooBigError.CreateFmt('File to big: %s (Size: %s)',
      		[FileName, FileSizeToStr(Size)]);

      if Size mod 512>0 then
		    AStream.Seek(512-(Size mod 512), soCurrent);

    end;
    
  end;                                            
end;

class function TAdTarBall.OctToInt(const Str: String): Cardinal;
var I: Integer;
begin
  I:=1;
  Result:=0;
  while (I<=length(Str)) and (Str[I]='0') do
    inc(I);
  while (I<=length(Str)) and (Str[I] in ['0'..'9']) do
  begin
    Result:=Cardinal((Result shl 3) or (Cardinal(ord(Str[I]))-48));
    inc(I);
  end;
end;

class function TAdTarBall.OctToInt64(const Str: String): Int64;
var I: Integer;
begin
  I:=1;
  Result:=0;
  while (I<=length(Str)) and (Str[I]='0') do
    inc(I);
  while (I<=length(Str)) and (Str[I] in ['0'..'9']) do
  begin
    Result:=(Result shl 3) or (ord(Str[I])-48);
    inc(I);
  end;
end;

procedure TAdTarBall.SaveDirectoryToStream(AStream: TStream;
  ADirectory: TAdDirectory; const Path: String);
{var
  I: Integer;
  newpath: String;}
begin
	//TODO

{  newpath:=Path+ADirectory.FName+'/';
  for I := 0 to ADirectory.FFiles.Count - 1 do
    SaveFileToStream(AStream, TAdFile(ADirectory.FFiles[I]), newpath);
  for I := 0 to ADirectory.FDirectories.Count - 1 do
    SaveDirectoryToStream(AStream, TAdDirectory(ADirectory.FDirectories[I]),
    	newpath);}
end;

procedure TAdTarBall.SaveFileToStream(AStream: TStream; AFile: TAdFile;
  const Path: String);
var Header: TAdTarHeader;
  	USTARHeader: TAdUSTARHeader;
  	FileName, Prefix: String;
begin
  //TODO

  FileName:=Path+FileName;
  if length(FileName)>100 then
  begin
    FileName:=copy(FileName, length(FileName)-99, 100);
    Prefix:=copy(FileName, 1, length(FileName)-100);
    if length(Prefix)>155 then
      raise EAdTarTooLongStringException.CreateFmt(
      	'Too long filename: %s', [Path]);
    DynStrToStaticStr(Prefix, USTARHeader.Prefix, 155);
  end;
  DynStrToStaticStr(FileName, Header.Name, 100);

  ZeroStr(Header.Mode, 8);
  ZeroStr(Header.UID, 8);
  ZeroStr(Header.GID, 8);
//  Header.Size:=IntToOct64(AFile.Size);
end;

procedure TAdTarBall.SaveToStream(const AStream: TStream);
const ZERO: Cardinal=0;
var I: Integer;
begin
  SaveDirectoryToStream(AStream, FRoot, '');
  for I := 0 to 255 do
  	AStream.Write(ZERO, 4);
end;

class procedure TAdTarBall.ZeroStr(const str; const Length: Integer);
type
	TByteArray=array of Byte;
  PByteArray=^TByteArray;
var I: Integer;
  	p: PByteArray;
begin
  p:=PByteArray(@str);
  for I := 1 to Length do
    p^[I]:=0;
end;

initialization
begin
  FileControl.RegisterPackageFormat(TAdTarBall);
end;

end.
