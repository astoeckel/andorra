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
* File: AdFileClasses.pas
* Comment: Provides some classes to handle virtual file systems, especially
* with packages. For packages, see also TAdFileStreamEx in AdFileStreamEx.pas.
*
* TODO:
* o Add new functionality, if needed
}

{Provides some classes to handle virtual file systems, especially with packages.
 For packages, see also TAdFileStreamEx in AdFileStreamEx.pas.}
unit AdFileClasses;

interface

uses SysUtils, Classes, AdContainers, AdTypes {$IFDEF Win32}, Windows{$ENDIF};

const
  PATHDELIMS = ['/', '\'];

type
  EAdFileLockedException = class(Exception);
  EAdFileStreamReadOnlyException = class(Exception);
  EAdUnknownPackageFormatException = class(Exception);
  EAdUnknownCompressionException = class(Exception);

  TAdDirectory=class;
  TAdFile=class;


  {A file stream which is returned by TAdFile.GetStream. It is always linked
   to a specific virtual file.}
  TAdFileMemStream = class(TMemoryStream)
  private
  	FReadOnly: Boolean;
    FFile: TAdFile;
  public
    constructor Create(const AFile: TAdFile;
    	const ReadOnly: Boolean); reintroduce;
    function Write(const Buffer; Count: LongInt): LongInt; override;
    destructor Destroy; override;
    property ReadOnly: Boolean read FReadOnly;
  end;

  {A virtual file in a virtual file system. Its data can be read by using
   the stream returned by GetStream. Note that every trial to get a
   non-read-only stream will throw an exception if there are any other streams
   reading or writing. Trying to get a read-only stream when a stream is
   writing will also result in an exception.}
  TAdFile=class
  private
    FFileName, FPath, FPackagePath: String;
    FSize: Int64;
    FContent: Pointer;
    FDirectory: TAdDirectory;
    FLocks: Integer;
    FWriting: Boolean;
    procedure Lock;
    procedure Unlock;
    function IsLockedForWriting: Boolean;
  public
    constructor Create(const Directory: TAdDirectory;
    	const FileName, Path: String; const AStream: TStream; const Size: Int64);
    destructor Destroy; override;

    function GetStream(const ReadOnly: Boolean=True): TStream;

    property FileName: String read FFileName;
    property Path: String read FPath;
    property PackagePath: String read FPackagePath;
    property Size: Int64 read FSize;
    property Directory: TAdDirectory read FDirectory;
    property Locked: Boolean read FWriting;
    property LockedForWriting: Boolean read IsLockedForWriting;
  end;

  {A virtual directory used in virtual file systems. Its only purpose is,
   more or less, to get a file in it or one of its subdirectories.}
  TAdDirectory=class
  private
    FName, FPath, FPackagePath: String;
    FParent: TAdDirectory;
    FFiles, FDirectories: TList;
    FFileLocks: Integer;
    function GetFile(const Index: Integer): TAdFile;
    function GetDirectory(const Index: Integer): TAdDirectory;
    function GetFileByName(const FileName: String): TAdFile;
    function GetDirectoryByName(const Name: String): TAdDirectory;
  public
    constructor Create(const Name: String;
    	const Parent: TAdDirectory; const Path: String = ''); reintroduce;
    destructor Destroy; override;
    procedure AddFile(AFile: TAdFile);
    procedure RemoveFile(const FileName: String);
    procedure AddDirectory(ADirectory: TAdDirectory);
    procedure RemoveDirectory(const Name: String);

    procedure AddFileLock;
    procedure RemoveFileLock;

    function CountFiles: Integer;
    function CountDirectories: Integer;

    property Name: String read FName;
    property Path: String read FPath;
    property PackagePath: String read FPackagePath;
    property Parent: TAdDirectory read FParent;
    property FileLocks: Integer read FFileLocks;

    property Files[const Index: Integer]: TAdFile
    	read GetFile;
    property Directories[const Index: Integer]: TAdDirectory
    	read GetDirectory;
    property FilesByName[const FileName: String]: TAdFile
    	read GetFileByName;
    property DirectoriesByName[const Name: String]: TAdDirectory
    	read GetDirectoryByName;
  end;

	{A special map that allows the file controller to free all the packages
   when it is destroyed by providing a list of all its elements.}
  TAdPackageMap = class(TAdMap)
  public
		procedure AddToList(const AList: TAdLinkedList);
  end;

  {A key class for a map. It automatically adapts to the operating system's
   file system's handling of case sensitivity about file names and also
   expands the given file name. So, a relative and an absolute path that are
   actually the same would also be recognized as the same path.}
  TAdWrappedFileName = class(TAdMapKey)
  private
    FFileName: String;
  public
    constructor Create(const AFileName: String); reintroduce;
    function Hash: Cardinal; override;
    function Equal(AItem: TAdMapKey): Boolean; override;
    property FileName: String read FFileName;
  end;

  {An abstract class that provides compression and decompression methods.}
	TAdCompression = class
  public
    class function GetCompressionName: String; virtual; abstract;
    class function CanOpen(const AStream: TStream): Boolean; virtual; abstract;
    class function ID: TAdVeryShortString; virtual; abstract;

    procedure Compress(const Input, Output: TStream);
    	overload; virtual; abstract;
    procedure Compress(const Input: TStream; const OutputFileName: String);
			overload;
    procedure Compress(const InputFileName: String; const Output: TStream);
    	overload;
    procedure Compress(const InputFileName, OutputFileName: String);
    	overload;

    procedure Decompress(const Input, Output: TStream);
    	overload; virtual; abstract;
    procedure Decompress(const Input: TStream; const OutputFileName: String);
    	overload;
    procedure Decompress(const InputFileName: String; const Output: TStream);
    	overload;
    procedure Decompress(const InputFileName, OutputFileName: String);
    	overload;
  end;

  TAdCompressionClass = class of TAdCompression;

  {A package is an archive file like a tar file.
   When loaded, it provides a virtual file system through its property Root.
   Files and directories in the archive can be accessed by going though
   the directory tree of the file system.
   A package class has to be able to load and save a specific package
   (archive) file format and furthermore determine whether it can load an
   archive given in a stream (function CanOpen).}
  TAdPackage = class
  protected
    FRoot: TAdDirectory;
    FKey: TAdMapKey;
    FOwnsFiles, FAutoUnload: Boolean;
    FPath: String;
    procedure SetRoot(ARoot: TAdDirectory);
    function GetEmpty: Boolean;
    procedure SetEmpty(const Value: Boolean);
    function CreatePath(const Path: String;
    	var FileName: String): TAdDirectory;
  public
    class function CanOpen(const AStream: TStream): Boolean; virtual; abstract;
    class function GetPackageFormatName: String; virtual; abstract;
    class function ID: TAdVeryShortString; virtual; abstract;

    constructor Create(const AOwnsFiles: Boolean = True); reintroduce;
    destructor Destroy; override;

    procedure LoadFromStream(const AStream: TStream;
    	const AFileName: String=''); virtual;
    procedure LoadFromFile(const FileName: String); virtual;
    procedure SaveToStream(const AStream: TStream);	virtual; abstract;
    procedure SaveToFile(const FileName: String); virtual;

    property OwnsFiles: Boolean read FOwnsFiles write FOwnsFiles;
    property AutoUnload: Boolean read FAutoUnload write FAutoUnload;
    property Root: TAdDirectory read FRoot write SetRoot;
    property Empty: Boolean read GetEmpty write SetEmpty;
    property FileName: String read FPath;
  end;

  TAdPackageClass = class of TAdPackage;

  {The file control handles connections between virtual file systems and
   the physical file system, for example by providing methods for loading and
   unloading packages It also provides general methods for processing
   files such as compressing and decompressing and information about
   the operating system's file system, such as whether file names are
   case-sensitive.}
  TAdFileControl = class
  protected
    FPackages: TAdPackageMap;
    FRegisteredCompressions, FRegisteredPackageFormats: TAdLinkedList;
    FIgnoreCase: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function LoadPackage(const AFileName: String): TAdDirectory; overload;
    function LoadPackage(const AStream: TStream; const AFileName: String):
    	TAdDirectory; overload;
    function LoadPackage(const AFile: TAdFile): TAdDirectory; overload;

    procedure UnloadPackage(const APackage: TAdPackage); overload;
    procedure UnloadPackage(const AFileName: String); overload;

    procedure CheckDirectory(const AFileName: String);

    procedure RegisterCompression(const AClass: TAdCompressionClass);
    procedure RegisterPackageFormat(const AClass: TAdPackageClass);

    function Decompress(const Input, Output: TStream): Boolean;
    function OpenPackage(const AStream: TStream;
			const FileName: String): TAdPackage;

    function GetAbsolutePath(const APath: String): String;

    property IgnoreCase: Boolean read FIgnoreCase;

  end;

{Converts a file size in byte to a string (GB is the highest unit)}
function FileSizeToStr(const FileSize: Int64): String;
{Determines whether the given character is a path delimiter on any known platform.}
function IsPathDelimiter(const AChar: Char): Boolean;

var
	FileControl: TAdFileControl;

implementation

uses AdFileStreamEx;

function FileSizeToStr(const FileSize: Int64): String;
begin
  if FileSize=1 then
    Result:='1 byte'
  else if FileSize<1024 then
    Result:=Format('%d bytes', [FileSize])
  else if FileSize<1024*1024 then
    Result:=Format('%.3f KB', [FileSize/1024])
  else if FileSize<1024*1024*1024 then
    Result:=Format('%.3f MB', [FileSize/(1024*1024)])
  else
  	Result:=Format('%.3f GB', [FileSize/(1024*1024*1024)]);
end;

function IsPathDelimiter(const AChar: Char): Boolean;
begin
  Result:=AChar in PATHDELIMS;
end;

{ TAdFile }

constructor TAdFile.Create(const Directory: TAdDirectory;
    	const FileName, Path: String; const AStream: TStream; const Size: Int64);
begin
  Directory.AddFile(Self);
  FDirectory:=Directory;
  FFileName:=FileName;
  FPath:=Path;
  FPackagePath:=Directory.PackagePath+FileName;
  FSize:=Size;
  GetMem(FContent, Size);
  AStream.Read(FContent^, Size);
end;

destructor TAdFile.Destroy;
begin
  FreeMem(FContent, Cardinal(FSize));
  inherited Destroy;
end;

function TAdFile.GetStream(const ReadOnly: Boolean=True): TStream;
begin
  if (FLocks>0) and not ReadOnly then
    raise EAdFileLockedException.CreateFmt(
    	'The file %s is locked for writing.', [FFileName])
  else if FWriting then
    raise EAdFileLockedException.CreateFmt(
    	'The file %s is locked.', [FFileName])
  else
    Result:=TAdFileMemStream.Create(Self, ReadOnly);
end;

function TAdFile.IsLockedForWriting: Boolean;
begin
  Result:=(FLocks<>0);
end;

procedure TAdFile.Lock;
begin
  inc(FLocks);
  Directory.AddFileLock;
end;

procedure TAdFile.Unlock;
begin
  dec(FLocks);
  Directory.RemoveFileLock;
end;

{ TAdFileMemStream }

constructor TAdFileMemStream.Create(const AFile: TAdFile;
	const ReadOnly: Boolean);
begin
  SetPointer(AFile.FContent, AFile.Size);
	AFile.Lock;
  FFile:=AFile;
  FReadOnly:=ReadOnly;
end;

destructor TAdFileMemStream.Destroy;
begin
  FFile.Unlock;
  if not FReadOnly then
    FFile.FWriting:=False;
  inherited Destroy;
end;

function TAdFileMemStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  if FReadOnly then
    raise EAdFileStreamReadOnlyException.CreateFmt(
      'Cannot write to a read only stream.', [FFile.FFileName])
  else
    Result:=inherited Write(Buffer, Count);
  FFile.FSize:=GetSize;
end;

{ TAdDirectory }

procedure TAdDirectory.AddDirectory(ADirectory: TAdDirectory);
begin
  if FDirectories.IndexOf(ADirectory)=-1 then
  	FDirectories.Add(ADirectory);
end;

procedure TAdDirectory.AddFile(AFile: TAdFile);
begin
  if FFiles.IndexOf(AFile)=-1 then
  	FFiles.Add(AFile);
end;

procedure TAdDirectory.AddFileLock;
begin
  inc(FFileLocks);
end;

function TAdDirectory.CountDirectories: Integer;
begin
  Result:=FDirectories.Count;
end;

function TAdDirectory.CountFiles: Integer;
begin
  Result:=FFiles.Count;
end;

constructor TAdDirectory.Create(const Name: String;
    	const Parent: TAdDirectory; const Path: String = '');
begin
  if FileControl.IgnoreCase then
	  FName:=AnsiLowerCaseFileName(Name)
  else
    FName:=Name;
  FDirectories:=TList.Create;
  FParent:=Parent;
  FFiles:=TList.Create;
  if assigned(Parent) then
  begin
		FPackagePath:=Parent.PackagePath+FName+PathDelim;
    FPath:=Parent.Path+FName+PathDelim
  end else
  begin
    FPackagePath:=PathDelim;
    FPath:=Path;
  end;
end;

destructor TAdDirectory.Destroy;
var I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do
    TObject(FFiles[I]).Free;
  FFiles.Free;

  for I := 0 to FDirectories.Count - 1 do
    TObject(FDirectories[I]).Free;
  FDirectories.Free;

  inherited Destroy;
end;

function TAdDirectory.GetDirectory(const Index: Integer): TAdDirectory;
begin
  Result:=FDirectories[Index];
end;

function TAdDirectory.GetDirectoryByName(const Name: String):
	TAdDirectory;
var I: Integer;
begin
  Result:=nil;
  for I := 0 to FDirectories.Count - 1 do
    if AnsiCompareFileName(TAdDirectory(FDirectories[I]).FName, Name)=0 then
    begin
      Result:=FDirectories[I];
      exit;
	  end;
end;

function TAdDirectory.GetFile(const Index: Integer): TAdFile;
begin
  Result:=FFiles[Index];
end;

function TAdDirectory.GetFileByName(const FileName: String): TAdFile;
var I: Integer;
begin
  Result:=nil;
  for I := 0 to FFiles.Count - 1 do
    if AnsiCompareFileName(TAdFile(FFiles[I]).FFileName, FileName)=0 then
    begin
      Result:=FFiles[I];
      exit;
	  end;
end;

procedure TAdDirectory.RemoveDirectory(const Name: String);
var I: Integer;
begin
  for I := 0 to FDirectories.Count - 1 do
    if TAdDirectory(FDirectories[I]).FName=Name then
    begin
      FDirectories.Delete(I);
      exit;
	  end;
end;

procedure TAdDirectory.RemoveFile(const FileName: String);
var I: Integer;
begin
  for I := 0 to FFiles.Count - 1 do
    if TAdFile(FFiles[I]).FFileName=Name then
    begin
      FFiles.Delete(I);
      exit;
	  end;
end;

procedure TAdDirectory.RemoveFileLock;
begin
  dec(FFileLocks);
  if assigned(FParent) then
    FParent.RemoveFileLock
  else if FFileLocks=0 then
    FileControl.CheckDirectory(FPath);
end;

{ TAdPackage }

constructor TAdPackage.Create(const AOwnsFiles: Boolean = True);
begin
  inherited Create;
  FOwnsFiles:=AOwnsFiles;
end;

function TAdPackage.CreatePath(const Path: String;
  var FileName: String): TAdDirectory;
var pos, start: Integer;
    directory: TAdDirectory;
    name: String;
begin
  start:=1;
  Result:=FRoot;
  for pos := 1 to length(Path) do
  begin
    if IsPathDelimiter(Path[pos]) then
    begin
      if pos>start then
      begin
        name:=copy(Path, start, pos-start);
        directory:=Result.GetDirectoryByName(name);
        if not assigned(directory) then
        begin
	        directory:=TAdDirectory.Create(copy(Path, start, pos-start), Result);
    	    Result.AddDirectory(directory);
        end;
        Result:=directory;
      end;
      start:=pos+1;
    end;
  end;

  FileName:=copy(Path, start, length(Path));
end;

destructor TAdPackage.Destroy;
begin
  FKey.Free;
  inherited Destroy;
end;

function TAdPackage.GetEmpty: Boolean;
begin
  Result:=not assigned(FRoot);
end;

procedure TAdPackage.LoadFromFile(const FileName: String);
var fs: TFileStream;
begin
	fs:=TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
  	LoadFromStream(fs, FileName);
  finally
    fs.Free;
  end;
end;

procedure TAdPackage.LoadFromStream(const AStream: TStream;
  const AFileName: String);
begin
  FPath:=FileControl.GetAbsolutePath(AFileName);
end;

procedure TAdPackage.SaveToFile(const FileName: String);
var fs: TFileStream;
begin
	fs:=TFileStream.Create(FileName, fmCreate, fmShareDenyWrite);
  try
  	SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAdPackage.SetEmpty(const Value: Boolean);
begin
  if not Value and assigned(FRoot) then
  	FreeAndNil(FRoot);
end;

procedure TAdPackage.SetRoot(ARoot: TAdDirectory);
begin
  if FRoot=ARoot then
  	exit;

  if assigned(FRoot) then
    if FOwnsFiles then
      FRoot.Free;
  FRoot:=ARoot;
end;

{ TAdWrappedFileName }

constructor TAdWrappedFileName.Create(const AFileName: String);
begin
  inherited Create;
  FFileName:=FileControl.GetAbsolutePath(AFileName);
end;

function TAdWrappedFileName.Equal(AItem: TAdMapKey): Boolean;
begin
  Result:=FFileName=TAdWrappedFileName(AItem).FileName;
end;

function TAdWrappedFileName.Hash: Cardinal;
var I, shift: Integer;
begin
  Result:=0;
  shift:=0;
  for I := 1 to length(FFileName) do
  begin
    Result:=Result xor (Ord(FFileName[I]) shl shift);
    inc(shift, 5);
    if shift>27 then
      shift:=0;
  end;
end;

{ TAdFileControl }

procedure TAdFileControl.CheckDirectory(const AFileName: String);
var key: TAdMapKey;
  	package: TAdPackage;
begin
  key:=TAdWrappedFileName.Create(AFileName);
  try
    package:=TAdPackage(FPackages.GetValue(key));
    if assigned(package) and package.AutoUnload then
	    FPackages.Remove(key);
  finally
    key.Free;
  end;
end;

constructor TAdFileControl.Create;
begin
  inherited Create;
  FIgnoreCase:=AnsiCompareFileName('A', 'a')=0;
  FPackages:=TAdPackageMap.Create;
  FRegisteredCompressions:=TAdLinkedList.Create;
  FRegisteredPackageFormats:=TAdLinkedList.Create;
end;

function TAdFileControl.Decompress(const Input, Output: TStream): Boolean;
var compressionclass: TAdCompressionClass;
  	compression: TAdCompression;
  	pos: Int64;
begin
  Result:=False;
	compressionclass:=nil;
  FRegisteredCompressions.StartIteration;
  pos:=input.Position;

  while not FRegisteredCompressions.ReachedEnd do
 	begin
   	compressionclass:=TAdCompressionClass(
     	FRegisteredCompressions.GetCurrent);
    if compressionclass.CanOpen(input) then
      break;
    input.seek(pos, soBeginning);
  end;

  if assigned(compressionclass) then
  begin
  	input.seek(pos, soBeginning);
    compression:=compressionclass.Create;
    try
      compression.Decompress(Input, Output);
      Result:=True;
    finally
      compression.Free;
    end;
  end;
end;

destructor TAdFileControl.Destroy;
var list: TAdLinkedList;
begin
	FRegisteredCompressions.Free;
  FRegisteredPackageFormats.Free;

  list:=TAdLinkedList.Create;
  try
  	FPackages.AddToList(list);
    list.StartIteration;
    while not list.ReachedEnd do
      TObject(list.GetCurrent).Free;
  finally
    list.Free;
  end;
  FPackages.Free;
  inherited Destroy;
end;

function TAdFileControl.GetAbsolutePath(const APath: String): String;
begin
  Result:=ExcludeTrailingPathDelimiter(ExpandFileName(APath));
  if FileControl.IgnoreCase then
  	Result:=AnsiLowerCaseFileName(Result);
end;

function TAdFileControl.LoadPackage(const AStream: TStream;
  const AFileName: String): TAdDirectory;
var key: TAdWrappedFileName;
  	package: TAdPackage;
    stream, tmpstream: TStream;
    pos: Int64;
begin
  key:=TAdWrappedFileName.Create(AFileName);
  package:=TAdPackage(FPackages.GetValue(key));
  if assigned(package) then
    key.Free
  else
  begin

      stream:=AStream;
      tmpstream:=TMemoryStream.Create;

      try
        pos:=stream.Position;
        
        if Decompress(stream, tmpstream) then
        begin
          stream:=tmpstream;
          stream.Seek(pos, soBeginning);
          tmpstream:=nil;
        end;

        tmpstream.Free;

        package:=OpenPackage(stream, AFileName);
        package.AutoUnload:=True;

	      if not assigned(package) then
  				raise EAdUnknownPackageFormatException.CreateFmt(
	  	 		'Package %s has an unknown format.', [AFileName]);

        package.FKey:=key;
		    FPackages.Insert(key, package);

      finally
      	if stream<>AStream then stream.Free;
      end;

  end;

  Result:=package.Root;
end;

function TAdFileControl.LoadPackage(const AFile: TAdFile): TAdDirectory;
var stream: TStream;
begin
  stream:=AFile.GetStream;
  try
	  Result:=LoadPackage(stream, AFile.Path);
  finally
    stream.Free;
  end;
end;

function TAdFileControl.LoadPackage(const AFileName: String): TAdDirectory;
var stream: TStream;
begin
	stream:=TAdFileStreamEx.Create(AFileName, fmOpenRead, fmShareDenyWrite);
  try
    Result:=LoadPackage(stream, AFileName);
  finally
  	stream.Free;
  end;
end;

function TAdFileControl.OpenPackage(const AStream: TStream;
	const FileName: String): TAdPackage;
var packageclass: TAdPackageClass;
  	pos: Int64;
begin
	packageclass:=nil;
	FRegisteredPackageFormats.StartIteration;
  pos:=AStream.Position;

 	while not FRegisteredPackageFormats.ReachedEnd do
  begin
 	 	packageclass:=TAdPackageClass(FRegisteredPackageFormats.GetCurrent);
	  if packageclass.CanOpen(AStream) then
   	  break;
	  AStream.Seek(pos, soBeginning);
 	end;

  if assigned(packageclass) then
  begin
	  AStream.Seek(pos, soBeginning);
	  Result:=packageclass.Create;
  	Result.LoadFromStream(AStream, FileName);
  end
  else
  	Result:=nil;
end;

procedure TAdFileControl.RegisterCompression(const AClass: TAdCompressionClass);
begin
  FRegisteredCompressions.Add(AClass);
end;

procedure TAdFileControl.RegisterPackageFormat(const AClass: TAdPackageClass);
begin
  FRegisteredPackageFormats.Add(AClass);
end;

procedure TAdFileControl.UnloadPackage(const APackage: TAdPackage);
begin
  UnloadPackage(APackage.FileName);
end;

procedure TAdFileControl.UnloadPackage(const AFileName: String);
var key: TAdMapKey;
begin
  key:=TAdWrappedFileName.Create(AFileName);
  try
    FPackages.Remove(key);
  finally
    key.Free;
  end;
end;

{ TAdPackageMap }

procedure TAdPackageMap.AddToList(const AList: TAdLinkedList);
var
  PItem:PAdMapPair;
  PList:PAdLinkedList;
  I: Integer;
begin
  getvalue(TAdWrappedFileName.Create('packagetest.tar.gz'));
  if assigned(Data) then
  begin
	  PList := Data;
	  for I := 0 to Capacity - 1 do
  	begin

		  with PList^ do
  		begin
    		StartIteration;
	  	  while not ReachedEnd do
  	  	begin
    	  	PItem := PAdMapPair(GetCurrent);
	        AList.Add(PItem^.Value);
		    end;
  		end;

	  	inc(PList);
	  end;
  end;
end;

{ TAdCompressedFile }

procedure TAdCompression.Compress(const Input: TStream;
  const OutputFileName: String);
var Output: TFileStream;
begin
  Output:=TFileStream.Create(OutputFileName, fmCreate, fmShareDenyWrite);
  try
    Compress(Input, Output);
  finally
  	Output.Free;
  end;
end;

procedure TAdCompression.Compress(const InputFileName: String;
  const Output: TStream);
var Input: TFileStream;
begin
  Input:=TFileStream.Create(InputFileName, fmOpenRead, fmShareDenyWrite);
  try
    Compress(Input, Output);
  finally
  	Input.Free;
  end;
end;

procedure TAdCompression.Compress(const InputFileName,
  OutputFileName: String);
var Input, Output: TFileStream;
begin
  Input:=TFileStream.Create(InputFileName, fmOpenRead, fmShareDenyWrite);
  Output:=TFileStream.Create(OutputFileName, fmCreate, fmShareDenyWrite);
  try
    Compress(Input, Output);
  finally
  	Input.Free;
    Output.Free;
  end;
end;

procedure TAdCompression.Decompress(const Input: TStream;
  const OutputFileName: String);
var Output: TFileStream;
begin
  Output:=TFileStream.Create(OutputFileName, fmCreate, fmShareDenyWrite);
  try
    Decompress(Input, Output);
  finally
  	Output.Free;
  end;
end;

procedure TAdCompression.Decompress(const InputFileName: String;
  const Output: TStream);
var Input: TFileStream;
begin
  Input:=TFileStream.Create(InputFileName, fmOpenRead, fmShareDenyWrite);
  try
    Decompress(Input, Output);
  finally
  	Input.Free;
  end;
end;

procedure TAdCompression.Decompress(const InputFileName,
  OutputFileName: String);
var Input, Output: TFileStream;
begin
  Input:=TFileStream.Create(InputFileName, fmOpenRead, fmShareDenyWrite);
  Output:=TFileStream.Create(OutputFileName, fmCreate, fmShareDenyWrite);
  try
    Decompress(Input, Output);
  finally
  	Input.Free;
    Output.Free;
  end;
end;

initialization
begin
  FileControl:=TAdFileControl.Create;
end;

finalization
begin
  FileControl.Free;
end;

end.
