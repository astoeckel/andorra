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
* File: AdTar.pas
* Comment: Provides a class for loading and saving (TODO) tar files.
*
* TODO:
* o Add support for saving archives
* o Add support for links and file rights (not a big deal, but not too
*   important either) 
}
unit AdFileStreamEx;

interface

uses SysUtils, Classes, AdFileClasses;

type
	TAdFileStreamEx = class(TFileStream)
  private
    FStream: TStream;
  public
    constructor Create(const AFileName: string; Mode: Word); overload;
    constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal);
    	overload;
    destructor Destroy; override;

    function Read(var Buffer; Count: Integer): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;

		procedure SetSize(const NewSize: Int64); override;
		function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

implementation

{ TAdFileStreamEx }

constructor TAdFileStreamEx.Create(const AFileName: string; Mode: Word);
begin
{$IFDEF MSWINDOWS}
  Create(AFilename, Mode, 0);
{$ELSE}
  Create(AFilename, Mode, FileAccessRights);
{$ENDIF}
end;

constructor TAdFileStreamEx.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
var I, start: Integer;
  	path, name: String;
    directory, tmpdir: TAdDirectory;
    tmpfile: TAdFile;
begin
	directory:=nil;

  if FileExists(AFileName) then
    inherited Create(AFileName, Mode, Rights)
  else begin
    start:=1;
    I:=1;

    while I<=length(AFileName) do
    begin

      if IsPathDelimiter(AFileName[I]) then
      begin
        if I>start then
        begin

  		    if assigned(directory) then
	    	  begin
      		  name:=copy(AFileName, start, I-start);
    	    	tmpdir:=directory.DirectoriesByName[name];
	  	      if assigned(tmpdir) then
		          directory:=tmpdir
    	    	else
      		  begin
    	  	    tmpfile:=directory.FilesByName[name];
  	      	  if assigned(tmpfile) then
              begin
          	  	directory:=FileControl.LoadPackage(tmpfile)
              end else
     	          raise EFOpenError.CreateFmt('File not found: %s', [AFileName]);
  	    	  end;
	  	    end else if fileexists(path) then
          begin
            directory:=FileControl.LoadPackage(path);
          end
          else
            raise EFOpenError.CreateFmt('File not found: %s', [AFileName]);

        end
        else
          raise EFOpenError.CreateFmt('File not found: %s', [AFileName]);
        start:=I+1;
      end;

      path:=path+AFileName[I];
      inc(I);
    end;

    if assigned(directory) then
    begin
			name:=copy(AFileName, start, length(AFileName));
  	  tmpfile:=directory.FilesByName[name];
    end
    else
    	tmpfile:=nil;

    if assigned(tmpfile) then
      FStream:=tmpfile.GetStream(Mode=fmOpenRead)
    else
      raise EFOpenError.CreateFmt('File not found: %s', [AFileName]);

  end;

end;

destructor TAdFileStreamEx.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TAdFileStreamEx.Read(var Buffer; Count: Integer): Integer;
begin
  if assigned(FStream) then
    Result:=FStream.Read(Buffer, Count)
  else
    Result:=inherited Read(Buffer, Count);
end;

function TAdFileStreamEx.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if assigned(FStream) then
    Result:=FStream.Seek(Offset, Origin)
  else
    Result:=inherited Seek(Offset, Origin);
end;

procedure TAdFileStreamEx.SetSize(const NewSize: Int64);
begin
  if assigned(FStream) then
    FStream.Size:=NewSize
  else
  	inherited SetSize(NewSize);
end;

function TAdFileStreamEx.Write(const Buffer; Count: Integer): Integer;
begin
  if assigned(FStream) then
    Result:=FStream.Write(Buffer, Count)
  else
    Result:=inherited Write(Buffer, Count);
end;

end.
