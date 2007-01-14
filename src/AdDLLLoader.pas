{
* This program is licensed under the to Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations
*
*Author: Andreas Stöckel
*Filename: AdDLLLoader.pas
*Comment: Loads the Plugin-DLL
}

unit AdDLLLoader;

interface

uses SysUtils,Windows,AdClasses;

type TAndorraDllLoader = class
  private
    DllHandle:THandle;
  public
    CreateApplication:TAdCreateApplicationProc;
    procedure LoadLibrary(afile:string);
    procedure UnLoadLibrary;
    function LibraryLoaded:boolean;
    constructor Create;
    destructor Destroy;override;
end;

implementation

constructor TAndorraDllLoader.Create;
begin
  inherited Create;
end;

destructor TAndorraDllLoader.Destroy;
begin
  UnLoadLibrary;
  inherited Destroy;
end;

function TAndorraDllLoader.LibraryLoaded:boolean;
begin
  result := DllHandle <> 0;
end;

procedure TAndorraDllLoader.LoadLibrary(afile: string);
begin
  if fileExists(ExtractFilePath(ParamStr(0))+afile) then
  begin
    if LibraryLoaded then
    begin
      UnLoadLibrary;
    end;
    DllHandle := Windows.LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+afile));
    if LibraryLoaded then
    begin
      @CreateApplication := GetProcAddress(DllHandle, 'CreateApplication');
    end;
  end;
end;

procedure TAndorraDllLoader.UnLoadLibrary;
begin
  if LibraryLoaded then
  begin
    FreeLibrary(DllHandle);
  end;  
end;

end.
