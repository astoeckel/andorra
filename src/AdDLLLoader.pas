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
*Filename: AdDLLLoader.pas
*Comment: Loads the Plugin-DLL
}


//Loads the plugin DLL
unit AdDLLLoader;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses SysUtils, Windows, AdClasses;

//This is the class which loads the plugin DLL
type TAndorraDllLoader = class
  private
    DllHandle:THandle;
  public
    //The function which creates the application from the DLL
    CreateApplication:TAdCreateApplicationProc;
    //Loads the library
    procedure LoadLibrary(afile:string);
    //Unloads the library
    procedure UnLoadLibrary;
    //Returns whether the library is loaded
    function LibraryLoaded:boolean;
    //Creates an instance of TAndorraDLLLoader
    constructor Create;
    //Destroys the instance of TAndorraDLLLoader
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
