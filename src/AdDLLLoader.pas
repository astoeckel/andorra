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

uses 
  SysUtils, {$IFDEF Win32}Windows{$ELSE}dynlibs{$ENDIF}, AdClasses;

//This is the class which loads the plugin DLL
type TAdDllLoader = class
  private
    DllHandle:THandle;
  public
    //The function which creates the application from the DLL
    CreateApplication:TAdCreateApplicationProc;
    //Contains information about the loaded library
    LibInfo:TAd2DLibInfo;
    //Contains information about the abilities of the plugin
    LibAbilities:TAd2DLibAbilities;
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

constructor TAdDllLoader.Create;
begin
  inherited Create;
end;

destructor TAdDllLoader.Destroy;
begin
  UnLoadLibrary;
  inherited Destroy;
end;

function TAdDllLoader.LibraryLoaded:boolean;
begin
  result := DllHandle <> 0;
end;

procedure TAdDllLoader.LoadLibrary(afile: string);
var
  InfoProc:TAndorra2DLibraryInformation;
  AbilitiesProc:TAndorra2DLibraryAbilities;
begin
  if fileExists(afile) then
  begin
    if LibraryLoaded then
    begin
      UnLoadLibrary;
    end;
    {$IFDEF Win32}
      DllHandle := Windows.LoadLibrary(PChar(afile));
    {$ELSE}
      DllHandle := dynlibs.LoadLibrary(PChar(afile));
    {$ENDIF}
    if LibraryLoaded then
    begin
      @CreateApplication := GetProcAddress(DllHandle, 'CreateApplication');

      //Get information
      @InfoProc := GetProcAddress(DllHandle, 'Andorra2DLibraryInformation');
      InfoProc(LibInfo);

      //Get abilities
      @AbilitiesProc := GetProcAddress(DllHandle, 'Andorra2DLibraryAbilities');
      AbilitiesProc(LibAbilities);
    end;
  end;
end;

procedure TAdDllLoader.UnLoadLibrary;
begin
  if LibraryLoaded then
  begin
    FreeLibrary(DllHandle);
  end;
end;

end.
