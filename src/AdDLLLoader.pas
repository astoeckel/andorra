{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and 
* the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Author: Andreas Stöckel
* Filename: AdDLLLoader.pas
* Comment: Contains a class that loads the Plugin-DLL
}

{ Contains the class that cares of loading the graphic plugin library. }
unit AdDLLLoader;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses 
  SysUtils, {$IFDEF Win32}Windows{$ELSE}dynlibs{$ENDIF},
  AdClasses, AdShaderClasses, AdMessages;

{$IFDEF CPU64}
type
  THandle = int64;
{$ENDIF}

type
  {Exception class used for exceptions that occur in the dll loader class.}
  EAdDllLoaderException = class(Exception);
  {Raised when the specified graphic library is incompatible to the current
   Andorra 2D version.}
  EAdDllIncompatible = class(EAdDllLoaderException);

  {Cares of loading the plugin dll.}
  TAdDllLoader = class
    private
      DllHandle:THandle;
    public
      //Creates the TAd2dApplication interface.
      CreateApplication:TAdCreateApplicationProc;

      //Creates the TAd2dShaderSystem interface.
      CreateShaderSystem:TAdCreateShaderProc;

      //Contains information about the loaded library
      LibInfo:TAd2DLibInfo;
      //Function that is used to receive information about the plugin. @seealso(TAd2dProperty)
      LibProperties:TAndorra2DApplicationProperties;
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
  InfoProc: TAndorra2DLibraryInformation;
begin
  if FileExists(afile) then
  begin
    //If a library is already loaded, unload it
    if LibraryLoaded then
      UnLoadLibrary;

    {$IFDEF Win32}
      DllHandle := Windows.LoadLibrary(PChar(afile));
    {$ELSE}
      DllHandle := dynlibs.LoadLibrary(PChar(afile));
    {$ENDIF}

    if LibraryLoaded then
    begin
      @CreateApplication := GetProcAddress(DllHandle, 'CreateApplication');

      @CreateShaderSystem := GetProcAddress(DllHandle, 'CreateShaderSystem');

      //Get information
      @InfoProc := GetProcAddress(DllHandle, 'Andorra2DLibraryInformation');

      if Assigned(InfoProc) then
        InfoProc(LibInfo)
      else
        raise EAdDllIncompatible.Create(MsgPluginInvalid);

      //Get library properties function
      @LibProperties := GetProcAddress(DllHandle, 'Andorra2DApplicationProperties');

      if LibInfo.LibVersion <> LibraryVersion then
      begin
        UnLoadLibrary;
        raise EAdDllIncompatible.CreateFmt(MsgPluginVersionIncompatible,
          [AFile, LibInfo.LibVersion, LibraryVersion]);
      end;
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
