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
*Filename: AdDLLExplorer.pas
*Comment: Class which lets you search for all Andorra 2D Plugin libraries in a specific directory
}

{Lets you search for all Andorra 2D Plugin libraries within a specific directory}
unit AdDLLExplorer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, {$IFDEF Win32}Windows{$ELSE}dynlibs{$ENDIF}, AdClasses;

type
  {A callback procedure for TAdDllExplorer}
  TAdDLLExplorerCallBack = procedure (DllFileName:string;DllInfo:TAd2DLibInfo;DllAbilities:TAd2DLibAbilities) of object;

  {A class, which lets you search for all Andorra 2D Plugin libraries within a specific directory.}
  TAdDLLExplorer = class
    private
      FStrings:TStrings;
      procedure StringsCallback(DllFileName:string;DllInfo:TAd2DLibInfo;DllAbilities:TAd2DLibAbilities);
    public
      {Returns all plugins in the StringList "Plugins" within the specific directory. Extension must include the trailing point. E.g. ".so" or ".dll"}
      procedure GetPlugins(Plugins:TStrings; Dir, Extension:string);overload;
      {Handles out all plugins via a callback within the specific directory. Extension must include the trailing point. E.g. ".so" or ".dll"}
      procedure GetPlugins(CallBack:TAdDllExplorerCallBack; Dir, Extension:String);overload;
  end;

implementation

{ TAdDLLExplorer }

procedure TAdDLLExplorer.GetPlugins(Plugins: TStrings; Dir, Extension: string);
begin
  FStrings := Plugins;
  GetPlugins(StringsCallback, Dir, Extension);
end;

procedure TAdDLLExplorer.GetPlugins(CallBack: TAdDllExplorerCallBack; Dir,
  Extension: String);
var
  searchrec:TSearchRec;
  res:integer;
  ahandle:THandle;
  fileinfo:TAndorra2DLibraryInformation;
  fileabilities:TAndorra2DLibraryAbilities;
  info:TAd2DLibInfo;
  abilities:TAd2DLibAbilities;
begin
  res := FindFirst(dir+'*'+Extension,faAnyFile, searchrec);
  ahandle := 0;
  while (res = 0) do
  begin
    try
      {$IFDEF Win32}
        ahandle := Windows.LoadLibrary(PChar(dir+searchrec.Name));
      {$ELSE}
        ahandle := dynlibs.LoadLibrary(PChar(dir+searchrec.Name));
      {$ENDIF}

      @fileinfo := GetProcAddress(ahandle, 'Andorra2DLibraryInformation');

      //If procedure exists, it must be an Andorra 2D Plugin Library
      if @fileinfo <> nil then
      begin
        //Read fileinfo
        fileinfo(info);

        //The library must be compatible
        if info.LibVersion = LibraryVersion then
        begin
          //Read abilities
          @fileabilities := GetProcAddress(ahandle, 'Andorra2DLibraryAbilities');
          fileabilities(abilities);

          CallBack(searchrec.Name,info,abilities);
        end;
      end;

    finally
      FreeLibrary(ahandle);
    end;
    res := FindNext(searchrec);
  end;
  FindClose(searchrec.FindHandle);
end;

procedure TAdDLLExplorer.StringsCallback(DllFileName: string;
  DllInfo: TAd2DLibInfo; DllAbilities:TAd2DLibAbilities);
begin
  FStrings.Add(DllInfo.LibTitle+'='+DllFileName);
end;

end.
