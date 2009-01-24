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
  SysUtils, Classes, {$IFDEF Win32}Windows{$ELSE}dynlibs{$ENDIF}, AdClasses,
  AdMessages;

{$i inc_andorra.inc}

{$IFDEF CPU64}
type
  THandle = int64;
{$ENDIF}

type
  {Exception that is raised when no plugin is found by the "DefaultPlugin" method.}
  EAdNoPluginFound = class(Exception);
  
  {A callback procedure for TAdDllExplorer}
  TAdDLLExplorerCallBack = procedure (DllFileName:string; DllInfo:TAd2DLibInfo) of object;

  {A class, which lets you search for all Andorra 2D Plugin libraries within a
   specific directory.}
  TAdDLLExplorer = class
    private
      FStrings:TStrings;
      procedure StringsCallback(DllFileName:string; DllInfo:TAd2DLibInfo);
    public
      {Returns all plugins in the StringList "Plugins" within the specific
       directory. Extension must include the trailing point. E.g. ".so" or ".dll".
       If the extension parameter is empty, a default extension will be used.}
      procedure GetPlugins(Plugins: TStrings; Dir: string;
        Extension: string='');overload;
      {Handles out all plugins via a callback within the specific directory.
       Extension must include the trailing point. E.g. ".so" or ".dll".
       If the extension parameter is empty, a default extension will be used.}
      procedure GetPlugins(CallBack: TAdDllExplorerCallBack; Dir: string;
        Extension: string='');overload;
      {Returns the first plugin that is found and that is loadable.}
      function DefaultPlugin(Dir: string = ''): string;
  end;

//Returns the first plugin library that has been found in the given directory
function DefaultPlugin: string;

implementation

const
  {$IFDEF WIN32}
    ext = '.dll';
  {$ELSE}
    ext = '.so';
  {$ENDIF}

{ TAdDLLExplorer }

procedure TAdDLLExplorer.GetPlugins(CallBack: TAdDllExplorerCallBack; Dir,
  Extension: String);
var
  searchrec:TSearchRec;
  res:integer;
  ahandle:THandle;
  fileinfo:TAndorra2DLibraryInformation;
  info:TAd2DLibInfo;
begin
  //Set the default extenstion, if the string was empty
  if Extension = '' then
    Extension := ext;
    
  res := FindFirst(dir+'*Andorra*'+Extension, faAnyFile, searchrec);
  ahandle := 0;
  while (res = 0) do
  begin
    try
      try
      {$IFDEF WIN32}
        ahandle := Windows.LoadLibrary(PChar(dir+searchrec.Name));
      {$ELSE}
        ahandle := dynlibs.LoadLibrary(PChar(dir+searchrec.Name));
      {$ENDIF}
      except
        //Load the next module if something didn't work...
        res := FindNext(searchrec);
        Continue;
      end;

      if AHandle <> 0 then
      begin
        @fileinfo := GetProcAddress(ahandle, 'Andorra2DLibraryInformation');

        //If procedure exists, it must be an Andorra 2D Plugin Library
        if @fileinfo <> nil then
        begin
          //Read fileinfo
          fileinfo(info);

          //The library must be compatible
          if info.LibVersion = LibraryVersion then
            //Call callback and pass name and information
            CallBack(dir + ExtractFileName(searchrec.Name), info);
        end;
      end;

    finally
      if AHandle <> 0 then
        FreeLibrary(AHandle);
    end;
    res := FindNext(searchrec);
  end;
  {$IFNDEF WIN32}
  FindClose(searchrec);
  {$ELSE}
  FindClose(searchrec.FindHandle);
  {$ENDIF}
end;

procedure TAdDLLExplorer.GetPlugins(Plugins: TStrings; Dir, Extension: string);
begin
  FStrings := Plugins;
  GetPlugins(StringsCallback, Dir, Extension);
end;

procedure TAdDLLExplorer.StringsCallback(DllFileName: String;
  DllInfo: TAd2DLibInfo);
begin
  FStrings.Add(DllInfo.LibTitle+'='+DllFileName);
end;

function TAdDLLExplorer.DefaultPlugin(Dir: string): string;
var
  lst: TStringList;
begin
  //Create a string list
  lst := TStringList.Create;

  //Set the search directory
  if Dir = '' then
    Dir := ExtractFilePath(ParamStr(0));

  //Get a list of all plugins in the given directory
  GetPlugins(lst, Dir);

  if lst.Count > 0 then
    result := lst.ValueFromIndex[0]
  else
    raise EAdNoPluginFound.Create(MsgSetupNoPluginsFound);

  lst.Free;
end;

function DefaultPlugin: string;
var
  expl: TAdDLLExplorer;
begin
  //Create a temporary instance of TAdDLLExplorer
  expl := TAdDLLExplorer.Create;
  try
    //Return the result of the TAdDLLExplorer default plugin function
    result := expl.DefaultPlugin;
  finally
    //Free the instance of TAdDLLExplorar
    expl.Free;
  end;
end;

end.
