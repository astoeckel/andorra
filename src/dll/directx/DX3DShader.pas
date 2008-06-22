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
* Author:  Andreas Stoeckel
* File: DX3DShader.pas
* Comment: Adds the ability of loading shaders into the DirectX-Plugin
}

unit DX3DShader;

interface

uses
  Windows, AdTypes, AdClasses, AdShaderClasses, AdContainers,
  DX3DShaderClasses, DX3DMain, DX3DHLSL;

type
  TDXShaderSystem = class(TAd2dShaderSystem)
    private
      FEngine: TDXShaderEngine;
      FAppl: TAd2dApplication;
      function LoadShaderPlugin(AID: TAdVeryShortString): boolean;
    protected
      function GetInitialized: boolean;override;
    public
      constructor Create;
      destructor Destroy;override;

      procedure Initialize(AApplication: TAd2dApplication);override;
      procedure Finalize;override;

      function CreateShader(AID: TAdVeryShortString): TAd2dShader;override;

      property AdAppl: TAd2dApplication read FAppl;
  end;

implementation

{ TDXShaderSystem }

constructor TDXShaderSystem.Create;
begin
  inherited;
end;

destructor TDXShaderSystem.Destroy;
begin
  Finalize;
  inherited;
end;

function TDXShaderSystem.CreateShader(AID: TAdVeryShortString): TAd2dShader;
begin
  result := nil;

  if (FEngine = nil) then
  begin
    if AID = 'hlsl' then
    begin
      FEngine := TDXHLSLEngine.Create;
      FEngine.ID := 'hlsl';
      Initialize(FAppl);
    end else
    begin
      //Load shader plugin and create shader engine
      if LoadShaderPlugin(AID) then
      begin
        //Initialize shader engine
        Initialize(FAppl);
      end;
    end;
  end;

  if (FEngine <> nil) and (FEngine.ID = AID) then
  begin
    result := FEngine.CreateShader;
  end;
end;

procedure TDXShaderSystem.Finalize;
begin
  if FEngine <> nil then
    FEngine.Finalize;
end;

function TDXShaderSystem.GetInitialized: boolean;
begin
  result := (FEngine <> nil) and (FEngine.Initialized);
end;

procedure TDXShaderSystem.Initialize(AApplication: TAd2dApplication);
begin
  //Only call the initialize routine of the shader engine, if we got a
  //new application object or the engine is not initialized
  if (FEngine <> nil) then
  begin
    FEngine.Initialize(
      TDXApplication(AApplication).Direct3DDevice9,
      AApplication.Log);
  end;

  FAppl := AApplication;
end;

function TDXShaderSystem.LoadShaderPlugin(AID: TAdVeryShortString): boolean;
var
  code: Integer;
  buf: string;
  pluginfilename: PChar;
  hdl: THandle;
  proc: TDXCreateShaderEngineProc;
begin
  result := false;

  //Generate name for the plugin
  buf := 'AndorraDX93D' + AID + '.dll';
  pluginfilename := PChar(buf);

  //Check whether file exists
  code := GetFileAttributes(pluginfilename);
  if (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code = 0) then
  begin
    //Load library
    hdl := Windows.LoadLibrary(pluginfilename);
    if hdl <> 0 then
    begin
      //Check whether entry point exists
      @proc := GetProcAddress(hdl, 'DXCreateShaderEngine');
      if @proc <> nil then
      begin
        //Create shader engine
        FEngine := proc;
        FEngine.ID := AID;
        
        result := true;
      end;
    end;
  end;
end;

end.
