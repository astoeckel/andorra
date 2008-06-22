{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: DX3DShaderClasses.pas
* Comment: Interface unit for the DirectX-Plugin CG-Shader Plugin
}
{Interface unit for the DirectX-Plugin CG-Shader Plugin.}
unit DX3DShaderClasses;

interface

uses
  Direct3D9, AdShaderClasses, AdClasses, AdTypes;

type
  TDXUsePixelShaderCallback = procedure(AShader: boolean) of object;

  TDXShaderEngine = class
    private
      FDevice: IDirect3DDevice9;
      FLogProc: TAd2dLogCallback;
      FUsePixelShaderProc: TDXUsePixelShaderCallback;
    protected
      function GetInitialized: boolean;virtual;abstract;
    public
      ID: TAdVeryShortString;
      
      procedure Initialize(ADevice: IDirect3DDevice9;
        ALogProc: TAd2dLogCallback;
        AUsePixelShaderProc: TDXUsePixelShaderCallback);virtual;
      procedure Finalize;virtual;abstract;

      function CreateShader: TAd2dShader;virtual;abstract;
      property Device: IDirect3DDevice9 read FDevice;
      property Log: TAd2dLogCallback read FLogProc;
      property Initialized: boolean read GetInitialized;
      property UsePixelShader: TDXUsePixelShaderCallback read FUsePixelShaderProc;
  end;

  TDXCreateShaderEngineProc = function:TDXShaderEngine;stdcall;

implementation

{ TDXShaderEngine }

procedure TDXShaderEngine.Initialize(ADevice: IDirect3DDevice9;
  ALogProc: TAd2dLogCallback; AUsePixelShaderProc: TDXUsePixelShaderCallback);
begin
  Finalize;

  FDevice := ADevice;
  FLogProc := ALogProc;
  FUsePixelShaderProc := AUsePixelShaderProc;
end;

end.
