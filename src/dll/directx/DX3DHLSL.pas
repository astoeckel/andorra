{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: DX3DHLSL.pas
* Comment: DirectX-Plugin CG-Shader Plugin
}
{DirectX-Plugin HLSL-Shader Plugin}
unit DX3DHLSL;

interface

uses
  SysUtils,
  AdShaderClasses, AdClasses, AdTypes, DX3DMain,
  DX3DShaderClasses, Direct3D9, d3dx9;

type
  TDXHLSLEngine = class(TDXShaderEngine)
    protected
      function GetInitialized: boolean;override;
    public
      procedure Initialize(ADevice: IDirect3DDevice9;
        ALogProc: TAd2dLogCallback);override;
      procedure Finalize;override;

      function CreateShader: TAd2dShader;override;
  end;

  TDXHLSLShader = class(TAd2dShader)
    private
      FProgram: Pointer;
      FBinCode: ID3DXBuffer;
      FProgramName: string;
      FProgramType: TAd2dShaderType;
      FEngine: TDXHLSLEngine;
      FConstantTable: ID3DXConstantTable;
    protected
      function GetLoaded: boolean;override;
    public
      constructor Create(AEngine: TDXHLSLEngine);
      destructor Destroy;override;

      procedure LoadProgramFromBuffer(ABuf: PChar;
        ASourceType: TAd2dShaderSourceType; AProgramName: PChar;
        AShaderType: TAd2dShaderType);override;

      procedure Initialize;override;
      procedure Finalize;override;

      function GetParameter(AName: PChar): Pointer;override;

      procedure SetParameter(AParam: Pointer; AValue: PSingle; ACount: integer);overload;override;
      procedure SetParameter(AParam: Pointer; AValue: PInteger; ACount: integer);overload;override;
      procedure SetParameter(AParam: Pointer; AValue: TAd2dTexture);overload;override;

      procedure Bind;override;
      procedure Unbind;override;
  end; 

implementation

{ TDXHLSLShader }

constructor TDXHLSLShader.Create(AEngine: TDXHLSLEngine);
begin
  inherited Create;

  FEngine := AEngine;
end;

destructor TDXHLSLShader.Destroy;
begin
  inherited;
end;

procedure TDXHLSLShader.Finalize;
begin
  case FProgramType of
    astVertex : IDirect3DVertexShader9(FProgram)._Release;
    astFragment : IDirect3DPixelShader9(FProgram)._Release;
  end;
  FProgram := nil;
end;

procedure TDXHLSLShader.Initialize;
begin
  case FProgramType of
    astVertex: FEngine.Device.CreateVertexShader(FBinCode.GetBufferPointer,
      IDirect3DVertexShader9(FProgram));
    astFragment: FEngine.Device.CreatePixelShader(FBinCode.GetBufferPointer,
      IDirect3DPixelShader9(FProgram));
  end;
end;

function TDXHLSLShader.GetLoaded: boolean;
begin
  result := FBinCode <> nil;
end;

procedure TDXHLSLShader.LoadProgramFromBuffer(ABuf: PChar;
  ASourceType: TAd2dShaderSourceType; AProgramName: PChar;
  AShaderType: TAd2dShaderType);
var
  profile: PAnsiChar;
  log: ID3DXBuffer;
  res: HResult;
  s: string;
begin
  profile := nil;

  FProgramName := AProgramName;
  FProgramType := AShaderType;

  if ASourceType = assSource then
  begin
    //Read the latest supported shader model profile depending on the program type
    case AShaderType of
      astVertex: profile := D3DXGetVertexShaderProfile(FEngine.Device);
      astFragment: profile := D3DXGetPixelShaderProfile(FEngine.Device);
    end;

    //Compile the shader
    res := D3DXCompileShader(ABuf, StrLen(ABuf), nil, nil,
      AProgramName, profile, 0, @FBinCode, @log, @FConstantTable);
    if res <> D3D_OK then
    begin
      FEngine.Log('Direct3D HLSL', lsError, log.GetBufferPointer);
    end else
    begin
      SetLength(s, FConstantTable.GetBufferSize);
      Move(FConstantTable.GetBufferPointer^, s[1], FConstantTable.GetBufferSize);
      
    end;
  end;
end;

procedure TDXHLSLShader.Unbind;
begin
  case FProgramType of
    astVertex: FEngine.Device.SetVertexShader(nil);
    astFragment: FEngine.Device.SetPixelShader(nil);
  end;
end;

procedure TDXHLSLShader.Bind;
begin
  case FProgramType of
    astVertex: FEngine.Device.SetVertexShader(IDirect3DVertexShader9(FProgram));
    astFragment: FEngine.Device.SetPixelShader(IDirect3DPixelShader9(FProgram));
  end;
end;

function TDXHLSLShader.GetParameter(AName: PChar): Pointer;
begin
  result := FConstantTable.GetConstantByName(nil, AName);
end;

procedure TDXHLSLShader.SetParameter(AParam: Pointer; AValue: PSingle;
  ACount: integer);
begin
  FConstantTable.SetFloatArray(FEngine.Device, AParam, @AValue^, ACount);
end;

procedure TDXHLSLShader.SetParameter(AParam: Pointer; AValue: PInteger;
  ACount: integer);
begin
  FConstantTable.SetIntArray(FEngine.Device, AParam, @AValue^, ACount);
end;

procedure TDXHLSLShader.SetParameter(AParam: Pointer; AValue: TAd2dTexture);
var
  count: Cardinal;
  desc: TD3DXConstantDesc;
begin
  FConstantTable.GetConstantDesc(AParam, @desc, count);
  if desc.RegisterSet = D3DXRS_SAMPLER then
  begin
    TDXBitmapTexture(AValue).SetFilter;
    FEngine.Device.SetTexture(desc.RegisterIndex, IDirect3DTexture9(AValue.Texture));
  end;
end;

{ TDXHLSLEngine }

function TDXHLSLEngine.CreateShader: TAd2dShader;
begin
  result := TDXHLSLShader.Create(self);
end;

procedure TDXHLSLEngine.Initialize(ADevice: IDirect3DDevice9;
  ALogProc: TAd2dLogCallback);
begin
  inherited;
end;

procedure TDXHLSLEngine.Finalize;
begin
  inherited;
end;

function TDXHLSLEngine.GetInitialized: boolean;
begin
  result := (Device <> nil) and (@Log <> nil);
end;

end.
