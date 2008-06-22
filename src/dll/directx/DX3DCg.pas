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
* File: DX3DCg.pas
* Comment: DirectX-Plugin CG-Shader Plugin
}
{DirectX-Plugin CG-Shader Plugin}
unit DX3DCg;

interface

uses
  AdShaderClasses, AdClasses, AdTypes,
  DX3DShaderClasses,
  cg, cgd3d9, Direct3D9;

type
  TDXCGEngine = class(TDXShaderEngine)
    private
      FContext: PCGContext;
    protected
      function GetInitialized: boolean;override;
    public
      procedure Initialize(ADevice: IDirect3DDevice9;
        ALogProc: TAd2dLogCallback;
        AUsePixelShaderProc: TDXUsePixelShaderCallback);override;
      procedure Finalize;override;

      function CreateShader: TAd2dShader;override;

      property Context: PCGContext read FContext;
  end;
       
  TDXCgShader = class(TAd2dShader)
    private
      FSystem: TDXCGEngine;
      FProgram: PCGProgram;
      FProgramName: string;
      FProgramType: TAd2dShaderType;
      procedure GetCGError(AState: string);
    protected
      function GetLoaded: boolean;override;
    public
      constructor Create(ASystem: TDXCGEngine);
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

{ TDXCGEngine }

procedure TDXCGEngine.Finalize;
begin
  inherited;

  if FContext <> nil then
    cgDestroyContext(FContext);
  FContext := nil;
end;

function TDXCGEngine.GetInitialized: boolean;
begin
  result := FContext <> nil;
end;

procedure TDXCGEngine.Initialize(ADevice: IDirect3DDevice9;
  ALogProc: TAd2dLogCallback; AUsePixelShaderProc: TDXUsePixelShaderCallback);
begin
  inherited;

  FContext := cgCreateContext;
  cgD3D9SetDevice(Device);
end;

function TDXCGEngine.CreateShader: TAd2dShader;
begin
  result := TDXCGShader.Create(self);
end;

{ TDXCgShader }

constructor TDXCgShader.Create(ASystem: TDXCGEngine);
begin
  inherited Create;

  FSystem := ASystem;
end;

destructor TDXCgShader.Destroy;
begin
  if FProgram <> nil then
    cgDestroyProgram(FProgram);

  inherited;
end;

procedure TDXCgShader.Finalize;
begin
  cgD3D9UnloadProgram(FProgram);
end;

procedure TDXCgShader.Initialize;
begin
  //Load program
  cgD3D9LoadProgram(FProgram, Integer(False), 0);
end;

procedure TDXCgShader.GetCGError(AState: string);
var
  error: CGError;
  str: PChar;
  buf: string;
begin
  buf := '';
  str := cgGetLastErrorString(@error);
  if (error <> CG_NO_ERROR) then
  begin
    if (error = CG_COMPILER_ERROR) then
    begin
      buf := '['+AState+'] Compiler error ' + #13#10 +
        'Excerpt from ' + FProgramName + #13#10 + 
        '---------' + #13#10 +
        str + #13#10 +
        cgGetLastListing(FSystem.Context) + #13#10 +
        '---------';
    end else
    begin
      buf := '['+AState+'] CG Runtime error' + str;
    end;
  end;

  if buf <> '' then
    FSystem.Log('CG Program', lsError, PChar(buf));
end;

function TDXCgShader.GetLoaded: boolean;
begin
  result := FProgram <> nil;
end;

procedure TDXCgShader.LoadProgramFromBuffer(ABuf: PChar;
  ASourceType: TAd2dShaderSourceType; AProgramName: PChar;
  AShaderType: TAd2dShaderType);
var
  srctype: TCGenum;
  profile: TCGprofile;
  opts: PPChar;
begin
  srctype := TCGenum(0);
  profile := TCGprofile(0);

  FProgramName := AProgramName;
  FProgramType := AShaderType;

  //Translate Andorra source-type enum
  case ASourceType of
    assSource: srctype := CG_SOURCE;
    assCompiled: srctype := CG_OBJECT;
  end;

  //Read the latest supported shader model profile depending on the program type
  case AShaderType of
    astVertex: profile := cgD3D9GetLatestVertexProfile;
    astFragment: profile := cgD3D9GetLatestPixelProfile;
  end;

  opts := cgD3D9GetOptimalOptions(profile);

  FProgram := cgCreateProgram(FSystem.Context, srctype, ABuf,
    profile, AProgramName, opts);
  GetCGError('Load program');
end;

procedure TDXCgShader.Unbind;
begin
  case FProgramType of
    astVertex: FSystem.Device.SetVertexShader(nil);
    astFragment: FSystem.Device.SetPixelShader(nil);
  end;
  if FProgramType = astFragment then
    FSystem.UsePixelShader(false);
end;

procedure TDXCgShader.Bind;
begin
  cgD3D9BindProgram(FProgram);
  if FProgramType = astFragment then
    FSystem.UsePixelShader(true);
end;

function TDXCgShader.GetParameter(AName: PChar): Pointer;
begin
  result := cgGetNamedParameter(FProgram, AName);
end;

procedure TDXCgShader.SetParameter(AParam: Pointer; AValue: PSingle;
  ACount: integer);
begin
  cgSetParameterValuefr(AParam, ACount, @AValue^);
end;

procedure TDXCgShader.SetParameter(AParam: Pointer; AValue: PInteger;
  ACount: integer);
begin
  cgSetParameterValueir(AParam, ACount, @AValue^);
end;

procedure TDXCgShader.SetParameter(AParam: Pointer; AValue: TAd2dTexture);
begin
  cgD3D9SetTexture(AParam, IDirect3DBaseTexture9(AValue.Texture));
end;

end.
