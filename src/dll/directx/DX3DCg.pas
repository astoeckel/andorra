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
        ALogProc: TAd2dLogCallback);override;
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
  ALogProc: TAd2dLogCallback);
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
end;

procedure TDXCgShader.Bind;
begin
  cgD3D9BindProgram(FProgram);
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
