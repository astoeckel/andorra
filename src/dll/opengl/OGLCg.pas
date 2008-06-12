unit OGLCg;

interface

uses
  AdShaderClasses, AdClasses, AdTypes,
  OGLShaderClasses,
  cg, cgGL;

type
  TOGLCgEngine = class(TOGLShaderEngine)
    private
      FContext: PCGContext;
    protected
      function GetInitialized: boolean;override;
    public
      procedure Initialize(ALogProc: TAd2dLogCallback);override;
      procedure Finalize;override;

      function CreateShader: TAd2dShader;override;

      property Context: PCGContext read FContext;
  end;
       
  TOGLCgShader = class(TAd2dShader)
    private
      FSystem: TOGLCgEngine;
      FProgram: PCGProgram;
      FProgramName: string;
      FProgramType: TAd2dShaderType;
      FProfile: TCGProfile;
      procedure GetCGError(AState: string);
    protected
      function GetLoaded: boolean;override;
    public
      constructor Create(ASystem: TOGLCgEngine);
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

{ TOGLCgEngine }

procedure TOGLCgEngine.Finalize;
begin
  inherited;

  if FContext <> nil then
    cgDestroyContext(FContext);
  FContext := nil;
end;

function TOGLCgEngine.GetInitialized: boolean;
begin
  result := FContext <> nil;
end;

procedure TOGLCgEngine.Initialize(ALogProc: TAd2dLogCallback);
begin
  inherited;

  FContext := cgCreateContext;
end;

function TOGLCgEngine.CreateShader: TAd2dShader;
begin
  result := TOGLCgShader.Create(self);
end;

{ TOGLCgShader }

constructor TOGLCgShader.Create(ASystem: TOGLCgEngine);
begin
  inherited Create;

  FSystem := ASystem;
end;

destructor TOGLCgShader.Destroy;
begin
  if FProgram <> nil then
    cgDestroyProgram(FProgram);

  inherited;
end;

procedure TOGLCgShader.Finalize;
begin
  //
end;

procedure TOGLCgShader.Initialize;
begin
  //Load program
  cgGLLoadProgram(FProgram);
end;

procedure TOGLCgShader.GetCGError(AState: string);
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

function TOGLCgShader.GetLoaded: boolean;
begin
  result := FProgram <> nil;
end;

procedure TOGLCgShader.LoadProgramFromBuffer(ABuf: PChar;
  ASourceType: TAd2dShaderSourceType; AProgramName: PChar;
  AShaderType: TAd2dShaderType);
var
  srctype: TCGenum;
begin
  srctype := TCGenum(0);
  FProfile := TCGprofile(0);

  FProgramName := AProgramName;
  FProgramType := AShaderType;

  //Translate Andorra source-type enum
  case ASourceType of
    assSource: srctype := CG_SOURCE;
    assCompiled: srctype := CG_OBJECT;
  end;

  //Read the latest supported shader model profile depending on the program type
  case AShaderType of
    astVertex: FProfile := cgGLGetLatestProfile(CG_GL_VERTEX);
    astFragment: FProfile := cgGLGetLatestProfile(CG_GL_FRAGMENT);
  end;

  cgGLSetOptimalOptions(FProfile);

  FProgram := cgCreateProgram(FSystem.Context, srctype, ABuf,
    FProfile, AProgramName, nil);
  GetCGError('Load program');
end;

procedure TOGLCgShader.Unbind;
begin
  cgGLDisableProfile(FProfile);
end;

procedure TOGLCgShader.Bind;
begin
  cgGLEnableProfile(FProfile);
  cgGLBindProgram(FProgram);
end;

function TOGLCgShader.GetParameter(AName: PChar): Pointer;
begin
  result := cgGetNamedParameter(FProgram, AName);
end;

procedure TOGLCgShader.SetParameter(AParam: Pointer; AValue: PSingle;
  ACount: integer);
begin
  cgSetParameterValuefr(AParam, ACount, @AValue^);
end;

procedure TOGLCgShader.SetParameter(AParam: Pointer; AValue: PInteger;
  ACount: integer);
begin
  cgSetParameterValueir(AParam, ACount, @AValue^);
end;

procedure TOGLCgShader.SetParameter(AParam: Pointer; AValue: TAd2dTexture);
begin
  cgGLSetTextureParameter(AParam, PCardinal(AValue.Texture)^);
end;

end.
