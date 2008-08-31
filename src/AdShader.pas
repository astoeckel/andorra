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
* File: AdShader.pas
* Comment: Contains objects for using shaders in Andorra 2D.
}

{Contains objects for using shaders in Andorra 2D.}
unit AdShader;

interface

uses
  SysUtils, Classes, IniFiles,
  AdClasses, AdShaderClasses, AdTypes, AdContainers, AdMessages,
  AdDraws, AdMath;

type
  {Parent exception class for shader purposes.}
  EShader = class(Exception);
  {Raised when a shader language is not supported, but you
   wanted the shader system to use it in TAdShader.CompileProgram.}
  EUnsupportedShaderLanguage = class(EShader);
  {Raised when you call a function of TAdShader, that requires the shader to be
   loaded and compiled. Check the return value of TAdShader.CompileProgram to
   ensure that your program is successfully compiled.}
  EShaderNotLoaded = class(EShader);

  TAdShader = class;

  {Andorra 2D's main shader system class. This class wraps around the the
   TAd2dShaderSystem class, that is returned by the Andorra 2D graphic plugin.
   TAdShaderSystem ensures that a new instance of TAd2dShaderSystem is created
   when the plugin changes. Initialisation and Finalisation routines of
   TAd2dShaderSystem are automatically called. @br
   @bold(Please notice that every shader system is able to handle a single
   shader system only. Every attempt to compile a new shader using another
   language will be rejected.)}
  TAdShaderSystem = class
    private
      FDraw: TAdDraw;
      FSystem: TAd2dShaderSystem;
      FDllName: string;
      FShaders: TList;
      function GetInitialized: boolean;
    protected
      procedure Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
      procedure Initialize;
      procedure Finalize;
    public
      {Creates an instance of TAdShaderSystem. Automatically registers in the
       TAdDraw surface events. The shader system is initialized parallel to
       TAdDraw. @seealso(TAdDraw)}
      constructor Create(ADraw: TAdDraw);
      {Destroys the instance of TAdShaderSystem. Unregisters itself from the
       parent TAdDraw.}
      destructor Destroy;override;

      {Pointer on the internal shader system. It is not recomended to use it
       directly. For instance, to create your own shader object use the
       abstracted TAdShader class. @seealso(TAd2dShaderSystem)}
      property ShaderSystem: TAd2dShaderSystem read FSystem;
      {Returns whether the shader system is actually initializied.
       Initialization is done automatically parallel to the parent TAdDraw of
       the shader system.}
      property Initialized: boolean read GetInitialized;

      {Pointer on the parent TAdDraw.}
      property AdDraw: TAdDraw read FDraw;
  end;

  {The TAdShader class represents a single vertex or fragment (pixel) shader
   program in the Andorra 2D application. The TAdShader class abstracts the
   TAd2dShader plugin interface. You are able to use your preferred shader
   language. But remember that not every shader language is supported by each
   rendering plugin. For example, the DirectX-Plugin is capable of executing
   HLSL and CG shaders, the OpenGL plugin is able to run GLSL and CG shaders. @br
   To load and compile a shader follow the following steps:
   @unorderedList(
     @item(Check whether your desired shader language is supported using the
       "SupportsShaderLanguage" function)
     @item(If you found a supported language, load the shader source using
       the "LoadFrom" functions)
     @item(Finally compile the loaded program using the "CompileProgram"
       function.)
   )
   @seealso(TAdShaderSystem)}
  TAdShader = class
    private
      FShaderSystem: TAdShaderSystem;
      FParameterList: THashedStringList;
      FShader: TAd2dShader;
      FShaderType: TAd2dShaderType;
      FShaderLanguage: TAdVeryShortString;
      FProgramName: string;
      FProgBuf: string;
    protected
      function Initialize: boolean;
      procedure Finalize;
      procedure Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
      procedure FreeShader;
      function GetParameter(AName: string): Pointer;
    public
      {Creates an instance of TAdShader, linked to the shader
       system defined in the "AShaderSystem" parameter. So before freeing
       TAdShaderSystem, always destroy the shader objects linked to it. }
      constructor Create(AShaderSystem: TAdShaderSystem);
      {Destroys the instance of TAdShader and frees its reserved memory and
       shader objects.}
      destructor Destroy;override;

      {Returns whether a shader language is supported.
       @param(ALanguage is the shader id consiting of maximal four letters.
         Currently supported shader languages are:
         @definitionList(
           @itemLabel(hlsl)
           @item(HLSL is the shader language integrated in D3D.)
           @itemLabel(glsl)
           @item(GLSL is the shader language integrated in OpenGL.)
           @itemLabel(cg)
           @item(The CG-Shader language developed by Nvidea, can be used
             simultaneously with D3D and OpenGL)
         )
       )
       @seealso(TAdShader.CompileProgram)}
      function SupportsShaderLanguage(ALanguage: TAdVeryShortString): boolean;

      {Loads the shader sourcecode from a stream.}
      procedure LoadFromStream(AStream: TStream);
      {Loads the shader sourcecode from a string list.}
      procedure LoadFromStrings(AStrs: TStrings);
      {Loads the shader sourcecode from a single string.}
      procedure LoadFromString(AStr: string);
      {Loads the shader sourcecode from the specified file.}
      procedure LoadFromFile(AFile: string);

      {Compiles the program loaded by the "LoadFrom" functions.
       @param(AShaderLanguage specifies the language the shader sourcecode is written in)
       @param(AProgramName specifies the name of the main function that is used by the
         shader language.)
       @param(AShaderType specifies whether you want to use the shader as a
         vertex or a fragment (pixel) shader.)
       @seealso(TAdShader.SupportsShaderLanguage)
       @seealso(TAdShader.LoadFromStream)
       @seealso(TAdShader.LoadFromFile)
       @seealso(TAdShader.LoadFromString)
       @seealso(TAdShader.LoadFromStrings)
       @raises(EUnsupportedShaderLanguage if the shader language is not
         supported.)}
      function CompileProgram(AShaderLanguage: TAdVeryShortString;
        AProgramName: string; AShaderType: TAd2dShaderType): boolean;

      {Activates the loaded effect. Remember that only one pixel and one vertex
       shader can be active at the same time.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure BindEffect;
      {Deactivate the loaded effect. Depending on the implementation in the
       graphic system.}
      procedure UnbindEffect;

      {Sets a "uniform" float parameter in the shader to the specified value.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: single);overload;
      {Sets a "uniform" integer parameter in the shader to the specified value.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: integer);overload;
      {Sets a "uniform" 4x4 float matrix parameter in the shader to the
       specified value.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: TAdMatrix);overload;
      {Sets a "uniform" sampler2D parameter in the shader to the specified
       Andorra 2D texture.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: TAd2dTexture);overload;
      {Sets a "uniform" float4 parameter in the shader to the specified
       Andorra 2D color.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: TAndorraColor);overload;
      {Sets a "uniform" float3 parameter in the shader to the specified
       vector.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: TAdVector3);overload;  
      {Sets a "uniform" float4 parameter in the shader to the specified
       vector.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: TAdVector4);overload;  
      {Sets a "uniform" float2 parameter in the shader to the specified
       vector.
       @raises(EShaderNotLoaded if the shader is not compiled or not available)}
      procedure SetParameter(AName: string; AValue: TAdVector2);overload;  

      {Pointer on the internal shader object returned by the graphic plugin. It
       is not recommended to use this direct way if you don't know exactly what
       you're doing.
       @seealso(TAd2dShader)}
      property Shader: TAd2dShader read FShader;

      {The language the loaded shader is written in.}
      property ShaderLanguage: TAdVeryShortString read FShaderLanguage;
      {The type of the currently loaded shader - vertex or fragment shader.}
      property ShaderType: TAd2dShaderType read FShaderType;
      {The name of the currently loaded shader main function.}
      property ProgramName: string read FProgramName;
  end;

  TAdShaderEffect = class
    private
      FObj: TAdRenderingObject;
      FSys: TAdShaderSystem;
      FVertexShader: TAdShader;
      FFragmentShader: TAdShader;
    protected
      procedure BeginRender(Sender: TObject; AModelViewProjection: TAdMatrix);
      procedure EndRender(Sender: TObject);
    public
      constructor Create(ASys: TAdShaderSystem);
      destructor Destroy;override;
      procedure Unbind;
      procedure BindToObject(AObj: TAdRenderingObject);
      property FragmentShader: TAdShader read FFragmentShader;
      property VertexShader: TAdShader read FVertexShader;
  end;

implementation

{ TAdShaderSystem }

constructor TAdShaderSystem.Create(ADraw: TAdDraw);
begin
  inherited Create;

  FDraw := ADraw;
  FDraw.RegisterNotifyEvent(Notify);

  FShaders := TList.Create;

  Initialize;
end;

destructor TAdShaderSystem.Destroy;
begin
  FDraw.UnRegisterNotifyEvent(Notify);

  if FSystem <> nil then
    FSystem.Free;

  FShaders.Free;

  inherited;
end;

procedure TAdShaderSystem.Initialize;
begin
  Finalize;

  if FDraw.Initialized then
  begin
    if FDllName <> FDraw.DllName then
    begin
      if FSystem <> nil then
        FSystem.Free;
        
      FDllName := FDraw.DllName;
      FSystem := FDraw.DllLoader.CreateShaderSystem;
    end;

    if FSystem <> nil then
      FSystem.Initialize(FDraw.AdAppl);
  end;
end;

procedure TAdShaderSystem.Finalize;
begin
  if (FSystem <> nil) and FSystem.Initialized then
    FSystem.Finalize;
end;

function TAdShaderSystem.GetInitialized: boolean;
begin
  result := (FSystem <> nil) and (FSystem.Initialized);
end;

procedure TAdShaderSystem.Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialize:
      Initialize;
    seFinalize:
      Finalize;
  end;
end;

{ TAdShader }

constructor TAdShader.Create(AShaderSystem: TAdShaderSystem);
begin
  inherited Create;

  FShaderSystem := AShaderSystem;
  FShaderSystem.AdDraw.RegisterNotifyEvent(Notify);

  FParameterList := THashedStringList.Create;
end;

destructor TAdShader.Destroy;
begin
  FShaderSystem.AdDraw.UnRegisterNotifyEvent(Notify);

  FParameterList.Free;

  if FShader <> nil then
    FShader.Free;

  inherited;
end;

procedure TAdShader.FreeShader;
begin
  if FShader <> nil then
    FreeAndNil(FShader);

  FParameterList.Clear;
end;

procedure TAdShader.Notify(Sender: TObject; AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialized:
      Initialize;
    seFinalize:
     Finalize;
  end;
end;

function TAdShader.SupportsShaderLanguage(
  ALanguage: TAdVeryShortString): boolean;
var
  tmp: TAd2dShader;
begin
  result := false;
  
  //Create a temporary shader object for test purposes
  tmp := FShaderSystem.ShaderSystem.CreateShader(Lowercase(ALanguage));

  if tmp <> nil then
  begin
    result := true;
    tmp.Free;
  end;
end;

function TAdShader.CompileProgram(AShaderLanguage: TAdVeryShortString;
  AProgramName: string; AShaderType: TAd2dShaderType): boolean;
begin
  //Free the existing instance of our shader object
  FreeShader;

  //Set program settings...
  FShaderType := AShaderType;
  FShaderLanguage := AShaderLanguage;
  FProgramName := AProgramName;

  //...and load it!
  result := Initialize;
end;

procedure TAdShader.Finalize;
begin
  //Free our shader
  FreeShader;
end;

function TAdShader.Initialize: boolean;
begin
  result := false;
  if (FProgBuf <> '') and (FShaderLanguage <> '') and (FProgramName <> '') then
  begin
    FShader := FShaderSystem.ShaderSystem.CreateShader(Lowercase(FShaderLanguage));
    if FShader <> nil then
    begin
      FShader.LoadProgramFromBuffer(PChar(FProgBuf),
        assSource, PChar(FProgramName), FShaderType);

      //If compiling was successful, initialize shader
      if FShader.Loaded then
      begin
        FShader.Initialize;
        result := true;
      end;
    end else
      raise EUnsupportedShaderLanguage.CreateFmt(
      MsgShaderLanguageNotSupported, [FShaderLanguage]);
  end;
end;

procedure TAdShader.LoadFromFile(AFile: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(AFile);
  LoadFromStrings(sl);
  sl.Free;
end;

procedure TAdShader.LoadFromStream(AStream: TStream);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromStream(AStream);
  LoadFromStrings(sl);
  sl.Free;
end;

procedure TAdShader.LoadFromString(AStr: string);
begin
  FProgBuf := AStr;
end;

procedure TAdShader.LoadFromStrings(AStrs: TStrings);
var
  ps: PAnsiChar;
begin
  ps := AStrs.GetText;
  FProgBuf := ps;
  StrDispose(ps);
end;

procedure TAdShader.BindEffect;
begin
  if FShader <> nil then  
    FShader.Bind
  else
    raise EShaderNotLoaded.Create(MsgShaderNotLoaded);
end;

procedure TAdShader.UnbindEffect;
begin
  if FShader <> nil then  
    FShader.Unbind;
end;

function TAdShader.GetParameter(AName: string): Pointer;
var
  ind: integer;
begin
  //Make sure that the shader exists
  if FShader = nil then
    raise EShaderNotLoaded.Create(MsgShaderNotLoaded);

  ind := FParameterList.IndexOf(AName);
  if ind = -1 then
  begin
    result := FShader.GetParameter(PChar(AName));
    FParameterList.AddObject(AName, result);
  end
  else
    result := FParameterList.Objects[ind];
end;

procedure TAdShader.SetParameter(AName: string; AValue: TAdMatrix);
begin
  FShader.SetParameter(GetParameter(AName), PSingle(@AValue[0, 0]), 16);
end;

procedure TAdShader.SetParameter(AName: string; AValue: single);
begin
  FShader.SetParameter(GetParameter(AName), PSingle(@AValue), 1);
end;

procedure TAdShader.SetParameter(AName: string; AValue: TAndorraColor);
var
  vec4: TAdVector4;
begin
  vec4 := AdVector4(
    AValue.r / 255,
    AValue.g / 255,
    AValue.b / 255,
    AValue.a / 255);
  FShader.SetParameter(GetParameter(AName), PSingle(@vec4), 4);
end;

procedure TAdShader.SetParameter(AName: string; AValue: TAd2dTexture);
begin
  FShader.SetParameter(GetParameter(AName), AValue);
end;

procedure TAdShader.SetParameter(AName: string; AValue: TAdVector3);
begin
  FShader.SetParameter(GetParameter(AName), PSingle(@AValue), 3);
end;

procedure TAdShader.SetParameter(AName: string; AValue: integer);
begin
  FShader.SetParameter(GetParameter(AName), PInteger(@AValue), 1);
end;

procedure TAdShader.SetParameter(AName: string; AValue: TAdVector4);
begin
  FShader.SetParameter(GetParameter(AName), PSingle(@AValue), 4);
end;

procedure TAdShader.SetParameter(AName: string; AValue: TAdVector2);
begin
  FShader.SetParameter(GetParameter(AName), PSingle(@AValue), 2);
end;

{ TAdShaderEffect }

constructor TAdShaderEffect.Create(ASys: TAdShaderSystem);
begin
  inherited Create;

  FSys := ASys;
  FVertexShader := TAdShader.Create(FSys);
  FFragmentShader := TAdShader.Create(FSys);
  FObj := nil;
end;

destructor TAdShaderEffect.Destroy;
begin
  FVertexShader.Free;
  FFragmentShader.Free;

  inherited Destroy;
end;

procedure TAdShaderEffect.Unbind;
begin
  if FObj <> nil then
  begin
    @FObj.OnBeginRender := nil;
    @FObj.OnEndRender := nil;
    FObj := nil;
  end;
end;

procedure TAdShaderEffect.BindToObject(AObj: TAdRenderingObject);
begin
  Unbind;
  FObj := AObj;
  
  if FObj <> nil then
  begin
    FObj.OnBeginRender := BeginRender;
    FObj.OnEndRender := EndRender;
  end;
end;

procedure TAdShaderEffect.BeginRender(Sender: TObject;
  AModelViewProjection: TAdMatrix);
var
  mat: TAdMatrix;
begin
  mat := AdMatrix_Transpose(AModelViewProjection);
  if FVertexShader.Shader <> nil then
  begin
    FVertexShader.SetParameter('modelview', mat);
    FVertexShader.BindEffect;
  end;
  if FFragmentShader.Shader <> nil then
  begin
    FFragmentShader.BindEffect;
  end;
end;

procedure TAdShaderEffect.EndRender(Sender: TObject);
begin
  FVertexShader.UnbindEffect;
  FFragmentShader.UnbindEffect;
end;

end.

