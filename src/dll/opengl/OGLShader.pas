unit OGLShader;

interface

uses
  {$IFDEF WIN32}Windows{$ELSE}dynlibs, SysUtils, {$ENDIF}, AdTypes, AdClasses,
  AdShaderClasses, AdContainers,
  OGLShaderClasses, OGLMain;

type
  TOGLShaderSystem = class(TAd2dShaderSystem)
    private
      FEngine: TOGLShaderEngine;
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

{ TOGLShaderSystem }

constructor TOGLShaderSystem.Create;
begin
  inherited;
end;

destructor TOGLShaderSystem.Destroy;
begin
  Finalize;
  inherited;
end;

function TOGLShaderSystem.CreateShader(AID: TAdVeryShortString): TAd2dShader;
begin
  result := nil;

  if (FEngine = nil) then
  begin
    if AID = 'hlsl' then
    begin
      //Create hlsl context here
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

procedure TOGLShaderSystem.Finalize;
begin
  if FEngine <> nil then
    FEngine.Finalize;
end;

function TOGLShaderSystem.GetInitialized: boolean;
begin
  result := (FEngine <> nil) and (FEngine.Initialized);
end;

procedure TOGLShaderSystem.Initialize(AApplication: TAd2dApplication);
begin
  //Only call the initialize routine of the shader engine, if we got a
  //new application object or the engine is not initialized
  if (FEngine <> nil) then
  begin
    FEngine.Initialize(AApplication.Log);
  end;

  FAppl := AApplication;
end;

function TOGLShaderSystem.LoadShaderPlugin(AID: TAdVeryShortString): boolean;
var
  {$IFDEF WIN32}
  code: Integer;
  {$ENDIF}
  buf: string;
  pluginfilename: PChar;
  hdl: THandle;
  proc: TOGLCreateShaderEngineProc;
begin
  result := false;

  //Generate name for the plugin
  buf := 'AndorraOGL' + AID + '.dll';
  pluginfilename := PChar(buf);

  //Check whether file exists
  {$IFDEF WIN32}
  code := GetFileAttributes(pluginfilename);
  if (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code = 0) then
  {$ELSE}
  if (FileExists(pluginfilename)) then  
  {$ENDIF}
  begin
    //Load library
    hdl := Windows.LoadLibrary(pluginfilename);
    if hdl <> 0 then
    begin
      //Check whether entry point exists
      @proc := GetProcAddress(hdl, 'OGLCreateShaderEngine');
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
