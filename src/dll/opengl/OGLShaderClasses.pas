unit OGLShaderClasses;

interface

uses
  AdShaderClasses, AdClasses, AdTypes;

type
  TOGLShaderEngine = class
    private
      FLogProc: TAd2dLogCallback;
    protected
      function GetInitialized: boolean;virtual;abstract;
    public
      ID: TAdVeryShortString;

      procedure Initialize(ALogProc: TAd2dLogCallback);virtual;
      procedure Finalize;virtual;abstract;

      function CreateShader: TAd2dShader;virtual;abstract;
      property Log: TAd2dLogCallback read FLogProc;
      property Initialized: boolean read GetInitialized;
  end;

  TOGLCreateShaderEngineProc = function:TOGLShaderEngine;stdcall;

implementation

{ TOGLShaderEngine }

procedure TOGLShaderEngine.Initialize(ALogProc: TAd2dLogCallback);
begin
  Finalize;

  FLogProc := ALogProc;
end;

end.
