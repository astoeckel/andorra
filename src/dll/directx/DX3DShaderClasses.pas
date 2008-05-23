unit DX3DShaderClasses;

interface

uses
  Direct3D9, AdShaderClasses, AdClasses, AdTypes;

type
  TDXShaderEngine = class
    private
      FDevice: IDirect3DDevice9;
      FLogProc: TAd2dLogCallback;
    protected
      function GetInitialized: boolean;virtual;abstract;
    public
      ID: TAdVeryShortString;
      
      procedure Initialize(ADevice: IDirect3DDevice9;
        ALogProc: TAd2dLogCallback);virtual;
      procedure Finalize;virtual;abstract;

      function CreateShader: TAd2dShader;virtual;abstract;
      property Device: IDirect3DDevice9 read FDevice;
      property Log: TAd2dLogCallback read FLogProc;
      property Initialized: boolean read GetInitialized;
  end;

  TDXCreateShaderEngineProc = function:TDXShaderEngine;stdcall;

implementation

{ TDXShaderEngine }

procedure TDXShaderEngine.Initialize(ADevice: IDirect3DDevice9;
  ALogProc: TAd2dLogCallback);
begin
  Finalize;

  FDevice := ADevice;
  FLogProc := ALogProc;
end;

end.
