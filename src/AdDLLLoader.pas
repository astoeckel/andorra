unit AdDLLLoader;

interface

uses SysUtils,Windows,AdClasses;

type TAndorraDllLoader = class
  private
    DllHandle:THandle;
  public
    CreateApplication:TAdCreateApplicationProc;
    procedure LoadLibrary(afile:string);
    procedure UnLoadLibrary;
    function LibraryLoaded:boolean;
    constructor Create;
    destructor Destroy;override;
end;

implementation

constructor TAndorraDllLoader.Create;
begin
  inherited Create;
end;

destructor TAndorraDllLoader.Destroy;
begin
  UnLoadLibrary;
  inherited Destroy;
end;

function TAndorraDllLoader.LibraryLoaded:boolean;
begin
  result := DllHandle <> 0;
end;

procedure TAndorraDllLoader.LoadLibrary(afile: string);
begin
  if fileExists(ExtractFilePath(ParamStr(0))+afile) then
  begin
    if LibraryLoaded then
    begin
      UnLoadLibrary;
    end;
    DllHandle := Windows.LoadLibrary(PChar(ExtractFilePath(ParamStr(0))+afile));
    if LibraryLoaded then
    begin
      @CreateApplication := GetProcAddress(DllHandle, 'CreateApplication');
    end;
  end;
end;

procedure TAndorraDllLoader.UnLoadLibrary;
begin
  if LibraryLoaded then
  begin
    FreeLibrary(DllHandle);
  end;  
end;

end.
