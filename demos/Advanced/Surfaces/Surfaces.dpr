program Surfaces;

//Set a icon for the application when using windows

uses
  SysUtils,
  Main in 'Main.pas'{$IFDEF FPC}, LazOpenGLContext{$ENDIF};

var
  appl: TAdAppl;

begin
  appl := TAdAppl.Create;
  try
    appl.Run
  finally
    appl.Free;
  end;
end.
