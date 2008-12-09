program Surfaces;

//Set a icon for the application when using windows
{$IFDEF WIN32}
  {$R '..\..\icon.res' '..\..\icon.rc'}
{$ENDIF}

uses
  SysUtils,
  Main in 'Main.pas';

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
