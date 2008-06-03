program Surfaces;

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
