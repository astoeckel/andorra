program VideoPlayer;

uses
  Main in 'Main.pas',
  AdSDLWindow in '..\..\..\src\AdSDLWindow.pas';

var
  Appl: TAdAppl;

begin
  Appl := TAdAppl.Create;
  Appl.Run;
  Appl.Free;
end.
