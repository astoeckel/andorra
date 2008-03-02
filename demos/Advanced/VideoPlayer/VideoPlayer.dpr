program VideoPlayer;

uses
  Forms,
  Main in 'Main.pas';

var
  Appl: TAdAppl;

begin
  Appl := TAdAppl.Create;
  Appl.Run;
  Appl.Free;
end.
