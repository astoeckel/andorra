program NoVCL;

{$MODE Delphi}

uses
  Main in 'Main.pas';
  
var
  appl:TAdAppl;

begin
  appl := TAdAppl.Create;
  appl.Run;
  appl.Free;
end.

