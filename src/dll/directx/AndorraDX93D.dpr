library AndorraDX93D;

uses
  SysUtils,
  Classes,
  DX3DMain in 'DX3DMain.pas',
  AdClasses in '..\..\AdClasses.pas';

{$E .dll}

{$R *.res}

function CreateApplication:TAd2DApplication;stdcall;
begin
  result := TDXApplication.Create;
end;

exports
  CreateApplication;

begin
end.
