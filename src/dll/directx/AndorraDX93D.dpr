library AndorraDX93D;

uses
  SysUtils,
  Classes,
  AdClasses,
  DX3DMain in 'DX3DMain.pas';

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
