program Bounce;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Andorra in '..\..\..\src\Andorra.pas',
  AndorraUtils in '..\..\..\src\AndorraUtils.pas',
  AdDraws in '..\..\..\src\AdDraws.pas',
  AdSprites in '..\..\..\src\AdSprites.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Bounce Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
