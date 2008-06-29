program Simple;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  AdBMPImage in '..\..\..\src\AdBMPImage.pas',
  AdBMP in '..\..\..\src\AdBMP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Simple Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
