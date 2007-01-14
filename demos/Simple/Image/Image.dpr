program Image;

uses
  Forms,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Image Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
