program Image;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  AdClasses in '..\..\..\src\AdClasses.pas',
  AdDLLLoader in '..\..\..\src\AdDLLLoader.pas',
  AdDraws in '..\..\..\src\AdDraws.pas',
  Huffman in '..\..\..\src\Huffman.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Andorra 2D Image Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
