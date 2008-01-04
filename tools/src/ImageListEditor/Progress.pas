unit Progress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls;

type
  TProgressDlg = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  ProgressDlg: TProgressDlg;

implementation

{$R *.dfm}

end.
