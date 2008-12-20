{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
}
unit Main;

interface

uses
  Windows, Dialogs, SysUtils, Graphics, Classes, Forms, Controls,
  AdPng, AdDraws, AdClasses, AdSetupDlg, AdPerformanceCounter, AdConsts;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdImageList:TAdImageList;
    procedure Idle(Sender:TObject;var Done:boolean);
    procedure SetLine;
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;
  Pattern:single;
  StartPt,EndPt:integer;
  Y,X:single;
  XSpeed:single;

const
  path = './resources/';

implementation

{$R *.dfm}

procedure TForm1.SetLine;
begin
  XSpeed := -XSpeed;
  if XSpeed > 0 then
  begin
    StartPt := 0;
    EndPt := 7;
    X := -96;
  end
  else
  begin
    StartPt := 8;
    EndPt := 15;
    X := ClientWidth+96;
  end;
  Y := Random(ClientHeight-96);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AdSetup: TAdSetup;
begin
  //Only for debuging - you can remove this line
  ReportMemoryLeaksOnShutdown := true;

  //Crate the main surface.
  AdDraw := TAdDraw.Create(self);

  //Create the setup dialog and pass the main surface
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    //Free the setup dialog
    AdSetup.Free;
    
    if AdDraw.Initialize then
    begin
      //Create the performance counter. This class is used for measuring the time
      //that passes between two frames.
      AdPerCounter := TAdPerformanceCounter.Create;

      //Connect the on idle event
      AdDraw.Window.Events.OnIdle := Idle;

      //Creat an image list and load the image that will be animated
      AdImageList := TAdImageList.Create(AdDraw);
      with AdImageList.Add('figur') do
      begin
        Texture.LoadGraphicFromFile(path+'boy.png',true,clFuchsia);

        //Pattern width and height specify the height and the width of a single
        //animation pattern. The patterns have to be orderd from left to right and
        //from top to bottom:
        //0  1  2  3
        //4  5  6  7
        //8  9  10 11
        //12 13 14 15
        PatternWidth := 96;
        PatternHeight := 96;
      end;
      //Call the restore function for all images in the image list
      AdImageList.Restore;

      XSpeed := -150;

      Randomize;
      SetLine;
    end
    else
    begin
      ShowMessage(AdDraw.GetLastError);
      halt;
    end;
  end
  else
  begin
    AdSetup.Free;
    halt;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AdImageList.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: boolean);
begin
  //Draw on the TAdDraw if drawing is possible
  if AdDraw.CanDraw then
  begin
    //Calculate the time difference.
    AdPerCounter.Calculate;

    Pattern := Pattern + 15*AdPerCounter.TimeGap/1000;
    if Pattern >= EndPt then Pattern := StartPt;

    X := X + XSpeed*AdPerCounter.TimeGap/1000;
    if ((X > ClientWidth) and (XSpeed > 0)) or ((X < -96) and (XSpeed < 0))  then SetLine;

    //Clear the surface of the TAdDraw with black color
    AdDraw.ClearSurface(AdCol24_Black);
    AdDraw.BeginScene;

    //Draw the image on the calculated position with the calculated animation pattern
    AdImageList.Find('figur').Draw(AdDraw,round(X),round(Y),round(Pattern));

    //Draw the FPS on the screen
    with AdDraw.Canvas do
    begin
      TextOut(5,5,'FPS: '+inttostr(AdPerCounter.FPS));
      Release;
    end;

    AdDraw.EndScene;
    //Bring the drawn things on the screen
    AdDraw.Flip;

    Done := false;
  end;
end;

end.
