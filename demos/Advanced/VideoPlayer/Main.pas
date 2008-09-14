unit Main;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils, Dialogs, AdStdWindow, AdDraws, AdPNG,
  AdClasses, AdTypes, AdPerformanceCounter, AdVideo,
  AdGUI, AdComponents, AdGUIConnector, AdEvents, AdSetupDlg,

  //Select one of the following video decoders here
  AdAcinerella; {GPL only!}
  //AdMPEG2Video;

type
  TAdAppl = class
  public
    AdDraw:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdVideo:TAdVideoPlayer;
    AdGUI: TAdGUI;
    AdGUIConnector: TAdGUIConnector;

    procedure Idle(Sender:TObject;var Done:boolean);
    procedure OpenClick(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure PlayClick(Sender: TObject);
    procedure PauseClick(Sender: TObject);
    procedure StopClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
 
    procedure Run;
  end;

const
  Path = './resources/';

implementation

procedure TAdAppl.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw.CanDraw then
  begin
    AdPerCounter.Calculate;
    AdDraw.ClearSurface(0);
    AdDraw.BeginScene;

    AdVideo.Move(AdPerCounter.TimeGap / 1000);
    AdVideo.Image.Filter := atLinear;
    AdVideo.Draw(AdDraw, AdRect(0,0,AdDraw.Window.ClientWidth,AdDraw.Window.ClientHeight));

    with AdDraw.Canvas do
    begin
      TextOut(0,0,'FPS: '+inttostr(AdPerCounter.FPS));
      TextOut(0,16,'Video - FPS: '+inttostr(AdVideo.CurrentFPS));
      Release;
    end;

    AdGUI.Update(AdPerCounter.TimeGap / 1000);

    AdDraw.EndScene;
    AdDraw.Flip;
  end;

  Done := false;
end;

procedure TAdAppl.CheckBoxClick(Sender: TObject);
begin
  AdVideo.Stretch := TAdCheckBox(AdGUI.FindComponent('chk_stretch')).Checked;
  AdVideo.Loop := TAdCheckBox(AdGUI.FindComponent('chk_loop')).Checked;
  AdVideo.Proportional := TAdCheckBox(AdGUI.FindComponent('chk_proportional')).Checked;
  AdVideo.Center := TAdCheckBox(AdGUI.FindComponent('chk_center')).Checked;
end;

procedure TAdAppl.CloseClick(Sender: TObject);
begin
  AdVideo.Close;
end;

procedure TAdAppl.OpenClick(Sender: TObject);
var
  od: TOpenDialog;
begin
  od := TOpenDialog.Create(nil);
  od.Options := od.Options + [ofNoChangeDir];
  if od.Execute then
  begin
    AdVideo.Open(od.FileName);
  end;
  od.Free;
end;

procedure TAdAppl.PauseClick(Sender: TObject);
begin
  AdVideo.Pause;
end;

procedure TAdAppl.PlayClick(Sender: TObject);
begin
  AdVideo.Play;
end;

procedure TAdAppl.Run;
var
  AdSetup: TAdSetup;
begin
  AdPerCounter := TAdPerformanceCounter.Create;
//  AdPerCounter.MaximumFrameRate := 100;

  AdDraw := TAdDraw.Create(nil);
  
  AdSetup := TAdSetup.Create(AdDraw);
  AdSetup.Image := 'logo1.png';

  if AdSetup.Execute then
  begin
    if AdDraw.Initialize then
    begin
      AdDraw.Window.Events.OnIdle := Idle;
      AdDraw.Window.CursorVisible := false;
      AdDraw.Window.Title := 'Andorra 2D Video Player';

      if DirectoryExists(path) then
        SetCurrentDir(path);

      AdGUI := TAdGUI.Create(AdDraw);
      AdGUI.Skin.LoadFromFile('sunna.axs');
      AdGUI.Cursors.LoadFromFile('cursors.xml');
      AdGUI.LoadFromFile('VideoGUI.axg');

      AdGUI.FindComponent('btn_open').OnClick := OpenClick;
      AdGUI.FindComponent('btn_close').OnClick := CloseClick;
      AdGUI.FindComponent('btn_play').OnClick := PlayClick;
      AdGUI.FindComponent('btn_pause').OnClick := PauseClick;
      AdGUI.FindComponent('btn_stop').OnClick := StopClick;
      AdGUI.FindComponent('chk_loop').OnClick := CheckBoxClick;
      AdGUI.FindComponent('chk_stretch').OnClick := CheckBoxClick;
      AdGUI.FindComponent('chk_proportional').OnClick := CheckBoxClick;
      AdGUI.FindComponent('chk_center').OnClick := CheckBoxClick;

      AdGUIConnector := TAdGUIConnector.Create(AdGUI);
      AdGUIConnector.ConnectEventHandlers(AdDraw.Window);

      AdVideo := TAdVideoPlayer.Create(AdDraw);

      AdDraw.Run;
    end else
      ShowMessage('Error while initializing Andorra 2D. Try to use another display '+
                  'mode or another video adapter.');
  end;
  AdSetup.Free;
  AdGUIConnector.Free;
  AdGUI.Free;
  AdVideo.Free;
  AdPerCounter.Free;
  AdDraw.Free;
end;

procedure TAdAppl.StopClick(Sender: TObject);
begin
  AdVideo.Stop;
end;

end.
