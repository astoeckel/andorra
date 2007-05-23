{*******************************************************}
{                                                       }
{       Andorra2D Addon Application Unit                }
{                                                       }
{       Copyright (c) 2007 Framik                       }
{                                                       }
{*******************************************************}

{
@abstract(Unit with appication classes for the "Andorra 2D" engine)
@author(Framik (framik@arcor.de))
@created(25 Feb 2007)
@lastmod(02 May 2007)
This unit is an addon for the "Andorra 2D" engine.
It contains application classes which contains all needed "Andorra 2D" objects
for a  special type of "Andorra 2D" application.
}
unit AdApps;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  AdDraws, AdSprites, Controls, SysUtils;

type
  {A application class for a spriteengine based "Andorra 2D" application}
  TAdSpriteApp = class
  strict private
    FDraw: TAdDraw;
    FPerformanceCounter: TPerformanceCounter;
    FImageList: TAdImageList;
    FSpriteEngine: TSpriteEngine;
    FSurfaceClearColor: Integer;
  public
    {Creates a object and all needed subobjects}
    constructor Create(AParent: TWinControl;AInterfaceDLL: String);
    {destorys the object and all of it's subobjects}
    destructor Destroy;
    {Main loop procedure for assigning to TApplication.OnIdle}
    procedure DoIdle(Sender: TObject;var Done: boolean);
    {Initialize TAdDraw subobject - returns "True" on success}
    function Initialize: Boolean;
    {Interface to TAdDraw suboject}
    property Draw: TAdDraw read FDraw write FDraw;
    {Interface to TPerformanceCounter suboject}
    property PerformanceCounter: TPerformanceCounter read FPerformanceCounter
      write FPerformanceCounter;
    {Interface to TPictureCollection suboject}
    property PictureCollection: TAdImageList read FImageList
      write FImageList;
    {Interface to TSpriteEngine suboject}
    property SpriteEngine: TSpriteEngine read FSpriteEngine write FSpriteEngine;
    {Color for ClearSurface-Method at DoIdle procedure}
    property SurfaceClearColor: Integer read FSurfaceClearColor write
      FSurfaceClearColor;
  end;

implementation

{ TAdApplication }

constructor TAdSpriteApp.Create(AParent: TWinControl;AInterfaceDLL: String);
begin
  FDraw := TAdDraw.Create(AParent);
  FDraw.AutoLoadLog := False;
  FDraw.DllName := AInterfaceDLL;
  FImageList := TAdImageList.Create(FDraw);
  FSpriteEngine := TSpriteEngine.Create(nil);
  FSpriteEngine.Surface := FDraw;
  FSpriteEngine.CollisionOptimizationTyp := ctOptimized;
  FPerformanceCounter := TPerformanceCounter.Create;
end;

destructor TAdSpriteApp.Destroy;
begin
  FPerformanceCounter.Free;
  FSpriteEngine.Free;
  FImageList.Free;
  FDraw.Finalize;
  FDraw.Free;
end;

procedure TAdSpriteApp.DoIdle(Sender: TObject; var Done: boolean);
begin
  FPerformanceCounter.Calculate;
  if FDraw.CanDraw then
  begin
    FDraw.ClearSurface(FSurfaceClearColor);
    FDraw.BeginScene;
    FSpriteEngine.Move(FPerformanceCounter.TimeGap/1000);
    FSpriteEngine.Draw;
    FSpriteEngine.Dead;
    FDraw.EndScene;
    FDraw.Flip;
  end;
  Done := False;
end;

function TAdSpriteApp.Initialize: Boolean;
begin
  Result := FDraw.Initialize;
end;

end.
