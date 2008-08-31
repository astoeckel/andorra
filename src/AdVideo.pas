{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program * under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdVideoTexture.pas
* Comment: Contains a simple class for loading a video from a stream and rendering it on the surface.
}

{Contains a simple class for loading a video from a stream and rendering it on the surface.}
unit AdVideo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs,
  AdTypes, AdClasses, AdDraws, AdVideoTexture;

type
  {The base video player class. TAdCustomVideoPlayer expands TAdVideoTexture in
   the capability of drawing the video in many ways.
   Reading from files or streams is not supported in this class, use 
   TAdVideoPlayer instead}
  TAdCustomVideoPlayer = class(TAdVideoTexture)
    private
      FParent: TAdDraw;
      FImage: TAdImage;
      FProportional: boolean;
      FStretch: boolean;
      FCenter: boolean;
    protected
      procedure Notify(ASender:TObject;AEvent:TAdSurfaceEventState);
      function DestinationRect(ADestRect: TAdRect):TAdRect;
    public
      {Creates an instance of TAdCustomVideoPlayer}
      constructor Create(AParent: TAdDraw);
      {Destroys the instance of TAdCustomVideoPlayer and all created objects.}
      destructor Destroy;override;

      {Draws the video on a surface and }
      procedure Draw(ASurface: TAdDraw; ADestRect: TAdRect);virtual;

      {Specifies wheter the image should be streched propertional.}
      property Proportional: boolean read FProportional write FProportional;
      {If true, the video is streched to the rectangle defined in the draw
       procedure.}
      property Stretch: boolean read FStretch write FStretch;
      {If true, the video is centered when drawing.}
      property Center: boolean read FCenter write FCenter;
      {The image, that is actually drawn.}
      property Image: TAdImage read FImage;
  end;

  {Use this class to display video in your application, if you load your video
   data from a stream or a video file. TAdVideo player adds the support of
   loading video data from files or streams to TAdCustomVideoPlayer.}
  TAdVideoPlayer = class(TAdCustomVideoPlayer)
    private
      FStream: TStream;
      FOwnStream: boolean;
      FCriticalSection: TCriticalSection;
      function GetSize: int64;
      function GetPos: int64;
    protected
      function GetOpened:boolean;override;
      procedure ClearData;override;
      function ReadData(const Dest: Pointer; const Size: Cardinal): integer;override;
      procedure ResetData;override;
    public
      {Creates an instance of TAdCustomVideoPlayer}
      constructor Create(AParent: TAdDraw);
      {Destroys the instance of TAdCustomVideoPlayer and all created objects.}
      destructor Destroy;override;
      {Opens a file.}
      procedure Open(AFile: string);overload;
      {Opens a stream. Do not free the stream before the video is closed.}
      procedure Open(AStream: TStream);overload;
      {Seeks to another position in the media stream.}
      procedure Seek(APos: int64);
      {Returns the size of the stream.}
      property Size: int64 read GetSize;
      {Returns the current stream position.}
      property Pos: int64 read GetPos;
  end;

implementation

{ TAdCustomVideoPlayer }

constructor TAdCustomVideoPlayer.Create(AParent: TAdDraw);
begin
  inherited Create(AParent.AdAppl);

  FParent := AParent;
  FParent.RegisterNotifyEvent(Notify);
  FImage := TAdImage.Create(FParent);

  FImage.Texture.Texture := Texture;

  FProportional := true;
  FCenter := true;
  FStretch := true;
end;

function TAdCustomVideoPlayer.DestinationRect(ADestRect: TAdRect): TAdRect;
var
  w,h:integer;
  v: double;
  Width, Height: integer;
begin
  w := round(Info.Width * Info.PixelAspect);
  h := Info.Height;

  Width := ADestRect.Right - ADestRect.Left;
  Height := ADestRect.Bottom - ADestRect.Top;

  if Stretch or (FProportional and ((w > Width) or (h > Height))) then
  begin
    if FProportional and (w > 0) and (h > 0) then
    begin
      v := w / h;
      if w > h then
      begin
        w := Width;
        h := round(Width / v);
        if h > Height then
        begin
          h := Height;
          w := round(Height * v);
        end;
      end
      else
      begin
        h := Height;
        w := round(Height * v);
        if w > Width then
        begin
          w := Width;
          h := round(Width / v);
        end;
      end;
    end
    else
    begin
      w := Width;
      h := Height;
    end;
  end;

  with result do
  begin
    Left := 0;
    Top := 0;
    Right := w;
    Bottom := h;
  end;

  if FCenter then
  begin
    AdOffsetRect(result, ADestRect.Left + (Width - w) div 2,
                         ADestRect.Top + (Height - h) div 2);
  end
  else
  begin
    AdOffsetRect(result, ADestRect.Left, ADestRect.Top);
  end;
end;

destructor TAdCustomVideoPlayer.Destroy;
begin
  FParent.UnRegisterNotifyEvent(Notify);
  FImage.Free;
  inherited;
end;

procedure TAdCustomVideoPlayer.Draw(ASurface: TAdDraw; ADestRect: TAdRect);
begin
  if not HasFrame then exit;
  
  if ASurface <> nil then
    ASurface.Activate;
  
  if (Info.Width <> FImage.Width) or (Info.Height <> FImage.Height) then
    FImage.Restore;

  FImage.StretchDraw(ASurface, DestinationRect(ADestRect), 0);
end;

procedure TAdCustomVideoPlayer.Notify(ASender: TObject;
  AEvent: TAdSurfaceEventState);
begin
  case AEvent of
    seInitialize:
    begin
      InitializeTexture(FParent.AdAppl);
      FImage.Texture.Texture := Texture;
    end;
    seFinalize:
      FinalizeTexture;
  end;
end;

{ TAdVideoPlayer }

procedure TAdVideoPlayer.ClearData;
begin
  inherited;
  if FOwnStream and (FStream <> nil) then
    FStream.Free;

  FStream := nil;
  FOwnStream := false;
end;

constructor TAdVideoPlayer.Create(AParent: TAdDraw);
begin
  inherited; 
  FCriticalSection := TCriticalSection.Create;
end;

destructor TAdVideoPlayer.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TAdVideoPlayer.GetOpened: boolean;
begin
  result := inherited GetOpened and (FStream <> nil);
end;

function TAdVideoPlayer.GetPos: int64;
begin
  if FStream <> nil then
    result := FStream.Position
  else
    result := 0;
end;

function TAdVideoPlayer.GetSize: int64;
begin
  if FStream <> nil then  
    result := FStream.Size
  else
    result := 0;
end;

procedure TAdVideoPlayer.Open(AFile: string);
begin
  Close;

  FStream := TFileStream.Create(AFile, fmOpenRead or fmShareDenyNone);
  FOwnStream := true;

  if SearchDecoder then
    InitPlayer
  else
    ClearData;
end;

procedure TAdVideoPlayer.Open(AStream: TStream);
begin
  Close;

  FStream := AStream;
  FOwnStream := false;

  if SearchDecoder then
    InitPlayer
  else
    ClearData;
end;

function TAdVideoPlayer.ReadData(const Dest: Pointer; const Size: Cardinal): integer;
begin
  if FStream <> nil then
  begin
    FCriticalSection.Enter;
    try
      result := FStream.Read(Dest^, Size);
    finally
      FCriticalSection.Leave;
    end;
  end else
    result := 0;
end;

procedure TAdVideoPlayer.ResetData;
begin
  inherited;

  if FStream <> nil then
    FStream.Position := 0;
end;

procedure TAdVideoPlayer.Seek(APos: int64);
begin
  if FStream <> nil then
  begin
    FCriticalSection.Enter;
    try
      FStream.Position := APos;
    finally
      FCriticalSection.Leave;
    end;
  end;
end;

end.
