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
* Comment: Contains a simple class for loading a video from a stream and rendering it on the surface
}

unit AdVideo;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes,
  AdTypes, AdClasses, AdDraws, AdVideoTexture;

type
  TAdCustomVideoPlayer = class(TAdVideoTexture)
    private
      FParent: TAdDraw;
      FImage: TAdImage;
      FProportional: boolean;
      FStretch: boolean;
      FCenter: boolean;
      FOldWidth, FOldHeight: integer;
    protected
      function DestinationRect(ADestRect: TAdRect):TAdRect;
      property Image: TAdImage read FImage;
    public
      constructor Create(AParent: TAdDraw);
      destructor Destroy;override;

      procedure Draw(ASurface: TAdDraw; ADestRect: TAdRect);

      property Proportional: boolean read FProportional write FProportional;
      property Stretch: boolean read FStretch write FStretch;
      property Center: boolean read FCenter write FCenter;    
  end;

  TAdVideoPlayer = class(TAdCustomVideoPlayer)
    private
      FStream: TStream;
      FOwnStream: boolean;
    protected
      function GetOpened:boolean;override;
      procedure ClearData;override;
      procedure ReadData(const Dest: Pointer; var Size: Cardinal);override;
      procedure ResetData;override;
    public
      procedure Open(AFile: string);overload;
      procedure Open(AStream: TStream);overload;
  end;

implementation

{ TAdCustomVideoPlayer }

constructor TAdCustomVideoPlayer.Create(AParent: TAdDraw);
begin
  inherited Create(AParent.AdAppl);

  FParent := AParent;
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
  FImage.Free;
  inherited;
end;

procedure TAdCustomVideoPlayer.Draw(ASurface: TAdDraw; ADestRect: TAdRect);
begin
  if (not Texture.Loaded) or (not GetOpened) then exit;
  
  if (FImage.Width <> FOldWidth) or (FImage.Height <> FOldHeight) then
    FImage.Restore;

  FOldWidth := Info.Width;
  FOldHeight := Info.Height;

  FImage.StretchDraw(ASurface, DestinationRect(ADestRect), 0);
end;

{ TAdVideoPlayer }

procedure TAdVideoPlayer.ClearData;
begin
  inherited;
  if FOwnStream and (FStream <> nil) then
  begin
    FStream.Free;
  end;
  FStream := nil;
  FOwnStream := false;
end;

function TAdVideoPlayer.GetOpened: boolean;
begin
  result := inherited GetOpened and (FStream <> nil);
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

procedure TAdVideoPlayer.ReadData(const Dest: Pointer; var Size: Cardinal);
begin
  Size := FStream.Read(Dest^, Size);
end;

procedure TAdVideoPlayer.ResetData;
begin
  inherited;
  if FStream <> nil then
  begin
    FStream.Position := 0;
  end;
end;

end.
