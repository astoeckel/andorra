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
* File: AdPersistent.pas
* Comment:Adds MPEG-2-Video (*.m2v files) playback capabilities to Andorra 2D.
}

{Adds MPEG-2-Video (*.m2v files) playback capabilities to Andorra 2D.}
unit AdMPEG2Video;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  AdBitmapClass, AdVideoTexture, libmpeg2;

type
  TAdMPEG2Decoder = class(TAdVideoDecoder)
    private
      FDecoder: PMPEG2_Decoder;
      FInfo: PMPEG2_Info;

      buf: TAd2dBitmap;

      procedure WriteBitmap(AWidth, AHeight:integer; AMem: Pointer); 

      procedure CloseDecoder;
      procedure OpenDecoder;    
    public
      constructor Create;override;
      destructor Destroy;override;
      
      class function SupportsFile(ABuf: Pointer; ASize: Cardinal):boolean; override;

      function ReadBuffer(ABuf: Pointer; ASize: Cardinal): TAdVideoDecoderState; override;
      function GetVideoInfo: TAdVideoInfo; override;
      function GetVideoPosition: TAdVideoPosition; override;
      procedure FillBuffer(ABuffer: Pointer); override;
  end;

implementation

{ TAdMPEG2Decoder }

procedure TAdMPEG2Decoder.CloseDecoder;
begin
  if FDecoder <> nil then
  begin
    mpeg2_close(FDecoder);
    FDecoder := nil;
    FInfo := nil;
  end;
end;

constructor TAdMPEG2Decoder.Create;
begin
  inherited Create;
  buf := TAd2dBitmap.Create;
end;

destructor TAdMPEG2Decoder.Destroy;
begin
  buf.Free;
  inherited;
end;

procedure TAdMPEG2Decoder.FillBuffer(ABuffer: Pointer);
begin
  if buf.Loaded then
    Move(buf.Scanline^, ABuffer^, buf.Size);
end;

function TAdMPEG2Decoder.GetVideoInfo: TAdVideoInfo;
begin
  with result do
  begin
    Width := 0;
    Height := 0;
    FPS := 0;
    PixelAspect := 1;
    
    if (FInfo <> nil) and (FInfo^.sequence <> nil) then
    begin
      Width := FInfo^.sequence^.width;
      Height := FInfo^.sequence^.height;
      FPS := 25;
      PixelAspect := FInfo^.sequence^.pixel_width / FInfo^.sequence^.pixel_height;
    end;
  end;
end;

function TAdMPEG2Decoder.GetVideoPosition: TAdVideoPosition;
begin
  with result do
  begin
    Hour := 0; Minute := 0; Second := 0; Frame := 0;
    if (FInfo <> nil) and (FInfo^.gop <> nil) then
    begin
      Hour := FInfo^.gop^.hours;
      Minute := FInfo^.gop^.minutes;
      Second := FInfo^.gop^.seconds;
      Frame := FInfo^.gop^.pictures;
    end;
  end;
end;

procedure TAdMPEG2Decoder.OpenDecoder;
begin
  CloseDecoder;

  FDecoder := mpeg2_init;
  if FDecoder <> nil then
  begin
    FInfo := mpeg2_info(FDecoder);
  end;
end;

function TAdMPEG2Decoder.ReadBuffer(ABuf: Pointer;
  ASize: Cardinal): TAdVideoDecoderState;
var
  pstart, pend: PByte;
  state: TMPEG2_State;
begin
  if FDecoder = nil then
    OpenDecoder;

  result := vdIncomplete;


  while true do
  begin
    state := mpeg2_parse(FDecoder);
    case state of
      STATE_BUFFER:
      begin
        pstart := ABuf;
        pend := ABuf;

        inc(pend, ASize);

        mpeg2_buffer(FDecoder, pstart, pend);

        Break;
      end;
      STATE_SEQUENCE: 
      begin
        mpeg2_convert(FDecoder, mpeg2convert_rgb32, nil);
      end;
      STATE_SLICE:
      begin
        if FInfo^.display_fbuf <> nil then
        begin
          result := vdHasFrame;
          WriteBitmap(FInfo^.sequence^.width, FInfo^.sequence^.height,
            FInfo^.display_fbuf^.buf[0]);
        end;
      end;
      STATE_END, STATE_INVALID, STATE_INVALID_END:
      begin
        result := vdEnd;
        CloseDecoder;
        Break;
      end;
    end;
  end;
end;

class function TAdMPEG2Decoder.SupportsFile(ABuf: Pointer;
  ASize: Cardinal): boolean;
begin
  result := true;
end;

procedure TAdMPEG2Decoder.WriteBitmap(AWidth, AHeight: integer; AMem: Pointer);
var
  i: Cardinal;
  PRGBA1: PCardinal;
  PRGBA2: PCardinal;
begin
  if FInfo <> nil then
  begin
    if FInfo^.display_fbuf <> nil then
    begin
      buf.ReserveMemory(AWidth, AHeight);

      PRGBA1 := PCardinal(AMem);
      PRGBA2 := PCardinal(buf.ScanLine);
      
      for i := 0 to AWidth * AHeight -1 do
      begin
        PRGBA2^ := PRGBA1^ or $FF000000;
        inc(PRGBA1); inc(PRGBA2);
      end;
    end;
  end;
end;

initialization
  RegisterVideoDecoder(TAdMPEG2Decoder);

finalization

end.
