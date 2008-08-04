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
  {@exclude}
  TAdMPEG2Decoder = class(TAdMediaDecoder)
    private
      FDecoder: PMPEG2_Decoder;
      FInfo: PMPEG2_Info;
      FReadProc: TAdMediaReadproc;

      buf: TAd2dBitmap;
      src_buf: PByte;
      src_buf_size: Cardinal;

      procedure WriteBitmap(AWidth, AHeight:integer; AMem: Pointer);
    public
      constructor Create(AReadProc: TAdMediaReadproc);override;
      destructor Destroy;override;

      procedure CloseDecoder;override;
      procedure OpenDecoder;override;

      function Decode: TAdMediaDecoderState; override;
      procedure GetPacket(var APacket: TAdMediaPacket); override;
    end;

implementation

const
  default_buf_size = 1024;

{ TAdMPEG2Decoder }

constructor TAdMPEG2Decoder.Create(AReadProc: TAdMediaReadproc);
begin
  inherited;
  buf := TAd2dBitmap.Create;

  FReadProc := AReadProc;
end;

destructor TAdMPEG2Decoder.Destroy;
begin
  buf.Free;
  inherited;
end;

procedure TAdMPEG2Decoder.GetPacket(var APacket: TAdMediaPacket);
begin
  if (FInfo <> nil) then
  begin
    if (FInfo^.sequence <> nil) then
    begin
      with PAdVideoInfo(@APacket.Info)^ do
      begin
        Width := FInfo^.sequence^.width;
        Height := FInfo^.sequence^.height;
        FPS := 25;
        PixelAspect := FInfo^.sequence^.pixel_width / FInfo^.sequence^.pixel_height;
      end;
    end;
    if (FInfo^.gop <> nil) then
    begin
      with APacket.Timecode do
      begin
        Hour := FInfo^.gop^.hours;
        Minute := FInfo^.gop^.minutes;
        Second := FInfo^.gop^.seconds;
        Frame := FInfo^.gop^.pictures;
        Timecode := (Hour * 3600) + (Minute * 60) + Second + (Frame / 25);
      end;
    end;

    APacket.StreamType := amVideo;
    APacket.StreamIndex := 0;
    APacket.Buffer := buf.ScanLine;
    APacket.BufferSize := buf.Size;
  end;
end;

procedure TAdMPEG2Decoder.CloseDecoder;
begin
  MediaStreams.Clear;
  
  if FDecoder <> nil then
  begin
    mpeg2_close(FDecoder);
    FDecoder := nil;
    FInfo := nil;

    if src_buf <> nil then
    begin
      //Free reserved memory
      FreeMemory(src_buf);

      src_buf := nil;
    end;
  end;
end;

procedure TAdMPEG2Decoder.OpenDecoder;
var
  tmp: TAdMediaStream;
begin
  CloseDecoder;

  FDecoder := mpeg2_init;
  if FDecoder <> nil then
  begin
    FInfo := mpeg2_info(FDecoder);

    //Reserve read memory
    GetMem(src_buf, default_buf_size);
    src_buf_size := default_buf_size;

    tmp := TAdMediaStream.Create(self, 0, amVideo);
    MediaStreams.Add(tmp);
  end;
end;

function TAdMPEG2Decoder.Decode: TAdMediaDecoderState;
var
  pstart, pend: PByte;
  state: TMPEG2_State;
  size: Cardinal;
begin
  result := vdIncomplete;     

  while true do
  begin
    state := mpeg2_parse(FDecoder);
    case state of
      STATE_BUFFER:
      begin
        //Read data...
        size := FReadProc(src_buf, src_buf_size);

        pstart := src_buf;
        pend := src_buf;

        inc(pend, size);

        //...and decode it
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
