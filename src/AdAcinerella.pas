{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL. 
* You also should have recieved a copy of this license with this file. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.

* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdAcinerella.pas
* Comment: Adds video playback support to Andorra using Acinerella. Remember that Acinerella
 is licensed under the GPL.
}

{Adds video playback support to Andorra using the FFMpeg Wrapper Acinerella.
 Remember that Acinerella is licensed under the GPL.}
unit AdAcinerella;

interface

uses
  AdBitmapClass, AdVideoTexture, acinerella;

type
  {@exclude}
  TAdAcinerellaDecoder = class(TAdMediaDecoder)
    private
      pInstance: PAc_instance;
      pDecoders: array of PAc_decoder;
      pLastDecoder: PAc_decoder;

      hasvideodecoder: boolean;

      function ConvertStreamType(AType:TAc_stream_type): TAdMediaStreamType; 
    protected
      procedure SetStreamActive(AStreamIndex: integer; AActive: boolean);override;
    public
      constructor Create(AReadProc: TAdMediaReadproc);override;
      destructor Destroy;override;

      function Decode: TAdMediaDecoderState; override;
      procedure GetPacket(var APacket: TAdMediaPacket); override;

      procedure OpenDecoder;override;
      procedure CloseDecoder;override;
  end;

implementation

function read_proc(sender: Pointer; buf: PByte; size: integer): integer; cdecl;
begin
  result := TAdAcinerellaDecoder(sender).ReadProc(buf, size);
end;

{ TAdAcinerellaDecoder }

function TAdAcinerellaDecoder.ConvertStreamType(
  AType: TAc_stream_type): TAdMediaStreamType;
begin
  case AType of
    AC_STREAM_TYPE_VIDEO: result := amVideo;
    AC_STREAM_TYPE_AUDIO: result := amAudio;
  else
    result := amData;
  end;
end;

constructor TAdAcinerellaDecoder.Create;
begin
  inherited;
end;

destructor TAdAcinerellaDecoder.Destroy;
begin
  CloseDecoder;
  inherited;
end;

procedure TAdAcinerellaDecoder.GetPacket(var APacket: TAdMediaPacket);
begin
  //Copy frame information data into the media file
  if pLastDecoder <> nil then
  begin
    APacket.StreamType := ConvertStreamType(pLastDecoder^.stream_info.stream_type);
    APacket.StreamIndex := pLastDecoder^.stream_index;

    APacket.Buffer := pLastDecoder^.buffer;
    APacket.BufferSize := pLastDecoder^.buffer_size;

    APacket.Timecode.Timecode := pLastDecoder^.timecode;
    FillTimeInfo(APacket.Timecode);

    //Set stream type specific information
    if APacket.StreamType = amVideo then
    begin
      //Set packet video info
      with PAdVideoInfo(@APacket.Info)^ do
      begin
        Width := pLastDecoder^.stream_info.video_info.frame_width;
        Height := pLastDecoder^.stream_info.video_info.frame_height;
        FPS := 1 / pLastDecoder^.stream_info.video_info.frames_per_second;
        PixelAspect := pLastDecoder^.stream_info.video_info.pixel_aspect;
        APacket.Timecode.Frame := round((round(pLastDecoder^.timecode * 1000) mod 1000) / (1000 / FPS));
      end;
    end else
    if APacket.StreamType = amAudio then
    begin
      //Set packet audio info
      with PAdAudioInfo(@APacket.Info)^ do
      begin
        SampleRate := pLastDecoder^.stream_info.audio_info.samples_per_second;
        BitDepth := pLastDecoder^.stream_info.audio_info.bit_depth;
        Channels := pLastDecoder^.stream_info.audio_info.channel_count;
        APacket.Timecode.Frame := round((round(pLastDecoder^.timecode * 1000) mod 1000) / (1000 / SampleRate));
      end;
    end;
  end;
end;

procedure TAdAcinerellaDecoder.CloseDecoder;
var
  i: integer;
begin
  //Clear the media stream list
  MediaStreams.Clear;
  hasvideodecoder := false;

  if pInstance <> nil then
  begin
    //Destroy all opened decoders
    for i := 0 to Length(pDecoders) - 1 do
      if pDecoders[i] <> nil then
        ac_free_decoder(pDecoders[i]);

    //Set the length of the decoders array to zero.
    SetLength(pDecoders, 0);

    ac_close(pInstance);
    ac_free(pInstance);

    pInstance := nil;
    pLastDecoder := nil;
  end;
end;

procedure TAdAcinerellaDecoder.OpenDecoder;
var
  i: integer;
  info: TAc_stream_info;
  tmp: TAdMediaStream;
  st: TAdMediaStreamType;
begin
  CloseDecoder;

  //Init Acinerella
  pInstance := ac_init;
  pInstance^.output_format := AC_OUTPUT_RGBA32;
  ac_open(pInstance, self, nil, @read_proc, nil);

  SetLength(pDecoders, pInstance^.stream_count);

  //Search Video decoder
  for i := 0 to pInstance^.stream_count - 1 do
  begin
    pDecoders[i] := nil;
    //Fetch info about the current stream
    ac_get_stream_info(pInstance, i, @info);

    //Convert the type of the media stream from Acinerella to Andorra. Only
    //Video and Audio streams are supported.
    st := ConvertStreamType(info.stream_type);
    if st = amData then
      continue;

    //Create a media stream handler and add it to the media stream list.
    tmp := TAdMediaStream.Create(self, i, st);
    MediaStreams.Add(tmp);

    if (not hasvideodecoder) and (st = amVideo) then
    begin
      hasvideodecoder := true;
      tmp.Active := true;
    end;
  end;
end;

function TAdAcinerellaDecoder.Decode: TAdMediaDecoderState;
var
  pckg: PAc_package;
begin
  result := vdIncomplete;

  if (pInstance <> nil) and (pInstance^.opened) then
  begin
    //Read package from data stream.
    pckg := ac_read_package(pInstance);
    if pckg <> nil then
    begin
      //Decode package if corresponding decoder is activated
      if (pDecoders[pckg^.stream_index] <> nil) and
         (ac_decode_package(pckg, pDecoders[pckg^.stream_index]) > 0)  then
      begin
        pLastDecoder := pDecoders[pckg^.stream_index];
        result := vdHasFrame;
      end;

      ac_free_package(pckg);
    end else
      //No package had been found, we are at the end of the stream
      result := vdEnd;
  end else
    result := vdEnd;
end;

procedure TAdAcinerellaDecoder.SetStreamActive(AStreamIndex: integer;
  AActive: boolean);
begin
  if AActive then
  begin
    //Create a new Acinerella decoder instance
    if pDecoders[AStreamIndex] = nil then
      pDecoders[AStreamIndex] := ac_create_decoder(pInstance, AStreamIndex);
  end else
  begin
    //Deactivate decoder by simply freeing it.
    if pDecoders[AStreamIndex] <> nil then
      ac_free_decoder(pDecoders[AStreamIndex]);

    pDecoders[AStreamIndex] := nil;
  end;
end;

initialization
  RegisterVideoDecoder(TAdAcinerellaDecoder);

end.
