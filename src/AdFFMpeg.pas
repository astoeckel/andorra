unit AdFFMpeg;

interface

uses
  SysUtils, Classes,
  AdBitmapClass, AdVideoTexture,
  avcodec, avformat, avio, avutil;

type
  TAdFFMpegDecoder = class(TAdVideoDecoder)
    private
      FContext: PAVFormatContext;
      FCodecContext: PAVCodecContext;
      FUrlContext: TURLContext;
      FIO: TByteIOContext;
      FCodec: PAVCodec;
      FVideoStream: integer;
      FFrame: PAVFrame;
      FFrameRGBA: PAVFrame;
      FBuf: PByte;
      FBufSize: Int64;
      FInputFormat: PAVInputFormat;

      FIOBuf: PChar;

      FDataBuffer: TMemoryStream;

      FProtocol: TURLProtocol;

      FDataPosition: int64;

      procedure OpenDecoder;
      procedure CloseDecoder;
    public
      constructor Create;override;
      destructor Destroy;override;

      class function SupportsFile(ABuf: Pointer; ASize: Cardinal):boolean; override;

      function ReadBuffer(ABuf: Pointer; ASize: Cardinal): TAdVideoDecoderState; override;
      function GetVideoInfo: TAdVideoInfo; override;
      function GetVideoPosition: TAdVideoPosition; override;
      procedure FillBuffer(ABuffer: Pointer); override;
      
      property DataBuffer: TMemoryStream read FDataBuffer;
      property DataPosition: int64 read FDataPosition write FDataPosition;
  end;

implementation


const
  AVFMT_NOFILE      =  $0001;
  AVFMT_NEEDNUMBER  =  $0002;
  AVFMT_NOHEADER    =  $0004;

  AVFMT_SHOW_IDS    =  $0008;
  AVFMT_RAWPICTURE  =  $0020;


//In order to read data from memory using FFMpeg, we need to write our own
//URL-Protocol.

function StreamRead(h: PURLContext; buf: pchar; size: integer): integer; cdecl;
var
  ADecoder: TAdFFMpegDecoder;
begin
  ADecoder := TAdFFMpegDecoder(h^.priv_data);

  //Store data in the data buffer
  ADecoder.DataBuffer.Position := ADecoder.DataPosition;
  ADecoder.DataBuffer.Read(buf^, size);
  ADecoder.DataPosition := ADecoder.DataPosition + size;

  result := 0;
end;

{ TAdMPEG2Decoder }

constructor TAdFFMpegDecoder.Create;
begin
  inherited Create;

  FDataBuffer := TMemoryStream.Create; 
end;

destructor TAdFFMpegDecoder.Destroy;
begin
  CloseDecoder;
  FDataBuffer.Free;
  inherited;
end;

procedure TAdFFMpegDecoder.FillBuffer(ABuffer: Pointer);
begin
  Move(FBuf^, ABuffer^, FBufSize);
end;

function TAdFFMpegDecoder.GetVideoInfo: TAdVideoInfo;
begin
  with result do
  begin
    Width := FCodecContext^.width;
    Height := FCodecContext^.height;
    FPS := 25;
    PixelAspect := 1;
   
    //TODO
  end;
end;

function TAdFFMpegDecoder.GetVideoPosition: TAdVideoPosition;
begin
  //TODO
end;


procedure TAdFFMpegDecoder.CloseDecoder;
begin
  //Free reserved memory
  if FContext <> nil then
  begin
    //Free frame buffer
    av_free(FBuf);
    av_free(FFrameRGBA);

    //Free frame
    av_free(FFrame);

    //Close context
    avcodec_close(FCodecContext);

    //Close input file
    av_close_input_file(FContext);

    FContext := nil;
  end;

  //Reset the data buffer
  FDataBuffer.Clear;
  FDataPosition := 0;
end;

procedure TAdFFMpegDecoder.OpenDecoder;
var
  i: integer;
  avpd: TAVProbeData;  
begin
  //Register all FFMpegVideo formats for playback, can be called more than once
  av_register_all();

  //Read format type
  avpd.filename := '';
  avpd.buf := FDataBuffer.Memory;
  avpd.buf_size := FDataBuffer.Size;

  FInputFormat := av_probe_input_format(@avpd, 1);

  //Write protocol data
  with FProtocol do
  begin
    name := 'AdFFMpeg I/O Wrapper';
    url_open := nil;
    url_read := @StreamRead;
    url_write := nil;
    url_seek := nil;
    url_close := nil;
    next := nil;
  end;

  FURLContext.priv_data := self;
  FURLContext.prot := @FProtocol;

  GetMem(FIOBuf, 1 shl 20);

  init_put_byte(@FIO, FIOBuf, 1 shl 20, 0,
                @FURLContext, @StreamRead, nil, nil);
  FIO.is_streamed := integer(true);
  FIO.buffer_size := 1 shl 20;

  //Allocate format context
  FContext := av_alloc_format_context();

  //FFMpeg most not call the fopen/fclose url functions
  FInputFormat.flags := FInputFormat.flags or AVFMT_NOFILE;

  //Open stream. Using the name of our registered protocol, FFMpeg will call our
  //streaming functions.
  av_open_input_stream(FContext, @FIO, '', FInputFormat, nil);

  FInputFormat.flags := FInputFormat.flags xor AVFMT_NOFILE;
  
  //Reset data position
//  FDataPosition := 0;


  if av_find_stream_info(FContext) >= 0 then
  begin
    FVideoStream := -1;

    for i := 0 to FContext^.nb_streams - 1 do
    begin
      if (FContext^.streams[i]^.codec^.codec_type = CODEC_TYPE_VIDEO) then
      begin
        FCodecContext := FContext^.streams[i]^.codec;
        FVideoStream := i;
        break;
      end;
    end;

    FCodec := avcodec_find_decoder(FCodecContext^.codec_id);

    avcodec_open(FCodecContext, FCodec);

    FFrame := avcodec_alloc_frame;

    FFrameRGBA := avcodec_alloc_frame;

    FBufSize := avpicture_get_size(PIX_FMT_RGB32, FCodecContext^.width,
                               FCodecContext^.height);

    FBuf := av_malloc(FBufSize);

    avpicture_fill(PAVPicture(FFrameRGBA), FBuf, PIX_FMT_RGB32,
                   FCodecContext^.width, FCodecContext^.height);
  end;
end;

function TAdFFMpegDecoder.ReadBuffer(ABuf: Pointer;
  ASize: Cardinal): TAdVideoDecoderState;
var
  frameFinished: integer;
  Packet: TAVPacket;
begin
  //Write data to the end of the data buffer
  FDataBuffer.Position := FDataBuffer.Size;
  FDataBuffer.Write(ABuf^, ASize);

  //Open decoder if not already done
  if FContext = nil then
    OpenDecoder;

  result := vdIncomplete;

  if av_read_frame(FContext, @Packet) >= 0 then
  begin
    // Is this a packet from the video stream?
    if(Packet.stream_index = FVideoStream) then
    begin
       // Decode video frame
      avcodec_decode_video(FCodecContext, FFrame, @frameFinished,
                           Packet.data, Packet.size);

      // Did we get a video frame?
      if (Boolean(frameFinished)) then
      begin
        // Convert the image from its native format to RGB
        (*mg_convert(PAVPicture(FFrameRGBA), PIX_FMT_RGB32,
                    PAVPicture(FFrame),     FCodecContext^.pix_fmt,
                       FCodecContext^.width, FCodecContext^.height);*)

        result := vdHasFrame;
      end;
    end;
   
    // Free the packet that was allocated by av_read_frame
    av_destruct_packet_nofree(@Packet);
  end else
    result := vdEnd;    
end;

class function TAdFFMpegDecoder.SupportsFile(ABuf: Pointer;
  ASize: Cardinal): boolean;
begin
  result := true;
end;

initialization
  RegisterVideoDecoder(TAdFFMpegDecoder);

end.
