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
* Comment: Contains a simple class for rendering videos in a texture
}

{Contains a simple class for rendering videos in a texture.}
unit AdVideoTexture;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs, Contnrs,
  AdEvents, AdClasses, AdPersistent, AdTypes, AdBitmap;

type
  {Infos about the current video frame that are exchanged between the video
   plugin and the player.}
  TAdVideoInfo = record
    Width: integer; {< Width of the video frame in memory}
    Height: Integer; {< Height of the video frame in memory}
    FPS: Double; {< Frequency the frames should change}
    PixelAspect: Double; {< Some video formats are streched, like the PAL 
      16/9 Video. PixelAspect specifies the relative width of one pixel: A 
      value of 1.2 eg. means that a pixel is streched to 120% if its original
      size.}
  end;
  {Pointer on TAdVideoInfo.}
  PAdVideoInfo = ^TAdVideoInfo;

  {Contains information about an audio stream.}
  TAdAudioInfo = record
    SampleRate: Cardinal; {< Count of samples per second.}
    BitDepth: Cardinal; {< Bits per sample.}
    Channels: Cardinal; {< Count of channels.}
  end;
  {Pointer on TAdAudioInfo.}
  PAdAudioInfo = ^TAdAudioInfo;


  {Specifies the current position of the video.}
  TAdVideoPosition = record
    Hour: Byte; {< Time in hours.}
    Minute: Byte; {< Time in minutes.}
    Second: Byte; {< Time in seconds.}
    Frame: Integer; {< The frame number or the sample number.}
    Timecode: double; {< The current frame position in seconds - unseperated.}
  end;

  {Current video decoding state of the video decoder plugin.}
  TAdMediaDecoderState = (
    vdIncomplete, {< The frame data was incomplete, we have to transfer more
      data to the video decoder.}
    vdHasFrame, {< The video decoder found a frame in the data we provided.
      It can be received by calling the "GetPacket" method.}
    vdEnd {< There was a fatal error in the video stream or it indicated that
      the video has come to an end.}
  );

  {Represents the type of an media stream that is opened using the Andorra
   Video Player interface.}
  TAdMediaStreamType = (
    {This stream is an video stream and contains video data.}
    amVideo,
    {This stream is an audio stream and contains audio data.}
    amAudio,
    {This stream is an stream that may contain any data (e.g. subtitles).}
    amData
  );

  {Represents an decoded media package that is recived from the decoder.}
  TAdMediaPacket = record
    {Type of the decoded data.}
    StreamType: TAdMediaStreamType;
    {Stream the packet belongs to.}
    StreamIndex: integer;
    {Position of the packet on the video timeline.}
    Timecode: TAdVideoPosition;
    {May contains information about the video.}
    Info: array[0..127] of Byte;
    {Size of the buffer.}
    BufferSize: integer;
    {Pointer to the buffer.}
    Buffer: PByte;
  end;

  TAdMediaDecoder = class;

  {Contains information about all media streams in a media file and gives the
   possibility to activate or deactivate specific streams. By default the
   first media stream with the type "amVideo" is activated.}
  TAdMediaStream = class
    private
      FStreamType: TAdMediaStreamType;
      FParent: TAdMediaDecoder;
      FActive: boolean;
      FIndex: integer;
      procedure SetActive(AValue: boolean);
    protected
      procedure SetActiveBySender(AValue: boolean; ASender: TAdMediaStream);
    public
      {Creates an instance of TAdMedia stream.}
      constructor Create(AParent: TAdMediaDecoder; AIndex: integer;
        AStreamType: TAdMediaStreamType);

      {Sepcifies whether this media stream is currently active. Is this is the
       case, the media decoder will decode this stream and send decoded data
       to the decoder thread.}
      property Active: boolean read FActive write SetActive;
      {Specifies the type of the media stream.}
      property StreamType: TAdMediaStreamType read FStreamType;
      {Specifies the index of the stream}
      property StreamIndex: integer read FIndex;
  end;

  {Contains a list of media streams that have been found in the media file.}
  TAdMediaStreamList = class(TList)
    private
      function GetItem(AIndex: integer): TAdMediaStream;
    protected
      procedure Notify(ptr: Pointer; Action: TListNotification);override;
    public
      {Provides access on the media stream items.}
      property Items[AIndex: integer]: TAdMediaStream read GetItem; default;
  end;

  {Procedure that is used by the video decder thread to copy undecoded video
   data from file/stream etc. to the video decoder.}
  TAdMediaReadproc = function(const Dest: Pointer; const Size:Cardinal): integer of object;

  {Abstract video decoder class that can be implemented to provide a new video
   decoder class. Call the "RegisterVideoDecoder" procedure to register your
   new video decoder class.}
  TAdMediaDecoder = class(TAdPersistent)
    private
      FStreams: TAdMediaStreamList;
      FReadProc: TAdMediaReadproc;
    protected
      {Activates a specific media stream.}
      procedure SetStreamActive(AStreamIndex: integer; AActive: boolean);virtual;abstract;
    public
      {Creates an instance of TAdVideoDecoder.}
      constructor Create(AReadProc: TAdMediaReadproc); virtual;
      {Destorys the instance of TAdVideoDecoder.}
      destructor Destroy; override;

      {Tells the decoder to go on decoding. When data is needed, the specified
       callback should be called.
       @returns(The current decoder state)
       @seealso(TAdVideoDecoderState)
       @seealso(GetPacket)}
      function Decode: TAdMediaDecoderState;virtual;abstract;

      {If Decoder returned "vdHasFrame", this method will fill information about
       the decoded buffer and a pointer to this buffer in the packet structure.}
      procedure GetPacket(var Packet: TAdMediaPacket);virtual;abstract;

      {Initializes the decoder and stores all streams, that were found in the
       "MediaStreams" property.}
      procedure OpenDecoder;virtual;abstract;

      {Closes the decoder.}
      procedure CloseDecoder;virtual;abstract;

      {A list that gives an overview over all media streams that have been found in
       the media file.}
      property MediaStreams: TAdMediaStreamList read FStreams;

      {Pointer to the read callback procedure}
      property ReadProc: TAdMediaReadproc read FReadProc;
  end;
  
  {Class decleration for video decorders}
  TAdVideoDecoderClass = class of TAdMediaDecoder;

  {This class represents an element in the video decoder thread buffer.}
  TAdVideoMemory = class
    public
      {Pointer to the reserved memory}
      Memory: PByte;
      {Width of the buffer.}
      Width: integer;
      {Height of the buffer.}
      Height: integer;
      {Critical section that has to be entered when changing data of this
       video buffer object.}
      CriticalSection: TCriticalSection;

      {If this flag is set, this element in the buffer queue will be the last one.}
      StreamEnd: boolean;
      {Video information that has been received from the video decoder when
       saving the video data.}
      VideoInfo: TAdVideoInfo;
      {Video position information that has been received when saving the video
       data.}
      Time: TAdVideoPosition;

      {Idicates whether this video memory element has been used and can be overwritten.}
      Used: boolean;

      {Creates an instance of the video memory}
      constructor Create;
      {Destroys the instance of the video memory and frees reserved memory.}
      destructor Destroy; override;
      
      {Reserves an amout of buffer memory and stores the size in the width
       and height property.}
      procedure ReserveMemory(AWidth, AHeight: integer);
      {Clears the reserved memory.}
      procedure ClearMemory;
  end;

  {Event class trigerred when a non-video media packet is decoded by the
   video decoder thread.}
  TAdDecodeMediaPacketEvent = procedure(Sender: TObject; APckt: TAdMediaPacket) of object;

  {TAdMediaHandler is an abstract class that cares about handling the decoded 
   media packets that come from the media decoder. For instance, a video handler
   should care about buffering the decoded video frames. A audio handler should
   buffer the audio data and pass it to the used audio engine.
   @seealso(TAdMediaDecoderThread.RegisterMediaHandler)}
  TAdMediaHandler = class
    public
      {This function is called, when a media packet was decoded by the media decoder.
       The media handler should check the type of the package and store the data
       in an internal buffer and pass it to the graphics/audio engine.}
      procedure FeedPackage(var APckt: TAdMediaPacket);virtual; abstract;
      {This method is called when the media handler should be notified by the media decoder
       about a special state change (e.g. the end of the stream).}
      procedure Notify(AState: TAdMediaDecoderState);virtual; abstract;
      {This method is called when the media handler should flush its buffer.}
      procedure FlushBuffer; virtual; abstract;
      {Return true, if your media handler experiences a buffer underrun and needs new
       data.}
      function NeedsData: Boolean; virtual; abstract;
      {Return true, if your buffer contains data for more than one or two seconds video.}
      function Overflow: Boolean; virtual; abstract;
  end;

  {TAdMediaDecoder thread cares about decoding the video data and passing it to the registered
   media handlers.}
  TAdMediaDecoderThread = class(TThread)
    private
      FDecoder: TAdMediaDecoder;
      FMediaHandlers: TList;
      FCriticalSection: TCriticalSection;
    protected
      procedure Execute;override;
    public
      {Creates a new instance of TAdMediaDecoder. ADecoder specifies media decoder class
       that should be used to decode the video/audio data.}
      constructor Create(ADecoder: TAdMediaDecoder);
      {Destroys the instance of TAdMediaDecoderThread.}
      destructor Destroy;override;
      
      {Registers a handler in the media decoder.
       @param(AHandler specifies the media handler that should be registered within the media
         decoder. The media decoder gets its data via the "TAdMediaHandler.FeedPackage" method.)}
      procedure RegisterMediaHandler(AHandler: TAdMediaHandler);
      {Flushs the buffers of the decoder and all reigstered media handlers. This function
       is for the seek and stop functionality.}
      procedure FlushBuffers;
      
      {A list of all registered media handlers. Use the critical section property
       to synchronize access on this property.}
      property MediaHandlers: TList read FMediaHandlers;
      {A critical section that should be used when accessing the "MediaHandlers" or
       the "Decoder" property.}
      property CriticalSection: TCriticalSection read FCriticalSection;
      {A pointer on the media decoder.}
      property Decoder: TAdMediaDecoder read FDecoder;
  end;

  {TAdVideoHandler is the standard media handler for video frames. It is used within
   TAdVideoTexture. TAdVideoHandler contains a list of video frames that may be accessed
   by the "GetNextFrame" function.}
  TAdVideoHandler = class(TAdMediaHandler)
    private
      FVideoMemQueue: TList;
      FBufferSize: Integer;
      FFreeBufferCount: Integer;
      FCriticalSection: TCriticalSection;
      FStreamIndex: Integer;
      procedure ClearMem;
      function GetLastQueueItem: TAdVideoMemory;
      procedure CleanUp;
    public
      {Creates an instance of TAdVideoHandler.
       @param(ABufferSize specifies the number of frames that should at least be
        buffered)
       @param(AStreamIndex specifies the index of the media stream the video handler
        should take care of. The stream index can be changed over the "StreamIndex"
        property. TAdVideoHandler doesn't activate the stream, you have to do
        this yourself.)}
      constructor Create(ABufferSize: Integer; AStreamIndex: Integer);
      {Destroys the instance of TAdVideoHandler.}
      destructor Destroy; override;
      
      {This function is called, when a media packet was decoded by the media decoder.
       The video handler checks the type of the package and stores the data
       in an internal buffer and passes it to the graphics engine.}      
      procedure FeedPackage(var APckt: TAdMediaPacket); override;
      {This method is called when the video handler should be notified by the media decoder
       about a special state change (e.g. the end of the stream).}
      procedure Notify(AState: TAdMediaDecoderState); override;
      {Flushs the video frame buffer.}
      procedure FlushBuffer; override;
      {Returns true, when the video handler experiences a buffer underrun.}
      function NeedsData: boolean; override;
      {Returns true, when the video handler experiences a buffer overflow.}
      function Overflow: Boolean; override;

      {Returns the next frame in the video memory queue that can be processed by
       the graphic engine. Returns nil if no frame is available.}
      function GetNextFrame: TAdVideoMemory;

      {Critical section that is used to synchronize access on the decoded texture data.}
      property CriticalSection: TCriticalSection read FCriticalSection;
      {The index of the video stream.}
      property StreamIndex: Integer read FStreamIndex write FStreamIndex;
  end;

  {Represents the state of a video player.}
  TAdVideoPlayerState = (
    vpStopped, {< The player is currently stopped}
    vpPaused, {< The player is paused, what means that it alread has data and
      simply doesn't go on displaying it.}
    vpPlaying {< The player is currently playing the video data.}
  );

  {A simple object, that is capable of rendering video to a texture.
   TAdCustomVideoTexture doesn't contain any timing or loading functions,
   it only builds the main class, that will be extended by
   TAdVideoTexture. The main functions of TAdCustomVideoTexture are
   protected. The playback of a audio track is not possible now, because Andorra 2D
   is a graphic engine.}
  TAdCustomVideoTexture = class
    private
      FBufferSize: Cardinal;

      FTexture: TAd2dBitmapTexture;

      FParent: TAd2dApplication;

      FDecoder: TAdMediaDecoder;
      FDecoderThread: TAdMediaDecoderThread;
      FVideoHandler: TAdVideoHandler;

      FInfo: TAdVideoInfo;
      FTime: TAdVideoPosition;
      FStreamEnd: boolean;
      FHasFrame: boolean;

    protected
      function SearchDecoder: boolean;

      function GetOpened: boolean; virtual;
      function ReadData(const Dest: Pointer; const Size: Cardinal): integer;virtual;
      procedure ResetData;virtual;
      function NextFrame:boolean;virtual;

      procedure RegisterHandlers; virtual;

      procedure InitPlayer;virtual;
      procedure ClearData;virtual;

      procedure InitializeTexture(AParent: TAd2DApplication);
      procedure FinalizeTexture;

      property Texture: TAd2dBitmapTexture read FTexture;
      property Info: TAdVideoInfo read FInfo write FInfo;
      property Time: TAdVideoPosition read FTime write FTime;
      property Decoder: TAdMediaDecoder read FDecoder write FDecoder;
      property DecoderThread: TAdMediaDecoderThread read FDecoderThread;
      property VideoHandler: TAdVideoHandler read FVideoHandler;
      property StreamEnd: boolean read FStreamEnd write FStreamEnd;
      property HasFrame: boolean read FHasFrame;
    public
      {Creates an instance of TAdVideoPlayer.}
      constructor Create(AParent: TAd2dApplication);
      {Destroys the instance.}
      destructor Destroy;override;
  end;

  {TAdVideoPlayer is extends TAdCustomVideoTexture by the capability
   of actually playing the video with the right speed. TAdVideoTexture
   is still not able to read video data from a stream or a file. This
   has to be done by manually overriding the protected ReadData method from
   TAdCustomVideoTexture. Instead you are also able to use the TAdVideoPlayer
   object from the unit AdVideo.}
  TAdVideoTexture = class(TAdCustomVideoTexture)
    private
      FTimeGap: double;
      FFPS: integer;
      FTmpFPS: integer;
      FTmpTime: double;
      FState: TAdVideoPlayerState;
      FLoop: boolean;
      FSpeed: double;
      FFrameTime: double;

      FOnPlay: TAdNotifyEvent;
      FOnStop: TAdNotifyEvent;
      FOnPause: TAdNotifyEvent;
      FOnClose: TAdNotifyEvent;
      FOnNextFrame: TAdNotifyEvent;

      procedure SetSpeed(AValue:double);
    protected
      procedure DoPlay;virtual;
      procedure DoPause;virtual;
      procedure DoStop;virtual;
      procedure DoNextFrame;virtual;
      procedure DoClose;virtual;

      property TimeGap: double read FTimeGap;
    public
      {Creates an instance of TAdVideoTexture.}
      constructor Create(AParent: TAd2dApplication);

      {Starts to play the video.}
      procedure Play;virtual;
      {Pauses video playback.}
      procedure Pause;virtual;
      {Stops video playback (resets the data source, clears all buffers)}
      procedure Stop;virtual;
      {Destroys all created video playback objects.}
      procedure Close;virtual;
      {Moves the timer of TAdVideoTexture on, so that playback continues.
       @param(ATimeGap specifies the time that has been passed since the
         last call of ATimeGap in seconds)}
      procedure Move(ATimeGap:double);virtual;

      {Texture that can be accessed when you want to draw the film.}
      property Texture;
      {Time information about the current displayed frame.
       @seealso(TAdVideoPosition)}
      property Time;
      {Information about the current video frame.
       @seealso(TAdVideoInformation)}
      property Info;
      {Current state of the player.}
      property State: TAdVideoPlayerState read FState;
      {The FPS the video is currently played with.}
      property CurrentFPS:integer read FFPS;
      {Set this property if you want the video to loop.}
      property Loop: boolean read FLoop write FLoop;
      {Use this property to vary playback speed. Default is 1. For example, 0.5
       would mean that the video is played with the half speed.}
      property Speed: double read FSpeed write SetSpeed;

      {Event that is triggered when playback starts.}
      property OnPlay:TAdNotifyEvent read FOnPlay write FOnPlay;
      {Event that is triggered when playback stops.}
      property OnStop:TAdNotifyEvent read FOnStop write FOnStop;
      {Event that is triggered when video playback is pasued.}
      property OnPause:TAdNotifyEvent read FOnPause write FOnPause;
      {Event that is triggered, when a new frame is displayed. This
       event can e.g. be used to synchronize the video to an audio buffer.}
      property OnNextFrame:TAdNotifyEvent read FOnNextFrame write FOnNextFrame;
      {Event that is triggered when the decoder is closed.}
      property OnClose:TAdNotifyEvent read FOnClose write FOnClose;
  end;

var
  {Contains all registered video decoder classes.}
  RegisteredVideoDecoders: TStringList;

{Registeres a new video decoder class. This should normally be done in the 
 initialization section of the corresponding unit.}
procedure RegisterVideoDecoder(AVideoDecoder: TAdVideoDecoderClass);

{Calculates the hour, minute and the seconds value from the timecode value.}
procedure FillTimeInfo(var ATimeInfo: TAdVideoPosition);

implementation

procedure RegisterVideoDecoder(AVideoDecoder: TAdVideoDecoderClass);
begin
  RegisteredVideoDecoders.Add(AVideoDecoder.ClassName);
  AdRegisterClass(AVideoDecoder);
end;

procedure FillTimeInfo(var ATimeInfo: TAdVideoPosition);
begin
  ATimeInfo.Hour := round(ATimeInfo.Timecode) div 3600;
  ATimeInfo.Minute := round(ATimeInfo.Timecode) div 60 mod 60;
  ATimeInfo.Second := round(ATimeInfo.Timecode) mod 60;
end;

{ TAdVideoPlayer }

constructor TAdCustomVideoTexture.Create(AParent: TAd2dApplication);
begin
  inherited Create;

  InitializeTexture(AParent);

  FBufferSize := 4096;
end;

destructor TAdCustomVideoTexture.Destroy;
begin
  ClearData;

  FinalizeTexture;
  
  if FDecoder <> nil then
    FDecoder.Free;

  inherited;
end;

procedure TAdCustomVideoTexture.FinalizeTexture;
begin
  if FTexture <> nil then
    FreeAndNil(FTexture);
end;

procedure TAdCustomVideoTexture.InitializeTexture(AParent: TAd2DApplication);
begin
  FinalizeTexture;
  FParent := AParent;
  FTexture := FParent.CreateBitmapTexture;
end;

function TAdCustomVideoTexture.GetOpened: boolean;
begin
  result := (FDecoderThread <> nil)
end;

procedure TAdCustomVideoTexture.ClearData;
begin
  FHasFrame := false;
  if (GetOpened) then
  begin
    try
      FDecoderThread.OnTerminate := nil;
      FDecoderThread.Terminate;
      FDecoderThread.Free;
    finally
      FDecoderThread := nil;
    end;

    //Destroy the video handler
    if FVideoHandler <> nil then
      FVideoHandler.Free;

    FVideoHandler := nil;
  end;
end;

procedure TAdCustomVideoTexture.InitPlayer;
begin
  //Create the decoder thread
  FDecoderThread := TAdMediaDecoderThread.Create(FDecoder);

  RegisterHandlers;

  //Start the decoder thread.
  FDecoderThread.Resume;
end;

function TAdCustomVideoTexture.NextFrame:boolean;
var
  adbmp:TAdBitmap;
  buf: TAdVideoMemory;
begin
  result := false;
  if FVideoHandler <> nil then
  begin
    buf := FVideoHandler.GetNextFrame;
    if buf <> nil then
    begin
      buf.CriticalSection.Enter;
      try
        FInfo := buf.VideoInfo;
        FTime := buf.Time;
        FStreamEnd := buf.StreamEnd;

        if not FStreamEnd then
        begin
          adbmp := TAdBitmap.Create;

          adbmp.ReserveMemory(buf.Width, buf.Height);
          System.Move(buf.Memory^, adbmp.Scanline^, adbmp.Size);

          FTexture.LoadFromBitmap(adbmp, ad32Bit);

          adbmp.Free;
          
          FHasFrame := true;
        end;
      finally
        buf.CriticalSection.Leave;
        result := true;
      end;
    end;
  end;
end;

function TAdCustomVideoTexture.ReadData(const Dest: Pointer; const Size: Cardinal): integer;
begin
  //Children of this class should actually read the data
  result := 0;
end;

procedure TAdCustomVideoTexture.RegisterHandlers;
var
  i: integer;
begin
  //Search for a video stream. If one exists, create the video stream handler.
  //If no video stream exists, ancestor classes may handle the other types of
  //data in the media file (e.g. audio or subtitle data).
  for i := 0 to FDecoder.MediaStreams.Count - 1 do
    if FDecoder.MediaStreams[i].FStreamType = amVideo then
    begin
      //Create and register the video handler in the decoder thread. The decoder
      //thread will now send information to the video handler whenever a piece of
      //the media file has been decoded.
      FVideoHandler := TAdVideoHandler.Create(3, i);
      FDecoderThread.RegisterMediaHandler(FVideoHandler);

      break;
    end;
end;

procedure TAdCustomVideoTexture.ResetData;
begin
  if GetOpened then
  begin
    FDecoderThread.FlushBuffers;
  end;
  FStreamEnd := false;
  FHasFrame := false;
end;

function TAdCustomVideoTexture.SearchDecoder: boolean;
var
  i: integer;
  cref: TAdVideoDecoderClass;
begin
  result := false;

  if FDecoder <> nil then
  begin
    FDecoder.Free;
    FDecoder := nil;
  end;

  ResetData;

  for i := 0 to RegisteredVideoDecoders.Count - 1 do
  begin
    cref := TAdVideoDecoderClass(AdGetClass(RegisteredVideoDecoders[i]));
    //Create an instance of the decoder
    FDecoder := cref.Create(ReadData);

    //Initialize the decoder and check whether media streams have been
    //found.
    FDecoder.OpenDecoder;
    if FDecoder.MediaStreams.Count = 0 then
    begin
      //No streams have been found. The decoder doesn't support the media data
      FDecoder.Free;
      FDecoder := nil;
    end else
    begin
      result := true;
      break;
    end;
  end;
end;

{ TAdVideoMemory }

constructor TAdVideoMemory.Create;
begin
  inherited;

  StreamEnd := false;
  CriticalSection := TCriticalSection.Create;
end;

destructor TAdVideoMemory.Destroy;
begin
  ClearMemory;

  CriticalSection.Free;
  inherited;
end;

procedure TAdVideoMemory.ClearMemory;
begin
  if Memory <> nil then
  begin
    FreeMem(Memory, Width * Height * 4);
    Memory := nil;
    Width := 0;
    Height := 0;
    StreamEnd := false;
  end;
end;

procedure TAdVideoMemory.ReserveMemory(AWidth, AHeight: integer);
begin
  if (Width <> AWidth) and (Height <> AHeight) then
  begin
    GetMem(Memory, AWidth * AHeight * 4);
    Width := AWidth;
    Height := AHeight;
  end;
end;

{ TAdMediaDecoder }

constructor TAdMediaDecoder.Create(AReadProc: TAdMediaReadproc);
begin
  inherited Create;
  FStreams := TAdMediaStreamList.Create;
  FReadProc := AReadProc;
end;

destructor TAdMediaDecoder.Destroy;
begin
  FStreams.Free;
  inherited;
end;

{ TAdVideoTexure }

constructor TAdVideoTexture.Create(AParent: TAd2dApplication);
begin
  inherited;

  SetSpeed(1);
end;

procedure TAdVideoTexture.DoClose;
begin
  if Assigned(FOnClose) then
    FOnClose(self);
end;

procedure TAdVideoTexture.DoNextFrame;
begin
  if Assigned(FOnNextFrame) then
    FOnNextFrame(self);
end;

procedure TAdVideoTexture.DoPause;
begin
  if Assigned(FOnPause) then
    FOnPause(self);
end;

procedure TAdVideoTexture.DoPlay;
begin
  if Assigned(FOnPlay) then
    FOnPlay(self);
end;

procedure TAdVideoTexture.DoStop;
begin
  if Assigned(FOnStop) then
    FOnStop(self);
end;

procedure TAdVideoTexture.Close;
begin
  FState := vpStopped;
  ClearData;
  DoClose;
end;

procedure TAdVideoTexture.Pause;
begin
  if FState = vpPlaying then
  begin
    //Pause video decoder thread
    if DecoderThread <> nil then
      FDecoderThread.Suspend;
      
    FState := vpPaused;
    DoPause;
  end;
end;

procedure TAdVideoTexture.Play;
begin
  if GetOpened then
  begin
    FStreamEnd := false;
    FState := vpPlaying;

    //Resume video decoder
    if DecoderThread <> nil then
      DecoderThread.Resume;

    DoPlay;
  end;
end;

procedure TAdVideoTexture.SetSpeed(AValue: double);
begin
  FSpeed := AValue;
  FFrameTime := 0.04 / FSpeed;
end;

procedure TAdVideoTexture.Stop;
begin
  if GetOpened then
  begin
    ResetData;
    FState := vpStopped;
    DoStop;
  end;
end;

procedure TAdVideoTexture.Move(ATimeGap: double);
begin
  if GetOpened and (State = vpPlaying) then
  begin
    FTmpTime := FTmpTime + ATimeGap;
    FTimeGap := FTimeGap + ATimeGap;
    if (FTimeGap > FFrameTime) then
      if NextFrame then
      begin
        FTmpFPS := FTmpFPS + 1;
        FTimeGap := FTimeGap - FFrameTime;
        DoNextFrame;
      end;

    if StreamEnd then
    begin
      Stop;
      if FLoop then
        Play;
    end;

    if FTmpTime > 1 then
    begin
      FFPS := FTmpFPS;
      FTmpFPS := 0;
      FTmpTime := 0;
    end;
  end;
end;

{ TAdMediaStream }

constructor TAdMediaStream.Create(AParent: TAdMediaDecoder; AIndex: integer;
  AStreamType: TAdMediaStreamType);
begin
  inherited Create;
  FIndex := AIndex;
  FParent := AParent;
  FStreamType := AStreamType;
end;

procedure TAdMediaStream.SetActive(AValue: boolean);
begin
  SetActiveBySender(AValue, self);
end;

procedure TAdMediaStream.SetActiveBySender(AValue: boolean;
  ASender: TAdMediaStream);
var
  i: integer;
begin
  //Only one audio and one video stream can be active at once. If you don't belive
  //this, try to activate two audio streams in VLC...
  if FStreamType <> amData then
    for i := 0 to FParent.MediaStreams.Count - 1 do
      if (FParent.MediaStreams[i].FStreamType = ASender.FStreamType) and
         (FParent.MediaStreams[i] <> self) and
         (FParent.MediaStreams[i] <> ASender) then
      begin
        FParent.SetStreamActive(i, false);
        FParent.MediaStreams[i].FActive := false;
      end;

  if AValue <> FActive then
    FParent.SetStreamActive(FIndex, AValue);

  FActive := AValue;
end;

{ TAdMediaStreamList }

function TAdMediaStreamList.GetItem(AIndex: integer): TAdMediaStream;
begin
  result := inherited Items[AIndex];
end;

procedure TAdMediaStreamList.Notify(ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    TAdMediaStream(ptr).Active := false;
    TAdMediaStream(ptr).Free;
  end;
end;

{ TAdMediaDecoderThread }

constructor TAdMediaDecoderThread.Create(ADecoder: TAdMediaDecoder);
begin
  inherited Create(true);

  FCriticalSection := TCriticalSection.Create;
  FMediaHandlers := TList.Create;
  FDecoder := ADecoder;

  //Start the thread
  Resume;
end;

destructor TAdMediaDecoderThread.Destroy;
begin
  //Wait for the decoder thread to be ready.
  FCriticalSection.Enter; FCriticalSection.Leave;
  FMediaHandlers.Free;
  FCriticalSection.Free;
  inherited Destroy;
end;

procedure TAdMediaDecoderThread.Execute;
var
  needframe: boolean;
  i: integer;
  state: TAdMediaDecoderState;
  pckt: TAdMediaPacket;
begin
  try
    while (not Terminated) and (FDecoder <> nil) do
    begin
      //Search for a media handler that needs new buffer data
      needframe := false;

      FCriticalSection.Enter;
      try
        for i := 0 to FMediaHandlers.Count - 1 do
          if TAdMediaHandler(FMediaHandlers[i]).NeedsData then
          begin
            //At least one decoder needs new decoded data.
            needframe := true;
            break;
          end;

        for i := 0 to FMediaHandlers.Count - 1 do
          if TAdMediaHandler(FMediaHandlers[i]).Overflow then
          begin
            //At least one decoder is flooded with data!
            needframe := false;
            break;
          end;
      finally
        FCriticalSection.Leave;
      end;

      //If a frame is needed, decode a new one - else sleep a while
      if needframe then
      begin
        //Decode a frame
        state := FDecoder.Decode;

        FCriticalSection.Enter;
        try
          case state of
            vdHasFrame:
            begin
              FDecoder.GetPacket(pckt);
              for i := 0 to FMediaHandlers.Count - 1 do
                TAdMediaHandler(FMediaHandlers[i]).FeedPackage(pckt);
            end;
            vdEnd:
            begin
              for i := 0 to FMediaHandlers.Count - 1 do
                TAdMediaHandler(FMediaHandlers[i]).Notify(state);

              //Close thread
              Terminate;
            end;
          end;
        finally
          FCriticalSection.Leave;
        end;
      end else
        Sleep(5);
    end;
  finally
    //
  end;
end;

procedure TAdMediaDecoderThread.FlushBuffers;
var
  i: Integer;
begin
  FCriticalSection.Enter;
  try
    for i := 0 to FMediaHandlers.Count - 1 do
      TAdMediaHandler(FMediaHandlers[i]).FlushBuffer;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TAdMediaDecoderThread.RegisterMediaHandler(AHandler: TAdMediaHandler);
begin
  FCriticalSection.Enter;
  try
    FMediaHandlers.Add(AHandler);
  finally
    FCriticalSection.Leave;
  end;
end;

{ TAdVideoHandler }

constructor TAdVideoHandler.Create(ABufferSize: Integer; AStreamIndex: Integer);
begin
  inherited Create;

  FBufferSize := ABufferSize;
  FStreamIndex := AStreamIndex;
  FFreeBufferCount := 0;

  FVideoMemQueue := TList.Create;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TAdVideoHandler.Destroy;
begin
  ClearMem;
  
  FVideoMemQueue.Free;
  FCriticalSection.Free;
  
  inherited;
end;

procedure TAdVideoHandler.CleanUp;
begin
  if (FVideoMemQueue.Count > 0) and
     (TAdVideoMemory(FVideoMemQueue[FVideoMemQueue.Count - 1]).Used) then
  begin
    TAdVideoMemory(FVideoMemQueue[FVideoMemQueue.Count - 1]).Free;
    FVideoMemQueue.Delete(FVideoMemQueue.Count - 1);
    FFreeBufferCount := FFreeBufferCount - 1;
  end;
end;

procedure TAdVideoHandler.ClearMem;
var
  i: integer;
begin
  //Free the video memory entries
  for i := 0 to FVideoMemQueue.Count - 1 do
    TAdVideoMemory(FVideoMemQueue[i]).Free;

  //Clear the memory queue
  FVideoMemQueue.Clear;
end;

function TAdVideoHandler.GetLastQueueItem: TAdVideoMemory;
var
  i: integer;
begin
  result := nil;

  FCriticalSection.Enter;
  try
    //Search the last free video memory item in the queue...
    for i := 0 to FVideoMemQueue.Count - 1 do
      if TAdVideoMemory(FVideoMemQueue[i]).Used then
      begin
        result := TAdVideoMemory(FVideoMemQueue[i]);
        break;
      end;

    //... if no item has been found, create a new one and add it to the queue
    if result = nil then
    begin
      result := TAdVideoMemory.Create;
      FVideoMemQueue.Add(result);
    end else
      //The returned buffer will be (or at least should be) used to store data in.
      //So after this, we have one buffer less, that is free.
      if FFreeBufferCount > 0 then      
        FFreeBufferCount := FFreeBufferCount - 1;

    //Reset the "used" flag of the package.
    result.Used := false;
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TAdVideoHandler.FeedPackage(var APckt: TAdMediaPacket);
begin
  //Break if the specified package isn't a video frame or has the wrong stream
  //index.
  if not ((APckt.StreamIndex = FStreamIndex) and (APckt.StreamType = amVideo)) then
    exit;
    
  with GetLastQueueItem do
  begin
    //Enter the critical section of the video frame buffer
    CriticalSection.Enter;

    try
      Time := APckt.Timecode;
      VideoInfo := PAdVideoInfo(@APckt.Info)^;
      StreamEnd := false;

      //Copy the video packet data into the video buffer
      ReserveMemory(VideoInfo.Width, VideoInfo.Height);
      Move(APckt.Buffer^, Memory^, APckt.BufferSize);
    finally
      CriticalSection.Leave;
    end;
  end;
end;

procedure TAdVideoHandler.FlushBuffer;
begin
  ClearMem;
end;

function TAdVideoHandler.GetNextFrame: TAdVideoMemory;
begin
  FCriticalSection.Enter;
  try         
    if FVideoMemQueue.Count > FBufferSize * 1.5 then
      CleanUp;

    result := nil;
    if (FVideoMemQueue.Count > 0) and
       (not TAdVideoMemory(FVideoMemQueue[0]).Used) then
    begin
      result := TAdVideoMemory(FVideoMemQueue[0]);
      result.Used := true;

      FFreeBufferCount := FFreeBufferCount + 1;

      //Remove the item from the queue list and add it to the end.
      FVideoMemQueue.Delete(0);
      FVideoMemQueue.Add(result);
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TAdVideoHandler.NeedsData: boolean;
begin
  result := (FVideoMemQueue.Count < FBufferSize) or (FFreeBufferCount >= FBufferSize);
end;

procedure TAdVideoHandler.Notify(AState: TAdMediaDecoderState);
begin
  FCriticalSection.Enter;
  try
    //Indicate the next buffer frame as last frame.
    if AState = vdEnd then
      GetLastQueueItem.StreamEnd := true;
  finally
    FCriticalSection.Leave;
  end;
end;

function TAdVideoHandler.Overflow: Boolean;
begin
  result := (FVideoMemQueue.Count > FBufferSize * 5);
end;

initialization
  RegisteredVideoDecoders := TStringList.Create;

finalization
  RegisteredVideoDecoders.Free;

end.
