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
    FPS: Byte; {< Frequency the frames should change}
    PixelAspect: Double; {< Some video formats are streched, like the PAL 
      16/9 Video. PixelAspect specifies the relative width of one pixel: A 
      value of 1.2 eg. means that a pixel is streched to 120% if its original
      size.}
  end;

  {Specifies the current position of the video.}
  TAdVideoPosition = record
    Hour: Byte; {< Time in hours.}
    Minute: Byte; {< Time in minutes.}
    Second: Byte; {< Time in seconds.}
    Frame: Byte; {< The frame number}
  end;

  {Current video decoding state of the video decoder plugin.}
  TAdVideoDecoderState = (
    vdIncomplete, {< The frame data was incomplete, we have to transfer more
      data to the video decoder.}
    vdHasFrame, {< The video decoder found a frame in the data we provided.
      It can be received by calling the "FillBuffer" method.}
    vdEnd {< There was a fatal error in the video stream or it idicated that
      the video has come to an end.}
  );

  {Abstract video decoder class that can be implemented to provide a new video
   decoder class. Call the "RegisterVideoDecoder" procedure to register your
   new video decoder class.}
  TAdVideoDecoder = class(TAdPersistent)
    public
      {Creates an instance of TAdVideoDecoder.}
      constructor Create; virtual;

      {Returns whether this file format is supported.
       @param(ABuf is the pointer to the first byte of the data in memory)
       @param(ASize is the size of the data in memory)
       @returns(true, if the video format is known and can be loaded)}
      class function SupportsFile(ABuf: Pointer; ASize: Cardinal):boolean;virtual;abstract;

      {Tells the decoder to read a buffer from memory.
       @param(ABuf is the pointer to the first byte of the data in memory)
       @param(ASize is the size of the data in memory)
       @returns(The current decoder state)
       @seealso(TAdVideoDecoderState)}
      function ReadBuffer(ABuf: Pointer; ASize: Cardinal): TAdVideoDecoderState;virtual;abstract;
      {Returns the video information for the current frame loaded.
       @seealso(TAdVideoInfo)}
      function GetVideoInfo: TAdVideoInfo;virtual;abstract;
      {Returns the current position of the video.
       @seealso(TAdVideoPosition)}
      function GetVideoPosition: TAdVideoPosition;virtual;abstract;
      {If ReadBuffer returned "vdHasFrame", this method will fill the given buffer
       with video data. The buffer has already allocated and can be filled with
       FrameWidth * FrameHeight * 4 Bytes. The video format is 32Bit, RGBA.}
      procedure FillBuffer(ABuffer: Pointer);virtual;abstract;
  end;
  
  {Class decleration for video decorders}
  TAdVideoDecoderClass = class of TAdVideoDecoder;

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

      {If this flag is set, this element in the buffer queue is the last one.}
      StreamEnd: boolean;
      {Video information that has been received from the video decoder when
       saving the video data.}
      VideoInfo: TAdVideoInfo;
      {Video position information that has been received when saving the video
       data.}
      Time: TAdVideoPosition;

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

  {Procedure that is used by the video decder thread to copy undecoded video
   data from file/stream etc. to the video decoder.}
  TAdVideoReadproc = procedure(const Dest: Pointer; var Size:Cardinal) of object;

  {Video decoder thread that is internally used by TAdCustomVideoTexture. The
   video decoder thread decodes up to 4 frames in the background while the 
   video is playing.}
  TAdVideoDecoderThread = class(TThread)
    private
      FVideoMem: array of TAdVideoMemory;
      FVideoMemIndex: integer;
      FVideoMemQueue: TObjectQueue;
      FReadProc: TAdVideoReadProc;
      FDecoder: TAdVideoDecoder;
      FCriticalSection: TCriticalSection;
      FBufferSize: Cardinal;
    protected
      procedure Execute;override;
    public
      {Creates an instance of TAdVideoDecoderThread.
       @param(ABufferSize specifies the size of the buffer the thread should use
         to transfer undecoded video data to the video decoder.)
       @param(AReadProc specifies the method that should be called when undecoded
         video data should be read.)}
      constructor Create(ABufferSize: Cardinal; AReadProc: TAdVideoReadProc;
        ADecoder: TAdVideoDecoder);
      {Destroys this instance of TAdVideoDecoderThread.}
      destructor Destroy;override;
      {Returns the next frame in the video memory queue that can be processed by
       the graphic engine.}
      function GetNextFrame: TAdVideoMemory;
      {Buffers in queue are marked as invalid, so that the queue is filled new.}
      procedure InvalidateBuffers;
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

      FDecoder: TAdVideoDecoder;
      FDecoderThread: TAdVideoDecoderThread;

      FInfo: TAdVideoInfo;
      FTime: TAdVideoPosition;
      FStreamEnd: boolean;
      FHasFrame: boolean;

      procedure VideoDecoderThreadTerminate(Sender: TObject);
    protected
      function SearchDecoder: boolean;

      function GetOpened: boolean; virtual;
      procedure ReadData(const Dest: Pointer; var Size: Cardinal);virtual;
      procedure ResetData;virtual;
      function NextFrame:boolean;virtual;

      procedure InitPlayer;virtual;
      procedure ClearData;virtual;

      property Texture: TAd2dBitmapTexture read FTexture;
      property Info: TAdVideoInfo read FInfo write FInfo;
      property Time: TAdVideoPosition read FTime write FTime;
      property Decoder: TAdVideoDecoder read FDecoder write FDecoder;
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

implementation

procedure RegisterVideoDecoder(AVideoDecoder: TAdVideoDecoderClass);
begin
  RegisteredVideoDecoders.Add(AVideoDecoder.ClassName);
  AdRegisterClass(AVideoDecoder);
end;

{ TAdVideoPlayer }

constructor TAdCustomVideoTexture.Create(AParent: TAd2dApplication);
begin
  inherited Create;

  FParent := AParent;

  FTexture := FParent.CreateBitmapTexture;

  FBufferSize := 4096;
end;

destructor TAdCustomVideoTexture.Destroy;
begin
  ClearData;

  FTexture.Free;
  
  if FDecoder <> nil then
    FDecoder.Free;

  inherited;
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
    ResetData;
    
    try
      FDecoderThread.OnTerminate := nil;
      FDecoderThread.Terminate;
      FDecoderThread.Free;
    finally
      FDecoderThread :=nil;
    end;
  end;
end;

procedure TAdCustomVideoTexture.InitPlayer;
begin
  FDecoderThread := TAdVideoDecoderThread.Create(
    FBufferSize, ReadData, FDecoder);
  FDecoderThread.OnTerminate := VideoDecoderThreadTerminate;
  FDecoderThread.Resume;
end;

function TAdCustomVideoTexture.NextFrame:boolean;
var
  adbmp:TAdBitmap;
  buf: TAdVideoMemory;
begin
  result := false;
  if FDecoderThread <> nil then
  begin
    buf := FDecoderThread.GetNextFrame;
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

procedure TAdCustomVideoTexture.ReadData(const Dest: Pointer; var Size: Cardinal);
begin
  //
end;

procedure TAdCustomVideoTexture.ResetData;
begin
  if GetOpened then
  begin
    FDecoderThread.InvalidateBuffers;
  end;
  FStreamEnd := false;
  FHasFrame := false;
end;

function TAdCustomVideoTexture.SearchDecoder: boolean;
var
  i: integer;
  cref: TAdVideoDecoderClass;
  buf: PByte;
  size: Cardinal;
begin
  result := false;

  if FDecoder <> nil then
  begin
    FDecoder.Free;
    FDecoder := nil;
  end;

  GetMem(buf, 128);

  Size := 128;
  ReadData(buf, size);
  ResetData;

  for i := 0 to RegisteredVideoDecoders.Count - 1 do
  begin
    cref := TAdVideoDecoderClass(AdGetClass(RegisteredVideoDecoders[i]));
    result := cref.SupportsFile(buf, size);
    if result then
    begin
      FDecoder := cref.Create;
      break;
    end;
  end;

  FreeMem(buf, 128);
end;

procedure TAdCustomVideoTexture.VideoDecoderThreadTerminate(Sender: TObject);
begin
  TAdVideoDecoderThread(Sender).Free;
  if Sender = FDecoderThread then
  begin
    FDecoderThread := nil;
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

{ TAdVideoDecoderThread }

constructor TAdVideoDecoderThread.Create(ABufferSize: Cardinal;
  AReadProc: TAdVideoReadProc; ADecoder: TAdVideoDecoder);
var
  i: integer;
begin
  inherited Create(true);

  FBufferSize := ABufferSize;
  FReadProc := AReadProc;
  FDecoder := ADecoder;

  FreeOnTerminate := false;

  FCriticalSection := TCriticalSection.Create;
  FVideoMemQueue := TObjectQueue.Create;

  SetLength(FVideoMem, 4);
  for i := 0 to High(FVideoMem) do
    FVideoMem[i] := TAdVideoMemory.Create;

  FVideoMemIndex := 0;
end;

destructor TAdVideoDecoderThread.Destroy;
var
  i: integer;
begin
  for i := 0 to High(FVideoMem) do
    FVideoMem[i].Free;
    
  FVideoMemQueue.Free;
  FCriticalSection.Free;
  
  inherited;
end;

procedure TAdVideoDecoderThread.Execute;
var
  buf: Pointer;
  state: TAdVideoDecoderState;
  ainfo: TAdVideoInfo;
  size: Cardinal;
  cansleep: boolean;
begin
  GetMem(buf, FBufferSize);
  try
    while not Terminated do
    begin
      cansleep := false;
      FCriticalSection.Enter;
      if FVideoMemQueue.Count < Length(FVideoMem) then
      begin
        //Read Buffer
        size := FBufferSize;
        FReadProc(buf, size);
        if size <> 0 then
        begin
          state := FDecoder.ReadBuffer(buf, size);
          if state = vdHasFrame then
          begin
            ainfo := FDecoder.GetVideoInfo;
            with FVideoMem[FVideoMemIndex] do
            begin
              CriticalSection.Enter;
              Time := FDecoder.GetVideoPosition;
              VideoInfo := ainfo;
              StreamEnd := false;

              ReserveMemory(ainfo.Width, ainfo.Height);
              FDecoder.FillBuffer(Memory);

              CriticalSection.Leave;
            end;

            FVideoMemQueue.Push(FVideoMem[FVideoMemIndex]);

            FVideoMemIndex := FVideoMemIndex + 1;
            if FVideoMemIndex > High(FVideoMem) then
              FVideoMemIndex := 0;
          end else
          if state = vdEnd then
          begin
            with FVideoMem[FVideoMemIndex] do
            begin
              CriticalSection.Enter;
              StreamEnd := true;
              CriticalSection.Leave;
            end;
            FVideoMemQueue.Push(FVideoMem[FVideoMemIndex]);
          end;
        end else
        begin
          with FVideoMem[FVideoMemIndex] do
          begin
            CriticalSection.Enter;
            StreamEnd := true;
            CriticalSection.Leave;
          end;
          FVideoMemQueue.Push(FVideoMem[FVideoMemIndex]);
        end;

      end else
        CanSleep := true;
      FCriticalSection.Leave;

      if CanSleep then
        Sleep(1);
    end;
  finally
    FreeMem(buf, FBufferSize);
  end;
end;

function TAdVideoDecoderThread.GetNextFrame: TAdVideoMemory;
begin
  result := nil;
  FCriticalSection.Enter;
  try
    if FVideoMemQueue.Count > 0 then
      result := TAdVideoMemory(FVideoMemQueue.Pop);
  finally
    FCriticalSection.Leave;
  end;
end;

procedure TAdVideoDecoderThread.InvalidateBuffers;
begin
  FCriticalSection.Enter;
  try
    while FVideoMemQueue.Count > 0 do
      FVideoMemQueue.Pop;
  finally
    FCriticalSection.Leave;
  end;
end;

{ TAdVideoDecoder }

constructor TAdVideoDecoder.Create;
begin
  inherited Create;
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

initialization
  RegisteredVideoDecoders := TStringList.Create;

finalization
  RegisteredVideoDecoders.Free;

end.
