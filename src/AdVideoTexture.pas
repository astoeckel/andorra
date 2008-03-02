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

unit AdVideoTexture;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes, SyncObjs, Contnrs,
  AdEvents, AdClasses, AdPersistent, AdTypes, AdBitmap;

type
  TAdVideoInfo = record
    Width, Height: Integer;
    FPS: Byte;
    PixelAspect: Double;
  end;

  TAdVideoPosition = record
    Hour: Byte;
    Minute: Byte;
    Second: Byte;
    Frame: Byte;
  end;

  TAdVideoDecoderState = (vdIncomplete, vdHasFrame, vdEnd);

  TAdVideoDecoder = class(TAdPersistent)
    public
      constructor Create; virtual;

      class function SupportsFile(ABuf: Pointer; ASize: Cardinal):boolean;virtual;abstract;

      function ReadBuffer(ABuf: Pointer; ASize: Cardinal): TAdVideoDecoderState;virtual;abstract;
      function GetVideoInfo: TAdVideoInfo;virtual;abstract;
      function GetVideoPosition: TAdVideoPosition;virtual;abstract;
      procedure FillBuffer(ABuffer: Pointer);virtual;abstract;
  end;
  TAdVideoDecoderClass = class of TAdVideoDecoder;

  TAdVideoMemory = class
    public
      Memory: PByte;
      Width, Height: integer;
      CriticalSection: TCriticalSection;

      StreamEnd: boolean;
      VideoInfo: TAdVideoInfo;
      Time: TAdVideoPosition;

      constructor Create;
      destructor Destroy; override;
      
      procedure ReserveMemory(AWidth, AHeight: integer);
      procedure ClearMemory;
  end;

  TAdVideoReadproc = procedure(const Dest: Pointer; var Size:Cardinal) of object;

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
      constructor Create(ABufferSize: Cardinal; AReadProc: TAdVideoReadProc;
        ADecoder: TAdVideoDecoder);
      destructor Destroy;override;
      function GetNextFrame: TAdVideoMemory;
      procedure InvalidateBuffers;
  end;

  TAdVideoPlayerState = (vpStopped, vpPaused, vpPlaying);

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
    public
      constructor Create(AParent: TAd2dApplication);
      destructor Destroy;override;
  end;

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
      constructor Create(AParent: TAd2dApplication);

      procedure Play;virtual;
      procedure Pause;virtual;
      procedure Stop;virtual;
      procedure Close;virtual;
      procedure Move(ATimeGap:double);virtual;

      property Texture;
      property Time;
      property Info;
      property State: TAdVideoPlayerState read FState;
      property CurrentFPS:integer read FFPS;
      property Loop: boolean read FLoop write FLoop;
      property Speed: double read FSpeed write SetSpeed;

      property OnPlay:TAdNotifyEvent read FOnPlay write FOnPlay;
      property OnStop:TAdNotifyEvent read FOnStop write FOnStop;
      property OnPause:TAdNotifyEvent read FOnPause write FOnPause;
      property OnNextFrame:TAdNotifyEvent read FOnNextFrame write FOnNextFrame;
      property OnClose:TAdNotifyEvent read FOnClose write FOnClose;
  end;

var
  RegisteredVideoDecoders: TStringList;
  CacheId: integer;

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
  aparams: TAd2dBitmapTextureParameters;
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

          with aparams do
          begin
            BitDepth := 32;
            UseMipMaps := false;
            MagFilter := atLinear;
            MinFilter := atLinear;
            MipFilter := atLinear;
          end;

          FTexture.LoadFromBitmap(adbmp, aparams);

          adbmp.Free;
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
