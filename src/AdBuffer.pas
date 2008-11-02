{
* THIS PROGRAM IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OR
* CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED INCLUDING, WITHOUT
* LIMITATION, ANY WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT,
* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
*
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more informations.
* You also should have recieved a copy of this license with this file.
* 
* Inspite of the incompatibility between the Common Public License (CPL) and the GNU General Public License (GPL) you're allowed to use this program 
* under the GPL. 
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdBuffer.pas
* Comment: Contains various classes that are usefull for buffering data. A simple
*   buffered filestream class that speeds up byte access on file streams is also
*   included.
}

{Contains various classes that are usefull for buffering data. A simple buffered
 filestream class that speeds up byte access on file streams is also included.}
unit AdBuffer;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes;

type
  {TAdBufferBucket represents a single buffer bucket in the buffer bucket list.
   Buffer buckets are used to seperate the memory into serveral sections, in
   order to reorder the buckets.
   Data is read from the bucket until it is "empty". If the bucket is empty, it
   may be refilled and added to the buffer queue/list again.
   Every bucket may contain a differtent amout of data.
   @seealso(TAdBufferBucketList)}
  TAdBufferBucket = class
    private
      FIsFree: boolean;
      FMemory: PByte;
      FMemorySize: Integer;
      FFilledSize: Integer;
      FReadPos: Integer;
      FTag: Cardinal;
      FPosition: int64;
      FDataSize: integer;
      procedure FreeMem;
    public
      {Creates an instance of TAdBufferBucket.}
      constructor Create;
      {Destroys the instance of TAdBufferBucket and frees all reserved memory.}
      destructor Destroy;override;

      {Writes data to the bucket. The memory of the bucket is enlarged, if size
       is bigger than the last size.}
      procedure WriteData(ABuf: PByte; ASize: Integer);
      {Reads data from the bucket. Returns the count of bytes that were actually
       read from the bucket. Every call of ReadData increments a internal pointer,
       so that the next call will read the data from that position.}
      function ReadData(ABuf: PByte; ASize: Integer): Integer;
      {Seeks the internall read pointer of the bucket forward or backward.}
      function Seek(AOffset: integer): integer;

      {Returns whether all data from the item has been read.}
      property IsFree: boolean read FIsFree;
      {Returns the count of bytes that are still in the bucket. "FilledSize" gets
       decremented with each call of "ReadData".}
      property FilledSize: Integer read FFilledSize;
      {May contain a user defined tag. This tag may be usefull if you want to
       use the buffer in a audio application to store the current position of
       this element.}
      property Tag: Cardinal read FTag write FTag;
      {Position is used internally by the buffer classes for seeking and contains
       the byte position of the buffer bucket since the last "Clear".}
      property Position: int64 read FPosition write FPosition;
      {Specifies the position of the internal read pointer.}
      property ReadPos: Integer read FReadPos;
      {The size of the data that was written to the bucket with the last write
       access.}
      property DataSize: integer read FDataSize;
  end;

  {TAdBufferBucketList is a list class that contains serveral TAdBufferBucket
   elements. TAdBufferBucketList is internally used as a queue for the buffer
   bucket itemts.}
  TAdBufferBucketList = class(TList)
    private
      function GetItem(AIndex: integer): TAdBufferBucket;
      procedure SetItem(AIndex: integer; AItem: TAdBufferBucket);
    public
      {Provides access on every single element in the list.}
      property Items[Index: integer]: TAdBufferBucket read GetItem write SetItem;default;
  end;

  {TAdBuffer is class that allows you to buffer data by writing data to the end
   of the buffer and reading it from the beginning. TAdBuffer is not thread safe,
   you have to protect every function call with a critical section when using
   multiple threads.}
  TAdBuffer = class
    private
      FFilled: integer;
      FBucketList: TAdBufferBucketList;
      FCurrentTag: Cardinal;
      FCurrentPosition: int64;
    public
      {Creates an instance of TAdBuffer.}
      constructor Create;
      {Destroys the instance of TAdBuffer.}
      destructor Destroy; override;
      
      {Writes "Size"-Bytes from "Buf" to the buffer.
       Every write operation stores the data in a different buffer bucket. Be
       sure to write relatively big buffers to the buffer. The size of the written
       data should also not variate.
       The "Tag" parameter allows you to give the data a special tag. This
       parameter is very usefull, e.g. if you want to add the playback position
       to your decoded audio data that you want to store in the buffer.}
      procedure Write(Buf: PByte; Size: Integer; Tag: Cardinal = 0);
      {Reads data from the buffer and returns the count of bytes that were actually
       read.}
      function Read(Buf: PByte; Size: Integer): integer;
      {Seeks forward or backward in the buffer. The given value is relative to
       the current stream position. A negative value will make the buffer seek
       backwards, a positive value will make it seek forward.}
      function Seek(Offset: Integer): Boolean;
      {Clears the buffer.}
      procedure Clear;

      {Returns the amount of data that is currently in the buffer.}
      property Filled: integer read FFilled;
      {Current tag is the tag you specified in the write method of the last
       buffer bucket that was read.}
      property CurrentTag: Cardinal read FCurrentTag;
  end;

  {TAdBufferedFileStram is a class that provides high speed read access on streams,
   by buffering parts of the stream.
   Instead of using this class directly, you may also use the QueryBufferedStream
   and the FreeBufferedStream classes.}
  TAdBufferStreamAdapter = class(TStream)
    private
      FBuffer: TAdBuffer;
      FStream: TStream;
      FMem: PByte;
      FPosition: int64;
      FNewSeekPos: int64;
      FInstanceCount: integer;
      procedure DoSeek;
    protected
      procedure SetSize(const NewSize: Int64); override;
    public
      {Creates a new instance of TAdBufferStreamAdapter.
       @param(AStream specifies the stream that should be buffered.)}
      constructor Create(AStream: TStream);
      {Destroys the instance of TAdBufferStreamAdapter.}
      destructor Destroy; override;
      
      {Reads "Count" bytes to the specified buffer and returns the count of bytes
       that were actually read.}
      function Read(var Buffer; Count: Longint): Longint; override;
      {Writes "Count" bytes from the specified buffer and returns the count of bytes
       that were actually written.}
      function Write(const Buffer; Count: Longint): Longint; override;
      {Seeks to the specified position in the stream.}
      function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

      {Property that is used by the QueryBufferedStream and FreeBufferedStream
       functions. InstanceCount gets incremented with every call of
       QueryBufferedStream and decremented with every call of FreeBufferedStream.
       If InstanceCount reaches zero, the instance of TAdBufferStreamAdapter
       is freed.}
      property InstanceCount: integer read FInstanceCount write FInstanceCount;
      {The source stream the TAdBufferStreamAdapter uses.}
      property AdaptedStream: TStream read FStream;
  end;

  {TAdBufferedFileStream is a class, that descends from TAdBufferStreamAdapter to
   provide high speed access on a file stream.}
  TAdBufferedFileStream = class(TAdBufferStreamAdapter)
    private
      FFileStream: TFileStream;
    public
      {Creates a instance of TAdBufferedFileStream. See your help on TFileStream
       for more info.}
      constructor Create(const AFileName: string; Mode: Word); overload;
      {Creates a instance of TAdBufferedFileStream. See your help on TFileStream
       for more info.}
      constructor Create(const AFileName: string; Mode: Word; Rights: Cardinal); overload;
      destructor Destroy; override;
  end;

{Speeds up the given stream using the TAdBufferStreamAdapter class. Simply call
 this method and use the stream you want to accelerate as parameter. Calls of
 QueryBufferedStream and FreeBufferedStream may be cascading.}
procedure QueryBufferedStream(var AStream: TStream);
{Frees a buffered stream and returns the original stream. Calls of
 QueryBufferedStream and FreeBufferedStream may be cascading.}
procedure FreeBufferedStream(var AStream: TStream);

const
  AdBufferSize = 4096;

implementation

procedure QueryBufferedStream(var AStream: TStream);
begin
  //We don't have to buffer TMemoryStream because its data already is "buffered"
  if not (AStream is TCustomMemoryStream) then
  begin
    //If AStream already is a TAdBufferStreamAdapter, increment its "InstanceCount" property
    if AStream is TAdBufferStreamAdapter then
    begin
      with TAdBufferStreamAdapter(AStream) do
        InstanceCount := InstanceCount + 1;
    end else
      AStream := TAdBufferStreamAdapter.Create(AStream); //InstanceCount is set to 1
  end;
end;

procedure FreeBufferedStream(var AStream: TStream);
begin
  if AStream is TAdBufferStreamAdapter then
  begin
    with AStream as TAdBufferStreamAdapter do
    begin
      InstanceCount := InstanceCount - 1;
      if InstanceCount = 0 then
      begin
        //Return the adapted stream
        AStream := AdaptedStream;

        //Free the adapter
        Free;
      end;
    end;
  end;
end;

{ TAdCacheBucket }

constructor TAdBufferBucket.Create;
begin
  inherited;

  //Initialize the buffer bucket
  FIsFree := true;
  FMemory := nil;
  FMemorySize := 0;
  FFilledSize := 0;
  FReadPos := 0;
end;

destructor TAdBufferBucket.Destroy;
begin
  FreeMem;
  inherited;
end;

procedure TAdBufferBucket.FreeMem;
begin
  if FMemory <> nil then
    FreeMemory(FMemory);

  FMemory := nil;
end;

function TAdBufferBucket.ReadData(ABuf: PByte; ASize: Integer): Integer;
var
  ptr: PByte;
begin
  //Go to the read position
  ptr := FMemory;
  inc(ptr, FReadPos);

  //Return the number of read bytes
  result := ASize;
  if result > FFilledSize then
    result := FFilledSize;

  //Read the calculated amout of bytes from the buffer
  Move(ptr^, ABuf^, result);

  FReadPos := FReadPos + result;
  FFilledSize := FFilledSize - result;
  FIsFree := FFilledSize = 0;
end;

function TAdBufferBucket.Seek(AOffset: integer): integer;
begin
  if AOffset + FReadPos > FDataSize then
    AOffset := FDataSize - FReadPos
  else if AOffset + FReadPos < 0 then
    AOffset := -FReadPos;

  //If we seek back, the block gets more filled, if we seek forward, the block
  //gets less filled.
  FFilledSize := FFilledsize - AOffset;
  FReadPos := FReadPos + AOffset;

  FIsFree := FFilledSize = 0;
  result := AOffset;
end;

procedure TAdBufferBucket.WriteData(ABuf: PByte; ASize: Integer);
begin
  if ASize > FMemorySize then
  begin
    FMemory := ReallocMemory(FMemory, ASize);
    FMemorySize := ASize;
  end;

  Move(ABuf^, FMemory^, ASize);

  FFilledSize := ASize;
  FIsFree := false;
  FDataSize := ASize;
  FReadPos := 0;
end;

{ TAdCacheBucketList }

function TAdBufferBucketList.GetItem(AIndex: integer): TAdBufferBucket;
begin
  result := inherited Items[AIndex];
end;

procedure TAdBufferBucketList.SetItem(AIndex: integer; AItem: TAdBufferBucket);
begin
  inherited Items[AIndex] := AItem;
end;

{ TAdCache }

constructor TAdBuffer.Create;
begin
  inherited;

  FBucketList := TAdBufferBucketList.Create;
end;

destructor TAdBuffer.Destroy;
begin
  Clear;
  FBucketList.Free;
  inherited;
end;

procedure TAdBuffer.Clear;
var
  i: integer;
begin
  for i := FBucketList.Count - 1 downto 0 do
    FBucketList[i].Free;

  FBucketList.Clear;
  FCurrentPosition := 0;

  FFilled := 0;
end;                   

function TAdBuffer.Read(Buf: PByte; Size: Integer): integer;
var
  ptr: PByte;
  bckt: TAdBufferBucket;
  readsize: integer;
begin
  result := 0;

  if FBucketList.Count > 0 then
  begin
    ptr := Buf;

    while result < size do
    begin
      bckt := FBucketList[0];
      //If the first bucket is free, there is no more data
      if bckt.IsFree then
      begin
        break;
      end else
      begin
        //Read as much data as possible from the first bucket
        readsize := bckt.ReadData(ptr, Size - result);
        inc(ptr, readsize);
        result := result + readsize;

        //Set the current buffer tag
        FCurrentTag := bckt.Tag;

        //If this bucket is now free, remove it from the beginning of the list
        //and add it to the end
        if bckt.IsFree then
        begin
          FBucketList.Delete(0);
          FBucketList.Add(bckt);
        end;
      end;      
    end;    
  end;

  FFilled := FFilled - result;
end;

function TAdBuffer.Seek(Offset: Integer): Boolean;
var
  i, ind: integer;
  bckt: TAdBufferBucket;
  startpos: int64;
  startofs: integer;
begin
  //Exit if offset = 0
  if Offset = 0 then
  begin
    result := true;
    exit;
  end else
    result := false;

  if FBucketList.Count > 0 then
  begin
    startofs := Offset;

    //Get the position of the first bucket in the list. If we are seeking forward,
    //the position of all succeding buckets must be greater than this value.
    //If we are seeking backward, all prior buckets must have a smaller position.
    startpos := FBucketList[0].Position;

    //Seek forward
    if Offset > 0 then
    begin
      for i := 0 to FBucketList.Count - 1 do
      begin
        //Select the current buffer bucket
        bckt := FBucketList[0];
        //Only continue if the bucket has a higher position value
        if bckt.Position >= startpos then
        begin
          //Tell the bucket to seek forward...
          Offset := Offset - bckt.Seek(Offset);         

          //...if it came to its outer boundaries, the bucket is now free and we
          //have to continue seeking - if it didn't we finished seeking.
          if not bckt.IsFree then
          begin
            result := true;
            FFilled := FFilled - (startofs - Offset);
            break;
          end else
          begin
            //The block was marked as free - add it to the end of the list
            FBucketList.Delete(0);
            FBucketList.Add(bckt);
          end;
        end else //bckt.Position > startpos
          break; //Break seeking here. We were not successful. Result is false
      end;
    end else
    //Offset > 0, seek backward
    begin
      for i := 0 to FBucketList.Count - 1 do
      begin
        //Select the current buffer bucket
        if i = 0 then        
          ind := 0
        else
          ind := FBucketList.Count - 1;

        bckt := FBucketList[ind];
          
        //Only continue if the bucket has a lower position value
        if bckt.Position <= startpos then
        begin
          //Tell the bucket to seek backward...
          Offset := Offset - bckt.Seek(Offset);         

          //...if it came to its outer boundaries, the bucket is now fully used
          //and we have to continue seeking - if it didn't we finished seeking.
          if (bckt.FilledSize <> bckt.DataSize) or (Offset = 0) then
          begin
            result := true;
            FFilled := FFilled - (startofs - Offset);
            break;
          end else
          begin
            //The block was marked as fully filled - add it to top of the list
            FBucketList.Delete(ind);
            FBucketList.Insert(0, bckt);
          end;
        end else //bckt.Position < startpos
          break; //Break seeking here. We were not successful. Result is false
      end;
    end;
  end;
end;

procedure TAdBuffer.Write(Buf: PByte; Size: Integer; Tag: Cardinal);
var
  i: integer;
  lastbuck: integer;
  BufferBucket: TAdBufferBucket;
begin
  lastbuck := FBucketList.Count;

  //Search for the last bucket in the list, that is free
  for i := FBucketList.Count - 1 downto 0 do
  begin
    if not FBucketList[i].IsFree then
      break
    else
      lastbuck := i;
  end;

  //If there is no free bucket in the list, simply create one.
  if lastbuck >= FBucketList.Count then
  begin
    BufferBucket := TAdBufferBucket.Create;
    FBucketList.Add(BufferBucket);
  end else
  begin
    BufferBucket := FBucketList[lastbuck];
  end;

  //Store our data in the bucket
  BufferBucket.Tag := Tag;
  BufferBucket.Position := FCurrentPosition;
  BufferBucket.WriteData(Buf, Size);
  FFilled := FFilled + Size;

  FCurrentPosition := FCurrentPosition + Size;
end;

{ TAdBufferedFileStream }

constructor TAdBufferStreamAdapter.Create(AStream: TStream);
begin
  inherited Create;

  FStream := AStream;

  FPosition := 0;
  FNewSeekPos := -1;
  FBuffer := TAdBuffer.Create;
  GetMem(FMem, AdBufferSize);
  FInstanceCount := 1;
end;

destructor TAdBufferStreamAdapter.Destroy;
begin
  FBuffer.Free;
  FreeMem(FMem);
  
  inherited;
end;

procedure TAdBufferStreamAdapter.DoSeek;
begin
  if FNewSeekPos <> -1 then
  begin
    if FNewSeekPos <> FPosition then
    begin
      if not FBuffer.Seek(FNewSeekPos - FPosition) then
      begin
        FBuffer.Clear;
        FStream.Position := FNewSeekPos;
      end;
      FPosition := FNewSeekPos;
    end;
    FNewSeekPos := -1;
  end;
end;

function TAdBufferStreamAdapter.Read(var Buffer; Count: Integer): Longint;
var
  size: Longint;
begin
  DoSeek;
  
  while (FBuffer.Filled < Count) do
  begin
    size := FStream.Read(FMem^, AdBufferSize);
    if size > 0 then    
      FBuffer.Write(FMem, size)
    else
    begin
      result := 0;
      exit;
    end;
    if size < Count then
      break;
  end;

  result := FBuffer.Read(PByte(@Buffer), Count);
  FPosition := FPosition + Count;
end;

function TAdBufferStreamAdapter.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  newpos: Int64;
begin
  newpos := 0;
  
  //Calculate the new seek position depending on the given origin.
  case Origin of
    soBeginning: newpos := Offset;
    soCurrent:
    begin
      if FNewSeekPos = -1 then
        newpos := FPosition + Offset
      else
        newpos := FNewSeekPos + Offset
    end;
    soEnd: newpos := FStream.Size + Offset; //Offset is < 0
  end;

  //Fit "newpos" to the stream size bounds
  if newpos <= 0 then
    newpos := 0
  else if newpos > FStream.Size then
    newpos := FStream.Size;  

  //Return the new position
  result := newpos;
  FNewSeekPos := newpos;
end;

procedure TAdBufferStreamAdapter.SetSize(const NewSize: Int64);
begin
  //Clear the buffer if
  if FBuffer.Filled > 0 then
    FBuffer.Clear;
    
  //Set the new size
  FStream.Size := NewSize;

  //Set the new position
  FPosition := FStream.Position;
end;

function TAdBufferStreamAdapter.Write(const Buffer; Count: Integer): Longint;
begin
  DoSeek;

  //Buffering for writing is currently disabled. We'll just clear the read buffer,
  //set the FileStream-position to the current position and start writing
  if FBuffer.Filled > 0 then  
    FBuffer.Clear;

  FStream.Position := FPosition;
  result := FStream.Write(Buffer, Count);
  FPosition := FPosition + Count;
end;

{ TAdBufferedFileStream }

constructor TAdBufferedFileStream.Create(const AFileName: string; Mode: Word);
begin
  FFileStream := TFileStream.Create(AFileName, Mode);
  inherited Create(FFileStream);
end;

constructor TAdBufferedFileStream.Create(const AFileName: string; Mode: Word;
  Rights: Cardinal);
begin
  FFileStream := TFileStream.Create(AFileName, Mode, Rights);
  inherited Create(FFileStream);
end;

destructor TAdBufferedFileStream.Destroy;
begin
  FFileStream.Free;
  inherited;
end;

end.
