{
* EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, THE PROGRAM IS PROVIDED ON
* AN "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS
* OR IMPLIED INCLUDING, WITHOUT LIMITATION, ANY WARRANTIES OR CONDITIONS OF
* TITLE, NON-INFRINGEMENT, MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
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
* File: Ad3DSModel.pas
* Comment: Contains a quite efficient parser for 3DS files.
}

{Contains a quite efficient parser for 3DS files.}
unit Ad3DSModel;

interface

uses
  Classes, SysUtils,
  AdTypes, AdMessages, Ad3DModelLoaderClass, AdBuffer;

type
  {E3DSLoader is raised when something goes wrong during the loading process.}
  E3DSLoader = class(Exception);

  {TAd3DSChunk represents a 3DS file chunk (chunk = section header)}
  TAd3DSChunk = packed record
    {ID contains the chunk ID. See all the CHK_3DS_* constants for supported chunk
     IDs.}
    ID: Word;
    {Length contains the length of the chunk. Length includes the six bytes the
     chunk header needs. Many chunks contain subchunks. Length contains the length
     of all sub chunks.}
    Length: Cardinal;
  end;

  {TAd3DSFace is an element in the index list.}
  TAd3DSFace = packed record
    Ind1,{< First face indice.}
    Ind2,{< Second face indice.}
    Ind3: Word;{< Third face indice.}
    Flags: Word;{< May contain flags for this indice. Not used in the parser.}
  end;

  {TAd3DSParser is the main 3DS parser class. It is able to read 3DS data to a
   model loader class.}
  TAd3DSParser = class
    private
      FTarget: TAd3DModelLoader;
      FTargetList: TAd3DModelSubmeshDataList;
      FTargetData: TAd3DModelSubmeshData;

      function ParseChunk(AStream: TStream; AChk: TAd3DSChunk): Cardinal;
      function ParseChunks(AStream: TStream; ALen: Cardinal): Cardinal;
      function ParseObject(AStream: TStream; ALen: Cardinal): Cardinal;
      function ParsePointArray(AStream: TStream; ALen: Cardinal): Cardinal;
      function ParsePointFlagArray(AStream: TStream; ALen: Cardinal): Cardinal;
      function ReadString(AStream: TStream): string;

      function ValidateChunk(AChunk: TAd3DSChunk;
        ARemainingSize: Cardinal; ARaiseException: boolean = true): boolean;
    public
      {Creates a new instance of the 3DS parser.}
      constructor Create(ATarget: TAd3DModelLoader);
      {Parses a stream.
       @raises(E3DSLoader when an error occurs.)}
      function ParseStream(AStream: TStream): boolean;
      {Searches and seeks to the chunk id indicated by AChunk. AMaxSearch is the
       maximum number of bytes that is read while searching.}
      function SearchChunk(AStream: TStream; AChunk: Cardinal; AMaxSearch: integer = 20): boolean;
  end;

const
  CHK_3DS_MAGIC = $4D4D;
  CHK_3DS_EDIT = $3D3D;
  CHK_3DS_OBJECT = $3000;
  CHK_3DS_NAMED_OBJECT = $4000;
  CHK_3DS_NAMED_TRIANGLE_OBJECT = $4100;
  CHK_3DS_POINT_ARRAY = $4110;
  CHK_3DS_POINT_FLAG_ARRAY = $4120;

implementation

var
  Depth: integer = 0;

{ TAd3DSParser }

constructor TAd3DSParser.Create(ATarget: TAd3DModelLoader);
begin
  inherited Create;

  FTarget := ATarget;
end;

function TAd3DSParser.ParseStream(AStream: TStream): boolean;
var
  chk: TAd3DSChunk;
begin
  //Set the initial result to false
  result := false;

  //Set the initial target
  FTargetList := FTarget.Submeshs;

  //Search for the 3DS magic number chunk and seek to it (search will break after
  //100 bytes read).
  if SearchChunk(AStream, CHK_3DS_MAGIC, 100) then
  begin
    //Read the magic number chunk
    AStream.Read(chk, SizeOf(chk));
    if ValidateChunk(chk, AStream.Size - AStream.Position) then
    begin
      ParseChunks(AStream, chk.Length - SizeOf(TAd3DSChunk));
      result := true;
    end;
  end;
end;

function TAd3DSParser.ReadString(AStream: TStream): string;
var
  c: Char;
begin
  result := '';

  //Search the string terminating #0
  repeat
    if AStream.Read(c, 1) <= 0 then
      c := #0;

    //Add the char that was read to the result string
    if (c <> #0) then
      result := result + c;
  until (c = #0);
end;

function TAd3DSParser.ParseChunk(AStream: TStream;
  AChk: TAd3DSChunk): Cardinal;
var
//  i: Integer;
  seek: boolean;
begin
{  for i := 0 to Depth - 1 do
    Write(' ');
  Writeln('$', IntToHex(AChk.ID, 4), ': ', AChk.Length, 'bytes');}

  //We didn't read anything till now
  result := 0;

  //Seek to the end of the chunk when reading has finished
  seek := true;

  case AChk.ID of
    //Skip the edit chunk - I guess it may be used to indicate undo steps
    CHK_3DS_EDIT:
      seek := false;

    //Read a named object from the file
    CHK_3DS_NAMED_OBJECT:
      result := ParseObject(AStream, AChk.Length - SizeOf(TAd3DSChunk));

    //Read mesh data related chunks from file
    CHK_3DS_NAMED_TRIANGLE_OBJECT:
      result := ParseChunks(AStream, AChk.Length - SizeOf(TAd3DSChunk));

    CHK_3DS_POINT_ARRAY:
      result := ParsePointArray(AStream, AChk.Length - SizeOf(TAd3DSChunk));

    CHK_3DS_POINT_FLAG_ARRAY:
      result := ParsePointFlagArray(AStream, AChk.Length - SizeOf(TAd3DSChunk));
  end;

  if seek then
  begin
    //Finally seek to the end of the chunk to asure data consistency
    if AChk.Length - result - 6 > 0 then
    begin
      AStream.Position := AStream.Position + (AChk.Length - result - 6);
      result := AChk.Length - 6;
    end;
  end;
end;

function TAd3DSParser.ParseChunks(AStream: TStream; ALen: Cardinal): Cardinal;
var
  chk: TAd3DSChunk;
  read: int64;
begin
  Depth := Depth + 1;
  read := 0;
  repeat
    //Try to read chunks from the stream
    read := read + AStream.Read(chk, SizeOf(TAd3DSChunk));
    //Validate the chunk before parsing it
    if ValidateChunk(chk, ALen - read) then
    begin
      //Parse the read chunk
      read := read + ParseChunk(AStream, chk);
    end;
  until read >= ALen;

  result := read;
end;

function TAd3DSParser.ParseObject(AStream: TStream; ALen: Cardinal): Cardinal;
var
  s: string;
  oldlist: TAd3DModelSubmeshDataList;
  olddata: TAd3DModelSubmeshData;
begin
  result := 0;

  //Store the old data container/list
  oldlist := FTargetList;
  olddata := FTargetData;
  
  //Create a new data container
  FTargetData := TAd3DModelSubmeshData.Create;

  //Add this object to the target list
  FTargetList.Add(FTargetData);
  
  //Read the name of the object
  s := ReadString(AStream);
  FTargetData.Name := s;

//  Writeln('Mesh ', s);

  //Set the new target list
  FTargetList := FTargetData.SubItems;

  result := result + Cardinal(Length(s) + 1);

  //Parse the subchunks
  result := result + ParseChunks(AStream, ALen - Cardinal(Length(s) + 1));

  //Restore the old data container/list
  FTargetList := oldlist;
  FTargetData := olddata;
end;

function TAd3DSParser.ParsePointArray(AStream: TStream;
  ALen: Cardinal): Cardinal;
var
  num: Word;
  i, start, r: Integer;
  vert: TAdVertexArray;
  vec: TAdVector3;
begin
  r := 0;

  //Check whether an target object is set
  if (FTargetData <> nil) then
  begin
    //Read the count of vertices that will be read
    r := r + AStream.Read(num, SizeOf(Word));

    //Copy the old vertices
    vert := Copy(FTargetData.Vertices, 0, Length(FTargetData.Vertices));

    //Calculate the array start position
    start := Length(FTargetData.Vertices) - 1;
    if start < 0 then
      start := 0;

    //Rescale the target array
    SetLength(vert, Length(vert) + num);

    //Append the vertices
    for i := start to start + num - 1 do
    begin
      r := r + AStream.Read(vec, SizeOf(vec));
      
      FillChar(vert[i], SizeOf(TAdVertex), 0);
      vert[i].Position := vec;
      vert[i].Position.z := -vec.z;
      PCardinal(@vert[i].Color)^ := $FFFFFFFF;
    end;

    //Store the vertex data in the mesh data object
    FTargetData.Vertices := vert;
  end else
    raise E3DSLoader.Create(Msg3DSInvalidChunkOrder);

  result := r;
end;

function TAd3DSParser.ParsePointFlagArray(AStream: TStream;
  ALen: Cardinal): Cardinal;
var
  num: Word;
  i, start, r: Integer;
  inds: TAdIndexArray;
  face: TAd3DSFace;
begin
  r := 0;

  //Check whether an target object is set
  if (FTargetData <> nil) then
  begin
    //Read the count of indices that will be read
    r := r + AStream.Read(num, SizeOf(Word));

    //Copy the old indices
    inds := Copy(FTargetData.Indices, 0, Length(FTargetData.Indices));

    //Calculate the array start position
    start := Length(FTargetData.Indices) - 1;
    if start < 0 then
      start := 0;

    //Rescale the target array
    SetLength(inds, Length(inds) + num * 3);

    //Append the indices
    for i := 0 to num - 1 do
    begin
      r := r + AStream.Read(face, SizeOf(TAd3DSFace));
      inds[start + i * 3 + 0] := face.Ind1;
      inds[start + i * 3 + 1] := face.Ind2;
      inds[start + i * 3 + 2] := face.Ind3;
    end;

    //Store the index data in the mesh data object
    FTargetData.Indices := inds;
  end else
    raise E3DSLoader.Create(Msg3DSInvalidChunkOrder);

  result := r;
end;

function TAd3DSParser.SearchChunk(AStream: TStream; AChunk: Cardinal; AMaxSearch: integer = 20): boolean;
var
  nb: Word;
  b: Byte;
  s: integer;
begin
  //Read the first four bytes of the stream
  AStream.Read(nb, 2);
  s := 2;

  //Loop until the magic number has been found
  while (nb <> AChunk) do
  begin
    //Read another byte...
    AStream.Read(b, 1);
    //...and append it to the magic number
    nb := (nb shr 8) or (b shl 8);

    //Exit if we are at the end of the stream
    if AStream.Position >= AStream.Size then
      break;

    //Quit the search after AMaxSearch bytes have been read
    s := s + 1;
    if s > AMaxSearch then
      break;
  end;

  result := nb = CHK_3DS_MAGIC;

  //Seek back four bytes
  AStream.Position := AStream.Position - 2;
end;

function TAd3DSParser.ValidateChunk(AChunk: TAd3DSChunk;
  ARemainingSize: Cardinal; ARaiseException: boolean): boolean;
begin
  //Return true if the length of the chunk is smaller or equals the remaining
  //stream size 
  result := (AChunk.Length - SizeOf(TAd3DSChunk)) <= ARemainingSize;
  if (not result) and ARaiseException then
    raise E3DSLoader.Create(Msg3DSInvalidChunkSize);
end;

end.
