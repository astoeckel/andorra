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
* File: AdPngUtils.pas
* Comment: Contains some helper functions for working with png images
}

{Contains some helper functions for working with png images}
unit AdPNGUtils;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  SysUtils, Classes;

type
  TPngChunk = packed record
    Length : LongWord;
    Name : array[0..3] of Char;
  end;

  EAdExtractPng = class(Exception);

procedure ExtractPng(Src, Dst : TStream);
function IsPng(Src : TStream) : boolean;
function PngConvertEndian(const AVal: LongWord):LongWord;

const
  PngSignature = #137#80#78#71#13#10#26#10;

implementation

function PngConvertEndian(const AVal: LongWord):LongWord;
begin
  result :=
    ((AVal shr 24) and ($000000FF)) or
    ((AVal shr  8) and ($0000FF00)) or
    ((AVal shl  8) and ($00FF0000)) or
    ((AVal shl 24) and ($FF000000));
end;

function IsPng(Src : TStream) : boolean;
var
  sig : string[8];
begin
  SetLength(sig, 8);
  Src.Read(sig[1], 8);
  result := sig = PngSignature;
end;

procedure ExtractPng(Src, Dst : TStream);
var
  Start, Stop : int64;
  Chunk : TPngChunk;
  endchunk : string[4];
begin
  endchunk := 'IEND';

  if IsPng(Src) then
  begin
    FillChar(Chunk, SizeOf(Chunk), 0);
    Start := Src.Position - 8;
    repeat
      Src.Read(Chunk, SizeOf(Chunk));
      Src.Position := Src.Position + PngConvertEndian(Chunk.Length) + 4;
    until CompareMem(@Chunk.Name[0], @endchunk[1], 4);
    Stop := Src.Position;

    Src.Position := Start;
    Dst.CopyFrom(Src, Stop - Start);
  end else
    raise EAdExtractPng.Create('Given stream does not contain a vaild png image.'); 
end;

end.
