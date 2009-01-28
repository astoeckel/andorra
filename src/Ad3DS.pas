unit Ad3DS;

interface

uses
  SysUtils, Classes,
  Ad3DModelLoaderClass, Ad3DSModel;

type
  TAd3DSModelLoader = class(TAd3DModelLoader)
    public
      function LoadFromStream(AStream: TStream): boolean;override;
      function SupportsStream(AStream: TStream): boolean;override;
  end;

implementation

{ TAd3DSModelLoader }

function TAd3DSModelLoader.LoadFromStream(AStream: TStream): boolean;
var
  parser: TAd3DSParser;
begin
  parser := TAd3DSParser.Create(self);
  try
    //Load the stream. The parser will store the loaded data in this model loader
    //instance.
    result := parser.ParseStream(AStream);
  finally
    parser.Free;
  end;
end;

function TAd3DSModelLoader.SupportsStream(AStream: TStream): boolean;
var
  parser: TAd3DSParser;
begin
  parser := TAd3DSParser.Create(self);
  try
    //Search for the magic 3DS number. The functions returns true if it was found
    result := parser.SearchChunk(AStream, CHK_3DS_MAGIC, 100);
  finally
    parser.Free;
  end;
end;

initialization
  Register3DModelLoader(TAd3DSModelLoader);

end.
