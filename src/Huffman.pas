
{*******************************************************}
{                                                       }
{       Huffman Encoder/Decoder Unit                    }
{                                                       }
{       Copyright (C) 2005 Marc Schmitz                 }
{                                                       }
{*******************************************************}

unit Huffman;

interface

uses
  Classes;

type
  THuffmanCode = array of Boolean;
  THuffmanCodes = array[0..255] of THuffmanCode;

  TCountings = array[0..255] of Integer;

  THuffmanHeader = record
    Size   : Int64;
  end;

  PHuffmanNode = ^THuffmanNode;
  THuffmanNode = record
    Count     : Integer;
    N1        : PHuffmanNode;
    N2        : PHuffmanNode;
    Character : Byte;
  end;

  THuffmanTree = class
  private
    FInput     : TStream;
    FRoot      : PHuffmanNode;
    FCodes     : THuffmanCodes;
    FCountings : TCountings;
    FEvaluate  : Boolean;

    procedure Clear; overload;
    procedure Clear(Node: PHuffmanNode); overload;

    procedure InitializeCountings;
    procedure DoEvaluateStream;

    procedure InitializeCodes;
    procedure ToCodes; overload;
    procedure ToCodes(Node: PHuffmanNode; Code: THuffmanCode); overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Analyze;

    property Input: TStream read FInput write FInput;
    property Evaluate: Boolean read FEvaluate write FEvaluate;

    property Root: PHuffmanNode read FRoot;
    property Countings: TCountings read FCountings write FCountings;
    property Codes: THuffmanCodes read FCodes write FCodes;
  end;

  THuffmanEncoder = class
  private
    FTree    : THuffmanTree;
    FInput   : TStream;
    FOutput  : TStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Encode;

    property Tree: THuffmanTree read FTree;
    property Input: TStream read FInput write FInput;
    property Output: TStream read FOutput write FOutput;
  end;

  THuffmanDecoder = class
  private
    FTree    : THuffmanTree;
    FInput   : TStream;
    FOutput  : TStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Decode;

    property Tree: THuffmanTree read FTree;
    property Input: TStream read FInput write FInput;
    property Output: TStream read FOutput write FOutput;
  end;

implementation

const
  MAX_BufferSize   = $FFFF;

type
  TBuffer = array[0..MAX_BufferSize - 1] of Byte;

function HuffmanListSortCompare(Item1, Item2: Pointer): Integer;
begin
  if PHuffmanNode(Item1).Count >
     PHuffmanNode(Item2).Count then
    result := 1
  else
    if PHuffmanNode(Item1).Count <
       PHuffmanNode(Item2).Count then
      result := -1
    else
      result := 0;
end;

{ THuffmanTree }

constructor THuffmanTree.Create;
begin
  FEvaluate := true;
end;

destructor THuffmanTree.Destroy;
begin
  Clear;

  inherited;
end;

procedure THuffmanTree.Analyze;

var
  a      : Integer;

  List   : TList;
  Node   : PHuffmanNode;

begin
  if FEvaluate and Assigned(FInput) then
    DoEvaluateStream;

  Clear;
  List := TList.Create;
  try
    for a := 0 to 255 do
    begin
      if FCountings[a] > 0 then
      begin
        New(Node);
        Node.N1 := nil;
        Node.N2 := nil;
        Node.Count := FCountings[a];
        Node.Character := a;
        List.Add(Node);
      end;
    end;

    while List.Count > 1 do
    begin
      List.Sort(@HuffmanListSortCompare);

      New(Node);
      Node.N1 := List.Items[0];
      Node.N2 := List.Items[1];
      Node.Count := Node.N1.Count +
                    Node.N2.Count;

      List.Delete(0);
      List.Delete(0);
      List.Add(Node);
    end;

    if List.Count = 1 then
    begin
      FRoot := List.Items[0];
      ToCodes;
    end;
  finally
    List.Free;
  end;
end;

procedure THuffmanTree.Clear;
begin
  if Assigned(FRoot) then
    Clear(FRoot);
end;

procedure THuffmanTree.Clear(Node: PHuffmanNode);
begin
  if Assigned(Node.N1) then
    Clear(Node.N1);

  if Assigned(Node.N2) then
    Clear(Node.N2);

  Dispose(Node);
end;

procedure THuffmanTree.InitializeCodes;

var
  a   : Integer;

begin
  for a := 0 to 255 do
    SetLength(FCodes[a], 0);
end;

procedure THuffmanTree.ToCodes;
begin
  InitializeCodes;

  if Assigned(FRoot.N1) then
    ToCodes(FRoot, nil)
  else
  begin
    SetLength(FCodes[FRoot.Character], 1);
    FCodes[FRoot.Character][0] := True;
  end;
end;

procedure THuffmanTree.ToCodes(Node: PHuffmanNode;
  Code: THuffmanCode);

begin
  if Assigned(Node.N1) then
  begin
    SetLength(Code, Length(Code) + 1);

    Code[High(Code)] := false;
    ToCodes(Node.N1, Code);

    Code[High(Code)] := true;
    ToCodes(Node.N2, Code);

    SetLength(Code, Length(Code) - 1);
  end
  else
    FCodes[Node.Character] := Copy(Code);
end;

procedure THuffmanTree.InitializeCountings;
begin
  FillChar(FCountings, 255, 0);
end;

procedure THuffmanTree.DoEvaluateStream;

var
  a      : Integer;

  ret    : Integer;
  Buffer : TBuffer;

begin
  InitializeCountings;

  FInput.Position := 0;
  repeat
    ret := FInput.Read(Buffer, MAX_BufferSize);

    for a := 0 to ret - 1 do
      Inc(FCountings[Buffer[a]]);
  until ret <> MAX_BufferSize;
end;

{ THuffmanEncoder }

constructor THuffmanEncoder.Create;
begin
  FTree := THuffmanTree.Create;
end;

destructor THuffmanEncoder.Destroy;
begin
  FTree.Free;
end;

procedure THuffmanEncoder.Encode;

var
  a           : Integer;
  b           : Integer;
  PtOutput    : Integer;
  PtBit       : Integer;

  HuffmanCode : THuffmanCode;
  ByteCode    : Byte;

  Header      : THuffmanHeader;

  ret         : Integer;
  BufInput    : TBuffer;
  BufOutput   : TBuffer;

begin
  HuffmanCode := nil;

  if Assigned(FInput) and Assigned(FOutput) then
  begin
    FTree.Input := FInput;
    FTree.Analyze;

    FInput.Position := 0;
    FOutput.Position := 0;

    Header.Size := FInput.Size;
    FOutput.Write(Header, SizeOf(Header));

    FOutput.Write(FTree.Countings, SizeOf(FTree.Countings));

    ByteCode := 0;
    PtBit := -1;
    PtOutput := -1;

    repeat
      ret := FInput.Read(BufInput, MAX_BufferSize);
      for a := 0 to ret - 1 do
      begin
        HuffmanCode := FTree.Codes[BufInput[a]];
        for b := Low(HuffmanCode) to High(HuffmanCode) do
        begin
          Inc(PtBit);
          ByteCode := ByteCode or (Byte(HuffmanCode[b]) shl PtBit);
          if PtBit = 7 then
          begin
            Inc(PtOutput);
            BufOutput[PtOutput] := ByteCode;

            if PtOutput = MAX_BufferSize - 1 then
            begin
              FOutput.Write(BufOutput, MAX_BufferSize);
              PtOutput := -1;
            end;

            ByteCode := 0;
            PtBit := -1;
          end;
        end;
      end;
    until ret <> MAX_BufferSize;

    if PtBit >= 0 then
    begin
      Inc(PtOutput);
      BufOutput[PtOutput] := ByteCode;
    end;

    if PtOutput >= 0 then
      FOutput.Write(BufOutput, PtOutput + 1);
  end;
end;

{ THuffmanDecoder }

constructor THuffmanDecoder.Create;
begin
  FTree := THuffmanTree.Create;
  FTree.Evaluate := false;
end;

destructor THuffmanDecoder.Destroy;
begin
  FTree.Free;

  inherited;
end;

procedure THuffmanDecoder.Decode;

var
  a          : Integer;
  b          : Integer;
  PtOutput   : Integer;

  Node       : PHuffmanNode;

  Header     : THuffmanHeader;
  Countings  : TCountings;

  ret        : Integer;
  BufInput   : TBuffer;
  BufOutput  : TBuffer;

begin
  if Assigned(FInput) and Assigned(FOutput) then
  begin
    FInput.Position := 0;
    FOutput.Position := 0;

    FInput.Read(Header, SizeOf(Header));
    FInput.Read(Countings, SizeOf(Countings));

    FTree.Countings := Countings;
    FTree.Analyze;

    Node := FTree.Root;

    PtOutput := -1;
    repeat
      ret := FInput.Read(BufInput, MAX_BufferSize);
      for a := 0 to ret - 1 do
      begin
        for b := 0 to 7 do
        begin
          if Header.Size > 0 then
          begin
            if not Assigned(Node.N1) then
            begin
              Inc(PtOutput);
              BufOutput[PtOutput] := Node.Character;

              if PtOutput = MAX_BufferSize - 1 then
              begin
                FOutput.Write(BufOutput, MAX_BufferSize);
                PtOutput := -1;
              end;

              Node := FTree.Root;

              Dec(Header.Size);
            end;

            if Assigned(FTree.Root.N1) then
              if BufInput[a] and (1 shl b) = (1 shl b) then
                Node := Node.N2
              else
                Node := Node.N1;
          end;
        end;
      end;
    until ret <> MAX_BufferSize;

    if PtOutput >= 0 then
      FOutput.Write(BufOutput, PtOutput + 1);
  end;
end;

end.
