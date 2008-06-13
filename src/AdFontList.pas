{
* This program is licensed under the Common Public License (CPL) Version 1.0
* You should have recieved a copy of the license with this file.
* If not, see http://www.opensource.org/licenses/cpl1.0.txt for more
* informations.
*
* Inspite of the incompatibility between the Common Public License (CPL) and
* the GNU General Public License (GPL) you're allowed to use this program
* under the GPL.
* You also should have recieved a copy of this license with this file.
* If not, see http://www.gnu.org/licenses/gpl.txt for more informations.
*
* Project: Andorra 2D
* Author:  Andreas Stoeckel
* File: AdFontList.pas
* Comment: Contains a font list class, which stores fonts with a corespondenting name
}

{Contains a font list class, which stores fonts with a corespondenting name}
unit AdFontList;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, AdClasses, AdContainers, AdFont, AdList, AdRAWFontData,
  AdFontFactory;

type
  TAdFontListData = record
    fnt:TAdFont;
    name:string;
    createdbylist:boolean;
  end;
  PAdFontListData = ^TAdFontListData;

  TAdFontList = class(TAdList)
    private
      FMetadata:boolean;
      FAppl:TAd2dApplication;
      FFntFct:TAdFontFactory;
      procedure SetItem(Index:integer; Value:TAdFont);
      procedure SetMetadata(Value:boolean);
      function GetItem(Index:integer):TAdFont;
      function GetItemByName(Index:string):TAdFont;
      function GetName(Index:integer):string;
      procedure SetName(Index:integer;Value:string);
    protected
      procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    public
      constructor Create(AAppl:TAd2dApplication);
      destructor Destroy;override;
      function Add(AName:string;AFont:TAdFont;AOwnedByList:boolean=false):integer;

      procedure SaveToStream(AStream:TStream);
      procedure LoadFromStream(AStream:TStream);
      procedure SaveToFile(AFile:string);
      procedure LoadFromFile(AFile:string);

      function IndexOf(Item:Pointer):integer;

      property Font[Index:string]:TAdFont read GetItemByName;default;
      property Items[Index:integer]:TAdFont read GetItem write SetItem;
      property Names[Index:integer]:string read GetName write SetName;
      property SaveMetadata:boolean read FMetadata write SetMetadata;
      property Application:TAd2dApplication read FAppl;
      property Fonts:TAdFontFactory read FFntFct;
  end;

implementation

{ TAdFontList }

constructor TAdFontList.Create(AAppl:TAd2dApplication);
begin
  inherited Create;
  FMetadata := true;
  FAppl := AAppl;

  FFntFct := TAdFontFactory.Create(FAppl);
  FFntFct.AutoFreeFonts := false;
end;

destructor TAdFontList.Destroy;
begin
  FFntFct.Free;
  inherited;
end;

function TAdFontList.Add(AName: string; AFont: TAdFont; AOwnedByList:boolean=false): integer;
var
  PData:PAdFontListData;
begin
  New(PData);
  PData^.name := AName;
  PData^.fnt := AFont;
  PData^.createdbylist := AOwnedByList;
  result := inherited Add(PData);
end;

function TAdFontList.GetItem(Index: integer): TAdFont;
begin
  result := PAdFontListData(inherited Items[Index])^.fnt;
end;

function TAdFontList.GetItemByName(Index: string): TAdFont;
var
  i:integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if PAdFontListData(inherited Items[i])^.name = Index then
    begin
      result := PAdFontListData(inherited Items[i])^.fnt;
      break;
    end;
  end;
end;

function TAdFontList.GetName(Index: integer): string;
begin
  result := PAdFontListData(inherited Items[Index])^.name;
end;

function TAdFontList.IndexOf(Item: Pointer): integer;
var
  i:integer;
begin
  result := -1;
  for i := 0 to Count - 1 do
    if Items[i] = Item then
    begin
      result := i;
      break;
    end;
end;

procedure TAdFontList.SetName(Index: integer; Value: string);
begin
  PAdFontListData(inherited Items[Index])^.name := Value;
end;

procedure TAdFontList.SetItem(Index: integer; Value: TAdFont);
begin
  inherited Items[Index] := Value;
end;

procedure TAdFontList.SetMetadata(Value: boolean);
begin
  FMetadata := Value;
end;

procedure TAdFontList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
  begin
    if PAdFontListData(ptr)^.createdbylist then
    begin
      TAdFont(PAdFontListData(ptr)^.fnt).Free;
    end;
    SetLength(PAdFontListData(ptr)^.name, 0);
    Dispose(PAdFontListData(ptr));
  end;
end;

procedure TAdFontList.LoadFromFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  ms.Position := 0;
  LoadFromStream(ms);  
  ms.Free;
end;

procedure TAdFontList.LoadFromStream(AStream: TStream);
var
  i, c, l:integer;
  s:string;
  size:Cardinal;
  tmp:TAdFont;
  ms:TMemoryStream;
begin
  Clear;
  
  AStream.Read(c, SizeOf(c));
  for i := 0 to c - 1 do
  begin
    ms := TMemoryStream.Create;

    AStream.Read(l, SizeOf(l));
    SetLength(s, l);
    AStream.Read(s[1], l);

    AStream.Read(size, SizeOf(Size));
    ms.CopyFrom(AStream, size);

    tmp := FFntFct.GenerateFont(ms.Memory, size);
    
    ms.Free;

    Add(s, tmp, true);
  end;
end;

procedure TAdFontList.SaveToFile(AFile: string);
var
  ms:TMemoryStream;
begin
  ms := TMemoryStream.Create;
  SaveToStream(ms);
  ms.SaveToFile(AFile);
  ms.Free;
end;

procedure TAdFontList.SaveToStream(AStream: TStream);
var
  i, c, l:integer;
  PData:PAdFontListData;
begin
  c := Count;
  AStream.Write(c, SizeOf(c));

  for i := 0 to c - 1 do
  begin
    PData := inherited Items[i];
    l := Length(PData^.name);
    AStream.Write(l, SizeOf(l));
    AStream.Write(PData^.name[1], l);
    if (FMetadata) and (Items[i].Creator <> nil) then
    begin
      TAdFontDataKey(Items[i].Creator).SaveToStream(AStream);
    end
    else
    begin
      SaveRAWFontData(Items[i], AStream);
    end;
  end;
end;

end.
