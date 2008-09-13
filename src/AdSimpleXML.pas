{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvSimpleXML.PAS, released on 2002-06-03

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Christophe Paris,
                Florent Ouchet (move from the JVCL to the JCL).

You may retrieve the latest version of this file at the Project JEDI's JCL home page,
located at http://jcl.sourceforge.net

Known Issues: This component does not parse the !DOCTYPE tags but preserves them
-----------------------------------------------------------------------------}
// $Id: AdSimpleXML.pas,v 1.8 2008/09/13 18:54:49 igel457 Exp $

//****IMPORTANT****
//
//This unit was changed by Andreas Stöckel and is used in the Andorra 2D
//Project, so that it compiles with Lazarus and can be used without having
//the JCL in the library path.
//If you have problems with this unit, try to use the original "JclSimpleXML.pas"
//instead.
//Please don't bother the contributors named above when the problem only
//occours with this modified version of the "JclSimpleXMl"-Unit.
//
//Changes:
//  -  Replaced "jcl" with "ad" to fit the Andorra 2D naming conventions
//  -  Removed all Jcl Unit Dependencies
//  -  Removed loading from resource
//  -  Moved some "Ansi"-Constants from "JclBase"/"JclStrings" to "AdSimpleXml"
//  -  Added FPC compiler directives
//  -  Replaced "string" with "AnsiString" to have Delphi2009 compatibility

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{Unit which is used for parsing XML-Files. Originally "JclSimpleXML.pas".}
unit AdSimpleXML;

{$I inc_andorra.inc}

interface

uses
  {$IFDEF Win32}Windows, {$ENDIF}
  SysUtils, Classes, Variants, IniFiles;

type
  {$IFDEF FPC}
  THashedStringList = class(TStringList);
  {$ENDIF}
  TAdSimpleXML = class;

  EAdSimpleXMLError = class(Exception);

  {$M+} // generate RTTI for published properties
  TAdSimpleXMLElem = class;
  {$M-}

  TAdSimpleXMLElems = class;
  TAdSimpleXMLProps = class;
  TAdSimpleXMLElemComment = class;
  TAdSimpleXMLElemClassic = class;
  TAdSimpleXMLElemCData = class;
  TAdSimpleXMLElemDocType = class;
  TAdSimpleXMLElemText = class;
  TAdSimpleXMLElemHeader = class;
  TAdSimpleXMLElemSheet = class;
  TAdOnSimpleXMLParsed = procedure(Sender: TObject; Name: String) of object;
  TAdOnValueParsed = procedure(Sender: TObject; Name, Value: String) of object;
  TAdOnSimpleProgress = procedure(Sender: TObject; const Position, Total: Integer) of object;

  //Those hash stuffs are for future use only
  //Plans are to replace current hash by this mechanism
  TAdHashKind = (hkList, hkDirect);
  {$IFDEF CLR}
  TAdHashElem = class(TObject)
    Next: TAdHashElem;
    Obj: TObject;
  end;
  PAdHashElem = TAdHashElem;
  TAdHashRecord = class;
  TAdHashList = array [0..25] of TAdHashRecord;
  PAdHashList = TAdHashList;
  TAdHashRecord = class(TObject)
  public
    Count: Byte;
    Kind: TAdHashKind;
    List: PAdHashList;
    FirstElem: PAdHashElem;
  end;
  PAdHashRecord = TAdHashRecord;
  {$ELSE}
  PAdHashElem = ^TAdHashElem;
  TAdHashElem = packed record
    Next: PAdHashElem;
    Obj: TObject;
  end;
  PAdHashRecord = ^TAdHashRecord;
  TAdHashList = array [0..25] of PAdHashRecord;
  PAdHashList = ^TAdHashList;
  TAdHashRecord = packed record
    Count: Byte;
    case Kind: TAdHashKind of
      hkList: (List: PAdHashList);
      hkDirect: (FirstElem: PAdHashElem);
  end;
  {$ENDIF CLR}

  TAdSimpleHashTable = class(TObject)
  private
    FList: PAdHashRecord;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObject(const AName: string; AObject: TObject);
    procedure Clear;
  end;

  TAdSimpleXMLProp = class(TObject)
  private
    FName: string;
    FValue: string;
    FParent: TAdSimpleXMLProps;
    FNameSpace: string;
    FData: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF};
    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
  protected
    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);
  public
    function GetSimpleXML: TAdSimpleXML;
    function SaveToString: string;
    function FullName:string;
    property Parent: TAdSimpleXMLProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property NameSpace: string read FNameSpace write FNameSpace;

    property Data: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF} read FData write FData;
  end;

  TAdSimpleXMLProps = class(TObject)
  private
    FProperties: THashedStringList;
    FParent: TAdSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TAdSimpleXMLProp;
    function GetItemNamed(const Name: string): TAdSimpleXMLProp;
  protected
    function GetSimpleXML: TAdSimpleXML;
    function GetItem(const Index: Integer): TAdSimpleXMLProp;
    procedure DoItemRename(var Value: TAdSimpleXMLProp; const Name: string);
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(Parent: TAdSimpleXMLElem);
    destructor Destroy; override;
    function Add(const Name, Value: string): TAdSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Int64): TAdSimpleXMLProp; overload;
    function Add(const Name: string; const Value: Boolean): TAdSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name, Value: string): TAdSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Int64): TAdSimpleXMLProp; overload;
    function Insert(const Index: Integer; const Name: string; const Value: Boolean): TAdSimpleXMLProp; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    procedure LoadFromStream(const Stream: TStream);
    procedure SaveToStream(const Stream: TStream);
    property Item[const Index: Integer]: TAdSimpleXMLProp read GetItem; default;
    property ItemNamed[const Name: string]: TAdSimpleXMLProp read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  TAdSimpleXMLElemsProlog = class(TObject)
  private
    FElems: THashedStringList;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TAdSimpleXMLElem;
    function GetEncoding: string;
    function GetStandAlone: Boolean;
    function GetVersion: string;
    procedure SetEncoding(const Value: string);
    procedure SetStandAlone(const Value: Boolean);
    procedure SetVersion(const Value: string);
  protected
    function FindHeader: TAdSimpleXMLElem;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create;
    destructor Destroy; override;
    function AddComment(const AValue: string): TAdSimpleXMLElemComment;
    function AddDocType(const AValue: string): TAdSimpleXMLElemDocType;
    procedure Clear;
    function AddStyleSheet(AType, AHRef: string): TAdSimpleXMLElemSheet;
    function LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; AParent: TAdSimpleXML = nil);
    property Item[const Index: Integer]: TAdSimpleXMLElem read GetItem; default;
    property Count: Integer read GetCount;
    property Encoding: string read GetEncoding write SetEncoding;
    property StandAlone: Boolean read GetStandAlone write SetStandAlone;
    property Version: string read GetVersion write SetVersion;
  end;

  TAdSimpleXMLElemCompare = function(Elems: TAdSimpleXMLElems; Index1, Index2: Integer): Integer of object;
  TAdSimpleXMLElems = class(TObject)
  private
    FParent: TAdSimpleXMLElem;
    function GetCount: Integer;
    function GetItemNamedDefault(const Name, Default: string): TAdSimpleXMLElem;
    function GetItemNamed(const Name: string): TAdSimpleXMLElem;
  protected
    FElems: THashedStringList;
    FCompare: TAdSimpleXMLElemCompare;
    function GetItem(const Index: Integer): TAdSimpleXMLElem;
    procedure AddChild(const Value: TAdSimpleXMLElem);
    procedure AddChildFirst(const Value: TAdSimpleXMLElem);
    procedure InsertChild(const Value: TAdSimpleXMLElem; Index: Integer);
    procedure DoItemRename(var Value: TAdSimpleXMLElem; const Name: string);
    procedure CreateElems;
  public
    constructor Create(const AOwner: TAdSimpleXMLElem);
    destructor Destroy; override;

    // Use notify to indicate to a list that the given element is removed
    // from the list so that it doesn't delete it as well as the one
    // that insert it in itself. This method is automatically called
    // by AddChild and AddChildFirst if the Container property of the
    // given element is set.
    procedure Notify(Value: TAdSimpleXMLElem; Operation: TOperation);

    function Add(const Name: string): TAdSimpleXMLElemClassic; overload;
    function Add(const Name, Value: string): TAdSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Int64): TAdSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Boolean): TAdSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: TStream): TAdSimpleXMLElemClassic; overload;
    function Add(const Name: string; const Value: Extended): TAdSimpleXMLElemClassic; overload;

    function Add(Value: TAdSimpleXMLElem): TAdSimpleXMLElem; overload;
    function AddFirst(Value: TAdSimpleXMLElem): TAdSimpleXMLElem; overload;
    function AddFirst(const Name: string): TAdSimpleXMLElemClassic; overload;
    function AddComment(const Name: string; const Value: string): TAdSimpleXMLElemComment;
    function AddCData(const Name: string; const Value: string): TAdSimpleXMLElemCData;
    function AddText(const Name: string; const Value: string): TAdSimpleXMLElemText;
    function Insert(Value: TAdSimpleXMLElem; Index: Integer): TAdSimpleXMLElem; overload;
    function Insert(const Name: string; Index: Integer): TAdSimpleXMLElemClassic; overload;
    procedure Clear; virtual;
    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;
    procedure Move(const CurIndex, NewIndex: Integer);
    function IndexOf(const Value: TAdSimpleXMLElem): Integer; overload;
    function IndexOf(const Value: string): Integer; overload;
    function Value(const Name: string; Default: string = ''): string;
    function IntValue(const Name: string; Default: Int64 = -1): Int64;
    function BoolValue(const Name: string; Default: Boolean = True): Boolean;
    function FloatValue(const Name: string; Default: Extended = -1): Extended;
    procedure BinaryValue(const Name: string; const Stream: TStream);
    function LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil): string;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil);
    procedure Sort;
    procedure CustomSort(AFunction: TAdSimpleXMLElemCompare);
    property Parent: TAdSimpleXMLElem read FParent write FParent;
    property Item[const Index: Integer]: TAdSimpleXMLElem read GetItem; default;
    property ItemNamed[const Name: string]: TAdSimpleXMLElem read GetItemNamed;
    property Count: Integer read GetCount;
  end;

  {$M+}
  TAdSimpleXMLElem = class(TObject)
  private
    FName: string;
    FParent: TAdSimpleXMLElem;
    FItems: TAdSimpleXMLElems;
    FProps: TAdSimpleXMLProps;
    FValue: string;
    FNameSpace: string;
    FData: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF};
    FSimpleXML: TAdSimpleXML;
    FContainer: TAdSimpleXMLElems;
    function GetFloatValue: Extended;
    procedure SetFloatValue(const Value: Extended);
  protected
    function GetSimpleXML: TAdSimpleXML;
    function GetIntValue: Int64;
    function GetBoolValue: Boolean;
    function GetChildsCount: Integer;
    function GetProps: TAdSimpleXMLProps;
    procedure SetBoolValue(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetIntValue(const Value: Int64);
    function GetItems: TAdSimpleXMLElems;
    procedure Error(const S: string);
    procedure FmtError(const S: string; const Args: array of const);
  public
    constructor Create(const AOwner: TAdSimpleXMLElem); virtual;
    destructor Destroy; override;
    procedure Assign(Value: TAdSimpleXMLElem); virtual;
    procedure Clear; virtual;
    function SaveToString(AParent: TAdSimpleXML = nil): string;
    procedure LoadFromString(const Value: string; AParent: TAdSimpleXML = nil);
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); virtual; abstract;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); virtual;
      abstract;
    procedure GetBinaryValue(const Stream: TStream);
    property Data: {$IFDEF CLR} TObject {$ELSE} Pointer {$ENDIF} read FData write FData;
    function GetChildIndex(const AChild: TAdSimpleXMLElem): Integer;

    property SimpleXML: TAdSimpleXML read GetSimpleXML;
    property Container: TAdSimpleXMLElems read FContainer write FContainer;
  published
    function FullName: string;virtual;
    property Name: string read FName write SetName;
    property Parent: TAdSimpleXMLElem read FParent write FParent;
    property NameSpace: string read FNameSpace write FNameSpace;
    property ChildsCount: Integer read GetChildsCount;
    property Items: TAdSimpleXMLElems read GetItems;
    property Properties: TAdSimpleXMLProps read GetProps;
    property IntValue: Int64 read GetIntValue write SetIntValue;
    property BoolValue: Boolean read GetBoolValue write SetBoolValue;
    property FloatValue: Extended read GetFloatValue write SetFloatValue;
    property Value: string read FValue write FValue;
  end;
  {$M-}
  TAdSimpleXMLElemClass = class of TAdSimpleXMLElem;

  TAdSimpleXMLElemComment = class(TAdSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
  end;

  TAdSimpleXMLElemClassic = class(TAdSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
  end;

  TAdSimpleXMLElemCData = class(TAdSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
  end;

  TAdSimpleXMLElemText = class(TAdSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
  end;

  TAdSimpleXMLElemHeader = class(TAdSimpleXMLElem)
  private
    FStandalone: Boolean;
    FEncoding: string;
    FVersion: string;
  public
    procedure Assign(Value: TAdSimpleXMLElem); override;
    
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
    property Version: string read FVersion write FVersion;
    property StandAlone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;
    constructor Create(const AOwner: TAdSimpleXMLElem); override;
  end;

  TAdSimpleXMLElemDocType = class(TAdSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
  end;

  TAdSimpleXMLElemSheet = class(TAdSimpleXMLElem)
  public
    procedure LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML = nil); override;
    procedure SaveToStream(const Stream: TStream; const Level: string = ''; AParent: TAdSimpleXML = nil); override;
  end;

  TAdSimpleXMLOptions = set of (sxoAutoCreate, sxoAutoIndent, sxoAutoEncodeValue,
    sxoAutoEncodeEntity, sxoDoNotSaveProlog, sxoTrimPrecedingTextWhitespace);
  TAdSimpleXMLEncodeEvent = procedure(Sender: TObject; var Value: string) of object;
  TAdSimpleXMLEncodeStreamEvent = procedure(Sender: TObject; InStream, OutStream: TStream) of object;

  TAdSimpleXML = class(TObject)
  protected
    FFileName: TFileName;
    FOptions: TAdSimpleXMLOptions;
    FRoot: TAdSimpleXMLElemClassic;
    FOnTagParsed: TAdOnSimpleXMLParsed;
    FOnValue: TAdOnValueParsed;
    FOnLoadProg: TAdOnSimpleProgress;
    FOnSaveProg: TAdOnSimpleProgress;
    FProlog: TAdSimpleXMLElemsProlog;
    FSaveCount: Integer;
    FSaveCurrent: Integer;
    FIndentString: string;
    FOnEncodeValue: TAdSimpleXMLEncodeEvent;
    FOnDecodeValue: TAdSimpleXMLEncodeEvent;
    FOnDecodeStream: TAdSimpleXMLEncodeStreamEvent;
    FOnEncodeStream: TAdSimpleXMLEncodeStreamEvent;
    procedure SetIndentString(const Value: string);
    procedure SetRoot(const Value: TAdSimpleXMLElemClassic);
    procedure SetFileName(Value: TFileName);
    procedure DoLoadProgress(const APosition, ATotal: Integer);
    procedure DoSaveProgress;
    procedure DoTagParsed(const AName: string);
    procedure DoValueParsed(const AName, AValue: string);
    procedure DoEncodeValue(var Value: string); virtual;
    procedure DoDecodeValue(var Value: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromString(const Value: string);
    procedure LoadFromFile(const FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(Stream: TStream);
    function SaveToString: string;
    property Prolog: TAdSimpleXMLElemsProlog read FProlog write FProlog;
    property Root: TAdSimpleXMLElemClassic read FRoot write SetRoot;
    property XMLData: string read SaveToString write LoadFromString;
    property FileName: TFileName read FFileName write SetFileName;
    property IndentString: string read FIndentString write SetIndentString;
    property Options: TAdSimpleXMLOptions read FOptions write FOptions;
    property OnSaveProgress: TAdOnSimpleProgress read FOnSaveProg write FOnSaveProg;
    property OnLoadProgress: TAdOnSimpleProgress read FOnLoadProg write FOnLoadProg;
    property OnTagParsed: TAdOnSimpleXMLParsed read FOnTagParsed write FOnTagParsed;
    property OnValueParsed: TAdOnValueParsed read FOnValue write FOnValue;
    property OnEncodeValue: TAdSimpleXMLEncodeEvent read FOnEncodeValue write FOnEncodeValue;
    property OnDecodeValue: TAdSimpleXMLEncodeEvent read FOnDecodeValue write FOnDecodeValue;
    property OnEncodeStream: TAdSimpleXMLEncodeStreamEvent read FOnEncodeStream write FOnEncodeStream;
    property OnDecodeStream: TAdSimpleXMLEncodeStreamEvent read FOnDecodeStream write FOnDecodeStream;
  end;

{$IFNDEF CLR}
{$IFDEF COMPILER6_UP}

  TXMLVariant = class(TInvokeableVariantType)
  public
    procedure Clear(var V: TVarData); override;
    function IsClear(const V: TVarData): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;

    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

  TXMLVarData = packed record
    vType: TVarType;
    Reserved1: Word;
    Reserved2: Word;
    Reserved3: Word;
    XML: TAdSimpleXMLElem;
    Reserved4: Longint;
  end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TAdSimpleXMLElem);
function XMLCreate(const AXML: TAdSimpleXMLElem): Variant; overload;
function XMLCreate: Variant; overload;
function VarXML: TVarType;

{$ENDIF COMPILER6_UP}
{$ENDIF !CLR}

// Encodes a string into an internal format:
// any character <= #127 is preserved
// all other characters are converted to hex notation except
// for some special characters that are converted to XML entities
function SimpleXMLEncode(const S: string): string;
// Decodes a string encoded with SimpleXMLEncode:
// any character <= #127 is preserved
// all other characters and substrings are converted from
// the special XML entities to characters or from hex to characters
// NB! Setting TrimBlanks to true will slow down the process considerably
procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);

function XMLEncode(const S: string): string;
function XMLDecode(const S: string): string;

// Encodes special characters (', ", <, > and &) into XML entities (@apos;, &quot;, &lt;, &gt; and &amp;)
function EntityEncode(const S: string): string;
// Decodes XML entities (@apos;, &quot;, &lt;, &gt; and &amp;) into special characters (', ", <, > and &)
function EntityDecode(const S: string): string;

resourcestring
  RsEInvalidXMLElementUnexpectedCharacte =
    'Invalid XML Element: Unexpected character in property declaration ("%s" found)';
  RsEInvalidXMLElementUnexpectedCharacte_ =
    'Invalid XML Element: Unexpected character in property declaration. Expecting " or '' but "%s"  found';
  RsEUnexpectedValueForLPos = 'Unexpected value for lPos';
  RsEInvalidXMLElementExpectedBeginningO = 'Invalid XML Element: Expected beginning of tag but "%s" found';
  RsEInvalidXMLElementExpectedEndOfTagBu = 'Invalid XML Element: Expected end of tag but "%s" found';
  RsEInvalidXMLElementMalformedTagFoundn = 'Invalid XML Element: malformed tag found (no valid name)';
  RsEInvalidXMLElementErroneousEndOfTagE =
    'Invalid XML Element: Erroneous end of tag, expecting </%0:s> but </%1:s> found';
  RsEInvalidCommentExpectedsButFounds = 'Invalid Comment: expected "%0:s" but found "%1:s"';
  RsEInvalidCommentNotAllowedInsideComme = 'Invalid Comment: "--" not allowed inside comments';
  RsEInvalidCommentUnexpectedEndOfData = 'Invalid Comment: Unexpected end of data';
  RsEInvalidCDATAExpectedsButFounds = 'Invalid CDATA: expected "%0:s" but found "%1:s"';
  RsEInvalidCDATAUnexpectedEndOfData = 'Invalid CDATA: Unexpected end of data';
  RsEInvalidHeaderExpectedsButFounds = 'Invalid Header: expected "%0:s" but found "%1:s"';
  RsEInvalidStylesheetExpectedsButFounds = 'Invalid Stylesheet: expected "%0:s" but found "%1:s"';
  RsEInvalidStylesheetUnexpectedEndOfDat = 'Invalid Stylesheet: Unexpected end of data';
  RsEInvalidDocumentUnexpectedTextInFile = 'Invalid Document: Unexpected text in file prolog';

implementation

function SecureFloatToStr(AFloat: Extended): string;
var
  i,zp: integer;
begin
  //Set sign
  if AFloat < 0 then
    result := '-'
  else
    result := '';
    
  //Return the integer part of the number
  result := result + IntToStr(Abs(trunc(AFloat))) + '.';

  //Get the decimal places of the number
  result := result + FormatFloat('0000000000', Abs(Round((AFloat - trunc(AFloat)) * 10000000000)));

  //Cut last zeros
  zp := Length(result);

  for i := Length(result) downto 1 do
    if result[i] <> '0' then
    begin
      zp := i;
      break;
    end;

  result := copy(result, 1, zp);
  if result[Length(result)] = '.' then
    result := copy(result, 1, zp - 1);
end;

function SecureStrToFloat(AStr: string): Extended;
var
  predecimalpositions: string;
  decimalplaces: string;
  i: integer;
  decimalposition: integer;
  predec: Double;

  function Power10(exp: integer): Extended;
  var
    i: integer;
  begin
    result := 1;
    for i := 0 to exp - 1 do
      result := result * 10;
  end;

const
  decimalpoints = '.,';

begin
  predecimalpositions := '';
  decimalplaces := '';

  //Find position of the decimal point
  for i := 1 to Length(decimalpoints) do
  begin
    decimalposition := Pos(decimalpoints[i], AStr);
    if decimalposition <> 0 then
      break;
  end;

  if decimalposition <> 0 then
  begin
    //Copy the predecimal digits
    predecimalpositions :=
      Copy(AStr, 1, decimalposition - 1);
      
    //Copy the decimal digits
    decimalplaces :=
      Copy(AStr, decimalposition + 1, Length(AStr) - decimalposition);

    //Calculate the value of the predecimal positions
    predec := StrToInt(predecimalpositions);

    //Calculate the value
    result := Abs(predec);
    if decimalplaces <> '' then
      result := result + StrToInt(decimalplaces) / Power10(length(decimalplaces));

    //Set sign
    if AStr[1] = '-' then
      result := -result;
  end else
  begin
    //If no decimal place was found, it is an integer
    result := StrToInt(AStr);
  end;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Value := 0;
  result := true;
  try
    Value := SecureStrToFloat(S);
  except
    result := false;
  end;
end;

const
  AnsiLineFeed       = AnsiChar(#10);
  AnsiCarriageReturn = AnsiChar(#13);
  AnsiCrLf           = AnsiString(#13#10);
  {$IFDEF WIN32}
  AnsiLineBreak = AnsiCrLf;
  {$ELSE}
  AnsiLineBreak = AnsiLineFeed;
  {$ENDIF}
  AnsiTab            = AnsiChar(#9);

  cBufferSize = 8192;
  DefaultTrueBoolStr = 'True'; // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

var
  GlobalSorts: TList = nil;

  {$IFNDEF CLR}
  {$IFDEF COMPILER6_UP}
  GlobalXMLVariant: TXMLVariant = nil;
  {$ENDIF COMPILER6_UP}
  {$ENDIF !CLR}

  {$IFDEF COMPILER5}
  TrueBoolStrs: array of string;
  FalseBoolStrs: array of string;
  {$ENDIF COMPILER5}

  PreparedNibbleCharMapping: Boolean = False;
  NibbleCharMapping: array [Low(Char)..High(Char)] of Byte;

function GSorts: TList;
begin
  if not Assigned(GlobalSorts) then
    GlobalSorts := TList.Create;
  Result := GlobalSorts;
end;

{$IFNDEF CLR}
{$IFDEF COMPILER6_UP}

function XMLVariant: TXMLVariant;
begin
  if not Assigned(GlobalXMLVariant) then
    GlobalXMLVariant := TXMLVariant.Create;
  Result := GlobalXMLVariant;
end;
{$ENDIF COMPILER6_UP}
{$ENDIF !CLR}

function EntityEncode(const S: string): string;
var
  I, J, K, L: Integer;
  tmp: string;
begin
  SetLength(Result, Length(S) * 6); // worst case
  J := 1;
  I := 1;
  L := Length(S);
  while I <= L do
  begin
    case S[I] of
      '"':
        tmp := '&quot;';
      '&':
        tmp := '&amp;';
      #39:
        tmp := '&apos;';
      '<':
        tmp := '&lt;';
      '>':
        tmp := '&gt;';
    else
      tmp := S[I];
    end;
    for K := 1 to Length(tmp) do
    begin
      Result[J] := tmp[K];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function EntityDecode(const S: string): string;
var
  I, J, L: Integer;
begin
  Result := S;
  I := 1;
  J := 1;
  L := Length(Result);

  while I <= L do
  begin
    if Result[I] = '&' then
    begin
      if AnsiSameText(AnsiString(Copy(Result, I, 5)), AnsiString('&amp;')) then
      begin
        Result[J] := '&';
        Inc(J);
        Inc(I, 4);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 4)), AnsiString('&lt;')) then
      begin
        Result[J] := '<';
        Inc(J);
        Inc(I, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 4)), AnsiString('&gt;')) then
      begin
        Result[J] := '>';
        Inc(J);
        Inc(I, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 6)), AnsiString('&apos;')) then
      begin
        Result[J] := #39;
        Inc(J);
        Inc(I, 5);
      end
      else
      if AnsiSameText(AnsiString(Copy(Result, I, 6)), AnsiString('&quot;')) then
      begin
        Result[J] := '"';
        Inc(J);
        Inc(I, 5);
      end
      else
      begin
        Result[J] := Result[I];
        Inc(J);
      end;
    end
    else
    begin
      Result[J] := Result[I];
      Inc(J);
    end;
    Inc(I);
  end;
  if J > 1 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

function ReadCharsFromStream(Stream: TStream; var Buf: array of Char; BufSize: Integer): Integer;
{$IFDEF CLR}
var
  Bytes: TBytes;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  SetLength(Bytes, BufSize);
  Result := Stream.Read(Bytes, 0, BufSize);
  System.Array.Copy(AnsiEncoding.GetChars(Bytes), 0, Buf, 0, BufSize);
  {$ELSE}
  Result := Stream.Read(Buf, BufSize);
  {$ENDIF CLR}
end;

function WriteStringToStream(Stream: TStream; const Buf: string; BufSize: Integer): Integer;
begin
  {$IFDEF CLR}
  Result := Stream.Write(BytesOf(Buf), BufSize);
  {$ELSE}
  Result := Stream.Write(Buf[1], BufSize);
  {$ENDIF CLR}
end;

{$IFDEF COMPILER5}

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
var
  lResult: Extended;

  function CompareWith(const AStrings: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(AStrings) to High(AStrings) do
      if AnsiSameText(S, AStrings[I]) then
      begin
        Result := True;
        Break;
      end;
  end;

begin
  Result := TryStrToFloat(S, lResult);
  if Result then
    Value := lResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

(*  make Delphi 5 compiler happy // andreas
function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    ConvertErrorFmt(@SInvalidBoolean, [S]);
end;
*)

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [Boolean] of string = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

{$ENDIF COMPILER5}

function SimpleXMLEncode(const S: string): string;
const
  NoConversion = [#0..#127] - ['"', '&', #39, '<', '>'];
var
  I, J, K: Integer;
  tmp: string;
begin
  SetLength(Result, Length(S) * 6); // worst case
  J := 1;
  for I := 1 to Length(S) do
  begin
    if AnsiChar(S[I]) in NoConversion then
      Result[J] := S[I]
    else
    begin
      case S[I] of
        '"':
          tmp := '&quot;';
        '&':
          tmp := '&amp;';
        #39:
          tmp := '&apos;';
        '<':
          tmp := '&lt;';
        '>':
          tmp := '&gt;';
      else
        tmp := Format('&#x%.2x;', [Ord(S[I])]);
      end;
      for K := 1 to Length(tmp) do
      begin
        Result[J] := tmp[K];
        Inc(J);
      end;
      Dec(J);
    end;
    Inc(J);
  end;
  if J > 0 then
    SetLength(Result, J - 1)
  else
    SetLength(Result, 0);
end;

procedure SimpleXMLDecode(var S: string; TrimBlanks: Boolean);
var
  StringLength, ReadIndex, WriteIndex: Cardinal;

  procedure DecodeEntity(var S: string; StringLength: Cardinal;
    var ReadIndex, WriteIndex: Cardinal);
  const
    cHexPrefix: array [Boolean] of string[1] = ('', '$');
  var
    I: Cardinal;
    Value: Integer;
    IsHex: Boolean;
  begin
    Inc(ReadIndex, 2);
    IsHex := (ReadIndex <= StringLength) and (AnsiChar(S[ReadIndex]) in ['x', 'X']);
    Inc(ReadIndex, Ord(IsHex));
    I := ReadIndex;
    while ReadIndex <= StringLength do
    begin
      if S[ReadIndex] = ';' then
      begin
        Value := StrToIntDef(cHexPrefix[IsHex] + Copy(S, I, ReadIndex - I), -1); // no characters are less than 0
        if Value > 0 then
          S[WriteIndex] := Chr(Value)
        else
          ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
        Exit;
      end;
      Inc(ReadIndex);
    end;
    ReadIndex := I - (2 + Cardinal(IsHex)); // reset to start
  end;

  procedure SkipBlanks(var S: string; StringLength: Cardinal; var ReadIndex: Cardinal);
  begin
    while ReadIndex < StringLength do
    begin
      if S[ReadIndex] = AnsiCarriageReturn then
        S[ReadIndex] := AnsiLineFeed
      else
      if S[ReadIndex + 1] = AnsiCarriageReturn then
        S[ReadIndex + 1] := AnsiLineFeed;
      if (S[ReadIndex] < #33) and (S[ReadIndex] = S[ReadIndex + 1]) then
        Inc(ReadIndex)
      else
        Exit;
    end;
  end;

begin
  // NB! This procedure replaces the text inplace to speed up the conversion. This
  // works because when decoding, the string can only become shorter. This is
  // accomplished by keeping track of the current read and write points.
  // In addition, the original string length is read only once and passed to the
  // inner procedures to speed up conversion as much as possible
  ReadIndex := 1;
  WriteIndex := 1;
  StringLength := Length(S);
  while ReadIndex <= StringLength do
  begin
    // this call lowers conversion speed by ~30%, ie 21MB/sec -> 15MB/sec (repeated tests, various inputs)
    if TrimBlanks then
      SkipBlanks(S, StringLength, ReadIndex);
    if S[ReadIndex] = '&' then
    begin
      if S[ReadIndex + 1] = '#' then
      begin
        DecodeEntity(S, StringLength, ReadIndex, WriteIndex);
        Inc(WriteIndex);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 5)), AnsiString('&amp;')) then
      begin
        S[WriteIndex] := '&';
        Inc(WriteIndex);
        Inc(ReadIndex, 4);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 4)), AnsiString('&lt;')) then
      begin
        S[WriteIndex] := '<';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 4)), AnsiString('&gt;')) then
      begin
        S[WriteIndex] := '>';
        Inc(WriteIndex);
        Inc(ReadIndex, 3);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 6)), AnsiString('&apos;')) then
      begin
        S[WriteIndex] := #39;
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      if AnsiSameText(AnsiString(Copy(S, ReadIndex, 6)), AnsiString('&quot;')) then
      begin
        S[WriteIndex] := '"';
        Inc(WriteIndex);
        Inc(ReadIndex, 5);
      end
      else
      begin
        S[WriteIndex] := S[ReadIndex];
        Inc(WriteIndex);
      end;
    end
    else
    begin
      S[WriteIndex] := S[ReadIndex];
      Inc(WriteIndex);
    end;
    Inc(ReadIndex);
  end;
  if WriteIndex > 0 then
    SetLength(S, WriteIndex - 1)
  else
    SetLength(S, 0);
    // this call lowers conversion speed by ~65%, ie 21MB/sec -> 7MB/sec (repeated tests, various inputs)
//  if TrimBlanks then
//    S := AdjustLineBreaks(S);
end;

function XMLEncode(const S: string): string;
begin
  Result := SimpleXMLEncode(S);
end;

function XMLDecode(const S: string): string;
begin
  Result := S;
  SimpleXMLDecode(Result, False);
end;

//=== { TAdSimpleXML } ======================================================

constructor TAdSimpleXML.Create;
begin
  inherited Create;
  FRoot := TAdSimpleXMLElemClassic.Create(nil);
  FRoot.FSimpleXML := Self;
  FProlog := TAdSimpleXMLElemsProlog.Create;
  FOptions := [sxoAutoIndent, sxoAutoEncodeValue, sxoAutoEncodeEntity];
  FIndentString := '  ';
end;

destructor TAdSimpleXML.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

procedure TAdSimpleXML.DoDecodeValue(var Value: string);
begin
  if sxoAutoEncodeValue in Options then
    SimpleXMLDecode(Value, False)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityDecode(Value);
  if Assigned(FOnDecodeValue) then
    FOnDecodeValue(Self, Value);
end;

procedure TAdSimpleXML.DoEncodeValue(var Value: string);
begin
  if Assigned(FOnEncodeValue) then
    FOnEncodeValue(Self, Value);
  if sxoAutoEncodeValue in Options then
    Value := SimpleXMLEncode(Value)
  else
  if sxoAutoEncodeEntity in Options then
    Value := EntityEncode(Value);
end;

procedure TAdSimpleXML.DoLoadProgress(const APosition, ATotal: Integer);
begin
  if Assigned(FOnLoadProg) then
    FOnLoadProg(Self, APosition, ATotal);
end;

procedure TAdSimpleXML.DoSaveProgress;
begin
  if Assigned(FOnSaveProg) then
  begin
    Inc(FSaveCount);
    FOnSaveProg(Self, FSaveCurrent, FSaveCount);
  end;
end;

procedure TAdSimpleXML.DoTagParsed(const AName: string);
begin
  if Assigned(FOnTagParsed) then
    FOnTagParsed(Self, AName);
end;

procedure TAdSimpleXML.DoValueParsed(const AName, AValue: string);
begin
  if Assigned(FOnValue) then
    FOnValue(Self, AName, AValue);
end;

procedure TAdSimpleXML.LoadFromFile(const FileName: TFileName);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAdSimpleXML.LoadFromStream(Stream: TStream);
var
  AOutStream: TStream;
  DoFree: Boolean;
begin
  FRoot.Clear;
  FProlog.Clear;
  AOutStream := nil;
  DoFree := False;
  try
    if Assigned(FOnDecodeStream) then
    begin
      AOutStream := TMemoryStream.Create;
      DoFree := True;
      FOnDecodeStream(Self, Stream, AOutStream);
      AOutStream.Seek(0, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
    end
    else
      AOutStream := Stream;
    if Assigned(FOnLoadProg) then
    begin
      FOnLoadProg(Self, AOutStream.Position, AOutStream.Size);
    // Read doctype and so on
      FProlog.LoadFromStream(AOutStream, Self);
    // Read elements
      FRoot.LoadFromStream(AOutStream, Self);
      FOnLoadProg(Self, AOutStream.Position, AOutStream.Size);
    end
    else
    begin
      FProlog.LoadFromStream(AOutStream, Self);
      FRoot.LoadFromStream(AOutStream, Self);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

procedure TAdSimpleXML.LoadFromString(const Value: string);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAdSimpleXML.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  if SysUtils.FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenWrite);
    Stream.Size := 0;
  end
  else
    Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAdSimpleXML.SaveToStream(Stream: TStream);
var
  lCount: Integer;
  AOutStream: TStream;
  DoFree: Boolean;
begin
  if Assigned(FOnEncodeStream) then
  begin
    AOutStream := TMemoryStream.Create;
    DoFree := True;
  end
  else
  begin
    AOutStream := Stream;
    DoFree := False;
  end;
  try
    if Assigned(FOnSaveProg) then
    begin
      lCount := Root.ChildsCount + Prolog.Count;
      FSaveCount := lCount;
      FSaveCurrent := 0;
      FOnSaveProg(Self, 0, lCount);
      if not (sxoDoNotSaveProlog in FOptions) then
        Prolog.SaveToStream(AOutStream, Self);
      Root.SaveToStream(AOutStream, '', Self);
      FOnSaveProg(Self, lCount, lCount);
    end
    else
    begin
      if not (sxoDoNotSaveProlog in FOptions) then
        Prolog.SaveToStream(AOutStream, Self);
      Root.SaveToStream(AOutStream, '', Self);
    end;
    if Assigned(FOnEncodeStream) then
    begin
      AOutStream.Seek(0, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
      FOnEncodeStream(Self, AOutStream, Stream);
    end;
  finally
    if DoFree then
      AOutStream.Free;
  end;
end;

function TAdSimpleXML.SaveToString: string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TAdSimpleXML.SetFileName(Value: TFileName);
begin
  FFileName := Value;
  LoadFromFile(Value);
end;

//=== { TAdSimpleXMLElem } ==================================================

procedure TAdSimpleXMLElem.Assign(Value: TAdSimpleXMLElem);
var
  Elems: TAdSimpleXMLElem;
  Elem: TAdSimpleXMLElem;
  I: Integer;
begin
  Clear;
  if Value = nil then
    Exit;
  Elems := TAdSimpleXMLElem(Value);
  Name := Elems.Name;
  Self.Value := Elems.Value;
  for I := 0 to Elems.Properties.Count - 1 do
    Properties.Add(Elems.Properties[I].Name, Elems.Properties[I].Value);

  for I := 0 to Elems.Items.Count - 1 do
  begin
    // Create from the class type, so that the virtual constructor is called
    // creating an element of the correct class type.
    Elem := TAdSimpleXMLElemClass(Elems.Items[I].ClassType).Create(Elems.Items[I].Parent);
    Elem.Assign(Elems.Items[I]);
    Items.Add(Elem);
  end;
end;

procedure TAdSimpleXMLElem.Clear;
begin
  if FItems <> nil then
    FItems.Clear;
  if FProps <> nil then
    FProps.Clear;
end;

constructor TAdSimpleXMLElem.Create(const AOwner: TAdSimpleXMLElem);
begin
  inherited Create;
  FName := '';
  FParent := TAdSimpleXMLElem(AOwner);
  if Assigned(FParent) then
    FSimpleXML := FParent.FSimpleXML;
  FContainer := nil;
end;

destructor TAdSimpleXMLElem.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FProps);
  inherited Destroy;
end;

procedure TAdSimpleXMLElem.Error(const S: string);
begin
  raise EAdSimpleXMLError.Create(S);
end;

procedure TAdSimpleXMLElem.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TAdSimpleXMLElem.FullName: string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + ':' + Name
  else
    Result := Name;
end;

procedure TAdSimpleXMLElem.GetBinaryValue(const Stream: TStream);
var
  I, J, ValueLength, RequiredStreamSize: Integer;
  Buf: array [0..cBufferSize - 1] of Byte;
  N1, N2: Byte;

  function NibbleCharToNibble(const AChar: Char): Byte;
  begin
    case AChar of
      '0': Result := 0;
      '1': Result := 1;
      '2': Result := 2;
      '3': Result := 3;
      '4': Result := 4;
      '5': Result := 5;
      '6': Result := 6;
      '7': Result := 7;
      '8': Result := 8;
      '9': Result := 9;
      'a', 'A': Result := 10;
      'b', 'B': Result := 11;
      'c', 'C': Result := 12;
      'd', 'D': Result := 13;
      'e', 'E': Result := 14;
      'f', 'F': Result := 15;
      else
        Result := 16;
    end;
  end;

  procedure PrepareNibbleCharMapping;
  var
    C: Char;
  begin
    if not PreparedNibbleCharMapping then
    begin
      for C := Low(Char) to High(Char) do
        NibbleCharMapping[C] := NibbleCharToNibble(C);
      PreparedNibbleCharMapping := True;
    end;
  end;

begin
  PrepareNibbleCharMapping;
  I := 1;
  J := 0;
  ValueLength := Length(Value);
  RequiredStreamSize := Stream.Position + ValueLength div 2;
  if Stream.Size < RequiredStreamSize then
    Stream.Size := RequiredStreamSize;
  while I < ValueLength do
  begin
    if J = cBufferSize - 1 then //Buffered write to speed up the process a little
    begin
      Stream.Write(Buf, J);
      J := 0;
    end;
    //faster replacement for St := '$' + Value[I] + Value[I + 1]; Buf[J] := StrToIntDef(St, 0);
    N1 := NibbleCharMapping[Value[I]];
    N2 := NibbleCharMapping[Value[I + 1]];
    if (N1 > 15) or (N2 > 15) then
      Buf[J] := 0
    else
      Buf[J] := N1 shl 4 + N2;
    Inc(J);
    Inc(I, 2);
  end;
  Stream.Write(Buf, J);
end;

function TAdSimpleXMLElem.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TAdSimpleXMLElem.GetChildIndex(
  const AChild: TAdSimpleXMLElem): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.FElems.IndexOfObject(AChild);
end;

function TAdSimpleXMLElem.GetChildsCount: Integer;
var
  I: Integer;
begin
  Result := 1;
  if FItems <> nil then
    for I := 0 to FItems.Count - 1 do
      Result := Result + FItems[I].ChildsCount;
end;

function TAdSimpleXMLElem.GetFloatValue: Extended;
begin
  result := SecureStrToFloat(Value);
end;

function TAdSimpleXMLElem.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TAdSimpleXMLElem.GetItems: TAdSimpleXMLElems;
begin
  if FItems = nil then
    FItems := TAdSimpleXMLElems.Create(Self);
  Result := FItems;
end;

function TAdSimpleXMLElem.GetProps: TAdSimpleXMLProps;
begin
  if FProps = nil then
    FProps := TAdSimpleXMLProps.Create(Self);
  Result := FProps;
end;

function TAdSimpleXMLElem.GetSimpleXML: TAdSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := FSimpleXML;
end;

procedure TAdSimpleXMLElem.LoadFromString(const Value: string; AParent : TAdSimpleXML);
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create(Value);
  try
    LoadFromStream(Stream, AParent);
  finally
    Stream.Free;
  end;
end;

function TAdSimpleXMLElem.SaveToString(AParent : TAdSimpleXML): string;
var
  Stream: TStringStream;
begin
  Stream := TStringStream.Create('');
  try
    SaveToStream(Stream, '', AParent);
    Result := Stream.DataString;
  finally
    Stream.Free;
  end;
end;

procedure TAdSimpleXMLElem.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TAdSimpleXMLElem.SetFloatValue(const Value: Extended);
begin
  FValue := SecureFloatToStr(Value);
end;

procedure TAdSimpleXMLElem.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TAdSimpleXMLElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TAdSimpleXMLElems } =================================================

function TAdSimpleXMLElems.Add(const Name: string): TAdSimpleXMLElemClassic;
begin
  Result := TAdSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChild(Result);
end;

function TAdSimpleXMLElems.Add(const Name, Value: string): TAdSimpleXMLElemClassic;
begin
  Result := TAdSimpleXMLElemClassic.Create(Parent);
  Result.Name := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TAdSimpleXMLElems.Add(const Name: string; const Value: Int64): TAdSimpleXMLElemClassic;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TAdSimpleXMLElems.Add(Value: TAdSimpleXMLElem): TAdSimpleXMLElem;
begin
  if Value <> nil then
    AddChild(Value);
  Result := Value;
end;

function TAdSimpleXMLElems.Add(const Name: string;
  const Value: Extended): TAdSimpleXMLElemClassic;
begin
  Result := Add(Name, SecureFloatToStr(Value));
end;

function TAdSimpleXMLElems.Add(const Name: string;
  const Value: Boolean): TAdSimpleXMLElemClassic;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TAdSimpleXMLElems.Add(const Name: string;
  const Value: TStream): TAdSimpleXMLElemClassic;
var
  Stream: TStringStream;
  Buf: array [0..cBufferSize - 1] of Byte;
  St: string;
  I, Count: Integer;
begin
  Stream := TStringStream.Create('');
  repeat
    Count := Value.Read(Buf, Length(Buf));
    St := '';
    for I := 0 to Count - 1 do
      St := St + IntToHex(Buf[I], 2);
    Stream.WriteString(St);
  until Count = 0;
  Result := Add(Name, Stream.DataString);
  Stream.Free;
end;

procedure TAdSimpleXMLElems.AddChild(const Value: TAdSimpleXMLElem);
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.AddObject(Value.Name, Value);

  Notify(Value, opInsert);
end;

procedure TAdSimpleXMLElems.AddChildFirst(const Value: TAdSimpleXMLElem);
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.InsertObject(0, Value.Name, Value);

  Notify(Value, opInsert);
end;

function TAdSimpleXMLElems.AddFirst(const Name: string): TAdSimpleXMLElemClassic;
begin
  Result := TAdSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  AddChildFirst(Result);
end;

function TAdSimpleXMLElems.AddFirst(Value: TAdSimpleXMLElem): TAdSimpleXMLElem;
begin
  if Value <> nil then
    AddChildFirst(Value);
  Result := Value;
end;

function TAdSimpleXMLElems.AddComment(const Name,
  Value: string): TAdSimpleXMLElemComment;
begin
  Result := TAdSimpleXMLElemComment.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TAdSimpleXMLElems.AddCData(const Name, Value: string): TAdSimpleXMLElemCData;
begin
  Result := TAdSimpleXMLElemCData.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

function TAdSimpleXMLElems.AddText(const Name, Value: string): TAdSimpleXMLElemText;
begin
  Result := TAdSimpleXMLElemText.Create(Parent);
  Result.FName := Name;
  Result.Value := Value;
  AddChild(Result);
end;

procedure TAdSimpleXMLElems.BinaryValue(const Name: string;
  const Stream: TStream);
var
  Elem: TAdSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem <> nil then
    Elem.GetBinaryValue(Stream);
end;

function TAdSimpleXMLElems.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Elem: TAdSimpleXMLElem;
begin
  try
    Elem := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Elem = nil) or (Elem.Value = '') then
      Result := Default
    else
      Result := Elem.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TAdSimpleXMLElems.Clear;
var
  I: Integer;
begin
  if FElems <> nil then
  begin
    for I := 0 to FElems.Count - 1 do
    begin
      // TAdSimpleXMLElem(FElems.Objects[I]).Clear; // (p3) not needed -called in Destroy
      FElems.Objects[I].Free;
      FElems.Objects[I] := nil;
    end;
    FElems.Clear;
  end;
end;

constructor TAdSimpleXMLElems.Create(const AOwner: TAdSimpleXMLElem);
begin
  inherited Create;
  FParent := AOwner;
end;

procedure TAdSimpleXMLElems.Delete(const Index: Integer);
begin
  if (FElems <> nil) and (Index >= 0) and (Index < FElems.Count) then
  begin
    TObject(FElems.Objects[Index]).Free;
    FElems.Delete(Index);
  end;
end;

procedure TAdSimpleXMLElems.CreateElems;
begin
  if FElems = nil then
    FElems := THashedStringList.Create;
end;

procedure TAdSimpleXMLElems.Delete(const Name: string);
begin
  if FElems <> nil then
    Delete(FElems.IndexOf(Name));
end;

destructor TAdSimpleXMLElems.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TAdSimpleXMLElems.DoItemRename(var Value: TAdSimpleXMLElem;
  const Name: string);
var
  I: Integer;
begin
  I := FElems.IndexOfObject(Value);
  if I <> -1 then
    FElems[I] := Name;
end;

function TAdSimpleXMLElems.GetCount: Integer;
begin
  if FElems = nil then
    Result := 0
  else
    Result := FElems.Count;
end;

function TAdSimpleXMLElems.GetItem(const Index: Integer): TAdSimpleXMLElem;
begin
  if (FElems = nil) or (Index > FElems.Count) then
    Result := nil
  else
    Result := TAdSimpleXMLElem(FElems.Objects[Index]);
end;

function TAdSimpleXMLElems.GetItemNamedDefault(const Name, Default: string): TAdSimpleXMLElem;
var
  I: Integer;
begin
  Result := nil;
  if FElems <> nil then
  begin
    I := FElems.IndexOf(Name);
    if I <> -1 then
      Result := TAdSimpleXMLElem(FElems.Objects[I])
    else
    if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(Parent) and Assigned(Parent.SimpleXML) and (sxoAutoCreate in Parent.SimpleXML.Options) then
    Result := Add(Name, Default);
end;

function TAdSimpleXMLElems.GetItemNamed(const Name: string): TAdSimpleXMLElem;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TAdSimpleXMLElems.IntValue(const Name: string; Default: Int64): Int64;
var
  Elem: TAdSimpleXMLElem;
begin
  Elem := GetItemNamedDefault(Name, IntToStr(Default));
  if Elem = nil then
    Result := Default
  else
    Result := Elem.IntValue;
end;

function TAdSimpleXMLElems.FloatValue(const Name: string;
  Default: Extended): Extended;
var
  Elem: TAdSimpleXMLElem;
begin
  Elem := GetItemNamed(Name);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.FloatValue;
end;

function TAdSimpleXMLElems.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML): string;
type
  TReadStatus = (rsWaitingTag, rsReadingTagKind, rsProcessingEndTag); 
var
  I, lStreamPos, Count: Integer;
  lPos: TReadStatus;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  Po: string;
  lElem: TAdSimpleXMLElem;
  Ch: Char;
  lTrimWhiteSpace, lContainsWhiteSpace: Boolean;
  lStartOfContentPos, lTempStreamPos: Integer;
begin
  lStreamPos := Stream.Position;
  Result := '';
  Po := '';
  St := '';
  lPos := rsWaitingTag;

  // Preserve old preceeding whitespace trimming behaviour
  lTrimWhiteSpace := Assigned(AParent) and (sxoTrimPrecedingTextWhitespace in AParent.Options);

  // We read from a stream, thus replacing the existing items
  Clear;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    lContainsWhiteSpace := False;
    lStartOfContentPos := lStreamPos;
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);
      Ch := lBuf[I];

      case lPos of
        rsWaitingTag: //We are waiting for a tag and thus avoiding spaces
          begin
            case Ch of
              ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
                begin
                  lContainsWhiteSpace := True;
                end;
              '<':
                begin
                  lPos := rsReadingTagKind;
                  St := Ch;
                end;
            else
              begin
                  //This is a text
                lElem := TAdSimpleXMLElemText.Create(Parent);
                if lTrimWhiteSpace then
                  Stream.Seek(lStreamPos - 1, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP})
                else
                  Stream.Seek(lStartOfContentPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                lElem.LoadFromStream(Stream, AParent);
                lStreamPos := Stream.Position;
                CreateElems;
                FElems.AddObject(lElem.Name, lElem);
                Break;
              end;
            end;
          end;

        rsReadingTagKind: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            case Ch of
              '/':
                if St = '<' then
                begin
                  lPos := rsProcessingEndTag;
                  St := '';
                end
                else
                begin
                  lElem := TAdSimpleXMLElemClassic.Create(Parent);
                  St := St + Ch;
                end;

              ' ', '>', ':': //This should be a classic tag
                begin
                  lElem := TAdSimpleXMLElemClassic.Create(Parent);
                  St := St + Ch;
                end;
            else
              begin
                if (St <> '<![CDATA') or not (AnsiChar(Ch) in [' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed]) then
                  St := St + Ch;
                if St = '<![CDATA[' then
                  lElem := TAdSimpleXMLElemCData.Create(Parent)
                else
                if St = '<!--' then
                  lElem := TAdSimpleXMLElemComment.Create(Parent);
                  //<?
              end;
            end;

            if lElem <> nil then
            begin
              CreateElems;
              Stream.Seek(lStreamPos - (Length(St)), {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
              lElem.LoadFromStream(Stream, AParent);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              Notify(lElem, opInsert);
              St := '';
              lPos := rsWaitingTag;
              Break;
            end;
          end;

        rsProcessingEndTag: //This is an end tag
          case Ch of
            '>':
              begin
                if Po <> '' then
                  Result := Po + ':' + St
                else
                  Result := St;
                Count := 0;

                // We have reached an end tag. If whitespace was found while
                // waiting for the end tag, and the user told us to keep it
                // then we have to create a text element.
                // But it must only be created if there are not other elements
                // in the list. If we did not check this, we would create a
                // text element for whitespace found between two adjacent end
                // tags.  
                if lContainsWhiteSpace and not lTrimWhiteSpace  and
                   (not Assigned(FElems) or (FElems.Count=0))then
                begin
                  lTempStreamPos := Stream.Position;
                  lElem := TAdSimpleXMLElemText.Create(Parent);
                  Stream.Seek(lStartOfContentPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                  lElem.LoadFromStream(Stream, AParent);
                  CreateElems;
                  FElems.AddObject(lElem.Name, lElem);
                  Stream.Seek(lTempStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                end;

                Break;
              end;
            ':':
              begin
                Po := St;
                St := '';
              end;
          else
            St := St + Ch;
          end;
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElems.Notify(Value: TAdSimpleXMLElem;
  Operation: TOperation);
begin
  case Operation of
    opRemove:
      if Value.Container = Self then  // Only remove if we have it
        FElems.Delete(FElems.IndexOfObject(Value));
    opInsert:
      Value.Container := Self;
  end;
end;

procedure TAdSimpleXMLElems.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TAdSimpleXML);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, Level, AParent);
end;

function TAdSimpleXMLElems.Value(const Name: string; Default: string): string;
var
  Elem: TAdSimpleXMLElem;
begin
  Result := '';
  Elem := GetItemNamedDefault(Name, Default);
  if Elem = nil then
    Result := Default
  else
    Result := Elem.Value;
end;

procedure TAdSimpleXMLElems.Move(const CurIndex, NewIndex: Integer);
begin
  if FElems <> nil then
    FElems.Move(CurIndex, NewIndex);
end;

function TAdSimpleXMLElems.IndexOf(const Value: TAdSimpleXMLElem): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOfObject(Value);
end;

function TAdSimpleXMLElems.IndexOf(const Value: string): Integer;
begin
  if FElems = nil then
    Result := -1
  else
    Result := FElems.IndexOf(Value);
end;

procedure TAdSimpleXMLElems.InsertChild(const Value: TAdSimpleXMLElem; Index: Integer);
begin
  CreateElems;

  // If there already is a container, notify it to remove the element
  if Assigned(Value.Container) then
  begin
    Value.Container.Notify(Value, opRemove);
    Value.Parent := Parent;
  end;

  FElems.InsertObject(Index, Value.Name, Value);

  Notify(Value, opInsert);
end;

function TAdSimpleXMLElems.Insert(Value: TAdSimpleXMLElem;
  Index: Integer): TAdSimpleXMLElem;
begin
  if Value <> nil then
    InsertChild(Value, Index);
  Result := Value;
end;

function TAdSimpleXMLElems.Insert(const Name: string;
  Index: Integer): TAdSimpleXMLElemClassic;
begin
  Result := TAdSimpleXMLElemClassic.Create(Parent);
  Result.FName := Name; //Directly set parent to avoid notification
  InsertChild(Result, Index);
end;

function SortItems(List: TStringList; Index1, Index2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GSorts.Count - 1 do
    if TAdSimpleXMLElems(GSorts[I]).FElems = List then
    begin
      Result := TAdSimpleXMLElems(GSorts[I]).FCompare(TAdSimpleXMLElems(GSorts[I]), Index1, Index2);
      Break;
    end;
end;

procedure TAdSimpleXMLElems.CustomSort(AFunction: TAdSimpleXMLElemCompare);
begin
  if FElems <> nil then
  begin
    GSorts.Add(Self);
    FCompare := AFunction;
    FElems.CustomSort(SortItems);
    GSorts.Remove(Self);
  end;
end;

procedure TAdSimpleXMLElems.Sort;
begin
  if FElems <> nil then
    FElems.Sort;
end;

//=== { TAdSimpleXMLProps } =================================================

function TAdSimpleXMLProps.Add(const Name, Value: string): TAdSimpleXMLProp;
var
  Elem: TAdSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TAdSimpleXMLProp.Create();
  FProperties.AddObject(Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TAdSimpleXMLProps.Add(const Name: string; const Value: Int64): TAdSimpleXMLProp;
begin
  Result := Add(Name, IntToStr(Value));
end;

function TAdSimpleXMLProps.Add(const Name: string; const Value: Boolean): TAdSimpleXMLProp;
begin
  Result := Add(Name, BoolToStr(Value));
end;

function TAdSimpleXMLProps.Insert(const Index: Integer; const Name, Value: string): TAdSimpleXMLProp;
var
  Elem: TAdSimpleXMLProp;
begin
  if FProperties = nil then
    FProperties := THashedStringList.Create;
  Elem := TAdSimpleXMLProp.Create();
  FProperties.InsertObject(Index, Name, Elem);
  Elem.FName := Name; //Avoid notification
  Elem.Value := Value;
  Elem.Parent := Self;
  Result := Elem;
end;

function TAdSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Int64): TAdSimpleXMLProp;
begin
  Result := Insert(Index, Name, IntToStr(Value));
end;

function TAdSimpleXMLProps.Insert(const Index: Integer; const Name: string; const Value: Boolean): TAdSimpleXMLProp;
begin
  Result := Insert(Index, Name, BoolToStr(Value));
end;

function TAdSimpleXMLProps.BoolValue(const Name: string; Default: Boolean): Boolean;
var
  Prop: TAdSimpleXMLProp;
begin
  try
    Prop := GetItemNamedDefault(Name, BoolToStr(Default));
    if (Prop = nil) or (Prop.Value = '') then
      Result := Default
    else
      Result := Prop.BoolValue;
  except
    Result := Default;
  end;
end;

procedure TAdSimpleXMLProps.Clear;
var
  I: Integer;
begin
  if FProperties <> nil then
  begin
    for I := 0 to FProperties.Count - 1 do
    begin
      TAdSimpleXMLProp(FProperties.Objects[I]).Free;
      FProperties.Objects[I] := nil;
    end;
    FProperties.Clear;
  end;
end;

procedure TAdSimpleXMLProps.Delete(const Index: Integer);
begin
  if (FProperties <> nil) and (Index >= 0) and (Index < FProperties.Count) then
  begin
    TObject(FProperties.Objects[Index]).Free;
    FProperties.Delete(Index);
  end;
end;

constructor TAdSimpleXMLProps.Create(Parent: TAdSimpleXMLElem);
begin
  inherited Create;
  FParent := Parent;
end;

procedure TAdSimpleXMLProps.Delete(const Name: string);
begin
  if FProperties <> nil then
    Delete(FProperties.IndexOf(Name));
end;

destructor TAdSimpleXMLProps.Destroy;
begin
  FParent := nil;
  Clear;
  FreeAndNil(FProperties);
  inherited Destroy;
end;

procedure TAdSimpleXMLProps.DoItemRename(var Value: TAdSimpleXMLProp;
  const Name: string);
var
  I: Integer;
begin
  if FProperties = nil then
    Exit;
  I := FProperties.IndexOfObject(Value);
  if I <> -1 then
    FProperties[I] := Name;
end;

procedure TAdSimpleXMLProps.Error(const S: string);
begin
  raise EAdSimpleXMLError.Create(S);
end;

procedure TAdSimpleXMLProps.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

function TAdSimpleXMLProps.GetCount: Integer;
begin
  if FProperties = nil then
    Result := 0
  else
    Result := FProperties.Count;
end;

function TAdSimpleXMLProps.GetItem(const Index: Integer): TAdSimpleXMLProp;
begin
  if FProperties <> nil then
    Result := TAdSimpleXMLProp(FProperties.Objects[Index])
  else
    Result := nil;
end;

function TAdSimpleXMLProps.GetItemNamedDefault(const Name, Default: string): TAdSimpleXMLProp;
var
  I, Index: Integer;
begin
  Result := nil;
  if FProperties <> nil then
  begin
    //I := FProperties.IndexOf(Name);
    Index := -1;
    for I := 0 to FProperties.Count - 1 do
      if FProperties[i] = Name then
      begin
        Index := I;
        break;
      end;
    if Index <> -1 then
      Result := TAdSimpleXMLProp(FProperties.Objects[Index])
    else
    if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
      Result := Add(Name, Default);
  end
  else
  if Assigned(FParent) and Assigned(FParent.SimpleXML) and (sxoAutoCreate in FParent.SimpleXML.Options) then
  begin
    Result := Add(Name, Default);
  end;
end;

function TAdSimpleXMLProps.GetItemNamed(const Name: string): TAdSimpleXMLProp;
begin
  Result := GetItemNamedDefault(Name, '');
end;

function TAdSimpleXMLProps.GetSimpleXML: TAdSimpleXML;
begin
  if FParent <> nil then
    Result := FParent.GetSimpleXML
  else
    Result := nil;
end;

function TAdSimpleXMLProps.IntValue(const Name: string; Default: Int64): Int64;
var
  Prop: TAdSimpleXMLProp;
begin
  Prop := GetItemNamedDefault(Name, IntToStr(Default));
  if Prop = nil then
    Result := Default
  else
    Result := Prop.IntValue;
end;

procedure TAdSimpleXMLProps.LoadFromStream(const Stream: TStream);
//<element Prop="foo" Prop='bar' foo:bar="beuh"/>
//Stop on / or ? or >
type
  TPosType = (
    ptWaiting,
    ptReadingName,
    ptStartingContent,
    ptReadingValue,
    ptSpaceBeforeEqual
    );
var
  lPos: TPosType;
  I, lStreamPos, Count: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lName, lValue, lNameSpace: string;
  lPropStart: Char;
  Ch: Char;
begin
  lStreamPos := Stream.Position;
  lValue := '';
  lNameSpace := '';
  lName := '';
  lPropStart := ' ';
  lPos := ptWaiting;

  // We read from a stream, thus replacing the existing properties
  Clear;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);
      Ch := lBuf[I];

      case lPos of
        ptWaiting: //We are waiting for a property
          begin
            case Ch of
              ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
                begin
                end;
              'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
                begin
                  lName := Ch;
                  lNameSpace := '';
                  lPos := ptReadingName;
                end;
              '/', '>', '?':
                begin
                  Dec(lStreamPos);
                  Count := 0;
                  Break;
                end;
            else
              FmtError(RsEInvalidXMLElementUnexpectedCharacte, [Ch]);
            end;
          end;

        ptReadingName: //We are reading a property name
          case Ch of
            'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
              lName := lName + Ch;
            ':':
              begin
                lNameSpace := lName;
                lName := '';
              end;
            '=':
              lPos := ptStartingContent;
            ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
              lPos := ptSpaceBeforeEqual;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte, [Ch]);
          end;

        ptStartingContent: //We are going to start a property content
          case Ch of
            ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
              ; // ignore white space
            '''', '"':
              begin
                lPropStart := Ch;
                lValue := '';
                lPos := ptReadingValue;
              end;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte_, [Ch]);
          end;
        ptReadingValue: //We are reading a property
          if Ch = lPropStart then
          begin
            if GetSimpleXML <> nil then
              GetSimpleXML.DoDecodeValue(lValue);
            with Add(lName, lValue) do
              NameSpace := lNameSpace;
            lPos := ptWaiting;
          end
          else
            lValue := lValue + Ch;
        ptSpaceBeforeEqual: // We are reading the white space between a property name and the = sign
          case Ch of
            ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed:
              ; // more white space, stay in this state and ignore
            '=':
              lPos := ptStartingContent;
          else
            FmtError(RsEInvalidXMLElementUnexpectedCharacte, [Ch]);
          end;
      else
        Assert(False, RsEUnexpectedValueForLPos);
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLProps.SaveToStream(const Stream: TStream);
var
  St: string;
  I: Integer;
begin
  St := '';
  for I := 0 to Count - 1 do
    St := St + Item[I].SaveToString;
  if St <> '' then
    WriteStringToStream(Stream, St, Length(St));
end;

function TAdSimpleXMLProps.Value(const Name: string; Default: string): string;
var
  Prop: TAdSimpleXMLProp;
begin
  Result := '';
  Prop := GetItemNamedDefault(Name, Default);
  if Prop = nil then
    Result := Default
  else
    Result := Prop.Value;
end;

//=== { TAdSimpleXMLProp } ==================================================

function TAdSimpleXMLProp.GetBoolValue: Boolean;
begin
  Result := StrToBoolDef(Value, False);
end;

function TAdSimpleXMLProp.GetFloatValue: Extended;
begin
  if not TryStrToFloat(Value, Result) then
    Result := 0.0;
end;

function TAdSimpleXMLProp.FullName: string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + ':' + Name
  else
    Result := Name;
end;

function TAdSimpleXMLProp.GetIntValue: Int64;
begin
  Result := StrToInt64Def(Value, -1);
end;

function TAdSimpleXMLProp.GetSimpleXML: TAdSimpleXML;
begin
  if (FParent <> nil) and (FParent.FParent <> nil) then
    Result := FParent.FParent.GetSimpleXML
  else
    Result := nil;
end;

function TAdSimpleXMLProp.SaveToString: string;
var
  AEncoder: TAdSimpleXML;
  tmp:string;
begin
  AEncoder := GetSimpleXML;
  tmp := FValue;
  if NameSpace <> '' then
  begin
    if AEncoder <> nil then
      AEncoder.DoEncodeValue(tmp);
    Result := Format(' %s:%s="%s"', [NameSpace, Name, tmp]);
  end
  else
  begin
    if AEncoder <> nil then
      AEncoder.DoEncodeValue(tmp);
    Result := Format(' %s="%s"', [Name, tmp]);
  end;
end;

procedure TAdSimpleXMLProp.SetBoolValue(const Value: Boolean);
begin
  FValue := BoolToStr(Value);
end;

procedure TAdSimpleXMLProp.SetFloatValue(const Value: Extended);
begin
  FValue := SecureFloatToStr(Value);
end;

procedure TAdSimpleXMLProp.SetIntValue(const Value: Int64);
begin
  FValue := IntToStr(Value);
end;

procedure TAdSimpleXMLProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then
  begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName := Value;
  end;
end;

//=== { TAdSimpleXMLElemClassic } ===========================================

procedure TAdSimpleXMLElemClassic.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML);
//<element Prop="foo" Prop='bar'/>
//<element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
//<xml:element Prop="foo" Prop='bar'>foor<b>beuh</b>bar</element>
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St, lName, lValue, lNameSpace: string;
  Ch: Char;
begin
  lStreamPos := Stream.Position;
  St := '';
  lValue := '';
  lNameSpace := '';
  lPos := 1;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);
      Ch := lBuf[I];

      case lPos of
        1:
          if Ch = '<' then
            lPos := 2
          else
            FmtError(RsEInvalidXMLElementExpectedBeginningO, [Ch]);
        -1:
          if Ch = '>' then
          begin
            Count := 0;
            Break;
          end
          else
            FmtError(RsEInvalidXMLElementExpectedEndOfTagBu, [Ch]);
      else
        begin
          if AnsiChar(Ch) in [AnsiTab, AnsiLineFeed, AnsiCarriageReturn, ' ' {, '.'}] then
          begin
            if lPos = 2 then
              Error(RsEInvalidXMLElementMalformedTagFoundn);
            Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Break; //Re read buffer
          end
          else
          begin
            case Ch of
              '>':
                begin
                  lName := St;
                  //Load elements
                  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
                  St := Items.LoadFromStream(Stream, AParent);
                  if lNameSpace <> '' then
                  begin
                    if not AnsiSameText(AnsiString(lNameSpace + ':' + lName), AnsiString(St)) then
                      FmtError(RsEInvalidXMLElementErroneousEndOfTagE, [lName, St]);
                  end
                  else
                    if not AnsiSameText(AnsiString(lName), AnsiString(St)) then
                      FmtError(RsEInvalidXMLElementErroneousEndOfTagE, [lName, St]);
                  lStreamPos := Stream.Position;

                  //Set value if only one sub element
                  //This might reduce speed, but this is for compatibility issues
                  if (Items.Count = 1) and (Items[0] is TAdSimpleXMLElemText) then
                  begin
                    lValue := Items[0].Value;
                    Items.Clear;
                  end;

                  Count := 0;
                  Break;
                end;
              '/':
                begin
                  lName := St;
                  lPos := -1;
                end;
              ':':
                begin
                  lNameSpace := St;
                  St := '';
                end;
            else
              begin
                St := St + Ch;
                Inc(lPos);
              end;
            end;
          end;
        end;
      end;
    end;
  until Count = 0;

  Name := lName;
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(lValue);
  Value := lValue;
  NameSpace := lNameSpace;

  if AParent <> nil then
  begin
    AParent.DoTagParsed(lName);
    AParent.DoValueParsed(lName, lValue);
  end;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemClassic.SaveToStream(const Stream: TStream; const Level: string; AParent: TAdSimpleXML);
var
  St, AName, tmp: string;
  LevelAdd: string;
begin
  if(NameSpace <> '') then
  begin
    AName := NameSpace + ':' + Name;
  end
  else
  begin
    AName := Name;
  end;

  if Name <> '' then
  begin
    if GetSimpleXML <> nil then
       GetSimpleXML.DoEncodeValue(AName);
    St := Level + '<' + AName;

    WriteStringToStream(Stream, St, Length(St));
    Properties.SaveToStream(Stream);
  end;

  if (Items.Count = 0) then
  begin
    tmp := FValue;
    if (Name <> '') then
    begin
      if Value = '' then
        St := '/>' + sLineBreak
      else
      begin
        if GetSimpleXML <> nil then
          GetSimpleXML.DoEncodeValue(tmp);
        St := '>' + tmp + '</' + AName + '>' + sLineBreak;
      end;
      WriteStringToStream(Stream, St, Length(St));
    end;
  end
  else
  begin
    if (Name <> '') then
    begin
      St := '>' + sLineBreak;
      WriteStringToStream(Stream, St, Length(St));
    end;
    if Assigned(SimpleXML) and
      (sxoAutoIndent in SimpleXML.Options) then
    begin
      LevelAdd := SimpleXML.IndentString;
    end;
    Items.SaveToStream(Stream, Level + LevelAdd, AParent);
    if Name <> '' then
    begin
      St := Level + '</' + AName + '>' + sLineBreak;
      WriteStringToStream(Stream, St, Length(St));
    end;
  end;
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemComment } ===========================================

procedure TAdSimpleXMLElemComment.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML);
//<!-- declarations for <head> & <body> -->
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT = '    -->';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<!--
          if lBuf[I] = CS_START_COMMENT[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidCommentExpectedsButFounds, [CS_START_COMMENT[lPos], lBuf[I]]);
        5:
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        6: //-
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
          begin
            St := St + '-' + lBuf[I];
            Dec(lPos);
          end;
        7: //>
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            if lBuf[I + 1] <> '>' then
              Error(RsEInvalidCommentNotAllowedInsideComme);
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemComment.SaveToStream(const Stream: TStream; const Level: string; AParent: TAdSimpleXML);
var
  St: string;
begin
  St := Level + '<!--';
  WriteStringToStream(Stream, St, Length(St));
  if Value <> '' then
    WriteStringToStream(Stream, Value, Length(Value));
  St := '-->' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemCData } =============================================

procedure TAdSimpleXMLElemCData.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML);
//<![CDATA[<greeting>Hello, world!</greeting>]]>
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA =  '         ]]>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  St := '';
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<![CDATA[
          if lBuf[I] = CS_START_CDATA[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidCDATAExpectedsButFounds, [CS_START_CDATA[lPos], lBuf[I]]);
        10:
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        11: //-
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
          begin
            St := St + ']' + lBuf[I];
            Dec(lPos);
          end;
        12: //>
          if lBuf[I] = CS_STOP_CDATA[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
          begin
            St := St + ']]' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCDATAUnexpectedEndOfData);

  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemCData.SaveToStream(const Stream: TStream; const Level: string; AParent: TAdSimpleXML);
var
  St: string;
begin
  St := Level + '<![CDATA[';
  WriteStringToStream(Stream, St, Length(St));
  if Value <> '' then
    WriteStringToStream(Stream, Value, Length(Value));
  St := ']]>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemText } ==============================================

procedure TAdSimpleXMLElemText.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML);
var
  I, lStreamPos, Count: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  StLength: Integer;
begin
  lStreamPos := Stream.Position;
  St := '';
  StLength := 0;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    SetLength(St, StLength + Count);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lBuf[I] of
        '<':
          begin
            //Quit text
            Dec(lStreamPos);
            Count := 0;
            Break;
          end;
      else
        begin
          Inc(StLength);
          St[StLength] := lBuf[I];
        end;
      end;
    end;
  until Count = 0;
  SetLength(St, StLength);
  if GetSimpleXML <> nil then
    GetSimpleXML.DoDecodeValue(St);
  Value := St;
  Name := '';

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemText.SaveToStream(const Stream: TStream; const Level: string; AParent: TAdSimpleXML);
var
  St, tmp: string;
begin
  if Value <> '' then
  begin
    tmp := Value;
    if GetSimpleXML <> nil then
      GetSimpleXML.DoEncodeValue(tmp);
    St := Level + tmp + sLineBreak;
    WriteStringToStream(Stream, St, Length(St));
  end;
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemHeader } ============================================

procedure TAdSimpleXMLElemHeader.Assign(Value: TAdSimpleXMLElem);
begin
  inherited Assign(Value);
  if Value is TAdSimpleXMLElemHeader then
  begin
    FStandalone := TAdSimpleXMLElemHeader(Value).FStandalone;
    FEncoding := TAdSimpleXMLElemHeader(Value).FEncoding;
    FVersion := TAdSimpleXMLElemHeader(Value).FVersion;
  end;
end;

constructor TAdSimpleXMLElemHeader.Create(const AOwner: TAdSimpleXMLElem);
begin
  inherited Create(AOwner);
  FVersion := '1.0';
  FEncoding := 'iso-8859-1';
  FStandalone := False;
end;

procedure TAdSimpleXMLElemHeader.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML);
//<?xml version="1.0" encoding="iso-xyzxx" standalone="yes"?>
const
  CS_START_HEADER = '<?xml';
  CS_STOP_HEADER = '     ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<?xml
          if lBuf[I] = CS_START_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_HEADER[lPos], lBuf[I]]);
        5: //L
          if lBuf[I] = CS_START_HEADER[lPos] then
          begin
            Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);

            FVersion := Properties.Value('version');
            FEncoding := Properties.Value('encoding');
            FStandalone := Properties.Value('standalone') = 'yes';

            Properties.Clear;

            Break; //Re read buffer
          end
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_HEADER[lPos], lBuf[I]]);
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_STOP_HEADER[lPos], lBuf[I]]);
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_STOP_HEADER[lPos], lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Name := '';

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemHeader.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TAdSimpleXML);
var
  St: string;
begin
  St := Level + '<?xml version="' + FVersion + '"';
  if Encoding <> '' then
    St := St + ' encoding="' + Encoding + '"';
  if StandAlone then
    St := St + ' standalone="yes"';
  St := St + '?>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemDocType } ===========================================

procedure TAdSimpleXMLElemDocType.LoadFromStream(const Stream: TStream; AParent: TAdSimpleXML);
{
<!DOCTYPE test [
<!ELEMENT test (#PCDATA) >
<!ENTITY % xx '&#37;zz;'>
<!ENTITY % zz '&#60;!ENTITY tricky "error-prone" >' >
%xx;
]>

<!DOCTYPE greeting SYSTEM "hello.dtd">
}
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
  lChar: Char;
  St: string;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;
  lChar := '>';
  St := '';

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<!DOCTYPE
          if lBuf[I] = CS_START_DOCTYPE[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidHeaderExpectedsButFounds, [CS_START_DOCTYPE[lPos], lBuf[I]]);
        10: //]> or >
          if lChar = lBuf[I] then
          begin
            if lChar = '>' then
            begin
              lOk := True;
              Count := 0;
              Break; //This is the end
            end
            else
            begin
              St := St + lBuf[I];
              lChar := '>';
            end;
          end
          else
          begin
            St := St + lBuf[I];
            if lBuf[I] = '[' then
              lChar := ']';
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidCommentUnexpectedEndOfData);

  Name := '';
  Value := Trim(St);

  if AParent <> nil then
    AParent.DoValueParsed('', St);

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemDocType.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TAdSimpleXML);
var
  St: string;
begin
  St := '<!DOCTYPE ' + Value + '>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemSheet } =============================================

procedure TAdSimpleXMLElemSheet.LoadFromStream(const Stream: TStream;
  AParent: TAdSimpleXML);
//<?xml-stylesheet alternate="yes" type="text/xsl" href="sheet.xsl"?>
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  lOk: Boolean;
begin
  lStreamPos := Stream.Position;
  lPos := 1;
  lOk := False;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..15: //<?xml-stylesheet
          if lBuf[I] = CS_START_PI[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_START_PI[lPos], lBuf[I]]);
        16: //L
          if lBuf[I] = CS_START_PI[lPos] then
          begin
            Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
            Properties.LoadFromStream(Stream);
            lStreamPos := Stream.Position;
            Inc(lPos);
            Break; //Re read buffer
          end
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_START_PI[lPos], lBuf[I]]);
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_STOP_PI[lPos], lBuf[I]]);
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            FmtError(RsEInvalidStylesheetExpectedsButFounds, [CS_STOP_PI[lPos], lBuf[I]]);
      end;
    end;
  until Count = 0;

  if not lOk then
    Error(RsEInvalidStylesheetUnexpectedEndOfDat);

  Name := '';

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemSheet.SaveToStream(const Stream: TStream;
  const Level: string; AParent: TAdSimpleXML);
var
  I: Integer;
  St: string;
begin
  St := Level + '<?xml-stylesheet';
  for I := 0 to Properties.GetCount - 1 do
    St := St + Properties.Item[I].SaveToString;
  St := St + '?>' + sLineBreak;
  WriteStringToStream(Stream, St, Length(St));
  if AParent <> nil then
    AParent.DoSaveProgress;
end;

//=== { TAdSimpleXMLElemsProlog } ===========================================

constructor TAdSimpleXMLElemsProlog.Create;
begin
  inherited Create;
  FElems := THashedStringList.Create;
end;

destructor TAdSimpleXMLElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);
  inherited Destroy;
end;

procedure TAdSimpleXMLElemsProlog.Clear;
var
  I: Integer;
begin
  for I := 0 to FElems.Count - 1 do
  begin
    FElems.Objects[I].Free;
    FElems.Objects[I] := nil;
  end;
  FElems.Clear;
end;

function TAdSimpleXMLElemsProlog.GetCount: Integer;
begin
  Result := FElems.Count;
end;

function TAdSimpleXMLElemsProlog.GetItem(const Index: Integer): TAdSimpleXMLElem;
begin
  Result := TAdSimpleXMLElem(FElems.Objects[Index]);
end;

function TAdSimpleXMLElemsProlog.LoadFromStream(
  const Stream: TStream; AParent: TAdSimpleXML): string;
{<?xml version="1.0" encoding="UTF-8" ?>
<!-- Test -->
<!DOCTYPE greeting [
  <!ELEMENT greeting (#PCDATA)>
]>
<greeting>Hello, world!</greeting>

<?xml version="1.0"?> <!DOCTYPE greeting SYSTEM "hello.dtd"> <greeting>Hello, world!</greeting>
}
var
  I, lStreamPos, Count, lPos: Integer;
  lBuf: array [0..cBufferSize - 1] of Char;
  St: string;
  lEnd: Boolean;
  lElem: TAdSimpleXMLElem;
begin
  lStreamPos := Stream.Position;
  Result := '';
  St := '';
  lPos := 0;

  repeat
    Count := ReadCharsFromStream(Stream, lBuf, Length(lBuf));
    if AParent <> nil then
      AParent.DoLoadProgress(Stream.Position, Stream.Size);
    for I := 0 to Count - 1 do
    begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces and any BOM
          begin
            case lBuf[I] of
              ' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed, #$00, #$FE, #$FF, #$EF, #$BB, #$BF:
                begin
                end;
              '<':
                begin
                  lPos := 1;
                  St := lBuf[I];
                end;
            else
              Error(RsEInvalidDocumentUnexpectedTextInFile);
            end;
          end;
        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            lEnd := False;

            if (St <> '<![CDATA') or not (AnsiChar(lBuf[I]) in [' ', AnsiTab, AnsiCarriageReturn, AnsiLineFeed]) then
              St := St + lBuf[I];
            if St = '<![CDATA[' then
              lEnd := True
            else
            if St = '<!--' then
              lElem := TAdSimpleXMLElemComment.Create(nil)
            else
            if St = '<?xml-stylesheet' then
              lElem := TAdSimpleXMLElemSheet.Create(nil)
            else
            if St = '<?xml ' then
              lElem := TAdSimpleXMLElemHeader.Create(nil)
            else
            if St = '<!DOCTYPE' then
              lElem := TAdSimpleXMLElemDocType.Create(nil)
            else
            if (Length(St) > 1) and not (AnsiChar(St[2]) in ['!', '?']) then
              lEnd := True;

            if lEnd then
            begin
              lStreamPos := lStreamPos - Length(St);
              Count := 0;
              Break;
            end
            else
            if lElem <> nil then
            begin
              Stream.Seek(lStreamPos - (Length(St)), {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
              lElem.LoadFromStream(Stream, AParent);
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;
          end;
      end;
    end;
  until Count = 0;

  Stream.Seek(lStreamPos, {$IFDEF COMPILER6_UP}soBeginning{$ELSE ~COMPILER6_UP}soFromBeginning{$ENDIF ~COMPILER6_UP});
end;

procedure TAdSimpleXMLElemsProlog.SaveToStream(const Stream: TStream; AParent: TAdSimpleXML);
var
  I: Integer;
begin
  FindHeader;
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, '', AParent);
end;

//=== { TAdSimpleHashTable } ================================================

constructor TAdSimpleHashTable.Create;
begin
  inherited Create;
  //XXX
  {$IFDEF CLR}
  FList := TAdHashRecord.Create;
  {$ELSE}
  New(FList);
  {$ENDIF CLR}
  FList.Count := 0;
  FList.Kind := hkDirect;
  FList.FirstElem := nil;
end;

destructor TAdSimpleHashTable.Destroy;
begin
  Clear;
  {$IFNDEF CLR}
  Dispose(FList);
  {$ENDIF !CLR}
  inherited Destroy;
end;

procedure TAdSimpleHashTable.AddObject(const AName: string;
  AObject: TObject);
begin
  //XXX
  {$IFDEF CLR}
  FList.FirstElem := TAdHashElem.Create;
  {$ELSE}
  New(FList.FirstElem);
  {$ENDIF CLR}
  //FList.FirstElem.Value := AName;
  //FList.FirstElem.Obj := nil;
end;

procedure TAdSimpleHashTable.Clear;
begin
  //XXX
end;

{$IFNDEF CLR}
{$IFDEF COMPILER6_UP}

function VarXML: TVarType;
begin
  Result := XMLVariant.VarType;
end;

procedure XMLCreateInto(var ADest: Variant; const AXML: TAdSimpleXMLElem);
begin
  TXMLVarData(ADest).vType := VarXML;
  TXMLVarData(ADest).XML := AXML;
end;

function XMLCreate(const AXML: TAdSimpleXMLElem): Variant;
begin
  XMLCreateInto(Result, AXML);
end;

function XMLCreate: Variant;
begin
  XMLCreateInto(Result, TAdSimpleXMLElemClassic.Create(nil));
end;

//=== { TXMLVariant } ========================================================

procedure TXMLVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if Source.vType = VarType then
  begin
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TXMLVarData(Source).XML.SaveToString);
      varString:
        VarDataFromStr(Dest, TXMLVarData(Source).XML.SaveToString);
    else
      RaiseCastError;
    end;
  end
  else
    inherited CastTo(Dest, Source, AVarType);
end;

procedure TXMLVariant.Clear(var V: TVarData);
begin
  V.vType := varEmpty;
  TXMLVarData(V).XML := nil;
end;

procedure TXMLVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin          
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TXMLVarData(Dest) do
    begin
      vType := VarType;
      XML := TXMLVarData(Source).XML;
    end;
end;

function TXMLVariant.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  LXML: TAdSimpleXMLElem;
  I, J, K: Integer;
begin
  Result := False;
  if (Length(Arguments) = 1) and (Arguments[0].vType in [vtInteger, vtExtended]) then
    with TXMLVarData(V) do
    begin
      K := Arguments[0].vInteger;
      J := 0;

      if K > 0 then
        for I := 0 to XML.Items.Count - 1 do
          if UpperCase(XML.Items[I].Name) = Name then
          begin
            Inc(J);
            if J = K then
              Break;
          end;

      if (J = K) and (J < XML.Items.Count) then
      begin
        LXML := XML.Items[J];
        if LXML <> nil then
        begin
          Dest.vType := VarXML;
          TXMLVarData(Dest).XML := LXML;
          Result := True;
        end
      end;
    end;
end;

function TXMLVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
var
  LXML: TAdSimpleXMLElem;
  lProp: TAdSimpleXMLProp;
begin
  Result := False;
  with TXMLVarData(V) do
  begin
    LXML := XML.Items.ItemNamed[Name];
    if LXML <> nil then
    begin
      Dest.vType := VarXML;
      TXMLVarData(Dest).XML := LXML;
      Result := True;
    end
    else
    begin
      lProp := XML.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        VarDataFromOleStr(Dest, lProp.Value);
        Result := True;
      end;
    end;
  end;
end;

function TXMLVariant.IsClear(const V: TVarData): Boolean;
begin
  Result := (TXMLVarData(V).XML = nil) or (TXMLVarData(V).XML.Items.Count = 0);
end;

function TXMLVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;

  function GetStrValue: string;
  begin
    try
      Result := Value.VOleStr;
    except
      Result := '';
    end;
  end;

var
  LXML: TAdSimpleXMLElem;
  lProp: TAdSimpleXMLProp;
begin
  Result := False;
  with TXMLVarData(V) do
  begin
    LXML := XML.Items.ItemNamed[Name];
    if LXML = nil then
    begin
      lProp := XML.Properties.ItemNamed[Name];
      if lProp <> nil then
      begin
        lProp.Value := GetStrValue;
        Result := True;
      end;
    end
    else
    begin
      LXML.Value := GetStrValue;
      Result := True;
    end;
  end;
end;

{$ENDIF COMPILER6_UP}
{$ENDIF !CLR}

procedure TAdSimpleXMLElemsProlog.Error(const S: string);
begin
  raise EAdSimpleXMLError.Create(S);
end;

procedure TAdSimpleXMLElemsProlog.FmtError(const S: string;
  const Args: array of const);
begin
  Error(Format(S, Args));
end;

procedure TAdSimpleXML.SetIndentString(const Value: string);
var
  I: Integer;
begin
  // test if the new value is only made of spaces or tabs
  for I := 1 to Length(Value) do
    if not (AnsiChar(Value[I]) in [AnsiTab, ' ']) then
      Exit;
  FIndentString := Value;
end;

procedure TAdSimpleXML.SetRoot(const Value: TAdSimpleXMLElemClassic);
begin
  if Value <> FRoot then
  begin
//    FRoot.FSimpleXML := nil;
    FRoot := Value;
//    FRoot.FSimpleXML := Self;
  end;
end;

function TAdSimpleXMLElemsProlog.GetEncoding: string;
var
  Elem: TAdSimpleXMLElemHeader;
begin
  Elem := TAdSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Encoding
  else
    Result := 'UTF-8';
end;

function TAdSimpleXMLElemsProlog.GetStandAlone: Boolean;
var
  Elem: TAdSimpleXMLElemHeader;
begin
  Elem := TAdSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.StandAlone
  else
    Result := False;
end;

function TAdSimpleXMLElemsProlog.GetVersion: string;
var
  Elem: TAdSimpleXMLElemHeader;
begin
  Elem := TAdSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Result := Elem.Version
  else
    Result := '1.0';
end;

procedure TAdSimpleXMLElemsProlog.SetEncoding(const Value: string);
var
  Elem: TAdSimpleXMLElemHeader;
begin
  Elem := TAdSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Encoding := Value;
end;

procedure TAdSimpleXMLElemsProlog.SetStandAlone(const Value: Boolean);
var
  Elem: TAdSimpleXMLElemHeader;
begin
  Elem := TAdSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.StandAlone := Value;
end;

procedure TAdSimpleXMLElemsProlog.SetVersion(const Value: string);
var
  Elem: TAdSimpleXMLElemHeader;
begin
  Elem := TAdSimpleXMLElemHeader(FindHeader);
  if Elem <> nil then
    Elem.Version := Value;
end;

function TAdSimpleXMLElemsProlog.FindHeader: TAdSimpleXMLElem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Item[I] is TAdSimpleXMLElemHeader then
    begin
      Result := Item[I];
      Exit;
    end;
  // (p3) if we get here, an xml header was not found
  Result := TAdSimpleXMLElemHeader.Create(nil);
  Result.Name := 'xml';
  FElems.AddObject('', Result);
end;

function TAdSimpleXMLElemsProlog.AddStyleSheet(AType, AHRef: string): TAdSimpleXMLElemSheet;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TAdSimpleXMLElemSheet.Create(nil);
  Result.Name := 'xml-stylesheet';
  Result.Properties.Add('type',AType);
  Result.Properties.Add('href',AHRef);
  FElems.AddObject('xml-stylesheet', Result);
end;

function TAdSimpleXMLElemsProlog.AddComment(const AValue: string): TAdSimpleXMLElemComment;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TAdSimpleXMLElemComment.Create(nil);
  Result.Value := AValue;
  FElems.AddObject('', Result);
end;

function TAdSimpleXMLElemsProlog.AddDocType(const AValue: string): TAdSimpleXMLElemDocType;
begin
  // make sure there is an xml header
  FindHeader;
  Result := TAdSimpleXMLElemDocType.Create(nil);
  Result.Value := AValue;
  FElems.AddObject('', Result);
end;

initialization

finalization
  {$IFNDEF CLR}
  {$IFDEF COMPILER6_UP}
  FreeAndNil(GlobalXMLVariant);
  {$ENDIF COMPILER6_UP}
  {$ENDIF !CLR}
  FreeAndNil(GlobalSorts);

end.
