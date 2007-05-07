unit Objects;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvComponentBase, JvInspector, JvExControls, JvComponent, ExtCtrls,
  StdCtrls, TypInfo;

type
  TObjectsDlg = class(TForm)
    ComboBox1: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure ItemDoubleClicked(Sender:TObject; Item:TJvCustomInspectorItem);
  public
    OnClickListEntry:TNotifyEvent;
    OnDblClickFont:TNotifyEvent;
    OnDblClickImage:TNotifyEvent;
    Inspector:TJvInspector;
    Painter:TJvInspectorBorlandPainter;
    Change:boolean;
  end;

implementation

{$R *.dfm}

procedure TObjectsDlg.ComboBox1Change(Sender: TObject);
begin
  if Combobox1.ItemIndex <> -1 then
  begin
    if Assigned(OnClickListEntry) and not Change then
    begin
      OnClickListEntry(Combobox1.Items.Objects[Combobox1.ItemIndex]);
    end;
  end;
end;

procedure TObjectsDlg.FormCreate(Sender: TObject);
begin
  Inspector := TJvInspector.Create(self);
  Inspector.Parent := self;
  Inspector.Align := alClient;

  Painter := TJvInspectorBorlandPainter.Create(self);
  Inspector.Painter := Painter;
  Inspector.OnItemDoubleClicked := ItemDoubleClicked;
end;

procedure TObjectsDlg.FormDestroy(Sender: TObject);
begin
  Inspector.Free;
  Painter.Free;
end;

procedure TObjectsDlg.ItemDoubleClicked(Sender: TObject;
  Item: TJvCustomInspectorItem);
var
  PropInfo:PPropInfo;
begin
  if (Item.Name = 'Font') and Assigned(OnDblClickFont) then
  begin
    OnDblClickFont(Self);
  end;
  if (Item.DisplayValue = '(TAdResourceImage)') and Assigned(OnDblClickFont) then
  begin
    PropInfo := GetPropInfo(Inspector.InspectObject.ClassInfo, Item.Name);
    OnDblClickImage(GetObjectProp(Inspector.InspectObject,Item.Name));
  end;
end;

end.
