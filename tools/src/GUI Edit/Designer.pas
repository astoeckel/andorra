unit Designer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AdDraws, AdClasses, StdCtrls, AdSkin, AdGUI, AdPNG,
  ExtCtrls, AdGUIConnector, Menus, ClipBrd, AdSimpleXML, ImgList, XMLEdit,
  AdPerformanceCounter, AdTypes, AdPersistent, AdEvents;

type
  TDesignerDlg = class(TForm)
    PopupMenu1: TPopupMenu;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    N1: TMenuItem;
    Paste1: TMenuItem;
    N2: TMenuItem;
    Sendtoback1: TMenuItem;
    Bringtofront1: TMenuItem;
    N3: TMenuItem;
    Export1: TMenuItem;
    N4: TMenuItem;
    Delete1: TMenuItem;
    ImageList1: TImageList;
    EditXML1: TMenuItem;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Sendtoback1Click(Sender: TObject);
    procedure Bringtofront1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure EditXML1Click(Sender: TObject);
  private
    AddComp:TAdComponentClass;
    AddRect:TAdRect;
    FirstPoint:boolean;
  public
    AdDraw1:TAdDraw;
    AdPerCounter:TAdPerformanceCounter;
    AdGUI:TAdGUI;
    AdImage:TAdImage;
    AdConnector:TAdGUIConnector;
    OnFocus:TNotifyEvent;
    OnChangeList:TNotifyEvent;
    procedure Idle(Sender:TObject;var Done:boolean);
    procedure AddComponent(AClass:TAdComponentClass);
    procedure Copy;
    procedure Delete;
    procedure Cut;
    procedure Paste;
  end;

var
  DesignerDlg: TDesignerDlg;
  CF_ADCOMPONENT: Word;

implementation

{$R *.dfm}

procedure CopyStreamToClipboard(fmt: Cardinal; S: TStream);
var
  hMem: THandle;
  pMem: Pointer;
begin
  S.Position := 0;
  hMem       := GlobalAlloc(GHND or GMEM_DDESHARE, S.Size);
  if hMem <> 0 then 
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then 
    begin
      S.Read(pMem^, S.Size);
      S.Position := 0;
      GlobalUnlock(hMem);
      Clipboard.Open;
      try
        Clipboard.SetAsHandle(fmt, hMem);
      finally
        Clipboard.Close;
      end;
    end
    else 
    begin
      GlobalFree(hMem);
      OutOfMemoryError;
    end;
  end
  else
    OutOfMemoryError;
end;

procedure CopyStreamFromClipboard(fmt: Cardinal; S: TStream);
var
  hMem: THandle;
  pMem: Pointer;
begin
  hMem := Clipboard.GetAsHandle(fmt);
  if hMem <> 0 then 
  begin
    pMem := GlobalLock(hMem);
    if pMem <> nil then 
    begin
      S.Write(pMem^, GlobalSize(hMem));
      S.Position := 0;
      GlobalUnlock(hMem);
    end
    else
      raise Exception.Create('CopyStreamFromClipboard: could not lock global handle ' +
        'obtained from clipboard!');
  end;
end;

procedure TDesignerDlg.AddComponent(AClass: TAdComponentClass);
begin
  AddComp := AClass;
  FirstPoint := false;
  AdConnector.RestoreEventHandlers;
  Cursor := crCross;
end;

procedure TDesignerDlg.Bringtofront1Click(Sender: TObject);
begin
  if (AdGUI.FocusedComponent <> nil) then
  begin
    AdGUI.FocusedComponent.BringToFront;
  end;
end;

procedure TDesignerDlg.Delete1Click(Sender: TObject);
begin
  Delete;
end;

procedure TDesignerDlg.EditXML1Click(Sender: TObject);
var
  Edit:TXMLEditor;
  ms:TMemoryStream;
begin
  Edit := TXMLEditor.Create(nil);
  ms := TMemoryStream.Create;
  AdGUI.SaveToStream(ms);
  ms.Position := 0;
  Edit.Memo1.Lines.LoadFromStream(ms);
  if Edit.ShowModal = mrOk then
  begin
    ms.Clear;
    Edit.Memo1.Lines.SaveToStream(ms);
    ms.Position := 0;
    AdGUI.LoadFromStream(ms);
  end;
  ms.Free;
  Edit.Free;
end;

procedure TDesignerDlg.Export1Click(Sender: TObject);
var
  filename:string;
begin
  if (AdGUI.FocusedComponent <> nil) then
  begin
    if SaveDialog1.Execute then
    begin
      if SaveDialog1.FilterIndex = 1 then
      begin
        filename := ChangeFileExt(SaveDialog1.FileName,'.axc');
      end
      else
      begin
        filename := SaveDialog1.FileName;
      end;
      AdGUI.FocusedComponent.SaveToFile(filename);
    end;
  end;
end;

procedure TDesignerDlg.Copy;
var ms:TMemoryStream;
begin
  if (AdGUI.FocusedComponent <> nil) and (AdGUI.FocusedComponent <> AdGUI) then
  begin
    ms := TMemoryStream.Create;
    AdGUI.FocusedComponent.SaveToStream(ms);
    ms.Position := 0;
    CopyStreamToClipboard(CF_ADCOMPONENT,ms);
    ms.Free;
    if Assigned(OnChangeList) then
      OnChangeList(self);
  end;
end;

procedure TDesignerDlg.Copy1Click(Sender: TObject);
begin
  Copy;
end;

procedure TDesignerDlg.Cut;
begin
  Copy;
  Delete;
end;

procedure TDesignerDlg.Cut1Click(Sender: TObject);
begin
  Cut;
end;

procedure TDesignerDlg.Delete;
begin
  if (AdGUI.FocusedComponent <> nil) and (AdGUI.FocusedComponent <> AdGUI) then
  begin
    AdGUI.FocusedComponent.Free;
    if Assigned(OnChangeList) then
      OnChangeList(self);
  end;
end;

procedure TDesignerDlg.Paste;
var
  cref:TAdComponentClass;
  XML:TAdSimpleXML;
  ms:TMemoryStream;
begin
  if (AdGUI.FocusedComponent <> nil) and (Clipboard.HasFormat(CF_ADCOMPONENT))then
  begin
    ms := TMemoryStream.Create;
    CopyStreamFromClipboard(CF_ADCOMPONENT, ms);
    ms.Position := 0;
    XML := TAdSimpleXML.Create;
    XML.LoadFromStream(ms);
    cref := TAdComponentClass(AdGetClass(XML.Root.Name));
    if cref <> nil then
    begin
      with TAdComponent(cref.Create(AdGUI.FocusedComponent)) do
      begin
        LoadFromXML(XML.Root);
        Name := GetUniqueName(System.Copy(ClassName,2,Length(ClassName)-1),true);
      end;
    end;
    XML.Free;
    ms.Free;
    if Assigned(OnChangeList) then
      OnChangeList(self);
  end;
end;

procedure TDesignerDlg.Paste1Click(Sender: TObject);
begin
  Paste;
end;

procedure TDesignerDlg.FormCreate(Sender: TObject);
begin
  AdPerCounter := TAdPerformanceCounter.Create;

  AdDraw1 := TAdDraw.Create(self);
  AdDraw1.DllName := 'AndorraOGL.dll';
  if AdDraw1.Initialize then
  begin
    Application.OnIdle := Idle;

    AdImage := TAdImage.Create(AdDraw1);

    AdGUI := TAdGUI.Create(AdDraw1);
    AdGUI.Skin.LoadFromFile('sunna.axs');
    AdGUI.Cursors.LoadFromFile('cursors.xml');
    AdGUI.Cursors.Visible := false;

    AdGUI.DesignMode := true;

    AdConnector := TAdGUIConnector.Create(AdGUI);
    AdConnector.ConnectEventHandlers(AdDraw1.Window);

    if Assigned(OnChangeList) then
      OnChangeList(self);
  end
  else
  begin
    ShowMessage('Error while initializing Andorra 2D. Try to use another display'+
                'mode or another video adapter.');
    halt;
  end;
end;

procedure TDesignerDlg.FormDestroy(Sender: TObject);
begin
  AdConnector.Free;
  AdGUI.Free;
  AdPerCounter.Free;
  AdImage.Free;
  AdDraw1.Free;
end;

procedure TDesignerDlg.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not AdGUI.DesignMode then exit;
  
  if Key = VK_ESCAPE then
  begin
    AddComp := nil;
    FirstPoint := false;
    Cursor := crDefault;
  end;
  if Key = VK_DELETE then
  begin
    Delete;
  end;
  if (Key = $58) and (ssCtrl in Shift) then
  begin
    Cut;
  end;
  if (Key = $43) and (ssCtrl in Shift) then
  begin
    Copy;
  end;
  if (Key = $56) and (ssCtrl in Shift) then
  begin
    Paste;
  end;
end;

procedure TDesignerDlg.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p:TPoint;
  b:boolean;
begin
  if (not FirstPoint) and (AddComp <> nil) then
  begin
    AdGUI.MouseUp(abLeft, [], X,Y);
    FirstPoint := true;
    AddRect.Left := (X div AdGUI.GridX) * AdGUI.GridX;
    AddRect.Top := (Y div AdGUI.GridY) * AdGUI.GridY;
    AddRect.Right := (X div AdGUI.GridX) * AdGUI.GridX;
    AddRect.Bottom := (Y div AdGUI.GridY) * AdGUI.GridY;
  end;
  if (Button = mbRight) and (AdGUI.DesignMode) then
  begin
    AdConnector.RestoreEventHandlers;
    if AdGUI.FocusedComponent <> nil then
    begin
      b := false;
      Idle(nil,b);
      p := ClientToScreen(Point(X,Y));
      PopupMenu1.Popup(p.X,p.Y);
    end;
  end;
end;

procedure TDesignerDlg.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  b:boolean;
begin
  if (not AdConnector.Connected) and (AddComp = nil) then
  begin
    AdConnector.ConnectEventHandlers(AdDraw1.Window);
    AdGUI.MouseUp(abLeft,[asLeft],X,Y);
    b := false;
    Idle(nil,b);
  end;
  
  if FirstPoint and (AddComp <> nil) then
  begin
    AddRect.Right := (X div AdGUI.GridX) * AdGUI.GridX;
    AddRect.Bottom := (Y div AdGUI.GridY) * AdGUI.GridY;
  end;
end;

procedure TDesignerDlg.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p1,p2:TAdPoint;
  comp,add:TAdComponent;
begin
  if Assigned(OnFocus) and (AdGUI.FocusedComponent <> nil) then
  begin
    OnFocus(self);
  end;

  if (FirstPoint) and (AddComp <> nil) then
  begin
    if AddRect.Left < AddRect.Right then
    begin
      p1.X := AddRect.Left;
      p2.X := AddRect.Right;
    end
    else
    begin
      p2.X := AddRect.Left;
      p1.X := AddRect.Right;
    end;
    if AddRect.Top < AddRect.Bottom then
    begin
      p1.Y := AddRect.Top;
      p2.Y := AddRect.Bottom;
    end
    else
    begin
      p2.Y := AddRect.Top;
      p1.Y := AddRect.Bottom;
    end;

    comp := AdGUI.FocusedComponent;
    if comp = nil then comp := AdGUI;
    

    add := AddComp.Create(comp);
    with add do
    begin
      X := comp.ScreenToClient(p1).X;
      Y := comp.ScreenToClient(p1).Y;
      Width := p2.X - p1.X;
      Height := p2.Y - p1.Y;
    end;

    AddComp := nil;
    FirstPoint := false;
    AdConnector.ConnectEventHandlers(AdDraw1.Window);
    Cursor := crDefault;

    if Assigned(OnChangeList) then
      OnChangeList(self);
  end;
end;

procedure TDesignerDlg.FormResize(Sender: TObject);
begin
  if AdDraw1.Initialized then
  begin
    AdDraw1.Setup2DScene(ClientWidth, ClientHeight);
  end;
end;

procedure TDesignerDlg.Idle(Sender: TObject; var Done: boolean);
begin
  if AdDraw1.CanDraw then
  begin
    AdPerCounter.Calculate;
    with AdDraw1 do
    begin
      ClearSurface(ColorToRGB(clBtnFace));
      BeginScene;
      AdImage.StretchDraw(AdDraw1,AdDraw1.DisplayRect,0);

      AdGUI.Update(AdPerCounter.TimeGap / 1000);

      if (AddComp <> nil) and (FirstPoint) then
      begin
        with AdDraw1.Canvas do
        begin
          Pen.Color := Ad_ARGB(128,128,128,128);
          Brush.Color := Ad_ARGB(64,128,128,255);
          Brush.GradientColor := Ad_ARGB(64,128,128,200);
          Rectangle(AddRect);
          Release;
        end;
      end;      

      EndScene;
      Flip;
    end;

 end;
 Done := AdGUI.DesignMode; 
end;

procedure TDesignerDlg.Sendtoback1Click(Sender: TObject);
begin
  if (AdGUI.FocusedComponent <> nil) then
  begin
    AdGUI.FocusedComponent.SendToBack;
  end;
end;

initialization
  CF_ADCOMPONENT := RegisterClipBoardFormat('Andorra Component');

finalization

end.
