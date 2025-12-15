unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ComCtrls, Menus, LCLType, FONCreator, WinFont;
const
  ProgramName = 'RetroNick'#39's Bitmap Font Editor';
type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnClearAll: TButton;
    btnClearChar: TButton;
    btnShiftLeft: TButton;
    btnShiftRight: TButton;
    btnShiftUp: TButton;
    btnShiftDown: TButton;
    btnInvert: TButton;
    btnFlipH: TButton;
    btnFlipV: TButton;
    btnCopyChar: TButton;
    btnPasteChar: TButton;
    btnApplyRange: TButton;
    btnResetLines: TButton;
    btnScanLines: TButton;
    cboBold: TCheckBox;
    cboItalic: TCheckBox;
    cboUnderline: TCheckBox;
    cboShowGrid: TCheckBox;
    cboShowBaseline: TCheckBox;
    chkShowAscender: TCheckBox;
    chkShowDescender: TCheckBox;
    chkShowXHeight: TCheckBox;
    cmbCharSet: TComboBox;
    cmbPitchFamily: TComboBox;
    cmbZoom: TComboBox;
    edtCopyright: TEdit;
    edtFontName: TEdit;
    edtPreviewText: TEdit;
    gbCharEditor: TGroupBox;
    gbCharList: TGroupBox;
    gbFontProps: TGroupBox;
    gbPreview: TGroupBox;
    gbLineMarkers: TGroupBox;
    lblCharSet: TLabel;
    lblCopyright: TLabel;
    lblCurrentChar: TLabel;
    lblFontName: TLabel;
    lblHeight: TLabel;
    lblPitchFamily: TLabel;
    lblPointSize: TLabel;
    lblZoom: TLabel;
    lblPreviewText: TLabel;
    lblCharWidth: TLabel;
    lblRangeStart: TLabel;
    lblRangeTo: TLabel;
    lblLineValues: TLabel;
    lstChars: TListBox;
    MainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuEdit: TMenuItem;
    mnuTools: TMenuItem;
    mnuHelp: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSep1: TMenuItem;
    mnuExit: TMenuItem;
    mnuUndo: TMenuItem;
    mnuRedo: TMenuItem;
    mnuSep2: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuGenerateFromFont: TMenuItem;
    mnuSep4: TMenuItem;
    mnuFontMetrics: TMenuItem;
    mnuAbout: TMenuItem;
    pnlCharEdit: TPaintBox;
    pnlPreview: TPaintBox;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    sbCharEdit: TScrollBox;
    spnAscent: TSpinEdit;
    spnHeight: TSpinEdit;
    spnPointSize: TSpinEdit;
    spnCharWidth: TSpinEdit;
    spnRangeStart: TSpinEdit;
    spnRangeEnd: TSpinEdit;
    spnAscenderLine: TSpinEdit;
    spnDescenderLine: TSpinEdit;
    spnXHeightLine: TSpinEdit;
    StatusBar: TStatusBar;
    tmrPreview: TTimer;
    procedure btnApplyRangeClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnClearCharClick(Sender: TObject);
    procedure btnCopyCharClick(Sender: TObject);
    procedure btnFlipHClick(Sender: TObject);
    procedure btnFlipVClick(Sender: TObject);
    procedure btnInvertClick(Sender: TObject);
    procedure btnLoadFontClick(Sender: TObject);
    procedure btnPasteCharClick(Sender: TObject);
    procedure btnSaveFontClick(Sender: TObject);
    procedure btnShiftDownClick(Sender: TObject);
    procedure btnShiftLeftClick(Sender: TObject);
    procedure btnShiftRightClick(Sender: TObject);
    procedure btnShiftUpClick(Sender: TObject);
    procedure btnResetLinesClick(Sender: TObject);
    procedure btnScanLinesClick(Sender: TObject);

    procedure cboShowBaselineChange(Sender: TObject);
    procedure cboShowGridChange(Sender: TObject);
    procedure chkLineMarkerChange(Sender: TObject);
    procedure spnLineMarkerChange(Sender: TObject);
    procedure cmbZoomChange(Sender: TObject);
    procedure edtPreviewTextChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lstCharsClick(Sender: TObject);
    procedure lstCharsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuFontMetricsClick(Sender: TObject);
    procedure mnuGenerateFromFontClick(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuUndoClick(Sender: TObject);
    procedure mnuRedoClick(Sender: TObject);
    procedure pnlCharEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlCharEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlCharEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pnlCharEditPaint(Sender: TObject);
    procedure pnlPreviewPaint(Sender: TObject);
    procedure spnCharWidthChange(Sender: TObject);
    procedure spnHeightChange(Sender: TObject);
    procedure tmrPreviewTimer(Sender: TObject);
  private
    FCreator: TFONCreator;
    FCurrentChar: Integer;
    FCharBitmaps: array[0..255] of TBitmap;
    FUndoStack: array[0..255] of TList;
    FRedoStack: array[0..255] of TList;
    FPixelSize: Integer;
    FDrawing: Boolean;
    FDrawColor: Boolean;
    FModified: Boolean;
    FCurrentFile: string;
    FShowGrid: Boolean;
    FShowBaseline: Boolean;
    FClipboardBitmap: TBitmap;
    FInitializing: Boolean;
    
    // Character range
    FCharRangeStart: Integer;
    FCharRangeEnd: Integer;
    FCharEnabled: array[0..255] of Boolean;
    
    // Line markers
    FShowAscenderLine: Boolean;
    FShowDescenderLine: Boolean;
    FShowXHeightLine: Boolean;
    FAscenderLine: Integer;    // -1 = auto
    FDescenderLine: Integer;   // -1 = auto
    FXHeightLine: Integer;     // -1 = auto
    
    procedure UpdateCharList;
    procedure UpdatePreview;
    procedure UpdateStatus;
    procedure UpdateTitle;
    procedure UpdateLineMarkerDisplay;
    procedure SetPixel(X, Y: Integer; Value: Boolean);
    function GetPixelAt(SX, SY: Integer; out PX, PY: Integer): Boolean;
    procedure EnsureCharBitmap(Idx: Integer);
    procedure SaveUndoState;
    procedure LoadFONFile(const FN: string);
    procedure LoadFNTFile(const FN: string);
    procedure SaveFNTFile(const FN: string);
    procedure ResizeCharBitmap(Idx, NewW, NewH: Integer);
    procedure UpdateEditorSize;
    function ConfirmSave: Boolean;
    procedure SetModified(V: Boolean);
    procedure ClearUndoRedo(Idx: Integer);
    procedure ApplyCharRange;
    procedure ScanFontLines(out AscenderY, DescenderY, XHeightY: Integer);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  FInitializing := True;
  FCreator := TFONCreator.Create;
  FCurrentChar := 65;
  FPixelSize := 16;
  FDrawing := False;
  FModified := False;
  FCurrentFile := '';
  FShowGrid := True;
  FShowBaseline := True;
  FClipboardBitmap := nil;
  
  // Character range
  FCharRangeStart := 32;
  FCharRangeEnd := 127;
  
  // Line markers
  FShowAscenderLine := True;
  FShowDescenderLine := True;
  FShowXHeightLine := True;
  FAscenderLine := -1;
  FDescenderLine := -1;
  FXHeightLine := -1;
  
  for I := 0 to 255 do
  begin
    FCharBitmaps[I] := nil;
    FUndoStack[I] := TList.Create;
    FRedoStack[I] := TList.Create;
    FCharEnabled[I] := (I >= 32) and (I <= 127);
  end;
  
  cmbCharSet.Items.AddStrings(['ANSI (0)', 'Default (1)', 'Symbol (2)', 'OEM (255)']);
  cmbCharSet.ItemIndex := 0;
  
  cmbPitchFamily.Items.AddStrings(['Default', 'Fixed', 'Variable', 'Roman', 'Swiss', 'Modern', 'Script', 'Decorative']);
  cmbPitchFamily.ItemIndex := 2;
  
  cmbZoom.Items.AddStrings(['8x', '12x', '16x', '20x', '24x', '32x']);
  cmbZoom.ItemIndex := 2;
  
  edtFontName.Text := 'MyFont';
  edtCopyright.Text := 'Created with FON Creator';
  spnPointSize.Value := 10;
  spnHeight.Value := 12;
  spnAscent.Value := 10;
  edtPreviewText.Text := 'Hello World! ABC abc 123';
  cboShowGrid.Checked := True;
  cboShowBaseline.Checked := True;
  
  for I := 32 to 127 do EnsureCharBitmap(I);
  
  lstChars.Style := lbOwnerDrawFixed;
  lstChars.ItemHeight := 20;
  
  UpdateCharList;
  UpdateStatus;
  UpdateTitle;
  UpdateEditorSize;
  UpdateLineMarkerDisplay;
  pnlCharEdit.Repaint;
  FInitializing := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
var
  I, J: Integer;
begin
  FCreator.Free;
  for I := 0 to 255 do
  begin
    if FCharBitmaps[I] <> nil then FCharBitmaps[I].Free;
    for J := 0 to FUndoStack[I].Count - 1 do TBitmap(FUndoStack[I][J]).Free;
    FUndoStack[I].Free;
    for J := 0 to FRedoStack[I].Count - 1 do TBitmap(FRedoStack[I][J]).Free;
    FRedoStack[I].Free;
  end;
  if FClipboardBitmap <> nil then FClipboardBitmap.Free;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not ConfirmSave then CloseAction := caNone;
end;

function TfrmMain.ConfirmSave: Boolean;
var
  R: TModalResult;
begin
  Result := True;
  if FModified then
  begin
    R := MessageDlg('Save Changes?', 'Font modified. Save?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case R of
      mrYes: begin btnSaveFontClick(nil); Result := not FModified; end;
      mrNo: Result := True;
      mrCancel: Result := False;
    end;
  end;
end;

procedure TfrmMain.SetModified(V: Boolean);
begin
  FModified := V;
  UpdateTitle;
end;

procedure TfrmMain.UpdateTitle;
begin
  if FCurrentFile <> '' then
    Caption := ProgramName+' - ' + ExtractFileName(FCurrentFile)
  else
    Caption := ProgramName+' - [Untitled]';
  if FModified then Caption := Caption + ' *';
end;

procedure TfrmMain.EnsureCharBitmap(Idx: Integer);
begin
  if FCharBitmaps[Idx] = nil then
  begin
    FCharBitmaps[Idx] := TBitmap.Create;
    FCharBitmaps[Idx].Width := 8;
    FCharBitmaps[Idx].Height := spnHeight.Value;
    FCharBitmaps[Idx].Canvas.Brush.Color := clWhite;
    FCharBitmaps[Idx].Canvas.FillRect(0, 0, 8, spnHeight.Value);
  end;
end;

procedure TfrmMain.ClearUndoRedo(Idx: Integer);
var
  I: Integer;
begin
  for I := 0 to FUndoStack[Idx].Count - 1 do TBitmap(FUndoStack[Idx][I]).Free;
  FUndoStack[Idx].Clear;
  for I := 0 to FRedoStack[Idx].Count - 1 do TBitmap(FRedoStack[Idx][I]).Free;
  FRedoStack[Idx].Clear;
end;

procedure TfrmMain.SaveUndoState;
var
  Bmp, Copy: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  Copy := TBitmap.Create;
  Copy.Assign(Bmp);
  FUndoStack[FCurrentChar].Add(Copy);
  if FUndoStack[FCurrentChar].Count > 50 then
  begin
    TBitmap(FUndoStack[FCurrentChar][0]).Free;
    FUndoStack[FCurrentChar].Delete(0);
  end;
  while FRedoStack[FCurrentChar].Count > 0 do
  begin
    TBitmap(FRedoStack[FCurrentChar][FRedoStack[FCurrentChar].Count - 1]).Free;
    FRedoStack[FCurrentChar].Delete(FRedoStack[FCurrentChar].Count - 1);
  end;
end;

procedure TfrmMain.mnuUndoClick(Sender: TObject);
var
  Bmp, UndoBmp, RedoCopy: TBitmap;
begin
  if FUndoStack[FCurrentChar].Count = 0 then Exit;
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  RedoCopy := TBitmap.Create;
  RedoCopy.Assign(Bmp);
  FRedoStack[FCurrentChar].Add(RedoCopy);
  UndoBmp := TBitmap(FUndoStack[FCurrentChar][FUndoStack[FCurrentChar].Count - 1]);
  FUndoStack[FCurrentChar].Delete(FUndoStack[FCurrentChar].Count - 1);
  Bmp.Assign(UndoBmp);
  UndoBmp.Free;
  pnlCharEdit.Repaint;
  UpdatePreview;
  SetModified(True);
end;

procedure TfrmMain.mnuRedoClick(Sender: TObject);
var
  Bmp, RedoBmp, UndoCopy: TBitmap;
begin
  if FRedoStack[FCurrentChar].Count = 0 then Exit;
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  UndoCopy := TBitmap.Create;
  UndoCopy.Assign(Bmp);
  FUndoStack[FCurrentChar].Add(UndoCopy);
  RedoBmp := TBitmap(FRedoStack[FCurrentChar][FRedoStack[FCurrentChar].Count - 1]);
  FRedoStack[FCurrentChar].Delete(FRedoStack[FCurrentChar].Count - 1);
  Bmp.Assign(RedoBmp);
  RedoBmp.Free;
  pnlCharEdit.Repaint;
  UpdatePreview;
  SetModified(True);
end;

procedure TfrmMain.UpdateCharList;
var
  I, SelIdx: Integer;
begin
  lstChars.Items.BeginUpdate;
  try
    lstChars.Items.Clear;
    SelIdx := -1;
    for I := 0 to 255 do
    begin
      if FCharEnabled[I] then
      begin
        lstChars.Items.AddObject(IntToStr(I), TObject(PtrInt(I)));
        if I = FCurrentChar then
          SelIdx := lstChars.Items.Count - 1;
      end;
    end;
    lstChars.ItemIndex := SelIdx;
  finally
    lstChars.Items.EndUpdate;
  end;
end;

procedure TfrmMain.ApplyCharRange;
var
  I: Integer;
begin
  for I := 0 to 255 do
    FCharEnabled[I] := (I >= FCharRangeStart) and (I <= FCharRangeEnd);
  
  // Initialize bitmaps for enabled characters
  for I := 0 to 255 do
    if FCharEnabled[I] then
      EnsureCharBitmap(I);
  
  // Make sure current char is in range
  if not FCharEnabled[FCurrentChar] then
  begin
    FCurrentChar := FCharRangeStart;
    EnsureCharBitmap(FCurrentChar);
  end;
  
  UpdateCharList;
  lstCharsClick(nil);
end;

procedure TfrmMain.btnApplyRangeClick(Sender: TObject);
begin
  FCharRangeStart := spnRangeStart.Value;
  FCharRangeEnd := spnRangeEnd.Value;
  ApplyCharRange;
  StatusBar.Panels[0].Text := Format('Character range: %d to %d', [FCharRangeStart, FCharRangeEnd]);
end;

procedure TfrmMain.lstCharsDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  CC: Integer;
  S: string;
  Bmp: TBitmap;
  X, Y, PS: Integer;
  HasContent: Boolean;
begin
  if (Index < 0) or (Index >= lstChars.Items.Count) then Exit;
  CC := PtrInt(lstChars.Items.Objects[Index]);
  with lstChars.Canvas do
  begin
    if odSelected in State then Brush.Color := clHighlight else Brush.Color := clWindow;
    FillRect(ARect);
    if odSelected in State then Font.Color := clHighlightText else Font.Color := clWindowText;
    if (CC >= 32) and (CC < 127) then S := Format('%3d ''%s''', [CC, Chr(CC)])
    else if CC = 127 then S := '127 DEL'
    else S := Format('%3d #%d', [CC, CC]);
    TextOut(ARect.Left + 4, ARect.Top + 2, S);
    Bmp := FCharBitmaps[CC];
    if Bmp <> nil then
    begin
      PS := ARect.Bottom - ARect.Top - 4;
      HasContent := False;
      for Y := 0 to Bmp.Height - 1 do
        for X := 0 to Bmp.Width - 1 do
          if Bmp.Canvas.Pixels[X, Y] = clBlack then begin HasContent := True; Break; end;
      if HasContent then
        StretchDraw(Rect(ARect.Right - PS - 4, ARect.Top + 2, ARect.Right - 4, ARect.Bottom - 2), Bmp)
      else begin Font.Color := clGray; TextOut(ARect.Right - 45, ARect.Top + 2, '(empty)'); end;
    end;
  end;
end;

procedure TfrmMain.lstCharsClick(Sender: TObject);
begin
  if (lstChars.ItemIndex >= 0) and (lstChars.ItemIndex < lstChars.Items.Count) then
  begin
    FCurrentChar := PtrInt(lstChars.Items.Objects[lstChars.ItemIndex]);
    EnsureCharBitmap(FCurrentChar);
    if (FCurrentChar >= 32) and (FCurrentChar < 127) then
      lblCurrentChar.Caption := Format('Editing: %d ''%s''', [FCurrentChar, Chr(FCurrentChar)])
    else
      lblCurrentChar.Caption := Format('Editing: %d', [FCurrentChar]);
    spnCharWidth.Value := FCharBitmaps[FCurrentChar].Width;
    UpdateEditorSize;
    pnlCharEdit.Repaint;
    UpdatePreview;
    UpdateStatus;
  end;
end;

procedure TfrmMain.UpdateEditorSize;
var
  Bmp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp <> nil then
  begin
    pnlCharEdit.Width := Bmp.Width * FPixelSize + 1;
    pnlCharEdit.Height := Bmp.Height * FPixelSize + 1;
  end;
end;

procedure TfrmMain.cmbZoomChange(Sender: TObject);
begin
  if FInitializing then Exit;
  case cmbZoom.ItemIndex of
    0: FPixelSize := 8;  1: FPixelSize := 12; 2: FPixelSize := 16;
    3: FPixelSize := 20; 4: FPixelSize := 24; 5: FPixelSize := 32;
  else FPixelSize := 16;
  end;
  UpdateEditorSize;
  pnlCharEdit.Repaint;
end;


procedure TfrmMain.cboShowGridChange(Sender: TObject);
begin
  FShowGrid := cboShowGrid.Checked;
  pnlCharEdit.Repaint;
end;

procedure TfrmMain.cboShowBaselineChange(Sender: TObject);
begin
  FShowBaseline := cboShowBaseline.Checked;
  pnlCharEdit.Repaint;
end;

procedure TfrmMain.pnlCharEditPaint(Sender: TObject);
var
  X, Y, CX, CY, GW, GH: Integer;
  Bmp: TBitmap;
  PB: TPaintBox;
  BlackCount: Integer;

begin
  PB := TPaintBox(Sender);
  Bmp := FCharBitmaps[FCurrentChar];
  
  // Always draw yellow first to confirm paint is firing
  PB.Canvas.Brush.Color := clYellow;
  PB.Canvas.FillRect(0, 0, PB.Width, PB.Height);
  
  if Bmp = nil then 
  begin
    PB.Canvas.Font.Color := clBlack;
    PB.Canvas.TextOut(5, PB.Height - 20, 'ERROR: Bmp is nil for char ' + IntToStr(FCurrentChar));
    Exit;
  end;
  
  // Count black pixels
  BlackCount := 0;
  for Y := 0 to Bmp.Height - 1 do
    for X := 0 to Bmp.Width - 1 do
      if Bmp.Canvas.Pixels[X, Y] = clBlack then Inc(BlackCount);
  
  GW := Bmp.Width * FPixelSize;
  GH := Bmp.Height * FPixelSize;
  
  // Draw pixels
  for Y := 0 to Bmp.Height - 1 do
    for X := 0 to Bmp.Width - 1 do
    begin
      CX := X * FPixelSize; 
      CY := Y * FPixelSize;
      if Bmp.Canvas.Pixels[X, Y] = clBlack then 
        PB.Canvas.Brush.Color := clBlack 
      else 
        PB.Canvas.Brush.Color := clWhite;
      PB.Canvas.FillRect(CX, CY, CX + FPixelSize, CY + FPixelSize);
    end;
    
  if FShowGrid then
  begin
    PB.Canvas.Pen.Color := clSilver; 
    PB.Canvas.Pen.Style := psSolid;
    for X := 0 to Bmp.Width do 
    begin 
      PB.Canvas.MoveTo(X * FPixelSize, 0); 
      PB.Canvas.LineTo(X * FPixelSize, GH); 
    end;
    for Y := 0 to Bmp.Height do 
    begin 
      PB.Canvas.MoveTo(0, Y * FPixelSize); 
      PB.Canvas.LineTo(GW, Y * FPixelSize); 
    end;
  end;
  
  // Line markers
  PB.Canvas.Pen.Style := psSolid;
  PB.Canvas.Pen.Width := 2;
  
  // Ascender line (Cyan)
  if FShowAscenderLine then
  begin
    PB.Canvas.Pen.Color := clTeal;
    if FAscenderLine >= 0 then
      Y := FAscenderLine
    else
      Y := 0;
    PB.Canvas.MoveTo(0, Y * FPixelSize);
    PB.Canvas.LineTo(GW, Y * FPixelSize);
  end;
  
  // X-Height line (Purple)
  if FShowXHeightLine then
  begin
    PB.Canvas.Pen.Color := clPurple;
    if FXHeightLine >= 0 then
      Y := FXHeightLine
    else
      Y := Round(spnAscent.Value * 0.6);
    PB.Canvas.MoveTo(0, Y * FPixelSize);
    PB.Canvas.LineTo(GW, Y * FPixelSize);
  end;
  
  // Baseline (Red)
  if FShowBaseline then
  begin
    PB.Canvas.Pen.Color := clRed;
    Y := spnAscent.Value;
    PB.Canvas.MoveTo(0, Y * FPixelSize); 
    PB.Canvas.LineTo(GW, Y * FPixelSize);
  end;
  
  // Descender line (Orange)
  if FShowDescenderLine then
  begin
    PB.Canvas.Pen.Color := $0080FF;  // Orange
    if FDescenderLine >= 0 then
      Y := FDescenderLine
    else
      Y := Bmp.Height;
    PB.Canvas.MoveTo(0, Y * FPixelSize);
    PB.Canvas.LineTo(GW, Y * FPixelSize);
  end;


  //Draw Frame
  PB.Canvas.Pen.Width := 1;
  PB.Canvas.Pen.Color := clSilver;
  PB.Canvas.Brush.Style := bsClear;
  PB.Canvas.Rectangle(0, 0, PB.Width, PB.Height);
end;

function TfrmMain.GetPixelAt(SX, SY: Integer; out PX, PY: Integer): Boolean;
var
  Bmp: TBitmap;
begin
  Result := False;
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  PX := SX div FPixelSize;
  PY := SY div FPixelSize;
  Result := (PX >= 0) and (PX < Bmp.Width) and (PY >= 0) and (PY < Bmp.Height);
end;

procedure TfrmMain.SetPixel(X, Y: Integer; Value: Boolean);
var
  Bmp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  if (X >= 0) and (X < Bmp.Width) and (Y >= 0) and (Y < Bmp.Height) then
  begin
    if Value then Bmp.Canvas.Pixels[X, Y] := clBlack else Bmp.Canvas.Pixels[X, Y] := clWhite;
    pnlCharEdit.Repaint;
    tmrPreview.Enabled := True;
    SetModified(True);
  end;
end;

procedure TfrmMain.pnlCharEditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PX, PY: Integer;
  Bmp: TBitmap;
begin
  if GetPixelAt(X, Y, PX, PY) then
  begin
    SaveUndoState;
    Bmp := FCharBitmaps[FCurrentChar];
    if Button = mbLeft then begin FDrawColor := Bmp.Canvas.Pixels[PX, PY] <> clBlack; SetPixel(PX, PY, FDrawColor); end
    else if Button = mbRight then begin FDrawColor := False; SetPixel(PX, PY, False); end;
    FDrawing := True;
  end;
end;

procedure TfrmMain.pnlCharEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  PX, PY: Integer;
begin
  if GetPixelAt(X, Y, PX, PY) then StatusBar.Panels[1].Text := Format('Pixel: %d, %d', [PX, PY])
  else StatusBar.Panels[1].Text := '';
  if FDrawing then
  begin
    if (ssLeft in Shift) and GetPixelAt(X, Y, PX, PY) then SetPixel(PX, PY, FDrawColor)
    else if (ssRight in Shift) and GetPixelAt(X, Y, PX, PY) then SetPixel(PX, PY, False)
    else if not ((ssLeft in Shift) or (ssRight in Shift)) then FDrawing := False;
  end;
end;

procedure TfrmMain.pnlCharEditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDrawing := False;
  UpdatePreview;
  lstChars.Invalidate;
end;

procedure TfrmMain.tmrPreviewTimer(Sender: TObject);
begin
  tmrPreview.Enabled := False;
  UpdatePreview;
  lstChars.Invalidate;
end;

procedure TfrmMain.edtPreviewTextChange(Sender: TObject);
begin
  UpdatePreview;
end;

procedure TfrmMain.pnlPreviewPaint(Sender: TObject);
var
  I, X, Y, CharX, Scale: Integer;
  Bmp: TBitmap;
  PB: TPaintBox;
begin
  PB := TPaintBox(Sender);
  Scale := 2;
  CharX := 8;
  with PB.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(0, 0, PB.Width, PB.Height);
    for I := 1 to Length(edtPreviewText.Text) do
    begin
      Bmp := FCharBitmaps[Ord(edtPreviewText.Text[I])];
      if Bmp <> nil then
      begin
        for Y := 0 to Bmp.Height - 1 do
          for X := 0 to Bmp.Width - 1 do
            if Bmp.Canvas.Pixels[X, Y] = clBlack then
            begin
              Brush.Color := clBlack;
              FillRect(CharX + X * Scale, 8 + Y * Scale, CharX + X * Scale + Scale, 8 + Y * Scale + Scale);
            end;
        CharX := CharX + (Bmp.Width + 1) * Scale;
      end
      else CharX := CharX + 8 * Scale;
      if CharX > PB.Width - 20 then Break;
    end;
    if FShowBaseline then
    begin
      Pen.Color := clRed; Pen.Style := psDot;
      MoveTo(0, 8 + spnAscent.Value * Scale);
      LineTo(PB.Width, 8 + spnAscent.Value * Scale);
    end;
  end;
end;

procedure TfrmMain.UpdatePreview;
begin
  pnlPreview.Invalidate;
end;

procedure TfrmMain.spnHeightChange(Sender: TObject);
var
  I, NH: Integer;
  OldBmp, NewBmp: TBitmap;
begin
  if FInitializing then Exit;
  NH := spnHeight.Value;
  for I := 0 to 255 do
    if FCharBitmaps[I] <> nil then
    begin
      OldBmp := FCharBitmaps[I];
      if OldBmp.Height <> NH then
      begin
        NewBmp := TBitmap.Create;
        NewBmp.Width := OldBmp.Width;
        NewBmp.Height := NH;
        NewBmp.Canvas.Brush.Color := clWhite;
        NewBmp.Canvas.FillRect(0, 0, NewBmp.Width, NH);
        NewBmp.Canvas.Draw(0, 0, OldBmp);
        OldBmp.Free;
        FCharBitmaps[I] := NewBmp;
      end;
    end;
  if spnAscent.Value > NH then spnAscent.Value := NH;
  spnAscent.MaxValue := NH;
  UpdateEditorSize;
  pnlCharEdit.Repaint;
  UpdatePreview;
  SetModified(True);
end;

procedure TfrmMain.spnCharWidthChange(Sender: TObject);
begin
  if FInitializing then Exit;
  ResizeCharBitmap(FCurrentChar, spnCharWidth.Value, spnHeight.Value);
  UpdateEditorSize;
  pnlCharEdit.Repaint;
  UpdatePreview;
  lstChars.Invalidate;
  SetModified(True);
end;

procedure TfrmMain.ResizeCharBitmap(Idx, NewW, NewH: Integer);
var
  OldBmp, NewBmp: TBitmap;
begin
  if FCharBitmaps[Idx] = nil then begin EnsureCharBitmap(Idx); FCharBitmaps[Idx].Width := NewW; FCharBitmaps[Idx].Height := NewH; Exit; end;
  OldBmp := FCharBitmaps[Idx];
  if (OldBmp.Width = NewW) and (OldBmp.Height = NewH) then Exit;
  NewBmp := TBitmap.Create;
  NewBmp.Width := NewW;
  NewBmp.Height := NewH;
  NewBmp.Canvas.Brush.Color := clWhite;
  NewBmp.Canvas.FillRect(0, 0, NewW, NewH);
  NewBmp.Canvas.Draw(0, 0, OldBmp);
  OldBmp.Free;
  FCharBitmaps[Idx] := NewBmp;
end;

procedure TfrmMain.btnClearCharClick(Sender: TObject);
var
  Bmp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp <> nil then
  begin
    SaveUndoState;
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
    pnlCharEdit.Repaint;
    UpdatePreview;
    lstChars.Invalidate;
    SetModified(True);
  end;
end;

procedure TfrmMain.btnClearAllClick(Sender: TObject);
var
  I: Integer;
begin
  if MessageDlg('Clear All', 'Clear all characters?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    for I := 0 to 255 do
    begin
      ClearUndoRedo(I);
      if FCharBitmaps[I] <> nil then
      begin
        FCharBitmaps[I].Canvas.Brush.Color := clWhite;
        FCharBitmaps[I].Canvas.FillRect(0, 0, FCharBitmaps[I].Width, FCharBitmaps[I].Height);
      end;
    end;
    pnlCharEdit.Repaint;
    UpdatePreview;
    lstChars.Invalidate;
    SetModified(True);
  end;
end;

procedure TfrmMain.btnCopyCharClick(Sender: TObject);
var
  Bmp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  if FClipboardBitmap <> nil then FClipboardBitmap.Free;
  FClipboardBitmap := TBitmap.Create;
  FClipboardBitmap.Assign(Bmp);
  StatusBar.Panels[0].Text := Format('Copied char %d', [FCurrentChar]);
end;

procedure TfrmMain.btnPasteCharClick(Sender: TObject);
var
  Bmp: TBitmap;
begin
  if FClipboardBitmap = nil then begin StatusBar.Panels[0].Text := 'Nothing to paste'; Exit; end;
  EnsureCharBitmap(FCurrentChar);
  Bmp := FCharBitmaps[FCurrentChar];
  SaveUndoState;
  if (Bmp.Width <> FClipboardBitmap.Width) or (Bmp.Height <> FClipboardBitmap.Height) then
  begin
    Bmp.Width := FClipboardBitmap.Width;
    Bmp.Height := FClipboardBitmap.Height;
    spnCharWidth.Value := Bmp.Width;
  end;
  Bmp.Assign(FClipboardBitmap);
  UpdateEditorSize;
  pnlCharEdit.Repaint;
  UpdatePreview;
  lstChars.Invalidate;
  SetModified(True);
  StatusBar.Panels[0].Text := Format('Pasted to char %d', [FCurrentChar]);
end;

procedure TfrmMain.btnShiftLeftClick(Sender: TObject);
var
  Bmp, Temp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  Temp := TBitmap.Create;
  try
    Temp.Width := Bmp.Width; Temp.Height := Bmp.Height;
    Temp.Canvas.Brush.Color := clWhite;
    Temp.Canvas.FillRect(0, 0, Temp.Width, Temp.Height);
    Temp.Canvas.Draw(-1, 0, Bmp);
    Bmp.Assign(Temp);
  finally Temp.Free; end;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnShiftRightClick(Sender: TObject);
var
  Bmp, Temp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  Temp := TBitmap.Create;
  try
    Temp.Width := Bmp.Width; Temp.Height := Bmp.Height;
    Temp.Canvas.Brush.Color := clWhite;
    Temp.Canvas.FillRect(0, 0, Temp.Width, Temp.Height);
    Temp.Canvas.Draw(1, 0, Bmp);
    Bmp.Assign(Temp);
  finally Temp.Free; end;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnShiftUpClick(Sender: TObject);
var
  Bmp, Temp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  Temp := TBitmap.Create;
  try
    Temp.Width := Bmp.Width; Temp.Height := Bmp.Height;
    Temp.Canvas.Brush.Color := clWhite;
    Temp.Canvas.FillRect(0, 0, Temp.Width, Temp.Height);
    Temp.Canvas.Draw(0, -1, Bmp);
    Bmp.Assign(Temp);
  finally Temp.Free; end;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnShiftDownClick(Sender: TObject);
var
  Bmp, Temp: TBitmap;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  Temp := TBitmap.Create;
  try
    Temp.Width := Bmp.Width; Temp.Height := Bmp.Height;
    Temp.Canvas.Brush.Color := clWhite;
    Temp.Canvas.FillRect(0, 0, Temp.Width, Temp.Height);
    Temp.Canvas.Draw(0, 1, Bmp);
    Bmp.Assign(Temp);
  finally Temp.Free; end;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnInvertClick(Sender: TObject);
var
  Bmp: TBitmap;
  X, Y: Integer;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  for Y := 0 to Bmp.Height - 1 do
    for X := 0 to Bmp.Width - 1 do
      if Bmp.Canvas.Pixels[X, Y] = clBlack then Bmp.Canvas.Pixels[X, Y] := clWhite
      else Bmp.Canvas.Pixels[X, Y] := clBlack;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnFlipHClick(Sender: TObject);
var
  Bmp, Temp: TBitmap;
  X, Y: Integer;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  Temp := TBitmap.Create;
  try
    Temp.Width := Bmp.Width; Temp.Height := Bmp.Height;
    Temp.Canvas.Brush.Color := clWhite;
    Temp.Canvas.FillRect(0, 0, Temp.Width, Temp.Height);
    for Y := 0 to Bmp.Height - 1 do
      for X := 0 to Bmp.Width - 1 do
        Temp.Canvas.Pixels[Bmp.Width - 1 - X, Y] := Bmp.Canvas.Pixels[X, Y];
    Bmp.Assign(Temp);
  finally Temp.Free; end;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnFlipVClick(Sender: TObject);
var
  Bmp, Temp: TBitmap;
  X, Y: Integer;
begin
  Bmp := FCharBitmaps[FCurrentChar];
  if Bmp = nil then Exit;
  SaveUndoState;
  Temp := TBitmap.Create;
  try
    Temp.Width := Bmp.Width; Temp.Height := Bmp.Height;
    Temp.Canvas.Brush.Color := clWhite;
    Temp.Canvas.FillRect(0, 0, Temp.Width, Temp.Height);
    for Y := 0 to Bmp.Height - 1 do
      for X := 0 to Bmp.Width - 1 do
        Temp.Canvas.Pixels[X, Bmp.Height - 1 - Y] := Bmp.Canvas.Pixels[X, Y];
    Bmp.Assign(Temp);
  finally Temp.Free; end;
  pnlCharEdit.Repaint; UpdatePreview; lstChars.Invalidate; SetModified(True);
end;

procedure TfrmMain.btnLoadFontClick(Sender: TObject);
var
  Ext: string;
begin
  if not ConfirmSave then Exit;
  dlgOpen.Filter := 'All Font Files|*.FON;*.FNT;*.ROM|Windows FON Files (*.FON)|*.FON|DOS BIOS Fonts (*.FNT;*.ROM)|*.FNT;*.ROM|All Files (*.*)|*.*';
  if dlgOpen.Execute then
  begin
    Ext := LowerCase(ExtractFileExt(dlgOpen.FileName));
    if (Ext = '.fnt') or (Ext = '.rom') then
      LoadFNTFile(dlgOpen.FileName)
    else
      LoadFONFile(dlgOpen.FileName);
  end;
end;

procedure TfrmMain.LoadFNTFile(const FN: string);
var
  FS: TFileStream;
  FileSize: Int64;
  CharHeight, CharCount, BytesPerChar: Integer;
  I, X, Y : Integer;
  CharData: array of Byte;
  Bmp: TBitmap;
begin
  FS := TFileStream.Create(FN, fmOpenRead or fmShareDenyWrite);
  try
    FileSize := FS.Size;
    
    // Determine character height from file size
    // FNT files contain 256 characters, each character is Height bytes (1 bit per pixel, 8 pixels wide)
    if FileSize mod 256 <> 0 then
    begin
      ShowMessage('Invalid FNT file size. Must be 256 * character_height bytes.');
      Exit;
    end;
    
    CharHeight := FileSize div 256;
    CharCount := 256;
    BytesPerChar := CharHeight;  // 8 pixels wide = 1 byte per row
    //BytesPerRow := 1;
    
    // Clear existing data
    for I := 0 to 255 do
    begin
      ClearUndoRedo(I);
      if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end;
    end;
    
    // Set font properties
    edtFontName.Text := ChangeFileExt(ExtractFileName(FN), '');
    spnHeight.Value := CharHeight;
    spnAscent.Value := CharHeight - 2;  // Reasonable default
    
    // Update character range to 0-255 for FNT files
    FCharRangeStart := 0;
    FCharRangeEnd := 255;
    spnRangeStart.Value := 0;
    spnRangeEnd.Value := 255;
    for I := 0 to 255 do
      FCharEnabled[I] := True;
    
    // Read character data
    SetLength(CharData, BytesPerChar);
    
    for I := 0 to CharCount - 1 do
    begin
      FS.ReadBuffer(CharData[0], BytesPerChar);
      
      Bmp := TBitmap.Create;
      Bmp.Width := 8;
      Bmp.Height := CharHeight;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.FillRect(0, 0, 8, CharHeight);
      
      // Convert bits to pixels
      for Y := 0 to CharHeight - 1 do
      begin
        for X := 0 to 7 do
        begin
          // Bit 7 is leftmost pixel, bit 0 is rightmost
          if (CharData[Y] and ($80 shr X)) <> 0 then
            Bmp.Canvas.Pixels[X, Y] := clBlack;
        end;
      end;
      
      FCharBitmaps[I] := Bmp;
    end;
    
    FCurrentFile := FN;
    FCurrentChar := 65;
    UpdateCharList;
    lstCharsClick(nil);
    UpdateStatus;
    SetModified(False);
    StatusBar.Panels[0].Text := Format('Loaded FNT: %s (%dx%d)', [ExtractFileName(FN), 8, CharHeight]);
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.SaveFNTFile(const FN: string);
var
  FS: TFileStream;
  I, X, Y, CharHeight: Integer;
  Bmp: TBitmap;
  RowByte: Byte;
begin
  CharHeight := spnHeight.Value;
  
  FS := TFileStream.Create(FN, fmCreate);
  try
    // Write all 256 characters
    for I := 0 to 255 do
    begin
      Bmp := FCharBitmaps[I];
      
      for Y := 0 to CharHeight - 1 do
      begin
        RowByte := 0;
        if Bmp <> nil then
        begin
          for X := 0 to 7 do
          begin
            if (X < Bmp.Width) and (Y < Bmp.Height) then
            begin
              if Bmp.Canvas.Pixels[X, Y] = clBlack then
                RowByte := RowByte or ($80 shr X);
            end;
          end;
        end;
        FS.WriteByte(RowByte);
      end;
    end;
    
    FCurrentFile := FN;
    SetModified(False);
    StatusBar.Panels[0].Text := 'Saved FNT: ' + ExtractFileName(FN);
  finally
    FS.Free;
  end;
end;

procedure TfrmMain.LoadFONFile(const FN: string);
var
  FONFont: TWinFont;
  I, CW, CH: Integer;
  Bmp: TBitmap;
begin
  FONFont := TWinFont.Create;
  try
    if not FONFont.LoadFromFile(FN) then begin ShowMessage('Failed to load: ' + FN); Exit; end;
    if FONFont.FontType <> ftRaster then begin ShowMessage('Only bitmap fonts supported'); Exit; end;
    
    for I := 0 to 255 do
    begin
      ClearUndoRedo(I);
      if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end;
    end;
    
    edtFontName.Text := FONFont.FontName;
    spnHeight.Value := FONFont.Height;
    spnAscent.Value := FONFont.Ascent;
    CH := FONFont.Height;
    
    for I := FONFont.FirstChar to FONFont.LastChar do
    begin
      CW := FONFont.GetCharWidth(I);
      if CW <= 0 then Continue;
      Bmp := TBitmap.Create;
      Bmp.Width := CW;
      Bmp.Height := CH;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.FillRect(0, 0, CW, CH);
      if FONFont.GetGlyphBitmap(I, Bmp) then
        FCharBitmaps[I] := Bmp
      else
        Bmp.Free;
    end;
    
    FCurrentFile := FN;
    FCurrentChar := FONFont.FirstChar;
    if FCurrentChar < 32 then FCurrentChar := 32;
    UpdateCharList;
    lstCharsClick(nil);
    UpdateStatus;
    SetModified(False);
    StatusBar.Panels[0].Text := 'Loaded: ' + ExtractFileName(FN);
  finally
    FONFont.Free;
  end;
end;

procedure TfrmMain.btnSaveFontClick(Sender: TObject);
var
  I: Integer;
  PF: TFontPitchFamily;
  CS: TFontCharSet;
  Ext: string;
begin
  dlgSave.Filter := 'Windows FON Files (*.FON)|*.FON|DOS BIOS Fonts (*.FNT)|*.FNT|ROM Fonts (*.ROM)|*.ROM|All Files (*.*)|*.*';
  dlgSave.FileName := edtFontName.Text + '.FON';
  if dlgSave.Execute then
  begin
    Ext := LowerCase(ExtractFileExt(dlgSave.FileName));
    
    if (Ext = '.fnt') or (Ext = '.rom') then
    begin
      // Save as FNT/ROM format
      try
        SaveFNTFile(dlgSave.FileName);
      except
        on E: Exception do ShowMessage('Error saving FNT: ' + E.Message);
      end;
    end
    else
    begin
      // Save as FON format
      try
        FCreator.ClearAll;
        FCreator.FontName := edtFontName.Text;
        FCreator.Copyright := edtCopyright.Text;
        FCreator.PointSize := spnPointSize.Value;
        FCreator.Height := spnHeight.Value;
        FCreator.Ascent := spnAscent.Value;
        if cboBold.Checked then FCreator.Weight := fwBold else FCreator.Weight := fwNormal;
        FCreator.Italic := cboItalic.Checked;
        FCreator.Underline := cboUnderline.Checked;
        case cmbCharSet.ItemIndex of
          0: CS := csANSI; 1: CS := csDefault; 2: CS := csSymbol; 3: CS := csOEM;
        else CS := csANSI; end;
        FCreator.CharSet := CS;
        case cmbPitchFamily.ItemIndex of
          0: PF := pfDefault; 1: PF := pfFixed; 2: PF := pfVariable;
          3: PF := pfRoman; 4: PF := pfSwiss; 5: PF := pfModern;
          6: PF := pfScript; 7: PF := pfDecorative;
        else PF := pfDefault; end;
        FCreator.PitchFamily := PF;
        for I := 0 to 255 do if FCharBitmaps[I] <> nil then FCreator.SetCharacter(I, FCharBitmaps[I]);
        FCreator.SaveToFile(dlgSave.FileName);
        FCurrentFile := dlgSave.FileName;
        SetModified(False);
        UpdateStatus;
        StatusBar.Panels[0].Text := 'Saved: ' + ExtractFileName(dlgSave.FileName);
      except
        on E: Exception do ShowMessage('Error: ' + E.Message);
      end;
    end;
  end;
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
var
  I: Integer;
begin
  if not ConfirmSave then Exit;
  for I := 0 to 255 do begin ClearUndoRedo(I); if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end; end;
  edtFontName.Text := 'MyFont';
  edtCopyright.Text := 'Created with FON Creator';
  spnPointSize.Value := 10;
  spnHeight.Value := 12;
  spnAscent.Value := 10;
  for I := 32 to 127 do EnsureCharBitmap(I);
  FCurrentFile := '';
  FCurrentChar := 65;
  UpdateCharList;
  lstCharsClick(nil);
  UpdateStatus;
  SetModified(False);
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuGenerateFromFontClick(Sender: TObject);
var
  FontDlg: TFontDialog;
  TempBmp: TBitmap;
  I, TW, TH: Integer;
begin
  FontDlg := TFontDialog.Create(nil);
  try
    FontDlg.Font.Name := 'Arial';
    FontDlg.Font.Size := 10;
    if FontDlg.Execute then
    begin
      if not ConfirmSave then Exit;
      for I := 0 to 255 do begin ClearUndoRedo(I); if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end; end;
      TempBmp := TBitmap.Create;
      try
        TempBmp.Canvas.Font.Assign(FontDlg.Font);
        TempBmp.Canvas.Font.Quality := fqNonAntialiased;
        TH := TempBmp.Canvas.TextHeight('Wj|g');
        spnHeight.Value := TH;
        spnAscent.Value := TH - 3;
        for I := 32 to 255 do
        begin
          TW := TempBmp.Canvas.TextWidth(Chr(I));
          if TW < 1 then TW := 4;
          FCharBitmaps[I] := TBitmap.Create;
          FCharBitmaps[I].Width := TW;
          FCharBitmaps[I].Height := TH;
          FCharBitmaps[I].Canvas.Brush.Color := clWhite;
          FCharBitmaps[I].Canvas.FillRect(0, 0, TW, TH);
          FCharBitmaps[I].Canvas.Font.Assign(FontDlg.Font);
          FCharBitmaps[I].Canvas.Font.Quality := fqNonAntialiased;
          FCharBitmaps[I].Canvas.Font.Color := clBlack;
          FCharBitmaps[I].Canvas.TextOut(0, 0, Chr(I));
        end;
        edtFontName.Text := FontDlg.Font.Name;
        FCurrentFile := '';
        FCurrentChar := 65;
        UpdateCharList;
        lstCharsClick(nil);
        UpdateStatus;
        SetModified(True);
        StatusBar.Panels[0].Text := 'Generated from ' + FontDlg.Font.Name;
      finally TempBmp.Free; end;
    end;
  finally FontDlg.Free; end;
end;

procedure TfrmMain.mnuFontMetricsClick(Sender: TObject);
var
  I, Cnt, MinW, MaxW, TotW: Integer;
begin
  Cnt := 0; MinW := MaxInt; MaxW := 0; TotW := 0;
  for I := 0 to 255 do
    if FCharBitmaps[I] <> nil then
    begin
      Inc(Cnt);
      if FCharBitmaps[I].Width < MinW then MinW := FCharBitmaps[I].Width;
      if FCharBitmaps[I].Width > MaxW then MaxW := FCharBitmaps[I].Width;
      TotW := TotW + FCharBitmaps[I].Width;
    end;
  if Cnt = 0 then begin ShowMessage('No characters defined.'); Exit; end;
  ShowMessage(Format('Font: %s'#13'Points: %d'#13'Height: %d'#13'Ascent: %d'#13#13'Chars: %d'#13'Min Width: %d'#13'Max Width: %d'#13'Avg Width: %.1f',
    [edtFontName.Text, spnPointSize.Value, spnHeight.Value, spnAscent.Value, Cnt, MinW, MaxW, TotW / Cnt]));
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  ShowMessage(ProgramName+#13#13'Create and edit Windows FON bitmap fonts.'#13#13'Features:'#13'- Load/save FON files'#13'- Generate from system fonts'#13'- Undo/Redo support'#13'- Character transformations'#13#13'Built with Lazarus/Free Pascal');
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      VK_Z: if ssShift in Shift then mnuRedoClick(nil) else mnuUndoClick(nil);
      VK_Y: mnuRedoClick(nil);
      VK_C: btnCopyCharClick(nil);
      VK_V: btnPasteCharClick(nil);
      VK_S: btnSaveFontClick(nil);
      VK_O: btnLoadFontClick(nil);
      VK_N: mnuNewClick(nil);
    end
  else if ssShift in Shift then
    case Key of
      VK_LEFT: btnShiftLeftClick(nil);
      VK_RIGHT: btnShiftRightClick(nil);
      VK_UP: btnShiftUpClick(nil);
      VK_DOWN: btnShiftDownClick(nil);
    end;
end;

procedure TfrmMain.UpdateStatus;
var
  Cnt, I: Integer;
  CharInfo: string;
begin
  Cnt := 0;
  for I := 0 to 255 do if FCharBitmaps[I] <> nil then Inc(Cnt);
  
  // Panel 0: Current character info
  if (FCurrentChar >= 0) and (FCurrentChar <= 255) then
  begin
    if FCharBitmaps[FCurrentChar] <> nil then
      CharInfo := Format('Char %d - %dx%d', [FCurrentChar, 
        FCharBitmaps[FCurrentChar].Width, FCharBitmaps[FCurrentChar].Height])
    else
      CharInfo := Format('Char %d - Empty', [FCurrentChar]);
    StatusBar.Panels[0].Text := CharInfo;
  end;
  
  StatusBar.Panels[2].Text := Format('Characters: %d', [Cnt]);
end;

procedure TfrmMain.chkLineMarkerChange(Sender: TObject);
begin
  FShowAscenderLine := chkShowAscender.Checked;
  FShowDescenderLine := chkShowDescender.Checked;
  FShowXHeightLine := chkShowXHeight.Checked;
  pnlCharEdit.Repaint;
end;

procedure TfrmMain.spnLineMarkerChange(Sender: TObject);
begin
  if FInitializing then Exit;
  FAscenderLine := spnAscenderLine.Value;
  FDescenderLine := spnDescenderLine.Value;
  FXHeightLine := spnXHeightLine.Value;
  UpdateLineMarkerDisplay;
  pnlCharEdit.Repaint;
end;

procedure TfrmMain.btnResetLinesClick(Sender: TObject);
begin
  spnAscenderLine.Value := -1;
  spnDescenderLine.Value := -1;
  spnXHeightLine.Value := -1;
  FAscenderLine := -1;
  FDescenderLine := -1;
  FXHeightLine := -1;
  UpdateLineMarkerDisplay;
  pnlCharEdit.Repaint;
end;

procedure TfrmMain.ScanFontLines(out AscenderY, DescenderY, XHeightY: Integer);
var
  I, X, Y: Integer;
  Bmp: TBitmap;
  AscenderChars: set of Char;
  XHeightChars: set of Char;
  DescenderChars: set of Char;
  C: Char;
  AscMinY, XHMinY, DescMaxY: Integer;
  FoundAsc, FoundXH, FoundDesc: Boolean;
begin
  // Characters used to determine each line
  AscenderChars := ['b', 'd', 'f', 'h', 'k', 'l', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 
                    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 
                    'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
  XHeightChars := ['a', 'c', 'e', 'm', 'n', 'o', 'r', 's', 'u', 'v', 'w', 'x', 'z'];
  DescenderChars := ['g', 'j', 'p', 'q', 'y'];
  
  AscMinY := MaxInt;
  XHMinY := MaxInt;
  DescMaxY := 0;
  FoundAsc := False;
  FoundXH := False;
  FoundDesc := False;
  
  // Scan ascender characters for minimum Y (top)
  for C in AscenderChars do
  begin
    I := Ord(C);
    Bmp := FCharBitmaps[I];
    if Bmp <> nil then
    begin
      for Y := 0 to Bmp.Height - 1 do
        for X := 0 to Bmp.Width - 1 do
          if Bmp.Canvas.Pixels[X, Y] = clBlack then
          begin
            if Y < AscMinY then AscMinY := Y;
            FoundAsc := True;
          end;
    end;
  end;
  
  // Scan x-height characters for minimum Y (top of lowercase)
  for C in XHeightChars do
  begin
    I := Ord(C);
    Bmp := FCharBitmaps[I];
    if Bmp <> nil then
    begin
      for Y := 0 to Bmp.Height - 1 do
        for X := 0 to Bmp.Width - 1 do
          if Bmp.Canvas.Pixels[X, Y] = clBlack then
          begin
            if Y < XHMinY then XHMinY := Y;
            FoundXH := True;
          end;
    end;
  end;
  
  // Scan descender characters for maximum Y (bottom)
  for C in DescenderChars do
  begin
    I := Ord(C);
    Bmp := FCharBitmaps[I];
    if Bmp <> nil then
    begin
      for Y := 0 to Bmp.Height - 1 do
        for X := 0 to Bmp.Width - 1 do
          if Bmp.Canvas.Pixels[X, Y] = clBlack then
          begin
            if Y > DescMaxY then DescMaxY := Y;
            FoundDesc := True;
          end;
    end;
  end;
  
  // Set results
  if FoundAsc then AscenderY := AscMinY else AscenderY := 0;
  if FoundXH then XHeightY := XHMinY else XHeightY := Round(spnAscent.Value * 0.6);
  if FoundDesc then DescenderY := DescMaxY + 1 else DescenderY := spnHeight.Value;
end;

procedure TfrmMain.btnScanLinesClick(Sender: TObject);
var
  AscY, DescY, XHY: Integer;
begin
  ScanFontLines(AscY, DescY, XHY);
  
  spnAscenderLine.Value := AscY;
  spnDescenderLine.Value := DescY;
  spnXHeightLine.Value := XHY;
  
  FAscenderLine := AscY;
  FDescenderLine := DescY;
  FXHeightLine := XHY;
  
  UpdateLineMarkerDisplay;
  pnlCharEdit.Repaint;
  
  ShowMessage(Format('Scan complete:' + LineEnding +
    'Ascender: %d' + LineEnding +
    'X-Height: %d' + LineEnding +
    'Baseline: %d' + LineEnding +
    'Descender: %d',
    [AscY, XHY, spnAscent.Value, DescY]));
end;



procedure TfrmMain.UpdateLineMarkerDisplay;
var
  AscVal, DescVal, XHVal, BaseVal: Integer;
begin
  BaseVal := spnAscent.Value;
  
  if FAscenderLine >= 0 then
    AscVal := FAscenderLine
  else
    AscVal := 0;
    
  if FDescenderLine >= 0 then
    DescVal := FDescenderLine
  else
    DescVal := spnHeight.Value;
    
  if FXHeightLine >= 0 then
    XHVal := FXHeightLine
  else
    XHVal := Round(spnAscent.Value * 0.6);
  
  lblLineValues.Caption := Format(
    'Current Values:' + LineEnding +
    'Baseline: %d'+LineEnding+'Ascender: %d ' + LineEnding +
    'Descencer: %d'+LineEnding+'X-Height: %d',
    [BaseVal,AscVal, DescVal, XHVal]);
end;

end.
