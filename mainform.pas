unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ComCtrls, Menus, LCLType, FONCreator, WinFont, DPFont, TEGLFont, AmigaFont;
const
  ProgramName = 'RetroNick'#39's Bitmap Font Editor v1.1';
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
    mnuSep5: TMenuItem;
    mnuIncreaseWidth: TMenuItem;
    mnuDecreaseWidth: TMenuItem;
    mnuSep6: TMenuItem;
    mnuShiftLeft: TMenuItem;
    mnuShiftRight: TMenuItem;
    mnuSep7: TMenuItem;
    mnuAutoTrim: TMenuItem;
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
    procedure mnuIncreaseWidthClick(Sender: TObject);
    procedure mnuShiftLeftClick(Sender: TObject);
    procedure mnuShiftRightClick(Sender: TObject);
    procedure mnuDecreaseWidthClick(Sender: TObject);
    procedure mnuAutoTrimClick(Sender: TObject);
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
    
    // Remember last dialog filter
    FLastOpenFilterIndex: Integer;
    FLastSaveFilterIndex: Integer;
    
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
    procedure LoadDPFontFile(const FN: string);
    procedure SaveDPFontFile(const FN: string);
    procedure LoadTEGLFontFile(const FN: string);
    procedure SaveTEGLFontFile(const FN: string);
    procedure LoadAmigaFontFile(const FN: string);
    procedure SaveAmigaFontFile(const FN: string);
    procedure AutoLoadFontFile(const FN: string);
    function TryLoadTEGLFont(const FN: string): Boolean;
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
  
  // Default filter indices
  FLastOpenFilterIndex := 1;
  FLastSaveFilterIndex := 1;
  
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
  edtCopyright.Text := 'RetroNicks Bitmap Font Editor';
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
begin
  if not ConfirmSave then Exit;
  // Filter indices: 1=All, 2=FON, 3=FNT/ROM, 4=Deluxe Paint, 5=TEGL, 6=Amiga, 7=All Files
  dlgOpen.Filter := 'All Font Files|*.FON;*.FNT;*.ROM;*.M*;*.PCL;*.RTF|' +
                    'Windows FON Files (*.FON)|*.FON|' +
                    'DOS BIOS Fonts (*.FNT;*.ROM)|*.FNT;*.ROM|' +
                    'Deluxe Paint Fonts (*.M*;*.PCL)|*.M*;*.PCL|' +
                    'TEGL Fonts (*.RTF;*.FNT)|*.RTF;*.FNT|' +
                    'Amiga Fonts (*.*)|*.*|' +
                    'All Files (*.*)|*.*';
  dlgOpen.FilterIndex := FLastOpenFilterIndex;
  
  if dlgOpen.Execute then
  begin
    FLastOpenFilterIndex := dlgOpen.FilterIndex;
    case dlgOpen.FilterIndex of
      1: // All Font Files - try to auto-detect by extension
        begin
          AutoLoadFontFile(dlgOpen.FileName);
        end;
      2: // Windows FON
        LoadFONFile(dlgOpen.FileName);
      3: // DOS BIOS FNT/ROM
        LoadFNTFile(dlgOpen.FileName);
      4: // Deluxe Paint
        LoadDPFontFile(dlgOpen.FileName);
      5: // TEGL
        LoadTEGLFontFile(dlgOpen.FileName);
      6: // Amiga
        LoadAmigaFontFile(dlgOpen.FileName);
      7: // All Files - try to auto-detect
        AutoLoadFontFile(dlgOpen.FileName);
    end;
  end;
end;

procedure TfrmMain.AutoLoadFontFile(const FN: string);
var
  Ext: string;
begin
  // Fall back to extension-based detection
  Ext := LowerCase(ExtractFileExt(FN));
  
  if Ext = '.fon' then
    LoadFONFile(FN)
  else if (Ext = '.rom') then
    LoadFNTFile(FN)
  else if ((Length(Ext) >= 2) and (Ext[2] = 'm')) or (Ext = '.pcl') then
    LoadDPFontFile(FN)
  else if Ext = '.rtf' then
    LoadTEGLFontFile(FN)
  else if Ext = '.fnt' then
    // For .fnt, try TEGL first (has signature), then fall back to raw FNT
    begin
      if not TryLoadTEGLFont(FN) then
        LoadFNTFile(FN);
    end
  else
    // Unknown - try FON format
    LoadFONFile(FN);
end;

function TfrmMain.TryLoadTEGLFont(const FN: string): Boolean;
var
  FS: TFileStream;
  Sig: Word;
begin
  Result := False;
  if not FileExists(FN) then Exit;
  
  try
    FS := TFileStream.Create(FN, fmOpenRead or fmShareDenyWrite);
    try
      if FS.Size < 10 then Exit;
      FS.ReadBuffer(Sig, 2);
      // TEGL signature is 'TR' = $54 $52, reads as $5254 little-endian
      if Sig = $5254 then
      begin
        FS.Free;
        FS := nil;
        LoadTEGLFontFile(FN);
        Result := True;
      end;
    finally
      if FS <> nil then FS.Free;
    end;
  except
    Result := False;
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

{ Deluxe Paint IIe Font Format Support }

procedure TfrmMain.LoadDPFontFile(const FN: string);
var
  DPFnt: TDPFont;
  I, CW, CH: Integer;
  Bmp: TBitmap;
  MaxCellHeight: Integer;
  GlyphTop, GlyphBottom: Integer;
  TopOffset, GlyphH: Integer;
begin
  DPFnt := TDPFont.Create;
  try
    if not DPFnt.LoadFromFile(FN) then
    begin
      ShowMessage('Failed to load Deluxe Paint font: ' + FN);
      Exit;
    end;
    
    // Clear existing data
    for I := 0 to 255 do
    begin
      ClearUndoRedo(I);
      if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end;
    end;
    
    edtFontName.Text := DPFnt.FontName;
    
    // Calculate actual cell height needed to contain all glyphs
    // Each glyph has TopOffset (from baseline to top of glyph)
    // and Height. We need to find the max extent above and below baseline.
    MaxCellHeight := DPFnt.CellHeight;
    
    // Scan all glyphs to find actual required cell height
    for I := 0 to 255 do
    begin
      CW := DPFnt.GetCharWidth(I);
      if CW <= 0 then Continue;
      
      TopOffset := DPFnt.GetGlyphTopOffset(I);
      GlyphH := DPFnt.GetGlyphHeight(I);
      
      // TopOffset = distance from baseline to top of glyph (positive = above baseline)
      // GlyphTop in cell = Baseline - TopOffset
      // GlyphBottom in cell = GlyphTop + GlyphH
      GlyphTop := DPFnt.Baseline - TopOffset;
      GlyphBottom := GlyphTop + GlyphH;
      
      if GlyphBottom > MaxCellHeight then
        MaxCellHeight := GlyphBottom;
      if GlyphTop < 0 then
      begin
        // Glyph extends above cell - adjust baseline would be needed
        // For now, just note this
      end;
    end;
    
    CH := MaxCellHeight;
    spnHeight.Value := CH;
    spnAscent.Value := DPFnt.Baseline;
    
    // Load all glyphs using cell-aware function
    for I := 0 to 255 do
    begin
      CW := DPFnt.GetCharWidth(I);
      if CW <= 0 then Continue;
      
      Bmp := TBitmap.Create;
      
      if DPFnt.GetGlyphBitmapInCell(I, Bmp, CH) then
        FCharBitmaps[I] := Bmp
      else
        Bmp.Free;
    end;
    
    // Update character range
    FCharRangeStart := 32;
    FCharRangeEnd := 127;
    for I := 0 to 255 do
      FCharEnabled[I] := FCharBitmaps[I] <> nil;
    
    FCurrentFile := FN;
    FCurrentChar := 65;
    UpdateCharList;
    lstCharsClick(nil);
    UpdateStatus;
    SetModified(False);
    StatusBar.Panels[0].Text := Format('Loaded DP font: %s (baseline=%d, height=%d)', 
      [ExtractFileName(FN), DPFnt.Baseline, CH]);
  finally
    DPFnt.Free;
  end;
end;

procedure TfrmMain.SaveDPFontFile(const FN: string);
var
  DPFnt: TDPFont;
  I: Integer;
begin
  DPFnt := TDPFont.Create;
  try
    DPFnt.FontName := edtFontName.Text;
    DPFnt.CellHeight := spnHeight.Value;
    DPFnt.CellWidth := 8;  // Default
    DPFnt.Baseline := spnAscent.Value;
    
    // Find max width
    for I := 0 to 255 do
      if FCharBitmaps[I] <> nil then
        if FCharBitmaps[I].Width > DPFnt.CellWidth then
          DPFnt.CellWidth := FCharBitmaps[I].Width;
    
    // Add all characters
    for I := 0 to 255 do
      if FCharBitmaps[I] <> nil then
        DPFnt.SetGlyphFromBitmap(I, FCharBitmaps[I], FCharBitmaps[I].Width * 4);
    
    if DPFnt.SaveToFile(FN) then
    begin
      FCurrentFile := FN;
      SetModified(False);
      StatusBar.Panels[0].Text := 'Saved DP font: ' + ExtractFileName(FN);
    end
    else
      ShowMessage('Failed to save Deluxe Paint font');
  finally
    DPFnt.Free;
  end;
end;

{ TEGL Font Format Support }

procedure TfrmMain.LoadTEGLFontFile(const FN: string);
var
  TEGLFnt: TTEGLFont;
  I, CW, CH: Integer;
  Bmp: TBitmap;
begin
  TEGLFnt := TTEGLFont.Create;
  try
    if not TEGLFnt.LoadFromFile(FN) then
    begin
      ShowMessage('Failed to load TEGL font: ' + FN);
      Exit;
    end;
    
    // Clear existing data
    for I := 0 to 255 do
    begin
      ClearUndoRedo(I);
      if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end;
    end;
    
    edtFontName.Text := TEGLFnt.FontName;
    spnHeight.Value := TEGLFnt.Height;
    spnAscent.Value := TEGLFnt.BaseLine;
    CH := TEGLFnt.Height;
    
    // Load glyphs for the defined range
    for I := TEGLFnt.RangeStart to TEGLFnt.RangeEnd do
    begin
      CW := TEGLFnt.GetCharWidth(I);
      if CW <= 0 then Continue;
      
      Bmp := TBitmap.Create;
      Bmp.Width := CW;
      Bmp.Height := CH;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.FillRect(0, 0, CW, CH);
      
      if TEGLFnt.GetGlyphBitmap(I, Bmp) then
        FCharBitmaps[I] := Bmp
      else
        Bmp.Free;
    end;
    
    // Update character range
    FCharRangeStart := TEGLFnt.RangeStart;
    FCharRangeEnd := TEGLFnt.RangeEnd;
    spnRangeStart.Value := FCharRangeStart;
    spnRangeEnd.Value := FCharRangeEnd;
    for I := 0 to 255 do
      FCharEnabled[I] := (I >= FCharRangeStart) and (I <= FCharRangeEnd);
    
    FCurrentFile := FN;
    FCurrentChar := 65;
    UpdateCharList;
    lstCharsClick(nil);
    UpdateStatus;
    SetModified(False);
    StatusBar.Panels[0].Text := Format('Loaded TEGL font: %s (%dx%d)', 
      [ExtractFileName(FN), TEGLFnt.MaxWidth, TEGLFnt.Height]);
  finally
    TEGLFnt.Free;
  end;
end;

procedure TfrmMain.SaveTEGLFontFile(const FN: string);
var
  TEGLFnt: TTEGLFont;
  I: Integer;
begin
  TEGLFnt := TTEGLFont.Create;
  try
    TEGLFnt.FontName := edtFontName.Text;
    TEGLFnt.SetProperties(spnHeight.Value, spnAscent.Value, FCharRangeStart, FCharRangeEnd);
    
    // Add all characters in range
    for I := FCharRangeStart to FCharRangeEnd do
      if FCharBitmaps[I] <> nil then
        TEGLFnt.SetGlyphFromBitmap(I, FCharBitmaps[I]);
    
    if TEGLFnt.SaveToFile(FN) then
    begin
      FCurrentFile := FN;
      SetModified(False);
      StatusBar.Panels[0].Text := 'Saved TEGL font: ' + ExtractFileName(FN);
    end
    else
      ShowMessage('Failed to save TEGL font');
  finally
    TEGLFnt.Free;
  end;
end;

{ Amiga Font Format Support }

procedure TfrmMain.LoadAmigaFontFile(const FN: string);
var
  AmigaFnt: TAmigaFont;
  I, CW, CH: Integer;
  Bmp: TBitmap;
begin
  AmigaFnt := TAmigaFont.Create;
  try
    if not AmigaFnt.LoadFromFile(FN) then
    begin
      ShowMessage('Failed to load Amiga font: ' + FN + #13 +
                  'Note: Amiga fonts are complex Amiga executables. ' +
                  'Some fonts may not be fully supported.');
      Exit;
    end;
    
    // Clear existing data
    for I := 0 to 255 do
    begin
      ClearUndoRedo(I);
      if FCharBitmaps[I] <> nil then begin FCharBitmaps[I].Free; FCharBitmaps[I] := nil; end;
    end;
    
    edtFontName.Text := AmigaFnt.FontName;
    spnHeight.Value := AmigaFnt.YSize;
    spnAscent.Value := AmigaFnt.Baseline;
    CH := AmigaFnt.YSize;
    
    // Load glyphs for the defined range
    for I := AmigaFnt.LoChar to AmigaFnt.HiChar do
    begin
      CW := AmigaFnt.GetCharWidth(I);
      if CW <= 0 then Continue;
      
      Bmp := TBitmap.Create;
      Bmp.Width := CW;
      Bmp.Height := CH;
      Bmp.Canvas.Brush.Color := clWhite;
      Bmp.Canvas.FillRect(0, 0, CW, CH);
      
      if AmigaFnt.GetGlyphBitmap(I, Bmp) then
        FCharBitmaps[I] := Bmp
      else
        Bmp.Free;
    end;
    
    // Update character range
    FCharRangeStart := AmigaFnt.LoChar;
    FCharRangeEnd := AmigaFnt.HiChar;
    spnRangeStart.Value := FCharRangeStart;
    spnRangeEnd.Value := FCharRangeEnd;
    for I := 0 to 255 do
      FCharEnabled[I] := (I >= FCharRangeStart) and (I <= FCharRangeEnd);
    
    FCurrentFile := FN;
    FCurrentChar := 65;
    UpdateCharList;
    lstCharsClick(nil);
    UpdateStatus;
    SetModified(False);
    StatusBar.Panels[0].Text := Format('Loaded Amiga font: %s (%dx%d)', 
      [ExtractFileName(FN), AmigaFnt.XSize, AmigaFnt.YSize]);
  finally
    AmigaFnt.Free;
  end;
end;

procedure TfrmMain.SaveAmigaFontFile(const FN: string);
var
  F: File;
  I, J, X, Y: Integer;
  NumChars: Integer;
  Modulo: Integer;
  MaxBitOffset: Integer;
  BitOffset: Integer;
  CharWidths: array of Integer;
  CharData: array of Byte;
  BytePos, BitPos: Integer;
  FontHeight, FontBaseline: Integer;
  LoChar, HiChar: Byte;
  
  // File structure offsets (from code start at 0x20)
  CharLocOffset: Integer;
  CharDataOffset: Integer;
  RelocOffset: Integer;
  CodeSize: Integer;
  
  procedure WriteBEWord(W: Word);
  var
    Buf: array[0..1] of Byte;
  begin
    Buf[0] := (W shr 8) and $FF;
    Buf[1] := W and $FF;
    BlockWrite(F, Buf, 2);
  end;
  
  procedure WriteBELong(L: LongWord);
  var
    Buf: array[0..3] of Byte;
  begin
    Buf[0] := (L shr 24) and $FF;
    Buf[1] := (L shr 16) and $FF;
    Buf[2] := (L shr 8) and $FF;
    Buf[3] := L and $FF;
    BlockWrite(F, Buf, 4);
  end;
  
  procedure WriteZeros(Count: Integer);
  var
    Buf: array[0..255] of Byte;
    ToWrite: Integer;
  begin
    FillChar(Buf, SizeOf(Buf), 0);
    while Count > 0 do
    begin
      ToWrite := Count;
      if ToWrite > SizeOf(Buf) then ToWrite := SizeOf(Buf);
      BlockWrite(F, Buf, ToWrite);
      Dec(Count, ToWrite);
    end;
  end;
  
begin
  // Determine character range
  LoChar := 32;
  HiChar := 255;
  
  // Find actual range with characters
  while (LoChar < 255) and (FCharBitmaps[LoChar] = nil) do Inc(LoChar);
  while (HiChar > LoChar) and (FCharBitmaps[HiChar] = nil) do Dec(HiChar);
  
  if FCharBitmaps[LoChar] = nil then
  begin
    ShowMessage('No characters to save.');
    Exit;
  end;
  
  NumChars := HiChar - LoChar + 2;  // +2 for notdef glyph
  FontHeight := spnHeight.Value;
  FontBaseline := spnAscent.Value - 1;  // Convert back from 1-based
  
  // Calculate character widths and total bit width
  SetLength(CharWidths, NumChars);
  MaxBitOffset := 0;
  for I := 0 to NumChars - 1 do
  begin
    if (LoChar + I <= HiChar) and (FCharBitmaps[LoChar + I] <> nil) then
      CharWidths[I] := FCharBitmaps[LoChar + I].Width
    else
      CharWidths[I] := 8;  // Default width for missing chars
    MaxBitOffset := MaxBitOffset + CharWidths[I];
  end;
  
  // Calculate modulo (bytes per row, rounded up)
  Modulo := (MaxBitOffset + 7) div 8;
  // Round up to even number for word alignment
  if (Modulo mod 2) <> 0 then Inc(Modulo);
  
  // Build CharData bitmap (strike font format)
  SetLength(CharData, Modulo * FontHeight);
  FillChar(CharData[0], Length(CharData), 0);
  
  BitOffset := 0;
  for I := 0 to NumChars - 1 do
  begin
    if (LoChar + I <= HiChar) and (FCharBitmaps[LoChar + I] <> nil) then
    begin
      // Copy bitmap data using Amiga bit layout
      for Y := 0 to FontHeight - 1 do
      begin
        for X := 0 to CharWidths[I] - 1 do
        begin
          if (Y < FCharBitmaps[LoChar + I].Height) and 
             (X < FCharBitmaps[LoChar + I].Width) and
             (FCharBitmaps[LoChar + I].Canvas.Pixels[X, Y] = clBlack) then
          begin
            // Set bit using Amiga formula: k = location + col + row * modulo * 8
            BytePos := (BitOffset + X + Y * Modulo * 8) div 8;
            BitPos := 7 - ((BitOffset + X + Y * Modulo * 8) mod 8);
            if BytePos < Length(CharData) then
              CharData[BytePos] := CharData[BytePos] or (1 shl BitPos);
          end;
        end;
      end;
    end;
    BitOffset := BitOffset + CharWidths[I];
  end;
  
  // Calculate file layout
  // Code starts at 0x20
  // TextFont at 0x6E (offset 0x4E from code start)
  // CharLoc follows TextFont (32 bytes) at 0x8E
  CharLocOffset := $6E;  // offset from code start for pointer storage
  CharDataOffset := CharLocOffset + NumChars * 4;
  // Align CharData to 4 bytes
  if (CharDataOffset mod 4) <> 0 then
    CharDataOffset := CharDataOffset + (4 - (CharDataOffset mod 4));
  
  CodeSize := $4E + 32 + NumChars * 4 + Length(CharData);
  // Align to longword
  if (CodeSize mod 4) <> 0 then
    CodeSize := CodeSize + (4 - (CodeSize mod 4));
  RelocOffset := CodeSize;
  
  AssignFile(F, FN);
  {$I-}
  Rewrite(F, 1);
  {$I+}
  if IOResult <> 0 then
  begin
    ShowMessage('Cannot create file: ' + FN);
    Exit;
  end;
  
  try
    // HUNK_HEADER
    WriteBELong($000003F3);  // HUNK_HEADER
    WriteBELong($00000000);  // No resident libraries
    WriteBELong($00000001);  // 1 hunk
    WriteBELong($00000000);  // First hunk
    WriteBELong($00000000);  // Last hunk
    WriteBELong((CodeSize + 3) div 4);  // Hunk size in longwords
    
    // HUNK_CODE
    WriteBELong($000003E9);  // HUNK_CODE
    WriteBELong((CodeSize + 3) div 4);  // Code size in longwords
    
    // Code stub (0x20-0x23)
    WriteBELong($70644E75);  // moveq #$64,d0 / rts
    
    // Padding to 0x6E (from 0x24)
    WriteZeros($6E - $24);
    
    // TextFont structure at 0x6E (32 bytes)
    WriteBEWord(FontHeight);           // +0: YSize
    WriteBEWord($0042);                // +2: Style=0, Flags=$42 (FPF_ROMFONT | FPF_DESIGNED)
    WriteBEWord(CharWidths[0]);        // +4: XSize (nominal width)
    WriteBEWord(FontBaseline);         // +6: Baseline
    WriteBEWord(1);                    // +8: BoldSmear
    WriteBEWord(0);                    // +10: Accessors
    WriteBEWord((LoChar shl 8) or HiChar);  // +12: LoChar, HiChar
    WriteBELong(CharDataOffset);       // +14: CharData pointer (offset from code start)
    WriteBEWord(Modulo);               // +18: Modulo
    WriteBELong(CharLocOffset);        // +20: CharLoc pointer (offset from code start)
    WriteBELong(0);                    // +24: CharSpace (none)
    WriteBELong(0);                    // +28: CharKern (none)
    
    // CharLoc table at 0x8E
    BitOffset := 0;
    for I := 0 to NumChars - 1 do
    begin
      WriteBEWord(BitOffset);          // Bit offset
      WriteBEWord(CharWidths[I]);      // Bit width
      BitOffset := BitOffset + CharWidths[I];
    end;
    
    // Padding to align CharData if needed
    J := $20 + CharLocOffset + NumChars * 4;
    while J < $20 + CharDataOffset do
    begin
      WriteBEWord(0);
      Inc(J, 2);
    end;
    
    // CharData bitmap
    BlockWrite(F, CharData[0], Length(CharData));
    
    // Padding to align to longword
    J := Length(CharData);
    while (J mod 4) <> 0 do
    begin
      WriteBEWord(0);
      Inc(J, 2);
    end;
    
    // HUNK_RELOC32
    WriteBELong($000003EC);  // HUNK_RELOC32
    WriteBELong(2);          // 2 relocations
    WriteBELong(0);          // Hunk 0
    WriteBELong($4E + 14);   // Offset to CharData pointer (font+14)
    WriteBELong($4E + 20);   // Offset to CharLoc pointer (font+20)
    WriteBELong(0);          // End of relocations
    
    // HUNK_END
    WriteBELong($000003F2);
    
    FCurrentFile := FN;
    SetModified(False);
    StatusBar.Panels[0].Text := 'Saved Amiga font: ' + ExtractFileName(FN);
  finally
    CloseFile(F);
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
  SaveFileName: string;
begin
  // Filter indices: 1=FON, 2=FNT, 3=ROM, 4=Deluxe Paint, 5=TEGL, 6=Amiga, 7=All
  dlgSave.Filter := 'Windows FON Files (*.FON)|*.FON|' +
                    'DOS BIOS Fonts (*.FNT)|*.FNT|' +
                    'ROM Fonts (*.ROM)|*.ROM|' +
                    'Deluxe Paint Fonts (*.M*)|*.M*|' +
                    'TEGL Fonts (*.RTF)|*.RTF|' +
                    'Amiga Fonts (*.FNT)|*.FNT|' +
                    'All Files (*.*)|*.*';
  dlgSave.FileName := edtFontName.Text;
  dlgSave.FilterIndex := FLastSaveFilterIndex;
  
  if dlgSave.Execute then
  begin
    FLastSaveFilterIndex := dlgSave.FilterIndex;
    SaveFileName := dlgSave.FileName;
    
    // Add default extension based on filter if none provided
    if ExtractFileExt(SaveFileName) = '' then
    begin
      case dlgSave.FilterIndex of
        1: SaveFileName := SaveFileName + '.FON';
        2: SaveFileName := SaveFileName + '.FNT';
        3: SaveFileName := SaveFileName + '.ROM';
        4: SaveFileName := SaveFileName + '.M8';
        5: SaveFileName := SaveFileName + '.RTF';
        6: SaveFileName := SaveFileName + '.FNT';
      end;
    end;
    
    // Save based on selected filter, not extension
    case dlgSave.FilterIndex of
      1: // Windows FON
        begin
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
            FCreator.SaveToFile(SaveFileName);
            FCurrentFile := SaveFileName;
            SetModified(False);
            UpdateStatus;
            StatusBar.Panels[0].Text := 'Saved FON: ' + ExtractFileName(SaveFileName);
          except
            on E: Exception do ShowMessage('Error saving FON: ' + E.Message);
          end;
        end;
        
      2, 3: // DOS FNT or ROM (same format)
        begin
          try
            SaveFNTFile(SaveFileName);
          except
            on E: Exception do ShowMessage('Error saving FNT: ' + E.Message);
          end;
        end;
        
      4: // Deluxe Paint
        begin
          try
            SaveDPFontFile(SaveFileName);
          except
            on E: Exception do ShowMessage('Error saving Deluxe Paint font: ' + E.Message);
          end;
        end;
        
      5: // TEGL
        begin
          try
            SaveTEGLFontFile(SaveFileName);
          except
            on E: Exception do ShowMessage('Error saving TEGL font: ' + E.Message);
          end;
        end;
        
      6: // Amiga
        begin
          try
            SaveAmigaFontFile(SaveFileName);
          except
            on E: Exception do ShowMessage('Error saving Amiga font: ' + E.Message);
          end;
        end;
        
    else
      // All Files - default to FON
      begin
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
          FCreator.SaveToFile(SaveFileName);
          FCurrentFile := SaveFileName;
          SetModified(False);
          UpdateStatus;
          StatusBar.Panels[0].Text := 'Saved: ' + ExtractFileName(SaveFileName);
        except
          on E: Exception do ShowMessage('Error: ' + E.Message);
        end;
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
  edtCopyright.Text := 'RetroNicks Bitmap Font Editor';
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

procedure TfrmMain.mnuIncreaseWidthClick(Sender: TObject);
var
  I, OldW, NewW, H: Integer;
  NewBmp: TBitmap;
  Cnt: Integer;
begin
  Cnt := 0;
  H := spnHeight.Value;
  
  for I := 0 to 255 do
  begin
    if FCharBitmaps[I] <> nil then
    begin
      OldW := FCharBitmaps[I].Width;
      NewW := OldW + 1;
      
      // Create new bitmap with increased width
      NewBmp := TBitmap.Create;
      NewBmp.Width := NewW;
      NewBmp.Height := H;
      NewBmp.Canvas.Brush.Color := clWhite;
      NewBmp.Canvas.FillRect(0, 0, NewW, H);
      
      // Copy old bitmap to new (left-aligned)
      NewBmp.Canvas.Draw(0, 0, FCharBitmaps[I]);
      
      // Replace old with new
      FCharBitmaps[I].Free;
      FCharBitmaps[I] := NewBmp;
      Inc(Cnt);
    end;
  end;
  
  if Cnt > 0 then
  begin
    SetModified(True);
    lstCharsClick(nil);
    UpdatePreview;
    StatusBar.Panels[0].Text := Format('Increased width of %d characters', [Cnt]);
  end
  else
    ShowMessage('No characters to modify.');
end;

procedure TfrmMain.mnuShiftLeftClick(Sender: TObject);
var
  I, X, Y, W, H: Integer;
  CanShift: Boolean;
  NewBmp: TBitmap;
  Cnt: Integer;
begin
  Cnt := 0;
  H := spnHeight.Value;
  
  for I := 0 to 255 do
  begin
    if FCharBitmaps[I] <> nil then
    begin
      W := FCharBitmaps[I].Width;
      if W < 1 then Continue;
      
      // Check if first column is empty (all white pixels)
      CanShift := True;
      for Y := 0 to H - 1 do
      begin
        if FCharBitmaps[I].Canvas.Pixels[0, Y] = clBlack then
        begin
          CanShift := False;
          Break;
        end;
      end;
      
      if CanShift then
      begin
        // Create new bitmap and shift content left by 1 pixel
        NewBmp := TBitmap.Create;
        NewBmp.Width := W;
        NewBmp.Height := H;
        NewBmp.Canvas.Brush.Color := clWhite;
        NewBmp.Canvas.FillRect(0, 0, W, H);
        
        // Copy pixels shifted left by 1
        for Y := 0 to H - 1 do
          for X := 1 to W - 1 do
            if FCharBitmaps[I].Canvas.Pixels[X, Y] = clBlack then
              NewBmp.Canvas.Pixels[X - 1, Y] := clBlack;
        
        // Replace old with new
        FCharBitmaps[I].Free;
        FCharBitmaps[I] := NewBmp;
        Inc(Cnt);
      end;
    end;
  end;
  
  if Cnt > 0 then
  begin
    SetModified(True);
    lstCharsClick(nil);
    UpdatePreview;
    StatusBar.Panels[0].Text := Format('Shifted %d characters left', [Cnt]);
  end
  else
    ShowMessage('No characters could be shifted (first column not empty).');
end;

procedure TfrmMain.mnuDecreaseWidthClick(Sender: TObject);
var
  I, X, Y, W, H: Integer;
  CanDecrease: Boolean;
  NewBmp: TBitmap;
  Cnt: Integer;
begin
  Cnt := 0;
  H := spnHeight.Value;
  
  for I := 0 to 255 do
  begin
    if FCharBitmaps[I] <> nil then
    begin
      W := FCharBitmaps[I].Width;
      if W < 2 then Continue;  // Can't decrease width below 1
      
      // Check if last column is empty (all white pixels)
      CanDecrease := True;
      for Y := 0 to H - 1 do
      begin
        if FCharBitmaps[I].Canvas.Pixels[W - 1, Y] = clBlack then
        begin
          CanDecrease := False;
          Break;
        end;
      end;
      
      if CanDecrease then
      begin
        // Create new bitmap with decreased width
        NewBmp := TBitmap.Create;
        NewBmp.Width := W - 1;
        NewBmp.Height := H;
        NewBmp.Canvas.Brush.Color := clWhite;
        NewBmp.Canvas.FillRect(0, 0, W - 1, H);
        
        // Copy pixels (excluding last column)
        for Y := 0 to H - 1 do
          for X := 0 to W - 2 do
            if FCharBitmaps[I].Canvas.Pixels[X, Y] = clBlack then
              NewBmp.Canvas.Pixels[X, Y] := clBlack;
        
        // Replace old with new
        FCharBitmaps[I].Free;
        FCharBitmaps[I] := NewBmp;
        Inc(Cnt);
      end;
    end;
  end;
  
  if Cnt > 0 then
  begin
    SetModified(True);
    lstCharsClick(nil);
    UpdatePreview;
    StatusBar.Panels[0].Text := Format('Decreased width of %d characters', [Cnt]);
  end
  else
    ShowMessage('No characters could be decreased (last column not empty).');
end;

procedure TfrmMain.mnuShiftRightClick(Sender: TObject);
var
  I, X, Y, W, H: Integer;
  CanShift: Boolean;
  NewBmp: TBitmap;
  Cnt: Integer;
begin
  Cnt := 0;
  H := spnHeight.Value;
  
  for I := 0 to 255 do
  begin
    if FCharBitmaps[I] <> nil then
    begin
      W := FCharBitmaps[I].Width;
      if W < 1 then Continue;
      
      // Check if last column is empty (all white pixels)
      CanShift := True;
      for Y := 0 to H - 1 do
      begin
        if FCharBitmaps[I].Canvas.Pixels[W - 1, Y] = clBlack then
        begin
          CanShift := False;
          Break;
        end;
      end;
      
      if CanShift then
      begin
        // Create new bitmap and shift content right by 1 pixel
        NewBmp := TBitmap.Create;
        NewBmp.Width := W;
        NewBmp.Height := H;
        NewBmp.Canvas.Brush.Color := clWhite;
        NewBmp.Canvas.FillRect(0, 0, W, H);
        
        // Copy pixels shifted right by 1
        for Y := 0 to H - 1 do
          for X := 0 to W - 2 do
            if FCharBitmaps[I].Canvas.Pixels[X, Y] = clBlack then
              NewBmp.Canvas.Pixels[X + 1, Y] := clBlack;
        
        // Replace old with new
        FCharBitmaps[I].Free;
        FCharBitmaps[I] := NewBmp;
        Inc(Cnt);
      end;
    end;
  end;
  
  if Cnt > 0 then
  begin
    SetModified(True);
    lstCharsClick(nil);
    UpdatePreview;
    StatusBar.Panels[0].Text := Format('Shifted %d characters right', [Cnt]);
  end
  else
    ShowMessage('No characters could be shifted (last column not empty).');
end;

procedure TfrmMain.mnuAutoTrimClick(Sender: TObject);
var
  I, X, Y, W, H: Integer;
  LeftTrim, RightTrim: Integer;
  NewW: Integer;
  NewBmp: TBitmap;
  Cnt: Integer;
  ColEmpty: Boolean;
begin
  Cnt := 0;
  H := spnHeight.Value;
  
  for I := 0 to 255 do
  begin
    if FCharBitmaps[I] <> nil then
    begin
      W := FCharBitmaps[I].Width;
      if W < 2 then Continue;  // Need at least 2 pixels to trim
      
      // Find how many empty columns on the left
      LeftTrim := 0;
      for X := 0 to W - 2 do  // Leave at least 1 column
      begin
        ColEmpty := True;
        for Y := 0 to H - 1 do
        begin
          if FCharBitmaps[I].Canvas.Pixels[X, Y] = clBlack then
          begin
            ColEmpty := False;
            Break;
          end;
        end;
        if ColEmpty then
          Inc(LeftTrim)
        else
          Break;
      end;
      
      // Find how many empty columns on the right
      RightTrim := 0;
      for X := W - 1 downto LeftTrim + 1 do  // Leave at least 1 column
      begin
        ColEmpty := True;
        for Y := 0 to H - 1 do
        begin
          if FCharBitmaps[I].Canvas.Pixels[X, Y] = clBlack then
          begin
            ColEmpty := False;
            Break;
          end;
        end;
        if ColEmpty then
          Inc(RightTrim)
        else
          Break;
      end;
      
      // Apply trimming if any
      if (LeftTrim > 0) or (RightTrim > 0) then
      begin
        NewW := W - LeftTrim - RightTrim;
        if NewW < 1 then NewW := 1;  // Ensure minimum width of 1
        
        NewBmp := TBitmap.Create;
        NewBmp.Width := NewW;
        NewBmp.Height := H;
        NewBmp.Canvas.Brush.Color := clWhite;
        NewBmp.Canvas.FillRect(0, 0, NewW, H);
        
        // Copy the non-empty portion
        for Y := 0 to H - 1 do
          for X := 0 to NewW - 1 do
            if FCharBitmaps[I].Canvas.Pixels[X + LeftTrim, Y] = clBlack then
              NewBmp.Canvas.Pixels[X, Y] := clBlack;
        
        // Replace old with new
        FCharBitmaps[I].Free;
        FCharBitmaps[I] := NewBmp;
        Inc(Cnt);
      end;
    end;
  end;
  
  if Cnt > 0 then
  begin
    SetModified(True);
    lstCharsClick(nil);
    UpdatePreview;
    StatusBar.Panels[0].Text := Format('Auto-trimmed %d characters', [Cnt]);
  end
  else
    ShowMessage('No characters needed trimming.');
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
