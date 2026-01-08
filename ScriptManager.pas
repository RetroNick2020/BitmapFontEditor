{$MODE OBJFPC}{$H+}
unit ScriptManager;

{
  ScriptManager - Script Manager Form for Bitmap Font Editor
  
  Provides UI for managing, editing, and running QBasic scripts.
}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Menus, Buttons, LCLType, SynEdit, SynHighlighterAny,
  QBasicInterp, FontScript;

type
  TScriptInfo = record
    FileName: string;
    Name: string;
    Author: string;
    Description: string;
    Hotkey: string;
    Menu: string;
    Modified: Boolean;
  end;

  { TfrmScriptManager }

  TfrmScriptManager = class(TForm)
    btnNew: TBitBtn;
    btnOpen: TBitBtn;
    btnSave: TBitBtn;
    btnRun: TBitBtn;
    btnStop: TBitBtn;
    lblScripts: TLabel;
    lblOutput: TLabel;
    lstScripts: TListBox;
    memoOutput: TMemo;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    pnlToolbar: TPanel;
    pnlOutput: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    synEdit: TSynEdit;
    synBasic: TSynAnySyn;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    StatusBar: TStatusBar;
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lstScriptsClick(Sender: TObject);
    procedure synEditChange(Sender: TObject);
    procedure synEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FScripts: array of TScriptInfo;
    FCurrentScript: Integer;
    FInterpreter: TQBasicInterpreter;
    FFontAPI: TFontScriptAPI;
    FScriptsPath: string;
    FModified: Boolean;
    
    procedure LoadScriptList;
    procedure LoadScript(const FileName: string);
    procedure SaveCurrentScript;
    procedure UpdateScriptInfo;
    function ConfirmSave: Boolean;
    procedure SetupSyntaxHighlighter;
    
    { Interpreter callbacks }
    procedure OnPrint(const AText: string);
    function OnInput(const Prompt: string; var Value: string): Boolean;
    
    { Font API function wrappers }
    function API_GetPixel(const Args: array of Double): Double;
    procedure API_SetPixel(const Args: array of Double; const StrArgs: array of string);
    function API_GetCharWidth(const Args: array of Double): Double;
    procedure API_SetCharWidth(const Args: array of Double; const StrArgs: array of string);
    function API_GetFontHeight(const Args: array of Double): Double;
    function API_GetCurrentChar(const Args: array of Double): Double;
    function API_GetRangeStart(const Args: array of Double): Double;
    function API_GetRangeEnd(const Args: array of Double): Double;
    function API_IsCharEmpty(const Args: array of Double): Double;
    procedure API_ClearChar(const Args: array of Double; const StrArgs: array of string);
    procedure API_CopyChar(const Args: array of Double; const StrArgs: array of string);
    procedure API_FlipCharH(const Args: array of Double; const StrArgs: array of string);
    procedure API_FlipCharV(const Args: array of Double; const StrArgs: array of string);
    procedure API_InvertChar(const Args: array of Double; const StrArgs: array of string);
    procedure API_ShiftChar(const Args: array of Double; const StrArgs: array of string);
    procedure API_SaveUndo(const Args: array of Double; const StrArgs: array of string);
    procedure API_SelectChar(const Args: array of Double; const StrArgs: array of string);
    procedure API_Refresh(const Args: array of Double; const StrArgs: array of string);
    procedure API_ShowMessage(const Args: array of Double; const StrArgs: array of string);
    function API_InputNumber(const Args: array of Double): Double;
    function API_Confirm(const Args: array of Double): Double;
    
    function API_CreateBitmap(const Args: array of Double): Double;
    procedure API_FreeBitmap(const Args: array of Double; const StrArgs: array of string);
    function API_GetBitmapPixel(const Args: array of Double): Double;
    procedure API_SetBitmapPixel(const Args: array of Double; const StrArgs: array of string);
    procedure API_CopyBitmapToChar(const Args: array of Double; const StrArgs: array of string);
    procedure API_CopyCharToBitmap(const Args: array of Double; const StrArgs: array of string);
    
    function API_GetFontName(const Args: array of Double; const StrArgs: array of string): string;
    function API_InputString(const Args: array of Double; const StrArgs: array of string): string;
    
  public
    property FontAPI: TFontScriptAPI read FFontAPI write FFontAPI;
    property ScriptsPath: string read FScriptsPath write FScriptsPath;
    
    procedure RunScript(const Code: string);
    procedure RunScriptFile(const FileName: string);
  end;

var
  frmScriptManager: TfrmScriptManager;

implementation

{$R *.lfm}

{ TfrmScriptManager }

procedure TfrmScriptManager.FormCreate(Sender: TObject);
const
  DefaultScript = 
    '''#NAME: New Script' + LineEnding +
    '''#AUTHOR: ' + LineEnding +
    '''#DESCRIPTION: ' + LineEnding +
    '' + LineEnding +
    ''' Your script code here' + LineEnding +
    '' + LineEnding +
    'PRINT "Hello!"' + LineEnding +
    '' + LineEnding +
    'END';
begin
  FCurrentScript := -1;
  FModified := False;
  SetLength(FScripts, 0);
  
  FInterpreter := TQBasicInterpreter.Create;
  FInterpreter.OnPrint := @OnPrint;
  FInterpreter.OnInput := @OnInput;
  
  FFontAPI := nil; // Set by owner
  FScriptsPath := ExtractFilePath(Application.ExeName) + 'Scripts' + PathDelim;
  
  SetupSyntaxHighlighter;
  
  // Set highlighter and initial text in code (more reliable than LFM)
  synEdit.Highlighter := synBasic;
  synEdit.Text := DefaultScript;
  
  btnStop.Enabled := False;
  FModified := False;
end;

procedure TfrmScriptManager.FormDestroy(Sender: TObject);
begin
  FInterpreter.Free;
end;

procedure TfrmScriptManager.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if not ConfirmSave then
    CloseAction := caNone;
end;

procedure TfrmScriptManager.FormShow(Sender: TObject);
begin
  LoadScriptList;
  synEdit.SetFocus;
end;

procedure TfrmScriptManager.synEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Handle F5 to run script
  if Key = VK_F5 then
  begin
    btnRunClick(nil);
    Key := 0;
    Exit;
  end;
  
  // Handle Escape to stop script
  if Key = VK_ESCAPE then
  begin
    if FInterpreter.Running then
      btnStopClick(nil);
    // Don't consume Escape - let SynEdit handle it too
    Exit;
  end;
  
  // Handle Ctrl+S to save
  if (ssCtrl in Shift) and (Key = VK_S) then
  begin
    btnSaveClick(nil);
    Key := 0;
    Exit;
  end;
  
  // Handle Ctrl+N for new
  if (ssCtrl in Shift) and (Key = VK_N) then
  begin
    btnNewClick(nil);
    Key := 0;
    Exit;
  end;
  
  // Handle Ctrl+O for open
  if (ssCtrl in Shift) and (Key = VK_O) then
  begin
    btnOpenClick(nil);
    Key := 0;
    Exit;
  end;
  
  // All other keys pass through to SynEdit normally
end;

procedure TfrmScriptManager.SetupSyntaxHighlighter;
begin
  synBasic.KeyWords.Clear;
  synBasic.KeyWords.Add('AND');
  synBasic.KeyWords.Add('AS');
  synBasic.KeyWords.Add('CALL');
  synBasic.KeyWords.Add('CASE');
  synBasic.KeyWords.Add('CLS');
  synBasic.KeyWords.Add('COLOR');
  synBasic.KeyWords.Add('DIM');
  synBasic.KeyWords.Add('DO');
  synBasic.KeyWords.Add('DOUBLE');
  synBasic.KeyWords.Add('ELSE');
  synBasic.KeyWords.Add('ELSEIF');
  synBasic.KeyWords.Add('END');
  synBasic.KeyWords.Add('EXIT');
  synBasic.KeyWords.Add('FOR');
  synBasic.KeyWords.Add('FUNCTION');
  synBasic.KeyWords.Add('GOSUB');
  synBasic.KeyWords.Add('GOTO');
  synBasic.KeyWords.Add('IF');
  synBasic.KeyWords.Add('INPUT');
  synBasic.KeyWords.Add('INTEGER');
  synBasic.KeyWords.Add('IS');
  synBasic.KeyWords.Add('LET');
  synBasic.KeyWords.Add('LOCATE');
  synBasic.KeyWords.Add('LOOP');
  synBasic.KeyWords.Add('MOD');
  synBasic.KeyWords.Add('NEXT');
  synBasic.KeyWords.Add('NOT');
  synBasic.KeyWords.Add('OR');
  synBasic.KeyWords.Add('PRINT');
  synBasic.KeyWords.Add('REM');
  synBasic.KeyWords.Add('RETURN');
  synBasic.KeyWords.Add('SCREEN');
  synBasic.KeyWords.Add('SELECT');
  synBasic.KeyWords.Add('SHARED');
  synBasic.KeyWords.Add('SINGLE');
  synBasic.KeyWords.Add('STATIC');
  synBasic.KeyWords.Add('STEP');
  synBasic.KeyWords.Add('STRING');
  synBasic.KeyWords.Add('SUB');
  synBasic.KeyWords.Add('THEN');
  synBasic.KeyWords.Add('TO');
  synBasic.KeyWords.Add('WEND');
  synBasic.KeyWords.Add('WHILE');
  synBasic.KeyWords.Add('XOR');
  
  synBasic.CommentAttri.Foreground := clGreen;
  synBasic.KeyAttri.Foreground := clBlue;
  synBasic.KeyAttri.Style := [fsBold];
  synBasic.StringAttri.Foreground := clMaroon;
  synBasic.NumberAttri.Foreground := clNavy;
  
  synBasic.StringDelim := sdDoubleQuote;
  synBasic.Enabled := True;
end;

procedure TfrmScriptManager.LoadScriptList;
var
  SR: TSearchRec;
  Info: TScriptInfo;
  Code: TStringList;
  Meta: TScriptMetadata;
begin
  lstScripts.Items.Clear;
  SetLength(FScripts, 0);
  
  if not DirectoryExists(FScriptsPath) then
    ForceDirectories(FScriptsPath);
  
  Code := TStringList.Create;
  try
    if FindFirst(FScriptsPath + '*.bas', faAnyFile, SR) = 0 then
    begin
      repeat
        Code.LoadFromFile(FScriptsPath + SR.Name);
        Meta := ParseScriptMetadata(Code.Text);
        
        Info.FileName := FScriptsPath + SR.Name;
        if Meta.Name <> '' then
          Info.Name := Meta.Name
        else
          Info.Name := ChangeFileExt(SR.Name, '');
        Info.Author := Meta.Author;
        Info.Description := Meta.Description;
        Info.Hotkey := Meta.Hotkey;
        Info.Menu := Meta.Menu;
        Info.Modified := False;
        
        SetLength(FScripts, Length(FScripts) + 1);
        FScripts[High(FScripts)] := Info;
        lstScripts.Items.Add(Info.Name);
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  finally
    Code.Free;
  end;
end;

procedure TfrmScriptManager.LoadScript(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    synEdit.Lines.LoadFromFile(FileName);
    FModified := False;
    UpdateScriptInfo;
  end;
end;

procedure TfrmScriptManager.SaveCurrentScript;
var
  FileName: string;
begin
  if FCurrentScript >= 0 then
    FileName := FScripts[FCurrentScript].FileName
  else
  begin
    if dlgSave.Execute then
      FileName := dlgSave.FileName
    else
      Exit;
  end;
  
  synEdit.Lines.SaveToFile(FileName);
  FModified := False;
  
  if FCurrentScript >= 0 then
    FScripts[FCurrentScript].Modified := False;
    
  UpdateScriptInfo;
  StatusBar.Panels[0].Text := 'Saved: ' + ExtractFileName(FileName);
end;

procedure TfrmScriptManager.UpdateScriptInfo;
var
  Meta: TScriptMetadata;
begin
  Meta := ParseScriptMetadata(synEdit.Text);
  
  if FCurrentScript >= 0 then
  begin
    if Meta.Name <> '' then
      FScripts[FCurrentScript].Name := Meta.Name;
    FScripts[FCurrentScript].Author := Meta.Author;
    FScripts[FCurrentScript].Description := Meta.Description;
    FScripts[FCurrentScript].Hotkey := Meta.Hotkey;
    FScripts[FCurrentScript].Menu := Meta.Menu;
  end;
  
  if FModified then
    Caption := 'Script Manager *'
  else
    Caption := 'Script Manager';
end;

function TfrmScriptManager.ConfirmSave: Boolean;
begin
  Result := True;
  if FModified then
  begin
    case MessageDlg('Save Changes?', 'Script has been modified. Save changes?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: SaveCurrentScript;
      mrCancel: Result := False;
    end;
  end;
end;

procedure TfrmScriptManager.btnNewClick(Sender: TObject);
const
  NewScriptTemplate =
    '''#NAME: New Script' + LineEnding +
    '''#AUTHOR: ' + LineEnding +
    '''#DESCRIPTION: ' + LineEnding +
    '' + LineEnding +
    ''' New Script' + LineEnding +
    '' + LineEnding +
    'PRINT "Hello from script!"' + LineEnding +
    '' + LineEnding +
    'END' + LineEnding;
begin
  if not ConfirmSave then Exit;
  
  FCurrentScript := -1;
  lstScripts.ItemIndex := -1;
  synEdit.Text := NewScriptTemplate;
  FModified := True;
  UpdateScriptInfo;
  memoOutput.Clear;
end;

procedure TfrmScriptManager.btnOpenClick(Sender: TObject);
begin
  if not ConfirmSave then Exit;
  
  if dlgOpen.Execute then
  begin
    FCurrentScript := -1;
    lstScripts.ItemIndex := -1;
    LoadScript(dlgOpen.FileName);
  end;
end;

procedure TfrmScriptManager.btnSaveClick(Sender: TObject);
begin
  SaveCurrentScript;
  LoadScriptList;
end;

procedure TfrmScriptManager.btnRunClick(Sender: TObject);
begin
  RunScript(synEdit.Text);
end;

procedure TfrmScriptManager.btnStopClick(Sender: TObject);
begin
  FInterpreter.Stop;
  StatusBar.Panels[0].Text := 'Script stopped';
end;

procedure TfrmScriptManager.lstScriptsClick(Sender: TObject);
begin
  if lstScripts.ItemIndex < 0 then Exit;
  if not ConfirmSave then
  begin
    lstScripts.ItemIndex := FCurrentScript;
    Exit;
  end;
  
  FCurrentScript := lstScripts.ItemIndex;
  LoadScript(FScripts[FCurrentScript].FileName);
  memoOutput.Clear;
end;

procedure TfrmScriptManager.synEditChange(Sender: TObject);
begin
  FModified := True;
  if FCurrentScript >= 0 then
    FScripts[FCurrentScript].Modified := True;
  UpdateScriptInfo;
end;

{ Interpreter callbacks }

procedure TfrmScriptManager.OnPrint(const AText: string);
begin
  if AText = #13#10 then
    memoOutput.Lines.Add('')
  else if (Length(AText) > 0) and (AText[Length(AText)] in [#13, #10]) then
    memoOutput.Lines.Add(Copy(AText, 1, Length(AText) - 1))
  else if memoOutput.Lines.Count > 0 then
    memoOutput.Lines[memoOutput.Lines.Count - 1] := 
      memoOutput.Lines[memoOutput.Lines.Count - 1] + AText
  else
    memoOutput.Lines.Add(AText);
  Application.ProcessMessages;
end;

function TfrmScriptManager.OnInput(const Prompt: string; var Value: string): Boolean;
begin
  Result := InputQuery('Script Input', Prompt, Value);
end;

{ Font API function wrappers }

function TfrmScriptManager.API_GetPixel(const Args: array of Double): Double;
begin
  if (Length(Args) >= 3) and Assigned(FFontAPI) then
  begin
    if FFontAPI.GetPixel(Trunc(Args[0]), Trunc(Args[1]), Trunc(Args[2])) then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TfrmScriptManager.API_SetPixel(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 4) and Assigned(FFontAPI) then
    FFontAPI.SetPixel(Trunc(Args[0]), Trunc(Args[1]), Trunc(Args[2]), Args[3] <> 0);
end;

function TfrmScriptManager.API_GetCharWidth(const Args: array of Double): Double;
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    Result := FFontAPI.GetCharWidth(Trunc(Args[0]))
  else
    Result := 0;
end;

procedure TfrmScriptManager.API_SetCharWidth(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 2) and Assigned(FFontAPI) then
    FFontAPI.SetCharWidth(Trunc(Args[0]), Trunc(Args[1]));
end;

function TfrmScriptManager.API_GetFontHeight(const Args: array of Double): Double;
begin
  if Assigned(FFontAPI) then
    Result := FFontAPI.GetFontHeight
  else
    Result := 16;
end;

function TfrmScriptManager.API_GetCurrentChar(const Args: array of Double): Double;
begin
  if Assigned(FFontAPI) then
    Result := FFontAPI.GetCurrentChar
  else
    Result := 65;
end;

function TfrmScriptManager.API_GetRangeStart(const Args: array of Double): Double;
begin
  if Assigned(FFontAPI) then
    Result := FFontAPI.GetRangeStart
  else
    Result := 32;
end;

function TfrmScriptManager.API_GetRangeEnd(const Args: array of Double): Double;
begin
  if Assigned(FFontAPI) then
    Result := FFontAPI.GetRangeEnd
  else
    Result := 127;
end;

function TfrmScriptManager.API_IsCharEmpty(const Args: array of Double): Double;
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
  begin
    if FFontAPI.IsCharEmpty(Trunc(Args[0])) then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 1;
end;

procedure TfrmScriptManager.API_ClearChar(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.ClearChar(Trunc(Args[0]));
end;

procedure TfrmScriptManager.API_CopyChar(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 2) and Assigned(FFontAPI) then
    FFontAPI.CopyChar(Trunc(Args[0]), Trunc(Args[1]));
end;

procedure TfrmScriptManager.API_FlipCharH(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.FlipCharH(Trunc(Args[0]));
end;

procedure TfrmScriptManager.API_FlipCharV(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.FlipCharV(Trunc(Args[0]));
end;

procedure TfrmScriptManager.API_InvertChar(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.InvertChar(Trunc(Args[0]));
end;

procedure TfrmScriptManager.API_ShiftChar(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 3) and Assigned(FFontAPI) then
    FFontAPI.ShiftChar(Trunc(Args[0]), Trunc(Args[1]), Trunc(Args[2]));
end;

procedure TfrmScriptManager.API_SaveUndo(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.SaveUndo(Trunc(Args[0]));
end;

procedure TfrmScriptManager.API_SelectChar(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.SelectChar(Trunc(Args[0]));
end;

procedure TfrmScriptManager.API_Refresh(const Args: array of Double; const StrArgs: array of string);
begin
  if Assigned(FFontAPI) then
    FFontAPI.Refresh;
end;

procedure TfrmScriptManager.API_ShowMessage(const Args: array of Double; const StrArgs: array of string);
begin
  if Length(StrArgs) >= 1 then
    Dialogs.ShowMessage(StrArgs[0])
  else if Assigned(FFontAPI) then
    FFontAPI.ShowMessage('');
end;

function TfrmScriptManager.API_InputNumber(const Args: array of Double): Double;
var
  Default: Integer;
begin
  Default := 0;
  if Length(Args) >= 1 then
    Default := Trunc(Args[0]);
  
  if Assigned(FFontAPI) then
    Result := FFontAPI.InputNumber('Enter number', Default)
  else
    Result := Default;
end;

function TfrmScriptManager.API_Confirm(const Args: array of Double): Double;
begin
  Result := 0;
end;

function TfrmScriptManager.API_CreateBitmap(const Args: array of Double): Double;
begin
  if (Length(Args) >= 2) and Assigned(FFontAPI) then
    Result := FFontAPI.CreateBitmap(Trunc(Args[0]), Trunc(Args[1]))
  else
    Result := -1;
end;

procedure TfrmScriptManager.API_FreeBitmap(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 1) and Assigned(FFontAPI) then
    FFontAPI.FreeBitmap(Trunc(Args[0]));
end;

function TfrmScriptManager.API_GetBitmapPixel(const Args: array of Double): Double;
begin
  if (Length(Args) >= 3) and Assigned(FFontAPI) then
  begin
    if FFontAPI.GetBitmapPixel(Trunc(Args[0]), Trunc(Args[1]), Trunc(Args[2])) then
      Result := 1
    else
      Result := 0;
  end
  else
    Result := 0;
end;

procedure TfrmScriptManager.API_SetBitmapPixel(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 4) and Assigned(FFontAPI) then
    FFontAPI.SetBitmapPixel(Trunc(Args[0]), Trunc(Args[1]), Trunc(Args[2]), Args[3] <> 0);
end;

procedure TfrmScriptManager.API_CopyBitmapToChar(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 2) and Assigned(FFontAPI) then
    FFontAPI.CopyBitmapToChar(Trunc(Args[0]), Trunc(Args[1]));
end;

procedure TfrmScriptManager.API_CopyCharToBitmap(const Args: array of Double; const StrArgs: array of string);
begin
  if (Length(Args) >= 2) and Assigned(FFontAPI) then
    FFontAPI.CopyCharToBitmap(Trunc(Args[0]), Trunc(Args[1]));
end;

function TfrmScriptManager.API_GetFontName(const Args: array of Double; const StrArgs: array of string): string;
begin
  if Assigned(FFontAPI) then
    Result := FFontAPI.GetFontName
  else
    Result := '';
end;

function TfrmScriptManager.API_InputString(const Args: array of Double; const StrArgs: array of string): string;
var
  Prompt, Default: string;
begin
  Prompt := 'Enter text';
  Default := '';
  if Length(StrArgs) >= 1 then
    Prompt := StrArgs[0];
  if Length(StrArgs) >= 2 then
    Default := StrArgs[1];
  
  if Assigned(FFontAPI) then
    Result := FFontAPI.InputString(Prompt, Default)
  else
    Result := Default;
end;

procedure TfrmScriptManager.RunScript(const Code: string);
begin
  memoOutput.Clear;
  btnRun.Enabled := False;
  btnStop.Enabled := True;
  StatusBar.Panels[0].Text := 'Running...';
  
  try
    FInterpreter.Reset;
    
    // Register Font API functions
    FInterpreter.RegisterFunction('GETPIXEL', 3, 3, @API_GetPixel);
    FInterpreter.RegisterFunction('GETCHARWIDTH', 1, 1, @API_GetCharWidth);
    FInterpreter.RegisterFunction('GETFONTHEIGHT', 0, 0, @API_GetFontHeight);
    FInterpreter.RegisterFunction('GETCURRENTCHAR', 0, 0, @API_GetCurrentChar);
    FInterpreter.RegisterFunction('GETRANGESTART', 0, 0, @API_GetRangeStart);
    FInterpreter.RegisterFunction('GETRANGEEND', 0, 0, @API_GetRangeEnd);
    FInterpreter.RegisterFunction('ISCHAREMPTY', 1, 1, @API_IsCharEmpty);
    FInterpreter.RegisterFunction('INPUTNUMBER', 1, 2, @API_InputNumber);
    FInterpreter.RegisterFunction('CONFIRM', 1, 1, @API_Confirm);
    FInterpreter.RegisterFunction('CREATEBITMAP', 2, 2, @API_CreateBitmap);
    FInterpreter.RegisterFunction('GETBITMAPPIXEL', 3, 3, @API_GetBitmapPixel);
    
    // Procedures
    FInterpreter.RegisterProcedure('SETPIXEL', 4, 4, @API_SetPixel);
    FInterpreter.RegisterProcedure('SETCHARWIDTH', 2, 2, @API_SetCharWidth);
    FInterpreter.RegisterProcedure('CLEARCHAR', 1, 1, @API_ClearChar);
    FInterpreter.RegisterProcedure('COPYCHAR', 2, 2, @API_CopyChar);
    FInterpreter.RegisterProcedure('FLIPCHARH', 1, 1, @API_FlipCharH);
    FInterpreter.RegisterProcedure('FLIPCHARV', 1, 1, @API_FlipCharV);
    FInterpreter.RegisterProcedure('INVERTCHAR', 1, 1, @API_InvertChar);
    FInterpreter.RegisterProcedure('SHIFTCHAR', 3, 3, @API_ShiftChar);
    FInterpreter.RegisterProcedure('SAVEUNDO', 1, 1, @API_SaveUndo);
    FInterpreter.RegisterProcedure('SELECTCHAR', 1, 1, @API_SelectChar);
    FInterpreter.RegisterProcedure('REFRESH', 0, 0, @API_Refresh);
    FInterpreter.RegisterProcedure('SHOWMESSAGE', 1, 1, @API_ShowMessage);
    FInterpreter.RegisterProcedure('FREEBITMAP', 1, 1, @API_FreeBitmap);
    FInterpreter.RegisterProcedure('SETBITMAPPIXEL', 4, 4, @API_SetBitmapPixel);
    FInterpreter.RegisterProcedure('COPYBITMAPTOCHAR', 2, 2, @API_CopyBitmapToChar);
    FInterpreter.RegisterProcedure('COPYCHARTOBITMAP', 2, 2, @API_CopyCharToBitmap);
    
    // String functions
    FInterpreter.RegisterStringFunction('GETFONTNAME$', 0, 0, @API_GetFontName);
    FInterpreter.RegisterStringFunction('INPUTSTRING$', 1, 2, @API_InputString);
    
    FInterpreter.LoadProgram(Code);
    FInterpreter.Execute;
    
    StatusBar.Panels[0].Text := 'Script completed';
  except
    on E: EQBasicError do
    begin
      memoOutput.Lines.Add('');
      memoOutput.Lines.Add('ERROR at line ' + IntToStr(E.Line) + ': ' + E.Message);
      StatusBar.Panels[0].Text := 'Error at line ' + IntToStr(E.Line);
      synEdit.CaretY := E.Line;
    end;
    on E: Exception do
    begin
      memoOutput.Lines.Add('');
      memoOutput.Lines.Add('ERROR: ' + E.Message);
      StatusBar.Panels[0].Text := 'Error: ' + E.Message;
    end;
  end;
  
  btnRun.Enabled := True;
  btnStop.Enabled := False;
end;

procedure TfrmScriptManager.RunScriptFile(const FileName: string);
var
  Code: TStringList;
begin
  Code := TStringList.Create;
  try
    Code.LoadFromFile(FileName);
    RunScript(Code.Text);
  finally
    Code.Free;
  end;
end;

end.
