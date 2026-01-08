{$MODE OBJFPC}{$H+}
unit FontScript;

{
  FontScript Unit - QBasic Scripting API for Bitmap Font Editor

  This unit provides the bridge between the QBasic interpreter and the
  font editor. It exposes font manipulation functions to scripts.

  Usage in scripts:
    ' Get/Set pixels
    value = GetPixel(charCode, x, y)
    SetPixel charCode, x, y, value

    ' Character properties
    w = GetCharWidth(charCode)
    SetCharWidth charCode, newWidth

    ' Font properties
    h = GetFontHeight()
    name$ = GetFontName$()

    ' Editor control
    Refresh
    SaveUndo charCode
    SelectChar charCode

    ' User interaction
    n = InputNumber("Prompt", defaultValue)
    s$ = InputString$("Prompt", "default")
    ok = Confirm("Are you sure?")
    ShowMessage "Done!"
}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Forms, Controls;

type
  { Forward declaration }
  TFontScriptAPI = class;

  { Callback types for editor integration }
  TGetPixelFunc = function(CharCode, X, Y: Integer): Boolean of object;
  TSetPixelProc = procedure(CharCode, X, Y: Integer; Value: Boolean) of object;
  TGetCharWidthFunc = function(CharCode: Integer): Integer of object;
  TSetCharWidthProc = procedure(CharCode, Width: Integer) of object;
  TGetCharBitmapFunc = function(CharCode: Integer): TBitmap of object;
  TSetCharBitmapProc = procedure(CharCode: Integer; Bmp: TBitmap) of object;
  TRefreshProc = procedure of object;
  TSaveUndoProc = procedure(CharCode: Integer) of object;
  TSelectCharProc = procedure(CharCode: Integer) of object;
  TMarkModifiedProc = procedure of object;
  TGetCurrentCharFunc = function: Integer of object;
  TGetFontHeightFunc = function: Integer of object;
  TGetFontNameFunc = function: string of object;
  TSetFontNameProc = procedure(const Name: string) of object;
  TGetRangeStartFunc = function: Integer of object;
  TGetRangeEndFunc = function: Integer of object;
  TSetRangeProc = procedure(StartChar, EndChar: Integer) of object;
  TPrintProc = procedure(const Msg: string) of object;

  { Script metadata parsed from comments }
  TScriptMetadata = record
    Name: string;
    Author: string;
    Description: string;
    Hotkey: string;
    Menu: string;
  end;

  { Temporary bitmap for script workspace }
  TScriptBitmap = class
  private
    FBitmap: TBitmap;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
    function GetPixel(X, Y: Integer): Boolean;
    procedure SetPixel(X, Y: Integer; Value: Boolean);
    procedure Clear;
    procedure Fill(Value: Boolean);
    procedure CopyFrom(Source: TScriptBitmap);
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Bitmap: TBitmap read FBitmap;
  end;

  { Main API class }
  TFontScriptAPI = class
  private
    FTempBitmaps: TList;
    FOutput: TStrings;

    { Editor callbacks }
    FOnGetPixel: TGetPixelFunc;
    FOnSetPixel: TSetPixelProc;
    FOnGetCharWidth: TGetCharWidthFunc;
    FOnSetCharWidth: TSetCharWidthProc;
    FOnGetCharBitmap: TGetCharBitmapFunc;
    FOnSetCharBitmap: TSetCharBitmapProc;
    FOnRefresh: TRefreshProc;
    FOnSaveUndo: TSaveUndoProc;
    FOnSelectChar: TSelectCharProc;
    FOnMarkModified: TMarkModifiedProc;
    FOnGetCurrentChar: TGetCurrentCharFunc;
    FOnGetFontHeight: TGetFontHeightFunc;
    FOnGetFontName: TGetFontNameFunc;
    FOnSetFontName: TSetFontNameProc;
    FOnGetRangeStart: TGetRangeStartFunc;
    FOnGetRangeEnd: TGetRangeEndFunc;
    FOnSetRange: TSetRangeProc;
    FOnPrint: TPrintProc;

  public
    constructor Create;
    destructor Destroy; override;

    { Pixel operations }
    function GetPixel(CharCode, X, Y: Integer): Boolean;
    procedure SetPixel(CharCode, X, Y: Integer; Value: Boolean);

    { Character operations }
    function GetCharWidth(CharCode: Integer): Integer;
    procedure SetCharWidth(CharCode, Width: Integer);
    function GetCharBitmap(CharCode: Integer): TBitmap;
    procedure SetCharBitmap(CharCode: Integer; Bmp: TBitmap);
    procedure ClearChar(CharCode: Integer);
    function IsCharEmpty(CharCode: Integer): Boolean;
    procedure CopyChar(FromChar, ToChar: Integer);
    procedure SwapChars(Char1, Char2: Integer);

    { Character transformations }
    procedure FlipCharH(CharCode: Integer);
    procedure FlipCharV(CharCode: Integer);
    procedure InvertChar(CharCode: Integer);
    procedure ShiftChar(CharCode, DX, DY: Integer);

    { Font properties }
    function GetFontHeight: Integer;
    function GetFontName: string;
    procedure SetFontName(const Name: string);
    function GetCurrentChar: Integer;
    function GetRangeStart: Integer;
    function GetRangeEnd: Integer;

    { Editor control }
    procedure Refresh;
    procedure SaveUndo(CharCode: Integer);
    procedure SelectChar(CharCode: Integer);
    procedure SetRange(StartChar, EndChar: Integer);
    procedure MarkModified;

    { Temporary bitmaps }
    function CreateBitmap(Width, Height: Integer): Integer;
    procedure FreeBitmap(Handle: Integer);
    function GetBitmapPixel(Handle, X, Y: Integer): Boolean;
    procedure SetBitmapPixel(Handle, X, Y: Integer; Value: Boolean);
    procedure ClearBitmap(Handle: Integer);
    function GetBitmapWidth(Handle: Integer): Integer;
    function GetBitmapHeight(Handle: Integer): Integer;
    procedure CopyBitmapToChar(Handle, CharCode: Integer);
    procedure CopyCharToBitmap(CharCode, Handle: Integer);

    { User interaction }
    function InputNumber(const Prompt: string; Default: Integer): Integer;
    function InputString(const Prompt, Default: string): string;
    function InputChar(const Prompt: string): Integer;
    function Confirm(const Prompt: string): Boolean;
    procedure ShowMessage(const Msg: string);
    procedure Print(const Msg: string);

    { Utility }
    function Min(A, B: Integer): Integer;
    function Max(A, B: Integer): Integer;
    function Clamp(Value, MinVal, MaxVal: Integer): Integer;

    { Output access }
    property Output: TStrings read FOutput write FOutput;

    { Editor callbacks }
    property OnGetPixel: TGetPixelFunc read FOnGetPixel write FOnGetPixel;
    property OnSetPixel: TSetPixelProc read FOnSetPixel write FOnSetPixel;
    property OnGetCharWidth: TGetCharWidthFunc read FOnGetCharWidth write FOnGetCharWidth;
    property OnSetCharWidth: TSetCharWidthProc read FOnSetCharWidth write FOnSetCharWidth;
    property OnGetCharBitmap: TGetCharBitmapFunc read FOnGetCharBitmap write FOnGetCharBitmap;
    property OnSetCharBitmap: TSetCharBitmapProc read FOnSetCharBitmap write FOnSetCharBitmap;
    property OnRefresh: TRefreshProc read FOnRefresh write FOnRefresh;
    property OnSaveUndo: TSaveUndoProc read FOnSaveUndo write FOnSaveUndo;
    property OnSelectChar: TSelectCharProc read FOnSelectChar write FOnSelectChar;
    property OnMarkModified: TMarkModifiedProc read FOnMarkModified write FOnMarkModified;
    property OnGetCurrentChar: TGetCurrentCharFunc read FOnGetCurrentChar write FOnGetCurrentChar;
    property OnGetFontHeight: TGetFontHeightFunc read FOnGetFontHeight write FOnGetFontHeight;
    property OnGetFontName: TGetFontNameFunc read FOnGetFontName write FOnGetFontName;
    property OnSetFontName: TSetFontNameProc read FOnSetFontName write FOnSetFontName;
    property OnGetRangeStart: TGetRangeStartFunc read FOnGetRangeStart write FOnGetRangeStart;
    property OnGetRangeEnd: TGetRangeEndFunc read FOnGetRangeEnd write FOnGetRangeEnd;
    property OnSetRange: TSetRangeProc read FOnSetRange write FOnSetRange;
    property OnPrint: TPrintProc read FOnPrint write FOnPrint;
  end;

{ Helper functions }
function ParseScriptMetadata(const ScriptText: string): TScriptMetadata;

implementation

{ TScriptBitmap }

constructor TScriptBitmap.Create(AWidth, AHeight: Integer);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FBitmap := TBitmap.Create;
  FBitmap.Width := AWidth;
  FBitmap.Height := AHeight;
  FBitmap.PixelFormat := pf24bit;
  Clear;
end;

destructor TScriptBitmap.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

function TScriptBitmap.GetPixel(X, Y: Integer): Boolean;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
    Result := FBitmap.Canvas.Pixels[X, Y] = clBlack
  else
    Result := False;
end;

procedure TScriptBitmap.SetPixel(X, Y: Integer; Value: Boolean);
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then
  begin
    if Value then
      FBitmap.Canvas.Pixels[X, Y] := clBlack
    else
      FBitmap.Canvas.Pixels[X, Y] := clWhite;
  end;
end;

procedure TScriptBitmap.Clear;
begin
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(0, 0, FWidth, FHeight);
end;

procedure TScriptBitmap.Fill(Value: Boolean);
begin
  if Value then
    FBitmap.Canvas.Brush.Color := clBlack
  else
    FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(0, 0, FWidth, FHeight);
end;

procedure TScriptBitmap.CopyFrom(Source: TScriptBitmap);
begin
  if Source <> nil then
    FBitmap.Canvas.Draw(0, 0, Source.Bitmap);
end;

{ TFontScriptAPI }

constructor TFontScriptAPI.Create;
begin
  inherited Create;
  FTempBitmaps := TList.Create;
  FOutput := nil;
end;

destructor TFontScriptAPI.Destroy;
var
  i: Integer;
begin
  for i := 0 to FTempBitmaps.Count - 1 do
    if FTempBitmaps[i] <> nil then
      TScriptBitmap(FTempBitmaps[i]).Free;
  FTempBitmaps.Free;
  inherited Destroy;
end;

{ Pixel operations }

function TFontScriptAPI.GetPixel(CharCode, X, Y: Integer): Boolean;
begin
  if Assigned(FOnGetPixel) then
    Result := FOnGetPixel(CharCode, X, Y)
  else
    Result := False;
end;

procedure TFontScriptAPI.SetPixel(CharCode, X, Y: Integer; Value: Boolean);
begin
  if Assigned(FOnSetPixel) then
    FOnSetPixel(CharCode, X, Y, Value);
end;

{ Character operations }

function TFontScriptAPI.GetCharWidth(CharCode: Integer): Integer;
begin
  if Assigned(FOnGetCharWidth) then
    Result := FOnGetCharWidth(CharCode)
  else
    Result := 0;
end;

procedure TFontScriptAPI.SetCharWidth(CharCode, Width: Integer);
begin
  if Assigned(FOnSetCharWidth) then
    FOnSetCharWidth(CharCode, Width);
end;

function TFontScriptAPI.GetCharBitmap(CharCode: Integer): TBitmap;
begin
  if Assigned(FOnGetCharBitmap) then
    Result := FOnGetCharBitmap(CharCode)
  else
    Result := nil;
end;

procedure TFontScriptAPI.SetCharBitmap(CharCode: Integer; Bmp: TBitmap);
begin
  if Assigned(FOnSetCharBitmap) then
    FOnSetCharBitmap(CharCode, Bmp);
end;

procedure TFontScriptAPI.ClearChar(CharCode: Integer);
var
  X, Y, W, H: Integer;
begin
  W := GetCharWidth(CharCode);
  H := GetFontHeight;
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetPixel(CharCode, X, Y, False);
end;

function TFontScriptAPI.IsCharEmpty(CharCode: Integer): Boolean;
var
  X, Y, W, H: Integer;
begin
  Result := True;
  W := GetCharWidth(CharCode);
  H := GetFontHeight;
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      if GetPixel(CharCode, X, Y) then
      begin
        Result := False;
        Exit;
      end;
end;

procedure TFontScriptAPI.CopyChar(FromChar, ToChar: Integer);
var
  X, Y, W, H: Integer;
begin
  W := GetCharWidth(FromChar);
  H := GetFontHeight;
  SetCharWidth(ToChar, W);
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetPixel(ToChar, X, Y, GetPixel(FromChar, X, Y));
end;

procedure TFontScriptAPI.SwapChars(Char1, Char2: Integer);
var
  Handle: Integer;
  W1, W2: Integer;
begin
  W1 := GetCharWidth(Char1);
  W2 := GetCharWidth(Char2);

  // Copy Char1 to temp
  Handle := CreateBitmap(Max(W1, W2), GetFontHeight);
  CopyCharToBitmap(Char1, Handle);

  // Copy Char2 to Char1
  CopyChar(Char2, Char1);

  // Copy temp to Char2
  CopyBitmapToChar(Handle, Char2);
  SetCharWidth(Char2, W1);

  FreeBitmap(Handle);
end;

{ Character transformations }

procedure TFontScriptAPI.FlipCharH(CharCode: Integer);
var
  X, Y, W, H: Integer;
  Handle: Integer;
begin
  W := GetCharWidth(CharCode);
  H := GetFontHeight;
  Handle := CreateBitmap(W, H);

  // Copy flipped
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetBitmapPixel(Handle, W - 1 - X, Y, GetPixel(CharCode, X, Y));

  CopyBitmapToChar(Handle, CharCode);
  FreeBitmap(Handle);
end;

procedure TFontScriptAPI.FlipCharV(CharCode: Integer);
var
  X, Y, W, H: Integer;
  Handle: Integer;
begin
  W := GetCharWidth(CharCode);
  H := GetFontHeight;
  Handle := CreateBitmap(W, H);

  // Copy flipped
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetBitmapPixel(Handle, X, H - 1 - Y, GetPixel(CharCode, X, Y));

  CopyBitmapToChar(Handle, CharCode);
  FreeBitmap(Handle);
end;

procedure TFontScriptAPI.InvertChar(CharCode: Integer);
var
  X, Y, W, H: Integer;
begin
  W := GetCharWidth(CharCode);
  H := GetFontHeight;
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
      SetPixel(CharCode, X, Y, not GetPixel(CharCode, X, Y));
end;

procedure TFontScriptAPI.ShiftChar(CharCode, DX, DY: Integer);
var
  X, Y, W, H: Integer;
  Handle: Integer;
begin
  W := GetCharWidth(CharCode);
  H := GetFontHeight;
  Handle := CreateBitmap(W, H);

  // Copy shifted
  for Y := 0 to H - 1 do
    for X := 0 to W - 1 do
    begin
      if (X - DX >= 0) and (X - DX < W) and (Y - DY >= 0) and (Y - DY < H) then
        SetBitmapPixel(Handle, X, Y, GetPixel(CharCode, X - DX, Y - DY))
      else
        SetBitmapPixel(Handle, X, Y, False);
    end;

  CopyBitmapToChar(Handle, CharCode);
  FreeBitmap(Handle);
end;

{ Font properties }

function TFontScriptAPI.GetFontHeight: Integer;
begin
  if Assigned(FOnGetFontHeight) then
    Result := FOnGetFontHeight()
  else
    Result := 16;
end;

function TFontScriptAPI.GetFontName: string;
begin
  if Assigned(FOnGetFontName) then
    Result := FOnGetFontName()
  else
    Result := '';
end;

procedure TFontScriptAPI.SetFontName(const Name: string);
begin
  if Assigned(FOnSetFontName) then
    FOnSetFontName(Name);
end;

function TFontScriptAPI.GetCurrentChar: Integer;
begin
  if Assigned(FOnGetCurrentChar) then
    Result := FOnGetCurrentChar()
  else
    Result := 65;
end;

function TFontScriptAPI.GetRangeStart: Integer;
begin
  if Assigned(FOnGetRangeStart) then
    Result := FOnGetRangeStart()
  else
    Result := 32;
end;

function TFontScriptAPI.GetRangeEnd: Integer;
begin
  if Assigned(FOnGetRangeEnd) then
    Result := FOnGetRangeEnd()
  else
    Result := 127;
end;

{ Editor control }

procedure TFontScriptAPI.Refresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh();
end;

procedure TFontScriptAPI.SaveUndo(CharCode: Integer);
begin
  if Assigned(FOnSaveUndo) then
    FOnSaveUndo(CharCode);
end;

procedure TFontScriptAPI.SelectChar(CharCode: Integer);
begin
  if Assigned(FOnSelectChar) then
    FOnSelectChar(CharCode);
end;

procedure TFontScriptAPI.SetRange(StartChar, EndChar: Integer);
begin
  if Assigned(FOnSetRange) then
    FOnSetRange(StartChar, EndChar);
end;

procedure TFontScriptAPI.MarkModified;
begin
  if Assigned(FOnMarkModified) then
    FOnMarkModified();
end;

{ Temporary bitmaps }

function TFontScriptAPI.CreateBitmap(Width, Height: Integer): Integer;
var
  Bmp: TScriptBitmap;
  i: Integer;
begin
  Bmp := TScriptBitmap.Create(Width, Height);

  // Find empty slot or add new
  for i := 0 to FTempBitmaps.Count - 1 do
    if FTempBitmaps[i] = nil then
    begin
      FTempBitmaps[i] := Bmp;
      Result := i;
      Exit;
    end;

  Result := FTempBitmaps.Add(Bmp);
end;

procedure TFontScriptAPI.FreeBitmap(Handle: Integer);
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
  begin
    TScriptBitmap(FTempBitmaps[Handle]).Free;
    FTempBitmaps[Handle] := nil;
  end;
end;

function TFontScriptAPI.GetBitmapPixel(Handle, X, Y: Integer): Boolean;
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
    Result := TScriptBitmap(FTempBitmaps[Handle]).GetPixel(X, Y)
  else
    Result := False;
end;

procedure TFontScriptAPI.SetBitmapPixel(Handle, X, Y: Integer; Value: Boolean);
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
    TScriptBitmap(FTempBitmaps[Handle]).SetPixel(X, Y, Value);
end;

procedure TFontScriptAPI.ClearBitmap(Handle: Integer);
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
    TScriptBitmap(FTempBitmaps[Handle]).Clear;
end;

function TFontScriptAPI.GetBitmapWidth(Handle: Integer): Integer;
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
    Result := TScriptBitmap(FTempBitmaps[Handle]).Width
  else
    Result := 0;
end;

function TFontScriptAPI.GetBitmapHeight(Handle: Integer): Integer;
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
    Result := TScriptBitmap(FTempBitmaps[Handle]).Height
  else
    Result := 0;
end;

procedure TFontScriptAPI.CopyBitmapToChar(Handle, CharCode: Integer);
var
  X, Y, W, H: Integer;
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
  begin
    W := TScriptBitmap(FTempBitmaps[Handle]).Width;
    H := TScriptBitmap(FTempBitmaps[Handle]).Height;
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
        SetPixel(CharCode, X, Y, GetBitmapPixel(Handle, X, Y));
  end;
end;

procedure TFontScriptAPI.CopyCharToBitmap(CharCode, Handle: Integer);
var
  X, Y, W, H: Integer;
begin
  if (Handle >= 0) and (Handle < FTempBitmaps.Count) and (FTempBitmaps[Handle] <> nil) then
  begin
    W := GetCharWidth(CharCode);
    H := GetFontHeight;
    for Y := 0 to H - 1 do
      for X := 0 to W - 1 do
        SetBitmapPixel(Handle, X, Y, GetPixel(CharCode, X, Y));
  end;
end;

{ User interaction }

function TFontScriptAPI.InputNumber(const Prompt: string; Default: Integer): Integer;
var
  S: string;
begin
  S := IntToStr(Default);
  if InputQuery('Script Input', Prompt, S) then
    Result := StrToIntDef(S, Default)
  else
    Result := Default;
end;

function TFontScriptAPI.InputString(const Prompt, Default: string): string;
begin
  Result := Default;
  InputQuery('Script Input', Prompt, Result);
end;

function TFontScriptAPI.InputChar(const Prompt: string): Integer;
var
  S: string;
begin
  S := 'A';
  if InputQuery('Script Input', Prompt, S) and (Length(S) > 0) then
    Result := Ord(S[1])
  else
    Result := 65;
end;

function TFontScriptAPI.Confirm(const Prompt: string): Boolean;
begin
  Result := MessageDlg(Prompt, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;

procedure TFontScriptAPI.ShowMessage(const Msg: string);
begin
  Dialogs.ShowMessage(Msg);
end;

procedure TFontScriptAPI.Print(const Msg: string);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Msg)
  else if Assigned(FOutput) then
    FOutput.Add(Msg);
end;

{ Utility }

function TFontScriptAPI.Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function TFontScriptAPI.Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

function TFontScriptAPI.Clamp(Value, MinVal, MaxVal: Integer): Integer;
begin
  if Value < MinVal then Result := MinVal
  else if Value > MaxVal then Result := MaxVal
  else Result := Value;
end;

{ Helper functions }

function ParseScriptMetadata(const ScriptText: string): TScriptMetadata;
var
  Lines: TStringList;
  i: Integer;
  Line, Key, Value: string;
  P: Integer;
begin
  Result.Name := '';
  Result.Author := '';
  Result.Description := '';
  Result.Hotkey := '';
  Result.Menu := '';

  Lines := TStringList.Create;
  try
    Lines.Text := ScriptText;
    for i := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[i]);
      if (Length(Line) > 2) and (Line[1] = '''') and (Line[2] = '#') then
      begin
        // Metadata line: '#KEY: Value
        Line := Copy(Line, 3, Length(Line) - 2);
        P := Pos(':', Line);
        if P > 0 then
        begin
          Key := UpperCase(Trim(Copy(Line, 1, P - 1)));
          Value := Trim(Copy(Line, P + 1, Length(Line)));

          if Key = 'NAME' then Result.Name := Value
          else if Key = 'AUTHOR' then Result.Author := Value
          else if Key = 'DESCRIPTION' then Result.Description := Value
          else if Key = 'HOTKEY' then Result.Hotkey := Value
          else if Key = 'MENU' then Result.Menu := Value;
        end;
      end
      else if (Length(Line) > 0) and (Line[1] <> '''') then
        Break; // Stop at first non-comment line
    end;
  finally
    Lines.Free;
  end;
end;

end.
