{
  DPFont.pas - Deluxe Paint IIe DOS Font Format Unit
  
  Supports loading and saving Deluxe Paint IIe fonts (.M8, .PCL)
  Based on HP PCL soft font format with big-endian values.
  
  File Structure:
    ESC )s <size> W          - File header (6 bytes) + font descriptor + name
    For each glyph:
      ESC *c <code> E        - Character code
      ESC (s <size> W        - Data size (including 16-byte header)
      [16 bytes]             - Glyph header
      [N bytes]              - Bitmap data (1-bit, MSB first)
      
  Font Descriptor (64 bytes total):
    [00-01] Size of descriptor (big-endian)
    [06-07] Baseline position
    [08-09] Cell width
    [10-11] Cell height
    ...
    [48-63] Font name (16 bytes)
    
  Glyph Header (16 bytes, big-endian):
    [00]    Format (4=bitmap)
    [01]    Continuation
    [02]    Descriptor size (14)
    [03]    Class (1=bitmap)
    [04]    Orientation
    [05]    Reserved
    [06-07] Left Offset (signed)
    [08-09] Top Offset (signed, baseline to top)
    [10-11] Character Width
    [12-13] Character Height
    [14-15] Delta X (advance width in quarter-pixels)
}

unit DPFont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

const
  DP_ESC = $1B;
  
type
  TDPGlyph = record
    CharCode: Integer;
    LeftOffset: SmallInt;
    TopOffset: SmallInt;
    Width: Word;
    Height: Word;
    DeltaX: Word;       // Advance width in quarter-pixels
    Data: array of Byte;
  end;
  
  TDPFont = class
  private
    FGlyphs: array of TDPGlyph;
    FFontName: string;
    FCellWidth: Word;
    FCellHeight: Word;
    FBaseline: Word;
    FNumGlyphs: Integer;
    
    function ReadBEWord(const Data: array of Byte; Offset: Integer): Word;
    function ReadBESmallInt(const Data: array of Byte; Offset: Integer): SmallInt;
    procedure WriteBEWord(Stream: TStream; Value: Word);
    function ParseDecimal(const Data: array of Byte; var Pos: Integer): Integer;
    function FindGlyph(CharCode: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadFromFile(const FileName: string): Boolean;
    function SaveToFile(const FileName: string): Boolean;
    
    function GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
    function GetGlyphBitmapInCell(CharCode: Integer; Bmp: TBitmap; CellH: Integer): Boolean;
    procedure SetGlyphFromBitmap(CharCode: Integer; Bmp: TBitmap; DeltaX: Word = 0);
    function GetCharWidth(CharCode: Integer): Integer;
    function GetCharAdvance(CharCode: Integer): Integer;
    function GetGlyphTopOffset(CharCode: Integer): SmallInt;
    function GetGlyphLeftOffset(CharCode: Integer): SmallInt;
    function GetGlyphHeight(CharCode: Integer): Integer;
    
    procedure Clear;
    
    property FontName: string read FFontName write FFontName;
    property CellWidth: Word read FCellWidth write FCellWidth;
    property CellHeight: Word read FCellHeight write FCellHeight;
    property Baseline: Word read FBaseline write FBaseline;
    property NumGlyphs: Integer read FNumGlyphs;
  end;

implementation

constructor TDPFont.Create;
begin
  inherited Create;
  FFontName := 'DPFONT';
  FCellWidth := 8;
  FCellHeight := 8;
  FBaseline := 6;
  FNumGlyphs := 0;
  SetLength(FGlyphs, 0);
end;

destructor TDPFont.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDPFont.Clear;
var
  I: Integer;
begin
  for I := 0 to High(FGlyphs) do
    SetLength(FGlyphs[I].Data, 0);
  SetLength(FGlyphs, 0);
  FNumGlyphs := 0;
end;

function TDPFont.ReadBEWord(const Data: array of Byte; Offset: Integer): Word;
begin
  Result := (Data[Offset] shl 8) or Data[Offset + 1];
end;

function TDPFont.ReadBESmallInt(const Data: array of Byte; Offset: Integer): SmallInt;
begin
  Result := SmallInt((Data[Offset] shl 8) or Data[Offset + 1]);
end;

procedure TDPFont.WriteBEWord(Stream: TStream; Value: Word);
var
  B: array[0..1] of Byte;
begin
  B[0] := (Value shr 8) and $FF;
  B[1] := Value and $FF;
  Stream.WriteBuffer(B, 2);
end;

function TDPFont.ParseDecimal(const Data: array of Byte; var Pos: Integer): Integer;
begin
  Result := 0;
  while (Pos <= High(Data)) and (Data[Pos] >= Ord('0')) and (Data[Pos] <= Ord('9')) do
  begin
    Result := Result * 10 + (Data[Pos] - Ord('0'));
    Inc(Pos);
  end;
end;

function TDPFont.FindGlyph(CharCode: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FGlyphs) do
    if FGlyphs[I].CharCode = CharCode then
    begin
      Result := I;
      Exit;
    end;
end;

function TDPFont.LoadFromFile(const FileName: string): Boolean;
var
  FS: TFileStream;
  FileData: array of Byte;
  FileSize: Int64;
  Pos, CharCode, DataSize: Integer;
  FontDescSize: Integer;
  GlyphHeader: array[0..15] of Byte;
  GlyphIdx: Integer;
begin
  Result := False;
  Clear;
  
  if not FileExists(FileName) then Exit;
  
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FileSize := FS.Size;
    if FileSize < 70 then Exit; // Minimum size for header + font descriptor
    
    SetLength(FileData, FileSize);
    FS.ReadBuffer(FileData[0], FileSize);
  finally
    FS.Free;
  end;
  
  Pos := 0;
  
  // Check file header: ESC )s <size> W
  if (FileData[0] <> DP_ESC) or (FileData[1] <> Ord(')')) or (FileData[2] <> Ord('s')) then
    Exit;
  
  Pos := 3;
  FontDescSize := ParseDecimal(FileData, Pos);
  if (Pos > High(FileData)) or (FileData[Pos] <> Ord('W')) then Exit;
  Inc(Pos);
  
  // Read font descriptor (48 bytes) + font name (16 bytes)
  if Pos + FontDescSize > Length(FileData) then Exit;
  
  // Parse font descriptor fields (big-endian)
  FBaseline := ReadBEWord(FileData, Pos + 6);
  FCellWidth := ReadBEWord(FileData, Pos + 8);
  FCellHeight := ReadBEWord(FileData, Pos + 10);
  
  // Font name is at descriptor + 48, up to 16 bytes
  if FontDescSize >= 64 then
  begin
    FFontName := '';
    for GlyphIdx := 0 to 15 do
    begin
      if FileData[Pos + 48 + GlyphIdx] = 0 then Break;
      FFontName := FFontName + Chr(FileData[Pos + 48 + GlyphIdx]);
    end;
    FFontName := Trim(FFontName);
  end;
  
  Pos := Pos + FontDescSize;
  
  // Parse glyphs
  while Pos < Length(FileData) - 10 do
  begin
    // Look for ESC *c <code> E (character code)
    if (FileData[Pos] <> DP_ESC) or (FileData[Pos + 1] <> Ord('*')) or (FileData[Pos + 2] <> Ord('c')) then
      Break;
    
    Pos := Pos + 3;
    CharCode := ParseDecimal(FileData, Pos);
    if (Pos > High(FileData)) or (FileData[Pos] <> Ord('E')) then Break;
    Inc(Pos);
    
    // Look for ESC (s <size> W (data size)
    if (Pos + 3 > High(FileData)) then Break;
    if (FileData[Pos] <> DP_ESC) or (FileData[Pos + 1] <> Ord('(')) or (FileData[Pos + 2] <> Ord('s')) then
      Break;
    
    Pos := Pos + 3;
    DataSize := ParseDecimal(FileData, Pos);
    if (Pos > High(FileData)) or (FileData[Pos] <> Ord('W')) then Break;
    Inc(Pos);
    
    // Read 16-byte glyph header
    if Pos + 16 > Length(FileData) then Break;
    Move(FileData[Pos], GlyphHeader, 16);
    
    // Add glyph
    SetLength(FGlyphs, Length(FGlyphs) + 1);
    GlyphIdx := High(FGlyphs);
    
    FGlyphs[GlyphIdx].CharCode := CharCode;
    FGlyphs[GlyphIdx].LeftOffset := ReadBESmallInt(GlyphHeader, 6);
    FGlyphs[GlyphIdx].TopOffset := ReadBESmallInt(GlyphHeader, 8);
    FGlyphs[GlyphIdx].Width := ReadBEWord(GlyphHeader, 10);
    FGlyphs[GlyphIdx].Height := ReadBEWord(GlyphHeader, 12);
    FGlyphs[GlyphIdx].DeltaX := ReadBEWord(GlyphHeader, 14);
    
    // Read bitmap data
    if DataSize > 16 then
    begin
      SetLength(FGlyphs[GlyphIdx].Data, DataSize - 16);
      Move(FileData[Pos + 16], FGlyphs[GlyphIdx].Data[0], DataSize - 16);
    end;
    
    Pos := Pos + DataSize;
  end;
  
  FNumGlyphs := Length(FGlyphs);
  Result := FNumGlyphs > 0;
end;

function TDPFont.SaveToFile(const FileName: string): Boolean;
var
  FS: TFileStream;
  I, X, Y, ByteIdx, BitIdx: Integer;
  BytesPerRow, DataSize: Integer;
  FontDesc: array[0..63] of Byte;
  GlyphHeader: array[0..15] of Byte;
  S: string;
begin
  Result := False;
  
  FS := TFileStream.Create(FileName, fmCreate);
  try
    // Write file header: ESC )s 64 W
    S := #$1B + ')s64W';
    FS.WriteBuffer(S[1], Length(S));
    
    // Write font descriptor (64 bytes)
    FillChar(FontDesc, SizeOf(FontDesc), 0);
    
    // Size of descriptor (big-endian)
    FontDesc[0] := 0;
    FontDesc[1] := 64;
    
    // Baseline position
    FontDesc[6] := (FBaseline shr 8) and $FF;
    FontDesc[7] := FBaseline and $FF;
    
    // Cell width
    FontDesc[8] := (FCellWidth shr 8) and $FF;
    FontDesc[9] := FCellWidth and $FF;
    
    // Cell height
    FontDesc[10] := (FCellHeight shr 8) and $FF;
    FontDesc[11] := FCellHeight and $FF;
    
    // Font name (at offset 48, up to 16 bytes)
    for I := 1 to Min(16, Length(FFontName)) do
      FontDesc[47 + I] := Ord(FFontName[I]);
    
    FS.WriteBuffer(FontDesc, 64);
    
    // Write glyphs
    for I := 0 to High(FGlyphs) do
    begin
      // Character code: ESC *c <code> E
      S := #$1B + '*c' + IntToStr(FGlyphs[I].CharCode) + 'E';
      FS.WriteBuffer(S[1], Length(S));
      
      // Calculate data size
      BytesPerRow := (FGlyphs[I].Width + 7) div 8;
      DataSize := 16 + BytesPerRow * FGlyphs[I].Height;
      
      // Data size: ESC (s <size> W
      S := #$1B + '(s' + IntToStr(DataSize) + 'W';
      FS.WriteBuffer(S[1], Length(S));
      
      // Glyph header
      FillChar(GlyphHeader, 16, 0);
      GlyphHeader[0] := 4;  // Format = bitmap
      GlyphHeader[2] := 14; // Descriptor size
      GlyphHeader[3] := 1;  // Class = bitmap
      
      // Left offset (big-endian signed)
      GlyphHeader[6] := (Word(FGlyphs[I].LeftOffset) shr 8) and $FF;
      GlyphHeader[7] := Word(FGlyphs[I].LeftOffset) and $FF;
      
      // Top offset (big-endian signed)
      GlyphHeader[8] := (Word(FGlyphs[I].TopOffset) shr 8) and $FF;
      GlyphHeader[9] := Word(FGlyphs[I].TopOffset) and $FF;
      
      // Width (big-endian)
      GlyphHeader[10] := (FGlyphs[I].Width shr 8) and $FF;
      GlyphHeader[11] := FGlyphs[I].Width and $FF;
      
      // Height (big-endian)
      GlyphHeader[12] := (FGlyphs[I].Height shr 8) and $FF;
      GlyphHeader[13] := FGlyphs[I].Height and $FF;
      
      // DeltaX (big-endian)
      GlyphHeader[14] := (FGlyphs[I].DeltaX shr 8) and $FF;
      GlyphHeader[15] := FGlyphs[I].DeltaX and $FF;
      
      FS.WriteBuffer(GlyphHeader, 16);
      
      // Write bitmap data
      if Length(FGlyphs[I].Data) > 0 then
        FS.WriteBuffer(FGlyphs[I].Data[0], Length(FGlyphs[I].Data));
    end;
    
    Result := True;
  finally
    FS.Free;
  end;
end;

function TDPFont.GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
var
  Idx, X, Y, ByteIdx, BitIdx, BytesPerRow: Integer;
begin
  Result := False;
  Idx := FindGlyph(CharCode);
  if Idx < 0 then Exit;
  
  if (FGlyphs[Idx].Width = 0) or (FGlyphs[Idx].Height = 0) then Exit;
  
  Bmp.Width := FGlyphs[Idx].Width;
  Bmp.Height := FGlyphs[Idx].Height;
  Bmp.Canvas.Brush.Color := clWhite;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
  
  BytesPerRow := (FGlyphs[Idx].Width + 7) div 8;
  
  for Y := 0 to FGlyphs[Idx].Height - 1 do
  begin
    for X := 0 to FGlyphs[Idx].Width - 1 do
    begin
      ByteIdx := Y * BytesPerRow + (X div 8);
      BitIdx := 7 - (X mod 8);  // MSB first
      
      if ByteIdx < Length(FGlyphs[Idx].Data) then
      begin
        if (FGlyphs[Idx].Data[ByteIdx] and (1 shl BitIdx)) <> 0 then
          Bmp.Canvas.Pixels[X, Y] := clBlack;
      end;
    end;
  end;
  
  Result := True;
end;

procedure TDPFont.SetGlyphFromBitmap(CharCode: Integer; Bmp: TBitmap; DeltaX: Word);
var
  Idx, X, Y, ByteIdx, BitIdx, BytesPerRow: Integer;
  RowByte: Byte;
begin
  Idx := FindGlyph(CharCode);
  if Idx < 0 then
  begin
    SetLength(FGlyphs, Length(FGlyphs) + 1);
    Idx := High(FGlyphs);
    FGlyphs[Idx].CharCode := CharCode;
    FGlyphs[Idx].LeftOffset := 0;
    FNumGlyphs := Length(FGlyphs);
  end;
  
  FGlyphs[Idx].Width := Bmp.Width;
  FGlyphs[Idx].Height := Bmp.Height;
  FGlyphs[Idx].TopOffset := FBaseline;
  
  // DeltaX in quarter-pixels. If not specified, use width * 4
  if DeltaX = 0 then
    FGlyphs[Idx].DeltaX := Bmp.Width * 4
  else
    FGlyphs[Idx].DeltaX := DeltaX;
  
  BytesPerRow := (Bmp.Width + 7) div 8;
  SetLength(FGlyphs[Idx].Data, BytesPerRow * Bmp.Height);
  FillChar(FGlyphs[Idx].Data[0], Length(FGlyphs[Idx].Data), 0);
  
  for Y := 0 to Bmp.Height - 1 do
  begin
    for X := 0 to Bmp.Width - 1 do
    begin
      ByteIdx := Y * BytesPerRow + (X div 8);
      BitIdx := 7 - (X mod 8);  // MSB first
      
      if Bmp.Canvas.Pixels[X, Y] = clBlack then
        FGlyphs[Idx].Data[ByteIdx] := FGlyphs[Idx].Data[ByteIdx] or (1 shl BitIdx);
    end;
  end;
end;

function TDPFont.GetCharWidth(CharCode: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := FindGlyph(CharCode);
  if Idx >= 0 then
    Result := FGlyphs[Idx].Width
  else
    Result := 0;
end;

function TDPFont.GetCharAdvance(CharCode: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := FindGlyph(CharCode);
  if Idx >= 0 then
    Result := FGlyphs[Idx].DeltaX div 4
  else
    Result := 0;
end;

function TDPFont.GetGlyphTopOffset(CharCode: Integer): SmallInt;
var
  Idx: Integer;
begin
  Idx := FindGlyph(CharCode);
  if Idx >= 0 then
    Result := FGlyphs[Idx].TopOffset
  else
    Result := 0;
end;

function TDPFont.GetGlyphLeftOffset(CharCode: Integer): SmallInt;
var
  Idx: Integer;
begin
  Idx := FindGlyph(CharCode);
  if Idx >= 0 then
    Result := FGlyphs[Idx].LeftOffset
  else
    Result := 0;
end;

function TDPFont.GetGlyphHeight(CharCode: Integer): Integer;
var
  Idx: Integer;
begin
  Idx := FindGlyph(CharCode);
  if Idx >= 0 then
    Result := FGlyphs[Idx].Height
  else
    Result := 0;
end;

function TDPFont.GetGlyphBitmapInCell(CharCode: Integer; Bmp: TBitmap; CellH: Integer): Boolean;
var
  Idx, X, Y, ByteIdx, BitIdx, BytesPerRow: Integer;
  GlyphTop: Integer;  // Y position in cell where glyph starts
begin
  Result := False;
  Idx := FindGlyph(CharCode);
  if Idx < 0 then Exit;
  
  if (FGlyphs[Idx].Width = 0) or (FGlyphs[Idx].Height = 0) then Exit;
  
  // Set bitmap to glyph width x cell height
  Bmp.Width := FGlyphs[Idx].Width;
  Bmp.Height := CellH;
  Bmp.Canvas.Brush.Color := clWhite;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
  
  // TopOffset is distance from baseline to top of glyph (positive = above baseline)
  // So glyph top in cell = Baseline - TopOffset
  GlyphTop := FBaseline - FGlyphs[Idx].TopOffset;
  
  // Clamp to valid range
  if GlyphTop < 0 then GlyphTop := 0;
  
  BytesPerRow := (FGlyphs[Idx].Width + 7) div 8;
  
  for Y := 0 to FGlyphs[Idx].Height - 1 do
  begin
    if (GlyphTop + Y >= 0) and (GlyphTop + Y < CellH) then
    begin
      for X := 0 to FGlyphs[Idx].Width - 1 do
      begin
        ByteIdx := Y * BytesPerRow + (X div 8);
        BitIdx := 7 - (X mod 8);  // MSB first
        
        if ByteIdx < Length(FGlyphs[Idx].Data) then
        begin
          if (FGlyphs[Idx].Data[ByteIdx] and (1 shl BitIdx)) <> 0 then
            Bmp.Canvas.Pixels[X, GlyphTop + Y] := clBlack;
        end;
      end;
    end;
  end;
  
  Result := True;
end;

end.
