{$MODE OBJFPC}{$H+}
{$R-} // Disable range checking in this unit - we handle it manually
{
  Windows FON/FNT Font Reader Library for Free Pascal
  
  Supports:
  - Windows 2.x/3.x NE format FON files
  - Both raster (bitmap) and vector (stroke) fonts
  - Scalable rendering for vector fonts
  
  Usage:
    uses WinFont, PTCGraph;
    
    var
      Font: TWinFont;
    begin
      Font := TWinFont.Create;
      if Font.LoadFromFile('ROMAN.FON') then
      begin
        Font.Scale := 2.0;  // Scale factor for vector fonts
        Font.DrawText(100, 100, 'Hello World!');
      end;
      Font.Free;
    end;
}
unit WinFont;

interface

uses
  Classes, SysUtils, Graphics, PTCGraph;

const
  MAX_GLYPHS = 256;
  MAX_STROKE_POINTS = 1024;

type
  // Stroke command types
  TStrokeCmd = (scMoveTo, scLineTo, scEnd);
  
  TStrokePoint = record
    Cmd: TStrokeCmd;
    X, Y: Integer;
  end;
  
  TGlyph = record
    Width: Integer;
    Height: Integer;
    // For raster fonts
    BitmapData: array of Byte;
    BitmapWidth: Integer;   // Width in bytes
    // For vector fonts
    StrokeData: array of TStrokePoint;
    StrokeCount: Integer;
    Defined: Boolean;
  end;
  
  TFontType = (ftUnknown, ftRaster, ftVector);
  
  TWinFont = class
  private
    FLoaded: Boolean;
    FFontType: TFontType;
    FFontName: string;
    FCopyright: string;
    FHeight: Integer;
    FAscent: Integer;
    FDescent: Integer;
    FFirstChar: Integer;
    FLastChar: Integer;
    FDefaultChar: Integer;
    FGlyphs: array[0..MAX_GLYPHS-1] of TGlyph;
    FScale: Single;
    FColor: LongWord;
    FDebugMode: Boolean;
    
    // Internal parsing
    function ReadWord(Stream: TStream): Word;
    function ReadDWord(Stream: TStream): LongWord;
    function ReadByte(Stream: TStream): Byte;
    function ReadSignedByte(Stream: TStream): ShortInt;
    function ParseNEFile(Stream: TStream): Boolean;
    function ParseFNTResource(Stream: TStream; Offset, Size: LongWord): Boolean;
    function ParseVectorGlyphs(Stream: TStream; StrokeDataBase: LongWord; CharTableOffset: LongWord): Boolean;
    function ParseRasterGlyphs(Stream: TStream; FNTOffset: LongWord; CharTableOffset: LongWord): Boolean;
    procedure DrawPixel(X, Y: Integer);
    procedure DrawLineBresenham(X1, Y1, X2, Y2: Integer);
    procedure DrawRasterChar(X, Y: Integer; CharIdx: Integer);
    procedure DebugLog(const Msg: string);
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadFromFile(const FileName: string): Boolean;
    function LoadFromStream(Stream: TStream): Boolean;
    procedure Clear;
    
    // Drawing functions
    procedure DrawChar(X, Y: Integer; C: Char);
    procedure DrawText(X, Y: Integer; const Text: string);
    function GetTextWidth(const Text: string): Integer;
    function GetCharWidth(C: Char): Integer; overload;
    function GetCharWidth(CharCode: Integer): Integer; overload;
    function GetGlyphBitmap(CharCode: Integer; Dest: TObject): Boolean;
    
    // Properties
    property Loaded: Boolean read FLoaded;
    property FontType: TFontType read FFontType;
    property FontName: string read FFontName;
    property Copyright: string read FCopyright;
    property Height: Integer read FHeight;
    property Ascent: Integer read FAscent;
    property Descent: Integer read FDescent;
    property Scale: Single read FScale write FScale;
    property Color: LongWord read FColor write FColor;
    property FirstChar: Integer read FFirstChar;
    property LastChar: Integer read FLastChar;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
  end;

implementation

constructor TWinFont.Create;
var
  I: Integer;
begin
  inherited Create;
  FLoaded := False;
  FFontType := ftUnknown;
  FScale := 1.0;
  FColor := $FFFFFF;  // White
  FHeight := 16;
  FAscent := 12;
  FDescent := 4;
  FFirstChar := 32;
  FLastChar := 127;
  FDefaultChar := 32;
  FDebugMode := False;
  
  for I := 0 to MAX_GLYPHS - 1 do
  begin
    FGlyphs[I].Defined := False;
    FGlyphs[I].Width := 8;
    FGlyphs[I].Height := 16;
    FGlyphs[I].StrokeCount := 0;
    FGlyphs[I].BitmapWidth := 1;
    SetLength(FGlyphs[I].BitmapData, 0);
    SetLength(FGlyphs[I].StrokeData, 0);
  end;
end;

destructor TWinFont.Destroy;
begin
  Clear;
  inherited;
end;

procedure TWinFont.Clear;
var
  I: Integer;
begin
  for I := 0 to MAX_GLYPHS - 1 do
  begin
    SetLength(FGlyphs[I].BitmapData, 0);
    SetLength(FGlyphs[I].StrokeData, 0);
    FGlyphs[I].Defined := False;
    FGlyphs[I].StrokeCount := 0;
  end;
  FLoaded := False;
  FFontType := ftUnknown;
  FFontName := '';
  FCopyright := '';
end;

procedure TWinFont.DebugLog(const Msg: string);
begin
  if FDebugMode then
    WriteLn(Msg);
end;

function TWinFont.ReadWord(Stream: TStream): Word;
var
  B: array[0..1] of Byte;
begin
  if Stream.Read(B, 2) < 2 then
    Result := 0
  else
    Result := B[0] or (Word(B[1]) shl 8);
end;

function TWinFont.ReadDWord(Stream: TStream): LongWord;
var
  B: array[0..3] of Byte;
begin
  if Stream.Read(B, 4) < 4 then
    Result := 0
  else
    Result := B[0] or (LongWord(B[1]) shl 8) or (LongWord(B[2]) shl 16) or (LongWord(B[3]) shl 24);
end;

function TWinFont.ReadByte(Stream: TStream): Byte;
begin
  if Stream.Read(Result, 1) < 1 then
    Result := 0;
end;

function TWinFont.ReadSignedByte(Stream: TStream): ShortInt;
var
  B: Byte;
begin
  if Stream.Read(B, 1) < 1 then
    Result := 0
  else
    Result := ShortInt(B);
end;

function TWinFont.ParseVectorGlyphs(Stream: TStream; StrokeDataBase: LongWord; CharTableOffset: LongWord): Boolean;
var
  I, CharIdx: Integer;
  GlyphOffset, GlyphWidth: Word;
  NextOffset: Word;
  StrokePos, StrokeEnd: LongWord;
  B: Byte;
  DX, DY: ShortInt;
  CurX, CurY: Integer;
  NumChars: Integer;
  StrokeIdx: Integer;
  CharOffsets: array of Word;
  J: Integer;
begin
  Result := False;
  NumChars := FLastChar - FFirstChar + 2;  // +1 for sentinel entry
  
  DebugLog(Format('Parsing %d vector glyphs', [NumChars - 1]));
  DebugLog(Format('  Char table at: $%x', [CharTableOffset]));
  DebugLog(Format('  Stroke base at: $%x', [StrokeDataBase]));
  
  // First, read all character offsets so we know boundaries
  SetLength(CharOffsets, NumChars);
  for I := 0 to NumChars - 1 do
  begin
    Stream.Position := CharTableOffset + I * 4;
    CharOffsets[I] := ReadWord(Stream);
  end;
  
  for I := 0 to NumChars - 2 do  // -2 because last is sentinel
  begin
    CharIdx := FFirstChar + I;
    if (CharIdx < 0) or (CharIdx >= MAX_GLYPHS) then Continue;
    
    // Read character table entry: 2 bytes offset, 2 bytes width
    Stream.Position := CharTableOffset + I * 4;
    GlyphOffset := ReadWord(Stream);
    GlyphWidth := ReadWord(Stream);
    
    FGlyphs[CharIdx].Width := GlyphWidth;
    FGlyphs[CharIdx].Height := FHeight;
    
    if GlyphWidth = 0 then Continue;  // No glyph data
    
    // Find next different offset to determine data size
    NextOffset := GlyphOffset;
    if I + 1 < NumChars then
    begin
      // Find the next character with a different (larger) offset
      for J := I + 1 to NumChars - 1 do
      begin
        if CharOffsets[J] > GlyphOffset then
        begin
          NextOffset := CharOffsets[J];
          Break;
        end;
      end;
    end;
    
    StrokePos := StrokeDataBase + GlyphOffset;
    if NextOffset > GlyphOffset then
      StrokeEnd := StrokeDataBase + NextOffset
    else
      StrokeEnd := StrokePos + 200;  // Fallback max size
    
    if StrokePos >= Stream.Size then Continue;
    if StrokeEnd > Stream.Size then StrokeEnd := Stream.Size;
    
    Stream.Position := StrokePos;
    
    // Initialize stroke data
    SetLength(FGlyphs[CharIdx].StrokeData, MAX_STROKE_POINTS);
    StrokeIdx := 0;
    CurX := 0;
    CurY := 0;
    
    // Parse stroke commands until we hit the boundary
    // Format:
    //   0x80 DX DY = pen up, move by (DX, DY) RELATIVE to current position
    //   DX DY = pen down, draw line by (DX, DY) relative to current position
    while (Stream.Position < StrokeEnd) and (StrokeIdx < MAX_STROKE_POINTS - 1) do
    begin
      B := ReadByte(Stream);
      
      if B = $80 then
      begin
        // Control byte - RELATIVE move (pen up)
        if Stream.Position + 1 >= StrokeEnd then Break;
        
        DX := ReadSignedByte(Stream);
        DY := ReadSignedByte(Stream);
        
        // Move RELATIVE to current position (this was the bug - was treating as absolute)
        CurX := CurX + DX;
        CurY := CurY + DY;
        FGlyphs[CharIdx].StrokeData[StrokeIdx].Cmd := scMoveTo;
        FGlyphs[CharIdx].StrokeData[StrokeIdx].X := CurX;
        FGlyphs[CharIdx].StrokeData[StrokeIdx].Y := CurY;
        Inc(StrokeIdx);
      end
      else
      begin
        // Regular delta - B is signed DX, need 1 more byte for DY
        DX := ShortInt(B);
        if Stream.Position >= StrokeEnd then Break;
        DY := ReadSignedByte(Stream);
        CurX := CurX + DX;
        CurY := CurY + DY;
        
        FGlyphs[CharIdx].StrokeData[StrokeIdx].Cmd := scLineTo;
        FGlyphs[CharIdx].StrokeData[StrokeIdx].X := CurX;
        FGlyphs[CharIdx].StrokeData[StrokeIdx].Y := CurY;
        Inc(StrokeIdx);
      end;
    end;
    
    FGlyphs[CharIdx].StrokeCount := StrokeIdx;
    SetLength(FGlyphs[CharIdx].StrokeData, StrokeIdx);
    FGlyphs[CharIdx].Defined := StrokeIdx > 0;
    
    if FDebugMode and (CharIdx >= 32) and (CharIdx <= 90) and ((CharIdx < 40) or (CharIdx = 65)) then
      DebugLog(Format('  Char %d ''%s'': width=%d, strokes=%d', 
        [CharIdx, Chr(CharIdx), GlyphWidth, StrokeIdx]));
  end;
  
  SetLength(CharOffsets, 0);
  Result := True;
end;

function TWinFont.ParseRasterGlyphs(Stream: TStream; FNTOffset: LongWord; CharTableOffset: LongWord): Boolean;
var
  I, CharIdx: Integer;
  GlyphWidth: Word;
  BitmapOffset: Word;
  BytesPerRow: Integer;
  BitmapSize: Integer;
  Row: Integer;
  NumChars: Integer;
begin
  Result := False;
  NumChars := FLastChar - FFirstChar + 1;
  
  DebugLog(Format('Parsing %d raster glyphs', [NumChars]));
  DebugLog(Format('  Char table at: $%x', [CharTableOffset]));
  DebugLog(Format('  FNT offset: $%x', [FNTOffset]));
  DebugLog(Format('  Font height: %d', [FHeight]));
  
  for I := 0 to NumChars - 1 do
  begin
    CharIdx := FFirstChar + I;
    if (CharIdx < 0) or (CharIdx >= MAX_GLYPHS) then Continue;
    
    // Read character table entry: 2 bytes width, 2 bytes bitmap offset
    // (for FNT 2.0/3.0 raster fonts)
    Stream.Position := CharTableOffset + I * 4;
    GlyphWidth := ReadWord(Stream);
    BitmapOffset := ReadWord(Stream);
    
    FGlyphs[CharIdx].Width := GlyphWidth;
    FGlyphs[CharIdx].Height := FHeight;
    
    if GlyphWidth = 0 then Continue;
    
    // Calculate bitmap size
    BytesPerRow := (GlyphWidth + 7) div 8;
    BitmapSize := BytesPerRow * FHeight;
    
    FGlyphs[CharIdx].BitmapWidth := BytesPerRow;
    
    // Read bitmap data (offset is relative to FNT resource start)
    if FNTOffset + BitmapOffset + LongWord(BitmapSize) <= Stream.Size then
    begin
      SetLength(FGlyphs[CharIdx].BitmapData, BitmapSize);
      Stream.Position := FNTOffset + BitmapOffset;
      Stream.Read(FGlyphs[CharIdx].BitmapData[0], BitmapSize);
      FGlyphs[CharIdx].Defined := True;
      
      if FDebugMode and (CharIdx >= 32) and (CharIdx <= 127) and 
         ((CharIdx = 32) or (CharIdx = 33) or (CharIdx = 65) or (CharIdx = 72)) then
        DebugLog(Format('  Char %d ''%s'': width=%d, height=%d, bytes/row=%d, offset=$%x, bitmap@$%x', 
          [CharIdx, Chr(CharIdx), GlyphWidth, FHeight, BytesPerRow, BitmapOffset, FNTOffset + BitmapOffset]));
    end
    else
    begin
      if FDebugMode then
        DebugLog(Format('  Char %d: bitmap out of range (offset=$%x, size=%d, stream=%d)', 
          [CharIdx, FNTOffset + BitmapOffset, BitmapSize, Stream.Size]));
    end;
  end;
  
  Result := True;
end;

function TWinFont.ParseFNTResource(Stream: TStream; Offset, Size: LongWord): Boolean;
var
  Version: Word;
  FntTypeFlags: Word;
  PixHeight: Word;
  FaceNameOffset: LongWord;
  CharTableOffset: LongWord;
  StrokeDataBase: LongWord;
  NumChars: Integer;
  C: Char;
  I: Integer;
begin
  Result := False;
  
  if Offset + 118 > Stream.Size then Exit;
  
  Stream.Position := Offset;
  
  // Read FNT header
  Version := ReadWord(Stream);      // 0x0100, 0x0200 or 0x0300
  DebugLog(Format('FNT Version: $%x', [Version]));
  
  // Skip to copyright at offset 6
  Stream.Position := Offset + 6;
  FCopyright := '';
  for I := 1 to 60 do
  begin
    Stream.Read(C, 1);
    if C = #0 then Break;
    FCopyright := FCopyright + C;
  end;
  DebugLog('Copyright: ' + FCopyright);
  
  // Font type at offset 66
  Stream.Position := Offset + 66;
  FntTypeFlags := ReadWord(Stream);
  DebugLog(Format('Font type flags: $%x', [FntTypeFlags]));
  
  // Points, VertRes, HorizRes, Ascent
  ReadWord(Stream);  // Points
  ReadWord(Stream);  // VertRes  
  ReadWord(Stream);  // HorizRes
  FAscent := ReadWord(Stream);
  DebugLog(Format('Ascent: %d', [FAscent]));
  
  // Skip to pixel height at offset 88
  Stream.Position := Offset + 88;
  PixHeight := ReadWord(Stream);
  FHeight := PixHeight;
  FDescent := FHeight - FAscent;
  DebugLog(Format('Height: %d', [FHeight]));
  
  // Skip PitchAndFamily, AvgWidth, MaxWidth
  Stream.Position := Offset + 95;
  FFirstChar := ReadByte(Stream);
  FLastChar := ReadByte(Stream);
  FDefaultChar := ReadByte(Stream) + FFirstChar;
  DebugLog(Format('Char range: %d - %d, default: %d', [FFirstChar, FLastChar, FDefaultChar]));
  
  // Face name offset at offset 105
  Stream.Position := Offset + 105;
  FaceNameOffset := ReadDWord(Stream);
  
  // Read font face name
  if FaceNameOffset > 0 then
  begin
    Stream.Position := Offset + FaceNameOffset;
    FFontName := '';
    for I := 1 to 32 do
    begin
      Stream.Read(C, 1);
      if C = #0 then Break;
      FFontName := FFontName + C;
    end;
    DebugLog('Font name: ' + FFontName);
  end;
  
  // Character table offset depends on FNT version:
  // FNT 1.0 (vector): starts at offset 117, format is (offset:2, width:2)
  // FNT 2.0/3.0 (raster): starts at offset 118, format is (width:2, offset:2)
  if Version = $0100 then
    CharTableOffset := Offset + 117
  else
    CharTableOffset := Offset + 118;
  
  // Number of entries = last_char - first_char + 2 (includes sentinel)
  NumChars := FLastChar - FFirstChar + 2;
  
  // Stroke data base = after character table + font name + null terminator + padding
  // Character table end = CharTableOffset + NumChars * 4
  // Then comes the font name (null terminated) and possibly padding
  // Stroke data starts after that
  StrokeDataBase := CharTableOffset + LongWord(NumChars * 4);
  
  // Skip past the device name and face name that appear after char table
  // Find the actual stroke data start by looking for the pattern
  Stream.Position := StrokeDataBase;
  while Stream.Position < Stream.Size do
  begin
    if ReadByte(Stream) = $80 then
    begin
      StrokeDataBase := Stream.Position - 1;
      Break;
    end;
    if Stream.Position - (CharTableOffset + LongWord(NumChars * 4)) > 100 then
    begin
      // Fallback - just use a fixed offset after font name
      StrokeDataBase := CharTableOffset + LongWord(NumChars * 4) + LongWord(Length(FFontName)) + 2;
      Break;
    end;
  end;
  
  DebugLog(Format('Character table at: $%x', [CharTableOffset]));
  DebugLog(Format('Stroke data base at: $%x', [StrokeDataBase]));
  
  // Check if vector or raster font
  // Bit 0 of dfType: 1 = vector, 0 = raster
  if (FntTypeFlags and $0001) <> 0 then
  begin
    FFontType := ftVector;
    DebugLog('Font type: Vector');
    Result := ParseVectorGlyphs(Stream, StrokeDataBase, CharTableOffset);
  end
  else
  begin
    FFontType := ftRaster;
    DebugLog('Font type: Raster');
    Result := ParseRasterGlyphs(Stream, Offset, CharTableOffset);
  end;
end;

function TWinFont.ParseNEFile(Stream: TStream): Boolean;
var
  MZSig: Word;
  NEOffset: LongWord;
  NESig: Word;
  ResourceTableOffset: Word;
  AlignShift: Word;
  TypeID, Count: Word;
  ResOffset, ResLength: Word;
  I: Integer;
  FontResOffset, FontResSize: LongWord;
begin
  Result := False;
  
  // Check MZ signature
  Stream.Position := 0;
  MZSig := ReadWord(Stream);
  if MZSig <> $5A4D then  // 'MZ'
  begin
    DebugLog('Not a valid MZ executable');
    Exit;
  end;
  
  // Get NE header offset from MZ header at offset 0x3C
  Stream.Position := $3C;
  NEOffset := ReadDWord(Stream);
  DebugLog(Format('NE header offset: $%x', [NEOffset]));
  
  if NEOffset + 64 > Stream.Size then Exit;
  
  // Check NE signature
  Stream.Position := NEOffset;
  NESig := ReadWord(Stream);
  if NESig <> $454E then  // 'NE'
  begin
    DebugLog('Not a valid NE executable');
    Exit;
  end;
  
  // Get resource table offset (relative to NE header) at NE+$24
  Stream.Position := NEOffset + $24;
  ResourceTableOffset := ReadWord(Stream);
  DebugLog(Format('Resource table offset: $%x (abs: $%x)', 
    [ResourceTableOffset, NEOffset + ResourceTableOffset]));
  
  // Go to resource table
  Stream.Position := NEOffset + ResourceTableOffset;
  
  // Read alignment shift count
  AlignShift := ReadWord(Stream);
  DebugLog(Format('Alignment shift: %d', [AlignShift]));
  
  // Parse resource table looking for FONT resources
  FontResOffset := 0;
  FontResSize := 0;
  
  while Stream.Position < Stream.Size do
  begin
    TypeID := ReadWord(Stream);
    if TypeID = 0 then Break;  // End of resource table
    
    Count := ReadWord(Stream);
    ReadDWord(Stream);  // Reserved
    
    DebugLog(Format('Resource type $%x, count %d', [TypeID, Count]));
    
    for I := 0 to Count - 1 do
    begin
      ResOffset := ReadWord(Stream);
      ResLength := ReadWord(Stream);
      ReadWord(Stream);  // Flags
      ReadWord(Stream);  // Resource ID
      ReadDWord(Stream); // Reserved
      
      // TypeID $8008 = RT_FONT (with high bit set)
      // TypeID $0008 = RT_FONT (without high bit)
      if (TypeID = $8008) or (TypeID = 8) then
      begin
        FontResOffset := LongWord(ResOffset) shl AlignShift;
        FontResSize := LongWord(ResLength) shl AlignShift;
        DebugLog(Format('Found FONT resource at $%x, size $%x', [FontResOffset, FontResSize]));
        Break;
      end;
    end;
    
    if FontResOffset > 0 then Break;
  end;
  
  if FontResOffset > 0 then
    Result := ParseFNTResource(Stream, FontResOffset, FontResSize)
  else
    DebugLog('No FONT resource found');
end;

function TWinFont.LoadFromStream(Stream: TStream): Boolean;
begin
  Clear;
  Result := ParseNEFile(Stream);
  FLoaded := Result;
end;

function TWinFont.LoadFromFile(const FileName: string): Boolean;
var
  Stream: TFileStream;
begin
  Result := False;
  if not FileExists(FileName) then 
  begin
    DebugLog('File not found: ' + FileName);
    Exit;
  end;
  
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    on E: Exception do
      DebugLog('Error loading file: ' + E.Message);
  end;
end;

procedure TWinFont.DrawPixel(X, Y: Integer);
begin
  if (X >= 0) and (Y >= 0) and (X < GetMaxX) and (Y < GetMaxY) then
    PutPixel(X, Y, FColor);
end;

procedure TWinFont.DrawLineBresenham(X1, Y1, X2, Y2: Integer);
var
  DX, DY, SX, SY, Err, E2: Integer;
begin
  DX := Abs(X2 - X1);
  DY := Abs(Y2 - Y1);
  
  if X1 < X2 then SX := 1 else SX := -1;
  if Y1 < Y2 then SY := 1 else SY := -1;
  
  Err := DX - DY;
  
  while True do
  begin
    DrawPixel(X1, Y1);
    
    if (X1 = X2) and (Y1 = Y2) then Break;
    
    E2 := 2 * Err;
    if E2 > -DY then
    begin
      Err := Err - DY;
      X1 := X1 + SX;
    end;
    if E2 < DX then
    begin
      Err := Err + DX;
      Y1 := Y1 + SY;
    end;
  end;
end;

procedure TWinFont.DrawRasterChar(X, Y: Integer; CharIdx: Integer);
var
  Row, Col: Integer;
  ByteIdx, BitIdx: Integer;
  ByteVal: Byte;
  PixelX, PixelY: Integer;
  ScaleInt: Integer;
  SX, SY: Integer;
  Plane: Integer;
begin
  if not FGlyphs[CharIdx].Defined then Exit;
  if Length(FGlyphs[CharIdx].BitmapData) = 0 then Exit;
  
  // For scaling, we'll draw each pixel as a ScaleInt x ScaleInt block
  ScaleInt := Round(FScale);
  if ScaleInt < 1 then ScaleInt := 1;
  
  for Row := 0 to FGlyphs[CharIdx].Height - 1 do
  begin
    for Col := 0 to FGlyphs[CharIdx].Width - 1 do
    begin
      // Bitmap is stored in PLANAR format:
      // - First Height bytes = bits 0-7 (plane 0)
      // - Next Height bytes = bits 8-15 (plane 1)
      // - etc.
      Plane := Col div 8;
      ByteIdx := Plane * FGlyphs[CharIdx].Height + Row;
      BitIdx := 7 - (Col mod 8);  // MSB is leftmost pixel
      
      if ByteIdx < Length(FGlyphs[CharIdx].BitmapData) then
      begin
        ByteVal := FGlyphs[CharIdx].BitmapData[ByteIdx];
        
        // Check if this bit is set
        if (ByteVal and (1 shl BitIdx)) <> 0 then
        begin
          // Draw pixel (or block of pixels if scaled)
          PixelX := X + Round(Col * FScale);
          PixelY := Y + Round(Row * FScale);
          
          if ScaleInt <= 1 then
            DrawPixel(PixelX, PixelY)
          else
          begin
            // Draw a block for scaling
            for SY := 0 to ScaleInt - 1 do
              for SX := 0 to ScaleInt - 1 do
                DrawPixel(PixelX + SX, PixelY + SY);
          end;
        end;
      end;
    end;
  end;
end;

procedure TWinFont.DrawChar(X, Y: Integer; C: Char);
var
  CharIdx: Integer;
  I: Integer;
  CurX, CurY: Integer;
  ScaledX, ScaledY: Integer;
  ScaledLastX, ScaledLastY: Integer;
begin
  CharIdx := Ord(C);
  
  // Bounds check
  if (CharIdx < 0) or (CharIdx >= MAX_GLYPHS) then
    CharIdx := FDefaultChar;
  
  if not FGlyphs[CharIdx].Defined then
  begin
    // Try space or default
    if (FDefaultChar >= 0) and (FDefaultChar < MAX_GLYPHS) and 
       FGlyphs[FDefaultChar].Defined then
      CharIdx := FDefaultChar
    else
      Exit;
  end;
  
  if FFontType = ftVector then
  begin
    // Draw vector font
    ScaledLastX := X;
    ScaledLastY := Y;
    
    for I := 0 to FGlyphs[CharIdx].StrokeCount - 1 do
    begin
      CurX := FGlyphs[CharIdx].StrokeData[I].X;
      CurY := FGlyphs[CharIdx].StrokeData[I].Y;
      
      // Transform coordinates
      // Try: Y increases downward in font too (opposite of typical)
      ScaledX := X + Round(CurX * FScale);
      ScaledY := Y + Round(CurY * FScale);
      
      case FGlyphs[CharIdx].StrokeData[I].Cmd of
        scMoveTo:
          begin
            ScaledLastX := ScaledX;
            ScaledLastY := ScaledY;
          end;
        scLineTo:
          begin
            DrawLineBresenham(ScaledLastX, ScaledLastY, ScaledX, ScaledY);
            ScaledLastX := ScaledX;
            ScaledLastY := ScaledY;
          end;
      end;
    end;
  end
  else if FFontType = ftRaster then
  begin
    // Draw raster (bitmap) font
    DrawRasterChar(X, Y, CharIdx);
  end;
end;

procedure TWinFont.DrawText(X, Y: Integer; const Text: string);
var
  I: Integer;
  CurX: Integer;
begin
  if not FLoaded then Exit;
  
  CurX := X;
  for I := 1 to Length(Text) do
  begin
    DrawChar(CurX, Y, Text[I]);
    CurX := CurX + GetCharWidth(Text[I]);
  end;
end;

function TWinFont.GetCharWidth(C: Char): Integer;
var
  CharIdx: Integer;
begin
  CharIdx := Ord(C);
  if (CharIdx >= 0) and (CharIdx < MAX_GLYPHS) and FGlyphs[CharIdx].Defined then
    Result := Round(FGlyphs[CharIdx].Width * FScale)
  else if (FDefaultChar >= 0) and (FDefaultChar < MAX_GLYPHS) then
    Result := Round(FGlyphs[FDefaultChar].Width * FScale)
  else
    Result := Round(8 * FScale);
end;

function TWinFont.GetCharWidth(CharCode: Integer): Integer;
begin
  if (CharCode >= 0) and (CharCode < MAX_GLYPHS) and FGlyphs[CharCode].Defined then
    Result := FGlyphs[CharCode].Width
  else if (FDefaultChar >= 0) and (FDefaultChar < MAX_GLYPHS) then
    Result := FGlyphs[FDefaultChar].Width
  else
    Result := 8;
end;

function TWinFont.GetGlyphBitmap(CharCode: Integer; Dest: TObject): Boolean;
var
  Row, Col, Plane, ByteIdx, BitIdx: Integer;
  ByteVal: Byte;
  DestCanvas: TCanvas;
  DestBmp: TBitmap;
begin
  Result := False;
  if not FLoaded then Exit;
  if FFontType <> ftRaster then Exit;
  if (CharCode < 0) or (CharCode >= MAX_GLYPHS) then Exit;
  if not FGlyphs[CharCode].Defined then Exit;
  if Length(FGlyphs[CharCode].BitmapData) = 0 then Exit;
  
  // Cast to TBitmap (from Graphics unit)
  if not (Dest is TBitmap) then Exit;
  DestBmp := TBitmap(Dest);
  DestCanvas := DestBmp.Canvas;
  
  // Ensure destination is correct size
  if (DestBmp.Width <> FGlyphs[CharCode].Width) or (DestBmp.Height <> FGlyphs[CharCode].Height) then
  begin
    DestBmp.Width := FGlyphs[CharCode].Width;
    DestBmp.Height := FGlyphs[CharCode].Height;
  end;
  
  // Clear to white
  DestCanvas.Brush.Color := clWhite;
  DestCanvas.FillRect(0, 0, DestBmp.Width, DestBmp.Height);
  
  for Row := 0 to FGlyphs[CharCode].Height - 1 do
  begin
    for Col := 0 to FGlyphs[CharCode].Width - 1 do
    begin
      // Planar format
      Plane := Col div 8;
      ByteIdx := Plane * FGlyphs[CharCode].Height + Row;
      BitIdx := 7 - (Col mod 8);
      
      if ByteIdx < Length(FGlyphs[CharCode].BitmapData) then
      begin
        ByteVal := FGlyphs[CharCode].BitmapData[ByteIdx];
        if (ByteVal and (1 shl BitIdx)) <> 0 then
          DestCanvas.Pixels[Col, Row] := clBlack;
      end;
    end;
  end;
  
  Result := True;
end;

function TWinFont.GetTextWidth(const Text: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Text) do
    Result := Result + GetCharWidth(Text[I]);
end;

end.
