{
  FON Font Creator Library for Lazarus/Free Pascal
  
  Creates Microsoft Windows FON bitmap font files.
  
  Usage:
    1. Create a TFONCreator instance
    2. Set font properties (FontName, PointSize, etc.)
    3. Call SetCharacter() for each glyph, passing a TBitmap
    4. Call SaveToFile() to write the FON file
    
  Example:
    Creator := TFONCreator.Create;
    try
      Creator.FontName := 'MyFont';
      Creator.PointSize := 12;
      Creator.Copyright := 'Copyright 2024';
      
      // Add characters
      for Ch := #32 to #127 do
      begin
        Bmp := CreateGlyphBitmap(Ch);  // Your function to create glyph
        Creator.SetCharacter(Ord(Ch), Bmp);
        Bmp.Free;
      end;
      
      Creator.SaveToFile('MYFONT.FON');
    finally
      Creator.Free;
    end;
}

unit FONCreator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

type
  { Character glyph data }
  TGlyphData = record
    Defined: Boolean;
    Width: Integer;           // Character width in pixels
    BitmapData: array of Byte; // Planar bitmap data
  end;
  
  { Font weight constants }
  TFontWeight = (
    fwDontCare = 0,
    fwThin = 100,
    fwExtraLight = 200,
    fwLight = 300,
    fwNormal = 400,
    fwMedium = 500,
    fwSemiBold = 600,
    fwBold = 700,
    fwExtraBold = 800,
    fwHeavy = 900
  );
  
  { Font character set }
  TFontCharSet = (
    csANSI = 0,
    csDefault = 1,
    csSymbol = 2,
    csShiftJIS = 128,
    csOEM = 255
  );
  
  { Font pitch and family }
  TFontPitchFamily = (
    pfDefault = 0,
    pfFixed = 1,          // Fixed pitch (monospace)
    pfVariable = 2,       // Variable pitch (proportional)
    pfRoman = 16,         // Serif
    pfSwiss = 32,         // Sans-serif
    pfModern = 48,        // Monospace
    pfScript = 64,        // Script/cursive
    pfDecorative = 80     // Decorative
  );

  { TFONCreator - Creates Windows FON bitmap font files }
  TFONCreator = class
  private
    FGlyphs: array[0..255] of TGlyphData;
    FFontName: string;
    FCopyright: string;
    FPointSize: Integer;
    FHeight: Integer;         // Will be auto-calculated from first glyph
    FAscent: Integer;
    FWeight: TFontWeight;
    FCharSet: TFontCharSet;
    FPitchFamily: TFontPitchFamily;
    FFirstChar: Byte;
    FLastChar: Byte;
    FDefaultChar: Byte;
    FBreakChar: Byte;
    FItalic: Boolean;
    FUnderline: Boolean;
    FStrikeOut: Boolean;
    FHeightSet: Boolean;
    
    procedure ConvertBitmapToGlyph(CharIdx: Integer; Bitmap: TBitmap);
    function BuildFNTResource: TMemoryStream;
    function BuildNEExecutable(FNTData: TMemoryStream): TMemoryStream;
    procedure WriteWord(Stream: TStream; Value: Word);
    procedure WriteDWord(Stream: TStream; Value: LongWord);
    procedure WriteByte(Stream: TStream; Value: Byte);
    procedure WriteString(Stream: TStream; const S: string; MaxLen: Integer);
    procedure UpdateCharRange;
  public
    constructor Create;
    destructor Destroy; override;
    
    { Set a character glyph from a bitmap
      CharCode: ASCII code (0-255)
      Bitmap: Monochrome or color bitmap (non-white pixels become foreground)
      The bitmap height must be consistent across all characters }
    procedure SetCharacter(CharCode: Byte; Bitmap: TBitmap); overload;
    
    { Set a character glyph with explicit width (for padding/spacing) }
    procedure SetCharacter(CharCode: Byte; Bitmap: TBitmap; CharWidth: Integer); overload;
    
    { Clear a character }
    procedure ClearCharacter(CharCode: Byte);
    
    { Clear all characters }
    procedure ClearAll;
    
    { Check if a character is defined }
    function IsCharacterDefined(CharCode: Byte): Boolean;
    
    { Get the width of a defined character }
    function GetCharacterWidth(CharCode: Byte): Integer;
    
    { Save the font to a FON file }
    procedure SaveToFile(const FileName: string);
    
    { Save to a stream }
    procedure SaveToStream(Stream: TStream);
    
    { Font properties }
    property FontName: string read FFontName write FFontName;
    property Copyright: string read FCopyright write FCopyright;
    property PointSize: Integer read FPointSize write FPointSize;
    property Height: Integer read FHeight write FHeight;
    property Ascent: Integer read FAscent write FAscent;
    property Weight: TFontWeight read FWeight write FWeight;
    property CharSet: TFontCharSet read FCharSet write FCharSet;
    property PitchFamily: TFontPitchFamily read FPitchFamily write FPitchFamily;
    property FirstChar: Byte read FFirstChar write FFirstChar;
    property LastChar: Byte read FLastChar write FLastChar;
    property DefaultChar: Byte read FDefaultChar write FDefaultChar;
    property BreakChar: Byte read FBreakChar write FBreakChar;
    property Italic: Boolean read FItalic write FItalic;
    property Underline: Boolean read FUnderline write FUnderline;
    property StrikeOut: Boolean read FStrikeOut write FStrikeOut;
  end;

implementation

{ TFONCreator }

constructor TFONCreator.Create;
var
  I: Integer;
begin
  inherited Create;
  
  // Initialize all glyphs as undefined
  for I := 0 to 255 do
  begin
    FGlyphs[I].Defined := False;
    FGlyphs[I].Width := 0;
    SetLength(FGlyphs[I].BitmapData, 0);
  end;
  
  // Default properties
  FFontName := 'NewFont';
  FCopyright := '';
  FPointSize := 10;
  FHeight := 0;
  FAscent := 0;
  FWeight := fwNormal;
  FCharSet := csANSI;
  FPitchFamily := pfVariable;
  FFirstChar := 32;
  FLastChar := 255;
  FDefaultChar := 32;
  FBreakChar := 32;
  FItalic := False;
  FUnderline := False;
  FStrikeOut := False;
  FHeightSet := False;
end;

destructor TFONCreator.Destroy;
var
  I: Integer;
begin
  for I := 0 to 255 do
    SetLength(FGlyphs[I].BitmapData, 0);
  inherited Destroy;
end;

procedure TFONCreator.WriteWord(Stream: TStream; Value: Word);
begin
  Stream.WriteBuffer(Value, 2);
end;

procedure TFONCreator.WriteDWord(Stream: TStream; Value: LongWord);
begin
  Stream.WriteBuffer(Value, 4);
end;

procedure TFONCreator.WriteByte(Stream: TStream; Value: Byte);
begin
  Stream.WriteBuffer(Value, 1);
end;

procedure TFONCreator.WriteString(Stream: TStream; const S: string; MaxLen: Integer);
var
  I: Integer;
  B: Byte;
begin
  for I := 1 to MaxLen do
  begin
    if I <= Length(S) then
      B := Ord(S[I])
    else
      B := 0;
    WriteByte(Stream, B);
  end;
end;

procedure TFONCreator.ConvertBitmapToGlyph(CharIdx: Integer; Bitmap: TBitmap);
var
  Row, Col, Plane: Integer;
  NumPlanes, ByteIdx: Integer;
  BitIdx: Integer;
  PixelColor: TColor;
  IsSet: Boolean;
  BmpWidth, BmpHeight: Integer;
begin
  BmpWidth := Bitmap.Width;
  BmpHeight := Bitmap.Height;
  
  // Set or verify font height
  if not FHeightSet then
  begin
    FHeight := BmpHeight;
    if FAscent = 0 then
      FAscent := (BmpHeight * 4) div 5;  // Default ascent to 80% of height
    FHeightSet := True;
  end
  else if BmpHeight <> FHeight then
    raise Exception.CreateFmt('Bitmap height (%d) does not match font height (%d)', [BmpHeight, FHeight]);
  
  FGlyphs[CharIdx].Width := BmpWidth;
  
  // Calculate number of planes needed
  NumPlanes := (BmpWidth + 7) div 8;
  
  // Allocate bitmap data (planar format: NumPlanes * BmpHeight bytes)
  SetLength(FGlyphs[CharIdx].BitmapData, NumPlanes * BmpHeight);
  
  // Initialize to zero
  for ByteIdx := 0 to Length(FGlyphs[CharIdx].BitmapData) - 1 do
    FGlyphs[CharIdx].BitmapData[ByteIdx] := 0;
  
  // Convert bitmap to planar format
  for Row := 0 to BmpHeight - 1 do
  begin
    for Col := 0 to BmpWidth - 1 do
    begin
      // Get pixel - treat non-white as foreground
      PixelColor := Bitmap.Canvas.Pixels[Col, Row];
      IsSet := (PixelColor <> clWhite) and (PixelColor <> $FFFFFF);
      
      if IsSet then
      begin
        // Calculate position in planar storage
        Plane := Col div 8;
        ByteIdx := Plane * BmpHeight + Row;
        BitIdx := 7 - (Col mod 8);  // MSB is leftmost pixel
        
        // Set the bit
        FGlyphs[CharIdx].BitmapData[ByteIdx] := 
          FGlyphs[CharIdx].BitmapData[ByteIdx] or (1 shl BitIdx);
      end;
    end;
  end;
  
  FGlyphs[CharIdx].Defined := True;
end;

procedure TFONCreator.SetCharacter(CharCode: Byte; Bitmap: TBitmap);
begin
  if Bitmap = nil then
    raise Exception.Create('Bitmap cannot be nil');
  if Bitmap.Width = 0 then
    raise Exception.Create('Bitmap width cannot be zero');
  if Bitmap.Height = 0 then
    raise Exception.Create('Bitmap height cannot be zero');
    
  ConvertBitmapToGlyph(CharCode, Bitmap);
end;

procedure TFONCreator.SetCharacter(CharCode: Byte; Bitmap: TBitmap; CharWidth: Integer);
begin
  SetCharacter(CharCode, Bitmap);
  // Override width for spacing purposes
  if CharWidth > 0 then
    FGlyphs[CharCode].Width := CharWidth;
end;

procedure TFONCreator.ClearCharacter(CharCode: Byte);
begin
  FGlyphs[CharCode].Defined := False;
  FGlyphs[CharCode].Width := 0;
  SetLength(FGlyphs[CharCode].BitmapData, 0);
end;

procedure TFONCreator.ClearAll;
var
  I: Integer;
begin
  for I := 0 to 255 do
    ClearCharacter(I);
  FHeightSet := False;
  FHeight := 0;
end;

function TFONCreator.IsCharacterDefined(CharCode: Byte): Boolean;
begin
  Result := FGlyphs[CharCode].Defined;
end;

function TFONCreator.GetCharacterWidth(CharCode: Byte): Integer;
begin
  if FGlyphs[CharCode].Defined then
    Result := FGlyphs[CharCode].Width
  else
    Result := 0;
end;

procedure TFONCreator.UpdateCharRange;
var
  I: Integer;
begin
  // Find actual first defined character
  FFirstChar := 255;
  for I := 0 to 255 do
    if FGlyphs[I].Defined then
    begin
      FFirstChar := I;
      Break;
    end;
  
  // Find actual last defined character  
  FLastChar := 0;
  for I := 255 downto 0 do
    if FGlyphs[I].Defined then
    begin
      FLastChar := I;
      Break;
    end;
    
  // Ensure at least space character range
  if FFirstChar > FLastChar then
  begin
    FFirstChar := 32;
    FLastChar := 32;
  end;
end;

function TFONCreator.BuildFNTResource: TMemoryStream;
var
  FNT: TMemoryStream;
  CharTableStart: LongWord;
  BitmapDataStart: LongWord;
  FaceNameOffset: LongWord;
  CurrentBitmapOffset: LongWord;
  CharCount: Integer;
  I: Integer;
  NumPlanes, BitmapSize: Integer;
  MaxWidth, AvgWidth, TotalWidth, DefinedCount: Integer;
  FNTSize: LongWord;
begin
  FNT := TMemoryStream.Create;
  
  UpdateCharRange;
  CharCount := FLastChar - FFirstChar + 1;
  
  // Calculate max and average widths
  MaxWidth := 0;
  TotalWidth := 0;
  DefinedCount := 0;
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
    begin
      if FGlyphs[I].Width > MaxWidth then
        MaxWidth := FGlyphs[I].Width;
      TotalWidth := TotalWidth + FGlyphs[I].Width;
      Inc(DefinedCount);
    end;
  end;
  if DefinedCount > 0 then
    AvgWidth := TotalWidth div DefinedCount
  else
    AvgWidth := 8;
  if MaxWidth = 0 then
    MaxWidth := 8;
  
  // FNT 2.0 header is 118 bytes, then character table
  CharTableStart := 118;
  
  // Character table: (CharCount + 1) entries × 4 bytes each
  // Extra entry is the sentinel for calculating last char size
  BitmapDataStart := CharTableStart + (CharCount + 1) * 4;
  
  // Calculate total bitmap data size
  CurrentBitmapOffset := BitmapDataStart;
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(Length(FGlyphs[I].BitmapData))
    else
    begin
      // Undefined chars get minimal 1-byte-wide empty glyph
      NumPlanes := 1;
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(NumPlanes * FHeight);
    end;
  end;
  
  // Face name comes after bitmap data
  FaceNameOffset := CurrentBitmapOffset;
  
  // Total size includes face name + null terminator
  FNTSize := FaceNameOffset + LongWord(Length(FFontName)) + 1;
  
  // ===== Write FNT Header (118 bytes) =====
  
  // Offset 0: dfVersion (WORD) - 0x0200 for raster
  WriteWord(FNT, $0200);
  
  // Offset 2: dfSize (DWORD) - total resource size
  WriteDWord(FNT, FNTSize);
  
  // Offset 6: dfCopyright (60 bytes)
  WriteString(FNT, FCopyright, 60);
  
  // Offset 66: dfType (WORD) - 0x0000 for raster
  WriteWord(FNT, $0000);
  
  // Offset 68: dfPoints (WORD)
  WriteWord(FNT, FPointSize);
  
  // Offset 70: dfVertRes (WORD) - vertical DPI
  WriteWord(FNT, 96);
  
  // Offset 72: dfHorizRes (WORD) - horizontal DPI
  WriteWord(FNT, 96);
  
  // Offset 74: dfAscent (WORD)
  WriteWord(FNT, FAscent);
  
  // Offset 76: dfInternalLeading (WORD)
  WriteWord(FNT, 0);
  
  // Offset 78: dfExternalLeading (WORD)
  WriteWord(FNT, 0);
  
  // Offset 80: dfItalic (BYTE)
  if FItalic then WriteByte(FNT, 1) else WriteByte(FNT, 0);
  
  // Offset 81: dfUnderline (BYTE)
  if FUnderline then WriteByte(FNT, 1) else WriteByte(FNT, 0);
  
  // Offset 82: dfStrikeOut (BYTE)
  if FStrikeOut then WriteByte(FNT, 1) else WriteByte(FNT, 0);
  
  // Offset 83: dfWeight (WORD)
  WriteWord(FNT, Word(FWeight));
  
  // Offset 85: dfCharSet (BYTE)
  WriteByte(FNT, Byte(FCharSet));
  
  // Offset 86: dfPixWidth (WORD) - 0 for proportional
  WriteWord(FNT, 0);
  
  // Offset 88: dfPixHeight (WORD)
  WriteWord(FNT, FHeight);
  
  // Offset 90: dfPitchAndFamily (BYTE)
  WriteByte(FNT, Byte(FPitchFamily));
  
  // Offset 91: dfAvgWidth (WORD)
  WriteWord(FNT, AvgWidth);
  
  // Offset 93: dfMaxWidth (WORD)
  WriteWord(FNT, MaxWidth);
  
  // Offset 95: dfFirstChar (BYTE)
  WriteByte(FNT, FFirstChar);
  
  // Offset 96: dfLastChar (BYTE)
  WriteByte(FNT, FLastChar);
  
  // Offset 97: dfDefaultChar (BYTE) - relative to dfFirstChar
  WriteByte(FNT, FDefaultChar - FFirstChar);
  
  // Offset 98: dfBreakChar (BYTE) - relative to dfFirstChar
  WriteByte(FNT, FBreakChar - FFirstChar);
  
  // Offset 99: dfWidthBytes (WORD) - width of bitmap in bytes (for fixed pitch)
  WriteWord(FNT, 0);
  
  // Offset 101: dfDevice (DWORD) - offset to device name (0 = none)
  WriteDWord(FNT, 0);
  
  // Offset 105: dfFace (DWORD) - offset to face name
  WriteDWord(FNT, FaceNameOffset);
  
  // Offset 109: dfBitsPointer (DWORD) - not used in file, set to 0
  WriteDWord(FNT, 0);
  
  // Offset 113: dfBitsOffset (DWORD) - offset to bitmap data
  WriteDWord(FNT, BitmapDataStart);
  
  // Offset 117: dfReserved (BYTE)
  WriteByte(FNT, 0);
  
  // ===== Write Character Table (offset 118) =====
  CurrentBitmapOffset := BitmapDataStart;
  
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
    begin
      // Width (WORD)
      WriteWord(FNT, FGlyphs[I].Width);
      // Offset (WORD)
      WriteWord(FNT, CurrentBitmapOffset);
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(Length(FGlyphs[I].BitmapData));
    end
    else
    begin
      // Undefined character - use minimal width
      WriteWord(FNT, 1);
      WriteWord(FNT, CurrentBitmapOffset);
      CurrentBitmapOffset := CurrentBitmapOffset + LongWord(FHeight);
    end;
  end;
  
  // Sentinel entry (points to end of bitmap data)
  WriteWord(FNT, 0);
  WriteWord(FNT, CurrentBitmapOffset);
  
  // ===== Write Bitmap Data =====
  for I := FFirstChar to FLastChar do
  begin
    if FGlyphs[I].Defined then
    begin
      FNT.WriteBuffer(FGlyphs[I].BitmapData[0], Length(FGlyphs[I].BitmapData));
    end
    else
    begin
      // Write empty glyph data (all zeros)
      for NumPlanes := 1 to FHeight do
        WriteByte(FNT, 0);
    end;
  end;
  
  // ===== Write Face Name =====
  for I := 1 to Length(FFontName) do
    WriteByte(FNT, Ord(FFontName[I]));
  WriteByte(FNT, 0);  // Null terminator
  
  Result := FNT;
end;

function TFONCreator.BuildNEExecutable(FNTData: TMemoryStream): TMemoryStream;
var
  EXE: TMemoryStream;
  NEHeaderOffset: LongWord;
  ResourceTableOffset: Word;
  ResidentNameTableOffset: Word;
  ModuleRefTableOffset: Word;
  ImportedNameTableOffset: Word;
  EntryTableOffset: Word;
  NonResidentNameTableOffset: LongWord;
  SegmentTableOffset: Word;
  FNTOffset: LongWord;
  FNTSize: LongWord;
  AlignShift: Word;
  I: Integer;
  NameLen: Byte;
begin
  EXE := TMemoryStream.Create;
  
  FNTSize := FNTData.Size;
  
  // Use alignment shift of 4 (16-byte alignment)
  AlignShift := 4;
  
  // MZ header at 0x00, NE header at 0x400
  NEHeaderOffset := $400;
  
  // NE header offsets (relative to NE header)
  SegmentTableOffset := $40;           // Empty segment table
  ResourceTableOffset := $40;          // Resource table starts here
  ResidentNameTableOffset := $60;      // After resource table
  ModuleRefTableOffset := $70;         // After resident name table
  ImportedNameTableOffset := $70;      // Same as module ref (both empty)
  EntryTableOffset := $70;             // Entry table
  NonResidentNameTableOffset := NEHeaderOffset + $80;  // After entry table
  
  // FNT resource starts at 0x600 (aligned)
  FNTOffset := $600;
  
  // ===== Write MZ (DOS) Header =====
  WriteWord(EXE, $5A4D);              // 0x00: 'MZ' signature
  WriteWord(EXE, $0080);              // 0x02: Bytes on last page
  WriteWord(EXE, $0001);              // 0x04: Pages in file
  WriteWord(EXE, $0000);              // 0x06: Relocations
  WriteWord(EXE, $0004);              // 0x08: Header size in paragraphs
  WriteWord(EXE, $0000);              // 0x0A: Min extra paragraphs
  WriteWord(EXE, $FFFF);              // 0x0C: Max extra paragraphs
  WriteWord(EXE, $0000);              // 0x0E: Initial SS
  WriteWord(EXE, $00B8);              // 0x10: Initial SP
  WriteWord(EXE, $0000);              // 0x12: Checksum
  WriteWord(EXE, $0000);              // 0x14: Initial IP
  WriteWord(EXE, $0000);              // 0x16: Initial CS
  WriteWord(EXE, $0040);              // 0x18: Relocation table offset
  WriteWord(EXE, $0000);              // 0x1A: Overlay number
  
  // Reserved words (0x1C - 0x3B)
  for I := 1 to 16 do
    WriteWord(EXE, $0000);
  
  // 0x3C: NE header offset
  WriteDWord(EXE, NEHeaderOffset);
  
  // Pad to 0x80 (DOS stub would go here)
  while EXE.Position < $80 do
    WriteByte(EXE, 0);
  
  // Simple DOS stub message
  WriteString(EXE, 'This is a Windows font file.'#13#10'$', 32);
  
  // Pad to NE header offset
  while EXE.Position < NEHeaderOffset do
    WriteByte(EXE, 0);
  
  // ===== Write NE Header =====
  WriteWord(EXE, $454E);              // 0x00: 'NE' signature
  WriteByte(EXE, $05);                // 0x02: Linker version
  WriteByte(EXE, $0A);                // 0x03: Linker revision
  WriteWord(EXE, EntryTableOffset);   // 0x04: Entry table offset
  WriteWord(EXE, $0000);              // 0x06: Entry table size
  WriteDWord(EXE, $00000000);         // 0x08: CRC (not used)
  WriteWord(EXE, $8000);              // 0x0C: Flags (library)
  WriteWord(EXE, $0000);              // 0x0E: Auto data segment
  WriteWord(EXE, $0000);              // 0x10: Heap size
  WriteWord(EXE, $0000);              // 0x12: Stack size
  WriteDWord(EXE, $00000000);         // 0x14: CS:IP
  WriteDWord(EXE, $00000000);         // 0x18: SS:SP
  WriteWord(EXE, $0000);              // 0x1C: Segment count
  WriteWord(EXE, $0000);              // 0x1E: Module reference count
  WriteWord(EXE, $0000);              // 0x20: Non-resident name table size
  WriteWord(EXE, SegmentTableOffset); // 0x22: Segment table offset
  WriteWord(EXE, ResourceTableOffset);// 0x24: Resource table offset
  WriteWord(EXE, ResidentNameTableOffset); // 0x26: Resident name table offset
  WriteWord(EXE, ModuleRefTableOffset);    // 0x28: Module reference table offset
  WriteWord(EXE, ImportedNameTableOffset); // 0x2A: Imported names table offset
  WriteDWord(EXE, NonResidentNameTableOffset); // 0x2C: Non-resident name table offset
  WriteWord(EXE, $0000);              // 0x30: Moveable entry points
  WriteWord(EXE, AlignShift);         // 0x32: Alignment shift count
  WriteWord(EXE, $0001);              // 0x34: Resource segments count
  WriteByte(EXE, $02);                // 0x36: Target OS (Windows)
  WriteByte(EXE, $08);                // 0x37: Additional flags (proportional font)
  WriteWord(EXE, $0000);              // 0x38: Fast load offset
  WriteWord(EXE, $0000);              // 0x3A: Fast load size
  WriteWord(EXE, $0000);              // 0x3C: Reserved
  WriteWord(EXE, $0300);              // 0x3E: Expected Windows version
  
  // ===== Write Resource Table (offset 0x40 from NE) =====
  // Alignment shift
  WriteWord(EXE, AlignShift);
  
  // Resource type: RT_FONT (0x8008)
  WriteWord(EXE, $8008);              // Type ID
  WriteWord(EXE, $0001);              // Count of resources
  WriteDWord(EXE, $00000000);         // Reserved
  
  // Font resource entry
  WriteWord(EXE, FNTOffset shr AlignShift);  // Offset (in alignment units)
  WriteWord(EXE, (FNTSize + (1 shl AlignShift) - 1) shr AlignShift); // Size (in alignment units)
  WriteWord(EXE, $0C50);              // Flags (moveable, preload, discardable)
  WriteWord(EXE, $0001);              // Resource ID
  WriteDWord(EXE, $00000000);         // Reserved
  
  // Resource type: RT_FONTDIR (0x8007)
  WriteWord(EXE, $8007);              // Type ID
  WriteWord(EXE, $0001);              // Count
  WriteDWord(EXE, $00000000);         // Reserved
  
  // Font directory resource (points to font info)
  // We'll place FONTDIR right after the FNT data
  WriteWord(EXE, (FNTOffset + FNTSize + 15) shr AlignShift); // Offset
  WriteWord(EXE, $0010);              // Size (minimal)
  WriteWord(EXE, $0C50);              // Flags
  WriteWord(EXE, $0001);              // Resource ID
  WriteDWord(EXE, $00000000);         // Reserved
  
  // End of resource types
  WriteWord(EXE, $0000);
  
  // ===== Write Resident Name Table =====
  // Module name (length-prefixed string)
  NameLen := Length(FFontName);
  if NameLen > 8 then NameLen := 8;
  WriteByte(EXE, NameLen);
  for I := 1 to NameLen do
    WriteByte(EXE, Ord(UpCase(FFontName[I])));
  WriteWord(EXE, $0000);              // Ordinal
  WriteByte(EXE, $00);                // End of table
  
  // Pad to entry table offset
  while EXE.Position < NEHeaderOffset + EntryTableOffset do
    WriteByte(EXE, 0);
  
  // ===== Entry Table (empty) =====
  WriteByte(EXE, $00);                // End of entry table
  
  // Pad to non-resident name table
  while EXE.Position < NonResidentNameTableOffset do
    WriteByte(EXE, 0);
  
  // ===== Non-Resident Name Table =====
  // Font description
  NameLen := Length(FFontName);
  WriteByte(EXE, NameLen);
  for I := 1 to NameLen do
    WriteByte(EXE, Ord(FFontName[I]));
  WriteWord(EXE, $0000);              // Ordinal
  WriteByte(EXE, $00);                // End of table
  
  // Pad to FNT offset
  while EXE.Position < FNTOffset do
    WriteByte(EXE, 0);
  
  // ===== Write FNT Resource =====
  FNTData.Position := 0;
  EXE.CopyFrom(FNTData, FNTData.Size);
  
  // Pad to alignment
  while (EXE.Position mod (1 shl AlignShift)) <> 0 do
    WriteByte(EXE, 0);
  
  // ===== Write FONTDIR Resource =====
  // Minimal font directory entry
  WriteWord(EXE, $0001);              // Number of fonts
  WriteWord(EXE, $0001);              // Font ordinal
  // Copy essential header info (first 113 bytes of FNT)
  FNTData.Position := 0;
  for I := 1 to 113 do
  begin
    if FNTData.Position < FNTData.Size then
      EXE.CopyFrom(FNTData, 1)
    else
      WriteByte(EXE, 0);
  end;
  // Device name (empty)
  WriteByte(EXE, 0);
  // Face name
  for I := 1 to Length(FFontName) do
    WriteByte(EXE, Ord(FFontName[I]));
  WriteByte(EXE, 0);
  
  Result := EXE;
end;

procedure TFONCreator.SaveToFile(const FileName: string);
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

procedure TFONCreator.SaveToStream(Stream: TStream);
var
  FNTData: TMemoryStream;
  EXEData: TMemoryStream;
begin
  if FHeight = 0 then
    raise Exception.Create('No characters defined or font height not set');
  
  UpdateCharRange;
  
  // Build FNT resource
  FNTData := BuildFNTResource;
  try
    // Build complete NE executable
    EXEData := BuildNEExecutable(FNTData);
    try
      EXEData.Position := 0;
      Stream.CopyFrom(EXEData, EXEData.Size);
    finally
      EXEData.Free;
    end;
  finally
    FNTData.Free;
  end;
end;

end.
