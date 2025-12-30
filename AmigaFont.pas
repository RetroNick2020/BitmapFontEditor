{
  AmigaFont.pas - Amiga Font Reader Unit for Bitmap Font Editor
  
  Reads Amiga OS 1.3/Workbench 1.3 font files (the data files like "11", "8", etc.)
  These are Amiga executable files with embedded font data.
  
  SUPPORTED FONTS:
  - Fixed-width fonts (like Topaz) - WORKING
  - Proportional fonts (like Times, Courier) - WORKING
  - Italic/oblique fonts (like Emerald) - WORKING
  
  TECHNICAL NOTES:
  - Amiga uses Motorola 68000 (big-endian byte order)
  - File structure: Hunk header + code stub + font structure at 0x6E
  - Pointers in font structure need +32 adjustment for file offsets
  - CharLoc format: First WORD = bit offset, Second WORD = bit width
  - NumChars = HiChar - LoChar + 2 (includes notdef glyph)
  - Bit extraction: k = location + col + row * modulo * 8
  
  Based on afont.c by Mark Craig
}

unit AmigaFont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

type
  TAmigaCharLoc = record
    BitOffset: Word;
    BitWidth: Word;
  end;
  
  TAmigaFont = class
  private
    FYSize: Word;
    FXSize: Word;
    FBaseline: Word;
    FLoChar: Byte;
    FHiChar: Byte;
    FModulo: Word;
    FNumChars: Word;
    FFontName: string;
    FLoaded: Boolean;
    
    FCharLoc: array of TAmigaCharLoc;
    FCharSpace: array of SmallInt;
    FCharKern: array of SmallInt;
    FCharData: array of Byte;
    
    function ReadBEWord(const Data: array of Byte; Offset: Integer): Word;
    function ReadBELong(const Data: array of Byte; Offset: Integer): LongWord;
    function GetGlyphPixel(CharIndex: Integer; Col, Row: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadFromFile(const FileName: string): Boolean;
    function GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
    function GetCharWidth(CharCode: Integer): Integer;
    
    procedure Clear;
    
    property YSize: Word read FYSize;
    property XSize: Word read FXSize;
    property Baseline: Word read FBaseline;
    property LoChar: Byte read FLoChar;
    property HiChar: Byte read FHiChar;
    property NumChars: Word read FNumChars;
    property FontName: string read FFontName;
    property Loaded: Boolean read FLoaded;
  end;

implementation

constructor TAmigaFont.Create;
begin
  inherited Create;
  Clear;
end;

destructor TAmigaFont.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TAmigaFont.Clear;
begin
  FYSize := 0;
  FXSize := 0;
  FBaseline := 0;
  FLoChar := 32;
  FHiChar := 127;
  FModulo := 0;
  FNumChars := 0;
  FFontName := '';
  FLoaded := False;
  SetLength(FCharLoc, 0);
  SetLength(FCharSpace, 0);
  SetLength(FCharKern, 0);
  SetLength(FCharData, 0);
end;

function TAmigaFont.ReadBEWord(const Data: array of Byte; Offset: Integer): Word;
begin
  if Offset + 1 <= High(Data) then
    Result := (Word(Data[Offset]) shl 8) or Data[Offset + 1]
  else
    Result := 0;
end;

function TAmigaFont.ReadBELong(const Data: array of Byte; Offset: Integer): LongWord;
begin
  if Offset + 3 <= High(Data) then
    Result := (LongWord(Data[Offset]) shl 24) or
              (LongWord(Data[Offset + 1]) shl 16) or
              (LongWord(Data[Offset + 2]) shl 8) or
              LongWord(Data[Offset + 3])
  else
    Result := 0;
end;

function TAmigaFont.GetGlyphPixel(CharIndex: Integer; Col, Row: Integer): Boolean;
var
  K, Pos, Ind: Integer;
  BitOffset: Integer;
begin
  Result := False;
  
  if (CharIndex < 0) or (CharIndex >= Length(FCharLoc)) then Exit;
  if (Col < 0) or (Col >= FCharLoc[CharIndex].BitWidth) then Exit;
  if (Row < 0) or (Row >= FYSize) then Exit;
  
  BitOffset := FCharLoc[CharIndex].BitOffset;
  
  // Amiga bit extraction formula:
  // k = location + col + row * modulo * 8
  K := BitOffset + Col + Row * FModulo * 8;
  Pos := K div 8;
  Ind := 7 - (K mod 8);
  
  if Pos < Length(FCharData) then
    Result := (FCharData[Pos] and (1 shl Ind)) <> 0;
end;

function TAmigaFont.LoadFromFile(const FileName: string): Boolean;
var
  FS: TFileStream;
  FileData: array of Byte;
  FileSize: Int64;
  FontOffset: Integer;
  CharDataPtr, CharLocPtr, CharSpacePtr, CharKernPtr: LongWord;
  CharDataOffset, CharLocOffset, CharSpaceOffset, CharKernOffset: Integer;
  I: Integer;
  DataSize: Integer;
  MaxBitOffset: Integer;
  HunkType: LongWord;
begin
  Result := False;
  Clear;
  
  if not FileExists(FileName) then Exit;
  
  try
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FS.Size;
      if FileSize < 32 then Exit;
      
      SetLength(FileData, FileSize);
      FS.ReadBuffer(FileData[0], FileSize);
    finally
      FS.Free;
    end;
  except
    Exit;
  end;
  
  // Check for Amiga HUNK_HEADER
  HunkType := ReadBELong(FileData, 0);
  if HunkType <> $000003F3 then
    Exit;
  
  // Font structure is at offset 0x6E (after hunk header + code stub + name)
  FontOffset := $6E;
  
  if FontOffset + 32 >= FileSize then
    Exit;
  
  // Parse the TextFont structure
  FYSize := ReadBEWord(FileData, FontOffset);
  // Style at offset 2 (byte), Flags at offset 3 (byte)
  FXSize := ReadBEWord(FileData, FontOffset + 4);
  FBaseline := ReadBEWord(FileData, FontOffset + 6) + 1;
  // BoldSmear at offset 8
  // Accessors at offset 10
  FLoChar := FileData[FontOffset + 12];
  FHiChar := FileData[FontOffset + 13];
  // Modulo is at offset 18
  FModulo := ReadBEWord(FileData, FontOffset + 18);
  
  // Validate parsed values
  if (FYSize < 4) or (FYSize > 64) or
     (FXSize < 1) or (FXSize > 64) or
     (FLoChar > FHiChar) then
    Exit;
  
  // NumChars includes the "notdef" glyph at the end
  FNumChars := FHiChar - FLoChar + 2;
  
  // Get CharLoc offset from pointer + 32
  CharLocPtr := ReadBELong(FileData, FontOffset + 20);
  CharLocOffset := CharLocPtr + 32;
  
  // Get CharData offset from pointer + 32
  CharDataPtr := ReadBELong(FileData, FontOffset + 14);
  CharDataOffset := CharDataPtr + 32;
  
  // Validate offsets
  if (CharLocOffset < 0) or (CharLocOffset + FNumChars * 4 > FileSize) then
    Exit;
  if (CharDataOffset < 0) or (CharDataOffset > FileSize) then
    Exit;
  
  // Load CharLoc table
  SetLength(FCharLoc, FNumChars);
  MaxBitOffset := 0;
  
  for I := 0 to FNumChars - 1 do
  begin
    // CharLoc format: first word = bit offset, second word = bit width
    FCharLoc[I].BitOffset := ReadBEWord(FileData, CharLocOffset + I * 4);
    FCharLoc[I].BitWidth := ReadBEWord(FileData, CharLocOffset + I * 4 + 2);
    
    if FCharLoc[I].BitOffset + FCharLoc[I].BitWidth > MaxBitOffset then
      MaxBitOffset := FCharLoc[I].BitOffset + FCharLoc[I].BitWidth;
  end;
  
  // Validate/calculate Modulo
  if (FModulo = 0) or (FModulo < (MaxBitOffset + 7) div 8) then
    FModulo := (MaxBitOffset + 7) div 8;
  
  // Load CharSpace table (proportional fonts only)
  CharSpacePtr := ReadBELong(FileData, FontOffset + 24);
  if CharSpacePtr <> 0 then
  begin
    CharSpaceOffset := CharSpacePtr + 32;
    if (CharSpaceOffset > 0) and (CharSpaceOffset + FNumChars * 2 <= FileSize) then
    begin
      SetLength(FCharSpace, FNumChars);
      for I := 0 to FNumChars - 1 do
        FCharSpace[I] := SmallInt(ReadBEWord(FileData, CharSpaceOffset + I * 2));
    end;
  end;
  
  // Load CharKern table (proportional fonts only)
  CharKernPtr := ReadBELong(FileData, FontOffset + 28);
  if CharKernPtr <> 0 then
  begin
    CharKernOffset := CharKernPtr + 32;
    if (CharKernOffset > 0) and (CharKernOffset + FNumChars * 2 <= FileSize) then
    begin
      SetLength(FCharKern, FNumChars);
      for I := 0 to FNumChars - 1 do
        FCharKern[I] := SmallInt(ReadBEWord(FileData, CharKernOffset + I * 2));
    end;
  end;
  
  // Load bitmap data
  DataSize := FModulo * FYSize;
  
  if CharDataOffset + DataSize > FileSize then
    DataSize := FileSize - CharDataOffset;
  
  if DataSize > 0 then
  begin
    SetLength(FCharData, DataSize);
    Move(FileData[CharDataOffset], FCharData[0], DataSize);
  end
  else
    Exit;
  
  FFontName := ChangeFileExt(ExtractFileName(FileName), '');
  FLoaded := (Length(FCharData) > 0) and (Length(FCharLoc) > 0);
  Result := FLoaded;
end;

function TAmigaFont.GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
var
  CharIndex, X, Y, CharWidth: Integer;
begin
  Result := False;
  
  if not FLoaded then Exit;
  if (CharCode < FLoChar) or (CharCode > FHiChar) then Exit;
  
  CharIndex := CharCode - FLoChar;
  if CharIndex >= Length(FCharLoc) then Exit;
  
  CharWidth := FCharLoc[CharIndex].BitWidth;
  if CharWidth = 0 then
  begin
    // Use CharSpace if available, or XSize
    if (Length(FCharSpace) > CharIndex) and (FCharSpace[CharIndex] > 0) then
      CharWidth := FCharSpace[CharIndex]
    else
      CharWidth := FXSize;
  end;
  
  if CharWidth <= 0 then Exit;
  
  Bmp.Width := CharWidth;
  Bmp.Height := FYSize;
  Bmp.Canvas.Brush.Color := clWhite;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
  
  // Only draw if we have bitmap data for this char
  if FCharLoc[CharIndex].BitWidth > 0 then
  begin
    for Y := 0 to FYSize - 1 do
    begin
      for X := 0 to FCharLoc[CharIndex].BitWidth - 1 do
      begin
        if GetGlyphPixel(CharIndex, X, Y) then
          Bmp.Canvas.Pixels[X, Y] := clBlack;
      end;
    end;
  end;
  
  Result := True;
end;

function TAmigaFont.GetCharWidth(CharCode: Integer): Integer;
var
  CharIndex: Integer;
begin
  Result := 0;
  if not FLoaded then Exit;
  if (CharCode < FLoChar) or (CharCode > FHiChar) then Exit;
  
  CharIndex := CharCode - FLoChar;
  
  // For proportional fonts, use CharSpace (spacing advance)
  if (Length(FCharSpace) > CharIndex) and (FCharSpace[CharIndex] > 0) then
    Result := FCharSpace[CharIndex]
  else if CharIndex < Length(FCharLoc) then
    Result := FCharLoc[CharIndex].BitWidth
  else
    Result := FXSize;
  
  // Ensure minimum width
  if Result <= 0 then
    Result := FXSize;
end;

end.
