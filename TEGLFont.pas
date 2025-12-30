{
  TEGLFont.pas - TEGL Windows ToolKit II Font Format Unit
  
  Supports loading and saving TEGL bitmap fonts (.RTF, .FNT)
  
  File Structure:
    Header (10 bytes):
      [00-01] Signature: 'TR' (0x54, 0x52) - reads as $5254 little-endian
      [02]    Font height in pixels
      [03]    Font width (max width value)
      [04]    ASCII range start character
      [05]    ASCII range end character
      [06]    Top line (ascender position)
      [07]    Baseline position
      [08]    Bottom line (descender position)
      [09]    Bytes per row (1 for <=8 pixels, 2 for <=16, etc.)
      
    Character Width Table (256 bytes):
      [10-265] Width of each character (0-255), 1 byte per character
               Characters outside defined range have width 0
               
    Bitmap Data:
      [266+]   Bitmap for each character from range start to end
               Each character uses: height * bytes_per_row bytes
               Bits stored MSB first (bit 7 = leftmost pixel)
}

unit TEGLFont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

const
  TEGL_SIGNATURE = $5254;  // 'TR' as little-endian word

type
  TTEGLHeader = packed record
    Signature: Word;      // 'TR' = $5254 as little-endian
    Height: Byte;         // Font height in pixels
    WidthFixed: Byte;     // Maximum/fixed width
    RangeStart: Byte;     // First character (usually 28 or 32)
    RangeEnd: Byte;       // Last character (usually 126)
    TopLine: Byte;        // Ascender line
    BaseLine: Byte;       // Baseline position
    BottomLine: Byte;     // Descender line
    BytesPerRow: Byte;    // Bytes per character row (1, 2, etc)
  end;

  TTEGLFont = class
  private
    FHeader: TTEGLHeader;
    FWidths: array[0..255] of Byte;
    FBitmaps: array of Byte;
    FLoaded: Boolean;
    FFontName: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadFromFile(const FileName: string): Boolean;
    function SaveToFile(const FileName: string): Boolean;
    
    function GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
    procedure SetGlyphFromBitmap(CharCode: Integer; Bmp: TBitmap);
    function GetCharWidth(CharCode: Integer): Integer;
    
    procedure Clear;
    procedure SetProperties(AHeight, ABaseline, ARangeStart, ARangeEnd: Integer);
    
    property Height: Byte read FHeader.Height write FHeader.Height;
    property MaxWidth: Byte read FHeader.WidthFixed write FHeader.WidthFixed;
    property RangeStart: Byte read FHeader.RangeStart write FHeader.RangeStart;
    property RangeEnd: Byte read FHeader.RangeEnd write FHeader.RangeEnd;
    property TopLine: Byte read FHeader.TopLine write FHeader.TopLine;
    property BaseLine: Byte read FHeader.BaseLine write FHeader.BaseLine;
    property BottomLine: Byte read FHeader.BottomLine write FHeader.BottomLine;
    property BytesPerRow: Byte read FHeader.BytesPerRow write FHeader.BytesPerRow;
    property Loaded: Boolean read FLoaded;
    property FontName: string read FFontName write FFontName;
  end;

implementation

constructor TTEGLFont.Create;
begin
  inherited Create;
  FillChar(FHeader, SizeOf(FHeader), 0);
  FillChar(FWidths, SizeOf(FWidths), 0);
  FLoaded := False;
  FFontName := 'TEGLFONT';
end;

destructor TTEGLFont.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTEGLFont.Clear;
begin
  SetLength(FBitmaps, 0);
  FillChar(FHeader, SizeOf(FHeader), 0);
  FillChar(FWidths, SizeOf(FWidths), 0);
  FLoaded := False;
end;

procedure TTEGLFont.SetProperties(AHeight, ABaseline, ARangeStart, ARangeEnd: Integer);
var
  NumChars, BitmapSize: Integer;
begin
  FHeader.Signature := TEGL_SIGNATURE;
  FHeader.Height := AHeight;
  FHeader.WidthFixed := 8;  // Default
  FHeader.RangeStart := ARangeStart;
  FHeader.RangeEnd := ARangeEnd;
  FHeader.TopLine := 0;
  FHeader.BaseLine := ABaseline;
  FHeader.BottomLine := AHeight;
  FHeader.BytesPerRow := 1;  // Default to 8-pixel width
  
  NumChars := ARangeEnd - ARangeStart + 1;
  BitmapSize := NumChars * AHeight * FHeader.BytesPerRow;
  SetLength(FBitmaps, BitmapSize);
  FillChar(FBitmaps[0], BitmapSize, 0);
  
  FLoaded := True;
end;

function TTEGLFont.LoadFromFile(const FileName: string): Boolean;
var
  FS: TFileStream;
  BitmapSize, NumChars, Remaining: Integer;
begin
  Result := False;
  Clear;
  
  if not FileExists(FileName) then Exit;
  
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if FS.Size < 266 then Exit;  // Minimum: 10 header + 256 widths
    
    // Read header
    FS.ReadBuffer(FHeader, SizeOf(FHeader));
    
    // Check signature
    if FHeader.Signature <> TEGL_SIGNATURE then Exit;
    
    // Validate header values
    if FHeader.Height = 0 then Exit;
    if FHeader.RangeEnd < FHeader.RangeStart then Exit;
    if FHeader.BytesPerRow = 0 then FHeader.BytesPerRow := 1;
    
    // Read width table
    FS.ReadBuffer(FWidths, 256);
    
    // Calculate bitmap size
    NumChars := FHeader.RangeEnd - FHeader.RangeStart + 1;
    BitmapSize := NumChars * FHeader.Height * FHeader.BytesPerRow;
    
    // Check remaining file size
    Remaining := FS.Size - FS.Position;
    if Remaining < BitmapSize then
    begin
      // Handle short files (common issue)
      BitmapSize := Remaining;
    end;
    
    if BitmapSize > 0 then
    begin
      SetLength(FBitmaps, BitmapSize);
      FS.ReadBuffer(FBitmaps[0], BitmapSize);
    end;
    
    FFontName := ChangeFileExt(ExtractFileName(FileName), '');
    FLoaded := True;
    Result := True;
  finally
    FS.Free;
  end;
end;

function TTEGLFont.SaveToFile(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  
  // Ensure signature is set
  FHeader.Signature := TEGL_SIGNATURE;
  
  FS := TFileStream.Create(FileName, fmCreate);
  try
    // Write header
    FS.WriteBuffer(FHeader, SizeOf(FHeader));
    
    // Write width table
    FS.WriteBuffer(FWidths, 256);
    
    // Write bitmap data
    if Length(FBitmaps) > 0 then
      FS.WriteBuffer(FBitmaps[0], Length(FBitmaps));
    
    Result := True;
  finally
    FS.Free;
  end;
end;

function TTEGLFont.GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
var
  CharIndex, Offset: Integer;
  X, Y, ByteIdx, Col: Integer;
  RowByte: Byte;
  CharWidth: Integer;
begin
  Result := False;
  
  if not FLoaded then Exit;
  if (CharCode < FHeader.RangeStart) or (CharCode > FHeader.RangeEnd) then Exit;
  
  CharWidth := FWidths[CharCode];
  if CharWidth = 0 then Exit;
  
  CharIndex := CharCode - FHeader.RangeStart;
  Offset := CharIndex * FHeader.Height * FHeader.BytesPerRow;
  
  if Offset + FHeader.Height * FHeader.BytesPerRow > Length(FBitmaps) then Exit;
  
  Bmp.Width := CharWidth;
  Bmp.Height := FHeader.Height;
  Bmp.Canvas.Brush.Color := clWhite;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
  
  for Y := 0 to FHeader.Height - 1 do
  begin
    for ByteIdx := 0 to FHeader.BytesPerRow - 1 do
    begin
      if Offset + Y * FHeader.BytesPerRow + ByteIdx < Length(FBitmaps) then
      begin
        RowByte := FBitmaps[Offset + Y * FHeader.BytesPerRow + ByteIdx];
        for Col := 0 to 7 do
        begin
          X := ByteIdx * 8 + Col;
          if X < CharWidth then
          begin
            if (RowByte and ($80 shr Col)) <> 0 then
              Bmp.Canvas.Pixels[X, Y] := clBlack;
          end;
        end;
      end;
    end;
  end;
  
  Result := True;
end;

procedure TTEGLFont.SetGlyphFromBitmap(CharCode: Integer; Bmp: TBitmap);
var
  CharIndex, Offset: Integer;
  X, Y, ByteIdx, Col: Integer;
  RowByte: Byte;
  NumChars, NewBitmapSize: Integer;
begin
  if (CharCode < FHeader.RangeStart) or (CharCode > FHeader.RangeEnd) then Exit;
  
  // Set width
  FWidths[CharCode] := Bmp.Width;
  
  // Update max width if needed
  if Bmp.Width > FHeader.WidthFixed then
    FHeader.WidthFixed := Bmp.Width;
  
  // Update bytes per row if needed
  if (Bmp.Width + 7) div 8 > FHeader.BytesPerRow then
    FHeader.BytesPerRow := (Bmp.Width + 7) div 8;
  
  // Ensure bitmap array is large enough
  NumChars := FHeader.RangeEnd - FHeader.RangeStart + 1;
  NewBitmapSize := NumChars * FHeader.Height * FHeader.BytesPerRow;
  if Length(FBitmaps) < NewBitmapSize then
  begin
    SetLength(FBitmaps, NewBitmapSize);
    FillChar(FBitmaps[0], NewBitmapSize, 0);
  end;
  
  CharIndex := CharCode - FHeader.RangeStart;
  Offset := CharIndex * FHeader.Height * FHeader.BytesPerRow;
  
  // Clear existing bitmap for this character
  FillChar(FBitmaps[Offset], FHeader.Height * FHeader.BytesPerRow, 0);
  
  // Convert bitmap to packed bits
  for Y := 0 to Min(Bmp.Height, FHeader.Height) - 1 do
  begin
    for ByteIdx := 0 to FHeader.BytesPerRow - 1 do
    begin
      RowByte := 0;
      for Col := 0 to 7 do
      begin
        X := ByteIdx * 8 + Col;
        if (X < Bmp.Width) then
        begin
          if Bmp.Canvas.Pixels[X, Y] = clBlack then
            RowByte := RowByte or ($80 shr Col);
        end;
      end;
      FBitmaps[Offset + Y * FHeader.BytesPerRow + ByteIdx] := RowByte;
    end;
  end;
  
  FLoaded := True;
end;

function TTEGLFont.GetCharWidth(CharCode: Integer): Integer;
begin
  if (CharCode >= 0) and (CharCode <= 255) then
    Result := FWidths[CharCode]
  else
    Result := 0;
end;

end.
