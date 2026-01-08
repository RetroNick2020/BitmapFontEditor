{
  APLFont.pas - Alaveri Pascal Library Font Format Unit
  
  Supports loading and saving APL/Veridian bitmap fonts (.FNT)
  
  File Structure:
    Header (318 bytes):
      [00-01] MajorVersion: SmallInt (2 bytes)
      [02-03] MinorVersion: SmallInt (2 bytes)
      [04-36] Name: String[50] (51 bytes: 1 length + 50 chars)
      [37-3A] DataSize: LongInt (4 bytes, uncompressed bitmap size)
      [3B]    MaxWidth: Byte
      [3C]    Height: Byte
      [3D]    Compression: Byte (0=None, 1=LZW)
      [3E]    Spacing: Byte
      [3F]    Format: Byte (0=Monospace, 1=Proportional)
      [40]    Unused: Byte
      [41]    CompressionLevel: Byte (0=Low/12bit, 1=Med/12bit, 2=High/13bit)
      [42]    BytesPerCharLine: Byte (1 for <=8 pixels, 2 for <=16 pixels)
      [43-13D] Reserved: 251 bytes (zeroed)
      
    Compressed Data:
      [13E]      BitSize: Byte (12 or 13)
      [13F-142]  OriginalSize: LongInt (same as DataSize)
      [143+]     LZW compressed bitmap data
      
    Font Data (uncompressed):
      Bitmap data for 256 characters (0-255)
      Each char = Height * BytesPerCharLine bytes
      MSB-first pixel ordering (bit 7 = leftmost pixel)
      
    LZW Special Codes:
      256 = EndOfStream
      257 = IncreaseCodeSize  
      258 = ClearDictionary
      259 = EmptyCode (unused)
      260+ = Dictionary entries
}

unit APLFont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Math;

const
  APL_FONT_MAJOR_VERSION = 1;
  APL_FONT_MINOR_VERSION = 2;
  APL_HEADER_SIZE = 318;  // Actual header size

type
  TAPLCompression = (acNone = 0, acLzw = 1, acGif = 2);
  TAPLFontFormat = (ffMonospace = 0, ffProportional = 1, ffSystem = 2, ffColored = 3);
  TAPLCompressionLevel = (clLow = 0, clMedium = 1, clHigh = 2);

  // Packed record matching Turbo Pascal layout
  TAPLFontHeader = packed record
    MajorVersion: SmallInt;        // Offset 0x00 (2 bytes)
    MinorVersion: SmallInt;        // Offset 0x02 (2 bytes)
    Name: array[0..50] of Byte;    // Offset 0x04 (51 bytes) - Pascal string
    DataSize: LongInt;             // Offset 0x37 (4 bytes)
    MaxWidth: Byte;                // Offset 0x3B
    Height: Byte;                  // Offset 0x3C
    Compression: Byte;             // Offset 0x3D
    Spacing: Byte;                 // Offset 0x3E
    Format: Byte;                  // Offset 0x3F
    Unused: Byte;                  // Offset 0x40
    CompressionLevel: Byte;        // Offset 0x41
    BytesPerCharLine: Byte;        // Offset 0x42
    Reserved: array[0..250] of Byte; // Offset 0x43 (251 bytes)
  end;

  { APL LZW Decompressor }
  TAPLLzwDecompressor = class
  private
    // LZW codes
    const
      CODE_END_OF_STREAM = 256;
      CODE_INCREASE_SIZE = 257;
      CODE_CLEAR_DICT = 258;
      CODE_EMPTY = 259;
      CODE_FIRST_ENTRY = 260;
    
  private
    FBitSize: Integer;
    FMaxCode: Integer;
    FCurrentBitSize: Integer;
    FCurrentMaxCode: Integer;
    FNextCode: Integer;
    
    // Bit reading
    FBitBuffer: LongWord;
    FBitsInBuffer: Integer;
    FInputStream: TStream;
    
    // Dictionary
    FDict: array of record
      Prefix: Integer;
      Character: Byte;
    end;
    
    // Decode buffer
    FDecodeBuffer: array of Byte;
    
    function ReadBits(NumBits: Integer): Integer;
    procedure InitCoder;
    function DecodeString(StartCount: Integer; Code: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Decompress(AInput: TStream; var AOutput: TBytes; ABitSize: Integer; AOutputSize: Integer);
  end;

  { APL LZW Compressor }
  TAPLLzwCompressor = class
  private
    const
      CODE_END_OF_STREAM = 256;
      CODE_INCREASE_SIZE = 257;
      CODE_CLEAR_DICT = 258;
      CODE_EMPTY = 259;
      CODE_FIRST_ENTRY = 260;
  private
    FBitSize: Integer;
    FMaxCode: Integer;
    FCurrentBitSize: Integer;
    FCurrentMaxCode: Integer;
    FNextCode: Integer;
    FOverflow: Boolean;
    
    // Bit writing
    FBitBuffer: LongWord;
    FBitsInBuffer: Integer;
    FOutputStream: TStream;
    
    // Dictionary using hash table
    FDict: array of record
      Code: Integer;
      Prefix: Integer;
      Character: Byte;
    end;
    FDictEntries: Integer;
    FHashShift: Integer;
    
    procedure WriteBits(Value: Integer; NumBits: Integer);
    procedure FlushBits;
    procedure InitCoder;
    function FindEntry(APrefix: Integer; AChar: Byte): Integer;
  public
    constructor Create(ABitSize: Integer);
    destructor Destroy; override;
    procedure Compress(const AInput: TBytes; AOutput: TStream);
  end;

  { APL Font class }
  TAPLFont = class
  private
    FHeader: TAPLFontHeader;
    FBitmapData: TBytes;
    FLoaded: Boolean;
    FFontName: string;
    FCharWidths: array[0..255] of Integer;
    
    procedure CalculateCharWidths;
  public
    constructor Create;
    destructor Destroy; override;
    
    function LoadFromFile(const FileName: string): Boolean;
    function SaveToFile(const FileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function SaveToStream(AStream: TStream): Boolean;
    
    function GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
    procedure SetGlyphFromBitmap(CharCode: Integer; Bmp: TBitmap);
    function GetCharWidth(CharCode: Integer): Integer;
    
    procedure Clear;
    procedure SetProperties(AName: string; AHeight, AMaxWidth, ASpacing: Integer;
      AFormat: TAPLFontFormat);
    
    class function IsAPLFont(const FileName: string): Boolean;
    class function IsAPLFontStream(AStream: TStream): Boolean;
    
    property Height: Byte read FHeader.Height write FHeader.Height;
    property MaxWidth: Byte read FHeader.MaxWidth write FHeader.MaxWidth;
    property Spacing: Byte read FHeader.Spacing write FHeader.Spacing;
    property FontFormat: Byte read FHeader.Format write FHeader.Format;
    property BytesPerCharLine: Byte read FHeader.BytesPerCharLine write FHeader.BytesPerCharLine;
    property Loaded: Boolean read FLoaded;
    property FontName: string read FFontName write FFontName;
  end;

implementation

{ TAPLLzwDecompressor }

constructor TAPLLzwDecompressor.Create;
begin
  inherited Create;
  SetLength(FDecodeBuffer, 65536);
end;

destructor TAPLLzwDecompressor.Destroy;
begin
  SetLength(FDict, 0);
  SetLength(FDecodeBuffer, 0);
  inherited Destroy;
end;

procedure TAPLLzwDecompressor.InitCoder;
var
  I: Integer;
begin
  FCurrentBitSize := 9;  // Always start at 9 bits
  FCurrentMaxCode := (1 shl FCurrentBitSize) - 1;
  FNextCode := CODE_FIRST_ENTRY;
  
  // Clear dictionary - entries 0-255 are implicit single-byte values
  for I := 0 to Length(FDict) - 1 do
  begin
    FDict[I].Prefix := -1;
    FDict[I].Character := 0;
  end;
end;

function TAPLLzwDecompressor.ReadBits(NumBits: Integer): Integer;
var
  B: Byte;
begin
  // Read bits LSB first (standard LZW bit packing)
  while FBitsInBuffer < NumBits do
  begin
    if FInputStream.Read(B, 1) <> 1 then
    begin
      Result := CODE_END_OF_STREAM;
      Exit;
    end;
    FBitBuffer := FBitBuffer or (LongWord(B) shl FBitsInBuffer);
    Inc(FBitsInBuffer, 8);
  end;
  
  Result := FBitBuffer and ((1 shl NumBits) - 1);
  FBitBuffer := FBitBuffer shr NumBits;
  Dec(FBitsInBuffer, NumBits);
end;

function TAPLLzwDecompressor.DecodeString(StartCount: Integer; Code: Integer): Integer;
var
  Count: Integer;
begin
  Count := StartCount;
  
  // Follow the chain back to a root character
  while Code > 255 do
  begin
    if Count >= Length(FDecodeBuffer) then
      SetLength(FDecodeBuffer, Length(FDecodeBuffer) * 2);
    FDecodeBuffer[Count] := FDict[Code].Character;
    Inc(Count);
    Code := FDict[Code].Prefix;
  end;
  
  // Code is now a single character (0-255)
  if Count >= Length(FDecodeBuffer) then
    SetLength(FDecodeBuffer, Length(FDecodeBuffer) * 2);
  FDecodeBuffer[Count] := Code;
  Inc(Count);
  
  Result := Count;
end;

procedure TAPLLzwDecompressor.Decompress(AInput: TStream; var AOutput: TBytes; 
  ABitSize: Integer; AOutputSize: Integer);
var
  OldCode, Code: Integer;
  Character: Integer;
  Count: Integer;
  I: Integer;
  OutPos: Integer;
begin
  FInputStream := AInput;
  FBitSize := ABitSize;
  FMaxCode := (1 shl ABitSize) - 1;
  FBitBuffer := 0;
  FBitsInBuffer := 0;
  OutPos := 0;
  
  SetLength(AOutput, AOutputSize);
  
  // Allocate dictionary
  if ABitSize = 12 then
    SetLength(FDict, 5021)
  else if ABitSize = 13 then
    SetLength(FDict, 9029)
  else
    SetLength(FDict, 9029);  // Default to 13-bit size
  
  InitCoder;
  
  // Read first code
  OldCode := ReadBits(FCurrentBitSize);
  if (OldCode = CODE_END_OF_STREAM) then Exit;
  
  Character := OldCode;
  
  // Output first character
  if OutPos < AOutputSize then
  begin
    AOutput[OutPos] := OldCode;
    Inc(OutPos);
  end;
  
  // Main decompression loop
  while OutPos < AOutputSize do
  begin
    Code := ReadBits(FCurrentBitSize);
    
    if Code = CODE_END_OF_STREAM then
      Break;
      
    if Code = CODE_INCREASE_SIZE then
    begin
      Inc(FCurrentBitSize);
      FCurrentMaxCode := (1 shl FCurrentBitSize) - 1;
      Continue;
    end;
    
    if Code = CODE_CLEAR_DICT then
    begin
      InitCoder;
      OldCode := ReadBits(FCurrentBitSize);
      if OldCode = CODE_END_OF_STREAM then
        Break;
      Character := OldCode;
      if OutPos < AOutputSize then
      begin
        AOutput[OutPos] := OldCode;
        Inc(OutPos);
      end;
      Continue;
    end;
    
    // Decode the string
    if Code >= FNextCode then
    begin
      // Special case: code not yet in dictionary
      FDecodeBuffer[0] := Character;
      Count := DecodeString(1, OldCode);
    end
    else
      Count := DecodeString(0, Code);
    
    // Get the first character of the decoded string (it's at the end due to reverse order)
    Character := FDecodeBuffer[Count - 1];
    
    // Output the decoded string in reverse order
    for I := Count - 1 downto 0 do
    begin
      if OutPos >= AOutputSize then Break;
      AOutput[OutPos] := FDecodeBuffer[I];
      Inc(OutPos);
    end;
    
    // Add new entry to dictionary
    if FNextCode <= FMaxCode then
    begin
      FDict[FNextCode].Prefix := OldCode;
      FDict[FNextCode].Character := Character;
      Inc(FNextCode);
    end;
    
    OldCode := Code;
  end;
end;

{ TAPLLzwCompressor }

constructor TAPLLzwCompressor.Create(ABitSize: Integer);
begin
  inherited Create;
  FBitSize := ABitSize;
  FMaxCode := (1 shl ABitSize) - 1;
  
  // Dictionary size based on bit size (prime numbers for better hashing)
  if ABitSize = 12 then
    FDictEntries := 5021
  else if ABitSize = 13 then
    FDictEntries := 9029
  else
    FDictEntries := 9029;
    
  FHashShift := ABitSize - 8;
  SetLength(FDict, FDictEntries);
end;

destructor TAPLLzwCompressor.Destroy;
begin
  SetLength(FDict, 0);
  inherited Destroy;
end;

procedure TAPLLzwCompressor.InitCoder;
var
  I: Integer;
begin
  FCurrentBitSize := 9;
  FCurrentMaxCode := (1 shl FCurrentBitSize) - 1;
  FNextCode := CODE_FIRST_ENTRY;
  FOverflow := False;
  
  for I := 0 to FDictEntries - 1 do
    FDict[I].Code := CODE_EMPTY;
end;

function TAPLLzwCompressor.FindEntry(APrefix: Integer; AChar: Byte): Integer;
var
  Index, Offset: Integer;
begin
  // Hash function
  Index := (AChar shl FHashShift) xor APrefix;
  if Index >= FDictEntries then
    Index := Index mod FDictEntries;
    
  if Index = 0 then
    Offset := 1
  else
    Offset := FDictEntries - Index;
  
  while True do
  begin
    if FDict[Index].Code = CODE_EMPTY then
    begin
      Result := -Index - 1;  // Return negative to indicate not found
      Exit;
    end;
    
    if (FDict[Index].Prefix = APrefix) and (FDict[Index].Character = AChar) then
    begin
      Result := FDict[Index].Code;
      Exit;
    end;
    
    Dec(Index, Offset);
    if Index < 0 then
      Inc(Index, FDictEntries);
  end;
end;

procedure TAPLLzwCompressor.WriteBits(Value: Integer; NumBits: Integer);
var
  B: Byte;
begin
  FBitBuffer := FBitBuffer or (LongWord(Value) shl FBitsInBuffer);
  Inc(FBitsInBuffer, NumBits);
  
  while FBitsInBuffer >= 8 do
  begin
    B := FBitBuffer and $FF;
    FOutputStream.Write(B, 1);
    FBitBuffer := FBitBuffer shr 8;
    Dec(FBitsInBuffer, 8);
  end;
end;

procedure TAPLLzwCompressor.FlushBits;
var
  B: Byte;
begin
  if FBitsInBuffer > 0 then
  begin
    B := FBitBuffer and $FF;
    FOutputStream.Write(B, 1);
    FBitBuffer := 0;
    FBitsInBuffer := 0;
  end;
end;

procedure TAPLLzwCompressor.Compress(const AInput: TBytes; AOutput: TStream);
var
  I: Integer;
  Code, FoundCode: Integer;
  Character: Byte;
  Index: Integer;
begin
  FOutputStream := AOutput;
  FBitBuffer := 0;
  FBitsInBuffer := 0;
  
  if Length(AInput) = 0 then Exit;
  
  InitCoder;
  
  Code := AInput[0];
  
  for I := 1 to Length(AInput) - 1 do
  begin
    Character := AInput[I];
    FoundCode := FindEntry(Code, Character);
    
    if FoundCode >= 0 then
    begin
      // Found in dictionary
      Code := FoundCode;
    end
    else
    begin
      // Not found
      
      // Check if we need to increase code size
      if (Code >= FCurrentMaxCode) and (FCurrentBitSize < FBitSize) then
      begin
        WriteBits(CODE_INCREASE_SIZE, FCurrentBitSize);
        Inc(FCurrentBitSize);
        FCurrentMaxCode := (1 shl FCurrentBitSize) - 1;
      end;
      
      // Output current code
      WriteBits(Code, FCurrentBitSize);
      
      // Add new entry if not overflowed
      if FNextCode < FMaxCode then
      begin
        Index := (-FoundCode) - 1;  // Convert back to index
        FDict[Index].Code := FNextCode;
        FDict[Index].Prefix := Code;
        FDict[Index].Character := Character;
        Inc(FNextCode);
      end
      else
        FOverflow := True;
      
      Code := Character;
      
      // Clear dictionary on overflow
      if FOverflow then
      begin
        WriteBits(CODE_CLEAR_DICT, FCurrentBitSize);
        InitCoder;
      end;
    end;
  end;
  
  // Output final code
  WriteBits(Code, FCurrentBitSize);
  
  // Write end of stream
  WriteBits(CODE_END_OF_STREAM, FCurrentBitSize);
  
  FlushBits;
end;

{ TAPLFont }

constructor TAPLFont.Create;
begin
  inherited Create;
  FillChar(FHeader, SizeOf(FHeader), 0);
  FillChar(FCharWidths, SizeOf(FCharWidths), 0);
  SetLength(FBitmapData, 0);
  FLoaded := False;
  FFontName := '';
end;

destructor TAPLFont.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TAPLFont.Clear;
begin
  SetLength(FBitmapData, 0);
  FillChar(FHeader, SizeOf(FHeader), 0);
  FillChar(FCharWidths, SizeOf(FCharWidths), 0);
  FLoaded := False;
  FFontName := '';
end;

procedure TAPLFont.CalculateCharWidths;
var
  CharIdx, X, Y: Integer;
  LineOffset: Integer;
  MaxX: Integer;
  LineByte: Byte;
  LineWord: Word;
  BytesPerLine: Integer;
  TotalWidth, WidthCount: Integer;
  AvgWidth: Integer;
begin
  BytesPerLine := FHeader.BytesPerCharLine;
  if BytesPerLine = 0 then BytesPerLine := 1;
  
  TotalWidth := 0;
  WidthCount := 0;
  
  for CharIdx := 0 to 255 do
  begin
    MaxX := 0;
    
    for Y := 0 to FHeader.Height - 1 do
    begin
      LineOffset := (CharIdx * FHeader.Height + Y) * BytesPerLine;
      
      if LineOffset < Length(FBitmapData) then
      begin
        if BytesPerLine = 1 then
        begin
          // Single byte: MSB = leftmost pixel
          LineByte := FBitmapData[LineOffset];
          for X := 0 to 7 do
          begin
            if (LineByte and ($80 shr X)) <> 0 then
              if X + 1 > MaxX then MaxX := X + 1;
          end;
        end
        else if (BytesPerLine >= 2) and (LineOffset + 1 < Length(FBitmapData)) then
        begin
          // Two bytes stored as little-endian Word:
          // byte0 = low byte (pixels 8-15), byte1 = high byte (pixels 0-7)
          // Read as Word: bit 15 = pixel 0, bit 0 = pixel 15
          LineWord := FBitmapData[LineOffset] or (Word(FBitmapData[LineOffset + 1]) shl 8);
          for X := 0 to 15 do
          begin
            if (LineWord and ($8000 shr X)) <> 0 then
              if X + 1 > MaxX then MaxX := X + 1;
          end;
        end;
      end;
    end;
    
    FCharWidths[CharIdx] := MaxX;
    if MaxX > 0 then
    begin
      Inc(TotalWidth, MaxX);
      Inc(WidthCount);
    end;
  end;
  
  // Set space width to half average if it's empty
  if WidthCount > 0 then
    AvgWidth := TotalWidth div WidthCount
  else
    AvgWidth := FHeader.MaxWidth div 2;
    
  if FCharWidths[32] <= 1 then
    FCharWidths[32] := AvgWidth div 2;
end;

class function TAPLFont.IsAPLFont(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  if not FileExists(FileName) then Exit;
  
  try
    FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := IsAPLFontStream(FS);
    finally
      FS.Free;
    end;
  except
    Result := False;
  end;
end;

class function TAPLFont.IsAPLFontStream(AStream: TStream): Boolean;
var
  Header: TAPLFontHeader;
  SavePos: Int64;
begin
  Result := False;
  SavePos := AStream.Position;
  
  try
    if AStream.Size - AStream.Position < APL_HEADER_SIZE then Exit;
    
    AStream.ReadBuffer(Header, SizeOf(Header));
    
    // Check for valid APL font signature
    if (Header.MajorVersion = 1) and
       (Header.MinorVersion >= 0) and (Header.MinorVersion <= 2) and
       (Header.Height > 0) and (Header.Height <= 64) and
       (Header.MaxWidth > 0) and (Header.MaxWidth <= 32) and
       (Header.Compression <= 2) and
       (Header.Format <= 3) and
       (Header.BytesPerCharLine >= 1) and (Header.BytesPerCharLine <= 4) then
      Result := True;
  finally
    AStream.Position := SavePos;
  end;
end;

function TAPLFont.LoadFromFile(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  Clear;
  
  if not FileExists(FileName) then Exit;
  
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := LoadFromStream(FS);
    if Result then
      FFontName := ChangeFileExt(ExtractFileName(FileName), '');
  finally
    FS.Free;
  end;
end;

function TAPLFont.LoadFromStream(AStream: TStream): Boolean;
var
  Decompressor: TAPLLzwDecompressor;
  NameLen: Integer;
  BytesPerLine: Integer;
  BitSize: Byte;
  OrigSize: LongInt;
begin
  Result := False;
  Clear;
  
  if AStream.Size - AStream.Position < APL_HEADER_SIZE then Exit;
  
  // Read header
  AStream.ReadBuffer(FHeader, SizeOf(FHeader));
  
  // Validate header
  if (FHeader.MajorVersion <> 1) then Exit;
  if (FHeader.Height = 0) or (FHeader.MaxWidth = 0) then Exit;
  
  // Extract font name from Pascal string
  NameLen := FHeader.Name[0];
  if NameLen > 50 then NameLen := 50;
  SetString(FFontName, PAnsiChar(@FHeader.Name[1]), NameLen);
  
  // Handle BytesPerCharLine
  BytesPerLine := FHeader.BytesPerCharLine;
  if BytesPerLine = 0 then
    BytesPerLine := 1;
  FHeader.BytesPerCharLine := BytesPerLine;
  
  // Decompress or read font data
  case TAPLCompression(FHeader.Compression) of
    acNone:
      begin
        SetLength(FBitmapData, FHeader.DataSize);
        AStream.ReadBuffer(FBitmapData[0], FHeader.DataSize);
      end;
      
    acLzw:
      begin
        // Read bit size byte
        AStream.ReadBuffer(BitSize, 1);
        
        // Read original size (should match DataSize)
        AStream.ReadBuffer(OrigSize, 4);
        
        Decompressor := TAPLLzwDecompressor.Create;
        try
          Decompressor.Decompress(AStream, FBitmapData, BitSize, FHeader.DataSize);
        finally
          Decompressor.Free;
        end;
      end;
  else
    Exit;  // Unsupported compression
  end;
  
  // Calculate character widths for proportional fonts
  CalculateCharWidths;
  
  FLoaded := True;
  Result := True;
end;

function TAPLFont.SaveToFile(const FileName: string): Boolean;
var
  FS: TFileStream;
begin
  Result := False;
  
  FS := TFileStream.Create(FileName, fmCreate);
  try
    Result := SaveToStream(FS);
  finally
    FS.Free;
  end;
end;

function TAPLFont.SaveToStream(AStream: TStream): Boolean;
var
  Compressor: TAPLLzwCompressor;
  NameLen: Integer;
  BitSize: Byte;
  OrigSize: LongInt;
begin
  Result := False;
  
  if Length(FBitmapData) = 0 then Exit;
  
  // Setup header
  FHeader.MajorVersion := APL_FONT_MAJOR_VERSION;
  FHeader.MinorVersion := APL_FONT_MINOR_VERSION;
  
  // Set font name in Pascal string format
  FillChar(FHeader.Name, SizeOf(FHeader.Name), 0);
  NameLen := Length(FFontName);
  if NameLen > 50 then NameLen := 50;
  FHeader.Name[0] := NameLen;
  if NameLen > 0 then
    Move(FFontName[1], FHeader.Name[1], NameLen);
  
  FHeader.DataSize := Length(FBitmapData);
  FHeader.Compression := Ord(acLzw);
  FHeader.CompressionLevel := Ord(clHigh);
  
  // Clear reserved area
  FillChar(FHeader.Reserved, SizeOf(FHeader.Reserved), 0);
  
  // Write header
  AStream.WriteBuffer(FHeader, SizeOf(FHeader));
  
  // Write bit size
  BitSize := 13;  // High compression = 13 bit
  AStream.WriteBuffer(BitSize, 1);
  
  // Write original size
  OrigSize := Length(FBitmapData);
  AStream.WriteBuffer(OrigSize, 4);
  
  // Compress and write data
  Compressor := TAPLLzwCompressor.Create(13);
  try
    Compressor.Compress(FBitmapData, AStream);
  finally
    Compressor.Free;
  end;
  
  Result := True;
end;

procedure TAPLFont.SetProperties(AName: string; AHeight, AMaxWidth, ASpacing: Integer;
  AFormat: TAPLFontFormat);
var
  DataSize: Integer;
  BytesPerLine: Integer;
begin
  Clear;
  
  FFontName := AName;
  FHeader.Height := AHeight;
  FHeader.MaxWidth := AMaxWidth;
  FHeader.Spacing := ASpacing;
  FHeader.Format := Ord(AFormat);
  
  // Calculate bytes per character line
  if AMaxWidth <= 8 then
    BytesPerLine := 1
  else
    BytesPerLine := 2;
  FHeader.BytesPerCharLine := BytesPerLine;
  
  // Allocate bitmap data for 256 characters
  DataSize := 256 * AHeight * BytesPerLine;
  SetLength(FBitmapData, DataSize);
  FillChar(FBitmapData[0], DataSize, 0);
  
  FLoaded := True;
end;

function TAPLFont.GetGlyphBitmap(CharCode: Integer; Bmp: TBitmap): Boolean;
var
  CharOffset, LineOffset: Integer;
  X, Y: Integer;
  LineByte: Byte;
  LineWord: Word;
  BytesPerLine: Integer;
  CharWidth: Integer;
begin
  Result := False;
  
  if not FLoaded then Exit;
  if (CharCode < 0) or (CharCode > 255) then Exit;
  
  BytesPerLine := FHeader.BytesPerCharLine;
  if BytesPerLine = 0 then BytesPerLine := 1;
  
  CharWidth := FCharWidths[CharCode];
  if CharWidth = 0 then
    CharWidth := FHeader.MaxWidth;
  
  CharOffset := CharCode * FHeader.Height * BytesPerLine;
  
  Bmp.Width := CharWidth;
  Bmp.Height := FHeader.Height;
  Bmp.Canvas.Brush.Color := clWhite;
  Bmp.Canvas.FillRect(0, 0, Bmp.Width, Bmp.Height);
  
  for Y := 0 to FHeader.Height - 1 do
  begin
    LineOffset := CharOffset + Y * BytesPerLine;
    
    if LineOffset < Length(FBitmapData) then
    begin
      if BytesPerLine = 1 then
      begin
        // Single byte: MSB = leftmost pixel
        LineByte := FBitmapData[LineOffset];
        for X := 0 to Min(7, CharWidth - 1) do
        begin
          if (LineByte and ($80 shr X)) <> 0 then
            Bmp.Canvas.Pixels[X, Y] := clBlack;
        end;
      end
      else if (BytesPerLine >= 2) and (LineOffset + 1 < Length(FBitmapData)) then
      begin
        // Two bytes stored as little-endian Word:
        // byte0 = low byte (pixels 8-15), byte1 = high byte (pixels 0-7)
        LineWord := FBitmapData[LineOffset] or (Word(FBitmapData[LineOffset + 1]) shl 8);
        for X := 0 to Min(15, CharWidth - 1) do
        begin
          if (LineWord and ($8000 shr X)) <> 0 then
            Bmp.Canvas.Pixels[X, Y] := clBlack;
        end;
      end;
    end;
  end;
  
  Result := True;
end;

procedure TAPLFont.SetGlyphFromBitmap(CharCode: Integer; Bmp: TBitmap);
var
  CharOffset, LineOffset: Integer;
  X, Y: Integer;
  LineByte: Byte;
  LineWord: Word;
  BytesPerLine: Integer;
  NewDataSize: Integer;
begin
  if (CharCode < 0) or (CharCode > 255) then Exit;
  if Bmp = nil then Exit;
  
  BytesPerLine := FHeader.BytesPerCharLine;
  if BytesPerLine = 0 then BytesPerLine := 1;
  
  // Update max width if needed
  if Bmp.Width > FHeader.MaxWidth then
  begin
    FHeader.MaxWidth := Bmp.Width;
    if Bmp.Width > 8 then
    begin
      BytesPerLine := 2;
      FHeader.BytesPerCharLine := 2;
    end;
  end;
  
  // Ensure bitmap data is allocated
  NewDataSize := 256 * FHeader.Height * BytesPerLine;
  if Length(FBitmapData) < NewDataSize then
  begin
    SetLength(FBitmapData, NewDataSize);
    FillChar(FBitmapData[0], NewDataSize, 0);
  end;
  
  CharOffset := CharCode * FHeader.Height * BytesPerLine;
  
  // Set character data from bitmap
  for Y := 0 to Min(Bmp.Height, FHeader.Height) - 1 do
  begin
    LineOffset := CharOffset + Y * BytesPerLine;
    
    if BytesPerLine = 1 then
    begin
      // Single byte: MSB = leftmost pixel
      LineByte := 0;
      for X := 0 to Min(7, Bmp.Width - 1) do
      begin
        if Bmp.Canvas.Pixels[X, Y] = clBlack then
          LineByte := LineByte or ($80 shr X);
      end;
      if LineOffset < Length(FBitmapData) then
        FBitmapData[LineOffset] := LineByte;
    end
    else
    begin
      // Two bytes as little-endian Word: bit 15 = pixel 0
      LineWord := 0;
      for X := 0 to Min(15, Bmp.Width - 1) do
      begin
        if Bmp.Canvas.Pixels[X, Y] = clBlack then
          LineWord := LineWord or ($8000 shr X);
      end;
      // Store as little-endian: low byte first, high byte second
      if LineOffset < Length(FBitmapData) then
        FBitmapData[LineOffset] := LineWord and $FF;
      if LineOffset + 1 < Length(FBitmapData) then
        FBitmapData[LineOffset + 1] := (LineWord shr 8) and $FF;
    end;
  end;
  
  // Update character width
  FCharWidths[CharCode] := Bmp.Width;
  
  FLoaded := True;
end;

function TAPLFont.GetCharWidth(CharCode: Integer): Integer;
begin
  if (CharCode >= 0) and (CharCode <= 255) then
    Result := FCharWidths[CharCode]
  else
    Result := 0;
end;

end.
