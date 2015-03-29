program writing;

{$APPTYPE CONSOLE}

// compiler options
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$ifndef VER140}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$endif}
{$O+}{$R-}{$I-}{$Q-}{$W-}

uses
  Windows, SysUtils, Classes, CachedBuffers;


const
  CORRECT_FILE_NAME = 'correct_file.txt';

{$if CompilerVersion < 19}
type
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ifend}

procedure ConsoleWrite(const StrFmt: string; const Args: array of const;
  const CRLFCount: Integer = 1); overload;
var
  i, Len: Integer;
  S: PChar;
  Buf: string;
begin
  Buf := Format(StrFmt, Args);
  Len := Length(Buf);
  S := pointer(Buf);
  {$ifNdef UNICODE}
  Windows.CharToOemBuffA(S, S, Len);
  {$endif}

  for i := 0 to Len - 1 do
  if (S[i] = #13) then S[i] := #10;

  if (CRLFCount <= 0) then
  begin
    Write(Buf);
  end else
  begin
    Writeln(Buf);
    for i := 2 to CRLFCount do Writeln;
  end;
end;

procedure ConsoleWrite(const Text: string; const CRLFCount: Integer = 1); overload;
begin
  ConsoleWrite(Text, [], CRLFCount);
end;

procedure CompareFiles(const FileName1, FileName2: string;
  const CompactInfo: Boolean = True);
const
  BUFFER_SIZE = 256 * 1024;
var
  Name1, Name2: string;
  F1, F2: THandle;
  S1, S2: Int64;

  Reading: Integer;
  SameFiles: Boolean;

  Buffer1: array[0..BUFFER_SIZE-1] of Byte;
  Buffer2: array[0..BUFFER_SIZE-1] of Byte;

  function OpenReadingFile(const FileName: string; out Size: Int64): THandle;
  begin
    Result := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
    if (NativeInt(Result) < 0) then
      raise Exception.CreateFmt('Cannot read file:'#13'%s', [FileName]);

    Size := FileSeek(Result, Int64(0), FILE_END);
    FileSeek(Result, 0, FILE_BEGIN);
  end;

  procedure ReadBuffer(const F: THandle; var Buffer; const Count: Integer);
  begin
    if (Count <> FileRead(F, Buffer, Count)) then
      raise Exception.Create('Stream read error');
  end;

begin
  Name1 := ExtractFileName(FileName1);
  Name2 := ExtractFileName(FileName2);
  if (CompactInfo) then ConsoleWrite('comparing with correct file... ', 0)
  else ConsoleWrite('Files comparing "%s" and "%s"... ', [Name1, Name2], 0);

  if (not FileExists(FileName1)) then
  begin
    ConsoleWrite('"%s" not found', [Name1]);
    Abort;
  end;
  if (not FileExists(FileName2)) then
  begin
    ConsoleWrite('"%s" not found', [Name2]);
    Abort;
  end;

  F1 := OpenReadingFile(FileName1, S1);
  try
    F2 := OpenReadingFile(FileName2, S2);
    try
      if (S1 <> S2) then
      begin
        ConsoleWrite('different sizes: %d and %d', [S1, S2]);
        Abort;
      end;

      SameFiles := True;
      while (S1 <> 0) do
      begin
        if (S1 < BUFFER_SIZE) then Reading := S1
        else Reading := BUFFER_SIZE;

        ReadBuffer(F1, Buffer1, Reading);
        ReadBuffer(F2, Buffer2, Reading);
        if (not CompareMem(@Buffer1, @Buffer2, Reading)) then
        begin
          SameFiles := False;
          Break;
        end;

        S1 := S1 - Reading;
      end;

      if (not SameFiles) then
      begin
        ConsoleWrite('NOT EQUAL !!!');
        Abort;
      end else
      begin
        ConsoleWrite('binary equal.');
      end;
    finally
      FileClose(F2);
    end;
  finally
    FileClose(F1);
  end;
end;


const
  CRLF_VALUE = 13 or (10 shl 8);
  CRLF: Word = CRLF_VALUE;
  STRINGS_COUNT = 1000;
  ITERATIONS_COUNT = 22000;// div 100;

var
  ProcNumber: Cardinal = 0;

  STRINGS: array[1..STRINGS_COUNT] of record
    Len: Integer;
    S: AnsiString;
  end;

  procedure INITIALIZE_STRINGS;
  var
    i: Integer;
  begin
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      S := AnsiString(IntToStr(i));
      Len := Length(S);
    end;
  end;


type
  TBenchmarkProc = procedure(const FileName: string);

// standard way to write data to Stream
procedure StreamWriter(Stream: TStream);
var
  iteration, i: Integer;
begin
  for iteration := 1 to ITERATIONS_COUNT do
  for i := Low(STRINGS) to High(STRINGS) do
  with STRINGS[i] do
  begin
    Stream.Write(Pointer(S)^, Len);
    Stream.Write(CRLF, SizeOf(CRLF));
  end;
end;

// use TFileStream to write data
procedure FileStreamWriter(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    StreamWriter(Stream);
  finally
    Stream.Free;
  end;
end;

// write to TMemoryStream and SaveToFile
procedure MemoryStreamWriter(const FileName: string);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    StreamWriter(Stream);
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
end;

// use standard TextFile + 64k buffer
procedure BufferedTextFileWriter(const FileName: string);
var
  iteration, i: Integer;
  F: TextFile;
  Buffer: array[Word] of Byte;
begin
  AssignFile(F, FileName);
  ReWrite(F);
  SetTextBuf(F, Buffer);
  try
    for iteration := 1 to ITERATIONS_COUNT do
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      Writeln(F, S);
    end;

  finally
    CloseFile(F);
  end;
end;

// simple way to write data with TCachedFileWriter
// Write method is like TStream.Write(Buffer, Size)
procedure SimpleCachedFileWriter(const FileName: string);
var
  iteration, i: Integer;
  Writer: TCachedFileWriter;
begin
//  Writer.Initialize(FileName);
  try
    for iteration := 1 to ITERATIONS_COUNT do
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      Writer.Write(Pointer(S)^, Len);
      Writer.Write(CRLF, SizeOf(CRLF));
    end;

  finally
//    Writer.Finalize;
  end;
end;

// difficult but the fastest way to write data with TCachedFileWriter
// you should use Current, Margin and Flush. [optional additional memory]
procedure FastCachedFileWriter(const FileName: string);
var
  iteration, i: Integer;
  Writer: TCachedFileWriter;
begin
//  Writer.Initialize(FileName);
  try
    for iteration := 1 to ITERATIONS_COUNT do
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      if (Len <= SizeOf(Int64)) then
      begin
        // fast Move
        if (Len <= SizeOf(Integer)) then PInteger(Writer.Current)^ := PInteger(S)^
        else PInt64(Writer.Current)^ := PInt64(S)^;

        Inc(NativeInt(Writer.Current), Len);
//        Dec(Writer.Margin, Len);
      end else
      begin
        // smart Move
        Writer.Write(pointer(S), Len);
      end;

      // CRLF
      PWord(Writer.Current)^ := CRLF_VALUE;
      Inc(NativeInt(Writer.Current), SizeOf(Word));
//      Dec(Writer.Margin, SizeOf(Word));

      // Flush if needed
//      if (Writer.Margin <= 0) then Writer.Flush;
    end;

  finally
//    Writer.Finalize;
  end;
end;


// generate file name and note the time
procedure RunBenchmarkProc(const Description: string; const Proc: TBenchmarkProc);
const
  FileName = 'out.txt';
var
  Time: Cardinal;
begin
  Inc(ProcNumber);
  ConsoleWrite('%d) "%s"... ', [ProcNumber, Description], 0);

  Time := GetTickCount;
    Proc(FileName);
  Time := GetTickCount - Time;

  ConsoleWrite('%dms', [Time]);
  CompareFiles(FileName, CORRECT_FILE_NAME);
end;


begin
  try
    // Information and data generation
    ConsoleWrite('The benchmark shows how quickly you can write files');
    ConsoleWrite('Destination file must be same as "correct_file.txt" (about 100Mb)');
    ConsoleWrite('Test data generating... ', 0);
    INITIALIZE_STRINGS;
    if (not FileExists(CORRECT_FILE_NAME)) then BufferedTextFileWriter(CORRECT_FILE_NAME);
    ConsoleWrite('done.', 3);

    // Run benchmark procs
    ConsoleWrite('Let''s test write methods (it may take an hour, but try to wait):');
    RunBenchmarkProc('TextFile+Buffer', BufferedTextFileWriter);
    RunBenchmarkProc('Simple CachedFileWriter', SimpleCachedFileWriter);
    RunBenchmarkProc('Fast CachedFileWriter', FastCachedFileWriter);
    RunBenchmarkProc('Standard TFileStream writer', FileStreamWriter);
    RunBenchmarkProc('Standard TMemoryStream+SaveToFile', MemoryStreamWriter);

  except
    on EAbort do ;

    on E: Exception do
    ConsoleWrite('%s: %s', [E.ClassName, E.Message]);
  end;

  if (ParamStr(1) <> '-nowait') then
  begin
    Writeln;
    ConsoleWrite('Press Enter to quit', 0);
    Readln;
  end;
end.
