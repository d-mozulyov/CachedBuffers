program writing;


{$APPTYPE CONSOLE}
// compiler options
{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$MINENUMSIZE 1}
{$ifndef VER140}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$endif}
{$O+}{$R-}{$I-}{$Q-}

uses
  Windows, SysUtils, CachedBuffers, Classes;

type
  EStopBenchmark = class(Exception);
{$if CompilerVersion < 19}
  NativeInt = Integer;
{$ifend}

const
  CORRECT_FILE_NAME = 'correct_file.txt';


procedure ConsoleWrite(const StrFmt: AnsiString; const Args: array of const; const CrlfCount: integer=1);
var
  i, Len: integer;
  Buf: AnsiString;
  S: PAnsiChar;
begin
  Buf := Format(StrFmt, Args);
  Len := Length(Buf);
  S := pointer(Buf);
  Windows.CharToOemBuffA(S, S, Len);

  for i := 0 to Len-1 do
  if (S[i] = #13) then S[i] := #10;

  if (CrlfCount <= 0) then
  begin
    Write(Buf);
  end else
  begin
    Writeln(Buf);
    for i := 2 to CrlfCount do Writeln;
  end;
end;

procedure CompareFiles(const FileName_1, FileName_2: string; const compact_info: boolean=true);
const
  BUFFER_SIZE = 256 * 1024;
var
  Name1, Name2: string;
  F1, F2: integer;
  S1, S2: int64;

  Reading: integer;
  SameFiles: boolean;

  Buffer1: array[0..BUFFER_SIZE-1] of byte;
  Buffer2: array[0..BUFFER_SIZE-1] of byte;

  function OpenReadingFile(const FileName: string; out Size: int64): integer;
  begin
    Result := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
    if (Result < 0) then raise Exception.CreateFmt('Cannot read file:'#13'%s', [FileName]);

    Size := FileSeek(Result, int64(0), FILE_END);
    FileSeek(Result, 0, FILE_BEGIN);
  end;

  procedure ReadBuffer(const F: integer; var Buffer; const Count: integer);
  begin
    if (Count <> FileRead(F, Buffer, Count)) then
    raise Exception.Create('Stream read error');
  end;

  procedure Error();
  begin
    raise EStopBenchmark.Create('');
  end;
begin
  Name1 := ExtractFileName(FileName_1);
  Name2 := ExtractFileName(FileName_2);
  if (compact_info) then ConsoleWrite('comparing with correct file... ', [], 0)
  else ConsoleWrite('Files comparing "%s" and "%s"... ', [Name1, Name2], 0);

  if (not FileExists(FileName_1)) then
  begin
    ConsoleWrite('"%s" not found', [Name1]);
    Error();
  end;
  if (not FileExists(FileName_2)) then
  begin
    ConsoleWrite('"%s" not found', [Name2]);
    Error();
  end;

  F1 := OpenReadingFile(FileName_1, S1);
  try
    F2 := OpenReadingFile(FileName_2, S2);
    try
      if (S1 <> S2) then
      begin
        ConsoleWrite('different sizes: %d and %d', [S1, S2]);
        Error();
      end;

      SameFiles := true;
      while (S1 <> 0) do
      begin
        if (S1 < BUFFER_SIZE) then Reading := S1
        else Reading := BUFFER_SIZE;

        ReadBuffer(F1, Buffer1, Reading);
        ReadBuffer(F2, Buffer2, Reading);
        if (not CompareMem(@Buffer1, @Buffer2, Reading)) then
        begin
          SameFiles := false;
          break;
        end;

        S1 := S1 - Reading;
      end;

      if (not SameFiles) then
      begin
        ConsoleWrite('NOT EQUAL !!!', []);
        Error();
      end
      else ConsoleWrite('binary equal.', []);
    finally
      FileClose(F2);
    end;
  finally
    FileClose(F1);
  end;
end;


const
  CRLF_VALUE = 13 or (10 shl 8);
  CRLF: word = CRLF_VALUE;
  STRINGS_COUNT = 1000;
  ITERATIONS_COUNT = 22000;// div 100;

var
  FileId: dword = 0;
  
  STRINGS: array[1..STRINGS_COUNT] of record
    Len: integer;
    S: AnsiString;
  end;

  procedure INITIALIZE_STRINGS;
  var
    i: integer;
  begin
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      S := IntToStr(i);
      Len := Length(S);
    end;
  end;


type
  TBenchmarkProc = procedure(const FileName: string);

// standard way to write data to Stream
procedure StreamWriter(Stream: TStream);
var
  iteration, i: integer;
begin
  for iteration := 1 to ITERATIONS_COUNT do
  for i := Low(STRINGS) to High(STRINGS) do
  with STRINGS[i] do
  begin
    Stream.Write(pointer(S)^, Len);
    Stream.Write(CRLF, sizeof(CRLF));
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

// write to TMemoryStream and SaveToFile()
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
  iteration, i: integer;
  F: TextFile;
  Buffer: array[word] of byte;
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
// Write() method is like TStream.Write(Buffer, Size)
procedure SimpleCachedFileWriter(const FileName: string);
var
  iteration, i: integer;
  Writer: TCachedFileWriter;
begin
  Writer.Initialize(FileName);
  try
    for iteration := 1 to ITERATIONS_COUNT do
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      Writer.Write(pointer(S)^, Len);
      Writer.Write(CRLF, sizeof(CRLF));
    end;

  finally
    Writer.Finalize();
  end;
end;

// difficult but the fastest way to write data with TCachedFileWriter
// you should use Current, Margin and Flush(). [optional additional memory]
procedure FastCachedFileWriter(const FileName: string);
var
  iteration, i: integer;
  Writer: TCachedFileWriter;
begin
  Writer.Initialize(FileName);
  try
    for iteration := 1 to ITERATIONS_COUNT do
    for i := Low(STRINGS) to High(STRINGS) do
    with STRINGS[i] do
    begin
      if (Len <= sizeof(int64)) then
      begin
        // fast Move()
        if (Len <= sizeof(integer)) then pinteger(Writer.Current)^ := pinteger(S)^
        else pint64(Writer.Current)^ := pint64(S)^;

        inc(NativeInt(Writer.Current), Len);
        dec(Writer.Margin, Len);
      end else
      begin
        // smart Move()
        Writer.Write(pointer(S), Len);
      end;

      // CRLF
      pword(Writer.Current)^ := CRLF_VALUE;
      inc(NativeInt(Writer.Current), sizeof(word));
      dec(Writer.Margin, sizeof(word));

      // Flush if needed
      if (Writer.Margin <= 0) then Writer.Flush();
    end;

  finally
    Writer.Finalize();
  end;
end;




// generate file name and note the time
procedure RunBenchmarkProc(Description: string; Proc: TBenchmarkProc);
const
  FileName = 'out.txt';
var
  Time: dword;
begin
  inc(FileId);
  ConsoleWrite('%d) "%s"... ', [FileId, Description], 0);

  Time := GetTickCount;
    Proc(FileName);
  Time := GetTickCount-Time;

  ConsoleWrite('%dms', [Time]);
  CompareFiles(FileName, CORRECT_FILE_NAME);
end;


begin
try
  ConsoleWrite('The benchmark shows how quickly you can write files', []);
  ConsoleWrite('Destination file must be same as "correct_file.txt" (about 100Mb)', []);
  ConsoleWrite('Test data generating... ', [], 0);
  INITIALIZE_STRINGS();
  if (not FileExists(CORRECT_FILE_NAME)) then BufferedTextFileWriter(CORRECT_FILE_NAME);
  ConsoleWrite('done.', [], 3);

  // Run benchmark procs
  ConsoleWrite('Let''s test methods (it may take an hour, but try to wait):', []);
  RunBenchmarkProc('TextFile+Buffer', BufferedTextFileWriter);
  RunBenchmarkProc('Simple CachedFileWriter', SimpleCachedFileWriter);
  RunBenchmarkProc('Fast CachedFileWriter', FastCachedFileWriter);
  RunBenchmarkProc('Standard TFileStream writer', FileStreamWriter);
  RunBenchmarkProc('Standard TMemoryStream+SaveToFile()', MemoryStreamWriter);

except
  on E: Exception do
  if (not (E is EStopBenchmark)) then
  ConsoleWrite('%s: %s', [E.ClassName, E.Message]);
end;
  if (ParamStr(1) <> '-nowait') then
  begin
    Writeln;
    ConsoleWrite('Press Enter to quit', [], 0);
    Readln;
  end;
end.
