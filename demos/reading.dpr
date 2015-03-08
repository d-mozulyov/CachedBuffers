program reading;

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
  CORRECT_SUM = Int64($2904E86C0);

{$if CompilerVersion < 19}
type
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ifend}


procedure GenerateTestFile;
const
  STRINGS_COUNT = 1000;
  ITERATIONS_COUNT = 22000;
var
  iteration, i: Integer;
  F: TextFile;
  Buffer: array[Word] of Byte;
begin
  AssignFile(F, CORRECT_FILE_NAME);
  ReWrite(F);
  SetTextBuf(F, Buffer);
  try
    for iteration := 1 to ITERATIONS_COUNT do
    for i := 1 to STRINGS_COUNT do
      Writeln(F, i);

  finally
    CloseFile(F);
  end;
end;

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

// copied from System.ValLong and corrected to ShortString with DefValue 0
function ShortStrToInt(const S: ShortString): Integer;
var
  I, Len: Integer;
  Negative, Hex: Boolean;
begin
  Result := 0;
  Negative := False;
  Hex := False;
  I := 1;
  Len := Length(S);

  while (I <= Len) and (S[I] = ' ') do Inc(I);
  if (I > Len) then Exit;  

  case S[I] of
    '$',
    'x',
    'X': begin
           Hex := True;
           Inc(I);
         end;
    '0': begin
          Hex := (Len > I) and (S[I+1] in ['x', 'X']);
    if Hex then Inc(I,2);
         end;
    '-': begin
          Negative := True;
          Inc(I);
         end;
    '+': Inc(I);
  end;
  if Hex then
    while I <= Len do
    begin
      if Result > (High(Result) div 16) then
      begin
        Result := 0;
        Exit;
      end;
      case s[I] of
        '0'..'9': Result := Result * 16 + Ord(S[I]) - Ord('0');
        'a'..'f': Result := Result * 16 + Ord(S[I]) - Ord('a') + 10;
        'A'..'F': Result := Result * 16 + Ord(S[I]) - Ord('A') + 10;
      else
        Result := 0;
        Exit;
      end;
    end
  else
    while (I <= Len) do
    begin
      if Result > (High(Result) div 10) then
      begin
        Result := 0;
        Exit;
      end;
      Result := Result * 10 + Ord(S[I]) - Ord('0');
      Inc(I);
    end;
  if Negative then
    Result := -Result;
end;


type
  TBenchmarkProc = function(const FileName: string): Int64;

var
  ProcNumber: Cardinal = 0;

// standard way to read and process strings:
// with TStringList
function StringListReader(const FileName: string): Int64;
var
  i: Integer;
  List: TStringList;
begin
  Result := 0;
  List := TStringList.Create;
  try
    List.LoadFromFile(FileName);

    for i := 0 to List.Count - 1 do
    Result := Result + StrToInt(List[i]);
  finally
    List.Free;
  end;
end;


// very fast reading method
// but it need too much memory (same as file size)
function FastBufferingReader(const FileName: string): Int64;
var
  Mem: Pointer;
  Size, Len: Integer;
  Current: PAnsiChar;
  S: PByte; // PShortString
  F: TFileStream;
begin
  Result := 0;

  Mem := nil;
  try
    // read file to memory
    F := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      Size := F.Size;
      GetMem(Mem, Size + 1);
      Current := Mem;
      Inc(Current);
      F.Read(Current^, Size);
    finally
      F.Free;
    end;

    // parse memory
    while (True) do
    begin
      // skip CRLF
      while (Size > 0) and (Current^ in [#13, #10]) do
      begin
        Inc(Current);
        Dec(Size);
      end;

      // look length of string
      S := Pointer(Current);
      Dec(S);
      while (Size > 0) and (not (Current^ in [#13, #10])) do
      begin
        Inc(Current);
        Dec(Size);
      end;
      Len := NativeInt(Current) - NativeInt(S) - 1;
      if (Len <= 0) or (Len > 255) then Break;

      // string to int
      S^ := Len;
      Result := Result + ShortStrToInt(PShortString(S)^);
    end;
  finally
    if (Mem <> nil) then FreeMem(Mem);
  end;
end;


// fast file parsing by using TCachedFileReader
function CachedFileReader(const FileName: string): Int64;
label
  flush;
var
  Reader: TCachedFileReader; 
  Size, Len: Integer;
  Current: PAnsiChar;
begin
  Result := 0;
  Reader.Initialize(FileName);
  try
    while (True) do
    begin
      Size := Reader.Margin;
      Current := Reader.Current;

      while (True) do
      begin
        // skip CRLF
        while (Size > 0) and (Current^ in [#13, #10]) do
        begin
          Inc(Current);
          Dec(Size);
        end;
        if (Size = 0) then goto flush;

        // look length of string
        Len := 1;
        while (Len < Size) and (not (Current[Len] in [#13, #10])) do Inc(Len);
        if (Len = Size) and (not Reader.Finishing) then goto flush;
        if (Len > 255) then exit;

        // get string + to int
        Dec(Current);
        PByte(Current)^ := Len;
        Result := Result + ShortStrToInt(PShortString(Current)^);

        // increment
        Inc(Current, Len + 1);
        Dec(Size, Len);
      end;

    flush:
      Reader.Margin := Size;
      Reader.Current := Current;
      if (Reader.Finishing) then Break;
      Reader.Flush();
    end;
  finally
    Reader.Finalize();
  end;
end;


// note the time and check proc result
procedure RunBenchmarkProc(const Description: string; const Proc: TBenchmarkProc);
var
  Time: Cardinal;
  Ret: Int64;
begin
  // reset filesystem cache to have same test conditions
  // (thanks for Sapersky)
  FileClose(CreateFile(PChar(CORRECT_FILE_NAME), GENERIC_READ, FILE_SHARE_READ, nil ,OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0));

  // run proc and get time
  Inc(ProcNumber);
  ConsoleWrite('%d) "%s"... ', [ProcNumber, Description], 0);
  Time := GetTickCount;
    Ret := Proc(CORRECT_FILE_NAME);
  Time := GetTickCount - Time;

  // information
  ConsoleWrite('%dms', [Time]);
  if (Ret <> CORRECT_SUM) then ConsoleWrite('FAIL!!! Sum = 0x%s', [IntToHex(Ret, 0)]);
end;

begin
  try
    // Information
    ConsoleWrite('The benchmark shows how quickly you can read/parse files');
    ConsoleWrite('Testing file is "correct_file.txt" (about 100Mb)');
    ConsoleWrite('Total sum of numbers must be equal 0x%s', [IntToHex(CORRECT_SUM, 0)], 3);

    // Generate the missing file
    if (not FileExists(CORRECT_FILE_NAME)) then GenerateTestFile;

    // Run benchmark procs
    ConsoleWrite('Let''s test methods (it may take a few minutes):');
    RunBenchmarkProc('Fast buffering reader', FastBufferingReader);
    RunBenchmarkProc('Using CachedFileReader', CachedFileReader);
    RunBenchmarkProc('Standard TStringList reader', StringListReader);

  except
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
