program reading;

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

{$if CompilerVersion < 19}
type
  NativeInt = Integer;
{$ifend}


procedure GenerateTestFile(const FileName: string);
const
  STRINGS_COUNT = 1000;
  ITERATIONS_COUNT = 22000;
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
    for i := 1 to STRINGS_COUNT do
      Writeln(F, i);

  finally
    CloseFile(F);
  end;
end;

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

// copying from System.ValLong and corrected to ShortString with DefValue 0
function ShortStrToInt(const S: ShortString): integer;
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


const
  FILE_NAME = 'correct_file.txt';
  CORRECT_SUM = int64($2904E86C0);

var
  ProcNumber: dword = 0;

type
  TBenchmarkProc = function(): int64;

// standard way to read and process strings:
// with TStringList
function StringListReader(): int64;
var
  i: integer;
  List: TStringList;
begin
  Result := 0;
  List := TStringList.Create;
  try
    List.LoadFromFile(FILE_NAME);

    for i := 0 to List.Count-1 do
    Result := Result + StrToInt(List[i]);
  finally
    List.Free;
  end;
end;


// very fast reading method
// but it need too much memory (same as file size)
function FastBufferingReader(): int64;
var
  Mem: pointer;
  Size, Len: integer;
  Current: pansichar;
  S: pbyte; // PShortString
  F: TFileStream;
begin
  Result := 0;

  Mem := nil;
  try
    // read file to memory
    F := TFileStream.Create(FILE_NAME, fmOpenRead or fmShareDenyNone);
    try
      Size := F.Size;
      GetMem(Mem, Size+1);
      Current := Mem;
      inc(Current);
      F.Read(Current^, Size);
    finally
      F.Free;
    end;

    // parse memory
    while (true) do
    begin
      // skip CRLF
      while (Size > 0) and (Current^ in [#13, #10]) do
      begin
        inc(Current);
        dec(Size);
      end;

      // look length of string
      S := pointer(Current);
      dec(S);
      while (Size > 0) and (not (Current^ in [#13, #10])) do
      begin
        inc(Current);
        dec(Size);
      end;
      Len := NativeInt(Current)-NativeInt(S)-1;
      if (Len <= 0) or (Len > 255) then break;

      // string to int
      S^ := Len;
      Result := Result + ShortStrToInt(PShortString(S)^);
    end;
  finally
    if (Mem <> nil) then FreeMem(Mem);
  end;
end;


// fast file parsing by using TCachedFileReader
function CachedFileReader(): int64;
label
  flush;
var
  Reader: TCachedFileReader; 
  Size, Len: integer;
  Current: pansichar;
  S: pbyte;
begin
  Result := 0;
  Reader.Initialize(FILE_NAME);
  try
    while (true) do
    begin
      Size := Reader.Margin;
      Current := Reader.Current;

      while (true) do
      begin
        // skip CRLF
        while (Size > 0) and (Current^ in [#13, #10]) do
        begin
          inc(Current);
          dec(Size);
        end;
        if (Size = 0) then goto flush;

        // look length of string
        Len := 1;
        while (Len < Size) and (not (Current[Len] in [#13, #10])) do inc(Len);
        if (Len = Size) and (not Reader.Finishing) then goto flush;
        if (Len > 255) then exit;

        // get string + to int
        S := pointer(Current);
        dec(S);
        S^ := Len;
        Result := Result + ShortStrToInt(PShortString(S)^);

        // increment
        inc(NativeInt(Current), Len);
        dec(Size, Len);
      end;

    flush:
      Reader.Margin := Size;
      Reader.Current := Current;
      if (Reader.Finishing) then break;
      Reader.Flush();
    end;
  finally
    Reader.Finalize();
  end;
end;


// note the time and check proc result
procedure RunBenchmarkProc(Description: string; Proc: TBenchmarkProc);
var
  Time: dword;
  Ret: int64;
begin
  // reset filesystem cache to have same test conditions
  // (thanks for Sapersky)
  FileClose(CreateFile(PChar(FILE_NAME), GENERIC_READ, FILE_SHARE_READ, nil ,OPEN_EXISTING, FILE_FLAG_NO_BUFFERING, 0));

  // run proc and get time
  inc(ProcNumber);
  ConsoleWrite('%d) "%s"... ', [ProcNumber, Description], 0);
  Time := GetTickCount;
    Ret := Proc();
  Time := GetTickCount-Time;

  // information
  ConsoleWrite('%dms', [Time]);
  if (Ret <> CORRECT_SUM) then ConsoleWrite('FAIL!!! Sum = 0x%s', [IntToHex(Ret,0)]);
end;

begin

try
  ConsoleWrite('The benchmark shows how quickly you can read/parse files', []);
  ConsoleWrite('Testing file is "correct_file.txt" (about 100Mb)', []);
  ConsoleWrite('Total sum of numbers must be equal 0x%s', [IntToHex(CORRECT_SUM,0)], 3);

  // Generate the missing file
  if (not FileExists(FILE_NAME)) then
    GenerateTestFile(FILE_NAME);

  // Run benchmark procs
  ConsoleWrite('Let''s test methods (it may take a few minutes):', []);
  RunBenchmarkProc('Standard TStringList reader', StringListReader);
  RunBenchmarkProc('Fast buffering reader', FastBufferingReader);
  RunBenchmarkProc('Using CachedFileReader', CachedFileReader);


except
  on E: Exception do
  ConsoleWrite('%s: %s', [E.ClassName, E.Message]);
end;
  if (ParamStr(1) <> '-nowait') then
  begin
    Writeln;
    ConsoleWrite('Press Enter to quit', [], 0);
    Readln;
  end;
end.
