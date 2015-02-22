unit CachedBuffers;


{ ******************************************************************** }
{ CachedBuffers is a simple and powerful object library to have cached }
{ data reading and writing with the best performance.                  }
{                                                                      }
{ Copyright: Dmitry Mozulyov (aka Devil)                               }
{ email: softforyou@inbox.ru                                           }
{ icq: 250481638                                                       }
{ ******************************************************************** }


 
// compiler options
{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$MINENUMSIZE 1}
{$ifndef VER140}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$endif}
{$O+}{$R-}{$I-}{$Q-}

interface
  uses {$ifdef MSWINDOWS}Windows,{$endif}
       Types, SysUtils;


type
  ECachedBuffer = class(Exception);
  TCachedBufferMode = (cbReader, cbWriter);
  PCustomCachedBuffer = ^TCustomCachedBuffer;
  TCachedBufferEvent = procedure(const Sender: PCustomCachedBuffer) of object;
  TCachedBufferProc = procedure(const CachedBuffer: PCustomCachedBuffer);
  TCachedBufferCallback = function(Sender: PCustomCachedBuffer; Data: pointer; Size: integer): integer;

  // allocated memory information
  TCachedBufferMemory = record
    Handle: pointer;
    PreviousSize: integer;   // 0..inf = Handle..Buffer
      Buffer: pointer;
      BufferSize: integer;
      FilledSize: integer;
    AdditionalSize: integer; // bonus memory after "Buffer+BufferSize", 4kb minimum
  end;

  // internal interface to use double threading, (rarely used)
  TCachedBufferThread = class
  private
    FAutoDestroy: boolean;
    FCachedBuffer: PCustomCachedBuffer;
    FMemory: TCachedBufferMemory;
    FRunningSize: integer;
  protected
    procedure RunWrite(Data: pointer; Size: integer); virtual;
    procedure RunRead(Data: pointer; Size: integer); virtual;
    function WaitWrite: integer; virtual;
    function WaitRead: integer; virtual;
    property RunningSize: integer read FRunningSize write FRunningSize;
  public
    constructor Create(const ACachedBuffer: PCustomCachedBuffer; const AReader: boolean; const ABufferSize: integer);
    destructor Destroy; override;

    procedure Run;
    function Wait: integer;
    function FullRunWait(Data: pointer; Size: integer): integer;

    property CachedBuffer: PCustomCachedBuffer read FCachedBuffer;
    property Memory: TCachedBufferMemory read FMemory;
    property AutoDestroy: boolean read FAutoDestroy write FAutoDestroy;
  end;


  // base object for cached reading/writing data
  TCustomCachedBuffer = object
  private
    FMemory: TCachedBufferMemory;
    FThread: TCachedBufferThread;
    FMode: TCachedBufferMode;
    FFinishing: boolean;

    FCallback: TCachedBufferCallback;
    FFinalizeProc: TCachedBufferProc;
    FFlushCount: dword;
    FOnProgress: TCachedBufferEvent;

    function GetPosition: int64;
    procedure FlushWriter();
    procedure FlushReader();
  protected
    procedure Initialize(AMode: TCachedBufferMode; ABufferSize: integer; ACallback: TCachedBufferCallback; AThread: TCachedBufferThread=nil);

    property Callback: TCachedBufferCallback read FCallback write FCallback;
    property FinalizeProc: TCachedBufferProc read FFinalizeProc write FFinalizeProc;
    property Thread: TCachedBufferThread read FThread write FThread;
  public
    procedure Finalize(); // destructor like Free/Destroy
  public
    // you can use highlevel Write(TCachedBufferWriter) and Read(TCachedBufferReader) methods,
    // but to have the really maximum performance, you should use Current, Margin and Flush()

    // current pointer in cached buffer
    Current: pointer;
    // available memory size in cached buffer
    Margin: integer;

    // call Flush() when the buffer ends (small or zero Margin) to update buffer, Current and Margin
    // - don't call Flush if Finishing flag is on (which is set when
    //   callback reading/writing size is not equal fixed buffer size)
    // - you can fill some additional memory in TCachedBufferWriter (Margin < 0),
    //   the memory will be automatically moved to Buffer area after Flush time,
    //   but be attention, AdditionalSize may be 4kb only
    // - you can leave some margin memory in TCachedBufferReader before Flush calling,
    //   the memory will be available, because it's automatically moved in Previous area.
    //   negative Margin (additional memory) is ignored in TCachedBufferReader
    procedure Flush();

    // useful properties
    property Mode: TCachedBufferMode read FMode;
    property Finishing: boolean read FFinishing;
    property FlushCount: dword read FFlushCount;
    property Position: int64 read GetPosition;
    property OnProgress: TCachedBufferEvent read FOnProgress write FOnProgress;
    property Memory: TCachedBufferMemory read FMemory;
    property BufferSize: integer read FMemory.BufferSize;
  end;


  PCachedBufferWriter = ^TCachedBufferWriter;
  TCachedBufferWriterCallback = function(const Sender: PCachedBufferWriter; const Data: pointer; const Size: integer): integer;
  
  // simple object for cached data writing
  TCachedBufferWriter = object(TCustomCachedBuffer)
  private
    procedure BigWrite(Data: pointer; Size: integer);
  public
    procedure Initialize(const ABufferSize: integer; const AWriteCallback: TCachedBufferWriterCallback; AThread: TCachedBufferThread=nil);

    // smart data writing
    procedure Write(const Buffer; const Count: integer);
  end;


  // file oriented cached buffer writer,
  // buffer size is 256kb
  TCachedFileWriter = object(TCachedBufferWriter)
  private
    FFileName: string;
    FHandle: integer;
  protected
    procedure InternalFinalize();
    function InternalWriter(const Data: pointer; const Size: integer): integer;
  public
    procedure Initialize(const AFileName: string);

    property FileName: string read FFileName;
  end;


  PCachedBufferReader = ^TCachedBufferReader;
  TCachedBufferReaderCallback = function(const Sender: PCachedBufferReader; const Data: pointer; const Size: integer): integer;

  // simple object for cached data reading
  TCachedBufferReader = object(TCustomCachedBuffer)
  private
    procedure BigRead(Data: pointer; Size: integer);  
  public
    procedure Initialize(const ABufferSize: integer; const AReaderCallback: TCachedBufferReaderCallback; AThread: TCachedBufferThread=nil);

    // smart data reading
    procedure Read(out Buffer; const Count: integer);
  end;


  // file oriented cached buffer reader,
  // buffer size is 256kb
  TCachedFileReader = object(TCachedBufferReader)
  private
    FFileName: string;
    FHandle: integer;
    FSize: int64;
  protected
    procedure InternalFinalize();
    function InternalReader(const Data: pointer; const ASize: integer): integer;
  public
    procedure Initialize(const AFileName: string);

    property FileName: string read FFileName;
    property Size: int64 read FSize;
  end;


implementation


{$if CompilerVersion < 19}
type
  NativeInt = Integer;
{$ifend}

{$if (CompilerVersion < 23) and (not Defined(FPC))}
  {$define CPUX86}
{$ifend}

{$if (not Defined(CPUX86)) and (not Defined(CPUX64))}
  {$DEFINE PUREPASCAL}
{$ifend}


const
  MEMORY_PAGE_SIZE = 4*1024; // 4kb
  FILE_BUFFER_SIZE = 256*1024; // 256kb

// allocate needed memory, align and fill all fields
function CachedBufferMemoryAlloc(APreviousSize, ABufferSize: integer): TCachedBufferMemory;
var
  Offset: NativeInt;

  procedure RaiseIncorrectSize(const Id: PAnsiChar; const Size: integer);
  begin
    raise ECachedBuffer.CreateFmt('Incorrect %sSize value: %d. It must be divisible by 4kb', [Id, Size]);
  end;
begin
  if (APreviousSize < 0) or
     ((APreviousSize>0)and(APreviousSize and (MEMORY_PAGE_SIZE-1) <> 0)) then RaiseIncorrectSize('Previous', APreviousSize);
  if (ABufferSize <= 0) or (ABufferSize and (MEMORY_PAGE_SIZE-1) <> 0) then RaiseIncorrectSize('Buffer', ABufferSize);

  Result.PreviousSize := APreviousSize;
  Result.BufferSize := ABufferSize;
  Result.AdditionalSize := MEMORY_PAGE_SIZE;

  with Result do
  GetMem(Handle, PreviousSize+BufferSize+AdditionalSize+{aligning}MEMORY_PAGE_SIZE);

  Offset := NativeInt(Result.Handle) and (MEMORY_PAGE_SIZE-1);
  inc(Result.PreviousSize, MEMORY_PAGE_SIZE-Offset);
  inc(Result.AdditionalSize, Offset);
  Result.Buffer := pointer(NativeInt(Result.Handle)+Result.PreviousSize);
end;

// free allocated memory
procedure CachedBufferMemoryFree(var Memory: TCachedBufferMemory);
begin
  if (Memory.Handle <> nil) then
  begin
    FreeMem(Memory.Handle);
    Memory.Handle := nil;
  end;
end;

// swap records
procedure CachedBufferMemorySwap(var M1, M2: TCachedBufferMemory);
var
  Buf: TCachedBufferMemory;
begin
  Buf := M1;
  M1 := M2;
  M2 := Buf;
end;

// internal additional memory copying
procedure CachedBufferMemoryCopyWriterOffset(var Dest, Src: TCachedBufferMemory; const Offset: integer);
begin
  Move(Pointer(NativeInt(Src.Buffer)+Src.BufferSize)^, Dest.Buffer^, Offset);
end;

// internal margin memory copying to previous area 
procedure CachedBufferMemoryCopyReaderOffset(var Dest, Src: TCachedBufferMemory; const Offset: integer);
var
  Buf: TCachedBufferMemory;

  procedure DoCopy(var D, S: TCachedBufferMemory);
  begin
    Move(Pointer(NativeInt(S.Buffer)+S.BufferSize-Offset)^,
         Pointer(NativeInt(D.Buffer)-Offset)^, Offset);
  end;
begin
  if (Offset <= Dest.PreviousSize) then
  begin
    DoCopy(Dest, Src);
  end else
  begin
    Buf := CachedBufferMemoryAlloc((Offset + (MEMORY_PAGE_SIZE-1)) and -MEMORY_PAGE_SIZE,
                                   Src.BufferSize);
    DoCopy(Buf, Src);
    CachedBufferMemoryFree(Dest);
    Dest := Buf;
  end;
end;

procedure RaiseCannotReadWrite(const Mode: TCachedBufferMode; const Size: integer);
const
  READ_WRITE: array[TCachedBufferMode] of AnsiString = ('read', 'write');
begin
  raise ECachedBuffer.CreateFmt('Cannot %s %d bytes', [READ_WRITE[Mode], Size]);
end;

procedure RaiseFinishing(const Mode: TCachedBufferMode; const Action: PAnsiChar);
const
  READER_WRITER: array[TCachedBufferMode] of AnsiString = ('reader', 'writer');
begin
  raise ECachedBuffer.CreateFmt('Cannot %s, because %s is already finishing', [Action, READER_WRITER[Mode]]);
end;
  

{ TCachedBufferThread }

constructor TCachedBufferThread.Create(const ACachedBuffer: PCustomCachedBuffer;
                                       const AReader: boolean; const ABufferSize: integer);
begin
  inherited Create;

  FCachedBuffer := ACachedBuffer;
  FMemory := CachedBufferMemoryAlloc(MEMORY_PAGE_SIZE*ord(AReader), ABufferSize);
end;

destructor TCachedBufferThread.Destroy;
begin
  if (RunningSize <> 0) then Wait();
  CachedBufferMemoryFree(FMemory);
  inherited;
end;

procedure TCachedBufferThread.RunWrite(Data: pointer; Size: integer);
begin
  RunningSize := Size;
  if (Size <= 0) then RaiseCannotReadWrite(CachedBuffer.FMode, Size);
end;

procedure TCachedBufferThread.RunRead(Data: pointer; Size: integer);
begin
  RunningSize := Size;
  if (Size <= 0) then RaiseCannotReadWrite(CachedBuffer.FMode, Size);
end;

function TCachedBufferThread.WaitWrite: integer;
begin
  Result := 0;
end;

function TCachedBufferThread.WaitRead: integer;
begin
  Result := 0;
end;

procedure TCachedBufferThread.Run;
begin
  if (RunningSize <> 0) then
  begin
    raise ECachedBuffer.Create('Thread is already running');
  end;

  if (CachedBuffer.FMode = cbWriter) then RunWrite(Memory.Buffer, Memory.FilledSize)
  else RunRead(Memory.Buffer, Memory.BufferSize);
end;

function TCachedBufferThread.Wait: integer;
begin
  if (RunningSize = 0) then
  begin
    raise ECachedBuffer.Create('Thread is already stoped');
  end;

  if (CachedBuffer.FMode = cbWriter) then
  begin
    Result := WaitWrite();
    if (Result <> RunningSize) then RaiseCannotReadWrite(cbWriter, RunningSize);
  end else
  begin
    Result := WaitRead();
    FMemory.FilledSize := Result;
  end;  
  RunningSize := 0;
end;

function TCachedBufferThread.FullRunWait(Data: pointer; Size: integer): integer;
begin
  if (RunningSize <> 0) then
  begin
    raise ECachedBuffer.Create('Thread is already running');
  end;

  if (CachedBuffer.FMode = cbWriter) then
  begin
    RunWrite(Data, Size);
    if (RunningSize = 0) then Result := Size else Result := WaitWrite();
  end else
  begin
    RunRead(Data, Size);
    if (RunningSize = 0) then Result := Size else Result := WaitRead();
  end;
  RunningSize := 0;
end;


{ TCustomCachedBuffer }


procedure TCustomCachedBuffer.Initialize(AMode: TCachedBufferMode; ABufferSize: integer; ACallback: TCachedBufferCallback; AThread: TCachedBufferThread);
begin
  FMemory := CachedBufferMemoryAlloc(MEMORY_PAGE_SIZE*ord(AMode=cbReader), ABufferSize);
  FThread := AThread;

  FMode := AMode;
  FFinishing := false;
  FCallback := ACallback;
  FFinalizeProc := nil;
  FFlushCount := 0;
  FOnProgress := nil;

  if (FMode = cbWriter) then
  begin
    Current := FMemory.Buffer;
    Margin := FMemory.BufferSize;
  end else
  // if (FMode = cbReader) then
  begin
    Current := pointer(NativeInt(FMemory.Buffer)+FMemory.BufferSize);
    Margin := 0;
    Flush();
  end;
end;

procedure TCustomCachedBuffer.Finalize;
begin
  if (FMode = cbWriter) and (not FFinishing) then Flush();
  if (FThread <> nil) and (FThread.AutoDestroy) then FThread.Free;
  CachedBufferMemoryFree(FMemory);
  if (assigned(FFinalizeProc)) then FFinalizeProc(@Self);  
end;

function TCustomCachedBuffer.GetPosition: int64;
begin
  // FFlushCount for writing and (FFlushCount - 1) for reading
  Result := (int64(FFlushCount+dword(ord(FMode))-1)*FMemory.BufferSize) + (integer(Current)-integer(FMemory.Buffer));
end;

procedure TCustomCachedBuffer.FlushWriter();
begin
  FMemory.FilledSize := FMemory.BufferSize;

  // if last
  if (Margin > 0) then
  begin
    dec(FMemory.FilledSize, Margin);
    FFinishing := true;
    Margin := 0;
    if (FMemory.FilledSize = 0) then exit;
  end;

  // different algorithms: standard and double FThreading
  if (FThread <> nil) then
  begin
    if (FFlushCount <> 1) then FThread.Wait();

    // swap
    CachedBufferMemorySwap(Self.FMemory, FThread.FMemory);

    // copy margin
    if (Margin < 0) then CachedBufferMemoryCopyWriterOffset(Self.FMemory, FThread.FMemory, -Margin);

    // thread running
    Thread.Run();
  end else
  // simple mode
  begin
    // run Callback
    if (Self.FMemory.FilledSize <> Callback(@Self, Self.FMemory.Buffer, Self.FMemory.FilledSize)) then
    RaiseCannotReadWrite(cbWriter, Self.FMemory.FilledSize);

    // copy margin (if needed)
    if (Margin < 0) then
    CachedBufferMemoryCopyWriterOffset(Self.FMemory, Self.FMemory, -Margin);
  end;

  // fill Current and Margin
  Current := Pointer(NativeInt(FMemory.Buffer) - Margin{<=0});
  Margin := FMemory.BufferSize + Margin{<=0};
end;

procedure TCustomCachedBuffer.FlushReader();
begin
  if (Margin < 0) then Margin := 0;

  // different algorithms: standard and double FThreading
  if (FThread <> nil) then
  begin
    if (FFlushCount = 1) then FThread.Run();
    if (FThread.FMemory.BufferSize <> FThread.Wait()) then FFinishing := true;

    CachedBufferMemorySwap(Self.FMemory, FThread.FMemory);
    if (Margin > 0) then CachedBufferMemoryCopyReaderOffset(Self.FMemory, FThread.FMemory, Margin);

    if (not FFinishing) then FThread.Run();
  end else
  // simple mode
  begin
    if (Margin > 0) then CachedBufferMemoryCopyReaderOffset(Self.FMemory, Self.FMemory, Margin);
    Self.FMemory.FilledSize := Callback(@Self, Self.FMemory.Buffer, Self.FMemory.BufferSize);
    if (Self.FMemory.BufferSize <> Self.FMemory.FilledSize) then FFinishing := true;
  end;

  // fill Current and Margin
  Current := Pointer(NativeInt(FMemory.Buffer) - Margin{>=0});
  Margin := FMemory.FilledSize + Margin{>=0};
end;

procedure TCustomCachedBuffer.Flush();
begin
  if (FFinishing) then RaiseFinishing(FMode, 'flush');
  inc(FFlushCount);

  if (FMode = cbWriter) then FlushWriter()
  else FlushReader();

  // event
  if (assigned(FOnProgress)) then FOnProgress(@Self);
end;




{ TCachedBufferWriter }

procedure TCachedBufferWriter.Initialize(const ABufferSize: integer; const AWriteCallback: TCachedBufferWriterCallback; AThread: TCachedBufferThread=nil);
begin
  inherited Initialize(cbWriter, ABufferSize, TCachedBufferCallback(AWriteCallback), AThread);
end;

// (Size > Margin)
procedure TCachedBufferWriter.BigWrite(Data: pointer; Size: integer);
var
  C, S: integer;
begin
  // write to buffer's end and Flush
  if (Current <> FMemory.Buffer) or (FFlushCount = 0) then
  begin
    if (Margin > 0) then
    begin
      Move(Data^, Current^, Margin);

      inc(NativeInt(Current), Margin);
      inc(NativeInt(Data), Margin);
      dec(Size, Margin);
      Margin := 0;
    end;

    Flush();
  end;

  // if written data is too large, we can call writer
  // without memory buffer using
  if (Size >= FMemory.BufferSize) then
  begin
    if (FThread <> nil) then FThread.Wait();
    C := Size div FMemory.BufferSize;
    S := C * FMemory.BufferSize;

    if (FThread <> nil) then
    begin
      FThread.FullRunWait(Data, S);
    end else
    begin
      if (S <> Callback(@Self, Data, S)) then RaiseCannotReadWrite(cbWriter, S);
    end;

    Inc(NativeInt(Data), S);
    Dec(Size, S);
    inc(FFlushCount, C);
    if (assigned(FOnProgress)) then FOnProgress(@Self);
  end;

  // last Data bytes
  if (Size <> 0) then
  begin
    Move(Data^, Current^, Size);
    inc(NativeInt(Current), Size);
    dec(Margin, Size);
  end;
end;

// smart data writing
const
  ID_WRITE_DATA: PAnsiChar = 'write data';

procedure TCachedBufferWriter.Write(const Buffer; const Count: integer);
{$ifdef PUREPASCAL}
begin
  if (FFinishing) then RaiseFinishing(cbWriter, ID_WRITE_DATA);
  if (Count <= 0) then
  begin
    if (Count < 0) then RaiseCannotReadWrite(cbWriter, Count);
    exit;
  end;

  if (Count <= Margin) then
  begin
    Move(Buffer, Current^, Count);
    inc(NativeInt(Current), Count);
    dec(Margin, Count);
  end else
  begin
    BigWrite(@Buffer, Count);
  end;
end;
{$elseif Defined(CPUX86)}
asm
  push esi
  push edi
  mov esi, edx
  test ecx, ecx
  mov edi, [EAX].TCachedBufferWriter.Current
  setle dl
  add dl, [EAX].TCachedBufferWriter.FFinishing
  jz @norm

  cmp byte ptr [EAX].TCachedBufferWriter.FFinishing, 0
  mov edx, ID_WRITE_DATA
  mov al, cbWriter
  jnz RaiseFinishing
  test ecx, ecx
  mov edx, ecx
  jl RaiseCannotReadWrite
  pop edi
  pop esi
  ret
@norm:
  sub [EAX].TCachedBufferWriter.Margin, ecx
  jge @small

  mov edx, esi
  add [EAX].TCachedBufferWriter.Margin, ecx
  pop edi
  pop esi
  jmp BigWrite
@small:
  add edi, ecx
  mov edx, ecx
  mov [EAX].TCachedBufferWriter.Current, edi
  and edx, 3
  sub edi, ecx
  shr ecx, 2
  jz @fill_bytes
  REP MOVSD
@fill_bytes:
jmp [offset @casebytes + edx*4]
@casebytes: DD @end,@1,@2,@3
@2:
  mov ax, [esi]
  mov [edi], ax
  pop edi
  pop esi
  ret
@3:
  mov ax, [esi]
  add esi, 2
  mov [edi], ax
  add edi, 2
@1:
  mov al, [esi]
  mov [edi], al
@end:
  pop edi
  pop esi
end;
{$elseif Defined(CPUX64)}
asm
  test r8d, r8d
  mov r9, [RCX].TCachedBufferWriter.Current
  setle al
  add al, [RCX].TCachedBufferWriter.FFinishing
  jz @norm

  cmp byte ptr [RCX].TCachedBufferWriter.FFinishing, 0
  mov rdx, ID_WRITE_DATA
  mov cl, cbWriter
  jnz RaiseFinishing
  test r8d, r8d
  mov edx, r8d
  jl RaiseCannotReadWrite
  ret

@norm:
  // Self = rcx
  // Data = rdx
  // Size = r8(d)
  // Current = r9
  sub [RCX].TCachedBufferWriter.Margin, r8d
  jge @small

  add [RCX].TCachedBufferWriter.Margin, r8d
  jmp BigWrite
@small:
  mov eax, r8d  // movzx rax, r8d
  and r8, 7
  mov r10, rax
  add rax, r9   // rax := Self.Current + int64(Size)
  mov r11, offset @casebytes
  mov [RCX].TCachedBufferWriter.Current, rax
  cmp r10, 8
  mov rax, 4
  jb @move_bytes

  // copy (r10 >> 3) qwads
  mov rcx, r10
  xchg rdx, rsi
  shr rcx, 3
  xchg r9, rdi
    REP MOVSQ
  xchg rdx, rsi
  xchg r9, rdi

@move_bytes:
jmp [r11 + r8*8]
@casebytes: DQ @end,@1,@2,@3,@4,@5,@6,@7
@7:
  mov ecx, [rdx]
  mov [r9], ecx
  add rdx, rax
  add r9, rax
  jmp @3
@6:
  mov ecx, [rdx]
  mov [r9], ecx
  add rdx, rax
  add r9, rax
  mov cx, [rdx]
  mov [r9], cx
  ret
@5:
  mov ecx, [rdx]
  mov [r9], ecx
  add rdx, rax
  add r9, rax
  mov cl, [rdx]
  mov [r9], cl
  ret
@4:
  mov ecx, [rdx]
  mov [r9], ecx
  ret
@2:
  mov cx, [rdx]
  mov [r9], cx
  ret
@3:
  dec rax
  mov cx, [rdx]
  dec rax
  mov [r9], cx
  add rdx, rax
  add r9, rax
@1:
  mov cl, [rdx]
  mov [r9], cl
@end:
end;
{$else}
begin
  {$message error 'Unknown compiling platform'}
end;
{$ifend}



{$ifdef MSWINDOWS}
type
  TWinFileBufferThread = class(TCachedBufferThread)
  private
    FFile: THandle;
    FNoBuffSectorSize: integer;
    F: packed record
    case Integer of
      0: (Struct: Windows._OVERLAPPED);
      1: (__: array[0..1] of NativeInt; Offset: int64; Event: THandle);
    end;

    function OverlappedWait: integer;
  protected
    procedure RunWrite(Data: pointer; Size: integer); override;
    procedure RunRead(Data: pointer; Size: integer); override;
    function WaitWrite: integer; override;
    function WaitRead: integer; override;

    property NoBuffSectorSize: integer read FNoBuffSectorSize;
  public
    constructor Create(ACachedBuffer: PCustomCachedBuffer; AFile: THandle; AReader: boolean; ANoBuffSectorSize: integer);
    destructor Destroy; override;
  end;

{ TWinFileBufferThread }

constructor TWinFileBufferThread.Create(ACachedBuffer: PCustomCachedBuffer; AFile: THandle; AReader: boolean; ANoBuffSectorSize: integer);
begin
  FFile := AFile;
  F.Event := Windows.CreateEvent(nil, True, False, nil); 
  FNoBuffSectorSize := ANoBuffSectorSize;
  inherited Create(ACachedBuffer, AReader, FILE_BUFFER_SIZE);
end;

destructor TWinFileBufferThread.Destroy;
begin
  inherited;
  CloseHandle(F.Event); 
end;

function TWinFileBufferThread.OverlappedWait: integer;
begin
  if (not Windows.GetOverlappedResult(FFile, F.Struct, dword(Result), TRUE)) then RaiseLastOSError();
  F.Offset := F.Offset + Result;
end;

procedure TWinFileBufferThread.RunWrite(Data: pointer; Size: integer);
var
  fake: dword;
  Count, i: integer;
begin
  inherited;

  if (NoBuffSectorSize = 0) or ((integer(Data) and (NoBuffSectorSize-1))=0) then
  begin
    Windows.WriteFile(FFile, Data^, Size, fake, @F.Struct);
  end else
  // difficult non-buffered case!
  begin
    Count := (Size+Memory.BufferSize-1) div Memory.BufferSize;

    for i := 0 to Count-2 do
    begin
      Move(Data^, Memory.Buffer^, Memory.BufferSize);
      Windows.WriteFile(FFile, Memory.Buffer^, Memory.BufferSize, fake, @F.Struct);
      if (OverlappedWait() <> Memory.BufferSize) then RaiseLastOSError();

      inc(NativeInt(Data), Memory.BufferSize);
      dec(Size, Memory.BufferSize);
    end;

    Move(Data^, Memory.Buffer^, Size);
    Windows.WriteFile(FFile, Memory.Buffer^, Size, fake, @F.Struct);
  end;
end;

procedure TWinFileBufferThread.RunRead(Data: pointer; Size: integer);
var
  fake: dword;
  Count, i: integer;

  procedure RaiseReading;
  begin
    RaiseCannotReadWrite(cbReader, Count*Memory.BufferSize + (Size mod Memory.BufferSize));
  end;
begin
  inherited;

  if (NoBuffSectorSize = 0) or ((integer(Data) and (NoBuffSectorSize-1))=0) then
  begin
    Windows.ReadFile(FFile, Data^, Size, fake, @F.Struct);
  end else
  // difficult non-buffered case!
  begin
    Count := (Size+Memory.BufferSize-1) div Memory.BufferSize;

    for i := 0 to Count-2 do
    begin
      Windows.ReadFile(FFile, Memory.Buffer^, Memory.BufferSize, fake, @F.Struct);
      if (OverlappedWait() <> Memory.BufferSize) then RaiseReading;
      Move(Memory.Buffer^, Data^, Memory.BufferSize);

      inc(NativeInt(Data), Memory.BufferSize);
      dec(Size, Memory.BufferSize);
    end;

    Windows.ReadFile(FFile, Memory.Buffer^, Size, fake, @F.Struct);
    if (OverlappedWait() <> Memory.BufferSize) then RaiseReading;
    Move(Memory.Buffer^, Data^, Size);

    RunningSize := 0;
  end;
end;

function TWinFileBufferThread.WaitRead: integer;
begin
  Result := OverlappedWait();
end;

function TWinFileBufferThread.WaitWrite: integer;
begin
  Result := OverlappedWait();
  if (Result <> RunningSize) then RaiseLastOSError();
end;
{ <-- TWinFileBufferThread }


function Windows7_higher: boolean;
begin
  Result := false;

  case Win32Platform of
    0..VER_PLATFORM_WIN32_WINDOWS: ;
    VER_PLATFORM_WIN32_NT:
      case Win32MajorVersion of
        0..5: ;
        6: Result := (Win32MinorVersion >= 1);
      else
        Result := true;
      end;
  else
    Result := true;
  end;
end;

function GetSectorSize(const FileName: string): integer;
var
  buf: dword;
begin
  GetDiskFreeSpace(PChar(Copy(ExpandFileName(FileName), 1, 3)), buf, dword(Result), buf, buf);
end;
{$endif}



{ TCachedFileWriter }

procedure TCachedFileWriter.Initialize(const AFileName: string);
begin
  {$ifdef MSWINDOWS}
  FHandle := Integer(CreateFile(PChar(AFileName), $0002{FILE_WRITE_DATA}, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0));
  {$else}
  FHandle := FileCreate(AFileName);
  {$endif}
  if (FHandle < 0) then raise ECachedBuffer.CreateFmt('Cannot create file:'#13'%s', [AFileName]);

  FFileName := AFileName;
  inherited Initialize(FILE_BUFFER_SIZE, TCachedBufferWriterCallback(@TCachedFileWriter.InternalWriter));
  Self.FinalizeProc := TCachedBufferProc(@TCachedFileWriter.InternalFinalize);  
end;

procedure TCachedFileWriter.InternalFinalize();
begin
  if (FHandle >= 0) then FileClose(FHandle);
  FFileName := '';
end;

function TCachedFileWriter.InternalWriter(const Data: pointer; const Size: integer): integer;
begin
  Result := FileWrite(FHandle, Data^, Size);
  if (Result <> Size) then RaiseLastOSError;
end;




{ TCachedBufferReader }

procedure TCachedBufferReader.Initialize(const ABufferSize: integer; const AReaderCallback: TCachedBufferReaderCallback; AThread: TCachedBufferThread=nil);
begin
  inherited Initialize(cbReader, ABufferSize, TCachedBufferCallback(AReaderCallback), AThread);
end;

// Margin < Size
procedure TCachedBufferReader.BigRead(Data: pointer; Size: integer);
var
  C, S: integer;

  procedure RaiseFinishedReading();
  begin
    RaiseFinishing(cbReader, 'read data');
  end;

begin
  if (FFinishing) then RaiseFinishedReading();

  // read Margin
  if (Margin <> 0) then
  begin
    Move(Current^, Data^, Margin);
    inc(NativeInt(Data), Margin);
    inc(NativeInt(Current), Margin);
    dec(Size, Margin);
    Margin := 0;
  end;

  // if read data is too large, we can call reader callback
  // without memory buffer using
  if (Size >= FMemory.BufferSize) then
  begin
    if (FThread <> nil) then
    begin
      if (FMemory.BufferSize <> FThread.Wait()) then RaiseFinishedReading();
      inc(FFlushCount);
      Move(FThread.FMemory.Buffer^, Data^, FMemory.BufferSize);
      Inc(NativeInt(Data), FMemory.BufferSize);
      Dec(Size, FMemory.BufferSize);
      if (assigned(FOnProgress)) then FOnProgress(@Self);
    end;

    if (Size >= FMemory.BufferSize) then
    begin
      C := Size div FMemory.BufferSize;
      S := C * FMemory.BufferSize;

      if (FThread <> nil) then
      begin
        if (S <> FThread.FullRunWait(Data, S)) then RaiseFinishedReading();
      end else
      begin
        if (S <> Callback(@Self, Data, S)) then RaiseFinishedReading();
      end;

      Inc(NativeInt(Data), S);
      Dec(Size, S);
      inc(FFlushCount, C);
      if (assigned(FOnProgress)) then FOnProgress(@Self);
    end;

    if (Thread <> nil) then Thread.Run();
  end;

  // last Data bytes
  if (Size <> 0) then
  begin
    Flush();
    if (Margin < Size) then RaiseFinishedReading();

    Move(Current^, Data^, Size);
    inc(NativeInt(Current), Size);
    dec(Margin, Size);
  end;
end;

// smart data reading
procedure TCachedBufferReader.Read(out Buffer; const Count: integer);
{$ifdef PUREPASCAL}
begin
  if (Count <= 0) then
  begin
    if (Count < 0) then RaiseCannotReadWrite(cbReader, Count);
    exit;
  end;

  if (Margin >= Count) then
  begin
    Move(Current^, Buffer, Count);
    inc(NativeInt(Current), Count);
    dec(Margin, Count);
  end else
  BigRead(@Buffer, Count);
end;
{$elseif Defined(CPUX86)}
asm
  test ecx, ecx
  jg @norm
  jl @error
  ret
@error:
  mov al, cbReader
  mov edx, ecx
  jmp RaiseCannotReadWrite
@norm:
  sub [EAX].TCachedBufferReader.Margin, ecx
  jge @small

  add [EAX].TCachedBufferReader.Margin, ecx
  jmp BigRead
@small:
  push edi
  push esi
  mov edi, edx
  mov edx, ecx
  mov esi, [EAX].TCachedBufferReader.Current
  and edx, 3
  shr ecx, 2
  jz @fill_bytes
  REP MOVSD
@fill_bytes:
jmp [offset @casebytes + edx*4]
@casebytes: DD @end,@1,@2,@3
@3:
  mov cx, [esi]
  mov [edi], cx
  mov cl, [esi+2]
  mov [edi+2], cl
  jmp @end
@2:
  mov cx, [esi]
  mov [edi], cx
  jmp @end
@1:
  mov cl, [esi]
  mov [edi], cl
@end:
  add edx, esi
  pop esi
  pop edi
  mov [EAX].TCachedBufferReader.Current, edx
end;
{$elseif Defined(CPUX64)}
asm
  test r8d, r8d
  jg @norm
  jl @error
  ret
@error:
  mov cl, cbReader
  mov edx, r8d
  jmp RaiseCannotReadWrite
@norm:
  sub [RCX].TCachedBufferReader.Margin, r8d
  jge @small

  add [RCX].TCachedBufferReader.Margin, r8d
  jmp BigRead
@small:
  mov r9, [RCX].TCachedBufferReader.Current
  // Self = rcx
  // Data (dest) = rdx
  // Size = r8(d)
  // Current (src) = r9

  mov eax, r8d  // movzx rax, r8d
  and r8, 7
  mov r10, rax
  add rax, r9   // rax := Self.Current + int64(Size)
  mov r11, offset @casebytes
  mov [RCX].TCachedBufferWriter.Current, rax
  cmp r10, 8
  mov rax, 4
  jb @move_bytes

  // copy (r10 >> 3) qwads
  mov rcx, r10
  xchg r9, rsi
  shr rcx, 3
  xchg rdx, rdi
    REP MOVSQ
  xchg r9, rsi
  xchg rdx, rdi

@move_bytes:
jmp [r11 + r8*8]
@casebytes: DQ @end,@1,@2,@3,@4,@5,@6,@7
@7:
  mov ecx, [r9]
  mov [rdx], ecx
  add r9, rax
  add rdx, rax
  jmp @3
@6:
  mov ecx, [r9]
  mov [rdx], ecx
  add r9, rax
  add rdx, rax
  mov cx, [r9]
  mov [rdx], cx
  ret
@5:
  mov ecx, [r9]
  mov [rdx], ecx
  add r9, rax
  add rdx, rax
  mov cl, [r9]
  mov [rdx], cl
  ret
@4:
  mov ecx, [r9]
  mov [rdx], ecx
  ret
@2:
  mov cx, [r9]
  mov [rdx], cx
  ret
@3:
  dec rax
  mov cx, [r9]
  dec rax
  mov [rdx], cx
  add r9, rax
  add rdx, rax
@1:
  mov cl, [r9]
  mov [rdx], cl
@end:
end;
{$else}
begin
  {$message error 'Unknown compiling platform'}
end;
{$ifend}



{ TCachedFileReader }

{$ifdef MSWINDOWS}
procedure TCachedFileReader.Initialize(const AFileName: string);
const
  ADoubleBuffering = true;
var
  Attributes: integer;
  NoBuffSectorSize: integer;
  AThread: TWinFileBufferThread;
begin
  Attributes := FILE_FLAG_SEQUENTIAL_SCAN;
  NoBuffSectorSize := 0;
  if (ADoubleBuffering) then
  begin
    Attributes := Attributes or FILE_FLAG_OVERLAPPED;

    if (not Windows7_higher) then
    begin
      NoBuffSectorSize := GetSectorSize(AFileName);
      if (NoBuffSectorSize <> 0) then Attributes := Attributes or FILE_FLAG_NO_BUFFERING;
    end;
  end;

  FHandle := Integer(CreateFile(PChar(AFileName), $0001{FILE_READ_DATA}, FILE_SHARE_READ, nil, OPEN_EXISTING, Attributes, 0));
  if (FHandle < 0) then raise ECachedBuffer.CreateFmt('Cannot read file:'#13'%s', [AFileName]);
  FFileName := AFileName;
  pdword(@FSize)^ := Windows.GetFileSize(FHandle, pdword(integer(@FSize)+sizeof(dword)));

//  AThread := nil;  
  if (ADoubleBuffering) then
  begin
    AThread := TWinFileBufferThread.Create(@Self, FHandle, true, NoBuffSectorSize);
    AThread.AutoDestroy := true;
  end;
  inherited Initialize(FILE_BUFFER_SIZE, TCachedBufferReaderCallback(@TCachedFileReader.InternalReader), AThread);
  Self.FinalizeProc := TCachedBufferProc(@TCachedFileReader.InternalFinalize);
end;
{$else}
procedure TCachedFileReader.Initialize(const AFileName: string);
begin
  FHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if (FHandle < 0) then raise ECachedBuffer.CreateFmt('Cannot read file:'#13'%s', [AFileName]);
  FFileName := AFileName;  
  FSize := FileSeek(FHandle, int64(0), FILE_END);
  FileSeek(FHandle, 0, FILE_BEGIN);
  inherited Initialize(FILE_BUFFER_SIZE, TCachedBufferReaderCallback(@TCachedFileReader.InternalReader));
  Self.FinalizeProc := TCachedBufferProc(@TCachedFileReader.InternalFinalize);
end;
{$endif}


procedure TCachedFileReader.InternalFinalize();
begin
  if (FHandle >= 0) then FileClose(FHandle);
  FFileName := '';
end;

function TCachedFileReader.InternalReader(const Data: pointer; const ASize: integer): integer;
begin
  Result := FileRead(FHandle, Data^, ASize);
  if (Result < 0) then RaiseLastOSError();
end;



end.


