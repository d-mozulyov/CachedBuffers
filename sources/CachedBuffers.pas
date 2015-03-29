unit CachedBuffers;

{******************************************************************************}
{ Copyright (c) 2013-2014 Dmitry Mozulyov (aka Devil)                          }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ email: softforyou@inbox.ru                                                   }
{ icq: 250481638                                                               }
{ skype: dimandevil                                                            }
{ site: http://sourceforge.net/projects/cachedbuffers/                         }
{******************************************************************************}


// compiler directives
{$ifdef FPC}
  {$mode Delphi}
  {$asmmode Intel}
{$endif}
{$if CompilerVersion >= 24}
  {$LEGACYIFEND ON}
{$ifend}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$if CompilerVersion >= 15}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ifend}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$if (CompilerVersion < 23) and (not Defined(FPC))}
  {$define CPUX86}
{$ifend}
{$if (Defined(FPC)) or (CompilerVersion >= 17)}
  {$define INLINESUPPORT}
{$ifend}
{$if Defined(CPUX86) or Defined(CPUX64)}
   {$define CPUINTEL}
{$ifend}
{$if SizeOf(Pointer) = 8}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$if CompilerVersion >= 21}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ifend}
{$if (not Defined(FPC)) and (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
  {$define INTERNALCODEPAGE}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}


interface
  uses Types,
       {$ifdef MSWINDOWS}Windows,{$endif}
       {$ifdef POSIX}Posix.String_, Posix.SysStat, Posix.Unistd,{$endif}
       {$ifdef KOL}
         KOL, err
       {$else}
         SysUtils
       {$endif};

const
  DEFAULT_CACHED_SIZE = 64*1024;

  soFromBeginning = 0;
  soFromCurrent = 1;
  soFromEnd = 2;

type
  {$if CompilerVersion < 19}
  NativeInt = Integer;
  PNativeInt = PInteger;
  NativeUInt = Cardinal;
  PNativeUInt = PCardinal;
  {$ifend}

  TBytes = array of Byte;

  {$if CompilerVersion < 23}
  TExtended80Rec = Extended;
  PExtended80Rec = ^TExtended80Rec;
  {$ifend}

  ECachedBuffer = class(Exception)
  {$ifdef KOL}
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
  {$endif}
  end;

{ TCachedBuffer abstract class }

  TCachedBufferMemory = record
    Handle: Pointer;
    PreviousSize: NativeUInt;   // 0..inf = Handle..Buffer
      Buffer: Pointer;
      BufferSize: NativeUInt;
    AdditionalSize: NativeUInt; // bonus memory after "Buffer+BufferSize", 4kb minimum
  end;
  PCachedBufferMemory = ^TCachedBufferMemory;
           {$message 'проработать всЄ! Position, Seek, Size, Flush, ошибки!!!'}
  TCachedBuffer = class(TObject)
  protected
    FMemory: TCachedBufferMemory;
    FOverflow: PByte;
    FCallback: TMethod;
    FFlushedPosition: Int64;
    FIsReader: Boolean;
    FIsFinishing: Boolean;
    FReserved1: Boolean;
    FReserved2: Boolean;

    procedure RaiseReadWrite(const Size: NativeInt);
    procedure RaiseFinishing(const Action: string);
    procedure RaiseOperation(const Operation: string);
    procedure RaiseVaraintType(const VType: Word);
    function GetPosition: Int64; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure SetPosition(const Value: Int64);
    procedure DoFlush; virtual; abstract;
  protected
    constructor Create(const IsReader: Boolean; const Callback: TMethod; const BufferSize: NativeUInt = 0);
    procedure CopyParameters(const Source: TCachedBuffer);
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    // you can use highlevel Write(TCachedWriter) and Read(TCachedReader) methods,
    // but to have the really maximum performance, you should use Current, Margin and Flush()

    // current pointer in cached buffer
    Current: PByte;
    // available memory size in cached buffer
  //  Margin: NativeInt;

    //
    procedure Seek(const Offset: NativeInt); overload;
    function Seek(const Offset: Int64; const Origin: Word): Int64; overload;

    // call Flush() when the buffer ends (small or zero Margin) to update buffer, Current and Margin
    // - don't call Flush if Finishing flag is on (which is set when
    //   callback reading/writing size is not equal fixed buffer size)
    // - you can fill some additional memory in TCachedWriter (Margin < 0),
    //   the memory will be automatically moved to Buffer area after Flush time,
    //   but be attention, AdditionalSize may be 4kb only
    // - you can leave some margin memory in TCachedReader before Flush calling,
    //   the memory will be available, because it's automatically moved in Previous area.
    //   negative Margin (additional memory) is ignored in TCachedReader
    function Flush: NativeInt;

    property Overflow: PByte read FOverflow;

    // useful properties
    property IsFinishing: Boolean read FIsFinishing;
    property Memory: TCachedBufferMemory read FMemory;
    property Position: Int64 read GetPosition write SetPosition;
  end;

{ TCachedReader class }

  TCachedReader = class;
  TCachedReaderCallback = function(Sender: TCachedReader; Buffer: PByte; BufferSize: NativeUInt): NativeUInt of object;

  TCachedReader = class(TCachedBuffer)
  protected
    procedure RaiseFinishedReading();
    procedure DifficultRead(Buffer: PByte; Count: NativeUInt);
    procedure DoFlush; override;{$if CompilerVersion >= 17}final;{$ifend}
  public
    constructor Create(const Callback: TCachedReaderCallback; const BufferSize: NativeUInt = 0);

    // TStream-like data reading
    procedure Read(var Buffer; const Count: NativeUInt);
    procedure ReadData(var Value: Boolean); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure ReadData(var Value: AnsiChar); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$endif}
    procedure ReadData(var Value: WideChar); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: ShortInt); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Byte); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: SmallInt); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Word); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Integer); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Cardinal); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Int64); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$if Defined(FPC) or (CompilerVersion >= 15)}
    procedure ReadData(var Value: UInt64); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$ifend}
    procedure ReadData(var Value: Single); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Double); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: TExtended80Rec); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: Currency); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: TPoint); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure ReadData(var Value: TRect); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure ReadData(var Value: ShortString); overload;
    procedure ReadData(var Value: AnsiString{$ifdef INTERNALCODEPAGE}; CodePage: Word = 0{$endif}); overload;
    procedure ReadData(var Value: WideString); overload;
    {$endif}
    {$ifdef UNICODE}
    procedure ReadData(var Value: UnicodeString); overload;
    {$endif}
    procedure ReadData(var Value: TBytes); overload;
    procedure ReadData(var Value: Variant); overload;
  end;


{ TCachedWriter class }

  TCachedWriter = class;
  TCachedWriterCallback = function(Sender: TCachedWriter; Buffer: PByte; BufferSize: NativeUInt): NativeUInt of object;

  TCachedWriter = class(TCachedBuffer)
  protected
    procedure DoFlush; override;{$if CompilerVersion >= 17}final;{$ifend}
    procedure DifficultWrite(Buffer: PByte; Count: NativeUInt);
  public
    constructor Create(const Callback: TCachedWriterCallback; const BufferSize: NativeUInt = 0);

    // TStream-like data writing
    procedure Write(const Buffer; const Count: NativeUInt);
    procedure WriteData(const Value: Boolean); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure WriteData(const Value: AnsiChar); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$endif}
    procedure WriteData(const Value: WideChar); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: ShortInt); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Byte); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: SmallInt); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Word); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Integer); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Cardinal); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Int64); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$if Defined(FPC) or (CompilerVersion >= 15)}
    procedure WriteData(const Value: UInt64); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$ifend}
    procedure WriteData(const Value: Single); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Double); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: TExtended80Rec); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Currency); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: TPoint); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: TRect); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure WriteData(const Value: ShortString); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: AnsiString); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: WideString); overload;
    {$endif}
    {$ifdef UNICODE}
    procedure WriteData(const Value: UnicodeString); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    {$endif}
    procedure WriteData(const Value: TBytes); overload; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure WriteData(const Value: Variant); overload;
  end;


{ TCachedReReader class }

  TCachedReReader = class;
  TCachedReReaderCallback = function(Sender: TCachedReReader; Buffer: PByte;
    BufferSize: NativeUInt; Source: TCachedReader): NativeUInt of object;

  TCachedReReader = class(TCachedReader)
  private
    function InternalCallback(Sender: TCachedReader; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  protected
    FSource: TCachedReader;
    FReCallback: TCachedReReaderCallback;

    function GetIsDirect: Boolean; virtual;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Callback: TCachedReReaderCallback; const Source: TCachedReader; const IsOwner: Boolean = False; const BufferSize: NativeUInt = 0); reintroduce;

    property IsOwner: Boolean read FReserved1 write FReserved1;
    property IsDirect: Boolean read FReserved2;
    property Source: TCachedReader read FSource;
  end;

{ TCachedReWriter class }

  TCachedReWriter = class;
  TCachedReWriterCallback = function(Sender: TCachedReWriter; Buffer: PByte;
    BufferSize: NativeUInt; Destination: TCachedWriter): NativeUInt of object;

  TCachedReWriter = class(TCachedWriter)
  private
    function InternalCallback(Sender: TCachedWriter; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  protected
    FDestination: TCachedWriter;
    FReCallback: TCachedReWriterCallback;

    function GetIsDirect: Boolean; virtual;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Callback: TCachedReWriterCallback; const Destination: TCachedWriter; const IsOwner: Boolean = False; const BufferSize: NativeUInt = 0); reintroduce;

    property IsOwner: Boolean read FReserved1 write FReserved1;
    property IsDirect: Boolean read FReserved2;
    property Destination: TCachedWriter read FDestination;
  end;


{ TCachedFileReader class }

  TCachedFileReader = class(TCachedReader)
  private
  protected
    FHandle: THandle;
    FLimitSize: Int64;

    function InternalCallback(Sender: TCachedReader; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const FileName: string; const Offset: Int64 = 0; const LimitSize: Int64 = 0); overload;
    constructor Create(const Handle: THandle; const LimitSize: Int64 = 0; const HandleOwner: Boolean = True); overload;

    property Handle: THandle read FHandle;
    property IsHandleOwner: Boolean read FReserved1 write FReserved1;
    property Limited: Boolean read FReserved2;
    property LimitSize: Int64 read FLimitSize;
  end;

{ TCachedFileWriter class }

  TCachedFileWriter = class(TCachedWriter)
  private
  protected
    FHandle: THandle;
    FLimitSize: Int64;

    function InternalCallback(Sender: TCachedWriter; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const FileName: string); overload;
    constructor Create(const Handle: THandle; const LimitSize: Int64 = 0; const HandleOwner: Boolean = True); overload;

    property Handle: THandle read FHandle;
    property IsHandleOwner: Boolean read FReserved1 write FReserved1;
    property Limited: Boolean read FReserved2;
  end;


{ TCachedMemoryReader class }

  TCachedMemoryReader = class(TCachedReader)
  protected
    FPtr: PByte;
    FPtrMargin: NativeUInt;

    function InternalCallback(Sender: TCachedReader; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  public
    constructor Create(const Ptr: Pointer; const Size: NativeUInt);
  end;

{ TCachedMemoryWriter class }

  TCachedMemoryWriter = class(TCachedWriter)
  protected
    FPtr: PByte;
    FPtrMargin: NativeUInt;

    function InternalCallback(Sender: TCachedWriter; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  public
    constructor Create(const Ptr: Pointer; const Size: NativeUInt);
  end;

{ TCachedResourceReader class }

  {$ifdef MSWINDOWS}
  TCachedResourceReader = class(TCachedMemoryReader)
  protected
    HGlobal: THandle;
    procedure InternalCreate(Instance: THandle; Name, ResType: PChar);
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
    constructor CreateFromID(Instance: THandle; ResID: Word; ResType: PChar);
  end;
  {$endif}





    {Custom?}

(*  TCachedBufferMode = (cbReader, cbWriter);
  PCustomCachedBuffer = ^TCustomCachedBuffer;
  TCachedBufferEvent = procedure(const Sender: PCustomCachedBuffer) of object;
  TCachedBufferProc = procedure(const CachedBuffer: PCustomCachedBuffer);
  TMethod = function(Sender: PCustomCachedBuffer; Data: pointer; Size: integer): integer;




  // base object for cached reading/writing data
  TCustomCachedBuffer = object
  private
    FMemory: TCachedBufferMemory;
    FThread: TCachedBufferThread;
    FMode: TCachedBufferMode;
    FFinishing: boolean;

    FCallback: TMethod;
    FFinalizeProc: TCachedBufferProc;
    FFlushCount: dword;
    FOnProgress: TCachedBufferEvent;

    function GetPosition: int64;
    procedure FlushWriter();
    procedure FlushReader();
  protected
    procedure Initialize(AMode: TCachedBufferMode; ABufferSize: integer; ACallback: TMethod; AThread: TCachedBufferThread=nil);

    property Callback: TMethod read FCallback write FCallback;
    property FinalizeProc: TCachedBufferProc read FFinalizeProc write FFinalizeProc;
    property Thread: TCachedBufferThread read FThread write FThread;
  public
    procedure Finalize(); // destructor like Free/Destroy
  public
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
    procedure Read(var Buffer; const Count: integer);
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
            *)




// fast non-collision Move() realization            
procedure NcMove(const Source; var Dest; const Size: NativeUInt); {$ifdef CPUARM}inline;{$endif}

implementation


{ ECachedBuffer }

{$ifdef KOL}
constructor ECachedBuffer.Create(const Msg: string);
begin
  inherited Create(e_Custom, Msg);
end;

constructor ECachedBuffer.CreateFmt(const Msg: string;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, Msg, Args);
end;
{$endif}


const
  MEMORY_PAGE_SIZE = 4 * 1024;

function CachedBufferMemory(const PreviousSize, BufferSize: NativeUInt): TCachedBufferMemory;
var
  Offset: NativeUInt;
begin
  // detect sizes
  Result.PreviousSize := (PreviousSize + MEMORY_PAGE_SIZE-1) and -MEMORY_PAGE_SIZE;
  Result.AdditionalSize := MEMORY_PAGE_SIZE;
  if (BufferSize = 0) then Result.BufferSize := DEFAULT_CACHED_SIZE;
  Result.BufferSize := (BufferSize + MEMORY_PAGE_SIZE-1) and -MEMORY_PAGE_SIZE;

  // allocate
  GetMem(Result.Handle, Result.PreviousSize + Result.BufferSize +
                        Result.AdditionalSize + MEMORY_PAGE_SIZE);

  // align
  Offset := NativeUInt(Result.Handle) and (MEMORY_PAGE_SIZE-1);
  Inc(Result.PreviousSize, MEMORY_PAGE_SIZE-Offset);
  Inc(Result.AdditionalSize, Offset);
  Result.Buffer := Pointer(NativeUInt(Result.Handle) + Result.PreviousSize);
end;


function GetFileSize(Handle: THandle): Int64;
var
  {$ifdef MSWINDOWS}
    P: TPoint;
  {$endif}
  {$ifdef POSIX}
    S: _stat;
  {$endif}
begin
  {$ifdef MSWINDOWS}
    P.X := Windows.GetFileSize(Handle, @P.Y);
    Result := PInt64(@P)^;
  {$endif}

  {$ifdef POSIX}
    fstat(Handle, S);
    Result := S.st_size;
  {$endif}
end;


(*
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



{ TCustomCachedBuffer }


procedure TCustomCachedBuffer.Initialize(AMode: TCachedBufferMode; ABufferSize: integer; ACallback: TMethod; AThread: TCachedBufferThread);
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
  inherited Initialize(cbWriter, ABufferSize, TMethod(AWriteCallback), AThread);
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
  // withvar memory buffer using
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
  inherited Initialize(cbReader, ABufferSize, TMethod(AReaderCallback), AThread);
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
  // withvar memory buffer using
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
procedure TCachedBufferReader.Read(var Buffer; const Count: integer);
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
       *)


{ TCachedBuffer }

constructor TCachedBuffer.Create(const IsReader: Boolean;
  const Callback: TMethod; const BufferSize: NativeUInt);
begin
  inherited Create;
  FIsReader := IsReader;
  FCallback := Callback;

  FMemory := CachedBufferMemory(Ord(IsReader){4kb}, BufferSize);

  if (FIsReader) then
  begin
    Current := Pointer(NativeUInt(FMemory.Buffer)+FMemory.BufferSize);
    FOverflow := Current;
    DoFlush();
  end else
  begin
    Current := FMemory.Buffer;
    FOverflow := Current;
    Inc(FOverflow, FMemory.BufferSize);
  end;
end;

procedure TCachedBuffer.CopyParameters(const Source: TCachedBuffer);
begin
  Self.FMemory := Source.FMemory;
  // Self.FCallback := Source.FCallback;
  Self.FFlushedPosition := Source.FFlushedPosition;
  Self.FIsReader := Source.FIsReader;
  Self.FIsFinishing := Source.FIsFinishing;
  // FReserved1: Boolean;
  // FReserved2: Boolean;
  Self.Current := Source.Current;
//  Self.Margin := Source.Margin;
end;

(*function TCachedBuffer.GetSize: Int64;
begin
  Result := {FPtrMargin +???} {Position}FFlushedPosition + (NativeInt(Current)-NativeInt(FMemory.Buffer));

  if (not FIsReader) then
    Inc(Result, Margin);
end;*)

destructor TCachedBuffer.Destroy;
begin
  if (not FIsReader) and (not FIsFinishing) then Flush;

  if (FMemory.Handle <> nil) then
  FreeMem(FMemory.Handle);

  inherited;
end;

procedure TCachedBuffer.RaiseReadWrite(const Size: NativeInt);
const
  READ_WRITE: array[Boolean] of string = ('write', 'read');
begin
  raise ECachedBuffer.CreateFmt('Can''t %s %d bytes', [READ_WRITE[FIsReader], Size]);
end;

procedure TCachedBuffer.RaiseFinishing(const Action: string);
const
  READER_WRITER: array[Boolean] of string = ('writer', 'reader');
begin
  raise ECachedBuffer.CreateFmt('Can''t %s, because %s is already finishing',
                                [Action, READER_WRITER[FIsReader]]);
end;

procedure TCachedBuffer.RaiseOperation(const Operation: string);
begin
  raise ECachedBuffer.CreateFmt('"%s" is invalid %s instance operation', [ClassName, Operation]);
end;

procedure TCachedBuffer.RaiseVaraintType(const VType: Word);
begin
  raise ECachedBuffer.CreateFmt('Unsupported Variant type 0x%x', [VType]);
end;

function TCachedBuffer.GetPosition: Int64;
begin
  Result := FFlushedPosition + (NativeInt(Current)-NativeInt(FMemory.Buffer));
end;

procedure TCachedBuffer.SetPosition(const Value: Int64);
begin
  // todo
end;

procedure TCachedBuffer.Seek(const Offset: NativeInt);
begin

end;

function TCachedBuffer.Seek(const Offset: Int64; const Origin: Word): Int64;
begin
  Result := 0;
  // todo
end;

function TCachedBuffer.Flush: NativeInt;
begin
  // exception?

  // по идее Flush надо сделать м€гкими
  // а чтение-завись твЄрдыми типа всегда ReadBuffer/WriteBuffer
  // но и c Seek кстати тоже что-то нужно сделать

  if (not FIsFinishing) then
  begin
    Inc(FFlushedPosition, NativeInt(Current)-NativeInt(FMemory.Buffer));
    DoFlush;
  end;

  Result := NativeInt(Self.Overflow) - NativeInt(Self.Current);

  if (Result < 0) then
  begin
    Current := Overflow;
    Result := 0;
  end;
end;


{$ifdef CPUX86}
procedure NcMoveInternal(const Source; var Dest; const Size: NativeUInt); forward;
{$endif}

procedure TCachedReader.Read(var Buffer; const Count: NativeUInt);
{$ifdef CPUARM}
var
  C: PByte;
begin
  C := Current;
  Inc(C, Count);

  if (C >= Self.Overflow) then
  begin
    DifficultRead(@Buffer, Count);
  end else
  begin
    Current := C;
    Dec(C, Count);
    NcMove(C^, Buffer, Count);
  end;
end;
{$else .CPUINTEL}
asm
  {$ifdef CPUX86}
    xchg eax, ebx
    push eax
    mov eax, ecx
    add eax, [EBX].TCachedReader.Current
    cmp eax, [EBX].TCachedReader.FOverflow
    ja @Difficult

    mov [EBX].TCachedReader.Current, eax
    sub eax, ecx
    jmp NcMoveInternal
  {$else .CPUX64}
    mov rax, rcx
    mov rcx, r8
    add rcx, [RAX].TCachedReader.Current
    cmp rcx, [RAX].TCachedReader.FOverflow
    ja @Difficult

    mov [RAX].TCachedReader.Current, rcx
    sub rcx, r8
    jmp NcMove
  {$endif}

@Difficult:
  {$ifdef CPUX86}
    xchg eax, ebx
    pop ebx
  {$else .CPUX64}
    xchg rax, rcx
  {$endif}
  jmp DifficultRead
end;
{$endif}

procedure TCachedWriter.Write(const Buffer; const Count: NativeUInt);
{$ifdef CPUARM}
var
  C: PByte;
begin
  C := Current;
  Inc(C, Count);

  if (C >= Self.Overflow) then
  begin
    DifficultWrite(@Buffer, Count);
  end else
  begin
    Current := C;
    Dec(C, Count);
    NcMove(Buffer, C^, Count);
  end;
end;
{$else .CPUINTEL}
asm
  {$ifdef CPUX86}
    xchg eax, ebx
    push eax
    mov eax, ecx
    add eax, [EBX].TCachedReader.Current
    cmp eax, [EBX].TCachedReader.FOverflow
    ja @Difficult

    mov [EBX].TCachedReader.Current, eax
    sub eax, ecx
    xchg eax, edx
    jmp NcMoveInternal
  {$else .CPUX64}
    mov rax, rcx
    mov rcx, r8
    add rcx, [RAX].TCachedReader.Current
    cmp rcx, [RAX].TCachedReader.FOverflow
    ja @Difficult

    mov [RAX].TCachedReader.Current, rcx
    sub rcx, r8
    xchg rcx, rdx
    jmp NcMove
  {$endif}

@Difficult:
  {$ifdef CPUX86}
    xchg eax, ebx
    pop ebx
  {$else .CPUX64}
    xchg rax, rcx
  {$endif}
  jmp DifficultWrite
end;
{$endif}

{$ifdef CPUARM}
// System memcpy recall
procedure NcMove(const Source; var Dest; const Size: NativeUInt);
begin
  memcpy(Dest, Source, Size);
end;
{$else .CPUINTEL}
// SSE-based non-collision Move() realization
procedure NcMove(const Source; var Dest; const Size: NativeUInt);
{$ifdef CPUX86}
asm
  push ebx
  jmp NcMoveInternal
end;
procedure NcMoveInternal(const Source; var Dest; const Size: NativeUInt);
{$endif}
asm
  // basic routine
  {$ifdef CPUX86}
    cmp ecx, 32
  {$else .CPUX64}
    cmp r8, 32
    // make Source = eax/rax, Dest = edx/rdx, Size = ecx/rcx
    mov rax, rcx
    xchg rcx, r8
    // r9 as pointer to @move_03_items
    lea r9, [@move_03_items]
  {$endif}

  // is big/large (32...inf)
  jae @move_big

  // is small (0..3)
  cmp ecx, 4
  jb @move_03

  // move middle(4..31) = move 16(0..16) + move dwords(0..12) + move small(0..3)
  cmp ecx, 16
  jb @move_015

  {$ifdef CPUX86}
    movups xmm0, [eax]
    movups [edx], xmm0
    jne @move_015_offset
    pop ebx
    ret
  @move_015_offset:
    sub ecx, 16
    add eax, 16
    add edx, 16
  @move_015:
    mov ebx, ecx
    and ecx, -4
    and ebx, 3
    add eax, ecx
    add edx, ecx
    jmp [ecx + @move_dwords]
    @move_dwords: DD @rw_0,@rw_4,@rw_8,@rw_12
    @rw_12:
      mov ecx, [eax-12]
      mov [edx-12], ecx
    @rw_8:
      mov ecx, [eax-8]
      mov [edx-8], ecx
    @rw_4:
      mov ecx, [eax-4]
      mov [edx-4], ecx
    @rw_0:
    xchg ecx, ebx
  {$else .CPUX64}
    movups xmm0, [rax]
    movups [rdx], xmm0
    jne @move_015_offset
    ret
  @move_015_offset:
    sub rcx, 16
    add rax, 16
    add rdx, 16
  @move_015:
    // make r9 = dest 0..3 pointer, rcx = dwords count
    mov r8, rcx
    shr rcx, 2
    and r8, 3
    lea r9, [r9 + r8*8]
    // case jump
    lea r8, [@move_dwords]
    jmp qword ptr [r8 + rcx*8]
    @move_dwords: DQ @rw_0,@rw_4,@rw_8,@rw_12
    @rw_8:
      mov rcx, [rax]
      mov [rdx], rcx
      add rax, 8
      add rdx, 8
    jmp qword ptr [r9]
    @rw_12:
      mov rcx, [rax]
      mov [rdx], rcx
      add rax, 8
      add rdx, 8
    @rw_4:
      mov ecx, [rax]
      mov [rdx], ecx
      add rax, 4
      add rdx, 4
    @rw_0:
    jmp qword ptr [r9]
  {$endif}

@move_03:
  {$ifdef CPUX86}
    jmp [offset @move_03_items + ecx*4]
    @move_03_items: DD @0,@1,@2,@3
    @2: mov cx, [eax]
        mov [edx], cx
        pop ebx
        ret
    @3: mov cx, [eax]
        mov [edx], cx
        add eax, 2
        add edx, 2
    @1: mov cl, [eax]
        mov [edx], cl
    @0: pop ebx
        ret
  {$else .CPUX64}
    jmp qword ptr [r9 + rcx*8]
    @move_03_items: DQ @0,@1,@2,@3
    @2: mov cx, [rax]
        mov [rdx], cx
        ret
    @3: mov cx, [rax]
        mov [rdx], cx
        add rax, 2
        add rdx, 2
    @1: mov cl, [rax]
        mov [rdx], cl
    @0: ret
  {$endif}

@move_big:
  {$ifdef CPUX86}
    cmp ecx, 16*4
  {$else .CPUX64}
    cmp rcx, 16*4
  {$endif}
  jae @move_large

  // big memory move by SSE (32..63) = (32..48) + (0..15)
  {$ifdef CPUX86}
     test ecx, 15
     jz @move_32_48

     mov ebx, ecx
     and ecx, 15
     movups xmm0, [eax]
     movups [edx], xmm0
     add eax, ecx
     add edx, ecx

     and ebx, -16
     xchg ecx, ebx
  {$else .CPUX64}
     mov r8, rcx
     test rcx, 15
     jz @move_32_48

     and r8, 15
     movups xmm0, [rax]
     movups [rdx], xmm0
     add rax, r8
     add rdx, r8

     and rcx, -16
  {$endif}

@move_32_48:
  {$ifdef CPUX86}
    add eax, ecx
    add edx, ecx
    cmp ecx, 48
    jb @rw_32
    @rw_48: movups xmm2, [eax - 2*16 - 16]
            movups [edx - 2*16 - 16], xmm2
    @rw_32: movups xmm1, [eax - 1*16 - 16]
            movups xmm0, [eax - 0*16 - 16]
            movups [edx - 1*16 - 16], xmm1
            movups [edx - 0*16 - 16], xmm0
    pop ebx
  {$else .CPUX64}
    add rax, rcx
    add rdx, rcx
    cmp rcx, 48
    jb @rw_32
    @rw_48: movups xmm2, [rax - 2*16 - 16]
            movups [rdx - 2*16 - 16], xmm2
    @rw_32: movups xmm1, [rax - 1*16 - 16]
            movups xmm0, [rax - 0*16 - 16]
            movups [rdx - 1*16 - 16], xmm1
            movups [rdx - 0*16 - 16], xmm0
  {$endif}

  ret
@move_large:
  // large memory move by SSE (64..inf)

  // destination alignment
  {$ifdef CPUX86}
    test edx, 15
    jz @move_16128_initialize

    mov ebx, edx
    movups xmm0, [eax]
    movups [ebx], xmm0

    add edx, 15
    and edx, -16
    sub ebx, edx
    sub eax, ebx
    add ecx, ebx
  {$else .CPUX64}
    test rdx, 15
    jz @move_16128_initialize

    mov r8, rdx
    movups xmm0, [rax]
    movups [r8], xmm0

    add rdx, 15
    and rdx, -16
    sub r8, rdx
    sub rax, r8
    add rcx, r8
  {$endif}

@move_16128_initialize:
  {$ifdef CPUX86}
    push ecx
    mov ebx, offset @aligned_reads
    shr ecx, 4
    test eax, 15
    jz @move_16128
    mov ebx, offset @unaligned_reads
  {$else .CPUX64}
    movaps [rsp-8-16], xmm6
    movaps [rsp-8-32], xmm7
    mov r8, rcx
    lea r9, [@aligned_reads]
    shr rcx, 4
    test rax, 15
    jz @move_16128
    lea r9, [@unaligned_reads]
  {$endif}

@move_16128:
  {$ifdef CPUX86}
    cmp ecx, 8
    jae @move_128

    lea ecx, [ecx + ecx]
    lea eax, [eax + ecx*8]
    lea edx, [edx + ecx*8]
    lea ebx, [ebx + 8*4]
    neg ecx
    lea ebx, [ebx + ecx*2]
    jmp ebx
  @move_128:
    lea eax, [eax + 128]
    lea edx, [edx + 128]
    lea ecx, [ecx - 8]
    jmp ebx
  {$else .CPUX64}
    cmp rcx, 8
    jae @move_128

    lea rcx, [rcx + rcx]
    lea rax, [rax + rcx*8]
    lea rdx, [rdx + rcx*8]
    lea r9, [r9 + 8*4]
    neg rcx
    lea r9, [r9 + rcx*2]
    jmp r9
  @move_128:
    lea rax, [rax + 128]
    lea rdx, [rdx + 128]
    lea rcx, [rcx - 8]
    jmp r9
  {$endif}

  // aligned sse read
  @aligned_reads:
  {$ifdef CPUX86}
    movaps xmm7, [eax - 7*16 - 16]
    movaps xmm6, [eax - 6*16 - 16]
    movaps xmm5, [eax - 5*16 - 16]
    movaps xmm4, [eax - 4*16 - 16]
    movaps xmm3, [eax - 3*16 - 16]
    movaps xmm2, [eax - 2*16 - 16]
    movaps xmm1, [eax - 1*16 - 16]
    movaps xmm0, [eax - 0*16 - 16]
  {$else .CPUX64}
    movaps xmm7, [rax - 7*16 - 16]
    movaps xmm6, [rax - 6*16 - 16]
    movaps xmm5, [rax - 5*16 - 16]
    movaps xmm4, [rax - 4*16 - 16]
    movaps xmm3, [rax - 3*16 - 16]
    movaps xmm2, [rax - 2*16 - 16]
    movaps xmm1, [rax - 1*16 - 16]
    movaps xmm0, [rax - 0*16 - 16]
  {$endif}
  jae @aligned_writes
  jmp @write_16112

  // unaligned sse read
  @unaligned_reads:
  {$ifdef CPUX86}
    movups xmm7, [eax - 7*16 - 16]
    movups xmm6, [eax - 6*16 - 16]
    movups xmm5, [eax - 5*16 - 16]
    movups xmm4, [eax - 4*16 - 16]
    movups xmm3, [eax - 3*16 - 16]
    movups xmm2, [eax - 2*16 - 16]
    movups xmm1, [eax - 1*16 - 16]
    movups xmm0, [eax - 0*16 - 16]
    jae @aligned_writes
  @write_16112:
    lea ebx, [offset @aligned_writes + 8*4 + ecx*2]
    jmp ebx
  {$else .CPUX64}
    movups xmm7, [rax - 7*16 - 16]
    movups xmm6, [rax - 6*16 - 16]
    movups xmm5, [rax - 5*16 - 16]
    movups xmm4, [rax - 4*16 - 16]
    movups xmm3, [rax - 3*16 - 16]
    movups xmm2, [rax - 2*16 - 16]
    movups xmm1, [rax - 1*16 - 16]
    movups xmm0, [rax - 0*16 - 16]
    jae @aligned_writes
  @write_16112:
    lea r9, [@aligned_writes + 8*4]
    lea r9, [r9 + rcx*2]
    jmp r9
  {$endif}

  // aligned sse write, loop
  @aligned_writes:
  {$ifdef CPUX86}
    movaps [edx - 7*16 - 16], xmm7
    movaps [edx - 6*16 - 16], xmm6
    movaps [edx - 5*16 - 16], xmm5
    movaps [edx - 4*16 - 16], xmm4
    movaps [edx - 3*16 - 16], xmm3
    movaps [edx - 2*16 - 16], xmm2
    movaps [edx - 1*16 - 16], xmm1
    movaps [edx - 0*16 - 16], xmm0
    test ecx, ecx
  {$else .CPUX64}
    movaps [rdx - 7*16 - 16], xmm7
    movaps [rdx - 6*16 - 16], xmm6
    movaps [rdx - 5*16 - 16], xmm5
    movaps [rdx - 4*16 - 16], xmm4
    movaps [rdx - 3*16 - 16], xmm3
    movaps [rdx - 2*16 - 16], xmm2
    movaps [rdx - 1*16 - 16], xmm1
    movaps [rdx - 0*16 - 16], xmm0
    test rcx, rcx
  {$endif}
  jg @move_16128

  // last 0..15 bytes
  {$ifdef CPUX86}
    pop ecx
    pop ebx
    and ecx, 15
    jnz @move_115
    ret
  @move_115:
    add eax, ecx
    add edx, ecx
    movups xmm0, [eax - 0*16 - 16]
    movups [edx - 0*16 - 16], xmm0
  {$else .CPUX64}
    movaps xmm6, [rsp-8-16]
    movaps xmm7, [rsp-8-32]
    and r8, 15
    jnz @move_115
    ret
  @move_115:
    add rax, r8
    add rdx, r8
    movups xmm0, [rax - 0*16 - 16]
    movups [rdx - 0*16 - 16], xmm0
  {$endif}
end;
{$endif .CPUINTEL}


{ TCachedReader }

constructor TCachedReader.Create(const Callback: TCachedReaderCallback;
  const BufferSize: NativeUInt);
begin
  inherited Create(True, TMethod(Callback), BufferSize);
end;

procedure TCachedReader.DoFlush;
var
  FilledSize, Margin: NativeUInt;
  Handle: Pointer;
  Temp: TCachedBufferMemory;
begin
  Margin := NativeUInt(Self.Overflow) - NativeUInt(Self.Current); //todo Self.Margin;

  // ignore overflows
  if (NativeInt(Margin) < 0) then
  begin
    Self.Current := Self.Overflow;
    Margin := 0;
  end;

  // move margin data to previous memory
  if (Margin > 0) then
  begin
    if (Margin > FMemory.PreviousSize) then
    begin
      Temp := CachedBufferMemory(Margin, FMemory.BufferSize);
      Handle := FMemory.Handle;
      try
        NcMove(Pointer(NativeUInt(FMemory.Buffer)+FMemory.BufferSize-Margin)^,
               Pointer(NativeUInt(Temp.Buffer)-Margin)^, Margin);
        FMemory := Temp;
      finally
        FreeMem(Handle);
      end;
    end else
    begin
      NcMove(Pointer(NativeUInt(FMemory.Buffer)+FMemory.BufferSize-Margin)^,
             Pointer(NativeUInt(FMemory.Buffer)-Margin)^, Margin);
    end;
  end;

  // run callback
  FilledSize := TCachedReaderCallback(FCallback)(Self, FMemory.Buffer, FMemory.BufferSize);
  if (FilledSize <> FMemory.BufferSize) then FIsFinishing := True;

  // fill Current and Overflow
  Current := Pointer(NativeUInt(FMemory.Buffer) - Margin{>=0});
  FOverflow := Pointer(NativeUInt(FMemory.Buffer) + FilledSize);
end;

procedure TCachedReader.RaiseFinishedReading();
begin
  RaiseFinishing('read data');
end;

// Margin < Count
procedure TCachedReader.DifficultRead(Buffer: PByte; Count: NativeUInt);
var
  S: NativeUInt;
  Margin: NativeInt;
begin
  if (FIsFinishing) then RaiseFinishedReading();

  // read Margin
  Margin := NativeInt(Self.Overflow) - NativeInt(Self.Current);
  if (Margin > 0) then
  begin
    NcMove(Current^, Buffer^, Margin);
    Inc(NativeInt(Buffer), Margin);
    Inc(NativeInt(Current), Margin);
    Dec(NativeInt(Count), Margin);
  end;

  // if read data is too large, we can run reader callback directly
  if (Count >= FMemory.BufferSize) then
  begin
    S := Count - (Count mod FMemory.BufferSize);
    if (S <> TCachedReaderCallback(FCallback)(Self, Buffer, S)) then RaiseFinishedReading();

    Inc(NativeUInt(Buffer), S);
    Dec(Count, S);
    Inc(FFlushedPosition, S);
  end;

  // last Buffer bytes
  if (Count <> 0) then
  begin
    Flush();
    Margin := NativeInt(Self.Overflow) - NativeInt(Self.Current);
    if (NativeUInt(Margin) < Count) then RaiseFinishedReading();

    NcMove(Current^, Buffer^, Count);
    Inc(NativeUInt(Current), Count);
  end;
end;

procedure TCachedReader.ReadData(var Value: Boolean);
var
  P: ^Boolean;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

{$ifNdef NEXTGEN}
procedure TCachedReader.ReadData(var Value: AnsiChar);
var
  P: ^AnsiChar;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;
{$endif}

procedure TCachedReader.ReadData(var Value: WideChar);
var
  P: ^WideChar;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: ShortInt);
var
  P: ^ShortInt;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Byte);
var
  P: ^Byte;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: SmallInt);
var
  P: ^SmallInt;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Word);
var
  P: ^Word;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Integer);
var
  P: ^Integer;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Cardinal);
var
  P: ^Cardinal;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Int64);
var
  P: ^Int64;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

{$if Defined(FPC) or (CompilerVersion >= 15)}
procedure TCachedReader.ReadData(var Value: UInt64);
var
  P: ^UInt64;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;
{$ifend}

procedure TCachedReader.ReadData(var Value: Single);
var
  P: ^Single;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Double);
var
  P: ^Double;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: TExtended80Rec);
var
  P: ^TExtended80Rec;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Currency);
var
  P: ^Currency;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: TPoint);
var
  P: ^TPoint;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: TRect);
var
  P: ^TRect;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then
  begin
    DifficultRead(Pointer(@Value), SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

{$ifNdef NEXTGEN}
procedure TCachedReader.ReadData(var Value: ShortString{; MaxLength: Byte});
var
  P: PByte;
  L, M, S: NativeInt;
begin
  P := Current;
  if (NativeUInt(P) >= NativeUInt(Self.Overflow)) then
  begin
    if (Flush < SizeOf(Byte)) then RaiseFinishedReading;
    P := Current;
  end;

  L := P^;
  Inc(P);
  Current := P;

  M := High(Value);
  if (M < L) then
  begin
    PByte(@Value)^ := M;
    S := L - M;
    Read(Value[1], M);

    P := Current;
    Inc(P, S);
    Current := P;
    if (NativeUInt(P) >= NativeUInt(Self.Overflow)) then Flush;
  end else
  begin
    PByte(@Value)^ := L;
    Read(Value[1], L);
  end;
end;


{$ifdef INTERNALCODEPAGE}
procedure LStrSetLength(var Str: AnsiString; NewLength: Integer; CodePage: Word);
asm
  jmp System.@LStrSetLength
end;
{$endif}

procedure TCachedReader.ReadData(var Value: AnsiString{$ifdef INTERNALCODEPAGE}; CodePage: Word{$endif});
var
  L: Integer;
begin
  ReadData(L);
  {$ifdef INTERNALCODEPAGE}
  LStrSetLength(Value, L, CodePage);
  {$else}
  SetLength(Value, L);
  {$endif}
  if (L <> 0) then Read(Pointer(Value)^, L);
end;

procedure TCachedReader.ReadData(var Value: WideString);
var
  L: Integer;
begin
  ReadData(L);
  SetLength(Value, L);
  if (L <> 0) then Read(Pointer(Value)^, L*2);
end;
{$endif}

{$ifdef UNICODE}
procedure TCachedReader.ReadData(var Value: UnicodeString);
var
  L: Integer;
begin
  ReadData(L);
  SetLength(Value, L);
  if (L <> 0) then Read(Pointer(Value)^, L*2);
end;
{$endif}

procedure TCachedReader.ReadData(var Value: TBytes);
var
  L: Integer;
begin
  ReadData(L);
  SetLength(Value, L);
  if (L <> 0) then Read(Pointer(Value)^, L);
end;

procedure TCachedReader.ReadData(var Value: Variant);
const
  varDeepData = $BFE8;
var
  VarData: PVarData;
begin
  VarData := @TVarData(Value);
  if (VarData.VType and varDeepData <> 0) then
  begin
    case VarData.VType of
      {$ifNdef NEXTGEN}
      varString:
      begin
        AnsiString(VarData.VString) := '';
        Exit;
      end;
      varOleStr:
      begin
        WideString(VarData.VPointer{VOleStr}) := '';
        Exit;
      end;
      {$endif}

      {$ifdef UNICODE}
      varUString:
      begin
        UnicodeString(VarData.VUString) := '';
        Exit;
      end;
      {$endif}
    else
      {$if Defined(FPC) or (CompilerVersion >= 15)}
        if (Assigned(VarClearProc)) then
        begin
          VarClearProc(VarData^);
        end else
        RaiseVaraintType(VarData.VType);
      {$else}
        VarClear(Value);
      {$ifend}
    end;
  end;
  VarData.VPointer := nil;

  ReadData(VarData.VType);
  case VarData.VType of
    varBoolean,
    varShortInt,
    varByte: ReadData(VarData.VByte);

    varSmallInt,
    varWord: ReadData(VarData.VWord);

    varInteger,
    varLongWord,
    varSingle: ReadData(VarData.VInteger);

    varDouble,
    varCurrency,
    varDate,
    varInt64,
    $15{varUInt64}: ReadData(VarData.VInt64);

    {$ifNdef NEXTGEN}
    varString:
    begin
      ReadData(AnsiString(VarData.VPointer));
      Exit;
    end;
    varOleStr:
    begin
      ReadData(WideString(VarData.VPointer));
      Exit;
    end;
    {$endif}

    {$ifdef UNICODE}
    varUString:
    begin
      ReadData(UnicodeString(VarData.VPointer));
      Exit;
    end;
    {$endif}
  else
    RaiseVaraintType(VarData.VType);
  end;
end;


{ TCachedWriter }

constructor TCachedWriter.Create(const Callback: TCachedWriterCallback;
  const BufferSize: NativeUInt);
begin
  inherited Create(False, TMethod(Callback), BufferSize);
end;

procedure TCachedWriter.DoFlush;
var
  Size: NativeUInt;
  Margin: NativeInt;
begin
  Size := FMemory.BufferSize;

  // finishing
  Margin := NativeInt(Self.Overflow) - NativeInt(Self.Current); // todo Self.Margin
  if (Margin > 0) then
  begin
    Dec(Size, Margin);
    FIsFinishing := True;
    Margin := 0;
    if (Size = 0) then Exit;
  end;

  // run callback
  if (Size <> TCachedWriterCallback(FCallback)(Self, FMemory.Buffer, Size)) then
  RaiseReadWrite(Size);

  // copy margin (if needed)
  if (Margin < 0) then
  begin
    NcMove(Pointer(NativeUInt(FMemory.Buffer)+FMemory.BufferSize)^,
           FMemory.Buffer^, -Margin);
  end;

  // fill Current and Overflow
  Current := Pointer(NativeInt(FMemory.Buffer) - Margin{<=0});
  FOverflow := Pointer(NativeUInt(FMemory.Buffer) + FMemory.BufferSize);
end;

// Count > Margin
procedure TCachedWriter.DifficultWrite(Buffer: PByte; Count: NativeUInt);
var
  S: NativeUInt;
  Margin: NativeInt;
begin
  // write Margin
  if (Current <> FMemory.Buffer) then
  begin
    Margin := NativeInt(Self.Overflow) - NativeInt(Self.Current);

    if (Margin > 0) then
    begin
      NcMove(Buffer^, Current^, Margin);

      Current := Self.Overflow;
      Inc(Buffer, Margin);
      Dec(Count, Margin);
    end;

    Flush();
  end;

  // if written data is too large, we can run writer callback directly
  if (Count >= FMemory.BufferSize) then
  begin
    S := Count - (Count mod FMemory.BufferSize);
    if (S <> TCachedWriterCallback(FCallback)(Self, Buffer, S)) then RaiseReadWrite(S);

    Inc(Buffer, S);
    Dec(Count, S);
    Inc(FFlushedPosition, S);
  end;

  // last Buffer bytes
  if (Count <> 0) then
  begin
    NcMove(Buffer^, Current^, Count);
    Inc(Current, Count);
  end;
end;

procedure TCachedWriter.WriteData(const Value: Boolean);
var
  P: ^Boolean;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$ifNdef NEXTGEN}
procedure TCachedWriter.WriteData(const Value: AnsiChar);
var
  P: ^AnsiChar;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;
{$endif}

procedure TCachedWriter.WriteData(const Value: WideChar);
var
  P: ^WideChar;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: ShortInt);
var
  P: ^ShortInt;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Byte);
var
  P: ^Byte;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: SmallInt);
var
  P: ^SmallInt;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Word);
var
  P: ^Word;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Integer);
var
  P: ^Integer;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Cardinal);
var
  P: ^Cardinal;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Int64);
var
  P: ^Int64;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$if Defined(FPC) or (CompilerVersion >= 15)}
procedure TCachedWriter.WriteData(const Value: UInt64);
var
  P: ^UInt64;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;
{$ifend}

procedure TCachedWriter.WriteData(const Value: Single);
var
  P: ^Single;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Double);
var
  P: ^Double;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: TExtended80Rec);
var
  P: ^TExtended80Rec;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: Currency);
var
  P: ^Currency;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: TPoint);
var
  P: ^TPoint;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const Value: TRect);
var
  P: ^TRect;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$ifNdef NEXTGEN}
procedure TCachedWriter.WriteData(const Value: ShortString);
begin
  Write(Value, Length(Value)+1);
end;

procedure TCachedWriter.WriteData(const Value: AnsiString);
var
  P: PInteger;
begin
  P := Pointer(Value);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    Dec(P);
    Write(P^, P^ + SizeOf(Integer));
  end;
end;

procedure TCachedWriter.WriteData(const Value: WideString);
var
  P: PInteger;
begin
  P := Pointer(Value);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    Dec(P);
    {$if Defined(MSWINDOWS) or Defined(FPC) or (CompilerVersion < 22)}
      WriteData(P^ shr 1);
      Write(Pointer(NativeUInt(P) + SizeOf(Integer))^, P^);
    {$else}
      Write(P^, P^*2 + SizeOf(Integer));
    {$ifend}
  end;
end;
{$endif}

{$ifdef UNICODE}
procedure TCachedWriter.WriteData(const Value: UnicodeString);
var
  P: PInteger;
begin
  P := Pointer(Value);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    Dec(P);
    Write(P^, P^*2 + SizeOf(Integer));
  end;
end;
{$endif}

procedure TCachedWriter.WriteData(const Value: TBytes);
var
  P: PNativeInt;
  {$if Defined(FPC) or (SizeOf(NativeInt) = 8)}
  L: Integer;
  {$ifend}
begin
  P := Pointer(Value);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    Dec(P);
    {$if Defined(FPC) or (SizeOf(NativeInt) = 8)}
      L := P^{$ifdef FPC}+1{$endif};
      Inc(P);
      WriteData(L);
      Write(P^, L);
    {$else}
      Write(P^, P^ + SizeOf(Integer));
    {$ifend}
  end;
end;

procedure TCachedWriter.WriteData(const Value: Variant);
var
  VType: Word;
  VPtr: Pointer;
begin
  VType := TVarData(Value).VType;
  VPtr := @TVarData(Value).VByte;

  if (VType and varByRef <> 0) then
  begin
    VType := VType and (not varByRef);
    VPtr := PPointer(VPtr)^;
  end;

  WriteData(VType);
  case VType of
    varBoolean,
    varShortInt,
    varByte: WriteData(PByte(VPtr)^);

    varSmallInt,
    varWord: WriteData(PWord(VPtr)^);

    varInteger,
    varLongWord,
    varSingle: WriteData(PInteger(VPtr)^);

    varDouble,
    varCurrency,
    varDate,
    varInt64,
    $15{varUInt64}: WriteData(PInt64(VPtr)^);

    {$ifNdef NEXTGEN}
    varString:
    begin
      WriteData(PAnsiString(VPtr)^);
      Exit;
    end;
    varOleStr:
    begin
      WriteData(PWideString(VPtr)^);
      Exit;
    end;
    {$endif}

    {$ifdef UNICODE}
    varUString:
    begin
      WriteData(PUnicodeString(VPtr)^);
      Exit;
    end;
    {$endif}
  else
    RaiseVaraintType(VType);
  end;
end;


{ TCachedReReader }

constructor TCachedReReader.Create(const Callback: TCachedReReaderCallback;
  const Source: TCachedReader; const IsOwner: Boolean;
  const BufferSize: NativeUInt);
begin
  FReCallback := Callback;
  FSource := Source;
  FReserved1 := IsOwner;
  FReserved2 := GetIsDirect;

  if (IsDirect) then
  begin
    Self.CopyParameters(Source);
    TCachedReaderCallback(Self.FCallback) := Self.InternalCallback;
  end else
  begin
    inherited Create(InternalCallback, BufferSize);
  end;
end;

function TCachedReReader.GetIsDirect: Boolean;
begin
  Result := False;
end;

destructor TCachedReReader.Destroy;
begin
  if (IsDirect) then FMemory.Handle := nil;  
  inherited;
  if (IsOwner) then FSource.Free;
end;

function TCachedReReader.InternalCallback(Sender: TCachedReader; Buffer: PByte;
  BufferSize: NativeUInt): NativeUInt;
begin
  if (IsDirect) then
  begin
    Source.CopyParameters(Self);
    Result := TCachedReaderCallback(Source.FCallback)(Source, {FMemory.}Buffer, {FMemory.}BufferSize);
  end else
  begin
    Result := FReCallback(Self, Buffer, BufferSize, Source);
  end;
end;


{ TCachedReWriter }

constructor TCachedReWriter.Create(const Callback: TCachedReWriterCallback;
  const Destination: TCachedWriter; const IsOwner: Boolean;
  const BufferSize: NativeUInt);
begin
  FReCallback := Callback;
  FDestination := Destination;
  FReserved1 := IsOwner;
  FReserved2 := GetIsDirect;

  if (IsDirect) then
  begin
    Self.CopyParameters(Destination);
    TCachedWriterCallback(Self.FCallback) := Self.InternalCallback;
  end else
  begin
    inherited Create(InternalCallback, BufferSize);
  end;
end;

function TCachedReWriter.GetIsDirect: Boolean;
begin
  Result := False;
end;

destructor TCachedReWriter.Destroy;
begin
  if (IsDirect) then FMemory.Handle := nil;
  inherited;
  if (IsOwner) then FDestination.Free;
end;

function TCachedReWriter.InternalCallback(Sender: TCachedWriter;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
begin
  if (IsDirect) then
  begin
    Destination.CopyParameters(Self);
    Result := TCachedWriterCallback(Destination.FCallback)(Destination, {FMemory.}Buffer, {FMemory.}BufferSize);
  end else
  begin
    Result := FReCallback(Self, Buffer, BufferSize, Destination);
  end;
end;


{ TCachedFileReader }

constructor TCachedFileReader.Create(const FileName: string; const Offset,
  LimitSize: Int64);

  procedure RaiseOpen;
  begin
    raise ECachedBuffer.CreateFmt('Cannot open file:'#13'%s', [FileName]);
  end;
begin
  {$ifdef MSWINDOWS}
  FHandle := CreateFile(PChar(FileName), $0001{FILE_READ_DATA}, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  {$else}
  FHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  {$endif}
  if (FHandle = INVALID_HANDLE_VALUE) then RaiseOpen;

  Create(FHandle);
end;

constructor TCachedFileReader.Create(const Handle: THandle;
  const LimitSize: Int64; const HandleOwner: Boolean);
var
  BufferSize: NativeUInt;
begin
  FHandle := Handle;
  FReserved1{IsOwner} := HandleOwner;
  FReserved2{Limited} := (LimitSize > 0);

  BufferSize := 0;
  if (Limited) then
  begin
    FLimitSize := LimitSize;
    if (LimitSize < DEFAULT_CACHED_SIZE) then BufferSize := LimitSize;
  end;

  inherited Create(InternalCallback, BufferSize);
end;

function TCachedFileReader.InternalCallback(Sender: TCachedReader;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
var
  I, Size: Integer;
begin
  Result := 0;

  repeat
    if (BufferSize > NativeUInt(High(Integer))) then Size := High(Integer)
    else Size := BufferSize;

    I := FileRead(FHandle, Buffer^, Size);
    if (I < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};

    Inc(Buffer, I);
    Dec(BufferSize, I);
    Inc(Result, I);
  until (I <> Size) or (BufferSize = 0);
end;

destructor TCachedFileReader.Destroy;
begin
  inherited;

  if (IsHandleOwner) and
     (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE) then FileClose(FHandle);
end;


{ TCachedFileWriter }

constructor TCachedFileWriter.Create(const FileName: string);
begin
  {$ifdef MSWINDOWS}
  FHandle := CreateFile(PChar(FileName), $0002{FILE_WRITE_DATA}, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0);
  {$else}
  FHandle := FileCreate(FileName);
  {$endif}
  if (FHandle = INVALID_HANDLE_VALUE) then raise ECachedBuffer.CreateFmt('Cannot create file:'#13'%s', [FileName]);

  Create(FHandle);
end;

constructor TCachedFileWriter.Create(const Handle: THandle; const LimitSize: Int64;
  const HandleOwner: Boolean);
var
  BufferSize: NativeUInt;
begin
  FHandle := Handle;
  FReserved1{IsOwner} := HandleOwner;
  FReserved2{Limited} := (LimitSize > 0);

  BufferSize := 0;
  if (Limited) then
  begin
    FLimitSize := LimitSize;
    if (LimitSize < DEFAULT_CACHED_SIZE) then BufferSize := LimitSize;
  end;

  inherited Create(InternalCallback, BufferSize);
end;

function TCachedFileWriter.InternalCallback(Sender: TCachedWriter;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
var
  I, Size: Integer;
begin
  Result := 0;

  repeat
    if (BufferSize > NativeUInt(High(Integer))) then Size := High(Integer)
    else Size := BufferSize;

    I := FileWrite(FHandle, Buffer^, Size);
    if (I < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};

    Inc(Buffer, I);
    Dec(BufferSize, I);
    Inc(Result, I);
  until (I <> Size) or (BufferSize = 0);
end;

destructor TCachedFileWriter.Destroy;
begin
  inherited;

  if (IsHandleOwner) and
     (Handle <> 0) and (Handle <> INVALID_HANDLE_VALUE) then FileClose(FHandle);
end;


{ TCachedMemoryReader }

constructor TCachedMemoryReader.Create(const Ptr: Pointer;
  const Size: NativeUInt);
var
  BufferSize: NativeUInt;
begin
  FPtr := Ptr;
  FPtrMargin := Size;

  if (Size = 0) then
  begin
    // inherited Create;
    FIsReader := True;
    FIsFinishing := True;
  end else
  begin
    BufferSize := 0;
    if (Size < DEFAULT_CACHED_SIZE) then BufferSize := Size;

    inherited Create(InternalCallback, BufferSize);
  end;
end;

function TCachedMemoryReader.InternalCallback(Sender: TCachedReader;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
begin
  Result := BufferSize;
  if (Result > FPtrMargin) then Result := FPtrMargin;

  NcMove(FPtr^, Buffer^, Result);
  Inc(FPtr, Result);
  Dec(FPtrMargin, Result);
end;


{ TCachedMemoryWriter }

constructor TCachedMemoryWriter.Create(const Ptr: Pointer;
  const Size: NativeUInt);
var
  BufferSize: NativeUInt;
begin
  FPtr := Ptr;
  FPtrMargin := Size;

  if (Size = 0) then
  begin
    // inherited Create;
    FIsReader := False;
    FIsFinishing := True;
  end else
  begin
    BufferSize := 0;
    if (Size < DEFAULT_CACHED_SIZE) then BufferSize := Size;

    inherited Create(InternalCallback, BufferSize);
  end;
end;

function TCachedMemoryWriter.InternalCallback(Sender: TCachedWriter;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
begin
  Result := BufferSize;
  if (Result > FPtrMargin) then Result := FPtrMargin;

  NcMove(Buffer^, FPtr^, Result);
  Inc(FPtr, Result);
  Dec(FPtrMargin, Result);
end;


{ TCachedResourceReader }

{$ifdef MSWINDOWS}
procedure TCachedResourceReader.InternalCreate(Instance: THandle; Name,
  ResType: PChar);

  procedure RaiseNotFound;
  var
    V: NativeUInt;
    N, T: string;
  begin
    V := NativeUInt(Name);
    if (V <= High(Word)) then N := '#' + {$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V))
    else N := '"' + string(Name) + '"';

    V := NativeUInt(ResType);
    if (V <= High(Word)) then T := '#' + {$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V))
    else T:= '"' + string(ResType) + '"';

    raise ECachedBuffer.CreateFmt('Resource %s (%s) not found', [N, T]);
  end;

var
  HResInfo: THandle;
begin
  HResInfo := FindResource(Instance, Name, ResType);
  if (HResInfo = 0) then RaiseNotFound;
  HGlobal := LoadResource(Instance, HResInfo);
  if (HGlobal = 0) then RaiseNotFound;
  inherited Create(LockResource(HGlobal), SizeOfResource(Instance, HResInfo));
end;

constructor TCachedResourceReader.Create(Instance: THandle;
  const ResName: string; ResType: PChar);
begin
  InternalCreate(Instance, PChar(ResName), ResType);
end;

constructor TCachedResourceReader.CreateFromID(Instance: THandle;
  ResID: Word; ResType: PChar);
begin
  InternalCreate(Instance, PChar(NativeUInt(ResID)), ResType);
end;

destructor TCachedResourceReader.Destroy;
begin
  inherited;
  FreeResource(HGlobal);
end;
{$endif}





//initialization
//  TCachedReader(nil).ReadData(PByte(nil)^);

end.
