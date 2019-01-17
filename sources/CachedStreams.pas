unit CachedStreams;

{******************************************************************************}
{ Copyright (c) 2019 Dmitry Mozulyov                                           }
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
{ skype: dimandevil                                                            }
{ repository: https://github.com/d-mozulyov/CachedBuffers                      }
{******************************************************************************}


// compiler directives
{$ifdef FPC}
  {$MODE DELPHIUNICODE}
  {$ASMMODE INTEL}
  {$define INLINESUPPORT}
  {$define INLINESUPPORTSIMPLE}
  {$define OPERATORSUPPORT}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
  {$if Defined(CPUARM) or Defined(UNIX)}
    {$define POSIX}
  {$ifend}
{$else}
  {$if CompilerVersion >= 24}
    {$LEGACYIFEND ON}
  {$ifend}
  {$if CompilerVersion >= 15}
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_TYPE OFF}
    {$WARN UNSAFE_CAST OFF}
  {$ifend}
  {$if CompilerVersion >= 20}
    {$define INLINESUPPORT}
  {$ifend}
  {$if CompilerVersion >= 17}
    {$define INLINESUPPORTSIMPLE}
  {$ifend}
  {$if CompilerVersion >= 18}
    {$define OPERATORSUPPORT}
  {$ifend}
  {$if CompilerVersion < 23}
    {$define CPUX86}
  {$else}
    {$define UNITSCOPENAMES}
  {$ifend}
  {$if CompilerVersion >= 21}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$ifend}
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$ifdef CPUX86}
  {$if not Defined(NEXTGEN)}
    {$define CPUX86ASM}
    {$define CPUINTELASM}
  {$ifend}
  {$define CPUINTEL}
{$endif}
{$ifdef CPUX64}
  {$if (not Defined(POSIX)) or Defined(FPC)}
    {$define CPUX64ASM}
    {$define CPUINTELASM}
  {$ifend}
  {$define CPUINTEL}
{$endif}
{$if Defined(CPUX64) or Defined(CPUARM64)}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

{$ifdef POSIX}
  {$undef CPUX86ASM}
  {$undef CPUX64ASM}
  {$undef CPUINTELASM}
{$endif}

interface
  uses {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif},
       {$ifdef MSWINDOWS}
         {$ifdef UNITSCOPENAMES}
           Winapi.Windows, Winapi.ActiveX,
         {$else}
           Windows, ActiveX,
         {$endif}
       {$endif}
       {$ifdef KOL}
         KOL, err,
       {$else}
         {$ifdef UNITSCOPENAMES}
           System.SysUtils, System.Classes,
         {$else}
           SysUtils, Classes,
         {$endif}
       {$endif}
       CachedBuffers;

type
  // standard types
  {$ifdef FPC}
    PUInt64 = ^UInt64;
    PBoolean = ^Boolean;
    PString = ^string;
  {$else}
    {$if CompilerVersion < 16}
      UInt64 = Int64;
      PUInt64 = ^UInt64;
    {$ifend}
    {$if CompilerVersion < 21}
      NativeInt = Integer;
      NativeUInt = Cardinal;
    {$ifend}
    {$if CompilerVersion < 22}
      PNativeInt = ^NativeInt;
      PNativeUInt = ^NativeUInt;
    {$ifend}
    PWord = ^Word;
  {$endif}
  {$if SizeOf(Extended) >= 10}
    {$define EXTENDEDSUPPORT}
  {$ifend}
  TBytes = {$if (not Defined(FPC)) and (CompilerVersion >= 23)}TArray<Byte>{$else}array of Byte{$ifend};
  PBytes = ^TBytes;

  // CachedBuffers types
  ECachedBuffer = CachedBuffers.ECachedBuffer;
  TCachedBufferKind = CachedBuffers.TCachedBufferKind;
  TCachedBufferCallback = CachedBuffers.TCachedBufferCallback;
  TCachedBufferProgress = CachedBuffers.TCachedBufferProgress;
  TCachedBufferMemory = CachedBuffers.TCachedBufferMemory;
  PCachedBufferMemory = CachedBuffers.PCachedBufferMemory;
  TCachedBuffer = CachedBuffers.TCachedBuffer;
  TCachedReader = CachedBuffers.TCachedReader;
  TCachedWriter = CachedBuffers.TCachedWriter;
  TCachedReReader = CachedBuffers.TCachedReReader;
  TCachedReWriter = CachedBuffers.TCachedReWriter;
  TCachedFileReader = CachedBuffers.TCachedFileReader;
  TCachedFileWriter = CachedBuffers.TCachedFileWriter;
  TCachedMemoryReader = CachedBuffers.TCachedMemoryReader;
  TCachedMemoryWriter = CachedBuffers.TCachedMemoryWriter;
  TCachedResourceReader = CachedBuffers.TCachedResourceReader;

  // Classes.TStream or KOL.PStream
  StreamType = {$ifdef KOL}PStream{$else}TStream{$endif};


{ TCachedStreamReader class }

  TCachedStreamReader = class(TCachedReader)
  protected
    FStream: StreamType;
    FOwner: Boolean;
    FOffset: Int64;
    function InternalCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Stream: StreamType; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);

    property Stream: StreamType read FStream;
    property Owner: Boolean read FOwner write FOwner;
    property Offset: Int64 read FOFfset;
  end;


{ TCachedStreamWriter class }

  TCachedStreamWriter = class(TCachedWriter)
  protected
    FStream: StreamType;
    FOwner: Boolean;
    FOffset: Int64;
    function InternalCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Stream: StreamType; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);

    property Stream: StreamType read FStream;
    property Owner: Boolean read FOwner write FOwner;
    property Offset: Int64 read FOFfset;
  end;


{ TCachedBufferStream class }

  {$ifdef KOL}
  PCachedBufferStream = ^TCachedBufferStream;
  TCachedBufferStream = object(TStream)
  {$else}
  TCachedBufferStream = class(TStream)
  {$endif}
  protected
    FKind: TCachedBufferKind;
    FOwner: Boolean;
    FCachedBuffer: TCachedBuffer;
  {$ifNdef KOL}
  protected
  {$if Defined(FPC) or (CompilerVersion >= 15)}
    function GetSize: Int64; override;
  {$ifend}
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  {$endif}
  {$if Defined(KOL) or (not Defined(AUTOREFCOUNT))}
  public
  {$ifend}
    destructor Destroy; {$ifdef KOL}virtual{$else}override{$endif};
  public
    {$ifNdef KOL}
    constructor Create(const CachedBuffer: TCachedBuffer; const Owner: Boolean = False);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$endif}

    property Kind: TCachedBufferKind read FKind;
    property Owner: Boolean read FOwner;
    property CachedBuffer: TCachedBuffer read FCachedBuffer;
  end;


{ TCachedBufferAdapter }
{ Implements OLE IStream on TCachedBuffer }

  {$ifNdef FPC}
    {$if CompilerVersion >= 29}
      {$define NEWISTREAM}
    {$ifend}
  {$endif}

  TCachedBufferAdapter = class(TInterfacedObject, IStream)
  protected
    FKind: TCachedBufferKind;
    FOwner: Boolean;
    FCachedBuffer: TCachedBuffer;
  public
    constructor Create(const CachedBuffer: TCachedBuffer; const Owner: Boolean = False);
    destructor Destroy; override;

    {$if Defined(FPC)}
      function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HRESULT; stdcall;
      function Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HRESULT; stdcall;
      function Seek(dlibMove: LargeUInt; dwOrigin: Longint; out libNewPosition: LargeUInt): HResult; stdcall;
      function SetSize(libNewSize: LargeUInt): HRESULT; stdcall;
      function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt; out cbWritten: LargeUInt): HRESULT; stdcall;
      function Commit(grfCommitFlags: Longint): HRESULT; stdcall;
      function Revert: HRESULT; stdcall;
      function LockRegion(libOffset: LargeUInt;cb: LargeUInt; dwLockType: Longint): HRESULT; stdcall;
      function UnlockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: Longint): HRESULT; stdcall;
      function Stat(out statstg: TStatStg; grfStatFlag: Longint): HRESULT; stdcall;
      function Clone(out stm: IStream): HRESULT; stdcall;
    {$elseif not Defined(NEWISTREAM)}
      function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult; virtual; stdcall;
      function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult; virtual; stdcall;
      function Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult; virtual; stdcall;
      function SetSize(libNewSize: Largeint): HResult; virtual; stdcall;
      function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult; virtual; stdcall;
      function Commit(grfCommitFlags: Longint): HResult; virtual; stdcall;
      function Revert: HResult; virtual; stdcall;
      function LockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; virtual; stdcall;
      function UnlockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult; virtual; stdcall;
      function Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult; virtual; stdcall;
      function Clone(out stm: IStream): HResult; virtual; stdcall;
    {$else .NEWISTREAM}
      function Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult; virtual; stdcall;
      function Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; virtual; stdcall;
      function Seek(dlibMove: Largeint; dwOrigin: DWORD; out libNewPosition: LargeUInt): HResult; virtual; stdcall;
      function SetSize(libNewSize: LargeUInt): HResult; virtual; stdcall;
      function CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt; out cbWritten: LargeUInt): HResult; virtual; stdcall;
      function Commit(grfCommitFlags: DWORD): HResult; virtual; stdcall;
      function Revert: HResult; virtual; stdcall;
      function LockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult; virtual; stdcall;
      function UnlockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult; virtual; stdcall;
      function Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult; virtual; stdcall;
      function Clone(out stm: IStream): HResult; virtual; stdcall;
    {$ifend}

    property Kind: TCachedBufferKind read FKind;
    property Owner: Boolean read FOwner;
    property CachedBuffer: TCachedBuffer read FCachedBuffer;
  end;


{$ifdef KOL}
function NewCachedBufferStream(const CachedBuffer: TCachedBuffer;
  const Owner: Boolean = False): PCachedBufferStream;
{$endif}

implementation


{ TCachedStreamReader }

constructor TCachedStreamReader.Create(const Stream: StreamType;
  const Owner: Boolean; const BufferSize: NativeUInt);
begin
  FStream := Stream;
  FOwner := Owner;
  FOffset := Stream.Position;
  inherited Create(InternalCallback, BufferSize);
end;

destructor TCachedStreamReader.Destroy;
begin
  inherited;
  if (FOwner) then FStream.Free;
end;

function TCachedStreamReader.InternalCallback(Sender: TCachedBuffer;
  Data: PByte; Size: NativeUInt): NativeUInt;
var
  S, R: Longint;
  N: NativeUInt;
begin
  Result := 0;

  while (Size <> 0) do
  begin
    S := Size;
    if (Size > NativeUInt(High(Longint))) then S := High(Longint);

    R := Stream.Read(Data^, S);
    if (R > 0) then
    begin
      N := NativeUInt(R);
      Inc(Result, N);
      Inc(Data, N);
      Dec(Size, N);
    end;

    if (R <> S) then Break;
  end;
end;


{ TCachedStreamWriter }

constructor TCachedStreamWriter.Create(const Stream: StreamType;
  const Owner: Boolean; const BufferSize: NativeUInt);
begin
  FStream := Stream;
  FOwner := Owner;
  FOffset := Stream.Position;
  inherited Create(InternalCallback, BufferSize);
end;

destructor TCachedStreamWriter.Destroy;
begin
  inherited;
  if (FOwner) then FStream.Free;
end;

function TCachedStreamWriter.InternalCallback(Sender: TCachedBuffer;
  Data: PByte; Size: NativeUInt): NativeUInt;
var
  S, R: Longint;
  N: NativeUInt;
begin
  Result := 0;

  while (Size <> 0) do
  begin
    S := Size;
    if (Size > NativeUInt(High(Longint))) then S := High(Longint);

    R := Stream.Write(Data^, S);
    if (R > 0) then
    begin
      N := NativeUInt(R);
      Inc(Result, N);
      Inc(Data, N);
      Dec(Size, N);
    end;

    if (R <> S) then Break;
  end;
end;


{ TCachedBufferStream }

{$ifNdef KOL}
constructor TCachedBufferStream.Create(const CachedBuffer: TCachedBuffer;
  const Owner: Boolean);
begin
  inherited Create;

  FKind := CachedBuffer.Kind;
  FOwner := Owner;
  FCachedBuffer := CachedBuffer;
end;
{$else}
  {$IFDEF STREAM_LARGE64}
    {$UNDEF STREAM_COMPAT}
  {$ENDIF}
  procedure FillCachedBufferStreamMethods(Stream: PCachedBufferStream); forward;

function NewCachedBufferStream(const CachedBuffer: TCachedBuffer;
  const Owner: Boolean = False): PCachedBufferStream;
begin
  New(Result, Create);

  Result.FKind := CachedBuffer.Kind;
  Result.FOwner := Owner;
  Result.FCachedBuffer := CachedBuffer;

  FillCachedBufferStreamMethods(Result);
end;
{$endif}

destructor TCachedBufferStream.Destroy;
begin
  inherited;
  if (FOwner) then FCachedBuffer.Free;
end;

function CachedBufferSeek(const CachedBuffer: TCachedBuffer;
  Offset: Int64; Origin: Word): Int64;
const
  soFromBeginning = 0;
  soFromCurrent = 1;
var
  Temp: Int64;
  Count: NativeUInt;
begin
  Result := CachedBuffer.Position;
  if (Origin = soFromBeginning) then
  begin
    Temp := Offset;
    Offset := Offset - Result;
    Result := Temp{from beginning Offset};
  end;
  if (Origin <= soFromCurrent) and (Offset = 0) then
    Exit;

  if (CachedBuffer.Kind = cbReader) and (Origin <= soFromCurrent) and (Offset > 0) then
  begin
    while (Offset <> 0) do
    begin
      Count := Offset;
      if (Count > NativeUInt(High(NativeInt))) then Count := NativeUInt(High(NativeInt));
      Offset := Offset - Int64(Count);

      TCachedReader(CachedBuffer).Skip(Count);
    end;
  end else
    raise ECachedBuffer.Create('Invalid seek operation');
end;

{$ifNdef KOL}
{$if Defined(FPC) or (CompilerVersion >= 15)}
function TCachedBufferStream.GetSize: Int64;
begin
  if (CachedBuffer.Limited) then
    Result := CachedBuffer.Limit
  else
    Result := CachedBuffer.Position;
end;
{$ifend}
procedure TCachedBufferStream.SetSize(NewSize: Longint);
begin
  CachedBuffer.Limit := NewSize;
end;

procedure TCachedBufferStream.SetSize(const NewSize: Int64);
begin
  CachedBuffer.Limit := NewSize;
end;

function TCachedBufferStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := CachedBufferSeek(CachedBuffer, Offset, Origin);
end;

function TCachedBufferStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := CachedBufferSeek(CachedBuffer, Offset, Ord(Origin));
end;
{$else .KOL}
function StreamGetSiz(Self: PCachedBufferStream): TStrmSize;
begin
  Result := Self.CachedBuffer.Position;
end;

procedure StreamSetSiz(Self: PCachedBufferStream; {$IFNDEF STREAM_COMPAT}const{$ENDIF} NewSize: TStrmSize);
begin
  Self.CachedBuffer.Limit := NewSize;
end;

function StreamSeek(Self: PCachedBufferStream; {$IFNDEF STREAM_COMPAT}const{$ENDIF} Offset: TStrmMove; Origin: TMoveMethod ): TStrmSize;
begin
  Result := CachedBufferSeek(Self.CachedBuffer, Offset, Ord(Origin));
end;
{$endif}

{$ifNdef KOL}
function TCachedBufferStream.Read(var Buffer; Count: Longint): Longint;
{$else .KOL}
function StreamRead(Self: PCachedBufferStream; var Buffer; {$IFNDEF STREAM_COMPAT}const{$ENDIF} Count: TStrmSize): TStrmSize;
{$endif}
begin
  if (Self.FKind = cbReader) then
  begin
    if (Count <= 0) then
    begin
      Result := 0;
    end else
    begin
      Result := Count;
      TCachedReader(Self.CachedBuffer).Read(Buffer, Result);
    end;
  end else
    raise ECachedBuffer.Create('Invalid read operation');
end;

{$ifNdef KOL}
function TCachedBufferStream.Write(const Buffer; Count: Longint): Longint;
{$else .KOL}
function StreamWrite(Self: PCachedBufferStream; var Buffer; {$IFNDEF STREAM_COMPAT}const{$ENDIF} Count: TStrmSize): TStrmSize;
{$endif}
begin
  if (Self.FKind = cbWriter) then
  begin
    if (Count <= 0) then
    begin
      Result := 0;
    end else
    begin
      Result := Count;
      TCachedWriter(Self.CachedBuffer).Write(Buffer, Result);
    end;
  end else
    raise ECachedBuffer.Create('Invalid write operation');
end;

{$ifdef KOL}
procedure StreamNone(Strm: PStream);
begin
end;

procedure FillCachedBufferStreamMethods(Stream: PCachedBufferStream);
begin
  with Stream.fMethods do
  begin
    fSeek := Pointer(@StreamSeek);
    fGetSiz := Pointer(@StreamGetSiz);
    fSetSiz := Pointer(@StreamSetSiz);
    fRead := Pointer(@StreamRead);
    fWrite := Pointer(@StreamWrite);
    fClose := Pointer(@StreamNone);
  end;
end;
{$endif}


{ TCachedBufferAdapter }

constructor TCachedBufferAdapter.Create(const CachedBuffer: TCachedBuffer;
  const Owner: Boolean);
begin
  inherited Create;

  FKind := CachedBuffer.Kind;
  FOwner := Owner;
  FCachedBuffer := CachedBuffer;
end;

destructor TCachedBufferAdapter.Destroy;
begin
  inherited;
  if (FOwner) then FCachedBuffer.Free;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Read(pv: Pointer; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
{$ifend}
var
  NumRead: Longint;
begin
  if (FKind = cbWriter) then
  begin
    Result := S_FALSE;
    Exit;
  end;

  try
    if (pv = nil) then
    begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    end;

    if (cb <= 0) then
    begin
      NumRead := 0;
    end else
    begin
      NumRead := cb;
      TCachedReader(Self.CachedBuffer).Read(pv^, NumRead);
    end;

    if (pcbRead <> nil) then pcbRead^ := NumRead;
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Write(pv: Pointer; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
{$ifend}
var
  NumWritten: Longint;
begin
  if (FKind = cbReader) then
  begin
    Result := STG_E_CANTSAVE;
    Exit;
  end;

  try
    if (pv = nil) then
    begin
      Result := STG_E_INVALIDPOINTER;
      Exit;
    end;

    if (cb <= 0) then
    begin
      NumWritten := 0;
    end else
    begin
      NumWritten := cb;
      TCachedWriter(Self.CachedBuffer).Write(pv^, NumWritten);
    end;

    if (pcbWritten <> nil) then pcbWritten^ := NumWritten;
    Result := S_OK;
  except
    Result := STG_E_CANTSAVE;
  end;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Seek(dlibMove: LargeUInt; dwOrigin: Longint; out libNewPosition: LargeUInt): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Seek(dlibMove: Largeint; dwOrigin: Longint; out libNewPosition: Largeint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Seek(dlibMove: Largeint; dwOrigin: DWORD; out libNewPosition: LargeUInt): HResult;
{$ifend}
var
  NewPos: Largeint;
begin
  try
    if (Integer(dwOrigin) < STREAM_SEEK_SET) or (dwOrigin > STREAM_SEEK_END) then
    begin
      Result := STG_E_INVALIDFUNCTION;
      Exit;
    end;

    NewPos := CachedBufferSeek(CachedBuffer, dlibMove, dwOrigin);
    if (@libNewPosition <> nil) then libNewPosition := NewPos;
    Result := S_OK;
  except
    Result := STG_E_INVALIDPOINTER;
  end;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.SetSize(libNewSize: LargeUInt): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.SetSize(libNewSize: Largeint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.SetSize(libNewSize: LargeUInt): HResult;
{$ifend}
begin
  try
    CachedBuffer.Limit := libNewSize;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt; out cbWritten: LargeUInt): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint; out cbWritten: Largeint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.CopyTo(stm: IStream; cb: LargeUInt; out cbRead: LargeUInt; out cbWritten: LargeUInt): HResult;
{$ifend}
var
  N: NativeInt;
  BytesRead, BytesWritten: Largeint;
  W: Longint;
begin
  if (Kind = cbWriter) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  BytesRead := 0;
  BytesWritten := 0;
  try
    while (cb > 0) do
    begin
      N := CachedBuffer.Margin;
      if (N <= 0) then
      begin
        if (TCachedReader(CachedBuffer).Finishing) then Break
        else
        begin
          CachedBuffer.Flush;
          N := CachedBuffer.Margin;
          if (N <= 0) then Break;
        end;
      end;

      {$if SizeOf(NativeInt) > SizeOf(Longint)}
      if (N > High(Longint)) then N := High(Longint);
      {$ifend}
      if (N > cb) then N := cb;
      Inc(BytesRead, N);

      W := 0;
      Result := stm.Write(CachedBuffer.Current, N, Pointer(@W));
      Inc(BytesWritten, W);
      if (Result = S_OK) and (W <> N) then Result := E_FAIL;
      if (Result <> S_OK) then Exit;

      Inc(NativeInt(CachedBuffer.Current), N);
      Dec(cb, N);
    end;

    if (@cbWritten <> nil) then cbWritten := BytesWritten;
    if (@cbRead <> nil) then cbRead := BytesRead;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Commit(grfCommitFlags: Longint): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Commit(grfCommitFlags: Longint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Commit(grfCommitFlags: DWORD): HResult;
{$ifend}
begin
  Result := S_OK;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Revert: HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Revert: HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Revert: HResult;
{$ifend}
begin
  Result := STG_E_REVERTED;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.LockRegion(libOffset: LargeUInt;cb: LargeUInt; dwLockType: Longint): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.LockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.LockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult;
{$ifend}
begin
  Result := STG_E_INVALIDFUNCTION;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.UnlockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: Longint): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.UnlockRegion(libOffset: Largeint; cb: Largeint; dwLockType: Longint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.UnlockRegion(libOffset: LargeUInt; cb: LargeUInt; dwLockType: DWORD): HResult;
{$ifend}
begin
  Result := STG_E_INVALIDFUNCTION;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Stat(out statstg: TStatStg; grfStatFlag: Longint): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Stat(out statstg: TStatStg; grfStatFlag: DWORD): HResult;
{$ifend}
begin
  Result := E_NOTIMPL;
end;

{$if Defined(FPC)}
function TCachedBufferAdapter.Clone(out stm: IStream): HRESULT;
{$elseif not Defined(NEWISTREAM)}
function TCachedBufferAdapter.Clone(out stm: IStream): HResult;
{$else .NEWISTREAM}
function TCachedBufferAdapter.Clone(out stm: IStream): HResult;
{$ifend}
begin
  Result := E_NOTIMPL;
end;

end.
