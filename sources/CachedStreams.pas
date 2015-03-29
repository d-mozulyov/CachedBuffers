unit CachedStreams;

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
  uses {$ifdef MSWINDOWS}Windows, ActiveX,{$endif}
       Types, CachedBuffers,
       {$ifdef KOL}
         KOL, err
       {$else}
         SysUtils, Classes
       {$endif};

type
  StreamType = {$ifdef KOL}PStream{$else}TStream{$endif};

{ TCachedStreamReader class }

  TCachedStreamReader = class(TCachedReader)
  private
    function InternalCallback(Sender: TCachedReader; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  protected
    FStream: StreamType;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Stream: StreamType; const IsOwner: Boolean = False; const BufferSize: NativeUInt = 0); reintroduce;

    property IsOwner: Boolean read FReserved1 write FReserved1;
    property Stream: StreamType read FStream;
  end;

{ TCachedStreamWriter class }

  TCachedStreamWriter = class(TCachedWriter)
  private
    function InternalCallback(Sender: TCachedWriter; Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
  protected
    FStream: StreamType;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Stream: StreamType; const IsOwner: Boolean = False; const BufferSize: NativeUInt = 0); reintroduce;

    property IsOwner: Boolean read FReserved1 write FReserved1;
    property Stream: StreamType read FStream;
  end;

{ TCachedBufferStream class }

  {$ifdef KOL}
  PCachedBufferStream = ^TCachedBufferStream;
  TCachedBufferStream = object(TStream)
  {$else}
  TCachedBufferStream = class(TStream)
  {$endif}
  private
    FIsReader: Boolean;
    FIsOwner: Boolean;
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
    constructor Create(const CachedBuffer: TCachedBuffer; const IsOwner: Boolean = False);

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$endif}

    property IsReader: Boolean read FIsReader;
    property IsOwner: Boolean read FIsOwner write FIsOwner;
    property CachedBuffer: TCachedBuffer read FCachedBuffer;
  end;


{ TCachedBufferAdapter }
{ Implements OLE IStream on TCachedBuffer }

{$ifdef MSWINDOWS}
  TCachedBufferAdapter = class(TInterfacedObject, IStream)
  private
    FIsReader: Boolean;
    FIsOwner: Boolean;
    FCachedBuffer: TCachedBuffer;
  public
    constructor Create(const CachedBuffer: TCachedBuffer; const IsOwner: Boolean = False);
    destructor Destroy; override;
    function Read(pv: Pointer; cb: Longint;
      pcbRead: PLongint): HResult; virtual; stdcall;
    function Write(pv: Pointer; cb: Longint;
      pcbWritten: PLongint): HResult; virtual; stdcall;
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HResult; virtual; stdcall;
    function SetSize(libNewSize: Largeint): HResult; virtual; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HResult; virtual; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; virtual; stdcall;
    function Revert: HResult; virtual; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; virtual; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HResult; virtual; stdcall;
    function Stat(out statstg: TStatStg;
      grfStatFlag: Longint): HResult; virtual; stdcall;
    function Clone(out stm: IStream): HResult; virtual; stdcall;

    property IsReader: Boolean read FIsReader;
    property IsOwner: Boolean read FIsOwner write FIsOwner;
    property CachedBuffer: TCachedBuffer read FCachedBuffer;
  end;
{$endif}


{$ifdef KOL}
function NewCachedBufferStream(const CachedBuffer: TCachedBuffer;
  const IsOwner: Boolean = False): PCachedBufferStream;
{$endif}

implementation

// protected fields "helper"
type
  TCachedBufferEx = class(TCachedBuffer);


{ TCachedStreamReader }

constructor TCachedStreamReader.Create(const Stream: StreamType;
  const IsOwner: Boolean; const BufferSize: NativeUInt);
begin
  FStream := Stream;
  FReserved1 := IsOwner;
  inherited Create(InternalCallback, BufferSize);
end;

destructor TCachedStreamReader.Destroy;
begin
  inherited;
  if (IsOwner) then FStream.Free;
end;

function TCachedStreamReader.InternalCallback(Sender: TCachedReader;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
var
  S, R: Longint;
  NR: NativeUInt;
begin
  Result := 0;

  while (BufferSize <> 0) do
  begin
    if (BufferSize > NativeUInt(High(S))) then S := High(S)
    else S := BufferSize;

    R := Stream.Read(Buffer^, S);
    if (R > 0) then
    begin
      NR := NativeUInt(R);

      Inc(Result, NR);
      Inc(Buffer, NR);
      Dec(BufferSize, NR);
    end;

    if (R <> S) then break;
  end;
end;


{ TCachedStreamWriter }

constructor TCachedStreamWriter.Create(const Stream: StreamType;
  const IsOwner: Boolean; const BufferSize: NativeUInt);
begin
  FStream := Stream;
  FReserved1 := IsOwner;
  inherited Create(InternalCallback, BufferSize);
end;

destructor TCachedStreamWriter.Destroy;
begin
  inherited;
  if (IsOwner) then FStream.Free;
end;

function TCachedStreamWriter.InternalCallback(Sender: TCachedWriter;
  Buffer: PByte; BufferSize: NativeUInt): NativeUInt;
var
  S, R: Longint;
  NR: NativeUInt;
begin
  Result := 0;

  while (BufferSize <> 0) do
  begin
    if (BufferSize > NativeUInt(High(S))) then S := High(S)
    else S := BufferSize;

    R := Stream.Write(Buffer^, S);
    if (R > 0) then
    begin
      NR := NativeUInt(R);

      Inc(Result, NR);
      Inc(Buffer, NR);
      Dec(BufferSize, NR);
    end;

    if (R <> S) then break;
  end;
end;


{ TCachedBufferStream }

{$ifNdef KOL}
constructor TCachedBufferStream.Create(const CachedBuffer: TCachedBuffer;
  const IsOwner: Boolean);
begin
  inherited Create;

  FIsReader := TCachedBufferEx(CachedBuffer).FIsReader;
  FIsOwner := IsOwner;
  FCachedBuffer := CachedBuffer;
end;
{$else}
  {$IFDEF STREAM_LARGE64}
    {$UNDEF STREAM_COMPAT}
  {$ENDIF}
  procedure FillCachedBufferStreamMethods(Stream: PCachedBufferStream); forward;

function NewCachedBufferStream(const CachedBuffer: TCachedBuffer;
  const IsOwner: Boolean = False): PCachedBufferStream;
begin
  New(Result, Create);

  Result.FIsReader := TCachedBufferEx(CachedBuffer).FIsReader;
  Result.FIsOwner := IsOwner;
  Result.FCachedBuffer := CachedBuffer;

  FillCachedBufferStreamMethods(Result);
end;
{$endif}

destructor TCachedBufferStream.Destroy;
begin
  inherited;
  if (FIsOwner) then FCachedBuffer.Free;
end;

{$ifNdef KOL}
{$if Defined(FPC) or (CompilerVersion >= 15)}
function TCachedBufferStream.GetSize: Int64;
begin
  Result := CachedBuffer.Position;
end;
{$ifend}
procedure TCachedBufferStream.SetSize(NewSize: Longint);
begin
  if (not FIsReader) and (NewSize > 0) then
  begin
    CachedBuffer.Position := NewSize;
  end else
  begin
    TCachedBufferEx(CachedBuffer).RaiseOperation('SetSize');
  end;
end;

procedure TCachedBufferStream.SetSize(const NewSize: Int64);
begin
  if (not FIsReader) and (NewSize > 0) then
  begin
    CachedBuffer.Position := NewSize;
  end else
  begin
    TCachedBufferEx(CachedBuffer).RaiseOperation('SetSize');
  end;
end;

function TCachedBufferStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := CachedBuffer.Seek(Offset, Origin);
end;

function TCachedBufferStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := CachedBuffer.Seek(Offset, Ord(Origin));
end;
{$else .KOL}
function StreamGetSiz(Self: PCachedBufferStream): TStrmSize;
begin
  Result := Self.CachedBuffer.Position;
end;

procedure StreamSetSiz(Self: PCachedBufferStream; {$IFNDEF STREAM_COMPAT}const{$ENDIF} NewSize: TStrmSize);
begin
  if (not Self.FIsReader) and (NewSize > 0) then
  begin
    Self.CachedBuffer.Position := NewSize;
  end else
  begin
    TCachedBufferEx(Self.CachedBuffer).RaiseOperation('SetSize');
  end;
end;

function StreamSeek(Self: PCachedBufferStream; {$IFNDEF STREAM_COMPAT}const{$ENDIF} Offset: TStrmMove; Origin: TMoveMethod ): TStrmSize;
begin
  Result := Self.CachedBuffer.Seek(Offset, Ord(Origin));
end;
{$endif}

{$ifNdef KOL}
function TCachedBufferStream.Read(var Buffer; Count: Longint): Longint;
{$else .KOL}
function StreamRead(Self: PCachedBufferStream; var Buffer; {$IFNDEF STREAM_COMPAT}const{$ENDIF} Count: TStrmSize): TStrmSize;
{$endif}
begin
  if (Self.FIsReader) then
  begin
    if (Count <= 0) then
    begin
      Result := 0;
    end else
    begin
      Result := Count;

      if (Self.CachedBuffer.IsFinishing) then
      begin
        if (NativeInt(Result) > Self.CachedBuffer.Margin) then
          Result := Self.CachedBuffer.Margin;
      end;

      if (Result <> 0) then      
      TCachedReader(Self.CachedBuffer).Read(Buffer, Result);
    end;
  end else
  begin
    Result := 0;
    TCachedBufferEx(Self.CachedBuffer).RaiseOperation('Read');
  end;
end;

{$ifNdef KOL}
function TCachedBufferStream.Write(const Buffer; Count: Longint): Longint;
{$else .KOL}
function StreamWrite(Self: PCachedBufferStream; var Buffer; {$IFNDEF STREAM_COMPAT}const{$ENDIF} Count: TStrmSize): TStrmSize;
{$endif}
begin
  if (not Self.FIsReader) then
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
  begin
    Result := 0;
    TCachedBufferEx(Self.CachedBuffer).RaiseOperation('Write');
  end;
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

{$ifdef MSWINDOWS}
constructor TCachedBufferAdapter.Create(const CachedBuffer: TCachedBuffer;
  const IsOwner: Boolean);
begin
  inherited Create;

  FIsReader := TCachedBufferEx(CachedBuffer).FIsReader;
  FIsOwner := IsOwner;
  FCachedBuffer := CachedBuffer;
end;

destructor TCachedBufferAdapter.Destroy;
begin
  inherited;
  if (FIsOwner) then CachedBuffer.Free;
end;

function TCachedBufferAdapter.Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HResult;
var
  NumRead: Longint;
begin
  if (not IsReader) then
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

      if (Self.CachedBuffer.IsFinishing) then
      begin
        if (NativeInt(NumRead) > Self.CachedBuffer.Margin) then
          NumRead := Self.CachedBuffer.Margin;
      end;

      if (NumRead <> 0) then      
      TCachedReader(Self.CachedBuffer).Read(pv^, NumRead);
    end;

    if (pcbRead <> nil) then pcbRead^ := NumRead;
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

function TCachedBufferAdapter.Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HResult;
var
  NumWritten: Longint;
begin
  if (IsReader) then
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

function TCachedBufferAdapter.Seek(dlibMove: Largeint; dwOrigin: Longint;
  out libNewPosition: Largeint): HResult;
var
  NewPos: LargeInt;
begin
  try
    if (dwOrigin < STREAM_SEEK_SET) or (dwOrigin > STREAM_SEEK_END) then
    begin
      Result := STG_E_INVALIDFUNCTION;
      Exit;
    end;

    NewPos := CachedBuffer.Seek(dlibMove, dwOrigin);
    if (@libNewPosition <> nil) then libNewPosition := NewPos;
    Result := S_OK;
  except
    Result := STG_E_INVALIDPOINTER;
  end;
end;

function TCachedBufferAdapter.SetSize(libNewSize: Largeint): HResult;
begin
  if (IsReader) or (libNewSize <= 0) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;

  try
    CachedBuffer.Position := libNewSize;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TCachedBufferAdapter.CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
  out cbWritten: Largeint): HResult;
var
  N: NativeInt;
  BytesRead, BytesWritten: LargeInt;
  W: Longint;
begin
  if (not IsReader) then
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
        if (CachedBuffer.IsFinishing) then break
        else
        begin
          CachedBuffer.Flush;
          N := CachedBuffer.Margin;
          if (N <= 0) then break;         
        end;
      end;

      {$if SizeOf(NativeInt) > SizeOf(Longint)}
      if (N > High(Longint)) then N := High(Longint);
      {$ifend}
      if (N > cb) then N := cb;
      Inc(BytesRead, N);

      W := 0;
      Result := stm.Write(CachedBuffer.Current, N, @W);
      Inc(BytesWritten, W);
      if (Result = S_OK) and (W <> N) then Result := E_FAIL;
      if (Result <> S_OK) then Exit;

      Inc(NativeInt(CachedBuffer.Current), N);
      Dec(CachedBuffer.Margin, N);
      cb := cb - N;
    end;

    if (@cbWritten <> nil) then cbWritten := BytesWritten;
    if (@cbRead <> nil) then cbRead := BytesRead;
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;

function TCachedBufferAdapter.Commit(grfCommitFlags: Longint): HResult;
begin
  Result := S_OK;
end;

function TCachedBufferAdapter.Revert: HResult;
begin
  Result := STG_E_REVERTED;
end;

function TCachedBufferAdapter.LockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TCachedBufferAdapter.UnlockRegion(libOffset: Largeint; cb: Largeint;
  dwLockType: Longint): HResult;
begin
  Result := STG_E_INVALIDFUNCTION;
end;

function TCachedBufferAdapter.Stat(out statstg: TStatStg; grfStatFlag: Longint): HResult;
begin
  Result := E_NOTIMPL;
end;

function TCachedBufferAdapter.Clone(out stm: IStream): HResult;
begin
  Result := E_NOTIMPL;
end;
{$endif}

end.