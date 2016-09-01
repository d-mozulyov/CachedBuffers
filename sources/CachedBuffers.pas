unit CachedBuffers;

{******************************************************************************}
{ Copyright (c) 2013 Dmitry Mozulyov                                           }
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
  {$mode delphi}
  {$asmmode intel}
  {$define INLINESUPPORT}
  {$define INLINESUPPORTSIMPLE}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
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
  {$if CompilerVersion < 23}
    {$define CPUX86}
  {$else}
    {$define UNITSCOPENAMES}
  {$ifend}
  {$if CompilerVersion >= 21}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
  {$ifend}
  {$if (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
    {$define INTERNALCODEPAGE}
  {$ifend}
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}{$A4}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$if Defined(CPUX86) or Defined(CPUX64)}
  {$define CPUINTEL}
{$ifend}
{$if Defined(CPUX64) or Defined(CPUARM64)}
  {$define LARGEINT}
{$else}
  {$define SMALLINT}
{$ifend}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}
{$if Defined(FPC) or (CompilerVersion >= 18)}
  {$define OPERATORSUPPORT}
{$ifend}


interface
  uses {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif},
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif},{$endif}
       {$ifdef POSIX}Posix.String_, Posix.SysStat, Posix.Unistd,{$endif}
       {$ifdef KOL}
         KOL, err
       {$else}
         {$ifdef UNITSCOPENAMES}System.SysUtils{$else}SysUtils{$endif}
       {$endif};

type
  // standard types
  {$ifdef FPC}
    PUInt64 = ^UInt64;
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
  {$endif}
  TBytes = array of Byte;
  PBytes = ^TBytes;

  // exception class
  ECachedBuffer = class(Exception)
  {$ifdef KOL}
    constructor Create(const Msg: string);
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateRes(Ident: NativeUInt); overload;
    constructor CreateRes(ResStringRec: PResStringRec); overload;
    constructor CreateResFmt(Ident: NativeUInt; const Args: array of const); overload;
    constructor CreateResFmt(ResStringRec: PResStringRec; const Args: array of const); overload;
  {$endif}
  end;

{ TCachedBuffer abstract class }

  TCachedBufferKind = (cbReader, cbWriter);
  TCachedBuffer = class;
  TCachedBufferCallback = function(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt of object;
  TCachedBufferProgress = procedure(Sender: TCachedBuffer; var Cancel: Boolean) of object;

  TCachedBufferMemory = record
    Handle: Pointer;
    PreviousSize: NativeUInt;   
      Data: Pointer;
      Size: NativeUInt;
    Additional: Pointer;
    AdditionalSize: NativeUInt;  
  end;
  PCachedBufferMemory = ^TCachedBufferMemory;

  TCachedBuffer = class(TObject)
  protected
    FMemory: TCachedBufferMemory;
    FKind: TCachedBufferKind;
    FFinishing: Boolean;
    FEOF: Boolean;
    FLimited: Boolean;
    FPositionBase: Int64;
    FLimit: Int64;
    FStart: PByte;
    FOverflow: PByte;
    FHighWritten: PByte;
    FCallback: TCachedBufferCallback;
    FOnProgress: TCachedBufferProgress;

    function GetOptimalBufferSize(const Value, DefValue: NativeUInt; const ALimit: Int64 = 0): NativeUInt;
    function GetMargin: NativeInt; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    function GetPosition: Int64; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure SetEOF(const Value: Boolean);
    procedure SetLimit(const Value: Int64);
    function CheckLimit(const Value: Int64): Boolean; virtual;
    function DoWriterFlush: Boolean;
    function DoReaderFlush: Boolean;
    function DoProgress: Boolean;
  protected
    constructor Create(const Kind: TCachedBufferKind; const Callback: TCachedBufferCallback; const BufferSize: NativeUInt = 0);
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    Current: PByte;
    function Flush: NativeUInt;
    property Kind: TCachedBufferKind read FKind;
    property Overflow: PByte read FOverflow;
    property Margin: NativeInt read GetMargin;
    property EOF: Boolean read FEOF write SetEOF;
    property Limited: Boolean read FLimited;
    property Limit: Int64 read FLimit write SetLimit;
    property Memory: TCachedBufferMemory read FMemory;
    property Position: Int64 read GetPosition;
    property OnProgress: TCachedBufferProgress read FOnProgress write FOnProgress;
  end;

  TCachedWriter = class;

{ TCachedReader class }

  TCachedReader = class(TCachedBuffer)
  protected
    procedure OverflowRead(var Buffer; Size: NativeUInt);
    function DoDirectPreviousRead(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; virtual;
    function DoDirectFollowingRead(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; virtual;
    procedure OverflowSkip(Size: NativeUInt);
  public
    constructor Create(const Callback: TCachedBufferCallback; const BufferSize: NativeUInt = 0);
    procedure DirectRead(const Position: Int64; var Buffer; const Count: NativeUInt);
    property Finishing: Boolean read FFinishing;
    procedure Skip(const Count: NativeUInt); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure Export(const Writer: TCachedWriter; const Count: NativeUInt = 0); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}

    // TStream-like data reading
    procedure Read(var Buffer; const Count: NativeUInt);
    procedure ReadData(var Value: Boolean); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure ReadData(var Value: AnsiChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    procedure ReadData(var Value: WideChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: ShortInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Byte); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: SmallInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Word); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Integer); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Cardinal); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Int64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if Defined(FPC) or (CompilerVersion >= 16)}
    procedure ReadData(var Value: UInt64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure ReadData(var Value: Single); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Double); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: Extended); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if not Defined(FPC) and (CompilerVersion >= 23)}
    procedure ReadData(var Value: TExtended80Rec); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure ReadData(var Value: Currency); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: TPoint); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var Value: TRect); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
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

  TCachedWriter = class(TCachedBuffer)
  protected
    procedure OverflowWrite(const Buffer; Size: NativeUInt);
    function DoDirectPreviousWrite(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; virtual;
    function DoDirectFollowingWrite(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; virtual;
  public
    constructor Create(const Callback: TCachedBufferCallback; const BufferSize: NativeUInt = 0);
    procedure DirectWrite(const Position: Int64; const Buffer; const Count: NativeUInt);
    procedure Import(const Reader: TCachedReader; const Count: NativeUInt = 0);

    // TStream-like data writing
    procedure Write(const Buffer; const Count: NativeUInt);
    procedure WriteData(const Value: Boolean); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure WriteData(const Value: AnsiChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    procedure WriteData(const Value: WideChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: ShortInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Byte); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: SmallInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Word); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Integer); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Cardinal); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Int64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if Defined(FPC) or (CompilerVersion >= 16)}
    procedure WriteData(const Value: UInt64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure WriteData(const Value: Single); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Double); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Extended); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if not Defined(FPC) and (CompilerVersion >= 23)}
    procedure WriteData(const Value: TExtended80Rec); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure WriteData(const Value: Currency); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: TPoint); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: TRect); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifNdef NEXTGEN}
    procedure WriteData(const Value: ShortString); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: AnsiString); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: WideString); overload;
    {$endif}
    {$ifdef UNICODE}
    procedure WriteData(const Value: UnicodeString); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    procedure WriteData(const Value: TBytes); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const Value: Variant); overload;
  end;


{ TCachedReReader class }

  TCachedReReader = class(TCachedReader)
  protected
    FSource: TCachedReader;
    FOwner: Boolean;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Callback: TCachedBufferCallback; const Source: TCachedReader; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);

    property Source: TCachedReader read FSource;
    property Owner: Boolean read FOwner write FOwner;
  end;

{ TCachedReWriter class }

  TCachedReWriter = class(TCachedWriter)
  protected
    FTarget: TCachedWriter;
    FOwner: Boolean;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Callback: TCachedBufferCallback; const Target: TCachedWriter; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);

    property Target: TCachedWriter read FTarget;
    property Owner: Boolean read FOwner write FOwner;
  end;


{ TCachedFileReader class }

  TCachedFileReader = class(TCachedReader)
  protected
    FFileName: string;
    FHandle: THandle;
    FHandleOwner: Boolean;
    FOffset: Int64;

    procedure InternalCreate(const Size: Int64; const Seeked: Boolean);
    function CheckLimit(const Value: Int64): Boolean; override;
    function DoDirectPreviousRead(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; override;
    function DoDirectFollowingRead(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; override;
    function InternalCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const FileName: string; const Offset: Int64 = 0; const Size: Int64 = 0);
    constructor CreateHandled(const Handle: THandle; const Size: Int64 = 0; const HandleOwner: Boolean = False);

    property FileName: string read FFileName;
    property Handle: THandle read FHandle;
    property HandleOwner: Boolean read FHandleOwner write FHandleOwner;
    property Offset: Int64 read FOffset;
  end;

{ TCachedFileWriter class }

  TCachedFileWriter = class(TCachedWriter)
  protected
    FFileName: string;
    FHandle: THandle;
    FHandleOwner: Boolean;
    FOffset: Int64;

    function DoDirectPreviousWrite(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; override;
    function DoDirectFollowingWrite(Position: Int64; Data: PByte; Size: NativeUInt): Boolean; override;
    function InternalCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const FileName: string; const Size: Int64 = 0);
    constructor CreateHandled(const Handle: THandle; const Size: Int64 = 0; const HandleOwner: Boolean = False);

    property FileName: string read FFileName;
    property Handle: THandle read FHandle;
    property HandleOwner: Boolean read FHandleOwner write FHandleOwner;
    property Offset: Int64 read FOffset;
  end;


{ TCachedMemoryReader class }

  TCachedMemoryReader = class(TCachedReader)
  protected
    FPtr: Pointer;
    FSize: NativeUInt;
    FPtrMargin: NativeUInt;

    function CheckLimit(const Value: Int64): Boolean; override;
    function InternalCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
  public
    constructor Create(const Ptr: Pointer; const Size: NativeUInt);
    property Ptr: Pointer read FPtr;
    property Size: NativeUInt read FSize;
  end;

{ TCachedMemoryWriter class }

  TCachedMemoryWriter = class(TCachedWriter)
  protected
    FTemporary: Boolean;
    FPtr: Pointer;
    FSize: NativeUInt;
    FPtrMargin: NativeUInt;

    function CheckLimit(const Value: Int64): Boolean; override;
    function InternalCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
    function InternalTemporaryCallback(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create(const Ptr: Pointer; const Size: NativeUInt);
    constructor CreateTemporary;
    property Temporary: Boolean read FTemporary;
    property Ptr: Pointer read FPtr;
    property Size: NativeUInt read FSize;
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


// fast non-collision Move() realization        
procedure NcMove(const Source; var Dest; const Size: NativeUInt); {$ifdef CPUARM}inline;{$endif}

implementation


procedure RaisePointers;
begin
  raise ECachedBuffer.Create('Invalid current, overflow or buffer memory values');
end;

procedure RaiseEOF;
begin
  raise ECachedBuffer.Create('EOF buffer data modified');
end;

procedure RaiseReading;
begin
  raise ECachedBuffer.Create('Data reading error');
end;

procedure RaiseWriting;
begin
  raise ECachedBuffer.Create('Data writing error');
end;

procedure RaiseLimitValue(const Value: Int64);
begin
  raise ECachedBuffer.CreateFmt('Invalid limit value %d', [Value]);
end;

procedure RaiseVaraintType(const VType: Word);
begin
  raise ECachedBuffer.CreateFmt('Invalid variant type 0x%.4x', [VType]);
end;


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

type
  PStrData = ^TStrData;
  TStrData = record
    Ident: Integer;
    Str: string;
  end;

function EnumStringModules(Instance: NativeInt; Data: Pointer): Boolean;
var
  Buffer: array [0..1023] of Char;
begin
  with PStrData(Data)^ do
  begin
    SetString(Str, Buffer, Windows.LoadString(Instance, Ident, Buffer, sizeof(Buffer)));
    Result := Str = '';
  end;
end;

function FindStringResource(Ident: Integer): string;
var
  StrData: TStrData;
  Func: TEnumModuleFunc;
begin
  StrData.Ident := Ident;
  StrData.Str := '';
  Pointer(@Func) := @EnumStringModules;
  EnumResourceModules(Func, @StrData);
  Result := StrData.Str;
end;

function LoadStr(Ident: Integer): string;
begin
  Result := FindStringResource(Ident);
end;

constructor ECachedBuffer.CreateRes(Ident: NativeUInt);
begin
  inherited Create(e_Custom, LoadStr(Ident));
end;

constructor ECachedBuffer.CreateRes(ResStringRec: PResStringRec);
begin
  inherited Create(e_Custom, System.LoadResString(ResStringRec));
end;

constructor ECachedBuffer.CreateResFmt(Ident: NativeUInt;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, LoadStr(Ident), Args);
end;

constructor ECachedBuffer.CreateResFmt(ResStringRec: PResStringRec;
  const Args: array of const);
begin
  inherited CreateFmt(e_Custom, System.LoadResString(ResStringRec), Args);
end;
{$endif}


const
  MEMORY_PAGE_SIZE = 4 * 1024;
  DEFAULT_CACHED_SIZE = 64 * 1024;

function CachedBufferMemory(const PreviousSize, BufferSize: NativeUInt): TCachedBufferMemory;
var
  Offset: NativeUInt;
begin
  // detect sizes
  Result.PreviousSize := (PreviousSize + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE;
  Result.AdditionalSize := MEMORY_PAGE_SIZE;
  if (BufferSize = 0) then Result.Size := DEFAULT_CACHED_SIZE
  else
  Result.Size := (BufferSize + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE;

  // allocate
  GetMem(Result.Handle, Result.PreviousSize + Result.Size +
                        Result.AdditionalSize + MEMORY_PAGE_SIZE);

  // align
  Offset := NativeUInt(Result.Handle) and (MEMORY_PAGE_SIZE - 1);
  Inc(Result.PreviousSize, MEMORY_PAGE_SIZE - Offset);
  Inc(Result.AdditionalSize, Offset);
  Result.Data := Pointer(NativeUInt(Result.Handle) + Result.PreviousSize);
  Result.Additional := Pointer(NativeUInt(Result.Data) + Result.Size);
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
    P.X := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.GetFileSize(Handle, @P.Y);
    if (P.Y = -1) then P.X := -1;
    Result := PInt64(@P)^;
  {$endif}

  {$ifdef POSIX}
    if (fstat(Handle, S) = 0) then
      Result := S.st_size
    else
      Result := -1;
  {$endif}
end;

function DirectCachedFileMethod(const Instance: TCachedBuffer;
  const InstanceHandle: THandle; const InstanceOffset, Position: Int64;
  const Data: PByte; const Size: NativeUInt): Boolean;
var
  SeekValue: Int64;
  PositionValue: Int64;
begin
  SeekValue := FileSeek(InstanceHandle, Int64(0), 1{soFromCurrent});
  try
    PositionValue := Position + InstanceOffset;
    if (PositionValue <> FileSeek(InstanceHandle, PositionValue, 0{soFromBeginning})) then
    begin
      Result := False;
    end else
    begin
      Result := (Size = Instance.FCallback(Instance, Data, Size));
    end;
  finally
    FileSeek(InstanceHandle, SeekValue, 0{soFromBeginning});
  end;
end;


{ TCachedBuffer }

function TCachedBuffer.GetOptimalBufferSize(const Value, DefValue: NativeUInt;
  const ALimit: Int64): NativeUInt;
var
  Size: NativeUInt;
begin
  if (Value <> 0) then
  begin
    Result := Value;
    if (ALimit > 0) and (Result > ALimit) then Result := ALimit;
    Exit;
  end;

  if (ALimit <= 0) or (ALimit >= (DefValue * 4)) then
  begin
    Result := DefValue;
    Exit;
  end;

  Size := ALimit;
  Size := Size shr 2;
  if (Size = 0) then
  begin
    Result := MEMORY_PAGE_SIZE;
  end else
  begin
    Result := (Size + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE;
  end;
end;

constructor TCachedBuffer.Create(const Kind: TCachedBufferKind;
  const Callback: TCachedBufferCallback; const BufferSize: NativeUInt);
var
  Size: NativeUInt;
begin
  inherited Create;
  FKind := Kind;
  FCallback := Callback;
  if (not Assigned(FCallback)) then
    raise ECachedBuffer.Create('Flush callback not defined');

  Size := BufferSize;
  if (Size <> 0) and (Kind = cbWriter) and (Size <= MEMORY_PAGE_SIZE) then
    Inc(Size, MEMORY_PAGE_SIZE);
  FMemory := CachedBufferMemory(Ord(Kind = cbReader){4kb}, Size);
  FOverflow := FMemory.Additional;

  if (Kind = cbWriter) then
  begin
    Current := FMemory.Data;
    FHighWritten := Current;
  end else
  begin
    Current := FOverflow;
    FPositionBase := -Int64(FMemory.Size);
  end;
  FStart := Current;
end;

procedure TCachedBuffer.AfterConstruction;
begin
  inherited;
  DoProgress;
end;

procedure TCachedBuffer.BeforeDestruction;
begin
  if (Kind = cbWriter) and (not FEOF) and (Assigned(FCallback)) then
    Flush;

  inherited;
end;

destructor TCachedBuffer.Destroy;
begin
  if (FMemory.Handle <> nil) then
    FreeMem(FMemory.Handle);

  inherited;
end;

procedure TCachedBuffer.SetEOF(const Value{True}: Boolean);
begin
  if (FEOF = Value) then Exit;
  if (not Value) then
    raise ECachedBuffer.Create('Can''t turn off EOF flag');

  FEOF := True;
  FFinishing := False;
  FLimited := True;
  FLimit := Self.Position;
  FStart := Current;
  FOverflow := Current;
end;

function TCachedBuffer.DoProgress: Boolean;
var
  Cancel: Boolean;
begin
  if (Assigned(FOnProgress)) then
  begin
    Cancel := FEOF;
    FOnProgress(Self, Cancel);

    if (Cancel) then
      SetEOF(True);
  end;

  Result := (not FEOF);
end;

function TCachedBuffer.GetMargin: NativeInt;
var
  P: NativeInt;
begin
  // Result := NativeInt(FOverflow) - NativeInt(Current);
  P := NativeInt(Current);
  Result := NativeInt(FOverflow);
  Dec(Result, P);
end;

function TCachedBuffer.GetPosition: Int64;
begin
  Result := FPositionBase + (NativeInt(Current) - NativeInt(FMemory.Data));
end;

function TCachedBuffer.CheckLimit(const Value: Int64): Boolean;
begin
  Result := True;
end;

procedure TCachedBuffer.SetLimit(const Value: Int64);
var
  Position, MarginLimit: Int64;
  Margin: NativeInt;
begin
  if (FLimited) and (Value = FLimit) then Exit;

  // check limit value
  Position := Self.Position;
  if (FEOF) or (Value < 0) or (Position > Value) or
     ({IsReader and} FFinishing and (Value > (Position + Self.Margin))) or
     (not CheckLimit(Value)) then
    RaiseLimitValue(Value);

  // fill parameters
  FLimited := True;
  FLimit := Value;

  // detect margin limit is too small
  MarginLimit := Value - Position;
  Margin := Self.Margin;
  if (MarginLimit <= Margin) then
  begin
    // correct Margin to MarginLimit value
    Dec(FOverflow, Margin - NativeInt(MarginLimit));

    // Finishing & EOF
    if (Kind = cbReader) then
    begin
      FFinishing := True;

      if (Current = FOverflow) then
      begin
        SetEOF({EOF := }True);
        DoProgress;
      end;
    end;
  end;
end;

function TCachedBuffer.Flush: NativeUInt;
var
  Cur, Over, MemLow, MemHigh: NativeUInt;
  NewPositionBase: Int64;
  NewEOF: Boolean;
begin
  // out of range test
  Cur := NativeUInt(Current);
  Over := NativeUInt(FOverflow);
  MemLow := NativeUInt(FStart);
  MemHigh := NativeUInt(FMemory.Additional) + FMemory.AdditionalSize;
  if (MemLow <= $ffff) or (Cur <= $ffff) or (Over <= $ffff) or
     (Cur < MemLow) or (Cur >= MemHigh) or
     (Over < MemLow) or (Over >= MemHigh) then RaisePointers;

  // EOF
  if (FEOF) then
  begin
    if (Current <> FOverflow) then RaiseEOF;
    Result := 0;
    Exit;
  end;

  // valid data reading/writing
  if (Kind = cbWriter) then
  begin
    if (FLimited) and (FLimit < Self.Position) then
      RaiseWriting;
  end else
  begin
    if (Cur > Over) then
      RaiseReading;
  end;

  // idle flush
  if {IsReader and} (FFinishing) then
  begin
    Result := (Over - Cur);

    if (Result = 0) then
    begin
      SetEOF(True);
      DoProgress;
    end;

    Exit;
  end;

  // flush buffer
  NewPositionBase := Self.Position;
  if (Cur > Over) then NewPositionBase := NewPositionBase - (Cur - Over);
  NewEOF := False;
  if (Kind = cbWriter) then
  begin
    if (DoWriterFlush) then
      NewEOF := True;
  end else
  begin
    if (DoReaderFlush) then
    begin
      FFinishing := True;
      if (Current = FOverflow{Margin = 0}) then NewEOF := True;
    end;
  end;
  FPositionBase := NewPositionBase;
  if (NewEOF) then SetEOF(True);  
  DoProgress;

  // Result
  Result := Self.Margin;
end;

function TCachedBuffer.DoWriterFlush: Boolean;
var
  FlushSize, R: NativeUInt;
  OverflowSize: NativeUInt;
  Margin: NativeInt;
  MarginLimit: Int64;
begin
  // Current correction
  if (NativeUInt(FHighWritten) > NativeUInt(Current)) then
    Current := FHighWritten;

  // flush size
  FlushSize := NativeUInt(Current) - NativeUInt(FMemory.Data);
  Result := (FlushSize < FMemory.Size);
  OverflowSize := 0;
  if (FlushSize > FMemory.Size) then
  begin
    OverflowSize := FlushSize - FMemory.Size;
    FlushSize := FMemory.Size;
  end;

  // detect margin limit
  MarginLimit := High(Int64);
  if (FLimited) then MarginLimit := FLimit - Position;

  // run callback
  if (FlushSize <> 0) then
  begin
    R := FCallback(Self, FMemory.Data, FlushSize);
    if (R <> FlushSize) then RaiseWriting;
  end;

  // current
  Current := FMemory.Data;
  if (OverflowSize <> 0) then
  begin
    NcMove(FOverflow^, Current^, OverflowSize);
    Inc(Current, OverflowSize);
  end;
  FHighWritten := Current;

  // overflow correction
  if (FLimited) then
  begin
    Margin := Self.Margin;
    if (MarginLimit < Margin) then
      Dec(FOverflow, Margin - NativeInt(MarginLimit));
  end;
end;

function TCachedBuffer.DoReaderFlush: Boolean;
var
  Margin: NativeUInt;
  Handle: Pointer;
  Temp: TCachedBufferMemory;
  MarginLimit: Int64;
  FlushSize, R: NativeUInt;
begin
  Margin := Self.Margin;

  // move margin data to previous memory
  if (Margin > 0) then
  begin
    if (Margin > FMemory.PreviousSize) then
    begin
      Temp := CachedBufferMemory(Margin, FMemory.Size);
      Handle := FMemory.Handle;
      try
        NcMove(Current^, Pointer(NativeUInt(Temp.Data) - Margin)^, Margin);
        FMemory := Temp;
      finally
        FreeMem(Handle);
      end;
    end else
    begin
      NcMove(Current^, Pointer(NativeUInt(FMemory.Data) - Margin)^, Margin);
    end;
  end;

  // flush size
  FlushSize := FMemory.Size;
  Result := False;
  if (FLimited) then
  begin
    MarginLimit := FLimit - Position;
    if (MarginLimit <= FlushSize) then
    begin
      FlushSize := MarginLimit;
      Result := True;
    end;
  end;

  // run callback
  if (FlushSize = 0) then
  begin
    R := FlushSize{0};
  end else
  begin
    R := FCallback(Self, FMemory.Data, FlushSize);
    if (R > FlushSize) then RaiseReading;
    if (R < FlushSize) then Result := True;
  end;

  // current/overflow
  FStart := Pointer(NativeUInt(FMemory.Data) - Margin);
  Current := FStart;
  FOverflow := Pointer(NativeUInt(FMemory.Data) + R);
end;


{$ifdef CPUX86}
var
  SSE_SUPPORT: Boolean;

procedure NcMoveInternal(const Source; var Dest; const Size: NativeUInt); forward;
{$endif}

procedure TCachedWriter.Write(const Buffer; const Count: NativeUInt);
{$ifNdef CPUINTEL}
var
  P: PByte;
begin
  P := Current;
  Inc(P, Count);

  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowWrite(Buffer, Count);
  end else
  begin
    Current := P;
    Dec(P, Count);
    NcMove(Buffer, P^, Count);
  end;
end;
{$else .CPUX86 or .CPUX64}
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
    movzx ebx, byte ptr [SSE_SUPPORT]
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
  jmp OverflowWrite
end;
{$endif}

procedure TCachedReader.Read(var Buffer; const Count: NativeUInt);
{$ifNdef CPUINTEL}
var
  P: PByte;
begin
  P := Current;
  Inc(P, Count);

  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Buffer, Count);
  end else
  begin
    Current := P;
    Dec(P, Count);
    NcMove(P^, Buffer, Count);
  end;
end;
{$else .CPUX86 or .CPUX64}
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
    movzx ebx, byte ptr [SSE_SUPPORT]
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
  jmp OverflowRead
end;
{$endif}


{$ifNdef CPUINTEL}
// System memcpy recall
procedure NcMove(const Source; var Dest; const Size: NativeUInt);
begin
  memcpy(Dest, Source, Size);
end;
{$else .CPUX86 or .CPUX64}
// SSE-based non-collision Move() realization
procedure NcMove(const Source; var Dest; const Size: NativeUInt);
{$ifdef CPUX86}
asm
  push ebx
  movzx ebx, byte ptr [SSE_SUPPORT]
  jmp NcMoveInternal
end;
procedure NcMoveInternal(const Source; var Dest; const Size: NativeUInt);
{$endif}
asm
  // basic routine
  {$ifdef CPUX86}
    test ebx, ebx
    jz @x86_non_SSE

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
  @x86_non_SSE:
    // non-SSE 0..15
    cmp ecx, 4
    jb @move_03
    cmp ecx, 16
    jb @move_015
    // non-SSE dwords
    mov ebx, ecx
    shr ecx, 2
    xchg esi, eax
    xchg edi, edx
    and ebx, 3
    rep movsd
    // non-SSE last 0..3
    xchg esi, eax
    xchg edi, edx
    jmp [offset @move_03_items + ebx*4]
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

constructor TCachedReader.Create(const Callback: TCachedBufferCallback;
  const BufferSize: NativeUInt);
begin
  inherited Create(cbReader, Callback, BufferSize);
end;

procedure TCachedReader.DirectRead(const Position: Int64; var Buffer;
  const Count: NativeUInt);
var
  Done: Boolean;
  PositionHigh: Int64;
  CachedLow, CachedHigh: Int64;
  CachedOffset, BufferOffset, Size: NativeUInt;
  {$ifdef SMALLINT}
    Size64: Int64;
  {$endif}
begin
  if (Count = 0) then Exit;

  PositionHigh := Position + Count;
  Done := (not FEOF) and (Position >= 0) and ((not Limited) or (Limit >= PositionHigh));
  if (Done) then
  begin
    CachedLow := FPositionBase - (NativeInt(FMemory.Data) - NativeInt(FStart));
    CachedHigh := FPositionBase + (NativeInt(FOverflow) - NativeInt(FMemory.Data));

    // cached data copy
    CachedOffset := 0;
    BufferOffset := 0;
    Size := 0;
    if (Position >= CachedLow) and (Position < CachedHigh) then
    begin
      CachedOffset := (Position - CachedLow);
      Size := (CachedHigh - Position);
    end else
    if (PositionHigh > CachedLow) and (Position < CachedHigh) then
    begin
      BufferOffset := (CachedLow - Position);
      Size := (PositionHigh - CachedLow);
    end;
    if (Size <> 0) then
    begin
      if (Size > Count) then Size := Count;
      NcMove(Pointer(NativeUInt(FStart) + CachedOffset)^,
             Pointer(NativeUInt(@Buffer) + BufferOffset)^,
             Size);
    end;

    // before cached
    if (Position < CachedLow) then
    begin
      {$ifdef LARGEINT}
        Size := (CachedLow - Position);
      {$else .SMALLINT}
        Size64 := (CachedLow - Position);
        Size := Size64;
        if (Size <> Size64) then Size := Count;
      {$endif}
      if (Size > Count) then Size := Count;
      Done := DoDirectPreviousRead(Position, @Buffer, Size);
    end;

    // after cached
    if (Done) and (PositionHigh > CachedHigh) then
    begin
      {$ifdef LARGEINT}
        Size := (PositionHigh - CachedHigh);
      {$else .SMALLINT}
        Size64 := (PositionHigh - CachedHigh);
        Size := Size64;
        if (Size <> Size64) then Size := Count;
      {$endif}
      if (Size > Count) then Size := Count;
      Done := DoDirectFollowingRead(PositionHigh - Size,
                                    Pointer(NativeUInt(@Buffer) + (Count - Size)),
                                    Size);
    end;
  end;

  if (not Done) then
    raise ECachedBuffer.Create('Direct read failure');
end;

function TCachedReader.DoDirectPreviousRead(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
begin
  Result := False;
end;

function TCachedReader.DoDirectFollowingRead(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
var
  PreviousSize, FilledSize, AllocatingSize: NativeUInt;
  {$ifdef SMALLINT}
    AllocatingSize64: Int64;
  {$endif}
  Temp: TCachedBufferMemory;
  Handle: Pointer;
  NewStart: Pointer;
  FlushSize, R: NativeUInt;
  MarginLimit: Int64;
begin
  Result := False;
  if (NativeUInt(FStart) > NativeUInt(FMemory.Data))  or
    (NativeUInt(Current) < NativeUInt(FStart)) then Exit;
  PreviousSize := NativeUInt(FMemory.Data) - NativeUInt(FStart);
  FilledSize := NativeUInt(FOverflow) - NativeUInt(FMemory.Data);

  {$ifdef LARGEINT}
    AllocatingSize := (Position - Self.FPositionBase) + Size;
  {$else .SMALLINT}
    AllocatingSize64 := (Position - Self.FPositionBase) + Size;
    AllocatingSize := AllocatingSize64;
    if (AllocatingSize <> AllocatingSize64) then Exit;
  {$endif}

  // allocate
  try
    Temp := CachedBufferMemory(PreviousSize or {4kb minimum}1, AllocatingSize);
    Result := True;
  except
  end;
  if (not Result) then Exit;

  // copy data, change pointers
  Handle := Temp.Handle;
  try
    NewStart := Pointer(NativeUInt(Temp.Data) - PreviousSize);
    NcMove(FStart^, NewStart^, PreviousSize + FilledSize);

    FStart := NewStart;
    Current := Pointer(NativeInt(Current) - NativeInt(FMemory.Data) + NativeInt(Temp.Data));
    FOverflow := Pointer(NativeUInt(Temp.Data) + FilledSize);

    Handle := FMemory.Handle;
    FMemory := Temp;
  finally
    FreeMem(Handle);
  end;

  // fill buffer
  FlushSize := NativeUInt(FMemory.Additional) - NativeUInt(FOverflow);
  if (FLimited) then
  begin
    MarginLimit := FLimit - (Self.FPositionBase + FilledSize);
    if (MarginLimit <= FlushSize) then
    begin
      FlushSize := MarginLimit;
      FFinishing := True;
    end;
  end;
  R := FCallback(Self, FOverflow, FlushSize);
  if (R > FlushSize) then RaiseReading;
  if (R < FlushSize) then FFinishing := True;
  Inc(FOverflow, R);
  if (Position + Size > Self.FPositionBase + (NativeUInt(FOverflow) - NativeUInt(Memory.Data))) then
  begin
    Result := False;
    Exit;
  end;

  // data read
  NcMove(Pointer(NativeUInt(Position - Self.FPositionBase) + NativeUInt(FMemory.Data))^,
    Data^, Size);
end;

// Margin < Size
procedure TCachedReader.OverflowRead(var Buffer; Size: NativeUInt);
var
  Data: PByte;
  S: NativeUInt;
  Margin: NativeUInt;
begin
  Data := Pointer(@Buffer);

  // last time failure reading
  if (NativeUInt(Current) > NativeUInt(FOverflow)) then
    RaiseReading;

  // limit test
  if (FLimited) and (Self.Position + Size > FLimit) then
    RaiseReading;

  // read Margin
  if (Current <> FOverflow) then
  begin
    Margin := Self.Margin;

    NcMove(Current^, Data^, Margin);
    Inc(Data, Margin);
    Inc(Current, Margin);
    Dec(Size, Margin);
  end;

  // if read data is too large, we can read data directly
  if (Size >= FMemory.Size) then
  begin
    S := Size - (Size mod FMemory.Size);
    Dec(Size, S);

    if (Assigned(FOnProgress)) then
    begin
      while (S <> 0) do
      begin
        if (FMemory.Size <> FCallback(Self, Data, FMemory.Size)) then
          RaiseReading;

        Dec(S, FMemory.Size);
        Inc(Data, FMemory.Size);
        Inc(FPositionBase, FMemory.Size);

        DoProgress;
        if (FEOF) and ((S <> 0) or (Size <> 0)) then
          RaiseReading;
      end;
    end else
    begin
      if (S <> FCallback(Self, Data, S)) then
        RaiseReading;

      Inc(Data, S);
      Inc(FPositionBase, S);
    end;
  end;

  // last Data bytes
  if (Size <> 0) then
  begin
    Flush;
    if (NativeUInt(Self.Margin) < Size) then RaiseReading;

    NcMove(Current^, Data^, Size);
    Inc(Current, Size);
  end;
end;

procedure TCachedReader.ReadData(var Value: Boolean);
var
  P: ^Boolean;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

{$if Defined(FPC) or (CompilerVersion >= 16)}
procedure TCachedReader.ReadData(var Value: UInt64);
var
  P: ^UInt64;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

procedure TCachedReader.ReadData(var Value: Extended);
var
  P: ^Extended;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;

{$if not Defined(FPC) and (CompilerVersion >= 23)}
procedure TCachedReader.ReadData(var Value: TExtended80Rec);
var
  P: ^TExtended80Rec;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    Value := P^;
  end;
end;
{$ifend}

procedure TCachedReader.ReadData(var Value: Currency);
var
  P: ^Currency;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(Value, SizeOf(Value));
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
    if (Flush < SizeOf(Byte)) then RaiseReading;
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

procedure TCachedReader.Skip(const Count: NativeUInt);
var
  P: PByte;
begin
  P := Current;
  Inc(P, Count);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowSkip(Count);
  end else
  begin
    Current := P;
  end;
end;

procedure TCachedReader.OverflowSkip(Size: NativeUInt);
var
  Done: Boolean;
  S: NativeUInt;
begin
  Done := False;
  if (not FEOF) and ((not FLimited) or (FLimit >= (Self.Position + Size))) then
  begin
    repeat
      S := NativeUInt(FOverflow) - NativeUInt(Current);
      if (S > Size) then S := Size;
      Current := FOverflow;
      Dec(Size, S);
      if (Size <> 0) then Flush;      
    until (FEOF) or (Size = 0);

    Done := (Size = 0);
  end;

  if (not Done) then
    raise ECachedBuffer.Create('Cached reader skip failure');
end;

procedure TCachedReader.Export(const Writer: TCachedWriter;
  const Count: NativeUInt);
begin
  Writer.Import(Self, Count);
end;

{ TCachedWriter }

constructor TCachedWriter.Create(const Callback: TCachedBufferCallback;
  const BufferSize: NativeUInt);
begin
  inherited Create(cbWriter, Callback, BufferSize);
end;

procedure TCachedWriter.DirectWrite(const Position: Int64; const Buffer;
  const Count: NativeUInt);
var
  Done: Boolean;
  PositionHigh: Int64;
  CachedLow, CachedHigh: Int64;
  CachedOffset, BufferOffset, Size: NativeUInt;
  {$ifdef SMALLINT}
    Size64: Int64;
  {$endif}
  HighWritten: NativeUInt;
begin
  if (Count = 0) then Exit;

  PositionHigh := Position + Count;
  Done := (not FEOF) and (Position >= 0) and ((not Limited) or (Limit >= PositionHigh));
  if (Done) then
  begin
    CachedLow := FPositionBase - (NativeInt(FMemory.Data) - NativeInt(FStart));
    CachedHigh := FPositionBase + (NativeInt(FOverflow) - NativeInt(FMemory.Data));

    // cached data copy
    CachedOffset := 0;
    BufferOffset := 0;
    Size := 0;
    if (Position >= CachedLow) and (Position < CachedHigh) then
    begin
      CachedOffset := (Position - CachedLow);
      Size := (CachedHigh - Position);
    end else
    if (PositionHigh > CachedLow) and (Position < CachedHigh) then
    begin
      BufferOffset := (CachedLow - Position);
      Size := (PositionHigh - CachedLow);
    end;
    if (Size <> 0) then
    begin
      if (Size > Count) then Size := Count;
      NcMove(Pointer(NativeUInt(@Buffer) + BufferOffset)^,
             Pointer(NativeUInt(FStart) + CachedOffset)^,
             Size);

      HighWritten := NativeUInt(FStart) + CachedOffset + Size;
      if (HighWritten > NativeUInt(Self.FHighWritten)) then Self.FHighWritten := Pointer(HighWritten);
    end;

    // before cached
    if (Position < CachedLow) then
    begin
      {$ifdef LARGEINT}
        Size := (CachedLow - Position);
      {$else .SMALLINT}
        Size64 := (CachedLow - Position);
        Size := Size64;
        if (Size <> Size64) then Size := Count;
      {$endif}
      if (Size > Count) then Size := Count;
      Done := DoDirectPreviousWrite(Position, @Buffer, Size);
    end;

    // after cached
    if (Done) and (PositionHigh > CachedHigh) then
    begin
      {$ifdef LARGEINT}
        Size := (PositionHigh - CachedHigh);
      {$else .SMALLINT}
        Size64 := (PositionHigh - CachedHigh);
        Size := Size64;
        if (Size <> Size64) then Size := Count;
      {$endif}
      if (Size > Count) then Size := Count;
      Done := DoDirectFollowingWrite(PositionHigh - Size,
                                     Pointer(NativeUInt(@Buffer) + (Count - Size)),
                                     Size);
    end;
  end;

  if (not Done) then
    raise ECachedBuffer.Create('Direct write failure');
end;

function TCachedWriter.DoDirectPreviousWrite(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
begin
  Result := False;
end;

function TCachedWriter.DoDirectFollowingWrite(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
var
  PreviousSize, FilledSize, AllocatingSize: NativeUInt;
  {$ifdef SMALLINT}
    AllocatingSize64: Int64;
  {$endif}
  Temp: TCachedBufferMemory;
  Handle: Pointer;
begin
  Result := False;
  if (NativeUInt(Current) < NativeUInt(FMemory.Data)) then
  begin
    PreviousSize := NativeUInt(FMemory.Data) - NativeUInt(Current);
    FilledSize := 0;
  end else
  begin
    PreviousSize := 0;
    FilledSize := NativeUInt(Current) - NativeUInt(FMemory.Data);
  end;

  {$ifdef LARGEINT}
    AllocatingSize := (Position - Self.FPositionBase) + Size;
  {$else .SMALLINT}
    AllocatingSize64 := (Position - Self.FPositionBase) + Size;
    AllocatingSize := AllocatingSize64;
    if (AllocatingSize <> AllocatingSize64) then Exit;
  {$endif}

  // allocate
  try
    Temp := CachedBufferMemory(PreviousSize{default = 0}, AllocatingSize);
    Result := True;
  except
  end;
  if (not Result) then Exit;

  // copy data, change pointers
  Handle := Temp.Handle;
  try
    NcMove(Pointer(NativeUInt(FMemory.Data) - PreviousSize)^,
      Pointer(NativeUInt(Temp.Data) - PreviousSize)^, PreviousSize + FilledSize);

    Current := Pointer(NativeInt(Current) - NativeInt(FMemory.Data) + NativeInt(Temp.Data));
    FOverflow := Temp.Additional;

    Handle := FMemory.Handle;
    FMemory := Temp;
  finally
    FreeMem(Handle);
  end;

  // data write
  FHighWritten := Pointer(NativeUInt(Position - Self.FPositionBase) + NativeUInt(FMemory.Data));
  NcMove(Data^, Pointer(NativeUInt(FHighWritten) - Size)^, Size);
end;

// Margin < Size
procedure TCachedWriter.OverflowWrite(const Buffer; Size: NativeUInt);
var
  Data: PByte;
  S: NativeUInt;
  Margin: NativeInt;
begin
  Data := Pointer(@Buffer);

  // limit test
  if (FLimited) and (Self.Position + Size > FLimit) then
    RaiseWriting;

  // write margin to buffer if used
  if (Current <> FMemory.Data) then
  begin
    if (NativeUInt(Current) < NativeUInt(FOverflow)) then
    begin
      Margin := Self.Margin;
      NcMove(Data^, Current^, Margin);

      Current := Self.Overflow;
      Inc(Data, Margin);
      Dec(Size, Margin);
    end;

    Flush();
  end;

  // if written data is too large, we can write data directly
  if (Size >= FMemory.Size) then
  begin
    S := Size - (Size mod FMemory.Size);
    Dec(Size, S);

    if (Assigned(FOnProgress)) then
    begin
      while (S <> 0) do
      begin
        if (FMemory.Size <> FCallback(Self, Data, FMemory.Size)) then
          RaiseWriting;

        Dec(S, FMemory.Size);
        Inc(Data, FMemory.Size);
        Inc(FPositionBase, FMemory.Size);

        DoProgress;
        if (FEOF) and ((S <> 0) or (Size <> 0)) then
          RaiseWriting;
      end;
    end else
    begin
      if (S <> FCallback(Self, Data, S)) then
        RaiseWriting;

      Inc(Data, S);
      Inc(FPositionBase, S);
    end;
  end;

  // last Data bytes
  if (Size <> 0) then
  begin
    NcMove(Data^, Current^, Size);
    Inc(Current, Size);
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

{$if Defined(FPC) or (CompilerVersion >= 16)}
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

procedure TCachedWriter.WriteData(const Value: Extended);
var
  P: ^Extended;
begin
  P := Pointer(Current);
  P^ := Value;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$if not Defined(FPC) and (CompilerVersion >= 23)}
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
{$ifend}

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

procedure TCachedWriter.Import(const Reader: TCachedReader;
  const Count: NativeUInt);
var
  Size, ReadLimit: NativeUInt;
begin
  ReadLimit := Count;
  if (Count = 0) then ReadLimit := High(NativeUInt);

  if (Reader <> nil) then
  while (not Reader.EOF) and (ReadLimit <> 0) do
  begin
    if (NativeUInt(Reader.Current) > NativeUInt(Reader.Overflow)) then Break;
    Size := NativeUInt(Reader.Overflow) - NativeUInt(Reader.Current);
    if (Size > ReadLimit) then Size := ReadLimit;

    Self.Write(Reader.Current^, Size);
    Inc(Reader.Current, Size);
    Dec(ReadLimit, Size);

    if (ReadLimit <> 0) then
      Reader.Flush;
  end;

  if (ReadLimit <> 0) and (Count <> 0) then
    raise ECachedBuffer.Create('Cached writer import failure');
end;


{ TCachedReReader }

constructor TCachedReReader.Create(const Callback: TCachedBufferCallback;
  const Source: TCachedReader; const Owner: Boolean;
  const BufferSize: NativeUInt);
begin
  FSource := Source;
  FOwner := Owner;
  inherited Create(Callback, GetOptimalBufferSize(BufferSize, DEFAULT_CACHED_SIZE, Source.Limit));
end;

destructor TCachedReReader.Destroy;
begin
  if (FOwner) then FSource.Free;
  inherited;
end;


{ TCachedReWriter }


constructor TCachedReWriter.Create(const Callback: TCachedBufferCallback;
  const Target: TCachedWriter; const Owner: Boolean;
  const BufferSize: NativeUInt);
begin
  FTarget := Target;
  FOwner := Owner;
  inherited Create(Callback, GetOptimalBufferSize(BufferSize, DEFAULT_CACHED_SIZE, Target.Limit));
end;

destructor TCachedReWriter.Destroy;
begin
  if (FOwner) then FTarget.Free;
  inherited;
end;


{ TCachedFileReader }

constructor TCachedFileReader.Create(const FileName: string; const Offset,
  Size: Int64);
begin
  FFileName := FileName;
  FHandleOwner := True;
  FOffset := Offset;
  {$ifdef MSWINDOWS}
  FHandle := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.CreateFile(PChar(FileName), $0001{FILE_READ_DATA}, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  {$else}
  FHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  {$endif}
  if (FHandle = INVALID_HANDLE_VALUE) then
    raise ECachedBuffer.CreateFmt('Cannot open file:'#13'%s', [FileName]);

  InternalCreate(Size, (Offset = 0));
end;

constructor TCachedFileReader.CreateHandled(const Handle: THandle;
  const Size: Int64; const HandleOwner: Boolean);
begin
  FHandle := Handle;
  FHandleOwner := HandleOwner;
  FOffset := FileSeek(FHandle, Int64(0), 1{soFromCurrent});
  if (FHandle = INVALID_HANDLE_VALUE) or (FOffset < 0) then
    raise ECachedBuffer.Create('Invalid file handle');

  InternalCreate(Size, True);
end;

procedure TCachedFileReader.InternalCreate(const Size: Int64; const Seeked: Boolean);
var
  FileSize: Int64;
begin
  FileSize := GetFileSize(FHandle);

  if (FOffset < 0) or (FOffset > FileSize) or
    ((not Seeked) and (FOffset <> FileSeek(FHandle, FOffset, 0{soFromBeginning}))) then
    raise ECachedBuffer.CreateFmt('Invalid offset %d in %d bytes file'#13'%s',
      [FOffset, FileSize, FFileName]);

  FileSize := FileSize - Offset;
  if (FileSize = 0) then
  begin
    FKind := cbReader; // inherited Create;
    EOF := True;
  end else
  begin
    if (Size > 0) and (Size < FileSize) then FileSize := Size;

    inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, FileSize));
    Limit := FileSize;
  end;
end;

destructor TCachedFileReader.Destroy;
begin
  inherited;

  if (FHandleOwner) and (FHandle <> 0) and
    (FHandle <> INVALID_HANDLE_VALUE) then FileClose(FHandle);
end;

function TCachedFileReader.CheckLimit(const Value: Int64): Boolean;
begin
  Result := (Value <= (GetFileSize(FHandle) - FOffset));
end;

function TCachedFileReader.InternalCallback(Sender: TCachedBuffer; Data: PByte;
  Size: NativeUInt): NativeUInt;
var
  Count, ReadingSize: Integer;
begin
  Result := 0;

  repeat
    if (Size > NativeUInt(High(Integer))) then ReadingSize := High(Integer)
    else ReadingSize := Size;

    Count := FileRead(FHandle, Data^, ReadingSize);
    if (Count < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};

    Inc(Data, Count);
    Dec(Size, Count);
    Inc(Result, Count);
  until (Count <> ReadingSize) or (Size = 0);
end;

function TCachedFileReader.DoDirectPreviousRead(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, Position, Data, Size);
end;

function TCachedFileReader.DoDirectFollowingRead(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, Position, Data, Size);
end;


{ TCachedFileWriter }

constructor TCachedFileWriter.Create(const FileName: string; const Size: Int64);
var
  Handle: THandle;
begin
  FFileName := FileName;
  {$ifdef MSWINDOWS}
  Handle := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.CreateFile(PChar(FileName), $0002{FILE_WRITE_DATA}, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0);
  {$else}
  Handle := FileCreate(FileName);
  {$endif}
  if (Handle = INVALID_HANDLE_VALUE) then
    raise ECachedBuffer.CreateFmt('Cannot create file:'#13'%s', [FileName]);

  CreateHandled(Handle, Size, True);
end;

constructor TCachedFileWriter.CreateHandled(const Handle: THandle;
  const Size: Int64; const HandleOwner: Boolean);
begin
  FHandle := Handle;
  FHandleOwner := HandleOwner;
  FOffset := FileSeek(FHandle, Int64(0), 1{soFromCurrent});
  if (FHandle = INVALID_HANDLE_VALUE) or (FOffset < 0) then
    raise ECachedBuffer.Create('Invalid file handle');

  inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, Size));
  if (Size > 0) then
    Limit := Size;
end;

destructor TCachedFileWriter.Destroy;
begin
  inherited;

  if (FHandleOwner) and (FHandle <> 0) and
    (FHandle <> INVALID_HANDLE_VALUE) then FileClose(FHandle);
end;

function TCachedFileWriter.InternalCallback(Sender: TCachedBuffer; Data: PByte;
  Size: NativeUInt): NativeUInt;
var
  Count, WritingSize: Integer;
begin
  Result := 0;

  repeat
    if (Size > NativeUInt(High(Integer))) then WritingSize := High(Integer)
    else WritingSize := Size;

    Count := FileWrite(FHandle, Data^, WritingSize);
    if (Count < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};

    Inc(Data, Count);
    Dec(Size, Count);
    Inc(Result, Count);
  until (Count <> WritingSize) or (Size = 0);
end;

function TCachedFileWriter.DoDirectPreviousWrite(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, Position, Data, Size);
end;

function TCachedFileWriter.DoDirectFollowingWrite(Position: Int64; Data: PByte;
  Size: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, Position, Data, Size);
end;


{ TCachedMemoryReader }

constructor TCachedMemoryReader.Create(const Ptr: Pointer;
  const Size: NativeUInt);
begin
  FPtr := Ptr;
  FSize := Size;
  FPtrMargin := Size;

  if (Ptr = nil) or (Size = 0) then
  begin
    FKind := cbReader; // inherited Create;
    EOF := True;
  end else
  begin
    inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, Size));
    Limit := Size;
  end;
end;

function TCachedMemoryReader.CheckLimit(const Value: Int64): Boolean;
begin
  Result := (Value <= Size);
end;

function TCachedMemoryReader.InternalCallback(Sender: TCachedBuffer; Data: PByte;
  Size: NativeUInt): NativeUInt;
begin
  Result := Size;
  if (Result > FPtrMargin) then Result := FPtrMargin;

  NcMove(Pointer(NativeUInt(FPtr) + Self.FSize - FPtrMargin)^, Data^, Result);
  Dec(FPtrMargin, Result);
end;


{ TCachedMemoryWriter }

constructor TCachedMemoryWriter.Create(const Ptr: Pointer;
  const Size: NativeUInt);
begin
  FPtr := Ptr;
  FSize := Size;
  FPtrMargin := Size;

  if (Ptr = nil) or (Size = 0) then
  begin
    FKind := cbWriter; // inherited Create;
    EOF := True;
  end else
  begin
    inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, Size));
    Limit := Size;
  end;
end;

constructor TCachedMemoryWriter.CreateTemporary;
begin
  inherited Create(InternalTemporaryCallback);
end;

destructor TCachedMemoryWriter.Destroy;
begin
  inherited;
  if (FTemporary) then FreeMem(FPtr);
end;

function TCachedMemoryWriter.CheckLimit(const Value: Int64): Boolean;
begin
  if (not FTemporary) then
  begin
    Result := (Value <= Size);
  end else
  begin
    Result := True;
  end;
end;

function TCachedMemoryWriter.InternalCallback(Sender: TCachedBuffer;
  Data: PByte; Size: NativeUInt): NativeUInt;
begin
  Result := Size;
  if (Result > FPtrMargin) then Result := FPtrMargin;

  NcMove(Data^, Pointer(NativeUInt(FPtr) + Self.FSize - FPtrMargin)^, Result);
  Dec(FPtrMargin, Result);
end;

function TCachedMemoryWriter.InternalTemporaryCallback(Sender: TCachedBuffer;
  Data: PByte; Size: NativeUInt): NativeUInt;
var
  NewPtrSize: NativeUInt;
begin
  if (Size <> 0) then
  begin
    NewPtrSize := Self.FSize + Size;
    Self.FSize := NewPtrSize;
    ReallocMem(FPtr, NewPtrSize);

    NcMove(Data^, Pointer(NativeUInt(FPtr) + NewPtrSize - Size)^, Size);
  end;

  Result := Size;
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
    if (V <= High(Word)) then N := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else N := '"' + string(Name) + '"';

    V := NativeUInt(ResType);
    if (V <= High(Word)) then T := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else T := '"' + string(ResType) + '"';

    raise ECachedBuffer.CreateFmt('Resource %s (%s) not found', [N, T]);
  end;

var
  HResInfo: THandle;
begin
  HResInfo := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.FindResource(Instance, Name, ResType);
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


{$ifdef CPUX86}
procedure CheckSSESupport;
asm
  push ebx
  mov eax, 1
  cpuid
  test edx, 02000000h
  setnz [SSE_SUPPORT]
  pop ebx
end;

initialization
  CheckSSESupport;
{$endif}

end.

