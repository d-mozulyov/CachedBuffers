unit CachedBuffers;

{******************************************************************************}
{ Copyright (c) Dmitry Mozulyov                                                }
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
  {$define STATICSUPPORT}
  {$define ANSISTRSUPPORT}
  {$define SHORTSTRSUPPORT}
  {$define WIDESTRSUPPORT}
  {$ifdef MSWINDOWS}
    {$define WIDESTRLENSHIFT}
  {$endif}
  {$define INTERNALCODEPAGE}
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
    {$WARN SYMBOL_DEPRECATED OFF}
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
  {$if CompilerVersion >= 18.5}
    {$define STATICSUPPORT}
  {$ifend}
  {$if CompilerVersion < 23}
    {$define CPUX86}
  {$ifend}
  {$if CompilerVersion >= 23}
    {$define UNITSCOPENAMES}
    {$define RETURNADDRESS}
    {$define SYSARRAYSUPPORT}
  {$ifend}
  {$if CompilerVersion >= 21}
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
    {$define EXTENDEDRTTI}
  {$ifend}
  {$if CompilerVersion >= 33}
    {$define MANAGEDRECORDS}
  {$ifend}
  {$if (not Defined(NEXTGEN)) or (CompilerVersion >= 31)}
    {$define ANSISTRSUPPORT}
  {$ifend}
  {$ifNdef NEXTGEN}
    {$define SHORTSTRSUPPORT}
  {$endif}
  {$if Defined(MSWINDOWS) or (Defined(MACOS) and not Defined(IOS))}
    {$define WIDESTRSUPPORT}
  {$ifend}
  {$if Defined(MSWINDOWS) or (Defined(WIDESTRSUPPORT) and (CompilerVersion <= 21))}
    {$define WIDESTRLENSHIFT}
  {$ifend}
  {$if Defined(ANSISTRSUPPORT) and (CompilerVersion >= 20)}
    {$define INTERNALCODEPAGE}
  {$ifend}
  {$if Defined(NEXTGEN)}
    {$POINTERMATH ON}
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
{$if Defined(CPUINTEL) and Defined(POSIX)}
  {$ifdef CPUX86}
    {$define POSIXINTEL32}
  {$else}
    {$define POSIXINTEL64}
  {$endif}
{$ifend}
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
       {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif},{$endif}
       {$ifdef POSIX}
         {$ifdef FPC}
           BaseUnix,
         {$else}
           Posix.String_, Posix.SysStat, Posix.Unistd,
         {$endif}
       {$endif}
       {$ifdef KOL}
         KOL, err
       {$else}
         {$ifdef UNITSCOPENAMES}System.SysUtils{$else}SysUtils{$endif}
       {$endif};

type
  // RTL types
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
  {$if not Defined(FPC) and (CompilerVersion < 20)}
  TDate = type TDateTime;
  TTime = type TDateTime;
  {$ifend}
  PDate = ^TDate;
  PTime = ^TTime;
  {$if SizeOf(Extended) >= 10}
    {$define EXTENDEDSUPPORT}
  {$ifend}
  TBytes = {$ifdef SYSARRAYSUPPORT}TArray<Byte>{$else}array of Byte{$endif};
  PBytes = ^TBytes;
  {$if Defined(NEXTGEN) and (CompilerVersion >= 31)}
    AnsiChar = type System.UTF8Char;
    PAnsiChar = ^AnsiChar;
    AnsiString = type System.RawByteString;
    PAnsiString = ^AnsiString;
  {$ifend}


{ TCachedObject class }

  TCachedObject = class;
  TCachedObjectCallback = procedure (const ASender: TCachedObject; const AParam: Pointer);

  ICachedObject = interface
    function GetSelf: TCachedObject {$ifdef AUTOREFCOUNT}unsafe{$endif};
    function GetPreallocated: Boolean;
    function GetRefCount: Integer;
    property Self: TCachedObject read GetSelf;
    property Preallocated: Boolean read GetPreallocated;
    property RefCount: Integer read GetRefCount;
  end;

  TCachedObject = class(TObject, IInterface, ICachedObject)
  {$ifdef INLINESUPPORTSIMPLE}
  protected
    const
      objDestroyingFlag = Integer($80000000);
      objDisposedFlag = Integer($40000000);
      objPreallocatedFlag = Integer($20000000);
      REFCOUNT_MASK = not (objPreallocatedFlag or {$ifdef AUTOREFCOUNT}objDisposedFlag{$else}objDestroyingFlag{$endif});
  {$endif}
  protected
    {$ifNdef AUTOREFCOUNT}
    {$if (not Defined(FPC)) and (CompilerVersion >= 29)}[Volatile]{$ifend}
    FRefCount: Integer;
    {$endif}
    function GetSelf: TCachedObject {$ifdef AUTOREFCOUNT}unsafe{$endif};
    function GetPreallocated: Boolean; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    function GetRefCount: Integer; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    function QueryInterface({$ifdef FPC}constref{$else}const{$endif} IID: TGUID; out Obj): {$if (not Defined(FPC)) or Defined(MSWINDOWS)}HResult; stdcall{$else}Longint; cdecl{$ifend};
    function _AddRef: {$if (not Defined(FPC)) or Defined(MSWINDOWS)}Integer; stdcall{$else}Longint; cdecl{$ifend};
    function _Release: {$if (not Defined(FPC)) or Defined(MSWINDOWS)}Integer; stdcall{$else}Longint; cdecl{$ifend};
  public
    class function NewInstance: TObject {$ifdef AUTOREFCOUNT}unsafe{$ENDIF}; override;
    class function PreallocatedInstance(const AMemory: Pointer; const ASize: Integer): TObject {$ifdef AUTOREFCOUNT}unsafe{$ENDIF}; virtual;
    class procedure PreallocatedCall(const AParam: Pointer; const ACallback: TCachedObjectCallback); overload;
    class procedure PreallocatedCall(const AParam: Pointer; const ASize: Integer; const ACallback: TCachedObjectCallback); overload;
    procedure FreeInstance; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    {$ifdef AUTOREFCOUNT}
    function __ObjAddRef: Integer; override;
    function __ObjRelease: Integer; override;
    {$endif}
    property Preallocated: Boolean read GetPreallocated;
    property RefCount: Integer read GetRefCount;
  end;
  TCachedObjectClass = class of TCachedObject;


{ ECachedBuffer class }

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


{ TCachedBufferMemory record }

  TCachedBufferMemory = {$ifdef OPERATORSUPPORT}record{$else}object{$endif}
  public
    Handle: Pointer;
    PreviousSize: NativeUInt;
      Data: Pointer;
      Size: NativeUInt;
    Additional: Pointer;
    AdditionalSize: NativeUInt;
  private
    function GetEmpty: Boolean; {$ifdef INLINESUPPORT}inline;{$endif}
    function GetFixed: Boolean; {$ifdef INLINESUPPORT}inline;{$endif}
    function GetPreallocated: Boolean; {$ifdef INLINESUPPORT}inline;{$endif}
  public
    property Empty: Boolean read GetEmpty;
    property Fixed: Boolean read GetFixed;
    property Preallocated: Boolean read GetPreallocated;
  end;
  PCachedBufferMemory = ^TCachedBufferMemory;


{ TCachedBuffer abstract class }

  TCachedBufferKind = (cbReader, cbWriter);
  TCachedBuffer = class;
  TCachedBufferCallback = function(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt of object;
  TCachedBufferProgress = procedure(const ASender: TCachedBuffer; var ACancel: Boolean) of object;

  TCachedBuffer = class(TCachedObject)
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

    class function GetOptimalBufferSize(const AValue, ADefValue: NativeUInt; const ALimit: Int64 = 0): NativeUInt;
    function GetMargin: NativeInt; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    function GetPosition: Int64; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure SetEOF(const AValue: Boolean);
    procedure SetLimit(const AValue: Int64);
    function CheckLimit(const AValue: Int64): Boolean; virtual;
    function DoFlush: NativeUInt;
    function DoWriterFlush: Boolean;
    function DoReaderFlush: Boolean;
    function DoProgress: Boolean;
  {$ifdef FPC}
  public
  {$endif}
    constructor Create(const AKind: TCachedBufferKind; const ACallback: TCachedBufferCallback; const ABufferSize: NativeUInt = 0);
  public
    class function PreallocatedInstance(const AMemory: Pointer; const ASize: Integer): TObject; override;
    class procedure PreallocatedCall(const AParam: Pointer; const ABufferSize: NativeUInt; const ACallback: TCachedObjectCallback); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
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
  TCachedBufferClass = class of TCachedBuffer;


{ TCachedReader class }

  TCachedWriter = class;

  TCachedReader = class(TCachedBuffer)
  protected
    procedure OverflowRead(var ABuffer; ASize: NativeUInt);
    function DoDirectPreviousRead(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; virtual;
    function DoDirectFollowingRead(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; virtual;
    procedure OverflowSkip(ASize: NativeUInt);
  public
    constructor Create(const ACallback: TCachedBufferCallback; const ABufferSize: NativeUInt = 0);
    procedure DirectRead(const APosition: Int64; var ABuffer; const ACount: NativeUInt);
    property Finishing: Boolean read FFinishing;
    procedure Skip(const ACount: NativeUInt); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure Export(const AWriter: TCachedWriter; const ACount: NativeUInt = 0); {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}

    // TStream-like data reading
    procedure Read(var ABuffer; const ACount: NativeUInt);
    procedure ReadData(var AValue: Boolean); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifdef ANSISTRSUPPORT}
    procedure ReadData(var AValue: AnsiChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    procedure ReadData(var AValue: WideChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: ShortInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: Byte); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: SmallInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: Word); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: Integer); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: Cardinal); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: Int64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if Defined(FPC) or (CompilerVersion >= 16)}
    procedure ReadData(var AValue: UInt64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure ReadData(var AValue: Single); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: Double); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if (not Defined(FPC)) or Defined(EXTENDEDSUPPORT)}
    procedure ReadData(var AValue: Extended); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    {$if not Defined(FPC) and (CompilerVersion >= 23)}
    procedure ReadData(var AValue: TExtended80Rec); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure ReadData(var AValue: Currency); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: TPoint); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure ReadData(var AValue: TRect); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifdef SHORTSTRSUPPORT}
    procedure ReadData(var AValue: ShortString); overload;
    {$endif}
    {$ifdef ANSISTRSUPPORT}
    procedure ReadData(var AValue: AnsiString{$ifdef INTERNALCODEPAGE}; ACodePage: Word = 0{$endif}); overload;
    {$endif}
    {$ifdef MSWINDOWS}
    procedure ReadData(var AValue: WideString); overload;
    {$endif}
    {$ifdef UNICODE}
    procedure ReadData(var AValue: UnicodeString); overload;
    {$endif}
    procedure ReadData(var AValue: TBytes); overload;
    procedure ReadData(var AValue: Variant); overload;
  end;


{ TCachedWriter class }

  TCachedWriter = class(TCachedBuffer)
  protected
    procedure OverflowWrite(const ABuffer; ASize: NativeUInt);
    function DoDirectPreviousWrite(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; virtual;
    function DoDirectFollowingWrite(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; virtual;
  public
    constructor Create(const ACallback: TCachedBufferCallback; const ABufferSize: NativeUInt = 0);
    procedure DirectWrite(const APosition: Int64; const ABuffer; const ACount: NativeUInt);
    procedure Import(const AReader: TCachedReader; const ACount: NativeUInt = 0);

    // TStream-like data writing
    procedure Write(const ABuffer; const ACount: NativeUInt);
    procedure WriteData(const AValue: Boolean); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifdef ANSISTRSUPPORT}
    procedure WriteData(const AValue: AnsiChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    procedure WriteData(const AValue: WideChar); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: ShortInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Byte); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: SmallInt); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Word); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Integer); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Cardinal); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Int64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if Defined(FPC) or (CompilerVersion >= 16)}
    procedure WriteData(const AValue: UInt64); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure WriteData(const AValue: Single); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Double); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$if (not Defined(FPC)) or Defined(EXTENDEDSUPPORT)}
    procedure WriteData(const AValue: Extended); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    {$if not Defined(FPC) and (CompilerVersion >= 23)}
    procedure WriteData(const AValue: TExtended80Rec); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifend}
    procedure WriteData(const AValue: Currency); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: TPoint); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: TRect); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$ifdef SHORTSTRSUPPORT}
    procedure WriteData(const AValue: ShortString); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    {$ifdef ANSISTRSUPPORT}
    procedure WriteData(const AValue: AnsiString); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    {$ifdef MSWINDOWS}
    procedure WriteData(const AValue: WideString); overload;
    {$endif}
    {$ifdef UNICODE}
    procedure WriteData(const AValue: UnicodeString); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    {$endif}
    procedure WriteData(const AValue: TBytes); overload; {$ifdef INLINESUPPORTSIMPLE}inline;{$endif}
    procedure WriteData(const AValue: Variant); overload;
  end;


{ TCachedReReader class }

  TCachedReReader = class(TCachedReader)
  protected
    FSource: TCachedReader;
    FOwner: Boolean;
  public
    constructor Create(const ACallback: TCachedBufferCallback; const ASource: TCachedReader; const AOwner: Boolean = False; const ABufferSize: NativeUInt = 0);
    destructor Destroy; override;

    property Source: TCachedReader read FSource;
    property Owner: Boolean read FOwner write FOwner;
  end;


{ TCachedReWriter class }

  TCachedReWriter = class(TCachedWriter)
  protected
    FTarget: TCachedWriter;
    FOwner: Boolean;
  public
    constructor Create(const ACallback: TCachedBufferCallback; const ATarget: TCachedWriter; const AOwner: Boolean = False; const ABufferSize: NativeUInt = 0);
    destructor Destroy; override;

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

    procedure InternalCreate(const ASize: Int64; const ASeeked: Boolean);
    function CheckLimit(const AValue: Int64): Boolean; override;
    function DoDirectPreviousRead(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; override;
    function DoDirectFollowingRead(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; override;
    function InternalCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
  public
    constructor Create(const AFileName: string; const AOffset: Int64 = 0; const ASize: Int64 = 0);
    constructor CreateHandled(const AHandle: THandle; const ASize: Int64 = 0; const AHandleOwner: Boolean = False);
    destructor Destroy; override;

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

    function DoDirectPreviousWrite(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; override;
    function DoDirectFollowingWrite(APosition: Int64; AData: PByte; ASize: NativeUInt): Boolean; override;
    function InternalCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
  public
    constructor Create(const AFileName: string; const ASize: Int64 = 0);
    constructor CreateHandled(const AHandle: THandle; const ASize: Int64 = 0; const AHandleOwner: Boolean = False);
    destructor Destroy; override;

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

    function CheckLimit(const AValue: Int64): Boolean; override;
    function InternalCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
    function FixedCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
  public
    constructor Create(const APtr: Pointer; const ASize: NativeUInt; const AFixed: Boolean = False);
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

    function CheckLimit(const AValue: Int64): Boolean; override;
    function InternalCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
    function InternalTemporaryCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
    function FixedCallback(const ASender: TCachedBuffer; AData: PByte; ASize: NativeUInt): NativeUInt;
  public
    constructor Create(const APtr: Pointer; const ASize: NativeUInt; const AFixed: Boolean = False);
    constructor CreateTemporary;
    destructor Destroy; override;
    property Temporary: Boolean read FTemporary;
    property Ptr: Pointer read FPtr;
    property Size: NativeUInt read FSize;
  end;


{ TCachedResourceReader class }

  TCachedResourceReader = class(TCachedMemoryReader)
  protected
    HGlobal: THandle;
    procedure InternalCreate(AInstance: THandle; AName, AResType: PChar; AFixed: Boolean);
  public
    constructor Create(const AInstance: THandle; const AResName: string; const AResType: PChar; const AFixed: Boolean = False);
    constructor CreateFromID(const AInstance: THandle; const AResID: Word; const AResType: PChar; const AFixed: Boolean = False);
    destructor Destroy; override;
  end;


// fast non-collision Move realization
procedure NcMove(const Source; var Dest; const Size: NativeUInt); {$ifNdef CPUINTELASM}inline;{$endif}

implementation

{$ifNdef ANSISTRSUPPORT}
type
  AnsiChar = type Byte;
  PAnsiChar = ^AnsiChar;
{$endif}

{$ifdef FPC}
const
  INVALID_HANDLE_VALUE = THandle(-1);
{$endif}

const
  KB_SIZE = 1024;
  MEMORY_PAGE_SIZE = 4 * KB_SIZE;
  DEFAULT_CACHED_SIZE = 64 * KB_SIZE;
  MAX_PREALLOCATED_SIZE = 20 * MEMORY_PAGE_SIZE; // 80KB


{ Exceptions }

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

procedure RaiseLimitValue(const AValue: Int64);
begin
  raise ECachedBuffer.CreateFmt('Invalid limit value %d', [AValue]);
end;

procedure RaiseVaraintType(const VType: Word);
begin
  raise ECachedBuffer.CreateFmt('Invalid variant type 0x%.4x', [VType]);
end;


{ Utilitarian functions }

{$if Defined(FPC) or (CompilerVersion < 24)}
{$ifdef FPC}
function AtomicIncrement(var Target: Integer): Integer; inline;
begin
  Result := InterLockedIncrement(Target);
end;
{$else .DELPHI}
function AtomicIncrement(var Target: Integer): Integer;
asm
  {$ifdef CPUX86}
    mov edx, 1
    lock xadd [eax], edx
    lea eax, [edx - 1]
  {$else .CPUX64}
    mov eax, 1
    lock xadd [RCX], eax
    dec eax
  {$endif}
end;
{$endif}

{$ifdef FPC}
function AtomicDecrement(var Target: Integer): Integer; inline;
begin
  Result := InterLockedDecrement(Target);
end;
{$else .DELPHI}
function AtomicDecrement(var Target: Integer): Integer;
asm
  {$ifdef CPUX86}
    or edx, -1
    lock xadd [eax], edx
    lea eax, [edx - 1]
  {$else .CPUX64}
    or eax, -1
    lock xadd [RCX], eax
    dec eax
  {$endif}
end;
{$endif}
{$ifend}

function AllocateCachedBufferMemory(const APreviousSize, ABufferSize: NativeUInt): TCachedBufferMemory;
var
  LOffset: NativeUInt;
begin
  // detect sizes
  Result.PreviousSize := (APreviousSize + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE;
  Result.AdditionalSize := MEMORY_PAGE_SIZE;
  if (ABufferSize = 0) then
  begin
    Result.Size := DEFAULT_CACHED_SIZE;
  end else
  begin
    Result.Size := (ABufferSize + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE;
  end;

  // allocate
  GetMem(Result.Handle, Result.PreviousSize + Result.Size + Result.AdditionalSize + MEMORY_PAGE_SIZE);

  // align
  LOffset := NativeUInt(Result.Handle) and (MEMORY_PAGE_SIZE - 1);
  Inc(Result.PreviousSize, MEMORY_PAGE_SIZE - LOffset);
  Inc(Result.AdditionalSize, LOffset);
  Result.Data := Pointer(NativeUInt(Result.Handle) + Result.PreviousSize);
  Result.Additional := Pointer(NativeUInt(Result.Data) + Result.Size);
end;

function GetFileSize(AHandle: THandle): Int64;
var
  {$ifdef MSWINDOWS}
    P: TPoint;
  {$endif}
  {$ifdef POSIX}
    S: {$ifdef FPC}Stat{$else}_stat{$endif};
  {$endif}
begin
  {$ifdef MSWINDOWS}
    P.X := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.GetFileSize(AHandle, Pointer(@P.Y));
    if (P.Y = -1) then P.X := -1;
    Result := PInt64(@P)^;
  {$endif}

  {$ifdef POSIX}
    if ({$ifdef FPC}FpFStat{$else}fstat{$endif}(AHandle, S) = 0) then
      Result := S.st_size
    else
      Result := -1;
  {$endif}
end;

function DirectCachedFileMethod(const AInstance: TCachedBuffer;
  const AInstanceHandle: THandle; const AInstanceOffset, APosition: Int64;
  const AData: PByte; const ASize: NativeUInt): Boolean;
var
  LSeekValue: Int64;
  LPositionValue: Int64;
begin
  LSeekValue := FileSeek(AInstanceHandle, Int64(0), 1{soFromCurrent});
  try
    LPositionValue := APosition + AInstanceOffset;
    if (LPositionValue <> FileSeek(AInstanceHandle, LPositionValue, 0{soFromBeginning})) then
    begin
      Result := False;
    end else
    begin
      Result := (ASize = AInstance.FCallback(AInstance, AData, ASize));
    end;
  finally
    FileSeek(AInstanceHandle, LSeekValue, 0{soFromBeginning});
  end;
end;


{ TCachedObject }

{$ifNdef INLINESUPPORTSIMPLE}
const
  objDestroyingFlag = Integer($80000000);
  objDisposedFlag = Integer($40000000);
  objPreallocatedFlag = Integer($20000000);
  REFCOUNT_MASK = not (objPreallocatedFlag or {$ifdef AUTOREFCOUNT}objDisposedFlag{$else}objDestroyingFlag{$endif});
{$endif}

class function TCachedObject.NewInstance: TObject;
var
  LSize: Integer;
  LMemory: Pointer;
begin
  LSize := PInteger(NativeInt(Self) + vmtInstanceSize)^;
  GetMem(LMemory, LSize);
  Result := InitInstance(LMemory);
  TCachedObject(Result).FRefCount := 1;
end;

class function TCachedObject.PreallocatedInstance(const AMemory: Pointer;
  const ASize: Integer): TObject;
var
  LSize: Integer;
  LMemory: Pointer;
begin
  LSize := PInteger(NativeInt(Self) + vmtInstanceSize)^;
  if (ASize >= LSize) then
  begin
    LMemory := Pointer((NativeInt(AMemory) + 15) and -16);
    if (ASize < LSize + (NativeInt(LMemory) - NativeInt(AMemory))) then
    begin
      LMemory := Pointer((NativeInt(AMemory) + 7) and -8);
      if (ASize < LSize + (NativeInt(LMemory) - NativeInt(AMemory))) then
      begin
        LMemory := Pointer((NativeInt(AMemory) + 3) and -4);
        if (ASize < LSize + (NativeInt(LMemory) - NativeInt(AMemory))) then
        begin
          LMemory := Pointer((NativeInt(AMemory) + 1) and -2);
          if (ASize < LSize + (NativeInt(LMemory) - NativeInt(AMemory))) then
          begin
            LMemory := AMemory;
          end;
        end;
      end;
    end;

    Result := InitInstance(LMemory);
    TCachedObject(Result).FRefCount := 1 or objPreallocatedFlag;
  end else
  begin
    GetMem(LMemory, LSize);
    Result := InitInstance(LMemory);
    TCachedObject(Result).FRefCount := 1;
  end;
end;

procedure CachedPreallocatedCall_1(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..1 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_2(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..2 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_3(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..3 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_4(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..4 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_5(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..5 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_6(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..6 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_7(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..7 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_8(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..8 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_9(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..9 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_10(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..10 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_11(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..11 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_12(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..12 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_13(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..13 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_14(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..14 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_15(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..15 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_16(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..16 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_17(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..17 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_18(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..18 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_19(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..19 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

procedure CachedPreallocatedCall_20(const AClass: TCachedObjectClass;
  const AParam: Pointer; const ACallback: TCachedObjectCallback);
var
  LBuffer: array[0..20 * MEMORY_PAGE_SIZE + 15] of Byte;
begin
  ACallback(TCachedObject(AClass.PreallocatedInstance(@LBuffer, SizeOf(LBuffer))), AParam);
end;

type
  TCachedPreallocatedCall = procedure(const AClass: TCachedObjectClass;
    const AParam: Pointer; const ACallback: TCachedObjectCallback);

const
  CACHED_PREALLOCATED_CALLS: array[1..MAX_PREALLOCATED_SIZE div MEMORY_PAGE_SIZE] of TCachedPreallocatedCall = (
    CachedPreallocatedCall_1,
    CachedPreallocatedCall_2,
    CachedPreallocatedCall_3,
    CachedPreallocatedCall_4,
    CachedPreallocatedCall_5,
    CachedPreallocatedCall_6,
    CachedPreallocatedCall_7,
    CachedPreallocatedCall_8,
    CachedPreallocatedCall_9,
    CachedPreallocatedCall_10,
    CachedPreallocatedCall_11,
    CachedPreallocatedCall_12,
    CachedPreallocatedCall_13,
    CachedPreallocatedCall_14,
    CachedPreallocatedCall_15,
    CachedPreallocatedCall_16,
    CachedPreallocatedCall_17,
    CachedPreallocatedCall_18,
    CachedPreallocatedCall_19,
    CachedPreallocatedCall_20
  );

class procedure TCachedObject.PreallocatedCall(const AParam: Pointer;
  const ACallback: TCachedObjectCallback);
var
  LSize: Integer;
begin
  LSize := PInteger(NativeInt(Self) + vmtInstanceSize)^;
  PreallocatedCall(AParam, LSize, ACallback);
end;

class procedure TCachedObject.PreallocatedCall(const AParam: Pointer; const ASize: Integer;
  const ACallback: TCachedObjectCallback);
begin
  case ASize of
    1..MAX_PREALLOCATED_SIZE:
    begin
      CACHED_PREALLOCATED_CALLS[ASize shr 12](Self, AParam, ACallback);
    end;
  else
    ACallback(TCachedObject(Self.NewInstance), AParam);
  end;
end;

procedure TCachedObject.FreeInstance;
begin
  CleanupInstance;
  if (FRefCount and objPreallocatedFlag = 0) then
    FreeMem(Pointer(Self));
end;

procedure TCachedObject.AfterConstruction;
begin
  {$ifNdef AUTOREFCOUNT}
  if (FRefCount and REFCOUNT_MASK = 1) then
  begin
    Dec(FRefCount);
  end else
  begin
    AtomicDecrement(FRefCount);
  end;
  {$endif}
end;

procedure TCachedObject.BeforeDestruction;
var
  LRefCount: Integer;
begin
  LRefCount := FRefCount;
  {$ifdef AUTOREFCOUNT}
  if (LRefCount and objDestroyingFlag = 0) and (LRefCount and REFCOUNT_MASK = 0) then
  {$else}
  if (LRefCount and REFCOUNT_MASK <> 0) then
  {$endif}
    System.Error(reInvalidPtr);
end;

destructor TCachedObject.Destroy;
begin
  inherited;
end;

function TCachedObject.GetSelf: TCachedObject;
begin
  Result := Self;
end;

function TCachedObject.GetPreallocated: Boolean;
begin
  Result := (FRefCount and objPreallocatedFlag <> 0);
end;

function TCachedObject.GetRefCount: Integer;
begin
  Result := FRefCount and REFCOUNT_MASK;
end;

function TCachedObject.QueryInterface({$ifdef FPC}constref{$else}const{$endif} IID: TGUID; out Obj): {$if (not Defined(FPC)) or Defined(MSWINDOWS)}HResult{$else}Longint{$ifend};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCachedObject._AddRef: {$if (not Defined(FPC)) or Defined(MSWINDOWS)}Integer{$else}Longint{$ifend};
begin
  {$ifdef AUTOREFCOUNT}
  if (PPointer(PNativeInt(Self)^ + vmtObjAddRef)^ <> @TCachedObject.__ObjAddRef) then
  begin
    Result := __ObjAddRef;
    Exit;
  end;
  {$endif}

  Result := FRefCount;
  if (Cardinal(Result and REFCOUNT_MASK) <= 1) then
  begin
    Inc(Result);
    FRefCount := Result;
  end else
  begin
    Result := AtomicIncrement(FRefCount);
  end;
  Result := Result and REFCOUNT_MASK;
end;

function TCachedObject._Release: {$if (not Defined(FPC)) or Defined(MSWINDOWS)}Integer{$else}Longint{$ifend};
{$ifdef AUTOREFCOUNT}
var
  LRefCount: Integer;
{$endif}
begin
  {$ifdef AUTOREFCOUNT}
  if (PPointer(PNativeInt(Self)^ + vmtObjRelease)^ <> @TCachedObject.__ObjRelease) then
  begin
    Result := __ObjRelease;
    Exit;
  end;
  {$endif}

  Result := FRefCount;
  if (Result and REFCOUNT_MASK = 1) then
  begin
    Dec(Result);
    FRefCount := Result;
    Result := 0;
  end else
  begin
    Result := AtomicDecrement(FRefCount) and REFCOUNT_MASK;
    if (Result <> 0) then
      Exit;
  end;

  {$ifdef AUTOREFCOUNT}
  repeat
    LRefCount := FRefCount;
    if (LRefCount and objDisposedFlag <> 0) then
      Break;

    if (AtomicCmpExchange(FRefCount, LRefCount or (objDestroyingFlag or objDisposedFlag),
      LRefCount) = LRefCount) then
    begin
      Destroy;
      Exit;
    end;
  until (False);

  FreeInstance;
  {$else}
  FRefCount := FRefCount or objDestroyingFlag;
  Destroy;
  {$endif}
end;

{$ifdef AUTOREFCOUNT}
function TCachedObject.__ObjAddRef: Integer;
begin
  Result := FRefCount;
  if (Cardinal(Result and REFCOUNT_MASK) <= 1) then
  begin
    Inc(Result);
    FRefCount := Result;
  end else
  begin
    Result := AtomicIncrement(FRefCount);
  end;
  Result := Result and REFCOUNT_MASK;
end;

function TCachedObject.__ObjRelease: Integer;
var
  LRefCount: Integer;
begin
  Result := FRefCount;
  if (Result and REFCOUNT_MASK = 1) then
  begin
    Dec(Result);
    FRefCount := Result;
    Result := 0;
  end else
  begin
    Result := AtomicDecrement(FRefCount) and REFCOUNT_MASK;
    if (Result <> 0) then
      Exit;
  end;

  repeat
    LRefCount := FRefCount;
    if (LRefCount and objDisposedFlag <> 0) then
      Break;

    if (AtomicCmpExchange(FRefCount, LRefCount or (objDestroyingFlag or objDisposedFlag),
      LRefCount) = LRefCount) then
    begin
      Destroy;
      Exit;
    end;
  until (False);

  FreeInstance;
end;
{$endif}


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


{ TCachedBufferMemory }

function TCachedBufferMemory.GetEmpty: Boolean;
begin
  Result := (Data = nil);
end;

function TCachedBufferMemory.GetFixed: Boolean;
begin
  Result := (PreviousSize = 0) or (AdditionalSize = 0);
end;

function TCachedBufferMemory.GetPreallocated: Boolean;
begin
  Result := (Handle = nil);
end;


{ TCachedBuffer }

class function TCachedBuffer.GetOptimalBufferSize(const AValue, ADefValue: NativeUInt;
  const ALimit: Int64): NativeUInt;
var
  LSize: NativeUInt;
begin
  if (AValue <> 0) then
  begin
    Result := AValue;
    if (ALimit > 0) and (Result > ALimit) then Result := ALimit;
    Exit;
  end;

  if (ALimit <= 0) or (ALimit >= (ADefValue * 4)) then
  begin
    Result := ADefValue;
    Exit;
  end;

  LSize := ALimit;
  LSize := LSize shr 2;
  if (LSize = 0) then
  begin
    Result := MEMORY_PAGE_SIZE;
  end else
  begin
    Result := (LSize + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE;
  end;
end;

constructor TCachedBuffer.Create(const AKind: TCachedBufferKind;
  const ACallback: TCachedBufferCallback; const ABufferSize: NativeUInt);
var
  LSize: NativeUInt;
begin
  inherited Create;
  FKind := AKind;
  FCallback := ACallback;
  if (not Assigned(FCallback)) then
    raise ECachedBuffer.Create('Flush callback not defined');

  if (FMemory.Empty) then
  begin
    LSize := ABufferSize;
    if (LSize <> 0) and (AKind = cbWriter) and (LSize <= MEMORY_PAGE_SIZE) then
      Inc(LSize, MEMORY_PAGE_SIZE);
    FMemory := AllocateCachedBufferMemory(Ord(AKind = cbReader){4kb}, LSize);
  end;

  FOverflow := FMemory.Additional;
  if (AKind = cbWriter) then
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

class function TCachedBuffer.PreallocatedInstance(const AMemory: Pointer;
  const ASize: Integer): TObject;
var
  LAlign: NativeInt;
  LInstanceSize: NativeUInt;
  LInstanceMemory: Pointer;
  LMemory: TCachedBufferMemory;
begin
  if (ASize < MEMORY_PAGE_SIZE) then
  begin
    Result := inherited PreallocatedInstance(AMemory, ASize);
    Exit;
  end;

  if (ASize < 8 * MEMORY_PAGE_SIZE) then
  begin
    LAlign := KB_SIZE;
  end else
  begin
    LAlign := MEMORY_PAGE_SIZE;
  end;

  LMemory.Handle := nil;
  LMemory.Data := Pointer((NativeInt(AMemory) + (LAlign + LAlign - 1)) and -LAlign);
  LMemory.Additional := Pointer((NativeInt(AMemory) + (ASize - 1) - LAlign) and -LAlign);
  LMemory.Size := NativeUInt(LMemory.Additional) - NativeUInt(LMemory.Data);
  LMemory.PreviousSize := NativeUInt(LMemory.Data) - NativeUInt(AMemory);
  LMemory.AdditionalSize := NativeUInt(AMemory) + NativeUInt(Cardinal(ASize)) - NativeUInt(LMemory.Additional);

  LInstanceSize := PCardinal(NativeInt(Self) + vmtInstanceSize)^;
  if (LMemory.PreviousSize > LMemory.AdditionalSize) and (LMemory.PreviousSize - NativeUInt(LAlign) >= LInstanceSize) then
  begin
    Result := inherited PreallocatedInstance(AMemory, LMemory.PreviousSize - NativeUInt(LAlign));
    LMemory.PreviousSize := LAlign;
  end else
  if (LMemory.AdditionalSize - NativeUInt(LAlign) >= LInstanceSize) then
  begin
    LInstanceMemory := Pointer(NativeInt(LMemory.Additional) + LAlign);
    Result := inherited PreallocatedInstance(LInstanceMemory, LMemory.AdditionalSize - NativeUInt(LAlign));
    LMemory.AdditionalSize := LAlign;
  end else
  if (NativeInt(LMemory.Size + LMemory.AdditionalSize) - NativeInt(LInstanceSize) >= 2 * LAlign) then
  begin
    LMemory.Additional := Pointer(NativeInt(NativeUInt(LMemory.Additional) + LMemory.AdditionalSize - LInstanceSize) and -LAlign);
    LMemory.AdditionalSize := LAlign;
    LInstanceMemory := Pointer(NativeInt(LMemory.Additional) + LAlign);
    Result := inherited PreallocatedInstance(LInstanceMemory,
      NativeUInt(AMemory) + NativeUInt(Cardinal(ASize)) - NativeUInt(LInstanceMemory));
  end else
  begin
    Result := NewInstance;
  end;

  TCachedBuffer(Result).FMemory := LMemory;
end;

class procedure TCachedBuffer.PreallocatedCall(const AParam: Pointer;
  const ABufferSize: NativeUInt; const ACallback: TCachedObjectCallback);
var
  LSize, LInstanceSize: NativeUInt;
begin
  case ABufferSize of
    1..16 * KB_SIZE:
    begin
      LSize := NativeUInt((NativeInt(ABufferSize) + KB_SIZE - 1) and -KB_SIZE) + 3 * KB_SIZE;
    end;
    16 * KB_SIZE + 1..DEFAULT_CACHED_SIZE:
    begin
      LSize := NativeUInt((NativeInt(ABufferSize) + MEMORY_PAGE_SIZE - 1) and -MEMORY_PAGE_SIZE) + 3 * MEMORY_PAGE_SIZE;
    end
  else
    LSize := 16 * KB_SIZE + 3 * KB_SIZE;
  end;

  LInstanceSize := PCardinal(NativeInt(Self) + vmtInstanceSize)^;
  LInstanceSize := NativeUInt((NativeInt(LInstanceSize) + KB_SIZE div 2 - 1) and -KB_SIZE);
  if (LSize + LInstanceSize <= MAX_PREALLOCATED_SIZE) then
  begin
    Inc(LSize, LInstanceSize);
  end;

  inherited PreallocatedCall(AParam, LSize, ACallback);
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
  if (not FMemory.Preallocated) then
    FreeMem(FMemory.Handle);

  inherited;
end;

procedure TCachedBuffer.SetEOF(const AValue{True}: Boolean);
begin
  if (FEOF = AValue) then Exit;
  if (not AValue) then
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
  LCancel: Boolean;
begin
  if (Assigned(FOnProgress)) then
  begin
    LCancel := FEOF;
    FOnProgress(Self, LCancel);

    if (LCancel) then
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

function TCachedBuffer.CheckLimit(const AValue: Int64): Boolean;
begin
  Result := True;
end;

procedure TCachedBuffer.SetLimit(const AValue: Int64);
var
  LPosition, LMarginLimit: Int64;
  LMargin: NativeInt;
begin
  if (FLimited) and (AValue = FLimit) then Exit;

  // check limit value
  LPosition := Self.Position;
  if (FEOF) or (AValue < 0) or (LPosition > AValue) or
     ({IsReader and} FFinishing and (AValue > (LPosition + Self.Margin))) or
     (not CheckLimit(AValue)) then
    RaiseLimitValue(AValue);

  // fill parameters
  FLimited := True;
  FLimit := AValue;

  // detect margin limit is too small
  LMarginLimit := AValue - LPosition;
  LMargin := Self.Margin;
  if (LMarginLimit <= LMargin) then
  begin
    // correct Margin to MarginLimit value
    Dec(FOverflow, LMargin - NativeInt(LMarginLimit));

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
  LDone: Boolean;
begin
  LDone := False;
  try
    Result := DoFlush;
    LDone := True;
  finally
    if (not LDone) then
    begin
      SetEOF({EOF := }True);
    end;
  end;
end;

function TCachedBuffer.DoFlush: NativeUInt;
var
  LCurrent, LOverflow, LMemoryLow, LMemoryHigh: NativeUInt;
  LNewPositionBase: Int64;
  LNewEOF: Boolean;
begin
  // out of range test
  LCurrent := NativeUInt(Current);
  LOverflow := NativeUInt(FOverflow);
  LMemoryLow := NativeUInt(FStart);
  LMemoryHigh := NativeUInt(FMemory.Additional) + FMemory.AdditionalSize;
  if (LMemoryLow <= $ffff) or (LCurrent <= $ffff) or (LOverflow <= $ffff) or
     (LCurrent < LMemoryLow) or (LCurrent >= LMemoryHigh) or
     (LOverflow < LMemoryLow) or (LOverflow >= LMemoryHigh) then RaisePointers;

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
    if (LCurrent > LOverflow) then
      RaiseReading;
  end;

  // idle flush
  if {IsReader and} (FFinishing) then
  begin
    Result := (LOverflow - LCurrent);

    if (Result = 0) then
    begin
      SetEOF(True);
      DoProgress;
    end;

    Exit;
  end;

  // flush buffer
  LNewPositionBase := Self.Position;
  if (LCurrent > LOverflow) then LNewPositionBase := LNewPositionBase - Int64(LCurrent - LOverflow);
  LNewEOF := False;
  if (Kind = cbWriter) then
  begin
    if (DoWriterFlush) then
      LNewEOF := True;
  end else
  begin
    if (DoReaderFlush) then
    begin
      FFinishing := True;
      if (Current = FOverflow{Margin = 0}) then LNewEOF := True;
    end;
  end;
  FPositionBase := LNewPositionBase;
  if (LNewEOF) then SetEOF(True);
  DoProgress;

  // Result
  Result := Self.Margin;
end;

function TCachedBuffer.DoWriterFlush: Boolean;
var
  LFlushSize, R: NativeUInt;
  LOverflowSize: NativeUInt;
  LMargin: NativeInt;
  LMarginLimit: Int64;
begin
  // Current correction
  if (NativeUInt(FHighWritten) > NativeUInt(Current)) then
    Current := FHighWritten;

  // flush size
  LFlushSize := NativeUInt(Current) - NativeUInt(FMemory.Data);
  Result := (LFlushSize < FMemory.Size);
  LOverflowSize := 0;
  if (LFlushSize > FMemory.Size) then
  begin
    LOverflowSize := LFlushSize - FMemory.Size;
    LFlushSize := FMemory.Size;
  end;

  // detect margin limit
  LMarginLimit := High(Int64);
  if (FLimited) then LMarginLimit := FLimit - Position;

  // run callback
  if (LFlushSize <> 0) then
  begin
    R := FCallback(Self, FMemory.Data, LFlushSize);
    if (R <> LFlushSize) then RaiseWriting;
  end;

  // current
  Current := FMemory.Data;
  if (LOverflowSize <> 0) then
  begin
    NcMove(FOverflow^, Current^, LOverflowSize);
    Inc(Current, LOverflowSize);
  end;
  FHighWritten := Current;

  // overflow correction
  if (FLimited) then
  begin
    LMargin := Self.Margin;
    if (LMarginLimit < LMargin) then
      Dec(FOverflow, LMargin - NativeInt(LMarginLimit));
  end;
end;

function TCachedBuffer.DoReaderFlush: Boolean;
var
  LMargin: NativeUInt;
  LOldMemory, LNewMemory: TCachedBufferMemory;
  LMarginLimit: Int64;
  LFlushSize, R: NativeUInt;
begin
  LMargin := Self.Margin;

  // move margin data to previous memory
  if (LMargin > 0) then
  begin
    if (LMargin > FMemory.PreviousSize) then
    begin
      LOldMemory := FMemory;
      LNewMemory := AllocateCachedBufferMemory(LMargin, FMemory.Size);
      try
        NcMove(Current^, Pointer(NativeUInt(LNewMemory.Data) - LMargin)^, LMargin);
        FMemory := LNewMemory;
      finally
        if (not LOldMemory.Preallocated) then
          FreeMem(LOldMemory.Handle);
      end;
    end else
    begin
      NcMove(Current^, Pointer(NativeUInt(FMemory.Data) - LMargin)^, LMargin);
    end;
  end;

  // flush size
  LFlushSize := FMemory.Size;
  Result := False;
  if (FLimited) then
  begin
    LMarginLimit := FLimit - Position;
    if (LMarginLimit <= LFlushSize) then
    begin
      LFlushSize := LMarginLimit;
      Result := True;
    end;
  end;

  // run callback
  if (LFlushSize = 0) then
  begin
    R := LFlushSize{0};
  end else
  begin
    R := FCallback(Self, FMemory.Data, LFlushSize);
    if (R > LFlushSize) then RaiseReading;
    if (R < LFlushSize) then Result := True;
  end;

  // current/overflow
  FStart := Pointer(NativeUInt(FMemory.Data) - LMargin);
  Current := FStart;
  FOverflow := Pointer(NativeUInt(FMemory.Data) + R);
end;


{$ifdef CPUX86}
var
  SSE_SUPPORT: Boolean;

procedure NcMoveInternal(const Source; var Dest; const Size: NativeUInt); forward;
{$endif}

procedure TCachedWriter.Write(const ABuffer; const ACount: NativeUInt);
{$ifNdef CPUINTELASM}
var
  P: PByte;
begin
  P := Current;
  Inc(P, ACount);

  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowWrite(ABuffer, ACount);
  end else
  begin
    Current := P;
    Dec(P, ACount);
    NcMove(ABuffer, P^, ACount);
  end;
end;
{$else .CPUX86 or .CPUX64} {$ifdef FPC}assembler; nostackframe;{$endif}
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

procedure TCachedReader.Read(var ABuffer; const ACount: NativeUInt);
{$ifNdef CPUINTELASM}
var
  P: PByte;
begin
  P := Current;
  Inc(P, ACount);

  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(ABuffer, ACount);
  end else
  begin
    Current := P;
    Dec(P, ACount);
    NcMove(P^, ABuffer, ACount);
  end;
end;
{$else .CPUX86 or .CPUX64} {$ifdef FPC}assembler; nostackframe;{$endif}
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


{$ifNdef CPUINTELASM}
// System memcpy recall
procedure NcMove(const Source; var Dest; const Size: NativeUInt);
begin
  {$ifdef FPC}
    Move(Source, Dest, Size);
  {$else}
    memcpy(Dest, Source, Size);
  {$endif}
end;
{$else .CPUX86 or .CPUX64}
// SSE-based non-collision Move realization
procedure NcMove(const Source; var Dest; const Size: NativeUInt); {$ifdef FPC}assembler; nostackframe;{$endif}
{$ifdef CPUX86}
asm
  push ebx
  movzx ebx, byte ptr [SSE_SUPPORT]
  jmp NcMoveInternal
end;
procedure NcMoveInternal(const Source; var Dest; const Size: NativeUInt); {$ifdef FPC}assembler; nostackframe;{$endif}
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
    mov r9, offset @move_03_items
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
    mov r8, offset @move_dwords
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
    mov r9, offset @aligned_reads
    shr rcx, 4
    test rax, 15
    jz @move_16128
    mov r9, offset @unaligned_reads
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
    mov r9, offset @aligned_writes + 8*4
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

constructor TCachedReader.Create(const ACallback: TCachedBufferCallback;
  const ABufferSize: NativeUInt);
begin
  inherited Create(cbReader, ACallback, ABufferSize);
end;

procedure TCachedReader.DirectRead(const APosition: Int64; var ABuffer;
  const ACount: NativeUInt);
var
  LDone: Boolean;
  LPositionHigh: Int64;
  LCachedLow, LCachedHigh: Int64;
  LCachedOffset, LBufferOffset, LSize: NativeUInt;
  {$ifdef SMALLINT}
    LSize64: Int64;
  {$endif}
begin
  if (ACount = 0) then Exit;

  LPositionHigh := APosition + Int64(ACount);
  LDone := (not FEOF) and (APosition >= 0) and ((not Limited) or (Limit >= LPositionHigh));
  if (LDone) then
  begin
    LCachedLow := FPositionBase - (NativeInt(FMemory.Data) - NativeInt(FStart));
    LCachedHigh := FPositionBase + (NativeInt(FOverflow) - NativeInt(FMemory.Data));

    // cached data copy
    LCachedOffset := 0;
    LBufferOffset := 0;
    LSize := 0;
    if (APosition >= LCachedLow) and (APosition < LCachedHigh) then
    begin
      LCachedOffset := (APosition - LCachedLow);
      LSize := (LCachedHigh - APosition);
    end else
    if (LPositionHigh > LCachedLow) and (APosition < LCachedHigh) then
    begin
      LBufferOffset := (LCachedLow - APosition);
      LSize := (LPositionHigh - LCachedLow);
    end;
    if (LSize <> 0) then
    begin
      if (LSize > ACount) then LSize := ACount;
      NcMove(Pointer(NativeUInt(FStart) + LCachedOffset)^,
             Pointer(NativeUInt(@ABuffer) + LBufferOffset)^,
             LSize);
    end;

    // before cached
    if (APosition < LCachedLow) then
    begin
      {$ifdef LARGEINT}
        LSize := (LCachedLow - APosition);
      {$else .SMALLINT}
        LSize64 := (LCachedLow - APosition);
        LSize := LSize64;
        if (LSize <> LSize64) then LSize := ACount;
      {$endif}
      if (LSize > ACount) then LSize := ACount;
      LDone := DoDirectPreviousRead(APosition, @ABuffer, LSize);
    end;

    // after cached
    if (LDone) and (LPositionHigh > LCachedHigh) then
    begin
      {$ifdef LARGEINT}
        LSize := (LPositionHigh - LCachedHigh);
      {$else .SMALLINT}
        LSize64 := (LPositionHigh - LCachedHigh);
        LSize := LSize64;
        if (LSize <> LSize64) then LSize := ACount;
      {$endif}
      if (LSize > ACount) then LSize := ACount;
      LDone := DoDirectFollowingRead(LPositionHigh - Int64(LSize),
                                    Pointer(NativeUInt(@ABuffer) + (ACount - LSize)),
                                    LSize);
    end;
  end;

  if (not LDone) then
    raise ECachedBuffer.Create('Direct read failure');
end;

function TCachedReader.DoDirectPreviousRead(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
begin
  Result := False;
end;

function TCachedReader.DoDirectFollowingRead(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
var
  LPreviousSize, LFilledSize, LAllocatingSize: NativeUInt;
  {$ifdef SMALLINT}
    LAllocatingSize64: Int64;
  {$endif}
  LRemovableMemory, LNewMemory: TCachedBufferMemory;
  LNewStart: Pointer;
  LFlushSize, R: NativeUInt;
  LMarginLimit: Int64;
begin
  Result := False;
  if (NativeUInt(FStart) > NativeUInt(FMemory.Data))  or
    (NativeUInt(Current) < NativeUInt(FStart)) then Exit;
  LPreviousSize := NativeUInt(FMemory.Data) - NativeUInt(FStart);
  LFilledSize := NativeUInt(FOverflow) - NativeUInt(FMemory.Data);

  {$ifdef LARGEINT}
    LAllocatingSize := NativeUInt(APosition - Self.FPositionBase) + ASize;
  {$else .SMALLINT}
    LAllocatingSize64 := (APosition - Self.FPositionBase) + ASize;
    LAllocatingSize := LAllocatingSize64;
    if (LAllocatingSize <> LAllocatingSize64) then Exit;
  {$endif}

  // allocate
  try
    LNewMemory := AllocateCachedBufferMemory(LPreviousSize or {4kb minimum}1, LAllocatingSize);
    Result := True;
  except
  end;
  if (not Result) then Exit;

  // copy data, change pointers
  LRemovableMemory := LNewMemory;
  try
    LNewStart := Pointer(NativeUInt(LNewMemory.Data) - LPreviousSize);
    NcMove(FStart^, LNewStart^, LPreviousSize + LFilledSize);

    FStart := LNewStart;
    Current := Pointer(NativeInt(Current) - NativeInt(FMemory.Data) + NativeInt(LNewMemory.Data));
    FOverflow := Pointer(NativeUInt(LNewMemory.Data) + LFilledSize);

    LRemovableMemory := FMemory;
    FMemory := LNewMemory;
  finally
    if (not LRemovableMemory.Preallocated) then
      FreeMem(LRemovableMemory.Handle);
  end;

  // fill buffer
  LFlushSize := NativeUInt(FMemory.Additional) - NativeUInt(FOverflow);
  if (FLimited) then
  begin
    LMarginLimit := FLimit - (Self.FPositionBase + Int64(LFilledSize));
    if (LMarginLimit <= LFlushSize) then
    begin
      LFlushSize := LMarginLimit;
      FFinishing := True;
    end;
  end;
  R := FCallback(Self, FOverflow, LFlushSize);
  if (R > LFlushSize) then RaiseReading;
  if (R < LFlushSize) then FFinishing := True;
  Inc(FOverflow, R);
  if (APosition + Int64(ASize) > Self.FPositionBase + Int64(NativeUInt(FOverflow) - NativeUInt(Memory.Data))) then
  begin
    Result := False;
    Exit;
  end;

  // data read
  NcMove(Pointer(NativeUInt(APosition - Self.FPositionBase) + NativeUInt(FMemory.Data))^,
    AData^, ASize);
end;

// Margin < Size
procedure TCachedReader.OverflowRead(var ABuffer; ASize: NativeUInt);
var
  S: NativeUInt;
  LData: PByte;
  LMargin: NativeUInt;
begin
  LData := Pointer(@ABuffer);

  // last time failure reading
  if (NativeUInt(Current) > NativeUInt(FOverflow)) then
    RaiseReading;

  // limit test
  if (FLimited) and (Self.Position + Int64(ASize) > FLimit) then
    RaiseReading;

  // read Margin
  if (Current <> FOverflow) then
  begin
    LMargin := Self.Margin;

    NcMove(Current^, LData^, LMargin);
    Inc(LData, LMargin);
    Inc(Current, LMargin);
    Dec(ASize, LMargin);
  end;

  // if read data is too large, we can read data directly
  if (ASize >= FMemory.Size) then
  begin
    S := ASize - (ASize mod FMemory.Size);
    Dec(ASize, S);

    if (Assigned(FOnProgress)) then
    begin
      while (S <> 0) do
      begin
        if (FMemory.Size <> FCallback(Self, LData, FMemory.Size)) then
          RaiseReading;

        Dec(S, FMemory.Size);
        Inc(LData, FMemory.Size);
        Inc(FPositionBase, FMemory.Size);

        DoProgress;
        if (FEOF) and ((S <> 0) or (ASize <> 0)) then
          RaiseReading;
      end;
    end else
    begin
      if (S <> FCallback(Self, LData, S)) then
        RaiseReading;

      Inc(LData, S);
      Inc(FPositionBase, S);
    end;
  end;

  // last Data bytes
  if (ASize <> 0) then
  begin
    Flush;
    if (NativeUInt(Self.Margin) < ASize) then RaiseReading;

    NcMove(Current^, LData^, ASize);
    Inc(Current, ASize);
  end;
end;

procedure TCachedReader.ReadData(var AValue: Boolean);
var
  P: ^Boolean;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

{$ifdef ANSISTRSUPPORT}
procedure TCachedReader.ReadData(var AValue: AnsiChar);
var
  P: ^AnsiChar;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;
{$endif}

procedure TCachedReader.ReadData(var AValue: WideChar);
var
  P: ^WideChar;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: ShortInt);
var
  P: ^ShortInt;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: Byte);
var
  P: ^Byte;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: SmallInt);
var
  P: ^SmallInt;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: Word);
var
  P: ^Word;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: Integer);
var
  P: ^Integer;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: Cardinal);
var
  P: ^Cardinal;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: Int64);
var
  P: ^Int64;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

{$if Defined(FPC) or (CompilerVersion >= 16)}
procedure TCachedReader.ReadData(var AValue: UInt64);
var
  P: ^UInt64;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;
{$ifend}

procedure TCachedReader.ReadData(var AValue: Single);
var
  P: ^Single;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: Double);
var
  P: ^Double;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

{$if (not Defined(FPC)) or Defined(EXTENDEDSUPPORT)}
procedure TCachedReader.ReadData(var AValue: Extended);
var
  P: ^Extended;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;
{$ifend}

{$if not Defined(FPC) and (CompilerVersion >= 23)}
procedure TCachedReader.ReadData(var AValue: TExtended80Rec);
var
  P: ^TExtended80Rec;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;
{$ifend}

procedure TCachedReader.ReadData(var AValue: Currency);
var
  P: ^Currency;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: TPoint);
var
  P: ^TPoint;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

procedure TCachedReader.ReadData(var AValue: TRect);
var
  P: ^TRect;
begin
  P := Pointer(Current);
  Inc(P);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowRead(AValue, SizeOf(AValue));
  end else
  begin
    Pointer(Current) := P;
    Dec(P);
    AValue := P^;
  end;
end;

{$ifdef SHORTSTRSUPPORT}
procedure TCachedReader.ReadData(var AValue: ShortString{; MaxLength: Byte});
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

  M := High(AValue);
  if (M < L) then
  begin
    PByte(@AValue)^ := M;
    S := L - M;
    Read(AValue[1], M);

    P := Current;
    Inc(P, S);
    Current := P;
    if (NativeUInt(P) >= NativeUInt(Self.Overflow)) then Flush;
  end else
  begin
    PByte(@AValue)^ := L;
    Read(AValue[1], L);
  end;
end;
{$endif}

{$ifdef ANSISTRSUPPORT}
procedure TCachedReader.ReadData(var AValue: AnsiString{$ifdef INTERNALCODEPAGE}; ACodePage: Word{$endif});
{$ifdef INTERNALCODEPAGE}
const
  ASTR_OFFSET_CODEPAGE = {$ifdef FPC}SizeOf(NativeInt) * 3{$else .DELPHI}12{$endif};
{$endif}
var
  L: Integer;
begin
  ReadData(L);
  SetLength(AValue, L);

  if (L <> 0) then
  begin
    {$ifdef INTERNALCODEPAGE}
    PWord(PAnsiChar(Pointer(AValue)) - ASTR_OFFSET_CODEPAGE)^ := ACodePage;
    {$endif}
    Read(Pointer(AValue)^, L);
  end;
end;
{$endif}

{$ifdef MSWINDOWS}
procedure TCachedReader.ReadData(var AValue: WideString);
var
  L: Integer;
begin
  ReadData(L);
  SetLength(AValue, L);
  if (L <> 0) then Read(Pointer(AValue)^, L*2);
end;
{$endif}

{$ifdef UNICODE}
procedure TCachedReader.ReadData(var AValue: UnicodeString);
var
  L: Integer;
begin
  ReadData(L);
  SetLength(AValue, L);
  if (L <> 0) then Read(Pointer(AValue)^, L*2);
end;
{$endif}

procedure TCachedReader.ReadData(var AValue: TBytes);
var
  L: Integer;
begin
  ReadData(L);
  SetLength(AValue, L);
  if (L <> 0) then Read(Pointer(AValue)^, L);
end;

procedure TCachedReader.ReadData(var AValue: Variant);
const
  varDeepData = $BFE8;
var
  LVarData: PVarData;
begin
  LVarData := @TVarData(AValue);
  if (LVarData.VType and varDeepData <> 0) then
  begin
    case LVarData.VType of
      {$ifdef ANSISTRSUPPORT}
      varString:
      begin
        AnsiString(LVarData.VString) := '';
        Exit;
      end;
      {$endif}
      {$ifdef WIDESTRSUPPORT}
      varOleStr:
      begin
        WideString(LVarData.VPointer{VOleStr}) := '';
        Exit;
      end;
      {$endif}
      {$ifdef UNICODE}
      varUString:
      begin
        UnicodeString(LVarData.VString) := '';
        Exit;
      end;
      {$endif}
    else
      {$if Defined(FPC) or (CompilerVersion >= 15)}
        if (Assigned(VarClearProc)) then
        begin
          VarClearProc(LVarData^);
        end else
        RaiseVaraintType(LVarData.VType);
      {$else}
        VarClear(Value);
      {$ifend}
    end;
  end;
  LVarData.VPointer := nil;

  ReadData(LVarData.VType);
  case LVarData.VType of
    varBoolean,
    varShortInt,
    varByte: ReadData(LVarData.VByte);

    varSmallInt,
    varWord: ReadData(LVarData.VWord);

    varInteger,
    varLongWord,
    varSingle: ReadData(LVarData.VInteger);

    varDouble,
    varCurrency,
    varDate,
    varInt64,
    $15{varUInt64}: ReadData(LVarData.VInt64);

    {$ifdef ANSISTRSUPPORT}
    varString:
    begin
      ReadData(AnsiString(LVarData.VPointer));
      Exit;
    end;
    {$endif}
    {$ifdef WIDESTRSUPPORT}
    varOleStr:
    begin
      ReadData(WideString(LVarData.VPointer));
      Exit;
    end;
    {$endif}
    {$ifdef UNICODE}
    varUString:
    begin
      ReadData(UnicodeString(LVarData.VPointer));
      Exit;
    end;
    {$endif}
  else
    RaiseVaraintType(LVarData.VType);
  end;
end;

procedure TCachedReader.Skip(const ACount: NativeUInt);
var
  P: PByte;
begin
  P := Current;
  Inc(P, ACount);
  if (NativeUInt(P) > NativeUInt(Self.FOverflow)) then
  begin
    OverflowSkip(ACount);
  end else
  begin
    Current := P;
  end;
end;

procedure TCachedReader.OverflowSkip(ASize: NativeUInt);
var
  S: NativeUInt;
  LDone: Boolean;
begin
  LDone := False;
  if (not FEOF) and ((not FLimited) or (FLimit >= (Self.Position + Int64(ASize)))) then
  begin
    repeat
      S := NativeUInt(FOverflow) - NativeUInt(Current);
      if (S > ASize) then S := ASize;
      Current := FOverflow;
      Dec(ASize, S);
      if (ASize <> 0) then Flush;
    until (FEOF) or (ASize = 0);

    LDone := (ASize = 0);
  end;

  if (not LDone) then
    raise ECachedBuffer.Create('Cached reader skip failure');
end;

procedure TCachedReader.Export(const AWriter: TCachedWriter;
  const ACount: NativeUInt);
begin
  AWriter.Import(Self, ACount);
end;

{ TCachedWriter }

constructor TCachedWriter.Create(const ACallback: TCachedBufferCallback;
  const ABufferSize: NativeUInt);
begin
  inherited Create(cbWriter, ACallback, ABufferSize);
end;

procedure TCachedWriter.DirectWrite(const APosition: Int64; const ABuffer;
  const ACount: NativeUInt);
var
  LDone: Boolean;
  LPositionHigh: Int64;
  LCachedLow, LCachedHigh: Int64;
  LCachedOffset, LBufferOffset, LSize: NativeUInt;
  {$ifdef SMALLINT}
    LSize64: Int64;
  {$endif}
  LHighWritten: NativeUInt;
begin
  if (ACount = 0) then Exit;

  LPositionHigh := APosition + Int64(ACount);
  LDone := (not FEOF) and (APosition >= 0) and ((not Limited) or (Limit >= LPositionHigh));
  if (LDone) then
  begin
    LCachedLow := FPositionBase - (NativeInt(FMemory.Data) - NativeInt(FStart));
    LCachedHigh := FPositionBase + (NativeInt(FOverflow) - NativeInt(FMemory.Data));

    // cached data copy
    LCachedOffset := 0;
    LBufferOffset := 0;
    LSize := 0;
    if (APosition >= LCachedLow) and (APosition < LCachedHigh) then
    begin
      LCachedOffset := (APosition - LCachedLow);
      LSize := (LCachedHigh - APosition);
    end else
    if (LPositionHigh > LCachedLow) and (APosition < LCachedHigh) then
    begin
      LBufferOffset := (LCachedLow - APosition);
      LSize := (LPositionHigh - LCachedLow);
    end;
    if (LSize <> 0) then
    begin
      if (LSize > ACount) then LSize := ACount;
      NcMove(Pointer(NativeUInt(@ABuffer) + LBufferOffset)^,
             Pointer(NativeUInt(FStart) + LCachedOffset)^,
             LSize);

      LHighWritten := NativeUInt(FStart) + LCachedOffset + LSize;
      if (LHighWritten > NativeUInt(Self.FHighWritten)) then Self.FHighWritten := Pointer(LHighWritten);
    end;

    // before cached
    if (APosition < LCachedLow) then
    begin
      {$ifdef LARGEINT}
        LSize := (LCachedLow - APosition);
      {$else .SMALLINT}
        LSize64 := (LCachedLow - APosition);
        LSize := LSize64;
        if (LSize <> LSize64) then LSize := ACount;
      {$endif}
      if (LSize > ACount) then LSize := ACount;
      LDone := DoDirectPreviousWrite(APosition, @ABuffer, LSize);
    end;

    // after cached
    if (LDone) and (LPositionHigh > LCachedHigh) then
    begin
      {$ifdef LARGEINT}
        LSize := (LPositionHigh - LCachedHigh);
      {$else .SMALLINT}
        LSize64 := (LPositionHigh - LCachedHigh);
        LSize := LSize64;
        if (LSize <> LSize64) then LSize := ACount;
      {$endif}
      if (LSize > ACount) then LSize := ACount;
      LDone := DoDirectFollowingWrite(LPositionHigh - Int64(LSize),
        Pointer(NativeUInt(@ABuffer) + (ACount - LSize)), LSize);
    end;
  end;

  if (not LDone) then
    raise ECachedBuffer.Create('Direct write failure');
end;

function TCachedWriter.DoDirectPreviousWrite(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
begin
  Result := False;
end;

function TCachedWriter.DoDirectFollowingWrite(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
var
  LPreviousSize, LFilledSize, LAllocatingSize: NativeUInt;
  {$ifdef SMALLINT}
    LAllocatingSize64: Int64;
  {$endif}
  LRemovableMemory, LNewMemory: TCachedBufferMemory;
begin
  Result := False;
  if (NativeUInt(Current) < NativeUInt(FMemory.Data)) then
  begin
    LPreviousSize := NativeUInt(FMemory.Data) - NativeUInt(Current);
    LFilledSize := 0;
  end else
  begin
    LPreviousSize := 0;
    LFilledSize := NativeUInt(Current) - NativeUInt(FMemory.Data);
  end;

  {$ifdef LARGEINT}
    LAllocatingSize := NativeUInt(APosition - Self.FPositionBase) + ASize;
  {$else .SMALLINT}
    LAllocatingSize64 := (APosition - Self.FPositionBase) + ASize;
    LAllocatingSize := LAllocatingSize64;
    if (LAllocatingSize <> LAllocatingSize64) then Exit;
  {$endif}

  // allocate
  try
    LNewMemory := AllocateCachedBufferMemory(LPreviousSize{default = 0}, LAllocatingSize);
    Result := True;
  except
  end;
  if (not Result) then Exit;

  // copy data, change pointers
  LRemovableMemory := LNewMemory;
  try
    NcMove(Pointer(NativeUInt(FMemory.Data) - LPreviousSize)^,
      Pointer(NativeUInt(LNewMemory.Data) - LPreviousSize)^, LPreviousSize + LFilledSize);

    Current := Pointer(NativeInt(Current) - NativeInt(FMemory.Data) + NativeInt(LNewMemory.Data));
    FOverflow := LNewMemory.Additional;

    LRemovableMemory := FMemory;
    FMemory := LNewMemory;
  finally
    if (not LRemovableMemory.Preallocated) then
      FreeMem(LRemovableMemory.Handle);
  end;

  // data write
  FHighWritten := Pointer(NativeUInt(APosition - Self.FPositionBase) + NativeUInt(FMemory.Data));
  NcMove(AData^, Pointer(NativeUInt(FHighWritten) - ASize)^, ASize);
end;

// Margin < Size
procedure TCachedWriter.OverflowWrite(const ABuffer; ASize: NativeUInt);
var
  S: NativeUInt;
  LData: PByte;
  LMargin: NativeInt;
begin
  LData := Pointer(@ABuffer);

  // limit test
  if (FLimited) and (Self.Position + Int64(ASize) > FLimit) then
    RaiseWriting;

  // write margin to buffer if used
  if (Current <> FMemory.Data) then
  begin
    if (NativeUInt(Current) < NativeUInt(FOverflow)) then
    begin
      LMargin := Self.Margin;
      NcMove(LData^, Current^, LMargin);

      Current := Self.Overflow;
      Inc(LData, LMargin);
      Dec(ASize, LMargin);
    end;

    Flush();
  end;

  // if written data is too large, we can write data directly
  if (ASize >= FMemory.Size) then
  begin
    S := ASize - (ASize mod FMemory.Size);
    Dec(ASize, S);

    if (Assigned(FOnProgress)) then
    begin
      while (S <> 0) do
      begin
        if (FMemory.Size <> FCallback(Self, LData, FMemory.Size)) then
          RaiseWriting;

        Dec(S, FMemory.Size);
        Inc(LData, FMemory.Size);
        Inc(FPositionBase, FMemory.Size);

        DoProgress;
        if (FEOF) and ((S <> 0) or (ASize <> 0)) then
          RaiseWriting;
      end;
    end else
    begin
      if (S <> FCallback(Self, LData, S)) then
        RaiseWriting;

      Inc(LData, S);
      Inc(FPositionBase, S);
    end;
  end;

  // last Data bytes
  if (ASize <> 0) then
  begin
    NcMove(LData^, Current^, ASize);
    Inc(Current, ASize);
  end;
end;

procedure TCachedWriter.WriteData(const AValue: Boolean);
var
  P: ^Boolean;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$ifdef ANSISTRSUPPORT}
procedure TCachedWriter.WriteData(const AValue: AnsiChar);
var
  P: ^AnsiChar;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;
{$endif}

procedure TCachedWriter.WriteData(const AValue: WideChar);
var
  P: ^WideChar;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: ShortInt);
var
  P: ^ShortInt;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: Byte);
var
  P: ^Byte;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: SmallInt);
var
  P: ^SmallInt;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: Word);
var
  P: ^Word;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: Integer);
var
  P: ^Integer;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: Cardinal);
var
  P: ^Cardinal;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: Int64);
var
  P: ^Int64;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$if Defined(FPC) or (CompilerVersion >= 16)}
procedure TCachedWriter.WriteData(const AValue: UInt64);
var
  P: ^UInt64;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;
{$ifend}

procedure TCachedWriter.WriteData(const AValue: Single);
var
  P: ^Single;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: Double);
var
  P: ^Double;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$if (not Defined(FPC)) or Defined(EXTENDEDSUPPORT)}
procedure TCachedWriter.WriteData(const AValue: Extended);
var
  P: ^Extended;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;
{$ifend}

{$if not Defined(FPC) and (CompilerVersion >= 23)}
procedure TCachedWriter.WriteData(const AValue: TExtended80Rec);
var
  P: ^TExtended80Rec;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;
{$ifend}

procedure TCachedWriter.WriteData(const AValue: Currency);
var
  P: ^Currency;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: TPoint);
var
  P: ^TPoint;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

procedure TCachedWriter.WriteData(const AValue: TRect);
var
  P: ^TRect;
begin
  P := Pointer(Current);
  P^ := AValue;
  Inc(P);
  Pointer(Current) := P;
  if (NativeUInt(P) >= NativeUInt(Self.FOverflow)) then Flush;
end;

{$ifdef SHORTSTRSUPPORT}
procedure TCachedWriter.WriteData(const AValue: ShortString);
begin
  Write(AValue, Length(AValue) + 1);
end;
{$endif}

{$ifdef ANSISTRSUPPORT}
procedure TCachedWriter.WriteData(const AValue: AnsiString);
var
  P: PInteger;
begin
  P := Pointer(AValue);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    {$if Defined(FPC) and Defined(LARGEINT)}
      Dec(NativeInt(P), SizeOf(NativeInt));
      WriteData(P^);
      Write(Pointer(PAnsiChar(P) + SizeOf(NativeInt))^, P^);
    {$else}
      Dec(P);
      Write(P^, P^ + SizeOf(Integer));
    {$ifend}
  end;
end;
{$endif}

{$ifdef MSWINDOWS}
procedure TCachedWriter.WriteData(const AValue: WideString);
var
  P: PInteger;
begin
  P := Pointer(AValue);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    Dec(P);
    WriteData(P^ shr 1);
    Write(Pointer(PAnsiChar(P) + SizeOf(Integer))^, P^);
  end;
end;
{$endif}

{$ifdef UNICODE}
procedure TCachedWriter.WriteData(const AValue: UnicodeString);
var
  P: PInteger;
begin
  P := Pointer(AValue);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    {$if Defined(FPC) and Defined(LARGEINT)}
      Dec(NativeInt(P), SizeOf(NativeInt));
      WriteData(P^);
      Write(Pointer(PAnsiChar(P) + SizeOf(NativeInt))^, P^ shl 1);
    {$else}
      Dec(P);
      Write(P^, P^ shl 1 + SizeOf(Integer));
    {$ifend}
  end;
end;
{$endif}

procedure TCachedWriter.WriteData(const AValue: TBytes);
var
  P: PNativeInt;
  {$if Defined(FPC) and Defined(LARGEINT)}
  L: Integer;
  {$ifend}
begin
  P := Pointer(AValue);
  if (P = nil) then
  begin
    WriteData(Integer(0));
  end else
  begin
    Dec(P);
    {$if Defined(FPC) and Defined(LARGEINT)}
      L := P^ {$ifdef FPC}+ 1{$endif};
      Inc(P);
      WriteData(L);
      Write(P^, L);
    {$else}
      Write(P^, P^ + SizeOf(Integer));
    {$ifend}
  end;
end;

procedure TCachedWriter.WriteData(const AValue: Variant);
var
  VType: Word;
  VPtr: Pointer;
begin
  VType := TVarData(AValue).VType;
  VPtr := @TVarData(AValue).VByte;

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

    {$ifdef ANSISTRSUPPORT}
    varString:
    begin
      WriteData(PAnsiString(VPtr)^);
      Exit;
    end;
    {$endif}
    {$ifdef WIDESTRSUPPORT}
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

procedure TCachedWriter.Import(const AReader: TCachedReader;
  const ACount: NativeUInt);
var
  LSize, LReadLimit: NativeUInt;
begin
  LReadLimit := ACount;
  if (ACount = 0) then LReadLimit := High(NativeUInt);

  if (AReader <> nil) then
  while (not AReader.EOF) and (LReadLimit <> 0) do
  begin
    if (NativeUInt(AReader.Current) > NativeUInt(AReader.Overflow)) then Break;
    LSize := NativeUInt(AReader.Overflow) - NativeUInt(AReader.Current);
    if (LSize > LReadLimit) then LSize := LReadLimit;

    Self.Write(AReader.Current^, LSize);
    Inc(AReader.Current, LSize);
    Dec(LReadLimit, LSize);

    if (LReadLimit <> 0) then
      AReader.Flush;
  end;

  if (LReadLimit <> 0) and (ACount <> 0) then
    raise ECachedBuffer.Create('Cached writer import failure');
end;


{ TCachedReReader }

constructor TCachedReReader.Create(const ACallback: TCachedBufferCallback;
  const ASource: TCachedReader; const AOwner: Boolean;
  const ABufferSize: NativeUInt);
begin
  FSource := ASource;
  FOwner := AOwner;
  inherited Create(ACallback, GetOptimalBufferSize(ABufferSize, DEFAULT_CACHED_SIZE, ASource.Limit));
end;

destructor TCachedReReader.Destroy;
begin
  if (FOwner) then FSource.Free;
  inherited;
end;


{ TCachedReWriter }


constructor TCachedReWriter.Create(const ACallback: TCachedBufferCallback;
  const ATarget: TCachedWriter; const AOwner: Boolean;
  const ABufferSize: NativeUInt);
begin
  FTarget := ATarget;
  FOwner := AOwner;
  inherited Create(ACallback, GetOptimalBufferSize(ABufferSize, DEFAULT_CACHED_SIZE, ATarget.Limit));
end;

destructor TCachedReWriter.Destroy;
begin
  if (FOwner) then FTarget.Free;
  inherited;
end;


{ TCachedFileReader }

constructor TCachedFileReader.Create(const AFileName: string; const AOffset,
  ASize: Int64);
begin
  FFileName := AFileName;
  FHandleOwner := True;
  FOffset := AOffset;
  {$ifdef MSWINDOWS}
  FHandle := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.{$ifdef UNICODE}CreateFileW{$else}CreateFile{$endif}
    (PChar(AFileName), $0001{FILE_READ_DATA}, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
  {$else}
  FHandle := FileOpen(AFileName, fmOpenRead or fmShareDenyNone);
  {$endif}
  if (FHandle = INVALID_HANDLE_VALUE) then
    raise ECachedBuffer.CreateFmt('Cannot open file:'#13'%s', [AFileName]);

  InternalCreate(ASize, (AOffset = 0));
end;

constructor TCachedFileReader.CreateHandled(const AHandle: THandle;
  const ASize: Int64; const AHandleOwner: Boolean);
begin
  FHandle := AHandle;
  FHandleOwner := AHandleOwner;
  FOffset := FileSeek(FHandle, Int64(0), 1{soFromCurrent});
  if (FHandle = INVALID_HANDLE_VALUE) or (FOffset < 0) then
    raise ECachedBuffer.Create('Invalid file handle');

  InternalCreate(ASize, True);
end;

procedure TCachedFileReader.InternalCreate(const ASize: Int64; const ASeeked: Boolean);
var
  LFileSize: Int64;
begin
  LFileSize := GetFileSize(FHandle);

  if (FOffset < 0) or (FOffset > LFileSize) or
    ((not ASeeked) and (FOffset <> FileSeek(FHandle, FOffset, 0{soFromBeginning}))) then
    raise ECachedBuffer.CreateFmt('Invalid offset %d in %d bytes file'#13'%s',
      [FOffset, LFileSize, FFileName]);

  LFileSize := LFileSize - Offset;
  if (LFileSize = 0) then
  begin
    FKind := cbReader; // inherited Create;
    EOF := True;
  end else
  begin
    if (ASize > 0) and (ASize < LFileSize) then LFileSize := ASize;

    inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, LFileSize));
    Limit := LFileSize;
  end;
end;

destructor TCachedFileReader.Destroy;
begin
  inherited;

  if (FHandleOwner) and (FHandle <> 0) and
    (FHandle <> INVALID_HANDLE_VALUE) then FileClose(FHandle);
end;

function TCachedFileReader.CheckLimit(const AValue: Int64): Boolean;
begin
  Result := (AValue <= (GetFileSize(FHandle) - FOffset));
end;

function TCachedFileReader.InternalCallback(const ASender: TCachedBuffer; AData: PByte;
  ASize: NativeUInt): NativeUInt;
var
  LCount, LReadingSize: Integer;
begin
  Result := 0;

  repeat
    if (ASize > NativeUInt(High(Integer))) then LReadingSize := High(Integer)
    else LReadingSize := ASize;

    LCount := FileRead(FHandle, AData^, LReadingSize);
    if (LCount < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};

    Inc(AData, LCount);
    Dec(ASize, LCount);
    Inc(Result, LCount);
  until (LCount <> LReadingSize) or (ASize = 0);
end;

function TCachedFileReader.DoDirectPreviousRead(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, APosition, AData, ASize);
end;

function TCachedFileReader.DoDirectFollowingRead(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, APosition, AData, ASize);
end;


{ TCachedFileWriter }

constructor TCachedFileWriter.Create(const AFileName: string; const ASize: Int64);
var
  LHandle: THandle;
begin
  FFileName := AFileName;
  {$ifdef MSWINDOWS}
  LHandle := {$ifdef UNITSCOPENAMES}Winapi.{$endif}Windows.{$ifdef UNICODE}CreateFileW{$else}CreateFile{$endif}
    (PChar(AFileName), $0002{FILE_WRITE_DATA}, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0);
  {$else}
  LHandle := FileCreate(AFileName);
  {$endif}
  if (LHandle = INVALID_HANDLE_VALUE) then
    raise ECachedBuffer.CreateFmt('Cannot create file:'#13'%s', [AFileName]);

  CreateHandled(LHandle, ASize, True);
end;

constructor TCachedFileWriter.CreateHandled(const AHandle: THandle;
  const ASize: Int64; const AHandleOwner: Boolean);
begin
  FHandle := AHandle;
  FHandleOwner := AHandleOwner;
  FOffset := FileSeek(FHandle, Int64(0), 1{soFromCurrent});
  if (FHandle = INVALID_HANDLE_VALUE) or (FOffset < 0) then
    raise ECachedBuffer.Create('Invalid file handle');

  inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, ASize));
  if (ASize > 0) then
    Limit := ASize;
end;

destructor TCachedFileWriter.Destroy;
begin
  inherited;

  if (FHandleOwner) and (FHandle <> 0) and
    (FHandle <> INVALID_HANDLE_VALUE) then FileClose(FHandle);
end;

function TCachedFileWriter.InternalCallback(const ASender: TCachedBuffer; AData: PByte;
  ASize: NativeUInt): NativeUInt;
var
  LCount, LWritingSize: Integer;
begin
  Result := 0;

  repeat
    if (ASize > NativeUInt(High(Integer))) then LWritingSize := High(Integer)
    else LWritingSize := ASize;

    LCount := FileWrite(FHandle, AData^, LWritingSize);
    if (LCount < 0) then {$ifdef KOL}RaiseLastWin32Error{$else}RaiseLastOSError{$endif};

    Inc(AData, LCount);
    Dec(ASize, LCount);
    Inc(Result, LCount);
  until (LCount <> LWritingSize) or (ASize = 0);
end;

function TCachedFileWriter.DoDirectPreviousWrite(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, APosition, AData, ASize);
end;

function TCachedFileWriter.DoDirectFollowingWrite(APosition: Int64; AData: PByte;
  ASize: NativeUInt): Boolean;
begin
  Result := DirectCachedFileMethod(Self, FHandle, FOffset, APosition, AData, ASize);
end;


{ TCachedMemoryReader }

constructor TCachedMemoryReader.Create(const APtr: Pointer;
  const ASize: NativeUInt; const AFixed: Boolean);
begin
  FPtr := APtr;
  FSize := ASize;
  FPtrMargin := ASize;

  if (APtr = nil) or (ASize = 0) then
  begin
    FKind := cbReader; // inherited Create;
    EOF := True;
  end else
  if (AFixed) then
  begin
    FKind := cbReader;
    FCallback := FixedCallback;
    FMemory.Data := APtr;
    FMemory.Size := ASize;
    FMemory.Additional := Pointer(NativeUInt(APtr) + ASize);
    Current := APtr;
    FOverflow := FMemory.Additional;
    FStart := APtr;
    FFinishing := True;
    Limit := ASize;
  end else
  begin
    inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, ASize));
    Limit := ASize;
  end;
end;

function TCachedMemoryReader.CheckLimit(const AValue: Int64): Boolean;
begin
  Result := (AValue <= Size);
end;

function TCachedMemoryReader.InternalCallback(const ASender: TCachedBuffer; AData: PByte;
  ASize: NativeUInt): NativeUInt;
begin
  Result := ASize;
  if (Result > FPtrMargin) then Result := FPtrMargin;

  NcMove(Pointer(NativeUInt(FPtr) + Self.FSize - FPtrMargin)^, AData^, Result);
  Dec(FPtrMargin, Result);
end;

function TCachedMemoryReader.FixedCallback(const ASender: TCachedBuffer; AData: PByte;
  ASize: NativeUInt): NativeUInt;
begin
  RaiseReading;
  Result := 0;
end;


{ TCachedMemoryWriter }

constructor TCachedMemoryWriter.Create(const APtr: Pointer;
  const ASize: NativeUInt; const AFixed: Boolean);
begin
  FPtr := APtr;
  FSize := ASize;
  FPtrMargin := ASize;

  if (APtr = nil) or (ASize = 0) then
  begin
    FKind := cbWriter; // inherited Create;
    EOF := True;
  end else
  if (AFixed) then
  begin
    FKind := cbWriter;
    FCallback := FixedCallback;
    FMemory.Data := APtr;
    FMemory.Size := ASize;
    FMemory.Additional := Pointer(NativeUInt(APtr) + ASize);
    Current := APtr;
    FOverflow := FMemory.Additional;
    FHighWritten := Current;
    FStart := APtr;
    Limit := ASize;
  end else
  begin
    inherited Create(InternalCallback, GetOptimalBufferSize(0, DEFAULT_CACHED_SIZE, ASize));
    Limit := ASize;
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

function TCachedMemoryWriter.CheckLimit(const AValue: Int64): Boolean;
begin
  if (not FTemporary) then
  begin
    Result := (AValue <= Size);
  end else
  begin
    Result := True;
  end;
end;

function TCachedMemoryWriter.InternalCallback(const ASender: TCachedBuffer;
  AData: PByte; ASize: NativeUInt): NativeUInt;
begin
  Result := ASize;
  if (Result > FPtrMargin) then Result := FPtrMargin;

  NcMove(AData^, Pointer(NativeUInt(FPtr) + Self.FSize - FPtrMargin)^, Result);
  Dec(FPtrMargin, Result);
end;

function TCachedMemoryWriter.InternalTemporaryCallback(const ASender: TCachedBuffer;
  AData: PByte; ASize: NativeUInt): NativeUInt;
var
  LNewPtrSize: NativeUInt;
begin
  if (ASize <> 0) then
  begin
    LNewPtrSize := Self.FSize + ASize;
    Self.FSize := LNewPtrSize;
    ReallocMem(FPtr, LNewPtrSize);

    NcMove(AData^, Pointer(NativeUInt(FPtr) + LNewPtrSize - ASize)^, ASize);
  end;

  Result := ASize;
end;

function TCachedMemoryWriter.FixedCallback(const ASender: TCachedBuffer; AData: PByte;
  ASize: NativeUInt): NativeUInt;
begin
  Result := ASize;
end;


{ TCachedResourceReader }

{$ifdef FPC}
function FindResource(AModuleHandle: TFPResourceHMODULE; AResourceName, AResourceType: PChar): TFPResourceHandle;
{$ifdef MSWINDOWS}
begin
  Result := Windows.FindResourceW(AModuleHandle, AResourceName, AResourceType);
end;
{$else}
var
  LBufferString: string;
  LBufferName, LBufferType: UTF8String;
  LResourceName, LResourceType: PAnsiChar;
begin
  LResourceName := Pointer(AResourceName);
  if (NativeUInt(AResourceName) <= High(Word)) then
  begin
    LBufferString := AResourceName;
    LBufferName := UTF8String(LBufferString);
    LResourceName := PAnsiChar(LBufferName);
  end;

  LResourceType := Pointer(AResourceType);
  if (NativeUInt(AResourceType) <= High(Word)) then
  begin
    LBufferString := AResourceType;
    LBufferType := UTF8String(LBufferString);
    LResourceType := PAnsiChar(LBufferType);
  end;

  Result := System.FindResource(AModuleHandle, LResourceName, LResourceType);
end;
{$endif}
{$endif}

procedure TCachedResourceReader.InternalCreate(AInstance: THandle; AName,
  AResType: PChar; AFixed: Boolean);

  procedure RaiseNotFound;
  var
    V: NativeUInt;
    N, T: string;
  begin
    V := NativeUInt(AName);
    if (V <= High(Word)) then N := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else N := '"' + string(AName) + '"';

    V := NativeUInt(AResType);
    if (V <= High(Word)) then T := '#' + string({$ifdef KOL}Int2Str{$else}IntToStr{$endif}(Integer(V)))
    else T := '"' + string(AResType) + '"';

    raise ECachedBuffer.CreateFmt('Resource %s (%s) not found', [N, T]);
  end;

var
  HResInfo: THandle;
begin
  HResInfo := FindResource(AInstance, AName, AResType);
  if (HResInfo = 0) then RaiseNotFound;
  HGlobal := LoadResource(AInstance, HResInfo);
  if (HGlobal = 0) then RaiseNotFound;
  inherited Create(LockResource(HGlobal), SizeOfResource(AInstance, HResInfo), AFixed);
end;

constructor TCachedResourceReader.Create(const AInstance: THandle;
  const AResName: string; const AResType: PChar; const AFixed: Boolean);
begin
  InternalCreate(AInstance, PChar(AResName), AResType, AFixed);
end;

constructor TCachedResourceReader.CreateFromID(const AInstance: THandle;
  const AResID: Word; const AResType: PChar; const AFixed: Boolean);
begin
  InternalCreate(AInstance, PChar(NativeUInt(AResID)), AResType, AFixed);
end;

destructor TCachedResourceReader.Destroy;
begin
  inherited;
  UnlockResource(HGlobal);
  FreeResource(HGlobal);
end;

{$ifdef CPUX86}
procedure CheckSSESupport; {$ifdef FPC}assembler; nostackframe;{$endif}
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

