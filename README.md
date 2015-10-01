# CachedBuffers
The library is irreplaceable for the tasks of sequential data reading or writing, especially if the requirements for the performance are increased and there are much data. The library is based on two main classes of the module CachedBuffers.pas:  `TCachedReader` and `TCachedWriter`. The functionality of these two classes largely repeats the standard `TStream` class, but the difference is that you are using a convenient temporary memory buffer. Such architecture makes it possible to give the direct access to the memory and to increase productivity avoid calling virtual functions, universal implementations and API features of operating systems. An example of usage of the library and the illustration of a high performance can be seen in the demonstration projects.

[Demo.zip]( http://dmozulyov.ucoz.net/CachedBuffers/Demo.zip)
![](http://dmozulyov.ucoz.net/CachedBuffers/ScreenShots.png)

The `TCachedReader` and `TCachedWriter` classes are supposed to indicate a callback in order to fulfill or commit a buffer.  However, the library already contains a several standard classes which help to perform distributed tasks: `TCachedFileReader`, `TCachedFileWriter`, `TCachedMemoryReader`, `TCachedMemoryWriter` and `TCachedResourceReader`. In the future there also may be standard classes for the network interaction.

The library also contains `TCachedReReader` and `TCachedReWriter` classes which serve to be intermediate members. Those classes are necessary when the data is sequentially converted from one format to another. For example, if your parser receives data  (`TCachedReader`) in the UTF-16LE encoding and an open file (`TCachedFileReader`) is written in another encoding, then you will need an intermediate member (`TCachedReReader`) processing the conversion into UTF-16LE. Another striking example of an intermediate member is the ZLib library data compression and decompression.

All the classes interacting among `TCachedReader`, `TCachedWriter` and `TStream` are located in CachedStreams.pas module. The `TCachedStreamReader` class (the heir of the `TCachedReader`) can read from `TStream` instance. The `TCachedStreamWriter` (the heir of the `TCachedWriter`) can write to `TStream` instance. The `TCachedBufferStream` (the heir of the `TStream`) uses `TCachedReader` or `TCachedWriter`. The `TCachedBufferAdapter` support the OLE interface `IStream` for the interaction with  the `TCachedReader` or `TCachedWriter`.

As a bonus there is an `NcMove` function accessible in the CachedBuffers.pas module which on the average is two times faster than the standard `Move` but which is designed for the non-overlapping memory areas.

##### TCachedBuffer
The `TCachedBuffer` is a mutual ancestor for the `TCachedReader` and the `TCachedWriter`. The main class property is the `Memory`. It is a allocated memory buffer aligned to 4KB which also has `Previous` и `Additional` areas (the description will be given below). The size of the buffer is set in a constructor where the default value is 64KB. If the size is not multiple of 4KB then the size automatically align (e.g. 5000 aligns to 8192).
![](http://dmozulyov.ucoz.net/CachedBuffers/MemoryScheme.png)

To fill or read the buffer is used `Current`/`Overflow` pair. `Current` indicates the position in a buffer and allows the direct access to the memory. `Overflow` is an upper limit of the buffer. `Margin` defines the number of bytes accessible in a buffer (`Overflow - Current`). The `Flush` function updates the buffer and returns the number of bytes accessible in a buffer (`Margin`). The function may return 0 if the reading or writing is over. 

The `EOF` is an indication of the end of the reading or writing, which also may be set to `True`. If needed it is possible to limit the reading or writing size by setting the `Limit` value.
```pascal
TCachedBufferKind = (cbReader, cbWriter);
TCachedBufferCallback = function(Sender: TCachedBuffer; Data: PByte; Size: NativeUInt): NativeUInt of object;
TCachedBufferProgress = procedure(Sender: TCachedBuffer; var Cancel: Boolean) of object;

TCachedBuffer = class
public
  Current: PByte;
  function Flush: NativeUInt;    
  property Kind: TCachedBufferKind read
  property Overflow: PByte read
  property Margin: NativeInt read
  property EOF: Boolean read/write
  property Limited: Boolean read
  property Limit: Int64 read/write
  property Memory: TCachedBufferMemory read
  property Position: Int64 read 
  property OnProgress: TCachedBufferProgress read/write
end;
```
##### TCachedReader
The `TCachedReader` is used for the sequential data reading. The direct use of a buffer is allowed with the `Current`, `Overflow` properties and `Flush` function but for convenience sake you may use high-level `TStream`-like methods `Read` and `ReadData`. The `Skip` method allows to pass through a certain number of useless bytes. The `Export` method allows to write whole or a certain number of data bytes in the `TCachedWriter`.

The main constructor’s parameter is a `Callback`. It is a function which fills the buffer. During reading much data the `Callback` may be called passing the buffer. If the `Callback` returns less than `Size` parameter then the `Finishing` flag is set to `True` and at the next `Flush` the reading will be over (`EOF = True`).

The `Memory.Additional` (`Overflow`) area is always bigger than 4KB and may also serve for any reading needs the main of which is an AccessViolation avoidance with specific algorithms of data parsing. At the `Flush` moment the `Current` pointer may be less than `Overflow`. In this case the margin data is transferred to the `Memory.Previous` area. For example it is useful when the algorithm offers the struct processing and it is not fully stored in the buffer. The same is with lexeme parsing. The size of `Memory.Previous` area is minimum 4KB and when necessary it automatically grows during the `Flush` moment. But if the `Current` pointer is bigger than `Overflow` an exception will be thrown.

The `DirectRead` procedure allows data reading from a random place even without the buffer memory.  Some instances of the `TCachedReader` allow addressing to random places (e.g. files or memory). If the addressing to random places is not supported, better be careful. While reading "before" the buffer an exception will throw. While reading "after" the buffer, it will be enlarged to the necessary size what can lead to memory shortage.
```pascal
TCachedReader = class(TCachedBuffer)
public
  constructor Create(const Callback: TCachedBufferCallback; const BufferSize: NativeUInt = 0);
  procedure DirectRead(const Position: Int64; var Buffer; const Count: NativeUInt);
  property Finishing: Boolean read
  procedure Skip(const Count: NativeUInt);
  procedure Export(const Writer: TCachedWriter; const Count: NativeUInt = 0);

  procedure Read(var Buffer; const Count: NativeUInt);
  procedure ReadData(var Value: Boolean);
  procedure ReadData(var Value: AnsiChar);
  procedure ReadData(var Value: WideChar);
  procedure ReadData(var Value: ShortInt);
  procedure ReadData(var Value: Byte);
  procedure ReadData(var Value: SmallInt);
  procedure ReadData(var Value: Word);
  procedure ReadData(var Value: Integer); 
  procedure ReadData(var Value: Cardinal);
  procedure ReadData(var Value: Int64); 
  procedure ReadData(var Value: UInt64);
  procedure ReadData(var Value: Single);
  procedure ReadData(var Value: Double);
  procedure ReadData(var Value: TExtended80Rec);
  procedure ReadData(var Value: Currency);
  procedure ReadData(var Value: TPoint);
  procedure ReadData(var Value: TRect); 
  procedure ReadData(var Value: ShortString); 
  procedure ReadData(var Value: AnsiString; CodePage: Word = 0);
  procedure ReadData(var Value: WideString);
  procedure ReadData(var Value: UnicodeString);
  procedure ReadData(var Value: TBytes); 
  procedure ReadData(var Value: Variant);
end;
```
##### TCachedWriter
The `TCachedWriter` is used for the sequential data writing. The class has only several differences from the `TCachedReader`.

The size of the `Memory.Previous` area may be equal 0 that is why do not write anything “before” the buffer. The size of the `Memory.Additional` (`Overflow`) is minimum 4KB. A writing in this area is permitted (`Current` is bigger than `Overflow`) and then at the next `Flush` an overflowed data will be transferred to the beginning of the buffer. If while `Flush` the `Current` pointer is less than `Overflow` then the writing is considered to be over and the `EOF` is set to `True`.
```pascal
TCachedWriter = class(TCachedBuffer)
public
  constructor Create(const Callback: TCachedBufferCallback; const BufferSize: NativeUInt = 0);
  procedure DirectWrite(const Position: Int64; const Buffer; const Count: NativeUInt);
  procedure Import(const Reader: TCachedReader; const Count: NativeUInt = 0);

  procedure Write(const Buffer; const Count: NativeUInt);
  procedure WriteData(const Value: Boolean);    
  procedure WriteData(const Value: AnsiChar);    
  procedure WriteData(const Value: WideChar);
  procedure WriteData(const Value: ShortInt);
  procedure WriteData(const Value: Byte); 
  procedure WriteData(const Value: SmallInt);
  procedure WriteData(const Value: Word);
  procedure WriteData(const Value: Integer); 
  procedure WriteData(const Value: Cardinal);
  procedure WriteData(const Value: Int64);    
  procedure WriteData(const Value: UInt64);    
  procedure WriteData(const Value: Single);
  procedure WriteData(const Value: Double);
  procedure WriteData(const Value: TExtended80Rec);
  procedure WriteData(const Value: Currency);
  procedure WriteData(const Value: TPoint);
  procedure WriteData(const Value: TRect); 
  procedure WriteData(const Value: ShortString);
  procedure WriteData(const Value: AnsiString); 
  procedure WriteData(const Value: WideString); 
  procedure WriteData(const Value: UnicodeString);
  procedure WriteData(const Value: TBytes); 
  procedure WriteData(const Value: Variant);
end;
```
##### TCachedReReader
The `TCachedReReader` is an intermediate member in reading tasks where a sequential data converting from one format to another is required. If during "destructor" the `Owner` is `True` then the `Source` is destroyed too. **Pay attention** to the fact that while calling the `Callback` the `TCachedReReader` itself is a `Sender` parameter but not the `Source`.
```pascal
TCachedReReader = class(TCachedReader)  
public
  constructor Create(const Callback: TCachedBufferCallback; const Source: TCachedReader; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);
  property Source: TCachedReader read
  property Owner: Boolean read/write
end;
```
##### TCachedReWriter
The `TCachedReWriter` is an intermediate member in writing tasks where a sequential data converting from one format to another is required. If during "destructor" the `Owner` is `True` then the `Target` is destroyed too. **Pay attention** to the fact that while calling the `Callback` the `TCachedReWriter` itself is a `Sender` parameter but not the `Target`.
```pascal
TCachedReWriter = class(TCachedWriter)
public
  constructor Create(const Callback: TCachedBufferCallback; const Target: TCachedWriter; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);
  property Target: TCachedWriter read
  property Owner: Boolean read/write  
end;
```
##### TCachedFileReader
The `TCachedFileReader` is a standard class designed for files reading of their parts. A standard buffer size equals 256KB. The `Limit` property is automatically set and equals the size of the file or the `Size` parameter.
```pascal
TCachedFileReader = class(TCachedReader)
public
  constructor Create(const FileName: string; const Offset: Int64 = 0; const Size: Int64 = 0);
  constructor CreateHandled(const Handle: THandle; const Size: Int64 = 0; const HandleOwner: Boolean = False);
  property FileName: string read
  property Handle: THandle read
  property HandleOwner: Boolean read/write
  property Offset: Int64 read
end;
```
##### TCachedFileWriter
The `TCachedFileWriter` is a standard class designed for files writing or their parts. A standard buffer size equals 256KB. The `Limit` parameter is set if the `Size` parameter is defined.
```pascal
TCachedFileWriter = class(TCachedWriter)
public
  constructor Create(const FileName: string; const Size: Int64 = 0); 
  constructor CreateHandled(const Handle: THandle; const Size: Int64 = 0; const HandleOwner: Boolean = False);
  property FileName: string read
  property Handle: THandle read
  property HandleOwner: Boolean read/write
  property Offset: Int64 read
end;
```
##### TCachedMemoryReader
`TCachedMemoryReader` is a standard class designed for compatibility of the `TCachedReader` with the reading from a fixed memory area.  The `Limit` property is automatically set and it equals `Size`.
```pascal
TCachedMemoryReader = class(TCachedReader)
public
  constructor Create(const Ptr: Pointer; const Size: NativeUInt);
  property Ptr: Pointer read
  property Size: NativeUInt read
end;
```
##### TCachedMemoryWriter
`TCachedMemoryWriter` is a standard class designed for compatibility of the `TCachedWriter` with the writing to the temporary or fixed memory area. If it is a `CreateTemporary` then the `Ptr` memory resizes with every `Flush` calling.
```pascal
TCachedMemoryWriter = class(TCachedWriter)
public
  constructor Create(const Ptr: Pointer; const Size: NativeUInt);
  constructor CreateTemporary;
  property Temporary: Boolean read
  property Ptr: Pointer read
  property Size: NativeUInt read
end;
```
##### TCachedResourceReader
`TCachedResourceReader` is a standard class designed for compatibility of the `TCachedReader` with the reading of the resources. It repeats interface of the `Classes.TResourceStream`. It is accessible only on Windows platform.
```pascal
TCachedResourceReader = class(TCachedMemoryReader)
public
  constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
  constructor CreateFromID(Instance: THandle; ResID: Word; ResType: PChar);
end;
```
