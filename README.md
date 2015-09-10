# CachedBuffers
Библиотека незаменима для задач последовательного (пер. “sequential”) чтения или записи данных, особенно если требования к производительности повышены и данных много. Основу библиотеки составляют 2 главных класса модуля CachedBuffers.pas: `TCachedReader` и `TCachedWriter`. Функционал этих классов во многом повторяет стандартный класс `TStream`, однако отличие состоит в том, что используется удобный временный (пер. “temporary”) буфер памяти. Данная архитектура позволяет осуществлять прямой (пер. “direct”) доступ к памяти и увеличить производительность за счёт обхода виртуальных функций (пер. “virtual functions”), универсальных реализаций (пер. “universal implementations”) и особенностей API операционных систем. Пример использования библиотеки и иллюстрацию высокой производительности можно увидеть в демонстрационных проектах.
![](http://dmozulyov.ucoz.net/CachedBuffers/screenshots.png)

Классы `TCachedReader` и `TCachedWriter` предполагают указание калбека (пер. “callback”) для заполнения или сохранения буфера. Однако библиотека уже содержит несколько стандартных классов, помогающих решать распространённые задачи: `TCachedFileReader`, `TCachedFileWriter`, `TCachedMemoryReader`, `TCachedMemoryWriter` и `TCachedResourceReader`, в будущем возможно будут стандартные классы для взаимодействия с сетью (пер. “network”).

Библиотека содержит так же классы `TCachedReReader` и `TCachedReWriter`, выполняющие роль промежуточных звеньев. Необходимость в этих классах возникает тогда, когда данные последовательно (пер. “sequentially”) конвертируются из одного формата в другой. Например ваш парсер принимает данные (`TCachedReader`) в кодировке UTF-16LE, и если открытый файл (`TCachedFileReader`) записан в другой кодировке - потребуется промежуточное звено (`TCachedReReader`), выполняющее конвертацию в UTF-16LE. Другим ярким примером промежуточного звена является компрессия или декомпрессия данных библиотекой ZLib.

В модуле CachedStreams.pas находятся классы, осуществляющие взаимодействие между `TCachedReader`, `TCachedWriter` и `TStream`. Класс `TCachedStreamReader` - это наследник `TCachedReader`, читающий `TStream`. `TCachedStreamWriter` - наследник `TCachedWriter`, пишущий в `TStream`. `TCachedBufferStream` - наследник `TStream`, использующий `TCachedReader` или `TCachedWriter`. `TCachedBufferAdapter` поддерживает OLE interface `IStream` для взаимодействия с `TCachedReader` или `TCachedWriter`.

В качестве бонуса в модуле CachedBuffers.pas доступна функция `NcMove`, которая в среднем вдвое быстрее стардартной `Move`, но предназначена для непересекающихся областей памяти.

##### TCachedBuffer
`TCachedBuffer` это общий предок для `TCachedReader` и `TCachedWriter`. Главным свойством (пер. “property”) класса является `Memory` - выделенный буфер памяти, выровненный (пер. “aligned”) на 4kb, а так же имеющий `Previous` и `Additional` области, описание которых будет ниже. Размер буфера задаётся в конструкторе (пер. “constructor”), значение по-умолчанию (пер. “default value”) 64kb. Если размер не кратен 4kb, то размер автоматически выравнивается (например 5000 выровняется к 8192).
![](http://dmozulyov.ucoz.net/CachedBuffers/MemoryScheme.png)

Для заполнения или чтения буфера используется пара `Current`/`Overflow`. `Current` указывает позицию в буфере и позволяет осуществлять прямой доступ к памяти, `Overflow` - верхняя граница буфера. `Margin` определяет количество байт, доступных в буфере (`Overflow - Current`). Функция `Flush`обновляет буфер и возвращает количество байт, доступных в буфере (`Margin`). Функция может вернуть 0 если чтение или запись окончены.

Признаком окончания чтения или записи является свойство `EOF`, которое так же может устанавливаться в `true`. При желании можно ограничить размер чтения или записи, установив свойство `Limit`.
```
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
`TCachedReader` используется для последовательного (пер. “sequential”) чтения данных. Допускается прямое обращение с буфером при помощи свойств `Current`, `Overflow` и функции `Flush`, но для удобства можете воспользоваться высокоуровневыми (пер. “high-level”) `TStream`-like методами (пер. “method(s)”) `Read` и `ReadData`. Метод `Skip` позволяет пропустит определённое количество ненужных байт. Метод `Export` позволяет записать в `TCachedWriter` все или определённое количество байт данных.

Главным параметром конструктора (пер. “constructor”) является `Callback` - функция осуществляющая наполнение буфера. При чтении (`Read`) большого количества данных `Callback` может вызываться, минуя буфер. Если `Callback` возвращает меньше, чем указано в параметре `Size` - то флаг `Finishing` выставляется в `true` и при следуещем `Flush` чтение будет окончено (`EOF = true`).

Область `Memory.Additional` (`Overflow`) всегда больше 4kb и может служить для произвольных нужд чтения, главная из которох - избежание AccessViolation при специфичных алгоритмах парсинга данных. На момент `Flush` указатель `Current` может быть меньше `Overflow` - в этом случае остаток (пер. "margin data") переносится в область `Memory.Previous`. Это полезно например когда алгоритм предполагает обработку структуры (пер. "struct" или "data struct"), но в буфере она содержится не полностью. Аналогично при парсинге лексемы. Размер `Memory.Previous` области минимум 4kb и автоматически увеличивается при необходимости в момент `Flush`. Однако если указатель `Current` больше `Overflow` - будет вызвано исключение (пер. "throw (an) exception").

Функция `DirectRead` позволяет прочитать данные из произвольного места, даже минуя буфер. Некоторые экземпляры (пер. "instances") `TCachedReader` позволяют обращаться к произвольным частям, например файлы или участки памяти. Если обращение к произвольным частям не поддерживается - будьте осторожны. При чтении "до" буфера - возникнет исключение. При чтении "после" буфера - буфер будет увеличен до требуемого размера, что может привести к нехватке памяти.
```
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
`TCachedWriter` используется для последовательной (пер. “sequential”) записи данных. У класса всего несколько отличий от `TCachedReader`.

Размер `Memory.Previous` области может быть равен 0, поэтому не стоит писать что-либо "до" буфера. Размер `Memory.Additional` (`Overflow`) области минимум 4kb. Допускается запись в эту область (`Current` больше `Overflow`), тогда при следующем `Flush` остаток (пер. наверное “overflow(ed) data”) будет перемещён в начало буфера. Если на момент `Flush` указатель `Current` меньше `Overflow` - запись считается оконченной и `EOF` устанавливается в `true`.
```
TCachedWriter = class(TCachedBuffer)
public
  constructor Create(const Callback: TCachedBufferCallback; const BufferSize: NativeUInt = 0);
  procedure DirectWrite(const Position: Int64; const Buffer; const Count: NativeUInt);
  procedure Import(const Reader: TCachedReder; const Count: NativeUInt = 0);

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
`TCachedReReader` выступает в качестве промежуточного звена в задачах чтения, где требуется последовательное конвертирование данных из одного формата в другой. Если на момент деструктора (пер. "destructor") `Owner` выставлен в `true`, то `Source` тоже удаляется. **Обратите внимание**, в момент вызова `Callback` в качестве `Sender` выступает сам `TCachedReReader`, а не `Source`.
```
TCachedReReader = class(TCachedReader)  
public
  constructor Create(const Callback: TCachedBufferCallback; const Source: TCachedReader; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);
  property Source: TCachedReader read
  property Owner: Boolean read/write
end;
```
##### TCachedReWriter
`TCachedReWriter` выступает в качестве промежуточного звена в задачах записи, где требуется последовательное конвертирование данных из одного формата в другой. Если на момент деструктора (пер. "destructor") `Owner` выставлен в `true`, то `Target` тоже удаляется. **Обратите внимание**, в момент вызова `Callback` в качестве `Sender` выступает сам `TCachedReWriter`, а не `Target`.
```
TCachedReWriter = class(TCachedWriter)
public
  constructor Create(const Callback: TCachedBufferCallback; const Target: TCachedWriter; const Owner: Boolean = False; const BufferSize: NativeUInt = 0);
  property Target: TCachedWriter read
  property Owner: Boolean read/write  
end;
```
##### TCachedFileReader
`TCachedFileReader` - стандартный класс, предназначенный для чтения файлов или их части. Стандартный размер буфера равен 256kb. Свойство `Limit` уставливается (пер. "set") автоматически и равно размеру файла или указаному параметру `Size`.
```
TCachedFileReader = class(TCachedReader)
public
  constructor Create(const FileName: string; const Offset: Int64 = 0; const Size: Int64 = 0);
  constructor CreateHandled(const Handle: THandle; const Size: Int64 = 0; const HandleOwner: Boolean = False);
  property Handle: THandle read
  property HandleOwner: Boolean read/write
end;
```
##### TCachedFileWriter
`TCachedFileWriter` - стандартный класс, предназначенный для записи файлов или их части. Стандартный размер буфера равен 256kb. Свойство `Limit` уставливается (пер. "set") если указан параметр `Size`.
```
TCachedFileWriter = class(TCachedWriter)
public
  constructor Create(const FileName: string; const Size: Int64 = 0); 
  constructor CreateHandled(const Handle: THandle; const Size: Int64 = 0; const HandleOwner: Boolean = False);
  property Handle: THandle read
  property HandleOwner: Boolean read/write
end;
```
##### TCachedMemoryReader
`TCachedMemoryReader` - стандартный класс, предназначенный для совместимости с `TCachedReader` и чтения из заранее известного участка памяти. Свойство `Limit` автоматически уставливается (пер. "set") и равно `Size`.
```
TCachedMemoryReader = class(TCachedReader)
public
  constructor Create(const Ptr: Pointer; const Size: NativeUInt);
  property Ptr: Pointer read
end;
```
##### TCachedMemoryWriter
`TCachedMemoryWriter` - стандартный класс, предназначенный для совместимости с `TCachedWriter` и записи во временную память или разранее известный участок. В случае `CreateTemporary` память `Ptr` перевыделяется (пер. "resize(ing)") при каждом `Flush`.
```
TCachedMemoryWriter = class(TCachedWriter)
public
  constructor Create(const Ptr: Pointer; const Size: NativeUInt);
  constructor CreateTemporary;
  property Temporary: Boolean read
  property Ptr: Pointer read
end;
```
##### TCachedResourceReader
`TCachedResourceReader` - стандартный класс, предназначенный для совместимости с `TCachedReader` и чтения ресурсов, повторяет интерфейс `Classes.TResourceStream`. Доступен только под операционной системой Windows.
```
TCachedResourceReader = class(TCachedMemoryReader)
public
  constructor Create(Instance: THandle; const ResName: string; ResType: PChar);
  constructor CreateFromID(Instance: THandle; ResID: Word; ResType: PChar);
end;
```