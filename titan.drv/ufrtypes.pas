unit UfrTypes;

{$ifdef FPC}
  {$mode DELPHI}
{$endif}

interface

uses
  Windows;

const

  //stPayNotEqualToPrice = 'Payment is not equal to the goods price';
  errOk = 0;                   // без ошибок

  { универсальные ошибки инициализации 1-99 }
  errPortAlreadyUsed      = 1; // порт уже используется
  errIllegalOS            = 2; // не та OS
  errProtocolNotSupported = 3; // запрашиваемый протокол  не поддерживается
  errFunctionNotSupported = 4; // функция драйвера не поддерживается данным фискальником
  errInvalidHandle        = 5; // Недействительный дескриптор (handle)

  { универсальные ошибки низкого уровня 100-199 }
  errLowNotReady          = 101; // устройство не готово принять команду, таймаут ожидания
  errLowSendError         = 102; // устройство отвечает ошибкой приёма команды
  errLowAnswerTimeout     = 103; // устройство не отвечает на команду
  errLowInactiveOnExec    = 104; // устройство не отвечает на проверку работоспособности после отправки команды
  errLowBadAnswer         = 105; // устройство отвечает мусором (и невозможно повторить или повтор не помог)

  { логические ошибки 200-299 }
  errLogicError           = 200; // логическая ошибка прочего типа, см. статус: LogicError, LogicErrorText
  errLogic24hour          = 201; // смена превысила максимальную продолжительность
  errLogicPrinterNotReady = 202;
  errLogicPaperOut        = 203; // закончилась бумага во время печати.
                          //При запросе статуса не надо возвращать эту ошибку.

  { Ошибки во входных данных, обнаруженные до отправки данных в ФР 300-399 }
  errAssertItemsPaysDifferent             = 301; // В чеке не совпадают суммы по товарам и платежам
  errAssertInvalidXMLInitializationParams = 302; // ошибки в XML параметрах UFRInit
  errAssertInvalidXMLParams               = 303; // ошибки в XML параметрах всех функций, кроме UFRInit
  errAssertInsufficientBufferSize         = 304; // недостаточный размер буфера для получения данных

  { UFRGetOptions }
  FoText             = $0001; // ФР поддерживает отдельную нефискальную печать (не в чеке)
  foZeroReceipt      = $0002; // ФР поддерживает печать нулевого чека(с нулевой стоимостью)
  foDeleteReceipt    = $0004; // ФР поддерживает удаление чека
  foZReport          = $0008; // ФР поддерживает Z отчёт
  foMoneyInOut       = $0010; // ФР поддерживает внесения-выдачи
  foXReport          = $0020; // ФР поддерживает X отчёт
  foSpecialReport    = $0040; // ФР поддерживает специальные отчёты
  foZeroSale         = $0080; // ФР поддерживает продажи с нулевой ценой
  foProgram          = $0100; // ФР поддерживает программирование
  foFullLastShift    = $0200; // ФР поддерживает распечатку последней смены
  foAllMoneyOut      = $0400; // ФР поддерживает изъятие всех денег
  foTextInReceipt    = $0800; // ФР поддерживает нефискальную печать внутри чека (внутри <Header>)
  foBarCodeInNotFisc = $1000; // ФР поддерживает печать штрих-кода в нефискальной части чека
  foZClearMoney      = $2000; // Z отчёт автоматически очищает остаток денег в кассе
  foCheckCopy        = $4000; // фискальный документ - копия чека (пока только в Латвии)
  foTextInLine       = $8000; // ФР поддерживает нефискальную печать внутри линии чека (внутри <Item> или <Payment> или <Discount>)
  foDepartments      = $00010000; // ФР поддерживает отделы по блюдам
  foOnlyFixed        = $00020000; // работа только с заранее запрограммированными блюдами (RK7 в этом случае использует блюда "итого" по отделам с кодами отделов и распечатывает "суммарные" чеки с такими блюдами)
  foTextOpenShift    = $00040000; // признак, что нефискальная печать открывает фискальную смену
  foDrawerOpen       = $00080000; // Может открывать ящик
  foDrawerState      = $00100000; // Может возвращать состояние ящика
  foCustomerDisplay  = $00200000; // Поддерживает вывод на дисплей покупателя
  foCalcChange       = $00800000; // Поддерживает вычисление сдачи
  foZWhenClosedShift = $01000000; // Поддерживает печать Z-отчета при закрытой смене
  foAbsDiscountSum   = $02000000; // ФР поддерживает абсолютные суммовые скидки/наценки
  foFiscInvoice      = $04000000; // Поддерживает печать фискального чека как счет-фактуру
  foCashRegValue     = $08000000; // ФР поддерживает возврат значения регистра кассовой наличности
  foFixedNames       = $10000000; // ФР требует неизменности имён позиций в течении дня

  { значения маски FieldsNeeded для UFRGetStatus }
  fnNotCancelReceipt = $0001; // не отменять автоматически открытый чек
  fnBusy             = $0002; // требуется заполнить Busy и NotReady
  fnUnfiscalPrint    = $0004; // требуется заполнить CanNotPrintUnfiscal
  fnPrintQueue       = $0008; // Длина очереди печати (для нефискальной печати)
  fnDrawerOpened     = $0010; // Признак открытого ящика
  fnShiftState       = $0100; // статус фискальной смены
  fnSerialNumber     = $0200; // строка-идентификатор фискальника, кроме серийного номера можно прописать закодированную модель фискальника
  fnLastShiftNum     = $0400; // номер последней закрытой(!) фискальной смены, если первая смена, то 0
  fnLastDocNum       = $0800; // последний номер документа (включая внесения-выдачи)
  fnLastReceiptNum   = $1000; // последний номер фискального чека, может совпадать с номером документа
  fnSaledValue       = $2000; // сумма продаж или за смену или глобально. Использовать только для проверки изменилась/не изменилась (прошёл или нет последний чек).
  //fnLogicError       = $4000; // Запрос последней логической ошибки.
  fnCashRegValue     = $8000; // Запрос значения регистра кассовой наличности в ФР


  { список функций Callback процедуры }
  cpfDialog            = 1; // вызов диалога, структура tDialogInfo, см. Вызов диалога на станции
  cpfProgress          = 2; // индикация прогресса, структура tProgressInfo, см. Иллюстрация прогресса
  cpfGetStringDialog   = 3; // диалог запроса строки, структура tGetStringInfo, см. Вызов диалога запроса строки
  cpfRegisterSpecMenu  = 4; // зарегистрировать специальное меню, структура tDriverMenuInfo см.Дополнительные функции (спец. меню, общее с другими драйверам)
  cpfLog               = 5; // логирование

  { для логов }
  letError               = 1; // Ошибка.
  letInitializing        = 2; // В начале инициализации. Data - строка в кодировке UTF8
  letInitialized         = 3; // При успешной инициализации. Data - строка в кодировке UTF8
  letFiscCommand         = 4; // Текстовое и бинарное представление фискальной команды (в тексте номер, возможно, описание, возможно, параметр, в бинарном представлении пакет данных для отправки фискальнику).
  letFiscCommandComplete = 5; // Текстовое и бинарное представление ответа фискальника. В тексте может быть код ошибки, время выполнения фискальной команды, возможно, расшифровка содержательного ответа фискальника, например, на запрос статуса. В бинарном представлении пакет данных, полученный от фискальника.
  letTextCommand         = 6; // Текстовое и бинарное представление команды вывода текста (нефискальная печать, вывод на дисплей покупателя), в бинарном представлении пакет данных для отправки фискальнику.
  letTextCommandComplete = 7; // Текстовое и бинарное представление ответа фискальника. В тексте может быть код ошибки, время выполнения фискальной команды, возможно, расшифровка содержательного ответа фискальника, например, статус печатающего стройства. В бинарном представлении пакет данных, полученный от фискальника.
  letBinInput            = 8; // Бинарный системный вход (не описанный в letFiscCommandComplete, letTextCommandComplete). Например, подтверждение фискальником о получении пакета данных.
  letBinOut              = 9; // Бинарный системный вывод (не описанный в letFiscCommand, letTextCommand). Например, подтверждение о получении пакета данных или подготовка фискальника к отправке команды.
  letOther               = 10; // все остальное

type
  TInterfaceCallbackProc = procedure(ModuleHandle: THandle; {хэнд модуля, который вызывает процедуру}
                                     Number: LongInt; {номер, который передали при регистрации}
                                     FuncID, ProtocolVersion: LongInt;
                                     FuncParams: Pointer {указатель на параметр, возможно структура, часть полей которой могут меняться(возврат)}); stdcall;

  TPropCallbackProc = procedure(XMLTag: PChar; // тип элемента, совпадает с тэгом, Для "Receipt"/"Receipt.Order"/"Receipt.Deletion"/"Receipt.Operator" идентификаторы не
                                               // нужны, для "Item""/"Discount"/"Pay" нужны (аттрибут Id)
                                Identifier: PChar; // Идентификатор элемента
                                PropName: PChar; // Имя свойства
                            var PropValue: OpenString); stdcall;

  TShiftState = (ssShiftClosed, ssShiftOpened, ssShiftOpened24hoursExceeded);

  TUFRStatus = packed record
    Size:                LongInt;     // SizeOf(TUFRStatus)

    NotReady:            Boolean;     // Не готов
    Busy:                Boolean;     // Работает [занят]
    CannotPrintUnfiscal: Boolean;     // Невозможно выполнить нефискальную печать
    QueueSize:           LongInt;     // Размер очереди для нефискальной печати
                                      // NOTE. По возможности, возвращать сумму
                                      //      длин очередей драйвера и устройства
    DrawerOpened:        Boolean;     // Ящик открыт
    ShiftState:          TShiftState; // Фискальная смена закрыта/открыта/открыта и прошло 24 часа
    SerialNum:           String[35];  // Уникальный идентификатор ФР (с моделью)
    LastShiftNum:        Word;        // Последний номер Z отчёта
                                      // (номер закрытой смены)
    LastDocNum:          LongInt;     // Последний номер документа
                                      // (в том числе инкассации и т.п.)
    LastReceiptNum:      LongInt;     // Последний номер чека
    SaledValue:          Int64;       // Сумма продаж за смену, в копейках
    CashRegValue:        Int64;       // Сумма в кассе, в копейках
  end;

  TFRLog = Packed Record
    Size: LongInt;         // sizeof(tFRLog)
    LogEventType: LongInt; // одна из констант, описанных ниже
    TextData: PChar;       // строка в кодировке UTF8, заканчивающаяся 0
    BinData: Pointer;
    BinDataSize: LongInt;
  end;

  TUFRLogicError = packed record
    Size:                LongInt;     // SizeOf(TUFRLogicError)
    LogicError:          LongInt;     // Внутренний код логической ошибки ФР
    LogicErrorText:      ShortString; // Описание логической ошибки ФР (если есть)
  end;

type
  TUFRMaxProtocolSupported = function(): Integer; stdcall;
  TUFRInit = function (ANumber: Integer;
                       AXMLParams: PChar;
                       AInterfaceCallback: TInterfaceCallbackProc;
                       APropCallback: TPropCallbackProc): Integer; stdcall;
  TUFRDone = procedure (ANumber: Integer); stdcall;
  TUFRGetOptions = function (ANumber: Integer;
                             var AOptions: Int64;
                             var ADriverName: OpenString;
                             var AVersionInfo: OpenString;
                             var ADriverState: OpenString): Integer; stdcall;
  TUFRGetStatus = function (ANumber: Integer;
                            var AStatus: TUFRStatus;
                            AFieldsNeeded: Cardinal): Integer; stdcall;
  TUFRUnfiscalPrint = function (ANumber: Integer;
                                AXMLBuffer: PChar;
                                AFlags: Integer): Integer; stdcall;
  TUFRFiscalDocument = function (ANumber: Integer;
                                 AXMLDoc: PChar;
                                 var AStatus: TUFRStatus;
                                 var AFieldsFilled: Cardinal): Integer; stdcall;
  TUFRGetZReportData = function (ANumber: Integer;
                                 AXMLData: PChar;
                                 var AXMLDataSize: Integer): Integer; stdcall;
  TUFROpenDrawer = function (ANumber: Integer; ADrawerNum: Integer): Integer; stdcall;
  TUFRCustomerDisplay = function (ANumber: Integer;
                                  AXMLBuffer: PChar;
                                  AFlags: Integer): Integer; stdcall;
  TUFRProgram = function (ANumber: Integer; AXMLDoc: PChar): Integer; stdcall;
  TUFRGetLastLogicError = procedure (ANumber: Integer; var ALogicError: TUFRLogicError); stdcall;

//псевдопроцедура, её не надо вызывать, достаточно написать вызов где то в невыполняемом месте
//в итоге проверятся все типы
procedure CheckFiscRegFuncTypes(
    fUFRMaxProtocolSupported: TUFRMaxProtocolSupported;
    fUFRInit: TUFRInit;
    fUFRDone: TUFRDone;
    fUFRGetOptions: TUFRGetOptions;
    fUFRGetStatus: TUFRGetStatus;
    fUFRUnfiscalPrint: TUFRUnfiscalPrint;
    fUFRFiscalDocument: TUFRFiscalDocument;
    fUFRGetZReportData: TUFRGetZReportData;
    fUFROpenDrawer: TUFROpenDrawer;
    fUFRCustomerDisplay: TUFRCustomerDisplay;
    fUFRProgram: TUFRProgram;
    fUFRGetLastLogicError: TUFRGetLastLogicError
  );

implementation

procedure CheckFiscRegFuncTypes();
begin
end;

end.
