{
  Драйвер фискального регистратора Titan-F
  Бодров Сергей (www.serbod.com)

  кодировка по умолчанию - CP-1251
}
unit TitanDrv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataStorage, synsock, blcksock;

const
  { типы отчетов с БЭП }
  FM_REPORT_FULL_BY_DATE  = 1;  // полный по датам
  FM_REPORT_FULL_BY_NUM   = 2;  // полный по номерам
  FM_REPORT_SHORT_BY_DATE = 3;  // сокращенный по датам
  FM_REPORT_SHORT_BY_NUM  = 4;  // сокращенный по номерам

  REPORT_Z1               = 0;  // дневной обнуляющий отчет (Z1)
  REPORT_CLEAN_LOG        = 1;  // обнуление электронного журнала
  REPORT_PRINT_CLEAN_LOG  = 2;  // печать контрольной ленты с обнулением
  REPORT_SKNO_REPORT      = 3;  // печать отчета состояния СКНО
  REPORT_PRINT_CLOSE_ORDERS = 4; // печать отчета по открытым ресторанным заказам с
                                 // автоматическим закрытием наличными
  REPORT_PRINT_ORDERS     = 5;  // печать отчета по открытым ресторанным заказам
  REPORT_X1               = 10; // (или 100) – дневной отчет без обнуление (X1)
  REPORT_SALES            = 20; // Отчет по проданным товарам
  REPORT_SALES_CLEAR      = 21; // Отчет по проданным товарам с обнулением этого отчета
  REPORT_BOX              = 102; // отчет по денежному ящику
  REPORT_SALES_BY_TIME    = 107; // статистика продаж по времени
  REPORT_SALES_BY_PERS    = 201; // 201..216 – статистика продаж по кассирам

  { Битовая маска состояния документа }
  DOC_STATE_OPEN_FIS      = $0001; // открыт фискальный чек
  DOC_STATE_OPEN_NONFIS   = $0002; // открыт не фискальный чек
  DOC_STATE_NEED_PAYMENT  = $0004; // для закрытия чека требуется оплата
  DOC_STATE_PRN_TOTALS    = $0008; // напечатаны итоги по чеку
  DOC_STATE_PRN_HEADER    = $0010; // напечатан заголовок чека
  DOC_STATE_HAS_GOODS     = $0020; // в чеке были продажи товара
  DOC_STATE_HAS_MONEY_IO  = $0040; // в чеке были вносы/выносы
  DOC_STATE_PAY_STARTED   = $0080; // начата расплата по чеку
  DOC_STATE_PRN_TAPE      = $0100; // печать контрольной ленты
  DOC_STATE_PRN_REPORT    = $0200; // печать служебного отчета
  DOC_STATE_CANCEL        = $0400; // отмена документа
  DOC_STATE_HAS_OPER      = $0800; // в чеке есть операции
  DOC_STATE_REFUND_START  = $1000; // начат чек возврата
  DOC_STATE_SAVED         = $2000; // запись занесена в ленту

  { Атрибуты нефискального комментария }
  TEXT_ATTR_NORM          = '';
  TEXT_ATTR_WIDE          = 'w';
  TEXT_ATTR_DOUBLE_HEIGHT = 'h';
  TEXT_ATTR_HALF_HEIGHT   = 'l';

type
  { Сеанс связи с ФР, содержит токен авторизации }
  TFrSession = class(TObject)
    realm: string;
    { server-generated data string }
    nonce: string;
    { A string of data, specified by the server, which should be returned
     by the client unchanged in the Authorization header of subsequent
     requests with URIs in the same protection space. }
    opaque: string;
    { "MD5" }
    algorithm: string;
    { quality of protection }
    qop: string;
    { hexadecimal count of the number of requests (including the current request)
     that the client has sent with the nonce value in this request }
    nc: string;
    { opaque quoted string value provided by the client and used by both client
     and server to avoid chosen plaintext attacks, to provide mutual
     authentication, and to provide some message integrity protection }
    cnonce: string;
    response: string;

    IntNC: Integer;
    OldNonce: string;
    OldCnonce: string;
  end;

  TFrRequest = class(TObject)
    Session: TFrSession;
    { момент времени отправки запроса }
    SendTimestamp: TDateTime;
    { тип запроса }
    RequestType: Integer;
    { URL запроса }
    RequestUrl: string;
    { данные запроса }
    RequestJson: string;
    { данные результата }
    ResultJson: string;
    ResultHeaders: string;
  end;

  { типы документов }
  TFrDocType = (frdUnknown,      // неопределен
                frdFiscal,       // фискальный чек
                frdRefund,       // чек возврата
                frdCashIO,       // чек внесения-изъятия денег
                frdVoiding,      // чек аннулирования
                frdNonFiscal,    // нефискальный чек
                frdCopy,         // копия чека или ресторанного счета
                frdOrder,        // ресторанный заказ
                frdOrderCancel,  // отмена ресторанного заказа
                frdLogin,        // регистрация кассира
                frdZReport);     // дневной Z-отчет

  { TFrDoc }

  TFrDoc = class(TObject)
  private
    FDocType: TFrDocType;
  public
    Data: IDataStorage;
    Lines: IDataStorage;

    // заполняется при чтении ленты
    // Дата и время объекта в ленте
    DateTime: TDateTime;
    // уникальный идентификатор объекта в чековой ленте
    ID: Integer;
    // номер оператора, связанный с объектом ленты
    OperID: Integer;
    // номер ресторанного счета
    BillNum: Integer;
    { -- только для чеков }
    // номер чека
    DocNum: Integer;
    // признак того, что чек еще не напечатан.
    IsPending: Boolean;
    // уникальный идентификатор документа
    UID: string;

    { строка продажи }
    procedure AddSale(AName, ACode: string; APrice: Currency; AQty: Currency = 1;
      ATax: Integer = -1; ACType: Integer = 1;
      ADep: Integer = 1; AGrp: Integer = 1);
    { строка скидки }
    procedure AddDiscount(ASum, APrc: Currency; IsAll: Boolean = False; ADn: Integer = 0);
    { строка оплаты }
    procedure AddPayment(ASum: Currency; ANo: Integer = 0);
    { строка служебного внесения/изъятия средств }
    procedure AddCashIO(ASum: Currency; ANo: Integer = 0);
    { строка текстового комментария }
    procedure AddTextComment(AText: string);
    { строка нефискального комментария
      AAttr: модификатор ширины и высоты текста }
    procedure AddNonFiscalComment(AText: string; AAttr: string = '');
    { штрих-код }
    procedure AddBarcode(ACode: string; AType: Integer = 1;
      AWidth: Integer = 2; AHeight: Integer = 60;
      AFeed: Integer = 20);
    { Аннулирование документа }
    procedure AddDocVoiding(ANo: Integer);
    { Коррекция документа
      ACode: Код товара (не более чем 13 цифр)
      Отменяется первая с начала чека продажа товара с указанным кодом (если
      код не указан, то отменяется последняя операция)}
    procedure AddDocCorrection(ACode: string = '');
    { Копия документа или ресторанного счета
      ABillNo: Номер открытого счета для копии(1…9999)
      ADocNo: Номер документа}
    procedure AddDocCopy(ABillNo, ADocNo: Integer);
    { Открытие ресторанного счета
      ATableNo: Номер столика (1…65000)
      APlaceNo: Номер места за столиком (1…250, по умолчанию 1) }
    procedure AddNewBill(ATableNo: Integer; APlaceNo: Integer = 1);
    { Новый ресторанный заказ
      ABillNo: Номер счета (1…9999) }
    procedure AddNewOrder(ABillNo: Integer);
    { Закрытые счета }
    procedure AddCloseBill(ABillNo: Integer);
    { Отмена ресторанного счета }
    procedure AddCancelBill(ABillNo: Integer);

    function GetDocTypeStr(): string;

    constructor Create(ADocType: TFrDocType);

    property DocType: TFrDocType read FDocType;
  end;

  { TFrDocList }

  TFrDocList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(AIndex: Integer): TFrDoc;
  end;

  TFrDevInfo = record
    Model: string;
    Name: string;
    SerialNum: string;
    Version: string;
    SwDate: TDateTime;
    SknoNum: string;
    RegNum: string;
    UnpNum: string;
    ProtocolVersion: string;

    ChkId: Integer;
    JrnTime: TDateTime;
    CurrZ: Integer;
    IsWrk: Boolean;
    IsFiscalization: Boolean;
    IsFskMode: Boolean;
    SknoState: Integer;
    Err: string;
  end;

  { Информация о послежднем или текущем документе }
  TFrDocInfo = record
    // Номер документа, 0 - неопределен
    DocNum: Integer;
    TotalSum: Currency;
    DateTime: TDateTime;
    // номер оператора, начавшего документ
    OpNum: Integer;
    UID: string;
    // для несохраненного документа
    // битовая маска состояния документа (DOC_STATE_)
    DocState: Integer;
    RepNum: Integer;
  end;

  { Информация о количестве записей }
  TFrRecListInfo = record
    TotalCount: Integer;
    FreeCount: Integer;
  end;

  { TTitanDriver }

  TTitanDriver = class(TComponent)
  private
    // создаются сразу
    FFrAddrList: TStringList;
    FDocList: TFrDocList;
    // создаются в процессе работы
    FSession: TFrSession;
    FUdpSocket: TUDPBlockSocket;

    FDevAddr: string;
    FDevLogin: string;
    FDevPassw: string;

    FDevInfo: TFrDevInfo;
    FLastDocInfo: TFrDocInfo;
    FCurDocInfo: TFrDocInfo;

    FFmRoomInfo: TFrRecListInfo;
    FJrnRoomInfo: TFrRecListInfo;

    procedure SendAuth(ARequest: TFrRequest);

    procedure SendRequest(AReqType: Integer; AUrl, AJson: string);

    { разбор ответа на запрос }
    procedure ParseReqResult(AReqType: Integer; AData: IDataStorage);
    { разбор ответа на запрос /cgi/chk }
    procedure ParseChk(AData: IDataStorage);

    //procedure OnUdpDataAppearHandler(Sender: TObject);
  public
    IsStateUpdated: Boolean;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    { прогон бумаги }
    procedure FeedPaper();
    { регистрация }
    //procedure Fiscalization();
    { состояние фискальной памяти }
    //procedure GetFMRoom();
    { состояние электронного журнала }
    //procedure GetJrnRoom();
    { информация по последнем зарегистрированном документе }
    //procedure LastReceipt();
    { открытие денежного ящика }
    procedure OpenBox();
    { печать отчета с БЭП }
    procedure PrintFMReport(AReportType: Integer; ADateStart, ADateEnd: TDateTime;
      ANumStart, ANumEnd: Integer);
    { печать отчета }
    procedure PrintReport(AReportType: Integer);
    { перерегистрация }
    //procedure PutHdrFM();
    {  запись налоговых ставок в БЭП }
    //procedure PutTaxFM();
    { установка времени }
    procedure SetClock(ADateTime: TDateTime);
    { состояние модуля СКНО }
    //procedure SknoState();
    { звуковой сигнал
      ALen - длительность (мс)
      AFreq - частота (Гц) }
    procedure Sound(ALen, AFreq: Integer);
    { состояние текущего документа }
    procedure GetDocState();
    { запросить состояние ФП }
    procedure GetFMState();
    { запросить состояние прибора }
    procedure GetDevState();
    { запросить информацию о приборе }
    procedure GetDevInfo();
    { запросить журнал документов
      ADocID - ID документа, с которого нужно читать журнал до конца, если 0 то все
      ADocNum - номер отдельного документа для чтения, если 0 то не нужен }
    procedure GetDocs(ADocID: Integer = 0; ADocNum: Integer = 0);

    { Запустить обнаружение ФР в сети }
    procedure Discover();

    { Такт, вызывается каждые 100 мс }
    procedure Tick();

    procedure SendFrDoc(AFrDoc: TFrDoc);

    property DevAddr: string read FDevAddr write FDevAddr;
    property DevLogin: string read FDevLogin write FDevLogin;
    property DevPassw: string read FDevPassw write FDevPassw;

    property DevInfo: TFrDevInfo read FDevInfo;
    property AddrList: TStringList read FFrAddrList;
    property DocList: TFrDocList read FDocList;
  end;

implementation

uses
  JsonStorage, HttpSend, md5, DateUtils, synautil;

const

  { === коды ошибок кассы === }
  ERR_PRICE_NOT_SET    = $01; // Цена не указана
  ERR_QTY_NOT_SET      = $02; // Количество не указано
  ERR_DEP_NOT_SET      = $03; // Отдел не указан
  ERR_GRP_NOT_SET      = $04; // Группа не указана
  ERR_NO_PAPER         = $25; // Нет бумаги
  ERR_USER_LOGGED      = $31; // Пользователь уже зарегистрирован
  ERR_BAD_PASSWD       = $32; // Неверный пароль
  ERR_BAD_TABL_NO      = $33; // Неверный номер таблицы
  ERR_TABL_NO_ACCESS   = $34; // Доступ к таблице запрещен
  ERR_NO_DEFAULT       = $35; // Умолчание не найдено
  ERR_BAD_INDEX        = $36; // Неверный индекс
  ERR_BAD_FIELD        = $37; // Неверное поле
  ERR_TABL_FULL        = $38; // Таблица переполнена
  ERR_BAD_BIN_LEN      = $39; // Неверная длина двоичных данных
  ERR_RO_FIELD         = $3A; // Попытка модификации поля только для чтения
  ERR_BAD_FIELD_VAL    = $3B; // Неверное значение поля
  ERR_PROD_EXIST       = $3C; // Товар уже существует
  ERR_PROD_SALE_EXIST  = $3D; // По товару были продажи
  ERR_REQ_NO_ACCESS    = $3E; // Запрос запрещен
  ERR_BAD_TAB          = $3F; // Неверная закладка
  ERR_KEY_NOT_FOUND    = $40; // Ключ не найден
  ERR_PROC_EXECUTED    = $41; // Процедура уже исполняется
  ERR_QTY_NEGATIVE     = $42; // Количество товара отрицательно
  ERR_TAPE_NO_PAPER    = $8A; // Нет бумаги для контрольной ленты
  ERR_NO_PAPER2        = $8B; // Нет бумаги
  ERR_NO_ODD_MONEY     = $8D; // Выдача сдачи запрещена
  ERR_HAS_STATES       = $A6; // Есть 3 или более непереданных отчета
  ERR_TAPE_NOT_EMPTY   = $BB; // Лента не пуста
  ERR_NO_KAFE_MODE     = $B6; // Ресторанный режим не активен
  ERR_NO_KAFE_BILL     = $B7; // Ресторанный счет не открыт
  ERR_ORDERS_FULL      = $B8; // Переполнение количества заказов
  ERR_BILL_OPENED      = $B9; // Ресторанный чек открыт
  ERR_BAD_BILL_NUM     = $BA; // Неверный номер счета
  ERR_PRACTICE_MODE    = $BC; // Режим тренировки
  ERR_BAD_DATE         = $BD; // Текущая дата неверна
  ERR_CANT_SET_DATE    = $BE; // Запрещено изменение времени
  ERR_SERVICE_TIMER    = $BF; // Истек сервисный таймер
  ERR_TERMINAL_ERR     = $C0; // Ошибка работы с терминалом НСМЕП
  ERR_BAD_TAX_NUM      = $C1; // Неверный номер налога
  ERR_BAD_PROC_PARAM   = $C2; // Неверный параметр у процедуры
  ERR_NO_FISCAL_MODE   = $C3; // Режим фискального принтера не активен
  ERR_PROD_TAX_CHANGED = $C4; // Изменялось название товара или его налог
  ERR_FM_BUSY          = $C5; // СКНО занято
  ERR_FM_NO_LINK       = $C6; // ошибка обмена с СКНО (нет связи)
  ERR_WORK_NOT_OPEN    = $C7; // смена не открыта
  ERR_FM_FULL          = $C8; // СКНО переполнено
  ERR_FM_BAD_STATE     = $C9; // Неверный статус СКНО
  ERR_FM_BAD_IDENT     = $CA; // Ошибка идентификации СКНО
  ERR_CANT_SALE        = $CB; // запрещена операция продажи
  ERR_REFUND_STARTED   = $CC; // Начата операция возврата
  ERR_BAD_PROD_TYPE    = $CE; // неверный тип кода товара
  ERR_Z1_FAIL          = $CF; // Не выведен отчет Z1
  ERR_INCASS_FAIL      = $D0; // Не сделана инкассация денег
  ERR_BOX_NOT_CLOSED   = $D1; // Сейф не закрыт
  ERR_TAPE_PRN_FAIL    = $D2; // Печать ленты прервана
  ERR_WORK_END         = $D3; // Достигнут конец текущей смены, или изменилась дата
  ERR_NO_DEF_DISCONT_PRC = $D4; // Не указано значение процентной скидки по умолчанию
  ERR_NO_DEF_DISCONT_VAL = $D5; // Не указано значение скидки по умолчанию
  ERR_WORK_REP_FAIL    = $D6; // Дневной отчет не выведен
  ERR_WORK_REP_EMPTY   = $D7; // Дневной отчет уже выведен (и пуст)
  ERR_NEED_REFUND_DISCONT = $D8; // Нельзя отменить товар на который сделана скидка без ее предварительной отмены
  ERR_NO_PROD_SALE     = $D9; // Товар не продавался в этом чеке
  ERR_NO_REFUND        = $DA; // Нечего отменять
  ERR_SUM_NEGATIVE     = $DB; // Отрицательная сумма продажи товара
  ERR_BAD_PRC          = $DC; // Неверный процент
  ERR_NO_SALE          = $DD; // Нет ни одной продажи
  ERR_CANT_DISCOUNT    = $DE; // Скидки запрещены
  ERR_BAD_SUM          = $DF; // Неверная сумма платежа
  ERR_NO_NEED_CUSTOMER = $E0; // Тип оплаты не предполагает введения кода клиента
  ERR_BAD_SUM2         = $E1; // Неверная сумма платежа
  ERR_PAY_BUSY         = $E2; // Идет оплата чека
  ERR_OUT_OF_PROD      = $E3; // Товар закончился
  ERR_CANT_SET_GRP     = $E4; // Номер группы не может меняться
  ERR_BAD_GRP          = $E5; // Неверная группа
  ERR_CANT_SET_DEP     = $E6; //  Номер отдела не может меняться
  ERR_BAD_DEP          = $E7; // Неверный отдел
  ERR_ZERO_SUM         = $E8; // Нулевое произведение количества на цену
  ERR_SUM_OVERFLOW     = $E9; // Переполнение внутренних сумм
  ERR_CANT_FRACT_QTY   = $EA; // Дробное количество запрещено
  ERR_BAD_QTY          = $EB; // Неверное количество
  ERR_CANT_SET_PRICE   = $EC; // Цена не может быть изменена
  ERR_BAD_PRICE        = $ED; // Неверная цена
  ERR_BAD_PROD         = $EE; // Товар не существует
  ERR_CASH_IO_START    = $EF; // Начат чек внесения-изъятия денег
  ERR_HAS_SALES        = $F0; // Чек содержит продажи
  ERR_BAD_PAY_TYPE     = $F1; // Не существующий или запрещенный тип оплаты
  ERR_FIELD_FULL       = $F2; // Поле в строке переполнено
  ERR_NEGATIVE_WORK_REP = $F3; // Отрицательная сумма по дневному отчету
  ERR_NEGATIVE_SUM     = $F4; // Отрицательная сумма по чеку
  ERR_DOC_FULL         = $F5; // Чек переполнен
  ERR_WORK_FULL        = $F6; // Дневной отчет переполнен
  ERR_BAD_COPY_NUM     = $F7; // Чек для копии не найден
  ERR_PAY_FAIL         = $F8; // Оплата чека не завершена
  ERR_BAD_PERS         = $F9; // Кассир не зарегистрирован
  ERR_CANT_PERS        = $FA; // У кассира нет прав на эту операцию
  ERR_NF_DOC_NOT_OPEN  = $FB; // Нефискальный чек не открыт
  ERR_DOC_NOT_OPEN     = $FC; // Чек не открыт
  ERR_NF_DOC_OPEN      = $FD; // Нефискальный чек уже открыт
  ERR_DOC_OPEN         = $FE; // Чек уже открыт
  ERR_TAPE_FULL        = $FF; // Переполнение ленты

  { === типы запросов === }
  REQ_TYPE_DEV_INFO    = $01; // /cgi/dev_info
  REQ_TYPE_DEV_STATE   = $02; // /cgi/state
  REQ_TYPE_CHK         = $03; // /cgi/chk
  REQ_TYPE_LOGO        = $04; // /cgi/logo.bmp
  REQ_TYPE_DESC        = $05; // /desc
  // таблицы
  REQ_TYPE_TBL         = $10; // /cgi/tbl
  REQ_TYPE_TBL_WHITEIP = $11; // /cgi/tbl/whiteIP
  REQ_TYPE_TBL_OPER    = $12; // /cgi/tbl/Oper
  REQ_TYPE_TBL_TAX     = $13; // /cgi/tbl/Tax
  REQ_TYPE_TBL_FSK     = $14; // /cgi/tbl/Fsk
  REQ_TYPE_TBL_FDAY    = $15; // /cgi/tbl/FDay
  REQ_TYPE_TBL_FTAX    = $16; // /cgi/tbl/FTax
  REQ_TYPE_TBL_FSBR    = $17; // /cgi/tbl/FSbr
  REQ_TYPE_TBL_TCP     = $18; // /cgi/tbl/TCP
  REQ_TYPE_TBL_SYSLOG  = $19; // /cgi/tbl/SysLog
  REQ_TYPE_TBL_PAY     = $1A; // /cgi/tbl/Pay
  REQ_TYPE_TBL_HDR     = $1B; // /cgi/tbl/Hdr
  REQ_TYPE_TBL_ADM     = $1C; // /cgi/tbl/Adm
  REQ_TYPE_TBL_FLG     = $1D; // /cgi/tbl/Flg
  // процедуры
  REQ_TYPE_PRINTREPORT   = $21; // /cgi/proc/printreport
  REQ_TYPE_PRINTFMREPORT = $22; // /cgi/proс/printfmreport
  REQ_TYPE_FEEDPAPER     = $23; // /cgi/proc/feedpaper
  REQ_TYPE_FISCALIZATION = $24; // /cgi/proc/fiscalization
  REQ_TYPE_GETFMROOM     = $25; // /cgi/proc/getfmroom
  REQ_TYPE_GETJRNROOM    = $26; // /cgi/proc/getjrnroom
  REQ_TYPE_LASTRECEIPT   = $27; // /cgi/proc/lastreceipt
  REQ_TYPE_OPENBOX       = $28; // /cgi/proc/openbox
  REQ_TYPE_PUTHDRFM      = $29; // /cgi/proc/puthdrfm
  REQ_TYPE_PUTTAXFM      = $2A; // /cgi/proc/puttaxfm
  REQ_TYPE_SETCLOCK      = $2B; // /cgi/proc/setclock
  REQ_TYPE_SKNOSTATE     = $2C; // /cgi/proc/sknostate
  REQ_TYPE_SOUND         = $2D; // /cgi/proc/sound
  REQ_TYPE_PROC_STATE    = $2E; // /cgi/proc/state
  // отчеты
  REQ_TYPE_REP_PAY     = $31; // /cgi/rep/pay
  //REQ_TYPE_            = $00; // /cgi/

function DataToJson(AData: IDataStorage): string;
var
  ser: TDataSerializerJson;
begin
  Result := '';
  ser := TDataSerializerJson.Create();
  try
    Result := ser.StorageToString(AData);
  finally
    ser.Free();
  end;
end;

function JsonToData(AJson: string): IDataStorage;
var
  ser: TDataSerializerJson;
begin
  Result := nil;
  ser := TDataSerializerJson.Create();
  try
    Result := ser.StorageFromString(AJson);
  finally
    ser.Free();
  end;
end;

function IsoDateToDateTime(AIsoDate: string): TDateTime;
var
  s: string;
  YY, MM, DD, HH, NN, SS: Integer;
begin
  s := AIsoDate;
  // 10.11.2012
  if Pos('.', s) = 3 then
  begin
    DD := StrToIntDef(Fetch(s, '.'), 1);
    MM := StrToIntDef(Fetch(s, '.'), 1);
    YY := StrToIntDef(Fetch(s, '.'), 2000);
    TryEncodeDate(YY, MM, DD, Result);
    Exit;
  end;
  // 2012.11.10
  if Pos('.', s) = 5 then
  begin
    YY := StrToIntDef(Fetch(s, '.'), 2000);
    MM := StrToIntDef(Fetch(s, '.'), 1);
    DD := StrToIntDef(Fetch(s, '.'), 1);
    TryEncodeDate(YY, MM, DD, Result);
    Exit;
  end;
  // 2012-12-31T23:59:59
  if Pos('T', s) > 0 then
  begin
    YY := StrToIntDef(Fetch(s, '-'), 2000);
    MM := StrToIntDef(Fetch(s, '-'), 1);
    DD := StrToIntDef(Fetch(s, 'T'), 1);
    HH := StrToIntDef(Fetch(s, ':'), 0);
    NN := StrToIntDef(Fetch(s, ':'), 0);
    SS := StrToIntDef(Fetch(s, '.'), 0);
    TryEncodeDateTime(YY, MM, DD, HH, NN, SS, 0, Result);
    Exit;
  end;
end;

function DateTimeToIso(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-ddThh:nn:ss', ADateTime);
end;

function DateToIso(ADateTime: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', ADateTime);
end;

function StripQuotes(StrIn: string): string;
begin
  Result := StringReplace(StrIn,'"', '', [rfReplaceAll]);
end;

procedure SetDefaultHeaders(var AHttpSend: THttpSend);
begin
  AHttpSend.Clear();
  AHttpSend.Headers.Clear();
  AHttpSend.Protocol := '1.1';
  //AHttpSend.Headers.Add('User-Agent: serbod');

  //AHttpSend.Headers.Add('Connection: keep-alive');
  //AHttpSend.Headers.Add('User-Agent: Mozilla/5.0 (X11; Linux i686; rv:9.0) Gecko/20100101 Firefox/9.0');
  //AHttpSend.Headers.Add('Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8');
  //AHttpSend.Headers.Add('Accept-Language: en-us,en;q=0.5');
  //AHttpSend.Headers.Add('Accept-Encoding: gzip, deflate');
  //AHttpSend.Headers.Add('Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7');
end;

{ TFrDocList }

procedure TFrDocList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TFrDoc(Ptr).Free();
end;

function TFrDocList.GetItem(AIndex: Integer): TFrDoc;
begin
  Result := TFrDoc(Get(AIndex));
end;

{ TFrDoc }

procedure TFrDoc.AddSale(AName, ACode: string; APrice: Currency;
  AQty: Currency; ATax: Integer; ACType: Integer; ADep: Integer; AGrp: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund, frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    if AQty <> 1 then
      dsLine.SetValue(AQty, 'qty');
    dsLine.SetValue(APrice, 'price');
    dsLine.SetValue(AName, 'name');
    dsLine.SetValue(ACode, 'code');
    if ATax <> 0 then
      dsLine.SetValue(ATax, 'tax');
    if ACType <> 1 then
      dsLine.SetValue(ACType, 'ctype');
    if ADep <> 1 then
      dsLine.SetValue(ADep, 'dep');
    if AGrp <> 1 then
      dsLine.SetValue(AGrp, 'grp');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddDiscount(ASum, APrc: Currency; IsAll: Boolean; ADn: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund, frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    if ASum <> 0 then
      dsLine.SetValue(ASum, 'sum')
    else
      dsLine.SetValue(APrc, 'prc');

    if IsAll then
      dsLine.SetValue(1, 'all');

    dsLine.SetValue(ADn, 'dn');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddPayment(ASum: Currency; ANo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    if ASum <> 0 then
      dsLine.SetValue(ASum, 'sum');

    if ANo <> 0 then
      dsLine.SetValue(ANo, 'no');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddCashIO(ASum: Currency; ANo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdCashIO] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue('IO', 'type');
    dsLine.SetValue(ASum, 'sum');

    if ANo <> 0 then
      dsLine.SetValue(ANo, 'no');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddTextComment(AText: string);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund, frdCashIO, frdNonFiscal, frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(AText, 'Cm');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddNonFiscalComment(AText: string; AAttr: string);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund, frdCashIO, frdNonFiscal, frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(AText, 'Cm');

    if AAttr <> '' then
      dsLine.SetValue(AAttr, 'Attr');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddBarcode(ACode: string; AType: Integer; AWidth: Integer;
  AHeight: Integer; AFeed: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund, frdCashIO, frdNonFiscal, frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(ACode, 'code');
    dsLine.SetValue(AType, 'type');

    if AWidth <> 2 then
      dsLine.SetValue(AWidth, 'width');

    if AHeight <> 60 then
      dsLine.SetValue(AHeight, 'height');

    if AFeed <> 20 then
      dsLine.SetValue(AFeed, 'feed');

    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddDocVoiding(ANo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdVoiding] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(ANo, 'no');
    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddDocCorrection(ACode: string);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal, frdRefund, frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    if ACode <> '' then
      dsLine.SetValue(ACode, 'code');
    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddDocCopy(ABillNo, ADocNo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdCopy] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    if ABillNo <> 0 then
      dsLine.SetValue(ABillNo, 'bill')
    else
      dsLine.SetValue(ADocNo, 'no');
    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddNewBill(ATableNo: Integer; APlaceNo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(ATableNo, 'table');
    dsLine.SetValue(APlaceNo, 'place');
    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddNewOrder(ABillNo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdOrder] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(ABillNo, 'bill');
    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddCloseBill(ABillNo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdFiscal] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(ABillNo, 'bill');
    Lines.SetValue(dsLine);
  end;
end;

procedure TFrDoc.AddCancelBill(ABillNo: Integer);
var
  dsLine: TDataStorage;
begin
  if not Assigned(Lines) then Exit;
  if FDocType in [frdOrderCancel] then
  begin
    dsLine := TDataStorage.Create(stDictionary);
    dsLine.SetValue(ABillNo, 'bill');
    Lines.SetValue(dsLine);
  end;
end;

function TFrDoc.GetDocTypeStr(): string;
begin
  case FDocType of
    frdUnknown: Result := 'Неопределен';
    frdFiscal: Result := 'Фискальный чек';
    frdRefund: Result := 'Чек на возврат';
    frdCashIO: Result := 'Внос/вынос денег';
    frdVoiding: Result := 'Обнуление';
    frdNonFiscal: Result := 'Нефискальный чек';
    frdCopy: Result := 'Копия чека/заказа';
    frdOrder: Result := 'Ресторанный заказ';
    frdOrderCancel: Result := 'Отмена заказа';
    frdLogin: Result := 'Регистрация кассира';
    frdZReport: Result := 'Дневной Z-отчет';
  end;
end;

constructor TFrDoc.Create(ADocType: TFrDocType);
begin
  inherited Create();
  FDocType := ADocType;
  Data := TDataStorage.Create(stDictionary);
  Lines := TDataStorage.Create(stList);
  case FDocType of
    frdFiscal: Data.SetValue(Lines, 'F');
    frdRefund: Data.SetValue(Lines, 'R');
    frdCashIO: Data.SetValue(Lines, 'IO');
    frdVoiding: Data.SetValue(Lines, 'VD');
    frdNonFiscal: Data.SetValue(Lines, 'P');
    frdCopy: Data.SetValue(Lines, 'L');
    frdOrder: Data.SetValue(Lines, 'RO');
    frdOrderCancel: Data.SetValue(Lines, 'VB');
  end;
end;

{ TTitanDriver }

procedure TTitanDriver.AfterConstruction();
begin
  inherited AfterConstruction();
  FFrAddrList := TStringList.Create();
  FDocList := TFrDocList.Create();
  FDevAddr := '169.254.148.191';
  FDevLogin := 'service';
  FDevPassw := '751426';
end;

procedure TTitanDriver.BeforeDestruction();
begin
  FreeAndNil(FDocList);
  FreeAndNil(FFrAddrList);
  inherited BeforeDestruction();
end;

procedure TTitanDriver.FeedPaper();
begin
  SendRequest(REQ_TYPE_FEEDPAPER, '/cgi/proc/feedpaper', '');
end;

procedure TTitanDriver.OpenBox();
begin
  SendRequest(REQ_TYPE_OPENBOX, '/cgi/proc/openbox', '');
end;

procedure TTitanDriver.PrintFMReport(AReportType: Integer; ADateStart,
  ADateEnd: TDateTime; ANumStart, ANumEnd: Integer);
var
  s: string;
begin
  if (AReportType < 1) or (AReportType > 4) then Exit;

  s := '&' + DateToIso(ADateStart) + '&' + DateToIso(ADateEnd)
     + '&' + IntToStr(ANumStart) + '&' + IntToStr(ANumEnd);
  SendRequest(REQ_TYPE_PRINTFMREPORT, '/cgi/proc/printfmreport?' + IntToStr(AReportType) + s, '');
end;

procedure TTitanDriver.PrintReport(AReportType: Integer);
begin
  SendRequest(REQ_TYPE_PRINTREPORT, '/cgi/proc/printreport?' + IntToStr(AReportType), '');
end;

procedure TTitanDriver.SetClock(ADateTime: TDateTime);
begin
  SendRequest(REQ_TYPE_SETCLOCK, '/cgi/proc/setclock?' + DateTimeToIso(ADateTime), '');
end;

procedure TTitanDriver.Sound(ALen, AFreq: Integer);
begin
  SendRequest(REQ_TYPE_SOUND, '/cgi/proc/sound?' + IntToStr(ALen) + '&' + IntToStr(AFreq), '');
end;

procedure TTitanDriver.GetDocState();
begin
  SendRequest(REQ_TYPE_PROC_STATE, '/cgi/proc/state', '');
end;

procedure TTitanDriver.GetFMState();
begin
  SendRequest(REQ_TYPE_SKNOSTATE, '/cgi/proc/sknostate', '');
  SendRequest(REQ_TYPE_GETFMROOM, '/cgi/proc/getfmroom', '');
  SendRequest(REQ_TYPE_GETJRNROOM, '/cgi/proc/getjrnroom', '');
  SendRequest(REQ_TYPE_LASTRECEIPT, '/cgi/proc/lastreceipt', '');
end;

procedure TTitanDriver.GetDevState();
begin
  SendRequest(REQ_TYPE_DEV_STATE, '/cgi/state', '');
end;

procedure TTitanDriver.GetDevInfo();
begin
  SendRequest(REQ_TYPE_DEV_INFO, '/cgi/dev_info', '');
end;

procedure TTitanDriver.GetDocs(ADocID: Integer; ADocNum: Integer);
var
  s: string;
begin
  s := '/cgi/chk';
  if ADocID <> 0 then
    s := s + '?id=' + IntToStr(ADocID)
  else if ADocNum <> 0 then
    s := s + '?no=' + IntToStr(ADocID);

  SendRequest(REQ_TYPE_CHK, s, '');
end;

procedure TTitanDriver.SendAuth(ARequest: TFrRequest);
var
  ht: THttpSend;
  slHeaders, slCookies: TStringList;
  x, y: Integer;
  isOk: Boolean;
  h1, h2, h3, sProt, sName, sPwd, sURI, sUrl: string;
  sUsr0, sPas0, sHost, sPort, sPath, sParams: string;
  aStr, sMethod: string;
  //wPort: Word;
begin
  if not Assigned(FSession) then
    FSession := TFrSession.Create();

  sUrl := 'http://' + Self.DevAddr + ARequest.RequestUrl;
  sProt := '';
  sUsr0 := '';
  sPas0 := '';
  sHost := '';
  sPort := '';
  sPath := '';
  sParams := '';

  ParseURL(sUrl, sProt, sUsr0, sPas0, sHost, sPort, sPath, sParams);

  sUri := sPath;
  sName := DevLogin;
  sPwd := DevPassw;
  sMethod := 'GET';

  ht := THttpSend.Create();
  slHeaders := TStringList.Create();
  slCookies := TStringList.Create();

  try
    SetDefaultHeaders(ht);
    if FSession.nonce <> '' then
    begin
      ht.Headers.Add('Authorization: Digest username=' + AnsiQuotedStr(sName, #34)
      + ', realm=' + AnsiQuotedStr(FSession.realm, #34)
      + ', nonce=' + AnsiQuotedStr(FSession.nonce, #34)
      + ', uri=' + AnsiQuotedStr(sUri, #34)
      + ', algorithm=MD5'
      + ', qop=' + FSession.qop
      + ', nc=' + FSession.nc
      + ', cnonce=' + AnsiQuotedStr(FSession.cnonce, #34)
      + ', response=' + AnsiQuotedStr(FSession.response, #34));
    end;

    ht.Timeout := 2;
    isOk := ht.HTTPMethod(sMethod, sUrl);
    if (ht.ResultCode = 401) then
    begin
      slCookies.Text := ht.Cookies.Text;
      for x:= 0 to Pred(ht.Headers.Count) do
      begin
        if LeftStr(UpperCase(ht.Headers.Strings[x]), 24) = 'WWW-AUTHENTICATE: DIGEST' then
        begin
          slHeaders.Clear;
          slHeaders.StrictDelimiter := true;
          slHeaders.Delimiter := ',';
          slHeaders.DelimitedText := Trim(Copy(ht.Headers.Strings[x], 25, 400));

          for y := 0 to pred(slHeaders.Count) do
          begin
            aStr := Trim(slHeaders.Strings[y]);
            if LeftStr(aStr, 5) = 'realm' then
              FSession.realm := StripQuotes(Copy(aStr, 7, Length(aStr)));
            if LeftStr(aStr, 5) = 'nonce' then
              FSession.nonce := StripQuotes(Copy(aStr, 7, Length(aStr)));
            if LeftStr(aStr, 3) = 'qop' then
              FSession.qop := StripQuotes(Copy(aStr, 5, Length(aStr)));
            if LeftStr(aStr, 6) = 'opaque' then
              FSession.opaque := StripQuotes(Copy(aStr, 8, Length(aStr)));
          end;

          isOk := False;

          if FSession.OldNonce = FSession.nonce then
          begin
            Inc(FSession.IntNC);
            FSession.cnonce := FSession.OldNonce;
          end
          else
          begin
            FSession.IntNC := 1;
            FSession.cnonce := md5Print(md5String(IntToStr(DateTimeToUnix(Now()))));
          end;

          FSession.nc := RightStr('00000000' + IntToStr(FSession.IntNC), 8);

          h1 := md5Print(md5String(sName + ':' + FSession.realm + ':' + sPwd));
          h2 := md5Print(md5String(sMethod + ':' + sUri));
          h3 := md5Print(md5String(h1 + ':' + FSession.nonce + ':' + FSession.nc + ':' + FSession.cnonce + ':' + FSession.qop + ':' + h2 ));
          FSession.response := h3;

          SetDefaultHeaders(ht);
          ht.Cookies.Text := slCookies.Text;
          ht.Headers.Add('Authorization: Digest username=' + AnsiQuotedStr(sName, #34)
          + ', realm=' + AnsiQuotedStr(FSession.realm, #34)
          + ', nonce=' + AnsiQuotedStr(FSession.nonce, #34)
          + ', uri=' + AnsiQuotedStr(sUri, #34)
          + ', algorithm=MD5'
          + ', qop=' + FSession.qop
          + ', nc=' + FSession.nc
          + ', cnonce=' + AnsiQuotedStr(FSession.cnonce, #34)
          + ', response=' + AnsiQuotedStr(FSession.response, #34));

          isOk := ht.HTTPMethod(sMethod, sUrl);
          Break;
        end;
      end;
    end;

    if IsOk then
    begin
      ht.Document.Seek(0,0);
      ARequest.ResultJson := ReadStrFromStream(ht.Document, ht.Document.Size);
      StrToFile('doc_body.json', ARequest.ResultJson);
    end;
    ARequest.ResultHeaders := IntToStr(ht.ResultCode) + ' ' + ht.ResultString + #13 + ht.Headers.Text;
    StrToFile('result_headers.txt', ARequest.ResultHeaders);
    FSession.OldNonce := FSession.nonce;
    FSession.OldcNonce := FSession.cnonce;
  finally
    ht.Free();
    slHeaders.Free();
    slCookies.Free();
  end;
end;

procedure TTitanDriver.SendRequest(AReqType: Integer; AUrl, AJson: string);
var
  TmpReq: TFrRequest;
  TmpData: IDataStorage;
begin
  TmpReq := TFrRequest.Create();
  TmpReq.SendTimestamp := Now();
  TmpReq.RequestType := AReqType;
  TmpReq.RequestUrl := AUrl;
  TmpReq.RequestJson := AJson;
  TmpReq.ResultJson := '';

  SendAuth(TmpReq);

  if TmpReq.ResultJson <> '' then
  begin
    // разбор ответа
    TmpData := JsonToData(TmpReq.ResultJson);
    if not Assigned(TmpData) then Exit;

    ParseReqResult(TmpReq.RequestType, TmpData);
    {case TmpReq.RequestType of
      REQ_TYPE_DEV_INFO: ParseDevInfo(TmpData);
      REQ_TYPE_DEV_STATE: ParseDevState(TmpData);
    end; }
  end;
end;

procedure TTitanDriver.ParseReqResult(AReqType: Integer; AData: IDataStorage);
begin
  case AReqType of
    REQ_TYPE_DEV_INFO:
    begin
      FDevInfo.SerialNum := AData.GetString('dev_zn');
      FDevInfo.Version := AData.GetString('dev_ver');
      FDevInfo.SwDate := IsoDateToDateTime(AData.GetString('dev_dat'));
      FDevInfo.SknoNum := AData.GetString('skno_num');
      FDevInfo.RegNum := AData.GetString('reg_num');
      FDevInfo.UnpNum := AData.GetString('unp_num');
      FDevInfo.ProtocolVersion := AData.GetString('prot');
    end;

    REQ_TYPE_DEV_STATE:
    begin
      FDevInfo.Model := AData.GetString('model');
      FDevInfo.Name := AData.GetString('name');
      FDevInfo.SerialNum := AData.GetString('serial');
      FDevInfo.ChkId := AData.GetInteger('chkId');
      FDevInfo.JrnTime := IsoDateToDateTime(AData.GetString('JrnTime'));
      FDevInfo.CurrZ := AData.GetInteger('currZ');
      FDevInfo.IsWrk := AData.GetBool('IsWrk');
      FDevInfo.IsFiscalization := AData.GetBool('Fiscalization');
      FDevInfo.IsFskMode := AData.GetBool('FskMode');
      FDevInfo.SknoState := AData.GetInteger('SKNOState');
      FDevInfo.Err := AData.GetString('err');
    end;

    REQ_TYPE_GETFMROOM:
    begin
      FFmRoomInfo.TotalCount := AData.GetObject(0).GetInteger();
      FFmRoomInfo.FreeCount := AData.GetObject(1).GetInteger();
    end;

    REQ_TYPE_GETJRNROOM:
    begin
      FJrnRoomInfo.TotalCount := AData.GetObject(0).GetInteger();
      FJrnRoomInfo.FreeCount := AData.GetObject(1).GetInteger();
    end;

    REQ_TYPE_LASTRECEIPT:
    begin
      FLastDocInfo.TotalSum := StrToCurr(AData.GetObject(0).GetValue());
      FLastDocInfo.DocNum := AData.GetObject(1).GetInteger();
      FLastDocInfo.DateTime := IsoDateToDateTime(AData.GetObject(2).GetValue());
      FLastDocInfo.OpNum := AData.GetObject(3).GetInteger();
      FLastDocInfo.UID := AData.GetObject(4).GetValue();
    end;

    REQ_TYPE_PROC_STATE:
    begin
      FCurDocInfo.DocState := AData.GetObject(0).GetInteger();
      FCurDocInfo.OpNum := AData.GetObject(1).GetInteger();
      FCurDocInfo.DocNum := AData.GetObject(2).GetInteger();
      FCurDocInfo.RepNum := AData.GetObject(3).GetInteger();
    end;

    REQ_TYPE_SKNOSTATE:
    begin
      FDevInfo.SknoState := AData.GetInteger();
    end;

    REQ_TYPE_CHK:
    begin
      ParseChk(AData);
    end;
  end;
  IsStateUpdated := True;
end;

procedure TTitanDriver.ParseChk(AData: IDataStorage);
var
  TmpDoc: TFrDoc;
  TmpDocType: TFrDocType;
begin
  if (not Assigned(AData)) or (not AData.HaveName('id')) then Exit;

  TmpDocType := frdUnknown;
  if AData.HaveName('L') then
  begin
    // реристрация кассира
    TmpDocType := frdLogin;
  end
  else if AData.HaveName('Z1') then
  begin
    // номер дневного отчета
    TmpDocType := frdZReport;
  end
  else if AData.HaveName('F') then
  begin
    // строки фискального чека
    TmpDocType := frdFiscal;
  end
  else if AData.HaveName('R') then
  begin
    // строки чека возврата
    TmpDocType := frdRefund;
  end
  else if AData.HaveName('IO') then
  begin
    // строки вноса-выноса денег
    TmpDocType := frdCashIO;
  end
  else if AData.HaveName('VD') then
  begin
    // строки аннулирования
    TmpDocType := frdVoiding;
  end
  else if AData.HaveName('RO') then
  begin
    // строки ресторанных заказов
    TmpDocType := frdOrder;
  end
  else if AData.HaveName('bill') then
  begin
    // номер ресторанного счета
  end;

  TmpDoc := TFrDoc.Create(TmpDocType);
  TmpDoc.DateTime := UnixToDateTime(AData.GetInteger('datetime'));
  TmpDoc.ID := AData.GetInteger('id');
  TmpDoc.OperID := AData.GetInteger('oper_id');
  TmpDoc.BillNum := AData.GetInteger('bill');
  TmpDoc.DocNum := AData.GetInteger('no');
  TmpDoc.IsPending := AData.HaveName('Pending');
  TmpDoc.UID := AData.GetString('UI');

  DocList.Add(TmpDoc);
end;

{
procedure TTitanDriver.OnUdpDataAppearHandler(Sender: TObject);
var
  s, sAddr: string;
begin
  sAddr := FDataPortUdp.RemoteHost + ':' + FDataPortUdp.RemotePort;
  s := FDataPortUdp.Pull();
  if Pos('HTTP/1.1 200 OK', s) > 0 then
  begin
    // добавление в список известных ФР
    if FFrAddrList.IndexOf(sAddr) = -1 then
      FFrAddrList.Add(sAddr);
  end;
end; }

procedure TTitanDriver.Discover();
var
  s, sr, sAddr: AnsiString;
begin
  s := 'M-SEARCH * HTTP/1.1' + #13 + #10
     + 'Host:239.255.255.250:1900' + #13 + #10
     //+ 'ST:urn:help-micro.kiev.ua:device:webdev:1' + #13 + #10
     + 'ST:upnp:rootdevice' + #13 + #10
     + 'Man:"ssdp:discover"' + #13 + #10
     + 'MX:3' + #13 + #10 + #13 + #10;

  FFrAddrList.Clear();
  FFrAddrList.Add('169.254.148.191');

  {if not Assigned(FDataPortUdp) then
    FDataPortUdp := TDataPortUDP.Create(Self);

  FDataPortUdp.OnDataAppear := @OnUdpDataAppearHandler;
  FDataPortUdp.Open('239.255.255.250:1900');
  FDataPortUdp.Push(s);   }

  if not Assigned(FUdpSocket) then
    FUdpSocket := TUDPBlockSocket.Create();

  FUdpSocket.EnableBroadcast(True);
  begin
    FUdpSocket.AddMulticast('239.255.255.250');
    //FUdpSocket.Bind('0.0.0.0', '1900');
    FUdpSocket.Connect('239.255.255.250', '1900');
    FUdpSocket.SendString(s);
    sr := FUdpSocket.RecvPacket(3000);
    if Pos('HTTP/1.1 200 OK', sr) > 0 then
    begin
      sAddr := FUdpSocket.GetRemoteSinIP();
      // добавление в список известных ФР
      if FFrAddrList.IndexOf(sAddr) = -1 then
        FFrAddrList.Add(sAddr);
    end;
    FreeAndNil(FUdpSocket);
  end;
end;

procedure TTitanDriver.Tick();
begin
  if Assigned(FUdpSocket) then
  begin
    FUdpSocket.RecvPacket(10);
  end;
end;

procedure TTitanDriver.SendFrDoc(AFrDoc: TFrDoc);
var
  sUrl: string;
  sJson: string;
begin
  if not Assigned(AFrDoc) then Exit;
  sUrl := '/cgi/chk';
  sJson := DataToJson(AFrDoc.Data);
end;

end.

