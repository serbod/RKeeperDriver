unit TitanDrv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DataPort, DataStorage,
  DataPortIP, DataPortHTTP, fphttpclient;

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
  REPORT_X1               = 10; // 100 – дневной отчет без обнуление (X1)
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
  end;

  TFrRequest = class(TObject)
    Session: TFrSession;
    { момент времени отправки запроса }
    SendTimestamp: TDateTime;
    { URL запроса }
    RequestUrl: string;
    { данные запроса }
    RequestJson: string;
    { данные результата }
    ResultJson: string;
  end;

  TFrDocType = (frdUncnown, frdFiscal, frdRefund, frdCashIO, frdVoiding, frdNonFiscal, frdCopy, frdOrder, frdOrderCancel);

  { TFrDoc }

  TFrDoc = class(TObject)
  private
    FDocType: TFrDocType;
  public
    Data: IDataStorage;
    Lines: IDataStorage;

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

    constructor Create(ADocType: TFrDocType);
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
    SknoState: string;
    Err: string;
  end;

  { TTitanDriver }

  TTitanDriver = class(TComponent)
  private
    FFrAddrList: TStringList;

    FDataPortUdp: TDataPortUDP;

    FDevAddr: string;
    FDevLogin: string;
    FDevPassw: string;
    FDataPort: TDataPort;

    FDevInfo: TFrDevInfo;

    procedure SendRequest(AUrl, AJson: string);

    { прочитать идентификацию устройства в FrDevInfo }
    procedure ParseDevInfo(AData: IDataStorage);
    { прочитать состояние устройства в FrDevInfo }
    procedure ParseDevState(AData: IDataStorage);


    procedure OnUdpDataAppearHandler(Sender: TObject);
  public
    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;

    { прогон бумаги }
    //procedure FeedPaper();
    { регистрация }
    //procedure Fiscalization();
    { состояние фискальной памяти }
    //procedure GetFMRoom();
    { состояние электронного журнала }
    //procedure GetJrnRoom();
    { информация по последнем зарегистрированном документе }
    //procedure LastReceipt();
    { открытие денежного ящика }
    //procedure OpenBox();
    { печать отчета с БЭП }
    //procedure PrintFMReport(AReportType: Integer; ADateStart, ADateEnd: TDateTime;
    //  ANumStart, ANumEnd: Integer);
    { печать отчета }
    //procedure PrintReport(AReportType: Integer);
    { перерегистрация }
    //procedure PutHdrFM();
    {  запись налоговых ставок в БЭП }
    //procedure PutTaxFM();
    { установка времени }
    //procedure SetClock(ADateTime: TDateTime);
    { состояние модуля СКНО }
    //procedure SknoState();
    { звуковой сигнал }
    //procedure Sound(ALen, AFreq: Integer);
    { состояние текущего документа }
    //procedure State();

    { Запустить обнаружение ФР в сети }
    procedure Discover();

    procedure SendFrDoc(AFrDoc: TFrDoc);

    property DevAddr: string read FDevAddr write FDevAddr;
    property DevLogin: string read FDevLogin write FDevLogin;
    property DevPassw: string read FDevPassw write FDevPassw;

    property DataPort: TDataPort read FDataPort write FDataPort;

    property DevInfo: TFrDevInfo read FDevInfo;
  end;

implementation

uses
  JsonStorage;

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

function IsoDateToDateTime(AIsoDate: string): TDateTime;
begin
  //
end;

const

  { Список ошибок кассы }
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
end;

procedure TTitanDriver.BeforeDestruction();
begin
  FreeAndNil(FFrAddrList);
  inherited BeforeDestruction();
end;

procedure TTitanDriver.SendRequest(AUrl, AJson: string);
var
  TmpReq: TFrRequest;
begin
  TmpReq := TFrRequest.Create();
  TmpReq.SendTimestamp := Now();
  TmpReq.RequestUrl := AUrl;
  TmpReq.RequestData := AJson;
  TmpReq.ResultJson := '';
end;

procedure TTitanDriver.ParseDevInfo(AData: IDataStorage);
begin
  FDevInfo.SerialNum := AData.GetString('dev_zn');
  FDevInfo.Version := AData.GetString('dev_ver');
  FDevInfo.SwDate := IsoDateToDateTime(AData.GetString('dev_dat'));
  FDevInfo.SknoNum := AData.GetString('skno_num');
  FDevInfo.RegNum := AData.GetString('reg_num');
  FDevInfo.UnpNum := AData.GetString('unp_num');
  FDevInfo.ProtocolVersion := AData.GetString('prot');
end;

procedure TTitanDriver.ParseDevState(AData: IDataStorage);
begin
  FDevInfo.Model := AData.GetString('model');
  FDevInfo.Name := AData.GetString('name');
  FDevInfo.SerialNum := AData.GetString('serial');
  FDevInfo.ChkId := AData.GetInteger('chkId');
  FDevInfo.JrnTime := IsoDateToDateTime(AData.GetString('JrnTime'));
  FDevInfo.CurrZ := AData.GetInteger('currZ');
  FDevInfo.IsWrk := AData.GetBool('IsWrk');
  FDevInfo.IsFiscalization := AData.GetInteger('Fiscalization');
  FDevInfo.IsFskMode := AData.GetInteger('FskMode');
  FDevInfo.SknoState := AData.GetString('SKNOState');
  FDevInfo.Err := AData.GetString('err');
end;

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
end;

procedure TTitanDriver.Discover();
var
  s: AnsiString;
begin
  s := 'M-SEARCH * HTTP/1.1' + #13 + #10
     + 'Host:239.255.255.250:1900' + #13 + #10
     + 'ST:urn:help-micro.kiev.ua:device:webdev:1' + #13 + #10
     + 'Man:"ssdp:discover"' + #13 + #10
     + 'MX:3' + #13 + #10 + #13 + #10;

  FFrAddrList.Clear();

  if not Assigned(FDataPortUdp) then
    FDataPortUdp := TDataPortUDP.Create(Self);

  FDataPortUdp.OnDataAppear := @OnUdpDataAppearHandler;
  FDataPortUdp.Open('239.255.255.250:1900');
  FDataPortUdp.Push(s);
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

