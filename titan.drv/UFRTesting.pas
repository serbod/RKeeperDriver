library UFRTesting;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
  {$IFnDEF FPC}
  xmldom, XMLDoc, XMLIntf, Windows,
  {$ELSE}
  LCLIntf, LCLType, LMessages, DOM, UfrXmlToolsFp,
  {$ENDIF}
  Classes, SysUtils, ActiveX, IniFiles,
  UfrTypes, TitanDrv;

const
  //Номер версии драйвера
  CNST__DRIVER_VERSION = 6;
  //Описание драйвера
  CNST__DRIVER_DESC = 'Titan Driver for UniFR';

type
  TArray4096OfByte = array[0..4095] of Byte;

  //поля LastShiftNum, SaledValue, CashRegValue, LastDocNum, LastReceiptNum и ShiftState нужны только для тестового драйвера
  TPrintDeviceCfg = record
    ComNum, DeviceNumber, LastErrorCode, LastShiftNum: Integer;
    LastDocNum, LastReceiptNum: DWord;
    SaledValue, CashRegValue: Int64;
    ShiftState: TShiftState;
    //    MainAdminPass, Oper :DWord;
    LastErrorMess: string;
    InterfaceCallback: TInterfaceCallbackProc;

    DevAddr: string;
    TitanDriver: TTitanDriver;
  end;

  TArrayOfByte = array of Byte;

const
  MessDeviceNotReady = 'Device not ready';
  MessShiftMore24 = 'Shift more than 24 hours';
  MessInvalidXML = 'Invalid XML parameters';
  MessItemsPaysDifferent = 'Goods price is differ than cash paid';

var
  PrintDevices: array of TPrintDeviceCfg = nil;
  SendCommandSection: TRTLCriticalSection;
  stCustomError, WhereMe: string;
  CustomErrorCode: Integer;

  CriticalSections: array[0..8] of TRTLCriticalSection;

  //пароль 1-го оператора 33554433

  //Вызывать InterfaceCallback надо только из того потока, из которого идет вызов экспортируемых функций dll(чаще всего это будет главный поток приложения).
  //Это надо: во-первых, чтобы юзер мог менять GUI в InterfaceCallback(т.к. в потоке dll не работает Synchronize и его "ручные" аналоги),
  //во-вторых, чтобы у юзера не возникало проблем с "отложенным" вызововом Callback'ов, т.е. когда юзер вызвал уже следующую экспортируемую процедуру, а
  //Callback'и еще выполняются для предыдущей. С другой стороны, нельзя чтобы Callback'и тормозили выполнение команд, нарушая тайм-ауты протокала ФР(нельзя
  //ждать, пока они у юзера завершат работу). Поэтому в процессе выполнения SendCommandAndGetAnswer(как и экспортируемых процедур) копим логи в очередь, а в
  //конце процедуры последовательно вызываем Callback'и для всей очереди и очищаем ее
type
  TLogEvent = record
    DevIndex: Integer;
    FRLog: TFRLog;
  end;
var
  LogEvents: array of TLogEvent = nil;
  LogEventsSection: TRTLCriticalSection;

procedure FlushLogsQueue();
var
  j: Integer;
begin
  if (Length(LogEvents) = 0) or (Length(PrintDevices) = 0) then
    Exit;
  EnterCriticalSection(LogEventsSection);
  for j := 0 to High(LogEvents) do
    with LogEvents[j] do
    begin
      PrintDevices[DevIndex].InterfaceCallback(
        hInstance, PrintDevices[DevIndex].DeviceNumber, cpfLog, 1, @FRLog);
      try
        FreeMem(FRLog.TextData);
        if FRLog.BinDataSize <> 0 then
          FreeMem(FRLog.BinData);
      except
      end;
    end;
  LogEvents := nil;
  LeaveCriticalSection(LogEventsSection);
end;

procedure AddLogEvent(const _DevIndex, _LogEventType: Integer;
  stTextData: string; const _BinDataSize: Integer = 0; const _BinData: Pointer = nil);
var
  j: Integer;
begin
  if not Assigned(PrintDevices[_DevIndex].InterfaceCallback) then
    Exit;
  EnterCriticalSection(LogEventsSection);
  j := Length(LogEvents);
  SetLength(LogEvents, j + 1);
  with LogEvents[j], FRLog do
  begin
    DevIndex := _DevIndex;
    Size := SizeOf(TFRLog);
    LogEventType := _LogEventType;
    stTextData := stTextData + #0;
    GetMem(TextData, Length(stTextData));
    Move(stTextData[1], TextData^, Length(stTextData));
    BinDataSize := _BinDataSize;
    if BinDataSize <> 0 then
    begin
      GetMem(BinData, BinDataSize);
      Move(_BinData^, BinData^, BinDataSize);
    end;
  end;
  LeaveCriticalSection(LogEventsSection);
end;

function GetDeviceIndex(const DeviceNumber: Integer): Integer;
begin
  Result := 0;
  while Result <= High(PrintDevices) do
  begin
    if PrintDevices[Result].DeviceNumber = DeviceNumber then
      Exit;
    Inc(Result);
  end;
  Result := 0;
  //если юзер неверно задал DeviceNumber, то считаем что 0
end;

procedure SaveConfig(const Number: Integer);
var
  DevInd: Integer;
begin
  DevInd := GetDeviceIndex(Number);
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'UFR_TitanDrv.ini') do
  begin
    WriteInteger('Counters Configuration', 'LastDocNum' + IntToStr(
      Number), PrintDevices[DevInd].LastDocNum);
    WriteInteger('Counters Configuration', 'LastReceiptNum' + IntToStr(
      Number), PrintDevices[DevInd].LastReceiptNum);
    WriteInteger('Counters Configuration', 'LastShiftNum' + IntToStr(
      Number), PrintDevices[DevInd].LastShiftNum);
    WriteInteger('Money Counters', 'SaledValue' + IntToStr(Number),
      PrintDevices[DevInd].SaledValue);
    WriteInteger('Money Counters', 'CashRegValue' + IntToStr(Number),
      PrintDevices[DevInd].CashRegValue);

    WriteString('Device', 'DevAddr', PrintDevices[DevInd].DevAddr);
    Free();
  end;
end;

procedure LoadConfig(const DevNumber: Integer);
var
  DevInd: Integer;
begin
  DevInd := GetDeviceIndex(DevNumber);
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'UFR_TitanDrv.ini') do
  begin
    PrintDevices[DevInd].LastDocNum := ReadInteger('Counters Configuration',
      'LastDocNum' + IntToStr(DevNumber), 0);
    PrintDevices[DevInd].LastReceiptNum := ReadInteger('Counters Configuration',
      'LastReceiptNum' + IntToStr(DevNumber), 0);
    PrintDevices[DevInd].LastShiftNum := ReadInteger('Counters Configuration',
      'LastShiftNum' + IntToStr(DevNumber), 0);
    PrintDevices[DevInd].SaledValue := ReadInteger('Money Counters', 'SaledValue' + IntToStr(DevNumber), 0);
    PrintDevices[DevInd].CashRegValue := ReadInteger('Money Counters', 'CashRegValue' +
      IntToStr(DevNumber), 0);

    PrintDevices[DevInd].DevAddr := ReadString('Device', 'DevAddr', '192.168.1.48');
    Free();
  end;

end;

function Int64ToCurr(AValue: Int64): Currency;
begin
  Result := AValue / 100;
end;

procedure _UFRDone(DevNumber: Integer);
var
  i, j: Integer;
begin
  FlushLogsQueue;
  for i := 0 to High(PrintDevices) do
  begin
    if PrintDevices[i].DeviceNumber = DevNumber then
    begin
      SaveConfig(DevNumber);
      CoUninitialize();

      if Assigned(PrintDevices[i].TitanDriver) then
        FreeAndNil(PrintDevices[i].TitanDriver);

      for j := i to High(PrintDevices) - 1 do
        PrintDevices[j] := PrintDevices[j + 1];
      SetLength(PrintDevices, High(PrintDevices));
      if Length(PrintDevices) = 0 then
      begin
        {$ifdef FPC}
        DoneCriticalSection(SendCommandSection);
        DoneCriticalSection(LogEventsSection);
        {$else}
        DeleteCriticalSection(SendCommandSection);
        DeleteCriticalSection(LogEventsSection);
        {$endif}
      end;
      Exit;
    end;
  end;
end;

procedure UFRDone(DevNumber: Integer); stdcall;
var
  j: Integer;
begin
  if Length(PrintDevices) = 0 then
    Exit;
  EnterCriticalSection(CriticalSections[1]);
  _UFRDone(DevNumber);
  LeaveCriticalSection(CriticalSections[1]);
  if Length(PrintDevices) = 0 then
    for j := 0 to High(CriticalSections) do
      DoneCriticalSection(CriticalSections[j]);
end;

function UFRMaxProtocolSupported: Integer; stdcall;
begin
  Result := 1;
end;

function WriteLog(const stLog: string): Boolean;
var
  fLog: TextFile;
begin
  Result := False;
  AssignFile(fLog, WhereMe + 'UFR_TitanDrv.log');
  if FileExists(WhereMe + 'UFR_TitanDrv.log') then
    Append(fLog)
  else
    Rewrite(fLog);
  if IOResult <> 0 then
    Exit;
  WriteLn(fLog, stLog);
  CloseFile(fLog);
  Result := True;
end;

function _UFRInit(DevNumber: Integer; XMLParams: PChar;
  InterfaceCallback: TInterfaceCallbackProc;
  PropCallback: TPropCallbackProc): Integer;
var
  XMLDocument: TDOMDocument;
  XMLNode: TDOMNode;
  j, MaxLen: Integer;
  fErr: TextFile;
begin
  if Length(PrintDevices) = 0 then
  begin
    WhereMe := ExtractFilePath(ParamStr(0));
    stCustomError := '';
    CustomErrorCode := 0;
    AssignFile(fErr, WhereMe + 'FRNextOpError.txt');
    Reset(fErr);
    if IOResult = 0 then
    begin
      Read(fErr, stCustomError);
      try
        j := Pos(' ', stCustomError);
        CustomErrorCode := StrToInt(Copy(stCustomError, 1, j - 1));
        Delete(stCustomError, 1, j);
      except
        stCustomError := '';
      end;
      CloseFile(fErr);
    end;

    InitCriticalSection(SendCommandSection);
    InitCriticalSection(LogEventsSection);
  end;

{
Result:=-1;
}

  Result := 0;
  CoInitialize(nil);
  XMLDocument := CreateXMLDoc(XMLParams);
  try
    if XMLDocument = nil then
    begin
      Result := errAssertInvalidXMLInitializationParams;
      Exit;
    end;

    WriteLog(Format(
      'UFRInit (%d)-----------------------------------------------------------------'#13#10,
      [DevNumber]) + XMLParams);

    MaxLen := Length(PrintDevices);
    SetLength(PrintDevices, MaxLen + 1);
    PrintDevices[MaxLen].InterfaceCallback := InterfaceCallback;

    PrintDevices[MaxLen].TitanDriver := TTitanDriver.Create(nil);

    with PrintDevices[MaxLen] do
      try
        DeviceNumber := DevNumber;
        AddLogEvent(MaxLen, letInitializing, 'Driver initializing');
        XMLNode := GetFirstNode(XMLDocument);
        XMLNode := XMLNode.firstChild;
        LastErrorCode := 0;
        LastErrorMess := '';

        LoadConfig(DevNumber);
        PrintDevices[MaxLen].TitanDriver.DevAddr := PrintDevices[MaxLen].DevAddr;
        if LastReceiptNum > 0 then
          ShiftState := ssShiftOpened
        else
          ShiftState := ssShiftClosed;
      except
        Result := errLogicError;
        AddLogEvent(MaxLen, letError, 'Driver initializing error');
        SetLength(PrintDevices, High(PrintDevices));
        UFRDone(DevNumber);
        Exit;
      end;
  finally
    if Result <> 0 then
      CoUninitialize;
    XMLDocument.Free();
  end;
  AddLogEvent(MaxLen, letInitialized, 'Driver successfully initialized');
end;

function _UFRGetStatus(Number: Integer; var Status: TUFRStatus;
  FieldsNeeded: DWord): Integer;
var
  DevInd: Integer;
  td: TTitanDriver;
begin
  Result := 0;
  DevInd := GetDeviceIndex(Number);
  AddLogEvent(DevInd, letOther, 'Getting the device status');
  if FlagPresented(FieldsNeeded, [fnBusy, fnShiftState, fnUnfiscalPrint,
    fnSerialNumber, fnLastShiftNum, fnCashRegValue]) then
  begin
    td := PrintDevices[DevInd].TitanDriver;
    td.GetDevState();
    td.GetDocState();
    td.LastReceipt();
    if td.IsStateUpdated then
    begin
      td.IsStateUpdated := False;
      Status.Busy := td.Busy;
      Status.SerialNum := td.DevInfo.Model + ' ' + td.DevInfo.SerialNum;
      Status.LastShiftNum := td.DevInfo.CurrZ;
    end;

    //Status.Busy := False;
    Status.ShiftState := PrintDevices[DevInd].ShiftState;
    Status.CanNotPrintUnfiscal := False;
    //Status.LastShiftNum := PrintDevices[DevInd].LastShiftNum;
    // Сумма в кассе, в копейках
    Status.CashRegValue := PrintDevices[DevInd].CashRegValue;
    //необнуляемый итог
    Status.SaledValue := PrintDevices[DevInd].SaledValue;
  end;
  if (FieldsNeeded and fnLastDocNum) <> 0 then
  begin
    //Status.LastDocNum := PrintDevices[DevInd].LastDocNum;
    //Status.LastReceiptNum := PrintDevices[DevInd].LastReceiptNum;

    // Последний номер документа (в том числе инкассации и т.п.)
    Status.LastDocNum := td.DevInfo.ChkId;
    // Последний номер чека
    Status.LastReceiptNum := td.LastDocInfo.DocNum;
  end;
  if Result = 0 then
    AddLogEvent(DevInd, letOther, 'The device status has successfully obtained');
end;

procedure SaveError(const DevInd, errCode: Integer);
var
  errMess: string;
begin
  case errCode of
    errLogicPrinterNotReady: errMess := MessDeviceNotReady;
    errLogic24hour: errMess := MessShiftMore24;
    errAssertInvalidXMLParams: errMess := MessInvalidXML;
    errAssertItemsPaysDifferent: errMess := MessItemsPaysDifferent;
    0: Exit;
  end;
  PrintDevices[DevInd].LastErrorCode := errCode;
  PrintDevices[DevInd].LastErrorMess := errMess;
end;

function _UFRUnfiscalPrint(Number: Integer; XMLBuffer: PChar; Flags: Integer): Integer;
const
  BarCodeTypes: array[0..8] of string =
    ('UPCA', 'UPCE', 'EAN13', 'EAN8', 'CODE39', 'ITF', 'CODABAR', 'CODE93', 'CODE128');
  TextPosition: array[0..3] of string = ('NO', 'TOP', 'BOTTOM', 'TOP&BOTTOM');
var
  XMLDocument: TXMLDocument;
  XMLNode: TDOMNode;
  sBarCodeType, st: string;
  DevInd: Integer;
  td: TTitanDriver;

  tmpDoc: TFrDoc;
begin
  DevInd := GetDeviceIndex(Number);
  AddLogEvent(DevInd, letOther, 'Start the non-fiscal printing');

  td := PrintDevices[DevInd].TitanDriver;
  td.GetDevState();

  //Result:=0;

  XMLDocument := CreateXMLDoc(XMLBuffer);
  if XMLDocument = nil then
  begin
    Result := errAssertInvalidXMLParams;
    AddLogEvent(DevInd, letError, MessInvalidXML);
    SaveError(DevInd, errAssertInvalidXMLParams);
    Exit;
  end;
  WriteLog('UFRUnfiscalPrint -----------------------------------------------------------------'#13#10 + XMLBuffer);

  tmpDoc := TFrDoc.Create(frdNonFiscal);

  try
    XMLNode := GetFirstNode(XMLDocument);

    XMLNode := XMLNode.firstChild;


    while XMLNode <> nil do
    begin
      if NodeNameIs(XMLNode, 'TextBlock')
      or NodeNameIs(XMLNode, 'TextLine')
      or NodeNameIs(XMLNode, 'TextPart') then
      begin
        if XMLNode.NodeName[5] = 'B' then
          st := string(XMLNode.FirstChild.NodeValue) //печатаемые строки текста
        else
          st := FindAttrByName(XMLNode, 'Text');

        tmpDoc.AddText(st);
      end
      else
      if NodeNameIs(XMLNode, 'BarCode') then   //печать штрих-кода
      begin
        sBarCodeType := FindAttrByName(XMLNode, 'Type');
        st := FindAttrByName(XMLNode, 'Value');
        if (sBarCodeType = 'EAN13')
        or (sBarCodeType = 'EAN-13') then
        begin
          tmpDoc.AddBarcode(st, 1);
        end;
      end
      else
      if NodeNameIs(XMLNode, 'Pass') then
      begin
      end
      else
      if NodeNameIs(XMLNode, 'Wait') then
        Sleep(GetIntValByName(XMLNode, 'MSecs', 1));
      XMLNode := XMLNode.nextSibling;
    end;

    XMLNode := GetFirstNode(XMLDocument);
    if GetIntValByName(XMLNode, 'CutAfter', 0) = 1 then
    begin
    end;

    td.SendFrDoc(tmpDoc);

    stCustomError := td.DevInfo.Err;

    if stCustomError = '' then
    begin
      Result := 0;

      AddLogEvent(DevInd, letOther,
        'The non-fiscal printing has been successfully completed');
    end
    else
    begin
      Result := CustomErrorCode;

      AddLogEvent(DevInd, letError, stCustomError);
    end;

    SaveError(DevInd, Result);

  finally
    tmpDoc.Free();
    if Assigned(XMLDocument) then
      FreeAndNil(XMLDocument);
  end;

  WriteLog('<Printing is complete>'#13#10);
end;

function _UFRFiscalDocument(Number: Integer; XMLDoc: PChar;
  var Status: TUFRStatus; var FieldsFilled: cardinal): Integer;
const
  stDocType: array[0..7] of string =
    ('Receipt', 'Return', 'Deletion', 'ReceiptCopy', 'CashInOut', 'CollectAll',
    'Report', 'Invoice');
  SuccessfullyCompleted = 'The fiscal printing has been successfully completed';

var
  XMLDocument: TXMLDocument;
  MainNode: TDOMNode;
  st: string;
  ReportType: string;
  DevInd, DocType: Integer;
  ItemsPresented, UnfiscalPresented, ReceiptOpened: Boolean;
  CashRes: int64; //это только для отладочной версии

  td: TTitanDriver;
  tmpDoc: TFrDoc;

  //при pPrint=False только проверяется равенство цены покупаемых товаров и заплаченными за них деньгами
  function _PrintFiscal(const pPrint: Boolean): Integer;
  var
    XMLNode, XMLNode2, XMLNode3, XMLNode4: TDOMNode;
    st2: string;
    i, j, Discount: Integer;
    Quantity, PricePerOne, Val64, Val64_2, Sum: int64;
  begin
    Result := 0;
    Sum := 0;
    CashRes := 0;
    ReceiptOpened := False;
    tmpDoc := TFrDoc.Create(frdFiscal);

    //признак что начали печатать чек(его фискальную часть)
    while MainNode <> nil do
    begin
      if NodeNameIs(MainNode, 'Header') then
      begin
        XMLNode := MainNode.FirstChild;
        while XMLNode <> nil do
        begin
          if NodeNameIs(XMLNode, 'Unfiscal') and XMLNode.HasChildNodes then
          begin
            st := string(XMLNode.FirstChild.nodeValue);
            if st <> '' then
              UnfiscalPresented := True
            else
            begin
              XMLNode2 := XMLNode.FirstChild;
              while XMLNode2 <> nil do
              begin
                UnfiscalPresented := True;
                st := FindAttrByName(XMLNode2, 'Text');
                if st = '' then
                  st := ' '
                else
                begin
                  tmpDoc.AddText(st);
                end;
                XMLNode2 := XMLNode2.NextSibling;
              end;
            end;
          end;
          XMLNode := XMLNode.NextSibling;
        end;
      end
      else

      if NodeNameIs(MainNode, 'Receipt') and (DocType < 3) then
      begin
        XMLNode := MainNode.FirstChild;
        while XMLNode <> nil do
        begin
          if NodeNameIs(XMLNode, 'Items') then
          begin
            XMLNode2 := XMLNode.FirstChild;
            while XMLNode2 <> nil do
            begin
              if NodeNameIs(XMLNode2, 'Item') then
              begin
                ItemsPresented := True;
                st := FindAttrByName(XMLNode2, 'Name');
                if Length(st) > 40 then
                  st := FindAttrByName(XMLNode2, 'ShortName');
                //в ФР указывается цена одного товара, в XML-параметрах может быть указана как цена одного товара(PricePerOne), так и
                //общая цена(Value=цена одного*количество) товаров. Параметр PricePerOne надо использовать в первую очередь.
                //количество указывается в тысячах как для ФР, так и в XML-параметрах, т.е. 1шт = 1000, 2шт = 2000 и т.п.
                Val64 := GetIntValByName(XMLNode2, 'Quantity', 1000);
                Quantity := Val64;
                if FindAttrByName(XMLNode2, 'PricePerOne') <> '' then
                begin
                  //если указана стоимость одного товара, то надо использовать ее
                  PricePerOne := GetIntValByName(XMLNode2, 'PricePerOne', 0);

                  //получаем цену всех товаров для подсчета Sum
                  if FindAttrByName(XMLNode2, 'Value') <> '' then
                    Val64 := GetIntValByName(XMLNode2, 'Value', 0)
                  else
                    Val64 := PricePerOne * Quantity div 1000;
                  Inc(Sum, Val64);
                end
                else
                begin
                  //иначе используем параметр Value с ценой всех товаров
                  Val64 := GetIntValByName(XMLNode2, 'Value', 0);
                  Inc(Sum, Val64);
                  //цена единицы товара
                  PricePerOne := Val64 * 1000 div Quantity;
                end;

                // --- добавление строки продажи товара
                tmpDoc.AddSale(st, '', Int64ToCurr(PricePerOne), (Currency(Quantity) / 1000));

                XMLNode3 := XMLNode2.firstChild;
                i := 1;
                st2 := '';
                while XMLNode3 <> nil do
                begin
                  if NodeNameIs(XMLNode3, 'Taxes') then
                  begin
                    XMLNode4 := XMLNode3.firstChild;
                    while XMLNode4 <> nil do
                    begin
                      if NodeNameIs(XMLNode4, 'Tax') then
                      begin
                        if i < 5 then
                          Inc(i);
                      end;
                      XMLNode4 := XMLNode4.nextSibling;
                    end;
                  end
                  else
                  if NodeNameIs(XMLNode3, 'Discounts') then
                  begin
                    //скидки и надбавки
                    XMLNode4 := XMLNode3.firstChild;
                    while XMLNode4 <> nil do
                    begin
                      if NodeNameIs(XMLNode4, 'Discount') then
                      begin
                        Val64 := GetIntValByName(XMLNode4, 'Value', 0);
                        Inc(Sum, Val64);
                        if Val64 < 0 then
                          Val64 := Abs(Val64); //скидка
                      end;
                      XMLNode4 := XMLNode4.nextSibling;
                    end;
                  end
                  else
                  if NodeNameIs(XMLNode3, 'Unfiscal') then
                    st2 := string(XMLNode3.firstChild.nodeValue);
                  XMLNode3 := XMLNode3.nextSibling;
                end;
                if pPrint then
                begin
                  ReceiptOpened := True;
                end;
              end;
              XMLNode2 := XMLNode2.nextSibling;
            end;
          end
          else
          if NodeNameIs(XMLNode, 'Discounts') then
          begin
            //скидки и надбавки
            Discount := 0;
            XMLNode2 := XMLNode.firstChild;
            while XMLNode2 <> nil do
            begin
              if NodeNameIs(XMLNode2, 'Discount') then
              begin
                Val64 := GetIntValByName(XMLNode2, 'Value', 0);
                Inc(Sum, Val64);
                Inc(Discount, Val64);
                // --- добавление скидки
                tmpDoc.AddDiscount(Int64ToCurr(Val64), 0);
              end;
              XMLNode2 := XMLNode2.nextSibling;
            end;
            if pPrint and ItemsPresented then
            begin
            end;
          end
          else
          if NodeNameIs(XMLNode, 'Payments') then
          begin
            //расплата и закрытие чека
            XMLNode2 := XMLNode.firstChild;
            Inc(CashRes, Sum);
            while XMLNode2 <> nil do
            begin
              if NodeNameIs(XMLNode2, 'Payment') then
              begin
                j := GetIntValByName(XMLNode2, 'TypeIndex', 0);
                if j > 4 then
                  j := 4;
                Val64_2 := GetIntValByName(XMLNode2, 'Value', 1);
                Dec(Sum, Val64_2);
                // --- добавление строки оплаты
                tmpDoc.AddPayment(Int64ToCurr(Val64_2), j);
              end;
              XMLNode2 := XMLNode2.nextSibling;
            end;
            if pPrint and ItemsPresented then
            begin
              ReceiptOpened := False;
            end;
          end;
          XMLNode := XMLNode.nextSibling;
        end;
      end
      else
      if NodeNameIs(MainNode, 'Payment') and (DocType in [4, 8]) and pPrint then
      begin
        //внесение или выплата
        Val64 := GetIntValByName(MainNode, 'Value', 0);
        Inc(CashRes, Val64);
      end;

      MainNode := MainNode.nextSibling;
    end;

    // --- Отправка чека на печать
    td.SendFrDoc(tmpDoc);
    tmpDoc.Free();

    if Sum <> 0 then
      Result := errAssertItemsPaysDifferent;
    //внесенная оплата не равна цене покупаемого товара
    SaveError(DevInd, Result);
  end;

  procedure _XMLError();
  begin
    Result := errAssertInvalidXMLParams;
    AddLogEvent(DevInd, letError, MessInvalidXML);
    SaveError(DevInd, errAssertInvalidXMLParams);
  end;

begin
  FillChar(Status, SizeOf(Status), 0);
  Result := 0;
  DevInd := GetDeviceIndex(Number);
  XMLDocument := CreateXMLDoc(XMLDoc);
  if XMLDocument = nil then
  begin
    _XMLError();
    Exit;
  end;

  WriteLog('UFRFiscalDocument -----------------------------------------------------------------'#13#10 + XMLDoc);

  td := PrintDevices[DevInd].TitanDriver;
  td.GetDevState();

  MainNode := GetFirstNode(XMLDocument);
  st := UpperCase(FindAttrByName(MainNode, 'DocType'));
  DocType := 0;
  while DocType <= High(stDocType) do
  begin
    if st = stDocType[DocType] then
      Break;
    Inc(DocType);
  end;

  MainNode := MainNode.firstChild;
  ReportType := '';
  if DocType in [3, 5, 6] then
  begin
    case DocType of
      3:
      begin  //ReceiptCopy(копия последнего чека)
      end;
      5:
      begin  //CollectAll(выплата всех денег из кассы)
        PrintDevices[DevInd].CashRegValue := 0;
        SaveConfig(Number);
      end;
      6:
      begin  //отчеты
        while (MainNode <> nil) and (not NodeNameIs(MainNode, 'Report')) do
          MainNode := MainNode.nextSibling;
        if MainNode = nil then
          Exit;

        ReportType := UpperCase(FindAttrByName(MainNode, 'ReportType'));
        if ReportType = 'X' then
        begin
          // --- печать X-отчета
          td.PrintReport(REPORT_X1);
        end
        else
        if ReportType = 'Z' then
        begin
          // --- печать Z-отчета
          td.PrintReport(REPORT_Z1);
        end
        else
        if (ReportType = 'BRIEFBYDATE') or (ReportType = 'DETAILBYDATE') then
        begin
          st := FindAttrByName(MainNode, 'Parameters');
          {DateFrom := ;
          DateTo := ;
          if (ReportType = 'BRIEFBYDATE') then
            td.PrintFMReport(FM_REPORT_SHORT_BY_DATE, DateFrom, DateTo)
          else if (ReportType = 'DETAILBYDATE') then
            td.PrintFMReport(FM_REPORT_FULL_BY_DATE, DateFrom, DateTo);}
        end
        else
        if (ReportType = 'BRIEFBYNUMBER') or (ReportType = 'DETAILBYNUMBER') then
        begin
        end
        else
          AddLogEvent(DevInd, letOther, SuccessfullyCompleted);
      end;
    end;
    if stCustomError = '' then
      AddLogEvent(DevInd, letOther, SuccessfullyCompleted)
    else
    begin
      Result := CustomErrorCode;
      AddLogEvent(DevInd, letError, stCustomError);
    end;
    SaveError(DevInd, Result);
    if Result = 0 then
    begin
      Inc(PrintDevices[DevInd].LastDocNum);
      if (DocType = 6) and (ReportType = 'Z') then  //Z-report
      begin
        PrintDevices[DevInd].LastDocNum := 0;
        PrintDevices[DevInd].LastReceiptNum := 0;
        PrintDevices[DevInd].SaledValue := 0;
        Inc(PrintDevices[DevInd].LastShiftNum);
        PrintDevices[DevInd].ShiftState := ssShiftClosed;
        SaveConfig(Number);
      end;
    end;
    FreeAndNil(XMLDocument);
    Exit;
  end;

  ItemsPresented := False;
  UnfiscalPresented := False;
  Result := _PrintFiscal(False);
  if Result = 0 then
  begin
    //      AutoClosingShift(DevInd);
    AddLogEvent(DevInd, letFiscCommand, 'Start the fiscal printing');
    MainNode := GetFirstNode(XMLDocument);
    MainNode := MainNode.firstChild;
    Result := _PrintFiscal(True);

    if stCustomError = '' then
    begin
      if Result = 0 then
        AddLogEvent(DevInd, letOther,
          'The fiscal printing has been successfully completed');
    end
    else
    begin
      Result := CustomErrorCode;
      AddLogEvent(DevInd, letError, stCustomError);
    end;
  end;
  if Result = 0 then
  begin
    Inc(PrintDevices[DevInd].LastDocNum);
    if DocType in [0, 7] then
    begin
      Inc(PrintDevices[DevInd].LastReceiptNum);
      PrintDevices[DevInd].ShiftState := ssShiftOpened;
    end;
    if DocType in [0, 1, 2] then
      Inc(PrintDevices[DevInd].SaledValue, CashRes);
    Inc(PrintDevices[DevInd].CashRegValue, CashRes);
    SaveConfig(Number);
  end;

  FreeAndNil(XMLDocument);
  WriteLog('<Printing is complete>'#13#10);
end;

function _UFRProgram(Number: Integer; XMLDoc: PChar): Integer;
var
  XMLDocument: TXMLDocument;
  XMLNode, XMLNode2: TDOMNode;
  TaxName, NewName, DefaultDepartmentName: string;
  DevInd: Integer;
  _YearHour, _MonthMin, _DaySec, TaxValue: word;

begin
  Result := 0;

  DevInd := GetDeviceIndex(Number);

  XMLDocument := CreateXMLDoc(XMLDoc);

  if XMLDocument = nil then
  begin
    Result := errAssertInvalidXMLParams;
    AddLogEvent(DevInd, letError, MessInvalidXML);
    SaveError(DevInd, errAssertInvalidXMLParams);
    Exit;
  end;
  XMLNode := GetFirstNode(XMLDocument);
  if XMLNode = nil then
  begin
    FreeAndNil(XMLDocument);
    Exit;
  end;
  XMLNode := XMLNode.FirstChild;
  while XMLNode <> nil do
  begin
    if NodeNameIs(XMLNode, 'ProgramDateTime') then
    begin
      if UpperCase(FindAttrByName(XMLNode, 'Source')) = 'SYSTEM' then
      begin
        //дата
        DecodeDate(Now(), _YearHour, _MonthMin, _DaySec);
        Dec(_YearHour, 2000);
        //время
        DecodeTime(Time, _YearHour, _MonthMin, _DaySec, TaxValue);
      end;
    end
    else
    if NodeNameIs(XMLNode, 'ProgramTaxes') then
    begin
      XMLNode2 := XMLNode.FirstChild;
      while XMLNode2 <> nil do
      begin
        TaxName := FindAttrByName(XMLNode2, 'TaxName');
        TaxValue := GetIntValByName(XMLNode2, 'RateValue', 1300);
        //делаем по умолчанию налог 13
        XMLNode2 := XMLNode2.NextSibling;
      end;
    end
    else
    if NodeNameIs(XMLNode, 'ProgramDepartments') then
    begin
      DefaultDepartmentName := FindAttrByName(XMLNode, 'DefaultDepartmentName');
      XMLNode2 := XMLNode.FirstChild;
      while XMLNode2 <> nil do
      begin
        //формируем данные для программирования параметров отдела
        NewName := FindAttrByName(XMLNode2, 'Name');
        if NewName = '' then
          NewName := DefaultDepartmentName;
        XMLNode2 := XMLNode2.NextSibling;
      end;
    end;
    XMLNode := XMLNode.NextSibling;
  end;

  FreeAndNil(XMLDocument);
  SaveError(DevInd, 0);
end;

function _UFROpenDrawer(Number: Integer): Integer;
var
  DevInd: Integer;
begin
  Result := 0;
  DevInd := GetDeviceIndex(Number);
end;

function _UFRGetOptions(Number: Integer; var Options: int64): Integer;
var
  DevInd: Integer;
  //FRVer :String[3];
begin
  DevInd := GetDeviceIndex(Number);
  AddLogEvent(DevInd, letOther, 'Getting the device options');

  Options := foText or foDeleteReceipt or foZReport or
    foMoneyInOut or foXReport or foProgram or
    foTextInReceipt or foZeroReceipt or
    foBarCodeInNotFisc or
    //foCheckCopy or
    foTextInLine or foCalcChange or
    foDrawerOpen or foAllMoneyOut or
    foZWhenClosedShift or foAbsDiscountSum;
  //or foFixedNames
  //or foZClearMoney

  //ФР поддерживает специальные отчёты:
  //  итоговый/детализированный отчет по датам;
  //  итоговый отчет по сменам.
  Options := Options or foSpecialReport;

  //определяем - это ККМ(поддерживает ЭКЛЗ) или ЧПМ(не поддерживает ЭКЛЗ)
{    FRVer:='123';
  FRVer:=CP866ToAnsi(FRVer);
  If FRVer<>'НЕТ' then  Options:=Options or foSpecialReport or foFullLastShift;}
  Result := 0;
  SaveError(DevInd, 0);
  AddLogEvent(DevInd, letOther, 'The device options has successfully obtained');
end;

function _UFRGetZReportData(Number: Integer; XMLData: PChar;
  var XMLDataSize: Integer): Integer; stdcall;
var
  DevInd: Integer;
  stRes: string;
begin
  Result := 0;
  DevInd := GetDeviceIndex(Number);
  AddLogEvent(DevInd, letOther, 'Getting Z-Report data');
  stRes := '<ZReportData> '#13 + '  <DepartmentValues />' +
    '</ZReportData>'#0;
  if (XMLData = nil) or (XMLDataSize < Length(stRes)) then
  begin
    XMLDataSize := Length(stRes);
    Result := errAssertInsufficientBufferSize;
    AddLogEvent(DevInd, letError, 'Insufficient buffer size');
    SaveError(DevInd, errAssertInsufficientBufferSize);
    Exit;
  end;
  Move(stRes[1], XMLData^, Length(stRes));
  AddLogEvent(DevInd, letOther, 'Z-Report has successfully obtained');
end;

function UFRCustomerDisplay(Number: Integer; XMLBuffer: PChar;
  Flags: Integer): Integer; stdcall;
begin
  Result := errFunctionNotSupported;
end;

function UFRInit(DevNumber: Integer; XMLParams: PChar;
  InterfaceCallback: TInterfaceCallbackProc;
  PropCallback: TPropCallbackProc): Integer; stdcall;
var
  j: Integer;
begin
  if Length(PrintDevices) = 0 then
    for j := 0 to High(CriticalSections) do
      InitCriticalSection(CriticalSections[j]);
  EnterCriticalSection(CriticalSections[0]);
  Result := _UFRInit(DevNumber, XMLParams, InterfaceCallback, PropCallback);
  FlushLogsQueue;
  LeaveCriticalSection(CriticalSections[0]);
end;

function UFRGetStatus(Number: Integer; var Status: TUFRStatus;
  FieldsNeeded: DWord): Integer; stdcall;
begin
  EnterCriticalSection(CriticalSections[2]);
  Result := _UFRGetStatus(Number, Status, FieldsNeeded);
  FlushLogsQueue();
  LeaveCriticalSection(CriticalSections[2]);
end;

function UFRUnfiscalPrint(Number: Integer; XMLBuffer: PChar;
  Flags: Integer): Integer; stdcall;
begin
  EnterCriticalSection(CriticalSections[3]);
  Result := _UFRUnfiscalPrint(Number, XMLBuffer, Flags);
  FlushLogsQueue();
  LeaveCriticalSection(CriticalSections[3]);
end;

function UFRFiscalDocument(Number: Integer; XMLDoc: PChar; var Status: TUFRStatus;
  var FieldsFilled: cardinal): Integer; stdcall;
begin
  EnterCriticalSection(CriticalSections[4]);
  Result := _UFRFiscalDocument(Number, XMLDoc, Status, FieldsFilled);
  FlushLogsQueue();
  LeaveCriticalSection(CriticalSections[4]);
end;

function UFRProgram(Number: Integer; XMLDoc: PChar): Integer; stdcall;
begin
  EnterCriticalSection(CriticalSections[5]);
  Result := _UFRProgram(Number, XMLDoc);
  FlushLogsQueue();
  LeaveCriticalSection(CriticalSections[5]);
end;

function UFRGetOptions(Number: Integer; var Options: int64;
  var DriverName, VersionInfo, DriverState: OpenString): Integer; stdcall;
begin
  Result := errLogicPrinterNotReady;
  DriverName := CNST__DRIVER_DESC;
  VersionInfo := SysUtils.IntToStr(CNST__DRIVER_VERSION);
  DriverState := 'No device initialized';
  if PrintDevices = nil then
    Exit;
  DriverState := '';

  EnterCriticalSection(CriticalSections[6]);
  Result := _UFRGetOptions(Number, Options);
  if Options and foSpecialReport <> 0 then
    DriverName := DriverName + 'K';
  FlushLogsQueue();
  LeaveCriticalSection(CriticalSections[6]);
end;

function UFROpenDrawer(Number: Integer; DrawerNum: Integer): Integer; stdcall;
begin
  EnterCriticalSection(CriticalSections[7]);
  Result := _UFROpenDrawer(Number);
  LeaveCriticalSection(CriticalSections[7]);
end;

function UFRGetZReportData(Number: Integer; XMLData: PChar;
  var XMLDataSize: Integer): Integer; stdcall;
begin
  EnterCriticalSection(CriticalSections[8]);
  Result := _UFRGetZReportData(Number, XMLData, XMLDataSize);
  FlushLogsQueue();
  LeaveCriticalSection(CriticalSections[8]);
end;

procedure UFRGetLastLogicError(Number: Integer; var LogicError: TUFRLogicError); stdcall;
begin
  if PrintDevices <> nil then
  begin
    LogicError.LogicError := PrintDevices[GetDeviceIndex(Number)].LastErrorCode;
    LogicError.LogicErrorText := PrintDevices[GetDeviceIndex(Number)].LastErrorMess;
  end;
end;

//для проверки правильности типов
procedure CheckFuncTypes();
begin
  CheckFiscRegFuncTypes(
    UFRMaxProtocolSupported, UFRInit, UFRDone, UFRGetOptions, UFRGetStatus,
    UFRUnfiscalPrint, UFRFiscalDocument, UFRGetZReportData, UFROpenDrawer,
    UFRCustomerDisplay, UFRProgram, UFRGetLastLogicError);
end;

exports
  UFRInit,
  UFRMaxProtocolSupported,
  UFRDone,
  UFRGetStatus,
  UFRUnfiscalPrint,
  UFRFiscalDocument,
  UFRProgram,
  UFRGetOptions,
  UFRCustomerDisplay,
  UFRGetLastLogicError,
  UFROpenDrawer,
  UFRGetZReportData;
end.
