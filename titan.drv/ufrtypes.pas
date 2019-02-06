unit UfrTypes;

{$ifdef FPC}
  {$mode DELPHI}
{$endif}

interface

uses
  Windows;

const

  //stPayNotEqualToPrice = 'Payment is not equal to the goods price';
  errOk = 0;                   // ��� ������

  { ������������� ������ ������������� 1-99 }
  errPortAlreadyUsed      = 1; // ���� ��� ������������
  errIllegalOS            = 2; // �� �� OS
  errProtocolNotSupported = 3; // ������������� ��������  �� ��������������
  errFunctionNotSupported = 4; // ������� �������� �� �������������� ������ ������������
  errInvalidHandle        = 5; // ���������������� ���������� (handle)

  { ������������� ������ ������� ������ 100-199 }
  errLowNotReady          = 101; // ���������� �� ������ ������� �������, ������� ��������
  errLowSendError         = 102; // ���������� �������� ������� ����� �������
  errLowAnswerTimeout     = 103; // ���������� �� �������� �� �������
  errLowInactiveOnExec    = 104; // ���������� �� �������� �� �������� ����������������� ����� �������� �������
  errLowBadAnswer         = 105; // ���������� �������� ������� (� ���������� ��������� ��� ������ �� �����)

  { ���������� ������ 200-299 }
  errLogicError           = 200; // ���������� ������ ������� ����, ��. ������: LogicError, LogicErrorText
  errLogic24hour          = 201; // ����� ��������� ������������ �����������������
  errLogicPrinterNotReady = 202;
  errLogicPaperOut        = 203; // ����������� ������ �� ����� ������.
                          //��� ������� ������� �� ���� ���������� ��� ������.

  { ������ �� ������� ������, ������������ �� �������� ������ � �� 300-399 }
  errAssertItemsPaysDifferent             = 301; // � ���� �� ��������� ����� �� ������� � ��������
  errAssertInvalidXMLInitializationParams = 302; // ������ � XML ���������� UFRInit
  errAssertInvalidXMLParams               = 303; // ������ � XML ���������� ���� �������, ����� UFRInit
  errAssertInsufficientBufferSize         = 304; // ������������� ������ ������ ��� ��������� ������

  { UFRGetOptions }
  FoText             = $0001; // �� ������������ ��������� ������������ ������ (�� � ����)
  foZeroReceipt      = $0002; // �� ������������ ������ �������� ����(� ������� ����������)
  foDeleteReceipt    = $0004; // �� ������������ �������� ����
  foZReport          = $0008; // �� ������������ Z �����
  foMoneyInOut       = $0010; // �� ������������ ��������-������
  foXReport          = $0020; // �� ������������ X �����
  foSpecialReport    = $0040; // �� ������������ ����������� ������
  foZeroSale         = $0080; // �� ������������ ������� � ������� �����
  foProgram          = $0100; // �� ������������ ����������������
  foFullLastShift    = $0200; // �� ������������ ���������� ��������� �����
  foAllMoneyOut      = $0400; // �� ������������ ������� ���� �����
  foTextInReceipt    = $0800; // �� ������������ ������������ ������ ������ ���� (������ <Header>)
  foBarCodeInNotFisc = $1000; // �� ������������ ������ �����-���� � ������������ ����� ����
  foZClearMoney      = $2000; // Z ����� ������������� ������� ������� ����� � �����
  foCheckCopy        = $4000; // ���������� �������� - ����� ���� (���� ������ � ������)
  foTextInLine       = $8000; // �� ������������ ������������ ������ ������ ����� ���� (������ <Item> ��� <Payment> ��� <Discount>)
  foDepartments      = $00010000; // �� ������������ ������ �� ������
  foOnlyFixed        = $00020000; // ������ ������ � ������� �������������������� ������� (RK7 � ���� ������ ���������� ����� "�����" �� ������� � ������ ������� � ������������� "���������" ���� � ������ �������)
  foTextOpenShift    = $00040000; // �������, ��� ������������ ������ ��������� ���������� �����
  foDrawerOpen       = $00080000; // ����� ��������� ����
  foDrawerState      = $00100000; // ����� ���������� ��������� �����
  foCustomerDisplay  = $00200000; // ������������ ����� �� ������� ����������
  foCalcChange       = $00800000; // ������������ ���������� �����
  foZWhenClosedShift = $01000000; // ������������ ������ Z-������ ��� �������� �����
  foAbsDiscountSum   = $02000000; // �� ������������ ���������� �������� ������/�������
  foFiscInvoice      = $04000000; // ������������ ������ ����������� ���� ��� ����-�������
  foCashRegValue     = $08000000; // �� ������������ ������� �������� �������� �������� ����������
  foFixedNames       = $10000000; // �� ������� ������������ ��� ������� � ������� ���

  { �������� ����� FieldsNeeded ��� UFRGetStatus }
  fnNotCancelReceipt = $0001; // �� �������� ������������� �������� ���
  fnBusy             = $0002; // ��������� ��������� Busy � NotReady
  fnUnfiscalPrint    = $0004; // ��������� ��������� CanNotPrintUnfiscal
  fnPrintQueue       = $0008; // ����� ������� ������ (��� ������������ ������)
  fnDrawerOpened     = $0010; // ������� ��������� �����
  fnShiftState       = $0100; // ������ ���������� �����
  fnSerialNumber     = $0200; // ������-������������� �����������, ����� ��������� ������ ����� ��������� �������������� ������ �����������
  fnLastShiftNum     = $0400; // ����� ��������� ��������(!) ���������� �����, ���� ������ �����, �� 0
  fnLastDocNum       = $0800; // ��������� ����� ��������� (������� ��������-������)
  fnLastReceiptNum   = $1000; // ��������� ����� ����������� ����, ����� ��������� � ������� ���������
  fnSaledValue       = $2000; // ����� ������ ��� �� ����� ��� ���������. ������������ ������ ��� �������� ����������/�� ���������� (������ ��� ��� ��������� ���).
  //fnLogicError       = $4000; // ������ ��������� ���������� ������.
  fnCashRegValue     = $8000; // ������ �������� �������� �������� ���������� � ��


  { ������ ������� Callback ��������� }
  cpfDialog            = 1; // ����� �������, ��������� tDialogInfo, ��. ����� ������� �� �������
  cpfProgress          = 2; // ��������� ���������, ��������� tProgressInfo, ��. ����������� ���������
  cpfGetStringDialog   = 3; // ������ ������� ������, ��������� tGetStringInfo, ��. ����� ������� ������� ������
  cpfRegisterSpecMenu  = 4; // ���������������� ����������� ����, ��������� tDriverMenuInfo ��.�������������� ������� (����. ����, ����� � ������� ���������)
  cpfLog               = 5; // �����������

  { ��� ����� }
  letError               = 1; // ������.
  letInitializing        = 2; // � ������ �������������. Data - ������ � ��������� UTF8
  letInitialized         = 3; // ��� �������� �������������. Data - ������ � ��������� UTF8
  letFiscCommand         = 4; // ��������� � �������� ������������� ���������� ������� (� ������ �����, ��������, ��������, ��������, ��������, � �������� ������������� ����� ������ ��� �������� �����������).
  letFiscCommandComplete = 5; // ��������� � �������� ������������� ������ �����������. � ������ ����� ���� ��� ������, ����� ���������� ���������� �������, ��������, ����������� ��������������� ������ �����������, ��������, �� ������ �������. � �������� ������������� ����� ������, ���������� �� �����������.
  letTextCommand         = 6; // ��������� � �������� ������������� ������� ������ ������ (������������ ������, ����� �� ������� ����������), � �������� ������������� ����� ������ ��� �������� �����������.
  letTextCommandComplete = 7; // ��������� � �������� ������������� ������ �����������. � ������ ����� ���� ��� ������, ����� ���������� ���������� �������, ��������, ����������� ��������������� ������ �����������, ��������, ������ ����������� ���������. � �������� ������������� ����� ������, ���������� �� �����������.
  letBinInput            = 8; // �������� ��������� ���� (�� ��������� � letFiscCommandComplete, letTextCommandComplete). ��������, ������������� ������������ � ��������� ������ ������.
  letBinOut              = 9; // �������� ��������� ����� (�� ��������� � letFiscCommand, letTextCommand). ��������, ������������� � ��������� ������ ������ ��� ���������� ����������� � �������� �������.
  letOther               = 10; // ��� ���������

type
  TInterfaceCallbackProc = procedure(ModuleHandle: THandle; {���� ������, ������� �������� ���������}
                                     Number: LongInt; {�����, ������� �������� ��� �����������}
                                     FuncID, ProtocolVersion: LongInt;
                                     FuncParams: Pointer {��������� �� ��������, �������� ���������, ����� ����� ������� ����� ��������(�������)}); stdcall;

  TPropCallbackProc = procedure(XMLTag: PChar; // ��� ��������, ��������� � �����, ��� "Receipt"/"Receipt.Order"/"Receipt.Deletion"/"Receipt.Operator" �������������� ��
                                               // �����, ��� "Item""/"Discount"/"Pay" ����� (�������� Id)
                                Identifier: PChar; // ������������� ��������
                                PropName: PChar; // ��� ��������
                            var PropValue: OpenString); stdcall;

  TShiftState = (ssShiftClosed, ssShiftOpened, ssShiftOpened24hoursExceeded);

  TUFRStatus = packed record
    Size:                LongInt;     // SizeOf(TUFRStatus)

    NotReady:            Boolean;     // �� �����
    Busy:                Boolean;     // �������� [�����]
    CannotPrintUnfiscal: Boolean;     // ���������� ��������� ������������ ������
    QueueSize:           LongInt;     // ������ ������� ��� ������������ ������
                                      // NOTE. �� �����������, ���������� �����
                                      //      ���� �������� �������� � ����������
    DrawerOpened:        Boolean;     // ���� ������
    ShiftState:          TShiftState; // ���������� ����� �������/�������/������� � ������ 24 ����
    SerialNum:           String[35];  // ���������� ������������� �� (� �������)
    LastShiftNum:        Word;        // ��������� ����� Z ������
                                      // (����� �������� �����)
    LastDocNum:          LongInt;     // ��������� ����� ���������
                                      // (� ��� ����� ���������� � �.�.)
    LastReceiptNum:      LongInt;     // ��������� ����� ����
    SaledValue:          Int64;       // ����� ������ �� �����, � ��������
    CashRegValue:        Int64;       // ����� � �����, � ��������
  end;

  TFRLog = Packed Record
    Size: LongInt;         // sizeof(tFRLog)
    LogEventType: LongInt; // ���� �� ��������, ��������� ����
    TextData: PChar;       // ������ � ��������� UTF8, ��������������� 0
    BinData: Pointer;
    BinDataSize: LongInt;
  end;

  TUFRLogicError = packed record
    Size:                LongInt;     // SizeOf(TUFRLogicError)
    LogicError:          LongInt;     // ���������� ��� ���������� ������ ��
    LogicErrorText:      ShortString; // �������� ���������� ������ �� (���� ����)
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

//���������������, � �� ���� ��������, ���������� �������� ����� ��� �� � ������������� �����
//� ����� ���������� ��� ����
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
