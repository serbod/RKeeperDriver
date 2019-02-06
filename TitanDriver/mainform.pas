unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  ExtCtrls, Menus, ComCtrls, TitanDrv;

type

  { TFormMain }

  TFormMain = class(TForm)
    actDiscover: TAction;
    actDocAddTestSale: TAction;
    actDocAddTestIO: TAction;
    actDocAddTestText: TAction;
    actDocPrint: TAction;
    actDocAddTestNewBill: TAction;
    actDocAddTestNewOrder: TAction;
    actReqDocs: TAction;
    actRep107: TAction;
    actRep102: TAction;
    actRep21: TAction;
    actRep20: TAction;
    actRep10: TAction;
    actRep5: TAction;
    actRep4: TAction;
    actRep3: TAction;
    actRep2: TAction;
    actRep1: TAction;
    actRep0: TAction;
    actSound2: TAction;
    actSound: TAction;
    actReqDevInfo: TAction;
    actReqStatus: TAction;
    alMain: TActionList;
    btnDiscover: TButton;
    btnCreateDoc: TButton;
    btnAddLine: TButton;
    btnDocPrint: TButton;
    cbDocType: TComboBox;
    edDocSaleGrp: TLabeledEdit;
    edDocSaleDep: TLabeledEdit;
    edDocSaleTax: TLabeledEdit;
    edDocSaleQty: TLabeledEdit;
    edDocSalePrice: TLabeledEdit;
    edDocSaleCType: TLabeledEdit;
    gbLogin: TGroupBox;
    edAddr: TLabeledEdit;
    edLogin: TLabeledEdit;
    edPassw: TLabeledEdit;
    gbDevInfo: TGroupBox;
    gbDocs: TGroupBox;
    edDocVoidingNo: TLabeledEdit;
    edDocCorrectionCode: TLabeledEdit;
    edDocSaleCode: TLabeledEdit;
    edDocSaleName: TLabeledEdit;
    lboxAddrList: TListBox;
    lvDocs: TListView;
    MemoHttpHeaders: TMemo;
    memoTestDocInfo: TMemo;
    memoDevInfo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pgcDocLine: TPageControl;
    pgcMain: TPageControl;
    pmMain: TPopupMenu;
    tsDocCorrection: TTabSheet;
    tsDocVoiding: TTabSheet;
    tsDocBarcode: TTabSheet;
    tsDocText: TTabSheet;
    tsDocFisText: TTabSheet;
    tsDocCashIO: TTabSheet;
    tsDocPayment: TTabSheet;
    tsDocDiscount: TTabSheet;
    tsDocSale: TTabSheet;
    tsPrintDocs: TTabSheet;
    tsDocs: TTabSheet;
    tsMain: TTabSheet;
    Timer100ms: TTimer;
    procedure actDocAddTestNewOrderExecute(Sender: TObject);
    procedure actDiscoverExecute(Sender: TObject);
    procedure actDocAddTestIOExecute(Sender: TObject);
    procedure actDocAddTestNewBillExecute(Sender: TObject);
    procedure actDocAddTestSaleExecute(Sender: TObject);
    procedure actDocAddTestTextExecute(Sender: TObject);
    procedure actDocPrintExecute(Sender: TObject);
    procedure actRepExecute(Sender: TObject);
    procedure actReqDevInfoExecute(Sender: TObject);
    procedure actReqDocsExecute(Sender: TObject);
    procedure actReqStatusExecute(Sender: TObject);
    procedure actSound2Execute(Sender: TObject);
    procedure actSoundExecute(Sender: TObject);
    procedure btnAddLineClick(Sender: TObject);
    procedure btnCreateDocClick(Sender: TObject);
    procedure edAddrEditingDone(Sender: TObject);
    procedure edLoginEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gbDevInfoClick(Sender: TObject);
    procedure lboxAddrListClick(Sender: TObject);
    procedure lvDocsData(Sender: TObject; Item: TListItem);
    procedure Timer100msTimer(Sender: TObject);
  private
    FTitanDriver: TTitanDriver;
    // Тестовый документ
    FTestDoc: TFrDoc;
    procedure SetAccount();
    // обновить расшифровку тестового чека
    procedure UpdateTestDocInfo();
  public
    property TitanDriver: TTitanDriver read FTitanDriver;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses
  DateUtils;

procedure NumToDTMF(ANum: Byte; var AToneH, AToneV: Integer);
const
  TonesV: array[0..3] of Integer = (697, 770, 852, 941);
  TonesH: array[0..3] of Integer = (1209, 1336, 1477, 1633);
var
  n: Byte;
begin
  if ANum > 15 then
    ANum := 15;

  case ANum of
    $1: n := 0;
    $2: n := 1;
    $3: n := 2;
    $A: n := 3;
    $4: n := 4;
    $5: n := 5;
    $6: n := 6;
    $B: n := 7;
    $7: n := 8;
    $8: n := 9;
    $9: n := 10;
    $C: n := 11;
    $E: n := 12;
    $0: n := 13;
    $F: n := 14;
    $D: n := 15;
  end;
  AToneV := TonesV[n div 4];
  AToneH := TonesH[n mod 4];
end;

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FTitanDriver := TTitanDriver.Create(Self);
  pgcMain.ActivePageIndex := 0;

  cbDocType.Clear();
  cbDocType.AddItem('(F) фискальный чек', nil);
  cbDocType.AddItem('(R) чек возврата', nil);
  cbDocType.AddItem('(IO) чек внесения-изъятия денег', nil);
  cbDocType.AddItem('(VD) чек аннулирования', nil);
  cbDocType.AddItem('(P) нефискальный чек', nil);
  cbDocType.AddItem('(L) копия чека или ресторанного счета', nil);
  cbDocType.AddItem('(RO) ресторанный заказ', nil);
  cbDocType.AddItem('(VB) отмена ресторанного заказа', nil);
end;

procedure TFormMain.actDiscoverExecute(Sender: TObject);
begin
  lboxAddrList.Items.Clear();
  TitanDriver.Discover();
end;

procedure TFormMain.actDocAddTestNewOrderExecute(Sender: TObject);
begin
  if Assigned(FTestDoc) then
    FreeAndNil(FTestDoc);
  FTestDoc := TFrDoc.Create(frdOrder);
  FTestDoc.AddNewOrder(1);
  FTestDoc.AddSale('Кофе', '3', 12);
  UpdateTestDocInfo();
end;

procedure TFormMain.actDocAddTestIOExecute(Sender: TObject);
begin
  if Assigned(FTestDoc) then
    FreeAndNil(FTestDoc);
  FTestDoc := TFrDoc.Create(frdCashIO);
  FTestDoc.AddFiscalComment('Кассир: Светлана');
  FTestDoc.AddCashIO(120);
  FTestDoc.AddCashIO(-140, 2);
  UpdateTestDocInfo();
end;

procedure TFormMain.actDocAddTestNewBillExecute(Sender: TObject);
begin
  if Assigned(FTestDoc) then
    FreeAndNil(FTestDoc);
  FTestDoc := TFrDoc.Create(frdOrder);
  FTestDoc.AddNewBill(1, 1);
  FTestDoc.AddSale('Конфета', '1', 5);
  FTestDoc.AddSale('Печенье', '2', 15, 0.5);
  UpdateTestDocInfo();
end;

procedure TFormMain.actDocAddTestSaleExecute(Sender: TObject);
begin
  if Assigned(FTestDoc) then
    FreeAndNil(FTestDoc);
  FTestDoc := TFrDoc.Create(frdFiscal);
  FTestDoc.AddFiscalComment('Кассир: Светлана');
  FTestDoc.AddSale('Конфета', '1', 5);
  FTestDoc.AddSale('Печенье', '2', 15, 0.5);
  FTestDoc.AddDiscount(0, 5, True);
  FTestDoc.AddPayment();
  UpdateTestDocInfo();
end;

procedure TFormMain.actDocAddTestTextExecute(Sender: TObject);
begin
  if Assigned(FTestDoc) then
    FreeAndNil(FTestDoc);
  FTestDoc := TFrDoc.Create(frdNonFiscal);
  //FTestDoc.AddFiscalComment('Line 1.1');
  //FTestDoc.AddFiscalComment('Line 1.2', TEXT_ATTR_WIDE);
  //FTestDoc.AddFiscalComment('Line 1.33');
  FTestDoc.AddText('Line 2.1');
  FTestDoc.AddText('Line 2.2', TEXT_ATTR_WIDE);
  FTestDoc.AddText('Line 2.3', TEXT_ATTR_DOUBLE_HEIGHT);
  UpdateTestDocInfo();
end;

procedure TFormMain.actDocPrintExecute(Sender: TObject);
begin
  if Assigned(FTestDoc) then
  begin
    TitanDriver.SendFrDoc(FTestDoc);
  end;
end;

procedure TFormMain.actRepExecute(Sender: TObject);
begin
  if Sender = actRep0 then
    TitanDriver.PrintReport(REPORT_Z1)
  else if Sender = actRep1 then
    TitanDriver.PrintReport(REPORT_CLEAN_LOG)
  else if Sender = actRep2 then
    TitanDriver.PrintReport(REPORT_PRINT_CLEAN_LOG)
  else if Sender = actRep3 then
    TitanDriver.PrintReport(REPORT_SKNO_REPORT)
  else if Sender = actRep4 then
    TitanDriver.PrintReport(REPORT_PRINT_CLOSE_ORDERS)
  else if Sender = actRep5 then
    TitanDriver.PrintReport(REPORT_PRINT_ORDERS)
  else if Sender = actRep10 then
    TitanDriver.PrintReport(REPORT_X1)
  else if Sender = actRep20 then
    TitanDriver.PrintReport(REPORT_SALES)
  else if Sender = actRep21 then
    TitanDriver.PrintReport(REPORT_SALES_CLEAR)
  else if Sender = actRep102 then
    TitanDriver.PrintReport(REPORT_BOX)
  else if Sender = actRep107 then
    TitanDriver.PrintReport(REPORT_SALES_BY_TIME);
end;

procedure TFormMain.actReqDevInfoExecute(Sender: TObject);
begin
  TitanDriver.GetDevInfo();
end;

procedure TFormMain.actReqDocsExecute(Sender: TObject);
begin
  TitanDriver.GetDocs();
end;

procedure TFormMain.actReqStatusExecute(Sender: TObject);
begin
  TitanDriver.GetDevState();
end;

procedure TFormMain.actSound2Execute(Sender: TObject);
var
  i: Integer;
begin
  for i := 1 to 30 do
    TitanDriver.Sound(100, 96 + Random(4000));

end;

procedure TFormMain.actSoundExecute(Sender: TObject);
begin
  TitanDriver.Sound(1000, 1000);
end;

procedure TFormMain.btnAddLineClick(Sender: TObject);
begin
  if not Assigned(FTestDoc) then Exit;

  if pgcDocLine.ActivePage = tsDocSale then
  begin
    FTestDoc.AddSale(edDocSaleName.Text,
                   edDocSaleCode.Text,
                   StrToCurrDef(edDocSalePrice.Text, 0),
                   StrToCurrDef(edDocSaleQty.Text, 1),
                   StrToIntDef(edDocSaleTax.Text, -1),
                   StrToIntDef(edDocSaleCType.Text, 1),
                   StrToIntDef(edDocSaleDep.Text, 1),
                   StrToIntDef(edDocSaleGrp.Text, 1));
  end;
  UpdateTestDocInfo();
end;

procedure TFormMain.btnCreateDocClick(Sender: TObject);
begin
  if Assigned(FTestDoc) then
    FreeAndNil(FTestDoc);
  case cbDocType.ItemIndex of
    0: FTestDoc := TFrDoc.Create(frdFiscal);
    1: FTestDoc := TFrDoc.Create(frdRefund);
    2: FTestDoc := TFrDoc.Create(frdCashIO);
    3: FTestDoc := TFrDoc.Create(frdVoiding);
    4: FTestDoc := TFrDoc.Create(frdNonFiscal);
    5: FTestDoc := TFrDoc.Create(frdCopy);
    6: FTestDoc := TFrDoc.Create(frdOrder);
    7: FTestDoc := TFrDoc.Create(frdOrderCancel);
  end;
  UpdateTestDocInfo();
end;

procedure TFormMain.edAddrEditingDone(Sender: TObject);
begin
  TitanDriver.DevAddr := edAddr.Text;
end;

procedure TFormMain.edLoginEditingDone(Sender: TObject);
begin
  SetAccount();
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTitanDriver);
end;

procedure TFormMain.gbDevInfoClick(Sender: TObject);
begin

end;

procedure TFormMain.lboxAddrListClick(Sender: TObject);
begin
  if lboxAddrList.GetSelectedText <> '' then
  begin
    edAddr.Text := lboxAddrList.GetSelectedText;
    TitanDriver.DevAddr := edAddr.Text;
  end;
end;

procedure TFormMain.lvDocsData(Sender: TObject; Item: TListItem);
var
  TmpItem: TFrDoc;
  n: Integer;
begin
  if Assigned(Item) then
  begin
    n := Item.Index;
    if (n >= 0) and (n < TitanDriver.DocList.Count) then
    begin
      TmpItem := TitanDriver.DocList.GetItem(Item.Index);
      Item.Caption := FormatDateTime('MM-DD HH:NN:SS', TmpItem.DateTime);
      Item.SubItems.Add(IntToStr(TmpItem.ID));
      Item.SubItems.Add(IntToStr(TmpItem.OperID));
      Item.SubItems.Add(TmpItem.GetDocTypeStr());
    end;
  end;
end;

procedure TFormMain.Timer100msTimer(Sender: TObject);
begin
  FTitanDriver.Tick();
  if TitanDriver.AddrList.Count <> lboxAddrList.Items.Count then
    lboxAddrList.Items.Assign(TitanDriver.AddrList);

  if lvDocs.Items.Count <> TitanDriver.DocList.Count then
    lvDocs.Items.Count := TitanDriver.DocList.Count;
  lvDocs.Invalidate();

  // update dev info
  if TitanDriver.IsStateUpdated then
  begin
    TitanDriver.IsStateUpdated := False;
    memoDevInfo.Lines.BeginUpdate();
    memoDevInfo.Lines.Clear();
    memoDevInfo.Lines.Add('Модель: '+ TitanDriver.DevInfo.Model);
    memoDevInfo.Lines.Add('Название: '+ TitanDriver.DevInfo.Name);
    memoDevInfo.Lines.Add('Серийник: '+ TitanDriver.DevInfo.SerialNum);
    memoDevInfo.Lines.Add('Версия ПО: '+ TitanDriver.DevInfo.Version);
    memoDevInfo.Lines.Add('Дата прошивки: '+ FormatDateTime('YYYY-MM-DD', TitanDriver.DevInfo.SwDate));
    memoDevInfo.Lines.Add('Номер СКНО: '+ TitanDriver.DevInfo.SknoNum);
    memoDevInfo.Lines.Add('Рег. номер: '+ TitanDriver.DevInfo.RegNum);
    memoDevInfo.Lines.Add('УНП: '+ TitanDriver.DevInfo.UnpNum);
    memoDevInfo.Lines.Add('Версия протокола: '+ TitanDriver.DevInfo.ProtocolVersion);
    memoDevInfo.Lines.Add('=========');
    memoDevInfo.Lines.Add('ID последнего чека: '+ IntToStr(TitanDriver.DevInfo.ChkId));
    memoDevInfo.Lines.Add('Время начала ленты: '+ FormatDateTime('YYYY-MM-DD HH:NN:SS', TitanDriver.DevInfo.JrnTime));
    memoDevInfo.Lines.Add('Номер посл. закрытой смены: '+ IntToStr(TitanDriver.DevInfo.CurrZ));
    memoDevInfo.Lines.Add('Есть записи в журнале: '+ BoolToStr(TitanDriver.DevInfo.IsWrk, 'Да', 'Нет'));
    memoDevInfo.Lines.Add('Фискализация режим: '+ BoolToStr(TitanDriver.DevInfo.IsFiscalization, 'Да', 'Нет'));
    memoDevInfo.Lines.Add('Фискальный режим: '+ BoolToStr(TitanDriver.DevInfo.IsFskMode, 'Да', 'Нет'));
    memoDevInfo.Lines.Add('СКНО: '+ IntToStr(TitanDriver.DevInfo.SknoState));
    memoDevInfo.Lines.Add('Err: '+ TitanDriver.DevInfo.Err);
    memoDevInfo.Lines.EndUpdate();

  end;

  if TitanDriver.IsResponseUpdated then
  begin
    TitanDriver.IsResponseUpdated := False;
    MemoHttpHeaders.Text := TitanDriver.LastHttpResponse;
  end;
end;

procedure TFormMain.SetAccount();
begin
  TitanDriver.DevAddr := edAddr.Text;
  TitanDriver.DevLogin := edLogin.Text;
  TitanDriver.DevPassw := edPassw.Text;
end;

procedure TFormMain.UpdateTestDocInfo();
begin
  if Assigned(FTestDoc) then
  begin
    FTestDoc.FillDocInfoText(memoTestDocInfo.Lines);

    memoTestDocInfo.Lines.Add('========');
    memoTestDocInfo.Lines.Add(DataToJson(FTestDoc.Lines));
  end;
end;

end.

