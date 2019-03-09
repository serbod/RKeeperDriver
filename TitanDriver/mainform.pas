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
    actFeedPaper: TAction;
    actFiscalization: TAction;
    actCurDocState: TAction;
    actReadTableGrp: TAction;
    actReadTableDep: TAction;
    actReadAll: TAction;
    actReadTableOper: TAction;
    actReadTableTax: TAction;
    actReadTablePay: TAction;
    actReadTablesList: TAction;
    actRegisterIP: TAction;
    actSetClock: TAction;
    actSknoState: TAction;
    actPutTaxFM: TAction;
    actPutHdrFm: TAction;
    actOpenBox: TAction;
    actLastReceipt: TAction;
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
    cbDocTextAttrW: TCheckBox;
    cbDocType: TComboBox;
    cbDocDiscountAll: TCheckBox;
    cbDocTextAttrH: TCheckBox;
    edDocBarcodeWidth: TLabeledEdit;
    edDocBarcodeFeed: TLabeledEdit;
    edDocBarcodeHeight: TLabeledEdit;
    edDocCashIONo: TLabeledEdit;
    edDocDiscountPrc: TLabeledEdit;
    edDocDiscountDn: TLabeledEdit;
    edDocCashIOSum: TLabeledEdit;
    edDocBarcodeCode: TLabeledEdit;
    edDocBarcodeType: TLabeledEdit;
    edDocText: TLabeledEdit;
    edDocPaySum: TLabeledEdit;
    edDocPayNo: TLabeledEdit;
    edDocSaleGrp: TLabeledEdit;
    edDocSaleDep: TLabeledEdit;
    edDocSaleTax: TLabeledEdit;
    edDocSaleQty: TLabeledEdit;
    edDocSalePrice: TLabeledEdit;
    edDocSaleCType: TLabeledEdit;
    gbSknoState: TGroupBox;
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
    gbCurDocState: TGroupBox;
    edDocDiscountSum: TLabeledEdit;
    edDocFisText: TLabeledEdit;
    gbLastDocState: TGroupBox;
    Label1: TLabel;
    lboxAddrList: TListBox;
    lvDocs: TListView;
    memoLastDocState: TMemo;
    memoCurDocState: TMemo;
    memoSknoState: TMemo;
    memoSelDocInfo: TMemo;
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
    MenuItem20: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem6: TMenuItem;
    miRequests: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    miDocs: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem35: TMenuItem;
    N1: TMenuItem;
    miProcs: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    miReports: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pgcDocLine: TPageControl;
    pgcMain: TPageControl;
    pmMain: TPopupMenu;
    pmPay: TPopupMenu;
    pmOper: TPopupMenu;
    tsCurState: TTabSheet;
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
    procedure actCurDocStateExecute(Sender: TObject);
    procedure actDocAddTestNewOrderExecute(Sender: TObject);
    procedure actDiscoverExecute(Sender: TObject);
    procedure actDocAddTestIOExecute(Sender: TObject);
    procedure actDocAddTestNewBillExecute(Sender: TObject);
    procedure actDocAddTestSaleExecute(Sender: TObject);
    procedure actDocAddTestTextExecute(Sender: TObject);
    procedure actDocPrintExecute(Sender: TObject);
    procedure actFeedPaperExecute(Sender: TObject);
    procedure actFiscalizationExecute(Sender: TObject);
    procedure actLastReceiptExecute(Sender: TObject);
    procedure actOpenBoxExecute(Sender: TObject);
    procedure actPutHdrFmExecute(Sender: TObject);
    procedure actReadAllExecute(Sender: TObject);
    procedure actReadTableDepExecute(Sender: TObject);
    procedure actReadTableGrpExecute(Sender: TObject);
    procedure actReadTableOperExecute(Sender: TObject);
    procedure actReadTablePayExecute(Sender: TObject);
    procedure actReadTablesListExecute(Sender: TObject);
    procedure actReadTableTaxExecute(Sender: TObject);
    procedure actRegisterIPExecute(Sender: TObject);
    procedure actRepExecute(Sender: TObject);
    procedure actReqDevInfoExecute(Sender: TObject);
    procedure actReqDocsExecute(Sender: TObject);
    procedure actReqStatusExecute(Sender: TObject);
    procedure actSetClockExecute(Sender: TObject);
    procedure actSknoStateExecute(Sender: TObject);
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
    procedure lvDocsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure pgcDocLineChange(Sender: TObject);
    procedure pmOperPopup(Sender: TObject);
    procedure pmPayPopup(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
  private
    FTitanDriver: TTitanDriver;
    // Тестовый документ
    FTestDoc: TFrDoc;

    FPayEdit: TObject;
    procedure SetAccount();
    // обновить расшифровку тестового чека
    procedure UpdateTestDocInfo();

    procedure OnPayClick(Sender: TObject);
    procedure OnOperClick(Sender: TObject);
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

  pgcDocLine.ActivePageIndex := 0;

  lvDocs.DoubleBuffered := True;
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

procedure TFormMain.actCurDocStateExecute(Sender: TObject);
begin
  TitanDriver.GetDocState();
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

procedure TFormMain.actFeedPaperExecute(Sender: TObject);
begin
  TitanDriver.FeedPaper();
end;

procedure TFormMain.actFiscalizationExecute(Sender: TObject);
begin
  TitanDriver.Fiscalization();
end;

procedure TFormMain.actLastReceiptExecute(Sender: TObject);
begin
  TitanDriver.LastReceipt();
end;

procedure TFormMain.actOpenBoxExecute(Sender: TObject);
begin
  TitanDriver.OpenBox();
end;

procedure TFormMain.actPutHdrFmExecute(Sender: TObject);
begin
  TitanDriver.PutHdrFM();
end;

procedure TFormMain.actReadAllExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('Pay');
  TitanDriver.ReadTable('Dep');
  TitanDriver.ReadTable('Grp');
  TitanDriver.ReadTable('Tax');
  TitanDriver.ReadTable('Oper');
  TitanDriver.GetDevInfo();
  TitanDriver.GetDevState();
end;

procedure TFormMain.actReadTableDepExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('Dep');
end;

procedure TFormMain.actReadTableGrpExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('Grp');
end;

procedure TFormMain.actReadTableOperExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('Oper');
end;

procedure TFormMain.actReadTablePayExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('Pay');
end;

procedure TFormMain.actReadTablesListExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('');
end;

{procedure TFormMain.actReadTablesListExecute(Sender: TObject);
var
  fs1, fs2: TFileStream;
  z: TInflater;
begin
  fs1 := TFileStream.Create('http_result_body.z', fmOpenRead);
  fs2 := TFileStream.Create('http_result_body.out', fmCreate);
  z := TInflater.Create(fs1, fs2, fs1.Size);
  try
    z.DeCompress();
  finally
    z.Free();
    fs2.Free();
    fs1.Free();
  end;
end; }

procedure TFormMain.actReadTableTaxExecute(Sender: TObject);
begin
  TitanDriver.ReadTable('Tax');
end;

procedure TFormMain.actRegisterIPExecute(Sender: TObject);
begin
  TitanDriver.RegisterWhiteIP();
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

procedure TFormMain.actSetClockExecute(Sender: TObject);
begin
  TitanDriver.SetClock(Now());
end;

procedure TFormMain.actSknoStateExecute(Sender: TObject);
begin
  TitanDriver.SknoState();
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
var
  s: string;
begin
  if not Assigned(FTestDoc) then Exit;

  if pgcDocLine.ActivePage = tsDocSale then
  begin
    FTestDoc.AddSale(
      edDocSaleName.Text,
      edDocSaleCode.Text,
      StrToCurrDef(edDocSalePrice.Text, 0),
      StrToCurrDef(edDocSaleQty.Text, 1),
      StrToIntDef(edDocSaleTax.Text, -1),
      StrToIntDef(edDocSaleCType.Text, 1),
      StrToIntDef(edDocSaleDep.Text, 1),
      StrToIntDef(edDocSaleGrp.Text, 1)
    );
  end
  else if pgcDocLine.ActivePage = tsDocDiscount then
  begin
    FTestDoc.AddDiscount(
      StrToCurrDef(edDocDiscountSum.Text, 0),
      StrToCurrDef(edDocDiscountPrc.Text, 0),
      cbDocDiscountAll.Checked,
      StrToIntDef(edDocDiscountDn.Text, 0)
    );
  end
  else if pgcDocLine.ActivePage = tsDocPayment then
  begin
    FTestDoc.AddPayment(
      StrToCurrDef(edDocPaySum.Text, 0),
      StrToIntDef(edDocPayNo.Text, 0)
    );
  end
  else if pgcDocLine.ActivePage = tsDocCashIO then
  begin
    FTestDoc.AddCashIO(
      StrToCurrDef(edDocCashIOSum.Text, 0),
      StrToIntDef(edDocCashIONo.Text, 0)
    );
  end
  else if pgcDocLine.ActivePage = tsDocFisText then
  begin
    FTestDoc.AddFiscalComment(edDocFisText.Text);
  end
  else if pgcDocLine.ActivePage = tsDocText then
  begin
    s := '';
    if cbDocTextAttrH.Checked then
      s := s + TEXT_ATTR_DOUBLE_HEIGHT;
    if cbDocTextAttrW.Checked then
      s := s + TEXT_ATTR_WIDE;
    FTestDoc.AddText(edDocText.Text, s);
  end
  else if pgcDocLine.ActivePage = tsDocBarcode then
  begin
    FTestDoc.AddBarcode(edDocBarcodeCode.Text,
                        StrToIntDef(edDocBarcodeType.Text, 1),
                        StrToIntDef(edDocBarcodeWidth.Text, 2),
                        StrToIntDef(edDocBarcodeHeight.Text, 60),
                        StrToIntDef(edDocBarcodeHeight.Text, 20));
  end
  else if pgcDocLine.ActivePage = tsDocVoiding then
  begin
    FTestDoc.AddDocVoiding(StrToIntDef(edDocVoidingNo.Text, 0));
  end
  else if pgcDocLine.ActivePage = tsDocCorrection then
  begin
    FTestDoc.AddDocCorrection(edDocCorrectionCode.Text);
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
    actReadAllExecute(nil);
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

procedure TFormMain.lvDocsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  TmpDoc: TFrDoc;
begin
  if Selected and Assigned(Item) then
  begin
    TmpDoc := TitanDriver.DocList.GetItem(Item.Index);
    if Assigned(TmpDoc) then
    begin
      TmpDoc.FillDocInfoText(memoSelDocInfo.Lines);
    end;
  end
  else
  begin
    memoSelDocInfo.Clear();
  end;
end;

procedure TFormMain.pgcDocLineChange(Sender: TObject);
begin

end;

procedure TFormMain.pmOperPopup(Sender: TObject);
var
  i: Integer;
  mi: TMenuItem;
begin
  pmOper.Items.Clear();
  if Assigned(TitanDriver) then
  begin
    mi := TMenuItem.Create(pmOper);
    mi.Caption := 'service';
    mi.Tag := 99;
    mi.Name := 'miOper_99';
    mi.OnClick := @OnOperClick;
    pmOper.Items.Add(mi);

    for i := 0 to Length(TitanDriver.OperItemList)-1 do
    begin
      mi := TMenuItem.Create(pmOper);
      mi.Caption := IntToStr(TitanDriver.OperItemList[i].Id) + ' - '
                  + TitanDriver.OperItemList[i].Name;
      mi.Tag := i;
      mi.Name := 'miOper_'+IntToStr(i);
      mi.OnClick := @OnOperClick;
      pmOper.Items.Add(mi);
    end;
  end;
end;

procedure TFormMain.pmPayPopup(Sender: TObject);
var
  i: Integer;
  mi: TMenuItem;
begin
  pmPay.Items.Clear();
  if Assigned(TitanDriver) then
  begin
    for i := 0 to Length(TitanDriver.PayItemList)-1 do
    begin
      mi := TMenuItem.Create(pmPay);
      mi.Caption := IntToStr(TitanDriver.PayItemList[i].Id) + ' - '
                  + TitanDriver.PayItemList[i].Name;
      mi.Tag := i;
      mi.Name := 'miPay_'+IntToStr(i);
      mi.OnClick := @OnPayClick;
      pmPay.Items.Add(mi);
    end;

    //FPayEdit := FindControlAtPosition(Mouse.CursorPos, False);
    if pgcDocLine.ActivePage = tsDocCashIO then
      FPayEdit := edDocCashIONo
    else if pgcDocLine.ActivePage = tsDocPayment then
      FPayEdit := edDocPayNo;
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
    memoDevInfo.Lines.Add('СКНО: '+ TitanDriver.GetSknoStateStr());
    memoDevInfo.Lines.Add('Err: '+ TitanDriver.DevInfo.Err);
    memoDevInfo.Lines.EndUpdate();

    // расшифровка состояния СКНО
    memoSknoState.Lines.BeginUpdate();
    memoSknoState.Lines.Clear();
    TitanDriver.FillSknoStateText(memoSknoState.Lines);
    memoSknoState.Lines.EndUpdate();

    // расшифровка состояния текущего документа
    memoCurDocState.Lines.BeginUpdate();
    memoCurDocState.Lines.Clear();
    memoCurDocState.Lines.Add('Номер оператора: ' + IntToStr(TitanDriver.CurDocInfo.OpNum));
    memoCurDocState.Lines.Add('Начальная запись документа: ' + IntToStr(TitanDriver.CurDocInfo.DocNum));
    memoCurDocState.Lines.Add('Начальная запись отчета: ' + IntToStr(TitanDriver.CurDocInfo.RepNum));
    memoCurDocState.Lines.Add('--- состояние документа ---');
    FillDocStateText(TitanDriver.CurDocInfo.DocState, memoCurDocState.Lines);
    memoCurDocState.Lines.Add('===');
    memoCurDocState.Lines.EndUpdate();

    // расшифровка состояния последнего документа в ленте
    memoLastDocState.Lines.BeginUpdate();
    memoLastDocState.Lines.Clear();
    memoLastDocState.Lines.Add('Сумма итого: ' + CurrToStr(TitanDriver.LastDocInfo.TotalSum));
    memoLastDocState.Lines.Add('Номер документа: ' + IntToStr(TitanDriver.LastDocInfo.DocNum));
    memoLastDocState.Lines.Add('Дата документа: ' + FormatDateTime('YYYY-MM-DD HH:NN:SS', TitanDriver.LastDocInfo.DateTime));
    memoLastDocState.Lines.Add('Номер оператора: ' + IntToStr(TitanDriver.LastDocInfo.OpNum));
    memoLastDocState.Lines.Add('UID: '+ TitanDriver.LastDocInfo.UID);
    memoLastDocState.Lines.EndUpdate();
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

procedure TFormMain.OnPayClick(Sender: TObject);
var
  n, PayId: Integer;
  PayName: string;
begin
  if (Sender is TMenuItem) then
  begin
    n := (Sender as TMenuItem).Tag;
    PayId := TitanDriver.PayItemList[n].Id;
    PayName := TitanDriver.PayItemList[n].Name;

    if Assigned(FPayEdit) then
    begin
      (FPayEdit as TCustomEdit).Text := IntToStr(PayId);
    end;
    FPayEdit := nil;
  end;
end;

procedure TFormMain.OnOperClick(Sender: TObject);
var
  n: Integer;
begin
  if (Sender is TMenuItem) then
  begin
    n := (Sender as TMenuItem).Tag;
    if n = 99 then
    begin
      edLogin.Text := 'service';
      edPassw.Text := '751426';
    end
    else
    begin
      edLogin.Text := IntToStr(TitanDriver.OperItemList[n].Id);
      edPassw.Text := TitanDriver.OperItemList[n].Pswd;
    end;
  end;
end;

end.

