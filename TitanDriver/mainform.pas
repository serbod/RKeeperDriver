unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdCtrls,
  ExtCtrls, Menus, TitanDrv;

type

  { TFormMain }

  TFormMain = class(TForm)
    actDiscover: TAction;
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
    Button1: TButton;
    gbLogin: TGroupBox;
    edAddr: TLabeledEdit;
    edLogin: TLabeledEdit;
    edPassw: TLabeledEdit;
    gbDevInfo: TGroupBox;
    lboxAddrList: TListBox;
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
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    pmMain: TPopupMenu;
    Timer100ms: TTimer;
    procedure actDiscoverExecute(Sender: TObject);
    procedure actRepExecute(Sender: TObject);
    procedure actReqDevInfoExecute(Sender: TObject);
    procedure actReqStatusExecute(Sender: TObject);
    procedure actSound2Execute(Sender: TObject);
    procedure actSoundExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lboxAddrListClick(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
  private
    FTitanDriver: TTitanDriver;
    procedure SetAccount();
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
end;

procedure TFormMain.actDiscoverExecute(Sender: TObject);
begin
  lboxAddrList.Items.Clear();
  TitanDriver.Discover();
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

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTitanDriver);
end;

procedure TFormMain.lboxAddrListClick(Sender: TObject);
begin
  if lboxAddrList.GetSelectedText <> '' then
    edAddr.Text := lboxAddrList.GetSelectedText;
end;

procedure TFormMain.Timer100msTimer(Sender: TObject);
begin
  FTitanDriver.Tick();
  if TitanDriver.AddrList.Count <> lboxAddrList.Items.Count then
    lboxAddrList.Items.Assign(TitanDriver.AddrList);

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
end;

procedure TFormMain.SetAccount();
begin
  TitanDriver.DevAddr := edAddr.Text;
  TitanDriver.DevLogin := edLogin.Text;
  TitanDriver.DevPassw := edPassw.Text;
end;

end.

