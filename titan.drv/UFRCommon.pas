unit UFRCommon;

interface

uses
  {$IFnDEF FPC}
  Windows,
  {$ELSE}
  LCLIntf, LCLType, LMessages, DOM, XmlRead,
  {$ENDIF}
  SysUtils;

procedure SaveXMLInit(var LogPath: string);
procedure SaveXML(const LogPath, stLog: string; var LogNum: Integer);

{$ifndef FPC}
procedure AppProcessMessages(); deprecated;
function IsThisWine: Boolean;
procedure Sleep2(const msec: DWord);
{$endif}

function ReadFileToString(fn: string): string;
procedure DeleteSpaces(var st: string);
function GetDigit(const DigNum: Integer; const stDig: string;
  const DecYear: Boolean = True): Integer;
function FlagPresented(const Mask: int64; const Flags: array of int64): Boolean;
function AnsiTo866(const stAnsi: string): string;
function CP866ToAnsi(const stAnsi: string): string;
function BCDToDWord(const BCD: DWord): DWord; stdcall; assembler;
function BCDToByte(const BCD: Byte): Byte; stdcall; assembler;
function BCDToInt64(const BCD: int64): int64; stdcall; assembler;
function GetBit(const bt, nBit: Byte): Boolean; stdcall; assembler;
function SetBit(const bt, nBit: Byte): Byte; stdcall; assembler;
procedure DWordToBcd(dw: DWord; BCDRes: Pointer; BCDResLen: DWord);
procedure SwapBytes(const pBt: Pointer; Count: Integer); stdcall; assembler;


implementation

function BCDToDWord(const BCD: DWord): DWord; stdcall; assembler;
asm
         PUSH    EBX
         PUSH    ECX
         PUSH    EDX
         PUSH    EDI
         PUSH    ESI

         MOV     ESI,BCD
         MOV     EDI,ESI
         AND     EDI,$0000000F //число единиц
         MOV     ECX,8-1
         //осталось еще семь шестнадцатеричных цифр (в DWORD их 8)
         MOV     EBX,10
         @@1:
         SHR     ESI,4
         MOV     EAX,ESI
         AND     EAX,$0000000F
         MUL     EBX
         //здесь последовательно умножаем на 10, на 100, на 1000 и т.д.
         ADD     EDI,EAX
         MOV     EAX,EBX
         SHL     EBX,3    //*8
         SHL     EAX,1    //*2
         ADD     EBX,EAX  //ebx = ebx * 10
         LOOP    @@1
         MOV     EAX,EDI //результат в EAX
         POP     ESI
         POP     EDI
         POP     EDX
         POP     ECX
         POP     EBX
end;

function BCDToByte(const BCD: Byte): Byte; stdcall; assembler;
asm
         PUSH    EBX
         XOR     EAX,EAX
         MOV     AL,Byte Ptr BCD
         MOV     BL,AL
         AND     BL,$0F
         AND     AL,$F0
         SHR     EAX,3
         LEA     EAX,[EAX+4*EAX]
         ADD     AL,BL   //в al результат
         POP     EBX
end;

function BCDToInt64(const BCD: int64): int64; stdcall; assembler;
asm
         PUSH    EBX
         PUSH    ECX
         PUSH    EDI
         PUSH    ESI

         //Сначала  обработаем старшее двойное слово
         MOV     ESI,DWord Ptr BCD[4]
         MOV     EDI,ESI
         AND     EDI,$0000000F
         MOV     ECX,8-1
         MOV     EBX,10
         @@1:
         SHR     ESI,4
         MOV     EAX,ESI
         AND     EAX,$0000000F
         MUL     EBX
         ADD     EDI,EAX
         MOV     EAX,EBX
         SHL     EBX,3    //*8
         SHL     EAX,1    //*2
         ADD     EBX,EAX  //ebx = ebx * 10
         LOOP    @@1
         PUSH    EDI    //Сохраним результат в стеке

         //Теперь  обработаем младшее двойное слово
         MOV     ESI,DWord Ptr BCD
         MOV     EDI,ESI
         AND     EDI,$0000000F
         MOV     ECX,8-1
         MOV     EBX,10
         @@2:
         SHR     ESI,4
         MOV     EAX,ESI
         AND     EAX,$0000000F
         MUL     EBX
         ADD     EDI,EAX
         MOV     EAX,EBX
         SHL     EBX,3    //*8
         SHL     EAX,1    //*2
         ADD     EBX,EAX  //ebx = ebx * 10
         LOOP    @@2

         //Извлечем из стека
         //результат обработки старшего двойного слова,
         POP     EAX
         MOV     EBX,100000000
         MUL     EBX      //умножим его на 100000000
         //и сложим с результатом обработки младшего двойного слова
         ADD     EAX,EDI
         ADC     EDX,0
         //Результат получается в регистрах EDX:EAX
         POP     ESI
         POP     EDI
         POP     ECX
         POP     EBX
end;

function GetBit(const bt, nBit: Byte): Boolean; stdcall; assembler;
asm
         PUSH    EBX
         //делаем в eax маску
         XOR     EAX,EAX
         MOVZX   EBX,nBit
         BTS     EAX,EBX //взводим в al бит nBit
         POP     EBX
         AND     AL,bt
end;

function SetBit(const bt, nBit: Byte): Byte; stdcall; assembler;
asm
         PUSH    EBX
         //делаем в eax маску
         XOR     EAX,EAX
         MOVZX   EBX,nBit
         BTS     EAX,EBX //взводим в al бит nBit
         MOV     BL,bt
         OR      BL,AL
         MOV     AL,BL
         POP     EBX
end;

//меняет местами заданное количество байтов
//если, напр., на входе массив (1,2,3,4,5), то на выходе будет (5,4,3,2,1)
procedure SwapBytes(const pBt: Pointer; Count: Integer); stdcall; assembler;
asm
         PUSH    EDI
         PUSH    ESI
         PUSHFD

         MOV     ESI,pBt
         MOV     EDI,ESI
         ADD     EDI,Count
         DEC     EDI
         CLD
         @Swap:
         LODSB
         XCHG    AL,[EDI]
         MOV     [ESI-1],AL
         DEC     EDI
         CMP     EDI,ESI
         JA      @Swap

         POPFD
         POP     ESI
         POP     EDI
end;

procedure DWordToBcd(dw: DWord; BCDRes: Pointer; BCDResLen: DWord);
var
  i: DWord;
  //i :Integer;
  rst: Byte;
  a: array[0..9] of Byte;
begin
  if BCDResLen > 10 then
    BCDResLen := 10;
  FillChar(a, 10, 0);
  i := 0;
  repeat
    rst := dw mod 10;
    dw := dw div 10;
    a[i] := rst;
    rst := dw mod 10;
    dw := dw div 10;
    a[i] := a[i] or (rst shl 4);
    Inc(i);
  until (dw = 0) or (i = BCDResLen);
  Dec(BCDResLen);
  for i := 0 to BCDResLen do
    pByte(DWord(BCDRes) + BCDResLen - i)^ := a[i];
end;

{$ifndef FPC}
procedure AppProcessMessages;
var
  Msg: TagMsg;
begin
  if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
end;

function IsThisWine: Boolean;
var
  H: DWord;
begin
  H := GetModuleHandle('ntdll.dll');
  Result := Assigned(GetProcAddress(H, 'wine_get_version'));
end;

procedure Sleep2(const msec: DWord);
var
  freq, Count, count2, delta: int64;
begin
  if (not QueryPerformanceFrequency(freq)) or (not QueryPerformanceCounter(Count)) then
  begin
    Sleep(msec);
    Exit;
  end;
  delta := msec * freq div 1000;
  count2 := Count + delta;
  while count2 > Count do
    QueryPerformanceCounter(Count);
end;
{$endif FPC}

{Function Utf8ToWin(s :String) :String;
Const  Utf2WinTable :Array [0..65, 0..1] of String = ((#208#144,#192), (#208#145,#193), (#208#146,#194),
                                                      (#208#147,#195), (#208#148,#196), (#208#149,#197),
                                                      (#208#129,#168), (#208#150,#198), (#208#151,#199),
                                                      (#208#152,#200), (#208#153,#201), (#208#154,#202),
                                                      (#208#155,#203), (#208#156,#204), (#208#157,#205),
                                                      (#208#158,#206), (#208#159,#207), (#208#160,#208),
                                                      (#208#161,#209), (#208#162,#210), (#208#163,#211),
                                                      (#208#164,#212), (#208#165,#213), (#208#166,#214),
                                                      (#208#167,#215), (#208#168,#216), (#208#169,#217),
                                                      (#208#170,#218), (#208#171,#219), (#208#172,#220),
                                                      (#208#173,#221), (#208#174,#222), (#208#175,#223),
                                                      (#208#176,#224), (#208#177,#225), (#208#178,#226),
                                                      (#208#179,#227), (#208#180,#228), (#208#181,#229),
                                                      (#209#145,#184), (#208#182,#230), (#208#183,#231),
                                                      (#208#184,#232), (#208#185,#233), (#208#186,#234),
                                                      (#208#187,#235), (#208#188,#236), (#208#189,#237),
                                                      (#208#190,#238), (#208#191,#239), (#209#128,#240),
                                                      (#209#129,#241), (#209#130,#242), (#209#131,#243),
                                                      (#209#132,#244), (#209#133,#245), (#209#134,#246),
                                                      (#209#135,#247), (#209#136,#248), (#209#137,#249),
                                                      (#209#138,#250), (#209#139,#251), (#209#140,#252),
                                                      (#209#141,#253), (#209#142,#254), (#209#143,#255) );
Var i :Integer;
Begin
  Result:=s;
  For i:=0 to 65 do
    If Pos(Utf2WinTable[i,0],Result)<>0 then
      Result:=StringReplace(Result,Utf2WinTable[i,0],Utf2WinTable[i,1],[rfReplaceAll]);
end;}

function Utf8ToWin(s: string): string;
var
  stU: WideString;
  l: Integer;
begin
  SetLength(stU, Length(s));
  Utf8ToUnicode(pWideChar(stU), Length(s), PChar(s), Length(s));

  l := WideCharToMultiByte(CP_ACP, WC_COMPOSITECHECK or
    WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,
    @stU[1], -1, nil, 0, nil, nil);
  SetLength(Result, l - 1);
  if l <> 0 then
    WideCharToMultiByte(CP_ACP,
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or
      WC_DEFAULTCHAR, @stU[1], -1, @Result[1], l - 1, nil, nil);
end;

procedure SaveXMLInit(var LogPath: string);
var
  FI: TSearchRec;
begin
  if not DirectoryExists(LogPath) then
    try
      if not ForceDirectories(LogPath) then
      begin
        LogPath := '';
        Exit;
      end;
    except
      LogPath := '';
      Exit;
    end;
  if LogPath[Length(LogPath)] <> '\' then
    LogPath := LogPath + '\';
  if FindFirst(LogPath + '*.log', faAnyFile, FI) = 0 then
  begin
    repeat
      DeleteFile(LogPath + FI.Name);
    until FindNext(FI) <> 0;
    FindClose(FI);
  end;
end;

procedure SaveXML(const LogPath, stLog: string; var LogNum: Integer);
var
  fLog: TextFile;
begin
  if LogPath = '' then
    Exit;
  Inc(LogNum);
  AssignFile(fLog, LogPath + IntToHex(LogNum, 5) + '.log');
  Rewrite(fLog);
  if IOResult <> 0 then
    Exit;
  Write(fLog, stLog);
  CloseFile(fLog);
end;

function ReadFileToString(fn: string): string;
var
  fXML: file;
begin
  AssignFile(fXML, fn);
  Reset(fXML, 1);
  SetLength(Result, FileSize(fXML));
  BlockRead(fXML, Result[1], Length(Result));
  CloseFile(fXML);
end;


procedure DeleteSpaces(var st: string);
begin
  while Pos(' ', st) <> 0 do
    Delete(st, Pos(' ', st), 1);
end;

//возвращает число из строки, содержащей несколько чисел, разделенных между собой какими-либо символами
//числа будут только положительные, т.е. знак '-' принимается за разделитель
function GetDigit(const DigNum: Integer; const stDig: string;
  const DecYear: Boolean = True): Integer;
var
  i, Pos1, Pos2: Integer;
begin
  i := 0;
  Result := -1;
  Pos1 := 1;
  for Pos2 := 1 to Length(stDig) do
    if not (stDig[Pos2] in ['0'..'9']) then
    begin
      Inc(i);
      if i = DigNum then
        Break;
      Pos1 := Pos2 + 1;
    end;
  if Pos1 = Pos2 then
    Exit;
  if Pos2 = Length(stDig) then
    Inc(Pos2);
  i := StrToInt(Copy(stDig, Pos1, Pos2 - Pos1));
  if DecYear and (i > 2000) then
    Dec(i, 2000);
  Result := i;
end;

function FlagPresented(const Mask: int64; const Flags: array of int64): Boolean;
var
  j: Integer;
begin
  Result := False;
  for j := 0 to High(Flags) do
    if Mask and Flags[j] <> 0 then
    begin
      Result := True;
      Exit;
    end;
end;

function AnsiTo866(const stAnsi: string): string;
var
  j: Integer;
begin
  SetLength(Result, Length(stAnsi));
  for j := 1 to Length(stAnsi) do
    case byte(stAnsi[j]) of
      $A8: {Ё} Result[j] := char($F0);
      $B8: {ё} Result[j] := char($F1);
      $AA: {Є} Result[j] := char($F2);
      $BA: {є} Result[j] := char($F3);
      $AF: {Ї} Result[j] := char($F4);
      $BF: {ї} Result[j] := char($F5);
      $A1: {Ў} Result[j] := char($F6);
      $A2: {ў} Result[j] := char($F7);
      $B9: {№} Result[j] := char($FC);
      $C0..$EF: {А-п} Result[j] := char(byte(stAnsi[j]) - $40);
      $F0..$FF: {р-я} Result[j] := char(byte(stAnsi[j]) - $10);
      else
        Result[j] := stAnsi[j];
    end;
end;

function CP866ToAnsi(const stAnsi: string): string;
var
  j: Integer;
begin
  SetLength(Result, Length(stAnsi));
  for j := 1 to Length(stAnsi) do
    case byte(stAnsi[j]) of
      $F0: {Ё} Result[j] := char($A8);
      $F1: {ё} Result[j] := char($B8);
      $F2: {Є} Result[j] := char($AA);
      $F3: {є} Result[j] := char($BA);
      $F4: {Ї} Result[j] := char($AF);
      $F5: {ї} Result[j] := char($BF);
      $F6: {Ў} Result[j] := char($A1);
      $F7: {ў} Result[j] := char($A2);
      $FC: {№} Result[j] := char($B9);
      $80..$AF: {А-п} Result[j] := char(byte(stAnsi[j]) + $40);
      $E0..$EF: {р-я} Result[j] := char(byte(stAnsi[j]) + $10);
      else
        Result[j] := stAnsi[j];
    end;
end;

end.
