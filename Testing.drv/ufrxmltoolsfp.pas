unit UfrXmlToolsFp;

{$mode delphi}

interface

uses
  Classes, SysUtils, DOM, XMLRead;

function CreateXMLDoc(const XMLBuf: PChar): TXMLDocument;
function GetFirstNode(const XMLDoc: TDOMDocument): TDOMNode;
function FindAttrByName(const XMLNode: TDOMNode; AttrName: string): string;
function FindChildNodeByName(const XMLNode: TDOMNode; TagName: string): TDOMNode;
function GetIntValByName(const XMLNode: TDOMNode; const AttrName: string;
  const DefaultValue: Integer): int64;
function NodeNameIs(const XMLNode: TDOMNode; const NodeName: string): Boolean;

function FlagPresented(const Mask: int64; const Flags: array of int64): Boolean;


implementation

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

function CreateXMLDoc(const XMLBuf: PChar): TXMLDocument;

procedure _SetXmlText(s: string);
var
  ss: TStringStream;
begin
  ss := TStringStream.Create(s);
  try
    ss.Position := 0;
    ReadXMLFile(Result, ss);
  finally
    ss.Free();
  end;
end;

var
  st: string;
begin
  Result := TXMLDocument.Create();
  st := XMLBuf;
  if FileExists(st) then
    st := ReadFileToString(st);

  if Pos(#$EF#$BB#$BF, st) = 1 then
  begin
    _SetXmlText(st);
    Exit;
  end;

  while (st <> '') and (st[1] <> '<') do
    Delete(st, 1, 1);

  if Pos('<?xml version', st) = 1 then
  begin
    _SetXmlText(st);
  end
  else
  begin
    _SetXmlText('<?xml version="1.0" encoding ="utf-8"?>'#13#10 + st);
  end;
end;

function GetFirstNode(const XMLDoc: TDOMDocument): TDOMNode;
begin
  if XMLDoc = nil then
  begin
    Result := nil;
    Exit;
  end;
  Result := XMLDoc.FirstChild;
  //после этого в виндовозе в Result.NodeName будет строка вида <?xml version="1.0" encoding...,
  //а никсы пропускают этот "заголовок" и возвращают следующую ноду. учитываем этот момент
  if not Result.HasChildNodes then
    Result := Result.NextSibling;  //этот If выполнится для виндовоза
end;

function FindAttrByName(const XMLNode: TDOMNode; AttrName: string): string;
var
  j: Integer;
begin
  Result := '';
  AttrName := UpperCase(AttrName);
  for j := 0 to XMLNode.Attributes.Length - 1 do
    if UpperCase(XMLNode.Attributes[j].NodeName) = AttrName then
    begin
      Result := XMLNode.Attributes[j].NodeValue;
      Exit;
    end;
end;

function FindChildNodeByName(const XMLNode: TDOMNode; TagName: string): TDOMNode;
var
  j: Integer;
  childs: TDOMNodeList;
begin
  Result := nil;
  childs := XMLNode.ChildNodes;
  if childs = nil then
    Exit;
  for j := 0 to childs.Length - 1 do
  begin
    if childs[j].NodeName = TagName then
    begin
      Result := childs[j];
      Exit;
    end;
  end;
end;


function GetIntValByName(const XMLNode: TDOMNode; const AttrName: string;
  const DefaultValue: Integer): int64;
begin
  Result := StrToInt64Def(FindAttrByName(XMLNode, AttrName), DefaultValue);
end;

function NodeNameIs(const XMLNode: TDOMNode; const NodeName: string): Boolean;
begin
  Result := UpperCase(XMLNode.NodeName) = UpperCase(NodeName);
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

end.

