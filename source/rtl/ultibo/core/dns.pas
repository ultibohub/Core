{
Ultibo DNS client unit.

Copyright (C) 2020 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:
 
 
References
==========

 
DNS
===

 Note: DNS Client is IPv4 based, for IPv6 see new
       Winsock2 functions

       See RFC 1035 Section 4 for details
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DNS;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,SysUtils,Classes,Network,Transport,Protocol,IP,IPv6,UDP,Ultibo,UltiboUtils,UltiboClasses;

//To Do //Implement GetAddrInfo/GetNameInfo/FreeAddrInfo

//To Do //Implement DnsQuery etc from JwaWinDns

//To Do //IPv6 support

//To Do //Look for:

//--

//Remove

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {DNS specific constants}
 {DNS Constants}
 DNS_TIMEOUT = 5000;  {We wait for 5 seconds for a DNS reply}
 DNS_RETRIES = 4;     {Try the request 4 times}

 DNS_HEADER_SIZE = 12;    {SizeOf(TDNSHeader);}

 DNS_QUESTION_SIZE = 4;   {SizeOf(TDNSQuestion);}
 DNS_RESOURCE_SIZE = 10;  {SizeOf(TDNSResource);} {Not including Record Data}

 MAX_DNS_NAME    = 255;
 MAX_DNS_LABEL   = 63;
 MAX_DNS_MESSAGE = 512;   {Maximum Size of DNS Message}

 {DNS Flags}
 DNS_FLAG_RESPONSE    = $8000; // query = 0, response = 1
 DNS_FLAG_AUTHORITY   = $0400; // Authoritative answer
 DNS_FLAG_TRUNCATED   = $0200; // Truncation, response was cut off at 512
 DNS_FLAG_DO_RECURSE  = $0100; // Recursion desired
 DNS_FLAG_CAN_RECURSE = $0080; // Recursion available

 {Field Masks in Flags}
 DNS_OPCODE_MASK   =  $7800;   // opcode, see below
 DNS_RESPONSE_MASK =  $000F;   // response code, see below

 {DNS Opcodes}
 DNS_QUERY             = 0;  // a standard query
 DNS_INV_QUERY         = 1;  // an inverse query
 DNS_COMP_QUERY_MULTI  = 2;  // a completion query, multiple reply (Obsolete)
 DNS_COMP_QUERY_SINGLE = 3;  // a completion query, single reply   (Obsolete)

 {DNS Response Codes}
 DNS_NO_ERROR        = 0;
 DNS_FORMAT_ERROR    = 1;
 DNS_SERVER_FAILURE  = 2;
 DNS_NAME_ERROR      = 3;
 DNS_NOT_IMPLEMENTED = 4;
 DNS_REFUSED         = 5;

 {DNS Record Types}
 DNS_TYPE_A       = 1;  // host address resource record (RR)
 DNS_TYPE_NS      = 2;
 DNS_TYPE_MD      = 3;
 DNS_TYPE_MF      = 4;
 DNS_TYPE_CNAME   = 5;
 DNS_TYPE_SOA     = 6;
 DNS_TYPE_MB      = 7;
 DNS_TYPE_MG      = 8;
 DNS_TYPE_MR      = 9;
 DNS_TYPE_RT_NULL = 10;
 DNS_TYPE_WKS     = 11;
 DNS_TYPE_PTR     = 12; // a domain name ptr
 DNS_TYPE_HINFO   = 13;
 DNS_TYPE_MINFO   = 14;
 DNS_TYPE_MX      = 15; // mail exchange
 //To Do //AAAA

 {DNS Address Classes}
 DNS_CLASS_IN   = 1;     // ARPA internet class
 DNS_CLASS_CS   = 2;
 DNS_CLASS_WILD = 255;   // wildcard for several of the classifications
 //To Do //IN6 ?
 
 {DNS Message Compression}
 DNS_POINTER_MASK = $C0;  //mask to indicate pointer to previously used name

{==============================================================================}
type
 {DNS specific types}
 PDNSHeader = ^TDNSHeader;
 TDNSHeader = packed record {All Network Order}
  Identifier:Word;          // unique identifier
  Flags:Word;               // QD/Opcode/AA/TC/RD/RA/RCODE
  QuestionCount:Word;       // question section, number of entries
  AnswerCount:Word;         // answers, how many
  AuthorityCount:Word;      // count of name server RRs
  AdditionalCount:Word;     // number of "additional" records
 end;

 PDNSMessage = ^TDNSMessage;
 TDNSMessage = packed record
  DNS:TDNSHeader;
  Data:array[0..(MAX_DNS_MESSAGE - DNS_HEADER_SIZE) - 1] of Byte;
 end;

 PDNSName = ^TDNSName;
 TDNSName = array[0..MAX_DNS_NAME - 1] of Char;

 PDNSQuestion = ^TDNSQuestion;
 TDNSQuestion = packed record {All Network Order}
  {Name:TDNSName;}    // variable length question name
  QuestionType:Word;    // question type (eg DNS_TYPE_A)
  QuestionClass:Word;   // question class (eg DNS_CLASS_IN)
 end;

 PDNSResource = ^TDNSResource;
 TDNSResource = packed record {All Network Order}
  {Name:TDNSName;}    // variable length resource name
  RecordType:Word;    // resource record type (eg DNS_TYPE_A)
  RecordClass:Word;   // resource record class (eg DNS_CLASS_IN)
  Ttl:LongWord;       // time-to-live, changed to 32 bits
  DataLength:Word;    // length of data field
  RecordData:array[0..MAX_DNS_MESSAGE - 1] of Byte; // data field
 end;
 
 PDNSClientData = ^TDNSClientData;
 TDNSClientData = record {Used for TLS Data}
  {Host Ent}
  HostEnt:THostEnt;                  
  HostEntName:array[0..MAX_NAME_SIZE - 1] of Char;  
  HostAliasesPtr:PChar;
  HostAliases:array[0..(MAX_NAME_SIZE * MAX_NAME_ALIASES) - 1] of Char;
  HostAddrListPtr:PChar;
  HostAddrList:array[0..MAX_HOST_ALIASES - 1] of TInAddr;
  {Net Ent}
  NetEnt:TNetEnt;
  NetEntName:array[0..MAX_NAME_SIZE - 1] of Char;
  NetAliasesPtr:PChar;
  NetAliases:array[0..(MAX_NAME_SIZE * MAX_NAME_ALIASES) - 1] of Char;
  {Serv Ent}
  ServEnt:TServEnt;
  ServEntName:array[0..MAX_NAME_SIZE - 1] of Char;
  ServAliasesPtr:PChar;
  ServAliases:array[0..(MAX_NAME_SIZE * MAX_NAME_ALIASES) - 1] of Char;
  ServEntProto:array[0..MAX_NAME_SIZE - 1] of Char;
  {Proto Ent}
  ProtoEnt:TProtoEnt;
  ProtoEntName:array[0..MAX_NAME_SIZE - 1] of Char;
  ProtoAliasesPtr:PChar;
  ProtoAliases:array[0..(MAX_NAME_SIZE * MAX_NAME_ALIASES) - 1] of Char;
 end;
 
{==============================================================================}
type
 {DNS specific classes}
 TDNSClient = class(TNetworkClient)  {DNS client}
   constructor Create(AProtocol:TNetworkProtocol);
   destructor Destroy; override;
  private
   {Internal Variables} 
   FTlsIndex:LongWord;

   {Status Variables}

   {Internal Methods}
   function GetClientData:PDNSClientData;
   function CreateClientData:Boolean;
   
   function AddressEntryToHostEnt(AddressEntry:TIPAddressEntry;const AName:String;ACount:Integer):PHostEnt;
   function HostEntryToHostEnt(HostEntry:TIPHostEntry):PHostEnt;
   function NetworkEntryToNetEnt(NetworkEntry:TIPNetworkEntry):PNetEnt;
   function ServEntryToServEnt(ServEntry:TIPServEntry):PServEnt;
   function ProtoEntryToProtoEnt(ProtoEntry:TIPProtoEntry):PProtoEnt;

   //function AddressEntryToAddrInfo   //To do
   //function HostEntryToAddrInfo      //To do
   //function ServEntryToAddrInfo      //To do
   
   function InAddrToName(const AAddress:TInAddr):String;
   function NameToInAddr(const AName:String):TInAddr;

   function In6AddrToName(const AAddress:TIn6Addr):String;
   function NameToIn6Addr(const AName:String):TIn6Addr;
   
   function GetDNSMessageSize(AMessage:PDNSMessage):Integer;

   function GetDNSNameSize(AMessage:PDNSMessage;AOffset:Word):Word;
   function GetDNSQuestionSize(AMessage:PDNSMessage;AOffset:Word):Word;
   function GetDNSResourceSize(AMessage:PDNSMessage;AOffset:Word):Word;

   function GetDNSQuestionOffset(AMessage:PDNSMessage;ACount:Word):Word;
   function GetDNSAnswerOffset(AMessage:PDNSMessage;ACount:Word):Word;
   function GetDNSAuthorityOffset(AMessage:PDNSMessage;ACount:Word):Word;
   function GetDNSAdditionalOffset(AMessage:PDNSMessage;ACount:Word):Word;

   function CreateDNSQuery(AMessage:PDNSMessage;AIdentifier:Word):Boolean;
   function CheckDNSResponse(AMessage:PDNSMessage;AIdentifier:Word):Boolean;
   function HandleDNSResponse(AMessage:PDNSMessage;AIdentifier:Word):Boolean;

   function SendDNSQuery(ASocket:TProtocolSocket;const AServer:TInAddr;AData:Pointer;ALength,AType,AClass,AIdentifier:Word):Boolean;
   function RecvDNSResponse(ASocket:TProtocolSocket;AType,AClass,AIdentifier:Word):Boolean;

   function InsertDNSName(AMessage:PDNSMessage;AOffset:Word;AName:Pointer;ALength:Word):Boolean;
   function ExtractDNSName(AMessage:PDNSMessage;AOffset:Word;AName:Pointer;var ALength:Word):Boolean;
   function ExtractDNSRData(AMessage:PDNSMessage;AOffset:Word;AData:Pointer;var ALength:Word;AType,AClass:Word):Boolean;

   function InsertDNSQuestion(AMessage:PDNSMessage;ACount:Word;AData:Pointer;ALength,AType,AClass:Word):Boolean;
   function ExtractDNSAnswer(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength,AType,AClass:Word;var ATtl:LongWord):Boolean;
   function ExtractDNSAuthority(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength,AType,AClass:Word;var ATtl:LongWord):Boolean;
   function ExtractDNSAdditional(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength,AType,AClass:Word;var ATtl:LongWord):Boolean;
  public
   {Status Variables}

   {BSD Database Methods}
   function GetHostByAddr(AAddr:Pointer;ALength,AFamily:Integer):PHostEnt;
   function GetHostByName(AName:PChar):PHostEnt;

   function GetHostName(AName:PChar;ALength:Integer):Integer;

   function GetServByPort(APort:Integer;AProto:PChar):PServEnt;
   function GetServByName(AName,AProto:PChar):PServEnt;
   function GetProtoByNumber(AProto:Integer):PProtoEnt;
   function GetProtoByName(AName:PChar):PProtoEnt;

   function GetNetByAddr(AAddr:Pointer;ALength,AStruct:Integer):PNetEnt;
   function GetNetByName(AName:PChar):PNetEnt;
   
   //function GetAddrInfo(     //To do
   //function GetNameInfo      //To do
   //procedure FreeAddrInfo(   //To do
   
   //function DnsQuery/DnsQueryEx etc  //To do
   
   {Public Methods}
   function StartClient:Boolean; override;
   function StopClient:Boolean; override;
 end;
 
 //TDNSThread = class(TClientThread)  {DNS client} //To Do
 
{==============================================================================}
var
 {DNS specific variables}
 DNSClient:TDNSClient;
 
{==============================================================================}
{Initialization Functions}
procedure DNSInit;
function DNSStart:LongWord;
function DNSStop:LongWord;

{==============================================================================}
{DNS Functions}
  
{==============================================================================}
{DNS Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {DNS specific variables}
 DNSInitialized:Boolean;
 DNSStarted:Boolean;
 
{==============================================================================}
{==============================================================================}
{TDNSClient}
constructor TDNSClient.Create(AProtocol:TNetworkProtocol);
begin
 {}
 inherited Create(AProtocol);
 FTlsIndex:=TlsAllocEx(True);
end;

{==============================================================================}

destructor TDNSClient.Destroy;
begin
 {}
 WriterLock;
 try
  TlsFree(FTlsIndex);
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end;  
end;

{==============================================================================}

function TDNSClient.GetClientData:PDNSClientData;
{Get the DNS client data for the current thread}
begin
 {}
 {Get TLS Value}
 Result:=TlsGetValue(FTlsIndex);
 if Result = nil then
  begin
   {Create Client Data}
   if not CreateClientData then Exit;
   
   {Get TLS Value}
   Result:=TlsGetValue(FTlsIndex);
  end;
end;

{==============================================================================}

function TDNSClient.CreateClientData:Boolean;
{Allocate the DNS client data for the current thread}
var
 ClientData:PDNSClientData;
begin
 {}
 Result:=False;
 
 {Get TLS Value}
 ClientData:=TlsGetValue(FTlsIndex);
 if ClientData = nil then
  begin
   {Allocate TLS Value}
   ClientData:=AllocMem(SizeOf(TDNSClientData));
   if ClientData = nil then Exit;
   
   {Set TLS Value}
   if not TlsSetValue(FTlsIndex,ClientData) then Exit;
  end;
 
 {Setup Client Data}
 {Host Ent}
 FillChar(ClientData.HostEnt,SizeOf(THostEnt),0);
 FillChar(ClientData.HostEntName,MAX_NAME_SIZE,0);
 FillChar(ClientData.HostAliases,(MAX_NAME_SIZE * MAX_NAME_ALIASES),0);
 FillChar(ClientData.HostAddrList,(MAX_HOST_ALIASES * SizeOf(TInAddr)),0);
 ClientData.HostEnt.h_name:=@ClientData.HostEntName;
 ClientData.HostAliasesPtr:=@ClientData.HostAliases;
 ClientData.HostEnt.h_aliases:=@ClientData.HostAliasesPtr;
 ClientData.HostAddrListPtr:=@ClientData.HostAddrList;
 ClientData.HostEnt.h_addr_list:=@ClientData.HostAddrListPtr;
 {Net Ent}
 FillChar(ClientData.NetEnt,SizeOf(TNetEnt),0);
 FillChar(ClientData.NetEntName,MAX_NAME_SIZE,0);
 FillChar(ClientData.NetAliases,(MAX_NAME_SIZE * MAX_NAME_ALIASES),0);
 ClientData.NetEnt.n_name:=@ClientData.NetEntName;
 ClientData.NetAliasesPtr:=@ClientData.NetAliases;
 ClientData.NetEnt.n_aliases:=@ClientData.NetAliasesPtr;
 {Serv Ent}
 FillChar(ClientData.ServEnt,SizeOf(TServEnt),0);
 FillChar(ClientData.ServEntName,MAX_NAME_SIZE,0);
 FillChar(ClientData.ServAliases,(MAX_NAME_SIZE * MAX_NAME_ALIASES),0);
 FillChar(ClientData.ServEntProto,MAX_NAME_SIZE,0);
 ClientData.ServEnt.s_name:=@ClientData.ServEntName;
 ClientData.ServAliasesPtr:=@ClientData.ServAliases;
 ClientData.ServEnt.s_aliases:=@ClientData.ServAliasesPtr;
 ClientData.ServEnt.s_proto:=@ClientData.ServEntProto;
 {Proto Ent}
 FillChar(ClientData.ProtoEnt,SizeOf(TProtoEnt),0);
 FillChar(ClientData.ProtoEntName,MAX_NAME_SIZE,0);
 FillChar(ClientData.ProtoAliases,(MAX_NAME_SIZE * MAX_NAME_ALIASES),0);
 ClientData.ProtoEnt.p_name:=@ClientData.ProtoEntName;
 ClientData.ProtoAliasesPtr:=@ClientData.ProtoAliases;
 ClientData.ProtoEnt.p_aliases:=@ClientData.ProtoAliasesPtr;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.AddressEntryToHostEnt(AddressEntry:TIPAddressEntry;const AName:String;ACount:Integer):PHostEnt;
{Note: Caller must hold Address entry lock}
var
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;
 
 {Check Address Entry}
 if AddressEntry = nil then Exit;
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Check Address Type}
 case AddressEntry.AddressType of
  ADDRESS_TYPE_PRIMARY,ADDRESS_TYPE_SECONDARY:begin
    {Check Count}
    if ACount > MAX_HOST_ALIASES then Exit;

    if ACount = 1 then
     begin
      {Create Host Ent}
      ClientData.HostEnt.h_addrtype:=AddressEntry.Family;
      ClientData.HostEnt.h_length:=AddressEntry.Length;
      StrLCopy(ClientData.HostEntName,PChar(AName),MAX_NAME_SIZE);
      ClientData.HostAddrList[ACount - 1]:=InAddrToNetwork(AddressEntry.Address);
    
      {Return Result}
      Result:=@ClientData.HostEnt;
     end
    else
     begin
      {Update Host Ent}
      ClientData.HostAddrList[ACount - 1]:=InAddrToNetwork(AddressEntry.Address);
      
      {Return Result}
      Result:=@ClientData.HostEnt;
     end;     
   end;
  ADDRESS_TYPE_LOOPBACK:begin
    {Check Count}
    if ACount > MAX_HOST_ALIASES then Exit;
    
    if ACount = 1 then
     begin
      {Create Host Ent}
      ClientData.HostEnt.h_addrtype:=AddressEntry.Family;
      ClientData.HostEnt.h_length:=AddressEntry.Length;
      StrLCopy(ClientData.HostEntName,PChar(AName),MAX_NAME_SIZE);
      ClientData.HostAddrList[ACount - 1]:=InAddrToNetwork(AddressEntry.Address);
      
      {Return Result}
      Result:=@ClientData.HostEnt;
     end
    else
     begin
      {Update Host Ent}
      ClientData.HostAddrList[ACount - 1]:=InAddrToNetwork(AddressEntry.Address);
      
      {Return Result}
      Result:=@ClientData.HostEnt;
     end;     
   end;
 end;
end;

{==============================================================================}

function TDNSClient.HostEntryToHostEnt(HostEntry:TIPHostEntry):PHostEnt;
{Note: Caller must hold Host entry lock}
var
 Count:Integer;
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;
 
 {Check Host Entry}
 if HostEntry = nil then Exit;
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Host Ent}
 ClientData.HostEnt.h_addrtype:=HostEntry.Family;
 ClientData.HostEnt.h_length:=HostEntry.Length;
 StrLCopy(ClientData.HostEntName,PChar(HostEntry.Name),MAX_NAME_SIZE);
 
 //ClientData.HostAliases //To Do //MAX_NAME_ALIASES
 
 for Count:=0 to MAX_HOST_ALIASES - 1 do
  begin
   ClientData.HostAddrList[Count]:=InAddrToNetwork(HostEntry.Addresses[Count]);
  end;
 
 {Return Result}
 Result:=@ClientData.HostEnt;
end;

{==============================================================================}

function TDNSClient.NetworkEntryToNetEnt(NetworkEntry:TIPNetworkEntry):PNetEnt;
{Note: Caller must hold Network entry lock}
var
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;
 
 {Check Network Entry}
 if NetworkEntry = nil then Exit;
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Net Ent}
 ClientData.NetEnt.n_addrtype:=NetworkEntry.Family;
 ClientData.NetEnt.n_net:=LongInt(NetworkEntry.Network);
 StrLCopy(ClientData.NetEntName,PChar(NetworkEntry.Name),MAX_NAME_SIZE);
 
 //ClientData.NetAliases //To Do //MAX_NAME_ALIASES
 
 {Return Result}
 Result:=@ClientData.NetEnt;
end;

{==============================================================================}

function TDNSClient.ServEntryToServEnt(ServEntry:TIPServEntry):PServEnt;
{Note: Caller must hold Serv entry lock}
var
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;
 
 {Check Serv Entry}
 if ServEntry = nil then Exit;
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Serv Ent}
 ClientData.ServEnt.s_port:=WordNtoBE(ServEntry.Port);
 StrLCopy(ClientData.ServEntName,PChar(ServEntry.Name),MAX_NAME_SIZE);
 
 //ClientData.ServAliases //To Do //MAX_NAME_ALIASES
 
 StrLCopy(ClientData.ServEntProto,PChar(ServEntry.Protocol),MAX_NAME_SIZE);
 
 {Return Result}
 Result:=@ClientData.ServEnt;
end;

{==============================================================================}

function TDNSClient.ProtoEntryToProtoEnt(ProtoEntry:TIPProtoEntry):PProtoEnt;
{Note: Caller must hold Proto entry lock}
var
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;
 
 {Check Proto Entry}
 if ProtoEntry = nil then Exit;
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Proto Ent}
 ClientData.ProtoEnt.p_proto:=ProtoEntry.Number;
 StrLCopy(ClientData.ProtoEntName,PChar(ProtoEntry.Name),MAX_NAME_SIZE);
 
 //ClientData.ProtoAliases //To Do //MAX_NAME_ALIASES
 
 {Return Result}
 Result:=@ClientData.ProtoEnt;
end;

{==============================================================================}

function TDNSClient.InAddrToName(const AAddress:TInAddr):String;
{Converts an Address to a Name in IN-ADDR.ARPA format}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=InAddrToString(AAddress) + '.IN-ADDR.ARPA';
end;

{==============================================================================}

function TDNSClient.NameToInAddr(const AName:String):TInAddr;
{Converts a Name in IN-ADDR.ARPA format to an Address}
{Note: Returns Address in Host order}
var
 PosIdx:Integer;
begin
 {}
 Result.S_addr:=INADDR_ANY;
 
 {Check Suffix}
 PosIdx:=Pos('.IN-ADDR.ARPA',AName);
 if PosIdx = 0 then Exit;
 
 {Return Result}
 Result:=StringToInAddr(Copy(AName,1,PosIdx - 1));
end;

{==============================================================================}

function TDNSClient.In6AddrToName(const AAddress:TIn6Addr):String;
{Converts an Address to a Name in IP6.ARPA format}
var
 Count:Integer;
begin
 {}
 Result:='';
 
 for Count:=0 to 15 do
  begin
   {Add Dot}
   if Count > 0 then Result:=Result + '.';
   
   {First Nibble}
   Result:=Result + IntToHex((AAddress.u6_addr8[Count] and $0F),1) + '.';
   
   {Second Nibble}
   Result:=Result + IntToHex((AAddress.u6_addr8[Count] and $F0) shr 8,1);
  end;
  
 Result:=Result +  '.IP6.ARPA'; 
end;

{==============================================================================}

function TDNSClient.NameToIn6Addr(const AName:String):TIn6Addr;
{Converts a Name in IN-IP6.ARPA format to an Address}
var
 PosIdx:Integer;
begin
 {} 
 Result:=IN6ADDR_ANY;
 
 {Check Suffix}
 PosIdx:=Pos('.IP6.ARPA',AName);
 if PosIdx = 0 then Exit;

 {Return Result}
 //To Do //Break down string into each nibble, convert to byte, OR together and insert into Result
end; 
{==============================================================================}

function TDNSClient.GetDNSMessageSize(AMessage:PDNSMessage):Integer;
{Return size of DNS Header plus Questions and Resource Records}
var
 Offset:Word;
 Count:Integer;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Set Result}
 Result:=DNS_HEADER_SIZE;
 
 {Get the Question sizes}
 for Count:=1 to WordBEtoN(AMessage.DNS.QuestionCount) do
  begin
   {Get Offset}
   Offset:=GetDNSQuestionOffset(AMessage,Count);
   
   {Update Result}
   Result:=Result + GetDNSQuestionSize(AMessage,Offset);
  end;
 
 {Get the Answer sizes}
 for Count:=1 to WordBEtoN(AMessage.DNS.AnswerCount) do
  begin
   {Get Offset}
   Offset:=GetDNSAnswerOffset(AMessage,Count);
   
   {Update Result}
   Result:=Result + GetDNSResourceSize(AMessage,Offset);
  end;
 
 {Get the Authority sizes}
 for Count:=1 to WordBEtoN(AMessage.DNS.AuthorityCount) do
  begin
   {Get Offset}
   Offset:=GetDNSAuthorityOffset(AMessage,Count);
   
   {Update Result}
   Result:=Result + GetDNSResourceSize(AMessage,Offset);
  end;
 
 {Get the Additional sizes}
 for Count:=1 to WordBEtoN(AMessage.DNS.AdditionalCount) do
  begin
   {Get Offset}
   Offset:=GetDNSAdditionalOffset(AMessage,Count);
   
   {Update Result}
   Result:=Result + GetDNSResourceSize(AMessage,Offset);
  end;
end;

{==============================================================================}

function TDNSClient.GetDNSNameSize(AMessage:PDNSMessage;AOffset:Word):Word;
{Returns the actual size of the Name pointed to by Offset}
{Compressed Names will return the compressed size not the full size}
var
 Offset:Word;
 Name:PDNSName;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Set Offset}
 Offset:=0;
 
 {Get the Name}
 Name:=PDNSName(PtrUInt(AMessage) + AOffset);
 
 {Check for Last Label}
 while Byte(Name[Offset]) <> 0 do
  begin
   {Check for Pointer (Compressed)}
   if (Byte(Name[Offset]) and DNS_POINTER_MASK) = DNS_POINTER_MASK then
    begin
     {Return the Size}
     Result:=Offset + SizeOf(Word);
     Exit;
    end;
   
   {Move to Next Label}
   Inc(Offset,Byte(Name[Offset]) + 1);
   
   {Check for End of Buffer}
   if Offset > (MAX_DNS_NAME - 1) then Exit;
  end;
 
 {Return the Size}
 Result:=Offset + 1;
end;

{==============================================================================}

function TDNSClient.GetDNSQuestionSize(AMessage:PDNSMessage;AOffset:Word):Word;
{Returns the actual size of the Question pointed to by Offset}
{Compressed Names will return the compressed size not the full size}
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Name size and Question size}
 Result:=GetDNSNameSize(AMessage,AOffset) + DNS_QUESTION_SIZE;
end;

{==============================================================================}

function TDNSClient.GetDNSResourceSize(AMessage:PDNSMessage;AOffset:Word):Word;
{Returns the actual size of the Resource pointed to by Offset}
{Compressed Names will return the compressed size not the full size}
var
 Resource:PDNSResource;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Name size}
 Result:=GetDNSNameSize(AMessage,AOffset);
 
 {Get the Resource size}
 Resource:=PDNSResource(PtrUInt(AMessage) + AOffset + Result);
 
 {Return Result}
 Result:=Result + DNS_RESOURCE_SIZE + WordBEtoN(Resource.DataLength);
end;

{==============================================================================}

function TDNSClient.GetDNSQuestionOffset(AMessage:PDNSMessage;ACount:Word):Word;
{Return the offset to the DNS Question number Count}
{Question numbers start at 1, if Count is higher than the
 current number of questions returns the offset for next Question}
var
 Count:Word;
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check Count}
 if ACount < 1 then Exit;
 
 {Get the Starting Offset}
 Offset:=DNS_HEADER_SIZE;
 
 {Enumerate the Questions}
 for Count:=1 to WordBEtoN(AMessage.DNS.QuestionCount) do
  begin
   {Check for Question}
   if Count = ACount then Break;
   
   {Get Offset of next Question}
   Inc(Offset,GetDNSQuestionSize(AMessage,Offset));
   
   {Check for End of Buffer}
   if Offset > (MAX_DNS_MESSAGE - 1) then Exit;
  end;
 
 {Return Offset}
 Result:=Offset;
end;

{==============================================================================}

function TDNSClient.GetDNSAnswerOffset(AMessage:PDNSMessage;ACount:Word):Word;
{Return the offset to the DNS Answer number Count}
{Answer numbers start at 1, if Count is higher than the
 current number of answers returns the offset for next Answer}
var
 Count:Word;
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check Count}
 if ACount < 1 then Exit;
 
 {Get the Starting Offset}
 Offset:=GetDNSQuestionOffset(AMessage,WordBEtoN(AMessage.DNS.QuestionCount) + 1);
 
 {Enumerate the Answers}
 for Count:=1 to WordBEtoN(AMessage.DNS.AnswerCount) do
  begin
   {Check for Answer}
   if Count = ACount then Break;
   
   {Get Offset of next Answer}
   Inc(Offset,GetDNSResourceSize(AMessage,Offset));
   
   {Check for End of Buffer}
   if Offset > (MAX_DNS_MESSAGE - 1) then Exit;
  end;
 
 {Return Offset}
 Result:=Offset;
end;

{==============================================================================}

function TDNSClient.GetDNSAuthorityOffset(AMessage:PDNSMessage;ACount:Word):Word;
{Return the offset to the DNS Authority number Count}
{Authority numbers start at 1, if Count is higher than the
 current number of authoritys returns the offset for next Authority}
var
 Count:Word;
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check Count}
 if ACount < 1 then Exit;
 
 {Get the Starting Offset}
 Offset:=GetDNSAnswerOffset(AMessage,WordBEtoN(AMessage.DNS.AnswerCount) + 1);
 
 {Enumerate the Authoritys}
 for Count:=1 to WordBEtoN(AMessage.DNS.AuthorityCount) do
  begin
   {Check for Authority}
   if Count = ACount then Break;
   
   {Get Offset of next Authority}
   Inc(Offset,GetDNSResourceSize(AMessage,Offset));
   
   {Check for End of Buffer}
   if Offset > (MAX_DNS_MESSAGE - 1) then Exit;
  end;
 
 {Return Offset}
 Result:=Offset;
end;

{==============================================================================}

function TDNSClient.GetDNSAdditionalOffset(AMessage:PDNSMessage;ACount:Word):Word;
{Return the offset to the DNS Additional number Count}
{Additional numbers start at 1, if Count is higher than the
 current number of additionals returns the offset for next Additional}
var
 Count:Word;
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check Count}
 if ACount < 1 then Exit;
 
 {Get the Starting Offset}
 Offset:=GetDNSAuthorityOffset(AMessage,WordBEtoN(AMessage.DNS.AuthorityCount) + 1);
 
 {Enumerate the Additionals}
 for Count:=1 to WordBEtoN(AMessage.DNS.AdditionalCount) do
  begin
   {Check for Additional}
   if Count = ACount then Break;
   
   {Get Offset of next Additional}
   Inc(Offset,GetDNSResourceSize(AMessage,Offset));
   
   {Check for End of Buffer}
   if Offset > (MAX_DNS_MESSAGE - 1) then Exit;
  end;
 
 {Return Offset}
 Result:=Offset;
end;

{==============================================================================}

function TDNSClient.CreateDNSQuery(AMessage:PDNSMessage;AIdentifier:Word):Boolean;
var
 Flags:Word;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Creating DNS Query');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Zero Message}
 FillChar(AMessage^,SizeOf(TDNSMessage),0);
 
 {Set the Id}
 AMessage.DNS.Identifier:=WordNtoBE(AIdentifier);
 
 {Set the Flags}
 Flags:=DNS_QUERY shl 11;              {Opcode}
 Flags:=Flags or DNS_FLAG_DO_RECURSE;  {Recursion Desired}
 AMessage.DNS.Flags:=WordNtoBE(Flags);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.CheckDNSResponse(AMessage:PDNSMessage;AIdentifier:Word):Boolean;
var
 Flags:Word;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Checking DNS Response');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check the Id}
 if WordBEtoN(AMessage.DNS.Identifier) <> AIdentifier then Exit;
 
 {Check the Flags}
 Flags:=WordBEtoN(AMessage.DNS.Flags);
 if ((Flags and DNS_OPCODE_MASK) shr 11) <> DNS_QUERY then Exit;
 if (Flags and DNS_FLAG_RESPONSE) <> DNS_FLAG_RESPONSE then Exit;
 if (Flags and DNS_RESPONSE_MASK) <> DNS_NO_ERROR then Exit;
 
 {Check the Counts}
 if WordBEtoN(AMessage.DNS.QuestionCount) < 1 then Exit;
 if WordBEtoN(AMessage.DNS.AnswerCount) < 1 then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.HandleDNSResponse(AMessage:PDNSMessage;AIdentifier:Word):Boolean;
var
 Name:Pointer;
 Data:Pointer;
 Count:Integer;
 NameLength:Word;
 DataLength:Word;
 AnswerType:Word;
 AnswerClass:Word;
 AnswerTtl:LongWord;
 Transport:TIPTransport;
 HostEntry:TIPHostEntry;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Handling DNS Response');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;

 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Get the Transport} //To Do //IPv6 //Maybe get Transport on each Answer ?
 Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ)); 
 if Transport = nil then Exit;
 try
  {Create the Buffers}
  Name:=GetMem(MAX_DNS_NAME);
  if Name = nil then Exit;
  try
   Data:=GetMem(MAX_DNS_NAME);
   if Data = nil then Exit;
   try
    {Enumerate the Answers}
    for Count:=1 to WordBEtoN(AMessage.DNS.AnswerCount) do
     begin
      {Get each Answer}
      if ExtractDNSAnswer(AMessage,Count,Name,Data,NameLength,DataLength,AnswerType,AnswerClass,AnswerTtl) then
       begin
        {Process the Class}
        case AnswerClass of
         DNS_CLASS_IN:begin
           {Process the Type}
           case AnswerType of
            DNS_TYPE_A:begin
              {Insert a Host Entry or update existing Entry}
              HostEntry:=Transport.GetHostByName(PChar(Name),True);
              if HostEntry = nil then
               begin
                {Add Host}
                Result:=(Transport.AddHost(PInAddr(Data)^,PChar(Name),HOST_TYPE_DYNAMIC,False) <> nil);
               end
              else
               begin
                {Update Host}
                Result:=HostEntry.AddAddress(PInAddr(Data)^);
                
                {Unlock Host}
                HostEntry.ReleaseLock;
               end;
             end;
            DNS_TYPE_PTR:begin
              {Insert a Host Entry or update existing Entry}
              HostEntry:=Transport.GetHostByAddress(PInAddr(Name)^,True); 
              if HostEntry = nil then
               begin
                {Add Host}
                Result:=(Transport.AddHost(PInAddr(Name)^,PChar(Data),HOST_TYPE_DYNAMIC,False) <> nil);
               end
              else
               begin
                {Update Host}
                Result:=HostEntry.AddAlias(PChar(Data));
                
                {Unlock Host}
                HostEntry.ReleaseLock;
               end;
             end;
            DNS_TYPE_CNAME:begin
              {Insert a Host Entry or update existing Entry}
              HostEntry:=Transport.GetHostByName(PChar(Name),True); 
              if HostEntry = nil then
               begin
                {Add Host}
                HostEntry:=Transport.AddHost(IP_DEFAULT_ADDRESS,PChar(Name),HOST_TYPE_DYNAMIC,True);
                if HostEntry <> nil then
                 begin
                  {Update Host}
                  Result:=HostEntry.AddAlias(PChar(Data));
                  
                  {Unlock Host}
                  HostEntry.ReleaseLock;
                 end;
               end
              else
               begin
                {Update Host}
                Result:=HostEntry.AddAlias(PChar(Data));
                
                {Unlock Host}
                HostEntry.ReleaseLock;
               end;
             end;
           end;
          end;
        end;
       end;
     end;
   finally
    FreeMem(Data);
   end;
  finally
   FreeMem(Name);
  end;
 finally
  Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.SendDNSQuery(ASocket:TProtocolSocket;const AServer:TInAddr;AData:Pointer;ALength,AType,AClass,AIdentifier:Word):Boolean;
{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 Query:TDNSMessage;
 SockAddr:TSockAddr;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Sending DNS Query');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Create the Message}
 if CreateDNSQuery(@Query,AIdentifier) then
  begin
   {Insert the Question}
   if not InsertDNSQuestion(@Query,1,AData,ALength,AType,AClass) then Exit;
   
   {Get the Size}
   Size:=GetDNSMessageSize(@Query);
   
   //To Do //IPv6 //Check Socket Family
   
   {Set the Address}
   SockAddr.sin_family:=AF_INET;
   SockAddr.sin_port:=WordNtoBE(IPPORT_DNS);
   SockAddr.sin_addr:=InAddrToNetwork(AServer);
   
   {Send the Query}
   Result:=FProtocol.SendTo(ASocket,Query,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
  end;
end;

{==============================================================================}

function TDNSClient.RecvDNSResponse(ASocket:TProtocolSocket;AType,AClass,AIdentifier:Word):Boolean;
{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 SockSize:Integer;
 SockAddr:TSockAddr;
 Response:TDNSMessage;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Receiving DNS Response');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Get the Size}
 Size:=SizeOf(TDNSMessage);
 
 {Create the Buffer}
 FillChar(Response,SizeOf(TDNSMessage),0);
 
 {Receive Datagram}
 SockSize:=SizeOf(TSockAddr);
 if FProtocol.RecvFrom(ASocket,Response,Size,0,SockAddr,SockSize) > 0 then
  begin
   {Check for Response}
   if CheckDNSResponse(@Response,AIdentifier) then
    begin
     {Handle Response}
     Result:=HandleDNSResponse(@Response,AIdentifier);
     if Result then Exit;
    end;
  end;
end;

{==============================================================================}

function TDNSClient.InsertDNSName(AMessage:PDNSMessage;AOffset:Word;AName:Pointer;ALength:Word):Boolean;
{Inserts the Name into the Message, Length contains the length of the Name}
{Converts name into DNS encoded format from ASCII string}
var
 Data:PChar;
 Start:Word;
 Offset:Word;
 Length:Byte;
 Count:Integer;
 Name:PDNSName;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Inserting DNS Name');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check Length}
 if ALength > MAX_DNS_NAME then Exit;
 
 {Get the Data}
 Data:=PChar(AName);
 
 {Get the Name}
 Name:=PDNSName(PtrUInt(AMessage) + AOffset);
 
 {Set the Offsets}
 Start:=0;
 Offset:=1;
 Length:=0;
 
 {Create the Labels}
 for Count:=0 to ALength - 1 do
  begin
   if Data[Count] = '.' then
    begin
     {Mark the Label}
     Byte(Name[Start]):=Length;
     Start:=Offset;
     Inc(Offset);
     Length:=0;
    end
   else
    begin
     {Copy the Data}
     Name[Offset]:=Data[Count];
     Inc(Offset);
     Inc(Length);
    end;
  end;
  
 {Mark the Label}
 Byte(Name[Start]):=Length;
 
 {Set the Last Label}
 Byte(Name[Offset]):=0;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.ExtractDNSName(AMessage:PDNSMessage;AOffset:Word;AName:Pointer;var ALength:Word):Boolean;
{Extracts the Name from the Message, Length returns the length of the Name}
{Converts name into ASCII string from DNS encoded format}
var
 Data:PChar;
 Offset:Word;
 Length:Byte;
 Name:PDNSName;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Name');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;

 {Get the Data}
 Data:=PChar(AName);

 {Get the Name}
 Name:=PDNSName(PtrUInt(AMessage) + AOffset);

 {Setup the Offsets}
 Offset:=0;
 ALength:=0;
 
 {Check for Last Label}
 while Byte(Name[Offset]) <> 0 do
  begin
   Length:=Byte(Name[Offset]);
   
   {Check for Pointer (Compressed)}
   if (Length and DNS_POINTER_MASK) = DNS_POINTER_MASK then
    begin
     {Get the Offset to the Compressed Name}
     Offset:=((Length and not(DNS_POINTER_MASK)) shl 8) + Byte(Name[Offset + 1]);
     
     {Return Result}
     Result:=ExtractDNSName(AMessage,Offset,Data,ALength);
     Exit;
    end
   else
    begin
     {Add the dot}
     if Offset <> 0 then
      begin
       Data^:='.';
       
       Inc(PtrUInt(Data));
       Inc(ALength);
      end;
     
     {Copy the Data}
     System.Move(Name[Offset + 1],Data^,Length);
     
     Inc(PtrUInt(Data),Length);
     Inc(ALength,Length);
    end;
   
   {Move to Next Label}
   Inc(Offset,Length + 1);
   
   {Check for End of Buffer}
   if Offset > (MAX_DNS_NAME - 1) then Exit;
  end;
  
 {Mark the end}
 Data^:=#0;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.ExtractDNSRData(AMessage:PDNSMessage;AOffset:Word;AData:Pointer;var ALength:Word;AType,AClass:Word):Boolean;
{Extracts the Resource Data from the Message, Length returns the length of the Data}
{For Types that are a Name simply calls ExtractDNSName to unformat the Data}
var
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS RData');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Resource}
 Resource:=PDNSResource(PtrUInt(AMessage) + AOffset);
 
 {Check the Class}
 case AClass of
  DNS_CLASS_IN:begin
    {Check the Type}
    case AType of
     DNS_TYPE_A:begin
       {Extract an Address}
       if WordBEtoN(Resource.DataLength) <> SizeOf(TInAddr) then Exit;
       ALength:=SizeOf(TInAddr);
       System.Move(Resource.RecordData[0],AData^,ALength);
       PInAddr(AData)^:=InAddrToHost(PInAddr(AData)^);
       
       {Return Result}
       Result:=True;
      end;
     DNS_TYPE_NS,DNS_TYPE_PTR,DNS_TYPE_CNAME:begin
       {Extract a Name}
       Result:=ExtractDNSName(AMessage,AOffset + DNS_RESOURCE_SIZE,AData,ALength);
      end;
     else
      begin
       {Extract Binary Data}
       ALength:=WordBEtoN(Resource.DataLength);
       System.Move(Resource.RecordData[0],AData^,ALength);
       
       {Return Result}
       Result:=True;
      end;
    end;
   end;
  else
   begin
    Exit;
   end;
 end;
end;

{==============================================================================}

function TDNSClient.InsertDNSQuestion(AMessage:PDNSMessage;ACount:Word;AData:Pointer;ALength,AType,AClass:Word):Boolean;
var
 Name:String;
 Offset:Word;
 Question:PDNSQuestion;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Inserting DNS Question - Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Offset}
 Offset:=GetDNSQuestionOffset(AMessage,ACount);
 if Offset = 0 then Exit;
 
 {Insert the Name}
 case AClass of
  DNS_CLASS_IN:begin
    {Check the Type}
    case AType of
     DNS_TYPE_PTR:begin
       {If PTR convert InAddr to Name}
       if ALength <> SizeOf(TInAddr) then Exit;
       Name:=InAddrToName(InAddrToHost(PInAddr(AData)^));
       if not InsertDNSName(AMessage,Offset,PChar(Name),Length(Name)) then Exit;
      end;
     else
      begin
       {All others simply add the Name}
       if not InsertDNSName(AMessage,Offset,AData,ALength) then Exit;
      end;
    end;
   end;
  else
   begin
    Exit;
   end;
 end;
 
 {Set the Question}
 Inc(Offset,GetDNSNameSize(AMessage,Offset));
 Question:=PDNSQuestion(PtrUInt(AMessage) + Offset);
 Question.QuestionType:=WordNtoBE(AType);
 Question.QuestionClass:=WordNtoBE(AClass);
 
 {Set the Question Count}
 AMessage.DNS.QuestionCount:=WordNtoBE(WordBEtoN(AMessage.DNS.QuestionCount) + 1);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.ExtractDNSAnswer(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength,AType,AClass:Word;var ATtl:LongWord):Boolean;
var
 Offset:Word;
 Address:TInAddr;
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Answer - Count = ' + IntToStr(ACount));
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Offset}
 Offset:=GetDNSAnswerOffset(AMessage,ACount);
 if Offset = 0 then Exit;
 
 {Extract the Name}
 if not ExtractDNSName(AMessage,Offset,AName,ANameLength) then Exit;
 
 {Get the Resource}
 Inc(Offset,GetDNSNameSize(AMessage,Offset));
 Resource:=PDNSResource(PtrUInt(AMessage) + Offset);
 AType:=WordBEtoN(Resource.RecordType);
 AClass:=WordBEtoN(Resource.RecordClass);
 ATtl:=LongWordBEtoN(Resource.Ttl);
 
 {Extract the Data}
 case AClass of
  DNS_CLASS_IN:begin
    {Check the Type}
    case AType of
     DNS_TYPE_PTR:begin
       {If PTR convert Name to InAddr}
       Address:=NameToInAddr(PChar(AName));
       PInAddr(AName)^:=Address;
       ANameLength:=SizeOf(TInAddr);
       if not ExtractDNSRData(AMessage,Offset,AData,ADataLength,AType,AClass) then Exit;
      end;
     else
      begin
       {All others simply extract the Data}
       if not ExtractDNSRData(AMessage,Offset,AData,ADataLength,AType,AClass) then Exit;
      end;
    end;
   end;
  else
   begin
    Exit;
   end;
 end;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.ExtractDNSAuthority(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength,AType,AClass:Word;var ATtl:LongWord):Boolean;
var
 Offset:Word;
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Authority');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Offset}
 Offset:=GetDNSAuthorityOffset(AMessage,ACount);
 if Offset = 0 then Exit;
 
 {Extract the Name}
 if not ExtractDNSName(AMessage,Offset,AName,ANameLength) then Exit;
 
 {Get the Resource}
 Inc(Offset,GetDNSNameSize(AMessage,Offset));
 Resource:=PDNSResource(PtrUInt(AMessage) + Offset);
 AType:=WordBEtoN(Resource.RecordType);
 AClass:=WordBEtoN(Resource.RecordClass);
 ATtl:=LongWordBEtoN(Resource.Ttl);
 
 {Extract the Data}
 //To Do //See above
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.ExtractDNSAdditional(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength,AType,AClass:Word;var ATtl:LongWord):Boolean;
var
 Offset:Word;
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Additional');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Get the Offset}
 Offset:=GetDNSAdditionalOffset(AMessage,ACount);
 if Offset = 0 then Exit;
 
 {Extract the Name}
 if not ExtractDNSName(AMessage,Offset,AName,ANameLength) then Exit;
 
 {Get the Resource}
 Inc(Offset,GetDNSNameSize(AMessage,Offset));
 Resource:=PDNSResource(PtrUInt(AMessage) + Offset);
 AType:=WordBEtoN(Resource.RecordType);
 AClass:=WordBEtoN(Resource.RecordClass);
 ATtl:=LongWordBEtoN(Resource.Ttl);
 
 {Extract the Data}
 //To Do //See above
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}

function TDNSClient.GetHostByAddr(AAddr:Pointer;ALength,AFamily:Integer):PHostEnt;
{Performs a DNS_TYPE_PTR Query for the Domain name of the Address supplied}
{Note: Address will be in network order where applicable}
var
 Count:Integer;
 Retries:Integer;
 Timeout:Integer;
 Identifier:Word;
 SockAddr:TSockAddr;
 Socket:TProtocolSocket;
 Transport:TIPTransport;
 HostEntry:TIPHostEntry;
 Nameservers:TIPNameservers;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetHostByAddr');
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;
  
  //To Do //IPv6 //Check ALength/AFamily for AF_INET/AF_INET6 etc
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AAddr = nil then Exit;
   if AFamily <> AF_INET then Exit;
   if ALength < SizeOf(TInAddr) then Exit;
   
   {Check the Cache}
   HostEntry:=Transport.GetHostByAddress(InAddrToHost(TInAddr(AAddr^)),True); 
   if HostEntry = nil then
    begin
     {Create a Socket}
     Socket:=FProtocol.Socket(AF_INET,FProtocol.SocketType,FProtocol.Protocol);
     if Socket = nil then Exit;
    
     {Lock Socket}
     Socket.ReaderLock;
     try
      {Bind the Socket}
      SockAddr.sin_family:=AF_INET;
      SockAddr.sin_port:=WordNtoBE(IPPORT_ANY);
      SockAddr.sin_addr.S_addr:=INADDR_ANY;
      if FProtocol.Bind(Socket,SockAddr,SizeOf(TSockAddr)) = SOCKET_ERROR then Exit;
      
      {Set the Timeout}
      Timeout:=DNS_TIMEOUT;
      if FProtocol.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) = SOCKET_ERROR then Exit;
      
      {Get the Nameservers}
      Nameservers:=Transport.Nameservers; //To Do //GetNameserverByNext etc
      for Count:=0 to MAX_NAME_SERVERS - 1 do
       begin
        if not Transport.CompareDefault(Nameservers[Count]) then
         begin
          Retries:=0;
          while Retries < DNS_RETRIES do
           begin
            {Get Identifier}
            Identifier:=Random($7FFF) + 1;
            
            {Send Request to each Nameserver}
            if SendDNSQuery(Socket,Nameservers[Count],AAddr,ALength,DNS_TYPE_PTR,DNS_CLASS_IN,Identifier) then
             begin
              {Receive Response}
              if RecvDNSResponse(Socket,DNS_TYPE_PTR,DNS_CLASS_IN,Identifier) then
               begin
                {Check the Cache}
                HostEntry:=Transport.GetHostByAddress(InAddrToHost(TInAddr(AAddr^)),True);
                if HostEntry <> nil then
                 begin
                  {Return Host Entry if found}
                  Result:=HostEntryToHostEnt(HostEntry);
                  NetworkSetLastError(ERROR_SUCCESS);
                  
                  {Unlock Host}
                  HostEntry.ReleaseLock;
                  Exit;
                 end;
               end;
             end;
             
            Inc(Retries);
           end;
         end;
       end;
     finally
      {Close the Socket}
      FProtocol.CloseSocket(Socket);
      
      {Unlock Socket}
      Socket.ReaderUnlock;
     end;
    end
   else
    begin
     {Return Host Entry if found}
     Result:=HostEntryToHostEnt(HostEntry);
     NetworkSetLastError(ERROR_SUCCESS);
     
     {Unlock Host}
     HostEntry.ReleaseLock;
    end;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetHostByName(AName:PChar):PHostEnt;
{Performs a DNS_TYPE_A or DNS_TYPE_AAAA Query for the Address of the Domain name supplied}
{If Name is blank then get the addresses for the local machine}
{If Name is 'localhost' then get the loopback addresses for the local machine}
var
 Count:Integer;
 Retries:Integer;
 Timeout:Integer;
 Identifier:Word;
 NameBuffer:String;
 SockAddr:TSockAddr;
 Socket:TProtocolSocket;
 Transport:TIPTransport;
 HostEntry:TIPHostEntry;
 AddressEntry:TIPAddressEntry;
 Nameservers:TIPNameservers;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
 
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetHostByName');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS:  Name = ' + AName);
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;
  
  //To Do //IPv6 //AAAA etc 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ)); 
  if Transport = nil then Exit;
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AName = nil then Exit;

   {Check the Name}
   NameBuffer:=StripTrailingDot(AName);
   
   {Check for Blank}
   if (NameBuffer = '') or (Lowercase(NameBuffer) = '..localmachine') or (Lowercase(NameBuffer) = Lowercase(FProtocol.Manager.Settings.HostName)) then
    begin
     {Check the Addresses}
     Count:=0;
     AddressEntry:=Transport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
     while AddressEntry <> nil do
      begin
       {Check Address}
       if AddressEntry.AddressType = ADDRESS_TYPE_PRIMARY then
        begin
         Inc(Count);
         
         {Return Address Entry if found}
         Result:=AddressEntryToHostEnt(AddressEntry,FProtocol.Manager.Settings.HostName + AddLeadingDot(FProtocol.Manager.Settings.DomainName),Count);
         NetworkSetLastError(ERROR_SUCCESS);
        end
       else if AddressEntry.AddressType = ADDRESS_TYPE_SECONDARY then 
        begin
         Inc(Count);
         
         {Return Address Entry if found}
         Result:=AddressEntryToHostEnt(AddressEntry,FProtocol.Manager.Settings.HostName + AddLeadingDot(FProtocol.Manager.Settings.DomainName),Count);
         NetworkSetLastError(ERROR_SUCCESS);
        end;
        
       {Get Next}
       AddressEntry:=Transport.GetAddressByNext(AddressEntry,True,True,NETWORK_LOCK_READ);
      end;
    end
   else if Lowercase(NameBuffer) = 'localhost' then
    begin
     {Check the Addresses}
     Count:=0;
     AddressEntry:=Transport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
     while AddressEntry <> nil do
      begin
       {Check Address}
       if AddressEntry.AddressType = ADDRESS_TYPE_LOOPBACK then
        begin
         Inc(Count);
         
         {Return Address Entry if found}
         Result:=AddressEntryToHostEnt(AddressEntry,'localhost',Count);
         NetworkSetLastError(ERROR_SUCCESS);
        end;
     
       {Get Next}
       AddressEntry:=Transport.GetAddressByNext(AddressEntry,True,True,NETWORK_LOCK_READ);
      end;
    end
   else
    begin
     {Check the Domain}
     if Pos('.',NameBuffer) = 0 then NameBuffer:=NameBuffer + AddLeadingDot(FProtocol.Manager.Settings.DomainName);

     {Check the Cache}
     HostEntry:=Transport.GetHostByName(NameBuffer,True); 
     if HostEntry = nil then
      begin
       {Create a Socket}
       Socket:=FProtocol.Socket(AF_INET,FProtocol.SocketType,FProtocol.Protocol);
       if Socket = nil then Exit;
     
       {Lock Socket}
       Socket.ReaderLock;
       try
        {Bind the Socket}
        SockAddr.sin_family:=AF_INET;
        SockAddr.sin_port:=WordNtoBE(IPPORT_ANY);
        SockAddr.sin_addr.S_addr:=INADDR_ANY;
        if FProtocol.Bind(Socket,SockAddr,SizeOf(TSockAddr)) = SOCKET_ERROR then Exit;
      
        {Set the Timeout}
        Timeout:=DNS_TIMEOUT;
        if FProtocol.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) = SOCKET_ERROR then Exit;
      
        {Get the Nameservers}
        Nameservers:=Transport.Nameservers; //To Do //GetNameserverByNext etc
        for Count:=0 to MAX_NAME_SERVERS - 1 do
         begin
          if not Transport.CompareDefault(Nameservers[Count]) then
           begin
            Retries:=0;
            while Retries < DNS_RETRIES do
             begin
              {Get Identifier}
              Identifier:=Random($7FFF) + 1;
            
              {Send Request to each Nameserver}
              if SendDNSQuery(Socket,Nameservers[Count],PChar(NameBuffer),Length(NameBuffer),DNS_TYPE_A,DNS_CLASS_IN,Identifier) then
               begin
                {Receive Response}
                if RecvDNSResponse(Socket,DNS_TYPE_A,DNS_CLASS_IN,Identifier) then
                 begin
                  {Check the Cache}
                  HostEntry:=Transport.GetHostByName(NameBuffer,True); 
                  if HostEntry <> nil then
                   begin
                    {Return Host Entry if found}
                    Result:=HostEntryToHostEnt(HostEntry);
                    NetworkSetLastError(ERROR_SUCCESS);
                    
                    {Unlock Host}
                    HostEntry.ReleaseLock;
                    Exit;
                   end;
                 end;
               end;
              
              Inc(Retries);
             end;
           end;
         end;
       finally
        {Close the Socket}
        FProtocol.CloseSocket(Socket);
      
        {Unlock Socket}
        Socket.ReaderUnlock;
       end;
      end
     else
      begin
       {Return Host Entry if found}
       Result:=HostEntryToHostEnt(HostEntry);
       NetworkSetLastError(ERROR_SUCCESS);
     
       {Unlock Host}
       HostEntry.ReleaseLock;
      end;
    end;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetHostName(AName:PChar;ALength:Integer):Integer;
var
 Transport:TIPTransport;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetHostName');
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;
  
  //To Do //IPv6 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try
   {Get the Host Name}
   NetworkSetLastError(WSAEFAULT);
   if AName = nil then Exit;
   if ALength < Length(FProtocol.Manager.Settings.HostName) then Exit;
   StrLCopy(AName,PChar(FProtocol.Manager.Settings.HostName),ALength);
  
   {Return Result}
   Result:=NO_ERROR;
   NetworkSetLastError(ERROR_SUCCESS);
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetServByPort(APort:Integer;AProto:PChar):PServEnt;
{Note: Port will be in network order}
var
 Transport:TIPTransport;
 ServEntry:TIPServEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetServByPort');
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  //To Do //IPv6 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AProto = nil then Exit;
  
   {Get the Service}
   NetworkSetLastError(WSANO_DATA);
   ServEntry:=Transport.GetServByPort(WordBEtoN(APort),AProto,True);
   if ServEntry = nil then Exit;
  
   {Return Service Entry if found}
   Result:=ServEntryToServEnt(ServEntry);
   NetworkSetLastError(ERROR_SUCCESS);
  
   {Unlock Service}
   ServEntry.ReleaseLock;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetServByName(AName,AProto:PChar):PServEnt;
var
 Transport:TIPTransport;
 ServEntry:TIPServEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetServByName');
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  //To Do //IPv6 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ)); 
  if Transport = nil then Exit;
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AName = nil then Exit;
   if AProto = nil then Exit;
  
   {Get the Service}
   NetworkSetLastError(WSANO_DATA);
   ServEntry:=Transport.GetServByName(AName,AProto,True); 
   if ServEntry = nil then Exit;
  
   {Return Service Entry if found}
   Result:=ServEntryToServEnt(ServEntry);
   NetworkSetLastError(ERROR_SUCCESS);
  
   {Unlock Service}
   ServEntry.ReleaseLock;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetProtoByNumber(AProto:Integer):PProtoEnt;
{Note: Protocol will be in host order}
var
 Transport:TIPTransport;
 ProtoEntry:TIPProtoEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetProtoByNumber');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;
  
  //To Do //IPv6 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try
   {Get the Protocol}
   NetworkSetLastError(WSANO_DATA);
   ProtoEntry:=Transport.GetProtoByNumber(AProto,True); 
   if ProtoEntry = nil then Exit;
  
   {Return Protocol Entry if found}
   Result:=ProtoEntryToProtoEnt(ProtoEntry);
   NetworkSetLastError(ERROR_SUCCESS);

   {Unlock Protocol}
   ProtoEntry.ReleaseLock;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetProtoByName(AName:PChar):PProtoEnt;
var
 Transport:TIPTransport;
 ProtoEntry:TIPProtoEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetProtoByName');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  //To Do //IPv6 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try 
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AName = nil then Exit;

   {Get the Protocol}
   NetworkSetLastError(WSANO_DATA);
   ProtoEntry:=Transport.GetProtoByName(AName,True);
   if ProtoEntry = nil then Exit;
  
   {Return Protocol Entry if found}
   Result:=ProtoEntryToProtoEnt(ProtoEntry);
   NetworkSetLastError(ERROR_SUCCESS);
   
   {Unlock Protocol}
   ProtoEntry.ReleaseLock;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetNetByAddr(AAddr:Pointer;ALength,AStruct:Integer):PNetEnt;
{Note: Address will be in network order where applicable}
var
 Transport:TIPTransport;
 NetworkEntry:TIPNetworkEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetNetByAddr');
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;
  
  //To Do //IPv6 //Check Struct etc
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AAddr = nil then Exit;
   if AStruct <> AF_INET then Exit;
   if ALength < SizeOf(TInAddr) then Exit;
  
   {Get the Network}
   NetworkSetLastError(WSANO_DATA);
   NetworkEntry:=Transport.GetNetworkByAddress(InAddrToHost(TInAddr(AAddr^)),True);
   if NetworkEntry = nil then Exit;
  
   {Return Network Entry if found}
   Result:=NetworkEntryToNetEnt(NetworkEntry);
   NetworkSetLastError(ERROR_SUCCESS);
   
   {Unlock Network}
   NetworkEntry.ReleaseLock;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.GetNetByName(AName:PChar):PNetEnt;
var
 Transport:TIPTransport;
 NetworkEntry:TIPNetworkEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;
  
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetNetByName');
  {$ENDIF}
  
  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;
  
  //To Do //IPv6 
  
  {Get the Transport}
  Transport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AName = nil then Exit;
  
   {Get the Network}
   NetworkSetLastError(WSANO_DATA);
   NetworkEntry:=Transport.GetNetworkByName(AName,True); 
   if NetworkEntry = nil then Exit;
  
   {Return Network Entry if found}
   Result:=NetworkEntryToNetEnt(NetworkEntry);
   NetworkSetLastError(ERROR_SUCCESS);
  
   {Unlock Network}
   NetworkEntry.ReleaseLock;
  finally
   Transport.ReaderUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.StartClient:Boolean; 
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  //To Do
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDNSClient.StopClient:Boolean; 
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  //To Do
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DNSInit;
begin
 {}
 {Check Initialized}
 if DNSInitialized then Exit;

 {Create DNS Client}
 if NetworkSettings.GetBooleanDefault('DNS_CLIENT_ENABLED',DNS_CLIENT_ENABLED) then
  begin
   DNSClient:=TDNSClient.Create(ProtocolManager.GetProtocolByType(IPPROTO_UDP,SOCK_DGRAM,False,NETWORK_LOCK_NONE)); //To Do //Pass Manager //Move to StartClient
  end; 
 
 DNSInitialized:=True;
end;

{==============================================================================}

function DNSStart:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if DNSStarted then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Client}
 if DNSClient = nil then Exit;
 
 //To Do //Remove this, move to DNSClient.StartClient etc
 
 {Set Started} 
 DNSStarted:=True;
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function DNSStop:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(DNSStarted) then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Client}
 if DNSClient = nil then Exit;
 
 //To Do //Remove this, move to DNSClient.StartClient etc

 {Set Started}
 DNSStarted:=False;    
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{DNS Functions}
  
{==============================================================================}
{==============================================================================}
{DNS Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 DNSInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 