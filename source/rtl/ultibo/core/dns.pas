{
Ultibo DNS client unit.

Copyright (C) 2025 - SoftOz Pty Ltd.

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

//To Do //IPv6 support

//To Do //Look for:

//Remove

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {DNS specific constants}
 {DNS Constants}
 DNS_TIMEOUT = 2000;  {We wait for 2 seconds for a DNS reply}
 DNS_RETRIES = 2;     {Try the request 2 times}

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
 DNS_OPCODE_QUERY         = 0;  // a standard query
 DNS_OPCODE_IQUERY        = 1;  // an inverse query
 DNS_OPCODE_SERVER_STATUS = 2;
 DNS_OPCODE_UNKNOWN       = 3;
 DNS_OPCODE_NOTIFY        = 4;
 DNS_OPCODE_UPDATE        = 5;
 
 {DNS Response Codes}
 DNS_RESPONSE_NO_ERROR        = 0;
 DNS_RESPONSE_FORMAT_ERROR    = 1;
 DNS_RESPONSE_SERVER_FAILURE  = 2;
 DNS_RESPONSE_NAME_ERROR      = 3;
 DNS_RESPONSE_NOT_IMPLEMENTED = 4;
 DNS_RESPONSE_REFUSED         = 5;

 {DNS Record Types}
 DNS_TYPE_A       = $0001;  // host address resource record (RR)
 DNS_TYPE_NS      = $0002;
 DNS_TYPE_MD      = $0003;
 DNS_TYPE_MF      = $0004;
 DNS_TYPE_CNAME   = $0005;
 DNS_TYPE_SOA     = $0006;
 DNS_TYPE_MB      = $0007;
 DNS_TYPE_MG      = $0008;
 DNS_TYPE_MR      = $0009;
 DNS_TYPE_NULL    = $000a;
 DNS_TYPE_WKS     = $000b;
 DNS_TYPE_PTR     = $000c;  // a domain name ptr
 DNS_TYPE_HINFO   = $000d;
 DNS_TYPE_MINFO   = $000e;
 DNS_TYPE_MX      = $000f;  // mail exchange
 DNS_TYPE_TEXT    = $0010;
 DNS_TYPE_RP      = $0011;
 DNS_TYPE_AFSDB   = $0012;
 DNS_TYPE_X25     = $0013;
 DNS_TYPE_ISDN    = $0014;
 DNS_TYPE_RT      = $0015;
 DNS_TYPE_NSAP    = $0016;
 DNS_TYPE_NSAPPTR = $0017;
 DNS_TYPE_SIG     = $0018;
 DNS_TYPE_KEY     = $0019;
 DNS_TYPE_PX      = $001a;
 DNS_TYPE_GPOS    = $001b;
 DNS_TYPE_AAAA    = $001c;
 DNS_TYPE_LOC     = $001d;
 DNS_TYPE_NXT     = $001e;
 DNS_TYPE_EID     = $001f;
 DNS_TYPE_NIMLOC  = $0020;
 DNS_TYPE_SRV     = $0021;
 DNS_TYPE_ATMA    = $0022;
 DNS_TYPE_NAPTR   = $0023;
 DNS_TYPE_KX      = $0024;
 DNS_TYPE_CERT    = $0025;
 DNS_TYPE_A6      = $0026;
 DNS_TYPE_DNAME   = $0027;
 DNS_TYPE_SINK    = $0028;
 DNS_TYPE_OPT     = $0029;
 DNS_TYPE_DS      = $002B;
 DNS_TYPE_RRSIG   = $002E;
 DNS_TYPE_NSEC    = $002F;
 DNS_TYPE_DNSKEY  = $0030;
 DNS_TYPE_DHCID   = $0031;
 DNS_TYPE_UINFO   = $0064;
 DNS_TYPE_UID     = $0065;
 DNS_TYPE_GID     = $0066;
 DNS_TYPE_UNSPEC  = $0067;
 DNS_TYPE_ADDRS   = $00f8;
 DNS_TYPE_TKEY    = $00f9;
 DNS_TYPE_TSIG    = $00fa;
 DNS_TYPE_IXFR    = $00fb;
 DNS_TYPE_AXFR    = $00fc;
 DNS_TYPE_MAILB   = $00fd;
 DNS_TYPE_MAILA   = $00fe;
 DNS_TYPE_ALL     = $00ff;
 DNS_TYPE_ANY     = $00ff;
 DNS_TYPE_WINS    = $ff01;
 DNS_TYPE_WINSR   = $ff02;
 DNS_TYPE_NBSTAT  = DNS_TYPE_WINSR;

 {DNS Address Classes}
 DNS_CLASS_INTERNET = $0001;
 DNS_CLASS_CSNET    = $0002;
 DNS_CLASS_CHAOS    = $0003;
 DNS_CLASS_HESIOD   = $0004;
 DNS_CLASS_NONE     = $00fe;
 DNS_CLASS_ALL      = $00ff;
 DNS_CLASS_ANY      = $00ff;
 DNS_CLASS_IN       = DNS_CLASS_INTERNET;  // ARPA internet class
 DNS_CLASS_CS       = DNS_CLASS_CSNET;
 DNS_CLASS_WILD     = DNS_CLASS_ANY;       // wildcard for several of the classifications
 
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
 
 PDNSClientName = ^TDNSClientName;
 TDNSClientName = array[0..MAX_NAME_SIZE - 1] of Char; 
 
 PDNSClientData = ^TDNSClientData;
 TDNSClientData = record {Used for TLS Data}
  {Host Ent}
  HostEnt:THostEnt;                  
  HostEntName:TDNSClientName;
  HostAliasesPtr:array[0..MAX_NAME_ALIASES] of PChar;            // One extra for terminating null pointer
  HostAliases:array[0..MAX_NAME_ALIASES - 1] of TDNSClientName;
  HostAddrListPtr:array[0..MAX_HOST_ALIASES] of PChar;           // One extra for terminating null pointer
  HostAddrList:array[0..MAX_HOST_ALIASES - 1] of TInAddr;
  HostAddr6ListPtr:array[0..MAX_HOST_ALIASES] of PChar;          // One extra for terminating null pointer
  HostAddr6List:array[0..MAX_HOST_ALIASES - 1] of TIn6Addr;
  {Net Ent}
  NetEnt:TNetEnt;
  NetEntName:TDNSClientName;
  NetAliasesPtr:array[0..MAX_NAME_ALIASES] of PChar;             // One extra for terminating null pointer
  NetAliases:array[0..MAX_NAME_ALIASES - 1] of TDNSClientName;
  {Serv Ent}
  ServEnt:TServEnt;
  ServEntName:TDNSClientName;
  ServAliasesPtr:array[0..MAX_NAME_ALIASES] of PChar;            // One extra for terminating null pointer
  ServAliases:array[0..MAX_NAME_ALIASES - 1] of TDNSClientName;
  ServEntProto:TDNSClientName;
  {Proto Ent}
  ProtoEnt:TProtoEnt;
  ProtoEntName:TDNSClientName;
  ProtoAliasesPtr:array[0..MAX_NAME_ALIASES] of PChar;           // One extra for terminating null pointer
  ProtoAliases:array[0..MAX_NAME_ALIASES - 1] of TDNSClientName;
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

   function GetLastAddrInfo(AAddrInfo:PAddrInfo):PAddrInfo;

   function AddressEntryToHostEnt(AddressEntry:TAddressEntry;const AName:String;ACount:Integer):PHostEnt;
   function HostEntryToHostEnt(HostEntry:THostEntry):PHostEnt;
   function NetworkEntryToNetEnt(NetworkEntry:TNetworkEntry):PNetEnt;
   function ServEntryToServEnt(ServEntry:TServEntry):PServEnt;
   function ProtoEntryToProtoEnt(ProtoEntry:TProtoEntry):PProtoEnt;
   
   function AddressEntryToAddrInfo(AddressEntry:TAddressEntry;AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word;AFirst:Boolean):PAddrInfo;
   function HostEntryToAddrInfo(HostEntry:THostEntry;AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word;AFirst:Boolean):PAddrInfo;

   function ResolveLocalAddrInfo(AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word):PAddrInfo;
   function ResolveLoopbackAddrInfo(AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word):PAddrInfo;

   function ResolveHostByName(const AName:String;AFamily:Integer):THostEntry;
   function ResolveHostByAddress(AAddress:Pointer;ALength,AFamily:Integer):THostEntry;

   function ResolveHostName:String;

   function ResolveServByName(const AName,AProto:String):TServEntry;
   function ResolveServByPort(APort:Integer;const AProto:String):TServEntry;
   function ResolveProtoByName(const AName:String):TProtoEntry;
   function ResolveProtoByNumber(AProto:Integer):TProtoEntry;

   function ResolveNetworkByName(const AName:String;AFamily:Integer):TNetworkEntry;
   function ResolveNetworkByAddress(AAddress:Pointer;ALength,AFamily:Integer):TNetworkEntry;

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
   function HandleDNSResponse(AMessage:PDNSMessage;AFamily,AIdentifier:Word):Boolean;

   function PerformDNSRequest(AData:Pointer;ALength,AFamily,AType,AClass:Word):Boolean;
   
   function SendDNSQuery(ASocket:TProtocolSocket;AServer:PSockAddr;AServerLength:Integer;AData:Pointer;ALength,AFamily,AType,AClass,AIdentifier:Word):Boolean;
   function RecvDNSResponse(ASocket:TProtocolSocket;AFamily,AType,AClass,AIdentifier:Word):Boolean;

   function InsertDNSName(AMessage:PDNSMessage;AOffset:Word;AName:Pointer;ALength:Word):Boolean;
   function ExtractDNSName(AMessage:PDNSMessage;AOffset:Word;AName:Pointer;var ALength:Word):Boolean;
   function ExtractDNSRData(AMessage:PDNSMessage;AOffset:Word;AData:Pointer;var ALength:Word;AType,AClass:Word):Boolean;

   function InsertDNSQuestion(AMessage:PDNSMessage;ACount:Word;AData:Pointer;ALength,AFamily,AType,AClass:Word):Boolean;
   function ExtractDNSAnswer(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength:Word;AFamily:Word;var AType,AClass:Word;var ATtl:LongWord):Boolean;
   function ExtractDNSAuthority(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength:Word;AFamily:Word;var AType,AClass:Word;var ATtl:LongWord):Boolean;
   function ExtractDNSAdditional(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength:Word;AFamily:Word;var AType,AClass:Word;var ATtl:LongWord):Boolean;
  public
   {Status Variables}

   {BSD Database Methods}
   function GetHostByAddr(AAddr:Pointer;ALength,AFamily:Integer):PHostEnt;
   function GetHostByName(AName:PChar;AFamily:Integer):PHostEnt;

   function GetHostName(AName:PChar;ALength:Integer):Integer;

   function GetServByPort(APort:Integer;AProto:PChar):PServEnt;
   function GetServByName(AName,AProto:PChar):PServEnt;
   function GetProtoByNumber(AProto:Integer):PProtoEnt;
   function GetProtoByName(AName:PChar):PProtoEnt;

   function GetNetByAddr(AAddr:Pointer;ALength,AFamily:Integer):PNetEnt;
   function GetNetByName(AName:PChar;AFamily:Integer):PNetEnt;

   {RFC 3493 Methods}
   function GetAddrInfo(ANodeName,AServName:PChar;AHints:PAddrInfo;var AAddrInfo:PAddrInfo):Integer;
   function GetNameInfo(AAddr:PSockAddr;AAddrLength:Integer;AHost:PChar;AHostLength:Integer;AServ:PChar;AServLength:Integer;AFlags:LongWord):Integer;
   procedure FreeAddrInfo(AAddrInfo:PAddrInfo);

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
 Count:Integer;
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
 FillChar(ClientData.HostEntName,SizeOf(TDNSClientName),0);
 FillChar(ClientData.HostAliases,MAX_NAME_ALIASES * SizeOf(TDNSClientName),0);
 FillChar(ClientData.HostAddrList,MAX_HOST_ALIASES * SizeOf(TInAddr),0);
 FillChar(ClientData.HostAddr6List,MAX_HOST_ALIASES * SizeOf(TIn6Addr),0);
 for Count:=0 to MAX_NAME_ALIASES - 1 do
  begin
   ClientData.HostAliasesPtr[Count]:=@ClientData.HostAliases[Count];
  end;
 ClientData.HostAliasesPtr[MAX_NAME_ALIASES]:=nil;
 for Count:=0 to MAX_HOST_ALIASES - 1 do
  begin
   ClientData.HostAddrListPtr[Count]:=@ClientData.HostAddrList[Count];
   ClientData.HostAddr6ListPtr[Count]:=@ClientData.HostAddr6List[Count];
  end; 
 ClientData.HostAddrListPtr[MAX_HOST_ALIASES]:=nil;
 ClientData.HostAddr6ListPtr[MAX_HOST_ALIASES]:=nil;
 ClientData.HostEnt.h_name:=@ClientData.HostEntName;
 ClientData.HostEnt.h_aliases:=@ClientData.HostAliasesPtr;
 ClientData.HostEnt.h_addr_list:=@ClientData.HostAddrListPtr;
 {Net Ent}
 FillChar(ClientData.NetEnt,SizeOf(TNetEnt),0);
 FillChar(ClientData.NetEntName,SizeOf(TDNSClientName),0);
 FillChar(ClientData.NetAliases,MAX_NAME_ALIASES * SizeOf(TDNSClientName),0);
 for Count:=0 to MAX_NAME_ALIASES - 1 do
  begin
   ClientData.NetAliasesPtr[Count]:=@ClientData.NetAliases[Count];
  end;
 ClientData.NetAliasesPtr[MAX_NAME_ALIASES]:=nil;
 ClientData.NetEnt.n_name:=@ClientData.NetEntName;
 ClientData.NetEnt.n_aliases:=@ClientData.NetAliasesPtr;
 {Serv Ent}
 FillChar(ClientData.ServEnt,SizeOf(TServEnt),0);
 FillChar(ClientData.ServEntName,SizeOf(TDNSClientName),0);
 FillChar(ClientData.ServAliases,MAX_NAME_ALIASES * SizeOf(TDNSClientName),0);
 FillChar(ClientData.ServEntProto,SizeOf(TDNSClientName),0);
 for Count:=0 to MAX_NAME_ALIASES - 1 do
  begin
   ClientData.ServAliasesPtr[Count]:=@ClientData.ServAliases[Count];
  end;
 ClientData.ServAliasesPtr[MAX_NAME_ALIASES]:=nil;
 ClientData.ServEnt.s_name:=@ClientData.ServEntName;
 ClientData.ServEnt.s_aliases:=@ClientData.ServAliasesPtr;
 ClientData.ServEnt.s_proto:=@ClientData.ServEntProto;
 {Proto Ent}
 FillChar(ClientData.ProtoEnt,SizeOf(TProtoEnt),0);
 FillChar(ClientData.ProtoEntName,SizeOf(TDNSClientName),0);
 FillChar(ClientData.ProtoAliases,MAX_NAME_ALIASES * SizeOf(TDNSClientName),0);
 for Count:=0 to MAX_NAME_ALIASES - 1 do
  begin
   ClientData.ProtoAliasesPtr[Count]:=@ClientData.ProtoAliases[Count];
  end;
 ClientData.ProtoAliasesPtr[MAX_NAME_ALIASES]:=nil;
 ClientData.ProtoEnt.p_name:=@ClientData.ProtoEntName;
 ClientData.ProtoEnt.p_aliases:=@ClientData.ProtoAliasesPtr;

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.GetLastAddrInfo(AAddrInfo:PAddrInfo):PAddrInfo;
begin
 {}
 Result:=AAddrInfo;
 
 if Result = nil then Exit;
 
 while Result.ai_next <> nil do
  begin
   Result:=Result.ai_next;
  end;
end;

{==============================================================================}

function TDNSClient.AddressEntryToHostEnt(AddressEntry:TAddressEntry;const AName:String;ACount:Integer):PHostEnt;
{Note: Caller must hold Address entry lock}
var
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;

 {Check Address Entry}
 if AddressEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: AddressEntryToHostEnt (Type = ' + IntToStr(AddressEntry.AddressType) + ' Family = ' + IntToStr(AddressEntry.Family) + ' Name = ' + AName + ' Count = ' + IntToStr(ACount) + ')');
 {$ENDIF}
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Check Address Type}
 case AddressEntry.AddressType of
  ADDRESS_TYPE_PRIMARY,
  ADDRESS_TYPE_SECONDARY,
  ADDRESS_TYPE_LOOPBACK:begin
    {Check Count}
    if ACount > MAX_HOST_ALIASES then Exit;

    if ACount = 1 then
     begin
      {Create Host Ent}
      ClientData.HostEnt.h_addrtype:=AddressEntry.Family;
      ClientData.HostEnt.h_length:=AddressEntry.Length;
      StrLCopy(ClientData.HostEntName,PChar(AName),MAX_NAME_SIZE);
      ClientData.HostAliasesPtr[0]:=nil;

      case AddressEntry.Family of
       AF_INET:begin
         {IPv4}
         ClientData.HostAddrList[ACount - 1]:=InAddrToNetwork(TIPAddressEntry(AddressEntry).Address);
         ClientData.HostAddrListPtr[ACount - 1]:=@ClientData.HostAddrList[ACount - 1];
         ClientData.HostAddrListPtr[ACount]:=nil;

         ClientData.HostEnt.h_addr_list:=@ClientData.HostAddrListPtr;
        end;
       AF_INET6:begin 
         {IPv6}
         ClientData.HostAddr6List[ACount - 1]:=TIP6AddressEntry(AddressEntry).Address;
         ClientData.HostAddr6ListPtr[ACount - 1]:=@ClientData.HostAddr6List[ACount - 1];
         ClientData.HostAddr6ListPtr[ACount]:=nil;

         ClientData.HostEnt.h_addr_list:=@ClientData.HostAddr6ListPtr;
        end;
      end;

      {Return Result}
      Result:=@ClientData.HostEnt;
     end
    else
     begin
      {Update Host Ent}
      case AddressEntry.Family of
       AF_INET:begin
         {IPv4}
         ClientData.HostAddrList[ACount - 1]:=InAddrToNetwork(TIPAddressEntry(AddressEntry).Address);
         ClientData.HostAddrListPtr[ACount - 1]:=@ClientData.HostAddrList[ACount - 1];
         ClientData.HostAddrListPtr[ACount]:=nil;

         ClientData.HostEnt.h_addr_list:=@ClientData.HostAddrListPtr;
        end;
       AF_INET6:begin 
         {IPv6}
         ClientData.HostAddr6List[ACount - 1]:=TIP6AddressEntry(AddressEntry).Address;
         ClientData.HostAddr6ListPtr[ACount - 1]:=@ClientData.HostAddr6List[ACount - 1];
         ClientData.HostAddr6ListPtr[ACount]:=nil;

         ClientData.HostEnt.h_addr_list:=@ClientData.HostAddr6ListPtr;
        end;
      end;
      
      {Return Result}
      Result:=@ClientData.HostEnt;
     end;     
   end;
 end;
end;

{==============================================================================}

function TDNSClient.HostEntryToHostEnt(HostEntry:THostEntry):PHostEnt;
{Note: Caller must hold Host entry lock}
var
 Alias:String;
 Index:Integer;
 Count:Integer;
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;

 {Check Host Entry}
 if HostEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: HostEntryToHostEnt (Name = ' + HostEntry.Name  + ')');
 {$ENDIF}
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Host Ent}
 ClientData.HostEnt.h_addrtype:=HostEntry.Family;
 ClientData.HostEnt.h_length:=HostEntry.Length;
 StrLCopy(ClientData.HostEntName,PChar(HostEntry.Name),MAX_NAME_SIZE);

 Count:=0;
 Alias:=HostEntry.GetAlias(Count);
 while Length(Alias) <> 0 do
  begin
   StrLCopy(ClientData.HostAliases[Count],PChar(Alias),MAX_NAME_SIZE);
   ClientData.HostAliasesPtr[Count]:=@ClientData.HostAliases[Count];

   Inc(Count);
   if Count = MAX_NAME_ALIASES then Break;

   Alias:=HostEntry.GetAlias(Count)
  end;
 ClientData.HostAliasesPtr[Count]:=nil;

 case HostEntry.Family of
  AF_INET:begin
    {IPv4}
    Count:=0;
    for Index:=0 to MAX_HOST_ALIASES - 1 do
     begin
      if not InAddrIsDefault(TIPHostEntry(HostEntry).Addresses[Index]) then
       begin
        ClientData.HostAddrList[Count]:=InAddrToNetwork(TIPHostEntry(HostEntry).Addresses[Index]);
        ClientData.HostAddrListPtr[Count]:=@ClientData.HostAddrList[Count];
       
        Inc(Count);
       end;
     end;
    ClientData.HostAddrListPtr[Count]:=nil;
    ClientData.HostEnt.h_addr_list:=@ClientData.HostAddrListPtr;
   end;
  AF_INET6:begin 
    {IPv6}
    Count:=0;
    for Index:=0 to MAX_HOST_ALIASES - 1 do
     begin
      if not In6AddrIsDefault(TIP6HostEntry(HostEntry).Addresses[Index]) then
       begin
        ClientData.HostAddr6List[Count]:=TIP6HostEntry(HostEntry).Addresses[Count];
        ClientData.HostAddr6ListPtr[Count]:=@ClientData.HostAddr6List[Count];
        
        Inc(Count);
       end;
     end;
    ClientData.HostAddr6ListPtr[Count]:=nil;
    ClientData.HostEnt.h_addr_list:=@ClientData.HostAddr6ListPtr;
   end;
 end;

 {Return Result}
 Result:=@ClientData.HostEnt;
end;

{==============================================================================}

function TDNSClient.NetworkEntryToNetEnt(NetworkEntry:TNetworkEntry):PNetEnt;
{Note: Caller must hold Network entry lock}
var
 Alias:String;
 Count:Integer;
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;

 {Check Network Entry}
 if NetworkEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: NetworkEntryToNetEnt (Name = ' + NetworkEntry.Name  + ')');
 {$ENDIF}
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Net Ent}
 ClientData.NetEnt.n_addrtype:=NetworkEntry.Family;
 StrLCopy(ClientData.NetEntName,PChar(NetworkEntry.Name),MAX_NAME_SIZE);

 Count:=0;
 Alias:=NetworkEntry.GetAlias(Count);
 while Length(Alias) <> 0 do
  begin
   StrLCopy(ClientData.NetAliases[Count],PChar(Alias),MAX_NAME_SIZE);
   ClientData.NetAliasesPtr[Count]:=@ClientData.NetAliases[Count];

   Inc(Count);
   if Count = MAX_NAME_ALIASES then Break;

   Alias:=NetworkEntry.GetAlias(Count)
  end;
 ClientData.NetAliasesPtr[Count]:=nil;

 case NetworkEntry.Family of
  AF_INET:begin
    {IPv4}
    ClientData.NetEnt.n_net:=LongInt(InAddrToNetwork(TIPNetworkEntry(NetworkEntry).Network));
   end;
  AF_INET6:begin 
    {IPv6}
    {Not Supported}
   end;
 end;

 {Return Result}
 Result:=@ClientData.NetEnt;
end;

{==============================================================================}

function TDNSClient.ServEntryToServEnt(ServEntry:TServEntry):PServEnt;
{Note: Caller must hold Serv entry lock}
var
 Alias:String;
 Count:Integer;
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;

 {Check Serv Entry}
 if ServEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ServEntryToServEnt (Name = ' + ServEntry.Name  + ')');
 {$ENDIF}
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Serv Ent}
 ClientData.ServEnt.s_port:=WordNtoBE(ServEntry.Port);
 StrLCopy(ClientData.ServEntName,PChar(ServEntry.Name),MAX_NAME_SIZE);

 Count:=0;
 Alias:=ServEntry.GetAlias(Count);
 while Length(Alias) <> 0 do
  begin
   StrLCopy(ClientData.ServAliases[Count],PChar(Alias),MAX_NAME_SIZE);
   ClientData.ServAliasesPtr[Count]:=@ClientData.ServAliases[Count];

   Inc(Count);
   if Count = MAX_NAME_ALIASES then Break;

   Alias:=ServEntry.GetAlias(Count)
  end;
 ClientData.ServAliasesPtr[Count]:=nil;

 StrLCopy(ClientData.ServEntProto,PChar(ServEntry.Protocol),MAX_NAME_SIZE);

 {Return Result}
 Result:=@ClientData.ServEnt;
end;

{==============================================================================}

function TDNSClient.ProtoEntryToProtoEnt(ProtoEntry:TProtoEntry):PProtoEnt;
{Note: Caller must hold Proto entry lock}
var
 Alias:String;
 Count:Integer;
 ClientData:PDNSClientData;
begin
 {}
 Result:=nil;

 {Check Proto Entry}
 if ProtoEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ProtoEntryToProtoEnt (Name = ' + ProtoEntry.Name  + ')');
 {$ENDIF}
 
 {Get Client Data}
 ClientData:=GetClientData;
 if ClientData = nil then Exit;
 
 {Create Proto Ent}
 ClientData.ProtoEnt.p_proto:=ProtoEntry.Number;
 StrLCopy(ClientData.ProtoEntName,PChar(ProtoEntry.Name),MAX_NAME_SIZE);

 Count:=0;
 Alias:=ProtoEntry.GetAlias(Count);
 while Length(Alias) <> 0 do
  begin
   StrLCopy(ClientData.ProtoAliases[Count],PChar(Alias),MAX_NAME_SIZE);
   ClientData.ProtoAliasesPtr[Count]:=@ClientData.ProtoAliases[Count];

   Inc(Count);
   if Count = MAX_NAME_ALIASES then Break;

   Alias:=ProtoEntry.GetAlias(Count)
  end;
 ClientData.ProtoAliasesPtr[Count]:=nil;

 {Return Result}
 Result:=@ClientData.ProtoEnt;
end;

{==============================================================================}

function TDNSClient.AddressEntryToAddrInfo(AddressEntry:TAddressEntry;AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word;AFirst:Boolean):PAddrInfo;
{Note: Caller must hold Address entry lock}
var
 Name:String;
 AddrInfo:PAddrInfo;
begin
 {}
 Result:=nil;

 {Check Address Entry}
 if AddressEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: AddressEntryToAddrInfo (Type = ' + IntToStr(AddressEntry.AddressType) + ' Family = ' + IntToStr(AddressEntry.Family) + ' Flags = ' + IntToHex(AFlags,8) + ' Family = ' + IntToStr(AFamily) + ' Protocol = ' + IntToStr(AProtocol) + ' SocketType = ' + IntToStr(ASocketType) + ' Port = ' + IntToStr(APort) + ')');
 {$ENDIF}

 {Set Defaults}
 AddrInfo:=nil;
 try
  {Create Address Info}
  AddrInfo:=AllocMem(SizeOf(TAddrInfo));
  if AddrInfo = nil then Exit;

  {Update Address Info}
  AddrInfo.ai_flags:=AFlags;
  AddrInfo.ai_family:=AFamily;
  AddrInfo.ai_socktype:=ASocketType;
  AddrInfo.ai_protocol:=AProtocol;
  case AddressEntry.Family of
   AF_INET:AddrInfo.ai_addrlen:=SizeOf(TSockAddr);
   AF_INET6:AddrInfo.ai_addrlen:=SizeOf(TSockAddr6);
  end;

  {Create Sock Address}
  AddrInfo.ai_addr:=AllocMem(AddrInfo.ai_addrlen);
  if AddrInfo.ai_addr = nil then Exit;

  {Return Sock Address}
  case AddressEntry.Family of
   AF_INET:begin
     AddrInfo.ai_addr.sin_family:=AFamily;
     AddrInfo.ai_addr.sin_port:=WordNtoBE(APort);
     AddrInfo.ai_addr.sin_addr:=InAddrToNetwork(TIPAddressEntry(AddressEntry).Address);
    end;
   AF_INET6:begin
     PSockAddr6(AddrInfo.ai_addr).sin6_family:=AFamily;
     PSockAddr6(AddrInfo.ai_addr).sin6_port:=WordNtoBE(APort);
     PSockAddr6(AddrInfo.ai_addr).sin6_addr:=TIP6AddressEntry(AddressEntry).Address;
    end; 
  end;

  {Check Canonical Name}
  if AFirst and ((AFlags and AI_CANONNAME) = AI_CANONNAME) then
   begin
    {Get Name}
    Name:=FProtocol.Manager.Settings.HostName + AddLeadingDot(FProtocol.Manager.Settings.DomainName);

    {Create Name}
    AddrInfo.ai_canonname:=AllocMem((Length(Name) + 1) * SizeOf(Char));
    if AddrInfo.ai_canonname = nil then Exit;

    StrLCopy(AddrInfo.ai_canonname,PChar(Name),Length(Name));
   end;

  {Return Result}
  Result:=AddrInfo;
 finally
  if Result = nil then FreeAddrInfo(AddrInfo);
 end; 
end;

{==============================================================================}

function TDNSClient.HostEntryToAddrInfo(HostEntry:THostEntry;AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word;AFirst:Boolean):PAddrInfo;
{Note: Caller must hold Host entry lock}
var
 Index:Integer;
 NextInfo:PAddrInfo;
 LastInfo:PAddrInfo;
begin
 {}
 Result:=nil;

 {Check Host Entry}
 if HostEntry = nil then Exit;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: HostEntryToAddrInfo (Name = ' + HostEntry.Name + ' Flags = ' + IntToHex(AFlags,8) + ' Family = ' + IntToStr(AFamily) + ' Protocol = ' + IntToStr(AProtocol) + ' SocketType = ' + IntToStr(ASocketType) + ' Port = ' + IntToStr(APort) + ')');
 {$ENDIF}

 {Set Defaults}
 NextInfo:=nil;
 LastInfo:=nil;
 try
  case HostEntry.Family of
   AF_INET:begin
     {IPv4}
     for Index:=0 to MAX_HOST_ALIASES - 1 do
      begin
       {Check Address}
       if not InAddrIsDefault(TIPHostEntry(HostEntry).Addresses[Index]) then
        begin
         {Create Address Info}
         NextInfo:=AllocMem(SizeOf(TAddrInfo));
         if NextInfo = nil then Exit;

         {Update Address Info}
         NextInfo.ai_flags:=AFlags;
         NextInfo.ai_family:=AFamily;
         NextInfo.ai_socktype:=ASocketType;
         NextInfo.ai_protocol:=AProtocol;
         NextInfo.ai_addrlen:=SizeOf(TSockAddr);

         {Create Sock Address}
         NextInfo.ai_addr:=AllocMem(NextInfo.ai_addrlen);
         if NextInfo.ai_addr = nil then Exit;

         {Return Sock Address}
         NextInfo.ai_addr.sin_family:=AFamily;
         NextInfo.ai_addr.sin_port:=WordNtoBE(APort);
         NextInfo.ai_addr.sin_addr:=InAddrToNetwork(TIPHostEntry(HostEntry).Addresses[Index]);

         {Check Canonical Name}
         if AFirst and (Result = nil) and ((AFlags and AI_CANONNAME) = AI_CANONNAME) then
          begin
           {Create Name}
           NextInfo.ai_canonname:=AllocMem((Length(HostEntry.Name) + 1) * SizeOf(Char));
           if NextInfo.ai_canonname = nil then Exit;

           StrLCopy(NextInfo.ai_canonname,PChar(HostEntry.Name),Length(HostEntry.Name));
          end;

         {Check Last}
         LastInfo:=GetLastAddrInfo(Result);
         if LastInfo = nil then Result:=NextInfo else LastInfo.ai_next:=NextInfo;
        end;
      end;
    end;
   AF_INET6:begin 
     {IPv6}
     for Index:=0 to MAX_HOST_ALIASES - 1 do
      begin
       {Check Address}
       if not In6AddrIsDefault(TIP6HostEntry(HostEntry).Addresses[Index]) then
        begin
         {Create Address Info}
         NextInfo:=AllocMem(SizeOf(TAddrInfo));
         if NextInfo = nil then Exit;

         {Update Address Info}
         NextInfo.ai_flags:=AFlags;
         NextInfo.ai_family:=AFamily;
         NextInfo.ai_socktype:=ASocketType;
         NextInfo.ai_protocol:=AProtocol;
         NextInfo.ai_addrlen:=SizeOf(TSockAddr6);

         {Create Sock Address}
         NextInfo.ai_addr:=AllocMem(NextInfo.ai_addrlen);
         if NextInfo.ai_addr = nil then Exit;

         {Return Sock Address}
         PSockAddr6(NextInfo.ai_addr).sin6_family:=AFamily;
         PSockAddr6(NextInfo.ai_addr).sin6_port:=WordNtoBE(APort);
         PSockAddr6(NextInfo.ai_addr).sin6_addr:=TIP6HostEntry(HostEntry).Addresses[Index];

         {Check Canonical Name}
         if AFirst and (Result = nil) and ((AFlags and AI_CANONNAME) = AI_CANONNAME) then
          begin
           {Create Name}
           NextInfo.ai_canonname:=AllocMem((Length(HostEntry.Name) + 1) * SizeOf(Char));
           if NextInfo.ai_canonname = nil then Exit;

           StrLCopy(NextInfo.ai_canonname,PChar(HostEntry.Name),Length(HostEntry.Name));
          end;

         {Check Last}
         LastInfo:=GetLastAddrInfo(Result);
         if LastInfo = nil then Result:=NextInfo else LastInfo.ai_next:=NextInfo;
        end;
      end;
    end;
  end;
 finally
  if Result = nil then FreeAddrInfo(NextInfo);
 end; 
end;

{==============================================================================}

function TDNSClient.ResolveLocalAddrInfo(AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word):PAddrInfo;
var
 AddrInfo:PAddrInfo;
 LastInfo:PAddrInfo;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
 IPAddressEntry:TIPAddressEntry;
 IP6AddressEntry:TIP6AddressEntry;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveLocalAddrInfo Flags = ' + IntToHex(AFlags,8) + ' Family = ' + IntToStr(AFamily) + ' Protocol = ' + IntToStr(AProtocol) + ' SocketType = ' + IntToStr(ASocketType) + ' Port = ' + IntToStr(APort) + ')');
 {$ENDIF}

 {Set Defaults}
 AddrInfo:=nil;
 LastInfo:=nil;

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Family}
  if (AFamily = AF_INET) and (IPTransport <> nil) then
   begin
    {Check the Addresses}
    IPAddressEntry:=IPTransport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
    while IPAddressEntry <> nil do
     begin
      {Check Address Type}
      if (IPAddressEntry.AddressType = ADDRESS_TYPE_PRIMARY) or (IPAddressEntry.AddressType = ADDRESS_TYPE_SECONDARY) then
       begin
        {Create Address Info}
        AddrInfo:=AddressEntryToAddrInfo(IPAddressEntry,AFlags,AFamily,AProtocol,ASocketType,APort,(Result = nil));
        if AddrInfo = nil then Exit;
 
        {Check Last}
        LastInfo:=GetLastAddrInfo(Result);
        if LastInfo = nil then Result:=AddrInfo else LastInfo.ai_next:=AddrInfo;
       end;

      {Get Next}
      IPAddressEntry:=IPTransport.GetAddressByNext(IPAddressEntry,True,True,NETWORK_LOCK_READ);
     end;
   end;

  if (AFamily = AF_INET6) and (IP6Transport <> nil) then
   begin
    {Check the Addresses}
    IP6AddressEntry:=IP6Transport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
    while IP6AddressEntry <> nil do
     begin
      {Check Address Type}
      if (IP6AddressEntry.AddressType = ADDRESS_TYPE_PRIMARY) or (IP6AddressEntry.AddressType = ADDRESS_TYPE_SECONDARY) then
       begin
        {Create Address Info}
        AddrInfo:=AddressEntryToAddrInfo(IPAddressEntry,AFlags,AFamily,AProtocol,ASocketType,APort,(Result = nil));
        if AddrInfo = nil then Exit;
 
        {Check Last}
        LastInfo:=GetLastAddrInfo(Result);
        if LastInfo = nil then Result:=AddrInfo else LastInfo.ai_next:=AddrInfo;
       end;

      {Get Next}
      IP6AddressEntry:=IP6Transport.GetAddressByNext(IP6AddressEntry,True,True,NETWORK_LOCK_READ);
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveLoopbackAddrInfo(AFlags,AFamily,AProtocol,ASocketType:LongInt;APort:Word):PAddrInfo;
var
 AddrInfo:PAddrInfo;
 LastInfo:PAddrInfo;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
 IPAddressEntry:TIPAddressEntry;
 IP6AddressEntry:TIP6AddressEntry;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveLoopbackAddrInfo Flags = ' + IntToHex(AFlags,8) + ' Family = ' + IntToStr(AFamily) + ' Protocol = ' + IntToStr(AProtocol) + ' SocketType = ' + IntToStr(ASocketType) + ' Port = ' + IntToStr(APort) + ')');
 {$ENDIF}

 {Set Defaults}
 AddrInfo:=nil;
 LastInfo:=nil;

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Family}
  if (AFamily = AF_INET) and (IPTransport <> nil) then
   begin
    {Check the Addresses}
    IPAddressEntry:=IPTransport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
    while IPAddressEntry <> nil do
     begin
      {Check Address Type}
      if IPAddressEntry.AddressType = ADDRESS_TYPE_LOOPBACK then
       begin
        {Create Address Info}
        AddrInfo:=AddressEntryToAddrInfo(IPAddressEntry,AFlags,AFamily,AProtocol,ASocketType,APort,(Result = nil));
        if AddrInfo = nil then Exit;
 
        {Check Last}
        LastInfo:=GetLastAddrInfo(Result);
        if LastInfo = nil then Result:=AddrInfo else LastInfo.ai_next:=AddrInfo;
       end;

      {Get Next}
      IPAddressEntry:=IPTransport.GetAddressByNext(IPAddressEntry,True,True,NETWORK_LOCK_READ);
     end;
   end;

  if (AFamily = AF_INET6) and (IP6Transport <> nil) then
   begin
    {Check the Addresses}
    IP6AddressEntry:=IP6Transport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
    while IP6AddressEntry <> nil do
     begin
      {Check Address Type}
      if IP6AddressEntry.AddressType = ADDRESS_TYPE_LOOPBACK then
       begin
        {Create Address Info}
        AddrInfo:=AddressEntryToAddrInfo(IPAddressEntry,AFlags,AFamily,AProtocol,ASocketType,APort,(Result = nil));
        if AddrInfo = nil then Exit;
 
        {Check Last}
        LastInfo:=GetLastAddrInfo(Result);
        if LastInfo = nil then Result:=AddrInfo else LastInfo.ai_next:=AddrInfo;
       end;

      {Get Next}
      IP6AddressEntry:=IP6Transport.GetAddressByNext(IP6AddressEntry,True,True,NETWORK_LOCK_READ);
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveHostByName(const AName:String;AFamily:Integer):THostEntry;
{Performs a DNS_TYPE_A or DNS_TYPE_AAAA Query for the Address of the Domain name supplied}
{Caller must check if name is blank or 'localhost' and get the appropriate local addresses}
{Will lock the returned host entry for read access, caller must unlock when finished}
var
 NameBuffer:String;
 HostEntry:THostEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveHostByName (Name = ' + AName + ' Family = ' + IntToStr(AFamily) + ')');
 {$ENDIF}

 {Get the Name}
 NameBuffer:=AName;

 {Check the Domain}
 if Pos('.',NameBuffer) = 0 then NameBuffer:=NameBuffer + AddLeadingDot(FProtocol.Manager.Settings.DomainName);

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Family}
  if (AFamily = AF_INET) and (IPTransport <> nil) then
   begin
    {IPv4}
    {Check the Cache}
    HostEntry:=IPTransport.GetHostByName(NameBuffer,True);
    if HostEntry <> nil then
     begin
      {Return Host Entry if found}
      Result:=HostEntry;
     end;
   end;

  if (AFamily = AF_INET6) and (IP6Transport <> nil) then
   begin
    {IPv6}
    {Check the Cache}
    HostEntry:=IP6Transport.GetHostByName(NameBuffer,True);
    if HostEntry <> nil then
     begin
      {Return Host Entry if found}
      Result:=HostEntry;
     end;
   end;
   
  {Check Result}
  if Result = nil then
   begin
    {Check Family}
    if (AFamily = AF_INET) and (IPTransport <> nil) then
     begin
      {IPv4}
      {Perform DNS_TYPE_A Query}
      if PerformDNSRequest(PChar(NameBuffer),Length(NameBuffer),AFamily,DNS_TYPE_A,DNS_CLASS_IN) then
       begin
        {Recheck the Cache}
        Result:=IPTransport.GetHostByName(NameBuffer,True);
       end;
     end;

    if (AFamily = AF_INET6) and (IP6Transport <> nil) then
     begin
      {IPv6}
      {Perform DNS_TYPE_AAAA Query}
      if PerformDNSRequest(PChar(NameBuffer),Length(NameBuffer),AFamily,DNS_TYPE_AAAA,DNS_CLASS_IN) then
       begin
        {Recheck the Cache}
        Result:=IP6Transport.GetHostByName(NameBuffer,True);
       end;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveHostByAddress(AAddress:Pointer;ALength,AFamily:Integer):THostEntry;
{Performs a DNS_TYPE_PTR Query for the Domain name of the Address supplied}
{Note: Address will be in network order where applicable}
{Will lock the returned host entry for read access, caller must unlock when finished}
var 
 HostEntry:THostEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveHostByAddress (Address = ' + PtrToHex(AAddress) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ')');
 {$ENDIF}

 {Check the Address}
 if AAddress = nil then Exit;

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Family}
  if (AFamily = AF_INET) and (ALength >= SizeOf(TInAddr)) and (IPTransport <> nil) then
   begin
    {IPv4}
    {Check the Cache}
    HostEntry:=IPTransport.GetHostByAddress(InAddrToHost(PInAddr(AAddress)^),True);
    if HostEntry <> nil then
     begin
      {Return Host Entry if found}
      Result:=HostEntry;
     end;
   end;

  if (AFamily = AF_INET6) and (ALength >= SizeOf(TIn6Addr)) and (IP6Transport <> nil) then
   begin
    {IPv6}
    {Check the Cache}
    HostEntry:=IP6Transport.GetHostByAddress(PIn6Addr(AAddress)^,True);
    if HostEntry <> nil then
     begin
      {Return Host Entry if found}
      Result:=HostEntry;
     end;
   end;

  {Check Result}
  if Result = nil then
   begin
    {Check Family}
    if (AFamily = AF_INET) and (ALength >= SizeOf(TInAddr)) and (IPTransport <> nil) then
     begin
      {IPv4}
      {Perform DNS_TYPE_PTR Query}
      if PerformDNSRequest(AAddress,ALength,AFamily,DNS_TYPE_PTR,DNS_CLASS_IN) then
       begin
        {Recheck the Cache}
        Result:=IPTransport.GetHostByAddress(InAddrToHost(PInAddr(AAddress)^),True);
       end;
     end;
    
    if (AFamily = AF_INET6) and (ALength >= SizeOf(TIn6Addr)) and (IP6Transport <> nil) then
     begin
      {IPv6}
      {Perform DNS_TYPE_PTR Query}
      if PerformDNSRequest(AAddress,ALength,AFamily,DNS_TYPE_PTR,DNS_CLASS_IN) then
       begin
        {Recheck the Cache}
        Result:=IP6Transport.GetHostByAddress(PIn6Addr(AAddress)^,True);
       end;
     end;
   end; 
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}
   
function TDNSClient.ResolveHostName:String;
begin
 {}
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveHostName');
 {$ENDIF}

 Result:=FProtocol.Manager.Settings.HostName;
end;

{==============================================================================}
   
function TDNSClient.ResolveServByName(const AName,AProto:String):TServEntry;
{Will lock the returned service entry for read access, caller must unlock when finished}
var 
 ServEntry:TServEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveServByName (Name = ' + AName + ' Proto = ' + AProto + ')');
 {$ENDIF}

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Transports}
  if IPTransport <> nil then
   begin
    {IPv4}
    {Check the Cache}
    ServEntry:=IPTransport.GetServByName(AName,AProto,True);
    if ServEntry <> nil then
     begin
      {Return Service Entry if found}
      Result:=ServEntry;
     end;
   end;

  if IP6Transport <> nil then
   begin
    {IPv6}
    {Check the Cache}
    ServEntry:=IP6Transport.GetServByName(AName,AProto,True);
    if ServEntry <> nil then
     begin
      {Return Service Entry if found}
      Result:=ServEntry;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveServByPort(APort:Integer;const AProto:String):TServEntry;
{Note: Port will be in network order}
{Will lock the returned service entry for read access, caller must unlock when finished}
var 
 ServEntry:TServEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveServByPort (Port = ' + IntToStr(WordBEToN(APort)) + ' Proto = ' + AProto + ')');
 {$ENDIF}

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Transports}
  if IPTransport <> nil then
   begin
    {IPv4}
    {Check the Cache}
    ServEntry:=IPTransport.GetServByPort(WordBEtoN(APort),AProto,True);
    if ServEntry <> nil then
     begin
      {Return Service Entry if found}
      Result:=ServEntry;
     end;
   end;

  if IP6Transport <> nil then
   begin
    {IPv6}
    {Check the Cache}
    ServEntry:=IP6Transport.GetServByPort(WordBEtoN(APort),AProto,True);
    if ServEntry <> nil then
     begin
      {Return Service Entry if found}
      Result:=ServEntry;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveProtoByName(const AName:String):TProtoEntry;
{Will lock the returned protocol entry for read access, caller must unlock when finished}
var
 ProtoEntry:TProtoEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveProtoByName (Name = ' + AName + ')');
 {$ENDIF}

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Transports}
  if IPTransport <> nil then
   begin
    {IPv4}
    {Check the Cache}
    ProtoEntry:=IPTransport.GetProtoByName(AName,True);
    if ProtoEntry <> nil then
     begin
      {Return Protocol Entry if found}
      Result:=ProtoEntry;
     end;
   end;

  if IP6Transport <> nil then
   begin
    {IPv6}
    {Check the Cache}
    ProtoEntry:=IP6Transport.GetProtoByName(AName,True);
    if ProtoEntry <> nil then
     begin
      {Return Protocol Entry if found}
      Result:=ProtoEntry;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveProtoByNumber(AProto:Integer):TProtoEntry;
{Note: Protocol will be in host order}
{Will lock the returned protocol entry for read access, caller must unlock when finished}
var
 ProtoEntry:TProtoEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveProtoByNumber (Proto = ' + IntToStr(AProto) + ')');
 {$ENDIF}

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Transports}
  if IPTransport <> nil then
   begin
    {IPv4}
    {Check the Cache}
    ProtoEntry:=IPTransport.GetProtoByNumber(AProto,True); 
    if ProtoEntry <> nil then
     begin
      {Return Protocol Entry if found}
      Result:=ProtoEntry;
     end;
   end;

  if IP6Transport <> nil then
   begin
    {IPv6}
    {Check the Cache}
    ProtoEntry:=IP6Transport.GetProtoByNumber(AProto,True); 
    if ProtoEntry <> nil then
     begin
      {Return Protocol Entry if found}
      Result:=ProtoEntry;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}
   
function TDNSClient.ResolveNetworkByName(const AName:String;AFamily:Integer):TNetworkEntry;
{Will lock the returned network entry for read access, caller must unlock when finished}
var
 NetworkEntry:TNetworkEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveNetworkByName (Name = ' + AName + ' Family = ' + IntToStr(AFamily) + ')');
 {$ENDIF}

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Family}
  if (AFamily = AF_INET) and (IPTransport <> nil) then
   begin
    {IPv4}
    {Check the Cache}
    NetworkEntry:=IPTransport.GetNetworkByName(AName,True); 
    if NetworkEntry <> nil then
     begin
      {Return Network Entry if found}
      Result:=NetworkEntry;
     end;
   end;

  if (AFamily = AF_INET6) and (IP6Transport <> nil) then
   begin
    {IPv6}
    {Check the Cache}
    NetworkEntry:=IP6Transport.GetNetworkByName(AName,True); 
    if NetworkEntry <> nil then
     begin
      {Return Network Entry if found}
      Result:=NetworkEntry;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.ResolveNetworkByAddress(AAddress:Pointer;ALength,AFamily:Integer):TNetworkEntry;
{Note: Address will be in network order where applicable}
{Will lock the returned network entry for read access, caller must unlock when finished}
var
 NetworkEntry:TNetworkEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=nil;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: ResolveNetworkByAddress (Address = ' + PtrToHex(AAddress) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ')');
 {$ENDIF}
 
 {Check the Address}
 if AAddress = nil then Exit;

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check the Family}
  if (AFamily = AF_INET) and (ALength >= SizeOf(TInAddr)) and (IPTransport <> nil) then
   begin
    {IPv4}
    {Check the Cache}
    NetworkEntry:=IPTransport.GetNetworkByAddress(InAddrToHost(PInAddr(AAddress)^),True);
    if NetworkEntry <> nil then
     begin
      {Return Network Entry if found}
      Result:=NetworkEntry;
     end;
   end;

  if (AFamily = AF_INET6) and (ALength >= SizeOf(TIn6Addr)) and (IP6Transport <> nil) then
   begin
    {IPv6}
    {Check the Cache}
    NetworkEntry:=IP6Transport.GetNetworkByAddress(PIn6Addr(AAddress)^,True);
    if NetworkEntry <> nil then
     begin
      {Return Network Entry if found}
      Result:=NetworkEntry;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.InAddrToName(const AAddress:TInAddr):String;
{Converts an Address to a Name in IN-ADDR.ARPA format}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=Lowercase(InAddrToString(AAddress) + '.IN-ADDR.ARPA');
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
 PosIdx:=Pos('.IN-ADDR.ARPA',Uppercase(AName));
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
   if Count > 0 then Result:='.' + Result;

   {High Nibble}
   Result:=IntToHex((AAddress.u6_addr8[Count] and $F0) shr 4,1) + Result;

   {Low Nibble}
   Result:=IntToHex((AAddress.u6_addr8[Count] and $0F),1) + '.' + Result;
  end;

 Result:=Lowercase(Result +  '.IP6.ARPA');
end;

{==============================================================================}

function TDNSClient.NameToIn6Addr(const AName:String):TIn6Addr;
{Converts a Name in IP6.ARPA format to an Address}
var
 Count:Integer;
 Index:Integer;
 PosIdx:Integer;
 DotCount:Integer;
 WorkBuffer:String;
begin
 {}
 try
  Result:=IN6ADDR_ANY;

  {Check Suffix}
  PosIdx:=Pos('.IP6.ARPA',Uppercase(AName));
  if PosIdx = 0 then Exit;

  WorkBuffer:=AName;

  {Check Address Length}
  if Length(WorkBuffer) <> 72 then Exit; {63 charactes for address and 9 for suffix}

  {Check for Hexadecimal Address}
  for Count:=1 to Length(WorkBuffer) - Length('.IP6.ARPA') do
   begin
    if not (WorkBuffer[Count] in ['.', '0'..'9', 'a'..'f', 'A'..'F']) then Exit;
   end;

  {Set Defaults}
  DotCount:=0;

  {Count the number of dots}
  for Count:=1 to Length(WorkBuffer) do
   begin
    if WorkBuffer[Count] = '.' then Inc(DotCount);
   end;
  if DotCount <> 33 then Exit; {31 dots in address plus 2 in suffix}

  {Extract each Byte}
  Count:=15;
  Index:=1;
  while Count >= 0 do
   begin
    {Low Nibble}
    Result.u6_addr8[Count]:=StrToInt('$' + WorkBuffer[Index]);
    Inc(Index);

    {Check Dot}
    if WorkBuffer[Index] <> '.' then Exit;
    Inc(Index);

    {High Nibble}
    Result.u6_addr8[Count]:=Result.u6_addr8[Count] or (StrToInt('$' + WorkBuffer[Index]) shl 4);
    Inc(Index);

    {Check Dot}
    if WorkBuffer[Index] <> '.' then Exit;
    Inc(Index);

    {Get next Byte}
    Dec(Count);
   end;
 except
  {}
  Result:=IN6ADDR_ANY;
 end;
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
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Creating DNS Query (Identifier = ' + IntToHex(AIdentifier,4) + ')');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Zero Message}
 FillChar(AMessage^,SizeOf(TDNSMessage),0);
 
 {Set the Id}
 AMessage.DNS.Identifier:=WordNtoBE(AIdentifier);
 
 {Set the Flags}
 Flags:=DNS_OPCODE_QUERY shl 11;       {Opcode}
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
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Checking DNS Response (Identifier = ' + IntToHex(AIdentifier,4) + ')');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;
 
 {Check the Id}
 if WordBEtoN(AMessage.DNS.Identifier) <> AIdentifier then Exit;
 
 {Check the Flags}
 Flags:=WordBEtoN(AMessage.DNS.Flags);
 if ((Flags and DNS_OPCODE_MASK) shr 11) <> DNS_OPCODE_QUERY then Exit;
 if (Flags and DNS_FLAG_RESPONSE) <> DNS_FLAG_RESPONSE then Exit;
 if (Flags and DNS_RESPONSE_MASK) <> DNS_RESPONSE_NO_ERROR then Exit;
 
 {Check the Counts}
 if WordBEtoN(AMessage.DNS.QuestionCount) < 1 then Exit;
 if WordBEtoN(AMessage.DNS.AnswerCount) < 1 then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDNSClient.HandleDNSResponse(AMessage:PDNSMessage;AFamily,AIdentifier:Word):Boolean;
var
 Name:Pointer;
 Data:Pointer;
 Count:Integer;
 NameLength:Word;
 DataLength:Word;
 AnswerType:Word;
 AnswerClass:Word;
 AnswerTtl:LongWord;
 HostEntry:THostEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Handling DNS Response (Family = ' + IntToStr(AFamily) + ' Identifier = ' + IntToHex(AIdentifier,4) + ')');
 {$ENDIF}
 
 {Check Message}
 if AMessage = nil then Exit;

 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ)); 
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
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
      if ExtractDNSAnswer(AMessage,Count,Name,Data,NameLength,DataLength,AFamily,AnswerType,AnswerClass,AnswerTtl) then
       begin
        {Process the Class}
        case AnswerClass of
         DNS_CLASS_IN:begin
           {Process the Type}
           case AnswerType of
            DNS_TYPE_A:begin
              {IPv4}
              if IPTransport <> nil then
               begin
                {Insert a Host Entry or update existing Entry}
                HostEntry:=IPTransport.GetHostByName(PChar(Name),True);
                if HostEntry = nil then
                 begin
                  {$IFDEF DNS_DEBUG}
                  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Adding A Record Host Entry (Address = ' + InAddrToString(InAddrToNetwork(PInAddr(Data)^)) + ' Name = ' + PChar(Name) + ')');
                  {$ENDIF}

                  {Add Host}
                  Result:=(IPTransport.AddHost(PInAddr(Data)^,PChar(Name),HOST_TYPE_DYNAMIC,False) <> nil);
                 end
                else
                 begin
                  {$IFDEF DNS_DEBUG}
                  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Updating A Record Host Entry (Address = ' + InAddrToString(InAddrToNetwork(PInAddr(Data)^)) + ' Name = ' + PChar(Name) + ')');
                  {$ENDIF}

                  {Update Host}
                  Result:=TIPHostEntry(HostEntry).AddAddress(PInAddr(Data)^);

                  {Unlock Host}
                  HostEntry.ReleaseLock;
                 end;
               end;
             end;
            DNS_TYPE_AAAA:begin
              {IPv6}
              if IP6Transport <> nil then
               begin
                {Insert a Host Entry or update existing Entry}
                HostEntry:=IP6Transport.GetHostByName(PChar(Name),True);
                if HostEntry = nil then
                 begin
                  {$IFDEF DNS_DEBUG}
                  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Adding AAAA Record Host Entry (Address = ' + In6AddrToString(PIn6Addr(Data)^) + ' Name = ' + PChar(Name) + ')');
                  {$ENDIF}

                  {Add Host}
                  Result:=(IP6Transport.AddHost(PIn6Addr(Data)^,PChar(Name),HOST_TYPE_DYNAMIC,False) <> nil);
                 end
                else
                 begin
                  {$IFDEF DNS_DEBUG}
                  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Updating AAAA Record Host Entry (Address = ' + In6AddrToString(PIn6Addr(Data)^) + ' Name = ' + PChar(Name) + ')');
                  {$ENDIF}

                  {Update Host}
                  Result:=TIP6HostEntry(HostEntry).AddAddress(PIn6Addr(Data)^);

                  {Unlock Host}
                  HostEntry.ReleaseLock;
                 end;
               end;
             end;
            DNS_TYPE_PTR:begin
              case AFamily of
               AF_INET:begin
                 {IPv4}
                 if IPTransport <> nil then
                  begin
                   {Insert a Host Entry or update existing Entry}
                   HostEntry:=IPTransport.GetHostByAddress(PInAddr(Name)^,True); 
                   if HostEntry = nil then
                    begin
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Adding PTR Record Host Entry (Address = ' + InAddrToString(InAddrToNetwork(PInAddr(Name)^)) + ' Name = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Add Host}
                     Result:=(IPTransport.AddHost(PInAddr(Name)^,PChar(Data),HOST_TYPE_DYNAMIC,False) <> nil);
                    end
                   else
                    begin
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Updating PTR Record Host Entry (Address = ' + InAddrToString(InAddrToNetwork(PInAddr(Name)^)) + ' Name = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Update Host}
                     Result:=HostEntry.AddAlias(PChar(Data));

                     {Unlock Host}
                     HostEntry.ReleaseLock;
                    end;
                  end;
                end;
               AF_INET6:begin 
                 {IPv6}
                 if IP6Transport <> nil then
                  begin
                   {Insert a Host Entry or update existing Entry}
                   HostEntry:=IP6Transport.GetHostByAddress(PIn6Addr(Name)^,True); 
                   if HostEntry = nil then
                    begin
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Adding PTR Record Host Entry (Address = ' + In6AddrToString(PIn6Addr(Name)^) + ' Name = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Add Host}
                     Result:=(IP6Transport.AddHost(PIn6Addr(Name)^,PChar(Data),HOST_TYPE_DYNAMIC,False) <> nil);
                    end
                   else
                    begin
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Updating PTR Record Host Entry (Address = ' + In6AddrToString(PIn6Addr(Name)^) + ' Name = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Update Host}
                     Result:=HostEntry.AddAlias(PChar(Data));

                     {Unlock Host}
                     HostEntry.ReleaseLock;
                    end;
                  end;
                end;
              end;
             end;
            DNS_TYPE_CNAME:begin
              case AFamily of
               AF_INET:begin
                 {IPv4}
                 if IPTransport <> nil then
                  begin
                   {Insert a Host Entry or update existing Entry}
                   HostEntry:=IPTransport.GetHostByName(PChar(Name),True); 
                   if HostEntry = nil then
                    begin
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Adding CNAME Record Host Entry (Name = ' + PChar(Data) + ' Alias = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Add Host}
                     HostEntry:=IPTransport.AddHost(IP_DEFAULT_ADDRESS,PChar(Name),HOST_TYPE_DYNAMIC,True);
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
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Updating CNAME Record Host Entry (Name = ' + PChar(Data) + ' Alias = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Update Host}
                     Result:=HostEntry.AddAlias(PChar(Data));

                     {Unlock Host}
                     HostEntry.ReleaseLock;
                    end;
                  end;
                end;
               AF_INET6:begin 
                 {IPv6}
                 if IP6Transport <> nil then
                  begin
                   {Insert a Host Entry or update existing Entry}
                   HostEntry:=IP6Transport.GetHostByName(PChar(Name),True); 
                   if HostEntry = nil then
                    begin
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Adding CNAME Record Host Entry (Name = ' + PChar(Data) + ' Alias = ' + PChar(Data) + ')');
                     {$ENDIF}

                     {Add Host}
                     HostEntry:=IP6Transport.AddHost(IP6_DEFAULT_ADDRESS,PChar(Name),HOST_TYPE_DYNAMIC,True);
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
                     {$IFDEF DNS_DEBUG}
                     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Updating CNAME Record Host Entry (Name = ' + PChar(Data) + ' Alias = ' + PChar(Data) + ')');
                     {$ENDIF}

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
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.PerformDNSRequest(AData:Pointer;ALength,AFamily,AType,AClass:Word):Boolean;
{Caller must confirm that the data and length are valid}
var
 Count:Integer;
 Retries:Integer;
 Timeout:Integer;
 Identifier:Word;
 SockAddr:PSockAddr;
 ServerAddr:PSockAddr;
 Socket:TProtocolSocket;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
 IPNameservers:TIPNameservers;
 IP6Nameservers:TIP6Nameservers;
begin
 {}
 Result:=False;

 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Performing DNS Query (Data = ' + PtrToHex(AData) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ' Type = ' + IntToStr(AType) + ' Class = ' + IntToStr(AClass) + ')');
 {$ENDIF}

 {Get the Transports}
 IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
 IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
 try
  {Check IPv4 Nameservers}
  if not Result and (IPTransport <> nil) then
   begin
    {Create a Socket}
    Socket:=FProtocol.Socket(AF_INET,FProtocol.SocketType,FProtocol.Protocol);
    if Socket <> nil then
     begin
      {Lock Socket}
      Socket.ReaderLock;
      try
       {Allocate Socket Address}
       SockAddr:=AllocMem(SizeOf(TSockAddrIn));
       if SockAddr <> nil then
        begin
         {Get Socket Address}
         PSockAddrIn(SockAddr).sin_family:=AF_INET;
         PSockAddrIn(SockAddr).sin_port:=WordNtoBE(IPPORT_ANY);
         PSockAddrIn(SockAddr).sin_addr.S_addr:=INADDR_ANY;

         {Bind the Socket}
         if FProtocol.Bind(Socket,PSockAddr(SockAddr)^,SizeOf(TSockAddrIn)) <> SOCKET_ERROR then
          begin
           {Set the Timeout}
           Timeout:=DNS_TIMEOUT;
           if FProtocol.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) <> SOCKET_ERROR then
            begin
             {Allocate Server Address}
             ServerAddr:=AllocMem(SizeOf(TSockAddrIn));
             if ServerAddr <> nil then
              begin
               {Get Identifier}
               Identifier:=Random($7FFF) + 1;

               {Get the Nameservers}
               IPNameservers:=IPTransport.Nameservers; //To Do //GetNameserverByNext etc
               for Count:=0 to MAX_NAME_SERVERS - 1 do
                begin
                 if IPTransport.CompareDefault(IPNameservers[Count]) then Continue;

                 {Get Server Address}
                 PSockAddrIn(ServerAddr).sin_family:=AF_INET;
                 PSockAddrIn(ServerAddr).sin_port:=WordNtoBE(IPPORT_DNS);
                 PSockAddrIn(ServerAddr).sin_addr:=InAddrToNetwork(IPNameservers[Count]);

                 Retries:=0;
                 while Retries < DNS_RETRIES do
                  begin
                   {Send Request to each Nameserver}
                   if SendDNSQuery(Socket,ServerAddr,SizeOf(TSockAddrIn),AData,ALength,AFamily,AType,AClass,Identifier) then
                    begin
                     {Receive Response}
                     if RecvDNSResponse(Socket,AFamily,AType,AClass,Identifier) then
                      begin
                       Result:=True;
                       Break;
                      end;
                    end;

                   Inc(Retries);
                  end;

                 if Result then Break;
                end;

               {Free Server Address}
               FreeMem(ServerAddr);
              end;
            end;
          end;

         {Free Socket Address}
         FreeMem(SockAddr);
        end;
      finally
       {Close the Socket}
       FProtocol.CloseSocket(Socket);

       {Unlock Socket}
       Socket.ReaderUnlock;
      end;
     end;
   end;

  {Check IPv6 Nameservers}
  if not Result and (IP6Transport <> nil) then
   begin
    {Create a Socket}
    Socket:=FProtocol.Socket(AF_INET6,FProtocol.SocketType,FProtocol.Protocol);
    if Socket <> nil then
     begin
      {Lock Socket}
      Socket.ReaderLock;
      try
       {Allocate Socket Address}
       SockAddr:=AllocMem(SizeOf(TSockAddrIn6));
       if SockAddr <> nil then
        begin
         {Get Socket Address}
         PSockAddrIn6(SockAddr).sin6_family:=AF_INET6;
         PSockAddrIn6(SockAddr).sin6_port:=WordNtoBE(IPPORT_ANY);
         PSockAddrIn6(SockAddr).sin6_addr:=IN6ADDR_ANY;

         {Bind the Socket}
         if FProtocol.Bind(Socket,PSockAddr(SockAddr)^,SizeOf(TSockAddrIn6)) <> SOCKET_ERROR then
          begin
           {Set the Timeout}
           Timeout:=DNS_TIMEOUT;
           if FProtocol.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) <> SOCKET_ERROR then
            begin
             {Allocate Server Address}
             ServerAddr:=AllocMem(SizeOf(TSockAddrIn6));
             if ServerAddr <> nil then
              begin
               {Get Identifier}
               Identifier:=Random($7FFF) + 1;

               {Get the Nameservers}
               IP6Nameservers:=IP6Transport.Nameservers; //To Do //GetNameserverByNext etc
               for Count:=0 to MAX_NAME_SERVERS - 1 do
                begin
                 if IP6Transport.CompareDefault(IP6Nameservers[Count]) then Continue;

                 {Get Server Address}
                 PSockAddrIn6(ServerAddr).sin6_family:=AF_INET6;
                 PSockAddrIn6(ServerAddr).sin6_port:=WordNtoBE(IPPORT_DNS);
                 PSockAddrIn6(ServerAddr).sin6_addr:=IP6Nameservers[Count];

                 Retries:=0;
                 while Retries < DNS_RETRIES do
                  begin
                   {Send Request to each Nameserver}
                   if SendDNSQuery(Socket,ServerAddr,SizeOf(TSockAddrIn6),AData,ALength,AFamily,AType,AClass,Identifier) then
                    begin
                     {Receive Response}
                     if RecvDNSResponse(Socket,AFamily,AType,AClass,Identifier) then
                      begin
                       Result:=True;
                       Break;
                      end;
                    end;

                   Inc(Retries);
                  end;

                 if Result then Break;
                end;

               {Free Server Address}
               FreeMem(ServerAddr);
              end;
            end;
          end;

         {Free Socket Address}
         FreeMem(SockAddr);
        end;
      finally
       {Close the Socket}
       FProtocol.CloseSocket(Socket);

       {Unlock Socket}
       Socket.ReaderUnlock;
      end;
     end;
   end;
 finally
  if IPTransport <> nil then IPTransport.ReaderUnlock;
  if IP6Transport <> nil then IP6Transport.ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.SendDNSQuery(ASocket:TProtocolSocket;AServer:PSockAddr;AServerLength:Integer;AData:Pointer;ALength,AFamily,AType,AClass,AIdentifier:Word):Boolean;
{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 Query:TDNSMessage;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Sending DNS Query (Data = ' + PtrToHex(AData) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ' Type = ' + IntToStr(AType) + ' Class = ' + IntToStr(AClass) + ')');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Create the Message}
 if CreateDNSQuery(@Query,AIdentifier) then
  begin
   {Insert the Question}
   if not InsertDNSQuestion(@Query,1,AData,ALength,AFamily,AType,AClass) then Exit;
   
   {Get the Size}
   Size:=GetDNSMessageSize(@Query);

   {Send the Query}
   Result:=FProtocol.SendTo(ASocket,Query,Size,0,AServer^,AServerLength) = Size;
  end;
end;

{==============================================================================}

function TDNSClient.RecvDNSResponse(ASocket:TProtocolSocket;AFamily,AType,AClass,AIdentifier:Word):Boolean;
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
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Receiving DNS Response (Family = ' + IntToStr(AFamily) + ' Type = ' + IntToStr(AType) + ' Class = ' + IntToStr(AClass) + ' Identifier = ' + IntToHex(AIdentifier,4) + ')');
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
     Result:=HandleDNSResponse(@Response,AFamily,AIdentifier);
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
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Inserting DNS Name (Offset = ' + IntToStr(AOffset) + ' Name = ' + PtrToHex(AName) + ' Length = ' + IntToStr(ALength) + ')');
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
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Name (Offset = ' + IntToStr(AOffset) + ' Name = ' + PtrToHex(AName) + ')');
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
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS RData (Offset = ' + IntToStr(AOffset) + ' Data = ' + PtrToHex(AData) + ' Type = ' + IntToStr(AType) + ' Class = ' + IntToStr(AClass) + ')');
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
     DNS_TYPE_AAAA:begin
       {Extract an Address}
       if WordBEtoN(Resource.DataLength) <> SizeOf(TIn6Addr) then Exit;
       ALength:=SizeOf(TIn6Addr);
       System.Move(Resource.RecordData[0],AData^,ALength);
       
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

function TDNSClient.InsertDNSQuestion(AMessage:PDNSMessage;ACount:Word;AData:Pointer;ALength,AFamily,AType,AClass:Word):Boolean;
var
 Name:String;
 Offset:Word;
 Question:PDNSQuestion;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Inserting DNS Question (Count = ' + IntToStr(ACount) + ' Data = ' + PtrToHex(AData) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ' Type = ' + IntToStr(AType) + ' Class = ' + IntToStr(AClass) + ')');
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
       case AFamily of
        AF_INET:begin
          {IPv4}
          {If PTR convert InAddr to Name}
          if ALength <> SizeOf(TInAddr) then Exit;
          Name:=InAddrToName(InAddrToHost(PInAddr(AData)^));
          if not InsertDNSName(AMessage,Offset,PChar(Name),Length(Name)) then Exit;
         end;
        AF_INET6:begin 
          {IPv6}
          {If PTR convert InAddr to Name}
          if ALength <> SizeOf(TIn6Addr) then Exit;
          Name:=In6AddrToName(PIn6Addr(AData)^);
          if not InsertDNSName(AMessage,Offset,PChar(Name),Length(Name)) then Exit;
         end;
       end;
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

function TDNSClient.ExtractDNSAnswer(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength:Word;AFamily:Word;var AType,AClass:Word;var ATtl:LongWord):Boolean;
var
 Offset:Word;
 Address:TInAddr;
 Address6:TIn6Addr;
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Answer (Count = ' + IntToStr(ACount) + ' Name = ' + PtrToHex(AName) + ' Data = ' + PtrToHex(AData) + ' Family = ' + IntToStr(AFamily) + ')');
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
       case AFamily of
        AF_INET:begin
          {IPv4}
          {If PTR convert Name to InAddr}
          Address:=NameToInAddr(PChar(AName));
          PInAddr(AName)^:=Address;
          ANameLength:=SizeOf(TInAddr);
          if not ExtractDNSRData(AMessage,Offset,AData,ADataLength,AType,AClass) then Exit;
         end;
        AF_INET6:begin 
          {IPv6}
          {If PTR convert Name to In6Addr}
          Address6:=NameToIn6Addr(PChar(AName));
          PIn6Addr(AName)^:=Address6;
          ANameLength:=SizeOf(TIn6Addr);
          if not ExtractDNSRData(AMessage,Offset,AData,ADataLength,AType,AClass) then Exit;
         end;
       end;
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

function TDNSClient.ExtractDNSAuthority(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength:Word;AFamily:Word;var AType,AClass:Word;var ATtl:LongWord):Boolean;
var
 Offset:Word;
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Authority (Count = ' + IntToStr(ACount) + ' Name = ' + PtrToHex(AName) + ' Data = ' + PtrToHex(AData) + ' Family = ' + IntToStr(AFamily) + ')');
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

function TDNSClient.ExtractDNSAdditional(AMessage:PDNSMessage;ACount:Word;AName,AData:Pointer;var ANameLength,ADataLength:Word;AFamily:Word;var AType,AClass:Word;var ATtl:LongWord):Boolean;
var
 Offset:Word;
 Resource:PDNSResource;
begin
 {}
 Result:=False;
 
 {$IFDEF DNS_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: Extracting DNS Additional (Count = ' + IntToStr(ACount) + ' Name = ' + PtrToHex(AName) + ' Data = ' + PtrToHex(AData) + ' Family = ' + IntToStr(AFamily) + ')');
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
 HostEntry:THostEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetHostByAddr (Addr = ' + PtrToHex(AAddr) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Check the Params}
  NetworkSetLastError(WSANO_RECOVERY);
  if AAddr = nil then Exit;

  {Get the Host}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  HostEntry:=ResolveHostByAddress(AAddr,ALength,AFamily); 
  if HostEntry = nil then Exit;

  {Return Host Entry if found}
  Result:=HostEntryToHostEnt(HostEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Host}
  HostEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetHostByName(AName:PChar;AFamily:Integer):PHostEnt;
{Performs a DNS_TYPE_A or DNS_TYPE_AAAA Query for the Address of the Domain name supplied}
{If Name is blank then get the addresses for the local machine}
{If Name is 'localhost' then get the loopback addresses for the local machine}
var
 Count:Integer;
 NameBuffer:String;
 HostEntry:THostEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
 IPAddressEntry:TIPAddressEntry;
 IP6AddressEntry:TIP6AddressEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetHostByName (Name = ' + AName + ' Family = ' + IntToStr(AFamily) + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Get the Transports}
  IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
  try
   {Check the Params}
   NetworkSetLastError(WSANO_RECOVERY);
   if AName = nil then Exit;

   {Check the Name}
   NameBuffer:=StripTrailingDot(AName);

   {Check for Blank}
   if (NameBuffer = '') or (Lowercase(NameBuffer) = '..localmachine') or (Lowercase(NameBuffer) = Lowercase(FProtocol.Manager.Settings.HostName)) then
    begin
     {Get Local Addresses}
     NetworkSetLastError(WSAHOST_NOT_FOUND);

     {Check the Family}
     if (AFamily = AF_INET) and (IPTransport <> nil) then
      begin
       {Check the Addresses}
       Count:=0;
       IPAddressEntry:=IPTransport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
       while IPAddressEntry <> nil do
        begin
         {Check Address Type}
         if (IPAddressEntry.AddressType = ADDRESS_TYPE_PRIMARY) or (IPAddressEntry.AddressType = ADDRESS_TYPE_SECONDARY) then
          begin
           Inc(Count);

           {Return Address Entry if found}
           Result:=AddressEntryToHostEnt(IPAddressEntry,FProtocol.Manager.Settings.HostName + AddLeadingDot(FProtocol.Manager.Settings.DomainName),Count);
           NetworkSetLastError(ERROR_SUCCESS);
          end;

         {Get Next}
         IPAddressEntry:=IPTransport.GetAddressByNext(IPAddressEntry,True,True,NETWORK_LOCK_READ);
        end;
      end;

     if (AFamily = AF_INET6) and (IP6Transport <> nil) then
      begin
       {Check the Addresses}
       Count:=0;
       IP6AddressEntry:=IP6Transport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
       while IP6AddressEntry <> nil do
        begin
         {Check Address Type}
         if (IP6AddressEntry.AddressType = ADDRESS_TYPE_PRIMARY) or (IP6AddressEntry.AddressType = ADDRESS_TYPE_SECONDARY) then
          begin
           Inc(Count);

           {Return Address Entry if found}
           Result:=AddressEntryToHostEnt(IP6AddressEntry,FProtocol.Manager.Settings.HostName + AddLeadingDot(FProtocol.Manager.Settings.DomainName),Count);
           NetworkSetLastError(ERROR_SUCCESS);
          end;

         {Get Next}
         IP6AddressEntry:=IP6Transport.GetAddressByNext(IP6AddressEntry,True,True,NETWORK_LOCK_READ);
        end;
      end;
    end
   else if Lowercase(NameBuffer) = 'localhost' then
    begin
     {Get Loopback Addresses}
     NetworkSetLastError(WSAHOST_NOT_FOUND);

     {Check the Family}
     if (AFamily = AF_INET) and (IPTransport <> nil) then
      begin
       {Check the Addresses}
       Count:=0;
       IPAddressEntry:=IPTransport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
       while IPAddressEntry <> nil do
        begin
         {Check Address Type}
         if IPAddressEntry.AddressType = ADDRESS_TYPE_LOOPBACK then
          begin
           Inc(Count);

           {Return Address Entry if found}
           Result:=AddressEntryToHostEnt(IPAddressEntry,'localhost',Count);
           NetworkSetLastError(ERROR_SUCCESS);
          end;

         {Get Next}
         IPAddressEntry:=IPTransport.GetAddressByNext(IPAddressEntry,True,True,NETWORK_LOCK_READ);
        end;
      end;

     if (AFamily = AF_INET6) and (IP6Transport <> nil) then
      begin
       {Check the Addresses}
       Count:=0;
       IP6AddressEntry:=IP6Transport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
       while IP6AddressEntry <> nil do
        begin
         {Check Address Type}
         if IP6AddressEntry.AddressType = ADDRESS_TYPE_LOOPBACK then
          begin
           Inc(Count);

           {Return Address Entry if found}
           Result:=AddressEntryToHostEnt(IP6AddressEntry,'localhost',Count);
           NetworkSetLastError(ERROR_SUCCESS);
          end;

         {Get Next}
         IP6AddressEntry:=IP6Transport.GetAddressByNext(IP6AddressEntry,True,True,NETWORK_LOCK_READ);
        end;
      end;
    end
   else
    begin
     {Get the Host}
     NetworkSetLastError(WSAHOST_NOT_FOUND);
     HostEntry:=ResolveHostByName(NameBuffer,AFamily); 
     if HostEntry = nil then Exit;

     {Return Host Entry if found}
     Result:=HostEntryToHostEnt(HostEntry);
     NetworkSetLastError(ERROR_SUCCESS);

     {Unlock Host}
     HostEntry.ReleaseLock;
    end;
  finally
   if IPTransport <> nil then IPTransport.ReaderUnlock;
   if IP6Transport <> nil then IP6Transport.ReaderUnlock;
  end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetHostName(AName:PChar;ALength:Integer):Integer;
var
 Name:String;
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

  {Get the Host Name}
  NetworkSetLastError(WSAEFAULT);
  if AName = nil then Exit;
  Name:=ResolveHostName;
  if ALength < Length(Name) then Exit;
  StrLCopy(AName,PChar(Name),ALength);

  {Return Result}
  Result:=NO_ERROR;
  NetworkSetLastError(ERROR_SUCCESS);
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetServByPort(APort:Integer;AProto:PChar):PServEnt;
{Note: Port will be in network order}
var
 ServEntry:TServEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetServByPort (Port = ' + IntToStr(WordBEToN(APort)) + ' Proto = ' + AProto + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Check the Params}
  NetworkSetLastError(WSANO_RECOVERY);
  if AProto = nil then Exit;

  {Get the Service}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  ServEntry:=ResolveServByPort(APort,AProto);
  if ServEntry = nil then Exit;

  {Return Service Entry if found}
  Result:=ServEntryToServEnt(ServEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Service}
  ServEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetServByName(AName,AProto:PChar):PServEnt;
var
 ServEntry:TServEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetServByName (Name = ' + AName + ' Proto = ' + AProto + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Check the Params}
  NetworkSetLastError(WSANO_RECOVERY);
  if AName = nil then Exit;
  if AProto = nil then Exit;

  {Get the Service}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  ServEntry:=ResolveServByName(AName,AProto); 
  if ServEntry = nil then Exit;

  {Return Service Entry if found}
  Result:=ServEntryToServEnt(ServEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Service}
  ServEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetProtoByNumber(AProto:Integer):PProtoEnt;
{Note: Protocol will be in host order}
var
 ProtoEntry:TProtoEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetProtoByNumber (Proto = ' + IntToStr(AProto) + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Get the Protocol}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  ProtoEntry:=ResolveProtoByNumber(AProto); 
  if ProtoEntry = nil then Exit;

  {Return Protocol Entry if found}
  Result:=ProtoEntryToProtoEnt(ProtoEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Protocol}
  ProtoEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetProtoByName(AName:PChar):PProtoEnt;
var
 ProtoEntry:TProtoEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetProtoByName (Name = ' + AName + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Check the Params}
  NetworkSetLastError(WSANO_RECOVERY);
  if AName = nil then Exit;

  {Get the Protocol}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  ProtoEntry:=ResolveProtoByName(AName);
  if ProtoEntry = nil then Exit;

  {Return Protocol Entry if found}
  Result:=ProtoEntryToProtoEnt(ProtoEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Protocol}
  ProtoEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetNetByAddr(AAddr:Pointer;ALength,AFamily:Integer):PNetEnt;
{Note: Address will be in network order where applicable}
var
 NetworkEntry:TNetworkEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetNetByAddr (Addr = ' + PtrToHex(AAddr) + ' Length = ' + IntToStr(ALength) + ' Family = ' + IntToStr(AFamily) + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Check the Params}
  NetworkSetLastError(WSANO_RECOVERY);
  if AAddr = nil then Exit;

  {Get the Network}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  NetworkEntry:=ResolveNetworkByAddress(AAddr,ALength,AFamily);
  if NetworkEntry = nil then Exit;

  {Return Network Entry if found}
  Result:=NetworkEntryToNetEnt(NetworkEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Network}
  NetworkEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetNetByName(AName:PChar;AFamily:Integer):PNetEnt;
var
 NetworkEntry:TNetworkEntry;
begin
 {}
 ReaderLock;
 try
  Result:=nil;

  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetNetByName (Name = ' + AName + ' Family = ' + IntToStr(AFamily) + ')');
  {$ENDIF}

  {Check the Protocol}
  NetworkSetLastError(WSAENETDOWN);
  if FProtocol = nil then Exit;

  {Check the Params}
  NetworkSetLastError(WSANO_RECOVERY);
  if AName = nil then Exit;

  {Get the Network}
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  NetworkEntry:=ResolveNetworkByName(AName,AFamily);
  if NetworkEntry = nil then Exit;

  {Return Network Entry if found}
  Result:=NetworkEntryToNetEnt(NetworkEntry);
  NetworkSetLastError(ERROR_SUCCESS);

  {Unlock Network}
  NetworkEntry.ReleaseLock;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetAddrInfo(ANodeName,AServName:PChar;AHints:PAddrInfo;var AAddrInfo:PAddrInfo):Integer;
{RFC 3493 protocol-independent translation from a host name to an address}
var
 Flags:LongInt;
 Family:LongInt;
 Protocol:LongInt;
 SocketType:LongInt;

 Port:Word;
 Addr:PInAddr;
 Host:TInAddr;
 Addr6:PIn6Addr;
 Host6:TIn6Addr;
 NextInfo:PAddrInfo;
 LastInfo:PAddrInfo;
 NameBuffer:String;
 WorkBuffer:String;
 HostEntry:THostEntry;
 ServEntry:TServEntry;
 IPTransport:TIPTransport;
 IP6Transport:TIP6Transport;
begin
 {}
 ReaderLock;
 try
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetAddrInfo (NodeName = ' + ANodeName + ' ServName = ' + AServName + ')');
  {$ENDIF}

  {Set Result}
  AAddrInfo:=nil;

  {Check Names}
  Result:=WSAHOST_NOT_FOUND;
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  if (ANodeName = nil) and (AServName = nil) then Exit;

  {Check Hints}
  if AHints = nil then
   begin
    Flags:=AI_V4MAPPED or AI_ADDRCONFIG; {Linux defaults / Windows defaults to 0}
    Family:=AF_UNSPEC;
    Protocol:=IPPROTO_IP;
    SocketType:=SOCK_UNSPEC;
   end
  else
   begin
    {Check Hints}
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    if AHints.ai_addrlen <> 0 then Exit;
    if AHints.ai_canonname <> nil then Exit;
    if AHints.ai_addr <> nil then Exit;
    if AHints.ai_next <> nil then Exit;

    {Get Hints}
    Flags:=AHints.ai_flags;
    Family:=AHints.ai_family;
    Protocol:=AHints.ai_protocol;
    SocketType:=AHints.ai_socktype;
   end;

  {Check Flags (Canonical Name)}
  if (Flags and AI_CANONNAME) = AI_CANONNAME then
   begin
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    if ANodeName = nil then Exit;
   end;

  {Check Flags (Numeric Host)}
  if (Flags and AI_NUMERICHOST) = AI_NUMERICHOST then
   begin
    Result:=WSAHOST_NOT_FOUND;
    NetworkSetLastError(WSAHOST_NOT_FOUND);
    if ANodeName = nil then Exit;
   end;

  {Check Flags (Numeric Service)}  
  if (Flags and AI_NUMERICSERV) = AI_NUMERICSERV then
   begin
    Result:=WSAHOST_NOT_FOUND;
    NetworkSetLastError(WSAHOST_NOT_FOUND);
    if AServName = nil then Exit;
   end;
  
  {Get Transports}
  IPTransport:=TIPTransport(FProtocol.Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ));
  IP6Transport:=TIP6Transport(FProtocol.Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ));
  try
   {Check Family}
   Result:=WSAEAFNOSUPPORT;
   NetworkSetLastError(WSAEAFNOSUPPORT);
   if (Family <> AF_UNSPEC) and (Family <> AF_INET) and (Family <> AF_INET6) then Exit;
   if (IPTransport = nil) and (Family = AF_INET) then Exit;
   if (IP6Transport = nil) and (Family = AF_INET6) then Exit;
   if Family = AF_UNSPEC then 
    begin
     {if (IPTransport <> nil) and (IP6Transport = nil) then Family:=AF_INET;} {Allow numeric host, bindable and connectable address even if protocol not enabled}
     {if (IPTransport = nil) and (IP6Transport <> nil) then Family:=AF_INET6;}
    end;
  finally
   if IPTransport <> nil then IPTransport.ReaderUnlock;
   if IP6Transport <> nil then IP6Transport.ReaderUnlock;
  end;

  {Check Socket Type}
  Result:=WSAESOCKTNOSUPPORT;
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if (SocketType <> SOCK_UNSPEC) and (SocketType <> SOCK_STREAM) and (SocketType <> SOCK_DGRAM) and (SocketType <> SOCK_RAW) then Exit;

  {Check Service Name}
  Port:=IPPORT_ANY;
  if AServName <> nil then
   begin
    {Check Flags (Numeric Service)}
    if (Flags and AI_NUMERICSERV) = AI_NUMERICSERV then
     begin
      {Convert Service}
      Result:=WSANO_RECOVERY;
      NetworkSetLastError(WSANO_RECOVERY);
      Port:=StrToIntDef(AServName,IPPORT_ANY);
      if Port = IPPORT_ANY then Exit;
     end
    else
     begin
      {Resolve Service}
      WorkBuffer:='TCP';
      if Protocol = IPPROTO_UDP then WorkBuffer:='UDP';

      ServEntry:=ResolveServByName(AServName,PChar(WorkBuffer));
      if ServEntry = nil then
       begin
        {Convert Service}
        Result:=WSATYPE_NOT_FOUND;
        NetworkSetLastError(WSATYPE_NOT_FOUND);
        Port:=StrToIntDef(AServName,IPPORT_ANY);
        if Port = IPPORT_ANY then Exit;
       end
      else
       begin
        Port:=ServEntry.Port;

        {Unlock Service}
        ServEntry.ReleaseLock;
       end;
     end;
   end;

  {Set Error}
  Result:=WSA_NOT_ENOUGH_MEMORY;
  NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY);

  {Check Node Name}
  if ANodeName <> nil then
   begin
    {Check the Name}
    NameBuffer:=StripTrailingDot(ANodeName);

    {Check Flags (Numeric Host)}
    if (Flags and AI_NUMERICHOST) = AI_NUMERICHOST then
     begin
      {Check Family (AF_INET)}
      if (Family = AF_INET) or (Family = AF_UNSPEC) then
       begin
        {Convert Host}
        Host:=StringToInAddr(NameBuffer);
        if (not(InAddrIsDefault(Host)) and not(InAddrIsNone(Host))) or (NameBuffer = INET_ADDRSTR_ANY) or (NameBuffer = INET_ADDRSTR_BROADCAST) then
         begin
          {Create Address Info}
          AAddrInfo:=AllocMem(SizeOf(TAddrInfo));
          if AAddrInfo = nil then Exit;

          {Update Address Info}
          AAddrInfo.ai_flags:=Flags;
          AAddrInfo.ai_family:=AF_INET; {Family}
          AAddrInfo.ai_socktype:=SocketType;
          AAddrInfo.ai_protocol:=Protocol;
          AAddrInfo.ai_addrlen:=SizeOf(TSockAddr);

          {Create Sock Address}
          AAddrInfo.ai_addr:=AllocMem(SizeOf(TSockAddr));
          if AAddrInfo.ai_addr = nil then Exit;

          {Return Sock Address}
          AAddrInfo.ai_addr.sin_family:=AF_INET; {Family}
          AAddrInfo.ai_addr.sin_port:=WordNtoBE(Port);
          AAddrInfo.ai_addr.sin_addr:=Host;
         end;
       end;

      {Check Family (AF_INET6)}
      if (Family = AF_INET6) or (Family = AF_UNSPEC) then
       begin
        {Convert Host}
        Host6:=StringToIn6Addr(NameBuffer);
        if (not(In6AddrIsDefault(Host6)) and not(In6AddrIsNone(Host6))) or (NameBuffer = INET6_ADDRSTR_ANY) then
         begin
          {Create Address Info}
          AAddrInfo:=AllocMem(SizeOf(TAddrInfo));
          if AAddrInfo = nil then Exit;

          {Update Address Info}
          AAddrInfo.ai_flags:=Flags;
          AAddrInfo.ai_family:=AF_INET6; {Family}
          AAddrInfo.ai_socktype:=SocketType;
          AAddrInfo.ai_protocol:=Protocol;
          AAddrInfo.ai_addrlen:=SizeOf(TSockAddr6);

          {Create Sock Address}
          AAddrInfo.ai_addr:=AllocMem(SizeOf(TSockAddr6));
          if AAddrInfo.ai_addr = nil then Exit;

          {Return Sock Address}
          PSockAddr6(AAddrInfo.ai_addr).sin6_family:=AF_INET6; {Family}
          PSockAddr6(AAddrInfo.ai_addr).sin6_port:=WordNtoBE(Port);
          PSockAddr6(AAddrInfo.ai_addr).sin6_addr:=Host6;
         end;
       end;
     end
    else
     begin
      {Check Family (AF_INET)}
      if (Family = AF_INET) or (Family = AF_UNSPEC) then
       begin
        {Check for Blank}
        if (NameBuffer = '') or (Lowercase(NameBuffer) = '..localmachine') or (Lowercase(NameBuffer) = Lowercase(FProtocol.Manager.Settings.HostName)) then
         begin
          {Get Local Addresses}
          AAddrInfo:=ResolveLocalAddrInfo(Flags,AF_INET,Protocol,SocketType,Port);

          {Don't check return, may be no local addresses}
         end
        else if Lowercase(NameBuffer) = 'localhost' then
         begin
          {Get Loopback Addresses}
          AAddrInfo:=ResolveLoopbackAddrInfo(Flags,AF_INET,Protocol,SocketType,Port);

          {Don't check return, may be no loopback addresses}
         end
        else
         begin
          {Resolve Host}
          HostEntry:=ResolveHostByName(NameBuffer,AF_INET);
          if HostEntry = nil then
           begin
            {Convert Host}
            Host:=StringToInAddr(NameBuffer);
            if (not(InAddrIsDefault(Host)) and not(InAddrIsNone(Host))) or (NameBuffer = INET_ADDRSTR_ANY) or (NameBuffer = INET_ADDRSTR_BROADCAST) then
             begin
              {Create Address Info}
              AAddrInfo:=AllocMem(SizeOf(TAddrInfo));
              if AAddrInfo = nil then Exit;

              {Update Address Info}
              AAddrInfo.ai_flags:=Flags;
              AAddrInfo.ai_family:=AF_INET; {Family}
              AAddrInfo.ai_socktype:=SocketType;
              AAddrInfo.ai_protocol:=Protocol;
              AAddrInfo.ai_addrlen:=SizeOf(TSockAddr);

              {Create Sock Address}
              AAddrInfo.ai_addr:=AllocMem(SizeOf(TSockAddr));
              if AAddrInfo.ai_addr = nil then Exit;

              {Return Sock Address}
              AAddrInfo.ai_addr.sin_family:=AF_INET; {Family}
              AAddrInfo.ai_addr.sin_port:=WordNtoBE(Port);
              AAddrInfo.ai_addr.sin_addr:=Host;
             end;
           end
          else
           begin
            {Return Host Entry}
            AAddrInfo:=HostEntryToAddrInfo(HostEntry,Flags,AF_INET,Protocol,SocketType,Port,(AAddrInfo = nil));

            {Unlock Host}
            HostEntry.ReleaseLock;

            if AAddrInfo = nil then Exit;
           end;
         end;
       end;

      {Check Family (AF_INET6)}
      if (Family = AF_INET6) or (Family = AF_UNSPEC) then
       begin
        {Check for Blank}
        if (NameBuffer = '') or (Lowercase(NameBuffer) = '..localmachine') or (Lowercase(NameBuffer) = Lowercase(FProtocol.Manager.Settings.HostName)) then
         begin
          {Get Local Addresses}
          NextInfo:=ResolveLocalAddrInfo(Flags,AF_INET6,Protocol,SocketType,Port);

          {Don't check return, may be no local addresses}

          {Check Last}
          LastInfo:=GetLastAddrInfo(AAddrInfo);
          if LastInfo = nil then AAddrInfo:=NextInfo else LastInfo.ai_next:=NextInfo;
         end
        else if Lowercase(NameBuffer) = 'localhost' then
         begin
          {Get Loopback Addresses}
          NextInfo:=ResolveLoopbackAddrInfo(Flags,AF_INET6,Protocol,SocketType,Port);

          {Don't check return, may be no loopback addresses}

          {Check Last}
          LastInfo:=GetLastAddrInfo(AAddrInfo);
          if LastInfo = nil then AAddrInfo:=NextInfo else LastInfo.ai_next:=NextInfo;
         end
        else
         begin
          {Resolve Host}
          HostEntry:=ResolveHostByName(NameBuffer,AF_INET6);
          if HostEntry = nil then
           begin
            {Convert Host}
            Host6:=StringToIn6Addr(NameBuffer);
            if (not(In6AddrIsDefault(Host6)) and not(In6AddrIsNone(Host6))) or (NameBuffer = INET6_ADDRSTR_ANY) then
             begin
              {Create Address Info}
              NextInfo:=AllocMem(SizeOf(TAddrInfo));
              if NextInfo = nil then Exit;

              {Update Address Info}
              NextInfo.ai_flags:=Flags;
              NextInfo.ai_family:=AF_INET6; {Family}
              NextInfo.ai_socktype:=SocketType;
              NextInfo.ai_protocol:=Protocol;
              NextInfo.ai_addrlen:=SizeOf(TSockAddr6);

              {Create Sock Address}
              NextInfo.ai_addr:=AllocMem(SizeOf(TSockAddr6));
              if NextInfo.ai_addr = nil then Exit;

              {Return Sock Address}
              PSockAddr6(NextInfo.ai_addr).sin6_family:=AF_INET6; {Family}
              PSockAddr6(NextInfo.ai_addr).sin6_port:=WordNtoBE(Port);
              PSockAddr6(NextInfo.ai_addr).sin6_addr:=Host6;

              {Check Last}
              LastInfo:=GetLastAddrInfo(AAddrInfo);
              if LastInfo = nil then AAddrInfo:=NextInfo else LastInfo.ai_next:=NextInfo;
             end;
           end
          else
           begin
            {Return Host Entry}
            NextInfo:=HostEntryToAddrInfo(HostEntry,Flags,AF_INET6,Protocol,SocketType,Port,(AAddrInfo = nil));

            {Unlock Host}
            HostEntry.ReleaseLock;

            if NextInfo = nil then Exit;

            {Check Last}
            LastInfo:=GetLastAddrInfo(AAddrInfo);
            if LastInfo = nil then AAddrInfo:=NextInfo else LastInfo.ai_next:=NextInfo;
           end;
         end;
       end;
     end;
   end
  else
   begin
    {Check Family (AF_INET)}
    if (Family = AF_INET) or (Family = AF_UNSPEC) then
     begin
      {Create Address Info}
      AAddrInfo:=AllocMem(SizeOf(TAddrInfo));
      if AAddrInfo = nil then Exit;

      {Update Address Info}
      AAddrInfo.ai_flags:=Flags;
      AAddrInfo.ai_family:=AF_INET; {Family}
      AAddrInfo.ai_socktype:=SocketType;
      AAddrInfo.ai_protocol:=Protocol;
      AAddrInfo.ai_addrlen:=SizeOf(TSockAddr);

      {Create Sock Address}
      AAddrInfo.ai_addr:=AllocMem(SizeOf(TSockAddr));
      if AAddrInfo.ai_addr = nil then Exit;

      {Check Flags (Passive)}
      if (Flags and AI_PASSIVE) = AI_PASSIVE then
       begin
        {Return Bindable Sock Address}
        AAddrInfo.ai_addr.sin_family:=AF_INET; {Family}
        AAddrInfo.ai_addr.sin_port:=WordNtoBE(Port);
        AAddrInfo.ai_addr.sin_addr.S_addr:=LongWordNtoBE(INADDR_ANY);
       end
      else
       begin
        {Return Connectable Sock Address}
        AAddrInfo.ai_addr.sin_family:=AF_INET; {Family}
        AAddrInfo.ai_addr.sin_port:=WordNtoBE(Port);
        AAddrInfo.ai_addr.sin_addr.S_addr:=LongWordNtoBE(INADDR_LOOPBACK);
       end;
     end;

    {Check Family (AF_INET6)}
    if (Family = AF_INET6) or (Family = AF_UNSPEC) then
     begin
      {Create Address Info}
      NextInfo:=AllocMem(SizeOf(TAddrInfo));
      if NextInfo = nil then Exit;

      {Update Address Info}
      NextInfo.ai_flags:=Flags;
      NextInfo.ai_family:=AF_INET6; {Family}
      NextInfo.ai_socktype:=SocketType;
      NextInfo.ai_protocol:=Protocol;
      NextInfo.ai_addrlen:=SizeOf(TSockAddr6);

      {Create Sock Address}
      NextInfo.ai_addr:=AllocMem(SizeOf(TSockAddr6));
      if NextInfo.ai_addr = nil then Exit;

      {Check Flags (Passive)}
      if (Flags and AI_PASSIVE) = AI_PASSIVE then
       begin
        {Return Bindable Sock Address}
        PSockAddr6(NextInfo.ai_addr).sin6_family:=AF_INET6; {Family}
        PSockAddr6(NextInfo.ai_addr).sin6_port:=WordNtoBE(Port);
        PSockAddr6(NextInfo.ai_addr).sin6_addr:=IN6ADDR_ANY_INIT;
       end
      else
       begin
        {Return Connectable Sock Address}
        PSockAddr6(NextInfo.ai_addr).sin6_family:=AF_INET6; {Family}
        PSockAddr6(NextInfo.ai_addr).sin6_port:=WordNtoBE(Port);
        PSockAddr6(NextInfo.ai_addr).sin6_addr:=IN6ADDR_LOOPBACK_INIT;
       end;     

      {Check Last}
      LastInfo:=GetLastAddrInfo(AAddrInfo);
      if LastInfo = nil then AAddrInfo:=NextInfo else LastInfo.ai_next:=NextInfo;
     end;
   end;

  {Check Result}
  if AAddrInfo = nil then
   begin
    Result:=WSAHOST_NOT_FOUND;
    NetworkSetLastError(WSAHOST_NOT_FOUND);
   end
  else
   begin
    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

function TDNSClient.GetNameInfo(AAddr:PSockAddr;AAddrLength:Integer;AHost:PChar;AHostLength:Integer;AServ:PChar;AServLength:Integer;AFlags:LongWord):Integer;
{RFC 3493 protocol-independent name resolution from an address to a host name and a port number to a service name}
var
 WorkBuffer:String;
 HostEntry:THostEntry;
 ServEntry:TServEntry;
begin
 {}
 ReaderLock;
 try
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: GetNameInfo (Addr = ' + PtrToHex(AAddr) + ' AddrLength = ' + IntToStr(AAddrLength) + ' HostLength = ' + IntToStr(AHostLength) + ' ServLength = ' + IntToStr(AServLength) + ' Flags = ' + IntToHex(AFlags,8) + ')');
  {$ENDIF}

  {Check Address}
  Result:=WSAEFAULT;
  NetworkSetLastError(WSAEFAULT);
  if AAddr = nil then Exit;
  
  {Check Names}
  Result:=WSAHOST_NOT_FOUND;
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  if (AHost = nil) and (AServ = nil) then Exit;

  {Check Name Lengths}
  Result:=WSAEINVAL;
  NetworkSetLastError(WSAEINVAL);
  if (AHost <> nil) and (AHostLength = 0) then Exit;
  if (AServ <> nil) and (AServLength = 0) then Exit;

  {Check Address Length}
  if AAddrLength >= SizeOf(TSockAddr6) then
   begin
    {Check AF_INET6}
    Result:=WSAEAFNOSUPPORT;
    NetworkSetLastError(WSAEAFNOSUPPORT);
    if PSockAddr6(AAddr).sin6_family <> AF_INET6 then Exit;

    {Check Host}
    if AHost <> nil then
     begin
      {Check Flags}
      if (AFlags and NI_NUMERICHOST) = NI_NUMERICHOST then
       begin
        {Convert Address}
        WorkBuffer:=In6AddrToString(PSockAddr6(AAddr).sin6_addr);

        {Check Host Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if AHostLength < Length(WorkBuffer) then Exit;

        StrLCopy(AHost,PChar(WorkBuffer),AHostLength);
       end
      else
       begin      
        {Resolve Address}
        HostEntry:=ResolveHostByAddress(@PSockAddr6(AAddr).sin6_addr,SizeOf(TIn6Addr),AF_INET6);
        if HostEntry = nil then
         begin
          Result:=WSAHOST_NOT_FOUND;
          NetworkSetLastError(WSAHOST_NOT_FOUND);
          if (AFlags and NI_NAMEREQD) = NI_NAMEREQD then Exit;

          {Convert Address}
          WorkBuffer:=In6AddrToString(PSockAddr6(AAddr).sin6_addr);

          {Check Host Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if AHostLength < Length(WorkBuffer) then Exit;

          StrLCopy(AHost,PChar(WorkBuffer),AHostLength);
         end
        else
         begin
          StrLCopy(AHost,PChar(HostEntry.Name),AHostLength);

          {Unlock Host}
          HostEntry.ReleaseLock;
         end;

        //To Do //Check Flag NI_NOFQDN
       end; 
     end;

    {Check Service}
    if AServ <> nil then
     begin
      {Check Flags}
      if (AFlags and NI_NUMERICSERV) = NI_NUMERICSERV then 
       begin
        {Convert Service}
        WorkBuffer:=IntToStr(WordBEtoN(PSockAddr6(AAddr).sin6_port));

        {Check Service Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if AServLength < Length(WorkBuffer) then Exit;

        StrLCopy(AServ,PChar(WorkBuffer),AServLength);
       end
      else
       begin
        {Resolve Service}
        WorkBuffer:='TCP';
        if (AFlags and NI_DGRAM) = NI_DGRAM then WorkBuffer:='UDP';

        ServEntry:=ResolveServByPort(PSockAddr6(AAddr).sin6_port,WorkBuffer);
        if ServEntry = nil then
         begin
          {Convert Service}
          WorkBuffer:=IntToStr(WordBEtoN(PSockAddr6(AAddr).sin6_port));

          {Check Service Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if AServLength < Length(WorkBuffer) then Exit;

          StrLCopy(AServ,PChar(WorkBuffer),AServLength);
         end
        else
         begin
          StrLCopy(AServ,PChar(ServEntry.Name),AServLength);

          {Unlock Service}
          ServEntry.ReleaseLock;
         end;
       end;       
     end;

    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end
  else if AAddrLength >= SizeOf(TSockAddr) then 
   begin
    {Check AF_INET}
    Result:=WSAEAFNOSUPPORT;
    NetworkSetLastError(WSAEAFNOSUPPORT);
    if PSockAddr(AAddr).sin_family <> AF_INET then Exit;

    {Check Host}
    if AHost <> nil then
     begin
      {Check Flags}
      if (AFlags and NI_NUMERICHOST) = NI_NUMERICHOST then
       begin
        {Convert Address}
        WorkBuffer:=InAddrToString(PSockAddr(AAddr).sin_addr);

        {Check Host Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if AHostLength < Length(WorkBuffer) then Exit;

        StrLCopy(AHost,PChar(WorkBuffer),AHostLength);
       end
      else
       begin
        {Resolve Address}
        HostEntry:=ResolveHostByAddress(@PSockAddr(AAddr).sin_addr,SizeOf(TInAddr),AF_INET);
        if HostEntry = nil then
         begin
          Result:=WSAHOST_NOT_FOUND;
          NetworkSetLastError(WSAHOST_NOT_FOUND);
          if (AFlags and NI_NAMEREQD) = NI_NAMEREQD then Exit;

          {Convert Address}
          WorkBuffer:=InAddrToString(PSockAddr(AAddr).sin_addr);

          {Check Host Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if AHostLength < Length(WorkBuffer) then Exit;

          StrLCopy(AHost,PChar(WorkBuffer),AHostLength);
         end
        else
         begin
          StrLCopy(AHost,PChar(HostEntry.Name),AHostLength);

          {Unlock Host}
          HostEntry.ReleaseLock;
         end;

        //To Do //Check NI_NOFQDN
       end; 
     end;

    {Check Service}
    if AServ <> nil then
     begin
      {Check Flags}
      if (AFlags and NI_NUMERICSERV) = NI_NUMERICSERV then 
       begin
        {Convert Service}
        WorkBuffer:=IntToStr(WordBEtoN(PSockAddr(AAddr).sin_port));

        {Check Service Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if AServLength < (Length(WorkBuffer) + 1) then Exit;

        StrLCopy(AServ,PChar(WorkBuffer),AServLength);
       end
      else
       begin
        {Resolve Service}
        WorkBuffer:='TCP';
        if (AFlags and NI_DGRAM) = NI_DGRAM then WorkBuffer:='UDP';

        ServEntry:=ResolveServByPort(PSockAddr(AAddr).sin_port,WorkBuffer);
        if ServEntry = nil then
         begin
          {Convert Service}
          WorkBuffer:=IntToStr(WordBEtoN(PSockAddr(AAddr).sin_port));

          {Check Service Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if AServLength < (Length(WorkBuffer) + 1) then Exit;

          StrLCopy(AServ,PChar(WorkBuffer),AServLength);
         end
        else
         begin
          StrLCopy(AServ,PChar(ServEntry.Name),AServLength);
          
          {Unlock Service}
          ServEntry.ReleaseLock;
         end;
       end;       
     end;

    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end;
 finally
  ReaderUnlock;
 end;
end;

{==============================================================================}

procedure TDNSClient.FreeAddrInfo(AAddrInfo:PAddrInfo);
{Free address information that GetAddrInfo dynamically allocates in TAddrInfo structures}
begin
 {}
 ReaderLock;
 try
  {$IFDEF DNS_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DNS: FreeAddrInfo (AddrInfo = ' + PtrToHex(AAddrInfo) + ')');
  {$ENDIF}

  {Check Addr}
  NetworkSetLastError(WSAEFAULT);
  if AAddrInfo = nil then Exit;

  {Check Next}
  if AAddrInfo.ai_next <> nil then
   begin
    {Free Next}
    FreeAddrInfo(AAddrInfo.ai_next);
   end;

  {Free Name}
  if AAddrInfo.ai_canonname <> nil then
   begin
    FreeMem(AAddrInfo.ai_canonname);
   end;

  {Free Addr}
  if AAddrInfo.ai_addr <> nil then
   begin
    FreeMem(AAddrInfo.ai_addr);
   end;

  {Free Addr}
  FreeMem(AAddrInfo);

  NetworkSetLastError(ERROR_SUCCESS);
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
 