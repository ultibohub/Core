{
Ultibo IP Helper interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

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


IP Helper
=========

 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Iphlpapi;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Threads,SysUtils,Winsock2;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {IP Helper specific constants}
 MAX_INTERFACE_NAME_LEN         = 256;
 IPRTRMGR_PID                   = 10000;
 ANY_SIZE                       = 1;

 MAX_ADAPTER_NAME               = 128;

 MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
 MAX_ADAPTER_NAME_LENGTH        = 256;
 MAX_ADAPTER_ADDRESS_LENGTH     = 8;
 DEFAULT_MINIMUM_ENTITIES       = 32;
 MAX_HOSTNAME_LEN               = 128;
 MAX_DOMAIN_NAME_LEN            = 128;
 MAX_SCOPE_ID_LEN               = 256;

 MAXLEN_IFDESCR                     = 256;
 MAXLEN_PHYSADDR                    = 8;

 MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
 MIB_IF_OPER_STATUS_UNREACHABLE     = 1;
 MIB_IF_OPER_STATUS_DISCONNECTED    = 2;
 MIB_IF_OPER_STATUS_CONNECTING      = 3;
 MIB_IF_OPER_STATUS_CONNECTED       = 4;
 MIB_IF_OPER_STATUS_OPERATIONAL     = 5;

 MIB_IF_TYPE_OTHER                  = 1;
 MIB_IF_TYPE_ETHERNET               = 6;
 MIB_IF_TYPE_TOKENRING              = 9;
 MIB_IF_TYPE_FDDI                   = 15;
 MIB_IF_TYPE_PPP                    = 23;
 MIB_IF_TYPE_LOOPBACK               = 24;
 MIB_IF_TYPE_SLIP                   = 28;

 MIB_IF_ADMIN_STATUS_UP             = 1;
 MIB_IF_ADMIN_STATUS_DOWN           = 2;
 MIB_IF_ADMIN_STATUS_TESTING        = 3;

const
 MIB_TCP_RTO_OTHER    = 1;
 MIB_TCP_RTO_CONSTANT = 2;
 MIB_TCP_RTO_RSRE     = 3;
 MIB_TCP_RTO_VANJ     = 4;
 MIB_TCP_MAXCONN_DYNAMIC = DWORD(-1);

const
 MIB_TCP_STATE_CLOSED     = 1;
 MIB_TCP_STATE_LISTEN     = 2;
 MIB_TCP_STATE_SYN_SENT   = 3;
 MIB_TCP_STATE_SYN_RCVD   = 4;
 MIB_TCP_STATE_ESTAB      = 5;
 MIB_TCP_STATE_FIN_WAIT1  = 6;
 MIB_TCP_STATE_FIN_WAIT2  = 7;
 MIB_TCP_STATE_CLOSE_WAIT = 8;
 MIB_TCP_STATE_CLOSING    = 9;
 MIB_TCP_STATE_LAST_ACK   = 10;
 MIB_TCP_STATE_TIME_WAIT  = 11;
 MIB_TCP_STATE_DELETE_TCB = 12;

const
 MIB_USE_CURRENT_TTL        = DWORD(-1);
 MIB_USE_CURRENT_FORWARDING = DWORD(-1);

const
 MIB_IP_FORWARDING     = 1;
 MIB_IP_NOT_FORWARDING = 2;

const
 MIB_IPROUTE_TYPE_OTHER    = 1;
 MIB_IPROUTE_TYPE_INVALID  = 2;
 MIB_IPROUTE_TYPE_DIRECT   = 3;
 MIB_IPROUTE_TYPE_INDIRECT = 4;
 MIB_IPROUTE_METRIC_UNUSED = DWORD(-1);

const
 MIB_IPPROTO_OTHER   = 1;
 MIB_IPPROTO_LOCAL   = 2;
 MIB_IPPROTO_NETMGMT = 3;
 MIB_IPPROTO_ICMP    = 4;
 MIB_IPPROTO_EGP     = 5;
 MIB_IPPROTO_GGP     = 6;
 MIB_IPPROTO_HELLO   = 7;
 MIB_IPPROTO_RIP     = 8;
 MIB_IPPROTO_IS_IS   = 9;
 MIB_IPPROTO_ES_IS   = 10;
 MIB_IPPROTO_CISCO   = 11;
 MIB_IPPROTO_BBN     = 12;
 MIB_IPPROTO_OSPF    = 13;
 MIB_IPPROTO_BGP     = 14;
 MIB_IPPROTO_NT_AUTOSTATIC     = 10002;
 MIB_IPPROTO_NT_STATIC         = 10006;
 MIB_IPPROTO_NT_STATIC_NON_DOD = 10007;

const
 MIB_IPNET_TYPE_OTHER   = 1;
 MIB_IPNET_TYPE_INVALID = 2;
 MIB_IPNET_TYPE_DYNAMIC = 3;
 MIB_IPNET_TYPE_STATIC  = 4;

{==============================================================================}
type
 {IP Helper specific types}
 IPAddr = Cardinal;     {An IP address}
 IPMask = Cardinal;     {An IP netmask}
 IP_STATUS = Cardinal;  {Status code returned from IP APIs}
 
type
 PMIB_IFNUMBER = ^MIB_IFNUMBER;
 MIB_IFNUMBER = record
   dwValue: DWORD;
 end;
 TMibIfnumber = MIB_IFNUMBER;
 PMibIfnumber = PMIB_IFNUMBER;

 PMIB_IFROW = ^MIB_IFROW;
 MIB_IFROW = record
   wszName: array [0..MAX_INTERFACE_NAME_LEN - 1] of WideChar;
   dwIndex: DWORD;
   dwType: DWORD;
   dwMtu: DWORD;
   dwSpeed: DWORD;
   dwPhysAddrLen: DWORD;
   bPhysAddr: array [0..MAXLEN_PHYSADDR - 1] of Byte;
   dwAdminStatus: DWORD;
   dwOperStatus: DWORD;
   dwLastChange: DWORD;
   dwInOctets: DWORD;
   dwInUcastPkts: DWORD;
   dwInNUcastPkts: DWORD;
   dwInDiscards: DWORD;
   dwInErrors: DWORD;
   dwInUnknownProtos: DWORD;
   dwOutOctets: DWORD;
   dwOutUcastPkts: DWORD;
   dwOutNUcastPkts: DWORD;
   dwOutDiscards: DWORD;
   dwOutErrors: DWORD;
   dwOutQLen: DWORD;
   dwDescrLen: DWORD;
   bDescr: array[0..MAXLEN_IFDESCR - 1] of Byte;
 end;
 TMibIfRow = MIB_IFROW;
 PMibIfRow = PMIB_IFROW;

 PMIB_IFTABLE = ^MIB_IFTABLE;
 MIB_IFTABLE = record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_IFROW;
 end;
 TMibIftable = MIB_IFTABLE;
 PMibIftable = PMIB_IFTABLE;

 MIBICMPSTATS = record
   dwMsgs: DWORD;
   dwErrors: DWORD;
   dwDestUnreachs: DWORD;
   dwTimeExcds: DWORD;
   dwParmProbs: DWORD;
   dwSrcQuenchs: DWORD;
   dwRedirects: DWORD;
   dwEchos: DWORD;
   dwEchoReps: DWORD;
   dwTimestamps: DWORD;
   dwTimestampReps: DWORD;
   dwAddrMasks: DWORD;
   dwAddrMaskReps: DWORD;
 end;
 TMibIcmpStats = MIBICMPSTATS;
 PMibIcmpStats = ^TMibIcmpStats;

 MIBICMPINFO = record
   icmpInStats: MIBICMPSTATS;
   icmpOutStats: MIBICMPSTATS;
 end;
 TMibIcmpInfo = MIBICMPINFO;
 PMibIcmpInfo = ^TMibIcmpInfo;

 PMIB_ICMP = ^MIB_ICMP;
 MIB_ICMP = record
   stats: MIBICMPINFO;
 end;
 TMibIcmp = MIB_ICMP;
 PMibIcmp = PMIB_ICMP;

 PMIB_UDPSTATS = ^MIB_UDPSTATS;
 MIB_UDPSTATS = record
   dwInDatagrams: DWORD;
   dwNoPorts: DWORD;
   dwInErrors: DWORD;
   dwOutDatagrams: DWORD;
   dwNumAddrs: DWORD;
 end;
 TMibUdpStats = MIB_UDPSTATS;
 PMibUdpStats = PMIB_UDPSTATS;

 PMIB_UDPROW = ^MIB_UDPROW;
 MIB_UDPROW = record
   dwLocalAddr: DWORD;
   dwLocalPort: DWORD;
 end;
 TMibUdpRow = MIB_UDPROW;
 PMibUdpRow = PMIB_UDPROW;

 PMIB_UDPTABLE = ^MIB_UDPTABLE;
 MIB_UDPTABLE = record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_UDPROW;
 end;
 TMibUdpTable = MIB_UDPTABLE;
 PMibUdpTable = PMIB_UDPTABLE;

 PMIB_TCPSTATS = ^MIB_TCPSTATS;
 MIB_TCPSTATS = record
   dwRtoAlgorithm: DWORD;
   dwRtoMin: DWORD;
   dwRtoMax: DWORD;
   dwMaxConn: DWORD;
   dwActiveOpens: DWORD;
   dwPassiveOpens: DWORD;
   dwAttemptFails: DWORD;
   dwEstabResets: DWORD;
   dwCurrEstab: DWORD;
   dwInSegs: DWORD;
   dwOutSegs: DWORD;
   dwRetransSegs: DWORD;
   dwInErrs: DWORD;
   dwOutRsts: DWORD;
   dwNumConns: DWORD;
 end;
 TMibTcpStats = MIB_TCPSTATS;
 PMibTcpStats = PMIB_TCPSTATS;

 PMIB_TCPROW = ^MIB_TCPROW;
 MIB_TCPROW = record
   dwState: DWORD;
   dwLocalAddr: DWORD;
   dwLocalPort: DWORD;
   dwRemoteAddr: DWORD;
   dwRemotePort: DWORD;
 end;
 TMibTcpRow = MIB_TCPROW;
 PMibTcpRow = PMIB_TCPROW;

 PMIB_TCPTABLE = ^MIB_TCPTABLE;
 MIB_TCPTABLE = record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_TCPROW;
 end;
 TMibTcpTable = MIB_TCPTABLE;
 PMibTcpTable = PMIB_TCPTABLE;

 PMIB_IPSTATS = ^MIB_IPSTATS;
 MIB_IPSTATS = record
   dwForwarding: DWORD;
   dwDefaultTTL: DWORD;
   dwInReceives: DWORD;
   dwInHdrErrors: DWORD;
   dwInAddrErrors: DWORD;
   dwForwDatagrams: DWORD;
   dwInUnknownProtos: DWORD;
   dwInDiscards: DWORD;
   dwInDelivers: DWORD;
   dwOutRequests: DWORD;
   dwRoutingDiscards: DWORD;
   dwOutDiscards: DWORD;
   dwOutNoRoutes: DWORD;
   dwReasmTimeout: DWORD;
   dwReasmReqds: DWORD;
   dwReasmOks: DWORD;
   dwReasmFails: DWORD;
   dwFragOks: DWORD;
   dwFragFails: DWORD;
   dwFragCreates: DWORD;
   dwNumIf: DWORD;
   dwNumAddr: DWORD;
   dwNumRoutes: DWORD;
 end;
 TMibIpStats = MIB_IPSTATS;
 PMibIpStats = PMIB_IPSTATS;

 PMIB_IPADDRROW = ^MIB_IPADDRROW;
 MIB_IPADDRROW = record
   dwAddr: DWORD;
   dwIndex: DWORD;
   dwMask: DWORD;
   dwBCastAddr: DWORD;
   dwReasmSize: DWORD;
   unused1: Word;
   unused2: Word;
 end;
 TMibIpAddrRow = MIB_IPADDRROW;
 PMibIpAddrRow = PMIB_IPADDRROW;

 PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
 MIB_IPADDRTABLE = record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_IPADDRROW;
 end;
 TMibIpAddrTable = MIB_IPADDRTABLE;
 PMibIpAddrTable = PMIB_IPADDRTABLE;

 PMIB_IPFORWARDNUMBER = ^MIB_IPFORWARDNUMBER;
 MIB_IPFORWARDNUMBER = record
   dwValue: DWORD;
 end;
 TMibIpForwardNumber = MIB_IPFORWARDNUMBER;
 PMibIpForwardNumber = PMIB_IPFORWARDNUMBER;

 PMIB_IPFORWARDROW = ^MIB_IPFORWARDROW;
 MIB_IPFORWARDROW = record
   dwForwardDest: DWORD;
   dwForwardMask: DWORD;
   dwForwardPolicy: DWORD;
   dwForwardNextHop: DWORD;
   dwForwardIfIndex: DWORD;
   dwForwardType: DWORD;
   dwForwardProto: DWORD;
   dwForwardAge: DWORD;
   dwForwardNextHopAS: DWORD;
   dwForwardMetric1: DWORD;
   dwForwardMetric2: DWORD;
   dwForwardMetric3: DWORD;
   dwForwardMetric4: DWORD;
   dwForwardMetric5: DWORD;
 end;
 TMibIpForwardRow = MIB_IPFORWARDROW;
 PMibIpForwardRow = PMIB_IPFORWARDROW;

 PMIB_IPFORWARDTABLE = ^MIB_IPFORWARDTABLE;
 MIB_IPFORWARDTABLE = record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_IPFORWARDROW;
 end;
 TMibIpForwardTable = MIB_IPFORWARDTABLE;
 PMibIpForwardTable = PMIB_IPFORWARDTABLE;

 PMIB_IPNETROW = ^MIB_IPNETROW;
 MIB_IPNETROW = record
   dwIndex: DWORD;
   dwPhysAddrLen: DWORD;
   bPhysAddr: array [0..MAXLEN_PHYSADDR - 1] of BYTE;
   dwAddr: DWORD;
   dwType: DWORD;
 end;
 TMibIpNetRow = MIB_IPNETROW;
 PMibIpNetRow = PMIB_IPNETROW;

 PMIB_IPNETTABLE = ^MIB_IPNETTABLE;
 MIB_IPNETTABLE = record
   dwNumEntries: DWORD;
   table: array [0..ANY_SIZE - 1] of MIB_IPNETROW;
 end;
 TMibIpNetTable = MIB_IPNETTABLE;
 PMibIpNetTable = PMIB_IPNETTABLE;

 PIP_ADAPTER_INDEX_MAP = ^IP_ADAPTER_INDEX_MAP;
 IP_ADAPTER_INDEX_MAP = record
   Index: ULONG;
   Name: array [0..MAX_ADAPTER_NAME - 1] of WideChar;
 end;
 TIpAdapterIndexMap = IP_ADAPTER_INDEX_MAP;
 PIpAdapterIndexMap = PIP_ADAPTER_INDEX_MAP;

 PIP_INTERFACE_INFO = ^IP_INTERFACE_INFO;
 IP_INTERFACE_INFO = record
   NumAdapters: Longint;
   Adapter: array [0..0] of IP_ADAPTER_INDEX_MAP;
 end;
 TIpInterfaceInfo = IP_INTERFACE_INFO;
 PIpInterfaceInfo = PIP_INTERFACE_INFO;

 PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS = ^IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
 IP_UNIDIRECTIONAL_ADAPTER_ADDRESS = record
   NumAdapters: ULONG;
   Address: array [0..0] of IPAddr;
 end;
 TIpUnidirectionalAdapterAddress = IP_UNIDIRECTIONAL_ADAPTER_ADDRESS;
 PIpUnidirectionalAdapterAddress = PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS;

 PIP_MASK_STRING = ^IP_MASK_STRING;
 IP_ADDRESS_STRING = record
   S: array [0..15] of Char;
 end;
 PIP_ADDRESS_STRING = ^IP_ADDRESS_STRING;
 IP_MASK_STRING = IP_ADDRESS_STRING;
 TIpAddressString = IP_ADDRESS_STRING;
 PIpAddressString = PIP_MASK_STRING;

 PIP_ADDR_STRING = ^IP_ADDR_STRING;
 IP_ADDR_STRING = record
   Next: PIP_ADDR_STRING;
   IpAddress: IP_ADDRESS_STRING;
   IpMask: IP_MASK_STRING;
   Context: DWORD;
 end;
 TIpAddrString = IP_ADDR_STRING;
 PIpAddrString = PIP_ADDR_STRING;

 PIP_ADAPTER_INFO = ^IP_ADAPTER_INFO;
 IP_ADAPTER_INFO = record
   Next: PIP_ADAPTER_INFO;
   ComboIndex: DWORD;
   AdapterName: array [0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
   Description: array [0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
   AddressLength: UINT;
   Address: array [0..MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
   Index: DWORD;
   Type_: UINT;
   DhcpEnabled: UINT;
   CurrentIpAddress: PIP_ADDR_STRING;
   IpAddressList: IP_ADDR_STRING;
   GatewayList: IP_ADDR_STRING;
   DhcpServer: IP_ADDR_STRING;
   HaveWins: BOOL;
   PrimaryWinsServer: IP_ADDR_STRING;
   SecondaryWinsServer: IP_ADDR_STRING;
   LeaseObtained: LongInt;  {time_t}
   LeaseExpires: LongInt;   {time_t}
 end;
 TIpAdapterInfo = IP_ADAPTER_INFO;
 PIpAdapterInfo = PIP_ADAPTER_INFO;

 PIP_PER_ADAPTER_INFO = ^IP_PER_ADAPTER_INFO;
 IP_PER_ADAPTER_INFO = record
   AutoconfigEnabled: UINT;
   AutoconfigActive: UINT;
   CurrentDnsServer: PIP_ADDR_STRING;
   DnsServerList: IP_ADDR_STRING;
 end;
 TIpPerAdapterInfo = IP_PER_ADAPTER_INFO;
 PIpPerAdapterInfo = PIP_PER_ADAPTER_INFO;

 PFIXED_INFO = ^FIXED_INFO;
 FIXED_INFO = record
   HostName: array [0..MAX_HOSTNAME_LEN + 3] of Char;
   DomainName: array[0..MAX_DOMAIN_NAME_LEN + 3] of Char;
   CurrentDnsServer: PIP_ADDR_STRING;
   DnsServerList: IP_ADDR_STRING;
   NodeType: UINT;
   ScopeId: array [0..MAX_SCOPE_ID_LEN + 3] of Char;
   EnableRouting: UINT;
   EnableProxy: UINT;
   EnableDns: UINT;
 end;
 TFixedInfo = FIXED_INFO;
 PFixedInfo = PFIXED_INFO;
 
{==============================================================================}
{var}
 {IP Helper specific variables}
 
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{IP Helper Functions}
function GetNumberOfInterfaces(var pdwNumIf: DWORD): DWORD;
function GetIfEntry(pIfRow: PMIB_IFROW): DWORD;
function GetIfTable(pIfTable: PMIB_IFTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
function GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
function GetIpNetTable(pIpNetTable: PMIB_IPNETTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
function GetIpForwardTable(pIpForwardTable: PMIB_IPFORWARDTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
function GetTcpTable(pTcpTable: PMIB_TCPTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
function GetUdpTable(pUdpTable: PMIB_UDPTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;

function GetIpStatistics(var pStats: MIB_IPSTATS): DWORD;
function GetIcmpStatistics(var pStats: MIB_ICMP): DWORD;
function GetTcpStatistics(var pStats: MIB_TCPSTATS): DWORD;
function GetUdpStatistics(var pStats: MIB_UDPSTATS): DWORD;

function SetIfEntry(const pIfRow: MIB_IFROW): DWORD;
function CreateIpForwardEntry(const pRoute: MIB_IPFORWARDROW): DWORD;
function SetIpForwardEntry(const pRoute: MIB_IPFORWARDROW): DWORD;
function DeleteIpForwardEntry(const pRoute: MIB_IPFORWARDROW): DWORD;

function SetIpStatistics(const pIpStats: MIB_IPSTATS): DWORD;
function SetIpTTL(nTTL: UINT): DWORD;

function CreateIpNetEntry(const pArpEntry: MIB_IPNETROW): DWORD;
function SetIpNetEntry(const pArpEntry: MIB_IPNETROW): DWORD;
function DeleteIpNetEntry(const pArpEntry: MIB_IPNETROW): DWORD;
function FlushIpNetTable(dwIfIndex: DWORD): DWORD;

function CreateProxyArpEntry(dwAddress, dwMask, dwIfIndex: DWORD): DWORD;
function DeleteProxyArpEntry(dwAddress, dwMask, dwIfIndex: DWORD): DWORD;

function SetTcpEntry(const pTcpRow: MIB_TCPROW): DWORD;
function GetInterfaceInfo(pIfTable: PIP_INTERFACE_INFO; var dwOutBufLen: DWORD): DWORD;
function GetUniDirectionalAdapterInfo(pIPIfInfo: PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS; var dwOutBufLen: DWORD): DWORD;

function GetBestInterface(dwDestAddr: IPAddr; var pdwBestIfIndex: DWORD): DWORD;
function GetBestRoute(dwDestAddr, dwSourceAddr: DWORD; pBestRoute: PMIB_IPFORWARDROW): DWORD;
function NotifyAddrChange(var Handle: THandle; overlapped: POVERLAPPED): DWORD;
function NotifyRouteChange(var Handle: THandle; overlapped: POVERLAPPED): DWORD;
function GetAdapterIndex(AdapterName: LPWSTR; var IfIndex: DWORD): DWORD;
function AddIPAddress(Address: IPAddr; IpMask: IPMask; IfIndex: DWORD; var NTEContext, NTEInstance: DWORD): DWORD;
function DeleteIPAddress(NTEContext: DWORD): DWORD;
function GetNetworkParams(pFixedInfo: PFIXED_INFO; var pOutBufLen: DWORD): DWORD;
function GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: DWORD): DWORD;
function GetPerAdapterInfo(IfIndex: DWORD; pPerAdapterInfo: PIP_PER_ADAPTER_INFO; var pOutBufLen: DWORD): DWORD;
function IpReleaseAddress(const AdapterInfo: IP_ADAPTER_INDEX_MAP): DWORD;
function IpRenewAddress(const AdapterInfo: IP_ADAPTER_INDEX_MAP): DWORD;
function SendARP(const DestIP, SrcIP: IPAddr; pMacAddr: PDWORD; var PhyAddrLen: DWORD): DWORD;
function GetRTTAndHopCount(DestIpAddress: IPAddr; var HopCount: DWORD; MaxHops: DWORD; var RTT: DWORD): BOOL;
function GetFriendlyIfIndex(IfIndex: DWORD): DWORD;
function EnableRouter(var pHandle: THandle; pOverlapped: POVERLAPPED): DWORD;
function UnenableRouter(pOverlapped: POVERLAPPED; lpdwEnableCount: LPDWORD): DWORD;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {IP Helper specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{IP Helper Functions}
function GetNumberOfInterfaces(var pdwNumIf: DWORD): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function GetIfEntry(pIfRow: PMIB_IFROW): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function GetIfTable(pIfTable: PMIB_IFTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
var
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Result:=WsControlEx(IPPROTO_IP,WSA_GETIFTABLE,pIfTable,pdwSize,pIfTable,pdwSize);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
var
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Result:=WsControlEx(IPPROTO_IP,WSA_GETIPADDRTABLE,pIpAddrTable,pdwSize,pIpAddrTable,pdwSize);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function GetIpNetTable(pIpNetTable: PMIB_IPNETTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function GetIpForwardTable(pIpForwardTable: PMIB_IPFORWARDTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function GetTcpTable(pTcpTable: PMIB_TCPTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetUdpTable(pUdpTable: PMIB_UDPTABLE; var pdwSize: DWORD; bOrder: BOOL): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetIpStatistics(var pStats: MIB_IPSTATS): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetIcmpStatistics(var pStats: MIB_ICMP): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetTcpStatistics(var pStats: MIB_TCPSTATS): DWORD;
begin
 {Not Implemented} 
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetUdpStatistics(var pStats: MIB_UDPSTATS): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function SetIfEntry(const pIfRow: MIB_IFROW): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function CreateIpForwardEntry(const pRoute: MIB_IPFORWARDROW): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function SetIpForwardEntry(const pRoute: MIB_IPFORWARDROW): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function DeleteIpForwardEntry(const pRoute: MIB_IPFORWARDROW): DWORD;
begin
 {}
 Result:=NO_ERROR;
 //To Do
end;

{==============================================================================}

function SetIpStatistics(const pIpStats: MIB_IPSTATS): DWORD;
var
 Size:DWORD;
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Size:=SizeOf(MIB_IPSTATS);
   
   Result:=WsControlEx(IPPROTO_IP,WSA_SETIPSTATISTICS,@pIpStats,Size,@pIpStats,Size);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function SetIpTTL(nTTL: UINT): DWORD;
var
 Size:DWORD;
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Size:=SizeOf(UINT);
   
   Result:=WsControlEx(IPPROTO_IP,WSA_SETIPTTL,@nTTL,Size,@nTTL,Size);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function CreateIpNetEntry(const pArpEntry: MIB_IPNETROW): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function SetIpNetEntry(const pArpEntry: MIB_IPNETROW): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function DeleteIpNetEntry(const pArpEntry: MIB_IPNETROW): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function FlushIpNetTable(dwIfIndex: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function CreateProxyArpEntry(dwAddress, dwMask, dwIfIndex: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function DeleteProxyArpEntry(dwAddress, dwMask, dwIfIndex: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function SetTcpEntry(const pTcpRow: MIB_TCPROW): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetInterfaceInfo(pIfTable: PIP_INTERFACE_INFO; var dwOutBufLen: DWORD): DWORD;
var
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Result:=WsControlEx(IPPROTO_IP,WSA_GETINTERFACEINFO,pIfTable,dwOutBufLen,pIfTable,dwOutBufLen);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function GetUniDirectionalAdapterInfo(pIPIfInfo: PIP_UNIDIRECTIONAL_ADAPTER_ADDRESS; var dwOutBufLen: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetBestInterface(dwDestAddr: IPAddr; var pdwBestIfIndex: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetBestRoute(dwDestAddr, dwSourceAddr: DWORD; pBestRoute: PMIB_IPFORWARDROW): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function NotifyAddrChange(var Handle: THandle; overlapped: POVERLAPPED): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function NotifyRouteChange(var Handle: THandle; overlapped: POVERLAPPED): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetAdapterIndex(AdapterName: LPWSTR; var IfIndex: DWORD): DWORD;
begin
 {}
 Result:=ERROR_INVALID_FUNCTION;
 //To Do
end;

{==============================================================================}

function AddIPAddress(Address: IPAddr; IpMask: IPMask; IfIndex: DWORD; var NTEContext, NTEInstance: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function DeleteIPAddress(NTEContext: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetNetworkParams(pFixedInfo: PFIXED_INFO; var pOutBufLen: DWORD): DWORD;
var
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Result:=WsControlEx(IPPROTO_IP,WSA_GETNETWORKPARAMS,pFixedInfo,pOutBufLen,pFixedInfo,pOutBufLen);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function GetAdaptersInfo(pAdapterInfo: PIP_ADAPTER_INFO; var pOutBufLen: DWORD): DWORD;
var
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Result:=WsControlEx(IPPROTO_IP,WSA_GETADAPTERSINFO,pAdapterInfo,pOutBufLen,pAdapterInfo,pOutBufLen);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function GetPerAdapterInfo(IfIndex: DWORD; pPerAdapterInfo: PIP_PER_ADAPTER_INFO; var pOutBufLen: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function IpReleaseAddress(const AdapterInfo: IP_ADAPTER_INDEX_MAP): DWORD;
var
 Size:DWORD;
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Size:=SizeOf(IP_ADAPTER_INDEX_MAP);
   
   Result:=WsControlEx(IPPROTO_IP,WSA_IPRELEASEADDRESS,@AdapterInfo,Size,@AdapterInfo,Size);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function IpRenewAddress(const AdapterInfo: IP_ADAPTER_INDEX_MAP): DWORD;
var
 Size:DWORD;
 WSAData:TWSAData;
begin
 {}
 FillChar(WSAData,SizeOf(TWSAData),0);
 Result:=WSAStartup(WINSOCK_VERSION,WSAData);
 if Result = ERROR_SUCCESS then
  begin
   Size:=SizeOf(IP_ADAPTER_INDEX_MAP);
   
   Result:=WsControlEx(IPPROTO_IP,WSA_IPRENEWADDRESS,@AdapterInfo,Size,@AdapterInfo,Size);
   
   WSACleanup;
  end;
end;

{==============================================================================}

function SendARP(const DestIP, SrcIP: IPAddr; pMacAddr: PDWORD; var PhyAddrLen: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function GetRTTAndHopCount(DestIpAddress: IPAddr; var HopCount: DWORD; MaxHops: DWORD; var RTT: DWORD): BOOL;
begin
 {Not Implemented}
 Result:=False;
end;

{==============================================================================}

function GetFriendlyIfIndex(IfIndex: DWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function EnableRouter(var pHandle: THandle; pOverlapped: POVERLAPPED): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}

function UnenableRouter(pOverlapped: POVERLAPPED; lpdwEnableCount: LPDWORD): DWORD;
begin
 {Not Implemented}
 Result:=ERROR_INVALID_FUNCTION;
end;

{==============================================================================}
{==============================================================================}

initialization
 {Nothing}

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
