{
Ultibo DHCP/BOOTP Protocol client unit.

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

 
DHCP/BOOTP Protocol
===================

 Notes: Also contains BOOTP, ARP (Psuedo), RARP, Static and Loopback config clients
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DHCP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,SysUtils,Classes,Network,Transport,Protocol,IP,IPv6,ARP,UDP,Ultibo,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {DHCP specific constants}
 {BOOTP/DHCP Constants}
 BOOTP_DELAY   = 1000;  {Previously 0}
 BOOTP_TIMEOUT = 8000;  {Previously 4000} {We wait for 8 seconds for a BOOTP reply}
 BOOTP_RETRIES = 6;     {Previously 4}    {Try the request 6 times}

 BOOTP_MIN_DELAY   = 0;
 BOOTP_MAX_DELAY   = 10000;
 BOOTP_MIN_TIMEOUT = 500;
 BOOTP_MAX_TIMEOUT = 15000;
 BOOTP_MIN_RETRIES = 1;
 BOOTP_MAX_RETRIES = 100;

 DHCP_DELAY   = 1000;   {Previously 0}
 DHCP_TIMEOUT = 8000;   {Previously 4000} {We wait for 8 seconds for a DHCP reply}
 DHCP_RETRIES = 6;      {Previously 4}    {Try the request 6 times}

 DHCP_MIN_DELAY   = 0;
 DHCP_MAX_DELAY   = 10000;
 DHCP_MIN_TIMEOUT = 500;
 DHCP_MAX_TIMEOUT = 15000;
 DHCP_MIN_RETRIES = 1;
 DHCP_MAX_RETRIES = 100;

 BOOTP_VENDOR_SIZE = 64;  {Size of the Vendor area}

 DHCP_OPTIONS_SIZE = 312; {Size of the Options area}
 DHCP_MESSAGE_SIZE = 576; {Size of the max message}

 {BOOTP/DHCP Opcodes (Request/Reply)}
 BOOTP_REQUEST   = 1;
 BOOTP_REPLY     = 2;

 {DHCP Messages (Request/Reply) (Option DHCP_MSG)}
 DHCP_DISCOVER = 1;
 DHCP_OFFER = 2;
 DHCP_REQUEST = 3;
 DHCP_DECLINE = 4;
 DHCP_ACK = 5;
 DHCP_NAK = 6;
 DHCP_RELEASE = 7;
 DHCP_INFORM = 8;

 DHCP_RENEWAL_TO = 1;
 DHCP_REBIND_TO = 2;

 {BOOTP/DHCP Options}
 PAD_OPT                  =  0;
 END_OPT                  = 255;
 SUBNET_MASK              =  1;
 TIME_OFFSET              =  2;
 ROUTERS_ON_SNET          =  3;
 TIME_SRV                 =  4;
 NAME_SRV                 =  5;
 DNS_SRV                  =  6;
 LOG_SRV                  =  7;
 COOKIE_SRV               =  8;
 LPR_SRV                  =  9;
 IMPRESS_SRV              = 10;
 RES_LOCATION_SRV         = 11;
 HOST_NAME                = 12;
 BOOT_FSIZE               = 13;
 MERIT_DUMPFILE           = 14;
 DOMAIN_NAME              = 15;
 SWAP_SRV                 = 16;
 ROOT_PATH                = 17;
 EXTENTIONS_PATH          = 18;
 IP_FORWARDING            = 19;
 NON_LOCAL_SRC_ROUTE      = 20;
 POLICY_FILTER            = 21;
 MAX_DGRAM_REASM_SIZE     = 22;
 IP_DEFAULT_TTL           = 23;
 PATH_MTU_AGING_TIMEOUT   = 24;
 PATH_MTU_PLATEAU_TABLE   = 25;
 IF_MTU                   = 26;
 ALL_SUBNETS_LOCAL        = 27;
 BROADCAST_ADDR           = 28;
 PERFORM_MASK_DISCOVERY   = 29;
 MASK_SUPPLIER            = 30;
 PERFORM_ROUTER_DISCOVERY = 31;
 ROUTER_SOLICITATION_ADDR = 32;
 STATIC_ROUTE             = 33;
 TRAILER_ENCAPSULATION    = 34;
 ARP_CACHE_TIMEOUT        = 35;
 ETHERNET_ENCAPSULATION   = 36;
 TCP_DEFAULT_TTL          = 37;
 TCP_KEEPALIVE_INTERVAL   = 38;
 TCP_KEEPALIVE_GARBAGE    = 39;
 NIS_DOMAIN_NAME          = 40;
 NIS_SRVS                 = 41;
 NTP_SRVS                 = 42;
 VENDOR_SPECIFIC_INFO     = 43;
 NBIOS_NAME_SRV           = 44;
 NBIOS_DGRAM_DIST_SRV     = 45;
 NBIOS_NODE_TYPE          = 46;
 NBIOS_SCOPE              = 47;
 XFONT_SRV                = 48;
 XDISPLAY_MANAGER         = 49;
 {DHCP Options}
 DHCP_REQUESTED_IP_ADDR   = 50;
 DHCP_IP_ADDR_LEASE_TIME  = 51;
 DHCP_OPT_OVERLOAD        = 52;
 DHCP_MSG_TYPE            = 53;
 DHCP_SRV_IDENTIFIER      = 54;
 DHCP_PARAM_REQUEST       = 55;
 DHCP_MSG                 = 56;
 DHCP_MAX_MSG_SIZE        = 57;
 DHCP_T1_VALUE            = 58;
 DHCP_T2_VALUE            = 59;
 DHCP_CLASS_ID            = 60;
 DHCP_CLIENT_ID           = 61;
 DHCP_NIS_DOMAIN_OPT      = 64;
 DHCP_NIS_SRV_OPT         = 65;
 DHCP_TFTP_SERVER         = 66;
 DHCP_BOOT_FILENAME       = 67;
 DHCP_MOBIP_HOME_AGENTS   = 68;
 DHCP_SMTP_SRVS           = 69;
 DHCP_POP3_SRVS           = 70;
 DHCP_NNTP_SRVS           = 71;
 DHCP_WWW_SRVS            = 72;
 DHCP_FINGER_SRVS         = 73;
 DHCP_IRC_SRVS            = 74;
 DHCP_STREET_TALK_SRVS    = 75;
 DHCP_STDA_SRVS           = 76;

 {DHCP Flags}
 DHCP_FLAG_BROADCAST  = $8000;

 {ARP Config Constants}
 ARP_CONFIG_START = $C0A86401;  {192.168.100.1}
 ARP_CONFIG_STOP =  $C0A864FE;  {192.168.100.254}

{==============================================================================}
type
 {DHCP specific types}
 PBOOTPHeader = ^TBOOTPHeader;
 TBOOTPHeader = packed record
  Opcode:Byte;                                         {packet op code / message type.}
  HardwareType:Byte;                                   {hardware address type, 1 = 10 mb ethernet}
  HardwareLength:Byte;                                 {hardware address len, eg '6' for 10mb eth}
  Hops:Byte;                                           {client sets to zero, optionally used by gateways in cross-gateway booting.}
  Identifier:LongWord;                                 {transaction ID, a random number}
  Seconds:Word;                                        {filled in by client, seconds elapsed since client started trying to boot.}
  Reserved:Word;
  ClientIP:TInAddr;                                    {client IP address filled in by client if known}
  YourIP:TInAddr;                                      {'your' (client) IP address filled by server if client doesn't know}
  ServerIP:TInAddr;                                    {server IP address returned in bootreply}
  GatewayIP:TInAddr;                                   {gateway IP address, used in optional cross-gateway booting.}
  {ClientHardware:array[0..15] of Byte;}               {client hardware address, filled by client}
  ClientHardware:THardwareAddress;                     {client hardware address, filled by client}
  DummyData:array[0..9] of Byte;                       {dummy to fill out remaining bytes of above}
  ServerName:array[0..63] of Byte;                     {optional server host name, null terminated}
  FileName:array[0..127] of Byte;                      {boot file name, null terminated string 'generic' name or null in bootrequest, fully qualified directory-path name in bootreply.}
  {VendorData:array[0..63] of Byte;}                   {optional vendor-specific area}
  VendorData:array[0..BOOTP_VENDOR_SIZE - 1] of Byte;  {optional vendor-specific area}
 end;

 PDHCPHeader = ^TDHCPHeader;
 TDHCPHeader = packed record
  Opcode:Byte;                {packet op code / message type.}
  HardwareType:Byte;          {hardware address type, 1 = 10 mb ethernet}
  HardwareLength:Byte;        {hardware address len, eg '6' for 10mb eth}
  Hops:Byte;                  {client sets to zero, optionally used by gateways in cross-gateway booting.}
  Identifier:LongWord;        {transaction ID, a random number}
  Seconds:Word;               {filled in by client, seconds elapsed since client started trying to boot.}
  Flags:Word;
  ClientIP:TInAddr;           {client IP address filled in by client if known}
  YourIP:TInAddr;             {'your' (client) IP address filled by server if client doesn't know}
  ServerIP:TInAddr;           {server IP address returned in bootreply}
  GatewayIP:TInAddr;          {gateway IP address, used in optional cross-gateway booting.}
  {ClientHardware:array[0..15] of Byte;}  {client hardware address, filled by client}
  ClientHardware:THardwareAddress;  {client hardware address, filled by client}
  DummyData:array[0..9] of Byte;    {dummy to fill out remaining bytes of above}
  ServerName:array[0..63] of Byte;  {optional server host name, null terminated}
  FileName:array[0..127] of Byte;   {boot file name, null terminated string 'generic' name or null in bootrequest, fully qualified directory-path name in bootreply.}
  {Options:array[0..311] of Byte;}    {DHCP options area (minimum 312 bytes)}
  Options:array[0..DHCP_OPTIONS_SIZE - 1] of Byte; {DHCP options area (minimum 312 bytes)}
 end;

 PDHCPClientId = ^TDHCPClientId;
 TDHCPClientId = packed record
  Tag:Byte;
  Hardware:THardwareAddress;
 end;
 
{==============================================================================}
type
 {DHCP specific classes}
 TDHCPConfigTransport = class(TConfigTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TDHCPConfig = class(TNetworkConfig) {DHCP config client}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FARP:TARPTransport;
   FUDP:TUDPProtocol;

   {Status Variables}

   {Internal Methods}
   function ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

   function GetDHCPClientId(AAdapter:TTransportAdapter):TDHCPClientId;

   function GetDHCPHeaderSize(AHeader:PDHCPHeader):Integer;

   function CreateDHCPRequest(AHeader:PDHCPHeader;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function CheckDHCPReply(AHeader:PDHCPHeader;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord):Boolean;
   function HandleDHCPReply(AHeader:PDHCPHeader;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

   function SendDHCPDiscover(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPRequest(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPDecline(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPRelease(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPInform(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPRenew(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPRebind(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function SendDHCPReboot(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;

   function RecvDHCPReply(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACommand:Word):Boolean;

   function InsertDHCPOption(AOption:Byte;AHeader:PDHCPHeader;AValue:Pointer;ASize:Integer):Boolean;
   function ExtractDHCPOption(AOption:Byte;AHeader:PDHCPHeader;AValue:Pointer;var ASize:Integer):Boolean;
  protected
   {Inherited Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
  public
   {Public Properties}

   {Public Methods}
   function StartConfig:Boolean; override;
   function StopConfig:Boolean; override;
   function ProcessConfig:Boolean; override;

   function SetConfig(AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean; override;
 end;

 TBOOTPConfigTransport = class(TConfigTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TBOOTPConfig = class(TNetworkConfig) {BOOTP config client}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FARP:TARPTransport;
   FUDP:TUDPProtocol;

   {Status Variables}

   {Internal Methods}
   function ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

   function CreateBOOTPRequest(AHeader:PBOOTPHeader;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function CheckBOOTPReply(AHeader:PBOOTPHeader;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord):Boolean;
   function HandleBOOTPReply(AHeader:PBOOTPHeader;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter):Boolean;

   function SendBOOTPRequest(ASocket:TProtocolSocket;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
   function RecvBOOTPReply(ASocket:TProtocolSocket;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord):Boolean;

   function ExtractBOOTPOption(AOption:Byte;AHeader:PBOOTPHeader;AValue:Pointer;var ASize:Integer):Boolean;
  protected
   {Inherited Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
  public
   {Public Properties}

   {Public Methods}
   function StartConfig:Boolean; override;
   function StopConfig:Boolean; override;
   function ProcessConfig:Boolean; override;

   function SetConfig(AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean; override;
 end;

 TARPConfigTransport = class(TConfigTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TARPConfig = class(TNetworkConfig)  {Psuedo dynamic ARP config client}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FARP:TARPTransport;

   {Status Variables}

   {Event Methods}

   {Internal Methods}
   function ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

  protected
   {Inherited Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
  public
   {Public Properties}

   {Public Methods}
   function StartConfig:Boolean; override;
   function StopConfig:Boolean; override;
   function ProcessConfig:Boolean; override;
 end;

 TRARPConfigTransport = class(TConfigTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TRARPConfig = class(TNetworkConfig) {RARP config client}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FRARP:TRARPTransport;

   {Status Variables}

   {Event Methods}

   {Internal Methods}
   function ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

  protected
   {Inherited Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
  public
   {Public Properties}

   {Public Methods}
   function StartConfig:Boolean; override;
   function StopConfig:Boolean; override;
   function ProcessConfig:Boolean; override;
 end;

 TStaticConfigTransport = class(TConfigTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TStaticConfig = class(TNetworkConfig) {Static config client}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FARP:TARPTransport;

   {Status Variables}

   {Event Methods}

   {Internal Methods}
   function ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

  protected
   {Inherited Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
  public
   {Public Properties}

   {Public Methods}
   function StartConfig:Boolean; override;
   function StopConfig:Boolean; override;
   function ProcessConfig:Boolean; override;
 end;

 TLoopbackConfigTransport = class(TConfigTransport)
  private
   {Internal Variables}

  public
   {Status Variables}

 end;

 TLoopbackConfig = class(TNetworkConfig) {Loopback config client}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}

   {Event Methods}

   {Internal Methods}
   function ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;

  protected
   {Inherited Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
  public
   {Public Properties}

   {Public Methods}
   function StartConfig:Boolean; override;
   function StopConfig:Boolean; override;
   function ProcessConfig:Boolean; override;
 end;
 
{==============================================================================}
{var}
 {DHCP specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure DHCPInit;

{==============================================================================}
{DHCP Functions}
  
{==============================================================================}
{DHCP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {DHCP specific variables}
 DHCPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TDHCPConfig}
constructor TDHCPConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create(AManager);
 FConfigType:=CONFIG_TYPE_DHCP;
 FInitDelay:=DHCP_DELAY;
 FRetryCount:=DHCP_RETRIES;
 FRetryTimeout:=DHCP_TIMEOUT;
 FARP:=nil;
 FUDP:=nil;
end;

{==============================================================================}

destructor TDHCPConfig.Destroy;
begin
 {}
 WriterLock;
 try
  FUDP:=nil;
  FARP:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TDHCPConfig.ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Process a network config request from a Transport}
{Handle: The Handle of the Config Transport the request is from}
{Adapter: The transport adapter to perform configuration on}
{Command: The configuration command to perform}

{Note: DHCP Handler is structured slightly different to others to account for the multiple types of requests it services}
{Note: Caller must hold the Adapter lock}
var
 Count:Integer;
 Attempt:Integer;
 Timeout:Integer;
 Socket:TUDPSocket;
 SockAddr:TSockAddr;
 Identifier:LongWord;
 Transport:TDHCPConfigTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: ConfigHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig:  Command = ' + ConfigCommandToString(ACommand));
 {$ENDIF}

 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get Transport}
 Transport:=TDHCPConfigTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check ARP Transport}
  if FARP = nil then Exit;
  
  {Check UDP Protocol}
  if FUDP = nil then Exit;
  
  {Check Address Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Check Command}
     case ACommand of
      {Note: DHCP does not accept REQUEST from Transport (Only internally)}
      CONFIG_ADAPTER_DISCOVER:begin
        {Perform Discover}
        {$IFDEF DHCP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Attempting DHCP Configuration - Discover');
        {$ENDIF}
        
        {Set Configuring}
        AAdapter.Configuring:=True;
        try
         {Delay Init}
         Sleep(FInitDelay); 
         
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Add the ARP Address}
         FARP.LoadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,AAdapter.Hardware,ADDRESS_TYPE_LOCAL);
         
         {Add the Address}
         TIPTransport(Transport.Transport).AddAddress(IP_DEFAULT_ADDRESS,AAdapter.Adapter,ADDRESS_TYPE_PRIMARY,False,NETWORK_LOCK_NONE);
         
         {Add the Route}
         TIPTransport(Transport.Transport).AddRoute(IP_BROADCAST_NETWORK,IP_BROADCAST_NETMASK,IP_DEFAULT_ADDRESS,IP_DEFAULT_ADDRESS,ROUTE_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);
         try
          Randomize;
          
          {Create the Socket}
          Socket:=TUDPSocket(FUDP.Socket(AF_INET,SOCK_DGRAM,IPPROTO_UDP));
          if TSocket(Socket) = INVALID_SOCKET then Exit;
          
          {Lock Socket}
          Socket.ReaderLock;
          try
           {Set the Options}
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_REUSEADDR,'1111',4) = SOCKET_ERROR then Exit;
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_BROADCAST,'1111',4) = SOCKET_ERROR then Exit;
           
           {Bind the Socket}
           SockAddr.sin_family:=AF_INET;
           SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPC);
           SockAddr.sin_addr.S_addr:=INADDR_ANY;
           if FUDP.Bind(Socket,SockAddr,SizeOf(TSockAddr)) = SOCKET_ERROR then Exit;
           
           {Set the Timeout}
           Timeout:=FRetryTimeout;
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) = SOCKET_ERROR then Exit;
           
           {Send the Request}
           Attempt:=0;
           while Attempt < Integer(FRetryCount) do {Attempt < DHCP_RETRIES} {Modified 29/8/2011}
            begin
             {Get Identifier}
             Identifier:=Random($7FFFFFFF);
            
             {Send Discover}
             if SendDHCPDiscover(Socket,Transport,AAdapter,Identifier,Attempt) then
              begin
               {Receive Reply}
               if RecvDHCPReply(Socket,Transport,AAdapter,Identifier,ACommand) then
                begin
                 {Send the Request using the same Identifier}
                 Count:=0;
                 while Count < Integer(FRetryCount) do {Count < DHCP_RETRIES} {Modified 29/8/2011}
                  begin
                   {Send Request}
                   if SendDHCPRequest(Socket,Transport,AAdapter,Identifier,Attempt) then
                    begin
                     {Receive Reply}
                     Result:=RecvDHCPReply(Socket,Transport,AAdapter,Identifier,CONFIG_ADAPTER_REQUEST);
                     if Result then Exit;
                    end;
                   
                   {Increment Count}
                   Inc(Count);
                  end;
                end;
              end;
              
             {Increment Attempt} 
             Inc(Attempt);
            end;
          finally
           {Close the Socket}
           FUDP.CloseSocket(Socket);
           
           {Unlock Socket}
           Socket.ReaderUnlock;
          end;
         finally
          {Remove the Route}
          TIPTransport(Transport.Transport).RemoveRoute(IP_BROADCAST_NETWORK,IP_DEFAULT_ADDRESS);
          
          {Remove the Address}
          TIPTransport(Transport.Transport).RemoveAddress(IP_DEFAULT_ADDRESS);
          
          {Remove the ARP Address}
          FARP.UnloadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS);
         end;
        finally
         {Reset Configuring}
         AAdapter.Configuring:=False;
        end;
       end;
      CONFIG_ADAPTER_REBOOT:begin
        {Perform Reboot}
        {$IFDEF DHCP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Attempting DHCP Configuration - Reboot');
        {$ENDIF}
        
        {Set Configuring}
        AAdapter.Configuring:=True;
        try
         {Delay Init}
         Sleep(FInitDelay); 
         
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Check the Address}
         if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Address) then Exit;
         
         {Add the ARP Address}
         FARP.LoadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,AAdapter.Hardware,ADDRESS_TYPE_LOCAL);
         
         {Add the Address}
         TIPTransport(Transport.Transport).AddAddress(IP_DEFAULT_ADDRESS,AAdapter.Adapter,ADDRESS_TYPE_PRIMARY,False,NETWORK_LOCK_NONE);
         
         {Add the Route}
         TIPTransport(Transport.Transport).AddRoute(IP_BROADCAST_NETWORK,IP_BROADCAST_NETMASK,IP_DEFAULT_ADDRESS,IP_DEFAULT_ADDRESS,ROUTE_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);
         try
          Randomize;
          
          {Create the Socket}
          Socket:=TUDPSocket(FUDP.Socket(AF_INET,SOCK_DGRAM,IPPROTO_UDP));
          if TSocket(Socket) = INVALID_SOCKET then Exit;
          
          {Lock Socket}
          Socket.ReaderLock;
          try
           {Set the Options}
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_REUSEADDR,'1111',4) = SOCKET_ERROR then Exit;
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_BROADCAST,'1111',4) = SOCKET_ERROR then Exit;
           
           {Bind the Socket}
           SockAddr.sin_family:=AF_INET;
           SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPC);
           SockAddr.sin_addr.S_addr:=INADDR_ANY;
           if FUDP.Bind(Socket,SockAddr,SizeOf(TSockAddr)) = SOCKET_ERROR then Exit;
           
           {Set the Timeout}
           Timeout:=FRetryTimeout;
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) = SOCKET_ERROR then Exit;
           
           {Send the Request}
           Count:=0;
           while Count < Integer(FRetryCount) do {Count < DHCP_RETRIES} {Modified 29/8/2011}
            begin
             {Get Identifier}
             Identifier:=Random($7FFFFFFF);
             
             {Send Reboot}
             if SendDHCPReboot(Socket,Transport,AAdapter,Identifier,Count) then
              begin
               {Receive Reply}
               Result:=RecvDHCPReply(Socket,Transport,AAdapter,Identifier,ACommand);
               if Result then Exit;
              end;
             
             {Increment Count}
             Inc(Count);
            end;
          finally
           {Close the Socket}
           FUDP.CloseSocket(Socket);
           
           {Unlock Socket}
           Socket.ReaderUnlock;
          end;
         finally
          {Remove the Route}
          TIPTransport(Transport.Transport).RemoveRoute(IP_BROADCAST_NETWORK,IP_DEFAULT_ADDRESS);
          
          {Remove the Address}
          TIPTransport(Transport.Transport).RemoveAddress(IP_DEFAULT_ADDRESS);
          
          {Remove the ARP Address}
          FARP.UnloadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS);
         end;
        finally
         {Reset Configuring}
         AAdapter.Configuring:=False;
        end;
       end;
      CONFIG_ADAPTER_RELEASE,CONFIG_ADAPTER_RENEW,CONFIG_ADAPTER_REBIND,CONFIG_ADAPTER_INFORM:begin
        {Perform Release, Renew, Rebind or Inform}
        {$IFDEF DHCP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Attempting DHCP Configuration - Release/Renew/Rebind/Inform');
        {$ENDIF}
        
        {Set Configuring}
        AAdapter.Configuring:=True;
        try
         Randomize;
         
         {Create the Socket}
         Socket:=TUDPSocket(FUDP.Socket(AF_INET,SOCK_DGRAM,IPPROTO_UDP));
         if TSocket(Socket) = INVALID_SOCKET then Exit;
         
         {Lock Socket}
         Socket.ReaderLock;
         try
          {Set the Options}
          if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_REUSEADDR,'1111',4) = SOCKET_ERROR then Exit;
          if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_BROADCAST,'1111',4) = SOCKET_ERROR then Exit;
          
          {Bind the Socket}
          SockAddr.sin_family:=AF_INET;
          SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPC);
          SockAddr.sin_addr.S_addr:=INADDR_ANY;
          if FUDP.Bind(Socket,SockAddr,SizeOf(TSockAddr)) = SOCKET_ERROR then Exit;
          
          {Set the Timeout}
          Timeout:=FRetryTimeout;
          if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) = SOCKET_ERROR then Exit;
          
          {Send the Request}
          Count:=0;
          while Count < Integer(FRetryCount) do {Count < DHCP_RETRIES} {Modified 29/8/2011}
           begin
            {Get Identifier}
            Identifier:=Random($7FFFFFFF);
            
            {Check Command}
            case ACommand of
             CONFIG_ADAPTER_RELEASE:begin
               {Send Release}
               Result:=SendDHCPRelease(Socket,Transport,AAdapter,Identifier,Count);
               if Result then Exit;
              end;
             CONFIG_ADAPTER_RENEW:begin
               {Send Renew}
               if SendDHCPRenew(Socket,Transport,AAdapter,Identifier,Count) then
                begin
                 {Receive Reply}
                 Result:=RecvDHCPReply(Socket,Transport,AAdapter,Identifier,ACommand);
                 if Result then Exit;
                end;
              end;
             CONFIG_ADAPTER_REBIND:begin
               {Send Rebind}
               if SendDHCPRebind(Socket,Transport,AAdapter,Identifier,Count) then
                begin
                 {Receive Reply}
                 Result:=RecvDHCPReply(Socket,Transport,AAdapter,Identifier,ACommand);
                 if Result then Exit;
                end;
              end;
             CONFIG_ADAPTER_INFORM:begin
               {Send Inform}
               if SendDHCPInform(Socket,Transport,AAdapter,Identifier,Count) then
                begin
                 {Receive Reply}
                 Result:=RecvDHCPReply(Socket,Transport,AAdapter,Identifier,ACommand);
                 if Result then Exit;
                end;
              end;
            end;
            
            {Increment Count}
            Inc(Count);
           end;
         finally
          {Close the Socket}
          FUDP.CloseSocket(Socket);
          
          {Unlock Socket}
          Socket.ReaderUnlock;
         end;
        finally
         {Reset Configuring}
         AAdapter.Configuring:=False;
        end;
       end;
     end;
    end;
   AF_INET6:begin
   
     //To Do
     
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDHCPConfig.GetDHCPClientId(AAdapter:TTransportAdapter):TDHCPClientId;
{Note: Client Id is just Hardware address prefixed by 01}
{Note: Caller must hold the Adapter lock}
begin
 {}
 FillChar(Result,SizeOf(TDHCPClientId),0);
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Return Result}
 Result.Tag:=1;
 Result.Hardware:=AAdapter.Hardware;
end;

{==============================================================================}

function TDHCPConfig.GetDHCPHeaderSize(AHeader:PDHCPHeader):Integer;
{Return size of Header and any Options it contains}
var
 Size:Integer;
 Count:Integer;
begin
 {}
 Result:=0;
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Get Header Size}
 Result:=SizeOf(TDHCPHeader) - DHCP_OPTIONS_SIZE;
 
 {Interate the Options starting after Magic Cookie}
 Count:=4;
 while Count < DHCP_OPTIONS_SIZE do
  begin
   {Check for End Option}
   if AHeader.Options[Count] = END_OPT then Break;
   
   {Check for Pad Option}
   if AHeader.Options[Count] = PAD_OPT then
    begin
     {Move to Next Option}
     Inc(Count,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Count + 1) > (DHCP_OPTIONS_SIZE - 1) then Break;
     
     {Get the Length}
     Size:=AHeader.Options[Count + 1];
     
     {Move to Next Option}
     Inc(Count,Size + 2);
    end;
  end;
  
 {Return Result}
 Result:=Result + Min(Count + 1,DHCP_OPTIONS_SIZE);
end;

{==============================================================================}

function TDHCPConfig.CreateDHCPRequest(AHeader:PDHCPHeader;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{The first four octets of the 'options' field of the DHCP message contain the (decimal) values 99, 130, 83 and 99, respectively}
{This is the same magic cookie as is defined in RFC 1497 [17]}

{Note: Caller must hold the Transport and Adapter locks}
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Creating DHCP Request');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Transport}
 {if ATransport = nil then Exit;} {Not Used}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Zero the Header}
 FillChar(AHeader^,SizeOf(TDHCPHeader),0);
 
 {Set the BOOTP Fields}
 AHeader.Opcode:=BOOTP_REQUEST;
 AHeader.HardwareType:=MEDIA_TYPE_ETHERNET; 
 AHeader.HardwareLength:=SizeOf(THardwareAddress);
 AHeader.Hops:=0;
 AHeader.Identifier:=LongWordNtoBE(AIdentifier);
 AHeader.Seconds:=WordNtoBE(ACount * (FRetryTimeout div 1000)); {WordNtoBE(ACount * (DHCP_TIMEOUT div 1000));} {Modified 29/8/2011}
 {AHeader.ClientIP:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);} {Varies depending on Request type}
 AHeader.ClientHardware:=AAdapter.Hardware;
 
 {Set the DHCP Fields}
 {Magic Cookie}
 AHeader.Options[0]:=99;
 AHeader.Options[1]:=130;
 AHeader.Options[2]:=83;
 AHeader.Options[3]:=99;

 {End of Options}
 AHeader.Options[4]:=END_OPT;

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDHCPConfig.CheckDHCPReply(AHeader:PDHCPHeader;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord):Boolean;
{The first four octets of the 'options' field of the DHCP message contain the (decimal) values 99, 130, 83 and 99, respectively}
{This is the same magic cookie as is defined in RFC 1497 [17]}

{Note: Caller must hold the Transport and Adapter locks}
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Checking DHCP Reply');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Transport}
 {if ATransport = nil then Exit;} {Not Used}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Check the BOOTP Fields}
 if AHeader.Opcode <> BOOTP_REPLY then Exit;
 if AHeader.HardwareType <> MEDIA_TYPE_ETHERNET then Exit;
 if AHeader.HardwareLength <> SizeOf(THardwareAddress) then Exit;
 if LongWordBEtoN(AHeader.Identifier) <> AIdentifier then Exit;
 if not AAdapter.Adapter.CompareAddress(AHeader.ClientHardware,AAdapter.Hardware) then Exit;
 
 {Check the DHCP Fields}
 {Magic Cookie}
 if AHeader.Options[0] <> 99 then Exit;
 if AHeader.Options[1] <> 130 then Exit;
 if AHeader.Options[2] <> 83 then Exit;
 if AHeader.Options[3] <> 99 then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDHCPConfig.HandleDHCPReply(AHeader:PDHCPHeader;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Note: Caller must hold the Transport and Adapter locks}
var
 Length:Integer;
 Option:Pointer;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Handling DHCP Reply');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create buffer for Options}
 Option:=GetMem(SizeOf(TDHCPHeader));
 if Option = nil then Exit;
 try
  {Check Address Family}
  case ATransport.Transport.Family of
   AF_INET:begin
     {Check Command}
     case ACommand of
      CONFIG_ADAPTER_DISCOVER:begin
        {Get the Message Type}
        if not ExtractDHCPOption(DHCP_MSG_TYPE,AHeader,Option,Length) then Exit;
        
        {Check Message Type}
        if PByte(Option)^ <> DHCP_OFFER then Exit;
        
        {Get the Address}
        TIPTransportAdapter(AAdapter).Address:=InAddrToHost(AHeader.YourIP);
        
        {Get the Netmask}
        if not ExtractDHCPOption(SUBNET_MASK,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(TInAddr) then Exit;
        TIPTransportAdapter(AAdapter).Netmask:=InAddrToHost(PInAddr(Option)^);
        
        {Get the Server}
        if not ExtractDHCPOption(DHCP_SRV_IDENTIFIER,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(TInAddr) then Exit;
        TIPTransportAdapter(AAdapter).Server:=InAddrToHost(PInAddr(Option)^);
        
        {Get the Lease Time}
        if not ExtractDHCPOption(DHCP_IP_ADDR_LEASE_TIME,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(LongWord) then Exit;
        TIPTransportAdapter(AAdapter).LeaseTime:=(LongWordBEtoN(PLongWord(Option)^) * 1000);
        
        {Get the Gateway}
        if ExtractDHCPOption(ROUTERS_ON_SNET,AHeader,Option,Length) then
         begin
          if Length >= SizeOf(TInAddr) then
           begin
            TIPTransportAdapter(AAdapter).Gateway:=InAddrToHost(PInAddr(Option)^);
           end;
         end;
        
        {Get the Nameservers}
        if ExtractDHCPOption(DNS_SRV,AHeader,Option,Length) then
         begin
          if Length >= SizeOf(TInAddr) then
           begin
            Offset:=0;
            while (Offset + SizeOf(TInAddr)) <= LongWord(Length) do
             begin
              TIPTransport(ATransport.Transport).AddNameserver(InAddrToHost(PInAddr(LongWord(Option) + Offset)^));
              
              Inc(Offset,SizeOf(TInAddr));
             end;
           end;
         end;
        
        {Get the Domainname}
        if ExtractDHCPOption(DOMAIN_NAME,AHeader,Option,Length) then
         begin
          if Length >= 1 then
           begin
            PByte(PtrUInt(Option) + PtrUInt(Length + 1))^:=0;
            Manager.Settings.DomainName:=PChar(Option);
            {TIPTransport(ATransport.Transport).DomainName:=PChar(Option);}
           end;
         end;
        
        {Get the Expiry Time}
        TIPTransportAdapter(AAdapter).ExpiryTime:=GetTickCount64 + TIPTransportAdapter(AAdapter).LeaseTime;
        
        {Get the Renewal Time}
        TIPTransportAdapter(AAdapter).RenewalTime:=GetTickCount64 + Trunc(TIPTransportAdapter(AAdapter).LeaseTime * 0.5);
        if ExtractDHCPOption(DHCP_T1_VALUE,AHeader,Option,Length) then
         begin
          if Length = SizeOf(LongWord) then
           begin
            TIPTransportAdapter(AAdapter).RenewalTime:=GetTickCount64 + (LongWordBEtoN(PLongWord(Option)^) * 1000);
           end;
         end;
        
        {Get the Rebinding Time}
        TIPTransportAdapter(AAdapter).RebindingTime:=GetTickCount64 + Trunc(TIPTransportAdapter(AAdapter).LeaseTime * 0.875);
        if ExtractDHCPOption(DHCP_T2_VALUE,AHeader,Option,Length) then
         begin
          if Length = SizeOf(LongWord) then
           begin
            TIPTransportAdapter(AAdapter).RebindingTime:=GetTickCount64 + (LongWordBEtoN(PLongWord(Option)^) * 1000);
           end;
         end;
        
        {Check for Address in use} {Note: DHCP Server seems to already check it}
        {if not FARP.ConfirmAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,TIPTransportAdapter(AAdapter).Address) then}
        { begin}
           {Send Decline if in use}
        {  SendDHCPDecline( //To Do}
        {  Exit;}
        { end;}
        
        {Return Result}
        Result:=True;
       end;
      CONFIG_ADAPTER_REQUEST:begin
        {Get the Message Type}
        if not ExtractDHCPOption(DHCP_MSG_TYPE,AHeader,Option,Length) then Exit;
        
        {Check Message Type}
        if PByte(Option)^ <> DHCP_ACK then Exit;
        
        {Advertise the Address} {Note: No one seems to listen to this}
        {FARP.AdvertiseAddress(AAdapter.Adapter,TIPTransportAdapter(AAdapter).Address);}
        
        {Return Result}
        Result:=True;
       end;
      CONFIG_ADAPTER_REBOOT:begin
        {Get the Message Type}
        if not ExtractDHCPOption(DHCP_MSG_TYPE,AHeader,Option,Length) then Exit;
        
        {Check Message Type}
        if PByte(Option)^ <> DHCP_ACK then Exit;
        
        {Get the Address}
        TIPTransportAdapter(AAdapter).Address:=InAddrToHost(AHeader.YourIP);
        
        {Get the Netmask}
        if not ExtractDHCPOption(SUBNET_MASK,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(TInAddr) then Exit;
        TIPTransportAdapter(AAdapter).Netmask:=InAddrToHost(PInAddr(Option)^);
        
        {Get the Server}
        if not ExtractDHCPOption(DHCP_SRV_IDENTIFIER,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(TInAddr) then Exit;
        TIPTransportAdapter(AAdapter).Server:=InAddrToHost(PInAddr(Option)^);
        
        {Get the Lease Time}
        if not ExtractDHCPOption(DHCP_IP_ADDR_LEASE_TIME,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(LongWord) then Exit;
        TIPTransportAdapter(AAdapter).LeaseTime:=(LongWordBEtoN(PLongWord(Option)^) * 1000);
        
        {Get the Gateway}
        if ExtractDHCPOption(ROUTERS_ON_SNET,AHeader,Option,Length) then
         begin
          if Length >= SizeOf(TInAddr) then
           begin
            TIPTransportAdapter(AAdapter).Gateway:=InAddrToHost(PInAddr(Option)^);
           end;
         end;
         
        {Get the Nameservers}
        if ExtractDHCPOption(DNS_SRV,AHeader,Option,Length) then
         begin
          if Length >= SizeOf(TInAddr) then
           begin
            Offset:=0;
            while (Offset + SizeOf(TInAddr)) <= LongWord(Length) do
             begin
              TIPTransport(ATransport.Transport).AddNameserver(InAddrToHost(PInAddr(LongWord(Option) + Offset)^));
              
              Inc(Offset,SizeOf(TInAddr));
             end;
           end;
         end;
        
        {Get the Domainname}
        if ExtractDHCPOption(DOMAIN_NAME,AHeader,Option,Length) then
         begin
          if Length >= 1 then
           begin
            PByte(PtrUInt(Option) + PtrUInt(Length + 1))^:=0;
            Manager.Settings.DomainName:=PChar(Option);
            {TIPTransport(ATransport.Transport).DomainName:=PChar(Option);}
           end;
         end;
        
        {Get the Expiry Time}
        TIPTransportAdapter(AAdapter).ExpiryTime:=GetTickCount64 + TIPTransportAdapter(AAdapter).LeaseTime;
        
        {Get the Renewal Time}
        TIPTransportAdapter(AAdapter).RenewalTime:=GetTickCount64 + Trunc(TIPTransportAdapter(AAdapter).LeaseTime * 0.5);
        if ExtractDHCPOption(DHCP_T1_VALUE,AHeader,Option,Length) then
         begin
          if Length = SizeOf(LongWord) then
           begin
            TIPTransportAdapter(AAdapter).RenewalTime:=GetTickCount64 + (LongWordBEtoN(PLongWord(Option)^) * 1000);
           end;
         end;
        
        {Get the Rebinding Time}
        TIPTransportAdapter(AAdapter).RebindingTime:=GetTickCount64 + Trunc(TIPTransportAdapter(AAdapter).LeaseTime * 0.875);
        if ExtractDHCPOption(DHCP_T2_VALUE,AHeader,Option,Length) then
         begin
          if Length = SizeOf(LongWord) then
           begin
            TIPTransportAdapter(AAdapter).RebindingTime:=GetTickCount64 + (LongWordBEtoN(PLongWord(Option)^) * 1000);
           end;
         end;
        
        {Check for Address in use} {Note: DHCP Server seems to already check it}
        if not FARP.ConfirmAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,TIPTransportAdapter(AAdapter).Address) then Exit;
        
        {Advertise the Address} {Note: No one seems to listen to this}
        {FARP.AdvertiseAddress(AAdapter.Adapter,TIPTransportAdapter(AAdapter).Address);}
        
        {Return Result}
        Result:=True;
       end;
      CONFIG_ADAPTER_RELEASE:begin {Note: Release does not wait for a response}
        {Get the Message Type}
        if not ExtractDHCPOption(DHCP_MSG_TYPE,AHeader,Option,Length) then Exit;
        
        {Check Message Type}
        if PByte(Option)^ <> DHCP_ACK then Exit;
        
        {Dont clear the Addresses, IP needs them to cleanup}
        Result:=True;
       end;
      CONFIG_ADAPTER_RENEW,CONFIG_ADAPTER_REBIND:begin
        {Get the Message Type}
        if not ExtractDHCPOption(DHCP_MSG_TYPE,AHeader,Option,Length) then Exit;
        
        {Check Message Type}
        if PByte(Option)^ <> DHCP_ACK then Exit;
        
        {Get the Lease Time}
        if not ExtractDHCPOption(DHCP_IP_ADDR_LEASE_TIME,AHeader,Option,Length) then Exit;
        if Length <> SizeOf(LongWord) then Exit;
        
        TIPTransportAdapter(AAdapter).LeaseTime:=(LongWordBEtoN(PLongWord(Option)^) * 1000);
        
        {Get the Expiry Time}
        TIPTransportAdapter(AAdapter).ExpiryTime:=GetTickCount64 + TIPTransportAdapter(AAdapter).LeaseTime;
        
        {Get the Renewal Time}
        TIPTransportAdapter(AAdapter).RenewalTime:=GetTickCount64 + Trunc(TIPTransportAdapter(AAdapter).LeaseTime * 0.5);
        if ExtractDHCPOption(DHCP_T1_VALUE,AHeader,Option,Length) then
         begin
          if Length = SizeOf(LongWord) then
           begin
            TIPTransportAdapter(AAdapter).RenewalTime:=GetTickCount64 + (LongWordBEtoN(PLongWord(Option)^) * 1000);
           end;
         end;
        
        {Get the Rebinding Time}
        TIPTransportAdapter(AAdapter).RebindingTime:=GetTickCount64 + Trunc(TIPTransportAdapter(AAdapter).LeaseTime * 0.875);
        if ExtractDHCPOption(DHCP_T2_VALUE,AHeader,Option,Length) then
         begin
          if Length = SizeOf(LongWord) then
           begin
            TIPTransportAdapter(AAdapter).RebindingTime:=GetTickCount64 + (LongWordBEtoN(PLongWord(Option)^) * 1000);
           end;
         end;
        
        {Return Result}
        Result:=True;
       end;
      CONFIG_ADAPTER_INFORM:begin
        {Get the Message Type}
        if not ExtractDHCPOption(DHCP_MSG_TYPE,AHeader,Option,Length) then Exit;
        
        {Check Message Type}
        if PByte(Option)^ <> DHCP_ACK then Exit;
        
        {Get the Gateway}
        if ExtractDHCPOption(ROUTERS_ON_SNET,AHeader,Option,Length) then
         begin
          if Length >= SizeOf(TInAddr) then
           begin
            TIPTransportAdapter(AAdapter).Gateway:=InAddrToHost(PInAddr(Option)^);
           end;
         end;
        
        {Get the Nameservers}
        if ExtractDHCPOption(DNS_SRV,AHeader,Option,Length) then
         begin
          if Length >= SizeOf(TInAddr) then
           begin
            Offset:=0;
            while (Offset + SizeOf(TInAddr)) <= LongWord(Length) do
             begin
              TIPTransport(ATransport.Transport).AddNameserver(InAddrToHost(PInAddr(LongWord(Option) + Offset)^));
              
              Inc(Offset,SizeOf(TInAddr));
             end;
           end;
         end;
        
        {Get the Domainname}
        if ExtractDHCPOption(DOMAIN_NAME,AHeader,Option,Length) then
         begin
          if Length >= 1 then
           begin
            PByte(PtrUInt(Option) + PtrUInt(Length + 1))^:=0;
            Manager.Settings.DomainName:=PChar(Option);
            {TIPTransport(ATransport.Transport).DomainName:=PChar(Option);}
           end;
         end;
        
        {Return Result}
        Result:=True;
       end;
     end;
    end;
   AF_INET6:begin
   
     //To Do
     
    end;
  end;
 finally
  FreeMem(Option);
 end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPDiscover(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Discover uses Broadcast send and ciaddr zero}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 HostName:String;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Discover');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;

 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=IP_DEFAULT_ADDRESS;
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_DISCOVER;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Host Name}
    HostName:=Manager.Settings.HostName;
    if Length(HostName) <> 0 then if not InsertDHCPOption(HOST_NAME,@Header,PChar(HostName),Length(HostName)) then Exit;
    
    {Requested Options}
    PByte(Option)^:=SUBNET_MASK;
    PByte(LongWord(Option) + 1)^:=ROUTERS_ON_SNET;
    PByte(LongWord(Option) + 2)^:=DNS_SRV;
    PByte(LongWord(Option) + 3)^:=DOMAIN_NAME;
    PByte(LongWord(Option) + 4)^:=DHCP_IP_ADDR_LEASE_TIME;
    PByte(LongWord(Option) + 5)^:=DHCP_T1_VALUE;
    PByte(LongWord(Option) + 6)^:=DHCP_T2_VALUE;
    if not InsertDHCPOption(DHCP_PARAM_REQUEST,@Header,Option,7) then Exit;
    
    {Max Message Size}
    PWord(Option)^:=WordNtoBE(DHCP_MESSAGE_SIZE);
    if not InsertDHCPOption(DHCP_MAX_MSG_SIZE,@Header,Option,2) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPRequest(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Request uses Broadcast send and ciaddr zero}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 HostName:String;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Request');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=IP_DEFAULT_ADDRESS;
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_REQUEST;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Host Name}
    HostName:=Manager.Settings.HostName;
    if Length(HostName) <> 0 then if not InsertDHCPOption(HOST_NAME,@Header,PChar(HostName),Length(HostName)) then Exit;
    
    {Requested Options}
    PByte(Option)^:=SUBNET_MASK;
    PByte(LongWord(Option) + 1)^:=ROUTERS_ON_SNET;
    PByte(LongWord(Option) + 2)^:=DNS_SRV;
    PByte(LongWord(Option) + 3)^:=DOMAIN_NAME;
    PByte(LongWord(Option) + 4)^:=DHCP_IP_ADDR_LEASE_TIME;
    PByte(LongWord(Option) + 5)^:=DHCP_T1_VALUE;
    PByte(LongWord(Option) + 6)^:=DHCP_T2_VALUE;
    if not InsertDHCPOption(DHCP_PARAM_REQUEST,@Header,Option,7) then Exit;
    
    {Server Identifier}
    PInAddr(Option)^:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Server);
    if not InsertDHCPOption(DHCP_SRV_IDENTIFIER,@Header,Option,SizeOf(TInAddr)) then Exit;
    
    {Requested Address}
    PInAddr(Option)^:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
    if not InsertDHCPOption(DHCP_REQUESTED_IP_ADDR,@Header,Option,SizeOf(TInAddr)) then Exit;
    
    {Max Message Size}
    PWord(Option)^:=WordNtoBE(DHCP_MESSAGE_SIZE);
    if not InsertDHCPOption(DHCP_MAX_MSG_SIZE,@Header,Option,2) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPDecline(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Decline uses Broadcast send and ciaddr zero}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Decline');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=IP_DEFAULT_ADDRESS;
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_DECLINE;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Server Identifier}
    PInAddr(Option)^:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Server);
    if not InsertDHCPOption(DHCP_SRV_IDENTIFIER,@Header,Option,SizeOf(TInAddr)) then Exit;
    
    {Requested Address}
    PInAddr(Option)^:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
    if not InsertDHCPOption(DHCP_REQUESTED_IP_ADDR,@Header,Option,SizeOf(TInAddr)) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPRelease(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Release uses Unicast send and ciaddr filled in}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Release');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_RELEASE;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Server Identifier}
    PInAddr(Option)^:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Server);
    if not InsertDHCPOption(DHCP_SRV_IDENTIFIER,@Header,Option,SizeOf(TInAddr)) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    SockAddr.sin_addr:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Server);
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPInform(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Inform uses Broadcast send and ciaddr filled in}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 HostName:String;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Inform');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_INFORM;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Host Name}
    HostName:=Manager.Settings.HostName;
    if Length(HostName) <> 0 then if not InsertDHCPOption(HOST_NAME,@Header,PChar(HostName),Length(HostName)) then Exit;
    
    {Requested Options}
    PByte(Option)^:=SUBNET_MASK;
    PByte(LongWord(Option) + 1)^:=ROUTERS_ON_SNET;
    PByte(LongWord(Option) + 2)^:=DNS_SRV;
    PByte(LongWord(Option) + 3)^:=DOMAIN_NAME;
    PByte(LongWord(Option) + 4)^:=DHCP_T1_VALUE;
    PByte(LongWord(Option) + 5)^:=DHCP_T2_VALUE;
    if not InsertDHCPOption(DHCP_PARAM_REQUEST,@Header,Option,6) then Exit;
    
    {Max Message Size}
    PWord(Option)^:=WordNtoBE(DHCP_MESSAGE_SIZE);
    if not InsertDHCPOption(DHCP_MAX_MSG_SIZE,@Header,Option,2) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPRenew(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Renew actually uses Request with Unicast send and ciaddr filled in}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Request (Renew)');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_REQUEST;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Requested Options}
    PByte(Option)^:=DHCP_IP_ADDR_LEASE_TIME;
    if not InsertDHCPOption(DHCP_PARAM_REQUEST,@Header,Option,1) then Exit;
    
    {Max Message Size}
    PWord(Option)^:=WordNtoBE(DHCP_MESSAGE_SIZE);
    if not InsertDHCPOption(DHCP_MAX_MSG_SIZE,@Header,Option,2) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}    
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    SockAddr.sin_addr:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Server);
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPRebind(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Rebind actually uses Request with Broadcast send and ciaddr filled in}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Request (Rebind)');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_REQUEST;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Requested Options}
    PByte(Option)^:=DHCP_IP_ADDR_LEASE_TIME;
    if not InsertDHCPOption(DHCP_PARAM_REQUEST,@Header,Option,1) then Exit;
    
    {Max Message Size}
    PWord(Option)^:=WordNtoBE(DHCP_MESSAGE_SIZE);
    if not InsertDHCPOption(DHCP_MAX_MSG_SIZE,@Header,Option,2) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}    
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.SendDHCPReboot(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Reboot actually uses Request with Broadcast send and ciaddr zero}
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 Option:Pointer;
 HostName:String;
 SockAddr:TSockAddr;
 Header:TDHCPHeader;
 ClientId:TDHCPClientId;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Sending DHCP Request (Reboot)');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create the Header}
 if CreateDHCPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the DHCP Fields}
   Header.ClientIP:=IP_DEFAULT_ADDRESS;
   
   {Add the Options}
   Option:=GetMem(SizeOf(TDHCPHeader));
   if Option = nil then Exit;
   try
    {Message Type}
    PByte(Option)^:=DHCP_REQUEST;
    if not InsertDHCPOption(DHCP_MSG_TYPE,@Header,Option,1) then Exit;
    
    {Client Identifier}
    ClientId:=GetDHCPClientId(AAdapter);
    if not InsertDHCPOption(DHCP_CLIENT_ID,@Header,@ClientId,SizeOf(TDHCPClientId)) then Exit;
    
    {Host Name}
    HostName:=Manager.Settings.HostName;
    if Length(HostName) <> 0 then if not InsertDHCPOption(HOST_NAME,@Header,PChar(HostName),Length(HostName)) then Exit;
    
    {Requested Options}
    PByte(Option)^:=SUBNET_MASK;
    PByte(LongWord(Option) + 1)^:=ROUTERS_ON_SNET;
    PByte(LongWord(Option) + 2)^:=DNS_SRV;
    PByte(LongWord(Option) + 3)^:=DOMAIN_NAME;
    PByte(LongWord(Option) + 4)^:=DHCP_IP_ADDR_LEASE_TIME;
    PByte(LongWord(Option) + 5)^:=DHCP_T1_VALUE;
    PByte(LongWord(Option) + 6)^:=DHCP_T2_VALUE;
    if not InsertDHCPOption(DHCP_PARAM_REQUEST,@Header,Option,7) then Exit;
    
    {Requested Address}
    PInAddr(Option)^:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
    if not InsertDHCPOption(DHCP_REQUESTED_IP_ADDR,@Header,Option,SizeOf(TInAddr)) then Exit;
    
    {Max Message Size}
    PWord(Option)^:=WordNtoBE(DHCP_MESSAGE_SIZE);
    if not InsertDHCPOption(DHCP_MAX_MSG_SIZE,@Header,Option,2) then Exit;
    
    {Get the Size}
    Size:=GetDHCPHeaderSize(@Header);
    
    {Set the Address}    
    SockAddr.sin_family:=AF_INET;
    SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
    LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
    
    {Send the Request}
    Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
   finally
    FreeMem(Option);
   end;
  end;
end;

{==============================================================================}

function TDHCPConfig.RecvDHCPReply(ASocket:TProtocolSocket;ATransport:TDHCPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACommand:Word):Boolean;
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 SockSize:Integer;
 SockAddr:TSockAddr;
 Header:PDHCPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Receiving DHCP Reply');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get the Size}
 Size:=SizeOf(TDHCPHeader) * 2; {Double Allocate for Safety}
 
 {Create the Buffer}
 Header:=GetMem(Size);
 if Header = nil then Exit;
 try
  {Zero the Header}
  FillChar(Header^,SizeOf(TDHCPHeader) * 2,0);
 
  {Receive Datagram}
  SockSize:=SizeOf(TSockAddr);
  if FUDP.RecvFrom(ASocket,Header^,Size,0,SockAddr,SockSize) > 0 then
   begin
    {Check for Reply}
    if CheckDHCPReply(Header,ATransport,AAdapter,AIdentifier) then
     begin
      {Handle Reply}
      Result:=HandleDHCPReply(Header,ATransport,AAdapter,ACommand);
      if Result then Exit;
     end;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function TDHCPConfig.InsertDHCPOption(AOption:Byte;AHeader:PDHCPHeader;AValue:Pointer;ASize:Integer):Boolean;
{The first four octets of the 'options' field of the DHCP message contain the (decimal) values 99, 130, 83 and 99, respectively}
{This is the same magic cookie as is defined in RFC 1497 [17]}
var
 Size:Integer;
 Count:Integer;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Inserting DHCP Option');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig:  Option = '  + IntToStr(AOption));
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Value}
 if AValue = nil then Exit;
 
 {Interate the Options starting after Magic Cookie}
 Count:=4;
 while Count < DHCP_OPTIONS_SIZE do
  begin
   {Check for Pad Option}
   if AHeader.Options[Count] = PAD_OPT then
    begin
     {Move to Next Option}
     Inc(Count,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Count + 1) > (DHCP_OPTIONS_SIZE - 1) then Exit;
     
     {Get the Length}
     Size:=AHeader.Options[Count + 1];
     
     {Check for End Option}
     if AHeader.Options[Count] = END_OPT then
      begin
       {Check for end of buffer}
       if (Count + ASize + 2) > (DHCP_OPTIONS_SIZE - 1) then Exit;
       
       {Set the Option}
       AHeader.Options[Count]:=AOption;
       
       {Set the Length}
       AHeader.Options[Count + 1]:=ASize;
       
       {Set the Value}
       System.Move(AValue^,AHeader.Options[Count + 2],ASize);
       
       {Set the End Option}
       AHeader.Options[Count + ASize + 2]:=END_OPT;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
      
     {Move to Next Option}
     Inc(Count,Size + 2);
    end;
  end;
end;

{==============================================================================}

function TDHCPConfig.ExtractDHCPOption(AOption:Byte;AHeader:PDHCPHeader;AValue:Pointer;var ASize:Integer):Boolean;
{The first four octets of the 'options' field of the DHCP message contain the (decimal) values 99, 130, 83 and 99, respectively}
{This is the same magic cookie as is defined in RFC 1497 [17]}
var
 Count:Integer;
begin
 {}
 //To Do //Account for the overflow option (quite easy)
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: Extracting DHCP Option');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Value}
 if AValue = nil then Exit;
 
 {Interate the Options starting after Magic Cookie}
 Count:=4;
 while Count < DHCP_OPTIONS_SIZE do
  begin
   {Check for End Option}
   if AHeader.Options[Count] = END_OPT then Exit;
   
   {Check for Pad Option}
   if AHeader.Options[Count] = PAD_OPT then
    begin
     {Move to Next Option}
     Inc(Count,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Count + 1) > (DHCP_OPTIONS_SIZE - 1) then Exit;
     
     {Get the Length}
     ASize:=AHeader.Options[Count + 1];
     
     {Check for Option}
     if AHeader.Options[Count] = AOption then
      begin
       {$IFDEF DHCP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: ExtractDHCPOption - Found Option');
       {$ENDIF}
       
       {Check for end of buffer}
       if (Count + ASize + 1) > (DHCP_OPTIONS_SIZE - 1) then Exit;
       
       {Get the Value}
       System.Move(AHeader.Options[Count + 2],AValue^,ASize);
       
       {Return Result}
       Result:=True;
       Exit;
      end;
      
     {Move to Next Option}
     Inc(Count,ASize + 2);
    end;
  end;
end;

{==============================================================================}

function TDHCPConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this config}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TDHCPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport} 
  Transport:=TDHCPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add DHCP Config}
       Handle:=TIPTransport(ATransport).AddConfig(CONFIG_TYPE_DHCP,True,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TDHCPConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_DHCP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
     AF_INET6:begin
       {Add DHCP Config}
       Handle:=TIP6Transport(ATransport).AddConfig(CONFIG_TYPE_DHCP,True,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TDHCPConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_DHCP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDHCPConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this config}
{Transport: The transport to remove}
var
 Transport:TDHCPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport}
  Transport:=TDHCPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove DHCP Config}
     if TIPTransport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
   AF_INET6:begin
     {Remove DHCP Config}
     if TIP6Transport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDHCPConfig.StartConfig:Boolean;
{Start this config ready for use}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: StartConfig');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Locate ARP Transport}
  FARP:=TARPTransport(Manager.Transports.GetTransportByType(AF_UNSPEC,PACKET_TYPE_ARP,False,NETWORK_LOCK_NONE)); //To Do //AddTransport ? //Some way to track this, applies to IP as well //Client ?
  if FARP = nil then Exit;
 
  {Locate UDP Protocol}
  FUDP:=TUDPProtocol(Manager.GetProtocolByType(IPPROTO_UDP,SOCK_DGRAM,False,NETWORK_LOCK_NONE)); //To Do //AddProtocol ? //Some way to track this, applies to IP as well //Client ?
  if FUDP = nil then Exit;
 
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Register with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDHCPConfig.StopConfig:Boolean;
{Stop this config ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'DHCPConfig: StopConfig');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Deregister with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Remove UDP Protocol}
  FUDP:=nil;
 
  {Remove ARP Transport}
  FARP:=nil;
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TDHCPConfig.ProcessConfig:Boolean;
{Process periodic tasks for this config}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TDHCPConfig.SetConfig(AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean;
{Set Delay, Retry and Timeout for this config}
begin
 {}
 WriterLock;
 try
  Result:=True;
  
  if (AInitDelay > DHCP_MIN_DELAY) and (AInitDelay <= DHCP_MAX_DELAY) then FInitDelay:=AInitDelay;
  if (ARetryCount >= DHCP_MIN_RETRIES) and (ARetryCount <= DHCP_MAX_RETRIES) then FRetryCount:=ARetryCount;
  if (ARetryTimeout >= DHCP_MIN_TIMEOUT) and (ARetryTimeout <= DHCP_MAX_TIMEOUT) then FRetryTimeout:=ARetryTimeout;
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TBOOTPConfig}
constructor TBOOTPConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create(AManager);
 FConfigType:=CONFIG_TYPE_BOOTP;
 FInitDelay:=BOOTP_DELAY;
 FRetryCount:=BOOTP_RETRIES;
 FRetryTimeout:=BOOTP_TIMEOUT;
 FARP:=nil;
 FUDP:=nil;
end;

{==============================================================================}

destructor TBOOTPConfig.Destroy;
begin
 {}
 WriterLock;
 try
  FUDP:=nil;
  FARP:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TBOOTPConfig.ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Process a network config request from a Transport}
{Handle: The Handle of the Config Transport the request is from}
{Adapter: The transport adapter to perform configuration on}
{Command: The configuration command to perform}

{Note: Caller must hold the Adapter lock}
var
 Count:Integer;
 Timeout:Integer;
 Socket:TUDPSocket;
 SockAddr:TSockAddr;
 Identifier:LongWord;
 Transport:TBOOTPConfigTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: ConfigHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig:  Command = ' + ConfigCommandToString(ACommand));
 {$ENDIF}
 
 {Check Adapter}
 if AAdapter = nil then Exit;

 {Get Transport}
 Transport:=TBOOTPConfigTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check ARP Transport}
  if FARP = nil then Exit;
  
  {Check UDP Protocol}
  if FUDP = nil then Exit;
  
  {Check Command}
  case ACommand of
   CONFIG_ADAPTER_DISCOVER:begin
     {Perform Discover}
     {$IFDEF DHCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Attempting BOOTP Configuration');
     {$ENDIF}
     
     {Set Configuring}
     AAdapter.Configuring:=True;
     try
      {Check Address Family}
      case Transport.Transport.Family of
       AF_INET:begin
         {Delay Init}
         Sleep(FInitDelay); 
         
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Add the ARP Address}
         FARP.LoadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,AAdapter.Hardware,ADDRESS_TYPE_LOCAL);
         
         {Add the Address}
         TIPTransport(Transport.Transport).AddAddress(IP_DEFAULT_ADDRESS,AAdapter.Adapter,ADDRESS_TYPE_PRIMARY,False,NETWORK_LOCK_NONE);
         
         {Add the Route}
         TIPTransport(Transport.Transport).AddRoute(IP_BROADCAST_NETWORK,IP_BROADCAST_NETMASK,IP_DEFAULT_ADDRESS,IP_DEFAULT_ADDRESS,ROUTE_TYPE_BROADCAST,False,NETWORK_LOCK_NONE);
         try
          Randomize;
          
          {Create the Socket}
          Socket:=TUDPSocket(FUDP.Socket(AF_INET,SOCK_DGRAM,IPPROTO_UDP));
          if TSocket(Socket) = INVALID_SOCKET then Exit;
          
          {Lock Socket}
          Socket.ReaderLock;
          try
           {Set the Options}
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_REUSEADDR,'1111',4) = SOCKET_ERROR then Exit;
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_BROADCAST,'1111',4) = SOCKET_ERROR then Exit;
           
           {Bind the Socket}
           SockAddr.sin_family:=AF_INET;
           SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPC);
           SockAddr.sin_addr.S_addr:=INADDR_ANY;
           if FUDP.Bind(Socket,SockAddr,SizeOf(TSockAddr)) = SOCKET_ERROR then Exit;
           
           {Set the Timeout}
           Timeout:=FRetryTimeout;
           if FUDP.SetSockOpt(Socket,SOL_SOCKET,SO_RCVTIMEO,PChar(@Timeout),SizeOf(Integer)) = SOCKET_ERROR then Exit;
           
           {Send the Request}
           Count:=0;
           while Count < Integer(FRetryCount) do  {Count < BOOTP_RETRIES} {Modified 29/8/2011}
            begin
             {Get Identifier}
             Identifier:=Random($7FFFFFFF);
             
             {Send Request}
             if SendBOOTPRequest(Socket,Transport,AAdapter,Identifier,Count) then
              begin
               {Receive Reply}
               Result:=RecvBOOTPReply(Socket,Transport,AAdapter,Identifier);
               if Result then Exit;
              end;
              
             {Increment Count} 
             Inc(Count);
            end;
          finally
           {Close the Socket}
           FUDP.CloseSocket(Socket);
           
           {Unlock Socket}
           Socket.ReaderUnlock;
          end;
         finally
          {Remove the Route}
          TIPTransport(Transport.Transport).RemoveRoute(IP_BROADCAST_NETWORK,IP_DEFAULT_ADDRESS);
          
          {Remove the Address}
          TIPTransport(Transport.Transport).RemoveAddress(IP_DEFAULT_ADDRESS);
          
          {Remove the ARP Address}
          FARP.UnloadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS);
         end;
        end;
      end;
     finally
      {Reset Configuring}
      AAdapter.Configuring:=False;
     end;
    end;
   CONFIG_ADAPTER_RELEASE:begin
     {Perform Release}
     Result:=True;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TBOOTPConfig.CreateBOOTPRequest(AHeader:PBOOTPHeader;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Caller must hold the Transport and Adapter locks}
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Creating BOOTP Request');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Transport}
 {if ATransport = nil then Exit;} {Not Used}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Zero the Header}
 FillChar(AHeader^,SizeOf(TBOOTPHeader),0);
 
 {Set the BOOTP Fields}
 AHeader.Opcode:=BOOTP_REQUEST;
 AHeader.HardwareType:=MEDIA_TYPE_ETHERNET;
 AHeader.HardwareLength:=SizeOf(THardwareAddress);
 AHeader.Hops:=0;
 AHeader.Identifier:=LongWordNtoBE(AIdentifier);
 AHeader.Seconds:=WordNtoBE(ACount * (FRetryTimeout div 1000)); {WordNtoBE(ACount * (BOOTP_TIMEOUT div 1000));} {Modified 29/8/2011}
 AHeader.ClientIP:=InAddrToNetwork(TIPTransportAdapter(AAdapter).Address);
 AHeader.ClientHardware:=AAdapter.Hardware;
 
 {Magic Cookie}
 AHeader.VendorData[0]:=99;
 AHeader.VendorData[1]:=130;
 AHeader.VendorData[2]:=83;
 AHeader.VendorData[3]:=99;
 
 {End of Vendor Data}
 AHeader.VendorData[4]:=END_OPT;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TBOOTPConfig.CheckBOOTPReply(AHeader:PBOOTPHeader;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord):Boolean;
{Note: Caller must hold the Transport and Adapter locks}
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Checking BOOTP Reply');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Transport}
 {if ATransport = nil then Exit;} {Not Used}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Check the BOOTP Fields}
 if AHeader.Opcode <> BOOTP_REPLY then Exit;
 if AHeader.HardwareType <> MEDIA_TYPE_ETHERNET then Exit;
 if AHeader.HardwareLength <> SizeOf(THardwareAddress) then Exit;
 if LongWordBEtoN(AHeader.Identifier) <> AIdentifier then Exit;
 if not AAdapter.Adapter.CompareAddress(AHeader.ClientHardware,AAdapter.Hardware) then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TBOOTPConfig.HandleBOOTPReply(AHeader:PBOOTPHeader;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter):Boolean;
{Note: Caller must hold the Transport and Adapter locks}
var
 Length:Integer;
 Option:Pointer;
 Offset:LongWord;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Handling BOOTP Reply');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Create buffer for Options}
 Option:=GetMem(SizeOf(TBOOTPHeader));
 if Option = nil then Exit;
 try
  {Check Address Family}
  case ATransport.Transport.Family of
   AF_INET:begin
     {Get the Address}
     TIPTransportAdapter(AAdapter).Address:=InAddrToHost(AHeader.YourIP);
     
     {Get the Netmask}
     if ExtractBOOTPOption(SUBNET_MASK,AHeader,Option,Length) then
      begin
       if Length >= SizeOf(TInAddr) then
        begin
         TIPTransportAdapter(AAdapter).Netmask:=InAddrToHost(PInAddr(Option)^);
        end;
      end;
     
     {Guess the Netmask}
     if TIPTransport(ATransport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Netmask) then TIPTransportAdapter(AAdapter).Netmask:=IP_CLASSC_NETMASK;
     
     {Get the Gateway}
     if ExtractBOOTPOption(ROUTERS_ON_SNET,AHeader,Option,Length) then
      begin
       if Length >= SizeOf(TInAddr) then
        begin
         TIPTransportAdapter(AAdapter).Gateway:=InAddrToHost(PInAddr(Option)^);
        end;
      end;
     
     {Get the Nameservers}
     if ExtractBOOTPOption(DNS_SRV,AHeader,Option,Length) then
      begin
       if Length >= SizeOf(TInAddr) then
        begin
         Offset:=0;
         while (Offset + SizeOf(TInAddr)) <= LongWord(Length) do
          begin
           TIPTransport(ATransport.Transport).AddNameserver(InAddrToHost(PInAddr(LongWord(Option) + Offset)^));
           Inc(Offset,SizeOf(TInAddr));
          end;
        end;
      end;
     
     {Get the Domainname}
     if ExtractBOOTPOption(DOMAIN_NAME,AHeader,Option,Length) then
      begin
       if Length >= 1 then
        begin
         PByte(PtrUInt(Option) + PtrUInt(Length + 1))^:=0;
         Manager.Settings.DomainName:=PChar(Option);
         {TIPTransport(ATransport.Transport).DomainName:=PChar(Option);}
        end;
      end;
      
     {Return Result} 
     Result:=True;
    end;
  end;
 finally
  FreeMem(Option);
 end;
end;

{==============================================================================}

function TBOOTPConfig.SendBOOTPRequest(ASocket:TProtocolSocket;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord;ACount:Word):Boolean;
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 SockAddr:TSockAddr;
 Header:TBOOTPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Sending BOOTP Request');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get the Size}
 Size:=SizeOf(TBOOTPHeader);

 {Create the Header}
 if CreateBOOTPRequest(@Header,ATransport,AAdapter,AIdentifier,ACount) then
  begin
   {Set the Address}
   SockAddr.sin_family:=AF_INET;
   SockAddr.sin_port:=WordNtoBE(IPPORT_BOOTPS);
   LongWord(SockAddr.sin_addr.S_addr):=INADDR_BROADCAST;
   
   {Send the Request}
   Result:=FUDP.SendTo(ASocket,Header,Size,0,SockAddr,SizeOf(TSockAddr)) = Size;
  end;
end;

{==============================================================================}

function TBOOTPConfig.RecvBOOTPReply(ASocket:TProtocolSocket;ATransport:TBOOTPConfigTransport;AAdapter:TTransportAdapter;AIdentifier:LongWord):Boolean;
{Note: Caller must hold the Socket, Transport and Adapter locks}
var
 Size:Integer;
 SockSize:Integer;
 SockAddr:TSockAddr;
 Header:PBOOTPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Receiving BOOTP Reply');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Transport}
 if ATransport = nil then Exit;
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get the Size}
 Size:=SizeOf(TBOOTPHeader) * 2; {Double Allocate for Safety}
 
 {Create the Buffer}
 Header:=GetMem(Size);
 if Header = nil then Exit;
 try
  {Zero the Header}
  FillChar(Header^,SizeOf(TBOOTPHeader) * 2,0);
  
  {Receive Datagram}
  SockSize:=SizeOf(TSockAddr);
  if FUDP.RecvFrom(ASocket,Header^,Size,0,SockAddr,SockSize) > 0 then
   begin
    {Check for Reply}
    if CheckBOOTPReply(Header,ATransport,AAdapter,AIdentifier) then
     begin
      {Handle Reply}
      Result:=HandleBOOTPReply(Header,ATransport,AAdapter);
      if Result then Exit;
     end;
   end;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function TBOOTPConfig.ExtractBOOTPOption(AOption:Byte;AHeader:PBOOTPHeader;AValue:Pointer;var ASize:Integer):Boolean;
{The first four octets of the 'vendordata' field of the BOOTP messagecontain the (decimal) values 99, 130, 83 and 99, respectively}
{This is the same magic cookie as is defined in RFC 1497 [17])}
var
 Count:Integer;
begin
 {}
 //To Do //Account for the overflow option (quite easy)
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: Extracting BOOTP Option');
 {$ENDIF}
 
 {Check Header}
 if AHeader = nil then Exit;
 
 {Check Value}
 if AValue = nil then Exit;
 
 {Interate the Options starting after Magic Cookie}
 Count:=4;
 while Count < BOOTP_VENDOR_SIZE do
  begin
   {Check for End Option}
   if AHeader.VendorData[Count] = END_OPT then Exit;
   
   {Check for Pad Option}
   if AHeader.VendorData[Count] = PAD_OPT then
    begin
     {Move to Next Option}
     Inc(Count,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Count + 1) > (BOOTP_VENDOR_SIZE - 1) then Exit;
     
     {Get the Length}
     ASize:=AHeader.VendorData[Count + 1];
     
     {Check for Option}
     if AHeader.VendorData[Count] = AOption then
      begin
       {$IFDEF DHCP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: ExtractBOOTPOption - Found Option');
       {$ENDIF}
       
       {Check for end of buffer}
       if (Count + ASize + 1) > (BOOTP_VENDOR_SIZE - 1) then Exit;
       
       {Get the Value}
       System.Move(AHeader.VendorData[Count + 2],AValue^,ASize);
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Move to Next Option}
     Inc(Count,ASize + 2);
    end;
  end;
end;

{==============================================================================}

function TBOOTPConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this config}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TBOOTPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport} 
  Transport:=TBOOTPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    case ATransport.Family of
     AF_INET:begin
       {Add BOOTP Config}
       Handle:=TIPTransport(ATransport).AddConfig(CONFIG_TYPE_BOOTP,True,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TBOOTPConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_BOOTP;
         Transport.Transport:=ATransport;

         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
          
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TBOOTPConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this config}
{Transport: The transport to remove}
var
 Transport:TBOOTPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport}
  Transport:=TBOOTPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove BOOTP Config}
     if TIPTransport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);

        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
        
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TBOOTPConfig.StartConfig:Boolean;
{Start this config ready for use}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;

  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: StartConfig');
  {$ENDIF}

  {Check Manager}
  if Manager = nil then Exit;

  {Locate ARP Transport}
  FARP:=TARPTransport(Manager.Transports.GetTransportByType(AF_UNSPEC,PACKET_TYPE_ARP,False,NETWORK_LOCK_NONE)); //To Do //AddTransport ? //Some way to track this, applies to IP as well //Client ?
  if FARP = nil then Exit;

  {Locate UDP Protocol}
  FUDP:=TUDPProtocol(Manager.GetProtocolByType(IPPROTO_UDP,SOCK_DGRAM,False,NETWORK_LOCK_NONE)); //To Do //AddProtocol ? //Some way to track this, applies to IP as well //Client ?
  if FUDP = nil then Exit;
  
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TBOOTPConfig.StopConfig:Boolean;
{Stop this config ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'BOOTPConfig: StopConfig');
  {$ENDIF}
  
  if Manager = nil then Exit;
  
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Remove UDP Protocol}
  FUDP:=nil;
  
  {Remove ARP Transport}
  FARP:=nil;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TBOOTPConfig.ProcessConfig:Boolean;
{Process periodic tasks for this config}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TBOOTPConfig.SetConfig(AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean;
{Set Delay, Retry and Timeout for this config}
begin
 {}
 WriterLock;
 try
  Result:=True;
  
  if (AInitDelay > BOOTP_MIN_DELAY) and (AInitDelay <= BOOTP_MAX_DELAY) then FInitDelay:=AInitDelay;
  if (ARetryCount >= BOOTP_MIN_RETRIES) and (ARetryCount <= BOOTP_MAX_RETRIES) then FRetryCount:=ARetryCount;
  if (ARetryTimeout >= BOOTP_MIN_TIMEOUT) and (ARetryTimeout <= BOOTP_MAX_TIMEOUT) then FRetryTimeout:=ARetryTimeout;
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TARPConfig}
constructor TARPConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create(AManager);
 FConfigType:=CONFIG_TYPE_PSEUDO;
 FARP:=nil;
end;

{==============================================================================}

destructor TARPConfig.Destroy;
begin
 {}
 WriterLock;
 try
  FARP:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TARPConfig.ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Process a network config request from a Transport}
{Handle: The Handle of the Config Transport the request is from}
{Adapter: The transport adapter to perform configuration on}
{Command: The configuration command to perform}

{Note: Caller must hold the Adapter lock}
var
 Range:TInAddr;
 Address:TInAddr;
 Hardware:THardwareAddress;
 Transport:TARPConfigTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig: ConfigHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig:  Command = ' + ConfigCommandToString(ACommand));
 {$ENDIF}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get Transport}
 Transport:=TARPConfigTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check ARP Transport}
  if FARP = nil then Exit;
  
  {Check Command}
  case ACommand of
   CONFIG_ADAPTER_DISCOVER:begin
     {Perform Discover}
     {$IFDEF DHCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig: Attempting ARP Configuration');
     {$ENDIF}
     
     {Set Configuring}
     AAdapter.Configuring:=True;
     try
      {Check Address Family}
      case Transport.Transport.Family of
       AF_INET:begin
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Add the ARP Address}
         FARP.LoadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,AAdapter.Hardware,ADDRESS_TYPE_LOCAL);
         try
          Randomize;
          
          {Setup start of range (Random 192.168.100.1 to 192.168.100.100) or (Random Start to Start + 100)}
          LongWord(Address.S_addr):=ARP_CONFIG_START + LongWord(Random(75));
          if not TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Address) then LongWord(Address.S_addr):=LongWord(TIPTransportAdapter(AAdapter).Address.S_addr) + LongWord(Random(75));
          
          {Setup end of range (192.168.100.100) or (Start + 100)}
          LongWord(Range.S_addr):=ARP_CONFIG_STOP;
          if not TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Address) then LongWord(Range.S_addr):=LongWord(TIPTransportAdapter(AAdapter).Address.S_addr) + LongWord(100);
          Hardware:=HARDWARE_DEFAULT;
          
          {Check for Address in use}
          while FARP.ResolveAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,Address,Hardware) do
           begin
            {Move to Next}
            Inc(LongWord(Address.S_addr));
            
            {Check for end of range}
            if LongWord(Address.S_addr) > LongWord(Range.S_addr) then Exit;
           end;
          
          {Return the Address}
          TIPTransportAdapter(AAdapter).Address:=Address;
          
          {Return the Netmask}
          if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Netmask) then TIPTransportAdapter(AAdapter).Netmask:=IP_CLASSC_NETMASK;
          
          {Return Result}
          Result:=True;
         finally
          {Remove the ARP Address}
          FARP.UnloadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS);
         end;
        end;
      end;
     finally
      {Reset Configuring}
      AAdapter.Configuring:=False;
     end;
    end;
   CONFIG_ADAPTER_RELEASE:begin
     {Perform Release}
     Result:=True;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TARPConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this config}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TARPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport} 
  Transport:=TARPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add ARP Config}
       Handle:=TIPTransport(ATransport).AddConfig(CONFIG_TYPE_PSEUDO,True,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TARPConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_PSEUDO;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TARPConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this config}
{Transport: The transport to remove}
var
 Transport:TARPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport}
  Transport:=TARPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove ARP Config}
     if TIPTransport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TARPConfig.StartConfig:Boolean;
{Start this config ready for use}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig: StartConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Locate ARP Transport}
  FARP:=TARPTransport(Manager.Transports.GetTransportByType(AF_UNSPEC,PACKET_TYPE_ARP,False,NETWORK_LOCK_NONE)); //To Do //AddTransport ? //Some way to track this, applies to IP as well //Client ?
  if FARP = nil then Exit;
  
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TARPConfig.StopConfig:Boolean;
{Stop this config ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ARPConfig: StopConfig');
  {$ENDIF} 
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Remove ARP Transport}
  FARP:=nil;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TARPConfig.ProcessConfig:Boolean;
{Process periodic tasks for this config}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TRARPConfig}
constructor TRARPConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create(AManager);
 FConfigType:=CONFIG_TYPE_RARP;
 FRARP:=nil;
end;

{==============================================================================}

destructor TRARPConfig.Destroy;
begin
 {}
 WriterLock;
 try
  FRARP:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TRARPConfig.ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Process a network config request from a Transport}
{Handle: The Handle of the Config Transport the request is from}
{Adapter: The transport adapter to perform configuration on}
{Command: The configuration command to perform}

{Note: Caller must hold the Adapter lock}
var
 Address:TInAddr;
 Transport:TRARPConfigTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig: ConfigHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig:  Command = ' + ConfigCommandToString(ACommand));
 {$ENDIF}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get Transport}
 Transport:=TRARPConfigTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check RARP Transport}
  if FRARP = nil then Exit;
  
  {Check Command}
  case ACommand of
   CONFIG_ADAPTER_DISCOVER:begin
     {Perform Discover}
     {$IFDEF DHCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig: Attempting RARP Configuration');
     {$ENDIF}
     
     {Set Configuring}
     AAdapter.Configuring:=True;
     try
      {Check Address Family}
      case Transport.Transport.Family of
       AF_INET:begin
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Request Address}
         if not FRARP.ResolveHardware(AAdapter.Adapter,Address) then Exit;
         
         {Return the Address}
         TIPTransportAdapter(AAdapter).Address:=Address;
         
         {Return the Netmask (Guessed)}
         if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Netmask) then TIPTransportAdapter(AAdapter).Netmask:=IP_CLASSC_NETMASK;
         
         {Return Result}
         Result:=True;
        end;
      end;
     finally
      {Reset Configuring}
      AAdapter.Configuring:=False;
     end;
    end;
   CONFIG_ADAPTER_RELEASE:begin
     {Perform Release}
     Result:=True;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRARPConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this config}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TRARPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
 
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;

  {Get Transport} 
  Transport:=TRARPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add RARP Config}
       Handle:=TIPTransport(ATransport).AddConfig(CONFIG_TYPE_RARP,True,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TRARPConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_RARP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRARPConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this config}
{Transport: The transport to remove}
var
 Transport:TRARPConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport}
  Transport:=TRARPConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove RARP Config}
     if TIPTransport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRARPConfig.StartConfig:Boolean;
{Start this config ready for use}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig: StartConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Locate RARP Transport}
  FRARP:=TRARPTransport(Manager.Transports.GetTransportByType(AF_UNSPEC,PACKET_TYPE_RARP,False,NETWORK_LOCK_NONE)); //To Do //AddTransport ? //Some way to track this, applies to IP as well //Client ?
  if FRARP = nil then Exit;
  
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRARPConfig.StopConfig:Boolean;
{Stop this config ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RARPConfig: StopConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Remove RARP Transport}
  FRARP:=nil;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRARPConfig.ProcessConfig:Boolean;
{Process periodic tasks for this config}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TStaticConfig}
constructor TStaticConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create(AManager);
 FConfigType:=CONFIG_TYPE_STATIC;
 FARP:=nil;
end;

{==============================================================================}

destructor TStaticConfig.Destroy;
begin
 {}
 WriterLock;
 try
  FARP:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TStaticConfig.ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Process a network config request from a Transport}
{Handle: The Handle of the Config Transport the request is from}
{Adapter: The transport adapter to perform configuration on}
{Command: The configuration command to perform}

{Note: Caller must hold the Adapter lock}
var
 Transport:TStaticConfigTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig: ConfigHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig:  Command = ' + ConfigCommandToString(ACommand));
 {$ENDIF}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get Transport}
 Transport:=TStaticConfigTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check ARP Transport}
  if FARP = nil then Exit;
  
  {Check Command}
  case ACommand of
   CONFIG_ADAPTER_REQUEST:begin
     {Perform Request}
     {$IFDEF DHCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig: Attempting Static Configuration');
     {$ENDIF}
     
     {Set Configuring}
     AAdapter.Configuring:=True;
     try
      {Check Address Family}
      case Transport.Transport.Family of
       AF_INET:begin
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Check for Defaults}
         if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Address) then Exit;
         if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Netmask) then Exit;
         
         {Check the Address}
         if TIPTransport(Transport.Transport).GetAddressByAddress(TIPTransportAdapter(AAdapter).Address,False,NETWORK_LOCK_NONE) <> nil then Exit;
         
         {Add the ARP Address}
         FARP.LoadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,AAdapter.Hardware,ADDRESS_TYPE_LOCAL);
         try
          {Check for Address in use}
          if not FARP.ConfirmAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS,TIPTransportAdapter(AAdapter).Address) then Exit;
          
          {Advertise the Address} {Note: No one seems to listen to this}
          {FARP.AdvertiseAddress(AAdapter.Adapter,TIPTransportAdapter(AAdapter).Address);}
          
          {Return Result}
          Result:=True;
         finally
          {Remove the ARP Address}
          FARP.UnloadAddress(AAdapter.Adapter,IP_DEFAULT_ADDRESS);
         end;
        end;
       AF_INET6:begin
         
         //To Do
         
        end;
      end;
     finally
      {Reset Configuring}
      AAdapter.Configuring:=False;
     end;
    end;
   CONFIG_ADAPTER_RELEASE:begin
     {Perform Release}
     Result:=True;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TStaticConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this config}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TStaticConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport} 
  Transport:=TStaticConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add Static Config}
       Handle:=TIPTransport(ATransport).AddConfig(CONFIG_TYPE_STATIC,False,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TStaticConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_STATIC;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
     AF_INET6:begin
       {Add Static Config}
       Handle:=TIP6Transport(ATransport).AddConfig(CONFIG_TYPE_STATIC,False,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TStaticConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_STATIC;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TStaticConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this config}
{Transport: The transport to remove}
var
 Transport:TStaticConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport}
  Transport:=TStaticConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove Static Config}
     if TIPTransport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
   AF_INET6:begin
     {Remove Static Config}
     if TIP6Transport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TStaticConfig.StartConfig:Boolean;
{Start this config ready for use}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig: StartConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Locate ARP Transport}
  FARP:=TARPTransport(Manager.Transports.GetTransportByType(AF_UNSPEC,PACKET_TYPE_ARP,False,NETWORK_LOCK_NONE)); //To Do //AddTransport ? //Some way to track this, applies to IP as well //Client ?
  if FARP = nil then Exit;
  
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Register with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TStaticConfig.StopConfig:Boolean;
{Stop this config ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'StaticConfig: StopConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Deregister with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Remove ARP Transport}
  FARP:=nil;
 
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TStaticConfig.ProcessConfig:Boolean;
{Process periodic tasks for this config}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{TLoopbackConfig}
constructor TLoopbackConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create(AManager);
 FConfigType:=CONFIG_TYPE_LOOPBACK;
end;

{==============================================================================}

destructor TLoopbackConfig.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TLoopbackConfig.ConfigHandler(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean;
{Process a network config request from a Transport}
{Handle: The Handle of the Config Transport the request is from}
{Adapter: The transport adapter to perform configuration on}
{Command: The configuration command to perform}

{Note: Caller must hold the Adapter lock}
var
 Transport:TLoopbackConfigTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF DHCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig: ConfigHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig:  Command = ' + ConfigCommandToString(ACommand));
 {$ENDIF}
 
 {Check Adapter}
 if AAdapter = nil then Exit;
 
 {Get Transport}
 Transport:=TLoopbackConfigTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {Check Command}
  case ACommand of
   CONFIG_ADAPTER_REQUEST:begin
     {Perform Request}
     {$IFDEF DHCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig: Attempting Loopback Configuration');
     {$ENDIF}
     
     {Set Configuring}
     AAdapter.Configuring:=True;
     try
      {Check Address Family}
      case Transport.Transport.Family of
       AF_INET:begin
         {Check the Adapter}
         if AAdapter.Configured then Exit;
         
         {Check for Defaults}
         if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Address) then Exit;
         if TIPTransport(Transport.Transport).CompareDefault(TIPTransportAdapter(AAdapter).Netmask) then Exit;
         
         {Check for Loopback}
         if not TIPTransport(Transport.Transport).CompareLoopback(TIPTransportAdapter(AAdapter).Address) then Exit;
         
         {Check the Address}
         if TIPTransport(Transport.Transport).GetAddressByAddress(TIPTransportAdapter(AAdapter).Address,False,NETWORK_LOCK_NONE) <> nil then Exit;
         
         {Return Result}
         Result:=True;
        end;
       AF_INET6:begin
        
         //To Do
        
        end;
      end;
     finally
      {Reset Configuring}
      AAdapter.Configuring:=False;
     end;
    end;
   CONFIG_ADAPTER_RELEASE:begin
     {Perform Release}
     Result:=True;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this config}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TLoopbackConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Get Transport} 
  Transport:=TLoopbackConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add Loopback Config}
       Handle:=TIPTransport(ATransport).AddConfig(CONFIG_TYPE_LOOPBACK,False,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TLoopbackConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_LOOPBACK;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
     AF_INET6:begin
       {Add Loopback Config}
       Handle:=TIP6Transport(ATransport).AddConfig(CONFIG_TYPE_LOOPBACK,False,ConfigHandler);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TLoopbackConfigTransport.Create;
         Transport.Handle:=Handle;
         Transport.ConfigType:=CONFIG_TYPE_LOOPBACK;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Return Result}
          Result:=True;
         finally 
          {Release Lock}
          FTransports.WriterUnlock;
         end; 
        end;
      end;
    end;
   end
  else
   begin
    {Unlock Transport}
    Transport.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this config}
{Transport: The transport to remove}
var
 Transport:TLoopbackConfigTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
 
  {Get Transport}
  Transport:=TLoopbackConfigTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Remove Loopback Config}
     if TIPTransport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
   AF_INET6:begin
     {Remove Loopback Config}
     if TIP6Transport(ATransport).RemoveConfig(Transport.Handle,Transport.ConfigType) then
      begin
       {Acquire Lock}
       FTransports.WriterLock;
       try
        {Remove Transport}
        FTransports.Remove(Transport);
       
        {Unlock Transport}
        Transport.WriterUnlock;
       
        {Destroy Transport}
        Transport.Free;
       
        {Return Result}
        Result:=True;
       finally 
        {Release Lock}
        FTransports.WriterUnlock;
       end; 
      end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackConfig.StartConfig:Boolean;
{Start this config ready for use}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig: StartConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Register with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Register with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Add Transport}
    AddTransport(Transport);
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackConfig.StopConfig:Boolean;
{Stop this config ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF DHCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'LoopbackConfig: StopConfig');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Deregister with IP Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ); 
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Deregister with IP6 Transport}
  Transport:=Manager.Transports.GetTransportByType(AF_INET6,PACKET_TYPE_IP6,True,NETWORK_LOCK_READ);
  if Transport <> nil then
   begin
    {Remove Transport}
    RemoveTransport(Transport);
    
    {Unlock Transport}
    Transport.ReaderUnlock;
   end; 
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TLoopbackConfig.ProcessConfig:Boolean;
{Process periodic tasks for this config}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DHCPInit;
begin
 {}
 {Check Initialized}
 if DHCPInitialized then Exit;
 
 {Setup Loopback Config}
 if NetworkSettings.GetBoolean('LOOPBACK_NETWORK_ENABLED') then NetworkSettings.AddBoolean('LOOPBACK_CONFIG_ENABLED',True);
 
 {Create Loopback Config}
 if NetworkSettings.GetBooleanDefault('LOOPBACK_CONFIG_ENABLED',LOOPBACK_CONFIG_ENABLED) then
  begin
   TLoopbackConfig.Create(ProtocolManager);
  end; 
 
 {Create Static Config}
 if NetworkSettings.GetBooleanDefault('STATIC_CONFIG_ENABLED',STATIC_CONFIG_ENABLED) then
  begin
   TStaticConfig.Create(ProtocolManager);
  end; 
 
 {Create DHCP Config}
 if NetworkSettings.GetBooleanDefault('DHCP_CONFIG_ENABLED',DHCP_CONFIG_ENABLED) then  
  begin
   TDHCPConfig.Create(ProtocolManager);
  end; 
 
 {Create BOOTP Config}
 if NetworkSettings.GetBooleanDefault('BOOTP_CONFIG_ENABLED',BOOTP_CONFIG_ENABLED) then  
  begin
   TBOOTPConfig.Create(ProtocolManager);
  end; 
 
 {Create RARP Config}
 if NetworkSettings.GetBooleanDefault('RARP_CONFIG_ENABLED',RARP_CONFIG_ENABLED) then  
  begin
   TRARPConfig.Create(ProtocolManager);
  end; 
 
 {Create ARP Config}
 if NetworkSettings.GetBooleanDefault('ARP_CONFIG_ENABLED',ARP_CONFIG_ENABLED) then  
  begin
   TARPConfig.Create(ProtocolManager);
  end; 
 
 DHCPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DHCP Functions}
  
{==============================================================================}
{==============================================================================}
{DHCP Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 DHCPInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 