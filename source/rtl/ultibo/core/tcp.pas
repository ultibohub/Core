{
Ultibo TCP (Transmission Control Protocol) unit.

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

 
Transmission Control Protocol
=============================

 Notes: Segment handling:
         Send adds new segments to the end of the list
              only accepts ACKs for whole segments
         Recv adds/inserts segments in correct SEQ order
              accepts overlapping segments
              only sends ACKs for whole segments

         Send will coalesce small data writes into larger
              segments if options allow this
         Recv will store the segments exactly as received

 Notes: TCP Send and Recv do not use a block buffer as per
        UDP etc. Instead the data for each Segment is stored
        in memory (TCP_MAX_MSS) allocated with the Segment.
        The Free and Used values still track the amount of
        data in the buffer but this method allows OOB data
        to be handled correctly

        TCP Sequence numbers are compared using modulo 2^32
        arithmetic. The following calculations provide the
        neccessary handling of wraparound etc.

        LT   = (A - B) < 0
        LEQ  = (A - B) <= 0
        GT   = (A - B) > 0
        GEQ  = (A - B) >= 0

        See GlobalSock.pas for actual functions that implement this.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit TCP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,SysUtils,Classes,Network,Transport,Protocol,IP,IPv6,ICMP,ICMPv6,Ultibo,UltiboClasses;
                                                   
//To Do //Look for:

//Testing

//Critical 

//ASocket.SignalChange; - Done

//SignalChange - Done

//WaitChangeEx - Done

//WaitChange - Done


//ScheduleSocket

//SendSocket

//ReceiveSocket


//WindowSize Testing

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {TCP specific constants}
 {Note: Some TCP definitions are in the Protocol or IP modules}
 TCP_PROTOCOL_NAME = 'TCP';
 
 TCP_TIMER_INTERVAL = 1;    {1ms timer interval for TCP}
 
 {TCP Constants}
 MIN_TCP_PACKET = 20;       {Not Counting Adapter and Transport Header}
 MAX_TCP_PACKET = 1480;     {Not Counting Adapter and Transport Header}

 TCP_DEFAULT_MSS = 536;     {Default Segment Size for TCP (576 - TCP - IP)}
 TCP_MAX_MSS     = 1460;    {Max Segment Size for TCP (1500 - TCP - IP)}

 TCP_MAX_LIFETIME = 120000; {Max Segment Lifetime (2 minutes)}

 TCP_TIMEOUT = 0;           {Wait forever on a TCP Read/Write}
 TCP_BUFFER_SIZE = 262144;  {65536;} {TCP Send/Receive Buffer Sizes}
 TCP_WINDOW_SIZE = 64240;   {Sliding Window Receive Size (44 x MSS)}
 TCP_INITIAL_WINDOW = 8192; {Initial Window to advertise in SYN packet} 
 
 TCP_TIMEOUT_OFFSET = 10;   {How much time offset to allow from the designated value}
 
 TCP_ADVERT_TIMEOUT = 100;  {Time to wait before sending a window size update after closing the receive window}
 
 TCP_WINDOW_TIMEOUT = 5000; {Time to wait before probing small/zero Window}
 TCP_ACK_TIMEOUT = 200;     {Max Delayed Ack Time}

 TCP_CONNECT_COUNT = 3;     {Number of Connect retries (on RST) before Failure}
 TCP_RESTART_TIMEOUT = 400; {Timeout between Connect retries (Milliseconds)}
 
 TCP_RETRY_COUNT = 10;      {Number of Retransmits before Reset}
 TCP_RETRY_TIMEOUT:array[1..TCP_RETRY_COUNT] of LongWord = (250,500,1000,2000,4000,8000,15000,30000,60000,120000);
 
 TCP_KEEPALIVE_COUNT = 3;   {Number of Failed Keepalives before Reset}
 TCP_KEEPALIVE_TIMEOUT:array[1..TCP_KEEPALIVE_COUNT] of LongWord = (600000,60000,60000);

 TCP_MAX_PORT = 65536;

 TCP_HEADER_SIZE  = 20;     {SizeOf(TTCPHeader);} {Does Not Allow for Options}
 TCP_OPTIONS_SIZE = 40;     {Maximum Allowed Options}

 TCP_SEGMENT_SIZE = 68;     {SizeOf(TTCPSegment)} {Including TSocketTimerItem}

 {TCP Socket Options}
 {See Sockets}

 {TCP Socket States}
 TCP_STATE_LISTEN  = 0;      // listening for connection
 TCP_STATE_SYNSENT = 1;      // SYN sent, active open
 TCP_STATE_SYNREC  = 2;      // SYN received, SYNACK sent
 TCP_STATE_ESTAB   = 3;      // established
 TCP_STATE_FINWAIT1  = 4;    // sent FIN
 TCP_STATE_FINWAIT2  = 5;    // sent FIN, received FINACK
 TCP_STATE_CLOSWAIT  = 6;    // received FIN waiting for close
 TCP_STATE_CLOSING = 7;      // sent FIN, received FIN (waiting for FINACK)
 TCP_STATE_LASTACK = 8;      // FIN received, FINACK + FIN sent
 TCP_STATE_TIMEWAIT  = 9;    // dally after sending final FINACK
 TCP_STATE_CLOSED  = 10;     // FINACK received

 {TCP Header Options}
 TCPOPT_EOL       = 0;       // end-of-option list
 TCPOPT_NOP       = 1;       // no-operation
 TCPOPT_MAXSEG    = 2;       // maximum segment size
 TCPOPT_WINDOW    = 3;       // window scale factor (rfc1072)
 TCPOPT_SACKOK    = 4;       // selective ack ok (rfc1072)
 TCPOPT_SACK      = 5;       // selective ack (rfc1072)
 TCPOPT_ECHO      = 6;       // echo (rfc1072)
 TCPOPT_ECHOREPLY = 7;       // echo (rfc1072)
 TCPOPT_TIMESTAMP = 8;       // timestamps (rfc1323)
 TCPOPT_CC        = 11;      // T/TCP CC options (rfc1644)
 TCPOPT_CCNEW     = 12;      // T/TCP CC options (rfc1644)
 TCPOPT_CCECHO    = 13;      // T/TCP CC options (rfc1644)

 {TCP Header Flags}
 TCP_FLAG_FIN =  $01;
 TCP_FLAG_SYN =  $02;
 TCP_FLAG_RST =  $04;
 TCP_FLAG_PUSH = $08;
 TCP_FLAG_ACK =  $10;
 TCP_FLAG_URG =  $20;
 TCP_FLAG_MASK = $3F;

 {Van Jacobson's Algorithm; max std. average and std. deviation}
 MAX_VJSA   = 80000;
 MAX_VJSD   = 20000;
 INIT_VJSA  = 220;

 TCP_PORT_START = 49152;   {First dynamic port (Previously 1024)} {As per IANA assignment}
 TCP_PORT_STOP  = 65534;   {Last dynamic port (Previously 5000)}  {Short of IANA assignment to allow for rollover} 
  
{==============================================================================}
type
 {TCP specific types}
 {Note: Some TCP definitions are in the Protocol or IP modules}
 PTCPHeader = ^TTCPHeader;
 TTCPHeader = packed record  {20 Bytes unless TCP Options are added}
  SourcePort:Word;         {Network Order}
  DestPort:Word;           {Network Order}
  Sequence:LongWord;       {First Sequence number in Data}
  Acknowledge:LongWord;    {Next Sequence number expected}
  HeaderLength:Byte;       {Number of 32 bit words in Header and Options} {Bits 4-7 Reserved}
  Flags:Byte;              {Flags for TCP Control} {Bits 1-2 Reserved}
  WindowSize:Word;         {Max 65536} {See WindowScale for higher values}
  Checksum:Word;           {As per standard (Header, Options and Data)}
  Urgent:Word;
 end;

 PTCPSegment = ^TTCPSegment;
 TTCPSegment = record  {48 Bytes} {Used by TCPSendBuffer/TCPRecvBuffer}
  Size:Word;               {Size of Segment Data (Can be zero)}
  Data:Pointer;            {Pointer to Data in Buffer (Can be nil)}
  FirstSequence:LongWord;  {Start Sequence of Data}
  LastSequence:LongWord;   {End Sequence of Data (including Control bits)(actually the Start Sequence of next Segment)}

  Control:Word;            {Control Bits on this segment (SYN/FIN/URG) (Send/Recv)}
  Transferred:WordBool;    {Data has been Transferred to/from Buffer (Send/Recv)}
  Acknowledged:WordBool;   {Data has been Acknowledged (Send/Recv)}
  SelectiveAck:WordBool;   {Data has been Selective Acknowledged (Send/Recv)}
  RoundTripTime:Int64;     //To Do //maybe in SendBuffer as well RTT/RTO see 1122 ??

  Count:Word;              {Transmit Count (Send)}
  Timeout:Int64;           {Transmit (Send) / Acknowledge (Recv) Timeout}
  Prev:PTCPSegment;        {Pointer to Prev Segment}
  Next:PTCPSegment;        {Pointer to Next Segment}
  
  Item:TSocketTimerItem;   {Socket Timer Item for this Segment}
 end;
  
{==============================================================================}
type
 {TCP specific classes}
 TTCPSocket = class;
 TTCPProtocolTransport = class(TProtocolTransport)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}

  public
   {Status Variables}
   Socket:TTCPSocket;   {Socket for responding to non matched segments}
 end;

 TTCPProtocol = class(TNetworkProtocol)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FNextPort:Word;
   FMinBacklog:Integer;
   FMaxBacklog:Integer;
   FReceiveBacklog:Integer;
   
   {Status Variables}
      
   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
   function SegmentHandler(ASocket:TTCPSocket;ASource,ADest,APacket:Pointer;ASize:Integer):Boolean;
   function ResetHandler(ASocket:TTCPSocket;ASource,ADest,APacket:Pointer;ASize:Integer):Boolean;

   function CloneSocket(ASocket:TTCPSocket;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ALock:Boolean;AState:LongWord):TTCPSocket;

   function GetTCPOptionsSize(ASocket:TTCPSocket;AOptions:Pointer):Word;

   function CreateTCPOptions(ASocket:TTCPSocket;AOptions:Pointer;AFlags:Byte):Boolean;
   function HandleTCPOptions(ASocket:TTCPSocket;AOptions:Pointer;AFlags:Byte):Boolean;

   function InsertTCPOption(ASocket:TTCPSocket;AOptions:Pointer;AOption:Byte):Boolean;
   function ExtractTCPOption(ASocket:TTCPSocket;AOptions:Pointer;AOption:Byte):Boolean;

   function SendReset(ASocket:TTCPSocket):Boolean;
   function SendAcknowledge(ASocket:TTCPSocket):Boolean;

   function SendSegment(ASocket:TTCPSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;ASequence,AAcknowledge:LongWord;AWindow,AUrgent:Word;AFlags:Byte;AOptions,AData:Pointer;ASize:Integer):Integer;
  protected
   {Inherited Methods}
   function OpenPort(ASocket:TProtocolSocket;APort:Word):Boolean; override;
   function ClosePort(ASocket:TProtocolSocket):Boolean; override;
   function FindPort(APort:Word;AWrite,ALock:Boolean):TProtocolPort; override;

   function SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer; override;
   function SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; override;
  public
   {Public Properties}

   {BSD Socket Methods}
   function Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket; override;
   function Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer; override;
   function CloseSocket(ASocket:TProtocolSocket):Integer; override;
   function Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer; override;
   function IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer; override;
   function GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer; override;
   function GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer; override;
   function GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; override;
   function Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer; override;
   function Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer; override;
   function RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer; override;
   {function Select(ANfds:Integer;AReadfds,AWritefds,AExceptfds:PFDSet;ATimeout:PTimeVal):LongInt; override;}
   function Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer; override;
   function SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer; override;
   function SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; override;
   function Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer; override;
   function Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket; override;

   {Public Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean; override;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; override;
   
   function FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket; override;
   procedure FlushSockets(All:Boolean); override;
   
   function StartProtocol:Boolean; override;
   function StopProtocol:Boolean; override;
   function ProcessProtocol:Boolean; override;
   
   function ProcessSockets:Boolean; override;
   function ProcessSocket(ASocket:TProtocolSocket):Boolean; override;
 end;

 TTCPState = class;
 TTCPOptions = class;
 TTCPSendBuffer = class;
 TTCPRecvBuffer = class;
 TTCPSocket = class(TProtocolSocket)  {SOCK_STREAM}
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   
   {TCP Layer Variables}
   FBackLog:Integer;          {Backlog size of Listen/Accept Queue}
   FListener:TProtocolSocket; {The Listener socket for this socket (If applicable)}
   FAcceptQueue:TSocketList;  {Sockets waiting for connection Accept}
   FReceiveQueue:TSocketList; {Sockets waiting for SYN/ACK handshake}

   {Data Layer Variables}
   FSendData:TTCPSendBuffer;
   FRecvData:TTCPRecvBuffer;
   
   {Internal Methods}
   procedure SetBackLog(ABackLog:Integer);
   procedure SetListener(AListener:TProtocolSocket);
  public
   {TCP Layer Properties}
   property BackLog:Integer read FBackLog write SetBackLog;
   property Listener:TProtocolSocket read FListener write SetListener;
   property AcceptQueue:TSocketList read FAcceptQueue;
   property ReceiveQueue:TSocketList read FReceiveQueue;

   {Data Layer Properties}
   property SendData:TTCPSendBuffer read FSendData;
   property RecvData:TTCPRecvBuffer read FRecvData;

   {Public Methods}
   function GetOption(ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; override;
   function SetOption(ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; override;
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; override;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; override;

   {Handshake Handling}
   function Listen:Boolean;
   function Connect:Boolean;
   function Reconnect:Boolean;
   function Disconnect:Boolean;

   {Queue Handling}
   function Accept(APeek,ALock:Boolean;AState:LongWord):TTCPSocket;

   {Segment Handling}
   function RecvSegment(ASequence,AAcknowledge:LongWord;AWindow,AUrgent:Word;AFlags:Byte;AData:Pointer;ASize:Word):Boolean;
   function SendSegment(var ASequence,AAcknowledge:LongWord;var AWindow,AUrgent:Word;var AFlags:Byte;var AData:Pointer;var ASize:Word):Boolean;
 end;

 TTCPState = class(TProtocolState)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FState:LongWord;

   {Socket States}
   procedure SetState(AState:LongWord);
   
   function GetListening:Boolean;
   procedure SetListening(AValue:Boolean);
   //To Do //
   //function GetSynSent:Boolean;
   //procedure SetSynSent(AValue:Boolean);
   //function GetSynReceived:Boolean;
   //procedure SetSynReceived(AValue:Boolean);
   function GetEstablished:Boolean;
   procedure SetEstablished(AValue:Boolean);
   //To Do
   //function GetFinWait1:Boolean;
   //procedure SetFinWait1(AValue:Boolean);
   //function GetFinWait2:Boolean;
   //procedure SetFinWait2(AValue:Boolean);
   //function GetCloseWait:Boolean;
   //procedure SetCloseWait(AValue:Boolean);
   //function GetClosing:Boolean;
   //procedure SetClosing(AValue:Boolean);
   //function GetLastAck:Boolean;
   //procedure SetLastAck(AValue:Boolean);
   //function GetTimeWait:Boolean;
   //procedure SetTimeWait(AValue:Boolean);
   function GetClosed:Boolean;
   procedure SetClosed(AValue:Boolean);

   {Connection States}
   function GetSynchronized:Boolean;
  public
   {Public Properties}
   property State:LongWord read FState write SetState;

   {Socket States}
   property Listening:Boolean read GetListening write SetListening;
   //To Do
   //property SynSent:Boolean read GetSynSent write SetSynSent;
   //property SynReceived:Boolean read GetSynReceived write SetSynReceived;
   property Established:Boolean read GetEstablished write SetEstablished;
   //To Do
   //property FinWait1:Boolean read GetFinWait1 write SetFinWait1;
   //property FinWait2:Boolean read GetFinWait2 write SetFinWait2;
   //property CloseWait:Boolean read GetCloseWait write SetCloseWait;
   //property Closing:Boolean read GetClosing write SetClosing;
   //property LastAck:Boolean read GetLastAck write SetLastAck;
   //property TimeWait:Boolean read GetTimeWait write SetTimeWait;
   property Closed:Boolean read GetClosed write SetClosed;

   {Connection States}
   property Synchronized:Boolean read GetSynchronized;
 end;

 TTCPOptions = class(TProtocolOptions) {For Get/Set Options Level = TCP_PROTO Option = TCP_NODELAY, TCP_MAXSEG etc}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FMemory:TMemoryStream; //To Do //Should this be a TMemoryStreamEx ?
   FOptions:LongWord;

   {TCP Layer Variables}
   FMaxSeg:Word;          {MSS Option in TCP Header} {MSS we send to Remote}
   FWindowScale:Byte;     {Window Scale Option in TCP Header} {Window Scale we send to Remote}
   
   {Internal Methods}
   procedure SetMaxSeg(AMaxSeg:Word); 
   procedure SetWindowScale(AWindowScale:Byte);
   
   function GetOptions:Pointer;
   function GetNoDelay:Boolean;
   procedure SetNoDelay(ANoDelay:Boolean);
   function GetNoPush:Boolean;
   procedure SetNoPush(ANoPush:Boolean);
   function GetNoOpt:Boolean;
   procedure SetNoOpt(ANoOpt:Boolean);
   function GetBsdUrgent:Boolean;
   procedure SetBsdUrgent(ABsdUrgent:Boolean);
   function GetNoSack:Boolean;
   procedure SetNoSack(ANoSack:Boolean);
  public
   {TCP Layer Properties}
   property MaxSeg:Word read FMaxSeg write SetMaxSeg;
   property WindowScale:Byte read FWindowScale write SetWindowScale;

   {Public Properties}
   property Options:Pointer read GetOptions;    //To Do //Does this need lock protection after reading (Copy on read etc) ?
   property NoDelay:Boolean read GetNoDelay write SetNoDelay; {Determines if we Delay small segments until previous sends are ACKed}
   property NoPush:Boolean read GetNoPush write SetNoPush;
   property NoOpt:Boolean read GetNoOpt write SetNoOpt;
   property BsdUrgent:Boolean read GetBsdUrgent write SetBsdUrgent;
   property NoSack:Boolean read GetNoSack write SetNoSack; {Determines if we send SACKOK to Remote}
 end;

 TTCPSendBuffer = class(TSocketBuffer)
   constructor Create(ASocket:TTransportSocket);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}
   FFirst:PTCPSegment;         {Pointer to First Segment}
   FLast:PTCPSegment;          {Pointer to Last Segment}

   {Internal Methods}
   function AddSegment(ASequence:LongWord;AFlags:Byte;ASize:Word):PTCPSegment;
   function RemoveSegment(ASegment:PTCPSegment):Boolean;
   procedure FlushSegments(All:Boolean);
  protected
   {Inherited Methods}
   procedure SetSize(ASize:LongWord); override;
  public
   {Status Variables}
   StartSequence:LongWord;     {Initial Sequence number of Local (ISS)}
   NextSequence:LongWord;      {Next Sequence number to Send (SND.NXT}
   LastSequence:LongWord;      {Last Sequence number in Buffer}
   LastAcknowledge:LongWord;   {Last Acknowledged number from Remote (SND.UNA)}
   {Note: LastAcknowledge is the NextSequence that Remote expects to Receive}
   {      or the first Unacknowledged Sequence number. If LastAcknowledge}
   {      equals NextSequence all segments have been Acknowledged}
   {      LastSequence is the last sequence in our buffer to send. If}
   {      NextSequence equals LastSequence there is no more data to send}

   UrgentPointer:LongWord;     {(SND.UP)}

   MaxSeg:Word;                {Remote Max Segment Size (Learned from Remote)}

   WindowSize:LongWord;        {Remote Window Size   (SND.WND)}
   WindowScale:Byte;           {Remote Window Scale}
   WindowTimeout:Int64;        {Timeout for probing small/zero Window}

   CongestionWindow:Word;      {Slow Start Window Size}

   SynSequence:LongWord;       {Sequence number of our SYN}
   FinSequence:LongWord;       {Sequence number of our FIN}

   WindowSequence:LongWord;    {Sequence number of last Window update (SND.WL1)}
   WindowAcknowledge:LongWord; {Acknowledge number of last Window update (SND.WL2)}

   NoPush:Boolean;             {Disable Push on final segment}
   NoSack:Boolean;             {Selective Ack not in use}
   NoNagle:Boolean;            {Nagle not in use}

   VjSa:LongWord;              {VJ's alg, standard average}
   VjSd:LongWord;              {VJ's alg, standard deviation}
   VjLast:LongWord;            {VJ's alg, last transmit time}

   {Public Methods}
   function CheckIdle:Boolean;
   
   function SynAcknowledged:Boolean;
   function FinAcknowledged:Boolean;

   function TestAcknowledge(AAcknowledge:LongWord):Boolean;
   function CheckAcknowledge(AAcknowledge:LongWord):Boolean;
   function ValidateAcknowledge(AAcknowledge:LongWord):Boolean;

   function WriteData(var ABuffer;ASize,AFlags:Integer):Boolean;

   function Finish:Boolean;
   function Synchronize:Boolean;

   function ReadSegment(var ASequence:LongWord;var AUrgent:Word;var AFlags:Byte;var AData:Pointer;var ASize:Word;AForce:Boolean):Boolean;
   function AcknowledgeSegments(ASequence,AAcknowledge:LongWord;AWindow:Word):Boolean;

   function TimestampSegment(AOptions:Pointer;var AOffset:Word):Boolean;
   function SelectiveAckSegments(AOptions:Pointer;var AOffset:Word;ASize:Byte):Boolean;
 end;

 TTCPRecvBuffer = class(TSocketBuffer)
   constructor Create(ASocket:TTransportSocket);
   destructor Destroy; override;
  private
   {Internal Variables}

   {Status Variables}
   FUrgent:LongWord;           {Number of OOB bytes readable from Buffer}
   FAvailable:LongWord;        {Number of bytes readable from Buffer}

   FFirst:PTCPSegment;         {Pointer to First Segment}
   FLast:PTCPSegment;          {Pointer to Last Segment}

   {Internal Methods}
   function DelayOverride(ASegment:PTCPSegment):Boolean;

   function GetSegment(ASequence:LongWord;ALength:Word):PTCPSegment;
   function GetPrevious(ASequence:LongWord;ALength:Word):PTCPSegment;
   function GetOverlapped(ASequence:LongWord;ALength:Word):PTCPSegment;

   function AddSegment(APrev:PTCPSegment;ASequence:LongWord;AFlags:Byte;ASize:Word):PTCPSegment;
   function RemoveSegment(ASegment:PTCPSegment):Boolean;
   procedure FlushSegments(All:Boolean);
  protected
   {Inherited Methods}
   procedure SetSize(ASize:LongWord); override;
  public
   {Status Variables}
   StartSequence:LongWord;     {Initial Sequence number of Remote (IRS)}
   NextSequence:LongWord;      {Next Sequence number to Receive (RCV.NXT)}
   LastSequence:LongWord;      {Last Sequence number in Buffer (Including Unacknowledged data)}
   LastAcknowledge:LongWord;   {Last Acknowledged number to Remote}
   {Note: NextSequence is the next byte we expect to Receive from Remote}
   {      LastAcknowledge will be the first Unacknowledged Seqeunce number}
   {      of the Remote. LastSequence is the last sequence we have received}
   {      from the Remote. If NextSequence equals LastSequence and LastAcknowledge}
   {      equals NextSequence all segments have been Acknowledged}

   UrgentPointer:LongWord;     {(RCV.UP)}

   MaxSeg:Word;                {Local Max Segment Size}

   WindowSize:LongWord;        {Local Window Size (RCV.WND)}
   WindowScale:Byte;           {Local Window Scale}
   LastWindow:LongWord;        {Last Window value sent to Remote}
   
   SynSequence:LongWord;       {Sequence number of remote SYN}
   FinSequence:LongWord;       {Sequence number of remote FIN}

   NoSack:Boolean;             {Selective Ack not in use}

   {Public Methods}
   function CheckIdle:Boolean;

   function GetUrgent:LongWord;
   function GetAvailable:LongWord;

   function SynReceived:Boolean;
   function FinReceived:Boolean;

   function CheckSequence(ASequence:LongWord;ASize:Word):Boolean;

   function ReadData(var ABuffer;var ASize:Integer;AFlags:Integer):Boolean;

   function WriteSegment(ASequence:LongWord;AUrgent:Word;AFlags:Byte;AData:Pointer;ASize:Word):Boolean;
   function AcknowledgeSegments(var AAcknowledge:LongWord;var AWindow:Word;AForce:Boolean):Boolean;

   function TimestampSegment(AOptions:Pointer;var AOffset:Word):Boolean;
   function SelectiveAckSegments(AOptions:Pointer;var AOffset:Word):Boolean;
 end;
  
{==============================================================================}
{var}
 {TCP specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure TCPInit;

{==============================================================================}
{TCP Functions}
function CheckTCP(AFamily:Word;ABuffer:Pointer):Boolean;

function GetTCPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetTCPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
function GetTCPOptionsLength(AFamily:Word;ABuffer:Pointer):Word;
function GetTCPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
function GetTCPDataLength(AFamily:Word;ABuffer:Pointer):Word;

function ChecksumTCPRecv(AFamily:Word;APseudo:PIPPseudo;ABuffer:Pointer;AOffset,ALength:Word):Word;
function ChecksumTCPSend(AFamily:Word;APseudo:PIPPseudo;AHeader:PTCPHeader;AOptions,AData:Pointer;AOptionsLength,ADataLength:Word):Word;
  
{==============================================================================}
{TCP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {TCP specific variables}
 TCPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{TTCPProtocolTransport}
constructor TTCPProtocolTransport.Create;
begin
 {}
 inherited Create;
 Socket:=nil;
end;

{==============================================================================}

destructor TTCPProtocolTransport.Destroy;
begin
 {}
 WriterLock;
 try
  Socket:=nil;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TTCPProtocol}
constructor TTCPProtocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FProtocol:=IPPROTO_TCP;
 FSocketType:=SOCK_STREAM;

 FNextPort:=TCP_PORT_START;
end;

{==============================================================================}

destructor TTCPProtocol.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TTCPProtocol.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by a Transport}
{Handle: The Handle of the Protocol Transport the packet was received from}
{Source: The source address of the received packet (Set by Transport)}
{Dest: The destination address of the received packet (Set by Transport)}
{Packet: The received packet (The complete packet including Transport header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 IP:PIPHeader;
 IP6:PIP6Header;
 TCP:PTCPHeader;
 ICMP:PICMPHeader;
 ICMP6:PICMP6Header;
 Socket:TTCPSocket;
 Transport:TTCPProtocolTransport;
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Dest}
 {if ADest = nil then Exit;} {Not Used}
 
 {Check Source}
 {if ASource = nil then Exit;} {Not Used} 
  
 {Check Packet}
 if APacket = nil then Exit;
 
 {Get Transport}
 Transport:=TTCPProtocolTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Family = ' + AddressFamilyToString(Transport.Transport.Family));
  {$ENDIF}
 
  {Check Transport Family}
  case Transport.Transport.Family of
   AF_INET:begin
     {Get Header}  
     IP:=PIPHeader(APacket);
    
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}
    
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_TCP:begin
        {Check for a valid TCP Packet}
        if CheckTCP(AF_INET,IP) then
         begin
          {Get Header}
          TCP:=PTCPHeader(LongWord(IP) + GetTCPHeaderOffset(AF_INET,IP));
          
          {Set the Ports to Host order}
          TCP.DestPort:=WordBEtoN(TCP.DestPort);
          TCP.SourcePort:=WordBEtoN(TCP.SourcePort);
          
          {$IFDEF TCP_DEBUG}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  DestPort = ' + IntToStr(TCP.DestPort));
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  SourcePort = ' + IntToStr(TCP.SourcePort));
          {$ENDIF}
          
          {Set the Fields to Host order}
          TCP.Sequence:=LongWordBEtoN(TCP.Sequence);
          TCP.Acknowledge:=LongWordBEtoN(TCP.Acknowledge);
          TCP.WindowSize:=WordBEtoN(TCP.WindowSize);
          TCP.Urgent:=WordBEtoN(TCP.Urgent);
          
          {Check for a Connected Socket}
          Socket:=TTCPSocket(FindSocket(AF_INET,SOCK_STREAM,IPPROTO_TCP,@IP.DestIP,@IP.SourceIP,TCP.DestPort,TCP.SourcePort,ABroadcast,False,True,NETWORK_LOCK_READ));
          if Socket = nil then
           begin
            {Check for a Listening Socket}
            Socket:=TTCPSocket(FindSocket(AF_INET,SOCK_STREAM,IPPROTO_TCP,@IP.DestIP,@IP.SourceIP,TCP.DestPort,TCP.SourcePort,ABroadcast,True,True,NETWORK_LOCK_READ));
           end;
          if Socket <> nil then
           begin
            {Call Segment Handler}
            if not ABroadcast then
             begin
              {Process Segment}
              SegmentHandler(Socket,@IP.SourceIP,@IP.DestIP,IP,ASize);
              
              {Return True even if the Handler failed (eg Sequence/Acknowledge is wrong, Socket is Closed etc)}
              Result:=True;
             end;
            
            {Unlock Socket}
            Socket.ReaderUnlock;
           end
          else
           begin
            {Call Reset Handler (Transport Socket)}
            if not ABroadcast then
             begin
              {Lock Socket}
              Transport.Socket.ReaderLock;
              
              {Send Reset}
              ResetHandler(Transport.Socket,@IP.SourceIP,@IP.DestIP,IP,ASize);
              
              {Return True even if the Handler failed (eg Reset was Set etc)}
              Result:=True;
              
              {Unlock Socket}
              Transport.Socket.ReaderUnlock;
             end;
           end;
         end
        else
         begin
          {Silently consume and discard a bad TCP packet}
          Result:=True;
         end;
       end;
      IPPROTO_ICMP:begin
        {Check for a valid ICMP Packet}
        if CheckICMP(AF_INET,IP) then
         begin
          {Get Header}
          ICMP:=PICMPHeader(LongWord(IP) + GetICMPHeaderOffset(AF_INET,IP));
          
          {Check for a Type that we handle}
          case ICMP.Unused.ICMPType of
           ICMP_UNREACH:begin
             //To Do //See RFC 1122 for Handling details
            end;
           ICMP_SOURCEQUENCH:begin
             //To Do //See RFC 1122 for Handling details
            end;
           ICMP_TIMXCEED:begin
             //To Do //See RFC 1122 for Handling details
            end;
           ICMP_PARAMPROB:begin
             //To Do //See RFC 1122 for Handling details
            end;
          end;
         end;
       end;
     end;
    end;
   AF_INET6:begin
     {Get Header}  
     IP6:=PIP6Header(APacket);
   
     //To Do //Extensions
     
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Protocol = ' + ProtocolToString(Transport.Protocol));
     {$ENDIF}
  
     {Check Protocol}
     case Transport.Protocol of
      IPPROTO_TCP:begin
        {Check for a valid TCP Packet}
        if CheckTCP(AF_INET6,IP6) then
         begin
          {Get Header}
          TCP:=PTCPHeader(LongWord(IP6) + GetTCPHeaderOffset(AF_INET6,IP6));
          
          {Set the Ports to Host order}
          TCP.DestPort:=WordBEtoN(TCP.DestPort);
          TCP.SourcePort:=WordBEtoN(TCP.SourcePort);
          
          {Set the Fields to Host order}
          TCP.Sequence:=LongWordBEtoN(TCP.Sequence);
          TCP.Acknowledge:=LongWordBEtoN(TCP.Acknowledge);
          TCP.WindowSize:=WordBEtoN(TCP.WindowSize);
          TCP.Urgent:=WordBEtoN(TCP.Urgent);
        
          {Check for a Connected Socket}
          Socket:=TTCPSocket(FindSocket(AF_INET6,SOCK_STREAM,IPPROTO_TCP,@IP6.DestIP,@IP6.SourceIP,TCP.DestPort,TCP.SourcePort,ABroadcast,False,True,NETWORK_LOCK_READ));
          if Socket = nil then
           begin
            {Check for a Listening Socket}
            Socket:=TTCPSocket(FindSocket(AF_INET6,SOCK_STREAM,IPPROTO_TCP,@IP6.DestIP,@IP6.SourceIP,TCP.DestPort,TCP.SourcePort,ABroadcast,True,True,NETWORK_LOCK_READ));
           end;
          if Socket <> nil then
           begin
            {Call Segment Handler}
            if not ABroadcast then
             begin
              {Process Segment}
              SegmentHandler(Socket,@IP6.SourceIP,@IP6.DestIP,IP6,ASize);
              
              {Return True even if the Handler failed (eg Sequence/Acknowledge is wrong, Socket is Closed etc)}
              Result:=True;
             end;
            
            {Unlock Socket}
            Socket.ReaderUnlock;
           end
          else
           begin
            {Call Reset Handler (Transport Socket)}
            if not ABroadcast then
             begin
              {Lock Socket}
              Transport.Socket.ReaderLock;
              
              {Send Reset}
              ResetHandler(Transport.Socket,@IP6.SourceIP,@IP6.DestIP,IP6,ASize);
              
              {Return True even if the Handler failed (eg Reset was Set etc)}
              Result:=True;
              
              {Unlock Socket}
              Transport.Socket.ReaderUnlock;
             end;
           end;
         end
        else
         begin
          {Silently consume and discard a bad TCP packet}
          Result:=True;
         end;
       end;
      IPPROTO_ICMPV6:begin
        {Check for a valid ICMP Packet}
        if CheckICMP6(AF_INET6,IP6) then
         begin
          {Get Header}
          ICMP6:=PICMP6Header(LongWord(IP6) + GetICMP6HeaderOffset(AF_INET6,IP6));
        
          {Check for a Type that we handle}

          //To do
        
         end;
       end;
     end;
    end;
  end;
 finally
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.SegmentHandler(ASocket:TTCPSocket;ASource,ADest,APacket:Pointer;ASize:Integer):Boolean;
{Check Socket state and feed Segments to the Sockets, discarding or resetting invalid Segments}

{Packet: The received packet (The complete packet including Transport header)}

{Note: Initial validity check on SEQ/ACK and Flags is done here}
{Note: Caller must hold the Socket lock}
var
 Flags:Byte;
 Offset:Word;
 Length:Word;
 IP:PIPHeader;
 IP6:PIP6Header;
 TCP:PTCPHeader;
 Options:Pointer;
 Socket:TTCPSocket;
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SegmentHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Packet}
 if APacket = nil then Exit;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Get the IP}
    IP:=PIPHeader(APacket);
    
    {Get the TCP}
    TCP:=PTCPHeader(LongWord(IP) + GetTCPHeaderOffset(AF_INET,IP));
    
    {Get the Flags}
    Flags:=(TCP.Flags and TCP_FLAG_MASK);
    
    {Get the Options}
    Options:=nil;
    if GetTCPOptionsLength(AF_INET,IP) > 0 then
     begin
      Options:=Pointer(LongWord(TCP) + TCP_HEADER_SIZE);
     end;
    
    {Get the Offset and Length}
    Offset:=GetTCPDataOffset(AF_INET,IP);
    Length:=GetTCPDataLength(AF_INET,IP);
    {$IFDEF TCP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Offset = ' + IntToStr(Offset));
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Length = ' + IntToStr(Length));
    {$ENDIF}
    
    {Check the State}
    case TTCPState(ASocket.ProtocolState).State of
     TCP_STATE_LISTEN:begin
       {Check for RST}
       if (Flags and TCP_FLAG_RST) = TCP_FLAG_RST then Exit;
       
       {Check for FIN}
       if (Flags and TCP_FLAG_FIN) = TCP_FLAG_FIN then Exit;
       
       {Check for ACK}
       if (Flags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
        begin
         {Call Reset Handler}
         Result:=ResetHandler(ASocket,@IP.SourceIP,@IP.DestIP,IP,ASize);
         
         {No further Processing}
         Exit;
        end;
       
       {Check for SYN}
       if (Flags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
        begin
         {Check Backlog}
         if ASocket.AcceptQueue.Count >= ASocket.BackLog then Exit;
         
         {Check Receive Backlog}
         if ASocket.ReceiveQueue.Count >= FReceiveBacklog then Exit;
         
         {Clone Socket}
         Socket:=CloneSocket(ASocket,@IP.DestIP,@IP.SourceIP,TCP.DestPort,TCP.SourcePort,True,NETWORK_LOCK_READ);
         if Socket = nil then Exit;
         
         {Extract the Options (Cloned Socket)}
         if Options <> nil then HandleTCPOptions(Socket,Options,Flags);
         
         {Receive Data (Cloned Socket in TCP_STATE_CLOSED)}
         Result:=Socket.RecvSegment(TCP.Sequence,TCP.Acknowledge,TCP.WindowSize,TCP.Urgent,Flags,Pointer(LongWord(IP) + Offset),Length);
         
         {Unlock Socket}
         Socket.ReaderUnlock;
         
         {No further Processing}
         Exit;
        end;
       
       {All others are dropped}
      end;
     TCP_STATE_SYNSENT:begin
       {Check for FIN}
       if (Flags and TCP_FLAG_FIN) = TCP_FLAG_FIN then Exit;
       
       {Check for ACK}
       if (Flags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
        begin
         {Test Acknowledge}
         if not ASocket.SendData.TestAcknowledge(TCP.Acknowledge) then
          begin
           {Call Reset Handler}
           Result:=ResetHandler(ASocket,@IP.SourceIP,@IP.DestIP,IP,ASize);
           
           {No further Processing}
           Exit;
          end;
        end;
        
       {Extract the Options}
       if Options <> nil then HandleTCPOptions(ASocket,Options,Flags);
       
       {Receive Data}
       Result:=ASocket.RecvSegment(TCP.Sequence,TCP.Acknowledge,TCP.WindowSize,TCP.Urgent,Flags,Pointer(LongWord(IP) + Offset),Length);
      end;
     TCP_STATE_SYNREC:begin
       {Check Sequence}
       if not ASocket.RecvData.CheckSequence(TCP.Sequence,Length) then
        begin
         {Check for RST}
         if (Flags and TCP_FLAG_RST) = TCP_FLAG_RST then Exit;
         
         {Send an ACK}
         Result:=SendAcknowledge(ASocket);
         
         {No further Processing}
         Exit;
        end;
       
       {Check for SYN}
       if (Flags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
        begin
         {Call Reset Handler}
         Result:=ResetHandler(ASocket,@IP.SourceIP,@IP.DestIP,IP,ASize);
         
         {Continue Processing (Will cause a Close)}
        end;
       
       {Check for ACK}
       if (Flags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
        begin
         {Check Acknowledge}
         if not ASocket.SendData.CheckAcknowledge(TCP.Acknowledge) then
          begin
           {Call Reset Handler}
           Result:=ResetHandler(ASocket,@IP.SourceIP,@IP.DestIP,IP,ASize);
           
           {No further Processing}
           Exit;
          end;
        end;
       
       {Extract the Options}
       if Options <> nil then HandleTCPOptions(ASocket,Options,Flags);
       
       {Receive Data}
       Result:=ASocket.RecvSegment(TCP.Sequence,TCP.Acknowledge,TCP.WindowSize,TCP.Urgent,Flags,Pointer(LongWord(IP) + Offset),Length);
      end;
     TCP_STATE_ESTAB,TCP_STATE_FINWAIT1,TCP_STATE_FINWAIT2,TCP_STATE_CLOSWAIT,
     TCP_STATE_CLOSING,TCP_STATE_LASTACK,TCP_STATE_TIMEWAIT:begin
       {Check Sequence}
       if not ASocket.RecvData.CheckSequence(TCP.Sequence,Length) then
        begin
         {Check for RST}
         if (Flags and TCP_FLAG_RST) = TCP_FLAG_RST then Exit;
         
         {Send an ACK}
         Result:=SendAcknowledge(ASocket);
         
         {No further Processing}
         Exit;
        end;
       
       {Check for SYN}
       if (Flags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
        begin
         {Call Reset Handler}
         Result:=ResetHandler(ASocket,@IP.SourceIP,@IP.DestIP,IP,ASize);
         
         {Continue Processing (Will cause a Close)}
        end;
       
       {Check for ACK}
       if (Flags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
        begin
         {Validate Acknowledge}
         if not ASocket.SendData.ValidateAcknowledge(TCP.Acknowledge) then
          begin
           {Check for RST}
           if (Flags and TCP_FLAG_RST) = TCP_FLAG_RST then Exit;
           
           {Send an ACK}
           Result:=SendAcknowledge(ASocket);
           
           {No further Processing}
           Exit;
          end;
        end;
       
       {Extract the Options}
       if Options <> nil then HandleTCPOptions(ASocket,Options,Flags);
       
       {Receive Data}
       Result:=ASocket.RecvSegment(TCP.Sequence,TCP.Acknowledge,TCP.WindowSize,TCP.Urgent,Flags,Pointer(LongWord(IP) + Offset),Length);
      end;
     //To Do //TCP_STATE_TIMEWAIT:begin
             //As above (ESTABLISHED etc) but check FIN after ACK and send ACK if set
     TCP_STATE_CLOSED:begin
       {Check for FIN}
       if (Flags and TCP_FLAG_FIN) = TCP_FLAG_FIN then Exit;
       
       {Call Reset Handler}
       Result:=ResetHandler(ASocket,@IP.SourceIP,@IP.DestIP,IP,ASize);
      end;
    end;
   end;
  AF_INET6:begin
    {Get the IP6}
    IP6:=PIP6Header(APacket);
    
    {Get the TCP}
    TCP:=PTCPHeader(LongWord(IP6) + GetTCPHeaderOffset(AF_INET6,IP6));
    
    {Get the Flags}
    Flags:=(TCP.Flags and TCP_FLAG_MASK);
    
    {Get the Options}
    Options:=nil;
    if GetTCPOptionsLength(AF_INET6,IP6) > 0 then
     begin
      Options:=Pointer(LongWord(TCP) + TCP_HEADER_SIZE);
     end;
    
    {Get the Offset and Length}
    Offset:=GetTCPDataOffset(AF_INET6,IP6);
    Length:=GetTCPDataLength(AF_INET6,IP6);
    {$IFDEF TCP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Offset = ' + IntToStr(Offset));
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Length = ' + IntToStr(Length));
    {$ENDIF}
    
    {Check the State}
  
    //To do
    
   end;
 end;
end;

{==============================================================================}

function TTCPProtocol.ResetHandler(ASocket:TTCPSocket;ASource,ADest,APacket:Pointer;ASize:Integer):Boolean;
{Send a Reset using the appropriate values from SEQ and ACK}

{Packet: The received packet (The complete packet including Transport header)}

{Note: Must never send a Reset if RST bit is set in Packet}
{Note: Caller must hold the Socket lock}
var
 Flags:Byte;
 Length:Word;
 IP:PIPHeader;
 IP6:PIP6Header;
 TCP:PTCPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: ResetHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Packet}
 if APacket = nil then Exit;
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Get the IP}
    IP:=PIPHeader(APacket);
    
    {Get the TCP}
    TCP:=PTCPHeader(LongWord(IP) + GetTCPHeaderOffset(AF_INET,IP));
    
    {Get the Flags}
    Flags:=(TCP.Flags and TCP_FLAG_MASK);
    
    {Get the Length}
    Length:=GetTCPDataLength(AF_INET,IP);
    
    {Check for SYN}
    if (Flags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
     begin
      Inc(Length);
     end;
    
    {Check for FIN}
    if (Flags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
     begin
      Inc(Length);
     end;
    
    {Check for RST}
    if (Flags and TCP_FLAG_RST) = TCP_FLAG_RST then Exit;
    
    {Check for ACK}
    if (Flags and TCP_FLAG_ACK) <> TCP_FLAG_ACK then
     begin
      {SEQ = 0, ACK = SEQ + LEN, CTL = RST + ACK}
      Result:=(SendSegment(ASocket,@IP.DestIP,@IP.SourceIP,TCP.DestPort,TCP.SourcePort,0,TCP.Sequence + Length,0,0,TCP_FLAG_RST or TCP_FLAG_ACK,nil,nil,0) <> SOCKET_ERROR);
     end
    else
     begin
      {SEQ = ACK, ACK = 0, CTL = RST}
      Result:=(SendSegment(ASocket,@IP.DestIP,@IP.SourceIP,TCP.DestPort,TCP.SourcePort,TCP.Acknowledge,0,0,0,TCP_FLAG_RST,nil,nil,0) <> SOCKET_ERROR);
     end;
   end;
  AF_INET6:begin
    {Get the IP6}
    IP6:=PIP6Header(APacket);
    
    {Get the TCP}
    TCP:=PTCPHeader(LongWord(IP6) + GetTCPHeaderOffset(AF_INET6,IP6));
    
    {Get the Flags}
    Flags:=(TCP.Flags and TCP_FLAG_MASK);
    
    {Get the Length}
    Length:=GetTCPDataLength(AF_INET6,IP6);
    
    {Check for SYN}
    if (Flags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
     begin
      Inc(Length);
     end;
    
    {Check for FIN}
    if (Flags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
     begin
      Inc(Length);
     end;
    
    {Check for RST}
    if (Flags and TCP_FLAG_RST) = TCP_FLAG_RST then Exit;
    
    {Check for ACK}
    if (Flags and TCP_FLAG_ACK) <> TCP_FLAG_ACK then
     begin
      {SEQ = 0, ACK = SEQ + LEN, CTL = RST + ACK}
      Result:=(SendSegment(ASocket,@IP6.DestIP,@IP6.SourceIP,TCP.DestPort,TCP.SourcePort,0,TCP.Sequence + Length,0,0,TCP_FLAG_RST or TCP_FLAG_ACK,nil,nil,0) <> SOCKET_ERROR);
     end
    else
     begin
      {SEQ = ACK, ACK = 0, CTL = RST}
      Result:=(SendSegment(ASocket,@IP6.DestIP,@IP6.SourceIP,TCP.DestPort,TCP.SourcePort,TCP.Acknowledge,0,0,0,TCP_FLAG_RST,nil,nil,0) <> SOCKET_ERROR);
     end;
   end;
 end;
end;

{==============================================================================}

function TTCPProtocol.CloneSocket(ASocket:TTCPSocket;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ALock:Boolean;AState:LongWord):TTCPSocket;
{Creates a cloned Socket based on the supplied socket with Options etc copied from the original. New Socket will be SS_UNCONNECTED
 and TCP_STATE_CLOSED but with the Local and Remote address and port bindings completed (different to a new Socket)}

{Note: Addresses and Ports are passed in Host order}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=nil;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: CloneSocket');
  {$ENDIF}
  
  {Check Socket}
  if ASocket = nil then Exit;
  
  {Check Address Family}
  case ASocket.Family of
   AF_INET:begin
     {Create the Socket}
     Result:=TTCPSocket.Create(ASocket.Protocol,ASocket.Transport);
     
     {Copy the Socket State}
     Result.SocketState.NonBlocking:=ASocket.SocketState.NonBlocking;
     Result.SocketState.Async:=ASocket.SocketState.Async;
     
     {Copy the Socket Options}
     Result.SocketOptions.Linger:=ASocket.SocketOptions.Linger;
     Result.SocketOptions.SendBuffer:=ASocket.SocketOptions.SendBuffer;
     Result.SocketOptions.RecvBuffer:=ASocket.SocketOptions.RecvBuffer;
     Result.SocketOptions.SendLowMark:=ASocket.SocketOptions.SendLowMark;
     Result.SocketOptions.RecvLowMark:=ASocket.SocketOptions.RecvLowMark;
     Result.SocketOptions.SendTimeout:=ASocket.SocketOptions.SendTimeout;
     Result.SocketOptions.RecvTimeout:=ASocket.SocketOptions.RecvTimeout;
     Result.SocketOptions.ConnTimeout:=ASocket.SocketOptions.ConnTimeout;
     Result.SocketOptions.Debug:=ASocket.SocketOptions.Debug;
     Result.SocketOptions.ReuseAddress:=True; {Always set ReuseAddress}
     Result.SocketOptions.KeepAlive:=ASocket.SocketOptions.KeepAlive;
     Result.SocketOptions.DontRoute:=ASocket.SocketOptions.DontRoute;
     Result.SocketOptions.UseLoopback:=ASocket.SocketOptions.UseLoopback;
     Result.SocketOptions.UrgentInline:=ASocket.SocketOptions.UrgentInline;
     
     {Copy the Transport Options}
     TIPOptions(Result.TransportOptions).TOS:=TIPOptions(ASocket.TransportOptions).TOS;
     TIPOptions(Result.TransportOptions).TTL:=TIPOptions(ASocket.TransportOptions).TTL;
     TIPOptions(Result.TransportOptions).Flags:=TIPOptions(ASocket.TransportOptions).Flags;
     {TIPOptions(Result.TransportOptions).Length:=TIPOptions(ASocket.TransportOptions).Length;}
     {TIPOptions(Result.TransportOptions).Options:=TIPOptions(ASocket.TransportOptions).Options;}
     
     {Copy the Protocol Options}
     TTCPOptions(Result.ProtocolOptions).MaxSeg:=TTCPOptions(ASocket.ProtocolOptions).MaxSeg;
     TTCPOptions(Result.ProtocolOptions).NoDelay:=TTCPOptions(ASocket.ProtocolOptions).NoDelay;
     TTCPOptions(Result.ProtocolOptions).NoPush:=TTCPOptions(ASocket.ProtocolOptions).NoPush;
     TTCPOptions(Result.ProtocolOptions).NoOpt:=TTCPOptions(ASocket.ProtocolOptions).NoOpt;
     TTCPOptions(Result.ProtocolOptions).BsdUrgent:=TTCPOptions(ASocket.ProtocolOptions).BsdUrgent;
     TTCPOptions(Result.ProtocolOptions).WindowScale:=TTCPOptions(ASocket.ProtocolOptions).WindowScale;
     TTCPOptions(Result.ProtocolOptions).NoSack:=TTCPOptions(ASocket.ProtocolOptions).NoSack;
     
     {Copy the Send Buffer Options}
     Result.SendData.NoPush:=ASocket.SendData.NoPush;
     Result.SendData.NoSack:=ASocket.SendData.NoSack;
     Result.SendData.NoNagle:=ASocket.SendData.NoNagle;
     
     {Copy the Recv Buffer Options}
     Result.RecvData.MaxSeg:=ASocket.RecvData.MaxSeg;
     Result.RecvData.WindowScale:=ASocket.RecvData.WindowScale;
     Result.RecvData.NoSack:=ASocket.RecvData.NoSack;
     
     {Set the Binding}
     OpenPort(Result,ALocalPort);
     TIPState(Result.TransportState).LocalAddress:=PInAddr(ALocalAddress)^;
     Result.SocketState.LocalAddress:=True;
     
     {Set the Connection}
     Result.ProtocolState.RemotePort:=ARemotePort;
     TIPState(Result.TransportState).RemoteAddress:=PInAddr(ARemoteAddress)^;
     Result.SocketState.RemoteAddress:=True;
     
     {Set the Listener}
     Result.Listener:=ASocket;
     
     {Acquire Lock}
     FSockets.WriterLock;
     try
      {Add the Socket}
      FSockets.Add(Result);
      
      {Add the Socket}
      ASocket.ReceiveQueue.Add(Result);
      
      {Lock Socket}
      if ALock then if AState = NETWORK_LOCK_READ then Result.ReaderLock else Result.WriterLock;
     finally 
      {Release Lock}
      FSockets.WriterUnlock;
     end; 
    end;
   AF_INET6:begin
     {Create the Socket}
     Result:=TTCPSocket.Create(ASocket.Protocol,ASocket.Transport);
     
     {Copy the Socket State}
     Result.SocketState.NonBlocking:=ASocket.SocketState.NonBlocking;
     Result.SocketState.Async:=ASocket.SocketState.Async;
     
     {Copy the Socket Options}
     Result.SocketOptions.Linger:=ASocket.SocketOptions.Linger;
     Result.SocketOptions.SendBuffer:=ASocket.SocketOptions.SendBuffer;
     Result.SocketOptions.RecvBuffer:=ASocket.SocketOptions.RecvBuffer;
     Result.SocketOptions.SendLowMark:=ASocket.SocketOptions.SendLowMark;
     Result.SocketOptions.RecvLowMark:=ASocket.SocketOptions.RecvLowMark;
     Result.SocketOptions.SendTimeout:=ASocket.SocketOptions.SendTimeout;
     Result.SocketOptions.RecvTimeout:=ASocket.SocketOptions.RecvTimeout;
     Result.SocketOptions.ConnTimeout:=ASocket.SocketOptions.ConnTimeout;
     Result.SocketOptions.Debug:=ASocket.SocketOptions.Debug;
     Result.SocketOptions.ReuseAddress:=True; {Always set ReuseAddress}
     Result.SocketOptions.KeepAlive:=ASocket.SocketOptions.KeepAlive;
     Result.SocketOptions.DontRoute:=ASocket.SocketOptions.DontRoute;
     Result.SocketOptions.UseLoopback:=ASocket.SocketOptions.UseLoopback;
     Result.SocketOptions.UrgentInline:=ASocket.SocketOptions.UrgentInline;
     
     {Copy the Transport Options}
     //To Do
     TIP6Options(Result.TransportOptions).HopLimit:=TIP6Options(ASocket.TransportOptions).HopLimit;
     {TIP6Options(Result.TransportOptions).Length:=TIP6Options(ASocket.TransportOptions).Length;}
     {TIP6Options(Result.TransportOptions).Options:=TIP6Options(ASocket.TransportOptions).Options;}
     
     {Copy the Protocol Options}
     TTCPOptions(Result.ProtocolOptions).MaxSeg:=TTCPOptions(ASocket.ProtocolOptions).MaxSeg;
     TTCPOptions(Result.ProtocolOptions).NoDelay:=TTCPOptions(ASocket.ProtocolOptions).NoDelay;
     TTCPOptions(Result.ProtocolOptions).NoPush:=TTCPOptions(ASocket.ProtocolOptions).NoPush;
     TTCPOptions(Result.ProtocolOptions).NoOpt:=TTCPOptions(ASocket.ProtocolOptions).NoOpt;
     TTCPOptions(Result.ProtocolOptions).BsdUrgent:=TTCPOptions(ASocket.ProtocolOptions).BsdUrgent;
     TTCPOptions(Result.ProtocolOptions).WindowScale:=TTCPOptions(ASocket.ProtocolOptions).WindowScale;
     TTCPOptions(Result.ProtocolOptions).NoSack:=TTCPOptions(ASocket.ProtocolOptions).NoSack;
     
     {Copy the Send Buffer Options}
     Result.SendData.NoPush:=ASocket.SendData.NoPush;
     Result.SendData.NoSack:=ASocket.SendData.NoSack;
     Result.SendData.NoNagle:=ASocket.SendData.NoNagle;
     
     {Copy the Recv Buffer Options}
     Result.RecvData.MaxSeg:=ASocket.RecvData.MaxSeg;
     Result.RecvData.WindowScale:=ASocket.RecvData.WindowScale;
     Result.RecvData.NoSack:=ASocket.RecvData.NoSack;
     
     {Set the Binding}
     OpenPort(Result,ALocalPort);
     TIP6State(Result.TransportState).LocalAddress:=PIn6Addr(ALocalAddress)^;
     Result.SocketState.LocalAddress:=True;
     
     {Set the Connection}
     Result.ProtocolState.RemotePort:=ARemotePort;
     TIP6State(Result.TransportState).RemoteAddress:=PIn6Addr(ARemoteAddress)^;
     Result.SocketState.RemoteAddress:=True;
     
     {Set the Listener}
     Result.Listener:=ASocket;
     
     {Acquire Lock}
     FSockets.WriterLock;
     try
      {Add the Socket}
      FSockets.Add(Result);
      
      {Add the Socket}
      ASocket.ReceiveQueue.Add(Result); 
      
      {Lock Socket}
      if ALock then if AState = NETWORK_LOCK_READ then Result.ReaderLock else Result.WriterLock;
     finally 
      {Release Lock}
      FSockets.WriterUnlock;
     end; 
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.GetTCPOptionsSize(ASocket:TTCPSocket;AOptions:Pointer):Word;
{Returns the Size of the Options currently in the buffer with padding to 32 bits}

{Note: Caller must hold the Socket lock}
var
 Size:Byte;
 Offset:Word;
 Remain:Word;
begin
 {}
 Result:=0;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: GetTCPOptionsSize');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Options}
 if AOptions = nil then Exit;
 
 {Check Offset}
 Offset:=0;
 while Offset < TCP_OPTIONS_SIZE do
  begin
   {Check for End Option}
   if PByte(LongWord(AOptions) + Offset)^ = TCPOPT_EOL then Break;
   
   {Check for Noop Option}
   if PByte(LongWord(AOptions) + Offset)^ = TCPOPT_NOP then
    begin
     {Move to Next Option}
     Inc(Offset,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Offset + 1) > (TCP_OPTIONS_SIZE - 1) then Break;
     
     {Get the Length}
     Size:=PByte(LongWord(AOptions) + Offset + 1)^;
     
     {Move to Next Option}
     Inc(Offset,Size); {Size includes Option and Size bytes}
    end;
  end;
 
 {Return the Result (Offset + 1 for EOL Option)}
 if Offset > 0 then Result:=Min(Offset + 1,TCP_OPTIONS_SIZE);
 
 {Pad the Result to 32 bits}
 Remain:=(Result mod 4);
 if Remain > 0 then Result:=Result + (4 - Remain);
end;

{==============================================================================}

function TTCPProtocol.CreateTCPOptions(ASocket:TTCPSocket;AOptions:Pointer;AFlags:Byte):Boolean;

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: CreateTCPOptions');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Options}
 if AOptions = nil then Exit;
 
 {Erase Options}
 FillChar(AOptions^,TCP_OPTIONS_SIZE,0);
 
 {End of Options}
 PByte(AOptions)^:=TCPOPT_EOL;
 
 {Check the Flags}
 if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
  begin
   {On SYN do MaxSeg / Window Scale / Sack Ok}
   if not InsertTCPOption(ASocket,AOptions,TCPOPT_MAXSEG) then Exit;
   
   {Check the Options}
   //To Do
  end
 else
  begin
   {Otherwise do active Options (Selective Ack / Timestamp)}
   {Check the Options}
   //To Do
  end;
 
 Result:=True;
end;

{==============================================================================}

function TTCPProtocol.HandleTCPOptions(ASocket:TTCPSocket;AOptions:Pointer;AFlags:Byte):Boolean;

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: HandleTCPOptions');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Options}
 if AOptions = nil then Exit;
 
 {Check the Flags}
 if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
  begin
   {On SYN do MaxSeg / Window Scale / Sack Ok}
   ExtractTCPOption(ASocket,AOptions,TCP_MAXSEG);
   
   {Check the Options}
   //To Do
  end
 else
  begin
   {Otherwise do active Options (Selective Ack / Timestamp)}
   {Check the Options}
   //To Do
  end;
 
 Result:=True;
end;

{==============================================================================}

function TTCPProtocol.InsertTCPOption(ASocket:TTCPSocket;AOptions:Pointer;AOption:Byte):Boolean;
{Inserts the Option into the buffer from the supplied Socket}

{Note: Caller must hold the Socket lock}
var
 Size:Byte;
 Offset:Word;
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: InsertTCPOption');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Options}
 if AOptions = nil then Exit;
 
 {Iterate the current Options}
 Offset:=0;
 while Offset < TCP_OPTIONS_SIZE do
  begin
   {Check for Noop Option}
   if PByte(LongWord(AOptions) + Offset)^ = TCPOPT_NOP then
    begin
     {Move to Next Option}
     Inc(Offset,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Offset + 1) > (TCP_OPTIONS_SIZE - 1) then Exit;
     
     {Get the Length}
     Size:=PByte(LongWord(AOptions) + Offset + 1)^;
     
     {Check for End Option}
     if PByte(LongWord(AOptions) + Offset)^ = TCPOPT_EOL then
      begin
       {Check the Option type}
       case AOption of
        TCPOPT_MAXSEG:begin
          {Max Seg - Size = 2 + 2}
          {Check for end of buffer}
          if (Offset + 4) > (TCP_OPTIONS_SIZE - 1) then Exit;
          
          {Set the Option}
          PByte(LongWord(AOptions) + Offset)^:=AOption;
          
          {Set the Length}
          PByte(LongWord(AOptions) + Offset + 1)^:=4;
          
          {Set the Value}
          PWord(LongWord(AOptions) + Offset + 2)^:=WordNtoBE(TTCPOptions(ASocket.ProtocolOptions).MaxSeg);
          
          Inc(Offset,4);
         end;
        TCPOPT_WINDOW:begin
          {Window Scale - Size = 2 + 1}
          
          {Check for end of buffer}
          if (Offset + 3) > (TCP_OPTIONS_SIZE - 1) then Exit;
          
          {Set the Option}
          PByte(LongWord(AOptions) + Offset)^:=AOption;
          
          {Set the Length}
          PByte(LongWord(AOptions) + Offset + 1)^:=3;
          
          {Set the Value}
          PByte(LongWord(AOptions) + Offset + 2)^:=TTCPOptions(ASocket.ProtocolOptions).WindowScale;
          
          Inc(Offset,3);
         end;
        TCPOPT_SACKOK:begin
          {Selective Ack Ok - Size = 2}
          if not TTCPOptions(ASocket.ProtocolOptions).NoSack then
           begin
            {Check for end of buffer}
            if (Offset + 2) > (TCP_OPTIONS_SIZE - 1) then Exit;
            
            {Set the Option}
            PByte(LongWord(AOptions) + Offset)^:=AOption;
            
            {Set the Length}
            PByte(LongWord(AOptions) + Offset + 1)^:=2;
            
            Inc(Offset,2);
           end;
         end;
        TCPOPT_SACK:begin
          {Selective Ack - Size = 2 + (4 per Segment)}
          {Check for end of buffer (Allow for at least 1 SACK)}
          if (Offset + 6) > (TCP_OPTIONS_SIZE - 1) then Exit;
          
          {Call Selective Ack Segments}
          ASocket.RecvData.SelectiveAckSegments(AOptions,Offset);
         end;
        TCPOPT_TIMESTAMP:begin
          {Timestamp - Size = 2 + 8}
          {Check for end of buffer}
          if (Offset + 10) > (TCP_OPTIONS_SIZE - 1) then Exit;
          
          {Call Timestamp Segment}
          ASocket.RecvData.TimestampSegment(AOptions,Offset);
         end;
       end;
       
       {Set the End Option}
       PByte(LongWord(AOptions) + Offset)^:=TCPOPT_EOL;
       
       Result:=True;
       Exit;
      end;
     
     {Move to Next Option}
     Inc(Offset,Size); {Size includes Option and Size bytes}
    end;
  end;
end;

{==============================================================================}

function TTCPProtocol.ExtractTCPOption(ASocket:TTCPSocket;AOptions:Pointer;AOption:Byte):Boolean;
{Extracts the Option from the buffer to the supplied Socket}

{Note: Caller must hold the Socket lock}
var
 Size:Byte;
 Offset:Word;
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: ExtractTCPOption');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Check Options}
 if AOptions = nil then Exit;
 
 {Iterate the current Options}
 Offset:=0;
 while Offset < TCP_OPTIONS_SIZE do
  begin
   {Check for End Option}
   if PByte(LongWord(AOptions) + Offset)^ = TCPOPT_EOL then Exit;
   
   {Check for Noop Option}
   if PByte(LongWord(AOptions) + Offset)^ = TCPOPT_NOP then
    begin
     {Move to Next Option}
     Inc(Offset,1);
    end
   else
    begin
     {Check for end of buffer}
     if (Offset + 1) > (TCP_OPTIONS_SIZE - 1) then Exit;
     
     {Get the Length}
     Size:=PByte(LongWord(AOptions) + Offset + 1)^;
     
     {Check for Option}
     if PByte(LongWord(AOptions) + Offset)^ = AOption then
      begin
       {$IFDEF TCP_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: ExtractTCPOption - Found Option');
       {$ENDIF}
       
       {Check for end of buffer}
       if (Offset + Size) > (TCP_OPTIONS_SIZE - 1) then Exit;
       
       {Check the Option type}
       case AOption of
        TCPOPT_MAXSEG:begin
          {Max Seg - Size = 2 + 2}
          {Get the Value}
          {ASocket.SendData.MaxSeg:=WordBEtoN(PWord(LongWord(AOptions) + Offset + 2)^);}
          ASocket.SendData.MaxSeg:=Min(WordBEtoN(PWord(LongWord(AOptions) + Offset + 2)^),TCP_MAX_MSS - TCP_OPTIONS_SIZE);
         end;
        TCPOPT_WINDOW:begin
          {Window Scale - Size = 2 + 1}
          {Get the Value}
          ASocket.SendData.WindowScale:=PByte(LongWord(AOptions) + Offset + 2)^;
         end;
        TCPOPT_SACKOK:begin
          {Selective Ack Ok - Size = 2}
          {Get the Value}
          ASocket.SendData.NoSack:=False;  {Defaults to True}
         end;
        TCPOPT_SACK:begin
          {Selective Ack - Size = 2 + (4 per Segment)}
          {Call Selective Ack Segments}
          ASocket.SendData.SelectiveAckSegments(AOptions,Offset,Size);
         end;
        TCPOPT_TIMESTAMP:begin
          {Timestamp - Size = 2 + 8}
          {Call Timestamp Segment}
          ASocket.SendData.TimestampSegment(AOptions,Offset);
         end;
       end;
       
       Result:=True;
       Exit;
      end;

     {Move to Next Option}
     Inc(Offset,Size); {Size includes Option and Size bytes}
    end;
  end;
end;

{==============================================================================}

function TTCPProtocol.SendReset(ASocket:TTCPSocket):Boolean;
{Sends an RST in order to abort an existing connection}
{Not called in response to a bad segment (done by ResetHandler)}
{May be invoked by calling CloseSocket with a hard close etc}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SendReset');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Send Segment} {SEQ = SND.NXT, ACK = 0, CTL = RST}
 Result:=(SendSegment(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@TIPState(ASocket.TransportState).RemoteAddress,ASocket.ProtocolState.LocalPort,ASocket.ProtocolState.RemotePort,ASocket.SendData.NextSequence,0,0,0,TCP_FLAG_RST,nil,nil,0) <> SOCKET_ERROR);
end;

{==============================================================================}

function TTCPProtocol.SendAcknowledge(ASocket:TTCPSocket):Boolean;
{Sends an ACK possibly in response to a bad segment or due to a keep alive etc}

{Note: Caller must hold the Socket lock}
var
 Window:Word;
 Acknowledge:LongWord; 
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SendAcknowledge');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Get Acknowledge to Send (Forced to prevent Delayed ACK)}
 ASocket.RecvData.AcknowledgeSegments(Acknowledge,Window,True);
 
 {Send Segment}
 Result:=(SendSegment(ASocket,@TIPState(ASocket.TransportState).LocalAddress,@TIPState(ASocket.TransportState).RemoteAddress,ASocket.ProtocolState.LocalPort,ASocket.ProtocolState.RemotePort,ASocket.SendData.NextSequence,Acknowledge,Window,0,TCP_FLAG_ACK,nil,nil,0) <> SOCKET_ERROR);
 
 {Signal the Thread} 
 ASocket.SendSocket;
end;

{==============================================================================}

function TTCPProtocol.SendSegment(ASocket:TTCPSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;ASequence,AAcknowledge:LongWord;AWindow,AUrgent:Word;AFlags:Byte;AOptions,AData:Pointer;ASize:Integer):Integer;
{Send Segment adds the Protocol Header and other details to the Data}

{Note: Both Options and Data can be nil, Size can be zero}
{Note: Used instead of SendPacket to allow for the extra parameters needed by TCP}
{Note: Addresses and Ports are passed in Host order}
{Note: Caller must hold the Socket lock}
var
 Length:Word;
 Size:Integer;
 TCP:TTCPHeader;
 Pseudo:TIPPseudo;
 Route:TRouteEntry;
 Header:TPacketFragment;
 Packet:TPacketFragment;
 Options:TPacketFragment;
 Transport:TTCPProtocolTransport;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SendSegment');
 {$ENDIF}
 
 {Check Socket}
 if ASocket = nil then Exit;
 
 {Get the Options Length}
 Length:=GetTCPOptionsSize(ASocket,AOptions);

 {Get the Size of the Packet}
 Size:=ASize + TCP_HEADER_SIZE + Length;

 {Fill in the TCP fields}
 TCP.SourcePort:=WordNtoBE(ASourcePort);
 TCP.DestPort:=WordNtoBE(ADestPort);
 TCP.Sequence:=LongWordNtoBE(ASequence);
 TCP.Acknowledge:=LongWordNtoBE(AAcknowledge);
 TCP.HeaderLength:=((TCP_HEADER_SIZE + Length) shl 2) and $F0;  {(Size div 4 (or shr 2)) shl 4}
 TCP.Flags:=AFlags;
 TCP.WindowSize:=WordNtoBE(AWindow);
 TCP.Checksum:=0;
 TCP.Urgent:=WordNtoBE(AUrgent);
 
 {Check Address Family}
 case ASocket.Family of
  AF_INET:begin
    {Check for Default}
    if TIPTransport(ASocket.Transport).CompareDefault(PInAddr(ASource)^) then
     begin
      {Get the Route}
      NetworkSetLastError(WSAENETUNREACH);
      Route:=TIPTransport(ASocket.Transport).GetRouteByAddress(PInAddr(ADest)^,True,NETWORK_LOCK_READ);
      if Route = nil then Exit;
      
      {Fill in the Pseudo Header}
      Pseudo.SourceIP:=InAddrToNetwork(TIPRouteEntry(Route).Address);
      
      {Unlock Route}
      Route.ReaderUnlock;
     end
    else
     begin
      {Fill in the Pseudo Header}
      Pseudo.SourceIP:=InAddrToNetwork(PInAddr(ASource)^);
     end;
    
    {Fill in the Pseudo Header}
    Pseudo.DestIP:=InAddrToNetwork(PInAddr(ADest)^);
    Pseudo.Mbz:=0;
    Pseudo.Protocol:=IPPROTO_TCP;
    Pseudo.Length:=WordNtoBE(Size);
    
    {Calculate the Checksum}
    TCP.Checksum:=ChecksumTCPSend(AF_INET,@Pseudo,@TCP,AOptions,AData,Length,ASize);
    
    {Create the Header}
    Header.Size:=TCP_HEADER_SIZE;
    Header.Data:=@TCP;
    
    {Check Options}
    if AOptions <> nil then
     begin
      Header.Next:=@Options;
      
      {Create the Options}
      Options.Size:=Length;
      Options.Data:=AOptions;
      if AData <> nil then
       begin
        Options.Next:=@Packet;
        
        {Create the Data}
        Packet.Size:=ASize;
        Packet.Data:=AData;
        Packet.Next:=nil;
       end
      else
       begin
        Options.Next:=nil;
       end;
     end
    else
     begin
      if AData <> nil then
       begin
        Header.Next:=@Packet;
        
        {Create the Data}
        Packet.Size:=ASize;
        Packet.Data:=AData;
        Packet.Next:=nil;
       end
      else
       begin
        Header.Next:=nil;
       end;
     end;
    
    {Get Transport}
    Transport:=TTCPProtocolTransport(GetTransportByTransport(ASocket.Transport,True,NETWORK_LOCK_READ));
    if Transport = nil then Exit;
    
    {Send the Packet}
    if TIPTransport(ASocket.Transport).SendPacket(ASocket,ASource,ADest,@Header,Size,0) = Size then
     begin
      {Return passed size not sent size}
      Result:=ASize;
     end;
     
    {Unlock Transport}
    Transport.ReaderUnlock;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function TTCPProtocol.OpenPort(ASocket:TProtocolSocket;APort:Word):Boolean;

{Note: Caller must hold the Socket lock}
var
 Start:Word;
 Port:TProtocolPort;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: OpenPort');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Port = '  + IntToStr(APort));
  {$ENDIF}
  
  {Check Socket}
  if ASocket = nil then Exit;
  
  {Check for Any Port}
  if APort <> IPPORT_ANY then
   begin
    {Acquire Lock}
    FPorts.WriterLock;
    try
     {Use the specified port if available}
     Port:=FindPort(APort,True,True); {Writer Lock / Lock Return}
     if Port <> nil then
      begin
       //To Do //Check the LocalAddress as well as Port
       {If already in use check ReuseAddress option}
       if ASocket.SocketOptions.ReuseAddress then
        begin
         {Add Socket}
         Port.Sockets.Add(ASocket);
         
         {Set Local Port}
         ASocket.ProtocolState.LocalPort:=APort;
         
         {Return Result}
         Result:=True;
        end; 
 
       {Unlock Port}
       Port.ReleaseLock;      
      end
     else
      begin
       {Create Port}
       Port:=TProtocolPort.Create;
       Port.Port:=APort;
       
       {Add Socket}
       Port.Sockets.Add(ASocket);
       
       {Add Port}
       FPorts.Add(Port);
       
       {Set Local Port}
       ASocket.ProtocolState.LocalPort:=APort;
       
       {Return Result}
       Result:=True;
      end;
    finally
     {Release Lock}
     FPorts.WriterUnlock;
    end;  
   end
  else
   begin
    {Acquire Lock}
    FPorts.WriterLock;
    try
     {Auto assign a dynamic port}
     Start:=FNextPort;
     while FindPort(FNextPort,True,False) <> nil do {Writer Lock / Do not lock Return}
      begin
       //To Do //Check the LocalAddress as well as Port
       {Increment Port}
       Inc(FNextPort);
       
       {Check for wrap around}
       if FNextPort > TCP_PORT_STOP then FNextPort:=TCP_PORT_START;
       
       {Check for a complete cycle}
       if FNextPort = Start then Exit;
      end;
     
     {Create Port}
     Port:=TProtocolPort.Create;
     Port.Port:=FNextPort;
     
     {Add Socket}
     Port.Sockets.Add(ASocket);
     
     {Add Port}
     FPorts.Add(Port);
     
     {Set Local Port}
     ASocket.ProtocolState.LocalPort:=FNextPort;
     
     {Return Result}
     Result:=True;
    finally
     {Release Lock}
     FPorts.WriterUnlock;
    end;  
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.ClosePort(ASocket:TProtocolSocket):Boolean;

{Note: Caller must hold the Socket lock}
var
 Port:TProtocolPort;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: ClosePort');
  {$ENDIF}
  
  {Check Socket}
  if ASocket = nil then Exit;
 
  {Get Port}
  Port:=FindPort(ASocket.ProtocolState.LocalPort,False,True); {Reader Lock / Lock Return}
  if Port = nil then Exit;
  
  {Remove Socket}
  Port.Sockets.Remove(ASocket);
 
  {Check Count}
  if Port.Sockets.Count = 0 then
   begin
    {Acquire Lock}
    FPorts.WriterLock;
    try
     {Remove Port}
     FPorts.Remove(Port);

     {Unlock Port}
     Port.ReleaseLock;
    
     {Destroy Port}
     Port.Free;
    finally
     {Release Lock}
     FPorts.WriterUnlock;
    end;  
   end
  else
   begin
    {Unlock Port}
    Port.ReleaseLock;
   end;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.FindPort(APort:Word;AWrite,ALock:Boolean):TProtocolPort;
{Find a protocol port for the specified port number}
{Port: The port number to find}
{Write: If True then use the writer lock}
{Lock: If True then lock the found entry before returning}
var
 Port:TProtocolPort;
begin
 {}
 Result:=nil;
 
 if AWrite then
  begin
   if not FPorts.WriterLock then Exit;
  end
 else
  begin 
   if not FPorts.ReaderLock then Exit;
  end; 
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: FindPort');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Port = '  + IntToStr(APort));
  {$ENDIF}
  
  {Check Port}
  if APort = IPPORT_ANY then Exit;
  
  {Get Port}
  Port:=TProtocolPort(FPorts.First);
  while Port <> nil do
   begin
    if Port.Port = APort then
     begin
      {Lock Port}
      if ALock then Port.AcquireLock;
      
      {Return Result}
      Result:=Port;
      Exit;
     end;
     
    {Get Next}
    Port:=TProtocolPort(Port.Next);
   end;
 finally 
  if AWrite then
   begin
    FPorts.WriterUnlock;
   end
  else
   begin  
    FPorts.ReaderUnlock;
   end; 
 end; 
end;

{==============================================================================}

function TTCPProtocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
var
 Count:Integer;
 Socket:TTCPSocket;
begin
 {}
 Result:=0;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SelectCheck');
 {$ENDIF}
 
 {Check Dest}
 if ADest = nil then Exit;
 
 {Check Source}
 if ASource = nil then Exit;
 
 {Check Code}
 case ACode of
  SELECT_READ:begin
    Result:=SOCKET_ERROR;
    
    {Get Sockets}
    for Count:=ASource.fd_count - 1 downto 0 do
     begin
      {Get Socket}
      Socket:=TTCPSocket(ASource.fd_array[Count]);
      
      {Check Socket}
      if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
      
      {Check Socket State}
      if Socket.SocketState.Listening then
       begin
        {Listening Sockets}
        {Check Accept Queue}
        if (Socket.AcceptQueue.Count > 0) and (Socket.Accept(True,False,NETWORK_LOCK_NONE) <> nil) then
         begin
          {Check Set}
          if not FD_ISSET(TSocket(Socket),ADest^) then
           begin
            FD_SET(TSocket(Socket),ADest^);
           end;
         end;
       end
      else
       begin
        {Normal Sockets}
        {Check Data Available}
        if Socket.RecvData.GetAvailable > 0 then
         begin
          if not FD_ISSET(TSocket(Socket),ADest^) then
           begin
            FD_SET(TSocket(Socket),ADest^);
           end;
         end;
        
        {Check Urgent if SO_OOBINLINE}
        if Socket.SocketOptions.UrgentInline then
         begin
          if Socket.RecvData.GetUrgent > 0 then
           begin
            {Check Set}
            if not FD_ISSET(TSocket(Socket),ADest^) then
             begin
              FD_SET(TSocket(Socket),ADest^);
             end;
           end;
         end;
        
        {Check for Closed, Unconnected, Disconnecting or Error}
        if (Socket.SocketState.Closed) or (Socket.SocketState.Unconnected) or (Socket.SocketState.Disconnecting) or (Socket.SocketError <> ERROR_SUCCESS) then 
         begin
          {Check Set}
          if not FD_ISSET(TSocket(Socket),ADest^) then
           begin
            FD_SET(TSocket(Socket),ADest^);
           end;
         end;
       end;
     
      {Unlock Socket} 
      Socket.ReaderUnlock;
     end;
     
    {Return Result} 
    Result:=ADest.fd_count;
   end;
  SELECT_WRITE:begin
    Result:=SOCKET_ERROR;
    
    {Get Sockets}
    for Count:=ASource.fd_count - 1 downto 0 do
     begin
      {Get Socket}
      Socket:=TTCPSocket(ASource.fd_array[Count]);
      
      {Check Socket}
      if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
      
      {All Sockets}
      {Check for Connected}
      if (Socket.SocketState.Connected) and (Socket.SendData.GetFree > 0) then
       begin
        {Check Set}
        if not FD_ISSET(TSocket(Socket),ADest^) then
         begin
          FD_SET(TSocket(Socket),ADest^);
         end;
       end;
     
      {Unlock Socket} 
      Socket.ReaderUnlock;
     end;
     
    {Return Result}
    Result:=ADest.fd_count;
   end;
  SELECT_ERROR:begin
    Result:=SOCKET_ERROR;
    
    {Get Sockets}
    for Count:=ASource.fd_count - 1 downto 0 do
     begin
      {Get Socket}
      Socket:=TTCPSocket(ASource.fd_array[Count]);
      
      {Check Socket}
      if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
      
      {All Sockets}
      {Check for Error}
      if Socket.SocketError <> ERROR_SUCCESS then
       begin
        if not FD_ISSET(TSocket(Socket),ADest^) then
         begin
          {Check Set}
          FD_SET(TSocket(Socket),ADest^);
         end;
       end;
      
      {Check Urgent if not SO_OOBINLINE}
      if not Socket.SocketOptions.UrgentInline then
       begin
        if Socket.RecvData.GetUrgent > 0 then
         begin
          {Check Set}
          if not FD_ISSET(TSocket(Socket),ADest^) then
           begin
            FD_SET(TSocket(Socket),ADest^);
           end;
         end;
       end;
     
      {Unlock Socket} 
      Socket.ReaderUnlock;
     end;
    
    {Return Result}
    Result:=ADest.fd_count;
   end;
 end;
end;

{==============================================================================}

function TTCPProtocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; 
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
var
 StartTime:Int64;
 Socket:TTCPSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SelectWait');
 {$ENDIF}

 {Get Socket}
 Socket:=TTCPSocket(ASocket);
 
 {Check Socket}
 if not CheckSocket(Socket,True,NETWORK_LOCK_READ) then Exit;
 try
  {Check Code}
  case ACode of
   SELECT_READ:begin
     {Check Socket State}
     if Socket.SocketState.Listening then
      begin
       {Listening Sockets}
       {Wait for Queue}
       StartTime:=GetTickCount64;
       while Socket.Accept(True,False,NETWORK_LOCK_NONE) = nil do
        begin
         {Check Timeout}
         if ATimeout = 0 then
          begin
           {Return Zero}
           Result:=0;
           Exit;
          end
         else if ATimeout = INFINITE then
          begin
           {Wait for Event}
           if not Socket.WaitChange then
            begin
             {Return Error}
             Result:=SOCKET_ERROR;
             Exit;
            end;
          end
         else 
          begin
           {Wait for Event}
           if not Socket.WaitChangeEx(ATimeout) then
            begin
             {Return Error}
             Result:=SOCKET_ERROR;
             Exit;
            end;

           {Check for Timeout}
           if GetTickCount64 > (StartTime + ATimeout) then
            begin
             {Return Error}
             Result:=SOCKET_ERROR;
             Exit;
            end;
          end;           
        end;
      end
     else
      begin
       {Normal Sockets}
       {Wait for Data}
       StartTime:=GetTickCount64;
       while Socket.RecvData.GetAvailable = 0 do
        begin
         {Check Urgent if SO_OOBINLINE}
         if (Socket.SocketOptions.UrgentInline) and (Socket.RecvData.GetUrgent > 0) then
          begin
           Break;
          end;
         
         {Check for Closed, Unconnected, Disconnecting or Error}
         if (Socket.SocketState.Closed) or (Socket.SocketState.Unconnected) or (Socket.SocketState.Disconnecting) or (Socket.SocketError <> ERROR_SUCCESS) then
          begin
           Break;
          end;
         
         {Check Timeout}
         if ATimeout = 0 then
          begin
           {Return Zero}
           Result:=0;
           Exit;
          end
         else if ATimeout = INFINITE then
          begin
           {Wait for Event}
           if not Socket.WaitChange then
            begin
             {Return Error}
             Result:=SOCKET_ERROR;
             Exit;
            end;
          end
         else 
          begin
           {Wait for Event}
           if not Socket.WaitChangeEx(ATimeout) then
            begin
             {Return Error}
             Result:=SOCKET_ERROR;
             Exit;
            end;

           {Check for Timeout}
           if GetTickCount64 > (StartTime + ATimeout) then
            begin
             {Return Error}
             Result:=SOCKET_ERROR;
             Exit;
            end;
          end;           
        end;
      end;
      
     {Return One}
     Result:=1; 
    end;
   SELECT_WRITE:begin
     {All Sockets}
     {Wait for Space}
     StartTime:=GetTickCount64;
     while Socket.SendData.GetFree = 0 do
      begin
       if not Socket.SocketState.Connected then
        begin
         {Return Zero}
         Result:=0;
         Exit;
        end;         

       {Check Timeout}
       if ATimeout = 0 then
        begin
         {Return Zero}
         Result:=0;
         Exit;
        end
       else if ATimeout = INFINITE then
        begin
         {Wait for Event}
         if not Socket.WaitChange then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;
        end
       else 
        begin
         {Wait for Event}
         if not Socket.WaitChangeEx(ATimeout) then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;

         {Check for Timeout}
         if GetTickCount64 > (StartTime + ATimeout) then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;
        end;           
      end; 
     
     {Return One}
     Result:=1; 
    end;
   SELECT_ERROR:begin
     {All Sockets}
     {Wait for Error}
     StartTime:=GetTickCount64;
     while Socket.SocketError = ERROR_SUCCESS do
      begin
       {Check Urgent if not SO_OOBINLINE}
       if not(Socket.SocketOptions.UrgentInline) and (Socket.RecvData.GetUrgent > 0) then
        begin
         Break;
        end;
      
       {Check Timeout}
       if ATimeout = 0 then
        begin
         {Return Zero}
         Result:=0;
         Exit;
        end
       else if ATimeout = INFINITE then
        begin
         {Wait for Event}
         if not Socket.WaitChange then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;
        end
       else 
        begin
         {Wait for Event}
         if not Socket.WaitChangeEx(ATimeout) then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;

         {Check for Timeout}
         if GetTickCount64 > (StartTime + ATimeout) then
          begin
           {Return Error}
           Result:=SOCKET_ERROR;
           Exit;
          end;
        end;           
      end;
     
     {Return One}
     Result:=1; 
    end;
  end;
   
 finally
  {Unlock Socket} 
  Socket.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
{BSD compatible Accept}
{Socket: The socket to accept from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
var
 Socket:TTCPSocket;
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);

 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Accept');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Listening or Closed}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Closed then Exit;
   if not ASocket.SocketState.Listening then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if (AAddrLength <> nil) and (AAddrLength^ < SizeOf(TSockAddr)) then Exit;
      
      {Wait for Connection} 
      Socket:=TTCPSocket(TTCPSocket(ASocket).Accept(False,True,NETWORK_LOCK_READ));
      while Socket = nil do
       begin
        {Wait for Event}
        if not ASocket.WaitChange then
         begin
          NetworkSetLastError(WSAEINTR);
          Exit;
         end; 
        
        {Check for Closed}
        if ASocket.SocketState.Closed then
         begin
          NetworkSetLastError(WSAEINTR);
          Exit;
         end;
         
        Socket:=TTCPSocket(TTCPSocket(ASocket).Accept(False,True,NETWORK_LOCK_READ));
       end;
      
      {Return the Peer Details}
      if ASockAddr <> nil then
       begin
        ASockAddr^.sin_family:=Socket.Family;
        ASockAddr^.sin_port:=WordNtoBE(Socket.ProtocolState.RemotePort);
        ASockAddr^.sin_addr:=InAddrToNetwork(TIPState(Socket.TransportState).RemoteAddress);
       end;
      if AAddrLength <> nil then AAddrLength^:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=Socket;
      
      {Unlock Socket}
      Socket.ReaderUnlock;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(ASockAddr);
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if (AAddrLength <> nil) and (AAddrLength^ < SizeOf(TSockAddr6)) then Exit;
      
      {Wait for Connection} 
      Socket:=TTCPSocket(TTCPSocket(ASocket).Accept(False,True,NETWORK_LOCK_READ));
      while Socket = nil do
       begin
        {Wait for Event}
        if not ASocket.WaitChange then
         begin
          NetworkSetLastError(WSAEINTR);
          Exit;
         end; 
        
        {Check for Closed}
        if ASocket.SocketState.Closed then
         begin
          NetworkSetLastError(WSAEINTR);
          Exit;
         end; 
         
        Socket:=TTCPSocket(TTCPSocket(ASocket).Accept(False,True,NETWORK_LOCK_READ));
       end;
      
      {Return the Peer Details}
      if SockAddr6 <> nil then
       begin
        SockAddr6^.sin6_family:=Socket.Family;
        SockAddr6^.sin6_port:=WordNtoBE(Socket.ProtocolState.RemotePort);
        SockAddr6^.sin6_addr:=TIP6State(Socket.TransportState).RemoteAddress;
       end;
      if AAddrLength <> nil then AAddrLength^:=SizeOf(TSockAddr6);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=Socket;
      
      {Unlock Socket}
      Socket.ReaderUnlock;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Bind}
{Sets the LocalAddress/Port for future Sends and Receives, Address can be specified as INADDR_ANY which allows Listening or auto assignment}
{If Port is IPPORT_ANY then a dynamic Port will be assigned}
{Socket: The socket to bind}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order} 
{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Bind');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected, Listening or Bound}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Listening then Exit;
   if ASocket.SocketState.Connected then Exit;
   if ASocket.SocketState.LocalAddress then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> ASockAddr.sin_family then Exit;
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Check LocalAddress}
      if not TIPTransport(ASocket.Transport).CompareDefault(InAddrToHost(ASockAddr.sin_addr)) then
       begin
        NetworkSetLastError(WSAEADDRNOTAVAIL);
        if TIPTransport(ASocket.Transport).GetAddressByAddress(InAddrToHost(ASockAddr.sin_addr),False,NETWORK_LOCK_NONE) = nil then Exit;
       end;
      
      {Bind the Port}
      NetworkSetLastError(WSAEADDRINUSE);
      if not OpenPort(ASocket,WordBEtoN(ASockAddr.sin_port)) then Exit;
      
      {Bind the Address}
      ASocket.SocketState.LocalAddress:=True;
      TIPState(ASocket.TransportState).LocalAddress:=InAddrToHost(ASockAddr.sin_addr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> SockAddr6.sin6_family then Exit;
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Check LocalAddress}
      if not TIP6Transport(ASocket.Transport).CompareDefault(SockAddr6.sin6_addr) then
       begin
        NetworkSetLastError(WSAEADDRNOTAVAIL);
        if TIP6Transport(ASocket.Transport).GetAddressByAddress(SockAddr6.sin6_addr,False,NETWORK_LOCK_NONE) = nil then Exit;
       end;
      
      {Bind the Port}
      NetworkSetLastError(WSAEADDRINUSE);
      if not OpenPort(ASocket,WordBEtoN(SockAddr6.sin6_port)) then Exit;
      
      {Bind the Address}
      ASocket.SocketState.LocalAddress:=True;
      TIP6State(ASocket.TransportState).LocalAddress:=SockAddr6.sin6_addr;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.CloseSocket(ASocket:TProtocolSocket):Integer;
{BSD compatible Close Socket}
{Closes and removes the socket, does not perform Linger}
{Socket: The socket to close}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: CloseSocket');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Listening} {Must be before Unconnected}
   if ASocket.SocketState.Listening then
    begin
     {Disconnect Socket}
     NetworkSetLastError(WSAEINVAL);
     if not TTCPSocket(ASocket).Disconnect then Exit;
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
     Exit;
    end;
   
   {Check for Closed or Unconnected}
   if (ASocket.SocketState.Closed) or (ASocket.SocketState.Unconnected) then
    begin
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
     Exit;
    end;
   
   {All other States}
   NetworkSetLastError(WSAEINVAL);
   
   {Check Linger}
   if ASocket.SocketOptions.Linger.l_onoff = 0 then
    begin
     {Linger Off}
     {Graceful Close - Send FIN}
     if not TTCPSocket(ASocket).Disconnect then Exit;
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
     Exit;
    end
   else
    begin
     {Linger On}
     if ASocket.SocketOptions.Linger.l_linger = 0 then
      begin
       {Hard Close - Send RST}
       if not SendReset(TTCPSocket(ASocket)) then Exit;
       
       {Close Socket}
       TTCPState(ASocket.ProtocolState).State:=TCP_STATE_CLOSED;
       ASocket.SocketState.Closed:=True;
       ASocket.CloseTime:=GetTickCount64;
       
       {Signal the Event}
       ASocket.SignalChange;
       
       {Return Result}
       NetworkSetLastError(ERROR_SUCCESS);
       Result:=NO_ERROR;
       Exit;
      end
     else
      begin
       {Graceful Close - Send FIN}
       if not TTCPSocket(ASocket).Disconnect then Exit;
       ASocket.LingerTime:=GetTickCount64;
       
       {Wait for Close}
       while (not ASocket.SocketState.Closed) and (not ASocket.SocketState.Unconnected) do
        begin
         {Wait for Event}
         if not ASocket.WaitChangeEx(ASocket.SocketOptions.Linger.l_linger) then
          begin
           Break;
          end; 
         
         {Check for Timeout}
         if GetTickCount64 > (ASocket.LingerTime + ASocket.SocketOptions.Linger.l_linger) then
          begin
           Break;
          end;
        end;
       
       {Return Result}
       NetworkSetLastError(ERROR_SUCCESS);
       Result:=NO_ERROR;
       Exit;
      end;
    end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
{BSD compatible Connect}
{Sets the RemoteAddress/Port of future Sends and Receives and begins the three way handshake,if Bind has not been
 called then the LocalAddress/Port will be set appropriately as well based on the route to the RemoteAddress}
{Socket: The socket to connect}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is passed in Network order}
{Note: Caller must hold the Socket lock}
var
 Count:Integer;
 StartTime:Int64;
 Route:TRouteEntry;
 Address:TAddressEntry;
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Connect');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAEISCONN);
   if ASocket.SocketState.Connected then Exit;
   
   {Check for Listening or Closed}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Closed then Exit;
   if ASocket.SocketState.Listening then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> ASockAddr.sin_family then Exit;
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Check for Default Remote Port}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if WordBEtoN(ASockAddr.sin_port) = IPPORT_ANY then Exit;
      
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIPTransport(ASocket.Transport).CompareDefault(InAddrToHost(ASockAddr.sin_addr)) then Exit;
      
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEADDRNOTAVAIL); {Normally WSAEACCES but TCP does not support Broadcast}
      if TIPTransport(ASocket.Transport).CompareBroadcast(InAddrToHost(ASockAddr.sin_addr)) or TIPTransport(ASocket.Transport).CompareDirected(InAddrToHost(ASockAddr.sin_addr)) then Exit;
      
      {Check the Route}
      NetworkSetLastError(WSAENETUNREACH);
      Route:=TIPTransport(ASocket.Transport).GetRouteByAddress(InAddrToHost(ASockAddr.sin_addr),True,NETWORK_LOCK_READ);
      if Route = nil then Exit;
      try
       {Check the LocalAddress}
       NetworkSetLastError(WSAEADDRNOTAVAIL);
       Address:=TIPTransport(ASocket.Transport).GetAddressByAddress(TIPRouteEntry(Route).Address,True,NETWORK_LOCK_READ);
       if Address = nil then Exit;
       try
        {Check the Binding}
        if not ASocket.SocketState.LocalAddress then
         begin
          {Bind the Port}
          NetworkSetLastError(WSAEADDRINUSE);
          if not OpenPort(ASocket,WordBEtoN(IPPORT_ANY)) then Exit;
        
          {Bind the Address}
          ASocket.SocketState.LocalAddress:=True;
          TIPState(ASocket.TransportState).LocalAddress:=TIPAddressEntry(Address).Address;
         end;
      
        {Check for Default Binding}
        if TIPTransport(ASocket.Transport).CompareDefault(TIPState(ASocket.TransportState).LocalAddress) then
         begin
          {Set the LocalAddress}
          TIPState(ASocket.TransportState).LocalAddress:=TIPAddressEntry(Address).Address;
         end;
        
        {Connect the Socket}
        ASocket.SocketState.RemoteAddress:=True;
        ASocket.ProtocolState.RemotePort:=WordBEtoN(ASockAddr.sin_port);
        TIPState(ASocket.TransportState).RemoteAddress:=InAddrToHost(ASockAddr.sin_addr);
       
        {Start the Handshake}
        NetworkSetLastError(WSAENOBUFS);
        if not TTCPSocket(ASocket).Connect then Exit;
      
        {Unlock Route} //To Do //Move the finally block ?
        Route.ReaderUnlock;
        Route:=nil;
        
        {Unlock Address} //To Do //Move the finally block ?
        Address.ReaderUnlock;
        Address:=nil;
        
        {Count the Retries}
        Count:=0;
        while Count < TCP_CONNECT_COUNT do
         begin
          {Wait for Connected}
          StartTime:=GetTickCount64;
          while not ASocket.SocketState.Connected do
           begin
            {Check for Timeout}
            if ASocket.SocketOptions.ConnTimeout > 0 then
             begin
              {Wait for Event}
              if not ASocket.WaitChangeEx(ASocket.SocketOptions.ConnTimeout) then
               begin
                NetworkSetLastError(WSAETIMEDOUT);
                Exit;
               end; 
              
              {Check for Timeout}
              if GetTickCount64 > (StartTime + ASocket.SocketOptions.ConnTimeout) then
               begin
                NetworkSetLastError(WSAETIMEDOUT);
                Exit;
               end;
             end
            else
             begin
              {Wait for Event}
              if not ASocket.WaitChange then
               begin
                NetworkSetLastError(WSAECONNREFUSED);
                Exit;
               end; 
             end;             
           
            {Check for Closed}
            if ASocket.SocketState.Closed then
             begin
              NetworkSetLastError(WSAECONNREFUSED);
              
              {Check for Error}
              if ASocket.SocketError <> ERROR_SUCCESS then
               begin
                NetworkSetLastError(ASocket.SocketError);
               end;
              Break;
             end;
           end;
           
          {Check for Connected}
          if ASocket.SocketState.Connected then Break;
         
          {Check for Retry}
          Inc(Count);
          if Count >= TCP_CONNECT_COUNT then Exit;
        
          {Restart the Handshake}
          Sleep(TCP_RESTART_TIMEOUT);
          NetworkSetLastError(WSAENOBUFS);
          if not TTCPSocket(ASocket).Reconnect then Exit;
         end;
      
        {Return Result}
        NetworkSetLastError(ERROR_SUCCESS);
        Result:=NO_ERROR;
       finally
        if Address <> nil then Address.ReaderUnlock;
       end;
      finally
       if Route <> nil then Route.ReaderUnlock;
      end;      
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      if ASocket.Family <> SockAddr6.sin6_family then Exit;
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Check for Default Remote Port}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if WordBEtoN(SockAddr6.sin6_port) = IPPORT_ANY then Exit;
     
      {Check for Default RemoteAddress}
      NetworkSetLastError(WSAEDESTADDRREQ);
      if TIP6Transport(ASocket.Transport).CompareDefault(SockAddr6.sin6_addr) then Exit;
     
      {Check for Broadcast RemoteAddress}
      NetworkSetLastError(WSAEADDRNOTAVAIL); {Normally WSAEACCES but TCP does not support Broadcast}
      if TIP6Transport(ASocket.Transport).CompareBroadcast(SockAddr6.sin6_addr) or TIP6Transport(ASocket.Transport).CompareDirected(SockAddr6.sin6_addr) then Exit;
      
      {Check the Route}
      NetworkSetLastError(WSAENETUNREACH);
      Route:=TIP6Transport(ASocket.Transport).GetRouteByAddress(SockAddr6.sin6_addr,True,NETWORK_LOCK_READ);
      if Route = nil then Exit;
      try
       {Check the LocalAddress}
       NetworkSetLastError(WSAEADDRNOTAVAIL);
       Address:=TIP6Transport(ASocket.Transport).GetAddressByAddress(TIP6RouteEntry(Route).Address,True,NETWORK_LOCK_READ);
       if Address = nil then Exit;
       try
        {Check the Binding}
        if not ASocket.SocketState.LocalAddress then
         begin
          {Bind the Port}
          NetworkSetLastError(WSAEADDRINUSE);
          if not OpenPort(ASocket,WordBEtoN(IPPORT_ANY)) then Exit;
          
          {Bind the Address}
          ASocket.SocketState.LocalAddress:=True;
          TIP6State(ASocket.TransportState).LocalAddress:=TIP6AddressEntry(Address).Address;
         end;
         
        {Check for Default Binding}
        if TIP6Transport(ASocket.Transport).CompareDefault(TIP6State(ASocket.TransportState).LocalAddress) then
         begin
          {Set the LocalAddress}
          TIP6State(ASocket.TransportState).LocalAddress:=TIP6AddressEntry(Address).Address;
         end;
      
        {Connect the Socket}
        ASocket.SocketState.RemoteAddress:=True;
        ASocket.ProtocolState.RemotePort:=WordBEtoN(SockAddr6.sin6_port);
        TIP6State(ASocket.TransportState).RemoteAddress:=SockAddr6.sin6_addr;
      
        {Start the Handshake}
        NetworkSetLastError(WSAENOBUFS);
        if not TTCPSocket(ASocket).Connect then Exit;
      
        {Unlock Route} //To do //Move the finally block ?
        Route.ReaderUnlock;
        Route:=nil;
        
        {Unlock Address} //To do //Move the finally block ?
        Address.ReaderUnlock;
        Address:=nil;
      
        {Count the Retries}
        Count:=0;
        while Count < TCP_CONNECT_COUNT do
         begin
          {Wait for Connected}
          StartTime:=GetTickCount64;
          while not ASocket.SocketState.Connected do
           begin
            {Check for Timeout}
            if ASocket.SocketOptions.ConnTimeout > 0 then
             begin
              {Wait for Event}
              if not ASocket.WaitChangeEx(ASocket.SocketOptions.ConnTimeout) then
               begin
                NetworkSetLastError(WSAETIMEDOUT);
                Exit;
               end; 
              
              {Check for Timeout}
              if GetTickCount64 > (StartTime + ASocket.SocketOptions.ConnTimeout) then
               begin
                NetworkSetLastError(WSAETIMEDOUT);
                Exit;
               end;
             end
            else
             begin
              {Wait for Event}
              if not ASocket.WaitChange then
               begin
                NetworkSetLastError(WSAECONNREFUSED);
                Exit;
               end; 
             end;             
           
            {Check for Closed}
            if ASocket.SocketState.Closed then
             begin
              NetworkSetLastError(WSAECONNREFUSED);
              
              {Check for Error}
              if ASocket.SocketError <> ERROR_SUCCESS then
               begin
                NetworkSetLastError(ASocket.SocketError);
               end;
              Break;
             end;
           end;
           
          {Check for Connected}
          if ASocket.SocketState.Connected then Break;
         
          {Check for Retry}
          Inc(Count);
          if Count >= TCP_CONNECT_COUNT then Exit;
        
          {Restart the Handshake}
          Sleep(TCP_RESTART_TIMEOUT);
          NetworkSetLastError(WSAENOBUFS);
          if not TTCPSocket(ASocket).Reconnect then Exit;
         end;
      
        {Return Result}
        NetworkSetLastError(ERROR_SUCCESS);
        Result:=NO_ERROR;
       finally
        if Address <> nil then Address.ReaderUnlock;
       end;
      finally
       if Route <> nil then Route.ReaderUnlock;
      end;      
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
{BSD compatible IO Control Socket}
{Socket: The socket to control}
{Cmd: The socket command}
{Arg: The command argument}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: IoctlSocket');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Pass the call to the socket}
   Result:=ASocket.IoCtl(ACmd,AArg);
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Peer Name (Remote)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: GetPeerName');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Return the Peer Details}
      ASockAddr.sin_family:=ASocket.Family;
      ASockAddr.sin_port:=WordNtoBE(ASocket.ProtocolState.RemotePort);
      ASockAddr.sin_addr:=InAddrToNetwork(TIPState(ASocket.TransportState).RemoteAddress);
      AAddrLength:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Return the Peer Details}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(ASocket.ProtocolState.RemotePort);
      SockAddr6.sin6_addr:=TIP6State(ASocket.TransportState).RemoteAddress;
      AAddrLength:=SizeOf(TSockAddr6);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
{BSD compatible Get Sock Name (Local)}
{Socket: The socket to get from}
{SockAddr: The socket address (Network Order)}
{AddrLength: The socket address length}

{Note: SockAddr is returned in Network order}
{Note: Caller must hold the Socket lock}
var
 SockAddr6:PSockAddr6;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: GetSockName');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr) then Exit;
      
      {Return the Socket Details}
      ASockAddr.sin_family:=ASocket.Family;
      ASockAddr.sin_port:=WordNtoBE(ASocket.ProtocolState.LocalPort);
      ASockAddr.sin_addr:=InAddrToNetwork(TIPState(ASocket.TransportState).LocalAddress);
      AAddrLength:=SizeOf(TSockAddr);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    AF_INET6:begin
      {Get Socket Address}
      SockAddr6:=PSockAddr6(@ASockAddr);
      
      {Check size of SockAddr}
      NetworkSetLastError(WSAEFAULT);
      if AAddrLength < SizeOf(TSockAddr6) then Exit;
      
      {Return the Peer Details}
      SockAddr6.sin6_family:=ASocket.Family;
      SockAddr6.sin6_port:=WordNtoBE(ASocket.ProtocolState.LocalPort);
      SockAddr6.sin6_addr:=TIP6State(ASocket.TransportState).LocalAddress;
      AAddrLength:=SizeOf(TSockAddr6);
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
{BSD compatible Get Socket Option}
{Socket: The socket to get the option from}
{Level: The protocol level for the option}
{OptName: The name of the option to get}
{OptValue: The value of the option}
{OptLength: The length of the option}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: GetSockOpt');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Level}
   case ALevel of
    IPPROTO_IP:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      case ASocket.Family of
       AF_INET:begin
         {Pass the call to the transport}
         Result:=TIPTransport(ASocket.Transport).GetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
        end;
       AF_INET6:begin
         {Pass the call to the transport}
         Result:=TIP6Transport(ASocket.Transport).GetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
        end;
      end;
     end;
    else
     begin
      {Pass the call to the socket}
      Result:=ASocket.GetOption(ALevel,AOptName,AOptValue,AOptLength);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
{BSD compatible Listen}
{Socket: The socket to listen on}
{Backlog: Queue depth for accepted connections}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Listen');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Listening}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Listening then Exit;
   
   {Check for Connected}
   NetworkSetLastError(WSAEISCONN);
   if ASocket.SocketState.Connected then Exit;
   
   {Check for Closed or not Bound}
   NetworkSetLastError(WSAEINVAL);
   if ASocket.SocketState.Closed then Exit;
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Start Listening}
   NetworkSetLastError(WSAENOBUFS);
   if not TTCPSocket(ASocket).Listen then Exit;

   {Set Backlog}
   TTCPSocket(ASocket).Backlog:=ABacklog;
   
   {Check Backlog}
   if TTCPSocket(ASocket).Backlog = SOMAXCONN then
    begin
     {Update Backlog}
     TTCPSocket(ASocket).Backlog:=TCP_MAX_BACKLOG;
    end
   else if TTCPSocket(ASocket).Backlog < TCP_MIN_BACKLOG then
    begin
     {Update Backlog}
     TTCPSocket(ASocket).Backlog:=TCP_MIN_BACKLOG;
    end
   else if TTCPSocket(ASocket).Backlog > TCP_MAX_BACKLOG then
    begin
     {Update Backlog}
     TTCPSocket(ASocket).Backlog:=TCP_MAX_BACKLOG;
    end;
   
   {Return Result}
   NetworkSetLastError(ERROR_SUCCESS);
   Result:=NO_ERROR;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
{BSD compatible Receive}
{Socket: The socket to receive from}
{Buffer: Buffer for received data}
{Length: Length of buffer in bytes}
{Flags: Protocol specific receive flags}

{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 StartTime:Int64;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Recv');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then
    begin
     {Check for CLOSWAIT}
     if TTCPState(ASocket.ProtocolState).State <> TCP_STATE_CLOSWAIT then Exit;
    end;
   
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check for Shutdown}
   NetworkSetLastError(WSAESHUTDOWN);
   if ASocket.SocketState.CantRecvMore then Exit;
   
   {Wait for Data}
   StartTime:=GetTickCount64;
   while TTCPSocket(ASocket).RecvData.GetAvailable = 0 do
    begin
     {Check for Disconnecting}
     if not ASocket.SocketState.Disconnecting then
      begin
       {Check for Timeout}
       if ASocket.SocketOptions.RecvTimeout > 0 then
        begin
         {Wait for Event}
         if not ASocket.WaitChangeEx(ASocket.SocketOptions.RecvTimeout) then
          begin
           NetworkSetLastError(WSAECONNABORTED);
           Exit;
          end; 
  
         {Check for Timeout}
         if GetTickCount64 > (StartTime + ASocket.SocketOptions.RecvTimeout) then
          begin
           NetworkSetLastError(WSAECONNABORTED);
           Exit;
          end;
        end
       else
        begin
         {Wait for Event}
         if not ASocket.WaitChange then
          begin
           NetworkSetLastError(WSAECONNABORTED);
           Exit;
          end; 
        end;
      end;
     
     {Check for Closed}
     if ASocket.SocketState.Closed then
      begin
       NetworkSetLastError(WSAECONNRESET);
       {Check for Error}
       if ASocket.SocketError <> ERROR_SUCCESS then
        begin
         NetworkSetLastError(ASocket.SocketError);
        end;
       Exit;
      end;
     
     {Check for Unconnected}
     if ASocket.SocketState.Unconnected then
      begin
       NetworkSetLastError(ASocket.SocketError);
       Result:=0;
       Exit;
      end;
     
     {Check for Disconnecting}
     if (ASocket.SocketState.Disconnecting) and (TTCPSocket(ASocket).RecvData.GetAvailable = 0) then
      begin
       NetworkSetLastError(ASocket.SocketError);
       Result:=0;
       Exit;
      end;
    end;
   
   {Check Size}
   NetworkSetLastError(ERROR_SUCCESS);
   Size:=TTCPSocket(ASocket).RecvData.GetAvailable;
   if Size > ALength then Size:=ALength;
   //To Do //MSG_OOB !!! What about SO_OOBLINE etc
   
   {Read Data}
   if TTCPSocket(ASocket).RecvData.ReadData(ABuffer,Size,AFlags) then
    begin
     {Return Size}
     Result:=Size;
    end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
{BSD compatible Receive From}
{Socket: The socket to receive from}
{Buffer: Buffer for received data}
{Length: Length of buffer in bytes}
{Flags: Protocol specific receive flags}
{FromAddr: The address the data was received from (Network Order)}
{FromLength: The length of the address}

{Note: TCP treats RecvFrom as Recv}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=Recv(ASocket,ABuffer,ALength,AFlags);
end;

{==============================================================================}

function TTCPProtocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
{BSD compatible Send}
{Socket: The socket to send to}
{Buffer: Buffer for data to send}
{Length: Length of buffer in bytes}
{Flags: Protocol specific send flags}

{Note: Caller must hold the Socket lock}
var
 Size:Integer;
 StartTime:Int64;
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Send');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;
   
   {Check for Bound}
   NetworkSetLastError(WSAEINVAL);
   if not ASocket.SocketState.LocalAddress then Exit;
   
   {Check for Shutdown}
   NetworkSetLastError(WSAESHUTDOWN);
   if ASocket.SocketState.CantSendMore then Exit;
   
   {Check Address Family}
   NetworkSetLastError(WSAEAFNOSUPPORT);
   case ASocket.Family of
    AF_INET:begin
      {Wait for Space}
      StartTime:=GetTickCount64;
      while TTCPSocket(ASocket).SendData.GetFree = 0 do
       begin
        {Check for Timeout}
        if ASocket.SocketOptions.SendTimeout > 0 then
         begin
          {Wait for Event}
          if not ASocket.WaitChangeEx(ASocket.SocketOptions.SendTimeout) then
           begin
            NetworkSetLastError(WSAECONNABORTED);
            Exit;
           end; 

          {Check for Timeout}
          if GetTickCount64 > (StartTime + ASocket.SocketOptions.SendTimeout) then
           begin
            NetworkSetLastError(WSAECONNABORTED);
            Exit;
           end;
         end
        else
         begin
          {Wait for Event}
          if not ASocket.WaitChange then
           begin
            NetworkSetLastError(WSAECONNABORTED);
            Exit;
           end; 
         end;         
        
        {Check for Closed}
        if ASocket.SocketState.Closed then
         begin
          NetworkSetLastError(WSAECONNRESET);
          {Check for Error}
          if ASocket.SocketError <> ERROR_SUCCESS then
           begin
            NetworkSetLastError(ASocket.SocketError);
           end;
          Exit;
         end;
        
        {Check for Unconnected}
        if ASocket.SocketState.Unconnected then
         begin
          NetworkSetLastError(ASocket.SocketError);
          Result:=0;
          Exit;
         end;
       end;
      
      {Check Size}
      NetworkSetLastError(ERROR_SUCCESS);
      Size:=TTCPSocket(ASocket).SendData.GetFree;
      if Size > ALength then Size:=ALength;
      //To Do //MSG_OOB !!! What about SO_OOBLINE etc - OOBINLINE not relevant to Send ?
      
      {Write Data}
      if TTCPSocket(ASocket).SendData.WriteData(ABuffer,Size,AFlags) then
       begin
        Result:=Size;
       end;
     end;
    AF_INET6:begin
      {Wait for Space} 
      StartTime:=GetTickCount64;
      while TTCPSocket(ASocket).SendData.GetFree = 0 do
       begin
        {Check for Timeout}
        if ASocket.SocketOptions.SendTimeout > 0 then
         begin
          {Wait for Event}
          if not ASocket.WaitChangeEx(ASocket.SocketOptions.SendTimeout) then
           begin
            NetworkSetLastError(WSAECONNABORTED);
            Exit;
           end; 

          {Check for Timeout}
          if GetTickCount64 > (StartTime + ASocket.SocketOptions.SendTimeout) then
           begin
            NetworkSetLastError(WSAECONNABORTED);
            Exit;
           end;
         end
        else
         begin
          {Wait for Event}
          if not ASocket.WaitChange then
           begin
            NetworkSetLastError(WSAECONNABORTED);
            Exit;
           end; 
         end;         
        
        {Check for Closed}
        if ASocket.SocketState.Closed then
         begin
          NetworkSetLastError(WSAECONNRESET);
          {Check for Error}
          if ASocket.SocketError <> ERROR_SUCCESS then
           begin
            NetworkSetLastError(ASocket.SocketError);
           end;
          Exit;
         end;
        
        {Check for Unconnected}
        if ASocket.SocketState.Unconnected then
         begin
          NetworkSetLastError(ASocket.SocketError);
          Result:=0;
          Exit;
         end;
       end;
      
      {Check Size}
      NetworkSetLastError(ERROR_SUCCESS);
      Size:=TTCPSocket(ASocket).SendData.GetFree;
      if Size > ALength then Size:=ALength;
      //To Do //MSG_OOB !!! What about SO_OOBLINE etc - OOBINLINE not relevant to Send ?
      
      {Write Data}
      if TTCPSocket(ASocket).SendData.WriteData(ABuffer,Size,AFlags) then
       begin
        Result:=Size;
       end;
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
{BSD compatible Send To}
{Socket: The socket to send to}
{Buffer: Buffer for data to send}
{Length: Length of buffer in bytes}
{Flags: Protocol specific send flags}
{ToAddr: The socket address to send to (Network Order)}
{ToLength: The length of the socket address}

{Note: TCP treats SendTo as Send}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=Send(ASocket,ABuffer,ALength,AFlags);
end;

{==============================================================================}

function TTCPProtocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
{BSD compatible Set Socket Option}
{Socket: The socket to set the option for}
{Level: The protocol level for the option}
{OptName: The name of the option to set}
{OptValue: The value of the option}
{OptLength: The length of the option}

{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: SetSockOpt');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check Level}
   case ALevel of
    SOL_SOCKET:begin
      {Socket Options Handled on Set}
      {Check Option}
      case AOptName of
       SO_RCVBUF:begin
         NetworkSetLastError(WSAEFAULT);
         
         if AOptLength >= SizeOf(Integer) then
          begin
           TTCPSocket(ASocket).RecvData.Size:=PInteger(AOptValue)^;
          end;
        end;
       SO_SNDBUF:begin
         NetworkSetLastError(WSAEFAULT);
         
         if AOptLength >= SizeOf(Integer) then
          begin
           TTCPSocket(ASocket).SendData.Size:=PInteger(AOptValue)^;
          end;
        end;
      end;
      
      {Pass the call to the socket}
      Result:=ASocket.SetOption(ALevel,AOptName,AOptValue,AOptLength);
     end;
    IPPROTO_IP:begin
      {Check Address Family}
      NetworkSetLastError(WSAEAFNOSUPPORT);
      case ASocket.Family of
       AF_INET:begin
         {Pass the call to the transport}
         Result:=TIPTransport(ASocket.Transport).SetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
        end;
       AF_INET6:begin
         {Pass the call to the transport}
         Result:=TIP6Transport(ASocket.Transport).SetSockOpt(ASocket,ALevel,AOptName,AOptValue,AOptLength);
        end;
      end;
     end;
    else
     begin
      {Pass the call to the socket}
      Result:=ASocket.SetOption(ALevel,AOptName,AOptValue,AOptLength);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
{BSD compatible Shutdown}
{Socket: The socket to shutdown}
{How: The direction to shutdown the socket}

{Note: Shutdown does not result in CloseSocket so Closed must not get set}
{Note: Caller must hold the Socket lock}
begin
 {}
 Result:=SOCKET_ERROR;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Shutdown');
 {$ENDIF}
 
 {Check Socket} 
 if CheckSocket(ASocket,False,NETWORK_LOCK_NONE) then
  begin
   {Check for Connected}
   NetworkSetLastError(WSAENOTCONN);
   if not ASocket.SocketState.Connected then Exit;
   
   {Check Direction}
   case AHow of
    SHUTDOWN_RECV:begin
      {Shutdown Receive}
      ASocket.SocketState.CantRecvMore:=True;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    SHUTDOWN_SEND:begin
      {Shutdown Send - Must Send FIN}
      ASocket.SocketState.CantSendMore:=True;
      
      {Disconnect Socket}
      NetworkSetLastError(WSAENOBUFS);
      if not TTCPSocket(ASocket).Disconnect then Exit;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    SHUTDOWN_BOTH:begin
      {Shutdown Both - Must Send FIN}
      ASocket.SocketState.CantRecvMore:=True;
      ASocket.SocketState.CantSendMore:=True;
      
      {Disconnect Socket}
      NetworkSetLastError(WSAENOBUFS);
      if not TTCPSocket(ASocket).Disconnect then Exit;
      
      {Return Result}
      NetworkSetLastError(ERROR_SUCCESS);
      Result:=NO_ERROR;
     end;
    else
     begin
      NetworkSetLastError(WSAEINVAL);
     end;
   end;
  end
 else
  begin
   {Not Socket}
   NetworkSetLastError(WSAENOTSOCK);
  end;
end;

{==============================================================================}

function TTCPProtocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
{BSD compatible Socket (Create a new socket)}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}

{Note: When a Socket is created it will be SS_UNCONNECTED and TCP_STATE_CLOSED}
var
 Transport:TTCPProtocolTransport;
begin
 {}
 Result:=TProtocolSocket(INVALID_SOCKET);
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: Socket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Check Socket Type}
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if AStruct <> SOCK_STREAM then Exit;
  
  {Check Address Family}
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (AFamily = AF_UNSPEC) and (AProtocol <> IPPROTO_IP) then AFamily:=AF_INET;

  {Check Protocol}
  NetworkSetLastError(WSAEPROTOTYPE);
  if (AProtocol <> IPPROTO_TCP) and (AProtocol <> IPPROTO_IP) then Exit;
  
  {Get Transport}
  Transport:=TTCPProtocolTransport(GetTransportByFamily(AFamily,True,NETWORK_LOCK_READ));
  if Transport = nil then Exit;
  
  {Create Socket}
  Result:=TTCPSocket.Create(Self,Transport.Transport);
  
  {Unlock Transport}
  Transport.ReaderUnlock;

  {Acquire Lock}
  FSockets.WriterLock;
  try
   {Add Socket}
   FSockets.Add(Result);
  finally 
   {Release Lock}
   FSockets.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.AddTransport(ATransport:TNetworkTransport):Boolean;
{Add a transport to this protocol}
{Transport: The transport to add}
var
 Handle:THandle;
 Transport:TTCPProtocolTransport;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: AddTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
 
  {Get Transport} 
  Transport:=TTCPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_READ));
  if Transport = nil then
   begin
    {Check Address Family}
    case ATransport.Family of
     AF_INET:begin
       {Add TCP Protocol}
       Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_TCP,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TTCPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_TCP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Add Control Socket}
          Transport.Socket:=TTCPSocket.Create(Self,ATransport);
          {FSockets.Add(Transport.Socket);} {Dont add this one to the list}
         
          {Add Proto Entry}
          TIPTransport(ATransport).AddProto(TCP_PROTOCOL_NAME,IPPROTO_TCP,False);
         
          {Add Serv Entries}
          TIPTransport(ATransport).AddServ('ECHO',TCP_PROTOCOL_NAME,7,False);
          TIPTransport(ATransport).AddServ('DISCARD',TCP_PROTOCOL_NAME,9,False);
          TIPTransport(ATransport).AddServ('SYSTAT',TCP_PROTOCOL_NAME,11,False);
          TIPTransport(ATransport).AddServ('DAYTIME',TCP_PROTOCOL_NAME,13,False);
          TIPTransport(ATransport).AddServ('NETSTAT',TCP_PROTOCOL_NAME,15,False);
          TIPTransport(ATransport).AddServ('QOTD',TCP_PROTOCOL_NAME,17,False);
          TIPTransport(ATransport).AddServ('CHARGEN',TCP_PROTOCOL_NAME,19,False);
          TIPTransport(ATransport).AddServ('FTP-DATA',TCP_PROTOCOL_NAME,20,False);
          TIPTransport(ATransport).AddServ('FTP',TCP_PROTOCOL_NAME,21,False);
          TIPTransport(ATransport).AddServ('TELNET',TCP_PROTOCOL_NAME,23,False);
          TIPTransport(ATransport).AddServ('SMTP',TCP_PROTOCOL_NAME,25,False);
          TIPTransport(ATransport).AddServ('TIME',TCP_PROTOCOL_NAME,37,False);
          TIPTransport(ATransport).AddServ('NAME',TCP_PROTOCOL_NAME,42,False);
          TIPTransport(ATransport).AddServ('WHOIS',TCP_PROTOCOL_NAME,43,False);
          TIPTransport(ATransport).AddServ('DOMAIN',TCP_PROTOCOL_NAME,53,False);
          TIPTransport(ATransport).AddServ('NAMESERVER',TCP_PROTOCOL_NAME,53,False);
          TIPTransport(ATransport).AddServ('MTP',TCP_PROTOCOL_NAME,57,False);
          TIPTransport(ATransport).AddServ('FINGER',TCP_PROTOCOL_NAME,79,False);
          TIPTransport(ATransport).AddServ('POP3',TCP_PROTOCOL_NAME,110,False);
          TIPTransport(ATransport).AddServ('PORTMAP',TCP_PROTOCOL_NAME,111,False);
          TIPTransport(ATransport).AddServ('AUTH',TCP_PROTOCOL_NAME,113,False);
          TIPTransport(ATransport).AddServ('NNTP',TCP_PROTOCOL_NAME,119,False);
          TIPTransport(ATransport).AddServ('NBSESSION',TCP_PROTOCOL_NAME,139,False);
          TIPTransport(ATransport).AddServ('IMAP4',TCP_PROTOCOL_NAME,143,False);
         
          {Return Result}
          Result:=True;
         finally
          {Release Lock}
          FTransports.WriterUnlock;
         end;  
        end;
        
       {Add ICMP Protocol}
       Handle:=TIPTransport(ATransport).AddProtocol(IPPROTO_ICMP,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TTCPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_ICMP;
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
       {Add TCP Protocol}
       Handle:=TIP6Transport(ATransport).AddProtocol(IPPROTO_TCP,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TTCPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_TCP;
         Transport.Transport:=ATransport;
         
         {Acquire Lock}
         FTransports.WriterLock;
         try
          {Add Transport}
          FTransports.Add(Transport);
         
          {Add Control Socket}
          Transport.Socket:=TTCPSocket.Create(Self,ATransport);
          {FSockets.Add(Transport.Socket);} {Dont add this one to the list}
          
          {Add Proto Entry}
          TIP6Transport(ATransport).AddProto(TCP_PROTOCOL_NAME,IPPROTO_TCP,False);
          
          {Add Serv Entries}
          TIP6Transport(ATransport).AddServ('ECHO',TCP_PROTOCOL_NAME,7,False);
          TIP6Transport(ATransport).AddServ('DISCARD',TCP_PROTOCOL_NAME,9,False);
          TIP6Transport(ATransport).AddServ('SYSTAT',TCP_PROTOCOL_NAME,11,False);
          TIP6Transport(ATransport).AddServ('DAYTIME',TCP_PROTOCOL_NAME,13,False);
          TIP6Transport(ATransport).AddServ('NETSTAT',TCP_PROTOCOL_NAME,15,False);
          TIP6Transport(ATransport).AddServ('QOTD',TCP_PROTOCOL_NAME,17,False);
          TIP6Transport(ATransport).AddServ('CHARGEN',TCP_PROTOCOL_NAME,19,False);
          TIP6Transport(ATransport).AddServ('FTP-DATA',TCP_PROTOCOL_NAME,20,False);
          TIP6Transport(ATransport).AddServ('FTP',TCP_PROTOCOL_NAME,21,False);
          TIP6Transport(ATransport).AddServ('TELNET',TCP_PROTOCOL_NAME,23,False);
          TIP6Transport(ATransport).AddServ('SMTP',TCP_PROTOCOL_NAME,25,False);
          TIP6Transport(ATransport).AddServ('TIME',TCP_PROTOCOL_NAME,37,False);
          TIP6Transport(ATransport).AddServ('NAME',TCP_PROTOCOL_NAME,42,False);
          TIP6Transport(ATransport).AddServ('WHOIS',TCP_PROTOCOL_NAME,43,False);
          TIP6Transport(ATransport).AddServ('DOMAIN',TCP_PROTOCOL_NAME,53,False);
          TIP6Transport(ATransport).AddServ('NAMESERVER',TCP_PROTOCOL_NAME,53,False);
          TIP6Transport(ATransport).AddServ('MTP',TCP_PROTOCOL_NAME,57,False);
          TIP6Transport(ATransport).AddServ('FINGER',TCP_PROTOCOL_NAME,79,False);
          TIP6Transport(ATransport).AddServ('POP3',TCP_PROTOCOL_NAME,110,False);
          TIP6Transport(ATransport).AddServ('PORTMAP',TCP_PROTOCOL_NAME,111,False);
          TIP6Transport(ATransport).AddServ('AUTH',TCP_PROTOCOL_NAME,113,False);
          TIP6Transport(ATransport).AddServ('NNTP',TCP_PROTOCOL_NAME,119,False);
          TIP6Transport(ATransport).AddServ('NBSESSION',TCP_PROTOCOL_NAME,139,False);
          TIP6Transport(ATransport).AddServ('IMAP4',TCP_PROTOCOL_NAME,143,False);
          
          {Return Result}
          Result:=True;
         finally
          {Release Lock}
          FTransports.WriterUnlock;
         end;  
        end;
        
       {Add ICMP6 Protocol}
       Handle:=TIP6Transport(ATransport).AddProtocol(IPPROTO_ICMPV6,PacketHandler,nil);
       if Handle <> INVALID_HANDLE_VALUE then
        begin
         {Create Transport}
         Transport:=TTCPProtocolTransport.Create;
         Transport.Handle:=Handle;
         Transport.Protocol:=IPPROTO_ICMPV6;
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

function TTCPProtocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
{Remove a transport from this protocol}
{Transport: The transport to remove}
var
 Transport:TTCPProtocolTransport;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: RemoveTransport');
  {$ENDIF}
  
  {Check Transport}
  if ATransport = nil then Exit;
  
  {Check Address Family}
  case ATransport.Family of
   AF_INET:begin
     {Get Transport}
     Transport:=TTCPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
     while Transport <> nil do
      begin
       {Check Protocol}
       case Transport.Protocol of
        IPPROTO_TCP:begin
          {Remove TCP Protocol}
          if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
           begin
            {Remove Serv Entries}
            TIPTransport(ATransport).RemoveServ('ECHO',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('DISCARD',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('SYSTAT',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('DAYTIME',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NETSTAT',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('QOTD',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('CHARGEN',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('FTP-DATA',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('FTP',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('TELNET',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('SMTP',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('TIME',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NAME',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('WHOIS',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('DOMAIN',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NAMESERVER',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('MTP',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('FINGER',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('POP3',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('PORTMAP',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('AUTH',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NNTP',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('NBSESSION',TCP_PROTOCOL_NAME);
            TIPTransport(ATransport).RemoveServ('IMAP4',TCP_PROTOCOL_NAME);
            
            {Remove Proto Entry}
            TIPTransport(ATransport).RemoveProto(TCP_PROTOCOL_NAME);
            
            {Remove Control Socket}
            {FSockets.Remove(Transport.Socket);} {This one is not on the list}
            Transport.Socket.Free;
            
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
        IPPROTO_ICMP:begin
          {Remove ICMP Protocol}
          if TIPTransport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
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
        else
         begin
          {Unlock Transport}
          Transport.WriterUnlock;
         end;         
       end;  
    
       {Get Transport}
       Transport:=TTCPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
      end; 
    end;
   AF_INET6:begin
     {Get Transport}
     Transport:=TTCPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
     if Transport = nil then Exit;
     while Transport <> nil do
      begin
       {Check Protocol}
       case Transport.Protocol of
        IPPROTO_TCP:begin
          {Remove TCP Protocol}
          if TIP6Transport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
           begin
            {Remove Serv Entries}
            TIP6Transport(ATransport).RemoveServ('ECHO',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('DISCARD',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('SYSTAT',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('DAYTIME',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NETSTAT',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('QOTD',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('CHARGEN',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('FTP-DATA',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('FTP',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('TELNET',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('SMTP',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('TIME',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NAME',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('WHOIS',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('DOMAIN',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NAMESERVER',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('MTP',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('FINGER',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('POP3',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('PORTMAP',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('AUTH',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NNTP',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('NBSESSION',TCP_PROTOCOL_NAME);
            TIP6Transport(ATransport).RemoveServ('IMAP4',TCP_PROTOCOL_NAME);
            
            {Remove Proto Entry}
            TIP6Transport(ATransport).RemoveProto(TCP_PROTOCOL_NAME);
            
            {Remove Control Socket}
            {FSockets.Remove(Transport.Socket);} {This one is not on the list}
            Transport.Socket.Free;
            
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
        IPPROTO_ICMPV6:begin
          {Remove ICMP6 Protocol}
          if TIP6Transport(ATransport).RemoveProtocol(Transport.Handle,Transport.Protocol) then
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
        else
         begin
          {Unlock Transport}
          Transport.WriterUnlock;
         end;         
       end;  
      
       {Get Transport}
       Transport:=TTCPProtocolTransport(GetTransportByTransport(ATransport,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
      end; 
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
{Find a protocol socket based on all relevant parameters}
{Family: Socket address family (eg AF_INET}
{Struct: Socket type (eg SOCK_DGRAM)}
{Protocol: Socket protocol (eg IPPROTO_UDP)}
{LocalAddress: Local transport address to match (Host Order)}
{RemoteAddress: Remote transport address to match (Host Order)}
{LocalPort: Local port to match (Host Order)}
{RemotePort: Remote port to match (Host Order)}
{Broadcast: If True then match broadcast addresses}
{Listen: If True then match only listening sockets}
{Lock: If True then lock the found entry before returning}
var
 Socket:TTCPSocket;
begin
 {}
 Result:=nil;
 
 if not FSockets.ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: FindSocket');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Family = ' + AddressFamilyToString(AFamily));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Struct = ' + SocketTypeToString(AStruct));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Protocol = ' + ProtocolToString(AProtocol));
  {$ENDIF}
  
  {Get Socket}
  Socket:=TTCPSocket(FSockets.First);
  while Socket <> nil do
   begin
    {Check for Closed}
    if not Socket.SocketState.Closed then
     begin
      {Check for Match}
      if (Socket.Family = AFamily) and (Socket.Struct = AStruct) and (Socket.Proto = AProtocol) then
       begin
        if not AListen then
         begin
          {Check for a Connected Socket}
          if Socket.IsConnected(ALocalAddress,ARemoteAddress,ALocalPort,ARemotePort,ABroadcast) then
           begin
            {Lock Socket}
            if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
            
            {Return Result}
            Result:=Socket;
            Exit;
           end;
         end
        else
         begin
          {Check for a Listening Socket}
          if Socket.IsListening(ALocalAddress,ARemoteAddress,ALocalPort,ARemotePort,ABroadcast) then
           begin
            {Lock Socket}
            if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
            
            {Return Result}
            Result:=Socket;
            Exit;
           end;
         end;
       end;
     end;
     
    {Get Next}
    Socket:=TTCPSocket(Socket.Next);
   end;
 finally 
  FSockets.ReaderUnlock;
 end; 
end;

{==============================================================================}

procedure TTCPProtocol.FlushSockets(All:Boolean);
{Flush sockets from the socket cache}
{All: If True flush all sockets, otherwise flush expired sockets}
var
 CurrentTime:Int64;
 Socket:TTCPSocket;
 Current:TTCPSocket;
begin
 {}
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: FlushSockets');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  All = ' + BoolToStr(All));
 {$ENDIF}
 
 {Get Tick Count}
 CurrentTime:=GetTickCount64;
  
 {Get Socket}
 Socket:=TTCPSocket(GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
 while Socket <> nil do
  begin
   {Get Next}
   Current:=Socket;
   Socket:=TTCPSocket(GetSocketByNext(Current,True,False,NETWORK_LOCK_READ));
    
   {Check Socket State}
   if (Current.SocketState.Closed) or (All) then
    begin
     {Check for Close Timeout}
     if ((Current.CloseTime + (CLOSE_TIMEOUT - TCP_TIMEOUT_OFFSET)) < CurrentTime) or (All) then
      begin
       {Convert Socket}
       if Current.ReaderConvert then
        begin
         {Close Port}
         ClosePort(Current);
        
         {Acquire Lock}
         FSockets.WriterLock;
        
         {Remove Socket}
         FSockets.Remove(Current);

         {Release Lock}
         FSockets.WriterUnlock;
         
         {Unlock Socket}
         Current.WriterUnlock;
        
         {Free Socket}
         Current.Free;
         Current:=nil;
        end; 
      end;
    end
   else if TTCPState(Current.ProtocolState).State = TCP_STATE_TIMEWAIT then
    begin
     {Check for Timewait Timeout}
     if ((Current.TimewaitTime + (TIMEWAIT_TIMEOUT - TCP_TIMEOUT_OFFSET)) < CurrentTime) then
      begin
       {Close the Socket}
       Current.SocketError:=ERROR_SUCCESS;
       Current.SocketState.Closed:=True;
       Current.CloseTime:=GetTickCount64;
       TTCPState(Current.ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       Current.SignalChange;
      end;
    end;
    
   {Unlock Socket}
   if Current <> nil then Current.ReaderUnlock;
  end;
end;

{==============================================================================}

function TTCPProtocol.StartProtocol:Boolean;
{Start this protocol ready for sending and receiving}
var
 Transport:TNetworkTransport;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: StartProtocol');
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
 
  {Get Min Backlog}
  FMinBacklog:=Manager.Settings.GetIntegerDefault('TCP_MIN_BACKLOG',TCP_MIN_BACKLOG);

  {Get Max Backlog}
  FMaxBacklog:=Manager.Settings.GetIntegerDefault('TCP_MAX_BACKLOG',TCP_MAX_BACKLOG);
  
  {Get Receive Backlog}
  FReceiveBacklog:=Manager.Settings.GetIntegerDefault('TCP_RECEIVE_BACKLOG',TCP_RECEIVE_BACKLOG);
  
  {Create Thread}
  FThread:=TSocketThread.Create(Self);
  {FThread.FreeOnTerminate:=True;} {Freed by StopProtocol}
  
  {Start Thread}
  FThread.Start;
 
  {Create Timer}
  FTimer:=TSocketTimer.Create(Self);
  
  {Start Timer}
  FTimer.StartTimer(TCP_TIMER_INTERVAL);
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPProtocol.StopProtocol:Boolean;
{Stop this protocol ready for removal}
var
 Transport:TNetworkTransport;
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: StopProtocol');
  {$ENDIF}
 
  {Check Manager}
  if Manager = nil then Exit;
 
  {Check Timer}
  if FTimer = nil then Exit;
  
  {Check Thread}
  if FThread = nil then Exit;
 
  {Stop Timer}
  FTimer.StopTimer;
  
  {Destroy Timer}
  FTimer.Free;
  FTimer:=nil;
  
  {Terminate Thread}
  FThread.Terminate;
  
  {Release Thread}
  FThread.SendSocket(nil);
  
  {Wait For Thread}
  FThread.WaitFor;
  
  {Destroy Thread}
  FThread.Free;
  FThread:=nil;
  
  {Close all Sockets}
  FlushSockets(True);
 
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

function TTCPProtocol.ProcessProtocol:Boolean;
{Process periodic tasks for this protocol}
begin
 {}
 {Close old Sockets}
 FlushSockets(False);
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TTCPProtocol.ProcessSockets:Boolean;
{Process periodic tasks for protocol sockets}
begin
 {}
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TTCPProtocol.ProcessSocket(ASocket:TProtocolSocket):Boolean; 
{Process periodic tasks for a protocol socket}
var
 Size:Word;
 Flags:Byte;
 Window:Word;
 Urgent:Word;
 Data:Pointer;
 Options:Pointer;
 Socket:TTCPSocket;
 Sequence:LongWord;
 Acknowledge:LongWord;
begin
 {}
 Result:=False;
 
 {Get Socket}
 Socket:=TTCPSocket(ASocket);
 if Socket <> nil then
  begin
   {Check Socket}
   if CheckSocket(Socket,True,NETWORK_LOCK_READ) then
    begin
     try
      {Check for something to Send}
      if Socket.SendSegment(Sequence,Acknowledge,Window,Urgent,Flags,Data,Size) then
       begin
        {Insert the Flags}
        if Acknowledge <> 0 then Flags:=Flags or TCP_FLAG_ACK; //To Do //RecvBuffer.AckSegment should probably set this as <> 0 is not a good test !!
        if Urgent <> 0 then Flags:=Flags or TCP_FLAG_URG; //To Do //SendBuffer.ReadSegment will probably set this as Segment.Control will contain URG when needed !!
      
        {Insert the Options}
        Options:=nil;
        if not TTCPOptions(Socket.ProtocolOptions).NoOpt then
         begin
          Options:=TTCPOptions(Socket.ProtocolOptions).Options;
          if not CreateTCPOptions(Socket,Options,Flags) then Exit;
         end;
        
        {Send the Segment}
        Result:=(SendSegment(Socket,@TIPState(Socket.TransportState).LocalAddress,@TIPState(Socket.TransportState).RemoteAddress,Socket.ProtocolState.LocalPort,Socket.ProtocolState.RemotePort,Sequence,Acknowledge,Window,Urgent,Flags,Options,Data,Size) <> SOCKET_ERROR);
       end;     
     finally
      {Unlock Socket}
      Socket.ReaderUnlock;
     end;     
    end;  
  end;
end;

{==============================================================================}
{==============================================================================}
{TTCPSocket}
constructor TTCPSocket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
begin
 {}
 inherited Create(AProtocol,ATransport);
 
 {Check Address Family}
 case Family of
  AF_INET:begin
    {Create IP Transport State}
    FTransportState:=TIPState.Create;
    {Create IP Transport Options}
    FTransportOptions:=TIPOptions.Create;
    
    {Set IP Defaults}
    TIPOptions(FTransportOptions).TTL:=TIPTransport(ATransport).DefaultTTL;
    TIPOptions(FTransportOptions).Flags:=IP_DF;
   end;
  AF_INET6:begin
    {Create IP6 Transport State}
    FTransportState:=TIP6State.Create;
    {Create IP6 Transport Options}
    FTransportOptions:=TIP6Options.Create;

    {Set IP6 Defaults}
    TIP6Options(FTransportOptions).HopLimit:=TIP6Transport(ATransport).DefaultHopLimit;
   end;
  else
   begin
    {Create Transport State}
    FTransportState:=TTransportState.Create;
    {Create Transport Options}
    FTransportOptions:=TTransportOptions.Create;
   end;
 end;

 {Create Protocol State}
 FProtocolState:=TTCPState.Create;
 {Create Protocol Options}
 FProtocolOptions:=TTCPOptions.Create;

 FOpenTime:=0;
 FCloseTime:=0;
 FKeepAliveTime:=0;

 {Create Accept Queue}
 FBackLog:=SOMAXCONN;
 FListener:=nil;
 FAcceptQueue:=TSocketList.Create;

 {Create Receive Queue}
 FReceiveQueue:=TSocketList.Create;
 
 {Create Send and Receive Buffer}
 FSendData:=TTCPSendBuffer.Create(Self);
 FRecvData:=TTCPRecvBuffer.Create(Self);

 FSendData.Size:=TCP_BUFFER_SIZE;
 FRecvData.Size:=TCP_BUFFER_SIZE;

 {Set Socket Defaults}
 FSocketOptions.SendTimeout:=TCP_TIMEOUT;
 FSocketOptions.RecvTimeout:=TCP_TIMEOUT;
end;

{==============================================================================}

destructor TTCPSocket.Destroy;
begin
 {}
 WriterLock;
 try
  {Free Send and Receive Buffer}
  FRecvData.Free;
  FSendData.Free;

  {Free Receive Queue}
  FReceiveQueue.Free;
  
  {Free Accept Queue}
  FAcceptQueue.Free;
  FListener:=nil;
  
  {Free Protocol Options}
  FProtocolOptions.Free;
  {Free Protocol State}
  FProtocolState.Free;

  {Free Transport Options}
  FTransportOptions.Free;
  {Free Transport State}
  FTransportState.Free;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

procedure TTCPSocket.SetBackLog(ABackLog:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FBackLog:=ABackLog;

 ReleaseLock;
end;

{==============================================================================}

procedure TTCPSocket.SetListener(AListener:TProtocolSocket);
begin
 {}
 if not AcquireLock then Exit;

 FListener:=AListener;

 ReleaseLock;
end;

{==============================================================================}

function TTCPSocket.GetOption(ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
{Note: With Socket Options that are Boolean 0 is False and > 0 is True}
begin
 {}
 Result:=SOCKET_ERROR;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: GetOption');
  {$ENDIF}
  
  {Check Level}
  case ALevel of
   IPPROTO_TCP:begin
     NetworkSetLastError(WSAENOPROTOOPT);
     
     {Check Option}
     case AOptName of
      TCP_NODELAY,TCP_MAXSEG,TCP_NOPUSH,TCP_NOOPT,TCP_BSDURGENT,TCP_WSCALE,TCP_NOSACK:begin
        NetworkSetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          {Setup Return}
          PInteger(AOptValue)^:=0;
          AOptLength:=SizeOf(Integer);
          
          {Check Option}
          case AOptName of
           TCP_NODELAY:begin
             if TTCPOptions(ProtocolOptions).NoDelay then PInteger(AOptValue)^:=1;
            end;
           TCP_MAXSEG:begin
             PInteger(AOptValue)^:=TTCPOptions(ProtocolOptions).MaxSeg;
            end;
           TCP_NOPUSH:begin
             if TTCPOptions(ProtocolOptions).NoPush then PInteger(AOptValue)^:=1;
            end;
           TCP_NOOPT:begin
             if TTCPOptions(ProtocolOptions).NoOpt then PInteger(AOptValue)^:=1;
            end;
           TCP_BSDURGENT:begin
             if TTCPOptions(ProtocolOptions).BsdUrgent then PInteger(AOptValue)^:=1;
            end;
           TCP_WSCALE:begin
             PInteger(AOptValue)^:=TTCPOptions(ProtocolOptions).WindowScale;
            end;
           TCP_NOSACK:begin
             if TTCPOptions(ProtocolOptions).NoSack then PInteger(AOptValue)^:=1;
            end;
          end;
          
          {Return Result}
          NetworkSetLastError(ERROR_SUCCESS);
          Result:=NO_ERROR;
         end;
       end;
     end;
    end;
   else
    begin
     Result:=inherited GetOption(ALevel,AOptName,AOptValue,AOptLength);
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.SetOption(ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
{Note: With Socket Options that are Boolean 0 is False and > 0 is True}
begin
 {}
 Result:=SOCKET_ERROR;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: SetOption');
  {$ENDIF}
  
  {Check Level}
  case ALevel of
   IPPROTO_TCP:begin
     NetworkSetLastError(WSAENOPROTOOPT);
     
     {Check Option}
     case AOptName of
      TCP_NODELAY,TCP_MAXSEG,TCP_NOPUSH,TCP_NOOPT,TCP_BSDURGENT,TCP_WSCALE,TCP_NOSACK:begin
        NetworkSetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          {Check Option}
          case AOptName of
           TCP_NODELAY:begin
             TTCPOptions(ProtocolOptions).NoDelay:=(PInteger(AOptValue)^ <> 0);
             SendData.NoNagle:=(PInteger(AOptValue)^ <> 0);
            end;
           TCP_MAXSEG:begin
             //To Do //Use Min to check for greater than Max Segment
             {Note: This is the value we send to Remote}
             TTCPOptions(ProtocolOptions).MaxSeg:=PInteger(AOptValue)^;
             RecvData.MaxSeg:=PInteger(AOptValue)^;
            end;
           TCP_NOPUSH:begin
             TTCPOptions(ProtocolOptions).NoPush:=(PInteger(AOptValue)^ <> 0);
             SendData.NoPush:=(PInteger(AOptValue)^ <> 0);
            end;
           TCP_NOOPT:begin
             TTCPOptions(ProtocolOptions).NoOpt:=(PInteger(AOptValue)^ <> 0);
            end;
           TCP_BSDURGENT:begin
             TTCPOptions(ProtocolOptions).BsdUrgent:=(PInteger(AOptValue)^ <> 0);
            end;
           TCP_WSCALE:begin
             //To Do //Use Min to check for greater than Max Window Scale
             {Note: This is the value we send to Remote}
             TTCPOptions(ProtocolOptions).WindowScale:=PInteger(AOptValue)^;
             RecvData.WindowScale:=PInteger(AOptValue)^;
            end;
           TCP_NOSACK:begin
             {Note: This is the value we send to Remote}
             TTCPOptions(ProtocolOptions).NoSack:=(PInteger(AOptValue)^ <> 0);
             SendData.NoSack:=(PInteger(AOptValue)^ <> 0);
             RecvData.NoSack:=(PInteger(AOptValue)^ <> 0);
            end;
          end;
          
          {Return Result}
          NetworkSetLastError(ERROR_SUCCESS);
          Result:=NO_ERROR;
         end;
       end;
     end;
    end;
   else
    begin
     Result:=inherited SetOption(ALevel,AOptName,AOptValue,AOptLength);
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {}
 Result:=SOCKET_ERROR;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: IoCtl');
  {$ENDIF}
 
  {Check Commmand}
  NetworkSetLastError(WSAEINVAL);
  case ACommand of
   FIONREAD:begin
     AArgument:=RecvData.GetAvailable;
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
    end;
   FIONBIO:begin
     SocketState.NonBlocking:=(AArgument <> 0);
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
    end;
   FIOASYNC:begin
     SocketState.Async:=(AArgument <> 0);
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
    end;
   SIOCATMARK:begin
     if not SocketOptions.UrgentInline then Exit;
     //To Do //only applies if SO_OOBINLINE
     //Return True is no OOB data waiting to be read
     //otherwise returns False and because it is OOBINLINE
     //the next Recv will read OOB data
     //RecvData.GetUrgent
     
     {Return Result}
     NetworkSetLastError(ERROR_SUCCESS);
     Result:=NO_ERROR;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this TCP Socket is Connected to the Host specified by RemoteAddress/Port}
{A connected Socket will have a Bound LocalAddress/Port and a Connected RemoteAddress/Port}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: IsConnected');
  {$ENDIF}
  
  {Note: TCP Sockets dont allow Broadcasts}
  if ABroadcast then Exit;

  {Check Local}
  if ALocalAddress = nil then Exit;
  
  {Check Remote}
  if ARemoteAddress = nil then Exit;
  
  {Check for Bound}
  if not SocketState.LocalAddress then Exit;
  
  {Check for Connected}
  if not SocketState.RemoteAddress then Exit; {Instead of Connected for Handshake and Close}
  
  {Check Address Family}
  case Family of
   AF_INET:begin
     {Check the Bound LocalAddress}
     if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).LocalAddress,PInAddr(ALocalAddress)^) then Exit;
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Check the Connected RemoteAddress}
     if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).RemoteAddress,PInAddr(ARemoteAddress)^) then Exit;
     
     {Check the Connected RemotePort}
     if ProtocolState.RemotePort <> ARemotePort then Exit;
     
     {Return Result}
     Result:=True;
    end;
   AF_INET6:begin
     {Check the Bound LocalAddress}
     if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).LocalAddress,PIn6Addr(ALocalAddress)^) then Exit;
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Check the Connected RemoteAddress}
     if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).RemoteAddress,PIn6Addr(ARemoteAddress)^) then Exit;
     
     {Check the Connected RemotePort}
     if ProtocolState.RemotePort <> ARemotePort then Exit;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
{Check if this TCP Socket is Listening on the LocalAddress/Port specified}
{A listening Socket may or may not have a Bound LocalAddress/Port and will have a default (INADDR_ANY/IPPORT_ANY) RemoteAddress/Port}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: IsListening');
  {$ENDIF}
  
  {Note: TCP Sockets dont allow Broadcasts}
  if ABroadcast then Exit;
  
  {Check Local}
  if ALocalAddress = nil then Exit;
  
  {Check Remote}
  {if ARemoteAddress = nil then Exit;} {Not Used}
  
  {Check for Bound}
  if not SocketState.LocalAddress then Exit;
  
  {Check for Listening}
  if not SocketState.Listening then Exit; {Dont check for Connected}
  
  {Check Address Family}
  case Family of
   AF_INET:begin
     {Check the LocalAddress}
     if not TIPTransport(Transport).CompareDefault(TIPState(TransportState).LocalAddress) then
      begin
       {Check the Bound LocalAddress}
       if not TIPTransport(Transport).CompareAddress(TIPState(TransportState).LocalAddress,PInAddr(ALocalAddress)^) then Exit;
      end;
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Return Result}
     Result:=True;
    end;
   AF_INET6:begin
     {Check the LocalAddress}
     if not TIP6Transport(Transport).CompareDefault(TIP6State(TransportState).LocalAddress) then
      begin
       {Check the Bound LocalAddress}
       if not TIP6Transport(Transport).CompareAddress(TIP6State(TransportState).LocalAddress,PIn6Addr(ALocalAddress)^) then Exit;
      end;
     
     {Check the Bound LocalPort}
     if ProtocolState.LocalPort <> ALocalPort then Exit;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.Listen:Boolean;
{Initiate listening for connections}
{Used by Listen to setup the listening state}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: Listen');
  {$ENDIF}
  
  {Check the State}
  case TTCPState(ProtocolState).State of
   TCP_STATE_CLOSED:begin {Socket is newly created}
     {Clear the Queues}
     AcceptQueue.ClearList;
     ReceiveQueue.ClearList;
     
     {Change State to Listen}
     TTCPState(ProtocolState).State:=TCP_STATE_LISTEN;
     SocketOptions.Accept:=True;
     SocketState.Listening:=True;
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.Connect:Boolean;
{Initiate a connection handshake (SYN/SYNACK/ACK)}
{Used by Connect to initiate outbound connection}
{Used by RecvSegment to respond to inbound connection}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: Connect');
  {$ENDIF}
  
  {Check the State}
  case TTCPState(ProtocolState).State of
   TCP_STATE_SYNREC:begin {Socket has received a SYN}
     {Send a SYN}
     if not SendData.Synchronize then Exit;
     
     {TCP State already set by Receive Segment}
     {Open Time already set by Receive Segment}
     SocketState.Connecting:=True;
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
   TCP_STATE_CLOSED:begin {Socket is newly created}
     {Send a SYN}
     if not SendData.Synchronize then Exit;
     
     {Change State to SynSent}
     TTCPState(ProtocolState).State:=TCP_STATE_SYNSENT;
     SocketState.Connecting:=True;
     OpenTime:=GetTickCount64;
     //To Do //Is it here where we set NoNagle and MaxSeg etc //on SendBuffer ??
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.Reconnect:Boolean;
{Retry the connection handshake (SYN/SYNACK/ACK)}
{Used by Connect to retry outbound connection}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: Reconnect');
  {$ENDIF}
  
  {Check the State}
  case TTCPState(ProtocolState).State of
   TCP_STATE_CLOSED:begin {Socket is newly created}
     {Reset Socket}
     SocketError:=ERROR_SUCCESS;
     SocketState.Closed:=False;
     CloseTime:=0;
     
     {Reset Send}
     SendData.SynSequence:=0;
     
     {Send a SYN}
     if not SendData.Synchronize then Exit;
     
     {Change State to SynSent}
     TTCPState(ProtocolState).State:=TCP_STATE_SYNSENT;
     SocketState.Connecting:=True;
     OpenTime:=GetTickCount64;
     //To Do //Is it here where we set NoNagle and MaxSeg etc //on SendBuffer ??
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.Disconnect:Boolean;
{Initiate a disconnection handshake (FIN/ACK)}
{Used by CloseSocket/Shutdown to terminate outbound connection}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: Disconnect');
  {$ENDIF}
  
  {Check the State}
  case TTCPState(ProtocolState).State of
   TCP_STATE_LISTEN:begin
     {Close the Socket}
     TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
     SocketOptions.Accept:=False;
     SocketState.Closed:=True;
     CloseTime:=GetTickCount64;
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
   TCP_STATE_SYNSENT:begin {Socket has sent a SYN but not received an ACK}
     {Close the Socket}
     TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
     SocketState.Closed:=True;
     CloseTime:=GetTickCount64;
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
   TCP_STATE_SYNREC,TCP_STATE_ESTAB:begin {Socket has sent and/or received a SYN and/or ACK}
     {Send a FIN}
     if not SendData.Finish then Exit;
     
     {Change State to FinWait1}
     TTCPState(ProtocolState).State:=TCP_STATE_FINWAIT1;
     SocketState.Disconnecting:=True;
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
   TCP_STATE_CLOSWAIT:begin {Socket has received a FIN}
     {Send a FIN}
     if not SendData.Finish then Exit;
     
     {Change State to LastAck}
     TTCPState(ProtocolState).State:=TCP_STATE_LASTACK; {Not CLOSING - See RFC 1122}
     SocketState.Disconnecting:=True;
     
     {Signal the Event}
     SignalChange;
     
     {Return Result}
     Result:=True;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.Accept(APeek,ALock:Boolean;AState:LongWord):TTCPSocket;
{Return the first Connected socket in the Accept Queue}
{Used by Accept/Select to receive an inbound connection}
var
 Count:Integer;
 Socket:TTCPSocket;
begin
 {}
 Result:=nil;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: Accept');
  {$ENDIF}
  
  {Check the State}
  case TTCPState(ProtocolState).State of
   TCP_STATE_LISTEN:begin {Socket is Listening}
     {Acquire Lock}
     if not AcceptQueue.AcquireLock then Exit;
     try
      {Look for a Connected socket}
      for Count:=0 to AcceptQueue.Count - 1 do
       begin
        {Get Socket}
        Socket:=TTCPSocket(AcceptQueue.Items[Count]);
        if Socket <> nil then
         begin
          {Check Socket State}
          if Socket.SocketState.Connected then
           begin
            {Remove from the Queue}
            if not APeek then AcceptQueue.Remove(Socket);
            
            {Lock Socket}
            if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
           
            {Return Result}
            Result:=Socket;
            Exit;
           end;
         end;
       end;
     finally
      AcceptQueue.ReleaseLock;
     end;     
    end;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.RecvSegment(ASequence,AAcknowledge:LongWord;AWindow,AUrgent:Word;AFlags:Byte;AData:Pointer;ASize:Word):Boolean;
{Used by Segment Handler to feed segments/acks to the Buffers}

{Note: ACK/SEQ have passed initial check by the Segment Handler}
{Note: Flags have passed a validity check by the Segment Handler}
{Note: Size can be zero, Data will then point to invalid or nil}
{Note: State transition and simultaneous Connect are done here}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: RecvSegment');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  Sequence    = ' + IntToStr(ASequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  Acknowledge = ' + IntToStr(AAcknowledge));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  Window      = ' + IntToStr(AWindow));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  Urgent      = ' + IntToStr(AUrgent));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  Size        = ' + IntToStr(ASize));
  {$ENDIF}
  
  {Check the State}
  case TTCPState(ProtocolState).State of
   TCP_STATE_SYNSENT:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_SYNSENT');
     {$ENDIF}
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Check for RST}
       if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
        begin
         {Connection Refused}
         SocketError:=WSAECONNREFUSED;
         SocketState.Closed:=True;
         CloseTime:=GetTickCount64;
         TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
         
         {Signal the Event}
         SignalChange;
         
         {Return Result}
         Result:=True;
         Exit;
        end;
       
       {Check for SYN}
       if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
        begin
         {Write Data}
         if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
         
         {Initial Window}
         SendData.WindowSequence:=ASequence;
         SendData.WindowAcknowledge:=AAcknowledge;
         SendData.WindowSize:=(AWindow shl SendData.WindowScale);
         
         {Check SYN Acknowledged}
         if SendData.SynAcknowledged then
          begin
           {Connection Establised}
           SocketError:=ERROR_SUCCESS;
           SocketState.Connected:=True;
           TTCPState(ProtocolState).State:=TCP_STATE_ESTAB;

           {Check the Listener}           
           if Listener <> nil then
            begin
             if CheckSocket(Listener,True,NETWORK_LOCK_READ) then
              begin
               {Remove the Socket}
               TTCPSocket(Listener).ReceiveQueue.Remove(Self);
               
               {Add the Socket}
               TTCPSocket(Listener).AcceptQueue.Add(Self);
               
               {Signal the Listener}
               Listener.SignalChange;
               
               {Unlock Listener}
               Listener.ReaderUnlock;
              end;
            end;
          end
         else
          begin
           {Connection Waiting}
           SocketError:=ERROR_SUCCESS;
           SocketState.Connecting:=True;
           TTCPState(ProtocolState).State:=TCP_STATE_SYNREC;
          end;
         
         {Signal the Event}
         SignalChange;
          
         {Return Result}
         Result:=True;
         Exit;
        end;
      end
     else
      begin
       {Check for SYN}
       if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
        begin
         {Write Data}
         if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
         
         {Initial Window}
         SendData.WindowSequence:=ASequence;
         SendData.WindowSize:=(AWindow shl SendData.WindowScale);
         
         {Connection Restarted}
         SocketError:=ERROR_SUCCESS;
         SocketState.Connecting:=True;
         OpenTime:=GetTickCount64;
         TTCPState(ProtocolState).State:=TCP_STATE_SYNREC;
         
         {Reset Send and Send SYN}
         SendData.SynSequence:=0;

         {Signal the Event}
         SignalChange;
         
         {Return Result}
         Result:=Connect;
         Exit;
        end;
      end;
    end;
   TCP_STATE_SYNREC:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_SYNREC');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Refused}
       SocketError:=WSAECONNREFUSED;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Check for FIN}
       if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
        begin
         //To Do //Should we check SynAcknowledged ???
         //RFC says shouldn't go to CLOSWAIT unless SYN is ACKed !! - Yes !! 

         //To do so we would need WriteSegment to check for SYN/FIN and
         //set SynSequence/FinSequence and then have functions for
         //SynReceived/FinReceived so that we could check later ?? - Done
 
         {Connection Closing}
         SocketError:=ERROR_SUCCESS;
         SocketState.Disconnecting:=True;
         TTCPState(ProtocolState).State:=TCP_STATE_CLOSWAIT;
        end
       else
        begin
         {Check SYN Acknowledged}
         if SendData.SynAcknowledged then
          begin
           {Connection Establised}
           SocketError:=ERROR_SUCCESS;
           SocketState.Connected:=True;
           TTCPState(ProtocolState).State:=TCP_STATE_ESTAB;
           
           {Check the Listener}           
           if Listener <> nil then
            begin
             if CheckSocket(Listener,True,NETWORK_LOCK_READ) then
              begin
               {Remove the Socket}
               TTCPSocket(Listener).ReceiveQueue.Remove(Self);
               
               {Add the Socket}
               TTCPSocket(Listener).AcceptQueue.Add(Self);
               
               {Signal the Listener}
               Listener.SignalChange;
               
               {Unlock Listener}
               Listener.ReaderUnlock;
              end;
            end;
          end;
        end;
       
       {Signal the Event}
       SignalChange;
        
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_ESTAB:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_ESTAB');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Check for FIN}
       if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
        begin
         {Connection Closing}
         SocketError:=ERROR_SUCCESS;
         SocketState.Disconnecting:=True;
         TTCPState(ProtocolState).State:=TCP_STATE_CLOSWAIT;
        end;
       //To Do //See handling of FIN above //Need to check FinReceived and move to
       //CLOSWAIT
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_FINWAIT1:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_FINWAIT1');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
      
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Check for FIN}
       if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
        begin
         {Check FIN Acknowledged}
         if SendData.FinAcknowledged then
          begin
           {Connection Closing}
           SocketError:=ERROR_SUCCESS;
           SocketState.Unconnected:=True;
           TimewaitTime:=GetTickCount64;
           TTCPState(ProtocolState).State:=TCP_STATE_TIMEWAIT;
          end
         else
          begin
           {Connection Closing}
           SocketError:=ERROR_SUCCESS;
           SocketState.Disconnecting:=True;
           TTCPState(ProtocolState).State:=TCP_STATE_CLOSING;
          end;
        end
       else
        begin
         {Check FIN Acknowledged}
         if SendData.FinAcknowledged then
          begin
           {Connection Closing}
           SocketError:=ERROR_SUCCESS;
           SocketState.Disconnecting:=True;
           TTCPState(ProtocolState).State:=TCP_STATE_FINWAIT2;
          end;
        end;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_FINWAIT2:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_FINWAIT2');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
      
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Check for FIN}
       if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
        begin
         {Connection Closing}
         SocketError:=ERROR_SUCCESS;
         SocketState.Unconnected:=True;
         TimewaitTime:=GetTickCount64;
         TTCPState(ProtocolState).State:=TCP_STATE_TIMEWAIT;
        end;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_CLOSWAIT:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_CLOSWAIT');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Signal the Event}
       SignalChange;
       
       {Waiting for call to CloseSocket}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_CLOSING:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_CLOSING');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Closed}
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Check FIN Acknowledged}
       if SendData.FinAcknowledged then
        begin
         {Connection Closing}
         SocketError:=ERROR_SUCCESS;
         SocketState.Unconnected:=True;
         TimewaitTime:=GetTickCount64;
         TTCPState(ProtocolState).State:=TCP_STATE_TIMEWAIT;
        end;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_LASTACK:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_LASTACK');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Closed}
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Check FIN Acknowledged}
       if SendData.FinAcknowledged then
        begin
         SocketError:=ERROR_SUCCESS;
         SocketState.Closed:=True;
         CloseTime:=GetTickCount64;
         TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
        end;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_TIMEWAIT:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_TIMEWAIT');
     {$ENDIF}
     
     {Check for RST}
     if (AFlags and TCP_FLAG_RST) = TCP_FLAG_RST then
      begin
       {Connection Closed}
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Connection Reset}
       SocketError:=WSAECONNRESET;
       SocketState.Closed:=True;
       CloseTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_CLOSED;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
     
     {Check for ACK}
     if (AFlags and TCP_FLAG_ACK) = TCP_FLAG_ACK then
      begin
       {Check Acknowledge}
       if not SendData.AcknowledgeSegments(ASequence,AAcknowledge,AWindow) then Exit;
       
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       //To Do //Send an Acknowledge - Must be a Retransmit of FIN
                     //Do this in SegmentHandler ??? - Already Done ??? by CheckSequence
       
       {Check for FIN}
       if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then
        begin
         {Reset the Timewait Timeout}
         TimewaitTime:=GetTickCount64; //This might have to go in SegmentHandler ??
         //To Do //Send an Acknowledge - Must be a Retransmit of FIN
                       //Do this in SegmentHandler ??? - Already Done ??? by CheckSequence
        end;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=True;
       Exit;
      end;
    end;
   TCP_STATE_CLOSED:begin
     {$IFDEF TCP_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket:  State       = TCP_STATE_CLOSED');
     {$ENDIF}
     
     {Can only occur on a newly cloned Socket}
     {Otherwise the Segment Handler rejects it}
     {Check for SYN}
     if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then
      begin
       {Write Data}
       if not RecvData.WriteSegment(ASequence,AUrgent,AFlags,AData,ASize) then Exit;
       
       {Initial Window}
       SendData.WindowSequence:=ASequence;
       SendData.WindowSize:=(AWindow shl SendData.WindowScale);
       
       {Connection Started}
       SocketError:=ERROR_SUCCESS;
       SocketState.Connecting:=True;
       OpenTime:=GetTickCount64;
       TTCPState(ProtocolState).State:=TCP_STATE_SYNREC;
       
       {Signal the Event}
       SignalChange;
       
       {Return Result}
       Result:=Connect;
       Exit;
      end;
    end;
  end;
  {All others should not happen}
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTCPSocket.SendSegment(var ASequence,AAcknowledge:LongWord;var AWindow,AUrgent:Word;var AFlags:Byte;var AData:Pointer;var ASize:Word):Boolean;
{Used by Process Socket to get segments/acks for Sending}

{Note: Size can return zero and Data can return nil}
begin
 {}
 Result:=False;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Socket: SendSegment');
  {$ENDIF}
  
  {Check for Send}
  {Get Segment to (Re)send}
  if SendData.ReadSegment(ASequence,AUrgent,AFlags,AData,ASize,False) then
   begin
    {Get Acknowledge to Send (Forced to prevent Delayed ACK)}
    RecvData.AcknowledgeSegments(AAcknowledge,AWindow,True);
    
    {Return Result}
    Result:=True;
   end
  else
   begin
    {Get Acknowledge to Send (If any is available)}
    Result:=RecvData.AcknowledgeSegments(AAcknowledge,AWindow,False);
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TTCPState}
constructor TTCPState.Create;
begin
 {}
 inherited Create;
 FState:=TCP_STATE_CLOSED;
end;

{==============================================================================}

destructor TTCPState.Destroy;
begin
 {}
 AcquireLock;
 try
  FState:=TCP_STATE_CLOSED;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

procedure TTCPState.SetState(AState:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FState:=AState;

 ReleaseLock;
end;

{==============================================================================}

function TTCPState.GetListening:Boolean;
begin
 {}
 Result:=(FState = TCP_STATE_LISTEN);
end;

{==============================================================================}

procedure TTCPState.SetListening(AValue:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AValue then FState:=TCP_STATE_LISTEN;

 ReleaseLock;
end;

{==============================================================================}

function TTCPState.GetEstablished:Boolean;
begin
 {}
 Result:=(FState = TCP_STATE_ESTAB);
end;

{==============================================================================}

procedure TTCPState.SetEstablished(AValue:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AValue then FState:=TCP_STATE_ESTAB;

 ReleaseLock;
end;

{==============================================================================}

function TTCPState.GetClosed:Boolean;
begin
 {}
 Result:=(FState = TCP_STATE_CLOSED);
end;

{==============================================================================}

procedure TTCPState.SetClosed(AValue:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AValue then FState:=TCP_STATE_CLOSED;

 ReleaseLock;
end;

{==============================================================================}

function TTCPState.GetSynchronized:Boolean;
{Check for one of the synchronized states, all others are non synchronized or closed}
begin
 {}
 Result:=False;
 
 {Check State}
 case FState of
  TCP_STATE_ESTAB,TCP_STATE_FINWAIT1,TCP_STATE_FINWAIT2,TCP_STATE_CLOSWAIT,TCP_STATE_CLOSING,TCP_STATE_LASTACK,TCP_STATE_TIMEWAIT:begin
    Result:=True;
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{TTCPOptions}
constructor TTCPOptions.Create;
begin
 {}
 inherited Create;
 FMemory:=TMemoryStream.Create;
 FMemory.SetSize(TCP_OPTIONS_SIZE);
 FOptions:=TCP_NOSACK; //To Do //Defaults  //Nagle/Sack/Timestamp/WindowScale ??
 
 FMaxSeg:=TCP_MAX_MSS;
 FWindowScale:=0;
end;

{==============================================================================}

destructor TTCPOptions.Destroy;
begin
 {}
 AcquireLock;
 try
  FMemory.Free;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

procedure TTCPOptions.SetMaxSeg(AMaxSeg:Word); 
begin
 {}
 if not AcquireLock then Exit;

 FMaxSeg:=AMaxSeg;

 ReleaseLock;
end;

{==============================================================================}

procedure TTCPOptions.SetWindowScale(AWindowScale:Byte);
begin
 {}
 if not AcquireLock then Exit;

 FWindowScale:=AWindowScale;

 ReleaseLock;
end;

{==============================================================================}

function TTCPOptions.GetOptions:Pointer;
begin
 {}
 Result:=FMemory.Memory;
end;

{==============================================================================}

function TTCPOptions.GetNoDelay:Boolean;
begin
 {}
 Result:=(FOptions and TCP_NODELAY) = TCP_NODELAY;
end;

{==============================================================================}

procedure TTCPOptions.SetNoDelay(ANoDelay:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ANoDelay then
  FOptions:=FOptions or TCP_NODELAY
 else
  FOptions:=FOptions and not(TCP_NODELAY);

 ReleaseLock;
end;

{==============================================================================}

function TTCPOptions.GetNoPush:Boolean;
begin
 {}
 Result:=(FOptions and TCP_NOPUSH) = TCP_NOPUSH;
end;

{==============================================================================}

procedure TTCPOptions.SetNoPush(ANoPush:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ANoPush then
  FOptions:=FOptions or TCP_NOPUSH
 else
  FOptions:=FOptions and not(TCP_NOPUSH);

 ReleaseLock;
end;

{==============================================================================}

function TTCPOptions.GetNoOpt:Boolean;
begin
 {}
 Result:=(FOptions and TCP_NOOPT) = TCP_NOOPT;
end;

{==============================================================================}

procedure TTCPOptions.SetNoOpt(ANoOpt:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ANoOpt then
  FOptions:=FOptions or TCP_NOOPT
 else
  FOptions:=FOptions and not(TCP_NOOPT);

 ReleaseLock;
end;

{==============================================================================}

function TTCPOptions.GetBsdUrgent:Boolean;
begin
 {}
 Result:=(FOptions and TCP_BSDURGENT) = TCP_BSDURGENT;
end;

{==============================================================================}

procedure TTCPOptions.SetBsdUrgent(ABsdUrgent:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ABsdUrgent then
  FOptions:=FOptions or TCP_BSDURGENT
 else
  FOptions:=FOptions and not(TCP_BSDURGENT);

 ReleaseLock;
end;

{==============================================================================}

function TTCPOptions.GetNoSack:Boolean;
begin
 {}
 Result:=(FOptions and TCP_NOSACK) = TCP_NOSACK;
end;

{==============================================================================}

procedure TTCPOptions.SetNoSack(ANoSack:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ANoSack then
  FOptions:=FOptions or TCP_NOSACK
 else
  FOptions:=FOptions and not(TCP_NOSACK);

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TTCPSendBuffer}
constructor TTCPSendBuffer.Create;
begin
 {}
 inherited Create(ASocket);
 FFirst:=nil;
 FLast:=nil;

 StartSequence:=GetTickCount; //To Do //Should this be something else now ?
 NextSequence:=StartSequence;
 LastSequence:=StartSequence;
 LastAcknowledge:=StartSequence;

 UrgentPointer:=0;

 MaxSeg:=TCP_DEFAULT_MSS;  {Start with MSS equal to Default (536) - Remote will tell us with SYN}

 WindowSize:=(MaxSeg * 4); {Start with a 4 x MSS Window as per RFC - Remote will tell us with SYN}
 WindowScale:=0;
 WindowTimeout:=0;
 
 CongestionWindow:=MaxSeg; {Slow Start Window of 1 Segment}

 SynSequence:=0;
 FinSequence:=0;

 WindowSequence:=0;
 WindowAcknowledge:=StartSequence; 

 NoPush:=False;             {Enable Push by Default}
 NoSack:=True;              {Disable Selective Ack by Default}
 NoNagle:=False;            {Enable Nagle by Default}

 VjSa:=INIT_VJSA;
 VjSd:=INIT_VJSA;
 VjLast:=0;
end;

{==============================================================================}

destructor TTCPSendBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FlushSegments(True);
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.AddSegment(ASequence:LongWord;AFlags:Byte;ASize:Word):PTCPSegment;
{Adds a new Segment as the last segment in the list}
{Sequence is the start sequence, Size can be zero}
{Size should not include the SYN and FIN sequence}
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: AddSegment');
  {$ENDIF}
  
  {Check Buffer Free}
  if ASize > FFree then Exit;
  
  {Create a new Segment and Data}
  Result:=GetMem(SizeOf(TTCPSegment) + TCP_MAX_MSS); {TCP_SEGMENT_SIZE}
  if Result = nil then Exit;
  
  {Update Segment}
  Result.Size:=ASize;
  Result.Data:=Pointer(LongWord(Result) + SizeOf(TTCPSegment)); {TCP_SEGMENT_SIZE}
  Result.FirstSequence:=ASequence;
  Result.LastSequence:=ASequence + ASize;
  Result.Control:=AFlags;
  Result.Transferred:=False;
  Result.Acknowledged:=False;
  Result.SelectiveAck:=False;
  Result.RoundTripTime:=0;
  Result.Count:=0;
  Result.Timeout:=0;
  Result.Prev:=nil;
  Result.Next:=nil;
  Result.Item.Flags:=SOCKET_TIMER_FLAG_NONE;
  
  {Check Flags}
  if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Inc(Result.LastSequence);
  if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then Inc(Result.LastSequence);
  
  {Add to List}
  if FLast = nil then
   begin
    {Is First Segment}
    FFirst:=Result;
    FLast:=Result;
   end
  else
   begin
    {Not First Segment}
    FLast.Next:=Result;
    Result.Prev:=FLast;
    FLast:=Result;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.RemoveSegment(ASegment:PTCPSegment):Boolean;
{Removes the passed Segment from the list}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: RemoveSegment');
  {$ENDIF}
  
  {Check Segment}
  if ASegment = nil then Exit;
  
  {Remove from List}
  if ASegment.Prev <> nil then
   begin
    {Not First Segment}
    if ASegment.Next <> nil then
     begin
      {Not Last Segment}
      ASegment.Prev.Next:=ASegment.Next;
      ASegment.Next.Prev:=ASegment.Prev;
     end
    else
     begin
      {Is Last Segment}
      ASegment.Prev.Next:=nil;
      FLast:=ASegment.Prev;
     end;
   end
  else
   begin
    {Is First Segment}
    if ASegment.Next <> nil then
     begin
      {Not Last Segment}
      ASegment.Next.Prev:=nil;
      FFirst:=ASegment.Next;
     end
    else
     begin
      {Is Last Segment}
      FFirst:=nil;
      FLast:=nil;
     end;
   end;
  
  {Unschedule the Thread} 
  TProtocolSocket(FSocket).UnscheduleSocketItem(@ASegment.Item);
  
  {Free the Segment and Data}
  FreeMem(ASegment,SizeOf(TTCPSegment) + TCP_MAX_MSS); {TCP_SEGMENT_SIZE}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TTCPSendBuffer.FlushSegments(All:Boolean);
{Removes ACKed Segments from the buffer (or All)}
var
 Segment:PTCPSegment;
 Current:PTCPSegment;
begin
 {}
 if not AcquireLock then Exit;
 try
  {Get Segment}
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Get Next}
    Current:=Segment;
    Segment:=Current.Next;
    
    {Check Status}
    if ((Current.Acknowledged and Current.Transferred)) or (All) then
     begin
      {Remove Segments}
      RemoveSegment(Current);
     end
    else
     begin
      {Exit if we found an Unacknowledged or Unwritten segment}
      Exit;
     end;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TTCPSendBuffer.SetSize(ASize:LongWord);
{For TCP setting the size does not clear existing segments}
{Size cannot be set smaller than the default or existing}
begin
 {}
 if not AcquireLock then Exit;
 try
  {Check Size}
  if ASize = 0 then Exit;
  
  {Get Size}
  ASize:=Max(ASize,TCP_BUFFER_SIZE);
  ASize:=Max(FSize,ASize);
  
  {Set the Buffer Values}
  FSize:=ASize;
  
  {Set the Data Values}
  FUsed:=FUsed;
  FFree:=FSize - FUsed;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.CheckIdle:Boolean;
{Check if all data in the send buffer has been sent and acknowledged}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: CheckIdle');
  {$ENDIF}
  
  {Check the Sequence}
  if (NextSequence = LastSequence) and (NextSequence = LastAcknowledge) then Result:=True;
 finally 
  ReleaseLock;
 end; 
end;
  
{==============================================================================}

function TTCPSendBuffer.SynAcknowledged:Boolean;
{Test if a SYN has been sent and if it has been ACKed}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: SynAcknowledged');
  {$ENDIF}
  
  {Check Syn Sequence}
  if SynSequence = 0 then Exit;
  
  {Check Syn Acknowledge}
  if SequenceLEQ(LastAcknowledge,SynSequence) then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.FinAcknowledged:Boolean;
{Test if a FIN has been sent and if it has been ACKed}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: FinAcknowledged');
  {$ENDIF}
  
  {Check Fin Sequence}
  if FinSequence = 0 then Exit;
  
  {Check Fin Acknowledge}
  if SequenceLEQ(LastAcknowledge,FinSequence) then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.TestAcknowledge(AAcknowledge:LongWord):Boolean;
{Tests an Acknowledge for invalidity using the method below - RFC793}
{If SEG.ACK =< ISS, or SEG.ACK > SND.NXT then the ACK is unacceptable}
{Note: Only ever used when Socket is in SYN_SENT state}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: TestAcknowledge');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  StartSequence = ' + IntToStr(StartSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  NextSequence  = ' + IntToStr(NextSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Acknowledge   = ' + IntToStr(AAcknowledge));
  {$ENDIF}
  
  {Check Acknowledge}
  if SequenceLEQ(AAcknowledge,StartSequence) or SequenceGT(AAcknowledge,NextSequence) then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.CheckAcknowledge(AAcknowledge:LongWord):Boolean;
{Checks an Acknowledge for validity using the method below - RFC793}
{If SND.UNA =< SEG.ACK =< SND.NXT then the ACK is acceptable}
{Note: Only ever used when Socket is in SYN_REC state}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: CheckAcknowledge');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  StartSequence = ' + IntToStr(StartSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  NextSequence  = ' + IntToStr(NextSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Acknowledge   = ' + IntToStr(AAcknowledge));
  {$ENDIF}
  
  {Check Acknowledge}
  if SequenceLT(AAcknowledge,LastAcknowledge) then Exit;
  
  {Check Sequence}
  if SequenceGT(AAcknowledge,NextSequence) then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.ValidateAcknowledge(AAcknowledge:LongWord):Boolean;
{Checks an Acknowledge for validity using the method below - RFC793}
{If the ACK acks something not yet sent (SEG.ACK > SND.NXT) then send an ACK}
{Note: Used when Socket is in any of the Synchronized states}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: ValidateAcknowledge');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  NextSequence  = ' + IntToStr(NextSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Acknowledge   = ' + IntToStr(AAcknowledge));
  {$ENDIF}
  
  {Check Sequence}
  if SequenceGT(AAcknowledge,NextSequence) then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.WriteData(var ABuffer;ASize,AFlags:Integer):Boolean;
{Used by Send/SendTo to Write data into the buffer ready for Sending}
{Data that will not fit in the Buffer is simply discarded}
{Flags is for MSG_PEEK and MSG_OOB when applicable}
{Note: Send coalescing is handled here by checking Last segment}
{Note: Push flag is set on the last written segment of user data}

{Socket Timing                                                                 }
{ Whenever data is written to the buffer a notification is sent to the socket  }
{ thread to check the socket immediately. This ensures that the segment is sent}
{ immediately if acceptable or otherwise will be sent when an acknowledge is   }
{ received from the remote due to the Nagle algorithm.                         }
var
 Offset:LongWord;
 Sequenced:Boolean;
 WriteNext:Pointer;
 WriteSize:LongWord;
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Clear old Segments}
  FlushSegments(False); 
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: WriteData');
  {$ENDIF}
  
  {Check the Data Size}
  if ASize = 0 then Exit; {Check for at least 1 byte of Data}
  
  {Check the Free Space}
  if FFree < 1 then Exit; {Check for at least 1 byte Free}
  
  {Set Sequenced}
  Sequenced:=False;
  
  {Check for Segment Size}
  Offset:=0;
  while ASize > 0 do
   begin
    {Get the Size to Write}
    WriteNext:=nil;
    WriteSize:=Min(MaxSeg,ASize);
    
    {Check Last Segment}
    Segment:=nil;
    if (AFlags and MSG_OOB) <> MSG_OOB then
     begin
      if FLast <> nil then
       begin
        {Check for Unsent and less than MSS}
        if (FLast.Count = 0) and ((FLast.Size + WriteSize) <= MaxSeg) then
         begin
          Segment:=FLast;
          WriteNext:=Pointer(LongWord(Segment.Data) + Segment.Size);
          Inc(Segment.Size,WriteSize);
          Inc(Segment.LastSequence,WriteSize);
          LastSequence:=Segment.LastSequence;  //To Do //Check this - Correct !!
         end;
       end;
      
      {Create a Segment}
      if Segment = nil then
       begin
        Segment:=AddSegment(LastSequence,0,WriteSize);
        if Segment = nil then Exit;
        LastSequence:=Segment.LastSequence;  //To Do //Check this - Correct !!
        WriteNext:=Segment.Data;
       end;
     end
    else
     begin
      {Create URG Segment} {Do not mix Urgent and Normal segments}
      Segment:=AddSegment(LastSequence,TCP_FLAG_URG,WriteSize);
      if Segment = nil then Exit;
      UrgentPointer:=Segment.LastSequence; //To Do //Check this
      LastSequence:=Segment.LastSequence;  //To Do //Check this
      WriteNext:=Segment.Data;
     end;
    
    {Write the Segment Data}
    System.Move(Pointer(LongWord(@ABuffer) + Offset)^,WriteNext^,WriteSize);
    
    {Update the Free and Used}
    Dec(FFree,WriteSize);
    Inc(FUsed,WriteSize);
    {$IFDEF TCP_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: WriteData: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed));
    {$ENDIF}
    
    {Update Segement}
    Segment.Transferred:=True;
    
    {Check Sequenced}
    if (Segment.Prev = nil) or (Segment.Prev.Count > 0) then Sequenced:=True;
    
    {Update Offset and Size}
    Inc(Offset,WriteSize);
    Dec(ASize,WriteSize);
    
    {Mark Push on the last Segment}
    if (ASize = 0) and not(NoPush) then Segment.Control:=Segment.Control or TCP_FLAG_PUSH;
   end;
  
  {Signal the Thread}
  if Sequenced then TProtocolSocket(FSocket).SendSocket;
  
  {Return Result} 
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.Finish:Boolean;
{Socket Timing                                                                 }
{ When the FIN segment is written to the buffer a notification is sent to the  }
{ socket thread to check the socket immediately. This ensures that the segment }
{ will be sent to the remote either immediately or after the Nagle delay.      }
var
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Finish');
  {$ENDIF}
  
  {Check already sent}
  if FinSequence <> 0 then Exit; {Could be a better test than <> 0}
  
  {Create FIN Segment}
  Segment:=AddSegment(LastSequence,TCP_FLAG_FIN,0);
  if Segment = nil then Exit;
  LastSequence:=Segment.LastSequence;
  FinSequence:=Segment.FirstSequence;
  
  {Update Segement}
  Segment.Transferred:=True;
  
  {Signal the Thread}  
  TProtocolSocket(FSocket).SendSocket;
  
  {Return Result} 
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.Synchronize:Boolean;
{Socket Timing                                                                 }
{ When the SYN segment is written to the buffer a notification is sent to the  }
{ socket thread to check the socket immediately. This ensures that the segment }
{ will be sent to the remote immediately.                                      }
var
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Synchronize');
  {$ENDIF}
  
  {Check already sent}
  if SynSequence <> 0 then Exit; {Could be a better test than <> 0}
  
  {Clear any Segments}
  FlushSegments(True);
  
  {Set Sequence numbers}
  StartSequence:=GetTickCount; //To Do //Should this be something else now ?
  NextSequence:=StartSequence;
  LastSequence:=StartSequence;
  LastAcknowledge:=StartSequence;
  
  {Create SYN Segment}
  Segment:=AddSegment(LastSequence,TCP_FLAG_SYN,0);
  if Segment = nil then Exit;
  LastSequence:=Segment.LastSequence;
  SynSequence:=Segment.FirstSequence;
  
  {Update Segement}
  Segment.Transferred:=True;
  
  {Signal the Thread}  
  TProtocolSocket(FSocket).SendSocket;
  
  {Return Result} 
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.ReadSegment(var ASequence:LongWord;var AUrgent:Word;var AFlags:Byte;var AData:Pointer;var ASize:Word;AForce:Boolean):Boolean;
{Used by Socket to Read segments to be (re)sent from queue}

{Note: Nagle Algorithm and Retry Timeouts are handled here}

{Socket Timing                                                                 }
{ For each sent segment a notification is scheduled for the socket thread to   }
{ check the socket in TCP_RETRY_TIMEOUT[Segment.Count] milliseconds. This is   }  
{ to ensure that the segment is retried if not acknowledged by that time.      }
{                                                                              }
{ For each sent segment that has a following segment a notification is sent to }
{ the socket thread to check the socket immediately. This ensures that all     }
{ acceptable segments are sent immediately.                                    }
{                                                                              }
{ For each retried segment a notification is also scheduled for the socket     }
{ thread to check the socket in TCP_RETRY_TIMEOUT[Segment.Count] milliseconds  }
{ which will be the next retry time if still not acknowledged.                 }
{                                                                              }
{ If the remote Window is not large enough to send the next segment then a     }
{ notification is scheduled for the socket thread to check the socket in       }
{ TCP_WINDOW_TIMEOUT milliseconds to ensure that a window probe is sent at the }
{ appropriate time.                                                            }
var
 CurrentTime:Int64;
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Clear old Segments}
  {FlushSegments(False);}
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: ReadSegment');
  {$ENDIF}
  
  {Get the Defaults}
  ASequence:=NextSequence;
  AUrgent:=UrgentPointer;
  AFlags:=0;
  AData:=nil;
  ASize:=0;
  
  {Check the Sequence}
  if (NextSequence = LastSequence) and (NextSequence = LastAcknowledge) then Exit;
  
  {Check Segments}
  CurrentTime:=GetTickCount64;
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Check for Sent}
    if Segment.Count = 0 then
     begin
      {Check for available Window}
      if SequenceLT(Segment.LastSequence,(LastAcknowledge + WindowSize)) then
       begin
        {Check for acceptable Send} {This is the Nagle Algortihm}
        if (NoNagle) or (AForce) or (NextSequence = LastAcknowledge) or (Segment.Size >= MaxSeg) then
         begin
          {Send if unsent}
          ASequence:=Segment.FirstSequence;
          AUrgent:=0; //To Do //UrgentPointer  ?? UrgentPointer - Segment.FirstSequence ?? //or just LastSequence or Size ??
          AFlags:=Segment.Control;
          AData:=Segment.Data;
          ASize:=Segment.Size;
          
          {Stamp the Segment}
          Inc(Segment.Count);
          Segment.Timeout:=CurrentTime;
          
          {Move the NextSequence}
          NextSequence:=Segment.LastSequence;
          
          {Reset the Window Timeout}
          WindowTimeout:=0;
                    
          {Schedule the Thread}  
          TProtocolSocket(FSocket).ScheduleSocketItem(@Segment.Item,TCP_RETRY_TIMEOUT[Segment.Count]);
          
          {Signal the Thread}  
          if Segment.Next <> nil then TProtocolSocket(FSocket).SendSocket;
          
          {$IFDEF TCP_DEBUG}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Acceptable Segment');
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Segment.Size = ' + IntToStr(Segment.Size));
          if Segment.Next = nil then
           begin
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: No following segment');
           end;
          {$ENDIF}
          
          {Return Result}
          Result:=True;
          
        {$IFDEF TCP_DEBUG}
         end
        else
         begin
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Unacceptable Segment');  
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  NextSequence = ' + IntToStr(NextSequence));
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  LastAcknowledge = ' + IntToStr(LastAcknowledge)); 
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Segment.Size = ' + IntToStr(Segment.Size));
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  MaxSeg = ' + IntToStr(MaxSeg)); 
        {$ENDIF}  
         end;         
       end
      else
       begin
        {$IFDEF TCP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: No available window'); 
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  WindowSize = ' + IntToStr(WindowSize)); 
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  WindowScale = ' + IntToStr(WindowScale)); 
        {$ENDIF}  
        
        {Not enough Window available}
        if WindowTimeout <> 0 then
         begin
          {Check the Window Timeout}
          if (WindowTimeout + (TCP_WINDOW_TIMEOUT - TCP_TIMEOUT_OFFSET)) < CurrentTime then
           begin
            {$IFDEF TCP_DEBUG}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Sending window probe');
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  NextSequence = ' + IntToStr(NextSequence));
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Segment.Size = ' + IntToStr(Segment.Size));
            {$ENDIF}  
            
            {Send a keep alive to probe the Window (Instead of a 1 byte probe segment)}
            ASequence:=NextSequence - 1;
            AUrgent:=UrgentPointer;
            AFlags:=0;
            AData:=nil; {Zero length keep alive}
            ASize:=0;
            
            {Do not update segment or next sequence}
            
            {Set the Window Timeout}
            WindowTimeout:=CurrentTime;
            
            {Schedule the Thread}  
            TProtocolSocket(FSocket).ScheduleSocket(TCP_WINDOW_TIMEOUT);
            
            {Return Result}
            Result:=True;
           end;
         end
        else
         begin
          {$IFDEF TCP_DEBUG}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Scheduling window timeout'); 
          {$ENDIF}  
          
          {Set the Window Timeout}
          WindowTimeout:=CurrentTime;
          
          {Schedule the Thread}  
          TProtocolSocket(FSocket).ScheduleSocket(TCP_WINDOW_TIMEOUT);
         end;
       end;
       
      {Exit if we found an unsent Segment (even if we didnt send it)}
      Exit;
     end
    else
     begin
      {Check for Acknowledged}
      if not Segment.Acknowledged then
       begin
        {Check for Resend}
        if (Segment.Timeout + (Min(TCP_MAX_LIFETIME,TCP_RETRY_TIMEOUT[Segment.Count]) - TCP_TIMEOUT_OFFSET)) < CurrentTime then
         begin
          {Resend if timeout}
          ASequence:=Segment.FirstSequence;
          AUrgent:=0; //To Do //UrgentPointer  ?? UrgentPointer - Segment.FirstSequence ??
          AFlags:=Segment.Control;
          AData:=Segment.Data;
          ASize:=Segment.Size; 

          {Resent Count exceeded close the Socket}
          if Segment.Count >= TCP_RETRY_COUNT then
           begin
            {Connection Timeout}
            FSocket.SocketError:=WSAETIMEDOUT;
            FSocket.SocketState.Closed:=True;
            FSocket.CloseTime:=GetTickCount64;
            TTCPState(TTCPSocket(FSocket).ProtocolState).State:=TCP_STATE_CLOSED;
            
            {Signal the Event}
            TProtocolSocket(FSocket).SignalChange; 
            
            Exit;
           end; 

          {Restamp the Segment}
          Inc(Segment.Count);
          Segment.Timeout:=CurrentTime;
          
          {Schedule the Thread}
          TProtocolSocket(FSocket).ScheduleSocketItem(@Segment.Item,TCP_RETRY_TIMEOUT[Segment.Count]);
          
          {Signal the Thread}  
          if (Segment.Next <> nil) and (Segment.Next.Count = 0) then TProtocolSocket(FSocket).SendSocket;
          
          {Return Result}
          Result:=True;
          Exit;
         end;
       end;
     end;
    
    {Get Next}
    Segment:=Segment.Next;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.AcknowledgeSegments(ASequence,AAcknowledge:LongWord;AWindow:Word):Boolean;
{Used by Socket to Acknowledge segments that have been sent and update the Window}
{Sequence, Acknowledge and Window are from the received Segment}

{Socket Timing                                                                 }
{ For each acknowledged segment a notification is sent to the socket thread to }
{ check the socket immediately. This is to ensure that segments waiting to be  }
{ sent due to the Nagle algorithm are sent on receipt of an acknowledge.       }
var
 Segment:PTCPSegment;
 Acknowledged:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Clear old Segments}
  {FlushSegments(False);}
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: AcknowledgeSegments');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Sequence = ' + IntToStr(ASequence));  
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Acknowledge = ' + IntToStr(AAcknowledge));  
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Window = ' + IntToStr(AWindow)); 
  {$ENDIF}
  
  {Check the Acknowledge}
  if SequenceGT(AAcknowledge,NextSequence) then Exit;
  
  {Check for Duplicate}
  if SequenceGT(AAcknowledge,LastAcknowledge) then
   begin
    {Set Acknowledged}
    Acknowledged:=False;
    
    {Check Segments}
    Segment:=FFirst;
    while Segment <> nil do
     begin
      {Check Acknowledged}
      if not Segment.Acknowledged then
       begin
        if SequenceGEQ(AAcknowledge,Segment.LastSequence) then
         begin
          {Unschedule the Thread}
          TProtocolSocket(FSocket).UnscheduleSocketItem(@Segment.Item);

          {Update Segment}
          Segment.Acknowledged:=True;
          
          {Update the Free and Used}
          Inc(FFree,Segment.Size);
          Dec(FUsed,Segment.Size);
          
          {Set Acknowledged}
          Acknowledged:=True;
          
          {$IFDEF TCP_DEBUG}
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Acknowledge Segment'); 
          if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  Segment.LastSequence = ' + IntToStr(Segment.LastSequence));
          {$ENDIF}
         end
        else
         begin
          {Stop if we found a Segment after this Acknowledge}
          Break;
         end;
       end;
      
      {Get Next}
      Segment:=Segment.Next;
     end;
    
    {Signal the Thread}
    if Acknowledged and not(NoNagle) then TProtocolSocket(FSocket).SendSocket;
    
    {Move the LastAcknowledge}
    LastAcknowledge:=AAcknowledge;
   end;
    
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: Update Window'); 
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  WindowSequence = ' + IntToStr(WindowSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer:  WindowAcknowledge = ' + IntToStr(WindowAcknowledge)); 
  {$ENDIF}
  
  {Update the Window}
  if SequenceLEQ(WindowSequence,ASequence) and SequenceLEQ(WindowAcknowledge,AAcknowledge) then
   begin
    WindowSequence:=ASequence;
    WindowAcknowledge:=AAcknowledge;
    WindowSize:=(AWindow shl WindowScale);
    
    {Signal the Thread}
    if WindowTimeout <> 0 then TProtocolSocket(FSocket).SendSocket;
   end;
   
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.TimestampSegment(AOptions:Pointer;var AOffset:Word):Boolean;
{Used by Socket to Timestamp (RTT) segments that have been sent}
{Offset points to the point to extract the timestamp option}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: TimestampSegment');
  {$ENDIF}
  
  {Check Options}
  if AOptions = nil then Exit;
  
  //To Do
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPSendBuffer.SelectiveAckSegments(AOptions:Pointer;var AOffset:Word;ASize:Byte):Boolean;
{Used by Socket to Selective Ack segments that have been sent}
{Offset points to the point to start extracting SACKS, Size includes the Option and Size}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Send Buffer: SelectiveAckSegments');
  {$ENDIF}
  
  {Check Options}
  if AOptions = nil then Exit;
  
  {Check Selective Ack (Sack)}
  if NoSack then Exit;
  
  //To do
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TTCPRecvBuffer}
constructor TTCPRecvBuffer.Create;
begin
 {}
 inherited Create(ASocket);
 FUrgent:=0;
 FAvailable:=0;

 FFirst:=nil;
 FLast:=nil;

 StartSequence:=0;
 NextSequence:=0;
 LastSequence:=0;
 LastAcknowledge:=0;

 UrgentPointer:=0;

 MaxSeg:=TCP_MAX_MSS;

 WindowSize:=TCP_WINDOW_SIZE;
 WindowScale:=0;
 LastWindow:=WindowSize;

 SynSequence:=0;
 FinSequence:=0;

 NoSack:=True; {Disable Selective Ack by Default}
end;

{==============================================================================}

destructor TTCPRecvBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FlushSegments(True);
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.DelayOverride(ASegment:PTCPSegment):Boolean;
{Checks if this segment can override delayed ack because it has an in sequence segment following it}
{Delayed ack must send an ack at least every two segments}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check Segment}
  if ASegment = nil then Exit;
  
  {Check for following segment}
  if ASegment.Next = nil then Exit;
  
  {Check if following segment is next in sequence}
  if SequenceLT(ASegment.LastSequence,ASegment.Next.FirstSequence) then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.GetSegment(ASequence:LongWord;ALength:Word):PTCPSegment;
{Returns the first Segment that contains ALL of the passed Sequence}
{Sequence is the start sequence, Length includes the control bits}

{Matching Conditions                                                           }
{  Segment     |------|                                                        }
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{1  Match      . |--| .    First <= Seqeuence and Last >= Sequence + Length    }
{2  Match      |------|    First <= Seqeuence and Last >= Sequence + Length    }
{3  No Match |----------|                                                      }
{4  No Match   . |------|                                                      }
{5  No Match |------| .                                                        }
{6  No Match   |----| .                                                        }
{7  No Match   . |----|                                                        }
var
 Segment:PTCPSegment;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: GetSegment');
  {$ENDIF}

  {Check Length}
  if ALength = 0 then Exit;
  
  {Get Segment}
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Check Seqeuence}
    if SequenceLEQ(Segment.FirstSequence,ASequence) and SequenceLEQ((ASequence + ALength),Segment.LastSequence) then
     begin
      {Return Result}
      Result:=Segment;
      Exit;
     end;
    
    {Get Next}
    Segment:=Segment.Next;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.GetPrevious(ASequence:LongWord;ALength:Word):PTCPSegment;
{Finds the first Segment that is AFTER the passed Sequence and returns the previous Segment}
{Sequence is the start sequence, Length includes the control bits}
{If no Segment is after the passed Sequence returns the last Segment}

{Matching Conditions                                                           }
{  Segment          |------|                                                   }
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{1  Match        |--|      .       Sequence + Length <= First                  }
{2  Match      |--| .      .       Sequence + Length <= First                  }
{3  No Match   |-------|   .                                                   }
{4  No Match       |-------|                                                   }
{5  No Match        .  |-------|                                               }
{6  No Match        .      |-------|                                           }
var
 Segment:PTCPSegment;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: GetPrevious');
  {$ENDIF}

  {Check Length}
  if ALength = 0 then Exit;

  {Get Segment}
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Check Sequence}
    if SequenceLEQ((ASequence + ALength),Segment.FirstSequence) then
     begin
      {Return Result}
      Result:=Segment.Prev;
      Exit;
     end
    else if Segment.Next = nil then
     begin
      {Return Result}
      Result:=Segment;
      Exit;
     end;
     
    {Get Next} 
    Segment:=Segment.Next;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.GetOverlapped(ASequence:LongWord;ALength:Word):PTCPSegment;
{Returns the first Segment that overlaps ANY of the passed Sequence}
{Sequence is the start sequence, Length includes the control bits}

{Matching Conditions                                                           }
{  Segment     |------|                                                        }
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{1  Match    |----------|  Seqeuence <= First and Sequence + Length >= Last    }
{2  Match    |------| . Sequence + Length > First and Sequence + Length <= Last}
{3  Match      . |------|       Sequence >= First and Sequence < Last          }
{------------------------------------------------------------------------------}
{4  No Match   . |--| .                Matched by GetSegment                   }
{5  Match      |------|         Matched by GetSegment (Also Matches 1)         }
{6  Match    |--------|                    Matched by 1                        }
{7  Match      |--------|                  Matched by 1                        }
{8  Match      |----| .             Matched by 2 (Also Matches 3)              }
{9  Match      . |----|             Matched by 3 (Also Matches 2)              }
{10 No Match   .      |------|                                                 }
{11 No Match   .      .  |------|                                              }
var
 Segment:PTCPSegment;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: GetOverlapped');
  {$ENDIF}

  {Check Length}
  if ALength = 0 then Exit;
  
  {Get Segment}
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Check Sequence}
    if SequenceLEQ(ASequence,Segment.FirstSequence) and SequenceGEQ((ASequence + ALength),Segment.LastSequence) then
     begin
      {Overlaps the entire Segment}
      Result:=Segment;
      Exit;
     end
    else if SequenceGT((ASequence + ALength),Segment.FirstSequence) and SequenceLEQ((ASequence + ALength),Segment.LastSequence) then
     begin
      {Overlaps the left hand edge of the Segment}
      Result:=Segment;
      Exit;
     end
    else if SequenceGEQ(ASequence,Segment.FirstSequence) and SequenceLT(ASequence,Segment.LastSequence) then
     begin
      {Overlaps the right hand edge of the Segment}
      Result:=Segment;
      Exit;
     end;
     
    {Get Next} 
    Segment:=Segment.Next;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.AddSegment(APrev:PTCPSegment;ASequence:LongWord;AFlags:Byte;ASize:Word):PTCPSegment;
{Adds a new Segment after the previous segment or first in the list}
{Prev is the Previous segment, Sequence is the start sequence, Size can be zero}
{Size should not include the SYN and FIN sequence}
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: AddSegment');
  {$ENDIF}
  
  {Check Buffer Free}
  if ASize > FFree then Exit;
  
  {Create a new Segment and Data}
  Result:=GetMem(SizeOf(TTCPSegment) + TCP_MAX_MSS); {TCP_SEGMENT_SIZE}
  if Result = nil then Exit;
  
  {Update Segment}
  Result.Size:=ASize;
  Result.Data:=Pointer(LongWord(Result) + SizeOf(TTCPSegment)); {TCP_SEGMENT_SIZE}
  Result.FirstSequence:=ASequence;
  Result.LastSequence:=ASequence + ASize;
  Result.Control:=AFlags;
  Result.Transferred:=False;
  Result.Acknowledged:=False;
  Result.SelectiveAck:=False;
  Result.RoundTripTime:=0;
  Result.Count:=0;
  Result.Timeout:=0;
  Result.Prev:=nil;
  Result.Next:=nil;
  Result.Item.Flags:=SOCKET_TIMER_FLAG_NONE;
  
  {Check Flags}
  if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Inc(Result.LastSequence);
  if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then Inc(Result.LastSequence);
  
  {Add to List}
  if APrev = nil then
   begin
    if FFirst = nil then
     begin
      {Is First Segment}
      FFirst:=Result;
      FLast:=Result;
     end
    else
     begin
      {Not First Segment}
      FFirst.Prev:=Result;
      Result.Next:=FFirst;
      FFirst:=Result;
     end;
   end
  else
   begin
    if APrev.Next = nil then
     begin
      {Is Last Segment}
      APrev.Next:=Result;
      Result.Prev:=APrev;
      FLast:=Result;
     end
    else
     begin
      {Not Last Segment}
      APrev.Next.Prev:=Result;
      Result.Next:=APrev.Next;
      APrev.Next:=Result;
      Result.Prev:=APrev;
     end;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.RemoveSegment(ASegment:PTCPSegment):Boolean;
{Removes the passed Segment from the list}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: RemoveSegment');
  {$ENDIF}
  
  {Check Segment}
  if ASegment = nil then Exit;
  
  {Remove from List}
  if ASegment.Prev <> nil then
   begin
    {Not First Segment}
    if ASegment.Next <> nil then
     begin
      {Not Last Segment}
      ASegment.Prev.Next:=ASegment.Next;
      ASegment.Next.Prev:=ASegment.Prev;
     end
    else
     begin
      {Is Last Segment}
      ASegment.Prev.Next:=nil;
      FLast:=ASegment.Prev;
     end;
   end
  else
   begin
    {Is First Segment}
    if ASegment.Next <> nil then
     begin
      {Not Last Segment}
      ASegment.Next.Prev:=nil;
      FFirst:=ASegment.Next;
     end
    else
     begin
      {Is Last Segment}
      FFirst:=nil;
      FLast:=nil;
     end;
   end;
   
  {Unschedule the Thread} 
  TProtocolSocket(FSocket).UnscheduleSocketItem(@ASegment.Item);
   
  {Free the Segment and Data}
  FreeMem(ASegment,SizeOf(TTCPSegment) + TCP_MAX_MSS); {TCP_SEGMENT_SIZE}
  
  {Return Result}
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TTCPRecvBuffer.FlushSegments(All:Boolean);
{Removes Read Segments from the buffer (or All)}
var
 Segment:PTCPSegment;
 Current:PTCPSegment;
begin
 {}
 if not AcquireLock then Exit;
 try
  {Get Segment}
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Get Next}
    Current:=Segment;
    Segment:=Current.Next;
    
    {Check Status}
    if ((Current.Acknowledged and Current.Transferred)) or (All) then
     begin
      {Remove Segment}
      RemoveSegment(Current);
     end
    else
     begin
      {Exit if we found an Unacknowledged or Unread segment}
      Exit;
     end;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TTCPRecvBuffer.SetSize(ASize:LongWord);
{For TCP setting the size does not clear existing segments}
{Size cannot be set smaller than the default or existing}
begin
 {}
 if not AcquireLock then Exit;
 try
  {Check Size}
  if ASize = 0 then Exit;
  
  {Get Size}
  ASize:=Max(ASize,TCP_BUFFER_SIZE);
  ASize:=Max(FSize,ASize);
  
  {Set the Buffer Values}
  FSize:=ASize;
  
  {Set the Data Values}
  FUsed:=FUsed;
  FFree:=FSize - FUsed;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.CheckIdle:Boolean;
{Check if all data in the recv buffer has been acknowledged}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: CheckIdle');
  {$ENDIF}
  
  {Check the Sequence}
  if (NextSequence = LastSequence) and (NextSequence = LastAcknowledge) then Result:=True;
 finally 
  ReleaseLock;
 end; 
end;
  
{==============================================================================}

function TTCPRecvBuffer.GetUrgent:LongWord;
begin
 {}
 Result:=FUrgent;
end;

{==============================================================================}

function TTCPRecvBuffer.GetAvailable:LongWord;
begin
 {}
 Result:=FAvailable;
end;

{==============================================================================}

function TTCPRecvBuffer.SynReceived:Boolean;
{Checks if a SYN has been received from the Remote}
{Used by ????}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: SynReceived');
 {$ENDIF}
  
 {Check Syn Sequence}
 Result:=SynSequence <> 0; {Could be a better test than <> 0}

 ReleaseLock;
end;

{==============================================================================}

function TTCPRecvBuffer.FinReceived:Boolean;
{Checks if a FIN has been received from the Remote}
{Used by ????}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: FinReceived');
 {$ENDIF}
  
 {Check Fin Seqeuence}
 Result:=FinSequence <> 0; {Could be a better test than <> 0}

 ReleaseLock;
end;

{==============================================================================}

function TTCPRecvBuffer.CheckSequence(ASequence:LongWord;ASize:Word):Boolean;
{Checks a Sequence for validity using the method below - RFC793}
{Segment Receive  Test
 Length  Window
 ------- -------  -------------------------------------------
    0       0     SEG.SEQ = RCV.NXT
    0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND
   >0       0     not acceptable
   >0      >0     RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND or RCV.NXT =< SEG.SEQ+SEG.LEN-1 < RCV.NXT+RCV.WND}
   
//To Do //this may need to be changed to account for SYN/FIN in Sequence space
              //since the passed ASize is only the Data size
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: CheckSequence');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  NextSequence  = ' + IntToStr(NextSequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  WindowSize    = ' + IntToStr(WindowSize));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  Sequence      = ' + IntToStr(ASequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  Size          = ' + IntToStr(ASize));
  {$ENDIF}
  
  {Default True}
  Result:=True;
  
  {Check Size}
  if ASize = 0 then
   begin
    {Check Window Size}
    if WindowSize = 0 then
     begin
      {Check Sequence} {SEG.SEQ = RCV.NXT}
      if ASequence = NextSequence then Exit;
     end
    else
     begin
      {Check Sequence} {RCV.NXT =< SEG.SEQ < RCV.NXT + RCV.WND}
      if SequenceLEQ(NextSequence,ASequence) and SequenceLT(ASequence,(NextSequence + WindowSize)) then Exit;
     end;
   end
  else
   begin
    {Check Window Size}
    if WindowSize = 0 then
     begin
      {Not Acceptable}
     end
    else
     begin
      {Check Sequence} {RCV.NXT =< SEG.SEQ < RCV.NXT+RCV.WND or RCV.NXT =< SEG.SEQ+SEG.LEN-1 < RCV.NXT+RCV.WND}
      if (SequenceLEQ(NextSequence,ASequence) and SequenceLT(ASequence,(NextSequence + WindowSize))) or (SequenceLEQ(NextSequence,(ASequence + ASize - 1)) and SequenceLT((ASequence + ASize - 1),(NextSequence + WindowSize))) then Exit;
     end;
   end;
   
  {Return Result} 
  Result:=False;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.ReadData(var ABuffer;var ASize:Integer;AFlags:Integer):Boolean;
{Used by Recv/RecvFrom to Read data from the buffer that has been Received}
{Passed Size contains size of Buffer and should return the size of Datagram}
{Flags is for MSG_PEEK and MSG_OOB when applicable}
var
 Offset:LongWord;
 ReadNext:Pointer;
 ReadSize:LongWord;
 BufferSize:LongWord;
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  //To Do //Handling of OOB
  
  {Clear old Segments}
  FlushSegments(False);
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: ReadData');
  {$ENDIF}
  
  {Check Buffer Size}
  if ASize = 0 then Exit;
  
  {Check there is Data}
  if FAvailable < 1 then Exit;
  
  {Get the Read Size}
  BufferSize:=Min(ASize,FAvailable);
  
  {Check for Segments}
  ASize:=0;
  Offset:=0;
  Segment:=FFirst;
  while Segment <> nil do
   begin
    {Check the Sequence}
    if SequenceLEQ(Segment.LastSequence,NextSequence) then
     begin
      {Check for Read}
      if not Segment.Transferred then
       begin
        {Check for Zero Size}
        if Segment.Size > 0 then
         begin
          {Get the Start and Size}
          ReadNext:=Segment.Data;
          ReadSize:=Segment.Size;
          
          {Read the Data}
          if BufferSize < ReadSize then
           begin
            {$IFDEF TCP_DEBUG}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: ReadData: Read Partial Segment');
            {$ENDIF}
            
            System.Move(ReadNext^,Pointer(LongWord(@ABuffer) + Offset)^,BufferSize);
            
            {Update the Return Size}
            Inc(ASize,BufferSize);
            
            {Check for Peek Flag}
            if (AFlags and MSG_PEEK) = 0 then
             begin
              {Update Available}
              Dec(FAvailable,BufferSize);
              
              {Update Free and Used}
              Inc(FFree,BufferSize);
              Dec(FUsed,BufferSize);
              
              {Shrink Segment}
              Segment.Control:=Segment.Control and not(TCP_FLAG_SYN);
              Inc(PtrUInt(Segment.Data),BufferSize);
              Dec(Segment.Size,BufferSize);
              
              {Dont mark as Read as there is more data}
             end;
            
            {Update Offset and Size}
            Inc(Offset,BufferSize);
            Dec(BufferSize,BufferSize);
           end
          else
           begin
            {$IFDEF TCP_DEBUG}
            if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: ReadData: Read Full Segment');
            {$ENDIF}
            
            System.Move(ReadNext^,Pointer(LongWord(@ABuffer) + Offset)^,ReadSize);
            
            {Update the Return Size}
            Inc(ASize,ReadSize);
            
            {Check for Peek Flag}
            if (AFlags and MSG_PEEK) = 0 then
             begin
              {Update Available}
              Dec(FAvailable,ReadSize);
              
              {Update Free and Used}
              Inc(FFree,ReadSize);
              Dec(FUsed,ReadSize);
              
              {Mark as Read}
              Segment.Transferred:=True;
             end;
            
            {Update Offset and Size}
            Inc(Offset,ReadSize);
            Dec(BufferSize,ReadSize);
           end;
         end
        else
         begin
          {Check for Peek Flag}
          if (AFlags and MSG_PEEK) = 0 then
           begin
            {Mark as Read}
            Segment.Transferred:=True;
           end;
         end;
        
        {$IFDEF TCP_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: ReadData: Free = ' + IntToStr(FFree) + ' Used = ' + IntToStr(FUsed));
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: ReadData: Available = ' + IntToStr(FAvailable));
        {$ENDIF}
        
        {Return Result}
        Result:=True;
        
        {Check Buffer Size}
        if BufferSize < 1 then Exit;
       end;
     end
    else
     begin
      {Exit if we found an out of order Segment}
      Exit;
     end;
    
    {Get Next}
    Segment:=Segment.Next;
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.WriteSegment(ASequence:LongWord;AUrgent:Word;AFlags:Byte;AData:Pointer;ASize:Word):Boolean;
{Used by Socket to Write segments that are received into the queue}

{Overlap Handling                                                              }
{  Segment     |------|                                                        }
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{1  Entire   |----------|  Seqeuence <= First and Sequence + Length >= Last    }
{2  Left       . |------|   Sequence > First and Sequence + Length > Last      }
{3  Right    |------| .     Sequence < First and Sequence + Length < Last      }
{4  Within     . |--| .    Seqeuence >= First and Sequence + Length <= Last    }
{------------------------------------------------------------------------------}
{5  Entire   |--------|                   Matched by 1                         }
{6  Entire     |--------|                 Matched by 1                         }
{7  Within     |----| .                   Matched by 4                         }
{8  Within     . |----|                   Matched by 4                         }
{9  Entire     |------|         Matched by GetSegment (Also Matches 1)         }

{Socket Timing                                                                 }
{ For each received segment a notification is scheduled for the socket thread  }
{ to check the socket in TCP_ACK_TIMEOUT milliseconds. This is the maximum to  }
{ before the received segment must be acknowledged, but it may be acknowledged }
{ earlier if a segment is sent or if there are earlier received segments to be }
{ acknowledged as well.                                                        }
var
 Length:Word;         {SEG.LEN (Size plus SYN/FIN)}
 Offset:LongWord;
 Sequenced:Boolean;
 Current:PTCPSegment;
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Clear old Segments}
  {FlushSegments(False);}
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: WriteSegment');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  Sequence = ' + IntToStr(ASequence));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  Urgent   = ' + IntToStr(AUrgent));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  Size     = ' + IntToStr(ASize));
  {$ENDIF}
  
  {Check the Size}
  if ASize > FFree then Exit;
  
  {Check Start Sequence (Zero when in SYNSENT State)}
  if StartSequence = 0 then
   begin
    StartSequence:=ASequence;
    NextSequence:=ASequence;
    LastSequence:=ASequence;
    LastAcknowledge:=ASequence;
   end;
  
  {Get the Length}
  Offset:=0;
  Length:=ASize;
  if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Inc(Length);
  if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then Inc(Length);
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer:  Length   = ' + IntToStr(Length));
  {$ENDIF}
  
  {Check the Sequence}
  //To Do //Put this simply after StartSequence check
  //To Do //What to Check (Sequence + Size (Length !!) is in Window) ??
  //Already done by CheckSequence really ?? //See RFC 793 Page 24
  
  {Trim before NextSequence}
  if SequenceLT(ASequence,NextSequence) then
   begin
    {Adjust Sequence, Size and Length}
    Dec(Length,NextSequence - ASequence);
    Dec(ASize,NextSequence - ASequence);
    Inc(Offset,NextSequence - ASequence);
    ASequence:=NextSequence;
    
    {Turn off SYN}
    if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Inc(ASize);
    if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Dec(Offset);
    AFlags:=AFlags and not(TCP_FLAG_SYN);
   end;
   
  {Trim after WindowSize}
  if SequenceGT((ASequence + Length),(NextSequence + WindowSize)) then
   begin
    {Adjust Size and Length}
    Length:=(NextSequence + WindowSize) - ASequence;
    ASize:=(NextSequence + WindowSize) - ASequence;
    
    {Turn off FIN}
    AFlags:=AFlags and not(TCP_FLAG_FIN);
   end;
  
  {Check for zero Length}
  if Length > 0 then
   begin
    {Set Sequenced}
    Sequenced:=False;
    
    {Check for Duplicated Segment} {Done here to save overlap checks}
    if GetSegment(ASequence,Length) = nil then
     begin
      {Add the Segment}
      if SequenceLEQ(ASequence,NextSequence) and SequenceGT((ASequence + Length),NextSequence) then
       begin
        {Sequence is on the left hand edge of the Window} 
        
        {Check for Overlapped Segment}
        Current:=GetOverlapped(ASequence,Length);
        while Current <> nil do
         begin
          {Note: No need to check for exact Duplicate by GetSegment, first test will match anyway}
          {Note: The checks here vary from GetOverlapped as we already know the Segment overlaps}
          {Check for Before or After}
          if SequenceLEQ(ASequence,Current.FirstSequence) and SequenceGEQ((ASequence + Length),Current.LastSequence) then
           begin
            {Overlaps the entire Segment}
            
            {Update Free and Used}
            Inc(FFree,Current.Size);
            Dec(FUsed,Current.Size);
            
            {Remove Existing Segment}
            RemoveSegment(Current);
           end
          else if SequenceGT(ASequence,Current.FirstSequence) and SequenceGT((ASequence + Length),Current.LastSequence) then
           begin
            {Overlaps at the left hand edge}
            
            {Adjust Sequence, Size and Length}
            Dec(Length,Current.LastSequence - ASequence);
            Dec(ASize,Current.LastSequence - ASequence);
            Inc(Offset,Current.LastSequence - ASequence);
            ASequence:=Current.LastSequence;
            
            {Turn off SYN}
            if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Inc(ASize);
            if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Dec(Offset);
            AFlags:=AFlags and not(TCP_FLAG_SYN);
           end
          else if SequenceLT(ASequence,Current.FirstSequence) and SequenceLT((ASequence + Length),Current.LastSequence) then
           begin
            {Overlaps at the right hand edge}
            
            {Adjust Size and Length}
            Length:=Current.FirstSequence - ASequence;
            ASize:=Current.FirstSequence - ASequence;
            
            {Turn off FIN}
            AFlags:=AFlags and not(TCP_FLAG_FIN);
           end
          else {SequenceGEQ(ASequence,Current.FirstSequence) and SequenceLEQ((ASequence + Length),Current.LastSequence)}
           begin
            {Overlaps within the Segment (Duplicate)}
            Length:=0;
            ASize:=0;
            Offset:=0;
            
            {Turn off SYN}
            AFlags:=AFlags and not(TCP_FLAG_SYN);
            
            {Turn off FIN}
            AFlags:=AFlags and not(TCP_FLAG_FIN);
            
            {Discard the Segment and Return True}
            Break;
           end;
           
          {Get Next} 
          Current:=GetOverlapped(ASequence,Length);
         end; 

        {Check for zero Length}
        if Length > 0 then
         begin
          {Get the Previous Segment}
          Current:=GetPrevious(ASequence,Length);
          
          {Check Sequenced}
          if (Current <> nil) and not(Current.Acknowledged) then Sequenced:=True;
           
          {Create a Segment}
          Segment:=AddSegment(Current,ASequence,AFlags,ASize);
          if Segment = nil then Exit;
          
          {Update the Last Sequence}
          if SequenceGT(Segment.LastSequence,LastSequence) then
           begin
            LastSequence:=Segment.LastSequence;
           end;
          
          {Write the Segment Data}
          if ASize > 0 then
           begin
            System.Move(Pointer(LongWord(AData) + Offset)^,Segment.Data^,ASize);
           end;
          
          {Stamp the Segment}
          Segment.Timeout:=GetTickCount64;
          
          {Move the Next Sequence}
          NextSequence:=Segment.LastSequence;
          
          {Update Free and Used}
          Dec(FFree,Segment.Size);
          Inc(FUsed,Segment.Size);
          
          {Update Available}
          Inc(FAvailable,Segment.Size);
          
          {Check Queued Segments}
          Current:=Segment.Next;
          while Current <> nil do
           begin
            if Current.FirstSequence = NextSequence then
             begin
              {Move the Next Sequence}
              NextSequence:=Current.LastSequence;
              
              {Update Available}
              Inc(FAvailable,Current.Size);
              
              {Set Sequenced}
              Sequenced:=True;
             end
            else
             begin
              {Stop if we found an out of order Segment}
              Break;
             end;
             
            {Get Next} 
            Current:=Current.Next;
           end;
           
          if Sequenced then
           begin
            {Signal the Thread}
            TProtocolSocket(FSocket).SendSocket;
           end
          else
           begin
            {Schedule the Thread} 
            TProtocolSocket(FSocket).ScheduleSocketItem(@Segment.Item,TCP_ACK_TIMEOUT);
           end;           
         end;
       end
      else
       begin
        {Sequence is in the Window but not on the left hand edge} 
        
        {Check for Overlapped Segment}
        Current:=GetOverlapped(ASequence,Length);
        while Current <> nil do
         begin
          {Note: No need to check for exact Duplicate by GetSegment, first test will match anyway}
          {Note: The checks here vary from GetOverlapped as we already know the Segment overlaps}
          {Check for Before or After}
          if SequenceLEQ(ASequence,Current.FirstSequence) and SequenceGEQ((ASequence + Length),Current.LastSequence) then
           begin
            {Overlaps the entire Segment}
            
            {Update Free and Used}
            Inc(FFree,Current.Size);
            Dec(FUsed,Current.Size);
            
            {Remove Existing Segment}
            RemoveSegment(Current);
           end
          else if SequenceGT(ASequence,Current.FirstSequence) and SequenceGT((ASequence + Length),Current.LastSequence) then
           begin
            {Overlaps at the left hand edge}
            
            {Adjust Sequence, Size and Length}
            Dec(Length,Current.LastSequence - ASequence);
            Dec(ASize,Current.LastSequence - ASequence);
            Inc(Offset,Current.LastSequence - ASequence);
            ASequence:=Current.LastSequence;
            
            {Turn off SYN}
            if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Inc(ASize);
            if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then Dec(Offset);
            AFlags:=AFlags and not(TCP_FLAG_SYN);
           end
          else if SequenceLT(ASequence,Current.FirstSequence) and SequenceLT((ASequence + Length),Current.LastSequence) then
           begin
            {Overlaps at the right hand edge}
            
            {Adjust Size and Length}
            Length:=Current.FirstSequence - ASequence;
            ASize:=Current.FirstSequence - ASequence;
            
            {Turn off FIN}
            AFlags:=AFlags and not(TCP_FLAG_FIN);
           end
          else {SequenceGEQ(ASequence,Current.FirstSequence) and SequenceLEQ((ASequence + Length),Current.LastSequence)}
           begin
            {Overlaps within the Segment (Duplicate)}
            Length:=0;
            ASize:=0;
            Offset:=0;
            
            {Turn off SYN}
            AFlags:=AFlags and not(TCP_FLAG_SYN);
            
            {Turn off FIN}
            AFlags:=AFlags and not(TCP_FLAG_FIN);
            
            {Discard the Segment and Return True}
            Break;
           end;
           
          {Get Next} 
          Current:=GetOverlapped(ASequence,Length);
         end; 

        {Check for zero Length}
        if Length > 0 then
         begin
          {Get the Previous Segment}
          Current:=GetPrevious(ASequence,Length);
          
          {Create a Segment}
          Segment:=AddSegment(Current,ASequence,AFlags,ASize);
          if Segment = nil then Exit;

          {Update the Last Sequence}
          if SequenceGT(Segment.LastSequence,LastSequence) then
           begin
            LastSequence:=Segment.LastSequence;
           end;
          
          {Write the Segment Data}
          if ASize > 0 then
           begin
            System.Move(Pointer(LongWord(AData) + Offset)^,Segment.Data^,ASize);
           end;
          
          {Stamp the Segment}
          Segment.Timeout:=GetTickCount64;
          
          {Update Free and Used}
          Dec(FFree,Segment.Size);
          Inc(FUsed,Segment.Size);
         
          {Schedule the Thread (Immediate for Fast Retransmit} 
          TProtocolSocket(FSocket).ScheduleSocket(TCP_TIMER_INTERVAL);
          
          {Schedule the Thread} 
          TProtocolSocket(FSocket).ScheduleSocketItem(@Segment.Item,TCP_ACK_TIMEOUT);
         end;
       end; 

      {Check the Flags} {SYN is before the first byte, FIN is after the last byte}
      if (AFlags and TCP_FLAG_SYN) = TCP_FLAG_SYN then SynSequence:=ASequence;
      if (AFlags and TCP_FLAG_FIN) = TCP_FLAG_FIN then FinSequence:=ASequence + Length; //To Do //Is this off by one ?? - Use ASize ?
     end;
   end;
   
  {Return Result} 
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.AcknowledgeSegments(var AAcknowledge:LongWord;var AWindow:Word;AForce:Boolean):Boolean;
{Used by Socket to Acknowledge segments that are received into the queue}
{Note: Delayed Ack is handled here by checking Timeout and following segments}

{Socket Timing                                                                 }
{ No notifications are sent to or scheduled for the socket thread when checking}
{ for received segments to acknowledge. This is because WriteSegment will have }
{ scheduled a notification for TCP_ACK_TIMEOUT milliseconds after each segment }
{ was received which will ensure they are acknowledged in time.}
var
 CurrentTime:Int64;
 Segment:PTCPSegment;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Clear old Segments}
  {FlushSegments(False);}
  
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: AcknowledgeSegments');
  {$ENDIF}
  
  {Get the Defaults}
  AAcknowledge:=LastAcknowledge;
  AWindow:=Min((WindowSize shr WindowScale),FFree);
  try
   {Check the Window}
   if (LastWindow < MaxSeg) and (AWindow >= MaxSeg) then
    begin
     {Send a Window Update}
     Result:=True;
    end;
   
   {Check the Sequence}
   if (NextSequence = LastSequence) and (NextSequence = LastAcknowledge) then Exit;
   
   {Check Segments}
   CurrentTime:=GetTickCount64;
   Segment:=FFirst;
   while Segment <> nil do
    begin
     {Check Acknowledged}
     if not Segment.Acknowledged then
      begin
       {Check Sequence (is this segment in order)}
       if SequenceLEQ(Segment.LastSequence,NextSequence) then
        begin
         {Check ACK Timeout (Unless Force, Override, another ACK is ready or segment contains FIN)}
         if (AForce) or (Result) or (DelayOverride(Segment)) or ((Segment.Timeout + (TCP_ACK_TIMEOUT - TCP_TIMEOUT_OFFSET)) < CurrentTime) or ((Segment.Control and TCP_FLAG_FIN) = TCP_FLAG_FIN) then
          begin
           {Unschedule the Thread}
           TProtocolSocket(FSocket).UnscheduleSocketItem(@Segment.Item);
          
           {Acknowledge Segment}
           Segment.Acknowledged:=True;
           LastAcknowledge:=Segment.LastSequence;
           AAcknowledge:=LastAcknowledge;
           
           {Update Window}
           AWindow:=Min((WindowSize shr WindowScale) - (NextSequence - LastAcknowledge),FFree);
            
           {Return Result}
           Result:=True;
           
           {Continue looking for Segments}
          end;
        end
       else
        begin
         {Send a Duplicate ACK for Fast Retransmit}
         Result:=True;
         
         {Exit if we found an out of order Segment}
         Exit;
        end;
      end;
      
     {Get Next} 
     Segment:=Segment.Next;
    end;
  finally
   {Save the Window}
   LastWindow:=AWindow;
  
   {Check the Window}
   if LastWindow < MaxSeg then
    begin
     {Schedule the Thread} 
     TProtocolSocket(FSocket).ScheduleSocket(TCP_ADVERT_TIMEOUT);
    end;
  end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.TimestampSegment(AOptions:Pointer;var AOffset:Word):Boolean;
{Used by Socket to Timestamp (RTT) segments that are received into the queue}
{Offset points to the point to insert the timestamp option}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: TimestampSegment');
  {$ENDIF}
  
  {Check Options}
  if AOptions = nil then Exit;
  
  //To do
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TTCPRecvBuffer.SelectiveAckSegments(AOptions:Pointer;var AOffset:Word):Boolean;
{Used by Socket to Selective Ack segments that are received into the queue}
{Offset points to the point to start inserting SACKS}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF TCP_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP Recv Buffer: SelectiveAckSegments');
  {$ENDIF}
  
  {Check Options}
  if AOptions = nil then Exit;
  
  {Check Selective Ack (Sack)}
  if NoSack then Exit;
  
  //To Do //
 finally 
  ReleaseLock;
 end; 
end;
  
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure TCPInit;
begin
 {}
 {Check Initialized}
 if TCPInitialized then Exit;

 {Create TCP Protocol}
 if NetworkSettings.GetBooleanDefault('TCP_PROTOCOL_ENABLED',TCP_PROTOCOL_ENABLED) then 
  begin
   TTCPProtocol.Create(ProtocolManager,TCP_PROTOCOL_NAME);
  end; 
 
 TCPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{TCP Functions}
function CheckTCP(AFamily:Word;ABuffer:Pointer):Boolean;
{Verify that the packet is a valid TCP packet}
{Buffer: The complete packet including Transport header}
var
 Length:Word;
 IP:PIPHeader;
 IP6:PIP6Header;
 TCP:PTCPHeader;
 Pseudo:TIPPseudo;
 Pseudo6:TIP6Pseudo;
begin
 {}
 Result:=False;
 
 {$IFDEF TCP_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP: CheckTCP');
 {$ENDIF}
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    IP:=PIPHeader(ABuffer);
    
    {Get Header}
    TCP:=PTCPHeader(LongWord(IP) + GetIPHeaderLength(ABuffer));
    
    {Check Length}
    Length:=GetIPDataLength(ABuffer);
    if Length >= TCP_HEADER_SIZE then
     begin
      {Get the Pseudo Header}
      Pseudo.SourceIP:=InAddrToNetwork(IP.SourceIP);
      Pseudo.DestIP:=InAddrToNetwork(IP.DestIP);
      Pseudo.Mbz:=0;
      Pseudo.Protocol:=IP.Protocol;
      Pseudo.Length:=WordNtoBE(Length);
      
      {Validate the Checksum}
      {$IFDEF TCP_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Original Checksum   = ' + IntToHex(WordBEtoN(TCP.Checksum),4));
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TCP:  Calculated Checksum = ' + IntToHex(WordBEtoN(ChecksumTCPRecv(AF_INET,@Pseudo,TCP,0,Length)),4));
      {$ENDIF}
      if TCP.Checksum = $FFFF then TCP.Checksum:=0; {Allow for all 1s case}
      if TCP.Checksum = ChecksumTCPRecv(AF_INET,@Pseudo,TCP,0,Length) then
       begin
        Result:=True;
       end;
     end;
   end;
  AF_INET6:begin
    {Get Header}
    IP6:=PIP6Header(ABuffer);
    
    {Get Header}
    TCP:=PTCPHeader(LongWord(IP6) + GetIP6HeaderLength(ABuffer));
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function GetTCPHeaderOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Return Start of TCP Header (Length of IP Header)}
    Result:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function GetTCPHeaderLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of TCP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
    
    {Return TCP Header Length field divided by 4 (shr 2) (Including Options)}
    {Same Result as ((HeaderLength shr 4) * 4)}
    Result:=(PTCPHeader(LongWord(ABuffer) + Offset).HeaderLength and $F0) shr 2;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function GetTCPOptionsLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of TCP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
    
    {Return TCP Header Length field divided by 4 (shr 2) (minus TCP_HEADER_SIZE}
    {Same Result as ((HeaderLength shr 4) * 4)}
    Result:=((PTCPHeader(LongWord(ABuffer) + Offset).HeaderLength and $F0) shr 2) - TCP_HEADER_SIZE;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function GetTCPDataOffset(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
 Length:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of TCP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
    
    {Get TCP Header Length field divided by 4 (shr 2) (Including Options)}
    Length:=(PTCPHeader(LongWord(ABuffer) + Offset).HeaderLength and $F0) shr 2;
    
    {Return Start of TCP Data (IP Header + TCP Header}
    Result:=Offset + Length;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function GetTCPDataLength(AFamily:Word;ABuffer:Pointer):Word;
{Buffer: The complete packet including Transport header}
var
 Offset:Word;
 Length:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Start of TCP Header (Length of IP Header)}
    Offset:=(PIPHeader(ABuffer).VersionLength and $0F) shl 2;
    
    {Get TCP Header Length field divided by 4 (shr 2)}
    Length:=(PTCPHeader(LongWord(ABuffer) + Offset).HeaderLength and $F0) shr 2;
    
    {Return Size of TCP Data (IP Total Length - IP Header - TCP Header)}
    Result:=(WordBEtoN(PIPHeader(ABuffer).TotalLength) - Offset) - Length;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function ChecksumTCPRecv(AFamily:Word;APseudo:PIPPseudo;ABuffer:Pointer;AOffset,ALength:Word):Word;
{Validate the Checksum of TCP Pseudo, Header and Data on Receive}
var
 Original:Word;
 TCP:PTCPHeader;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Get Header}
    TCP:=PTCPHeader(LongWord(ABuffer) + AOffset);
    
    {Save Checksum}
    Original:=TCP.Checksum;
    TCP.Checksum:=0;
    
    {Calculate 1s Compliment Checksum on TCP Pseudo, Header and Data}
    Result:=GetChecksum2(APseudo,ABuffer,IP_PSEUDO_SIZE,AOffset,ALength);
    
    {Restore Checksum}
    TCP.Checksum:=Original;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;

{==============================================================================}

function ChecksumTCPSend(AFamily:Word;APseudo:PIPPseudo;AHeader:PTCPHeader;AOptions,AData:Pointer;AOptionsLength,ADataLength:Word):Word;
{Checksum the TCP Pseudo, Header, Options and Data on Send}
var
 Original:Word;
begin
 {}
 Result:=0;
 
 {Check Address Family}
 case AFamily of
  AF_INET:begin
    {Save Checksum}
    Original:=AHeader.Checksum;
    AHeader.Checksum:=0;

    {Calculate 1s Compliment Checksum on TCP Pseudo, Header, Options and Data}
    Result:=GetChecksum4(APseudo,AHeader,AOptions,AData,IP_PSEUDO_SIZE,TCP_HEADER_SIZE,AOptionsLength,0,ADataLength);

    {Restore Checksum}
    AHeader.Checksum:=Original;
   end;
  AF_INET6:begin
    
    //To do
    
   end;
 end;
end;
  
{==============================================================================}
{==============================================================================}
{TCP Helper Functions}
 
{==============================================================================}
{==============================================================================}

initialization
 TCPInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
  