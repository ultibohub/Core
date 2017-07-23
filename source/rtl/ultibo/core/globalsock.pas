{
Ultibo Global Socket Definitions.

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


Global Sockets
==============

 Notes: Includes all global socket defintions, base classes for GetXbyY
        interfaces and DNS handling classes. 

        This unit is used by every network layer

        When changing the public section of this unit also change the
        matching declarations in the Sockets, Winsock and Winsock2 units

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GlobalSock; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes;

//To Do //Clean this up to be more in line with other units //See headers of Winsock/Winsock2 for format

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Global Socket constants}
 FD_SETSIZE     =   64;

const
 IOCPARM_MASK = $7f;
 IOC_VOID     = $20000000;
 IOC_OUT      = $40000000;
 IOC_IN       = $80000000;
 IOC_INOUT    = (IOC_IN or IOC_OUT);

 FIONREAD =Cardinal( IOC_OUT or { get # bytes to read }
  ((4 and IOCPARM_MASK) shl 16) or
  (102 shl 8) or 127);
 FIONBIO = Cardinal(IOC_IN or { set/clear non-blocking i/o }
  ((4 and IOCPARM_MASK) shl 16) or
  (102 shl 8) or 126);
 FIOASYNC     = Cardinal(IOC_IN or { set/clear async i/o }
  ((4 and IOCPARM_MASK) shl 16) or
  (102 shl 8) or 125);

 SIOCSHIWAT = Cardinal( IOC_IN or {set high watermark}
   ((4 and IOCPARM_MASK) shl 16) or
   (115 shl 8) or 0);
 SIOCGHIWAT = Cardinal( IOC_OUT or {get high watermark}
   ((4 and IOCPARM_MASK) shl 16) or
   (115 shl 8) or 1);
 SIOCSLOWAT = Cardinal( IOC_IN or {set low watermark}
   ((4 and IOCPARM_MASK) shl 16) or
   (115 shl 8) or 2);
 SIOCGLOWAT = Cardinal( IOC_OUT or  {get low watermark}
   ((4 and IOCPARM_MASK) shl 16) or
   (115 shl 8) or 3);
 SIOCATMARK = Cardinal( IOC_OUT or  {at oob mark?}
   ((4 and IOCPARM_MASK) shl 16) or
   (115 shl 8) or 7);
 
const
{ Protocols }
 IPPROTO_IP       =   0;           { dummy for IP }
 IPPROTO_ICMP     =   1;           { control message protocol }
 IPPROTO_IGMP     =   2;           { group management protocol }
 IPPROTO_GGP      =   3;           { gateway^2 (deprecated) }
 IPPROTO_TCP      =   6;           { tcp }
 IPPROTO_EGP      =   8;           { egp }
 IPPROTO_PUP      =  12;           { pup }
 IPPROTO_UDP      =  17;           { user datagram protocol }
 IPPROTO_HMP      =  20;           { hmp }
 IPPROTO_IDP      =  22;           { xns idp }
 IPPROTO_RDP      =  27;           { rdp }
 IPPROTO_IPV6	  =  41;           { IP6 header }
 IPPROTO_ROUTING  =  43;           { IP6 routing header }
 IPPROTO_FRAGMENT =  44;           { IP6 fragmentation header }
 IPPROTO_ICMPV6   =  58;           { ICMP6 }
 IPPROTO_RVD      =  66;           { rvd }
 IPPROTO_ND       =  77;           { UNOFFICIAL net disk proto }
 IPPROTO_RAW      = 255;           { raw IP packet }
 IPPROTO_MAX      = 256;

 NSPROTO_IPX      = 1000;          { ipx protocol }
 NSPROTO_SPX      = 1256;          { spx protocol }
 NSPROTO_SPXII    = 1257;          { spxii protocol }

{ Port/socket numbers: network standard functions}
 IPPORT_ANY     =   0;
 IPPORT_ECHO    =   7;
 IPPORT_DISCARD =   9;
 IPPORT_SYSTAT  =   11;
 IPPORT_DAYTIME =   13;
 IPPORT_NETSTAT =   15;
 IPPORT_FTP     =   21;
 IPPORT_TELNET  =   23;
 IPPORT_SMTP    =   25;
 IPPORT_TIMESERVER  =  37;
 IPPORT_NAMESERVER  =  42;
 IPPORT_WHOIS       =  43;
 IPPORT_DNS         =  53;
 IPPORT_MTP         =  57;
 IPPORT_BOOTPS      =  67;
 IPPORT_BOOTPC      =  68;

{ Port/socket numbers: host specific functions }
 IPPORT_TFTP        =  69;
 IPPORT_RJE         =  77;
 IPPORT_FINGER      =  79;
 IPPORT_TTYLINK     =  87;
 IPPORT_SUPDUP      =  95;

{ UNIX TCP sockets }
 IPPORT_EXECSERVER  =  512;
 IPPORT_LOGINSERVER =  513;
 IPPORT_CMDSERVER   =  514;
 IPPORT_EFSSERVER   =  520;

{ UNIX UDP sockets }
 IPPORT_BIFFUDP     =  512;
 IPPORT_WHOSERVER   =  513;
 IPPORT_ROUTESERVER =  520;

{ Ports < IPPORT_RESERVED are reserved for
  privileged processes (e.g. root). }
 IPPORT_RESERVED    =  1024;

{ Link numbers }
 IMPLINK_IP         =  155;
 IMPLINK_LOWEXPER   =  156;
 IMPLINK_HIGHEXPER  =  158;
 
const
 TF_DISCONNECT           = $01;
 TF_REUSE_SOCKET         = $02;
 TF_WRITE_BEHIND         = $04;

{ Options for use with [gs]etsockopt at the IP level. }
 IP_OPTIONS          = 1;
 IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
 IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
 IP_MULTICAST_LOOP   = 4;           { set/get IP multicast loopback    }
 IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }
 IP_DROP_MEMBERSHIP  = 6;           { drop an IP group membership      }
 IP_TTL              = 7;           { set/get IP Time To Live          }
 IP_TOS              = 8;           { set/get IP Type Of Service       }
 IP_DONTFRAGMENT     = 9;           { set/get IP Don't Fragment flag   }
 IP_HDRINCL          = 10;          { set/get IP Header include        }

 IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
 IP_DEFAULT_MULTICAST_LOOP  = 1;    { normally hear sends if a member  }
 IP_MAX_MEMBERSHIPS         = 20;   { per socket; must fit in one mbuf }

{ This is used instead of -1, since the TSocket type is unsigned.}
 INVALID_SOCKET         = TSocket(NOT(0));
 SOCKET_ERROR                   = -1;

{ Types }
 SOCK_UNSPEC     = 0;               { unspecified}
 SOCK_STREAM     = 1;               { stream socket }
 SOCK_DGRAM      = 2;               { datagram socket }
 SOCK_RAW        = 3;               { raw-protocol interface }
 SOCK_RDM        = 4;               { reliably-delivered message }
 SOCK_SEQPACKET  = 5;               { sequenced packet stream }
 SOCK_PACKET     = 10;              { linux specific way of  }
                                    { getting packets at the dev }
                                    { level.  For writing rarp and }
                                    { other similar things on the }
                                    { user level.  }

{ Option flags per-socket. }
 SO_DEBUG        = $0001;          { turn on debugging info recording }
 SO_ACCEPTCONN   = $0002;          { socket has had listen() }
 SO_REUSEADDR    = $0004;          { allow local address reuse }
 SO_KEEPALIVE    = $0008;          { keep connections alive }
 SO_DONTROUTE    = $0010;          { just use interface addresses }
 SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
 SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
 SO_LINGER       = $0080;          { linger on close if data present }
 SO_OOBINLINE    = $0100;          { leave received OOB data in line }

 SO_DONTLINGER  =   $ff7f;

{ Additional options. }
 SO_SNDBUF       = $1001;          { send buffer size }
 SO_RCVBUF       = $1002;          { receive buffer size }
 SO_SNDLOWAT     = $1003;          { send low-water mark }
 SO_RCVLOWAT     = $1004;          { receive low-water mark }
 SO_SNDTIMEO     = $1005;          { send timeout }
 SO_RCVTIMEO     = $1006;          { receive timeout }
 SO_ERROR        = $1007;          { get error status and clear }
 SO_TYPE         = $1008;          { get socket type }
 SO_CONNTIMEO    = $1009;          { connection timeout}

{ Options for connect and disconnect data and options.  Used only by
  non-TCP/IP transports such as DECNet, OSI TP4, etc. }
 SO_CONNDATA     = $7000;
 SO_CONNOPT      = $7001;
 SO_DISCDATA     = $7002;
 SO_DISCOPT      = $7003;
 SO_CONNDATALEN  = $7004;
 SO_CONNOPTLEN   = $7005;
 SO_DISCDATALEN  = $7006;
 SO_DISCOPTLEN   = $7007;

{ Option for opening sockets for synchronous access. }
 SO_OPENTYPE     = $7008;
 SO_SYNCHRONOUS_ALERT    = $10;
 SO_SYNCHRONOUS_NONALERT = $20;

{ Other NT-specific options. }
 SO_MAXDG        = $7009;
 SO_MAXPATHDG    = $700A;
 SO_UPDATE_ACCEPT_CONTEXT     = $700B;
 SO_CONNECT_TIME = $700C;

{ TCP options. }
 TCP_NODELAY     = $0001;
 TCP_MAXSEG      = $0002;
 TCP_NOPUSH      = $0004;
 TCP_NOOPT       = $0008;
 TCP_BSDURGENT   = $7000;  {Implement BSD Urgent instead of RFC793/1122}

 TCP_WSCALE      = $0010;
 TCP_NOSACK      = $0020;

{ UDP options. }
 UDP_NOCHECKSUM  = $0001;

{ Address families. }
 AF_UNSPEC       = 0;               { unspecified }
 AF_UNIX         = 1;               { local to host (pipes, portals) }
 AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
 AF_IMPLINK      = 3;               { arpanet imp addresses }
 AF_PUP          = 4;               { pup protocols: e.g. BSP }
 AF_CHAOS        = 5;               { mit CHAOS protocols }
 AF_IPX          = 6;               { IPX and SPX }
 AF_NS           = 6;               { XEROX NS protocols }
 AF_ISO          = 7;               { ISO protocols }
 AF_OSI          = AF_ISO;          { OSI is ISO }
 AF_ECMA         = 8;               { european computer manufacturers }
 AF_DATAKIT      = 9;               { datakit protocols }
 AF_CCITT        = 10;              { CCITT protocols, X.25 etc }
 AF_SNA          = 11;              { IBM SNA }
 AF_DECnet       = 12;              { DECnet }
 AF_DLI          = 13;              { Direct data link interface }
 AF_LAT          = 14;              { LAT }
 AF_HYLINK       = 15;              { NSC Hyperchannel }
 AF_APPLETALK    = 16;              { AppleTalk }
 AF_NETBIOS      = 17;              { NetBios-style addresses }
 AF_VOICEVIEW    = 18;              { VoiceView }
 AF_FIREFOX      = 19;              { FireFox }
 AF_UNKNOWN1     = 20;              { Somebody is using this! }
 AF_BAN          = 21;              { Banyan }
 AF_ATM          = 22;              { Native ATM Services }
 AF_INET6        = 23;              { Internetwork Version 6 }
 AF_CLUSTER      = 24;              { Microsoft Wolfpack }
 AF_12844        = 25;              { IEEE 1284.4 WG AF }
 AF_IRDA         = 26;              { IrDA }
 AF_NETDES       = 28;              { Network Designers OSI & gateway enabled }

 AF_MAX          = 29;
 
const
{ Protocol families, same as address families for now. }
 PF_UNSPEC       = AF_UNSPEC;
 PF_UNIX         = AF_UNIX;
 PF_INET         = AF_INET;
 PF_IMPLINK      = AF_IMPLINK;
 PF_PUP          = AF_PUP;
 PF_CHAOS        = AF_CHAOS;
 PF_NS           = AF_NS;
 PF_IPX          = AF_IPX;
 PF_ISO          = AF_ISO;
 PF_OSI          = AF_OSI;
 PF_ECMA         = AF_ECMA;
 PF_DATAKIT      = AF_DATAKIT;
 PF_CCITT        = AF_CCITT;
 PF_SNA          = AF_SNA;
 PF_DECnet       = AF_DECnet;
 PF_DLI          = AF_DLI;
 PF_LAT          = AF_LAT;
 PF_HYLINK       = AF_HYLINK;
 PF_APPLETALK    = AF_APPLETALK;
 PF_VOICEVIEW    = AF_VOICEVIEW;
 PF_FIREFOX      = AF_FIREFOX;
 PF_UNKNOWN1     = AF_UNKNOWN1;
 PF_BAN          = AF_BAN;
 PF_ATM          = AF_ATM;
 PF_INET6        = AF_INET6;
 PF_CLUSTER      = AF_CLUSTER;
 PF_12844        = AF_12844;
 PF_IRDA         = AF_IRDA;
 PF_NETDES       = AF_NETDES;

 PF_MAX          = AF_MAX;
 
const
{ Level number for (get/set)sockopt() to apply to socket itself. }
 SOL_SOCKET      = $ffff;          {options for socket level }

{ Maximum queue length specifiable by listen. }
 SOMAXCONN       = $7fffffff;

 MSG_OOB         = $1;             {process out-of-band data }
 MSG_PEEK        = $2;             {peek at incoming message }
 MSG_DONTROUTE   = $4;             {send without using routing tables }

 MSG_INTERRUPT   = $10;            {send/recv in the interrupt context}
 MSG_MAXIOVLEN   = 16;

 MSG_PARTIAL     = $8000;          {partial send or recv for message xport }

{ Define constant based on rfc883, used by gethostbyxxxx() calls. }
 MAXGETHOSTSTRUCT        = 1024;

{ Define flags to be used with the WSAAsyncSelect() call. }
 FD_READ         = $01;
 FD_WRITE        = $02;
 FD_OOB          = $04;
 FD_ACCEPT       = $08;
 FD_CONNECT      = $10;
 FD_CLOSE        = $20;
 
const
{ All Windows Sockets error constants are biased by WSABASEERR from the "normal"}
 WSABASEERR              = 10000;

{ Windows Sockets definitions of regular Microsoft C error constants }

 WSAEINTR                = (WSABASEERR+4);
 WSAEBADF                = (WSABASEERR+9);
 WSAEACCES               = (WSABASEERR+13);
 WSAEFAULT               = (WSABASEERR+14);
 WSAEINVAL               = (WSABASEERR+22);
 WSAEMFILE               = (WSABASEERR+24);

{ Windows Sockets definitions of regular Berkeley error constants }

 WSAEWOULDBLOCK          = (WSABASEERR+35);
 WSAEINPROGRESS          = (WSABASEERR+36);
 WSAEALREADY             = (WSABASEERR+37);
 WSAENOTSOCK             = (WSABASEERR+38);
 WSAEDESTADDRREQ         = (WSABASEERR+39);
 WSAEMSGSIZE             = (WSABASEERR+40);
 WSAEPROTOTYPE           = (WSABASEERR+41);
 WSAENOPROTOOPT          = (WSABASEERR+42);
 WSAEPROTONOSUPPORT      = (WSABASEERR+43);
 WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
 WSAEOPNOTSUPP           = (WSABASEERR+45);
 WSAEPFNOSUPPORT         = (WSABASEERR+46);
 WSAEAFNOSUPPORT         = (WSABASEERR+47);
 WSAEADDRINUSE           = (WSABASEERR+48);
 WSAEADDRNOTAVAIL        = (WSABASEERR+49);
 WSAENETDOWN             = (WSABASEERR+50);
 WSAENETUNREACH          = (WSABASEERR+51);
 WSAENETRESET            = (WSABASEERR+52);
 WSAECONNABORTED         = (WSABASEERR+53);
 WSAECONNRESET           = (WSABASEERR+54);
 WSAENOBUFS              = (WSABASEERR+55);
 WSAEISCONN              = (WSABASEERR+56);
 WSAENOTCONN             = (WSABASEERR+57);
 WSAESHUTDOWN            = (WSABASEERR+58);
 WSAETOOMANYREFS         = (WSABASEERR+59);
 WSAETIMEDOUT            = (WSABASEERR+60);
 WSAECONNREFUSED         = (WSABASEERR+61);
 WSAELOOP                = (WSABASEERR+62);
 WSAENAMETOOLONG         = (WSABASEERR+63);
 WSAEHOSTDOWN            = (WSABASEERR+64);
 WSAEHOSTUNREACH         = (WSABASEERR+65);
 WSAENOTEMPTY            = (WSABASEERR+66);
 WSAEPROCLIM             = (WSABASEERR+67);
 WSAEUSERS               = (WSABASEERR+68);
 WSAEDQUOT               = (WSABASEERR+69);
 WSAESTALE               = (WSABASEERR+70);
 WSAEREMOTE              = (WSABASEERR+71);

 WSAEDISCON              = (WSABASEERR+101);

{ Extended Windows Sockets error constant definitions }

 WSASYSNOTREADY          = (WSABASEERR+91);
 WSAVERNOTSUPPORTED      = (WSABASEERR+92);
 WSANOTINITIALISED       = (WSABASEERR+93);

 WSAENOMORE              = (WSABASEERR+102);
 WSAECANCELLED           = (WSABASEERR+103);
 WSAEINVALIDPROCTABLE    = (WSABASEERR+104);
 WSAEINVALIDPROVIDER     = (WSABASEERR+105);
 WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
 WSASYSCALLFAILURE       = (WSABASEERR+107);
 WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
 WSATYPE_NOT_FOUND       = (WSABASEERR+109);
 WSA_E_NO_MORE           = (WSABASEERR+110);
 WSA_E_CANCELLED         = (WSABASEERR+111);
 WSAEREFUSED             = (WSABASEERR+112);
 
{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }

 WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
 HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;

{ Non-Authoritative: Host not found, or SERVERFAIL }

 WSATRY_AGAIN            = (WSABASEERR+1002);
 TRY_AGAIN               = WSATRY_AGAIN;

{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }

 WSANO_RECOVERY          = (WSABASEERR+1003);
 NO_RECOVERY             = WSANO_RECOVERY;

{ Valid name, no data record of requested type }

 WSANO_DATA              = (WSABASEERR+1004);
 NO_DATA                 = WSANO_DATA;

{ no address, look for MX record }

 WSANO_ADDRESS           = WSANO_DATA;
 NO_ADDRESS              = WSANO_ADDRESS;

const
{ WinSock 2 extension -- new error codes and type definition }
 WSA_IO_PENDING = ERROR_IO_PENDING;
 WSA_IO_INCOMPLETE = ERROR_IO_INCOMPLETE;
 WSA_INVALID_HANDLE = ERROR_INVALID_HANDLE;
 WSA_INVALID_PARAMETER = ERROR_INVALID_PARAMETER;
 WSA_NOT_ENOUGH_MEMORY = ERROR_NOT_ENOUGH_MEMORY;
 WSA_OPERATION_ABORTED = ERROR_OPERATION_ABORTED;
 WSA_INVALID_EVENT = WSAEVENT(nil);
 WSA_MAXIMUM_WAIT_EVENTS = MAXIMUM_WAIT_OBJECTS;
 WSA_WAIT_FAILED = $ffffffff;
 WSA_WAIT_EVENT_0 = WAIT_OBJECT_0;
 WSA_WAIT_IO_COMPLETION = WAIT_IO_COMPLETION;
 WSA_WAIT_TIMEOUT = WAIT_TIMEOUT;
 WSA_INFINITE = INFINITE;
 
{ Windows Sockets errors redefined as regular Berkeley error constants.
  These are commented out in Windows NT to avoid conflicts with errno.h.
  Use the WSA constants instead. }

 EWOULDBLOCK        =  WSAEWOULDBLOCK;
 EINPROGRESS        =  WSAEINPROGRESS;
 EALREADY           =  WSAEALREADY;
 ENOTSOCK           =  WSAENOTSOCK;
 EDESTADDRREQ       =  WSAEDESTADDRREQ;
 EMSGSIZE           =  WSAEMSGSIZE;
 EPROTOTYPE         =  WSAEPROTOTYPE;
 ENOPROTOOPT        =  WSAENOPROTOOPT;
 EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
 ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
 EOPNOTSUPP         =  WSAEOPNOTSUPP;
 EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
 EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
 EADDRINUSE         =  WSAEADDRINUSE;
 EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
 ENETDOWN           =  WSAENETDOWN;
 ENETUNREACH        =  WSAENETUNREACH;
 ENETRESET          =  WSAENETRESET;
 ECONNABORTED       =  WSAECONNABORTED;
 ECONNRESET         =  WSAECONNRESET;
 ENOBUFS            =  WSAENOBUFS;
 EISCONN            =  WSAEISCONN;
 ENOTCONN           =  WSAENOTCONN;
 ESHUTDOWN          =  WSAESHUTDOWN;
 ETOOMANYREFS       =  WSAETOOMANYREFS;
 ETIMEDOUT          =  WSAETIMEDOUT;
 ECONNREFUSED       =  WSAECONNREFUSED;
 ELOOP              =  WSAELOOP;
 ENAMETOOLONG       =  WSAENAMETOOLONG;
 EHOSTDOWN          =  WSAEHOSTDOWN;
 EHOSTUNREACH       =  WSAEHOSTUNREACH;
 ENOTEMPTY          =  WSAENOTEMPTY;
 EPROCLIM           =  WSAEPROCLIM;
 EUSERS             =  WSAEUSERS;
 EDQUOT             =  WSAEDQUOT;
 ESTALE             =  WSAESTALE;
 EREMOTE            =  WSAEREMOTE;

 ENOTREADY          =  WSASYSNOTREADY;
 EVERNOTSUPPORTED   =  WSAVERNOTSUPPORTED;
 ENOTINITIALISED    =  WSANOTINITIALISED;

const 
 WSADESCRIPTION_LEN     =   256;
 WSASYS_STATUS_LEN      =   128;
 
const
 SD_RECEIVE = $00;
 SD_SEND = $01;
 SD_BOTH = $02;
 
{==============================================================================}
const
 {Global Socket constants for undocumented Winsock functions}

 {Constants for WsControlEx}
 WSA_MAX_INTERFACE_NAME_LEN = 256;
 WSA_ANY_SIZE = 1;

 WSA_MAX_ADAPTER_NAME = 128;

 WSA_MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
 WSA_MAX_ADAPTER_NAME_LENGTH        = 256;
 WSA_MAX_ADAPTER_ADDRESS_LENGTH     = 8;
 WSA_MAX_HOSTNAME_LEN               = 128;
 WSA_MAX_DOMAIN_NAME_LEN            = 128;
 WSA_MAX_SCOPE_ID_LEN               = 256;

 WSA_MAXLEN_IFDESCR         = 256;
 WSA_MAXLEN_PHYSADDR        = 8;

const
 WSA_USE_CURRENT_TTL        = DWORD(-1);
 WSA_USE_CURRENT_FORWARDING = DWORD(-1);

const
 WSA_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
 WSA_IF_OPER_STATUS_UNREACHABLE     = 1;
 WSA_IF_OPER_STATUS_DISCONNECTED    = 2;
 WSA_IF_OPER_STATUS_CONNECTING      = 3;
 WSA_IF_OPER_STATUS_CONNECTED       = 4;
 WSA_IF_OPER_STATUS_OPERATIONAL     = 5;
 
const
 WSA_IF_TYPE_OTHER              = 1;   {Some other type of network interface}
 WSA_IF_TYPE_ETHERNET_CSMACD    = 6;   {An Ethernet network interface}
 WSA_IF_TYPE_ISO88025_TOKENRING = 9;   {A token ring network interface}
 WSA_IF_TYPE_FDDI               = 15;  {A Fiber Distributed Data Interface (FDDI) network interface}
 WSA_IF_TYPE_PPP                = 23;  {A PPP network interface}
 WSA_IF_TYPE_SOFTWARE_LOOPBACK  = 24;  {A software loopback network interface}
 WSA_IF_TYPE_SLIP               = 28;  {A SLIP network interface}
 WSA_IF_TYPE_ATM                = 37;  {An ATM network interface}
 WSA_IF_TYPE_IEEE80211          = 71;  {An IEEE 802.11 wireless network interface}
 WSA_IF_TYPE_TUNNEL             = 131; {A tunnel type encapsulation network interface}
 WSA_IF_TYPE_IEEE1394           = 144; {An IEEE 1394 (Firewire) high performance serial bus network interface}
 WSA_IF_TYPE_IEEE80216_WMAN     = 237; {A mobile broadband interface for WiMax devices}
 WSA_IF_TYPE_WWANPP             = 243; {A mobile broadband interface for GSM-based devices}
 WSA_IF_TYPE_WWANPP2            = 244; {An mobile broadband interface for CDMA-based devices}

const 
 WSA_IF_ADMIN_STATUS_UP         = 1;
 WSA_IF_ADMIN_STATUS_DOWN       = 2;
 WSA_IF_ADMIN_STATUS_TESTING    = 3;
 
const
 WSA_IP_FORWARDING     = 1;
 WSA_IP_NOT_FORWARDING = 2;

const 
 WSA_IPNET_TYPE_OTHER   = 1;
 WSA_IPNET_TYPE_INVALID = 2;
 WSA_IPNET_TYPE_DYNAMIC = 3;
 WSA_IPNET_TYPE_STATIC  = 4;
 
 {WsControlEx Functions}
 WSA_GETNUMBEROFINTERFACES  = 1;
 WSA_GETIFENTRY             = 2;
 WSA_GETIFTABLE             = 3;
 WSA_GETIPADDRTABLE         = 4;
 WSA_GETIPNETTABLE          = 5;
 WSA_GETIPFORWARDTABLE      = 6;
 WSA_GETTCPTABLE            = 7;
 WSA_GETUDPTABLE            = 8;

 WSA_GETIPSTATISTICS        = 9;
 WSA_GETICMPSTATISTICS      = 10;
 WSA_GETTCPSTATISTICS       = 11;
 WSA_GETUDPSTATISTICS       = 12;

 WSA_SETIFENTRY             = 13;
 WSA_CREATEIPFORWARDENTRY   = 14;
 WSA_SETIPFORWARDENTRY      = 15;
 WSA_DELETEIPFORWARDENTRY   = 16;

 WSA_SETIPSTATISTICS        = 17;
 WSA_SETIPTTL               = 18;

 WSA_CREATEIPNETENTRY       = 19;
 WSA_SETIPNETENTRY          = 20;
 WSA_DELETEIPNETENTRY       = 21;
 WSA_FLUSHIPNETTABLE        = 22;

 WSA_CREATEPROXYARPENTRY    = 23;
 WSA_DELETEPROXTARPENTRY    = 24;

 WSA_SETTCPENTRY            = 25;
 WSA_GETINTERFACEINFO       = 26;
 WSA_GETUNIDIRECTIONALADAPTERINFO  = 27;

 WSA_GETBESTINTERFACE       = 28;
 WSA_GETBESTROUTE           = 29;
 WSA_NOTIFYADDRCHANGE       = 30;
 WSA_NOTIFYROUTECHANGE      = 31;
 WSA_GETADAPTERINDEX        = 32;
 WSA_ADDIPADDRESS           = 33;
 WSA_DELETEIPADDRESS        = 34;
 WSA_GETNETWORKPARAMS       = 35;
 WSA_GETADAPTERSINFO        = 36;
 WSA_GETPERADAPTERINFO      = 37;
 WSA_IPRELEASEADDRESS       = 38;
 WSA_IPRENEWADDRESS         = 39;
 WSA_SENDARP                = 40;
 WSA_GETRTTANDHOPCOUNT      = 41;
 WSA_GETFRIENDLYIFINDEX     = 42;
 WSA_ENABLEROUTER           = 43;
 WSA_UNENABLEROUTER         = 44;
 
{==============================================================================}
const
 {Global Socket constants for enhanced Winsock functions}
 MAX_NAME_SIZE = 80;
 MAX_NAME_ALIASES = 5;
 MAX_NAME_SERVERS = 5;
 MAX_HOST_ALIASES = 16;
 
const
 {Error codes from getaddrinfo()}
 EAI_AGAIN    = WSATRY_AGAIN;
 EAI_BADFLAGS = WSAEINVAL;
 EAI_FAIL     = WSANO_RECOVERY;
 EAI_FAMILY   = WSAEAFNOSUPPORT;
 EAI_MEMORY   = WSA_NOT_ENOUGH_MEMORY;
 {EAI_NODATA   = WSANO_DATA;}
 EAI_NONAME   = WSAHOST_NOT_FOUND;
 EAI_SERVICE  = WSATYPE_NOT_FOUND;
 EAI_SOCKTYPE = WSAESOCKTNOSUPPORT;

 EAI_NODATA = EAI_NONAME;
 
const
 {Flags used in "hints" argument to getaddrinfo()}
 {Note: Under Linux these values may be different}
 AI_PASSIVE                = $00000001; {Socket address will be used in bind() call}
 AI_CANONNAME              = $00000002; {Return canonical name from DNS in the first ai_canonname (Cannot be used with AI_FQDN)}
 AI_NUMERICHOST            = $00000004; {Nodename must be a numeric address string}
 AI_NUMERICSERV            = $00000008; {Servicename must be a numeric port string}
 AI_ALL                    = $00000100; {Return both IPv6 and IPv4 addresses}
 AI_ADDRCONFIG             = $00000400; {Only return addresses if a global address is configured (IPv4 or IPv6), loopback addresses do not count as global}
 AI_V4MAPPED               = $00000800; {Map returned IPv4 addresses to IPv6 address format}
 AI_NON_AUTHORITATIVE      = $00004000; {Allow both autoritive and non authoritive return addresses (NS_EMAIL only)}
 AI_SECURE                 = $00008000; {Obtain result using enhanced security only (NS_EMAIL only)}
 AI_RETURN_PREFERRED_NAMES = $00010000; {Return preferred names (NS_EMAIL only)}
 AI_FQDN                   = $00020000; {Return the FQDN for the single name specified on NodeName (Cannot be used with AI_CANONNAME)}
 AI_FILESERVER             = $00040000; {The requested name is being used for file sharing (Hint Only)}

 {Flags for getnameinfo()}
 NI_NOFQDN      = $01; {Only return nodename portion for local hosts}
 NI_NUMERICHOST = $02; {Return numeric form of the host's address}
 NI_NAMEREQD    = $04; {Error if the host's name not in DNS}
 NI_NUMERICSERV = $08; {Return numeric form of the service (port #)}
 NI_DGRAM       = $10; {Service is a datagram service}

 NI_MAXHOST = 1025; {Max size of a fully-qualified domain name}
 NI_MAXSERV = 32;   {Max size of a service name}

 INET_ADDRSTR_ANY = '0.0.0.0';
 INET6_ADDRSTR_INIT = '0::0';

 INET_ADDRSTR_BROADCAST = '255.255.255.255';
 
 INET_ADDRSTRLEN  = 16; {Max size of numeric form of IPv4 address}
 INET6_ADDRSTRLEN = 46; {Max size of numeric form of IPv6 address}
 
{==============================================================================}
type
 {Global Socket types}
 tOS_INT  = LongInt;
 tOS_UINT = DWORD;
 ptOS_INT = ^tOS_INT;
 ptOS_UINT = ^tOS_UINT;
 u_char = Char;
 u_short = Word;
 u_int = tOS_UINT;
 u_long = DWORD;
 pu_char = ^u_char;
 pu_short = ^u_short;
 pu_int = ^u_int;
 pu_long = ^u_long;
 
 TSocket = GlobalTypes.TSocket;

type
 WSAEVENT = GlobalTypes.WSAEVENT;
 PWSAEVENT = ^WSAEVENT;
 LPWSAEVENT = PWSAEVENT;
 PMBChar = PChar;
 
type
 PFDSet = ^TFDSet;
 fdset = record
  fd_count: u_int;
  fd_array: array[0..FD_SETSIZE-1] of TSocket;
 end;
 TFDSet = fdset;
 
 PTimeVal = ^TTimeVal;
 timeval = record
  tv_sec: LongInt;
  tv_usec: LongInt;
 end;
 TTimeVal = timeval;

type
 PHostEnt = ^THostEnt;
 hostent = record
  h_name: PChar;
  h_aliases: ^PChar;
  h_addrtype: Smallint;
  h_length: Smallint;
  case Byte of
   0: (h_addr_list: ^PChar);
   1: (h_addr: ^PChar)
 end;
 THostEnt = hostent;

 PNetEnt = ^TNetEnt;
 netent = record
  n_name: PChar;
  n_aliases: ^PChar;
  n_addrtype: Smallint;
  n_net: u_long;
 end;
 TNetEnt = netent;

 PServEnt = ^TServEnt;
 servent = record
  s_name: PChar;
  s_aliases: ^PChar;
  s_port: Smallint;
  s_proto: PChar;
 end;
 TServEnt = servent;

 PProtoEnt = ^TProtoEnt;
 protoent = record
  p_name: PChar;
  p_aliases: ^Pchar;
  p_proto: Smallint;
 end;
 TProtoEnt = protoent;

type
 SunB = packed record
  s_b1, s_b2, s_b3, s_b4: u_char;
 end;

 SunW = packed record
  s_w1, s_w2: u_short;
 end;

 PInAddr = ^TInAddr;
 in_addr = record
  case integer of
   0: (S_un_b: SunB);
   1: (S_un_w: SunW);
   2: (S_addr: u_long);
   3: (S_bytes: packed array[1..4] of Byte);
 end;
 TInAddr = in_addr;

 {IPv6 version of above}
 PIn6Addr = ^TIn6Addr;
 in6_addr = record
  case byte of
   0: (u6_addr8  : array[0..15] of byte);
   1: (u6_addr16 : array[0..7] of Word);
   2: (u6_addr32 : array[0..3] of Cardinal);
   3: (s6_addr8  : array[0..15] of shortint);
   4: (s6_addr   : array[0..15] of shortint);
   5: (s6_addr16 : array[0..7] of smallint);
   6: (s6_addr32 : array[0..3] of LongInt);
  {case integer of
   0: (S6_u8: array[0..15] of u_char);
   1: (S6_u32: array[0..3] of u_int);
   2: (S6_u64: array[0..1] of Int64);}
  {S6_addr: array [0..15] of Byte;}
 end;
 TIn6Addr = in6_addr;

 {IPX version of above}
 PIpxAddr = ^TIpxAddr;
 ipx_addr = record
  S_net: array [0..3] of Byte;
  S_node: array [0..5] of Byte;
  S_socket: Word;
 end;
 TIpxAddr = ipx_addr;

 PSockAddrIn = ^TSockAddrIn;
 sockaddr_in = record
  case Integer of
   0: (sin_family: u_short;
       sin_port: u_short;
       sin_addr: TInAddr;
       sin_zero: array[0..7] of Char);
   1: (sa_family: u_short;
       sa_data: array[0..13] of Char)
 end;
 TSockAddrIn = sockaddr_in;

 {IPv6 version of above}
 PSockAddrIn6 = ^TSockAddrIn6;
 sockaddr_in6 = record
  sin6_family: u_short;    { AF_INET6 }
  sin6_port: u_short;      { transport layer port # }
  sin6_flowinfo: u_int;    { IPv6 flow information }
  sin6_addr: in6_addr;     { IPv6 address }
  sin6_scope_id: u_int;    { set of interfaces for a scope }
 end;
 TSockAddrIn6 = sockaddr_in6;

 {IPX version of above}
 PSockAddrIpx = ^TSockAddrIpx;
 sockaddr_ipx = record
  sipx_family: u_short;    { AF_IPX }
  sipx_addr: ipx_addr;     { IPX address }
 end;
 TSockAddrIpx = sockaddr_ipx;

type
 { Structure used by kernel to store most addresses. }
 PSOCKADDR = ^TSockAddr;
 TSockAddr = sockaddr_in;

 {IPv6 version of above}
 PSOCKADDR6 = ^TSockAddr6;
 TSockAddr6 = sockaddr_in6;

 {IPX version of above}
 {PSOCKADDRIPX = ^TSockAddrIpx;}
 {TSockAddrIpx = sockaddr_ipx;}
 
 { Structure used by kernel to pass protocol information in raw sockets. }
 PSockProto = ^TSockProto;
 sockproto = record
  sp_family: u_short;
  sp_protocol: u_short;
 end;
 TSockProto = sockproto;

type
{ Structure used for manipulating linger option. }
 PLinger = ^TLinger;
 linger = record
  l_onoff: u_short;
  l_linger: u_short;
 end;
 TLinger = linger;
 
type
 PWSAData = ^TWSAData;
 WSAData = record {Also WSDATA}
  wVersion: Word;
  wHighVersion: Word;
  szDescription: array[0..WSADESCRIPTION_LEN] of Char;
  szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
  iMaxSockets: Word;
  iMaxUdpDg: Word;
  lpVendorInfo: PChar;
 end;
 TWSAData = WSAData;

 PTransmitFileBuffers = ^TTransmitFileBuffers;
 _TRANSMIT_FILE_BUFFERS = record
  Head: Pointer;
  HeadLength: DWORD;
  Tail: Pointer;
  TailLength: DWORD;
 end;
 TTransmitFileBuffers = _TRANSMIT_FILE_BUFFERS;
 TRANSMIT_FILE_BUFFERS = _TRANSMIT_FILE_BUFFERS;
 
{==============================================================================}
type
 {Global Socket types for undocumented Winsock functions}
 
 {Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP}
 PMulticastRequest = ^TMulticastRequest;
 ip_mreq = record
  IMRMultiAddr : TInAddr;   {IP multicast address of group}
  IMRInterface : TInAddr;   {local IP address of interface}
 end;
 TMulticastRequest = ip_mreq;

type
 {Structures for WsControlEx} {Note: Must be same as IP Helper}
 PWSA_IFROW = ^WSA_IFROW;
 WSA_IFROW = record
  wszName: array [0..WSA_MAX_INTERFACE_NAME_LEN - 1] of WideChar;
  dwIndex: DWORD;
  dwType: DWORD;
  dwMtu: DWORD;
  dwSpeed: DWORD;
  dwPhysAddrLen: DWORD;
  bPhysAddr: array [0..WSA_MAXLEN_PHYSADDR - 1] of Byte;
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
  bDescr: array[0..WSA_MAXLEN_IFDESCR - 1] of Byte;
 end;
 TWSAIfRow = WSA_IFROW;
 PWSAIfRow = PWSA_IFROW;

 PWSA_IFTABLE = ^WSA_IFTABLE;
 WSA_IFTABLE = record
  dwNumEntries: DWORD;
  table: array [0..WSA_ANY_SIZE - 1] of WSA_IFROW;
 end;
 TWSAIftable = WSA_IFTABLE;
 PWSAIftable = PWSA_IFTABLE;

 PWSA_IPSTATS = ^WSA_IPSTATS;
 WSA_IPSTATS = record
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
 TWSAIpStats = WSA_IPSTATS;
 PWSAIpStats = PWSA_IPSTATS;

 PWSA_IPADDRROW = ^WSA_IPADDRROW;
 WSA_IPADDRROW = record
  dwAddr: DWORD;
  dwIndex: DWORD;
  dwMask: DWORD;
  dwBCastAddr: DWORD;
  dwReasmSize: DWORD;
  unused1: Word;
  unused2: Word;
 end;
 TWSAIpAddrRow = WSA_IPADDRROW;
 PWSAIpAddrRow = PWSA_IPADDRROW;

 PWSA_IPADDRTABLE = ^WSA_IPADDRTABLE;
 WSA_IPADDRTABLE = record
  dwNumEntries: DWORD;
  table: array [0..WSA_ANY_SIZE - 1] of WSA_IPADDRROW;
 end;
 TWSAIpAddrTable = WSA_IPADDRTABLE;
 PWSAIpAddrTable = PWSA_IPADDRTABLE;

 PWSA_IPFORWARDNUMBER = ^WSA_IPFORWARDNUMBER;
 WSA_IPFORWARDNUMBER = record
   dwValue: DWORD;
 end;
 TWSAIpForwardNumber = WSA_IPFORWARDNUMBER;
 PWSAIpForwardNumber = PWSA_IPFORWARDNUMBER;

 PWSA_IPFORWARDROW = ^WSA_IPFORWARDROW;
 WSA_IPFORWARDROW = record
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
 TWSAIpForwardRow = WSA_IPFORWARDROW;
 PWSAIpForwardRow = PWSA_IPFORWARDROW;

 PWSA_IPFORWARDTABLE = ^WSA_IPFORWARDTABLE;
 WSA_IPFORWARDTABLE = record
   dwNumEntries: DWORD;
   table: array [0..WSA_ANY_SIZE - 1] of WSA_IPFORWARDROW;
 end;
 TWSAIpForwardTable = WSA_IPFORWARDTABLE;
 PWSAIpForwardTable = PWSA_IPFORWARDTABLE;
 
 PWSA_IPNETROW = ^WSA_IPNETROW;
 WSA_IPNETROW = record
   dwIndex: DWORD;
   dwPhysAddrLen: DWORD;
   bPhysAddr: array [0..WSA_MAXLEN_PHYSADDR - 1] of BYTE;
   dwAddr: DWORD;
   dwType: DWORD;
 end;
 TWSAIpNetRow = WSA_IPNETROW;
 PWSAIpNetRow = PWSA_IPNETROW;

 PWSA_IPNETTABLE = ^WSA_IPNETTABLE;
 WSA_IPNETTABLE = record
   dwNumEntries: DWORD;
   table: array [0..WSA_ANY_SIZE - 1] of WSA_IPNETROW;
 end;
 TWSAIpNetTable = WSA_IPNETTABLE;
 PWSAIpNetTable = PWSA_IPNETTABLE;
 
 PWSA_IP_ADAPTER_INDEX_MAP = ^WSA_IP_ADAPTER_INDEX_MAP;
 WSA_IP_ADAPTER_INDEX_MAP = record
  Index: ULONG;
  Name: array [0..WSA_MAX_ADAPTER_NAME - 1] of WideChar;
 end;
 TWSAIpAdapterIndexMap = WSA_IP_ADAPTER_INDEX_MAP;
 PWSAIpAdapterIndexMap = PWSA_IP_ADAPTER_INDEX_MAP;

 PWSA_IP_INTERFACE_INFO = ^WSA_IP_INTERFACE_INFO;
 WSA_IP_INTERFACE_INFO = record
  NumAdapters: DWORD;  {LongInt}
  Adapter: array [0..0] of WSA_IP_ADAPTER_INDEX_MAP;
 end;
 TWSAIpInterfaceInfo = WSA_IP_INTERFACE_INFO;
 PWSAIpInterfaceInfo = PWSA_IP_INTERFACE_INFO;

 PWSA_IP_MASK_STRING = ^WSA_IP_MASK_STRING;
 WSA_IP_ADDRESS_STRING = record
  S: array [0..15] of Char;
 end;
 PWSA_IP_ADDRESS_STRING = ^WSA_IP_ADDRESS_STRING;
 WSA_IP_MASK_STRING = WSA_IP_ADDRESS_STRING;
 TWSAIpAddressString = WSA_IP_ADDRESS_STRING;
 PWSAIpAddressString = PWSA_IP_MASK_STRING;

 PWSA_IP_ADDR_STRING = ^WSA_IP_ADDR_STRING;
 WSA_IP_ADDR_STRING = record
  Next: PWSA_IP_ADDR_STRING;
  IpAddress: WSA_IP_ADDRESS_STRING;
  IpMask: WSA_IP_MASK_STRING;
  Context: DWORD;
 end;
 TWSAIpAddrString = WSA_IP_ADDR_STRING;
 PWSAIpAddrString = PWSA_IP_ADDR_STRING;

 PWSA_IP_ADAPTER_INFO = ^WSA_IP_ADAPTER_INFO;
 WSA_IP_ADAPTER_INFO = record
  Next: PWSA_IP_ADAPTER_INFO;
  ComboIndex: DWORD;
  AdapterName: array [0..WSA_MAX_ADAPTER_NAME_LENGTH + 3] of Char;
  Description: array [0..WSA_MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
  AddressLength: UINT;
  Address: array [0..WSA_MAX_ADAPTER_ADDRESS_LENGTH - 1] of BYTE;
  Index: DWORD;
  Type_: UINT;
  DhcpEnabled: UINT;
  CurrentIpAddress: PWSA_IP_ADDR_STRING;
  IpAddressList: WSA_IP_ADDR_STRING;
  GatewayList: WSA_IP_ADDR_STRING;
  DhcpServer: WSA_IP_ADDR_STRING;
  HaveWins: BOOL;
  PrimaryWinsServer: WSA_IP_ADDR_STRING;
  SecondaryWinsServer: WSA_IP_ADDR_STRING;
  LeaseObtained: LongInt;  {time_t}
  LeaseExpires: LongInt;   {time_t}
 end;
 TWSAIpAdapterInfo = WSA_IP_ADAPTER_INFO;
 PWSAIpAdapterInfo = PWSA_IP_ADAPTER_INFO;

 PWSA_FIXED_INFO = ^WSA_FIXED_INFO;
 WSA_FIXED_INFO = record
  HostName: array [0..WSA_MAX_HOSTNAME_LEN + 3] of Char;
  DomainName: array[0..WSA_MAX_DOMAIN_NAME_LEN + 3] of Char;
  CurrentDnsServer: PWSA_IP_ADDR_STRING;
  DnsServerList: WSA_IP_ADDR_STRING;
  NodeType: UINT;
  ScopeId: array [0..WSA_MAX_SCOPE_ID_LEN + 3] of Char;
  EnableRouting: UINT;
  EnableProxy: UINT;
  EnableDns: UINT;
 end;
 TWSAFixedInfo = WSA_FIXED_INFO;
 PWSAFixedInfo = PWSA_FIXED_INFO;

{==============================================================================}
type
 {Global Socket types for enhanced Winsock functions}
 PNetToAddr = ^TNetToAddr;
 TNetToAddr = array[0..MAX_NAME_SIZE - 1] of Char;
 
 PWSABinding = ^TWSABinding;  {A network Binding (Address)} //To Do //Remove ?
 WSABinding = record   //To Do
  Version: Word;
  ConfigType: Word;
  ConfigServer: TInAddr;
  BindingAddress: TInAddr;
  BindingNetmask: TInAddr;
  BindingGateway: TInAddr;
  NextBinding: PWSABinding;
 end;
 TWSABinding = WSABinding;

 PWSAProvider = ^TWSAProvider;    {A network Provider (Transport)} //To Do //Remove ?
 WSAProvider = record
  Version: Word;
  TransportFamily: Word; //To Do //AddressFamily
  TransportType: Word;   //To Do //PacketType
  TransportHandle: Pointer;
  ConfigType: Word;
  ConfigServer: TInAddr;
  ProviderAddress: TInAddr;
  ProviderNetmask: TInAddr;
  ProviderGateway: TInAddr;
  ProviderName: array[0..MAX_NAME_SIZE] of Char; //To Do //Change to PacketName/TransportName move above
  NextProvider: PWSAProvider;
 end;
 TWSAProvider = WSAProvider;

 PWSAInterface = ^TWSAInterface;  {A network Interface (Adapter)} //To Do //Remove ?
 WSAInterface = record
  Version: Word;
  AdapterType: Word;
  AdapterInt: Word;
  AdapterPort: Word;
  AdapterName: array[0..MAX_NAME_SIZE] of Char;
  AdapterHandle: Pointer;
  FirstProvider: PWSAProvider;
  NextInterface: PWSAInterface;
 end;
 TWSAInterface = WSAInterface;

 PWSAConfig = ^TWSAConfig;  {General Network Configuration} //To Do //Remove ?
 WSAConfig = record
  Version: Word;
  Hostname: array[0..MAX_NAME_SIZE - 1] of Char;
  Namesuffix: array[0..MAX_NAME_SIZE - 1] of Char;
  Nameservers: array[0..MAX_NAME_SERVERS - 1] of TInAddr;
  //DebugProc: TDebugProc; //To Do //Remove
  //ErrorProc: TDebugProc; //To Do //Remove
  FirstInterface: PWSAInterface;
 end;
 TWSAConfig = WSAConfig;

type
 PWSAExtendedBinding = ^TWSAExtendedBinding;  {A network Binding (Address)} //To Do //Remove ?
 WSAExtendedBinding = record
  Size: LongWord;
  Version: Word;
  ConfigType: Word;
  ConfigServer: TInAddr;
  ConfigServer6: TIn6Addr;
  ConfigInitDelay: LongWord;
  ConfigRetryCount: LongWord;
  ConfigRetryTimeout: LongWord;
  BindingAddress: TInAddr;
  BindingNetmask: TInAddr;
  BindingGateway: TInAddr;
  BindingAddress6: TIn6Addr;
  BindingNetmask6: TIn6Addr;
  BindingGateway6: TIn6Addr;
  Namesuffix: array[0..MAX_NAME_SIZE - 1] of Char;
  Nameservers: array[0..MAX_NAME_SERVERS - 1] of TInAddr;
  Nameservers6: array[0..MAX_NAME_SERVERS - 1] of TIn6Addr;
  NextBinding: PWSAExtendedBinding;
 end;
 TWSAExtendedBinding = WSAExtendedBinding;

 PWSAExtendedProvider = ^TWSAExtendedProvider;    {A network Provider (Transport)} //To Do //Remove ?
 WSAExtendedProvider = record
  Size: LongWord;
  Version: Word;
  TransportFamily: Word; //To Do //AddressFamily
  TransportType: Word;   //To Do //PacketType
  TransportHandle: Pointer;
  ProviderName: array[0..MAX_NAME_SIZE] of Char; //To Do //Change to PacketName/TransportName move above
  FirstBinding: PWSAExtendedBinding;
  NextProvider: PWSAExtendedProvider;
 end;
 TWSAExtendedProvider = WSAExtendedProvider;

 PWSAExtendedInterface = ^TWSAExtendedInterface;  {A network Interface (Adapter)} //To Do //Remove ?
 WSAExtendedInterface = record
  Size: LongWord;
  Version: Word;
  AdapterType: Word;
  AdapterInt: Word;
  AdapterPort: Word;
  AdapterName: array[0..MAX_NAME_SIZE] of Char;
  AdapterHandle: Pointer;
  FirstProvider: PWSAExtendedProvider;
  NextInterface: PWSAExtendedInterface;
 end;
 TWSAExtendedInterface = WSAExtendedInterface;

 PWSAExtendedConfig = ^TWSAExtendedConfig;  {General Network Configuration} //To Do //Remove ?
 WSAExtendedConfig = record
  Size: LongWord;
  Version: Word;
  Hostname: array[0..MAX_NAME_SIZE - 1] of Char;
  //DebugProc: TDebugProc; //To Do //Remove
  //ErrorProc: TDebugProc; //To Do //Remove
  FirstInterface: PWSAExtendedInterface;
 end;
 TWSAExtendedConfig = WSAExtendedConfig;
 
type
 {Structure used in getaddrinfo() call}
 PAddrInfo = ^TAddrInfo;
 TAddrInfo = record
  ai_flags:LongInt;       {AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST}
  ai_family:LongInt;      {PF_xxx}
  ai_socktype:LongInt;    {SOCK_xxx}
  ai_protocol:LongInt;    {0 or IPPROTO_xxx for IPv4 and IPv6}
  ai_addrlen:size_t;      {Length of ai_addr}
  ai_canonname:PChar;     {Canonical name for nodename}
  ai_addr:PSockAddr;      {Binary address}
  ai_next:PAddrInfo;      {Next structure in linked list}
 end;
 
{==============================================================================}
{var}
 {Global Socket variables}
 
const
 INADDR_ANY       = $00000000;
 INADDR_LOOPBACK  = $7F000001;
 INADDR_BROADCAST = $FFFFFFFF;
 INADDR_NONE      = $FFFFFFFF;

 IN_CLASSA_NET = $ff000000;
 IN_CLASSA_NSHIFT = 24;
 IN_CLASSA_HOST = $00ffffff;
 IN_CLASSA_MAX = 128;
 IN_CLASSB_NET = $ffff0000;
 IN_CLASSB_NSHIFT = 16;
 IN_CLASSB_HOST = $0000ffff;
 IN_CLASSB_MAX = 65536;
 IN_CLASSC_NET = $ffffff00;
 IN_CLASSC_NSHIFT = 8;
 IN_CLASSC_HOST = $000000ff;
 
 IN6ADDR_ANY:TIn6Addr = (u6_addr16: (0, 0, 0, 0, 0, 0, 0, 0));
 IN6ADDR_LOOPBACK:TIn6Addr = (u6_addr8: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1));
 
const
 {Variables for getaddrinfo()} 
 IN6ADDR_ANY_INIT:TIn6Addr = (u6_addr16: (0, 0, 0, 0, 0, 0, 0, 0));
 IN6ADDR_LOOPBACK_INIT:TIn6Addr = (u6_addr8: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1));
 
{==============================================================================}
{Global Socket functions}
procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

function SequenceLT(A,B:LongInt):Boolean; inline;
function SequenceLEQ(A,B:LongInt):Boolean; inline;
function SequenceGT(A,B:LongInt):Boolean; inline;
function SequenceGEQ(A,B:LongInt):Boolean; inline;

function ProtocolToString(Protocol:Word):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Global Socket functions}
procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var
 I:Integer;
begin
 {}
 I:=0;
 while I < FDSet.fd_count do
  begin
   if FDSet.fd_array[I] = Socket then
    begin
     while I < FDSet.fd_count - 1 do
      begin
       FDSet.fd_array[I]:=FDSet.fd_array[I + 1];
       Inc(I);
      end;
      
     Dec(FDSet.fd_count);
     Break;
    end;
    
   Inc(I);
  end;
end;

{==============================================================================}

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
var
 I:Integer;
begin
 {}
 I:=FDSet.fd_count;
 while I > 0 do
  begin
   Dec(I);
   if FDSet.fd_array[I] = Socket then
    begin
     Result:=True;
     Exit;
    end;
  end;
  
 Result:=False;
end;

{==============================================================================}

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
 {}
 if FDSet.fd_count < FD_SETSIZE then
  begin
   FDSet.fd_array[FDSet.fd_count]:=Socket;
   Inc(FDSet.fd_count);
  end;
end;

{==============================================================================}

procedure FD_ZERO(var FDSet: TFDSet);
begin
 {}
 FDSet.fd_count:=0;
end;

{==============================================================================}

function SequenceLT(A,B:LongInt):Boolean; inline;
begin
 {}
 Result:=(A - B) < 0;
end;

{==============================================================================}

function SequenceLEQ(A,B:LongInt):Boolean; inline;
begin
 {}
 Result:=(A - B) <= 0;
end;

{==============================================================================}

function SequenceGT(A,B:LongInt):Boolean; inline;
begin
 {}
 Result:=(A - B) > 0;
end;

{==============================================================================}

function SequenceGEQ(A,B:LongInt):Boolean; inline;
begin
 {}
 Result:=(A - B) >= 0;
end;

{==============================================================================}

function ProtocolToString(Protocol:Word):String;
begin
 {}
 Result:='';
 
 case Protocol of
  IPPROTO_IP:Result:='IPPROTO_IP';
  IPPROTO_ICMP:Result:='IPPROTO_ICMP';
  IPPROTO_IGMP:Result:='IPPROTO_IGMP';
  IPPROTO_TCP:Result:='IPPROTO_TCP';
  IPPROTO_EGP:Result:='IPPROTO_EGP';
  IPPROTO_PUP:Result:='IPPROTO_PUP';
  IPPROTO_UDP:Result:='IPPROTO_UDP';
  IPPROTO_HMP:Result:='IPPROTO_HMP';
  IPPROTO_IDP:Result:='IPPROTO_IDP';
  IPPROTO_RDP:Result:='IPPROTO_RDP';
  IPPROTO_IPV6:Result:='IPPROTO_IPV6';
  IPPROTO_ROUTING:Result:='IPPROTO_ROUTING';
  IPPROTO_FRAGMENT:Result:='IPPROTO_FRAGMENT';
  IPPROTO_ICMPV6:Result:='IPPROTO_ICMPV6';
  IPPROTO_RVD:Result:='IPPROTO_RVD';
  IPPROTO_RAW:Result:='IPPROTO_RAW';
 end;
end;

{==============================================================================}
{==============================================================================}

end.
