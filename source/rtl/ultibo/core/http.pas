{
Ultibo HTTP interface unit.

Copyright (C) 2023 - SoftOz Pty Ltd.

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

 RFC2068 - Hypertext Transfer Protocol (HTTP/1.1) - https://tools.ietf.org/html/rfc2068
 RFC2616 - Hypertext Transfer Protocol (HTTP/1.1) - http://www.w3.org/Protocols/rfc2616/rfc2616.html
 

HTTP
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HTTP;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,Classes,Ultibo,UltiboClasses,UltiboUtils,Winsock2,Crypto,Authentication;

//To Do //SetResponse/Result method in Response
//To Do //SetRedirect method in Response

//To Do //Some other Get methods in Request ?
            
//To Do //Some form of locking (Reader/Writer ?) around DoGet/DoHead etc (Host, Error and Document) (also in WebStatus ?)
         
                         
//To Do //See also: \source\packages\fcl-base\src\uriparser.pp
        //          \source\packages\fcl-web\src\base\fphttpserver.pp
        //          \source\packages\fcl-web\src\base\fphttpclient.pp
        
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {HTTP specific constants}
 HTTP_TAB = Chr(9);                 {Tab}
 HTTP_SPACE = Chr(32);              {Space}
 HTTP_DASH = '-';                   {-} 
 HTTP_COLON = ':';                  {:}
 HTTP_LINE_END = Chr(13) + Chr(10); {CR LF}
 
 HTTP_PORT_SEPARATOR = ':';         {:}
 HTTP_PATH_SEPARATOR = '/';         {/}
 HTTP_QUERY_SEPARATOR = '?';        {?}
 HTTP_PARAM_SEPARATOR = '=';        {=}
 HTTP_PARAM_DELIMITER = '&';        {&}
 HTTP_HEADER_SEPARATOR = ':';       {:}
 HTTP_BOOKMARK_SEPARATOR = '#';     {#}
 HTTP_PROTOCOL_SEPARATOR = '://';   {://}
 
 HTTP_BUFFER_SIZE = SIZE_2K;
 
 {HTTP Character constants}
 
 {HTTP Date constants}
 HTTP_DATE_FORMAT_RFC1123 = 'ddd, dd mmm yyyy hh:nn:ss "GMT"';  {RFC 822, updated by RFC 1123}
 HTTP_DATE_FORMAT_RFC850  = 'dddd, dd-mmm-yy hh:nn:ss "GMT"';   {RFC 850, obsoleted by RFC 1036}
 HTTP_DATE_FORMAT_ANSIC   = 'ddd mmm d hh:nn:ss yyyy';          {ANSI C's asctime() format}
 
 HTTP_DATE_FORMAT = HTTP_DATE_FORMAT_RFC1123;
 
 {HTTP Client constants}
 HTTP_USERAGENT_STRING = 'Mozilla/5.0 (compatible; Ultibo/' + ULTIBO_RELEASE_VERSION + ')';
 HTTP_REQUEST_TIMEOUT = 60000;      {60 seconds}
 HTTP_KEEPALIVE_TIMEOUT = 300;      {300 seconds (5 minutes)}
 HTTP_MAX_REDIRECTS = 16;
 HTTP_PROXY_PORT_DEFAULT = 8080;
 
 {HTTP Server constants} 
 HTTP_SERVER_STRING = 'Ultibo/' + ULTIBO_RELEASE_VERSION;

 {HTTP Protocol constants}
 HTTP_PROTOCOL_NONE  = 0;
 HTTP_PROTOCOL_HTTP  = 1;
 HTTP_PROTOCOL_HTTPS = 2;
 
 HTTP_PROTOCOL_STRING_HTTP = 'http';
 HTTP_PROTOCOL_STRING_HTTPS = 'https';
 
 {HTTP Method constants}
 HTTP_METHOD_NONE    = 0;
 HTTP_METHOD_OPTIONS = 1;
 HTTP_METHOD_GET     = 2;
 HTTP_METHOD_HEAD    = 3;
 HTTP_METHOD_POST    = 4;
 HTTP_METHOD_PUT     = 5;
 HTTP_METHOD_DELETE  = 6;
 HTTP_METHOD_TRACE   = 7;
 HTTP_METHOD_CONNECT = 8;

 HTTP_METHOD_STRING_OPTIONS = 'OPTIONS';
 HTTP_METHOD_STRING_GET     = 'GET';
 HTTP_METHOD_STRING_HEAD    = 'HEAD';
 HTTP_METHOD_STRING_POST    = 'POST';
 HTTP_METHOD_STRING_PUT     = 'PUT';
 HTTP_METHOD_STRING_DELETE  = 'DELETE';
 HTTP_METHOD_STRING_TRACE   = 'TRACE';
 HTTP_METHOD_STRING_CONNECT = 'CONNECT';
 
 {HTTP Version constants}
 HTTP_VERSION_00 = 0;
 HTTP_VERSION_10 = 1;
 HTTP_VERSION_11 = 2;

 HTTP_VERSION = HTTP_VERSION_11;
 
 HTTP_VERSION_STRING_10 = 'HTTP/1.0';
 HTTP_VERSION_STRING_11 = 'HTTP/1.1';

 {HTTP Encoding constants}
 HTTP_ENCODING_NONE     = 0;
 HTTP_ENCODING_IDENTITY = 1;
 HTTP_ENCODING_CHUNKED  = 2;
 HTTP_ENCODING_GZIP     = 3;
 HTTP_ENCODING_COMPRESS = 4;
 HTTP_ENCODING_DEFLATE  = 5;
 
 HTTP_ENCODING_STRING_IDENTITY = 'identity';
 HTTP_ENCODING_STRING_CHUNKED  = 'chunked';
 HTTP_ENCODING_STRING_GZIP     = 'gzip';
 HTTP_ENCODING_STRING_COMPRESS = 'compress';
 HTTP_ENCODING_STRING_DEFLATE  = 'deflate';
 
 {HTTP General Header constants}
 HTTP_GENERAL_HEADER_CACHE_CONTROL = 'Cache-Control';             
 HTTP_GENERAL_HEADER_CONNECTION = 'Connection';             
 HTTP_GENERAL_HEADER_DATE = 'Date';             
 HTTP_GENERAL_HEADER_PRAGMA = 'Pragma';             
 HTTP_GENERAL_HEADER_TRAILER = 'Trailer';             
 HTTP_GENERAL_HEADER_TRANSFER_ENCODING = 'Transfer-Encoding';             
 HTTP_GENERAL_HEADER_UPGRADE = 'Upgrade';             
 HTTP_GENERAL_HEADER_VIA = 'Via';             
 HTTP_GENERAL_HEADER_WARNING = 'Warning';           
 
 {HTTP Request Header constants}
 HTTP_REQUEST_HEADER_ACCEPT = 'Accept';             
 HTTP_REQUEST_HEADER_ACCEPT_CHARSET = 'Accept-Charset'; 
 HTTP_REQUEST_HEADER_ACCEPT_ENCODING = 'Accept-Encoding';
 HTTP_REQUEST_HEADER_ACCEPT_LANGUAGE = 'Accept-Language'; 
 HTTP_REQUEST_HEADER_AUTHORIZATION = 'Authorization';   
 HTTP_REQUEST_HEADER_EXPECT = 'Expect';          
 HTTP_REQUEST_HEADER_FROM = 'From';             
 HTTP_REQUEST_HEADER_HOST = 'Host';                
 HTTP_REQUEST_HEADER_IF_MATCH = 'If-Match';            
 HTTP_REQUEST_HEADER_IF_MODIFIED_SINCE = 'If-Modified-Since';    
 HTTP_REQUEST_HEADER_IF_NONE_MATCH = 'If-None-Match';        
 HTTP_REQUEST_HEADER_IF_RANGE = 'If-Range';              
 HTTP_REQUEST_HEADER_IF_UNMODIFIED_SINCE = 'If-Unmodified-Since';   
 HTTP_REQUEST_HEADER_MAX_FORWARDS = 'Max-Forwards';           
 HTTP_REQUEST_HEADER_PROXY_AUTH = 'Proxy-Authorization';    
 HTTP_REQUEST_HEADER_RANGE = 'Range';                  
 HTTP_REQUEST_HEADER_REFERER = 'Referer';                
 HTTP_REQUEST_HEADER_TE = 'TE';                      
 HTTP_REQUEST_HEADER_USER_AGENT = 'User-Agent';
 
 {HTTP Response Header constants}
 HTTP_RESPONSE_HEADER_ACCEPT_RANGES = 'Accept-Ranges';
 HTTP_RESPONSE_HEADER_AGE = 'Age';
 HTTP_RESPONSE_HEADER_ETAG = 'ETag';
 HTTP_RESPONSE_HEADER_LOCATION = 'Location';
 HTTP_RESPONSE_HEADER_PROXY_AUTH = 'Proxy-Authenticate';
 HTTP_RESPONSE_HEADER_RETRY_AFTER = 'Retry-After';
 HTTP_RESPONSE_HEADER_SERVER = 'Server';
 HTTP_RESPONSE_HEADER_VARY = 'Vary';
 HTTP_RESPONSE_HEADER_WWW_AUTHENTICATE = 'WWW-Authenticate';
 
 {HTTP Entity Header constants}
 HTTP_ENTITY_HEADER_ALLOW = 'Allow';
 HTTP_ENTITY_HEADER_CONTENT_ENCODING = 'Content-Encoding';
 HTTP_ENTITY_HEADER_CONTENT_LANGUAGE = 'Content-Language';
 HTTP_ENTITY_HEADER_CONTENT_LENGTH = 'Content-Length';
 HTTP_ENTITY_HEADER_CONTENT_LOCATION = 'Content-Location';
 HTTP_ENTITY_HEADER_CONTENT_MD5 = 'Content-MD5';
 HTTP_ENTITY_HEADER_CONTENT_RANGE = 'Content-Range';
 HTTP_ENTITY_HEADER_CONTENT_TYPE = 'Content-Type';
 HTTP_ENTITY_HEADER_CONTENT_DISPOSITION = 'Content-Disposition';
 HTTP_ENTITY_HEADER_EXPIRES = 'Expires';
 HTTP_ENTITY_HEADER_LAST_MODIFIED = 'Last-Modified';
 
 {HTTP Status constants}
 HTTP_STATUS_NONE                  = 0;
 HTTP_STATUS_CONTINUE              = 100; {Continue}
 HTTP_STATUS_SWITCH_PROTOCOL       = 101; {Switching Protocols}
 HTTP_STATUS_OK                    = 200; {OK}
 HTTP_STATUS_CREATED               = 201; {Created}
 HTTP_STATUS_ACCEPTED              = 202; {Accepted}
 HTTP_STATUS_NON_AUTHORITATIVE     = 203; {Non-Authoritative Information}
 HTTP_STATUS_NO_CONTENT            = 204; {No Content}
 HTTP_STATUS_RESET_CONTENT         = 205; {Reset Content}
 HTTP_STATUS_PARTIAL_CONTENT       = 206; {Partial Content}
 HTTP_STATUS_MULTIPLE_CHOICES      = 300; {Multiple Choices}
 HTTP_STATUS_MOVED_PERMANENT       = 301; {Moved Permanently}
 HTTP_STATUS_FOUND                 = 302; {Found}
 HTTP_STATUS_SEE_OTHER             = 303; {See Other}
 HTTP_STATUS_NOT_MODIFIED          = 304; {Not Modified}
 HTTP_STATUS_USE_PROXY             = 305; {Use Proxy}
 HTTP_STATUS_TEMPORARY_REDIRECT    = 307; {Temporary Redirect}
 HTTP_STATUS_BAD_REQUEST           = 400; {Bad Request}
 HTTP_STATUS_UNAUTHORIZED          = 401; {Unauthorized}
 HTTP_STATUS_PAYMENT_REQUIRED      = 402; {Payment Required}
 HTTP_STATUS_FORBIDDEN             = 403; {Forbidden}
 HTTP_STATUS_NOT_FOUND             = 404; {Not Found}
 HTTP_STATUS_METHOD_NOT_ALLOWED    = 405; {Method Not Allowed}
 HTTP_STATUS_NOT_ACCEPTABLE        = 406; {Not Acceptable}
 HTTP_STATUS_PROXY_AUTH_REQUIRED   = 407; {Proxy Authentication Required}
 HTTP_STATUS_REQUEST_TIMEOUT       = 408; {Request Time-out}
 HTTP_STATUS_CONFLICT              = 409; {Conflict}
 HTTP_STATUS_GONE                  = 410; {Gone}
 HTTP_STATUS_LENGTH_REQUIRED       = 411; {Length Required}
 HTTP_STATUS_PRECONDITION_FAILED   = 412; {Precondition Failed}
 HTTP_STATUS_ENTITY_TOO_LARGE      = 413; {Request Entity Too Large}
 HTTP_STATUS_URI_TOO_LARGE         = 414; {Request-URI Too Large}
 HTTP_STATUS_UNSUPPORTED_MEDIA     = 415; {Unsupported Media Type}
 HTTP_STATUS_RANGE_NOT_SATISFIED   = 416; {Requested range not satisfiable}
 HTTP_STATUS_EXPECTATION_FAILED    = 417; {Expectation Failed}
 HTTP_STATUS_INTERNAL_SERVER_ERROR = 500; {Internal Server Error}
 HTTP_STATUS_NOT_IMPLEMENTED       = 501; {Not Implemented}
 HTTP_STATUS_BAD_GATEWAY           = 502; {Bad Gateway}
 HTTP_STATUS_SERICE_UNAVAILABLE    = 503; {Service Unavailable}
 HTTP_STATUS_GATEWAY_TIMEOUT       = 504; {Gateway Time-out}
 HTTP_STATUS_VERSION_NOT_SUPPORTED = 505; {HTTP Version not supported}
 
 HTTP_MIN_STATUS = 100;
 HTTP_MAX_STATUS = 599;
 
 {HTTP Reason constants}
 HTTP_REASON_100 = 'Continue';
 HTTP_REASON_101 = 'Switching Protocols';
 HTTP_REASON_200 = 'OK';
 HTTP_REASON_201 = 'Created';
 HTTP_REASON_202 = 'Accepted';
 HTTP_REASON_203 = 'Non-Authoritative Information';
 HTTP_REASON_204 = 'No Content';
 HTTP_REASON_205 = 'Reset Content';
 HTTP_REASON_206 = 'Partial Content';
 HTTP_REASON_300 = 'Multiple Choices';
 HTTP_REASON_301 = 'Moved Permanently';
 HTTP_REASON_302 = 'Found';
 HTTP_REASON_303 = 'See Other';
 HTTP_REASON_304 = 'Not Modified';
 HTTP_REASON_305 = 'Use Proxy';
 HTTP_REASON_307 = 'Temporary Redirect';
 HTTP_REASON_400 = 'Bad Request';
 HTTP_REASON_401 = 'Unauthorized';
 HTTP_REASON_402 = 'Payment Required';
 HTTP_REASON_403 = 'Forbidden';
 HTTP_REASON_404 = 'Not Found';
 HTTP_REASON_405 = 'Method Not Allowed';
 HTTP_REASON_406 = 'Not Acceptable';
 HTTP_REASON_407 = 'Proxy Authentication Required';
 HTTP_REASON_408 = 'Request Time-out';
 HTTP_REASON_409 = 'Conflict';
 HTTP_REASON_410 = 'Gone';
 HTTP_REASON_411 = 'Length Required';
 HTTP_REASON_412 = 'Precondition Failed';
 HTTP_REASON_413 = 'Request Entity Too Large';
 HTTP_REASON_414 = 'Request-URI Too Large';
 HTTP_REASON_415 = 'Unsupported Media Type';
 HTTP_REASON_416 = 'Requested range not satisfiable';
 HTTP_REASON_417 = 'Expectation Failed';
 HTTP_REASON_500 = 'Internal Server Error';
 HTTP_REASON_501 = 'Not Implemented';
 HTTP_REASON_502 = 'Bad Gateway';
 HTTP_REASON_503 = 'Service Unavailable';
 HTTP_REASON_504 = 'Gateway Time-out';
 HTTP_REASON_505 = 'HTTP Version not supported ';
 
 {HTTP Content Type constants}
 HTTP_CONTENT_TEXT_HTML = 'text/html';
 
 {HTTP Cache Control constants}
 HTTP_CACHE_NO_CACHE = 'no-cache';

 {HTTP Connection constants}
 HTTP_CONNECTION_CLOSE = 'close';
 HTTP_CONNECTION_KEEPALIVE = 'keep-alive';
 
 {HTTP Request flags}
 HTTP_REQUEST_FLAG_NONE             = $00000000;
 HTTP_REQUEST_FLAG_CONTENT_RECEIVED = $00000001; {Server}
 HTTP_REQUEST_FLAG_LINE_SENT        = $00000002; {Client}
 HTTP_REQUEST_FLAG_HEADERS_SENT     = $00000004; {Client}
 HTTP_REQUEST_FLAG_CONTENT_SENT     = $00000008; {Client}
 
 {HTTP Response flags}
 HTTP_RESPONSE_FLAG_NONE             = $00000000;
 HTTP_RESPONSE_FLAG_STATUS_SENT      = $00000001; {Server}
 HTTP_RESPONSE_FLAG_HEADERS_SENT     = $00000002; {Server}
 HTTP_RESPONSE_FLAG_CONTENT_SENT     = $00000004; {Server}
 HTTP_RESPONSE_FLAG_CONNECTION_CLOSE = $00000008; {Server / Client}
 HTTP_RESPONSE_FLAG_NO_CACHE         = $00000010; {Server / Client}
 HTTP_RESPONSE_FLAG_CONTENT_RECEIVED = $00000020; {Client}
 
 {HTTP Client states}
 HTTP_CLIENT_STATE_NONE     = 0;
 HTTP_CLIENT_STATE_REQUEST  = 1;
 HTTP_CLIENT_STATE_REDIRECT = 2;
 HTTP_CLIENT_STATE_SUCCESS  = 3;
 HTTP_CLIENT_STATE_FAILURE  = 4;
 
 {HTTP Host flags}
 HTTP_HOST_FLAG_NONE    = $00000000;
 HTTP_HOST_FLAG_DEFAULT = $00000001;
 HTTP_HOST_FLAG_DOMAIN  = $00000002;
 
 {HTTP CGI flags}
 HTTP_CGI_FLAG_NONE      = $00000000;
 HTTP_CGI_FLAG_FOLDER    = $00000001;
 HTTP_CGI_FLAG_SUBTREE   = $00000002;
 HTTP_CGI_FLAG_EXTENSION = $00000004;
 
 {HTTP Error flags}
 HTTP_ERROR_FLAG_NONE    = $00000000;
 HTTP_ERROR_FLAG_DEFAULT = $00000001;
  
 {HTTP Module flags}
 HTTP_MODULE_FLAG_NONE    = $00000000;
 
 {HTTP Document flags}
 HTTP_DOCUMENT_FLAG_NONE      = $00000000;
 HTTP_DOCUMENT_FLAG_DEFAULT   = $00000001;
 HTTP_DOCUMENT_FLAG_FOLDER    = $00000002;
 HTTP_DOCUMENT_FLAG_SUBTREE   = $00000004;
 HTTP_DOCUMENT_FLAG_EXTENSION = $00000008;
 
 {HTTP Mime Types (See: Apache mime.types file)}
 HTTP_MIME_TYPE_MAX = 29;
 HTTP_MIME_TYPES:array[0..HTTP_MIME_TYPE_MAX,0..1] of String = (
  ('avi','video/x-msvideo'),
  ('bat','application/x-msdownload'),
  ('bmp','image/bmp'),
  ('com','application/x-msdownload'),
  ('conf','text/plain'),
  ('css','text/css'),
  ('dll','application/x-msdownload'),
  ('doc','application/msword'),
  ('dot','application/msword'),
  ('exe','application/x-msdownload'),
  ('gif','image/gif'),
  ('html','text/html'),
  ('htm','text/html'),
  ('iso','application/octet-stream'),
  ('jpeg','image/jpeg'),
  ('jpg','image/jpeg'),
  ('js','application/javascript'),
  ('pdf','application/pdf'),
  ('png','image/png'),
  ('txt','text/plain'),
  ('text','text/plain'),
  ('log','text/plain'),
  ('mov','video/quicktime'),
  ('mp3','audio/mpeg'),
  ('mp4','video/mp4'),
  ('msi','application/x-msdownload'),
  ('qt','video/quicktime'),
  ('tar','application/x-tar'),
  ('wmv','video/x-ms-wmv'),
  ('zip','application/zip')
  );
 
 HTTP_MIME_TYPE_DEFAULT = 'application/octet-stream';
 
 {HTTP logging}
 HTTP_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {HTTP debugging messages}
 HTTP_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {HTTP informational messages}
 HTTP_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {HTTP warning messages}
 HTTP_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {HTTP error messages}
 HTTP_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No HTTP messages}

var 
 HTTP_DEFAULT_LOG_LEVEL:LongWord = HTTP_LOG_LEVEL_DEBUG; {Minimum level for HTTP messages. Only messages with level greater than or equal to this will be printed} 
 
var 
 {HTTP logging}
 HTTP_LOG_ENABLED:Boolean; 
              
{==============================================================================}
type
 {HTTP specific types}
 THTTPReservedChars = set of AnsiChar;
 
{==============================================================================}
type
 {HTTP specific classes}

 {Helper classes}
 THTTPBuffer = class(TObject)
 public
  constructor Create(ASize:LongWord);
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle;
  
  FData:Pointer;
  FSize:LongWord;
  FCount:LongWord;
  FStart:LongWord;
  
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;

  function GetCount:LongWord;  
 public
  {Public Properties}
  property Count:LongWord read GetCount;
  
  {Public Methods}
  procedure Clear;
  
  function ReadData:Char;
  function WriteData(AChar:Char):Boolean;
  
  function ReadLock(var ASize:LongWord):Pointer;
  function ReadUnlock(ACount:LongWord):Boolean; 
  
  function WriteLock(var ASize:LongWord):Pointer;
  function WriteUnlock(ACount:LongWord):Boolean;
 end;
 
 {Common classes}
 THTTPParam = class(TListObject)
 public
  {}
  constructor Create(const AName:String);
 private
  {Internal Variables}
  FName:String;
  FHash:LongWord;
  FValue:String;
  
 protected
  {Internal Methods}
  
 public
  {Public Properties}
  property Name:String read FName;
  property Hash:LongWord read FHash;
  property Value:String read FValue write FValue;
 end;
 
 THTTPParams = class(TObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FParams:TLinkedObjList;
  
 protected
  {Internal Methods}
 
 public
  {Public Properties}
  
  {Public Methods}
  procedure Clear;
  
  function GetCount:Integer;
  
  function GetParam(APrevious:THTTPParam):THTTPParam;
  function FindParam(const AName:String):THTTPParam;
  
  function AddParam(const AName,AValue:String):Boolean;
  function DeleteParam(const AName:String):Boolean;
 end;
  
 //THTTPCookie //To Do
 //THTTPCookies //To Do
 
 THTTPHeader = class(TListObject)
 public
  {}
  constructor Create(const AName:String);
  destructor Destroy; override;
 private
  {Internal Variables}
  FName:String;
  FHash:LongWord;
  FValues:TLinkedStringList;
  
 protected
  {Internal Methods}
  
 public
  {Public Properties}
  property Name:String read FName;
  property Hash:LongWord read FHash;
  
  {Public Methods}
  function GetCount:Integer;
  
  function GetValue(AIndex:Integer):String;
  function SetValue(AIndex:Integer;const AValue:String):Boolean;
  function FindValue(const AValue:String):Integer;
  
  function AddValue(const AValue:String):Boolean;
  function DeleteValue(const AValue:String):Boolean;
 end;
 
 THTTPHeaders = class(TObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FHeaders:TLinkedObjList;
  
 protected
  {Internal Methods}
 
 public
  {Public Properties}
  
  {Public Methods}
  procedure Clear;
  
  function GetCount:Integer;
  
  function GetHeader(APrevious:THTTPHeader):THTTPHeader;
  function FindHeader(const AName:String):THTTPHeader;
  
  function AddHeader(const AName,AValue:String):Boolean;
  function DeleteHeader(const AName:String):Boolean;
 end;
 
 {Client classes}
 THTTPClient = class;

 THTTPClientRequest = class(TObject)
 public
  {}
  constructor Create(AClient:THTTPClient);
  destructor Destroy; override;
 private
  {Internal Variables}
  FFlags:LongWord;
  
  {Internal Methods}
  function GetLineSent:Boolean;
  function GetHeadersSent:Boolean;
  function GetContentSent:Boolean;
 protected
  {Internal Variables}
  FClient:THTTPClient;
 
  {Internal Methods}
  
 public
  {Request Properties}
  URL:String;     
  Method:LongWord;
  Version:LongWord;
  
  Params:THTTPParams;
  //Cookies //To Do
  Headers:THTTPHeaders;
 
  Protocol:String;
  Host:String;
  Port:String;
  Path:String;
  Query:String;
  
  ContentStream:TStream;
  ContentString:String;
  
  RedirectCount:LongWord;
  
  {Public Properties}
  property Flags:LongWord read FFlags;
  property Client:THTTPClient read FClient;
 
  property LineSent:Boolean read GetLineSent;
  property HeadersSent:Boolean read GetHeadersSent;
  property ContentSent:Boolean read GetContentSent;
 
  {Public Methods}
  function Close:Boolean;
  function Clear:Boolean;
  
  function SetParam(const AName,AValue:String):Boolean;
  function SetParamEx(const AName,AValue:String;AReplace:Boolean):Boolean;
  
  function SetHeader(const AName,AValue:String):Boolean;
  function SetHeaderEx(const AName,AValue:String;AReplace:Boolean):Boolean;

  function WriteRequest:Boolean;
  
  function WriteLine:Boolean;
  function WriteHeaders:Boolean;
  function WriteContentStream(AContent:TStream;ASize:LongWord;ACompleted:Boolean):Boolean;
  function WriteContentString(const AContent:String;ASize:LongWord;ACompleted:Boolean):Boolean;
 end;
  
 THTTPClientResponse = class(TObject)
 public
  {}
  constructor Create(AClient:THTTPClient);
  destructor Destroy; override;
 private
  {Internal Variables}
  FFlags:LongWord;
  
  {Internal Methods}
  function GetContentReceived:Boolean;
  
  function GetNoCache:Boolean;
  procedure SetNoCache(ANoCache:Boolean);
  function GetConnectionClose:Boolean;
  procedure SetConnectionClose(AConnectionClose:Boolean);
 protected
  {Internal Variables}
  FClient:THTTPClient;
  
  {Internal Methods}
 
 public
  {Response Properties}
  Reason:String;
  Status:LongWord;
  Version:LongWord;
  
  //Cookies //To Do
  Headers:THTTPHeaders;
  
  {Public Properties}
  property Flags:LongWord read FFlags;
  property Client:THTTPClient read FClient;

  property ContentReceived:Boolean read GetContentReceived;

  property NoCache:Boolean read GetNoCache write SetNoCache;
  property ConnectionClose:Boolean read GetConnectionClose write SetConnectionClose;
  
  {Public Methods}
  function Clear:Boolean;
  
  function GetHeader(const AName:String):String;
  function GetHeaderEx(const AName:String):TStringList;
  
  function FindHeader(const AName,AValue:String):Boolean;
  
  function ReadContentStream(AContent:TStream;ASize:LongWord):Boolean;
  function ReadContentString(var AContent:String;ASize:LongWord):Boolean;
 end;
  
 THTTPRedirectEvent = function(AClient:THTTPClient;const AURL:String;var ALocation:String):Boolean of Object;
 THTTPPasswordEvent = function(AClient:THTTPClient;var AUsername,APassword:String):Boolean of Object;
 
 THTTPClient = class(TWinsock2TCPClient)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle; 
  
  FState:LongWord;            {Internal request state}
  FBuffer:THTTPBuffer;        {Buffer for received data}
  
  FRequest:THTTPClientRequest;
  FResponse:THTTPClientResponse;

  FTimeout:LongWord;
  FUserAgent:String;
  FAllowRedirect:Boolean;
  FMaxRedirects:LongWord;
  FKeepAlive:Boolean;
  FKeepAliveTimeout:LongWord;
  
  FOnRedirect:THTTPRedirectEvent;
  FOnPassword:THTTPPasswordEvent;
  
  {Host Variables}
  FHost:String;
  FPort:String;
  FUsername:String;
  FPassword:String;
  
  {Proxy Variables}
  FProxyHost:String;
  FProxyPort:String;
  FProxyUsername:String;
  FProxyPassword:String;
  
  {Request Variables}
  FURL:String;
  FMethod:LongWord;
  FVersion:LongWord;
  FMimeType:String;
  FRangeStart:LongWord;
  FRangeEnd:LongWord;
  FEncoding:LongWord;
  
  {Response Variables}
  
  {Internal Methods}
  procedure SetTimeout(ATimeout:LongWord);
  function GetUserAgent:String;
  procedure SetUserAgent(const AUserAgent:String);
  procedure SetAllowRedirect(AAllowRedirect:Boolean);
  procedure SetMaxRedirects(AMaxRedirects:LongWord);
  procedure SetKeepAlive(AKeepAlive:Boolean);
  procedure SetKeepAliveTimeout(AKeepAliveTimeout:LongWord);
  
  function GetHost:String;
  procedure SetHost(const AHost:String);
  function GetPort:String;
  procedure SetPort(const APort:String);
  function GetUsername:String;
  procedure SetUsername(const AUsername:String);
  function GetPassword:String;
  procedure SetPassword(const APassword:String);
  
  function GetProxyHost:String;
  procedure SetProxyHost(const AProxyHost:String);
  function GetProxyPort:String;
  procedure SetProxyPort(const AProxyPort:String);
  function GetProxyUsername:String;
  procedure SetProxyUsername(const AProxyUsername:String);
  function GetProxyPassword:String;
  procedure SetProxyPassword(const AProxyPassword:String);
  
  function GetURL:String;
  procedure SetURL(const AURL:String);
  procedure SetMethod(AMethod:LongWord);
  procedure SetVersion(AVersion:LongWord);
  function GetMimeType:String;
  procedure SetMimeType(const AMimeType:String);
  procedure SetRangeStart(ARangeStart:LongWord);
  procedure SetRangeEnd(ARangeEnd:LongWord);
  procedure SetEncoding(AEncoding:LongWord);
  
  function GetResponseStatus:LongWord;
  function GetResponseReason:String;
  function GetResponseVersion:LongWord;
  function GetResponseMimeType:String;
  function GetResponseEncoding:LongWord;
  function GetResponseContentSize:LongWord;
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  function DoRedirect(const AURL:String;var ALocation:String):Boolean;
  function DoPassword(var AUsername,APassword:String):Boolean;
  
  function ReadResponseLine(AResponse:THTTPClientResponse):Boolean;
  function ReadResponseHeaders(AResponse:THTTPClientResponse):Boolean;
  function ReadResponseContentStream(AResponse:THTTPClientResponse;AContent:TStream;ASize:LongWord):Boolean;
  function ReadResponseContentString(AResponse:THTTPClientResponse;var AContent:String;ASize:LongWord):Boolean;
  
  function WriteRequestLine(ARequest:THTTPClientRequest):Boolean;
  function WriteRequestHeaders(ARequest:THTTPClientRequest):Boolean;
  function WriteRequestContentStream(ARequest:THTTPClientRequest;AContent:TStream;ASize:LongWord):Boolean;
  function WriteRequestContentString(ARequest:THTTPClientRequest;const AContent:String;ASize:LongWord):Boolean;
 public
  {Public Properties}
  property Request:THTTPClientRequest read FRequest;
  property Response:THTTPClientResponse read FResponse;
  
  property Timeout:LongWord read FTimeout write SetTimeout;
  property UserAgent:String read GetUserAgent write SetUserAgent;
  property AllowRedirect:Boolean read FAllowRedirect write SetAllowRedirect;
  property MaxRedirects:LongWord read FMaxRedirects write SetMaxRedirects;
  property KeepAlive:Boolean read FKeepAlive write SetKeepAlive;
  property KeepAliveTimeout:LongWord read FKeepAliveTimeout write SetKeepAliveTimeout;
  
  property OnRedirect:THTTPRedirectEvent read FOnRedirect write FOnRedirect;
  property OnPassword:THTTPPasswordEvent read FOnPassword write FOnPassword;
  
  {Host Properties}
  property Host:String read GetHost write SetHost;
  property Port:String read GetPort write SetPort;
  property Username:String read GetUsername write SetUsername;
  property Password:String read GetPassword write SetPassword;
  
  {Proxy Properties}
  property ProxyHost:String read GetProxyHost write SetProxyHost;
  property ProxyPort:String read GetProxyPort write SetProxyPort;
  property ProxyUsername:String read GetProxyUsername write SetProxyUsername;
  property ProxyPassword:String read GetProxyPassword write SetProxyPassword;
  
  {Request Properties}
  property RequestURL:String read GetURL write SetURL;
  property RequestMethod:LongWord read FMethod write SetMethod;
  property RequestVersion:LongWord read FVersion write SetVersion;
  property RequestMimeType:String read GetMimeType write SetMimeType;
  property RequestRangeStart:LongWord read FRangeStart write SetRangeStart;
  property RequestRangeEnd:LongWord read FRangeEnd write SetRangeEnd;
  property RequestEncoding:LongWord read FEncoding write SetEncoding;
  
  {Response Properties}
  property ResponseStatus:LongWord read GetResponseStatus;
  property ResponseReason:String read GetResponseReason;
  property ResponseVersion:LongWord read GetResponseVersion;
  property ResponseMimeType:String read GetResponseMimeType;
  property ResponseEncoding:LongWord read GetResponseEncoding;
  property ResponseContentSize:LongWord read GetResponseContentSize;
  
  {Public Methods}
  function Head(const AURL:String):Boolean;
  
  function GetString(const AURL:String;var AContent:String):Boolean;
  function GetStream(const AURL:String;AContent:TStream):Boolean;
  
  //To Do //PostForm/PostMultipart etc
  function PostString(const AURL:String;var AContent:String):Boolean;
  function PostStream(const AURL:String;AContent:TStream):Boolean;
  
  function SendRequest:Boolean;
  function CloseRequest(Close:Boolean):Boolean;
  function ClearRequest:Boolean;
  function CancelRequest:Boolean;
  
  {Request Methods}
  function SetRequestParam(const AName,AValue:String):Boolean;
  function SetRequestParamEx(const AName,AValue:String;AReplace:Boolean):Boolean;

  //To Do //Cookies
  
  function SetRequestHeader(const AName,AValue:String):Boolean;
  function SetRequestHeaderEx(const AName,AValue:String;AReplace:Boolean):Boolean;
  
  function SetRequestContentStream(AContent:TStream):Boolean;
  function AddRequestContentString(const AContent:String):Boolean;
  function SetRequestContentString(const AContent:String):Boolean;
  
  {Response Methods}
  //To Do //Cookies
  
  function GetResponseHeader(const AName:String):String;
  function GetResponseHeaderEx(const AName:String):TStringList;
  
  function GetResponseContentStream(AContent:TStream;ASize:LongWord):Boolean;
  function GetResponseContentString(var AContent:String;ASize:LongWord):Boolean;
 end;  
 
 {Server classes}
 THTTPListener = class;
 
 THTTPServerRequest = class(TObject)
 public
  {}
  constructor Create(AThread:TWinsock2TCPServerThread);
  destructor Destroy; override;
 private
  {Internal Variables}
  FFlags:LongWord;
  FThread:TWinsock2TCPServerThread;
  
  {Internal Methods}
  function GetContentReceived:Boolean;
 protected
  {Internal Variables}
  FListener:THTTPListener;
  
  {Internal Methods}
  
 public
  {Request Properties}
  URL:String;     
  Method:LongWord;
  Version:LongWord;
 
  Params:THTTPParams;
  Headers:THTTPHeaders;
   
  Protocol:String;
  Host:String;
  Port:String;
  Path:String;
  Query:String;
  
  BasePath:String;       {The base path of the document or alias matched to this request}
  BaseHost:String;       {The base name of the host or alias matched to this request}
  
  {Public Properties}
  property Flags:LongWord read FFlags;
  property Thread:TWinsock2TCPServerThread read FThread;
  
  property ContentReceived:Boolean read GetContentReceived;
 
  {Public Methods}
  function GetParam(const AName:String):String;
  function GetHeader(const AName:String):String;
  function GetHeaderEx(const AName:String):TStringList;
  
  function FindHeader(const AName,AValue:String):Boolean;
  
  function ReadContentStream(AContent:TStream;ASize:LongWord):Boolean;
  function ReadContentString(var AContent:String;ASize:LongWord):Boolean;
 end;
 
 THTTPServerResponse = class(TObject)
 public
  {}
  constructor Create(AThread:TWinsock2TCPServerThread);
  destructor Destroy; override;
 private
  {Internal Variables}
  FFlags:LongWord;
  FThread:TWinsock2TCPServerThread;
  
  {Internal Methods}
  function GetStatusSent:Boolean;
  function GetHeadersSent:Boolean;
  function GetContentSent:Boolean;
  
  function GetNoCache:Boolean;
  procedure SetNoCache(ANoCache:Boolean);
  function GetConnectionClose:Boolean;
  procedure SetConnectionClose(AConnectionClose:Boolean);
 protected
  {Internal Variables}
  FListener:THTTPListener;
  
  {Internal Methods}
 
 public
  {Response Properties}
  Reason:String;
  Status:LongWord;
  Version:LongWord;
  
  Headers:THTTPHeaders;
  
  ContentStream:TStream;
  ContentString:String;
  
  {Public Properties}
  property Flags:LongWord read FFlags;
  property Thread:TWinsock2TCPServerThread read FThread;
  
  property StatusSent:Boolean read GetStatusSent;
  property HeadersSent:Boolean read GetHeadersSent;
  property ContentSent:Boolean read GetContentSent;
  
  property NoCache:Boolean read GetNoCache write SetNoCache;
  property ConnectionClose:Boolean read GetConnectionClose write SetConnectionClose;
  
  {Public Methods}
  function SetHeader(const AName,AValue:String):Boolean;
  function SetHeaderEx(const AName,AValue:String;AReplace:Boolean):Boolean;
  
  function WriteResponse:Boolean;
  
  function WriteStatus:Boolean;
  function WriteHeaders:Boolean;
  function WriteContentStream(AContent:TStream;ASize:LongWord;ACompleted:Boolean):Boolean;
  function WriteContentString(const AContent:String;ASize:LongWord;ACompleted:Boolean):Boolean;
 end;
 
 //THTTPVariable = class() //To Do //For CGI style interface
 //THTTPVariables = class() //To Do //For CGI style interface
 
 //THTTPCgi = class()  //To Do //For CGI style interface
 
 THTTPHost = class;
 THTTPError = class;
 THTTPAlias = class;
 THTTPModule = class;
 THTTPDocument = class;
 THTTPMimeType = class;
 
 THTTPHostEvent = function(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean of Object;
 THTTPErrorEvent = function(AHost:THTTPHost;AError:THTTPError;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean of Object;
 THTTPModuleEvent = function(AHost:THTTPHost;AModule:THTTPModule;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean of Object;
 THTTPDocumentEvent = function(AHost:THTTPHost;ADocument:THTTPDocument;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean of Object;
 
 THTTPHost = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle; //To Do //Make this a Reader/Writer for performance ? //Also add a LocalLock ?
  
  FName:String;
  FHash:LongWord;
  FFlags:LongWord;
  
  FDefaultMimeType:String;
  
  FErrors:TLinkedList;
  FAliases:TLinkedList;
  FModules:TLinkedList;
  FDocuments:TLinkedList;
  FMimeTypes:TLinkedList;
  
  FOnGet:THTTPHostEvent;
  FOnHead:THTTPHostEvent;
  FOnPost:THTTPHostEvent;
  FOnPut:THTTPHostEvent;
  FOnError:THTTPHostEvent;
  
  FOnRequest:THTTPHostEvent;
  FOnResponse:THTTPHostEvent;
  
  {Internal Methods}
  function GetName:String;
  procedure SetName(const AName:String);
  procedure SetFlags(AFlags:LongWord);

  function GetDefaultMimeType:String;
  procedure SetDefaultMimeType(const ADefaultMimeType:String);
  
  function GetIsDefault:Boolean;
  procedure SetIsDefault(AIsDefault:Boolean);
  function GetIsDomain:Boolean;
  procedure SetIsDomain(AIsDomain:Boolean);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  function MatchError(AStatus:LongWord):THTTPError; virtual;
  function MatchDocument(const AName:String;var AAlias:THTTPAlias):THTTPDocument; virtual;
  
  function DoGet(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoHead(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoPost(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoPut(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoError(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  
  function DoRequest(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoResponse(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
 public
  {Public Properties}
  property Name:String read GetName write SetName;
  property Hash:LongWord read FHash;
  property Flags:LongWord read FFlags write SetFlags;
  
  property IsDefault:Boolean read GetIsDefault write SetIsDefault;
  property IsDomain:Boolean read GetIsDomain write SetIsDomain;
  
  property DefaultMimeType:String read GetDefaultMimeType write SetDefaultMimeType;
  
  property OnGet:THTTPHostEvent read FOnGet write FOnGet;
  property OnHead:THTTPHostEvent read FOnHead write FOnHead;
  property OnPost:THTTPHostEvent read FOnPost write FOnPost;
  property OnPut:THTTPHostEvent read FOnPut write FOnPut;
  property OnError:THTTPHostEvent read FOnError write FOnError;

  property OnRequest:THTTPHostEvent read FOnRequest write FOnRequest;
  property OnResponse:THTTPHostEvent read FOnResponse write FOnResponse;
  
  {Public Methods}
  function FindError(AStatus:LongWord):THTTPError;
  
  function RegisterError(AError:THTTPError):Boolean;
  function DeregisterError(AError:THTTPError):Boolean;
  
  function FindAlias(const AName:String):THTTPAlias;
  function MatchAlias(const AName:String):THTTPAlias; virtual;
  
  function RegisterAlias(AAlias:THTTPAlias):Boolean;
  function DeregisterAlias(AAlias:THTTPAlias):Boolean;
  
  function GetModule(APrevious:THTTPModule):THTTPModule;
  function FindModule(AModule:THTTPModule):THTTPModule;
  
  function RegisterModule(AModule:THTTPModule):Boolean;
  function DeregisterModule(AModule:THTTPModule):Boolean;
  
  function FindDocument(const AName:String):THTTPDocument;
  
  function RegisterDocument(ADocument:THTTPDocument):Boolean;
  function DeregisterDocument(ADocument:THTTPDocument):Boolean;
  
  function LoadMimeTypes:Boolean;
  
  function FindMimeType(const AExtension:String):THTTPMimeType;
  function ResolveMimeType(const AExtension:String):String;
  
  function RegisterMimeType(AMimeType:THTTPMimeType):Boolean;
  function DeregisterMimeType(AMimeType:THTTPMimeType):Boolean;
 end;

 THTTPError = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle; //To Do //Make this a Reader/Writer for performance ? //Also add a LocalLock ?
 
  FFlags:LongWord;
  FStatus:LongWord;
  
  FOnError:THTTPErrorEvent;
 
  {Internal Methods}
  procedure SetFlags(AFlags:LongWord);
  procedure SetStatus(AStatus:LongWord);
  
  function GetIsDefault:Boolean;
  procedure SetIsDefault(AIsDefault:Boolean);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 
  function DoError(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
 public
  {Public Properties}
  property Flags:LongWord read FFlags write SetFlags;
  property Status:LongWord read FStatus write SetStatus;
  
  property IsDefault:Boolean read GetIsDefault write SetIsDefault;
  
  property OnError:THTTPErrorEvent read FOnError write FOnError;
 end;
 
 THTTPAlias = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TMutexHandle;
  
  FName:String;
  FHash:LongWord;
 
  {Internal Methods}
  function GetName:String;
  procedure SetName(const AName:String);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  {Public Properties}
  property Name:String read GetName write SetName;
  property Hash:LongWord read FHash;
 end;
 
 THTTPModule = class(TListObject) 
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle; //To Do //Make this a Reader/Writer for performance ? //Also add a LocalLock ?
  
  FFlags:LongWord;
 
  FOnRequest:THTTPModuleEvent;
  FOnResponse:THTTPModuleEvent;

  {Internal Methods}
  procedure SetFlags(AFlags:LongWord);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 
  function DoRequest(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoResponse(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
 public
  {Public Properties}
  property Flags:LongWord read FFlags write SetFlags;
  
  property OnRequest:THTTPModuleEvent read FOnRequest write FOnRequest;
  property OnResponse:THTTPModuleEvent read FOnResponse write FOnResponse;
 end;
 
 THTTPDocument = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle; //To Do //Make this a Reader/Writer for performance ? //Also add a LocalLock ?
  
  FName:String;
  FHash:LongWord;
  FFlags:LongWord;
  
  FAliases:TLinkedList;
  
  FOnGet:THTTPDocumentEvent;
  FOnHead:THTTPDocumentEvent;
  FOnPost:THTTPDocumentEvent;
  FOnPut:THTTPDocumentEvent;
  
  {Internal Methods}
  function GetName:String;
  procedure SetName(const AName:String);
  procedure SetFlags(AFlags:LongWord);
  
  function GetIsDefault:Boolean;
  procedure SetIsDefault(AIsDefault:Boolean);
  function GetIsFolder:Boolean;
  procedure SetIsFolder(AIsFolder:Boolean);
  function GetIsSubtree:Boolean;
  procedure SetIsSubtree(AIsSubtree:Boolean);
  function GetIsExtension:Boolean;
  procedure SetIsExtension(AIsExtension:Boolean);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoHead(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoPost(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
  function DoPut(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; virtual;
 public
  {Public Properties}
  property Name:String read GetName write SetName;
  property Hash:LongWord read FHash;
  property Flags:LongWord read FFlags write SetFlags;
  
  property IsDefault:Boolean read GetIsDefault write SetIsDefault;
  property IsFolder:Boolean read GetIsFolder write SetIsFolder;
  property IsSubtree:Boolean read GetIsSubtree write SetIsSubtree;
  property IsExtension:Boolean read GetIsExtension write SetIsExtension;
  
  property OnGet:THTTPDocumentEvent read FOnGet write FOnGet;
  property OnHead:THTTPDocumentEvent read FOnHead write FOnHead;
  property OnPost:THTTPDocumentEvent read FOnPost write FOnPost;
  property OnPut:THTTPDocumentEvent read FOnPut write FOnPut;
  
  {Public Methods}
  function FindAlias(const AName:String):THTTPAlias;
  function MatchAlias(const AName:String):THTTPAlias; virtual;
  
  function RegisterAlias(AAlias:THTTPAlias):Boolean;
  function DeregisterAlias(AAlias:THTTPAlias):Boolean;
 end;
 
 THTTPMimeType = class(TListObject)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TMutexHandle;
  
  FExtension:String;
  FHash:LongWord;
  FMimeType:String;
  
  {Internal Methods}
  function GetExtension:String;
  procedure SetExtension(const AExtension:String);
  function GetMimeType:String;
  procedure SetMimeType(const AMimeType:String);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  {Public Properties}
  property Extension:String read GetExtension write SetExtension;
  property Hash:LongWord read FHash;
  property MimeType:String read GetMimeType write SetMimeType;
 end;
 
 THTTPRedirect = class(THTTPDocument)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FLocation:String;
  FPermanent:Boolean;
  
  {Internal Methods}
  function GetLocation:String;
  procedure SetLocation(const ALocation:String);
  procedure SetPermanent(APermanent:Boolean);
 protected
  {Internal Methods}
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
  property Location:String read GetLocation write SetLocation;
  property Permanent:Boolean read FPermanent write SetPermanent;
 end;
 
 THTTPFolder = class(THTTPDocument)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FFolder:String;
  FIndexPage:String;
  FAllowCache:Boolean;
  FAllowListing:Boolean;
  FAllowSubtree:Boolean;
  
  FHideSubfolders:Boolean;
  FForceTrailingSlash:Boolean;
  
  {Internal Methods}
  function GetFolder:String;
  procedure SetFolder(const AFolder:String);
  function GetIndexPage:String;
  procedure SetIndexPage(const AIndexPage:String);
  procedure SetAllowCache(AAllowCache:Boolean);
  procedure SetAllowListing(AAllowListing:Boolean);
  procedure SetAllowSubtree(AAllowSubtree:Boolean);

  procedure SetHideSubfolders(AHideSubfolders:Boolean);
  procedure SetForceTrailingSlash(AForceTrailingSlash:Boolean);
 protected
  {Internal Methods}
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  function DoHead(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  
  function DoGetFile(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse;const AFile:String):Boolean; virtual;
  function DoGetFolder(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse;const AFolder:String):Boolean; virtual;
 public
  {Public Properties}
  property Folder:String read GetFolder write SetFolder;
  property IndexPage:String read GetIndexPage write SetIndexPage;
  property AllowCache:Boolean read FAllowCache write SetAllowCache;
  property AllowListing:Boolean read FAllowListing write SetAllowListing;
  property AllowSubtree:Boolean read FAllowSubtree write SetAllowSubtree;
  
  property HideSubfolders:Boolean read FHideSubfolders write SetHideSubfolders;             {If not AllowSubtree then don't list sub folders of the configured folder}
  property ForceTrailingSlash:Boolean read FForceTrailingSlash write SetForceTrailingSlash; {If a folder request doesn't contain a trailing slash then redirect to add it}
 end;

 THTTPFile = class(THTTPDocument)
 public
  {}
  constructor Create;
 private
  {Internal Variables}
  FFilename:String;
  FAllowCache:Boolean;
  
  {Internal Methods}
  function GetFilename:String;
  procedure SetFilename(const AFilename:String);
  procedure SetAllowCache(AAllowCache:Boolean);
 protected
  {Internal Methods}
  function DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
  function DoHead(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; override;
 public
  {Public Properties}
  property Filename:String read GetFilename write SetFilename;
  property AllowCache:Boolean read FAllowCache write SetAllowCache;
 end;
 
 THTTPListener = class(TWinsock2TCPListener)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {Internal Variables}
  FLock:TCriticalSectionHandle; //To Do //Make this a Reader/Writer for performance ?
  
  FHost:THTTPHost;     {Default Host}
  FHosts:TLinkedList;

  FServer:String;
  
  {Internal Methods}
  function GetServer:String;
  procedure SetServer(const AServer:String);
 protected
  {Internal Methods}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;

  procedure DoConnect(AThread:TWinsock2TCPServerThread); override;
  procedure DoDisconnect(AThread:TWinsock2TCPServerThread); override;
  
  function DoExecute(AThread:TWinsock2TCPServerThread):Boolean; override;
  
  //To Do //Need DoCreateThread to override TWinsock2TCPServerThread with THTTPWorkerThread/THTTPServerThread (with Preallocated Request/Response ?)
  
  function MatchHost(const AName:String;var AAlias:THTTPAlias):THTTPHost;
  
  function GetRequestLine(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest):Boolean;
  function GetRequestHeaders(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest):Boolean;
  function GetRequestContentStream(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest;AContent:TStream;ASize:LongWord):Boolean;
  function GetRequestContentString(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest;var AContent:String;ASize:LongWord):Boolean;
  
  function SendResponseLine(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse):Boolean;
  function SendResponseHeaders(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse):Boolean;
  function SendResponseContentStream(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse;AContent:TStream;ASize:LongWord):Boolean;
  function SendResponseContentString(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse;const AContent:String;ASize:LongWord):Boolean;
 public
  {Public Properties}
  property Server:String read GetServer write SetServer;
  
  {Public Methods}
  function FindHost(const AName:String):THTTPHost;
  
  function RegisterHost(AHost:THTTPHost):Boolean;
  function DeregisterHost(AHost:THTTPHost):Boolean;
  
  function FindError(const AHost:String;AStatus:LongWord):THTTPError;
  
  function RegisterError(const AHost:String;AError:THTTPError):Boolean;
  function DeregisterError(const AHost:String;AError:THTTPError):Boolean;
  
  function GetModule(const AHost:String;APrevious:THTTPModule):THTTPModule;
  function FindModule(const AHost:String;AModule:THTTPModule):THTTPModule;
  
  function RegisterModule(const AHost:String;AModule:THTTPModule):Boolean;
  function DeregisterModule(const AHost:String;AModule:THTTPModule):Boolean;
  
  function FindDocument(const AHost,AName:String):THTTPDocument;
  
  function RegisterDocument(const AHost:String;ADocument:THTTPDocument):Boolean;
  function DeregisterDocument(const AHost:String;ADocument:THTTPDocument):Boolean;
  
  function LoadMimeTypes(const AHost:String):Boolean;
  
  function FindMimeType(const AHost,AExtension:String):THTTPMimeType;
  function ResolveMimeType(const AHost,AExtension:String):String;
  
  function RegisterMimeType(const AHost:String;AMimeType:THTTPMimeType):Boolean;
  function DeregisterMimeType(const AHost:String;AMimeType:THTTPMimeType):Boolean;
 end;
  
{==============================================================================}
{var}
 {HTTP specific variables}

const
 HTTPReservedURLChars:THTTPReservedChars = [#$00..#$20,'_','<','>','"','%','{','}','|','\','^','~','[',']','`',#$7F..#$FF];
 HTTPReservedElementChars:THTTPReservedChars = [#$00..#$20,';','/','?',':','@','=','&','#','+','_','<','>','"','%','{','}','|','\','^','~','[',']','`',#$7F..#$FF];
 
{==============================================================================}
{Initialization Functions}
procedure HTTPInit;

{==============================================================================}
{HTTP Functions}

{==============================================================================}
{HTTP Helper Functions}
function HTTPEncode(const AValue:String;AReserved:THTTPReservedChars):String;
function HTTPDecode(const AValue:String;AQuery:Boolean = False):String;

function HTTPParseURI(const AURI:String;var AProtocol,AHost,APort,APath,AQuery:String):Boolean;
function HTTPBuildURI(const AProtocol,AHost,APort,APath,AQuery:String;var AURI:String):Boolean;

function HTTPParseHost(const AHost:String;var AName,APort:String):Boolean;
function HTTPBuildHost(const AName,APort:String;var AHost:String):Boolean;

function HTTPParseParam(const AParam:String;var AName,AValue:String):Boolean;
function HTTPBuildParam(const AName,AValue:String;var AParam:String):Boolean;

function HTTPParseQuery(const AQuery:String;AParams:THTTPParams):Boolean;
function HTTPBuildQuery(AParams:THTTPParams;var AQuery:String):Boolean;

//To Do //HTTPParseCookie/HTTPBuildCookie

function HTTPParseHeader(const AHeader:String;var AName,AValue:String):Boolean;
function HTTPBuildHeader(const AName,AValue:String;var AHeader:String):Boolean;

function HTTPPathExtractName(const ASource:String;var AName:String):Boolean;
function HTTPPathExtractDir(const ASource:String;var APath:String):Boolean;
function HTTPPathExtractPath(const ASource:String;var APath:String):Boolean;
function HTTPPathExtractExtension(const ASource:String;var AExtension:String):Boolean;

function HTTPParseRequestLine(const ARequest:String;var AMethod:LongWord;var AURL:String;var AVersion:LongWord):Boolean;
function HTTPBuildRequestLine(AMethod:LongWord;const AURL:String;AVersion:LongWord;var ARequest:String):Boolean;

function HTTPParseResponseLine(const AResponse:String;var AVersion,AStatus:LongWord;var AReason:String):Boolean;
function HTTPBuildResponseLine(AVersion,AStatus:LongWord;const AReason:String;var AResponse:String):Boolean;

function FileTimeToHTTPDate(const AFileTime:TFileTime):String;
function HTTPDateToFileTime(const AHTTPDate:String):TFileTime;

function DateTimeToHTTPDate(const ADateTime:TDateTime):String;
function HTTPDateToDateTime(const AHTTPDate:String):TDateTime;

function StringToHTTPProtocol(const AProtocol:String):LongWord;
function HTTPProtocolToString(AProtocol:LongWord):String;

function StringToHTTPMethod(const AMethod:String):LongWord;
function HTTPMethodToString(AMethod:LongWord):String;

function StringToHTTPVersion(const AVersion:String):LongWord;
function HTTPVersionToString(AVersion:LongWord):String;

function StringToHTTPEncoding(const AEncoding:String):LongWord;
function HTTPEncodingToString(AEncoding:LongWord):String;

function StringToHTTPStatus(const AStatus:String):LongWord;
function HTTPStatusToString(AStatus:LongWord):String;

procedure HTTPLog(Level:LongWord;const AText:String);
procedure HTTPLogInfo(const AText:String); inline;
procedure HTTPLogWarn(const AText:String); inline;
procedure HTTPLogError(const AText:String); inline;
procedure HTTPLogDebug(const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HTTP specific variables}
 HTTPInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{THTTPBuffer}
constructor THTTPBuffer.Create(ASize:LongWord);
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FData:=nil;
 FSize:=ASize;
 FCount:=0;
 FStart:=0;
 
 if FSize <> 0 then FData:=GetMem(FSize);
end;

{==============================================================================}

destructor THTTPBuffer.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FData <> nil then FreeMem(FData);
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function THTTPBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPBuffer.GetCount:LongWord;  
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 
 Result:=FCount;
 
 ReleaseLock; 
end;

{==============================================================================}

procedure THTTPBuffer.Clear;
begin
 {}
 if not AcquireLock then Exit;

 FCount:=0;
 FStart:=0;
 
 ReleaseLock; 
end;
 
{==============================================================================}

function THTTPBuffer.ReadData:Char;
begin
 {}
 Result:=#0;
 
 if not AcquireLock then Exit;
 try
  if FCount > 0 then
   begin
    {Read Char}
    Result:=Char(Pointer(PtrUInt(FData) + PtrUInt(FStart))^);
    
    {Update Start}
    FStart:=(FStart + 1) mod FSize;

    {Update Count}
    Dec(FCount);
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPBuffer.WriteData(AChar:Char):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if FCount < FSize then
   begin
    {Write Char}
    Char(Pointer(PtrUInt(FData) + PtrUInt((FStart + FCount) mod FSize))^):=AChar;
    
    {Update Count}
    Inc(FCount);
    
    Result:=True;
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPBuffer.ReadLock(var ASize:LongWord):Pointer;
begin
 {}
 {Setup Result}
 ASize:=0;
 Result:=nil;
 
 if not AcquireLock then Exit;

 if FCount > 0 then
  begin
   {Check Wraparound}
   if (FStart + FCount) > FSize then
    begin  
     {Get Size}    
     ASize:=FCount - ((FStart + FCount) mod FSize);
     
     {Get Data}
     Result:=Pointer(PtrUInt(FData) + PtrUInt(FStart));
    end
   else
    begin
     {Get Size}
     ASize:=FCount;
     
     {Get Data}
     Result:=Pointer(PtrUInt(FData) + PtrUInt(FStart));
    end;
  end
 else
  begin
   ReleaseLock;
  end; 
end;

{==============================================================================}

function THTTPBuffer.ReadUnlock(ACount:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 if ACount <= FCount then
  begin
   {Update Start}
   FStart:=(FStart + ACount) mod FSize;

   {Update Count}
   Dec(FCount,ACount);
   
   Result:=True;
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPBuffer.WriteLock(var ASize:LongWord):Pointer;
{Lock the buffer and return a pointer to the next write}
begin
 {}
 {Setup Result}
 ASize:=0;
 Result:=nil;

 if not AcquireLock then Exit;
 
 if FCount < FSize then
  begin
   {Check Wraparound}
   if (FStart + FCount) >= FSize then
    begin
     {Get Size}
     ASize:=FStart - ((FStart + FCount) mod FSize);
     
     {Get Data}
     Result:=Pointer(PtrUInt(FData) + PtrUInt((FStart + FCount) mod FSize));
    end
   else
    begin
     {Get Size}
     ASize:=FSize - (FStart + FCount);
     
     {Get Data}
     Result:=Pointer(PtrUInt(FData) + PtrUInt((FStart + FCount) mod FSize));
    end;    
  end
 else
  begin
   ReleaseLock;
  end;  
end;

{==============================================================================}

function THTTPBuffer.WriteUnlock(ACount:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if (FCount + ACount) <= FSize then
  begin
   {Update Count}
   Inc(FCount,ACount);
   
   Result:=True;
  end;
  
 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{THTTPParam}
constructor THTTPParam.Create(const AName:String);
begin
 {}
 inherited Create;
 FName:=AName;
 FHash:=GenerateNameHash(FName,stringHashSize);
 FValue:='';
end;

{==============================================================================}
{==============================================================================}
{THTTPParams}
constructor THTTPParams.Create;
begin
 {}
 inherited Create;
 FParams:=TLinkedObjList.Create;
end;

{==============================================================================}

destructor THTTPParams.Destroy; 
begin
 {}
 FParams.Free;
 inherited Destroy;
end;

{==============================================================================}

procedure THTTPParams.Clear;
begin
 {}
 FParams.ClearList;
end;

{==============================================================================}

function THTTPParams.GetCount:Integer;
begin
 {}
 Result:=FParams.Count;
end;

{==============================================================================}
  
function THTTPParams.GetParam(APrevious:THTTPParam):THTTPParam;  
begin
 {}
 Result:=nil;
 
 if APrevious = nil then
  begin
   Result:=THTTPParam(FParams.First);
  end
 else
  begin
   Result:=THTTPParam(APrevious.Next);
  end;  
end;

{==============================================================================}

function THTTPParams.FindParam(const AName:String):THTTPParam;
var
 Hash:LongWord;
 Param:THTTPParam;
begin
 {}
 Result:=nil;
 
 Hash:=GenerateNameHash(AName,stringHashSize);
 Param:=THTTPParam(FParams.First);
 while Param <> nil do
  begin
   if Param.Hash = Hash then
    begin
     if Uppercase(Param.Name) = Uppercase(AName) then
      begin
       Result:=Param;
       Exit;
      end;
    end;
  
   Param:=THTTPParam(Param.Next);
  end;
end;

{==============================================================================}
  
function THTTPParams.AddParam(const AName,AValue:String):Boolean;
var
 Param:THTTPParam;
begin
 {}
 Result:=False;
 
 if Length(AName) = 0 then Exit;
 
 if FindParam(AName) <> nil then Exit;
 
 Param:=THTTPParam.Create(AName);
 Param.Value:=AValue;
 
 if FParams.Add(Param) then
  begin
   Result:=True;
  end
 else
  begin
   Param.Free;
  end;  
end;

{==============================================================================}

function THTTPParams.DeleteParam(const AName:String):Boolean;
var
 Param:THTTPParam;
begin
 {}
 Result:=False;
 
 if Length(AName) = 0 then Exit;
 
 Param:=FindParam(AName);
 if Param = nil then Exit;
 
 if FParams.Remove(Param) then
  begin
   Param.Free;
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{THTTPHeader}
constructor THTTPHeader.Create(const AName:String);
begin
 {}
 inherited Create;
 FName:=AName;
 FHash:=GenerateNameHash(FName,stringHashSize);
 FValues:=TLinkedStringList.Create;
end;

{==============================================================================}

destructor THTTPHeader.Destroy; 
begin
 {}
 FValues.Free;
 inherited Destroy;
end;

{==============================================================================}

function THTTPHeader.GetCount:Integer;
begin
 {}
 Result:=FValues.Count;
end;

{==============================================================================}
  
function THTTPHeader.GetValue(AIndex:Integer):String;
begin
 {}
 Result:='';
 
 if (AIndex < 0) or (AIndex >= FValues.Count) then Exit;
 
 Result:=FValues.Strings[AIndex];
end;

{==============================================================================}

function THTTPHeader.SetValue(AIndex:Integer;const AValue:String):Boolean;
begin
 {}
 Result:=False;
 
 if (AIndex < 0) or (AIndex >= FValues.Count) then Exit;

 FValues.Strings[AIndex]:=AValue;

 Result:=True; 
end;

{==============================================================================}

function THTTPHeader.FindValue(const AValue:String):Integer;
begin
 {}
 Result:=FValues.IndexOf(AValue);
end;

{==============================================================================}
  
function THTTPHeader.AddValue(const AValue:String):Boolean;
begin
 {}
 Result:=(FValues.Add(AValue) <> -1);
end;

{==============================================================================}

function THTTPHeader.DeleteValue(const AValue:String):Boolean;
var
 Index:Integer;
begin
 {}
 Result:=False;
 
 Index:=FValues.IndexOf(AValue);
 if Index = -1 then Exit;
 
 FValues.Delete(Index);
 
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{THTTPHeaders}
constructor THTTPHeaders.Create;
begin
 {}
 inherited Create;
 FHeaders:=TLinkedObjList.Create;
end;

{==============================================================================}

destructor THTTPHeaders.Destroy; 
begin
 {}
 FHeaders.Free;
 inherited Destroy;
end;

{==============================================================================}

procedure THTTPHeaders.Clear;
begin
 {}
 FHeaders.ClearList;
end;

{==============================================================================}

function THTTPHeaders.GetCount:Integer;
begin
 {}
 Result:=FHeaders.Count;
end;

{==============================================================================}
  
function THTTPHeaders.GetHeader(APrevious:THTTPHeader):THTTPHeader;  
begin
 {}
 Result:=nil;
 
 if APrevious = nil then
  begin
   Result:=THTTPHeader(FHeaders.First);
  end
 else
  begin
   Result:=THTTPHeader(APrevious.Next);
  end;  
end;

{==============================================================================}

function THTTPHeaders.FindHeader(const AName:String):THTTPHeader;
var
 Hash:LongWord;
 Header:THTTPHeader;
begin
 {}
 Result:=nil;
 
 Hash:=GenerateNameHash(AName,stringHashSize);
 Header:=THTTPHeader(FHeaders.First);
 while Header <> nil do
  begin
   if Header.Hash = Hash then
    begin
     if Uppercase(Header.Name) = Uppercase(AName) then
      begin
       Result:=Header;
       Exit;
      end;
    end;
  
   Header:=THTTPHeader(Header.Next);
  end;
end;

{==============================================================================}
  
function THTTPHeaders.AddHeader(const AName,AValue:String):Boolean;
var
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 if Length(AName) = 0 then Exit;
 
 if FindHeader(AName) <> nil then Exit;
 
 Header:=THTTPHeader.Create(AName);
 Header.AddValue(AValue);
 
 if FHeaders.Add(Header) then
  begin
   Result:=True;
  end
 else
  begin
   Header.Free;
  end;  
end;

{==============================================================================}

function THTTPHeaders.DeleteHeader(const AName:String):Boolean;
var
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 if Length(AName) = 0 then Exit;
 
 Header:=FindHeader(AName);
 if Header = nil then Exit;
 
 if FHeaders.Remove(Header) then
  begin
   Header.Free;
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{THTTPClientRequest}
constructor THTTPClientRequest.Create(AClient:THTTPClient);
begin
 {}
 inherited Create;
 FFlags:=HTTP_REQUEST_FLAG_NONE;
 FClient:=AClient;
 
 URL:='';
 Method:=HTTP_METHOD_GET;
 Version:=HTTP_VERSION;
 
 Params:=THTTPParams.Create;
 Headers:=THTTPHeaders.Create;
 
 Protocol:='';
 Host:='';
 Port:='';
 Path:='';
 Query:='';
  
 ContentStream:=nil;
 ContentString:='';
 
 RedirectCount:=0;
end;
 
{==============================================================================}

destructor THTTPClientRequest.Destroy; 
begin
 {}
 Params.Free;
 Headers.Free;
 inherited Destroy;
end;

{==============================================================================}

function THTTPClientRequest.GetLineSent:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_REQUEST_FLAG_LINE_SENT) <> 0);
end;

{==============================================================================}

function THTTPClientRequest.GetHeadersSent:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_REQUEST_FLAG_HEADERS_SENT) <> 0);
end;

{==============================================================================}

function THTTPClientRequest.GetContentSent:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_REQUEST_FLAG_CONTENT_SENT) <> 0);
end;

{==============================================================================}

function THTTPClientRequest.Close:Boolean;
begin
 {}
 Result:=False;
 
 FFlags:=HTTP_REQUEST_FLAG_NONE;

 Protocol:='';
 Host:='';
 Port:='';
 Path:='';
 Query:='';
  
 ContentStream:=nil;
 ContentString:='';
 
 RedirectCount:=0;
 
 Result:=True;
end;

{==============================================================================}

function THTTPClientRequest.Clear:Boolean;
begin
 {}
 Result:=False;
 
 FFlags:=HTTP_REQUEST_FLAG_NONE;
 
 URL:='';
 Method:=HTTP_METHOD_GET;
 Version:=HTTP_VERSION;
 
 Params.Clear;
 Headers.Clear;
 
 Protocol:='';
 Host:='';
 Port:='';
 Path:='';
 Query:='';
  
 ContentStream:=nil;
 ContentString:='';
 
 RedirectCount:=0;
 
 Result:=True;
end;
 
{==============================================================================}

function THTTPClientRequest.SetParam(const AName,AValue:String):Boolean;
begin
 {}
 Result:=SetParamEx(AName,AValue,True);
end;

{==============================================================================}

function THTTPClientRequest.SetParamEx(const AName,AValue:String;AReplace:Boolean):Boolean;
var
 Param:THTTPParam;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: SetParamEx');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Name = ' + AName);
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Value = ' + AValue);
 {$ENDIF}
 
 Param:=Params.FindParam(AName);
 if Param <> nil then
  begin
   {Check Replace}
   if not AReplace then Exit;

   {Replace Value}
   Param.Value:=AValue;
   
   Result:=True;
  end
 else
  begin
   {Add Param}
   Result:=Params.AddParam(AName,AValue);
  end;  
end;

{==============================================================================}

function THTTPClientRequest.SetHeader(const AName,AValue:String):Boolean;
begin
 {}
 Result:=SetHeaderEx(AName,AValue,True);
end;

{==============================================================================}

function THTTPClientRequest.SetHeaderEx(const AName,AValue:String;AReplace:Boolean):Boolean;
var
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: SetHeaderEx');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Name = ' + AName);
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Value = ' + AValue);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header <> nil then
  begin
   {Check Replace}
   if AReplace then
    begin
     {Replace Value}
     Result:=Header.SetValue(0,AValue);
    end
   else
    begin   
     {Add Value}
     if Header.FindValue(AValue) <> -1 then
      begin
       Result:=True;
      end
     else
      begin
       Result:=Header.AddValue(AValue);
      end;    
    end;  
  end
 else
  begin
   {Add Header}
   Result:=Headers.AddHeader(AName,AValue);
  end;  
end;

{==============================================================================}

function THTTPClientRequest.WriteRequest:Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: WriteRequest');
 {$ENDIF}
 
 {Check Sent}
 if not ContentSent then
  begin
   {Check Content}
   if ContentStream <> nil then
    begin
     {Stream Content}
     {Check Content Type}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE,HTTP_CONTENT_TEXT_HTML);
      end;
   
     {Check Content Length}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(ContentStream.Size));
      end;
     
     {Send Line}
     if not LineSent then if not WriteLine then Exit;
   
     {Send Headers}
     if not HeadersSent then if not WriteHeaders then Exit;
   
     {Write Content}
     Result:=FClient.WriteRequestContentStream(Self,ContentStream,ContentStream.Size);
     if not Result then Exit;
   
     {Set Sent}
     FFlags:=FFlags or HTTP_REQUEST_FLAG_CONTENT_SENT;
    end
   else if Length(ContentString) <> 0 then
    begin
     {String Content}
     {Check Content Type}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE,HTTP_CONTENT_TEXT_HTML);
      end;
   
     {Check Content Length}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(Length(ContentString)));
      end;
   
     {Send Line}
     if not LineSent then if not WriteLine then Exit;
   
     {Send Headers}
     if not HeadersSent then if not WriteHeaders then Exit;
   
     {Write Content}
     Result:=FClient.WriteRequestContentString(Self,ContentString,Length(ContentString));
     if not Result then Exit;
   
     {Set Sent}
     FFlags:=FFlags or HTTP_REQUEST_FLAG_CONTENT_SENT;
    end
   else
    begin
     {No Content}
     {Send Line}
     if not LineSent then if not WriteLine then Exit;
   
     {Send Headers}
     if not HeadersSent then if not WriteHeaders then Exit;
   
     {Set Sent}
     FFlags:=FFlags or HTTP_REQUEST_FLAG_CONTENT_SENT;
   
     {Return Result}
     Result:=True;
    end;
  end 
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function THTTPClientRequest.WriteLine:Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: WriteLine');
 {$ENDIF}
 
 {Check Sent}
 if not LineSent then
  begin
   {Write Line}
   Result:=FClient.WriteRequestLine(Self);
   if not Result then Exit;
   
   {Set Sent}
   FFlags:=FFlags or HTTP_REQUEST_FLAG_LINE_SENT;
  end
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function THTTPClientRequest.WriteHeaders:Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: WriteHeaders');
 {$ENDIF}
 
 {Check Sent}
 if not HeadersSent then
  begin
   {Write Headers}
   Result:=FClient.WriteRequestHeaders(Self);
   if not Result then Exit;
   
   {Set Sent}
   FFlags:=FFlags or HTTP_REQUEST_FLAG_HEADERS_SENT;
  end
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function THTTPClientRequest.WriteContentStream(AContent:TStream;ASize:LongWord;ACompleted:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: WriteContentStream');
 {$ENDIF}
 
 {Check Content}
 if AContent = nil then Exit;
 
 {Check Sent}
 if ContentSent then Exit;
 
 {Send Line}
 if not LineSent then if not WriteLine then Exit;
 
 {Send Headers}
 if not HeadersSent then if not WriteHeaders then Exit;
 
 {Write Content}
 Result:=FClient.WriteRequestContentStream(Self,AContent,ASize);
 if not Result then Exit;
   
 {Set Sent}
 if ACompleted then FFlags:=FFlags or HTTP_REQUEST_FLAG_CONTENT_SENT;
end;

{==============================================================================}

function THTTPClientRequest.WriteContentString(const AContent:String;ASize:LongWord;ACompleted:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: WriteContentString');
 {$ENDIF}
 
 {Check Sent}
 if ContentSent then Exit;
 
 {Send Line}
 if not LineSent then if not WriteLine then Exit;
 
 {Send Headers}
 if not HeadersSent then if not WriteHeaders then Exit;
 
 {Write Content}
 Result:=FClient.WriteRequestContentString(Self,AContent,ASize);
 if not Result then Exit;
   
 {Set Sent}
 if ACompleted then FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_SENT;
end;

{==============================================================================}
{==============================================================================}
{THTTPClientResponse}
constructor THTTPClientResponse.Create(AClient:THTTPClient);
begin
 {}
 inherited Create;
 FFlags:=HTTP_RESPONSE_FLAG_NONE;
 FClient:=AClient;
 
 Reason:='';
 Status:=HTTP_STATUS_NONE;
 Version:=HTTP_VERSION_00;
 
 Headers:=THTTPHeaders.Create;
end;
 
{==============================================================================}

destructor THTTPClientResponse.Destroy; 
begin
 {}
 Headers.Free;
 inherited Destroy;
end;

{==============================================================================}

function THTTPClientResponse.GetContentReceived:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_CONTENT_RECEIVED) <> 0);
end;

{==============================================================================}

function THTTPClientResponse.GetNoCache:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_NO_CACHE) <> 0);
end;

{==============================================================================}

procedure THTTPClientResponse.SetNoCache(ANoCache:Boolean);
begin
 {}
 if ANoCache then
  begin
   FFlags:=FFlags or HTTP_RESPONSE_FLAG_NO_CACHE;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_RESPONSE_FLAG_NO_CACHE);
  end;
end;

{==============================================================================}

function THTTPClientResponse.GetConnectionClose:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_CONNECTION_CLOSE) <> 0);
end;

{==============================================================================}

procedure THTTPClientResponse.SetConnectionClose(AConnectionClose:Boolean);
begin
 {}
 if AConnectionClose then
  begin
   FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONNECTION_CLOSE;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_RESPONSE_FLAG_CONNECTION_CLOSE);
  end;
end;

{==============================================================================}

function THTTPClientResponse.Clear:Boolean;
begin
 {}
 Result:=False;
 
 FFlags:=HTTP_RESPONSE_FLAG_NONE;
 
 Reason:='';
 Status:=HTTP_STATUS_NONE;
 Version:=HTTP_VERSION_00;
 
 Headers.Clear;
 
 Result:=True;
end;

{==============================================================================}

function THTTPClientResponse.GetHeader(const AName:String):String;
var
 Header:THTTPHeader;
begin
 {}
 Result:='';
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: GetHeader');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response:  Name = ' + AName);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header = nil then Exit;
 
 Result:=Header.GetValue(0);
end;

{==============================================================================}

function THTTPClientResponse.GetHeaderEx(const AName:String):TStringList;
var
 Count:Integer;
 Header:THTTPHeader;
begin
 {}
 Result:=nil;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: GetHeaderEx');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response:  Name = ' + AName);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header = nil then Exit;
 
 Result:=TStringList.Create;
 
 for Count:=0 to Header.GetCount - 1 do
  begin
   Result.Add(Header.GetValue(Count));
  end;
end;

{==============================================================================}

function THTTPClientResponse.FindHeader(const AName,AValue:String):Boolean;
var
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: FindHeader');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response:  Name = ' + AName);
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response:  Value = ' + AValue);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header = nil then Exit;

 Result:=(Header.FindValue(AValue) <> -1);
end;

{==============================================================================}

function THTTPClientResponse.ReadContentStream(AContent:TStream;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: ReadContentStream');
 {$ENDIF}
 
 {Check Content}
 if AContent = nil then Exit;
 
 {Check Received}
 if ContentReceived then Exit;
 
 {Read Content}
 Result:=FClient.ReadResponseContentStream(Self,AContent,ASize);
 if not Result then Exit;
 
 {Set Received}
 FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_RECEIVED;
end;

{==============================================================================}

function THTTPClientResponse.ReadContentString(var AContent:String;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Client}
 if FClient = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: ReadContentString');
 {$ENDIF}
 
 {Check Received}
 if ContentReceived then Exit;
 
 {Read Content}
 Result:=FClient.ReadResponseContentString(Self,AContent,ASize);
 if not Result then Exit;

 {Set Received}
 FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_RECEIVED;
end;

{==============================================================================}
{==============================================================================}
{THTTPClient}
constructor THTTPClient.Create;
begin
 {}
 inherited Create;
 UseNagle:=False; {Nagle is not recommended for HTTP (Will often delay the last segment without reason)}
 
 FLock:=CriticalSectionCreate;
 
 FState:=HTTP_CLIENT_STATE_NONE;
 FBuffer:=THTTPBuffer.Create(HTTP_BUFFER_SIZE);
 
 FRequest:=THTTPClientRequest.Create(Self);
 FResponse:=THTTPClientResponse.Create(Self);

 FTimeout:=HTTP_REQUEST_TIMEOUT;
 FUserAgent:=HTTP_USERAGENT_STRING;
 FAllowRedirect:=True;
 FMaxRedirects:=HTTP_MAX_REDIRECTS;
 FKeepAlive:=True;
 FKeepAliveTimeout:=HTTP_KEEPALIVE_TIMEOUT;
 
 FProxyHost:='';
 FProxyPort:=IntToStr(HTTP_PROXY_PORT_DEFAULT);
 FProxyUsername:='';
 FProxyPassword:='';
 
 {Clear Request}
 ClearRequest;
end;

{==============================================================================}

destructor THTTPClient.Destroy; 
begin
 {}
 AcquireLock;
 try
  FResponse.Free;
  FRequest.Free;
  FBuffer.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

procedure THTTPClient.SetTimeout(ATimeout:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FTimeout:=ATimeout;
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetUserAgent:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FUserAgent;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetUserAgent(const AUserAgent:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FUserAgent:=AUserAgent;
 UniqueString(FUserAgent);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetAllowRedirect(AAllowRedirect:Boolean);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FAllowRedirect:=AAllowRedirect;
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetMaxRedirects(AMaxRedirects:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FMaxRedirects:=AMaxRedirects;
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetKeepAlive(AKeepAlive:Boolean);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FKeepAlive:=AKeepAlive;
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetKeepAliveTimeout(AKeepAliveTimeout:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FKeepAliveTimeout:=AKeepAliveTimeout;
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetHost:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FHost;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetHost(const AHost:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FHost:=AHost;
 UniqueString(FHost);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetPort:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FPort;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetPort(const APort:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FPort:=APort;
 UniqueString(FPort);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetUsername:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FUsername;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetUsername(const AUsername:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FUsername:=AUsername;
 UniqueString(FUsername);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetPassword:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FPassword;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetPassword(const APassword:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FPassword:=APassword;
 UniqueString(FPassword);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetProxyHost:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FProxyHost;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetProxyHost(const AProxyHost:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FProxyHost:=AProxyHost;
 UniqueString(FProxyHost);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetProxyPort:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FProxyPort;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetProxyPort(const AProxyPort:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FProxyPort:=AProxyPort;
 UniqueString(FProxyPort);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetProxyUsername:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FProxyUsername;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetProxyUsername(const AProxyUsername:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FProxyUsername:=AProxyUsername;
 UniqueString(FProxyUsername);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetProxyPassword:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FProxyPassword;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetProxyPassword(const AProxyPassword:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FProxyPassword:=AProxyPassword;
 UniqueString(FProxyPassword);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetURL:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FURL;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetURL(const AURL:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FURL:=AURL;
 UniqueString(FURL);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetMethod(AMethod:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FMethod:=AMethod;
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetVersion(AVersion:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FVersion:=AVersion;
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetMimeType:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FMimeType;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetMimeType(const AMimeType:String);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
 
 FMimeType:=AMimeType;
 UniqueString(FMimeType);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetRangeStart(ARangeStart:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FRangeStart:=ARangeStart;
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetRangeEnd(ARangeEnd:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FRangeEnd:=ARangeEnd;
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPClient.SetEncoding(AEncoding:LongWord);
begin
 {}
 if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

 if not AcquireLock then Exit;
  
 FEncoding:=AEncoding;
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetResponseStatus:LongWord;
begin
 {}
 Result:=HTTP_STATUS_NONE;
 
 if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
 if not AcquireLock then Exit;
 
 Result:=FResponse.Status;
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetResponseReason:String;
begin
 {}
 Result:='';
 
 if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
 if not AcquireLock then Exit;
 
 Result:=FResponse.Reason;
 UniqueString(Result);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetResponseVersion:LongWord;
begin
 {}
 Result:=HTTP_VERSION_00;
 
 if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
 if not AcquireLock then Exit;
 
 Result:=FResponse.Version;
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetResponseMimeType:String;
begin
 {}
 Result:='';
 
 if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
 if not AcquireLock then Exit;
 
 Result:=FResponse.GetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetResponseEncoding:LongWord;
begin
 {}
 Result:=HTTP_ENCODING_NONE;
 
 if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
 if not AcquireLock then Exit;
 
 Result:=StringToHTTPEncoding(FResponse.GetHeader(HTTP_GENERAL_HEADER_TRANSFER_ENCODING));
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.GetResponseContentSize:LongWord;
begin
 {}
 Result:=0;
 
 if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
 if not AcquireLock then Exit;
 
 Result:=StrToIntDef(FResponse.GetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH),0);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPClient.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPClient.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPClient.DoRedirect(const AURL:String;var ALocation:String):Boolean;
begin
 {}
 if Assigned(FOnRedirect) then
  begin
   Result:=FOnRedirect(Self,AURL,ALocation);
  end
 else
  begin
   Result:=False;
   
   {Check Redirect}
   if not AllowRedirect then Exit;
   
   {Check Redirect Count}
   if FRequest.RedirectCount >= MaxRedirects then Exit;
   
   Result:=True;
  end;  
end;

{==============================================================================}

function THTTPClient.DoPassword(var AUsername,APassword:String):Boolean;
begin
 {}
 if Assigned(FOnPassword) then
  begin
   Result:=FOnPassword(Self,AUsername,APassword);
  end
 else
  begin
   Result:=False;
  end;  
end;

{==============================================================================}

function THTTPClient.ReadResponseLine(AResponse:THTTPClientResponse):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 WorkBuffer:String;
begin
 {}
 Result:=False;

 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: ReadResponseLine');
 {$ENDIF}
 
 {Read Response Line}
 Completed:=False;
 WorkBuffer:='';
 while not(Completed) do
  begin
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer FStart = ' + IntToStr(FBuffer.FStart));
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Count = ' + IntToStr(FBuffer.Count));
   {$ENDIF}
 
   {Read from Buffer}
   while FBuffer.Count > 0 do
    begin
     {Read Value}
     Value:=FBuffer.ReadData;
     
     {Check for CR LF}
     if not(Value in [#10,#13]) then
      begin
       WorkBuffer:=WorkBuffer + Value;
      end
     else
      begin
       {Check for LF}
       if Value = #10 then
        begin
         Completed:=True;
         Break;
        end;
      end;      
    end;
   
   {Check Completed}
   if Completed then Break;
   
   {Read from Socket}
   Data:=FBuffer.WriteLock(Size);
   if Data = nil then Exit;
   try
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer FStart = ' + IntToStr(FBuffer.FStart));
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;
      
    {Read Available}
    if not ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Write Count = ' + IntToStr(Count));
    {$ENDIF}
   finally
    FBuffer.WriteUnlock(Count);
   end; 
  end;  

 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Response = ' + WorkBuffer);
 {$ENDIF}
  
 {Parse Response Line}
 if not HTTPParseResponseLine(WorkBuffer,AResponse.Version,AResponse.Status,AResponse.Reason) then Exit;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function THTTPClient.ReadResponseHeaders(AResponse:THTTPClientResponse):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 WorkBuffer:String;
 HeaderName:String;
 HeaderValue:String;
begin
 {}
 Result:=False;

 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: ReadResponseHeaders');
 {$ENDIF}
 
 {Read Response Headers}
 repeat
  WorkBuffer:='';
  
  {Read Response Header}
  Completed:=False;
  while not(Completed) do
   begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer FStart = ' + IntToStr(FBuffer.FStart));
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Count = ' + IntToStr(FBuffer.Count));
    {$ENDIF}
   
    {Read from Buffer}
    while FBuffer.Count > 0 do
     begin
      {Read Value}
      Value:=FBuffer.ReadData;
     
      {Check for CR LF}
      if not(Value in [#10,#13]) then
       begin
        WorkBuffer:=WorkBuffer + Value;
       end
      else
       begin
        {Check for LF}
        if Value = #10 then
         begin
          Completed:=True;
          Break;
         end;
       end;      
     end;
   
    {Check Completed}
    if Completed then Break;
   
    {Read from Socket}
    Data:=FBuffer.WriteLock(Size);
    if Data = nil then Exit;
    try
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer FStart = ' + IntToStr(FBuffer.FStart));
     if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Write Size = ' + IntToStr(Size));
     {$ENDIF}
 
     Count:=0;
       
     {Read Available}
     if not ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Write Count = ' + IntToStr(Count));
     {$ENDIF}
    finally
     FBuffer.WriteUnlock(Count);
    end; 
   end;  

  {Check Response Header}
  if Length(WorkBuffer) = 0 then Break;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Header = ' + WorkBuffer);
  {$ENDIF}

  {Parse Response Header}
  if not HTTPParseHeader(WorkBuffer,HeaderName,HeaderValue) then Exit;
  
  {Add Response Header} //To Do //This will fail if a header is repeated or split, how to handle ? //See RFCs //Allow Multiple Headers and Folded Headers //See RFC (Check for Space or Tab)
  if not AResponse.Headers.AddHeader(HeaderName,HeaderValue) then Exit;
  
 until Length(WorkBuffer) = 0;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function THTTPClient.ReadResponseContentStream(AResponse:THTTPClientResponse;AContent:TStream;ASize:LongWord):Boolean;
{If Size is 0 then read the entire content to the stream}
var
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;

 Buffer:Pointer;
 BytesRemain:Int64;
 BlockSize:LongWord;
begin
 {}
 Result:=False;

 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: ReadResponseContentStream');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Content}
 if AContent = nil then Exit;
 
 {Read Response Content}
 case ResponseEncoding of
  HTTP_ENCODING_NONE,HTTP_ENCODING_IDENTITY:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(ResponseEncoding));
    {$ENDIF}
    
    {Allocate Buffer}
    BlockSize:=Min(SegmentSize,SIZE_2M - (SIZE_2M mod MaxSegmentSize)); {SIZE_256K;}
    Buffer:=GetMem(BlockSize);
    if Buffer = nil then Exit;
    try
     {Get Size}
     BytesRemain:=ASize;
     if ASize = 0 then BytesRemain:=ResponseContentSize;
    
     {Read Content}
     while BytesRemain > 0 do
      begin
       {Read from Buffer}
       while FBuffer.Count > 0 do
        begin
         {Read Buffer}
         Data:=FBuffer.ReadLock(Size);
         if Data = nil then Exit;
         try
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer FStart = ' + IntToStr(FBuffer.FStart));
          if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Read Size = ' + IntToStr(Size));
          {$ENDIF}
          
          if BytesRemain >= Size then
           begin
            AContent.WriteBuffer(Data^,Size);
            Count:=Size;
          
            Dec(BytesRemain,Size);
           end
          else
           begin
            AContent.WriteBuffer(Data^,BytesRemain);
            Count:=BytesRemain;
            
            BytesRemain:=0;
            Break;
           end;
         finally
          FBuffer.ReadUnlock(Count);
         end;
        end;
       
       {Check Remain}
       if BytesRemain = 0 then Break;
       
       {Read from Socket}
       if BytesRemain >= BlockSize then 
        begin
         if not ReadData(Buffer,BlockSize) then Exit;
         AContent.WriteBuffer(Buffer^,BlockSize);
         
         Dec(BytesRemain,BlockSize);
        end
       else
        begin
         if not ReadData(Buffer,BytesRemain) then Exit;
         AContent.WriteBuffer(Buffer^,BytesRemain);
         
         BytesRemain:=0;
        end;
      end;
      
     {Return Result}
     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   end; 
  HTTP_ENCODING_CHUNKED:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(ResponseEncoding));
    {$ENDIF}

    //To Do
   end;
 end;  
end;

{==============================================================================}

function THTTPClient.ReadResponseContentString(AResponse:THTTPClientResponse;var AContent:String;ASize:LongWord):Boolean;
{If Size is 0 then read the entire content to the string}
var
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;

 Buffer:PChar;
 BytesRemain:Int64;
begin
 {}
 Result:=False;

 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: ReadResponseContentString');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Read Response Content}
 case ResponseEncoding of
  HTTP_ENCODING_NONE,HTTP_ENCODING_IDENTITY:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(ResponseEncoding));
    {$ENDIF}

    {Get Size}
    BytesRemain:=ASize;
    if ASize = 0 then BytesRemain:=ResponseContentSize;
    
    {Read Content}
    if BytesRemain > 0 then
     begin
      {Size Content}
      SetLength(AContent,BytesRemain);
      
      {Get Buffer}
      Buffer:=PChar(AContent);
      
      {Read from Buffer}
      while FBuffer.Count > 0 do
       begin
        {Read Buffer}
        Data:=FBuffer.ReadLock(Size);
        if Data = nil then Exit;
        try
         {$IFDEF HTTP_DEBUG}
         if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer FStart = ' + IntToStr(FBuffer.FStart));
         if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Buffer Read Size = ' + IntToStr(Size));
         {$ENDIF}
         
         if BytesRemain >= Size then
          begin
           System.Move(Data^,Buffer^,Size);
           Count:=Size;
         
           Inc(Buffer,Size);
           Dec(BytesRemain,Size);
          end
         else
          begin
           System.Move(Data^,Buffer^,BytesRemain);
           Count:=BytesRemain;
           
           BytesRemain:=0;
           Break;
          end;
        finally
         FBuffer.ReadUnlock(Count);
        end;
       end;
      
      {Check Remain}
      if BytesRemain > 0 then
       begin
        {Read from Socket}
        if not ReadData(Buffer,BytesRemain) then Exit;
     
        BytesRemain:=0;
       end; 
     end;
     
    {Return Result}
    Result:=True;
   end; 
  HTTP_ENCODING_CHUNKED:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(ResponseEncoding));
    {$ENDIF}

    //To Do
   end;
 end;  
end;

{==============================================================================}

function THTTPClient.WriteRequestLine(ARequest:THTTPClientRequest):Boolean;
var
 WorkURL:String;
 WorkQuery:String;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: WriteRequestLine');
 {$ENDIF}

 {Get Query}
 if not HTTPBuildQuery(ARequest.Params,WorkQuery) then Exit;
 
 {Get URL}
 if Length(ProxyHost) <> 0 then
  begin
   {Full URL}
   if not HTTPBuildURI(ARequest.Protocol,ARequest.Host,ARequest.Port,HTTPEncode(ARequest.Path,HTTPReservedURLChars),WorkQuery,WorkURL) then Exit;
  end
 else
  begin
   {Relative URL}
   if not HTTPBuildURI('','','',HTTPEncode(ARequest.Path,HTTPReservedURLChars),WorkQuery,WorkURL) then Exit;
  end;
 
 {Get Request Line}
 if not HTTPBuildRequestLine(ARequest.Method,WorkURL,ARequest.Version,WorkBuffer) then Exit;
 
 {Write Request Line}
 if not WriteData(PChar(WorkBuffer + HTTP_LINE_END),Length(WorkBuffer + HTTP_LINE_END)) then Exit;
 
 {Return Result}
 Result:=True;
end;
 
{==============================================================================}

function THTTPClient.WriteRequestHeaders(ARequest:THTTPClientRequest):Boolean;
var
 Count:Integer;
 WorkBuffer:String;
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: WriteRequestHeaders');
 {$ENDIF}

 {Get Header}
 Header:=ARequest.Headers.GetHeader(nil);
 while Header <> nil do
  begin
   for Count:=0 to Header.GetCount - 1 do
    begin
     {Build Header}
     if Count = 0 then
      begin
       if not HTTPBuildHeader(Header.Name,Header.GetValue(Count),WorkBuffer) then Exit;
      end
     else
      begin
       if not HTTPBuildHeader('',Header.GetValue(Count),WorkBuffer) then Exit;
      end;

     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Header = ' + WorkBuffer);
     {$ENDIF}
     
     {Write Header}
     if not WriteData(PChar(WorkBuffer + HTTP_LINE_END),Length(WorkBuffer + HTTP_LINE_END)) then Exit;
    end;
   
   {Get Header}
   Header:=ARequest.Headers.GetHeader(Header);
  end;

 {Write Header End}
 if not WriteData(PChar(HTTP_LINE_END),Length(HTTP_LINE_END)) then Exit;
 
 {Return Result}
 Result:=True;
end;
 
{==============================================================================}

function THTTPClient.WriteRequestContentStream(ARequest:THTTPClientRequest;AContent:TStream;ASize:LongWord):Boolean;
var
 Buffer:Pointer;
 BytesRemain:Int64;
 BlockSize:LongWord;
begin
 {}
 Result:=False;
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: WriteRequestContentStream');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Content}
 if AContent = nil then Exit;
 
 {Check Size}
 if ASize > AContent.Size then Exit;
 
 {Write Request Content}
 case RequestEncoding of
  HTTP_ENCODING_NONE,HTTP_ENCODING_IDENTITY:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(RequestEncoding));
    {$ENDIF}
 
    {Allocate Buffer}
    BlockSize:=Min(SegmentSize,SIZE_2M - (SIZE_2M mod MaxSegmentSize)); {SIZE_256K;}
    Buffer:=GetMem(BlockSize);
    if Buffer = nil then Exit;
    try
     {Get Size}
     BytesRemain:=ASize;
     
     {Write Content}
     while BytesRemain > 0 do
      begin
       if BytesRemain >= BlockSize then 
        begin
         AContent.ReadBuffer(Buffer^,BlockSize);
         if not WriteData(Buffer,BlockSize) then Exit;
         
         Dec(BytesRemain,BlockSize);
        end
       else
        begin
         AContent.ReadBuffer(Buffer^,BytesRemain);
         if not WriteData(Buffer,BytesRemain) then Exit;
         
         BytesRemain:=0;
        end;
      end;
    
     {Return Result}
     Result:=True;
    finally
     FreeMem(Buffer);
    end;
   end; 
  HTTP_ENCODING_CHUNKED:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(RequestEncoding));
    {$ENDIF}

    //To Do
   end;
 end;  
end;

{==============================================================================}

function THTTPClient.WriteRequestContentString(ARequest:THTTPClientRequest;const AContent:String;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: WriteRequestContentString');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Size}
 if ASize > Length(AContent) then Exit;
 
 {Write Request Content}
 case RequestEncoding of
  HTTP_ENCODING_NONE,HTTP_ENCODING_IDENTITY:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(RequestEncoding));
    {$ENDIF}

    {Write Content}
    if not WriteData(PChar(AContent),ASize) then Exit;

    {Return Result}
    Result:=True;
   end; 
  HTTP_ENCODING_CHUNKED:begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Encoding = ' + HTTPEncodingToString(RequestEncoding));
    {$ENDIF}

    //To Do
   end;
 end;  
end;

{==============================================================================}

function THTTPClient.Head(const AURL:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: Head');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  URL = ' + AURL);
  {$ENDIF}
  
  {Setup Request}
  RequestURL:=AURL;
  RequestMethod:=HTTP_METHOD_HEAD;
  
  {Send Request}
  Result:=SendRequest;
  
  {Close Request}
  CloseRequest(True);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.GetString(const AURL:String;var AContent:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: GetString');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  URL = ' + AURL);
  {$ENDIF}
  
  {Setup Request}
  RequestURL:=AURL;
  RequestMethod:=HTTP_METHOD_GET;
  
  {Send Request}
  if SendRequest then
   begin
    {Read Content String}
    Result:=FResponse.ReadContentString(AContent,0);
   end;
   
  {Close Request}
  CloseRequest(True);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.GetStream(const AURL:String;AContent:TStream):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: GetStream');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  URL = ' + AURL);
  {$ENDIF}
  
  {Setup Request}
  RequestURL:=AURL;
  RequestMethod:=HTTP_METHOD_GET;
  
  {Send Request}
  if SendRequest then
   begin
    {Read Content Stream}
    Result:=FResponse.ReadContentStream(AContent,0);
   end;
   
  {Close Request}
  CloseRequest(True);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.PostString(const AURL:String;var AContent:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: PostString');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  URL = ' + AURL);
  {$ENDIF}
  
  {Setup Request}
  RequestURL:=AURL;
  RequestMethod:=HTTP_METHOD_POST;
  
  {Setup Content}
  FRequest.ContentString:=AContent;
  
  {Send Request}
  if SendRequest then
   begin
    {Read Content String}
    Result:=FResponse.ReadContentString(AContent,0);
   end;
   
  {Close Request}
  CloseRequest(True);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.PostStream(const AURL:String;AContent:TStream):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: PostStream');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  URL = ' + AURL);
  {$ENDIF}
  
  {Setup Request}
  RequestURL:=AURL;
  RequestMethod:=HTTP_METHOD_POST;
  
  {Setup Content}
  FRequest.ContentStream:=AContent;
  
  {Send Request}
  if SendRequest then
   begin
    {Read Content Stream}
    Result:=FResponse.ReadContentStream(AContent,0);
   end;
   
  {Close Request}
  CloseRequest(True);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.SendRequest:Boolean;
var
 WorkBuffer:String;
 WorkUsername:String;
 WorkPassword:String;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: SendRequest');
  {$ENDIF}
  
  {Set State}
  FState:=HTTP_CLIENT_STATE_REQUEST;
  try
   {Clear Response}
   FResponse.Clear;
   
   {Update Request}
   FRequest.URL:=RequestURL;
   FRequest.Method:=RequestMethod;
   FRequest.Version:=RequestVersion;
   
   {Parse URL}
   if not HTTPParseURI(FRequest.URL,FRequest.Protocol,FRequest.Host,FRequest.Port,FRequest.Path,FRequest.Query) then Exit;
   
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Protocol = ' + FRequest.Protocol);
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Host = ' + FRequest.Host);
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Port = ' + FRequest.Port);
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Path = ' + FRequest.Path);
   if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Query = ' + FRequest.Query);
   {$ENDIF}
   
   {Parse Query}
   if not HTTPParseQuery(FRequest.Query,FRequest.Params) then Exit;
   
   {Add Host Header}
   if not HTTPBuildHost(FRequest.Host,FRequest.Port,WorkBuffer) then Exit;
   FRequest.SetHeader(HTTP_REQUEST_HEADER_HOST,WorkBuffer);
   
   {Add UserAgent Header}
   FRequest.SetHeader(HTTP_REQUEST_HEADER_USER_AGENT,UserAgent);
   
   {Add Connection Header}
   if KeepAlive then
    begin
     FRequest.SetHeader(HTTP_GENERAL_HEADER_CONNECTION,HTTP_CONNECTION_KEEPALIVE);
    end
   else
    begin
     FRequest.SetHeader(HTTP_GENERAL_HEADER_CONNECTION,HTTP_CONNECTION_CLOSE);
    end;    
   
   {Add Cookies}
   //To Do 
   
   {Add Ranges}
   if (RequestRangeStart > 0) or (RequestRangeEnd > 0) then
    begin
     if RequestRangeEnd >= RequestRangeStart then
      begin
       FRequest.SetHeader(HTTP_REQUEST_HEADER_RANGE,'bytes=' + IntToStr(RequestRangeStart) + '-' + IntToStr(RequestRangeEnd));
      end
     else
      begin     
       FRequest.SetHeader(HTTP_REQUEST_HEADER_RANGE,'bytes=' + IntToStr(RequestRangeStart) + '-');
      end; 
    end;
   
   {Add Authorization}
   if Length(Username) <> 0 then
    begin
     FRequest.SetHeader(HTTP_REQUEST_HEADER_AUTHORIZATION,'Basic ' +  Base64EncodeString(Username + ':' + Password));
    end; 
   
   {Add Proxy-Authorization}
   if (Length(ProxyHost) <> 0) and (Length(ProxyUsername) <> 0) then
    begin
     FRequest.SetHeader(HTTP_REQUEST_HEADER_PROXY_AUTH,'Basic ' +  Base64EncodeString(ProxyUsername + ':' + ProxyPassword));
    end; 
   
   {Setup Host/Port}
   if Length(ProxyHost) <> 0 then
    begin
     {Set Host}
     RemoteHost:=ProxyHost;
     
     {Set Port}
     RemotePort:=StrToIntDef(ProxyPort,HTTP_PROXY_PORT_DEFAULT);
    end
   else 
    begin
     {Set Host}
     RemoteHost:=FRequest.Host;
     
     {Set Port}
     RemotePort:=StrToIntDef(FRequest.Port,HTTP_PORT_DEFAULT);
    end;  
    
   {Setup Timeout}
   //To Do //ReadTimeout ?
   
   //To Do //Redirects
   
   {Connect}    
   if Connected or Connect then
    begin
     {Write Request}
     if not FRequest.WriteRequest then Exit;
     
     {Read Response Line}
     if not ReadResponseLine(FResponse) then Exit;
     
     {Check Version}
     case FResponse.Version of
      HTTP_VERSION_10,HTTP_VERSION_11:begin
        {Get Headers}
        if not ReadResponseHeaders(FResponse) then Exit;
        
        {Check Connection Close}
        FResponse.ConnectionClose:=False;
        if FResponse.FindHeader(HTTP_GENERAL_HEADER_CONNECTION,HTTP_CONNECTION_CLOSE) then
         begin
          FResponse.ConnectionClose:=True;
         end;
        
        {Check Version}
        if (FRequest.Version = HTTP_VERSION_10) or (FResponse.Version = HTTP_VERSION_10) then
         begin
          FResponse.ConnectionClose:=True;
         end;
        
        //To Do //Cookies
        
        {Check Status}
        case FResponse.Status of
         HTTP_STATUS_OK:begin
           {Return Result}
           Result:=True;
          end;
         HTTP_STATUS_MOVED_PERMANENT,HTTP_STATUS_FOUND,HTTP_STATUS_TEMPORARY_REDIRECT:begin
           {Check Redirect}
           if not AllowRedirect then Exit;
           
           {Update Count}
           Inc(FRequest.RedirectCount);
           
           {Get Location}
           WorkBuffer:=FResponse.GetHeader(HTTP_RESPONSE_HEADER_LOCATION);
           
           {Check Location}
           if not DoRedirect(FRequest.URL,WorkBuffer) then Exit;

           {Parse URI}
           //To Do 
           
           {Setup Host/Port}
           //To Do 
          end;
         HTTP_STATUS_SEE_OTHER:begin
           {Check Redirect}
           if not AllowRedirect then Exit;
           
           {Update Count}
           Inc(FRequest.RedirectCount);
           
           {Get Location}
           WorkBuffer:=FResponse.GetHeader(HTTP_RESPONSE_HEADER_LOCATION);
         
           {Check Location}
           if not DoRedirect(FRequest.URL,WorkBuffer) then Exit;
           
           {Parse URI}
           //To Do //
           
           //Change to GET
           
           {Setup Host/Port}
           //To Do 
          end;
         HTTP_STATUS_UNAUTHORIZED:begin
           {Check Password}
           WorkUsername:=Username;
           WorkPassword:=Password;
           
           if not DoPassword(WorkUsername,WorkPassword) then Exit;

           {Update Password}
           Username:=WorkUsername;
           Password:=WorkPassword;
           
           {Add Authorization}
           //To Do 
          end;          
        end; 
       end;
      else
       begin
        Exit;
       end;
     end;
    end;
  finally
   if Result then
    begin
     FState:=HTTP_CLIENT_STATE_SUCCESS;
    end
   else
    begin
     FState:=HTTP_CLIENT_STATE_FAILURE;
    end;    
  end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.CloseRequest(Close:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: CloseRequest');
  {$ENDIF}

  {Close Request}
  FRequest.Close;
  
  {Check Close}
  if Close then {if (FState = HTTP_CLIENT_STATE_FAILURE) or (FResponse.ConnectionClose) then} {Allow caller to specify Close}
   begin
    {Shutdown}
    Shutdown;
    
    {Disconnect}
    Disconnect;
   end; 
  
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;
  
{==============================================================================}

function THTTPClient.ClearRequest:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: ClearRequest');
  {$ENDIF}
  
  {Clear Request}
  FState:=HTTP_CLIENT_STATE_NONE;
  
  FRequest.Clear;
  FResponse.Clear;
  
  FHost:='';
  FPort:='';
  FUsername:='';
  FPassword:='';
  
  FURL:='';
  FMethod:=HTTP_METHOD_GET;
  FVersion:=HTTP_VERSION;
  FMimeType:='';
  FRangeStart:=0;
  FRangeEnd:=0;
  FEncoding:=HTTP_ENCODING_IDENTITY;
  
  {Disconnect}
  Disconnect;
  
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.CancelRequest:Boolean;
{Note: No lock to allow another thread to cancel the request}
begin
 {}
 Result:=False;
 
 if FState <> HTTP_CLIENT_STATE_REQUEST then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Client: CancelRequest');
 {$ENDIF}
 
 {Disconnect}
 Disconnect;
 
 Result:=True;
end;

{==============================================================================}

function THTTPClient.SetRequestParam(const AName,AValue:String):Boolean;
begin
 {}
 Result:=SetRequestParamEx(AName,AValue,True);
end;

{==============================================================================}

function THTTPClient.SetRequestParamEx(const AName,AValue:String;AReplace:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: SetRequestParamEx');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Name = ' + AName);
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Value = ' + AValue);
  {$ENDIF}
 
  {Set Param}
  Result:=FRequest.SetParamEx(AName,AValue,AReplace);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.SetRequestHeader(const AName,AValue:String):Boolean;
begin
 {}
 Result:=SetRequestHeaderEx(AName,AValue,True);
end;

{==============================================================================}

function THTTPClient.SetRequestHeaderEx(const AName,AValue:String;AReplace:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: SetRequestHeaderEx');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Name = ' + AName);
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Value = ' + AValue);
  {$ENDIF}
 
  {Set Header}
  Result:=FRequest.SetHeaderEx(AName,AValue,AReplace);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.SetRequestContentStream(AContent:TStream):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: SetRequestContentStream');
  {$ENDIF}
 
  {Set Content Stream}
  FRequest.ContentStream:=AContent;

  {Clear Content String}
  FRequest.ContentString:='';
  
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.AddRequestContentString(const AContent:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: AddRequestContentString');
  {$ENDIF}
 
  {Clear Content Stream}
  FRequest.ContentStream:=nil;
  
  {Add Content String}
  FRequest.ContentString:=FRequest.ContentString + AContent;
  
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.SetRequestContentString(const AContent:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState = HTTP_CLIENT_STATE_REQUEST then Exit;
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: SetRequestContentString');
  {$ENDIF}
 
  {Clear Content Stream}
  FRequest.ContentStream:=nil;
 
  {Set Content String}
  FRequest.ContentString:=AContent;
  
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.GetResponseHeader(const AName:String):String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: GetResponseHeader');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Name = ' + AName);
  {$ENDIF}
 
  Result:=FResponse.GetHeader(AName);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.GetResponseHeaderEx(const AName:String):TStringList;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: GetResponseHeaderEx');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client:  Name = ' + AName);
  {$ENDIF}
 
  Result:=FResponse.GetHeaderEx(AName);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.GetResponseContentStream(AContent:TStream;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: GetResponseContentStream');
  {$ENDIF}
 
  {Read Content Stream}
  Result:=FResponse.ReadContentStream(AContent,ASize);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPClient.GetResponseContentString(var AContent:String;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check State}
  if FState <= HTTP_CLIENT_STATE_REQUEST then Exit; 
 
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Client: GetResponseContentStream');
  {$ENDIF}
 
  {Read Content String}
  Result:=FResponse.ReadContentString(AContent,ASize);
 finally
  ReleaseLock;
 end; 
end;
  
{==============================================================================}
{==============================================================================}
{THTTPServerRequest}
constructor THTTPServerRequest.Create(AThread:TWinsock2TCPServerThread);
begin
 {}
 inherited Create;
 FFlags:=HTTP_REQUEST_FLAG_NONE;
 FThread:=AThread;
 FListener:=nil;
 
 URL:='';
 Method:=HTTP_METHOD_NONE;
 Version:=HTTP_VERSION_00;
 
 Params:=THTTPParams.Create;
 Headers:=THTTPHeaders.Create;
 
 Protocol:='';
 Host:='';
 Port:='';
 Path:='';
 Query:='';
 
 BasePath:='';
 BaseHost:='';
 
 if (FThread <> nil) and (FThread.Server <> nil) then FListener:=THTTPListener(FThread.Server.Listener);
end;

{==============================================================================}

destructor THTTPServerRequest.Destroy;
begin
 {}
 Params.Free;
 Headers.Free;
 inherited Destroy;
end;

{==============================================================================}

function THTTPServerRequest.GetContentReceived:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_REQUEST_FLAG_CONTENT_RECEIVED) <> 0);
end;

{==============================================================================}

function THTTPServerRequest.GetParam(const AName:String):String;
var
 Param:THTTPParam;
begin
 {}
 Result:='';
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: GetParam');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Name = ' + AName);
 {$ENDIF}
 
 Param:=Params.FindParam(AName);
 if Param = nil then Exit;
 
 Result:=Param.Value;
end;

{==============================================================================}

function THTTPServerRequest.GetHeader(const AName:String):String;
var
 Header:THTTPHeader;
begin
 {}
 Result:='';
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: GetHeader');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Name = ' + AName);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header = nil then Exit;
 
 Result:=Header.GetValue(0);
end;

{==============================================================================}

function THTTPServerRequest.GetHeaderEx(const AName:String):TStringList;
var
 Count:Integer;
 Header:THTTPHeader;
begin
 {}
 Result:=nil;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: GetHeaderEx');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Name = ' + AName);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header = nil then Exit;
 
 Result:=TStringList.Create;
 
 for Count:=0 to Header.GetCount - 1 do
  begin
   Result.Add(Header.GetValue(Count));
  end;
end;

{==============================================================================}

function THTTPServerRequest.FindHeader(const AName,AValue:String):Boolean;
var
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: FindHeader');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Name = ' + AName);
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request:  Value = ' + AValue);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header = nil then Exit;

 Result:=(Header.FindValue(AValue) <> -1);
end;
 
{==============================================================================}

function THTTPServerRequest.ReadContentStream(AContent:TStream;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: ReadContentStream');
 {$ENDIF}

 {Check Content}
 if AContent = nil then Exit;
 
 {Check Received}
 if ContentReceived then Exit;
 
 {Read Content}
 Result:=FListener.GetRequestContentStream(FThread,Self,AContent,ASize);
 if not Result then Exit;
 
 {Set Received}
 FFlags:=FFlags or HTTP_REQUEST_FLAG_CONTENT_RECEIVED;
end;

{==============================================================================}

function THTTPServerRequest.ReadContentString(var AContent:String;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Request: ReadContentString');
 {$ENDIF}
 
 {Check Received}
 if ContentReceived then Exit;
 
 {Read Content}
 Result:=FListener.GetRequestContentString(FThread,Self,AContent,ASize);
 if not Result then Exit;

 {Set Received}
 FFlags:=FFlags or HTTP_REQUEST_FLAG_CONTENT_RECEIVED;
end;

{==============================================================================}
{==============================================================================}
{THTTPServerResponse}
constructor THTTPServerResponse.Create(AThread:TWinsock2TCPServerThread);
begin
 {}
 inherited Create;
 FFlags:=HTTP_RESPONSE_FLAG_NONE;
 FThread:=AThread;
 FListener:=nil;
 
 Reason:='';
 Status:=HTTP_STATUS_NONE;
 Version:=HTTP_VERSION_00;
 
 Headers:=THTTPHeaders.Create;
 
 ContentStream:=nil;
 ContentString:='';
 
 if (FThread <> nil) and (FThread.Server <> nil) then FListener:=THTTPListener(FThread.Server.Listener);
end;
 
{==============================================================================}

destructor THTTPServerResponse.Destroy; 
begin
 {}
 Headers.Free;
 
 if ContentStream <> nil then ContentStream.Free;
 if Length(ContentString) <> 0 then SetLength(ContentString,0);
 inherited Destroy;
end;

{==============================================================================}

function THTTPServerResponse.GetStatusSent:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_STATUS_SENT) <> 0);
end;

{==============================================================================}

function THTTPServerResponse.GetHeadersSent:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_HEADERS_SENT) <> 0);
end;

{==============================================================================}

function THTTPServerResponse.GetContentSent:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_CONTENT_SENT) <> 0);
end;

{==============================================================================}

function THTTPServerResponse.GetNoCache:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_NO_CACHE) <> 0);
end;

{==============================================================================}

procedure THTTPServerResponse.SetNoCache(ANoCache:Boolean);
begin
 {}
 if ANoCache then
  begin
   FFlags:=FFlags or HTTP_RESPONSE_FLAG_NO_CACHE;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_RESPONSE_FLAG_NO_CACHE);
  end;
end;

{==============================================================================}

function THTTPServerResponse.GetConnectionClose:Boolean;
begin
 {}
 Result:=((FFlags and HTTP_RESPONSE_FLAG_CONNECTION_CLOSE) <> 0);
end;

{==============================================================================}

procedure THTTPServerResponse.SetConnectionClose(AConnectionClose:Boolean);
begin
 {}
 if AConnectionClose then
  begin
   FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONNECTION_CLOSE;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_RESPONSE_FLAG_CONNECTION_CLOSE);
  end;
end;

{==============================================================================}

function THTTPServerResponse.SetHeader(const AName,AValue:String):Boolean;
begin
 {}
 Result:=SetHeaderEx(AName,AValue,True);
end;

{==============================================================================}

function THTTPServerResponse.SetHeaderEx(const AName,AValue:String;AReplace:Boolean):Boolean;
var
 Header:THTTPHeader;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: SetHeaderEx');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response:  Name = ' + AName);
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response:  Value = ' + AValue);
 {$ENDIF}
 
 Header:=Headers.FindHeader(AName);
 if Header <> nil then
  begin
   {Check Replace}
   if AReplace then
    begin
     {Replace Value}
     Result:=Header.SetValue(0,AValue);
    end
   else
    begin   
     {Add Value}
     if Header.FindValue(AValue) <> -1 then
      begin
       Result:=True;
      end
     else
      begin
       Result:=Header.AddValue(AValue);
      end;    
    end;  
  end
 else
  begin
   {Add Header}
   Result:=Headers.AddHeader(AName,AValue);
  end;  
end;

{==============================================================================}

function THTTPServerResponse.WriteResponse:Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: WriteResponse');
 {$ENDIF}

 {Check Sent}
 if not ContentSent then
  begin
   {Check No Cache}
   if NoCache and (Headers.FindHeader(HTTP_GENERAL_HEADER_CACHE_CONTROL) = nil) then
    begin
     SetHeader(HTTP_GENERAL_HEADER_CACHE_CONTROL,HTTP_CACHE_NO_CACHE);
    end;
   
   {Check Connection Close}
   if ConnectionClose and (Headers.FindHeader(HTTP_GENERAL_HEADER_CONNECTION) = nil) then
    begin
     SetHeader(HTTP_GENERAL_HEADER_CONNECTION,HTTP_CONNECTION_CLOSE);
    end;
   
   {Check Content}
   if ContentStream <> nil then
    begin
     {Stream Content}
     {Check Content Type}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE,HTTP_CONTENT_TEXT_HTML);
      end;
   
     {Check Content Length}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(ContentStream.Size));
      end;

     {Send Status}
     if not StatusSent then if not WriteStatus then Exit;
   
     {Send Headers}
     if not HeadersSent then if not WriteHeaders then Exit;
   
     {Write Content}
     Result:=FListener.SendResponseContentStream(FThread,Self,ContentStream,ContentStream.Size);
     if not Result then Exit;
   
     {Set Sent}
     FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_SENT;
    end
   else if Length(ContentString) <> 0 then
    begin
     {String Content}
     {Check Content Type}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE,HTTP_CONTENT_TEXT_HTML);
      end;
   
     {Check Content Length}
     if Headers.FindHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH) = nil then
      begin
       SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(Length(ContentString)));
      end;
   
     {Send Status}
     if not StatusSent then if not WriteStatus then Exit;
   
     {Send Headers}
     if not HeadersSent then if not WriteHeaders then Exit;
   
     {Write Content}
     Result:=FListener.SendResponseContentString(FThread,Self,ContentString,Length(ContentString));
     if not Result then Exit;
   
     {Set Sent}
     FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_SENT;
    end
   else
    begin
     {No Content}
     {Send Status}
     if not StatusSent then if not WriteStatus then Exit;
   
     {Send Headers}
     if not HeadersSent then if not WriteHeaders then Exit;
   
     {Set Sent}
     FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_SENT;
   
     {Return Result}
     Result:=True;
    end;
  end 
 else
  begin
   Result:=True;
  end;  
end;
  
{==============================================================================}

function THTTPServerResponse.WriteStatus:Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: WriteStatus');
 {$ENDIF}
 
 {Check Sent}
 if not StatusSent then
  begin
   {Write Status}
   Result:=FListener.SendResponseLine(FThread,Self);
   if not Result then Exit;
   
   {Set Sent}
   FFlags:=FFlags or HTTP_RESPONSE_FLAG_STATUS_SENT;
  end
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function THTTPServerResponse.WriteHeaders:Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: WriteHeaders');
 {$ENDIF}
 
 {Check Sent}
 if not HeadersSent then
  begin
   {Write Headers}
   Result:=FListener.SendResponseHeaders(FThread,Self);
   if not Result then Exit;
   
   {Set Sent}
   FFlags:=FFlags or HTTP_RESPONSE_FLAG_HEADERS_SENT;
  end
 else
  begin
   Result:=True;
  end;  
end;

{==============================================================================}

function THTTPServerResponse.WriteContentStream(AContent:TStream;ASize:LongWord;ACompleted:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: WriteContentStream');
 {$ENDIF}
 
 {Check Content}
 if AContent = nil then Exit;
 
 {Check Sent}
 if ContentSent then Exit;
 
 {Send Status}
 if not StatusSent then if not WriteStatus then Exit;
 
 {Send Headers}
 if not HeadersSent then if not WriteHeaders then Exit;
 
 {Write Content}
 Result:=FListener.SendResponseContentStream(FThread,Self,AContent,ASize);
 if not Result then Exit;
   
 {Set Sent}
 if ACompleted then FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_SENT;
end;

{==============================================================================}

function THTTPServerResponse.WriteContentString(const AContent:String;ASize:LongWord;ACompleted:Boolean):Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if FThread = nil then Exit;
 
 {Check Listener}
 if FListener = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Response: WriteContentString');
 {$ENDIF}
 
 {Check Sent}
 if ContentSent then Exit;
 
 {Send Status}
 if not StatusSent then if not WriteStatus then Exit;
 
 {Send Headers}
 if not HeadersSent then if not WriteHeaders then Exit;
 
 {Write Content}
 Result:=FListener.SendResponseContentString(FThread,Self,AContent,ASize);
 if not Result then Exit;
   
 {Set Sent}
 if ACompleted then FFlags:=FFlags or HTTP_RESPONSE_FLAG_CONTENT_SENT;
end;

{==============================================================================}
{==============================================================================}
{THTTPHost}
constructor THTTPHost.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FName:='';
 FHash:=0;
 FFlags:=HTTP_HOST_FLAG_NONE;
 
 FDefaultMimeType:=HTTP_MIME_TYPE_DEFAULT;
 
 FErrors:=TLinkedList.Create;
 FAliases:=TLinkedList.Create;
 FModules:=TLinkedList.Create;
 FDocuments:=TLinkedList.Create;
 FMimeTypes:=TLinkedList.Create;
 
 LoadMimeTypes;
end;

{==============================================================================}

destructor THTTPHost.Destroy; 
begin
 {}
 AcquireLock;
 try
  FErrors.Free;
  FAliases.Free;
  FModules.Free;
  FDocuments.Free;
  FMimeTypes.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}
  
function THTTPHost.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPHost.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);
 FHash:=GenerateNameHash(FName,stringHashSize);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPHost.SetFlags(AFlags:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FFlags:=AFlags;

 ReleaseLock;
end;

{==============================================================================}

function THTTPHost.GetDefaultMimeType:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FDefaultMimeType;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPHost.SetDefaultMimeType(const ADefaultMimeType:String);
begin
 {}
 if not AcquireLock then Exit;

 FDefaultMimeType:=ADefaultMimeType;
 UniqueString(FDefaultMimeType);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPHost.GetIsDefault:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_HOST_FLAG_DEFAULT) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPHost.SetIsDefault(AIsDefault:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsDefault then
  begin
   FFlags:=FFlags or HTTP_HOST_FLAG_DEFAULT;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_HOST_FLAG_DEFAULT);
  end;
  
 ReleaseLock;
end;
 
{==============================================================================}

function THTTPHost.GetIsDomain:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_HOST_FLAG_DOMAIN) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPHost.SetIsDomain(AIsDomain:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsDomain then
  begin
   FFlags:=FFlags or HTTP_HOST_FLAG_DOMAIN;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_HOST_FLAG_DOMAIN);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPHost.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPHost.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPHost.MatchError(AStatus:LongWord):THTTPError; 
var
 Error:THTTPError;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: MatchError');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Status = ' + IntToStr(AStatus));
  {$ENDIF}
  
  {Check Errors}
  Error:=THTTPError(FErrors.First);
  while Error <> nil do
   begin
    {Check Default}
    if not Error.IsDefault then
     begin
      {Specific Error}
      if Error.Status = AStatus then
       begin
        Result:=Error;
        Exit;
       end;
     end;  

    Error:=THTTPError(Error.Next);   
   end;
   
  {Get Default Error}
  Result:=FindError(HTTP_STATUS_NONE);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.MatchDocument(const AName:String;var AAlias:THTTPAlias):THTTPDocument;
var
 Dir:String;
 Path:String;
 Hash:LongWord;
 Extension:String;
 Alias:THTTPAlias;
 Document:THTTPDocument;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: MatchDocument');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Name = ' + AName);
  {$ENDIF}
  
  {Set Alias}
  AAlias:=nil;
  
  {Check Name}
  if Length(AName) = 0 then
   begin
    {Default Document}
    Result:=FindDocument(AName);
   end
  else
   begin
    {Match Document}
    Hash:=GenerateNameHash(AName,stringHashSize);
    
    //To Do //Check Exact (None) first then Extensions then Folders then Subfolders ?
    
    {Check Documents}
    Document:=THTTPDocument(FDocuments.First);
    while Document <> nil do
     begin
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
      {$ENDIF}
      
      {Check Default}
      if not Document.IsDefault then
       begin
        {Check Extension}
        if Document.IsExtension then
         begin
          {Extension Document}
          {Get Extension}
          HTTPPathExtractExtension(AName,Extension);
          
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Extension = ' + Extension);
          {$ENDIF}
          
          {Check Extension}
          if Uppercase(Document.Name) = Uppercase(Extension) then
           begin
            Result:=Document;
            Exit;
           end;
         end
        {Check Subtree} 
        else if Document.IsSubtree then
         begin
          {Subtree Document}
          {Check Name}
          if Uppercase(Document.Name) = Uppercase(Copy(AName,1,Length(Document.Name))) then
           begin
            Result:=Document;
            Exit;
           end;
         end
        {Check Folder}
        else if Document.IsFolder then
         begin
          {Folder Document}
          {Get Dir}
          HTTPPathExtractDir(AName,Dir);
          
          {Get Path}
          HTTPPathExtractPath(AName,Path);
          
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Dir = ' + Dir);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Path = ' + Path);
          {$ENDIF}
          
          {Check Name, Dir and Path}
          if Uppercase(Document.Name) = Uppercase(AName) then
           begin
            Result:=Document;
            Exit;
           end
          else if Uppercase(Document.Name) = Uppercase(Dir) then
           begin
            Result:=Document;
            Exit;
           end
          else if Uppercase(Document.Name) = Uppercase(Path) then
           begin
            Result:=Document;
            Exit;
           end;
         end
        else
         begin
          {Specific Document}
          if Document.Hash = Hash then
           begin
            if Uppercase(Document.Name) = Uppercase(AName) then
             begin
              Result:=Document;
              Exit;
             end;
           end;
         end;         
       end; 
      
      {Check Aliases}
      Alias:=Document.MatchAlias(AName);
      if Alias <> nil then
       begin
        AAlias:=Alias;
        Result:=Document;
        Exit;
       end;       
      
      Document:=THTTPDocument(Document.Next);   
     end;
    
    {Get Default Document}
    Result:=FindDocument('');
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
  
function THTTPHost.DoGet(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base GET Method for an HTTP Host}
var
 Alias:THTTPAlias;
 Document:THTTPDocument;
begin
 {}
 Result:=False;

 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoGet');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Get Document}
 Document:=MatchDocument(ARequest.Path,Alias);
 if Document <> nil then
  begin
   {Check Alias}
   if Alias = nil then
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Document.Name;
    end
   else
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Alias = ' + Alias.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Alias.Name;
    end;    
   
   {Do Get}
   Result:=Document.DoGet(Self,ARequest,AResponse);
  end
 else
  begin
   {Check Method}
   if Assigned(FOnGet) then
    begin
     {Get Method}
     Result:=FOnGet(Self,ARequest,AResponse);
    end
   else
    begin
     {Not Found}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_NOT_FOUND;
     AResponse.Reason:=HTTP_REASON_404;
     
     {Do Error}
     Result:=DoError(ARequest,AResponse);
    end;    
  end;  
end;

{==============================================================================}

function THTTPHost.DoHead(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base HEAD Method for an HTTP Host}
var
 Alias:THTTPAlias;
 Document:THTTPDocument;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoHead');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {Get Document}
 Document:=MatchDocument(ARequest.Path,Alias);
 if Document <> nil then
  begin
   {Check Alias}
   if Alias = nil then
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Document.Name;
    end
   else
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Alias = ' + Alias.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Alias.Name;
    end;    

   {Do Head}
   Result:=Document.DoHead(Self,ARequest,AResponse);
  end
 else
  begin
   {Check Method}
   if Assigned(FOnHead) then
    begin
     {Head Method}
     Result:=FOnHead(Self,ARequest,AResponse);
    end
   else
    begin
     {Not Found}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_NOT_FOUND;
     AResponse.Reason:=HTTP_REASON_404;
     
     {Do Error}
     Result:=DoError(ARequest,AResponse);
    end;    
  end;  
end;

{==============================================================================}

function THTTPHost.DoPost(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base POST Method for an HTTP Host}
var
 Alias:THTTPAlias;
 Document:THTTPDocument;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoPost');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {Get Document}
 Document:=MatchDocument(ARequest.Path,Alias);
 if Document <> nil then
  begin
   {Check Alias}
   if Alias = nil then
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Document.Name;
    end
   else
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Alias = ' + Alias.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Alias.Name;
    end;    

   {Do Post}
   Result:=Document.DoPost(Self,ARequest,AResponse);
  end
 else
  begin
   {Check Method}
   if Assigned(FOnPost) then
    begin
     {Post Method}
     Result:=FOnPost(Self,ARequest,AResponse);
    end
   else
    begin
     {Not Found}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_NOT_FOUND;
     AResponse.Reason:=HTTP_REASON_404;
     
     {Do Error}
     Result:=DoError(ARequest,AResponse);
    end;    
  end;  
end;

{==============================================================================}

function THTTPHost.DoPut(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base PUT Method for an HTTP Host}
var
 Alias:THTTPAlias;
 Document:THTTPDocument;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoPut');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {Get Document}
 Document:=MatchDocument(ARequest.Path,Alias);
 if Document <> nil then
  begin
   {Check Alias}
   if Alias = nil then
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Document.Name;
    end
   else
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Document = ' + Document.Name);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Alias = ' + Alias.Name);
     {$ENDIF}
     
     {Set Base Path}
     ARequest.BasePath:=Alias.Name;
    end;    

   {Do Put}
   Result:=Document.DoPut(Self,ARequest,AResponse);
  end
 else
  begin
   {Check Method}
   if Assigned(FOnPut) then
    begin
     {Put Method}
     Result:=FOnPut(Self,ARequest,AResponse);
    end
   else
    begin
     {Not Found}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_NOT_FOUND;
     AResponse.Reason:=HTTP_REASON_404;
     
     {Do Error}
     Result:=DoError(ARequest,AResponse);
    end;    
  end;  
end;

{==============================================================================}

function THTTPHost.DoError(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base Error Method for an HTTP Host}
var
 Error:THTTPError;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoError');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Get Error}
 Error:=MatchError(AResponse.Status);
 if Error <> nil then
  begin
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Error = ' + IntToStr(Error.Status));
   {$ENDIF}

   {Do Error}
   Result:=Error.DoError(Self,ARequest,AResponse);
  end
 else
  begin
   {Check Method}
   if Assigned(FOnError) then
    begin
     {Error Method}
     Result:=FOnError(Self,ARequest,AResponse);
    end
   else
    begin
     {Default Error}
     if AResponse.Status = HTTP_STATUS_NONE then
      begin
       {Internal Server Error}
       AResponse.Version:=HTTP_VERSION;
       AResponse.Status:=HTTP_STATUS_INTERNAL_SERVER_ERROR;
       AResponse.Reason:=HTTP_REASON_500;
      end;
      
     {Set Content}
     AResponse.ContentString:='<html><head><title>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</title></head><body>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</body></html>' + HTTP_LINE_END; 
     
     {Return Result}
     Result:=True;
    end;    
  end;  
end;

{==============================================================================}

function THTTPHost.DoRequest(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base Request Method for an HTTP Host}
var
 Module:THTTPModule;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoRequest');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {Get Module}
 Module:=GetModule(nil);
 while Module <> nil do
  begin
   {Do Request}
   if not Module.DoRequest(Self,ARequest,AResponse) then Exit;
  
   {Get Module}
   Module:=GetModule(Module);
  end;
  
 {Check Method}
 if Assigned(FOnRequest) then
  begin
   {Request Method}
   Result:=FOnRequest(Self,ARequest,AResponse);
  end
 else
  begin 
   {Return Result}  
   Result:=True; 
  end; 
end;
 
{==============================================================================}

function THTTPHost.DoResponse(ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base Response Method for an HTTP Host}
var
 Module:THTTPModule;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DoResponse');
 {$ENDIF}
 
 {Check Request}
 if ARequest = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;

 {Get Module}
 Module:=GetModule(nil);
 while Module <> nil do
  begin
   {Do Response}
   if not Module.DoResponse(Self,ARequest,AResponse) then Exit;
  
   {Get Module}
   Module:=GetModule(Module);
  end;
  
 {Check Method}
 if Assigned(FOnResponse) then
  begin
   {Response Method}
   Result:=FOnResponse(Self,ARequest,AResponse);
  end
 else
  begin 
   {Return Result}  
   Result:=True; 
  end; 
end;

{==============================================================================}

function THTTPHost.FindError(AStatus:LongWord):THTTPError;
var
 Error:THTTPError;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Error:=THTTPError(FErrors.First);
  while Error <> nil do
   begin
    if Error.Status = AStatus then
     begin
      Result:=Error;
      Exit;
     end;

    Error:=THTTPError(Error.Next);
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.RegisterError(AError:THTTPError):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: RegisterError');
  {$ENDIF}
  
  {Check Error}
  if AError = nil then Exit; 
 
  {Check Status}
  if FindError(AError.Status) <> nil then Exit;
 
  {Check Default}
  if AError.Status = HTTP_STATUS_NONE then AError.IsDefault:=True;
 
  {Add Error}
  Result:=FErrors.Add(AError);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.DeregisterError(AError:THTTPError):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DeregisterError');
  {$ENDIF}
 
  {Check Error}
  if AError = nil then Exit; 
 
  {Check Status}
  if FindError(AError.Status) = nil then Exit;
  
  {Remove Error}
  Result:=FErrors.Remove(AError);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.FindAlias(const AName:String):THTTPAlias;
var
 Hash:LongWord;
 Alias:THTTPAlias;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Hash:=GenerateNameHash(AName,stringHashSize);
  Alias:=THTTPAlias(FAliases.First);
  while Alias <> nil do
   begin
    if Alias.Hash = Hash then
     begin
      if Uppercase(Alias.Name) = Uppercase(AName) then
       begin
        Result:=Alias;
        Exit;
       end;
     end;

    Alias:=THTTPAlias(Alias.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.MatchAlias(const AName:String):THTTPAlias; 
var
 Hash:LongWord;
 Alias:THTTPAlias;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: MatchAlias');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host:  Name = ' + AName);
  {$ENDIF}
 
  {Check Name}
  if Length(AName) = 0 then Exit;
 
  {Match Alias}
  Hash:=GenerateNameHash(AName,stringHashSize);
 
  {Check Hosts}
  Alias:=THTTPAlias(FAliases.First);
  while Alias <> nil do
   begin
    {Check Domain}
    if IsDomain then
     begin
      {Domain Host}
      if Uppercase(Copy(AName,Length(AName) - Length(Alias.Name) + 1,Length(Alias.Name))) = Uppercase(Alias.Name) then
       begin
        Result:=Alias;
        Exit;
       end; 
     end
    else
     begin
      {Specific Host}
      if Alias.Hash = Hash then
       begin
        if Uppercase(Alias.Name) = Uppercase(AName) then
         begin
          Result:=Alias;
          Exit;
         end;
       end;
     end;  
     
    Alias:=THTTPAlias(Alias.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
  
function THTTPHost.RegisterAlias(AAlias:THTTPAlias):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: RegisterAlias');
  {$ENDIF}
  
  {Check Alias}
  if AAlias = nil then Exit; 
  if Length(AAlias.Name) = 0 then Exit;
  
  {Check Name}
  if FindAlias(AAlias.Name) <> nil then Exit;
  
  {Add Alias}
  Result:=FAliases.Add(AAlias);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.DeregisterAlias(AAlias:THTTPAlias):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DeregisterAlias');
  {$ENDIF}
 
  {Check Alias}
  if AAlias = nil then Exit; 
 
  {Check Name}
  if FindAlias(AAlias.Name) = nil then Exit;
  
  {Remove Alias}
  Result:=FAliases.Remove(AAlias);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.GetModule(APrevious:THTTPModule):THTTPModule;
begin
 {}
 Result:=nil;
 
 if APrevious = nil then
  begin
   Result:=THTTPModule(FModules.First);
  end
 else
  begin
   Result:=THTTPModule(APrevious.Next);
  end;  
end;

{==============================================================================}

function THTTPHost.FindModule(AModule:THTTPModule):THTTPModule;
var
 Module:THTTPModule;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Module:=THTTPModule(FModules.First);
  while Module <> nil do
   begin
    if Module = AModule then
     begin
      Result:=Module;
      Exit;
     end;

    Module:=THTTPModule(Module.Next);
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
  
function THTTPHost.RegisterModule(AModule:THTTPModule):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: RegisterModule');
  {$ENDIF}
  
  {Check Module}
  if AModule = nil then Exit; 

  {Check Module}
  if FindModule(AModule) <> nil then Exit;
  
  {Add Module}
  Result:=FModules.Add(AModule);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.DeregisterModule(AModule:THTTPModule):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DeregisterModule');
  {$ENDIF}
 
  {Check Module}
  if AModule = nil then Exit; 
 
  {Check Module}
  if FindModule(AModule) = nil then Exit;
  
  {Remove Module}
  Result:=FModules.Remove(AModule);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.FindDocument(const AName:String):THTTPDocument;
var
 Hash:LongWord;
 Document:THTTPDocument;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Hash:=GenerateNameHash(AName,stringHashSize);
  Document:=THTTPDocument(FDocuments.First);
  while Document <> nil do
   begin
    if Document.Hash = Hash then
     begin
      if Uppercase(Document.Name) = Uppercase(AName) then
       begin
        Result:=Document;
        Exit;
       end;
     end;

    Document:=THTTPDocument(Document.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
  
function THTTPHost.RegisterDocument(ADocument:THTTPDocument):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: RegisterDocument');
  {$ENDIF}
  
  {Check Document}
  if ADocument = nil then Exit; 
 
  {Check Name}
  if FindDocument(ADocument.Name) <> nil then Exit;
  
  {Check Default}
  if Length(ADocument.Name) = 0 then ADocument.IsDefault:=True;
  
  {Add Document}
  Result:=FDocuments.Add(ADocument);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.DeregisterDocument(ADocument:THTTPDocument):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DeregisterDocument');
  {$ENDIF}
 
  {Check Document}
  if ADocument = nil then Exit; 
 
  {Check Name}
  if FindDocument(ADocument.Name) = nil then Exit;
  
  {Remove Document}
  Result:=FDocuments.Remove(ADocument);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.LoadMimeTypes:Boolean;
var
 Count:LongWord;
 MimeType:THTTPMimeType;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  for Count:=0 to HTTP_MIME_TYPE_MAX do
   begin  
    {Create MimeType}
    MimeType:=THTTPMimeType.Create;
    MimeType.Extension:=HTTP_MIME_TYPES[Count,0];
    MimeType.MimeType:=HTTP_MIME_TYPES[Count,1];
    
    {Register MimeType}
    if not RegisterMimeType(MimeType) then
     begin
      MimeType.Free;
     end; 
   end;
   
  Result:=True;  
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.FindMimeType(const AExtension:String):THTTPMimeType;
var
 Hash:LongWord;
 MimeType:THTTPMimeType;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Hash:=GenerateNameHash(AExtension,stringHashSize);
  MimeType:=THTTPMimeType(FMimeTypes.First);
  while MimeType <> nil do
   begin
    if MimeType.Hash = Hash then
     begin
      if Uppercase(MimeType.Extension) = Uppercase(AExtension) then
       begin
        Result:=MimeType;
        Exit;
       end;
     end;

    MimeType:=THTTPMimeType(MimeType.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.ResolveMimeType(const AExtension:String):String;
var
 Hash:LongWord;
 MimeType:THTTPMimeType;
begin
 {}
 Result:=DefaultMimeType;
 
 if not AcquireLock then Exit;
 try
  Hash:=GenerateNameHash(AExtension,stringHashSize);
  MimeType:=THTTPMimeType(FMimeTypes.First);
  while MimeType <> nil do
   begin
    if MimeType.Hash = Hash then
     begin
      if Uppercase(MimeType.Extension) = Uppercase(AExtension) then
       begin
        Result:=MimeType.MimeType;
        Exit;
       end;
     end;

    MimeType:=THTTPMimeType(MimeType.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.RegisterMimeType(AMimeType:THTTPMimeType):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: RegisterMimeType');
  {$ENDIF}
  
  {Check MimeType}
  if AMimeType = nil then Exit; 
  
  {Check Extension}
  if Length(AMimeType.Extension) = 0 then Exit;
  if FindMimeType(AMimeType.Extension) <> nil then Exit;
  
  {Add MimeType}
  Result:=FMimeTypes.Add(AMimeType);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPHost.DeregisterMimeType(AMimeType:THTTPMimeType):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DeregisterMimeType');
  {$ENDIF}
 
  {Check MimeType}
  if AMimeType = nil then Exit; 
 
  {Check Extension}
  if FindMimeType(AMimeType.Extension) = nil then Exit;
  
  {Remove MimeType}
  Result:=FMimeTypes.Remove(AMimeType);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{THTTPError}
constructor THTTPError.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;

 FFlags:=HTTP_ERROR_FLAG_NONE; 
 FStatus:=HTTP_STATUS_NONE;
end;

{==============================================================================}

destructor THTTPError.Destroy; 
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

procedure THTTPError.SetFlags(AFlags:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FFlags:=AFlags;

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPError.SetStatus(AStatus:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FStatus:=AStatus;

 ReleaseLock;
end;

{==============================================================================}

function THTTPError.GetIsDefault:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_ERROR_FLAG_DEFAULT) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPError.SetIsDefault(AIsDefault:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsDefault then
  begin
   FFlags:=FFlags or HTTP_ERROR_FLAG_DEFAULT;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_ERROR_FLAG_DEFAULT);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPError.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPError.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPError.DoError(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base Method for an HTTP Error}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Error: DoError');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnError) then
  begin
   {Error Method}
   Result:=FOnError(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Default Error}
   if AResponse.Status = HTTP_STATUS_NONE then
    begin
     {Internal Server Error}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_INTERNAL_SERVER_ERROR;
     AResponse.Reason:=HTTP_REASON_500;
    end;
   
   {Set Content}
   AResponse.ContentString:='<html><head><title>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</title></head><body>Error ' + HTTPStatusToString(AResponse.Status) + ' (' + AResponse.Reason + ')</body></html>' + HTTP_LINE_END;
 
   {Return Result}
   Result:=True;
  end;    
end;

{==============================================================================}
{==============================================================================}
{THTTPAlias}
constructor THTTPAlias.Create;
begin
 {}
 inherited Create;
 FLock:=MutexCreate;

 FName:='';
 FHash:=0;
end;

{==============================================================================}

destructor THTTPAlias.Destroy; 
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  MutexDestroy(FLock);
 end;
end;

{==============================================================================}

function THTTPAlias.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPAlias.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);
 FHash:=GenerateNameHash(FName,stringHashSize);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPAlias.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPAlias.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{THTTPModule}
constructor THTTPModule.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FFlags:=HTTP_MODULE_FLAG_NONE;
end;

{==============================================================================}

destructor THTTPModule.Destroy; 
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

procedure THTTPModule.SetFlags(AFlags:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FFlags:=AFlags;

 ReleaseLock;
end;

{==============================================================================}

function THTTPModule.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPModule.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
 
function THTTPModule.DoRequest(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base Request Method for an HTTP Module}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Module: DoRequest');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Check Method}
 if Assigned(FOnRequest) then
  begin
   {Request Method}
   Result:=FOnRequest(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Return Result}
   Result:=True;
  end;
end;

{==============================================================================}
  
function THTTPModule.DoResponse(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base Response Method for an HTTP Module}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Module: DoResponse');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;

 {Check Method}
 if Assigned(FOnResponse) then
  begin
   {Response Method}
   Result:=FOnResponse(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Return Result}
   Result:=True;
  end;
end;
 
{==============================================================================}
{==============================================================================}
{THTTPDocument}
constructor THTTPDocument.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FName:='';
 FHash:=0;
 FFlags:=HTTP_DOCUMENT_FLAG_NONE;
 
 FAliases:=TLinkedList.Create;
end;

{==============================================================================}

destructor THTTPDocument.Destroy; 
begin
 {}
 AcquireLock;
 try
  FAliases.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}
  
function THTTPDocument.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPDocument.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);
 FHash:=GenerateNameHash(FName,stringHashSize);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPDocument.SetFlags(AFlags:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FFlags:=AFlags;

 ReleaseLock;
end;

{==============================================================================}

function THTTPDocument.GetIsDefault:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_DOCUMENT_FLAG_DEFAULT) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPDocument.SetIsDefault(AIsDefault:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsDefault then
  begin
   FFlags:=FFlags or HTTP_DOCUMENT_FLAG_DEFAULT;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_DOCUMENT_FLAG_DEFAULT);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPDocument.GetIsFolder:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_DOCUMENT_FLAG_FOLDER) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPDocument.SetIsFolder(AIsFolder:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsFolder then
  begin
   FFlags:=FFlags or HTTP_DOCUMENT_FLAG_FOLDER;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_DOCUMENT_FLAG_FOLDER);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPDocument.GetIsSubtree:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_DOCUMENT_FLAG_SUBTREE) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPDocument.SetIsSubtree(AIsSubtree:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsSubtree then
  begin
   FFlags:=FFlags or HTTP_DOCUMENT_FLAG_SUBTREE;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_DOCUMENT_FLAG_SUBTREE);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPDocument.GetIsExtension:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=((FFlags and HTTP_DOCUMENT_FLAG_EXTENSION) <> 0);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPDocument.SetIsExtension(AIsExtension:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AIsExtension then
  begin
   FFlags:=FFlags or HTTP_DOCUMENT_FLAG_EXTENSION;
  end
 else
  begin
   FFlags:=FFlags and not(HTTP_DOCUMENT_FLAG_EXTENSION);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function THTTPDocument.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPDocument.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPDocument.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base GET Method for an HTTP Document}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Document: DoGet');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnGet) then
  begin
   {Get Method}
   Result:=FOnGet(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Method Not Allowed}
   AResponse.Version:=HTTP_VERSION;
   AResponse.Status:=HTTP_STATUS_METHOD_NOT_ALLOWED;
   AResponse.Reason:=HTTP_REASON_405;
     
   {Do Error}
   Result:=AHost.DoError(ARequest,AResponse);
  end;
end;

{==============================================================================}

function THTTPDocument.DoHead(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base HEAD Method for an HTTP Document}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Document: DoHead');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnHead) then
  begin
   {Head Method}
   Result:=FOnHead(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Method Not Allowed}
   AResponse.Version:=HTTP_VERSION;
   AResponse.Status:=HTTP_STATUS_METHOD_NOT_ALLOWED;
   AResponse.Reason:=HTTP_REASON_405;
     
   {Do Error}
   Result:=AHost.DoError(ARequest,AResponse);
  end;
end;

{==============================================================================}

function THTTPDocument.DoPost(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base POST Method for an HTTP Document}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Document: DoPost');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnPost) then
  begin
   {Post Method}
   Result:=FOnPost(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Method Not Allowed}
   AResponse.Version:=HTTP_VERSION;
   AResponse.Status:=HTTP_STATUS_METHOD_NOT_ALLOWED;
   AResponse.Reason:=HTTP_REASON_405;
     
   {Do Error}
   Result:=AHost.DoError(ARequest,AResponse);
  end;
end;

{==============================================================================}

function THTTPDocument.DoPut(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base PUT Method for an HTTP Document}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Document: DoPut');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnPut) then
  begin
   {Put Method}
   Result:=FOnPut(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Method Not Allowed}
   AResponse.Version:=HTTP_VERSION;
   AResponse.Status:=HTTP_STATUS_METHOD_NOT_ALLOWED;
   AResponse.Reason:=HTTP_REASON_405;
     
   {Do Error}
   Result:=AHost.DoError(ARequest,AResponse);
  end;
end;

{==============================================================================}

function THTTPDocument.FindAlias(const AName:String):THTTPAlias;
var
 Hash:LongWord;
 Alias:THTTPAlias;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Hash:=GenerateNameHash(AName,stringHashSize);
  Alias:=THTTPAlias(FAliases.First);
  while Alias <> nil do
   begin
    if Alias.Hash = Hash then
     begin
      if Uppercase(Alias.Name) = Uppercase(AName) then
       begin
        Result:=Alias;
        Exit;
       end;
     end;

    Alias:=THTTPAlias(Alias.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPDocument.MatchAlias(const AName:String):THTTPAlias; 
var
 Path:String;
 Hash:LongWord;
 Extension:String;
 Alias:THTTPAlias;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Document: MatchAlias');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Document:  Name = ' + AName);
  {$ENDIF}
 
  {Check Name}
  if Length(AName) = 0 then Exit;
 
  {Match Alias}
  Hash:=GenerateNameHash(AName,stringHashSize);
  
  {Check Aliases}
  Alias:=THTTPAlias(FAliases.First);
  while Alias <> nil do
   begin
    {Check Extension}
    if IsExtension then
     begin
      {Extension Document}
      {Get Extension}
      HTTPPathExtractExtension(AName,Extension);
      
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('Document:  Extension = ' + Extension);
      {$ENDIF}
      
      {Check Extension}
      if Uppercase(Alias.Name) = Uppercase(Extension) then
       begin
        Result:=Alias;
        Exit;
       end;
     end
    {Check Subtree} 
    else if IsSubtree then
     begin
      {Subtree Document}
      {Check Name}
      if Uppercase(Alias.Name) = Uppercase(Copy(AName,1,Length(Alias.Name))) then
       begin
        Result:=Alias;
        Exit;
       end;
     end
    {Check Folder}
    else if IsFolder then
     begin
      {Folder Document}
      {Get Path}
      HTTPPathExtractPath(AName,Path);
      
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('Document:  Path = ' + Path);
      {$ENDIF}
      
      {Check Name and Path}
      if Uppercase(Alias.Name) = Uppercase(AName) then
       begin
        Result:=Alias;
        Exit;
       end
      else if Uppercase(Alias.Name) = Uppercase(Path) then
       begin
        Result:=Alias;
        Exit;
       end;
     end
    else
     begin
      {Specific Document}
      if Alias.Hash = Hash then
       begin
        if Uppercase(Alias.Name) = Uppercase(AName) then
         begin
          Result:=Alias;
          Exit;
         end;
       end;
     end;         
 
    Alias:=THTTPAlias(Alias.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
  
function THTTPDocument.RegisterAlias(AAlias:THTTPAlias):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Document: RegisterAlias');
  {$ENDIF}
  
  {Check Alias}
  if AAlias = nil then Exit; 
  if Length(AAlias.Name) = 0 then Exit;
  
  {Check Name}
  if FindAlias(AAlias.Name) <> nil then Exit;
  
  {Add Alias}
  Result:=FAliases.Add(AAlias);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPDocument.DeregisterAlias(AAlias:THTTPAlias):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Host: DeregisterAlias');
  {$ENDIF}
 
  {Check Alias}
  if AAlias = nil then Exit; 
 
  {Check Name}
  if FindAlias(AAlias.Name) = nil then Exit;
  
  {Remove Alias}
  Result:=FAliases.Remove(AAlias);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{THTTPMimeType}
constructor THTTPMimeType.Create;
begin
 {}
 inherited Create;
 FLock:=MutexCreate;

 FExtension:='';
 FHash:=0;
 FMimeType:='';
end;

{==============================================================================}

destructor THTTPMimeType.Destroy; 
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  MutexDestroy(FLock);
 end;
end;

{==============================================================================}

function THTTPMimeType.GetExtension:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FExtension;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPMimeType.SetExtension(const AExtension:String);
begin
 {}
 if not AcquireLock then Exit;

 FExtension:=AExtension;
 UniqueString(FExtension);
 FHash:=GenerateNameHash(FExtension,stringHashSize);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPMimeType.GetMimeType:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FMimeType;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPMimeType.SetMimeType(const AMimeType:String);
begin
 {}
 if not AcquireLock then Exit;

 FMimeType:=AMimeType;
 UniqueString(FMimeType);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPMimeType.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPMimeType.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{THTTPRedirect}
constructor THTTPRedirect.Create;
begin
 {}
 inherited Create;

 FLocation:='';
 FPermanent:=False;
end;

{==============================================================================}

function THTTPRedirect.GetLocation:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FLocation;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPRedirect.SetLocation(const ALocation:String);
begin
 {}
 if not AcquireLock then Exit;

 FLocation:=ALocation;
 UniqueString(FLocation);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPRedirect.SetPermanent(APermanent:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FPermanent:=APermanent;

 ReleaseLock;
end;

{==============================================================================}

function THTTPRedirect.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean;
{Base GET Method for an HTTP Redirect}
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Redirect: DoGet');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnGet) then
  begin
   {Get Method}
   Result:=FOnGet(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   if Permanent then
    begin
     {Moved Permanently}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_MOVED_PERMANENT;
     AResponse.Reason:=HTTP_REASON_301;
    
     {Add Location Header}
     AResponse.SetHeader(HTTP_RESPONSE_HEADER_LOCATION,Location);
    end
   else
    begin
     {Temporary Redirect}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_TEMPORARY_REDIRECT;
     AResponse.Reason:=HTTP_REASON_307;
     
     {Add Location Header}
     AResponse.SetHeader(HTTP_RESPONSE_HEADER_LOCATION,Location);
    end;    
   
   {Set Content}
   AResponse.ContentString:='<html><head><title>' + AResponse.Reason + ' (' + HTTPStatusToString(AResponse.Status) + ')</title></head><body>' + AResponse.Reason + ' (' + HTTPStatusToString(AResponse.Status) + ') to <a href="' + Location + '">' + Location + '</a></body></html>' + HTTP_LINE_END; 
   
   {Return Result}
   Result:=True;
  end;
end;

{==============================================================================}
{==============================================================================}
{THTTPFolder}
constructor THTTPFolder.Create;
begin
 {}
 inherited Create;

 FFolder:='';
 FIndexPage:='index.html';
 FAllowCache:=True;
 FAllowListing:=True;
 FAllowSubtree:=True;
 
 FHideSubfolders:=True;
 FForceTrailingSlash:=True;
 
 {Set Subtree}
 IsSubtree:=True;
end;

{==============================================================================}

function THTTPFolder.GetFolder:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FFolder;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFolder.SetFolder(const AFolder:String);
begin
 {}
 if not AcquireLock then Exit;

 FFolder:=AFolder;
 UniqueString(FFolder);
 
 ReleaseLock;
end;

{==============================================================================}

function THTTPFolder.GetIndexPage:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FIndexPage;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFolder.SetIndexPage(const AIndexPage:String);
begin
 {}
 if not AcquireLock then Exit;

 FIndexPage:=AIndexPage;
 UniqueString(FIndexPage);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFolder.SetAllowCache(AAllowCache:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FAllowCache:=AAllowCache;

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFolder.SetAllowListing(AAllowListing:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FAllowListing:=AAllowListing;

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFolder.SetAllowSubtree(AAllowSubtree:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FAllowSubtree:=AAllowSubtree;

 ReleaseLock;
 
 {Check Subtree}
 if FAllowSubtree then
  begin
   IsSubtree:=True;
   IsFolder:=False;
  end
 else
  begin
   IsFolder:=True;
   IsSubtree:=False;
  end;  
end;

{==============================================================================}

procedure THTTPFolder.SetHideSubfolders(AHideSubfolders:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FHideSubfolders:=AHideSubfolders;

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFolder.SetForceTrailingSlash(AForceTrailingSlash:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FForceTrailingSlash:=AForceTrailingSlash;

 ReleaseLock;
end;

{==============================================================================}

function THTTPFolder.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base GET Method for an HTTP Folder}
var
 Path:String;
 Base:String;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: DoGet');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnGet) then
  begin
   {Get Method}
   Result:=FOnGet(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {Get Path}
   Path:=ReplaceChar(ARequest.Path,HTTP_PATH_SEPARATOR,DirectorySeparator); 
   
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Path: ' + Path);
   {$ENDIF}
   
   {Get Base}
   Base:=AddTrailingChar(Folder,DirectorySeparator) + StripLeadingChar(Copy(Path,Length(ARequest.BasePath) + 1,Length(Path)),DirectorySeparator);
   
   {Update Base}
   Base:=StripTrailingChar(Base,DirectorySeparator);
   
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Base: ' + Base);
   {$ENDIF}
   
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Checking file: ' + Base);
   {$ENDIF}
   
   {Check File}
   if FileExists(Base) then
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: File exists: ' + Base);
     {$ENDIF}
     
     {Get File}
     Result:=DoGetFile(AHost,ARequest,AResponse,Base);
    end
   else
    begin   
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Checking folder: ' + Base);
     {$ENDIF}

     {Check Folder}
     if DirectoryExists(Base) then
      begin
       {$IFDEF HTTP_DEBUG}
       if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Folder exists: ' + Base);
       {$ENDIF}
       
       {Check Force Slash}
       if ForceTrailingSlash and (Path <> AddTrailingChar(Path,DirectorySeparator)) then
        begin
         {Get New Path}
         Path:=AddTrailingChar(ARequest.Path,HTTP_PATH_SEPARATOR);

         {$IFDEF HTTP_DEBUG}
         if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Redirecting to force trailing slash: ' + Path);
         {$ENDIF}
         
         {Moved Permanently}
         AResponse.Version:=HTTP_VERSION;
         AResponse.Status:=HTTP_STATUS_MOVED_PERMANENT;
         AResponse.Reason:=HTTP_REASON_301;
        
         {Add Location Header}
         AResponse.SetHeader(HTTP_RESPONSE_HEADER_LOCATION,Path);

         {Set Content}
         AResponse.ContentString:='<html><head><title>' + AResponse.Reason + ' (' + HTTPStatusToString(AResponse.Status) + ')</title></head><body>' + AResponse.Reason + ' (' + HTTPStatusToString(AResponse.Status) + ') to <a href="' + Path + '">' + Path + '</a></body></html>' + HTTP_LINE_END; 
         
         Result:=True;
        end
       else
        begin
         {$IFDEF HTTP_DEBUG}
         if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Checking index: ' + AddTrailingChar(Base,DirectorySeparator) + IndexPage);
         {$ENDIF}
         
         {Check Index}
         if (Length(IndexPage) <> 0) and (FileExists(AddTrailingChar(Base,DirectorySeparator) + IndexPage)) then
          begin
           {$IFDEF HTTP_DEBUG}
           if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Index exists: ' + AddTrailingChar(Base,DirectorySeparator) + IndexPage);
           {$ENDIF}
           
           {Get Index}
           Result:=DoGetFile(AHost,ARequest,AResponse,AddTrailingChar(Base,DirectorySeparator) + IndexPage);
          end
         else
          begin
           {Check Listing}
           if AllowListing then
            begin
             {$IFDEF HTTP_DEBUG}
             if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Listing allowed: ' + Base);
             {$ENDIF}
             
             {Get Listing}
             Result:=DoGetFolder(AHost,ARequest,AResponse,Base);
            end
           else
            begin
             {$IFDEF HTTP_DEBUG}
             if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Forbidden: ' + Base);
             {$ENDIF}
  
             {Forbidden}
             AResponse.Version:=HTTP_VERSION;
             AResponse.Status:=HTTP_STATUS_FORBIDDEN;
             AResponse.Reason:=HTTP_REASON_403;
             
             {Do Error}
             Result:=AHost.DoError(ARequest,AResponse);
            end;          
          end;
        end;
      end
     else
      begin
       {$IFDEF HTTP_DEBUG}
       if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Not found: ' + Base);
       {$ENDIF}

       {Not Found}
       AResponse.Version:=HTTP_VERSION;
       AResponse.Status:=HTTP_STATUS_NOT_FOUND;
       AResponse.Reason:=HTTP_REASON_404;

       {Do Error}
       Result:=AHost.DoError(ARequest,AResponse);
      end;      
    end; 
  end;
end;

{==============================================================================}

function THTTPFolder.DoHead(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base HEAD Method for an HTTP Folder}
begin
 {}
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: DoHead');
 {$ENDIF}

 Result:=DoGet(AHost,ARequest,AResponse);
end;

{==============================================================================}

function THTTPFolder.DoGetFile(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse;const AFile:String):Boolean; 
{Note: Caller must ensure the file exists}
var
 MimeType:String;
 Extension:String;
 LastModified:String;
 ModifiedSince:String;
 UnmodifiedSince:String;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: DoGetFile');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Folder:  File = ' + AFile);
  {$ENDIF}
  
  {Get Extension}
  HTTPPathExtractExtension(AFile,Extension);
  
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Extension: ' + Extension);
  {$ENDIF}
  
  {Get MimeType}
  MimeType:=AHost.ResolveMimeType(Extension);
  
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: MimeType: ' + MimeType);
  {$ENDIF}
 
  {Get LastModified}
  LastModified:=DateTimeToHTTPDate(FileDateToDateTime(FileAge(AFile)));
  
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: LastModified: ' + LastModified);
  {$ENDIF}
  
  {Get ModifiedSince}
  ModifiedSince:=ARequest.GetHeader(HTTP_REQUEST_HEADER_IF_MODIFIED_SINCE);
  
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: ModifiedSince: ' + ModifiedSince);
  {$ENDIF}
  
  //To Do //UnmodifiedSince //See: 14.28 If-Unmodified-Since
  
  {Check ModifiedSince}
  if Uppercase(ModifiedSince) = Uppercase(LastModified) then
   begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: File not modified: ' + AFile);
    {$ENDIF}
 
    {Not Modified}
    AResponse.Version:=HTTP_VERSION;
    AResponse.Status:=HTTP_STATUS_NOT_MODIFIED;
    AResponse.Reason:=HTTP_REASON_304;
    
    {Return Result}
    Result:=True;
   end
  else
   begin      
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Opening file: ' + AFile);
    {$ENDIF}
    
    {Open File}
    FileStream:=TFileStream.Create(AFile,fmOpenRead or fmShareDenyNone);
    if FileStream <> nil then
     begin
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: File opened: ' + AFile);
      {$ENDIF}

      {Success}
      AResponse.Version:=HTTP_VERSION;
      AResponse.Status:=HTTP_STATUS_OK;
      AResponse.Reason:=HTTP_REASON_200;
    
      {Set Content Type}
      AResponse.SetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE,MimeType);
      
      {Check Allow Cache}
      if AllowCache then
       begin
        {Set Last Modified}
        AResponse.SetHeader(HTTP_ENTITY_HEADER_LAST_MODIFIED,LastModified);
       end
      else
       begin
        {Set No Cache}
        AResponse.NoCache:=True;
       end; 
      
      {Check Method}
      if ARequest.Method = HTTP_METHOD_GET then
       begin
        {Set Content}
        AResponse.ContentStream:=FileStream;
       end
      else if ARequest.Method = HTTP_METHOD_HEAD then
       begin
        {Set Content Length}
        AResponse.SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(FileStream.Size));
        
        {Close Stream}
        FileStream.Free;
       end;
      
      {Return Result}
      Result:=True;
     end
    else
     begin
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Open failed: ' + AFile);
      {$ENDIF}
      
      {Internal Server Error}
      AResponse.Version:=HTTP_VERSION;
      AResponse.Status:=HTTP_STATUS_INTERNAL_SERVER_ERROR;
      AResponse.Reason:=HTTP_REASON_500;
      
      {Do Error}
      Result:=AHost.DoError(ARequest,AResponse);
     end;
   end;
 except
  on E: Exception do
   begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Exception: ' + E.Message);
    {$ENDIF}

    {Internal Server Error}
    AResponse.Version:=HTTP_VERSION;
    AResponse.Status:=HTTP_STATUS_INTERNAL_SERVER_ERROR;
    AResponse.Reason:=HTTP_REASON_500;
    
    {Do Error}
    Result:=AHost.DoError(ARequest,AResponse);
   end;
 end;     
end;

{==============================================================================}

function THTTPFolder.DoGetFolder(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse;const AFolder:String):Boolean; 
{Note: Caller must ensure the folder exists}
var
 Path:String;
 Parent:String;
 WorkBuffer:String;
 SearchRec:TSearchRec;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: DoGetFolder');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Folder:  Folder = ' + AFolder);
 {$ENDIF}
 
 {Get Path}
 Path:=AddTrailingChar(ARequest.Path,HTTP_PATH_SEPARATOR);
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Path: ' + Path);
 {$ENDIF}
 
 {Get Parent}
 HTTPPathExtractPath(StripTrailingChar(Path,HTTP_PATH_SEPARATOR),Parent);

 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: Parent: ' + Parent);
 {$ENDIF}

 {Success}
 AResponse.Version:=HTTP_VERSION;
 AResponse.Status:=HTTP_STATUS_OK;
 AResponse.Reason:=HTTP_REASON_200;
 
 {Add Header}
 WorkBuffer:='';
 WorkBuffer:=WorkBuffer + '<html>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + ' <head>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + '  <title>Index of ' + Path + '</title>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + ' </head>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + ' <body>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + '  <h1>Index of ' + Path + '</h1>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + '  <ul><li><a href="' + AddTrailingChar(Parent,HTTP_PATH_SEPARATOR) + '"> Parent Directory</a></li>' + HTTP_LINE_END;
 
 {Add Contents}
 if FindFirst(AddTrailingChar(AFolder,DirectorySeparator) + '*.*',faAnyFile,SearchRec) = 0 then
  begin
   repeat
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Folder: SearchRec.Name: ' + SearchRec.Name);
    {$ENDIF}
    
    if (SearchRec.Attr and faDirectory) = faDirectory then
     begin
      if AllowSubtree or not(HideSubfolders) then
       begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
         begin
          WorkBuffer:=WorkBuffer + '<li><a href="' + AddTrailingChar(SearchRec.Name,HTTP_PATH_SEPARATOR) + '"> ' + SearchRec.Name + '</a></li>' + HTTP_LINE_END;
         end;
       end;  
     end
    else
     begin
      WorkBuffer:=WorkBuffer + '<li><a href="' + SearchRec.Name + '"> ' + SearchRec.Name + '</a></li>' + HTTP_LINE_END;
     end;
   until FindNext(SearchRec) <> 0;
  end;
  
 FindClose(SearchRec);
  
 {Add Footer}
 WorkBuffer:=WorkBuffer + '  </ul>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + ' </body>' + HTTP_LINE_END;
 WorkBuffer:=WorkBuffer + '</html>' + HTTP_LINE_END;
 
 {Check Method}
 if ARequest.Method = HTTP_METHOD_GET then
  begin
   {Set Content}
   AResponse.ContentString:=WorkBuffer; 
  end
 else if ARequest.Method = HTTP_METHOD_HEAD then
  begin
   {Set Content Length}
   AResponse.SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(Length(WorkBuffer)));
  end;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
{==============================================================================}
{THTTPFile}
constructor THTTPFile.Create;
begin
 {}
 inherited Create;

 FFilename:='';
 FAllowCache:=True;
end;

{==============================================================================}

function THTTPFile.GetFilename:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FFilename;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFile.SetFilename(const AFilename:String);
begin
 {}
 if not AcquireLock then Exit;

 FFilename:=AFilename;
 UniqueString(FFilename);
 
 ReleaseLock;
end;

{==============================================================================}

procedure THTTPFile.SetAllowCache(AAllowCache:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FAllowCache:=AAllowCache;

 ReleaseLock;
end;

{==============================================================================}

function THTTPFile.DoGet(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base GET Method for an HTTP File}
var
 MimeType:String;
 Extension:String;
 LastModified:String;
 ModifiedSince:String;
 UnmodifiedSince:String;
 FileStream:TFileStream;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('File: DoGet');
 {$ENDIF}
 
 {Check Host}
 if AHost = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;

 {Check Response}
 if AResponse = nil then Exit;
 
 {Check Method}
 if Assigned(FOnGet) then
  begin
   {Get Method}
   Result:=FOnGet(AHost,Self,ARequest,AResponse);
  end
 else
  begin
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('File: Checking file: ' + Filename);
   {$ENDIF}
   
   {Check File}
   if FileExists(Filename) then
    begin
     try
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('File: File exists: ' + Filename);
      {$ENDIF}
      
      {Get Extension}
      HTTPPathExtractExtension(ARequest.Path,Extension);
      
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('File: Extension: ' + Extension);
      {$ENDIF}
      
      {Get MimeType}
      MimeType:=AHost.ResolveMimeType(Extension);
      
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('File: MimeType: ' + MimeType);
      {$ENDIF}

      {Get LastModified}
      LastModified:=DateTimeToHTTPDate(FileDateToDateTime(FileAge(Filename)));
      
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('File: LastModified: ' + LastModified);
      {$ENDIF}
      
      {Get ModifiedSince}
      ModifiedSince:=ARequest.GetHeader(HTTP_REQUEST_HEADER_IF_MODIFIED_SINCE);
      
      {$IFDEF HTTP_DEBUG}
      if HTTP_LOG_ENABLED then HTTPLogDebug('File: ModifiedSince: ' + ModifiedSince);
      {$ENDIF}
      
      //To Do //UnmodifiedSince //See: 14.28 If-Unmodified-Since
      
      {Check ModifiedSince}
      if Uppercase(ModifiedSince) = Uppercase(LastModified) then
       begin
        {$IFDEF HTTP_DEBUG}
        if HTTP_LOG_ENABLED then HTTPLogDebug('File: File not modified: ' + Filename);
        {$ENDIF}

        {Not Modified}
        AResponse.Version:=HTTP_VERSION;
        AResponse.Status:=HTTP_STATUS_NOT_MODIFIED;
        AResponse.Reason:=HTTP_REASON_304;
        
        {Return Result}
        Result:=True;
       end
      else
       begin      
        {$IFDEF HTTP_DEBUG}
        if HTTP_LOG_ENABLED then HTTPLogDebug('File: Opening file: ' + Filename);
        {$ENDIF}
        
        {Open File}
        FileStream:=TFileStream.Create(Filename,fmOpenRead or fmShareDenyNone);
        if FileStream <> nil then
         begin
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('File: File opened: ' + Filename);
          {$ENDIF}

          {Success}
          AResponse.Version:=HTTP_VERSION;
          AResponse.Status:=HTTP_STATUS_OK;
          AResponse.Reason:=HTTP_REASON_200;
        
          {Set Content Type}
          AResponse.SetHeader(HTTP_ENTITY_HEADER_CONTENT_TYPE,MimeType);
          
          {Check Allow Cache}
          if AllowCache then
           begin
            {Set Last Modified}
            AResponse.SetHeader(HTTP_ENTITY_HEADER_LAST_MODIFIED,LastModified);
           end
          else
           begin
            {Set No Cache}
            AResponse.NoCache:=True;
           end; 
          
          {Check Method}
          if ARequest.Method = HTTP_METHOD_GET then
           begin
            {Set Content}
            AResponse.ContentStream:=FileStream;
           end
          else if ARequest.Method = HTTP_METHOD_HEAD then
           begin
            {Set Content Length}
            AResponse.SetHeader(HTTP_ENTITY_HEADER_CONTENT_LENGTH,IntToStr(FileStream.Size));
            
            {Close Stream}
            FileStream.Free;
           end;
          
          {Return Result}
          Result:=True;
         end
        else
         begin
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('File: Open failed: ' + Filename);
          {$ENDIF}
          
          {Internal Server Error}
          AResponse.Version:=HTTP_VERSION;
          AResponse.Status:=HTTP_STATUS_INTERNAL_SERVER_ERROR;
          AResponse.Reason:=HTTP_REASON_500;
          
          {Do Error}
          Result:=AHost.DoError(ARequest,AResponse);
         end;
       end;
     except
      on E: Exception do
       begin
        {$IFDEF HTTP_DEBUG}
        if HTTP_LOG_ENABLED then HTTPLogDebug('File: Exception: ' + E.Message);
        {$ENDIF}

        {Internal Server Error}
        AResponse.Version:=HTTP_VERSION;
        AResponse.Status:=HTTP_STATUS_INTERNAL_SERVER_ERROR;
        AResponse.Reason:=HTTP_REASON_500;
        
        {Do Error}
        Result:=AHost.DoError(ARequest,AResponse);
       end;
     end;     
    end
   else
    begin
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('File: File not found: ' + Filename);
     {$ENDIF}

     {Not Found}
     AResponse.Version:=HTTP_VERSION;
     AResponse.Status:=HTTP_STATUS_NOT_FOUND;
     AResponse.Reason:=HTTP_REASON_404;

     {Do Error}
     Result:=AHost.DoError(ARequest,AResponse);
    end;    
  end;
end;

{==============================================================================}

function THTTPFile.DoHead(AHost:THTTPHost;ARequest:THTTPServerRequest;AResponse:THTTPServerResponse):Boolean; 
{Base HEAD Method for an HTTP File}
begin
 {}
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('File: DoHead');
 {$ENDIF}

 Result:=DoGet(AHost,ARequest,AResponse);
end;

{==============================================================================}
{==============================================================================}
{THTTPListener}
constructor THTTPListener.Create;
begin
 {}
 inherited Create;
 BoundPort:=HTTP_PORT_DEFAULT;
 UseNagle:=False; {Nagle is not recommended for HTTP (Will often delay the last segment without reason)}
 
 FLock:=CriticalSectionCreate;
 
 FHosts:=TLinkedList.Create;
 
 FServer:=HTTP_SERVER_STRING;
 
 {Add Default Host}
 FHost:=THTTPHost.Create;
 FHost.Name:='';
 FHost.IsDefault:=True;
 FHosts.Add(FHost);
end;
 
{==============================================================================}
 
destructor THTTPListener.Destroy; 
begin
 {}
 AcquireLock;
 try
  FHost.Free;
  FHosts.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function THTTPListener.GetServer:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FServer;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THTTPListener.SetServer(const AServer:String);
begin
 {}
 if not AcquireLock then Exit;

 FServer:=AServer;
 UniqueString(FServer);
 
 ReleaseLock;
end;

{==============================================================================}
 
function THTTPListener.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THTTPListener.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;
 
{==============================================================================}

procedure THTTPListener.DoConnect(AThread:TWinsock2TCPServerThread); 
var
 Buffer:THTTPBuffer;
begin
 {}
 inherited DoConnect(AThread);
 
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: DoConnect');
 {$ENDIF}
 
 {Create Buffer}
 Buffer:=THTTPBuffer.Create(HTTP_BUFFER_SIZE);
 
 {Update Thread}
 AThread.Data:=Buffer;
end;
 
{==============================================================================}

procedure THTTPListener.DoDisconnect(AThread:TWinsock2TCPServerThread); 
begin
 {}
 inherited DoDisconnect(AThread);
 
 if AThread = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: DoDisconnect');
 {$ENDIF}
end;
 
{==============================================================================}
  
function THTTPListener.DoExecute(AThread:TWinsock2TCPServerThread):Boolean;
var
 Host:THTTPHost;
 Alias:THTTPAlias;
 WorkBuffer:String;
 Request:THTTPServerRequest;
 Response:THTTPServerResponse;
begin
 {}
 Result:=inherited DoExecute(AThread);
 if not Result then Exit;
 
 Result:=False;

 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: DoExecute');
 {$ENDIF}
 
 {Check Connected}
 if AThread.Server.Connected then
  begin
   {Create Request}
   Request:=THTTPServerRequest.Create(AThread);
   try
    {Create Response}
    Response:=THTTPServerResponse.Create(AThread);
    try
     {Get Request}
     if not GetRequestLine(AThread,Request) then Exit;
     
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  URL = ' + Request.URL);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Method = ' + HTTPMethodToString(Request.Method));
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Version = ' + HTTPVersionToString(Request.Version));
     {$ENDIF}
     
     {Parse URL}
     if not HTTPParseURI(Request.URL,Request.Protocol,Request.Host,Request.Port,Request.Path,Request.Query) then Exit;

     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Protocol = ' + Request.Protocol);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host = ' + Request.Host);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Port = ' + Request.Port);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Path = ' + Request.Path);
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Query = ' + Request.Query);
     {$ENDIF}
     
     {Add Server Header}
     Response.SetHeader(HTTP_RESPONSE_HEADER_SERVER,Server);

     {Add Date Header}
     Response.SetHeader(HTTP_GENERAL_HEADER_DATE,FileTimeToHTTPDate(GetCurrentTime));
     
     {Check Version}
     case Request.Version of
      HTTP_VERSION_10,HTTP_VERSION_11:begin
        {Get Headers}
        if not GetRequestHeaders(AThread,Request) then Exit;
        
        {Get Host}
        WorkBuffer:=Request.GetHeader(HTTP_REQUEST_HEADER_HOST);
        if Length(Request.Host) = 0 then
         begin
          HTTPParseHost(WorkBuffer,Request.Host,Request.Port);
        
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host Header = ' + WorkBuffer);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:   Name = ' + Request.Host);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:   Port = ' + Request.Port);
          {$ENDIF}
         end;
        Host:=MatchHost(Request.Host,Alias);
        if Host = nil then Exit;
        
        {Check Alias}
        if Alias = nil then
         begin
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host Name = ' + Host.Name);
          {$ENDIF}
          
          {Set Base Host}
          Request.BaseHost:=Host.Name;
         end
        else
         begin
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host Name = ' + Host.Name);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Alias Name = ' + Alias.Name);
          {$ENDIF}
          
          {Set Base Host}
          Request.BaseHost:=Alias.Name;
         end;
        
        {Check Connection Close}
        if Request.FindHeader(HTTP_GENERAL_HEADER_CONNECTION,HTTP_CONNECTION_CLOSE) then
         begin
          Response.ConnectionClose:=True;
         end;
        
        {Check Version}
        if Request.Version = HTTP_VERSION_10 then
         begin
          Response.ConnectionClose:=True;
         end;
         
        {Check Method}
        case Request.Method of
         HTTP_METHOD_GET:begin
           {Parse Query}
           if not HTTPParseQuery(Request.Query,Request.Params) then Exit;
           
           {Do Request}
           if not Host.DoRequest(Request,Response) then Exit;
           
           {Do Get}
           if not Host.DoGet(Request,Response) then Exit;
           
           {Do Response}
           if not Host.DoResponse(Request,Response) then Exit;
           
           {Write Response}
           Result:=Response.WriteResponse;
          end;
         HTTP_METHOD_HEAD:begin
           {Parse Query}
           if not HTTPParseQuery(Request.Query,Request.Params) then Exit;
           
           {Do Request}
           if not Host.DoRequest(Request,Response) then Exit;
           
           {Do Head}
           if not Host.DoHead(Request,Response) then Exit;
           
           {Do Response}
           if not Host.DoResponse(Request,Response) then Exit;
           
           {Write Response}
           Result:=Response.WriteResponse;
          end;
         HTTP_METHOD_POST:begin
           {Parse Query}
           if not HTTPParseQuery(Request.Query,Request.Params) then Exit;
           
           {Do Request}
           if not Host.DoRequest(Request,Response) then Exit;
           
           {Do Post}
           if not Host.DoPost(Request,Response) then Exit;
           
           {Do Response}
           if not Host.DoResponse(Request,Response) then Exit;
           
           {Write Response}
           Result:=Response.WriteResponse;
          end;
         HTTP_METHOD_PUT:begin
           {Parse Query}
           if not HTTPParseQuery(Request.Query,Request.Params) then Exit;
           
           {Do Request}
           if not Host.DoRequest(Request,Response) then Exit;
           
           {Do Put}
           if not Host.DoPut(Request,Response) then Exit;
           
           {Do Response}
           if not Host.DoResponse(Request,Response) then Exit;
           
           {Write Response}
           Result:=Response.WriteResponse;
          end;
         else
          begin
           {Method Not Implemented}
           Response.Version:=HTTP_VERSION;
           Response.Status:=HTTP_STATUS_NOT_IMPLEMENTED;
           Response.Reason:=HTTP_REASON_501;
        
           {Parse Query}
           if not HTTPParseQuery(Request.Query,Request.Params) then Exit;
        
           {Do Request}
           if not Host.DoRequest(Request,Response) then Exit;
        
           {Do Error}
           if not Host.DoError(Request,Response) then Exit;
           
           {Do Response}
           if not Host.DoResponse(Request,Response) then Exit;
           
           {Write Response}
           Result:=Response.WriteResponse;
          end;          
        end;  
       end;
      else
       begin
        {Version Not Supported}
        Response.Version:=HTTP_VERSION;
        Response.Status:=HTTP_STATUS_VERSION_NOT_SUPPORTED;
        Response.Reason:=HTTP_REASON_505;
        
        {Set Connection Close}
        Response.ConnectionClose:=True;
        
        {Get Host}
        WorkBuffer:=Request.GetHeader(HTTP_REQUEST_HEADER_HOST);
        if Length(Request.Host) = 0 then
         begin
          HTTPParseHost(WorkBuffer,Request.Host,Request.Port);
        
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host Header = ' + WorkBuffer);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:   Name = ' + Request.Host);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:   Port = ' + Request.Port);
          {$ENDIF}
         end;
        Host:=MatchHost(Request.Host,Alias);
        if Host = nil then Exit;
        
        {Check Alias}
        if Alias = nil then
         begin
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host Name = ' + Host.Name);
          {$ENDIF}
          
          {Set Base Host}
          Request.BaseHost:=Host.Name;
         end
        else
         begin
          {$IFDEF HTTP_DEBUG}
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Host Name = ' + Host.Name);
          if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Alias Name = ' + Alias.Name);
          {$ENDIF}
          
          {Set Base Host}
          Request.BaseHost:=Alias.Name;
         end;
        
        {Parse Query}
        if not HTTPParseQuery(Request.Query,Request.Params) then Exit;
        
        {Do Request}
        if not Host.DoRequest(Request,Response) then Exit;
        
        {Do Error}
        if not Host.DoError(Request,Response) then Exit;
        
        {Do Response}
        if not Host.DoResponse(Request,Response) then Exit;
        
        {Write Response}
        Result:=Response.WriteResponse;
       end;     
     end;
     
     {Check Connection Close}
     if Response.ConnectionClose then AThread.Server.Disconnect;
    finally
     Response.Free;
    end;
   finally
    Request.Free;
   end;
  end;
end;
 
{==============================================================================}
 
function THTTPListener.MatchHost(const AName:String;var AAlias:THTTPAlias):THTTPHost;
var
 Hash:LongWord;
 Host:THTTPHost;
 Alias:THTTPAlias;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: MatchHost');
  if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Name = ' + AName);
  {$ENDIF}
 
  {Set Alias}
  AAlias:=nil;
  
  {Check Name}
  if Length(AName) = 0 then
   begin
    {Default Host}
    Result:=FindHost(AName);
   end
  else
   begin
    {Match Host}
    Hash:=GenerateNameHash(AName,stringHashSize);
    
    {Check Hosts}
    Host:=THTTPHost(FHosts.First);
    while Host <> nil do
     begin
      {Check Default}
      if not Host.IsDefault then
       begin
        {Check Domain}
        if Host.IsDomain then
         begin
          {Domain Host}
          if Uppercase(Copy(AName,Length(AName) - Length(Host.Name) + 1,Length(Host.Name))) = Uppercase(Host.Name) then
           begin
            Result:=Host;
            Exit;
           end; 
         end
        else
         begin
          {Specific Host}
          if Host.Hash = Hash then
           begin
            if Uppercase(Host.Name) = Uppercase(AName) then
             begin
              Result:=Host;
              Exit;
             end;
           end;
         end;         
       end; 
       
      {Check Aliases}
      Alias:=Host.MatchAlias(AName);
      if Alias <> nil then
       begin
        AAlias:=Alias;
        Result:=Host;
        Exit;
       end;       
      
      Host:=THTTPHost(Host.Next);   
     end;
    
    {Get Default Host}
    Result:=FindHost('');
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPListener.GetRequestLine(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 WorkBuffer:String;
 Buffer:THTTPBuffer;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: GetRequestLine');
 {$ENDIF}
 
 {Get Buffer}
 Buffer:=THTTPBuffer(AThread.Data);
 if Buffer = nil then Exit;
 
 {Get Request Line}
 Completed:=False;
 WorkBuffer:='';
 while not(Completed) do
  begin
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer FStart = ' + IntToStr(Buffer.FStart));
   if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer Count = ' + IntToStr(Buffer.Count));
   {$ENDIF}
   
   {Read from Buffer}
   while Buffer.Count > 0 do
    begin
     {Read Value}
     Value:=Buffer.ReadData;
     
     {Check for CR LF}
     if not(Value in [#10,#13]) then
      begin
       WorkBuffer:=WorkBuffer + Value;
      end
     else
      begin
       {Check for LF}
       if Value = #10 then
        begin
         Completed:=True;
         Break;
        end;
      end;      
    end;
   
   {Check Completed}
   if Completed then Break;
   
   {Read from Socket}
   Data:=Buffer.WriteLock(Size);
   if Data = nil then Exit;
   try
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer FStart = ' + IntToStr(Buffer.FStart));
    if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer Write Size = ' + IntToStr(Size));
    {$ENDIF}

    Count:=0;
      
    {Read Available}
    if not AThread.Server.ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer Write Count = ' + IntToStr(Count));
    {$ENDIF}
   finally
    Buffer.WriteUnlock(Count);
   end; 
  end;  

 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Request = ' + WorkBuffer);
 {$ENDIF}
  
 {Parse Request Line}
 if not HTTPParseRequestLine(WorkBuffer,ARequest.Method,ARequest.URL,ARequest.Version) then Exit;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function THTTPListener.GetRequestHeaders(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest):Boolean;
var
 Value:Char;
 Data:Pointer;
 Size:LongWord;
 Count:LongWord;
 Closed:Boolean;
 Completed:Boolean;
 WorkBuffer:String;
 HeaderName:String;
 HeaderValue:String;
 Buffer:THTTPBuffer;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: GetRequestHeaders');
 {$ENDIF}
 
 {Get Buffer}
 Buffer:=THTTPBuffer(AThread.Data);
 if Buffer = nil then Exit;

 {Get Request Headers}
 repeat
  WorkBuffer:='';
  
  {Get Request Header}
  Completed:=False;
  while not(Completed) do
   begin
    {$IFDEF HTTP_DEBUG}
    if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer FStart = ' + IntToStr(Buffer.FStart));
    if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer Count = ' + IntToStr(Buffer.Count));
    {$ENDIF}
   
    {Read from Buffer}
    while Buffer.Count > 0 do
     begin
      {Read Value}
      Value:=Buffer.ReadData;
     
      {Check for CR LF}
      if not(Value in [#10,#13]) then
       begin
        WorkBuffer:=WorkBuffer + Value;
       end
      else
       begin
        {Check for LF}
        if Value = #10 then
         begin
          Completed:=True;
          Break;
         end;
       end;      
     end;
   
    {Check Completed}
    if Completed then Break;
   
    {Read from Socket}
    Data:=Buffer.WriteLock(Size);
    if Data = nil then Exit;
    try
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer FStart = ' + IntToStr(Buffer.FStart));
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer Write Size = ' + IntToStr(Size));
     {$ENDIF}
 
     Count:=0;
       
     {Read Available}
     if not AThread.Server.ReadAvailable(Data,Size,LongInt(Count),Closed) then Exit;
    
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Buffer Write Count = ' + IntToStr(Count));
     {$ENDIF}
    finally
     Buffer.WriteUnlock(Count);
    end; 
   end;  

  {Check Request Header}
  if Length(WorkBuffer) = 0 then Break;

  {$IFDEF HTTP_DEBUG}
  if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Header = ' + WorkBuffer);
  {$ENDIF}

  {Parse Request Header}
  if not HTTPParseHeader(WorkBuffer,HeaderName,HeaderValue) then Exit;
  
  {Add Request Header} //To Do //This will fail if a header is repeated or split, how to handle ? //See RFCs //Allow Multiple Headers and Folded Headers //See RFC (Check for Space or Tab)
  if not ARequest.Headers.AddHeader(HeaderName,HeaderValue) then Exit;
  
 until Length(WorkBuffer) = 0;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function THTTPListener.GetRequestContentStream(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest;AContent:TStream;ASize:LongWord):Boolean;
{If Size is 0 then read the entire content to the stream}
var
 Buffer:THTTPBuffer;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: GetRequestContentStream');
 {$ENDIF}
 
 {Get Buffer}
 Buffer:=THTTPBuffer(AThread.Data);
 if Buffer = nil then Exit;
 
 //To Do //ReadLock/ReadUnlock
end;

{==============================================================================}

function THTTPListener.GetRequestContentString(AThread:TWinsock2TCPServerThread;ARequest:THTTPServerRequest;var AContent:String;ASize:LongWord):Boolean;
{If Size is 0 then read the entire content to the string}
var
 Buffer:THTTPBuffer;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;

 {Check Request}
 if ARequest = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: GetRequestContentString');
 {$ENDIF}
 
 {Get Buffer}
 Buffer:=THTTPBuffer(AThread.Data);
 if Buffer = nil then Exit;
 
 //To Do //ReadLock/ReadUnlock
end;

{==============================================================================}

function THTTPListener.SendResponseLine(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse):Boolean;
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: SendResponseLine');
 {$ENDIF}

 {Get Response Line}
 if not HTTPBuildResponseLine(AResponse.Version,AResponse.Status,AResponse.Reason,WorkBuffer) then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Response = ' + WorkBuffer);
 {$ENDIF}

 {Send Response Line}
 if not AThread.Server.WriteData(PChar(WorkBuffer + HTTP_LINE_END),Length(WorkBuffer + HTTP_LINE_END)) then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function THTTPListener.SendResponseHeaders(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse):Boolean;
var
 Count:Integer;
 WorkBuffer:String;
 Header:THTTPHeader;
begin
 {}
 Result:=False;

 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: SendResponseHeaders');
 {$ENDIF}

 {Get Header}
 Header:=AResponse.Headers.GetHeader(nil);
 while Header <> nil do
  begin
   for Count:=0 to Header.GetCount - 1 do
    begin
     {Build Header}
     if Count = 0 then
      begin
       if not HTTPBuildHeader(Header.Name,Header.GetValue(Count),WorkBuffer) then Exit;
      end
     else
      begin
       if not HTTPBuildHeader('',Header.GetValue(Count),WorkBuffer) then Exit;
      end;

     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Header = ' + WorkBuffer);
     {$ENDIF}
     
     {Send Header}
     if not AThread.Server.WriteData(PChar(WorkBuffer + HTTP_LINE_END),Length(WorkBuffer + HTTP_LINE_END)) then Exit;
    end;
   
   {Get Header}
   Header:=AResponse.Headers.GetHeader(Header);
  end;

 {Send Header End}
 if not AThread.Server.WriteData(PChar(HTTP_LINE_END),Length(HTTP_LINE_END)) then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function THTTPListener.SendResponseContentStream(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse;AContent:TStream;ASize:LongWord):Boolean;
var
 Buffer:Pointer;
 BytesRemain:Int64;
 BlockSize:LongWord;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: SendResponseContentStream');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Content}
 if AContent = nil then Exit;
 
 {Check Size}
 if ASize > AContent.Size then Exit;
 
 {Allocate Buffer}
 BlockSize:=Min(AThread.Server.SegmentSize,SIZE_2M - (SIZE_2M mod AThread.Server.MaxSegmentSize)); {SIZE_256K;}
 Buffer:=GetMem(BlockSize);
 if Buffer = nil then Exit;
 try
  {Get Size}
  BytesRemain:=ASize;
  
  {Send Content}
  while BytesRemain > 0 do
   begin
    if BytesRemain >= BlockSize then 
     begin
      AContent.ReadBuffer(Buffer^,BlockSize);
      if not AThread.Server.WriteData(Buffer,BlockSize) then Exit;
      
      Dec(BytesRemain,BlockSize);
     end
    else
     begin
      AContent.ReadBuffer(Buffer^,BytesRemain);
      if not AThread.Server.WriteData(Buffer,BytesRemain) then Exit;
      
      BytesRemain:=0;
     end;
   end;
 
  {Return Result}
  Result:=True;
 finally
  FreeMem(Buffer);
 end;
end;

{==============================================================================}

function THTTPListener.SendResponseContentString(AThread:TWinsock2TCPServerThread;AResponse:THTTPServerResponse;const AContent:String;ASize:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Thread}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {Check Response}
 if AResponse = nil then Exit;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener: SendResponseContentString');
 if HTTP_LOG_ENABLED then HTTPLogDebug('Listener:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Check Size}
 if ASize > Length(AContent) then Exit;
 
 {Send Content}
 if not AThread.Server.WriteData(PChar(AContent),ASize) then Exit;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}
 
function THTTPListener.FindHost(const AName:String):THTTPHost;
var
 Hash:LongWord;
 Host:THTTPHost;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Hash:=GenerateNameHash(AName,stringHashSize);
  Host:=THTTPHost(FHosts.First);
  while Host <> nil do
   begin
    if Host.Hash = Hash then
     begin
      if Uppercase(Host.Name) = Uppercase(AName) then
       begin
        Result:=Host;
        Exit;
       end;
     end;

    Host:=THTTPHost(Host.Next);   
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
  
function THTTPListener.RegisterHost(AHost:THTTPHost):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check Host}
  if AHost = nil then Exit;
  
  {Check Name}
  if FindHost(AHost.Name) <> nil then Exit;
  
  {Check Default}
  if Length(AHost.Name) = 0 then AHost.IsDefault:=True;
  
  {Add Host}
  Result:=FHosts.Add(AHost);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPListener.DeregisterHost(AHost:THTTPHost):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check Host}
  if AHost = nil then Exit;
  
  {Check Name}
  if FindHost(AHost.Name) = nil then Exit;
  
  {Remove Host}
  Result:=FHosts.Remove(AHost);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THTTPListener.FindError(const AHost:String;AStatus:LongWord):THTTPError;
var
 Host:THTTPHost;
begin
 {}
 Result:=nil;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;
 
 {Find Error}
 Result:=Host.FindError(AStatus);
end;

{==============================================================================}
  
function THTTPListener.RegisterError(const AHost:String;AError:THTTPError):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;

 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Register Error}
 Result:=Host.RegisterError(AError);
end;

{==============================================================================}

function THTTPListener.DeregisterError(const AHost:String;AError:THTTPError):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;

 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Deregister Error}
 Result:=Host.DeregisterError(AError);
end;

{==============================================================================}

function THTTPListener.GetModule(const AHost:String;APrevious:THTTPModule):THTTPModule;
var
 Host:THTTPHost;
begin
 {}
 Result:=nil;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;
 
 {Get Module}
 Result:=Host.GetModule(APrevious);
end;

{==============================================================================}

function THTTPListener.FindModule(const AHost:String;AModule:THTTPModule):THTTPModule;
var
 Host:THTTPHost;
begin
 {}
 Result:=nil;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;
 
 {Find Module}
 Result:=Host.FindModule(AModule);
end;

{==============================================================================}
  
function THTTPListener.RegisterModule(const AHost:String;AModule:THTTPModule):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;

 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Register Module}
 Result:=Host.RegisterModule(AModule);
end;

{==============================================================================}

function THTTPListener.DeregisterModule(const AHost:String;AModule:THTTPModule):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;

 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Deregister Module}
 Result:=Host.DeregisterModule(AModule);
end;

{==============================================================================}
  
function THTTPListener.FindDocument(const AHost,AName:String):THTTPDocument;
var
 Host:THTTPHost;
begin
 {}
 Result:=nil;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;
 
 {Find Document}
 Result:=Host.FindDocument(AName);
end;

{==============================================================================}
  
function THTTPListener.RegisterDocument(const AHost:String;ADocument:THTTPDocument):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;

 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Register Document}
 Result:=Host.RegisterDocument(ADocument);
end;

{==============================================================================}

function THTTPListener.DeregisterDocument(const AHost:String;ADocument:THTTPDocument):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;

 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Deregister Document}
 Result:=Host.DeregisterDocument(ADocument);
end;

{==============================================================================}

function THTTPListener.LoadMimeTypes(const AHost:String):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Load Mime Types}
 Result:=Host.LoadMimeTypes;
end;

{==============================================================================}

function THTTPListener.FindMimeType(const AHost,AExtension:String):THTTPMimeType;
var
 Host:THTTPHost;
begin
 {}
 Result:=nil;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Find Mime Type}
 Result:=Host.FindMimeType(AExtension);
end;

{==============================================================================}

function THTTPListener.ResolveMimeType(const AHost,AExtension:String):String;
var
 Host:THTTPHost;
begin
 {}
 Result:='';
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Resolve Mime Type}
 Result:=Host.ResolveMimeType(AExtension);
end;

{==============================================================================}

function THTTPListener.RegisterMimeType(const AHost:String;AMimeType:THTTPMimeType):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Register Mime Type}
 Result:=Host.RegisterMimeType(AMimeType);
end;

{==============================================================================}

function THTTPListener.DeregisterMimeType(const AHost:String;AMimeType:THTTPMimeType):Boolean;
var
 Host:THTTPHost;
begin
 {}
 Result:=False;
 
 {Find Host}
 Host:=FindHost(AHost);
 if Host = nil then Exit;

 {Deregister Mime Type}
 Result:=Host.DeregisterMimeType(AMimeType);
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HTTPInit;
begin
 {}
 {Check Initialized}
 if HTTPInitialized then Exit;
 
 {Initialize Logging}
 HTTP_LOG_ENABLED:=(HTTP_DEFAULT_LOG_LEVEL <> HTTP_LOG_LEVEL_NONE); 
 
 HTTPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HTTP Functions}

{==============================================================================}
{==============================================================================}
{HTTP Helper Functions}
function HTTPEncode(const AValue:String;AReserved:THTTPReservedChars):String;
var
 Next:Char;     
 Code:String[2];
 Buffer:PChar;  
 Count:LongWord;
 Total:LongWord;
begin
 {}
 {Get Length}
 Total:=Length(AValue);
 SetLength(Result,Total * 3); {The maximum possible length of the Result is 3 times the length of Value}

 {Check Length}
 if Total = 0 then Exit;
 
 {Get Buffer} 
 Buffer:=PChar(Result);
  
 {Check Buffer}  
 for Count:=1 to Total do
  begin
   {Get Next}
   Next:=AValue[Count];
   
   {Check Next}
   if Next in AReserved then
    begin
     {Encode Next}
     Buffer^:='%';
     Inc(Buffer);
     
     {Get Code}
     Code:=IntToHex(Ord(Next),2);
     
     {Encode Next}
     Buffer^:=Code[1];
     Inc(Buffer);
     Buffer^:=Code[2];
     Inc(Buffer);
     
    end
   else
    begin
     {Write Next}
     Buffer^:=Next;
     
     {Update Buffer}
     Inc(Buffer);
    end;
  end;

 {Update Result}
 SetLength(Result,Buffer - PChar(Result));
end;

{==============================================================================}

function HTTPDecode(const AValue:String;AQuery:Boolean):String;
var
 Next:Char;
 Code:String;
 Buffer:PChar; 
 Value:Integer;
 Count:LongWord;
 Total:LongWord;
begin
 {}
 {Get Length}
 Total:=Length(AValue);
 SetLength(Result,Total);  {The maximum possible length of the Result is the length of Value}
 
 {Check Length}
 if Total = 0 then Exit;

 {Get Buffer} 
 Buffer:=PChar(Result);
 
 {Check Buffer}
 Count:=1;
 while Count <= Total do
  begin
   {Get Next}
   Next:=AValue[Count];
   
   {Check Next}
   if Next <> '%' then
    begin
     if AQuery and (Next = '+') then
      begin
       Next:=' ';
      end;
      
     {Write Next}
     Buffer^:=Next;
     
     {Update Buffer}
     Inc(Buffer);
    end
   else if Count < (Total - 1) then
    begin
     {Get Code}
     Code:='$' + Copy(AValue,Count + 1,2);
     
     {Get Value}
     Value:=StrToIntDef(Code,-1);
     if (Value >= 0) and (Value <= 255) then
      begin
       {Write Value}
       Buffer^:=Char(Value);
       
       {Update Buffer}
       Inc(Buffer);
       
       {Update Count}
       Inc(Count,2);
      end
     else
      begin
       {Write Next}
       Buffer^:=Next;

       {Update Buffer}
       Inc(Buffer);
      end;      
    end;    
    
   {Update Count}
   Inc(Count);
  end;
 
 {Update Result}
 SetLength(Result,Buffer - PChar(Result));
end;

{==============================================================================}

function HTTPParseURI(const AURI:String;var AProtocol,AHost,APort,APath,AQuery:String):Boolean;
var
 URIBuffer:String;
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Check URI}
 if Length(AURI) = 0 then Exit;
 URIBuffer:=AURI;

 {Get Query}
 WorkBuffer:=GetFirstWord(URIBuffer,HTTP_QUERY_SEPARATOR);
 if Length(URIBuffer) <> 0 then
  begin
   AQuery:=URIBuffer; {HTTPDecode(URIBuffer);} {Do not decode}
  end
 else
  begin
   AQuery:='';
  end;  
 URIBuffer:=WorkBuffer;

 {Get Protocol}
 WorkBuffer:=GetFirstWord(URIBuffer,HTTP_PROTOCOL_SEPARATOR);
 if Length(URIBuffer) <> 0 then
  begin
   AProtocol:=WorkBuffer;
  end
 else
  begin
   AProtocol:='';
   URIBuffer:=WorkBuffer;
  end;  

 {Get Path}
 WorkBuffer:=GetFirstWord(URIBuffer,HTTP_PATH_SEPARATOR);
 if Length(URIBuffer) <> 0 then
  begin
   APath:=HTTP_PATH_SEPARATOR + HTTPDecode(URIBuffer);
  end
 else
  begin
   APath:=HTTP_PATH_SEPARATOR;
  end;
 URIBuffer:=WorkBuffer;
 
 {Get Host}
 WorkBuffer:=GetFirstWord(URIBuffer,HTTP_PORT_SEPARATOR);
 if Length(WorkBuffer) <> 0 then
  begin
   AHost:=WorkBuffer;
  end
 else
  begin
   AHost:='';
  end;
  
 {Get Port}
 WorkBuffer:=GetFirstWord(URIBuffer,HTTP_PORT_SEPARATOR);
 if Length(WorkBuffer) <> 0 then
  begin
   APort:=WorkBuffer;
  end
 else
  begin
   APort:='';
  end;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPBuildURI(const AProtocol,AHost,APort,APath,AQuery:String;var AURI:String):Boolean;
begin
 {}
 Result:=False;
 
 {Set URI}
 AURI:='';
 
 {Get Protocol}
 if Length(AProtocol) <> 0 then
  begin
   AURI:=AProtocol + HTTP_PROTOCOL_SEPARATOR;
  end;
  
 {Get Host}
 if Length(AHost) <> 0 then
  begin
   AURI:=AURI + AHost;
   
   {Get Port}
   if (Length(APort) <> 0) and (StrToIntDef(APort,0) <> 0) then
    begin
     AURI:=AURI + HTTP_PORT_SEPARATOR + APort;
    end;
  end
 else
  begin
   {Check Protocol}
   if Length(AProtocol) <> 0 then Exit;
  end;
  
 {Get Path}
 if Length(APath) <> 0 then
  begin
   AURI:=AURI + APath;
  end
 else
  begin
   {Add Root Path}
   AURI:=AURI + HTTP_PATH_SEPARATOR;
  end;  
 
 {Get Query}
 if Length(AQuery) <> 0 then
  begin
   AURI:=AURI + HTTP_QUERY_SEPARATOR + AQuery;
  end;

 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPParseHost(const AHost:String;var AName,APort:String):Boolean;
var
 HostBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Host}
 if Length(AHost) = 0 then Exit;
 HostBuffer:=AHost;
 
 {Get Name}
 AName:=GetFirstWord(HostBuffer,HTTP_PORT_SEPARATOR);
 if Length(AName) = 0 then Exit;
 
 {Get Port}
 APort:=GetFirstWord(HostBuffer,HTTP_PORT_SEPARATOR);
 {Do not check}
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPBuildHost(const AName,APort:String;var AHost:String):Boolean;
begin
 {}
 Result:=False;
 
 {Set Host}
 AHost:='';
 
 {Get Name}
 if Length(AName) <> 0 then
  begin
   AHost:=AName;
   
   {Get Port}
   if (Length(APort) <> 0) and (StrToIntDef(APort,0) <> 0) then
    begin
     AHost:=AHost + HTTP_PORT_SEPARATOR + APort;
    end;
  end;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPParseParam(const AParam:String;var AName,AValue:String):Boolean;
var
 ParamBuffer:String;
begin
 {}
 Result:=False;

 {Check Param}
 if Length(AParam) = 0 then Exit;
 ParamBuffer:=AParam;
 
 {Get Name}
 AName:=GetFirstWord(ParamBuffer,HTTP_PARAM_SEPARATOR);
 if Length(AName) = 0 then Exit;
 
 {Get Value}
 AValue:=Trim(ParamBuffer);
 {if Length(AValue) = 0 then Exit;} {Do not check}
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPBuildParam(const AName,AValue:String;var AParam:String):Boolean;
begin
 {}
 Result:=True;
 
 {Build Param}
 AParam:=AName + HTTP_PARAM_SEPARATOR + AValue;
end;

{==============================================================================}

function HTTPParseQuery(const AQuery:String;AParams:THTTPParams):Boolean;
var
 Name:String;
 Value:String;
 WorkBuffer:String;
 QueryBuffer:String;
begin
 {}
 Result:=False;
 
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('ParseQuery: Query = ' + AQuery);
 {$ENDIF}
 
 {Check Query}
 if Length(AQuery) <> 0 then
  begin
   QueryBuffer:=AQuery;
   
   {Check Params}
   if AParams = nil then Exit;
   
   {Get Param}
   while Length(QueryBuffer) <> 0 do
    begin
     WorkBuffer:=GetFirstWord(QueryBuffer,HTTP_PARAM_DELIMITER);
     
     {$IFDEF HTTP_DEBUG}
     if HTTP_LOG_ENABLED then HTTPLogDebug('ParseQuery: Param = ' + HTTPDecode(WorkBuffer,True));
     {$ENDIF}
    
     {Parse Param}
     if HTTPParseParam(HTTPDecode(WorkBuffer,True),Name,Value) then
      begin
       {Check Param}
       if (Length(Name) <> 0) and (AParams.FindParam(Name) = nil) then
        begin
         {Add Param}
         if not AParams.AddParam(Name,Value) then Exit;
        end; 
      end;  
    end;
  end;
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function HTTPBuildQuery(AParams:THTTPParams;var AQuery:String):Boolean;
var
 Param:THTTPParam;
 WorkBuffer:String;
begin
 {}
 Result:=False;

 {Set Query}
 AQuery:='';
 
 {Check Params}
 if AParams = nil then Exit;
 
 {Get Param}
 Param:=AParams.GetParam(nil);
 while Param <> nil do
  begin
   {Build Param}
   if not HTTPBuildParam(HTTPEncode(Param.Name,HTTPReservedElementChars),HTTPEncode(Param.Value,HTTPReservedElementChars),WorkBuffer) then Exit;
   
   {$IFDEF HTTP_DEBUG}
   if HTTP_LOG_ENABLED then HTTPLogDebug('BuildQuery: Param = ' + WorkBuffer);
   {$ENDIF}
   
   {Add Param}
   AQuery:=AQuery + HTTP_PARAM_DELIMITER + WorkBuffer;
   
   {Get Param}
   Param:=AParams.GetParam(Param);
  end;
  
 {$IFDEF HTTP_DEBUG}
 if HTTP_LOG_ENABLED then HTTPLogDebug('BuildQuery: Query = ' + AQuery);
 {$ENDIF}
  
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPParseHeader(const AHeader:String;var AName,AValue:String):Boolean;
var
 HeaderBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Header}
 if Length(AHeader) = 0 then Exit;
 HeaderBuffer:=AHeader;
 
 {Check Tab/Space}
 if (Copy(HeaderBuffer,1,Length(HTTP_SPACE)) = HTTP_SPACE) or (Copy(HeaderBuffer,1,Length(HTTP_TAB)) = HTTP_TAB) then
  begin
   {Get Name}
   AName:='';
   
   {Get Value}
   AValue:=Trim(HeaderBuffer);
   if Length(AValue) = 0 then Exit;
  end
 else
  begin 
   {Get Name}
   AName:=GetFirstWord(HeaderBuffer,HTTP_HEADER_SEPARATOR);
   if Length(AName) = 0 then Exit;
 
   {Get Value}
   AValue:=Trim(HeaderBuffer);
   if Length(AValue) = 0 then Exit;
  end;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPBuildHeader(const AName,AValue:String;var AHeader:String):Boolean;
begin
 {}
 Result:=True;
 
 {Check Name}
 if Length(AName) = 0 then
  begin
   {Build Header}
   AHeader:=HTTP_SPACE + AValue;
  end
 else
  begin
   {Build Header}
   AHeader:=AName + HTTP_HEADER_SEPARATOR + HTTP_SPACE + AValue;
  end;
end;

{==============================================================================}

function HTTPPathExtractName(const ASource:String;var AName:String):Boolean;
begin
 {}
 Result:=False; 

 if Length(ASource) = 0 then Exit;
 
 AName:=ExtractFileName(ASource);
 
 Result:=(Length(AName) <> 0);
end;

{==============================================================================}

function HTTPPathExtractDir(const ASource:String;var APath:String):Boolean;
begin
 {}
 Result:=False; 

 if Length(ASource) = 0 then Exit;

 APath:=ExtractFileDir(ASource);
 
 Result:=(Length(APath) <> 0);
end;

{==============================================================================}

function HTTPPathExtractPath(const ASource:String;var APath:String):Boolean;
begin
 {}
 Result:=False; 

 if Length(ASource) = 0 then Exit;

 APath:=ExtractFilePath(ASource);
 
 Result:=(Length(APath) <> 0);
end;

{==============================================================================}

function HTTPPathExtractExtension(const ASource:String;var AExtension:String):Boolean;
begin
 {}
 Result:=False; 
 
 if Length(ASource) = 0 then Exit;
 
 AExtension:=StripLeadingDot(ExtractFileExt(ASource));
 
 Result:=(Length(AExtension) <> 0);
end;

{==============================================================================}

function HTTPParseRequestLine(const ARequest:String;var AMethod:LongWord;var AURL:String;var AVersion:LongWord):Boolean;
var
 WorkBuffer:String;
 RequestBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Request}
 if Length(ARequest) = 0 then Exit;
 RequestBuffer:=ARequest;
 
 {Get Method}
 WorkBuffer:=GetFirstWord(RequestBuffer,HTTP_SPACE);
 if Length(WorkBuffer) = 0 then Exit;
 
 AMethod:=StringToHTTPMethod(WorkBuffer);
 if AMethod = HTTP_METHOD_NONE then Exit;
 
 {Get URL}
 AURL:=GetFirstWord(RequestBuffer,HTTP_SPACE);
 if Length(AURL) = 0 then Exit;
 
 {Get Version}
 WorkBuffer:=GetFirstWord(RequestBuffer,HTTP_SPACE);
 if Length(WorkBuffer) = 0 then Exit;
 
 AVersion:=StringToHTTPVersion(WorkBuffer);
 if AVersion = HTTP_VERSION_00 then Exit;

 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPBuildRequestLine(AMethod:LongWord;const AURL:String;AVersion:LongWord;var ARequest:String):Boolean;
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Get Method}
 WorkBuffer:=HTTPMethodToString(AMethod);
 if Length(WorkBuffer) = 0 then Exit;
 ARequest:=WorkBuffer;
 
 {Get URL}
 if Length(AURL) = 0 then Exit;
 ARequest:=ARequest + HTTP_SPACE + AURL;
 
 {Get Version}
 WorkBuffer:=HTTPVersionToString(AVersion);
 if Length(WorkBuffer) = 0 then Exit;
 ARequest:=ARequest + HTTP_SPACE + WorkBuffer;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPParseResponseLine(const AResponse:String;var AVersion,AStatus:LongWord;var AReason:String):Boolean;
var
 WorkBuffer:String;
 ResponseBuffer:String;
begin
 {}
 Result:=False;
 
 {Check Request}
 if Length(AResponse) = 0 then Exit;
 ResponseBuffer:=AResponse;
 
 {Get Version}
 WorkBuffer:=GetFirstWord(ResponseBuffer,HTTP_SPACE);
 if Length(WorkBuffer) = 0 then Exit;
 
 AVersion:=StringToHTTPVersion(WorkBuffer);
 if AVersion = HTTP_VERSION_00 then Exit;
 
 {Get Status}
 WorkBuffer:=GetFirstWord(ResponseBuffer,HTTP_SPACE);
 if Length(WorkBuffer) = 0 then Exit;
 
 AStatus:=StringToHTTPStatus(WorkBuffer);
 if AStatus = HTTP_STATUS_NONE then Exit;
 
 {Get Reason}
 AReason:=ResponseBuffer;
 
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function HTTPBuildResponseLine(AVersion,AStatus:LongWord;const AReason:String;var AResponse:String):Boolean;
var
 WorkBuffer:String;
begin
 {}
 Result:=False;
 
 {Get Version}
 AResponse:=HTTPVersionToString(AVersion);
 if Length(AResponse) = 0 then Exit;
 
 {Get Status}
 WorkBuffer:=HTTPStatusToString(AStatus);
 if Length(WorkBuffer) = 0 then Exit;
 AResponse:=AResponse + HTTP_SPACE + WorkBuffer;
 
 {Get Reason}
 if Length(AReason) <> 0 then
  begin
   AResponse:=AResponse + HTTP_SPACE + AReason;
  end;
  
 {Return Result}
 Result:=True; 
end;

{==============================================================================}

function FileTimeToHTTPDate(const AFileTime:TFileTime):String;
var
 DateTime:TDateTime;
begin
 {}
 DateTime:=SystemFileTimeToDateTime(AFileTime); {No Conversion}
 
 Result:=FormatDateTime(HTTP_DATE_FORMAT,DateTime);
end;

{==============================================================================}

function HTTPDateToFileTime(const AHTTPDate:String):TFileTime;
var
 DateTime:TDateTime;
begin
 {}
 DateTime:=HTTPDateToDateTime(AHTTPDate);
 
 Result:=DateTimeToSystemFileTime(DateTime); {No Conversion}
end;

{==============================================================================}

function DateTimeToHTTPDate(const ADateTime:TDateTime):String;
begin
 {}
 Result:=FormatDateTime(HTTP_DATE_FORMAT,ADateTime);
end;

{==============================================================================}

function HTTPDateToDateTime(const AHTTPDate:String):TDateTime;
var
 DateTime:TDateTime;
 
 Day:LongWord;
 Month:LongWord;
 Year:LongWord;
 
 Hour:LongWord;
 Minute:LongWord;
 Second:LongWord;
 
 HTTPDateBuffer:String;
begin
 {}
 Result:=0;
 
 {Check Date}
 if Length(AHTTPDate) = 0 then Exit;
  
 {Try to convert from RFC1123 format (Sun, 06 Nov 1994 08:49:37 GMT)}
 HTTPDateBuffer:=AHTTPDate; 
 {Remove the weekday}
 GetFirstWord(HTTPDateBuffer,HTTP_SPACE);
 {Get and Convert Day}
 Day:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),0);
 {Get and Convert Month}
 Month:=ShortMonthToMonth(GetFirstWord(HTTPDateBuffer,HTTP_SPACE));
 {Get and Convert Year}
 Year:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),0);
 {Get and Convert Hour}
 Hour:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_COLON),99);
 {Get and Convert Minute}
 Minute:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_COLON),99);
 {Get and Convert Second}
 Second:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),99);
 try
  {Encode Date and Time}
  DateTime:=EncodeDate(Year,Month,Day) + EncodeTime(Hour,Minute,Second,0);
  
  {Return Result}  
  Result:=DateTime;
  Exit;
 except
  {Move to next format}
 end; 

 {Try to convert from RFC850 format (Sunday, 06-Nov-94 08:49:37 GMT)}
 HTTPDateBuffer:=AHTTPDate;
 {Remove the weekday}
 GetFirstWord(HTTPDateBuffer,HTTP_SPACE);
 {Get and Convert Day}
 Day:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_DASH),0);
 {Get and Convert Month}
 Month:=ShortMonthToMonth(GetFirstWord(HTTPDateBuffer,HTTP_DASH));
 {Get and Convert Year}
 Year:=TwoDigitYearToYear(StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),0));
 {Get and Convert Hour}
 Hour:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_COLON),99);
 {Get and Convert Minute}
 Minute:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_COLON),99);
 {Get and Convert Second}
 Second:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),99);
 try
  {Encode Date and Time}
  DateTime:=EncodeDate(Year,Month,Day) + EncodeTime(Hour,Minute,Second,0);
  
  {Return Result}  
  Result:=DateTime;
  Exit;
 except
  {Move to next format}
 end; 
 
 {Try to convert from ANSI C asctime format (Sun Nov  6 08:49:37 1994)}
 HTTPDateBuffer:=AHTTPDate;
 {Remove the weekday}
 GetFirstWord(HTTPDateBuffer,HTTP_SPACE);
 {Get and Convert Month}
 Month:=ShortMonthToMonth(GetFirstWord(HTTPDateBuffer,HTTP_SPACE));
 {Get and Convert Day}
 Day:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),0);
 if Day = 0 then Day:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),0);
 {Get and Convert Hour}
 Hour:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_COLON),99);
 {Get and Convert Minute}
 Minute:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_COLON),99);
 {Get and Convert Second}
 Second:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),99);
 {Get and Convert Year}
 Year:=StrToIntDef(GetFirstWord(HTTPDateBuffer,HTTP_SPACE),0);
 try
  {Encode Date and Time}
  DateTime:=EncodeDate(Year,Month,Day) + EncodeTime(Hour,Minute,Second,0);
  
  {Return Result}  
  Result:=DateTime;
  Exit;
 except
  {Return failure}
 end; 
end;

{==============================================================================}

function StringToHTTPProtocol(const AProtocol:String):LongWord;
var
 WorkBuffer:String;
begin
 {}
 Result:=HTTP_PROTOCOL_NONE;
 
 WorkBuffer:=Uppercase(AProtocol);
 
 if WorkBuffer = HTTP_PROTOCOL_STRING_HTTP then
  begin
   Result:=HTTP_PROTOCOL_HTTP;
  end 
 else if WorkBuffer = HTTP_PROTOCOL_STRING_HTTPS then
  begin
   Result:=HTTP_PROTOCOL_HTTPS;
  end;
end;

{==============================================================================}

function HTTPProtocolToString(AProtocol:LongWord):String;
begin
 {}
 Result:='';
 
 case AProtocol of
  HTTP_PROTOCOL_HTTP:Result:=HTTP_PROTOCOL_STRING_HTTP;
  HTTP_PROTOCOL_HTTPS:Result:=HTTP_PROTOCOL_STRING_HTTPS;
 end;
end;
 
{==============================================================================}

function StringToHTTPMethod(const AMethod:String):LongWord;
var
 WorkBuffer:String;
begin
 {}
 Result:=HTTP_METHOD_NONE;
 
 WorkBuffer:=Uppercase(AMethod);
 
 if WorkBuffer = HTTP_METHOD_STRING_OPTIONS then
  begin
   Result:=HTTP_METHOD_OPTIONS;
  end 
 else if WorkBuffer = HTTP_METHOD_STRING_GET then
  begin
   Result:=HTTP_METHOD_GET;
  end
 else if WorkBuffer = HTTP_METHOD_STRING_HEAD then
  begin
   Result:=HTTP_METHOD_HEAD;
  end
 else if WorkBuffer = HTTP_METHOD_STRING_POST then
  begin
   Result:=HTTP_METHOD_POST;
  end
 else if WorkBuffer = HTTP_METHOD_STRING_PUT then
  begin
   Result:=HTTP_METHOD_PUT;
  end
 else if WorkBuffer = HTTP_METHOD_STRING_DELETE then
  begin
   Result:=HTTP_METHOD_DELETE;
  end
 else if WorkBuffer = HTTP_METHOD_STRING_TRACE then
  begin
   Result:=HTTP_METHOD_TRACE;
  end
 else if WorkBuffer = HTTP_METHOD_STRING_CONNECT then 
  begin
   Result:=HTTP_METHOD_CONNECT;
  end;
end;

{==============================================================================}

function HTTPMethodToString(AMethod:LongWord):String;
begin
 {}
 Result:='';
 
 case AMethod of
  HTTP_METHOD_OPTIONS:Result:=HTTP_METHOD_STRING_OPTIONS;
  HTTP_METHOD_GET:Result:=HTTP_METHOD_STRING_GET;
  HTTP_METHOD_HEAD:Result:=HTTP_METHOD_STRING_HEAD;
  HTTP_METHOD_POST:Result:=HTTP_METHOD_STRING_POST;
  HTTP_METHOD_PUT:Result:=HTTP_METHOD_STRING_PUT;
  HTTP_METHOD_DELETE:Result:=HTTP_METHOD_STRING_DELETE;
  HTTP_METHOD_TRACE:Result:=HTTP_METHOD_STRING_TRACE;
  HTTP_METHOD_CONNECT:Result:=HTTP_METHOD_STRING_CONNECT;
 end;
end;

{==============================================================================}

function StringToHTTPVersion(const AVersion:String):LongWord;
var
 WorkBuffer:String;
begin
 {}
 Result:=HTTP_VERSION_00;
 
 WorkBuffer:=Uppercase(AVersion);
 
 if WorkBuffer = HTTP_VERSION_STRING_10 then
  begin
   Result:=HTTP_VERSION_10;
  end 
 else if WorkBuffer = HTTP_VERSION_STRING_11 then
  begin
   Result:=HTTP_VERSION_11;
  end;
end;  

{==============================================================================}

function HTTPVersionToString(AVersion:LongWord):String;
begin
 {}
 Result:='';
 
 case AVersion of
  HTTP_VERSION_10:Result:=HTTP_VERSION_STRING_10;
  HTTP_VERSION_11:Result:=HTTP_VERSION_STRING_11;
 end;
end;
 
{==============================================================================}
 
function StringToHTTPEncoding(const AEncoding:String):LongWord;
var
 WorkBuffer:String;
begin
 {}
 Result:=HTTP_ENCODING_NONE;
 
 WorkBuffer:=Uppercase(AEncoding);
 
 if WorkBuffer = HTTP_ENCODING_STRING_IDENTITY then
  begin
   Result:=HTTP_ENCODING_IDENTITY;
  end 
 else if WorkBuffer = HTTP_ENCODING_STRING_CHUNKED then
  begin
   Result:=HTTP_ENCODING_CHUNKED;
  end
 else if WorkBuffer = HTTP_ENCODING_STRING_GZIP then
  begin
   Result:=HTTP_ENCODING_GZIP;
  end
 else if WorkBuffer = HTTP_ENCODING_STRING_COMPRESS then
  begin
   Result:=HTTP_ENCODING_COMPRESS;
  end
 else if WorkBuffer = HTTP_ENCODING_STRING_DEFLATE then
  begin
   Result:=HTTP_ENCODING_DEFLATE;
  end;
end;

{==============================================================================}

function HTTPEncodingToString(AEncoding:LongWord):String;
begin
 {}
 Result:='';
 
 case AEncoding of
  HTTP_ENCODING_IDENTITY:Result:=HTTP_ENCODING_STRING_IDENTITY;
  HTTP_ENCODING_CHUNKED:Result:=HTTP_ENCODING_STRING_CHUNKED;
  HTTP_ENCODING_GZIP:Result:=HTTP_ENCODING_STRING_GZIP;
  HTTP_ENCODING_COMPRESS:Result:=HTTP_ENCODING_STRING_COMPRESS;
  HTTP_ENCODING_DEFLATE:Result:=HTTP_ENCODING_STRING_DEFLATE;
 end;
end;

{==============================================================================}

function StringToHTTPStatus(const AStatus:String):LongWord;
var
 Value:LongWord;
begin
 {}
 Result:=HTTP_STATUS_NONE;
 
 Value:=StrToIntDef(AStatus,HTTP_STATUS_NONE);
 if (Value >= HTTP_MIN_STATUS) and (Value <= HTTP_MAX_STATUS) then
  begin
   Result:=Value;
  end;
end;

{==============================================================================}
 
function HTTPStatusToString(AStatus:LongWord):String;
begin
 {}
 Result:='';
 
 if (AStatus >= HTTP_MIN_STATUS) and (AStatus <= HTTP_MAX_STATUS) then
  begin
   Result:=IntToStr(AStatus);
  end;
end;
 
{==============================================================================}

procedure HTTPLog(Level:LongWord;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < HTTP_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = HTTP_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = HTTP_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = HTTP_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'HTTP: ';

 {Output Logging} 
 LoggingOutputEx(LOGGING_FACILITY_HTTP,LogLevelToLoggingSeverity(Level),'HTTP',WorkBuffer + AText);
end;

{==============================================================================}

procedure HTTPLogInfo(const AText:String); inline;
begin
 {}
 HTTPLog(HTTP_LOG_LEVEL_INFO,AText);
end;

{==============================================================================}

procedure HTTPLogWarn(const AText:String); inline;
begin
 {}
 HTTPLog(HTTP_LOG_LEVEL_WARN,AText);
end;

{==============================================================================}

procedure HTTPLogError(const AText:String); inline;
begin
 {}
 HTTPLog(HTTP_LOG_LEVEL_ERROR,AText);
end;

{==============================================================================}

procedure HTTPLogDebug(const AText:String); inline;
begin
 {}
 HTTPLog(HTTP_LOG_LEVEL_DEBUG,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 HTTPInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
