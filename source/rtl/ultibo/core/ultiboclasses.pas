{
Ultibo classes unit.

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


Ultibo Classes
==============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit UltiboClasses;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Unicode,Ultibo,SysUtils,Classes;

//To Do //FPC supports TStream with 64bit interaces, check and confirm and remove TStreamEx etc
                      //Retain TMemoryStreamEx because it is for a different purpose (Grow without Realloc)
                    
//To Do //Look for:

//Testing                    

//Remove

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Ultibo Class constants}
const
 {Error constants}
 SListIndexError = 'List index out of bounds (%d)';
 SFCreateError = 'Cannot create file %s';
 SFOpenError = 'Cannot open file %s';

const
 {B-Tree constants}
 btreeCompareLess    = -1;
 btreeCompareEqual   = 0;
 btreeCompareGreater = 1;

const
 {Hash constants}
 stringHashSize = 8;

 listHashBits = 8;
 treeHashBits = 3;

 keyHashMinBits = 1;
 keyHashMaxBits = 16; {Maximum of 16 bit hash mask (65535 buckets) due to memory usage}
 keyHashMasks:array[keyHashMinBits..keyHashMaxBits] of LongWord = (
  $00000001,$00000003,$00000007,$0000000F,
  $0000001F,$0000003F,$0000007F,$000000FF,
  $000001FF,$000003FF,$000007FF,$00000FFF,
  $00001FFF,$00003FFF,$00007FFF,$0000FFFF);

const
 {Delta constants}
 memoryStreamDelta = $2000;
 memoryStreamShift = 13;

 stringListDelta = $2000;
 
{==============================================================================}
{Ultibo Class types (Threads)}
type
 {TThreadEx  - TThread with Before and After Execution methods}
 TThreadEx = class(TThread)
 private
  {}
 protected
  {}
  procedure Execution; virtual;
  procedure AfterExecution; virtual;
  procedure BeforeExecution; virtual;
  procedure Execute; override;
 public
  {}
  procedure TerminateAndWaitFor;
 end;
  
{==============================================================================}
{Ultibo Class types (Timers)}
type
 {TTimerEx  - TTimer/TFPTimer equivalent for Ultibo specific timers}
 TTimerEx = class(TObject)
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FInterval:Integer;
  FEnabled:Boolean;
  FWorker:Boolean;  {If True then use a Worker thread for the timer event}
  FOnTimer:TNotifyEvent;
  FTimerHandle:TTimerHandle;
  
  {}
  procedure SetEnabled(Value:Boolean);
 protected
  {}
  procedure Timer; virtual;
  procedure StartTimer; virtual;
  procedure StopTimer; virtual;
 public
  {}
  property Interval:Integer read FInterval write FInterval;
  property Enabled:Boolean read FEnabled write SetEnabled;
  property Worker:Boolean read FWorker write FWorker;
  
  property OnTimer:TNotifyEvent read FOnTimer write FOnTimer;
 end;
 
{==============================================================================}
{Ultibo Class types (Object Lists)}
type
 {TObjList  - TList with Auto Free of List Objects}
 TObjList = class(TList)
   {List which Frees all nodes on Destroy}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Functions}
   procedure DestroyListObjects;
  public
   {Public Functions}
   procedure ClearList;
 end;
 
type
 {TThreadObjList  - TThreadList with Auto Free of List Objects} 
 TThreadObjList = class(TThreadList)
   {Thread Safe List which Frees all nodes on Destroy}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Functions}
   procedure DestroyListObjects;
  public
   {Public Functions}
   procedure ClearList;
 end;
 
{==============================================================================}
{Ultibo Class types (Linked Lists)}
type 
 {TListObject - A TObject with .Prev/.Next with Properties for use in Lists}
 TListObject = class(TObject)
 private
  {}
  FPrev:TListObject;
  FNext:TListObject;
 public
  {}
  property Prev:TListObject read FPrev write FPrev;
  property Next:TListObject read FNext write FNext;
 end;
 
type 
 {TLinkedList - A List of TListObjects with automatic .Prev/.Next Links}
 TLinkedList = class(TObject)
  {Linked List which does not Free nodes on Destroy}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FCount:Integer;
  FFirst:TListObject;
  FLast:TListObject;
  function GetCount:Integer;
  function Link(AValue:TListObject):Boolean;
  function LinkEx(APrev,AValue:TListObject):Boolean;
  function Unlink(AValue:TListObject):Boolean;
 public
  {}
  property Count:Integer read GetCount;
  property First:TListObject read FFirst;
  property Last:TListObject read FLast;
  function Add(AValue:TListObject):Boolean; virtual;          
  function Remove(AValue:TListObject):Boolean; virtual;       
  function Insert(APrev,AValue:TListObject):Boolean; virtual;
  procedure Clear; virtual;                                   
end;
 
type
 {TLinkedObjList  - TLinkedList with Auto Free of List Objects}
 TLinkedObjList = class(TLinkedList)
   {Linked List which Frees all nodes on Destroy}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Functions}
  public
   {Public Functions}
   procedure ClearList;
 end;
 
//type
 {TThreadLinkedList - A List of TListObjects with automatic .Prev/.Next Links}
 //To Do {Thread Safe Linked List which does not Free nodes on Destroy}
 
type
 {TThreadLinkedObjList  - TLinkedList with Auto Free of List Objects}
 TThreadLinkedObjList = class(TLinkedList) //TThreadLinkedList
   {Thread Safe Linked List which Frees all nodes on Destroy}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Variables}
   FLock:TRTLCriticalSection;  //To Do //Move to TThreadLinkedList
   {Private Functions}
  public
   {Public Functions}
   procedure ClearList;

   procedure LockList;    //To Do //Move to TThreadLinkedList
   procedure UnlockList;  //To Do //Move to TThreadLinkedList
 end;
 
//To Do //TSimpleListObject / TSimpleLinkedList / TSimpleLinkedObjList
//Same as TLinkedList but with only .First/.Next links to save memory when using
//for lists with many thousands of objects
 
{==============================================================================}
{Ultibo Class types (Linked Trees)}
type
 {TTreeObject - A TObject with Prev/Next/Parent/FirstChild for use in Trees}
 TTreeObject = class(TListObject)
 private
  {}
  FParent:TTreeObject;
  FFirstChild:TTreeObject;
  FLastChild:TTreeObject;
 public
  {}
  property Parent:TTreeObject read FParent write FParent;
  property FirstChild:TTreeObject read FFirstChild write FFirstChild;
  property LastChild:TTreeObject read FLastChild write FLastChild;
 end;
 
type
 {TLinkedTree - A Tree of TTreeObjects with automatic Prev/Next etc Links}
 TLinkedTree = class(TObject)
  {Linked Tree which does not Free nodes on Destroy}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FCount:Integer;
  FFirst:TTreeObject;
  FLast:TTreeObject;
  function GetCount:Integer;
  function Link(AValue,AParent:TTreeObject):Boolean;
  function LinkEx(APrev,AValue,AParent:TTreeObject):Boolean;
  function Unlink(AValue:TTreeObject):Boolean;
 public
  {}
  property Count:Integer read GetCount;
  property Root:TTreeObject read FFirst;
  property First:TTreeObject read FFirst;
  property Last:TTreeObject read FLast;
  function Add(AValue,AParent:TTreeObject):Boolean; virtual;          {Modified 22/6/2009 for THashLinkedList}
  function Remove(AValue:TTreeObject):Boolean; virtual;               {Modified 22/6/2009 for THashLinkedList}
  function Insert(APrev,AValue,AParent:TTreeObject):Boolean; virtual; {Modified 22/6/2009 for THashLinkedList}
  procedure Move(AValue,AParent:TTreeObject); virtual;                {Modified 22/6/2009 for THashLinkedList}
  procedure Clear; virtual;                                           {Modified 22/6/2009 for THashLinkedList}
  function FirstChild(AParent:TTreeObject):TTreeObject;
  function LastChild(AParent:TTreeObject):TTreeObject;
end;
 
type
 {TLinkedObjTree  - TLinkedTree with Auto Free of Tree Objects}
 TLinkedObjTree = class(TLinkedTree)
   {Linked Tree which Frees all nodes on Destroy}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Functions}
   
  protected
   {Protected Functions}
   procedure ClearListItems(AParent:TTreeObject);
  public
   {Public Functions}
   procedure ClearList;
 end;
 
//type
 {TThreadLinkedTree - A Tree of TTreeObjects with automatic Prev/Next etc Links}
 //To Do {Thread Safe Linked Tree which does not Free nodes on Destroy}
 
//type
 {TThreadLinkedObjTree  - TLinkedTree with Auto Free of Tree Objects} 
 //To Do  {Thread Safe Linked Tree which Frees all nodes on Destroy}
               //Subclassed from TThreadLinkedTree

//type
 {TSimpleTreeObject}               
 {TSimpleLinkedTree}               
//To Do //TSimpleTreeObject / TSimpleLinkedTree / TSimpleLinkedObjTree
//Same as TLinkedTree but with only .First/.Next/.FirstChild links to save memory when
//using for trees with many thousands of objects
               
{==============================================================================}
{Ultibo Class types (B-Trees)}
type
 {TBtreeObject - A TObject with Left/Right/Parent for use in B-Trees}
 TBtreeObject = class(TObject)
 private
  {}
  FBlank:Boolean;         {Object is the blank key (1 per node)}

  FPrev:TBtreeObject;     {Prev object in Linked List (Sorted)}
  FNext:TBtreeObject;     {Next object in Linked List (Sorted)}

  FLeft:TBtreeObject;     {Left sibling in B-Tree (Lower)}
  FRight:TBtreeObject;    {Right sibling in B-Tree (Higher)}

  FChild:TBtreeObject;    {Child object in B-Tree (Lower)}
  FParent:TBtreeObject;   {Parent object in B-Tree (Higher)}
 public
  {}
  property Blank:Boolean read FBlank write FBlank;

  property Prev:TBtreeObject read FPrev write FPrev;
  property Next:TBtreeObject read FNext write FNext;

  property Left:TBtreeObject read FLeft write FLeft;
  property Right:TBtreeObject read FRight write FRight;

  property Child:TBtreeObject read FChild write FChild;
  property Parent:TBtreeObject read FParent write FParent;

  procedure Clear;
 end;
               
type
 {TLinkedBtree - A B-Tree of TBtreeObjects with automatic Left/Right etc Links}
 TLinkedBtree = class(TObject)
  {Linked B-Tree which does not Free nodes on Destroy}
  {Notes: Search/Find should be implemented by descendants}
  {       Compare must always be implemented by descendants}
  {       Blank keys are not included in the linked list}
  {       Blank key always compares as higher than anything}
  {       Linked list allows easy traversal in either direction}
  {       Also allows implementation of the rebalance function}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  procedure SetOrder(AOrder:LongWord);

  function PropogateDrop(AEntry:TBtreeObject):Boolean;
  function PropogateMerge(AEntry:TBtreeObject):Boolean;
  function PropogateSplit(AEntry:TBtreeObject):Boolean;
 protected
  {}
  FOrder:LongWord;          {Order N of B-Tree (N - 1 Keys per Node)}
  FMedian:LongWord;         {Ceil(N / 2) (Median - 1 Keys as Minimum)}

  FSwapLeft:Boolean;        {If True then Remove will Swap with Left not Right}

  FRoot:TBtreeObject;       {Root Node of B-Tree}
  FFirst:TBtreeObject;      {First object in Linked List (Sorted)}
  FLast:TBtreeObject;       {Last object in Linked List (Sorted)}
  FFirstBlank:TBtreeObject; {First blank key in Linked List (Not Sorted)}
  FLastBlank:TBtreeObject;  {Last blank key in Linked List (Not Sorted)}

  function GetCount(AEntry:TBtreeObject):LongWord; virtual;
  function GetDepth(AEntry:TBtreeObject):LongWord; virtual;

  function GetEnd(AEntry:TBtreeObject):TBtreeObject; virtual;
  function GetStart(AEntry:TBtreeObject):TBtreeObject; virtual;
  function GetBlank(AEntry:TBtreeObject):TBtreeObject; virtual;
  function GetMedian(AEntry:TBtreeObject):TBtreeObject; virtual;

  function GetDrop(AEntry:TBtreeObject;var ALeft:Boolean):TBtreeObject; virtual;
  function GetMerge(AEntry:TBtreeObject):TBtreeObject; virtual;
  function GetBorrow(AEntry:TBtreeObject):TBtreeObject; virtual;
  function GetTarget(ADrop:TBtreeObject;ALeft:Boolean):TBtreeObject; virtual;

  function SetParent(AEntry,AParent:TBtreeObject):Boolean;

  function GetLefthand(AEntry:TBtreeObject):TBtreeObject;
  function GetRighthand(AEntry:TBtreeObject):TBtreeObject;

  function GetLeftmost(AEntry:TBtreeObject):TBtreeObject;
  function GetRightmost(AEntry:TBtreeObject):TBtreeObject;

  function GetSuccessor(AEntry:TBtreeObject):TBtreeObject;
  function GetPredecessor(AEntry:TBtreeObject):TBtreeObject;

  function GetPosition(AStart,AEntry:TBtreeObject):TBtreeObject;

  function Push(AEntry:TBtreeObject):Boolean;
  function Split(AEntry:TBtreeObject):Boolean;
  function Swap(AEntry,ASwap:TBtreeObject;ALeft:Boolean):Boolean;
  function Drop(AEntry,ADrop,ATarget:TBtreeObject;ALeft:Boolean):Boolean;
  function DropOld(AEntry,ADrop:TBtreeObject;ALeft:Boolean):Boolean;        //To Do //Remove ?
  function Merge(AEntry,AMerge:TBtreeObject):Boolean;
  function Borrow(AEntry,ABorrow:TBtreeObject):Boolean;

  function Link(AEntry,ANext:TBtreeObject):Boolean;
  function Unlink(AEntry:TBtreeObject):Boolean;

  function LinkBlank(AEntry:TBtreeObject):Boolean;
  function UnlinkBlank(AEntry:TBtreeObject):Boolean;

  function Attach(AParent,AEntry,ARight:TBtreeObject):Boolean;
  function Detach(AEntry:TBtreeObject):Boolean;

  {Descendant Methods}
  function PushNode(AEntry:TBtreeObject):Boolean; virtual;                      {Allows descendants to monitor node push}
  function SplitNode(AEntry:TBtreeObject):Boolean; virtual;                     {Allows descendants to monitor node split}
  function DropNode(AEntry,ADrop,ATarget:TBtreeObject;ALeft:Boolean):Boolean; virtual;  {Allows descendants to monitor node drop}
  function MergeNode(AEntry,AMerge:TBtreeObject):Boolean; virtual;              {Allows descendants to monitor node merge}
  function BorrowEntry(AEntry,ABorrow:TBtreeObject):Boolean; virtual;           {Allows descendants to monitor entry borrow}

  function SwapEntry(AEntry,ASwap:TBtreeObject;ALeft:Boolean):Boolean; virtual; {Allows descendants to monitor entry swap}
  function SetParentEntry(AEntry,AParent:TBtreeObject):Boolean; virtual;        {Allows descendants to monitor entry parent}

  function CreateBlank:TBtreeObject; virtual;                                   {Allows descendants to monitor node creation}
  function DeleteBlank(ABlank:TBtreeObject):Boolean; virtual;                   {Allows descendants to monitor node deletion}

  function AttachBlank(ABlank:TBtreeObject):Boolean; virtual;                   {Allows descendants to monitor node modification}
  function DetachBlank(ABlank:TBtreeObject):Boolean; virtual;                   {Allows descendants to monitor node modification}

  function AttachEntry(AEntry:TBtreeObject):Boolean; virtual;                   {Allows descendants to monitor node modification}
  function DetachEntry(AEntry:TBtreeObject):Boolean; virtual;                   {Allows descendants to monitor node modification}

  function RequirePush(AEntry:TBtreeObject):Boolean; virtual;                   {Allows descendants to modify balancing behaviour}
  function RequireSplit(AEntry:TBtreeObject):Boolean; virtual;                  {Allows descendants to modify balancing behaviour}
  function RequireDrop(AEntry:TBtreeObject):Boolean; virtual;                   {Allows descendants to modify balancing behaviour}
  function RequireMerge(AEntry:TBtreeObject):Boolean; virtual;                  {Allows descendants to modify balancing behaviour}
  function RequireBorrow(AEntry:TBtreeObject):Boolean; virtual;                 {Allows descendants to modify balancing behaviour}

  function Compare(AEntry1,AEntry2:TBtreeObject):Integer; virtual;
 public
  {}
  property Order:LongWord read FOrder write SetOrder;
  property Median:LongWord read FMedian;

  property SwapLeft:Boolean read FSwapLeft;
  
  property Root:TBtreeObject read FRoot;
  property First:TBtreeObject read FFirst;
  property Last:TBtreeObject read FLast;

  function Add(AParent,AEntry:TBtreeObject):Boolean;

  function Insert(AEntry:TBtreeObject):Boolean;
  function Remove(AEntry:TBtreeObject):Boolean;

  procedure Clear;
  procedure Empty;
  procedure Rebuild;
end;
 
type
 {TLinkedObjBtree  - TLinkedBtree with Auto Free of B-Tree Objects}
 TLinkedObjBtree = class(TLinkedBtree)
   {Linked B-Tree which Frees all nodes on Destroy}
   constructor Create;
   destructor Destroy; override;
  private
   {Private Functions}
  public
   {Public Functions}
   procedure ClearBtree;
   procedure EmptyBtree;
 end;
 
{==============================================================================}
{Ultibo Class types (Hash Linked Lists)}
type
 THashLinkedList = class;
 {THashListObject - A TObject with KeyPrev/KeyNext/KeyHash for use in HashLists}
 THashListObject = class(TListObject)
 private
  {}
  FKeyList:THashLinkedList;
  FKeyPrev:THashListObject;
  FKeyNext:THashListObject;
 protected
  {}
  FKeyHash:LongWord; {Publish in descendants if required}

  procedure SetKeyHash(AKeyHash:LongWord);
 public
  {}
  property KeyList:THashLinkedList read FKeyList write FKeyList;
  property KeyPrev:THashListObject read FKeyPrev write FKeyPrev;
  property KeyNext:THashListObject read FKeyNext write FKeyNext;
 end;
 
 {THashLinkedList - A List of THashListObjects with automatic hash buckets}
 THashLinkedList = class(TLinkedList)
  constructor Create(AKeyBits:Byte);
  destructor Destroy; override;
 private
  {}
  FKeyBits:Byte;
  FKeyMask:LongWord;
  FKeyShift:LongWord;
  FKeyBuckets:Pointer;
  function KeyLink(AValue:THashListObject):Boolean;
  function KeyUnlink(AValue:THashListObject):Boolean;
 public
  {}
  function KeyFirst(AKeyHash:LongWord):THashListObject;

  function Add(AValue:TListObject):Boolean; override;
  function Remove(AValue:TListObject):Boolean; override;
  function Insert(APrev,AValue:TListObject):Boolean; override;
  procedure Clear; override;
end;
 
type
 {THashLinkedObjList - THashLinkedList with Auto Free of List Objects}
 THashLinkedObjList = class(THashLinkedList)
   constructor Create(AKeyBits:Byte);
   destructor Destroy; override;
  private
   {Private Functions}
  public
   {Public Functions}
   procedure ClearList;
 end;
 
{==============================================================================}
{Ultibo Class types (Hash Linked Trees)}
type
 THashLinkedTree = class;
 {THashTreeObject - A TObject with KeyPrev/KeyNext/KeyHash for use in HashTrees}
 THashTreeObject = class(TTreeObject)
  constructor Create(AKeyBits:Byte);
  destructor Destroy; override;
 private
  {}
  FKeyBits:Byte;
  FKeyMask:LongWord;
  FKeyShift:LongWord;
  FKeyBuckets:Pointer;
  FKeyTree:THashLinkedTree;
  FKeyPrev:THashTreeObject;
  FKeyNext:THashTreeObject;
 protected
  {}
  FKeyHash:LongWord; {Publish in descendants if required}
  procedure SetKeyHash(AKeyHash:LongWord);
  function KeyLink(AValue:THashTreeObject):Boolean;
  function KeyUnlink(AValue:THashTreeObject):Boolean;
 public
  {}
  function KeyFirst(AKeyHash:LongWord):THashTreeObject;

  property KeyTree:THashLinkedTree read FKeyTree write FKeyTree;
  property KeyPrev:THashTreeObject read FKeyPrev write FKeyPrev;
  property KeyNext:THashTreeObject read FKeyNext write FKeyNext;
 end;
 
 {THashLinkedTree - A Tree of THashTreeObjects with automatic hash buckets}
 THashLinkedTree = class(TLinkedTree)
  constructor Create(AKeyBits:Byte);
  destructor Destroy; override;
 private
  {}
  FKeyBits:Byte;
  FKeyMask:LongWord;
  FKeyShift:LongWord;
  FKeyBuckets:Pointer;
  function KeyLink(AValue,AParent:THashTreeObject):Boolean;
  function KeyUnlink(AValue,AParent:THashTreeObject):Boolean;
 public
  {}
  function KeyFirst(AParent:THashTreeObject;AKeyHash:LongWord):THashTreeObject;

  function Add(AValue,AParent:TTreeObject):Boolean; override;
  function Remove(AValue:TTreeObject):Boolean; override;
  function Insert(APrev,AValue,AParent:TTreeObject):Boolean; override;
  procedure Move(AValue,AParent:TTreeObject); override;
  procedure Clear; override;
end;
 
type
 {THashLinkedObjTree - THashLinkedTree with Auto Free of List Objects}
 THashLinkedObjTree = class(THashLinkedTree)
   constructor Create(AKeyBits:Byte);
   destructor Destroy; override;
  private
   {Private Functions}
   procedure ClearListItems(AParent:THashTreeObject);
  public
   {Public Functions}
   procedure ClearList;
 end;
 
{==============================================================================}
{Ultibo Class types (String Lists)}
type
 {TStringObject - A TObject with Prev/Next/Value/Hash for use in StringLists}
 TStringObject = class(TListObject)
 private
  {}
  FValue:String;
  FData:TObject;
 protected
  {}
  FHash:LongWord;

  procedure SetValue(const AValue:String); virtual;
 public
  {}
  property Value:String read FValue write SetValue;
  property Data:TObject read FData write FData;
  property Hash:LongWord read FHash;
 end;
 
type
 {TLinkedStringList - A List of TStringObjects with automatic Prev/Next Links}
 {Mostly identical methods to TStringList with addition of First/Last/Next/Prev}
 {Note: Currently does not implement Sorting or Duplicate handling}
 TLinkedStringList = class(TStrings)
  constructor Create;
 public {Public destructor for FPC RTL}
  destructor Destroy; override;
 private
  {}
  FCount:Integer;
  FFirst:TStringObject;
  FLast:TStringObject;
  {FSorted:Boolean;}
  FUpdating:Boolean;
  {FDuplicates:TDuplicates;}
  FOnChange:TNotifyEvent;
  FOnChanging:TNotifyEvent;
  {procedure QuickSort(L,R:Integer);}
  {procedure SetSorted(Value:Boolean);}

  function GetItem(AIndex:Integer):TStringObject;

  function Link(AValue:TStringObject):Boolean;
  function LinkEx(APrev,AValue:TStringObject):Boolean;
  function Unlink(AValue:TStringObject):Boolean;
 protected
  {}
  procedure ClearList; virtual;

  procedure Changed; virtual;
  procedure Changing; virtual;
  function Get(Index:Integer):String; override;
  function GetCount:Integer; override;
  function GetObject(Index:Integer):TObject; override;
  procedure Put(Index:Integer;const S:String); override;
  procedure PutObject(Index:Integer;AObject:TObject); override;
  procedure SetUpdateState(Updating:Boolean); override;
 public
  {}
  function Add(const S:String):Integer; override;
  procedure Clear; override;
  procedure Delete(Index:Integer); override;
  procedure Exchange(Index1,Index2:Integer); override;
  {function Find(const S:String;var Index:Integer):Boolean; virtual;}
  function IndexOf(const S:String):Integer; override;
  procedure Insert(Index:Integer;const S:String); override;
  {procedure Sort; virtual;}
  property First:TStringObject read FFirst;
  property Last:TStringObject read FLast;
  {property Duplicates:TDuplicates read FDuplicates write FDuplicates;}
  {property Sorted:Boolean read FSorted write SetSorted;}
  property OnChange:TNotifyEvent read FOnChange write FOnChange;
  property OnChanging:TNotifyEvent read FOnChanging write FOnChanging;
 end;
 
type
 {TStringBlock - Block object for TLinkedStringListEx}
 TStringBlock = class(TListObject)
  destructor Destroy; override;
 private
  {}
 public
  {}
  Data:Pointer;      {Pointer to Allocated Memory Block}
  Size:LongWord;     {Size of Allocated Memory Block}
  Start:LongWord;    {Index of First Item in this Block}
  Count:LongWord;    {Current Items in this Block}
  Capacity:LongWord; {Total Capacity of this Block}
end;
 
type
 {TStringObjectEx - A TStringObject for use with StringListEx}
 TStringObjectEx = class(TStringObject)
 private
  {}
  FBlock:TStringBlock;
 public
  {}
  property Block:TStringBlock read FBlock write FBlock;
end;
 
type
 {TLinkedStringListEx - A TLinkedStringList with block list for entry Indexing}
 TLinkedStringListEx = class(TLinkedStringList)
  constructor Create;
 public {Public destructor for FPC RTL}
  destructor Destroy; override;
 private
  {}
  FRecent:TStringBlock;
  FBlocks:TLinkedObjList;

  FCapacity:Integer;

  function GetBlock(Index:Integer):TStringBlock;
  function AddBlock(Block:TStringBlock;Index:Integer):TStringBlock;
  function DeleteBlock(Block:TStringBlock):Boolean;
  function UpdateBlocks(Block:TStringBlock):Boolean;

  function GetItem(Block:TStringBlock;Index:Integer):TStringObjectEx;
  function AddItem(Block:TStringBlock;Index:Integer;Item:TStringObjectEx):Boolean;
  function DeleteItem(Block:TStringBlock;Index:Integer;Item:TStringObjectEx):Boolean;

  function IndexOfItem(Block:TStringBlock;Item:TStringObjectEx):Integer;
 protected
  {}
  function Get(Index:Integer):String; override;
  function GetCapacity:Integer; override;
  function GetObject(Index:Integer):TObject; override;
  procedure Put(Index:Integer;const S:String); override;
  procedure PutObject(Index:Integer;AObject:TObject); override;
 public
  {}
  function Add(const S:String):Integer; override;
  procedure Clear; override;
  procedure Delete(Index:Integer); override;
  procedure Exchange(Index1,Index2:Integer); override;
  function IndexOf(const S:String):Integer; override;
  procedure Insert(Index:Integer;const S:String); override;
end;
 
{==============================================================================}
{Ultibo Class types (Hash String Lists)}
type
 THashLinkedStringList = class;
 {THashStringObject - A TObject with Prev/Next/Hash for use in HashStringLists}
 THashStringObject = class(TStringObjectEx)
 private
  {}
  FList:THashLinkedStringList;
  FKeyPrev:THashStringObject;
  FKeyNext:THashStringObject;

  procedure SetHash(AHash:LongWord);
 protected
  {}
  procedure SetValue(const AValue:String); override;
 public
  {}
  property List:THashLinkedStringList read FList write FList;
  property KeyPrev:THashStringObject read FKeyPrev write FKeyPrev;
  property KeyNext:THashStringObject read FKeyNext write FKeyNext;
 end;
 
 {THashLinkedStringList - A List of TStringObjects with automatic hash buckets}
 THashLinkedStringList = class(TLinkedStringListEx)
  constructor Create(AKeyBits:Byte);
 public {Public destructor for FPC RTL}
  destructor Destroy; override;
 private
  {}
  FKeyBits:Byte;
  FKeyMask:LongWord;
  FKeyShift:LongWord;
  FKeyBuckets:Pointer;
  function KeyLink(AValue:THashStringObject):Boolean;
  function KeyUnlink(AValue:THashStringObject):Boolean;
 protected
  {}
  procedure ClearList; override;
 public
  {}
  function KeyFirst(AKeyHash:LongWord):THashStringObject;

  function Add(const S:String):Integer; override;
  procedure Delete(Index:Integer); override;
  function IndexOf(const S:String):Integer; override;
  procedure Insert(Index:Integer;const S:String); override;
 end;
 
{==============================================================================}
{Ultibo Class types (Integer Lists)}
type
 {TIntegerList - Same as a TStringList but for Integer values}
 TIntegerList = class(TObject)
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FItems:TList; //To Do //Should this be something better than a TList ?
  function GetItem(Idx:Integer):Integer;
  procedure SetItem(Idx,Value:Integer);
  function GetCount:Integer;
 public
  {}
  property Items[Idx:Integer]:Integer read GetItem write SetItem;
  property Count:Integer read GetCount;
  function Add(AValue:Integer):Integer;
  function Remove(AValue:Integer):Integer;
  procedure Delete(Idx:Integer);
  function IndexOf(AValue:Integer):Integer;
  procedure Clear;
end;
 
{==============================================================================}
{Ultibo Class types (Date Time Lists)}
type
 {TDateTimeList - Same as a TStringList but for TDateTime values}
 TDateTimeList = class(TObject)
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FItems:TList; //To Do //Should this be something better than a TList ?
  function GetItem(Idx:Integer):TdateTime;
  procedure SetItem(Idx:Integer;Value:TDateTime);
  function GetCount:Integer;
 public
  {}
  property Items[Idx:Integer]:TDateTime read GetItem write SetItem;
  property Count:Integer read GetCount;
  function Add(AValue:TDateTime):Integer;
  function Remove(AValue:TDateTime):Integer;
  procedure Delete(Idx:Integer);
  function IndexOf(AValue:TDateTime):Integer;
  procedure Clear;
end;
 
{==============================================================================}
{Ultibo Class types (Memory Streams)}
type
 {TMemoryBlock - Block object for TMemoryStreamEx}
 TMemoryBlock = class(TListObject)
  destructor Destroy; override;
 private
  {}
 public
  {}
  Memory:Pointer;    {Pointer to Allocated Memory Block}
  Size:LongWord;     {Size of Allocated Memory Block}
  Start:LongWord;    {Start of Allocated Memory Block in the Stream}
end;
 
type
 {TMemoryStreamEx - A memory stream which does not Realloc on expand}
 TMemoryStreamEx = class(TStream)
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FRecent:TMemoryBlock;
  FBlocks:TLinkedObjList;

  FSize:LongInt;
  FPosition:LongInt;
  FCapacity:LongWord;

  function RoundSize(ASize:LongInt):LongWord;
  function GetBlock(AOffset:LongWord):TMemoryBlock;
  function ReadBlock(ABlock:TMemoryBlock;ABuffer:Pointer;AOffset,ACount:LongWord):LongWord;
  function WriteBlock(ABlock:TMemoryBlock;ABuffer:Pointer;AOffset,ACount:LongWord):LongWord;

  procedure SetCapacity(ACapacity:LongWord);
 protected
  {}
  procedure SetSize(ASize:LongInt); override;
 public
  {}
  function Read(var ABuffer;ACount:LongInt):LongInt; override;
  function Write(const ABuffer;ACount:LongInt):LongInt; override;
  function Seek(AOffset:LongInt;AOrigin:Word):LongInt; override;
  procedure SaveToStream(AStream:TStream);
  procedure LoadFromStream(AStream:TStream);
  procedure SaveToFile(const AFileName:String);
  procedure LoadFromFile(const AFileName:String);
  procedure Clear;
end;
 
{==============================================================================}
{Ultibo Class types (64bit Streams)}
{Note: No 64 bit TMemoryStreamEx because Pointers are currently only 32 bit} 
type
 {TStreamEx - A 64bit capable Stream class}
 TStreamEx = class(TStream)
 private
  {}
  function GetPositionEx:Int64;
  procedure SetPositionEx(const Pos:Int64);
  function GetSizeEx:Int64;
 protected
  {}
  procedure SetSizeEx(const NewSize:Int64); virtual;
 public
  {}
  property PositionEx:Int64 read GetPositionEx write SetPositionEx;
  property SizeEx:Int64 read GetSizeEx write SetSizeEx;
  function SeekEx(const Offset:Int64;Origin:Word):Int64; virtual; abstract;
end;
 
type
 {THandleStreamEx - A 64bit capable Handle Stream class}
 THandleStreamEx = class(TStreamEx)
  constructor Create(AHandle:Integer);
 private
  {}
  FHandle:Integer;
 protected
  {}
  procedure SetSize(NewSize:LongInt); override;
  procedure SetSizeEx(const NewSize:Int64); override;
 public
  {}
  function Read(var Buffer;Count:LongInt):LongInt; override;
  function Write(const Buffer;Count:LongInt):LongInt; override;
  function Seek(Offset:LongInt;Origin:Word):LongInt; override;
  function SeekEx(const Offset:Int64;Origin:Word):Int64; override;
  property Handle:Integer read FHandle;
end;
 
type
 {TFileStreamEx - A 64bit capable File Stream class}
 TFileStreamEx = class(THandleStreamEx)
  constructor Create(const FileName:String;Mode:Word);
  destructor Destroy; override;
 private
  {}
 public
  {}
end;

{==============================================================================}
{Ultibo Class types (String Lists)}
type
 {TStringItemEx - A String item with Hash for use in TStringListEx}
 TStringItemEx = class(TObject)
 private
  {}
  FValue:String;
  FData:TObject;
  FHash:LongWord;
 protected
  {}
  procedure SetValue(const AValue:String); virtual;
 public
  {}
  property Value:String read FValue write SetValue;
  property Data:TObject read FData write FData;
  property Hash:LongWord read FHash;
 end;
 
type
 {TStringListEx - A String List class that does not use realloc to grow}
 {Mostly identical methods to TStringList with addition of block list}
 {Note: Currently does not implement Sorting or Duplicate handling}
 TStringListEx = class(TStrings)
  constructor Create;
 public {Public destructor for FPC RTL}
  destructor Destroy; override;
 private
  {}
  FRecent:TStringBlock;
  FBlocks:TLinkedObjList;

  FCount:Integer;
  FCapacity:Integer;
  {FSorted:Boolean;}
  FUpdating:Boolean;
  {FDuplicates:TDuplicates;}
  FOnChange:TNotifyEvent;
  FOnChanging:TNotifyEvent;
  {procedure QuickSort(L,R:Integer);}
  {procedure SetSorted(Value:Boolean);}

  function GetBlock(Index:Integer):TStringBlock;
  function AddBlock(Block:TStringBlock;Index:Integer):TStringBlock;
  function DeleteBlock(Block:TStringBlock):Boolean;
  function UpdateBlocks(Block:TStringBlock):Boolean;

  function GetItem(Block:TStringBlock;Index:Integer):TStringItemEx;
  function AddItem(Block:TStringBlock;Index:Integer;Item:TStringItemEx):Boolean;
  function DeleteItem(Block:TStringBlock;Index:Integer;Item:TStringItemEx):Boolean;
 protected
  {}
  procedure ClearList; virtual;

  procedure Changed; virtual;
  procedure Changing; virtual;
  function Get(Index:Integer):String; override;
  function GetCapacity:Integer; override;
  function GetCount:Integer; override;
  function GetObject(Index:Integer):TObject; override;
  procedure Put(Index:Integer;const S:String); override;
  procedure PutObject(Index:Integer;AObject:TObject); override;
  procedure SetUpdateState(Updating:Boolean); override;
 public
  {}
  function Add(const S:String):Integer; override;
  procedure Clear; override;
  procedure Delete(Index:Integer); override;
  procedure Exchange(Index1,Index2:Integer); override;
  {function Find(const S:String;var Index:Integer):Boolean; virtual;}
  function IndexOf(const S:String):Integer; override;
  procedure Insert(Index:Integer;const S:String); override;
  {procedure Sort; virtual;}
  {property Duplicates:TDuplicates read FDuplicates write FDuplicates;}
  {property Sorted:Boolean read FSorted write SetSorted;}
  property OnChange:TNotifyEvent read FOnChange write FOnChange;
  property OnChanging:TNotifyEvent read FOnChanging write FOnChanging;
 end;
 
{==============================================================================}
{Ultibo Class Variables}
{var}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Helper Functions} 

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

procedure TimerExEvent(TimerEx:TTimerEx); 
begin
 {}
 if TimerEx = nil then Exit;
 
 TimerEx.Timer;
end;

{==============================================================================}
{==============================================================================}
{TThreadEx}
procedure TThreadEx.Execution;
begin
 {Nothing}
end;

{==============================================================================}

procedure TThreadEx.AfterExecution;
begin
 {Nothing}
end;

{==============================================================================}

procedure TThreadEx.BeforeExecution;
begin
 {Nothing}
end;

{==============================================================================}

procedure TThreadEx.Execute;
begin
 {}
 try
  BeforeExecution;
  try
   while not Terminated do
    begin
     Execution;
    end;
  finally
   AfterExecution;
  end;
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('ThreadEx: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end;
end;

{==============================================================================}

procedure TThreadEx.TerminateAndWaitFor;
begin
 {}
 if not Terminated then Terminate;
 if Suspended then Start; {Resume is Deprecated}
 WaitFor;
end;

{==============================================================================}
{==============================================================================}
{TTimerEx}
constructor TTimerEx.Create;
begin
 {}
 inherited Create;
 FTimerHandle:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

destructor TTimerEx.Destroy; 
begin
 {}
 if FEnabled then StopTimer;
 inherited Destroy;
end;

{==============================================================================}

procedure TTimerEx.SetEnabled(Value:Boolean);
begin
 {}
 if Value <> FEnabled then
  begin
   if Value then
    begin
     StartTimer;
    end  
   else
    begin
     StopTimer;
    end;
  end;  
end;

{==============================================================================}

procedure TTimerEx.Timer; 
begin
 {}
 {Check enabled in case the event fires once more after being set to false}
 if FEnabled and Assigned(FOnTimer) then
  begin
   FOnTimer(Self);
  end; 
end;

{==============================================================================}

procedure TTimerEx.StartTimer; 
var
 Flags:LongWord;
begin
 {}
 if FEnabled then Exit;
 
 FEnabled:=True;
 
 {Check Interval}
 if FInterval < 1 then FInterval:=1000;
 
 {Get Flags}
 Flags:=TIMER_FLAG_RESCHEDULE; {Rescheduled Automatically}
 if FWorker then Flags:=Flags or TIMER_FLAG_WORKER;
 
 {Create Timer}
 FTimerHandle:=TimerCreateEx(FInterval,TIMER_STATE_ENABLED,Flags,TTimerEvent(TimerExEvent),Self);
 if FTimerHandle = INVALID_HANDLE_VALUE then
  begin
   FEnabled:=False;
  end;
end;

{==============================================================================}

procedure TTimerEx.StopTimer; 
begin
 {}
 if not FEnabled then Exit;

 FEnabled:=False;

 {Destroy Timer}
 TimerDestroy(FTimerHandle);
 
 FTimerHandle:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}
{==============================================================================}
{TObjList}
constructor TObjList.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}

destructor TObjList.Destroy;
begin
 {}
 DestroyListObjects;
 inherited Destroy;
end;

{==============================================================================}

procedure TObjList.DestroyListObjects;
var
 ListCount:Integer;
 ListObject:TObject;
begin
 {}
 try
  for ListCount:=Count - 1 downto 0 do
   begin
    try
     ListObject:=TObject(Items[ListCount]);
     ListObject.Free;
    except
     {}
    end;
   end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TObjList.ClearList;
begin
 {}
 DestroyListObjects;
 Clear;
end;

{==============================================================================}
{==============================================================================}
{TThreadObjList}
constructor TThreadObjList.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}

destructor TThreadObjList.Destroy;
begin
 {}
 DestroyListObjects;
 inherited Destroy;
end;

{==============================================================================}

procedure TThreadObjList.DestroyListObjects;
var
 ListCount:Integer;
 ListObject:TObject;
begin
 {}
 try
  with LockList do
   begin
    try
     for ListCount:=Count-1 downto 0 do
      begin
       try
        ListObject:=TObject(Items[ListCount]);
        ListObject.Free;
       except
        {}
       end;
      end;
    finally
     UnlockList;
    end;
   end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TThreadObjList.ClearList;
begin
 {}
 with LockList do
  begin
   try
    DestroyListObjects;
    Clear;
   finally
    UnlockList;
   end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TLinkedList}
constructor TLinkedList.Create;
begin
 {}
 inherited Create;
 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TLinkedList.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TLinkedList.GetCount:Integer;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TLinkedList.Link(AValue:TListObject):Boolean;
{Link AValue to Prev,Next Siblings and Adjust First/Last}
var
 Prev:TListObject;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 Prev:=FLast;
 if Prev = nil then
  begin
   {Is First Object}
   AValue.Prev:=nil;
   AValue.Next:=nil;
   FFirst:=AValue;
   FLast:=AValue;
  end
 else
  begin
   {Not First Object}
   Prev.Next:=AValue;
   AValue.Prev:=Prev;
   AValue.Next:=nil;
   FLast:=AValue;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TLinkedList.LinkEx(APrev,AValue:TListObject):Boolean;
{Link AValue after APrev Sibling and Adjust First/Last/Prev/Next}
{If APrev is nil then Link as first value in list}
var
 Next:TListObject;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if APrev = nil then
  begin
   if FLast <> nil then
    begin
     {Not First Object}
     Next:=FFirst;
     FFirst:=AValue;
     AValue.Prev:=nil;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is First Object}
     AValue.Prev:=nil;
     AValue.Next:=nil;
     FFirst:=AValue;
     FLast:=AValue;
    end;
  end
 else
  begin
   if APrev.Next <> nil then
    begin
     {Not Last Object}
     Next:=APrev.Next;
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is Last Object}
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=nil;
     FLast:=AValue;
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TLinkedList.Unlink(AValue:TListObject):Boolean;
{Unlink AValue from Prev,Next Siblings and Adjust First/Last}
var
 Prev,Next:TListObject;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if AValue.Prev <> nil then
  begin
   {Not First Object}
   Prev:=AValue.Prev;
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=AValue.Next;
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end
   else
    begin
     {Is Last Object}
     Prev.Next:=nil;
     FLast:=Prev;
    end;
  end
 else
  begin
   {Is First Object}
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=AValue.Next;
     Next.Prev:=nil;
     FFirst:=Next;
    end
   else
    begin
     {Is Last Object}
     FFirst:=nil;
     FLast:=nil;
    end;
  end;
 AValue.Prev:=nil;
 AValue.Next:=nil;
 
 Result:=True;
end;

{==============================================================================}

function TLinkedList.Add(AValue:TListObject):Boolean;
{Add AValue to List and link with Siblings}
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if Link(AValue) then
  begin
   Inc(FCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TLinkedList.Remove(AValue:TListObject):Boolean;
{Unlink AValue from Siblings and Remove from List}
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if Unlink(AValue) then
  begin
   Dec(FCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TLinkedList.Insert(APrev,AValue:TListObject):Boolean;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if LinkEx(APrev,AValue) then
  begin
   Inc(FCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

procedure TLinkedList.Clear;
begin
 {}
 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}
{==============================================================================}
{TLinkedObjList}
constructor TLinkedObjList.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}

destructor TLinkedObjList.Destroy;
begin
 {}
 ClearList;
 inherited Destroy;
end;

{==============================================================================}

procedure TLinkedObjList.ClearList;
var
 Next:TListObject;
 Current:TListObject;
begin
 {}
 try
  {Clear Objects}
  Next:=FFirst;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=Current.Next;
    Current.Free;
   end;
   
  {Reset Defaults}
  Clear;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{TThreadLinkedObjList}
constructor TThreadLinkedObjList.Create;
begin
 {}
 inherited Create;
 InitializeCriticalSection(FLock);
end;

{==============================================================================}

destructor TThreadLinkedObjList.Destroy;
begin
 {}
 LockList;
 try
  ClearList;
  inherited Destroy;
 finally
  UnlockList;
  DeleteCriticalSection(FLock);
 end;
end;

{==============================================================================}

procedure TThreadLinkedObjList.ClearList;
var
 Next:TListObject;
 Current:TListObject;
begin
 {}
 try
  LockList;
  try
   {Clear Objects}
   Next:=FFirst;
   while Next <> nil do
    begin
     Current:=Next;
     Next:=Current.Next;
     Current.Free;
    end;
    
   {Reset Defaults}
   Clear;
  finally
   UnlockList;
  end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TThreadLinkedObjList.LockList;
begin
 {}
 EnterCriticalSection(FLock);
end;

{==============================================================================}

procedure TThreadLinkedObjList.UnlockList;
begin
 {}
 LeaveCriticalSection(FLock);
end;

{==============================================================================}
{==============================================================================}
{TLinkedTree}
constructor TLinkedTree.Create;
begin
 {}
 inherited Create;
 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TLinkedTree.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

function TLinkedTree.GetCount:Integer;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TLinkedTree.Link(AValue,AParent:TTreeObject):Boolean;
var
 Prev:TTreeObject;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if AParent <> nil then
  begin
   {Entry with Parent, add to Parents Chain}
   AValue.Parent:=AParent;
   if AParent.FirstChild = nil then
    begin
     {Is First Object}
     AValue.Prev:=nil;
     AValue.Next:=nil;
     AParent.FirstChild:=AValue;
     AParent.LastChild:=AValue;
    end
   else
    begin
     {Not First Object}
     Prev:=AParent.LastChild;
     Prev.Next:=AValue;
     AValue.Prev:=Prev;
     AValue.Next:=nil;
     AParent.LastChild:=AValue;
    end;
  end
 else
  begin
   {Entry with No Parent, add to Root Chain}
   {This is a standard List Link}
   Prev:=FLast;
   if Prev = nil then
    begin
     {Is First Object}
     AValue.Prev:=nil;
     AValue.Next:=nil;
     FFirst:=AValue;
     FLast:=AValue;
    end
   else
    begin
     {Not First Object}
     Prev.Next:=AValue;
     AValue.Prev:=Prev;
     AValue.Next:=nil;
     FLast:=AValue;
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TLinkedTree.LinkEx(APrev,AValue,AParent:TTreeObject):Boolean;
{Link AValue after APrev Sibling and Adjust FirstChild/LastChild/Prev/Next}
{If APrev is nil then Link as first child in parent or first value if Parent is nil}
var
 Next:TTreeObject;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if AParent <> nil then
  begin
   {Entry with Parent, add to Parents Chain}
   AValue.Parent:=AParent;
   if APrev = nil then
    begin
     if AParent.LastChild <> nil then
      begin
       {Not First Object}
       Next:=AParent.FirstChild;
       AParent.FirstChild:=AValue;
       AValue.Prev:=nil;
       AValue.Next:=Next;
       Next.Prev:=AValue;
      end
     else
      begin
       {Is First Object}
       AValue.Prev:=nil;
       AValue.Next:=nil;
       AParent.FirstChild:=AValue;
       AParent.LastChild:=AValue;
      end;
    end
   else
    begin
     if APrev.Next <> nil then
      begin
       {Not Last Object}
       Next:=TTreeObject(APrev.Next);
       APrev.Next:=AValue;
       AValue.Prev:=APrev;
       AValue.Next:=Next;
       Next.Prev:=AValue;
      end
     else
      begin
       {Is Last Object}
       APrev.Next:=AValue;
       AValue.Prev:=APrev;
       AValue.Next:=nil;
       AParent.LastChild:=AValue;
      end;
    end;
  end
 else
  begin
   {Entry with No Parent, add to Root Chain}
   AValue.Parent:=nil;
   {This is a standard List LinkEx}
   if APrev = nil then
    begin
     if FLast <> nil then
      begin
       {Not First Object}
       Next:=FFirst;
       FFirst:=AValue;
       AValue.Prev:=nil;
       AValue.Next:=Next;
       Next.Prev:=AValue;
      end
     else
      begin
       {Is First Object}
       AValue.Prev:=nil;
       AValue.Next:=nil;
       FFirst:=AValue;
       FLast:=AValue;
      end;
    end
   else
    begin
     if APrev.Next <> nil then
      begin
       {Not Last Object}
       Next:=TTreeObject(APrev.Next);
       APrev.Next:=AValue;
       AValue.Prev:=APrev;
       AValue.Next:=Next;
       Next.Prev:=AValue;
      end
     else
      begin
       {Is Last Object}
       APrev.Next:=AValue;
       AValue.Prev:=APrev;
       AValue.Next:=nil;
       FLast:=AValue;
      end;
    end;
  end;
 
 Result:=True;
end;

{==============================================================================}

function TLinkedTree.Unlink(AValue:TTreeObject):Boolean;
var
 Parent,Prev,Next:TTreeObject;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if AValue.Parent <> nil then
  begin
   {Unlink Entry from current Parent}
   Parent:=TTreeObject(AValue.Parent);
   if Parent.FirstChild = AValue then
    begin
     if AValue.Next = nil then
      begin
       {If Entry is First Child and only Child}
       Parent.FirstChild:=nil;
       Parent.LastChild:=nil;
      end
     else
      begin
       {If Entry is First Child and has other Siblings}
       Next:=TTreeObject(AValue.Next);
       Parent.FirstChild:=Next;
       Next.Prev:=nil;
      end;
    end
   else if Parent.LastChild = AValue then
    begin
     {If Entry is Last Child and not only Child (otherwise it would be First Child)}
     Prev:=TTreeObject(AValue.Prev);
     Parent.LastChild:=Prev;
     Prev.Next:=nil;
    end
   else
    begin
     {If Entry is not First Child and not Last Child and not only Child}
     Prev:=TTreeObject(AValue.Prev);
     Next:=TTreeObject(AValue.Next);
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end;
  end
 else
  begin
   {Unlink Entry with no Parent, remove from Root Chain}
   {This is a standard List Unlink}
   if AValue.Prev <> nil then
    begin
     {Not First Object}
     Prev:=TTreeObject(AValue.Prev);
     if AValue.Next <> nil then
      begin
       {Not Last Object}
       Next:=TTreeObject(AValue.Next);
       Prev.Next:=Next;
       Next.Prev:=Prev;
      end
     else
      begin
       {Is Last Object}
       Prev.Next:=nil;
       FLast:=Prev;
      end;
    end
   else
    begin
     {Is First Object}
     if AValue.Next <> nil then
      begin
       {Not Last Object}
       Next:=TTreeObject(AValue.Next);
       Next.Prev:=nil;
       FFirst:=Next;
      end
     else
      begin
       {Is Last Object}
       FFirst:=nil;
       FLast:=nil;
      end;
    end;
  end;
  
 AValue.Parent:=nil;
 AValue.Prev:=nil;
 AValue.Next:=nil;
 
 Result:=True;
end;

{==============================================================================}

function TLinkedTree.Add(AValue,AParent:TTreeObject):Boolean;
{Add AValue to List and Link with Parent and Siblings}
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if Link(AValue,AParent) then
  begin
   Inc(FCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TLinkedTree.Remove(AValue:TTreeObject):Boolean;
{Unlink AValue from Parent and Siblings, Remove from List}
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if Unlink(AValue) then
  begin
   Dec(FCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

function TLinkedTree.Insert(APrev,AValue,AParent:TTreeObject):Boolean;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if LinkEx(APrev,AValue,AParent) then
  begin
   Inc(FCount);
   
   Result:=True;
  end;
end;

{==============================================================================}

procedure TLinkedTree.Move(AValue,AParent:TTreeObject);
begin
 {}
 if AValue = nil then Exit;
 
 if Unlink(AValue) then
  begin
   Link(AValue,AParent);
  end;
end;

{==============================================================================}

procedure TLinkedTree.Clear;
begin
 {}
 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

function TLinkedTree.FirstChild(AParent:TTreeObject):TTreeObject;
begin
 {}
 if AParent <> nil then
  begin
   {Get First Child of Parent Chain}
   Result:=AParent.FirstChild;
  end
 else
  begin
   {Get First Child of Root Chain}
   Result:=FFirst;
  end;
end;

{==============================================================================}

function TLinkedTree.LastChild(AParent:TTreeObject):TTreeObject;
begin
 {}
 if AParent <> nil then
  begin
   {Get Last Child of Parent Chain}
   Result:=AParent.LastChild;
  end
 else
  begin
   {Get Last Child of Root Chain}
   Result:=FLast;
  end;
end;

{==============================================================================}
{==============================================================================}
{TLinkedObjTree}
constructor TLinkedObjTree.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}

destructor TLinkedObjTree.Destroy;
begin
 {}
 ClearList;
 inherited Destroy;
end;

{==============================================================================}

procedure TLinkedObjTree.ClearListItems(AParent:TTreeObject);
var
 Next:TTreeObject;
 Current:TTreeObject;
begin
 {}
 if AParent = nil then Exit;
 
 {Clear Objects}
 Next:=AParent.FirstChild;
 while Next <> nil do
  begin
   Current:=Next;
   Next:=TTreeObject(Current.Next);
   ClearListItems(Current);
   Current.Free;
  end;
end;

{==============================================================================}

procedure TLinkedObjTree.ClearList;
var
 Next:TTreeObject;
 Current:TTreeObject;
begin
 {}
 try
  {Clear Objects}
  Next:=FFirst;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=TTreeObject(Current.Next);
    ClearListItems(Current);
    Current.Free;
   end;
   
  {Reset Defaults}
  Clear;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{TBtreeObject}
procedure TBtreeObject.Clear;
begin
 {}
 FPrev:=nil;
 FNext:=nil;
 FLeft:=nil;
 FRight:=nil;
 FChild:=nil;
 FParent:=nil;
end;

{==============================================================================}
{==============================================================================}
{TLinkedBtree}
constructor TLinkedBtree.Create;
begin
 {}
 inherited Create;
 FOrder:=11;  {Default Size}
 FMedian:=6;

 FSwapLeft:=False;

 FRoot:=nil;
 FFirst:=nil;
 FLast:=nil;
 FFirstBlank:=nil;
 FLastBlank:=nil;
end;

{==============================================================================}

destructor TLinkedBtree.Destroy;
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TLinkedBtree.SetOrder(AOrder:LongWord);
{Set a new Order and calculate a new Median value}
{Minimum Order is 5 and minimum Median is 3}
begin
 {}
 if AOrder < 5 then Exit;
 if AOrder = FOrder then Exit;
 
 FOrder:=AOrder;
 FMedian:=(FOrder div 2);
 if (FOrder mod 2) > 0 then Inc(FMedian);
end;

{==============================================================================}

function TLinkedBtree.PropogateDrop(AEntry:TBtreeObject):Boolean;
var
 Left:Boolean;
 Parent:TBtreeObject;
 Current:TBtreeObject;
 Target:TBtreeObject;
 Neighbour:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 {Get Current}
 Current:=GetStart(AEntry);
 if Current = nil then Exit;
 {Check Drop}
 if RequireDrop(Current) then  {New 7/1/2010}
  begin
   {Get Drop}
   Neighbour:=GetDrop(Current,Left);
   if Neighbour = nil then Exit;
   if Neighbour = Current then Neighbour:=nil; {Get Drop returns Entry to indicate no Left or Right neighbour}
   if Current.Child <> nil then Neighbour:=nil; {New 18/7/2011} {If child exists drop without neighbour}
   {Get Target} {New 21/7/2011}
   Target:=nil;
   if Neighbour <> nil then
    begin
     Target:=GetTarget(Neighbour,Left);
     if Target = nil then Exit;
    end;
   {Drop Nodes}
   if not DropNode(Current,Neighbour,Target,Left) then Exit; {New 7/1/2010}
   if not Drop(Current,Neighbour,Target,Left) then Exit;     {New 7/1/2010}
   {Check Target} {New 21/7/2011}
   if (Target <> nil) and (Target <> Neighbour) then
    begin
     if not PropogateSplit(Target) then Exit;
    end;
   {Check Neighbour}
   if Neighbour <> nil then
    begin
     {Propogate Split}
     if not PropogateSplit(Neighbour) then Exit;
     {Get Parent}
     Parent:=Neighbour.Parent;
     {Check Parent}
     while Parent <> nil do
      begin
       {Get Current}
       Current:=Parent;
       {Get Parent}
       Parent:=nil;
       {Check Drop}
       if RequireDrop(Current) then  {New 7/1/2010}
        begin
         {Get Drop}
         Neighbour:=GetDrop(Current,Left);
         if Neighbour = nil then Exit;
         if Neighbour = Current then Neighbour:=nil; {Get Drop returns Entry to indicate no Left or Right neighbour}
         if Current.Child <> nil then Neighbour:=nil; {New 18/7/2011} {If child exists drop without neighbour}
         {Get Target} {New 21/7/2011}
         Target:=nil;
         if Neighbour <> nil then
          begin
           Target:=GetTarget(Neighbour,Left);
           if Target = nil then Exit;
          end;
         {Drop Nodes}
         if not DropNode(Current,Neighbour,Target,Left) then Exit; {New 7/1/2010}
         if not Drop(Current,Neighbour,Target,Left) then Exit;     {New 7/1/2010}
         {Check Target} {New 21/7/2011}
         if (Target <> nil) and (Target <> Neighbour) then
          begin
           if not PropogateSplit(Target) then Exit;
          end;
         {Check Neighbour}
         if Neighbour <> nil then
          begin
           {Propogate Split}
           if not PropogateSplit(Neighbour) then Exit;
           {Get Parent}
           Parent:=Neighbour.Parent;
          end;
        end;
      end;
    end;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.PropogateMerge(AEntry:TBtreeObject):Boolean;
begin
 {}
 Result:=False;
 //To Do ?
end;

{==============================================================================}

function TLinkedBtree.PropogateSplit(AEntry:TBtreeObject):Boolean;
{Note: This is currently not used by insert and must be tested before use}
var
 Parent:TBtreeObject;
 Current:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 {Get Current}
 Current:=GetStart(AEntry);
 if Current = nil then Exit;
 {Get Parent}
 Parent:=Current.Parent; {Parent may be nil}
 {Check Push}
 if RequirePush(Current) then  {New 22/12/2009}
  begin
   {Push Node}
   if not PushNode(Current) then Exit; {New 22/12/2009}
   if not Push(Current) then Exit;
   {Check Parent}
   while Parent <> nil do
    begin
     {Get Current}
     Current:=Parent;
     {Get Parent}
     Parent:=Current.Parent; {Parent may be nil}
     {Check Push}
     if RequirePush(Current) then  {New 22/12/2009}
      begin
       {Push Node}
       if not PushNode(Current) then Exit; {New 22/12/2009}
       if not Push(Current) then Exit;
      end
     else
      begin
       {Check Split}
       if RequireSplit(Current) then
        begin
         {Split Node}
         if not SplitNode(Current) then Exit; {New 22/12/2009}
         if not Split(Current) then Exit;
        end;
      end;
    end;
  end
 else
  begin
   {Check Split}
   if RequireSplit(Current) then
    begin
     {Split Node}
     if not SplitNode(Current) then Exit; {New 22/12/2009}
     if not Split(Current) then Exit;
     {Check Parent}
     while Parent <> nil do
      begin
       {Get Current}
       Current:=Parent;
       {Get Parent}
       Parent:=Current.Parent; {Parent may be nil}
       {Check Push}
       if RequirePush(Current) then  {New 22/12/2009}
        begin
         {Push Node}
         if not PushNode(Current) then Exit; {New 22/12/2009}
         if not Push(Current) then Exit;
        end
       else
        begin
         {Check Split}
         if RequireSplit(Current) then
          begin
           {Split Node}
           if not SplitNode(Current) then Exit; {New 22/12/2009}
           if not Split(Current) then Exit;
          end;
        end;
      end;
    end;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.GetCount(AEntry:TBtreeObject):LongWord;
{Get the count of entries in the node of the supplied entry}
{Note: Does not include the blank key}
var
 Sibling:TBtreeObject;
begin
 {}
 Result:=0;
 if AEntry = nil then Exit;
 Sibling:=GetStart(AEntry);
 while Sibling <> nil do
  begin
   if not Sibling.Blank then Inc(Result);
   Sibling:=Sibling.Right;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetDepth(AEntry:TBtreeObject):LongWord;
{Get the depth of the supplied entry in the btree}
var
 Parent:TBtreeObject;
begin
 {}
 Result:=0;
 if AEntry = nil then Exit;
 Result:=1;
 Parent:=AEntry.Parent;
 while Parent <> nil do
  begin
   Inc(Result);
   Parent:=Parent.Parent;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetEnd(AEntry:TBtreeObject):TBtreeObject;
{Get the end entry in the node of the supplied entry}
{Note: Includes the blank key which may be the only key}
begin
 {}
 Result:=GetBlank(AEntry);
end;

{==============================================================================}

function TLinkedBtree.GetStart(AEntry:TBtreeObject):TBtreeObject;
{Get the start entry in the node of the supplied entry}
{Note: Includes the blank key which may be the only key}
var
 Sibling:TBtreeObject;
begin
 {}
 Result:=AEntry;
 if AEntry = nil then Exit;
 Sibling:=AEntry.Left;
 while Sibling <> nil do
  begin
   Result:=Sibling;
   Sibling:=Sibling.Left;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetBlank(AEntry:TBtreeObject):TBtreeObject;
{Get the blank entry in the node of the supplied entry}
{Note: Blank entry is always the last entry in every node}
var
 Sibling:TBtreeObject;
begin
 {}
 Result:=AEntry;
 if AEntry = nil then Exit;
 Sibling:=AEntry.Right;
 while Sibling <> nil do
  begin
   Result:=Sibling;
   Sibling:=Sibling.Right;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetMedian(AEntry:TBtreeObject):TBtreeObject;
{Get the median entry in the node of the supplied entry}
{Note: Does not include the blank key}
var
 Count:LongWord;
 Sibling:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 Count:=0;
 Sibling:=GetStart(AEntry);
 while Sibling <> nil do
  begin
   if not Sibling.Blank then Inc(Count);
   if Count = FMedian then Break;
   Sibling:=Sibling.Right;
  end;
 Result:=Sibling; {Sibling will only be not nil if Median was reached}
end;

{==============================================================================}

function TLinkedBtree.GetDrop(AEntry:TBtreeObject;var ALeft:Boolean):TBtreeObject;
{Get the neighbour with appropriate number of keys to drop}
{Always drop with the Righthand neighbour if available}

{Note: Only supported by descendant classes with non balanced nodes}
begin
 {Virtual Base}
 Result:=nil;
end;

{==============================================================================}

function TLinkedBtree.GetMerge(AEntry:TBtreeObject):TBtreeObject;
{Get the neighbour with appropriate number of keys to merge}
{Always merge with the Righthand neighbour if available}
var
 Lefthand:TBtreeObject;
 Righthand:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Get Lefthand/Righthand}
 Lefthand:=GetLefthand(AEntry);
 Righthand:=GetRighthand(AEntry);
 {Check Lefthand/Righthand}
 if (Righthand <> nil) and ((GetCount(Righthand) + GetCount(AEntry)) < FOrder) then
  begin
   Result:=Righthand;
  end
 else if (Lefthand <> nil) and ((GetCount(Lefthand) + GetCount(AEntry)) < FOrder) then
  begin
   Result:=Lefthand;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetBorrow(AEntry:TBtreeObject):TBtreeObject;
{Get the neighbour with sufficient keys to borrow one}
{Always borrow from the Righthand neighbour if available}
var
 Lefthand:TBtreeObject;
 Righthand:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Get Lefthand/Righthand}
 Lefthand:=GetLefthand(AEntry);
 Righthand:=GetRighthand(AEntry);
 {Check Lefthand/Righthand}
 if GetCount(Righthand) > (FMedian - 1) then
  begin
   Result:=Righthand;
  end
 else if GetCount(Lefthand) > (FMedian - 1) then
  begin
   Result:=Lefthand;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetTarget(ADrop:TBtreeObject;ALeft:Boolean):TBtreeObject;
{Get the actual target within the neighbour that is appropriate to drop}

{Note: Only supported by descendant classes with non balanced nodes}
begin
 {Virtual Base}
 Result:=nil;
end;

{==============================================================================}

function TLinkedBtree.SetParent(AEntry,AParent:TBtreeObject):Boolean;
{Set the parent for all entries in a node and set the child of the parent}
{Note: Includes the blank key which may be the only key}
var
 Sibling:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit; {Parent may be nil}
 {Get Start of Node}
 Sibling:=GetStart(AEntry);
 if Sibling = nil then Exit;
 {Set Child of Parent}
 if AParent <> nil then
  begin
   AParent.Child:=Sibling;
  end;
 {Set Parent of Siblings}
 while Sibling <> nil do
  begin
   Sibling.Parent:=AParent;
   Sibling:=Sibling.Right;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.GetLefthand(AEntry:TBtreeObject):TBtreeObject;
{Get the lefthand neighbour (Node) of the supplied entries node}
var
 Left:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Get Parent}
 Parent:=AEntry.Parent;
 if Parent = nil then Exit;
 {Get Left}
 Left:=Parent.Left;
 if Left = nil then Exit;
 {Get Child}
 Result:=Left.Child;
end;

{==============================================================================}

function TLinkedBtree.GetRighthand(AEntry:TBtreeObject):TBtreeObject;
{Get the righthand neighbour (Node) of the supplied entries node}
var
 Right:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Get Parent}
 Parent:=AEntry.Parent;
 if Parent = nil then Exit;
 {Get Right}
 Right:=Parent.Right;
 if Right = nil then Exit;
 {Get Child}
 Result:=Right.Child;
end;

{==============================================================================}

function TLinkedBtree.GetLeftmost(AEntry:TBtreeObject):TBtreeObject;
{Get the leftmost entry in the tree of the supplied entry}
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Get Start of Node}
 Result:=GetStart(AEntry);
 if Result = nil then Exit;
 {Check for Child}
 if Result.Child <> nil then
  begin
   {Get Leftmost of Child}
   Result:=GetLeftmost(Result.Child);
  end;
end;

{==============================================================================}

function TLinkedBtree.GetRightmost(AEntry:TBtreeObject):TBtreeObject;
{Get the rightmost entry in the tree of the supplied entry}
{Note: Does not include the blank key}
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Get End of Node}
 Result:=GetEnd(AEntry);
 if Result = nil then Exit;
 {Check for Child}
 if Result.Child <> nil then
  begin
   {Get Rightmost of Child}
   Result:=GetRightmost(Result.Child);
  end
 else
  begin
   {Get Left of Blank}
   Result:=Result.Left; {Note: Some callers (eg GetPredecessor) rely on this not returning the blank key}
  end;
end;

{==============================================================================}

function TLinkedBtree.GetSuccessor(AEntry:TBtreeObject):TBtreeObject;
{Get the successor (Right) entry of the supplied entry}
{The returned entry may be a blank key or a real key}
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Check for Right}
 if AEntry.Right <> nil then
  begin
   Result:=AEntry.Right;
   {Check for Child}
   if Result.Child <> nil then
    begin
     Result:=GetLeftmost(Result.Child);
    end;
  end
 else
  begin
   {Check for Parent}
   if AEntry.Parent <> nil then
    begin
     Result:=AEntry.Parent;
    end;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetPredecessor(AEntry:TBtreeObject):TBtreeObject;
{Get the predecessor (Left) entry of the supplied entry}
{The returned entry may be a blank key or a real key}
var
 Parent:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 {Check for Child}
 if AEntry.Child <> nil then
  begin
   Result:=GetRightmost(AEntry.Child); {Note: Does not return the blank key}
  end
 else
  begin
   {Check for Left}
   if AEntry.Left <> nil then
    begin
     Result:=AEntry.Left;
    end
   else
    begin
     {Check for Parent}
     Parent:=AEntry.Parent;
     while Parent <> nil do
      begin
       {Check for Left}
       if Parent.Left <> nil then
        begin
         Result:=Parent.Left;
         Exit;
        end;
       Parent:=Parent.Parent;
      end;
    end;
  end;
end;

{==============================================================================}

function TLinkedBtree.GetPosition(AStart,AEntry:TBtreeObject):TBtreeObject;
{Get the position where entry should be inserted into the btree}
{The returned entry will be the entry to the right in insert node}
{The returned entry may be the blank key or may be a real key}
var
 Current:TBtreeObject;
begin
 {}
 Result:=nil;
 if AEntry = nil then Exit;
 Current:=AStart;
 if Current = nil then Current:=FRoot;
 if Current = nil then Exit;
 {Check Left or Right}
 case Compare(AEntry,Current) of
  {Entry = Current}
  btreeCompareEqual:begin
    {Entry Exists}
    Exit;
   end;
  {Entry < Current}
  btreeCompareLess:begin
    {Follow Left}
    if Current.Child <> nil then
     begin
      Result:=GetPosition(Current.Child,AEntry);
     end
    else
     begin
      Result:=Current;
     end;
   end;
  {Entry > Current}
  btreeCompareGreater:begin
    {Follow Right}
    if Current.Right <> nil then
     begin
      Result:=GetPosition(Current.Right,AEntry);
     end
    else
     begin
      Exit; {Cannot happen as last key is always blank key and compare returns less than}
     end;
   end;
 end;
end;

{==============================================================================}

function TLinkedBtree.Push(AEntry:TBtreeObject):Boolean;
{Push the node containing the supplied entry}
{Will create a new blank as root}

{All keys will be parented by new blank root}

{Push can only occur on the root node}
var
 Start:TBtreeObject;
 Blank:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 {Check for Parent}   {Entry must not have a parent}
 if AEntry.Parent <> nil then Exit;

 {Get Start}
 Start:=GetStart(AEntry);
 if Start = nil then Exit;

 {Root Blank}
 FRoot:=nil;
 Blank:=CreateBlank;
 if not LinkBlank(Blank) then Exit;
 if not Attach(nil,Blank,nil) then Exit;
 if not AttachBlank(Blank) then Exit; {New 20/8/2009}

 {Set Parent}
 if not SetParent(Start,Blank) then Exit;
 if not SetParentEntry(Start,Blank) then Exit; {New 22/12/2009}

 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.Split(AEntry:TBtreeObject):Boolean;
{Split the node containing the supplied entry}
{Will promote the median entry to the parent}

{Keys left of median will be parented by median}
{Keys right of median will retain current parent}
{Child of median will parent to new blank on left}
{Median will be attached to the left of the parent}

{Split can propogate all the way to root since median is promoted}
var
 Left:TBtreeObject;
 Right:TBtreeObject;
 Child:TBtreeObject;
 Parent:TBtreeObject;

 Start:TBtreeObject;
 Blank:TBtreeObject;
 Median:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 {Get Start}
 Start:=GetStart(AEntry);
 if Start = nil then Exit;
 {Get Median}
 Median:=GetMedian(Start);
 if Median = nil then Exit;
 {Get Left}
 Left:=Median.Left;
 if Left = nil then Exit;
 {Get Right}
 Right:=Median.Right;
 if Right = nil then Exit;
 {Get Child}
 Child:=Median.Child;   {Child may be nil}
 {Get Parent}
 Parent:=Median.Parent; {Parent may be nil}

 {Detach Median}
 if not DetachEntry(Median) then Exit;
 if not Detach(Median) then Exit;
 {Split Node}
 Left.Right:=nil;
 Right.Left:=nil;

 {Left Parent}
 if not SetParent(Start,Median) then Exit;
 if not SetParentEntry(Start,Median) then Exit; {New 22/12/2009}
 {Left Blank}
 Blank:=CreateBlank;
 if not LinkBlank(Blank) then Exit;
 if not Attach(Median,Blank,nil) then Exit;
 if not AttachBlank(Blank) then Exit; {New 20/8/2009}

 {Check for Child}
 if Child <> nil then
  begin
   {Child Parent}
   if not SetParent(Child,Blank) then Exit;
   if not SetParentEntry(Child,Blank) then Exit; {New 22/12/2009}
  end;

 {Check for Parent}
 if Parent = nil then
  begin
   {Promote to Root}
   {Root Blank}
   FRoot:=nil;
   Blank:=CreateBlank;
   if not LinkBlank(Blank) then Exit;
   if not Attach(nil,Blank,nil) then Exit;
   if not AttachBlank(Blank) then Exit; {New 20/8/2009}
   {Attach Median}
   if not Attach(nil,Median,Blank) then Exit;
   if not AttachEntry(Median) then Exit;
   {Right Parent}
   if not SetParent(Right,Blank) then Exit;
   if not SetParentEntry(Right,Blank) then Exit; {New 22/12/2009}
  end
 else
  begin
   {Promote to Parent}
   {Attach Median}
   if not Attach(Parent.Parent,Median,Parent) then Exit;
   if not AttachEntry(Median) then Exit;
   {Right Parent}
   if not SetParent(Right,Parent) then Exit;
   if not SetParentEntry(Right,Parent) then Exit; {New 22/12/2009}
  end;

 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.Swap(AEntry,ASwap:TBtreeObject;ALeft:Boolean):Boolean;
{Swap the supplied entries directly from node to node}
{Entry is always a parent and Swap is always a leaf}

{No balancing is done as the entry is to be deleted or borrowed}
var
 Left:TBtreeObject;
 Right:TBtreeObject;
 Child:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if ASwap = nil then Exit;
 {Check for Blank}     {Neither entry can be a blank}
 if AEntry.Blank then Exit;
 if ASwap.Blank then Exit;
 {Check for Child}     {Child on Entry, no child on Swap}
 if AEntry.Child = nil then Exit;
 if ASwap.Child <> nil then Exit;

 {Check for Predecessor}
 if ALeft then if ASwap <> GetPredecessor(AEntry) then Exit;
 {Check for Successor}
 if not ALeft then if ASwap <> GetSuccessor(AEntry) then Exit;

 {Swap the Left and Right links}
 {Get Left/Right}     {From Entry (Parent)}
 Left:=AEntry.Left;
 Right:=AEntry.Right;

 {Swap Left/Right}    {From Swap to Entry}
 AEntry.Left:=ASwap.Left;
 AEntry.Right:=ASwap.Right;

 {Swap Left/Right}    {From Entry to Swap}
 ASwap.Left:=Left;
 ASwap.Right:=Right;

 {Link Left/Right}    {To Swap (Parent)}
 if Left <> nil then Left.Right:=ASwap;
 if Right <> nil then Right.Left:=ASwap;

 {Get Left/Right}     {From Entry (Child)}
 Left:=AEntry.Left;
 Right:=AEntry.Right;

 {Link Left/Right}    {To Entry (Child)}
 if Left <> nil then Left.Right:=AEntry;
 if Right <> nil then Right.Left:=AEntry;

 {Swap the Child and Parent links} {Note: The order of these is important for both Left and Right swap to work}
 {Get Child/Parent}   {From Entry (Parent)}
 Child:=AEntry.Child;
 Parent:=AEntry.Parent;

 {Swap Child/Parent}  {From Swap to Entry}
 AEntry.Child:=ASwap.Child;
 AEntry.Parent:=ASwap.Parent;

 {Swap Child/Parent}  {From Entry to Swap}
 ASwap.Child:=Child;
 ASwap.Parent:=Parent;

 {Check Child/Parent} {For Direct Swap of Entry and Child}   {New 18/1/2010}
 if ASwap.Child = ASwap then ASwap.Child:=AEntry;
 if AEntry.Parent = AEntry then AEntry.Parent:=ASwap;

 {Get Child/Parent}   {From Swap (Parent)}                   {New 18/1/2010}
 Child:=ASwap.Child;
 Parent:=ASwap.Parent;

 {Link Child/Parent}  {To Swap (Parent)}
 if Child <> nil then SetParent(Child,ASwap);
 if Child <> nil then SetParentEntry(Child,ASwap); {New 22/12/2009}
 if Parent <> nil then SetParent(ASwap,Parent);
 if Parent <> nil then SetParentEntry(ASwap,Parent); {New 22/12/2009}

 {Get Child/Parent}   {From Entry (Child)}
 Child:=AEntry.Child;
 Parent:=AEntry.Parent;

 {Link Child/Parent}  {To Entry (Child)}
 if Child <> nil then SetParent(Child,AEntry);
 if Child <> nil then SetParentEntry(Child,AEntry); {New 22/12/2009}
 if Parent <> nil then SetParent(AEntry,Parent);
 if Parent <> nil then SetParentEntry(AEntry,Parent); {New 22/12/2009}

 {Swap Root}          {From Entry to Swap}
 if AEntry = FRoot then FRoot:=ASwap;

 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.Drop(AEntry,ADrop,ATarget:TBtreeObject;ALeft:Boolean):Boolean;
{Drop the nodes of the supplied entries into one}
{Will demote the parent entry to the drop node}

{Drop with Right} {Entry has no child}
{Parent of left node will become part of target node (start)}
{Parent of right node will remain as parent of drop node}
{Left node will be deleted}

{Drop with Right} {Entry has a child}
{Node will be deleted}
{Child of node will be parented by parent of node}

{Drop with Left} {Entry has no child}
{Parent of left node will become part of target node (last before blank)}
{Parent of right node will become parent of drop node}
{Right node will be deleted}

{Drop with Left} {Entry has a child}
{Node will be deleted}
{Child of node will be parented by parent of node}

{Drop with no Neighbour}
{Node will be deleted}
{Child of node will be parented by parent of node}

{Drop can propogate all the way to root since parent is demoted}
var
 Left:TBtreeObject;
 Right:TBtreeObject;
 Last:TBtreeObject;
 Start:TBtreeObject;
 Blank:TBtreeObject;
 Child:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit; {Drop may be nil} {Target may be nil}
 {Check for Parent}   {Entry must have a parent}
 if AEntry.Parent = nil then Exit;

 if ADrop = nil then
  begin
   {Drop with no Neighbour}
   {Get Blank}   {Blank key of entry node which will be deleted}
   Blank:=GetBlank(AEntry);
   if Blank = nil then Exit;
   {Get Parent}  {Parent key of entry node which will be parent of child}
   Parent:=AEntry.Parent;
   {Get Child}   {Child key of blank which will be child of parent}
   Child:=Blank.Child; {Child may be nil}

   {Detach Blank}
   if not DetachBlank(Blank) then Exit;
   if not Detach(Blank) then Exit;
   if not UnlinkBlank(Blank) then Exit;
   if not DeleteBlank(Blank) then Exit;

   {Link Child}
   Parent.Child:=Child;     //To Do //Testing4

   {Check Child}
   if Child <> nil then
    begin
     {Set Parent}
     if not SetParent(Child,Parent) then Exit;
     if not SetParentEntry(Child,Parent) then Exit;
    end;

   Result:=True;
  end
 else
  begin
   {Check for Parent}   {Drop must have a parent}
   if ADrop.Parent = nil then Exit;
   {Check Left}
   if not(ALeft) then
    begin
     {Drop with Right}
     if AEntry.Child <> nil then
      begin
       {Convert to Drop with no Neighbour}
       Result:=Drop(AEntry,nil,nil,ALeft);
      end
     else
      begin
       {Get Blank}   {Blank key of entry node which will be deleted}
       Blank:=GetBlank(AEntry);
       if Blank = nil then Exit;
       {Get Start}   {Start key of drop node which will be linked}
       Start:=ATarget; //GetLeftmost(ADrop);        //GetStart(ADrop); //To Do //Testing3
       if Start = nil then Exit;
       {Get Parent}  {Parent key of entry node which will be demoted}
       Parent:=AEntry.Parent;
       {Get Right}   {Right key of parent which will become parent}
       Right:=Start.Parent;             //Parent.Right; //To Do //Testing2
       if Right = nil then Exit;
       {Get Child}   {Child key of blank which will be child of demoted}
       Child:=Blank.Child; {Child may be nil}

       {Detach Parent}
       if not DetachEntry(Parent) then Exit;
       if not Detach(Parent) then Exit;
       Parent.Child:=nil;
       {Detach Blank}
       if not DetachBlank(Blank) then Exit;
       if not Detach(Blank) then Exit;
       if not UnlinkBlank(Blank) then Exit;
       if not DeleteBlank(Blank) then Exit;
       {Link Start}
       Parent.Right:=Start;
       Start.Left:=Parent;
       {Set Parent} {Must pass the demoted key for NTFS}
       if not SetParent(Parent,Right) then Exit;      {if not SetParent(Start,Right) then Exit;}
       if not SetParentEntry(Parent,Right) then Exit; {if not SetParentEntry(Start,Right) then Exit;}

       {Check Child} {Normally nil}
       if Child <> nil then
        begin
         {Set Parent}
         if not SetParent(Child,Parent) then Exit;
         if not SetParentEntry(Child,Parent) then Exit;
        end;

       Result:=True;
      end;
    end
   else
    begin
     {Drop with Left}
     if AEntry.Child <> nil then
      begin
       {Convert to Drop with no Neighbour}
       Result:=Drop(AEntry,nil,nil,ALeft);
      end
     else
      begin
       {Get Blank}   {Blank key of entry node which will be deleted}
       Blank:=GetBlank(AEntry);
       if Blank = nil then Exit;
       {Get Last}    {Blank key of drop node which will be linked}
       Last:=ATarget; //GetRightmost(ADrop);    //GetBlank(ADrop); //To Do //Testing3
       if Last = nil then Exit;
       Last:=GetBlank(Last); {In case Target equals Drop and is the Start not the Blank}   //To Do //Testing3
       if Last = nil then Exit;
       {Get Left}    {Last key of drop node which will be linked}
       Left:=Last.Left;
       if Left = nil then Exit;
       {Get Parent}  {Parent key of drop node which will be demoted}
       Parent:=ADrop.Parent;
       {Get Right}   {Right key of parent which will become parent}
       Right:=Parent.Right;
       if Right = nil then Exit;
       {Get Child}   {Child key of blank which will be child of demoted}
       Child:=Blank.Child; {Child may be nil}

       {Detach Parent}
       if not DetachEntry(Parent) then Exit;
       if not Detach(Parent) then Exit;
       Parent.Child:=nil;
       {Detach Blank}
       if not DetachBlank(Blank) then Exit;
       if not Detach(Blank) then Exit;
       if not UnlinkBlank(Blank) then Exit;
       if not DeleteBlank(Blank) then Exit;
       {Link Parent}
       Left.Right:=Parent;
       Parent.Left:=Left;
       {Link Last}
       Parent.Right:=Last;
       Last.Left:=Parent;

       {Set Parent} {Must be in this order}             //To Do //Testing2
       if not SetParent(ADrop,Right) then Exit;
       if not SetParentEntry(ADrop,Right) then Exit;
       {Set Parent}                                     //To Do //Testing2
       if not SetParent(Parent,Last.Parent) then Exit;
       if not SetParentEntry(Parent,Last.Parent) then Exit;

       //{Set Parent} {Must pass the demoted key for NTFS}
       //if not SetParent(Parent,Right) then Exit;      {if not SetParent(Last,Right) then Exit;}
       //if not SetParentEntry(Parent,Right) then Exit; {if not SetParentEntry(Last,Right) then Exit;}

       {Check Child} {Normally nil}
       if Child <> nil then
        begin
         {Set Parent}
         if not SetParent(Child,Parent) then Exit;
         if not SetParentEntry(Child,Parent) then Exit;
        end;

       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}
//To Do //Remove ?
function TLinkedBtree.DropOld(AEntry,ADrop:TBtreeObject;ALeft:Boolean):Boolean;
{Note: This one works but was replaced to implement Target}

{Drop the nodes of the supplied entries into one}
{Will demote the parent entry to the drop node}

{Drop with Right} {Entry has no child}
{Parent of left node will become part of drop node (start)}
{Parent of right node will remain as parent of drop node}
{Left node will be deleted}

{Drop with Right} {Entry has a child}
{Node will be deleted}
{Child of node will be parented by parent of node}

{Drop with Left} {Entry has no child}
{Parent of left node will become part of drop node (last before blank)}
{Parent of right node will become parent of drop node}
{Right node will be deleted}

{Drop with Left} {Entry has a child}
{Node will be deleted}
{Child of node will be parented by parent of node}

{Drop with no Neighbour}
{Node will be deleted}
{Child of node will be parented by parent of node}

{Drop can propogate all the way to root since parent is demoted}
var
 Left:TBtreeObject;
 Right:TBtreeObject;
 Last:TBtreeObject;
 Start:TBtreeObject;
 Blank:TBtreeObject;
 Child:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit; {Drop may be nil}
 {Check for Parent}   {Entry must have a parent}
 if AEntry.Parent = nil then Exit;

 if ADrop = nil then
  begin
   {Drop with no Neighbour}
   {Get Blank}   {Blank key of entry node which will be deleted}
   Blank:=GetBlank(AEntry);
   if Blank = nil then Exit;
   {Get Parent}  {Parent key of entry node which will be parent of child}
   Parent:=AEntry.Parent;
   {Get Child}   {Child key of blank which will be child of parent}
   Child:=Blank.Child; {Child may be nil}

   {Detach Blank}
   if not DetachBlank(Blank) then Exit;
   if not Detach(Blank) then Exit;
   if not UnlinkBlank(Blank) then Exit;
   if not DeleteBlank(Blank) then Exit;

   {Check Child}
   if Child <> nil then
    begin
     {Set Parent}
     if not SetParent(Child,Parent) then Exit;
     if not SetParentEntry(Child,Parent) then Exit;
    end;

   Result:=True;
  end
 else
  begin
   {Check for Parent}   {Drop must have a parent}
   if ADrop.Parent = nil then Exit;
   {Check Left}
   if not(ALeft) then
    begin
     {Drop with Right}
     if AEntry.Child <> nil then
      begin
       {Convert to Drop with no Neighbour}
       Result:=DropOld(AEntry,nil,ALeft);
      end
     else
      begin
       {Get Blank}   {Blank key of entry node which will be deleted}
       Blank:=GetBlank(AEntry);
       if Blank = nil then Exit;
       {Get Start}   {Start key of drop node which will be linked}
       Start:=GetLeftmost(ADrop);        //GetStart(ADrop); //To Do //Testing2
       if Start = nil then Exit;
       {Get Parent}  {Parent key of entry node which will be demoted}
       Parent:=AEntry.Parent;
       {Get Right}   {Right key of parent which will become parent}
       Right:=Start.Parent;             //Parent.Right; //To Do //Testing2
       if Right = nil then Exit;
       {Get Child}   {Child key of blank which will be child of demoted}
       Child:=Blank.Child; {Child may be nil}

       {Detach Parent}
       if not DetachEntry(Parent) then Exit;
       if not Detach(Parent) then Exit;
       Parent.Child:=nil;
       {Detach Blank}
       if not DetachBlank(Blank) then Exit;
       if not Detach(Blank) then Exit;
       if not UnlinkBlank(Blank) then Exit;
       if not DeleteBlank(Blank) then Exit;
       {Link Start}
       Parent.Right:=Start;
       Start.Left:=Parent;
       {Set Parent} {Must pass the demoted key for NTFS}
       if not SetParent(Parent,Right) then Exit;      {if not SetParent(Start,Right) then Exit;}
       if not SetParentEntry(Parent,Right) then Exit; {if not SetParentEntry(Start,Right) then Exit;}

       //To Do //Remove ?
       {Check Child}
       if Child <> nil then
        begin
         {Set Parent}
         if not SetParent(Child,Parent) then Exit;
         if not SetParentEntry(Child,Parent) then Exit;
        end;

       Result:=True;
      end;
    end
   else
    begin
     {Drop with Left}
     if AEntry.Child <> nil then
      begin
       {Convert to Drop with no Neighbour}
       Result:=DropOld(AEntry,nil,ALeft);
      end
     else
      begin
       {Get Blank}   {Blank key of entry node which will be deleted}
       Blank:=GetBlank(AEntry);
       if Blank = nil then Exit;
       {Get Last}    {Blank key of drop node which will be linked}
       Last:=GetRightmost(ADrop);    //GetBlank(ADrop); //To Do //Testing2
       if Last = nil then Exit;
       Last:=GetBlank(Last);                            //To Do //Testing2
       if Last = nil then Exit;
       {Get Left}    {Last key of drop node which will be linked}
       Left:=Last.Left;
       if Left = nil then Exit;
       {Get Parent}  {Parent key of drop node which will be demoted}
       Parent:=ADrop.Parent;
       {Get Right}   {Right key of parent which will become parent}
       Right:=Parent.Right;
       if Right = nil then Exit;
       {Get Child}   {Child key of blank which will be child of demoted}
       Child:=Blank.Child; {Child may be nil}

       {Detach Parent}
       if not DetachEntry(Parent) then Exit;
       if not Detach(Parent) then Exit;
       Parent.Child:=nil;
       {Detach Blank}
       if not DetachBlank(Blank) then Exit;
       if not Detach(Blank) then Exit;
       if not UnlinkBlank(Blank) then Exit;
       if not DeleteBlank(Blank) then Exit;
       {Link Parent}
       Left.Right:=Parent;
       Parent.Left:=Left;
       {Link Last}
       Parent.Right:=Last;
       Last.Left:=Parent;

       {Set Parent} {Must be in this order}             //To Do //Testing2
       if not SetParent(ADrop,Right) then Exit;
       if not SetParentEntry(ADrop,Right) then Exit;
       {Set Parent}                                     //To Do //Testing2
       if not SetParent(Parent,Last.Parent) then Exit;
       if not SetParentEntry(Parent,Last.Parent) then Exit;

       //{Set Parent} {Must pass the demoted key for NTFS}
       //if not SetParent(Parent,Right) then Exit;      {if not SetParent(Last,Right) then Exit;}
       //if not SetParentEntry(Parent,Right) then Exit; {if not SetParentEntry(Last,Right) then Exit;}

       //To Do //Remove ?
       {Check Child}
       if Child <> nil then
        begin
         {Set Parent}
         if not SetParent(Child,Parent) then Exit;
         if not SetParentEntry(Child,Parent) then Exit;
        end;

       Result:=True;
      end;
    end;
  end;
end;

{==============================================================================}

function TLinkedBtree.Merge(AEntry,AMerge:TBtreeObject):Boolean;
{Merge the nodes of the supplied entries into one}
{Will demote the parent entry to the merged node}

{Merge with Right}
{Parent of left node will become part of child node (Between last of left and start of right)}
{Parent of right node will become parent of merged child}

{Merge with Left}
{Convert to merge with right by calling itself with swapped parameters}

{Merge can propogate all the way to root since parent is demoted}
var
 Left:TBtreeObject;
 Right:TBtreeObject;
 Start:TBtreeObject;
 Blank:TBtreeObject;
 Child:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AMerge = nil then Exit;
 {Check for Parent}   {Both entries must have a parent}
 if AEntry.Parent = nil then Exit;
 if AMerge.Parent = nil then Exit;
 {Check Left or Right}
 case Compare(AEntry,AMerge) of
  {Entry = Merge}
  btreeCompareEqual:begin
    {Merge with Equal}
    Exit;
   end;
  {Entry < Merge}
  btreeCompareLess:begin
    {Merge with Right}
    {Get Blank}   {Blank key of entry node which will be deleted}
    Blank:=GetBlank(AEntry);
    if Blank = nil then Exit;
    {Get Start}   {Start key of merge node which will be linked}
    Start:=GetStart(AMerge);
    if Start = nil then Exit;
    {Get Parent}  {Parent key of entry node which will be demoted}
    Parent:=AEntry.Parent;
    {Get Left}    {Last key of entry node which will be linked}
    Left:=Blank.Left;
    if Left = nil then Exit;
    {Get Right}   {Right key of parent which will become parent}
    Right:=Parent.Right;
    if Right = nil then Exit;
    {Get Child}   {Child key of blank which will be child of demoted}
    Child:=Blank.Child; {Child may be nil}

    {Detach Parent}
    if not DetachEntry(Parent) then Exit;
    if not Detach(Parent) then Exit;
    Parent.Child:=nil;
    {Detach Blank}
    if not DetachBlank(Blank) then Exit; {New 20/8/2009}
    if not Detach(Blank) then Exit;
    if not UnlinkBlank(Blank) then Exit;
    if not DeleteBlank(Blank) then Exit;
    {Link Parent}
    Left.Right:=Parent;
    Parent.Left:=Left;
    {Link Start}
    Parent.Right:=Start;
    Start.Left:=Parent;
    {Set Parent}
    if not SetParent(Start,Right) then Exit;
    if not SetParentEntry(Start,Right) then Exit; {New 22/12/2009}

    {Check Child}
    if Child <> nil then
     begin
      {Set Parent}
      if not SetParent(Child,Parent) then Exit;
      if not SetParentEntry(Child,Parent) then Exit; {New 22/12/2009}
     end;

    {Check Root} {Note: Detach above will set right as root if needed}
    if Right = FRoot then
     begin
      if Right.Blank then
       begin
        {Collapse Root}
        {Detach Right}
        if not DetachBlank(Right) then Exit; {New 20/8/2009}
        if not Detach(Right) then Exit;
        if not UnlinkBlank(Right) then Exit;
        if not DeleteBlank(Right) then Exit;
        {New Root}
        FRoot:=GetStart(Start);
        {Root Parent}
        if not SetParent(Start,nil) then Exit;
        if not SetParentEntry(Start,nil) then Exit; {New 22/12/2009}
       end;
     end;

    Result:=True;
   end;
  {Entry > Merge}
  btreeCompareGreater:begin
    {Merge with Left}
    Result:=Merge(AMerge,AEntry);
   end;
 end;
end;

{==============================================================================}

function TLinkedBtree.Borrow(AEntry,ABorrow:TBtreeObject):Boolean;
{Borrow an entry from the supplied node to balance the tree}
{Will demote the parent and premote the successor}

{Borrow from Right}
{Start key of right node will become the parent key of left node}
{Parent key of left node will become the last key of left node}

{Borrow from Left}
{Last key of left node will become the parent key of left node}
{Parent key of left node will become the start key of right node}

{Borrow does not propogate to parent since their is a promote and a demote}
{Borrow can occur in non leaf nodes due to merge of child nodes which demotes}

{Borrow with Child}
{Child of promoted key will go to blank key of left node}
{Child of blank key of left node will go to demoted key}
var
 Left:TBtreeObject;
 Start:TBtreeObject;
 Blank:TBtreeObject;
 Child:TBtreeObject;
 Parent:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if ABorrow = nil then Exit;
 {Check for Parent}   {Both entries must have a parent}
 if AEntry.Parent = nil then Exit;
 if ABorrow.Parent = nil then Exit;
 {Check for Child}    {Both entries must have or not have a child} //To Do //Is that correct ?
 if (AEntry.Child = nil) and (ABorrow.Child <> nil) then Exit;
 if (AEntry.Child <> nil) and (ABorrow.Child = nil) then Exit;
 {Check Left or Right}
 case Compare(AEntry,ABorrow) of
  {Entry = Borrow}
  btreeCompareEqual:begin
    {Borrow from Equal}
    Exit;
   end;
  {Entry < Borrow}
  btreeCompareLess:begin
    {Borrow from Right}
    {Get Blank}   {Blank key of entry node where last key will attach}
    Blank:=GetBlank(AEntry);
    if Blank = nil then Exit;
    {Get Start}   {Start key of borrow node which will become the parent key of entry node}
    Start:=GetStart(ABorrow);
    if Start = nil then Exit;
    {Get Parent}  {Parent key of entry node which will become the last key of entry node}
    Parent:=AEntry.Parent;
    {Get Child}   {Child key of promoted key which will become child of blank of entry node}
    Child:=Start.Child; {Child may be nil}
    if Child <> nil then Start.Child:=nil;

    {Swap Entry}
    if not SwapEntry(Parent,Start,False) then Exit; {New 22/12/2009}
    if not Swap(Parent,Start,False) then Exit;
    //To Do //Do we need to do PropogateSplit on one or both of the Nodes ? - see Remove
    {Detach Entry}
    if not DetachEntry(Parent) then Exit;
    if not Detach(Parent) then Exit;
    {Attach Entry}
    if not Attach(Start,Parent,Blank) then Exit;
    if not AttachEntry(Parent) then Exit;

    {Check Child}
    if Child <> nil then
     begin
      {Set Parent}
      if not SetParent(Blank.Child,Parent) then Exit;
      if not SetParentEntry(Blank.Child,Parent) then Exit; {New 22/12/2009}
      if not SetParent(Child,Blank) then Exit;
      if not SetParentEntry(Child,Blank) then Exit; {New 22/12/2009}
     end;

    Result:=True;
   end;
  {Entry > Borrow}
  btreeCompareGreater:begin
    {Borrow from Left}
    {Get Blank}   {Blank key of borrow node where last key is attached}
    Blank:=GetBlank(ABorrow);
    if Blank = nil then Exit;
    {Get Left}    {Last key of borrow node which will become the parent key of borrow node}
    Left:=Blank.Left;
    if Left = nil then Exit;
    {Get Start}   {Start key of entry node where start key will attach}
    Start:=GetStart(AEntry);
    if Start = nil then Exit;
    {Get Parent}  {Parent key of borrow node which will become the start key of entry node}
    Parent:=ABorrow.Parent;
    {Get Child}   {Child key of promoted key which will become child of blank of borrow node}
    Child:=Left.Child; {Child may be nil}
    if Child <> nil then Left.Child:=nil;

    {Swap Entry}
    if not SwapEntry(Parent,Left,True) then Exit; {New 22/12/2009}
    if not Swap(Parent,Left,True) then Exit;
    //To Do //Do we need to do PropogateSplit on one or both of the Nodes ? - see Remove
    {Detach Entry}
    if not DetachEntry(Parent) then Exit;
    if not Detach(Parent) then Exit;
    {Attach Entry}
    if not Attach(Start.Parent,Parent,Start) then Exit;
    if not AttachEntry(Parent) then Exit;

    {Check Child}
    if Child <> nil then
     begin
      {Set Parent}
      if not SetParent(Blank.Child,Parent) then Exit;
      if not SetParentEntry(Blank.Child,Parent) then Exit; {New 22/12/2009}
      if not SetParent(Child,Blank) then Exit;
      if not SetParentEntry(Child,Blank) then Exit; {New 22/12/2009}
     end;

    Result:=True;
   end;
 end;
end;

{==============================================================================}

function TLinkedBtree.Link(AEntry,ANext:TBtreeObject):Boolean;
{Link the object to Prev/Next in linked list}
{Note: If Next is nil then link as last}
var
 Prev:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if ANext = nil then
  begin
   Prev:=FLast;
   if Prev = nil then
    begin
     {Is First Object}
     AEntry.Prev:=nil;
     AEntry.Next:=nil;
     FFirst:=AEntry;
     FLast:=AEntry;
    end
   else
    begin
     {Not First Object}
     Prev.Next:=AEntry;
     AEntry.Prev:=Prev;
     AEntry.Next:=nil;
     FLast:=AEntry;
    end;
  end
 else
  begin
   Prev:=ANext.Prev;
   if Prev = nil then
    begin
     {Is First Object}
     ANext.Prev:=AEntry;
     AEntry.Next:=ANext;
     AEntry.Prev:=nil;
     FFirst:=AEntry;
    end
   else
    begin
     {Not First Object}
     Prev.Next:=AEntry;
     ANext.Prev:=AEntry;
     AEntry.Prev:=Prev;
     AEntry.Next:=ANext;
    end;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.Unlink(AEntry:TBtreeObject):Boolean;
{Unlink the object from Prev/Next in linked list}
var
 Prev,Next:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AEntry.Prev <> nil then
  begin
   {Not First Object}
   Prev:=AEntry.Prev;
   if AEntry.Next <> nil then
    begin
     {Not Last Object}
     Next:=AEntry.Next;
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end
   else
    begin
     {Is Last Object}
     Prev.Next:=nil;
     FLast:=Prev;
    end;
  end
 else
  begin
   {Is First Object}
   if AEntry.Next <> nil then
    begin
     {Not Last Object}
     Next:=AEntry.Next;
     Next.Prev:=nil;
     FFirst:=Next;
    end
   else
    begin
     {Is Last Object}
     FFirst:=nil;
     FLast:=nil;
    end;
  end;
 AEntry.Prev:=nil;
 AEntry.Next:=nil;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.LinkBlank(AEntry:TBtreeObject):Boolean;
{Link the object to Prev/Next in blank key list}
{Note: Always link as last}
var
 Prev:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 Prev:=FLastBlank;
 if Prev = nil then
  begin
   {Is First Object}
   AEntry.Prev:=nil;
   AEntry.Next:=nil;
   FFirstBlank:=AEntry;
   FLastBlank:=AEntry;
  end
 else
  begin
   {Not First Object}
   Prev.Next:=AEntry;
   AEntry.Prev:=Prev;
   AEntry.Next:=nil;
   FLastBlank:=AEntry;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.UnlinkBlank(AEntry:TBtreeObject):Boolean;
{Unlink the object from Prev/Next in blank key list}
var
 Prev,Next:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AEntry.Prev <> nil then
  begin
   {Not First Object}
   Prev:=AEntry.Prev;
   if AEntry.Next <> nil then
    begin
     {Not Last Object}
     Next:=AEntry.Next;
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end
   else
    begin
     {Is Last Object}
     Prev.Next:=nil;
     FLastBlank:=Prev;
    end;
  end
 else
  begin
   {Is First Object}
   if AEntry.Next <> nil then
    begin
     {Not Last Object}
     Next:=AEntry.Next;
     Next.Prev:=nil;
     FFirstBlank:=Next;
    end
   else
    begin
     {Is Last Object}
     FFirstBlank:=nil;
     FLastBlank:=nil;
    end;
  end;
 AEntry.Prev:=nil;
 AEntry.Next:=nil;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.Attach(AParent,AEntry,ARight:TBtreeObject):Boolean;
{Attach the object to Parent/Left/Right in btree}
{Note: If Right is nil then attach as last}
{Note: If Parent is nil then attach at root}
var
 Left:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AParent = nil then
  begin
   {Is Root Object}
   if ARight = nil then
    begin
     {Is Last Object}
     Left:=GetEnd(FRoot);
     if Left = nil then
      begin
       {Is First Object}
       AEntry.Parent:=nil;
       AEntry.Left:=nil;
       AEntry.Right:=nil;
       FRoot:=AEntry;
      end
     else
      begin
       {Not First Object}
       AEntry.Parent:=nil;
       Left.Right:=AEntry;
       AEntry.Left:=Left;
       AEntry.Right:=nil;
      end;
    end
   else
    begin
     {Not Last Object}
     Left:=ARight.Left;
     if Left = nil then
      begin
       {Is First Object}
       AEntry.Parent:=nil;
       AEntry.Left:=nil;
       AEntry.Right:=ARight;
       ARight.Left:=AEntry;
       FRoot:=AEntry;
      end
     else
      begin
       {Not First Object}
       AEntry.Parent:=nil;
       Left.Right:=AEntry;
       ARight.Left:=AEntry;
       AEntry.Left:=Left;
       AEntry.Right:=ARight;
      end;
    end;
  end
 else
  begin
   {Not Root Object}
   if ARight = nil then
    begin
     {Is Last Object}
     Left:=GetEnd(AParent.Child);
     if Left = nil then
      begin
       {Is First Object}
       AEntry.Parent:=AParent;
       AEntry.Left:=nil;
       AEntry.Right:=nil;
       AParent.Child:=AEntry;
      end
     else
      begin
       {Not First Object}
       AEntry.Parent:=AParent;
       Left.Right:=AEntry;
       AEntry.Left:=Left;
       AEntry.Right:=nil;
      end;
    end
   else
    begin
     {Not Last Object}
     Left:=ARight.Left;
     if Left = nil then
      begin
       {Is First Object}
       AEntry.Parent:=AParent;
       AEntry.Left:=nil;
       AEntry.Right:=ARight;
       ARight.Left:=AEntry;
       AParent.Child:=AEntry;
      end
     else
      begin
       {Not First Object}
       AEntry.Parent:=AParent;
       Left.Right:=AEntry;
       ARight.Left:=AEntry;
       AEntry.Left:=Left;
       AEntry.Right:=ARight;
      end;
    end;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.Detach(AEntry:TBtreeObject):Boolean;
{Detach the object from Parent/Left/Right in btree}
var
 Left,Right,Parent:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AEntry.Parent = nil then
  begin
   {Is Root Object}
   if AEntry.Left = nil then
    begin
     {Is First Object}
     if AEntry.Right = nil then
      begin
       {Is Last Object}
       FRoot:=nil;
      end
     else
      begin
       {Not Last Object}
       Right:=AEntry.Right;
       Right.Left:=nil;
       FRoot:=Right;
      end;
    end
   else
    begin
     {Not First Object}
     if AEntry.Right = nil then
      begin
       {Is Last Object}
       Left:=AEntry.Left;
       Left.Right:=nil;
      end
     else
      begin
       {Not Last Object}
       Left:=AEntry.Left;
       Right:=AEntry.Right;
       Left.Right:=Right;
       Right.Left:=Left;
      end;
    end;
  end
 else
  begin
   {Not Root Object}
   if AEntry.Left = nil then
    begin
     {Is First Object}
     if AEntry.Right = nil then
      begin
       {Is Last Object}
       Parent:=AEntry.Parent;
       Parent.Child:=nil;
      end
     else
      begin
       {Not Last Object}
       Parent:=AEntry.Parent;
       Right:=AEntry.Right;
       Right.Left:=nil;
       Parent.Child:=Right;
      end;
    end
   else
    begin
     {Not First Object}
     if AEntry.Right = nil then
      begin
       {Is Last Object}
       Left:=AEntry.Left;
       Left.Right:=nil;
      end
     else
      begin
       {Not Last Object}
       Left:=AEntry.Left;
       Right:=AEntry.Right;
       Left.Right:=Right;
       Right.Left:=Left;
      end;
    end;
  end;
 AEntry.Parent:=nil;
 {AEntry.Child:=nil;} {Do not detach from child to allow moving nodes (eg Merge/Promote)}
 AEntry.Left:=nil;
 AEntry.Right:=nil;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.PushNode(AEntry:TBtreeObject):Boolean;
{Called before a node is pushed following insert of an entry}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.SplitNode(AEntry:TBtreeObject):Boolean;
{Called before a node is split following insert of an entry}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.DropNode(AEntry,ADrop,ATarget:TBtreeObject;ALeft:Boolean):Boolean;
{Called before a node is dropped following removal of an entry}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.MergeNode(AEntry,AMerge:TBtreeObject):Boolean;
{Called before a node is merged following removal of an entry}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.BorrowEntry(AEntry,ABorrow:TBtreeObject):Boolean;
{Called before an entry is borrowed following removal of an entry}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.SwapEntry(AEntry,ASwap:TBtreeObject;ALeft:Boolean):Boolean;
{Called before an entry is swapped during a merge or borrow}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.SetParentEntry(AEntry,AParent:TBtreeObject):Boolean;
{Called after an entry is reparented during a push, split, merge, borrow or swap}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.CreateBlank:TBtreeObject;
{Create a blank key when a node is added (Split/Empty)}
begin
 {Virtual Base}
 Result:=TBtreeObject.Create;
 Result.Blank:=True;
end;

{==============================================================================}

function TLinkedBtree.DeleteBlank(ABlank:TBtreeObject):Boolean;
{Delete a blank key when a node is removed (Merge)}
begin
 {Virtual Base}
 Result:=False;
 if ABlank = nil then Exit;
 ABlank.Free;
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.AttachBlank(ABlank:TBtreeObject):Boolean;
{Called after a blank entry is attached to a node during split or merge}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.DetachBlank(ABlank:TBtreeObject):Boolean;
{Called before a blank entry is detached from a node during split or merge}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.AttachEntry(AEntry:TBtreeObject):Boolean;
{Called after a non blank entry is attached to a node during insert or remove}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.DetachEntry(AEntry:TBtreeObject):Boolean;
{Called before a non blank entry is detached from a node during insert or remove}
begin
 {Virtual Base}
 Result:=True;
end;

{==============================================================================}

function TLinkedBtree.RequirePush(AEntry:TBtreeObject):Boolean;
{Called after an entry is inserted to determine if a push is required}
{Entry is the key that was inserted}
{Or a key in a parent node of the node where the entry was inserted}

{Note: Only supported by descendant classes with non balanced nodes}
begin
 {Virtual Base}
 Result:=False;
end;

{==============================================================================}

function TLinkedBtree.RequireSplit(AEntry:TBtreeObject):Boolean;
{Called after an entry is inserted to determine if a split is required}
{Entry is the key that was inserted}
{Or a key in a parent node of the node where the entry was inserted}
begin
 {Virtual Base}
 Result:=(GetCount(AEntry) > (FOrder - 1));
end;

{==============================================================================}

function TLinkedBtree.RequireDrop(AEntry:TBtreeObject):Boolean;
{Called after an entry is removed to determine if a drop is required}
{Entry is the start key of the node where the entry was removed}
{Or a key in a parent node of the node where the entry was removed}

{Note: Only supported by descendant classes with non balanced nodes}
begin
 {Virtual Base}
 Result:=False;
end;

{==============================================================================}

function TLinkedBtree.RequireMerge(AEntry:TBtreeObject):Boolean;
{Called after an entry is removed to determine if a merge is required}
{Entry is the start key of the node where the entry was removed}
{Or a key in a parent node of the node where the entry was removed}
begin
 {Virtual Base}
 Result:=(GetCount(AEntry) < (FMedian - 1));
end;

{==============================================================================}

function TLinkedBtree.RequireBorrow(AEntry:TBtreeObject):Boolean;
{Called after an entry is removed to determine if a borrow is required}
{Entry is the start key of the node where the entry was removed}
{Or a key in a parent node of the node where the entry was removed}
begin
 {Virtual Base}
 Result:=(GetCount(AEntry) < (FMedian - 1));
end;

{==============================================================================}

function TLinkedBtree.Compare(AEntry1,AEntry2:TBtreeObject):Integer;
{Always returns greater than unless the second entry is a blank key}
{This means keys will end up in added order if no compare is provided}
begin
 {Virtual Base}
 Result:=btreeCompareEqual; {Equal}
 if AEntry1 = nil then Exit;
 if AEntry2 = nil then Exit;
 Result:=btreeCompareGreater; {Greater}
 if AEntry2.Blank then Result:=btreeCompareLess; {Less}
 {AEntry1.Blank is an error}
end;

{==============================================================================}

function TLinkedBtree.Add(AParent,AEntry:TBtreeObject):Boolean;
{Add an entry to the btree without doing the full insert}
{Entries must be added in btree order to obtain final order}
{Note: Both real and blank keys can be added}
{Note: No events are triggered by Add}
var
 Next:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit; {Parent may be nil}
 {Attach Entry}
 if Attach(AParent,AEntry,nil) then
  begin
   {Check Blank}
   if not AEntry.Blank then
    begin
     {Get Next}
     Next:=GetSuccessor(AEntry);
     while Next <> nil do
      begin
       if not Next.Blank then Break;
       Next:=GetSuccessor(Next);
      end;
     {Link Entry}
     if not Link(AEntry,Next) then Exit;
    end
   else
    begin
     {Link Blank}
     if not LinkBlank(AEntry) then Exit;
    end;
   Result:=True;
  end;
end;

{==============================================================================}

function TLinkedBtree.Insert(AEntry:TBtreeObject):Boolean;
{Insert an entry in the btree by finding its position}
{Rebalances the tree after inserting the new entry}
{Note: Blank keys cannot be inserted}
{Note: Entry must be created by the caller}
var
 Next:TBtreeObject;
 Right:TBtreeObject;
 Parent:TBtreeObject;
 Current:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AEntry.Blank then Exit;
 {Get Position}
 Right:=GetPosition(FRoot,AEntry);
 if Right = nil then Exit;
 {Attach Entry}
 if Attach(Right.Parent,AEntry,Right) then
  begin
   if not AttachEntry(AEntry) then Exit;
   {Get Next}
   Next:=Right;
   while Next <> nil do
    begin
     if not Next.Blank then Break;
     Next:=GetSuccessor(Next);
    end;
   {Link Entry}
   if not Link(AEntry,Next) then Exit;
   {Get Current}
   Current:=AEntry;
   {Get Parent}
   Parent:=Current.Parent; {Parent may be nil}
   {Check Push}
   if RequirePush(Current) then  {New 22/12/2009}
    begin
     {Push Node}
     if not PushNode(Current) then Exit; {New 22/12/2009}
     if not Push(Current) then Exit;
     {Check Parent}
     while Parent <> nil do
      begin
       {Get Current}
       Current:=Parent;
       {Get Parent}
       Parent:=Current.Parent; {Parent may be nil}
       {Check Push}
       if RequirePush(Current) then  {New 22/12/2009}
        begin
         {Push Node}
         if not PushNode(Current) then Exit; {New 22/12/2009}
         if not Push(Current) then Exit;
        end
       else
        begin
         {Check Split}
         if RequireSplit(Current) then
          begin
           {Split Node}
           if not SplitNode(Current) then Exit; {New 22/12/2009}
           if not Split(Current) then Exit;
          end;
        end;
      end;
    end
   else
    begin
     {Check Split}
     if RequireSplit(Current) then
      begin
       {Split Node}
       if not SplitNode(Current) then Exit; {New 22/12/2009}
       if not Split(Current) then Exit;
       {Check Parent}
       while Parent <> nil do
        begin
         {Get Current}
         Current:=Parent;
         {Get Parent}
         Parent:=Current.Parent; {Parent may be nil}
         {Check Push}
         if RequirePush(Current) then  {New 22/12/2009}
          begin
           {Push Node}
           if not PushNode(Current) then Exit; {New 22/12/2009}
           if not Push(Current) then Exit;
          end
         else
          begin
           {Check Split}
           if RequireSplit(Current) then
            begin
             {Split Node}
             if not SplitNode(Current) then Exit; {New 22/12/2009}
             if not Split(Current) then Exit;
            end;
          end;
        end;
      end;
    end;
   Result:=True;
  end;
end;

{==============================================================================}

function TLinkedBtree.Remove(AEntry:TBtreeObject):Boolean;
{Remove an entry from the btree by deleting it}
{Rebalances the tree after deleting the entry}
{Note: Blank keys cannot be removed}
{Note: Entry must be destroyed by the caller}
var
 Left:Boolean;
 Right:TBtreeObject;
 Blank:TBtreeObject;
 Parent:TBtreeObject;
 Current:TBtreeObject;
 Target:TBtreeObject;
 Neighbour:TBtreeObject;
begin
 {}
 Result:=False;
 if AEntry = nil then Exit;
 if AEntry.Blank then Exit;
 {Check Child}
 if AEntry.Child <> nil then
  begin
   if FSwapLeft then {New 12/12/2010}
    begin
     {Get Predecessor}
     Right:=GetPredecessor(AEntry); {No need to check for blank as GetPredecessor will never return blank if this entry has a child}
     if Right = nil then Exit;
     {Swap Entry}
     if not SwapEntry(AEntry,Right,True) then Exit; {New 22/12/2009}
     if not Swap(AEntry,Right,True) then Exit;
     {Propogate Split}
     if not PropogateSplit(Right) then Exit;
    end
   else
    begin
     {Get Successor}
     Right:=GetSuccessor(AEntry); {No need to check for blank as GetSuccessor will never return blank if this entry has a child}
     if Right = nil then Exit;
     {Swap Entry}
     if not SwapEntry(AEntry,Right,False) then Exit; {New 22/12/2009}
     if not Swap(AEntry,Right,False) then Exit;
     {Propogate Split}
     if not PropogateSplit(Right) then Exit;
    end;
  end;
 {Get Blank} {Since start may be deleted}
 Blank:=GetBlank(AEntry);
 {Unlink Entry}
 if not Unlink(AEntry) then Exit;
 {Detach Entry}
 if not DetachEntry(AEntry) then Exit;
 if not Detach(AEntry) then Exit;
 {Get Current}
 Current:=GetStart(Blank);
 if Current = nil then Exit;
 {Get Parent}
 Parent:=Current.Parent; {Parent may be nil}
 {Check Parent}
 if Parent <> nil then
  begin
   {Check Drop}
   if RequireDrop(Current) then  {New 7/1/2010}
    begin
     {Get Drop}
     Neighbour:=GetDrop(Current,Left);
     if Neighbour = nil then Exit;
     if Neighbour = Current then Neighbour:=nil; {Get Drop returns Entry to indicate no Left or Right neighbour}
     if Current.Child <> nil then Neighbour:=nil; {New 18/7/2011} {If child exists drop without neighbour}
     {Get Target} {New 21/7/2011}
     Target:=nil;
     if Neighbour <> nil then
      begin
       Target:=GetTarget(Neighbour,Left);
       if Target = nil then Exit;
      end;
     {Drop Nodes}
     if not DropNode(Current,Neighbour,Target,Left) then Exit; {New 7/1/2010}
     if not Drop(Current,Neighbour,Target,Left) then Exit;     {New 7/1/2010}
     {Check Target} {New 21/7/2011}
     if (Target <> nil) and (Target <> Neighbour) then
      begin
       {Propogate Split}
       if not PropogateSplit(Target) then Exit;
      end;
     {Check Neighbour}
     if Neighbour <> nil then
      begin
       {Propogate Split}
       if not PropogateSplit(Neighbour) then Exit;
       {Propogate Drop}
       if not PropogateDrop(Neighbour.Parent) then Exit;
      end;
    end
   else
    begin
     {Check Borrow or Merge}
     if RequireBorrow(Current) or RequireMerge(Current) then {Modified 8/1/2010}
      begin
       {Get Borrow}
       Neighbour:=GetBorrow(Current);
       if Neighbour <> nil then
        begin
         {Borrow Entry}
         if not BorrowEntry(Current,Neighbour) then Exit; {New 22/12/2009}
         if not Borrow(Current,Neighbour) then Exit;
         {Borrow does not propogate}
        end
       else
        begin
         {Get Merge}
         Neighbour:=GetMerge(Current);
         if Neighbour = nil then Exit;
         {Merge Nodes}
         if not MergeNode(Current,Neighbour) then Exit; {New 22/12/2009}
         if not Merge(Current,Neighbour) then Exit;
         {Get Current}
         Current:=GetStart(Parent);
         if Current = nil then Exit;
         {Get Parent}
         Parent:=Current.Parent; {Parent may be nil}
         {Check Parent}
         while Parent <> nil do
          begin
           {Check Borrow or Merge}
           if RequireBorrow(Current) or RequireMerge(Current) then {Modified 8/1/2010}
            begin
             {Get Borrow}
             Neighbour:=GetBorrow(Current);
             if Neighbour <> nil then
              begin
               {Borrow Entry}
               if not BorrowEntry(Current,Neighbour) then Exit; {New 22/12/2009}
               if not Borrow(Current,Neighbour) then Exit;
               Break; {Borrow does not propogate}
              end
             else
              begin
               {Get Merge}
               Neighbour:=GetMerge(Current);
               if Neighbour = nil then Exit;
               {Merge Nodes}
               if not MergeNode(Current,Neighbour) then Exit; {New 22/12/2009}
               if not Merge(Current,Neighbour) then Exit;
              end;
            end;
           {Get Current}
           Current:=GetStart(Parent);
           if Current = nil then Exit;
           {Get Parent}
           Parent:=Current.Parent; {Parent may be nil}
          end;
        end;
      end;
    end;
  end;
 Result:=True;
end;

{==============================================================================}

procedure TLinkedBtree.Clear;
{Removes all entries from the btree}
begin
 {}
 FRoot:=nil;
 FFirst:=nil;
 FLast:=nil;
 FFirstBlank:=nil;
 FLastBlank:=nil;
end;

{==============================================================================}

procedure TLinkedBtree.Empty;
{Removes all entries from the btree and adds a blank root key}
var
 Blank:TBtreeObject;
begin
 {Clear Tree}
 Clear;
 {Root Blank}
 Blank:=CreateBlank;
 LinkBlank(Blank);
 Attach(nil,Blank,nil);
 AttachBlank(Blank);
end;

{==============================================================================}

procedure TLinkedBtree.Rebuild;
{Empties the btree and rebuilds from the linked list}
var
 Next:TBtreeObject;
 Current:TBtreeObject;
begin
 {}
 try
  {Clear Blank Keys}
  Next:=FFirstBlank;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=Current.Next;
    Current.Free;
   end;
  {Get First Object}
  Next:=FFirst;
  {Empty Tree}
  Empty;
  {Insert Objects}
  while Next <> nil do
   begin
    Current:=Next;
    Next:=Current.Next;
    Current.Clear;
    Insert(Current);
   end;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{TLinkedObjBtree}
constructor TLinkedObjBtree.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}

destructor TLinkedObjBtree.Destroy;
begin
 {}
 ClearBtree;
 inherited Destroy;
end;

{==============================================================================}

procedure TLinkedObjBtree.ClearBtree;
{Removes and frees all entries in the btree}
var
 Next:TBtreeObject;
 Current:TBtreeObject;
begin
 {}
 try
  {Clear Objects}
  Next:=FFirst;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=Current.Next;
    Current.Free;
   end;
  {Clear Blank Keys}
  Next:=FFirstBlank;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=Current.Next;
    Current.Free;
   end;
  {Reset Defaults}
  Clear;
 except
  {}
 end;
end;

{==============================================================================}

procedure TLinkedObjBtree.EmptyBtree;
{Removes and frees all entries in the btree and adds a blank root key}
var
 Blank:TBtreeObject;
begin
 {Clear Tree}
 ClearBtree;
 {Root Blank}
 Blank:=CreateBlank;
 LinkBlank(Blank);
 Attach(nil,Blank,nil);
 AttachBlank(Blank);
end;

{==============================================================================}
{==============================================================================}
{THashListObject}
procedure THashListObject.SetKeyHash(AKeyHash:LongWord);
var
 AKeyList:THashLinkedList;
begin
 {}
 if FKeyList = nil then
  begin
   FKeyHash:=AKeyHash;
  end
 else
  begin
   if AKeyHash = FKeyHash then Exit;
   AKeyList:=FKeyList;
   if AKeyList.KeyUnlink(Self) then
    begin
     FKeyHash:=AKeyHash;
     AKeyList.KeyLink(Self);
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{THashLinkedList}
constructor THashLinkedList.Create(AKeyBits:Byte);
begin
 {}
 inherited Create;
 FKeyBits:=AKeyBits;
 if FKeyBits = 0 then FKeyBits:=listHashBits;
 if FKeyBits < keyHashMinBits then FKeyBits:=keyHashMinBits;
 if FKeyBits > keyHashMaxBits then FKeyBits:=keyHashMaxBits;
 FKeyMask:=keyHashMasks[FKeyBits];
 FKeyShift:=PtrShift; {Key Shift value (1 shl FKeyShift = SizeOf(Pointer))}
 FKeyBuckets:=AllocMem((keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}

destructor THashLinkedList.Destroy;
begin
 {}
 inherited Destroy;
 FreeMem(FKeyBuckets); {Must be after inherited to allow for ClearList/Clear}
end;

{==============================================================================}

function THashLinkedList.KeyLink(AValue:THashListObject):Boolean;
{Link AValue to Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 FirstKey:THashListObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 AValue.KeyList:=Self;
 {Get Offset}
 Offset:=(AValue.FKeyHash and FKeyMask) shl FKeyShift;
 {Get First Key}
 FirstKey:=THashListObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
 if FirstKey = nil then
  begin
   {Is First Object}
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=nil;
   THashListObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
  end
 else
  begin
   {Not First Object}
   FirstKey.KeyPrev:=AValue;
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=FirstKey;
   THashListObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
  end;
 Result:=True;
end;

{==============================================================================}

function THashLinkedList.KeyUnlink(AValue:THashListObject):Boolean;
{Unlink AValue from Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 PrevKey,NextKey:THashListObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if AValue.KeyPrev <> nil then
  begin
   {Not First Object}
   PrevKey:=AValue.KeyPrev;
   if AValue.KeyNext <> nil then
    begin
     {Not Last Object}
     NextKey:=AValue.KeyNext;
     PrevKey.KeyNext:=NextKey;
     NextKey.KeyPrev:=PrevKey;
    end
   else
    begin
     {Is Last Object}
     PrevKey.KeyNext:=nil;
    end;
  end
 else
  begin
   {Get Offset}
   Offset:=(AValue.FKeyHash and FKeyMask) shl FKeyShift;
   {Is First Object}
   if AValue.KeyNext <> nil then
    begin
     {Not Last Object}
     NextKey:=AValue.KeyNext;
     NextKey.KeyPrev:=nil;
     THashListObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=NextKey;
    end
   else
    begin
     {Is Last Object}
     THashListObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=nil;
    end;
  end;
 AValue.KeyList:=nil;
 AValue.KeyPrev:=nil;
 AValue.KeyNext:=nil;
 Result:=True;
end;

{==============================================================================}

function THashLinkedList.KeyFirst(AKeyHash:LongWord):THashListObject;
var
 Offset:LongWord;
begin
 {}
 Result:=nil;
 if FKeyBuckets = nil then Exit;
 {Get Offset}
 Offset:=(AKeyHash and FKeyMask) shl FKeyShift;
 {Get First Key}
 Result:=THashListObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
end;

{==============================================================================}

function THashLinkedList.Add(AValue:TListObject):Boolean;
{Add AValue to List and link with Siblings}
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if Link(AValue) then
  begin
   Inc(FCount);
   if KeyLink(THashListObject(AValue)) then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function THashLinkedList.Remove(AValue:TListObject):Boolean;
{Unlink AValue from Siblings and Remove from List}
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if Unlink(AValue) then
  begin
   Dec(FCount);
   if KeyUnlink(THashListObject(AValue)) then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function THashLinkedList.Insert(APrev,AValue:TListObject):Boolean;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if LinkEx(APrev,AValue) then
  begin
   Inc(FCount);
   if KeyLink(THashListObject(AValue)) then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

procedure THashLinkedList.Clear;
begin
 {}
 inherited Clear;
 ZeroMemory(FKeyBuckets,(keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}
{==============================================================================}
{THashLinkedObjList}
constructor THashLinkedObjList.Create(AKeyBits:Byte);
begin
 {}
 inherited Create(AKeyBits);
end;

{==============================================================================}

destructor THashLinkedObjList.Destroy;
begin
 {}
 ClearList;
 inherited Destroy;
end;

{==============================================================================}

procedure THashLinkedObjList.ClearList;
var
 Next:TListObject;
 Current:TListObject;
begin
 {}
 try
  {Clear Objects}
  Next:=FFirst;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=Current.Next;
    Current.Free;
   end;
  {Reset Defaults}
  Clear;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{THashTreeObject}
constructor THashTreeObject.Create(AKeyBits:Byte);
begin
 {}
 inherited Create;
 FKeyBits:=AKeyBits;
 if FKeyBits = 0 then FKeyBits:=treeHashBits;
 if FKeyBits < keyHashMinBits then FKeyBits:=keyHashMinBits;
 if FKeyBits > keyHashMaxBits then FKeyBits:=keyHashMaxBits;
 FKeyMask:=keyHashMasks[FKeyBits];
 FKeyShift:=PtrShift; {Key Shift value (1 shl FKeyShift = SizeOf(Pointer))}
 FKeyBuckets:=AllocMem((keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}

destructor THashTreeObject.Destroy;
begin
 {}
 inherited Destroy;
 FreeMem(FKeyBuckets); {Must be after inherited to allow for ClearList/Clear}
end;

{==============================================================================}

procedure THashTreeObject.SetKeyHash(AKeyHash:LongWord);
var
 AParent:THashTreeObject;
 AKeyTree:THashLinkedTree;
begin
 {}
 if FKeyTree = nil then
  begin
   FKeyHash:=AKeyHash;
  end
 else
  begin
   if AKeyHash = FKeyHash then Exit;
   AParent:=THashTreeObject(FParent);
   AKeyTree:=FKeyTree;
   if AKeyTree.KeyUnlink(Self,AParent) then
    begin
     FKeyHash:=AKeyHash;
     AKeyTree.KeyLink(Self,AParent);
    end;
  end;
end;

{==============================================================================}

function THashTreeObject.KeyLink(AValue:THashTreeObject):Boolean;
{Link AValue to Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 FirstKey:THashTreeObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 AValue.KeyTree:=FKeyTree;
 {Get Offset}
 Offset:=(AValue.FKeyHash and FKeyMask) shl FKeyShift;
 {Get First Key}
 FirstKey:=THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
 if FirstKey = nil then
  begin
   {Is First Object}
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=nil;
   THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
  end
 else
  begin
   {Not First Object}
   FirstKey.KeyPrev:=AValue;
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=FirstKey;
   THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
  end;
 Result:=True;
end;

{==============================================================================}

function THashTreeObject.KeyUnlink(AValue:THashTreeObject):Boolean;
{Unlink AValue from Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 PrevKey,NextKey:THashTreeObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if AValue.KeyPrev <> nil then
  begin
   {Not First Object}
   PrevKey:=AValue.KeyPrev;
   if AValue.KeyNext <> nil then
    begin
     {Not Last Object}
     NextKey:=AValue.KeyNext;
     PrevKey.KeyNext:=NextKey;
     NextKey.KeyPrev:=PrevKey;
    end
   else
    begin
     {Is Last Object}
     PrevKey.KeyNext:=nil;
    end;
  end
 else
  begin
   {Get Offset}
   Offset:=(AValue.FKeyHash and FKeyMask) shl FKeyShift;
   {Is First Object}
   if AValue.KeyNext <> nil then
    begin
     {Not Last Object}
     NextKey:=AValue.KeyNext;
     NextKey.KeyPrev:=nil;
     THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=NextKey;
    end
   else
    begin
     {Is Last Object}
     THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=nil;
    end;
  end;
 AValue.KeyTree:=nil;
 AValue.KeyPrev:=nil;
 AValue.KeyNext:=nil;
 Result:=True;
end;

{==============================================================================}

function THashTreeObject.KeyFirst(AKeyHash:LongWord):THashTreeObject;
var
 Offset:LongWord;
begin
 {}
 Result:=nil;
 if FKeyBuckets = nil then Exit;
 {Get Offset}
 Offset:=(AKeyHash and FKeyMask) shl FKeyShift;
 {Get First Key}
 Result:=THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
end;

{==============================================================================}
{==============================================================================}
{THashLinkedTree}
constructor THashLinkedTree.Create(AKeyBits:Byte);
begin
 {}
 inherited Create;
 FKeyBits:=AKeyBits;
 if FKeyBits = 0 then FKeyBits:=treeHashBits;
 if FKeyBits < keyHashMinBits then FKeyBits:=keyHashMinBits;
 if FKeyBits > keyHashMaxBits then FKeyBits:=keyHashMaxBits;
 FKeyMask:=keyHashMasks[FKeyBits];
 FKeyShift:=PtrShift; {Key Shift value (1 shl FKeyShift = SizeOf(Pointer))}
 FKeyBuckets:=AllocMem((keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}

destructor THashLinkedTree.Destroy;
begin
 {}
 inherited Destroy;
 FreeMem(FKeyBuckets); {Must be after inherited to allow for ClearList/Clear}
end;

{==============================================================================}

function THashLinkedTree.KeyLink(AValue,AParent:THashTreeObject):Boolean;
{Link AValue to Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 FirstKey:THashTreeObject;
begin
 {}
 if AParent <> nil then
  begin
   Result:=AParent.KeyLink(AValue);
  end
 else
  begin
   Result:=False;
   if AValue = nil then Exit;
   AValue.KeyTree:=Self;
   {Get Offset}
   Offset:=(AValue.FKeyHash and FKeyMask) shl FKeyShift;
   {Get First Key}
   FirstKey:=THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
   if FirstKey = nil then
    begin
     {Is First Object}
     AValue.KeyPrev:=nil;
     AValue.KeyNext:=nil;
     THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
    end
   else
    begin
     {Not First Object}
     FirstKey.KeyPrev:=AValue;
     AValue.KeyPrev:=nil;
     AValue.KeyNext:=FirstKey;
     THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
    end;
   Result:=True;
  end;
end;

{==============================================================================}

function THashLinkedTree.KeyUnlink(AValue,AParent:THashTreeObject):Boolean;
{Unlink AValue from Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 PrevKey,NextKey:THashTreeObject;
begin
 {}
 if AParent <> nil then
  begin
   Result:=AParent.KeyUnlink(AValue);
  end
 else
  begin
   Result:=False;
   if AValue = nil then Exit;
   if AValue.KeyPrev <> nil then
    begin
     {Not First Object}
     PrevKey:=AValue.KeyPrev;
     if AValue.KeyNext <> nil then
      begin
       {Not Last Object}
       NextKey:=AValue.KeyNext;
       PrevKey.KeyNext:=NextKey;
       NextKey.KeyPrev:=PrevKey;
      end
     else
      begin
       {Is Last Object}
       PrevKey.KeyNext:=nil;
      end;
    end
   else
    begin
     {Get Offset}
     Offset:=(AValue.FKeyHash and FKeyMask) shl FKeyShift;
     {Is First Object}
     if AValue.KeyNext <> nil then
      begin
       {Not Last Object}
       NextKey:=AValue.KeyNext;
       NextKey.KeyPrev:=nil;
       THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=NextKey;
      end
     else
      begin
       {Is Last Object}
       THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=nil;
      end;
    end;
   AValue.KeyTree:=nil;
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=nil;
   Result:=True;
  end;
end;

{==============================================================================}

function THashLinkedTree.KeyFirst(AParent:THashTreeObject;AKeyHash:LongWord):THashTreeObject;
var
 Offset:LongWord;
begin
 {}
 if AParent <> nil then
  begin
   Result:=AParent.KeyFirst(AKeyHash);
  end
 else
  begin
   Result:=nil;
   if FKeyBuckets = nil then Exit;
   {Get Offset}
   Offset:=(AKeyHash and FKeyMask) shl FKeyShift;
   {Get First Key}
   Result:=THashTreeObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
  end;
end;

{==============================================================================}

function THashLinkedTree.Add(AValue,AParent:TTreeObject):Boolean;
{Add AValue to List and Link with Parent and Siblings}
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if Link(AValue,AParent) then
  begin
   Inc(FCount);
   if KeyLink(THashTreeObject(AValue),THashTreeObject(AParent)) then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function THashLinkedTree.Remove(AValue:TTreeObject):Boolean;
{Unlink AValue from Parent and Siblings, Remove from List}
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if Unlink(AValue) then
  begin
   Dec(FCount);
   if KeyUnlink(THashTreeObject(AValue),THashTreeObject(AValue.Parent)) then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function THashLinkedTree.Insert(APrev,AValue,AParent:TTreeObject):Boolean;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if LinkEx(APrev,AValue,AParent) then
  begin
   Inc(FCount);
   if KeyLink(THashTreeObject(AValue),THashTreeObject(AParent)) then
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

procedure THashLinkedTree.Move(AValue,AParent:TTreeObject);
begin
 {}
 if AValue = nil then Exit;
 if Unlink(AValue) then
  begin
   if KeyUnlink(THashTreeObject(AValue),THashTreeObject(AValue.Parent)) then
    begin
     if Link(AValue,AParent) then
      begin
       KeyLink(THashTreeObject(AValue),THashTreeObject(AParent));
      end;
    end;
  end;
end;

{==============================================================================}

procedure THashLinkedTree.Clear;
begin
 {}
 inherited Clear;
 ZeroMemory(FKeyBuckets,(keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}
{==============================================================================}
{THashLinkedObjTree}
constructor THashLinkedObjTree.Create(AKeyBits:Byte);
begin
 {}
 inherited Create(AKeyBits);
end;

{==============================================================================}

destructor THashLinkedObjTree.Destroy;
begin
 {}
 ClearList;
 inherited Destroy;
end;

{==============================================================================}

procedure THashLinkedObjTree.ClearListItems(AParent:THashTreeObject);
var
 Next:TTreeObject;
 Current:TTreeObject;
begin
 {}
 if AParent = nil then Exit;
 {Clear Objects}
 Next:=AParent.FirstChild;
 while Next <> nil do
  begin
   Current:=Next;
   Next:=TTreeObject(Current.Next);
   ClearListItems(THashTreeObject(Current));
   Current.Free;
  end;
end;

{==============================================================================}

procedure THashLinkedObjTree.ClearList;
var
 Next:TTreeObject;
 Current:TTreeObject;
begin
 {}
 try
  {Clear Objects}
  Next:=FFirst;
  while Next <> nil do
   begin
    Current:=Next;
    Next:=TTreeObject(Current.Next);
    ClearListItems(THashTreeObject(Current));
    Current.Free;
   end;
  {Reset Defaults}
  Clear;
 except
  {}
 end;
end;

{==============================================================================}
{==============================================================================}
{TStringObject}
procedure TStringObject.SetValue(const AValue:String);
begin
 {}
 FValue:=AValue;
 FHash:=GenerateNameHash(FValue,stringHashSize);
end;

{==============================================================================}
{==============================================================================}
{TLinkedStringList}
constructor TLinkedStringList.Create;
begin
 {}
 inherited Create;
 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TLinkedStringList.Destroy;
begin
 {}
 FOnChange:=nil;
 FOnChanging:=nil;
 inherited Destroy;
 ClearList;
 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

function TLinkedStringList.GetItem(AIndex:Integer):TStringObject;
var
 Index:Integer;
 Next:TStringObject;
begin
 {}
 Result:=nil;
 Index:=0;
 Next:=TStringObject(FFirst);
 while Next <> nil do
  begin
   if Index = AIndex then
    begin
     Result:=Next;
     Exit;
    end;
   Inc(Index);
   Next:=TStringObject(Next.Next);
  end;
end;

{==============================================================================}

function TLinkedStringList.Link(AValue:TStringObject):Boolean;
{Link AValue to Prev,Next Siblings and Adjust First/Last}
var
 Prev:TStringObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 Prev:=FLast;
 if Prev = nil then
  begin
   {Is First Object}
   AValue.Prev:=nil;
   AValue.Next:=nil;
   FFirst:=AValue;
   FLast:=AValue;
  end
 else
  begin
   {Not First Object}
   Prev.Next:=AValue;
   AValue.Prev:=Prev;
   AValue.Next:=nil;
   FLast:=AValue;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedStringList.LinkEx(APrev,AValue:TStringObject):Boolean;
{Link AValue after APrev Sibling and Adjust First/Last/Prev/Next}
{If APrev is nil then Link as first value in list}
var
 Next:TStringObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if APrev = nil then
  begin
   if FLast <> nil then
    begin
     {Not First Object}
     Next:=FFirst;
     FFirst:=AValue;
     AValue.Prev:=nil;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is First Object}
     AValue.Prev:=nil;
     AValue.Next:=nil;
     FFirst:=AValue;
     FLast:=AValue;
    end;
  end
 else
  begin
   if APrev.Next <> nil then
    begin
     {Not Last Object}
     Next:=TStringObject(APrev.Next);
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is Last Object}
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=nil;
     FLast:=AValue;
    end;
  end;
 Result:=True;
end;

{==============================================================================}

function TLinkedStringList.Unlink(AValue:TStringObject):Boolean;
{Unlink AValue from Prev,Next Siblings and Adjust First/Last}
var
 Prev,Next:TStringObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if AValue.Prev <> nil then
  begin
   {Not First Object}
   Prev:=TStringObject(AValue.Prev);
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=TStringObject(AValue.Next);
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end
   else
    begin
     {Is Last Object}
     Prev.Next:=nil;
     FLast:=Prev;
    end;
  end
 else
  begin
   {Is First Object}
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=TStringObject(AValue.Next);
     Next.Prev:=nil;
     FFirst:=Next;
    end
   else
    begin
     {Is Last Object}
     FFirst:=nil;
     FLast:=nil;
    end;
  end;
 AValue.Prev:=nil;
 AValue.Next:=nil;
 Result:=True;
end;

{==============================================================================}

procedure TLinkedStringList.ClearList;
var
 Next:TStringObject;
 Current:TStringObject;
begin
 {}
 try
  {Clear Objects}
  Next:=TStringObject(FFirst);
  while Next <> nil do
   begin
    Current:=Next;
    Next:=TStringObject(Current.Next);
    Current.Free;
   end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TLinkedStringList.Changed;
begin
 {}
 if not(FUpdating) and Assigned(FOnChange) then FOnChange(Self);
end;

{==============================================================================}

procedure TLinkedStringList.Changing;
begin
 {}
 if not(FUpdating) and Assigned(FOnChanging) then FOnChanging(Self);
end;

{==============================================================================}

function TLinkedStringList.Get(Index:Integer):String;
var
 Item:TStringObject;
begin
 {}
 Result:='';
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Item:=GetItem(Index);
 if Item = nil then Exit;
 Result:=Item.Value;
end;

{==============================================================================}

function TLinkedStringList.GetCount:Integer;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TLinkedStringList.GetObject(Index:Integer):TObject;
var
 Item:TStringObject;
begin
 {}
 Result:=nil;
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Item:=GetItem(Index);
 if Item = nil then Exit;
 Result:=Item.Data;
end;

{==============================================================================}

procedure TLinkedStringList.Put(Index:Integer;const S:String);
var
 Item:TStringObject;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Item:=GetItem(Index);
 if Item = nil then Exit;
 Item.Value:=S;
 Changed;
end;

{==============================================================================}

procedure TLinkedStringList.PutObject(Index:Integer;AObject:TObject);
var
 Item:TStringObject;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Item:=GetItem(Index);
 if Item = nil then Exit;
 Item.Data:=AObject;
 Changed;
end;

{==============================================================================}

procedure TLinkedStringList.SetUpdateState(Updating:Boolean);
begin
 {}
 if Updating then
  begin
   Changing;
   FUpdating:=Updating;
  end
 else
  begin
   FUpdating:=Updating;
   Changed;
  end;
end;

{==============================================================================}

function TLinkedStringList.Add(const S:String):Integer;
var
 Item:TStringObject;
begin
 {}
 Result:=-1;
 Changing;
 Item:=TStringObject.Create;
 Item.Value:=S;
 if Link(Item) then
  begin
   Result:=FCount;
   Inc(FCount);
   Changed;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}

procedure TLinkedStringList.Clear;
begin
 {}
 if FCount <> 0 then
  begin
   Changing;
   ClearList;
   FCount:=0;
   FFirst:=nil;
   FLast:=nil;
   Changed;
  end;
end;

{==============================================================================}

procedure TLinkedStringList.Delete(Index:Integer);
var
 Item:TStringObject;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Item:=GetItem(Index);
 if Item = nil then Exit;
 if Unlink(Item) then
  begin
   Item.Free;
   Dec(FCount);
   Changed;
  end;
end;

{==============================================================================}

procedure TLinkedStringList.Exchange(Index1,Index2:Integer);
var
 WorkValue:String;
 WorkData:TObject;
 Item1:TStringObject;
 Item2:TStringObject;
begin
 {}
 if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError,Index1);
 if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError,Index2);
 Changing;
 Item1:=GetItem(Index1);
 if Item1 = nil then Exit;
 Item2:=GetItem(Index2);
 if Item2 = nil then Exit;
 WorkValue:=Item1.Value;
 WorkData:=Item1.Data;
 Item1.Value:=Item2.Value;
 Item1.Data:=Item2.Data;
 Item2.Value:=WorkValue;
 Item2.Data:=WorkData;
 Changed;
end;

{==============================================================================}

function TLinkedStringList.IndexOf(const S:String):Integer;
{Uses Counted Index starting from First String Object}
var
 Index:Integer;
 Hash:LongWord;
 Item:TStringObject;
begin
 {}
 Result:=-1;
 Index:=0;
 Hash:=GenerateNameHash(S,stringHashSize);
 Item:=TStringObject(FFirst);
 while Item <> nil do
  begin
   if Item.Hash = Hash then
    begin
     if Uppercase(Item.Value) = Uppercase(S) then
      begin
       Result:=Index;
       Exit;
      end;
    end;
   Inc(Index);
   Item:=TStringObject(Item.Next);
  end;
end;

{==============================================================================}

procedure TLinkedStringList.Insert(Index:Integer;const S:String);
var
 Item:TStringObject;
 Prev:TStringObject;
begin
 {}
 if (Index < 0) or (Index > FCount) then Error(SListIndexError,Index);
 Changing;
 Prev:=GetItem(Index - 1); {Prev may be nil}
 Item:=TStringObject.Create;
 Item.Value:=S;
 if LinkEx(Prev,Item) then
  begin
   Inc(FCount);
   Changed;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}
{==============================================================================}
{TStringBlock}
destructor TStringBlock.Destroy;
begin
 {}
 if Data <> nil then FreeMem(Data);
 Data:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TLinkedStringListEx}
constructor TLinkedStringListEx.Create;
begin
 {}
 inherited Create;
 FRecent:=nil;
 FBlocks:=TLinkedObjList.Create;
 FCapacity:=0;
end;

{==============================================================================}

destructor TLinkedStringListEx.Destroy;
begin
 {}
 inherited Destroy;
 FCapacity:=0;
 FRecent:=nil;
 FBlocks.Free;
end;

{==============================================================================}

function TLinkedStringListEx.GetBlock(Index:Integer):TStringBlock;
var
 Block:TStringBlock;
begin
 {}
 Result:=nil;
 try
  {Check Index}
  if Index = FCount then Exit;
  {Check Recent}
  if FRecent <> nil then
   begin
    if (LongWord(Index) >= FRecent.Start) and (LongWord(Index) < (FRecent.Start + FRecent.Count)) then
     begin
      Result:=FRecent;
      Exit;
     end;
   end;
  {Get Block}
  Block:=TStringBlock(FBlocks.First);
  while Block <> nil do
   begin
    if (LongWord(Index) >= Block.Start) and (LongWord(Index) < (Block.Start + Block.Count)) then
     begin
      FRecent:=Block;
      Result:=Block;
      Exit;
     end;
    Block:=TStringBlock(Block.Next);
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.AddBlock(Block:TStringBlock;Index:Integer):TStringBlock;
{Index = Starting Index of Block to be Added}
{Block = Current Block containing Index or nil}
var
 Length:LongWord;
 Offset:LongWord;
 Last:TStringBlock;
begin
 {}
 Result:=nil;
 try
  if Block = nil then
   begin
    {Get Last}
    Last:=TStringBlock(FBlocks.Last);
    if (Last <> nil) and (Last.Count < Last.Capacity) then
     begin
      {Return Block}
      Result:=Last;
      
      {Set Recent}
      FRecent:=Result;
     end
    else
     begin
      {Add Block}
      Result:=TStringBlock.Create;
      Result.Data:=GetMem(stringListDelta);
      Result.Size:=stringListDelta;
      Result.Start:=LongWord(Index);
      Result.Count:=0;
      Result.Capacity:=stringListDelta shr PtrShift; {Divide by SizeOf(Pointer)}
      FBlocks.Add(Result);
      Inc(FCapacity,Result.Capacity);
      
      {Set Recent}
      FRecent:=Result;
      
      {Update Blocks}
      UpdateBlocks(Result);
     end;
   end
  else
   begin
    {Get Length}
    Length:=(Block.Count shr 1); {Divide by 2}
    
    {Get Offset}
    Offset:=(Block.Count - Length) shl PtrShift; {Multiply by SizeOf(Pointer)}
    
    {Update Block}
    Dec(Block.Count,Length);
    
    {Add Block}
    Result:=TStringBlock.Create;
    Result.Data:=GetMem(stringListDelta);
    Result.Size:=stringListDelta;
    Result.Start:=0; {Set by Update Blocks}
    Result.Count:=Length;
    Result.Capacity:=stringListDelta shr PtrShift; {Divide by SizeOf(Pointer)}
    FBlocks.Insert(Block,Result);
    Inc(FCapacity,Result.Capacity);
    
    {Copy Data}
    System.Move(Pointer(PtrUInt(Block.Data) + Offset)^,Pointer(PtrUInt(Result.Data))^,Length shl PtrShift);
    
    {Set Recent}
    FRecent:=Result;
    
    {Update Blocks}
    UpdateBlocks(Block);
    
    {Check Index}
    if LongWord(Index) < (Block.Start + Block.Count) then Result:=Block;
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.DeleteBlock(Block:TStringBlock):Boolean;
var
 Prev:TStringBlock;
begin
 {}
 Result:=False;
 try
  if Block = nil then Exit;

  {Get Previous}
  Prev:=TStringBlock(Block.Prev);
  
  {Check Recent}
  if FRecent = Block then FRecent:=nil;
  
  {Delete Block}
  Dec(FCapacity,Block.Capacity);
  FBlocks.Remove(Block);
  Block.Free;
  
  {Update Blocks}
  Result:=UpdateBlocks(Prev);
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.UpdateBlocks(Block:TStringBlock):Boolean;
var
 Start:LongWord;
 Next:TStringBlock;
begin
 {}
 Result:=False;
 try
  {if Block = nil then Exit;} {Block may be nil}

  {Get Start}
  Start:=0;
  if Block <> nil then Start:=Block.Start;
  {Get Next}
  Next:=Block;
  if Next = nil then Next:=TStringBlock(FBlocks.First);
  {Update Blocks}
  while Next <> nil do
   begin
    Next.Start:=Start;
    Inc(Start,Next.Count);
    Next:=TStringBlock(Next.Next);
   end;
  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.GetItem(Block:TStringBlock;Index:Integer):TStringObjectEx;
var
 Offset:LongWord;
begin
 {}
 Result:=nil;
 try
  if Block = nil then Exit;

  {Get Offset}
  Offset:=(LongWord(Index) - Block.Start) shl PtrShift; {Multiply by SizeOf(Pointer)}
  {Get Item}
  Result:=TStringObjectEx(Pointer(PtrUInt(Block.Data) + Offset)^);
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.AddItem(Block:TStringBlock;Index:Integer;Item:TStringObjectEx):Boolean;
var
 Offset:LongWord;
 Current:TStringBlock;
begin
 {}
 Result:=False;
 try
  if Item = nil then Exit;
  {if Block = nil then Exit;} {Block may be nil}

  {Get Block}
  Current:=Block;
  if Current = nil then Current:=AddBlock(Block,Index);
  if Current = nil then Exit;
  {Check Capacity}
  if (Current.Capacity - Current.Count) < 1 then
   begin
    {Insert Block}
    Current:=AddBlock(Current,Index);
    if Current = nil then Exit;
   end;
  {Get Offset}
  Offset:=(LongWord(Index) - Current.Start) shl PtrShift; {Multiply by SizeOf(Pointer)}
  {Check Index}
  if LongWord(Index) < (Current.Start + Current.Count) then
   begin
    {Move Items}
    System.Move(Pointer(PtrUInt(Current.Data) + Offset)^,Pointer(PtrUInt(Current.Data) + Offset + 4)^,((Current.Start + Current.Count) - LongWord(Index)) shl PtrShift);
   end;
  {Add Item}
  TStringObjectEx(Pointer(PtrUInt(Current.Data) + Offset)^):=Item;
  Item.Block:=Current;
  {Update Block}
  Inc(Current.Count);
  {Update Blocks}
  Result:=UpdateBlocks(Current);
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.DeleteItem(Block:TStringBlock;Index:Integer;Item:TStringObjectEx):Boolean;
var
 Offset:LongWord;
begin
 {}
 Result:=False;
 try
  if Item = nil then Exit;
  if Block = nil then Exit;

  {Update Block}
  Dec(Block.Count);
  {Check Capacity}
  if Block.Count = 0 then
   begin
    {Delete Item}
    Item.Block:=nil;
    {Delete Block}
    Result:=DeleteBlock(Block);
   end
  else
   begin
    {Get Offset}
    Offset:=(LongWord(Index) - Block.Start) shl PtrShift; {Multiply by SizeOf(Pointer)}
    {Check Index}
    if LongWord(Index) < (Block.Start + Block.Count) then
     begin
      {Move Items}
      System.Move(Pointer(PtrUInt(Block.Data) + Offset + 4)^,Pointer(PtrUInt(Block.Data) + Offset)^,((Block.Start + Block.Count) - LongWord(Index)) shl PtrShift);
     end;
    {Delete Item}
    Item.Block:=nil;
    {Update Blocks}
    Result:=UpdateBlocks(Block);
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.IndexOfItem(Block:TStringBlock;Item:TStringObjectEx):Integer;
var
 Index:Integer;
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=-1;
 try
  if Item = nil then Exit;
  if Block = nil then Exit;

  Index:=Block.Start;
  Count:=0;
  Offset:=0;
  while Count < Block.Count do
   begin
    if TStringObjectEx(Pointer(PtrUInt(Block.Data) + Offset)^) = Item then
     begin
      Result:=Index;
      Exit;
     end;
    Inc(Index);
    Inc(Count);
    Inc(Offset,4);
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TLinkedStringListEx.Get(Index:Integer):String;
var
 Block:TStringBlock;
 Item:TStringObjectEx;
begin
 {}
 Result:='';
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Result:=Item.Value;
end;

{==============================================================================}

function TLinkedStringListEx.GetCapacity:Integer;
begin
 {}
 Result:=FCapacity;
end;

{==============================================================================}

function TLinkedStringListEx.GetObject(Index:Integer):TObject;
var
 Block:TStringBlock;
 Item:TStringObjectEx;
begin
 {}
 Result:=nil;
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Result:=Item.Data;
end;

{==============================================================================}

procedure TLinkedStringListEx.Put(Index:Integer;const S:String);
var
 Block:TStringBlock;
 Item:TStringObjectEx;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Item.Value:=S;
 Changed;
end;

{==============================================================================}

procedure TLinkedStringListEx.PutObject(Index:Integer;AObject:TObject);
var
 Block:TStringBlock;
 Item:TStringObjectEx;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Item.Data:=AObject;
 Changed;
end;

{==============================================================================}

function TLinkedStringListEx.Add(const S:String):Integer;
var
 Block:TStringBlock;
 Item:TStringObjectEx;
begin
 {}
 Result:=-1;
 Changing;
 Block:=GetBlock(FCount); {Block may be nil}
 Item:=TStringObjectEx.Create;
 Item.Value:=S;
 if AddItem(Block,FCount,Item) then
  begin
   if Link(Item) then
    begin
     Result:=FCount;
     Inc(FCount);
     Changed;
    end;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}

procedure TLinkedStringListEx.Clear;
begin
 {}
 if FCount <> 0 then
  begin
   Changing;
   ClearList;
   FRecent:=nil;
   FBlocks.ClearList;
   FCount:=0;
   FFirst:=nil;
   FLast:=nil;
   FCapacity:=0;
   Changed;
  end;
end;

{==============================================================================}

procedure TLinkedStringListEx.Delete(Index:Integer);
var
 Block:TStringBlock;
 Item:TStringObjectEx;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 if DeleteItem(Block,Index,Item) then
  begin
   if Unlink(Item) then
    begin
     Item.Free;
     Dec(FCount);
     Changed;
    end;
  end;
end;

{==============================================================================}

procedure TLinkedStringListEx.Exchange(Index1,Index2:Integer);
var
 WorkValue:String;
 WorkData:TObject;
 Block:TStringBlock;
 Item1:TStringObjectEx;
 Item2:TStringObjectEx;
begin
 {}
 if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError,Index1);
 if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError,Index2);
 Changing;
 Block:=GetBlock(Index1);
 if Block = nil then Exit;
 Item1:=GetItem(Block,Index1);
 if Item1 = nil then Exit;
 Block:=GetBlock(Index2);
 if Block = nil then Exit;
 Item2:=GetItem(Block,Index2);
 if Item2 = nil then Exit;
 WorkValue:=Item1.Value;
 WorkData:=Item1.Data;
 Item1.Value:=Item2.Value;
 Item1.Data:=Item2.Data;
 Item2.Value:=WorkValue;
 Item2.Data:=WorkData;
 Changed;
end;

{==============================================================================}

function TLinkedStringListEx.IndexOf(const S:String):Integer;
{Uses IndexOfItem within the Block of the matched String Object}
{Could use Counted Index method of TLinkedStringList instead}
var
 Hash:LongWord;
 Item:TStringObjectEx;
begin
 {}
 Result:=-1;
 Hash:=GenerateNameHash(S,stringHashSize);
 Item:=TStringObjectEx(FFirst);
 while Item <> nil do
  begin
   if Item.Hash = Hash then
    begin
     if Uppercase(Item.Value) = Uppercase(S) then
      begin
       Result:=IndexOfItem(Item.Block,Item);
       Exit;
      end;
    end;
   Item:=TStringObjectEx(Item.Next);
  end;
end;

{==============================================================================}

procedure TLinkedStringListEx.Insert(Index:Integer;const S:String);
var
 Block:TStringBlock;
 Item:TStringObjectEx;
 Prev:TStringObjectEx;
begin
 {}
 if (Index < 0) or (Index > FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index - 1);     {Block may be nil}
 Prev:=GetItem(Block,Index - 1); {Prev may be nil}
 Block:=GetBlock(Index);         {Block may be nil}
 Item:=TStringObjectEx.Create;
 Item.Value:=S;
 if AddItem(Block,Index,Item) then
  begin
   if LinkEx(Prev,Item) then
    begin
     Inc(FCount);
     Changed;
    end;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}
{==============================================================================}
{THashStringObject}
procedure THashStringObject.SetHash(AHash:LongWord);
var
 AList:THashLinkedStringList;
begin
 {}
 if FList = nil then
  begin
   FHash:=AHash;
  end
 else
  begin
   if AHash = FHash then Exit;
   AList:=FList;
   if AList.KeyUnlink(Self) then
    begin
     FHash:=AHash;
     AList.KeyLink(Self);
    end;
  end;
end;

{==============================================================================}

procedure THashStringObject.SetValue(const AValue:String);
begin
 {}
 FValue:=AValue;
 SetHash(GenerateNameHash(FValue,stringHashSize));
end;

{==============================================================================}
{==============================================================================}
{THashLinkedStringList}
constructor THashLinkedStringList.Create(AKeyBits:Byte);
begin
 {}
 inherited Create;
 FKeyBits:=AKeyBits;
 if FKeyBits = 0 then FKeyBits:=listHashBits;
 if FKeyBits < keyHashMinBits then FKeyBits:=keyHashMinBits;
 if FKeyBits > keyHashMaxBits then FKeyBits:=keyHashMaxBits;
 FKeyMask:=keyHashMasks[FKeyBits];
 FKeyShift:=PtrShift; {Key Shift value (1 shl FKeyShift = SizeOf(Pointer))}
 FKeyBuckets:=AllocMem((keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}

destructor THashLinkedStringList.Destroy;
begin
 {}
 inherited Destroy;
 FreeMem(FKeyBuckets); {Must be after inherited to allow for ClearList/Clear}
end;

{==============================================================================}

function THashLinkedStringList.KeyLink(AValue:THashStringObject):Boolean;
{Link AValue to Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 FirstKey:THashStringObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 AValue.List:=Self;
 {Get Offset}
 Offset:=(AValue.Hash and FKeyMask) shl FKeyShift;
 {Get First Key}
 FirstKey:=THashStringObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
 if FirstKey = nil then
  begin
   {Is First Object}
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=nil;
   THashStringObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
  end
 else
  begin
   {Not First Object}
   FirstKey.KeyPrev:=AValue;
   AValue.KeyPrev:=nil;
   AValue.KeyNext:=FirstKey;
   THashStringObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=AValue;
  end;
 Result:=True;
end;

{==============================================================================}

function THashLinkedStringList.KeyUnlink(AValue:THashStringObject):Boolean;
{Unlink AValue from Prev,Next Keys and Adjust First}
var
 Offset:LongWord;
 PrevKey,NextKey:THashStringObject;
begin
 {}
 Result:=False;
 if AValue = nil then Exit;
 if AValue.KeyPrev <> nil then
  begin
   {Not First Object}
   PrevKey:=AValue.KeyPrev;
   if AValue.KeyNext <> nil then
    begin
     {Not Last Object}
     NextKey:=AValue.KeyNext;
     PrevKey.KeyNext:=NextKey;
     NextKey.KeyPrev:=PrevKey;
    end
   else
    begin
     {Is Last Object}
     PrevKey.KeyNext:=nil;
    end;
  end
 else
  begin
   {Get Offset}
   Offset:=(AValue.Hash and FKeyMask) shl FKeyShift;
   {Is First Object}
   if AValue.KeyNext <> nil then
    begin
     {Not Last Object}
     NextKey:=AValue.KeyNext;
     NextKey.KeyPrev:=nil;
     THashStringObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=NextKey;
    end
   else
    begin
     {Is Last Object}
     THashStringObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^):=nil;
    end;
  end;
 AValue.List:=nil;
 AValue.KeyPrev:=nil;
 AValue.KeyNext:=nil;
 Result:=True;
end;

{==============================================================================}

procedure THashLinkedStringList.ClearList;
begin
 {}
 inherited ClearList;
 ZeroMemory(FKeyBuckets,(keyHashMasks[FKeyBits] + 1) shl FKeyShift); {Multiply bucket count (Mask + 1) by SizeOf(Pointer)}
end;

{==============================================================================}

function THashLinkedStringList.KeyFirst(AKeyHash:LongWord):THashStringObject;
var
 Offset:LongWord;
begin
 {}
 Result:=nil;
 if FKeyBuckets = nil then Exit;
 {Get Offset}
 Offset:=(AKeyHash and FKeyMask) shl FKeyShift;
 {Get First Key}
 Result:=THashStringObject(Pointer(PtrUInt(FKeyBuckets) + Offset)^);
end;

{==============================================================================}

function THashLinkedStringList.Add(const S:String):Integer;
var
 Block:TStringBlock;
 Item:THashStringObject;
begin
 {}
 Result:=-1;
 Changing;
 Block:=GetBlock(FCount); {Block may be nil}
 Item:=THashStringObject.Create;
 Item.Value:=S;
 if AddItem(Block,FCount,Item) then
  begin
   if Link(Item) then
    begin
     if KeyLink(Item) then
      begin
       Result:=FCount;
       Inc(FCount);
       Changed;
      end;
    end
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}

procedure THashLinkedStringList.Delete(Index:Integer);
var
 Block:TStringBlock;
 Item:THashStringObject;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=THashStringObject(GetItem(Block,Index));
 if Item = nil then Exit;
 if DeleteItem(Block,Index,Item) then
  begin
   if Unlink(Item) then
    begin
     if KeyUnlink(Item) then
      begin
       Item.Free;
       Dec(FCount);
       Changed;
      end;
    end;
  end;
end;

{==============================================================================}

function THashLinkedStringList.IndexOf(const S:String):Integer;
{Uses IndexOfItem within the Block of the matched String Object}
{Cannot use Counted Index method due to use of Hash Buckets}
var
 Hash:LongWord;
 Item:THashStringObject;
begin
 {}
 Result:=-1;
 Hash:=GenerateNameHash(S,stringHashSize);
 Item:=KeyFirst(Hash);
 while Item <> nil do
  begin
   if Item.Hash = Hash then
    begin
     if Uppercase(Item.Value) = Uppercase(S) then
      begin
       Result:=IndexOfItem(Item.Block,Item);
       Exit;
      end;
    end;
   Item:=THashStringObject(Item.KeyNext);
  end;
end;

{==============================================================================}

procedure THashLinkedStringList.Insert(Index:Integer;const S:String);
var
 Block:TStringBlock;
 Item:THashStringObject;
 Prev:THashStringObject;
begin
 {}
 if (Index < 0) or (Index > FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index - 1);                        {Block may be nil}
 Prev:=THashStringObject(GetItem(Block,Index - 1)); {Prev may be nil}
 Block:=GetBlock(Index);                            {Block may be nil}
 Item:=THashStringObject.Create;
 Item.Value:=S;
 if AddItem(Block,Index,Item) then
  begin
   if LinkEx(Prev,Item) then
    begin
     if KeyLink(Item) then
      begin
       Inc(FCount);
       Changed;
      end;
    end;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}
{==============================================================================}
{TIntegerList}
constructor TIntegerList.Create;
begin
 {}
 inherited Create;
 FItems:=TList.Create;
end;

{==============================================================================}

destructor TIntegerList.Destroy;
begin
 {}
 FItems.Free;
 inherited Destroy;
end;

{==============================================================================}

function TIntegerList.GetItem(Idx:Integer):Integer;
begin
 {}
 Result:=0;
 //To Do
end;

{==============================================================================}

procedure TIntegerList.SetItem(Idx,Value:Integer);
begin
 {}
 //To Do
end;

{==============================================================================}

function TIntegerList.GetCount:Integer;
begin
 {}
 Result:=0;
 //To Do
end;

{==============================================================================}

function TIntegerList.Add(AValue:Integer):Integer;
begin
 {}
 Result:=-1;
 //To Do
end;

{==============================================================================}

function TIntegerList.Remove(AValue:Integer):Integer;
begin
 {}
 Result:=-1;
 //To Do
end;

{==============================================================================}

procedure TIntegerList.Delete(Idx:Integer);
begin
 {}
 //To Do
end;

{==============================================================================}

function TIntegerList.IndexOf(AValue:Integer):Integer;
begin
 {}
 Result:=-1;
 //To Do
end;

{==============================================================================}

procedure TIntegerList.Clear;
begin
 {}
 //To Do
 FItems.Clear;
end;

{==============================================================================}
{==============================================================================}
{TDateTimeList}
constructor TDateTimeList.Create;
begin
 {}
 inherited Create;
 FItems:=TList.Create;
end;

{==============================================================================}

destructor TDateTimeList.Destroy;
begin
 {}
 FItems.Free;
 inherited Destroy;
end;

{==============================================================================}

function TDateTimeList.GetItem(Idx:Integer):TDateTime;
begin
 {}
 Result:=0;
 //To Do
end;

{==============================================================================}

procedure TDateTimeList.SetItem(Idx:Integer;Value:TDateTime);
begin
 {}
 //To Do
end;

{==============================================================================}

function TDateTimeList.GetCount:Integer;
begin
 {}
 Result:=0;
 //To Do
end;

{==============================================================================}

function TDateTimeList.Add(AValue:TDateTime):Integer;
begin
 {}
 Result:=-1;
 //To Do
end;

{==============================================================================}

function TDateTimeList.Remove(AValue:TDateTime):Integer;
begin
 {}
 Result:=-1;
 //To Do
end;

{==============================================================================}

procedure TDateTimeList.Delete(Idx:Integer);
begin
 {}
 //To Do
end;

{==============================================================================}

function TDateTimeList.IndexOf(AValue:TDateTime):Integer;
begin
 {}
 Result:=-1;
 //To Do
end;

{==============================================================================}

procedure TDateTimeList.Clear;
begin
 {}
 //To Do
 FItems.Clear;
end;

{==============================================================================}
{==============================================================================}
{TMemoryBlock}
destructor TMemoryBlock.Destroy;
begin
 {}
 if Memory <> nil then FreeMem(Memory);
 Memory:=nil;
 inherited Destroy;
end;

{==============================================================================}
{==============================================================================}
{TMemoryStreamEx}
constructor TMemoryStreamEx.Create;
begin
 {}
 inherited Create;
 FRecent:=nil;
 FBlocks:=TLinkedObjList.Create;
end;

{==============================================================================}

destructor TMemoryStreamEx.Destroy;
begin
 {}
 Clear;
 FRecent:=nil;
 FBlocks.Free;
 inherited Destroy;
end;

{==============================================================================}

function TMemoryStreamEx.RoundSize(ASize:LongInt):LongWord;
var
 Count:LongWord;
begin
 {}
 Count:=(LongWord(ASize) shr memoryStreamShift);
 if (Count shl memoryStreamShift) < LongWord(ASize) then Inc(Count);
 Result:=(Count shl memoryStreamShift);
end;

{==============================================================================}

function TMemoryStreamEx.GetBlock(AOffset:LongWord):TMemoryBlock;
var
 Block:TMemoryBlock;
begin
 {}
 Result:=nil;
 try
  {Check Recent}
  if FRecent <> nil then
   begin
    if (AOffset >= FRecent.Start) and (AOffset < (FRecent.Start + FRecent.Size)) then
     begin
      Result:=FRecent;
      Exit;
     end;
   end;
  {Get Block}
  Block:=TMemoryBlock(FBlocks.First);
  while Block <> nil do
   begin
    if (AOffset >= Block.Start) and (AOffset < (Block.Start + Block.Size)) then
     begin
      FRecent:=Block;
      Result:=Block;
      Exit;
     end;
    Block:=TMemoryBlock(Block.Next);
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TMemoryStreamEx.ReadBlock(ABlock:TMemoryBlock;ABuffer:Pointer;AOffset,ACount:LongWord):LongWord;
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;
 try
  if ABlock = nil then Exit;
  if ABuffer = nil then Exit;

  {Get Offset}
  Offset:=(AOffset - ABlock.Start);
  Count:=Min(ACount,(ABlock.Size - (AOffset - ABlock.Start)));
  {Read Block}
  System.Move(Pointer(PtrUInt(ABlock.Memory) + Offset)^,ABuffer^,Count);
  Result:=Count;
 except
  {}
 end;
end;

{==============================================================================}

function TMemoryStreamEx.WriteBlock(ABlock:TMemoryBlock;ABuffer:Pointer;AOffset,ACount:LongWord):LongWord;
var
 Count:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=0;
 try
  if ABlock = nil then Exit;
  if ABuffer = nil then Exit;

  {Get Offset}
  Offset:=(AOffset - ABlock.Start);
  Count:=Min(ACount,(ABlock.Size - (AOffset - ABlock.Start)));
  {Write Block}
  System.Move(ABuffer^,Pointer(PtrUInt(ABlock.Memory) + Offset)^,Count);
  Result:=Count;
 except
  {}
 end;
end;

{==============================================================================}

procedure TMemoryStreamEx.SetCapacity(ACapacity:LongWord);
var
 Prev:TMemoryBlock;
 Block:TMemoryBlock;
 Difference:LongWord;
begin
 {}
 if ACapacity = 0 then
  begin
   {Set to Zero}
   FRecent:=nil;
   FBlocks.ClearList;
   FCapacity:=0;
  end
 else if ACapacity < FCapacity then
  begin
   {Make Smaller}
   {Get Previous}
   Prev:=TMemoryBlock(FBlocks.Last);
   
   {Get Difference}
   Difference:=FCapacity - ACapacity;
   
   {Remove Block}
   while Prev <> nil do
    begin
     if Difference >= Prev.Size then
      begin
       if FRecent = Prev then FRecent:=nil;
       FBlocks.Remove(Prev);
       Dec(Difference,Prev.Size);
       Dec(FCapacity,Prev.Size);
       Prev.Free;
      end
     else
      begin
       Break;
      end;
      
     Prev:=TMemoryBlock(FBlocks.Last);
    end;
  end
 else if ACapacity > FCapacity then
  begin
   {Make Larger}
   {Get Previous}
   Prev:=TMemoryBlock(FBlocks.Last);
   
   {Get Difference}
   Difference:=ACapacity - FCapacity;
   
   {Create Block}
   Block:=TMemoryBlock.Create;
   Block.Memory:=GetMem(Difference);
   Block.Size:=Difference;
   Block.Start:=0;
   if Prev <> nil then Block.Start:=(Prev.Start + Prev.Size);
   FBlocks.Add(Block);
   FCapacity:=ACapacity;
   FRecent:=Block;
  end;
 {If equal then do nothing}
end;

{==============================================================================}

procedure TMemoryStreamEx.SetSize(ASize:LongInt);
var
 Current:LongInt;
begin
 {}
 Current:=FPosition;
 SetCapacity(RoundSize(ASize));
 FSize:=ASize;
 if Current > ASize then Seek(0,soFromEnd);
end;

{==============================================================================}

function TMemoryStreamEx.Read(var ABuffer;ACount:LongInt):LongInt;
var
 Start:LongWord;
 Remain:LongWord;
 Offset:LongWord;
 Length:LongWord;
 Block:TMemoryBlock;
begin
 {}
 Result:=0;
 if FPosition < 0 then Exit;
 if ACount < 0 then Exit;

 {Check Read}
 if FPosition >= FSize then Exit;
 {if (FPosition + ACount) > FSize then Exit;} {Allow request for read greater than available}
 if ((FPosition + ACount) > FSize) then ACount:=FSize - FPosition;
 if ACount <= 0 then Exit;

 {Get Start}
 Block:=nil;
 Offset:=0;
 Start:=FPosition;
 Remain:=ACount;
 while Remain > 0 do
  begin
   {Get Block}
   if Block = nil then Block:=GetBlock(Start) else Block:=TMemoryBlock(Block.Next);
   if Block = nil then Exit;
   FRecent:=Block;
   {Read Block}
   Length:=ReadBlock(Block,Pointer(PtrUInt(@ABuffer) + Offset),Start,Remain);
   if Length = 0 then Exit;
   Inc(Start,Length);
   Dec(Remain,Length);
   Inc(Offset,Length);
   Inc(Result,Length);
   Inc(FPosition,Length);
  end;
end;

{==============================================================================}

function TMemoryStreamEx.Write(const ABuffer;ACount:LongInt):LongInt;
var
 Start:LongWord;
 Remain:LongWord;
 Offset:LongWord;
 Length:LongWord;
 Block:TMemoryBlock;
begin
 {}
 Result:=0;
 if FPosition < 0 then Exit;
 if ACount < 0 then Exit;

 {Check Write}
 {if FPosition >= FSize then Exit;} {Allow Resize on Write}
 if (FPosition + ACount) > FSize then SetSize(FPosition + ACount);

 {Get Start}
 Block:=nil;
 Offset:=0;
 Start:=FPosition;
 Remain:=ACount;
 while Remain > 0 do
  begin
   {Get Block}
   if Block = nil then Block:=GetBlock(Start) else Block:=TMemoryBlock(Block.Next);
   if Block = nil then Exit;
   FRecent:=Block;
   {Write Block}
   Length:=WriteBlock(Block,Pointer(PtrUInt(@ABuffer) + Offset),Start,Remain);
   if Length = 0 then Exit;
   Inc(Start,Length);
   Dec(Remain,Length);
   Inc(Offset,Length);
   Inc(Result,Length);
   Inc(FPosition,Length);
  end;
end;

{==============================================================================}

function TMemoryStreamEx.Seek(AOffset:LongInt;AOrigin:Word):LongInt;
begin
 {}
 case AOrigin of
  soFromBeginning:FPosition:=AOffset;
  soFromCurrent:Inc(FPosition,AOffset);
  soFromEnd:FPosition:=FSize + AOffset;
 end;
 Result:=FPosition;
end;

{==============================================================================}

procedure TMemoryStreamEx.SaveToStream(AStream:TStream);
var
 Count:LongWord;
 Remain:LongWord;
 Block:TMemoryBlock;
begin
 {}
 if FSize <> 0 then
  begin
   Remain:=FSize;
   Block:=TMemoryBlock(FBlocks.First);
   while Block <> nil do
    begin
     Count:=Min(Remain,Block.Size);
     AStream.WriteBuffer(Block.Memory^,Count);
     Dec(Remain,Count);
     if Remain = 0 then Break;
     Block:=TMemoryBlock(Block.Next);
    end;
  end;
end;

{==============================================================================}

procedure TMemoryStreamEx.LoadFromStream(AStream:TStream);
var
 Count:LongWord;
 Remain:LongWord;
 Block:TMemoryBlock;
begin
 {}
 AStream.Position:=0;
 SetSize(AStream.Size);
 if FSize <> 0 then
  begin
   Remain:=FSize;
   Block:=TMemoryBlock(FBlocks.First);
   while Block <> nil do
    begin
     Count:=Min(Remain,Block.Size);
     AStream.ReadBuffer(Block.Memory^,Count);
     Dec(Remain,Count);
     if Remain = 0 then Break;
     Block:=TMemoryBlock(Block.Next);
    end;
  end;
end;

{==============================================================================}

procedure TMemoryStreamEx.SaveToFile(const AFileName:String);
var
 Stream:TStream;
begin
 {}
 Stream:=TFileStream.Create(AFileName,fmCreate);
 try
  SaveToStream(Stream);
 finally
  Stream.Free;
 end;
end;

{==============================================================================}

procedure TMemoryStreamEx.LoadFromFile(const AFileName:String);
var
 Stream:TStream;
begin
 {}
 Stream:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
 try
  LoadFromStream(Stream);
 finally
  Stream.Free;
 end;
end;

{==============================================================================}

procedure TMemoryStreamEx.Clear;
begin
 {}
 SetCapacity(0);
 FSize:=0;
 FPosition:=0;
end;

{==============================================================================}
{==============================================================================}
{TStreamEx}
function TStreamEx.GetPositionEx:Int64;
begin
 {}
 Result:=SeekEx(0,soFromCurrent);
end;

{==============================================================================}

procedure TStreamEx.SetPositionEx(const Pos:Int64);
begin
 {}
 SeekEx(Pos,soFromBeginning);
end;

{==============================================================================}

function TStreamEx.GetSizeEx:Int64;
var
 Pos:Int64;
begin
 {}
 Pos:=SeekEx(0,soFromCurrent);
 Result:=SeekEx(0,soFromEnd);
 SeekEx(Pos,soFromBeginning);
end;

{==============================================================================}

procedure TStreamEx.SetSizeEx(const NewSize:Int64);
begin
 {Nothing  (read-only streams, etc}
end;

{==============================================================================}
{==============================================================================}
{THandleStreamEx}
constructor THandleStreamEx.Create(AHandle:Integer);
begin
 {}
 FHandle:=AHandle;
end;

{==============================================================================}

procedure THandleStreamEx.SetSize(NewSize:LongInt);
begin
 {}
 FileTruncate(FHandle,NewSize);
end;

{==============================================================================}

procedure THandleStreamEx.SetSizeEx(const NewSize:Int64);
begin
 {}
 FileTruncate(FHandle,NewSize);
end;

{==============================================================================}

function THandleStreamEx.Read(var Buffer;Count:Longint):Longint;
begin
 {}
 Result:=FileRead(FHandle,Buffer,Count);
 if Result = -1 then Result:=0;
end;

{==============================================================================}

function THandleStreamEx.Write(const Buffer;Count:Longint):Longint;
begin
 {}
 Result:=FileWrite(FHandle,Buffer,Count);
 if Result = -1 then Result:=0;
end;

{==============================================================================}

function THandleStreamEx.Seek(Offset:Longint;Origin:Word):Longint;
begin
 {}
 Result:=FileSeek(FHandle,Offset,Origin);
end;

{==============================================================================}

function THandleStreamEx.SeekEx(const Offset:Int64;Origin:Word):Int64;
begin
 {}
 Result:=FileSeek(FHandle,Offset,Origin);
end;

{==============================================================================}
{==============================================================================}
{TFileStreamEx}
constructor TFileStreamEx.Create(const FileName:String;Mode:Word);
begin
 {}
 if Mode = fmCreate then
  begin
   FHandle:=FileCreate(FileName);
   if FHandle < 0 then
    raise EFCreateError.CreateFmt(SFCreateError,[FileName]);
  end
 else
  begin
   FHandle:=FileOpen(FileName,Mode);
   if FHandle < 0 then
    raise EFOpenError.CreateFmt(SFOpenError,[FileName]);
  end;
end;

{==============================================================================}

destructor TFileStreamEx.Destroy;
begin
 {}
 if FHandle >= 0 then FileClose(FHandle);
end;

{==============================================================================}
{==============================================================================}
{TStringItemEx}
procedure TStringItemEx.SetValue(const AValue:String);
begin
 {}
 FValue:=AValue;
 FHash:=GenerateNameHash(FValue,stringHashSize);
end;

{==============================================================================}
{==============================================================================}
{TStringListEx}
constructor TStringListEx.Create;
begin
 {}
 inherited Create;
 FRecent:=nil;
 FBlocks:=TLinkedObjList.Create;
 FCount:=0;
 FCapacity:=0;
end;

{==============================================================================}

destructor TStringListEx.Destroy;
begin
 {}
 FOnChange:=nil;
 FOnChanging:=nil;
 inherited Destroy;
 ClearList;
 FCount:=0;
 FCapacity:=0;
 FRecent:=nil;
 FBlocks.Free;
end;

{==============================================================================}

function TStringListEx.GetBlock(Index:Integer):TStringBlock;
var
 Block:TStringBlock;
begin
 {}
 Result:=nil;
 try
  {Check Index}
  if Index = FCount then Exit;
  {Check Recent}
  if FRecent <> nil then
   begin
    if (LongWord(Index) >= FRecent.Start) and (LongWord(Index) < (FRecent.Start + FRecent.Count)) then
     begin
      Result:=FRecent;
      Exit;
     end;
   end;
  {Get Block}
  Block:=TStringBlock(FBlocks.First);
  while Block <> nil do
   begin
    if (LongWord(Index) >= Block.Start) and (LongWord(Index) < (Block.Start + Block.Count)) then
     begin
      FRecent:=Block;
      Result:=Block;
      Exit;
     end;
    Block:=TStringBlock(Block.Next);
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TStringListEx.AddBlock(Block:TStringBlock;Index:Integer):TStringBlock;
{Index = Starting Index of Block to be Added}
{Block = Current Block containing Index or nil}
var
 Length:LongWord;
 Offset:LongWord;
 Last:TStringBlock;
begin
 {}
 Result:=nil;
 try
  if Block = nil then
   begin
    {Get Last}
    Last:=TStringBlock(FBlocks.Last);
    if (Last <> nil) and (Last.Count < Last.Capacity) then
     begin
      {Return Block}
      Result:=Last;
      
      {Set Recent}
      FRecent:=Result;
     end
    else
     begin
      {Add Block}
      Result:=TStringBlock.Create;
      Result.Data:=GetMem(stringListDelta);
      Result.Size:=stringListDelta;
      Result.Start:=LongWord(Index);
      Result.Count:=0;
      Result.Capacity:=stringListDelta shr PtrShift; {Divide by SizeOf(Pointer)}
      FBlocks.Add(Result);
      Inc(FCapacity,Result.Capacity);
      
      {Set Recent}
      FRecent:=Result;
      
      {Update Blocks}
      UpdateBlocks(Result);
     end;
   end
  else
   begin
    {Get Length}
    Length:=(Block.Count shr 1);
    
    {Get Offset}
    Offset:=(Block.Count - Length) shl PtrShift; {Multiply by SizeOf(Pointer)}
    
    {Update Block}
    Dec(Block.Count,Length);
    
    {Add Block}
    Result:=TStringBlock.Create;
    Result.Data:=GetMem(stringListDelta);
    Result.Size:=stringListDelta;
    Result.Start:=0; {Set by Update Blocks}
    Result.Count:=Length;
    Result.Capacity:=stringListDelta shr PtrShift; {Divide by SizeOf(Pointer)}
    FBlocks.Insert(Block,Result);
    Inc(FCapacity,Result.Capacity);
    
    {Copy Data}
    System.Move(Pointer(PtrUInt(Block.Data) + Offset)^,Pointer(PtrUInt(Result.Data))^,Length shl PtrShift);
    
    {Set Recent}
    FRecent:=Result;
    
    {Update Blocks}
    UpdateBlocks(Block);
    
    {Check Index}
    if LongWord(Index) < (Block.Start + Block.Count) then Result:=Block;
   end;
 except
  {}
 end;
end;

{==============================================================================}

function TStringListEx.DeleteBlock(Block:TStringBlock):Boolean;
var
 Prev:TStringBlock;
begin
 {}
 Result:=False;
 try
  if Block = nil then Exit;

  {Get Previous}
  Prev:=TStringBlock(Block.Prev);
  {Check Recent}
  if FRecent = Block then FRecent:=nil;
  {Delete Block}
  Dec(FCapacity,Block.Capacity);
  FBlocks.Remove(Block);
  Block.Free;
  {Update Blocks}
  Result:=UpdateBlocks(Prev);
 except
  {}
 end;
end;

{==============================================================================}

function TStringListEx.UpdateBlocks(Block:TStringBlock):Boolean;
var
 Start:LongWord;
 Next:TStringBlock;
begin
 {}
 Result:=False;
 try
  {if Block = nil then Exit;} {Block may be nil}

  {Get Start}
  Start:=0;
  if Block <> nil then Start:=Block.Start;
  {Get Next}
  Next:=Block;
  if Next = nil then Next:=TStringBlock(FBlocks.First);
  {Update Blocks}
  while Next <> nil do
   begin
    Next.Start:=Start;
    Inc(Start,Next.Count);
    Next:=TStringBlock(Next.Next);
   end;
  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function TStringListEx.GetItem(Block:TStringBlock;Index:Integer):TStringItemEx;
var
 Offset:LongWord;
begin
 {}
 Result:=nil;
 try
  if Block = nil then Exit;

  {Get Offset}
  Offset:=(LongWord(Index) - Block.Start) shl PtrShift; {Multiply by SizeOf(Pointer)}
  {Get Item}
  Result:=TStringItemEx(Pointer(PtrUInt(Block.Data) + Offset)^);
 except
  {}
 end;
end;

{==============================================================================}

function TStringListEx.AddItem(Block:TStringBlock;Index:Integer;Item:TStringItemEx):Boolean;
var
 Offset:LongWord;
 Current:TStringBlock;
begin
 {}
 Result:=False;
 try
  if Item = nil then Exit;
  {if Block = nil then Exit;} {Block may be nil}

  {Get Block}
  Current:=Block;
  if Current = nil then Current:=AddBlock(Block,Index);
  if Current = nil then Exit;
  {Check Capacity}
  if (Current.Capacity - Current.Count) < 1 then
   begin
    {Insert Block}
    Current:=AddBlock(Current,Index);
    if Current = nil then Exit;
   end;
  {Get Offset}
  Offset:=(LongWord(Index) - Current.Start) shl PtrShift; {Multiply by SizeOf(Pointer)}
  {Check Index}
  if LongWord(Index) < (Current.Start + Current.Count) then
   begin
    {Move Items}
    System.Move(Pointer(PtrUInt(Current.Data) + Offset)^,Pointer(PtrUInt(Current.Data) + Offset + 4)^,((Current.Start + Current.Count) - LongWord(Index)) shl PtrShift);
   end;
  {Add Item}
  TStringItemEx(Pointer(PtrUInt(Current.Data) + Offset)^):=Item;
  {Update Block}
  Inc(Current.Count);
  {Update Blocks}
  Result:=UpdateBlocks(Current);
 except
  {}
 end;
end;

{==============================================================================}

function TStringListEx.DeleteItem(Block:TStringBlock;Index:Integer;Item:TStringItemEx):Boolean;
var
 Offset:LongWord;
begin
 {}
 Result:=False;
 try
  if Item = nil then Exit;
  if Block = nil then Exit;

  {Update Block}
  Dec(Block.Count);
  {Check Count}
  if Block.Count = 0 then
   begin
    {Delete Block}
    Result:=DeleteBlock(Block);
   end
  else
   begin
    {Get Offset}
    Offset:=(LongWord(Index) - Block.Start) shl PtrShift; {Multiply by SizeOf(Pointer)}
    {Check Index}
    if LongWord(Index) < (Block.Start + Block.Count) then
     begin
      {Move Items}
      System.Move(Pointer(PtrUInt(Block.Data) + Offset + 4)^,Pointer(PtrUInt(Block.Data) + Offset)^,((Block.Start + Block.Count) - LongWord(Index)) shl PtrShift);
     end;
    {Update Blocks}
    Result:=UpdateBlocks(Block);
   end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TStringListEx.ClearList;
var
 Count:LongWord;
 Offset:LongWord;
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 try
  {Clear Objects}
  Block:=TStringBlock(FBlocks.First);
  while Block <> nil do
   begin
    Count:=0;
    Offset:=0;
    while Count < Block.Count do
     begin
      Item:=TStringItemEx(Pointer(PtrUInt(Block.Data) + Offset)^);
      Item.Free;
      Inc(Count);
      Inc(Offset,4);
     end;
    Block:=TStringBlock(Block.Next);
   end;
 except
  {}
 end;
end;

{==============================================================================}

procedure TStringListEx.Changed;
begin
 {}
 if not(FUpdating) and Assigned(FOnChange) then FOnChange(Self);
end;

{==============================================================================}

procedure TStringListEx.Changing;
begin
 {}
 if not(FUpdating) and Assigned(FOnChanging) then FOnChanging(Self);
end;

{==============================================================================}

function TStringListEx.Get(Index:Integer):String;
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 Result:='';
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Result:=Item.Value;
end;

{==============================================================================}

function TStringListEx.GetCapacity:Integer;
begin
 {}
 Result:=FCapacity;
end;

{==============================================================================}

function TStringListEx.GetCount:Integer;
begin
 {}
 Result:=FCount;
end;

{==============================================================================}

function TStringListEx.GetObject(Index:Integer):TObject;
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 Result:=nil;
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Result:=Item.Data;
end;

{==============================================================================}

procedure TStringListEx.Put(Index:Integer;const S:String);
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Item.Value:=S;
 Changed;
end;

{==============================================================================}

procedure TStringListEx.PutObject(Index:Integer;AObject:TObject);
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 Item.Data:=AObject;
 Changed;
end;

{==============================================================================}

procedure TStringListEx.SetUpdateState(Updating:Boolean);
begin
 {}
 if Updating then
  begin
   Changing;
   FUpdating:=Updating;
  end
 else
  begin
   FUpdating:=Updating;
   Changed;
  end;
end;

{==============================================================================}

function TStringListEx.Add(const S:String):Integer;
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 Result:=-1;
 Changing;
 Block:=GetBlock(FCount); {Block may be nil}
 Item:=TStringItemEx.Create;
 Item.Value:=S;
 if AddItem(Block,FCount,Item) then
  begin
   Result:=FCount;
   Inc(FCount);
   Changed;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}

procedure TStringListEx.Clear;
begin
 {}
 if FCount <> 0 then
  begin
   Changing;
   ClearList;
   FRecent:=nil;
   FBlocks.ClearList;
   FCount:=0;
   FCapacity:=0;
   Changed;
  end;
end;

{==============================================================================}

procedure TStringListEx.Delete(Index:Integer);
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 if (Index < 0) or (Index >= FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index);
 if Block = nil then Exit;
 Item:=GetItem(Block,Index);
 if Item = nil then Exit;
 if DeleteItem(Block,Index,Item) then
  begin
   Item.Free;
   Dec(FCount);
   Changed;
  end;
end;

{==============================================================================}

procedure TStringListEx.Exchange(Index1,Index2:Integer);
var
 WorkValue:String;
 WorkData:TObject;
 Item1:TStringItemEx;
 Item2:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 if (Index1 < 0) or (Index1 >= FCount) then Error(SListIndexError,Index1);
 if (Index2 < 0) or (Index2 >= FCount) then Error(SListIndexError,Index2);
 Changing;
 Block:=GetBlock(Index1);
 if Block = nil then Exit;
 Item1:=GetItem(Block,Index1);
 if Item1 = nil then Exit;
 Block:=GetBlock(Index2);
 if Block = nil then Exit;
 Item2:=GetItem(Block,Index2);
 if Item2 = nil then Exit;
 WorkValue:=Item1.Value;
 WorkData:=Item1.Data;
 Item1.Value:=Item2.Value;
 Item1.Data:=Item2.Data;
 Item2.Value:=WorkValue;
 Item2.Data:=WorkData;
 Changed;
end;

{==============================================================================}

function TStringListEx.IndexOf(const S:String):Integer;
{Uses Counted Index within the Block of the matched String Object}
var
 Hash:LongWord;
 Index:Integer;
 Count:LongWord;
 Offset:LongWord;
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 Result:=-1;
 Hash:=GenerateNameHash(S,stringHashSize);
 Block:=TStringBlock(FBlocks.First);
 while Block <> nil do
  begin
   Index:=Block.Start;
   Count:=0;
   Offset:=0;
   while Count < Block.Count do
    begin
     Item:=TStringItemEx(Pointer(PtrUInt(Block.Data) + Offset)^);
     if Item.Hash = Hash then
      begin
       if Uppercase(Item.Value) = Uppercase(S) then
        begin
         Result:=Index;
         Exit;
        end;
      end;
     Inc(Index);
     Inc(Count);
     Inc(Offset,4);
    end;
   Block:=TStringBlock(Block.Next);
  end;
end;

{==============================================================================}

procedure TStringListEx.Insert(Index:Integer;const S:String);
var
 Item:TStringItemEx;
 Block:TStringBlock;
begin
 {}
 if (Index < 0) or (Index > FCount) then Error(SListIndexError,Index);
 Changing;
 Block:=GetBlock(Index); {Block may be nil}
 Item:=TStringItemEx.Create;
 Item.Value:=S;
 if AddItem(Block,Index,Item) then
  begin
   Inc(FCount);
   Changed;
  end
 else
  begin
   Item.Free;
  end;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{Helper Functions} 

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
