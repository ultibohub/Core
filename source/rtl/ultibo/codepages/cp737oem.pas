{
Ultibo CP737 (OEM) interface unit.

Copyright (C) 2016 - SoftOz Pty Ltd.

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


CP737 OEM (Greek)
=================

 Note: This unit automatically includes CP1253ANSI as the matching ANSI code page.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit CP737OEM;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Locale;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}
        
{==============================================================================}
{const}
 {CP737OEM specific constants}

{==============================================================================}
{type}
 {CP737OEM specific types}

{==============================================================================}
{var}
 {CP737OEM specific variables}

{==============================================================================}
{Initialization Functions}
procedure CP737OEMInit;
 
{==============================================================================}
{CP737OEM Functions}
 
{==============================================================================}
{CP737OEM Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

uses CP1253ANSI;

{==============================================================================}
{==============================================================================}
var
 {CP737OEM specific variables}
 CP737OEMInitialized:Boolean;

 CPOEM737:TCodeTable = (
  MaxCharSize:1;
  DefaultChar:(
  $3F,$00);
  LeadByte:(
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00);
  Values:(
  $0000,$0001,$0002,$0003,$0004,$0005,$0006,$0007,
  $0008,$0009,$000A,$000B,$000C,$000D,$000E,$000F,
  $0010,$0011,$0012,$0013,$0014,$0015,$0016,$0017,
  $0018,$0019,$001A,$001B,$001C,$001D,$001E,$001F,
  $0020,$0021,$0022,$0023,$0024,$0025,$0026,$0027,
  $0028,$0029,$002A,$002B,$002C,$002D,$002E,$002F,
  $0030,$0031,$0032,$0033,$0034,$0035,$0036,$0037,
  $0038,$0039,$003A,$003B,$003C,$003D,$003E,$003F,
  $0040,$0041,$0042,$0043,$0044,$0045,$0046,$0047,
  $0048,$0049,$004A,$004B,$004C,$004D,$004E,$004F,
  $0050,$0051,$0052,$0053,$0054,$0055,$0056,$0057,
  $0058,$0059,$005A,$005B,$005C,$005D,$005E,$005F,
  $0060,$0061,$0062,$0063,$0064,$0065,$0066,$0067,
  $0068,$0069,$006A,$006B,$006C,$006D,$006E,$006F,
  $0070,$0071,$0072,$0073,$0074,$0075,$0076,$0077,
  $0078,$0079,$007A,$007B,$007C,$007D,$007E,$007F,
  $0391,$0392,$0393,$0394,$0395,$0396,$0397,$0398,
  $0399,$039A,$039B,$039C,$039D,$039E,$039F,$03A0,
  $03A1,$03A3,$03A4,$03A5,$03A6,$03A7,$03A8,$03A9,
  $03B1,$03B2,$03B3,$03B4,$03B5,$03B6,$03B7,$03B8,
  $03B9,$03BA,$03BB,$03BC,$03BD,$03BE,$03BF,$03C0,
  $03C1,$03C3,$03C2,$03C4,$03C5,$03C6,$03C7,$03C8,
  $2591,$2592,$2593,$2502,$2524,$2561,$2562,$2556,
  $2555,$2563,$2551,$2557,$255D,$255C,$255B,$2510,
  $2514,$2534,$252C,$251C,$2500,$253C,$255E,$255F,
  $255A,$2554,$2569,$2566,$2560,$2550,$256C,$2567,
  $2568,$2564,$2565,$2559,$2558,$2552,$2553,$256B,
  $256A,$2518,$250C,$2588,$2584,$258C,$2590,$2580,
  $03C9,$03AC,$03AD,$03AE,$03CA,$03AF,$03CC,$03CD,
  $03CB,$03CE,$0386,$0388,$0389,$038A,$038C,$038E,
  $038F,$00B1,$2265,$2264,$03AA,$03AB,$00F7,$2248,
  $00B0,$2219,$00B7,$221A,$207F,$00B2,$25A0,$00A0)
 );
 
 CP737TO1253:TTransTable = (
  TransID:1253;
  Values:(
  $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0A,$0B,$0C,$0D,$0E,$A4,
  $10,$11,$12,$13,$B6,$A7,$16,$17,$18,$19,$1A,$1B,$1C,$1D,$1E,$1F,
  $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2A,$2B,$2C,$2D,$2E,$2F,
  $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$3F,
  $40,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4A,$4B,$4C,$4D,$4E,$4F,
  $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5A,$5B,$5C,$5D,$5E,$5F,
  $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6A,$6B,$6C,$6D,$6E,$6F,
  $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7A,$7B,$7C,$7D,$7E,$7F,
  $C1,$C2,$C3,$C4,$C5,$C6,$C7,$C8,$C9,$CA,$CB,$CC,$CD,$CE,$CF,$D0,
  $D1,$D3,$D4,$D5,$D6,$D7,$D8,$D9,$E1,$E2,$E3,$E4,$E5,$E6,$E7,$E8,
  $E9,$EA,$EB,$EC,$ED,$EE,$EF,$F0,$F1,$F3,$F2,$F4,$F5,$F6,$F7,$F8,
  $2D,$2D,$2D,$A6,$2B,$5F,$5F,$5F,$5F,$A6,$A6,$AC,$2D,$5F,$5F,$AC,
  $4C,$2B,$54,$2B,$2D,$2B,$5F,$5F,$4C,$2D,$A6,$54,$A6,$3D,$2B,$5F,
  $5F,$5F,$5F,$5F,$5F,$5F,$5F,$5F,$5F,$2D,$2D,$2D,$2D,$5F,$5F,$2D,
  $F9,$DC,$DD,$DE,$FA,$DF,$FC,$FD,$FB,$FE,$A2,$B8,$B9,$BA,$BC,$BE,
  $BF,$B1,$5F,$5F,$DA,$DB,$5F,$5F,$B0,$5F,$B7,$5F,$5F,$B2,$A6,$A0)
 );
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CP737OEMInit;
begin
 {}
 {Check Initialized}
 if CP737OEMInitialized then Exit;

 {Load Default Code Pages}
 LoadPage(CP_OEM_737,@CPOEM737,nil,nil);
 InstallTrans(CP_OEM_737,@CP737TO1253);

 CP737OEMInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{CP737OEM Functions}

{==============================================================================}
{==============================================================================}
{CP737OEM Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 CP737OEMInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.