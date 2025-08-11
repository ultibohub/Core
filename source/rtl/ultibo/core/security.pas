{
Ultibo Security interface unit.

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


Security
========

 This unit implements the security support for Ultibo.

 This unit provides compatible implementations of the following functions:

        AddAccessAllowedAce
        AddAccessAllowedAceEx
        AddAccessDeniedAce
        AddAccessDeniedAceEx
        AddAce
        AddAuditAccessAce
        AddAuditAccessAceEx
        AllocateAndInitializeSid
        CopySid
        DeleteAce
        EqualPrefixSid
        EqualSid
        FindFirstFreeAce
        FreeSid
        GetAce
        GetAclInformation
        GetLengthSid
        GetSecurityDescriptorControl
        GetSecurityDescriptorDacl
        GetSecurityDescriptorGroup
        GetSecurityDescriptorLength
        GetSecurityDescriptorOwner
        GetSecurityDescriptorSacl
        GetSidIdentifierAuthority
        GetSidLengthRequired
        GetSidSubAuthority
        GetSidSubAuthorityCount
        InitializeAcl
        InitializeSecurityDescriptor
        InitializeSid
        IsValidAcl
        IsValidSecurityDescriptor
        IsValidSid
        MakeAbsoluteSD
        MakeSelfRelativeSD
        SetAclInformation
        SetSecurityDescriptorDacl
        SetSecurityDescriptorGroup
        SetSecurityDescriptorOwner
        SetSecurityDescriptorSacl

        The following functions are not currently intended to be implemented:

        AccessCheck
        AccessCheckAndAuditAlarm
        AdjustTokenGroups
        AdjustTokenPrivileges
        AllocateLocallyUniqueId
        AreAllAccessesGranted
        AreAnyAccessesGranted
        CreatePrivateObjectSecurity
        DdeImpersonateClient
        DestroyPrivateObjectSecurity
        DuplicateToken
        GetFileSecurity
        GetKernelObjectSecurity
        GetPrivateObjectSecurity
        GetProcessWindowStation
        GetTokenInformation
        GetUserObjectSecurity
        ImpersonateNamedPipeClient
        ImpersonateSelf
        LookupAccountName
        LookupAccountSid
        LookupPrivilegeDisplayName
        LookupPrivilegeName
        LookupPrivilegeValue
        MapGenericMask
        ObjectCloseAuditAlarm
        ObjectOpenAuditAlarm
        ObjectPrivilegeAuditAlarm
        OpenProcessToken
        OpenThreadToken
        PrivilegeCheck
        PrivilegedServiceAuditAlarm
        RevertToSelf
        SetFileSecurity
        SetKernelObjectSecurity
        SetPrivateObjectSecurity
        SetThreadToken
        SetTokenInformation
        SetUserObjectSecurity

        Most of the above are currently not implemented.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Security;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  System.SysUtils,
  System.Classes; //To Do //Can we remove Classes ? //Required for TStringList
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  SysUtils,
  Classes; //To Do //Can we remove Classes ? //Required for TStringList
{$ENDIF FPC_DOTTEDUNITS}

//To Do //Look for:

//LongWord(Pointer()^) -> PLongWord()^

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Security specific constants}
 ANYSIZE_ARRAY = 1;

{==============================================================================}

////////////////////////////////////////////////////////////////////////
//                                                                    //
//              Security Id     (SID)                                 //
//                                                                    //
////////////////////////////////////////////////////////////////////////
//
//
// Pictorially the structure of an SID is as follows:
//
//         1   1   1   1   1   1
//         5   4   3   2   1   0   9   8   7   6   5   4   3   2   1   0
//      +---------------------------------------------------------------+
//      |      SubAuthorityCount        |Reserved1 (SBZ)|   Revision    |
//      +---------------------------------------------------------------+
//      |                   IdentifierAuthority[0]                      |
//      +---------------------------------------------------------------+
//      |                   IdentifierAuthority[1]                      |
//      +---------------------------------------------------------------+
//      |                   IdentifierAuthority[2]                      |
//      +---------------------------------------------------------------+
//      |                                                               |
//      +- -  -  -  -  -  -  -  SubAuthority[]  -  -  -  -  -  -  -  - -+
//      |                                                               |
//      +---------------------------------------------------------------+
//
//

type
 {Security specific types}
 {SID Identifier Authority}
 PSID_IDENTIFIER_AUTHORITY = ^SID_IDENTIFIER_AUTHORITY;
 _SID_IDENTIFIER_AUTHORITY = record
   Value: array [0..5] of Byte;
 end;
 SID_IDENTIFIER_AUTHORITY = _SID_IDENTIFIER_AUTHORITY;
 TSidIdentifierAuthority = SID_IDENTIFIER_AUTHORITY;
 PSidIdentifierAuthority = PSID_IDENTIFIER_AUTHORITY;

 {SID}
 PSid = ^SID;
 _SID = record
   Revision: Byte;
   SubAuthorityCount: Byte;
   IdentifierAuthority: SID_IDENTIFIER_AUTHORITY;
   SubAuthority: array [0..ANYSIZE_ARRAY - 1] of DWORD;
 end;
 SID = _SID;
 PPSID = ^PSID;
 TSid = SID;

{==============================================================================}
const
 {Security specific constants}
 {SID}
 SID_REVISION                    = 1;  {Current revision level}
 SID_MAX_SUB_AUTHORITIES         = 15;
 SID_RECOMMENDED_SUB_AUTHORITIES = 1;  {Will change to around 6 in a future release.}

 SECURITY_MIN_SID_SIZE = SizeOf(SID) - SizeOf(DWORD); {Account for SubAuthority[0]}
 SECURITY_MAX_SID_SIZE = SizeOf(SID) - SizeOf(DWORD) + (SID_MAX_SUB_AUTHORITIES * SizeOf(DWORD)); {Account for SubAuthority[0]}

 SidTypeUser           = 1;
 SidTypeGroup          = 2;
 SidTypeDomain         = 3;
 SidTypeAlias          = 4;
 SidTypeWellKnownGroup = 5;
 SidTypeDeletedAccount = 6;
 SidTypeInvalid        = 7;
 SidTypeUnknown        = 8;
 SidTypeComputer       = 9;

 {ACL}
 ACL_REVISION    = 2;
 ACL_REVISION_DS = 4;

 ACL_REVISION1    = 1;
 ACL_REVISION2    = 2;
 MIN_ACL_REVISION = ACL_REVISION2;
 ACL_REVISION3    = 3;
 ACL_REVISION4    = 4;
 MAX_ACL_REVISION = ACL_REVISION4;

 AclRevisionInformation = 1;
 AclSizeInformation = 2;

 {ACE}
 {The following are the predefined ace types that go into the AceType field of an Ace header}
 ACCESS_MIN_MS_ACE_TYPE    = $0;
 ACCESS_ALLOWED_ACE_TYPE   = $0;
 ACCESS_DENIED_ACE_TYPE    = $1;
 SYSTEM_AUDIT_ACE_TYPE     = $2;
 SYSTEM_ALARM_ACE_TYPE     = $3;
 ACCESS_MAX_MS_V2_ACE_TYPE = $3;

 ACCESS_ALLOWED_COMPOUND_ACE_TYPE = $4;
 ACCESS_MAX_MS_V3_ACE_TYPE        = $4;

 ACCESS_MIN_MS_OBJECT_ACE_TYPE  = $5;
 ACCESS_ALLOWED_OBJECT_ACE_TYPE = $5;
 ACCESS_DENIED_OBJECT_ACE_TYPE  = $6;
 SYSTEM_AUDIT_OBJECT_ACE_TYPE   = $7;
 SYSTEM_ALARM_OBJECT_ACE_TYPE   = $8;
 ACCESS_MAX_MS_OBJECT_ACE_TYPE  = $8;

 ACCESS_MAX_MS_V4_ACE_TYPE = $8;
 ACCESS_MAX_MS_ACE_TYPE    = $8;

 ACCESS_ALLOWED_CALLBACK_ACE_TYPE        = $9;
 ACCESS_DENIED_CALLBACK_ACE_TYPE         = $A;
 ACCESS_ALLOWED_CALLBACK_OBJECT_ACE_TYPE = $B;
 ACCESS_DENIED_CALLBACK_OBJECT_ACE_TYPE  = $C;
 SYSTEM_AUDIT_CALLBACK_ACE_TYPE          = $D;
 SYSTEM_ALARM_CALLBACK_ACE_TYPE          = $E;
 SYSTEM_AUDIT_CALLBACK_OBJECT_ACE_TYPE   = $F;
 SYSTEM_ALARM_CALLBACK_OBJECT_ACE_TYPE   = $10;

 ACCESS_MAX_MS_V5_ACE_TYPE               = $10;

 {The following are the inherit flags that go into the AceFlags field of an Ace header}
 OBJECT_INHERIT_ACE       = $1;
 CONTAINER_INHERIT_ACE    = $2;
 NO_PROPAGATE_INHERIT_ACE = $4;
 INHERIT_ONLY_ACE         = $8;
 INHERITED_ACE            = $10;
 VALID_INHERIT_FLAGS      = $1F;

 {The following are the currently defined ACE flags that go into the AceFlags field of an ACE header.  Each ACE type has its own set of AceFlags}
 {SYSTEM_AUDIT and SYSTEM_ALARM AceFlags}
 SUCCESSFUL_ACCESS_ACE_FLAG = $40;
 FAILED_ACCESS_ACE_FLAG     = $80;

 {Currently defined Flags for "OBJECT" ACE types}
 ACE_OBJECT_TYPE_PRESENT           = $1;
 ACE_INHERITED_OBJECT_TYPE_PRESENT = $2;

 {Security Descriptor}
 SECURITY_DESCRIPTOR_REVISION  = 1;  {Current security descriptor revision value}
 SECURITY_DESCRIPTOR_REVISION1 = 1;

 SE_OWNER_DEFAULTED       = $0001;
 SE_GROUP_DEFAULTED       = $0002;
 SE_DACL_PRESENT          = $0004;
 SE_DACL_DEFAULTED        = $0008;
 SE_SACL_PRESENT          = $0010;
 SE_SACL_DEFAULTED        = $0020;
 SE_DACL_AUTO_INHERIT_REQ = $0100;
 SE_SACL_AUTO_INHERIT_REQ = $0200;
 SE_DACL_AUTO_INHERITED   = $0400;
 SE_SACL_AUTO_INHERITED   = $0800;
 SE_DACL_PROTECTED        = $1000;
 SE_SACL_PROTECTED        = $2000;
 SE_RM_CONTROL_VALID      = $4000;
 SE_SELF_RELATIVE         = $8000;

{==============================================================================}
type
 {Security specific types}
 {Signed types}
 BOOL = LongBool;

 {Unsigned types}
 UCHAR = Byte;

 {Pointer types}
 PUCHAR = ^Byte;

 {GUID types}
 GUID = TGUID;

 {SID}
 _SID_NAME_USE = DWORD;
 SID_NAME_USE = _SID_NAME_USE;
 PSID_NAME_USE = ^SID_NAME_USE;
 TSidNameUse = SID_NAME_USE;
 PSidNameUse = PSID_NAME_USE;

 PSID_AND_ATTRIBUTES = ^SID_AND_ATTRIBUTES;
 _SID_AND_ATTRIBUTES = record
   Sid: PSID;
   Attributes: DWORD;
 end;
 SID_AND_ATTRIBUTES = _SID_AND_ATTRIBUTES;
 TSidAndAttributes = SID_AND_ATTRIBUTES;
 PSidAndAttributes = PSID_AND_ATTRIBUTES;

 SID_AND_ATTRIBUTES_ARRAY = array [0..ANYSIZE_ARRAY - 1] of SID_AND_ATTRIBUTES;
 PSID_AND_ATTRIBUTES_ARRAY = ^SID_AND_ATTRIBUTES_ARRAY;
 PSidAndAttributesArray = ^TSidAndAttributesArray;
 TSidAndAttributesArray = SID_AND_ATTRIBUTES_ARRAY;

 {ACL}
 PACL = ^ACL;
 _ACL = record
   AclRevision: Byte;
   Sbz1: Byte;
   AclSize: Word;
   AceCount: Word;
   Sbz2: Word;
 end;
 ACL = _ACL;
 TAcl = ACL;
 PPACL = ^PACL;

 _ACL_INFORMATION_CLASS = DWORD;
 ACL_INFORMATION_CLASS = _ACL_INFORMATION_CLASS;
 TAclInformationClass = ACL_INFORMATION_CLASS;

 {This record is returned/sent if the user is requesting/setting the AclRevisionInformation}
 PACL_REVISION_INFORMATION = ^ACL_REVISION_INFORMATION;
 _ACL_REVISION_INFORMATION = record
   AclRevision: DWORD;
 end;
 ACL_REVISION_INFORMATION = _ACL_REVISION_INFORMATION;
 TAclRevisionInformation = ACL_REVISION_INFORMATION;
 PAclRevisionInformation = PACL_REVISION_INFORMATION;

 {This record is returned if the user is requesting AclSizeInformation}
 PACL_SIZE_INFORMATION = ^ACL_SIZE_INFORMATION;
 _ACL_SIZE_INFORMATION = record
   AceCount: DWORD;
   AclBytesInUse: DWORD;
   AclBytesFree: DWORD;
 end;
 ACL_SIZE_INFORMATION = _ACL_SIZE_INFORMATION;
 TAclSizeInformation = ACL_SIZE_INFORMATION;
 PAclSizeInformation = PACL_SIZE_INFORMATION;

 {ACE}
 ACCESS_MASK = DWORD;
 PACCESS_MASK = ^ACCESS_MASK;
 TAccessMask = ACCESS_MASK;
 PAccessMask = PACCESS_MASK;

 PACE_HEADER = ^ACE_HEADER;
 _ACE_HEADER = record
   AceType: Byte;
   AceFlags: Byte;
   AceSize: Word;
 end;
 ACE_HEADER = _ACE_HEADER;
 TAceHeader = ACE_HEADER;
 PAceHeader = PACE_HEADER;

 PACCESS_ALLOWED_ACE = ^ACCESS_ALLOWED_ACE;
 _ACCESS_ALLOWED_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   SidStart: DWORD;
 end;
 ACCESS_ALLOWED_ACE = _ACCESS_ALLOWED_ACE;
 TAccessAllowedAce = ACCESS_ALLOWED_ACE;
 PAccessAllowedAce = PACCESS_ALLOWED_ACE;

 PACCESS_DENIED_ACE = ^ACCESS_DENIED_ACE;
 _ACCESS_DENIED_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   SidStart: DWORD;
 end;
 ACCESS_DENIED_ACE = _ACCESS_DENIED_ACE;
 TAccessDeniedAce = ACCESS_DENIED_ACE;
 PAccessDeniedAce = PACCESS_DENIED_ACE;

 PSYSTEM_AUDIT_ACE = ^SYSTEM_AUDIT_ACE;
 _SYSTEM_AUDIT_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   SidStart: DWORD;
 end;
 SYSTEM_AUDIT_ACE = _SYSTEM_AUDIT_ACE;
 TSystemAuditAce = SYSTEM_AUDIT_ACE;
 PSystemAuditAce = PSYSTEM_AUDIT_ACE;

 PSYSTEM_ALARM_ACE = ^SYSTEM_ALARM_ACE;
 _SYSTEM_ALARM_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   SidStart: DWORD;
 end;
 SYSTEM_ALARM_ACE = _SYSTEM_ALARM_ACE;
 TSystemAlarmAce = SYSTEM_ALARM_ACE;
 PSystemAlarmAce = PSYSTEM_ALARM_ACE;

 PACCESS_ALLOWED_OBJECT_ACE = ^ACCESS_ALLOWED_OBJECT_ACE;
 _ACCESS_ALLOWED_OBJECT_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   Flags: DWORD;
   ObjectType: GUID;
   InheritedObjectType: GUID;
   SidStart: DWORD;
 end;
 ACCESS_ALLOWED_OBJECT_ACE = _ACCESS_ALLOWED_OBJECT_ACE;
 TAccessAllowedObjectAce = ACCESS_ALLOWED_OBJECT_ACE;
 PAccessAllowedObjectAce = PACCESS_ALLOWED_OBJECT_ACE;

 PACCESS_DENIED_OBJECT_ACE = ^ACCESS_DENIED_OBJECT_ACE;
 _ACCESS_DENIED_OBJECT_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   Flags: DWORD;
   ObjectType: GUID;
   InheritedObjectType: GUID;
   SidStart: DWORD;
 end;
 ACCESS_DENIED_OBJECT_ACE = _ACCESS_DENIED_OBJECT_ACE;
 TAccessDeniedObjectAce = ACCESS_DENIED_OBJECT_ACE;
 PAccessDeniedObjectAce = PACCESS_DENIED_OBJECT_ACE;

 PSYSTEM_AUDIT_OBJECT_ACE = ^SYSTEM_AUDIT_OBJECT_ACE;
 _SYSTEM_AUDIT_OBJECT_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   Flags: DWORD;
   ObjectType: GUID;
   InheritedObjectType: GUID;
   SidStart: DWORD;
 end;
 SYSTEM_AUDIT_OBJECT_ACE = _SYSTEM_AUDIT_OBJECT_ACE;
 TSystemAuditObjectAce = SYSTEM_AUDIT_OBJECT_ACE;
 PSystemAuditObjectAce = PSYSTEM_AUDIT_OBJECT_ACE;

 PSYSTEM_ALARM_OBJECT_ACE = ^SYSTEM_ALARM_OBJECT_ACE;
 _SYSTEM_ALARM_OBJECT_ACE = record
   Header: ACE_HEADER;
   Mask: ACCESS_MASK;
   Flags: DWORD;
   ObjectType: GUID;
   InheritedObjectType: GUID;
   SidStart: DWORD;
 end;
 SYSTEM_ALARM_OBJECT_ACE = _SYSTEM_ALARM_OBJECT_ACE;
 TSystemAlarmObjectAce = SYSTEM_ALARM_OBJECT_ACE;
 PSystemAlarmObjectAce = PSYSTEM_ALARM_OBJECT_ACE;

 {Callback Ace Types not currently defined}

 {Security Descriptor}
 SECURITY_DESCRIPTOR_CONTROL = WORD;
 PSECURITY_DESCRIPTOR_CONTROL = ^SECURITY_DESCRIPTOR_CONTROL;
 TSecurityDescriptorControl = SECURITY_DESCRIPTOR_CONTROL;
 PSecurityDescriptorControl = PSECURITY_DESCRIPTOR_CONTROL;

 PSECURITY_DESCRIPTOR_RELATIVE = ^SECURITY_DESCRIPTOR_RELATIVE;
 _SECURITY_DESCRIPTOR_RELATIVE = record
   Revision: Byte;
   Sbz1: Byte;
   Control: SECURITY_DESCRIPTOR_CONTROL;
   Owner: DWORD;
   Group: DWORD;
   Sacl: DWORD;
   Dacl: DWORD;
 end;
 SECURITY_DESCRIPTOR_RELATIVE = _SECURITY_DESCRIPTOR_RELATIVE;
 TSecurityDescriptorRelative = SECURITY_DESCRIPTOR_RELATIVE;
 PSecurityDescriptorRelative = PSECURITY_DESCRIPTOR_RELATIVE;

 PSECURITY_DESCRIPTOR = ^SECURITY_DESCRIPTOR;
 _SECURITY_DESCRIPTOR = record
   Revision: Byte;
   Sbz1: Byte;
   Control: SECURITY_DESCRIPTOR_CONTROL;
   Owner: PSID;
   Group: PSID;
   Sacl: PACL;
   Dacl: PACL;
 end;
 SECURITY_DESCRIPTOR = _SECURITY_DESCRIPTOR;
 TSecurityDescriptor = SECURITY_DESCRIPTOR;
 PSecurityDescriptor = PSECURITY_DESCRIPTOR;
 PPSECURITY_DESCRIPTOR = ^PSECURITY_DESCRIPTOR;

type
 {$PACKENUM 1} {--$Z4} {Change to LongWord Enumerated Types}
 WELL_KNOWN_SID_TYPE = (
  WinNullSid,
  WinWorldSid,
  WinLocalSid,
  WinCreatorOwnerSid,
  WinCreatorGroupSid,
  WinCreatorOwnerServerSid,
  WinCreatorGroupServerSid,
  WinNtAuthoritySid,
  WinDialupSid,
  WinNetworkSid,
  WinBatchSid,
  WinInteractiveSid,
  WinServiceSid,
  WinAnonymousSid,
  WinProxySid,
  WinEnterpriseControllersSid,
  WinSelfSid,
  WinAuthenticatedUserSid,
  WinRestrictedCodeSid,
  WinTerminalServerSid,
  WinRemoteLogonIdSid,
  WinLogonIdsSid,
  WinLocalSystemSid,
  WinLocalServiceSid,
  WinNetworkServiceSid,
  WinBuiltinDomainSid,
  WinBuiltinAdministratorsSid,
  WinBuiltinUsersSid,
  WinBuiltinGuestsSid,
  WinBuiltinPowerUsersSid,
  WinBuiltinAccountOperatorsSid,
  WinBuiltinSystemOperatorsSid,
  WinBuiltinPrintOperatorsSid,
  WinBuiltinBackupOperatorsSid,
  WinBuiltinReplicatorSid,
  WinBuiltinPreWindows2000CompatibleAccessSid,
  WinBuiltinRemoteDesktopUsersSid,
  WinBuiltinNetworkConfigurationOperatorsSid,
  WinAccountAdministratorSid,
  WinAccountGuestSid,
  WinAccountKrbtgtSid,
  WinAccountDomainAdminsSid,
  WinAccountDomainUsersSid,
  WinAccountDomainGuestsSid,
  WinAccountComputersSid,
  WinAccountControllersSid,
  WinAccountCertAdminsSid,
  WinAccountSchemaAdminsSid,
  WinAccountEnterpriseAdminsSid,
  WinAccountPolicyAdminsSid,
  WinAccountRasAndIasServersSid);
 TWellKnownSidType = WELL_KNOWN_SID_TYPE;
 {$PACKENUM DEFAULT}  {--$Z1}

type
 PWellKnownSid = ^TWellKnownSid;
 TWellKnownSid = record
  SidHeader:TSID;
  SubAuthorities:array[0..5] of DWORD;
 end;

 TWellKnownAce = record {Not Packed}        {Descriptor Ace defaults}
  AceType:Byte;
  AceFlags:Byte;
  AceSize:Word;
  Mask:LongWord;
  Sid:TWellKnownSidType;
 end;

 TWellKnownAcl = record {Not Packed}        {Descriptor Acl defaults}
  AclRevision:Byte;
  AclSize:Word;
  AceCount:Word;
  Aces:array[0..7] of TWellKnownAce;
 end;

 PWellKnownDescriptor = ^TWellKnownDescriptor;
 TWellKnownDescriptor = record {Not Packed} {Descriptor defaults}
  Size:LongWord;
  Revision:Byte;             {Revision}
  Control:Word;              {Control Flags}
  OwnerOffset:LongWord;      {Offset to Owner SID}
  GroupOffset:LongWord;      {Offset to Group SID}
  SaclOffset:LongWord;       {Offset to SACL}
  DaclOffset:LongWord;       {Offset to DACL}
  Owner:TWellKnownSidType;
  Group:TWellKnownSidType;
  Sacl:TWellKnownAcl;
  Dacl:TWellKnownAcl;
 end;

{==============================================================================}
{var}
 {Security specific variables}

{==============================================================================}
const
 {Security specific constants}
 SECURITY_DESCRIPTOR_MIN_LENGTH = SizeOf(SECURITY_DESCRIPTOR);

 WellKnownDescriptorFile:TWellKnownDescriptor = (
   {File (Everyone - Full Control)}
   Size:$0000004C;
   Revision:$01;
   Control:$9404;
   OwnerOffset:$00000030;
   GroupOffset:$00000040;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$001C;
         AceCount:$0001;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0014;Mask:$001F01FF;Sid:WinWorldSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  );

 WellKnownDescriptorFolder:TWellKnownDescriptor = (
   {Folder (Everyone - Full Control)}
   Size:$0000004C;
   Revision:$01;
   Control:$9404;
   OwnerOffset:$00000030;
   GroupOffset:$00000040;
   SaclOffset:$00000000;
   DaclOffset:$00000014;
   Owner:WinBuiltinAdministratorsSid;
   Group:WinLocalSystemSid;
   Sacl:(AclRevision:$00;
         AclSize:$0000;
         AceCount:$0000;
         Aces:((AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
   Dacl:(AclRevision:$02;
         AclSize:$001C;
         AceCount:$0001;
         Aces:((AceType:$00;AceFlags:$03;AceSize:$0014;Mask:$001F01FF;Sid:WinWorldSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid),
               (AceType:$00;AceFlags:$00;AceSize:$0000;Mask:$00000000;Sid:WinNullSid)
              )
        );
  );

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// Universal well-known SIDs                                               //
//                                                                         //
//     Null SID                     S-1-0-0                                //
//     World                        S-1-1-0                                //
//     Local                        S-1-2-0                                //
//     Creator Owner ID             S-1-3-0                                //
//     Creator Group ID             S-1-3-1                                //
//     Creator Owner Server ID      S-1-3-2                                //
//     Creator Group Server ID      S-1-3-3                                //
//                                                                         //
//     (Non-unique IDs)             S-1-4                                  //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

const
 SECURITY_NULL_SID_IDENTIFIER          = 0;
 SECURITY_WORLD_SID_IDENTIFIER          = 1;
 SECURITY_LOCAL_SID_IDENTIFIER          = 2;
 SECURITY_CREATOR_SID_IDENTIFIER      = 3;
 SECURITY_NON_UNIQUE_IDENTIFIER       = 4;
 SECURITY_RESOURCE_MANAGER_IDENTIFIER = 9;

 SECURITY_NULL_SID_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 0));
 SECURITY_WORLD_SID_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 1));
 SECURITY_LOCAL_SID_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 2));
 SECURITY_CREATOR_SID_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 3));
 SECURITY_NON_UNIQUE_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 4));
 SECURITY_RESOURCE_MANAGER_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 9));

 SECURITY_NULL_RID                 = $00000000;
 SECURITY_WORLD_RID                = $00000000;
 SECURITY_LOCAL_RID                = $00000000;

 SECURITY_CREATOR_OWNER_RID        = $00000000;
 SECURITY_CREATOR_GROUP_RID        = $00000001;

 SECURITY_CREATOR_OWNER_SERVER_RID = $00000002;
 SECURITY_CREATOR_GROUP_SERVER_RID = $00000003;

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// NT well-known SIDs                                                      //
//                                                                         //
//     NT Authority          S-1-5                                         //
//     Dialup                S-1-5-1                                       //
//                                                                         //
//     Network               S-1-5-2                                       //
//     Batch                 S-1-5-3                                       //
//     Interactive           S-1-5-4                                       //
//     Service               S-1-5-6                                       //
//     AnonymousLogon        S-1-5-7       (aka null logon session)        //
//     Proxy                 S-1-5-8                                       //
//     ServerLogon           S-1-5-9       (aka domain controller account) //
//     Self                  S-1-5-10      (self RID)                      //
//     Authenticated User    S-1-5-11      (Authenticated user somewhere)  //
//     Restricted Code       S-1-5-12      (Running restricted code)       //
//     Terminal Server       S-1-5-13      (Running on Terminal Server)    //
//     Remote Logon          S-1-5-14      (Remote Interactive Logon)      //
//                                                                         //
//     (Logon IDs)           S-1-5-5-X-Y                                   //
//                                                                         //
//     (NT non-unique IDs)   S-1-5-0x15-... (S-1-5-21-)                    //
//                                                                         //
//     (Built-in domain)     S-1-5-0x20     (S-1-5-32)                     //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

const
 SECURITY_NT_IDENTIFIER = 5;

 SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));

 SECURITY_DIALUP_RID                 = $00000001;
 SECURITY_NETWORK_RID                = $00000002;
 SECURITY_BATCH_RID                  = $00000003;
 SECURITY_INTERACTIVE_RID            = $00000004;
 SECURITY_SERVICE_RID                = $00000006;
 SECURITY_ANONYMOUS_LOGON_RID        = $00000007;
 SECURITY_PROXY_RID                  = $00000008;
 SECURITY_ENTERPRISE_CONTROLLERS_RID = $00000009;
 SECURITY_SERVER_LOGON_RID           = SECURITY_ENTERPRISE_CONTROLLERS_RID;
 SECURITY_PRINCIPAL_SELF_RID         = $0000000A;
 SECURITY_AUTHENTICATED_USER_RID     = $0000000B;
 SECURITY_RESTRICTED_CODE_RID        = $0000000C;
 SECURITY_TERMINAL_SERVER_RID        = $0000000D;
 SECURITY_REMOTE_LOGON_RID           = $0000000E;

 SECURITY_LOGON_IDS_RID       = $00000005;
 SECURITY_LOGON_IDS_RID_COUNT = 3;
 SECURITY_LOCAL_SYSTEM_RID    = $00000012;
 SECURITY_LOCAL_SERVICE_RID   = $00000013;
 SECURITY_NETWORK_SERVICE_RID = $00000014;
 SECURITY_NT_NON_UNIQUE       = $00000015;
 SECURITY_NT_NON_UNIQUE_SUB_AUTH_COUNT = 3;
 SECURITY_BUILTIN_DOMAIN_RID  = $00000020;

/////////////////////////////////////////////////////////////////////////////
//                                                                         //
// well-known domain relative sub-authority values (RIDs)...               //
//                                                                         //
/////////////////////////////////////////////////////////////////////////////

// Well-known users ...

 DOMAIN_USER_RID_ADMIN  = $000001F4;
 DOMAIN_USER_RID_GUEST  = $000001F5;
 DOMAIN_USER_RID_KRBTGT = $000001F6;

// well-known groups ...

 DOMAIN_GROUP_RID_ADMINS            = $00000200;
 DOMAIN_GROUP_RID_USERS             = $00000201;
 DOMAIN_GROUP_RID_GUESTS            = $00000202;
 DOMAIN_GROUP_RID_COMPUTERS         = $00000203;
 DOMAIN_GROUP_RID_CONTROLLERS       = $00000204;
 DOMAIN_GROUP_RID_CERT_ADMINS       = $00000205;
 DOMAIN_GROUP_RID_SCHEMA_ADMINS     = $00000206;
 DOMAIN_GROUP_RID_ENTERPRISE_ADMINS = $00000207;
 DOMAIN_GROUP_RID_POLICY_ADMINS     = $00000208;

// well-known aliases ...

 DOMAIN_ALIAS_RID_ADMINS           = $00000220;
 DOMAIN_ALIAS_RID_USERS            = $00000221;
 DOMAIN_ALIAS_RID_GUESTS           = $00000222;
 DOMAIN_ALIAS_RID_POWER_USERS      = $00000223;

 DOMAIN_ALIAS_RID_ACCOUNT_OPS      = $00000224;
 DOMAIN_ALIAS_RID_SYSTEM_OPS       = $00000225;
 DOMAIN_ALIAS_RID_PRINT_OPS        = $00000226;
 DOMAIN_ALIAS_RID_BACKUP_OPS       = $00000227;

 DOMAIN_ALIAS_RID_REPLICATOR       = $00000228;
 DOMAIN_ALIAS_RID_RAS_SERVERS      = $00000229;
 DOMAIN_ALIAS_RID_PREW2KCOMPACCESS = $0000022A;
 DOMAIN_ALIAS_RID_REMOTE_DESKTOP_USERS = $0000022B;
 DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS = $0000022C;

// Group attributes

 SE_GROUP_MANDATORY          = $00000001;
 SE_GROUP_ENABLED_BY_DEFAULT = $00000002;
 SE_GROUP_ENABLED            = $00000004;
 SE_GROUP_OWNER              = $00000008;
 SE_GROUP_USE_FOR_DENY_ONLY  = $00000010;
 SE_GROUP_LOGON_ID           = $C0000000;
 SE_GROUP_RESOURCE           = $20000000;

// User attributes

// (None yet defined.)

const
 {Null (Nobody) S-1-0-0}
 SECURITY_NOBODY_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NULL_SID_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_NULL_RID,0,0,0,0,0););

 {World (Everyone) S-1-1-0}
 SECURITY_EVERYONE_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_WORLD_SID_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_WORLD_RID,0,0,0,0,0););

 {Local S-1-2-0}
  {Not Used}

 {Creator Owner S-1-3-0}
 SECURITY_CREATOR_OWNER_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_CREATOR_SID_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_CREATOR_OWNER_RID,0,0,0,0,0););

 {Creator Group S-1-3-1}
 SECURITY_CREATOR_GROUP_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_CREATOR_SID_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_CREATOR_GROUP_RID,0,0,0,0,0););

 {Creator Owner Server S-1-3-2}
 SECURITY_CREATOR_OWNER_SERVER_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_CREATOR_SID_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_CREATOR_OWNER_SERVER_RID,0,0,0,0,0););

 {Creator Group Server S-1-3-3}
 SECURITY_CREATOR_GROUP_SERVER_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_CREATOR_SID_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_CREATOR_GROUP_SERVER_RID,0,0,0,0,0););

 {Nt Authority S-1-5}
 SECURITY_NTAUTHORITY_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:0;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(0,0,0,0,0,0););

 {Dialup S-1-5-1}
 SECURITY_DIALUP_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_DIALUP_RID,0,0,0,0,0););

 {Network S-1-5-2}
 SECURITY_NETWORK_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_NETWORK_RID,0,0,0,0,0););

 {Batch S-1-5-3}
 SECURITY_BATCH_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BATCH_RID,0,0,0,0,0););

 {Interactive S-1-5-4}
 SECURITY_INTERACTIVE_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_INTERACTIVE_RID,0,0,0,0,0););

 {Service S-1-5-6}
 SECURITY_SERVICE_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_SERVICE_RID,0,0,0,0,0););

 {Anonymous S-1-5-7}
 SECURITY_ANONYMOUS_LOGON_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_ANONYMOUS_LOGON_RID,0,0,0,0,0););

 {Proxy S-1-5-8}
 SECURITY_PROXY_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_PROXY_RID,0,0,0,0,0););

 {Server Logon / Enterprise Controllers S-1-5-9}
 SECURITY_SERVER_LOGON_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_SERVER_LOGON_RID,0,0,0,0,0););

 {Self S-1-5-10}
 SECURITY_PRINCIPAL_SELF_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_PRINCIPAL_SELF_RID,0,0,0,0,0););

 {Authenticated User S-1-5-11}
 SECURITY_AUTHENTICATED_USER_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_AUTHENTICATED_USER_RID,0,0,0,0,0););

 {Restricted Code S-1-5-12}
 SECURITY_RESTRICTED_CODE_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_RESTRICTED_CODE_RID,0,0,0,0,0););

 {Terminal Server S-1-5-13}
 SECURITY_TERMINAL_SERVER_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_TERMINAL_SERVER_RID,0,0,0,0,0););

 {Remote LogonId S-1-5-14}
 SECURITY_REMOTE_LOGON_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_REMOTE_LOGON_RID,0,0,0,0,0););

 {LogonIds S-1-5-5-X-Y}
  {Not Used}

 {Local System S-1-5-18}
 SECURITY_LOCAL_SYSTEM_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_LOCAL_SYSTEM_RID,0,0,0,0,0););

 {Local Service S-1-5-19}
 SECURITY_LOCAL_SERVICE_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_LOCAL_SERVICE_RID,0,0,0,0,0););

 {Network Service S-1-5-20}
 SECURITY_NETWORK_SERVICE_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_NETWORK_SERVICE_RID,0,0,0,0,0););

 {Builtin Domain S-1-5-32}
 SECURITY_BUILTIN_DOMAIN_SID:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:1;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,0,0,0,0,0););

 {Builtin Administrators S-1-5-32-544}
 LOCAL_ALIAS_RID_ADMINS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_ADMINS,0,0,0,0););

 {Builtin Users S-1-5-32-545}
 LOCAL_ALIAS_RID_USERS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_USERS,0,0,0,0););

 {Builtin Guests S-1-5-32-546}
 LOCAL_ALIAS_RID_GUESTS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_GUESTS,0,0,0,0););

 {Builtin Power Users S-1-5-32-547}
 LOCAL_ALIAS_RID_POWER_USERS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_POWER_USERS,0,0,0,0););

 {Builtin Account Operators S-1-5-32-548}
 LOCAL_ALIAS_RID_ACCOUNT_OPS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_ACCOUNT_OPS,0,0,0,0););

 {Builtin System Operators S-1-5-32-549}
 LOCAL_ALIAS_RID_SYSTEM_OPS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_SYSTEM_OPS,0,0,0,0););

 {Builtin Print Operators S-1-5-32-550}
 LOCAL_ALIAS_RID_PRINT_OPS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_PRINT_OPS,0,0,0,0););

 {Builtin Backup Operators S-1-5-32-551}
 LOCAL_ALIAS_RID_BACKUP_OPS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_BACKUP_OPS,0,0,0,0););

 {Builtin Replicator S-1-5-32-552}
 LOCAL_ALIAS_RID_REPLICATOR:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_REPLICATOR,0,0,0,0););

 {Builtin Pre Windows2000 Compatible Access S-1-5-32-554}
 LOCAL_ALIAS_RID_PREW2KCOMPACCESS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_PREW2KCOMPACCESS,0,0,0,0););

 {Builtin Remote Desktop Users S-1-5-32-555}
 LOCAL_ALIAS_RID_REMOTE_DESKTOP_USERS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_REMOTE_DESKTOP_USERS,0,0,0,0););

 {Builtin Network Configuration Operators S-1-5-32-556}
 LOCAL_ALIAS_RID_NETWORK_CONFIGURATION_OPS:TWellKnownSid =
  (SidHeader:(Revision:1;SubAuthorityCount:2;IdentifierAuthority:(Value:(0,0,0,0,0,SECURITY_NT_IDENTIFIER));SubAuthority:(0));
   SubAuthorities:(SECURITY_BUILTIN_DOMAIN_RID,DOMAIN_ALIAS_RID_NETWORK_CONFIGURATION_OPS,0,0,0,0););

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Security Functions}
function IsWellKnownSid(Sid: PSID; WellKnownSidType: WELL_KNOWN_SID_TYPE): BOOL;
function CreateWellKnownSid(WellKnownSidType: WELL_KNOWN_SID_TYPE; DomainSid: PSID; Sid: PSID; var cbSid: DWORD): BOOL;

function IsValidSid(Sid: PSID): BOOL;
function EqualSid(Sid1, Sid2: PSID): BOOL;
function EqualPrefixSid(Sid1, Sid2: PSID): BOOL;
function GetSidLengthRequired(nSubAuthorityCount: UCHAR): DWORD;
function AllocateAndInitializeSid(const pIdentifierAuthority: TSIDIdentifierAuthority; nSubAuthorityCount: Byte; nSubAuthority0, nSubAuthority1: DWORD; nSubAuthority2, nSubAuthority3, nSubAuthority4: DWORD; nSubAuthority5, nSubAuthority6, nSubAuthority7: DWORD; var Sid: PSID): BOOL;
function FreeSid(Sid: PSID): Pointer;
function InitializeSid(Sid: PSID; const pIdentifierAuthority: TSIDIdentifierAuthority; nSubAuthorityCount: Byte): BOOL;
function GetSidIdentifierAuthority(Sid: PSID): PSIDIdentifierAuthority;
function GetSidSubAuthority(Sid: PSID; nSubAuthority: DWORD): PDWORD;
function GetSidSubAuthorityCount(Sid: PSID): PUCHAR;
function GetLengthSid(Sid: PSID): DWORD;
function CopySid(nDestinationSidLength: DWORD; pDestinationSid, pSourceSid: PSID): BOOL;

function ConvertSidToStringSid(Sid: PSID; var StringSid: PChar): BOOL;
function ConvertStringSidToSid(StringSid: PChar; var Sid: PSID): BOOL;

function IsValidAcl(const pAcl: TACL): BOOL;
function InitializeAcl(var pAcl: TACL; nAclLength, dwAclRevision: DWORD): BOOL;
function GetAclInformation(const pAcl: TACL; pAclInformation: Pointer; nAclInformationLength: DWORD; dwAclInformationClass: TAclInformationClass): BOOL;
function SetAclInformation(var pAcl: TACL; pAclInformation: Pointer; nAclInformationLength: DWORD; dwAclInformationClass: TAclInformationClass): BOOL;
function AddAce(var pAcl: TACL; dwAceRevision, dwStartingAceIndex: DWORD; pAceList: Pointer; nAceListLength: DWORD): BOOL;
function DeleteAce(var pAcl: TACL; dwAceIndex: DWORD): BOOL;
function GetAce(const pAcl: TACL; dwAceIndex: DWORD; var pAce: Pointer): BOOL;
function AddAccessAllowedAce(var pAcl: TACL; dwAceRevision: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
function AddAccessAllowedAceEx(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
function AddAccessDeniedAce(var pAcl: TACL; dwAceRevision: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
function AddAccessDeniedAceEx(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
function AddAuditAccessAce(var pAcl: TACL; dwAceRevision: DWORD; dwAccessMask: DWORD; Sid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL;
function AddAuditAccessAceEx(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; dwAccessMask: DWORD; Sid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL;
function AddAccessAllowedObjectAce(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: PGUID; Sid: PSID): BOOL;
function AddAccessDeniedObjectAce(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: PGUID; Sid: PSID): BOOL;
function AddAuditAccessObjectAce(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: PGUID; Sid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL;
function FindFirstFreeAce(var pAcl: TACL; var pAce: Pointer): BOOL;

function InitializeSecurityDescriptor(pSecurityDescriptor: PSecurityDescriptor; dwRevision: DWORD): BOOL;
function IsValidSecurityDescriptor(pSecurityDescriptor: PSecurityDescriptor): BOOL;
function GetSecurityDescriptorLength(pSecurityDescriptor: PSecurityDescriptor): DWORD;
function GetSecurityDescriptorControl(pSecurityDescriptor: PSecurityDescriptor; var pControl: SECURITY_DESCRIPTOR_CONTROL; var lpdwRevision: DWORD): BOOL;
function SetSecurityDescriptorControl(pSecurityDescriptor: PSecurityDescriptor; ControlBitsOfInterest, ControlBitsToSet: SECURITY_DESCRIPTOR_CONTROL): BOOL;
function GetSecurityDescriptorDacl(pSecurityDescriptor: PSecurityDescriptor; var lpbDaclPresent: BOOL; var pDacl: PACL; var lpbDaclDefaulted: BOOL): BOOL;
function SetSecurityDescriptorDacl(pSecurityDescriptor: PSecurityDescriptor; bDaclPresent: BOOL; pDacl: PACL; bDaclDefaulted: BOOL): BOOL;
function GetSecurityDescriptorSacl(pSecurityDescriptor: PSecurityDescriptor; var lpbSaclPresent: BOOL; var pSacl: PACL; var lpbSaclDefaulted: BOOL): BOOL;
function SetSecurityDescriptorSacl(pSecurityDescriptor: PSecurityDescriptor; bSaclPresent: BOOL; pSacl: PACL; bSaclDefaulted: BOOL): BOOL;
function GetSecurityDescriptorOwner(pSecurityDescriptor: PSecurityDescriptor; var pOwner: PSID; var lpbOwnerDefaulted: BOOL): BOOL;
function SetSecurityDescriptorOwner(pSecurityDescriptor: PSecurityDescriptor; pOwner: PSID; bOwnerDefaulted: BOOL): BOOL;
function GetSecurityDescriptorGroup(pSecurityDescriptor: PSecurityDescriptor; var pGroup: PSID; var lpbGroupDefaulted: BOOL): BOOL;
function SetSecurityDescriptorGroup(pSecurityDescriptor: PSecurityDescriptor; pGroup: PSID; bGroupDefaulted: BOOL): BOOL;

function MakeSelfRelativeSD(pAbsoluteSecurityDescriptor: PSecurityDescriptor; pSelfRelativeSecurityDescriptor: PSecurityDescriptor; var lpdwBufferLength: DWORD): BOOL;
function MakeAbsoluteSD(pSelfRelativeSecurityDescriptor: PSecurityDescriptor; pAbsoluteSecurityDescriptor: PSecurityDescriptor; var lpdwAbsoluteSecurityDescriptorSi: DWORD; var pDacl: TACL; var lpdwDaclSize: DWORD; var pSacl: TACL; var lpdwSaclSize: DWORD; pOwner: PSID; var lpdwOwnerSize: DWORD; pPrimaryGroup: PSID; var lpdwPrimaryGroupSize: DWORD): BOOL;
function MakeAbsoluteSD2(pSelfRelativeSecurityDescriptor: PSecurityDescriptor; var lpdwBufferSize: DWORD): BOOL;

{==============================================================================}
{Security Helper Functions}
function SplitStringSid(const StringSid:String):TStringList;

function CreateDefaultSid(var pCreatedSid: PSID): BOOL;
function DestroyDefaultSid(pDefaultSid: PSID): BOOL;

function CreateDefaultSecurityDescriptor(var pCreatedSecurityDescriptor: PSecurityDescriptor; bFolder: BOOL): BOOL;
function DestroyDefaultSecurityDescriptor(pDefaultSecurityDescriptor: PSecurityDescriptor): BOOL;

{These functions are temporary until

  BuildSecurityDescriptor
  CreatePrivateObjectSecurity
  CreatePrivateObjectSecurityEx
  ConvertToAutoInheritPrivateObjectSecurity

 are completely implemented in this unit}

function CreateInheritedSecurityDescriptorNT(pParentSecurityDescriptor: PSecurityDescriptor; var pCreatedSecurityDescriptor: PSecurityDescriptor): BOOL;
function CreateInheritedSecurityDescriptor2K(pParentSecurityDescriptor: PSecurityDescriptor; var pCreatedSecurityDescriptor: PSecurityDescriptor): BOOL;
function CreateMergedSecurityDescriptor2K(pParentSecurityDescriptor, pChildSecurityDescriptor: PSecurityDescriptor; var pCreatedSecurityDescriptor: PSecurityDescriptor): BOOL;

function DestroyInheritedSecurityDescriptor(pInheritedSecurityDescriptor: PSecurityDescriptor): BOOL;
function DestroyMergedSecurityDescriptor(pMergedSecurityDescriptor: PSecurityDescriptor): BOOL;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{Security Functions}
function IsWellKnownSid(Sid: PSID; WellKnownSidType: WELL_KNOWN_SID_TYPE): BOOL;
var
 Size:DWORD;
 WellKnownSid:PSID;
begin
 {}
 Result:=False;

 Size:=SECURITY_MAX_SID_SIZE;
 WellKnownSid:=AllocMem(Size);
 try
  if CreateWellKnownSid(WellKnownSidType,nil,WellKnownSid,Size) then
   begin
    Result:=EqualSid(WellKnownSid,Sid);
   end;
  finally
   FreeMem(WellKnownSid);
  end;
end;

{==============================================================================}

function CreateWellKnownSid(WellKnownSidType: WELL_KNOWN_SID_TYPE; DomainSid: PSID; Sid: PSID; var cbSid: DWORD): BOOL;
var
 Size:DWORD;
 NewSid:PSID;
 WellKnownSid:PWellKnownSid;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_PARAMETER);

 NewSid:=nil;
 WellKnownSid:=nil;
 case WellKnownSidType of
  WinNullSid:begin
    WellKnownSid:=@SECURITY_NOBODY_SID;
   end;
  WinWorldSid:begin
    WellKnownSid:=@SECURITY_EVERYONE_SID;
   end;
  WinLocalSid:begin
    {Not Supported}
   end;
  WinCreatorOwnerSid:begin
    WellKnownSid:=@SECURITY_CREATOR_OWNER_SID;
   end;
  WinCreatorGroupSid:begin
    WellKnownSid:=@SECURITY_CREATOR_GROUP_SID;
   end;
  WinCreatorOwnerServerSid:begin
    WellKnownSid:=@SECURITY_CREATOR_OWNER_SERVER_SID;
   end;
  WinCreatorGroupServerSid:begin
    WellKnownSid:=@SECURITY_CREATOR_GROUP_SERVER_SID;
   end;
  WinNtAuthoritySid:begin
    WellKnownSid:=@SECURITY_NTAUTHORITY_SID;
   end;
  WinDialupSid:begin
    WellKnownSid:=@SECURITY_DIALUP_SID;
   end;
  WinNetworkSid:begin
    WellKnownSid:=@SECURITY_NETWORK_SID;
   end;
  WinBatchSid:begin
    WellKnownSid:=@SECURITY_BATCH_SID;
   end;
  WinInteractiveSid:begin
    WellKnownSid:=@SECURITY_INTERACTIVE_SID;
   end;
  WinServiceSid:begin
    WellKnownSid:=@SECURITY_SERVICE_SID;
   end;
  WinAnonymousSid:begin
    WellKnownSid:=@SECURITY_ANONYMOUS_LOGON_SID;
   end;
  WinProxySid:begin
    WellKnownSid:=@SECURITY_PROXY_SID;
   end;
  WinEnterpriseControllersSid:begin
    WellKnownSid:=@SECURITY_SERVER_LOGON_SID;
   end;
  WinSelfSid:begin
    WellKnownSid:=@SECURITY_PRINCIPAL_SELF_SID;
   end;
  WinAuthenticatedUserSid:begin
    WellKnownSid:=@SECURITY_AUTHENTICATED_USER_SID;
   end;
  WinRestrictedCodeSid:begin
    WellKnownSid:=@SECURITY_RESTRICTED_CODE_SID;
   end;
  WinTerminalServerSid:begin
    WellKnownSid:=@SECURITY_TERMINAL_SERVER_SID;
   end;
  WinRemoteLogonIdSid:begin
    WellKnownSid:=@SECURITY_REMOTE_LOGON_SID;
   end;
  WinLogonIdsSid:begin
    {Not Supported}
   end;
  WinLocalSystemSid:begin
    WellKnownSid:=@SECURITY_LOCAL_SYSTEM_SID;
   end;
  WinLocalServiceSid:begin
    WellKnownSid:=@SECURITY_LOCAL_SERVICE_SID;
   end;
  WinNetworkServiceSid:begin
    WellKnownSid:=@SECURITY_NETWORK_SERVICE_SID;
   end;
  WinBuiltinDomainSid:begin
    WellKnownSid:=@SECURITY_BUILTIN_DOMAIN_SID;
   end;
  WinBuiltinAdministratorsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_ADMINS;
   end;
  WinBuiltinUsersSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_USERS;
   end;
  WinBuiltinGuestsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_GUESTS;
   end;
  WinBuiltinPowerUsersSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_POWER_USERS;
   end;
  WinBuiltinAccountOperatorsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_ACCOUNT_OPS;
   end;
  WinBuiltinSystemOperatorsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_SYSTEM_OPS;
   end;
  WinBuiltinPrintOperatorsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_PRINT_OPS;
   end;
  WinBuiltinBackupOperatorsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_BACKUP_OPS;
   end;
  WinBuiltinReplicatorSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_REPLICATOR;
   end;
  WinBuiltinPreWindows2000CompatibleAccessSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_PREW2KCOMPACCESS;
   end;
  WinBuiltinRemoteDesktopUsersSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_REMOTE_DESKTOP_USERS;
   end;
  WinBuiltinNetworkConfigurationOperatorsSid:begin
    WellKnownSid:=@LOCAL_ALIAS_RID_NETWORK_CONFIGURATION_OPS;
   end;
  WinAccountAdministratorSid:begin
    {Not Supported}
   end;
  WinAccountGuestSid:begin
    {Not Supported}
   end;
  WinAccountKrbtgtSid:begin
    {Not Supported}
   end;
  WinAccountDomainAdminsSid:begin
    {Not Supported}
   end;
  WinAccountDomainUsersSid:begin
    {Not Supported}
   end;
  WinAccountDomainGuestsSid:begin
    {Not Supported}
   end;
  WinAccountComputersSid:begin
    {Not Supported}
   end;
  WinAccountControllersSid:begin
    {Not Supported}
   end;
  WinAccountCertAdminsSid:begin
    {Not Supported}
   end;
  WinAccountSchemaAdminsSid:begin
    {Not Supported}
   end;
  WinAccountEnterpriseAdminsSid:begin
    {Not Supported}
   end;
  WinAccountPolicyAdminsSid:begin
    {Not Supported}
   end;
  WinAccountRasAndIasServersSid:begin
    {Not Supported}
   end;
 end;
 if WellKnownSid = nil then Exit;

 {Create Sid}
 if AllocateAndInitializeSid(WellKnownSid.SidHeader.IdentifierAuthority,WellKnownSid.SidHeader.SubAuthorityCount,WellKnownSid.SubAuthorities[0],WellKnownSid.SubAuthorities[1],WellKnownSid.SubAuthorities[2],WellKnownSid.SubAuthorities[3],WellKnownSid.SubAuthorities[4],WellKnownSid.SubAuthorities[5],0,0,NewSid) then
  begin
   try
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    Size:=GetLengthSid(NewSid);
    if cbSid < Size then Exit;
    if CopySid(cbSid,Sid,NewSid) then
     begin
      cbSid:=Size;

      SetLastError(ERROR_SUCCESS);
      Result:=True;
     end;
   finally
    FreeSid(NewSid);
   end;
  end;
end;

{==============================================================================}

function IsValidSid(Sid: PSID): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SID);

 {Do not check for null SID}

 if Sid.Revision <> SID_REVISION then Exit;
 if Sid.SubAuthorityCount > SID_MAX_SUB_AUTHORITIES then Exit;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function EqualSid(Sid1, Sid2: PSID): BOOL;
var
 Size:LongWord;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SID);

 {Do not check for null SID}

 Size:=GetLengthSid(Sid1);
 if Size = 0 then Exit;
 if Size <> GetLengthSid(Sid2) then Exit;

 Result:=SysUtils.CompareMem(Sid1,Sid2,Size);
 if Result then SetLastError(ERROR_SUCCESS);
end;

{==============================================================================}

function EqualPrefixSid(Sid1, Sid2: PSID): BOOL;
var
 Count:Byte;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SID);

 {Do not check for null SID}
 if Sid1.Revision <> Sid2.Revision then Exit;
 if Sid1.SubAuthorityCount <> Sid2.SubAuthorityCount then Exit;
 if not SysUtils.CompareMem(@Sid1.IdentifierAuthority.Value[0],@Sid2.IdentifierAuthority.Value[0],SizeOf(SID_IDENTIFIER_AUTHORITY)) then Exit;

 Count:=0;
 while Count < Sid1.SubAuthorityCount do
  begin
   if LongWord(Pointer(PtrUInt(@Sid1.SubAuthority[0]) + LongWord(Count * SizeOf(DWORD)))^) <> LongWord(Pointer(PtrUInt(@Sid2.SubAuthority[0]) + LongWord(Count * SizeOf(DWORD)))^) then Exit;
  end;

  SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function GetSidLengthRequired(nSubAuthorityCount: UCHAR): DWORD;
begin
 {}
 SetLastError(ERROR_SUCCESS);
 Result:=(SizeOf(TSID) - SizeOf(DWORD)) + (nSubAuthorityCount * SizeOf(DWORD)); {Account for SubAuthority[0]}
end;

{==============================================================================}

function AllocateAndInitializeSid(const pIdentifierAuthority: TSIDIdentifierAuthority; nSubAuthorityCount: Byte; nSubAuthority0, nSubAuthority1: DWORD; nSubAuthority2, nSubAuthority3, nSubAuthority4: DWORD; nSubAuthority5, nSubAuthority6, nSubAuthority7: DWORD; var Sid: PSID): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SID);

 if nSubAuthorityCount < 1 then Exit;
 if nSubAuthorityCount > 8 then Exit;

 Sid:=AllocMem(GetSidLengthRequired(nSubAuthorityCount));
 if Sid = nil then Exit;

 Sid.Revision:=SID_REVISION;
 Sid.SubAuthorityCount:=nSubAuthorityCount;
 Sid.IdentifierAuthority:=pIdentifierAuthority;
 if nSubAuthorityCount > 0 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(0 * SizeOf(DWORD)))^:=nSubAuthority0;
 if nSubAuthorityCount > 1 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(1 * SizeOf(DWORD)))^:=nSubAuthority1;
 if nSubAuthorityCount > 2 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(2 * SizeOf(DWORD)))^:=nSubAuthority2;
 if nSubAuthorityCount > 3 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(3 * SizeOf(DWORD)))^:=nSubAuthority3;
 if nSubAuthorityCount > 4 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(4 * SizeOf(DWORD)))^:=nSubAuthority4;
 if nSubAuthorityCount > 5 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(5 * SizeOf(DWORD)))^:=nSubAuthority5;
 if nSubAuthorityCount > 6 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(6 * SizeOf(DWORD)))^:=nSubAuthority6;
 if nSubAuthorityCount > 7 then PLongWord(PtrUInt(@Sid.SubAuthority[0]) + LongWord(7 * SizeOf(DWORD)))^:=nSubAuthority7;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function FreeSid(Sid: PSID): Pointer;
begin
 {}
 Result:=Sid;
 SetLastError(ERROR_INVALID_SID);

 if Sid = nil then Exit;

 FreeMem(Sid);

 SetLastError(ERROR_SUCCESS);
 Result:=nil;
end;

{==============================================================================}

function InitializeSid(Sid: PSID; const pIdentifierAuthority: TSIDIdentifierAuthority; nSubAuthorityCount: Byte): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SID);

 if Sid = nil then Exit;

 Sid.Revision:=SID_REVISION;
 Sid.SubAuthorityCount:=nSubAuthorityCount;
 Sid.IdentifierAuthority:=pIdentifierAuthority;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function GetSidIdentifierAuthority(Sid: PSID): PSIDIdentifierAuthority;
begin
 {}
 {Do not check for null SID}

 SetLastError(ERROR_SUCCESS);
 Result:=@Sid.IdentifierAuthority;
end;

{==============================================================================}

function GetSidSubAuthority(Sid: PSID; nSubAuthority: DWORD): PDWORD;
begin
 {}
 {Do not check for null SID}

 SetLastError(ERROR_SUCCESS);
 Result:=PDWORD(PtrUInt(@Sid.SubAuthority[0]) + LongWord((nSubAuthority) * SizeOf(DWORD))); {Sub Authority is zero based index}
end;

{==============================================================================}

function GetSidSubAuthorityCount(Sid: PSID): PUCHAR;
begin
 {}
 {Do not check for null SID}

 SetLastError(ERROR_SUCCESS);
 Result:=PUCHAR(@Sid.SubAuthorityCount);
end;

{==============================================================================}

function GetLengthSid(Sid: PSID): DWORD;
begin
 {}
 {Do not check for null SID}

 SetLastError(ERROR_SUCCESS);
 Result:=(SizeOf(TSID) - SizeOf(DWORD)) + (Sid.SubAuthorityCount * SizeOf(DWORD)); {Account for SubAuthority[0]}
end;

{==============================================================================}

function CopySid(nDestinationSidLength: DWORD; pDestinationSid, pSourceSid: PSID): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SID);

 if pSourceSid = nil then Exit;
 if pDestinationSid = nil then Exit;
 if nDestinationSidLength < GetLengthSid(pSourceSid) then Exit;

 System.Move(pSourceSid^,pDestinationSid^,GetLengthSid(pSourceSid)); {System.Move(pSourceSid^,pDestinationSid^,nDestinationSidLength);} {Must copy source length not dest length}

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function ConvertSidToStringSid(Sid: PSID; var StringSid: PChar): BOOL;
var
 Count:Integer;
 WorkBuffer:String;
 SubAuthorityCount:Byte;
 IdentifierAuthority:PSIDIdentifierAuthority;
begin
 {}
 Result:=False;
 try
  SetLastError(ERROR_INVALID_PARAMETER);
  if Sid = nil then Exit;

  SetLastError(ERROR_INVALID_SID);
  if not IsValidSid(Sid) then Exit;

  {Get Revision}
  WorkBuffer:='S-' + IntToStr(Sid.Revision) + '-';

  {Get Identifier Authority}
  IdentifierAuthority:=GetSidIdentifierAuthority(Sid);
  WorkBuffer:=WorkBuffer + IntToStr(IdentifierAuthority.Value[5]) + '-';

  {Get Sub Authority Count}
  SubAuthorityCount:=GetSidSubAuthorityCount(Sid)^; {Dereference Pointer}

  {Get Sub Authorities}
  for Count:=0 to SubAuthorityCount - 1 do
   begin
    if Count = SubAuthorityCount - 1 then
     begin
      WorkBuffer:=WorkBuffer + IntToStr(GetSidSubAuthority(Sid,Count)^);
     end
    else
     begin
      WorkBuffer:=WorkBuffer + IntToStr(GetSidSubAuthority(Sid,Count)^) + '-';
     end;
   end;

  {Return String}
  StringSid:=PChar(AllocMem(Length(WorkBuffer) + 1));
  StrLCopy(StringSid,PChar(WorkBuffer),Length(WorkBuffer) + 1);

  SetLastError(ERROR_SUCCESS);
  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function ConvertStringSidToSid(StringSid: PChar; var Sid: PSID): BOOL;
var
 WorkBuffer:String;
 Values:TStringList;
 SubAuthorityCount:Byte;
 IdentifierAuthority:TSIDIdentifierAuthority;
begin
 {}
 Result:=False;
 try
  SetLastError(ERROR_INVALID_PARAMETER);
  WorkBuffer:=StringSid;
  WorkBuffer:=Uppercase(Trim(WorkBuffer));
  if Length(WorkBuffer) = 0 then Exit;
  SetLastError(ERROR_INVALID_SID);

  {Check Prefix}
  if Copy(WorkBuffer,1,2) <> 'S-' then Exit;

  {Split String}
  Values:=SplitStringSid(WorkBuffer);
  try
   {Check Values}
   if Values.Count < 4 then Exit;

   {Check Prefix and Revision}
   if Values.Strings[0] <> 'S' then Exit;
   if Values.Strings[1] <> IntToStr(SID_REVISION) then Exit;

   {Get Identifier Authority}
   FillChar(IdentifierAuthority,SizeOf(TSIDIdentifierAuthority),0);
   IdentifierAuthority.Value[5]:=StrToInt(Values.Strings[2]);

   {Allocate and Initialize Sid}
   SubAuthorityCount:=Values.Count - 3;
   while Values.Count < 11 do Values.Add('0');

   Result:=AllocateAndInitializeSid(IdentifierAuthority,SubAuthorityCount,StrToInt64(Values.Strings[3]),StrToInt64(Values.Strings[4]),StrToInt64(Values.Strings[5]),StrToInt64(Values.Strings[6]),StrToInt64(Values.Strings[7]),StrToInt64(Values.Strings[8]),StrToInt64(Values.Strings[9]),StrToInt64(Values.Strings[10]),Sid);
  finally
   Values.Free;
  end;
 except
  {}
 end;
end;

{==============================================================================}

function IsValidAcl(const pAcl: TACL): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_ACL);

 if pAcl.AclRevision < MIN_ACL_REVISION then Exit;
 if pAcl.AclRevision > MAX_ACL_REVISION then Exit;
 if pAcl.AclSize < (SizeOf(TACL) + (pAcl.AceCount * SizeOf(TAceHeader))) then Exit; {Note: This needs to be expanded to fully check the Ace and Sid entries}

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function InitializeAcl(var pAcl: TACL; nAclLength, dwAclRevision: DWORD): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_ACL);

 if (dwAclRevision <> ACL_REVISION) and (dwAclRevision <> ACL_REVISION_DS) then Exit;

 pAcl.AclRevision:=dwAclRevision;
 pAcl.Sbz1:=0;
 pAcl.AclSize:=nAclLength;
 pAcl.AceCount:=0;
 pAcl.Sbz2:=0;

 Result:=IsValidAcl(pAcl);
end;

{==============================================================================}

function GetAclInformation(const pAcl: TACL; pAclInformation: Pointer; nAclInformationLength: DWORD; dwAclInformationClass: TAclInformationClass): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_PARAMETER);

 {Do not check for null ACL}

 case dwAclInformationClass of
  AclRevisionInformation:begin
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    if nAclInformationLength < SizeOf(TAclRevisionInformation) then Exit;
    PAclRevisionInformation(pAclInformation).AclRevision:=pAcl.AclRevision;

    SetLastError(ERROR_SUCCESS);
    Result:=True;
   end;
  AclSizeInformation:begin
    SetLastError(ERROR_INSUFFICIENT_BUFFER);
    if nAclInformationLength < SizeOf(TAclSizeInformation) then Exit;
    PAclSizeInformation(pAclInformation).AceCount:=pAcl.AceCount;
    PAclSizeInformation(pAclInformation).AclBytesInUse:=pAcl.AclSize; {Note: This needs to be implemented}
    PAclSizeInformation(pAclInformation).AclBytesFree:=0;             {Note: This needs to be implemented}

    SetLastError(ERROR_SUCCESS);
    Result:=True;
   end;
 end;
end;

{==============================================================================}

function SetAclInformation(var pAcl: TACL; pAclInformation: Pointer; nAclInformationLength: DWORD; dwAclInformationClass: TAclInformationClass): BOOL;
begin
 {}
 Result:=False;

 //To Do //AclRevisionInformation only
end;

{==============================================================================}

function AddAce(var pAcl: TACL; dwAceRevision, dwStartingAceIndex: DWORD; pAceList: Pointer; nAceListLength: DWORD): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function DeleteAce(var pAcl: TACL; dwAceIndex: DWORD): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function GetAce(const pAcl: TACL; dwAceIndex: DWORD; var pAce: Pointer): BOOL;
var
 Index:LongWord;
 Offset:LongWord;
 AceHeader:PAceHeader;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_PARAMETER);

 if dwAceIndex >= pAcl.AceCount then Exit;

 Index:=0;
 Offset:=SizeOf(TACL);
 AceHeader:=PAceHeader(PtrUInt(@pAcl) + Offset);
 while Index < dwAceIndex do
  begin
   Inc(Index);
   Inc(Offset,AceHeader.AceSize);
   AceHeader:=PAceHeader(PtrUInt(@pAcl) + Offset);
   if Index = dwAceIndex then Break;
  end;
 pAce:=AceHeader;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function AddAccessAllowedAce(var pAcl: TACL; dwAceRevision: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAccessAllowedAceEx(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAccessDeniedAce(var pAcl: TACL; dwAceRevision: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAccessDeniedAceEx(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; Sid: PSID): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAuditAccessAce(var pAcl: TACL; dwAceRevision: DWORD; dwAccessMask: DWORD; Sid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAuditAccessAceEx(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; dwAccessMask: DWORD; Sid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAccessAllowedObjectAce(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: PGUID; Sid: PSID): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAccessDeniedObjectAce(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: PGUID; Sid: PSID): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function AddAuditAccessObjectAce(var pAcl: TACL; dwAceRevision: DWORD; AceFlags: DWORD; AccessMask: DWORD; ObjectTypeGuid, InheritedObjectTypeGuid: PGUID; Sid: PSID; bAuditSuccess, bAuditFailure: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function FindFirstFreeAce(var pAcl: TACL; var pAce: Pointer): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function InitializeSecurityDescriptor(pSecurityDescriptor: PSecurityDescriptor; dwRevision: DWORD): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_PARAMETER);

 if dwRevision <> SECURITY_DESCRIPTOR_REVISION then Exit;

 pSecurityDescriptor.Revision:=dwRevision;
 pSecurityDescriptor.Sbz1:=0;
 pSecurityDescriptor.Control:=0;
 pSecurityDescriptor.Owner:=nil;
 pSecurityDescriptor.Group:=nil;
 pSecurityDescriptor.Sacl:=nil;  {Sacl is prior to Dacl in structure (Offset always places it after)}
 pSecurityDescriptor.Dacl:=nil;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function IsValidSecurityDescriptor(pSecurityDescriptor: PSecurityDescriptor): BOOL;
var
 Acl:PACL;
 Sid:PSID;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSecurityDescriptor = nil then Exit;
 if pSecurityDescriptor.Revision <> SECURITY_DESCRIPTOR_REVISION then Exit;

 {Check Relative}
 if (pSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   {Check Owner}
   if PSecurityDescriptorRelative(pSecurityDescriptor).Owner > 0 then
    begin
     Sid:=PSID(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Owner);
     if not IsValidSid(Sid) then Exit;
    end;

   {Check Group}
   if PSecurityDescriptorRelative(pSecurityDescriptor).Group > 0 then
    begin
     Sid:=PSID(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Group);
     if not IsValidSid(Sid) then Exit;
    end;

   {Check DACL}
   if PSecurityDescriptorRelative(pSecurityDescriptor).Dacl > 0 then
    begin
     Acl:=PACL(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Dacl);
     if not IsValidAcl(Acl^) then Exit;
    end;

   {Check SACL}
   if PSecurityDescriptorRelative(pSecurityDescriptor).Sacl > 0 then
    begin
     Acl:=PACL(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Sacl);
     if not IsValidAcl(Acl^) then Exit;
    end;
  end
 else
  begin
   {Absolute Descriptor}
   {Check Owner}
   Sid:=PSID(pSecurityDescriptor.Owner);
   if Sid <> nil then
    begin
     if not IsValidSid(Sid) then Exit;
    end;

   {Check Group}
   Sid:=PSID(pSecurityDescriptor.Group);
   if Sid <> nil then
    begin
     if not IsValidSid(Sid) then Exit;
    end;

   {Check DACL}
   Acl:=pSecurityDescriptor.Dacl;
   if Acl <> nil then
    begin
     if not IsValidAcl(Acl^) then Exit;
    end;

   {Check SACL}
   Acl:=pSecurityDescriptor.Sacl;
   if Acl <> nil then
    begin
     if not IsValidAcl(Acl^) then Exit;
    end;
  end;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function GetSecurityDescriptorLength(pSecurityDescriptor: PSecurityDescriptor): DWORD;
var
 Acl:PACL;
 Sid:PSID;
 Present:BOOL;
 Defaulted:BOOL;
begin
 {}
 SetLastError(ERROR_SUCCESS);
 Result:=SECURITY_DESCRIPTOR_MIN_LENGTH;

 {Do not check for null Descriptor}

 {Get Owner}
 Sid:=nil;
 Defaulted:=False;
 if GetSecurityDescriptorOwner(pSecurityDescriptor,Sid,Defaulted) then
  begin
   if Sid <> nil then Result:=Result + GetLengthSid(Sid);
  end;

 {Get Group}
 Sid:=nil;
 Defaulted:=False;
 if GetSecurityDescriptorGroup(pSecurityDescriptor,Sid,Defaulted) then
  begin
   if Sid <> nil then Result:=Result + GetLengthSid(Sid);
  end;

 {Get DACL}
 Acl:=nil;
 Present:=False;
 Defaulted:=False;
 if GetSecurityDescriptorDacl(pSecurityDescriptor,Present,Acl,Defaulted) then
  begin
   if Acl <> nil then Result:=Result + Acl.AclSize;
  end;

 {Get SACL}
 Acl:=nil;
 Present:=False;
 Defaulted:=False;
 if GetSecurityDescriptorSacl(pSecurityDescriptor,Present,Acl,Defaulted) then
  begin
   if Acl <> nil then Result:=Result + Acl.AclSize;
  end;
end;

{==============================================================================}

function GetSecurityDescriptorControl(pSecurityDescriptor: PSecurityDescriptor; var pControl: SECURITY_DESCRIPTOR_CONTROL; var lpdwRevision: DWORD): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSecurityDescriptor = nil then Exit;

 {Get Revision}
 pControl:=pSecurityDescriptor.Control;

 {Get Control}
 lpdwRevision:=pSecurityDescriptor.Revision;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function SetSecurityDescriptorControl(pSecurityDescriptor: PSecurityDescriptor; ControlBitsOfInterest, ControlBitsToSet: SECURITY_DESCRIPTOR_CONTROL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function GetSecurityDescriptorDacl(pSecurityDescriptor: PSecurityDescriptor; var lpbDaclPresent: BOOL; var pDacl: PACL; var lpbDaclDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSecurityDescriptor = nil then Exit;

 {Check Present}
 lpbDaclPresent:=((pSecurityDescriptor.Control and SE_DACL_PRESENT) = SE_DACL_PRESENT);

 {Check Defaulted}
 lpbDaclDefaulted:=((pSecurityDescriptor.Control and SE_DACL_DEFAULTED) = SE_DACL_DEFAULTED);

 {Check Relative}
 if (pSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   pDacl:=nil;
   if PSecurityDescriptorRelative(pSecurityDescriptor).Dacl > 0 then
    begin
     pDacl:=PACL(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Dacl);
    end;
  end
 else
  begin
   {Absolute Descriptor}
   pDacl:=pSecurityDescriptor.Dacl;
  end;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function SetSecurityDescriptorDacl(pSecurityDescriptor: PSecurityDescriptor; bDaclPresent: BOOL; pDacl: PACL; bDaclDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function GetSecurityDescriptorSacl(pSecurityDescriptor: PSecurityDescriptor; var lpbSaclPresent: BOOL; var pSacl: PACL; var lpbSaclDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSecurityDescriptor = nil then Exit;

 {Check Present}
 lpbSaclPresent:=((pSecurityDescriptor.Control and SE_SACL_PRESENT) = SE_SACL_PRESENT);

 {Check Defaulted}
 lpbSaclDefaulted:=((pSecurityDescriptor.Control and SE_SACL_DEFAULTED) = SE_SACL_DEFAULTED);

 {Check Relative}
 if (pSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   pSacl:=nil;
   if PSecurityDescriptorRelative(pSecurityDescriptor).Sacl > 0 then
    begin
     pSacl:=PACL(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Sacl);
    end;
  end
 else
  begin
   {Absolute Descriptor}
   pSacl:=pSecurityDescriptor.Sacl;
  end;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function SetSecurityDescriptorSacl(pSecurityDescriptor: PSecurityDescriptor; bSaclPresent: BOOL; pSacl: PACL; bSaclDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function GetSecurityDescriptorOwner(pSecurityDescriptor: PSecurityDescriptor; var pOwner: PSID; var lpbOwnerDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSecurityDescriptor = nil then Exit;

 {Check Defaulted}
 lpbOwnerDefaulted:=((pSecurityDescriptor.Control and SE_OWNER_DEFAULTED) = SE_OWNER_DEFAULTED);

 {Check Relative}
 if (pSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   pOwner:=nil;
   if PSecurityDescriptorRelative(pSecurityDescriptor).Owner > 0 then
    begin
     pOwner:=PSID(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Owner);
    end;
  end
 else
  begin
   {Absolute Descriptor}
   pOwner:=pSecurityDescriptor.Owner;
  end;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function SetSecurityDescriptorOwner(pSecurityDescriptor: PSecurityDescriptor; pOwner: PSID; bOwnerDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function GetSecurityDescriptorGroup(pSecurityDescriptor: PSecurityDescriptor; var pGroup: PSID; var lpbGroupDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSecurityDescriptor = nil then Exit;

 {Check Defaulted}
 lpbGroupDefaulted:=((pSecurityDescriptor.Control and SE_GROUP_DEFAULTED) = SE_GROUP_DEFAULTED);

 {Check Relative}
 if (pSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   pGroup:=nil;
   if PSecurityDescriptorRelative(pSecurityDescriptor).Group > 0 then
    begin
     pGroup:=PSID(PtrUInt(pSecurityDescriptor) + PSecurityDescriptorRelative(pSecurityDescriptor).Group);
    end;
  end
 else
  begin
   {Absolute Descriptor}
   pGroup:=pSecurityDescriptor.Group;
  end;

 SetLastError(ERROR_SUCCESS);
 Result:=True;
end;

{==============================================================================}

function SetSecurityDescriptorGroup(pSecurityDescriptor: PSecurityDescriptor; pGroup: PSID; bGroupDefaulted: BOOL): BOOL;
begin
 {}
 Result:=False;

 //To Do
end;

{==============================================================================}

function MakeSelfRelativeSD(pAbsoluteSecurityDescriptor: PSecurityDescriptor; pSelfRelativeSecurityDescriptor: PSecurityDescriptor; var lpdwBufferLength: DWORD): BOOL;
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pAbsoluteSecurityDescriptor = nil then Exit;

 {Check Relative}
 if (pAbsoluteSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   {Check Size}
   SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
   Size:=GetSecurityDescriptorLength(pAbsoluteSecurityDescriptor);
   if lpdwBufferLength < Size then
    begin
     lpdwBufferLength:=Size;
     Exit;
    end;
   SetLastError(ERROR_INVALID_SECURITY_DESCR);
   if pSelfRelativeSecurityDescriptor = nil then Exit;

   {Copy Descriptor}
   System.Move(pAbsoluteSecurityDescriptor^,pSelfRelativeSecurityDescriptor^,Size);

   SetLastError(ERROR_SUCCESS);
   Result:=True;
  end
 else
  begin
   {Absolute Descriptor}
   {Check Size}
   SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
   Size:=GetSecurityDescriptorLength(pAbsoluteSecurityDescriptor);
   if lpdwBufferLength < Size then
    begin
     lpdwBufferLength:=Size;
     Exit;
    end;
   SetLastError(ERROR_INVALID_SECURITY_DESCR);
   if pSelfRelativeSecurityDescriptor = nil then Exit;

   {Convert Descriptor}
   Offset:=SECURITY_DESCRIPTOR_MIN_LENGTH;
   FillChar(pSelfRelativeSecurityDescriptor^,lpdwBufferLength,0);
   pSelfRelativeSecurityDescriptor.Revision:=pAbsoluteSecurityDescriptor.Revision;
   pSelfRelativeSecurityDescriptor.Control:=(pAbsoluteSecurityDescriptor.Control or SE_SELF_RELATIVE);

   {Convert Owner}
   if pAbsoluteSecurityDescriptor.Owner <> nil then
    begin
     PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Owner:=Offset;
     Size:=GetLengthSid(pAbsoluteSecurityDescriptor.Owner);
     System.Move(pAbsoluteSecurityDescriptor.Owner^,Pointer(PtrUInt(pSelfRelativeSecurityDescriptor) + PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Owner)^,Size);
     Inc(Offset,Size);
    end;

   {Convert Group}
   if pAbsoluteSecurityDescriptor.Group <> nil then
    begin
     PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Group:=Offset;
     Size:=GetLengthSid(pAbsoluteSecurityDescriptor.Group);
     System.Move(pAbsoluteSecurityDescriptor.Group^,Pointer(PtrUInt(pSelfRelativeSecurityDescriptor) + PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Group)^,Size);
     Inc(Offset,Size);
    end;

   {Convert Dacl}
   if pAbsoluteSecurityDescriptor.Dacl <> nil then
    begin
     PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Dacl:=Offset;
     Size:=pAbsoluteSecurityDescriptor.Dacl.AclSize;
     System.Move(pAbsoluteSecurityDescriptor.Dacl^,Pointer(PtrUInt(pSelfRelativeSecurityDescriptor) + PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Dacl)^,Size);
     Inc(Offset,Size);
    end;

   {Convert Sacl}
   if pAbsoluteSecurityDescriptor.Sacl <> nil then
    begin
     PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Sacl:=Offset;
     Size:=pAbsoluteSecurityDescriptor.Sacl.AclSize;
     System.Move(pAbsoluteSecurityDescriptor.Sacl^,Pointer(PtrUInt(pSelfRelativeSecurityDescriptor) + PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Sacl)^,Size);
     {Inc(Offset,Size);} {Not Required}
    end;

   SetLastError(ERROR_SUCCESS);
   Result:=True;
  end;
end;

{==============================================================================}

function MakeAbsoluteSD(pSelfRelativeSecurityDescriptor: PSecurityDescriptor; pAbsoluteSecurityDescriptor: PSecurityDescriptor; var lpdwAbsoluteSecurityDescriptorSi: DWORD; var pDacl: TACL; var lpdwDaclSize: DWORD; var pSacl: TACL; var lpdwSaclSize: DWORD; pOwner: PSID; var lpdwOwnerSize: DWORD; pPrimaryGroup: PSID; var lpdwPrimaryGroupSize: DWORD): BOOL;
var
 Acl:PACL;
 Sid:PSID;
 Size:DWORD;
 Present:BOOL;
 Defaulted:BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);

 if pSelfRelativeSecurityDescriptor = nil then Exit;

 {Check Relative}
 if (pSelfRelativeSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   {Check Size}
   SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
   Size:=SECURITY_DESCRIPTOR_MIN_LENGTH;
   if lpdwAbsoluteSecurityDescriptorSi < Size then
    begin
     lpdwAbsoluteSecurityDescriptorSi:=Size;
     Exit;
    end;
   SetLastError(ERROR_INVALID_SECURITY_DESCR);
   if pAbsoluteSecurityDescriptor = nil then Exit;

   {Convert Descriptor}
   FillChar(pAbsoluteSecurityDescriptor^,lpdwAbsoluteSecurityDescriptorSi,0);
   pAbsoluteSecurityDescriptor.Revision:=pSelfRelativeSecurityDescriptor.Revision;
   pAbsoluteSecurityDescriptor.Control:=(pSelfRelativeSecurityDescriptor.Control and not(SE_SELF_RELATIVE));

   {Convert Owner}
   if PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Owner > 0 then
    begin
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=GetLengthSid(PSID(PtrUInt(pSelfRelativeSecurityDescriptor) + PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Owner));
     if lpdwOwnerSize < Size then
      begin
       lpdwOwnerSize:=Size;
       Exit;
      end;
     SetLastError(ERROR_INVALID_SECURITY_DESCR);
     if pOwner = nil then Exit;
     Sid:=nil;
     Defaulted:=False;
     if not GetSecurityDescriptorOwner(pSelfRelativeSecurityDescriptor,Sid,Defaulted) then Exit;
     System.Move(Sid^,pOwner^,Size);
     pAbsoluteSecurityDescriptor.Owner:=pOwner;
    end;

   {Convert Group}
   if PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Group > 0 then
    begin
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=GetLengthSid(PSID(PtrUInt(pSelfRelativeSecurityDescriptor) + PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Group));
     if lpdwPrimaryGroupSize < Size then
      begin
       lpdwPrimaryGroupSize:=Size;
       Exit;
      end;
     SetLastError(ERROR_INVALID_SECURITY_DESCR);
     if pPrimaryGroup = nil then Exit;
     Sid:=nil;
     Defaulted:=False;
     if not GetSecurityDescriptorGroup(pSelfRelativeSecurityDescriptor,Sid,Defaulted) then Exit;
     System.Move(Sid^,pPrimaryGroup^,Size);
     pAbsoluteSecurityDescriptor.Group:=pPrimaryGroup;
    end;

   {Convert Dacl}
   if PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Dacl > 0 then
    begin
     SetLastError(ERROR_INVALID_SECURITY_DESCR);
     Acl:=nil;
     Present:=False;
     Defaulted:=False;
     if not GetSecurityDescriptorDacl(pSelfRelativeSecurityDescriptor,Present,Acl,Defaulted) then Exit;
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=Acl.AclSize;
     if lpdwDaclSize < Size then
      begin
       lpdwDaclSize:=Size;
       Exit;
      end;
     System.Move(Acl^,pDacl,Size);
     pAbsoluteSecurityDescriptor.Dacl:=@pDacl;
    end;

   {Convert Sacl}
   if PSecurityDescriptorRelative(pSelfRelativeSecurityDescriptor).Sacl > 0 then
    begin
     SetLastError(ERROR_INVALID_SECURITY_DESCR);
     Acl:=nil;
     Present:=False;
     Defaulted:=False;
     if not GetSecurityDescriptorSacl(pSelfRelativeSecurityDescriptor,Present,Acl,Defaulted) then Exit;
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=Acl.AclSize;
     if lpdwSaclSize < Size then
      begin
       lpdwSaclSize:=Size;
       Exit;
      end;
     System.Move(Acl^,pSacl,Size);
     pAbsoluteSecurityDescriptor.Sacl:=@pSacl;
    end;

   SetLastError(ERROR_SUCCESS);
   Result:=True;
  end
 else
  begin
   {Absolute Descriptor}
   {Check Size}
   SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
   Size:=SECURITY_DESCRIPTOR_MIN_LENGTH;
   if lpdwAbsoluteSecurityDescriptorSi < Size then
    begin
     lpdwAbsoluteSecurityDescriptorSi:=Size;
     Exit;
    end;
   SetLastError(ERROR_INVALID_SECURITY_DESCR);
   if pAbsoluteSecurityDescriptor = nil then Exit;

   {Copy Descriptor}
   System.Move(pSelfRelativeSecurityDescriptor^,pAbsoluteSecurityDescriptor^,Size);

   {Copy Owner}
   if pSelfRelativeSecurityDescriptor.Owner <> nil then
    begin
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=GetLengthSid(pSelfRelativeSecurityDescriptor.Owner);
     if lpdwOwnerSize < Size then
      begin
       lpdwOwnerSize:=Size;
       Exit;
      end;
     SetLastError(ERROR_INVALID_SECURITY_DESCR);
     if pOwner = nil then Exit;
     System.Move(pSelfRelativeSecurityDescriptor.Owner^,pOwner^,Size);
     pAbsoluteSecurityDescriptor.Owner:=pOwner;
    end;

   {Copy Group}
   if pSelfRelativeSecurityDescriptor.Group <> nil then
    begin
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=GetLengthSid(pSelfRelativeSecurityDescriptor.Group);
     if lpdwPrimaryGroupSize < Size then
      begin
       lpdwPrimaryGroupSize:=Size;
       Exit;
      end;
     SetLastError(ERROR_INVALID_SECURITY_DESCR);
     if pPrimaryGroup = nil then Exit;
     System.Move(pSelfRelativeSecurityDescriptor.Group^,pPrimaryGroup^,Size);
     pAbsoluteSecurityDescriptor.Group:=pPrimaryGroup;
    end;

   {Copy Dacl}
   if pSelfRelativeSecurityDescriptor.Dacl <> nil then
    begin
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=pSelfRelativeSecurityDescriptor.Dacl.AclSize;
     if lpdwDaclSize < Size then
      begin
       lpdwDaclSize:=Size;
       Exit;
      end;
     System.Move(pSelfRelativeSecurityDescriptor.Dacl^,pDacl,Size);
     pAbsoluteSecurityDescriptor.Dacl:=@pDacl;
    end;

   {Copy Sacl}
   if pSelfRelativeSecurityDescriptor.Sacl <> nil then
    begin
     SetLastError(ERROR_INSUFFICIENT_BUFFER); {Also could be STATUS_BUFFER_TOO_SMALL}
     Size:=pSelfRelativeSecurityDescriptor.Sacl.AclSize;
     if lpdwSaclSize < Size then
      begin
       lpdwSaclSize:=Size;
       Exit;
      end;
     System.Move(pSelfRelativeSecurityDescriptor.Sacl^,pSacl,Size);
     pAbsoluteSecurityDescriptor.Sacl:=@pSacl;
    end;

   SetLastError(ERROR_SUCCESS);
   Result:=True;
  end;
end;

{==============================================================================}

function MakeAbsoluteSD2(pSelfRelativeSecurityDescriptor: PSecurityDescriptor; var lpdwBufferSize: DWORD): BOOL;
begin
 {}
 Result:=False;

 //To Do //Not sure of the implementation of this one - Some info on ReactOS web site
               //http://doxygen.reactos.org/
               //This appears to be an inplace conversion to Absolute //Simple enough
               //Change Control flags //Change Offsets to Pointers etc
end;

{==============================================================================}
{==============================================================================}
{Security Helper Functions}
function SplitStringSid(const StringSid:String):TStringList;
var
 Count:Integer;
 WorkBuffer:String;
begin
 {}
 Result:=TStringList.Create;
 try
  WorkBuffer:='';
  for Count:=1 to Length(StringSid) do
   begin
    if StringSid[Count] = '-' then
     begin
      if Length(WorkBuffer) > 0 then Result.Add(WorkBuffer);
      WorkBuffer:='';
     end
    else
     begin
      WorkBuffer:=WorkBuffer + StringSid[Count];
     end;
   end;
  if Length(WorkBuffer) > 0 then Result.Add(WorkBuffer);
 except
  {}
 end;
end;

{==============================================================================}

function CreateDefaultSid(var pCreatedSid: PSID): BOOL;
var
 Sid:PSID;
 SidSize:LongWord;
begin
 {}
 Result:=False;
 if pCreatedSid <> nil then Exit;
 {Create Sid}
 Sid:=AllocMem(SECURITY_MAX_SID_SIZE);
 try
  SidSize:=SECURITY_MAX_SID_SIZE;
  if not CreateWellKnownSid(WinWorldSid,nil,Sid,SidSize) then Exit;
  {Copy Sid}
  pCreatedSid:=AllocMem(SidSize);
  try
   if not CopySid(SidSize,pCreatedSid,Sid) then Exit;
   Result:=True;
  finally
   if not Result then FreeMem(pCreatedSid);
   if not Result then pCreatedSid:=nil;
  end;
 finally
  FreeMem(Sid);
 end;
end;

{==============================================================================}

function DestroyDefaultSid(pDefaultSid: PSID): BOOL;
begin
 {}
 Result:=False;
 if pDefaultSid = nil then Exit;
 FreeMem(pDefaultSid);
 {LocalFree(THandle(pDefaultSid));}
 Result:=True;
end;

{==============================================================================}

function CreateDefaultSecurityDescriptor(var pCreatedSecurityDescriptor: PSecurityDescriptor; bFolder: BOOL): BOOL;
var
 Acl:PACL;
 Sid:PSID;
 Ace:PAceHeader;

 Count:Integer;
 Offset:LongWord;
 SidSize:LongWord;

 Descriptor:PSecurityDescriptorRelative;
 WellKnownDescriptor:PWellKnownDescriptor;
begin
 {}
 Result:=False;
 if pCreatedSecurityDescriptor <> nil then Exit;
 {Check Type}
 if bFolder then
  begin
   WellKnownDescriptor:=@WellKnownDescriptorFolder;
  end
 else
  begin
   WellKnownDescriptor:=@WellKnownDescriptorFile;
  end;
 {Create Descriptor}
 pCreatedSecurityDescriptor:=AllocMem(WellKnownDescriptor.Size);
 try
  Descriptor:=PSecurityDescriptorRelative(pCreatedSecurityDescriptor);
  Descriptor.Revision:=WellKnownDescriptor.Revision;
  Descriptor.Control:=WellKnownDescriptor.Control;
  Descriptor.Owner:=WellKnownDescriptor.OwnerOffset;
  Descriptor.Group:=WellKnownDescriptor.GroupOffset;
  Descriptor.Sacl:=WellKnownDescriptor.SaclOffset;
  Descriptor.Dacl:=WellKnownDescriptor.DaclOffset;
  {Check Owner}
  if WellKnownDescriptor.OwnerOffset <> 0 then
   begin
    {Get Sid}
    Sid:=PSID(PtrUInt(Descriptor) + WellKnownDescriptor.OwnerOffset);
    SidSize:=SECURITY_MAX_SID_SIZE;
    if not CreateWellKnownSid(WellKnownDescriptor.Owner,nil,Sid,SidSize) then Exit;
   end;
  {Check Group}
  if WellKnownDescriptor.GroupOffset <> 0 then
   begin
    {Get Sid}
    Sid:=PSID(PtrUInt(Descriptor) + WellKnownDescriptor.GroupOffset);
    SidSize:=SECURITY_MAX_SID_SIZE;
    if not CreateWellKnownSid(WellKnownDescriptor.Group,nil,Sid,SidSize) then Exit;
   end;
  {Check Sacl}
  if WellKnownDescriptor.SaclOffset <> 0 then
   begin
    {Get Acl}
    Acl:=PACL(PtrUInt(Descriptor) + WellKnownDescriptor.SaclOffset);
    Acl.AclRevision:=WellKnownDescriptor.Sacl.AclRevision;
    Acl.AclSize:=WellKnownDescriptor.Sacl.AclSize;
    Acl.AceCount:=WellKnownDescriptor.Sacl.AceCount;
    {Get Offset}
    Offset:=SizeOf(TACL);
    {Check Aces}
    for Count:=0 to WellKnownDescriptor.Sacl.AceCount - 1 do
     begin
      {Get Offset}
      if Count > 0 then Inc(Offset,WellKnownDescriptor.Sacl.Aces[Count - 1].AceSize);
      {Get Ace}
      Ace:=PAceHeader(PtrUInt(Acl) + Offset);
      Ace.AceType:=WellKnownDescriptor.Sacl.Aces[Count].AceType;
      Ace.AceFlags:=WellKnownDescriptor.Sacl.Aces[Count].AceFlags;
      Ace.AceSize:=WellKnownDescriptor.Sacl.Aces[Count].AceSize;
      PAccessAllowedAce(Ace).Mask:=WellKnownDescriptor.Sacl.Aces[Count].Mask;
      {Get Sid}
      Sid:=PSID(PtrUInt(Ace) + SizeOf(TAceHeader) + SizeOf(ACCESS_MASK));
      SidSize:=SECURITY_MAX_SID_SIZE;
      if not CreateWellKnownSid(WellKnownDescriptor.Sacl.Aces[Count].Sid,nil,Sid,SidSize) then Exit;
     end;
   end;
  {Check Dacl}
  if WellKnownDescriptor.DaclOffset <> 0 then
   begin
    {Get Acl}
    Acl:=PACL(PtrUInt(Descriptor) + WellKnownDescriptor.DaclOffset);
    Acl.AclRevision:=WellKnownDescriptor.Dacl.AclRevision;
    Acl.AclSize:=WellKnownDescriptor.Dacl.AclSize;
    Acl.AceCount:=WellKnownDescriptor.Dacl.AceCount;
    {Get Offset}
    Offset:=SizeOf(TACL);
    {Check Aces}
    for Count:=0 to WellKnownDescriptor.Dacl.AceCount - 1 do
     begin
      {Get Offset}
      if Count > 0 then Inc(Offset,WellKnownDescriptor.Dacl.Aces[Count - 1].AceSize);
      {Get Ace}
      Ace:=PAceHeader(PtrUInt(Acl) + Offset);
      Ace.AceType:=WellKnownDescriptor.Dacl.Aces[Count].AceType;
      Ace.AceFlags:=WellKnownDescriptor.Dacl.Aces[Count].AceFlags;
      Ace.AceSize:=WellKnownDescriptor.Dacl.Aces[Count].AceSize;
      PAccessAllowedAce(Ace).Mask:=WellKnownDescriptor.Dacl.Aces[Count].Mask;
      {Get Sid}
      Sid:=PSID(PtrUInt(Ace) + SizeOf(TAceHeader) + SizeOf(ACCESS_MASK));
      SidSize:=SECURITY_MAX_SID_SIZE;
      if not CreateWellKnownSid(WellKnownDescriptor.Dacl.Aces[Count].Sid,nil,Sid,SidSize) then Exit;
     end;
   end;
  Result:=True;
 finally
  if not Result then FreeMem(pCreatedSecurityDescriptor);
  if not Result then pCreatedSecurityDescriptor:=nil;
 end;
end;

{==============================================================================}

function DestroyDefaultSecurityDescriptor(pDefaultSecurityDescriptor: PSecurityDescriptor): BOOL;
begin
 {}
 Result:=False;
 if pDefaultSecurityDescriptor = nil then Exit;
 FreeMem(pDefaultSecurityDescriptor);
 {LocalFree(THandle(pDefaultSecurityDescriptor));}
 Result:=True;
end;

{==============================================================================}

function CreateInheritedSecurityDescriptorNT(pParentSecurityDescriptor: PSecurityDescriptor; var pCreatedSecurityDescriptor: PSecurityDescriptor): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);
 if pParentSecurityDescriptor = nil then Exit;
 if pCreatedSecurityDescriptor <> nil then Exit;

 {Temporary Behaviour}
 Result:=CreateInheritedSecurityDescriptor2K(pParentSecurityDescriptor,pCreatedSecurityDescriptor);
end;

{==============================================================================}

function CreateInheritedSecurityDescriptor2K(pParentSecurityDescriptor: PSecurityDescriptor; var pCreatedSecurityDescriptor: PSecurityDescriptor): BOOL;
var
 Size:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);
 if pParentSecurityDescriptor = nil then Exit;
 if pCreatedSecurityDescriptor <> nil then Exit;

 {Temporary Behaviour}
 {Check Relative}
 if (pParentSecurityDescriptor.Control and SE_SELF_RELATIVE) = SE_SELF_RELATIVE then
  begin
   {Relative Descriptor}
   {Create Descriptor}
   Size:=GetSecurityDescriptorLength(pParentSecurityDescriptor);
   pCreatedSecurityDescriptor:=AllocMem(Size);
   {Copy Descriptor}
   System.Move(pParentSecurityDescriptor^,pCreatedSecurityDescriptor^,Size);
   SetLastError(ERROR_SUCCESS);
   Result:=True;
  end
 else
  begin
   {Absolute Descriptor}
   {Create Descriptor}
   Size:=GetSecurityDescriptorLength(pParentSecurityDescriptor);
   pCreatedSecurityDescriptor:=AllocMem(Size);
   {Convert Descriptor}
   Offset:=SECURITY_DESCRIPTOR_MIN_LENGTH;
   pCreatedSecurityDescriptor.Revision:=pParentSecurityDescriptor.Revision;
   pCreatedSecurityDescriptor.Control:=(pParentSecurityDescriptor.Control or SE_SELF_RELATIVE);
   {Convert Owner}
   if pParentSecurityDescriptor.Owner <> nil then
    begin
     PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Owner:=Offset;
     Size:=GetLengthSid(pParentSecurityDescriptor.Owner);
     System.Move(pParentSecurityDescriptor.Owner^,Pointer(PtrUInt(pCreatedSecurityDescriptor) + PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Owner)^,Size);
     Inc(Offset,Size);
    end;
   {Convert Group}
   if pParentSecurityDescriptor.Group <> nil then
    begin
     PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Group:=Offset;
     Size:=GetLengthSid(pParentSecurityDescriptor.Group);
     System.Move(pParentSecurityDescriptor.Group^,Pointer(PtrUInt(pCreatedSecurityDescriptor) + PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Group)^,Size);
     Inc(Offset,Size);
    end;
   {Convert Dacl}
   if pParentSecurityDescriptor.Dacl <> nil then
    begin
     PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Dacl:=Offset;
     Size:=pParentSecurityDescriptor.Dacl.AclSize;
     System.Move(pParentSecurityDescriptor.Dacl^,Pointer(PtrUInt(pCreatedSecurityDescriptor) + PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Dacl)^,Size);
     Inc(Offset,Size);
    end;
   {Convert Sacl}
   if pParentSecurityDescriptor.Sacl <> nil then
    begin
     PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Sacl:=Offset;
     Size:=pParentSecurityDescriptor.Sacl.AclSize;
     System.Move(pParentSecurityDescriptor.Sacl^,Pointer(PtrUInt(pCreatedSecurityDescriptor) + PSecurityDescriptorRelative(pCreatedSecurityDescriptor).Sacl)^,Size);
     {Inc(Offset,Size);} {Not Required}
    end;
   SetLastError(ERROR_SUCCESS);
   Result:=True;
  end;
end;

{==============================================================================}

function CreateMergedSecurityDescriptor2K(pParentSecurityDescriptor, pChildSecurityDescriptor: PSecurityDescriptor; var pCreatedSecurityDescriptor: PSecurityDescriptor): BOOL;
begin
 {}
 Result:=False;
 SetLastError(ERROR_INVALID_SECURITY_DESCR);
 if pParentSecurityDescriptor = nil then Exit;
 if pChildSecurityDescriptor = nil then Exit;
 if pCreatedSecurityDescriptor <> nil then Exit;

 {Temporary Behaviour}
 Result:=CreateInheritedSecurityDescriptor2K(pParentSecurityDescriptor,pCreatedSecurityDescriptor);
end;

{==============================================================================}

function DestroyInheritedSecurityDescriptor(pInheritedSecurityDescriptor: PSecurityDescriptor): BOOL;
begin
 {}
 Result:=False;
 if pInheritedSecurityDescriptor = nil then Exit;

 {Default Behaviour}
 FreeMem(pInheritedSecurityDescriptor);
end;

{==============================================================================}

function DestroyMergedSecurityDescriptor(pMergedSecurityDescriptor: PSecurityDescriptor): BOOL;
begin
 {}
 Result:=False;
 if pMergedSecurityDescriptor = nil then Exit;

 {Default Behaviour}
 FreeMem(pMergedSecurityDescriptor);
end;

{==============================================================================}
{==============================================================================}

end.
