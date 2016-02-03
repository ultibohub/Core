{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Free Pascal development team

    Low level OS functions for Ultibo
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
{$define HAS_GETCPUCOUNT}
function GetCPUCount: LongWord;
begin
  Result := SystemCPUCount;
end; 