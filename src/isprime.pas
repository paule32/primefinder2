// prime finder: (c) 2024 by Jens Kallup
// all rights reserved.
{$mode delphi}
{$APPTYPE CONSOLE}
program isprime;

uses
  SysUtils, Int128;

var number: String;

function check_prime(nst: String): Boolean;
var
  start, ende: UInt128;
begin
  start := StrToInt128('2');
  ende  := StrToInt128(nst);
  
  result := false;
  
  if Trim(nst) = '1' then begin WriteLn('1: is not prime'); result := false; exit; end;
  if Trim(nst) = '2' then begin WriteLn('2: is prime'    ); result := true ; exit; end;
  
  while true do
  begin
    if StrToInt128(Int128ToStr(start)) > ende then
	break;
	
	if StrToInt128(nst) mod StrToInt128(Int128ToStr(start)) = StrToInt128('0') then
	begin
	  result := false;
	  WriteLn(nst + ': is not prime');
	  exit;
	end else
	begin
	  result := true;
	  WriteLn(nst + ': is prime');
	  exit;
	end;
	
	start := start + StrToInt128('1');
  end;
end;

begin
  try
    WriteLn('primefind.exe 1.0 (c) 2024 Jens Kallup - paule32');
    WriteLn('all rights reserved.');
    WriteLn;
    WriteLn('only for education purpose, and non-profit.');
    WriteLn('commerical use is not allowed !');
    WriteLn;
    Write('Enter number: ');
	ReadLn(number);
    check_prime(number);
  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
    end;
  end;
end.