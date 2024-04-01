// prime finder: (c) 2024 by Jens Kallup
// all rights reserved.
{$mode delphi}
{$APPTYPE CONSOLE}
program isprime;

uses
  SysUtils, Int128;

var number: String;

function check_prime(nst: String): Boolean;
label check;
var
  start, index, gap: UInt128;
  i: Integer;
  last, len: LongInt;
begin
  start := 1;
  index := 1;
  gap   := 0;
  
  result := false;
  
  if StrToInt128(Trim(nst)) = 1 then begin WriteLn('1: is not prime'); result := false; exit; end else
  if StrToInt128(Trim(nst)) = 2 then begin WriteLn('index(1) = 2'   ); result := true ; exit; end;
  
  // -----------------------------------------------
  // evtl. Leerzeichen bei der Eingabe entsorgen ...
  // -----------------------------------------------
  nst := StringReplace(nst,' ','',[rfReplaceAll]);
  
  // -------------------------------------------------------------
  // alle 23-Zwillinge, die "keine" Primzahlen sind entsorgen ...
  // -------------------------------------------------------------
  if (Length(nst) mod 2) = 0 then begin
    len := 1;
    if nst = '23' then begin
      if Pos('23', nst, Length(nst) - len) = 0 then begin
        result := true;
        WriteLn('Index(9) is prime: 23');
        exit;
      end else
      if Pos('23', nst, Length(nst) - len) > 0 then begin
        while true do begin
          if len = 0 then exit;
          len := len - 2;
          if Pos('23', nst, Length(nst) - len) = 0 then begin
            WriteLn(nst + ': is not prime (23. twins)');
            exit;
          end;
        end;
      end;
    end;
  end;

  if Pos('0', nst, Length(nst) - 1) > 0 then begin
    WriteLn(nst + ': is not prime');
    result := false;
    exit;
  end else
  if Pos('1', nst, Length(nst) - 1) > 0 then begin
    if Length(nst) > 1 then begin
      // todo
    end else begin
      WriteLn('Index(2) is prime: 3');
      result := true;
      exit;
    end;
  end else
  if Pos('2', nst, Length(nst) - 1) > 0 then begin
    if Length(nst) > 1 then begin
      WriteLn(nst + ': is not prime');
      result := false;
      exit;
    end else begin
      WriteLn('Index(1) is prime: 2');
      result := true;
      exit;
    end;
  end else
  if Pos('3', nst, Length(nst) - 1) > 0 then begin
    if Length(nst) > 1 then begin
      // todo
    end else begin
      WriteLn('Index(2) is prime: 3');
      result := true;
      exit;
    end;
  end else
  if Pos('4', nst, Length(nst) - 1) > 0 then begin
    WriteLn(nst + ': is not prime');
    result := false;
    exit;
  end else
  if Pos('5', nst, Length(nst) - 1) > 0 then begin
    if Length(nst) > 1 then begin
      WriteLn(nst + ': is not prime');
      result := false;
      exit;
    end else begin
      WriteLn('Index(3) is prime: 5');
      result := true;
      exit;
    end;
  end;
  if Pos('6', nst, Length(nst) - 1) > 0 then begin
    WriteLn(nst + ': is not prime');
    result := false;
    exit;
  end else
  if Pos('7', nst, Length(nst) - 1) > 0 then begin
    if Length(nst) > 1 then begin
      // todo
    end else begin
      WriteLn('Index(4) is prime: 7');
      result := true;
      exit;
    end;
  end else
  if Pos('8', nst, Length(nst) - 1) > 0 then begin
    WriteLn(nst + ': is not prime');
    result := false;
    exit;
  end else
  if Pos('9', nst, Length(nst) - 1) > 0 then begin
    if Length(nst) > 1 then begin
      // todo
    end else begin
      WriteLn(nst + ': is not prime');
      result := false;
      exit;
    end;
  end;
  
  // ------------------------
  // erste Primzahl holen ...
  // ------------------------
  while true do
  begin
    if (StrToInt128(nst) mod StrToInt128(Int128ToStr(start))) = StrToInt128('1') then
    begin
      WriteLn(Format('%s: is prime.', [nst]));
      gap := gap + 1;
      break;
    end else begin
      WriteLn(Format('%s: is not prime.', [nst]));
      gap := gap + 1;
      break;
    end;
    start := start + 1;
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