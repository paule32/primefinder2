// ----------------------------------------------------------------
// prime finder: (c) 2024 by Jens Kallup
// all rights reserved.
//
// only for education !
// commercial use not allowed !
// ----------------------------------------------------------------
{$mode delphi}
{$APPTYPE CONSOLE}
program isprime;

uses
  SysUtils, Int128;

// ----------------------------------------------------------------
// @brief global used variables and constants ...
// ----------------------------------------------------------------
var
  number: String;
  dbmode: Boolean = false;

// ----------------------------------------------------------------
// @brief  check, if the string nst have all c character are equal.
// @paramd nst: String  -  search string
//         c  : Char    -  search pattern
// @return boolean
//
// the return value will be true, if all chars are equal. else the
// value will be false.
// ----------------------------------------------------------------
function check_equal(nst: String; c: Char): Boolean;
var
  len: LongInt;  // length of: nst
begin
  result := false;
  len    := 1;
  while true do begin
    if len > Length(nst) then break;
    if not (nst[len] = c) then begin
      result := false;
      exit;
    end;
    inc(len);
  end;
end;

// ----------------------------------------------------------------
// @brief dummy to check the goven parameter. todo !
// ----------------------------------------------------------------
function check_dummy(nst: String; nc: LongInt): Boolean;
begin
  result := false;
  case nc of
    1: begin if check_equal(nst, '1') then result := true; end;
    2: begin if check_equal(nst, '2') then result := true; end;
    3: begin if check_equal(nst, '3') then result := true; end;
    4: begin if check_equal(nst, '4') then result := true; end;
    5: begin if check_equal(nst, '5') then result := true; end;
    6: begin if check_equal(nst, '6') then result := true; end;
    7: begin if check_equal(nst, '7') then result := true; end;
    8: begin if check_equal(nst, '8') then result := true; end;
    9: begin if check_equal(nst, '9') then result := true; end;
  end;
end;

// ----------------------------------------------------------------
// @breif optimized for UInt128 (from wikipedia.org) ...
// ----------------------------------------------------------------
function integer_sqrt(y: UInt128): UInt128;
var
  L, a, d: Uint128;
begin
  L := 0;
  a := 1;
  d := 3;
  while (a <= y) do begin
    a := a + d;
    d := d + 2;
    L := L + 1;
  end;
  result := L;
end;

// ----------------------------------------------------------------
// @brief check nst if it is a prime, or not.
// @params nst: String  -  prime number to check
// @return boolean
//
// the return value will be false, if it is not prime, else return
// false.
// ----------------------------------------------------------------
function check_prime(nst: String): Boolean;
var
  suche, index, check, i, j, ionez, even, m, prime: UInt128;
  lastIndex, lastPrime: UInt128;
  len, b: LongInt;
begin
  // -------------------------------------------------------------
  // pre-initinitalize the result of this function:
  // -------------------------------------------------------------
  result := false;
  
  // -------------------------------------------------------------
  // clearify input string with remove the empty whitespaces ...
  // -------------------------------------------------------------
  nst := StringReplace(nst,' ','',[rfReplaceAll]);
  
  // -------------------------------------------------------------
  // sanity checks (not all currently), to improve calc security:
  // -------------------------------------------------------------
  if (Pos('-', nst, 1) > 0) or (Pos('+', nst, 1) > 0)
  or (Pos('*', nst, 1) > 0) or (Pos('/', nst, 1) > 0)
  or (Pos('%', nst, 1) > 0) or (Pos('$', nst, 1) > 0) or (Length(nst) < 1) then begin
    WriteLn('the input value is not valid.');
    Halt(1);
  end;

  if nst = '0' then begin WriteLn('0: is not prime'); result := false; exit; end else
  if nst = '1' then begin WriteLn('1: is not prime'); result := false; exit; end else
  
  if nst = '2' then begin WriteLn('Index(1) for 2: is a prime.'); result := false; exit; end else
  if nst = '3' then begin WriteLn('Index(2) for 3: is a prime.'); result := false; exit; end;
  
  // -------------------------------------------------------------
  // check if prime have twins, if so, then it is not a prime.
  // todo: complete check other prime twins ...
  // -------------------------------------------------------------
  if Length(nst) > 2 then begin
    result := true;
    for b := StrToInt('0') to StrToInt('9') do begin
      if result = false then begin
        WriteLn(Format('%s: is no prime, it has twins.',[nst]));
        Halt(1);
      end else
      if check_dummy(nst, b) then result := false;
    end;
  end;
  
  // -------------------------------------------------------------
  // here, we check if the given prime has a length > 1, and if it
  // is a prime. So, 23 is prime, result will return true.
  // -------------------------------------------------------------
  if (Length(nst) mod 2) = 1 then begin
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

  // -------------------------------------------------------------
  // post-initialize used variables ...
  // -------------------------------------------------------------
  suche := StrToInt128(nst);

  check     := integer_sqrt(suche);
  index     := 1;
  i         := 2;
  j         := 1;
  
  prime     := 2;
  lastIndex := 1;
  lastPrime := 2;

  // -------------------------------------------------------------
  // here, we use a while loop, because the for loop of pascal do
  // not have advantages for big numbers. So, it coukd be result
  // into a exception of range checking in context of LongInt and
  // the resulting 64-Bit length ...
  // -------------------------------------------------------------
  while true do begin
    if i >= suche then break;
    j := i * i;
    even := 1;
    while j <= suche do begin
      m := ((even mod i));
      if dbmode then begin
        WriteLn(Format('--> idx: %s  m: %s  j: %s', [
        Int128ToStr(index), Int128ToStr(m), Int128ToStr(j) ]));
      end;
      if m = 0 then begin
        index := index + 1;
        prime  := j;
        if j >= suche then begin
          WriteLn(Format('Index(%s) for: %s is a prime.', [
          Int128ToStr(index + 1), nst ]));
          result := true;
          break;
        end;
      end else begin
        lastIndex := index;
        lastPrime := prime;
        if j >= suche then begin
          WriteLn(Format('no Index for: %s is not prime.', [nst]));
          result := false;
          break;
        end;
      end;
      j := j + 1;
      even := even + 1;
    end;
    if i >= check then break;
    if j >= suche then break;
    i := i + 1;
  end;
end;

// ----------------------------------------------------------------
// @brief entry point procedure for our application. tested and
//        compiled with fpc (free pascal compiler) version: 3.2.2
//        64-Bit under Windows 10 64-Bit, 4 GB of RAM.
// ----------------------------------------------------------------
begin
  try
    dbmode := false;

    // -------------------------------------------------------------
    // print a nice banner on the screen. Please be so fair and hold
    // you on the CODE OF CONDUCT codex - Thank you.
    // -------------------------------------------------------------
    WriteLn('primefind.exe 1.0 (c) 2024 Jens Kallup - paule32');
    WriteLn('all rights reserved.');
    WriteLn;
    WriteLn('only for education purpose, and non-profit.');
    WriteLn('commerical use is not allowed !');
    WriteLn;
    
    // -------------------------------------------------------------
    // you can pass application parameter - if you would like:
    // d => debug mode - to display progress (slow)
    // n => n for prime to pass as start prime (currently no check
    // of other chars than 0..9.
    // -------------------------------------------------------------
    if ParamCount() > 0 then begin
      if ParamStr(1) = 'd' then begin
        dbmode := true;
      end else begin
        check_prime(ParamStr(1));
        exit;
      end;
    end;
    
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
