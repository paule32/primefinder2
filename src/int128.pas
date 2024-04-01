unit Int128;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  UInt128 = packed record
    case byte of
    0: (dw0,dw1,dw2,dw3: longword);
    1: (dw: packed array[0..3] of longword);
  end;

function ToUInt128(value: longword): UInt128;
function ToUInt128(value: QWord): UInt128;
operator := (const Value: LongWord): UInt128;
operator := (const Value: QWord): UInt128;
operator shl (const Value: UInt128; shift: integer): UInt128;
operator shl (const Value: UInt128; shift: UInt128): UInt128;
operator shr (const Value: UInt128; shift: integer): UInt128;
operator shr (const Value: UInt128; shift: UInt128): UInt128;
operator = (const Value1: UInt128; const Value2: UInt128): boolean;
operator = (const Value1: UInt128; const Value2: LongWord): boolean;
operator = (const Value1: UInt128; const Value2: QWord): boolean;
operator = (const Value1: UInt128; const Value2: integer): boolean;
operator > (const Value1: UInt128; const Value2: UInt128): boolean;
operator < (const Value1: UInt128; const Value2: UInt128): boolean;
operator >= (const Value1: UInt128; const Value2: UInt128): boolean;
operator <= (const Value1: UInt128; const Value2: UInt128): boolean;
operator * (const Value1: UInt128; const Value2: UInt128): UInt128;
operator * (const Value1: UInt128; const Value2: LongWord): UInt128;
operator * (const Value1: UInt128; const Value2: QWord): UInt128;
operator + (const Value1: UInt128; const Value2: UInt128): UInt128;
operator + (const Value1: UInt128; const Value2: LongWord): UInt128;
operator + (const Value1: UInt128; const Value2: QWord): UInt128;
operator + (const Value1: UInt128; const Value2: integer): UInt128;
operator - (const Value1: UInt128; const Value2: UInt128): UInt128;
operator - (const Value1: UInt128; const Value2: LongWord): UInt128;
operator - (const Value1: UInt128; const Value2: QWord): UInt128;
operator - (const Value1: UInt128; const Value2: integer): UInt128;
procedure Inc128(var Value: UInt128);
function StrToInt128(text: string): UInt128;
function Int128ToStr(const value: UInt128): string;
function Int128ToBin(const Value: UInt128): string;
function Int128ToHex(const Value: UInt128): string;
function Power128(const Value: UInt128; const Exponent: UInt128): UInt128;
function Power128(const Value: UInt128; Exponent: LongWord): UInt128;
function Power128(const Value: UInt128; const Exponent: QWord): UInt128;
procedure SetBit128(var Value: UInt128; numBit: integer);
procedure UnsetBit128(var Value: UInt128; numBit: integer);
function GetBit128(const Value: UInt128; numBit: integer): integer;
procedure ToggleBit128(var Value: UInt128; numBit: integer);
procedure DivMod128(const Value1: UInt128; const Value2: UInt128;
     out DivResult : UInt128; out Remainder: UInt128);
operator div (const Value1: UInt128; const Value2: UInt128): UInt128;
operator mod (const Value1: UInt128; const Value2: UInt128): UInt128;
operator not (const Value: UInt128): UInt128;
operator or (const Value1: UInt128; const Value2: UInt128): UInt128;
operator xor (const Value1: UInt128; const Value2: UInt128): UInt128;
operator and (const Value1: UInt128; const Value2: UInt128): UInt128;

implementation

function ToUInt128(value: longword): UInt128;
begin
  result.dw0 := value;
  result.dw1 := 0;
  result.dw2 := 0;
  result.dw3 := 0;
end;

function ToUInt128(value: QWord): UInt128;
begin
  result.dw0 := value and $ffffffff;
  result.dw1 := value shr 32;
  result.dw2 := 0;
  result.dw3 := 0;
end;

operator:=(const Value: LongWord): UInt128;
begin
  result := ToUInt128(Value);
end;

operator:=(const Value: QWord): UInt128;
begin
  result := ToUInt128(Value);
end;

operator shl(const Value: UInt128; shift: integer): UInt128;
begin
  if shift >= 128 then
    result := 0
  else if shift >= 64 then
  begin
    result.dw2 := Value.dw0;
    result.dw3 := Value.dw1;
    result.dw1 := 0;
    result.dw0 := 0;
    result := result shl (shift - 64);
  end else if shift >= 32 then
  begin
    result.dw3 := Value.dw2;
    result.dw2 := Value.dw1;
    result.dw1 := Value.dw0;
    result.dw0 := 0;
    result := result shl (shift - 32);
  end else if shift = 0 then
    result := Value else
  if shift < 0 then
    result := Value shr (-shift)
  else
  begin
    result.dw3 := (Value.dw3 shl shift) or (Value.dw2 shr (32-shift));
    result.dw2 := (Value.dw2 shl shift) or (Value.dw1 shr (32-shift));
    result.dw1 := (Value.dw1 shl shift) or (Value.dw0 shr (32-shift));
    result.dw0 := (Value.dw0 shl shift);
  end;
end;

operator shl(const Value: UInt128; shift: UInt128): UInt128;
begin
  if (shift.dw0 > 128) or (shift.dw1 <>0) or (shift.dw2 <>0)
    or (shift.dw3 <> 0) then result := 0 else
    result := Value shl shift.dw0;
end;

operator shr(const Value: UInt128; shift: integer): UInt128;
begin
  if shift >= 128 then
    result := 0
  else if shift >= 64 then
  begin
    result.dw0 := Value.dw2;
    result.dw1 := Value.dw3;
    result.dw2 := 0;
    result.dw3 := 0;
    result := result shr (shift - 64);
  end else if shift >= 32 then
  begin
    result.dw0 := Value.dw1;
    result.dw1 := Value.dw2;
    result.dw2 := Value.dw3;
    result.dw3 := 0;
    result := result shr (shift - 32);
  end else if shift = 0 then
    result := Value else
  if shift < 0 then
    result := Value shl (-shift)
  else
  begin
    result.dw0 := (Value.dw0 shr shift) or (Value.dw1 shl (32-shift));
    result.dw1 := (Value.dw1 shr shift) or (Value.dw2 shl (32-shift));
    result.dw2 := (Value.dw2 shr shift) or (Value.dw3 shl (32-shift));
    result.dw3 := (Value.dw3 shr shift);
  end;
end;

operator shr(const Value: UInt128; shift: UInt128): UInt128;
begin
  if (shift.dw0 > 128) or (shift.dw1 <>0) or (shift.dw2 <>0)
    or (shift.dw3 <> 0) then result := 0 else
    result := Value shr shift.dw0;
end;

operator=(const Value1: UInt128; const Value2: UInt128): boolean;
begin
  result := (Value1.dw0 = Value2.dw0) and (Value1.dw1 = Value2.dw1) and
    (Value1.dw2 = Value2.dw2) and (Value1.dw3 = Value2.dw3);
end;

operator=(const Value1: UInt128; const Value2: LongWord): boolean;
begin
  result := (Value1.dw0 = Value2) and (Value1.dw1 = 0) and (Value1.dw2 = 0)
    and(Value1.dw3 = 0);
end;

operator=(const Value1: UInt128; const Value2: QWord): boolean;
begin
  result := (Value1.dw0 = Value2 and $ffffffff) and (Value1.dw1 =Value2 shr 32) and (Value1.dw2 = 0)
    and(Value1.dw3 = 0);
end;

operator=(const Value1: UInt128; const Value2: integer): boolean;
begin
  if Value2 < 0 then result := false
    else result := (Value1 = longword(Value2));
end;

operator>(const Value1: UInt128; const Value2: UInt128): boolean;
begin
  if Value1.dw3 > Value2.dw3 then result := true
  else if Value1.dw3 < Value2.dw3 then result := false
  else if Value1.dw2 > Value2.dw2 then result := true
  else if Value1.dw2 < Value2.dw2 then result := false
  else if Value1.dw1 > Value2.dw1 then result := true
  else if Value1.dw1 < Value2.dw1 then result := false
  else if Value1.dw0 > Value2.dw0 then result := true
  else result := false;
end;

operator<(const Value1: UInt128; const Value2: UInt128): boolean;
begin
  if Value1.dw3 < Value2.dw3 then result := true
  else if Value1.dw3 > Value2.dw3 then result := false
  else if Value1.dw2 < Value2.dw2 then result := true
  else if Value1.dw2 > Value2.dw2 then result := false
  else if Value1.dw1 < Value2.dw1 then result := true
  else if Value1.dw1 > Value2.dw1 then result := false
  else if Value1.dw0 < Value2.dw0 then result := true
  else result := false;
end;

operator>=(const Value1: UInt128; const Value2: UInt128): boolean;
begin
  result := (Value1 = Value2) or (Value1 > Value2);
end;

operator<=(const Value1: UInt128; const Value2: UInt128): boolean;
begin
  result := (Value1 = Value2) or (Value1 < Value2);
end;

operator*(const Value1: UInt128; const Value2: UInt128): UInt128;
var qw: qword;
   temp: UInt128;
begin
  qw := qword(Value1.dw0) * qword(Value2.dw0);
  result.dw0 := qw and $ffffffff;
  result.dw1 := qw shr 32;
  result.dw2 := 0;
  result.dw3 := 0;

  qw := qword(Value1.dw0) * qword(value2.dw1);
  temp.dw0 := 0;
  temp.dw1 := qw and $ffffffff;
  temp.dw2 := qw shr 32;
  temp.dw3 := 0;
  result += temp;

  qw := qword(Value1.dw1) * qword(value2.dw0);
  temp.dw0 := 0;
  temp.dw1 := qw and $ffffffff;
  temp.dw2 := qw shr 32;
  temp.dw3 := 0;
  result += temp;

  qw := qword(Value1.dw0) * qword(value2.dw2);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := qw and $ffffffff;
  temp.dw3 := qw shr 32;
  result += temp;

  qw := qword(Value1.dw1) * qword(value2.dw1);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := qw and $ffffffff;
  temp.dw3 := qw shr 32;
  result += temp;

  qw := qword(Value1.dw2) * qword(value2.dw0);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := qw and $ffffffff;
  temp.dw3 := qw shr 32;
  result += temp;

  qw := qword(Value1.dw0) * qword(value2.dw3);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := 0;
  temp.dw3 := qw and $ffffffff;
  result += temp;

  qw := qword(Value1.dw1) * qword(value2.dw2);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := 0;
  temp.dw3 := qw and $ffffffff;
  result += temp;

  qw := qword(Value1.dw2) * qword(value2.dw1);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := 0;
  temp.dw3 := qw and $ffffffff;
  result += temp;

  qw := qword(Value1.dw3) * qword(value2.dw0);
  temp.dw0 := 0;
  temp.dw1 := 0;
  temp.dw2 := 0;
  temp.dw3 := qw and $ffffffff;
  result += temp;
end;

operator*(const Value1: UInt128; const Value2: LongWord): UInt128;
begin
  result := Value1*ToUInt128(Value2);
end;

operator*(const Value1: UInt128; const Value2: QWord): UInt128;
begin
  result := Value1*ToUInt128(Value2);
end;

operator+(const Value1: UInt128; const Value2: UInt128): UInt128;
var qw: qword;
   c0,c1,c2: boolean;

  procedure inc3;
  begin
    if result.dw3 = $ffffffff then
    begin
      result.dw3 := 0;
    end else
      inc(result.dw3);
  end;

  procedure inc2;
  begin
    if result.dw2 = $ffffffff then
    begin
      result.dw2 := 0;
      inc3;
    end else
      inc(result.dw2);
  end;

  procedure inc1;
  begin
    if result.dw1 = $ffffffff then
    begin
      result.dw1 := 0;
      inc2;
    end else
      inc(result.dw1);
  end;

begin
  qw := qword(Value1.dw0) + qword(Value2.dw0);
  result.dw0 := qw and $ffffffff;
  c0 := (qw shr 32) = 1;

  qw := qword(Value1.dw1) + qword(Value2.dw1);
  result.dw1 := qw and $ffffffff;
  c1 := (qw shr 32) = 1;

  qw := qword(Value1.dw2) + qword(Value2.dw2);
  result.dw2 := qw and $ffffffff;
  c2 := (qw shr 32) = 1;

  qw := qword(Value1.dw3) + qword(Value2.dw3);
  result.dw3 := qw and $ffffffff;

  if c0 then inc1;
  if c1 then inc2;
  if c2 then inc3;
end;

operator+(const Value1: UInt128; const Value2: LongWord): UInt128;
begin
  result := Value1 + ToUInt128(value2);
end;

operator+(const Value1: UInt128; const Value2: QWord): UInt128;
begin
  result := Value1 + ToUInt128(value2);
end;

operator+(const Value1: UInt128; const Value2: integer): UInt128;
begin
  if Value2 > 0 then result := Value1 + longword(Value2)
  else result := Value1 - longword(-Value2);
end;

operator-(const Value1: UInt128; const Value2: UInt128): UInt128;
var temp: UInt128;
begin
  temp := not Value2;
  Inc128(temp);
  result := Value1 + temp;
end;

operator-(const Value1: UInt128; const Value2: LongWord): UInt128;
begin
  result := Value1 - ToUInt128(Value2);
end;

operator-(const Value1: UInt128; const Value2: QWord): UInt128;
begin
  result := Value1 - ToUInt128(Value2);
end;

operator-(const Value1: UInt128; const Value2: integer): UInt128;
begin
  if Value2 > 0 then result := Value1 - longword(Value2)
  else result := Value1 + longword(-Value2);
end;

procedure Inc128(var Value: UInt128);
begin
  if Value.dw0 <> $ffffffff then inc(value.dw0) else
  begin
    value.dw0 := 0;
    if Value.dw1 <> $ffffffff then inc(value.dw1) else
    begin
      value.dw1 := 0;
      if Value.dw2 <> $ffffffff then inc(value.dw2) else
      begin
        value.dw2 := 0;
        if value.dw3 <> $ffffffff then inc(value.dw3) else
        begin
          value.dw3 := 0;
        end;
      end;
    end;
  end;
end;

function StrToInt128(text: string): UInt128;
var i: integer;
   ten: UInt128;
begin
  result := 0;
  ten := 10;
  for i := 1 to length(text) do
  begin
    if text[i] in ['0'..'9'] then
    begin
      result *= ten;
      result += longword(ord(text[I])-ord('0'));
    end;
  end;
end;

function Int128ToStr(const value: UInt128): string;
var
   digit,curValue,nextValue,ten: UInt128;
begin
  result := '';
  ten := 10;
  curValue := value;
  while CurValue <> 0 do
  begin
    DivMod128(CurValue,ten,nextValue,digit);
    result := chr(ord('0')+digit.dw0)+result;
    curValue := NextValue;
  end;
  if result ='' then result := '0';
end;

function Int128ToBin(const Value: UInt128): string;
var i: integer;
begin
  setLength(result,128);
  for i := 0 to 127 do
    result[128-i] := chr(GetBit128(Value,i)+ord('0'));
end;

function Int128ToHex(const Value: UInt128): string;
begin
  result := IntToHex(Value.dw3,8)+IntToHex(Value.dw2,8)+IntToHex(Value.dw1,8)+IntToHex(Value.dw0,8);
end;

function Power128(const Value: UInt128; const Exponent: UInt128): UInt128;
begin
  result := Power128(Value, Exponent.dw0);
end;

function Power128(const Value: UInt128; Exponent: LongWord): UInt128;
var
   mul: UInt128;
begin
  result := 1;
  mul := Value;
  while Exponent <>0 do
  begin
    if Exponent and 1 = 1 then result := result * mul;
    Exponent := Exponent shr 1;
    mul := mul*mul;
  end;
end;

function Power128(const Value: UInt128; const Exponent: QWord): UInt128;
begin
  result := Power128(Value, longword(Exponent));
end;

procedure SetBit128(var Value: UInt128; numBit: integer);
begin
  Value.dw[numBit shr 5] := Value.dw[numBit shr 5] or longword(1 shl (numBit and 31));
end;

procedure UnsetBit128(var Value: UInt128; numBit: integer);
begin
  Value.dw[numBit shr 5] := Value.dw[numBit shr 5] and not longword(1 shl (numBit and 31));
end;

function GetBit128(const Value: UInt128; numBit: integer): integer;
begin
  result := (Value.dw[numBit shr 5] shr (numBit and 31)) and 1;
end;

procedure ToggleBit128(var Value: UInt128; numBit: integer);
begin
  Value.dw[numBit shr 5] := Value.dw[numBit shr 5] xor longword(1 shl (numBit and 31));
end;

procedure DivMod128(const Value1: UInt128; const Value2: UInt128; out
  DivResult: UInt128; out Remainder: UInt128);
var
   curShift: integer;
   sub: UInt128;

begin
  if Value2 = 0 then raise EDivByZero.Create('DivMod128: Div by zero');
  if Value2 = 1 then
  begin
    DivResult := Value1;
    Remainder := 0;
    exit;
  end;
  sub := Value2;
  Remainder := Value1;
  DivResult := 0;

  curShift := 0;
  while (sub.dw3 and $80000000 = 0) and (sub < Remainder) do
  begin
    sub := sub shl 1;
    inc(curShift);
    if (sub > Remainder) then
    begin
      sub := sub shr 1;
      dec(curShift);
      break;
    end;
  end;

  while true do
  begin
    if sub <= Remainder then
    begin
      Remainder -= sub;
      SetBit128(DivResult, curShift);
    end;
    if curShift > 0 then
    begin
      sub := sub shr 1;
      dec(curShift);
    end else
      break;
  end;
end;

operator div(const Value1: UInt128; const Value2: UInt128): UInt128;
var temp: UInt128;
begin
  DivMod128(Value1,Value2,result,temp);
end;

operator mod(const Value1: UInt128; const Value2: UInt128): UInt128;
var temp: UInt128;
begin
  DivMod128(Value1,Value2,temp,result);
end;

operator not(const Value: UInt128): UInt128;
begin
  result.dw0 := not Value.dw0;
  result.dw1 := not Value.dw1;
  result.dw2 := not Value.dw2;
  result.dw3 := not Value.dw3;
end;

operator or(const Value1: UInt128; const Value2: UInt128): UInt128;
begin
  result.dw0 := Value1.dw0 or value2.dw0;
  result.dw1 := Value1.dw1 or value2.dw1;
  result.dw2 := Value1.dw2 or value2.dw2;
  result.dw3 := Value1.dw3 or value2.dw3;
end;

operator xor(const Value1: UInt128; const Value2: UInt128): UInt128;
begin
  result.dw0 := Value1.dw0 xor value2.dw0;
  result.dw1 := Value1.dw1 xor value2.dw1;
  result.dw2 := Value1.dw2 xor value2.dw2;
  result.dw3 := Value1.dw3 xor value2.dw3;
end;

operator and(const Value1: UInt128; const Value2: UInt128): UInt128;
begin
  result.dw0 := Value1.dw0 and value2.dw0;
  result.dw1 := Value1.dw1 and value2.dw1;
  result.dw2 := Value1.dw2 and value2.dw2;
  result.dw3 := Value1.dw3 and value2.dw3;
end;

end.

