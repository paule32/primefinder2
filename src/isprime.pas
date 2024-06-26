// ----------------------------------------------------------------
// prime finder: (c) 2024 by Jens Kallup
// all rights reserved.
//
// only for education !
// commercial use not allowed !
// ----------------------------------------------------------------
{$ifdef FPC}
  {$macro on}           // set macro processing: on
  {$mode delphi}{$H+}   // support delphi syntax when using fpc
{$endif}
{$APPTYPE CONSOLE}      // sign application for the console
program isprime;

// ----------------------------------------------------------------
// depend on the compiler, we use pre-defined macro conditionals...
// ----------------------------------------------------------------
uses
  {$ifdef FPC}
// ----------------------------------------------------------------
// function signatures und *nix systems.
// NOTE: currently only MS-Windows 10 64-Bit Pro, and FPC 3.2.2 is
//       testet. There is no support of other operating system, yet
// ----------------------------------------------------------------
    {$IFDEF UNIX}
      {$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}  // UseCThreads
      Unix,
    {$ENDIF}    // UNIX
// ----------------------------------------------------------------
// text user interface stuff for the (Windows) 32-Bit Console ...
// ----------------------------------------------------------------
    App,        // TApplication
    Objects,    // window range (TRect)
    Drivers,    // Hotkey
    Views,      // events (cmQuit)
    Menus,      // TMenuBar, TStatusBar
    MsgBox,     // message boxes
    Dialogs,    // modal views
    Video,      // con video settings
    Keyboard,   // con keyboard settings
  {$endif}      // FPC
// ----------------------------------------------------------------
// win32api stuff, used by the text user interface (tui) under the
// Microsoft Windows 10 32/64-Bit command line interface (cli) ...
// ----------------------------------------------------------------
  {$ifdef WINDOWS}
  Windows,
  {$endif}      // WINDOWS
// ----------------------------------------------------------------
// common used classes that are used under FPC, and Delphi ...
// ----------------------------------------------------------------
  SysUtils, Classes, IniFiles, Int128;

// ----------------------------------------------------------------
// @brief global used variables and constants ...
// ----------------------------------------------------------------
const
  EXIT_SUCCESS = 0;     // application exit code without errors
  EXIT_FAILURE = 1;     // ... with errors

// ----------------------------------------------------------------
// .ini file setting varaubles ...
// ----------------------------------------------------------------
var
  ini_cols : Word    =    80;  // def. con coloumns
  ini_rows : Word    =    25;  // def. con rows
  ini_iface: Word    =     1;  // def. con text user interface
  ini_color: Boolean =  true;  // def. con in color mode?
  ini_debug: Boolean = false;  // def. debug mode
  
  ini_name, S : AnsiString;    // the name of the .ini file
  ini_file    : TextFile;      // ini file handle

// ----------------------------------------------------------------
// the object classes for the prime application ...
// ----------------------------------------------------------------
type
  PPrimeDialog = ^TPrimeDialog;
  TPrimeDialog = object(TDialog)
  private
    procedure doStopAndSave;
    procedure doStopAndLoad;
  public
    constructor Init;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

// ----------------------------------------------------------------
// @brief object to handle check boxes ...
// ----------------------------------------------------------------
type
  PPrimeCheckBox = ^TPrimeCheckBox;
  TPrimeCheckBox = object(TCheckBoxes)
  public
    constructor Init(R: Objects.TRect; item: String);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

// ----------------------------------------------------------------
// @brief object to handle data input line ...
// ----------------------------------------------------------------
type
  PPrimeInputLine = ^TPrimeInputLine;
  TPrimeInputLine = object(TInputLine)
  private
    internal_id: Integer;
  public
    constructor Init(R: Objects.TRect; len: Word);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;
  PPrimeInputLineCount = ^TPrimeInputLineCount;
  TPrimeInputLineCount = object(TStaticText)
  public
    constructor Init(R: Objects.TRect);
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

// ----------------------------------------------------------------
// @brief object structure for the core application TUI view ...
// ----------------------------------------------------------------
type
  TPrimeApp = object(TApplication)
  private
    procedure doAbout;
  public
    constructor Init;
    procedure InitMenuBar; virtual;
    procedure InitStatusLine; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

// ----------------------------------------------------------------
// object, that is used to get, and set the values for the prime
// dialog fields ...
// ----------------------------------------------------------------
type
  PPrimeSuperObject = ^TPrimeSuperObject;
  TPrimeSuperObject = object
  public
    PrimeDialog: TPrimeDialog;
    
    PrimeStartTime: TDateTime;
    PrimeEndTime  : TDateTime;
    
    PrimeTimePass1 : PStaticText;
    PrimeTimePass2 : PStaticText;
    PrimeTimePass3 : PStaticText;
    PrimeTimePass4 : PStaticText;
    //
    PrimeTime1     : TDateTime;
    PrimeTime2     : TDateTime;
    PrimeTime3     : TDateTime;
    PrimeTime4     : TDateTime;
    //
    indexPrev      : PCheckBoxes;
    indexNext      : PCheckBoxes;
    
    line_start : PPrimeInputLine;
    line_end   : PPrimeInputLine;
    
    indexPrime : PPrimeCheckBox;
    forceStart : PPrimeCheckBox;
    
    labl_start : PPrimeInputLineCount;
    labl_end   : PPrimeInputLineCount;
    
    internal_input_counter : Integer;
    
    cont_start : Integer;
    cont_end   : Integer;
    
    fond_index : PStaticText;
    fond_prime : PStaticText;
  end;

// ----------------------------------------------------------------
// global forwarded members for later use ...
// ----------------------------------------------------------------
function check_prime_only(nst: String): Boolean; forward;
function check_prime     (nst: String): Boolean; forward;

// ----------------------------------------------------------------
// application command ID's ...
// ----------------------------------------------------------------
const
  cmAbout        = 1002;  // help event requested
  cmPrimeDialog  = 1003;  // main primer :)
  
  cmCancelSearch = 1004;
  cmStarteSearch = 1005;
  
  cmStopAndLoad  = 1006;
  cmStopAndSave  = 1007;
  
// ----------------------------------------------------------------
// internal global used variables ...
// ----------------------------------------------------------------  
var
  PrimeApp  : TPrimeApp;  // Turbo Vision like application
  PrimeIni  : TIniFile;   // support for application .ini settings
  
  PrimeObject : TPrimeSuperObject;
  ErrorRes    : Byte;     // if any error, set this flag
  
var
  vm        : TVideoMode; // fine tuning the tui application
  number    : AnsiString;
  
  primeList: TStringList; // holds last primes + index

// ----------------------------------------------------------------
// @brief ctor - construct a TCheckBox
// ----------------------------------------------------------------
{$ifdef FPC}
constructor TPrimeCheckBox.Init(R: Objects.TRect; item: String);
begin
  inherited Init(R, NewSItem(item, nil));
end;

// ----------------------------------------------------------------
// @brief handle events in the running tui application.
//        waits for keyboard or mouse interaction from the user.
// ----------------------------------------------------------------
procedure TPrimeCheckBox.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  
  if Event.What = evCommand then begin
  end;
end;

// ----------------------------------------------------------------
// @brief ctor - construct a TInputLine
// ----------------------------------------------------------------
constructor TPrimeInputLineCount.Init(R: Objects.TRect);
begin
  inherited Init(R, '   0 / 250');
  
  PrimeObject.cont_start := 1;
  PrimeObject.cont_end   := 1;
end;

procedure TPrimeInputLineCount.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
end;

constructor TPrimeInputLine.Init(R: Objects.TRect; len: Word);
begin
  inherited Init(R, len);
  internal_id :=
  PrimeObject.internal_input_counter;
  inc(PrimeObject.internal_input_counter);
end;
// ----------------------------------------------------------------
// @brief handle events in the running tui application.
//        waits for keyboard or mouse interaction from the user.
// ----------------------------------------------------------------
procedure TPrimeInputLine.HandleEvent(var Event: TEvent);
var
  fmt: String;
begin
  fmt := '%4d / 250';
  if (Event.What = evKeyDown) then begin
    with PrimeObject do begin
      if event.keycode = kbBack then begin
        if internal_id = 1 then begin
          dec(cont_start);
          if cont_start < 1   then
          cont_start := 1;  Delete(
          line_start^.data^,  Length(
          line_start^.data^), 1);
          
          labl_start^.Text^ := Format(fmt, [
          cont_start-1]);
          labl_start.draw;
        end else
        if internal_id = 2 then begin
          dec(cont_end);
          if cont_end < 1   then
          cont_end := 1;  Delete(
          
          line_end^.data^,  Length(
          line_end^.data^), 1);
          
          labl_end^.Text^ := Format(fmt, [
          cont_end-1]);
          labl_end.draw;
        end;
      end else
      if ((event.charcode >= '0') and (event.charcode <= '9')) then begin
        if internal_id = 1 then begin
          if cont_start >= 250 then begin
            cont_start := 250;
            ClearEvent(event);
            exit;
          end else
          if Length(labl_start^.Text^) < 250 then begin
            labl_start^.Text^ := Format(fmt, [
            Length(line_start^.data^)+1]);
            labl_start.draw;
            inc(cont_start);
          end;
        end else
        if internal_id = 2 then begin
          if cont_end >= 250 then begin
            cont_end := 250;
            ClearEvent(event);
            exit;
          end else
          if Length(labl_end^.Text^) < 251 then begin
            labl_end^.Text^ := Format(fmt, [
            Length(line_end^.data^)+1]);
            labl_end.draw;
            inc(cont_end);
          end;
        end;
      end else begin
        ClearEvent(event);
        exit;
      end;
    end;
  end;
  inherited HandleEvent(Event);
end;

// ----------------------------------------------------------------
// @brief stop the prime calculation and save current found values.
// ----------------------------------------------------------------
procedure TPrimeDialog.doStopAndSave;
begin
  MsgBox.MessageBox(PrimeObject.line_start^.data^, nil, mfInformation + mfOkButton);
  MsgBox.MessageBox(PrimeObject.line_end  ^.data^, nil, mfInformation + mfOkButton);
  
  if PrimeObject.indexPrime^.Value = 1 then
  MsgBox.MessageBox('index true' , nil, mfInformation + mfOkButton) else
  MsgBox.MessageBox('index false', nil, mfInformation + mfOkButton) ;
  
  if PrimeObject.forceStart^.Value = 1 then
  MsgBox.MessageBox('start true' , nil, mfInformation + mfOkButton) else
  MsgBox.MessageBox('start false', nil, mfInformation + mfOkButton) ;
end;

// ----------------------------------------------------------------
// @brief stop the prime calculation and load current found values.
// ----------------------------------------------------------------
procedure TPrimeDialog.doStopAndLoad;
begin
  MsgBox.MessageBox('lodser', nil, mfInformation + mfOkButton);
end;

procedure TPrimeDialog.HandleEvent(var Event: TEvent);
var
  result: Boolean;
begin
  result := false;
  inherited HandleEvent(Event);
  
  case Event.What of
    evCommand: begin
      case Event.Command of
        // lets rumbble :-()
        cmStarteSearch: begin
          with PrimeObject do begin
            PrimeStartTime := Now;
            
            // before we start calculation, reset the display values ...
            PrimeTimePass1.Text^ := 'Pass 1:  00:00:00:000'; PrimeTimePass1.draw();
            PrimeTimePass2.Text^ := 'Pass 2:  00:00:00:000'; PrimeTimePass2.draw();
            PrimeTimePass3.Text^ := 'Pass 3:  00:00:00:000'; PrimeTimePass3.draw();
            PrimeTimePass4.Text^ := 'Total :  00:00:00:000'; PrimeTimePass4.draw();
            
            fond_index.Text^ := ' ';
            fond_prime.Text^ := ' ';
            //
            fond_index.draw();
            fond_prime.draw();
            
            if indexPrime^.Value = 0 then begin
              // check prime with index
              result := check_prime(String(line_start^.data^));
            end else
            if indexPrime^.Value = 1 then begin
              // check prime without index calc.
              result := check_prime_only(line_start^.data^);
            end;
          end;
          ClearEvent(Event);
          exit;
        end;
        cmStopAndSave : begin doStopAndSave; ClearEvent(Event); end;
        cmStopAndLoad : begin doStopAndLoad; ClearEvent(Event); end;
      end;
    end;
  end;
end;

// ----------------------------------------------------------------
// standard prime dialog constructor ...
// ----------------------------------------------------------------
constructor TPrimeDialog.Init;
var
  R: Objects.TRect;
begin
  R.Assign(0,0,72,19);
  R.Move(4,2);
  inherited Init(R, 'Prime finder Dialog');
  
  with PrimeObject do begin
    // text 1
    R.Assign(2,1, 43,2); Insert(New(PStaticText, Init(R, 'Start Prime:')));
    R.Assign(2,4, 43,5); Insert(New(PStaticText, Init(R, 'End Prime:')));

    // input line: prime start
    R.Assign(2,2, 55, 3);  line_start := New(PPrimeInputLine, Init(R, 250));
    R.Assign(2,5, 55, 6);  line_end   := New(PPrimeInputLine, Init(R, 250));
    //
    insert(line_start);
    insert(line_end  );
    //
    R.Assign(58,2, 72, 3); labl_start := New(PPrimeInputLineCount, Init(R));
    R.Assign(58,5, 72, 6); labl_end   := New(PPrimeInputLineCount, Init(R));
    //
    insert(PrimeObject.labl_start);
    insert(PrimeObject.labl_end  );

    // checkbox: parameter
    R.Assign( 2, 7, 23, 8); Insert(New(PStaticText, Init(R, 'Parameter 1:')));
    R.Assign(26, 7, 47, 8); Insert(New(PStaticText, Init(R, 'Parameter 2:')));

    R.Assign( 2, 8, 24, 9); indexPrime := New(PPrimeCheckBox, Init(R, '~o~nly prime check' ));
    //
    R.Assign(26, 8, 45, 9); forceStart := New(PPrimeCheckBox, Init(R, 'force start' ));
    R.Assign(26, 9, 45,10); indexPrev  := New(PPrimeCheckBox, Init(R, 'prev index'  ));
    R.Assign(26,10, 45,11); indexNext  := New(PPrimeCheckBox, Init(R, 'next index'  ));
    //
    Insert(indexPrime);
    Insert(forceStart);
    //
    insert(indexPrev);
    insert(indexNext);
  
    // button: start
    R.Assign(48,8,  69,10); Insert(New(PButton, Init(R, 'S T A R T', cmStarteSearch, bfNormal)));
  
    R.Assign(48,10, 69,11); PrimeTimePass1 := New(PStaticText, Init(R, 'Pass 1:  00:00:00:000'));
    R.Assign(48,11, 69,12); PrimeTimePass2 := New(PStaticText, Init(R, 'Pass 2:  00:00:00:000'));
    R.Assign(48,12, 69,13); PrimeTimePass3 := New(PStaticText, Init(R, 'Pass 3:  00:00:00:000'));
    //
    insert(PrimeTimePass1);
    insert(PrimeTimePass2);
    insert(PrimeTimePass3);
    //
    R.Assign(48,13, 69,14); Insert(New(PStaticText, Init(R, '---------------------')));
    //
    R.Assign(48,14, 69,15); PrimeTimePass4 := New(PStaticText, Init(R, 'Total :  00:00:00:000'));
    insert(PrimeTimePass4);
  
    // text: index
    R.Assign(2,10, 25,11); Insert(New(PStaticText, Init(R, 'Index:')));
    R.Assign(2,11, 25,12); Insert(New(PStaticText, Init(R, 'Prime:')));
    //
    R.Assign(2,13, 30,14); Insert(New(PStaticText, Init(R, 'Prev Index:')));
    R.Assign(2,14, 39,15); Insert(New(PStaticText, Init(R, 'Next Index:')));
    
    // text: prime
    R.Assign(10,10, 25,11); fond_index := New(PStaticText, Init(R, ' '));
    R.Assign(10,11, 25,12); fond_prime := New(PStaticText, Init(R, ' '));
    //
    insert(fond_index);
    insert(fond_prime);

    // buttons
    R.Assign( 2,16, 24,18); Insert(New(PButton, Init(R, '~C~ancel Search' , cmCancelSearch, bfDefault)));
    R.Assign(26,16, 46,18); Insert(New(PButton, Init(R, '~L~oad Data'     , cmStopAndLoad , bfNormal )));
    R.Assign(48,16, 69,18); Insert(New(PButton, Init(R, '~S~top and saves', cmStopAndSave, bfNormal )));
  
    desktop^.ExecView(@self);
  end;
end;

// ----------------------------------------------------------------
// @brief ctor - construct a text user interface (tui) application.
// ----------------------------------------------------------------
constructor TPrimeApp.Init;
begin
  inherited init;
end;

// ----------------------------------------------------------------
// @brief display a nice informative dialog from the makers of this
//        application - me (paule32 :-)
// ----------------------------------------------------------------
procedure TPrimeApp.doAbout;
var
  dlg: PDialog;
  R: Objects.TRect;
begin
  R.Assign(0,0,46,14);
  R.Move(15,3);
  
  dlg := New(PDialog, Init(R, 'About this Application'));
  with dlg^ do begin
    // text
    R.Assign(3,3, 43,5);
    Insert(New(PStaticText, Init(R, ''
    + #3 + 'PrimeFinder FPC 1.0.0' + #13
    + #3 + '(c) 2024 Jens Kallup - paule32')));
    
    R.Assign(3,6, 43,9);
    Insert(New(PStaticText, Init(R, ''
    + #3 + 'only for education, and non-profit.' + #13
    + #3 + 'commercial use is not allowed !!!')));
    
    R.Assign(3,9, 43,10);
    Insert(New(PStaticText, Init(R, ''
    + #3 + 'Press F2-Key to open Prime Dialog')));
    
    // license button
    R.Assign(20,11, 37,13);
    Insert(New(PButton, Init(R, '~L~icense', cmOK, bfNormal)));
    
    // ok button
    R.Assign(7,11, 17,13);
    Insert(New(PButton, Init(R, '~O~K', cmOK, bfDefault)));
  end;
  
  if ValidView(dlg) <> nil then begin
    desktop^.ExecView(dlg);
    Dispose(dlg, done);
  end;
end;

// ----------------------------------------------------------------
// @brief handle events in the running tui application.
//        waits for keyboard or mouse interaction from the user.
// ----------------------------------------------------------------
procedure TPrimeApp.HandleEvent(var Event: TEvent);
begin
  inherited HandleEvent(Event);
  
  if Event.What = evCommand then begin
    case Event.Command of
      cmAbout: begin doAbout; end;
      cmPrimeDialog: begin
        PrimeObject.primeDialog.Init;
        //desktop^.insert(primeDialog);
      end;
      else begin
        exit;
      end;
    end;
  end;
  ClearEvent(Event);
end;

// ----------------------------------------------------------------
// currantly a poor mini menu - time will come ... :-)
// ----------------------------------------------------------------
procedure TPrimeApp.InitMenuBar;
var
  R: Objects.TRect;
  M: PMenu;
  M0_0, M1_0,
  M0_1, M1_1,
  M0_2, M1_2, SM0, SM1: PMenuItem;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  
  m1_2 := NewItem('~A~bout ...', 'F1', kbF1, cmAbout, hcNoContext, nil);
  M1_1 := NewLine(M1_2);
  m1_0 := NewItem('~S~ervice', 'F5', kbF5, 1003, hcNoContext, M1_1);
  SM1  := NewSubMenu('~H~elp', hcNoContext, NewMenu(M1_0), nil);
  
  M0_2 := NewItem('~P~rime Dialog', 'F2', kbF2, cmPrimeDialog, hcNoContext, nil);
  M0_1 := NewLine(M0_2);
  m0_0 := NewItem('E~x~it', 'Alt + X', kbAltX, cmQuit, hcNoContext, M0_1);
  SM0  := NewSubMenu('~A~pplication', hcNoContext, NewMenu(M0_0), SM1);

  M := NewMenu(SM0);
  MenuBar := New(PMenuBar, Init(R, M));
end;

// ----------------------------------------------------------------
// @brief a simple status line on the bottom of the screen.
// ----------------------------------------------------------------
procedure TPrimeApp.InitStatusLine;
var
  R: Objects.TRect;
begin
  GetExtent(R);
  R.A.Y := R.B.Y - 1;
  StatusLine := New(PStatusLine, Init(R, NewStatusDef(0, $ffff,
    NewStatusKey('~Alt+X~ Exit', kbAltX, cmQuit, nil),
    nil)));
end;
{$endif}

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
  suche, index, check, i, j, m, prime: UInt128;
  lastIndex, lastPrime: UInt128;
  len, b: LongInt;
  A: AnsiString;
begin
  // -------------------------------------------------------------
  // pre-initinitalize the result of this function:
  // -------------------------------------------------------------
  result := false;
  index  := 1;

  with PrimeObject do begin  
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
      MsgBox.MessageBox('the input value is not valid.', nil,
      mfError + mfOkButton);
      exit;
    end;

    // -------------------------------------------------------------
    // pre-filter "not" primes ...
    // -------------------------------------------------------------
    if nst = '0' then begin
      MsgBox.MessageBox('0: is not prime', nil,
      mfError + mfOkButton);
      result := false;
      exit;
    end else
    if nst = '1' then begin
      MsgBox.MessageBox('1: is not prime', nil,
      mfError + mfOkButton);
      result := false;
      exit;
    end else
    if nst = '2' then begin
      fond_index.Text^ := '1';
      fond_index.draw;
    
      fond_prime.Text^ := '2';
      fond_prime.draw;
      result := true;
      exit;
    end else
    if nst = '3' then begin
      fond_index.Text^ := '2';
      fond_index.draw;
    
      fond_prime.Text^ := '3';
      fond_prime.draw;
      result := true;
      exit;
    end;
  
    // -------------------------------------------------------------
    // check if prime have twins, if so, then it is not a prime.
    // todo: complete check other prime twins ...
    // -------------------------------------------------------------
    if Length(nst) > 2 then begin
      result := true;
      for b := StrToInt('0') to StrToInt('9') do begin
        if result = false then begin
          MsgBox.MessageBox(Format('%s: is no prime, it has twins.',[nst]),
          nil, mfError + mfOkButton);
          result := false;
          exit;
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
          fond_index.Text^ := '9'; // todo !
          fond_index.draw;

          fond_prime.Text^ := '23';
          fond_prime.draw;
          result := true;
          exit;
        end else
        if Pos('23', nst, Length(nst) - len) > 0 then begin
          while true do begin
            if len = 0 then exit;
            len := len - 2;
            if Pos('23', nst, Length(nst) - len) = 0 then begin
              MsgBox.MessageBox('number is not prime (23. twins) !',
              nil, mfError + mfOkButton);
              exit;
            end;
          end;
        end;
      end;
    end;

    // -------------------------------------------------------------
    // post-initialize used variables ...
    // -------------------------------------------------------------
    suche     := StrToInt128(nst);
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
    i := 1;
    A := '';

    PrimeStartTime := Now;
    PrimeTimePass1.Text^ := 'Pass 1:  00:00:00:000';
    PrimeTimePass1.draw();
  
    // fill array (string list) with T as true sign ...
    while i <= suche do begin
      A := A + 'T';
      i := i + 1;
    
      PrimeEndTime := Now;
      PrimeTime1 :=
      PrimeStartTime - PrimeEndTime;
      
      PrimeTimePass1.Text^ := Format(
      'Pass 1:  %s', [FormatDateTime('HH:MM:SS:ZZZ',
      PrimeTime1)]);
      PrimeTimePass1.draw();
    end;

    // -------------------------------------------------------
    // calculate prime, and extend A by setting F, if no prime
    // begining with prime 2 ...
    // -------------------------------------------------------
    PrimeStartTime := Now;
    PrimeTimePass2.Text^ := 'Pass 2:  00:00:00:000';
    PrimeTimePass2.draw();
  
    i := 2;
    while true do begin
      if A[StrToInt(Int128ToStr(i))] = 'T' then begin
        j := i * i;
        PrimeEndTime := Now;
        PrimeTime2 :=
        PrimeStartTime - PrimeEndTime;
      
        PrimeTimePass2.Text^ := Format(
        'Pass 2:  %s', [FormatDateTime('HH:MM:SS:ZZZ',
        PrimeTime2)]);
        PrimeTimePass2.draw();
      
        while j <= suche do begin
          A[StrToInt(Int128ToStr(j))] := 'F';
        
          PrimeEndTime := Now;
          PrimeTime2 :=
          PrimeStartTime - PrimeEndTime;
    
          PrimeTimePass2.Text^ := Format(
          'Pass 2:  %s', [FormatDateTime('HH:MM:SS:ZZZ',
          PrimeTime2)]);
          PrimeTimePass2.draw();
        
          j := j + i;
          if j > suche then break;
        end;
      end;
      if i >= check then break;
      index := index + 1;
      i := i + 1;
    end;

    // -------------------------------------------------------
    // now, calculate indices of T ...
    // -------------------------------------------------------
    m := 0;
    i := 1;
  
    PrimeStartTime := Now;
    PrimeTimePass3.Text^ := 'Pass 3:  00:00:00:000';
    PrimeTimePass3.draw();
  
    while true do begin
      PrimeEndTime := Now;
      PrimeTime3 :=
      PrimeStartTime - PrimeEndTime;
        
      PrimeTimePass3.Text^ := Format(
      'Pass 3:  %s', [FormatDateTime('HH:MM:SS:ZZZ',
      PrimeTime3)]);
      PrimeTimePass3.draw();
      
      if A[StrToInt(Int128ToStr(i))] = 'T' then begin
        m := m + 1;
      end;
      if i >= Length(A) then begin
        m := m - 1;
        break;
      end else begin
        i := i + 1;
      end;
    end;

    // -------------------------------------------------------
    // if end of string contains 'F', then the input value is
    // not prime; else by 'T': input is prime, and the index
    // will be display:
    // -------------------------------------------------------
    if A[Length(A)] = 'F' then begin
      fond_index.Text^ := ' ';
      fond_prime.Text^ := ' ';
      result := false;
    end else begin
      fond_index.Text^ := Int128ToStr(m);
      fond_index.draw;
      fond_prime.Text^ := 'is prime';
      result := true;
    end;

    PrimeTime4 :=
    PrimeTime3 +
    PrimeTime2 +
    PrimeTime1 ;
  
    PrimeTimePass4.Text^ := Format(
    'Total :  %s', [FormatDateTime('HH:MM:SS:ZZZ',
    PrimeTime4)]);
    PrimeTimePass4.draw();
  
    if result then begin
      MsgBox.MessageBox(#3 + 'SUCCESS'   +
      #13#3  + 'end of calculation.'     +
      #13#13 + 'Index: ' + Int128ToStr(m),
      nil, mfInformation + mfOkButton);
    end else begin
      MsgBox.MessageBox(#3 + 'FAILED'    +
      #13#3  + 'end of calculation.'     ,    
      nil, mfInformation + mfOkButton);
    end;
  end;
end;

function check_prime_only(nst: String): Boolean;
var
  i, suche: UInt128;
begin
  result := false;
  suche := StrToInt128(nst);
  if ((suche = 2) or (suche = 3)) then begin
    result := true;
    exit;
  end;
  if ((suche <= 1) or ((suche mod 2) = 0) or ((suche mod 3) = 0)) then begin
    result := false;
    exit;
  end;
  i := 5;
  while (i * i <= suche) do begin
    if ((suche mod i) = 0) or ((suche mod (i + 2)) = 0) then begin
      result := false;
      exit;
    end else begin
      i := i + 6;
    end;
  end;
  result := true;
end;

// ----------------------------------------------------------------
// @brief entry point procedure for our application. tested and
//        compiled with fpc (free pascal compiler) version: 3.2.2
//        64-Bit under Windows 10 64-Bit, 4 GB of RAM.
// ----------------------------------------------------------------
begin
  try // finally   block
  try // exception block
      // ----------------------------------------------------------
      // pre-initinitalize application varuables ...
      // ----------------------------------------------------------
      ErrorRes  := EXIT_SUCCESS;
      ini_name  := ChangeFileExt(ExtractFileName(ParamStr(0)),'.ini');
      ini_debug := false;

      // ----------------------------------------------------------
      // use exceptions to catch errors (this is the default so
      // not absolutely requiered).
      // ----------------------------------------------------------
      if not FileExists(ini_name) then begin
        AssignFile(ini_file, ini_name);
        {$I+}
        ReWrite(ini_file);
        
        WriteLn(ini_file,'[common]'   );  // section name
        WriteLn(ini_file,'ui=1'       );  // user interface 1: tui
        WriteLn(ini_file,'cols=80'    );  // tui cols: 80
        WriteLn(ini_file,'rows=25'    );  // tui rows: 25
        WriteLn(ini_file,'color=true' );  // tui in color mode
        WriteLn(ini_file,'debug=false');  // not in debug mode
        
        CloseFile(ini_file);
      end;
      
      // ---------------------------------------------------------
      // initialize the .ini settings with default values, if they
      // not already set up...
      // ---------------------------------------------------------
      PrimeIni := TIniFile.Create(ini_name);
      S        := 'common';
      
      with primeIni do begin
        ini_cols  := ReadInteger(S, 'cols' ,    80);
        ini_rows  := ReadInteger(S, 'rows' ,    25);
        ini_iface := ReadInteger(S, 'ui'   ,     1);
        ini_color := ReadBool   (S, 'color',  true);
        ini_debug := ReadBool   (S, 'debug', false);
      end;
      
      Randomize;
      InitVideo;
      InitKeyboard;
      
      PrimeObject.internal_input_counter := 1;
        
      // -----------------------------------------------------------
      // this values can be override by .ini file setttings ...
      // -----------------------------------------------------------
      vm.col   := ini_cols;   // default columns: 80
      vm.row   := ini_rows;   // default rows:    25
          
      vm.color := ini_color;  // default color mode
          
      // -----------------------------------------------------------
      // set the video options ...
      // -----------------------------------------------------------
      SetVideoMode(vm);
      SetCursorPos(0, 0);
      
      primeList := TStringList.Create;
      primeList.Clear;
        
      // -----------------------------------------------------------
      // start tge application text user interface (tui) ...
      // -----------------------------------------------------------
      PrimeApp.Init;  PrimeApp.doAbout;
      PrimeApp.Run;
      PrimeApp.Done;
  
      // -------------------------------------------------------------
      // print a nice banner on the screen. Please be so fair and hold
      // you on the CODE OF CONDUCT codex - Thank you.
      // -------------------------------------------------------------
      if ini_iface = 0 then begin
        WriteLn(''
        + #13 + 'primefind.exe 1.0 (c) 2024 Jens Kallup - paule32'
        + #13 + 'all rights reserved.'
        + #13
        + #13 + 'only for education purpose, and non-profit.'
        + #13 + 'commerical use is not allowed !'
        + #13);
      end;
    
      // -------------------------------------------------------------
      // you can pass application parameter - if you would like:
      // d => debug mode - to display progress (slow)
      // n => n for prime to pass as start prime (currently no check
      // of other chars than 0..9.
      // -------------------------------------------------------------
      if ParamCount() > 0 then begin
        if ParamStr(1) = 'd' then begin
          ini_debug := true;
        end else begin
          check_prime(ParamStr(1));
          exit;
        end;
      end;
      
      // -------------------------------------------------------------
      // there, you are on the plain command line console interface :
      // -------------------------------------------------------------
      if ini_iface = 0 then begin
        Write('Enter number: ');
        ReadLn(number);
        check_prime(number);
      end;
    except
      on E: EInOutError do begin
        WriteLn('class: ', E.ClassName);
        WriteLn('File: "', ini_name, '" is present.');
        ErrorRes := EXIT_FAILURE;
      end;
      on E: Exception   do begin
        WriteLn(E.ClassName, ': ', E.Message);
        ErrorRes := EXIT_FAILURE;
      end;
    end;
  finally
    // ---------------------------------------------------------------
    // save the settings of the applicatiion to the local storeage ...
    // ---------------------------------------------------------------
    S := 'common';
    
    with primeIni do begin
      WriteInteger(S, 'cols' , ini_cols );
      WriteInteger(S, 'rows' , ini_rows );
      WriteInteger(S, 'ui'   , ini_iface);
      WriteBool   (S, 'color', ini_color);
      WriteBool   (S, 'debug', ini_debug);
    end;

    // ---------------------------------------------------------------
    // try to free/empty allocated memory ...
    // ---------------------------------------------------------------
    PrimeIni.Free;
    PrimeIni := nil;
    
    primeList.Clear;
    primeList.Free;
    primeList := nil;
    
    // ---------------------------------------------------------------
    // reset system settings to the startup/defualt values ...
    // ---------------------------------------------------------------
    DoneVideo;
    DoneKeyboard;
    
    // ---------------------------------------------------------------
    // if any error, ErrorRes is > 0 then halt with ErrorRes value,
    // when no error detected, return value is 0.
    // ---------------------------------------------------------------
    Halt(ErrorRes);
  end;
end.
