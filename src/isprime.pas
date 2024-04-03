// ----------------------------------------------------------------
// prime finder: (c) 2024 by Jens Kallup
// all rights reserved.
//
// only for education !
// commercial use not allowed !
// ----------------------------------------------------------------
{$ifdef FPC}
  {$macro on}
  {$mode delphi}
{$endif}
{$APPTYPE CONSOLE}
program isprime;

// ----------------------------------------------------------------
// depend on the compiler, we use pre-defined macro conditionals...
// ----------------------------------------------------------------
uses
  {$ifdef FPC}
    {$IFDEF UNIX}
      {$IFDEF UseCThreads}
      cthreads,
      {$ENDIF}  // UseCThreads
      Unix,
    {$ENDIF}    // UNIX
    App,        // TApplication
    Objects,    // window range (TRect)
    Drivers,    // Hotkey
    Views,      // events (cmQuit)
    Menus,      // TMenuBar, TStatusBar
    Dialogs,    // modal views
    Video,      // con video settings
    Keyboard,   // con keyboard settings
  {$endif}      // FPC
  {$ifdef WINDOWS}
  Windows,
  {$endif}      // WINDOWS
  SysUtils, Classes, IniFiles, Int128;

// ----------------------------------------------------------------
// @brief global used variables and constants ...
// ----------------------------------------------------------------
const
  EXIT_SUCCESS = 0;
  EXIT_FAILURE = 1;

// ----------------------------------------------------------------
// .ini file setting varaubles ...
// ----------------------------------------------------------------
var
  ini_cols : Word    =    80;  // def. con coloumns
  ini_rows : Word    =    25;  // def. con rows
  ini_iface: Word    =     1;  // def. con text user interface
  ini_color: Boolean =  true;  // def. con in color mode?
  ini_debug: Boolean = false;  // def. debug mode
  
  ini_name : String;           // the name of the .ini file
  ini_file : TextFile;         // ini file handle

// ----------------------------------------------------------------
// the object class for the prime application ...
// ----------------------------------------------------------------
type
  {$ifdef FPC}
  TPrimeApp = object(TApplication)
  {$else}
  TPrimeApp = class (TApplication)
  {$endif}
  private
    procedure doPrimeDialog;
    procedure doAbout;
  public
    constructor Init;
    procedure InitMenuBar; virtual;
    procedure InitStatusLine; virtual;
    procedure HandleEvent(var Event: TEvent); virtual;
  end;

// ----------------------------------------------------------------
// application command ID's ...
// ----------------------------------------------------------------
const
  cmdAbout       = 1002;  // help event requested
  cmdPrimeDialog = 1003;  // main primer :)

// ----------------------------------------------------------------
// internal global used variables ...
// ----------------------------------------------------------------  
var
  PrimeApp  : TPrimeApp;  // Turbo Vision like application
  PrimeIni  : TIniFile;   // support for application .ini settings
  
  ErrorRes  : Byte;       // if any error, set this flag
  
var
  vm        : TVideoMode;
  app_name  :      PChar; // paramstr(0) for CreateProcess
  number    :     String;

// ----------------------------------------------------------------
// @brief ctor - construct a text user interface (tui) application.
// ----------------------------------------------------------------
{$ifdef FPC}
constructor TPrimeApp.Init;
begin
  inherited init;
end;

procedure TPrimeApp.doPrimeDialog;
var
  dlg: PDialog;
  R: Objects.TRect;
  line_start, line_end: PView;
begin
  R.Assign(0,0,72,16);
  R.Move(4,3);
  
  dlg := New(PDialog, Init(R, 'Prime finder Dialog'));
  with dlg^ do begin
    // text 1
    R.Assign(2,1, 43,2);
    Insert(New(PStaticText, Init(R, 'Start Prime:')));
    
    // text 2
    R.Assign(2,4, 43,5);
    Insert(New(PStaticText, Init(R, 'End Prime:')));
    
    // input line: prime start
    R.Assign(2,2, 64,3);
    line_start := New(PInputLine,Init(R,1024));
    insert(line_start);
    
    // input line: prime end
    R.Assign(2,5, 64,6);
    line_end := New(PInputLine,Init(R,1024));
    insert(line_end);

    
    // text: index
    R.Assign(2, 9,  42,10); Insert(New(PStaticText, Init(R, 'Index:')));
    R.Assign(2,10,  42,11); Insert(New(PStaticText, Init(R, 'Prime:')));
    
    // text: prime
    R.Assign(10, 9, 42,10); Insert(New(PStaticText, Init(R, '1')));
    R.Assign(10,10, 42,11); Insert(New(PStaticText, Init(R, '2')));
    
    // button: cancel
    R.Assign(2,13, 24,15);
    Insert(New(PButton, Init(R, '~C~ancel Search', cmOK, bfDefault)));
  end;
  
  if ValidView(dlg) <> nil then begin
    desktop^.insert(dlg);
  end;
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
      cmdAbout: begin
        doAbout;
      end;
      cmdPrimeDialog: begin
        doPrimeDialog;
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
  M0_0, M1_0, M2_0,
  M0_1, M1_1, M2_1,
  M0_2, M1_2, M2_2, SM0, SM1: PMenuItem;
begin
  GetExtent(R);
  R.B.Y := R.A.Y + 1;
  
  m1_2 := NewItem('~A~bout ...', 'F1', kbF1, cmdAbout, hcNoContext, nil);
  M1_1 := NewLine(M1_2);
  m1_0 := NewItem('~S~ervice', 'F5', kbF5, 1003, hcNoContext, M1_1);
  SM1  := NewSubMenu('~H~elp', hcNoContext, NewMenu(M1_0), nil);
  
  M0_2 := NewItem('~P~rime Dialog', 'F2', kbF2, cmdPrimeDialog, hcNoContext, nil);
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
  suche, index, check, i, j, even, m, prime: UInt128;
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
      if ini_debug then begin
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
      
      ini_cols  := primeIni.ReadInteger('common', 'cols' ,    80);
      ini_rows  := primeIni.ReadInteger('common', 'rows' ,    25);
      ini_iface := primeIni.ReadInteger('common', 'ui'   ,     1);
      ini_color := primeIni.ReadBool   ('common', 'color',  true);
      ini_debug := primeIni.ReadBool   ('common', 'debug', false);
      
      Randomize;
      {$ifdef FPC}
      InitVideo;
      InitKeyboard;
        
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
        
      // -----------------------------------------------------------
      // start tge application text user interface (tui) ...
      // -----------------------------------------------------------
      PrimeApp.Init;  PrimeApp.doAbout;
      PrimeApp.Run;
      PrimeApp.Done;
      {$endif}
  
      // -------------------------------------------------------------
      // print a nice banner on the screen. Please be so fair and hold
      // you on the CODE OF CONDUCT codex - Thank you.
      // -------------------------------------------------------------
      if ini_iface = 0 then begin
        WriteLn('primefind.exe 1.0 (c) 2024 Jens Kallup - paule32');
        WriteLn('all rights reserved.');
        WriteLn;
        WriteLn('only for education purpose, and non-profit.');
        WriteLn('commerical use is not allowed !');
        WriteLn;
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
    primeIni.WriteInteger('common', 'cols' , ini_cols );
    primeIni.WriteInteger('common', 'rows' , ini_rows );
    primeIni.WriteInteger('common', 'ui'   , ini_iface);
    primeIni.WriteBool   ('common', 'color', ini_color);
    primeIni.WriteBool   ('common', 'debug', ini_debug);

    // ---------------------------------------------------------------
    // try to free/empty allocated memory ...
    // ---------------------------------------------------------------
    PrimeIni.Free;
    PrimeIni := nil;
    
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
