(*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************)
program lisp (input, output);

label 99;

const
   NAMELENG = 20;      (* Maximum length of a name *)
   MAXNAMES = 300;     (* Maximum number of different names *)
   MAXINPUT = 4000;    (* Maximum length of an input *)
   STACKSIZE = 1000;
   MEMSIZE = 500;
   PROMPT = '-> ';
   PROMPT2 = '> ';
   COMMENTCHAR = ';';
   TABCODE = 9;        (* in ASCII *)

type
   NAMESIZE = 0..NAMELENG;
   NAMESTRING = packed array [1..NAMELENG] of char;

   NAME = 1 .. MAXNAMES; (* a NAME is an index in printNames *)

   BUILTINOP = (IFOP,WHILEOP,SETOP,BEGINOP,PLUSOP,MINUSOP,
                TIMESOP,DIVOP,EQOP,LTOP,GTOP,CONSOP,
                CAROP,CDROP,NUMBERPOP,SYMBOLPOP,
                LISTPOP,NULLPOP,PRINTOP);
   VALUEOP = PLUSOP .. PRINTOP;
   CONTROLOP = IFOP .. BEGINOP;

   SEXP = ^SEXPREC;
   EXP = ^EXPREC;
   EXPLIST = ^EXPLISTREC;
   ENV = ^ENVREC;
   VALUELIST = ^VALUELISTREC;
   NAMELIST = ^NAMELISTREC;
   FUNDEF = ^FUNDEFREC;

   SEXPTYPE = (NILSXP,NUMSXP,SYMSXP,LISTSXP);
   SEXPREC = record
               refcnt: integer;
               case sxptype: SEXPTYPE of
                  NILSXP: ();
                  NUMSXP: (intval: integer);
                  SYMSXP: (symval: NAME);
                  LISTSXP: (carval, cdrval: SEXP)
            end;

   EXPTYPE = (VALEXP,VAREXP,APEXP);
   EXPREC = record
               case etype: EXPTYPE of
                  VALEXP: (sxp: SEXP);
                  VAREXP: (varble: NAME; offset: integer); (* 0 for global *)
                  APEXP: (optr: NAME; args: EXPLIST)
            end;

   EXPLISTREC = record
               head: EXP;
               tail: EXPLIST
            end;

   VALUELISTREC = record
               head: SEXP;
               tail: VALUELIST
            end;

   NAMELISTREC = record
               head: NAME;
               tail: NAMELIST
            end;

   ENVREC = record
               vars: NAMELIST;
               values: VALUELIST
            end;
   FUNDEFREC = record
               funname: NAME;
               formals: NAMELIST;
               body: EXP;
               nextfundef: FUNDEF
            end;

var
   fundefs: FUNDEF;
   
   globalEnv: ENV;

   argStack: array [1..STACKSIZE] of SEXP;
   stacktop: integer; (* range is 1..STACKSIZE+1 *)

   freeSEXP: SEXP;
   
   currentExp: EXP;
   
   userinput: array [1..MAXINPUT] of char;
   inputleng, pos: 0..MAXINPUT;
   
   printNames: array [NAME] of NAMESTRING;
   numNames, numBuiltins: NAME;

   nilValue, trueValue: SEXP;

   quittingtime: Boolean;

(*****************************************************************
 *                     MEMORY MANAGEMENT                         *
 *****************************************************************)

(* initMemory - allocate all SEXPREC's                           *)
procedure initMemory;
var
   i: integer;
   s: SEXP;
begin
   freeSEXP := nil;
   for i := 1 to MEMSIZE do begin
      s := freeSEXP;
      new(freeSEXP);
      freeSEXP^.carval := s
      end
end; (* initMemory *)

(* allocSExp - allocate SEXPREC from free list                   *)
function allocSExp (t: SEXPTYPE): SEXP;
var s: SEXP;
begin
   if freeSEXP = nil
   then begin
           writeln('Out of memory');
           goto 99
        end;
   s := freeSEXP;
   s^.sxptype := t;
   s^.refcnt := 0;
   allocSExp := s;
   freeSEXP := freeSEXP^.carval
end; (* allocSExp *)

(* deallocSExp - return SEXPREC to free list                     *)
procedure deallocSExp (s: SEXP);
begin
   s^.carval := freeSEXP;
   freeSEXP := s
end; (* deallocSExp *)

(* increfcnt - increment refcnt field of argument                *)
procedure increfcnt (s: SEXP);
begin
   s^.refcnt := s^.refcnt+1
end; (* increfcnt *)

(* decrefcnt - decrement refcnt field of argument and referents  *)
procedure decrefcnt (s: SEXP);
begin
   with s^ do begin
      refcnt := refcnt-1;
      if refcnt = 0
      then begin
              if sxptype = LISTSXP
              then begin
                      decrefcnt(carval);
                      decrefcnt(cdrval)
                   end;
              deallocSExp(s)
           end
      end
end; (* decrefcnt *)

(* rlsExp - decrement refcnt in all SEXP's within e              *)
procedure rlsExp (e: EXP);
var argl: EXPLIST;
begin
   case e^.etype of
      VALEXP: decrefcnt(e^.sxp);
      VAREXP: ;
      APEXP:
         begin
            argl := e^.args;
            while argl <> nil do begin
               rlsExp(argl^.head);
               argl := argl^.tail
               end
         end
   end
end; (* rlsExp *)

(* initStack - initialize argument stack                         *)
procedure initStack;
begin
   stacktop := 1
end; (* initStack *)

(* pushArg - push a single argument onto argStack                *)
procedure pushArg (s: SEXP);
begin
   if stacktop > STACKSIZE
     then begin
             writeln('stack overflow');
             stacktop := 1;
             goto 99
          end;
   argStack[stacktop] := s;
   stacktop := stacktop + 1;
   increfcnt(s)
end; (* pushArg *)

(* topArg - return top item in stack                             *)
function topArg: SEXP;
begin
   topArg := argStack[stacktop-1]
end; (* topArg *)

(* belowtopArg - return next-to-top item in stack                *)
function belowtopArg: SEXP;
begin
   belowtopArg := argStack[stacktop-2]
end; (* belowtopArg *)

(* popArgs - pop argument list from top of stack                 *)
procedure popArgs (l: integer);
var i: integer;
begin
   for i := 1 to l do begin
      decrefcnt(argStack[stacktop-1]);
      stacktop := stacktop - 1
      end
end; (* popArgs *)

(*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************)

(* mkVALEXP - return an EXP of type VALEXP with sxp s            *)
function mkVALEXP (s: SEXP): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VALEXP;
   e^.sxp := s;
   increfcnt(s);
   mkVALEXP := e
end; (* mkVALEXP *)

(* mkVAREXP - return an EXP of type VAREXP with varble nm        *)
function mkVAREXP (nm: NAME): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := VAREXP;
   e^.varble := nm;
   e^.offset := 0;
   mkVAREXP := e
end; (* mkVAREXP *)

(* mkAPEXP - return EXP of type APEXP w/ optr op and args el     *)
function mkAPEXP (op: NAME; el: EXPLIST): EXP;
var e: EXP;
begin
   new(e);
   e^.etype := APEXP;
   e^.optr := op;
   e^.args := el;
   mkAPEXP := e
end; (* mkAPEXP *)

(* mkExplist - return an EXPLIST with head e and tail el         *)
function mkExplist (e: EXP; el: EXPLIST): EXPLIST;
var newel: EXPLIST;
begin
   new(newel);
   newel^.head := e;
   newel^.tail := el;
   mkExplist := newel
end; (* mkExplist *)

(* mkNamelist - return a NAMELIST with head n and tail nl        *)
function mkNamelist (nm: NAME; nl: NAMELIST): NAMELIST;
var newnl: NAMELIST;
begin
   new(newnl);
   newnl^.head := nm;
   newnl^.tail := nl;
   mkNamelist := newnl
end; (* mkNamelist *)

(* mkValuelist - return an VALUELIST with head s and tail vl     *)
function mkValuelist (s: SEXP; vl: VALUELIST): VALUELIST;
var newvl: VALUELIST;
begin
   new(newvl);
   newvl^.head := s;
   newvl^.tail := vl;
   mkValuelist := newvl
end; (* mkValuelist *)

(* mkEnv - return an ENV with vars nl and values vl              *)
function mkEnv (nl: NAMELIST; vl: VALUELIST): ENV;
var rho: ENV;
begin
   new(rho);
   rho^.vars := nl;
   rho^.values := vl;
   mkEnv := rho
end; (* mkEnv *)

(* lengthNL - return length of NAMELIST nl                       *)
function lengthNL (nl: NAMELIST): integer;
var i: integer;
begin
   i := 0;
   while nl <> nil do begin
      i := i+1;
      nl := nl^.tail
      end;
   lengthNL := i
end; (* lengthNL *)

(*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************)

(* fetchFun - get function definition of fname from fundefs      *)
function fetchFun (fname: NAME): FUNDEF;
var
   f: FUNDEF;
   found: Boolean;
begin
   found := false;
   f := fundefs;
   while (f <> nil) and not found do
      if f^.funname = fname
      then found := true
      else f := f^.nextfundef;
   fetchFun := f
end; (* fetchFun *)

(* newFunDef - add new function fname w/ parameters nl, body e   *)
procedure newFunDef (fname: NAME; nl: NAMELIST; e: EXP);
var f: FUNDEF;
begin
   f := fetchFun(fname);
   if f = nil (* fname not yet defined as a function *)
   then begin
           new(f);
           f^.nextfundef := fundefs; (* place new FUNDEFREC *)
           fundefs := f              (* on fundefs list *)
        end;
   f^.funname := fname;
   f^.formals := nl;
   f^.body := e
end; (* newFunDef *)

(* initNames - place all pre-defined names into printNames       *)
procedure initNames;
var i: integer;
begin
   fundefs := nil;
   i := 1;
   printNames[i] := 'if                  '; i := i+1;
   printNames[i] := 'while               '; i := i+1;
   printNames[i] := 'set                 '; i := i+1;
   printNames[i] := 'begin               '; i := i+1;
   printNames[i] := '+                   '; i := i+1;
   printNames[i] := '-                   '; i := i+1;
   printNames[i] := '*                   '; i := i+1;
   printNames[i] := '/                   '; i := i+1;
   printNames[i] := '=                   '; i := i+1;
   printNames[i] := '<                   '; i := i+1;
   printNames[i] := '>                   '; i := i+1;
   printNames[i] := 'cons                '; i := i+1;
   printNames[i] := 'car                 '; i := i+1;
   printNames[i] := 'cdr                 '; i := i+1;
   printNames[i] := 'number?             '; i := i+1;
   printNames[i] := 'symbol?             '; i := i+1;
   printNames[i] := 'list?               '; i := i+1;
   printNames[i] := 'null?               '; i := i+1;
   printNames[i] := 'print               '; i := i+1;
   printNames[i] := 'T                   ';
   numNames := i;
   numBuiltins := i
end; (* initNames *)

(* install - insert new name into printNames                     *)
function install (nm: NAMESTRING): NAME;
var
   i: integer;
   found: Boolean;
begin
   i := 1; found := false;
   while (i <= numNames) and not found
   do if nm = printNames[i]
      then found := true
      else i := i+1;
   if not found
   then begin
           if i > MAXNAMES
           then begin
                   writeln('No more room for names');
                   goto 99
                end;
           numNames := i;
           printNames[i] := nm
        end;
   install := i
end; (* install *)

(* prName - print name nm                                        *)
procedure prName (nm: NAME);
var i: integer;
begin
   i := 1;
   while i <= NAMELENG
   do if printNames[nm][i] <> ' '
      then begin
              write(printNames[nm][i]);
              i := i+1
           end
      else i := NAMELENG+1
end; (* prName *)

(* primOp - translate NAME optr to corresponding BUILTINOP       *)
function primOp (optr: NAME): BUILTINOP;
var
   op: BUILTINOP;
   i: integer;
begin
   op := IFOP; (* N.B. IFOP is first value in BUILTINOPS *)
   for i := 1 to optr-1 do op := succ(op);
   primOp := op
end; (* primOp *)

(*****************************************************************
 *                        INPUT                                  *
 *****************************************************************)

(* isDelim - check if c is a delimiter                           *)
function isDelim (c: char): Boolean;
begin
   isDelim := c in ['(', ')', ' ', COMMENTCHAR]
end; (* isDelim *)

(* skipblanks - return next non-blank position in userinput      *)
function skipblanks (p: integer): integer;
begin
   while userinput[p] = ' ' do p := p+1;
   skipblanks := p
end; (* skipblanks *)

(* matches - check if string nm matches userinput[s .. s+leng]   *)
function matches (s: integer; leng: NAMESIZE;
                   nm: NAMESTRING): Boolean;
var
   match: Boolean;
   i: integer;
begin
   match := true; i := 1;
   while match and (i <= leng) do begin
      if userinput[s] <> nm[i] then match := false;
      i := i+1;
      s := s+1
      end;
   if not isDelim(userinput[s]) then match := false;
   matches := match
end; (* matches *)

(* reader - read char's into userinput; be sure input not blank  *)
procedure reader;

(* readInput - read char's into userinput                        *)
   procedure readInput;

   var c: char;

(* nextchar - read next char - filter tabs and comments          *)
      procedure nextchar (var c: char);
      begin
         read(c);
         if c = chr(TABCODE)
         then c := ' '
         else if c = COMMENTCHAR
              then begin while not eoln do read(c); c := ' ' end
      end; (* nextchar *)

(* readParens - read char's, ignoring newlines, to matching ')'  *)
      procedure readParens;
      var
         parencnt: integer; (* current depth of parentheses *)
         c: char;
      begin
         parencnt := 1; (* '(' just read *)
         repeat
            if eoln then write(PROMPT2);
            nextchar(c);
            pos := pos+1;
            if pos = MAXINPUT
            then begin
                    writeln('User input too long');
                    goto 99
                 end;
            userinput[pos] := c;
            if c = '(' then parencnt := parencnt+1;
            if c = ')' then parencnt := parencnt-1
         until parencnt = 0
      end; (* readParens *)

   begin (* readInput *)
      write(PROMPT);
      pos := 0;
      repeat
         pos := pos+1;
         if pos = MAXINPUT
         then begin
                 writeln('User input too long');
                 goto 99
              end;
         nextchar(c);
         userinput[pos] := c;
         if userinput[pos] = '(' then readParens
      until eoln;
      inputleng := pos;
      userinput[pos+1] := COMMENTCHAR (* sentinel *)
   end; (* readInput *)

begin (* reader *)
    repeat
       readInput;
       pos := skipblanks(1);
    until pos <= inputleng (* ignore blank lines *)
end; (* reader *)

(* parseName - return (installed) NAME starting at userinput[pos]*)
function parseName: NAME;
var
   nm: NAMESTRING; (* array to accumulate characters *)
   leng: NAMESIZE; (* length of name *)
begin
   leng := 0;
   while (pos <= inputleng) and not isDelim(userinput[pos])
   do begin
         if leng = NAMELENG
         then begin
                 writeln('Name too long, begins: ', nm);
                 goto 99
              end;
         leng := leng+1;
         nm[leng] := userinput[pos];
         pos := pos+1
      end;
   if leng = 0
   then begin
           writeln('Error: expected name, instead read: ',
                   userinput[pos]);
           goto 99
        end;
   for leng := leng+1 to NAMELENG do nm[leng] := ' ';
   pos := skipblanks(pos); (* skip blanks after name *)
   parseName := install(nm)
end; (* parseName *)

(* isNumber - check if a number begins at pos                    *)
function isNumber (pos: integer): Boolean;

(* isDigits - check if sequence of digits begins at pos          *)
   function isDigits (pos: integer): Boolean;
   begin
      if not (userinput[pos] in ['0'..'9'])
      then isDigits := false
      else begin
              isDigits := true;
              while userinput[pos] in ['0'..'9'] do pos := pos+1;
              if not isDelim(userinput[pos])
              then isDigits := false
           end
   end; (* isDigits *)

begin (* isNumber *)
   isNumber := isDigits(pos) or
              ((userinput[pos] = '-') and isDigits(pos+1))
end; (* isNumber *)

(* isValue - check if a number or quoted const begins at pos     *)
function isValue (pos: integer): Boolean;
begin
   isValue:= (userinput[pos] = '''') or isNumber(pos)
end; (* isValue *)

(* parseVal - return S-expression starting at userinput[pos]     *)
function parseVal: SEXP;

(* parseSExp - return quoted S-expr starting at userinput[pos]   *)
   function parseSExp: SEXP;

   var s: SEXP;

(* parseInt - return number starting at userinput[pos]           *)
      function parseInt: SEXP;
      var sum, sign: integer;
      begin
         s := allocSExp(NUMSXP);
         sum := 0; sign := 1;
         if userinput[pos] = '-'
         then begin
                 sign := -1;
                 pos := pos+1
              end;
         while userinput[pos] in ['0'..'9'] do begin
            sum := 10*sum + (ord(userinput[pos]) - ord('0'));
            pos := pos+1
            end;
         s^.intval := sum * sign;
         pos := skipblanks(pos); (* skip blanks after number *)
         parseInt := s
      end; (* parseInt *)

(* parseSym - return symbol starting at userinput[pos]           *)
      function parseSym: SEXP;
      begin
         s := allocSExp(SYMSXP);
         s^.symval := parseName;
         parseSym := s
      end; (* parseSym *)

(* parseList - return list starting at userinput[pos]            *)
      function parseList: SEXP;
      var car, cdr: SEXP;
      begin
         if userinput[pos] = ')'
         then begin
                 parseList := allocSExp(NILSXP);
                 pos := skipblanks(pos+1)
              end
         else begin
                 car := parseSExp;
                 cdr := parseList;
                 s := allocSExp(LISTSXP);
                 s^.carval := car;
                 increfcnt(car);
                 s^.cdrval := cdr;
                 increfcnt(cdr);
                 parseList := s
              end
      end; (* parseList *)

   begin (* parseSExp *)
      if isNumber(pos)
      then parseSExp := parseInt
      else if userinput[pos] = '('
           then begin
                   pos := skipblanks(pos+1);
                   parseSExp := parseList
                end
           else parseSExp := parseSym
   end; (* parseSExp *)

begin (* parseVal *)
   if userinput[pos] = '''' then pos := pos+1;
   parseVal := parseSExp
end; (* parseVal *)

function parseEL: EXPLIST; forward;

(* parseExp - return EXP starting at userinput[pos]              *)
function parseExp: EXP;
var
   nm: NAME;
   el: EXPLIST;
begin
   if userinput[pos] = '('
   then begin   (* APEXP *)
           pos := skipblanks(pos+1); (* skip '( ..' *)
           nm := parseName;
           el := parseEL;
           parseExp := mkAPEXP(nm, el)
        end
   else if isValue(pos)
        then parseExp := mkVALEXP(parseVal)   (* VALEXP *)
        else parseExp := mkVAREXP(parseName)  (* VAREXP *)
end; (* parseExp *)

(* parseEL - return EXPLIST starting at userinput[pos]           *)
function parseEL;
var
   e: EXP;
   el: EXPLIST;
begin
   if userinput[pos] = ')'
   then begin
           pos := skipblanks(pos+1); (* skip ') ..' *)
           parseEL := nil
        end
   else begin
           e := parseExp;
           el := parseEL;
           parseEL := mkExplist(e, el)
        end
end; (* parseEL *)

(* parseNL - return NAMELIST starting at userinput[pos]          *)
function parseNL: NAMELIST;
var
   nm: NAME;
   nl: NAMELIST;
begin
   if userinput[pos] = ')'
   then begin
           pos := skipblanks(pos+1); (* skip ') ..' *)
           parseNL := nil
        end
   else begin
           nm := parseName;
           nl := parseNL;
           parseNL := mkNamelist(nm, nl)
        end
end; (* parseNL *)

(* parseDef - parse function definition at userinput[pos]        *)
function parseDef: NAME;
var
   fname: NAME;        (* function name *)
   nl: NAMELIST;       (* formal parameters *)
   e: EXP;             (* body *)

(* processExpVars - insert offsets in all VAREXP's within e      *)
   procedure processExpVars (e: EXP);

(* offsetOfVar - return location of nm in nl, or zero            *)
      function offsetOfVar (nm: NAME; nl: NAMELIST): integer;
      var
         i: integer;
         found: Boolean;
      begin
         i := 1; found := false;
         while (nl <> nil) and not found do
            if nm = nl^.head
            then found := true
            else begin
                    i := i+1;
                    nl := nl^.tail
                 end;
         if not found then i := 0;
         offsetOfVar := i
      end; (* offsetOfVar *)

(* processELVars - apply processExpVars to each expression in el *)
      procedure processELVars (el: EXPLIST);
      begin
         while el <> nil do begin
            processExpVars(el^.head);
            el := el^.tail
            end
      end; (* processELVars *)

   begin (* processExpVars *)
      with e^ do
         case etype of
            VALEXP: ;
            VAREXP: offset := offsetOfVar(varble, nl);
            APEXP: processELVars(args)
         end
   end; (* processExpVars *)

begin (* parseDef *)
   pos := skipblanks(pos+1); (* skip '( ..' *)
   pos := skipblanks(pos+6); (* skip 'define ..' *)
   fname := parseName;
   pos := skipblanks(pos+1); (* skip '( ..' *)
   nl := parseNL;
   e := parseExp;
   pos := skipblanks(pos+1); (* skip ') ..' *)
   newFunDef(fname, nl, e);
   processExpVars(e);
   parseDef := fname
end; (* parseDef *)

(*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************)

(* emptyEnv - return an environment with no bindings             *)
function emptyEnv: ENV;
begin
   emptyEnv := mkEnv(nil, nil)
end; (* emptyEnv *)

(* bindVar - bind variable nm to value s in environment rho      *)
procedure bindVar (nm: NAME; s: SEXP; rho: ENV);
begin
   increfcnt(s);
   rho^.vars := mkNamelist(nm, rho^.vars);
   rho^.values := mkValuelist(s, rho^.values)
end; (* bindVar *)

(* findVar - look up nm in rho                                   *)
function findVar (nm: NAME; rho: ENV): VALUELIST;
var
   nl: NAMELIST;
   vl: VALUELIST;
   found: Boolean;
begin
   found := false;
   nl := rho^.vars;
   vl := rho^.values;
   while (nl <> nil) and not found do
      if nl^.head = nm
      then found := true
      else begin
              nl := nl^.tail;
              vl := vl^.tail
           end;
   findVar := vl
end; (* findVar *)

(* assign - assign value s to variable nm in rho                 *)
procedure assign (nm: NAME; s: SEXP; rho: ENV);
var varloc: VALUELIST;
begin
   increfcnt(s);
   varloc := findVar(nm, rho);
   decrefcnt(varloc^.head);
   varloc^.head := s
end; (* assign *)

(* fetch - return SEXP bound to nm in rho                        *)
function fetch (nm: NAME; rho: ENV): SEXP;
var vl: VALUELIST;
begin
   vl := findVar(nm, rho);
   fetch := vl^.head
end; (* fetch *)

(* isBound - check if nm is bound in rho                         *)
function isBound (nm: NAME; rho: ENV): Boolean;
begin
   isBound := findVar(nm, rho) <> nil
end; (* isBound *)
(*****************************************************************
 *                     S-EXPRESSIONS                             *
 *****************************************************************)

(* prValue - print S-expression s                                *)
procedure prValue (s: SEXP);
var s1: SEXP;
begin
   with s^ do
      case sxptype of
         NILSXP: write('()');
         NUMSXP: write(intval:1);
         SYMSXP: prName(symval);
         LISTSXP:
            begin
               write('(');
               prValue(carval);
               s1 := cdrval;
               while s1^.sxptype = LISTSXP do begin
                  write(' ');
                  prValue(s1^.carval);
                  s1 := s1^.cdrval
                  end;
               write(')')
            end
      end (* case and with *)
end; (* prValue *)

(* isTrueVal - return true if s is true (non-NIL) value          *)
function isTrueVal (s: SEXP): Boolean;
begin
   isTrueVal := s^.sxptype <> NILSXP
end; (* isTrueVal *)

(* applyValueOp - apply VALUEOP op to arguments on top of stack  *)
procedure applyValueOp (op: VALUEOP);

var
   result: SEXP;
   s1, s2: SEXP;

(* applyArithOp - apply binary, arithmetic VALUEOP to arguments  *)
   procedure applyArithOp (n1, n2: integer);
   begin
      result := allocSExp(NUMSXP);
      with result^ do
         case op of
            PLUSOP: intval := n1+n2;
            MINUSOP: intval := n1-n2;
            TIMESOP: intval := n1*n2;
            DIVOP: intval := n1 div n2
         end
   end; (* applyArithOp *)

(* applyRelOp - apply binary, relational VALUEOP to arguments    *)
   procedure applyRelOp (n1, n2: integer) ;
   begin
      case op of
         LTOP: if n1 < n2 then result := trueValue;
         GTOP: if n1 > n2 then result := trueValue
      end
   end; (* applyRelOp *)

(* arity - return number of arguments expected by op             *)
   function arity (op: VALUEOP): integer;
   begin
      if op in [PLUSOP .. CONSOP] then arity := 2 else arity := 1
   end; (* arity *)

begin (* applyValueOp *)
   result := nilValue;
   s1 := topArg; (* 1st actual *)
   if arity(op) = 2
   then begin
           s2 := s1; (* 1st actual was really 2nd actual *)
           s1 := belowtopArg (* 1st actual *)
        end;
   if op in [PLUSOP .. DIVOP, LTOP .. GTOP]
   then if (s1^.sxptype = NUMSXP)
           and (s2^.sxptype = NUMSXP)
        then if op in [PLUSOP .. DIVOP]
             then applyArithOp(s1^.intval, s2^.intval)
             else applyRelOp(s1^.intval, s2^.intval)
        else begin
                write('Non-arithmetic arguments to ');
                prName(ord(op)+1);
                writeln;
                goto 99
             end
   else with s1^ do
           case op of
              EQOP:
                 if (sxptype = NILSXP)
                    and (s2^.sxptype = NILSXP)
                 then result := trueValue
                 else if (sxptype = NUMSXP)
                         and (s2^.sxptype = NUMSXP)
                         and (intval = s2^.intval)
                      then result := trueValue
                      else if (sxptype = SYMSXP)
                              and (s2^.sxptype = SYMSXP)
                              and (symval = s2^.symval)
                           then result := trueValue;
              CONSOP:
                 begin
                    result := allocSExp(LISTSXP);
                    with result^ do begin
                       carval := s1;
                       increfcnt(s1);
                       cdrval := s2;
                       increfcnt(s2)
                       end
                 end;
              CAROP:
                 if sxptype <> LISTSXP
                 then begin
                         write('Error: car applied to non-list: ');
                         prValue(s1);
                         writeln
                      end
                 else result := carval;
              CDROP:
                 if sxptype <> LISTSXP
                 then begin
                         write('Error: cdr applied to non-list: ');
                         prValue(s1);
                         writeln
                      end
                 else result := cdrval;
              NUMBERPOP:
                 if sxptype = NUMSXP then result := trueValue;
              SYMBOLPOP:
                 if sxptype = SYMSXP then result := trueValue;
              LISTPOP:
                 if sxptype = LISTSXP then result := trueValue;
              NULLPOP:
                 if sxptype = NILSXP then result := trueValue;
              PRINTOP:
                 begin prValue(s1); writeln; result := s1 end
           end; (* case and with *)
   popArgs(arity(op));
   pushArg(result)
end; (* applyValueOp *)

(*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************)

(* eval - return value of e; param's start at argStack[AR]       *)
procedure eval (e: EXP; AR: integer);

var
   s: SEXP;
   op: BUILTINOP;
   newAR: integer;

(* evalList - evaluate each expression in el                     *)
   procedure evalList (el: EXPLIST);
   begin
      if el <> nil
      then begin
              eval(el^.head, AR);
              evalList(el^.tail)
           end
   end; (* evalList *)

(* applyUserFun - apply nm; arg's start at argStack[newAR]       *)
   procedure applyUserFun (nm: NAME; newAR: integer);
   var f: FUNDEF;
   begin
      f := fetchFun(nm);
      if f = nil
      then begin
              write('Undefined function: ');
              prName(nm);
              writeln;
              goto 99
           end;
      with f^ do begin
         eval(body, newAR);
         s := topArg;
         stacktop := stacktop-1;
         popArgs(lengthNL(formals));
         argStack[stacktop] := s ;
         stacktop := stacktop+1
         end
   end; (* applyUserFun *)

(* applyCtrlOp - apply CONTROLOP op to args                      *)
   procedure applyCtrlOp (op: CONTROLOP; args: EXPLIST);
   var s: SEXP;
   begin
      with args^ do
         case op of
           IFOP:
              begin
                 eval(head,AR);
                 s := topArg;
                 if isTrueVal(s)
                 then begin popArgs(1); eval(tail^.head, AR) end
                 else begin popArgs(1); eval(tail^.tail^.head, AR) end
              end;
           WHILEOP:
              begin
                 pushArg(nilValue);
                 eval(head, AR);
                 s := topArg;
                 while isTrueVal(s) do begin
                    popArgs(2);
                    eval(tail^.head, AR);
                    eval(head, AR);
                    s := topArg
                    end;
                 popArgs(1)
              end;
           SETOP:
              begin
                 eval(tail^.head, AR);
                 s := topArg;
                 if head^.offset>0
                 then begin
                         increfcnt(s);
                         decrefcnt(argStack[AR+head^.offset-1]);
                         argStack[AR+head^.offset-1] := s
                      end
                 else if isBound(head^.varble, globalEnv)
                      then assign(head^.varble, s, globalEnv)
                      else bindVar(head^.varble, s, globalEnv)
              end;
           BEGINOP: 
              begin
                 while args^.tail <> nil do
                    begin
                       eval(args^.head, AR);
                       popArgs(1);
                       args := args^.tail
                    end;
                 eval(args^.head, AR)
              end
         end (* case and with *)
   end; (* applyCtrlOp *)

begin (* eval *)
   with e^ do
      case etype of
         VALEXP:
            pushArg(sxp);
         VAREXP:
            begin
               if offset > 0
               then s := argStack[AR+offset-1]
               else if isBound(varble, globalEnv)
                    then s := fetch(varble, globalEnv)
                    else begin
                            write('Undefined variable: ');
                            prName(varble);
                            writeln;
                            goto 99
                         end;
               pushArg(s)
            end;
         APEXP: 
            if optr > numBuiltins
            then begin
                    newAR := stacktop;
                    evalList(args);
                    applyUserFun(optr, newAR)
                 end
            else begin
                    op := primOp(optr);
                    if op in [IFOP .. BEGINOP]
                    then applyCtrlOp(op, args)
                    else begin
                            evalList(args);
                            applyValueOp(op)
                         end
                 end
      end (* case and with *)
end; (* eval *)

(*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************)

begin (* lisp main *)
   initNames;
   initMemory;

   nilValue := allocSExp(NILSXP);
   increfcnt(nilValue);
   trueValue := allocSExp(SYMSXP); trueValue^.symval := numNames;
   increfcnt(trueValue);

   globalEnv := emptyEnv;

   quittingtime := false;
99:
   while not quittingtime do begin
      reader;
      if matches(pos, 4, 'quit                ')
      then quittingtime := true
      else if (userinput[pos] = '(') and
              matches(skipblanks(pos+1), 6, 'define              ')
           then begin
                   prName(parseDef);
                   writeln
                end
           else begin
                   currentExp := parseExp;
                   initStack;
                   eval(currentExp, 0);
                   prValue(topArg);
                   popArgs(1);
                   rlsExp(currentExp);
                   writeln;
                   writeln
                end
      end (* while *)
end. (* lisp *)
   


