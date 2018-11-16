/* Output from p2c, the Pascal-to-C translator */
/* From input file "chap1.p" */


/*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************/

#include <p2c/p2c.h>


#define NAMELENG        20   /* Maximum length of a name */
#define MAXNAMES        100   /* Maximum number of different names */
#define MAXINPUT        500   /* Maximum length of an input */

#define PROMPT          "-> "
#define PROMPT2         "> "
#define COMMENTCHAR     ";"

#define TABCODE         9   /* in ASCII */


typedef Char NAMESTRING[NAMELENG];

/* a NAME is an index in printNames */

typedef enum {
  IFOP, WHILEOP, SETOP, BEGINOP, PLUSOP, MINUSOP, TIMESOP, DIVOP, EQOP, LTOP,
  GTOP, PRINTOP
} BUILTINOP;


typedef enum {
  VALEXP, VAREXP, APEXP
} EXPTYPE;

typedef struct EXPREC {
  EXPTYPE etype;
  union {
    long num;
    char varble;
    struct {
      char optr;
      struct EXPLISTREC *args;
    } U2;
  } UU;
} EXPREC;

typedef struct EXPLISTREC {
  EXPREC *head;
  struct EXPLISTREC *tail;
} EXPLISTREC;

typedef struct VALUELISTREC {
  long head;
  struct VALUELISTREC *tail;
} VALUELISTREC;

typedef struct NAMELISTREC {
  char head;
  struct NAMELISTREC *tail;
} NAMELISTREC;

typedef struct ENVREC {
  NAMELISTREC *vars;
  VALUELISTREC *values;
} ENVREC;

typedef struct FUNDEFREC {
  char funname;
  NAMELISTREC *formals;
  EXPREC *body;
  struct FUNDEFREC *nextfundef;
} FUNDEFREC;


Static FUNDEFREC *fundefs;

Static ENVREC *globalEnv;

Static EXPREC *currentExp;

Static Char userinput[MAXINPUT];
Static short inputleng, pos_;

Static NAMESTRING printNames[MAXNAMES];
Static char numNames, numBuiltins;

Static boolean quittingtime;


/*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************/

/* mkVALEXP - return an EXP of type VALEXP with num n            */
Static EXPREC *mkVALEXP(n)
long n;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VALEXP;
  e->UU.num = n;
  return e;
}  /* mkVALEXP */


/* mkVAREXP - return an EXP of type VAREXP with varble nm        */
Static EXPREC *mkVAREXP(nm)
char nm;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VAREXP;
  e->UU.varble = nm;
  return e;
}  /* mkVAREXP */


/* mkAPEXP - return EXP of type APEXP w/ optr op and args el     */
Static EXPREC *mkAPEXP(op, el)
char op;
EXPLISTREC *el;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = APEXP;
  e->UU.U2.optr = op;
  e->UU.U2.args = el;
  return e;
}  /* mkAPEXP */


/* mkExplist - return an EXPLIST with head e and tail el         */
Static EXPLISTREC *mkExplist(e, el)
EXPREC *e;
EXPLISTREC *el;
{
  EXPLISTREC *newel;

  newel = (EXPLISTREC *)Malloc(sizeof(EXPLISTREC));
  newel->head = e;
  newel->tail = el;
  return newel;
}  /* mkExplist */


/* mkNamelist - return a NAMELIST with head n and tail nl        */
Static NAMELISTREC *mkNamelist(nm, nl)
char nm;
NAMELISTREC *nl;
{
  NAMELISTREC *newnl;

  newnl = (NAMELISTREC *)Malloc(sizeof(NAMELISTREC));
  newnl->head = nm;
  newnl->tail = nl;
  return newnl;
}  /* mkNamelist */


/* mkValuelist - return an VALUELIST with head n and tail vl     */
Static VALUELISTREC *mkValuelist(n, vl)
long n;
VALUELISTREC *vl;
{
  VALUELISTREC *newvl;

  newvl = (VALUELISTREC *)Malloc(sizeof(VALUELISTREC));
  newvl->head = n;
  newvl->tail = vl;
  return newvl;
}  /* mkValuelist */


/* mkEnv - return an ENV with vars nl and values vl              */
Static ENVREC *mkEnv(nl, vl)
NAMELISTREC *nl;
VALUELISTREC *vl;
{
  ENVREC *rho;

  rho = (ENVREC *)Malloc(sizeof(ENVREC));
  rho->vars = nl;
  rho->values = vl;
  return rho;
}  /* mkEnv */


/* lengthVL - return length of VALUELIST vl                      */
Static long lengthVL(vl)
VALUELISTREC *vl;
{
  long i;

  i = 0;
  while (vl != NULL) {
    i++;
    vl = vl->tail;
  }
  return i;
}  /* lengthVL */


/* lengthNL - return length of NAMELIST nl                       */
Static long lengthNL(nl)
NAMELISTREC *nl;
{
  long i;

  i = 0;
  while (nl != NULL) {
    i++;
    nl = nl->tail;
  }
  return i;
}  /* lengthNL */


/*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************/

/* fetchFun - get function definition of fname from fundefs      */
Static FUNDEFREC *fetchFun(fname)
char fname;
{
  FUNDEFREC *f;
  boolean found;

  found = false;
  f = fundefs;
  while (f != NULL && !found) {
    if (f->funname == fname)
      found = true;
    else
      f = f->nextfundef;
  }
  return f;
}  /* fetchFun */


/* newFunDef - add new function fname w/ parameters nl, body e   */
Static Void newFunDef(fname, nl, e)
char fname;
NAMELISTREC *nl;
EXPREC *e;
{
  FUNDEFREC *f;

  f = fetchFun(fname);
  if (f == NULL) {   /* fname not yet defined as a function */
    f = (FUNDEFREC *)Malloc(sizeof(FUNDEFREC));
    f->nextfundef = fundefs;   /* place new FUNDEFREC */
    fundefs = f;   /* on fundefs list */
  }
  f->funname = fname;
  f->formals = nl;
  f->body = e;
}  /* newFunDef */


/* initNames - place all pre-defined names into printNames       */
Static Void initNames()
{
  long i;

  fundefs = NULL;
  i = 1;
  memcpy(printNames[i - 1], "if                  ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "while               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "set                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "begin               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "+                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "-                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "*                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "/                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "=                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "<                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], ">                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "print               ", sizeof(NAMESTRING));
  numNames = i;
  numBuiltins = i;
}  /* initNames */


Static jmp_buf _JL99;


/* install - insert new name into printNames                     */
Static char install(nm)
Char *nm;
{
  long i;
  boolean found;

  i = 1;
  found = false;
  while (i <= numNames && !found) {
    if (!memcmp(nm, printNames[i - 1], sizeof(NAMESTRING)))
      found = true;
    else
      i++;
  }
  if (found)
    return i;
  if (i > MAXNAMES) {
    printf("No more room for names\n");
    longjmp(_JL99, 1);
  }
  numNames = i;
  memcpy(printNames[i - 1], nm, sizeof(NAMESTRING));
  return i;
}  /* install */


/* prName - print name nm                                        */
Static Void prName(nm)
char nm;
{
  long i;

  i = 1;
  while (i <= NAMELENG) {
    if (printNames[nm - 1][i - 1] != ' ') {
      putchar(printNames[nm - 1][i - 1]);
      i++;
    } else
      i = NAMELENG + 1;
  }
}  /* prName */


/* primOp - translate NAME optr to corresponding BUILTINOP       */
Static BUILTINOP primOp(optr)
char optr;
{
  BUILTINOP op;
  long i;

  op = IFOP;   /* N.B. IFOP is first value in BUILTINOPS */
  for (i = 1; i < optr; i++)
    op = (BUILTINOP)((long)op + 1);
  return op;
}  /* primOp */


/*****************************************************************
 *                        INPUT                                  *
 *****************************************************************/

/* isDelim - check if c is a delimiter                           */
Static boolean isDelim(c)
Char c;
{
  return (c == ';' || c == ' ' || c == ')' || c == '(');
}  /* isDelim */


/* skipblanks - return next non-blank position in userinput      */
Static long skipblanks(p)
long p;
{
  while (userinput[p - 1] == ' ')
    p++;
  return p;
}  /* skipblanks */


/* matches - check if string nm matches userinput[s .. s+leng]   */
Static boolean matches(s, leng, nm)
long s;
char leng;
Char *nm;
{
  boolean match;
  long i;

  match = true;
  i = 1;
  while (match && i <= leng) {
    if (userinput[s - 1] != nm[i - 1])
      match = false;
    i++;
    s++;
  }
  if (!isDelim(userinput[s - 1]))
    match = false;
  return match;
}  /* matches */


/* nextchar - read next char - filter tabs and comments          */
Local Void nextchar(c)
Char *c;
{
  Char STR1[256];

  *c = getchar();
  if (*c == '\n')
    *c = ' ';
  if (*c == (Char)TABCODE) {
    *c = ' ';
    return;
  }
  sprintf(STR1, "%c", *c);
  if (strcmp(STR1, COMMENTCHAR))
    return;
  while (!P_eoln(stdin)) {
    *c = getchar();
    if (*c == '\n')
      *c = ' ';
  }
  *c = ' ';
}  /* nextchar */

/* readParens - read char's, ignoring newlines, to matching ')'  */
Local Void readParens()
{
  long parencnt;   /* current depth of parentheses */
  Char c;

  parencnt = 1;   /* '(' just read */
  do {
    if (P_eoln(stdin))
      fputs(PROMPT2, stdout);
    nextchar(&c);
    pos_++;
    if (pos_ == MAXINPUT) {
      printf("User input too long\n");
      longjmp(_JL99, 1);
    }
    userinput[pos_ - 1] = c;
    if (c == '(')
      parencnt++;
    if (c == ')')
      parencnt--;
  } while (parencnt != 0);   /* readParens */
}

Local Void readInput()
{
  Char c;

  fputs(PROMPT, stdout);
  pos_ = 0;
  do {
    pos_++;
    if (pos_ == MAXINPUT) {
      printf("User input too long\n");
      longjmp(_JL99, 1);
    }
    nextchar(&c);
    userinput[pos_ - 1] = c;
    if (userinput[pos_ - 1] == '(')
      readParens();
  } while (!P_eoln(stdin));
  inputleng = pos_;
  userinput[pos_] = ';';   /* sentinel */
}  /* readInput */


/* reader - read char's into userinput; be sure input not blank  */
Static Void reader()
{

  /* readInput - read char's into userinput                        */
  do {
    readInput();
    pos_ = skipblanks(1L);   /* ignore blank lines */
  } while (pos_ > inputleng);   /* reader */
}


/* parseName - return (installed) NAME starting at userinput[pos]*/
Static char parseName()
{
  NAMESTRING nm;   /* array to accumulate characters */
  char leng;   /* length of name */

  leng = 0;
  while ((pos_ <= inputleng) & (!isDelim(userinput[pos_ - 1]))) {
    if (leng == NAMELENG) {
      printf("Name too long, begins: %.*s\n", NAMELENG, nm);
      longjmp(_JL99, 1);
    }
    leng++;
    nm[leng - 1] = userinput[pos_ - 1];
    pos_++;
  }
  if (leng == 0) {
    printf("Error: expected name, instead read: %c\n", userinput[pos_ - 1]);
    longjmp(_JL99, 1);
  }
  for (; leng < NAMELENG; leng++)
    nm[leng] = ' ';
  pos_ = skipblanks((long)pos_);   /* skip blanks after name */
  return (install(nm));
}  /* parseName */


Local boolean isDigits(pos)
long pos;
{
  boolean Result;

  if (!isdigit(userinput[pos - 1]))
    return false;
  Result = true;
  while (isdigit(userinput[pos - 1]))
    pos++;
  if (!isDelim(userinput[pos - 1]))
    return false;
  return Result;
}  /* isDigits */


/* isNumber - check if a number begins at pos                    */
Static boolean isNumber(pos)
long pos;
{

  /* isDigits - check if sequence of digits begins at pos          */
  return (isDigits(pos) | ((userinput[pos - 1] == '-') & isDigits(pos + 1)));
}  /* isNumber */


/* parseVal - return number starting at userinput[pos]           */
Static long parseVal()
{
  long n, sign;

  n = 0;
  sign = 1;
  if (userinput[pos_ - 1] == '-') {
    sign = -1;
    pos_++;
  }
  while (isdigit(userinput[pos_ - 1])) {
    n = n * 10 + userinput[pos_ - 1] - '0';
    pos_++;
  }
  pos_ = skipblanks((long)pos_);   /* skip blanks after number */
  return (n * sign);
}  /* parseVal */


Static EXPLISTREC *parseEL PV();


/* parseExp - return EXP starting at userinput[pos]              */
Static EXPREC *parseExp()
{
  char nm;
  EXPLISTREC *el;

  if (userinput[pos_ - 1] == '(') {  /* APEXP */
    pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
    nm = parseName();
    el = parseEL();
    return (mkAPEXP(nm, el));
  } else if (isNumber((long)pos_))
    return (mkVALEXP(parseVal()));   /* VALEXP */
  else
    return (mkVAREXP(parseName()));   /* VAREXP */
}  /* parseExp */


/* parseEL - return EXPLIST starting at userinput[pos]           */
Static EXPLISTREC *parseEL()
{
  EXPREC *e;
  EXPLISTREC *el;

  if (userinput[pos_ - 1] == ')') {
    pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
    return NULL;
  } else {
    e = parseExp();
    el = parseEL();
    return (mkExplist(e, el));
  }
}  /* parseEL */


/* parseNL - return NAMELIST starting at userinput[pos]          */
Static NAMELISTREC *parseNL()
{
  char nm;
  NAMELISTREC *nl;

  if (userinput[pos_ - 1] == ')') {
    pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
    return NULL;
  } else {
    nm = parseName();
    nl = parseNL();
    return (mkNamelist(nm, nl));
  }
}  /* parseNL */


/* parseDef - parse function definition at userinput[pos]        */
Static char parseDef()
{
  char fname;   /* function name */
  NAMELISTREC *nl;   /* formal parameters */
  EXPREC *e;   /* body */

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 6L);   /* skip 'define ..' */
  fname = parseName();
  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  nl = parseNL();
  e = parseExp();
  pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
  newFunDef(fname, nl, e);
  return fname;
}  /* parseDef */


/*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************/

/* emptyEnv - return an environment with no bindings             */
Static ENVREC *emptyEnv()
{
  return (mkEnv(NULL, NULL));
}  /* emptyEnv */


/* bindVar - bind variable nm to value n in environment rho      */
Static Void bindVar(nm, n, rho)
char nm;
long n;
ENVREC *rho;
{
  rho->vars = mkNamelist(nm, rho->vars);
  rho->values = mkValuelist(n, rho->values);
}  /* bindVar */


/* findVar - look up nm in rho                                   */
Static VALUELISTREC *findVar(nm, rho)
char nm;
ENVREC *rho;
{
  NAMELISTREC *nl;
  VALUELISTREC *vl;
  boolean found;

  found = false;
  nl = rho->vars;
  vl = rho->values;
  while (nl != NULL && !found) {
    if (nl->head == nm)
      found = true;
    else {
      nl = nl->tail;
      vl = vl->tail;
    }
  }
  return vl;
}  /* findVar */


/* assign - assign value n to variable nm in rho                 */
Static Void assign(nm, n, rho)
char nm;
long n;
ENVREC *rho;
{
  VALUELISTREC *varloc;

  varloc = findVar(nm, rho);
  varloc->head = n;
}  /* assign */


/* fetch - return number bound to nm in rho                      */
Static long fetch(nm, rho)
char nm;
ENVREC *rho;
{
  VALUELISTREC *vl;

  vl = findVar(nm, rho);
  return (vl->head);
}  /* fetch */


/* isBound - check if nm is bound in rho                         */
Static boolean isBound(nm, rho)
char nm;
ENVREC *rho;
{
  return (findVar(nm, rho) != NULL);
}  /* isBound */


/*****************************************************************
 *                     NUMBERS                                   *
 *****************************************************************/

/* prValue - print number n                                      */
Static Void prValue(n)
long n;
{
  printf("%ld", n);
}  /* prValue */


/* isTrueVal - return true if n is a true (non-zero) value       */
Static boolean isTrueVal(n)
long n;
{
  return (n != 0);
}  /* isTrueVal */


/* arity - return number of arguments expected by op             */
Local long arity(op)
BUILTINOP op;
{
  if (((1L << ((long)op)) & ((1 << ((long)GTOP + 1)) - (1 << ((long)PLUSOP)))) != 0)
    return 2;
  else
    return 1;
}  /* arity */


/* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  */
Static long applyValueOp(op, vl)
BUILTINOP op;
VALUELISTREC *vl;
{
  long n, n1, n2;

  if (arity(op) != lengthVL(vl)) {
    printf("Wrong number of arguments to ");
    prName((int)op + 1);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  n1 = vl->head;   /* 1st actual */
  if (arity(op) == 2)   /* 2nd actual */
    n2 = vl->tail->head;
  switch (op) {

  case PLUSOP:
    n = n1 + n2;
    break;

  case MINUSOP:
    n = n1 - n2;
    break;

  case TIMESOP:
    n = n1 * n2;
    break;

  case DIVOP:
    n = n1 / n2;
    break;

  case EQOP:
    if (n1 == n2)
      n = 1;
    else
      n = 0;
    break;

  case LTOP:
    if (n1 < n2)
      n = 1;
    else
      n = 0;
    break;

  case GTOP:
    if (n1 > n2)
      n = 1;
    else
      n = 0;
    break;

  case PRINTOP:
    prValue(n1);
    putchar('\n');
    n = n1;
    break;
  }/* case */
  return n;
}  /* applyValueOp */


Static long eval PP((EXPREC *e, ENVREC *rho));

/* Local variables for eval: */
struct LOC_eval {
  ENVREC *rho;
} ;

/* evalList - evaluate each expression in el                     */
Local VALUELISTREC *evalList(el, LINK)
EXPLISTREC *el;
struct LOC_eval *LINK;
{
  long h;
  VALUELISTREC *t;

  if (el == NULL)
    return NULL;
  else {
    h = eval(el->head, LINK->rho);
    t = evalList(el->tail, LINK);
    return (mkValuelist(h, t));
  }
}  /* evalList */

/* applyUserFun - look up definition of nm and apply to actuals  */
Local long applyUserFun(nm, actuals, LINK)
char nm;
VALUELISTREC *actuals;
struct LOC_eval *LINK;
{
  FUNDEFREC *f;
  ENVREC *rho;

  f = fetchFun(nm);
  if (f == NULL) {
    printf("Undefined function: ");
    prName(nm);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  if (lengthNL(f->formals) != lengthVL(actuals)) {
    printf("Wrong number of arguments to: ");
    prName(nm);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  rho = mkEnv(f->formals, actuals);
  return (eval(f->body, rho));
}  /* applyUserFun */

/* applyCtrlOp - apply CONTROLOP op to args in rho               */
Local long applyCtrlOp(op, args, LINK)
BUILTINOP op;
EXPLISTREC *args;
struct LOC_eval *LINK;
{
  long Result, n;
  EXPLISTREC *WITH;

  WITH = args;
  switch (op) {

  case IFOP:
    if (isTrueVal(eval(WITH->head, LINK->rho)))
      Result = eval(WITH->tail->head, LINK->rho);
    else
      Result = eval(WITH->tail->tail->head, LINK->rho);
    break;

  case WHILEOP:
    n = eval(WITH->head, LINK->rho);
    while (isTrueVal(n)) {
      n = eval(WITH->tail->head, LINK->rho);
      n = eval(WITH->head, LINK->rho);
    }
    Result = n;
    break;

  case SETOP:
    n = eval(WITH->tail->head, LINK->rho);
    if (isBound(WITH->head->UU.varble, LINK->rho))
      assign(WITH->head->UU.varble, n, LINK->rho);
    else if (isBound(WITH->head->UU.varble, globalEnv))
      assign(WITH->head->UU.varble, n, globalEnv);
    else
      bindVar(WITH->head->UU.varble, n, globalEnv);
    Result = n;
    break;

  case BEGINOP:
    while (args->tail != NULL) {
      n = eval(args->head, LINK->rho);
      args = args->tail;
    }
    Result = eval(args->head, LINK->rho);
    break;
  }/* case and with */
  return Result;
}  /* applyCtrlOp */


/*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************/

/* eval - return value of expression e in local environment rho  */
Static long eval(e, rho_)
EXPREC *e;
ENVREC *rho_;
{
  struct LOC_eval V;
  long Result;
  BUILTINOP op;

  V.rho = rho_;
  switch (e->etype) {

  case VALEXP:
    Result = e->UU.num;
    break;

  case VAREXP:
    if (isBound(e->UU.varble, V.rho))
      Result = fetch(e->UU.varble, V.rho);
    else if (isBound(e->UU.varble, globalEnv))
      Result = fetch(e->UU.varble, globalEnv);
    else {
      printf("Undefined variable: ");
      prName(e->UU.varble);
      putchar('\n');
      longjmp(_JL99, 1);
    }
    break;

  case APEXP:
    if (e->UU.U2.optr > numBuiltins)
      Result = applyUserFun(e->UU.U2.optr, evalList(e->UU.U2.args, &V), &V);
    else {
      op = primOp(e->UU.U2.optr);
      if (((1L << ((long)op)) &
	   ((1 << ((long)BEGINOP + 1)) - (1 << ((long)IFOP)))) != 0)
	Result = applyCtrlOp(op, e->UU.U2.args, &V);
      else
	Result = applyValueOp(op, evalList(e->UU.U2.args, &V));
    }
    break;
  }/* case and with */
  return Result;
}  /* eval */


/*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************/

main(argc, argv)
int argc;
Char *argv[];
{  /* chapter1 main */
  PASCAL_MAIN(argc, argv);
  if (setjmp(_JL99))
    goto _L99;
  initNames();
  globalEnv = emptyEnv();

  quittingtime = false;
_L99:
  while (!quittingtime) {
    reader();
    if (matches((long)pos_, 4, "quit                ")) {
      quittingtime = true;
      break;
    }
    if ((userinput[pos_ - 1] == '(') & matches(skipblanks(pos_ + 1L), 6,
					       "define              ")) {
      prName(parseDef());
      putchar('\n');
    } else {
      currentExp = parseExp();
      prValue(eval(currentExp, emptyEnv()));
      printf("\n\n");
    }
  }  /* while */
  exit(0);
}  /* chapter1 */






/* End. */
