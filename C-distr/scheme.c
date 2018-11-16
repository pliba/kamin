/* Output from p2c, the Pascal-to-C translator */
/* From input file "scheme.p" */


/*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************/

#include <p2c/p2c.h>


#define NAMELENG        20   /* Maximum length of a name */
#define MAXNAMES        300   /* Maximum number of different names */
#define MAXINPUT        4000   /* Maximum length of an input */

#define PROMPT          "-> "
#define PROMPT2         "> "
#define COMMENTCHAR     ";"

#define TABCODE         9   /* in ASCII */


typedef Char NAMESTRING[NAMELENG];

/* a NAME is an index in printNames */

typedef enum {
  IFOP, WHILEOP, SETOP, BEGINOP, PLUSOP, MINUSOP, TIMESOP, DIVOP, EQOP, LTOP,
  GTOP, CONSOP, CAROP, CDROP, NUMBERPOP, SYMBOLPOP, LISTPOP, NULLPOP,
  PRIMOPPOP, CLOSUREPOP, PRINTOP
} BUILTINOP;


typedef enum {
  NILSXP, NUMSXP, SYMSXP, LISTSXP, CLOSXP, PRIMSXP
} SEXPTYPE;

typedef struct SEXPREC {
  SEXPTYPE sxptype;
  union {
    long intval;
    short symval;
    struct {
      struct SEXPREC *carval, *cdrval;
    } U3;
    struct {
      struct EXPREC *clofun;
      struct ENVREC *cloenv;
    } U4;
    BUILTINOP primval;
  } UU;
} SEXPREC;

typedef enum {
  VALEXP, VAREXP, APEXP, LAMEXP
} EXPTYPE;

typedef struct EXPREC {
  EXPTYPE etype;
  union {
    SEXPREC *sxp;
    short varble;
    struct {
      struct EXPREC *optr;
      struct EXPLISTREC *args;
    } U2;
    struct {
      struct NAMELISTREC *formals;
      struct EXPREC *lambdabody;
    } U3;
  } UU;
} EXPREC;

typedef struct EXPLISTREC {
  EXPREC *head;
  struct EXPLISTREC *tail;
} EXPLISTREC;

typedef struct VALUELISTREC {
  SEXPREC *head;
  struct VALUELISTREC *tail;
} VALUELISTREC;

typedef struct NAMELISTREC {
  short head;
  struct NAMELISTREC *tail;
} NAMELISTREC;

typedef struct ENVREC {
  NAMELISTREC *vars;
  VALUELISTREC *values;
  struct ENVREC *enclosing;
} ENVREC;


Static ENVREC *globalEnv;

Static EXPREC *currentExp;

Static Char userinput[MAXINPUT];
Static short inputleng, pos_;

Static NAMESTRING printNames[MAXNAMES];
Static short numNames, numBuiltins;

Static SEXPREC *nilValue, *trueValue;

Static boolean quittingtime;


/*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************/

/* mkVALEXP - return an EXP of type VALEXP with sxp s            */
Static EXPREC *mkVALEXP(s)
SEXPREC *s;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VALEXP;
  e->UU.sxp = s;
  return e;
}  /* mkVALEXP */


/* mkVAREXP - return an EXP of type VAREXP with varble nm        */
Static EXPREC *mkVAREXP(nm)
short nm;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VAREXP;
  e->UU.varble = nm;
  return e;
}  /* mkVAREXP */


/* mkAPEXP - return EXP of type APEXP w/ optr op and args el     */
Static EXPREC *mkAPEXP(op, el)
EXPREC *op;
EXPLISTREC *el;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = APEXP;
  e->UU.U2.optr = op;
  e->UU.U2.args = el;
  return e;
}  /* mkAPEXP */


/* mkLAMEXP - return EXP of type LAMEXP w/ formals f and body b  */
Static EXPREC *mkLAMEXP(f, b)
NAMELISTREC *f;
EXPREC *b;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = LAMEXP;
  e->UU.U3.formals = f;
  e->UU.U3.lambdabody = b;
  return e;
}  /* mkLAMEXP */


/* mkSExp - return SEXP of type t (but no value)                 */
Static SEXPREC *mkSExp(t)
SEXPTYPE t;
{
  SEXPREC *s;

  s = (SEXPREC *)Malloc(sizeof(SEXPREC));
  s->sxptype = t;
  return s;
}  /* mkSExp */


/* mkPRIMSXP - return SEXP of type PRIMSXP w/ value op           */
Static SEXPREC *mkPRIMSXP(op)
BUILTINOP op;
{
  SEXPREC *result;

  result = (SEXPREC *)Malloc(sizeof(SEXPREC));
  result->sxptype = PRIMSXP;
  result->UU.primval = op;
  return result;
}  /* mkPRIMSXP */


/* mkCLOSXP - return SEXP of type CLOSXP w/ expr e and env rho   */
Static SEXPREC *mkCLOSXP(e, rho)
EXPREC *e;
ENVREC *rho;
{
  SEXPREC *result;

  result = (SEXPREC *)Malloc(sizeof(SEXPREC));
  result->sxptype = CLOSXP;
  result->UU.U4.clofun = e;
  result->UU.U4.cloenv = rho;
  return result;
}  /* mkCLOSXP */


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
short nm;
NAMELISTREC *nl;
{
  NAMELISTREC *newnl;

  newnl = (NAMELISTREC *)Malloc(sizeof(NAMELISTREC));
  newnl->head = nm;
  newnl->tail = nl;
  return newnl;
}  /* mkNamelist */


/* mkValuelist - return an VALUELIST with head s and tail vl     */
Static VALUELISTREC *mkValuelist(s, vl)
SEXPREC *s;
VALUELISTREC *vl;
{
  VALUELISTREC *newvl;

  newvl = (VALUELISTREC *)Malloc(sizeof(VALUELISTREC));
  newvl->head = s;
  newvl->tail = vl;
  return newvl;
}  /* mkValuelist */


/* mkEnv - return an ENV with vars nl, value vl, enclosing rho   */
Static ENVREC *mkEnv(nl, vl, rho)
NAMELISTREC *nl;
VALUELISTREC *vl;
ENVREC *rho;
{
  ENVREC *newrho;

  newrho = (ENVREC *)Malloc(sizeof(ENVREC));
  newrho->vars = nl;
  newrho->values = vl;
  newrho->enclosing = rho;
  return newrho;
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

/* initNames - place all pre-defined names into printNames       */
Static Void initNames()
{
  long i;

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
  memcpy(printNames[i - 1], "cons                ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "car                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "cdr                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "number?             ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "symbol?             ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "list?               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "null?               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "primop?             ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "closure?            ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "print               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "T                   ", sizeof(NAMESTRING));
  numNames = i;
  numBuiltins = i;
}  /* initNames */


Static jmp_buf _JL99;


/* install - insert new name into printNames                     */
Static short install(nm)
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
short nm;
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
Static short parseName()
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


/* isValue - check if a number or quoted const begins at pos     */
Static boolean isValue(pos)
long pos;
{
  return ((userinput[pos - 1] == '\'') | isNumber(pos));
}  /* isValue */


Local SEXPREC *parseSExp PV();

/* Local variables for parseSExp: */
struct LOC_parseSExp {
  SEXPREC *s;
} ;

/* parseInt - return number starting at userinput[pos]           */
Local SEXPREC *parseInt(LINK)
struct LOC_parseSExp *LINK;
{
  long sum, sign;

  LINK->s = mkSExp(NUMSXP);
  sum = 0;
  sign = 1;
  if (userinput[pos_ - 1] == '-') {
    sign = -1;
    pos_++;
  }
  while (isdigit(userinput[pos_ - 1])) {
    sum = sum * 10 + userinput[pos_ - 1] - '0';
    pos_++;
  }
  LINK->s->UU.intval = sum * sign;
  pos_ = skipblanks((long)pos_);   /* skip blanks after number */
  return LINK->s;
}  /* parseInt */

/* parseSym - return symbol starting at userinput[pos]           */
Local SEXPREC *parseSym(LINK)
struct LOC_parseSExp *LINK;
{
  LINK->s = mkSExp(SYMSXP);
  LINK->s->UU.symval = parseName();
  return LINK->s;
}  /* parseSym */

/* parseList - return list starting at userinput[pos]            */
Local SEXPREC *parseList(LINK)
struct LOC_parseSExp *LINK;
{
  SEXPREC *Result, *car, *cdr;

  if (userinput[pos_ - 1] == ')') {
    Result = mkSExp(NILSXP);
    pos_ = skipblanks(pos_ + 1L);
    return Result;
  } else {
    car = parseSExp();
    cdr = parseList(LINK);
    LINK->s = mkSExp(LISTSXP);
    LINK->s->UU.U3.carval = car;
    LINK->s->UU.U3.cdrval = cdr;
    return LINK->s;
  }
  return Result;
}  /* parseList */

Local SEXPREC *parseSExp()
{
  struct LOC_parseSExp V;

  if (isNumber((long)pos_))
    return (parseInt(&V));
  else if (userinput[pos_ - 1] == '(') {
    pos_ = skipblanks(pos_ + 1L);
    return (parseList(&V));
  } else
    return (parseSym(&V));
}  /* parseSExp */


/* parseVal - return S-expression starting at userinput[pos]     */
Static SEXPREC *parseVal()
{

  /* parseSExp - return quoted S-expr starting at userinput[pos]   */
  if (userinput[pos_ - 1] == '\'')
    pos_++;
  return (parseSExp());
}  /* parseVal */


Static EXPLISTREC *parseEL PV();

Static NAMELISTREC *parseNL PV();


/* parseExp - return EXP starting at userinput[pos]              */
Static EXPREC *parseExp()
{
  EXPREC *op, *body;
  NAMELISTREC *nl;
  EXPLISTREC *el;

  if (userinput[pos_ - 1] == '(') {
    pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
    if (matches((long)pos_, 6, "lambda              ")) {  /* LAMEXP */
      pos_ = skipblanks(pos_ + 6L);   /* skip 'lambda ..' */
      pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
      nl = parseNL();
      body = parseExp();
      pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
      return (mkLAMEXP(nl, body));
    } else {  /* APEXP */
      op = parseExp();
      el = parseEL();
      return (mkAPEXP(op, el));
    }
  } else if (isValue((long)pos_))
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
  short nm;
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


/*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************/

/* emptyEnv - return an environment with no bindings             */
Static ENVREC *emptyEnv()
{
  return (mkEnv(NULL, NULL, NULL));
}  /* emptyEnv */


/* bindVar - bind variable nm to value s in environment rho      */
Static Void bindVar(nm, s, rho)
short nm;
SEXPREC *s;
ENVREC *rho;
{
  rho->vars = mkNamelist(nm, rho->vars);
  rho->values = mkValuelist(s, rho->values);
}  /* bindVar */


/*  extendEnv - extend environment rho by binding vars to vals   */
Static ENVREC *extendEnv(rho, vars, vals)
ENVREC *rho;
NAMELISTREC *vars;
VALUELISTREC *vals;
{
  return (mkEnv(vars, vals, rho));
}  /* extendEnv */


/* Local variables for findVar: */
struct LOC_findVar {
  short nm;
} ;

/* findVarInFrame - look up nm in one frame                      */
Local VALUELISTREC *findVarInFrame(nl, vl, LINK)
NAMELISTREC *nl;
VALUELISTREC *vl;
struct LOC_findVar *LINK;
{
  boolean found;

  found = false;
  while (nl != NULL && !found) {
    if (nl->head == LINK->nm)
      found = true;
    else {
      nl = nl->tail;
      vl = vl->tail;
    }  /* while */
  }
  return vl;
}  /* findVarInFrame */


/* findVar - look up nm in rho                                   */
Static VALUELISTREC *findVar(nm_, rho)
short nm_;
ENVREC *rho;
{
  struct LOC_findVar V;
  VALUELISTREC *vl;

  V.nm = nm_;
  do {
    vl = findVarInFrame(rho->vars, rho->values, &V);
    rho = rho->enclosing;
  } while (vl == NULL && rho != NULL);
  return vl;
}  /* findVar */


/* assign - assign value s to variable nm in rho                 */
Static Void assign(nm, s, rho)
short nm;
SEXPREC *s;
ENVREC *rho;
{
  VALUELISTREC *varloc;

  varloc = findVar(nm, rho);
  varloc->head = s;
}  /* assign */


/* fetch - return SEXP bound to nm in rho                        */
Static SEXPREC *fetch(nm, rho)
short nm;
ENVREC *rho;
{
  VALUELISTREC *vl;

  vl = findVar(nm, rho);
  return (vl->head);
}  /* fetch */


/* isBound - check if nm is bound in rho                         */
Static boolean isBound(nm, rho)
short nm;
ENVREC *rho;
{
  return (findVar(nm, rho) != NULL);
}  /* isBound */


/*****************************************************************
 *                     S-EXPRESSIONS                             *
 *****************************************************************/

/* prValue - print S-expression s                                */
Static Void prValue(s)
SEXPREC *s;
{
  SEXPREC *s1;

  switch (s->sxptype) {

  case NILSXP:
    printf("()");
    break;

  case NUMSXP:
    printf("%ld", s->UU.intval);
    break;

  case SYMSXP:
    prName(s->UU.symval);
    break;

  case PRIMSXP:
    printf("<primitive: ");
    prName((int)s->UU.primval + 1);
    putchar('>');
    break;

  case CLOSXP:
    printf("<closure>");
    break;

  case LISTSXP:
    putchar('(');
    prValue(s->UU.U3.carval);
    s1 = s->UU.U3.cdrval;
    while (s1->sxptype == LISTSXP) {
      putchar(' ');
      prValue(s1->UU.U3.carval);
      s1 = s1->UU.U3.cdrval;
    }
    putchar(')');
    break;
  }/* case and with */
}  /* prValue */


/* isTrueVal - return true if s is true (non-NIL) value          */
Static boolean isTrueVal(s)
SEXPREC *s;
{
  return (s->sxptype != NILSXP);
}  /* isTrueVal */


/* Local variables for applyValueOp: */
struct LOC_applyValueOp {
  BUILTINOP op;
  SEXPREC *result;
} ;

/* applyArithOp - apply binary, arithmetic VALUEOP to arguments  */
Local Void applyArithOp(n1, n2, LINK)
long n1, n2;
struct LOC_applyValueOp *LINK;
{
  SEXPREC *WITH;

  LINK->result = mkSExp(NUMSXP);
  WITH = LINK->result;
  switch (LINK->op) {

  case PLUSOP:
    WITH->UU.intval = n1 + n2;
    break;

  case MINUSOP:
    WITH->UU.intval = n1 - n2;
    break;

  case TIMESOP:
    WITH->UU.intval = n1 * n2;
    break;

  case DIVOP:
    WITH->UU.intval = n1 / n2;
    break;
  }
}  /* applyArithOp */

/* applyRelOp - apply binary, relational VALUEOP to arguments    */
Local Void applyRelOp(n1, n2, LINK)
long n1, n2;
struct LOC_applyValueOp *LINK;
{
  switch (LINK->op) {

  case LTOP:
    if (n1 < n2)
      LINK->result = trueValue;
    break;

  case GTOP:
    if (n1 > n2)
      LINK->result = trueValue;
    break;
  }
}  /* applyRelOp */

/* arity - return number of arguments expected by op             */
Local long arity(op, LINK)
BUILTINOP op;
struct LOC_applyValueOp *LINK;
{
  if (((1L << ((long)op)) &
       ((1 << ((long)CONSOP + 1)) - (1 << ((long)PLUSOP)))) != 0)
    return 2;
  else
    return 1;
}  /* arity */


/* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  */
Static SEXPREC *applyValueOp(op_, vl)
BUILTINOP op_;
VALUELISTREC *vl;
{
  struct LOC_applyValueOp V;
  SEXPREC *s1, *s2, *WITH1;

  V.op = op_;
  if (arity(V.op, &V) != lengthVL(vl)) {
    printf("Wrong number of arguments to ");
    prName((int)V.op + 1);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  V.result = nilValue;
  s1 = vl->head;   /* 1st actual */
  if (arity(V.op, &V) == 2)   /* 2nd actual */
    s2 = vl->tail->head;
  if (((1L << ((long)V.op)) &
       (((1L << ((long)DIVOP + 1)) - (1 << ((long)PLUSOP))) |
	((1 << ((long)GTOP + 1)) - (1 << ((long)LTOP))))) != 0) {
    if (s1->sxptype == NUMSXP && s2->sxptype == NUMSXP) {
      if (((1L << ((long)V.op)) &
	   ((1 << ((long)DIVOP + 1)) - (1 << ((long)PLUSOP)))) != 0)
	applyArithOp(s1->UU.intval, s2->UU.intval, &V);
      else
	applyRelOp(s1->UU.intval, s2->UU.intval, &V);
      return V.result;
    }
    printf("Non-arithmetic arguments to ");
    prName((int)V.op + 1);
    putchar('\n');
    longjmp(_JL99, 1);
    return V.result;
  }
  switch (V.op) {

  case EQOP:
    if (s1->sxptype == NILSXP && s2->sxptype == NILSXP)
      V.result = trueValue;
    else if (s1->sxptype == NUMSXP && s2->sxptype == NUMSXP &&
	     s1->UU.intval == s2->UU.intval)
      V.result = trueValue;
    else if (s1->sxptype == SYMSXP && s2->sxptype == SYMSXP &&
	     s1->UU.symval == s2->UU.symval)
      V.result = trueValue;
    break;

  case CONSOP:
    V.result = mkSExp(LISTSXP);
    WITH1 = V.result;
    WITH1->UU.U3.carval = s1;
    WITH1->UU.U3.cdrval = s2;
    break;

  case CAROP:
    if (s1->sxptype != LISTSXP) {
      printf("Error: car applied to non-list: ");
      prValue(s1);
      putchar('\n');
    } else
      V.result = s1->UU.U3.carval;
    break;

  case CDROP:
    if (s1->sxptype != LISTSXP) {
      printf("Error: cdr applied to non-list: ");
      prValue(s1);
      putchar('\n');
    } else
      V.result = s1->UU.U3.cdrval;
    break;

  case NUMBERPOP:
    if (s1->sxptype == NUMSXP)
      V.result = trueValue;
    break;

  case SYMBOLPOP:
    if (s1->sxptype == SYMSXP)
      V.result = trueValue;
    break;

  case LISTPOP:
    if (s1->sxptype == LISTSXP)
      V.result = trueValue;
    break;

  case NULLPOP:
    if (s1->sxptype == NILSXP)
      V.result = trueValue;
    break;

  case PRIMOPPOP:
    if (s1->sxptype == PRIMSXP)
      V.result = trueValue;
    break;

  case CLOSUREPOP:
    if (s1->sxptype == CLOSXP)
      V.result = trueValue;
    break;

  case PRINTOP:
    prValue(s1);
    putchar('\n');
    V.result = s1;
    break;
  }/* case and with */
  return V.result;
}  /* applyValueOp */


Static SEXPREC *eval PP((EXPREC *e, ENVREC *rho));

/* Local variables for eval: */
struct LOC_eval {
  ENVREC *rho;
} ;

/* evalList - evaluate each expression in el                     */
Local VALUELISTREC *evalList(el, LINK)
EXPLISTREC *el;
struct LOC_eval *LINK;
{
  SEXPREC *h;
  VALUELISTREC *t;

  if (el == NULL)
    return NULL;
  else {
    h = eval(el->head, LINK->rho);
    t = evalList(el->tail, LINK);
    return (mkValuelist(h, t));
  }
}  /* evalList */

/* applyClosure - apply SEXP op of type CLOSXP to actuals        */
Local SEXPREC *applyClosure(op, actuals, LINK)
SEXPREC *op;
VALUELISTREC *actuals;
struct LOC_eval *LINK;
{
  EXPREC *fun, *body;
  NAMELISTREC *forms;
  ENVREC *savedrho, *newrho;

  fun = op->UU.U4.clofun;
  savedrho = op->UU.U4.cloenv;
  forms = fun->UU.U3.formals;
  body = fun->UU.U3.lambdabody;
  if (lengthNL(forms) != lengthVL(actuals)) {
    printf("Wrong number of arguments to closure\n");
    longjmp(_JL99, 1);
  }
  newrho = extendEnv(savedrho, forms, actuals);
  return (eval(body, newrho));
}  /* applyClosure */

/* applyCtrlOp - apply CONTROLOP op to args in rho               */
Local SEXPREC *applyCtrlOp(op, args, LINK)
BUILTINOP op;
EXPLISTREC *args;
struct LOC_eval *LINK;
{
  SEXPREC *Result, *s;
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
    s = eval(WITH->head, LINK->rho);
    while (isTrueVal(s)) {
      s = eval(WITH->tail->head, LINK->rho);
      s = eval(WITH->head, LINK->rho);
    }
    Result = s;
    break;

  case SETOP:
    s = eval(WITH->tail->head, LINK->rho);
    if (isBound(WITH->head->UU.varble, LINK->rho))
      assign(WITH->head->UU.varble, s, LINK->rho);
    else
      bindVar(WITH->head->UU.varble, s, globalEnv);
    Result = s;
    break;

  case BEGINOP:
    while (args->tail != NULL) {
      s = eval(args->head, LINK->rho);
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
Static SEXPREC *eval(e, rho_)
EXPREC *e;
ENVREC *rho_;
{
  struct LOC_eval V;
  SEXPREC *Result, *op;
  BUILTINOP primname;

  V.rho = rho_;
  switch (e->etype) {

  case VALEXP:
    Result = e->UU.sxp;
    break;

  case VAREXP:
    if (isBound(e->UU.varble, V.rho))
      Result = fetch(e->UU.varble, V.rho);
    else {
      printf("Undefined variable: ");
      prName(e->UU.varble);
      putchar('\n');
      longjmp(_JL99, 1);
    }
    break;

  case APEXP:
    op = eval(e->UU.U2.optr, V.rho);
    if (op->sxptype == PRIMSXP) {
      primname = op->UU.primval;
      if (((1L << ((long)primname)) &
	   ((1 << ((long)BEGINOP + 1)) - (1 << ((long)IFOP)))) != 0)
	Result = applyCtrlOp(primname, e->UU.U2.args, &V);
      else
	Result = applyValueOp(primname, evalList(e->UU.U2.args, &V));
    } else
      Result = applyClosure(op, evalList(e->UU.U2.args, &V), &V);
    break;

  case LAMEXP:
    Result = mkCLOSXP(e, V.rho);
    break;
  }/* case and with */
  return Result;
}  /* eval */


/*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************/

/* initGlobalEnv - assign primitive function values to names     */
Static Void initGlobalEnv()
{
  BUILTINOP op;

  globalEnv = emptyEnv();
  for (op = IFOP; (long)op <= (long)PRINTOP; op = (BUILTINOP)((long)op + 1))
    bindVar((int)op + 1, mkPRIMSXP(op), globalEnv);
}  /* initGlobalEnv */


main(argc, argv)
int argc;
Char *argv[];
{  /* scheme main */
  PASCAL_MAIN(argc, argv);
  if (setjmp(_JL99))
    goto _L99;
  initNames();

  nilValue = mkSExp(NILSXP);
  trueValue = mkSExp(SYMSXP);
  trueValue->UU.symval = numNames;

  initGlobalEnv();

  quittingtime = false;
_L99:
  while (!quittingtime) {
    reader();
    if (matches((long)pos_, 4, "quit                ")) {
      quittingtime = true;
      break;
    }
    currentExp = parseExp();
    prValue(eval(currentExp, globalEnv));
    printf("\n\n");
  }  /* while */
  exit(0);
}  /* scheme */



/* End. */
