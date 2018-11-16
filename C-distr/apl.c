/* Output from p2c, the Pascal-to-C translator */
/* From input file "apl.p" */


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
  IFOP, WHILEOP, SETOP, BEGINOP, PLUSOP, MINUSOP, TIMESOP, DIVOP, MAXOP, OROP,
  ANDOP, EQOP, LTOP, GTOP, REDPLUSOP, REDMINUSOP, REDTIMESOP, REDDIVOP,
  REDMAXOP, REDOROP, REDANDOP, COMPRESSOP, SHAPEOP, RAVELOP, RESTRUCTOP,
  CATOP, INDXOP, TRANSOP, SUBOP, PRINTOP
} BUILTINOP;


typedef enum {
  SCALAR, VECTOR, MATRIX
} RANK;

typedef struct APLVALUEREC {
  struct INTLISTREC *intvals;
  RANK rnk;
  union {
    long leng;
    struct {
      long rows, cols;
    } U2;
  } UU;
} APLVALUEREC;

typedef struct INTLISTREC {
  long int_;
  struct INTLISTREC *nextint;
} INTLISTREC;

typedef enum {
  VALEXP, VAREXP, APEXP
} EXPTYPE;

typedef struct EXPREC {
  EXPTYPE etype;
  union {
    APLVALUEREC *aplval;
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
  APLVALUEREC *head;
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

/* mkVALEXP - return an EXP of type VALEXP with aplval a         */
Static EXPREC *mkVALEXP(a)
APLVALUEREC *a;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VALEXP;
  e->UU.aplval = a;
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


/* mkValuelist - return an VALUELIST with head a and tail vl     */
Static VALUELISTREC *mkValuelist(a, vl)
APLVALUEREC *a;
VALUELISTREC *vl;
{
  VALUELISTREC *newvl;

  newvl = (VALUELISTREC *)Malloc(sizeof(VALUELISTREC));
  newvl->head = a;
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


/* lengthIL - return length of INTLIST il                        */
Static long lengthIL(il)
INTLISTREC *il;
{
  long i;

  i = 0;
  while (il != NULL) {
    i++;
    il = il->nextint;
  }
  return i;
}  /* lengthIL */


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
  memcpy(printNames[i - 1], "max                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "or                  ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "and                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "=                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "<                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], ">                   ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "+/                  ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "-/                  ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "*/                  ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "//                  ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "max/                ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "or/                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "and/                ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "compress            ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "shape               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "ravel               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "restruct            ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "cat                 ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "indx                ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "trans               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "[]                  ", sizeof(NAMESTRING));
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


/* isValue - check if a number or vector const begins at pos     */
Static boolean isValue(pos)
long pos;
{
  return ((userinput[pos - 1] == '\'') | isNumber(pos));
}  /* isValue */


/* parseInt - return number starting at userinput[pos]            */
Local long parseInt()
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
}  /* parseInt */

/* parseVec - return INTLIST starting at userinput[pos]           */
Local INTLISTREC *parseVec()
{
  INTLISTREC *il;

  if (userinput[pos_ - 1] == ')') {
    pos_ = skipblanks(pos_ + 1L);   /* skip ') ...' */
    il = NULL;
    return il;
  }
  il = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
  il->int_ = parseInt();
  il->nextint = parseVec();
  return il;
}  /* parseVec */


/* parseVal - return APL value starting at userinput[pos]         */
Static APLVALUEREC *parseVal()
{
  APLVALUEREC *result;

  result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  if (userinput[pos_ - 1] == '\'') {
    result->rnk = VECTOR;
    pos_ = skipblanks(pos_ + 2L);   /* skip "'(..." */
    result->intvals = parseVec();
    result->UU.leng = lengthIL(result->intvals);
    return result;
  }
  result->rnk = SCALAR;
  result->intvals = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
  result->intvals->int_ = parseInt();
  result->intvals->nextint = NULL;
  return result;
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


/* bindVar - bind variable nm to value a in environment rho      */
Static Void bindVar(nm, a, rho)
char nm;
APLVALUEREC *a;
ENVREC *rho;
{
  rho->vars = mkNamelist(nm, rho->vars);
  rho->values = mkValuelist(a, rho->values);
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


/* assign - assign value a to variable nm in rho                 */
Static Void assign(nm, a, rho)
char nm;
APLVALUEREC *a;
ENVREC *rho;
{
  VALUELISTREC *varloc;

  varloc = findVar(nm, rho);
  varloc->head = a;
}  /* assign */


/* fetch - return number bound to nm in rho                      */
Static APLVALUEREC *fetch(nm, rho)
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


Local Void prIntlist(il, dim1, dim2)
INTLISTREC *il;
long dim1, dim2;
{
  long i, j;

  for (i = 1; i <= dim1; i++) {
    for (j = 1; j <= dim2; j++) {
      printf("%6ld ", il->int_);
      il = il->nextint;
    }
    putchar('\n');
  }
}  /* prIntlist */


/*****************************************************************
 *                     APL VALUES                                *
 *****************************************************************/

/* prValue - print APL value a                                   */
Static Void prValue(a)
APLVALUEREC *a;
{

  /* prIntlist - print INTLIST il as dim1 x dim2 matrix            */
  switch (a->rnk) {

  case SCALAR:
    prIntlist(a->intvals, 1L, 1L);
    break;

  case VECTOR:
    prIntlist(a->intvals, 1L, a->UU.leng);
    break;

  case MATRIX:
    prIntlist(a->intvals, a->UU.U2.rows, a->UU.U2.cols);
    break;
  }
}  /* prValue */


/* isTrueVal - return true if first value in a is one            */
Static boolean isTrueVal(a)
APLVALUEREC *a;
{
  if (a->intvals == NULL)
    return false;
  else
    return (a->intvals->int_ == 1);
}  /* isTrueVal */


/* Local variables for applyValueOp: */
struct LOC_applyValueOp {
  APLVALUEREC *result;
} ;

/* size - return number of elements in a                         */
Local long size(a, LINK)
APLVALUEREC *a;
struct LOC_applyValueOp *LINK;
{
  long Result;

  switch (a->rnk) {

  case SCALAR:
    Result = 1;
    break;

  case VECTOR:
    Result = a->UU.leng;
    break;

  case MATRIX:
    Result = a->UU.U2.rows * a->UU.U2.cols;
    break;
  }
  return Result;
}  /* size */

/* skipover - return pointer to nth record in il                 */
Local INTLISTREC *skipover(n, il, LINK)
long n;
INTLISTREC *il;
struct LOC_applyValueOp *LINK;
{
  while (n > 0) {
    il = il->nextint;
    n--;
  }
  return il;
}  /* skipover */

Local Void copyrank(a, r)
APLVALUEREC *a, *r;
{
  r->rnk = a->rnk;
  switch (r->rnk) {   /* with */

  case SCALAR:
    /* blank case */
    break;

  case VECTOR:
    r->UU.leng = a->UU.leng;
    break;

  case MATRIX:
    r->UU.U2.rows = a->UU.U2.rows;
    r->UU.U2.cols = a->UU.U2.cols;
    break;
  }/* case */
}  /* copyrank */

/* applyOp - apply VALUEOP op to integer arguments               */
Local long applyOp(op, i, j)
BUILTINOP op;
long i, j;
{
  long Result;

  switch (op) {

  case PLUSOP:
    Result = i + j;
    break;

  case MINUSOP:
    Result = i - j;
    break;

  case TIMESOP:
    Result = i * j;
    break;

  case DIVOP:
    Result = i / j;
    break;

  case MAXOP:
    if (i > j)
      Result = i;
    else
      Result = j;
    break;

  case OROP:
    if (i == 1 || j == 1)
      Result = 1;
    else
      Result = 0;
    break;

  case ANDOP:
    if (i == 1 && j == 1)
      Result = 1;
    else
      Result = 0;
    break;

  case EQOP:
    if (i == j)
      Result = 1;
    else
      Result = 0;
    break;

  case LTOP:
    if (i < j)
      Result = 1;
    else
      Result = 0;
    break;

  case GTOP:
    if (i > j)
      Result = 1;
    else
      Result = 0;
    break;
  }/* case */
  return Result;
}  /* applyOp */

/* applyIntlis - apply op to two lists, extending appropriately  */
Local INTLISTREC *applyIntlis(op, il1, il2, il1leng, il2leng)
BUILTINOP op;
INTLISTREC *il1, *il2;
long il1leng, il2leng;
{
  INTLISTREC *il;

  if (il1 == NULL || il2 == NULL)
    return NULL;
  else {
    il = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    il->int_ = applyOp(op, il1->int_, il2->int_);
    if (il1leng == 1) {
      il->nextint = applyIntlis(op, il1, il2->nextint, il1leng, il2leng);
      return il;
    }
    if (il2leng == 1)
      il->nextint = applyIntlis(op, il1->nextint, il2, il1leng, il2leng);
    else
      il->nextint = applyIntlis(op, il1->nextint, il2->nextint, il1leng,
				il2leng);
    return il;   /* with */
  }
}  /* applyIntlis */

/* applyArithOp - apply binary operator to a1 and a2              */
Local Void applyArithOp(op, a1, a2, LINK)
BUILTINOP op;
APLVALUEREC *a1, *a2;
struct LOC_applyValueOp *LINK;
{

  /* copyrank - copy rank and shape of a to r                      */
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  if (a1->rnk == SCALAR)
    copyrank(a2, LINK->result);
  else if (a2->rnk == SCALAR)
    copyrank(a1, LINK->result);
  else if (size(a1, LINK) == 1)
    copyrank(a2, LINK->result);
  else
    copyrank(a1, LINK->result);
  LINK->result->intvals = applyIntlis(op, a1->intvals, a2->intvals,
				      size(a1, LINK), size(a2, LINK));
}  /* applyArithOp */

/* Local variables for applyRedOp: */
struct LOC_applyRedOp {
  struct LOC_applyValueOp *LINK;
  BUILTINOP op;
} ;

Local long applyOp_(op, i, j, LINK)
BUILTINOP op;
long i, j;
struct LOC_applyRedOp *LINK;
{
  long Result;

  switch (op) {

  case REDPLUSOP:
    Result = i + j;
    break;

  case REDMINUSOP:
    Result = i - j;
    break;

  case REDTIMESOP:
    Result = i * j;
    break;

  case REDDIVOP:
    Result = i / j;
    break;

  case REDMAXOP:
    if (i > j)
      Result = i;
    else
      Result = j;
    break;

  case REDOROP:
    if (i == 1 || j == 1)
      Result = 1;
    else
      Result = 0;
    break;

  case REDANDOP:
    if (i == 1 && j == 1)
      Result = 1;
    else
      Result = 0;
    break;
  }/* case */
  return Result;
}  /* applyOp */

/* redVec - reduce op (argument to applyRedOp) over list         */
Local long redVec(il, leng, LINK)
INTLISTREC *il;
long leng;
struct LOC_applyRedOp *LINK;
{
  if (leng == 0)
    return 0;
  else if (leng == 1)
    return (il->int_);
  else
    return (applyOp_(LINK->op, il->int_, redVec(il->nextint, leng - 1, LINK),
		     LINK));
}  /* redVec */

/* redMat - reduce op (argument to applyRedOp) over matrix       */
Local INTLISTREC *redMat(il, cols, rows, LINK)
INTLISTREC *il;
long cols, rows;
struct LOC_applyRedOp *LINK;
{
  INTLISTREC *ilnew;

  if (rows == 0)
    return NULL;
  else {
    ilnew = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    ilnew->int_ = redVec(il, cols, LINK);
    ilnew->nextint = redMat(skipover(cols, il, LINK->LINK), cols, rows - 1,
			    LINK);
    return ilnew;
  }
}  /* redmat */

/* applyRedOp - apply reduction operator                         */
Local Void applyRedOp(op_, a, LINK)
BUILTINOP op_;
APLVALUEREC *a;
struct LOC_applyValueOp *LINK;
{

  /* applyOp - apply base operator of reduction operator           */
  struct LOC_applyRedOp V;
  APLVALUEREC *WITH;

  V.LINK = LINK;
  V.op = op_;
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  switch (a->rnk) {

  case SCALAR:
    LINK->result = a;
    break;

  case VECTOR:
    WITH = LINK->result;
    WITH->rnk = SCALAR;
    WITH->intvals = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    WITH->intvals->int_ = redVec(a->intvals, a->UU.leng, &V);
    WITH->intvals->nextint = NULL;
    break;

  case MATRIX:
    WITH = LINK->result;
    WITH->rnk = VECTOR;
    WITH->UU.leng = a->UU.U2.rows;
    WITH->intvals = redMat(a->intvals, a->UU.U2.cols, WITH->UU.leng, &V);
    break;
  }/* case */
}  /* applyRedOp */

/* append - append il2 to il1; il1 is altered                    */
Local INTLISTREC *append(il1, il2, LINK)
INTLISTREC *il1, *il2;
struct LOC_applyValueOp *LINK;
{
  INTLISTREC *Result;

  if (il1 == NULL)
    return il2;
  Result = il1;
  while (il1->nextint != NULL)
    il1 = il1->nextint;
  il1->nextint = il2;
  return Result;
}  /* append */

/* ncopy - copy elements of src until list has reps elements     */
Local INTLISTREC *ncopy(src, reps, LINK)
INTLISTREC *src;
long reps;
struct LOC_applyValueOp *LINK;
{
  INTLISTREC *Result, *il, *suffix;
  long i;

  if (reps == 0)
    return NULL;
  il = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
  Result = il;
  il->int_ = src->int_;
  suffix = src->nextint;
  for (i = 2; i <= reps; i++) {
    if (suffix == NULL)   /* exhausted src */
      suffix = src;
    /* start over */
    il->nextint = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    il = il->nextint;
    il->int_ = suffix->int_;
    suffix = suffix->nextint;
  }
  return Result;
}  /* ncopy */

/* Local variables for compress: */
struct LOC_compress {
  struct LOC_applyValueOp *LINK;
} ;

/* ilcompress - il1 over il2, taking il2 in chunks of size width */
Local INTLISTREC *ilcompress(il1, il2, width, LINK)
INTLISTREC *il1, *il2;
long width;
struct LOC_compress *LINK;
{
  INTLISTREC *il;

  if (il1 == NULL)
    return NULL;
  else if (il1->int_ == 1) {
    il = ncopy(il2, width, LINK->LINK);
    il = append(il, ilcompress(il1->nextint, skipover(width, il2, LINK->LINK),
			       width, LINK), LINK->LINK);
    return il;
  } else
    return (ilcompress(il1->nextint, skipover(width, il2, LINK->LINK), width,
		       LINK));
}  /* ilcompress */

/* countones - count ones in il                                  */
Local long countones(il, LINK)
INTLISTREC *il;
struct LOC_compress *LINK;
{
  long i;

  i = 0;
  while (il != NULL) {
    if (il->int_ == 1)
      i++;
    il = il->nextint;
  }
  return i;
}  /* countones */

/* compress - compress a1 over a2                                */
Local Void compress(a1, a2, LINK)
APLVALUEREC *a1, *a2;
struct LOC_applyValueOp *LINK;
{
  struct LOC_compress V;
  long width;
  APLVALUEREC *WITH;

  V.LINK = LINK;
  if (a2->rnk == VECTOR)
    width = 1;
  else
    width = a2->UU.U2.cols;
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  WITH = LINK->result;
  WITH->rnk = a2->rnk;
  WITH->intvals = ilcompress(a1->intvals, a2->intvals, width, &V);
  if (WITH->rnk == VECTOR)   /* with */
    WITH->UU.leng = countones(a1->intvals, &V);
  else {
    WITH->UU.U2.cols = a2->UU.U2.cols;
    WITH->UU.U2.rows = countones(a1->intvals, &V);
  }
}  /* compress */

/* shape - return vector giving dimensions of a                  */
Local Void shape(a, LINK)
APLVALUEREC *a;
struct LOC_applyValueOp *LINK;
{
  INTLISTREC *il;

  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  LINK->result->rnk = VECTOR;
  switch (a->rnk) {

  case SCALAR:
    LINK->result->UU.leng = 0;
    LINK->result->intvals = NULL;
    break;

  case VECTOR:
    LINK->result->UU.leng = 1;
    il = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    LINK->result->intvals = il;
    il->int_ = a->UU.leng;
    il->nextint = NULL;
    break;

  case MATRIX:
    LINK->result->UU.leng = 2;
    il = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    LINK->result->intvals = il;
    il->int_ = a->UU.U2.rows;
    il->nextint = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    il = il->nextint;
    il->int_ = a->UU.U2.cols;
    il->nextint = NULL;
    break;
  }/* case */
}  /* shape */

/* ravel - transform a to a vector without changing elements     */
Local Void ravel(a, LINK)
APLVALUEREC *a;
struct LOC_applyValueOp *LINK;
{
  long size;
  APLVALUEREC *WITH;

  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  switch (a->rnk) {

  case SCALAR:
    size = 1;
    break;

  case VECTOR:
    size = a->UU.leng;
    break;

  case MATRIX:
    size = a->UU.U2.rows * a->UU.U2.cols;
    break;
  }
  WITH = LINK->result;
  WITH->rnk = VECTOR;
  WITH->UU.leng = size;
  WITH->intvals = a->intvals;
}  /* ravel */

/* restruct - restructure valuevec according to shapevec         */
Local Void restruct(shapevec, valuevec, LINK)
APLVALUEREC *shapevec, *valuevec;
struct LOC_applyValueOp *LINK;
{
  RANK newrank;
  long dim1, dim2;
  APLVALUEREC *WITH;

  if (valuevec->intvals == NULL) {
    printf("Cannot restructure null vector\n");
    longjmp(_JL99, 1);
  }
  if (shapevec->rnk == SCALAR) {
    newrank = VECTOR;
    dim1 = shapevec->intvals->int_;
    dim2 = 1;
  } else if (shapevec->UU.leng == 0) {
    newrank = SCALAR;
    dim1 = 1;
    dim2 = 1;
  } else if (shapevec->UU.leng == 1) {
    newrank = VECTOR;
    dim1 = shapevec->intvals->int_;
    dim2 = 1;
  } else {
    newrank = MATRIX;
    dim1 = shapevec->intvals->int_;
    dim2 = shapevec->intvals->nextint->int_;
  }
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  WITH = LINK->result;
  WITH->rnk = newrank;
  if (WITH->rnk == VECTOR)
    WITH->UU.leng = dim1;
  else if (WITH->rnk == MATRIX) {
    WITH->UU.U2.rows = dim1;
    WITH->UU.U2.cols = dim2;
  }
  WITH->intvals = ncopy(valuevec->intvals, dim1 * dim2, LINK);   /* with */

  /* with */
}  /* restruct */

/* copyIntlis - make a fresh copy of il                          */
Local INTLISTREC *copyIntlis(il, LINK)
INTLISTREC *il;
struct LOC_applyValueOp *LINK;
{
  return (ncopy(il, lengthIL(il), LINK));
}  /* copyIntlis */

/* cat - create a vector by joining ravels of a1 and a2          */
Local Void cat(a1, a2, LINK)
APLVALUEREC *a1, *a2;
struct LOC_applyValueOp *LINK;
{
  APLVALUEREC *WITH;

  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  WITH = LINK->result;
  WITH->rnk = VECTOR;
  WITH->UU.leng = size(a1, LINK) + size(a2, LINK);
  WITH->intvals = copyIntlis(a1->intvals, LINK);
  WITH->intvals = append(WITH->intvals, a2->intvals, LINK);
}  /* cat */

/* indx - perform index generation, using first value in a       */
Local Void indx(a, LINK)
APLVALUEREC *a;
struct LOC_applyValueOp *LINK;
{
  long i;
  INTLISTREC *il;
  APLVALUEREC *WITH;

  i = a->intvals->int_;
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  WITH = LINK->result;
  WITH->rnk = VECTOR;
  WITH->intvals = NULL;
  WITH->UU.leng = i;
  while (i > 0) {   /* with */
    il = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
    il->int_ = i;
    il->nextint = WITH->intvals;
    WITH->intvals = il;
    i--;
  }  /* while */
}  /* indx */

/* Local variables for trans: */
struct LOC_trans {
  struct LOC_applyValueOp *LINK;
} ;

/* skiplist - subscript il by cols and rows                      */
Local INTLISTREC *skiplist(il, cols, rows, LINK)
INTLISTREC *il;
long cols, rows;
struct LOC_trans *LINK;
{
  INTLISTREC *ilnew;

  ilnew = (INTLISTREC *)Malloc(sizeof(INTLISTREC));
  if (rows == 1) {
    ilnew->int_ = il->int_;
    ilnew->nextint = NULL;
  } else {
    ilnew->int_ = il->int_;
    ilnew->nextint = skiplist(skipover(cols, il, LINK->LINK), cols, rows - 1,
			      LINK);
  }
  return ilnew;
}  /* skiplist */

/* trans - perform "trans"                                       */
Local Void trans(a, LINK)
APLVALUEREC *a;
struct LOC_applyValueOp *LINK;
{
  struct LOC_trans V;
  INTLISTREC *il, *ilnew;
  long i;
  APLVALUEREC *WITH;
  long FORLIM;

  V.LINK = LINK;
  if (a->rnk != MATRIX || a->intvals == NULL) {
    LINK->result = a;
    return;
  }
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  WITH = LINK->result;
  WITH->rnk = MATRIX;
  WITH->UU.U2.cols = a->UU.U2.rows;
  WITH->UU.U2.rows = a->UU.U2.cols;
  il = a->intvals;
  ilnew = NULL;
  FORLIM = WITH->UU.U2.rows;
  for (i = 1; i <= FORLIM; i++) {
    ilnew = append(ilnew,
		   skiplist(il, WITH->UU.U2.rows, WITH->UU.U2.cols, &V),
		   LINK);
    il = il->nextint;
  }
  WITH->intvals = ilnew;   /* with */
}  /* trans */

/* Local variables for subscript: */
struct LOC_subscript {
  struct LOC_applyValueOp *LINK;
} ;

/* sub - find nth chunk in il, each chunk having width elements  */
Local INTLISTREC *sub(il, n, width, LINK)
INTLISTREC *il;
long n, width;
struct LOC_subscript *LINK;
{
  long i, j;

  for (i = 1; i < n; i++) {
    for (j = 1; j <= width; j++)
      il = il->nextint;
  }
  return il;
}  /* sub */

/* ilsub - subscript src by subs in chunks of size width         */
Local INTLISTREC *ilsub(src, subs, width, LINK)
INTLISTREC *src, *subs;
long width;
struct LOC_subscript *LINK;
{
  INTLISTREC *il;

  if (subs == NULL) {
    il = NULL;
    return il;
  }
  il = sub(src, subs->int_, width, LINK);
  il = ncopy(il, width, LINK->LINK);
  il = append(il, ilsub(src, subs->nextint, width, LINK), LINK->LINK);
  return il;
}  /* ilsub */

/* subscript - "[]" operation; a1 a vector or matrix, a2 vector  */
Local Void subscript(a1, a2, LINK)
APLVALUEREC *a1, *a2;
struct LOC_applyValueOp *LINK;
{
  struct LOC_subscript V;
  long width;
  APLVALUEREC *WITH;

  V.LINK = LINK;
  LINK->result = (APLVALUEREC *)Malloc(sizeof(APLVALUEREC));
  WITH = LINK->result;
  WITH->rnk = a1->rnk;
  if (WITH->rnk == VECTOR) {
    if (a2->rnk == SCALAR)
      WITH->UU.leng = 1;
    else
      WITH->UU.leng = a2->UU.leng;
    width = 1;
  } else {
    if (a2->rnk == SCALAR)
      WITH->UU.U2.rows = 1;
    else
      WITH->UU.U2.rows = a2->UU.leng;
    WITH->UU.U2.cols = a1->UU.U2.cols;
    width = WITH->UU.U2.cols;
  }
  WITH->intvals = ilsub(a1->intvals, a2->intvals, width, &V);   /* with */
}  /* subscript */

/* arity - return number of arguments expected by op             */
Local long arity(op, LINK)
BUILTINOP op;
struct LOC_applyValueOp *LINK;
{
  if (((1L << ((long)op)) & (((1L << ((long)GTOP + 1)) - (1 << ((long)PLUSOP))) |
	 (1 << ((long)COMPRESSOP)) | (1 << ((long)RESTRUCTOP)) |
	 (1 << ((long)CATOP)) | (1 << ((long)SUBOP)))) != 0)
    return 2;
  else
    return 1;
}  /* arity */


/* applyValueOp - apply VALUEOP op to arguments in VALUELIST vl  */
Static APLVALUEREC *applyValueOp(op, vl)
BUILTINOP op;
VALUELISTREC *vl;
{
  struct LOC_applyValueOp V;
  APLVALUEREC *a1, *a2;

  if (arity(op, &V) != lengthVL(vl)) {
    printf("Wrong number of arguments to ");
    prName((int)op + 1);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  a1 = vl->head;   /* 1st actual */
  if (arity(op, &V) == 2)   /* 2nd actual */
    a2 = vl->tail->head;
  switch (op) {

  case PLUSOP:
  case MINUSOP:
  case TIMESOP:
  case DIVOP:
  case MAXOP:
  case OROP:
  case ANDOP:
  case EQOP:
  case LTOP:
  case GTOP:
    applyArithOp(op, a1, a2, &V);
    break;

  case REDPLUSOP:
  case REDMINUSOP:
  case REDTIMESOP:
  case REDDIVOP:
  case REDMAXOP:
  case REDOROP:
  case REDANDOP:
    applyRedOp(op, a1, &V);
    break;

  case COMPRESSOP:
    compress(a1, a2, &V);
    break;

  case SHAPEOP:
    shape(a1, &V);
    break;

  case RAVELOP:
    ravel(a1, &V);
    break;

  case RESTRUCTOP:
    restruct(a1, a2, &V);
    break;

  case CATOP:
    cat(a1, a2, &V);
    break;

  case INDXOP:
    indx(a1, &V);
    break;

  case TRANSOP:
    trans(a1, &V);
    break;

  case SUBOP:
    subscript(a1, a2, &V);
    break;

  case PRINTOP:
    prValue(a1);
    V.result = a1;
    break;
  }/* case */
  return V.result;
}  /* applyValueOp */


Static APLVALUEREC *eval PP((EXPREC *e, ENVREC *rho));

/* Local variables for eval: */
struct LOC_eval {
  ENVREC *rho;
} ;

/* evalList - evaluate each expression in el                     */
Local VALUELISTREC *evalList(el, LINK)
EXPLISTREC *el;
struct LOC_eval *LINK;
{
  APLVALUEREC *h;
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
Local APLVALUEREC *applyUserFun(nm, actuals, LINK)
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
Local APLVALUEREC *applyCtrlOp(op, args, LINK)
BUILTINOP op;
EXPLISTREC *args;
struct LOC_eval *LINK;
{
  APLVALUEREC *Result, *a;
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
    a = eval(WITH->head, LINK->rho);
    while (isTrueVal(a)) {
      a = eval(WITH->tail->head, LINK->rho);
      a = eval(WITH->head, LINK->rho);
    }
    Result = a;
    break;

  case SETOP:
    a = eval(WITH->tail->head, LINK->rho);
    if (isBound(WITH->head->UU.varble, LINK->rho))
      assign(WITH->head->UU.varble, a, LINK->rho);
    else if (isBound(WITH->head->UU.varble, globalEnv))
      assign(WITH->head->UU.varble, a, globalEnv);
    else
      bindVar(WITH->head->UU.varble, a, globalEnv);
    Result = a;
    break;

  case BEGINOP:
    while (args->tail != NULL) {
      a = eval(args->head, LINK->rho);
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
Static APLVALUEREC *eval(e, rho_)
EXPREC *e;
ENVREC *rho_;
{
  struct LOC_eval V;
  APLVALUEREC *Result;
  BUILTINOP op;

  V.rho = rho_;
  switch (e->etype) {

  case VALEXP:
    Result = e->UU.aplval;
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
{  /* apl main */
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
}  /* apl */






/* End. */
