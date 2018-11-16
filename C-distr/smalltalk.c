/* Output from p2c, the Pascal-to-C translator */
/* From input file "smalltalk.p" */


/*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************/

#include <p2c/p2c.h>


#define NAMELENG        30   /* Maximum length of a name */
#define MAXNAMES        300   /* Maximum number of different names */
#define MAXINPUT        5000   /* Maximum length of an input */

#define PROMPT          "-> "
#define PROMPT2         "> "
#define COMMENTCHAR     ";"

#define TABCODE         9   /* in ASCII */


typedef Char NAMESTRING[NAMELENG];

/* a NAME is an index in printNames */

typedef enum {
  IFOP, WHILEOP, SETOP, BEGINOP, NEWOP, PLUSOP, MINUSOP, TIMESOP, DIVOP, EQOP,
  LTOP, GTOP, PRINTOP
} BUILTINOP;


typedef enum {
  INT, SYM, USER
} STVALUETYPE;

typedef struct STVALUEREC {
  struct CLASSREC *owner;
  STVALUETYPE vtype;
  union {
    long intval;
    short symval;
    struct ENVREC *userval;
  } UU;
} STVALUEREC;

typedef enum {
  VALEXP, VAREXP, APEXP
} EXPTYPE;

typedef struct EXPREC {
  EXPTYPE etype;
  union {
    STVALUEREC *valu;
    short varble;
    struct {
      short optr;
      struct EXPLISTREC *args;
    } U2;
  } UU;
} EXPREC;

typedef struct EXPLISTREC {
  EXPREC *head;
  struct EXPLISTREC *tail;
} EXPLISTREC;

typedef struct VALUELISTREC {
  STVALUEREC *head;
  struct VALUELISTREC *tail;
} VALUELISTREC;

typedef struct NAMELISTREC {
  short head;
  struct NAMELISTREC *tail;
} NAMELISTREC;

typedef struct ENVREC {
  NAMELISTREC *vars;
  VALUELISTREC *values;
} ENVREC;

typedef struct FUNDEFREC {
  short funname;
  NAMELISTREC *formals;
  EXPREC *body;
  struct FUNDEFREC *nextfundef;
} FUNDEFREC;

typedef struct CLASSREC {
  short clname;
  struct CLASSREC *clsuper;
  NAMELISTREC *clrep;
  FUNDEFREC *exported;
  struct CLASSREC *nextclass;
} CLASSREC;


Static FUNDEFREC *fundefs;
Static CLASSREC *classes;

Static ENVREC *globalEnv;

Static EXPREC *currentExp;

Static Char userinput[MAXINPUT];
Static short inputleng, pos_;

Static NAMESTRING printNames[MAXNAMES];
Static short numNames, numBuiltins, numCtrlOps;

Static short SELF;

Static CLASSREC *OBJECTCLASS;
Static STVALUEREC *objectInst;

Static STVALUEREC *trueValue, *falseValue;

Static boolean quittingtime;


/*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************/

/* mkVALEXP - return an EXP of type VALEXP with valu v           */
Static EXPREC *mkVALEXP(v)
STVALUEREC *v;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VALEXP;
  e->UU.valu = v;
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
short op;
EXPLISTREC *el;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = APEXP;
  e->UU.U2.optr = op;
  e->UU.U2.args = el;
  return e;
}  /* mkAPEXP */


/* mkINT - return an STVALUE with integer value n                */
Static STVALUEREC *mkINT(n)
long n;
{
  STVALUEREC *newval;

  newval = (STVALUEREC *)Malloc(sizeof(STVALUEREC));
  newval->owner = OBJECTCLASS;
  newval->vtype = INT;
  newval->UU.intval = n;
  return newval;
}  /* mkINT */


/* mkSYM - return an STVALUE with symbol value s                 */
Static STVALUEREC *mkSYM(s)
short s;
{
  STVALUEREC *newval;

  newval = (STVALUEREC *)Malloc(sizeof(STVALUEREC));
  newval->owner = OBJECTCLASS;
  newval->vtype = SYM;
  newval->UU.symval = s;
  return newval;
}  /* mkSYM */


/* mkUSER - return a USER-type STVALUE                           */
Static STVALUEREC *mkUSER(rho, ownr)
ENVREC *rho;
CLASSREC *ownr;
{
  STVALUEREC *newval;

  newval = (STVALUEREC *)Malloc(sizeof(STVALUEREC));
  newval->vtype = USER;
  newval->UU.userval = rho;
  newval->owner = ownr;
  return newval;
}  /* mkUSER */


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


/* mkValuelist - return an VALUELIST with head v and tail vl     */
Static VALUELISTREC *mkValuelist(v, vl)
STVALUEREC *v;
VALUELISTREC *vl;
{
  VALUELISTREC *newvl;

  newvl = (VALUELISTREC *)Malloc(sizeof(VALUELISTREC));
  newvl->head = v;
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

/* fetchClass - get class definition of NAME cname               */
Static CLASSREC *fetchClass(cname)
short cname;
{
  CLASSREC *cl;
  boolean found;

  found = false;
  cl = classes;
  while (cl != NULL && !found) {
    if (cl->clname == cname)
      found = true;
    else
      cl = cl->nextclass;
  }
  return cl;
}  /* fetchClass */


/* newClass - add new class cname to classes                     */
Static CLASSREC *newClass(cname, super)
short cname;
CLASSREC *super;
{
  CLASSREC *cl;

  cl = fetchClass(cname);
  if (cl == NULL) {   /* cname not yet defined as class */
    cl = (CLASSREC *)Malloc(sizeof(CLASSREC));
    cl->clname = cname;
    cl->nextclass = classes;   /* place new CLASSREC */
    classes = cl;   /* on classes list */
  }
  cl->clsuper = super;
  return cl;
}  /* newClass */


/* fetchFun - get function definition of NAME fname from fenv    */
Static FUNDEFREC *fetchFun(fname, fenv)
short fname;
FUNDEFREC *fenv;
{
  boolean found;

  found = false;
  while (fenv != NULL && !found) {
    if (fenv->funname == fname)
      found = true;
    else
      fenv = fenv->nextfundef;
  }
  return fenv;
}  /* fetchFun */


/* newFunDef - add new function fname to fenv                    */
Static FUNDEFREC *newFunDef(fname, fenv)
short fname;
FUNDEFREC **fenv;
{
  FUNDEFREC *f;

  f = fetchFun(fname, *fenv);
  if (f != NULL)   /* fname not yet defined as a function */
    return f;
  f = (FUNDEFREC *)Malloc(sizeof(FUNDEFREC));
  f->funname = fname;
  f->nextfundef = *fenv;   /* place new FUNDEFREC */
  *fenv = f;   /* on fenv list */
  return f;
}  /* newFunDef */


/* initNames - place all pre-defined names into printNames       */
Static Void initNames()
{
  long i;

  fundefs = NULL;
  i = 1;
  memcpy(printNames[i - 1], "if                            ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "while                         ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "set                           ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "begin                         ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "new                           ",
	 sizeof(NAMESTRING));
  i++;
  numCtrlOps = i - 1;
  memcpy(printNames[i - 1], "+                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "-                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "*                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "/                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "=                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "<                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], ">                             ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "print                         ",
	 sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "self                          ",
	 sizeof(NAMESTRING));
  SELF = i;
  numNames = i;
  numBuiltins = i - 1;
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


/* primOp - translate NAME optr to corresponding BUILTINOP       */
Static BUILTINOP primOp(optr)
short optr;
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

Local boolean isNumber(pos)
long pos;
{

  /* isDigits - check if sequence of digits begins at pos          */
  return (isDigits(pos) | ((userinput[pos - 1] == '-') & isDigits(pos + 1)));
}  /* isNumber */


/* isValue - check if a number or quoted const begins at pos     */
Static boolean isValue(pos)
long pos;
{

  /* isNumber - check if a number begins at pos                    */
  return ((userinput[pos - 1] == '#') | isNumber(pos));
}  /* isValue */


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


/* parseVal - return primitive value starting at userinput[pos]  */
Static STVALUEREC *parseVal()
{

  /* parseInt - return number starting at userinput[pos]           */
  if (userinput[pos_ - 1] == '#') {
    pos_++;
    return (mkSYM(parseName()));
  } else
    return (mkINT(parseInt()));
}  /* parseVal */


Static EXPLISTREC *parseEL PV();


/* parseExp - return EXP starting at userinput[pos]              */
Static EXPREC *parseExp()
{
  short nm;
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


/* parseDef - parse function definition at userinput[pos]        */
Static short parseDef(fenv)
FUNDEFREC **fenv;
{
  short fname;   /* function name */
  FUNDEFREC *newfun;   /* new FUNDEFREC */

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 6L);   /* skip 'define ..' */
  fname = parseName();
  newfun = newFunDef(fname, fenv);
  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  newfun->formals = parseNL();
  newfun->body = parseExp();
  pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
  return fname;
}  /* parseDef */


/* parseClass - parse class definition at userinput[pos]         */
Static short parseClass()
{
  short cname, sname, fname;
  CLASSREC *thisclass, *superclass;
  NAMELISTREC *rep;
  FUNDEFREC *cenv;

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 5L);   /* skip 'class ...' */
  cname = parseName();
  sname = parseName();
  superclass = fetchClass(sname);
  if (superclass == NULL) {
    printf("Undefined superclass: ");
    prName(sname);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  thisclass = newClass(cname, superclass);
  pos_ = skipblanks(pos_ + 1L);   /* skip '( ...' */
  rep = parseNL();   /* component names */
  cenv = NULL;
  while (userinput[pos_ - 1] == '(') {
    fname = parseDef(&cenv);
    prName(fname);
    putchar('\n');
  }
  thisclass->exported = cenv;
  if (rep == NULL)
    thisclass->clrep = superclass->clrep;
  else {
    thisclass->clrep = rep;
    while (rep->tail != NULL)
      rep = rep->tail;
    rep->tail = superclass->clrep;
  }
  pos_ = skipblanks(pos_ + 1L);   /* skip ' ..)' */
  return cname;
}  /* parseClass */


/*****************************************************************
 *                     ENVIRONMENTS                              *
 *****************************************************************/

/* emptyEnv - return an environment with no bindings             */
Static ENVREC *emptyEnv()
{
  return (mkEnv(NULL, NULL));
}  /* emptyEnv */


/* bindVar - bind variable nm to value n in environment rho      */
Static Void bindVar(nm, v, rho)
short nm;
STVALUEREC *v;
ENVREC *rho;
{
  rho->vars = mkNamelist(nm, rho->vars);
  rho->values = mkValuelist(v, rho->values);
}  /* bindVar */


/* findVar - look up nm in rho                                   */
Static VALUELISTREC *findVar(nm, rho)
short nm;
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
Static Void assign(nm, v, rho)
short nm;
STVALUEREC *v;
ENVREC *rho;
{
  VALUELISTREC *varloc;

  varloc = findVar(nm, rho);
  varloc->head = v;
}  /* assign */


/* fetch - return number bound to nm in rho                      */
Static STVALUEREC *fetch(nm, rho)
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
 *                           VALUES                              *
 *****************************************************************/

/* prValue - print value v                                       */
Static Void prValue(v)
STVALUEREC *v;
{
  if (v->vtype == INT) {
    printf("%ld", v->UU.intval);
    return;
  }
  if (v->vtype == SYM)
    prName(v->UU.symval);
  else
    printf("<userval>");
}  /* prValue */


/* isTrueVal - return true if v is true (non-zero) value         */
Static boolean isTrueVal(v)
STVALUEREC *v;
{
  if (v->vtype == USER || v->vtype == SYM)
    return true;
  else
    return (v->UU.intval != 0);
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
Static STVALUEREC *applyValueOp(op, vl)
BUILTINOP op;
VALUELISTREC *vl;
{
  long n, n1, n2;
  STVALUEREC *s1, *s2;

  if (arity(op) != lengthVL(vl)) {
    printf("Wrong number of arguments to ");
    prName((int)op + 1);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  s1 = vl->head;   /* 1st actual */
  if (arity(op) == 2)   /* 2nd actual */
    s2 = vl->tail->head;
  if (op == PRINTOP) {
    prValue(s1);
    putchar('\n');
    return s1;
  } else if (op == EQOP) {
    if (s1->vtype == s2->vtype) {
      if (s1->vtype == INT && s1->UU.intval == s2->UU.intval ||
	  s1->vtype == SYM && s1->UU.symval == s2->UU.symval)
	return trueValue;
      else
	return falseValue;
    } else
      return falseValue;
  } else {
    if (s1->vtype != INT || s2->vtype != INT) {
      printf("Arguments to numeric op not integer: ");
      prName((int)op+1);
      putchar('\n');
      longjmp(_JL99, 1);
    }
    n1 = s1->UU.intval;
    n2 = s2->UU.intval;
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
    }/* case */
    return (mkINT(n));
  }
}  /* applyValueOp */


Static STVALUEREC *eval PP((EXPREC *e, ENVREC *rho, STVALUEREC *rcvr));

/* Local variables for eval: */
struct LOC_eval {
  ENVREC *rho;
  STVALUEREC *rcvr;
} ;

/* evalList - evaluate each expression in el                     */
Local VALUELISTREC *evalList(el, LINK)
EXPLISTREC *el;
struct LOC_eval *LINK;
{
  STVALUEREC *h;
  VALUELISTREC *t;

  if (el == NULL)
    return NULL;
  else {
    h = eval(el->head, LINK->rho, LINK->rcvr);
    t = evalList(el->tail, LINK);
    return (mkValuelist(h, t));
  }
}  /* evalList */

/* applyGlobalFun - apply function defined at top level          */
Local STVALUEREC *applyGlobalFun(optr, actuals, LINK)
short optr;
VALUELISTREC *actuals;
struct LOC_eval *LINK;
{
  FUNDEFREC *f;
  ENVREC *rho;

  f = fetchFun(optr, fundefs);
  if (f == NULL) {
    printf("Undefined function: ");
    prName(optr);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  if (lengthNL(f->formals) != lengthVL(actuals)) {
    printf("Wrong number of arguments to: ");
    prName(f->funname);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  rho = mkEnv(f->formals, actuals);
  return (eval(f->body, rho, LINK->rcvr));   /* with */
}  /* applyGlobalFun */

/* methodSearch - find class of optr, if any, starting at cl     */
Local FUNDEFREC *methodSearch(optr, cl, LINK)
short optr;
CLASSREC *cl;
struct LOC_eval *LINK;
{
  FUNDEFREC *f;

  f = NULL;
  while (f == NULL && cl != NULL) {
    f = fetchFun(optr, cl->exported);
    if (f == NULL)
      cl = cl->clsuper;
  }
  return f;
}  /* methodSearch */

/* applyMethod - apply method f to actuals                       */
Local STVALUEREC *applyMethod(f, actuals, LINK)
FUNDEFREC *f;
VALUELISTREC *actuals;
struct LOC_eval *LINK;
{
  ENVREC *rho;

  if (lengthNL(f->formals) != lengthVL(actuals) - 1) {
    printf("Wrong number of arguments to: ");
    prName(f->funname);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  rho = mkEnv(f->formals, actuals->tail);
  return (eval(f->body, rho, actuals->head));
}  /* applyMethod */

/* mkRepFor - make list of all zeros of same length as nl        */
Local VALUELISTREC *mkRepFor(nl)
NAMELISTREC *nl;
{
  if (nl == NULL)
    return NULL;
  else
    return (mkValuelist(falseValue, mkRepFor(nl->tail)));
}  /* mkRepFor */

/* applyCtrlOp - apply CONTROLOP op to args in rho               */
Local STVALUEREC *applyCtrlOp(op, args, LINK)
BUILTINOP op;
EXPLISTREC *args;
struct LOC_eval *LINK;
{
  STVALUEREC *Result, *v;
  CLASSREC *cl;
  STVALUEREC *newval;
  EXPLISTREC *WITH;

  WITH = args;
  switch (op) {

  case IFOP:
    if (isTrueVal(eval(WITH->head, LINK->rho, LINK->rcvr)))
      Result = eval(WITH->tail->head, LINK->rho, LINK->rcvr);
    else
      Result = eval(WITH->tail->tail->head, LINK->rho, LINK->rcvr);
    break;

  case WHILEOP:
    v = eval(WITH->head, LINK->rho, LINK->rcvr);
    while (isTrueVal(v)) {
      v = eval(WITH->tail->head, LINK->rho, LINK->rcvr);
      v = eval(WITH->head, LINK->rho, LINK->rcvr);
    }
    Result = v;
    break;

  case SETOP:
    v = eval(WITH->tail->head, LINK->rho, LINK->rcvr);
    if (isBound(WITH->head->UU.varble, LINK->rho))
      assign(WITH->head->UU.varble, v, LINK->rho);
    else if (isBound(WITH->head->UU.varble, LINK->rcvr->UU.userval))
      assign(WITH->head->UU.varble, v, LINK->rcvr->UU.userval);
    else if (isBound(WITH->head->UU.varble, globalEnv))
      assign(WITH->head->UU.varble, v, globalEnv);
    else
      bindVar(WITH->head->UU.varble, v, globalEnv);
    Result = v;
    break;

  case BEGINOP:
    while (args->tail != NULL) {
      v = eval(args->head, LINK->rho, LINK->rcvr);
      args = args->tail;
    }
    Result = eval(args->head, LINK->rho, LINK->rcvr);
    break;

  case NEWOP:
    /* Argument is a VAREXP with the name of a class */
    cl = fetchClass(args->head->UU.varble);
    newval = mkUSER(mkEnv(cl->clrep, mkRepFor(cl->clrep)), cl);
    assign(SELF, newval, newval->UU.userval);
    Result = newval;
    break;
  }/* case and with */
  return Result;
}  /* applyCtrlOp */


/*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************/
/* eval - return value of e in environment rho, receiver rcvr    */
Static STVALUEREC *eval(e, rho_, rcvr_)
EXPREC *e;
ENVREC *rho_;
STVALUEREC *rcvr_;
{
  struct LOC_eval V;
  STVALUEREC *Result;
  VALUELISTREC *vl;
  FUNDEFREC *f;

  V.rho = rho_;
  V.rcvr = rcvr_;
  switch (e->etype) {

  case VALEXP:
    Result = e->UU.valu;
    break;

  case VAREXP:
    if (isBound(e->UU.varble, V.rho))
      Result = fetch(e->UU.varble, V.rho);
    else if (isBound(e->UU.varble, V.rcvr->UU.userval))
      Result = fetch(e->UU.varble, V.rcvr->UU.userval);
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
    if (e->UU.U2.optr <= numCtrlOps)
      Result = applyCtrlOp(primOp(e->UU.U2.optr), e->UU.U2.args, &V);
    else {
      vl = evalList(e->UU.U2.args, &V);
      if (lengthVL(vl) == 0)
	Result = applyGlobalFun(e->UU.U2.optr, vl, &V);
      else {
	f = methodSearch(e->UU.U2.optr, vl->head->owner, &V);
	if (f != NULL)
	  Result = applyMethod(f, vl, &V);
	else if (e->UU.U2.optr <= numBuiltins)
	  Result = applyValueOp(primOp(e->UU.U2.optr), vl);
	else
	  Result = applyGlobalFun(e->UU.U2.optr, vl, &V);
      }
    }
    break;
  }/* case and with */
  return Result;
}  /* eval */


/*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************/

/* initHierarchy - allocate class Object and create an instance  */
Static Void initHierarchy()
{
  classes = NULL;
  OBJECTCLASS = newClass(install("Object                        "), NULL);
  OBJECTCLASS->exported = NULL;
  OBJECTCLASS->clrep = mkNamelist(SELF, NULL);
  objectInst = mkUSER(mkEnv(OBJECTCLASS->clrep, mkValuelist(mkINT(0L), NULL)),
		      OBJECTCLASS);
}  /* initHierarchy */


main(argc, argv)
int argc;
Char *argv[];
{  /* smalltalk main */
  PASCAL_MAIN(argc, argv);
  if (setjmp(_JL99))
    goto _L99;
  initNames();
  initHierarchy();
  globalEnv = emptyEnv();

  trueValue = mkINT(1L);
  falseValue = mkINT(0L);

  quittingtime = false;
_L99:
  while (!quittingtime) {
    reader();
    if (matches((long)pos_, 4, "quit                          ")) {
      quittingtime = true;
      break;
    }
    if ((userinput[pos_ - 1] == '(') & matches(skipblanks(pos_ + 1L), 6,
	  "define                        ")) {
      prName(parseDef(&fundefs));
      putchar('\n');
    } else if ((userinput[pos_ - 1] == '(') & matches(skipblanks(pos_ + 1L),
		 5, "class                         ")) {
      prName(parseClass());
      putchar('\n');
    } else {
      currentExp = parseExp();
      prValue(eval(currentExp, emptyEnv(), objectInst));
      printf("\n\n");
    }
  }  /* while */
  exit(0);
}  /* smalltalk */



/* End. */
