/* Output from p2c, the Pascal-to-C translator */
/* From input file "clu.p" */


/*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************/

#include <p2c/p2c.h>


#define NAMELENG        20   /* Maximum length of a name */
#define MAXNAMES        150   /* Maximum number of different names */
#define MAXINPUT        5000   /* Maximum length of an input */

#define PROMPT          "-> "
#define PROMPT2         "> "
#define COMMENTCHAR     ";"

#define TABCODE         9   /* in ASCII */


typedef Char NAMESTRING[NAMELENG];

/* a NAME is an index in printNames */

typedef enum {
  ONEPART, TWOPART
} FNAMETYPE;

typedef struct FUNNAME {
  uchar funpart;
  FNAMETYPE nametype;
  union {
    uchar clpart;
  } UU;
} FUNNAME;

typedef enum {
  IFOP, WHILEOP, SETOP, BEGINOP, PLUSOP, MINUSOP, TIMESOP, DIVOP, EQOP, LTOP,
  GTOP, PRINTOP
} BUILTINOP;


typedef enum {
  PRIM, USER
} CLUVALUETYPE;

typedef struct CLUVALUEREC {
  CLUVALUETYPE vtype;
  union {
    long intval;
    struct ENVREC *userval;
  } UU;
} CLUVALUEREC;

typedef enum {
  VALEXP, VAREXP, APEXP
} EXPTYPE;

typedef struct EXPREC {
  EXPTYPE etype;
  union {
    CLUVALUEREC *valu;
    uchar varble;
    struct {
      FUNNAME optr;
      struct EXPLISTREC *args;
    } U2;
  } UU;
} EXPREC;

typedef struct EXPLISTREC {
  EXPREC *head;
  struct EXPLISTREC *tail;
} EXPLISTREC;

typedef struct VALUELISTREC {
  CLUVALUEREC *head;
  struct VALUELISTREC *tail;
} VALUELISTREC;

typedef struct NAMELISTREC {
  uchar head;
  struct NAMELISTREC *tail;
} NAMELISTREC;

typedef struct ENVREC {
  NAMELISTREC *vars;
  VALUELISTREC *values;
} ENVREC;

typedef enum {
  NORMAL, CONSTRUCTOR, SELECTOR, SETTOR
} FUNTYPE;

typedef struct FUNDEFREC {
  uchar funname;
  struct FUNDEFREC *nextfundef;
  FUNTYPE ftype;
  union {
    struct {
      NAMELISTREC *formals;
      EXPREC *body;
    } U0;
    uchar selname;
  } UU;
} FUNDEFREC;

typedef struct CLUSTERREC {
  uchar clname;
  NAMELISTREC *clrep;
  FUNDEFREC *exported, *nonexported;
  struct CLUSTERREC *nextcluster;
} CLUSTERREC;


Static FUNDEFREC *fundefs;
Static CLUSTERREC *clusters;

Static ENVREC *globalEnv;

Static EXPREC *currentExp;

Static Char userinput[MAXINPUT];
Static short inputleng, pos_;

Static NAMESTRING printNames[MAXNAMES];
Static uchar numNames, numBuiltins;

Static boolean quittingtime;


/*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************/

/* mkVALEXP - return an EXP of type VALEXP with valu v           */
Static EXPREC *mkVALEXP(v)
CLUVALUEREC *v;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VALEXP;
  e->UU.valu = v;
  return e;
}  /* mkVALEXP */


/* mkVAREXP - return an EXP of type VAREXP with varble nm        */
Static EXPREC *mkVAREXP(nm)
uchar nm;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = VAREXP;
  e->UU.varble = nm;
  return e;
}  /* mkVAREXP */


/* mkAPEXP - return EXP of type APEXP w/ optr op or cl$op        */
Static EXPREC *mkAPEXP(ot, op, cl, el)
FNAMETYPE ot;
uchar op, cl;
EXPLISTREC *el;
{
  EXPREC *e;

  e = (EXPREC *)Malloc(sizeof(EXPREC));
  e->etype = APEXP;
  e->UU.U2.optr.funpart = op;
  e->UU.U2.optr.nametype = ot;
  if (ot == TWOPART)
    e->UU.U2.optr.UU.clpart = cl;
  e->UU.U2.args = el;
  return e;
}  /* mkAPEXP */


/* mkPRIM - return a CLUVALUE with integer value n               */
Static CLUVALUEREC *mkPRIM(n)
long n;
{
  CLUVALUEREC *newval;

  newval = (CLUVALUEREC *)Malloc(sizeof(CLUVALUEREC));
  newval->vtype = PRIM;
  newval->UU.intval = n;
  return newval;
}  /* mkPRIM */


/* mkUSER - return a user-type CLUVALUE                          */
Static CLUVALUEREC *mkUSER(rho)
ENVREC *rho;
{
  CLUVALUEREC *newval;

  newval = (CLUVALUEREC *)Malloc(sizeof(CLUVALUEREC));
  newval->vtype = USER;
  newval->UU.userval = rho;
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
uchar nm;
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
CLUVALUEREC *v;
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

/* fetchCluster - get cluster definition of cname from clusters  */
Static CLUSTERREC *fetchCluster(cname)
uchar cname;
{
  CLUSTERREC *cl;
  boolean found;

  found = false;
  cl = clusters;
  while (cl != NULL && !found) {
    if (cl->clname == cname)
      found = true;
    else
      cl = cl->nextcluster;
  }
  return cl;
}  /* fetchCluster */


/* newCluster - add new cluster cname to clusters                */
Static CLUSTERREC *newCluster(cname)
uchar cname;
{
  CLUSTERREC *cl;

  cl = fetchCluster(cname);
  if (cl != NULL)   /* cname not yet defined as cluster */
    return cl;
  cl = (CLUSTERREC *)Malloc(sizeof(CLUSTERREC));
  cl->clname = cname;
  cl->nextcluster = clusters;   /* place new CLUSTERREC */
  clusters = cl;   /* on clusters list */
  return cl;
}  /* newCluster */


/* fetchFun - get function definition of NAME fname from fenv    */
Static FUNDEFREC *fetchFun(fname, fenv)
uchar fname;
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
uchar fname;
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
  clusters = NULL;
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
Static uchar install(nm)
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
uchar nm;
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
uchar optr;
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
  return (c == ';' || c == '$' || c == ' ' || c == ')' || c == '(');
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
Static uchar parseName()
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
Static CLUVALUEREC *parseVal()
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
  return (mkPRIM(n * sign));
}  /* parseVal */


Static EXPLISTREC *parseEL PV();


/* parseExp - return EXP starting at userinput[pos]              */
Static EXPREC *parseExp()
{
  uchar fnm, cnm;
  EXPLISTREC *el;
  FNAMETYPE optrtype;

  if (userinput[pos_ - 1] == '(') {  /* APEXP */
    pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
    optrtype = ONEPART;
    cnm = 1;   /* arbitrary name */
    fnm = parseName();
    if (userinput[pos_ - 1] == '$') {  /* two-part name */
      pos_++;
      cnm = fnm;
      optrtype = TWOPART;
      fnm = parseName();
    }
    el = parseEL();
    return (mkAPEXP(optrtype, fnm, cnm, el));
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
  uchar nm;
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
Static uchar parseDef(fenv)
FUNDEFREC **fenv;
{
  uchar fname;   /* function name */
  FUNDEFREC *newfun;   /* new FUNDEFREC */

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 6L);   /* skip 'define ..' */
  fname = parseName();
  newfun = newFunDef(fname, fenv);
  newfun->ftype = NORMAL;
  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  newfun->UU.U0.formals = parseNL();
  newfun->UU.U0.body = parseExp();
  pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
  return fname;
}  /* parseDef */


/* mkSetName - make name of settor corresponding to selector nm  */
Local uchar mkSetName(nm)
uchar nm;
{
  NAMESTRING setname;
  long i;

  memcpy(setname, "set-                ", sizeof(NAMESTRING));
  if (printNames[nm - 1][NAMELENG - 4] != ' ') {
    printf("Selector name too long: ");
    prName(nm);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  for (i = 1; i <= NAMELENG - 4; i++)
    setname[i + 3] = printNames[nm - 1][i - 1];
  return (install(setname));
}  /* mkSetName */


/* parseCluster - parse cluster definition at userinput[pos]     */
Static uchar parseCluster()
{
  uchar cname, sel, fname;
  CLUSTERREC *newclust;
  NAMELISTREC *rep;
  FUNDEFREC *cenv, *confun, *selfun, *setfun;

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 7L);   /* skip 'cluster ...' */
  cname = parseName();
  newclust = newCluster(cname);
  pos_ = skipblanks(pos_ + 1L);   /* skip '( ...' */
  pos_ = skipblanks(pos_ + 3L);   /* skip 'rep ...' */
  rep = parseNL();   /* selector names */
  newclust->clrep = rep;
  cenv = NULL;
  while (userinput[pos_ - 1] == '(') {
    fname = parseDef(&cenv);
    prName(fname);
    putchar('\n');
  }
  newclust->exported = cenv;
  cenv = NULL;
  confun = newFunDef(cname, &cenv);
  confun->ftype = CONSTRUCTOR;
  while (rep != NULL) {
    sel = rep->head;
    selfun = newFunDef(sel, &cenv);
    selfun->ftype = SELECTOR;
    setfun = newFunDef(mkSetName(sel), &cenv);
    setfun->ftype = SETTOR;
    setfun->UU.selname = sel;
    rep = rep->tail;
  }
  newclust->nonexported = cenv;
  pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
  return cname;
}  /* parseCluster */


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
uchar nm;
CLUVALUEREC *v;
ENVREC *rho;
{
  rho->vars = mkNamelist(nm, rho->vars);
  rho->values = mkValuelist(v, rho->values);
}  /* bindVar */


/* findVar - look up nm in rho                                   */
Static VALUELISTREC *findVar(nm, rho)
uchar nm;
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
uchar nm;
CLUVALUEREC *v;
ENVREC *rho;
{
  VALUELISTREC *varloc;

  varloc = findVar(nm, rho);
  varloc->head = v;
}  /* assign */


/* fetch - return number bound to nm in rho                      */
Static CLUVALUEREC *fetch(nm, rho)
uchar nm;
ENVREC *rho;
{
  VALUELISTREC *vl;

  vl = findVar(nm, rho);
  return (vl->head);
}  /* fetch */


/* isBound - check if nm is bound in rho                         */
Static boolean isBound(nm, rho)
uchar nm;
ENVREC *rho;
{
  return (findVar(nm, rho) != NULL);
}  /* isBound */


/*****************************************************************
 *                           VALUES                              *
 *****************************************************************/

/* prValue - print value v                                       */
Static Void prValue(v)
CLUVALUEREC *v;
{
  if (v->vtype == PRIM)
    printf("%ld", v->UU.intval);
  else
    printf("<userval>");
}  /* prValue */


/* isTrueVal - return true if v is true (non-zero) value         */
Static boolean isTrueVal(v)
CLUVALUEREC *v;
{
  if (v->vtype == USER)
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
Static CLUVALUEREC *applyValueOp(op, vl)
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
  if (op == PRINTOP) {
    prValue(vl->head);
    putchar('\n');
    return (vl->head);
  } else {
    if (vl->head->vtype != PRIM || vl->tail->head->vtype != PRIM) {
      printf("Arguments to primitive op not primitive: ");
      prName((int)op+1);
      putchar('\n');
      longjmp(_JL99, 1);
    }
    n1 = vl->head->UU.intval;   /* 1st actual */
    n2 = vl->tail->head->UU.intval;   /* 2nd actual */
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
    }/* case */
    return (mkPRIM(n));
  }
}  /* applyValueOp */


Static CLUVALUEREC *eval PP((EXPREC *e, ENVREC *rho, CLUSTERREC *c));

/* Local variables for eval: */
struct LOC_eval {
  ENVREC *rho;
  CLUSTERREC *c;
} ;

/* evalList - evaluate each expression in el                     */
Local VALUELISTREC *evalList(el, LINK)
EXPLISTREC *el;
struct LOC_eval *LINK;
{
  CLUVALUEREC *h;
  VALUELISTREC *t;

  if (el == NULL)
    return NULL;
  else {
    h = eval(el->head, LINK->rho, LINK->c);
    t = evalList(el->tail, LINK);
    return (mkValuelist(h, t));
  }
}  /* evalList */

/* Local variables for applyUserFun: */
struct LOC_applyUserFun {
  struct LOC_eval *LINK;
  VALUELISTREC *actuals;
} ;

/* Local variables for checkArgs: */
struct LOC_checkArgs {
  struct LOC_applyUserFun *LINK;
  FUNNAME nm;
  FUNDEFREC *f;
  CLUSTERREC *cl;
} ;

Local long arity_(LINK)
struct LOC_checkArgs *LINK;
{
  long Result;
  FUNDEFREC *WITH;

  WITH = LINK->f;
  switch (WITH->ftype) {

  case NORMAL:
    Result = lengthNL(WITH->UU.U0.formals);
    break;

  case CONSTRUCTOR:
    Result = lengthNL(LINK->cl->clrep);
    break;

  case SELECTOR:
    Result = 1;
    break;

  case SETTOR:
    Result = 2;
    break;
  }/* case and with */
  return Result;
}  /* arity */

/* typeError - print type error message                          */
Local Void typeError(LINK)
struct LOC_checkArgs *LINK;
{
  printf("Wrong type argument to: ");
  prName(LINK->nm.funpart);
  putchar('\n');
  longjmp(_JL99, 1);
}  /* typeError */

/* checkArgs - check number/type (as far as possible) of args    */
Local Void checkArgs(nm_, f_, cl_, LINK)
FUNNAME nm_;
FUNDEFREC *f_;
CLUSTERREC *cl_;
struct LOC_applyUserFun *LINK;
{

  /* arity - number of arguments expected by f                     */
  struct LOC_checkArgs V;
  FUNDEFREC *WITH;

  V.LINK = LINK;
  V.nm = nm_;
  V.f = f_;
  V.cl = cl_;
  if (arity_(&V) != lengthVL(LINK->actuals)) {
    printf("Wrong number of arguments to: ");
    prName(V.nm.funpart);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  WITH = V.f;
  if (((1L << ((long)WITH->ftype)) &
       ((1L << ((long)SELECTOR)) | (1 << ((long)SETTOR)))) != 0) {
    if (LINK->actuals->head->vtype == PRIM)
      typeError(&V);
  }
  if (WITH->ftype == SELECTOR) {
    if (!isBound(V.nm.funpart, LINK->actuals->head->UU.userval))
      typeError(&V);
  }
  if (WITH->ftype == SETTOR) {
    if (!isBound(WITH->UU.selname, LINK->actuals->head->UU.userval))
      typeError(&V);
  }
}  /* checkArgs */

/* applyUserFun - look up definition of nm and apply to actuals  */
Local CLUVALUEREC *applyUserFun(nm, actuals_, LINK)
FUNNAME nm;
VALUELISTREC *actuals_;
struct LOC_eval *LINK;
{
  struct LOC_applyUserFun V;
  CLUVALUEREC *Result;
  FUNDEFREC *f;
  ENVREC *rho, *valrho;
  CLUVALUEREC *v;
  CLUSTERREC *cl;

  V.LINK = LINK;
  V.actuals = actuals_;
  if (nm.nametype == TWOPART) {
    cl = fetchCluster(nm.UU.clpart);
    if (cl == NULL) {
      printf("Non-existent cluster: ");
      prName(nm.UU.clpart);
      putchar('\n');
      longjmp(_JL99, 1);
    }
    f = fetchFun(nm.funpart, cl->exported);
  } else {  /* one-part name */
    cl = LINK->c;
    if (cl == NULL)   /* called from top level */
      f = fetchFun(nm.funpart, fundefs);
    else {  /* try exported function first */
      f = fetchFun(nm.funpart, cl->exported);
      if (f == NULL) {  /* else non-exported */
	f = fetchFun(nm.funpart, cl->nonexported);
	if (f == NULL) {  /* else top-level */
	  cl = NULL;
	  f = fetchFun(nm.funpart, fundefs);
	}
      }
    }
  }
  if (f == NULL) {
    printf("Undefined function: ");
    prName(nm.funpart);
    putchar('\n');
    longjmp(_JL99, 1);
  }
  checkArgs(nm, f, cl, &V);
  switch (f->ftype) {

  case NORMAL:
    rho = mkEnv(f->UU.U0.formals, V.actuals);
    Result = eval(f->UU.U0.body, rho, cl);
    break;

  case CONSTRUCTOR:
    Result = mkUSER(mkEnv(cl->clrep, V.actuals));
    break;

  case SELECTOR:
    valrho = V.actuals->head->UU.userval;
    Result = fetch(nm.funpart, valrho);
    break;

  case SETTOR:
    valrho = V.actuals->head->UU.userval;
    v = V.actuals->tail->head;
    assign(f->UU.selname, v, valrho);
    Result = v;
    break;
  }/* case and with */
  return Result;
}  /* applyUserFun */

/* applyCtrlOp - apply CONTROLOP op to args in rho               */
Local CLUVALUEREC *applyCtrlOp(op, args, LINK)
BUILTINOP op;
EXPLISTREC *args;
struct LOC_eval *LINK;
{
  CLUVALUEREC *Result, *v;
  EXPLISTREC *WITH;

  WITH = args;
  switch (op) {

  case IFOP:
    if (isTrueVal(eval(WITH->head, LINK->rho, LINK->c)))
      Result = eval(WITH->tail->head, LINK->rho, LINK->c);
    else
      Result = eval(WITH->tail->tail->head, LINK->rho, LINK->c);
    break;

  case WHILEOP:
    v = eval(WITH->head, LINK->rho, LINK->c);
    while (isTrueVal(v)) {
      v = eval(WITH->tail->head, LINK->rho, LINK->c);
      v = eval(WITH->head, LINK->rho, LINK->c);
    }
    Result = v;
    break;

  case SETOP:
    v = eval(WITH->tail->head, LINK->rho, LINK->c);
    if (isBound(WITH->head->UU.varble, LINK->rho))
      assign(WITH->head->UU.varble, v, LINK->rho);
    else if (isBound(WITH->head->UU.varble, globalEnv))
      assign(WITH->head->UU.varble, v, globalEnv);
    else
      bindVar(WITH->head->UU.varble, v, globalEnv);
    Result = v;
    break;

  case BEGINOP:
    while (args->tail != NULL) {
      v = eval(args->head, LINK->rho, LINK->c);
      args = args->tail;
    }
    Result = eval(args->head, LINK->rho, LINK->c);
    break;
  }/* case and with */
  return Result;
}  /* applyCtrlOp */


/*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************/
/* eval - return value of e in environment rho, cluster c        */
Static CLUVALUEREC *eval(e, rho_, c_)
EXPREC *e;
ENVREC *rho_;
CLUSTERREC *c_;
{
  struct LOC_eval V;
  CLUVALUEREC *Result;
  BUILTINOP op;

  V.rho = rho_;
  V.c = c_;
  switch (e->etype) {

  case VALEXP:
    Result = e->UU.valu;
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
    if (e->UU.U2.optr.funpart > numBuiltins)
      Result = applyUserFun(e->UU.U2.optr, evalList(e->UU.U2.args, &V), &V);
    else {
      op = primOp(e->UU.U2.optr.funpart);
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
{  /* clu main */
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
      prName(parseDef(&fundefs));
      putchar('\n');
    } else if ((userinput[pos_ - 1] == '(') & matches(skipblanks(pos_ + 1L),
		 7, "cluster             ")) {
      prName(parseCluster());
      putchar('\n');
    } else {
      currentExp = parseExp();
      prValue(eval(currentExp, emptyEnv(), NULL));
      printf("\n\n");
    }
  }  /* while */
  exit(0);
}  /* clu */



/* End. */
