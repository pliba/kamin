/* Output from p2c, the Pascal-to-C translator */
/* From input file "prolog.p" */


/*****************************************************************
 *                     DECLARATIONS                              *
 *****************************************************************/

#include <p2c/p2c.h>


#define NAMELENG        20   /* Maximum length of a name */
#define MAXNAMES        300   /* Maximum number of different names */
#define MAXINPUT        2000   /* Maximum length of an input */

#define PROMPT          "-> "
#define PROMPT2         "> "
#define COMMENTCHAR     ";"

#define TABCODE         9   /* in ASCII */


typedef Char NAMESTRING[NAMELENG];

/* a NAME is an index in printNames */


typedef struct GOALREC {
  short pred;
  struct EXPLISTREC *args;
} GOALREC;

typedef struct GOALLISTREC {
  GOALREC *head;
  struct GOALLISTREC *tail;
} GOALLISTREC;

typedef struct VARIABLEREC {
  short varname;
  long varindex;
} VARIABLEREC;

typedef struct VARLISTREC {
  VARIABLEREC *head;
  struct VARLISTREC *tail;
} VARLISTREC;

typedef enum {
  VAREXP, INTEXP, APEXP
} EXPTYPE;

typedef struct EXPREC {
  EXPTYPE etype;
  union {
    VARIABLEREC *varble;
    long intval;
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

typedef struct CLAUSEREC {
  GOALREC *lhs;
  GOALLISTREC *rhs;
  struct CLAUSEREC *nextclause;
} CLAUSEREC;

typedef struct SUBSTREC {
  VARLISTREC *domain;
  EXPLISTREC *range;
} SUBSTREC;


Static CLAUSEREC *clauses, *lastclause;

Static GOALLISTREC *toplevelGoal;

Static Char userinput[MAXINPUT];
Static short inputleng, pos_;

Static NAMESTRING printNames[MAXNAMES];
Static short numNames, numBuiltins;

Static boolean quittingtime;


/*****************************************************************
 *                     DATA STRUCTURE OP'S                       *
 *****************************************************************/

/* mkGoal - create a new GOAL with pred p and arguments a        */
Static GOALREC *mkGoal(p, a)
short p;
EXPLISTREC *a;
{
  GOALREC *newg;

  newg = (GOALREC *)Malloc(sizeof(GOALREC));
  newg->pred = p;
  newg->args = a;
  return newg;
}  /* mkGoal */


/* mkVAREXP - create a new EXP of type VAREXP                    */
Static EXPREC *mkVAREXP(v)
VARIABLEREC *v;
{
  EXPREC *newe;

  newe = (EXPREC *)Malloc(sizeof(EXPREC));
  newe->etype = VAREXP;
  newe->UU.varble = v;
  return newe;
}  /* mkVAREXP */


/* mkINTEXP - create a new EXP of type INTEXP                    */
Static EXPREC *mkINTEXP(n)
long n;
{
  EXPREC *newe;

  newe = (EXPREC *)Malloc(sizeof(EXPREC));
  newe->etype = INTEXP;
  newe->UU.intval = n;
  return newe;
}  /* mkINTEXP */


/* mkAPEXP - create a new EXP of type APEXP                      */
Static EXPREC *mkAPEXP(o, a)
short o;
EXPLISTREC *a;
{
  EXPREC *newe;

  newe = (EXPREC *)Malloc(sizeof(EXPREC));
  newe->etype = APEXP;
  newe->UU.U2.optr = o;
  newe->UU.U2.args = a;
  return newe;
}  /* mkAPEXP */


/* mkVariable - create a new VARIABLE with name n and index i    */
Static VARIABLEREC *mkVariable(n, i)
short n;
long i;
{
  VARIABLEREC *newv;

  newv = (VARIABLEREC *)Malloc(sizeof(VARIABLEREC));
  newv->varname = n;
  newv->varindex = i;
  return newv;
}  /* mkVariable */


/* mkVarlist - create a new VARLIST with head v and tail vl      */
Static VARLISTREC *mkVarlist(v, vl)
VARIABLEREC *v;
VARLISTREC *vl;
{
  VARLISTREC *newvl;

  newvl = (VARLISTREC *)Malloc(sizeof(VARLISTREC));
  newvl->head = v;
  newvl->tail = vl;
  return newvl;
}  /* mkVarlist */


/* mkGoallist - return a GOALLIST with head g and tail gl        */
Static GOALLISTREC *mkGoallist(g, gl)
GOALREC *g;
GOALLISTREC *gl;
{
  GOALLISTREC *newgl;

  newgl = (GOALLISTREC *)Malloc(sizeof(GOALLISTREC));
  newgl->head = g;
  newgl->tail = gl;
  return newgl;
}  /* mkGoallist */


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


/* mkClause - create a new GOAL with lhs l and rhs r             */
Static CLAUSEREC *mkClause(l, r)
GOALREC *l;
GOALLISTREC *r;
{
  CLAUSEREC *c;

  c = (CLAUSEREC *)Malloc(sizeof(CLAUSEREC));
  c->lhs = l;
  c->rhs = r;
  c->nextclause = NULL;
  return c;
}  /* mkClause */


/* eqVar - compare two VARIABLE's for equality                   */
Static boolean eqVar(v1, v2)
VARIABLEREC *v1, *v2;
{
  return (v1->varname == v2->varname && v1->varindex == v2->varindex);
}  /* eqVar */


/* lengthEL - return length of EXPLIST el                        */
Static long lengthEL(el)
EXPLISTREC *el;
{
  long i;

  i = 0;
  while (el != NULL) {
    i++;
    el = el->tail;
  }
  return i;
}  /* lengthEL */


/*****************************************************************
 *                     NAME MANAGEMENT                           *
 *****************************************************************/

/* newClause - add new clause at end of clauses list             */
Static Void newClause(l, r)
GOALREC *l;
GOALLISTREC *r;
{
  if (lastclause == NULL) {
    clauses = mkClause(l, r);
    lastclause = clauses;
  } else {
    lastclause->nextclause = mkClause(l, r);
    lastclause = lastclause->nextclause;
  }
}  /* newClause */


/* initNames - place all pre-defined names into printNames       */
Static Void initNames()
{
  long i;

  clauses = NULL;
  lastclause = NULL;
  i = 1;
  memcpy(printNames[i - 1], "plus                ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "minus               ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "less                ", sizeof(NAMESTRING));
  i++;
  memcpy(printNames[i - 1], "print               ", sizeof(NAMESTRING));
  numBuiltins = i;
  numNames = i;
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
 *                      INPUT                                    *
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


/* matches - check if string nm matches userinput[s .. s+ln]     */
Static boolean matches(s, ln, nm)
long s;
char ln;
Char *nm;
{
  boolean match;
  long i;

  match = true;
  i = 1;
  while (match && i <= ln) {
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
  long parencnt;
  Char c;

  parencnt = 1;
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


/* isVar - check if first character of name n is upper-case      */
Static boolean isVar(n)
short n;
{
  return isupper(printNames[n - 1][0]);
}  /* isVar */


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


/* parseInt - return number starting at userinput[pos]           */
Static long parseInt()
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


Static EXPLISTREC *parseEL PV();


/* parseExp - return EXP starting at userinput[pos]              */
Static EXPREC *parseExp()
{
  short n;
  EXPLISTREC *el;

  if (userinput[pos_ - 1] == '(') {
    pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
    n = parseName();
    el = parseEL();
    return (mkAPEXP(n, el));
  } else if (isNumber((long)pos_))
    return (mkINTEXP(parseInt()));
  else {
    n = parseName();
    if (isVar(n))
      return (mkVAREXP(mkVariable(n, 0L)));
    else
      return (mkAPEXP(n, NULL));
  }
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


/* parseGoal - return GOAL starting at userinput[pos]            */
Static GOALREC *parseGoal()
{
  short pred;
  EXPLISTREC *il;

  if (userinput[pos_ - 1] != '(') {
    pred = parseName();
    il = NULL;
    return (mkGoal(pred, il));
  }
  pos_ = skipblanks(pos_ + 1L);   /* skip '( ...' */
  pred = parseName();
  il = parseEL();
  return (mkGoal(pred, il));
}  /* parseGoal */


/* parseGL - return GOALLIST starting at userinput[pos]          */
Static GOALLISTREC *parseGL()
{
  GOALREC *g;
  GOALLISTREC *gl;

  if (userinput[pos_ - 1] == ')') {
    pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
    return NULL;
  } else {
    g = parseGoal();
    gl = parseGL();
    return (mkGoallist(g, gl));
  }
}  /* parseGL */


/* parseClause - return CLAUSE at userinput[pos]                 */
Static Void parseClause()
{
  GOALREC *h;
  GOALLISTREC *g;

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 5L);   /* skip 'infer ..' */
  h = parseGoal();
  if (userinput[pos_ - 1] == ')')
    g = NULL;
  else {
    pos_ = skipblanks(pos_ + 4L);   /* skip 'from ..' */
    g = parseGL();
  }
  pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
  newClause(h, g);
}  /* parseClause */


/* parseQuery - return GOALLIST starting at userinput[pos]       */
Static GOALLISTREC *parseQuery()
{
  GOALLISTREC *Result;

  pos_ = skipblanks(pos_ + 1L);   /* skip '( ..' */
  pos_ = skipblanks(pos_ + 6L);   /* skip 'infer? ..' */
  Result = parseGL();
  pos_ = skipblanks(pos_ + 1L);   /* skip ') ..' */
  return Result;
}  /* parseQuery */


Static Void prExplist PP((EXPLISTREC *el));

Local Void prVariable(v)
VARIABLEREC *v;
{
  prName(v->varname);
  if (v->varindex > 0)
    printf("%ld", v->varindex);
}  /* prVariable */

Local Void prExp(e)
EXPREC *e;
{

  /* prVariable - print variable, including index                  */
  switch (e->etype) {

  case INTEXP:
    printf("%ld", e->UU.intval);
    break;

  case VAREXP:
    prVariable(e->UU.varble);
    break;

  case APEXP:
    if (e->UU.U2.args == NULL)
      prName(e->UU.U2.optr);
    else {
      putchar('(');
      prName(e->UU.U2.optr);
      if (e->UU.U2.args != NULL) {
	putchar(' ');
	prExplist(e->UU.U2.args);
      }
      putchar(')');
    }
    break;
  }/* case */
}  /* prExp */


/*****************************************************************
 *                     OUTPUT                                    *
 *****************************************************************/

/* prExplist - print an EXPLIST                                  */
Static Void prExplist(el)
EXPLISTREC *el;
{

  /* prExp - print an EXP                                          */
  if (el == NULL)
    return;
  prExp(el->head);
  if (el->tail != NULL) {
    putchar(' ');
    prExplist(el->tail);
  }
}  /* prExplist */


/*****************************************************************
 *                     SUBSTITUTIONS                             *
 *****************************************************************/

/* emptySubst - create a substitution with no bindings           */
Static SUBSTREC *emptySubst()
{
  SUBSTREC *s;

  s = (SUBSTREC *)Malloc(sizeof(SUBSTREC));
  s->domain = NULL;
  s->range = NULL;
  return s;
}  /* emptySubst */


/* bindVar - bind variable v to expression e in sigma            */
Static Void bindVar(v, e, sigma)
VARIABLEREC *v;
EXPREC *e;
SUBSTREC *sigma;
{
  sigma->domain = mkVarlist(v, sigma->domain);
  sigma->range = mkExplist(e, sigma->range);
}  /* bindVar */


/* findVar - look up variable v in sigma                         */
Static EXPLISTREC *findVar(v, sigma)
VARIABLEREC *v;
SUBSTREC *sigma;
{
  VARLISTREC *dl;
  EXPLISTREC *rl;
  boolean found;

  found = false;
  dl = sigma->domain;
  rl = sigma->range;
  while (dl != NULL && !found) {
    if (eqVar(dl->head, v))
      found = true;
    else {
      dl = dl->tail;
      rl = rl->tail;
    }
  }
  return rl;
}  /* findVar */


/* fetch - fetch binding of variable v in sigma                  */
Static EXPREC *fetch(v, sigma)
VARIABLEREC *v;
SUBSTREC *sigma;
{
  EXPLISTREC *el;

  el = findVar(v, sigma);
  return (el->head);
}  /* fetch */


/* isBound - check if variable v is bound in sigma               */
Static boolean isBound(v, sigma)
VARIABLEREC *v;
SUBSTREC *sigma;
{
  return (findVar(v, sigma) != NULL);
}  /* isBound */


Static Void applyToExplist PP((SUBSTREC *s, EXPLISTREC *el));


/* applyToExp - apply substitution s to e, modifying e           */
Static Void applyToExp(s, e)
SUBSTREC *s;
EXPREC **e;
{
  switch ((*e)->etype) {

  case INTEXP:
    /* blank case */
    break;

  case VAREXP:
    if (isBound((*e)->UU.varble, s))
      *e = fetch((*e)->UU.varble, s);
    break;

  case APEXP:
    applyToExplist(s, (*e)->UU.U2.args);
    break;
  }
}  /* applyToExp */


/* applyToExplist - apply substitution s to el, modifying el     */
Static Void applyToExplist(s, el)
SUBSTREC *s;
EXPLISTREC *el;
{
  while (el != NULL) {
    applyToExp(s, &el->head);
    el = el->tail;
  }
}  /* applyToExplist */


/* applyToGoal - apply substitution s to g, modifying g          */
Static Void applyToGoal(s, g)
SUBSTREC *s;
GOALREC *g;
{
  applyToExplist(s, g->args);
}  /* applyToGoal */


/* applyToGoallist - apply substitution s to gl, modifying gl    */
Static Void applyToGoallist(s, gl)
SUBSTREC *s;
GOALLISTREC *gl;
{
  while (gl != NULL) {
    applyToGoal(s, gl->head);
    gl = gl->tail;
  }
}  /* applyToGoallist */


/* compose - change substitution s1 to composition of s1 and s2  */
Static Void compose(s1, s2)
SUBSTREC *s1, *s2;
{
  VARLISTREC *dom;
  EXPLISTREC *rng;

  applyToExplist(s2, s1->range);
  if (s1->domain == NULL) {
    s1->domain = s2->domain;
    s1->range = s2->range;
    return;
  }
  dom = s1->domain;
  rng = s1->range;
  while (dom->tail != NULL) {
    dom = dom->tail;
    rng = rng->tail;
  }
  dom->tail = s2->domain;
  rng->tail = s2->range;
}  /* compose */


/* Local variables for unify: */
struct LOC_unify {
  EXPREC *diff1, *diff2;
} ;

Local boolean findExpDiff PP((EXPREC *e1, EXPREC *e2, struct LOC_unify *LINK));

/* findELDiff - set diff1, diff2 to EXP's where el1, el2 differ  */
Local boolean findELDiff(el1, el2, LINK)
EXPLISTREC *el1, *el2;
struct LOC_unify *LINK;
{
  boolean foundDiff;

  foundDiff = false;
  while (el1 != NULL && !foundDiff) {
    foundDiff = findExpDiff(el1->head, el2->head, LINK);
    el1 = el1->tail;
    el2 = el2->tail;
  }
  return foundDiff;
}  /* findELDiff */

/* findExpDiff - set diff1, diff2 to EXP's where e1, e2 differ   */
Local boolean findExpDiff(e1, e2, LINK)
EXPREC *e1, *e2;
struct LOC_unify *LINK;
{
  boolean Result;

  Result = true;
  LINK->diff1 = e1;
  LINK->diff2 = e2;
  if (e1->etype != e2->etype)
    return Result;
  switch (e1->etype) {

  case VAREXP:
    if (eqVar(e1->UU.varble, e2->UU.varble))
      Result = false;
    break;

  case INTEXP:
    if (e1->UU.intval == e2->UU.intval)
      Result = false;
    break;

  case APEXP:
    if (e1->UU.U2.optr == e2->UU.U2.optr)
      Result = findELDiff(e1->UU.U2.args, e2->UU.U2.args, LINK);
    break;
  }/* case */
  return Result;
}  /* findExpDiff */

/* occursInExp - check whether variable v occurs in exp e        */
Local boolean occursInExp(v, e, LINK)
VARIABLEREC *v;
EXPREC *e;
struct LOC_unify *LINK;
{
  boolean Result, occurs;
  EXPLISTREC *el;

  switch (e->etype) {

  case INTEXP:
    Result = false;
    break;

  case VAREXP:
    Result = eqVar(v, e->UU.varble);
    break;

  case APEXP:
    occurs = false;
    el = e->UU.U2.args;
    while (el != NULL && !occurs) {
      occurs = occursInExp(v, el->head, LINK);
      el = el->tail;
    }
    Result = occurs;
    break;
  }/* case and with */
  return Result;
}  /* occursInExp */

/* makeSubst - bind d1 to d2 in s, first checking if possible    */
Local Void makeSubst(d1, d2, s, LINK)
EXPREC *d1, *d2;
SUBSTREC **s;
struct LOC_unify *LINK;
{
  if (d1->etype != VAREXP) {
    *s = NULL;
    return;
  }
  if (occursInExp(d1->UU.varble, d2, LINK))
    *s = NULL;
  else
    bindVar(d1->UU.varble, d2, *s);
}  /* makeSubst */


/*****************************************************************
 *                     UNIFICATION                               *
 *****************************************************************/

/* unify - unify g1 and g2; return unifying subst. (or nil)      */

Static SUBSTREC *unify(g1, g2)
GOALREC *g1, *g2;
{
  struct LOC_unify V;
  SUBSTREC *sigma, *varsubst;
  boolean foundDiff;

  sigma = emptySubst();
  do {
    foundDiff = findELDiff(g1->args, g2->args, &V);
    varsubst = emptySubst();
    if (foundDiff) {
      if (V.diff1->etype == VAREXP)
	makeSubst(V.diff1, V.diff2, &varsubst, &V);
      else
	makeSubst(V.diff2, V.diff1, &varsubst, &V);
    }
    if (foundDiff && varsubst != NULL) {   /* done */
      applyToGoal(varsubst, g1);
      applyToGoal(varsubst, g2);
      compose(sigma, varsubst);
    }
  } while (foundDiff && varsubst != NULL);   /* not unifiable */
  if (varsubst == NULL)
    return NULL;
  else
    return sigma;
}  /* unify */


/* Local variables for applyPrim: */
struct LOC_applyPrim {
  boolean Result;
  SUBSTREC **sigma;
  EXPLISTREC *arglist;
  EXPREC *arg1, *arg2, *arg3;
} ;

Local Void applyArith(op, LINK)
long op;
struct LOC_applyPrim *LINK;
{
  long i;

  LINK->arg3 = LINK->arglist->tail->tail->head;
  if (LINK->arg3->etype == APEXP) {
    LINK->Result = false;
    return;
  }
  switch (op) {

  case 1:
    i = LINK->arg1->UU.intval + LINK->arg2->UU.intval;
    break;

  case 2:
    i = LINK->arg1->UU.intval - LINK->arg2->UU.intval;
    break;
  }
  if (LINK->arg3->etype == INTEXP) {
    if (LINK->arg3->UU.intval != i)
      LINK->Result = false;
  } else
    bindVar(LINK->arg3->UU.varble, mkINTEXP(i), *LINK->sigma);

  /* applyPrim already true */
}  /* applyArith */


/*****************************************************************
 *                     EVALUATION                                *
 *****************************************************************/

/* applyPrim - apply primitive predicate, modifying sigma        */
Static boolean applyPrim(g, sigma_)
GOALREC *g;
SUBSTREC **sigma_;
{
  struct LOC_applyPrim V;

  V.sigma = sigma_;
  *V.sigma = emptySubst();
  V.Result = true;
  V.arglist = g->args;
  if (g->pred == 4) {   /* print */
    prExplist(V.arglist);
    putchar('\n');
    return V.Result;
  }
  V.arg1 = V.arglist->head;
  V.arg2 = V.arglist->tail->head;
  if (V.arg1->etype != INTEXP || V.arg2->etype != INTEXP)
    return false;
  switch (g->pred) {

  case 1:
  case 2:   /* plus, minus */
    applyArith((long)g->pred, &V);
    break;

  case 3:   /* less */
    if (V.arg1->UU.intval >= V.arg2->UU.intval)
      V.Result = false;
    break;
  }/* case */
  return V.Result;
}  /* applyPrim */


Static GOALREC *copyGoal PP((GOALREC *g, long id));


/* copyGoallist - copy gl; rename variables if id<>0             */
Static GOALLISTREC *copyGoallist(gl, id)
GOALLISTREC *gl;
long id;
{
  if (gl == NULL)
    return NULL;
  else
    return (mkGoallist(copyGoal(gl->head, id), copyGoallist(gl->tail, id)));
}  /* copyGoallist */


/* Local variables for copyGoal: */
struct LOC_copyGoal {
  long id;
} ;

Local EXPLISTREC *copyExplist PP((EXPLISTREC *el, struct LOC_copyGoal *LINK));

/* Local variables for copyExplist: */
struct LOC_copyExplist {
  struct LOC_copyGoal *LINK;
} ;

Local EXPREC *copyExp(e, LINK)
EXPREC *e;
struct LOC_copyExplist *LINK;
{
  EXPREC *Result;

  switch (e->etype) {

  case INTEXP:
    Result = e;
    break;

  case VAREXP:
    if (LINK->LINK->id == 0)
      Result = mkVAREXP(mkVariable(e->UU.varble->varname,
				   e->UU.varble->varindex));
    else
      Result = mkVAREXP(mkVariable(e->UU.varble->varname, LINK->LINK->id));
    break;

  case APEXP:
    Result = mkAPEXP(e->UU.U2.optr, copyExplist(e->UU.U2.args, LINK->LINK));
    break;
  }/* case */
  return Result;
}  /* copyExp */

Local EXPLISTREC *copyExplist(el, LINK)
EXPLISTREC *el;
struct LOC_copyGoal *LINK;
{

  /* copyExp - copy e; rename variables if id<>0                   */
  struct LOC_copyExplist V;

  V.LINK = LINK;
  if (el == NULL)
    return NULL;
  else
    return (mkExplist(copyExp(el->head, &V), copyExplist(el->tail, LINK)));
}  /* copyExplist */


/* copyGoal - copy g; rename variables if id<>0                  */
Static GOALREC *copyGoal(g, id_)
GOALREC *g;
long id_;
{

  /* copyExplist - copy el; rename variables if id<>0              */
  struct LOC_copyGoal V;

  V.id = id_;
  return (mkGoal(g->pred, copyExplist(g->args, &V)));
}  /* copyGoal */


/* append - append second to end of first, modifying first       */
Static GOALLISTREC *append(first, second)
GOALLISTREC *first, *second;
{
  GOALLISTREC *Result;

  if (first == NULL)
    return second;
  Result = first;
  while (first->tail != NULL)
    first = first->tail;
  first->tail = second;
  return Result;
}  /* append */


Static SUBSTREC *prove PP((GOALLISTREC *gl, long id));

/* Local variables for prove: */
struct LOC_prove {
  long id;
  SUBSTREC *sigma0;
} ;

/* tryClause - try to match goal g and clause head of c          */
Local SUBSTREC *tryClause(clgoal, g, LINK)
GOALREC *clgoal, *g;
struct LOC_prove *LINK;
{
  SUBSTREC *Result;

  Result = NULL;
  if (!((clgoal->pred == g->pred) & (lengthEL(clgoal->args) == lengthEL(g->args))))
    return Result;
  clgoal = copyGoal(clgoal, LINK->id);
  g = copyGoal(g, 0L);
  return (unify(clgoal, g));
/* p2c: prolog.p: Note: Deleting unreachable code [255] */
}  /* tryClause */

/* proveRest - add subgoals to restofgoals and prove             */
Local SUBSTREC *proveRest(subgoals, restofgoals, LINK)
GOALLISTREC *subgoals, *restofgoals;
struct LOC_prove *LINK;
{
  subgoals = copyGoallist(subgoals, LINK->id);
  applyToGoallist(LINK->sigma0, subgoals);
  restofgoals = copyGoallist(restofgoals, 0L);
  applyToGoallist(LINK->sigma0, restofgoals);
  return (prove(append(subgoals, restofgoals), LINK->id + 1));
}  /* proveRest */


/* prove - prove goals gl; return subst; id used to rename var's */
Static SUBSTREC *prove(gl, id_)
GOALLISTREC *gl;
long id_;
{
  struct LOC_prove V;
  CLAUSEREC *cl;
  SUBSTREC *sigma1;

  V.id = id_;
  if (gl == NULL)
    return (emptySubst());
  else {
    if (gl->head->pred <= numBuiltins) {
      if (applyPrim(gl->head, &V.sigma0)) {
	applyToGoallist(V.sigma0, gl->tail);
	sigma1 = prove(gl->tail, V.id + 1);
      } else
	sigma1 = NULL;
    } else {
      sigma1 = NULL;
      cl = clauses;
      while (cl != NULL && sigma1 == NULL) {
	V.sigma0 = tryClause(cl->lhs, gl->head, &V);
	if (V.sigma0 != NULL)
	  sigma1 = proveRest(cl->rhs, gl->tail, &V);
	cl = cl->nextclause;
      }  /* while */
    }
    if (sigma1 == NULL)
      return NULL;
    else {
      compose(V.sigma0, sigma1);
      return V.sigma0;
    }
  }
}  /* prove */


/*****************************************************************
 *                     READ-EVAL-PRINT LOOP                      *
 *****************************************************************/

main(argc, argv)
int argc;
Char *argv[];
{  /* prolog main */
  PASCAL_MAIN(argc, argv);
  if (setjmp(_JL99))
    goto _L99;
  initNames();

  quittingtime = false;
_L99:
  while (!quittingtime) {
    reader();
    if (matches((long)pos_, 4, "quit                ")) {
      quittingtime = true;
      break;
    }
    if (matches(skipblanks(pos_ + 1L), 5, "infer               ")) {
      parseClause();
      putchar('\n');
    } else {
      toplevelGoal = parseQuery();
      putchar('\n');
      if (prove(toplevelGoal, 1L) == NULL)
	printf("Not satisfied\n");
      else
	printf("Satisfied\n");
      putchar('\n');
    }
  }
  exit(0);
}  /* prolog */



/* End. */
