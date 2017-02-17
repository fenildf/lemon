/*
** This file contains all sources (including headers) to the LEMON
** LALR(1) parser generator.  The sources have been combined into a
** single file to make it easy to include LEMON in the source tree
** and Makefile of another program.
**
** The author of this program disclaims copyright.
*/
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#ifndef __WIN32__
#   if defined(_WIN32) || defined(WIN32)
#	define __WIN32__
#   endif
#endif

/* #define PRIVATE static */
#define PRIVATE

#ifdef TEST
#define MAXRHS 5       /* Set low to exercise exception code */
#else
#define MAXRHS 1000
#endif

char *msort();
extern void *malloc();

/******** From the file "action.h" *************************************/
struct action *Action_new();
struct action *Action_sort();

/********* From the file "assert.h" ************************************/
void myassert();
#ifndef NDEBUG
#  define assert(X) if(!(X))myassert(__FILE__,__LINE__)
#else
#  define assert(X)
#endif

/********** From the file "build.h" ************************************/
void FindRulePrecedences();
void FindFirstSets();
void FindStates();
void FindLinks();
void FindFollowSets();
void FindActions();

/********* From the file "configlist.h" *********************************/
void Configlist_init(/* void */);
struct config *Configlist_add(/* struct rule *, int */);
struct config *Configlist_addbasis(/* struct rule *, int */);
void Configlist_closure(/* void */);
void Configlist_sort(/* void */);
void Configlist_sortbasis(/* void */);
struct config *Configlist_return(/* void */);
struct config *Configlist_basis(/* void */);
void Configlist_eat(/* struct config * */);
void Configlist_reset(/* void */);

/********* From the file "error.h" ***************************************/
void ErrorMsg(const char *, int,const char *, ...);

/****** From the file "option.h" ******************************************/
struct s_options {
  enum { OPT_FLAG=1,  OPT_INT,  OPT_DBL,  OPT_STR,
         OPT_FFLAG, OPT_FINT, OPT_FDBL, OPT_FSTR} type;
  char *label;
  char *arg;
  char *message;
};
int    OptInit(/* char**,struct s_options*,FILE* */);
int    OptNArgs(/* void */);
char  *OptArg(/* int */);
void   OptErr(/* int */);
void   OptPrint(/* void */);

/******** From the file "parse.h" *****************************************/
void Parse(/* struct lemon *lemp */);

/********* From the file "plink.h" ***************************************/
struct plink *Plink_new(/* void */);
void Plink_add(/* struct plink **, struct config * */);
void Plink_copy(/* struct plink **, struct plink * */);
void Plink_delete(/* struct plink * */);

/********** From the file "report.h" *************************************/
void Reprint(/* struct lemon * */);
void ReportOutput(/* struct lemon * */);
void ReportTable(/* struct lemon * */);
void ReportHeader(/* struct lemon * */);
void CompressTables(/* struct lemon * */);

/********** From the file "set.h" ****************************************/
void  SetSize(/* int N */);             /* All sets will be of size N */
char *SetNew(/* void */);               /* A new set for element 0..N */
void  SetFree(/* char* */);             /* Deallocate a set */

int SetAdd(/* char*,int */);            /* Add element to a set */
int SetUnion(/* char *A,char *B */);    /* A <- A U B, thru element N */

#define SetFind(X,Y) (X[Y])       /* True if Y is in set X */

/********** From the file "struct.h" *************************************/
/*
** Principal data structures for the LEMON parser generator.
*/

typedef enum {B_FALSE=0, B_TRUE} Boolean;

/* Symbols (terminals and nonterminals) of the grammar are stored
** in the following: */
struct symbol { // symbol 结构体的作用是存储愈发文件中的终结符与非终结符,两者的相同特征太多,所以统一用symbol来标识
  char *name;              /* Name of the symbol */
  int index;               /* Index number for this symbol */
  enum {
    TERMINAL,
    NONTERMINAL
  } type;                  /* Symbols are all either TERMINALS or NTs */
  struct rule *rule;       /* Linked list of rules of this (if an NT) */
  struct symbol *fallback; /* fallback token in case this token doesn't parse */
  int prec;                /* Precedence if defined (-1 otherwise) */
  enum e_assoc {
    LEFT,
    RIGHT,
    NONE,
    UNK
  } assoc;                 /* Associativity if predecence is defined */
  char *firstset;          /* First-set for all rules of this symbol */
  Boolean lambda;          /* True if NT and can generate an empty string */
  char *destructor;        /* Code which executes whenever this symbol is
                           ** popped from the stack during error processing */
  int destructorln;        /* Line number of destructor code */
  char *datatype;          /* The data type of information held by this
                           ** object. Only used if type==NONTERMINAL */
  int dtnum;               /* The data type number.  In the parser, the value
                           ** stack is a union.  The .yy%d element of this
                           ** union is the correct data type for this object */
};

/* Each production rule in the grammar is stored in the following
** structure.  */
struct rule { // 产生式
  struct symbol *lhs;      /* Left-hand side of the rule */ // 它指向产生式定义符(::=)左边的那个符号,大家知道,左边符号肯定是一个非终结符
  char *lhsalias;          /* Alias for the LHS (NULL if none) */ // expr(A) ::= 表示别名,小括号里面的那个A
  int ruleline;            /* Line number for the rule */ // 本产生式 排序中的序号
  int nrhs;                /* Number of RHS symbols */ // 产生式右边所有符号的总数,既包括非终止符,也包括终止符
  struct symbol **rhs;     /* The RHS symbols */
  char **rhsalias;         /* An alias for each RHS symbol (NULL if none) */ // 右边的别名
  int line;                /* Line number at which code begins */
  char *code;              /* The code executed when this rule is reduced */ //规约时,执行的动作代码
  struct symbol *precsym;  /* Precedence symbol for this rule */
  int index;               /* An index number for this rule */
  Boolean canReduce;       /* True if this rule is ever reduced */
  struct rule *nextlhs;    /* Next rule with the same LHS */ //nextlhs 存放着 正是左边具有相同非终结符的产生式链条
  struct rule *next;       /* Next rule in the global list */
};

/* A configuration is a production rule of the grammar together with
** a mark (dot) showing how much of that rule has been processed so far.
** Configurations also contain a follow-set which is a list of terminal
** symbols which are allowed to immediately follow the end of the rule.
** Every configuration is recorded as an instance of the following: */
struct config { // config 用于存储项目, 项目是产生式的变形,在产生式右部的某处加点。(LEMON在.out文件中用*号代替了黑点)
  struct rule *rp;         /* The rule upon which the configuration is based */
  int dot;                 /* The parse point */
  char *fws;               /* Follow-set for this configuration only */
  struct plink *fplp;      /* Follow-set forward propagation links */
  struct plink *bplp;      /* Follow-set backwards propagation links */
  struct state *stp;       /* Pointer to state which contains this */
  enum {
    COMPLETE,              /* The status is used during followset and */
    INCOMPLETE             /*    shift computations */
  } status;
  struct config *next;     /* Next configuration in the state */
  struct config *bp;       /* The next basis configuration */
};

/* Every shift or reduce operation is stored as one of the following */
struct action { // 语法分析器中的有穷自动机
  struct symbol *sp;       /* The look-ahead symbol */
  enum e_action {
    SHIFT,
    ACCEPT,
    REDUCE,
    ERROR,
    CONFLICT,                /* Was a reduce, but part of a conflict */
    SH_RESOLVED,             /* Was a shift.  Precedence resolved conflict */
    RD_RESOLVED,             /* Was reduce.  Precedence resolved conflict */
    NOT_USED                 /* Deleted by compression */
  } type;
  union {
    struct state *stp;     /* The new state, if a shift */
    struct rule *rp;       /* The rule, if a reduce */
  } x; // 如果这个动作是移进,则联合取状态state。如果是归约,则联合取产生式rule
  struct action *next;     /* Next action for this state */
  struct action *collide;  /* Next action with the same hash */
};

/* Each state of the generated parser's finite state machine
** is encoded as an instance of the following structure. */
struct state {
  struct config *bp;       /* The basis configurations for this state */
  struct config *cfp;      /* All configurations in this set */
  int index;               /* Sequencial number for this state */
  struct action *ap;       /* Array of actions for this state */ //action 移进 规约 出错 接受
  int nTknAct, nNtAct;     /* Number of actions on terminals and nonterminals */
  int iTknOfst, iNtOfst;   /* yy_action[] offset for terminals and nonterms */
  int iDflt;               /* Default action */
};
#define NO_OFFSET (-2147483647)

/* A followset propagation link indicates that the contents of one
** configuration followset should be propagated to another whenever
** the first changes. */
struct plink { // 为了follow集而生
  struct config *cfp;      /* The configuration to which linked */
  struct plink *next;      /* The next propagate link */
};

/* The state vector for the entire parser generator is recorded as
** follows.  (LEMON uses no global variables and makes little use of
** static variables.  Fields in the following structure can be thought
** of as begin global variables in the program.) */
struct lemon {
  struct state **sorted;   /* 1  Table of states sorted by state number */
  struct rule *rule;       /* 2  List of all rules */
  int nstate;              /* 3  Number of states */
  int nrule;               /* 4  Number of rules */
  int nsymbol;             /* 5  Number of terminal and nonterminal symbols */
  int nterminal;           /* 6  Number of terminal symbols */
  struct symbol **symbols; /* 7  Sorted array of pointers to symbols */
  int errorcnt;            /* 8  Number of errors */
  struct symbol *errsym;   /* 9  The error symbol */
  char *name;              /* 10 Name of the generated parser */
  char *arg;               /* 11 Declaration of the 3th argument to parser */
  char *tokentype;         /* 12 Type of terminal symbols in the parser stack */
  char *vartype;           /* 13 The default type of non-terminal symbols */
  char *start;             /* 14 Name of the start symbol for the grammar */
  char *stacksize;         /* 15 Size of the parser stack */
  char *include;           /* 16 Code to put at the start of the C file */
  int  includeln;          /* 17 Line number for start of include code */
  char *error;             /* 18 Code to execute when an error is seen */
  int  errorln;            /* 19 Line number for start of error code */
  char *overflow;          /* 20 Code to execute on a stack overflow */
  int  overflowln;         /* 21 Line number for start of overflow code */
  char *failure;           /* 22 Code to execute on parser failure */
  int  failureln;          /* 23 Line number for start of failure code */
  char *accept;            /* 24 Code to execute when the parser excepts */
  int  acceptln;           /* 25 Line number for the start of accept code */
  char *extracode;         /* 26 Code appended to the generated file */
  int  extracodeln;        /* 27 Line number for the start of the extra code */
  char *tokendest;         /* 28 Code to execute to destroy token data */
  int  tokendestln;        /* 29 Line number for token destroyer code */
  char *vardest;           /* 30 Code for the default non-terminal destructor */
  int  vardestln;          /* 31 Line number for default non-term destructor code*/
  char *filename;          /* 32 Name of the input file */
  char *outname;           /* 33 Name of the current output file */
  char *tokenprefix;       /* 34 A prefix added to token names in the .h file */
  int nconflict;           /* 35 Number of parsing conflicts */
  int tablesize;           /* 36 Size of the parse tables */
  int basisflag;           /* 37 Print only basis configurations */
  int has_fallback;        /* 38 True if any %fallback is seen in the grammer */
  char *argv0;             /* 39 Name of the program */
};

#define MemoryCheck(X) if((X)==0){ \
  extern void memory_error(); \
  memory_error(); \
}

/**************** From the file "table.h" *********************************/
/*
** All code in this file has been automatically generated
** from a specification in the file
**              "table.q"
** by the associative array code building program "aagen".
** Do not edit this file!  Instead, edit the specification
** file, then rerun aagen.
*/
/*
** Code for processing tables in the LEMON parser generator.
*/

/* Routines for handling a strings */

char *Strsafe();

void Strsafe_init(/* void */);
int Strsafe_insert(/* char * */);
char *Strsafe_find(/* char * */);

/* Routines for handling symbols of the grammar */

struct symbol *Symbol_new();
int Symbolcmpp(/* struct symbol **, struct symbol ** */);
void Symbol_init(/* void */);
int Symbol_insert(/* struct symbol *, char * */);
struct symbol *Symbol_find(/* char * */);
struct symbol *Symbol_Nth(/* int */);
int Symbol_count(/*  */);
struct symbol **Symbol_arrayof(/*  */);

/* Routines to manage the state table */

int Configcmp(/* struct config *, struct config * */);
struct state *State_new();
void State_init(/* void */);
int State_insert(/* struct state *, struct config * */);
struct state *State_find(/* struct config * */);
struct state **State_arrayof(/*  */);

/* Routines used for efficiency in Configlist_add */

void Configtable_init(/* void */);
int Configtable_insert(/* struct config * */);
struct config *Configtable_find(/* struct config * */);
void Configtable_clear(/* int(*)(struct config *) */);
/****************** From the file "action.c" *******************************/
/*
** Routines processing parser actions in the LEMON parser generator.
*/

/* Allocate a new parser action */
struct action *Action_new(){
  static struct action *freelist = 0;
  struct action *new;

  if( freelist==0 ){
    int i;
    int amt = 100;
    freelist = (struct action *)malloc( sizeof(struct action)*amt );
    if( freelist==0 ){
      fprintf(stderr,"Unable to allocate memory for a new parser action.");
      exit(1);
    }
    for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
    freelist[amt-1].next = 0;
  }
  new = freelist;
  freelist = freelist->next;
  return new;
}

/* Compare two actions */
static int actioncmp(ap1,ap2)
struct action *ap1;
struct action *ap2;
{
  int rc;
  rc = ap1->sp->index - ap2->sp->index;
  if( rc==0 ) rc = (int)ap1->type - (int)ap2->type;
  if( rc==0 ){
    assert( ap1->type==REDUCE || ap1->type==RD_RESOLVED || ap1->type==CONFLICT);
    assert( ap2->type==REDUCE || ap2->type==RD_RESOLVED || ap2->type==CONFLICT);
    rc = ap1->x.rp->index - ap2->x.rp->index;
  }
  return rc;
}

/* Sort parser actions */
struct action *Action_sort(ap)
struct action *ap;
{
  ap = (struct action *)msort((char *)ap,(char **)&ap->next,actioncmp);
  return ap;
}

void Action_add(struct action **app,enum e_action type,struct symbol *sp,char *arg)
//struct action **app;
//enum e_action type;
//struct symbol *sp;
//char *arg;
{
  struct action *new;
  new = Action_new();
  new->next = *app;
  *app = new;
  new->type = type;
  new->sp = sp;
  if( type==SHIFT ){
    new->x.stp = (struct state *)arg;
  }else{
    new->x.rp = (struct rule *)arg;
  }
}
/********************** New code to implement the "acttab" module ***********/
/*
** This module implements routines use to construct the yy_action[] table.
*/

/*
** The state of the yy_action table under construction is an instance of
** the following structure
*/
typedef struct acttab acttab;
struct acttab {
  int nAction;                 /* Number of used slots in aAction[] */
  int nActionAlloc;            /* Slots allocated for aAction[] */
  struct {
    int lookahead;             /* Value of the lookahead token */
    int action;                /* Action to take on the given lookahead */
  } *aAction,                  /* The yy_action[] table under construction */
    *aLookahead;               /* A single new transaction set */
  int mnLookahead;             /* Minimum aLookahead[].lookahead */
  int mnAction;                /* Action associated with mnLookahead */
  int mxLookahead;             /* Maximum aLookahead[].lookahead */
  int nLookahead;              /* Used slots in aLookahead[] */
  int nLookaheadAlloc;         /* Slots allocated in aLookahead[] */
};

/* Return the number of entries in the yy_action table */
#define acttab_size(X) ((X)->nAction)

/* The value for the N-th entry in yy_action */
#define acttab_yyaction(X,N)  ((X)->aAction[N].action)

/* The value for the N-th entry in yy_lookahead */
#define acttab_yylookahead(X,N)  ((X)->aAction[N].lookahead)

/* Free all memory associated with the given acttab */
void acttab_free(acttab *p){
  free( p->aAction );
  free( p->aLookahead );
  free( p );
}

/* Allocate a new acttab structure */
acttab *acttab_alloc(void){
  acttab *p = malloc( sizeof(*p) );
  if( p==0 ){
    fprintf(stderr,"Unable to allocate memory for a new acttab.");
    exit(1);
  }
  memset(p, 0, sizeof(*p));
  return p;
}

/* Add a new action to the current transaction set
*/
void acttab_action(acttab *p, int lookahead, int action){
  if( p->nLookahead>=p->nLookaheadAlloc ){
    p->nLookaheadAlloc += 25;
    p->aLookahead = realloc( p->aLookahead,
                             sizeof(p->aLookahead[0])*p->nLookaheadAlloc );
    if( p->aLookahead==0 ){
      fprintf(stderr,"malloc failed\n");
      exit(1);
    }
  }
  if( p->nLookahead==0 ){
    p->mxLookahead = lookahead;
    p->mnLookahead = lookahead;
    p->mnAction = action;
  }else{
    if( p->mxLookahead<lookahead ) p->mxLookahead = lookahead;
    if( p->mnLookahead>lookahead ){
      p->mnLookahead = lookahead;
      p->mnAction = action;
    }
  }
  p->aLookahead[p->nLookahead].lookahead = lookahead;
  p->aLookahead[p->nLookahead].action = action;
  p->nLookahead++;
}

/*
** Add the transaction set built up with prior calls to acttab_action()
** into the current action table.  Then reset the transaction set back
** to an empty set in preparation for a new round of acttab_action() calls.
**
** Return the offset into the action table of the new transaction.
*/
int acttab_insert(acttab *p){
  int i, j, k, n;
  assert( p->nLookahead>0 );

  /* Make sure we have enough space to hold the expanded action table
  ** in the worst case.  The worst case occurs if the transaction set
  ** must be appended to the current action table
  */
  n = p->mxLookahead + 1;
  if( p->nAction + n >= p->nActionAlloc ){
    int oldAlloc = p->nActionAlloc;
    p->nActionAlloc = p->nAction + n + p->nActionAlloc + 20;
    p->aAction = realloc( p->aAction,
                          sizeof(p->aAction[0])*p->nActionAlloc);
    if( p->aAction==0 ){
      fprintf(stderr,"malloc failed\n");
      exit(1);
    }
    for(i=oldAlloc; i<p->nActionAlloc; i++){
      p->aAction[i].lookahead = -1;
      p->aAction[i].action = -1;
    }
  }

  /* Scan the existing action table looking for an offset where we can
  ** insert the current transaction set.  Fall out of the loop when that
  ** offset is found.  In the worst case, we fall out of the loop when
  ** i reaches p->nAction, which means we append the new transaction set.
  **
  ** i is the index in p->aAction[] where p->mnLookahead is inserted.
  */
  for(i=0; i<p->nAction+p->mnLookahead; i++){
    if( p->aAction[i].lookahead<0 ){
      for(j=0; j<p->nLookahead; j++){
        k = p->aLookahead[j].lookahead - p->mnLookahead + i;
        if( k<0 ) break;
        if( p->aAction[k].lookahead>=0 ) break;
      }
      if( j<p->nLookahead ) continue;
      for(j=0; j<p->nAction; j++){
        if( p->aAction[j].lookahead==j+p->mnLookahead-i ) break;
      }
      if( j==p->nAction ){
        break;  /* Fits in empty slots */
      }
    }else if( p->aAction[i].lookahead==p->mnLookahead ){
      if( p->aAction[i].action!=p->mnAction ) continue;
      for(j=0; j<p->nLookahead; j++){
        k = p->aLookahead[j].lookahead - p->mnLookahead + i;
        if( k<0 || k>=p->nAction ) break;
        if( p->aLookahead[j].lookahead!=p->aAction[k].lookahead ) break;
        if( p->aLookahead[j].action!=p->aAction[k].action ) break;
      }
      if( j<p->nLookahead ) continue;
      n = 0;
      for(j=0; j<p->nAction; j++){
        if( p->aAction[j].lookahead<0 ) continue;
        if( p->aAction[j].lookahead==j+p->mnLookahead-i ) n++;
      }
      if( n==p->nLookahead ){
        break;  /* Same as a prior transaction set */
      }
    }
  }
  /* Insert transaction set at index i. */
  for(j=0; j<p->nLookahead; j++){
    k = p->aLookahead[j].lookahead - p->mnLookahead + i;
    p->aAction[k] = p->aLookahead[j];
    if( k>=p->nAction ) p->nAction = k+1;
  }
  p->nLookahead = 0;

  /* Return the offset that is added to the lookahead in order to get the
  ** index into yy_action of the action */
  return i - p->mnLookahead;
}

/********************** From the file "assert.c" ****************************/
/*
** A more efficient way of handling assertions.
*/
void myassert(file,line)
char *file;
int line;
{
  fprintf(stderr,"Assertion failed on line %d of file \"%s\"\n",line,file);
  exit(1);
}
/********************** From the file "build.c" *****************************/
/*
** Routines to construction the finite state machine for the LEMON
** parser generator.
*/

/* Find a precedence symbol of every rule in the grammar.
** 
** Those rules which have a precedence symbol coded in the input
** grammar using the "[symbol]" construct will already have the
** rp->precsym field filled.  Other rules take as their precedence
** symbol the first RHS symbol with a defined precedence.  If there
** are not RHS symbols with a defined precedence, the precedence
** symbol field is left blank.
*/
void FindRulePrecedences(struct lemon *xp)
//struct lemon *xp;
{
  struct rule *rp;
  for (rp = xp->rule; rp; rp = rp->next) {
    if (rp->precsym == 0) { // 以"伪"终结符形式直接指定。 [xxx]符号的产生式,这些xxx是代表优先级的"伪"终结符号,存在precsym字段中
      int i;
      for (i = 0; i < rp->nrhs; i++) {
        if (rp->rhs[i]->prec >= 0) { // 由产生式右端的第一个具有优先级的文法符号来决定
          rp->precsym = rp->rhs[i]; // 有可能,探索完了右边的所有符号之后,却没有发现任一个符号具有优先级,那么precsym还是为空
          break;
        }
      }
    }
  }
  return;
}

/* Find all nonterminals which will generate the empty string.
** Then go back and compute the first sets of every nonterminal.
** The first set is the set of all terminal symbols which can begin
** a string generated by that nonterminal.
*/
void FindFirstSets(struct lemon *lemp) // 对于一个终结符X，它的First集合就是它自己,即First(X)={X}
//struct lemon *lemp;                 // 对于一个非终结符x则较为复杂。因为任何一个非终结符都有它自身的产生式。x:==x1x2x3```x4xn。就是说如果x1是一个终结符,则First(x)={x1}。如果x1不是一个终结符,那么First(x)={First(x1)}。不过可以肯定,最终非终结符总归用终结符来定义。可以用First(x)={Z}来标识,Z这里代表终结符
{                                      // 还有一种情况必须考虑,x1可能有多个产生式,此时,First(x)={Z1,Z2,Z3}。还有一种情况,可能X被视为空串,此时First(x)={ε} Epsilon艾普西隆
  int i;                               // 最后考虑最全面的一种情况:x:==x1x2x3```x4xn,中的每一个xi既有几个定义式,又可能是空串。则First(x)={First(x1)-{ε},First(x2)-{ε},```,} .但是从左到右的求取过程中,只要哪一个xi没有空串,则就在该First(xi)上终止求取First(x)的过程
  struct rule *rp;
  int progress;

  for(i=0; i<lemp->nsymbol; i++){ // nsymbol 是终结符跟非终结符的总个数
    lemp->symbols[i]->lambda = B_FALSE; // 所有的符号 的lambda 置false, True 则代表 NT and can generate an empty string,表示文法的右边可以是空串
  }
  for(i=lemp->nterminal; i<lemp->nsymbol; i++){ // 给所有的非终结符的first集 申请空间
    lemp->symbols[i]->firstset = SetNew(); // 还记得Symbolcmpp函数吗?小写都在后面,大写在前面,nterminal是终结符的个数,所以 symbols[nterminal]是第一个非终结符的位置
  } // setNew()是分配内存空间的函数,其中size=nterminal+1,最后一位当然是考虑拿来放'\0'切割符号的。

  /* First compute all lambdas */ // 此处拉姆达的意思就是可否为空串。。lambda为0,就是false,表示右边为空. 为1,就是true
  do {
    progress = 0; // 是否继续寻找空串的标志变量progress
    for (rp = lemp->rule; rp; rp = rp->next) {
      if (rp->lhs->lambda) continue; // 初始值都是 false
      for (i = 0; i < rp->nrhs; i++) { // nrhs表示产生式右边所有符号的总数,既包括非终止符,也包括终止符
        if (rp->rhs[i]->lambda == B_FALSE) break;
      }
      if (i == rp->nrhs) { // 当右边所有的符号(终结符的lambda肯定是false,主要是计算非终结符的lambda值)
        rp->lhs->lambda = B_TRUE;
        progress = 1; // 某个非空串变成了空串,需要重新 for循环算下所有的产生式rule
      }
    }
  } while (progress); // 该while循环是为每一个文法符号寻找它是否可为空串
  // 上面的do while循环,内部的for循环处理一条产生式的右边所有的文法符号,而外部的for循环则是对每一条产生式的左边符号逐一进行处理。两个for循环结束之后,当progress的值为1,说明处理过程中,至少有一个文法符号已从非空串变成了空串,我们还应该把所有的产生式再进行一遍类似的搜索。可能经过很多次扫描之后,progress才不会有机会成为1,此时意味着没有再发现任何一个文法符号可以改由空串构成,此时,while结束
  /* Now compute all first sets */
  do { // 上面知道了所有的非终结符可否为空串之后,才能进一步寻找First集合
    struct symbol *s1, *s2;
    progress = 0;
    for (rp = lemp->rule; rp; rp = rp->next) {
      s1 = rp->lhs;
      for (i = 0; i < rp->nrhs; i++) { // 进入右边文法符号 进行for循环
        s2 = rp->rhs[i];
        if (s2->type == TERMINAL) { // 遇到右边的终结符,不用说了,终结符就是s1的First结合中的元素
          progress += SetAdd(s1->firstset, s2->index); // /* s2->index代表Index number for this symbol */
          break; // 找到终止符 就可以 跳出当前循环了
        } else if (s1 == s2) { // 右边等于左边,允许递归
          if (s1->lambda == B_FALSE) break; // 这时候看左边符号lambda,如果为false,说明它不是一个空串,那么肯定说右边符号的firstset已经有元素了,而且左边等于右边,右边的firstset就是等于左边的first。 不必要再处理了,break跳出
        } else { // 当不是终结符也不是递归是非终结符自身的话没那就要把 右边符号s2的firstset集合中的元素加入到左边符号的firstset集合中去
          progress += SetUnion(s1->firstset, s2->firstset);
          if (s2->lambda == B_FALSE) break;// 如果右边的文法符号不可能是空串,说明左边文法符号的firstset已经加好了。 如果右边的该符号可能是空串,那么就需要继续考虑s2后面的各个其他符号的firstset的情况
        }
      }
    }
  } while (progress);
  return; // 总结:如果右边是终结符,就把终结符加到左边符号的firstset中去。如果是非终结符,就把此非终结符firstset集合中的所有元素 加到这边的符号的firstset中去
} // 运行完了之后,每一个非终结符的firstset集合中都有元素,并且firstset非空。只有一种特殊情况,它的firstset集合中可能包含空串,但是这个空串的表达方式不在firstset中,而是体现在lambda字段里面。lambda字段等于true,表示可为空。
// 如果一个产生式的转变富豪可以是空串的话,那么就是说,符号的右边的所有符号必然都可以是空串。
/* Compute all LR(0) states for the grammar.  Links
** are added to between some states so that the LR(1) follow sets
** can be computed later.
*/
PRIVATE struct state *getstate(/* struct lemon * */);  /* forward reference */
void FindStates(struct lemon *lemp) // 此函数分为三段。第一段是存放项目的项目表x4a的初始化。第二段是找到正确的开始符号。第三段最关键,就是计算得到第一状态state0的基本项目
//struct lemon *lemp;
{
  struct symbol *sp; // start position的意思
  struct rule *rp;

  Configlist_init(); //  FindStates第一段代码:初始化config 的list..config就是项目

  /* Find the start symbol */
  if (lemp->start) { // 用%start_symbol 来声明的
    sp = Symbol_find(lemp->start); //  不能保证%start_symbol 声明的符号是 符合"位于声明式左边的" 这一原则,所以得 检验下
    if (sp == 0) { // 从符号表里面 找不到数据, 说明%start_symbol声明的符号就不是终结符,也不是非终结符。
      ErrorMsg(lemp->filename, 0,
               "The specified start symbol \"%s\" is not \
                in a nonterminal of the grammar.  \"%s\" will be used as the start \
                symbol instead.", lemp->start, lemp->rule->lhs->name);
      lemp->errorcnt++;
      sp = lemp->rule->lhs; // 如果出错,那么就把第一条产生式的左边的那个符号当成 开始符号
    }
  } else {
    sp = lemp->rule->lhs;
  }

  /* Make sure the start symbol doesn't occur on the right-hand side of
  ** any rule.  Report an error if it does.  (YACC would generate a new
  ** start symbol in this case.) */
  for(rp=lemp->rule; rp; rp=rp->next){ // 检验下,保证开始符号出现在左边,并且永远不出现在右边。这是LR(0)扩展文法中必须要求的。 著名的YACC在这点的处理上要更高明一点,当遇到这种情况,它会自动生成一个start符号
    int i;
    for(i=0; i<rp->nrhs; i++){
      if( rp->rhs[i]==sp ){   // sp指向左边的符号。。
        ErrorMsg(lemp->filename,0,
"The start symbol \"%s\" occurs on the \
right-hand side of a rule. This will result in a parser which \
does not work properly.",sp->name);
        lemp->errorcnt++; // 要明确一点,程序不会由于这个错误而马上终止,LEMON还是会沿用错误的符号,是继续之后的所有步骤的操作
      }
    }
  } // FindStates第二段代码结束,下面是FindStates函数第三段代码的开始
 // TODO 最难最难的代码开始了。先理解是什么基本项目【核心项目】,就是初始项项目!! 什么是非基本项目,非基本项目就是分割点在最左端的非初始项的项目【通过闭包计算出来的】
  /* The basis configuration set for the first state
  ** is all rules which have the start symbol as their
  ** left-hand side */ // 考虑到有些人把开始符号出现在多条产生式的左边
  for(rp=sp->rule; rp; rp=rp->nextlhs){  //nextlhs 存放着 正是左边具有相同非终结符的产生式链条
    struct config *newcfp;
    newcfp = Configlist_addbasis(rp,0); // Configlist_addbasis是拿来安装基本项目【或者称作核心项目】。。该函数有两个参数,一个是产生式,一个是一个整数。整数表示*符号位于产生式右边的下标值。
    SetAdd(newcfp->fws,0); // newcfp->fws follow集合。。把它的第0个元素置为1.。其实这个第0个元素就是$,表示栈底的开始符号。。。超级特殊的符号
  }
  // TODO 特别标注:在同一个状态下面,只有当基本项目的*号后面跟着非终结符,才有可能派生出非基本项目。而且这些派生的非基本项目都是*号开头。
  /* Compute the first state.  All other states will be
  ** computed automatically during the computation of the first one.
  ** The returned pointer to the first state is not used. */
  (void)getstate(lemp); // 计算其他状态,其他的所有状态,以及各个状态中的基本项目和通过闭包运算得出的非基本项目等,都由getstate()函数来获取。。。作者的注释,强调了automatically
  return; // 总结:Configlist_addbasis 用来存储开始符号【称为第一状态State0】的基本项目到basis链上,getstate用来存储其他状态的基本项目跟非基本项目到current链上
} // 捷径理解:每个状态都有基本项目【至少一个基本项目】。只有当某个状态的基本项目的分隔符【*或者.标示】后面跟着非终结符,这时候,某个状态会有非基本项目【而且这些非基本项目都是分隔符开头】。
// 基本项目basis configuration就是核心项目kernel configuration::包括初始项start ::= * a,还有所有分割点不在最左端的项目。。。那么非基本项目就好理解了,所有分割点在最左端的非初始项的项目
/* Return a pointer to a state which is described by the configuration
** list which has been built from calls to Configlist_add.
*/
PRIVATE void buildshifts(/* struct lemon *, struct state * */); /* Forwd ref */
PRIVATE struct state *getstate(struct lemon *lemp) // 除了第一个状态的基本项目【或者叫做核心项目】,其他状态,包括各个状态中的基本项目和通过闭包运算得到的非基本项目等,都另外由getstate()函数来获取
//struct lemon *lemp;
{// 补充一点知识, ./lemon -b examply4.y 生成的eample4.out 不含非基本项目。./lemon examply4.y 生成的eample4.out 含非基本项目。 非基本项目就是分割点在最左端的非初始项的项目
  struct config *cfp, *bp;
  struct state *stp;
  // 非常重要:因为在Configlist_init中,currentend二级指针指向了current的地址,所以修改了currentend指向的内容,相当于修改了current指向的值。
  /* Extract the sorted basis of the new state.  The basis was constructed
  ** by prior calls to "Configlist_addbasis()". */
  Configlist_sortbasis(); // 先对basis内存储链表的所有节点进行排序 操作之后,basis正引用着一条按产生式的序号和分割点的左右位置进行排序的全部项目集链表(current)
  bp = Configlist_basis(); // 执行完了之后,current中有链表,basis链表已空。 basis被转移到了bp上。

  /* Get a state with the same basis */
  stp = State_find(bp); // 根据config基本项目 链条 找对应的状态?。#TODO,太难理解 FIXME:现在理解了,用bp基本项目来作为一个状态state的key值
  if (stp) { // 该状态已经存在了。
    /* A state with the same basis already exists!  Copy all the follow-set
    ** propagation links from the state under construction into the
    ** preexisting state, then return a pointer to the preexisting state */
    struct config *x, *y; // x是新进的bp,指向bp。y是找到的bp,是旧的
    for (x = bp, y = stp->bp; x && y; x = x->bp, y = y->bp) { // bp就是同一个状态下的基本项目链表。当x y后面还有节点的的时候
      Plink_copy(&y->bplp, x->bplp); // x跟y 只有Follow集合不一样。 函数Plink_copy 参数,前面参数是二级指针to,后面参数是一级指针from。
      Plink_delete(x->fplp); // 通过Plink_copy 把新近的x的bplp 拷贝到旧的y的bplp上。
      x->fplp = x->bplp = 0; // 把 新近处理过程得到的正向传播链表fplp跟逆向传播链表bplp,全部删除
    } // 切记,由于x的bplp 还有用,现在被拷贝到了y上了【通过地址拷贝过去的】。。只能回收x的fplp。。
    cfp = Configlist_return();  // current就是一个 相同状态下的所有项目集合链表。。
    Configlist_eat(cfp); // 释放cfp的空间
  } else { // 说明此基本项目 链条 还没有进入 hash状态表
    /* This really is a new state.  Construct all the details */
    Configlist_closure(lemp);    /* Compute the configuration closure */ // 闭包运算
    Configlist_sort();           /* Sort the configuration closure */
    cfp = Configlist_return();   /* Get a pointer to the config list */ // 闭包计算过后,current就是一个 相同状态下的所有项目集合链表。所以用return把configlist输出来。
    stp = State_new();           /* A new state structure */
    MemoryCheck(stp);
    stp->bp = bp;                /* Remember the configuration basis */ // 切记,bp就是基本项目集合,一般state状态结构体中,用来存储的key就是bp基本项目的key。而不是所有的项目作为一个key
    stp->cfp = cfp;              /* Remember the configuration closure */ // cfp就是所有
    stp->index = lemp->nstate++; /* Every state gets a sequence number */ // state的index就是状态的下标
    stp->ap = 0;                 /* No actions, yet. */
    State_insert(stp, stp->bp);   /* Add to the state table */ // 用基本项目的key 作为stp状态链的上节点 的key
    buildshifts(lemp, stp);       /* Recursively compute successor states */ // getstate跟buildshifts 相互递归调用
  }
  return stp;
}

/* Construct all successor states to the given state.  A "successor"
** state is any state which can be reached by a shift action.
*/
PRIVATE void buildshifts(struct lemon *lemp,struct state *stp)
//struct lemon *lemp;
//struct state *stp;     /* The state from which successors are computed */
{
  struct config *cfp;  /* For looping thru the config closure of "stp" */
  struct config *bcfp; /* For the inner loop on config closure of "stp" */
  struct config *new;  /* */
  struct symbol *sp;   /* Symbol following the dot in configuration "cfp" */
  struct symbol *bsp;  /* Symbol following the dot in configuration "bcfp" */
  struct state *newstp; /* A pointer to a successor state */

  /* Each configuration becomes complete after it contibutes to a successor
  ** state.  Initially, all configurations are incomplete */
  for(cfp=stp->cfp; cfp; cfp=cfp->next) cfp->status = INCOMPLETE;

  /* Loop through all configurations of the state "stp" */
  for(cfp=stp->cfp; cfp; cfp=cfp->next){
    if( cfp->status==COMPLETE ) continue;    /* Already used by inner loop */
    if( cfp->dot>=cfp->rp->nrhs ) continue;  /* Can't shift this config */
    Configlist_reset();                      /* Reset the new config set */ // 说明这个项目没有经过处理,也没有到达最右端
    sp = cfp->rp->rhs[cfp->dot]; // sp当前分割点后面的符号            /* Symbol after the dot */

    /* For every configuration in the state "stp" which has the symbol "sp"
    ** following its dot, add the same configuration to the basis set under
    ** construction but with the dot shifted one symbol to the right. */
    for(bcfp=cfp; bcfp; bcfp=bcfp->next){
      if( bcfp->status==COMPLETE ) continue;    /* Already used */
      if( bcfp->dot>=bcfp->rp->nrhs ) continue; /* Can't shift this one */
      bsp = bcfp->rp->rhs[bcfp->dot];           /* Get symbol after dot */
      if( bsp!=sp ) continue;                   /* Must be same as for "cfp" */
      bcfp->status = COMPLETE;                  /* Mark this config as used */
      new = Configlist_addbasis(bcfp->rp,bcfp->dot+1);
      Plink_add(&new->bplp,bcfp);
    }

    /* Get a pointer to the state described by the basis configuration set
    ** constructed in the preceding loop */
    newstp = getstate(lemp);

    /* The state "newstp" is reached from the state "stp" by a shift action
    ** on the symbol "sp" */
    Action_add(&stp->ap,SHIFT,sp,(char *)newstp);
  }
}

/*
** Construct the propagation links
*/
void FindLinks(struct lemon *lemp)
//struct lemon *lemp;
{
  int i;
  struct config *cfp, *other;
  struct state *stp;
  struct plink *plp;

  /* Housekeeping detail:
  ** Add to every propagate link a pointer back to the state to
  ** which the link is attached. */
  for(i=0; i<lemp->nstate; i++){ // nstate: Number of states
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){
      cfp->stp = stp; // stp:Pointer to state which contains this
    }
  }

  /* Convert all backlinks into forward links.  Only the forward
  ** links are used in the follow-set computation. */
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    for(cfp=stp->cfp; cfp; cfp=cfp->next){
      for(plp=cfp->bplp; plp; plp=plp->next){
        other = plp->cfp;
        Plink_add(&other->fplp,cfp);
      }
    }
  }
}

/* Compute all followsets.
**
** A followset is the set of all symbols which can come immediately
** after a configuration.
*/
void FindFollowSets(struct lemon *lemp)
//struct lemon *lemp;
{
  int i;
  struct config *cfp;
  struct plink *plp;
  int progress;
  int change;

  for (i = 0; i < lemp->nstate; i++) {
    for (cfp = lemp->sorted[i]->cfp; cfp; cfp = cfp->next) {
      cfp->status = INCOMPLETE;
    }
  }

  do {
    progress = 0;
    for (i = 0; i < lemp->nstate; i++) {
      for (cfp = lemp->sorted[i]->cfp; cfp; cfp = cfp->next) {
        if (cfp->status == COMPLETE) continue;
        for (plp = cfp->fplp; plp; plp = plp->next) {
          change = SetUnion(plp->cfp->fws, cfp->fws);
          if (change) {
            plp->cfp->status = INCOMPLETE;
            progress = 1;
          }
        }
        cfp->status = COMPLETE;
      }
    }
  } while (progress);
}

static int resolve_conflict();

/* Compute the reduce actions, and resolve conflicts.
*/
void FindActions(struct lemon *lemp)
//struct lemon *lemp;
{
  int i, j;
  struct config *cfp;
  struct state *stp;
  struct symbol *sp;
  struct rule *rp;

  /* Add all of the reduce actions 
  ** A reduce action is added for each element of the followset of
  ** a configuration which has its dot at the extreme right.
  */
  for (i = 0; i < lemp->nstate; i++) {   /* Loop over all states */
    stp = lemp->sorted[i];
    for (cfp = stp->cfp; cfp; cfp = cfp->next) {  /* Loop over all configurations */
      if (cfp->rp->nrhs == cfp->dot) {        /* Is dot at extreme right? */
        for (j = 0; j < lemp->nterminal; j++) {
          if (SetFind(cfp->fws, j)) {
            /* Add a reduce action to the state "stp" which will reduce by the
            ** rule "cfp->rp" if the lookahead symbol is "lemp->symbols[j]" */
            Action_add(&stp->ap, REDUCE, lemp->symbols[j], (char *) cfp->rp);
          }
        }
      }
    }
  }

  /* Add the accepting token */
  if (lemp->start) {
    sp = Symbol_find(lemp->start);
    if (sp == 0) sp = lemp->rule->lhs;
  } else {
    sp = lemp->rule->lhs;
  }
  /* Add to the first state (which is always the starting state of the
  ** finite state machine) an action to ACCEPT if the lookahead is the
  ** start nonterminal.  */
  Action_add(&lemp->sorted[0]->ap, ACCEPT, sp, 0);

  /* Resolve conflicts */
  for (i = 0; i < lemp->nstate; i++) {
    struct action *ap, *nap;
    struct state *stp;
    stp = lemp->sorted[i];
    assert(stp->ap);
    stp->ap = Action_sort(stp->ap);
    for (ap = stp->ap; ap && ap->next; ap = ap->next) {
      for (nap = ap->next; nap && nap->sp == ap->sp; nap = nap->next) {
        /* The two actions "ap" and "nap" have the same lookahead.
        ** Figure out which one should be used */
        lemp->nconflict += resolve_conflict(ap, nap, lemp->errsym);
      }
    }
  }

  /* Report an error for each rule that can never be reduced. */
  for (rp = lemp->rule; rp; rp = rp->next) rp->canReduce = B_FALSE;
  for (i = 0; i < lemp->nstate; i++) {
    struct action *ap;
    for (ap = lemp->sorted[i]->ap; ap; ap = ap->next) {
      if (ap->type == REDUCE) ap->x.rp->canReduce = B_TRUE;
    }
  }
  for (rp = lemp->rule; rp; rp = rp->next) {
    if (rp->canReduce) continue;
    ErrorMsg(lemp->filename, rp->ruleline, "This rule can not be reduced.\n");
    lemp->errorcnt++;
  }
}

/* Resolve a conflict between the two given actions.  If the
** conflict can't be resolve, return non-zero.
**
** NO LONGER TRUE:
**   To resolve a conflict, first look to see if either action
**   is on an error rule.  In that case, take the action which
**   is not associated with the error rule.  If neither or both
**   actions are associated with an error rule, then try to
**   use precedence to resolve the conflict.
**
** If either action is a SHIFT, then it must be apx.  This
** function won't work if apx->type==REDUCE and apy->type==SHIFT.
*/
static int resolve_conflict(apx,apy,errsym)
struct action *apx;
struct action *apy;
struct symbol *errsym;   /* The error symbol (if defined.  NULL otherwise) */
{
  struct symbol *spx, *spy;
  int errcnt = 0;
  assert( apx->sp==apy->sp );  /* Otherwise there would be no conflict */
  if( apx->type==SHIFT && apy->type==REDUCE ){
    spx = apx->sp;
    spy = apy->x.rp->precsym;
    if( spy==0 || spx->prec<0 || spy->prec<0 ){
      /* Not enough precedence information. */
      apy->type = CONFLICT;
      errcnt++;
    }else if( spx->prec>spy->prec ){    /* Lower precedence wins */
      apy->type = RD_RESOLVED;
    }else if( spx->prec<spy->prec ){
      apx->type = SH_RESOLVED;
    }else if( spx->prec==spy->prec && spx->assoc==RIGHT ){ /* Use operator */
      apy->type = RD_RESOLVED;                             /* associativity */
    }else if( spx->prec==spy->prec && spx->assoc==LEFT ){  /* to break tie */
      apx->type = SH_RESOLVED;
    }else{
      assert( spx->prec==spy->prec && spx->assoc==NONE );
      apy->type = CONFLICT;
      errcnt++;
    }
  }else if( apx->type==REDUCE && apy->type==REDUCE ){
    spx = apx->x.rp->precsym;
    spy = apy->x.rp->precsym;
    if( spx==0 || spy==0 || spx->prec<0 ||
    spy->prec<0 || spx->prec==spy->prec ){
      apy->type = CONFLICT;
      errcnt++;
    }else if( spx->prec>spy->prec ){
      apy->type = RD_RESOLVED;
    }else if( spx->prec<spy->prec ){
      apx->type = RD_RESOLVED;
    }
  }else{
    assert( 
      apx->type==SH_RESOLVED ||
      apx->type==RD_RESOLVED ||
      apx->type==CONFLICT ||
      apy->type==SH_RESOLVED ||
      apy->type==RD_RESOLVED ||
      apy->type==CONFLICT
    );
    /* The REDUCE/SHIFT case cannot happen because SHIFTs come before
    ** REDUCEs on the list.  If we reach this point it must be because
    ** the parser conflict had already been resolved. */
  }
  return errcnt;
}
/********************* From the file "configlist.c" *************************/
/*
** Routines to processing a configuration list and building a state
** in the LEMON parser generator.
*/

static struct config *freelist = 0;      /* List of free configurations */
static struct config *current = 0;       /* Top of list of configurations */
static struct config **currentend = 0;   /* Last on list of configs */
static struct config *basis = 0;         /* Top of list of basis configs */
static struct config **basisend = 0;     /* End of list of basis configs */

/* Return a pointer to a new configuration */
PRIVATE struct config *newconfig(){ // 调用newconfig函数的结果,总会送出一个新的项目结构来。
  struct config *new;
  if( freelist==0 ){
    int i;
    int amt = 3; //  每次都搞出3个config空间出来存放信息
    freelist = (struct config *)malloc( sizeof(struct config)*amt );
    if( freelist==0 ){
      fprintf(stderr,"Unable to allocate memory for a new configuration.");
      exit(1);
    }
    for(i=0; i<amt-1; i++) freelist[i].next = &freelist[i+1];
    freelist[amt-1].next = 0;
  }
  new = freelist;
  freelist = freelist->next;
  return new;
}

/* The configuration "old" is no longer used */
PRIVATE void deleteconfig(struct config *old)
//struct config *old;
{
  old->next = freelist;
  freelist = old;
}

/* Initialized the configuration list builder */
void Configlist_init(){
  current = 0; // Top of list of configurations, 全局变量, 类型是config  安放正由闭包运算产生的项目
  currentend = &current;
  basis = 0; // 放置基本项目,或曰核心项目
  basisend = &basis;
  Configtable_init();
  return;
}

/* Initialized the configuration list builder */
void Configlist_reset(){
  current = 0;
  currentend = &current;
  basis = 0;
  basisend = &basis;
  Configtable_clear(0);
  return;
}

/* Add another configuration to the configuration list */
struct config *Configlist_add(struct rule *rp,int dot) // 这个函数跟Configlist_addbasis完全一样。但是,它加入的链条仅有current。而Configlist_addbasis加入的链条除了current还有basis
//struct rule *rp;    /* The rule */
//int dot;            /* Index into the RHS of the rule where the dot goes */
{
  struct config *cfp, model;

  assert( currentend!=0 );
  model.rp = rp;
  model.dot = dot;
  cfp = Configtable_find(&model);
  if( cfp==0 ){
    cfp = newconfig();
    cfp->rp = rp;
    cfp->dot = dot;
    cfp->fws = SetNew();
    cfp->stp = 0;
    cfp->fplp = cfp->bplp = 0;
    cfp->next = 0;
    cfp->bp = 0;
    *currentend = cfp;
    currentend = &cfp->next;
    Configtable_insert(cfp);
  }
  return cfp;
}

/* Add a basis configuration to the configuration list */
struct config *Configlist_addbasis(struct rule *rp,int dot) // Configtable_insert ==> x4a的结构体,装备config...rp表示那个产生式, dot 就是产生式的"腰斩状态"的两部分的斩点
//struct rule *rp;
//int dot;
{
  struct config *cfp, model;

  assert( basisend!=0 ); // 确保basisend不是空指针
  assert( currentend!=0 ); // // 确保currentend不是空指针
  model.rp = rp;
  model.dot = dot;
  cfp = Configtable_find(&model);
  if( cfp==0 ){ // 如果返回0,说明这个项目还没有被安装到项目的hash表x4a中
    cfp = newconfig(); // newconfig 使用了freelist临时变量,一次性申请3个config的空间。每当执行了newconfig()函数用掉一个空间,当三次都用掉了,再一次性申请三个config空间。
    cfp->rp = rp;
    cfp->dot = dot;
    cfp->fws = SetNew(); // fws 域,放置所有follow集元素的数组
    cfp->stp = 0;        // stp是指向项目所在状态state的指针 stp全写:state to where pointer
    cfp->fplp = cfp->bplp = 0; // fplp跟bplp是很重要的两个指针。TODO 后面讲
    cfp->next = 0;
    cfp->bp = 0;
    *currentend = cfp; // 把处理过的项目config装到currentend指针的尾部  【currentend存储所有处理过项目,basisend存储所有处理过的基本项目。】 由于currentend是二级指针,currentend=&current。所以*currentend就是修改current的指向。
    currentend = &cfp->next; // next指针表示 同一个状态中另一个项目的指针 原文解释:Next configuration in the state。。 上一句修改了current的指向,然后这句再修改currentend的指向。
    *basisend = cfp;
    basisend = &cfp->bp; // bp字段表示下一个基本项目bp指针。
    Configtable_insert(cfp);
  }
  return cfp;
}

/* Compute the closure of the configuration list */
void Configlist_closure(struct lemon *lemp) // 闭包运算:只要发现*号 后面是非终结符，就去把**非终结符出现在产生式左边**的那个产生式 拖进来。
//struct lemon *lemp;
{
  struct config *cfp, *newcfp;
  struct rule *rp, *newrp;
  struct symbol *sp, *xsp;
  int i, dot;

  assert(currentend != 0); // 要求不是空链条。currentend 不等于0,说明current不是一条空的链条
  for (cfp = current; cfp; cfp = cfp->next) { // next字段表示同个状态下不同项目
    rp = cfp->rp;
    dot = cfp->dot;
    if (dot >= rp->nrhs) continue; // nrhs  产生式右边所有符号的总数,既包括非终止符,也包括终止符... 如果已经到达产生式的最右端, 说明后面没有符号了。闭包也就没有任何项目可加了。
    sp = rp->rhs[dot]; // sp : start position pointer
    if (sp->type == NONTERMINAL) { // 只有非终结符才需要考虑闭包运算。。。。
      if (sp->rule == 0 && sp != lemp->errsym) {
        ErrorMsg(lemp->filename, rp->line, "Nonterminal \"%s\" has no rules.",
                 sp->name);
        lemp->errorcnt++; // 如果它是非终结符,而它又没有产生式(用sp->rule==0来判断),同时它还不是我们预先认定的错误符号。则有误。 【因为右边的非终结符号,起码得在产生式的左边出现一次。不然非终结符就没办法转成终结符。】
      }
      for (newrp = sp->rule; newrp; newrp = newrp->nextlhs) { //nextlhs 存放着 正是左边具有相同非终结符的产生式链条
        newcfp = Configlist_add(newrp, 0); // 由sp作为左边符号的每一个产生式,都可以建立一个分割符在0处的项目。这种项目成为 非基本项目、。。。。至此,我们为闭包增加了一条项目。。此处的0表示分隔符的位置
        for (i = dot + 1; i < rp->nrhs; i++) { // TODO 开搞follow集,每个项目,他后面都带了一个follow集合,字段是fws。 // nrhs产生式右边所有符号的总数,既包括非终止符,也包括终止符
          xsp = rp->rhs[i]; // 取分隔符后面的那个符号。 xsp的index就是 该符号在所有symbol符号数组里面的下标值
          if (xsp->type == TERMINAL) {
            SetAdd(newcfp->fws, xsp->index); // 第一种情况,终结符,直接放入follow集合。然后退出for。。 特别强调,char数组,其实就是一个最小整型的数组(一个字节)
            break;
          } else {
            SetUnion(newcfp->fws, xsp->firstset); // 第二种情况,非终结符, 把当前符号xsp的First集合中的所有元素合并到 newcfp的Follow集。
            if (xsp->lambda == B_FALSE) break; // 假如lambda为false,那么当前符号xsp不可能是空串。它必定有产生式。所以当执行完前面的那个setUnion操作之后,就可跳出循环了,xsp之后的符号不用再考虑了。
          }
        }
        if (i == rp->nrhs) Plink_add(&cfp->fplp, newcfp); // 如果分割点已经位于最右边符号之后了,只需要把新生成的newcfp项目加挂到正在分析的原来cfp项目的fplp链表中
      } // 为什么这么做呢?**FOLLOW****集的计算方法：**1. 对于方法的开始符S，置#于FOLLOW(S)中；2. 若A->αBβ是一个产生式，则把FIRST(β)除去{ε}加至FOLLOW(B)中；3. 若A→αB是一个产生式，则把FOLLOW(A)加至FOLLOW(B)中。
    }   //  Plink_add就是规则3... 具体看代码分析  fplp含义forward propagation links 即项目的顺向传播链表
  }
  return;
}

/* Sort the configuration list */
void Configlist_sort(){
  current = (struct config *)msort((char *)current,(char **)&(current->next),Configcmp);
  currentend = 0;
  return;
}

/* Sort the basis configuration list */
void Configlist_sortbasis(){ // current的bp存储的是所有的基本项目。。TODO 作者使用了的msort使用了归并排序,这里就不多说了。
  basis = (struct config *)msort((char *)current,(char **)&(current->bp),Configcmp);
  basisend = 0;
  return;
}

/* Return a pointer to the head of the configuration list and
** reset the list */
struct config *Configlist_return(){
  struct config *old;
  old = current;
  current = 0;
  currentend = 0;
  return old;
}

/* Return a pointer to the head of the configuration list and
** reset the list */
struct config *Configlist_basis(){
  struct config *old;
  old = basis;
  basis = 0;
  basisend = 0;
  return old;
}

/* Free all elements of the given configuration list */
void Configlist_eat(struct config *cfp)
//struct config *cfp;
{
  struct config *nextcfp;
  for(; cfp; cfp=nextcfp){
    nextcfp = cfp->next;
    assert( cfp->fplp==0 );
    assert( cfp->bplp==0 );
    if( cfp->fws ) SetFree(cfp->fws);
    deleteconfig(cfp);
  }
  return;
}
/***************** From the file "error.c" *********************************/
/*
** Code for printing error message.
*/

/* Find a good place to break "msg" so that its length is at least "min"
** but no more than "max".  Make the point as close to max as possible.
*/
static int findbreak(msg,min,max)
char *msg;
int min;
int max;
{
  int i,spot;
  char c;
  for(i=spot=min; i<=max; i++){
    c = msg[i];
    if( c=='\t' ) msg[i] = ' ';
    if( c=='\n' ){ msg[i] = ' '; spot = i; break; }
    if( c==0 ){ spot = i; break; }
    if( c=='-' && i<max-1 ) spot = i+1;
    if( c==' ' ) spot = i;
  }
  return spot;
}

/*
** The error message is split across multiple lines if necessary.  The
** splits occur at a space, if there is a space available near the end
** of the line.
*/
#define ERRMSGSIZE  10000 /* Hope this is big enough.  No way to error check */ // 出错字符串的最大字节数
#define LINEWIDTH      79 /* Max width of any output line */ // 输出到屏幕上一行内容的最大宽度
#define PREFIXLIMIT    30 /* Max width of the prefix on each line */ // 定义在屏幕上输出一行内容时候,可加的前缀字符数量
void ErrorMsg(const char *filename, int lineno, const char *format, ...){ // format 是参数格式
  char errmsg[ERRMSGSIZE];
  char prefix[PREFIXLIMIT+10];
  int errmsgsize;
  int prefixsize;
  int availablewidth;
  va_list ap; // variable list可变化的列表,处理一个va_list 需要va_start va_arg(可选) va_end vsprintf四个函数一起用
  int end, restart, base;

  va_start(ap, format);
  /* Prepare a prefix to be prepended to every output line */
  if( lineno>0 ){ // 如果lineno大于0,说明已经开始处理y文件的实质内容了,这时候y文件出错的行号也要打印出来。
    sprintf(prefix,"%.*s:%d: ",PREFIXLIMIT-10,filename,lineno);
  }else{ // 当lineno等于0的时候,说明还没搞到y文件,这是只打印文件名称即可,不用打印文件出错的行号
    sprintf(prefix,"%.*s: ",PREFIXLIMIT-10,filename);
  }
  prefixsize = strlen(prefix);
  availablewidth = LINEWIDTH - prefixsize; // 最大宽度-减去-可用前缀的最大宽度

  /* Generate the error message */
  vsprintf(errmsg,format,ap); // int vsprintf(char *string, char *format, va_list param);//将param 按格式format写入字符串string中
  va_end(ap);
  errmsgsize = strlen(errmsg);
  /* Remove trailing '\n's from the error message. */
  while( errmsgsize>0 && errmsg[errmsgsize-1]=='\n' ){
     errmsg[--errmsgsize] = 0;
  }

  /* Print the error message */
  base = 0;
  while( errmsg[base]!=0 ){
    end = restart = findbreak(&errmsg[base],0,availablewidth); // findbreak既能让字符串的长度至少有大于min个字符的绝对不多于max个字符,并且让分行得到的字符串的长度尽量达到max
    restart += base;
    while( errmsg[restart]==' ' ) restart++;
    fprintf(stdout,"%s%.*s\n",prefix,end,&errmsg[base]);
    base = restart;
  }
}
/**************** From the file "main.c" ************************************/
/*
** Main program file for the LEMON parser generator.
*/

/* Report an out-of-memory condition and abort.  This function
** is used mostly by the "MemoryCheck" macro in struct.h
*/
void memory_error(){
  fprintf(stderr,"Out of memory.  Aborting...\n");
  exit(1);
}

static int nDefine = 0;      /* Number of -D options on the command line */
static char **azDefine = 0;  /* Name of the -D macros */

/* This routine is called with the argument to each -D command-line option.
** Add the macro defined to the azDefine array.
*/
static void handle_D_option(char *z){
  char **paz;
  nDefine++; // realloc先判断当前的指针是否有足够的连续空间,连续空间大小是newsize，如果有，扩大mem_address指向的地址，并且将mem_address返回，如果空间不够，先按照newsize指定的大小分配空间，将原有数据从头到尾拷贝到新分配的内存区域，而后释放原来mem_address所指内存区域（注意：原来指针是自动释放，不需要使用free），同时返回新分配的内存区域的首地址。即重新分配存储器块的地址。
  azDefine = realloc(azDefine, sizeof(azDefine[0])*nDefine); // realloc原型是extern void *realloc(void *mem_address, unsigned int newsize);
    if( azDefine==0 ){ // sizeof是计算对象所占的字节数, 因为static char **azDefine = 0; 所以sizeof(azDefine[0])=8,表示8个字节
    fprintf(stderr,"out of memory\n");
    exit(1);
  }
  paz = &azDefine[nDefine-1];
  *paz = malloc( strlen(z)+1 );
  if( *paz==0 ){
    fprintf(stderr,"out of memory\n");
    exit(1);
  }
  strcpy(*paz, z);
  for(z=*paz; *z && *z!='='; z++){}
  *z = 0; // 砍掉后面的第二个=, 比如-D=123=12 参数传进来,那么只取得123的值,第二个等号后面的东西忽略
}


/* The main program.  Parse the command line and do it... */
int main(int argc, char ** argv)
//int argc;
//char **argv;
{
  static int version = 0;
  static int rpflag = 0;
  static int basisflag = 0;
  static int compress = 0;
  static int quiet = 0;
  static int statistics = 0;
  static int mhflag = 0;
  static struct s_options options[] = {
    {OPT_FLAG, "b", (char*)&basisflag, "Print only the basis in report."},
    {OPT_FLAG, "c", (char*)&compress, "Don't compress the action table."},
    {OPT_FSTR, "D", (char*)handle_D_option, "Define an %ifdef macro."}, // handle_D_option 说明看书籍<LEMON语法分析生成器>第100页
    {OPT_FLAG, "g", (char*)&rpflag, "Print grammar without actions."},
    {OPT_FLAG, "m", (char*)&mhflag, "Output a makeheaders compatible file"},
    {OPT_FLAG, "q", (char*)&quiet, "(Quiet) Don't print the report file."},
    {OPT_FLAG, "s", (char*)&statistics,
                                   "Print parser stats to standard output."},
    {OPT_FLAG, "x", (char*)&version, "Print the version number."},
    {OPT_FLAG,0,0,0}
  };
  int i;
  struct lemon lem; // 这个变量 总揽全局。

  OptInit(argv,options,stderr);
  if( version ){
     printf("Lemon version 1.0\n");
     exit(0); 
  }
  if( OptNArgs()!=1 ){
    fprintf(stderr,"Exactly one filename argument is required.\n");
    exit(1);
  }
  lem.errorcnt = 0; // 如果走到这一步,那么什么错误都没有,errorcnt置为0,可以开干了

  /* Initialize the machine */
  Strsafe_init();
  Symbol_init();
  State_init();
  lem.argv0 = argv[0];
  lem.filename = OptArg(0);
  lem.basisflag = basisflag;
  lem.has_fallback = 0;
  lem.nconflict = 0;
  lem.name = lem.include = lem.arg = lem.tokentype = lem.start = 0;
  lem.vartype = 0;
  lem.stacksize = 0;
  lem.error = lem.overflow = lem.failure = lem.accept = lem.tokendest =
     lem.tokenprefix = lem.outname = lem.extracode = 0;
  lem.vardest = 0;
  lem.tablesize = 0;
  Symbol_new("$");
  lem.errsym = Symbol_new("error");

  /* Parse the input file */
  Parse(&lem); // ※※※LEMON作者说了很多遍,很多遍了,是这个名字虽然叫做parse,但其实他不是语法分析器parser,他是一个词法分析器lexer。。※※※
  if( lem.errorcnt ) exit(lem.errorcnt);
  if( lem.rule==0 ){
    fprintf(stderr,"Empty grammar.\n");
    exit(1);
  }

  /* Count and index the symbols of the grammar */ // 下面 是一个计算 x2a 终结符 非终结符 的个数。 值得看的是qsort算法
  lem.nsymbol = Symbol_count();
  Symbol_new("{default}");
  lem.symbols = Symbol_arrayof();
  for(i=0; i<=lem.nsymbol; i++) lem.symbols[i]->index = i;
  qsort(lem.symbols,lem.nsymbol+1,sizeof(struct symbol*),
        (int(*)())Symbolcmpp);
  for(i=0; i<=lem.nsymbol; i++) lem.symbols[i]->index = i;
  for(i=1; isupper(lem.symbols[i]->name[0]); i++);
  lem.nterminal = i; // 通过i递增,记录大写字母的符号在前面的总个数

  /* Generate a reprint of the grammar, if requested on the command line */
  if( rpflag ){ // 这个参数 是命令行传进来的,-g, 打印分析结果的统计情况
    Reprint(&lem);
  }else{ // TODO 重头戏来了。。。。first集->LR(0)->follow集->LALR(1)。。。看完前五章,懂了 lexer的实现,下面才是真正的parser的实现了。
    /* Initialize the size for all follow and first sets */
    SetSize(lem.nterminal); // 令全局变量size=nterminal+1 ::: nterminal就是终结符的个数

    /* Find the precedence for every production rule (that has one) */
    FindRulePrecedences(&lem); // 为各个产生式找到它可能具有的优先级

    /* Compute the lambda-nonterminals and the first-sets for every
    ** nonterminal */
    FindFirstSets(&lem); // 找出每一个非终结符的First集合,以及λ类型的非终结符
    // 第六章的first集合 寻找到此结束。。。!!!@@@###
    /* Compute all LR(0) states.  Also record follow-set propagation
    ** links so that the follow-set can be computed later */
    lem.nstate = 0; // nstate 状态个数。。此时 状态一个都没有
    FindStates(&lem); // TODO 最难最难的代码开始了。、、 寻找第一个状态State0的基本项目;;寻找LR(0)的所有状态;;闭包运算。第七章有详细介绍
    lem.sorted = State_arrayof(); // sorted字段解释:Table of states sorted by state number

    /* Tie up loose ends on the propagation links */
    FindLinks(&lem); // 寻找follow集合

    /* Compute the follow set of every reducible configuration */
    FindFollowSets(&lem); // 真正用于寻找Follow集的函数

    /* Compute the action tables */
    FindActions(&lem); // 装配动作链表

    /* Compress the action tables */
    if( compress==0 ) CompressTables(&lem); // 压缩动作链表

    /* Generate a report of the parser generated.  (the "y.output" file) */
    if( !quiet ) ReportOutput(&lem); // 报告动作链表

    /* Generate the source code for the parser */
    ReportTable(&lem, mhflag); // 如果参数有-m,那么mhflag就不会为0. 为1的话就会输出头文件

    /* Produce a header file for use by the scanner.  (This step is
    ** omitted if the "-m" option is used because makeheaders will
    ** generate the file for us.) */
    if( !mhflag ) ReportHeader(&lem);
  }
  if( statistics ){
    printf("Parser statistics: %d terminals, %d nonterminals, %d rules\n",
      lem.nterminal, lem.nsymbol - lem.nterminal, lem.nrule);
    printf("                   %d states, %d parser table entries, %d conflicts\n",
      lem.nstate, lem.tablesize, lem.nconflict);
  }
  if( lem.nconflict ){
    fprintf(stderr,"%d parsing conflicts.\n",lem.nconflict);
  }
  exit(lem.errorcnt + lem.nconflict);
  return (lem.errorcnt + lem.nconflict);
}
/******************** From the file "msort.c" *******************************/
/*
** A generic merge-sort program.
**
** USAGE:
** Let "ptr" be a pointer to some structure which is at the head of
** a null-terminated list.  Then to sort the list call:
**
**     ptr = msort(ptr,&(ptr->next),cmpfnc);
**
** In the above, "cmpfnc" is a pointer to a function which compares
** two instances of the structure and returns an integer, as in
** strcmp.  The second argument is a pointer to the pointer to the
** second element of the linked list.  This address is used to compute
** the offset to the "next" field within the structure.  The offset to
** the "next" field must be constant for all structures in the list.
**
** The function returns a new pointer which is the head of the list
** after sorting.
**
** ALGORITHM:
** Merge-sort.
*/

/*
** Return a pointer to the next structure in the linked list.
*/
#define NEXT(A) (*(char**)(((unsigned long)A)+offset))

/*
** Inputs:
**   a:       A sorted, null-terminated linked list.  (May be null).
**   b:       A sorted, null-terminated linked list.  (May be null).
**   cmp:     A pointer to the comparison function.
**   offset:  Offset in the structure to the "next" field.
**
** Return Value:
**   A pointer to the head of a sorted list containing the elements
**   of both a and b.
**
** Side effects:
**   The "next" pointers for elements in the lists a and b are
**   changed.
*/
static char *merge(a,b,cmp,offset) // 合并排序
char *a;
char *b;
int (*cmp)();
int offset;
{
  char *ptr, *head;

  if( a==0 ){
    head = b;
  }else if( b==0 ){
    head = a;
  }else{
    if( (*cmp)(a,b)<0 ){
      ptr = a;
      a = NEXT(a);
    }else{
      ptr = b;
      b = NEXT(b);
    }
    head = ptr;
    while( a && b ){
      if( (*cmp)(a,b)<0 ){
        NEXT(ptr) = a;
        ptr = a;
        a = NEXT(a);
      }else{
        NEXT(ptr) = b;
        ptr = b;
        b = NEXT(b);
      }
    }
    if( a ) NEXT(ptr) = a;
    else    NEXT(ptr) = b;
  }
  return head;
}

/*
** Inputs:
**   list:      Pointer to a singly-linked list of structures.
**   next:      Pointer to pointer to the second element of the list.
**   cmp:       A comparison function.
**
** Return Value:
**   A pointer to the head of a sorted list containing the elements
**   orginally in list.
**
** Side effects:
**   The "next" pointers for elements in list are changed.
*/
#define LISTSIZE 30
char *msort(char *list,char **next,int (*cmp)())
//char *list;
//char **next;
//int (*cmp)();
{
  unsigned long offset;
  char *ep;
  char *set[LISTSIZE];
  int i;
  offset = (unsigned long)next - (unsigned long)list;
  for(i=0; i<LISTSIZE; i++) set[i] = 0;
  while( list ){
    ep = list;
    list = NEXT(list); // 宏定义 #define NEXT(A) (*(char**)(((unsigned long)A)+offset))
    NEXT(ep) = 0;
    for(i=0; i<LISTSIZE-1 && set[i]!=0; i++){
      ep = merge(ep,set[i],cmp,offset);
      set[i] = 0;
    }
    set[i] = ep;
  }
  ep = 0;
  for(i=0; i<LISTSIZE; i++) if( set[i] ) ep = merge(ep,set[i],cmp,offset);
  return ep;
}
/************************ From the file "option.c" **************************/
static char **argv;
static struct s_options *op;
static FILE *errstream;

#define ISOPT(X) ((X)[0]=='-'||(X)[0]=='+'||strchr((X),'=')!=0)

/*
** Print the command line with a carrot pointing to the k-th character
** of the n-th field.
*/
static void errline(n,k,err) // n是那些带着"+"或者"-"开头的参数的位置
int n;
int k;
FILE *err;
{
  int spcnt, i;
  spcnt = 0;
  if( argv[0] ) fprintf(err,"%s",argv[0]); // 打印第0个参数,就是程序的全名
  spcnt = strlen(argv[0]) + 1;
  for(i=1; i<n && argv[i]; i++){
    fprintf(err," %s",argv[i]);
    spcnt += strlen(argv[i])+1; // 原来的代码是spcnt += strlen(argv[i]+1); 这明显是一个bug. 在3.2.2 中已经得到修复了。本代码是3.1.2,所以还是一个bug
  }
  spcnt += k;
  for(; argv[i]; i++) fprintf(err," %s",argv[i]);
  if( spcnt<20 ){
    fprintf(err,"\n%*s^-- here\n",spcnt,"");
  }else{
    fprintf(err,"\n%*shere --^\n",spcnt-7,"");
  }
}

/*
** Return the index of the N-th non-switch argument.  Return -1
** if N is out of range.
*/
static int argindex(n) // 很简单。就是拿到非opt参数组的第n个值,在这程序里,只有非opt组只能是y文件的名字,所以参数n只能取值0。n不是0的话,返回都会是-1
int n;
{
  int i;
  int dashdash = 0;
  if( argv!=0 && *argv!=0 ){
    for(i=1; argv[i]; i++){ // argv[0] 是程序的全名,要避开
      if( dashdash || !ISOPT(argv[i]) ){
        if( n==0 ) return i; // 当n=0,表示要取得语法.y文件的全路径名称
        n--;
      }
      if( strcmp(argv[i],"--")==0 ) dashdash = 1; // 这是废话、。不可能执行到这里
    }
  }
  return -1;
}

static char emsg[] = "Command line syntax error: ";

/*
** Process a flag command line argument.
*/
static int handleflags(i,err) // i是那些带着"+"或者"-"开头的参数的位置
int i;
FILE *err;
{
  int v;
  int errcnt = 0;
  int j;
  for(j=0; op[j].label; j++){
    if( strncmp(&argv[i][1],op[j].label,strlen(op[j].label))==0 ) break; // strncmp(const char * str1, const char * str2, size_t n)函数,若str1与str2的前n个字符相同，则返回0；若s1大于s2，则返回大于0的值；若s1 若小于s2，则返回小于0的值。
  }
  v = argv[i][0]=='-' ? 1 : 0;
  if( op[j].label==0 ){ // label 是第1个属性[下标0开始],那些b c g m
    if( err ){ // 如果一直找不到对应的属性[b c g m q s x], 那么最后的op[j]就是 {OPT_FLAG,0,0,0}, 那么就报错了
      fprintf(err,"%sundefined option.\n",emsg);
      errline(i,1,err);
    }
    errcnt++;
  }else if( op[j].type==OPT_FLAG ){ //  OPT_FLAG 用到, '-'开头就是1,表示打开。'+'开头就是0,表示关闭
    *((int*)op[j].arg) = v; // arg是s_options结构体的第2个属性[下标0开始],代表值
  }else if( op[j].type==OPT_FFLAG ){ // OPT_FFLAG 也没用到,可以忽略
    (*(void(*)())(op[j].arg))(v);
  }else if( op[j].type==OPT_FSTR ){ // TODO 主要是D参数。。。后面再讨论.OPT_FSTR类型的s_options结构体的arg是一个函数指针
    (*(void(*)())(op[j].arg))(&argv[i][2]); // -D123,因为前面一定是"-D",所以用argv[i][0]是'-',argv[i][1]是'D',那么&argv[i][2]就是123的开始位置了
  }else{ // 目前的代码逻辑,应该走不到这一步
    if( err ){
      fprintf(err,"%smissing argument on switch.\n",emsg);
      errline(i,1,err);
    }
    errcnt++;
  }
  return errcnt;
}

/*
** Process a command line switch which has an argument.
*/
static int handleswitch(i,err)
int i;
FILE *err;
{
  int lv = 0;
  double dv = 0.0;
  char *sv = 0, *end;
  char *cp;
  int j;
  int errcnt = 0;
  cp = strchr(argv[i],'=');
  *cp = 0; // 由于把'=' 改成 '\0',这样可以 使用strcmp 直接对比 等号前面的key 字符串
  for(j=0; op[j].label; j++){
    if( strcmp(argv[i],op[j].label)==0 ) break;
  }
  *cp = '='; // 比较完了之后,回复老样子
  if( op[j].label==0 ){
    if( err ){ // 逻辑同handleflags,如果一直找不到对应的属性[b c g m q s x], 那么最后的op[j]就是 {OPT_FLAG,0,0,0}, 那么就报错了
      fprintf(err,"%sundefined option.\n",emsg);
      errline(i,0,err);  // 为什么参数是0,而handleflags函数的errline参数是1. 因为这里的D参数前面没有中划线。而handleflags函数的-b -m -g 参数,前面有个中划线。所以报错指明的"^-- here"标志的时候有所不同
    }
    errcnt++;
  }else{
    cp++;
    switch( op[j].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        if( err ){ // flag参数,应该是-c这样,不要c=xxx这样
          fprintf(err,"%soption requires an argument.\n",emsg);
          errline(i,0,err);
        }
        errcnt++;
        break;
      case OPT_DBL:
      case OPT_FDBL: // 目前没用到,忽略
        dv = strtod(cp,&end);
        if( *end ){
          if( err ){
            fprintf(err,"%sillegal character in floating-point argument.\n",emsg);
            errline(i,((unsigned long)end)-(unsigned long)argv[i],err);
          }
          errcnt++;
        }
        break;
      case OPT_INT:
      case OPT_FINT: // 目前没用到,忽略
        lv = strtol(cp,&end,0);
        if( *end ){
          if( err ){
            fprintf(err,"%sillegal character in integer argument.\n",emsg);
            errline(i,((unsigned long)end)-(unsigned long)argv[i],err);
          }
          errcnt++;
        }
        break;
      case OPT_STR:
      case OPT_FSTR: // TODO 用到了,D参数,以后再看 。 cp指针指向 参数等号 后面的一个字符,比如./lemon D=123,那么cp就指向1的位置
        sv = cp;
        break;
    }
    switch( op[j].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        break;
      case OPT_DBL: // 忽略,目前sqlite版本暂时没用到
        *(double*)(op[j].arg) = dv;
        break;
      case OPT_FDBL: // 忽略,目前sqlite版本暂时没用到
        (*(void(*)())(op[j].arg))(dv);
        break;
      case OPT_INT: // 忽略,目前sqlite版本暂时没用到
        *(int*)(op[j].arg) = lv;
        break;
      case OPT_FINT: // 忽略,目前sqlite版本暂时没用到
        (*(void(*)())(op[j].arg))((int)lv);
        break;
      case OPT_STR: // TODO,用到了,D参数,以后再看
        *(char**)(op[j].arg) = sv;
        break;
      case OPT_FSTR: // TODO,用到了,D参数,以后再看 handle_D_option 方法
        (*(void(*)())(op[j].arg))(sv);
        break;
    }
  }
  return errcnt;
}
// c89支持参数类型写在括号的外面,c99也应该支持
int OptInit(a,o,err)
char **a;
struct s_options *o;
FILE *err;
{
  int errcnt = 0;
  argv = a;
  op = o;
  errstream = err;
  if( argv && *argv && op ){
    int i;
    for(i=1; argv[i]; i++){ // argc是命令行总的参数个数,argv[]是argc个参数,其中第0个参数是程序的全名。 所以argv[0]到argv[argc-1]有值,而argv[argc]是NULL
      if( argv[i][0]=='+' || argv[i][0]=='-' ){
        errcnt += handleflags(i,err);
      }else if( strchr(argv[i],'=') ){ // strchr 寻找等号的 指针位置,找不到就返回NULL,NULL在实际的底层代码中就是0。
          errcnt += handleswitch(i,err);
      }
    }
  }
  if( errcnt>0 ){
    fprintf(err,"Valid command line options for \"%s\" are:\n",*a);
    OptPrint();
    exit(1);
  }
  return 0;
}

int OptNArgs(){
  int cnt = 0;
  int dashdash = 0;
  int i;
  if( argv!=0 && argv[0]!=0 ){
    for(i=1; argv[i]; i++){ // gcc中,main函数进来的argv参数组,能保证最后一个NULL的地址一定是0x00
      if( dashdash || !ISOPT(argv[i]) ) cnt++; // 不是opt的话,就当成是传进来的file路径
      if( strcmp(argv[i],"--")==0 ) dashdash = 1; //strcmp 比较两个字符串..这里感觉没有毛用,因为'--'一定死在前面的OptInit,走不到这一步
    }
  }
  return cnt; // 只允许一个filename,所以这里的cnt不是1 ,就报错了
}

char *OptArg(n)
int n;
{
  int i;
  i = argindex(n);
  return i>=0 ? argv[i] : 0;
}

void OptErr(n) // 这个函数没用到过。。
int n;
{
  int i;
  i = argindex(n);
  if( i>=0 ) errline(i,0,errstream);
}

void OptPrint(){
  int i;
  int max, len;
  max = 0;
  for(i=0; op[i].label; i++){ // label 是第1个属性[下标0开始]
    len = strlen(op[i].label) + 1;
    switch( op[i].type ){ // type 是第0个属性
      case OPT_FLAG:
      case OPT_FFLAG:
        break;
      case OPT_INT:
      case OPT_FINT:
        len += 9;       /* length of "<integer>" */
        break;
      case OPT_DBL:
      case OPT_FDBL:
        len += 6;       /* length of "<real>" */
        break;
      case OPT_STR:
      case OPT_FSTR:
        len += 8;       /* length of "<string>" */
        break;
    }
    if( len>max ) max = len; // 得到长度最大的值,以方便打印整齐的提示信息
  }
  for(i=0; op[i].label; i++){
    switch( op[i].type ){
      case OPT_FLAG:
      case OPT_FFLAG:
        fprintf(errstream,"  -%-*s  %s\n",max,op[i].label,op[i].message);
        break;
      case OPT_INT:
      case OPT_FINT:
        fprintf(errstream,"  %s=<integer>%*s  %s\n",op[i].label,
          (int)(max-strlen(op[i].label)-9),"",op[i].message);
        break;
      case OPT_DBL:
      case OPT_FDBL:
        fprintf(errstream,"  %s=<real>%*s  %s\n",op[i].label,
          (int)(max-strlen(op[i].label)-6),"",op[i].message);
        break;
      case OPT_STR:
      case OPT_FSTR:
        fprintf(errstream,"  %s=<string>%*s  %s\n",op[i].label,
          (int)(max-strlen(op[i].label)-8),"",op[i].message);
        break;
    }
  }
}
/*********************** From the file "parse.c" ****************************/
/*
** Input file parser for the LEMON parser generator.
*/

/* The state of the parser */
struct pstate {
  char *filename;       /* Name of the input file */  // 语法文件的名字
  int tokenlineno;      /* Linenumber at which current token starts */ // 当前正在分析的符号的位置
  int errorcnt;         /* Number of errors so far */
  char *tokenstart;     /* Text of current token */
  struct lemon *gp;     /* Global state vector */  // 就是那个全局变量 lemon
  enum e_state {
    INITIALIZE,
    WAITING_FOR_DECL_OR_RULE,
    WAITING_FOR_DECL_KEYWORD,
    WAITING_FOR_DECL_ARG,
    WAITING_FOR_PRECEDENCE_SYMBOL,
    WAITING_FOR_ARROW,
    IN_RHS,
    LHS_ALIAS_1,
    LHS_ALIAS_2,
    LHS_ALIAS_3,
    RHS_ALIAS_1,
    RHS_ALIAS_2,
    PRECEDENCE_MARK_1,
    PRECEDENCE_MARK_2,
    RESYNC_AFTER_RULE_ERROR,
    RESYNC_AFTER_DECL_ERROR,
    WAITING_FOR_DESTRUCTOR_SYMBOL,
    WAITING_FOR_DATATYPE_SYMBOL,
    WAITING_FOR_FALLBACK_ID
  } state;                   /* The state of the parser */
  struct symbol *fallback;   /* The fallback token */
  struct symbol *lhs;        /* Left-hand side of current rule */ // 左边文法符号,是一个非终结符
  char *lhsalias;            /* Alias for the LHS */
  int nrhs;                  /* Number of right-hand side symbols seen */
  struct symbol *rhs[MAXRHS];  /* RHS symbols */ // 右边文法符号的数组,最大值 不能超过1000
  char *alias[MAXRHS];       /* Aliases for each RHS symbol (or NULL) */
  struct rule *prevrule;     /* Previous rule parsed */
  char *declkeyword;         /* Keyword of a declaration */
  char **declargslot;        /* Where the declaration argument should be put */
  int *decllnslot;           /* Where the declaration linenumber is put */
  enum e_assoc declassoc;    /* Assign this association to decl arguments */
  int preccounter;           /* Assign this precedence to decl arguments */
  struct rule *firstrule;    /* Pointer to first rule in the grammar */
  struct rule *lastrule;     /* Pointer to the most recently parsed rule */
};

/* Parse a single token */
static void parseonetoken(psp)
struct pstate *psp;
{
  char *x;
  x = Strsafe(psp->tokenstart);     /* Save the token permanently */ // x1a 主要是拿来存字符串, 一段一段存
#if 0
  printf("%s:%d: Token=[%s] state=%d\n",psp->filename,psp->tokenlineno,
    x,psp->state);
#endif
  switch( psp->state ){ // 一共有19个case
    case INITIALIZE:
      psp->prevrule = 0;
          psp->preccounter = 0;
          psp->firstrule = psp->lastrule = 0;
          psp->gp->nrule = 0;
          /* Fall thru to next case */ // 注意此处没有break,所以会往下走
    case WAITING_FOR_DECL_OR_RULE:
      if (x[0] == '%') {
        psp->state = WAITING_FOR_DECL_KEYWORD; // %开头的话,下一个状态就是描述的keyword
      } else if (islower(x[0])) { // lower的话就是 非终结符的开始
        psp->lhs = Symbol_new(x);
        psp->nrhs = 0;
        psp->lhsalias = 0;
        psp->state = WAITING_FOR_ARROW;
      } else if (x[0] == '{') {
        if (psp->prevrule == 0) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "There is not prior rule opon which to attach the code \
fragment which begins on this line.");
          psp->errorcnt++;
        } else if (psp->prevrule->code != 0) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "Code fragment beginning on this line is not the first \
to follow the previous rule.");
          psp->errorcnt++;
        } else {
          psp->prevrule->line = psp->tokenlineno;
          psp->prevrule->code = &x[1];
        }
      } else if (x[0] == '[') {
        psp->state = PRECEDENCE_MARK_1;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Token \"%s\" should be either \"%%\" or a nonterminal name.",
                 x);
        psp->errorcnt++;
      }
          break;
    case PRECEDENCE_MARK_1:
      if (!isupper(x[0])) {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "The precedence symbol must be a terminal.");
        psp->errorcnt++;
      } else if (psp->prevrule == 0) {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "There is no prior rule to assign precedence \"[%s]\".", x);
        psp->errorcnt++;
      } else if (psp->prevrule->precsym != 0) {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Precedence mark on this line is not the first \
to follow the previous rule.");
        psp->errorcnt++;
      } else {
        psp->prevrule->precsym = Symbol_new(x);
      }
          psp->state = PRECEDENCE_MARK_2;
          break;
    case PRECEDENCE_MARK_2:
      if (x[0] != ']') {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Missing \"]\" on precedence mark.");
        psp->errorcnt++;
      }
          psp->state = WAITING_FOR_DECL_OR_RULE;
          break;
    case WAITING_FOR_ARROW:
      if (x[0] == ':' && x[1] == ':' && x[2] == '=') {
        psp->state = IN_RHS;
      } else if (x[0] == '(') {
        psp->state = LHS_ALIAS_1;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Expected to see a \":\" following the LHS symbol \"%s\".",
                 psp->lhs->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case LHS_ALIAS_1:
      if (isalpha(x[0])) {
        psp->lhsalias = x;
        psp->state = LHS_ALIAS_2;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "\"%s\" is not a valid alias for the LHS \"%s\"\n",
                 x, psp->lhs->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case LHS_ALIAS_2:
      if (x[0] == ')') {
        psp->state = LHS_ALIAS_3;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Missing \")\" following LHS alias name \"%s\".", psp->lhsalias);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case LHS_ALIAS_3:
      if (x[0] == ':' && x[1] == ':' && x[2] == '=') {
        psp->state = IN_RHS;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Missing \"->\" following: \"%s(%s)\".",
                 psp->lhs->name, psp->lhsalias);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case IN_RHS:
      if (x[0] == '.') {
        struct rule *rp;
        rp = (struct rule *) malloc(sizeof(struct rule) +
                                    sizeof(struct symbol *) * psp->nrhs + sizeof(char *) * psp->nrhs);
        if (rp == 0) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "Can't allocate enough memory for this rule.");
          psp->errorcnt++;
          psp->prevrule = 0;
        } else {
          int i;
          rp->ruleline = psp->tokenlineno;
          rp->rhs = (struct symbol **) &rp[1];
          rp->rhsalias = (char **) &(rp->rhs[psp->nrhs]);
          for (i = 0; i < psp->nrhs; i++) {
            rp->rhs[i] = psp->rhs[i];
            rp->rhsalias[i] = psp->alias[i];
          }
          rp->lhs = psp->lhs;
          rp->lhsalias = psp->lhsalias;
          rp->nrhs = psp->nrhs;
          rp->code = 0;
          rp->precsym = 0;
          rp->index = psp->gp->nrule++;
          rp->nextlhs = rp->lhs->rule;
          rp->lhs->rule = rp;
          rp->next = 0;
          if (psp->firstrule == 0) {
            psp->firstrule = psp->lastrule = rp;
          } else {
            psp->lastrule->next = rp;
            psp->lastrule = rp;
          }
          psp->prevrule = rp;
        }
        psp->state = WAITING_FOR_DECL_OR_RULE;
      } else if (isalpha(x[0])) {
        if (psp->nrhs >= MAXRHS) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "Too many symbol on RHS or rule beginning at \"%s\".",
                   x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_RULE_ERROR;
        } else {
          psp->rhs[psp->nrhs] = Symbol_new(x);
          psp->alias[psp->nrhs] = 0;
          psp->nrhs++;
        }
      } else if (x[0] == '(' && psp->nrhs > 0) {
        psp->state = RHS_ALIAS_1;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Illegal character on RHS of rule: \"%s\".", x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case RHS_ALIAS_1:
      if (isalpha(x[0])) {
        psp->alias[psp->nrhs - 1] = x;
        psp->state = RHS_ALIAS_2;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "\"%s\" is not a valid alias for the RHS symbol \"%s\"\n",
                 x, psp->rhs[psp->nrhs - 1]->name);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case RHS_ALIAS_2:
      if (x[0] == ')') {
        psp->state = IN_RHS;
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Missing \")\" following LHS alias name \"%s\".", psp->lhsalias);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_RULE_ERROR;
      }
          break;
    case WAITING_FOR_DECL_KEYWORD:
      if (isalpha(x[0])) {
        psp->declkeyword = x;
        psp->declargslot = 0;
        psp->decllnslot = 0;
        psp->state = WAITING_FOR_DECL_ARG; // WAITING_FOR_DECL_KEYWORD的下一个状态就是 arg参数了
        if (strcmp(x, "name") == 0) {
          psp->declargslot = &(psp->gp->name);
        } else if (strcmp(x, "include") == 0) {
          psp->declargslot = &(psp->gp->include);
          psp->decllnslot = &psp->gp->includeln;
        } else if (strcmp(x, "code") == 0) {
          psp->declargslot = &(psp->gp->extracode);
          psp->decllnslot = &psp->gp->extracodeln;
        } else if (strcmp(x, "token_destructor") == 0) {
          psp->declargslot = &psp->gp->tokendest;
          psp->decllnslot = &psp->gp->tokendestln;
        } else if (strcmp(x, "default_destructor") == 0) {
          psp->declargslot = &psp->gp->vardest;
          psp->decllnslot = &psp->gp->vardestln;
        } else if (strcmp(x, "token_prefix") == 0) {
          psp->declargslot = &psp->gp->tokenprefix;
        } else if (strcmp(x, "syntax_error") == 0) {
          psp->declargslot = &(psp->gp->error);
          psp->decllnslot = &psp->gp->errorln;
        } else if (strcmp(x, "parse_accept") == 0) {
          psp->declargslot = &(psp->gp->accept);
          psp->decllnslot = &psp->gp->acceptln;
        } else if (strcmp(x, "parse_failure") == 0) {
          psp->declargslot = &(psp->gp->failure);
          psp->decllnslot = &psp->gp->failureln;
        } else if (strcmp(x, "stack_overflow") == 0) {
          psp->declargslot = &(psp->gp->overflow);
          psp->decllnslot = &psp->gp->overflowln;
        } else if (strcmp(x, "extra_argument") == 0) {
          psp->declargslot = &(psp->gp->arg);
        } else if (strcmp(x, "token_type") == 0) {
          psp->declargslot = &(psp->gp->tokentype);
        } else if (strcmp(x, "default_type") == 0) {
          psp->declargslot = &(psp->gp->vartype);
        } else if (strcmp(x, "stack_size") == 0) {
          psp->declargslot = &(psp->gp->stacksize);
        } else if (strcmp(x, "start_symbol") == 0) {
          psp->declargslot = &(psp->gp->start);
        } else if (strcmp(x, "left") == 0) {
          psp->preccounter++;
          psp->declassoc = LEFT;
          psp->state = WAITING_FOR_PRECEDENCE_SYMBOL; // 找到了left关键字,那么下一个状态就是想找优先符了。
        } else if (strcmp(x, "right") == 0) {
          psp->preccounter++;
          psp->declassoc = RIGHT;
          psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
        } else if (strcmp(x, "nonassoc") == 0) {
          psp->preccounter++;
          psp->declassoc = NONE;
          psp->state = WAITING_FOR_PRECEDENCE_SYMBOL;
        } else if (strcmp(x, "destructor") == 0) {
          psp->state = WAITING_FOR_DESTRUCTOR_SYMBOL;
        } else if (strcmp(x, "type") == 0) {
          psp->state = WAITING_FOR_DATATYPE_SYMBOL;
        } else if (strcmp(x, "fallback") == 0) {
          psp->fallback = 0;
          psp->state = WAITING_FOR_FALLBACK_ID;
        } else {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "Unknown declaration keyword: \"%%%s\".", x);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_DECL_ERROR;
        }
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Illegal declaration keyword: \"%s\".", x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
          break;
    case WAITING_FOR_DESTRUCTOR_SYMBOL:
      if (!isalpha(x[0])) {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Symbol name missing after %destructor keyword");
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      } else {
        struct symbol *sp = Symbol_new(x);
        psp->declargslot = &sp->destructor;
        psp->decllnslot = &sp->destructorln;
        psp->state = WAITING_FOR_DECL_ARG;
      }
          break;
    case WAITING_FOR_DATATYPE_SYMBOL:
      if (!isalpha(x[0])) {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Symbol name missing after %destructor keyword");
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      } else {
        struct symbol *sp = Symbol_new(x);
        psp->declargslot = &sp->datatype;
        psp->decllnslot = 0;
        psp->state = WAITING_FOR_DECL_ARG;
      }
          break;
    case WAITING_FOR_PRECEDENCE_SYMBOL:
      if (x[0] == '.') {
        psp->state = WAITING_FOR_DECL_OR_RULE; // 如果出现小黑点了,那么就代表结束了。下一个状态就是寻找 特殊声明符 或者 产生式了
      } else if (isupper(x[0])) {
        struct symbol *sp;
        sp = Symbol_new(x); // 大写的话就是终结符,开始写入 Symbol结构体,这个结构体的 全局变量就是x2a.. x2a 可以存放终结符或者非终结符。。此处没有出现小黑点,所以下个状态还是寻找优先符
        if (sp->prec >= 0) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "Symbol \"%s\" has already be given a precedence.", x);
          psp->errorcnt++;
        } else {
          sp->prec = psp->preccounter;
          sp->assoc = psp->declassoc;
        }
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Can't assign a precedence to \"%s\".", x);
        psp->errorcnt++;
      }
          break;
    case WAITING_FOR_DECL_ARG: // DECL 代表 特殊声明符
      if ((x[0] == '{' || x[0] == '\"' || isalnum(x[0]))) {
        if (*(psp->declargslot) != 0) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "The argument \"%s\" to declaration \"%%%s\" is not the first.",
                   x[0] == '\"' ? &x[1] : x, psp->declkeyword);
          psp->errorcnt++;
          psp->state = RESYNC_AFTER_DECL_ERROR;
        } else {
          *(psp->declargslot) = (x[0] == '\"' || x[0] == '{') ? &x[1] : x;
          if (psp->decllnslot) *psp->decllnslot = psp->tokenlineno;
          psp->state = WAITING_FOR_DECL_OR_RULE; // WAITING_FOR_DECL_ARG的参数拿完了之后,那下一个期待状态就是 特殊声明符或者产生式
        }
      } else {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "Illegal argument to %%%s: %s", psp->declkeyword, x);
        psp->errorcnt++;
        psp->state = RESYNC_AFTER_DECL_ERROR;
      }
          break;
    case WAITING_FOR_FALLBACK_ID:
      if (x[0] == '.') {
        psp->state = WAITING_FOR_DECL_OR_RULE;
      } else if (!isupper(x[0])) {
        ErrorMsg(psp->filename, psp->tokenlineno,
                 "%%fallback argument \"%s\" should be a token", x);
        psp->errorcnt++;
      } else {
        struct symbol *sp = Symbol_new(x);
        if (psp->fallback == 0) {
          psp->fallback = sp;
        } else if (sp->fallback) {
          ErrorMsg(psp->filename, psp->tokenlineno,
                   "More than one fallback assigned to token %s", x);
          psp->errorcnt++;
        } else {
          sp->fallback = psp->fallback;
          psp->gp->has_fallback = 1;
        }
      }
          break;
    case RESYNC_AFTER_RULE_ERROR:
/*      if( x[0]=='.' ) psp->state = WAITING_FOR_DECL_OR_RULE;
**      break; */
    case RESYNC_AFTER_DECL_ERROR:
      if (x[0] == '.') psp->state = WAITING_FOR_DECL_OR_RULE;
          if (x[0] == '%') psp->state = WAITING_FOR_DECL_KEYWORD;
          break;
  }
}

/* Run the proprocessor over the input file text.  The global variables
** azDefine[0] through azDefine[nDefine-1] contains the names of all defined
** macros.  This routine looks for "%ifdef" and "%ifndef" and "%endif" and
** comments them out.  Text in between is also commented out as appropriate.
*/
static void preprocess_input(char *z){ // 处理预编译 代码%ifdef-%endif
  int i, j, k, n;
  int exclude = 0;
  int start;
  int lineno = 1;
  int start_lineno;
  for(i=0; z[i]; i++){
    if( z[i]=='\n' ) lineno++; // 遇到换行,那就lineno加1
    if( z[i]!='%' || (i>0 && z[i-1]!='\n') ) continue; // (i>0 && z[i-1]!='\n') 表示的意思是:即使当前字符是%,那么要求%的前面是回车,不然忽略
    if( strncmp(&z[i],"%endif",6)==0 && isspace(z[i+6]) ){ // int strncmp ( const char * str1, const char * str2, size_t n );参数】str1, str2 为需要比较的两个字符串，n为要比较的字符的数目。
      if( exclude ){ // isspace 空格(' ')、水平定位字符('\t')、归位键('\r')、换行('\n')、垂直定位字符('\v')或翻页('\f'),说白了,%endif后面必须是空格字符
        exclude--;
        if( exclude==0 ){ // 如果置零了,说明 即使存在的%ifdef %ifdef-%endif %endif这种嵌套 也结束了。开始 开删了。。
          for(j=start; j<i; j++) if( z[j]!='\n' ) z[j] = ' ';
        }
      }
      for(j=i; z[j] && z[j]!='\n'; j++) z[j] = ' ';
    }else if( (strncmp(&z[i],"%ifdef",6)==0 && isspace(z[i+6]))
          || (strncmp(&z[i],"%ifndef",7)==0 && isspace(z[i+7])) ){
      if( exclude ){
        exclude++; // 排除个数+1
      }else{
        for(j=i+7; isspace(z[j]); j++){} // 跨越所有的空格 比如%ifdef      first这样,忽略中间所有的空格
        for(n=0; z[j+n] && !isspace(z[j+n]); n++){} // 跨越空格 之后的那个值的所有非空格 比如%ifdef      first这样, 得到first整个单词的长度值n
        exclude = 1;
        for(k=0; k<nDefine; k++){ // nDefine就是那个 从命令行参数 传进来的那些D参数的 总个数
          if( strncmp(azDefine[k],&z[j],n)==0 && strlen(azDefine[k])==n ){ // 如果这货 已经存在 D参数列表里面了的话,就跳出循环。 那么排除个数exclude就置为0
            exclude = 0;
            break;
          }
        }
        if( z[i+3]=='n' ) exclude = !exclude; //1.1 属于%ifndef的话, 并且D参数没有这货,那么 exclude 此时的值是0,表示不用排除这代码. 1.2 属于%ifndef的话, 并且D参数有这货,那么 exclude 此时的值是1,那得排除这段代码
        if( exclude ){ // 开始干掉代码了,设置 干掉代码的起点。。等下次 遇到%endif 就开始全部置空格
          start = i;
          start_lineno = lineno;
        }
      }
      for(j=i; z[j] && z[j]!='\n'; j++) z[j] = ' '; // %ifndef 或者 %ifdef 虽然后面是空格,但是在同一行中还有其他东西的东西,比如"%ifdef 123 234"之类的,把后面的"123 234"删除
    }
  }
  if( exclude ){ // 如果exclude 不为0, 说明%ifdef有头无尾
    fprintf(stderr,"unterminated %%ifdef starting on line %d\n", start_lineno);
    exit(1);
  }
}

/* In spite of its name, this function is really a scanner.  It read
** in the entire input file (all at once) then tokenizes it.  Each
** token is passed to the function "parseonetoken" which builds all
** the appropriate data structures in the global state vector "gp".
*/
void Parse(struct lemon *gp) // 理解成scanner 或者 lexer 。。。其实准确说,他不是一个Parser,这点要注意
//struct lemon *gp;
{
  struct pstate ps;
  FILE *fp;
  char *filebuf;
  int filesize;
  int lineno;
  int c;
  char *cp, *nextcp;
  int startline = 0;

  ps.gp = gp;
  ps.filename = gp->filename;
  ps.errorcnt = 0;
  ps.state = INITIALIZE;

  /* Begin by reading the input file */
  fp = fopen(ps.filename,"rb");
  if( fp==0 ){
    ErrorMsg(ps.filename,0,"Can't open this file for reading.");
    gp->errorcnt++;
    return;
  }
  fseek(fp,0,2); //指向文件尾巴。 int fseek(FILE *stream, long offset, int fromwhere); 如果执行成功，指针将指向以fromwhere【偏移起始位置：文件头0(SEEK_SET)，当前位置1(SEEK_CUR)，文件尾2(SEEK_END)】为基准，偏移offset（指针偏移量）个字节的位置。
  filesize = ftell(fp); // 函数 ftell 用于得到文件位置指针当前位置相对于文件首的偏移字节数
  rewind(fp); // 功能是将文件内部的指针重新指向一个流的开头,滚回头部
  filebuf = (char *)malloc( filesize+1 ); // 多申请一个字节,最后一个字节用来放 '\0'
  if( filebuf==0 ){
    ErrorMsg(ps.filename,0,"Can't allocate %d of memory to hold this file.",
      filesize+1);
    gp->errorcnt++;
    return;
  }
  if( fread(filebuf,1,filesize,fp)!=filesize ){ // 读取整个文件
    ErrorMsg(ps.filename,0,"Can't read in all %d bytes of this file.",
      filesize);
    free(filebuf);
    gp->errorcnt++;
    return;
  }
  fclose(fp);
  filebuf[filesize] = 0;

  /* Make an initial pass through the file to handle %ifdef and %ifndef */
  preprocess_input(filebuf); // 处理 %ifdef and %ifndef */ 的那些内容

  /* Now scan the text of the input file */
  lineno = 1;
  for(cp=filebuf; (c= *cp)!=0; ){
    if( c=='\n' ) lineno++;              /* Keep track of the line number */
    if( isspace(c) ){ cp++; continue; }  /* Skip all white space */
    if( c=='/' && cp[1]=='/' ){          /* Skip C++ style comments */ // 忽略// 注释
      cp+=2;
      while( (c= *cp)!=0 && c!='\n' ) cp++;
      continue;
    }
    if( c=='/' && cp[1]=='*' ){          /* Skip C style comments */ // 忽略/* */ 注释
      cp+=2;
      while( (c= *cp)!=0 && (c!='/' || cp[-1]!='*') ){
        if( c=='\n' ) lineno++;
        cp++;
      }
      if( c ) cp++;
      continue;
    }
    ps.tokenstart = cp;                /* Mark the beginning of the token */ // ps 就是一个pstate,词法分析的专有数据结构
    ps.tokenlineno = lineno;           /* Linenumber on which token begins */
    if( c=='\"' ){                     /* String literals */
      cp++;
      while( (c= *cp)!=0 && c!='\"' ){
        if( c=='\n' ) lineno++;
        cp++;
      }
      if( c==0 ){
        ErrorMsg(ps.filename,startline,
"String starting on this line is not terminated before the end of the file."); // 没有找到右引号,就结束了。fxxk,string没有右边双引号
        ps.errorcnt++;
        nextcp = cp;
      }else{
        nextcp = cp+1;
      }
    }else if( c=='{' ){               /* A block of C code */// 识别出 代码
      int level;
      cp++;
        for(level=1; (c= *cp)!=0 && (level>1 || c!='}'); cp++){
            if( c=='\n' ) lineno++;
            else if( c=='{' ) level++;
            else if( c=='}' ) level--;
            else if( c=='/' && cp[1]=='*' ){  /* Skip comments */
              int prevc;
              cp = &cp[2];
              prevc = 0;
              while( (c= *cp)!=0 && (c!='/' || prevc!='*') ){
                if( c=='\n' ) lineno++;
                prevc = c;
                cp++;
              }
            }else if( c=='/' && cp[1]=='/' ){  /* Skip C++ style comments too */
              cp = &cp[2];
              while( (c= *cp)!=0 && c!='\n' ) cp++;
              if( c ) lineno++;
            }else if( c=='\'' || c=='\"' ){    /* String a character literals */
              int startchar, prevc;
              startchar = c;
              prevc = 0;
              for(cp++; (c= *cp)!=0 && (c!=startchar || prevc=='\\'); cp++){
                if( c=='\n' ) lineno++;
                if( prevc=='\\' ) prevc = 0;
                else              prevc = c;
              }
            }
      }
      if( c==0 ){
        ErrorMsg(ps.filename,ps.tokenlineno,
"C code starting on this line is not terminated before the end of the file.");
        ps.errorcnt++;
        nextcp = cp;
      }else{
        nextcp = cp+1;
      }
    }else if( isalnum(c) ){          /* Identifiers */ // 识别出符号
      while( (c= *cp)!=0 && (isalnum(c) || c=='_') ) cp++;
      nextcp = cp;
    }else if( c==':' && cp[1]==':' && cp[2]=='=' ){ /* The operator "::=" */ // 识别出定义符
      cp += 3;
      nextcp = cp;
    }else{                          /* All other (one character) operators */
      cp++;
      nextcp = cp;
    }
    c = *cp;
    *cp = 0; //很关键的代码,由于找到了这段符号,所以用\0截断,然后送入 parseonetoken函数分析  /* Null terminate the token */
    parseonetoken(&ps);             /* Parse the token */ // 开始把一个元素扔到parse 里面去识别下
    *cp = c; //分析完了之后,进行恢复,必须恢复现场。。。                                  /* Restore the buffer */
    cp = nextcp;
  }
  free(filebuf);                    /* Release the buffer after parsing */
  gp->rule = ps.firstrule;
  gp->errorcnt = ps.errorcnt;
}
/*************************** From the file "plink.c" *********************/
/*
** Routines processing configuration follow-set propagation links
** in the LEMON parser generator.
*/
static struct plink *plink_freelist = 0;

/* Allocate a new plink */
struct plink *Plink_new(){
  struct plink *new;

  if (plink_freelist == 0) {
    int i;
    int amt = 100;
    plink_freelist = (struct plink *) malloc(sizeof(struct plink) * amt);
    if (plink_freelist == 0) {
      fprintf(stderr,
              "Unable to allocate memory for a new follow-set propagation link.\n");
      exit(1);
    }
    for (i = 0; i < amt - 1; i++) plink_freelist[i].next = &plink_freelist[i + 1];
    plink_freelist[amt - 1].next = 0;
  }
  new = plink_freelist;
  plink_freelist = plink_freelist->next;
  return new;
}

/* Add a plink to a plink list */
void Plink_add(struct plink **plpp, struct config *cfp)
//struct plink **plpp;
//struct config *cfp;
{
  struct plink *new;
  new = Plink_new(); // Plink_new函数用了plink_freelist 一次性创建了100个plink 空间
  new->next = *plpp;
  *plpp = new; // plpp是二级指针,所以这两句代码的意思就是 将新建的new变量插到原来的plpp链表之首。
  new->cfp = cfp;
}

/* Transfer every plink on the list "from" to the list "to" */
void Plink_copy(struct plink **to,struct plink *from)
//struct plink **to;
//struct plink *from;
{
  struct plink *nextpl;
  while( from ){
    nextpl = from->next;
    from->next = *to;
    *to = from;
    from = nextpl;
  } // 当while循环出来之后,所有from的东西都转移到to上面去了
}

/* Delete every plink on the list */
void Plink_delete(struct plink *plp)
//struct plink *plp;
{
  struct plink *nextpl;

  while( plp ){
    nextpl = plp->next;
    plp->next = plink_freelist;
    plink_freelist = plp;
    plp = nextpl;
  } // 名义上是删除plink,实际上是把plp占据的空间送回plink_free列表
}
/*********************** From the file "report.c" **************************/
/*
** Procedures for generating reports and tables in the LEMON parser generator.
*/

/* Generate a filename with the given suffix.  Space to hold the
** name comes from malloc() and must be freed by the calling
** function.
*/
PRIVATE char *file_makename(lemp,suffix)
struct lemon *lemp;
char *suffix;
{
  char *name;
  char *cp;

  name = malloc( strlen(lemp->filename) + strlen(suffix) + 5 );
  if( name==0 ){
    fprintf(stderr,"Can't allocate space for a filename.\n");
    exit(1);
  }
  strcpy(name,lemp->filename);
  cp = strrchr(name,'.');
  if( cp ) *cp = 0;
  strcat(name,suffix);
  return name;
}

/* Open a file with a name based on the name of the input file,
** but with a different (specified) suffix, and return a pointer
** to the stream */
PRIVATE FILE *file_open(lemp,suffix,mode)
struct lemon *lemp;
char *suffix;
char *mode;
{
  FILE *fp;

  if( lemp->outname ) free(lemp->outname);
  lemp->outname = file_makename(lemp, suffix);
  fp = fopen(lemp->outname,mode);
  if( fp==0 && *mode=='w' ){
    fprintf(stderr,"Can't open file \"%s\".\n",lemp->outname);
    lemp->errorcnt++;
    return 0;
  }
  return fp;
}

/* Duplicate the input file without comments and without actions 
** on rules */
void Reprint(struct lemon *lemp)
//struct lemon *lemp;
{
  struct rule *rp;
  struct symbol *sp;
  int i, j, maxlen, len, ncolumns, skip;
  printf("// Reprint of input file \"%s\".\n// Symbols:\n",lemp->filename);
  maxlen = 10;
  for(i=0; i<lemp->nsymbol; i++){
    sp = lemp->symbols[i];
    len = strlen(sp->name);
    if( len>maxlen ) maxlen = len;
  }
  ncolumns = 76/(maxlen+5);
  if( ncolumns<1 ) ncolumns = 1;
  skip = (lemp->nsymbol + ncolumns - 1)/ncolumns;
  for(i=0; i<skip; i++){
    printf("//");
    for(j=i; j<lemp->nsymbol; j+=skip){
      sp = lemp->symbols[j];
      assert( sp->index==j );
      printf(" %3d %-*.*s",j,maxlen,maxlen,sp->name);
    }
    printf("\n");
  }
  for(rp=lemp->rule; rp; rp=rp->next){
    printf("%s",rp->lhs->name);
/*    if( rp->lhsalias ) printf("(%s)",rp->lhsalias); */
    printf(" ::=");
    for(i=0; i<rp->nrhs; i++){
      printf(" %s",rp->rhs[i]->name);
/*      if( rp->rhsalias[i] ) printf("(%s)",rp->rhsalias[i]); */
    }
    printf(".");
    if( rp->precsym ) printf(" [%s]",rp->precsym->name);
/*    if( rp->code ) printf("\n    %s",rp->code); */
    printf("\n");
  }
}

void ConfigPrint(fp,cfp)
FILE *fp;
struct config *cfp;
{
  struct rule *rp;
  int i;
  rp = cfp->rp;
  fprintf(fp,"%s ::=",rp->lhs->name);
  for(i=0; i<=rp->nrhs; i++){
    if( i==cfp->dot ) fprintf(fp," *");
    if( i==rp->nrhs ) break;
    fprintf(fp," %s",rp->rhs[i]->name);
  }
}

/* #define TEST */
#ifdef TEST
/* Print a set */
PRIVATE void SetPrint(out,set,lemp)
FILE *out;
char *set;
struct lemon *lemp;
{
  int i;
  char *spacer;
  spacer = "";
  fprintf(out,"%12s[","");
  for(i=0; i<lemp->nterminal; i++){
    if( SetFind(set,i) ){
      fprintf(out,"%s%s",spacer,lemp->symbols[i]->name);
      spacer = " ";
    }
  }
  fprintf(out,"]\n");
}

/* Print a plink chain */
PRIVATE void PlinkPrint(out,plp,tag)
FILE *out;
struct plink *plp;
char *tag;
{
  while( plp ){
    fprintf(out,"%12s%s (state %2d) ","",tag,plp->cfp->stp->index);
    ConfigPrint(out,plp->cfp);
    fprintf(out,"\n");
    plp = plp->next;
  }
}
#endif

/* Print an action to the given file descriptor.  Return FALSE if
** nothing was actually printed.
*/
int PrintAction(struct action *ap, FILE *fp, int indent){
  int result = 1;
  switch( ap->type ){
    case SHIFT:
      fprintf(fp,"%*s shift  %d",indent,ap->sp->name,ap->x.stp->index);
      break;
    case REDUCE:
      fprintf(fp,"%*s reduce %d",indent,ap->sp->name,ap->x.rp->index);
      break;
    case ACCEPT:
      fprintf(fp,"%*s accept",indent,ap->sp->name);
      break;
    case ERROR:
      fprintf(fp,"%*s error",indent,ap->sp->name);
      break;
    case CONFLICT:
      fprintf(fp,"%*s reduce %-3d ** Parsing conflict **",
        indent,ap->sp->name,ap->x.rp->index);
      break;
    case SH_RESOLVED:
    case RD_RESOLVED:
    case NOT_USED:
      result = 0;
      break;
  }
  return result;
}

/* Generate the "y.output" log file */
void ReportOutput(lemp)
struct lemon *lemp;
{
  int i;
  struct state *stp;
  struct config *cfp;
  struct action *ap;
  FILE *fp;

  fp = file_open(lemp,".out","wb");
  if( fp==0 ) return;
  fprintf(fp," \b");
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    fprintf(fp,"State %d:\n",stp->index);
    if( lemp->basisflag ) cfp=stp->bp;
    else                  cfp=stp->cfp;
    while( cfp ){
      char buf[20];
      if( cfp->dot==cfp->rp->nrhs ){
        sprintf(buf,"(%d)",cfp->rp->index);
        fprintf(fp,"    %5s ",buf);
      }else{
        fprintf(fp,"          ");
      }
      ConfigPrint(fp,cfp);
      fprintf(fp,"\n");
#ifdef TEST
      SetPrint(fp,cfp->fws,lemp);
      PlinkPrint(fp,cfp->fplp,"To  ");
      PlinkPrint(fp,cfp->bplp,"From");
#endif
      if( lemp->basisflag ) cfp=cfp->bp;
      else                  cfp=cfp->next;
    }
    fprintf(fp,"\n");
    for(ap=stp->ap; ap; ap=ap->next){
      if( PrintAction(ap,fp,30) ) fprintf(fp,"\n");
    }
    fprintf(fp,"\n");
  }
  fclose(fp);
  return;
}

/* Search for the file "name" which is in the same directory as
** the exacutable */
PRIVATE char *pathsearch(argv0,name,modemask)
char *argv0;
char *name;
int modemask;
{
  char *pathlist;
  char *path,*cp;
  char c;
  extern int access();

#ifdef __WIN32__
  cp = strrchr(argv0,'\\');
#else
  cp = strrchr(argv0,'/');
#endif
  if( cp ){
    c = *cp;
    *cp = 0;
    path = (char *)malloc( strlen(argv0) + strlen(name) + 2 );
    if( path ) sprintf(path,"%s/%s",argv0,name);
    *cp = c;
  }else{
    extern char *getenv();
    pathlist = getenv("PATH");
    if( pathlist==0 ) pathlist = ".:/bin:/usr/bin";
    path = (char *)malloc( strlen(pathlist)+strlen(name)+2 );
    if( path!=0 ){
      while( *pathlist ){
        cp = strchr(pathlist,':');
        if( cp==0 ) cp = &pathlist[strlen(pathlist)];
        c = *cp;
        *cp = 0;
        sprintf(path,"%s/%s",pathlist,name);
        *cp = c;
        if( c==0 ) pathlist = "";
        else pathlist = &cp[1];
        if( access(path,modemask)==0 ) break;
      }
    }
  }
  return path;
}

/* Given an action, compute the integer value for that action
** which is to be put in the action table of the generated machine.
** Return negative if no action should be generated.
*/
PRIVATE int compute_action(lemp,ap)
struct lemon *lemp;
struct action *ap;
{
  int act;
  switch( ap->type ){
    case SHIFT:  act = ap->x.stp->index;               break;
    case REDUCE: act = ap->x.rp->index + lemp->nstate; break;
    case ERROR:  act = lemp->nstate + lemp->nrule;     break;
    case ACCEPT: act = lemp->nstate + lemp->nrule + 1; break;
    default:     act = -1; break;
  }
  return act;
}

#define LINESIZE 1000
/* The next cluster of routines are for reading the template file
** and writing the results to the generated parser */
/* The first function transfers data from "in" to "out" until
** a line is seen which begins with "%%".  The line number is
** tracked.
**
** if name!=0, then any word that begin with "Parse" is changed to
** begin with *name instead.
*/
PRIVATE void tplt_xfer(name,in,out,lineno)
char *name;
FILE *in;
FILE *out;
int *lineno;
{
  int i, iStart;
  char line[LINESIZE];
  while( fgets(line,LINESIZE,in) && (line[0]!='%' || line[1]!='%') ){
    (*lineno)++;
    iStart = 0;
    if( name ){
      for(i=0; line[i]; i++){
        if( line[i]=='P' && strncmp(&line[i],"Parse",5)==0
          && (i==0 || !isalpha(line[i-1]))
        ){
          if( i>iStart ) fprintf(out,"%.*s",i-iStart,&line[iStart]);
          fprintf(out,"%s",name);
          i += 4;
          iStart = i+1;
        }
      }
    }
    fprintf(out,"%s",&line[iStart]);
  }
}

/* The next function finds the template file and opens it, returning
** a pointer to the opened file. */
PRIVATE FILE *tplt_open(struct lemon *lemp) // 打开模板文件
//struct lemon *lemp;
{
  static char templatename[] = "lempar.c";
  char buf[1000];
  FILE *in;
  char *tpltname;
  char *cp;

  cp = strrchr(lemp->filename,'.');
  if( cp ){
    sprintf(buf,"%.*s.lt",(int)(cp-lemp->filename),lemp->filename);
  }else{
    sprintf(buf,"%s.lt",lemp->filename);
  }
  if( access(buf,004)==0 ){
    tpltname = buf;
  }else if( access(templatename,004)==0 ){
    tpltname = templatename;
  }else{
    tpltname = pathsearch(lemp->argv0,templatename,0);
  }
  if( tpltname==0 ){
    fprintf(stderr,"Can't find the parser driver template file \"%s\".\n",
    templatename);
    lemp->errorcnt++;
    return 0;
  }
  in = fopen(tpltname,"rb");
  if( in==0 ){
    fprintf(stderr,"Can't open the template file \"%s\".\n",templatename);
    lemp->errorcnt++;
    return 0;
  }
  return in;
}

/* Print a #line directive line to the output file. */
PRIVATE void tplt_linedir(out,lineno,filename)
FILE *out;
int lineno;
char *filename;
{
  fprintf(out,"#line %d \"",lineno);
  while( *filename ){
    if( *filename == '\\' ) putc('\\',out);
    putc(*filename,out);
    filename++;
  }
  fprintf(out,"\"\n");
}

/* Print a string to the file and keep the linenumber up to date */
PRIVATE void tplt_print(out,lemp,str,strln,lineno)
FILE *out;
struct lemon *lemp;
char *str;
int strln;
int *lineno;
{
  if( str==0 ) return;
  tplt_linedir(out,strln,lemp->filename);
  (*lineno)++;
  while( *str ){
    if( *str=='\n' ) (*lineno)++;
    putc(*str,out);
    str++;
  }
  if( str[-1]!='\n' ){
    putc('\n',out);
    (*lineno)++;
  }
  tplt_linedir(out,*lineno+2,lemp->outname); 
  (*lineno)+=2;
  return;
}

/*
** The following routine emits code for the destructor for the
** symbol sp
*/
void emit_destructor_code(out,sp,lemp,lineno)
FILE *out;
struct symbol *sp;
struct lemon *lemp;
int *lineno;
{
 char *cp = 0;

 int linecnt = 0;
 if( sp->type==TERMINAL ){
   cp = lemp->tokendest;
   if( cp==0 ) return;
   tplt_linedir(out,lemp->tokendestln,lemp->filename);
   fprintf(out,"{");
 }else if( sp->destructor ){
   cp = sp->destructor;
   tplt_linedir(out,sp->destructorln,lemp->filename);
   fprintf(out,"{");
 }else if( lemp->vardest ){
   cp = lemp->vardest;
   if( cp==0 ) return;
   tplt_linedir(out,lemp->vardestln,lemp->filename);
   fprintf(out,"{");
 }else{
   assert( 0 );  /* Cannot happen */
 }
 for(; *cp; cp++){
   if( *cp=='$' && cp[1]=='$' ){
     fprintf(out,"(yypminor->yy%d)",sp->dtnum);
     cp++;
     continue;
   }
   if( *cp=='\n' ) linecnt++;
   fputc(*cp,out);
 }
 (*lineno) += 3 + linecnt;
 fprintf(out,"}\n");
 tplt_linedir(out,*lineno,lemp->outname);
 return;
}

/*
** Return TRUE (non-zero) if the given symbol has a destructor.
*/
int has_destructor(sp, lemp)
struct symbol *sp;
struct lemon *lemp;
{
  int ret;
  if( sp->type==TERMINAL ){
    ret = lemp->tokendest!=0;
  }else{
    ret = lemp->vardest!=0 || sp->destructor!=0;
  }
  return ret;
}

/*
** Append text to a dynamically allocated string.  If zText is 0 then
** reset the string to be empty again.  Always return the complete text
** of the string (which is overwritten with each call).
**
** n bytes of zText are stored.  If n==0 then all of zText up to the first
** \000 terminator is stored.  zText can contain up to two instances of
** %d.  The values of p1 and p2 are written into the first and second
** %d.
**
** If n==-1, then the previous character is overwritten.
*/
PRIVATE char *append_str(char *zText, int n, int p1, int p2){
  static char *z = 0;
  static int alloced = 0;
  static int used = 0;
  int c;
  char zInt[40];

  if( zText==0 ){
    used = 0;
    return z;
  }
  if( n<=0 ){
    if( n<0 ){
      used += n;
      assert( used>=0 );
    }
    n = strlen(zText);
  }
  if( n+sizeof(zInt)*2+used >= alloced ){
    alloced = n + sizeof(zInt)*2 + used + 200;
    z = realloc(z,  alloced);
  }
  if( z==0 ) return "";
  while( n-- > 0 ){
    c = *(zText++);
    if( c=='%' && zText[0]=='d' ){
      sprintf(zInt, "%d", p1);
      p1 = p2;
      strcpy(&z[used], zInt);
      used += strlen(&z[used]);
      zText++;
      n--;
    }else{
      z[used++] = c;
    }
  }
  z[used] = 0;
  return z;
}

/*
** zCode is a string that is the action associated with a rule.  Expand
** the symbols in this string so that the refer to elements of the parser
** stack.
*/
PRIVATE void translate_code(struct lemon *lemp, struct rule *rp){
  char *cp, *xp;
  int i;
  char lhsused = 0;    /* True if the LHS element has been used */
  char used[MAXRHS];   /* True for each RHS element which is used */

  for(i=0; i<rp->nrhs; i++) used[i] = 0;
  lhsused = 0;

  append_str(0,0,0,0);
  for(cp=rp->code; *cp; cp++){
    if( isalpha(*cp) && (cp==rp->code || (!isalnum(cp[-1]) && cp[-1]!='_')) ){
      char saved;
      for(xp= &cp[1]; isalnum(*xp) || *xp=='_'; xp++);
      saved = *xp;
      *xp = 0;
      if( rp->lhsalias && strcmp(cp,rp->lhsalias)==0 ){
        append_str("yygotominor.yy%d",0,rp->lhs->dtnum,0);
        cp = xp;
        lhsused = 1;
      }else{
        for(i=0; i<rp->nrhs; i++){
          if( rp->rhsalias[i] && strcmp(cp,rp->rhsalias[i])==0 ){
            if( cp!=rp->code && cp[-1]=='@' ){
              /* If the argument is of the form @X then substituted
              ** the token number of X, not the value of X */
              append_str("yymsp[%d].major",-1,i-rp->nrhs+1,0);
            }else{
              append_str("yymsp[%d].minor.yy%d",0,
                         i-rp->nrhs+1,rp->rhs[i]->dtnum);
            }
            cp = xp;
            used[i] = 1;
            break;
          }
        }
      }
      *xp = saved;
    }
    append_str(cp, 1, 0, 0);
  } /* End loop */

  /* Check to make sure the LHS has been used */
  if( rp->lhsalias && !lhsused ){
    ErrorMsg(lemp->filename,rp->ruleline,
      "Label \"%s\" for \"%s(%s)\" is never used.",
        rp->lhsalias,rp->lhs->name,rp->lhsalias);
    lemp->errorcnt++;
  }

  /* Generate destructor code for RHS symbols which are not used in the
  ** reduce code */
  for(i=0; i<rp->nrhs; i++){
    if( rp->rhsalias[i] && !used[i] ){
      ErrorMsg(lemp->filename,rp->ruleline,
        "Label %s for \"%s(%s)\" is never used.",
        rp->rhsalias[i],rp->rhs[i]->name,rp->rhsalias[i]);
      lemp->errorcnt++;
    }else if( rp->rhsalias[i]==0 ){
      if( has_destructor(rp->rhs[i],lemp) ){
        append_str("  yy_destructor(%d,&yymsp[%d].minor);\n", 0,
           rp->rhs[i]->index,i-rp->nrhs+1);
      }else{
        /* No destructor defined for this term */
      }
    }
  }
  cp = append_str(0,0,0,0);
  rp->code = Strsafe(cp);
}

/* 
** Generate code which executes when the rule "rp" is reduced.  Write
** the code to "out".  Make sure lineno stays up-to-date.
*/
PRIVATE void emit_code(out,rp,lemp,lineno)
FILE *out;
struct rule *rp;
struct lemon *lemp;
int *lineno;
{
 char *cp;
 int linecnt = 0;

 /* Generate code to do the reduce action */
 if( rp->code ){
   tplt_linedir(out,rp->line,lemp->filename);
   fprintf(out,"{%s",rp->code);
   for(cp=rp->code; *cp; cp++){
     if( *cp=='\n' ) linecnt++;
   } /* End loop */
   (*lineno) += 3 + linecnt;
   fprintf(out,"}\n");
   tplt_linedir(out,*lineno,lemp->outname);
 } /* End if( rp->code ) */

 return;
}

/*
** Print the definition of the union used for the parser's data stack.
** This union contains fields for every possible data type for tokens
** and nonterminals.  In the process of computing and printing this
** union, also set the ".dtnum" field of every terminal and nonterminal
** symbol.
*/
void print_stack_union(out,lemp,plineno,mhflag)
FILE *out;                  /* The output stream */
struct lemon *lemp;         /* The main info structure for this parser */
int *plineno;               /* Pointer to the line number */
int mhflag;                 /* True if generating makeheaders output */
{
  int lineno = *plineno;    /* The line number of the output */
  char **types;             /* A hash table of datatypes */
  int arraysize;            /* Size of the "types" array */
  int maxdtlength;          /* Maximum length of any ".datatype" field. */
  char *stddt;              /* Standardized name for a datatype */
  int i,j;                  /* Loop counters */
  int hash;                 /* For hashing the name of a type */
  char *name;               /* Name of the parser */

  /* Allocate and initialize types[] and allocate stddt[] */
  arraysize = lemp->nsymbol * 2;
  types = (char**)malloc( arraysize * sizeof(char*) );
  for(i=0; i<arraysize; i++) types[i] = 0;
  maxdtlength = 0;
  if( lemp->vartype ){
    maxdtlength = strlen(lemp->vartype);
  }
  for(i=0; i<lemp->nsymbol; i++){
    int len;
    struct symbol *sp = lemp->symbols[i];
    if( sp->datatype==0 ) continue;
    len = strlen(sp->datatype);
    if( len>maxdtlength ) maxdtlength = len;
  }
  stddt = (char*)malloc( maxdtlength*2 + 1 );
  if( types==0 || stddt==0 ){
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }

  /* Build a hash table of datatypes. The ".dtnum" field of each symbol
  ** is filled in with the hash index plus 1.  A ".dtnum" value of 0 is
  ** used for terminal symbols.  If there is no %default_type defined then
  ** 0 is also used as the .dtnum value for nonterminals which do not specify
  ** a datatype using the %type directive.
  */
  for(i=0; i<lemp->nsymbol; i++){
    struct symbol *sp = lemp->symbols[i];
    char *cp;
    if( sp==lemp->errsym ){
      sp->dtnum = arraysize+1;
      continue;
    }
    if( sp->type!=NONTERMINAL || (sp->datatype==0 && lemp->vartype==0) ){
      sp->dtnum = 0;
      continue;
    }
    cp = sp->datatype;
    if( cp==0 ) cp = lemp->vartype;
    j = 0;
    while( isspace(*cp) ) cp++;
    while( *cp ) stddt[j++] = *cp++;
    while( j>0 && isspace(stddt[j-1]) ) j--;
    stddt[j] = 0;
    hash = 0;
    for(j=0; stddt[j]; j++){
      hash = hash*53 + stddt[j];
    }
    hash = (hash & 0x7fffffff)%arraysize;
    while( types[hash] ){
      if( strcmp(types[hash],stddt)==0 ){
        sp->dtnum = hash + 1;
        break;
      }
      hash++;
      if( hash>=arraysize ) hash = 0;
    }
    if( types[hash]==0 ){
      sp->dtnum = hash + 1;
      types[hash] = (char*)malloc( strlen(stddt)+1 );
      if( types[hash]==0 ){
        fprintf(stderr,"Out of memory.\n");
        exit(1);
      }
      strcpy(types[hash],stddt);
    }
  }

  /* Print out the definition of YYTOKENTYPE and YYMINORTYPE */
  name = lemp->name ? lemp->name : "Parse";
  lineno = *plineno;
  if( mhflag ){ fprintf(out,"#if INTERFACE\n"); lineno++; }
  fprintf(out,"#define %sTOKENTYPE %s\n",name,
    lemp->tokentype?lemp->tokentype:"void*");  lineno++;
  if( mhflag ){ fprintf(out,"#endif\n"); lineno++; }
  fprintf(out,"typedef union {\n"); lineno++;
  fprintf(out,"  %sTOKENTYPE yy0;\n",name); lineno++;
  for(i=0; i<arraysize; i++){
    if( types[i]==0 ) continue;
    fprintf(out,"  %s yy%d;\n",types[i],i+1); lineno++;
    free(types[i]);
  }
  fprintf(out,"  int yy%d;\n",lemp->errsym->dtnum); lineno++;
  free(stddt);
  free(types);
  fprintf(out,"} YYMINORTYPE;\n"); lineno++;
  *plineno = lineno;
}

/*
** Return the name of a C datatype able to represent values between
** lwr and upr, inclusive.
*/
static const char *minimum_size_type(int lwr, int upr){
  if( lwr>=0 ){
    if( upr<=255 ){
      return "unsigned char";
    }else if( upr<65535 ){
      return "unsigned short int";
    }else{
      return "unsigned int";
    }
  }else if( lwr>=-127 && upr<=127 ){
    return "signed char";
  }else if( lwr>=-32767 && upr<32767 ){
    return "short";
  }else{
    return "int";
  }
}

/*
** Each state contains a set of token transaction and a set of
** nonterminal transactions.  Each of these sets makes an instance
** of the following structure.  An array of these structures is used
** to order the creation of entries in the yy_action[] table.
*/
struct axset {
  struct state *stp;   /* A pointer to a state */
  int isTkn;           /* True to use tokens.  False for non-terminals */
  int nAction;         /* Number of actions */
};

/*
** Compare to axset structures for sorting purposes
*/
static int axset_compare(const void *a, const void *b){
  struct axset *p1 = (struct axset*)a;
  struct axset *p2 = (struct axset*)b;
  return p2->nAction - p1->nAction;
}

/* Generate C source code for the parser */
void ReportTable(struct lemon *lemp, int mhflag)
//struct lemon *lemp;
//int mhflag;     /* Output in makeheaders format if true */
{
  FILE *out, *in;
  char line[LINESIZE];
  int  lineno;
  struct state *stp;
  struct action *ap;
  struct rule *rp;
  struct acttab *pActtab;
  int i, j, n;
  char *name;
  int mnTknOfst, mxTknOfst;
  int mnNtOfst, mxNtOfst;
  struct axset *ax;

  in = tplt_open(lemp);
  if( in==0 ) return;
  out = file_open(lemp,".cpp","wb");
  if( out==0 ){
    fclose(in);
    return;
  }
  lineno = 1;
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the include code, if any */
  tplt_print(out,lemp,lemp->include,lemp->includeln,&lineno);
  if( mhflag ){
    char *name = file_makename(lemp, ".h");
    fprintf(out,"#include \"%s\"\n", name); lineno++;
    free(name);
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate #defines for all tokens */
  if( mhflag ){
    char *prefix;
    fprintf(out,"#if INTERFACE\n"); lineno++;
    if( lemp->tokenprefix ) prefix = lemp->tokenprefix;
    else                    prefix = "";
    for(i=1; i<lemp->nterminal; i++){
      fprintf(out,"#define %s%-30s %2d\n",prefix,lemp->symbols[i]->name,i);
      lineno++;
    }
    fprintf(out,"#endif\n"); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the defines */
  fprintf(out,"#define YYCODETYPE %s\n",
    minimum_size_type(0, lemp->nsymbol+5)); lineno++;
  fprintf(out,"#define YYNOCODE %d\n",lemp->nsymbol+1);  lineno++;
  fprintf(out,"#define YYACTIONTYPE %s\n",
    minimum_size_type(0, lemp->nstate+lemp->nrule+5));  lineno++;
  print_stack_union(out,lemp,&lineno,mhflag);
  if( lemp->stacksize ){
    if( atoi(lemp->stacksize)<=0 ){
      ErrorMsg(lemp->filename,0,
"Illegal stack size: [%s].  The stack size should be an integer constant.",
        lemp->stacksize);
      lemp->errorcnt++;
      lemp->stacksize = "100";
    }
    fprintf(out,"#define YYSTACKDEPTH %s\n",lemp->stacksize);  lineno++;
  }else{
    fprintf(out,"#define YYSTACKDEPTH 100\n");  lineno++;
  }
  if( mhflag ){
    fprintf(out,"#if INTERFACE\n"); lineno++;
  }
  name = lemp->name ? lemp->name : "Parse";
  if( lemp->arg && lemp->arg[0] ){
    int i;
    i = strlen(lemp->arg);
    while( i>=1 && isspace(lemp->arg[i-1]) ) i--;
    while( i>=1 && (isalnum(lemp->arg[i-1]) || lemp->arg[i-1]=='_') ) i--;
    fprintf(out,"#define %sARG_SDECL %s;\n",name,lemp->arg);  lineno++;
    fprintf(out,"#define %sARG_PDECL ,%s\n",name,lemp->arg);  lineno++;
    fprintf(out,"#define %sARG_FETCH %s = yypParser->%s\n",
                 name,lemp->arg,&lemp->arg[i]);  lineno++;
    fprintf(out,"#define %sARG_STORE yypParser->%s = %s\n",
                 name,&lemp->arg[i],&lemp->arg[i]);  lineno++;
  }else{
    fprintf(out,"#define %sARG_SDECL\n",name);  lineno++;
    fprintf(out,"#define %sARG_PDECL\n",name);  lineno++;
    fprintf(out,"#define %sARG_FETCH\n",name); lineno++;
    fprintf(out,"#define %sARG_STORE\n",name); lineno++;
  }
  if( mhflag ){
    fprintf(out,"#endif\n"); lineno++;
  }
  fprintf(out,"#define YYNSTATE %d\n",lemp->nstate);  lineno++;
  fprintf(out,"#define YYNRULE %d\n",lemp->nrule);  lineno++;
  fprintf(out,"#define YYERRORSYMBOL %d\n",lemp->errsym->index);  lineno++;
  fprintf(out,"#define YYERRSYMDT yy%d\n",lemp->errsym->dtnum);  lineno++;
  if( lemp->has_fallback ){
    fprintf(out,"#define YYFALLBACK 1\n");  lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the action table and its associates:
  **
  **  yy_action[]        A single table containing all actions.
  **  yy_lookahead[]     A table containing the lookahead for each entry in
  **                     yy_action.  Used to detect hash collisions.
  **  yy_shift_ofst[]    For each state, the offset into yy_action for
  **                     shifting terminals.
  **  yy_reduce_ofst[]   For each state, the offset into yy_action for
  **                     shifting non-terminals after a reduce.
  **  yy_default[]       Default action for each state.
  */

  /* Compute the actions on all states and count them up */
  ax = malloc( sizeof(ax[0])*lemp->nstate*2 );
  if( ax==0 ){
    fprintf(stderr,"malloc failed\n");
    exit(1);
  }
  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    stp->nTknAct = stp->nNtAct = 0;
    stp->iDflt = lemp->nstate + lemp->nrule;
    stp->iTknOfst = NO_OFFSET;
    stp->iNtOfst = NO_OFFSET;
    for(ap=stp->ap; ap; ap=ap->next){
      if( compute_action(lemp,ap)>=0 ){
        if( ap->sp->index<lemp->nterminal ){
          stp->nTknAct++;
        }else if( ap->sp->index<lemp->nsymbol ){
          stp->nNtAct++;
        }else{
          stp->iDflt = compute_action(lemp, ap);
        }
      }
    }
    ax[i*2].stp = stp;
    ax[i*2].isTkn = 1;
    ax[i*2].nAction = stp->nTknAct;
    ax[i*2+1].stp = stp;
    ax[i*2+1].isTkn = 0;
    ax[i*2+1].nAction = stp->nNtAct;
  }
  mxTknOfst = mnTknOfst = 0;
  mxNtOfst = mnNtOfst = 0;

  /* Compute the action table.  In order to try to keep the size of the
  ** action table to a minimum, the heuristic of placing the largest action
  ** sets first is used.
  */
  qsort(ax, lemp->nstate*2, sizeof(ax[0]), axset_compare);
  pActtab = acttab_alloc();
  for(i=0; i<lemp->nstate*2 && ax[i].nAction>0; i++){
    stp = ax[i].stp;
    if( ax[i].isTkn ){
      for(ap=stp->ap; ap; ap=ap->next){
        int action;
        if( ap->sp->index>=lemp->nterminal ) continue;
        action = compute_action(lemp, ap);
        if( action<0 ) continue;
        acttab_action(pActtab, ap->sp->index, action);
      }
      stp->iTknOfst = acttab_insert(pActtab);
      if( stp->iTknOfst<mnTknOfst ) mnTknOfst = stp->iTknOfst;
      if( stp->iTknOfst>mxTknOfst ) mxTknOfst = stp->iTknOfst;
    }else{
      for(ap=stp->ap; ap; ap=ap->next){
        int action;
        if( ap->sp->index<lemp->nterminal ) continue;
        if( ap->sp->index==lemp->nsymbol ) continue;
        action = compute_action(lemp, ap);
        if( action<0 ) continue;
        acttab_action(pActtab, ap->sp->index, action);
      }
      stp->iNtOfst = acttab_insert(pActtab);
      if( stp->iNtOfst<mnNtOfst ) mnNtOfst = stp->iNtOfst;
      if( stp->iNtOfst>mxNtOfst ) mxNtOfst = stp->iNtOfst;
    }
  }
  free(ax);

  /* Output the yy_action table */
  fprintf(out,"static const YYACTIONTYPE yy_action[] = {\n"); lineno++;
  n = acttab_size(pActtab);
  for(i=j=0; i<n; i++){
    int action = acttab_yyaction(pActtab, i);
    if( action<0 ) action = lemp->nsymbol + lemp->nrule + 2;
    if( j==0 ) fprintf(out," /* %5d */ ", i);
    fprintf(out, " %4d,", action);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "};\n"); lineno++;

  /* Output the yy_lookahead table */
  fprintf(out,"static const YYCODETYPE yy_lookahead[] = {\n"); lineno++;
  for(i=j=0; i<n; i++){
    int la = acttab_yylookahead(pActtab, i);
    if( la<0 ) la = lemp->nsymbol;
    if( j==0 ) fprintf(out," /* %5d */ ", i);
    fprintf(out, " %4d,", la);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "};\n"); lineno++;

  /* Output the yy_shift_ofst[] table */
  fprintf(out, "#define YY_SHIFT_USE_DFLT (%d)\n", mnTknOfst-1); lineno++;
  fprintf(out, "static const %s yy_shift_ofst[] = {\n", 
          minimum_size_type(mnTknOfst-1, mxTknOfst)); lineno++;
  n = lemp->nstate;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iTknOfst;
    if( ofst==NO_OFFSET ) ofst = mnTknOfst - 1;
    if( j==0 ) fprintf(out," /* %5d */ ", i);
    fprintf(out, " %4d,", ofst);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "};\n"); lineno++;

  /* Output the yy_reduce_ofst[] table */
  fprintf(out, "#define YY_REDUCE_USE_DFLT (%d)\n", mnNtOfst-1); lineno++;
  fprintf(out, "static const %s yy_reduce_ofst[] = {\n", 
          minimum_size_type(mnNtOfst-1, mxNtOfst)); lineno++;
  n = lemp->nstate;
  for(i=j=0; i<n; i++){
    int ofst;
    stp = lemp->sorted[i];
    ofst = stp->iNtOfst;
    if( ofst==NO_OFFSET ) ofst = mnNtOfst - 1;
    if( j==0 ) fprintf(out," /* %5d */ ", i);
    fprintf(out, " %4d,", ofst);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "};\n"); lineno++;

  /* Output the default action table */
  fprintf(out, "static const YYACTIONTYPE yy_default[] = {\n"); lineno++;
  n = lemp->nstate;
  for(i=j=0; i<n; i++){
    stp = lemp->sorted[i];
    if( j==0 ) fprintf(out," /* %5d */ ", i);
    fprintf(out, " %4d,", stp->iDflt);
    if( j==9 || i==n-1 ){
      fprintf(out, "\n"); lineno++;
      j = 0;
    }else{
      j++;
    }
  }
  fprintf(out, "};\n"); lineno++;
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the table of fallback tokens.
  */
  if( lemp->has_fallback ){
    for(i=0; i<lemp->nterminal; i++){
      struct symbol *p = lemp->symbols[i];
      if( p->fallback==0 ){
        fprintf(out, "    0,  /* %10s => nothing */\n", p->name);
      }else{
        fprintf(out, "  %3d,  /* %10s => %s */\n", p->fallback->index,
          p->name, p->fallback->name);
      }
      lineno++;
    }
  }
  tplt_xfer(lemp->name, in, out, &lineno);

  /* Generate a table containing the symbolic name of every symbol
  */
  for(i=0; i<lemp->nsymbol; i++){
    sprintf(line,"\"%s\",",lemp->symbols[i]->name);
    fprintf(out,"  %-15s",line);
    if( (i&3)==3 ){ fprintf(out,"\n"); lineno++; }
  }
  if( (i&3)!=0 ){ fprintf(out,"\n"); lineno++; }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate a table containing a text string that describes every
  ** rule in the rule set of the grammer.  This information is used
  ** when tracing REDUCE actions.
  */
  for(i=0, rp=lemp->rule; rp; rp=rp->next, i++){
    assert( rp->index==i );
    fprintf(out," /* %3d */ \"%s ::=", i, rp->lhs->name);
    for(j=0; j<rp->nrhs; j++) fprintf(out," %s",rp->rhs[j]->name);
    fprintf(out,"\",\n"); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes every time a symbol is popped from
  ** the stack while processing errors or while destroying the parser. 
  ** (In other words, generate the %destructor actions)
  */
  if( lemp->tokendest ){
    for(i=0; i<lemp->nsymbol; i++){
      struct symbol *sp = lemp->symbols[i];
      if( sp==0 || sp->type!=TERMINAL ) continue;
      fprintf(out,"    case %d:\n",sp->index); lineno++;
    }
    for(i=0; i<lemp->nsymbol && lemp->symbols[i]->type!=TERMINAL; i++);
    if( i<lemp->nsymbol ){
      emit_destructor_code(out,lemp->symbols[i],lemp,&lineno);
      fprintf(out,"      break;\n"); lineno++;
    }
  }
  if( lemp->vardest ){
    struct symbol *dflt_sp = 0;
    for(i=0; i<lemp->nsymbol; i++){
      struct symbol *sp = lemp->symbols[i];
      if( sp==0 || sp->type==TERMINAL ||
          sp->index<=0 || sp->destructor!=0 ) continue;
      fprintf(out,"    case %d:\n",sp->index); lineno++;
      dflt_sp = sp;
    }
    if( dflt_sp!=0 ){
      emit_destructor_code(out,dflt_sp,lemp,&lineno);
      fprintf(out,"      break;\n"); lineno++;
    }
  }
  for(i=0; i<lemp->nsymbol; i++){
    struct symbol *sp = lemp->symbols[i];
    if( sp==0 || sp->type==TERMINAL || sp->destructor==0 ) continue;
    fprintf(out,"    case %d:\n",sp->index); lineno++;

    /* Combine duplicate destructors into a single case */
    for(j=i+1; j<lemp->nsymbol; j++){
      struct symbol *sp2 = lemp->symbols[j];
      if( sp2 && sp2->type!=TERMINAL && sp2->destructor
          && sp2->dtnum==sp->dtnum
          && strcmp(sp->destructor,sp2->destructor)==0 ){
         fprintf(out,"    case %d:\n",sp2->index); lineno++;
         sp2->destructor = 0;
      }
    }

    emit_destructor_code(out,lemp->symbols[i],lemp,&lineno);
    fprintf(out,"      break;\n"); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes whenever the parser stack overflows */
  tplt_print(out,lemp,lemp->overflow,lemp->overflowln,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate the table of rule information 
  **
  ** Note: This code depends on the fact that rules are number
  ** sequentually beginning with 0.
  */
  for(rp=lemp->rule; rp; rp=rp->next){
    fprintf(out,"  { %d, %d },\n",rp->lhs->index,rp->nrhs); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which execution during each REDUCE action */
  for(rp=lemp->rule; rp; rp=rp->next){
    if( rp->code ) translate_code(lemp, rp);
  }
  for(rp=lemp->rule; rp; rp=rp->next){
    struct rule *rp2;
    if( rp->code==0 ) continue;
    fprintf(out,"      case %d:\n",rp->index); lineno++;
    for(rp2=rp->next; rp2; rp2=rp2->next){
      if( rp2->code==rp->code ){
        fprintf(out,"      case %d:\n",rp2->index); lineno++;
        rp2->code = 0;
      }
    }
    emit_code(out,rp,lemp,&lineno);
    fprintf(out,"        break;\n"); lineno++;
  }
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes if a parse fails */
  tplt_print(out,lemp,lemp->failure,lemp->failureln,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes when a syntax error occurs */
  tplt_print(out,lemp,lemp->error,lemp->errorln,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Generate code which executes when the parser accepts its input */
  tplt_print(out,lemp,lemp->accept,lemp->acceptln,&lineno);
  tplt_xfer(lemp->name,in,out,&lineno);

  /* Append any addition code the user desires */
  tplt_print(out,lemp,lemp->extracode,lemp->extracodeln,&lineno);

  fclose(in);
  fclose(out);
  return;
}

/* Generate a header file for the parser */
void ReportHeader(struct lemon *lemp)
//struct lemon *lemp;
{
  FILE *out, *in;
  char *prefix;
  char line[LINESIZE];
  char pattern[LINESIZE];
  int i;

  if( lemp->tokenprefix ) prefix = lemp->tokenprefix;
  else                    prefix = "";
  in = file_open(lemp,".h","rb");
  if( in ){
    for(i=1; i<lemp->nterminal && fgets(line,LINESIZE,in); i++){
      sprintf(pattern,"#define %s%-30s %2d\n",prefix,lemp->symbols[i]->name,i);
      if( strcmp(line,pattern) ) break;
    }
    fclose(in);
    if( i==lemp->nterminal ){
      /* No change in the file.  Don't rewrite it. */
      return;
    }
  }
  out = file_open(lemp,".h","wb");
  if( out ){
    for(i=1; i<lemp->nterminal; i++){
      fprintf(out,"#define %s%-30s %2d\n",prefix,lemp->symbols[i]->name,i);
    }
    fclose(out);  
  }
  return;
}

/* Reduce the size of the action tables, if possible, by making use
** of defaults.
**
** In this version, we take the most frequent REDUCE action and make
** it the default.  Only default a reduce if there are more than one.
*/
void CompressTables(lemp)
struct lemon *lemp;
{
  struct state *stp;
  struct action *ap, *ap2;
  struct rule *rp, *rp2, *rbest;
  int nbest, n;
  int i;

  for(i=0; i<lemp->nstate; i++){
    stp = lemp->sorted[i];
    nbest = 0;
    rbest = 0;

    for(ap=stp->ap; ap; ap=ap->next){
      if( ap->type!=REDUCE ) continue;
      rp = ap->x.rp;
      if( rp==rbest ) continue;
      n = 1;
      for(ap2=ap->next; ap2; ap2=ap2->next){
        if( ap2->type!=REDUCE ) continue;
        rp2 = ap2->x.rp;
        if( rp2==rbest ) continue;
        if( rp2==rp ) n++;
      }
      if( n>nbest ){
        nbest = n;
        rbest = rp;
      }
    }
 
    /* Do not make a default if the number of rules to default
    ** is not at least 2 */
    if( nbest<2 ) continue;


    /* Combine matching REDUCE actions into a single default */
    for(ap=stp->ap; ap; ap=ap->next){
      if( ap->type==REDUCE && ap->x.rp==rbest ) break;
    }
    assert( ap );
    ap->sp = Symbol_new("{default}");
    for(ap=ap->next; ap; ap=ap->next){
      if( ap->type==REDUCE && ap->x.rp==rbest ) ap->type = NOT_USED;
    }
    stp->ap = Action_sort(stp->ap);
  }
}

/***************** From the file "set.c" ************************************/
/*
** Set manipulation routines for the LEMON parser generator.
*/

static int size = 0;

/* Set the set size */
void SetSize(int n)
//int n;
{
  size = n+1;
}

/* Allocate a new set */
char *SetNew(){
  char *s;
  int i;
  s = (char*)malloc( size ); //  全局变量size=nterminal+1 ::: nterminal就是终结符的个数
  if( s==0 ){
    extern void memory_error();
    memory_error();
  }
  for(i=0; i<size; i++) s[i] = 0;
  return s;
}

/* Deallocate a set */
void SetFree(char *s)
//char *s;
{
  free(s);
}

/* Add a new element to the set.  Return TRUE if the element was added
** and FALSE if it was already there. */
int SetAdd(char *s,int e)
//char *s;
//int e;
{
  int rv;     // 第一种情况,s2文法从来没有加到过firstset中,则rv=s[s2->index]必定为0,这时候令s[s2->index]为1,然后return !0=1,表示加入成功
  rv = s[e];  // 第一种情况,s2文法之前已经加到过firstset中,则rv=s[s2->index]必定为1,这时候令s[s2->index]为1,然后return !1=0,表示加入多余
  s[e] = 1;   // setNew的时候 s是字符串,而此处 直接把整数0或者1 赋给它
  return !rv; // char占1字节，其表示范围为-128~127，由于C语言对char运算的规则和存储的方式，均与其它整型相同，所以可以把char当做是范围最小的整型，字符型数据也就可以同整型数据通用了。
}

/* Add every element of s2 to s1.  Return TRUE if s1 changes. */
int SetUnion(char *s1,char *s2)
//char *s1;
//char *s2;
{
  int i, progress;
  progress = 0;
  for(i=0; i<size; i++){
    if( s2[i]==0 ) continue; // 先判断第i个终结符是否已在s2的firstset集合中。如果s2[i]为0,表示这个终结符不在集合中,不用考虑添加。
    if( s1[i]==0 ){ // 如果s2[i]不为0,说明那个终结符已经在右边该符号的firstset中了,需要添加到左边符号的s1的firstset中
      progress = 1; // 添加到s1的过程需要考虑该终结符是否已经 添加过了。
      s1[i] = 1;
    }
  }
  return progress;
}
/********************** From the file "table.c" ****************************/
/*
** All code in this file has been automatically generated
** from a specification in the file
**              "table.q"
** by the associative array code building program "aagen".
** Do not edit this file!  Instead, edit the specification
** file, then rerun aagen.
*/
/*
** Code for processing tables in the LEMON parser generator.
*/

PRIVATE int strhash(x) // 重复得把前一个字符的ASCII值乘以13,再与后一位字母的ASCII值相加,最后得到一个hash值
char *x;
{
  int h = 0;
  while( *x) h = h*13 + *(x++);
  return h;
}

/* Works like strdup, sort of.  Save a string in malloced memory, but
** keep strings in a table so that the same string is not in more
** than one place.
*/
char *Strsafe(char *y)
//char *y;
{
  char *z;

  z = Strsafe_find(y);
  if( z==0 && (z=malloc( strlen(y)+1 ))!=0 ){
    strcpy(z,y);
    Strsafe_insert(z);
  }
  MemoryCheck(z);
  return z;
}

/* There is one instance of the following structure for each
** associative array of type "x1".
*/
struct s_x1 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x1node *tbl;  /* The data stored here */
  struct s_x1node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x1".
*/
typedef struct s_x1node {
  char *data;                  /* The data */
  struct s_x1node *next;   /* Next entry with the same hash */
  struct s_x1node **from;  /* Previous link */ //TODO 二级指针,跟next有区别,后面再讨论
} x1node;

/* There is only one instance of the array, which is the following */
static struct s_x1 *x1a; // 这是一个数组,整个LEMON程序只有这一个数组。 装备字符串之用x1a

/* Allocate a new associative array */
void Strsafe_init(){
  if( x1a ) return;
  x1a = (struct s_x1*)malloc( sizeof(struct s_x1) );
  if( x1a ){
    x1a->size = 1024; // 2的指数幂
    x1a->count = 0;
    x1a->tbl = (x1node*)malloc( // sizeof(x1node)=24个字节 sizeof(x1node*)=8个字节。一般C语言一个指针就是4个字节=32bits,但是 在MAC64位机器上就是8个字节=64bits。 也就是8个char,在c语言中,一个char就是一个byte
      (sizeof(x1node) + sizeof(x1node*))*1024 );// √TODO 为什么是(sizeof(x1node) + sizeof(x1node*))*1024 而不是 (sizeof(x1node))*1024? 要结合下面的代码来理解x1a->ht
    if( x1a->tbl==0 ){
      free(x1a);
      x1a = 0;
    }else{
      int i;
      x1a->ht = (x1node**)&(x1a->tbl[1024]); // ht 是二级指针,所以指向还是指针。x1a->tbl 不仅申请了1024个sizeof(x1node),分别是x1a->tbl[0]到x1a->tbl[1023]。还申请了1024个sizeof(x1node*),这样从x1a->tbl[1024]就是x1a->ht 的首地址了,再依次初始化后面的1023个指针
      for(i=0; i<1024; i++) x1a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Strsafe_insert(char *data)
//char *data;
{
  x1node *np;
  int h;
  int ph;

  if( x1a==0 ) return 0;
  ph = strhash(data);
  h = ph & (x1a->size-1);
  np = x1a->ht[h];
  while( np ){
    if( strcmp(np->data,data)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0; // 已经存在了,无需添加
    }
    np = np->next;
  }
  if( x1a->count>=x1a->size ){ // 1024个位置不够了,count大于size,这个时候就需要double一下空间
    /* Need to make the hash table bigger */
    int i,size;
    struct s_x1 array;
    array.size = size = x1a->size*2; // 把空间double一下
    array.count = x1a->count;
    array.tbl = (x1node*)malloc(
      (sizeof(x1node) + sizeof(x1node*))*size );
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x1node**)&(array.tbl[size]);
    for(i=0; i<size; i++) array.ht[i] = 0;
    for(i=0; i<x1a->count; i++){ // 把老空间的x1a 结构里面的数据全部移到 新空间这里来
      x1node *oldnp, *newnp;
      oldnp = &(x1a->tbl[i]);
      h = strhash(oldnp->data) & (size-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x1a->tbl); // 把旧空间的数据free掉
    *x1a = array;
  }
  /* Insert the new data */ // 如果空间足够,就直接 insert数据
  h = ph & (x1a->size-1);
  np = &(x1a->tbl[x1a->count++]);
  np->data = data;
  if( x1a->ht[h] ) x1a->ht[h]->from = &(np->next);
  np->next = x1a->ht[h];
  x1a->ht[h] = np;
  np->from = &(x1a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
char *Strsafe_find(char *key)
//char *key;
{
  int h;
  x1node *np;

  if( x1a==0 ) return 0;
  h = strhash(key) & (x1a->size-1);
  np = x1a->ht[h];
  while( np ){
    if( strcmp(np->data,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return a pointer to the (terminal or nonterminal) symbol "x".
** Create a new symbol if this is the first time "x" has been seen.
*/
struct symbol *Symbol_new(char *x)
//char *x;
{
  struct symbol *sp;

  sp = Symbol_find(x);
  if( sp==0 ){ // sp是0的话,说明 x2a 这个结构体的ht 表没有这个符号。 那就要为这个符号 创建内存
    sp = (struct symbol *)malloc( sizeof(struct symbol) );
    MemoryCheck(sp);
    sp->name = Strsafe(x);
    sp->type = isupper(*x) ? TERMINAL : NONTERMINAL;
    sp->rule = 0;
    sp->fallback = 0;
    sp->prec = -1;
    sp->assoc = UNK;
    sp->firstset = 0;
    sp->lambda = B_FALSE;
    sp->destructor = 0;
    sp->datatype = 0;
    Symbol_insert(sp,sp->name);
  }
  return sp;
}

/* Compare two symbols for working purposes
**
** Symbols that begin with upper case letters (terminals or tokens)
** must sort before symbols that begin with lower case letters
** (non-terminals).  Other than that, the order does not matter.
**
** We find experimentally that leaving the symbols in their original
** order (the order they appeared in the grammar file) gives the
** smallest parser tables in SQLite.
*/ // 这个函数结合qsort函数一起使用,首先,明确下,大写的字符跟小写的字符是混合在一起的。 我们需要做的事,就是将大写小写分开,然后大写集合里面跟小写集合里面的原由顺序又不被破坏。。查阅ascii表,大写字母的ascii码比小写字母的ascii码 要小
int Symbolcmpp(struct symbol **a, struct symbol **b){ // 所以通过这个函数实现。
  int i1 = (**a).index + 10000000*((**a).name[0]>'Z');
  int i2 = (**b).index + 10000000*((**b).name[0]>'Z');
  return i1-i2; // 简单说下该算法,让每一个符号的第一个字母跟大写的"Z"进行比较,如果ASCII值比大写"Z"还大,说明是小写字母,否则是大写字母。当是小写字母的时候,得到"真"值,把值放大一千倍倍,所以小写字母的数值都很大了。
} // return < 0, a将被排在b前面.. return > 0, a将被排在b后面..  ==> 所以当a小写的时候,b大写的时候,i1-i2>0. 所以a将排在b后面,所以小写都在后面,大写都在前面

/* There is one instance of the following structure for each
** associative array of type "x2".
*/
struct s_x2 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x2node *tbl;  /* The data stored here */
  struct s_x2node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x2".
*/
typedef struct s_x2node {
  struct symbol *data;                  /* The data */
  char *key;                   /* The key */
  struct s_x2node *next;   /* Next entry with the same hash */
  struct s_x2node **from;  /* Previous link */
} x2node;

/* There is only one instance of the array, which is the following */
static struct s_x2 *x2a;

/* Allocate a new associative array */
void Symbol_init(){
  if( x2a ) return;
  x2a = (struct s_x2*)malloc( sizeof(struct s_x2) );
  if( x2a ){
    x2a->size = 128;
    x2a->count = 0;
    x2a->tbl = (x2node*)malloc( 
      (sizeof(x2node) + sizeof(x2node*))*128 ); // TODO 疑问同x1a
    if( x2a->tbl==0 ){
      free(x2a);
      x2a = 0;
    }else{
      int i;
      x2a->ht = (x2node**)&(x2a->tbl[128]);
      for(i=0; i<128; i++) x2a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Symbol_insert(data,key)
struct symbol *data;
char *key;
{
  x2node *np;
  int h;
  int ph;

  if( x2a==0 ) return 0;
  ph = strhash(key); // 见简单的3919行的代码,hash函数 // 难度在于,:由于strhash函数返回的哈希值的大小范围是无法预知的。
  h = ph & (x2a->size-1); // x2a->size-1的值是127,也就是111111, 这就是 位与 操作,保证了h的值控制在[0,127]之间
  np = x2a->ht[h];
  while( np ){
    if( strcmp(np->key,key)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0; // 说明符号已经存在了,无需要再添加了
    }
    np = np->next;
  }
  if( x2a->count>=x2a->size ){
    /* Need to make the hash table bigger */
    int i,size;
    struct s_x2 array;
    array.size = size = x2a->size*2;
    array.count = x2a->count;
    array.tbl = (x2node*)malloc(
      (sizeof(x2node) + sizeof(x2node*))*size );
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x2node**)&(array.tbl[size]);
    for(i=0; i<size; i++) array.ht[i] = 0;
    for(i=0; i<x2a->count; i++){
      x2node *oldnp, *newnp;
      oldnp = &(x2a->tbl[i]);
      h = strhash(oldnp->key) & (size-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->key = oldnp->key;
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x2a->tbl);
    *x2a = array;
  }
  /* Insert the new data */ // 注意写入数据的时候,hash在同一个key上有冲突的时候 用的是 链表解决方法
  h = ph & (x2a->size-1);
  np = &(x2a->tbl[x2a->count++]);
  np->key = key;
  np->data = data;
  if( x2a->ht[h] ) x2a->ht[h]->from = &(np->next);
  np->next = x2a->ht[h];
  x2a->ht[h] = np;
  np->from = &(x2a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct symbol *Symbol_find(char *key)
//char *key;
{
  int h;
  x2node *np;

  if( x2a==0 ) return 0; // x2a 装备符号,包括终结符跟非终结符
  h = strhash(key) & (x2a->size-1);
  np = x2a->ht[h]; // x2a 是一个hash表,每个key对应的value是一个链条。 这就是常用的 hash+link算法。
  while( np ){
    if( strcmp(np->key,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return the n-th data.  Return NULL if n is out of range. */
struct symbol *Symbol_Nth(n)
int n;
{
  struct symbol *data;
  if( x2a && n>0 && n<=x2a->count ){
    data = x2a->tbl[n-1].data;
  }else{
    data = 0;
  }
  return data;
}

/* Return the size of the array */
int Symbol_count()
{
  return x2a ? x2a->count : 0;
}

/* Return an array of pointers to all data in the table.
** The array is obtained from malloc.  Return NULL if memory allocation
** problems, or if the array is empty. */
struct symbol **Symbol_arrayof()
{
  struct symbol **array;
  int i,size;
  if( x2a==0 ) return 0;
  size = x2a->count;
  array = (struct symbol **)malloc( sizeof(struct symbol *)*size );
  if( array ){
    for(i=0; i<size; i++) array[i] = x2a->tbl[i].data;
  }
  return array;
}

/* Compare two configurations */
int Configcmp(struct config *a,struct config *b)
//struct config *a;
//struct config *b;
{
  int x;
  x = a->rp->index - b->rp->index; // 产生式的序号index...先比较index,就是产生式的具体下标值
  if( x==0 ) x = a->dot - b->dot; // 再比较dot
  return x;
}

/* Compare two states */
PRIVATE int statecmp(struct config *a,struct config *b)
//struct config *a;
//struct config *b;
{
  int rc;
  for(rc=0; rc==0 && a && b;  a=a->bp, b=b->bp){
    rc = a->rp->index - b->rp->index;
    if( rc==0 ) rc = a->dot - b->dot;
  }
  if( rc==0 ){
    if( a ) rc = 1;
    if( b ) rc = -1;
  }
  return rc;
}

/* Hash a state */
PRIVATE int statehash(struct config *a)
//struct config *a;
{
  int h=0;
  while( a ){
    h = h*571 + a->rp->index*37 + a->dot;
    a = a->bp;
  }
  return h;
}

/* Allocate a new state structure */
struct state *State_new()
{
  struct state *new;
  new = (struct state *)malloc( sizeof(struct state) );
  MemoryCheck(new);
  return new;
}

/* There is one instance of the following structure for each
** associative array of type "x3".
*/
struct s_x3 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x3node *tbl;  /* The data stored here */
  struct s_x3node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x3".
*/
typedef struct s_x3node {
  struct state *data;                  /* The data */
  struct config *key;                   /* The key */
  struct s_x3node *next;   /* Next entry with the same hash */
  struct s_x3node **from;  /* Previous link */
} x3node;

/* There is only one instance of the array, which is the following */
static struct s_x3 *x3a;

/* Allocate a new associative array */
void State_init(){
  if( x3a ) return;
  x3a = (struct s_x3*)malloc( sizeof(struct s_x3) );
  if( x3a ){
    x3a->size = 128;
    x3a->count = 0;
    x3a->tbl = (x3node*)malloc( 
      (sizeof(x3node) + sizeof(x3node*))*128 );
    if( x3a->tbl==0 ){
      free(x3a);
      x3a = 0;
    }else{
      int i;
      x3a->ht = (x3node**)&(x3a->tbl[128]);
      for(i=0; i<128; i++) x3a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int State_insert(struct state *data,struct config *key)
//struct state *data;
//struct config *key;
{
  x3node *np;
  int h;
  int ph;

  if( x3a==0 ) return 0;
  ph = statehash(key);
  h = ph & (x3a->size-1);
  np = x3a->ht[h];
  while( np ){
    if( statecmp(np->key,key)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x3a->count>=x3a->size ){
    /* Need to make the hash table bigger */
    int i,size;
    struct s_x3 array;
    array.size = size = x3a->size*2;
    array.count = x3a->count;
    array.tbl = (x3node*)malloc(
      (sizeof(x3node) + sizeof(x3node*))*size );
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x3node**)&(array.tbl[size]);
    for(i=0; i<size; i++) array.ht[i] = 0;
    for(i=0; i<x3a->count; i++){
      x3node *oldnp, *newnp;
      oldnp = &(x3a->tbl[i]);
      h = statehash(oldnp->key) & (size-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->key = oldnp->key;
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x3a->tbl);
    *x3a = array;
  }
  /* Insert the new data */
  h = ph & (x3a->size-1);
  np = &(x3a->tbl[x3a->count++]);
  np->key = key;
  np->data = data;
  if( x3a->ht[h] ) x3a->ht[h]->from = &(np->next);
  np->next = x3a->ht[h];
  x3a->ht[h] = np;
  np->from = &(x3a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct state *State_find(struct config *key) // x3a存储状态state。
//struct config *key;
{
  int h;
  x3node *np;

  if( x3a==0 ) return 0;
  h = statehash(key) & (x3a->size-1);
  np = x3a->ht[h];
  while( np ){
    if( statecmp(np->key,key)==0 ) break;
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Return an array of pointers to all data in the table.
** The array is obtained from malloc.  Return NULL if memory allocation
** problems, or if the array is empty. */
struct state **State_arrayof() // x3a存储state
{
  struct state **array;
  int i,size;
  if( x3a==0 ) return 0;
  size = x3a->count;
  array = (struct state **)malloc( sizeof(struct state *)*size );
  if( array ){
    for(i=0; i<size; i++) array[i] = x3a->tbl[i].data;
  }
  return array;
}

/* Hash a configuration */
PRIVATE int confighash(struct config *a)
//struct config *a;
{
  int h=0;
  h = h*571 + a->rp->index*37 + a->dot;
  return h;
}

/* There is one instance of the following structure for each
** associative array of type "x4".
*/
struct s_x4 {
  int size;               /* The number of available slots. */
                          /*   Must be a power of 2 greater than or */
                          /*   equal to 1 */
  int count;              /* Number of currently slots filled */
  struct s_x4node *tbl;  /* The data stored here */
  struct s_x4node **ht;  /* Hash table for lookups */
};

/* There is one instance of this structure for every data element
** in an associative array of type "x4".
*/
typedef struct s_x4node {
  struct config *data;                  /* The data */
  struct s_x4node *next;   /* Next entry with the same hash */
  struct s_x4node **from;  /* Previous link */
} x4node;

/* There is only one instance of the array, which is the following */
static struct s_x4 *x4a;

/* Allocate a new associative array */
void Configtable_init(){ // x4a 装备 项目config,这货跟之前的x1a x2a x3a 是一样的
  if (x4a) return;
  x4a = (struct s_x4 *) malloc(sizeof(struct s_x4));
  if (x4a) {
    x4a->size = 64;
    x4a->count = 0;
    x4a->tbl = (x4node *) malloc(
            (sizeof(x4node) + sizeof(x4node *)) * 64);
    if (x4a->tbl == 0) {
      free(x4a);
      x4a = 0;
    } else {
      int i;
      x4a->ht = (x4node **) &(x4a->tbl[64]);
      for (i = 0; i < 64; i++) x4a->ht[i] = 0;
    }
  }
}
/* Insert a new record into the array.  Return TRUE if successful.
** Prior data with the same key is NOT overwritten */
int Configtable_insert(struct config *data)
//struct config *data;
{
  x4node *np;
  int h;
  int ph;

  if( x4a==0 ) return 0;
  ph = confighash(data);
  h = ph & (x4a->size-1);
  np = x4a->ht[h];
  while( np ){
    if( Configcmp(np->data,data)==0 ){
      /* An existing entry with the same key is found. */
      /* Fail because overwrite is not allows. */
      return 0;
    }
    np = np->next;
  }
  if( x4a->count>=x4a->size ){
    /* Need to make the hash table bigger */
    int i,size;
    struct s_x4 array;
    array.size = size = x4a->size*2;
    array.count = x4a->count;
    array.tbl = (x4node*)malloc(
      (sizeof(x4node) + sizeof(x4node*))*size );
    if( array.tbl==0 ) return 0;  /* Fail due to malloc failure */
    array.ht = (x4node**)&(array.tbl[size]);
    for(i=0; i<size; i++) array.ht[i] = 0;
    for(i=0; i<x4a->count; i++){
      x4node *oldnp, *newnp;
      oldnp = &(x4a->tbl[i]);
      h = confighash(oldnp->data) & (size-1);
      newnp = &(array.tbl[i]);
      if( array.ht[h] ) array.ht[h]->from = &(newnp->next);
      newnp->next = array.ht[h];
      newnp->data = oldnp->data;
      newnp->from = &(array.ht[h]);
      array.ht[h] = newnp;
    }
    free(x4a->tbl);
    *x4a = array;
  }
  /* Insert the new data */
  h = ph & (x4a->size-1);
  np = &(x4a->tbl[x4a->count++]);
  np->data = data;
  if( x4a->ht[h] ) x4a->ht[h]->from = &(np->next);
  np->next = x4a->ht[h];
  x4a->ht[h] = np;
  np->from = &(x4a->ht[h]);
  return 1;
}

/* Return a pointer to data assigned to the given key.  Return NULL
** if no such key. */
struct config *Configtable_find(struct config *key) // 诸如Configtable_find Symbol_find函数,它们的实现机制都是 hash表定位+链条处理key冲突的算法。
//struct config *key;
{
  int h;
  x4node *np;

  if( x4a==0 ) return 0;
  h = confighash(key) & (x4a->size-1);
  np = x4a->ht[h];
  while( np ){
    if( Configcmp(np->data,key)==0 ) break; // Configcmp函数,就先比较产生式的index,再比较dot小黑点的位置
    np = np->next;
  }
  return np ? np->data : 0;
}

/* Remove all data from the table.  Pass each data to the function "f"
** as it is removed.  ("f" may be null to avoid this step.) */
void Configtable_clear(int(*f)(/* struct config * */))
//int(*f)(/* struct config * */);
{
  int i;
  if( x4a==0 || x4a->count==0 ) return;
  if( f ) for(i=0; i<x4a->count; i++) (*f)(x4a->tbl[i].data); // 由于f是0,这步不走
  for(i=0; i<x4a->size; i++) x4a->ht[i] = 0;
  x4a->count = 0;
  return;
}
