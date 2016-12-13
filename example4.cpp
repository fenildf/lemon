/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 1 "example4.y"

#include <iostream>
#include "example4.h"
#include <ctype.h>
#include <math.h>
#include <stdlib.h>
#include <string>
#define NUMBER 20
using namespace std;

struct Symbol {
    char *name;
    double value;
    double (*funcptr)(double);
    double (*funcptr2)(double,double);

};
union Token {
    Symbol* symt;
    double value;
};
void token_destructor(Token t) {
 // do nothing
}

#line 35 "example4.cpp"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    ParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is ParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.
**    ParseARG_SDECL     A static variable declaration for the %extra_argument
**    ParseARG_PDECL     A parameter declaration for the %extra_argument
**    ParseARG_STORE     Code to store %extra_argument into yypParser
**    ParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 20
#define YYACTIONTYPE unsigned char
#define ParseTOKENTYPE  Token 
typedef union {
  ParseTOKENTYPE yy0;
  Token yy30;
  int yy39;
} YYMINORTYPE;
#define YYSTACKDEPTH 100
#define ParseARG_SDECL
#define ParseARG_PDECL
#define ParseARG_FETCH
#define ParseARG_STORE
#define YYNSTATE 32
#define YYNRULE 17
#define YYERRORSYMBOL 14
#define YYERRSYMDT yy39
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* Next are that tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
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
static const YYACTIONTYPE yy_action[] = {
 /*     0 */    46,   46,   46,   46,   46,    7,   46,    3,   31,   25,
 /*    10 */    46,    4,   46,   18,   39,   39,   14,   12,   16,   24,
 /*    20 */    39,   20,   38,   23,   39,   11,   39,   26,   41,   41,
 /*    30 */    41,   41,   16,    9,   41,   50,    1,   13,   41,   21,
 /*    40 */    41,   38,   40,   40,   40,   40,   16,   17,   40,   15,
 /*    50 */    29,   19,   40,   38,   40,   38,   48,   48,   48,   48,
 /*    60 */    48,   38,   48,   38,   38,   38,   48,   38,   48,   38,
 /*    70 */    47,   47,   47,   47,   47,   38,   47,   38,   38,   38,
 /*    80 */    47,   38,   47,   38,   46,   46,   46,   46,   46,   38,
 /*    90 */    46,   38,    6,   25,   45,   45,   45,   45,   45,   38,
 /*   100 */    45,   38,   38,   38,   45,   38,   45,   38,   44,   44,
 /*   110 */    44,   44,   44,   38,   44,   38,   38,   38,   44,   38,
 /*   120 */    44,   38,   38,   38,   14,   12,   16,   38,   38,   38,
 /*   130 */    38,   38,   38,   38,   38,   38,   43,   43,   43,   43,
 /*   140 */    16,   38,   43,   38,   38,   38,   43,   38,   43,   38,
 /*   150 */    42,   42,   42,   42,   16,   38,   42,   38,   38,   38,
 /*   160 */    42,   38,   42,   38,   10,    8,   14,   12,   16,   10,
 /*   170 */     8,   14,   12,   16,   27,   33,   28,   33,   38,   30,
 /*   180 */    38,   38,   33,   33,   38,   33,   38,   33,   38,   10,
 /*   190 */     8,   14,   12,   16,   35,   36,   35,   38,   38,   38,
 /*   200 */    38,   35,   35,   34,   35,   34,   35,   38,   38,   38,
 /*   210 */    34,   34,   38,   34,   38,   34,   38,   10,    8,   14,
 /*   220 */    12,   16,   10,    8,   14,   12,   16,   22,   37,   32,
 /*   230 */    38,   18,   38,   38,   38,   38,    2,    5,   38,   20,
 /*   240 */    38,   23,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */     1,    2,    3,    4,    5,   18,    7,   17,   18,   10,
 /*    10 */    11,    7,   13,    2,    1,    2,    3,    4,    5,    8,
 /*    20 */     7,   10,   19,   12,   11,   18,   13,   18,    1,    2,
 /*    30 */     3,    4,    5,   18,    7,   15,   16,   18,   11,   18,
 /*    40 */    13,   19,    1,    2,    3,    4,    5,   18,    7,   18,
 /*    50 */    18,   18,   11,   19,   13,   19,    1,    2,    3,    4,
 /*    60 */     5,   19,    7,   19,   19,   19,   11,   19,   13,   19,
 /*    70 */     1,    2,    3,    4,    5,   19,    7,   19,   19,   19,
 /*    80 */    11,   19,   13,   19,    1,    2,    3,    4,    5,   19,
 /*    90 */     7,   19,    9,   10,    1,    2,    3,    4,    5,   19,
 /*   100 */     7,   19,   19,   19,   11,   19,   13,   19,    1,    2,
 /*   110 */     3,    4,    5,   19,    7,   19,   19,   19,   11,   19,
 /*   120 */    13,   19,    1,    2,    3,    4,    5,   19,    7,   19,
 /*   130 */    19,   19,   11,   19,   13,   19,    1,    2,    3,    4,
 /*   140 */     5,   19,    7,   19,   19,   19,   11,   19,   13,   19,
 /*   150 */     1,    2,    3,    4,    5,   19,    7,   19,   19,   19,
 /*   160 */    11,   19,   13,   19,    1,    2,    3,    4,    5,    1,
 /*   170 */     2,    3,    4,    5,   11,    0,   13,    2,   19,   11,
 /*   180 */    19,   19,    7,    8,   19,   10,   19,   12,   19,    1,
 /*   190 */     2,    3,    4,    5,    0,    7,    2,   19,   19,   19,
 /*   200 */    19,    7,    8,    0,   10,    2,   12,   19,   19,   19,
 /*   210 */     7,    8,   19,   10,   19,   12,   19,    1,    2,    3,
 /*   220 */     4,    5,    1,    2,    3,    4,    5,   11,    7,    0,
 /*   230 */    19,    2,   19,   19,   19,   19,    7,    8,   19,   10,
 /*   240 */    19,   12,
};
#define YY_SHIFT_USE_DFLT (-2)
static const short yy_shift_ofst[] = {
 /*     0 */   175,  229,  203,    4,  194,   83,   11,  188,   11,  121,
 /*    10 */    11,   13,   11,   41,   11,   27,   11,  149,   11,  135,
 /*    20 */    11,  216,  107,   93,   -1,   11,  163,   69,   11,  168,
 /*    30 */    55,  221,
};
#define YY_REDUCE_USE_DFLT (-14)
static const signed char yy_reduce_ofst[] = {
 /*     0 */    20,  -10,  -14,  -14,  -14,  -14,  -13,  -14,   15,  -14,
 /*    10 */     7,  -14,   19,  -14,   31,  -14,   29,  -14,   33,  -14,
 /*    20 */    21,  -14,  -14,  -14,  -14,    9,  -14,  -14,   32,  -14,
 /*    30 */   -14,  -14,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */    49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
 /*    10 */    49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
 /*    20 */    49,   49,   49,   49,   49,   49,   49,   49,   49,   49,
 /*    30 */    49,   49,
};
#define YY_SZ_ACTTAB (sizeof(yy_action)/sizeof(yy_action[0]))

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammer, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  int stateno;       /* The state-number */
  int major;         /* The major token value.  This is the code
                     ** number for the token at this stack level */
  YYMINORTYPE minor; /* The user-supplied minor token value.  This
                     ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
  int yyerrcnt;                 /* Shifts left before out of the error */
  ParseARG_SDECL                /* A place to hold %extra_argument */
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char *yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void ParseTrace(FILE *TraceFILE, char *zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "PLUS",          "MINUS",         "DIVIDE",      
  "TIMES",         "POW",           "NOT",           "NEWLINE",     
  "NAME",          "EQ",            "LP",            "RP",          
  "NUM",           "COMMA",         "error",         "main",        
  "in",            "program",       "expr",        
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "main ::= in",
 /*   1 */ "in ::=",
 /*   2 */ "in ::= in NEWLINE",
 /*   3 */ "in ::= in program NEWLINE",
 /*   4 */ "program ::= NAME EQ expr",
 /*   5 */ "program ::= expr",
 /*   6 */ "expr ::= expr MINUS expr",
 /*   7 */ "expr ::= expr PLUS expr",
 /*   8 */ "expr ::= expr TIMES expr",
 /*   9 */ "expr ::= expr DIVIDE expr",
 /*  10 */ "expr ::= expr POW expr",
 /*  11 */ "expr ::= MINUS expr",
 /*  12 */ "expr ::= LP expr RP",
 /*  13 */ "expr ::= NUM",
 /*  14 */ "expr ::= NAME",
 /*  15 */ "expr ::= NAME LP expr RP",
 /*  16 */ "expr ::= NAME LP expr COMMA expr RP",
};
#endif /* NDEBUG */

/*
** This function returns the symbolic name associated with a token
** value.
*/
const char *ParseTokenName(int tokenType){
#ifndef NDEBUG
  if( tokenType>0 && tokenType<(sizeof(yyTokenName)/sizeof(yyTokenName[0])) ){
    return yyTokenName[tokenType];
  }else{
    return "Unknown";
  }
#else
  return "";
#endif
}

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to Parse and ParseFree.
*/
void *ParseAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(YYCODETYPE yymajor, YYMINORTYPE *yypminor){
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
    case 1:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
    case 8:
    case 9:
    case 10:
    case 11:
    case 12:
    case 13:
#line 31 "example4.y"
{ token_destructor((yypminor->yy0));}
#line 412 "example4.cpp"
      break;
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor( yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from ParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void ParseFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
  (*freeProc)((void*)pParser);
}

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  int iLookAhead            /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  /* if( pParser->yyidx<0 ) return YY_NO_ACTION;  */
  i = yy_shift_ofst[stateno];
  if( i==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  if( iLookAhead==YYNOCODE ){
    return YY_NO_ACTION;
  }
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
#ifdef YYFALLBACK
    int iFallback;            /* Fallback token */
    if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
           && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
           yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
      }
#endif
      return yy_find_shift_action(pParser, iFallback);
    }
#endif
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  int iLookAhead            /* The look-ahead token */
){
  int i;
  /* int stateno = pParser->yystack[pParser->yyidx].stateno; */
 
  i = yy_reduce_ofst[stateno];
  if( i==YY_REDUCE_USE_DFLT ){
    return yy_default[stateno];
  }
  if( iLookAhead==YYNOCODE ){
    return YY_NO_ACTION;
  }
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer ot the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
  if( yypParser->yyidx>=YYSTACKDEPTH ){
     ParseARG_FETCH;
     yypParser->yyidx--;
#ifndef NDEBUG
     if( yyTraceFILE ){
       fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
     }
#endif
     while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
     /* Here code is inserted which will execute if the parser
     ** stack every overflows */
     ParseARG_STORE; /* Suppress warning about unused %extra_argument var */
     return;
  }
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = yyNewState;
  yytos->major = yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 15, 1 },
  { 16, 0 },
  { 16, 2 },
  { 16, 3 },
  { 17, 3 },
  { 17, 1 },
  { 18, 3 },
  { 18, 3 },
  { 18, 3 },
  { 18, 3 },
  { 18, 3 },
  { 18, 2 },
  { 18, 3 },
  { 18, 1 },
  { 18, 1 },
  { 18, 4 },
  { 18, 6 },
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  ParseARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<sizeof(yyRuleName)/sizeof(yyRuleName[0]) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

#ifndef NDEBUG
  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  */
  memset(&yygotominor, 0, sizeof(yygotominor));
#endif

  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 4:
#line 52 "example4.y"
{
                if(yymsp[-2].minor.yy0.symt->funcptr || yymsp[-2].minor.yy0.symt->funcptr2)
                {
                    cout << yymsp[-2].minor.yy0.symt->name << "is a function! must type in right side!\n" << endl;
                    yymsp[-2].minor.yy0.symt->value = 0.0;
                } else {
                    yymsp[-2].minor.yy0.symt->value = yymsp[0].minor.yy30.value;
                }
              yy_destructor(9,&yymsp[-1].minor);
}
#line 665 "example4.cpp"
        break;
      case 5:
#line 61 "example4.y"
{ cout << "= " << yymsp[0].minor.yy30.value << "\n" <<endl;}
#line 670 "example4.cpp"
        break;
      case 6:
#line 63 "example4.y"
{ yygotominor.yy30.value = yymsp[-2].minor.yy30.value - yymsp[0].minor.yy30.value;   yy_destructor(2,&yymsp[-1].minor);
}
#line 676 "example4.cpp"
        break;
      case 7:
#line 64 "example4.y"
{ yygotominor.yy30.value = yymsp[-2].minor.yy30.value + yymsp[0].minor.yy30.value;   yy_destructor(1,&yymsp[-1].minor);
}
#line 682 "example4.cpp"
        break;
      case 8:
#line 65 "example4.y"
{ yygotominor.yy30.value = yymsp[-2].minor.yy30.value * yymsp[0].minor.yy30.value;   yy_destructor(4,&yymsp[-1].minor);
}
#line 688 "example4.cpp"
        break;
      case 9:
#line 66 "example4.y"
{ yygotominor.yy30.value = yymsp[-2].minor.yy30.value / yymsp[0].minor.yy30.value;   yy_destructor(3,&yymsp[-1].minor);
}
#line 694 "example4.cpp"
        break;
      case 10:
#line 70 "example4.y"
{yygotominor.yy30.value = pow(yymsp[-2].minor.yy30.value, yymsp[0].minor.yy30.value);  yy_destructor(5,&yymsp[-1].minor);
}
#line 700 "example4.cpp"
        break;
      case 11:
#line 72 "example4.y"
{yygotominor.yy30.value = - yymsp[0].minor.yy30.value;  yy_destructor(2,&yymsp[-1].minor);
}
#line 706 "example4.cpp"
        break;
      case 12:
#line 74 "example4.y"
{yygotominor.yy30.value=yymsp[-1].minor.yy30.value;  yy_destructor(10,&yymsp[-2].minor);
  yy_destructor(11,&yymsp[0].minor);
}
#line 713 "example4.cpp"
        break;
      case 13:
#line 75 "example4.y"
{ yygotominor.yy30.value = yymsp[0].minor.yy0.value; }
#line 718 "example4.cpp"
        break;
      case 14:
#line 77 "example4.y"
{
                        if(yymsp[0].minor.yy0.symt->funcptr || yymsp[0].minor.yy0.symt->funcptr2)
                        {
                            cout << yymsp[0].minor.yy0.symt->name << " is a function! Must type like " <<
                            yymsp[0].minor.yy0.symt->name << "(number) !" <<endl;
                            yygotominor.yy30.value=0.0;
                        } else{
                            yygotominor.yy30.value=yymsp[0].minor.yy0.symt->value;
                        }
                    }
#line 732 "example4.cpp"
        break;
      case 15:
#line 89 "example4.y"
{
                        if(yymsp[-3].minor.yy0.symt->funcptr)
                        {
                           yygotominor.yy30.value = (yymsp[-3].minor.yy0.symt->funcptr)(yymsp[-1].minor.yy30.value);
                        } else{
                            cout << yymsp[-3].minor.yy0.symt->name << " function undefined" << endl;
                             exit(1);
                        }
                      yy_destructor(10,&yymsp[-2].minor);
  yy_destructor(11,&yymsp[0].minor);
}
#line 747 "example4.cpp"
        break;
      case 16:
#line 99 "example4.y"
{
                        if(yymsp[-5].minor.yy0.symt->funcptr2)
                        {
                           yygotominor.yy30.value = (yymsp[-5].minor.yy0.symt->funcptr2)(yymsp[-3].minor.yy30.value,yymsp[-1].minor.yy30.value);
                        } else{
                            cout << yymsp[-5].minor.yy0.symt->name << "  function undefined" << endl;
                            yygotominor.yy30.value = yymsp[-5].minor.yy0.symt->value;
                        }
                      yy_destructor(10,&yymsp[-4].minor);
  yy_destructor(13,&yymsp[-2].minor);
  yy_destructor(11,&yymsp[0].minor);
}
#line 763 "example4.cpp"
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = yyact;
      yymsp->major = yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else if( yyact == YYNSTATE + YYNRULE + 1 ){
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  ParseARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 41 "example4.y"

cout << " Syntax error!\n" << endl;
exit(1);
#line 825 "example4.cpp"
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  ParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
#line 37 "example4.y"

    printf("paring complete!\n");
#line 848 "example4.cpp"
  ParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "ParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void Parse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  ParseTOKENTYPE yyminor       /* The value for the token */
  ParseARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
    if( yymajor==0 ) return;
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yyminorunion.yy0 = yyminor;
  yyendofinput = (yymajor==0);
  ParseARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yymajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,yymajor);
    if( yyact<YYNSTATE ){
      yy_shift(yypParser,yyact,yymajor,&yyminorunion);
      yypParser->yyerrcnt--;
      if( yyendofinput && yypParser->yyidx>=0 ){
        yymajor = 0;
      }else{
        yymajor = YYNOCODE;
      }
    }else if( yyact < YYNSTATE + YYNRULE ){
      yy_reduce(yypParser,yyact-YYNSTATE);
    }else if( yyact == YY_ERROR_ACTION ){
      int yymx;
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yymajor]);
        }
#endif
        yy_destructor(yymajor,&yyminorunion);
        yymajor = YYNOCODE;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_shift_action(yypParser,YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yymajor==0 ){
          yy_destructor(yymajor,&yyminorunion);
          yy_parse_failed(yypParser);
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      yyerrorhit = 1;
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yymajor,&yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yymajor = YYNOCODE;
#endif
    }else{
      yy_accept(yypParser);
      yymajor = YYNOCODE;
    }
  }while( yymajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}
#line 108 "example4.y"


    static int getToken(const char *z, int *tokenType) {
        int i,c;

        switch(*z) {
            case '\n' :
            {
                *tokenType = NEWLINE;
                return 1;
            }
            case '-' :
            {
                *tokenType = MINUS;
                return 1;
            }
            case '+':
            {
                *tokenType= PLUS;
                return 1;
            }
            case '*':
            {
                 *tokenType= TIMES;
                 return 1;
            }
             case '/':
            {
                 *tokenType= DIVIDE;
                 return 1;
            }
             case '^':
            {
                 *tokenType= POW;
                 return 1;
            }
             case '(':
            {
                 *tokenType= LP;
                 return 1;
            }
             case ')':
            {
                 *tokenType= RP;
                 return 1;
            }
            case '=':
            {
                 *tokenType= EQ;
                 return 1;
            }
            case ',':
            {
                 *tokenType= COMMA;
                 return 1;
            }
            case 'A':case 'a':
            case 'B':case 'b':
            case 'C':case 'c':
            case 'D':case 'd':
            case 'E':case 'e':
            case 'F':case 'f':
            case 'G':case 'g':
             case 'H':case 'h':
             case 'I':case 'i':
             case 'J':case 'j':
             case 'K':case 'k':
             case 'L':case 'l':
             case 'M':case 'm':
             case 'N':case 'n':
             case 'O':case 'o':
             case 'P':case 'p':
             case 'Q':case 'q':
             case 'R':case 'r':
             case 'S':case 's':
             case 'T':case 't':
             case 'U':case 'u':
             case 'V':case 'v':
             case 'W':case 'w':
             case 'X':case 'x':
             case 'Y':case 'y':
             case 'Z':case 'z':

             {
                  for(i=1; isalnum(z[i])||z[i]=='_';i++) {}
                  *tokenType = NAME;
                  return i;
             }
             case '0':
              case '1':
               case '2':
                case '3':
                 case '4':
                  case '5':
                   case '6':
                    case '7':
                     case '8':
                      case '9':


            {
                 for(i=1;isdigit(z[i]);i++){}
                 if(z[i]=='.' && isdigit(z[i+1]))
                 {
                    i+=2;
                    while( isdigit(z[i]))
                    {
                        i++;
                    }

                 }
                  *tokenType= NUM;
                   return i;

            }
            default  :
            {
                 *tokenType= -1;
                 return 1;
            }
        }
    }


     static char* getstring( char *z, int n) {
            char* paz;
            paz=(char *) malloc(n+1);
            if(paz==0) {
                fprintf(stderr,"out of memory\n");
                exit(1);
            }
            strncpy(paz,z,n);
            paz[n]='\0';
            return paz;

     }
     static Symbol* symlook (char *s, Symbol* symtab) {
        struct Symbol* sp;
        for( sp = symtab; sp < &symtab[NUMBER];sp++) {
            // 他已经在这里了吗?
            if(sp->name && !strcmp(sp->name,s)) {
                return sp;
            }
            //它是空的吗?
            if(!sp->name) {
                sp->name = s;
                return sp;
            }
            // 否则继续下一个
        }
        // 符号数组已经装满符号
        cout << "Too many symbol" << endl;
        exit(1);
     }
     static void addfunc(char *name, double (*func) (double x), Symbol* symtab) {
        struct Symbol* sp = symlook(name, symtab);
        sp->funcptr = func;
     }
     static void addfunc2(char *name, double (*func) (double x,double x2), Symbol* symtab) {
         struct Symbol* sp = symlook(name, symtab);
         sp->funcptr2 = func;
     }
     static double localabs(double x) {
        if(x>0.0) {
            return x;
        } else {
            return -x;
        }
     }


    int main() {
        FILE *f;
        f = fopen("re.txt","w");
        ParseTrace(f,"");
         // ==start code


        union Token* t0;
        int n;
        char *z;
        int *tokenType;
        struct Symbol symtab[NUMBER];
        for(int i=0;i<NUMBER;i++) {
            symtab[i].value=0.0;
            symtab[i].name=NULL;
            symtab[i].funcptr=NULL;
            symtab[i].funcptr2=NULL;

        }
        addfunc("exp",exp, symtab);
        addfunc("log",log, symtab);
        addfunc("sqrt",sqrt, symtab);
        addfunc("localabs",localabs, symtab);
        addfunc("hypot",localabs, symtab);
        addfunc2("hypot",hypot, symtab);

        t0 = (Token *) malloc(sizeof(Token));
        if(t0==0) {
              fprintf(stderr,"out of memory\n");
                            exit(1);
        }

        t0->value=0;
         t0->symt=NULL;

         tokenType= (int*) malloc(sizeof(int));
         if(tokenType==0) {
                       fprintf(stderr,"out of memory\n");
                                     exit(1);
                 }

                 z = (char *) malloc(1024);
                 if(z==0) {
                   fprintf(stderr,"out of memory\n");
                                                      exit(1);
                 }
        void* pParser = ParseAlloc(malloc);
         while(1) {
            gets(z);
            printf("%s\n", z);
            if(z=="") break;
            strcat(z,"\n");
            while(*z) {
                n = getToken(z,tokenType);
                if(*tokenType == NUM) {
                    char *s = getstring(z,n);
                    t0->value = atof(s);
                    printf("dd:%d\n",  t0->value);
                }
                if(*tokenType == NAME) {
                    char *s = getstring(z,n);
                    t0->symt = symlook(s,symtab);
                }
                if(*tokenType >=0) {
                    Parse(pParser, *tokenType, *t0);
                }
                z= z+n;
            }
         }
         Parse(pParser, 0, *t0);

        ParseFree(pParser, free);
  // ==end code
        ParseTrace(NULL,"");
        int  flag = fclose(f);
        return 0;
    }
#line 1251 "example4.cpp"
