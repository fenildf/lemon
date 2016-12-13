%include {
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

}

%token_type { Token }
%default_type { Token }

%token_destructor { token_destructor($$);}

%left PLUS MINUS.
%left DIVIDE TIMES.
%right POW NOT.

%parse_accept {
    printf("paring complete!\n");
}

%syntax_error {
cout << " Syntax error!\n" << endl;
exit(1);
}

main ::= in.
in ::= .
in ::= in NEWLINE.
in ::= in program NEWLINE.

program ::= NAME(A) EQ expr(B).
            {
                if(A.symt->funcptr || A.symt->funcptr2)
                {
                    cout << A.symt->name << "is a function! must type in right side!\n" << endl;
                    A.symt->value = 0.0;
                } else {
                    A.symt->value = B.value;
                }
            } /* end of Name */
program ::= expr(A). { cout << "= " << A.value << "\n" <<endl;}

expr(A) ::= expr(B) MINUS expr(C). { A.value = B.value - C.value; }
expr(A) ::= expr(B) PLUS expr(C). { A.value = B.value + C.value; }
expr(A) ::= expr(B) TIMES expr(C). { A.value = B.value * C.value; }
expr(A) ::= expr(B) DIVIDE expr(C). { A.value = B.value / C.value; }



expr(A) ::= expr(B) POW expr(C). {A.value = pow(B.value, C.value);}

expr(A) ::= MINUS expr(B). [NOT] {A.value = - B.value;}

expr(A) ::= LP expr(B) RP. {A.value=B.value;}
expr(A) ::= NUM(B). { A.value = B.value; }

expr(A) ::= NAME(B). {
                        if(B.symt->funcptr || B.symt->funcptr2)
                        {
                            cout << B.symt->name << " is a function! Must type like " <<
                            B.symt->name << "(number) !" <<endl;
                            A.value=0.0;
                        } else{
                            A.value=B.symt->value;
                        }
                    }


expr(A) ::= NAME(B) LP expr(C) RP. {
                        if(B.symt->funcptr)
                        {
                           A.value = (B.symt->funcptr)(C.value);
                        } else{
                            cout << B.symt->name << " function undefined" << endl;
                             exit(1);
                        }
                    }

expr(A) ::= NAME(B) LP expr(C) COMMA expr(D) RP. {
                        if(B.symt->funcptr2)
                        {
                           A.value = (B.symt->funcptr2)(C.value,D.value);
                        } else{
                            cout << B.symt->name << "  function undefined" << endl;
                            A.value = B.symt->value;
                        }
                    }
%code{

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
}
