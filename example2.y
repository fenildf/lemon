%include {
#include <iostream>
#include "example2.h"
using namespace std;
struct Token {
    int value;
     unsigned n;

};
}


%token_type { Token }
%default_type {Token}
%left PLUS MINUS.
%left DIVIDE TIMES.

%syntax_error {
cout << " Syntax error!\n" << endl;
exit(1);
}

program ::= expr(A). {
                            cout << "Result.value = " << A.value << "\n" << endl;
                             cout << "Result.n = " << A.n << "\n" << endl;
                            }

expr(A) ::= expr(B) MINUS expr(C). { A.value = B.value - C.value; }
expr(A) ::= expr(B) PLUS expr(C). { A.value = B.value + C.value; }
expr(A) ::= expr(B) TIMES expr(C). { A.value = B.value * C.value; }
expr(A) ::= expr(B) DIVIDE expr(C). { A.value = B.value / C.value; }

expr(A) ::= NUM(B). { A.value = B.value; }
%code{
    int main() {
        FILE *f;
        f = fopen("re.txt","w");
        ParseTrace(f,"");
        void* pParser = ParseAlloc(malloc);
        struct Token t0,t1;
        t0.value=4;
        t1.value=3;
        Parse(pParser, NUM, t0);
        Parse(pParser, PLUS, t0);
        Parse(pParser, NUM, t1);
        Parse(pParser, TIMES, t0);
        Parse(pParser, NUM, t1);
        Parse(pParser, 0, t0);
        ParseFree(pParser, free);

        ParseTrace(NULL,"");
        int  flag = fclose(f);
    }
}
