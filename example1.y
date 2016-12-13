%include {
#include <iostream>
#include "example1.h"
using namespace std;

}


%token_type { int }
%left PLUS MINUS.
%left DIVIDE TIMES.

%syntax_error {
cout << " Syntax error!\n" << endl;
exit(1);
}

program ::= expr(A). {
                            cout << "Result = " << A << "\n" << endl;
                            }

expr(A) ::= expr(B) MINUS expr(C). { A = B - C; }
expr(A) ::= expr(B) PLUS expr(C). { A = B + C; }
expr(A) ::= expr(B) TIMES expr(C). { A = B * C; }
expr(A) ::= expr(B) DIVIDE expr(C). { A = B / C; }

expr(A) ::= NUM(B). { A = B; }
%code{
    int main() {

        void* pParser = ParseAlloc(malloc);

        Parse(pParser, NUM, 1);
        Parse(pParser, PLUS, 0);
        Parse(pParser, NUM, 2);

        Parse(pParser, 0, 0);
        ParseFree(pParser, free);


    }
}
