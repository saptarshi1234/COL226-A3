val space  = " "
%%
%name Bool

%term  
  NOT  | AND  | OR  | XOR  | EQUALS  
| IMPLIES  | IF  | THEN  | ELSE  
| PLUS | MINUS | TIMES | NEGATE
| LPAREN  | RPAREN  
| ID  | CONST 
| TERM  | EOF

%nonterm  
  start  
| program  
| statement  
| expression  
| formula  


%pos int

%eop EOF
%noshift EOF

(* header *)

%right IF THEN ELSE
%right IMPLIES
%left  AND OR XOR EQUALS 
%right NOT

%nonassoc LPAREN RPAREN

%start start

%verbose

%%


start       :   program (program @ ["start"])
program     :   statement program (statement @ program @ ["program"]) | ([])
statement   :   formula TERM (formula @ TERM :: ["statement"])

formula     :   IF formula THEN formula ELSE formula (IF :: formula1 @ THEN :: formula2 @ ELSE :: formula3 @ ["formula"])
            |   formula IMPLIES formula (formula1 @ IMPLIES :: formula2 @ ["formula"])
            |   LPAREN formula RPAREN (LPAREN :: formula @ RPAREN :: ["formula"])
            |   formula AND formula (formula1 @ AND :: formula2 @ ["formula"] )
            |   formula OR formula (formula1 @ OR :: formula2 @ ["formula"])
            |   formula XOR formula (formula1 @ XOR :: formula2 @ ["formula"])
            |   formula EQUALS formula (formula1 @ EQUALS :: formula2 @ ["formula"])
            |   NOT formula (NOT :: formula @ ["formula"])
            |   ID (ID :: ["formula"])
            |   CONST (CONST :: ["formula"])
            