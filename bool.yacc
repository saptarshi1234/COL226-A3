val space  = " "
%%
%name Bool

%term  
  NOT  | AND  | OR  | XOR  | EQUALS  | IMPLIES  
| IF  | THEN  | ELSE | FI  
| PLUS | MINUS | TIMES | NEGATE
| LESSTHAN | GREATERTHAN
| LPAREN  | RPAREN  
| ID of string | INTCONST of int | BOOLCONST of bool 
| LET | IN | END | ASSIGN
| TERM  | EOF

%nonterm  
  start of AST.exp list 
| program of AST.exp list
| statement of AST.exp
| formula of AST.exp


%pos int

%eop EOF
%noshift EOF

(* header *)

%nonassoc LET IN END

%right IF THEN ELSE FI
%right IMPLIES
%left  AND OR XOR 
%left  EQUALS 
%nonassoc LESSTHAN GREATERTHAN 
%left  PLUS MINUS
%left  TIMES
%right NOT NEGATE

%nonassoc LPAREN RPAREN

%start start

%verbose

%%
start       :   program (program)
program     :   statement program (statement :: program) | formula ([formula])
statement   :   formula TERM (formula)

formula     :   IF formula THEN formula ELSE formula FI (AST.CondExp(formula1, formula2, formula3))
            |   LET ID ASSIGN formula IN formula END (AST.LetExp(ID, formula1, formula2))
            |   LPAREN formula RPAREN (formula1)

            |   formula IMPLIES formula (AST.BinExp(AST.Implies, formula1, formula2))
            |   formula AND formula (AST.BinExp(AST.And, formula1, formula2))
            |   formula OR formula (AST.BinExp(AST.Or, formula1, formula2))
            |   formula XOR formula (AST.BinExp(AST.Xor, formula1, formula2))
            |   formula EQUALS formula (AST.BinExp(AST.Equals, formula1, formula2))

            |   formula PLUS formula (AST.BinExp(AST.Plus, formula1, formula2))
            |   formula MINUS formula (AST.BinExp(AST.Minus, formula1, formula2))
            |   formula TIMES formula (AST.BinExp(AST.Times, formula1, formula2))

            |   formula GREATERTHAN formula (AST.BinExp(AST.GreaterThan, formula1, formula2))
            |   formula LESSTHAN formula (AST.BinExp(AST.LessThan, formula1, formula2))
            
            |   NOT formula (AST.UnaryExp(AST.Not, formula))
            |   ID (AST.VarExp(ID))
            |   BOOLCONST (AST.BoolExp(BOOLCONST))
            |   INTCONST (AST.NumExp(INTCONST))