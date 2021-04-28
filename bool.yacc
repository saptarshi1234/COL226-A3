val space  = " "
type loc = int * (int * int)
%%
%name Bool

%term  
  NOT of int * (int * int) | AND of int * (int * int) | OR of int * (int * int) | XOR of int * (int * int) | EQUALS of int * (int * int) | IMPLIES of int * (int * int) 
| IF of int * (int * int) | THEN of int * (int * int) | ELSE of int * (int * int)| FI of int * (int * int) 
| PLUS of int * (int * int)| MINUS of int * (int * int)| TIMES of int * (int * int)| NEGATE of int * (int * int)
| LESSTHAN of int * (int * int)| GREATERTHAN of int * (int * int)
| LPAREN of int * (int * int) | RPAREN of int * (int * int) 
| ID of (int * (int * int))*string | INTCONST of (int * (int * int))*int | BOOLCONST of (int * (int * int))*bool 
| LET of int * (int * int)| IN of int * (int * int)| END of int * (int * int)| ASSIGN of int * (int * int)
| FN of int * (int * int)| FUN of int * (int * int)| INT of int * (int * int)| BOOL of int * (int * int)| ARROWTYP of int * (int * int)| ARROWDEF of int * (int * int)| COLON of int * (int * int)
| TERM of int * (int * int) 
| EOF

%nonterm  
  start of AST.exp list 
| program of AST.exp list
| formula of AST.exp
| expression of AST.exp
| lambda of AST.exp
| typ of AST.typ

%pos int

%eop EOF
%noshift EOF

(* header *)

%right LET IN END ASSIGN
%right ARROWDEF COLON FN FUN ARROWTYP

%right IF THEN ELSE FI
%right IMPLIES
%left  AND OR XOR 
%left  EQUALS 
%left  LESSTHAN GREATERTHAN (* TODO *)
%left  PLUS MINUS
%left  TIMES
%right NOT NEGATE

%nonassoc LPAREN RPAREN

%start start

%verbose

%%
start       :   program (program)
program     :   expression TERM program (expression :: program) | expression ([expression])
expression  :   formula (formula) 
            |   FUN ID LPAREN ID COLON typ RPAREN COLON typ ARROWDEF formula (AST.FunctionExp((AST.VarExp ID1), (AST.VarExp ID2), typ1, typ2, formula))

typ         :   INT (AST.Int) | BOOL (AST.Bool) | typ ARROWTYP typ (AST.Arrow(typ1, typ2)) | LPAREN typ RPAREN (typ)
lambda      :   FN LPAREN ID COLON typ RPAREN COLON typ ARROWDEF formula (AST.LambdaExp((AST.VarExp ID), typ1, typ2, formula))

formula     :   IF formula THEN formula ELSE formula FI (AST.CondExp(formula1, formula2, formula3))
            |   LET ID ASSIGN formula IN formula END (AST.LetExp( (AST.VarExp ID), formula1, formula2))
            |   lambda (lambda)
            |   LPAREN formula formula RPAREN (AST.AppExp(formula1, formula2))

            |   LPAREN formula RPAREN (formula1)

            |   formula IMPLIES formula (AST.BinExp(AST.Implies(IMPLIES), formula1, formula2))
            |   formula AND formula (AST.BinExp(AST.And(AND), formula1, formula2))
            |   formula OR formula (AST.BinExp(AST.Or(OR), formula1, formula2))
            |   formula XOR formula (AST.BinExp(AST.Xor(XOR), formula1, formula2))
            |   formula EQUALS formula (AST.BinExp(AST.Equals(EQUALS), formula1, formula2))

            |   formula PLUS formula (AST.BinExp(AST.Plus(PLUS), formula1, formula2))
            |   formula MINUS formula (AST.BinExp(AST.Minus(MINUS), formula1, formula2))
            |   formula TIMES formula (AST.BinExp(AST.Times(TIMES), formula1, formula2))

            |   formula GREATERTHAN formula (AST.BinExp(AST.GreaterThan(GREATERTHAN), formula1, formula2))
            |   formula LESSTHAN formula (AST.BinExp(AST.LessThan(LESSTHAN), formula1, formula2))

			      |	  NEGATE formula (AST.UnaryExp(AST.Negate(NEGATE), formula))            
            |   NOT formula (AST.UnaryExp(AST.Not(NOT), formula))
            |   ID (AST.VarExp(ID))
            |   BOOLCONST (AST.BoolExp(BOOLCONST))
            |   INTCONST (AST.NumExp(INTCONST))
