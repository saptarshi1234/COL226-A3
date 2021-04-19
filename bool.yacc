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
| FN | FUN | INT | BOOL | ARROW | COLON
| TERM  | EOF

%nonterm  
  start of AST.exp list 
| program of AST.exp list
| statement of AST.exp
| formula of AST.exp
| expression of AST.exp
| lambda of AST.exp
| typ of AST.typ

%pos int

%eop EOF
%noshift EOF

(* header *)

%nonassoc LET IN END
%right ARROW COLON FN

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
program     :   statement program (statement :: program) | expression ([expression])
statement   :   expression TERM (expression)
expression  :   formula (formula) 
            |   FUN ID LPAREN ID COLON typ RPAREN COLON typ ARROW formula (AST.FunctionExp((AST.VarExp ID1), (AST.VarExp ID2), typ1, typ2, formula))

typ         :   INT (AST.Int) | BOOL (AST.Bool) | typ ARROW typ (AST.Arrow(typ1, typ2)) | LPAREN typ RPAREN (typ)
lambda      :   FN LPAREN ID COLON typ RPAREN COLON typ ARROW formula (AST.LambdaExp((AST.VarExp ID), typ1, typ2, formula))

formula     :   IF formula THEN formula ELSE formula FI (AST.CondExp(formula1, formula2, formula3))
            |   LET ID ASSIGN formula IN formula END (AST.LetExp( (AST.VarExp ID), formula1, formula2))
            |   lambda (lambda)
            |   LPAREN lambda formula RPAREN (AST.AppExp(lambda, formula))
            |   LPAREN ID formula RPAREN (AST.AppExp((AST.VarExp ID), formula))
            |   lambda formula (AST.AppExp(lambda, formula))
            |   ID formula (AST.AppExp((AST.VarExp ID), formula))
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
