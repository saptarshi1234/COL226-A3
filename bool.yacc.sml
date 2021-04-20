functor BoolLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Bool_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
val space  = " "

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\111\000\002\000\111\000\003\000\111\000\004\000\111\000\
\\005\000\111\000\006\000\111\000\007\000\111\000\008\000\111\000\
\\009\000\111\000\010\000\111\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\017\000\111\000\018\000\111\000\019\000\111\000\
\\020\000\111\000\021\000\111\000\022\000\111\000\023\000\111\000\
\\024\000\111\000\026\000\111\000\033\000\111\000\034\000\111\000\000\000\
\\001\000\001\000\112\000\002\000\112\000\003\000\112\000\004\000\112\000\
\\005\000\112\000\006\000\112\000\007\000\112\000\008\000\112\000\
\\009\000\112\000\010\000\112\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\017\000\112\000\018\000\112\000\019\000\112\000\
\\020\000\112\000\021\000\112\000\022\000\112\000\023\000\112\000\
\\024\000\112\000\026\000\112\000\033\000\112\000\034\000\112\000\000\000\
\\001\000\001\000\016\000\002\000\027\000\003\000\026\000\004\000\025\000\
\\005\000\024\000\006\000\023\000\007\000\015\000\011\000\022\000\
\\012\000\021\000\013\000\020\000\015\000\019\000\016\000\018\000\
\\017\000\014\000\018\000\051\000\019\000\013\000\020\000\012\000\
\\021\000\011\000\022\000\010\000\026\000\009\000\000\000\
\\001\000\001\000\016\000\007\000\015\000\017\000\014\000\019\000\013\000\
\\020\000\012\000\021\000\011\000\022\000\010\000\026\000\009\000\000\000\
\\001\000\001\000\016\000\007\000\015\000\017\000\014\000\019\000\013\000\
\\020\000\012\000\021\000\011\000\022\000\010\000\026\000\009\000\
\\027\000\008\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\008\000\052\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\015\000\019\000\016\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\009\000\065\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\015\000\019\000\016\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\010\000\077\000\011\000\022\000\012\000\021\000\
\\013\000\020\000\015\000\019\000\016\000\018\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\018\000\056\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\018\000\057\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\023\000\064\000\000\000\
\\001\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\024\000\076\000\000\000\
\\001\000\017\000\030\000\000\000\
\\001\000\017\000\046\000\000\000\
\\001\000\017\000\063\000\028\000\062\000\029\000\061\000\000\000\
\\001\000\018\000\068\000\030\000\067\000\000\000\
\\001\000\018\000\072\000\030\000\067\000\000\000\
\\001\000\018\000\075\000\030\000\067\000\000\000\
\\001\000\019\000\029\000\000\000\
\\001\000\019\000\031\000\000\000\
\\001\000\019\000\047\000\000\000\
\\001\000\019\000\053\000\000\000\
\\001\000\025\000\048\000\000\000\
\\001\000\030\000\067\000\031\000\081\000\000\000\
\\001\000\030\000\067\000\031\000\082\000\000\000\
\\001\000\032\000\054\000\000\000\
\\001\000\032\000\059\000\000\000\
\\001\000\032\000\074\000\000\000\
\\001\000\032\000\078\000\000\000\
\\001\000\034\000\000\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\033\000\017\000\000\000\
\\089\000\000\000\
\\090\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\091\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\030\000\067\000\000\000\
\\095\000\000\000\
\\096\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\099\000\001\000\016\000\007\000\015\000\017\000\014\000\019\000\013\000\
\\020\000\012\000\021\000\011\000\022\000\010\000\026\000\009\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\002\000\027\000\003\000\026\000\004\000\025\000\005\000\024\000\
\\006\000\023\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\104\000\005\000\024\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\105\000\005\000\024\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\106\000\005\000\024\000\011\000\022\000\012\000\021\000\013\000\020\000\
\\015\000\019\000\016\000\018\000\000\000\
\\107\000\011\000\022\000\012\000\021\000\013\000\020\000\015\000\019\000\
\\016\000\018\000\000\000\
\\108\000\013\000\020\000\000\000\
\\109\000\013\000\020\000\000\000\
\\110\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\"
val actionRowNumbers =
"\004\000\043\000\032\000\034\000\
\\004\000\030\000\018\000\012\000\
\\019\000\058\000\059\000\057\000\
\\003\000\003\000\003\000\033\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\003\000\003\000\
\\003\000\003\000\031\000\013\000\
\\020\000\022\000\044\000\002\000\
\\005\000\056\000\000\000\001\000\
\\055\000\054\000\053\000\048\000\
\\052\000\051\000\050\000\049\000\
\\021\000\025\000\003\000\008\000\
\\009\000\047\000\003\000\026\000\
\\014\000\010\000\045\000\046\000\
\\006\000\014\000\015\000\037\000\
\\036\000\014\000\003\000\003\000\
\\016\000\014\000\027\000\017\000\
\\011\000\007\000\028\000\038\000\
\\014\000\039\000\042\000\041\000\
\\014\000\023\000\024\000\003\000\
\\003\000\040\000\035\000\029\000"
val gotoT =
"\
\\001\000\083\000\002\000\005\000\003\000\004\000\004\000\003\000\
\\005\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\026\000\003\000\004\000\004\000\003\000\005\000\002\000\
\\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\031\000\006\000\030\000\000\000\
\\004\000\032\000\006\000\001\000\000\000\
\\004\000\033\000\006\000\001\000\000\000\
\\000\000\
\\004\000\034\000\006\000\001\000\000\000\
\\004\000\035\000\006\000\001\000\000\000\
\\004\000\036\000\006\000\001\000\000\000\
\\004\000\037\000\006\000\001\000\000\000\
\\004\000\038\000\006\000\001\000\000\000\
\\004\000\039\000\006\000\001\000\000\000\
\\004\000\040\000\006\000\001\000\000\000\
\\004\000\041\000\006\000\001\000\000\000\
\\004\000\042\000\006\000\001\000\000\000\
\\004\000\043\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\047\000\006\000\001\000\000\000\
\\004\000\048\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\053\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\056\000\006\000\001\000\000\000\
\\000\000\
\\007\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\064\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\067\000\000\000\
\\004\000\068\000\006\000\001\000\000\000\
\\004\000\069\000\006\000\001\000\000\000\
\\000\000\
\\007\000\071\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\078\000\000\000\
\\000\000\
\\000\000\
\\004\000\081\000\006\000\001\000\000\000\
\\004\000\082\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 84
val numrules = 31
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | BOOLCONST of unit ->  (bool) | INTCONST of unit ->  (int)
 | ID of unit ->  (string) | typ of unit ->  (AST.typ)
 | lambda of unit ->  (AST.exp) | expression of unit ->  (AST.exp)
 | formula of unit ->  (AST.exp) | statement of unit ->  (AST.exp)
 | program of unit ->  (AST.exp list)
 | start of unit ->  (AST.exp list)
end
type svalue = MlyValue.svalue
type result = AST.exp list
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 33) => true | _ => false
val showTerminal =
fn (T 0) => "NOT"
  | (T 1) => "AND"
  | (T 2) => "OR"
  | (T 3) => "XOR"
  | (T 4) => "EQUALS"
  | (T 5) => "IMPLIES"
  | (T 6) => "IF"
  | (T 7) => "THEN"
  | (T 8) => "ELSE"
  | (T 9) => "FI"
  | (T 10) => "PLUS"
  | (T 11) => "MINUS"
  | (T 12) => "TIMES"
  | (T 13) => "NEGATE"
  | (T 14) => "LESSTHAN"
  | (T 15) => "GREATERTHAN"
  | (T 16) => "LPAREN"
  | (T 17) => "RPAREN"
  | (T 18) => "ID"
  | (T 19) => "INTCONST"
  | (T 20) => "BOOLCONST"
  | (T 21) => "LET"
  | (T 22) => "IN"
  | (T 23) => "END"
  | (T 24) => "ASSIGN"
  | (T 25) => "FN"
  | (T 26) => "FUN"
  | (T 27) => "INT"
  | (T 28) => "BOOL"
  | (T 29) => "ARROWTYP"
  | (T 30) => "ARROWDEF"
  | (T 31) => "COLON"
  | (T 32) => "TERM"
  | (T 33) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.start (fn _
 => let val  (program as program1) = program1 ()
 in (program)
end)
 in ( LrTable.NT 0, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.program program1, _, program1right)) :: ( _,
 ( MlyValue.statement statement1, statement1left, _)) :: rest671)) =>
 let val  result = MlyValue.program (fn _ => let val  (statement as 
statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in (statement :: program)
end)
 in ( LrTable.NT 1, ( result, statement1left, program1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.expression expression1, expression1left, 
expression1right)) :: rest671)) => let val  result = MlyValue.program
 (fn _ => let val  (expression as expression1) = expression1 ()
 in ([expression])
end)
 in ( LrTable.NT 1, ( result, expression1left, expression1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.expression 
expression1, expression1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (expression)
end)
 in ( LrTable.NT 2, ( result, expression1left, TERM1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.formula formula1, formula1left, 
formula1right)) :: rest671)) => let val  result = MlyValue.expression
 (fn _ => let val  (formula as formula1) = formula1 ()
 in (formula)
end)
 in ( LrTable.NT 4, ( result, formula1left, formula1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _ ::
 ( _, ( MlyValue.typ typ2, _, _)) :: _ :: _ :: ( _, ( MlyValue.typ 
typ1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  typ1 = typ1 ()
 val  typ2 = typ2 ()
 val  (formula as formula1) = formula1 ()
 in (
AST.FunctionExp((AST.VarExp ID1), (AST.VarExp ID2), typ1, typ2, formula)
)
end)
 in ( LrTable.NT 4, ( result, FUN1left, formula1right), rest671)
end
|  ( 6, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => (AST.Int))
 in ( LrTable.NT 6, ( result, INT1left, INT1right), rest671)
end
|  ( 7, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.typ (fn _ => (AST.Bool))
 in ( LrTable.NT 6, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.typ typ2, _, typ2right)) :: _ :: ( _, ( 
MlyValue.typ typ1, typ1left, _)) :: rest671)) => let val  result = 
MlyValue.typ (fn _ => let val  typ1 = typ1 ()
 val  typ2 = typ2 ()
 in (AST.Arrow(typ1, typ2))
end)
 in ( LrTable.NT 6, ( result, typ1left, typ2right), rest671)
end
|  ( 9, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.typ typ1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.typ (fn _ => let val  (typ as typ1) = typ1 ()
 in (typ)
end)
 in ( LrTable.NT 6, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: _
 :: ( _, ( MlyValue.typ typ2, _, _)) :: _ :: _ :: ( _, ( MlyValue.typ 
typ1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, 
FN1left, _)) :: rest671)) => let val  result = MlyValue.lambda (fn _
 => let val  (ID as ID1) = ID1 ()
 val  typ1 = typ1 ()
 val  typ2 = typ2 ()
 val  (formula as formula1) = formula1 ()
 in (AST.LambdaExp((AST.VarExp ID), typ1, typ2, formula))
end)
 in ( LrTable.NT 5, ( result, FN1left, formula1right), rest671)
end
|  ( 11, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.formula formula3
, _, _)) :: _ :: ( _, ( MlyValue.formula formula2, _, _)) :: _ :: ( _,
 ( MlyValue.formula formula1, _, _)) :: ( _, ( _, IF1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  
formula1 = formula1 ()
 val  formula2 = formula2 ()
 val  formula3 = formula3 ()
 in (AST.CondExp(formula1, formula2, formula3))
end)
 in ( LrTable.NT 3, ( result, IF1left, FI1right), rest671)
end
|  ( 12, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.formula 
formula2, _, _)) :: _ :: ( _, ( MlyValue.formula formula1, _, _)) :: _
 :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, LET1left, _)) :: 
rest671)) => let val  result = MlyValue.formula (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  formula1 = formula1 ()
 val  formula2 = formula2 ()
 in (AST.LetExp( (AST.VarExp ID), formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.lambda lambda1, lambda1left, lambda1right))
 :: rest671)) => let val  result = MlyValue.formula (fn _ => let val 
 (lambda as lambda1) = lambda1 ()
 in (lambda)
end)
 in ( LrTable.NT 3, ( result, lambda1left, lambda1right), rest671)
end
|  ( 14, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( MlyValue.lambda lambda1, _, _)) :: ( _, ( _
, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (lambda as lambda1) = lambda1 ()
 val  (formula as formula1) = formula1 ()
 in (AST.AppExp(lambda, formula))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula2, _, _)) :: ( _, ( MlyValue.formula formula1, _, _)) :: ( _, (
 _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  formula1 = formula1 ()
 val  formula2 = formula2 ()
 in (AST.AppExp(formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  formula1 = formula1
 ()
 in (formula1)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Implies, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.And, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Or, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Xor, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 21, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Equals, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 22, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Plus, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 23, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Minus, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 24, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.Times, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 25, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.GreaterThan, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: _
 :: ( _, ( MlyValue.formula formula1, formula1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  formula1 = 
formula1 ()
 val  formula2 = formula2 ()
 in (AST.BinExp(AST.LessThan, formula1, formula2))
end)
 in ( LrTable.NT 3, ( result, formula1left, formula2right), rest671)

end
|  ( 27, ( ( _, ( MlyValue.formula formula1, _, formula1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  (formula as formula1) = formula1 ()
 in (AST.UnaryExp(AST.Not, formula))
end)
 in ( LrTable.NT 3, ( result, NOT1left, formula1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.formula (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.VarExp(ID))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.BOOLCONST BOOLCONST1, BOOLCONST1left, 
BOOLCONST1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (BOOLCONST as BOOLCONST1) = BOOLCONST1 ()
 in (AST.BoolExp(BOOLCONST))
end)
 in ( LrTable.NT 3, ( result, BOOLCONST1left, BOOLCONST1right), 
rest671)
end
|  ( 30, ( ( _, ( MlyValue.INTCONST INTCONST1, INTCONST1left, 
INTCONST1right)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  (INTCONST as INTCONST1) = INTCONST1 ()
 in (AST.NumExp(INTCONST))
end)
 in ( LrTable.NT 3, ( result, INTCONST1left, INTCONST1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Bool_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INTCONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.INTCONST (fn () => i),p1,p2))
fun BOOLCONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.BOOLCONST (fn () => i),p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROWTYP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROWDEF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
