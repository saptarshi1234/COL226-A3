structure AST =
struct
  type loc = int * (int*int)

  datatype binop = Plus of loc | Minus of loc | Times of loc | LessThan of loc | GreaterThan of loc | And of loc | Or of loc | Xor of loc | Equals of loc | Implies of loc
  datatype unop = Not of loc | Negate of loc
  datatype typ = Int | Bool | Arrow of typ * typ
  type id = string

  datatype exp = 
    NumExp of loc * int
  | BoolExp of loc * bool
  | VarExp of loc * id
  | CondExp of exp * exp * exp
  | LetExp of exp * exp * exp
  | BinExp of binop * exp * exp
  | UnaryExp of unop * exp
  | LambdaExp of exp * typ * typ * exp 
  | FunctionExp of exp * exp * typ * typ * exp
  | AppExp of exp * exp 


  datatype value = NumVal of int | BoolVal of bool | FunVal of id * id * typ * typ * exp * ((id * value) list)
  type environment = (id * value) list

  type type_env = (id * typ) list

  fun binopToString bin = 
    case bin of Plus(loc)=> "PLUS" | Minus(loc)=> "MINUS" | Times(loc)=> "TIMES" | LessThan(loc)=> "LESSTHAN" | GreaterThan(loc)=> "GREATERTHAN" | And(loc)=> "AND" | Or(loc)=> "OR" | Xor(loc)=> "XOR" | Equals(loc)=> "EQUALS" | Implies(loc)=> "IMPLIES"

  fun unopToString un =
    case un of Not(loc)=> "NOT" | Negate(loc)=> "NEGATE" 
  
  fun getType (v: value) : typ = case v of 
    NumVal(n)           =>  Int
  | BoolVal(b)          =>  Bool
  | FunVal(a,b,t1,t2,e,f) =>  Arrow(t1, t2)

  fun typeToString(t:typ) = case t of
    Int           =>  "int"
  | Bool          =>  "bool"
  | Arrow(t1,t2)  =>  "(" ^ (typeToString t1) ^ "->" ^ (typeToString t2) ^ ")"

  val getTypeString = typeToString o getType

  fun valToString value = 
    case value of 
      NumVal(n)                         => Int.toString n
    | BoolVal(b)                        => Bool.toString b
    | FunVal("", arg, t1,t2, exp, l)    => "fn " ^ typeToString(Arrow(t1,t2)) 
    | FunVal(name, arg, t1,t2, exp, l)  => "val " ^ name ^ " = fn " ^ typeToString(Arrow(t1,t2)) 


  fun expToString (exp) = 
    case exp of 
      NumExp(_,num)                                           =>  Int.toString num
    | BoolExp(_,b)                                            =>  Bool.toString b
    | VarExp(_,id)                                            =>  id
    | LetExp(VarExp(_,var_id), var_val, exp)                  =>  "let " ^ var_id ^ " = " ^ expToString(var_val) ^ " in " ^ expToString(exp) ^ " end"
    | BinExp(oper, exp1, exp2)                                =>  expToString(exp1) ^ " " ^ binopToString(oper) ^ " " ^ expToString(exp2)
    | UnaryExp(unop, exp)                                     =>  unopToString(unop) ^ " " ^ expToString(exp)
    | CondExp(exp1, exp2, exp3)                               =>  "if " ^ expToString(exp1) ^ " then " ^ expToString(exp2) ^ " else " ^ expToString(exp3) ^ " fi"
    | AppExp(fexp, exp)                                       =>  "(" ^ expToString(fexp) ^ " " ^ expToString(exp) ^ ")"
    | FunctionExp(VarExp(_,name), VarExp(_,arg), typ1, typ2, exp) =>  "fun " ^ name ^ " (" ^ arg ^ ":" ^ typeToString(typ1) ^ "):" ^ typeToString(typ2) ^ " => " ^ expToString(exp)  
    | LambdaExp(VarExp(_,arg), typ1, typ2, exp)                 =>  "fn (" ^ arg ^ ":" ^ typeToString(typ1) ^ "):" ^ typeToString(typ2) ^ " => " ^ expToString(exp)
    | _                                                       =>  ""

  fun expToTree (exp) = 
    case exp of 
      NumExp(_,num)                                               =>  Int.toString num
    | BoolExp(_,b)                                                =>  Bool.toString b
    | VarExp(_,id)                                                =>  id
    | LetExp(VarExp(_,var_id), var_val, exp)                      =>  "LetExp(" ^ var_id ^ "," ^ expToTree(var_val) ^ "," ^ expToTree(exp) ^ ")"
    | BinExp(oper, exp1, exp2)                                    =>  binopToString(oper) ^ "(" ^ expToTree(exp1) ^ "," ^ expToTree(exp2) ^ ")"
    | UnaryExp(unop, exp)                                         =>  unopToString(unop) ^ "(" ^ expToTree(exp) ^ ")"
    | CondExp(exp1, exp2, exp3)                                   =>  "CondExp(" ^ expToTree(exp1) ^ "," ^ expToTree(exp2) ^ "," ^ expToTree(exp3) ^ ")"
    | AppExp(fexp, exp)                                           =>  "Apply(" ^ expToTree(fexp) ^ "," ^ expToTree(exp) ^ ")"
    | FunctionExp(VarExp(_,name), VarExp(_,arg), typ1, typ2, exp) =>  "Fun(" ^ name ^ "," ^ arg ^ "," ^ typeToString(typ1) ^ "," ^ typeToString(typ2) ^ "," ^ expToTree(exp) ^ ")"  
    | LambdaExp(VarExp(_,arg), typ1, typ2, exp)                   =>  "Fn(" ^ arg ^ "," ^ typeToString(typ1) ^ "," ^ typeToString(typ2) ^ "," ^ expToTree(exp) ^ ")"
    | _                                                           =>  ""

  fun getLineColRange(exp) = 
    case exp of 
      NumExp((l,(ca,cb)),num)                                     =>  ((l,ca), (l,cb))
    | BoolExp((l,(ca,cb)),b)                                      =>  ((l,ca), (l,cb))
    | VarExp((l,(ca,cb)),id)                                      =>  ((l,ca), (l,cb))
    | LetExp(VarExp((l1,(c1,_)),var_id), var_val, exp)            =>  let val (_,(l2,c2)) = getLineColRange exp in ((l1, c1-4), (l2, c2)) end
    | BinExp(oper, exp1, exp2)                                    =>  let val (l1,_) = getLineColRange exp1; val (_,l2) = getLineColRange exp2 in (l1,l2) end
    | UnaryExp(unop, exp)                                         =>  let val lp = getLineColRange exp in lp end
    | CondExp(exp1, exp2, exp3)                                   =>  let val ((l1,c1),_) = getLineColRange exp1; val (_,(l3,c3)) = getLineColRange exp3;  in ((l1, c1), (l3, c3)) end
    | AppExp(fexp, exp)                                           =>  let val ((l1,c1),_) = getLineColRange fexp; val (_,(l3,c3)) = getLineColRange exp;  in ((l1, c1), (l3, c3)) end
    | FunctionExp(Vname, vararg, typ1, typ2, exp)                 =>  let val ((l1,c1),_) = getLineColRange Vname; val (_,(l3,c3)) = getLineColRange exp;  in ((l1, c1-4), (l3, c3)) end
    | LambdaExp(Varg, typ1, typ2, exp)                            =>  let val ((l1,c1),_) = getLineColRange Varg; val (_,(l3,c3)) = getLineColRange exp;  in ((l1, c1-3), (l3, c3)) end
    | _                                                           =>  ((0,0),(0,0))
  
  fun getLocStr exp = let 
    val its = Int.toString
    val ((l1,c1), (l2,c2)) = getLineColRange(exp) 
    in 
      (its l1) ^ "." ^ (its c1) ^ "-" ^ (its l2) ^ "." ^ (its c2)
    end

  val itss = Int.toString
  
  fun envLookup (( (l,(c1,c2)), var:id ), env) =
    case List.find (fn (x, _) => x = var ) env of
      SOME (x, v) => v
    | NONE => raise Fail("\n" ^ itss l ^ "." ^ itss c1 ^ "-" ^ itss l ^ "." ^ itss c2 ^ " Error: could not find variable: " ^ var)


  fun envAdd(var : id , v , env) =
    (var, v) :: env

end