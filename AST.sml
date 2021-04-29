val fileName = ref ""
structure AST =
struct
  (* type loc = int * (int*int) *)
  type loc1 = int*int
  type loc2 = loc1*loc1

  datatype binop = Plus of loc2 | Minus of loc2 | Times of loc2 | LessThan of loc2 | GreaterThan of loc2 | And of loc2 | Or of loc2 | Xor of loc2 | Equals of loc2 | Implies of loc2
  datatype unop = Not of loc2 | Negate of loc2
  datatype typ = Int | Bool | Arrow of typ * typ
  type id = string

  datatype exp = 
    NumExp of loc2 * int
  | BoolExp of loc2 * bool
  | VarExp of loc2 * id
  | CondExp of exp * exp * exp * loc2
  | LetExp of exp * exp * exp * loc2
  | BinExp of binop * exp * exp
  | UnaryExp of unop * exp * loc1
  | LambdaExp of exp * typ * typ * exp * loc1
  | FunctionExp of exp * exp * typ * typ * exp * loc1
  | AppExp of exp * exp * loc2


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
    | LetExp(VarExp(_,var_id), var_val, exp,_)                  =>  "let " ^ var_id ^ " = " ^ expToString(var_val) ^ " in " ^ expToString(exp) ^ " end"
    | BinExp(oper, exp1, exp2)                                =>  expToString(exp1) ^ " " ^ binopToString(oper) ^ " " ^ expToString(exp2)
    | UnaryExp(unop, exp,_)                                     =>  unopToString(unop) ^ " " ^ expToString(exp)
    | CondExp(exp1, exp2, exp3, _)                               =>  "if " ^ expToString(exp1) ^ " then " ^ expToString(exp2) ^ " else " ^ expToString(exp3) ^ " fi"
    | AppExp(fexp, exp,_)                                       =>  "(" ^ expToString(fexp) ^ " " ^ expToString(exp) ^ ")"
    | FunctionExp(VarExp(_,name), VarExp(_,arg), typ1, typ2, exp,_) =>  "fun " ^ name ^ " (" ^ arg ^ ":" ^ typeToString(typ1) ^ "):" ^ typeToString(typ2) ^ " => " ^ expToString(exp)  
    | LambdaExp(VarExp(_,arg), typ1, typ2, exp,_)                 =>  "fn (" ^ arg ^ ":" ^ typeToString(typ1) ^ "):" ^ typeToString(typ2) ^ " => " ^ expToString(exp)
    | _                                                       =>  ""

  fun expToTree (exp) = 
    case exp of 
      NumExp(_,num)                                               =>  Int.toString num
    | BoolExp(_,b)                                                =>  Bool.toString b
    | VarExp(_,id)                                                =>  id
    | LetExp(VarExp(_,var_id), var_val, exp, _)                      =>  "LetExp(" ^ var_id ^ "," ^ expToTree(var_val) ^ "," ^ expToTree(exp) ^ ")"
    | BinExp(oper, exp1, exp2)                                    =>  binopToString(oper) ^ "(" ^ expToTree(exp1) ^ "," ^ expToTree(exp2) ^ ")"
    | UnaryExp(unop, exp, _)                                         =>  unopToString(unop) ^ "(" ^ expToTree(exp) ^ ")"
    | CondExp(exp1, exp2, exp3, _)                                   =>  "CondExp(" ^ expToTree(exp1) ^ "," ^ expToTree(exp2) ^ "," ^ expToTree(exp3) ^ ")"
    | AppExp(fexp, exp, _)                                           =>  "Apply(" ^ expToTree(fexp) ^ "," ^ expToTree(exp) ^ ")"
    | FunctionExp(VarExp(_,name), VarExp(_,arg), typ1, typ2, exp, _) =>  "Fun(" ^ name ^ "," ^ arg ^ "," ^ typeToString(typ1) ^ "," ^ typeToString(typ2) ^ "," ^ expToTree(exp) ^ ")"  
    | LambdaExp(VarExp(_,arg), typ1, typ2, exp,_)                   =>  "Fn(" ^ arg ^ "," ^ typeToString(typ1) ^ "," ^ typeToString(typ2) ^ "," ^ expToTree(exp) ^ ")"
    | _                                                           =>  ""

  fun getLineColRange(exp) = 
    case exp of           
      NumExp(p,num)                                               =>  p
    | BoolExp(p,b)                                                =>  p
    | VarExp(p,id)                                                =>  p
    | LetExp(VarExp(_,var_id), var_val, exp, p)                   =>  p
    | BinExp(oper, exp1, exp2)                                    =>  let val (l1,_) = getLineColRange exp1; val (_,l2) = getLineColRange exp2 in (l1,l2) end
    | UnaryExp(unop, exp, p)                                      =>  let val (_, p2) = getLineColRange exp in (p,p2) end
    | CondExp(exp1, exp2, exp3, p)                                =>  p
    | AppExp(fexp, exp, p)                                        =>  p
    | FunctionExp(Vname, vararg, typ1, typ2, exp, p)              =>  let val (_,(l3,c3)) = getLineColRange exp;  in (p, (l3, c3)) end
    | LambdaExp(Varg, typ1, typ2, exp, p)                         =>  let val (_,(l3,c3)) = getLineColRange exp;  in (p, (l3, c3)) end
    | _                                                           =>  ((0,0),(0,0))
  

  fun locToStr loc = let 
    val its = Int.toString
    val ((l1,c1), (l2,c2)) = loc
    in 
      (its l1) ^ "." ^ (its c1) ^ "-" ^ (its l2) ^ "." ^ (its c2)
    end
  
  fun getLocStr exp = locToStr (getLineColRange exp)


  val itss = Int.toString
  
  fun envLookup (( p, var:id ), env) =
    case List.find (fn (x, _) => x = var ) env of
      SOME (x, v) => v
    | NONE => raise Fail("\n" ^ !fileName ^ ": " ^ locToStr p ^ " Error: could not find variable: " ^ var)


  fun envAdd(var : id , v , env) =
    (var, v) :: env

end