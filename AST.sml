structure AST =
struct
  type id = string

  datatype binop = Plus | Minus | Times | LessThan | GreaterThan | And | Or | Xor | Equals | Implies
  datatype unop = Not | Negate
  datatype typ = Int | Bool | Arrow of typ * typ

  datatype exp = 
    NumExp of int
  | BoolExp of bool
  | VarExp of id
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
    case bin of Plus => "Plus" | Minus => "Minus" | Times => "Times" | LessThan => "LessThan" | GreaterThan => "GreaterThan" | And => "And" | Or => "Or" | Xor => "Xor" | Equals => "Equals" | Implies => "Implies"

  fun unopToString un =
    case un of Not => "Not" | Negate => "Negate" 
  
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
      NumExp(num)                                             =>  Int.toString num
    | BoolExp(b)                                              =>  Bool.toString b
    | VarExp(id)                                              =>  id
    | LetExp(VarExp(var_id), var_val, exp)                    =>  "let " ^ var_id ^ " = " ^ expToString(var_val) ^ " in " ^ expToString(exp) ^ " end"
    | BinExp(oper, exp1, exp2)                                =>  expToString(exp1) ^ " " ^ binopToString(oper) ^ " " ^ expToString(exp2)
    | UnaryExp(unop, exp)                                     =>  unopToString(unop) ^ " " ^ expToString(exp)
    | CondExp(exp1, exp2, exp3)                               =>  "if " ^ expToString(exp1) ^ " then " ^ expToString(exp2) ^ " else " ^ expToString(exp3) ^ " fi"
    | AppExp(fexp, exp)                                       =>  "(" ^ expToString(fexp) ^ " " ^ expToString(exp) ^ ")"
    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  "fun " ^ name ^ " (" ^ arg ^ ":" ^ typeToString(typ1) ^ "):" ^ typeToString(typ2) ^ " => " ^ expToString(exp)  
    | LambdaExp(VarExp(arg), typ1, typ2, exp)                 =>  "fn (" ^ arg ^ ":" ^ typeToString(typ1) ^ "):" ^ typeToString(typ2) ^ " => " ^ expToString(exp)
    | _                                                       =>  ""

  fun expToTree (exp) = 
    case exp of 
      NumExp(num)                                             =>  Int.toString num
    | BoolExp(b)                                              =>  Bool.toString b
    | VarExp(id)                                              =>  id
    | LetExp(VarExp(var_id), var_val, exp)                    =>  "LetExp(" ^ var_id ^ "," ^ expToTree(var_val) ^ "," ^ expToTree(exp) ^ ")"
    | BinExp(oper, exp1, exp2)                                =>  binopToString(oper) ^ "(" ^ expToTree(exp1) ^ "," ^ expToTree(exp2) ^ ")"
    | UnaryExp(unop, exp)                                     =>  unopToString(unop) ^ "(" ^ expToTree(exp) ^ ")"
    | CondExp(exp1, exp2, exp3)                               =>  "CondExp(" ^ expToTree(exp1) ^ "," ^ expToTree(exp2) ^ "," ^ expToTree(exp3) ^ ")"
    | AppExp(fexp, exp)                                       =>  "Apply(" ^ expToTree(fexp) ^ "," ^ expToTree(exp) ^ ")"
    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  "Fun(" ^ name ^ "," ^ arg ^ "," ^ typeToString(typ1) ^ "," ^ typeToString(typ2) ^ "," ^ expToTree(exp) ^ ")"  
    | LambdaExp(VarExp(arg), typ1, typ2, exp)                 =>  "Fn(" ^ arg ^ "," ^ typeToString(typ1) ^ "," ^ typeToString(typ2) ^ "," ^ expToTree(exp) ^ ")"
    | _                                                       =>  ""


  fun envLookup (var : id, env) =
    case List.find (fn (x, _) => x = var ) env of
      SOME (x, v) => v
    | NONE => raise Fail("\n\t" ^ "could not find variable: " ^ var)


  fun envAdd(var : id , v , env) =
    (var, v) :: env

end