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

  fun envLookup (var : id, env : environment) : value =
    case List.find (fn (x, _) => x = var ) env of
      SOME (x, v) => v
    | NONE => raise Fail ("could not find variable: " ^ var)


  fun envAdd(var : id , v : value , env : environment ) =
    (var, v) :: env

end