structure AST =
struct

  datatype binop = Plus | Minus | Times | LessThan | GreaterThan | And | Or | Xor | Equals | Implies
  datatype unop = Not | Negate
  type id = string

  datatype exp = 
    NumExp of int
  | BoolExp of bool
  | VarExp of id
  | CondExp of exp * exp * exp
  | LetExp of id * exp * exp
  | BinExp of binop * exp * exp
  | UnaryExp of unop * exp

  datatype value = NumVal of int | BoolVal of bool

  type environment = (id * value) list


  fun envLookup (var : id, env : environment) : value =
    case List.find (fn (x, _) => x = var ) env of
      SOME (x, v) => v
    | NONE => raise Fail "Evaluation Error"


  fun envAdd(var : id , v : value , env : environment ) =
    (var, v) :: env
end