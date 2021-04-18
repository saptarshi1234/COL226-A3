structure AST =
struct
datatype binop = Add | Sub | Mul | Div

datatype exp = Num of int
| Id of string
| BinExp of binop * exp * exp
end
