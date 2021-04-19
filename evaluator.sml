
structure EVALUATOR = 
struct 
  open AST

  fun xor a b = (a andalso not b) orelse (not a andalso b)
  fun implies a b = not a orelse b


  fun evalBinExp (BinExp(oper, exp1, exp2), env) = 
    let 
      val val1 = evaluate(exp1, env)
      val val2 = evaluate(exp2, env)
    in
      case (oper, val1, val2) of 
        (Plus, NumVal(v1), NumVal(v2))          => NumVal (v1 + v2)
      | (Minus, NumVal(v1), NumVal(v2))         => NumVal (v1 - v2)
      | (Times, NumVal(v1), NumVal(v2))         => NumVal (v1 * v2)
      | (GreaterThan, NumVal(v1), NumVal(v2))   => BoolVal(v1 > v2)
      | (LessThan, NumVal(v1), NumVal(v2))      => BoolVal(v1 < v2)
      | (And, BoolVal(v1), BoolVal(v2))         => BoolVal(v1 andalso v2)
      | (Or, BoolVal(v1), BoolVal(v2))          => BoolVal(v1 orelse v2)
      | (Xor, BoolVal(v1), BoolVal(v2))         => BoolVal(xor v1 v2)
      | (Equals, NumVal(v1), NumVal(v2))        => BoolVal(v1 = v2)
      | (Equals, BoolVal(v1), BoolVal(v2))      => BoolVal(v1 = v2)
      | (Implies, BoolVal(v1), BoolVal(v2))     => BoolVal(implies v1 v2)
      | _                                       => raise Fail("broken types")
    end

  and evalUnaryExp (UnaryExp(oper, exp), env) = 
    let 
      val val1 = evaluate(exp, env)
    in
      case (oper, val1) of 
        (Not, BoolVal(b))   =>  BoolVal(not b)
      | (Negate, NumVal(n)) =>  NumVal(~n)
      | _                             => raise Fail("broken types")
    end

  and evaluateCondExp(CondExp(exp1, exp2, exp3), env) = 
    let 
      val val1 = evaluate (exp1, env)
      val val2 = evaluate (exp2, env)
      val val3 = evaluate (exp3, env)
    in 
      case (val1, val2, val3) of 
        (BoolVal(b), NumVal(n2), NumVal(n3))    =>  NumVal(if b then n2 else n3)
      | (BoolVal(b), BoolVal(b2), BoolVal(b3))  =>  BoolVal(if b then b2 else b3)
      | _                                       =>  raise Fail("broken types")
    end

  and apply(name, exp, env) = let 
    val value = envLookup(name, env)
    val lambda = case value of 
      FunVal(name, arg, typ1, typ2, function_body)  =>  LambdaVal(arg, typ1, typ2, function_body)
    | _                                             =>  value
    in 
      applylambda(lambda, exp, env)
    end
  
  and applylambda(lval, arg_exp, env) = let
    val arg = evaluate (arg_exp, env)
    val LambdaVal(id, typ1, typ2, function_body) = lval
    val ans = evaluate(function_body, envAdd(id, arg, env))
  in
    ans
  end


  and evaluate (ast: AST.exp, env:environment): value =
    case ast of 
      NumExp(num)                                             =>  NumVal(num)
    | BoolExp(b)                                              =>  BoolVal(b)
    | VarExp(id)                                              =>  envLookup(id, env)
    | LetExp(VarExp(var_id), var_val, exp)                    =>  evaluate(exp, envAdd(var_id, evaluate(var_val, env), env))
    | BinExp(oper, exp1, exp2)                                =>  evalBinExp(ast, env)
    | UnaryExp(unop, exp)                                     =>  evalUnaryExp(ast, env)
    | CondExp(exp1, exp2, exp3)                               =>  evaluateCondExp(ast, env)
    | AppExp(VarExp(name), exp)                               =>  apply(name, exp, env)
    | LambdaExp(VarExp(id), typ1, typ2, exp)                  =>  LambdaVal(id, typ1, typ2, exp)
    | AppExp(l_exp, exp)                                      =>  applylambda(evaluate (l_exp, env), exp, env)   (* evn or e ?? *)  
    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  FunVal(name, arg, typ1, typ2, exp)
    | _                                                       =>  raise Fail("broken types")
    
end