
structure EVALUATOR = 
struct 
  open AST

  fun xor a b = (a andalso not b) orelse (not a andalso b)
  fun implies a b = not a orelse b

  fun getTypeError (expected, received) = 
    raise Fail("Type mismatch: Expected " ^ (typeToString expected) ^ " received " ^ (typeToString received) )


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
      | _                   => raise Fail("broken types")
    end

  and evaluateCondExp(CondExp(exp1, exp2, exp3), env) = 
    let 
      val val1 = evaluate (exp1, env)
    in 
      case val1 of 
        BoolVal(b)    => if b then evaluate (exp2, env) else evaluate(exp3, env)
        | _           =>  raise Fail("broken types") 
      
      (* case (val1, val2, val3) of 
        (BoolVal(b), NumVal(n2), NumVal(n3))    =>  NumVal(if b then n2 else n3)
      | (BoolVal(b), BoolVal(b2), BoolVal(b3))  =>  BoolVal(if b then b2 else b3)
      | (BoolVal(b), )
      | _                                       =>  (print (getTypeString val2); print (getTypeString val3);raise Fail("broken types")) *)
    end

  and applyfunction(name, exp, env) = let 
    val lambda = envLookup(name, env)
    in 
      applylambda(lambda, exp, env)
    end
    
  and applylambda(lval, arg_exp, env) = let
    val arg = evaluate (arg_exp, env)
    val FunVal(_, id, arg_typ, return_typ, function_body, env_internal) = lval
    val env_internal = envAdd(id, arg, env_internal)
    val ans = evaluate(function_body, env_internal @ env) 

    val expected_params_type = (getType arg)
    val expected_return_type = (getType ans)

    val params_typed = expected_params_type = arg_typ
    val return_typed = expected_return_type = return_typ
  in
    if params_typed then 
      if return_typed then ans else getTypeError(expected_return_type, return_typ) 
    else getTypeError(expected_params_type, arg_typ)  
  end

  (* fun getType exp =  *)
    (* cas *)


  and evaluate (ast: AST.exp, env:environment): value =
    case ast of 
      NumExp(num)                                             =>  NumVal(num)
    | BoolExp(b)                                              =>  BoolVal(b)
    | VarExp(id)                                              =>  envLookup(id, env)
    | LetExp(VarExp(var_id), var_val, exp)                    =>  evaluate(exp, envAdd(var_id, evaluate(var_val, env), env))
    | BinExp(oper, exp1, exp2)                                =>  evalBinExp(ast, env)
    | UnaryExp(unop, exp)                                     =>  evalUnaryExp(ast, env)
    | CondExp(exp1, exp2, exp3)                               =>  evaluateCondExp(ast, env)

    (* | AppExp(VarExp(name), exp)                               =>  applyfunction(name, exp, env) *)
    | AppExp(fexp, exp)                                       =>  applylambda(evaluate(fexp, env), exp, env)   (* evn or e ?? *)  
    
    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  FunVal(name, arg, typ1, typ2, exp, env)
    | LambdaExp(VarExp(id), typ1, typ2, exp)                  =>  FunVal("", id, typ1, typ2, exp, env)
    | _                                                       =>  raise Fail("broken types")
  
  
  fun computeTypes (ast : AST.exp, env:type_env) = 
        case ast of 
      NumExp(num)                                             =>  Int
    | BoolExp(b)                                              =>  Bool
    | VarExp(id)                                              =>  envLookup (id, env)
    | LetExp(VarExp(var_id), var_val, exp)                    =>  computeTypes (exp, envAdd(var_id, computeTypes(var_val, env), env) )
    | BinExp(oper, exp1, exp2)                                =>  
        let 
          val t1 = computeTypes (exp1, env)
          val t2 = computeTypes (exp2, env)
        in case oper of 
          Plus => if t1 = Int andalso t2 = Int then Int else raise Fail("Plus")
        | Minus => if t1 = Int andalso t2 = Int then Int else raise Fail("Minus")
        | Times => if t1 = Int andalso t2 = Int then Int else raise Fail("Times")

        | GreaterThan => if t1 = Int andalso t2 = Int then Bool else raise Fail("GreaterThan")
        | LessThan => if t1 = Int andalso t2 = Int then Bool else raise Fail("LessThan")

        | And => if t1 = Bool andalso t2 = Bool then Bool else raise Fail("And")
        | Or => if t1 = Bool andalso t2 = Bool then Bool else raise Fail("Or")
        | Xor => if t1 = Bool andalso t2 = Bool then Bool else raise Fail("Xor")
        | Equals => if (t1 = Bool andalso t2 = Bool) orelse (t1 = Int andalso t2 = Int) then Bool else raise Fail("Equals")
        | Implies => if t1 = Bool andalso t2 = Bool then Bool else raise Fail("Implies")
        end
    | UnaryExp(oper, exp)                                     =>
        let val t = computeTypes (exp, env)
        in case oper of 
          Not => if t = Bool then Bool else raise Fail("Not")
        | Negate => if t = Int then Int else raise Fail("Int")
        end


    | CondExp(exp1, exp2, exp3)                               =>  
        let
          val t1 = computeTypes (exp1, env)
          val t2 = computeTypes (exp2, env)
          val t3 = computeTypes (exp3, env)
        in
          if t1 = Bool andalso t2 = t3 then (print(typeToString(t1) ^ "\n");t2)
          (* else raise Fail("IF") *)
          else (print(typeToString t1);print(typeToString t2);print(typeToString t3);raise Fail("IF"))
        end

          

    | AppExp(fexp, exp)                                       =>   
        (case (computeTypes (fexp, env)) of 
          Arrow(arg, ret) => if (computeTypes (exp, env)) = arg then ret else raise Fail("Wrong arguments passed")
        | _               => (print (typeToString (computeTypes(fexp, env)) ); raise Fail("Wrong application")) )
    
    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  
        let 
          val body = computeTypes (exp, envAdd( name, Arrow(typ1, typ2), envAdd(arg, typ1, env)) )
        in 
          if body = typ2 then Arrow(typ1, typ2) 
          else raise Fail("failed type checks expected:" ^ typeToString(typ2) ^ " got " ^ typeToString(body))
        end 
    | LambdaExp(VarExp(arg), typ1, typ2, exp)                  =>  
        let 
          val body = computeTypes (exp, envAdd(arg, typ1, env)) 
        in 
          if body = typ2 then Arrow(typ1, typ2) 
          else raise Fail("failed type checks expected:" ^ typeToString(typ2) ^ " got " ^ typeToString(body))
        end 
    | _                                                       =>  raise Fail("F")
end