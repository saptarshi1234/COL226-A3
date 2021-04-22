
structure EVALUATOR = 
struct 
  (* exception ReturnTypeMismatch of string
  exception ArgumentTypeMismatch of string
  exception OperatorOperandMismatch of string
  exception OperatorNotFunction of string *)

  open AST

  fun xor a b = (a andalso not b) orelse (not a andalso b)
  fun implies a b = not a orelse b

  fun getTypeError (expected, received) = 
    raise Fail("\n\t" ^ "Type mismatch -> Expected " ^ (typeToString expected) ^ ", received " ^ (typeToString received) )


  fun evalBinExp ((oper, exp1, exp2), env) = 
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
      | _                                       => raise Fail("\n\t" ^ "broken1 types " ^ (expToString exp1) ^ " " ^ (expToString exp2)) 
    end

  and evalUnaryExp ((oper, exp), env) = 
    let 
      val val1 = evaluate(exp, env)
    in
      case (oper, val1) of 
        (Not, BoolVal(b))   =>  BoolVal(not b)
      | (Negate, NumVal(n)) =>  NumVal(~n)
      | _                   => raise Fail("\n\t" ^ "broken2 types")
    end

  and evaluateCondExp((exp1, exp2, exp3), env) = 
    let 
      val val1 = evaluate (exp1, env)
    in 
      case val1 of 
        BoolVal(b)    => if b then evaluate (exp2, env) else evaluate(exp3, env)
        | _           =>  raise Fail("\n\t" ^ "broken3 types") 
    end

    
  and applylambda(lval, arg_exp, env) = let
    val arg = evaluate (arg_exp, env)
    val p = case lval of 
      FunVal(_, id, arg_typ, return_typ, function_body, env_internal) => 
        (id, arg_typ, return_typ, function_body, env_internal) 
      | _ => raise Fail("Broken4 Types")
    val (id, arg_typ, return_typ, function_body, env_internal) = p
    val env_internal = envAdd(id, arg, env_internal)
    val ans = evaluate(function_body, env_internal @ env) 

    val expected_params_type = (getType arg)
    val expected_return_type = (getType ans)

    val params_typed = expected_params_type = arg_typ
    val return_typed = expected_return_type = return_typ
  in
    ans
  end

  (* fun getType exp =  *)
    (* cas *)


  and evaluate (ast: AST.exp, env:environment): value =
(*     (print (Int.toString(length env) ^ " ");  *)
    case ast of
      NumExp(num)                                             =>  NumVal(num)
    | BoolExp(b)                                              =>  BoolVal(b)
    | VarExp(id)                                              =>  envLookup(id, env)
    | LetExp(VarExp(var_id), var_val, exp)                    =>  evaluate(exp, envAdd(var_id, evaluate(var_val, env), env))
    | BinExp(oper, exp1, exp2)                                =>  evalBinExp((oper, exp1, exp2), env)
    | UnaryExp(unop, exp)                                     =>  evalUnaryExp((unop, exp), env)
    | CondExp(exp1, exp2, exp3)                               =>  evaluateCondExp((exp1, exp2, exp3), env)
    | AppExp(fexp, exp)                                       =>  applylambda(evaluate(fexp, env), exp, env)   (* evn or e ?? *)  
    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  FunVal(name, arg, typ1, typ2, exp, env)
    | LambdaExp(VarExp(id), typ1, typ2, exp)                  =>  FunVal("", id, typ1, typ2, exp, env)
    | _                                                       =>  raise Fail("\n\t" ^ "broken5 types")
(*   ) *)
  fun opErr(oper, expected_type, act_type, exp) = raise Fail("\n\t" ^ "OperatorOperandMismatch -> " ^ oper ^ " expected: " ^ expected_type ^ ", received: " ^ act_type ^ " in expr: " ^ (expToString exp) ) 
  
  fun compose (t1: typ, t2:typ) = "(" ^ typeToString(t1) ^ "*" ^ typeToString(t2) ^ ")" 

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
          Plus          => if t1 = Int andalso t2 = Int then Int else raise opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | Minus         => if t1 = Int andalso t2 = Int then Int else raise opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | Times         => if t1 = Int andalso t2 = Int then Int else raise opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)

        | GreaterThan   => if t1 = Int andalso t2 = Int then Bool else raise opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | LessThan      => if t1 = Int andalso t2 = Int then Bool else raise opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)

        | And           => if t1 = Bool andalso t2 = Bool then Bool else raise opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Or            => if t1 = Bool andalso t2 = Bool then Bool else raise opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Xor           => if t1 = Bool andalso t2 = Bool then Bool else raise opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Implies       => if t1 = Bool andalso t2 = Bool then Bool else raise opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)

        | Equals        => if (t1 = Bool andalso t2 = Bool) orelse (t1 = Int andalso t2 = Int) then Bool else raise Fail("\n\t" ^ "OperatorOperandMismatch -> " ^ "Equals expected: " ^ compose(Int, Int) ^ " or " ^ compose(Bool, Bool) ^ ", received: " ^ compose(t1,t2) ^ " in expr: " ^ expToString(ast) )
        end
    | UnaryExp(oper, exp)                                     =>
        let val t = computeTypes (exp, env)
        in case oper of 
          Not     => if t = Bool then Bool else raise opErr(unopToString oper, typeToString Bool, typeToString t, exp)
        | Negate  => if t = Int then Int else raise opErr(unopToString oper, typeToString Int, typeToString t, exp)
        end


    | CondExp(exp1, exp2, exp3)                               =>  
        let
          val t1 = computeTypes (exp1, env)
          val t2 = computeTypes (exp2, env)
          val t3 = computeTypes (exp3, env)
        in
          if t1 <> Bool then raise opErr("IF ", typeToString Bool, typeToString t1, exp1)
          else if t2 <> t3 then raise Fail("\n\t" ^ "OperatorOperandMismatch -> " ^ "THEN ... ELSE ... have different types: " ^ typeToString(t2) ^" and " ^ typeToString(t3))
          else t2
        end

          

    | AppExp(fexp, exp)                                       =>   
        let 
          val funType = computeTypes (fexp, env) 
          val actualParamsType = computeTypes (exp, env) 
        in 
          case funType of 
            Arrow(formalParamsType, returnType) => if formalParamsType = actualParamsType then returnType else raise Fail("\n\t" ^ "ArgumentTypeMismatch -> " ^ "Formal: " ^ typeToString(formalParamsType) ^ " actual: " ^ typeToString(actualParamsType) ^ " in exp: " ^ expToString(ast))
          | _                                   => raise Fail("\n\t" ^ "OperatorNotFunction -> " ^ expToString(fexp) ^ " has type: " ^ typeToString(funType) ^ " in expression " ^ expToString(ast)) 
        end

    | FunctionExp(VarExp(name), VarExp(arg), typ1, typ2, exp) =>  
        let 
          val bodyType = computeTypes (exp, envAdd( name, Arrow(typ1, typ2), envAdd(arg, typ1, env)) )
        in 
          if bodyType = typ2 then Arrow(typ1, typ2) 
          else raise Fail("\n\t" ^ "ReturnTypeMismatch -> " ^ "expected:" ^ typeToString(typ2) ^ " got: " ^ typeToString(bodyType) ^ " in function " ^ name ^ " : " ^ expToString(ast) )
        end 
    | LambdaExp(VarExp(arg), typ1, typ2, exp)                  =>  
        let 
          val bodyType = computeTypes (exp, envAdd(arg, typ1, env)) 
        in 
          if bodyType = typ2 then Arrow(typ1, typ2) 
          else raise Fail("\n\t" ^ "ReturnTypeMismatch -> " ^ "expected:" ^ typeToString(typ2) ^ " got " ^ typeToString(bodyType) ^ " in lambda function : " ^ expToString(ast))
        end 
    | _                                                       =>  raise Fail("\n\t" ^ "F")
end
