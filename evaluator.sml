structure EVALUATOR = 
struct 
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
    ( 
      (* print (Int.toString(length env) ^ " "); *)
      case (oper, val1, val2) of 
        (Plus(_), NumVal(v1), NumVal(v2))          => NumVal (v1 + v2)
      | (Minus(_), NumVal(v1), NumVal(v2))         => NumVal (v1 - v2)
      | (Times(_), NumVal(v1), NumVal(v2))         => NumVal (v1 * v2)
      | (GreaterThan(_), NumVal(v1), NumVal(v2))   => BoolVal(v1 > v2)
      | (LessThan(_), NumVal(v1), NumVal(v2))      => BoolVal(v1 < v2)
      | (And(_), BoolVal(v1), BoolVal(v2))         => BoolVal(v1 andalso v2)
      | (Or(_), BoolVal(v1), BoolVal(v2))          => BoolVal(v1 orelse v2)
      | (Xor(_), BoolVal(v1), BoolVal(v2))         => BoolVal(xor v1 v2)
      | (Equals(_), NumVal(v1), NumVal(v2))        => BoolVal(v1 = v2)
      | (Equals(_), BoolVal(v1), BoolVal(v2))      => BoolVal(v1 = v2)
      | (Implies(_), BoolVal(v1), BoolVal(v2))     => BoolVal(implies v1 v2)
      | _                                       => raise Fail("\n\t" ^ "broken1 types " ^ (expToString exp1) ^ " " ^ (expToString exp2)) 
    )
    end

  and evalUnaryExp ((oper, exp), env) = 
    let 
      val val1 = evaluate(exp, env)
    in
      case (oper, val1) of 
        (Not(_), BoolVal(b))   =>  BoolVal(not b)
      | (Negate(_), NumVal(n)) =>  NumVal(~n)
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
      FunVal(name, id, arg_typ, return_typ, function_body, env_internal) => 
        (name, id, arg_typ, return_typ, function_body, env_internal) 
      | _ => raise Fail("Broken4 Types")
    val (name, id, arg_typ, return_typ, function_body, env_internal) = p
    val env_internal = envAdd(id, arg, env_internal)
    val env_internal = if name <> "" then envAdd(name, lval, env_internal) else env_internal
    val ans = evaluate(function_body, env_internal) 

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
    (
      (* print ( expToTree(ast) ^ " " ^ Int.toString(length env) ^ "\n");  *)
    case ast of
      NumExp(_,num)                                           =>  NumVal(num)
    | BoolExp(_,b)                                            =>  BoolVal(b)
    | VarExp(loc,id)                                          =>  envLookup((loc, id), env)
    | LetExp(VarExp(_,var_id), var_val, exp, _)               =>  evaluate(exp, envAdd(var_id, evaluate(var_val, env), env))
    | BinExp(oper, exp1, exp2)                                =>  evalBinExp((oper, exp1, exp2), env)
    | UnaryExp(unop, exp, _)                                     =>  evalUnaryExp((unop, exp), env)
    | CondExp(exp1, exp2, exp3, _)                               =>  evaluateCondExp((exp1, exp2, exp3), env)
    | AppExp(fexp, exp, _)                                       =>  applylambda(evaluate(fexp, env), exp, env)   (* evn or e ?? *)  
    | FunctionExp(VarExp(_,name), VarExp(_,arg), typ1, typ2, exp, _) =>  FunVal(name, arg, typ1, typ2, exp, env)
    | LambdaExp(VarExp(_,id), typ1, typ2, exp, _)                  =>  FunVal("", id, typ1, typ2, exp, env)
    | _                                                       =>  raise Fail("\n\t" ^ "broken5 types")
  )
  fun getLocStr exp = let 
    val its = Int.toString
    val ((l1,c1), (l2,c2)) = getLineColRange(exp) 
  in 
    (its l1) ^ "." ^ (its c1) ^ "-" ^ (its l2) ^ "." ^ (its c2)
  end

  fun opErr(oper, expected_type, act_type, exp) = raise Fail(errBody(getLocStr exp) ^ "operator and operand do not agree \n\t operator domain\t: " ^ expected_type ^ "\n\t operand\t\t\t: " ^ act_type ^ "\n\t in expression:\n\t\t" ^ HEADER ^ (expToString exp) ^ ENDC ) 
  fun opNfn(expected_type, oper, exp)       = raise Fail(errBody(getLocStr exp) ^ "operator is not a function \n\t operator\t: [" ^ (expToString oper) ^ "] " ^ typeToString expected_type ^ "\n\t in expression :\n\t\t" ^ HEADER ^ (expToString exp) ^ ENDC ) 
  fun fnErr(constraint, etype, exp)             = raise Fail(errBody(getLocStr exp) ^ "expression does not match return type constraint \n\t expression \t: " ^ (typeToString etype)^ "\n\t return type\t: " ^ (typeToString constraint) ^ "\n\t in expression:\n\t\t" ^ HEADER ^ (expToString exp) ^ ENDC )

  fun compose (t1: typ, t2:typ) = "(" ^ typeToString(t1) ^ " * " ^ typeToString(t2) ^ ")" 

  fun computeTypes (ast : AST.exp, env:type_env) = 
        case ast of 
      NumExp(_,num)                                             =>  Int
    | BoolExp(_,b)                                              =>  Bool
    | VarExp(loc,id)                                            =>  envLookup ((loc, id), env)
    | LetExp(VarExp(_,var_id), var_val, exp, _)                    =>  computeTypes (exp, envAdd(var_id, computeTypes(var_val, env), env) )
    | BinExp(oper, exp1, exp2)                                  =>  
        let 
          val t1 = computeTypes (exp1, env)
          val t2 = computeTypes (exp2, env)
        in case oper of 
          Plus(_)          => if t1 = Int andalso t2 = Int then Int else opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | Minus(_)         => if t1 = Int andalso t2 = Int then Int else opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | Times(_)         => if t1 = Int andalso t2 = Int then Int else opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | GreaterThan(_)   => if t1 = Int andalso t2 = Int then Bool else opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | LessThan(_)      => if t1 = Int andalso t2 = Int then Bool else opErr(binopToString oper,compose(Int, Int), compose(t1,t2), ast)
        | And(_)           => if t1 = Bool andalso t2 = Bool then Bool else opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Or(_)            => if t1 = Bool andalso t2 = Bool then Bool else opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Xor(_)           => if t1 = Bool andalso t2 = Bool then Bool else opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Implies(_)       => if t1 = Bool andalso t2 = Bool then Bool else opErr(binopToString oper,compose(Bool, Bool), compose(t1,t2), ast)
        | Equals(_)        => if (t1 = Bool andalso t2 = Bool) orelse (t1 = Int andalso t2 = Int) then Bool else opErr(binopToString oper, compose (Int,Int) ^ " or " ^ compose(Bool,Bool), compose(t1,t2), ast)
        end
    | UnaryExp(oper, exp, _)                                     =>
        let val t = computeTypes (exp, env)
        in case oper of 
          Not(_)     => if t = Bool then Bool else opErr(unopToString oper, typeToString Bool, typeToString t, exp)
        | Negate(_)  => if t = Int then Int else opErr(unopToString oper, typeToString Int, typeToString t, exp)
        end


    | CondExp(exp1, exp2, exp3, _)                               =>  
        let
          val t1 = computeTypes (exp1, env)
          val t2 = computeTypes (exp2, env)
          val t3 = computeTypes (exp3, env)

          val loc_str = getLocStr ast
        in
          if t1 <> Bool then raise Fail(errBody(loc_str) ^ "conditional expression in IF is not a boolean expression \n\t conditional expression\t: [" ^ (expToString exp1) ^ "] - " ^ typeToString(t1) ^ "\n\t in expression:\n\t\t" ^ HEADER ^ (expToString ast) ^ ENDC ) 
          else if t2 <> t3 then raise Fail(errBody(loc_str) ^ "types of IF branches do not agree \n\t then branch\t: " ^ typeToString(t2) ^ "\n\t else branch\t: " ^ typeToString(t3) ^ "\n\t in expression: \n\t\t" ^ HEADER ^ (expToString ast) ^ ENDC)
          else t2
        end

          

    | AppExp(fexp, exp, _)                                       =>   
        let 
          val funType = computeTypes (fexp, env) 
          val actualParamsType = computeTypes (exp, env) 
        in 
          case funType of 
            Arrow(formalParamsType, returnType) => if formalParamsType = actualParamsType then returnType else opErr("", typeToString(formalParamsType) , typeToString(actualParamsType),ast)
          | _                                   => opNfn(funType, fexp, ast) 
        end

    | FunctionExp(VarExp(_,name), VarExp(_,arg), typ1, typ2, exp, _) =>  
        let 
          val bodyType = computeTypes (exp, envAdd( name, Arrow(typ1, typ2), envAdd(arg, typ1, env)) )
        in 
          if bodyType = typ2 then Arrow(typ1, typ2) 
          else fnErr(typ2, bodyType, ast)
        end 

    | LambdaExp(VarExp(_,arg), typ1, typ2, exp, _)                  =>  
        let 
          val bodyType = computeTypes (exp, envAdd(arg, typ1, env)) 
        in 
          if bodyType = typ2 then Arrow(typ1, typ2) 
          else fnErr(typ2, bodyType, ast)
        end 
    | _                                                       =>  raise Fail("\n\t" ^ "F")
end
