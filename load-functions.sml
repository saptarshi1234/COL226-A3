structure BoolLrVals = BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex = BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BoolLrVals.ParserData
     	       structure Lex = BoolLex)
     
val err_str = ref ""
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) = 
		    		(err_str := ( (!err_str) ^ WARNING ^ !fileName ^ ENDC ^ " " ^ HEADER ^ BOLD ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^ ENDC ^":"^ s ^ "\n" ) ) 
		in
		    BoolParser.parse(0,lexstream,print_error,())
		end
		

fun stringToLexer str =
    let val done = ref false
    	val lexer =  BoolParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val _ = print(!err_str)
		val (nextToken, lexer) = BoolParser.Stream.get lexer
    in
        if BoolParser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end
	handle ParseError => (print(!err_str); OS.Process.exit(OS.Process.failure) [""])

val verbose = ref true
val error_occ = ref false

fun evaluateListInternal ([], env, env_types, index) = []
|   evaluateListInternal ((a::x), env, env_types, index) = 
    let 
      open AST
      val _ = if !verbose then (
        print (GREEN ^ "Expression " ^ ENDC ^ (Int.toString index) ^ " ::\n");
        print ("Text \t:  " ^ expToString(a) ^ "\n");
        print ("AST \t:  " ^ (expToTree (a, "\t   ")) ^ "\n")
      ) else ()

      val curr_type = EVALUATOR.computeTypes(a, env_types) handle Fail(s) => (print(s ^ "\n");error_occ := true;Int)
      val ans = if not (!error_occ) then EVALUATOR.evaluate(a, env) else NumVal(1)

      val _ = if not (!error_occ) then print ("Value  : " ^ HEADER ^ (valToString ans) ^ "\n" ^ ENDC ) else ()
      
      val (env, env_types) = if not (!error_occ) then 
        case ans of 
            FunVal("", arg, typ1, typ2, exp, params)    => (env, env_types)
        |   FunVal(name, arg, typ1, typ2, exp, params)  => (envAdd(name, ans, env), envAdd(name, curr_type, env_types))
        | _                                             => (env, env_types)
      else 
        case a of 
            FunctionExp(VarExp(_,name), _, typ1, typ2, _,_) => (env, envAdd(name, Arrow(typ1, typ2), env_types) )
            | _ => (env, env_types)

      val _ = print("\n\n\n")

    in
        (   
            (* print (Int.toString(length env) ^ " "); *)
            ans :: (evaluateListInternal (x, env, env_types, index + 1))
        )
    end

fun evaluateList x = evaluateListInternal (x,[], [], 1)

val parseString = parse o stringToLexer
val evaluateString = evaluateList o parseString
(* val printTokens = getTokens o stringToLexer *)


fun readString fileName = let
    val inStream = TextIO.openIn fileName
    val data = TextIO.inputAll inStream
in 
        (TextIO.closeIn inStream; data)
end

fun saveString data fileName = let
	val outStream = TextIO.openOut(fileName)
in	
	(TextIO.output (outStream, data);
    TextIO.closeOut outStream)
end


val evalFromFile = evaluateString o readString
val parseFromFile = parseString o readString

fun evaluateFromFile (s, v) = (verbose := v; fileName:=s; evalFromFile s) 
