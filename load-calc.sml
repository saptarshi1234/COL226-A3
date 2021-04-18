structure BoolLrVals = BoolLrValsFun(structure Token = LrParser.Token)
structure BoolLex = BoolLexFun(structure Tokens = BoolLrVals.Tokens);
structure BoolParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BoolLrVals.ParserData
     	       structure Lex = BoolLex)
     
val err_str = ref ""
fun invoke lexstream =
    	     	let fun print_error (s,pos:int,col:int) = 
		    		(err_str := (!err_str) ^ "Syntax Error:" ^ (Int.toString pos) ^ ":" ^ (Int.toString col) ^":"^ s ^ "\n" ) 
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

fun partially_concat_list [] = ""
|	partially_concat_list ([a]) = a 
|	partially_concat_list (a::x) = a ^ ", " ^ partially_concat_list(x)

fun concat_list parsedString = "[" ^ partially_concat_list parsedString ^ "]\n"

val parseString = concat_list o parse o stringToLexer

val printParsedString = print o parseString

fun saveString data fileName = let
	val outStream = TextIO.openOut(fileName)
in	
	(TextIO.output (outStream, data);
    TextIO.closeOut outStream)
end

val saveParsedString = saveString o parseString

