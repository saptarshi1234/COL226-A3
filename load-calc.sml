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

fun getTokenType tokenIndex = 
	case tokenIndex of 
		0 => "NOT"
  | 1 => "AND"
  | 2 => "OR"
  | 3 => "XOR"
  | 4 => "EQUALS"
  | 5 => "IMPLIES"
  | 6 => "IF"
  | 7 => "THEN"
  | 8 => "ELSE"
  | 9 => "FI"
  | 10 => "PLUS"
  | 11 => "MINUS"
  | 12 => "TIMES"
  | 13 => "NEGATE"
  | 14 => "LESSTHAN"
  | 15 => "GREATERTHAN"
  | 16 => "LPAREN"
  | 17 => "RPAREN"
  | 18 => "ID"
  | 19 => "INTCONST"
  | 20 => "BOOLCONST"
  | 21 => "LET"
  | 22 => "IN"
  | 23 => "END"
  | 24 => "ASSIGN"
  | 25 => "TERM"
  | 26 => "EOF"
  | _ => "bogus-term"

fun getTokens lexer = let
    val dummyEOF = BoolLrVals.Tokens.EOF(0,0)
		val (nextToken, lexer) = BoolParser.Stream.get lexer
		val LrParser.Token.TOKEN(LrParser.LrTable.T(x),b) = nextToken
	in 
		if BoolParser.sameToken(nextToken, dummyEOF) then []
		else (getTokenType x) :: (getTokens lexer)
	end 


fun evaluateList [] = []
|   evaluateList (a::x) = (EVALUATOR.evaluate(a, [])) :: (evaluateList x)

val parseString = parse o stringToLexer
val evaluateString = evaluateList o parseString
val printTokens = getTokens o stringToLexer


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

