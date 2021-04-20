exception LexingError
structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token

val str = ref [""];
val err = ref false;

fun partially_concat_list [] = ""
|	partially_concat_list ([a]) = a 
|	partially_concat_list (a::x) = a ^ ", " ^ partially_concat_list(x)

fun concat_list parsedString = "[" ^ partially_concat_list parsedString ^ "]\n"


val pos = ref 1
val col = ref 1

fun reset () = (
    err := false
)

fun checkErr () = 
    if (!err) then ( reset(); OS.Process.exit(OS.Process.failure)) else (reset())


val eof = fn () => ( str := [""] ; checkErr(); Tokens.EOF(!pos, !col))
val error = fn (p:int, c:int, text:string) => print ("Unknown Token:" ^ Int.toString(p) ^ ":" ^ Int.toString(c) ^ ":" ^ text ^ "\n")

val space  = " "
val quote = "\""

fun stringify (name, text) = name ^ space ^ quote ^ text ^ quote

fun update text = col := (!col) + size(text)


%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha = [A-Za-z];
digit = [0-9];
alphadigit = [A-Za-z0-9];
space = [\ \t];
newline = [\n \r\n];

%%

{newline}       =>  (pos := (!pos) + 1; col := 1; lex());
{space}+        =>  (col := (!col) + size(yytext);lex());


"TRUE"          =>  (update(yytext); Tokens.BOOLCONST(true, !pos, !col - size yytext ));  
"FALSE"         =>  (update(yytext); Tokens.BOOLCONST(false, !pos, !col - size yytext ));  

"NOT"           =>  (update(yytext); Tokens.NOT(!pos, !col - size yytext ));  
"AND"           =>  (update(yytext); Tokens.AND(!pos, !col - size yytext ));  
"OR"            =>  (update(yytext); Tokens.OR(!pos, !col - size yytext ));  
"XOR"           =>  (update(yytext); Tokens.XOR(!pos, !col - size yytext ));  
"IMPLIES"       =>  (update(yytext); Tokens.IMPLIES(!pos, !col - size yytext ));  

"EQUALS"        =>  (update(yytext); Tokens.EQUALS(!pos, !col - size yytext ));  

"PLUS"          =>  (update(yytext); Tokens.PLUS(!pos, !col - size yytext ));
"MINUS"         =>  (update(yytext); Tokens.MINUS(!pos, !col - size yytext ));
"TIMES"         =>  (update(yytext); Tokens.TIMES(!pos, !col - size yytext ));
"NEGATE"        =>  (update(yytext); Tokens.NEGATE(!pos, !col - size yytext )); 

"LESSTHAN"      =>  (update(yytext); Tokens.LESSTHAN(!pos, !col - size yytext ));  
"GREATERTHAN"   =>  (update(yytext); Tokens.GREATERTHAN(!pos, !col - size yytext ));  

"="             =>  (update(yytext); Tokens.ASSIGN(!pos, !col - size yytext ));  
"let"           =>  (update(yytext); Tokens.LET(!pos, !col - size yytext ));  
"in"            =>  (update(yytext); Tokens.IN(!pos, !col - size yytext ));  
"end"           =>  (update(yytext); Tokens.END(!pos, !col - size yytext ));  


"if"            =>  (update(yytext); Tokens.IF(!pos, !col - size yytext ));  
"then"          =>  (update(yytext); Tokens.THEN(!pos, !col - size yytext ));  
"else"          =>  (update(yytext); Tokens.ELSE(!pos, !col - size yytext ));  
"fi"            =>  (update(yytext); Tokens.FI(!pos, !col - size yytext ));  

"("             =>  (update(yytext); Tokens.LPAREN(!pos, !col - size yytext ));  
")"             =>  (update(yytext); Tokens.RPAREN(!pos, !col - size yytext ));  
";"             =>  (update(yytext); Tokens.TERM(!pos, !col - size yytext ));  

"int"           =>  (update(yytext); Tokens.INT(!pos, !col - size yytext ));  
"bool"          =>  (update(yytext); Tokens.BOOL(!pos, !col - size yytext ));  
"->"            =>  (update(yytext); Tokens.ARROWTYP(!pos, !col - size yytext ));  
"=>"            =>  (update(yytext); Tokens.ARROWDEF(!pos, !col - size yytext ));  
":"             =>  (update(yytext); Tokens.COLON(!pos, !col - size yytext ));  

"fn"          =>  (update(yytext); Tokens.FN(!pos, !col - size yytext ));  
"fun"          =>  (update(yytext); Tokens.FUN(!pos, !col - size yytext ));  


{alpha}{alphadigit}*     =>  (update(yytext); Tokens.ID(yytext, !pos, !col - size yytext));
{digit}+        =>  (update(yytext); Tokens.INTCONST((case (Int.fromString yytext) of SOME(x) => x | NONE =>0), !pos, !col - size yytext ));

.               =>  (error(!pos, !col, yytext); col := (!col) + size(yytext); err := true ;lex());