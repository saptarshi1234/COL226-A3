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
val nextcol = ref 1

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

fun update text = (
    col := !nextcol;
    nextcol := (!col) + size(text)
)


%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha = [A-Za-z];
digit = [0-9];
alphadigit = [A-Za-z0-9];
space = [\ \t];
newline = [\n \r\n];

%%

{newline}       =>  (pos := (!pos) + 1; col := 1; nextcol := 1; lex());
{space}+        =>  (update yytext;lex());


"TRUE"          =>  (update(yytext); Tokens.BOOLCONST( ((!pos, (!col, !nextcol)), true), !pos , !col));  
"FALSE"         =>  (update(yytext); Tokens.BOOLCONST( ((!pos, (!col, !nextcol)), false), !pos , !col));  

"NOT"           =>  (update(yytext); Tokens.NOT((!pos, (!col, !nextcol)), !pos, !col));  
"AND"           =>  (update(yytext); Tokens.AND((!pos, (!col, !nextcol)), !pos, !col));  
"OR"            =>  (update(yytext); Tokens.OR((!pos, (!col, !nextcol)), !pos, !col));  
"XOR"           =>  (update(yytext); Tokens.XOR((!pos, (!col, !nextcol)), !pos, !col));  
"IMPLIES"       =>  (update(yytext); Tokens.IMPLIES((!pos, (!col, !nextcol)), !pos, !col));  
"EQUALS"        =>  (update(yytext); Tokens.EQUALS((!pos, (!col, !nextcol)), !pos, !col));  

"PLUS"          =>  (update(yytext); Tokens.PLUS((!pos, (!col, !nextcol)), !pos, !col));
"MINUS"         =>  (update(yytext); Tokens.MINUS((!pos, (!col, !nextcol)), !pos, !col));
"TIMES"         =>  (update(yytext); Tokens.TIMES((!pos, (!col, !nextcol)), !pos, !col));
"NEGATE"        =>  (update(yytext); Tokens.NEGATE((!pos, (!col, !nextcol)), !pos, !col)); 
"LESSTHAN"      =>  (update(yytext); Tokens.LESSTHAN((!pos, (!col, !nextcol)), !pos, !col));  
"GREATERTHAN"   =>  (update(yytext); Tokens.GREATERTHAN((!pos, (!col, !nextcol)), !pos, !col));

"="             =>  (update(yytext); Tokens.ASSIGN((!pos, (!col, !nextcol)), !pos, !col));  
"let"           =>  (update(yytext); Tokens.LET((!pos, (!col, !nextcol)), !pos, !col));  
"in"            =>  (update(yytext); Tokens.IN((!pos, (!col, !nextcol)), !pos, !col));  
"end"           =>  (update(yytext); Tokens.END((!pos, (!col, !nextcol)), !pos, !col));  

"if"            =>  (update(yytext); Tokens.IF((!pos, (!col, !nextcol)), !pos, !col));  
"then"          =>  (update(yytext); Tokens.THEN((!pos, (!col, !nextcol)), !pos, !col));  
"else"          =>  (update(yytext); Tokens.ELSE((!pos, (!col, !nextcol)), !pos, !col));  
"fi"            =>  (update(yytext); Tokens.FI((!pos, (!col, !nextcol)), !pos, !col));  

"("             =>  (update(yytext); Tokens.LPAREN((!pos, (!col, !nextcol)), !pos, !col));  
")"             =>  (update(yytext); Tokens.RPAREN((!pos, (!col, !nextcol)), !pos, !col));  
";"             =>  (update(yytext); Tokens.TERM((!pos, (!col, !nextcol)), !pos, !col)); 

"int"           =>  (update(yytext); Tokens.INT((!pos, (!col, !nextcol)), !pos, !col));  
"bool"          =>  (update(yytext); Tokens.BOOL((!pos, (!col, !nextcol)), !pos, !col));  

"->"            =>  (update(yytext); Tokens.ARROWTYP((!pos, (!col, !nextcol)), !pos, !col));  
"=>"            =>  (update(yytext); Tokens.ARROWDEF((!pos, (!col, !nextcol)), !pos, !col));  
":"             =>  (update(yytext); Tokens.COLON((!pos, (!col, !nextcol)), !pos, !col));  
"fn"            =>  (update(yytext); Tokens.FN((!pos, (!col, !nextcol)), !pos, !col));  
"fun"           =>  (update(yytext); Tokens.FUN((!pos, (!col, !nextcol)), !pos, !col));  

{alpha}{alphadigit}*     =>  (update(yytext); Tokens.ID( ((!pos, (!col, !nextcol)), yytext), !pos, !col));
{digit}+        =>  (update(yytext); Tokens.INTCONST((case (Int.fromString yytext) of SOME(x) => ((!pos, (!col, !nextcol)), x) | NONE =>raise Fail("")), !pos, !col));

.               =>  (error(!pos, !col, yytext); col := (!col) + size(yytext); err := true ;lex());