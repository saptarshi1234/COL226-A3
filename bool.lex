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


val eof = fn () => (print (concat_list (tl (rev (!str) ) ) ) ; str := [""] ; checkErr(); Tokens.EOF(!pos, !col))
val error = fn (p:int, c:int, text:string) => print ("Unknown Token:" ^ Int.toString(p) ^ ":" ^ Int.toString(c) ^ ":" ^ text ^ "\n")

val space  = " "
val quote = "\""

fun stringify (name, text) = name ^ space ^ quote ^ text ^ quote

fun update (name, text) = 
let
    val value = stringify (name, text)
in
(
    str := value :: (!str);
    col := (!col) + size(text)
)
end


%%
%header (functor BoolLexFun(structure Tokens:Bool_TOKENS));

alpha = [A-Za-z];
digit = [0-9];
space = [\ \t];

%%

\n          =>  (pos := (!pos) + 1; col := 1; lex());
{space}+    =>  (col := (!col) + size(yytext);lex());

"TRUE"      =>  (update("CONST", yytext); Tokens.CONST( stringify("CONST", yytext), !pos, !col - size yytext ));  
"FALSE"     =>  (update("CONST", yytext); Tokens.CONST( stringify("CONST", yytext), !pos, !col - size yytext ));  
"NOT"       =>  (update("NOT", yytext); Tokens.NOT( stringify("NOT", yytext), !pos, !col - size yytext ));  
"AND"       =>  (update("AND", yytext); Tokens.AND( stringify("AND", yytext), !pos, !col - size yytext ));  
"OR"        =>  (update("OR", yytext); Tokens.OR( stringify("OR", yytext), !pos, !col - size yytext ));  
"XOR"       =>  (update("XOR", yytext); Tokens.XOR( stringify("XOR", yytext), !pos, !col - size yytext ));  
"EQUALS"    =>  (update("EQUALS", yytext); Tokens.EQUALS( stringify("EQUALS", yytext), !pos, !col - size yytext ));  
"IMPLIES"   =>  (update("IMPLIES", yytext); Tokens.IMPLIES( stringify("IMPLIES", yytext), !pos, !col - size yytext ));  
"IF"        =>  (update("IF", yytext); Tokens.IF( stringify("IF", yytext), !pos, !col - size yytext ));  
"THEN"      =>  (update("THEN", yytext); Tokens.THEN( stringify("THEN", yytext), !pos, !col - size yytext ));  
"ELSE"      =>  (update("ELSE", yytext); Tokens.ELSE( stringify("ELSE", yytext), !pos, !col - size yytext ));  
"("         =>  (update("LPAREN", yytext); Tokens.LPAREN( stringify("LPAREN", yytext), !pos, !col - size yytext ));  
")"         =>  (update("RPAREN", yytext); Tokens.RPAREN( stringify("RPAREN", yytext), !pos, !col - size yytext ));  
";"         =>  (update("TERM", yytext); Tokens.TERM( stringify("TERM", yytext), !pos, !col - size yytext ));  

{alpha}+    =>  (update("ID", yytext); Tokens.ID( stringify("ID", yytext), !pos, !col - size yytext ) );

.           =>  (error (!pos, !col, yytext); col := (!col) + size(yytext); err := true ;lex());


