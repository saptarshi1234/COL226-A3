run:
	ml-lex bool.lex
	ml-yacc bool.yacc
	rlwrap sml loader.sml
clean:
	rm bool.lex.*
	rm bool.yacc.*