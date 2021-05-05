all:
	mllex bool.lex
	mlyacc bool.yacc
	mlton -output a3 -default-ann 'allowExtendedTextConsts true' bundler.mlb 

rebuild: clean all

clean:
	rm bool.lex.*
	rm bool.yacc.*