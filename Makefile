all:
	mllex bool.lex
	mlyacc bool.yacc
	mlton -output a3 -default-ann 'allowExtendedTextConsts true' bundler.mlb 

rebuild: clean all

clean:
	rm bool.lex.*
	rm bool.yacc.*

zip:
	mkdir -p 2019CS50447
	cp *.lex *.yacc *.sml *.mlb ./Makefile 2019CS50447/
	zip 2019CS50447.zip 2019CS50447 -r
	rm -rf 2019CS50447
