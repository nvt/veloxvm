all: cyclus

cyclus: cyclus-scanner.l cyclus-parser.y
	bison -d --debug -v cyclus-parser.y
	flex cyclus-scanner.l
	clang -g -o cyclus cyclus-parser.tab.c lex.yy.c

clean:
	@rm -rf cyclus *.output
