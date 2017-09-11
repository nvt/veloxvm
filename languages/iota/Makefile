all: iota

iota: iota-scanner.l iota-parser.y
	bison -d --debug -v iota-parser.y
	flex iota-scanner.l
	clang -g -o iota iota-parser.tab.c lex.yy.c

clean:
	@rm -rf iota *.output
