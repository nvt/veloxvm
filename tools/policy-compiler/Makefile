all: vm-policy

vm-policy: policy-scanner.l policy-parser.y
	bison -d policy-parser.y
	flex policy-scanner.l
	cc -I../../include -o vm-policy policy-parser.tab.c lex.yy.c

clean:
	@rm -rf vm-policy
