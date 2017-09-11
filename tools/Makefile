all: create-ram-module read-debug

create-ram-module: create-ram-module.c
	$(CC) -g -O -o create-ram-module create-ram-module.c

read-debug: read-debug.c
	$(CC) -g -O -o read-debug read-debug.c

clean:
	@rm -rf create-ram-module read-debug *.o
