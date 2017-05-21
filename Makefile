LIB  	= -lncursesw -ltinfo
OCOM 	= -std=c11 -pedantic -Wall -Wextra
FILES 	= ste.c
BIN	= ste

all:
	@gcc $(FILES) -Og -g $(LIB) $(OCOM) -o $(BIN)
	@ctags -R .

relese:
	@gcc $(FILES) -O3 $(LIB) $(OCOM) -o $(BIN)
