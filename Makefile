all:
	@gcc -O0 -g -std=c11 -lncursesw -ltinfo -pedantic -Wall -Wextra ste.c -o ste
	@ctags -R .
