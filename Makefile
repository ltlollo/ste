all:
	@gcc ste.c -O0 -g -std=c11 -lncursesw -ltinfo -pedantic -Wall -Wextra -o ste
	@ctags -R .
