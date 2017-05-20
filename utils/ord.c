#include <fcntl.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#ifdef SLOW
#include <assert.h>
#define min(a, b) (a < b ? a : b)

#else
#define min(a, b) (a)
#define assert(a)
#endif

struct Func {
	char *name;
	size_t namesz;
	char *alltxt;
	size_t alltxtsz;
	char *body;
	size_t bodysz;
};

struct Data {
	struct Func *funcs;
	int nfuncs;
	char *text;
	size_t size;
};

void
swap(struct Func *arr, int i, int j) {
	struct Func tmp;

	memcpy(&tmp, arr + i, sizeof *arr);
	memcpy(arr + i, arr + j, sizeof *arr);
	memcpy(arr + j, &tmp, sizeof *arr);
}

int
search_functions(const char *fname, struct Data *data) {
	struct Func *funcs = NULL;
	int nfuncs = 0;
	int fd;
	char *beg;
	char *end;
	char *ftxt_end;
	char *body;
	char *name_end;
	char *name_beg;
	int nparen;

	fd = open(fname, O_RDONLY);
	assert(fd > 0);
	data->size = lseek(fd, 0, SEEK_END);
	assert(data->size > 0);
	data->text = mmap(NULL, data->size, PROT_READ | PROT_WRITE, MAP_PRIVATE,
					  fd, 0);
	assert(data->text != MAP_FAILED);

	beg = data->text;
	end = data->text + data->size;
	while (beg != end) {
		if (*beg == '{') {
			nparen = 0;
			for (ftxt_end = beg; *ftxt_end != '}' || nparen > 1; ftxt_end++) {
				assert(ftxt_end < end);
				if (*ftxt_end == '{') nparen++;
				if (*ftxt_end == '}') nparen--;
			}
			assert(beg - 2 >= data->text);
			if (beg[-2] == ')') {
				body = beg;
				beg -= 2;
				nparen = 0;
				for (; *beg != '(' || nparen > 1; beg--) {
					assert(beg >= data->text);
					if (*ftxt_end == ')') nparen++;
					if (*ftxt_end == '(') nparen--;
				}
				name_end = beg;
				assert(beg >= data->text);
				while (*beg != ' ' && *beg != '\n') {
					assert(beg >= data->text);
					beg--;
				}
				name_beg = beg + 1;
				assert(beg >= data->text);
				while (*beg != '\n') {
					assert(beg >= data->text);
					beg--;
				}
				if (beg[1] == ' ') {
					beg = ftxt_end + 1;
					continue;
				}
				beg--;
				assert(beg >= data->text);
				while (*beg != '\n') {
					assert(beg >= data->text);
					beg--;
				}
				beg++;
				ftxt_end++;
				nfuncs++;
				funcs = realloc(funcs, sizeof *funcs * nfuncs);
				assert(funcs);

				funcs[nfuncs - 1].name = name_beg;
				funcs[nfuncs - 1].namesz = name_end - name_beg;
				funcs[nfuncs - 1].alltxt = beg;
				funcs[nfuncs - 1].alltxtsz = ftxt_end - beg;
				funcs[nfuncs - 1].body = body;
				funcs[nfuncs - 1].bodysz = ftxt_end - body;
			}
			beg = ftxt_end;
		}
		beg++;
	}
	data->funcs = funcs;
	data->nfuncs = nfuncs;
	return 0;
}

int
main() {
	char *text;
	struct Func *funcs;
	struct Func *ordered;
	int nfuncs;
	struct Data data;
	int i;
	struct Func *curr_func;
	struct Func *to_ord; 
	char *call_end;
	char *call_beg;
	FILE *nfile;
	FILE *fsigs;
	char *nl = "\n\n";

	nfile = fopen("new.ste.c", "w");
	assert(nfile);

	fsigs = fopen("new.ste.h", "w");
	assert(fsigs);

	search_functions("ste.c", &data);

	funcs  = data.funcs;
	nfuncs = data.nfuncs;
	text   = data.text;

	fwrite(text, 1, funcs->alltxt - text, nfile);
	
	ordered = malloc(nfuncs * sizeof *ordered);
	memcpy(ordered, funcs, nfuncs * sizeof *ordered);

	for (i = 0; i < nfuncs; i++) {
		if (memcmp("main", funcs[i].name, funcs[i].namesz) == 0) {
			break;
		}
	}
	swap(ordered, i, 0);
	i = 1;

	for(curr_func  = ordered; curr_func != ordered + nfuncs; curr_func++) {
		fwrite(curr_func->alltxt, 1, curr_func->alltxtsz, nfile);
		fwrite(nl, 1, 2, nfile);
		if (i == nfuncs || curr_func - ordered == nfuncs - 1) {
			continue;
		}
		for (call_end = curr_func->body; 
			 call_end < curr_func->body + curr_func->bodysz; call_end++) {
			if (*call_end == '(') {
				call_beg = call_end;
				while (*call_beg != ' ') {
					call_beg--;
				}
				call_beg++;
				if (call_end - call_beg == 0) {
					continue;
				}
				for (to_ord = ordered;
					 to_ord < ordered + nfuncs && to_ord < ordered + i;
					 to_ord++) {
					if (memcmp(call_beg, to_ord->name,
							   min(to_ord->namesz, call_end - call_beg)) == 0) {
						break;
					}
				}
				if (to_ord != ordered + i) {
					continue;
				}
				for (to_ord = ordered + i + 1; to_ord < ordered + nfuncs;
					 to_ord++) {
					if (memcmp(call_beg, to_ord->name,
							   min(to_ord->namesz, call_end - call_beg))
						== 0) {
						swap(ordered, i, to_ord - ordered);
						i++;
						break;
					}
				}
			}
		}
	}

	for (i = 1; i < nfuncs; i++) {
		ordered[i].body[-1] = ';';
		fwrite(ordered[i].alltxt, 1, ordered[i].body - ordered[i].alltxt,
			   fsigs);
		fwrite(nl, 1, 2, fsigs);
	}
	return 0;
}
