#define _XOPEN_SOURCE 700
#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <signal.h>
#include <stdalign.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <wctype.h>
#define _XOPEN_SOURCE_EXTENDED
#include <ncursesw/ncurses.h>

#define TAB_SIZE        8
#define SMALL_STR       8
#define INIT_DIFF       4096
#define INIT_SUBDIFF    128
#define INIT_DOC        1024
#define INIT_LINE       16
#define INIT_SEARCH     128
#define INIT_COMMAND    128
#define SSTR_SIZE       128
#define MSG_SEARCH      "/: "
#define MSG_COMMAND     "$: "
#define MSG_FILES       "@: "
#define UNNAMED_FNAME   "NONAME"

#define MAX_DIFF_SIZE   0x1fffffff

#define KEY_SHIFT_DOWN  336
#define KEY_SHIFT_UP    337
#define KEY_SHIFT_RIGHT 402
#define KEY_SHIFT_LEFT  393
#define KEY_PGDOWN      338
#define KEY_PGUP        339
#define KEY_CTRL_CANC   519
#define KEY_CTRL_LEFT   545
#define KEY_CTRL_RIGHT  560
#define KEY_CTRL_DOWN   525
#define KEY_CTRL_UP     566
#define KEY_CTRL_PGDOWN 550
#define KEY_CTRL_PGUP   555

#define xrealloc_arr(p, size)                                                 \
    xrealloc_ptr(p, size, sizeof(*(*p)->data), sizeof(**p))

#define xrealloc_owndata(p, size)                                             \
    xrealloc_ptr(&p->data, size, sizeof(*p->data), 0)

#define min(a, b) ((a) < (b) ? (a) : (b))
#define arrsize(cx) ((long)(sizeof(cx) / sizeof(*cx)))
#define strsize(cx) (arrsize(cx) - 1)
#define expect(x, v) __builtin_expect(x, v)
#define unlikely(x) expect(!!(x), 0)
#define likely(x) expect(!!(x), 1)
#define unlikely_if_(x) if (unlikely(x))
#define likely_if_(x) if (likely(x))
#define case_key(n, s)                      \
    case n:                                 \
        key = keyname(ch);                  \
        unlikely_if_(strcmp(key, s) != 0) { \
            break;                          \
        }


typedef wint_t lint_t;
typedef wchar_t lchar_t;
typedef int (*filt_fn_t)(lint_t);

enum DIFF_TYPE {
    DIFF_TYPE_ADDBRK,
    DIFF_TYPE_DELBRK,
    DIFF_TYPE_ADDCHR_SMALL,
    DIFF_TYPE_DELCHR_SMALL,
    DIFF_TYPE_INLINE = DIFF_TYPE_DELCHR_SMALL,
    DIFF_TYPE_ADDCHR,
    DIFF_TYPE_DELCHR,
    DIFF_TYPE_SUBSTCK,
};

enum OPT {
    OPT_NONE,
    OPT_SHOW_LINENO,
};

struct Diff {
    struct {
        enum DIFF_TYPE type : 3;
        unsigned size : 29;
        int y;
        int x;
    } __attribute__((packed));
    union {
        unsigned char content[SMALL_STR];
        lchar_t *data;
        struct DiffStk *diff_sub;
        filt_fn_t fil;
    } __attribute__((packed));
} __attribute__((packed));

static_assert(sizeof(struct Diff) == 12 + SMALL_STR, "unsupported arch");

struct Line {
    long size;
    long alloc;
    lchar_t *data;
};

struct DiffStk {
    long curr_save_point;
    long size;
    long alloc;
    long curr;
    struct Diff alignas(32) data[];
};

struct LineArr {
    long size;
    long alloc;
    struct Line alignas(32) data[];
};

enum EFILE {
    EFILE_OK = 0,
    EFILE_OPEN,
    EFILE_UTF8,
    EFILE_NOFILE
};

enum DIREC {
    DIREC_FORW,
    DIREC_BACK,
};

enum TERN {
    TERN_Y,
    TERN_N,
    TERN_CANC,
};

enum MODE {
    MODE_NORMAL,
    MODE_COMMAND,
    MODE_FILES,
    MODE_SEARCH,
    MODE_SELECT_HORIZ,
    MODE_SELECT_VERT,
};

enum FILE_ACT {
    FILE_ACT_OPEN,
    FILE_ACT_RENAME,
};

struct Window {
    int y;
    int x;
    int pgspan;
    int offx;
    int offy;
    int fullx;
};

struct FileInfo {
    char *fname;
};

struct Selection {
    /* xbeg, yend values are excluded */
    int ybeg;
    int yend;
    int xbeg;
    int xend;
};

struct Range {
    lchar_t *beg;
    lchar_t *end;
};

struct SplitIter {
    struct Range line;
    struct Range selct;
};

#define EDITOR_PRIV             \
    struct LineArr *doc;        \
    struct DiffStk *diffstk;    \
    int cursy;                  \
    int cursx;                  \
    int framebeg;

struct DocFile {
    EDITOR_PRIV
};

typedef struct Editor {
    union {
        struct {
            EDITOR_PRIV
        };
        struct DocFile pad;
    };
    struct Window win;
    struct Line *exec_line;
    struct Line *filename;
    struct FileInfo fileinfo;
    enum MODE mode;
    enum OPT opt;
    struct Editor *search;
    struct Editor *command;
    struct Editor *files;
    struct Selection selct;
    struct DocFile *docs;
} Editor;

static int
always(lint_t p) {
    (void)p;
    return 1;
}

static filt_fn_t filts[] = {
    iswalnum, iswpunct, iswspace, iswprint, always,
};

static int always(lint_t);
static void show_keymap(void);
static void diffstk_incr(struct DiffStk *);
static void diffstk_reserve(struct DiffStk **);
static void diffstk_insert_addbk(struct Editor *, int, int);
static void diffstk_insert_delbk(struct Editor *, int, int);
static void diffstk_free_all(struct DiffStk *);
static int eq_bigch(struct Diff *, enum DIFF_TYPE);
static void diffstk_insert_span(struct Editor *, lchar_t *, int);
static void insert_doc(struct LineArr **, int, struct Line *, int);
static void insert_doc_nl_times(struct LineArr **, int, int);
static void line_insert(struct Line *, int, lchar_t *, int);
static void merge_lines(struct LineArr *, int);
static void line_remove_span_qdiff(struct Editor *, struct Line *, int, int);
static filt_fn_t find_match_fil(lint_t);
static int find_prev_simil(const void *, int);
static int find_next_simil(const void *, int, int);
static int realloc_ptr(void *, size_t, size_t, size_t);
static void xrealloc_ptr(void *, size_t, size_t, size_t);
static void close_win(void);
static void open_win(struct Editor *);
static struct Editor *init_editor(const char *);
static int move_right(struct Editor *);
static int move_left(struct Editor *);
static int del_back(struct Editor *);
static int move_up(struct Editor *);
static int move_pgup(struct Editor *);
static int move_pgdown(struct Editor *);
static int move_down(struct Editor *);
static int move_down_natural(struct Editor *);
static int move_up_natural(struct Editor *);
static void diff_apply_brk(struct Editor *, struct Diff *, enum DIREC);
static void diff_apply_chr(struct Editor *, struct Diff *, enum DIREC);
static int diffstk_apply_last(struct Editor *, enum DIREC);
static int diffstk_apply_all(struct Editor *, struct DiffStk *, enum DIREC );
static void save_current(struct LineArr *, struct FileInfo *,
                         struct DiffStk *);
static int handle_input(struct Editor *, lint_t);
static int render_loop(struct Editor *);
static int count_render_width_upto(struct Window *, struct Line *, int);
static lchar_t * render_max_given_width(struct Window *, lchar_t *, lchar_t *,
                                        int);
static lchar_t * render_back_max_given_width(struct Window *, lchar_t *,
                                             lchar_t *, int);
static int count_nlines_upto(struct Editor *, struct Line *, int);
static int count_nlines(struct Editor *, struct Line *);
static int count_lines(struct Editor *);
static void reposition_frame(struct Editor *);
static void render_lines(struct Editor *);
static void render_editor_info(struct Editor *);
static void reposition_cursor(struct Editor *);
static void xinit_doc(struct Editor *, size_t, const char *);
static enum EFILE load_file_utf8(struct Editor *, const char *);
static int is_ascii(lchar_t *, int);
static void diff_insert_span(struct Diff *, lchar_t *, int);
static void save_file_utf8(struct LineArr *, struct FileInfo *,
                           struct DiffStk *);
static char * mkstr_nmt(const char *, ...);
static wchar_t *wmkstr_nmt(const wchar_t *, ...);
static int usr_quit(struct Editor *);
static int move_to_word(struct Editor *, const lchar_t *str, long size);
static void line_remove_span(struct Line *, int, int);

static void move_brush(struct Editor *, int, int);
static void clear_window(struct Editor *);
static int num_digits(int, int);
static void paint_string(struct Editor *, lchar_t *, int, int, int);
static int calc_padlx(struct Editor *);
static void calc_window_size(struct Editor *);
static int replace_word_positrange(struct Editor *, struct Selection *,
                                   lchar_t *, int, lchar_t *, int);
static int delete_lines(struct Editor *, struct Selection *);
static int delete_lines_positrange(struct Editor *, struct Selection *);
static int delete_vert(struct Editor *edp, struct Selection *);

static int replace_word(struct Editor *, struct Selection *, lchar_t *,
                        int, lchar_t *, int);
static int exec_command(struct Editor *, lchar_t *, int);
static int edit_vert(struct Editor *);
static int iter_split(struct SplitIter *, struct Range *);
static void init_split_iter(struct SplitIter *, lchar_t *, int, int, int);
static int insert_vert(struct Editor *, lchar_t);
static lchar_t *u32str_convert(const char *);
static char * u8str_convert(lchar_t *, int);
static void init_docfile(struct Editor *, const char *);
static lchar_t denormalize_xcale(unsigned char);
static int normalize_xcape(lchar_t);
static void file_chooser(struct Editor *, enum FILE_ACT);

static void
diffstk_incr(struct DiffStk *diffstk) {
    int i;

    if (diffstk->curr < diffstk->size) {
        for (i = diffstk->curr; i < diffstk->size; i++) {
            if (diffstk->data[i].type > DIFF_TYPE_INLINE) {
                unlikely_if_(diffstk->data[i].type == DIFF_TYPE_SUBSTCK) {
                    diffstk_free_all(diffstk->data[i].diff_sub);
                }
                free(diffstk->data[i].data);
                diffstk->data[i].type = 0;
            }
        }
    }
    if (diffstk->curr < diffstk->curr_save_point) {
        diffstk->curr_save_point = -1;
    }
    diffstk->curr++;
    diffstk->size = diffstk->curr;
}

static void
diffstk_reserve(struct DiffStk **diffstk) {
    struct Diff *beg;
    struct Diff *end;

    if ((*diffstk)->size == (*diffstk)->alloc) {
        xrealloc_arr(diffstk, (*diffstk)->alloc * 2 + 1);
        (*diffstk)->alloc = (*diffstk)->alloc * 2 + 1;
    }
    beg = (*diffstk)->data + (*diffstk)->size;
    end = (*diffstk)->data + (*diffstk)->alloc;
    memset(beg, 0, (end - beg) * sizeof(*(*diffstk)->data));
}

static void
diffstk_insert_addbk(struct Editor *edp, int y, int x) {
    long pos;
    struct Diff *curr;
    struct Diff *prev;

    unlikely_if_(edp->diffstk == NULL) {
        return;
    }
    diffstk_reserve(&edp->diffstk);

    pos  = edp->diffstk->curr;
    curr = edp->diffstk->data + pos;
    prev = edp->diffstk->data + pos - 1;

    if (pos == 0 || prev->type != DIFF_TYPE_ADDBRK ||
        prev->size == MAX_DIFF_SIZE) {
        diffstk_incr(edp->diffstk);
        curr->type = DIFF_TYPE_ADDBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    } else if (prev->y == edp->cursy - prev->size && edp->cursx == 0) {
        prev->size++;
    } else {
        diffstk_incr(edp->diffstk);
        curr->type = DIFF_TYPE_ADDBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    }
}

static void
diffstk_insert_delbk(struct Editor *edp, int y, int x) {
    long pos;
    struct Diff *curr;
    struct Diff *prev;

    unlikely_if_(edp->diffstk == NULL) {
        return;
    }
    diffstk_reserve(&edp->diffstk);

    pos  = edp->diffstk->curr;
    curr = edp->diffstk->data + pos;
    prev = edp->diffstk->data + pos - 1;

    if (pos == 0 || prev->type != DIFF_TYPE_DELBRK ||
        prev->size == MAX_DIFF_SIZE) {
        diffstk_incr(edp->diffstk);
        curr->type = DIFF_TYPE_DELBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    } else if (prev->y == edp->cursy + prev->size && prev->x == 0) {
        prev->x = edp->cursx;
        prev->y--;
        prev->size++;
    } else {
        diffstk_incr(edp->diffstk);
        curr->type = DIFF_TYPE_DELBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    }
}

static int
eq_bigch(struct Diff *diff, enum DIFF_TYPE small) {
    assert(small == DIFF_TYPE_DELCHR_SMALL || small == DIFF_TYPE_ADDCHR_SMALL);

    if (small == DIFF_TYPE_DELCHR_SMALL) {
        return diff->type == DIFF_TYPE_DELCHR ||
               diff->type == DIFF_TYPE_DELCHR_SMALL;
    } else {
        return diff->type == DIFF_TYPE_ADDCHR ||
               diff->type == DIFF_TYPE_ADDCHR_SMALL;
    }
}

static void
diffstk_insert_span(struct Editor *edp, lchar_t *str, int delta) {
    long pos;
    struct Diff *curr;
    struct Diff *prev;
    enum DIFF_TYPE type;
    lchar_t sample;
    filt_fn_t curr_fil;
    filt_fn_t prev_fil;

    unlikely_if_(edp->diffstk == NULL) {
        return;
    }
    diffstk_reserve(&edp->diffstk);

    pos  = edp->diffstk->curr;
    curr = edp->diffstk->data + pos;
    prev = edp->diffstk->data + pos - 1;

    if (delta < 0) {
        type = DIFF_TYPE_DELCHR_SMALL;
        sample = str[-1];

    } else {
        type = DIFF_TYPE_ADDCHR_SMALL;
        sample = str[0];
    }
    curr_fil = find_match_fil(sample);

    if (pos == 0 || !eq_bigch(prev, type) || prev->size == MAX_DIFF_SIZE) {
        diffstk_incr(edp->diffstk);
        curr->type = type;
        curr->size = 0;
        curr->x = edp->cursx;
        curr->y = edp->cursy;
        diff_insert_span(curr, str, delta);
        return;
    }
    if (prev->type == DIFF_TYPE_DELCHR_SMALL ||
        prev->type == DIFF_TYPE_ADDCHR_SMALL) {
        sample = prev->content[0];
    } else {
        sample = prev->data[0];
    }
    prev_fil = find_match_fil(sample);
    if (prev->y == edp->cursy && prev->x + prev->size == edp->cursx &&
        curr_fil == prev_fil) {
        diff_insert_span(prev, str, delta);
    } else {
        diffstk_incr(edp->diffstk);
        curr->type = type;
        curr->size = 0;
        curr->x = edp->cursx;
        curr->y = edp->cursy;
        diff_insert_span(curr, str, delta);
    }
}

static void
insert_doc(struct LineArr **doc, int pos, struct Line *cpy, int size) {
    struct Line *beg;
    struct Line *end;

    assert((*doc)->size <= (*doc)->alloc);
    assert(pos <= (*doc)->size);

    if ((*doc)->size + size > (*doc)->alloc) {
        xrealloc_arr(doc, (*doc)->alloc * 2 + size);
        (*doc)->alloc = (*doc)->alloc * 2 + size;
    }
    beg = (*doc)->data + pos;
    end = (*doc)->data + (*doc)->size;

    memmove(beg + size, beg, (end - beg) * sizeof(*(*doc)->data));
    memcpy(beg, cpy, size * sizeof(*(*doc)->data));
    (*doc)->size += size;
}

static void
insert_doc_nl_times(struct LineArr **doc, int pos, int size) {
    struct Line *beg;
    struct Line *end;

    assert((*doc)->size <= (*doc)->alloc);
    assert(pos <= (*doc)->size);

    if ((*doc)->size + size > (*doc)->alloc) {
        xrealloc_arr(doc, (*doc)->alloc * 2 + size);
        (*doc)->alloc = (*doc)->alloc * 2 + size;
    }
    beg = (*doc)->data + pos;
    end = (*doc)->data + (*doc)->size;

    memmove(beg + size, beg, (end - beg) * sizeof(*(*doc)->data));
    memset(beg, 0, size * sizeof(*(*doc)->data));
    (*doc)->size += size;
}

static void
line_insert(struct Line *line, int pos, lchar_t *cpy, int size) {
    lchar_t *beg;
    lchar_t *end;

    assert(line->size <= line->alloc);
    assert(pos <= line->size);

    if (line->size + size > line->alloc) {
        xrealloc_owndata(line, line->alloc * 2 + size);
        line->alloc = line->alloc * 2 + size;
    }
    beg = line->data + pos;
    end = line->data + line->size;

    memmove(beg + size, beg, (end - beg) * sizeof(*line->data));
    memcpy(beg, cpy, size * sizeof(*line->data));
    line->size += size;
}

static void
merge_lines(struct LineArr *doc, int pos) {
    struct Line *line = &doc->data[pos];
    struct Line *next = line + 1;
    struct Line *end;
    struct Line *beg;

    assert(doc->size > 1);
    assert(pos + 1 < doc->size);

    line_insert(line, line->size, next->data, next->size);
    if (next->alloc) {
        assert(next->data);
        free(next->data);
    }
    beg = next + 1;
    end = doc->data + doc->size;
    memmove(next, beg, (end - beg) * sizeof(*doc->data));
    doc->size--;
}

static void
line_remove_span(struct Line *line, int pos, int delta) {
    lchar_t *beg  = line->data + pos;
    lchar_t *prev = line->data + pos + delta;
    lchar_t *end  = line->data + line->size;

    assert(beg <= end);
    memmove(prev, beg, (end - beg) * sizeof(*line->data));
    line->size += delta;
}

static void
line_remove_span_qdiff(struct Editor *edp, struct Line *line, int pos,
                       int delta) {
    assert(delta);
    diffstk_insert_span(edp, line->data + pos, delta);
    line_remove_span(line, pos, delta);
}

static filt_fn_t
find_match_fil(lint_t ch) {
    filt_fn_t fil = NULL;
    int i;

    for (i = 0; i < arrsize(filts); i++) {
        if (filts[i](ch)) {
            fil = filts[i];
            break;
        }
    }
    assert(fil);

    return fil;
}

static int
find_prev_simil(const void *data, int cursor) {
    const lint_t *usrin = data;
    lint_t ch = usrin[cursor - 1];
    filt_fn_t fil = find_match_fil(ch);

    while (cursor && fil(usrin[cursor - 1])) {
        cursor--;
    }
    return cursor;
}

static int
find_next_simil(const void *data, int cursor, int size) {
    const lint_t *usrin = data;
    lint_t ch = usrin[cursor];
    filt_fn_t fil = find_match_fil(ch);

    while (cursor < size && fil(usrin[cursor])) {
        cursor++;
    }
    return cursor;
}

static int
realloc_ptr(void *p, size_t nmemb, size_t size, size_t head) {
    void **ptr = p;
    size_t nsize = nmemb * size;

    if (INT_MAX - head < nsize && nmemb && size && INT_MAX / nmemb < size) {
        errno = ENOMEM;
        return -1;
    }
    nsize = nsize + head;
    if ((*ptr = realloc(*ptr, nsize)) == NULL && nsize) {
        return -1;
    }
    return 0;
}

static void
xrealloc_ptr(void *p, size_t nmemb, size_t size, size_t head) {
    if (realloc_ptr(p, nmemb, size, head) == -1) {
        err(1, "realloc");
    }
}

static void
close_win(void) {
    endwin();
}

static void
open_win(struct Editor *edp) {
    initscr();
    noecho();
    raw();
    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);
    set_tabsize(TAB_SIZE);
    edp->win.x = COLS;
    edp->win.y = LINES - 1;
    if (edp->win.y > 1) {
        edp->win.y--;
    }
    edp->win.pgspan = edp->win.y / 4 * 3;
}

static void
init_docfile(struct Editor *edp, const char * fname) {
    memset(edp, 0, sizeof(edp->pad));

    xrealloc_arr(&edp->doc, INIT_DOC);
    xrealloc_arr(&edp->diffstk, INIT_DIFF);

    xinit_doc(edp, INIT_DOC, fname);

    edp->diffstk->curr_save_point = 0;

    memset(edp->diffstk, 0,
           sizeof(struct DiffStk) + sizeof(*edp->diffstk->data) * INIT_DIFF);
    edp->diffstk->alloc = INIT_DIFF;
    edp->diffstk->size = 0;
}

static struct Editor *
init_editor(const char *fname) {
    static struct Editor editor;
    static struct Editor search;
    static struct Editor command;
    static struct Editor files;
    struct Editor *edp = &editor;
    struct Line *line;
    lchar_t *open_fname;
    size_t size;

    setlocale(LC_ALL, "");

    xrealloc_ptr(&edp->docs, 1, sizeof(*edp->docs), 0);

    init_docfile(edp, fname);

    open_win(edp);
    atexit(close_win);

    xinit_doc(&search, INIT_SEARCH, NULL);
    search.mode = MODE_SEARCH;
    search.win.y = 1;
    search.cursy = search.doc->size - 1;

    xinit_doc(&command, INIT_COMMAND, NULL);
    command.mode = MODE_COMMAND;
    command.win.y = 1;
    command.cursy = command.doc->size - 1;

    xinit_doc(&files, 2, NULL);
    files.doc->size = 2;
    files.mode = MODE_FILES;
    files.win.y = 1;
    files.cursy = 0;

    edp->search  = &search;
    edp->command = &command;
    edp->files   = &files;

    line = &edp->files->doc->data[0];
    if (fname == NULL || strlen(fname) == 0) {
        line_insert(line, edp->cursx, L""UNNAMED_FNAME,
                    strsize(UNNAMED_FNAME));
    } else {
        open_fname = u32str_convert(fname);
        size = wcslen(open_fname);
        line_insert(line, edp->cursx, open_fname, size);
    }
    edp->filename = &edp->files->doc->data[0];

    /* share search history between command and normal */
    edp->command->search = edp->search;

    edp->win.fullx = COLS;
    edp->win.y = LINES;
    if (edp->win.y > 1) {
        edp->win.y--;
    }
    edp->opt = OPT_NONE;

    start_color();
    use_default_colors();
    init_pair(1, COLOR_WHITE, COLOR_RED);
    return edp;
}

static void
xinit_doc(struct Editor *edp, size_t sz, const char *fname) {
    enum EFILE efile;

    xrealloc_arr(&edp->doc, sz);
    memset(edp->doc, 0, sizeof(struct LineArr) + sizeof(*edp->doc->data) * sz);
    edp->doc->alloc = sz;
    edp->doc->size = 0;

    if (fname == NULL) {
        edp->doc->size = 1;
        return;
    }
    efile = load_file_utf8(edp, fname);
    if (efile != EFILE_OK) {
        if (efile == EFILE_UTF8) {
            errx(1, "invalid utf8 content in '%s'", fname);
        } else if (efile == EFILE_OPEN) {
            err(1, "fopen '%s'", fname);
        } else {
            edp->doc->size = 1;
        }
    }
    edp->fileinfo.fname = strdup(fname);
    assert(edp->fileinfo.fname);
}

static int
move_right(struct Editor *edp) {
    struct Line *line = &edp->doc->data[edp->cursy];

    if (edp->mode == MODE_SELECT_VERT) {
        edp->selct.xend++;
        return 0;
    }
    if (edp->cursx < line->size) {
        edp->cursx++;
    } else if (edp->cursy < edp->doc->size - 1) {
        unlikely_if_(edp->mode == MODE_COMMAND || edp->mode == MODE_FILES ||
                     edp->mode == MODE_SEARCH) {
            return -1;
        }
        edp->cursy++;
        edp->cursx = 0;
    } else {
        return -1;
    }
    return 0;
}

static int
move_left(struct Editor *edp) {
    unlikely_if_(edp->mode == MODE_SELECT_VERT && edp->selct.xend) {
        edp->selct.xend--;
        return 0;
    }
    if (edp->cursy == 0 && edp->cursx == 0) {
        return -1;
    } else if (edp->cursx != 0) {
        edp->cursx--;
    } else {
        edp->cursy--;
        edp->cursx = edp->doc->data[edp->cursy].size;
    }
    return 0;
}

static int
del_back(struct Editor *edp) {
    struct Line *line;

    unlikely_if_(edp->cursy == 0 && edp->mode == MODE_FILES) {
        return -1;
    }
    if (edp->cursy == 0 && edp->cursx == 0) {
        return -1;
    }
    if (edp->cursx == 0) {
        if (edp->mode != MODE_NORMAL && edp->mode != MODE_SELECT_VERT) {
            return -1;
        }
        edp->cursy--;
        line = &edp->doc->data[edp->cursy];
        edp->cursx = line->size;
        merge_lines(edp->doc, edp->cursy);

        diffstk_insert_delbk(edp, edp->cursy, edp->cursx);
    } else {
        line = &edp->doc->data[edp->cursy];
        line_remove_span_qdiff(edp, line, edp->cursx, -1);
        edp->cursx--;
    }
    return 0;
}

static int
move_up(struct Editor *edp) {
    struct Line *line = &edp->doc->data[edp->cursy];

    if (edp->cursy == 0) {
        if (edp->cursx != 0) {
            edp->cursx = 0;
            return 0;
        } else {
            return -1;
        }
    }
    edp->cursy--;
    line--;
    if (edp->cursx > line->size) {
        edp->cursx = line->size;
    }
    return 0;
}

static int
move_pgup(struct Editor *edp) {
    static struct Line *line;

    if (edp->cursy - edp->win.pgspan < 0) {
        edp->cursy = 0;
    } else {
        edp->cursy -= edp->win.pgspan;
    }
    line = &edp->doc->data[edp->cursy];

    if (edp->cursx <= line->size) {
        return 0;
    } else {
        edp->cursx = line->size;
    }
    return 0;
}

static int
move_pgdown(struct Editor *edp) {
    static struct Line *line;

    if (edp->cursy + edp->win.pgspan >= edp->doc->size) {
        edp->cursy = edp->doc->size - 1;
    } else {
        edp->cursy += edp->win.pgspan;
    }
    line = &edp->doc->data[edp->cursy];

    if (edp->cursx <= line->size) {
        return 0;
    } else {
        edp->cursx = line->size;
    }
    return 0;
}

static int
move_down(struct Editor *edp) {
    struct Line *line = &edp->doc->data[edp->cursy];

    if (edp->cursy == edp->doc->size - 1) {
        if (edp->cursx != line->size) {
            edp->cursx = line->size;
            return 0;
        } else {
            return -1;
        }
    }
    edp->cursy++;
    line++;
    if (edp->cursx > line->size) {
        edp->cursx = line->size;
    }
    return 0;
}

static int
move_down_natural(struct Editor *edp) {
    struct Line *line = &edp->doc->data[edp->cursy];
    lchar_t *beg = &line->data[edp->cursx];
    lchar_t *end = render_max_given_width(&edp->win, beg,
                                          line->data + line->size, edp->win.x);

    if (edp->mode == MODE_SELECT_VERT &&
        edp->selct.ybeg < edp->selct.yend - 1) {
        edp->selct.ybeg++;
        return 0;
    }
    if (end != line->data + line->size) {
        edp->cursx += end - beg;
        return 0;
    } else {
        move_down(edp);
    }
    return 0;
}

static int
move_up_natural(struct Editor *edp) {
    struct Line *line = &edp->doc->data[edp->cursy];
    lchar_t *end = &line->data[edp->cursx];
    lchar_t *beg = render_back_max_given_width(&edp->win, end, line->data - 1,
                                               edp->win.x);

    if (edp->mode == MODE_SELECT_VERT && edp->selct.ybeg) {
        edp->selct.ybeg--;
        return 0;
    }
    if (beg < line->data || line->size == 0) {
        move_up(edp);
        return 0;
    } else {
        edp->cursx -= end - beg;
    }
    return 0;
}

static void
diff_apply_brk(struct Editor *edp, struct Diff *diff, enum DIREC direc) {
    struct Line *line;
    struct Line own = { 0, 0, 0 };
    int i;

    edp->cursx = diff->x;
    edp->cursy = diff->y;
    line = &edp->doc->data[edp->cursy];
    if (((diff->type == DIFF_TYPE_ADDBRK) ^ (direc == DIREC_FORW)) == 0) {
        assert(edp->cursy >= 0);
        for (i = 0; i < diff->size; i++) {
            merge_lines(edp->doc, edp->cursy);
        }
    } else {
        assert(diff->size);
        line_insert(&own, 0, line->data + edp->cursx, line->size - edp->cursx);
        line->size = edp->cursx;
        insert_doc_nl_times(&edp->doc, edp->cursy + 1, diff->size - 1);
        insert_doc(&edp->doc, edp->cursy + diff->size, &own, 1);
        edp->cursx = 0;
        edp->cursy += diff->size;
    }
}

static void
diff_apply_chr(struct Editor *edp, struct Diff *diff, enum DIREC direc) {
    struct Line *line;
    lchar_t *beg;
    lchar_t *to;
    lchar_t *end;
    int i;

    edp->cursx = diff->x;
    edp->cursy = diff->y;
    line = &edp->doc->data[edp->cursy];

    if ((eq_bigch(diff, DIFF_TYPE_ADDCHR_SMALL) ^ (direc == DIREC_FORW))
        == 0) {
        if (direc == DIREC_FORW) {
            to = line->data + edp->cursx;
            beg = line->data + edp->cursx + diff->size;
        } else {
            to = line->data + edp->cursx - diff->size;
            beg = line->data + edp->cursx;
            edp->cursx -= diff->size;
        }
        end = line->data + line->size;
        memmove(to, beg, (end - beg) * sizeof(*line->data));
        line->size -= diff->size;
    } else {
        if (line->size + diff->size > line->alloc) {
            xrealloc_owndata(line, line->alloc * 2 + diff->size);
            line->alloc = line->alloc * 2 + diff->size;
        }
        beg = line->data + edp->cursx - diff->size;
        end = line->data + line->size;

        memmove(beg + diff->size, beg, (end - beg) * sizeof(*line->data));
        line->size += diff->size;
        if (direc == DIREC_FORW) {
            if (diff->type <= DIFF_TYPE_INLINE) {
                for (i = diff->size; i; i--) {
                    beg[diff->size - i] = diff->content[i - 1];
                }
            } else {
                for (i = diff->size; i; i--) {
                    beg[diff->size - i] = diff->data[i - 1];
                }
            }
        } else {
            if (diff->type <= DIFF_TYPE_INLINE) {
                for (i = 0; i < diff->size; i++) {
                    beg[diff->size + i] = diff->content[i];
                }
            } else {
                for (i = 0; i < diff->size; i++) {
                    beg[diff->size + i] = diff->data[i];
                }
            }
            edp->cursx += diff->size;
        }
    }
}

static int
diffstk_apply_last(struct Editor *edp, enum DIREC direc) {
    if (edp->mode == MODE_SELECT_VERT) {
        /* not yet supported */
        return -1;
    }
    if (edp->diffstk == NULL) {
        return -1;
    }
    struct Diff *diff = edp->diffstk->data + edp->diffstk->curr - 1;
    int delta;

    if (direc == DIREC_FORW) {
        delta = -1;
        diff = edp->diffstk->data + edp->diffstk->curr - 1;
    } else {
        delta = 1;
        diff = edp->diffstk->data + edp->diffstk->curr;
    }

    if (direc == DIREC_FORW && edp->diffstk->curr == 0) {
        return -1;
    } else if (direc == DIREC_BACK &&
               edp->diffstk->curr == edp->diffstk->size) {
        return -1;
    }
    switch (diff->type) {
    default:
        return -1;
    case DIFF_TYPE_ADDBRK:
    case DIFF_TYPE_DELBRK:
        diff_apply_brk(edp, diff, direc);
        break;
    case DIFF_TYPE_ADDCHR:
    case DIFF_TYPE_ADDCHR_SMALL:
    case DIFF_TYPE_DELCHR:
    case DIFF_TYPE_DELCHR_SMALL:
        diff_apply_chr(edp, diff, direc);
        break;
    case DIFF_TYPE_SUBSTCK:
        diffstk_apply_all(edp, diff->diff_sub, direc);
        break;
    }
    edp->diffstk->curr += delta;
    return 0;
}

static int
diffstk_apply_all(struct Editor *edp, struct DiffStk *ds, enum DIREC direc) {
    int delta;
    struct Diff *curr;
    struct Diff *end;

    if (ds->size == 0) {
        return -1;
    }
    if (direc == DIREC_FORW) {
        delta = -1;
        curr = ds->data + ds->size - 1;
        end = ds->data - 1;
    } else {
        delta = 1;
        curr = ds->data;
        end = ds->data + ds->size;
    }
    while (curr != end) {
        switch (curr->type) {
        default:
        case DIFF_TYPE_ADDBRK:
        case DIFF_TYPE_DELBRK:
            diff_apply_brk(edp, curr, direc);
            break;
        case DIFF_TYPE_ADDCHR:
        case DIFF_TYPE_ADDCHR_SMALL:
        case DIFF_TYPE_DELCHR:
        case DIFF_TYPE_DELCHR_SMALL:
            diff_apply_chr(edp, curr, direc);
            break;
        case DIFF_TYPE_SUBSTCK:
            diffstk_apply_all(edp, curr->diff_sub, direc);
            break;
        }
        curr += delta;
    }
    return 0;
}

static void
diffstk_free_all(struct DiffStk *ds) {
    struct Diff *curr = ds->data;
    struct Diff *end = ds->data + ds->size;

    for (; curr != end; curr++) {
        if (curr->type <= DIFF_TYPE_INLINE) {
            continue;
        }
        if (curr->type == DIFF_TYPE_SUBSTCK) {
            diffstk_free_all(curr->diff_sub);
        }
        free(curr->data);
    }
}

static void
save_current(struct LineArr *doc, struct FileInfo *info,
             struct DiffStk *diffstk) {
    if (diffstk && diffstk->curr == diffstk->curr_save_point) {
        return;
    }
    save_file_utf8(doc, info, diffstk);
}


static int
handle_input(struct Editor *edp, lint_t c) {
    static struct DocFile stash;
    struct Line own = { 0, 0, 0 };
    struct Line *line = &edp->doc->data[edp->cursy];
    int rest;
    lchar_t ch = c;
    const char *key;
    int cont;
    int mode;

    assert(edp->cursy < edp->doc->size);
    assert(edp->cursx <= line->size);

    switch (ch) {
    default:
        unlikely_if_(edp->mode == MODE_SELECT_HORIZ) {
            delete_lines(edp, &edp->selct);
            line = &edp->doc->data[edp->cursy];
        } else unlikely_if_(edp->mode == MODE_SELECT_VERT) {
            if (iswprint(ch) || ch == L'\t') {
                insert_vert(edp, ch);
            }
            break;
        } else unlikely_if_(edp->mode == MODE_FILES && !is_ascii(&ch, 1)) {
            break;
        }
        if (iswprint(ch) || ch == L'\t') {
            diffstk_insert_span(edp, &ch, 1);
            line = &edp->doc->data[edp->cursy];
            line_insert(line, edp->cursx, &ch, 1);
            edp->cursx++;
            edp->selct.xbeg++;
            edp->selct.xend++;
        }
        if (edp->mode == MODE_SELECT_HORIZ) {
            return 0;
        }
        break;
    case KEY_CTRL_CANC:
        return 0;
    case L'\n':
        if (edp->mode == MODE_SEARCH || edp->mode == MODE_COMMAND) {
            if (edp->cursx == 0) {
                if (edp->cursy != edp->doc->size - 1) {
                    line = edp->doc->data + edp->cursy;
                    edp->cursx = line->size;
                    merge_lines(edp->doc, edp->cursy);
                    edp->cursy = edp->doc->size - 1;
                    edp->cursx = 0;
                    edp->exec_line = NULL;
                }
            } else {
                edp->exec_line = edp->doc->data + edp->cursy;
                insert_doc(&edp->doc, edp->doc->size, &own, 1);
                edp->cursy = edp->doc->size - 1;
                edp->cursx = 0;
            }
            return 0;
        } else if (edp->mode >= MODE_SELECT_HORIZ) {
            return 0;
        } else if (edp->mode == MODE_FILES) {
            return 0;
        }
        diffstk_insert_addbk(edp, edp->cursy, edp->cursx);
        line_insert(&own, 0, line->data + edp->cursx, line->size - edp->cursx);
        insert_doc(&edp->doc, edp->cursy + 1, &own, 1);
        line->size = edp->cursx;
        edp->cursx = 0;
        edp->cursy++;
        break;
    case KEY_RIGHT:
        move_right(edp);
        break;
    case KEY_LEFT:
        move_left(edp);
        break;
    case KEY_UP:
        move_up(edp);
        break;
    case KEY_DOWN:
        move_down(edp);
        break;

    case KEY_CTRL_UP:
    case KEY_SHIFT_UP:
        move_up_natural(edp);
        break;

    case KEY_CTRL_DOWN:
    case KEY_SHIFT_DOWN:
        move_down_natural(edp);
        break;

    case KEY_CTRL_LEFT:
    case KEY_SHIFT_LEFT:
        if (edp->mode == MODE_SELECT_VERT) {
            if (edp->selct.xbeg) {
                edp->selct.xbeg--;
                edp->selct.xend--;
            }
            return 1;
        }
        if (edp->cursy == 0 && edp->cursx == 0) {
            break;
        } else if (edp->cursx == 0) {
            edp->cursx = (line - 1)->size;
            edp->cursy--;
        } else {
            edp->cursx = find_prev_simil(line->data, edp->cursx);
        }
        break;

    case KEY_CTRL_RIGHT:
    case KEY_SHIFT_RIGHT:
        if (edp->mode == MODE_SELECT_VERT) {
            edp->selct.xbeg++;
            edp->selct.xend++;
            return 1;
        }
        if (edp->cursx < line->size) {
            edp->cursx = find_next_simil(line->data, edp->cursx, line->size);
        } else if (edp->cursy < edp->doc->size - 1) {
            unlikely_if_(edp->mode == MODE_COMMAND || edp->mode == MODE_FILES ||
                         edp->mode == MODE_SEARCH) {
                return -1;
            }
            edp->cursx = 0;
            edp->cursy++;
        }
        break;

    case KEY_PGUP:
        move_pgup(edp);
        break;
    case KEY_PGDOWN:
        move_pgdown(edp);
        break;
    case KEY_CTRL_PGUP:
    case KEY_CTRL_PGDOWN:
        break;
    case_key(23, "^W")
        if (edp->cursx == 0) {
            del_back(edp);
            break;
        }
        line = &edp->doc->data[edp->cursy];
        rest = find_prev_simil(line->data, edp->cursx);
        line_remove_span_qdiff(edp, line, edp->cursx, rest - edp->cursx);
        edp->cursx = rest;
        break;

    case_key(18, "^R")
        diffstk_apply_last(edp, DIREC_BACK);
        break;
    case_key(21, "^U")
        diffstk_apply_last(edp, DIREC_FORW);
        break;
    case_key(19, "^S")
        save_current(edp->doc, &edp->fileinfo, edp->diffstk);
        break;
    case_key(17, "^Q")
        return usr_quit(edp);
    case_key(24, "^X")
        save_current(edp->doc, &edp->fileinfo, edp->diffstk);
        return 0;
    case_key(31, "^_")
        if (edp->mode == MODE_SEARCH) {
            break;
        }
        edp->search->win.fullx = edp->win.x;
        edp->search->win.offy = edp->win.y - edp->search->win.y;

        do {
            cont = render_loop(edp->search);
        } while (cont);

        edp->exec_line = edp->search->exec_line;

        if (edp->exec_line && edp->exec_line->size) {
            move_to_word(edp, edp->exec_line->data, edp->exec_line->size);
        }
        break;
    case_key(14, "^N")
        if (edp->exec_line && edp->exec_line->size) {
            move_to_word(edp, edp->exec_line->data, edp->exec_line->size);
        }
        break;

    case KEY_BACKSPACE:
        if (edp->mode == MODE_SELECT_HORIZ) {
            delete_lines(edp, &edp->selct);
            return 0;
        }
        if (edp->mode == MODE_SELECT_VERT) {
            if (edp->selct.xbeg == 0 && edp->selct.xend == 0) {
                delete_vert(edp, &edp->selct);
                return 0;
            }
            delete_vert(edp, &edp->selct);
            break;
        }
        del_back(edp);
        break;
    case KEY_DC:
        if (move_right(edp) == -1) {
            break;
        }
        del_back(edp);
        break;
    case KEY_RESIZE:
        erase();
        edp->win.fullx = COLS;
        edp->win.y = LINES;
        if (edp->win.y > 1) {
            edp->win.y--;
        }
        edp->win.pgspan = edp->win.y / 4 * 3;
        calc_window_size(edp);
        reposition_frame(edp);
        render_lines(edp);
        reposition_cursor(edp);
        refresh();
        break;
    case_key(39, "^?")
        /* fix combination */
        if (edp->mode != MODE_NORMAL) {
            break;
        }
        show_keymap();
        lint_t ignore;
        get_wch(&ignore);
        break;
    case_key(8, "^H")
        mode = edp->mode;
        if (mode != MODE_NORMAL) {
            break;
        }
        edp->mode = MODE_SELECT_HORIZ;
        edp->selct.ybeg = edp->cursy;
        edp->selct.yend = edp->cursy + 1;
        do {
            cont = render_loop(edp);
        } while (cont);
        edp->mode = mode;
        break;
    case_key(22, "^V")
        mode = edp->mode;
        if (mode != MODE_NORMAL) {
            break;
        }
        edp->mode = MODE_SELECT_VERT;
        edp->selct.ybeg = edp->cursy;
        edp->selct.yend = edp->cursy + 1;
        edp->selct.xbeg = edp->cursx;
        edp->selct.xend = edp->cursx;

        edit_vert(edp);
        edp->mode = mode;
        break;
    case_key(5, "^E")
        if (edp->mode != MODE_NORMAL) {
            break;
        }
        edp->command->win.fullx = edp->win.x;
        edp->command->win.offy  = edp->win.y - edp->command->win.y;
        do {
            cont = render_loop(edp->command);
        } while (cont);
        if (edp->command->exec_line && edp->command->exec_line->size) {
            line = edp->command->exec_line;
            exec_command(edp, line->data, line->size);
        }
        break;
    case_key(15, "^O")
        file_chooser(edp, FILE_ACT_OPEN);
        break;
    case_key(1, "^A")
        file_chooser(edp, FILE_ACT_RENAME);
        break;
    }
    edp->selct.yend = edp->cursy + 1;
    return 1;
}

static void
file_chooser(struct Editor *edp, enum FILE_ACT action) {
    struct Line own = { 0, 0, 0 };
    struct Line *line = &edp->doc->data[edp->cursy];
    int cont;
    int cursy;

    if (edp->mode != MODE_NORMAL) {
        return;
    }
    edp->files->win.fullx = edp->win.x;
    edp->files->win.offy = edp->win.y - edp->files->win.y;

    cursy = edp->files->cursy;
    memcpy(edp->docs + cursy, edp, sizeof(*edp->docs));

    if (action == FILE_ACT_OPEN) {
        edp->files->cursy = edp->files->doc->size - 1;
    }
    edp->files->cursx = edp->files->doc->data[edp->files->cursy].size;

    do {
        cont = render_loop(edp->files);
    } while (cont);

    if (edp->files->cursy != edp->files->doc->size - 1) {
        memcpy(edp, edp->docs + edp->files->cursy, sizeof(*edp->docs));
    } else if (edp->files->doc->data[edp->files->cursy].size == 0) {
            edp->files->cursy = cursy;
            edp->files->cursx = 0;
    } else {
        line = &edp->files->doc->data[edp->files->cursy];
        init_docfile(edp, u8str_convert(line->data, line->size));
        insert_doc(&edp->files->doc, edp->files->doc->size, &own, 1);
    }
    edp->filename = &edp->files->doc->data[edp->files->cursy];
}

static void
calc_window_size(struct Editor *edp) {
    edp->win.offx = calc_padlx(edp);
    edp->win.x = edp->win.fullx - edp->win.offx;
}

static int
render_loop(struct Editor *edp) {
    lint_t ch;

    clear_window(edp);
    calc_window_size(edp);
    reposition_frame(edp);
    render_lines(edp);
    render_editor_info(edp);
    reposition_cursor(edp);

    if (get_wch(&ch) == ERR) {
        return 1;
    }
    return handle_input(edp, ch);
}

int
main(int argc, char *argv[]) {
    static struct Editor *edp;
    char *fname;
    int cont;

    if (argc - 1 != 1) {
        fname = UNNAMED_FNAME;
    } else {
        fname = argv[1];
    }
    edp = init_editor(fname);
    do {
        cont = render_loop(edp);
    } while (cont);
    return 0;
}

static int
count_render_width_upto(struct Window *win, struct Line *line, int size) {
    int nlines = 0;
    int currline = 0;
    int char_w;
    int i;

    for (i = 0; i < size && i < line->size; i++) {
        char_w = wcwidth(line->data[i]);
        assert(char_w < win->x);
        if (char_w != -1) {
            if (currline + char_w > win->x) {
                nlines++;
                currline = char_w;
            } else {
                currline += char_w;
            }
        } else {
            assert(line->data[i] == L'\t');
            currline /= TAB_SIZE;
            currline++;
            currline *= TAB_SIZE;
            if (currline > win->x) {
                nlines++;
                currline = 0;
            }
        }
    }
    return nlines * win->x + currline;
}

static lchar_t *
render_max_given_width(struct Window *win, lchar_t *beg, lchar_t *end,
                       int width) {
    int currline = 0;
    int char_w;

    while (beg < end && currline < width) {
        char_w = wcwidth(*beg);
        assert(char_w < win->x);
        if (char_w != -1) {
            if (currline + char_w > win->x) {
                break;
            } else {
                currline += char_w;
            }
        } else {
            assert(*beg == L'\t');
            currline /= TAB_SIZE;
            currline++;
            currline *= TAB_SIZE;
            if (currline > win->x) {
                beg++;
                break;
            }
        }
        beg++;
    }
    return beg;
}

static lchar_t *
render_back_max_given_width(struct Window *win, lchar_t *beg, lchar_t *end,
                            int width) {
    int currline = 0;
    int char_w;

    while (beg > end && currline < width) {
        char_w = wcwidth(*beg);
        assert(char_w < win->x);
        if (char_w != -1) {
            if (currline + char_w > win->x) {
                break;
            } else {
                currline += char_w;
            }
        } else {
            assert(*beg == L'\t');
            currline /= TAB_SIZE;
            currline++;
            currline *= TAB_SIZE;
            if (currline > win->x) {
                beg--;
                break;
            }
        }
        beg--;
    }
    return beg;
}

static int
count_nlines_upto(struct Editor *edp, struct Line *line, int size) {
    int chars = count_render_width_upto(&edp->win, line, size);

    if (edp->doc->data + edp->cursy == line && edp->cursx == line->size) {
        /* add space for the cursor when is at the end of the line */
        chars++;
    }
    if (chars == 0) {
        return 1;
    }
    return (chars / edp->win.x) + (chars % edp->win.x != 0);
}

static int
count_nlines(struct Editor *edp, struct Line *line) {
    return count_nlines_upto(edp, line, line->size);
}

static int
count_lines(struct Editor *edp) {
    int lines = 0;
    struct Line *line = edp->doc->data + edp->framebeg;

    while (line < edp->doc->data + edp->cursy) {
        lines += count_nlines(edp, line);
        line++;
    }
    return lines + count_nlines_upto(edp, line, edp->cursx);
}

static void
reposition_frame(struct Editor *edp) {
    int overlines;

    if (edp->cursy > edp->framebeg) {
        overlines = count_lines(edp) - edp->win.y;
        while (overlines > 0) {
            overlines -= count_nlines(edp, &edp->doc->data[edp->framebeg]);
            edp->framebeg++;
        }
    } else if (edp->cursy < edp->framebeg) {
        edp->framebeg = edp->cursy;
    }
}

static void
render_lines(struct Editor *edp) {
    struct Line *dbeg = edp->doc->data;
    struct Line *line = edp->doc->data + edp->framebeg;
    struct Line *end  = edp->doc->data + edp->doc->size;
    int lines = 0;
    int rest;
    int i;

    assert(edp->cursy >= 0);
    struct Line *curr_line = &edp->doc->data[edp->cursy];
    int curr_line_size = count_nlines(edp, curr_line);

    move_brush(edp, 0, 0);

    if (curr_line_size > edp->win.y) {
        lchar_t *end = curr_line->data;
        lchar_t *beg;
        while (curr_line_size > edp->win.y) {
            beg = end;
            end = render_max_given_width(&edp->win, end,
                                         curr_line->data + curr_line->size,
                                         edp->win.x);
            curr_line_size--;
        }
        for (i = 0; i < edp->win.y; i++) {
            beg = end;
            end = render_max_given_width(&edp->win, end,
                                         curr_line->data + curr_line->size,
                                         edp->win.x);
            paint_string(edp, beg, end - beg, edp->cursy, beg - line->data);
        }
        return;
    }
    while (lines < edp->win.y && line < end) {
        rest = count_nlines(edp, line);
        lchar_t *end = line->data;
        lchar_t *beg;
        for (i = 0; i < rest && lines < edp->win.y; i++) {
            move_brush(edp, lines, 0);
            beg = end;
            end = render_max_given_width(&edp->win, end,
                                         line->data + line->size,
                                         edp->win.x);
            paint_string(edp, beg, end - beg, line - dbeg, beg - line->data);
            lines++;
        }
        line++;
    }
}

static void
render_editor_info(struct Editor *edp) {
    static wchar_t *msg;
    int size = 0;
    lchar_t *str = L"";

    if (edp->win.y < 2) {
        return;
    }
    if (edp->search->exec_line) {
        str = edp->search->exec_line->data;
        size = edp->search->exec_line->size;
    }
    msg = wmkstr_nmt(L"file: %ls\t\t\t#: %.*ls\t\t\t%d,%d\t%lld%%",
                     edp->filename->data,
                     size, str,
                     edp->cursy + 1, edp->cursx + 1,
                     (edp->cursy + 1) * 100 / edp->doc->size);
    move_brush(edp, edp->win.y, 0);
    addwstr(msg);
}

static void
reposition_cursor(struct Editor *edp) {
    struct Line *line = edp->doc->data + edp->framebeg;
    int lines = 0;
    int rest;
    int y;
    int x;

    while (line < edp->doc->data + edp->cursy) {
        lines += count_nlines(edp, line);
        line++;
    }
    rest = count_render_width_upto(&edp->win, line, edp->cursx);
    y = lines + rest / edp->win.x;
    x = rest % edp->win.x;

    if (y > edp->win.y - 1) {
        y = edp->win.y - 1;
    }

    move_brush(edp, y, x + edp->win.offx);


}

static enum EFILE
load_file_utf8(struct Editor *edp, const char *fname) {
    enum EFILE err = OK;
    struct Line linebuf = { 0, 0, 0 };
    unsigned char filebuf[4096];
    FILE *file = fopen(fname, "r");
    unsigned char *beg;
    unsigned char *end;
    ssize_t size;
    lchar_t ch;

    if (file == NULL) {
        if (errno == ENOENT) {
            return EFILE_NOFILE;
        }
        return EFILE_OPEN;
    }
    while ((size = fread(filebuf, sizeof(*filebuf), arrsize(filebuf), file))) {
        beg = filebuf;
        end = filebuf + size;
        while (beg < end) {
            likely_if_((beg[0] >> 7) == 0x00) {
                ch = beg[0];
                beg += 1;
            }
            else if ((beg[0] >> 5) == 0x06) {
                unlikely_if_(beg + 2 > end) {
                    err = EFILE_UTF8;
                    goto FAILREAD;
                }
                unlikely_if_(((beg[1] >> 6) != 0x02)) {
                    err = EFILE_UTF8;
                    goto FAILREAD;
                }
                ch = ((beg[0] & 0x1f) << 0x06) | ((beg[1] & 0x3f) << 0x00);
                beg += 2;
            }
            else if ((beg[0] >> 4) == 0x0e) {
                unlikely_if_(beg + 3 > end) {
                    err = EFILE_UTF8;
                    goto FAILREAD;
                }
                unlikely_if_(((beg[1] >> 6) != 0x02) ||
                             ((beg[2] >> 6) != 0x02)) {
                    err = EFILE_UTF8;
                    goto FAILREAD;
                }
                ch = ((beg[0] & 0x0f) << 0x0c) | ((beg[1] & 0x3f) << 0x06) |
                     ((beg[2] & 0x3f) << 0x00);
                beg += 3;
            }
            else if ((beg[0] >> 3) == 0x1e) {
                unlikely_if_(beg + 4 > end) {
                    err = EFILE_UTF8;
                    goto FAILREAD;
                }
                unlikely_if_(((beg[1] >> 6) != 0x02) ||
                             ((beg[2] >> 6) != 0x02) ||
                             ((beg[3] >> 6) != 0x02)) {
                    err = EFILE_UTF8;
                    goto FAILREAD;
                }
                ch = ((beg[0] & 0x07) << 0x12) | ((beg[1] & 0x3f) << 0x0c) |
                     ((beg[2] & 0x3f) << 0x06) | ((beg[3] & 0x3f) << 0x00);
                beg += 4;
            }
            else {
                err = EFILE_UTF8;
                goto FAILREAD;
            }
            unlikely_if_(ch == L'\n') {
                insert_doc(&edp->doc, edp->doc->size, &linebuf, 1);
                linebuf.size = 0;
                linebuf.alloc = 0;
                linebuf.data = NULL;
                continue;
            }
            line_insert(&linebuf, linebuf.size, &ch, 1);
        }
    }
    if (linebuf.size != 0) {
        insert_doc(&edp->doc, edp->doc->size, &linebuf, 1);
    }
FAILREAD:
    fclose(file);
    return err;
}

static int
is_ascii(lchar_t *str, int size) {
    lchar_t *end = str + size;
    while (str != end) {
        unlikely_if_(*str >> 7) {
            return 0;
        }
        str++;
    }
    return 1;
}

static void
diff_insert_span(struct Diff *diff, lchar_t *str, int delta) {
    lchar_t *own = NULL;
    int size = diff->size;
    int to_add;
    lchar_t *beg;
    lchar_t *end;
    int i;

    if (delta < 0) {
        to_add = -delta;
        beg = str - 1;
        end = beg - to_add;

        if (diff->type == DIFF_TYPE_DELCHR) {
            xrealloc_ptr(&diff->data, size + to_add, sizeof(*diff->data), 0);
            while (beg != end) {
                diff->data[size] = *beg;
                size++;
                beg--;
            }
        } else if (!is_ascii(str - to_add, to_add) ||
                   size + to_add > SMALL_STR) {
            xrealloc_ptr(&own, size + to_add, sizeof(*diff->data), 0);
            for (i = 0; i < size; i++) {
                own[i] = diff->content[i];
            }
            diff->data = own;
            while (beg != end) {
                diff->data[size] = *beg;
                size++;
                beg--;
            }
            diff->type = DIFF_TYPE_DELCHR;
        } else {
            for (i = size; i < to_add + size; i++) {
                diff->content[i] = (unsigned char)*beg;
                beg--;
            }
        }
    } else {
        to_add = delta;
        beg = str;
        end = beg + to_add;

        if (diff->type == DIFF_TYPE_ADDCHR) {
            xrealloc_ptr(&diff->data, size + to_add, sizeof(*diff->data), 0);
            memcpy(diff->data + size, beg, (end - beg) * sizeof(*diff->data));
        } else if (!is_ascii(str, to_add) || size + to_add > SMALL_STR) {
            xrealloc_ptr(&own, size + to_add, sizeof(*diff->data), 0);
            for (i = 0; i < size; i++) {
                own[i] = diff->content[i];
            }
            diff->data = own;
            memcpy(diff->data + size, beg, (end - beg) * sizeof(*diff->data));
            diff->type = DIFF_TYPE_ADDCHR;
        } else {
            for (i = size; i < to_add + size; i++) {
                diff->content[i] = (unsigned char)*beg;
                beg++;
            }
        }
    }
    diff->size += to_add;
}

static void
save_file_utf8(struct LineArr *doc, struct FileInfo *info,
               struct DiffStk *diffstk) {
    const char *fname = info->fname;
    if (fname == NULL) {
        return;
    }

    unsigned char buf[4 * 4096 + 1];
    FILE *fout = fopen(fname, "w");
    struct Line *line = doc->data;
    struct Line *line_end = doc->data + doc->size;

    while (line != line_end) {
        lchar_t *ch = line->data;
        lchar_t *ch_end = line->data + line->size;
        int i = 0;

        while (ch != ch_end) {
            likely_if_(*ch < 0x80) {
                buf[i] = *ch;
                i += 1;
            }
            else if (*ch < 0x800) {
                buf[i + 0] = ((*ch >> 0x06) & 0x1f) | 0xc0;
                buf[i + 1] = ((*ch >> 0x00) & 0x3f) | 0x80;
                i += 2;
            }
            else if (*ch < 0x10000) {
                buf[i + 0] = ((*ch >> 0x0c) & 0x0f) | 0xe0;
                buf[i + 1] = ((*ch >> 0x06) & 0x3f) | 0x80;
                buf[i + 2] = ((*ch >> 0x00) & 0x3f) | 0x80;
                i += 3;
            }
            else if (*ch < 0x11000) {
                buf[i + 0] = ((*ch >> 0x12) & 0x07) | 0xf0;
                buf[i + 1] = ((*ch >> 0x0c) & 0x3f) | 0x80;
                buf[i + 2] = ((*ch >> 0x06) & 0x3f) | 0x80;
                buf[i + 3] = ((*ch >> 0x00) & 0x3f) | 0x80;
                i += 4;
            } else {
                assert(0 && "unreacheable");
            }
            ch++;
            unlikely_if_(i >= 4096) {
                fwrite(buf, sizeof(*buf), i, fout);
                i = 0;
            }
        }
        buf[i] = '\n';
        i += 1;
        fwrite(buf, sizeof(*buf), i, fout);
        line++;
    }
    fclose(fout);
    diffstk->curr_save_point = diffstk->curr;
}

static char *
mkstr_nmt(const char *fmt, ...) {
    static char strbuf[SSTR_SIZE];
    va_list vl;

    va_start(vl, fmt);
    vsnprintf(strbuf, SSTR_SIZE, fmt, vl);
    return strbuf;
}

static wchar_t *
wmkstr_nmt(const wchar_t *fmt, ...) {
    static wchar_t strbuf[SSTR_SIZE];
    va_list vl;

    va_start(vl, fmt);
    vswprintf(strbuf, SSTR_SIZE, fmt, vl);
    return strbuf;
}

static int
move_to_word(struct Editor *edp, const lchar_t *str, long size) {
    struct Line *line = edp->doc->data + edp->cursy;
    lchar_t *cur = line->data + edp->cursx + 1;
    lchar_t *end = line->data + line->size - size + 1;
    int i;

    if (line->size - size + 1 > 0) {
        for (; cur < end; cur++) {
            if (*cur != *str) {
                continue;
            }
            if (memcmp(cur, str, size * sizeof(*str)) == 0) {
                edp->cursx = cur - line->data;
                return 0;
            }
        }
    }
    for (i = 1; i < edp->doc->size + 1; i++) {
        line = edp->doc->data + (edp->cursy + i) % edp->doc->size;
        cur = line->data;
        end = line->data + line->size - size + 1;

        if (line->size - size + 1 <= 0) {
            continue;
        }
        for (; cur < end; cur++) {
            if (*cur != *str) {
                continue;
            }
            if (memcmp(cur, str, size * sizeof(*str)) == 0) {
                edp->cursy = line - edp->doc->data;
                edp->cursx = cur - line->data;
                return 0;
            }
        }
    }
    return -1;
}

static int
usr_quit(struct Editor *edp) {
    const char *msg;
    lint_t ch;

    if (edp->diffstk->curr != edp->diffstk->curr_save_point) {
        msg = mkstr_nmt("Save changes to %s? (ynC):", edp->fileinfo.fname);
        move_brush(edp, edp->win.y, 0);
        clrtoeol();
        addstr(msg);
        do {
            if (get_wch(&ch) == ERR) {
                return 1;
            }
        } while (ch != 'Y' && ch != 'N' && ch != 'C' && ch != 'y' &&
                 ch != 'n' && ch != 'c');
        switch (ch) {
        case 'Y':
        case 'y':
            save_file_utf8(edp->doc, &edp->fileinfo, edp->diffstk);
            return 0;
        case 'N':
        case 'n':
            return 0;
        default:
        case 'C':
        case 'c':
            return 1;
        }
    }
    return 0;
}

static void
show_keymap(void) {
    clear();
    mvaddstr(0, 0,
       "\nHELP (KEYMAP)"
       "\n"
       "\n    CTRL+CANC: Quit without saving"
       "\n    CTRL+{UP, DOWN, LEFT RIGHT}: Natural Movement"
       "\n    CTRL+{PGUP PGDOWN}: "
       "\n    CTRL+W: Delete one word"
       "\n    CTRL+R: Undo last action"
       "\n    CTRL+S: Save current document"
       "\n    CTRL+Q: Quit"
       "\n    CTRL+X: Save"
       "\n    CTRL+/: Find a word"
       "\n    CTRL+N: Find next"
       "\n    CTRL+H: Horizontal Selection Mode"
       "\n    CTRL+V: Vertical Selection Mode"
       "\n    CTRL+E: Command Mode"
       "\n"
       "\nHELP (END)"
       "\n"
       "\n"
    ); 
}

static int
replace_word_positrange(struct Editor *edp, struct Selection *selct,
                        lchar_t *str, int size, lchar_t *nstr, int nsize) {
    int found = 0;
    int ycurr = selct->ybeg;
    struct Editor sub = {
        .doc = edp->doc,
        .diffstk = NULL,
    };
    struct Editor *subp = &sub;
    lchar_t *ccurr; 
    int xcurr = 0;
    int xend;
    long pos;
    struct Diff *dcurr;

    unlikely_if_(edp->mode != MODE_SELECT_HORIZ) {
        return -1;
    }
    assert(selct->ybeg <= selct->yend);
    if (selct->ybeg == selct->yend) {
        return 0;
    }
    assert(edp->doc->size > selct->ybeg);
    assert(edp->doc->size >= selct->yend);

    xrealloc_arr(&subp->diffstk, INIT_SUBDIFF);
    memset(subp->diffstk, 0, sizeof(*subp->diffstk));
    subp->diffstk->alloc = INIT_SUBDIFF;

    unlikely_if_(size == 0) {
        return 0;
    }
    while (ycurr < selct->yend) {
        xcurr = 0;
        xend = subp->doc->data[ycurr].size;

        unlikely_if_(subp->doc->data[ycurr].size < size) {
            ycurr++;
            continue;
        }
        ccurr = &subp->doc->data[ycurr].data[xcurr];
        while (xcurr < xend - size + 1) {
            if (ccurr[xcurr] == *str) {
                if (memcmp(ccurr + xcurr, str, size * sizeof(*str)) == 0) {
                    found = 1;
                    subp->cursx = xcurr + size;
                    subp->cursy = ycurr;
                    line_remove_span_qdiff(subp, &subp->doc->data[ycurr],
                                           xcurr + size, -size);
                    subp->cursx -= size;

                    if (nsize != 0) {
                        diffstk_insert_span(subp, nstr, nsize);
                        line_insert(&subp->doc->data[ycurr], subp->cursx, nstr,
                                    nsize);
                        subp->cursx += nsize;
                    }
                    xend -= size + nsize;
                    continue;
                }
            }
            xcurr++;
        }
        ycurr++;
    }
    if (found == 0) {
        free(subp->diffstk);
    } else {
        xrealloc_arr(&subp->diffstk, subp->diffstk->size);
        subp->diffstk->alloc = subp->diffstk->size;

        diffstk_reserve(&edp->diffstk);
        pos   = edp->diffstk->curr;
        dcurr = edp->diffstk->data + pos;
        diffstk_incr(edp->diffstk);
        dcurr->type = DIFF_TYPE_SUBSTCK;
        dcurr->diff_sub = subp->diffstk;
    }
    edp->cursx = subp->cursx;
    edp->cursy = subp->cursy;
    return found;
}

static int
delete_lines(struct Editor *edp, struct Selection *selct) {
    int res;
    struct Selection f;
    struct Selection s;

    unlikely_if_(edp->mode != MODE_SELECT_HORIZ) {
        return -1;
    }
    if (selct->ybeg >= selct->yend) {
        f.ybeg = 0;
        f.yend = selct->yend;
        s.ybeg = selct->ybeg + 1;
        s.yend = edp->doc->size;

        res  = delete_lines_positrange(edp, &s);
        res |= delete_lines_positrange(edp, &f);
        return res;
    }
    res = delete_lines_positrange(edp, selct);
    return res;
}

static int
edit_vert(struct Editor *edp) {
    struct DiffStk *dsave = edp->diffstk;
    struct Diff *dcurr;
    int pos;
    int cont;

    unlikely_if_(edp->mode != MODE_SELECT_VERT) {
        return -1;
    }
    edp->diffstk = NULL;
    xrealloc_arr(&edp->diffstk, INIT_SUBDIFF);
    memset(edp->diffstk, 0, sizeof(*edp->diffstk));
    edp->diffstk->alloc = INIT_SUBDIFF;

    do {
        cont = render_loop(edp);
    } while (cont);

    if (edp->diffstk->size == 0) {
        free(edp->diffstk);
    } else {
        xrealloc_arr(&edp->diffstk, edp->diffstk->size);
        edp->diffstk->alloc = edp->diffstk->size;

        diffstk_reserve(&dsave);
        pos   = dsave->curr;
        dcurr = dsave->data + pos;
        diffstk_incr(dsave);
        dcurr->type = DIFF_TYPE_SUBSTCK;
        dcurr->diff_sub = edp->diffstk;
    }
    edp->diffstk = dsave;
    return 0;
}

static int
delete_lines_positrange(struct Editor *edp, struct Selection *selct) {
    int ycurr = selct->ybeg;
    int size;
    int pos;
    struct Editor sub = {
        .doc = edp->doc,
        .diffstk = NULL,
    };
    struct Editor *subp = &sub;
    struct Diff *dcurr;
    int i;

    unlikely_if_(edp->mode != MODE_SELECT_HORIZ) {
        return -1;
    }
    assert(selct->ybeg <= selct->yend);
    if (selct->ybeg == selct->yend) {
        return 0;
    }
    assert(edp->doc->size > selct->ybeg);
    assert(edp->doc->size >= selct->yend);

    xrealloc_arr(&subp->diffstk, INIT_SUBDIFF);
    memset(subp->diffstk, 0, sizeof(*subp->diffstk));
    subp->diffstk->alloc = INIT_SUBDIFF;

    while (ycurr < selct->yend) {
        size = subp->doc->data[ycurr].size;
        subp->cursx = size;
        subp->cursy = ycurr;
        if (size) {
            line_remove_span_qdiff(subp, &subp->doc->data[ycurr], size, -size);
            subp->cursx -= size;
        }
        ycurr++;
    }
    for (i = 1; i < edp->selct.yend - edp->selct.ybeg; ++i) {
        del_back(subp);
    }
    xrealloc_arr(&subp->diffstk, subp->diffstk->size);
    subp->diffstk->alloc = subp->diffstk->size;

    diffstk_reserve(&edp->diffstk);
    pos   = edp->diffstk->curr;
    dcurr = edp->diffstk->data + pos;
    diffstk_incr(edp->diffstk);
    dcurr->type = DIFF_TYPE_SUBSTCK;
    dcurr->diff_sub = subp->diffstk;

    edp->cursx = subp->cursx;
    edp->cursy = subp->cursy;
    return 0;
}

static int
replace_word(struct Editor *edp, struct Selection *selct, lchar_t *str,
             int size, lchar_t *nstr, int nsize) {
    int res;
    struct Selection f;
    struct Selection s;

    unlikely_if_(edp->mode != MODE_SELECT_HORIZ) {
        return -1;
    }
    if (selct->ybeg >= selct->yend) {
        f.ybeg = 0;
        f.yend = selct->yend;
        s.ybeg = selct->ybeg + 1;
        s.yend = edp->doc->size;

        res = replace_word_positrange(edp, &s, str, size, nstr, nsize);
        return res | replace_word_positrange(edp, &f, str, size, nstr, nsize);
    }
    return replace_word_positrange(edp, selct, str, size, nstr, nsize);
}

static void
move_brush(struct Editor *edp, int y, int x) {
    move(edp->win.offy + y, x);
}

static void
clear_window(struct Editor *edp) {
    int i;

    for (i = 0; i < edp->win.y; i++) {
        move_brush(edp, i, 0);
        clrtoeol();
    }
}

static int
num_digits(int i, int base) {
    int res = 0;

    do {
        i /= base;
        res++;
    } while (i);

    return res;
}

static void
init_split_iter(struct SplitIter *it, lchar_t *str, int size, int beg,
                int end) {
    it->line.beg = str;
    it->line.end = str + size;
    it->selct.beg = str + beg;
    it->selct.end = str + end;
}

static int
calc_padlx(struct Editor *edp) {
    if (edp->mode == MODE_NORMAL && edp->opt & OPT_SHOW_LINENO) {
        return num_digits(edp->doc->size + 1, 10) + 1;
    } else if (edp->mode == MODE_SEARCH) {
        return strsize(MSG_SEARCH);
    } else if (edp->mode == MODE_COMMAND) {
        return strsize(MSG_COMMAND);
    } else if (edp->mode == MODE_FILES) {
        return strsize(MSG_FILES);
    }
    return 0;
}

static void
regularize_selection(struct Selection *in, struct Selection *out) {
    out->ybeg = in->ybeg;
    out->yend = in->yend;
    out->xbeg = in->xbeg;
    out->xend = in->xend;

    if (in->ybeg >= in->yend) {
        out->ybeg = out->yend - 1;
        out->yend = in->ybeg;
    }
    if (in->xbeg > in->xend) {
        out->xbeg = out->xend;
        out->xend = in->xbeg;
    }
}

static void
paint_string(struct Editor *edp, lchar_t *str, int size, int y, int x) {
    struct Range r;
    struct SplitIter it;
    struct Selection selct;
    int i;

    if (edp->mode == MODE_NORMAL && edp->opt & OPT_SHOW_LINENO) {
        printw("%*d ", edp->win.offx - 1, y + 1);
    } else if (edp->mode == MODE_SEARCH) {
        addstr(MSG_SEARCH);
    } else if (edp->mode == MODE_COMMAND) {
        addstr(MSG_COMMAND);
    } else if (edp->mode == MODE_FILES) {
        addstr(MSG_FILES);
    }
    likely_if_(edp->mode < MODE_SELECT_HORIZ) {
        addnwstr(str, size);
    } else if (edp->mode == MODE_SELECT_HORIZ) {
        if (edp->selct.ybeg < edp->selct.yend) {
            if (y >= edp->selct.ybeg && y < edp->selct.yend) {
                attron(A_REVERSE);
                addnwstr(str, size);
                attroff(A_REVERSE);
            } else {
                addnwstr(str, size);
            }
        } else {
            if (y <= edp->selct.ybeg && y >= edp->selct.yend) {
                addnwstr(str, size);
            } else {
                attron(A_REVERSE);
                addnwstr(str, size);
                attroff(A_REVERSE);
            }
        }
    } else if (edp->mode == MODE_SELECT_VERT) {
        regularize_selection(&edp->selct, &selct);
        if (y >= selct.ybeg && y < selct.yend) {
            init_split_iter(&it, str, size, selct.xbeg + x, selct.xend + x);
            for (i = 0; i < 2; i++) {
                if (iter_split(&it, &r)) {
                    attron(A_REVERSE);
                    addnwstr(r.beg, r.end - r.beg);
                    attroff(A_REVERSE);
                    iter_split(&it, &r);
                    if (r.end == r.beg) {
                        break;
                    } else {
                        attron(COLOR_PAIR(1));
                        addnwstr(r.beg, 1);
                        attroff(COLOR_PAIR(1));
                        addnwstr(r.beg + 1, r.end - r.beg - 1);
                        return;
                    }
                } else {
                    addnwstr(r.beg, r.end - r.beg);
                }
            }
            attron(COLOR_PAIR(1));
            addch(' ');
            attroff(COLOR_PAIR(1));
        } else {
            addnwstr(str, size);
        }
    }
}

static int
exec_command(struct Editor *edp, lchar_t *str, int size) {
    (void)edp;

    assert(str);
    assert(size > 0);

    return -1;
}

static int
delete_vert(struct Editor *edp, struct Selection *s) {
    struct Selection selct;
    struct Line *line;
    int delta;
    int i;

    regularize_selection(s, &selct);
    unlikely_if_(selct.ybeg >= selct.yend || selct.xbeg > selct.xend) {
        return -1;
    }
    if (selct.xend - selct.xbeg == 0) {
        for (i = selct.yend - 1; i != selct.ybeg - 1; i--) {
            edp->cursy = i;
            edp->cursx = min(selct.xend, edp->doc->data[edp->cursy].size);
            if (i == selct.ybeg && selct.xbeg == 0) {
                return 0;
            }
            del_back(edp);
        }
        edp->cursy = s->yend - 1;
        memcpy(&edp->selct, &selct, sizeof(selct));
        edp->selct.xend--;
        edp->selct.xbeg--;
        edp->cursx = min(edp->selct.xend, edp->doc->data[edp->cursy].size);
    } else {
        for (i = selct.yend - 1; i != selct.ybeg - 1; i--) {
            edp->cursy = i;
            edp->cursx = min(selct.xend, edp->doc->data[edp->cursy].size);
            if (selct.xbeg >= edp->doc->data[edp->cursy].size) {
                continue;
            }
            delta = edp->cursx - selct.xbeg;
            assert(delta >= 0);
            if (delta == 0) {
                continue;
            }
            line = &edp->doc->data[edp->cursy];
            line_remove_span_qdiff(edp, line, edp->cursx, -delta);
            edp->cursy = s->yend - 1;
            memcpy(&edp->selct, &selct, sizeof(selct));
            edp->selct.xend = edp->selct.xbeg;
            edp->cursx = min(edp->selct.xend, edp->doc->data[edp->cursy].size);
        }
    }
    return 0;
}

static int
iter_split(struct SplitIter *it, struct Range *delta) {
    if (it->line.end <= it->selct.beg) {
        delta->beg = it->line.beg;
        delta->end = it->line.end;
        it->line.beg = it->line.end;
        return 0;
    }
    if (it->line.beg < it->selct.beg) {
        delta->beg = it->line.beg;
        delta->end = it->selct.beg;
        it->line.beg = it->selct.beg;
        return 0;
    }
    if (it->line.beg == it->selct.beg) {
        delta->beg = it->line.beg;
        delta->end = min(it->selct.end, it->line.end);
        it->line.beg = delta->end;
        it->selct.beg = it->line.end;
        return 1;
    }
    delta->beg = it->line.beg;
    delta->end = it->line.end;
    return 0;
}

static int
insert_vert(struct Editor *edp, lchar_t ch) {
    struct Line *line;
    int i;

    if (edp->selct.xend - edp->selct.xbeg) {
        delete_vert(edp, &edp->selct);
    }
    for (i = edp->selct.ybeg; i < edp->selct.yend; i++) {
        edp->cursy = i;
        line = edp->doc->data + edp->cursy;
        edp->cursx = min(edp->selct.xend, line->size);
        diffstk_insert_span(edp, &ch, 1);
        line_insert(line, edp->cursx, &ch, 1);
    }
    edp->cursx++;
    edp->selct.xbeg = edp->cursx;
    edp->selct.xend = edp->cursx;
    return 0;
}

static lchar_t *
u32str_convert(const char *fname) {
    static lchar_t res[4096 * 3 + 1];
    lchar_t *beg = res;
    size_t usize;
    int size;
    int i;

    assert(fname);
    usize = strlen(fname);
    assert(usize && usize < 4096);
    size = usize;

    for (i = 0; i < size; i++) {
        likely_if_((fname[i] >= 0x20 && fname[i] < 0x7f) || fname[i] == '\t') {
            *beg = fname[i];
            beg++;
        } else {
            beg[0] = L'\\';
            beg[1] = denormalize_xcale((unsigned char)fname[i] >> 4);
            beg[2] = denormalize_xcale((unsigned char)fname[i] & 0xf);
            beg += 3;
        }
    }
    *beg = L'\0';
    return res;
}

static lchar_t
denormalize_xcale(unsigned char in) {
    if (in < 0xa) {
        return in + L'0';
    } else {
        return in - 0xa + L'A';
    }
}

static int
normalize_xcape(lchar_t in) {
    if (in >= L'0' && in <= L'9') {
        return in - L'0';
    } else if (in >= L'a' && in <= L'f') {
        return in - L'a' + 10;
    } else if (in >= L'A' && in <= L'F') {
        return in - L'A' + 10;
    }
    return -1;
}

static char *
u8str_convert(lchar_t *fname, int size) {
    static char res[4096 + 1];
    char *beg = res;
    char *end = res + 4096;
    lchar_t *fbeg = fname;
    lchar_t *fend = fname + size;
    int norm0;
    int norm1;

    assert(fname);
    assert(size);

    while (fbeg != fend) {
        if (beg == end) {
            return NULL;
        }
        if (*fbeg == L'\\') {
            fbeg++;
            if (fbeg == fend) {
                *beg = '\\';
                beg++;
                break;
            }
            norm0 = normalize_xcape(*fbeg);
            if (norm0 == -1) {
                *beg = (char)*fbeg;
                beg++;
            }
            fbeg++;
            if (fbeg == fend) {
                *beg = norm0;
                beg++;
                break;
            }
            norm1 = normalize_xcape(*fbeg);
            if (norm1 == -1) {
                *beg = norm0;
                beg++;
                continue;
            }
            *beg = (norm0 << 4) | norm1;
            beg++;
            fbeg++;
        } else {
            *beg = *fbeg;
            beg++;
            fbeg++;
        }
    }
    assert(beg != res + 4096 + 1);
    *beg = '\0';
    return res;
}
