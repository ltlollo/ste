// gcc self -g -O0 $(pkg-config --libs ncursesw) -o ste -pedantic -Wall -Wextra

#define _XOPEN_SOURCE 700
#include <assert.h>
#include <ctype.h>
#include <err.h>
#include <errno.h>
#include <limits.h>
#include <locale.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>
#include <wctype.h>
#define _XOPEN_SOURCE_EXTENDED
#include <ncursesw/ncurses.h>

#define TAB_SIZE 8
#define SMALL_STR 8
#define INIT_DIFF 4096
#define INIT_DOC 1024
#define INIT_LINE 16

#define MAX_DIFF_SIZE 0x1fffffff

#define KEY_SHIFT_DOWN 336
#define KEY_SHIFT_UP 337
#define KEY_SHIFT_RIGHT 402
#define KEY_SHIFT_LEFT 393
#define KEY_PGDOWN 338
#define KEY_PGUP 339
#define KEY_CTRL_CANC 519
#define KEY_CTRL_LEFT 545
#define KEY_CTRL_RIGHT 560
#define KEY_CTRL_DOWN 525
#define KEY_CTRL_UP 566
#define KEY_CTRL_PGDOWN 550
#define KEY_CTRL_PGUP 555

#define xrealloc_arr(p, size)                                                 \
    xrealloc_ptr(p, size, sizeof(*(*p)->data), sizeof(**p))

#define xrealloc_owndata(p, size)                                             \
    xrealloc_ptr(&p->data, size, sizeof(*p->data), 0)

#define arrsize(cx) ((long long)(sizeof(cx) / sizeof(*cx)))

#define expect(x, v) __builtin_expect(x, v)
#define unlikely(x) expect(!!(x), 0)
#define likely(x) expect(!!(x), 1)
#define unlikely_if_(x) if (unlikely(x))
#define likely_if_(x) if (likely(x))

typedef wint_t lint_t;
typedef wchar_t lchar_t;
typedef int (*filt_fn_t)(lint_t);

typedef enum DIFF_TYPE {
    DIFF_TYPE_ADDBRK,
    DIFF_TYPE_DELBRK,
    DIFF_TYPE_ADDCHR_SMALL,
    DIFF_TYPE_DELCHR_SMALL,
    DIFF_TYPE_INLINE = DIFF_TYPE_DELCHR_SMALL,
    DIFF_TYPE_ADDCHR,
    DIFF_TYPE_DELCHR,
    // DIFF_TYPE_SUB,
} DIFF_TYPE;

typedef struct __attribute__((packed)) Diff {
    struct __attribute__((packed)) {
        enum DIFF_TYPE type : 3;
        unsigned size : 29;
        int y;
        int x;
    };
    union __attribute__((packed)) {
        unsigned char content[SMALL_STR];
        lchar_t *data;
        filt_fn_t fil;
    };
} Diff;
_Static_assert(sizeof(Diff) == 12 + SMALL_STR, "unsupported architecture");

typedef struct Line {
    long long size;
    long long alloc;
    lchar_t *data;
} Line;

typedef struct DiffStk {
    long long size;
    long long alloc;
    long long curr;
    Diff _Alignas(32) data[];
} DiffStk;

typedef struct LineArr {
    long long size;
    long long alloc;
    Line _Alignas(32) data[];
} LineArr;

typedef enum EFILE {
    EFILE_OK = 0,
    EFILE_OPEN,
    EFILE_UTF8,
    EFILE_NOFILE
} EFILE;

typedef enum DIREC {
    DIREC_FORW,
    DIREC_BACK,
} DIREC;

typedef struct Editor {
    struct LineArr *doc;
    struct DiffStk *diffstk;
    int cursy;
    int cursx;
    int framebeg;
    int winy;
    int winx;
    int pgspan;
} Editor;

typedef struct FileInfo { char *fname; } FileInfo;

static int
always(lint_t p) {
    (void)p;
    return 1;
}

static filt_fn_t filts[] = {
    iswalnum, iswpunct, iswspace, iswprint, always,
};
static struct FileInfo fileinfo = { NULL };
static struct LineArr *doc = NULL;
static struct DiffStk *diffstk = NULL;
static int cursx = 0;
static int cursy = 0;
static int framebeg = 0;
static int winx;
static int winy;
static int pgspan;

static int always(lint_t);

static void diffstk_incr(struct DiffStk *);
static void diffstk_reserve(struct DiffStk *);
static void diffstk_insert_addbk(struct DiffStk *, int, int);
static void diffstk_insert_delbk(struct DiffStk *, int, int);
static int eq_bigch(struct Diff *, enum DIFF_TYPE);
static void diffstk_insert_span(struct DiffStk *, lchar_t *, int);
static void insert_doc(int, struct Line *, int);
static void insert_doc_nl_times(int, int);
static void line_insert(struct Line *, int, lchar_t *, int);
static void merge_lines(int);
static void line_remove_span(struct Line *, int, int);
static filt_fn_t find_match_fil(lint_t);
static int find_prev_simil(const void *, int);
static int find_next_simil(const void *, int, int);
static int realloc_ptr(void *, size_t, size_t, size_t);
static void xrealloc_ptr(void *, size_t, size_t, size_t);
static void close_win(void);
static void open_win(void);
static void init_editor(const char *);
static int move_right(void);
static int move_left(void);
static int del_back(void);
static int move_up(void);
static int move_pgup(void);
static int move_pgdown(void);
static int move_down(void);
static int move_down_natural(void);
static int move_up_natural(void);
void diff_apply_brk(struct Diff *, enum DIREC);
void diff_apply_chr(struct Diff *, enum DIREC);
int diffstk_apply_last(struct DiffStk *, enum DIREC);
static int handle_input(lint_t);
static int render_loop(void);

static int count_render_width_upto(struct Line *, int);
static lchar_t *render_max_given_width(lchar_t *, lchar_t *, int);
static lchar_t *render_back_max_given_width(lchar_t *, lchar_t *, int);
static int count_nlines_upto(struct Line *, int);
static int count_nlines(struct Line *);
static int count_lines(void);
static void reposition_frame(void);
static void render_lines(void);
static void reposition_cursor(void);
static enum EFILE load_file_utf8(const char *);
int is_ascii(lchar_t *, int);
static void diff_insert_span(struct Diff *, lchar_t *, int);

static void
diffstk_incr(struct DiffStk *diffstk) {
    if (diffstk->curr < diffstk->size) {
        for (int i = diffstk->curr; i < diffstk->size; ++i) {
            if (diffstk->data[i].type > DIFF_TYPE_INLINE) {
                free(diffstk->data[i].data);
                diffstk->data[i].type = 0;
            }
        }
    }
    diffstk->curr++;
    diffstk->size = diffstk->curr;
}

static void
diffstk_reserve(struct DiffStk *diffstk) {
    struct Diff *beg;
    struct Diff *end;

    if (diffstk->size == diffstk->alloc) {
        xrealloc_arr(&diffstk, diffstk->alloc * 2 + 1);
        diffstk->alloc = diffstk->alloc * 2 + 1;
    }
    beg = diffstk->data + diffstk->size;
    end = diffstk->data + diffstk->alloc;
    memset(beg, 0, (end - beg) * sizeof(*diffstk->data));
}

static void
diffstk_insert_addbk(struct DiffStk *diffstk, int y, int x) {
    long long pos;
    struct Diff *curr;
    struct Diff *prev;

    diffstk_reserve(diffstk);

    pos = diffstk->curr;
    curr = diffstk->data + pos;
    prev = diffstk->data + pos - 1;

    if (pos == 0 || prev->type != DIFF_TYPE_ADDBRK ||
        prev->size == MAX_DIFF_SIZE) {
        diffstk_incr(diffstk);
        curr->type = DIFF_TYPE_ADDBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    } else if (prev->y == cursy - prev->size && cursx == 0) {
        prev->size++;
    } else {
        diffstk_incr(diffstk);
        curr->type = DIFF_TYPE_ADDBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    }
}

static void
diffstk_insert_delbk(struct DiffStk *diffstk, int y, int x) {
    long long pos;
    struct Diff *curr;
    struct Diff *prev;

    diffstk_reserve(diffstk);

    pos = diffstk->curr;
    curr = diffstk->data + pos;
    prev = diffstk->data + pos - 1;

    if (pos == 0 || prev->type != DIFF_TYPE_DELBRK ||
        prev->size == MAX_DIFF_SIZE) {
        diffstk_incr(diffstk);
        curr->type = DIFF_TYPE_DELBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
    } else if (prev->y == cursy + prev->size && prev->x == 0) {
        prev->x = cursx;
        prev->y--;
        prev->size++;
    } else {
        diffstk_incr(diffstk);
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
diffstk_insert_span(struct DiffStk *diffstk, lchar_t *str, int delta) {
    long long pos;
    struct Diff *curr;
    struct Diff *prev;
    enum DIFF_TYPE type;
    lchar_t sample;
    filt_fn_t curr_fil;
    filt_fn_t prev_fil;

    diffstk_reserve(diffstk);

    pos = diffstk->curr;
    curr = diffstk->data + pos;
    prev = diffstk->data + pos - 1;

    if (delta < 0) {
        type = DIFF_TYPE_DELCHR_SMALL;
        sample = str[-1];

    } else {
        type = DIFF_TYPE_ADDCHR_SMALL;
        sample = str[0];
    }
    curr_fil = find_match_fil(sample);

    if (pos == 0 || !eq_bigch(prev, type) || prev->size == MAX_DIFF_SIZE) {
        diffstk_incr(diffstk);
        curr->type = type;
        curr->size = 0;
        curr->x = cursx;
        curr->y = cursy;
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
    if (prev->y == cursy && prev->x + prev->size == cursx &&
        curr_fil == prev_fil) {
        diff_insert_span(prev, str, delta);
    } else {
        diffstk_incr(diffstk);
        curr->type = type;
        curr->size = 0;
        curr->x = cursx;
        curr->y = cursy;
        diff_insert_span(curr, str, delta);
    }
}

static void
insert_doc(int pos, struct Line *cpy, int size) {
    struct Line *beg;
    struct Line *end;

    assert(doc->size <= doc->alloc);
    assert(pos <= doc->size);

    if (doc->size + size > doc->alloc) {
        xrealloc_arr(&doc, doc->alloc * 2 + size);
        doc->alloc = doc->alloc * 2 + size;
    }
    beg = doc->data + pos;
    end = doc->data + doc->size;

    memmove(beg + size, beg, (end - beg) * sizeof(*doc->data));
    memcpy(beg, cpy, size * sizeof(*doc->data));
    doc->size += size;
}

static void
insert_doc_nl_times(int pos, int size) {
    struct Line *beg;
    struct Line *end;

    assert(doc->size <= doc->alloc);
    assert(pos <= doc->size);

    if (doc->size + size > doc->alloc) {
        xrealloc_arr(&doc, doc->alloc * 2 + size);
        doc->alloc = doc->alloc * 2 + size;
    }
    beg = doc->data + pos;
    end = doc->data + doc->size;

    memmove(beg + size, beg, (end - beg) * sizeof(*doc->data));
    memset(beg, 0, size * sizeof(*doc->data));
    doc->size += size;
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
merge_lines(int pos) {
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
    --doc->size;
}

static void
line_remove_span(struct Line *line, int pos, int delta) {
    lchar_t *beg = line->data + pos;
    lchar_t *prev = line->data + pos + delta;
    lchar_t *end = line->data + line->size;

    assert(beg <= end);

    diffstk_insert_span(diffstk, beg, delta);
    memmove(prev, beg, (end - beg) * sizeof(*line->data));

    line->size += delta;
}

static filt_fn_t
find_match_fil(lint_t ch) {
    filt_fn_t fil = NULL;

    for (int i = 0; i < arrsize(filts); ++i) {
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
        --cursor;
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
open_win(void) {
    initscr();
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    intrflush(stdscr, FALSE);
    set_tabsize(TAB_SIZE);
    winx = COLS;
    winy = LINES;
    pgspan = winy / 4 * 3;
}

static void
init_editor(const char *fname) {
    enum EFILE efile;

    setlocale(LC_ALL, "");

    xrealloc_arr(&doc, INIT_DOC);
    xrealloc_arr(&diffstk, INIT_DIFF);

    memset(doc, 0, sizeof(struct LineArr) + sizeof(*doc->data) * INIT_DOC);
    doc->alloc = INIT_DOC;
    doc->size = 0;

    efile = load_file_utf8(fname);
    if (efile != EFILE_OK) {
        if (efile == EFILE_UTF8) {
            errx(1, "invalid utf8 content in '%s'", fname);
        } else if (efile == EFILE_OPEN) {
            err(1, "fopen '%s'", fname);
        } else {
            doc->size = 1;
        }
    }
    fileinfo.fname = strdup(fname);

    memset(diffstk, 0, sizeof(DiffStk) + sizeof(*diffstk->data) * INIT_DIFF);
    diffstk->alloc = INIT_DIFF;
    diffstk->size = 0;

    open_win();
    atexit(close_win);
}

static int
move_right(void) {
    struct Line *line = &doc->data[cursy];

    if (cursx < line->size) {
        ++cursx;
    } else if (cursy < doc->size - 1) {
        cursy++;
        cursx = 0;
    } else {
        return -1;
    }
    return 0;
}

static int
move_left(void) {
    if (cursy == 0 && cursx == 0) {
        return -1;
    } else if (cursx != 0) {
        --cursx;
    } else {
        cursy--;
        cursx = doc->data[cursy].size;
    }
    return 0;
}

static int
del_back(void) {
    struct Line *line;

    if (cursy == 0 && cursx == 0) {
        return -1;
    }
    if (cursx == 0) {
        --cursy;
        line = &doc->data[cursy];
        cursx = line->size;
        merge_lines(cursy);

        diffstk_insert_delbk(diffstk, cursy, cursx);
    } else {
        line = &doc->data[cursy];
        line_remove_span(line, cursx, -1);
        --cursx;
    }
    return 0;
}

static int
move_up(void) {
    struct Line *line = &doc->data[cursy];

    if (cursy == 0) {
        if (cursx != 0) {
            cursx = 0;
            return 0;
        } else {
            return -1;
        }
    }
    --cursy;
    --line;
    if (cursx > line->size) {
        cursx = line->size;
    }
    return 0;
}

static int
move_pgup(void) {
    static Line *line;

    if (cursy - pgspan < 0) {
        cursy = 0;
    } else {
        cursy -= pgspan;
    }
    line = &doc->data[cursy];

    if (cursx <= line->size) {
        return 0;
    } else {
        cursx = line->size;
    }
    return 0;
}

static int
move_pgdown(void) {
    static Line *line;

    if (cursy + pgspan >= doc->size) {
        cursy = doc->size - 1;
    } else {
        cursy += pgspan;
    }
    line = &doc->data[cursy];

    if (cursx <= line->size) {
        return 0;
    } else {
        cursx = line->size;
    }
    return 0;
}

static int
move_down(void) {
    struct Line *line = &doc->data[cursy];

    if (cursy == doc->size - 1) {
        if (cursx != line->size) {
            cursx = line->size;
            return 0;
        } else {
            return -1;
        }
    }
    ++cursy;
    ++line;
    if (cursx > line->size) {
        cursx = line->size;
    }
    return 0;
}

static int
move_down_natural(void) {
    struct Line *line = &doc->data[cursy];
    lchar_t *beg = &line->data[cursx];
    lchar_t *end = render_max_given_width(beg, line->data + line->size, winx);

    if (end != line->data + line->size) {
        cursx += end - beg;
        return 0;
    } else {
        move_down();
    }
    return 0;
}

static int
move_up_natural(void) {
    struct Line *line = &doc->data[cursy];
    lchar_t *end = &line->data[cursx];
    lchar_t *beg = render_back_max_given_width(end, line->data - 1, winx);

    if (beg < line->data || line->size == 0) {
        move_up();
        return 0;
    } else {
        cursx -= end - beg;
    }
    return 0;
}

void
diff_apply_brk(struct Diff *diff, enum DIREC direc) {
    struct Line *line;
    struct Line own = { 0, 0, 0 };

    cursx = diff->x;
    cursy = diff->y;
    line = &doc->data[cursy];
    if (((diff->type == DIFF_TYPE_ADDBRK) ^ (direc == DIREC_FORW)) == 0) {
        assert(cursy >= 0);
        for (int i = 0; i < diff->size; ++i) {
            merge_lines(cursy);
        }
    } else {
        assert(diff->size);
        line_insert(&own, 0, line->data + cursx, line->size - cursx);
        line->size = cursx;
        insert_doc_nl_times(cursy + 1, diff->size - 1);
        insert_doc(cursy + diff->size, &own, 1);
        cursx = 0;
        cursy += diff->size;
    }
}

void
diff_apply_chr(struct Diff *diff, enum DIREC direc) {
    struct Line *line;
    lchar_t *beg;
    lchar_t *to;
    lchar_t *end;

    cursx = diff->x;
    cursy = diff->y;
    line = &doc->data[cursy];

    if ((eq_bigch(diff, DIFF_TYPE_ADDCHR_SMALL) ^ (direc == DIREC_FORW)) ==
        0) {
        if (direc == DIREC_FORW) {
            to = line->data + cursx;
            beg = line->data + cursx + diff->size;
        } else {
            to = line->data + cursx - diff->size;
            beg = line->data + cursx;
            cursx -= diff->size;
        }
        end = line->data + line->size;
        memmove(to, beg, (end - beg) * sizeof(*line->data));
        line->size -= diff->size;
    } else {
        if (line->size + diff->size > line->alloc) {
            xrealloc_owndata(line, line->alloc * 2 + diff->size);
            line->alloc = line->alloc * 2 + diff->size;
        }
        beg = line->data + cursx - diff->size;
        end = line->data + line->size;

        memmove(beg + diff->size, beg, (end - beg) * sizeof(*line->data));
        line->size += diff->size;
        if (direc == DIREC_FORW) {
            if (diff->type <= DIFF_TYPE_INLINE) {
                for (int i = diff->size; i; --i) {
                    beg[diff->size - i] = diff->content[i - 1];
                }
            } else {
                for (int i = diff->size; i; --i) {
                    beg[diff->size - i] = diff->data[i - 1];
                }
            }
        } else {
            if (diff->type <= DIFF_TYPE_INLINE) {
                for (int i = 0; i < diff->size; ++i) {
                    beg[diff->size + i] = diff->content[i];
                }
            } else {
                for (int i = 0; i < diff->size; ++i) {
                    beg[diff->size + i] = diff->data[i];
                }
            }
            cursx += diff->size;
        }
    }
}

int
diffstk_apply_last(struct DiffStk *diffstk, enum DIREC direc) {
    struct Diff *diff = diffstk->data + diffstk->curr - 1;
    int err = 0;
    int delta;

    if (direc == DIREC_FORW) {
        delta = -1;
        diff = diffstk->data + diffstk->curr - 1;
    } else {
        delta = 1;
        diff = diffstk->data + diffstk->curr;
    }

    if (direc == DIREC_FORW && diffstk->curr == 0) {
        return -1;
    } else if (direc == DIREC_BACK && diffstk->curr == diffstk->size) {
        return -1;
    }
    switch (diff->type) {
    default:
        err = -1;
        break;
    case DIFF_TYPE_ADDBRK:
    case DIFF_TYPE_DELBRK:
        diff_apply_brk(diff, direc);
        break;
    case DIFF_TYPE_ADDCHR:
    case DIFF_TYPE_ADDCHR_SMALL:
    case DIFF_TYPE_DELCHR:
    case DIFF_TYPE_DELCHR_SMALL:
        diff_apply_chr(diff, direc);
        break;
    }
    diffstk->curr += delta;
    return err;
}

static int
handle_input(lint_t c) {
    struct Line own = { 0, 0, 0 };
    struct Line *line = &doc->data[cursy];
    int rest;
    lchar_t ch = c;

    assert(cursy < doc->size);
    assert(cursx <= line->size);

    switch (ch) {
    case KEY_CTRL_CANC:
        return 0;
    case L'\n':
        diffstk_insert_addbk(diffstk, cursy, cursx);
        line_insert(&own, 0, line->data + cursx, line->size - cursx);
        insert_doc(cursy + 1, &own, 1);
        line->size = cursx;
        cursx = 0;
        cursy++;
        break;
    case KEY_RIGHT:
        move_right();
        break;
    case KEY_LEFT:
        move_left();
        break;
    case KEY_UP:
        move_up();
        break;
    case KEY_DOWN:
        move_down();
        break;

    case KEY_CTRL_UP:
    case KEY_SHIFT_UP:
        move_up_natural();
        break;

    case KEY_CTRL_DOWN:
    case KEY_SHIFT_DOWN:
        move_down_natural();
        break;

    case KEY_CTRL_LEFT:
    case KEY_SHIFT_LEFT:
        if (cursy == 0 && cursx == 0) {
            break;
        } else if (cursx == 0) {
            cursx = (line - 1)->size;
            cursy--;
        } else {
            cursx = find_prev_simil(line->data, cursx);
        }
        break;

    case KEY_CTRL_RIGHT:
    case KEY_SHIFT_RIGHT:
        if (cursx < line->size) {
            cursx = find_next_simil(line->data, cursx, line->size);
        } else if (cursy < doc->size - 1) {
            cursx = 0;
            cursy++;
        }
        break;

    case KEY_PGUP:
        move_pgup();
        break;
    case KEY_PGDOWN:
        move_pgdown();
        break;
    case KEY_CTRL_PGUP:
    case KEY_CTRL_PGDOWN:
        break;
    case 23:
        if (strcmp(keyname(ch), "^W") == 0) {
            if (cursx == 0) {
                del_back();
                break;
            }
            line = &doc->data[cursy];
            rest = find_prev_simil(line->data, cursx);
            line_remove_span(line, cursx, rest - cursx);
            cursx = rest;
            break;
        }
        break;
    case 21:
        if (strcmp(keyname(ch), "^U") == 0) {
            diffstk_apply_last(diffstk, DIREC_FORW);
        }
        break;
    case 18:
        if (strcmp(keyname(ch), "^R") == 0) {
            diffstk_apply_last(diffstk, DIREC_BACK);
        }
        break;
    case KEY_BACKSPACE:
        del_back();
        break;
    case KEY_DC:
        if (move_right() == -1) {
            break;
        }
        del_back();
        break;
    case KEY_RESIZE:
        clear();
        winx = COLS;
        winy = LINES;
        pgspan = winy / 4 * 3;
        reposition_frame();
        render_lines();
        reposition_cursor();
        refresh();
        break;
    default:
        if (iswprint(ch) || ch == L'\t') {
            diffstk_insert_span(diffstk, &ch, 1);
            line_insert(line, cursx, &ch, 1);
            cursx++;
        }
        break;
    }
    return 1;
}

static int
render_loop(void) {
    lint_t ch;

    reposition_frame();
    clear();
    render_lines();
    reposition_cursor();

    if (get_wch(&ch) == ERR) {
        return 1;
    }
    return handle_input(ch);
}

int
main(int argc, char *argv[]) {
    int cont;

    if (argc - 1 != 1) {
        errx(1, "need help?");
    }
    init_editor(argv[1]);

    do {
        cont = render_loop();
    } while (cont);

    return 0;
}

static int
count_render_width_upto(struct Line *line, int size) {
    int nlines = 0;
    int currline = 0;
    int char_w;

    for (int i = 0; i < size && i < line->size; ++i) {
        char_w = wcwidth(line->data[i]);
        assert(char_w < winx);
        if (char_w != -1) {
            if (currline + char_w > winx) {
                ++nlines;
                currline = char_w;
            } else {
                currline += char_w;
            }
        } else {
            assert(line->data[i] == L'\t');
            currline /= TAB_SIZE;
            ++currline;
            currline *= TAB_SIZE;
            if (currline > winx) {
                ++nlines;
                currline = 0;
            }
        }
    }
    return nlines * winx + currline;
}

static lchar_t *
render_max_given_width(lchar_t *beg, lchar_t *end, int width) {
    int currline = 0;
    int char_w;

    while (beg < end && currline < width) {
        char_w = wcwidth(*beg);
        assert(char_w < winx);
        if (char_w != -1) {
            if (currline + char_w > winx) {
                break;
            } else {
                currline += char_w;
            }
        } else {
            assert(*beg == L'\t');
            currline /= TAB_SIZE;
            ++currline;
            currline *= TAB_SIZE;
            if (currline > winx) {
                ++beg;
                break;
            }
        }
        ++beg;
    }
    return beg;
}

static lchar_t *
render_back_max_given_width(lchar_t *beg, lchar_t *end, int width) {
    int currline = 0;
    int char_w;

    while (beg > end && currline < width) {
        char_w = wcwidth(*beg);
        assert(char_w < winx);
        if (char_w != -1) {
            if (currline + char_w > winx) {
                break;
            } else {
                currline += char_w;
            }
        } else {
            assert(*beg == L'\t');
            currline /= TAB_SIZE;
            ++currline;
            currline *= TAB_SIZE;
            if (currline > winx) {
                --beg;
                break;
            }
        }
        --beg;
    }
    return beg;
}

static int
count_nlines_upto(struct Line *line, int size) {
    int chars = count_render_width_upto(line, size);

    if (doc->data + cursy == line && cursx == line->size) {
        // add space for the cursor when is at the end of the line
        ++chars;
    }
    if (chars == 0) {
        return 1;
    }
    return (chars / winx) + (chars % winx != 0);
}

static int
count_nlines(struct Line *line) {
    return count_nlines_upto(line, line->size);
}

static int
count_lines(void) {
    int lines = 0;
    struct Line *line = doc->data + framebeg;

    while (line < doc->data + cursy) {
        lines += count_nlines(line);
        ++line;
    }
    return lines + count_nlines_upto(line, cursx);
}

static void
reposition_frame(void) {
    int overlines;

    if (cursy > framebeg) {
        overlines = count_lines() - winy;
        while (overlines > 0) {
            overlines -= count_nlines(&doc->data[framebeg]);
            ++framebeg;
        }
    } else if (cursy < framebeg) {
        framebeg = cursy;
    }
}

static void
render_lines(void) {
    struct Line *line = doc->data + framebeg;
    struct Line *end = doc->data + doc->size;
    int lines = 0;
    int rest;

    while (lines < winy && line < end) {
        rest = count_nlines(line);
        if (lines + rest <= winy) {
            addnwstr(line->data, line->size);
            lines += rest;
            ++line;
            move(lines, 0);
        } else {
            lchar_t *end = line->data, *beg;
            while (lines != winy) {
                beg = end;
                end = render_max_given_width(end, line->data + line->size,
                                             winx);
                addnwstr(beg, end - beg);
                ++lines;
            }
            ++line;
        }
    }
}

static void
reposition_cursor(void) {
    struct Line *line = doc->data + framebeg;
    int lines = 0;
    int rest;
    int y;
    int x;

    while (line < doc->data + cursy) {
        lines += count_nlines(line);
        ++line;
    }
    rest = count_render_width_upto(line, cursx);
    y = lines + rest / winx;
    x = rest % winx;
    move(y, x);
}

static enum EFILE
load_file_utf8(const char *fname) {
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
                insert_doc(doc->size, &linebuf, 1);
                linebuf.size = 0;
                linebuf.alloc = 0;
                linebuf.data = NULL;
                continue;
            }
            line_insert(&linebuf, linebuf.size, &ch, 1);
        }
    }
    if (linebuf.size != 0) {
        insert_doc(doc->size, &linebuf, 1);
    }
FAILREAD:
    fclose(file);
    return err;
}

int
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
            for (int i = 0; i < size; ++i) {
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
            for (int i = size; i < to_add + size; ++i) {
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
            for (int i = 0; i < size; ++i) {
                own[i] = diff->content[i];
            }
            diff->data = own;
            memcpy(diff->data + size, beg, (end - beg) * sizeof(*diff->data));
            diff->type = DIFF_TYPE_ADDCHR;
        } else {
            for (int i = size; i < to_add + size; ++i) {
                diff->content[i] = (unsigned char)*beg;
                beg++;
            }
        }
    }
    diff->size += to_add;
}

