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
#define SMALL_STR 20
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
    DIFF_TYPE_ADDCHR,
    DIFF_TYPE_DELCHR_SMALL,
    DIFF_TYPE_DELCHR,
    // DIFF_TYPE_SUB,
    DIFF_TYPE_ADDBRK,
    DIFF_TYPE_DELBRK,
} DIFF_TYPE;

typedef struct __attribute__((packed)) Diff {
    struct {
        enum DIFF_TYPE type : 3;
        unsigned size : 29;
        int y;
        int x;
    };
    union {
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

typedef struct DiffArr {
    long long size;
    long long alloc;
    Diff _Alignas(32) data[];
} DiffArr;

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
static struct DiffArr *vecdiff = NULL;
static int cursx = 0;
static int cursy = 0;
static int framebeg = 0;
static int winx;
static int winy;
static int pgspan;

static int always(lint_t);
static void reserve_vecdiff(void);
static void insert_addbrk(int, int);
static void insert_delbrk(int, int);
static void insert_addchr(lchar_t);
static void insert_diff(struct Diff *, lchar_t);
static void insert_delchr(lchar_t);
static void insert_doc(int, struct Line *, int);
static void insert_line(struct Line *, int, lchar_t *, int);
static void merge_lines(int);
static void remove_slice(struct Line *, int, int);
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
int revert_diff(void);
static int handle_input(lint_t);
static int render_loop(void);

static int count_render_width_upto(struct Line *, int);
static lchar_t * render_max_given_width(lchar_t *, lchar_t *, int);
static lchar_t * render_back_max_given_width(lchar_t *, lchar_t *, int);
static int count_nlines_upto(struct Line *, int);
static int count_nlines(struct Line *);
static int count_lines(void);
static void reposition_frame(void);
static void render_lines(void);
static void reposition_cursor(void);
static enum EFILE load_file_utf8(const char *);

static void
reserve_vecdiff(void) {
    if (vecdiff->size == vecdiff->alloc) {
        xrealloc_arr(&vecdiff, vecdiff->alloc * 2 + 1);
        vecdiff->alloc = vecdiff->alloc * 2 + 1;
    }
}

static void
insert_addbrk(int y, int x) {
    reserve_vecdiff();
    long long pos = vecdiff->size;
    struct Diff *curr = &vecdiff->data[pos];
    struct Diff *prev = &vecdiff->data[pos - 1];

    if (pos == 0 || prev->type != DIFF_TYPE_ADDBRK ||
        prev->size == MAX_DIFF_SIZE) {
        curr->type = DIFF_TYPE_ADDBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
        vecdiff->size++;
    } else {
        if (prev->y == cursy - prev->size && cursx == 0) {
            prev->size++;
        } else {
            curr->type = DIFF_TYPE_ADDBRK;
            curr->size = 1;
            curr->y = y;
            curr->x = x;
            vecdiff->size++;
        }
    }
}

static void
insert_delbrk(int y, int x) {
    reserve_vecdiff();
    long long pos = vecdiff->size;
    struct Diff *curr = &vecdiff->data[pos];
    struct Diff *prev = &vecdiff->data[pos - 1];

    if (pos == 0 || prev->type != DIFF_TYPE_DELBRK ||
        prev->size == MAX_DIFF_SIZE) {
        curr->type = DIFF_TYPE_DELBRK;
        curr->size = 1;
        curr->y = y;
        curr->x = x;
        vecdiff->size++;
    } else {
        if (prev->y == cursy + prev->size && prev->x == 0) {
            prev->x = cursx;
            prev->y--;
            prev->size++;
        } else {
            curr->type = DIFF_TYPE_DELBRK;
            curr->size = 1;
            curr->y = y;
            curr->x = x;
            vecdiff->size++;
        }
    }
}

static void
insert_addchr(lchar_t ch) {
    reserve_vecdiff();
    long long pos = vecdiff->size;
    struct Diff *curr = &vecdiff->data[pos];
    struct Diff *prev = &vecdiff->data[pos - 1];
    filt_fn_t curr_fil = find_match_fil(ch);
    filt_fn_t prev_fil;

    if (pos == 0 || prev->type != DIFF_TYPE_ADDCHR ||
        prev->size == MAX_DIFF_SIZE) {
        curr->type = DIFF_TYPE_ADDCHR;
        curr->size = 1;
        curr->x = cursx;
        curr->y = cursy;
        curr->fil = curr_fil;
        vecdiff->size++;
    } else {
        prev_fil = prev->fil;
        if (prev->y == cursy && prev->x + prev->size == cursx &&
            curr_fil == prev_fil) {
            prev->size++;
        } else {
            curr->type = DIFF_TYPE_ADDCHR;
            curr->size = 1;
            curr->x = cursx;
            curr->y = cursy;
            curr->fil = curr_fil;
            vecdiff->size++;
        }
    }
}

static void
insert_diff(struct Diff *diff, lchar_t ch) {
    lchar_t *own = NULL;
    int size = diff->size;

    if (diff->type == DIFF_TYPE_DELCHR) {
        xrealloc_ptr(&diff->data, size + 1, sizeof(*diff->data), 0);
        diff->data[size] = ch;
    } else {
        if ((ch >> 7) || diff->size == SMALL_STR) {
            xrealloc_ptr(&own, size + 1, sizeof(*diff->data), 0);
            for (int i = 0; i < size; ++i) {
                own[i] = diff->content[i];
            }
            diff->data = own;
            diff->data[size] = ch;
            diff->type = DIFF_TYPE_DELCHR;
        } else {
            diff->content[size] = (unsigned char)ch;
        }
    }
    diff->size++;
}

static void
insert_delchr(lchar_t ch) {
    reserve_vecdiff();
    long long pos = vecdiff->size;
    struct Diff *curr = &vecdiff->data[pos];
    struct Diff *prev = &vecdiff->data[pos - 1];
    filt_fn_t curr_fil = find_match_fil(ch);
    filt_fn_t prev_fil;

    if (pos == 0 ||
        !(prev->type == DIFF_TYPE_DELCHR ||
          prev->type == DIFF_TYPE_DELCHR_SMALL) ||
        prev->size == MAX_DIFF_SIZE) {
        curr->type = DIFF_TYPE_DELCHR_SMALL;
        curr->size = 0;
        curr->x = cursx;
        curr->y = cursy;
        insert_diff(curr, ch);
        vecdiff->size++;
    } else {
        prev_fil = prev->type == DIFF_TYPE_DELCHR_SMALL
                           ? find_match_fil(prev->content[0])
                           : find_match_fil(prev->data[0]);
        if (prev->y == cursy && prev->x - prev->size == cursx &&
            curr_fil == prev_fil) {
            insert_diff(prev, ch);
        } else {
            curr->type = DIFF_TYPE_DELCHR_SMALL;
            curr->size = 0;
            curr->x = cursx;
            curr->y = cursy;
            insert_diff(curr, ch);
            vecdiff->size++;
        }
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
insert_line(struct Line *line, int pos, lchar_t *cpy, int size) {
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

    insert_line(line, line->size, next->data, next->size);
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
remove_slice(struct Line *line, int pos, int size) {
    lchar_t *beg = line->data + pos;
    lchar_t *next = line->data + pos + size;
    lchar_t *end = line->data + line->size;

    assert(next <= end);

    // OPT, also sucky API
    cursx += size;
    for (lchar_t *i = next - 1; i != beg - 1; --i) {
        cursx--;
        insert_delchr(*i);
    }

    memmove(beg, next, (end - next) * sizeof(*line->data));


    line->size -= size;
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

    if (SIZE_MAX - head < nsize && nmemb && size && SIZE_MAX / nmemb < size) {
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
    xrealloc_arr(&vecdiff, INIT_DIFF);

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

    memset(vecdiff, 0, sizeof(DiffArr) + sizeof(*vecdiff->data) * INIT_DIFF);
    vecdiff->alloc = INIT_DIFF;
    vecdiff->size = 0;

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

        insert_delbrk(cursy, cursx);
    } else {

        line = &doc->data[cursy];
        --cursx;
        remove_slice(line, cursx, 1);
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

int
revert_diff(void) {
    struct Diff *diff = &vecdiff->data[vecdiff->size - 1];
    struct Line own = { 0, 0, 0 };
    struct Line *line;
    lchar_t *beg;
    lchar_t *end;

    if (vecdiff->size == 0) {
        return -1;
    }
    switch (diff->type) {
    default:
        vecdiff->size--;
        return -1;
    case DIFF_TYPE_ADDBRK:
        cursx = 0;
        cursy = diff->y + diff->size - 1;
        assert(cursy >= 0);
        line = &doc->data[cursy];
        cursx = line->size;
        merge_lines(cursy);
        diff->size--;
        if (diff->size == 0) {
            vecdiff->size--;
        }
        break;
    case DIFF_TYPE_ADDCHR:
        cursx = diff->x;
        cursy = diff->y;
        line = &doc->data[cursy];
        memmove(line->data + cursx, line->data + cursx + diff->size,
                (line->size - diff->size - cursx) * sizeof(*line->data));
        line->size -= diff->size;
        vecdiff->size--;
        break;
    case DIFF_TYPE_DELBRK:
        line = &doc->data[diff->y];
        insert_line(&own, 0, line->data + diff->x, line->size - diff->x);
        insert_doc(diff->y + 1, &own, 1);
        line->size = diff->x;
        diff->x = 0;
        diff->y++;
        cursx = diff->x;
        cursy = diff->y;
        diff->size--;
        if (diff->size == 0) {
            vecdiff->size--;
        }
        break;
    case DIFF_TYPE_DELCHR:
    case DIFF_TYPE_DELCHR_SMALL:
        line = &doc->data[diff->y];
        cursx = diff->x + 1;
        cursy = diff->y;

        if (line->size + diff->size > line->alloc) {
            xrealloc_owndata(line, line->alloc * 2 + diff->size);
            line->alloc = line->alloc * 2 + diff->size;
        }
        beg = line->data + cursx - diff->size;
        end = line->data + line->size;

        memmove(beg + diff->size, beg, (end - beg) * sizeof(*line->data));
        line->size += diff->size;
        if (diff->type == DIFF_TYPE_DELCHR_SMALL) {
            for (int i = diff->size; i; --i) {
                beg[diff->size - i] = diff->content[i - 1];
            }
        } else {
            for (int i = diff->size; i; --i) {
                beg[diff->size - i] = diff->data[i - 1];
            }
            free(diff->data);
        }
        vecdiff->size--;
        break;
    }
    return 0;
}

static int
handle_input(lint_t c) {
    struct Line own = { 0, 0, 0 };
    struct Line *line = &doc->data[cursy];
    unsigned rest;
    lchar_t ch = c;

    assert(cursy < doc->size);
    assert(cursx <= line->size);

    switch (ch) {
    case KEY_CTRL_CANC:
        return 0;
    case L'\n':
        insert_addbrk(cursy, cursx);
        insert_line(&own, 0, line->data + cursx, line->size - cursx);
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
            rest = cursx;
            cursx = find_prev_simil(line->data, rest);
            remove_slice(line, cursx, rest - cursx);
            break;
        }
        break;
    case 21:
        if (strcmp(keyname(ch), "^U") == 0) {
            revert_diff();
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
            insert_addchr(ch);
            insert_line(line, cursx, &ch, 1);
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
            insert_line(&linebuf, linebuf.size, &ch, 1);
        }
    }
    if (linebuf.size != 0) {
        insert_doc(doc->size, &linebuf, 1);
    }
FAILREAD:
    fclose(file);
    return err;
}
