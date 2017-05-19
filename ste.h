static struct Editor *
init_editor(const char *fname);
static int
render_loop(struct Editor *edp);
static void
xrealloc_ptr(void *p, size_t nmemb, size_t size, size_t head);
static void
init_docfile(struct Editor *edp, const char * fname);
static void
open_win(struct Editor *edp);
static void
xinit_doc(struct Editor *edp, size_t sz, const char *fname);
static void
line_insert(struct Line *line, int pos, lchar_t *cpy, int size);
static lchar_t *
u32str_convert(const char *fname);
static void
clear_window(struct Editor *edp);
static void
calc_window_size(struct Editor *edp);
static void
reposition_frame(struct Editor *edp);
static void
render_lines(struct Editor *edp);
static void
render_editor_info(struct Editor *edp);
static void
reposition_cursor(struct Editor *edp);
static int
handle_input(struct Editor *edp, lint_t c);
static enum EFILE
load_file_utf8(struct Editor *edp, const char *fname);
static lchar_t
denormalize_xcale(unsigned char in);
static void
move_brush(struct Editor *edp, int y, int x);
static int
calc_padlx(struct Editor *edp);
static int
count_lines(struct Editor *edp);
static int
count_nlines(struct Editor *edp, struct Line *line);
static lchar_t *
render_max_given_width(struct Window *win, lchar_t *beg, lchar_t *end,
                       int width);
static void
paint_string(struct Editor *edp, lchar_t *str, int size, int y, int x);
static wchar_t *
wmkstr_nmt(const wchar_t *fmt, ...);
static int
count_render_width_upto(struct Window *win, struct Line *line, int size);
static int
delete_lines(struct Editor *edp, struct Selection *selct);
static int
insert_vert(struct Editor *edp, lchar_t ch);
static void
diffstk_insert_span(struct Editor *edp, lchar_t *str, int delta);
static void
merge_lines(struct LineArr *doc, int pos);
static void
insert_doc(struct LineArr **doc, int pos, struct Line *cpy, int size);
static void
diffstk_insert_addbk(struct Editor *edp, int y, int x);
static int
move_right(struct Editor *edp);
static int
move_left(struct Editor *edp);
static int
move_up(struct Editor *edp);
static int
move_down(struct Editor *edp);
static int
find_prev_simil(const void *data, int cursor);
static int
find_next_simil(const void *data, int cursor, int size);
static int
move_pgup(struct Editor *edp);
static int
move_pgdown(struct Editor *edp);
static int
del_back(struct Editor *edp);
static void
line_remove_span(struct Line *line, int pos, int delta);
static int
diffstk_apply_last(struct Editor *edp, enum DIREC direc);
static void
save_current(struct LineArr *doc, struct FileInfo *info,
             struct DiffStk *diffstk);
static int
usr_quit(struct Editor *edp);
static int
move_to_word(struct Editor *edp, const lchar_t *str, long size);
static int
delete_vert(struct Editor *edp, struct Selection *s);
static void
show_keymap(void);
static int
edit_vert(struct Editor *edp);
static int
exec_command(struct Editor *edp, lchar_t *str, int size);
static void
file_chooser(struct Editor *edp, enum FILE_ACT action);
static int
num_digits(int i, int base);
static void
regularize_selection(struct Selection *in, struct Selection *out);
static void
init_split_iter(struct SplitIter *it, lchar_t *str, int size, int beg,
                int end);
static int
iter_split(struct SplitIter *it, struct Range *delta);
static void
diffstk_reserve(struct DiffStk **diffstk);
static filt_fn_t
find_match_fil(lint_t ch);
static void
diffstk_incr(struct DiffStk *diffstk);
static void
diff_insert_span(struct Diff *diff, lchar_t *str, int delta);
static void
diffstk_insert_delbk(struct Editor *edp, int y, int x);
static void
diff_apply_brk(struct Editor *edp, struct Diff *diff, enum DIREC direc);
static void
diff_apply_chr(struct Editor *edp, struct Diff *diff, enum DIREC direc);
static int
diffstk_apply_all(struct Editor *edp, struct DiffStk *ds, enum DIREC direc);
static void
save_file_utf8(struct LineArr *doc, struct FileInfo *info,
               struct DiffStk *diffstk);
static char *
mkstr_nmt(const char *fmt, ...);
static char *
u8str_convert(lchar_t *fname, int size);
static void
diffstk_free_all(struct DiffStk *ds);
static int
normalize_xcape(lchar_t in);
static int
replace_word(struct Editor *edp, struct Selection *selct, lchar_t *str,
             int size, lchar_t *nstr, int nsize);
static lchar_t *
render_back_max_given_width(struct Window *win, lchar_t *beg, lchar_t *end,
                            int width);
static int
realloc_ptr(void *p, size_t nmemb, size_t size, size_t head);
static int
eq_bigch(struct Diff *diff, enum DIFF_TYPE small);
static int
move_down_natural(struct Editor *edp);
static void
line_remove_span_qdiff(struct Editor *edp, struct Line *line, int pos,
                       int delta);
static int
always(lint_t p);
static int
count_nlines_upto(struct Editor *edp, struct Line *line, int size);
static int
is_ascii(lchar_t *str, int size);
static int
replace_word_positrange(struct Editor *edp, struct Selection *selct,
                        lchar_t *str, int size, lchar_t *nstr, int nsize);
static void
insert_doc_nl_times(struct LineArr **doc, int pos, int size);
static int
move_up_natural(struct Editor *edp);
static void
close_win(void);
static int
delete_lines_positrange(struct Editor *edp, struct Selection *selct);
