#define _DEFAULT_SOURCE

#include <ctype.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <string.h>
#include <time.h>
#include <stdarg.h>

#define CTRL_KEY(k) ((k) & 0x1f)

#define TAB_SIZE 4
#define LINENUM_WIDTH 4
#define LINENUM_COLOR 33

enum editorHighlight {
    HL_NORMAL = 0,
    HL_NUMBER,
    HL_STRING,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_MATCH,
};

enum editorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN,
};

typedef struct erow {
    int idx;
    int size;
    int rsize;
    char * chars;
    char * rchars; // render chars like tabs being multipe long
    unsigned char * hl; // highlighting
    int hl_open_comment;
} erow;

struct editorConfig {
    int screenrows;
    int screencols;
    int linenum_width;
    struct editorBuffer * buffers;
    struct editorBuffer * cur_buf;
    int buffers_size;
    char statusmsg[80];
    time_t statusmsg_time;
    struct termios orig_termios;
};
struct editorConfig E;

struct editorBuffer {
    erow * row;
    int numrows;
    int rowoff; // offset from beginning of file
    int cx, cy; // cursor pos
    int rx; // cursor pos counting characters that take more than one column e.g. tabs
    char * filename;
    int dirty; // has file been changed
    struct editorSyntax * syntax;
};

// buffer of things to write to screen
struct abuf {
    char * b;
    int len;
};

#define ABUF_INIT {NULL, 0}

struct editorSyntax {
    char * filetype; // name of filetype
    char ** fileEndings; // list of possible file endings this applies to
    char ** keywords;
    char * singleline_comment_start;
    char * mlcomment_start;
    char * mlcomment_end;
    int flags;
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRINGS (1<<1)

char * C_extensions[] = {".c", ".h", ".cpp", NULL};
char * C_HL_keywords[] = {
"switch", "if", "while", "for", "break", "continue", "return", "else", "struct", "union", "typedef", "enum", "class", "case", "sizeof",

"int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "static", "void|", NULL
};

struct editorSyntax HLDB[] = {
    { "c", C_extensions, C_HL_keywords, "//", "/*", "*/", HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS},
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

void refreshScreen();
void editorSetStatusMessage(const char * fmt, ...);
char * editorPrompt(char * prompt, void (*callback)(char*, int));

void abAppend(struct abuf * ab, const char * s, int len) {
    // reallocate the buffer with space for the new text
    char * new = realloc(ab->b, ab->len + len);

    if (new == NULL) return;

    // copy the new text over and set new as the current buffer
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf * ab) {
    free(ab->b);
}

// terminal things

void die(char * error) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(error);
    exit(1);
}

void disableRawMode() {
    tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios);
}

void enableRawMode() {
    struct termios raw;

    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
        atexit(disableRawMode);

    raw = E.orig_termios;
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1; // wait time to read in tenths of a second

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) die("tcsetattr");
}

int getCursorPosition(int * rows, int * cols) {
    char buf[32];
    unsigned int i = 0;

    // ask for the cursor position
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

    // read characters into the buffer until 'R' is reached
    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    // return if it's not the right response
    if (buf[0] != '\x1b' || buf[1] != '[') return -1; 

    // buf looks like this: <esc>[rows;cols
    // start at buf[2] to skip <esc>[ and read the two numbers
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

    return 0;
}

int getWindowSize(int * rows, int * cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        // get window size the hard way by moving the cursor to the bottom and getting it's position
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return getCursorPosition(rows, cols);
    } else {
        *cols = ws.ws_col;
        *rows = ws.ws_row;
        return 0;
    }
}

int readKey() {
    int nread;
    char c;

    // read to c or die
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN) die("read");
    }

    // if it's a special key e.g. arrow key 
    if (c == '\x1b') {
        char seq[3];

        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
            if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
            if (seq[2] == '~') {
                switch (seq[1]) {
                    case '1': return HOME_KEY;
                    case '3': return DEL_KEY;
                    case '4': return END_KEY;
                    case '5': return PAGE_UP;
                    case '6': return PAGE_DOWN;
                    case '7': return HOME_KEY;
                    case '8': return END_KEY;
                }
            }
            } else {
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
          switch (seq[1]) {
            case 'H': return HOME_KEY;
            case 'F': return END_KEY;
          }
        }

        return '\x1b';
        } else {
        return c;
    }
}

// editor stuff

int rowCxToRx(erow * row, int cx) {
    int rx = 0;
    int j;

    for (j=0; j<cx; j++) {
        if (row->chars[j] == '\t')
            rx += (TAB_SIZE - 1) - (rx % TAB_SIZE);
        rx++;
    }
    return rx;
}

int editorRowRxToCx(erow *row, int rx) {
    int cur_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            cur_rx += (TAB_SIZE - 1) - (cur_rx % TAB_SIZE);
        cur_rx++;
        if (cur_rx > rx) return cx;
    }
    return cx;
}

int editorSyntaxToColor(int hl) {
    // return the color for a highlight type
  switch (hl) {
    case HL_NUMBER: return 31;
    case HL_STRING: return 35;
    case HL_MLCOMMENT:
    case HL_COMMENT: return 36;
    case HL_KEYWORD1: return 33;
    case HL_KEYWORD2: return 32;
    case HL_MATCH: return 34;
    default: return 37;
  }
}

// syntax highlighting

int is_separator(int c) {
  return c == '\0' || strchr(" \t\n,.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow * row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize); // set all of hl to HL_NORMAL

    if (E.cur_buf->syntax == NULL) return;

    char ** keywords = E.cur_buf->syntax->keywords;

    char * scs = E.cur_buf->syntax->singleline_comment_start;
    char * mcs = E.cur_buf->syntax->mlcomment_start;
    char * mce = E.cur_buf->syntax->mlcomment_end;

    int scs_len = scs ? strlen(scs) : 0;
    int mcs_len = scs ? strlen(mcs) : 0;
    int mce_len = scs ? strlen(mce) : 0;

    int prev_sep = 1; // previous character was a seperator
    int in_string = 0; // is set to the type of quote when in string
    int in_comment = (row->idx > 0 && E.cur_buf->row[row->idx - 1].hl_open_comment);

    int i = 0;
    while (i < row->rsize) {
        char c = row->rchars[i];
        unsigned char prev_hl = (i > 0) ? row->hl[i-1] : HL_NORMAL;

        // check if a single line comment has started
        if (scs_len && !in_string && !in_comment) {
            if (!strncmp(&row->rchars[i], scs, scs_len)) {
                // make comment color the default instead of normal
                memset(&row->hl[i], HL_COMMENT, row->rsize - i);
            }
        }

        if (mcs_len && mce_len && !in_string) {
            if (in_comment) {
                row->hl[i] = HL_COMMENT;
                if (!strncmp(&row->rchars[i], mce, mce_len)) {
                    memset(&row->hl[i], HL_MLCOMMENT, mce_len);
                    i += mce_len;
                    in_comment = 0;
                    prev_sep = 1;
                    continue;
                } else {
                    i++;
                    continue;
                }
            } else if (!strncmp(&row->rchars[i], mcs, mcs_len)) {
                // set the highlight for the whole comment start string
                memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
                i += mcs_len;
                in_comment = 1;
                continue;
            }
        }

        if (E.cur_buf->syntax->flags & HL_HIGHLIGHT_STRINGS) {
            if (in_string) {
                row->hl[i] = HL_STRING;

                if (c == '\\' && i + 1 < row->size) {
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                // if c is the type of quote that started the string
                if (c == in_string) in_string = 0; 
                i++;
                prev_sep = 1;
                continue;
            } else {
                // if a string is starting remember the quote type
                if (c == '"' || c == '\'') {
                    in_string = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if (E.cur_buf->syntax->flags & HL_HIGHLIGHT_NUMBERS) {
            // color digits after seperators and periods after numbers
            if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
                (c == '.' && prev_hl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prev_sep = 0;
                continue;
            }
        }

        if (prev_sep) {
            int j;
            for (j=0; keywords[j]; j++) {
                int klen = strlen(keywords[j]);

                // if ending with '|' mark as kw2 and reduce the length to cut it out
                int kw2 = keywords[j][klen - 1] == '|';
                if (kw2) klen--;

                // if currently on a keyword that ends in a seperator color it
                if (!strncmp(&row->rchars[i], keywords[j], klen) &&
                    is_separator(row->rchars[i + klen])) {
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            // the for loop stopped before the end
            if (keywords[j] != NULL) {
                prev_sep = 0;
                continue;
            }
        }

       prev_sep = is_separator(c);
        i++;
    }
    // update syntax for all the rows affected after by mlcomments
    int changed = (row->hl_open_comment != in_comment);
     row->hl_open_comment = in_comment;
     if (changed && row->idx + 1 < E.cur_buf->numrows)
         editorUpdateSyntax(&E.cur_buf->row[row->idx + 1]);
}

void editorSelectSyntaxHighlight(struct editorBuffer * buf) {
    buf->syntax = NULL;

    if (buf->filename == NULL) return;

    char * ext = strrchr(buf->filename, '.');

    // go through each possible highlight type
    for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
        struct editorSyntax * s = &HLDB[j];
        unsigned int i = 0;

        while (s->fileEndings[i]) {
            // if it begins with a dot match file extention otherwise match name
            int is_ext = (s->fileEndings[i][0] == '.');
            if ((is_ext && ext && !strcmp(ext, s->fileEndings[i])) ||
                (!is_ext && strstr(buf->filename, s->fileEndings[i]))) {

                // update highlighting for the entire file
                int filerow;
                for (filerow = 0; filerow < buf->numrows; filerow++) {
                    editorUpdateSyntax(&buf->row[filerow]);
                }

                buf->syntax = s;
                return;
            }
            i++;
        }
    }
}

void editorDrawRows(struct abuf * ab) {
    for (int y = E.cur_buf->rowoff; y < E.screenrows + E.cur_buf->rowoff; y++) {
        // if it's past the number of lines int the file draw a '~'
        if (y >= E.cur_buf->numrows) {
        abAppend(ab, "~", 1);
        } else {
            int len = E.cur_buf->row[y].rsize;
            // cut off the line if it's too long
            if (len > (E.screencols - E.linenum_width)) len = E.screencols - E.linenum_width;

            // draw line numbers
            char buf[5];
            int buflen = snprintf(buf, sizeof(buf), "%-4d", E.cur_buf->row[y].idx + 1);
            abAppend(ab, "\x1b[33m", 5);
            abAppend(ab, buf, buflen);
            abAppend(ab, "\x1b[39m", 5);

            char * c = E.cur_buf->row[y].rchars;
            unsigned char * hl = E.cur_buf->row[y].hl;
            int current_color = -1;

            for (int j = 0; j < len; j++) {
                if (iscntrl(c[j])) {
                    // capital letters are after @ so Ctrl-x would become inverted X
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?'; 
                    abAppend(ab, "\x1b[7m", 4); // invert colors
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3); // turn off formatting
                } else if (hl[j] == HL_NORMAL) {
                    if (current_color != -1) {
                        // go back to normal color
                        abAppend(ab, "\x1b[39m", 5);
                        current_color = -1;
                    }
                    abAppend(ab, &c[j], 1);
                } else {
                    int color = editorSyntaxToColor(hl[j]);
                    // change color if it's different from the current
                    if (color != current_color) {
                        current_color = color;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color); // add color to escape
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3); // clear line after the cursor
        abAppend(ab, "\r\n", 2); // add newline
    }
}

void editorScroll() {
    // find the apparent cursor position
    E.cur_buf->rx = 0;
    if (E.cur_buf->cy < E.cur_buf->numrows) {
        E.cur_buf->rx = rowCxToRx(&E.cur_buf->row[E.cur_buf->cy], E.cur_buf->cx);
    }
    // scroll up or down if the cursor goes off screen
    if (E.cur_buf->cy < E.cur_buf->rowoff) {
        E.cur_buf->rowoff = E.cur_buf->cy;
    }
    if (E.cur_buf->cy >= E.cur_buf->rowoff + E.screenrows) {
        E.cur_buf->rowoff = E.cur_buf->cy - E.screenrows + 1;
    }
}

void editorDrawStatusBar(struct abuf *ab) {
    abAppend(ab, "\x1b[7m", 4); // invert formatting

    char status[80], rstatus[80];

    int len = snprintf(status, sizeof(status), "%.20s%s",
                       E.cur_buf->filename ? E.cur_buf->filename : "[No Name]", E.cur_buf->dirty ? "[+]": "");
    if (len > E.screencols) len = E.screencols;

    int rlen = snprintf(rstatus, sizeof(rstatus), "%.20s | %d/%d : %d ",
                        E.cur_buf->syntax ? E.cur_buf->syntax->filetype : "no ft", E.cur_buf->cy + 1, E.cur_buf->numrows, E.cur_buf->cx + 1);

    abAppend(ab, status, len);

    // fill the rest with spaces
    while (len < E.screencols) {
        // once we reach the length of the right text put it in and stop
        if (E.screencols - len == rlen) {
            abAppend(ab, rstatus, rlen);
            break;
        } else {
            abAppend(ab, " ", 1);
            len++;
        }
    }
    abAppend(ab, "\x1b[m", 3); // return to normal formatting
    abAppend(ab, "\r\n", 2);
}

char * editorPrompt(char * prompt, void (*callback)(char*, int)) {
    size_t bufsize = 128;
    char * buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        // display the prompt and what has been typed
        editorSetStatusMessage(prompt, buf);
        refreshScreen();

        // read the key and add it to the buffer
        int c = readKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
        } else if (c == '\x1b') { // escape aborts
            editorSetStatusMessage("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') { // enter submits
            if (buflen != 0) {
                editorSetStatusMessage("");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
        buf[buflen++] = c;
        buf[buflen] = '\0';
        }
        if (callback) callback(buf, c);
    }
}

void editorSetStatusMessage(const char * fmt, ...) {
    va_list ap; // magic list of parameters

    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap); // print with formatting like printf
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

void editorDrawMessageBar(struct abuf *ab) {
    abAppend(ab, "\x1b[K", 3); //clear line

    int msglen = strlen(E.statusmsg);
    if (msglen > E.screencols) msglen = E.screencols;
    if (msglen && time(NULL) - E.statusmsg_time < 5)
        abAppend(ab, E.statusmsg, msglen);
}

void refreshScreen() {
    struct abuf ab = ABUF_INIT;

    editorScroll();

    abAppend(&ab, "\x1b[?25l", 6); // hide cursor
    // abAppend(&ab, "\x1b[2J", 4); // clear screen
    abAppend(&ab, "\x1b[H", 3); // move cursor to top right

    editorDrawRows(&ab);
    editorDrawStatusBar(&ab);
    editorDrawMessageBar(&ab);

    // move cursor to cursor position
    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cur_buf->cy - E.cur_buf->rowoff) + 1, E.cur_buf->rx + E.linenum_width + 1);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6); // show cursor

    // write the buffer to the screen all at once
    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

// row stuff

void updateRow(erow * row) {
    int tabs = 0;
    int j;

    // count tabs
    for (j = 0; j < row->size; j++)
    if (row->chars[j] == '\t') tabs++;

    // reallocate with room for the tabs being spaces
    free(row->rchars);
    row->rchars = malloc(row->size + tabs * (TAB_SIZE - 1) + 1);

    // go through replacing tabs with spaces
    int idx = 0;
    for (j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            row->rchars[idx++] = ' ';
            while (idx % TAB_SIZE != 0) row->rchars[idx++] = ' ';
        } else {
            row->rchars[idx++] = row->chars[j];
        }
    }
    row->rchars[idx] = '\0';
    row->rsize = idx;

    editorUpdateSyntax(row);
}

void editorInsertRow(int at, char *s, size_t len) {
    // allocate space for a new row
    E.cur_buf->row = realloc(E.cur_buf->row, sizeof(erow) * (E.cur_buf->numrows + 1)); 
    // move all the rows over to fit
    memmove(&E.cur_buf->row[at + 1], &E.cur_buf->row[at], sizeof(erow) * (E.cur_buf->numrows - at)); 

    erow * row = &E.cur_buf->row[at];

    // incriment the idx of all the rows
    for (int j = at + 1; j <= E.cur_buf->numrows; j++) E.cur_buf->row[j].idx++;
    row->idx = at;

    // create the row and copy the chars over
    row->size = len;
    row->chars = malloc(len + 1);
    memcpy(row->chars, s, len);
    row->chars[len] = '\0';

    row->rsize = 0;
    row->rchars = NULL;
    row->hl = NULL;
    row->hl_open_comment = 0;
    updateRow(row);

    E.cur_buf->numrows++;
    E.cur_buf->dirty++;
}

void rowFree(erow * row) {
    free(row->rchars);
    free(row->chars);
    free(row->hl);
}

void editorDelRow(int at) {
    rowFree(&E.cur_buf->row[at]);
    memmove(&E.cur_buf->row[at], &E.cur_buf->row[at + 1], sizeof(erow) * (E.cur_buf->numrows - at - 1));

    // update idx of rows
    for (int j = at; j < E.cur_buf->numrows - 1; j++) E.cur_buf->row[j].idx--;

    E.cur_buf->numrows--;
    E.cur_buf->dirty = 1;
}

void rowInsertChar(erow * row, int at, int c) {
    // if the insert place doesn't exist use the end
    if (at < 0 || at > row->size) at = row->size; 

    row->chars = realloc(row->chars, row->size + 2);
    // move everything after at over to make space
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    updateRow(row); // update how it's displayed
    E.cur_buf->dirty = 1; // mark file as changed
}

void rowAppendString(erow * row, char * s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    updateRow(row);
    E.cur_buf->dirty++;
}

void editorInsertChar(int c) {
    if (E.cur_buf->cy == E.cur_buf->numrows) {
        editorInsertRow(E.cur_buf->numrows, "", 0);
    }
    rowInsertChar(&E.cur_buf->row[E.cur_buf->cy], E.cur_buf->cx, c);
    E.cur_buf->cx++;
}

void rowDelChar(erow * row, int at) {
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    updateRow(row);
    E.cur_buf->dirty = 1;
}

void editorDelChar() {
    if (E.cur_buf->cx == 0 && E.cur_buf->cy == 0) return;

    erow * row = &E.cur_buf->row[E.cur_buf->cy];
    if (E.cur_buf->cx > 0) {
        rowDelChar(row, E.cur_buf->cx - 1);
        E.cur_buf->cx--;
    } else { // combine lines if at the beginning
        E.cur_buf->cx = E.cur_buf->row[E.cur_buf->cy - 1].size;
        rowAppendString(&E.cur_buf->row[E.cur_buf->cy - 1], row->chars, row->size);
        editorDelRow(E.cur_buf->cy);
        E.cur_buf->cy--;
    }
}

void editorInsertNewline() {
    if (E.cur_buf->cx == 0) {
        editorInsertRow(E.cur_buf->cy, "", 0);
    } else {
        erow *row = &E.cur_buf->row[E.cur_buf->cy];
        editorInsertRow(E.cur_buf->cy + 1, &row->chars[E.cur_buf->cx], row->size - E.cur_buf->cx);
        row = &E.cur_buf->row[E.cur_buf->cy];
        row->size = E.cur_buf->cx;
        row->chars[row->size] = '\0';
        updateRow(row);
    }
    E.cur_buf->cy++;
    E.cur_buf->cx = 0;
}

char * editorRowsToString(int * buflen) {
    // get total length
    int totlen = 0;
    int j;
    for (j=0; j<E.cur_buf->numrows; j++)
        totlen += E.cur_buf->row[j].size + 1;
    *buflen = totlen;

    char * buf = malloc(totlen);

    // copy over each line
    char * p = buf;
    for (j=0; j<E.cur_buf->numrows; j++) {
        memcpy(p, E.cur_buf->row[j].chars, E.cur_buf->row[j].size);
        p += E.cur_buf->row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

void editorFindCallback(char * query, int key) {
    static int last_match;
    static int direction;

    // save hl to restore
    static int saved_hl_line;
    static char *saved_hl = NULL;
    if (saved_hl) {
        memcpy(E.cur_buf->row[saved_hl_line].hl, saved_hl, E.cur_buf->row[saved_hl_line].rsize);
        free(saved_hl);
        saved_hl = NULL;
    }

    if (key == '\r' || key == '\x1b') {
        // stop searching
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key ==  ARROW_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;

    int i;
    for (i = 0; i < E.cur_buf->numrows; i++) {
        current += direction;
        if (current == -1) current = E.cur_buf->numrows - 1;
        else if (current == E.cur_buf->numrows) current = 0;

        erow * row = &E.cur_buf->row[current];
        char * match = strstr(row->rchars, query); // check if rchars contains query
        if (match) {
            last_match = current;
            E.cur_buf->cy = current;
            E.cur_buf->cx = editorRowRxToCx(row, match - row->rchars);
            E.cur_buf->rowoff = E.cur_buf->numrows;

            saved_hl_line = current;
            saved_hl = malloc(row->rsize);
            memcpy(saved_hl, row->hl, row->rsize);
            memset(&row->hl[match - row->rchars], HL_MATCH, strlen(query));

            break;
        }
    }
}

void editorFind() {
    int cx = E.cur_buf->cx;
    int cy = E.cur_buf->cy;
    int rowoff = E.cur_buf->rowoff;

    char * query = editorPrompt("Search: %s (Use Arrow Keys)", editorFindCallback);

    if (query) {
        free(query);
    } else {
        E.cur_buf->cx = cx;
        E.cur_buf->cx = cy;
        E.cur_buf->rowoff = rowoff;
    }
}

void editorAddBuffer(char * filename) {
    E.buffers = realloc(E.buffers, sizeof(struct editorBuffer) * ++E.buffers_size);
    struct editorBuffer * buf = &(E.buffers[E.buffers_size - 1]);

    buf->filename = strdup(filename);
    buf->numrows = 0;
    buf->row = NULL;
    buf->syntax = NULL;
    buf->cx = 0;
    buf->cy = 0;

    E.cur_buf = buf;

    editorSelectSyntaxHighlight(E.cur_buf);

    // if file exists
    if (access(filename, F_OK) != -1) {
        FILE *file = fopen(filename, "r");
        if (!file) die("fopen");

        char *line = NULL;
        size_t linecap = 0;
        ssize_t len;

        // go through each line of the file adding it
        while ((len = getline(&line, &linecap, file)) != -1) {
            // remove lewline
            if (len != -1 && line[len - 1] == '\n') len--;
            editorInsertRow(E.cur_buf->numrows, line, len);
        }

        free(line);
        fclose(file);
    }

    buf->dirty = 0;
}

void editorSaveBuffer() {
    if (E.cur_buf->filename == NULL) {
        E.cur_buf->filename = editorPrompt("Save as: %s", NULL);

        if (E.cur_buf->filename == NULL) {
            editorSetStatusMessage("Save aborted");
            return;
        }

        editorSelectSyntaxHighlight(E.cur_buf);
    }

    int len;
    char * buf = editorRowsToString(&len);

    // O_CREAT creates file if missing, O_RDWR reads and writes, 0664 is premissions
    int file = open(E.cur_buf->filename, O_RDWR | O_CREAT, 0644);
    if (file != -1) {
        if (ftruncate(file, len) != len) {
            if (write(file, buf, len) == len) {
            close(file);
            free(buf);
            editorSetStatusMessage("%d bytes written to disk", len);
            E.cur_buf->dirty = 0;
            return;
            }
        }
        close(file);
    }
    editorSetStatusMessage("Failed to save! I/O error: %s", strerror(errno));
    free(buf);
}

void editorMoveCursor(int key) {
    erow *row = (E.cur_buf->cy >= E.cur_buf->numrows) ? NULL : &E.cur_buf->row[E.cur_buf->cy];

    switch (key) {
        case ARROW_LEFT:
            if (E.cur_buf->cx != 0) {
                E.cur_buf->cx--;
            }
            break;
        case ARROW_RIGHT:
            if (row && E.cur_buf->cx < row->size) {
                E.cur_buf->cx++;
            }
            break;
        case ARROW_UP:
            if (E.cur_buf->cy != 0) {
                E.cur_buf->cy--;
            }
            break;
        case ARROW_DOWN:
            if (E.cur_buf->cy < E.cur_buf->numrows - 1) {
                E.cur_buf->cy++;
        }
        break;
    }
}

void deleteWord() {
    erow * row = &E.cur_buf->row[E.cur_buf->cy];
    int word = 0;

    while (row->size) {
        if ((row->chars[E.cur_buf->cx - 1] == ' ') ^ word) {
            editorDelChar();
        } else if (word) {
            break;
        } else {
            word = 1;
        }
    }
}

void processKey() {
    int c = readKey();

    // get the current row
    erow * row = (E.cur_buf->cy >= E.cur_buf->numrows) ? NULL : &E.cur_buf->row[E.cur_buf->cy];

    switch (c) {
        case CTRL_KEY('q'):
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'):
            editorSaveBuffer();
            break;

        case CTRL_KEY('f'):
            editorFind();
            break;

        case CTRL_KEY('w'):
            deleteWord();
            break;

        case CTRL_KEY('g'):
            {
                int num = atoi(editorPrompt("line number: %s", NULL)) - 1;
                if (num < 0 || num > E.cur_buf->numrows - 1) break;

                E.cur_buf->cy = num;
                E.cur_buf->cx = 0;
            }
            break;

        // select file
        case CTRL_KEY('p'):
            {
                char * filename = editorPrompt("file name: %s", NULL);
                int exists = 0;
                for (int i=0; i<E.buffers_size; i++) {
                    if (!strcmp(E.buffers[i].filename, filename)) {
                        E.cur_buf = &E.buffers[i];
                        exists = 1;
                        break;
                    }
                }
                if (!exists)
                    editorAddBuffer(filename);
            }
            break;

        case '\r': // enter
            editorInsertNewline();
            break;

        case DEL_KEY:
            editorMoveCursor(ARROW_RIGHT);
            // fall through
        case BACKSPACE:
        case CTRL_KEY('h'):
            editorDelChar();
            break;

        // cursor movement
        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            editorMoveCursor(c);
            break;

        case PAGE_DOWN:
        case CTRL_KEY('d'):
            // place cursor at bottom of screen
            E.cur_buf->cy = E.cur_buf->rowoff + E.screenrows - 1; 
            if (E.cur_buf->cy > E.cur_buf->numrows) E.cur_buf->cy = E.cur_buf->numrows - 1;
            // move down the screens size
            for (int i = 0; i<E.screenrows; i++) editorMoveCursor(ARROW_DOWN);
            break;

        case PAGE_UP:
        case CTRL_KEY('u'):
            E.cur_buf->cy = E.cur_buf->rowoff;
            for (int i=0; i<E.screenrows; i++) editorMoveCursor(ARROW_UP);
            break;

        case END_KEY:
        case CTRL_KEY('e'):
            if (E.cur_buf->cy < E.cur_buf->numrows)
                E.cur_buf->cx = E.cur_buf->row[E.cur_buf->cy].size;
            break;

        case HOME_KEY:
        case CTRL_KEY('a'):
            E.cur_buf->cx = 0;
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            break;

        default:
            editorInsertChar(c);
            break;
    }

    // move the cursor back if it goes beyond a the legth of a line
    row = (E.cur_buf->cy >= E.cur_buf->numrows) ? NULL : &E.cur_buf->row[E.cur_buf->cy];
    int rowlen = row ? row->size : 0;
    if (E.cur_buf->cx > rowlen)
        E.cur_buf->cx = rowlen;
}

void initEditor() {
    E.linenum_width = LINENUM_WIDTH;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;

    if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
    E.screenrows -= 2; // take line off for status-bar
}

int main(int argc, char ** argv) {
    enableRawMode();
    initEditor();
    if (argc >= 2) {
        editorAddBuffer(argv[1]);
    } else {
        editorAddBuffer("test.c");
    }

    editorSetStatusMessage("Help: Ctrl-S = save | Ctrl-Q = quit");

    while (1) {
        // editorSetStatusMessage(&E.cur_buf->row[E.cur_buf->cy].chars[E.cur_buf->cx]);
        refreshScreen();
        processKey();
    }

    return 0;
}
