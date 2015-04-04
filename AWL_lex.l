
%{

/*

	+---+---+---+---+---+---+
	|	'AWL_lex.l':
	|	AWL lexical analizer core.
	|
	|	AWL source.
	|	Written by trilirium
	+---+---+---+---+---+---+

 */


#include "Defs.h"

#include "Lexer.h"

#include "Logger.h"

// Lexer interface variables

enum { MaxIdLen = 255 };		// Max identifier length

char LV_ident[MaxIdLen+1];	// identifier

long LV_long;				// long literal
double LV_double;			// double literal

char *LV_string;			// string literal
unsigned LV_strlen;			// string length
int LV_strtype;				// string type (single char, if < 0)

// Copy C-string 'from' to 'to' (not more than 'n + 1' bytes)
void strcpyL (char *to, char *from, unsigned n) {
while (*to ++ = *from ++)
	if (! n --) { * -- to = '\0'; break; }
}	// strcpyL

// Copy block 'from' to 'to' until 'delim'
// (not more than 'n + 1' bytes)
void strcpyLD (char *to, char *from, char delim, unsigned n) {
while ((* to ++ = * from ++) != delim)
	if (! n --) break;
* -- to = '\0';
}	// strcpyLD

/* Parse decimal int from string */
char *parse_decint (char *str, long &val, char *limit) {
unsigned char ch;
val = 0;
while (str != limit &&
	'0' <= (ch = *str) && ch <= '9')
	{ val *= 10; val += ch - '0'; ++ str; }
return str;
}	/* parse_decint */

/* Parse hexadecimal int from string */
char *parse_hexint (char *str, long &val, char *limit) {
unsigned char ch;
val = 0;
while (str != limit && (
	'0' <= (ch = *str) && ch <= '9' ? (ch -= '0', true) :
    'A' <= ch && ch <= 'F' ? (ch -= 'A' - 10, true) :
    'a' <= ch && ch <= 'f' ? (ch -= 'a' - 10, true) :
		false))
		{ val <<= 4; val |= ch; ++ str; }
return str;
}	/* parse_hexint */

/* Parse octal int from string */
char *parse_octint (char *str, long &val, char *limit) {
unsigned char ch;
val = 0;
while (str != limit &&
	'0' <= (ch = *str) && ch <= '7')
		{ val <<= 3; val |= ch - '0'; ++ str; }
return str;
}	/* parse_octint */

/* Parse binary int from string */
char *parse_binint (char *str, long &val, char *limit) {
unsigned char ch;
val = 0;
while (str != limit &&
	((ch = *str) == '0' || ch == '1'))
		{ val <<= 1; val |= ch - '0'; ++ str; }
return str;
}	/* parse_binint */

#include <math.h>

/* Parse float from string */
#ifdef OWN_PARSE_FLOAT

char *parse_float (char *str, double &val) {
unsigned char ch;
int exp = 0, sign = 1;
val = 0.0;

while ('0' <= (ch = * str ++) && ch <= '9')
	val *= 10.0, val += ch - '0';

if (ch == '.') {
	while ('0' <= (ch = * str ++) && ch <= '9')
		val *= 10.0, val += ch - '0', exp --;
	}

if (ch == 'e' || ch == 'E') {
	switch (* str) {
	 	case '+': str ++; break;
		case '-': str ++; sign = - sign; break;
		default:  break;
		}
	unsigned exp1;
	str = parse_decint (str, exp1) + 1;
	exp += exp1 * sign;
	}
val *= pow (10, exp);
return -- str;
}	/* parse_float */

#else

char *parse_float (char *str, double &val) {
val = strtod (str, &str);
return str;
}	/* parse_float */

#endif

static unsigned parse_nchar (char *&ptr, char *limit, unsigned max,
	char *(*parse_int) (char *str, long &val, char *limit)) {
if (limit - ptr > max) limit = ptr + max;

long val;
ptr = parse_int(ptr, val, (limit - ptr > max) ? ptr + max : limit);

return val;
}	// parse_nchar

static unsigned char parse_control (char *&ptr, char *limit) {
if (limit != ptr) {
	unsigned code = *ptr ++;

	return
		(0x40 <= code && code < 0x80) ?
			code < 0x60 ? code - 0x40 : code - 0x60
		:	0;
	}

return 0;
}	// parse_control

// (in "char_symdef.cpp"...)
int symdef_lookup (char *from, unsigned len);

static unsigned parse_namecode (char *from, char *to) {
int value = symdef_lookup (from, to-from);

if (value < 0) {
	syslog->put_cstr ("Illegal namecode: ");
	while (from != to)
		syslog->put_ch (*from ++);
	syslog->put_cstr (".\n");
	return 0;		// consider it 0
	}

return value;
}	// parse_namecode

static unsigned parse_char (bool type, char *&text, char *limit) {
if (text != limit)
    if (*text != '\\')
		return *text ++;

    else if (++ text != limit)
	switch (*text ++) {
		case 'a':	return '\a';			// alert
		case 'n':	return '\n';			// line feed
		case 'r':	return '\r';			// carriage return
		case 't':	return '\t';			// tab
		case 'b':	return '\b';			// backspace
		case 'f':	return '\f';			// form feed
		case 'v':	return '\v';			// vertical tab
		case 'e':	return '\e';			// escape

		case 'x':
		case 'h':	// hexadecimal literal
					return parse_nchar (text, limit, type ? 4 : 2, parse_hexint);

		case 'o':	// octal literal
					return parse_nchar (text, limit, type ? 6 : 3, parse_octint);

		case 'd':	// decimal literal
					return parse_nchar (text, limit, type ? 5 : 3, parse_decint);

		case 'y':	// binary literal
					return parse_nchar (text, limit, type ? 16 : 8, parse_binint);

		case 'c':
		case '^':
					return parse_control (text, limit);	// control char...

		case 'B':	return 12;				// line break
		case 'P':	return 13;				// paragraph break
		case ' ':	return 0xA0;			// non-break space

		case '[': {
			char *end = text;
			while (end != limit && *end != ']') ++ end;
			if (end != limit) {
				char *start = text;
				text = end+1;
				return parse_namecode (start, end);
				}
			return 0;			// (illegal...)
			}

		case '\'':	return '\'';		// quotes
		case '"':	return '"';

		case '\\':	return '\\';		// escape
		}

return '\0';
}	// parse_char

// (buffer for parsed string content)
static char *str_buffer = 0;
static unsigned str_buflen = 0;

// Parse string of characters
char *parse_string (char *text, unsigned length) {
char *limit = text + length, *text1;
bool type16 = LV_strtype < 0 ? true : LV_strtype;

// (first calculate total count)
unsigned count = 0;
for (text1 = text; text1 < limit; count ++)
	parse_char (type16, text1, limit);

LV_strlen = count;

unsigned str_len = (count << type16) + 1;		// (for '\0')

if (str_len > str_buflen) {		// (more than currently)
	if (str_buffer) delete [] str_buffer;
	str_buffer = new ("<str_buffer>") char [str_buflen = str_len];
	}

char *ptr;

// (copy all characters)
for (ptr = str_buffer; count --;) {
	unsigned charval = parse_char (type16, text, limit);
	*ptr ++ = charval;
	if (type16) *ptr ++ = charval >> 8;
	}

*ptr = '\0';		// Used no more (just in case...)
return str_buffer;
}	// parse_string

extern "C" int yywrap () { return true; }

int yyparse ();

// Defined in: "kernel.cpp"

// Synchronize lines counter
void sync_lines (unsigned lines);

// Count # of newlines in `text`
void count_lines (char *text) {
char ch;
unsigned lines = 0;
while (ch = * text ++)
    if (ch == '\n') ++ lines;

sync_lines (lines);
}	/* count_lines */

/* Switch source of lexer data input to 'in'
   (returns previous source) */
FILE *select_source (FILE *in) {
if (in != yyin) {
	FILE *prev = yyin;
	yyin = in;
	return prev;
	}

return in;
}	// select_source

bool block_eval = false;

char *getc_buf_ptr;
unsigned getc_code_len;

#include <mem.h>

void yy_input (char *buf, int &result, int max_size) {
if (block_eval) {
	result = max_size < getc_code_len ? max_size : getc_code_len;
	memcpy (buf, getc_buf_ptr, result);
	getc_buf_ptr += result;
	getc_code_len -= result;
	}
else {
	// from flex skeleton...
	if (YY_CURRENT_BUFFER_LVALUE->yy_is_interactive)
		{
		int c, n;
		for (n = 0; n < max_size &&
			(c = getc( yyin )) != EOF && c != '\n'; ++ n)
			buf[n] = (char) c;
		if (c == '\n')
			buf[n++] = (char) c;
		if (c == EOF && ferror( yyin ))
			yy_fatal_error( "input in flex scanner failed" );
		result = n;
		}
	else if (((result = fread( buf, 1, max_size, yyin )) == 0)
		  && ferror( yyin ))
		yy_fatal_error( "input in flex scanner failed" );
	}
}	// yy_input

#define YY_INPUT(buf,result,max_size) yy_input(buf, result, max_size)

void save_yystate (YY_BUFFER_STATE &state, FILE *input) {
state = YY_CURRENT_BUFFER;
yy_switch_to_buffer (yy_create_buffer (input, YY_BUF_SIZE));
}	// save_yystate

void load_yystate (YY_BUFFER_STATE &state) {
yy_delete_buffer (YY_CURRENT_BUFFER);
yy_switch_to_buffer(state);
}	// load_yystate

int exec_code (char *code_start, unsigned code_len, bool is_expr) {
getc_buf_ptr = new ("temp-buffer") char [getc_code_len = code_len + (is_expr ? 2 : 0)];
char *base = getc_buf_ptr;

if (is_expr) *base ++ = '\x01';
while (code_len --) *base ++ = *code_start ++;
if (is_expr) *base ++ = '\x02';

base = getc_buf_ptr;

YY_BUFFER_STATE yy_state;

save_yystate (yy_state, stdout);

block_eval = true;

int result = yyparse ();

block_eval = false;

load_yystate (yy_state);

delete [] base;

return result;
}	// exec_code

// Execute source file 'file' (with 'flags')
// Returns value returned by parser
int exec_file (char *file, unsigned flags, unsigned x_flags, Expr *args) {
int result = -1;

FILE *in = file ? fopen (file, "rt") : stdin;
if (in != NULL) {
	FILE *prev = select_source (in);

	(new ("RootModule") RootModule (file, flags, x_flags, args))->prolog ();

	result = yyparse ();

	delete Module::epilog ();

	select_source (prev);
	fclose (in);
	}
else {
	// TODO: report error...
	syslog->put_cstr (file);
	syslog->put_cstr (": can't read file!\n");
	}

return result;
}	// exec_file

//
//	Inclusion module
//
struct IncludeModule : Module {
	YY_BUFFER_STATE yy_state;
	FILE *input;

	IncludeModule (char *modname, unsigned flags, unsigned x_flags, FILE *in) :
		Module (modname, flags, x_flags)
		{ input = in; }

	// (nesting depth)
	static unsigned depth;

	void open () {
		save_yystate (yy_state, input);

		depth ++;
		}	// open

	void close () {
		depth --;

		load_yystate (yy_state);

		fclose (input);
		}	// close

	// This is crucial!
	void release () { Module::release (); }
	};

unsigned IncludeModule::depth = 0;

bool include_file (char *file) {
FILE *in = fopen (file, "rt");

if (in != NULL) {
	(new ("Include") IncludeModule (file, 0, 0, in))->prolog ();
	return true;
	}

return false;
}	// include_file

static bool include_end () {
if (IncludeModule::depth) {
	delete Module::epilog ();
	return true;
	}

return false;
}	// include_end

%}

/* binary digit */
BDIGIT	[01]
/* octal digit */
ODIGIT	[0-7]
/* decimal digit */
DDIGIT	[0-9]
/* hex digit */
HDIGIT	[0-9A-Fa-f]

/* identifier head */

/* Latin only:
ID_HEAD	[A-Za-z_]
ID_TAIL [A-Za-z_0-9]
  */

ID_HEAD	[A-Za-z_]
ID_TAIL [A-Za-z_0-9]

%%

{ID_HEAD}{ID_TAIL}*	{
			/* Identifier */
			strcpyL (LV_ident, yytext, MaxIdLen);
			return L_IDENT;
			}

{DDIGIT}+	{
			/* Fixed decimal number literal */
			parse_decint (yytext, LV_long, yytext + yyleng);
			return L_FIXED;
			}

"\\b"{BDIGIT}+	{
			/* Fixed binary number literal */
			parse_binint (yytext + 2, LV_long, yytext + yyleng);
			return L_FIXED;
			}

"\\o"{ODIGIT}+	{
			/* Fixed octal number literal */
			parse_octint (yytext + 2, LV_long, yytext + yyleng);
			return L_FIXED;
			}

"\\x"{HDIGIT}+	{
			/* Fixed hex number literal */
			parse_hexint (yytext + 2, LV_long, yytext + yyleng);
			return L_FIXED;
			}

"\\h"{HDIGIT}+	{
			/* Fixed hex number literal */
			parse_hexint (yytext + 2, LV_long, yytext + yyleng);
			return L_FIXED;
			}

{DDIGIT}+("."{DDIGIT}+)?([Ee][+-]?{DDIGIT}+)?	{
			/* Float number literal */
			parse_float (yytext, LV_double);
			return L_FLOAT;
			}

"\\u"		{			/* Wide flag for string */
			LV_strtype = 1;
			// (continue...)
			}

"\\U"		{			/* Wide flag for string */
			LV_strtype = 1;
			// (continue...)
			}

"\\c"		{			/* Character flag for string */
			LV_strtype = -1;
			// (continue...)
			}

"'"[^']*"'"		{
			/* String literal */
			LV_string = parse_string (yytext + 1, yyleng - 2);
			count_lines (yytext);
			return L_STRING;
			}

"\""[^"]*"\""	{
			/* String literal */
			LV_string = parse_string (yytext + 1, yyleng - 2);
			count_lines (yytext);
			return L_STRING;
			}

"`"[^`]+"`"		{
			/* Comment */
			count_lines (yytext);
			}

[ \n\t\r]+		{
			/* Blank space */
			count_lines (yytext);
			}

	/*
	 *	operations & delimiters
	 */

"++"			return L_Inc;
"--"			return L_Dec;
"%%"			return L_RemI;

"<<"			return L_Shl;
">>"			return L_Shr;

"?<"			return L_Min;
"?>"			return L_Max;
"<?>"			return L_Cmp;

"<="			return L_Le;
"~>"			return L_Le;
">="			return L_Ge;
"~<"			return L_Ge;
"=="			return L_Eq;
"<>"			return L_Ne;
"~="			return L_Ne;

"&&"			return L_CAnd;
"||"			return L_COr;
"~~"			return L_CNot;

":="			return L_Let;

"<:"			return L_Put;
":>"			return L_Get;

"->"			return L_Arrow;

".."			return L_Range;

"??"			return L_Loop;

"!!"			return L_Qual;

"::"			return L_With;

"[+]"			return L_LCat;
"[*]"			return L_LRep;
"[~]"			return L_LRev;

"[<]"			return L_LHead;
"[>]"			return L_LTail;

"[<-]"			return L_LPush;
"[->]"			return L_LPop;

"[=]"			return L_LReduce;

"[==]"			return L_LEq;
"[<>]"			return L_LNe;

"\x01"			return block_eval ? L_EvalBeg : '\x01';

"\x02"			return block_eval ? L_EvalEnd : '\x02';

.			return *yytext;		/* Any other character */

<<EOF>>		{		/* End of input */
			if (! include_end ()) {
			// (release string buffer)
			if (str_buffer) {
				delete [] str_buffer;
				str_buffer = 0;
				str_buflen = 0;
				}
			yyterminate ();
			}
			}

%%

// (new yylex)
int yylex (SynTerm **lvalp, void *lcharp) {
return yylex ();
}	// yylex

