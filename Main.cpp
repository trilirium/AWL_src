
/*

	+---+---+---+---+---+---+
	|	"Main.cpp":
	|	'main' lives here.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Defs.h"

#include "Logger.h"

#include <stdio.h>
#include <string.h>

#include "Eval.h"

#define HEADER

#include "E_Hash.cpp"
#include "E_Stream.cpp"

#undef HEADER


//
// Module level definitions...
//

InStream *sysin = 0;
OutStream *sysout = 0;
OutStream *syserr = 0;

// System initialisation
// (return scope counter)
unsigned system_init ();

// System termination
void system_term ();

//
//
//

// In "AWL.L" :
// Execute source file 'file' (with 'flags')
// Returns value returned by parser
int exec_file (char *file, unsigned flags, unsigned x_flags, Expr *args);

//
// AWL version
//

//^V 0.8.0

unsigned main_version = Version (0, 8, 0);

//
// Build date
//

char main_date [] = __DATE__;

//
//	System log
//

DL_EXPORT Logger *syslog;

// TODO: never output banner to syslog...
void Logger::log_banner (unsigned functor_count) {
	put_cstr ("AWL interpreter\n(version ");
	log_version (main_version);
	put_cstr (" from ");
	put_cstr (main_date);
	put_cstr ("; ");
	log_fixed (functor_count);
	put_cstr (" builtins).\n");
}	// Logger::log_banner

//
//	Arguments list processing
//

// Make string argument
static Expr *argv_string (char *str) {
if (str)
	return new ("String/argv") X_String (str);

else return 0;
}	// argv_string

//
//	TODO: modify argument passing mode
//

// Add to list tail
void add_tail (Expr * &expr, Expr *node) {
if (expr) {
	X_List *list = expr->isList ();
	if (list)
		add_tail (list->next, node);
	else
		expr = new ("List/argv") X_List (expr, node);
	}

else expr = node;
}	// add_tail

static Expr *argv_get_list (char ** &_argp, unsigned depth) {
Expr *result = 0;

if (* _argp && ** _argp == '(') {
	_argp ++;

	while (* _argp && ** _argp != ')') {
		Expr *arg = argv_get_list (_argp, depth + 1);

		if (!arg) arg = argv_string (* _argp ++);
		add_tail (result, arg);
		}

	if (* _argp) _argp ++;
	}

return result;
}	// argv_get_list

Expr *parse_list (char ** &_argp);

Expr *parse_list_tail (char ** &_argp, char term) {
if (* _argp) {
	if (** _argp == term)
		{ _argp ++; return 0; }

	Expr *_head = parse_list (_argp);
	if (! _head) _head = argv_string (* _argp ++);

	Expr *_tail = parse_list_tail (_argp, term);
	return _tail ?
		new ("List/argv") X_List (_head, _tail) : _head;
	}

return 0;
}	// parse_list_tail

Expr *parse_list (char ** &_argp) {
if (* _argp && ** _argp == '(') {
	_argp ++;
	return parse_list_tail (_argp, ')');
	}

return 0;
}	// parse_list

// Make simple list w/o parsing
Expr *build_list (char ** argv) {
if (* argv)
	return new ("List/argv") X_List (argv_string (*argv), build_list (argv + 1));

return 0;		// (end of list)
}	// build_list

X_Hash *environment = 0;

void create_environment (char **envp) {

link_expr (environment = new ("Hash/env") X_Hash (0, FixedRange (0, 0)));

while (*envp) {
	char *name = *envp ++;
	char *value = strchr (name, '=');

	if (value) {
		*value = '\0';
		if (value[1] != '\0')	
			mutateR_X (environment->elem (argv_string (name), true), argv_string (value + 1));
		}
	}
}	// create_environment

void release_environment () {
unlink_expr (environment);
}	// release_environment

//
//	Memory manager interface
//

// Dump block information in datail...
void detail_dump (Logger &log, char *ptr, unsigned len) {
bool echo_mode = false;		// ? character : hexcode

while (len --) {
	unsigned char ch = *ptr ++;
	if (' ' <= ch && ch < 127) {
		// (is printable: to char mode)
		if (! echo_mode) { log.put_ch ('"'); echo_mode = true; }
		log.put_ch (ch);
		}
	else {
		// (is not printable: to hex mode)
		if (echo_mode) { log.put_ch ('"'); echo_mode = false; }
		log.put_ch ('<')->put_hex (2, ch)->put_ch ('>');
		}
	}	// while (len)

if (echo_mode) { log.put_ch ('"'); }
}	// detail_dump

static void dump_blk (char const *tag, unsigned size, void *ptr) {
syslog->put_cstr ("\t: ")->put_cstr (tag ? tag : "")->put_cstr (" [")->log_fixed (size)->put_ch (']');

if (1) {
	syslog->put_ch ('\t');
	detail_dump (*syslog, (char *) ptr, size);
	}

syslog->put_nl ();
}	// dump_blk

// Memory manager error
void MM_error (void *addr, char const *where, char const *message) {
syslog->put_cstr ("MM_error [")->put_cstr (where)->put_cstr ("] ")->put_hex (8, (unsigned) addr)->
	put_ch (' ')->put_cstr (message)->put_ch ('!')->put_nl ();
}	// MM_error

static unsigned p_balance = 0;

void meminfo_start () {
unsigned summary;
unsigned peak_count, peak_size;

MM_getstat (summary, p_balance, peak_count, peak_size);
}	// meminfo_start

void meminfo_finish (bool verbose) {
unsigned balance, summary;
unsigned peak_count, peak_size;

MM_getstat (summary, balance, peak_count, peak_size);

if (balance != p_balance) {
	unsigned remain_count, remain_size;

	MM_forall (dump_blk, remain_count, remain_size);

	syslog->put_cstr ("\aWarning: memory leak ")->
		log_fixed (balance)->put_cstr(" / ")->log_fixed (p_balance)->put_nl ();
	syslog->put_cstr ("Remaining: ")->
		log_fixed (remain_count)->put_ch (':')->
		put_ch ('[')->log_fixed (remain_size)->put_ch (']')->put_nl ();
	}

if (verbose)
syslog->put_cstr ("Memory usage: ")->
	put_cstr ("total = ")->log_fixed (summary)->
	put_cstr (" / peak = ")->log_fixed (peak_count)->
	put_ch ('[')->log_fixed (peak_size)->put_ch (']')->
	put_nl ();
}	// meminfo_finish

void procinfo_start () {
cpu_start ();
}	// procinfo_start

void procinfo_finish (bool verbose) {
double t_total, t_used;
cpu_finish (t_total, t_used);

if (verbose)
syslog->put_cstr ("Runtime: ")->
	put_cstr ("active: ")->log_float (t_used)->put_cstr (" sec(s). / ")->
	put_cstr ("total: ")->log_float (t_total)->put_cstr (" sec(s).")->
	put_nl ();
}	// procinfo_finish

void finalize () {}

//
//	Output logger
//

struct OutLog : Logger {
	OutStream *out;				// log destination

	OutLog (OutStream *out)
		{ this->out = out; }

	Logger *put_ch (char ch) { out->put_octet (ch); return this; }

	// Put C-string (null-terminated)
	Logger *put_cstr (const char *str) { out->put_cstr (str); return this; }

	// Log integer
	Logger *log_fixed (S_fixed val) { out->put_fixed (val); return this; }

	// Log float
	Logger *log_float (S_float val) { out->put_float (val); return this; }

	};	// OutLog

void init_sighandlers ();

int main (int argc, char **argv, char **envp) {
bool int_mode = false;

link_expr (sysin = new ("[System Input]") InStream (0, "SysIn"));
link_expr (sysout = new ("[System Output]") OutStream (1, "SysOut"));
link_expr (syserr = new ("[System Error]") OutStream (2, "SysErr"));

syslog = new ("[System Logger]") OutLog (sysout);

procinfo_start ();
meminfo_start ();

create_environment (envp);

int result = 0;

init_sighandlers ();

unsigned counter = system_init ();

if (argc > 1) {
	++ argv;

	if (strcmp (*argv, "=") == 0) {		// (Multi-module execution mode)
		++ argv;
		while (* argv) {
			char *file = *argv ++;
			result += exec_file (file, 0, 0, parse_list (argv));
			}
		}
	else {			// (Single-module execution mode)
		char *file = *argv ++;
		result = exec_file (file, 0, 0, build_list (argv));
		}
	}

else {
	// (Interactive mode)
	syslog->log_banner (counter);
	syslog->put_cstr ("(Interactive mode: ^Z to quit session.)\n\n");
	int_mode = true;

	result = exec_file (0, OF_tr_expr|OF_tr_vals|OF_prompt, XF_dialog, 0);
	}

system_term ();

release_environment ();

meminfo_finish (int_mode);
procinfo_finish (int_mode);

delete syslog;

unlink_expr (syserr);
unlink_expr (sysout);
unlink_expr (sysin);

finalize ();

return result;
}	// main

