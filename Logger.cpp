
/*

	+---+---+---+---+---+---+
	|	"Logger.cpp":
	|	Logging (debugging & error output).
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Defs.h"

#include "NameTab.h"

#include "Logger.h"

unsigned step = 2;

// Put indent at line start
DL_EXPORT void Logger::log_indent () {
if (indent) {
	unsigned n = indent * step;
	while (n >= 8) { put_ch ('\t'); n -= 8; }
	while (n --) put_ch (' ');
	}
}	// Logger::log_indent

//
// Output names
//

void Name::log (Logger &log) {
log.put_cstr (ident);
}	// Name::log

void Name::log_ex (Logger &log) {
Name::log (log);
}	// Name::log_ex

void AnyVar::log_ex (Logger &log) {
Name::log (log);

Expr *expr = evalR ();
if (expr) {
	log.put_cstr (" = ");
	expr->log (log);
	}
}	// AnyVar::log_ex

void PrefixName::log_ex (Logger &log) {
prefix->log_ex (log);
}	// PrefixName::log_ex

//
// Output scopes
//

static Logger *t_log = 0;		// (temporary)

static bool putName (Name *name) {
t_log->log_indent ();
if (name) name->log_ex (*t_log);
t_log->put_ch (';');
t_log->put_nl ();

return true;
}	// putName

//
//	Table output mode:
//		< 0 ?	by ordinals descending
//		= 0 ?	unsorted
//		> 0 ?	by ordinals ascending
//

void Logger::log_nametab (NameTab *table, int order) {
put_nl ();
log_enter ();

Logger *s_log = t_log;
t_log = this;

if (order)
	table->forall_ordered (putName, order > 0);
else
	table->forall (putName);

t_log = s_log;

log_leave ();
put_nl ();
}	// Logger::log_nametab

void NameTab::log (Logger &log) {
log.log_nametab (this, 0);
}	// NameTab::log

//
// Name qualifiers
//

// TODO: corect recursion problem in anon functor names!!!

void Logger::log_qname (Name *name) {
if (name) {
Prefix *context = name->table ? name->table->get_owner() : 0;
if (context && context != local_scope) {
	if (context != global_scope)
		context->log (*this);
	put_ch ('.');
	}
name->log (*this);
}	// (name)
}	// Logger::log_qname

//
// Output prefixes
//

void Prefix::log (Logger &log) {
if (name) name->log (log);
else log_ex (log);			// (only if nameless)
}	// Prefix::log

DL_EXPORT void Prefix::log_ex (Logger &log) {
Prefix::log (log);
log.put_cstr (" {native}");
}	// Prefix::log_ex

void Logger::log_params (Param *param) {
put_ch ('(');
while (param) {
	if (param->flags) put_ch ('@');

	param->var->log (*this);

	if (param->init) {
		put_cstr (" = ");
		log_expr (param->init);
		}
	if (param = param->next) put_ch (' ');
	}
put_ch (')');
}	// Logger::log_params

void Logger::log_functor (P_Functor *func) {
put_cstr ("! ");

// (avoid recursion loop...)
if (func->name) log_prefix (func);

log_params (func->params);

put_cstr (" ["); log_fixed (func->local_no); put_ch (']');

put_cstr (" = ");

Prefix *saved_scope = local_scope;
local_scope = func;

log_expr (func->body);

local_scope = saved_scope;
}	// Logger::log_functor

void Logger::log_class (P_Class *classref) {
put_cstr ("!! ");

if (classref->super) {
	log_prefix (classref->super);
	put_cstr ("->");
	}

// (avoid recursion loop...)
if (classref->name) log_prefix (classref);

log_params (classref->params);

put_cstr (" {");
log_fixed (classref->member_no);
put_ch ('}');

Prefix *saved_scope = local_scope;
local_scope = classref;

log_enter ();

// (if class have constructor)
if (classref->ctor) {
	put_nl ();
	log_indent ();
	put_cstr ("= ");
	log_expr (classref->ctor);
	}

// (if class have destructor)
if (classref->ctor) {
	put_nl ();
	log_indent ();
	put_cstr ("~ ");
	log_expr (classref->dtor);
	}

// (if class originates virtuals)
if (classref->virtuals) {
	put_nl ();
	log_indent ();
	put_cstr ("# ");
	put_cstr ("[ ");
	for (P_Virtual *virtref = classref->virtuals; virtref; virtref = virtref->next) {
		virtref->log_ex (*this);
		put_ch (' ');
		}
	put_ch (']');
	}

// (if class has any virtuals)
if (classref->virtual_no)
	log_virtuals (classref);

log_leave ();

local_scope = saved_scope;
}	// Logger::log_class

// Output class virtuals table
void Logger::log_virtuals (P_Class *classref) {
unsigned count = classref->virtual_no;
P_Functor **table = classref->virtual_tab;

put_nl ();
log_indent ();

put_cstr ("# ");
put_ch ('{');
put_nl ();

log_enter ();

for (unsigned ordinal = 0; ordinal != count; ++ ordinal) {
	// (show only locally-redefined virtuals)
	if (classref->virtual_redefined (ordinal)) {
		log_indent ();
		classref->locate (ordinal)->log_ex (*this);
		if (table[ordinal]) {
			put_cstr (" => ");
			log_functor (table [ordinal]);
			}
		put_ch (';');
		put_nl ();
		}
	}

log_indent ();
put_ch ('}');
put_nl ();

log_leave ();
}	// Logger::log_virtuals

void P_Functor::log_ex (Logger &log) {
log.log_functor (this);
}	// P_Functor::log_ex

void P_Class::log_ex (Logger &log) {
log.log_class (this);
}	// P_Class::log_ex

void P_Virtual::log_ex (Logger &log) {
log.log_prefix (this);
if (classref) classref->log (log);
log.put_ch ('#')->log_fixed (ordinal);
}	// P_Virtual::log_ex

//
// Variables
//

void X_Variable::log (Logger &log) {
log.log_qname (name);
}	// X_Variable::log

//
// Numeric literals
//

static char diglist[] = "0123456789ABCDEF";

// Put hexadecimal integer
DL_EXPORT Logger *Logger::put_hex (unsigned digits, unsigned val) {
while (digits --)
	put_ch (diglist [(val >> 4*digits) & 0x0F]);

return this;
}	// Logger::put_hex

// Put octal integer
DL_EXPORT Logger *Logger::put_oct (unsigned digits, unsigned val) {
while (digits --)
	put_ch (diglist [(val >> 3*digits) & 0x07]);

return this;
}	// Logger::put_oct

// Put binary integer
DL_EXPORT Logger *Logger::put_bin (unsigned digits, unsigned val) {
while (digits --)
	put_ch (diglist [(val >> digits) & 0x01]);

return this;
}	// Logger::put_bin

// Put decimal integer
DL_EXPORT Logger *Logger::put_dec (unsigned digits, unsigned val) {
unsigned div = 1, count = digits;
while (count --) div *= 10;

while (digits --) {
	div /= 10;
	put_ch (diglist [(val / div) % 10]);
	}

return this;
}	// Logger::put_dec

//
// Characters / strings
//

static char char_code (unsigned char ch) {
switch (ch) {
	case '\a':	return 'a';
	case '\b':	return 'b';
	case '\f':	return 'f';
	case '\n':	return 'n';
	case '\r':	return 'r';
	case '\t':	return 't';
	case '\e':	return 'e';
	}

if (ch < 0x10)
	return (ch < 10 ? '0' : 'A') + ch;

return 0;
}	// char_code

// Log ASCII character
DL_EXPORT Logger *Logger::log_ch (unsigned char ch) {
if (ch < ' ' || ch >= 0x7f) {
	char cch = char_code (ch);
	put_ch ('\\');

	if (cch) put_ch (cch);
	else { put_ch ('x'); put_hex (2, ch); }
	}

else if (ch == '"')
	{ put_ch ('\\'); put_ch (ch); }
else put_ch (ch);

return this;
}	// Logger::log_ch

// Log Unicode character
DL_EXPORT Logger *Logger::log_wch (unsigned wch) {
if (wch < 0x100)
	log_ch (wch);
else if (wch < 0x10000) {
	put_ch ('\\'); put_ch ('X');
	put_hex (4, wch);
	}
else {
	put_ch ('\\'); put_ch ('X');
	put_hex (8, wch);
	}

return this;
}	// Logger::log_wch

Logger *Logger::log_sstring (S_string &s_src) {
unsigned l_src;
unsigned char t_src;
str_ptr p_src = s_src.fetch (l_src, t_src);

if (t_src) put_ch ('_');
if (t_src > 1) put_ch ('_');

put_ch ('"');
// TODO: wide wide string...

if (l_src) {
	if (t_src)
	if (t_src > 1) {
		unsigned *str = (unsigned *) p_src;
		while (l_src --) log_wch (*str ++);
		}
	else {
		unsigned short *str = (unsigned short *) p_src;
		while (l_src --) log_wch (*str ++);
		}
	else {
		char *str = (char *) p_src;
		while (l_src --) log_ch (*str ++);
		}
	}
put_ch ('"');

return this;
}	// Logger::log_sstring

DL_EXPORT void S_string::log (Logger &log) {
log.log_sstring (*this);
}	// S_string::log

//
//	Fixed range
//

void FixedRange::log (Logger &log) {
log.log_fixed (from)->put_cstr ("..")->log_fixed (to);
}	// FixedRange::log

//
//	Lists
//

Logger *Logger::log_list (X_List *list) {
put_ch ('(');

while (list) {
	Expr *next = list->next;

	log_expr (list->first);
	put_cstr (", ");

	if (! (list = next ? next->isList() : 0))
		if (next) log_expr (next);
	}

put_ch (')');

return this;
}	// Logger::log_list

void X_List::log (Logger &log) {
log.log_list (this);
}	// X_List::log

//
// Prefixes & terms
//

DL_EXPORT Logger *Logger::log_prefix (Prefix *pfx) {
if (pfx) {
	if (pfx->name) {
		log_qname (pfx->name);
		put_ch (':');
		}
	else pfx->log (*this);
	}

return this;
}	// Logger::log_prefix

Logger *Logger::log_term (X_Term *term) {
log_prefix (term->prefix);
log_expr (term->args);

return this;
}	// Logger::log_term

void X_Term::log (Logger &log) {
log.log_term (this);
}	// X_Term::log

//
// Blocks
//

Logger *Logger::log_block (X_Block *block) {
put_ch ('{');
put_nl ();

log_enter ();

for (X_Block::SNode *node = block->body; node; node = node->next) {
	log_indent ();
	log_expr (node->expr);
	put_ch (';');
	put_nl ();
	}

log_indent ();
if (block->last) { log_expr (block->last); put_ch (' '); }

log_leave ();
put_ch ('}');

return this;
}	// Logger::log_block

void X_Block::log (Logger &log) {
log.log_block (this);
}	// X_Block::log

//
// Objects
//

// Put 'object' belonging to 'class' (recursive)
Logger *Logger::log_object (P_Class *classref, X_Object *object) {
unsigned no = classref->member_no;
Expr **data = object->data;

P_Class *super = classref->super;
if (super) {
	log_object (super, object);
	put_cstr ("->");

	data += super->member_no;
	no -= super->member_no;
	}

classref->log (*this);
put_ch (':');
put_ch ('{');

while (no --) {
	log_expr (*data ++);
	if (no) put_cstr (", ");
	}

put_ch ('}');

return this;
}	// Logger::log_object

void X_Object::log (Logger &log) {
log.log_object (classref, this);
}	// X_Object::log

//
//	Externals
//

DL_EXPORT void External::log (Logger &log) {
log.log_external (this);
}	// External::log

Logger *Logger::log_external (External *external) {
if (external) external->log (*this);
return this;
}	// Logger::log_external

void X_Extern::log (Logger &log) {
log.put_ch ('{')->put_cstr (sign)->put_ch (':')->log_external (external)->put_ch ('}');
}	// X_Extern::log

//
// Output evaluated
//

static char const *type_name (VType type) {
switch (type) {
    case T_undef:		return "Undef";
	case T_fixed:		return "Fixed";
	case T_float:		return "Float";
	case T_string:		return "String";
	case T_list:		return "List";
	case T_term:		return "Term";
	case T_var:			return "Variable";
	case T_block:		return "Block";
	case T_prefix:		return "Functor";
	case T_object:		return "Object";
	case T_array:		return "Array";
	case T_hash:		return "Hash";
	case T_pattern:		return "Pattern";
	case T_ring:		return "Ring";
	case T_stream:		return "Stream";
	case T_scodec:		return "S_Codec";
	case T_extern:		return "External";
	case T_xcpt:		return "Exception";
	}	// switch

return "???";			// (must not happen)
}	// type_name

// Log type name
Logger *Logger::log_type_name (VType type) {
put_cstr (type_name (type));

return this;
}	// Logger::log_type_name

Logger *Logger::log_value (VType type, VDatum &val) {
switch (type) {
    case T_undef:
    	put_cstr ("()");			// undef
		break;

	case T_fixed:
		log_fixed (val._fixed);
		break;

	case T_float:
		log_float (val._float);
		break;

	case T_string:
		val._string.log (*this);
		break;

	case T_list:
		log_list (val._list);
		break;

	case T_term:
		log_term (val._term);
		break;

	case T_var:
		val._var->log (*this);
		break;

	case T_block:
		log_block (val._block);
		break;

	case T_prefix:
		log_prefix (val._pfx);
		break;

	case T_object:
		val._object->log (*this);
		break;

	case T_array:
		log_array (val._array);
		break;

	case T_hash:
		log_hash (val._hash);
		break;

	case T_pattern:
		log_pattern (val._pattern);
		break;

	case T_ring:
		log_ring (val._ring);
		break;

	case T_stream:
		log_stream (val._stream);
		break;

	case T_scodec:
		log_scodec (val._scodec);
		break;

	case T_xcpt:
		val._except->log (*this);
		break;

	case T_extern:
		val._extern->log (*this);
		break;
	}	// switch

return this;
}	// Logger::log_value

void Expr::log (Logger &log) {
VDatum value;
log.log_value (evalV (value, false), value);
}	// Expr::log

//
// Output expression
//

DL_EXPORT Logger *Logger::log_expr (Expr *expr) {
if (expr) expr->log (*this);
else put_cstr ("()");				// (undef)

return this;
}	// Logger::log_expr

//
// Ouput statements
//

void DefVars::log (Logger &log) {
log.put_ch ('['); log.put_ch (']');
}	// DefVars::log

void DefFunctor::log (Logger &log) {
// filter out nameless (virtual) definitions
if (func->name) log.log_functor (func);
}	// DefFunctor::log

void Logger::log_functor_group (GroupDefFunctor *func_def) {
put_ch ('{');
put_nl ();

log_enter ();

do {
	log_indent ();
	log_functor (func_def->func);
	put_ch (';');
	put_nl ();
	}
while (func_def = func_def->next);

log_leave ();
log_indent ();
put_ch ('}');
}	// Logger::log_functor_group

void GroupDefFunctor::log (Logger &log) {
log.log_functor_group (this);
}	// GroupDefFunctor::log

void DefClass::log (Logger &log) {
log.log_class (classref);
}	// DefClass::log

void Module::log (Logger &log) {
log.put_ch ('<');
if (name && name->ident) log.put_cstr (name->ident);
log.put_ch ('>');
}	// Module::log

// Show module line/statement ##
void Logger::log_location (Module &module) {
module.log (*this);

if (module.parent) {
	put_cstr (" <- ");
	module.parent->log (*this);
	}

put_ch (' ');
put_ch ('#');
log_fixed (module.lines + 1);
put_cstr (" / [");
log_fixed (module.stmts);
put_ch (']');
}	// Logger::log_location

// Start module
void Logger::log_prolog (Module &module, bool flag) {
if (module.flags & OF_tr_mod) {
log_indent ();
log_location (module);

put_cstr (" {");
put_nl ();

put_nl ();

log_enter ();
}

if (! module.parent)
if (flag) global_scope = local_scope = &module;
}	// Logger::log_prolog

// End module
void Logger::log_epilog (Module &module, bool flag) {
if (module.flags & OF_tr_mod) {

log_leave ();

put_nl ();

module.stmts --;

log_indent ();
put_cstr ("} ");
log_location (module);

if (module.errors) {
	put_cstr (" !");
	log_fixed (module.errors);
	}

if (module.warns) {
	put_cstr (" ?");
	log_fixed (module.warns);
	}

put_nl ();
}

if (! module.parent)
if (flag) global_scope = local_scope = module.parent;
}	// Logger::log_epilog

// Put prompt
void Logger::log_prompt (Module &module) {
module.stmts ++;

if (module.flags & OF_prompt) {
	put_ch ('['); log_fixed (module.stmts); put_cstr ("]: ");
	}
}	// Logger::log_prompt

// Put statement
void Logger::log_stmt (Module &module, Expr *expr, VType type, VDatum &val) {
unsigned flags = module.flags;

if (flags) {
if (! (flags & OF_tr_stmt)) log_indent ();

if (flags & OF_tr_expr) {
	log_expr (expr);
	if (flags & OF_tr_refc)
		{ put_cstr (": #"); log_fixed (expr->refs); }

	if (flags & OF_tr_vals) put_cstr (" => ");
	}

if (flags & OF_tr_vals) log_value (type, val);
if (! (flags & OF_tr_stmt)) { put_ch (';'); put_nl (); }
}	// flags
}	// Logger::log_stmt

//
//	Version reporting
//

void Logger::log_version (unsigned version) {
log_fixed (Version_Maj (version));
	put_ch ('.');
log_fixed (Version_Min (version));
	put_ch ('.');
log_fixed (Version_Pat (version));
}	// Logger::log_version

//
//	Error reporting
//

void Error::report (Logger &log, Module *module) {
switch (type) {
	case EC_error:
	case EC_syntax:
		log.put_ch ('!')->log_fixed (++ module->errors);
		break;

	case EC_warn:
		log.put_ch ('?')->log_fixed (++ module->warns);
		break;
	}	// switch (type)

log.put_ch (' ');
log.log_location (*module);
log.put_ch (':');
_report (log);
log.put_nl ();
}	// Error::report

void Error::_report (Logger &log) {
log.put_cstr ("Unknown error");
}	// Error::_report

void ExecError::_report (Logger &log) {
log.put_cstr ("Unknown execution error");
}	// ExecError::_report

void FatalError::_report (Logger &log) {
log.put_cstr ("Fatal error");
}	// FatalError::_report

//
//	Signal report
//

void SysX_Signal::log (Logger &log) {
log.put_cstr ("signal [")->log_fixed (signo)->put_cstr ("]");
}	// SysX_Signal::log

