
//
//	'ExpImp.cpp':
//	export/import code and data
//

#ifndef HEADER

#include "Eval.h"

#define HEADER

#include "E_Stream.cpp"

#undef HEADER

#endif

//
// Exporter declaration
//
struct Exporter {

	struct OutStream *out;			// Output stream to export to
	struct RootModule *module;		// Module to export

	// Exporter stream constructor
	Exporter (OutStream *out) { this->out = out; }

	// Exporter file constructor
	Exporter (char *filename);

	// Exporter destructor
	~Exporter ();

	// Is exporter valid?
	bool valid ();

	// Write single character
	void put_ch (char ch);
	// Write block: data [size]
	void write (void *data, unsigned size);

	//
	//	Export methods
	//

	// Export tag byte
	void ex_tag (unsigned char tag);

	// Export fixed number
	void ex_fixval (S_fixed value);
	// Export packed fixed value
	void ex_fixpak (S_fixed value);

	// Export C-string
	void ex_cstring (char const *string);

	// Export fixed scalar value
	void ex_fixed (S_fixed value);

	// Export float number
	void ex_float (S_float value);

	// Export string slice
	void ex_sstring (S_string &s_src);

	// Export list
	void ex_list (X_List *list);

	// Export parameters list
	void ex_params (Param *param);

	// Export identifier
	void ex_ident (char ident[]);

	// Export scope
	void ex_scope (bool phase, NameTab *scope);

	// Export scope declarations
	void ex_scope_decls (NameTab *scope);

	// Export scope definitions
	void ex_scope_defs (NameTab *scope);

	// Exporting lambda
	void export_lambda (Prefix *prefix);

	// Export expression
	void ex_expr (Expr *expr);

	// Export functor reference
	void ex_func_ref (Prefix *pfx);

	// Export module
	void ex_module (RootModule *module);
	};

//
// Variable links table for scope
//
struct VarLinker {
	unsigned count;				// variable counter
	AnyVar **table;				// variable references [count]
	Prefix *owner;				// owned by...

	// constructor
	VarLinker (unsigned count, Prefix *owner);

	// destructor
	~VarLinker ();

	// look up variable in table
	AnyVar *access_var (unsigned ordinal);
	};

//
// Functor links table for scope
//
struct FuncLinker {
	unsigned count;				// functor counter
	Prefix **table;				// functor references [count]
	VarLinker **func_vars;		// functor variables tables [count]
	FuncLinker **func_funcs;	// functor functors tables [count]
	Prefix *owner;				// owned by...

	// constructor
	FuncLinker (unsigned count, Prefix *owner);

	// destructor
	~FuncLinker ();

	// look up functor in table
	Prefix *access_func (unsigned ordinal);
	// look up variables subtable
	VarLinker *access_var_table (unsigned ordinal);
	// look up functors subtable
	FuncLinker *access_func_table (unsigned ordinal);
	};

//
// Importer declaration
//

struct Importer {

	InStream *in;			// Input stream to import from
	RootModule *module;		// Module to import

	// Importer file constructor
	Importer (char *filename);

	// Importer destructor
	~Importer ();

	// Is importer valid?
	bool valid ();

	// Read single character
	char get_ch ();
	// Read block: data [size]
	void read (void *data, unsigned size);

	// Import tag byte
	unsigned char im_tag ();

	// Import fixed value
	S_fixed im_fixval ();
	// Import packed fixed value
	S_fixed im_fixpak ();
	// Import float value
	S_float im_float ();

	// Import identifier
	void im_ident (char ident[]);

	// Expect C-string 'string'
	bool xp_cstring (char const *string);

	// Import variable name
	AnyVar *im_var_ref ();

	// Import variable table reference
	VarLinker *im_var_table ();

	// Import variable declarations
	void im_var_decls (bool local, VarLinker * &var_table, Prefix *owner);

	// Import variable definitions for declarations
	void im_var_defs (VarLinker *var_table);

	VarLinker *module_vars;

	FuncLinker *module_funcs;

	// Import prefix
	Prefix *im_func_ref ();

	// Import functor table reference
	FuncLinker *im_func_table (unsigned char tag);

	// Import functor declarations
	// (to 'func_table')
	void im_func_decls (FuncLinker * &func_table, Prefix *owner);

	// Import functor definitions for declarations
	void im_func_defs (FuncLinker *func_table);

	// Import prefix declaration
	Prefix *impo_pfx_decl (char *ident, VarLinker * &var_table, FuncLinker * &func_table);

	// Import functor declaration
	Prefix *im_decl_functor (char *ident, VarLinker * &var_table, FuncLinker * &func_table);

	// Import class declaration
	Prefix *im_decl_class (char *ident, Prefix *super,
		VarLinker * &var_table, FuncLinker * &func_table);

	// Import functor lambda definition
	Prefix *import_lambda ();

	// Check ordinal
	void xp_ordinal (unsigned expected);

	// Import parameters list
	Param *im_params (VarLinker *var_table, unsigned offset);

	// Import string value
	X_String *im_string (unsigned char t_src, unsigned len);

	// Import string slice
	void im_sstring (S_string &s_src);

	// Import list
	Expr *im_list (unsigned count);

	// Import expression
	Expr *im_expr ();

	// Import module
	void im_module (RootModule *module);
	};


#ifndef HEADER

#include "NameTab.h"

#include "Logger.h"

#include <string.h>

#include <fcntl.h>

//
//	Constructors/Destructors
//

//	Exporter

Exporter::Exporter (char *filename) {
out = new ("Exporter/Output") OutStream (-1, filename);
// TTT out->file_open (filename);
}	// Exporter::Exporter

Exporter::~Exporter () {
out->close ();
delete out;
}	// Exporter::~Exporter

bool Exporter::valid () {
// TTT return out->fd >= 0;
}	// Exporter::valid

//	Importer

Importer::Importer (char *filename) {
// TTT in = new ("Importer/Input") InStream (-1, filename);
// TTT in->file_open (filename);

module_vars = 0;
module_funcs = 0;
}	// Importer::Importer

Importer::~Importer () {
in->close ();
delete in;
}	// Importer::~Importer

bool Importer::valid () {
// TTT return in->fd >= 0;
}	// Importer::valid

// Report position (in importer)
void report_pos (Importer &importer) {
syslog->put_ch ('[')->put_hex (8, importer.in->tell ())->put_ch (']');
}	// report_pos

// Report position (in exporter)
void report_pos (Exporter &exporter) {
syslog->put_ch ('[')->put_hex (8, exporter.out->tell ())->put_ch (']');
}	// report_pos

//
//	Low level export/import funcs
//

void Exporter::put_ch (char ch) {
out->put_octet (ch);
}	// Exporter::put_ch

void Exporter::write (void *data, unsigned size) {
out->put_data ((char *) data, size);
}	// Exporter::write

// Export C-string
void Exporter::ex_cstring (char const *string) {
out->put_data (string, strlen(string) + 1);
}	// Exporter::ex_cstring

char Importer::get_ch () {
char ch;
in->get_octet (ch);
return ch;
}	// Importer::get_ch

void Importer::read (void *data, unsigned size) {
in->get_data ((char *) data, size);
}	// Importer::read

// Expect C-string 'string'
bool Importer::xp_cstring (char const *string) {
unsigned length = strlen(string) + 1;
char test [8];

while (length) {
	unsigned l_test = length < sizeof(test) ? length : sizeof(test);
	read (test, l_test);

	if (memcmp (string, test, l_test))
		return false;

	string += l_test;
	length -= l_test;
	}

return true;
}	// Importer::xp_cstring

//
//	Warnings
//

static void ex_warn (char const *message) {
syserr->put_cstr (message);
}	// ex_warn

//
//
//	Import/export primitives
//
//

// Export fixed value
void Exporter::ex_fixval (S_fixed val) {
write (&val, sizeof (val));
}	// Exporter::ex_fixval

// Import fixed value
S_fixed Importer::im_fixval () {
S_fixed val;
read (&val, sizeof (val));
return val;
}	// Importer::im_fixval

// Export packed fixed value
void Exporter::ex_fixpak (S_fixed val) {
do {
	unsigned char byte = val & 0x7f;
	val = (val >> 7) & 0x1ffffff;
	put_ch (val? (byte | 0x80) : byte);
	} while (val);
}	// Exporter::ex_fixpak

// Import packed fixed value
S_fixed Importer::im_fixpak () {
S_fixed val = 0;
unsigned char byte;
unsigned shift = 0;
for (;;) {
	byte = get_ch ();
	val |= (S_fixed) (byte & 0x7f) << shift;
	if (! (byte & 0x80) || (shift > 31)) break;
	shift += 7;
	}	// for (;;)
return val;
}	// Importer::im_fixpak

// Export float value
void Exporter::ex_float (S_float val) {
write (&val, sizeof (val));
}	// Exporter::ex_float

// Import float value
S_float Importer::im_float () {
S_float val;
read (&val, sizeof (val));
return val;
}	// Importer::im_float

// Export byte tag
void Exporter::ex_tag (unsigned char tag) {
put_ch (tag);
}	// Exporter::ex_tag

// Import byte tag
unsigned char Importer::im_tag () {
return get_ch ();
}	// Importer::im_tag

enum { MaxId = 127 };
typedef char t_ident [MaxId + 1];

// Export identifier
void Exporter::ex_ident (t_ident ident) {
if (ident) {
unsigned len = strlen (ident);
if (len <= MaxId)
	write (ident, len + 1);
else {
	write (ident, MaxId);
	put_ch ('\0');
	}
}
else put_ch ('\0');
}	// Exporter::ex_ident

// Import identifier
void Importer::im_ident (t_ident ident) {
char *ptr = ident, ch;
while ((ch = get_ch ()) && ptr != ident + MaxId)
	*ptr ++ = ch;
*ptr = '\0';
}	// Importer::im_ident

//
// Export names / scopes
//

static Exporter *t_expo = 0;
static bool t_phase = false;

static bool ex_Name (Name *name) {
name->expo (*t_expo, t_phase);
return true;
}	// ex_Name

void Exporter::ex_scope (bool phase, NameTab *scope) {
Exporter *s_expo = t_expo;
bool s_phase = t_phase;

t_expo = this;
t_phase = phase;

scope->forall_ordered (ex_Name, false);

t_expo = s_expo;
t_phase = s_phase;
}	// Exporter::ex_scope

void Exporter::ex_scope_decls (NameTab *scope) {
ex_fixpak (scope->counter);
ex_scope (true, scope);
}	// Exporter::ex_scope_decls

void Exporter::ex_scope_defs (NameTab *scope) {
ex_scope (false, scope);
}	// Exporter::ex_scope_defs

// Check ordinal
void Importer::xp_ordinal (unsigned expected) {
unsigned found = im_fixpak ();
if (found != expected) {
	syslog->put_cstr ("\aOrdinal mismatch: ")->
		log_fixed (found)->put_cstr (" <> ")->log_fixed (expected)->put_cstr ("!\n");
	}
}	// Importer::xp_ordinal

// Report wrong tag
static void wrong_tag (char const *context, unsigned char tag) {
syslog->put_cstr ("\aIllegal tag: ")->
	put_hex (2, tag)->put_cstr (" (at ")->put_cstr (context)->put_cstr (")!\n");
}	// wrong_tag

//
//
//	Export expressions (TX)
//
//

enum TX_enum {
	TX_Undef,		// undefined

	// (integers)
	TX_Fixed,		// unpacked fixed
	TX_FixNul,		// zero
	TX_FixPos,		// packed positive fixed
	TX_FixNeg,		// packed negative fixed

	TX_Float,		// float

	// (strings)
	TX_StringN,		// nil string
	TX_CharA,		// ASCII character
	TX_StringA,		// ASCII string
	TX_CharW,		// Unicode character
	TX_StringW,		// Unicode string

	// (lists)
	TX_List2,		// 2 elements list
	TX_ListN,		// N elements list

	TX_Var,			// variable reference
	TX_Term,		// term
	TX_Block,		// block
	TX_Prefix,		// functor reference

	TX_Object,		// class instance

	TX_DefFunc,		// functor definition
	TX_DefClass,	// class definition
	};

void Expr::expo (Exporter &exporter) {
ex_warn ("Exporting: unknown Expr");
}	// Expr::expo

//
//	Tag mapping by constructors...
//

// Export fixed scalar value
void Exporter::ex_fixed (S_fixed value) {
if (value > 0) {
	if (value < (1 << 21)) {
		ex_tag (TX_FixPos);
		ex_fixpak (value);
		return;
		}

	// (fall through otherwise...)
	}	// (value > 0)
else if (value < 0) {
	if (value > -(1 << 21)) {
		ex_tag (TX_FixNeg);
		ex_fixpak (-value);
		return;
		}

	// (fall through otherwise...)
	}	// (value < 0)
else {
	ex_tag (TX_FixNul);
	return;
	}

// default integer type
ex_tag (TX_Fixed);
ex_fixval (value);
}	// Exporter::ex_fixed

// Export fixed scalar
void X_Fixed::expo (Exporter &exporter) {
exporter.ex_fixed (value);
}	// X_Fixed::expo

// Export float scalar
void X_Float::expo (Exporter &exporter) {
exporter.ex_tag (TX_Float);
exporter.ex_float (value);
}	// X_Float::expo

// Export string slice
void Exporter::ex_sstring (S_string &s_src) {
unsigned char t_src;
unsigned l_src;

str_ptr p_src = s_src.fetch (l_src, t_src);

switch (l_src) {
	case 0:			// (empty string)
		ex_tag (TX_StringN);
		return;

	case 1:			// (one character)
		ex_tag (t_src ? TX_CharW : TX_CharA);
		break;

	default:		// (several characters)
		ex_tag (t_src ? TX_StringW : TX_StringA);
		ex_fixpak (l_src);
		break;
	}

// export string content
write (p_src, l_src << t_src);
}	// Exporter::ex_sstring

// Export string value
void X_String::expo (Exporter &exporter) {
S_string s_str;
s_str.s_set (this);
exporter.ex_sstring (s_str);
}	// X_String::expo

// Import string value
X_String *Importer::im_string (unsigned char t_src, unsigned len) {
S_string s_src;
read (s_src.alloc ("String/im", len, t_src), len << t_src);
return s_src.cons ();
}	// Importer::im_string

// Calculate export list length: minus 2 items
// (assuming 'list != 0')
static unsigned list_exlen (X_List *list) {
Expr *next;
unsigned len = 0;

while (list = (next = list->next)? next->isList () : 0)
	len ++;

return len;
}	// list_exlen

// Export list
// (assuming 'list != 0')
void Exporter::ex_list (X_List *list) {
unsigned len = list_exlen (list);

if (len) {
	// (N-elems list:)
	ex_tag (TX_ListN);
	ex_fixpak (len);

	Expr *next;
	for (;;) {
		ex_expr (list->first);
		next = list->next;

		if (! len --) break;

		list = next->isList ();
		}	// for (;;)

	ex_expr (next);
	}

else {
	// (Two-elems list:)
	ex_tag (TX_List2);
	ex_expr (list->first);
	ex_expr (list->next);
	}
}	// Exporter::ex_list

void X_List::expo (Exporter &exporter) {
exporter.ex_list (this);
}	// X_List::expo

// Import list
Expr *Importer::im_list (unsigned count) {
Expr *first = im_expr ();
return count ?
	new ("List/im") X_List (first, im_list (count - 1)) :
	first;
}	// Importer::im_list

// Export term
void X_Term::expo (Exporter &exporter) {
exporter.ex_tag (TX_Term);
exporter.ex_func_ref (prefix);
exporter.ex_expr (args);
}	// X_Term::expo

// Export variable
void X_Variable::expo (Exporter &exporter) {
exporter.ex_tag (TX_Var);
name->ex_var_ref (exporter);
}	// X_Variable::expo

// Export block
// (attention: reverse order!)
static unsigned export_block_chain (Exporter &exporter, X_Block::SNode *node) {
if (node) {
	unsigned count = export_block_chain (exporter, node->next);
	exporter.ex_expr (node->expr);
	return count + 1;
	}

return 0;
}	// export_block_chain

void X_Block::expo (Exporter &exporter) {
exporter.ex_tag (TX_Block);
exporter.ex_expr (last);

unsigned count = 0;
for (X_Block::SNode *node = body; node; node = node->next)
	++ count;

exporter.ex_fixpak (count);
export_block_chain (exporter, body);
}	// X_Block::expo

// Import block
static X_Block *import_block (Importer &importer) {
X_Block *block = new ("Block/im") X_Block (importer.im_expr ());

unsigned count = importer.im_fixpak ();
X_Block::SNode *nodes = 0;

while (count --)
	nodes = new ("Block/im") X_Block::SNode (importer.im_expr (), nodes);

block->body = nodes;
return block;
}	// import_block

// Export prefix
void X_Prefix::expo (Exporter &exporter) {
exporter.ex_tag (TX_Prefix);
exporter.ex_func_ref (prefix);
}	// X_Prefix::expo

// Export object
void X_Object::expo (Exporter &exporter) {
exporter.ex_tag (TX_Object);
exporter.ex_func_ref (classref);

unsigned count = classref->member_no;
Expr **ptr = data;
while (count --) exporter.ex_expr (*ptr ++);
}	// X_Object::expo

// Export any expression
void Exporter::ex_expr (Expr *expr) {
if (expr) expr->expo (*this);
else ex_tag (TX_Undef);
}	// Exporter::ex_expr

// Import any expression
Expr *Importer::im_expr () {
unsigned char tag;

switch (tag = im_tag ()) {
	case TX_Undef:
		return 0;

	// Numbers

	case TX_FixNul:
		return new ("Fixed/im") X_Fixed (0);

	case TX_FixPos:
		return new ("Fixed/im") X_Fixed (im_fixpak ());

	case TX_FixNeg:
		return new ("Fixed/im") X_Fixed (- im_fixpak ());

	case TX_Fixed:
		return new ("Fixed/im") X_Fixed (im_fixval ());

	case TX_Float:
		return new ("Float/im") X_Float (im_float ());

	// Strings

	case TX_StringN:
		return im_string (false, 0);

	case TX_CharA:
	case TX_CharW:
		return im_string (tag == TX_CharW, 1);

	case TX_StringA:
	case TX_StringW:
		return im_string (tag == TX_StringW, im_fixpak ());

	// Lists

	case TX_List2:
		return im_list (1);

	case TX_ListN:
		return im_list (im_fixpak () + 1);

	// Variables and Terms

	case TX_Var:
		return new ("Var/im") X_Variable (im_var_ref ());

	case TX_Term: {
		Prefix *prefix = im_func_ref ();
		return new ("Term/im") X_Term (prefix, im_expr ());
		}

	// Prefixes

	case TX_Prefix:
		return new ("Prefix/im") X_Prefix (im_func_ref ());

	// Blocks

	case TX_Block:
		return import_block (*this);

	// Objects

	case TX_Object: {
		P_Class *classref = Cast (P_Class, im_func_ref ());
		X_Object *object = new ("Object/im") X_Object (classref);

		unsigned count = classref->member_no;
		Expr **ptr = object->data;
		while (count --) *ptr ++ = link_expr (im_expr ());

		return object;
		}

	// Definition of functor
	case TX_DefFunc:
		return new ("DefFunc/im") DefFunctor (Cast (P_Functor, im_func_ref ()));

	// Definition of class
	case TX_DefClass:
		return new ("DefClass/im") DefClass (Cast (P_Class, im_func_ref ()));

	// TODO: GroupDefFunc

	default:
		wrong_tag ("expr", tag);
		return 0;
	}

return 0;
}	// Importer::im_expr

//
//	Export/import: names
//

void Name::expo (Exporter &exporter, bool pre) {
if (pre) exporter.ex_ident (ident);
else ex_warn ("Exporting: unknown Name");
}	// Name::expo

void AnyVar::expo (Exporter &exporter, bool pre) {
if (pre) Name::expo (exporter, pre);
}	// AnyVar::expo

void ModuleVar::expo (Exporter &exporter, bool pre) {
if (pre) Name::expo (exporter, true);
else exporter.ex_expr (expr);
}	// ModuleVar::expo

void PrefixName::expo (Exporter &exporter, bool pre) {
if (pre) {
	Name::expo (exporter, true);
	prefix->expo_decls (exporter);
	}
else {
	exporter.ex_fixpak (ordinal);
	prefix->expo_defs (exporter);
	}
}	// PrefixName::expo

//
// Scope access tags
//
enum TS_enum {
	TS_None = 0,		// (no valid scope)
	TS_Prim = 1,		// (primaries)
	TS_Mod = 2,			// (modular scope)
	TS_Func = 3,		// (user functor/class scope)
	TS_Lambda = 4,		// (functor lambda: definition follows)
	TS_Self = 5,		// (lambda reference)

	TS_Base = 8			// (opcodes relative to base)
	};

// Report bad ordinal error
static void ordinal_error (unsigned ordinal, unsigned total, char const *where, Prefix *prefix) {
syslog->put_cstr ("Ordinal error: at ")->put_cstr (where)->put_cstr (": ")->
	log_fixed (ordinal)->put_cstr (" > ")->log_fixed (total)->put_cstr ("!\n");
}	// ordinal_error

// Current lambda definition
// (when importing / exporting)
static Prefix *current_lambda = 0;

// Export reference to name
bool Name::ex_ref (Exporter &exporter) {
Prefix *owner = table->get_owner();

if (owner) {
	if (owner == exporter.module)
		// (module global)
		exporter.ex_tag (TS_Mod);
	else {
		// (functor/class name)
		if (owner == current_lambda)
			exporter.ex_tag (TS_Self);
		else {
			exporter.ex_tag (TS_Func);
			exporter.ex_func_ref (owner);
			}
		}

	exporter.ex_fixpak (ordinal);
	return true;
	}

// otherwise: normal export failed!
return false;
}	// Name::ex_ref

//
//	Export/import: variables
//

FuncLinker *lambda_func_link = 0;

// Export variable name reference
void AnyVar::ex_var_ref (Exporter &exporter) {
Name::ex_ref (exporter);
}	// AnyVar::ex_var_ref

// Look up variable in table
AnyVar *VarLinker::access_var (unsigned ordinal) {
if (ordinal < count) return table[ordinal];

ordinal_error (ordinal, count, "VarScope: Var", owner);
return 0;
}	// VarLinker::access_var

// Import variable table reference
VarLinker *Importer::im_var_table () {
unsigned char tag = im_tag ();

switch (tag) {
	case TS_Mod:
		return module_vars;

	case TS_Func:
		{
		FuncLinker *func_table = im_func_table (TS_None);
		return func_table->access_var_table (im_fixpak ());
		}

	case TS_Self:
		return lambda_func_link ? lambda_func_link->access_var_table (0) : 0;

	default:
		wrong_tag ("var_table", tag);
		return 0;
	}	// switch (tag)
}	// Importer::im_var_table

// Import variable name reference
AnyVar *Importer::im_var_ref () {
VarLinker *var_table = im_var_table ();
return var_table ? var_table->access_var (im_fixpak ()) : 0;
}	// Importer::im_var_ref

// Constructor for VarLinker
VarLinker::VarLinker (unsigned count, Prefix *owner) {
table = new ("Var/Links") AnyVar * [this->count = count];
while (count --) table[count] = 0;

this->owner = owner;
}	// Importer::VarLinker::VarLinker

// Import variable declarations
void Importer::im_var_decls (bool is_local, VarLinker * &var_table, Prefix *owner) {
unsigned var_count = im_fixpak ();
var_table = new ("Importer/VarTab") VarLinker (var_count, owner);

AnyVar **var_ptr = var_table->table;
while (var_count --) {
	t_ident ident;
	im_ident (ident);
	link_name (*var_ptr ++ = AnyVar::install (ident, is_local));
	}
}	// Importer::im_var_decls

// Import variable definitions
void Importer::im_var_defs (VarLinker *var_table) {
unsigned var_count = var_table->count;
AnyVar **var_ptr = var_table->table;

while (var_count --)
	mutateR_X ((*var_ptr ++)->evalR (), im_expr ());
}	// Importer::im_var_defs

// Destructor for Importer::VarLinker
VarLinker::~VarLinker () {
unsigned var_count = count;
AnyVar **var_ptr = table;

while (var_count --)
	unlink_name (*var_ptr ++);

delete [] table;
}	// VarLinker::~VarLinker

//
//	Export/import: functor references
//

// Find prefix in scope by ordinal
Prefix *find_by_ordinal (unsigned ordinal);

// Look for Prefix in opcodes table
unsigned opcode_lookup (Prefix *pfx);

// Look up functor in table
Prefix *FuncLinker::access_func (unsigned ordinal) {
if (ordinal < count) return table[ordinal];

ordinal_error (ordinal, count, "FuncScope: Func", owner);
return 0;
}	// FuncLinker::access_func

// Look up variables subtable
VarLinker *FuncLinker::access_var_table (unsigned ordinal) {
if (ordinal < count) return func_vars[ordinal];

ordinal_error (ordinal, count, "FuncScope: VarScope", owner);
return 0;
}	// FuncLinker::access_var_table

// Look up functors subtable
FuncLinker *FuncLinker::access_func_table (unsigned ordinal) {
if (ordinal < count) return func_funcs[ordinal];

ordinal_error (ordinal, count, "FuncScope: FuncScope", owner);
return 0;
}	// FuncLinker::access_func_table

// Import functor table reference
FuncLinker *Importer::im_func_table (unsigned char tag) {
if (tag == TS_None) tag = im_tag ();

switch (tag) {
	case TS_Mod:
		return module_funcs;

	case TS_Func:
		{
		FuncLinker *func_table = im_func_table (TS_None);
		return func_table->access_func_table (im_fixpak ());
		}

	case TS_Self:
		return lambda_func_link ? lambda_func_link->access_func_table (0) : 0;

	default:
		wrong_tag ("func_table", tag);
		return 0;
	}
}	// Importer::im_func_table

// Exporting lambda
void Exporter::export_lambda (Prefix *prefix) {
Prefix *save_lambda = current_lambda;
current_lambda = prefix;

ex_tag (TS_Lambda);
prefix->expo_decls (*this);
prefix->expo_defs (*this);

current_lambda = save_lambda;
}	// Exporter::export_lambda

// Export prefix reference
void Exporter::ex_func_ref (Prefix *prefix) {
if (prefix) {
	PrefixName *name = prefix->name;
	
	if (name) {
	if (! name->ex_ref (*this)) {
		// no owner: built-in prefix
		unsigned opcode = opcode_lookup (prefix);
		if (opcode != Op_Null)
			ex_fixpak (TS_Base + opcode);				// (export as opcode)
		else {
			ex_tag (TS_Prim);							// (export as ordinal)
			ex_fixpak (name->ordinal);
			}
		}
	}	// (name)
	else
		export_lambda (prefix);
	}	// (prefix)

else	// (null prefix....)
		ex_tag (TS_None);
}	// Exporter::ex_func_ref

// Import prefix reference
Prefix *Importer::im_func_ref () {
// this is either tag or opcode
unsigned opcode = im_fixpak ();

if (opcode > TS_Base)
	return Prefix::locate (_O_Enum (opcode - TS_Base));

else switch (opcode) {
	case TS_None:
		return 0;

	case TS_Prim:
		return find_by_ordinal (im_fixpak ());

	case TS_Lambda:
		return import_lambda ();

	default: {
		FuncLinker *func_table = im_func_table (opcode);
		return func_table->access_func (im_fixpak ());
		}
	}	// switch (opcode)
}	// Importer::im_func_ref

//
//	Export/import: parameter lists
//

// Export parameters list
void Exporter::ex_params (Param *param) {
while (param) {
	// Note: parameters always belong to current scope
	ex_fixpak (param->ordinal + 1);
	ex_fixpak (param->flags);

	// TODO: export 'param->init'

	param = param->next;
	}

// (parameter list terminator)
ex_fixpak (0);
}	// Exporter::ex_params

// Import parameters list
Param *Importer::im_params (VarLinker *var_table, unsigned offset) {
Param *list;
Param **p_tail = &list;
unsigned ordinal;

while (ordinal = im_fixpak ()) {
	unsigned flags = im_fixpak ();
	// TODO: import initialiser...
	// TODO: check ordinals...

	Param *param = new ("Param")
		Param (var_table->access_var (ordinal - 1 - offset), flags, 0, 0);

	*p_tail = param;
	p_tail = &param->next;
	}	// while (ordinal)

*p_tail = 0;			// (final)
return list;
}	// Importer::im_params

//
//	Export/import: functors
//

// tags for functor/class
enum TF_enum {
	TF_Functor = 1,			// user functor
	TF_Class = 2,			// user class (w/o superclass)
	TF_SubClass = 3,		// user derived class
	};

// Constructor for Importer::FuncLinker
FuncLinker::FuncLinker (unsigned count, Prefix *owner) {
table = new ("Functor/Links") Prefix * [this->count = count];
func_vars = new ("Func.Vars/Links") VarLinker * [count];
func_funcs = new ("Func.Funcs/Links") FuncLinker * [count];

while (count --) {
	table[count] = 0;
	func_vars[count] = 0;
	func_funcs[count] = 0;
	}

this->owner = owner;
}	// FuncLinker::FuncLinker

// Import prefix declaration
Prefix *Importer::impo_pfx_decl (char *ident, VarLinker * &var_table, FuncLinker * &func_table) {
unsigned char tag = im_tag ();

Prefix *super = 0;

switch (tag) {
	case TF_Functor:
		return im_decl_functor (ident, var_table, func_table);

	case TF_SubClass:
		super = im_func_ref ();
		// (fall through)

	case TF_Class:
		return im_decl_class (ident, super, var_table, func_table);

	default:
		wrong_tag ("func_decls", tag);
	}	// switch (tag)

return 0;
}	// Importer::impo_pfx_decl

void Importer::im_func_decls (FuncLinker * &func_table, Prefix *owner) {
unsigned func_count = im_fixpak ();

func_table = new ("Importer/FuncTab") FuncLinker (func_count, owner);

Prefix **func_ptr = func_table->table;
VarLinker **var_table_ptr = func_table->func_vars;
FuncLinker **func_table_ptr = func_table->func_funcs;

while (func_count --) {
	t_ident ident;
	im_ident (ident);
	link_prefix (*func_ptr ++ = impo_pfx_decl (ident, *var_table_ptr ++, *func_table_ptr ++));
	}
}	// Importer::im_func_decls

// Import functor definitions for declarations
void Importer::im_func_defs (FuncLinker *func_table) {
unsigned func_count = func_table->count;
Prefix **func_ptr = func_table->table;
VarLinker **func_vars = func_table->func_vars;
FuncLinker **func_table_ptr = func_table->func_funcs;

unsigned ordinal = 0;

while (func_count --) {
	Prefix *func = *func_ptr ++;

	xp_ordinal (ordinal ++);
	func->impo_pfx_def (*this, *func_vars ++, *func_table_ptr ++);
	}
}	// Importer::im_func_defs

// Import functor lambda definition
Prefix *Importer::import_lambda () {
if (im_tag () == TF_Functor) {
	FuncLinker linker (1, 0);

	FuncLinker *save_linker = lambda_func_link;
	lambda_func_link = &linker;

	VarLinker * &var_table = *linker.func_vars;
	FuncLinker * &func_table = *linker.func_funcs;

	Prefix *func = im_decl_functor (0, var_table, func_table);
	*linker.table = func;
	func->impo_pfx_def (*this, var_table, func_table);

	lambda_func_link = save_linker;

	return func;
	}

// TODO: report error?
else return 0;
}	// Importer::import_lambda

// Destructor for Importer::FuncLinker
FuncLinker::~FuncLinker () {
unsigned index = count;
while (index --) {
	unlink_prefix (table [index]);
	delete func_vars [index];
	delete func_funcs [index];
	}

delete [] table;
delete [] func_vars;
delete [] func_funcs;
}	// FuncLinker::~FuncLinker

//
//	Export/import: prefixes
//

DL_EXPORT void Prefix::expo_decls (Exporter &exporter) {
ex_warn ("Exporting (pre): base Prefix");
}	// Prefix::expo_decls

DL_EXPORT void Prefix::expo_defs (Exporter &exporter) {
ex_warn ("Exporting: base Prefix");
}	// Prefix::expo_defs

DL_EXPORT void Prefix::impo_pfx_def (Importer &importer, VarLinker *var_table, FuncLinker *func_table) {
// TODO...
}	// Prefix::impo_pfx_def

// Export functor declaration
void P_Functor::expo_decls (Exporter &exporter) {
exporter.ex_tag (TF_Functor);

exporter.ex_scope_decls (var_scope);
exporter.ex_scope_decls (prefix_scope);
}	// P_Functor::expo_decls

// Import functor declaration
Prefix *Importer::im_decl_functor (char *ident, VarLinker * &var_table, FuncLinker * &func_table) {
P_Functor *functor = new ("Functor/im") P_Functor (ident);

functor->def_open ();

im_var_decls (true, var_table, functor);
im_func_decls (func_table, functor);

functor->def_close ();

return functor;
}	// Importer::im_decl_functor

// Export functor implementation
void P_Functor::expo_defs (Exporter &exporter) {
exporter.ex_params (params);

// Note: there is no need to export locals definitions
exporter.ex_scope_defs (prefix_scope);

exporter.ex_expr (body);
}	// P_Functor::expo_defs

// Import functor definition
void P_Functor::impo_pfx_def (Importer &importer, VarLinker *var_table, FuncLinker *func_table) {
Param *params = importer.im_params (var_table, 0);
importer.im_func_defs (func_table);

def_bind (params, importer.im_expr ());
}	// P_Functor::impo_pfx_def

// Export class declaration
void P_Class::expo_decls (Exporter &exporter) {
exporter.ex_tag (super ? TF_SubClass : TF_Class);
if (super)
	exporter.ex_func_ref (super);

exporter.ex_scope_decls (var_scope);
exporter.ex_scope_decls (prefix_scope);
}	// P_Class::expo_decls

// Import class declaration
Prefix *Importer::im_decl_class (char *ident, Prefix *super,
	VarLinker * &var_table, FuncLinker * &func_table) {
P_Class *classref = new ("Class/im") P_Class (ident, super);

classref->def_open ();

im_var_decls (true, var_table, classref);
im_func_decls (func_table, classref);

classref->def_close ();

return classref;
}	// Importer::im_decl_class

// Export class implementation
void P_Class::expo_defs (Exporter &exporter) {
exporter.ex_params (params);

exporter.ex_expr (ctor);
exporter.ex_expr (dtor);

// Note: there is no need to export members definitions
exporter.ex_scope_defs (prefix_scope);

// TODO: 
// virtuals decls/defs
}	// P_Class::expo_defs

// Import class definition
void P_Class::impo_pfx_def (Importer &importer, VarLinker *var_table, FuncLinker *func_table) {
Param *params = importer.im_params (var_table, super ? super->member_no : 0);

Expr *ctor = importer.im_expr ();
Expr *dtor = importer.im_expr ();

importer.im_func_defs (func_table);

// TODO...
virtuals = 0;
virtual_no = 0;
virtual_tab = 0;

def_bind (params, ctor, dtor);
}	// P_Class::impo_pfx_def

void DefFunctor::expo (Exporter &exporter) {
exporter.ex_tag (TX_DefFunc);
exporter.ex_func_ref (func);
}	// DefFunctor::expo

void DefClass::expo (Exporter &exporter) {
exporter.ex_tag (TX_DefClass);
exporter.ex_func_ref (classref);
}	// DefClass::expo

void GroupDefFunctor::expo (Exporter &exporter) {
// TODO...
}	// GroupDefFunctor::expo

//
//	Export/import: modules
//

void RootModule::expo_decls (Exporter &exporter) {
exporter.ex_scope_decls (var_scope);
exporter.ex_scope_decls (prefix_scope);
}	// RootModule::expo_decls

void RootModule::expo_defs (Exporter &exporter) {
exporter.ex_scope_defs (var_scope);
exporter.ex_scope_defs (prefix_scope);
}	// RootModule::expo_defs

static char const *starter = "<AWC 0.5.6>";
static char const *terminator = "<EOF>";

void Exporter::ex_module (RootModule *module) {
this->module = module;

ex_cstring (starter);

ex_ident (module->name ? module->name->ident : 0);

module->expo_decls (*this);
module->expo_defs (*this);

ex_cstring (terminator);
}	// Exporter::ex_module

void Importer::im_module (RootModule *module) {
this->module = module;		// ????

xp_cstring (starter);

t_ident mod_name;
im_ident (mod_name);

// TODO: store var ords at beginning...

im_var_decls (false, module_vars, module);
im_func_decls (module_funcs, module);

im_var_defs (module_vars);
im_func_defs (module_funcs);

if (! xp_cstring (terminator))
	syslog->put_cstr ("Bad terminator!\n");

delete module_vars;
delete module_funcs;

module_vars = 0;
module_funcs = 0;
}	// Importer::im_module

//
//	Entry points
//

// Export all modular data
bool Module::export_all (Exporter &exporter, Module *module) {
if (module) exporter.ex_module (Cast (RootModule, module));
return true;
}	// Module::export_all

// Import all modular data
bool Module::import_all (Importer &importer, Module *module) {
if (module) importer.im_module (Cast (RootModule, module));
return true;
}	// Module::import_all

#endif

