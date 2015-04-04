
/*

	+---+---+---+---+---+---+
	|	"Logger.h":
	|	Logging (debugging & error output).
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

//
// Output flags
//
enum OF_enum {
	OF_tr_expr	= 1 << 0,		// (expressions trace)
	OF_tr_vals	= 1 << 1,		// (values trace)
	OF_tr_refc	= 1 << 2,		// (reference count trace)
	OF_tr_addr	= 1 << 3,		// (addresses trace)
	OF_prompt	= 1 << 4,		// (input prompt)
	OF_tr_mod	= 1 << 5,		// (module frames)
	OF_tr_full	= 1 << 6,		// (show contents of arrays/hashes)

	OF_tr_stmt	= 1 << 7,		// (statement trace)
	};

//
//	Output log
//

struct Logger {
	unsigned indent;			// (current indent level)

	// (global / local scope)
	Prefix *global_scope, *local_scope;

	Logger () {
		global_scope = local_scope = 0;
		indent = 0;
		}

	// Put character
	virtual Logger *put_ch (char ch) = 0;

	// Put C-string (null-terminated)
	virtual Logger *put_cstr (const char *str) = 0;

	// Put newline
	Logger *put_nl () { put_ch ('\n'); return this; }

	// Log integer
	virtual Logger *log_fixed (S_fixed val) = 0;

	// Log float
	virtual Logger *log_float (S_float val) = 0;

	// Put decimal integer
	Logger *put_dec (unsigned digits, unsigned val);
	// Put hexadecimal integer
	Logger *put_hex (unsigned digits, unsigned val);
	// Put octal integer
	Logger *put_oct (unsigned digits, unsigned val);
	// Put binary integer
	Logger *put_bin (unsigned digits, unsigned val);

	// Startup banner
	void log_banner (unsigned f_count);

	// Increment/decrement indentation
	void log_enter () { indent ++; }
	void log_leave () { indent --; }

	// Log indent at line start
	void log_indent ();

	// Log name (qualified with scope)
	void log_qname (Name *name);

	// Flag value (as '+' / '-')
	Logger *log_flag (bool flag)
		{ put_ch (flag ? '+' : '-'); return this; }

	// Log name table
	// (mode < 0 ? sort descending; mode > 0 ? sort ascending)
	void log_nametab (NameTab *table, int mode);

	// Log functor/class parameters list
	void log_params (Param *params);

	// Log functor definition
	void log_functor (P_Functor *func);

	// Log group of functors
	void log_functor_group (GroupDefFunctor *func_def);

	// Log class definition
	void log_class (P_Class *classref);

	// Log class virtuals table
	void log_virtuals (P_Class *classref);

	// Log ASCII character
	Logger *log_ch (unsigned char ch);

	// Log Unicode character
	Logger *log_wch (unsigned wch);

	// Log string
	Logger *log_sstring (S_string &s_src);

	// Log string
	Logger *log_string (X_String *string) { string->log (*this); return this; }

	// Log list
	Logger *log_list (X_List *list);

	// Log term
	Logger *log_term (X_Term *term);

	// Log prefix
	Logger *log_prefix (Prefix *pfx);

	// Log block
	Logger *log_block (X_Block *block);

	// Log 'object' belonging to 'class' (recursive)
	Logger *log_object (P_Class *classref, X_Object *object);

	// Log array
	Logger *log_array (X_Array *array);
	
	// Log hash
	Logger *log_hash (X_Hash *hash);

	// Log pattern
	Logger *log_pattern (X_Pattern *pattern);

	// Log ring
	Logger *log_ring (X_Ring *ring);

	// Log stream
	Logger *log_stream (Stream *stream);

	// Log stream codec
	Logger *log_scodec (SCodec *codec);

	// Log external
	Logger *log_external (External *external);

	//
	// Expressions
	//

	// Log expression
	Logger *log_expr (Expr *expr);

	// Log value
	Logger *log_value (VType type, VDatum &val);

	// Log type name
	Logger *log_type_name (VType type);

	//
	// Modular
	//

	// Log module opening
	void log_prolog (Module &module, bool flag);

	// Log module closing
	void log_epilog (Module &module, bool flag);

	// Log module current locations (stmt / line)
	void log_location (Module &module);

	// Log module prompt
	void log_prompt (Module &module);

	// Log statement && results of evaluation
	// (depending on module flags)
	void log_stmt (Module &module, Expr *expr, VType type, VDatum &val);

	// Log version number
	void log_version (unsigned version);
	};

extern Logger *syslog;

