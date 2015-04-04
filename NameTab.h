
/*

	+---+---+---+---+---+---+
	|	'NameTab.h':
	|	Name tables, and name scopes.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

//
// Name node
//

// Log name
#define	D_Name_log_ex				\
	void log_ex (Logger &log)

// Export name
#define	D_Name_expo					\
	void expo (Exporter &exporter, bool pre)

// Release name
#define	D_Name_release				\
	void release ()

struct Name : SynTerm {
	char *ident;			// identifier
	NameTab *table;			// name table where symbol belongs (if any)
	unsigned hash;			// hash code of `name`
	unsigned ordinal;		// ordinal in nametable
	unsigned refs;			// references count

	struct Name *next;		// link to following node (if any)

	Name (char const *ident);
	~Name ();

	void log (Logger &log);

	// Export: reference to name
	bool ex_ref (Exporter &exporter);

	virtual D_Name_log_ex;

	virtual D_Name_expo;

	virtual D_Name_release;
	};

// Operation on name
typedef bool (* funcName) (Name *name);

//
// Name table
//

// Make name in this scope
#define	D_NameTab_mkName		\
	Name *mkName (char const *ident)

// Query owner of this scope
#define	D_NameTab_get_owner			\
	Prefix *get_owner ()

struct NameTab : SynTerm {
	Name **base;			// hash base
	unsigned no;			// capacity of hash base
	unsigned counter;		// total # of names stored
	bool locked;			// is scope locked?

	NameTab (unsigned no = 16);
	~NameTab ();

	// Lookup/install control routine
	Name *control (char const *ident, bool install);
	// Install 'ident'
	// returns 0, if installed already
	Name *install (char const *ident);
	// Look up for 'ident';
	// returns 0, if not installed
	Name *lookup (char const *ident);

	// Iterate 'func' through all of names stored
	// Returns # of successes
	unsigned forall (funcName func);

	// Same as 'forall', but sorted by ordinals
	// (flag ? descending : ascending)
	unsigned forall_ordered (funcName func, bool flag);

	// Find name with appropriate ordinal in scope
	Name *find_ordinal (unsigned ordinal);

	// Output scope to log
	void log (Logger &log);

	virtual D_NameTab_mkName;
	virtual D_NameTab_get_owner;
	};

#define	D_AnyVar_evalR		\
	Expr *&evalR()

// Variable name (abstract)
struct AnyVar : Name {
	AnyVar (char const *ident) : Name(ident) {}

	D_Name_log_ex;

	D_Name_expo;

	static AnyVar *install (char const *ident, bool is_local);

	virtual D_AnyVar_evalR;

	// Export variable name
	void ex_var_ref (Exporter &exporter);
	};

// Variables table (abstract)
struct VarScope : NameTab {
	// Wrappers for functions from 'NameTab'

	AnyVar *install (char const *ident)
		{ return (AnyVar *) NameTab::install (ident); }

	AnyVar *lookup (char const *ident)
		{ return (AnyVar *) NameTab::lookup (ident); }

	unsigned forall (bool (* funcAnyVar) (AnyVar *))
		{ return NameTab::forall ((funcName) funcAnyVar); }
	};

// Module global variable
struct ModuleVar : AnyVar {
	Expr *expr;

	ModuleVar (char const *ident) : AnyVar(ident) { expr = 0; }

	D_AnyVar_evalR;

	D_Name_expo;

	D_Name_release;
	};

// Module variables table
struct ModuleScope : VarScope {
	Module *owner;			// (qualified owner)

	ModuleScope (Module *owner) { this->owner = owner; }

	D_NameTab_mkName;
	D_NameTab_get_owner;
	};

// Functor local variable
struct FunctorVar : AnyVar {
	Expr **p_expr;			// (dynamically bound)

	FunctorVar (char const *ident) : AnyVar (ident) { p_expr = 0; }

	D_AnyVar_evalR;
	};

// Functor variables table
struct FunctorScope : VarScope {
	P_Functor *owner;			// (qualified owner)

	FunctorScope (P_Functor *owner) { this->owner = owner; }

	unsigned forall (bool (* funcFunctorVar) (FunctorVar *))
		{ return VarScope::forall ((bool (*) (AnyVar *)) funcFunctorVar); }

	D_NameTab_mkName;
	D_NameTab_get_owner;
	};

// Class local variable
struct ClassVar : AnyVar {
	Expr **p_expr;		// (dynamically bound)

	ClassVar (char const *ident) : AnyVar (ident) { p_expr = 0; }

	D_AnyVar_evalR;
	};

// Class variables table
struct ClassScope : VarScope {
	P_Class *owner;			// (qualified owner)

	ClassScope (P_Class *owner) { this->owner = owner; }

	unsigned forall (bool (* funcClassVar) (ClassVar *))
		{ return VarScope::forall ((bool (*) (AnyVar *)) funcClassVar); }

	D_NameTab_mkName;
	D_NameTab_get_owner;
	};

// Prefix name
struct PrefixName : Name {
	Prefix *prefix;				// (functor named by...)

	PrefixName (char const *ident) : Name(ident) { prefix = 0; }

	D_Name_log_ex;

	D_Name_expo;

	D_Name_release;
	};

// Prefix table
struct PrefixScope : NameTab {
	Prefix *owner;

	PrefixScope (Prefix *owner) { this->owner = owner; }

	D_NameTab_mkName;
	D_NameTab_get_owner;

	PrefixName *install (char const *ident)
		{ return (PrefixName *) NameTab::install (ident); }

	PrefixName *lookup (char const *ident)
		{ return (PrefixName *) NameTab::lookup (ident); }

	unsigned forall (bool (* funcPrefixName) (PrefixName *))
		{ return NameTab::forall ((funcName) funcPrefixName); }
	};

