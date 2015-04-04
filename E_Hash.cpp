
/*

	+---+---+---+---+---+---+
	|	"E_Hash.cpp":
	|	Hash & identity primaries; implementation of hashes.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef HEADER

#include "Eval.h"

#endif

struct X_Hash : Expr {
	enum { DefaultCap = 16 };

	unsigned count;			// (# of items in hash)
	unsigned cap;			// (table capacity)

	struct H_Node {
		unsigned h_code;			// (key hash code)
		Expr *h_key;				// (key)
		Expr *h_value;				// (value)

		H_Node (unsigned code, Expr *key, Expr *value)
			{ h_code = code; h_key = key; h_value = value; }

		struct H_Node *next;		// (next node)
		} ** table;					// (nodes table [cap])

	FixedRange tolerance;			// tolerance (optional)

	// Constructor
	X_Hash (unsigned capacity, FixedRange tolerance);

	// Change hash capacity/tolerance
	unsigned rehash (unsigned new_capacity, FixedRange new_tolerance);

	// Clear entire  hash contents
	void clear ();

	// Get access to hash element
	// (insert it, if 'do_insert')
	Expr *&elem (Expr *h_key, bool do_insert);

	// Remove element with 'key' from hash
	// (Returns value of element removed)
	Expr *remove (Expr *h_key);

	// Iterate through hash
	// (assigning 'key'/'value' pairs to 'R_loop' and evaluating 'body' on each iteration)
	void loop (Expr *&R_loop, P_Iterator::IterContext &IC);

	// Save hash to list (with 'h_save')
	X_List *save (Expr *(*h_save) (H_Node *h_node));

	// Load hash from list
	void load (Expr *args);

	D_Expr_put;
	D_Expr_evalV;
	D_Expr_identV;
	D_Expr_hash;
	D_Expr_log;
	D_Expr_release;
	};

#ifndef HEADER

#include "Logger.h"

//
//	Calculate hash code
//

struct P_HCode : Prefix {
	P_HCode (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
		val._fixed = hashX (evalX_X_R (args));
		return T_fixed;
		}	// evalV
	};

//
//	Identity / not identity check predicate
//

struct P_Ident : Prefix {
	bool polarity;

	P_Ident (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	VType evalV (VDatum &val, Expr *args) {
		Expr *first = get_arg (args);
		val._fixed = identX (evalX_X_R (first), evalX_X_R (args)) == polarity;
		return T_fixed;
		}	// evalV
	};

//
//
//	Hash operations
//
//

// Hash constructor
X_Hash::X_Hash (unsigned capacity, FixedRange tolerance) {
if (! capacity) capacity = DefaultCap;
this->tolerance = tolerance;

count = 0;

H_Node **tbl_p = table = new ("Hash/table") H_Node * [cap = capacity];
while (capacity --) *tbl_p ++ = 0;
}	// X_Hash::X_Hash

//
//	Restructurize hash
//

unsigned X_Hash::rehash (unsigned new_capacity, FixedRange new_tolerance) {
unsigned old_cap = cap;
if (new_capacity <= 0) new_capacity = DefaultCap;

if (old_cap != new_capacity) {

H_Node **old_tbl = table;
H_Node **tbl_p = table = new ("Hash/table") H_Node * [cap = new_capacity];
while (new_capacity --) *tbl_p ++ = 0;

// move all hash nodes to new table:
while (old_cap --) {
	H_Node *h_next;
	for (H_Node *h_node = old_tbl[old_cap]; h_node; h_node = h_next) {
		h_next = h_node->next;

		H_Node *&h_entry = table [h_node->h_code % cap];
		h_node->next = h_entry;
		h_entry = h_node;
		}
	}	// while (old_cap)

delete [] old_tbl;

}	// (old_cap != new_capacity)

return count;
}	// X_Hash::rehash

VType X_Hash::evalV (VDatum &val, bool full) {
val._hash = this;
return T_hash;
}	// X_Hash::evalV

// Log 'hash'
Logger *Logger::log_hash (X_Hash *hash) {
put_ch ('<');
	log_fixed (hash->count);
put_cstr (" / ");
	log_fixed (hash->cap);
put_ch ('>');

if (Module::current->flags & OF_tr_full) {
	put_cstr (" = ");
	
	put_ch ('{');

	X_Hash::H_Node **table_p = hash->table;
	unsigned count = hash->cap;

	while (count --)
		for (X_Hash::H_Node *h_node = *table_p ++; h_node; h_node = h_node->next) {
			log_expr (h_node->h_key);
			put_cstr ("::");
			log_expr (h_node->h_value);
			put_cstr (", ");
			}	// for ()

	put_ch ('}');
	}

return this;
}	// Logger::log_hash

void X_Hash::log (Logger &log) {
log.log_hash (this);
}	// X_Hash::log

// Clear hash
void X_Hash::clear () {
unsigned capacity = cap;
for (H_Node **tbl_p = table; capacity --; tbl_p ++) {
	H_Node *next;
	for (H_Node *node = *tbl_p; node; node = next) {
		unlink_expr (node->h_key);
		unlink_expr (node->h_value);

		next = node->next;
		delete node;
		}

	*tbl_p = 0;
	}

count = 0;
}	// X_Hash::clear

void X_Hash::release () {
clear ();

delete [] table;
Expr::release ();
}	// X_Hash::release

unsigned X_Hash::put (Stream *out) {
unsigned index = cap;
X_Hash::H_Node **tbl_p = table;
unsigned count = 0;

while (index --) {
	for (X_Hash::H_Node *h_node = *tbl_p ++; h_node; h_node = h_node->next) {
		count += put_expr (out, h_node->h_key);
		count += put_expr (out, h_node->h_value);
		}
	}	// while (index)

return count;
}	// X_Hash::put

//
//
//	TODO: make hash table growing on need
//
//

//
// Get access to hash value associated with 'key'
// (insert it, if 'do_insert')
//
Expr *&X_Hash::elem (Expr *h_key, bool do_insert) {
if (h_key) {
unsigned h_code = h_key->hash ();
VDatum h_val;
VType h_type = h_key->evalV (h_val, false);

H_Node *&h_entry = table[h_code % cap], *h_node;

for (h_node = h_entry; h_node; h_node = h_node->next)
	if (h_node->h_code == h_code &&
		h_node->h_key->identV (h_type, h_val))
			break;			// (found node with required key!)

if (! h_node && do_insert) {
	// (node not found -- create it)
	h_node = new ("Hash/H_Node") H_Node (h_code, link_expr (h_key), 0);
	h_node->next = h_entry;
	h_entry = h_node;

	++ count;
	}

relink_expr (h_key);				// (new)
// relink_value (h_type, h_val);
if (h_node) return h_node->h_value;
// (else fall through...)
}	// (h_key)

return R_null;
}	// X_Hash::elem

//
// Remove (and return) hash value associated with 'key'
//
Expr *X_Hash::remove (Expr *h_key) {
if (h_key) {
VDatum h_val;
VType h_type = h_key->evalV (h_val, false);
Expr *value = 0;

unsigned h_code = h_key->hash ();

H_Node *h_node;
for (H_Node **h_pnode = &table[h_code % cap];
	 h_node = *h_pnode;
	 h_pnode = &h_node->next)
	if (h_node->h_code == h_code &&
		h_node->h_key->identV (h_type, h_val)) {

		// (found node with required key -- remove it!)
		value = h_node->h_value;
		relock_expr (-1, value);
		
		unlink_expr (h_node->h_key);
		*h_pnode = h_node->next;
		delete h_node;

		-- count;
		break;
		}

relink_expr (h_key);				// (new)
return value;
}	// (h_key)

// No such key in hash
return 0;
}	// X_Hash::remove

// Iterate through hash
// (assigning 'key'/'value' pairs to 'R_loop' and evaluating 'body' on each iteration)
void X_Hash::loop (Expr *&R_loop, P_Iterator::IterContext &IC) {
if (count) {
X_List *pair = new X_List (0, 0);
link_expr (pair);

unsigned no = cap;
for (H_Node **tbl_p = table; no --; tbl_p ++) {
	for (H_Node *node = *tbl_p; node; node = node->next) {
		mutateR_X (R_loop, pair);
		mutateR_X (pair->first, node->h_key);
		mutateR_X (pair->next, node->h_value);

		if (! IC.next ()) goto done;
		}
	}

done:
unlink_expr (pair);
}	// (count)
}	// X_Hash::loop

// Calculate hash code of hash
unsigned X_Hash::hash () {
unsigned value = 0;

unsigned no = cap;
for (H_Node **tbl_p = table; no --; tbl_p ++) {
	for (H_Node *node = *tbl_p; node; node = node->next) {
		value ^= hashX (node->h_key) ^ hashX (node->h_value);
		}
	}

return value;
}	// X_Hash::hash

// Check hashes for identity
bool X_Hash::identV (VType type, VDatum &val) {
if (type == T_hash) {
	X_Hash *hash = val._hash;
	
	if (hash == this) return true;

	// TODO: implement hashes identity test
	return false;
	}

return false;
}	// X_Hash::identV

//
// Prototype for functor returning hash object
//

#define	D_P_Hash_eval		X_Hash* eval(Expr *args)

struct P_Hash : Prefix {
	P_Hash (char const *ident, O_Enum op) : Prefix (ident, op) {}

	D_Prefix_evalV;

	virtual D_P_Hash_eval;
	};

VType P_Hash::evalV (VDatum &val, Expr *args) {
X_Hash *hash = eval (args);
if (hash) {
	val._hash = hash;
	return T_hash;
	}
else
	return T_undef;
}	// P_Hash::evalV

X_Hash *P_Hash::eval (Expr *args) { return 0; }

//
// Expect hash expression
//
X_Hash *Prefix::expect_hash (Expr *expr) {
VDatum val;
VType type;

if ((type = evalV_X (expr, val)) == T_hash)
	return val._hash;

type_error (expr, T_hash, type, val);
return 0;
}	// Prefix::expect_hash

//
//	Create new hash
//

struct P_Hash_Create : P_Hash {
	P_Hash_Create (char const *ident) : P_Hash (ident, Op_Null) {}

	D_P_Hash_eval;
	};

X_Hash *P_Hash_Create::eval (Expr *args) {
S_fixed capacity = expect_fixed (get_arg (args), 0);

FixedRange tolerance (0, 0);
if (args)
	expect_range (args, tolerance);

return new ("Hash") X_Hash (capacity, tolerance);
}	// P_Hash_Create::eval

//
//	Restructurize hash
//

struct P_Hash_Rehash : Prefix {
	P_Hash_Rehash (char const *ident, O_Enum op): Prefix (ident, op) {}

	D_Prefix_evalV;
	};

VType P_Hash_Rehash::evalV (VDatum &val, Expr *args) {
X_Hash *hash = expect_hash (get_arg (args));
unsigned capacity = expect_fixed (args, 0);

FixedRange tolerance (0, 0);
if (args)
	expect_range (args, tolerance);

if (hash) {
	val._fixed = hash->rehash (capacity, tolerance);
	return T_fixed;
	}

return T_undef;
}	// P_Hash_Rehash::evalV

//
//	Count of elements in hash
//

struct P_Hash_Count : Prefix {
	P_Hash_Count (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Hash *hash = expect_hash (args);

	if (hash) {
		val._fixed = hash->count;
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

//
//	Get element of hash
//

struct P_Hash_Elem : PrefixR {
	P_Hash_Elem (char const *ident, O_Enum op): PrefixR (ident, op) {}

	Expr *&evalR (Expr *args) {
	X_Hash *hash = expect_hash (get_arg (args));

	if (hash)
		return hash->elem (evalX_X (args), true);

	return R_null;
	}	// evalR
	};

//
//	Check key presence in the hash (do not create)
//

struct P_Hash_Exists : Prefix {
	P_Hash_Exists (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Hash *hash = expect_hash (get_arg (args));

	val._fixed = hash ?
		&(hash->elem (evalX_X (args), false)) != &R_null :
		0;
	return T_fixed;
	}	// evalV
	};

//
//	Lookup element in hash (do not create)
//

struct P_Hash_Lookup : PrefixX {
	P_Hash_Lookup (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
	X_Hash *hash = expect_hash (get_arg (args));

	if (hash)
		return hash->elem (evalX_X (args), false);

	return 0;
	}	// evalX
	};

//
//	Lookup element in hash (never create it)
//

struct P_Hash_Remove : PrefixX {
	P_Hash_Remove (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
	X_Hash *hash = expect_hash (get_arg (args));

	if (hash)
		return hash->remove (evalX_X (args));

	return 0;
	}	// evalX
	};

//
//	Clear entire hash
//

struct P_Hash_Clear : Prefix {
	P_Hash_Clear (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Hash *hash = expect_hash (args);

	if (hash) {
		val._fixed = hash->count;
		hash->clear ();
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

//
//	Iterate through hash
//

struct P_Hash_Loop : P_Iterator {
	P_Hash_Loop (char const *ident, O_Enum op): P_Iterator (ident, op) {}

	void evaluate (IterContext &IC, Expr *args) {
		X_Hash *hash = expect_hash (get_arg (args));
		Expr *&R_loop = expectR_X (get_arg (args));
		if (hash && args) {
			IC.start (args);
			hash->loop (R_loop, IC);
			}
		}	// evaluate
	};

//
//	Save list of keys/values/elems from hash
//

// Save key of hash node
static Expr *h_save_key (X_Hash::H_Node *h_node) {
return h_node->h_key;
}	// h_save_key

// Save value of hash node
static Expr *h_save_value (X_Hash::H_Node *h_node) {
return h_node->h_value;
}	// h_save_value

// Save (key, value) of hash node (in list form)
static Expr *h_save_elem (X_Hash::H_Node *h_node) {
return new ("List/Hash") X_List (h_node->h_key, h_node->h_value);
}	// h_save_elem

// Save hash to list (with 'h_save')
X_List *X_Hash::save (Expr *(*h_save) (X_Hash::H_Node *h_node)) {
X_List *list = 0;
unsigned count = cap;
X_Hash::H_Node **tbl_p = table;

while (count --) {
	for (X_Hash::H_Node *h_node = *tbl_p ++; h_node; h_node = h_node->next)
		list = new ("Hash/elem") X_List (h_save (h_node), list);
	}	// while (count)

return list;
}	// X_Hash::save

struct P_Hash_Save : PrefixX {
	Expr * (*h_save) (X_Hash::H_Node *h_node);

	P_Hash_Save (char const *ident, O_Enum op,
		Expr * (*h_save) (X_Hash::H_Node *h_node)): PrefixX (ident, op)
			{ this->h_save = h_save; }

	Expr *evalX (Expr *args) {
		X_Hash *hash = expect_hash (args);
		return hash ? hash->save (h_save) : 0;
		}	// evalX
	};

//
//	Load list of keys/values/elems to hash
//

// Deconstruct list/expression
void de_list (Expr *list, Expr *&head, Expr *&tail) {
if (list) {
	X_List *node = list->isList ();
	if (node) {
		head = node->first;
		tail = node->next;
		}
	else {
		head = list;
		tail = 0;
		}
	}
}	// de_list

// Load hash from list 'args'
void X_Hash::load (Expr *args) {
while (args) {
	Expr *node;
	de_list (args, node, args);
	if (node) {
		Expr *h_key, *h_value;
		de_list (node, h_key, h_value);
		if (h_key)
			mutateR_X (elem (evalX_X (h_key), true), evalX_X (h_value));
		}
	}
}	// X_Hash::load

struct P_Hash_Load : P_Hash {
	P_Hash_Load (char const *ident) : P_Hash (ident, Op_Null) {}

	X_Hash *eval (Expr *args) {
	X_Hash *hash = expect_hash (get_arg (args));

	if (hash) hash->load (args);

	return hash;
	}	// eval
	};

//
//	Internal hash capacities
//

struct P_Hash_Inspect : PrefixX {
	P_Hash_Inspect (char const *ident, O_Enum op): PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_Hash_Inspect::evalX (Expr *args) {
X_Hash *hash = expect_hash (args);

if (hash) {
Expr *list = 0;

unsigned cap = hash->cap;
X_Hash::H_Node **tbl_p = hash->table;

while (cap --) {
	unsigned count = 0;
	for (X_Hash::H_Node *h_node = *tbl_p ++; h_node; h_node = h_node->next)
		++ count;

	Expr *value = new ("Hash/inspect") X_Fixed (count);
	if (list)
		list = new ("Hash/inspect") X_List (value, list);
	else
		list = value;
	}	// while (cap)

return list;
}

return 0;
}	// P_Hash_Inspect::evalX

//
//	Switch expression
//

struct P_Switch : Prefix {
	P_Switch (char const *ident) : Prefix (ident, Op_Null) {}

	D_Prefix_evalV;
	};

VType P_Switch::evalV (VDatum &val, Expr *args) {
Expr *key = evalX_X (get_arg (args));

// TODO: rewrite with de_list
// TODO: use get_arg (???) to expand...

X_List *node;
while (args && (node = args->isList ())) {
	X_List *check = (args = node->first) ? args->isList () : 0;
	VDatum v_val;

	if (check && key->identV (evalV_X (check->first, v_val), v_val))
		{ args = check->next; break; }

	args = node->next;
	}

relink_expr (key);

return args ? args->evalV (val, true) : T_undef;
}	// P_Switch::evalV

//
//	Operation table
//

static bool init_primaries_hash (int order) {

//		[Categories]

//^C	Hash
//^B	Hash functors
//^D	Functors, operating on hashes.

//		[Types]

//^T	Hash
//^B	Hash value.
//^D	Anything evaluating to hash.
//^D	(Reports error, if argument is not hash.)

//		[Errors]

//^E	ExpectHash
//^B	Hash operand expected.
//^D	Expected operand, evaluating to hash.

//		--------

//^G	hcode ident differ

//^N	hcode [Unary]
//^P	hcode (V: Any) => Int
//^B	Hash code value.
//^D	Calculate hash code of result of \V.

	DefBuiltin (P_HCode ("hcode"));

//^N	ident [Predicate | Binary]
//^P	ident (V: Any, W: Any) => Bool
//^B	Test for identity.
//^D	Check results of \V and \W for structural identity:
//^\	!true (1), if both operands evaluate to same result; !false (0) otherwise.
//^D	Syntax: \V [==] \W.

//^N	differ [Predicate | Binary]
//^P	differ (V: Any, W: Any) => Bool
//^B	Test for difference.
//^D	Check results of \V and \W for structural difference:
//^\	!false (0), if both operands evaluate to same result; !true (1) otherwise.
//^D	Syntax: \V [<>] \W.

	DefBuiltin (P_Ident ("ident", Op_Ident, true));
	DefBuiltin (P_Ident ("differ", Op_Differ, false));

//^G	is_hash expect_hash

//^N	is_hash [Predicate | Hash]
//^P	is_hash (V: Any) => Bool
//^B	Check for hash value.
//^D	Predicate: true, if argument \V evaluates to hash.

	DefBuiltin (P_IsType ("is_hash", Op_Null, T_hash));

//^N	expect_hash [Wrapper | Hash]
//^P	expect_hash (V: Any, @Body: Any) => Any
//^B	Expect hash value.
//^D	If argument \V evaluates to hash, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_hash", Op_Null, T_hash));

//^G	hash h_rehash

//^N	hash [Hash | Constructor]
//^P	hash ([Capacity: Int]) => Hash
//^B	Hash constructor.
//^D	Creates and returns new (empty) hash (with optional initial \Capacity).

	DefBuiltin (P_Hash_Create ("hash"));
	DefBuiltin (P_Hash_Create ("h_create"));

//^G	h_elem h_lookup h_remove

//^N	h_elem [Hash | Mutable]
//^P	h_elem (Hash: Hash, Key: Any) => Mutable
//^B	Hash value accessor.
//^D	Returns mutable reference to value associated with \Key in hash \Hash.
//^D	(If \Key was not in \Hash, it is created with undefined initial value.)
//^D	Syntax: \Hash->\Key.

	DefBuiltin (P_Hash_Elem ("h_elem", Op_HElem));

//^N	h_lookup [Hash]
//^P	h_lookup (Hash: Hash, Key: Any) => Any
//^B	Hash value lookup.
//^D	Returns value associated with \Key in hash \Hash.
//^D	(If \Key is not in \Hash returns !undef; new key is NOT created).

	DefBuiltin (P_Hash_Lookup ("h_lookup", Op_HLookup));

//^N	h_remove [Hash | Mutator]
//^P	h_remove (Hash: Hash, Key: Any) => Any
//^B	Hash value remove.
//^D	Removes value associated with \Key from hash \Hash.
//^D	(Returns removed value; or !undef, if there was no value with \Key.)

	DefBuiltin (P_Hash_Remove ("h_remove", Op_HRemove));

//^N	h_count [Hash]
//^P	h_count (Hash: Hash) => Int
//^B	Hash counter.
//^D	Returns total number of key/value pairs in hash \Hash.

	DefBuiltin (P_Hash_Count ("h_count", Op_Null));

//^N	h_clear [Hash]
//^P	h_clear (Hash: Hash) => Int
//^B	Clear hash.
//^D	Clears hash \Hash, removing all elements from it.
//^D	Returns number of elements, which were present in hash.

	DefBuiltin (P_Hash_Clear ("h_clear", Op_Null));

//^G	h_loop

//^N	h_loop [Hash | Iterator]
//^P	h_loop (Hash: Hash, Var: Mutable, @Body: Any) => Any
//^B	Hash iterator.
//^D	Iterates through hash \Hash.
//^D	For each element of hash, list of (key, value) is assigned to mutable \Var, then \Body is evaluated.
//^D	(Keys/values order is unpredictable.)
//^D	Returns result of final evaluation of \Body.

	DefBuiltin (P_Hash_Loop ("h_loop", Op_Null));

//^G	h_keys h_values h_save h_load

//^N	h_keys [Hash | List]
//^P	h_keys (Hash: Hash) => List
//^B	List of hash keys.
//^D	Return open list of all keys in \Hash, in no predictable (but same, as in !h_values) order.

//^N	h_values [Hash | List]
//^P	h_values (Hash: Hash) => List
//^B	List of hash values.
//^D	Return open list of all values in \Hash, in no predictable (but same, as in !h_keys) order.

//^N	h_save [Hash | List]
//^P	h_save (Hash: Hash) => List
//^B	List of hash elements.
//^D	Return open list of all (Key, Value) pairs in \Hash, in no predictable (but same, as in !h_keys and !h_values) order.

	DefBuiltin (P_Hash_Save ("h_keys", Op_Null, h_save_key));
	DefBuiltin (P_Hash_Save ("h_values", Op_Null, h_save_value));
	DefBuiltin (P_Hash_Save ("h_save", Op_Null, h_save_elem));

//^N	h_load [Hash | List]
//^P	h_load (Hash: Hash, (Key1: Any, Value1: Any), (Key2: Any, Value2: Any), ... (KeyN: Any, ValueN: Any), ) => Hash
//^B	Load hash with list of elements.
//^D	Loads N pairs of (\Key, \Value) into hash \Hash.
//^D	Returns \Hash.

	DefBuiltin (P_Hash_Load ("h_load"));

//^N	h_rehash [Hash]
//^P	h_rehash (Hash: Hash, Capacity: Int) => Int
//^B	Change hash capacity.
//^D	Explicitly change capacity of hash \Hash to \Capacity (preserving current hash contents).
//^D	Returns: current number of hash entries.

	DefBuiltin (P_Hash_Rehash ("h_rehash", Op_Null));

//^N	h_inspect [Hash]
//^P	h_inspect (Hash: Hash) => List
//^B	Inspect hash internal allocation.
//^D	Returns list of internal bucket capacities in hash \Hash.

	DefBuiltin (P_Hash_Inspect ("h_inspect", Op_Null));

//^N	h_exists [Hash | Predicate]
//^P	h_exists (Hash: Hash, Key: Any) => Bool
//^B	Check key for presence in hash.
//^D	Returns !true, if \Key present in hash \Hash; !false otherwise.

	DefBuiltin (P_Hash_Exists ("h_exists", Op_Null));

//^G	switch

//^N	switch [Conditional]
//^P	switch (Value: Any, (Case1: Any, Action1: Any), (Case2: Any, Action2: Any), ... (CaseN: Any, ActionN: Any), [Default: Any]) => Any
//^B	Choose action by key.
//^D	For \Value specified, select first (\Case, \Action) pair, where result of \Case is identical to \Value.
//^D	If all checks failed, select \Default as \Action.
//^D	(Returns result of evaluation of selected \Action.)

	DefBuiltin (P_Switch ("switch"));

return true;
}	// init_primaries_hash

DefSubSystem ("hash", init_primaries_hash, 0);

#endif

