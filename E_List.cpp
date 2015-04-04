
/*

	+---+---+---+---+---+---+
	|	"E_List.cpp":
	|	List primaries.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include "Eval.h"

//
//	Decay value to list
//

static void list_eval (Expr *&expr) {
if (expr) expr = expr->evalX();
}	// list_eval

//
//	Output list to stream
//

unsigned X_List::put (Stream *out) {
return put_expr (out, first) + put_expr (out, next);
}	// X_List::put

//
//	List length
//

// Calculate actual list length of 'expr'
static unsigned list_len (Expr *expr) {
unsigned count = 0;
while (expr) {
	X_List *list = expr->isList();
	expr = list ? list->next : 0;
	++ count;
	}
return count;
}	// list_len

struct P_List_Length : Prefix {
	P_List_Length (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		list_eval(args);

		val._fixed = list_len(args);

		relink_expr(args);
		return T_fixed;
		}	// evalV
	};

//
//	List clone
//

// Make exact list copy of 'expr'
static Expr *list_clone (Expr *expr) {
if (expr) {
	X_List *list = expr->isList ();

	return list ?
		new ("List/clone") X_List (list_clone(list->first), list_clone(list->next)) :
		expr;
	}	// if (expr)

return 0;
}	// list_clone

struct P_List_Clone : PrefixX {
	P_List_Clone (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		list_eval (args);

		Expr *result = list_clone (args);

		relink_expr(args);

		return result;
		}	// evalX
	};

//
//	Lists concatenation
//

// Concatenate lists left & right
// (recursive, with invariant left [+] right)
// Note: 'right' *is included* into result
static Expr *list_cat_rec (Expr *left, Expr *right) {
if (left) {
	X_List *list = left->isList ();
	if (list) {
		left = list->first;
		right = list_cat_rec (list->next, right);
		}
	return right ? new ("List/rcat") X_List(left, right) : left;
	}

else return right;
}	// list_cat_rec

struct P_List_Cat : PrefixX {
	P_List_Cat (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		Expr *first = get_arg (args);

		list_eval (first);
		list_eval (args);

		Expr *result = list_cat_rec (first, args);

		if (result != first) relink_expr (first);
		if (result != args) relink_expr (args);

		return result;
		}	// evalX
	};

//
//	Lists replication
//

// Replication of 'expr' 'count' times
// (assuming count > 0)
// Note: 'expr' *is included* into result, as last element
static Expr *list_rep_rec (unsigned count, Expr *expr) {
Expr *result = expr;
while (-- count) result = list_cat_rec (expr, result);
return result;
}	// list_rep_rec

struct P_List_Rep : PrefixX {
	P_List_Rep (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		int count = expect_fixed (get_arg (args), 1);
		list_eval(args);

		if (count > 1) {
			Expr *result = list_rep_rec (count, args);
			relink_expr(args);
			return result;
			}

		return count == 1 ? args : 0;
		}	// evalX
	};

//
//	List reversion
//

// List reversion
// Recursive: invariant is [~] left [+] right
static Expr *list_rev_rec (Expr *to, Expr *from) {
if (from) {
	X_List *list = from->isList();
	if (list) from = list->first;
	if (to) from = new ("List/rev") X_List (from, to);
	return list ? list_rev_rec (from, list->next) : from;
	}
else return to;
}	// list_rev_rec

struct P_List_Rev : PrefixX {
	P_List_Rev (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		list_eval(args);

		Expr *result = list_rev_rec (0, args);

		relink_expr(args);

		return result;
		}	// evalX
	};

//
//	Get reference to list
//

struct P_List_Ref : PrefixX {
	P_List_Ref (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		list_eval (args);
		return args;
		}	// evalX
	};

//
// Return protected reference
// (protected from destruction of 'owner')
//

Expr *&prot_ref (Expr *owner, Expr **p_node) {
if (p_node) {
if (*p_node == owner)
	{ return consR_X (owner); }

if (owner->refs)			// (owner have links...)
	return *p_node;

// (owner unlinked: protect reference)
Expr *value = *p_node;
relock_expr (1, value);
relink_expr (owner);
relock_expr (-1, value);
return consR_X (value);			// TODO: release everything unneeded (???)
}	// (p_node)

relink_expr (owner);
return consR_X (0);
}	// prot_ref

// TODO: in LItem/LNode: evaluate first as mutable....

// Retrieve pointer to list item by index (from the end)
static Expr **list_item_rev (Expr **p_item, unsigned index) {
Expr *current = *p_item;

if (index == 1) {
	X_List *list;
	while (list = current ? current->isList() : 0) {
		p_item = &list->next;
		current = *p_item;
		}
	return p_item;
	}
else {
Expr *before = 0;

for (;;) {
	unsigned count = index;
	
	Expr *after = current;

	while (count) {
		X_List *list = after ? after->isList() : 0;
		if (list)
			after = list->next;
		else break;

		count --;
		}

	if (count) {		// end of list reached...
		if (before) {
		count = index - count + 1;

		while (count --) {
			X_List *list = before->isList ();
			before = list->next;
			}

		return &(before->isList ()->first);
		}

		else if (count == 1)
			return &((*p_item)->isList ()->first);

		else return 0;
		}
	else {
		before = current;
		current = after;
		}
	}
}
}	// list_item_rev

// Retrieve pointer to list item by index (from the start)
static Expr **list_item (Expr **p_item, unsigned index) {
while (*p_item) {
	X_List *list = (*p_item)->isList();

	if (! index) {
		if (list) return &list->first;
		break;
		}

	if (! list) {
		if (index) return 0;
		break;
		}

	index --;
	p_item = &list->next;
	}	// while (p_item)

return p_item;
}	// list_item

struct P_List_Item : PrefixR {
	P_List_Item (char const *ident, O_Enum op): PrefixR (ident, op) {}

	Expr *&evalR (Expr *args) {
		S_fixed index = expect_fixed (get_arg (args), -1);

		list_eval(args);

		if (args)
			return prot_ref (args,
				index >= 0 ? list_item (&args, index) : list_item_rev (&args, -index));

		relink_expr (args);
		return consR_X (0);		// (no ref)
		}	// evalR
	};

//
// Mutable: either first or next node of the list
//

static Expr **list_node (Expr **p_item, unsigned index, bool tail_head) {
while (*p_item && index --) {
	X_List *list = (*p_item)->isList ();

	if (list)
		p_item = tail_head ? &list->next : &list->first;
	else return 0;
	}	// while (...)

return p_item;
}	// list_node

struct P_List_Node : PrefixR {
	bool indexed;		// (get argument for index)
	bool tail_head;		// ? tail node : head node

	P_List_Node (char const *ident, O_Enum op, bool tail_head, bool indexed): PrefixR (ident, op)
		{ this->indexed = indexed; this->tail_head = tail_head; }

	Expr *&evalR (Expr *args) {
		S_fixed index = indexed ? expect_fixed (get_arg (args), -1) : 1;

		list_eval(args);

		if (args && index >= 0)
			return prot_ref (args, list_node (&args, index, tail_head));

		relink_expr (args);
		return consR_X (0);		// (no ref)
		}	// evalR

	};

//
//	Iterate through list
//

// Evaluate 'body' setting 'var' for all items of 'expr' in forward order
// [Return true on exception interrupt]
static bool list_loop (P_Iterator::IterContext &IC, Expr *&var, Expr *expr) {
while (expr) {
	Expr *first;
	X_List *list = expr->isList();
	if (list)
		{ first = list->first; expr = list->next; }
	else
		{ first = expr; expr = 0; }

	mutateR_X (var, first);
	if (! IC.next ()) return true;
	}	// while (expr)

return false;
}	// list_loop

// Evaluate 'body' setting 'var' for all items of 'expr' in reverse order
// (recursive)
// [Return true on exception interrupt]
static bool list_loop_rev (P_Iterator::IterContext &IC, Expr *&var, Expr *expr) {
if (expr) {
	Expr *first;
	X_List *list = expr->isList();

	if (list)
		{ first = list->first; expr = list->next; }
	else
		{ first = expr; expr = 0; }

	// (tail evaluation)
	if (list_loop_rev (IC, var, expr))
		return true;

	// (current evaluation)
	mutateR_X (var, first);
	return ! IC.next ();
	}	// if (expr)

return false;
}	// list_loop_rev

struct P_List_Loop : P_Iterator {
	bool direction;		// ? reverse order : forward order

	P_List_Loop (char const *ident, bool dir, O_Enum op) : P_Iterator (ident, op)
		{ direction = dir; }

	D_P_Iterator_evaluate;
	};

void P_List_Loop::evaluate (IterContext &IC, Expr *args) {
Expr *&R_loop = expectR_X (get_arg (args));
Expr *list = get_arg (args);
list_eval (list);

if (args) {
	IC.start (args);
	direction ?
		list_loop_rev (IC, R_loop, list):
		list_loop     (IC, R_loop, list);
	}

relink_expr (list);
}	// P_List_Loop::evaluate

//
// TODO:
//	P_List_Mutate -- functional list mutator
//

//
//	Push item(s) to list
//

struct P_List_Push : PrefixX {
	P_List_Push (char const *ident, O_Enum op): PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_List_Push::evalX (Expr *args) {
Expr *&R_list = expectR_X (get_arg (args));
Expr *list = R_list, *opnd;

while (opnd = pop_list (args)) {
	Expr *first = evalX_X (opnd);
	list = list ? new ("List/push") X_List (first, list) : first;
	}

mutateR_X (R_list, list);
return list;
}	// P_List_Push::evalX

//
//	Pop item(s) from list
//

struct P_List_Pop : PrefixX {
	P_List_Pop (char const *ident, O_Enum op): PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_List_Pop::evalX (Expr *args) {
Expr *&R_list = expectR_X (get_arg (args));
Expr *list = R_list, *opnd;

while (opnd = pop_list (args)) {
	X_List *node = list ? list->isList () : 0;
	Expr *first;
	if (node) {
		first = node->first;
		list = node->next;
		}
	else {
		first = list;
		list = 0;
		}

	mutateX_X (opnd, first);
	}

mutateR_X (R_list, list);
return list;
}	// P_List_Pop::evalX

//
//	Split list at selected position
//

Expr *list_split (Expr *&node, unsigned index) {
Expr **p_expr = &node;

// (locate node to make a split at:)

while (*p_expr) {
	X_List *list = (*p_expr)->isList ();

	if (list) {
	if (! index --) {
		*p_expr = list->first;
		list->first = node;
		node = list;
		break;
		}

	p_expr = &list->next;
	}
	else break;
	}
}	// list_split

struct P_List_Split : PrefixR {
	P_List_Split (char const *ident, O_Enum op) : PrefixR (ident, op) {}

	Expr *&evalR (Expr *args) {
	int pos = expect_fixed (get_arg (args), -1);
	Expr *&R_list = expectR_X (args);

	if (pos > 0) list_split (R_list, pos);

	return R_list;
	}	// evalR
	};

//
//	Map list elements applying functor
//

static Expr *list_fmap (Prefix *fn_map, Expr *expr) {
// No exception trapping!
if (expr) {
	X_List *list = expr->isList ();
	Expr *first;

	if (list) {
		first = list->first;
		expr = list_fmap (fn_map, list->next);
		}
	else {
		first = expr;
		expr = 0;
		}

	// Note: terminated list is mapped to terminated list
	if (first) first = fn_map->evalX (first);
	return list ? new ("List/fmap") X_List (first, expr) : first;
	}

return 0;
}	// list_fmap

struct P_List_Map : PrefixX {
	P_List_Map (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		Prefix *fn_map = expect_prefix (get_arg (args));
		list_eval (args);

		Expr *result = fn_map ? list_fmap (fn_map, args) : 0;

		relink_expr (args);
		return result;
		}	// evalX
	};

//
//	Scan list forward/backward (checking predicate)
//

// Scan list forward (checking 'fn_scan' for 'polarity')
// Returns: index of first element which fails test
static unsigned list_fscan (Prefix *fn_scan, bool polarity, Expr *args) {
unsigned index = 0;

// No exception trapping!

while (args) {
	X_List *list = args->isList ();

	if (apply_bool (fn_scan, list ? list->first : args) == polarity)
		break;

	++ index;
	args = list ? list->next : 0;
	}

return index;
}	// list_fscan

// Scan recursively list backward (checking 'fn_scan' for 'polarity')
// Returns: index of last element which fails test
// ('index', if no such element in list)
static unsigned list_fscan_rev (unsigned index,
	Prefix *fn_scan, bool polarity, Expr *args) {
// No exception trapping!

X_List *list = args->isList ();
unsigned result;

++ index;
if (list && (result = list_fscan_rev (index, fn_scan, polarity, list->next)) > index)
		return result;

return apply_bool (fn_scan, list ? list->first : args) == polarity ?
	index : index - 1;
}	// list_fscan_rev

struct P_List_Scan : Prefix {
	bool direction;		// scan direction: forward / backward
	bool polarity;		// test polarity: while / until

	P_List_Scan (char const *ident, O_Enum op, bool direction, bool polarity) :
		Prefix (ident, op) {
			this->direction = direction;
			this->polarity = polarity;
			}

	VType evalV (VDatum &val, Expr *args) {
	Prefix *fn_scan = expect_prefix (get_arg (args));

	list_eval (args);

	if (fn_scan) {
		val._fixed = direction ?
			list_fscan_rev (0, fn_scan, polarity, args) :
			list_fscan (fn_scan, polarity, args);
		return T_fixed;
		}

	return T_undef;
	}	// evalV
	};

//
//	Filter list (checking predicate)
//

// Filter list by 'fn_filter' predicate
// Returns: result of list filtration
static Expr *list_filter (Prefix *fn_filter, bool polarity, Expr *args) {
// No exception trapping!

X_List *list = args ? args->isList () : 0;
Expr *tail;
if (list) {
	args = list->first;
	tail = list_filter (fn_filter, polarity, list->next);
	}
else
	tail = 0;

return
	apply_bool (fn_filter, args) == polarity ?
		(tail ? new ("List/filter") X_List (args, tail) : args) :
	tail;
}	// list_filter

struct P_List_Filter : PrefixX {
	bool polarity;		// test polarity: if / unless

	P_List_Filter (char const *ident, O_Enum op, bool polarity) :
		PrefixX (ident, op) { this->polarity = polarity; }

	Expr *evalX (Expr *args) {
	Prefix *fn_filter = expect_prefix (get_arg (args));

	list_eval (args);

	if (fn_filter) {
		Expr *result = list_filter (fn_filter, polarity, args);
		relink_expr (args);
		return result;
		}
	else
		return args;
	}	// evalX
	};

//
//	Count list items (to match predicate)
//

// Count items in list
static unsigned list_count (Prefix *fn_count, bool polarity, Expr *args) {
unsigned count = 0;

// No exception trapping!

while (args) {
	X_List *list = args->isList ();

	if (apply_bool (fn_count, list ? list->first : args) == polarity)
		++ count;

	args = list ? list->next : 0;
	}

return count;
}	// list_count

struct P_List_Count : Prefix {
	bool polarity;		// test polarity: if / unless

	P_List_Count (char const *ident, O_Enum op, bool polarity) : Prefix (ident, op)
		{ this->polarity = polarity; }

	VType evalV (VDatum &val, Expr *args) {
		Prefix *fn_count = expect_prefix (get_arg (args));

		list_eval (args);

		val._fixed = fn_count ? list_count (fn_count, polarity, args) : 0;

		relink_expr (args);

		return T_fixed;
		}	// evalV
	};

//
//	Create list from integral range
//

static Expr *list_range (Prefix *fn_map, bool dir, FixedRange range) {
Expr *list = 0;

// No exception trapping!

while (range.notempty ()) {
	Expr *item = new ("Fixed/range") X_Fixed (dir ? range.from ++ : -- range.to);
	if (fn_map) item = fn_map->evalX (item);
	list = list ? new ("List/range") X_List (item, list) : item;
	}

return list;
}	// list_range

static Expr *list_open_range (Prefix *fn_map, bool dir, FixedRange range) {
Expr *list = 0;

// No exception trapping!

while (range.notempty ()) {
	Expr *item = new ("Fixed/range") X_Fixed (dir ? range.from ++ : -- range.to);
	if (fn_map) item = fn_map->evalX (item);
	list = new ("List/range") X_List (item, list);
	}

return list;
}	// list_open_range

struct P_List_Range : PrefixX {
	bool dir;			// (direction ? decending / ascending)
	bool open;

	P_List_Range (char const *ident, O_Enum op, bool dir, bool open): PrefixX (ident, op)
		{ this->dir = dir; this->open = open; }

	Expr *evalX (Expr *args) {
		FixedRange range (0, 0);
		expect_range (get_arg (args), range);
		Prefix *fn_map = args ? expect_prefix (args) : 0;

		Expr *result = open ?
			list_open_range (fn_map, dir, range) :
			list_range (fn_map, dir, range);

		relink_expr (args);
		return result;
		}	// evalX
	};

//
//	Compare lists (with 'comparer')
//

static int list_compare (Prefix *comparer, Expr *left, Expr *right) {
int result;
X_List *l_node = new ("List/compare") X_List (0, 0);
link_expr (l_node);

// No exception trapping!

while (left && right) {
	X_List *l_left = left->isList(), *l_right = right->isList();
	VDatum _val;

	// (left list node)
	unlink_expr (l_node->first);
	if (l_left) {
		l_node->first = link_expr(l_left->first);
		left = l_left->next;
		}
	else { l_node->first = link_expr(left); left = 0; }

	// (right list node)
	unlink_expr (l_node->next);
	if (l_right) {
		l_node->next = link_expr(l_right->first);
		right = l_right->next;
		}
	else { l_node->next = link_expr(right); right = 0; }

	if (comparer->evalV (_val, l_node) == T_fixed) {
		if (_val._fixed) {
			result = _val._fixed;
			goto done;
			}
		}
	else break;
	}	// while (left && right)

result = left ? 1 : right ? -1 : 0;

done: unlink_expr (l_node);
return result;
}	// list_compare

struct P_List_Compare : Prefix {
	P_List_Compare (char const *ident, O_Enum op): Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		Prefix *comparer = expect_prefix (get_arg (args));
		Expr *first = get_arg (args);

		list_eval (first);
		list_eval (args);

		if (comparer)
			val._fixed = list_compare (comparer, first, args);

		relink_expr (first);
		relink_expr (args);

		return comparer ? T_fixed : T_undef;
		}	// evalV
	};

//
//	Zip lists
//

static Expr *list_zip (Expr *left, Expr *right) {
if (left || right) {
X_List *l_node = new ("List/zip") X_List (0, 0);

if (left) {
	X_List *l_left = left->isList();
	if (l_left) {
		l_node->first = link_expr(l_left->first);
		left = l_left->next;
		}
	else { l_node->first = link_expr(left); left = 0; }
	}	// (left)

if (right) {
	X_List *l_right = right->isList();
	if (l_right) {
		l_node->next = link_expr(l_right->first);
		right = l_right->next;
		}
	else { l_node->next = link_expr(right); right = 0; }
	}	// (right)

return new ("List/zip")
	X_List (l_node, list_zip (left, right));
}	// (left || right)

return 0;
}	// list_zip

struct P_List_Zip : PrefixX {
	P_List_Zip (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		Expr *first = get_arg (args);

		list_eval (first);
		list_eval (args);

		Expr *result = list_zip (first, args);

		relink_expr (first);
		relink_expr (args);

		return result;
		}	// evalX
	};

//
//	Expand/shrink list
//

static void list_resize (Expr *&refval, unsigned size) {

Expr **p_tail = &refval;

while (size) {
	X_List *list;

	if (*p_tail && (list = (*p_tail)->isList ())) {
		p_tail = &list->next;
		}
	else break;

	size --;
	}	// while (size)

if (! size)
	mutateR_X (*p_tail, 0);			// list truncate

else {								// list expand
	Expr *tail = 0;
	while (-- size) tail = new ("List/resize") X_List (0, tail);
	mutateR_X (*p_tail, new ("List/resize") X_List (*p_tail, tail));
	}
}	// list_resize

struct P_List_Resize : PrefixX {
	P_List_Resize (char const *ident, O_Enum op): PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		Expr *&refval = expectR_X (get_arg (args));
		S_fixed total = expect_fixed (args, 0);

		list_resize (refval, total);
		return refval;
		}	// evalX
	};

#define	HEADER

#include "Sortable.cpp"

#undef HEADER

//
//	Generate sorted index
//

// Sortable indexer
struct Indexer : Sortable {
	S_fixed *array;
	Prefix *comparer;
	Expr *args;
	X_Fixed *left, *right;

	// Constructor
	Indexer (S_fixed *array, S_fixed start, S_fixed end, Prefix *comparer) :
		Sortable (end - start) {
		this->array = array;
		link_prefix (this->comparer = comparer);
		args = link_expr (new ("List/Indexer") X_List
			(left = new ("Fixed/Indexer") X_Fixed (0),
			right = new ("Fixed/Indexer") X_Fixed (0)
			)
		);
	}	// Indexer

	// Destructor
	~Indexer () { unlink_expr (args); unlink_prefix (comparer); }

	// Compare: [I] <?> [J]
	int compare (unsigned I, unsigned J) {
		left->value = array [I], right->value = array [J];
		VDatum val;
		return (comparer->evalV (val, args) == T_fixed) ? val._fixed : 0;
		}	// compare

	// Swap: [I] :=: [J]
	void swap (unsigned I, unsigned J) {
		unsigned temp = array [I];
		array [I] = array [J];
		array [J] = temp;
		}	// swap
	};

Expr *list_sort_index (S_fixed start, S_fixed end, Prefix *comparer) {
if (start < end) {
	unsigned i;
	S_fixed *temp_arr = new ("<temp>") S_fixed [end - start];
	for (i = start; i != end; ++ i) temp_arr [i - start] = i;

	// Do sorting quickly...
	if (comparer) Indexer (temp_arr, start, end, comparer).sort ();

	// Construct index list...
	Expr *list = 0;
	i = end - start;
	do {	Expr *elem = new ("Fixed/sort") X_Fixed (temp_arr [-- i]);
			list = list ? new ("List/sort") X_List (elem, list) : elem;
	} while (i);

	delete [] temp_arr;

	return list;
	}

return 0;
}	// list_sort_index

struct P_List_SortIndex : PrefixX {
	P_List_SortIndex (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	D_Prefix_evalX;
	};

Expr *P_List_SortIndex::evalX (Expr *args) {
Prefix *comparer = expect_prefix (get_arg (args));
FixedRange range (0, 0);
expect_range (args, range);
Expr *result = list_sort_index (range.from, range.to, comparer);

relink_prefix (comparer);

return result;
}	// P_List_SortIndex::evalX

// Sortable reorderer
struct Reorderer : Sortable {
	Expr *** reflist;

	Prefix *comparer;
	X_List *args;

	// (constructor)
	Reorderer (unsigned count, Prefix *accessor, Prefix *comparer) :
		Sortable (count) {
		link_prefix (this->comparer = comparer);

		link_expr (args = new X_List (0, 0));

		// allocate reflist storage
		reflist = new ("Reorder") Expr ** [count];

		// load ref list
		X_Fixed *arg = new X_Fixed (-1);
		link_expr (arg);
		link_prefix (accessor);

		for (unsigned i = 0; i != count; ++ i) {
			arg->value = i;
			link_expr (* (reflist [i] = &accessor->evalR (arg)));
			}

		unlink_prefix (accessor);
		unlink_expr (arg);
		}	// constructor

	// Compare: [I] <?> [J]
	int compare (unsigned I, unsigned J) {
		args->first = * reflist[I];
		args->next = * reflist[J];

		VDatum val;
		S_fixed result = (comparer->evalV (val, args) == T_fixed) ? val._fixed : 0;

		args->first = args->next = 0;
		return result;
		}	// compare

	// Swap: [I] :=: [J]
	void swap (unsigned I, unsigned J) {
		Expr *temp = * reflist[I];
		* reflist[I] = * reflist[J];
		* reflist[J] = temp;
		}	// swap

	// Destructor
	~Reorderer () {
		for (unsigned i = 0; i != N; ++ i)
			unlink_expr (* reflist [i]);

		delete [] reflist;

		unlink_prefix (comparer);
		unlink_expr (args);
		}
	};

struct P_List_SortMut : PrefixX {
	P_List_SortMut (char const *ident, O_Enum op) : PrefixX (ident, op) {}

	Expr *evalX (Expr *args) {
		S_fixed count = expect_fixed (get_arg (args), 0);
		Prefix *accessor = expect_prefix (get_arg (args));
		Prefix *comparer = expect_prefix (args);

		Reorderer (count, accessor, comparer).sort ();

		return 0;
		}	// evalX
	};

//
//	To implement: l_(bi|tri)map
//

//
//	Operation table
//

static bool init_primaries_list (int order) {

//		[Categories]

//^C	List
//^B	List functors
//^D	Functors, operating on lists.

//		[Types]

//^T	List
//^B	List value.
//^D	Anything evaluating to list (allowing all atomic values and !undef, as trivial lists).

//^T	Range
//^B	Integer range.
//^D	Range of integers, confined in boundary list (\Start, \End).
//^D	Range includes values, for which \Start <= \value && \value < \End (or is empty, if \Start >= \End).
//^D	(If single scalar value \N is supplied, range 0..\N is normally assumed.)

//		--------

//^N	is_undef [Predicate]
//^P	is_undef (V: Any) => Bool
//^B	Check for undefined value.
//^D	Predicate: !true, if argument \V evaluates to !undef.

//^N	is_list [Predicate | List]
//^P	is_list (V: Any) => Bool
//^B	Check for list.
//^D	Predicate: !true, if argument \V evaluates to list.

	DefBuiltin (P_IsType ("is_undef", Op_Null, T_undef));
	DefBuiltin (P_IsType ("is_list", Op_Null, T_list));

//^N	expect_list [Wrapper | List]
//^P	expect_list (V: Any, @Body: Any) => Any
//^B	Expect list value.
//^D	If argument \V evaluates to list, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_list", Op_Null, T_list));

//^G	l_len l_ref

//^N	l_len [List]
//^P	l_len (L: List) => Int
//^B	Length of list.
//^D	Returns length of list \L (total # of list items).
//^D	(Returns 1 for atomic values and 0 for !undef.)
//^D	Syntax: #\L.

	DefBuiltin (P_List_Length ("l_len", Op_LLength));

//^N	l_ref [List]
//^P	l_ref (L: List) => List
//^B	List reference.
//^D	Return reference to \L (evaluated as list).

	DefBuiltin (P_List_Ref ("l_ref", Op_LRef));

//^G	l_copy l_cat l_rep l_rev

//^N	l_copy [List]
//^P	l_copy (L: List) => List
//^B	List copying.
//^D	Returns exact copy of list \L (copying all list elements, including nested lists).

	DefBuiltin (P_List_Clone ("l_copy", Op_LCopy));

//^N	l_cat [List]
//^P	l_cat (L: List, M: List) => List
//^B	List concatenation.
//^D	Returns concatenation of \L and \M, evaluated as lists.
//^D	(Returns copy of \L, if \M is !undef; returns \M, if \L is !undef.)
//^D	(If \M is open list, result is open too.)
//^D	Syntax: \L [+] \M.

	DefBuiltin (P_List_Cat ("l_cat", Op_LCat));

//^N	l_rep [List]
//^P	l_rep (N: Int, L: List) => List
//^B	List replication.
//^D	Returns replication of \L (evaluated as list) N times in succession.
//^D	(Returns !undef, if \N <= 0; returns \L , if \N == 1.)
//^D	(If \L is open list, result is open too.)
//^D	Syntax: \L [*] \N.

	DefBuiltin (P_List_Rep ("l_rep", Op_LRep));

//^N	l_rev [List]
//^P	l_rev (L: List) => List
//^B	List reversion.
//^D	Returns reversion, from last item to first, of \L, evaluated as list.
//^D	Syntax: [~] \L.

	DefBuiltin (P_List_Rev ("l_rev", Op_LRev));

//^G	l_item

//^N	l_item [List | Mutable]
//^P	l_item (I: Int, L: List) => Mutable
//^B	List element accessor.
//^D	Returns mutable reference to item \I (starting at 0) of \L, evaluated as list.
//^D	Syntax: \L [\I].

	DefBuiltin (P_List_Item ("l_item", Op_LItem));

//^G	l_head l_tail l_head_by l_tail_by

//^N	l_head [List | Mutable]
//^P	l_head (L: List) => Mutable
//^B	List head accessor.
//^D	Returns mutable reference to head (first item) of list \L.
//^D	(Equivalent to !l_item (0, \L)).

//^N	l_tail [List | Mutable]
//^P	l_tail (L: List) => Mutable
//^B	List tail accessor.
//^D	Returns mutable reference to tail (all items except first) of list \L.

	DefBuiltin (P_List_Node ("l_head", Op_LHead, false, false));
	DefBuiltin (P_List_Node ("l_tail", Op_LTail, true, false));

//^N	l_head_by [List | Mutable]
//^P	l_head_by (N: Int, L: List) => Mutable
//^B	Head of list, with offset.
//^D	Equivalent of !l_head(L) applied \N times.
//^D	(Returns \L, if \N == 0; equivalent to !l_head(L), if \N == 1.)

//^N	l_tail_by [List | Mutable]
//^P	l_tail_by (N: Int, L: List) => Mutable
//^B	Tail of list, with offset.
//^D	Equivalent of !l_tail(L) applied \N times.
//^D	(Returns \L, if \N == 0; equivalent to !l_tail(L), if \N == 1.)

	DefBuiltin (P_List_Node ("l_head_by", Op_LHeadBy, false, true));
	DefBuiltin (P_List_Node ("l_tail_by", Op_LTailBy, true, true));

//^G	l_push l_pop l_split

//^N	l_split [List | Mutator]
//^P	l_split (N: Int, L: Mutable) => List
//^B	Split list at specified position.
//^D	Split list \L in two parts at position \N (modifying \L).
//^D	For new value of \L:
//^\	!l_head(L) [+] !l_tail(L) [==] L; !l_len(!l_head(L)) == N+1.
//^D	Returns: new value of \L.

	DefBuiltin (P_List_Split ("l_split", Op_LSplit));

//^N	l_push [List | Mutator]
//^P	l_push (L: Mutable, V: Any) => List
//^B	Push elements to list.
//^D	Evaluates elements of list \V, and inserts them (in reverse order) into list \L.
//^D	Returns: new value of \L.
//^D	Syntax: \L [<-] \V.

//^N	l_pop [List | Mutator]
//^P	l_pop (L: Mutable, V: Mutable) => List
//^B	Pop elements from list.
//^D	Removes (in direct order) elements from list \L into list of mutables \V.
//^D	Returns: new value of \L.
//^D	Syntax: \L [->] \V.

	DefBuiltin (P_List_Push ("l_push", Op_LPush));
	DefBuiltin (P_List_Pop ("l_pop", Op_LPop));

//^G	l_loop l_loop_r

//^N	l_loop [List | Iterator]
//^P	l_loop (V: Mutable, L: List, @Body: Any) => Any
//^B	Direct order list iterator.
//^D	Evaluates \Body for all items in list \L (from first to last).
//^D	On each iteration, reference to current list item is assigned to mutable \V.
//^D	Returns result of last evaluation of \Body.

//^N	l_loop_r [List | Iterator]
//^P	l_loop_r (V: Mutable, L: List, @Body: Any) => Any
//^B	Reverse order list iterator.
//^D	Evaluates \Body for all items in list \L (from last to first).
//^D	On each iteration, reference to current list item is assigned to mutable \V.
//^D	Returns result of last evaluation of \Body.

	DefBuiltin (P_List_Loop ("l_loop", false, Op_LLoop));
	DefBuiltin (P_List_Loop ("l_loop_r", true, Op_LLoopR));

//^G	l_range l_range_r

//^N	l_range [List]
//^P	l_range (R: Range, [F: Func]) => List
//^B	Convert range to incremental list.
//^D	Constructs and returns new list, containing all integers from range \R, in ascending order from first to last.
//^D	If optional functor \F is specified, it is applied to elements of resulting list.

//^N	l_range_r [List]
//^P	l_range_r (R: Range, [F: Func]) => List
//^B	Convert range to decremental list.
//^D	Constructs and returns new list, containing all integers from range \R, in descending order from last to first.
//^D	If optional functor \F is specified, it is applied to elements of resulting list.

	DefBuiltin (P_List_Range ("l_range", Op_Null, false, false));
	DefBuiltin (P_List_Range ("l_range_r", Op_Null, true, false));

// TODO

	DefBuiltin (P_List_Range ("l_open_range", Op_Null, false, true));
	DefBuiltin (P_List_Range ("l_open_range_r", Op_Null, true, true));


//^G	l_map

//^N	l_map [List | Func]
//^P	l_map (F: Func, L: List) => List
//^B	Functional list mapping.
//^D	Returns new list, constructed by applying functor \F to all elements of list \L.
//^D	Note: !l_map maps open lists to open lists.

	DefBuiltin (P_List_Map ("l_map", Op_Null));

//^G	l_while l_until l_while_r l_until_r

//^N	l_while [List | Func]
//^P	l_while (Predicate: Func, L: List) => Int
//^B	Search list forward, positively testing predicate.
//^D	Scans list \L (from first element to last), applying functor \Predicate to each element, while result remains !true.
//^D	Returns: index of first element, where \Predicate ! \L[index] becomes !false
//^D	(returns 0, if condition is !false on first element; returns length of \L, if condition is !true for all elements).

//^N	l_until [List | Func]
//^P	l_until (Predicate: Func, L: List) => Int
//^B	Search list forward, negatively testing predicate.
//^D	Scans list \L (from first element to last), applying functor \Predicate to each element, while result remains !false.
//^D	Returns: index of first element, where \Predicate ! \L[index] becomes !true
//^D	(returns 0, if condition is !true on first element; returns length of \L, if condition is !false for all elements).

	DefBuiltin (P_List_Scan ("l_while", Op_Null, false, false));
	DefBuiltin (P_List_Scan ("l_until", Op_Null, false, true));

//^N	l_while_r [List | Func]
//^P	l_while_r (Predicate: Func, L: List) => Int
//^B	Search list backward, positively testing predicate.
//^D	Scans list \L (from last element to first), applying functor \Predicate to each element, while result remains !true.
//^D	Returns: index of last element, where \Predicate ! \L[index] remains !true
//^D	(returns length of \L, if condition is !false on last element; returns 0, if condition is !true for all elements).

//^N	l_until_r [List | Func]
//^P	l_until_r (Predicate: Func, L: List) => Int
//^B	Search list backward, negatively testing predicate.
//^D	Scans list \L (from last element to first), applying functor \Predicate to each element, while result remains !false.
//^D	Returns: index of last element, where \Predicate ! \L[index] remains !false
//^D	(returns length of \L, if condition is !true on last element; returns 0, if condition is !false for all elements).

	DefBuiltin (P_List_Scan ("l_while_r", Op_Null, true, false));
	DefBuiltin (P_List_Scan ("l_until_r", Op_Null, true, true));

//^G	l_filter_in l_filter_ex

//^N	l_filter_in [List | Func]
//^P	l_filter_in (Predicate: Func, L: List) => List
//^B	Filter list inclusively.
//^D	Creates and returns new list, containing all elements of \List, for which application of \Predicate evaluates to !true.

//^N	l_filter_ex [List | Func]
//^P	l_filter_ex (Predicate: Func, L: List) => List
//^B	Filter list exclusively.
//^D	Creates and returns new list, containing all elements of \List, for which application of \Predicate evaluates to !false.

	DefBuiltin (P_List_Filter ("l_filter_in", Op_Null, true));
	DefBuiltin (P_List_Filter ("l_filter_ex", Op_Null, false));

//^G	l_count_in l_count_ex

//^N	l_count_in [List | Func]
//^P	l_count_in (Predicate: Func, L: List) => List
//^B	Count list items inclusively.
//^D	Returns total count of elements of \List, for which application of \Predicate evaluates to !true.

//^N	l_count_ex [List | Func]
//^P	l_count_ex (Predicate: Func, L: List) => List
//^B	Count list items exclusively.
//^D	Returns total count of elements of \List, for which application of \Predicate evaluates to !false.

	DefBuiltin (P_List_Count ("l_count_in", Op_Null, true));
	DefBuiltin (P_List_Count ("l_count_ex", Op_Null, false));

//^G	l_sort_index

//^N	l_sort_index [List | Func]
//^P	l_sort_index (Comp: Func, R: Range) => List
//^B	Build sorted index.
//^D	Sort range \R, using comparator \Comp to compare pairs of indexes, belonging to \R.
//^D	Returns list of indexes, ordered according to \Comp.

	DefBuiltin (P_List_SortIndex ("l_sort_index", Op_Null));

//^G	l_cmp

//^N	l_cmp [List | Func]
//^P	l_cmp (Compare: Func, L: List, M: List) => Int
//^B	Compare lists of ordered elements.
//^D	Compares lists \L and \M, applying \Compare to each pair of elements from \L and \M.
//^D	Returns result of first non-zero comparison (or 0, if all elements compare to 0).

	DefBuiltin (P_List_Compare ("l_cmp", Op_Null));

//^G	l_zip

//^N	l_zip [List]
//^P	l_zip (L: List, M: List) => List
//^B	Zip lists.
//^D	Returns list of combined list pairs of elements from both \L and \M.

	DefBuiltin (P_List_Zip ("l_zip", Op_Null));

//^N	l_resize [List | Mutator]
//^P	l_resize (L: List, N: Int) => List
//^B	Resize list.
//^D	Shrink or expand list \L to length \N.

	DefBuiltin (P_List_Resize ("l_resize", Op_Null));

//^N	l_sort_mutator [List | Mutator]
//^P	l_sort_mutator (Count: Int, Accessor: Func, Comparator: Func) => ()
//^B	Sorting in place.
//^D	Sort any set of \Count mutables (with access provided by \Accessor, and comparisons done by \Comparator).

	DefBuiltin (P_List_SortMut ("l_sort_mutator", Op_Null));

return true;
}	// init_primaries_list

DefSubSystem ("list", init_primaries_list, 0);

