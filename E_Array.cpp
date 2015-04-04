
/*

	+---+---+---+---+---+---+
	|	"E_Array.cpp":
	|	Implementation of arrays.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#ifndef HEADER

#include "Eval.h"

#endif

struct X_Array : Expr {
	// Maximum array rank allowed
	enum { MaxRank = 16 };

	unsigned rank;			// Array rank (# of dimensions)
	unsigned *dims;			// Vector of array dimensions (outer -> inner)
	Expr **elems;			// Vector of array elements

	// Default constructor
	X_Array (unsigned rank, unsigned dimensions []);

	// Uninitialized constructor
	X_Array ();

	// Calculate total # of elements
	unsigned calc_total ();

	// Map array (optionally applying functor 'func')
	X_Array *copy_fn (Prefix *func);

	// Load array with 'list' of elements
	void load (Expr *list);
	
	// Save array contents to 'list'
	Expr *save ();

	// Fill array with 'filler'
	void fill (Expr *filler);

	// Mutable reference to array element
	Expr *&elem (unsigned count, unsigned index []);

	// (Recursive helper function for 'init apply')
	Expr **init_apply_rec (Prefix *func, unsigned dim_index, Expr **elem_ptr,
		Expr *index_list, Expr *index_node);

	// Init array applying 'func'
	void init_apply (Prefix *func);

	// Insert new elements (initialised with 'init') at 'index_range' on 'dimension'
	void insert_range (unsigned dimension, FixedRange index_range, Expr *fill);

	// Delete elements at 'index_range' on 'dimension'
	void delete_range (unsigned dimension, FixedRange index_range);

	// Reshape range (as by 'new_range_list')
	void reshape_range (FixedRange *new_range_list, Expr *fill);

	// Reverse range 'rev_range' of array 'dimension'
	void reverse_dimension (unsigned dimension, FixedRange &rev_range);

	// Rotate range 'rot_range' of array 'dimension' by 'shift'
	void rotate_dimension (unsigned dimension, int shift, FixedRange &rot_range);

	D_Expr_put;
	D_Expr_evalV;
	D_Expr_identV;
	D_Expr_hash;
	D_Expr_log;
	D_Expr_release;
	};

#ifndef HEADER

#include "Logger.h"

// Get list of dimensions 'index' (not more than 'limit')
// Returns: actual # of dimensions
static unsigned get_index (unsigned limit, unsigned dims [], Expr *args, Prefix *where) {
unsigned count = 0;

while (args && count != limit) {
	S_fixed val = where->expect_fixed (count + 1 != limit ? get_arg (args) : args, -1);
	dims [count ++] = val > 0 ? val : 0;
	}	// (while)

return count;
}	// get_index

// Trivial array constructor
X_Array::X_Array () {
rank = 0;
dims = 0;
elems = 0;
}	// X_Array::X_Array

// Array constructor
// (from list of dimension arguments)
X_Array::X_Array (unsigned rank, unsigned dimensions []) {
unsigned *dim_p;
Expr **elems;

unsigned total = 1;

this->dims = dim_p = new ("Array/dims") unsigned [this->rank = rank];
while (rank --)
	total *= (dim_p[rank] = dimensions[rank]);

this->elems = elems = total ? new ("Array/elems") Expr * [total] : 0;
while (total --) *elems ++ = 0;
}	// X_Array::X_Array

// Map array (optionally applying functor 'func')
X_Array *X_Array::copy_fn (Prefix *func) {
X_Array *result = new ("Array/copy") X_Array ();

unsigned rank_c = rank;
unsigned *d_dims, *s_dims = dims;
result->dims = d_dims = new ("Array/dims") unsigned [result->rank = rank_c];

unsigned total = 1;
while (rank_c --)
	total *= (*d_dims ++ = *s_dims ++);

// TODO: apply functor 'func' to array elements

Expr **d_elems, **s_elems = elems;
result->elems = d_elems = total ? new ("Array/elems") Expr * [total] : 0;
while (total --)
	*d_elems ++ = link_expr (*s_elems ++);

return result;
}	// X_Array::copy_fn

// Calculate array total # of elements
unsigned X_Array::calc_total () {
unsigned total = 1, rank = this->rank;
unsigned *dim_p = dims;
while (rank --) total *= *dim_p ++;
return total;
}	// X_Array::calc_total

// Load array with elements list
void X_Array::load (Expr *list) {
unsigned total = calc_total ();

Expr **elems = this->elems;
Expr *elem;
while (total -- && (elem = get_arg (list)))
	mutateR_X (*elems ++, elem);
}	// X_Array::load

// Save array contents to list
Expr *X_Array::save () {
unsigned total = calc_total ();
Expr *list = 0;

if (total) {
	Expr **elems = this->elems + total;
	while (total --) {
		Expr *elem = * -- elems;
		list = list ? new ("List/asave") X_List (elem, list) : elem;
		}
	}

return list;
}	// X_Array::save

// Fill array with element
void X_Array::fill (Expr *filler) {
unsigned total = calc_total ();
Expr **elems = this->elems;

while (total --)
	mutateR_X (*elems ++, filler);
}	// X_Array::fill

// Get access to array element
Expr *&X_Array::elem (unsigned count, unsigned index []) {
unsigned offset = 0;

for (unsigned i = 0; i != count; ++ i) {
	unsigned dimension = dims[i];
	unsigned at = index[i];
	if (at < dimension)
		offset = at + offset * dimension;
	else return R_null;
	}

return elems[offset];
}	// X_Array::elem

// Calculate hash code of array
unsigned X_Array::hash () {
unsigned value = 0;

if (elems) {
	unsigned total = calc_total ();

	Expr **elem_p = elems;
	while (total --) value ^= hashX (*elem_p ++);
	}

return value;
}	// X_Array::hash

// Check arrays for identity
bool X_Array::identV (VType type, VDatum &val) {
if (type == T_array) {
	X_Array *array = val._array;
	
	if (array == this) return true;
	
	unsigned rank = array->rank;
	if (rank != this->rank) return false;
	
	unsigned total = 1;
	while (rank --) {
		unsigned size = array->dims[rank];
		if (size != dims[rank]) return false;
		total *= size;
		}
	
	Expr **elems_p = array->elems + total;
	while (total --)
		if (! identX (* -- elems_p, elems[total]))
			return false;

	// (all tests passed)
	return true;
	}
}	// X_Array::identV

VType X_Array::evalV (VDatum &val, bool full) {
val._array = this;
return T_array;
}	// X_Array::evalV

//
// Log 'array'
//

static Expr **log_array_rec (Logger &log, unsigned r_rank, unsigned *p_dims, Expr **p_elems) {
if (r_rank) {
	unsigned total = *p_dims ++;
	-- r_rank;

	log.put_ch ('[');
	while (total --) {
		p_elems = log_array_rec (log, r_rank, p_dims, p_elems);
		if (total) log.put_cstr (", ");
		}
	log.put_ch (']');
	}
else
	log.log_expr (*p_elems ++);

return p_elems;
}	// log_array_rec

Logger *Logger::log_array (X_Array *array) {
unsigned rank = array->rank;
unsigned *dims = array->dims;

put_ch ('[');
while (rank --) {
	log_fixed (* dims ++);
	if (rank) put_ch (' ');
	}
put_ch (']');

if (Module::current->flags & OF_tr_full) {
	put_cstr (" = ");
	log_array_rec (*this, array->rank, array->dims, array->elems);
	}

return this;
}	// Logger::log_array

void X_Array::log (Logger &log) {
log.log_array (this);
}	// X_Array::log

void X_Array::release () {
if (elems) {
	unsigned total = calc_total ();
	Expr **elem_p = elems;
	while (total --) unlink_expr (*elem_p ++);

	delete [] elems;
	}

if (dims) delete [] dims;

Expr::release ();
}	// X_Array::release

unsigned X_Array::put (Stream *out) {
if (elems) {
	unsigned count = 0;
	Expr **elem_p = elems;
	unsigned total = calc_total ();
	while (total --)
		count += put_expr (out, *elem_p ++);
	return count;
	}

return 0;
}	// X_Array::put

//
//	Array block internals
//

// Overwrite expression 'dest' with 'value'
void overwrite_expr (Expr **dest, Expr *value) {
unlink_expr (*dest);
link_expr (*dest = value);
}	// overwrite_expr

//	{INTERNAL}:
//	Array block fill
static void array_block_fill (unsigned remain,
	Expr **dst_ptr, unsigned dst_total, unsigned *dst_dims, FixedRange *dst_range,
	Expr *value, bool overwrite) {
S_fixed start;
unsigned count;

(dst_range ++)->get_ext (start, count);
dst_total /= *dst_dims ++;

if (-- remain) {
	dst_ptr += dst_total * start;
	while (count --) {
		array_block_fill (remain, dst_ptr, dst_total, dst_dims, dst_range, value, overwrite);
		dst_ptr += dst_total;
		}
	}
else if (dst_total != 1) {
	dst_ptr += dst_total * start;
	if (overwrite)
		while (count --) {
			overwrite_expr (dst_ptr, value);
			dst_ptr += dst_total;
			}
	else
		while (count --) {
			link_expr (*dst_ptr = value);
			dst_ptr += dst_total;
			}
	}
else {		// (dst_total == 1)
	dst_ptr += start;
	if (overwrite)
		while (count --)
			overwrite_expr (dst_ptr ++, value);
	else
		while (count --)
			link_expr (*dst_ptr ++ = value);
	}
}	// array_block_fill

//	{INTERNAL}:
//	Array block release
static void array_block_release (unsigned remain,
	Expr **dst_ptr, unsigned dst_total, unsigned *dst_dims, FixedRange *dst_range) {
S_fixed start;
unsigned count;

(dst_range++)->get_ext (start, count);
dst_total /= *dst_dims ++;

if (-- remain) {
	dst_ptr += dst_total * start;
	while (count --) {
		array_block_release (remain, dst_ptr, dst_total, dst_dims, dst_range);
		dst_ptr += dst_total;
		}
	}
else if (dst_total != 1) {
	dst_ptr += dst_total * start;
	while (count --) {
		unlink_expr (*dst_ptr);
		dst_ptr += dst_total;
		}
	}
else {		// (dst_total == 1)
	dst_ptr += start;
	while (count --)
		unlink_expr (*dst_ptr ++);
	}
}	// array_block_release

//	{INTERNAL}:
//	Array block copy
static void array_block_copy (unsigned remain,
	Expr **dst_ptr, unsigned dst_total, unsigned *dst_dims, FixedRange *dst_range,
	Expr **src_ptr, unsigned src_total, unsigned *src_dims, S_fixed *src_index,
	bool overwrite) {

S_fixed src_start = *src_index++, dst_start;
unsigned count;

(dst_range++)->get_ext (dst_start, count);

src_ptr += (src_total /= *src_dims ++) * src_start;
dst_ptr += (dst_total /= *dst_dims ++) * dst_start;

if (-- remain) {
	while (count --) {
		array_block_copy (remain, dst_ptr, dst_total, dst_dims, dst_range,
			src_ptr, src_total, src_dims, src_index, overwrite);
		dst_ptr += dst_total;
		src_ptr += src_total;
		}
	}
else if (dst_total != 1) {
	if (overwrite)
		while (count --) {
			overwrite_expr (dst_ptr, *src_ptr);
			dst_ptr += dst_total;
			src_ptr += src_total;
			}
	else
		while (count --) {
			// Note: no 'link_expr' ...
			*dst_ptr = *src_ptr;
			dst_ptr += dst_total;
			src_ptr += src_total;
			}
	}
else {		// (dst_total == 1)
	if (src_total != 1) {
		if (overwrite)
			while (count --) {
				overwrite_expr (dst_ptr ++, *src_ptr);
				src_ptr += src_total;
				}
		else
			while (count --) {
				// Note: no 'link_expr' ...
				*dst_ptr ++ = *src_ptr;
				src_ptr += src_total;
				}
		}
	else {
		if (overwrite)
			while (count --)
				overwrite_expr (dst_ptr ++, *src_ptr ++);
		else
			while (count --)
				// Note: no 'link_expr' ...
				*dst_ptr ++ = *src_ptr ++;
		}
	}
}	// array_block_copy

//
//	P_Array_XFill
//

struct P_Array_XFill : Prefix {
	P_Array_XFill (char const *ident) : Prefix (ident, Op_Null) {}

	D_Prefix_evalV;
	};

VType P_Array_XFill::evalV (VDatum &val, Expr *args) {
X_Array *array = expect_array (get_arg (args));
Expr *value = evalX_X (get_arg (args));

if (array) {
	unsigned rank = array->rank;
	FixedRange *range_list = new ("Ranges/tmp") FixedRange [rank];
	for (unsigned index = 0; index != rank; ++ index)
		expect_range (get_arg (args), range_list[index]);

	array_block_fill (rank, array->elems, array->calc_total(), array->dims, range_list, value, true);

	delete [] range_list;
	}

return T_undef;
}	// P_Array_XFill::evalV

//
//	P_Array_XCopy
//

struct P_Array_XCopy : Prefix {
	P_Array_XCopy (char const *ident) : Prefix (ident, Op_Null) {}

	D_Prefix_evalV;
	};

VType P_Array_XCopy::evalV (VDatum &val, Expr *args) {
X_Array *dst_array = expect_array (get_arg (args));
X_Array *src_array = expect_array (get_arg (args));

if (dst_array && src_array) {
	unsigned rank = dst_array->rank;
	S_fixed *index_list = new ("Index/tmp") S_fixed [rank];
	FixedRange *range_list = new ("Ranges/tmp") FixedRange [rank];

	for (unsigned index = 0; index != rank; ++ index) {
		Expr *arg = get_arg (args);
		index_list[index] = expect_fixed (get_arg (arg), 0);
		expect_range (arg, range_list[index]);
		}

	array_block_copy (rank, dst_array->elems, dst_array->calc_total(), dst_array->dims, range_list,
		src_array->elems, src_array->calc_total(), src_array->dims, index_list, true);

	delete [] index_list;
	delete [] range_list;
	}

return T_undef;
}	// P_Array_XCopy::evalV

//
//
//	Functor definitions
//
//

//
// Prototype for functor returning array object
//

#define	D_P_Array_eval		X_Array* eval(Expr *args)

struct P_Array : Prefix {
	P_Array (char const *ident, O_Enum op) : Prefix (ident, op) {}

	VType evalV (VDatum &val, Expr *args) {
		X_Array *array = eval (args);
		if (array) {
			val._array = array;
			return T_array;
			}
		else
			return T_undef;
		}	// evalV

	virtual D_P_Array_eval;
	};

X_Array *P_Array::eval (Expr *args) { return 0; }

//
// Convert argument to array
// (return 0 otherwise)
//

// Expect array expression
X_Array *Prefix::expect_array (Expr *expr) {
VDatum val;
VType type;

if ((type = evalV_X (expr, val)) == T_array)
	return val._array;

type_error (expr, T_array, type, val);
return (X_Array *) 0;
}	// Prefix::expect_array

//
//	TODO:
//	relink array arguments afterwards
//

//
//	Create new array
//

struct P_Array_Create : P_Array {
	P_Array_Create (char const *ident) : P_Array (ident, Op_ACreate) {}

	X_Array *eval (Expr *args) {
	unsigned dimensions[X_Array::MaxRank];
	unsigned rank = get_index (X_Array::MaxRank, dimensions, args, this);

	return new ("Array") X_Array (rank, dimensions);
	}	// eval

	};

//
//	Get array rank
//

struct P_Array_Rank : Prefix {
	P_Array_Rank (char const *ident) : Prefix (ident, Op_ARank) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Array *array = expect_array (args);

	if (array)
		{ return_fixed (val, array->rank); }

	return T_undef;
	}	// evalV
	};

//
//	Total count
//

struct P_Array_Total : Prefix {
	P_Array_Total (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Array *array = expect_array (args);

	if (array)
		{ return_fixed (val, array->calc_total ()); }

	return T_undef;
	}	// evalV
	};

//
//	Get array dimensions list
//

struct P_Array_Dims : PrefixX {
	P_Array_Dims (char const *ident) : PrefixX (ident, Op_ADims) {}

	Expr *evalX (Expr *args) {
	X_Array *array = expect_array (args);

	if (array) {
		unsigned rank = array->rank, *dims = array->dims;
		Expr *res = 0;
		while (rank --) {
			Expr *dim = new ("Fixed/adims") X_Fixed (dims[rank]);
			res = res ? new ("List/adims") X_List (dim, res) : dim;
			}
		return res;
		}

	return UNDEF;
	}	// evalX
	};

//
//	Get element of array
//

struct P_Array_Elem : PrefixR {
	P_Array_Elem (char const *ident, O_Enum op): PrefixR (ident, op) {}

	D_Prefix_evalR;
	};

Expr *&P_Array_Elem::evalR (Expr *args) {
X_Array *array = expect_array (get_arg (args));

if (array) {
	unsigned index [X_Array::MaxRank];
	unsigned count = get_index (array->rank, index, args, this);
	return array->elem (count, index);
	}

return R_null;
}	// P_Array_Elem::evalR

//
//	Load array from list of values
//

struct P_Array_Load : P_Array {
	P_Array_Load (char const *ident) : P_Array (ident, Op_Null) {}

	X_Array *eval (Expr *args) {
	X_Array *array = expect_array (get_arg (args));
	if (array) array->load (args);
	return array;
	}	// eval
	};

//
//	Save array to list of values
//

struct P_Array_Save : PrefixX {
	P_Array_Save (char const *ident) : PrefixX (ident, Op_Null) {}

	Expr *evalX (Expr *args) {
		X_Array *array = expect_array (args);
		return array ? array->save () : UNDEF;
		}	// evalX
	};

//
//	Fill array with value
//

struct P_Array_Fill : P_Array {
	P_Array_Fill (char const *ident) : P_Array (ident, Op_Null) {}

	X_Array *eval (Expr *args) {
	X_Array *array = expect_array (get_arg (args));
	if (array) array->fill (evalX_X (args));
	return array;
	}	// eval
	};

//
//	Initialize element with results of functor
//

// (Recursive helper function for 'init apply')
Expr **X_Array::init_apply_rec (Prefix *func, unsigned dim_index, Expr **elem_ptr,
	Expr *index_list, Expr *index_node) {
Expr *current;
X_List *list;

if (list = index_node->isList ()) {
	current = list->first;
	index_node = list->next;
	}
else {
	current = index_node;
	index_node = 0;
	}

unsigned dimension = dims[dim_index];
for (unsigned index = 0; index != dimension; ++ index) {
	assign_fixed (current, index);
	if (dim_index != rank-1)
		elem_ptr = init_apply_rec (func, dim_index + 1, elem_ptr,
			index_list, index_node);
	else {
		unlink_expr (*elem_ptr);
		*elem_ptr ++ = link_expr (func->evalX (index_list));
		}
	}

return elem_ptr;
}	// X_Array::init_apply_rec

void X_Array::init_apply (Prefix *func) {

// construct list of indexes
unsigned count = rank;
Expr *list = 0;

while (count --) {
	X_Fixed *node = new ("Array/apply") X_Fixed (0);
	if (list)
		list = new ("Array/apply") X_List (node, list);
	else
		list = node;
	}	// while (count)

// apply functor to array
link_expr (list);
init_apply_rec (func, 0, elems, list, list);
unlink_expr (list);
}	// X_Array::init_apply

struct P_Array_InitAll : Prefix {
	P_Array_InitAll (char const *ident) : Prefix (ident, Op_Null) {}

	VType evalV (VDatum &val, Expr *args) {
	X_Array *array = expect_array (get_arg (args));
	Prefix *func = expect_prefix (args);

	if (array && func)
		array->init_apply (func);

	return T_undef;
	}	// evalV
	};

//
//	Make exact copy of existing array
//

struct P_Array_Copy : P_Array {
	P_Array_Copy (char const *ident) : P_Array (ident, Op_Null) {}

	X_Array *eval (Expr *args) {
	X_Array *array = expect_array (get_arg (args));
	Prefix *pfx = args ? expect_prefix (args) : 0;

	return array ? array->copy_fn (pfx) : 0;
	}	// eval
	};

//
//	Array iterator
//

struct P_Array_Loop : P_Iterator {
	P_Array_Loop (char const *ident) : P_Iterator (ident, Op_Null) {}

	D_P_Iterator_evaluate;
	};

void P_Array_Loop::evaluate (IterContext &IC, Expr *args) {
X_Array *array = expect_array (get_arg (args));
Expr *&loop_var = expectR_X (get_arg (args));

if (array && args) {
	unsigned total = array->calc_total ();
	Expr **p_elem = array->elems;
	IC.start (args);

	while (total --) {
		mutateR_X (loop_var, *p_elem ++);
		
		if (! IC.next ()) break;
		}	// while (total)
	}	// if (array && args)
}	// P_Array_Loop::evaluate

//
//	Expand array
//

// Insert new elements (initialised with 'init') at 'index_range' on 'dimension'
void X_Array::insert_range (unsigned dimension, FixedRange index_range, Expr *fill) {
unsigned rank = this->rank;

if (dimension < rank && index_range.notempty() &&
	0 <= index_range.from && index_range.from <= dims[dimension]) {
unsigned count = index_range.count ();

unsigned old_total = calc_total ();
Expr **old_elems = elems;
unsigned *old_dims = dims;

FixedRange *range_list = new FixedRange [rank];
S_fixed *offset_list = new S_fixed [rank];
dims = new ("Array/dims") unsigned [rank];

for (unsigned index = 0; index != rank; ++ index) {
	offset_list[index] = 0;
	range_list[index].set (0, (dims[index] = old_dims[index]));
	}

unsigned extent = (dims[dimension] += count);
unsigned total = (old_total / (extent - count)) * extent;

//
// Allocate new elements block
//
elems = new ("Array/elems") Expr * [total];

//
// Copy elements BEFORE inserted block (if any)
//
if (index_range.from) {
	range_list[dimension].set (0, index_range.from);
	offset_list[dimension] = 0;
	array_block_copy (rank, elems, total, dims, range_list,
		old_elems, old_total, old_dims, offset_list, false);
	}	// (copy elements block before)

//
// Fill inserted block
//
range_list[dimension] = index_range;
array_block_fill (rank, elems, total, dims, range_list, fill, false);

//
// Copy elements AFTER inserted block (if any)
//
if (index_range.to != extent) {
	range_list[dimension].set (index_range.to, extent);
	offset_list[dimension] = index_range.from;
	array_block_copy (rank, elems, total, dims, range_list,
		old_elems, old_total, old_dims, offset_list, false);
	}	// (copy elements block after)

//
// Clean up
//
delete [] old_dims;
delete [] old_elems;

delete [] range_list;
delete [] offset_list;
}	// (operation valid)
}	// X_Array::insert_range

//
//	Shrink array
//

// Delete elements at 'index_range' on 'dimension'
void X_Array::delete_range (unsigned dimension, FixedRange index_range) {
unsigned rank = this->rank;

if (dimension < rank && index_range.notempty() &&
	0 <= index_range.from && index_range.to <= dims[dimension]) {
unsigned count = index_range.count ();

unsigned old_total = calc_total ();
Expr **old_elems = elems;
unsigned *old_dims = dims;

FixedRange *range_list = new FixedRange [rank];
S_fixed *offset_list = new S_fixed [rank];
dims = new ("Array/dims") unsigned [rank];

for (unsigned index = 0; index != rank; ++ index) {
	offset_list[index] = 0;
	range_list[index].set (0, (dims[index] = old_dims[index]));
	}

unsigned extent = (dims[dimension] -= count);
unsigned total = (old_total / (extent + count)) * extent;

//
// Allocate new elements block
//
elems = new ("Array/elems") Expr * [total];

//
// Copy elements BEFORE deleted block (if any)
//
if (index_range.from) {
	range_list[dimension].set (0, index_range.from);
	offset_list[dimension] = 0;
	array_block_copy (rank, elems, total, dims, range_list,
		old_elems, old_total, old_dims, offset_list, false);
	}	// (copy elements block before)

//
// Release elements block
//

range_list[dimension] = index_range;
array_block_release (rank, old_elems, old_total, old_dims, range_list);

//
// Copy elements AFTER deleted block (if any)
//
if (index_range.from != extent) {
	range_list[dimension].set (index_range.from, extent);
	offset_list[dimension] = index_range.to;
	array_block_copy (rank, elems, total, dims, range_list,
		old_elems, old_total, old_dims, offset_list, false);
	}	// (copy elements block after)

//
// Clean up
//
delete [] old_dims;
delete [] old_elems;

delete [] range_list;
delete [] offset_list;
}	// (operation valid)
}	// X_Array::delete_range

//
//	Reshape array
//

// Clip dimension [length] to [start..end],
// (placing result in 'range' and 'offset')
static bool clip_reshape (S_fixed start, S_fixed end, S_fixed length,
	FixedRange &range, S_fixed &offset) {
// find intersection...
if (0 < end && start < length) {
	// (intersection found)
	if (start >= 0) {
		range.from = 0;
		offset = start;
		}
	else {
		range.from = -start;
		offset = 0;
		}

	range.to = ((end <= length) ? end : length) - start;
	return true;
	}

return false;
}	// clip_reshape

void X_Array::reshape_range (FixedRange *new_range_list, Expr *fill) {
unsigned rank = this->rank;
unsigned old_total = calc_total ();
Expr **old_elems = elems;
unsigned *old_dims = dims;

unsigned i;

FixedRange *range_list = new ("temp/Range []") FixedRange [rank];
S_fixed *offset_list = new ("temp/Offset []") S_fixed [rank];
dims = new ("Array/dims") unsigned [rank];

bool is_common = true;

// Calculate new total
unsigned total = 1;
for (i = 0; i != rank; ++ i) {
	S_fixed start;
	unsigned count;

	// TODO: check range...
	new_range_list[i].get_ext (start, count);

	total *= (dims[i] = count);

	is_common = is_common &&
		clip_reshape (start, start + count, old_dims[i], range_list[i], offset_list[i]);
	}	// for (i)

//
// Allocate new elements block
//
elems = new ("Array/elems") Expr * [total];

for (i = 0; i != total; ++ i) elems[i] = 0;

if (is_common) {

array_block_copy (rank, elems, total, dims, range_list,
	old_elems, old_total, old_dims, offset_list, false);

FixedRange *rel_ranges = new ("temp/release-range []") FixedRange [rank];

for (i = 0; i != rank; ++ i)
	rel_ranges[i].set (0, old_dims[i]);

for (i = 0; i != rank; ++ i) {
	FixedRange &index_range = new_range_list[i];
	FixedRange &release_range = rel_ranges[i];
	S_fixed s_start, s_end, s_total = old_dims[i];

	// (release array block before)
	if (0 < index_range.from) {
		s_start = index_range.from;
		release_range.set (0, s_start);

		array_block_release (rank, old_elems, old_total, old_dims, rel_ranges);
		}
	else s_start = 0;

	// (release array block after)
	if (index_range.to < s_total) {
		s_end = index_range.to;
		release_range.set (s_end, s_total);

		array_block_release (rank, old_elems, old_total, old_dims, rel_ranges);
		}
	else s_end = s_total;

	// (clip index to)
	release_range.set (s_start, s_end);
	}	// (dimensions loop)

delete [] rel_ranges;
}	// (is_common)

else {
// (release old array entirely...)

for (i = 0; i != old_total; ++ i)
	unlink_expr (old_elems[i]);
}

//
// Clean up
//

delete [] old_dims;
delete [] old_elems;

delete [] range_list;
delete [] offset_list;
}	// X_Array::reshape_range

struct P_Array_Insert : P_Array {
	P_Array_Insert (char const *ident) : P_Array (ident, Op_Null) {}

	D_P_Array_eval;
	};

X_Array *P_Array_Insert::eval (Expr *args) {
X_Array *array = expect_array (get_arg (args));

if (array) {
	unsigned dimension = expect_fixed (get_arg (args), 0);
	FixedRange index_range (0, 0);
	expect_range (get_arg (args), index_range);

	array->insert_range (dimension, index_range, evalX_X (args));
	}

return array;
}	// P_Array_Insert::eval

struct P_Array_Delete : P_Array {
	P_Array_Delete (char const *ident) : P_Array (ident, Op_Null) {}

	D_P_Array_eval;
	};

X_Array *P_Array_Delete::eval (Expr *args) {
X_Array *array = expect_array (get_arg (args));

if (array) {
	unsigned dimension = expect_fixed (get_arg (args), 0);
	FixedRange index_range (0, 0);
	expect_range (args, index_range);

	array->delete_range (dimension, index_range);
	}

return array;
}	// P_Array_Delete::eval

struct P_Array_Reshape : P_Array {
	P_Array_Reshape (char const *ident) : P_Array (ident, Op_Null) {}

	D_P_Array_eval;
	};

X_Array *P_Array_Reshape::eval (Expr *args) {
X_Array *array = expect_array (get_arg (args));

if (array) {
	unsigned rank = array->rank;
	FixedRange *reshape_list = new ("Array/ranges") FixedRange [rank];
	for (unsigned i = 0; i != rank; ++ i) {
		reshape_list[i].set (0, array->dims[i]);
		expect_range (get_arg (args), reshape_list[i]);
		}

	array->reshape_range (reshape_list, 0);
	
	delete [] reshape_list;
	}

return array;
}	// P_Array_Reshape::eval

//
//	Array reversion / rotation
//

// Reverse range 'rev_range' of array 'dimension'
void X_Array::reverse_dimension (unsigned dimension, FixedRange &rev_range) {
if (dimension < rank) {
unsigned unit_count = 1;

for (unsigned i = 0; i != dimension; ++ i)
	unit_count *= dims[i];
unsigned count = dims[dimension];
unsigned unit_size = calc_total () / (unit_count * count);

// TODO: rev_range...

Expr **base = elems;
while (unit_count --) {

if (unit_size == 1) {
Expr **l_ptr = base, **r_ptr = base + count;
while (l_ptr < r_ptr) {
	Expr *temp = *l_ptr;
	*l_ptr ++ = *-- r_ptr;
	*r_ptr = temp;
	}
}
else {
Expr **l_ptr = base, **r_ptr = base + count*unit_size;

while (l_ptr < r_ptr) {
	r_ptr -= unit_size;

	unsigned c = unit_size;
	while (c --) {
		Expr *temp = *l_ptr;
		*l_ptr ++ = *r_ptr;
		*r_ptr ++ = temp;
		}

	r_ptr -= unit_size;
	}
}

base += count * unit_size;
}	// while (unit_count)
}	// valid dimension
}	// X_Array::reverse_dimension

// Rotate range 'rot_range' of array 'dimension' by 'shift'
void X_Array::rotate_dimension (unsigned dimension, int shift, FixedRange &rot_range) {
if (dimension < rank) {
unsigned unit_count = 1;

for (unsigned i = 0; i != dimension; ++ i)
	unit_count *= dims[i];
unsigned count = dims[dimension];
unsigned unit_size = calc_total () / (unit_count * count);

// (TODO: not very efficient rotation...)

if (shift < 0) shift += count;
shift %= count;

Expr **base = elems;
while (unit_count --) {

if (unit_size == 1) {
unsigned l_cnt = shift;
while (l_cnt --) {
Expr **e_ptr = base;
Expr *temp = *e_ptr;
unsigned c = count - 1;
while (c --) {
	*e_ptr = e_ptr[1];
	e_ptr ++;
	}
*e_ptr = temp;
}
}

else {
unsigned unit_cnt = unit_size;
base += unit_cnt;
while (unit_cnt --) {
	unsigned l_cnt = shift;
	-- base;
	while (l_cnt --) {
	Expr **e_ptr = base;

	Expr *temp = *e_ptr;
	unsigned c = count - 1;
	while (c --) {
		*e_ptr = e_ptr[unit_size];
		e_ptr += unit_size;
		}
	*e_ptr = temp;
	}
	}	// while (unit_cnt)
}

base += count * unit_size;
}	// while (unit_count)
}	// valid dimension
}	// X_Array::rotate_dimension

struct P_Array_Reverse : P_Array {
	P_Array_Reverse (char const *ident) : P_Array (ident, Op_Null) {}

	D_P_Array_eval;
	};

X_Array *P_Array_Reverse::eval (Expr *args) {
X_Array *array = expect_array (get_arg (args));

if (array) {
	unsigned rank = array->rank;
	unsigned dimension = expect_fixed (args, 0);

	FixedRange unused;

	array->reverse_dimension (dimension, unused);
	}

return array;
}	// P_Array_Reverse::eval

struct P_Array_Rotate : P_Array {
	P_Array_Rotate (char const *ident) : P_Array (ident, Op_Null) {}

	D_P_Array_eval;
	};

X_Array *P_Array_Rotate::eval (Expr *args) {
X_Array *array = expect_array (get_arg (args));

if (array) {
	unsigned rank = array->rank;
	unsigned dimension = expect_fixed (get_arg (args), 0);
	int shift = expect_fixed (get_arg (args), 0);

	FixedRange unused;

	array->rotate_dimension (dimension, shift, unused);
	}

return array;
}	// P_Array_Rotate::eval

//
//	Operation table
//

// TODO: array -> a_create ...

static bool init_primaries_array (int order) {

//		[Categories]

//^C	Array
//^B	Array functors
//^D	Functors, operating on arrays.

//		[Types]

//^T	Array
//^B	Array value.
//^D	Anything evaluating to array.
//^D	(Reports error, if argument is not array.)

//		[Errors]

//^E	ExpectArray
//^B	Array operand expected.
//^D	Expected operand, evaluating to array.

//		--------

//^N	is_array [Predicate | Array]
//^P	is_array (V: Any) => Bool
//^B	Check for array value.
//^D	Predicate: !true, if argument \V evaluates to array.

	DefBuiltin (P_IsType ("is_array", Op_Null, T_array));

//^N	expect_array [Wrapper | Array]
//^P	expect_array (V: Any, @Body: Any) => Any
//^B	Expect array value.
//^D	If argument \V evaluates to array, evaluates and returns \Body.
//^D	(Reports type error otherwise.)

	DefBuiltin (P_ExpectType ("expect_array", Op_Null, T_array));

//^G	array

//^N	array [Array | Constructor]
//^P	array (Dims: List) => Array
//^B	Array constructor.
//^D	Creates new array (list \Dims contains dimensions, from outer to inner).
//^D	(Elements of created array are initialised to !undef.)

	DefBuiltin (P_Array_Create ("array"));
	DefBuiltin (P_Array_Create ("a_create"));

//^G	a_rank a_dims a_total

//^N	a_rank [Array]
//^P	a_rank (Array: Array) => Int
//^B	Array rank.
//^D	Returns rank (total number of dimensions) of array \Array.

	DefBuiltin (P_Array_Rank ("a_rank"));

//^N	a_dims [Array]
//^P	a_dims (Array: Array) => List
//^B	Array dimensions.
//^D	Returns list of dimensions of array \Array (in order from outer to inner).

	DefBuiltin (P_Array_Dims ("a_dims"));

//^N	a_total [Array]
//^P	a_total (Array: Array) => Int
//^B	Total elements in array.
//^D	Returns total number of elements in array \Array.

	DefBuiltin (P_Array_Total ("a_total"));

//^G	a_elem

//^N	a_elem [Array | Mutable]
//^P	a_elem (Array: Array, Index: List) => Mutable
//^B	Array element accessor.
//^D	Returns mutable reference to element of array \Array with index list \Index (from outer indexes to inner).
//^D	Syntax: \Array {\Index}.

	DefBuiltin (P_Array_Elem ("a_elem", Op_AElem));

//^G	a_fill a_load a_save

//^N	a_fill [Array | Mutator]
//^P	a_fill (Array: Array, Elem: Any) => Array
//^B	Fill array with value.
//^D	Fills entire array \Array with value of \Elem.
//^D	Returns \Array.

	DefBuiltin (P_Array_Fill ("a_fill"));

//^N	a_load [Array | Mutator | List]
//^P	a_load (Array: Array, Elems: List) => Array
//^B	Load array elements from list of values.
//^D	Loads array \Array with list of \Elems (according to internal elements ordering).
//^D	(Extra elements are ignored).
//^D	Returns \Array.

	DefBuiltin (P_Array_Load ("a_load"));

//^N	a_save [Array | List]
//^P	a_save (Array: Array) => List
//^B	Save array elements to list of values.
//^D	Returns contents of array \Array as list (according to internal elements ordering).
//^D	(Final undefined elements are NOT included.)

	DefBuiltin (P_Array_Save ("a_save"));

//^N	a_init_all [Array | Mutator]
//^P	a_init_all (Array: Array, FnVal: Func) => ()
//^B	Array functional initializer.
//^D	Initializes all elements of array \Array by using functor reference \FnVal.
//^D	(Each element is initialized by \FnVal ! \Index, where \Index is list of element indexes, from outer to inner.)

	DefBuiltin (P_Array_InitAll ("a_init_all"));

//^G	a_copy

//^N	a_copy [Array | Constructor]
//^P	a_copy (Array: Array) => Array
//^B	Copy array.
//^D	Makes and returns copy of array \Array
//^\	(both copies of array are sharing same elements).

	DefBuiltin (P_Array_Copy ("a_copy"));

//^G	a_loop

//^N	a_loop [Array | Iterator]
//^P	a_loop (Array: Array, Var: Mutable, @Body: Any) => Any
//^B	Array iterator.
//^D	Evaluates \Body for all elements of the \Array (in internal order),
//^\	assigning current element to \Var before each iteration.

	DefBuiltin (P_Array_Loop ("a_loop"));

//^G	a_insert a_delete a_reshape

//^N	a_insert [Array | Mutator]
//^P	a_insert (Array: Array, DimIndex: Int, InsertRange: Range, [Init: Any]) => Array
//^B	Insert elements block into array.
//^D	Expand array \Array by dimension \DimIndex
//^\	by inserting elements block at \InsertRange.
//^D	(Inserted block is initialized with \Init.)
//^D	Returns \Array.

//^N	a_delete [Array | Mutator]
//^P	a_delete (Array: Array, DimIndex: Int, DeleteRange: Range) => Array
//^B	Delete elements block from array.
//^D	Shrink array \Array at dimension \DimIndex
//^\	by deleting elements block at \DeleteRange.
//^D	Returns \Array.

	DefBuiltin (P_Array_Insert ("a_insert"));
	DefBuiltin (P_Array_Delete ("a_delete"));

//^N	a_reshape [Array | Mutator]
//^P	a_reshape (Array: Array, DimRange0: Range, ... . DimRangeN: Range, ) => Array
//^B	Reshape array.
//^D	Reshape array \Array, changing dimension ranges to \DimRange0 .. \DimRangeN.
//^D	Returns \Array.

	DefBuiltin (P_Array_Reshape ("a_reshape"));

// TMP...

	DefBuiltin (P_Array_Reverse ("a_reverse"));
	DefBuiltin (P_Array_Rotate ("a_rotate"));

	DefBuiltin (P_Array_XFill ("a__xfill"));
	DefBuiltin (P_Array_XCopy ("a__xcopy"));

return true;
}	// init_primaries_array

DefSubSystem ("array", init_primaries_array, 0);

#endif
