
/*

	+---+---+---+---+---+---+
	|	'NameTab.cpp':
	|	Name tables, and name scopes.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include <string.h>

#include "Defs.h"

#include "NameTab.h"

#include "Eval.h"

//
// Name table
//

// Hash code for name
static unsigned hash_code (char const *ident) {
unsigned code = 0, i = 0;
char ch;
while (ch = *ident ++)
	code += i ++, code <<= 4, code ^= ch;

return code ^ i;
}	// hash_code

// TODO: implement inflatable nametable

// Reusable storage for all one-symbol names
static char L1_names [255*2];

// Constructor for 'Name"
Name::Name (char const *ident) {
if (ident && *ident) {
unsigned length = strlen (ident);

this->ident =
	length == 1 ?
		L1_names + 2*((unsigned char) *ident - 1) :
		new ("Name::name") char [length+1];

strcpy (this->ident, ident);
}
else this->ident = 0;

// NOTE -- not initialised:
//	Name::table
//	Name::hash
//	Name::ordinal
//	Name::refs
//	Name::next
//
}	// Name::Name

Name::~Name () {
if (ident && strlen (ident) > 1)
	delete [] ident;
}	// Name::~Name

// Link name
Name *link_name (Name *name) {
if (name) ++ name->refs;
return name;
}	// link_name

// Unlink name
void unlink_name (Name *name) {
if (name && ! --name->refs)
	name->release ();
}	// unlink_name

//
// Name table
//

// TODO: make nametab unallocated by default

NameTab::NameTab (unsigned no) {
counter = 0;
base = new ("Name []") Name * [this->no = no];

for (unsigned i = 0; i != no; ++ i)
	base [i] = 0;			// initialise

locked = false;
}	// NameTab::NameTab

NameTab::~NameTab () {
for (unsigned i = 0; i != no; ++ i) {
	Name *&root = base[i], *name;
	while (name = root) {
		root = name->next;
		unlink_name (name);
		}
	}

delete [] base;
}	// NameTab::~NameTab

Name *NameTab::mkName (char const *ident) {
return new ("Name") Name (ident);
}	// NameTab::mkName

Name *NameTab::control (char const *ident, bool install) {
unsigned hash = hash_code(ident);
Name * &root = base[hash % no], *name;

for (name = root; name; name = name->next)
	if (hash == name->hash && !strcmp (ident, name->ident))
		return name;

if (! install || locked) return 0;		/* not found */

// (Install new name node)
name = mkName (ident);

name->table = this;
name->hash = hash;
name->ordinal = counter ++;
name->refs = 1;
name->next = root;
root = name;

return name;
}	// NameTab::control

// Name table unsorted iterator
unsigned NameTab::forall (funcName func) {
unsigned count = 0;

for (unsigned i = 0; i != no; ++ i)
	for (Name *name = base[i]; name; name = name->next)
		if (func (name)) count ++;

return count;
}	// NameTab::forall

// Name table sorted iterator
unsigned NameTab::forall_ordered (funcName func, bool flag) {
Name **name_tab = new ("NameTab/tmp") Name * [counter];

for (unsigned i = 0; i != no; ++ i)
	for (Name *name = base[i]; name; name = name->next)
		name_tab[name->ordinal] = name;

unsigned count = 0;

for (unsigned j = 0; j != counter; ++ j)
	if (func (name_tab [flag ? counter - j - 1 : j]))
		++ count;

delete [] name_tab;

return count;
}	// NameTab::forall_ordered

// Find name with appropriate ordinal in scope
Name *NameTab::find_ordinal (unsigned ordinal) {
for (unsigned i = 0; i != no; ++ i)
	for (Name *name = base[i]; name; name = name->next)
		if (name->ordinal == ordinal)
			return name;

return 0;
}	// NameTab::find_ordinal

Name *NameTab::install (char const *ident) {
return control (ident, true);
}	// NameTab::install

Name *NameTab::lookup (char const *ident) {
return control (ident, false);
}	// NameTab::lookup

Prefix *NameTab::get_owner () { return 0; }

//
//	Variables tables
//

// Make name for: module variables table
Name *ModuleScope::mkName (char const *ident) {
return new ("ModuleVar") ModuleVar (ident);
}	// ModuleScope::mkName

Prefix *ModuleScope::get_owner () { return owner; }

// Make name for: functor variables table
Name *FunctorScope::mkName (char const *ident) {
return new ("FunctorVar") FunctorVar (ident);
}	// FunctorScope::mkName

Prefix *FunctorScope::get_owner () { return owner; }

// Make name for: class variables table
Name *ClassScope::mkName (char const *ident) {
return new ("ClassVar") ClassVar (ident);
}	// ClassScope::mkName

Prefix *ClassScope::get_owner () { return owner; }

//
//	Prefixes tables
//

Name *PrefixScope::mkName (char const *ident) {
return new ("PrefixName") PrefixName (ident);
}	// PrefixScope::mkName

Prefix *PrefixScope::get_owner () { return owner; }

//
//	Evaluation of names
//

Expr *&AnyVar::evalR () {
return R_null;
}	// AnyVar::evalR

Expr *&ModuleVar::evalR () {
return expr;
}	// ModuleVar::evalR

Expr *&FunctorVar::evalR () {
return p_expr ? *p_expr : R_null;
}	// FunctorVar::evalR

Expr *&ClassVar::evalR () {
return p_expr ? *p_expr : R_null;
}	// ClassVar::evalR

//
//	Destruction of names
//

void Name::release () {
delete this;
}	// Name::release

void ModuleVar::release () {
unlink_expr (expr);
Name::release ();
}	// ModuleVar::release

void PrefixName::release () {
unlink_prefix (prefix);
Name::release ();
}	// PrefixName::release

