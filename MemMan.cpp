
//
//	"MemMan.cpp":
//	Memory manager; tracking/debugging functions
//

#ifdef TARGET_UNIX
#include "memory.h"
#else
#include "mem.h"
#endif

#include <cstdlib>

#include "Defs.h"

#include "MemMan.h"

// (Memory: watch mode)
static bool mc_watch = true;

// (Wipe memory blocks on alloc)
static bool mc_wipe = true;

// (Memory watch block)
struct mc_block {
	unsigned signature;
	char const *tag;
	unsigned size;
	struct mc_block *prev, *next;
	};

// Root block
static mc_block *mc_root = 0;

// Memory balance
static int mc_balance = 0;

// Total allocated
static unsigned mc_total = 0;

// Total # of operations
static unsigned mc_summary = 0;

// Peak size/count
static unsigned mc_peak_count = 0;
static unsigned mc_peak_size = 0;

// Memory block signature
#define MC_SIGN		(((((('M' << 8) | 'B') << 8) | 'l') << 8) | 'k')

// (function on block)
typedef void (* mc_func) (char const *tag, unsigned size, void *ptr);

// (alloc/free hooks)

static mc_func alloc_hook = 0, free_hook = 0;

// Controlled block allocation
static void *mc_alloc (size_t size, char const *tag) {
mc_balance ++;
mc_summary ++;

if (mc_watch) {
	mc_block *mc_blk = (mc_block *) malloc(sizeof(mc_block) + size);
	if (!mc_blk) return 0;

	if (mc_wipe)
		memset (mc_blk, unsigned (mc_blk) & 0xFF, sizeof(mc_block) + size);

	mc_blk->signature = MC_SIGN;
	mc_blk->size = size;
	mc_blk->tag = tag;

	if ((mc_total += size) > mc_peak_size)
		mc_peak_size = mc_total;
	if (mc_balance > mc_peak_count)
		mc_peak_count = mc_balance;

	if (mc_root) {		// (was not first...)
		mc_blk->prev = mc_root->prev;
		mc_blk->next = mc_root;

		mc_root->prev->next = mc_blk;
		mc_root->prev = mc_blk;
		}
	else			// (was first..)
		mc_root = mc_blk->prev = mc_blk->next = mc_blk;

	if (alloc_hook) alloc_hook (tag, size, mc_blk + 1);

	return mc_blk + 1;
	}	// (mc_watch)

else return malloc(size);
}	// mc_alloc

// Controlled block deallocation
static void mc_free (void *ptr) {
mc_balance --;
mc_summary ++;

if (! ptr) return;

if (mc_watch) {
	mc_block *mc_blk = ((mc_block *) ptr) - 1;

	if (mc_blk->signature != (unsigned) MC_SIGN) {
		MM_error (mc_blk, "mc_free", "Bad block signature");
		return;
		}

	mc_total -= mc_blk->size;

	if (mc_blk->next != mc_blk) {	// (was not last...)
		mc_blk->next->prev = mc_blk->prev;
		mc_blk->prev->next = mc_blk->next;

		if (mc_root == mc_blk) mc_root = mc_blk->next;
		}
	else		// (was last...)
		mc_root = 0;

	if (free_hook) free_hook (mc_blk->tag, mc_blk->size, ptr);

	mc_blk->signature = 0;		// (precaution...)
	
	free (mc_blk);
	}	// (mc_watch)

else free (ptr);
}	// mc_free

// Set hooks
void MM_sethooks (mc_func on_alloc, mc_func on_free) {
alloc_hook = on_alloc, free_hook = on_free;
}	// MM_sethooks

// Iterator for all allocated memory blocks
void MM_forall (mc_func mc_op, unsigned &t_count, unsigned &t_size) {
t_count = t_size = 0;

if (mc_watch) {
if (mc_root) {
	mc_block *mc_ptr = mc_root;
	do {
		mc_op (mc_ptr->tag, mc_ptr->size, ((mc_block *) mc_ptr) + 1);
		++ t_count;
		t_size += mc_ptr->size;
		} while ((mc_ptr = mc_ptr->next) != mc_root);
	}
}	// (mc_watch)
}	// MM_forall

// Get final memory info
void MM_getstat (unsigned &summary, unsigned &balance, unsigned &peak_count, unsigned &peak_size) {
summary = mc_summary;
balance = mc_balance;
peak_count = mc_peak_count;
peak_size = mc_peak_size;
}	// MM_getstat

//
//	(operators)
//

DL_EXPORT void *operator new (size_t size) {
return mc_alloc (size, 0);
}	// operator new

DL_EXPORT void *operator new [] (size_t size) {
return mc_alloc (size, 0);
}	// operator new

DL_EXPORT void *operator new (size_t size, char const *tag) {
return mc_alloc (size, tag);
}	// operator new

DL_EXPORT void *operator new [] (size_t size, char const *tag) {
return mc_alloc (size, tag);
}	// operator new

DL_EXPORT void operator delete (void *ptr) {
mc_free (ptr);
}	// operator delete

DL_EXPORT void operator delete [] (void *ptr) {
mc_free (ptr);
}	// operator delete

