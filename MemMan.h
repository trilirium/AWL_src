
//
//	"MemMan.h":
//	Memory manager; tracking/debugging functions
//

//
// Allocator interface
//

void *operator new (size_t size);
void *operator new [] (size_t size);
void *operator new (size_t size, char const *tag);
void *operator new [] (size_t size, char const *tag);

void operator delete (void *ptr);
void operator delete [] (void *ptr);

// (function on block)
typedef void (* mc_func) (char const *tag, unsigned size, void *ptr);

// MM error handler
void MM_error (void *addr, char const *where, char const *message);

// Set hooks
void MM_sethooks (mc_func on_alloc, mc_func on_free);

// Iterator for all allocated memory blocks
void MM_forall (mc_func op, unsigned &t_count, unsigned &t_size);

// Get final memory info
void MM_getstat (unsigned &summary, unsigned &balance, unsigned &peak_count, unsigned &peak_size);

