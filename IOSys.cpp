
/*

	+---+---+---+---+---+---+
	|	"IOSys.cpp":
	|	Input/output layers:
	|	buffering, debugging.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

//
//	Abstract I/O layer
//

struct IOSystem {

	IOSystem ();

	// Read block of data
	// Returns # of bytes read (-1 on error)
	virtual int read (void *in_buf, unsigned in_count);

	// Write block of data
	// Returns # of bytes written (-1 on error)
	virtual int write (const void *out_buf, unsigned out_count);

	// Get single octet (true on success)
	virtual bool get_octet (char &octet);

	// Put single octet (true on success)
	virtual bool put_octet (char octet);

	// Skip by offset (backward/forward)
	virtual int skip (int offset);

	// Seek to position (from beginning / from end)
	virtual int seek (int position);

	// Tell current position
	virtual int tell ();

	// Sync
	virtual bool sync ();

	// Check for end
	virtual bool is_at_eos ();

	// Get current length
	virtual int get_length ();

	// Set current length
	virtual int set_length (unsigned length);

	// Check for console device
	virtual bool is_console ();

	// Close I/O subsystem (return true on success)
	virtual bool close ();

	// Log I/O system info
	virtual void log (struct Logger &log);

	// Get some stream info
	virtual struct Expr *get_attr ();

	// Report illegal operation
	void illegal (char const *what);

	};	// IOSystem

#ifndef HEADER

#include "Defs.h"

#include "Logger.h"

#define HEADER

#include "E_Stream.cpp"

#undef HEADER

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

DL_EXPORT IOSystem::IOSystem () {}

// Read block of data
// Returns # of bytes read (-1 on error)
int IOSystem::read (void *in_buf, unsigned in_count)
	{ illegal ("read"); return 0; }

// Write block of data
// Returns # of bytes written (-1 on error)
int IOSystem::write (const void *out_buf, unsigned out_count)
	{ illegal ("write"); return 0; }

// Get single octet (true on success)
bool IOSystem::get_octet (char &octet)
	{ illegal ("get"); return false; }

// Put single octet (true on success)
bool IOSystem::put_octet (char octet)
	{ illegal ("put"); return true; }

// Skip by offset (backward/forward)
int IOSystem::skip (int offset) { illegal ("skip"); return 0; }

// Seek to position (from beginning / from end)
int IOSystem::seek (int position) { illegal ("seek"); return 0; }

// Tell current position
int IOSystem::tell () { illegal ("tell"); return 0; }

// Sync data
bool IOSystem::sync () { illegal ("sync"); return false; }

// Check for stream end
bool IOSystem::is_at_eos () { illegal ("is_at_eos"); return true; }

// Get current length
int IOSystem::get_length () { illegal ("get_length"); return 0; }

// Set current length
int IOSystem::set_length (unsigned length) { illegal ("set_length"); return 0; }

// Check for console device
bool IOSystem::is_console () { illegal ("is_console"); return false; }

// Close I/O subsystem (return true on success)
bool IOSystem::close () { illegal ("close"); return true; }

// Log I/O system info
void IOSystem::log (Logger &log) { log.put_cstr ("<>"); }

// Get some stream info
Expr *IOSystem::get_attr () { return 0; }

// Report illegal operation
void IOSystem::illegal (char const *what) {
	syslog->put_cstr ("Illegal stream operation: ")->put_cstr (what)->put_cstr (" ");
	log (*syslog);
	syslog->put_nl ();
	}	// illegal

//
//
//	Base level input/output streams
//
//

//
//	Implementation of Input/output buffering
//

bool IO_debug_mode = false;

struct IOBuffer : IOSystem {

	// constructor
	IOBuffer (unsigned size);
	
	// destructor
	~IOBuffer ();

	IOSystem *io_base;		// (a base I/O layer)

	unsigned total;			// (total bytes allocated for buffer; multiply of 4)
	unsigned used;			// (bytes of currently available content in buffer)
	unsigned origin;		// (of buffer relative to stream)
	int iopos;				// (I/O offset, inside buffer 'origin')
	bool dirty;				// (dirty flag)
	char *data;				// (actual data allocated here)

	// Base read to: "in_buf@in_off" [count] @origin
	inline int base_read_at (unsigned origin, void *in_buf, unsigned in_off, unsigned count) {
		if (io_base->seek (origin) == origin)
			return io_base->read ((char *)in_buf + in_off, count);
		
		return -1;
		}	// base_read_at

	// Base write from: "out_buf@out_off" [count] @origin
	inline int base_write_at (unsigned origin, const void *out_buf, unsigned out_off, unsigned count) {
		if (io_base->seek (origin) == origin)
			return io_base->write ((char *)out_buf + out_off, count);

		return -1;
		}	// base_write_at

	// Copy data to I/O buffer from user space
	// (out_buf@out_off ==>> IOBuf@iopos) [count]
	inline void copy_from (const void *out_buf, unsigned out_off, unsigned count)
		{ memcpy (data + iopos, (char *)out_buf + out_off, count); }

	// Copy data from I/O buffer to user space
	// (in_buf@in_off <<== IOBuf@iopos) [count]
	inline void copy_to (void *in_buf, unsigned in_off, unsigned count)
		{ memcpy ((char *)in_buf + in_off, data + iopos, count); }

	// Shift buffer to new origin
	void shift (unsigned new_origin);

	// Close
	bool close ();

	// Check, is stream at end
	bool is_at_eos ();

	// Check for console
	bool is_console ();

	// Pre-read IOBuffer
	void preread ();

	// Post-write IOBuffer (assuming it is dirty)
	void postwrite ();

	// Read data: stream -> out_buf [out_len]
	int read (void *out_buf, unsigned out_len);

	// Write data: in_buf [in_len] -> stream
	int write (const void *in_buf, unsigned in_len);

	// Seek to absolute position 'abspos' (> 0 ? from start : from end)
	int seek (int abspos);

	// Skip by relative offset 'relpos' (> 0 ? forward : backward)
	int skip (int relpos);

	// Tell current position, relative to start
	int tell ();

	// Put 'octet' to stream
	bool put_octet (char octet);

	// Get 'octet' from stream
	bool get_octet (char &octet);

	// Sync & clear buffer
	bool sync ();

	// Get current stream length
	int get_length ();

	// Set current stream length
	int set_length (unsigned length);

	// Logging
	void log (Logger &log) {
		log.put_ch ('[')->log_fixed (used)->put_ch (':')->log_fixed (total)->put_ch (']');
		io_base->log (log);
		}

	// I/O debugging (optional)
	void IO_debug (char const *str0, unsigned val0, char const *str1 = 0, unsigned val1 = 0) {
	if (IO_debug_mode) {
		syslog->put_cstr ("IO Debug: ");
		log (*syslog);
		syslog->put_cstr (" [")->put_cstr (str0)->log_fixed (val0);
		if (str1) {
			syslog->put_cstr (str1)->log_fixed (val1);
			}
		syslog->put_cstr ("]\n");
		}
	}	// IO_debug

	};	// IOBuffer

// Control buffer bound to stream
int Stream::buf_resize (unsigned size) {
// (round up size)
size += 0xF;
size &= ~0xF;

IOBuffer *buffer = Cast (IOBuffer, io_layer);

if (buffer && buffer->total == size)
	return size;			// (nothing changed)

if (! buffer && ! size)
	return 0;				// (nothing changed)

unsigned p_size = 0;
unsigned origin = buffer ? buffer->origin : 0;

if (buffer) { p_size = buffer->total; release_buf (); }
if (size) allocate_buf (size, origin);

return p_size;
}	// Stream::buf_resize

// Constructor for IOBuffer (with 'size')
IOBuffer::IOBuffer (unsigned size) {
data = new ("IOBuffer.data") char [size];
total = size;
iopos = 0;				// (keep iopos?)
used = 0;
dirty = false;

io_base = 0;
}	// IOBuffer::IOBuffer

// Allocate buffer
void Stream::allocate_buf (unsigned size, unsigned origin) {
if (size) {
	IOBuffer *buffer = new ("IO/Buffer") IOBuffer (size);

	buffer->origin = origin;

	buffer->io_base = io_layer;
	io_layer = buffer;
	
	buffer->IO_debug ("Allocate: size=", size);
	}
}	// Stream::allocate_buf

// Close I/O subsystem
// (return true on success)
bool IOBuffer::close () {
if (dirty) sync ();

if (io_base->close ()) {
	delete io_base;

	delete [] data;
	return true;
	}

return false;
}	// IOBuffer::close

// Destructor for IOBuffer
IOBuffer::~IOBuffer () {
delete [] data;
}	// IOBuffer::~IOBuffer

// Release buffer
void Stream::release_buf () {
IOBuffer *buffer = Cast (IOBuffer, io_layer);

if (buffer) {
	if (buffer->dirty) sync ();

	buffer->IO_debug ("Release: size=", buffer->total);

	io_layer = buffer->io_base;
	delete (buffer);
	}
}	// Stream::release_buf

// Reposition buffer to new origin
void IOBuffer::shift (unsigned new_origin) {
iopos -= (new_origin - origin);

IO_debug ("shift: ", origin, " -> ", new_origin);

origin = new_origin;
}	// IOBuffer::shift

// Pre-read buffer content @origin
void IOBuffer::preread () {
used = base_read_at (origin, data, 0, total);			// TODO: check errors
dirty = false;

IO_debug ("preread: @", origin, " #", used);
}	// IOBuffer::preread

// Post-write (flush) buffer content @origin
void IOBuffer::postwrite () {
IO_debug ("postwrite @", origin, " #", used);

base_write_at (origin, data, 0, used);					// TODO: check errors
used = 0;
dirty = false;
}	// IOBuffer::postwrite

// Read from IOBuffer at current position to 'in_buf' maximum 'in_len' bytes
// Returns: # of bytes read (-1 on error)
int IOBuffer::read (void *in_buf, unsigned in_len) {
unsigned offset;	// (offset relative to 'in_buf')
int count;			// read byte count (temporary)

if (! in_len) return 0;	// (nothing to read)

// (check pointer)
if (iopos < 0) {
	// (read before buffer)
	if ((count = -iopos) > in_len) count = in_len;

	if ((count = base_read_at (origin + iopos, in_buf, 0, count)) < 0)
		return -1;

	iopos += count;

	if (! (in_len -= count))
		return count;		// (done reading)

	offset = count;
	}
else offset = 0;

// (assuming iopos >= 0)
if (iopos < used) {
	// (read from buffer)
	if ((count = used - iopos) > in_len) count = in_len;

	// (copy some (maybe all) data from buffer)
	copy_to (in_buf, offset, count);
	iopos += count;

	if (! (in_len -= count))
		return offset + count;		// (done reading)

	offset += count;
	}

// (read after buffer)
if (dirty) postwrite ();

if (in_len < total) {
	// (preread buffer)
	shift (origin + iopos);
	preread ();

	if ((count = in_len) > used) count = used;
	copy_to (in_buf, offset, in_len);
	iopos = count;
	}
else {
	// (direct read: no buffering)
	if ((count = base_read_at (origin + iopos, in_buf, offset, in_len)) < 0)
		return offset;

	iopos += count;
	}

return offset + count;
}	// IOBuffer::read

// Write to IOBuffer at current position from 'out_buf' maximum 'out_len' bytes
// Returns: # of bytes written (-1 on error)
int IOBuffer::write (const void *out_buf, unsigned out_len) {
unsigned offset;	// (offset relative to 'out_buf')
int count;			// write byte count (temporary)

if (! out_len) return 0;	// (nothing to write)

// (check pointer)
if (iopos < 0) {
	// (write before buffer)
	if ((count = -iopos) > out_len) count = out_len;

	if ((count = base_write_at (origin + iopos, out_buf, 0, count)) < 0)
		return -1;

	iopos += count;

	if (! (out_len -= count))
		return count;		// (done writing)

	offset = count;
	}
else offset = 0;

// (assuming iopos >= 0)
if (iopos <= used) {
	// (write to buffer)
	if ((count = total - iopos) > out_len) count = out_len;

	// (copy some (maybe all) data to buffer)
	copy_from (out_buf, offset, count);
	iopos += count;

	if (iopos > used) used = iopos;
	dirty = true;

	if (! (out_len -= count))
		return offset + count;		// (done writing)

	offset += count;
	}

// (write after buffer)
if (dirty) postwrite ();

shift (origin + iopos);

if (out_len < total) {
	copy_from (out_buf, offset, out_len);
	iopos += out_len;
	used = out_len;
	dirty = true;
	}
else {
	// (direct write: no buffering)
	if ((count = base_write_at (origin + iopos, out_buf, offset, out_len)) < 0)
		return offset;

	iopos += count;
	}

return offset + count;
}	// IOBuffer::write

// Get octet from IOBuffer to 'octet'
// (Returns true on success)
bool IOBuffer::get_octet (char &octet) {
if (0 <= iopos && iopos < used) {
	// Easy way is possible...
	octet = data[iopos ++];
	return true;
	}

// Get octet hard way...
return read (&octet, 1) == 1;
}	// IOBuffer::get_octet

// Put octet to IOBuffer from 'octet'
// (Returns true on success)
bool IOBuffer::put_octet (char octet) {
if (iopos == used && used < total) {
	// Easy way is possible...
	data[iopos ++] = octet;
	used ++;
	dirty = true;
	return true;
	}
else if (0 <= iopos && iopos < used) {
	// Easy way is possible...
	data[iopos ++] = octet;
	dirty = true;
	return true;
	}

// Put octet hard way..
return write (&octet, 1) == 1;
}	// IOBuffer::put_octet

// Seek to absolute position 'abspos'
int IOBuffer::seek (int abspos) {
iopos = abspos - origin + (abspos >= 0 ? 0 : io_base->get_length () + 1);
return iopos;
}	// IOBuffer::seek

// Seek by relative offset 'reloff'
int IOBuffer::skip (int relpos) {
iopos += relpos;		// ????
return iopos;
}	// IOBuffer::skip

// Tell current position in buffer
int IOBuffer::tell () {
return origin + iopos;
}	// IOBuffer::tell

// Check, is stream at end
bool IOBuffer::is_at_eos () {
if (iopos == used)
	return io_base->is_at_eos ();

return false;
}	// IOBuffer::is_at_eos

// Check for console
bool IOBuffer::is_console () {
return io_base->is_console ();
}	// IOBuffer::is_console

// Sync buffer
bool IOBuffer::sync () {
if (dirty) postwrite ();
else used = 0;

return true;
}	// IOBuffer::sync

// Get current stream length
int IOBuffer::get_length () {
sync ();

// TODO: check buffer

return io_base->get_length ();
}	// IOBuffer::get_length

// Set current stream length
int IOBuffer::set_length (unsigned length) {
sync ();

// TODO: truncate buffer

return io_base->set_length (length);
}	// IOBuffer::set_length

//
//	Debugging I/O layer
//

struct IODebug : IOSystem {
	IOSystem *io_base;		// (a base I/O layer to debug)

	unsigned bytes_in, bytes_out;

	// (constructor)
	IODebug () {
		bytes_in = bytes_out = 0;
		}

	// (destructor)
	~IODebug () {
		}

	//
	//	Debug output
	//

	// (Start output record)
	void ArgEnter (char const *name) {
		syslog->put_cstr ("\t[IO_Debug]: ")->put_cstr (name)->put_cstr (" (");
		}	// ArgEnter

	// (End output record)
	void ArgLeave (int result) {
		syslog->put_cstr (") => ")->log_fixed (result)->put_nl ();
		}	// ArgLeave

	// (Fixed argument)
	void ArgValue (int value) {
		syslog->log_fixed (value);
		}	// ArgValue

	// (Block adress argument)
	void ArgBlock (const void *addr, unsigned count) {
		syslog->put_ch ('[')->put_hex (8, (unsigned) addr)->put_ch (':')->log_fixed (count)->put_ch (']');
		}	// ArgBlock

	//
	//	(Methods implemented)
	//

	// Close I/O subsystem
	// (return true on success)
	bool close () {
		bool state = io_base->close ();

		ArgEnter ("close");
		ArgLeave (state);

		return state;
		}	// close

	// Read from input stream to 'in_buf' ('in_count' bytes)
	int read (void *in_buf, unsigned in_count) {
		int count = io_base->read (in_buf, in_count);

		ArgEnter ("read");
		ArgBlock (in_buf, in_count);
		ArgLeave (count);

		return count;
		}	// read

	// Write to output stream from 'out_buf' ('out_count' bytes)
	int write (const void *out_buf, unsigned out_count) {
		int count = io_base->write (out_buf, out_count);

		ArgEnter ("write");
		ArgBlock (out_buf, out_count);
		ArgLeave (count);

		return count;
		}	// write

	// Skip by offset (backward/forward)
	int skip (int offset) {
		int pos = io_base->skip (offset);

		ArgEnter ("skip");
		ArgValue (offset);
		ArgLeave (pos);

		return pos;
		}	// skip

	// Seek to position (from beginning / from end)
	int seek (int position) {
		int pos = io_base->seek (position);

		ArgEnter ("seek");
		ArgValue (position);
		ArgLeave (pos);

		return pos;
		}	// seek

	// Tell current position
	int tell () {
		int pos = io_base->tell ();

		ArgEnter ("tell");
		ArgLeave (pos);

		return pos;
		}	// tell

	// Put single octet
	bool put_octet (char octet) {
		bool flag = io_base->put_octet (octet);

		ArgEnter ("put_octet");
		ArgValue (octet);
		ArgLeave (flag);
		return flag;
		}	// put_octet

	// Get single octet
	bool get_octet (char &octet) {
		bool flag = io_base->get_octet (octet);

		ArgEnter ("get_octet");
		ArgValue (octet);
		ArgLeave (flag);
		return flag;
		}	// get_octet

	// Sync
	bool sync () {
		bool flag = io_base->sync ();

		ArgEnter ("sync");
		ArgLeave (flag);
		return flag;
		}	// sync

	// Check for end
	bool is_at_eos () {
		bool flag = io_base->is_at_eos ();

		ArgEnter ("is_at_eos");
		ArgLeave (flag);
		return flag;
		}	// is_at_eos

	// Get current stream length
	int get_length () {
		int length = io_base->get_length ();

		ArgEnter ("get_length");
		ArgLeave (length);
		return length;
		}	// get_length

	// Set current stream length
	int set_length (unsigned length) {
		int res = io_base->set_length (length);

		ArgEnter ("set_length");
		ArgValue (length);
		ArgLeave (res);
		return length;
		}	// set_length

	// Check for console
	bool is_console () {
		bool flag = io_base->is_console ();

		ArgEnter ("is_console");
		ArgLeave (flag);
		return flag;
		}	// is_console

	// Logging stream
	void log (Logger &log) { log.put_cstr ("Debug: "); io_base->log (log); }

	// Getting stream info
	Expr *get_attr ()
		{ return new X_List (new X_String ("Debug"), io_base->get_attr ()); }

	};	// IODebug

#endif
