
/*

	+---+---+---+---+---+---+
	|	"Coerce.cpp":
	|	All numbers <-> strings coercion logic is implemented here.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

//
// FP conversion format
//
enum FC_format {

	FC_format_f,		// (as %f)
	FC_format_e,		// (as %e)
	FC_format_g			// (as %g)

	};

//
//	String accumulator
//	Generic output buffer for coerced values
//

struct FormatGen {

	// (Put character 'ch')
	virtual void putc (char ch) = 0;

	// (Put repeated character 'ch'['count'])
	virtual void putr (char ch, unsigned count) = 0;

	// (Put C-string 'str')
	virtual void puts (char const *str) = 0;

	// (Put fragment from memory 'src'['len'])
	virtual void putm (char *src, unsigned cnt) = 0;

	//
	// Output float value
	//

	// Format fixed 'value' by 'base' (unsigned)
	void format_fixed (unsigned value, unsigned base, bool upcase);

	// Format fixed 'value' by 'base' (signed)
	void format_signed (int value, unsigned base, bool upcase);

	// Format float 'value' (according to 'format', with 'digits' & 'e_shift')
	void format_float (double value, FC_format format, int digits, int e_shift);

	};		// FormatGen

#ifndef HEADER

#include <cmath>
#include <cstdlib>

//
//	Floating point conversions
//

// Format fixed 'value' by 'base' (unsigned)
void FormatGen::format_fixed (unsigned value, unsigned base, bool upcase) {
unsigned div = 1;

while (value / div >= base) div *= base;

do {
	unsigned digit = (value / div) % base;
	putc (digit >= 10 ? (upcase ? 'A' : 'a') + digit - 10 : '0' + digit);
	} while (div /= base);
}	// FormatGen::format_fixed

// Format fixed 'value' by 'base' (signed)
void FormatGen::format_signed (int value, unsigned base, bool upcase) {
if (value < 0) { putc ('-'); value = -value; }

format_fixed (value, base, upcase);
}	// FormatGen::format_signed

// Mantissa digits count
enum { mantissa_cnt = 18 };

// Format float 'value' (according to 'format', with 'digits' & 'e_shift')
void FormatGen::format_float (double value, FC_format format, int digits, int e_shift) {
int sign, exp = 0;
double digit;
bool no_trail_zeroes = false;

char mantissa [mantissa_cnt];

// Check for Inf/NaN
if (value * 0.0 != 0.0) {
	// (...either Inf or NaN...)
	if (value != value)
		// Not A Number
		puts ("#NaN");
	else {
		// +/- Infinity
		putc (value < 0 ? '-' : '+');
		puts ("#Inf");
		}

	return;
	}

// Check for sign
if (value < 0.0) {
	sign = -1;
	value = -value;
	}
else if (value > 0.0) {
	sign = 1;
	}
else {
	// Float zero
	if (format == FC_format_f) {
		if (digits < 0) digits = 0;
		putc ('.');
		putr ('0', digits);
		}
	else {
		putc ('0');
		putc ('.');
		}
	return;
	}

// fast normalize
if (value < 0.001 || value >= 1000.0) {
	exp = (int) ceil (log10 (value));
	value *= pow (10.0, -exp);
	}

if (value < 0.1)
	do -- exp;
	while ((value *= 10.0) < 0.1);
else if (value >= 1.0)
	do ++ exp;
	while ((value *= 0.1) >= 1.0);

// exp: exponent part
// value: normalized (0.1 <= value < 1.0)

// (Sign)
if (sign < 0) putc ('-');
else if (format != FC_format_g) putc (' ');

switch (format) {
	case FC_format_f:				// (format F)
		if ((digits += exp) < 0)
			digits = 0;
		break;

	case FC_format_e:				// (format E)
		if (digits < 0) digits = 0;
		break;

	case FC_format_g:				// (format G: choose either E or F)
		format = (exp > 0 ? digits < exp : exp < -2 + e_shift) ? FC_format_e : FC_format_f;
		if (format == FC_format_e) {
			if (e_shift > digits) digits = e_shift;
			}
		else
			no_trail_zeroes = true;
		break;
	}	// switch (format)

//
//	(generate decimal mantissa [digits])
//

{
unsigned pos = 0;
while (pos != digits) {
    double digit;
    value = modf (value * 10.0, &digit);
	mantissa[pos ++] = '0' + (int)digit;
	}

// final rounding...
if (value >= 0.5) {
	if (value < 0.6) {
		// try to extract one more significant digit:
		value = modf (value * 10.0, &digit);
		if (digit < 0.5)
			goto done;
		}

	if (pos) {
	// round up...
	do {
		if (mantissa[-- pos] ++ == '9')
			mantissa[pos] = '0';
		else goto done;
	} while (pos);
	}

	mantissa[0] = '1';			// ????? shift
	++ exp;
	if (digits) {
		if (format == FC_format_f) mantissa[digits ++] = '0';
		}
	else digits ++;
	}	// value >= 0.5

}

done:

// Format conversion
if (format == FC_format_f) {			// (F-format)

// Numeric part
if (exp < 0) {
	putc ('0');
	putc ('.');
	putr ('0', -exp);

	if (no_trail_zeroes) {
		while (digits && mantissa[digits - 1] == '0')
			-- digits;
		}

	putm (mantissa, digits);
	}
else {
	if (exp <= digits) {
		exp ? putm (mantissa, exp) : putc ('0');
		putc ('.');

		if (no_trail_zeroes) {
			while (digits > exp && mantissa[digits - 1] == '0')
				-- digits;
			}

		putm (mantissa + exp, digits - exp);
		}
	else {		// (exp > digits)
		if (digits > 0) {
			putm (mantissa, digits);
			putr ('0', exp - digits);
			putc ('.');
			}
		else putc ('*');			// (total loss of sigdigits!)
		}
	}
	}		// (F-format)
else {								// (E-format)
	// Mantissa part
	
	// (always kill trailing zeroes)
	while (digits && mantissa[digits - 1] == '0')
		-- digits;

	if (e_shift < 0) {
		putc ('.');
		putr ('0', - e_shift);
		putm (mantissa, digits);
		}
	else if (e_shift < digits) {
		e_shift ? putm (mantissa, e_shift) : putc ('0');
		putc ('.');
		putm (mantissa + e_shift, digits - e_shift);
		}
	else {	// (e_shift >= digits)
		putm (mantissa, digits);
		putr ('0', e_shift - digits);
		putc ('.');
		}

	// Exponent part
	putc ('e');
	exp -= e_shift;

	// TODO : remove e0 ????

	if (exp < 0) { putc ('-'); exp = -exp; }
	else if (exp > 0) { putc ('+'); }
	else { putc ('0'); return; }

	format_fixed (exp, 10, false);
	}		// (E-format)
}	// FormatGen::format_float

#endif

//
//	String parser
//

//	Generic input buffer for coercion
struct ParseGen {
	// (returns 0 on end of parse buffer)
	virtual unsigned get_ch () = 0;

	// Parse & return fixed value (by 'base')
	unsigned parse_fixed (unsigned base);

	// Parse & return float value
	double parse_float ();

	// Parse signature string
	unsigned parse_sigstr (unsigned count, int cflag);
	};

#define _shift(v,c) (((v) << 8) | (c))

#define _signature3(c1,c2,c3)	_shift(_shift(c1, c2), c3)

#ifndef HEADER

unsigned ParseGen::parse_sigstr (unsigned count, int cflag) {
unsigned acc = 0, val;

while (count -- && (val = get_ch())) {
	acc <<= 8;
	acc |= (unsigned char) val;
	}

return acc;
}	// ParseGen::parse_sigstr

// Raise to integer power: value * power^exponent
double i_power (double value, double power, int exponent) {
    double result = 1.0;
    bool negative = false;
    unsigned bit;

    if (value == 0.0 || exponent == 0) return value;

    if (exponent < 0) {
		negative = 1;
		exponent = -exponent;
		}

	for (;;) {
	if (exponent & 1) {
	    result *= power;
	    if (! (exponent ^= 1)) break;
		}

	power *= power;
	exponent >>= 1;
	}	// for

    return negative ? value / result : value * result;
}	// i_power

#define	MIN_INT		(1 << 31)
#define	MAX_INT		~(1 << 31)

// Parse integer value (by 'base' specified)
unsigned ParseGen::parse_fixed (unsigned base) {
bool negative = false;
unsigned value = 0;
unsigned cur_ch;

// Check sign
repeat:
switch (cur_ch = get_ch ()) {
	case '-':	negative = true;
				// (fall through)
	case '+':	cur_ch = get_ch();
				break;

	case ' ' : case '\t': case '\v':
	case '\n': case '\r': case '\f':
		// (skip blanks)
		goto repeat;

	default:	break;					// (continue)
	}	// switch

if (cur_ch == '#') {
	// (special)
	switch (parse_sigstr (3, 1)) {
		case _signature3('M', 'I', 'N'):
			return MIN_INT;

		case _signature3('M', 'A', 'X'):
			return MAX_INT;

		default:
			return 0;
		}
	}

// base must be valid!
if (base < 2 || base > 36) return 0;

for (;;) {
	if ('0' <= cur_ch && cur_ch <= '9')
		cur_ch -= '0';
	else if ('A' <= cur_ch && cur_ch <= 'Z')
		cur_ch += 10 - 'A';
	else if ('a' <= cur_ch && cur_ch <= 'z')
		cur_ch += 10 - 'a';
	else if (cur_ch == '_');			// (allow '_' in numbers)
	else break;		// (not a valid digit)

	if (cur_ch >= base) break;		// (not a valid digit)

	value = value * base + cur_ch;

	// TODO: check overflow???

	cur_ch = get_ch ();
	}	// for (;;)

return negative ? -value : value;
}	// ParseGen::parse_fixed

// Convert float value, with mantissa[digits] and exp
double convert_float (bool neg, unsigned char *mantissa, unsigned digits, int exp) {
double val = 0.0;
unsigned i_val = 0, i_shift = 0;

// ignore leading zeroes
while (digits && ! *mantissa) { ++ mantissa, -- digits; }

while (digits --) {
	i_val = i_val * 10 + (* mantissa ++);
	i_shift ++;
	
	if (i_val > 10000) {
		val = i_power (val, 10.0, i_shift) + double (i_val);
		i_val = i_shift = 0;
		}
	}

if (i_shift) val = i_power (val, 10.0, i_shift) + double (i_val);
val = i_power(val, 10.0, exp);

return neg ? -val : val;
}	// convert_float

// Parse float value
double ParseGen::parse_float () {

bool negative = false;
unsigned char mantissa [mantissa_cnt];
unsigned digits = 0;
unsigned offset = 0;

unsigned cur_ch;

// Check sign
repeat:
switch (cur_ch = get_ch ()) {
	default:	break;			// (continue)

	case ' ' : case '\t': case '\v':
	case '\n': case '\r': case '\f':
		goto repeat;

	case '-':	negative = true;
				// (fall through)
	case '+':	cur_ch = get_ch();
				break;

	}	// switch

if (cur_ch == '#') {
	// (special)
	switch (parse_sigstr (3, 1)) {
		case _signature3('N', 'A', 'N'):
			return 0.0 / 0.0;

		case _signature3('I', 'N', 'F'):
			return (negative ? -1.0 : 1.0) / 0.0;
			
		default:
			return 0.0;
		}
	}

for (; ; cur_ch = get_ch ()) {
	if ('0' <= cur_ch && cur_ch <= '9') {
		if (digits != mantissa_cnt)
			mantissa[digits ++] = cur_ch - '0';
		if (offset) ++ offset;
		}

	else if (cur_ch == '.') {
		if (offset) break;				// (already was '.')
		else ++ offset;
		}

	else if (cur_ch == '_');			// (allow '_' in numbers)

	else break;
	}

int exp = 0;
switch (cur_ch) {
	case 'e': case 'E':
		exp = parse_fixed (10);
	}	// switch

return convert_float (negative, mantissa, digits, exp + (offset ? 1 - offset : 0));
}	// ParseGen::parse_float

#endif

