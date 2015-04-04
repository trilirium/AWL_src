
/*

	+---+---+---+---+---+---+
	|	"CPUMan.cpp":
	|	CPU management
	|	(currently, only calculates CPU timing and load)
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

#include <time.h>

static clock_t time_start;
static clock_t cpu_begin;
static clock_t cpu_total;

static unsigned cpu_latch = 0;

// Start CPU accounting
void cpu_start () {
time_start = clock();
cpu_total = 0;
}	// cpu_start

// Start of CPU activity
void cpu_capture () {
if (! cpu_latch ++) cpu_begin = clock();
}	// cpu_capture

// End of CPU activity
void cpu_release () {
if (! -- cpu_latch) cpu_total += clock() - cpu_begin;
}	// cpu_release

// End CPU accounting
void cpu_finish (double &cpu_total, double &cpu_used) {
cpu_total = double (clock() - time_start) / CLOCKS_PER_SEC;
cpu_used = double (::cpu_total) / CLOCKS_PER_SEC;
}	// cpu_finish

