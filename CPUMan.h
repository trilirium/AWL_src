
/*

	+---+---+---+---+---+---+
	|	"CPUMan.h":
	|	CPU management
	|	(currently, only calculates CPU timing and load)
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

// Start CPU accounting
void cpu_start ();

// Start of CPU activity
void cpu_capture ();

// End of CPU activity
void cpu_release ();

// End CPU accounting
void cpu_finish (double &cpu_total, double &cpu_used);

