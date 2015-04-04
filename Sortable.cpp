
/*

	+---+---+---+---+---+---+
	|	"Sortable.cpp":
	|	Sortable objects and sorting methods.
	|
	|	AWL source code.
	|	Written by trilirium, 2014
	+---+---+---+---+---+---+

 */

struct Sortable {
	unsigned N;				// # of items to sort...

	Sortable (unsigned N);

	// compare items (I, J)
	virtual int compare (unsigned I, unsigned J) = 0;

	// swap items (I, J)
	virtual void swap (unsigned I, unsigned J) = 0;
	
	// Simple exchange sort
	void x_sort ();

	// quick sort
	void q_sort_rec (bool dir, int lo, int hi);
	void q_sort ();

	// 
	void horsley_qsort ();

	// do sorting
	void sort ();
	};		// Sortable

#ifndef	HEADER

Sortable::Sortable (unsigned N) { this->N = N; }

// compare items (I, J)
int Sortable::compare (unsigned I, unsigned J) { return 0; }

// swap items (I, J)
void Sortable::swap (unsigned I, unsigned J) {}

// Simple exchange sort
void Sortable::x_sort () {
if (N) {
	for (unsigned I = 1; I != N; ++ I) {
		for (unsigned J = I; J && compare (J - 1, J) > 0; -- J)
			swap (J - 1, J);
	}   // (I loop)
}
}	// Sortable::x_sort

#if 0

// Binary quicksort algorythm
void Sortable::q_sort (int lo0, int hi0) {
if (lo0 < hi0) {
	int lo = lo0, hi = hi0, mid = (lo0 + hi0) / 2;
	while (lo <= hi) {
		while (lo < hi0 && compare (lo, mid) < 0) lo ++;
		while (hi > lo0 && compare (hi, mid) > 0) hi --;

		if (lo <= hi) swap (lo ++, hi --);
		}	// while (lo <= hi)

	if (lo0 < hi) q_sort (lo0, hi);
	if (lo < hi0) q_sort (lo, hi0);
	}
}	// Sortable::q_sort

#endif

// Binary quicksort algorythm
void Sortable::q_sort_rec (bool dir, int lo, int hi) {
if (lo < hi) {
	switch (hi - lo) {
		case 1:		// (2 elements fast reorder)
			if (compare (lo, hi) > 0)
				swap (lo, hi);
			return;

		case 2:		// (3 elements fast reorder)
			if (compare (lo, hi) > 0)
				swap (lo, hi);

			if (compare (lo+1, lo) < 0)
				swap (lo, lo+1);
			else if (compare (hi-1, hi) > 0)
				swap (hi, hi-1);

			return;

		default: {
			int i, pivot, mid = (lo + hi) / 2;

			if (dir) {			// (incrementally)
				if (compare (pivot = lo, mid) < 0)
					swap (pivot, mid);

				for (i = lo + 1; i <= hi; ++ i)
					if (compare (i, pivot) < 0 && ++ lo != i)
						swap (lo, i);

				if (lo != pivot) swap (lo, pivot);

				// (recursive sort both partitions)
				q_sort_rec (false, pivot, lo - 1);
				q_sort_rec (true, lo + 1, hi);
				}	// (inc)

			else {			// (decrementally)
				if (compare (pivot = hi, mid) > 0)
					swap (pivot, mid);

				for (i = hi - 1; i >= lo; -- i)
					if (compare (i, pivot) > 0 && -- hi != i)
						swap (hi, i);

				if (hi != pivot) swap (hi, pivot);

				// recursive sort both partitions:
				q_sort_rec (true, lo, hi - 1);
				q_sort_rec (false, hi + 1, pivot);
				}	// (dec)
			}
		}	// switch (hi - lo)
	}	// (lo < hi)
}	// Sortable::q_sort_rec

// Q-sort proper
void Sortable::q_sort () { q_sort_rec (false, 0, N - 1); }

// Sorting...
void Sortable::sort () {
// x_sort ();
q_sort ();
}	// Sortable::sort

/*
 * The quicksort implementation was derived from source code contributed
 * by Tom Horsley.
 *
 * NOTE: this code was derived from Tom Horsley's qsort replacement
 * and should not be confused with the original code.
 */

/* Copyright (C) Tom Horsley, 1997. All rights reserved.

   Permission granted to distribute under the same terms as perl which are
   (briefly):

    This program is free software; you can redistribute it and/or modify
    it under the terms of either:

	a) the GNU General Public License as published by the Free
	Software Foundation; either version 1, or (at your option) any
	later version, or

	b) the "Artistic License" which comes with this Kit.

   Details on the perl license can be found in the perl source code which
   may be located via the www.perl.com web page.

   This is the most wonderfulest possible qsort I can come up with (and
   still be mostly portable) My (limited) tests indicate it consistently
   does about 20% fewer calls to compare than does the qsort in the Visual
   C++ library, other vendors may vary.

   Some of the ideas in here can be found in "Algorithms" by Sedgewick,
   others I invented myself (or more likely re-invented since they seemed
   pretty obvious once I watched the algorithm operate for a while).

   Most of this code was written while watching the Marlins sweep the Giants
   in the 1997 National League Playoffs - no Braves fans allowed to use this
   code (just kidding :-).

   I realize that if I wanted to be true to the perl tradition, the only
   comment in this file would be something like:

   ...they shuffled back towards the rear of the line. 'No, not at the
   rear!'  the slave-driver shouted. 'Three files up. And stay there...

   However, I really needed to violate that tradition just so I could keep
   track of what happens myself, not to mention some poor fool trying to
   understand this years from now :-).
*/

/* ********************************************************** Configuration */

#ifndef QSORT_ORDER_GUESS
#define QSORT_ORDER_GUESS 2	/* Select doubling version of the netBSD trick */
#endif

/* QSORT_MAX_STACK is the largest number of partitions that can be stacked up for
   future processing - a good max upper bound is log base 2 of memory size
   (32 on 32 bit machines, 64 on 64 bit machines, etc). In reality can
   safely be smaller than that since the program is taking up some space and
   most operating systems only let you grab some subset of contiguous
   memory (not to mention that you are normally sorting data larger than
   1 byte element size :-).
*/
#ifndef QSORT_MAX_STACK
#define QSORT_MAX_STACK 32
#endif

/* QSORT_BREAK_EVEN is the size of the largest partition we should insertion sort.
   Anything bigger and we use qsort. If you make this too small, the qsort
   will probably break (or become less efficient), because it doesn't expect
   the middle element of a partition to be the same as the right or left -
   you have been warned).
*/
#ifndef QSORT_BREAK_EVEN
#define QSORT_BREAK_EVEN 6
#endif

/* QSORT_PLAY_SAFE is the size of the largest partition we're willing
   to go quadratic on.  We innoculate larger partitions against
   quadratic behavior by shuffling them before sorting.  This is not
   an absolute guarantee of non-quadratic behavior, but it would take
   staggeringly bad luck to pick extreme elements as the pivot
   from randomized data.
*/
#ifndef QSORT_PLAY_SAFE
#define QSORT_PLAY_SAFE 255
#endif

/* ************************************************************* Data Types */

/* hold left and right index values of a partition waiting to be sorted (the
   partition includes both left and right - right is NOT one past the end or
   anything like that).
*/
struct partition_stack_entry {
   int left;
   int right;

#ifdef QSORT_ORDER_GUESS
   int qsort_break_even;
#endif
};

/* ******************************************************* Shorthand Macros */

/* Note that these macros will be used from inside the qsort function where
   we happen to know that the variable 'elt_size' contains the size of an
   array element and the variable 'temp' points to enough space to hold a
   temp element and the variable 'array' points to the array being sorted
   and 'compare' is the pointer to the compare routine.

   Also note that there are very many highly architecture specific ways
   these might be sped up, but this is simply the most generally portable
   code I could think of.
*/

/* Return < 0 == 0 or > 0 as the value of elt1 is < elt2, == elt2, > elt2 */
#define	qsort_cmp(elt1, elt2)	Sortable::compare (elt1, elt2)

#ifdef QSORT_ORDER_GUESS
#define QSORT_NOTICE_SWAP swapped ++
#else
#define QSORT_NOTICE_SWAP
#endif

/* swaps contents of array elements elt1, elt2. */
#define	qsort_swap(elt1, elt2)				\
	( QSORT_NOTICE_SWAP, Sortable::swap(elt1, elt2) )

/* rotate contents of elt1, elt2, elt3 such that elt1 gets elt2, elt2 gets elt3 and elt3 gets elt1. */
#define qsort_rotate(elt1, elt2, elt3)		\
	( QSORT_NOTICE_SWAP, Sortable::swap(elt1, elt2), Sortable::swap(elt2, elt3) )

void Sortable::horsley_qsort () {

   struct partition_stack_entry partition_stack[QSORT_MAX_STACK];
   int next_stack_entry = 0;

   int part_left;
   int part_right;

#ifdef QSORT_ORDER_GUESS
   int qsort_break_even;
   int swapped;
#endif

   /* Make sure we actually have work to do. */
   if (N <= 1) return;

   /* Setup the initial partition definition and fall into the sorting loop */
   part_left = 0;
   part_right = (int) (N - 1);

#ifdef QSORT_ORDER_GUESS
   qsort_break_even = QSORT_BREAK_EVEN;
#else
#define qsort_break_even QSORT_BREAK_EVEN
#endif

   for ( ; ; ) {
      if ((part_right - part_left) >= qsort_break_even) {
         /* OK, this is gonna get hairy, so lets try to document all the
            concepts and abbreviations and variables and what they keep
            track of:

            pc: pivot chunk - the set of array elements we accumulate in the
                middle of the partition, all equal in value to the original
                pivot element selected. The pc is defined by:

                pc_left - the leftmost array index of the pc
                pc_right - the rightmost array index of the pc

                we start with pc_left == pc_right and only one element
                in the pivot chunk (but it can grow during the scan).

            u:  uncompared elements - the set of elements in the partition
                we have not yet compared to the pivot value. There are two
                uncompared sets during the scan - one to the left of the pc
                and one to the right.

                u_right - the rightmost index of the left side's uncompared set
                u_left - the leftmost index of the right side's uncompared set

                The leftmost index of the left sides's uncompared set
                doesn't need its own variable because it is always defined
                by the leftmost edge of the whole partition (part_left). The
                same goes for the rightmost edge of the right partition
                (part_right).

                We know there are no uncompared elements on the left once we
                get u_right < part_left and no uncompared elements on the
                right once u_left > part_right. When both these conditions
                are met, we have completed the scan of the partition.

                Any elements which are between the pivot chunk and the
                uncompared elements should be less than the pivot value on
                the left side and greater than the pivot value on the right
                side (in fact, the goal of the whole algorithm is to arrange
                for that to be true and make the groups of less-than and
                greater-then elements into new partitions to sort again).

            As you marvel at the complexity of the code and wonder why it
            has to be so confusing. Consider some of the things this level
            of confusion brings:

            Once I do a compare, I squeeze every ounce of juice out of it. I
            never do compare calls I don't have to do, and I certainly never
            do redundant calls.

            I also never swap any elements unless I can prove there is a
            good reason. Many sort algorithms will swap a known value with
            an uncompared value just to get things in the right place (or
            avoid complexity :-), but that uncompared value, once it gets
            compared, may then have to be swapped again. A lot of the
            complexity of this code is due to the fact that it never swaps
            anything except compared values, and it only swaps them when the
            compare shows they are out of position.
         */
         int pc_left, pc_right;
         int u_right, u_left;

         int s;

         pc_left = ((part_left + part_right) / 2);
         pc_right = pc_left;
         u_right = pc_left - 1;
         u_left = pc_right + 1;

         /* Qsort works best when the pivot value is also the median value
            in the partition (unfortunately you can't find the median value
            without first sorting :-), so to give the algorithm a helping
            hand, we pick 3 elements and sort them and use the median value
            of that tiny set as the pivot value.

            Some versions of qsort like to use the left middle and right as
            the 3 elements to sort so they can insure the ends of the
            partition will contain values which will stop the scan in the
            compare loop, but when you have to call an arbitrarily complex
            routine to do a compare, its really better to just keep track of
            array index values to know when you hit the edge of the
            partition and avoid the extra compare. An even better reason to
            avoid using a compare call is the fact that you can drop off the
            edge of the array if someone foolishly provides you with an
            unstable compare function that doesn't always provide consistent
            results.

            So, since it is simpler for us to compare the three adjacent
            elements in the middle of the partition, those are the ones we
            pick here (conveniently pointed at by u_right, pc_left, and
            u_left). The values of the left, center, and right elements
            are refered to as l c and r in the following comments.
         */

#ifdef QSORT_ORDER_GUESS
         swapped = 0;
#endif
         s = qsort_cmp(u_right, pc_left);
         if (s < 0) {
            /* l < c */
            s = qsort_cmp(pc_left, u_left);
            /* if l < c, c < r - already in order - nothing to do */
            if (s == 0) {
               /* l < c, c == r - already in order, pc grows */
               ++ pc_right;
            } else if (s > 0) {
               /* l < c, c > r - need to know more */
               s = qsort_cmp(u_right, u_left);
               if (s < 0) {
                  /* l < c, c > r, l < r - swap c & r to get ordered */
                  qsort_swap(pc_left, u_left);
               } else if (s == 0) {
                  /* l < c, c > r, l == r - swap c&r, grow pc */
                  qsort_swap(pc_left, u_left);
                  -- pc_left;
               } else {
                  /* l < c, c > r, l > r - make lcr into rlc to get ordered */
                  qsort_rotate(pc_left, u_right, u_left);
               }
            }
         } else if (s == 0) {
            /* l == c */
            s = qsort_cmp(pc_left, u_left);
            if (s < 0) {
               /* l == c, c < r - already in order, grow pc */
               -- pc_left;
            } else if (s == 0) {
               /* l == c, c == r - already in order, grow pc both ways */
               -- pc_left;
               ++ pc_right;
            } else {
               /* l == c, c > r - swap l & r, grow pc */
               qsort_swap(u_right, u_left);
               ++ pc_right;
            }
         } else {
            /* l > c */
            s = qsort_cmp(pc_left, u_left);
            if (s < 0) {
               /* l > c, c < r - need to know more */
               s = qsort_cmp(u_right, u_left);
               if (s < 0) {
                  /* l > c, c < r, l < r - swap l & c to get ordered */
                  qsort_swap(u_right, pc_left);
               } else if (s == 0) {
                  /* l > c, c < r, l == r - swap l & c, grow pc */
                  qsort_swap(u_right, pc_left);
                  ++ pc_right;
               } else {
                  /* l > c, c < r, l > r - rotate lcr into crl to order */
                  qsort_rotate(u_right, pc_left, u_left);
               }
            } else if (s == 0) {
               /* l > c, c == r - swap ends, grow pc */
               qsort_swap(u_right, u_left);
               -- pc_left;
            } else {
               /* l > c, c > r - swap ends to get in order */
               qsort_swap(u_right, u_left);
            }
         }
         /* We now know the 3 middle elements have been compared and
            arranged in the desired order, so we can shrink the uncompared
            sets on both sides
         */
         -- u_right;
         ++ u_left;

         /* The above massive nested if was the simple part :-). We now have
            the middle 3 elements ordered and we need to scan through the
            uncompared sets on either side, swapping elements that are on
            the wrong side or simply shuffling equal elements around to get
            all equal elements into the pivot chunk.
         */

         for ( ; ; ) {
            int still_work_on_left, still_work_on_right;

            /* Scan the uncompared values on the left. If I find a value
               equal to the pivot value, move it over so it is adjacent to
               the pivot chunk and expand the pivot chunk. If I find a value
               less than the pivot value, then just leave it - its already
               on the correct side of the partition. If I find a greater
               value, then stop the scan.
            */
            while ((still_work_on_left = (u_right >= part_left))) {
               s = qsort_cmp(u_right, pc_left);
               if (s < 0) {
                  -- u_right;
               } else if (s == 0) {
                  -- pc_left;
                  if (pc_left != u_right) {
                     qsort_swap(u_right, pc_left);
                  }
                  -- u_right;
               } else {
                  break;
               }
            }

            /* Do a mirror image scan of uncompared values on the right */
            while ((still_work_on_right = (u_left <= part_right))) {
               s = qsort_cmp(pc_right, u_left);
               if (s < 0) {
                  ++ u_left;
               } else if (s == 0) {
                  ++ pc_right;
                  if (pc_right != u_left) {
                     qsort_swap(pc_right, u_left);
                  }
                  ++ u_left;
               } else {
                  break;
               }
            }

            if (still_work_on_left) {
               /* I know I have a value on the left side which needs to be
                  on the right side, but I need to know more to decide
                  exactly the best thing to do with it.
               */
               if (still_work_on_right) {
                  /* I know I have values on both side which are out of
                     position. This is a big win because I kill two birds
                     with one swap (so to speak). I can advance the
                     uncompared pointers on both sides after swapping both
                     of them into the right place.
                  */
                  qsort_swap(u_right, u_left);
                  -- u_right;
                  ++ u_left;
               } else {
                  /* I have an out of position value on the left, but the
                     right is fully scanned, so I "slide" the pivot chunk
                     and any less-than values left one to make room for the
                     greater value over on the right. If the out of position
                     value is immediately adjacent to the pivot chunk (there
                     are no less-than values), I can do that with a swap,
                     otherwise, I have to rotate one of the less than values
                     into the former position of the out of position value
                     and the right end of the pivot chunk into the left end
                     (got all that?).
                  */
                  -- pc_left;
                  if (pc_left == u_right) {
                     qsort_swap(u_right, pc_right);
                  } else {
                     qsort_rotate(u_right, pc_left, pc_right);
                  }
                  -- pc_right;
                  -- u_right;
               }
            } else if (still_work_on_right) {
               /* Mirror image of complex case above: I have an out of
                  position value on the right, but the left is fully
                  scanned, so I need to shuffle things around to make room
                  for the right value on the left.
               */
               ++ pc_right;
               if (pc_right == u_left) {
                  qsort_swap(u_left, pc_left);
               } else {
                  qsort_rotate(pc_right, pc_left, u_left);
               }
               ++ pc_left;
               ++ u_left;
            } else {
               /* No more scanning required on either side of partition,
                  break out of loop and figure out next set of partitions
               */
               break;
            }
         }

         /* The elements in the pivot chunk are now in the right place. They
            will never move or be compared again. All I have to do is decide
            what to do with the stuff to the left and right of the pivot
            chunk.

            Notes on the QSORT_ORDER_GUESS ifdef code:

            1. If I just built these partitions without swapping any (or
               very many) elements, there is a chance that the elements are
               already ordered properly (being properly ordered will
               certainly result in no swapping, but the converse can't be
               proved :-).

            2. A (properly written) insertion sort will run faster on
               already ordered data than qsort will.

            3. Perhaps there is some way to make a good guess about
               switching to an insertion sort earlier than partition size 6
               (for instance - we could save the partition size on the stack
               and increase the size each time we find we didn't swap, thus
               switching to insertion sort earlier for partitions with a
               history of not swapping).

            4. Naturally, if I just switch right away, it will make
               artificial benchmarks with pure ascending (or descending)
               data look really good, but is that a good reason in general?
               Hard to say...
         */

#ifdef QSORT_ORDER_GUESS
         if (swapped < 3) {
#if QSORT_ORDER_GUESS == 1
            qsort_break_even = (part_right - part_left) + 1;
#endif
#if QSORT_ORDER_GUESS == 2
            qsort_break_even *= 2;
#endif
#if QSORT_ORDER_GUESS == 3
            const int prev_break = qsort_break_even;
            qsort_break_even *= qsort_break_even;
            if (qsort_break_even < prev_break) {
               qsort_break_even = (part_right - part_left) + 1;
            }
#endif
         } else {
            qsort_break_even = QSORT_BREAK_EVEN;
         }
#endif

         if (part_left < pc_left) {
            /* There are elements on the left which need more processing.
               Check the right as well before deciding what to do.
            */
            if (pc_right < part_right) {
               /* We have two partitions to be sorted. Stack the biggest one
                  and process the smallest one on the next iteration. This
                  minimizes the stack height by insuring that any additional
                  stack entries must come from the smallest partition which
                  (because it is smallest) will have the fewest
                  opportunities to generate additional stack entries.
               */
               if ((part_right - pc_right) > (pc_left - part_left)) {
                  /* stack the right partition, process the left */
                  partition_stack[next_stack_entry].left = pc_right + 1;
                  partition_stack[next_stack_entry].right = part_right;
#ifdef QSORT_ORDER_GUESS
                  partition_stack[next_stack_entry].qsort_break_even = qsort_break_even;
#endif
                  part_right = pc_left - 1;
               } else {
                  /* stack the left partition, process the right */
                  partition_stack[next_stack_entry].left = part_left;
                  partition_stack[next_stack_entry].right = pc_left - 1;
#ifdef QSORT_ORDER_GUESS
                  partition_stack[next_stack_entry].qsort_break_even = qsort_break_even;
#endif
                  part_left = pc_right + 1;
               }
               ++ next_stack_entry;
            } else {
               /* The elements on the left are the only remaining elements
                  that need sorting, arrange for them to be processed as the
                  next partition.
               */
               part_right = pc_left - 1;
            }
         } else if (pc_right < part_right) {
            /* There is only one chunk on the right to be sorted, make it
               the new partition and loop back around.
            */
            part_left = pc_right + 1;
         } else {
            /* This whole partition wound up in the pivot chunk, so
               we need to get a new partition off the stack.
            */
            if (next_stack_entry == 0) {
               /* the stack is empty - we are done */
               break;
            }
            -- next_stack_entry;
            part_left = partition_stack[next_stack_entry].left;
            part_right = partition_stack[next_stack_entry].right;

#ifdef QSORT_ORDER_GUESS
            qsort_break_even = partition_stack[next_stack_entry].qsort_break_even;
#endif
         }
      } else {
         /* This partition is too small to fool with qsort complexity, just
            do an ordinary insertion sort to minimize overhead.
         */
         int i;
         /* Assume 1st element is in right place already, and start checking
            at 2nd element to see where it should be inserted.
         */
         for (i = part_left + 1; i <= part_right; ++i) {
            int j;
            /* Scan (backwards - just in case 'i' is already in right place)
               through the elements already sorted to see if the ith element
               belongs ahead of one of them.
            */
            for (j = i - 1; j >= part_left; --j) {
               if (qsort_cmp(i, j) >= 0) {
                  /* i belongs right after j
                  */
                  break;
               }
            }
            ++ j;
            if (j != i) {
               /* Looks like we really need to move some things */
#if 0
	       int k;
		      register SV * temp;

	       temp = array[i];
	       for (k = i - 1; k >= j; --k)
		  array[k + 1] = array[k];
               array[j] = temp;
#endif

		    for (int k = i - 1; k >= j; -- k)
				qsort_swap (k + 1, k);
            }
         }

         /* That partition is now sorted, grab the next one, or get out
            of the loop if there aren't any more.
         */

         if (next_stack_entry == 0) {
            /* the stack is empty - we are done */
            break;
         }
         -- next_stack_entry;
         part_left = partition_stack[next_stack_entry].left;
         part_right = partition_stack[next_stack_entry].right;

#ifdef QSORT_ORDER_GUESS
         qsort_break_even = partition_stack[next_stack_entry].qsort_break_even;
#endif
      }
   }

   /* Believe it or not, the array is sorted at this point! */
}

#endif
