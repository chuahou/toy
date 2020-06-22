// SPDX-License-Identifier: MIT
// Copyright (c) 2020 Chua Hou
//
// Naive implementation of quicksort with pretty colorful printing to remind
// myself that C is a language and that I can program in something that isn't
// Scala or Haskell.

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define LEN 25  // length of array to work on
#define MAX 100 // max value to work on

#define PCOLOR(code) printf("\033" code)
#define WHITE()      PCOLOR("[0m")
#define BLUE()       PCOLOR("[1;34m")
#define GREEN()      PCOLOR("[1;32m")
#define YELLOW()     PCOLOR("[1;33m")
#define RED()        PCOLOR("[1;31m")

// display array with elements separated by spaces
void print(int* A, size_t N)
{
	for (size_t i = 0; i < N; i++) {
		printf("%d ", A[i]);
	}
}

// perform quicksort on A[l..r)
void quicksort(int* A, size_t l, size_t r)
{
	// do nothing if singleton or empty
	if (l + 1 >= r) return;

	// use middle as pivot
	size_t m = (l + r) / 2;
	int tmp = A[l]; A[l] = A[m]; A[m] = tmp; // move pivot to A[l]

	// perform partition
	// A[l+1..p) and A[q..r) are permutations of A_0[l+1..p) ++ A_0[q..r)
	// && A[l+1..p) <= A[l] < A[q..r) && l <= p <= q <= r
	size_t p = l + 1;
	size_t q = r;
	while (p < q) {
		if (A[p] < A[l]) {
			p++;
		} else {
			q--;
			tmp = A[p]; A[p] = A[q]; A[q] = tmp;
		}
	}

	// move pivot to in between partitions
	tmp = A[l]; A[l] = A[p - 1]; A[p - 1] = tmp;

	// print partition results
	// A[0..l) | A[l..p-1) | A[p-1] | A[p..r) | A[r..LEN)
	// white   | blue      | red    | yellow  | white
	WHITE();  print(A, l); printf("| ");
	BLUE();   print(A + l, p - 1 - l);
	WHITE();  printf("| ");
	RED();    printf("%d ", A[p - 1]);
	WHITE();  printf("| ");
	YELLOW(); print(A + p, r - p);
	WHITE();  printf("| "); print(A + r, LEN - r);
	puts("");

	// sort partitions
	quicksort(A, l, p - 1);
	quicksort(A, p, r);
}

int main(void)
{
	// create random array with values 0..MAX with length LEN
	int A[LEN];
	srand(time(NULL)); // seed RNG with time
	for (size_t i = 0; i < LEN; i++) {
		A[i] = rand() % MAX;
	}

	// print initial values
	printf("Initial: ");
	print(A, LEN);
	puts("\n");

	// print colour key
	printf("Colour key: ");
	BLUE();   printf("Left partition ");
	RED();    printf("Pivot ");
	YELLOW(); printf("Right partition\n\n");
	WHITE();

	// perform sort
	quicksort(A, 0, LEN);

	// print result
	printf("\nEnd result: ");
	print(A, LEN);
	puts("");

	return 0;
}
