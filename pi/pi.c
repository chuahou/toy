// SPDX-License-Identifier: MIT
// Copyright (c) 2022 Chua Hou
//
// Approximates pi using a pseudorandom stream by approximating the number of
// times a random point within a unit square from (0, 0) to (1, 1) lies within
// the quarter circle contained in that square centered at (0, 0).

#include <math.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define RAND(seedp) ((double) rand_r(seedp) / RAND_MAX)
#define N_ITERS 200000000
#define N_WORKERS 12

void *worker(void *result)
{
	uint64_t *in_circle = result;
	unsigned int seed = time(NULL) ^ pthread_self();
	*in_circle = 0;
	for (uint64_t i = 0; i < N_ITERS; i++) {
		double x = RAND(&seed);
		double y = RAND(&seed);
		if (x * x + y * y <= 1.) (*in_circle)++;
	};
	pthread_exit(NULL);
}

int main()
{
	uint64_t results[N_WORKERS];
	pthread_t threads[N_WORKERS];
	uint64_t result = 0;

	srand(time(NULL));
	for (uint64_t i = 0; i < N_WORKERS; i++)
		pthread_create(&threads[i], NULL, worker, &results[i]);
	for (uint64_t i = 0; i < N_WORKERS; i++) {
		pthread_join(threads[i], NULL);
		result += results[i];
	}

	printf("%lf\n", (4. * (double) result / N_ITERS) / N_WORKERS);
	return 0;
}
