// SPDX-License-Identifier: MIT
// Copyright (c) 2021 Chua Hou
//
// Simple example based on an exercise sheet. Sorts an array of n elements using
// n workers.

#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "channel.h"

// Information to pass to worker thread.
struct worker_info { int N; struct channel *left, *right; };

// Worker thread. Receives values on left channel, keeping the largest seen so
// far and passing the rest to the right channel. Does this N times.
void *worker(void *_info)
{
	struct worker_info *info = (struct worker_info *) _info;
	int largest = channel_recv(info->left);
	for (int i = 1; i < info->N; i++) {
		int next = channel_recv(info->left);
		if (next > largest) {
			channel_send(info->right, largest);
			largest = next;
		} else channel_send(info->right, next);
	}
	channel_send(info->right, largest);
	return NULL;
}

// Information to pass to instream thread.
struct instream_info { int N; int *arr; struct channel *out; };

// Sends N values from arr on channel out.
void *instream(void *_info)
{
	struct instream_info *info = (struct instream_info *) _info;
	for (int i = 0; i < info->N; i++) channel_send(info->out, info->arr[i]);
	return NULL;
}

// Prints sorted version of array.
void psort(size_t N, int *arr)
{
	// channels[0] from instream to first worker
	// channels[i] from i-th worker to (i+1)-th worker
	// channels[N] from last worker to us
	struct channel *channels = malloc(sizeof(struct channel) * (N + 1));
	for (int i = 0; i < N + 1; i++) channel_init(&channels[i]);

	// Create instream thread.
	pthread_t instream_thread;
	struct instream_info instream_info = {
		.N = N, .arr = arr, .out = &channels[0]
	};
	pthread_create(&instream_thread, NULL, instream, &instream_info);

	// Create worker threads.
	struct worker_info *worker_infos = malloc(sizeof(struct worker_info) * N);
	pthread_t *worker_threads = malloc(sizeof(pthread_t) * N);
	for (int i = 0; i < N; i++) {
		worker_infos[i].N = N;
		worker_infos[i].left = &channels[i];
		worker_infos[i].right = &channels[i+1];
		pthread_create(&worker_threads[i], NULL, worker, &worker_infos[i]);
	}

	// Receive and print values.
	for (int i = 0; i < N; i++)
		printf("%d ", channel_recv(&channels[N]));
	puts("");

	pthread_join(instream_thread, NULL);
	for (int i = 0; i < N; i++) pthread_join(worker_threads[i], NULL);
	for (int i = 0; i < N + 1; i++) channel_close(&channels[i]);
	free(worker_infos); free(worker_threads); free(channels);
}

int main(void)
{
	// Problem input.
	size_t N; // Size of array.
	int *arr; // Array to sort.

	// Get input from stdin.
	scanf("%lu", &N);
	arr = (int *) malloc(sizeof(int) * N);
	for (size_t i = 0; i < N; i++) scanf("%d", &arr[i]);

	// Sort and print output.
	psort(N, arr);

	// Clean up.
	free(arr);
}
