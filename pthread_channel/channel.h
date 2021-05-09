// SPDX-License-Identifier: MIT
// Copyright (c) 2021 Chua Hou

#ifndef __CHANNEL_H_INCLUDED__
#define __CHANNEL_H_INCLUDED__

#include <pthread.h>

// Simple synchronous channel implementation with no error handling.
struct channel {
	pthread_mutex_t mutex_full;     // Signals to receiver that value deposited.
	pthread_mutex_t mutex_empty;    // Signals to next sender to deposit.
	pthread_mutex_t mutex_continue; // Signals to previous sender to continue.
	int slot; // Slot to deposit value.
};

// Creates a new channel in c.
void channel_init(struct channel *c);

// Closes channel by destroying mutexes.
void channel_close(struct channel *c);

// Sends on a channel synchronously.
void channel_send(struct channel *c, int value);

// Receives on a channel synchronously.
int channel_recv(struct channel *c);

#endif
