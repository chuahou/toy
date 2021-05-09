// SPDX-License-Identifier: MIT
// Copyright (c) 2021 Chua Hou

#include <pthread.h>
#include "channel.h"

// Creates a new channel in c.
void channel_init(struct channel *c)
{
	// Initializes mutexes.
	pthread_mutex_init(&c->mutex_full, NULL);
	pthread_mutex_init(&c->mutex_empty, NULL);
	pthread_mutex_init(&c->mutex_continue, NULL);

	// These should start off locked.
	pthread_mutex_lock(&c->mutex_full);
	pthread_mutex_lock(&c->mutex_continue);
}

// Closes channel by destroying mutexes.
void channel_close(struct channel *c)
{
	pthread_mutex_destroy(&c->mutex_full);
	pthread_mutex_destroy(&c->mutex_empty);
	pthread_mutex_destroy(&c->mutex_continue);
}

// Sends on a channel synchronously.
void channel_send(struct channel *c, int value)
{
	// Wait for channel to empty before sending next message.
	pthread_mutex_lock(&c->mutex_empty); c->slot = value;

	// Signal that receiver can receive message, then wait for receiver to
	// signal synchronization.
	pthread_mutex_unlock(&c->mutex_full);
	pthread_mutex_lock(&c->mutex_continue);

	// Signal that next message can begin.
	pthread_mutex_unlock(&c->mutex_empty);
}

// Receives on a channel synchronously.
int channel_recv(struct channel *c)
{
	// Wait for sender to fill channel before getting value.
	pthread_mutex_lock(&c->mutex_full); int value = c->slot;

	// Synchronize with sender.
	pthread_mutex_unlock(&c->mutex_continue);

	return value;
}
