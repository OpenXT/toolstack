/*
 * Copyright (C) 2008-2009 Citrix Ltd.
 * Author Vincent Hanquez <vincent.hanquez@eu.citrix.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 */

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <errno.h>

int main(int argc, char **argv)
{
	int i, split_index, maxfd, n;
	char *to_leave_open;
	int with_path = 0;

	maxfd = sysconf(_SC_OPEN_MAX);
	to_leave_open = calloc(maxfd, sizeof(char));
	if (!to_leave_open)
		return 99;

	split_index = -1;
	for (i = 0; argv[i]; i++)
		if (strcmp(argv[i], "--") == 0) {
			split_index = i;
			break;
		}

	if (split_index == -1 || (split_index + 1) >= argc) {
		printf("Usage:\n");
		printf("  %s fd0 .. fdN -- argv0 .. argvN\n", argv[0]);
		exit(1);
	}

	for (i = 0; i < split_index; i++) {
		if (strcmp(argv[1 + i], "--exec-with-path") == 0) {
			with_path = 1;
		} else {
			int fd = atoi(argv[1 + i]);
			to_leave_open[fd] = 1;
		}
	}

	for (i = 0; i < maxfd; i++)
		if (!to_leave_open[i])
			close(i);

	argv += split_index + 1;
	for (n = 5; n > 0; n--) {
		if (with_path)
			execvp(argv[0], argv);
		else
			execv(argv[0], argv);
		perror("Unix error: ");
		if (errno == ETXTBSY)
			sleep(1);
		else
			exit(127);
	}
	return 0;
}
