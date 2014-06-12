/*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2009      Citrix Ltd.
 * Author Ross Philipson <ross.philipson@citrix.com>
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
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "xg_utils.h"

#define EFI_LINE_SIZE 64

uint8_t *helper_mmap(size_t phys_addr, size_t length)
{
    uint32_t page_offset = phys_addr % sysconf(_SC_PAGESIZE);
    uint8_t *addr;
    int fd;

    fd = open("/dev/mem", O_RDONLY);
    if (fd == -1)
        return NULL;

    addr = (uint8_t*)mmap(0, page_offset + length, PROT_READ, MAP_PRIVATE, fd, phys_addr - page_offset);
    close(fd);

    if (addr == MAP_FAILED)
        return NULL;      

    return addr + page_offset;
}

void helper_unmmap(uint8_t *addr, size_t length)
{
    uint32_t page_offset = (size_t)addr % sysconf(_SC_PAGESIZE);

    munmap(addr - page_offset, length + page_offset);
}

int helper_efi_locate(const char *efi_entry, uint32_t length, size_t *location)
{
    FILE *systab = NULL;
    char efiline[EFI_LINE_SIZE];
    char *val;
    off_t loc = 0;
    
    *location = 0;

    /* use EFI tables if present */
    systab = fopen("/sys/firmware/efi/systab", "r");
    if (systab != NULL) {
        while((fgets(efiline, EFI_LINE_SIZE - 1, systab)) != NULL) {               
            if (strncmp(efiline, efi_entry, 6) == 0) {
                /* found EFI entry, get the associated value */
                val = memchr(efiline, '=', strlen(efiline)) + 1;
                loc = strtol(val, NULL, 0);
                break;
            }
        }
        fclose(systab);

        if (loc != 0) {
        	*location = loc;
            return 0;
        }
    }
    
    return -1;
}

