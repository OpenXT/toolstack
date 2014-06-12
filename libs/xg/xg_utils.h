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

#ifndef __XEN_GUEST_UTIL_H__
#define __XEN_GUEST_UTIL_H__

#if __WORDSIZE == 64
#define INT_FMT "%ld"
#define UINT_FMT "%lx"
#else
#define INT_FMT "%d"
#define UINT_FMT "%x"
#endif

/* simple log helper so people don't log to stdout and break the comm channel */
#define XG_LOG(_f, _a...)   fprintf(stderr, _f, ##_a)

uint8_t *helper_mmap(size_t phys_addr, size_t length);
void helper_unmmap(uint8_t *addr, size_t length);
int helper_efi_locate(const char *efi_entry, uint32_t length, size_t *location);

#endif /* __XEN_GUEST_UTIL_H__ */

