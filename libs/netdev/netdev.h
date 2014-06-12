/*
 * Copyright (c) 2006 XenSource Inc.
 * Author: Vincent Hanquez <vincent@xensource.com>
 */

/*
 * Copyright (c) 2008 Citrix Systems, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */


#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/if.h>
#include <linux/sockios.h>

#ifndef SIOCBRADDBR
#include "sockios_compat.h"
#endif

#define CHECK_IOCTL(err, S)	\
	if (err < 0) {		\
		caml_failwith(S ": ioctl failed");	\
	}
