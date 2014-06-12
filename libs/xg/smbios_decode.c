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

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <syslog.h>
#include <errno.h>
#include <xenctrl.h>
#include <xenguest.h>
#include <xenhvm.h>
#include "xg_utils.h"
#include <xentoollog.h>

#define SMBIOS_TYPE_BIOS_INFO 0
#define SMBIOS_TYPE_SYSTEM_INFO 1
#define SMBIOS_TYPE_BASE_BOARD 2
#define SMBIOS_TYPE_ENCLOSURE 3
#define SMBIOS_TYPE_OEM_STRINGS 11
#define SMBIOS_TYPE_BATTERY 22
#define SMBIOS_TYPE_POWER_SUPPLY 39
#define SMBIOS_TYPE_INACTIVE 126
#define SMBIOS_TYPE_EOT 127
#define SMBIOS_TYPE_VENDOR_MIN 128
#define SMBIOS_TYPE_VENDOR_MAX 255

/*
 * Read a set of SMBIOS structures and build the in memory data to pass
 * to hvmloader when running VMs with passthrough enabled.
 */
int
hvm_make_smbios_fw(struct xc_hvm_build_args *args)
{
        struct xh_smbios_spec tl[] = {
            {SMBIOS_TYPE_BIOS_INFO,     XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_SYSTEM_INFO,   XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_BASE_BOARD,    XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_ENCLOSURE,     XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_OEM_STRINGS,   XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_BATTERY,       XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_POWER_SUPPLY,  XH_SMBIOS_FLAG_FIRST},
            {SMBIOS_TYPE_VENDOR_MIN,    XH_SMBIOS_FLAG_ALL | XH_SMBIOS_FLAG_GE},
        };
        int rc;
        uint32_t fw_length;
        uint8_t *fw_bytes;
        struct xh_firmware_block *fw;

        XG_LOG("Create SMBIOS tables from FW.");
        rc = xh_find_smbios_structures(XH_DECODE_BOTH,
            tl, sizeof (tl)/sizeof (tl[0]),
            &fw_bytes, &fw_length);

        if (rc) {
                XG_LOG("Failed to read fw SMBIOS, errno: %d", errno);
                return (-1);
        }

        /*
         * The libxenhvm lib formats the fw blocks with 32b length integers
         * between each SMBIOS struct which is exactly what we want to pass
         * to the hvmloader code.
         */
        fw = (struct xh_firmware_block *)fw_bytes;

        if (fw->length <= sizeof (struct xh_firmware_block)) {
                XG_LOG("Invalid FW table returned (len=%d)",
                    fw->length);
                return (-1);
        }

        args->smbios_module.length =
            fw->length - sizeof (struct xh_firmware_block);
        args->smbios_module.data = (void*)malloc(args->smbios_module.length);
        memcpy(args->smbios_module.data, fw+1, args->smbios_module.length);

        free(fw_bytes);

        return (0);
}
