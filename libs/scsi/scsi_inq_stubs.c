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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <dlfcn.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>

#define VPD_SUPPORTED_VPDS  0x0
#define VPD_UNIT_SERIAL_NUM 0x80
#define VPD_DEVICE_ID       0x83

#define SI_STANDARD_INQ_LEN 36
#define SI_STANDARD_HDR_LEN 5
#define SI_DEFAULT_LEN      252
#define SI_MAX_LEN          1024

#define SI_VPD_SUPPORTED    1
#define SI_VPD_UNSUPPORTED  0
#define SI_VPD_CHECK_ERROR -1

#define SI_OK            0
#define SI_ERR_FAILURE  -1
#define SI_ERR_OUTOFMEM -2

typedef int (*sg_cmds_open_device_t)(const char * device_name, int read_only,
									 int verbose);

typedef int (*sg_cmds_close_device_t)(int device_fd);

typedef int (*sg_ll_inquiry_t)(int sg_fd, int cmddt, int evpd, int pg_op,
							   void * resp, int mx_resp_len, int noisy,
							   int verbose);

typedef int (*sg_vpd_dev_id_iter_t)(const unsigned char * initial_desig_desc,
									int page_len, int * off, int m_assoc,
									int m_desig_type, int m_code_set);

struct sg_inst {
	void *handle;
	int fd;
	sg_cmds_open_device_t sg_cmds_open_device_fn;
	sg_cmds_close_device_t sg_cmds_close_device_fn;
	sg_ll_inquiry_t sg_ll_inquiry_fn;
	sg_vpd_dev_id_iter_t sg_vpd_dev_id_iter_fn;
};

struct scsi_inq_data {
	uint8_t *data;
	uint32_t length;
};

static const char *sg_lib_list[] = {
	"libsgutils2.so",
	"/usr/lib/libsgutils2.so",
	"/lib/libsgutils2.so",
	"libsgutils2.so.2.0.0",
	"/usr/lib/libsgutils2.so.2.0.0",
	"/lib/libsgutils2.so.2.0.0",
	NULL
};

static int scsi_inq_open_device(struct sg_inst *inst, const char *device)
{
	void *handle = NULL;
	int i = 0, fd = 0;

	while (sg_lib_list[i] != NULL) {
		handle = dlopen(sg_lib_list[i++], RTLD_LOCAL | RTLD_LAZY);
		if (handle)
			break;
	}

	if (!handle)
		return SI_ERR_FAILURE;

	do {
		dlerror(); /* clear */
		/* locate the function we need to make the scsi inquries */
		inst->sg_cmds_open_device_fn = 
			(sg_cmds_open_device_t)dlsym(handle, "sg_cmds_open_device");
		if (dlerror())
			break;
		inst->sg_cmds_close_device_fn = 
			(sg_cmds_close_device_t)dlsym(handle, "sg_cmds_close_device");
		if (dlerror())
			break;
		inst->sg_ll_inquiry_fn = 
			(sg_ll_inquiry_t)dlsym(handle, "sg_ll_inquiry");
		if (dlerror())
			break;
		inst->sg_vpd_dev_id_iter_fn =
			(sg_vpd_dev_id_iter_t)dlsym(handle, "sg_vpd_dev_id_iter");
		if (dlerror())
			break;

		/* open the scsi/ata device */
		fd = inst->sg_cmds_open_device_fn(device, 1, 0);
		if (!fd)          
			break;

		/* else lib is open */
		inst->handle = handle;
		inst->fd = fd;
		return SI_OK;
	} while (0);

	/* cleanup on failure */
	if (fd && inst->sg_cmds_close_device_fn)
		inst->sg_cmds_close_device_fn(fd);
	dlclose(handle);

	return SI_ERR_FAILURE;
}

static void scsi_inq_close_device(struct sg_inst *inst)
{
	if (inst && inst->handle) {
		if (inst->fd && inst->sg_cmds_close_device_fn)
			inst->sg_cmds_close_device_fn(inst->fd);
		dlclose(inst->handle);
	}
}

static int scsi_check_vpd(struct sg_inst *inst, int vpdnum)
{
	uint8_t rbuf[SI_DEFAULT_LEN];
	uint8_t i, length;
	int res;

	memset(rbuf, 0xff, SI_DEFAULT_LEN);
	res = inst->sg_ll_inquiry_fn(inst->fd, 0, 1, VPD_SUPPORTED_VPDS, rbuf, SI_DEFAULT_LEN, 0, 0);
	if (res)
		return SI_VPD_CHECK_ERROR;
	if (rbuf[1] != VPD_SUPPORTED_VPDS || rbuf[2] != 0)
		return SI_VPD_CHECK_ERROR;
	length = rbuf[3] + 4;
	for (i = 4; i < length; i++) {
		if (rbuf[i] == (uint8_t)vpdnum)
			return SI_VPD_SUPPORTED;
	}
	
	return SI_VPD_UNSUPPORTED;
}

static int scsi_do_vpd_inq(struct sg_inst *inst, struct scsi_inq_data *si_data, int vpdnum)
{
	uint8_t *rbuf = NULL;
	uint32_t length;
	int res, rc = SI_ERR_FAILURE;

	/* Allocate a default buffer to receive a minimal request */
	rbuf = malloc(SI_DEFAULT_LEN);
	if (!rbuf)
		return SI_ERR_OUTOFMEM;
	memset(rbuf, 0, SI_DEFAULT_LEN);

	do {
		/* Make the request, redo the request if more data is available */
		res = inst->sg_ll_inquiry_fn(inst->fd, 0, 1, vpdnum, rbuf, SI_DEFAULT_LEN, 0, 0);
		if (res)
			break;

		if (vpdnum == VPD_UNIT_SERIAL_NUM) {
			if (rbuf[1] != VPD_UNIT_SERIAL_NUM)
				break;
			length = rbuf[3] + 4;
		}
		else if (vpdnum == VPD_DEVICE_ID) {
			if (rbuf[1] != VPD_DEVICE_ID)
				break;
			length = ((rbuf[2] << 8) + rbuf[3]) + 4;
			if (length > SI_MAX_LEN)
				break;
			if (length > SI_DEFAULT_LEN) {
				free(rbuf);
				rbuf = malloc(length);
				if (!rbuf) {
					rc = SI_ERR_OUTOFMEM;
					break;
				}
				memset(rbuf, 0, length);
				res = inst->sg_ll_inquiry_fn(inst->fd, 0, 1, vpdnum, rbuf, length, 0, 0);
				if (res)
					break;
			}
		}
		else
			break;

		si_data->data = rbuf;
		si_data->length = length;
		return SI_OK;
	} while (0);

	if (rbuf)
		free(rbuf);

	return rc;
}

static int scsi_do_ata_mods(struct sg_inst *inst, struct scsi_inq_data *si_std,
							struct scsi_inq_data *si_page83)
{
	int ret, offset, vilen, dlen;
	uint8_t *desc, *sbuf, *dbuf, *nbuf;
	uint8_t *viloc = NULL;
	
	/* Sanity check, is this an ATA simulated response? */
	sbuf = si_std->data;
	if ((si_std->length < 36)||(strncmp("ATA     ", (const char *)&sbuf[8], 8) != 0))
		return SI_OK; /* not ATA, do not modify */

	dlen = (int)si_page83->length - 4;
	dbuf = si_page83->data + 4;
	offset = -1;

	/* Locate the vendor identification information descriptor - type T10 */
	ret = inst->sg_vpd_dev_id_iter_fn(dbuf, dlen, &offset, -1, -1, -1);
	while (ret == 0) {
		desc = dbuf + offset;
		if (desc + 3 >= dbuf + dlen)
			break; /* hugh, nothing much in this descriptor, malformed */

		if ((desc[1] & 0xf) == 1) {
			/* We have located the vendor information so save it. */
			vilen = desc[3];
			viloc = desc + 4;
			break;
		}

		ret = inst->sg_vpd_dev_id_iter_fn(dbuf, dlen, &offset, -1, -1, -1);
	}
	
	/* If we did not find the vendor info location then just return the 
	   original inquiry intact */
	if (viloc == NULL)
		return SI_OK;

	/* Ideally we want to get the vendor, product, and revision level from
	   page 83h. If this is all here it takes up 68 bytes but we don't need
       the last 20 bytes where the serial number is */
	if (vilen < 48)
		return SI_OK;

	/* Build up new std request */
	nbuf = (uint8_t*)malloc(si_std->length);
	if (!nbuf) 
		return SI_ERR_OUTOFMEM;

	memcpy(nbuf, sbuf, 8); /* original header */
	memcpy(nbuf + 8, viloc + 8, 8); /* vendor info is here in ATA simulated response */
	memcpy(nbuf + 16, viloc + 16, 16); /* product info is here in ATA simulated response */
	memcpy(nbuf + 32, sbuf + 32, 4); /* original revision */
	if (si_std->length > 36)
		memcpy(nbuf + 36, sbuf + 36, (si_std->length - 36)); /* anything else */

	free(sbuf);
	si_std->data = nbuf;
	
	return SI_OK;
}

static int scsi_do_standard_inq(struct sg_inst *inst, struct scsi_inq_data *si_data)
{
	uint8_t *rbuf = NULL;
	uint32_t length;
	int res, rc = SI_ERR_FAILURE;

	/* Allocate a minimal 36 buffer to receive a minimal request */
	rbuf = malloc(SI_STANDARD_INQ_LEN);
	if (!rbuf)
		return SI_ERR_OUTOFMEM;
	memset(rbuf, 0, SI_STANDARD_INQ_LEN);

	do {
		/* Make the request, redo the request if more data is available */
		res = inst->sg_ll_inquiry_fn(inst->fd, 0, 0, 0, rbuf, SI_STANDARD_INQ_LEN, 0, 0);
		if (res)
			break;

		length = rbuf[4] + SI_STANDARD_HDR_LEN;
		if (length > SI_MAX_LEN)
			break;
		if (length <= SI_STANDARD_INQ_LEN) {
			si_data->data = rbuf;
			si_data->length = length;
			return SI_OK;
		}
		
		rbuf = realloc(rbuf, length);
		if (!rbuf) {
			rc = SI_ERR_OUTOFMEM;
			break;
		}
		memset(rbuf, 0, length);

		res = inst->sg_ll_inquiry_fn(inst->fd, 0, 0, 0, rbuf, length, 0, 0);
		if (res)
			break;

		si_data->data = rbuf;
		si_data->length = length;
		return SI_OK;
	} while (0);

	if (rbuf)
		free(rbuf);

	return rc;
}

CAMLprim value stub_scsi_inq_standard(value device, value atamods)
{
	CAMLparam2(device, atamods);
	CAMLlocal1(data);
	int rc = SI_OK, supported;
	const char *reason = "";
	struct sg_inst inst = {0};
	struct scsi_inq_data si_std = {0};
	struct scsi_inq_data si_page83 = {0};

	do {
		/* Locate and open the sg_utils shared lib */
		rc = scsi_inq_open_device(&inst, String_val(device));
		if (rc != SI_OK) {
			reason = "stub_scsi_inq_standard - cannot open sg-lib or device";
			break;
		}

		/* Get the standard inquiry data */
		rc = scsi_do_standard_inq(&inst, &si_std);
		if (rc != SI_OK) {
			reason = "stub_scsi_inq_standard - scsi_do_standard_inq failed";
			break;
		}

		/* If ATA mods were requested, modify the standard inquiry data else just return
		   the original response */
		if (Bool_val(atamods)) {
			/* Check if the device ID VPD is supported */
			supported = scsi_check_vpd(&inst, VPD_DEVICE_ID);
			if (supported == SI_VPD_UNSUPPORTED) {
				reason = "stub_scsi_inq_standard - Device ID VPD not supported";
				rc = -1;
				break;
			}
			else if (supported == SI_VPD_CHECK_ERROR) {
				rc = -1;
				reason = "stub_scsi_inq_standard - VPD check error";
				break;
			}

			/* Get the device ID VPD data and make the ATA modifications */
			rc = scsi_do_vpd_inq(&inst, &si_page83, VPD_DEVICE_ID);
			if (rc != SI_OK) {
				reason = "stub_scsi_inq_standard - scsi_do_vpd_inq for Device ID failed";
				break;
			}

			rc = scsi_do_ata_mods(&inst, &si_std, &si_page83);
			if (rc != SI_OK) {
				reason = "stub_scsi_inq_standard - ATA modifications failed";
				break;
			}
		}
		
		data = caml_alloc_string(si_std.length);
		memcpy((char *)data, si_std.data, si_std.length);
	} while (0);
	
	/* Cleanup */
	if (si_page83.data)
		free(si_page83.data);
	if (si_std.data)
		free(si_std.data);
	scsi_inq_close_device(&inst);

	if (rc == SI_ERR_FAILURE)
		caml_failwith(reason);
	else if (rc == SI_ERR_OUTOFMEM)
		caml_raise_out_of_memory();

	CAMLreturn(data);
}

CAMLprim value stub_scsi_inq_vpd(value device, value vpdnum)
{
	CAMLparam2(device, vpdnum);
	CAMLlocal1(data);
	int rc = 0, supported;
	const char *reason = "";
	struct sg_inst inst = {0};
	struct scsi_inq_data si_vpd = {0};

	do {
		/* Locate and open the sg_utils shared lib */
		rc = scsi_inq_open_device(&inst, String_val(device));
		if (rc != SI_OK) {
			reason = "stub_scsi_inq_vpd - cannot open sg-lib or device";
			break;
		}
		
		/* Check if the requested VPD is supported */
		if (Int_val(vpdnum) != VPD_UNIT_SERIAL_NUM && Int_val(vpdnum) != VPD_DEVICE_ID) {
			reason = "stub_scsi_inq_vpd - only pages 0x80 and 0x83 requests are supported";
			rc = SI_ERR_FAILURE;
			break;
		}

		supported = scsi_check_vpd(&inst, Int_val(vpdnum));
		if (supported == SI_VPD_UNSUPPORTED) {
			reason = "stub_scsi_inq_vpd - requested VPD not supported";
			rc = SI_ERR_FAILURE;
			break;
		}
		else if (supported == SI_VPD_CHECK_ERROR) {
			rc = SI_ERR_FAILURE;
			reason = "stub_scsi_inq_vpd - VPD check error";
			break;
		}

		/* Get the requested VPD data */
		rc = scsi_do_vpd_inq(&inst, &si_vpd, Int_val(vpdnum));
		if (rc != SI_OK) {
			reason = "stub_scsi_inq_vpd - scsi_do_vpd_inq failed";
			break;
		}

		data = caml_alloc_string(si_vpd.length);
		memcpy((char *)data, si_vpd.data, si_vpd.length);
	} while (0);
	
	/* Cleanup */
	if (si_vpd.data)
		free(si_vpd.data);
	scsi_inq_close_device(&inst);

	if (rc == SI_ERR_FAILURE)
		caml_failwith(reason);
	else if (rc == SI_ERR_OUTOFMEM)
		caml_raise_out_of_memory();

	CAMLreturn(data);
}
