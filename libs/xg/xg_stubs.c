/*
 * Copyright (C) 2006-2007 XenSource Ltd.
 * Copyright (C) 2008      Citrix Ltd.
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

#include <stdlib.h>
#include <stdint.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <syslog.h>

#include <xenctrl.h>
#include <xenstore.h> // please forgive me Lord
#include <xenguest.h>
#include <xen/hvm/hvm_info_table.h>
#include <xen/hvm/hvm_xs_strings.h>
#include <xen/hvm/params.h>
#include <xen/hvm/e820.h>
#include <sys/mman.h>
#include "xg_utils.h"
#include <xentoollog.h>

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/signals.h>
#include <caml/fail.h>

#include <inttypes.h>

#define _H(__h) ((xc_interface *)(__h))
#define _D(__d) ((uint32_t)Int_val(__d))

#define None_val (Val_int(0))

#ifndef HVM_PARAM_NX_ENABLED
#define XEN_UNSTABLE
#endif

#ifndef HVM_PARAM_VIRIDIAN
#warning missing viridian parameter
#endif

#ifndef HVM_PARAM_NESTEDHVM
#warning missing nestedhvm parameter
#endif

#include <stdio.h>

/* The following boolean flags are all set by their value
   in the platform area of xenstore. The only value that
   is considered true is the string 'true' */
struct flags {
  int vcpus;
  int vcpus_current;
  uint64_t vcpu_affinity; /* 0 means unset */
  uint16_t vcpu_weight;   /* 0 means unset (0 is an illegal weight) */
  uint16_t vcpu_cap;      /* 0 is default (no cap) */
  int nx;
  int viridian;
  int nested;
  int pae;
  int acpi;
  int apic;
  int acpi_s3;
  int acpi_s4;
  int smbios_pt;
  char *smbios_oem_types_pt;
  int acpi_pt;
};

extern int hvm_make_smbios_fw(struct xc_hvm_build_args *args);
extern int hvm_make_acpi_fw(struct xc_hvm_build_args *args);

int boolean(char *s)
{
	if (strcmp(s, "true") == 0)
		return 1;
	if (atoi(s) == 1)
		return 1;
	return 0;
}

static void
get_platform_flags(struct flags *f, int domid, value list)
{
	memset(f, '\0', sizeof(struct flags));
	while (list != Val_emptylist) {
		value t = Field(list, 0);
		value f1 = Field(t, 0);
		value f2 = Field(t, 1);
		list = Field(list, 1);

		if (strcmp("vcpu_number", String_val(f1)) == 0)
			f->vcpus = atoi(String_val(f2));
		else if (strcmp("vcpu_current", String_val(f1)) == 0)
			f->vcpus_current = atoi(String_val(f2));
		else if (strcmp("vcpu/affinity", String_val(f1)) == 0)
			f->vcpu_affinity = atoi(String_val(f2));
		else if (strcmp("vcpu/weight", String_val(f1)) == 0)
			f->vcpu_weight = atoi(String_val(f2));
		else if (strcmp("vcpu/cap", String_val(f1)) == 0)
			f->vcpu_cap = atoi(String_val(f2));
		else if (strcmp("nx", String_val(f1)) == 0)
			f->nx = boolean(String_val(f2));
		else if (strcmp("viridian", String_val(f1)) == 0)
			f->viridian = boolean(String_val(f2));
		else if (strcmp("nested", String_val(f1)) == 0)
			f->nested = boolean(String_val(f2));
		else if (strcmp("apic", String_val(f1)) == 0)
			f->apic = boolean(String_val(f2));
		else if (strcmp("acpi", String_val(f1)) == 0)
			f->acpi = boolean(String_val(f2));
		else if (strcmp("pae", String_val(f1)) == 0)
			f->pae = boolean(String_val(f2));
		else if (strcmp("acpi_s4", String_val(f1)) == 0)
			f->acpi_s4 = boolean(String_val(f2));
		else if (strcmp("acpi_s3", String_val(f1)) == 0)
			f->acpi_s3 = boolean(String_val(f2));
		else if (strcmp("smbios-pt", String_val(f1)) == 0)
			f->smbios_pt = boolean(String_val(f2));
		else if (strcmp("smbios-oem-types-pt", String_val(f1)) == 0)
			f->smbios_oem_types_pt = strdup(String_val(f2));
		else if (strcmp("acpi-pt", String_val(f1)) == 0)
			f->acpi_pt = boolean(String_val(f2));
	}
  openlog("xenguest",LOG_NDELAY,LOG_DAEMON);
  syslog(LOG_INFO|LOG_DAEMON,"Determined the following parameters from xenstore:");
  syslog(LOG_INFO|LOG_DAEMON,"vcpu/number:%d vcpu/affinity:%Ld vcpu/weight:%d vcpu/cap:%d nx: %d viridian: %d nested: %d apic: %d acpi: %d pae: %d acpi_s4: %d acpi_s3: %d smbios_pt: %d acpi_pt: %d",
	 f->vcpus,f->vcpu_affinity,f->vcpu_weight,f->vcpu_cap, f->nx,f->viridian,f->nested,f->apic,f->acpi,f->pae,f->acpi_s4,f->acpi_s3, f->smbios_pt, f->acpi_pt);
  closelog();

}


static void failwith_oss_xc(xc_interface *xch, char *fct)
{
	char buf[512];
	const xc_error *error;

	error = xc_get_last_error(xch);
	if (error->code == XC_ERROR_NONE)
		snprintf(buf, 512, "%s: [%d] %s", fct, errno, strerror(errno));
	else
		snprintf(buf, 512, "%s: [%d] %s", fct, error->code, error->message);
	xc_clear_last_error(xch);
	caml_failwith(buf);
}

static int dispatch_suspend(void *arg)
{
	value * __suspend_closure;
	int domid = (int) arg;
	int ret;

	__suspend_closure = caml_named_value("suspend_callback");
	if (!__suspend_closure)
		return 0;
	caml_leave_blocking_section();
	ret = Int_val(caml_callback(*__suspend_closure, Val_int(domid)));
	caml_enter_blocking_section();
	return ret;
}

static int suspend_flag_list[] = {
	XCFLAGS_DEBUG, XCFLAGS_LIVE, XCFLAGS_HVM
};

CAMLprim value stub_xenguest_init()
{
	xc_interface *xch;
	xentoollog_logger_stdiostream *lg = NULL;

	/* debug to stdio in desperate cases */
	if (0) {
		lg = xtl_createlogger_stdiostream(stdout, 0, XTL_STDIOSTREAM_SHOW_DATE);
	}
	xch = xc_interface_open((xentoollog_logger *) lg, (xentoollog_logger *) lg, 0);
	if (xch == NULL)
		failwith_oss_xc(NULL, "xc_interface_open");
	return (value)xch;
}

CAMLprim value stub_xenguest_close(value xenguest_handle)
{
	CAMLparam1(xenguest_handle);
	xc_interface_close(_H(xenguest_handle));
	CAMLreturn(Val_unit);
}

extern struct xc_dom_image *xc_dom_allocate(xc_interface *xch, const char *cmdline, const char *features);

static void configure_vcpus(xc_interface *xch, int domid, struct flags f)
{
  struct xen_domctl_sched_credit sdom;
  int r;
  r = xc_sched_credit_domain_get(xch, domid, &sdom);
  if (r)
    failwith_oss_xc(xch, "xc_sched_credit_domain_get");
  if (f.vcpu_weight != 0L) sdom.weight = f.vcpu_weight;
  if (f.vcpu_cap != 0L) sdom.cap = f.vcpu_cap;
  r = xc_sched_credit_domain_set(xch, domid, &sdom);
  if (r)
    failwith_oss_xc(xch, "xc_sched_credit_domain_set");
}

CAMLprim value stub_xc_linux_build_native(value xc_handle, value domid, value mem_start_mib,
                                          value image_name, value ramdisk_name,
                                          value cmdline, value features, value platformflags,
                                          value flags, value store_evtchn,
                                          value console_evtchn)
{
	CAMLparam5(xc_handle, domid, mem_start_mib, image_name, ramdisk_name);
	CAMLxparam5(cmdline, features, platformflags, flags, store_evtchn);
	CAMLxparam1(console_evtchn);
	CAMLlocal1(result);

	unsigned long store_mfn;
	unsigned long console_mfn;
	int r;
	struct xc_dom_image *dom;
	char c_protocol[64];

	/* Copy the ocaml values into c-land before dropping the mutex */
	xc_interface *xch = _H(xc_handle);
	unsigned int c_mem_start_mib = Int_val(mem_start_mib);
	uint32_t c_domid = _D(domid);
	char *c_image_name = strdup(String_val(image_name));
	char *c_ramdisk_name = ramdisk_name == None_val ? NULL : strdup(String_val(Field(ramdisk_name, 0)));
	unsigned long c_flags = Int_val(flags);
	unsigned int c_store_evtchn = Int_val(store_evtchn);
	unsigned int c_console_evtchn = Int_val(console_evtchn);

	struct flags f;
	get_platform_flags(&f,c_domid, platformflags);

	xc_dom_loginit(xch);
	dom = xc_dom_allocate(xch, String_val(cmdline), String_val(features));
	if (!dom)
		failwith_oss_xc(xch, "xc_dom_allocate");

	configure_vcpus(xch, c_domid, f);

	caml_enter_blocking_section();
	r = xc_dom_linux_build(xch, dom, c_domid, c_mem_start_mib,
	                       c_image_name, c_ramdisk_name, c_flags,
	                       c_store_evtchn, &store_mfn,
	                       c_console_evtchn, &console_mfn);
	caml_leave_blocking_section();

#ifndef XEN_UNSTABLE
	strncpy(c_protocol, xc_dom_get_native_protocol(dom), 64);
#else
	memset(c_protocol, '\0', 64);
#endif
	free(c_image_name);
	free(c_ramdisk_name);
	xc_dom_release(dom);

	if (r != 0)
		failwith_oss_xc(xch, "xc_dom_linux_build");

	result = caml_alloc_tuple(3);
	Store_field(result, 0, caml_copy_nativeint(store_mfn));
	Store_field(result, 1, caml_copy_nativeint(console_mfn));
	Store_field(result, 2, caml_copy_string(c_protocol));

	CAMLreturn(result);
}


CAMLprim value stub_xc_linux_build_bytecode(value * argv, int argn)
{
	return stub_xc_linux_build_native(argv[0], argv[1], argv[2], argv[3],
	                                  argv[4], argv[5], argv[6], argv[7],
	                                  argv[8], argv[9], argv[10]);
}

static int hvm_build_set_params(xc_interface *xch, int domid,
                                int store_evtchn, unsigned long *store_mfn,
                                int console_evtchn, unsigned long *console_mfn,
				struct flags f,
				struct xc_hvm_build_args *args)
{
	struct hvm_info_table *va_hvm;
	uint8_t *va_map, sum;
	int i;

	va_map = xc_map_foreign_range(xch, domid,
			    XC_PAGE_SIZE, PROT_READ | PROT_WRITE,
			    HVM_INFO_PFN);
	if (va_map == NULL)
		return -1;

	if (args->smbios_module.guest_addr_out) {
		struct xs_handle *xsh;
		xsh = xs_open(0);
		if (xsh) {
			char path[256] = { 0 };
			char value[64] = { 0 };

			snprintf(path, sizeof(path), "/local/domain/%d/"HVM_XS_SMBIOS_PT_ADDRESS, domid);
			snprintf(value,sizeof(value),"0x%"PRIx64, args->smbios_module.guest_addr_out);
			xs_write(xsh, XBT_NULL, path, value, strlen(value));

			snprintf(path, sizeof(path), "/local/domain/%d/"HVM_XS_SMBIOS_PT_LENGTH, domid);
			snprintf(value,sizeof(value),"0x%x", args->smbios_module.length);
			xs_write(xsh, XBT_NULL, path, value, strlen(value));
			xs_close(xsh);
		} else {
			XG_LOG("xenguest: failed to open xs handle");
			return -1;
		}
	}

	if (args->acpi_module.guest_addr_out) {
		struct xs_handle *xsh;
		xsh = xs_open(0);
		if (xsh) {
			char path[256] = { 0 };
			char value[64] = { 0 };

			snprintf(path, sizeof(path), "/local/domain/%d/"HVM_XS_ACPI_PT_ADDRESS, domid);
			snprintf(value,sizeof(value),"0x%"PRIx64, args->acpi_module.guest_addr_out);
			xs_write(xsh, XBT_NULL, path, value, strlen(value));

			snprintf(path, sizeof(path), "/local/domain/%d/"HVM_XS_ACPI_PT_LENGTH, domid);
			snprintf(value,sizeof(value),"0x%x", args->acpi_module.length);
			xs_write(xsh, XBT_NULL, path, value, strlen(value));
			xs_close(xsh);
		} else {
			XG_LOG("xenguest: failed to open xs handle");
			return -1;
		}
	}

	va_hvm = (struct hvm_info_table *)(va_map + HVM_INFO_OFFSET);
#if 0
	/* moved to xenstore in xen 4.3 */
	va_hvm->acpi_enabled = f.acpi;
#endif
	va_hvm->apic_mode = f.apic;
	va_hvm->nr_vcpus = f.vcpus;
	memset(va_hvm->vcpu_online, 0, sizeof(va_hvm->vcpu_online));
	for (i = 0; i < f.vcpus_current; i++)
		va_hvm->vcpu_online[i/8] |= 1 << (i % 8);
#if 0
        va_hvm->s4_enabled = f.acpi_s4;
        va_hvm->s3_enabled = f.acpi_s3;
#endif
	va_hvm->checksum = 0;
	for (i = 0, sum = 0; i < va_hvm->length; i++)
		sum += ((uint8_t *) va_hvm)[i];
	va_hvm->checksum = -sum;
	munmap(va_map, XC_PAGE_SIZE);

	xc_get_hvm_param(xch, domid, HVM_PARAM_STORE_PFN, store_mfn);
	xc_set_hvm_param(xch, domid, HVM_PARAM_PAE_ENABLED, f.pae);
#ifdef HVM_PARAM_VIRIDIAN
	xc_set_hvm_param(xch, domid, HVM_PARAM_VIRIDIAN, f.viridian);
#endif
#ifdef HVM_PARAM_NESTEDHVM
	xc_set_hvm_param(xch, domid, HVM_PARAM_NESTEDHVM, f.nested);
#endif
	xc_set_hvm_param(xch, domid, HVM_PARAM_STORE_EVTCHN, store_evtchn);
#ifndef XEN_UNSTABLE
	xc_set_hvm_param(xch, domid, HVM_PARAM_NX_ENABLED, f.nx);
  xc_get_hvm_param(xch, domid, HVM_PARAM_CONSOLE_PFN, console_mfn);
  xc_set_hvm_param(xch, domid, HVM_PARAM_CONSOLE_EVTCHN, console_evtchn);
#endif
	return 0;
}

/*
 * This function was hoisted out of libxl. It is a required step in building
 * an HVM. Note that the RDM support is not being used and is stubbed out.
 *
 * Original comment:
 *
 * Here we're just trying to set these kinds of e820 mappings:
 *
 * #1. Low memory region
 *
 * Low RAM starts at least from 1M to make sure all standard regions
 * of the PC memory map, like BIOS, VGA memory-mapped I/O and vgabios,
 * have enough space.
 * Note: Those stuffs below 1M are still constructed with multiple
 * e820 entries by hvmloader. At this point we don't change anything.
 *
 * #2. RDM region if it exists
 *
 * #3. High memory region if it exists
 *
 * Note: these regions are not overlapping since we already check
 * to adjust them. Please refer to libxl__domain_device_construct_rdm().
 */
#define GUEST_LOW_MEM_START_DEFAULT 0x100000
static int xc_arch_domain_construct_memmap(xc_interface *xch, uint32_t domid,
                                        struct xc_hvm_build_args *args)
{
	int rc = 0;
	unsigned int nr = 0, i;
	/* We always own at least one lowmem entry. */
	unsigned int e820_entries = 1;
	struct e820entry *e820 = NULL;
	uint64_t highmem_size =
		args->highmem_end ? args->highmem_end - (1ull << 32) : 0;

	/* Add all rdm entries.
	for (i = 0; i < d_config->num_rdms; i++)
		if (d_config->rdms[i].policy != LIBXL_RDM_RESERVE_POLICY_INVALID)
			e820_entries++;*/

	/* If we should have a highmem range. */
	if (highmem_size)
		e820_entries++;

	/* Not gonna happen, not using RDMs
	if (e820_entries >= E820MAX) {
		LOG(ERROR, "Ooops! Too many entries in the memory map!");
		rc = ERROR_INVAL;
		goto out;
	}*/

	e820 = malloc(sizeof(struct e820entry) * e820_entries);

	/* Low memory */
	e820[nr].addr = GUEST_LOW_MEM_START_DEFAULT;
	e820[nr].size = args->lowmem_end - GUEST_LOW_MEM_START_DEFAULT;
	e820[nr].type = E820_RAM;
	nr++;

	/* RDM mapping
	for (i = 0; i < d_config->num_rdms; i++) {
		if (d_config->rdms[i].policy == LIBXL_RDM_RESERVE_POLICY_INVALID)
			continue;

		e820[nr].addr = d_config->rdms[i].start;
		e820[nr].size = d_config->rdms[i].size;
		e820[nr].type = E820_RESERVED;
		nr++;
	}*/

	/* High memory */
	if (highmem_size) {
		e820[nr].addr = ((uint64_t)1 << 32);
		e820[nr].size = highmem_size;
		e820[nr].type = E820_RAM;
	}

	if (xc_domain_set_memory_map(xch, domid, e820, e820_entries) != 0) {
		rc = -1;
		/*goto out;*/
	}

	free(e820);

/*out:*/
	return rc;
}

CAMLprim value stub_xc_hvm_build_native(value xc_handle, value domid,
    value mem_max_mib, value mem_start_mib, value image_name, value platformflags, value store_evtchn, value console_evtchn)
{
	CAMLparam5(xc_handle, domid, mem_max_mib, mem_start_mib, image_name);
	CAMLxparam3(platformflags, store_evtchn, console_evtchn);
	CAMLlocal1(result);

	char *image_name_c = strdup(String_val(image_name));
	char *error[256];
	xc_interface *xch;

	unsigned long store_mfn=0;
	unsigned long console_mfn=0;
	int r;
	struct flags f;
	struct xc_hvm_build_args args;
	uint64_t mmio_start, lowmem_end, highmem_end;

	get_platform_flags(&f, _D(domid), platformflags);

	xch = _H(xc_handle);
	configure_vcpus(xch, _D(domid), f);

	memset(&args, 0, sizeof(struct xc_hvm_build_args));
	args.mem_size = (uint64_t) Int_val(mem_max_mib) << 20;
	args.mem_target = (uint64_t) Int_val(mem_start_mib) << 20;
	args.image_file_name = image_name_c;
	/* Need to setup new "out" params below. These are used to calculate
         * a number of other values in libxc. Also args.nr_vmemranges must
	 * be zero which will make libxc setup the vmem rananges values.
	 */
	args.mmio_size = HVM_BELOW_4G_MMIO_LENGTH;
	lowmem_end = args.mem_size;
	highmem_end = 0;
	mmio_start = (1ull << 32) - args.mmio_size;
	if (lowmem_end > mmio_start)
	{
		highmem_end = (1ull << 32) + (lowmem_end - mmio_start);
		lowmem_end = mmio_start;
	}
	args.lowmem_end = lowmem_end;
	args.highmem_end = highmem_end;
	args.mmio_start = mmio_start;

	caml_enter_blocking_section ();
	if (f.smbios_pt) {
		if (hvm_make_smbios_fw(&args)) {
			XG_LOG("xenguest helper: %s decoding failed - pass through functionality will be disabled.", "SMBIOS");
		}
	}
	if (f.acpi_pt) {
		/* call routine to decode and load ACPI tables */
		if (hvm_make_acpi_fw(&args)) {
			XG_LOG("xenguest helper: %s decoding failed - pass through functionality will be disabled.", "ACPI");
		}
	}

	r = xc_hvm_build(xch, _D(domid), &args);
	caml_leave_blocking_section ();

	free(image_name_c);

	if (r)
		failwith_oss_xc(xch, "hvm_build");

	caml_enter_blocking_section ();
	r = xc_arch_domain_construct_memmap(xch, _D(domid), &args);
	caml_leave_blocking_section ();

	if (r)
		failwith_oss_xc(xch, "xc_arch_domain_construct_memmap");

	caml_enter_blocking_section ();
	r = hvm_build_set_params(xch, _D(domid), Int_val(store_evtchn), &store_mfn,
				 Int_val(console_evtchn), &console_mfn, f, &args);
	caml_leave_blocking_section ();
	if (r)
		failwith_oss_xc(xch, "hvm_build_params");

	result = caml_alloc_tuple(2);
	Store_field(result, 0, caml_copy_nativeint(store_mfn));
	Store_field(result, 1, caml_copy_nativeint(console_mfn));

	CAMLreturn(result);
}

CAMLprim value stub_xc_hvm_build_bytecode(value * argv, int argn)
{
	return stub_xc_hvm_build_native(argv[0], argv[1], argv[2], argv[3],
                                  argv[4], argv[5], argv[6], argv[7]);
}

CAMLprim value stub_xc_domain_save(value handle, value fd, value domid,
                                   value max_iters, value max_factors,
                                   value flags, value hvm)
{
	CAMLparam5(handle, fd, domid, max_iters, max_factors);
	CAMLxparam2(flags, hvm);
	struct save_callbacks callbacks;

	uint32_t c_flags;
	uint32_t c_domid;
	int r;

	c_flags = caml_convert_flag_list(flags, suspend_flag_list);
	c_domid = _D(domid);

	memset(&callbacks, 0, sizeof(callbacks));
	callbacks.data = (void*)c_domid;
	callbacks.suspend = dispatch_suspend;

	caml_enter_blocking_section();
	r = xc_domain_save(_H(handle), Int_val(fd), c_domid,
	                   Int_val(max_iters), Int_val(max_factors),
	                   c_flags, &callbacks, Bool_val(hvm));
	caml_leave_blocking_section();
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_save");

	CAMLreturn(Val_unit);
}

CAMLprim value stub_xc_domain_save_bytecode(value *argv, int argn)
{
	return stub_xc_domain_save(argv[0], argv[1], argv[2], argv[3],
	                           argv[4], argv[5], argv[6]);
}

/* this is the slow version of resume for uncooperative domain,
 * the fast version is available in close source xc */
CAMLprim value stub_xc_domain_resume_slow(value handle, value domid)
{
	CAMLparam2(handle, domid);
	int r;

	/* hard code fast to 0, we only want to expose the slow version here */
	r = xc_domain_resume(_H(handle), _D(domid), 0);
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_resume");
	CAMLreturn(Val_unit);
}


CAMLprim value stub_xc_domain_restore(value handle, value fd, value domid, value platformflags,
                                      value store_evtchn, value console_evtchn,
                                      value hvm)
{
	CAMLparam5(handle, fd, domid, platformflags, store_evtchn);
	CAMLxparam2(console_evtchn, hvm);
	CAMLlocal1(result);
	unsigned long store_mfn, console_mfn;
	unsigned int c_store_evtchn, c_console_evtchn;
	int r;

	struct flags f;
	get_platform_flags(&f,_D(domid), platformflags);

	c_store_evtchn = Int_val(store_evtchn);
	c_console_evtchn = Int_val(console_evtchn);

#ifdef HVM_PARAM_VIRIDIAN
	xc_set_hvm_param(_H(handle), _D(domid), HVM_PARAM_VIRIDIAN, f.viridian);
#endif
#ifdef HVM_PARAM_NESTEDHVM
	xc_set_hvm_param(_H(handle), _D(domid), HVM_PARAM_NESTEDHVM, f.nested);
#endif
	configure_vcpus(_H(handle), _D(domid), f);

	caml_enter_blocking_section();
	r = xc_domain_restore(_H(handle), Int_val(fd), _D(domid),
	                      c_store_evtchn, &store_mfn, _D(domid),
	                      c_console_evtchn, &console_mfn, _D(domid),
			      Bool_val(hvm), f.pae, 0 /*superpages*/,
		              0, NULL);
	caml_leave_blocking_section();
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_restore");

	result = caml_alloc_tuple(2);
	Store_field(result, 0, caml_copy_nativeint(store_mfn));
	Store_field(result, 1, caml_copy_nativeint(console_mfn));

	CAMLreturn(result);
}

CAMLprim value stub_xc_domain_restore_bytecode(value * argv, int argn)
{
	return stub_xc_domain_restore(argv[0], argv[1], argv[2], argv[3],
	                              argv[4], argv[5], argv[6]);
}

CAMLprim value stub_xc_domain_dumpcore(value handle, value domid, value file)
{
	CAMLparam3(handle, domid, file);
	int r;

	r = xc_domain_dumpcore(_H(handle), _D(domid), String_val(file));
	if (r)
		failwith_oss_xc(_H(handle), "xc_domain_dumpcore");
	CAMLreturn(Val_unit);
}
