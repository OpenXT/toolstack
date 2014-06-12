/*
 * Copyright (c) 2011 Citrix Systems, Inc.
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

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <sys/file.h>
#include <time.h>
#include <errno.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <string.h>
#include <unistd.h> /* needed for _SC_OPEN_MAX */
#include <stdio.h> /* snprintf */
#include <pthread.h> /* needed for caml_condition_timedwait */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/signals.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

static void failwith_errno(void)
{
	char buf[256];
	char buf2[280];
	memset(buf, '\0', sizeof(buf));
	//strerror_r(errno, buf, sizeof(buf));
	snprintf(buf2, sizeof(buf2), "errno: %d msg: %s", errno, buf);
	caml_failwith(buf2);
}

/* Set the TCP_NODELAY flag on a Unix.file_descr */
CAMLprim value stub_unixext_set_tcp_nodelay (value fd, value bool)
{
	CAMLparam2 (fd, bool);
	int c_fd = Int_val(fd);
	int opt = (Bool_val(bool)) ? 1 : 0;
	if (setsockopt(c_fd, IPPROTO_TCP, TCP_NODELAY, (void *)&opt, sizeof(opt)) != 0){
		failwith_errno();
	}
	CAMLreturn(Val_unit);
}

CAMLprim value stub_unixext_fsync (value fd)
{
	CAMLparam1(fd);
	int c_fd = Int_val(fd);
	if (fsync(c_fd) != 0) failwith_errno();
	CAMLreturn(Val_unit);
}

CAMLprim value stub_unixext_get_max_fd (value unit)
{
	CAMLparam1 (unit);
	long maxfd;
	maxfd = sysconf(_SC_OPEN_MAX);
	CAMLreturn(Val_int(maxfd));
}

#include <sys/vfs.h>

CAMLprim value stub_unixext_statfs(value path)
{
	CAMLparam1(path);
	CAMLlocal1(statinfo);
	struct statfs info;

	if (statfs(String_val(path), &info))
		failwith_errno();

	statinfo = caml_alloc_tuple(8);
	Store_field(statinfo, 0, caml_copy_int64(info.f_type));
	Store_field(statinfo, 1, Val_int(info.f_bsize));
	Store_field(statinfo, 2, caml_copy_int64(info.f_blocks));
	Store_field(statinfo, 3, caml_copy_int64(info.f_bfree));
	Store_field(statinfo, 4, caml_copy_int64(info.f_bavail));
	Store_field(statinfo, 5, caml_copy_int64(info.f_files));
	Store_field(statinfo, 6, caml_copy_int64(info.f_ffree));
	Store_field(statinfo, 7, Val_int(info.f_namelen));

	CAMLreturn(statinfo);
}

#define FDSET_OF_VALUE(v) (&(((struct fdset_t *) v)->fds))
#define MAXFD_OF_VALUE(v) (((struct fdset_t *) v)->max)
struct fdset_t { fd_set fds; int max; };

CAMLprim value stub_fdset_of_list(value l)
{
	CAMLparam1(l);
	CAMLlocal1(set);

	set = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	FD_ZERO(FDSET_OF_VALUE(set));
	MAXFD_OF_VALUE(set) = -1;
	while (l != Val_int(0)) {
		int fd;
		fd = Int_val(Field(l, 0));
		FD_SET(fd, FDSET_OF_VALUE(set));
		if (fd > MAXFD_OF_VALUE(set))
			MAXFD_OF_VALUE(set) = fd;
		l = Field(l, 1);
	}
	CAMLreturn(set);
}

CAMLprim value stub_fdset_is_set(value set, value fd)
{
	CAMLparam2(set, fd);
	CAMLreturn(Val_bool(FD_ISSET(Int_val(fd), FDSET_OF_VALUE(set))));
}

CAMLprim value stub_fdset_set(value set, value fd)
{
	CAMLparam2(set, fd);
	int cfd;

	cfd = Int_val(fd);
	FD_SET(cfd, FDSET_OF_VALUE(set));
	if (cfd > MAXFD_OF_VALUE(set))
		MAXFD_OF_VALUE(set) = cfd;
	CAMLreturn(Val_unit);
}

CAMLprim value stub_fdset_clear(value set, value fd)
{
	CAMLparam2(set, fd);
	int cfd, d;

	cfd = Int_val(fd);
	FD_CLR(cfd, FDSET_OF_VALUE(set));
	if (cfd == MAXFD_OF_VALUE(set)) {
		for (d = cfd - 1; d >= 0; d--) {
			if (FD_ISSET(d, FDSET_OF_VALUE(set))) {
				MAXFD_OF_VALUE(set) = d;
				break;
			}
		}
		if (d < 0)
			MAXFD_OF_VALUE(set) = -1;
	}
	CAMLreturn(Val_unit);
}

void unixext_error(int code)
{
	static value *exn = NULL;

	if (!exn) {
		exn = caml_named_value("unixext.unix_error");
		if (!exn)
			caml_invalid_argument("unixext.unix_error not initialiazed");
	}
	caml_raise_with_arg(*exn, Val_int(code));
}

CAMLprim value stub_fdset_select(value rset, value wset, value eset, value t)
{
	CAMLparam4(rset, wset, eset, t);
	CAMLlocal4(ret, nrset, nwset, neset);
	fd_set r, w, e;
	int maxfd;
	double tm;
	struct timeval tv;
	struct timeval *tvp;
	int v;

	memcpy(&r, FDSET_OF_VALUE(rset), sizeof(fd_set));
	memcpy(&w, FDSET_OF_VALUE(wset), sizeof(fd_set));
	memcpy(&e, FDSET_OF_VALUE(eset), sizeof(fd_set));

	maxfd = (MAXFD_OF_VALUE(rset) > MAXFD_OF_VALUE(wset))
		? MAXFD_OF_VALUE(rset)
		: MAXFD_OF_VALUE(wset);
	maxfd = (maxfd > MAXFD_OF_VALUE(eset)) ? maxfd : MAXFD_OF_VALUE(eset);

	tm = Double_val(t);
	if (tm < 0.0)
		tvp = NULL;
	else {
		tv.tv_sec = (int) tm;
		tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
		tvp = &tv;
	}

	caml_enter_blocking_section();
	v = select(maxfd + 1, &r, &w, &e, tvp);
	caml_leave_blocking_section();
	if (v == -1)
		unixext_error(errno);

	nrset = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	nwset = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	neset = caml_alloc(sizeof(struct fdset_t), Abstract_tag);

	memcpy(FDSET_OF_VALUE(nrset), &r, sizeof(fd_set));
	memcpy(FDSET_OF_VALUE(nwset), &w, sizeof(fd_set));
	memcpy(FDSET_OF_VALUE(neset), &e, sizeof(fd_set));
	MAXFD_OF_VALUE(nrset) = MAXFD_OF_VALUE(rset);
	MAXFD_OF_VALUE(nwset) = MAXFD_OF_VALUE(wset);
	MAXFD_OF_VALUE(neset) = MAXFD_OF_VALUE(eset);

	ret = caml_alloc_small(3, 0);
	Field(ret, 0) = nrset;
	Field(ret, 1) = nwset;
	Field(ret, 2) = neset;

	CAMLreturn(ret);
}

CAMLprim value stub_fdset_select_ro(value rset, value t)
{
	CAMLparam2(rset, t);
	CAMLlocal1(ret);
	fd_set r;
	int maxfd;
	double tm;
	struct timeval tv;
	struct timeval *tvp;
	int v;

	memcpy(&r, FDSET_OF_VALUE(rset), sizeof(fd_set));
	maxfd = MAXFD_OF_VALUE(rset);

	tm = Double_val(t);
	if (tm < 0.0)
		tvp = NULL;
	else {
		tv.tv_sec = (int) tm;
		tv.tv_usec = (int) (1e6 * (tm - tv.tv_sec));
		tvp = &tv;
	}

	caml_enter_blocking_section();
	v = select(maxfd + 1, &r, NULL, NULL, tvp);
	caml_leave_blocking_section();
	if (v == -1)
		unixext_error(errno);

	ret = caml_alloc(sizeof(struct fdset_t), Abstract_tag);
	memcpy(FDSET_OF_VALUE(ret), &r, sizeof(fd_set));

	CAMLreturn(ret);
}

value stub_unixext_get_major_minor(value dpath)
{
	CAMLparam1(dpath);
	CAMLlocal1(majmin);
	struct stat statbuf;
	unsigned major, minor;
	int ret;

	ret = stat(String_val(dpath), &statbuf);
	if (ret == -1)
		caml_failwith("cannot stat path");

	major = (statbuf.st_rdev & 0xfff00) >> 8;
	minor = (statbuf.st_rdev & 0xff) | ((statbuf.st_rdev >> 12) & 0xfff00);

	majmin = caml_alloc_tuple(2);
	Store_field(majmin, 0, Val_int(major));
	Store_field(majmin, 1, Val_int(minor));
	CAMLreturn(majmin);
}

CAMLprim value stub_unixext_gettimeofday_monotonic(value unit)
{
    CAMLparam1(unit);
    struct timespec ts;
    double t;
    clock_gettime( CLOCK_MONOTONIC, &ts );
    t = ts.tv_sec + ts.tv_nsec / 10e9;
    CAMLreturn(caml_copy_double(t));
}

// from otherlibs/systhreads/posix.c
#define Condition_val(v) (* ((pthread_cond_t **) Data_custom_val(v)))
#define Mutex_val(v) (* ((pthread_mutex_t **) Data_custom_val(v)))

static void caml_pthread_check(int retcode, char *msg)
{
	char * err;
	int errlen, msglen;
	value str;

	if (retcode == 0) return;
	err = strerror(retcode);
	msglen = strlen(msg);
	errlen = strlen(err);
	str = alloc_string(msglen + 2 + errlen);
	memmove (&Byte(str, 0), msg, msglen);
	memmove (&Byte(str, msglen), ": ", 2);
	memmove (&Byte(str, msglen + 2), err, errlen);
	raise_sys_error(str);
}

// from http://caml.inria.fr/mantis/view.php?id=4104
CAMLprim value caml_condition_timedwait(value v_cnd, value v_mtx, value v_timeo)
{
	CAMLparam2(v_cnd, v_mtx);
	int ret;
	pthread_cond_t *cnd = Condition_val(v_cnd);
	pthread_mutex_t *mtx = Mutex_val(v_mtx);
	double timeo = Double_val(v_timeo);
	struct timespec ts;

	ts.tv_sec = timeo;
	ts.tv_nsec = (timeo - ts.tv_sec) * 1e9;
	enter_blocking_section();
	ret = pthread_cond_timedwait(cnd, mtx, &ts);
	leave_blocking_section();
	if (ret == ETIMEDOUT) CAMLreturn(Val_false);
	caml_pthread_check(ret, "Condition.timedwait");
	CAMLreturn(Val_true);
}

CAMLprim value stub_unixext_flock_sh(value fd)
{
	CAMLparam1 (fd);
        int err;
	int c_fd = Int_val(fd);

	caml_enter_blocking_section();
        err = flock( c_fd, LOCK_SH );
	caml_leave_blocking_section();

        if ( err == -1 ) {
            unixext_error(errno);
        }
        CAMLreturn( Val_unit );
}

CAMLprim value stub_unixext_flock_ex(value fd)
{
	CAMLparam1 (fd);
        int err;
	int c_fd = Int_val(fd);

	caml_enter_blocking_section();
        err = flock( c_fd, LOCK_EX );
	caml_leave_blocking_section();

        if ( err == -1 ) {
            unixext_error(errno);
        }
        CAMLreturn( Val_unit );
}

CAMLprim value stub_unixext_flock_un(value fd)
{
	CAMLparam1 (fd);
        int err;
	int c_fd = Int_val(fd);

	caml_enter_blocking_section();
        err = flock( c_fd, LOCK_UN );
	caml_leave_blocking_section();

        if ( err == -1 ) {
            unixext_error(errno);
        }
        CAMLreturn( Val_unit );
}
