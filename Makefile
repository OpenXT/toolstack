#
# Copyright (c) 2012 Citrix Systems, Inc.
# 
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
# 
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#

.PHONY: all
all: build

INSTALL = install

HOTPLUG_SCRIPTS=block block-frontend tap vif

SUBDIRS        = libs/uuid libs/stdext \
                 libs/json libs/jsonrpc libs/http \
                 libs/log libs/common

STAGE_SUBDIRS  = libs/uuid libs/stdext \
                 libs/json libs/jsonrpc libs/http \
                 libs/log libs/common

-include extra/Makefile

.PHONY: build build-stage build-nostage $(SUBDIRS)

build: $(SUBDIRS)
	for dir in $(SUBDIRS); do \
		echo "=> $$dir"; \
		$(MAKE) --no-print-directory -C $$dir || exit 1; \
	done

stage: $(STAGE_SUBDIRS)
	for dir in $(STAGE_SUBDIRS); do \
	        echo "STAGING => $$dir"; \
		$(MAKE) --no-print-directory -C $$dir stage || exit 1; \
	done

install-scripts:
	$(INSTALL) -d $(DESTDIR)/etc/udev/rules.d $(DESTDIR)/etc/xen/scripts \
		$(DESTDIR)/etc/ifplugd
	$(INSTALL) scripts/*.rules $(DESTDIR)/etc/udev/rules.d/.
	$(INSTALL) $(addprefix scripts/,$(HOTPLUG_SCRIPTS)) \
		$(DESTDIR)/etc/xen/scripts
	$(INSTALL) scripts/ifplugd.* $(DESTDIR)/etc/ifplugd/.

install: install-scripts

clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) --no-print-directory -C $$dir clean; \
	done
