#!/bin/bash
#
# Copyright (c) 2009 Citrix Systems, Inc.
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

PROG=./test_parser

for f in `ls *pass*.json`; do
	$PROG $f -no-print-value
	ec=$?
	if [ $ec -eq 1 ] ; then
		echo "Test case $f should pass, but failed."
	elif [ $ec -eq 255 ] ; then
		echo "Test case $f triggered an internal error!"
	fi
done
for f in `ls *fail*.json`; do
	$PROG $f -no-print-value
	ec=$?
	if [ $ec -eq 0 ] ; then
		echo "Test case $f should fail, but passed."
	elif [ $ec -eq 255 ] ; then
		echo "Test case $f triggered an internal error!"
	fi
done
