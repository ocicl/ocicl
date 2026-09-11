# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-
#
# --- Make targets for test echo server.
#

# On my mingw/msys system this Makefile doesn't work so well for
# some reason.  The following, however, does work:
#   gcc -c echo-server.c
#   g++ -l echo-server.o -o echo-server.exe

CFLAGS ?= -Wall

all: echo-server

echo-server: echo-server.c
	$(CC) -o $@ $(CFLAGS) $<

clean:
	rm -f echo-server echo-server.exe

# vim: ft=make ts=4 noet
