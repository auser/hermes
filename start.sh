#!/bin/sh
cd `dirname $0`
if [[ ! -f ebin/hermes.boot ]]; then
	make all_boot
fi
erl -pa $PWD/ebin -pa $PWD/deps/*/ebin -name hermes -s reloader -boot hermes $*