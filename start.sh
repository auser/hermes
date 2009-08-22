#!/bin/sh

# Make the boot file, if necessary
cd `dirname $0`
if [[ ! -f ebin/hermes.boot ]]; then
	make boot
fi

# Find the next free erlang nodename using epmd  
EXISTING_NAMES=`epmd -names`

DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
 
CLIENT_NAME="hermes"
 
exec erl \
    -pa ebin \
    -pa $DEP_EBINS \
    -name $CLIENT_NAME \
    -s reloader \
    -boot hermes \
    $*