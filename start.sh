#!/bin/sh

# Make the boot file, if necessary
# cd `dirname $0`
# if [[ ! -f ebin/hermes.boot ]]; then
#     make boot
#fi

# Find the next free erlang nodename using epmd  
EXISTING_NAMES=`epmd -names`

DEP_EBINS=`find deps -type d | grep -v \/test\/ | grep ebin | grep -v .svn | grep -v .git`
CLIENT_NAME="hermes"
RELEASE=`ls -1 ebin/$CLIENT_NAME*.boot | sed 's/\(ebin\/hermes-\)\(.*\)\(\.boot\)/\2 \1\2\3/g' | awk -F '.' '{ ver=1000000*\$1 + 1000*\$2 + \$3; printf "%010d %s\n", ver, \$0}'| sort | awk '{print \$3}' | tail -n 1`
APP_NAME=`basename $RELEASE .boot`
echo $APP_NAME
 
exec erl \
    -pa ebin \
    -pa $DEP_EBINS \
    -name $CLIENT_NAME \
    -s reloader \
    -boot $APP_NAME \
    $*