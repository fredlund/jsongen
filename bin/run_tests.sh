#!/bin/sh

# A stupid shell script for starting erlang with the right libraries;
# there is a probably a better way, but...

CMD=`basename $0`
PRG=$0

while [ -L "${PRG}" ]; do
    ls=`ls -ld "${PRG}"`
    link=`expr "$ls" : '.*-> \(.*\)$'`
    if expr "${link}" : '\/' > /dev/null; then
        PRG="${link}"
    else
        PRG="`dirname ${PRG}`/${link}"
    fi
done

JSONGENHOME=`dirname "${PRG}"`/..

# Exits successfully if testing succeeds, and otherwise a failure condition.
# That is, this command is composable with other shell scripts.
erl -pa $JSONGENHOME/ebin -run rtest check


