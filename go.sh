#!/bin/sh
dir=`pwd`
running_nail=`ps -ef | grep -v 'grep' | grep 'nailgun.NGServer' | awk '{print $2}'`
if [[ -n "$running_nail" ]]; then
    kill -9 "$running_nail"
fi
(cd $dir && mvn clojure:nailgun &>/dev/null) & vim
