#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

case $1 in
  start|stop|ping)
    echo ./prime_numbers/bin/prime_numbers $1
    exec $DIR/prime_numbers/bin/prime_numbers $1
    ;;
  *)
    echo "u can use {start|stop|ping}"
esac
