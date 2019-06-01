#!/bin/bash

cmd=$1

SERVER_PORT=8019

if [ -z "$cmd" ]; then
  echo "Usage: $0 <command>"

  echo "Available commands:"
  for path in $(ls project/scripts/*.sh);
  do
    filename=$(basename "$path")
    echo "  - ${filename%.*}"
  done
else
  DOCKER_OPTIONS=""
  if [[ $cmd == "run" ]] || [[ $cmd == "run-watch-server" ]] || [[ $cmd == "repl" ]]; then
    DOCKER_OPTIONS="-p $SERVER_PORT:$SERVER_PORT" # expose server ports
  fi

  local_ip=$(ifconfig | sed -En 's/127.0.0.1//;s/.*inet (addr:)?(([0-9]*\.){3}[0-9]*).*/\2/p')

  docker run \
    $DOCKER_OPTIONS \
    -e LOCAL_IP_ADDRESS="$local_ip" \
    -v $(pwd)/project:/project \
    -it fizruk/stack-ghcjs:lts-7.19 \
    "./scripts/${cmd}.sh"
fi
