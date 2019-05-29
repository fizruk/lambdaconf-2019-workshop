#!/bin/bash
docker run \
  -v $(pwd)/project:/project \
  -p 8019:8019 \
  -it fizruk/stack-ghcjs:lts-7.19 \
  ./build.sh
