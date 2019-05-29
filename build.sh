#!/bin/bash
docker run \
  -v $(pwd)/project:/project \
  -it fizruk/stack-ghcjs:lts-7.19 \
  ./build.sh
