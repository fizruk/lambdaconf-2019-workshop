#!/bin/bash

# learn where compiled project is located
local_install_root=$(docker run \
  -v $(pwd)/project:/project \
  -it fizruk/stack-ghcjs:lts-7.19 \
  stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)
# remove trailing carriage return
local_install_root=${local_install_root%$'\r'}

# open application using default tools
open "$(pwd)/${local_install_root}/bin/corridor-client.jsexe/index.html"
