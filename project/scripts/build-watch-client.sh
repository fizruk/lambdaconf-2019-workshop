#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

STATIC_SRC="$(pwd)/client/static/."
STATIC_DST="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/corridor-client.jsexe/"

stack build --stack-yaml=stack-ghcjs.yaml \
  --file-watch \
  --exec "cp -r $STATIC_SRC $STATIC_DST"
