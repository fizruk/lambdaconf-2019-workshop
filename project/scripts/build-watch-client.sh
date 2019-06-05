#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

STATIC_SRC="$(pwd)/ar-cube-client/static/."
STATIC_DST_AR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/ar-cube-client-ar.jsexe/"
STATIC_DST_VR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/ar-cube-client-vr.jsexe/"

stack build --stack-yaml=stack-ghcjs.yaml \
  --file-watch \
  --exec "cp -r $STATIC_SRC $STATIC_DST_VR" \
  --exec "cp -r $STATIC_SRC $STATIC_DST_AR"
