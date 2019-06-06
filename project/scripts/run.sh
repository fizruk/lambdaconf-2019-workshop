#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

STATIC_DIR_VR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root --allow-different-user)/bin/ar-cube-client-vr.jsexe/"
STATIC_DIR_AR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root --allow-different-user)/bin/ar-cube-client-ar.jsexe/"

echo "Starting ar-cube-server..."
stack exec --stack-yaml=stack.yaml --allow-different-user ar-cube-server \
    -- "$STATIC_DIR_VR" "$STATIC_DIR_AR"
