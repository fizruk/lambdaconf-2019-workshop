#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo -n "Building GHCJS part (common + client)... "
stack build --stack-yaml=stack-ghcjs.yaml --allow-different-user
echo "DONE"

echo -n "Building GHC part (common + server)... "
stack build --stack-yaml=stack.yaml --allow-different-user
echo "DONE"

STATIC_SRC="$(pwd)/ar-cube-client/static/."
STATIC_DST_VR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root --allow-different-user)/bin/ar-cube-client-vr.jsexe/"
STATIC_DST_AR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root --allow-different-user)/bin/ar-cube-client-ar.jsexe/"

echo -n "Deploying static files... "
cp -r $STATIC_SRC $STATIC_DST_VR
cp -r $STATIC_SRC $STATIC_DST_AR
echo "DONE"

echo "Project built successfully!"
