#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo -n "Cleaning up GHCJS part (common + client)... "
stack clean --stack-yaml=stack-ghcjs.yaml --allow-different-user
echo "DONE"

echo -n "Cleaning up GHC part (common + server)... "
stack clean --stack-yaml=stack.yaml --allow-different-user
echo "DONE"
