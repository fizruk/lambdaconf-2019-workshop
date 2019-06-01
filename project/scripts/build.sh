#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo -n "Building GHCJS part (common + client)... "
stack build --stack-yaml=stack-ghcjs.yaml
echo "DONE"

echo -n "Building GHC part (common + server)... "
stack build --stack-yaml=stack.yaml
echo "DONE"

echo -n "Deploying static files... "
cp -r \
  client/static/* \
  "$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/corridor-client.jsexe/."
echo "DONE"

echo "Project built successfully!"
