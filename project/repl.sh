#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo -n "Deploying static files... "
cp -r \
  client/static/* \
  "$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/corridor-client.jsexe/."
echo "DONE"

echo -n "Launching GHCi for (common + server)... "
stack repl --stack-yaml=stack.yaml
echo "DONE"
