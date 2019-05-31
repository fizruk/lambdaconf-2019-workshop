#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo "Starting corridor-server..."
stack exec corridor-server -- "$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/corridor-client.jsexe/"
