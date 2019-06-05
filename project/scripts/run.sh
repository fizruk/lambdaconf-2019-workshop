#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo "Starting ar-cube-server..."
stack exec ar-cube-server -- "$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/ar-cube-client.jsexe/"
