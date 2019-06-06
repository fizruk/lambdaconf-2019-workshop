#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

echo -n "Launching ghcid for (common + server)... "
/root/.local/bin/ghcid -c "stack repl --stack-yaml=stack.yaml --allow-different-user"
