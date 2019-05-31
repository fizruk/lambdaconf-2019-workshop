#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

STATIC_DIR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/corridor-client.jsexe/"

# build server for the first time
stack build --stack-yaml=stack.yaml

# automatically restart server if it shuts down
while true; do
  echo "(Re)starting corridor-server..."
  stack exec --stack-yaml=stack.yaml corridor-server -- "$STATIC_DIR" \
    || true # avoid exiting when server stops
done & # do restarts in background

# NOTE: we don't want to exit on failure above

# Thanks for this idea to a reply in a Reddit thread:
# https://www.reddit.com/r/haskell/comments/44b5rv/rebuild_restart_program_on_file_change/czov7j6/
stack build --stack-yaml=stack.yaml \
  --file-watch \
  --exec "killall corridor-server" # force restart after rebuild
