#!/bin/bash
# NOTE: to be run from inside Docker container

set -e # exit on failure

STATIC_DIR_VR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/ar-cube-client-vr.jsexe/"
STATIC_DIR_AR="$(stack path --stack-yaml=stack-ghcjs.yaml --local-install-root)/bin/ar-cube-client-ar.jsexe/"

# build server for the first time
stack build --stack-yaml=stack.yaml

# automatically restart server if it shuts down
while true; do
  echo "(Re)starting ar-cube-server..."
  stack exec --stack-yaml=stack.yaml ar-cube-server \
    -- "$STATIC_DIR_VR" "$STATIC_DIR_AR" \
    || true # avoid exiting when server stops
done & # do restarts in background

# NOTE: we don't want to exit on failure above

# Thanks for this idea to a reply in a Reddit thread:
# https://www.reddit.com/r/haskell/comments/44b5rv/rebuild_restart_program_on_file_change/czov7j6/
stack build --stack-yaml=stack.yaml \
  --file-watch \
  --exec "killall ar-cube-server" # force restart after rebuild
