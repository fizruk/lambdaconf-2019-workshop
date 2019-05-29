#!/bin/bash
# NOTE: to be run from inside Docker container

echo -n "Building GHCJS project... "
stack build
echo "DONE"

echo -n "Deploying static files... "
cp -r \
  static/* \
  "$(stack path --local-install-root)/bin/corridor.jsexe/."
echo "DONE"

echo "Project built successfully!"
