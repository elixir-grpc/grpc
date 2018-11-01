#!/usr/bin/env bash
# This script builds/restores PLT cache files to/from a cache at ~/.pltcache
set -ex

# "Mix will default to the :dev environment"
MIX_ENV="${MIX_ENV:-dev}"

mix compile

# Create the PLT cache directory, if it doesn't already exist
mkdir -p "$HOME"/.pltcache
# Copy the PLT files into the _build directory, if they exist
cp "$HOME"/.pltcache/*-"$MIX_ENV".plt _build/"$MIX_ENV"/ || true

# Build the PLT cache (uses the existing one if present)
mix dialyzer --plt

# Copy the PLT files into the cache so they can be used next time
cp _build/"$MIX_ENV"/*-"$MIX_ENV".plt "$HOME"/.pltcache/
