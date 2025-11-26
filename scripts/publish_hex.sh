#!/usr/bin/env bash
set -e

# Script to publish grpc packages to Hex in the correct order
# Usage: ./scripts/publish_hex.sh <version>
# Example: ./scripts/publish_hex.sh 1.0.0

VERSION=$1

if [ -z "$VERSION" ]; then
  echo "Error: Version is required"
  echo "Usage: $0 <version>"
  echo "Example: $0 1.0.0"
  exit 1
fi

# Color output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}Publishing grpc packages version ${VERSION} to Hex...${NC}"

# Function to toggle dependencies from path to hex
toggle_to_hex() {
  local mix_file=$1
  echo -e "${BLUE}Switching ${mix_file} to hex dependencies...${NC}"
  
  # Comment path dependency and uncomment hex dependency
  sed -i 's/^\(\s*{:grpc_core, path:.*\)$/# \1/' "$mix_file"
  sed -i 's/^# \(\s*{:grpc_core, "~> .*# Uncomment for hex release\)$/\1/' "$mix_file"
  
  sed -i 's/^\(\s*{:grpc_server, path:.*\)$/# \1/' "$mix_file"
  sed -i 's/^# \(\s*{:grpc_server, "~> .*# Uncomment for hex release\)$/\1/' "$mix_file"
}

# Function to toggle dependencies from hex to path
toggle_to_path() {
  local mix_file=$1
  echo -e "${BLUE}Switching ${mix_file} back to path dependencies...${NC}"
  
  # Uncomment path dependency and comment hex dependency
  sed -i 's/^# \(\s*{:grpc_core, path:.*\)$/\1/' "$mix_file"
  sed -i 's/^\(\s*{:grpc_core, "~> .*# Uncomment for hex release\)$/# \1/' "$mix_file"
  
  sed -i 's/^# \(\s*{:grpc_server, path:.*\)$/\1/' "$mix_file"
  sed -i 's/^\(\s*{:grpc_server, "~> .*# Uncomment for hex release\)$/# \1/' "$mix_file"
}

# Function to verify version in mix.exs
verify_version() {
  local package=$1
  local mix_file="${package}/mix.exs"
  
  echo -e "${BLUE}Verifying version in ${mix_file}...${NC}"
  
  if ! grep -q "@version \"${VERSION}\"" "$mix_file"; then
    echo -e "${RED}Error: Version mismatch in ${mix_file}${NC}"
    echo -e "${RED}Expected: @version \"${VERSION}\"${NC}"
    exit 1
  fi
  
  echo -e "${GREEN}✓ Version ${VERSION} confirmed in ${mix_file}${NC}"
}

# Function to publish a package
publish_package() {
  local package=$1
  local mix_file="${package}/mix.exs"
  
  echo -e "${BLUE}Publishing ${package}...${NC}"
  
  cd "$package"
  
  # Clean and get deps
  mix deps.clean --all
  mix deps.get
  
  # Build hex package
  echo -e "${BLUE}Building hex package for ${package}...${NC}"
  mix hex.build
  
  # Publish to hex
  echo -e "${BLUE}Publishing ${package} to Hex...${NC}"
  mix hex.publish --yes
  
  cd ..
  
  echo -e "${GREEN}✓ Successfully published ${package} ${VERSION}${NC}"
}

# Cleanup function to restore path dependencies on exit
cleanup() {
  echo -e "${BLUE}Restoring path dependencies...${NC}"
  toggle_to_path "grpc_server/mix.exs"
  toggle_to_path "grpc_client/mix.exs"
  echo -e "${GREEN}✓ Dependencies restored${NC}"
}

# Set trap to always restore dependencies
trap cleanup EXIT

# Verify versions in all packages
echo -e "${BLUE}Verifying versions...${NC}"
verify_version "grpc_core"
verify_version "grpc_server"
verify_version "grpc_client"

# Step 1: Publish grpc_core (no dependencies on other packages)
echo -e "${BLUE}Step 1/3: Publishing grpc_core...${NC}"
publish_package "grpc_core"

# Wait a bit for Hex to process
echo -e "${BLUE}Waiting for Hex to process grpc_core...${NC}"
sleep 5

# Step 2: Publish grpc_server (depends on grpc_core)
echo -e "${BLUE}Step 2/3: Publishing grpc_server...${NC}"
toggle_to_hex "grpc_server/mix.exs"
publish_package "grpc_server"

# Wait a bit for Hex to process
echo -e "${BLUE}Waiting for Hex to process grpc_server...${NC}"
sleep 5

# Step 3: Publish grpc_client (depends on grpc_core and grpc_server for tests)
echo -e "${BLUE}Step 3/3: Publishing grpc_client...${NC}"
toggle_to_hex "grpc_client/mix.exs"
publish_package "grpc_client"

echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
echo -e "${GREEN}✓ All packages published successfully!${NC}"
echo -e "${GREEN}  - grpc_core ${VERSION}${NC}"
echo -e "${GREEN}  - grpc_server ${VERSION}${NC}"
echo -e "${GREEN}  - grpc_client ${VERSION}${NC}"
echo -e "${GREEN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
