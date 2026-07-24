#!/usr/bin/env bash

# Pin claude-code to a specific release, ahead of nixpkgs if needed.
#
# Usage:
#   scripts/update-claude-code.sh            # pin to the latest release
#   scripts/update-claude-code.sh 2.1.219    # pin to a specific version
#
# Rewrites claude-code.json (version + darwin-arm64 checksum) from Anthropic's
# official release manifest, then run `home-manager switch --flake .#sean-darwin`.

set -euo pipefail

base="https://downloads.claude.ai/claude-code-releases"
platform="darwin-arm64"

version="${1:-$(curl -fsSL "${base}/latest")}"

checksum=$(curl -fsSL "${base}/${version}/manifest.json" \
  | jq -r --arg p "$platform" '.platforms[$p].checksum')

if [[ -z "$checksum" || "$checksum" == "null" ]]; then
  echo "error: no ${platform} checksum found for version ${version}" >&2
  exit 1
fi

out="$(cd "$(dirname "$0")/.." && pwd)/claude-code.json"
cat > "$out" <<EOF
{
  "version": "${version}",
  "checksum": "${checksum}"
}
EOF

echo "Pinned claude-code ${version}"
echo "  ${platform} sha256: ${checksum}"
echo "  wrote ${out}"
echo
echo "Now run: home-manager switch --flake .#sean-darwin"
