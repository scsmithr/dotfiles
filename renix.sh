#!/usr/bin/env bash

# Update /etc/zshrc with nix specific stuff if needed.
#
# Necessary after most mac updates.

set -e

if grep -Fxq "# Nix" /etc/zshrc
then
    echo "/etc/zshrc already has nix configuration"
else
    echo "adding nix configuration to /etc/zshrc"
    echo "
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
" | sudo tee -a /etc/zshrc
    echo "nix configuration added"
fi
