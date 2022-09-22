FROM gitpod/workspace-base

SHELL ["/bin/bash", "-c"]
USER gitpod

RUN sh <(curl -L https://nixos.org/nix/install) --no-daemon && \
    . /home/vscode/.nix-profile/etc/profile.d/nix.sh && \
    nix-env -i stack