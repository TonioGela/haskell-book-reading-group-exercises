FROM gitpod/workspace-base

SHELL ["/bin/bash", "-c"]
USER gitpod
ENV USER gitpod

RUN sh <(curl -L https://nixos.org/nix/install) --no-daemon && \
    source /home/gitpod/.nix-profile/etc/profile.d/nix.sh && \
    echo source /home/gitpod/.nix-profile/etc/profile.d/nix.sh > $HOME/.bashrc.d/nix.sh && \
    nix-env -i stack