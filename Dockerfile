# syntax=docker/dockerfile:1
FROM nixos/nix

RUN nix-channel --update
RUN nix-env -iA nixpkgs.gnumake

# generate a working directory
WORKDIR /flora-server 

# copy the files relevant to build core dependencies
COPY default.nix shell.nix flora.cabal environment.sh Makefile ./
COPY nix/ ./nix/

# let nix build the dependencies. This uses nix-shell to cache the setup phase.
RUN nix-shell

# copy asset-relevant dependency files
COPY assets/package.json assets/yarn.lock assets/
RUN nix-shell --run "make assets-deps"

# copy the rest of the flora-server repository into the image
COPY . . 

ENTRYPOINT ["nix-shell"]
