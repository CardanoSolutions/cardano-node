# CIP-0094 / cardano-cli 

This repository contains a clone of [input-output-hk/cardano-node][] with an extra patch that extends the `cardano-cli` to includes features described in [CIP-0094][]. More specifically, the features have been <u>implemented</u>, <u>tested</u>, <u>reviewed</u> and <u>approved</u> on the source [input-output-hk/cardano-node][] repository through the following changes:

- [input-output-hk/cardano-node#5050](https://github.com/input-output-hk/cardano-node/pull/5050)
- [input-output-hk/cardano-node#5132](https://github.com/input-output-hk/cardano-node/pull/5132)

However, the main branch on the cardano-node's repository lives months ahead of the latest release. In addition, there's a lot of other work streams in integration which make it hard to cut a new release of the cardano-node/cardano-cli including those changes at the moment. Therefore, changes have been [backported on top of the latest release `1.35.7`](https://github.com/input-output-hk/cardano-node/pull/5152). This repository contains the backported source and a release of `1.35.7` augmented with the [CIP-0094][] additions.

## How to use

See the following [📘 tutorial](https://hackmd.io/@KtorZ/cip-0094-tutorial).

## How to build (Linux only)

### Pre-requisite

- [git](https://github.com/git-guides/install-git) >= 2.0.0

- [nix](https://nixos.wiki/wiki/Nix_Installation_Guide) >= 2.0.0
  > **Note**
  > Here are some quick installation instructions:
  >
  > ```bash
  > curl -L https://nixos.org/nix/install > install-nix.sh
  > chmod +x install-nix.sh
  > ./install-nix.sh
  > ```

- [niv](https://github.com/nmattia/niv) >= 0.2.0
  > **Note** 
  > You can install this one directly through Nix via:
  > 
  > ```
  > nix-env -iA nixpkgs.niv
  > ```

### Configure Nix

Make sure to use the binary cache maintained by _Input Output_ to speed up compilation down the line:

```bash
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

### Clone `input-output-hk/tools`

```
git clone https://github.com/input-output-hk/tools
cd tools
```

### Update `cardano-node/cardano-cli` revision

```
cd arm-test
niv update cardano-node-mainnet -o CardanoSolutions -r cardano-node -b release/1.35+cip-0094
cd ..
```

### Build a static executable

```
nix build -f release.nix cardano-node.x86_64-linux-musl
```

This may take some time but eventually generate a `result/` directory with a
zip archive containing the patched version of the cardano-cli ready to use.

[input-output-hk/cardano-node]: https://github.com/input-output-hk/cardano-node
[CIP-0094]: https://github.com/cardano-foundation/CIPs/pull/496
