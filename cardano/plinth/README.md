# README

## Building

To get the needed environment for compiling the contract, use the provided image by the IOHK:

```
> docker run \
    -v /path-to-project/cardano/plinth:/workspaces/plinth \
    -it ghcr.io/input-output-hk/devx-devcontainer:x86_64-linux.ghc96-iog
```

Inside of the container enter the project directory and run `cabal update` followed by `cabal build`.

```
[workspaces] cd plinth
[workspaces/plinth] cabal update
[workspaces/plinth] cabal build
```

To compile and generate the blueprint run the following command:

```
cabal run gen-escrow-validator-blueprint -- ./off-chain/escrow-validator-blueprint.json
```

This generates a [CIP-0057 Plutus blueprint](https://cips.cardano.org/cip/CIP-57) as `escrow-validator-blueprint.json` in the `/off-chain` folder.

## Resources

Find more in the [Plinth documentation](https://plutus.cardano.intersectmbo.org/docs/).

Other examples: <br> [Plinth Template](https://github.com/IntersectMBO/plinth-template) (does not use Plutus V3)<br>
[cardano-plinth-examples](https://github.com/lley154/cardano-plinth-examples) (most up-to-date example)