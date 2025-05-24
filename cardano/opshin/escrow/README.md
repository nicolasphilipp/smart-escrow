# README

## Building

```sh
opshin build escrow.py
```

This generates a build folder in the current directory that looks like this:

```sh
build/
  └-escrow/
    ├-blueprint.json
    ├-mainnet.addr
    ├-script.cbor
    ├-script.plutus
    ├-script.policy_id
    └-testnet.addr
```

It generates a [CIP-0057 Plutus blueprint](https://cips.cardano.org/cip/CIP-57) as `blueprint.json` and also gives the mainnet and testnet address.

## Resources

Find more in the [Opshin documentation](https://book.opshin.dev/).
