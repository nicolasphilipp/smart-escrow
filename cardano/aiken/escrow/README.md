# README

Write validators in the `validators` folder, and supporting functions in the `lib` folder using `.ak` as a file extension.

```aiken
validator my_first_validator {
  spend(_datum: Option<Data>, _redeemer: Data, _output_reference: Data, _context: Data) {
    True
  }
}
```

## Building

```sh
aiken build
```

This will generate a [CIP-0057 Plutus blueprint](https://cips.cardano.org/cip/CIP-57) as `plutus.json` at the root of the project. The blueprint contains the generated on-chain code that will be executed on-chain and a hash of the validator(s) to construct addresses.

## Testing

You can write tests in any module using the `test` keyword. For example:

```aiken
use config

test foo() {
  config.network_id + 1 == 42
}
```

To run all tests, simply do:

```sh
aiken check
```

To run only tests matching the string `foo`, do:

```sh
aiken check -m foo
```

## Resources

Find more in [Aiken's user manual](https://aiken-lang.org).
