use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use cardano/address.{Address, VerificationKey}
use cardano/assets
use cardano/transaction.{Input, Output, Transaction}

pub fn get_outputs_by_key(
  tx: Transaction,
  key: VerificationKeyHash,
) -> List<Output> {
  list.filter(
    tx.outputs,
    fn(output) { output.address.payment_credential == VerificationKey(key) },
  )
}

pub fn get_inputs_by_key(
  tx: Transaction,
  key: VerificationKeyHash,
) -> List<Input> {
  list.filter(
    tx.inputs,
    fn(input) {
      input.output.address.payment_credential == VerificationKey(key)
    },
  )
}

pub fn get_outputs_by_addr(tx: Transaction, addr: Address) -> List<Output> {
  list.filter(tx.outputs, fn(output) { output.address == addr })
}

pub fn get_inputs_by_addr(tx: Transaction, addr: Address) -> List<Input> {
  list.filter(tx.inputs, fn(input) { input.output.address == addr })
}

pub fn get_ada_from_outputs(outputs: List<Output>) -> Int {
  list.reduce(
    outputs,
    0,
    fn(acc, output) { acc + assets.lovelace_of(output.value) },
  )
}

pub fn get_ada_from_inputs(inputs: List<Input>) -> Int {
  list.reduce(
    inputs,
    0,
    fn(acc, input) { acc + assets.lovelace_of(input.output.value) },
  )
}
