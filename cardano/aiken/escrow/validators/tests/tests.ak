use cardano/address.{Address, Script, VerificationKey}
use cardano/assets
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction,
}
use escrow
use escrow/types.{Accept, Datum, Redeemer, Refund}

test accept_escrow() {
  let datum =
    Datum {
      seller: #"00000000000000000000000000000000000000000000000000000001",
      buyer: #"00000000000000000000000000000000000000000000000000000002",
      price: 15000000,
      seller_deposit: 20000000,
      buyer_deposit: 5000000,
    }

  let redeemer = Redeemer { action: Accept }
  let utxo = OutputReference { transaction_id: "", output_index: 1 }
  let escrow_input =
    Input {
      output_reference: utxo,
      output: Output {
        address: Address {
          payment_credential: Script(
            #"00000000000000000000000000000000000000000000000000000000",
          ),
          stake_credential: None,
        },
        value: assets.from_lovelace(40000000),
        datum: InlineDatum(datum),
        reference_script: None,
      },
    }

  let seller_output =
    Output {
      address: Address {
        payment_credential: VerificationKey(
          #"00000000000000000000000000000000000000000000000000000001",
        ),
        stake_credential: None,
      },
      value: assets.from_lovelace(35000000),
      datum: NoDatum,
      reference_script: None,
    }

  let buyer_output =
    Output {
      address: Address {
        payment_credential: VerificationKey(
          #"00000000000000000000000000000000000000000000000000000002",
        ),
        stake_credential: None,
      },
      value: assets.from_lovelace(5000000),
      datum: NoDatum,
      reference_script: None,
    }

  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [escrow_input],
      outputs: [seller_output, buyer_output],
      extra_signatories: [datum.buyer],
    }
  escrow.escrow.spend(Some(datum), redeemer, utxo, tx)
}

test refund_escrow() {
  let datum =
    Datum {
      seller: #"00000000000000000000000000000000000000000000000000000001",
      buyer: #"00000000000000000000000000000000000000000000000000000002",
      price: 15000000,
      seller_deposit: 20000000,
      buyer_deposit: 5000000,
    }

  let redeemer = Redeemer { action: Refund }
  let utxo = OutputReference { transaction_id: "", output_index: 1 }
  let escrow_input =
    Input {
      output_reference: utxo,
      output: Output {
        address: Address {
          payment_credential: Script(
            #"00000000000000000000000000000000000000000000000000000000",
          ),
          stake_credential: None,
        },
        value: assets.from_lovelace(40000000),
        datum: InlineDatum(datum),
        reference_script: None,
      },
    }

  let seller_output =
    Output {
      address: Address {
        payment_credential: VerificationKey(
          #"00000000000000000000000000000000000000000000000000000001",
        ),
        stake_credential: None,
      },
      value: assets.from_lovelace(20000000),
      datum: NoDatum,
      reference_script: None,
    }

  let buyer_output =
    Output {
      address: Address {
        payment_credential: VerificationKey(
          #"00000000000000000000000000000000000000000000000000000002",
        ),
        stake_credential: None,
      },
      value: assets.from_lovelace(20000000),
      datum: NoDatum,
      reference_script: None,
    }

  let tx =
    Transaction {
      ..transaction.placeholder,
      inputs: [escrow_input],
      outputs: [seller_output, buyer_output],
      extra_signatories: [datum.buyer, datum.seller],
    }
  escrow.escrow.spend(Some(datum), redeemer, utxo, tx)
}
