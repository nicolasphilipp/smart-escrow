use aiken/collection/list
use aiken/crypto.{verify_ed25519_signature}
use aiken/primitive/bytearray.{concat}
use cardano/transaction.{OutputReference, Transaction}
use escrow/types.{Accept, Complaint, Datum, Redeemer}
use escrow/utils

validator escrow {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    utxo: OutputReference,
    self: Transaction,
  ) {
    expect Some(Datum {
      seller,
      buyer,
      price,
      seller_deposit,
      buyer_deposit,
      product_hash,
      nonce,
    }) = datum
    trace @"redeemer": redeemer

    expect Some(own_input) =
      list.find(self.inputs, fn(input) { input.output_reference == utxo })

    let contract_address = own_input.output.address
    let contract_balance =
      utils.get_ada_from_inputs(
        utils.get_inputs_by_addr(self, contract_address),
      )
    trace @"balance": contract_balance

    let must_match_total =
      contract_balance == seller_deposit + buyer_deposit + price
    let seller_outputs = utils.get_outputs_by_key(self, seller)
    trace @"Accept": @"seller_outputs", seller_outputs
    let buyer_outputs = utils.get_outputs_by_key(self, buyer)
    trace @"Accept": @"buyer_outputs", buyer_outputs

    when redeemer is {
      Accept { r_nonce } -> {
        // only buyer can trigger accept
        let must_be_signed = list.has(self.extra_signatories, buyer)
        let must_match_nonce = nonce == r_nonce

        let seller_must_be_compensated =
          utils.get_ada_from_outputs(seller_outputs) == seller_deposit + price
        let buyer_must_get_deposit =
          utils.get_ada_from_outputs(buyer_outputs) == buyer_deposit

        and {
          must_be_signed?,
          must_match_nonce?,
          must_match_total?,
          seller_must_be_compensated?,
          buyer_must_get_deposit?,
        }
      }
      Complaint { r_nonce, r_signature, r_product_hash } -> {
        // both seller and buyer must sign to submit complaint
        let must_be_signed =
          list.has(self.extra_signatories, buyer) && list.has(
            self.extra_signatories,
            seller,
          )
        let must_match_nonce = nonce == r_nonce

        if verify_ed25519_signature(
          seller,
          concat(r_product_hash, nonce),
          r_signature,
        ) {
          if product_hash == r_product_hash {
            // buyer tried to cheat
            let seller_must_get_deposit =
              utils.get_ada_from_outputs(seller_outputs) == seller_deposit + price
            let buyer_must_get_nothing =
              utils.get_ada_from_outputs(buyer_outputs) == 0

            and {
              must_be_signed?,
              must_match_nonce?,
              must_match_total?,
              seller_must_get_deposit?,
              buyer_must_get_nothing?,
            }
          } else {
            // seller tried to cheat
            let seller_must_get_nothing =
              utils.get_ada_from_outputs(seller_outputs) == 0
            let buyer_must_get_deposit =
              utils.get_ada_from_outputs(buyer_outputs) == buyer_deposit + price

            and {
              must_be_signed?,
              must_match_nonce?,
              must_match_total?,
              seller_must_get_nothing?,
              buyer_must_get_deposit?,
            }
          }
        } else {
          // seller tried to cheat
          let seller_must_get_nothing =
            utils.get_ada_from_outputs(seller_outputs) == 0
          let buyer_must_get_deposit =
            utils.get_ada_from_outputs(buyer_outputs) == buyer_deposit + price

          and {
            must_be_signed?,
            must_match_nonce?,
            must_match_total?,
            seller_must_get_nothing?,
            buyer_must_get_deposit?,
          }
        }
      }
    }
  }

  else(_) {
    fail @"unsupported"
  }
}
