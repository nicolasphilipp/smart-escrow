use aiken/collection/list
use cardano/transaction.{OutputReference, Transaction}
use escrow/types.{Accept, Datum, Redeemer, Refund}
use escrow/utils

validator escrow {
  spend(
    datum: Option<Datum>,
    redeemer: Redeemer,
    utxo: OutputReference,
    self: Transaction,
  ) {
    expect Some(Datum { seller, buyer, price, seller_deposit, buyer_deposit }) =
      datum
    trace @"redeemer": redeemer.action

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
    when redeemer.action is {
      Accept -> {
        // only buyer can trigger accept
        let must_be_signed = list.has(self.extra_signatories, buyer)

        let seller_outputs = utils.get_outputs_by_key(self, seller)
        trace @"Accept": @"seller_outputs", seller_outputs
        let seller_must_be_compensated =
          utils.get_ada_from_outputs(seller_outputs) == seller_deposit + price

        let buyer_outputs = utils.get_outputs_by_key(self, buyer)
        trace @"Accept": @"buyer_outputs", buyer_outputs
        let buyer_must_get_deposit =
          utils.get_ada_from_outputs(buyer_outputs) == buyer_deposit

        and {
          must_be_signed?,
          must_match_total?,
          seller_must_be_compensated?,
          buyer_must_get_deposit?,
        }
      }
      Refund -> {
        // both seller and buyer must sign to refund
        let must_be_signed =
          list.has(self.extra_signatories, buyer) && list.has(
            self.extra_signatories,
            seller,
          )

        let seller_outputs = utils.get_outputs_by_key(self, seller)
        trace @"Refund": @"seller_outputs", seller_outputs
        let seller_must_get_deposit =
          utils.get_ada_from_outputs(seller_outputs) == seller_deposit

        let buyer_outputs = utils.get_outputs_by_key(self, buyer)
        trace @"Refund": @"buyer_outputs", buyer_outputs
        let buyer_must_get_deposit =
          utils.get_ada_from_outputs(buyer_outputs) == buyer_deposit + price

        and {
          must_be_signed?,
          must_match_total?,
          seller_must_get_deposit?,
          buyer_must_get_deposit?,
        }
      }
    }
  }

  else(_) {
    fail @"unsupported"
  }
}
