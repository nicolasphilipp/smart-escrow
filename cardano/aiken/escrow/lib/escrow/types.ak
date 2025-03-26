use aiken/crypto.{VerificationKeyHash}

pub type Action {
  Accept
  Refund
}

pub type Datum {
  seller: VerificationKeyHash,
  buyer: VerificationKeyHash,
  price: Int,
  seller_deposit: Int,
  buyer_deposit: Int,
}

pub type Redeemer {
  action: Action,
}
