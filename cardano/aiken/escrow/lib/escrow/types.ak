use aiken/crypto.{VerificationKeyHash}

pub type Datum {
  seller: VerificationKeyHash,
  buyer: VerificationKeyHash,
  price: Int,
  seller_deposit: Int,
  buyer_deposit: Int,
  product_hash: ByteArray,
  nonce: ByteArray,
}

pub type Redeemer {
  Accept { r_nonce: ByteArray }
  Complaint {
    r_nonce: ByteArray,
    r_signature: ByteArray,
    r_product_hash: ByteArray,
  }
}
