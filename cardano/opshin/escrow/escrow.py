# escrow.py
from opshin.prelude import *
from opshin.std.builtins import *


@dataclass()
class EscrowDatum(PlutusData):
  seller_pubkeyhash: PubKeyHash
  buyer_pubkeyhash: PubKeyHash
  price: int
  seller_deposit: int
  buyer_deposit: int
  product_hash: bytes
  nonce: int


@dataclass
class Accept(PlutusData):
  CONSTR_ID = 0
  nonce: int
  
@dataclass
class Complaint(PlutusData):
  CONSTR_ID = 1
  sig: bytes # signature of str(product_hash) + str(nonce), signed with seller's secret key
  product_hash: bytes
  nonce: int
  
Redeemer = Union[Accept, Complaint]


def get_outputs_by_key(context: ScriptContext, key: PubKeyHash) -> List[TxOut]:
  return [output for output in context.tx_info.outputs if output.address.payment_credential == PubKeyCredential(key)]
  

def get_inputs_by_addr(context: ScriptContext, addr: Address) -> List[TxInInfo]:
  return [input for input in context.tx_info.inputs if input.resolved.address == addr]
  

def get_ada_from_outputs(outputs: List[TxOut]) -> int:
  sum = 0
  for txo in outputs:
    sum += txo.value.get(b"", {b"": 0}).get(b"", 0) # value is Dict[bytes, Dict[bytes, int]]
  return sum


def get_ada_from_inputs(inputs: List[TxInInfo]) -> int:
  sum = 0
  for txi in inputs:
    sum += txi.resolved.value.get(b"", {b"": 0}).get(b"", 0) # value is Dict[bytes, Dict[bytes, int]]
  return sum

  
def validator(datum: EscrowDatum, redeemer: Redeemer, context: ScriptContext) -> None:
  assert isinstance(context.purpose, Spending), f"Wrong script purpose: {context.purpose}"
  purpose: Spending = context.purpose
  
  tx_info = context.tx_info
  own_input = resolve_spent_utxo(tx_info.inputs, purpose)
  contract_address = own_input.address
  contract_balance = get_ada_from_inputs(get_inputs_by_addr(context, contract_address))
  
  assert contract_balance == datum.seller_deposit + datum.buyer_deposit + datum.price, "Contract balance mismatch"
  
  seller_outputs = get_outputs_by_key(context, datum.seller_pubkeyhash)
  buyer_outputs = get_outputs_by_key(context, datum.buyer_pubkeyhash)
  
  if isinstance(redeemer, Accept):
      # only buyer can trigger accept
      assert datum.buyer_pubkeyhash in tx_info.signatories, "Required signature missing"
      assert datum.nonce == redeemer.nonce, "Nonce mismatch"
      assert get_ada_from_outputs(seller_outputs) == datum.seller_deposit + datum.price, "Seller output mismatch"
      assert get_ada_from_outputs(buyer_outputs) == datum.buyer_deposit, "Buyer output mismatch"
      
  elif isinstance(redeemer, Complaint):
      # both seller and buyer must sign to submit complaint, so both have to agree on the provided values
      assert datum.buyer_pubkeyhash in tx_info.signatories and datum.seller_pubkeyhash in tx_info.signatories, "Required signature missing"
      assert datum.nonce == redeemer.nonce, "Nonce mismatch"
      
      if verify_ed25519_signature(datum.seller_pubkeyhash, (str(redeemer.product_hash) + str(redeemer.nonce)).encode(), redeemer.sig):        
        if datum.product_hash == redeemer.product_hash:
          # buyer tried to cheat, seller gets his deposit and price back, buyer loses his deposit
          assert get_ada_from_outputs(seller_outputs) == datum.seller_deposit + datum.price, "Seller output mismatch"
          assert get_ada_from_outputs(buyer_outputs) == 0, "Buyer output mismatch"
        else:
          # seller tried to cheat, buyer gets his deposit and price back, seller loses his deposit
          assert get_ada_from_outputs(seller_outputs) == 0, "Seller output mismatch"
          assert get_ada_from_outputs(buyer_outputs) == datum.buyer_deposit + datum.price, "Buyer output mismatch"
      else:
        # seller tried to cheat
        assert get_ada_from_outputs(seller_outputs) == 0, "Seller output mismatch"
        assert get_ada_from_outputs(buyer_outputs) == datum.buyer_deposit + datum.price, "Buyer output mismatch" 
        
  else:
    assert False, "Unsupported"
