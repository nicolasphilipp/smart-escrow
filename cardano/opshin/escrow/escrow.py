# escrow.py
from opshin.prelude import *


@dataclass()
class EscrowDatum(PlutusData):
  seller_pubkeyhash: PubKeyHash
  buyer_pubkeyhash: PubKeyHash
  price: int
  seller_deposit: int
  buyer_deposit: int


@dataclass
class Accept(PlutusData):
  CONSTR_ID = 0
  
@dataclass
class Refund(PlutusData):
  CONSTR_ID = 1
  
Redeemer = Union[Accept, Refund]


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
  
  if isinstance(redeemer, Accept):
      # only buyer can trigger accept
      assert datum.buyer_pubkeyhash in tx_info.signatories, "Required signature missing"
      
      seller_outputs = get_outputs_by_key(context, datum.seller_pubkeyhash)
      assert get_ada_from_outputs(seller_outputs) == datum.seller_deposit + datum.price, "Seller output mismatch"

      buyer_outputs = get_outputs_by_key(context, datum.buyer_pubkeyhash)
      assert get_ada_from_outputs(buyer_outputs) == datum.buyer_deposit, "Buyer output mismatch"

  elif isinstance(redeemer, Refund):
      # both seller and buyer must sign to refund
      assert datum.buyer_pubkeyhash in tx_info.signatories and datum.seller_pubkeyhash in tx_info.signatories, "Required signature missing"

      seller_outputs = get_outputs_by_key(context, datum.seller_pubkeyhash)
      assert get_ada_from_outputs(seller_outputs) == datum.seller_deposit, "Seller output mismatch"

      buyer_outputs = get_outputs_by_key(context, datum.buyer_pubkeyhash)
      assert get_ada_from_outputs(buyer_outputs) == datum.buyer_deposit + datum.price, "Buyer output mismatch"

  else:
    assert False, "Unsupported"
