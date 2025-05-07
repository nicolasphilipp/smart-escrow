#pragma version >0.3.10

enum State:
    CREATED
    LOCKED
    RELEASED
    COMPLETED 

seller: public(address)
buyer: public(address)
price: public(uint256)
seller_deposit: uint256 
buyer_deposit: uint256 
product_hash: bytes32
nonce: bytes32

pending_returns: HashMap[address, uint256]
state: public(State)

event Deposited:
    pass

event Accepted:
    pass

event Settled:
    pass


@deploy
@payable
def __init__(_seller: address, _buyer: address, _price: uint256, _seller_deposit: uint256, _buyer_deposit: uint256, _product_hash: bytes32, _nonce: bytes32):
    assert _seller != empty(address) and _buyer != empty(address), "Seller or buyer is the zero address"
    assert msg.sender == _seller, "The creator must be the seller"
    assert msg.value == _seller_deposit, "Seller needs to initialize contract with their deposit"
    
    self.seller = _seller
    self.buyer = _buyer
    self.price = _price
    self.seller_deposit = _seller_deposit
    self.buyer_deposit = _buyer_deposit
    self.product_hash = _product_hash
    self.nonce = _nonce
    self.state = State.CREATED


@external
@payable
def deposit():
    assert self.state == State.CREATED, "Invalid state"
    assert msg.sender == self.buyer, "The caller must be the buyer"
    assert msg.value == self.price + self.buyer_deposit, "Invalid amount"

    log Deposited()
    self.state = State.LOCKED


@external
def withdraw():
    assert self.state == State.RELEASED or self.state == State.COMPLETED, "Invalid state"
     
    amount: uint256 = self.pending_returns[msg.sender]
    if (amount > 0):
        self.pending_returns[msg.sender] = 0
        send(msg.sender, amount)


@external
def accept():
    assert self.state == State.LOCKED, "Invalid state"
    assert msg.sender == self.buyer, "The caller must be the buyer"

    log Accepted()
    self.state = State.COMPLETED

    # seller can call withdraw to get price and deposit
    self.pending_returns[self.seller] = self.price + self.seller_deposit
    send(msg.sender, self.buyer_deposit)


# needs to be signed by both seller and buyer in multi sig wallet
@external
def complaint(_product: bytes32, _nonce: bytes32):
    assert self.state == State.LOCKED, "Invalid state"
    assert keccak256(self.nonce) == keccak256(_nonce), "Nonce mismatch"

    log Settled()
    self.state = State.RELEASED

    if keccak256(_product) == self.product_hash:
        # buyer tried to cheat, their deposit stays locked
        self.pending_returns[self.seller] = self.price + self.seller_deposit
    else:
        # seller tried to cheat, their deposit stays locked
        self.pending_returns[self.buyer] = self.price + self.buyer_deposit
