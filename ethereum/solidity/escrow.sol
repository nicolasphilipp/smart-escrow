// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.8.2 <0.9.0;

import "@openzeppelin/contracts/utils/cryptography/SignatureChecker.sol";
import "@openzeppelin/contracts/utils/cryptography/MessageHashUtils.sol";

/**
 * @title Escrow
 */
contract Escrow {

    address payable public seller;
    address payable public buyer;
    uint public price;
    uint sellerDeposit;
    uint buyerDeposit;
    bytes productHash;
    bytes nonce;

    mapping(address => uint) pendingReturns;

    enum State { Created, Locked, Released, Completed }
    State public state;

    modifier condition(bool condition_) {
        require(condition_);
        _;
    }

    /// Only the buyer can call this function.
    error OnlyBuyer();
    /// Only the seller can call this function.
    error OnlySeller();
    /// The function cannot be called at the current state.
    error InvalidState();
        
    modifier onlyBuyer() {
        if (msg.sender != buyer)
            revert OnlyBuyer();
        _;
    }

    modifier onlySeller() {
        if (msg.sender != seller)
            revert OnlySeller();
        _;
    }

    modifier inState(State _state) {
        if (state != _state)
            revert InvalidState();
        _;
    }

    event Deposited();
    event Accepted();
    event Settled();

    constructor(address payable _seller, address payable _buyer, uint _price, uint _sellerDeposit, uint _buyerDeposit, bytes memory _productHash, bytes memory _nonce) payable
        onlySeller
        condition(_seller != address(0x0) && _buyer != address(0x0))
        condition(msg.value == _sellerDeposit)
    {
        //require(_seller != address(0x0) && _buyer != address(0x0), "");
        //require(msg.sender == _seller, "The creator must be the seller");
        //require(msg.value == _sellerDeposit, "Seller needs to initialize contract with their deposit");
        seller = _seller;
        buyer = _buyer;
        price = _price;
        sellerDeposit = _sellerDeposit;
        buyerDeposit = _buyerDeposit;
        productHash = _productHash;
        nonce = _nonce;
    }

    // only buyer can call deposit, seller deposits upon creation of contract
    function deposit() external payable
        onlyBuyer
        inState(State.Created)
        condition(msg.value == price + buyerDeposit)
    {
        //require(msg.value == price + buyerDeposit, "Invalid amount");
        state = State.Locked;
    }

    function withdraw() external 
        condition(state == State.Released || state == State.Completed)
    {
        //require(state == State.Released || state == State.Completed, "Invalid state");
        uint amount = pendingReturns[msg.sender];
        if (amount > 0) {
            pendingReturns[msg.sender] = 0;
            payable(msg.sender).transfer(amount);
        }
    }

    // only buyer can accept delivery
    function accept() external 
        onlyBuyer 
        inState(State.Locked)
    {
        emit Accepted();
        state = State.Completed;

        // seller can call withdraw to get price and deposit
        pendingReturns[seller] = price + sellerDeposit;
        buyer.transfer(buyerDeposit);
    }

    // needs to be signed by both seller and buyer in multi sig wallet
    function complaint(bytes calldata sig, bytes calldata _productHash, bytes calldata _nonce) external 
        condition(keccak256(abi.encodePacked(nonce)) == keccak256(abi.encodePacked(_nonce)))
    {
        emit Settled();
        state = State.Released;

        if(SignatureChecker.isValidSignatureNow(seller, MessageHashUtils.toEthSignedMessageHash(abi.encodePacked(_productHash, _nonce)), sig)) {
            if(keccak256(abi.encodePacked(productHash)) == keccak256(abi.encodePacked(_productHash))) {
                // buyer tried to cheat, their deposit stays locked
                pendingReturns[seller] = price + sellerDeposit;
            } else {
                // seller tried to cheat, their deposit stays locked
                pendingReturns[buyer] = price + buyerDeposit;
            }
        } else {
            // seller tried to cheat, their deposit stays locked
            pendingReturns[buyer] = price + buyerDeposit;
        }
    }
}
