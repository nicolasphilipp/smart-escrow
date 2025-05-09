import { Address, PScriptContext, ScriptType, Credential, Script, compile, pfn, unit, plet, pmatch, perror, PMaybe, data, pBool, passert, pstruct, PPubKeyHash, int, punsafeConvertType, bs, padd, PTxInfo, PTxOut, pfilter, list, pInt, pfoldl, PAddress, PTxInInfo, pfind, PTxOutRef, pBs } from "@harmoniclabs/plu-ts";
import keccak256 from 'keccak256';
const EscrowDatum = pstruct({
    EscrowDatum: {
        seller: PPubKeyHash.type,
        buyer: PPubKeyHash.type,
        price: int,
        sellerDeposit: int,
        buyerDeposit: int,
        productHash: bs,
        nonce: bs
    }
});
const EscrowRedeemer = pstruct({
    EscrowRedeemer: {
        r_nonce: bs,
        r_product: bs
    }
});
const get_ada_from_outputs = pfn([list(PTxOut.type)], int)((outputs) => {
    // iterate through outputs and sum up values
    return pfoldl(PTxOut.type, int).$((acc, out) => {
        const amount = out.value.amountOf("", ""); // first argument is policy, second is tokenName, for ADA its ""
        return acc.add(amount);
    }).$(pInt(0)).$(outputs);
});
const get_ada_from_inputs = pfn([list(PTxInInfo.type)], int)((inputs) => {
    // iterate through inputs and sum up values
    return pfoldl(PTxInInfo.type, int).$((acc, out) => {
        const amount = out.resolved.value.amountOf("", ""); // first argument is policy, second is tokenName, for ADA its ""
        return acc.add(amount);
    }).$(pInt(0)).$(inputs);
});
const get_outputs_by_key = pfn([PTxInfo.type, PPubKeyHash.type], list(PTxOut.type))((tx, key) => {
    // filter outputs whose address.credential.pkh equals the provided key
    return pfilter(PTxOut.type).$((output) => {
        const convOut = plet(punsafeConvertType(output, PTxOut.type));
        return pmatch(convOut.address.credential)
            .onPPubKeyCredential(pubkey => pubkey.pkh.eq(key))
            .onPScriptCredential(_ => pBool(false));
    }).$(tx.outputs);
});
const get_inputs_by_addr = pfn([PTxInfo.type, PAddress.type], list(PTxInInfo.type))((tx, addr) => {
    // filter inputs whose address equals the provided address
    return pfilter(PTxInInfo.type).$((input) => {
        const convIn = plet(punsafeConvertType(input, PTxInInfo.type));
        return pmatch(convIn.resolved.address)
            .onPAddress(address => address.credential.eq(addr.credential).and(address.stakingCredential.eq(addr.stakingCredential)));
    }).$(tx.inputs);
});
const contract = pfn([
    PScriptContext.type
], unit)(({ redeemer, tx, purpose }) => {
    const maybeDatum = plet(pmatch(purpose)
        .onSpending(({ datum }) => datum)
        ._(_ => perror(PMaybe(data).type)));
    const datum = plet(punsafeConvertType(maybeDatum.unwrap, EscrowDatum.type));
    const maybeUtxo = plet(pmatch(purpose)
        .onSpending(({ utxoRef }) => utxoRef)
        ._(_ => perror(PTxOutRef.type)));
    const utxoRef = plet(punsafeConvertType(maybeUtxo, PTxOutRef.type));
    const convRedeemer = plet(punsafeConvertType(redeemer, EscrowRedeemer.type));
    const signedBySeller = tx.signatories.some(datum.seller.eq);
    const signedByBuyer = tx.signatories.some(datum.buyer.eq);
    const ownInput = plet(pfind(PTxInInfo.type).$((input) => {
        const convIn = plet(punsafeConvertType(input, PTxInInfo.type));
        return convIn.utxoRef.eq(utxoRef);
    }).$(tx.inputs));
    const ownInputUnwrapped = plet(punsafeConvertType(ownInput.unwrap, PTxInInfo.type));
    const contract_balance = get_ada_from_inputs.$(get_inputs_by_addr.$(tx).$(ownInputUnwrapped.resolved.address));
    const must_match_total = contract_balance == padd.$(datum.sellerDeposit).$(datum.buyerDeposit).padd.$(datum.price);
    const seller_outputs = get_outputs_by_key.$(tx).$(datum.seller);
    const buyer_outputs = get_outputs_by_key.$(tx).$(datum.buyer);
    const must_match_nonce = datum.nonce == convRedeemer.r_nonce;
    if (!convRedeemer.r_product) {
        // accept because no r_product provided in redeemer
        const seller_must_be_compensated = get_ada_from_outputs.$(seller_outputs) == padd.$(datum.sellerDeposit).$(datum.price);
        const buyer_must_get_deposit = get_ada_from_outputs.$(buyer_outputs) == datum.buyerDeposit;
        return passert.$(signedByBuyer.and(must_match_nonce).and(must_match_total).and(seller_must_be_compensated).and(buyer_must_get_deposit));
    }
    else {
        // complaint because r_product is present in redeemer
        if (pBs(keccak256(convRedeemer.r_product.toString()).toString('hex')).eq(datum.productHash)) {
            // buyer tried to cheat
            const seller_must_get_deposit = get_ada_from_outputs.$(seller_outputs) == padd.$(datum.sellerDeposit).$(datum.price);
            const buyer_must_get_nothing = get_ada_from_outputs.$(buyer_outputs) == pInt(0);
            return passert.$(signedBySeller.and(signedByBuyer).and(must_match_nonce).and(must_match_total).and(seller_must_get_deposit).and(buyer_must_get_nothing));
        }
        else {
            // seller tried to cheat
            const seller_must_get_nothing = get_ada_from_outputs.$(seller_outputs) == pInt(0);
            const buyer_must_get_deposit = get_ada_from_outputs.$(buyer_outputs) == padd.$(datum.buyerDeposit).$(datum.price);
            return passert.$(signedBySeller.and(signedByBuyer).and(must_match_nonce).and(must_match_total).and(seller_must_get_nothing).and(buyer_must_get_deposit));
        }
    }
});
export const compiledContract = compile(contract);
export const script = new Script(ScriptType.PlutusV3, compiledContract);
export const scriptTestnetAddr = new Address("testnet", Credential.script(script.hash));
