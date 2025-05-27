{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
--{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module EscrowValidator where

import GHC.Generics (Generic)

import PlutusLedgerApi.V1 (Lovelace, PubKeyHash, BuiltinByteString, lovelaceValueOf)
import PlutusLedgerApi.V3 (ScriptContext, TxInfo, TxOut, txInInfoResolved, txOutValue, unsafeFromBuiltinData, toBuiltinData)
import PlutusLedgerApi.Data.V3 qualified as V3Data
import PlutusLedgerApi.V3.Contexts (findOwnInput, scriptContextTxInfo, txInfoSignatories, valuePaidTo)

import PlutusTx (CompiledCode, compile, makeIsDataSchemaIndexed, makeLift)
import PlutusTx.Builtins.Internal (blake2b_256)
import PlutusTx.Blueprint (HasBlueprintDefinition, definitionRef)
import PlutusTx.Prelude (Bool(..), BuiltinUnit, Maybe(Just, Nothing), check, elem, traceError, traceIfFalse, ($), (&&), (==), (+))


data EscrowDatum = EscrowDatum
  {
    seller :: PubKeyHash,
    buyer :: PubKeyHash,
    price :: Lovelace,
    sellerDeposit :: Lovelace,
    buyerDeposit :: Lovelace,
    productHash :: BuiltinByteString,
    nonce :: BuiltinByteString
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''EscrowDatum [('EscrowDatum, 0)]
makeLift ''EscrowDatum


data EscrowRedeemer = Accept BuiltinByteString | Complaint BuiltinByteString BuiltinByteString
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''EscrowRedeemer [('Accept, 0), ('Complaint, 1)]
makeLift ''EscrowRedeemer


{-# INLINEABLE escrowTypedValidator #-}

escrowTypedValidator :: EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
escrowTypedValidator datum redeemer ctx =
  case redeemer of
      Accept r_nonce -> traceIfFalse "Must be signed by buyer" signedByBuyer &&
                      traceIfFalse "Total mismatch" totalMatches &&
                      traceIfFalse "Nonce mismatch" (nonceMatches r_nonce) &&
                      traceIfFalse "Seller must get price and deposit" sellerMustBeCompensated &&
                      traceIfFalse "Buyer must get deposit" buyerMustGetDeposit

      Complaint r_product r_nonce -> traceIfFalse "Must be signed by buyer" signedByBuyer &&
                                 traceIfFalse "Must be signed by seller" signedBySeller &&
                                 traceIfFalse "Total mismatch" totalMatches &&
                                 traceIfFalse "Nonce mismatch" (nonceMatches r_nonce) &&
                                 traceIfFalse "Seller buyer output mismatch" (sellerAndBuyerMustBeCompensated r_product)
  where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      ownInput :: TxOut
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "ownInput missing"
        Just i -> txInInfoResolved i

      ownInputValue :: Lovelace
      ownInputValue = lovelaceValueOf (txOutValue ownInput)

      totalMatches :: Bool
      totalMatches = ownInputValue == price datum + sellerDeposit datum + buyerDeposit datum

      nonceMatches :: BuiltinByteString -> Bool
      nonceMatches pNonce = pNonce == nonce datum

      signedByBuyer :: Bool
      signedByBuyer = buyer datum `elem` txInfoSignatories info

      signedBySeller :: Bool
      signedBySeller = seller datum `elem` txInfoSignatories info

      valueToSeller :: Lovelace
      valueToSeller = lovelaceValueOf (valuePaidTo info (seller datum))

      valueToBuyer :: Lovelace
      valueToBuyer = lovelaceValueOf (valuePaidTo info (buyer datum))

      sellerMustBeCompensated :: Bool
      sellerMustBeCompensated = valueToSeller == price datum + sellerDeposit datum

      buyerMustGetDeposit :: Bool
      buyerMustGetDeposit = valueToBuyer == buyerDeposit datum

      sellerAndBuyerMustBeCompensated :: BuiltinByteString -> Bool
      sellerAndBuyerMustBeCompensated product =
          if blake2b_256 product == productHash datum
            then
              valueToSeller == sellerDeposit datum + price datum && valueToBuyer == 0
            else
              valueToSeller == 0 && valueToBuyer == buyerDeposit datum + price datum


escrowUntypedValidator :: V3Data.ScriptContext -> BuiltinUnit
escrowUntypedValidator ctx =
  check (escrowTypedValidator getDatum (getRedeemer ctx) (unsafeFromBuiltinData $ toBuiltinData ctx))
  where
    getDatum :: EscrowDatum
    getDatum =
      case V3Data.scriptContextScriptInfo ctx of
        V3Data.SpendingScript _TxOutRef (Just (V3Data.Datum datum)) -> unsafeFromBuiltinData datum
        _ -> traceError "No datum found"

    getRedeemer :: V3Data.ScriptContext -> EscrowRedeemer
    getRedeemer V3Data.ScriptContext {V3Data.scriptContextRedeemer = V3Data.Redeemer redeemer} =
      unsafeFromBuiltinData redeemer


{-# INLINEABLE escrowUntypedValidator #-}

escrowValidatorScript :: CompiledCode (V3Data.ScriptContext -> BuiltinUnit)
escrowValidatorScript =
  $$(compile [||escrowUntypedValidator||])