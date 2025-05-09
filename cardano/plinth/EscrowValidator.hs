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
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

module EscrowValidator where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion100)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, PubKeyHash)
import PlutusLedgerApi.V1.Address (toPubKeyHash)
import PlutusLedgerApi.V1.Interval (contains)
import PlutusLedgerApi.V1.Value (lovelaceValueOf, valueOf)
import PlutusLedgerApi.V2 (CurrencySymbol, Datum (..), OutputDatum (..), ScriptContext (..),
                           TokenName, TxInfo (..), TxOut (..), from, to)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx.AsData qualified as PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Show qualified as PlutusTx
import PlutusTx.List qualified as List

-- TODO fix imports

import PlutusTx.Prelude
import PlutusTx
import Ledger
import Ledger.Contexts
import Ledger.Value
import Prelude (Show)


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
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EscrowDatum [('EscrowDatum, 0)]
PlutusTx.makeLift ''EscrowDatum


data EscrowRedeemer = Accept BuiltinByteString | Complaint BuiltinByteString BuiltinByteString
  deriving (Show)

PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('Accept, 0), ('Complaint, 1)]
PlutusTx.makeLift ''EscrowRedeemer


{-# INLINEABLE escrowValidator #-}
escrowValidator :: EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
escrowValidator datum redeemer ctx = List.and conditions
  where
    conditions :: [Bool]
    conditions = case redeemer of
      Accept nonce ->
        [ 
          signedByBuyer,
          totalMatches,
          nonceMatches nonce,
          sellerMustBeCompensated,
          buyerMustGetDeposit
        ]
      Complaint product nonce ->
        [
          signedBySellerAndBuyer,
          totalMatches,
          nonceMatches nonce,
          sellerAndBuyerMustBeCompensated product
        ]

    totalMatches :: Bool
    -- TODO get own_input and check if total matches that of datum

    signedByBuyer :: Bool
    -- TODO

    signedBySellerAndBuyer :: Bool
    -- TODO

    nonceMatches :: BuiltinByteString -> Bool
    -- TODO

    sellerMustBeCompensated :: Bool
    -- TODO seller must get price and his deposit

    buyerMustGetDeposit :: Bool
    -- TODO buyer must get only his deposit

    sellerAndBuyerMustBeCompensated :: BuiltinByteString -> Bool
    -- TODO check hash with blake2b_256 function


