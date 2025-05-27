{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           System.Environment          (getArgs)
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import           PlutusTx.Blueprint
import           EscrowValidator

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "escrow-validator"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @[EscrowDatum, EscrowRedeemer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Escrow Validator"
    , preambleDescription =
        Just "Blueprint for an Escrow Plutus script"
    , preambleVersion = "1.1.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Escrow Validator"
    , validatorDescription =
        Just "Plutus script validating escrow transactions"
    , validatorParameters = []
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the escrow validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @EscrowRedeemer
          }
    , validatorDatum = 
        Just $ MkArgumentBlueprint 
          { argumentTitle = Just "Datum"
          , argumentDescription = Just "Datum for the escrow validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @EscrowDatum
          }
    , validatorCompiled = do
        let code = Short.fromShort (serialiseCompiledCode escrowValidatorScript)
        Just (compiledValidator PlutusV3 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)