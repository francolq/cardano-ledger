{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Shelley.PropertyTests
  ( propertyTests,
    minimalPropertyTests,
    relevantCasesAreCovered,
    delegProperties,
    poolProperties,
    removedAfterPoolreap,
    adaPreservationChain,
    collisionFreeComplete,
    onlyValidLedgerSignalsAreGenerated,
    onlyValidChainSignalsAreGenerated,
    -- Crypto era only
    propCompactAddrRoundTrip,
    propCompactSerializationAgree,
  )
where

import Cardano.Ledger.BaseTypes (Globals)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (DSignable, Hash)
import Cardano.Ledger.SafeHash (SafeHash)
import Cardano.Ledger.Shelley.API (LedgerState, PPUPState)
import Cardano.Ledger.Shelley.Rules (LedgerEnv)
import Cardano.Ledger.Shelley.Tx (ShelleyTx)
import Cardano.Ledger.UTxO (makeWitnessVKey)
import Control.Monad.Trans.Reader (ReaderT)
import Control.State.Transition
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Functor.Identity (Identity)
import Data.List (nub, sort)
import Data.Set as Set (fromList, singleton)
import Test.Cardano.Ledger.Shelley.Address.Bootstrap
  ( bootstrapHashTest,
  )
import Test.Cardano.Ledger.Shelley.Address.CompactAddr
  ( propCompactAddrRoundTrip,
    propCompactSerializationAgree,
    propDecompactErrors,
    propDeserializeRewardAcntErrors,
    propValidateNewDecompact,
  )
import Test.Cardano.Ledger.Shelley.ByronTranslation (testGroupByronTranslation)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen)
import Test.Cardano.Ledger.Shelley.Rules.Chain (CHAIN)
import Test.Cardano.Ledger.Shelley.Rules.ClassifyTraces
  ( onlyValidChainSignalsAreGenerated,
    onlyValidLedgerSignalsAreGenerated,
    relevantCasesAreCovered,
  )
import Test.Cardano.Ledger.Shelley.Rules.TestChain
  ( TestingLedger,
    adaPreservationChain,
    collisionFreeComplete,
    delegProperties,
    poolProperties,
    removedAfterPoolreap,
    stakeIncrTest,
  )
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators ()
import Test.Cardano.Ledger.Shelley.ShelleyTranslation (testGroupShelleyTranslation)
import Test.Cardano.Ledger.Shelley.Utils (ChainProperty, RawSeed, mkKeyPair')
import Test.QuickCheck (conjoin, (===), (==>))
import Test.Tasty (TestTree, localOption, testGroup)
import qualified Test.Tasty.QuickCheck as TQC

-- =====================================================================

propWitVKeys ::
  forall c.
  (CC.Crypto c, DSignable c (Hash c EraIndependentTxBody)) =>
  RawSeed ->
  SafeHash c EraIndependentTxBody ->
  SafeHash c EraIndependentTxBody ->
  TQC.Property
propWitVKeys seed h1 h2 =
  let kp = mkKeyPair' seed
      w1 = makeWitnessVKey h1 kp
      w2 = makeWitnessVKey h2 kp
   in conjoin
        [ sort [w1, w2] === sort [w2, w1],
          length (nub [w1, w2]) === length (Set.fromList [w1, w2]),
          w1 /= w2 ==> length (Set.singleton w1 <> Set.singleton w2) === 2
        ]

minimalPropertyTests ::
  forall era ledger.
  ( EraGen era,
    Tx era ~ ShelleyTx era,
    TestingLedger era ledger,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    State (EraRule "PPUP" era) ~ PPUPState era
  ) =>
  TestTree
minimalPropertyTests =
  testGroup
    "Minimal Property Tests"
    [ (localOption (TQC.QuickCheckMaxRatio 50) $ TQC.testProperty "Chain and Ledger traces cover the relevant cases" (relevantCasesAreCovered @era)),
      TQC.testProperty "total amount of Ada is preserved (Chain)" (adaPreservationChain @era @ledger),
      TQC.testProperty "Only valid CHAIN STS signals are generated" (onlyValidChainSignalsAreGenerated @era),
      bootstrapHashTest,
      testGroup
        "Compact Address Tests"
        [ TQC.testProperty "Compact address round trip" (propCompactAddrRoundTrip @(EraCrypto era)),
          TQC.testProperty "Compact address binary representation" (propCompactSerializationAgree @(EraCrypto era)),
          TQC.testProperty "Ensure Addr failures on incorrect binary data" $
            propDecompactErrors @(EraCrypto era),
          TQC.testProperty "Ensure RewardAcnt failures on incorrect binary data" $
            propDeserializeRewardAcntErrors @(EraCrypto era),
          TQC.testProperty "Decompacting an address is still valid" $
            propValidateNewDecompact @(EraCrypto era)
        ],
      TQC.testProperty "WitVKey does not brake containers due to invalid Ord" $
        propWitVKeys @(EraCrypto era)
    ]

-- | 'TestTree' of property-based testing properties.
propertyTests ::
  forall era ledger.
  ( EraGen era,
    ChainProperty era,
    QC.HasTrace (CHAIN era) (GenEnv era),
    QC.HasTrace ledger (GenEnv era),
    Embed (EraRule "DELEGS" era) ledger,
    Embed (EraRule "UTXOW" era) ledger,
    State (EraRule "PPUP" era) ~ PPUPState era,
    Environment ledger ~ LedgerEnv era,
    QC.BaseEnv ledger ~ Globals,
    BaseM ledger ~ ReaderT Globals Identity,
    State ledger ~ LedgerState era,
    Signal ledger ~ Tx era
  ) =>
  TestTree
propertyTests =
  testGroup
    "Property-Based Testing"
    [ testGroup
        "Classify Traces"
        [ ( localOption (TQC.QuickCheckMaxRatio 100) $
              TQC.testProperty
                "Chain and Ledger traces cover the relevant cases"
                (relevantCasesAreCovered @era)
          )
        ],
      testGroup
        "STS Rules - Delegation Properties"
        [ TQC.testProperty
            "properties of the DELEG STS"
            (delegProperties @era)
        ],
      testGroup
        "STS Rules - Pool Properties"
        [ TQC.testProperty
            "properties of the POOL STS"
            (poolProperties @era)
        ],
      testGroup
        "STS Rules - Poolreap Properties"
        [ TQC.testProperty
            "pool is removed from stake pool and retiring maps"
            (removedAfterPoolreap @era)
        ],
      testGroup
        "CHAIN level Properties"
        [ TQC.testProperty
            "collection of Ada preservation properties"
            (adaPreservationChain @era @ledger),
          TQC.testProperty
            "inputs are eliminated, outputs added to utxo and TxIds are unique"
            (collisionFreeComplete @era @ledger),
          TQC.testProperty
            "incremental stake calc"
            (stakeIncrTest @era @ledger)
        ],
      testGroup
        "Properties of Trace generators"
        [ TQC.testProperty
            "Only valid LEDGER STS signals are generated"
            (onlyValidLedgerSignalsAreGenerated @era @ledger),
          TQC.testProperty
            "Only valid CHAIN STS signals are generated"
            (onlyValidChainSignalsAreGenerated @era)
        ],
      testGroupByronTranslation,
      testGroupShelleyTranslation,
      testGroup
        "Compact Address Tests"
        [ TQC.testProperty "Compact address round trip" (propCompactAddrRoundTrip @(EraCrypto era)),
          TQC.testProperty "Compact address binary representation" (propCompactSerializationAgree @(EraCrypto era)),
          TQC.testProperty "Ensure failures on incorrect binary data" $
            propDecompactErrors @(EraCrypto era)
        ]
    ]
