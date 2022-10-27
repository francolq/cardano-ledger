{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.ShelleyMA.Serialisation.Timelocks
  ( timelockTests,
    MultiSig,
  )
where

import Cardano.Binary (Annotator (..), FromCBOR (..), FullByteString (Full), ToCBOR (..), decodeFull)
import Cardano.Ledger.MemoBytes (MemoBytes (Memo))
import Cardano.Ledger.Shelley.Scripts (MultiSig, getMultiSigBytes)
import Cardano.Ledger.ShelleyMA.Timelocks
  ( Timelock (..),
    showTimelock,
    pattern TimelockConstr,
  )
import Cardano.Slotting.Slot (SlotNo (..))
import qualified Data.ByteString.Lazy as Lazy
import Data.Roundtrip (embedTripAnn, roundTripAnn)
import Data.Sequence.Strict (fromList)
import Test.Cardano.Ledger.EraBuffet (ShelleyEra, TestCrypto)
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.ShelleyMA.Serialisation.Generators ()
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- ================================================================

s1 :: Timelock (ShelleyEra TestCrypto)
s1 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire 18])

s2 :: Timelock (ShelleyEra TestCrypto)
s2 = RequireAllOf (fromList [RequireTimeStart (SlotNo 12), RequireTimeExpire (SlotNo 23)])

s4 :: Timelock (ShelleyEra TestCrypto)
s4 = RequireAllOf (fromList [s1, s2])

-- ================================================================

checkOne :: (FromCBOR (Annotator t), ToCBOR t, Show t) => String -> t -> TestTree
checkOne nm t = testProperty ("RoundTrip: " ++ nm) $
  case roundTripAnn t of
    Right _ -> True
    Left s -> error ("Fail to roundtrip " ++ show t ++ " with error " ++ show s)

checkAnn :: Timelock (ShelleyEra TestCrypto) -> Bool
checkAnn t =
  case roundTripAnn t of
    Right _ -> True
    Left s -> error (show s)

checkEmbed :: MultiSig (ShelleyEra TestCrypto) -> Bool
checkEmbed multi =
  case embedTripAnn @(Timelock (ShelleyEra TestCrypto)) multi of
    Right (left, _) | left == Lazy.empty -> True
    Right (left, _) -> error ("Bytes left over: " ++ show left)
    Left s -> error (show s)

checkTranslate :: MultiSig (ShelleyEra TestCrypto) -> Bool
checkTranslate multi = bytes == bytes2
  where
    bytes = getMultiSigBytes multi
    (TimelockConstr (Memo _ bytes2)) = translateMultiSig multi

timelockTests :: TestTree
timelockTests =
  testGroup
    "Timelock tests"
    [ checkOne ("s1 " ++ showTimelock s1) s1,
      checkOne ("s2 " ++ showTimelock s2) s2,
      checkOne ("s4 " ++ showTimelock s4) s4,
      testProperty "roundtripTimelock" checkAnn,
      testProperty "MultiSig deserialises as Timelock" checkEmbed,
      testProperty "Translate preserves bytes" checkTranslate
    ]


-- | We translate a MultiSig by deserializing its bytes as a Timelock If this succeeds
-- (and it should, we designed Timelock to have that property), then both versions should
-- have the same bytes, because we are using FromCBOR(Annotator Timelock) instance.
translateMultiSig :: forall era. (HasCallStack, Era era) => MultiSig era -> Timelock era
translateMultiSig multi =
  case decodeFull (eraProtVerLow @era) (fromShort (getMultiSigBytes multi)) of
    Left err ->
      error $ "Translating MultiSig script to Timelock script fails\n" ++ show err
    Right (Annotator f) -> f (Full bytes)
