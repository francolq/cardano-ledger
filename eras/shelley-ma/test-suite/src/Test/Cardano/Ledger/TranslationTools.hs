{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- TODO: Move to cardano-ledger-core:test-lib
module Test.Cardano.Ledger.TranslationTools
  ( translateEraPartial,
    translateEraEncoding,
    translateEraToCBOR,
    decodeTest,
    decodeTestAnn,
    expectDecodeFailure,
  )
where

import Cardano.Ledger.Binary
  ( Annotator,
    DecoderError,
    Encoding,
    FromCBOR (..),
    ToCBOR (..),
    Version,
    decodeAnnotator,
    decodeFull,
    serialize,
    serializeEncoding',
  )
import Cardano.Ledger.Core
import Control.Monad
import Control.Monad.Except (runExcept)
import qualified Data.ByteString.Base16.Lazy as B16
import GHC.Stack
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Binary.TreeDiff
import Test.Tasty.HUnit (Assertion, assertFailure)

translateEraPartial ::
  forall era f.
  (TranslateEra era f, Show (TranslationError era f), HasCallStack) =>
  TranslationContext era ->
  f (PreviousEra era) ->
  f era
translateEraPartial tc fe =
  case runExcept $ translateEra @era tc fe of
    Right result -> result
    Left err -> error $ "TranslateEra failure: " <> show err

-- Tests that the serializing before translation or after translating
-- does not change the result
translateEraEncoding ::
  forall era f.
  ( HasCallStack,
    TranslateEra era f,
    Show (TranslationError era f)
  ) =>
  TranslationContext era ->
  (f era -> Encoding) ->
  (f (PreviousEra era) -> Encoding) ->
  f (PreviousEra era) ->
  Assertion
translateEraEncoding tc encodeThisEra encodePreviousEra x =
  let previousEra = serializeEncoding' (eraProtVerHigh @(PreviousEra era)) (encodePreviousEra x)
      currentEra = serializeEncoding' (eraProtVerLow @era) (encodeThisEra $ translate @era tc x)
   in unless (previousEra == currentEra) $
        assertFailure $
          diffExpr (CBORBytes previousEra) (CBORBytes currentEra)

-- Tests that the serializing before translation or after translating
-- does not change the result
translateEraToCBOR ::
  forall proxy era f.
  ( HasCallStack,
    TranslateEra era f,
    ToCBOR (f era),
    ToCBOR (f (PreviousEra era)),
    Show (TranslationError era f)
  ) =>
  proxy era ->
  TranslationContext era ->
  f (PreviousEra era) ->
  Assertion
translateEraToCBOR _ tc = translateEraEncoding @era tc toCBOR toCBOR

-- Tests that the type a can be decoded as b
decodeTest ::
  forall a b proxy.
  (ToCBOR a, FromCBOR b) =>
  Version ->
  proxy b ->
  a ->
  Assertion
decodeTest version _ = embedTrip version (cborTrip @a @b)

-- Tests that the type a can be decoded as b
decodeTestAnn ::
  forall a b proxy.
  (ToCBOR a, FromCBOR (Annotator b)) =>
  proxy b ->
  a ->
  Assertion
decodeTestAnn _ x =
  let bytes = serialize x
      decoded = decodeAnnotator mempty fromCBOR bytes :: Either DecoderError b
   in case decoded of
        Left e ->
          assertFailure $
            "\nerror: "
              <> show e
              <> "\nbytes: "
              <> show (B16.encode bytes)
              <> "\n"
        Right _ -> return ()

-- Tests that a decoder error happens
expectDecodeFailure :: forall a. (ToCBOR a, FromCBOR a) => a -> Assertion
expectDecodeFailure x = case decodeFull (serialize x) :: Either DecoderError a of
  Left _ -> pure ()
  Right _ -> assertFailure "should not deserialize"
