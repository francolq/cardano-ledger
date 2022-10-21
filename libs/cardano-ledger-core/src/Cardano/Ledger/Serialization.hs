{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Cardano.Ledger.Serialization
  {-# DEPRECATED "Use `Cardano.Ledger.Binary` from 'cardano-ledger-binary' package instead" #-}
  ( ToCBORGroup (..),
    FromCBORGroup (..),
    CBORGroup (..),
    decodeList,
    decodeSeq,
    decodeStrictSeq,
    decodeSet,
    decodeMap,
    decodeMapContents,
    decodeMapTraverse,
    decodeMaybe,
    decodeRecordNamed,
    decodeRecordNamedT,
    decodeRecordSum,
    decodeNullMaybe,
    encodeFoldable,
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
    encodeNullMaybe,
    encodeMap,
    groupRecord,
    ratioToCBOR,
    ratioFromCBOR,
    mapToCBOR,
    mapFromCBOR,
    translateViaCBORAnnotator,
    -- IPv4
    ipv4ToBytes,
    ipv4FromBytes,
    ipv4ToCBOR,
    ipv4FromCBOR,
    -- IPv6
    ipv6ToBytes,
    ipv6FromBytes,
    ipv6ToCBOR,
    ipv6FromCBOR,
    -- Raw
    listLenInt,
    runByteBuilder,
    -- UTC Time
    utcTimeToCBOR,
    utcTimeFromCBOR,
    -- This abstraction can/should be moved into cardano-binary
    Sized (..),
    mkSized,
    sizedDecoder,
    toSizedL,
  )
where

import Cardano.Ledger.Binary
  ( CBORGroup (..),
    Decoder,
    DecoderError (..),
    Encoding,
    FromCBOR (..),
    FromCBORGroup (..),
    Sized (..),
    ToCBOR (..),
    ToCBORGroup (..),
    cborError,
    decodeFraction,
    decodeIPv4,
    decodeIPv6,
    decodeList,
    decodeMap,
    decodeMapContents,
    decodeMapTraverse,
    decodeNullMaybe,
    decodeSeq,
    decodeSet,
    decodeStrictSeq,
    encodeFoldableEncoder,
    encodeFoldableMapEncoder,
    encodeIPv4,
    encodeIPv6,
    encodeListLen,
    encodeMap,
    encodeNullMaybe,
    encodeTag,
    enforceVersionDecoder,
    groupRecord,
    ipv4ToBytes,
    ipv6ToBytes,
    listLenInt,
    mkSized,
    natVersion,
    runByteBuilder,
    sizedDecoder,
    toSizedL,
    translateViaCBORAnnotator,
  )
import Cardano.Ledger.Binary.Coders
  ( decodeRecordNamed,
    decodeRecordNamedT,
    decodeRecordSum,
  )
import Data.Binary.Get (Get, getWord32le, runGetOrFail)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.IP
  ( IPv4,
    IPv6,
    fromHostAddress,
    fromHostAddress6,
  )
import Data.Map.Strict (Map)
import Data.Ratio (Ratio, denominator, numerator)
import Data.Time (UTCTime (..))
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate, toOrdinalDate)
import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import Network.Socket (HostAddress6)

mapToCBOR :: (ToCBOR a, ToCBOR b) => Map a b -> Encoding
mapToCBOR = encodeMap toCBOR toCBOR

mapFromCBOR :: (Ord a, FromCBOR a, FromCBOR b) => Decoder s (Map a b)
mapFromCBOR = decodeMap fromCBOR fromCBOR

decodeMaybe :: Decoder s a -> Decoder s (Maybe a)
decodeMaybe d =
  decodeList d >>= \case
    [] -> pure Nothing
    [x] -> pure $ Just x
    _ ->
      cborError $
        DecoderErrorCustom
          "Maybe"
          "Expected an array of length 0 or 1"

ratioToCBOR :: ToCBOR a => Ratio a -> Encoding
ratioToCBOR r =
  encodeTag 30
    <> encodeListLen 2
    <> toCBOR (numerator r)
    <> toCBOR (denominator r)

ratioFromCBOR :: (Integral a, FromCBOR a) => Decoder s (Ratio a)
ratioFromCBOR = decodeFraction fromCBOR

ipv4FromBytes :: BS.ByteString -> Either String IPv4
ipv4FromBytes b =
  case runGetOrFail getWord32le (BSL.fromStrict b) of
    Left (_, _, err) -> Left err
    Right (_, _, ha) -> Right $ fromHostAddress ha

ipv4ToCBOR :: IPv4 -> Encoding
ipv4ToCBOR = encodeIPv4

ipv4FromCBOR :: Decoder s IPv4
ipv4FromCBOR = enforceVersionDecoder (natVersion @2) decodeIPv4

getHostAddress6 :: Get HostAddress6
getHostAddress6 = do
  w1 <- getWord32le
  w2 <- getWord32le
  w3 <- getWord32le
  w4 <- getWord32le
  return (w1, w2, w3, w4)

ipv6FromBytes :: BS.ByteString -> Either String IPv6
ipv6FromBytes b =
  case runGetOrFail getHostAddress6 (BSL.fromStrict b) of
    Left (_, _, err) -> Left err
    Right (_, _, ha) -> Right $ fromHostAddress6 ha

ipv6ToCBOR :: IPv6 -> Encoding
ipv6ToCBOR = encodeIPv6

ipv6FromCBOR :: Decoder s IPv6
ipv6FromCBOR = enforceVersionDecoder (natVersion @2) decodeIPv6

--
-- Raw serialisation
--

utcTimeToCBOR :: UTCTime -> Encoding
utcTimeToCBOR t =
  encodeListLen 3
    <> toCBOR year
    <> toCBOR dayOfYear
    <> (toCBOR . diffTimeToPicoseconds . utctDayTime) t
  where
    (year, dayOfYear) = toOrdinalDate . utctDay $ t

utcTimeFromCBOR :: Decoder s UTCTime
utcTimeFromCBOR = do
  decodeRecordNamed "UTCTime" (const 3) $ do
    year <- fromCBOR
    dayOfYear <- fromCBOR
    diff <- fromCBOR
    pure $
      UTCTime
        (fromOrdinalDate year dayOfYear)
        (picosecondsToDiffTime diff)

encodeFoldable :: (ToCBOR a, Foldable f) => f a -> Encoding
encodeFoldable = encodeFoldableEncoder toCBOR
