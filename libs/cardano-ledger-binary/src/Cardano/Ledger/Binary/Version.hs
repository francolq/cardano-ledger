{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Cardano.Ledger.Binary.Version
  ( -- * Versioning
    Version,
    MinVersion,
    MaxVersion,
    natVersion,
    natVersionProxy,
    mkVersion,
    mkVersion64,
    allVersions,

    -- ** Concrete era versions
    byronProtVer,
    shelleyProtVer,
  )
where

import Control.DeepSeq (NFData)
import Data.Proxy (Proxy (..))
import Data.Word (Word64)
import GHC.TypeLits (KnownNat, natVal, type (<=))
import NoThunks.Class (NoThunks)
#if __GLASGOW_HASKELL__ < 900
-- This import is redundant wih ghc-9.2.
import Numeric.Natural (Natural)
#endif

--------------------------------------------------------------------------------
-- Version
--------------------------------------------------------------------------------

-- | Protocol version number that is used during encoding and decoding. All supported
-- versions are in the range from `MinVersion` to `MaxVersion`.
newtype Version = Version Word64
  deriving (Eq, Ord, Show, Enum, NFData, NoThunks)

-- | Minimum supported version
type MinVersion = 1

-- | Maximum supported version
type MaxVersion = 8

instance Bounded Version where
  minBound = Version (fromInteger (natVal (Proxy @MinVersion)))
  maxBound = Version (fromInteger (natVal (Proxy @MaxVersion)))

-- | Same as `natVersionProxy`, construct a version from a type level `Nat`, except it can be
-- supplied through @TypeApplications@.
natVersion :: forall v. (KnownNat v, MinVersion <= v, v <= MaxVersion) => Version
natVersion = natVersionProxy (Proxy @v)

-- | Safely construct a `Version` from a type level `Nat`, which is supplied as a `Proxy`
natVersionProxy :: (KnownNat v, MinVersion <= v, v <= MaxVersion) => Proxy v -> Version
natVersionProxy = Version . fromInteger . natVal

-- | Construct a `Version` and fail if the supplied value is not supported version number.
mkVersion :: MonadFail m => Natural -> m Version
mkVersion v
  | v <= fromIntegral (maxBound :: Word64) = mkVersion64 (fromIntegral v)
  | otherwise = fail $ "Decoder version is too big: " ++ show v

-- | Construct a `Version` and fail if the supplied value is not supported version number.
mkVersion64 :: MonadFail m => Word64 -> m Version
mkVersion64 v
  | minVersion <= v && v <= maxVersion =
      pure (Version (fromIntegral v))
  | otherwise =
      fail $
        "Unsupported decoder version: "
          ++ show v
          ++ ". Expected value in bounds: ["
          ++ show minVersion
          ++ ", "
          ++ show maxVersion
          ++ "]"
  where
    Version minVersion = minBound
    Version maxVersion = maxBound

allVersions :: [Version]
allVersions = [minBound .. maxBound]

byronProtVer :: Version
byronProtVer = natVersion @1

shelleyProtVer :: Version
shelleyProtVer = natVersion @2
