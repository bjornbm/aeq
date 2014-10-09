{-# LANGUAGE ForeignFunctionInterface #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Numeric.IEEE
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Operations on IEEE floating point numbers.
--
module Numeric.IEEE (
    -- * IEEE type class
    IEEE(..),
    -- * NaN-aware minimum and maximum
    minNum,
    maxNum,
    minNaN,
    maxNaN,
    ) where

import Data.Word
import Foreign.C.Types( CFloat, CDouble )


-- | IEEE floating point types.
class (RealFloat a) => IEEE a where
    -- | Infinity value.
    infinity :: a

    -- | The smallest representable positive normalized value.
    minNormal :: a

    -- | The largest representable finite value.
    maxFinite :: a

    -- | The smallest representalbe positive value @x@ such that @1 + x /= 1@.
    epsilon :: a

    -- | Default @NaN@ value.
    nan :: a

    -- | Maximum @NaN@ payload for type @a@.
    maxNaNPayload :: a -> Word64

    -- | The number of significand bits which are equal in the two arguments
    -- (equivalent to @feqrel@ from the Tango Math library).  The result is
    -- between @0@ and @'floatDigits'@.
    sameSignificandBits :: a -> a -> Int
    sameSignificandBits x y | isInfinite x && isInfinite y && signum x == signum y = floatDigits x
                            | x == y = floatDigits x
                            | otherwise = max 0 (exponent x - exponent (x - y))

-- | Return the maximum of two values; if one value is @NaN@, return the
-- other.  Prefer the first if both values are @NaN@.
maxNum :: (RealFloat a) => a -> a -> a
maxNum x y | x >= y || isNaN y = x
           | otherwise         = y
{-# INLINE maxNum #-}

-- | Return the minimum of two values; if one value is @NaN@, return the
-- other.  Prefer the first if both values are @NaN@.
minNum :: (RealFloat a) => a -> a -> a
minNum x y | x <= y || isNaN y = x
           | otherwise         = y
{-# INLINE minNum #-}

-- | Return the maximum of two values; if one value is @NaN@, return it.
-- Prefer the first if both values are @NaN@.
maxNaN :: (RealFloat a) => a -> a -> a
maxNaN x y | x >= y || isNaN x = x
           | otherwise         = y
{-# INLINE maxNaN #-}

-- | Return the minimum of two values; if one value is @NaN@, return it.
-- Prefer the first if both values are @NaN@.
minNaN :: (RealFloat a) =>  a -> a -> a
minNaN x y | x <= y || isNaN x = x
           | otherwise         = y
{-# INLINE minNaN #-}


instance IEEE Float where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
    {-# INLINE nan #-}
    maxNaNPayload _ = 0x003FFFFF
    {-# INLINE maxNaNPayload #-}
    minNormal = 1.17549435e-38
    {-# INLINE minNormal #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}
    epsilon = 1.19209290e-07


instance IEEE CFloat where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
    {-# INLINE nan #-}
    maxNaNPayload _ = 0x003FFFFF
    {-# INLINE maxNaNPayload #-}
    minNormal = 1.17549435e-38
    {-# INLINE minNormal #-}
    maxFinite = 3.40282347e+38
    {-# INLINE maxFinite #-}
    epsilon = 1.19209290e-07
    {-# INLINE epsilon #-}


instance IEEE Double where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
    {-# INLINE nan #-}
    maxNaNPayload _ = 0x0007FFFFFFFFFFFF
    {-# INLINE maxNaNPayload #-}
    minNormal = 2.2250738585072014e-308
    {-# INLINE minNormal #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}
    epsilon = 2.2204460492503131e-16
    {-# INLINE epsilon #-}


instance IEEE CDouble where
    infinity = 1/0
    {-# INLINE infinity #-}
    nan = (0/0)
    {-# INLINE nan #-}
    maxNaNPayload _ = 0x0007FFFFFFFFFFFF
    {-# INLINE maxNaNPayload #-}
    minNormal = 2.2250738585072014e-308
    {-# INLINE minNormal #-}
    maxFinite = 1.7976931348623157e+308
    {-# INLINE maxFinite #-}
    epsilon = 2.2204460492503131e-16
    {-# INLINE epsilon #-}
