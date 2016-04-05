{-# LANGUAGE RankNTypes #-}
{- |
   Module      : Control.Quiver.Group
   Description : Group and chunk values within a Quiver
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Control.Quiver.Group where

import Control.Quiver.SP

import           Control.Arrow (second)
import qualified Data.DList    as D

--------------------------------------------------------------------------------

-- | Accumulate values within a Quiver.
spaccum :: (a -> p)
              -- ^ Create the initial partial accumulation @p@.
           -> (p -> a -> Either p (g, Maybe a))
              -- ^ Attempt to add a new value to a partial
              -- accumulation; returns either an updated partial
              -- accumulation or else a completed accumulation @g@ as
              -- well as optionally the initial value (if it was /not/
              -- added to the completed accumulation).
           -> (p -> Maybe g)
              -- ^ Attempt to convert the final partial accumulation
              -- to a complete accumulation.  If this function returns
              -- @'Nothing'@ then the final partial accumulation is
              -- returned using 'spfailed'.
           -> SP a g f p
spaccum mkInit addA finalise = createNewAccum
  where
    createNewAccum = spconsume newAccumFrom spcomplete

    newAccumFrom = accumPartial . mkInit

    accumPartial p = spconsume (withValue . addA p) (finalisePartial p)

    withValue epa = case epa of
                      Left p        -> accumPartial p
                      Right (g,ma') -> produce g
                                               (const (maybe createNewAccum newAccumFrom ma'))
                                               spincomplete

    finalisePartial p = maybe (spfailed p) spemit (finalise p)

-- | As with 'spaccum' but the finalisation function always succeeds
spaccum' :: (Functor f) => (a -> p) -> (p -> a -> Either p (g, Maybe a)) -> (p -> g) -> SP a g f ()
spaccum' mkInit addA finalise = spaccum mkInit addA (Just . finalise) >&> fmap (fmap (const ()))
{-# ANN spaccum' "HLint: ignore Use void" #-}
-- Don't want to use 'void' to make sure the 'SPResult' is maintained.

--------------------------------------------------------------------------------

-- | Collect consecutive equal elements together.
spgroup :: (Eq a, Functor f) => SP a [a] f ()
spgroup = spgroupBy (==)

-- | Collect consecutive elements together that satisfy the provided
-- function.
spgroupBy :: (Functor f) => (a -> a -> Bool) -> SP a [a] f ()
spgroupBy f = spaccum' mkInit addA finalise
  where
    mkInit a = (a, D.singleton a)

    addA p@(a, d) a'
      | f a a'    = Left (second (`D.snoc` a') p)
      | otherwise = Right (D.toList d, Just a')

    finalise = D.toList . snd

--------------------------------------------------------------------------------

-- | Collect the elements into lists of the specified size (though the
-- last such may be shorter).
--
-- A size that is @<= 0@ will return 'spcomplete' (that is, no outputs
-- will be produced).
spchunks :: (Functor f) => Int -> SP a [a] f ()
spchunks n
  | n <= 0    = spcomplete
  | n == 1    = sppure (:[]) -- Required for the INVARIANT below to be correct
  | otherwise = spaccum' mkInit addA finalise
  where
    mkInit a = (n', D.singleton a)
    n' = pred n -- n' is >= 1

    -- INVARIANT: c >= 1
    addA (c,d) a
      | c' <= 0   = Right (D.toList d', Nothing)
      | otherwise = Left (c', d')
      where
        c' = pred c
        d' = d `D.snoc` a

    finalise = D.toList . snd
