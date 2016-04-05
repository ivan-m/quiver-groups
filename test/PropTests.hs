{-# LANGUAGE RankNTypes #-}
{- |
   Module      : Main
   Description : Properties for quiver-groups
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import Control.Quiver.Group

import Control.Applicative   (liftA2)
import Control.Quiver.SP
import Data.Functor.Identity
import Data.List             (group)

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

--------------------------------------------------------------------------------

main :: IO ()
main = hspec $ do
  describe "spgroup" $ do
    prop "keeps elements" $
      propInts ((==) <*> concat . spIdentityList spgroup)
    prop "same as list-based" $
      propInts (liftA2 (==) group (spIdentityList spgroup))

  describe "spchunks" $ do
    prop "keeps elements" $
      propInts $ \as (Positive n) ->
        concat (spIdentityList (spchunks n) as) == as
    prop "size == 0" $
      propInts (null . spIdentityList (spchunks 0))
    prop "size == 1" $
      propInts (all isSingleton . spIdentityList (spchunks 1))
    prop "size > 1" $
      propInts $ \as (GTOne n) -> initLast (==n) (<=n)
                                  . map length
                                  $ spIdentityList (spchunks n) as

spToList :: SQ a x f [a]
spToList = spfoldr (:) []

spIdentity :: SQ a b Identity c -> c
spIdentity = runIdentity . sprun

spIdentityList :: SQ a b Identity e -> [a] -> [b]
spIdentityList p as = spIdentity (spevery as >->> p >->> spToList >&> snd)

propInts :: (Testable prop) => ([Int] -> prop) -> Property
propInts = forAllShrink arbitrary shrink

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False

initLast :: (a -> Bool) -> (a -> Bool) -> [a] -> Bool
initLast i l = go
  where
    go []     = True
    go [a]    = l a
    go (a:as) = i a && go as

newtype GTOne a = GTOne a
                deriving (Eq, Ord, Show, Read)

instance Functor GTOne where
  fmap f (GTOne a) = GTOne (f a)

instance (Num a, Ord a, Arbitrary a) => Arbitrary (GTOne a) where
  arbitrary = GTOne <$> ((abs <$> arbitrary) `suchThat` (>1))

  shrink (GTOne a) = [ GTOne a' | a' <- shrink a, a' > 1 ]
