module Test.Main where

import Prelude

import Data.Foldable                            (Foldable, foldl)
import Data.List                                (List(), toList)
import Test.QuickCheck                          (Result(Success), quickCheck)
import Test.QuickCheck.Arbitrary                ( Arbitrary
                                                , Coarbitrary
                                                , arbitrary
                                                , coarbitrary
                                                )
import Test.QuickCheck.Gen                      (Gen())
import Test.QuickCheck.Laws.Data.Eq             (checkEq)
import Test.QuickCheck.Laws.Data.Ord            (checkOrd)
import Test.QuickCheck.Laws.Data.Functor        (checkFunctor)
import Test.QuickCheck.Laws.Control.Apply       (checkApply)
import Test.QuickCheck.Laws.Control.Applicative (checkApplicative)
import Test.QuickCheck.Laws.Control.Alt         (checkAlt)
import Test.QuickCheck.Laws.Control.Plus        (checkPlus)
import Test.QuickCheck.Laws.Control.Alternative (checkAlternative)
import Type.Proxy                               (Proxy(Proxy), Proxy2(Proxy2))

import Data.Functor.Compose (Compose(Compose), decompose)

type LA = Compose List Array

lap  :: Proxy2 LA
lap  = Proxy2
lanp :: Proxy (LA Number)
lanp = Proxy
np   :: Proxy Number
np   = Proxy
ip   :: Proxy Int
ip   = Proxy
bp   :: Proxy Boolean
bp   = Proxy

main = do
    -- check that show is total
    quickCheck       (const Success $ show :: LA Number -> String)
    checkEq          lanp
    checkOrd         lanp
    checkFunctor     lap np ip
    checkApply       lap np ip bp
    checkApplicative lap np ip bp
    -- TODO: check foldable (no standard laws)
    -- TODO: check traversable (standard laws, not included in purescript-quickcheck-laws)
    checkAlt         lap np ip
    checkPlus        lap np ip
    checkAlternative lap np ip

instance arbList :: (Arbitrary a) => Arbitrary (List a) where
    arbitrary = toList <$> (arbitrary :: Gen (Array a))

instance coarbFoldable :: (Foldable f, Coarbitrary a) => Coarbitrary (f a) where
    coarbitrary = foldl (\f x -> f <<< coarbitrary x) id

instance arbCompose :: (Arbitrary (f (g x))) => Arbitrary ((Compose f g) x) where
    arbitrary = Compose <$> arbitrary

instance coarbCompose :: (Coarbitrary (f (g x))) => Coarbitrary ((Compose f g) x) where
    coarbitrary = coarbitrary <<< decompose
