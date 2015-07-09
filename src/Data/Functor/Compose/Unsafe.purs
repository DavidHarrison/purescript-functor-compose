-- | Unsafe instances for composed typeclasses where not all conditions for
-- | for the instance are fulfilled by the constraints.
module Data.Functor.Compose.Unsafe where

import Prelude

import Control.Extend       (Extend, duplicate, extend)
import Control.Comonad      (Comonad, extract)
import Data.Traversable     (Traversable, sequence, traverse)
import Data.Functor.Compose (Compose(..), decompose)

-- | See http://stackoverflow.com/questions/12963733/writing-cojoin-or-cobind-for-n-dimensional-grid-type for a discussion of the instance.
-- | To hold for all comonad laws, the structure of g must be regular, so that no values are dropped.
instance unsafeExtendCompose :: ( Extend f
                                , Extend g
                                , Traversable f
                                , Applicative g
                                ) => Extend (Compose f g) where
    extend f = Compose
           <<< extend (map (f <<< Compose) <<< traverse duplicate)
           <<< decompose

instance unsafeComonadCompose :: ( Comonad f
                                 , Comonad g
                                 , Traversable f
                                 , Applicative g
                                 ) => Comonad (Compose f g) where
    extract = extract <<< extract <<< decompose
