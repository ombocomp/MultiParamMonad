{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Multi-parameter variant of Functor.
--  The contained data type is an explicit type parameter,
--  allowing instances to be made dependent on it.
--  Sets, for example, require an Ord-context on their
--  type parameters, while the type @Data.Functor.fmap@
--  is '(a -> b) -> f a -> f b', with no possible
--  restriction on 'a' or 'b'.
--  With the multi-parameter functor, the following instance
--  can be declared for Set:
--  @instance (Ord a, Ord b) => Functor Set a b where fmap = map@.
--
--  Adapted from 'http://okmij.org/ftp/Haskell/types.html#restricted-datatypes'.
module Data.Functor.MultiParam (
   Functor'(..),
   (<$>)
   ) where

import Prelude hiding (Functor(..))
import qualified Data.Functor as Fu(fmap)
import qualified Data.Set as Set

import Text.ParserCombinators.ReadP(ReadP)
import Text.ParserCombinators.ReadPrec(ReadPrec)
import GHC.Conc(STM)
import Control.Applicative(ZipList)

--infixl 4 <$
infixl 4 <$>

class Functor' f a b where
   -- |Applies a function over a functor.
   fmap :: (a -> b) -> f a -> f b
   -- |Replaces a value inside a functor with another value,
   --  discarding the old, but preserving the functor's structure.
   (<$) :: Functor' f b a => a -> f b -> f a
   x <$ f = fmap (const x) f

-- |Infix version of @fmap@.
(<$>) :: (Functor' f a b) => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor' [] a b where fmap = Fu.fmap
instance Functor' IO a b where fmap = Fu.fmap
instance Functor' Maybe a b where fmap = Fu.fmap
instance Functor' ReadP a b where fmap = Fu.fmap
instance Functor' ReadPrec a b where fmap = Fu.fmap
instance Functor' STM a b where fmap = Fu.fmap
instance Functor' ZipList a b where fmap = Fu.fmap
instance (Ord a, Ord b) => Functor' Set.Set a b where fmap = Set.map
