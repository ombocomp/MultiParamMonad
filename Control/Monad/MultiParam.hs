{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Restricted variant of Monad whose elements
--  have an Ord-context. This allows additional types
--  (like Sets) to be instances. The laws for OrdMonad
--  are the same as those for Monad.
--
--  Note that OrdMonad is a subclass of OrdFunctor, but
--  NOT a subclass of (Ord)Applicative -
--  (Ord)Applicative.(<*>) would require the instance
--  Ord (a -> b).
module Control.Monad.MultiParam (
   Monad'(..),
   ) where

import qualified Control.Monad as Mo
import qualified Data.Set as Set
import Control.Applicative.MultiParam

import Text.ParserCombinators.ReadP(ReadP)
import Text.ParserCombinators.ReadPrec(ReadPrec)
import GHC.Conc(STM)


class Applicative' m a b => Monad' m a b where
   (>>=) :: m a -> (a -> m b) -> m b

instance Monad' [] a b where
   x >>= f = x Mo.>>= f

instance Monad' IO a b where
   x >>= f = x Mo.>>= f

instance Monad' Maybe a b where
   x >>= f = x Mo.>>= f

instance Monad' ReadP a b where
   x >>= f = x Mo.>>= f

instance Monad' ReadPrec a b where
   x >>= f = x Mo.>>= f

instance Monad' STM a b where
   x >>= f = x Mo.>>= f

instance (Ord a, Ord b) => Monad' Set.Set a b where
   x >>= f = Set.foldl' Set.union Set.empty $ Set.map f x
