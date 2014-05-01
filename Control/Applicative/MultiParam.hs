{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Multi-parameter variant of Applicative.
--  The contained data type is an explicit type parameter,
--  allowing instances to be made dependent on it.
--
--  Note that, unlike regular monads, multi-parameter
--  monads with restrictions do not always imply the existence
--  of (meaningul) instances of Applicative\'.
--  For example, Set can be a Monad' (with Ord-restrictions),
--  but it cannot be a meaningful Applicative'-instance, since
--  '<*> :: f (a -> b) -> f a -> f b' would require the context 'Ord (a -> b)'.
--
--  For technical reasons, the instance 'Applicative\' Set a b' is
--  provided nonetheless, with the definition
--
--  @
--  f <*> x = Set.fromList $ Set.toList f Ap.<*> Set.toList x
--  @
--
--  This might as well be undefined, as, by default, a set of functions
--  is not constructible. Should the user declare '(Ord (a -> b))', however,
--  @<*>@ will work.
--
--  Adapted from 'http://okmij.org/ftp/Haskell/types.html#restricted-datatypes'.
module Control.Applicative.MultiParam (
   Applicative'(..),
   ) where

import qualified Control.Monad as Mo
import qualified Control.Applicative as Ap
import qualified Data.Set as Set
import Data.Functor.MultiParam

import Text.ParserCombinators.ReadP(ReadP)
import Text.ParserCombinators.ReadPrec(ReadPrec)
import GHC.Conc(STM)


class Functor' f a b => Applicative' f a b where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative' [] a b where
   pure = Mo.return
   f <*> x = f Ap.<*> x

instance Applicative' IO a b where
   pure = Mo.return
   f <*> x = f Ap.<*> x

instance Applicative' Maybe a b where
   pure = Mo.return
   f <*> x = f Ap.<*> x

instance Applicative' ReadP a b where
   pure = Mo.return
   f <*> x = f Ap.<*> x

instance Applicative' ReadPrec a b where
   pure = Mo.return
   f <*> x = f Ap.<*> x

instance Applicative' STM a b where
   pure = Mo.return
   f <*> x = f Ap.<*> x

instance (Ord a, Ord b) => Applicative' Set.Set a b where
   pure = Set.singleton
   f <*> x = Set.fromList $ Set.toList f Ap.<*> Set.toList x
