{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Multi-parameter variant of Applicative.
--  The contained data type is an explicit type parameter,
--  allowing instances to be made dependent on it.
--
--  The Applicative type class is split into two classes:
--  @Pure f a@, which provides @pure :: a -> f a@, and
--  @Applicative' f a b@, which provides
--  @(<*>) :: f (a -> b) -> f a -> f b@.
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
--  Adapted from Oleg Kiselyov's
--  'http://okmij.org/ftp/Haskell/types.html#restricted-datatypes'.
module Control.Applicative.MultiParam (
   Applicative'(..),
   Pure(..),
   ) where

import qualified Control.Monad as Mo
import qualified Control.Applicative as Ap
import qualified Data.Set as Set
import Data.Functor.MultiParam

import Text.ParserCombinators.ReadP(ReadP)
import Text.ParserCombinators.ReadPrec(ReadPrec)
import GHC.Conc(STM)

class Pure f a where
   pure :: a -> f a

class Functor' f a b => Applicative' f a b where
   (<*>) :: f (a -> b) -> f a -> f b

instance Pure [] a where pure = Mo.return
instance Applicative' [] a b where f <*> x = f Ap.<*> x

instance Pure IO a where pure = Mo.return
instance Applicative' IO a b where f <*> x = f Ap.<*> x

instance Pure Maybe a where pure = Mo.return
instance Applicative' Maybe a b where f <*> x = f Ap.<*> x

instance Pure ReadP a where pure = Mo.return
instance Applicative' ReadP a b where f <*> x = f Ap.<*> x

instance Pure ReadPrec a where pure = Mo.return
instance Applicative' ReadPrec a b where f <*> x = f Ap.<*> x

instance Pure STM a where pure = Mo.return
instance Applicative' STM a b where f <*> x = f Ap.<*> x

instance Ord a => Pure Set.Set a where pure = Set.singleton
instance (Ord a, Ord b) => Applicative' Set.Set a b where
   f <*> x = Set.fromList $ Set.toList f Ap.<*> Set.toList x
