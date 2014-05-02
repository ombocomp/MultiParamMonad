{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Multi-parameter variant of Monad.
--  The contained data type is an explicit type parameter,
--  allowing instances to be made dependent on it.
--
--  @Monad\'@ is a subclass of @Applicative\'@ and @Pure@ and @Applicative\'@
--  is a subclass of @Monad'@.
--
--  @Monad\'@ only provides @(>>=)@, @(>>)@, and @join@. @return@ is not
--  part of the class, since it's just a synonym for @pure@.
--
--  While the type class is named differently, the rest of
--  the functions in this module clash heavily with those
--  of @Control.Monad@ and therefore, a qualified import is recommended.
--
--  Adapted from 'http://okmij.org/ftp/Haskell/types.html#restricted-datatypes'.
module Control.Monad.MultiParam (
   Monad'(..),
   filterM,
   ) where

import Prelude hiding ((>>=), Functor(..))
import qualified Control.Monad as Mo
import qualified Data.Set as Set
import Data.Functor.MultiParam
import Control.Applicative.MultiParam

import Text.ParserCombinators.ReadP(ReadP)
import Text.ParserCombinators.ReadPrec(ReadPrec)
import GHC.Conc(STM)

-- |Monad class with explicit type parameters for
--  the contained types.
--  Minimal complete definition: @(>>=)@.
class (Pure m a, Applicative' m a b) => Monad' m a b where
   -- |Sequentially compose two actions, passing any value
   --  produced by the first as an argument to the second.
   (>>=) :: m a -> (a -> m b) -> m b
   -- |Sequentially compose two actions, discarding any value
   -- produced by the first, like sequencing operators
   --(such as the semicolon) in imperative languages.
   (>>) :: m a -> m b -> m b
   (>>) x y = x >>= const y
   join :: Monad' m (m a) a => m (m a) -> m a
   -- |Flattens the monadic structure.
   join m = m >>= id

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

filterM :: (Monad' m Bool [a], Monad' m [a] [a]) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM f (x:xs) = f x
                   >>= (\t -> filterM f xs
                   >>= (\ys -> pure $! if t then x:ys else ys))

