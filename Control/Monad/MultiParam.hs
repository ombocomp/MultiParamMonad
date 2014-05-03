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
   MonadPlus'(..),
   (=<<),
   (>=>),
   (<=<),
   filterM,
   ) where

import Prelude hiding ((>>=), (=<<), Functor(..), Monad (..))
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

-- |Synonym for @pure@.
return :: (Pure m a) => a -> m a
return = pure

-- |MonadPlus class with type parameter for the contained type.
--  '(mzero,mplus)' form a monoid over the monad 'm a'.
class (Monad' m a a) => MonadPlus' m a where
   mzero :: m a
   mplus :: m a -> m a -> m a

instance Monad' [] a b where
   x >>= f = x Mo.>>= f

instance MonadPlus' [] a where
   mzero = Mo.mzero
   mplus = Mo.mplus

instance Monad' IO a b where
   x >>= f = x Mo.>>= f

instance Monad' Maybe a b where
   x >>= f = x Mo.>>= f

instance MonadPlus' Maybe a where
   mzero = Mo.mzero
   mplus = Mo.mplus

instance Monad' ReadP a b where
   x >>= f = x Mo.>>= f

instance MonadPlus' ReadP a where
   mzero = Mo.mzero
   mplus = Mo.mplus

instance Monad' ReadPrec a b where
   x >>= f = x Mo.>>= f

instance MonadPlus' ReadPrec a where
   mzero = Mo.mzero
   mplus = Mo.mplus

instance Monad' STM a b where
   x >>= f = x Mo.>>= f

instance MonadPlus' STM a where
   mzero = Mo.mzero
   mplus = Mo.mplus

instance (Ord a, Ord b) => Monad' Set.Set a b where
   x >>= f = Set.foldl' Set.union Set.empty $ Set.map f x

instance Ord a => MonadPlus' Set.Set a where
   mzero = Set.empty
   mplus = Set.union


-- These functions are basically copies of those found in @Control.Monad@.

filterM :: (Monad' m Bool [a], Monad' m [a] [a]) => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM f (x:xs) = f x
                   >>= (\t -> filterM f xs
                   >>= (\ys -> pure $! if t then x:ys else ys))

-- |Flipped version of @>>=@.
(=<<) :: Monad' m a b => (a -> m b) -> m a -> m b
(=<<) = flip (>>=)

-- |Monadic version of function concatenation @.@.
(>=>) :: (Monad' m a b, Monad' m b c) => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = pure x >>= f >>= g 

-- |Flipped version of @>=>@.
(<=<) :: (Monad' m a b, Monad' m b c) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) = flip (>=>)

-- todo 
foldM = undefined
foldM_ = undefined
forM = undefined
forM_ = undefined
forever = undefined
guard = undefined
liftM = undefined
liftM2 = undefined
liftM3 = undefined
liftM4 = undefined
liftM5 = undefined
mapAndUnzipM = undefined
mapM = undefined
mapM_ = undefined
mfilter = undefined
msum = undefined
replicateM = undefined
replicateM_ = undefined
sequence = undefined
sequence_ = undefined
unless = undefined
void = undefined
when = undefined
zipWithM = undefined
zipWithM_ = undefined
