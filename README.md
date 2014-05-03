MultiParamMonad
===============

Versions of Functor, Applicative, and Monad which include their contained data types as type parameters, allowing restrictions on them in the instance declarations. The structure of the new type classes is as follows:

#### Data.Functor.MultiParam

```haskell
class Functor' f a b where
   fmap :: (a -> b) -> f a -> f b
```

The only difference to Functor is the presence of the type variables `a` and `b`. This allows the following instance, which is impossible with `Functor` due to the required `Ord`-context:

```haskell
import qualified Data.Set as S

instance (Ord a, Ord b) => Functor' S.Set a where
   fmap = S.map
```

#### Control.Applicative.MultiParam

```haskell
class Pure f a where
   pure :: a -> f a
   
class Functor' f a b => Applicative' f a b where
   (<*>) :: f (a -> b) -> f a -> f b
```

Since `b` does not occur in `pure`, it has been split off into the type class `Pure`.

`Applicative'` is problematic in that a `Set`-instance would require a spurious instance `Ord (a -> b)`. Nonetheless, the instance can at least be written, even if a corresponding set cannot be constructed:

```haskell
import qualified Control.Applicative as A

instance Ord a => Pure Set a where
   pure = S.singleton
   
instance (Ord a, Ord b) => Applicative' Set a b where
  f <*> x = S.fromList $ f'  A.<*> x'
     where f' = S.toList f
           x' = S.toList x
```

The problem comes when one tries to use `<*>` with sets:
```
> :t \f -> f <*> fromList ([1,2,3]::[Integer])
 :: Ord b => Set (Integer -> b) -> Set b
```

The `Set (Integer -> b)` object cannot be constructed. Nonetheless, this only precludes the use of `<*>` in the context of sets, but doesn't pose any further problems.

#### Control.Monad.MultiParam

```haskell
class (Pure m a, Applicative' m a b) => Monad' m a b where
   (>>=) :: m a -> (a -> m b) -> m b
   (>>) :: m a -> m b -> m b
   (>>) x f = x >>= f
   join :: Monad m (m a) a => m (m a) -> m a
   join m = m >>= id
```

`Monad'` is different from `Monad` in three points:

1. It's a subclass of `Applicative`.
2. It contains `join` as a definable method.
3. It doesn't contain `return`, since `pure` already provides the same functionality.

