MultiParamMonad
===============

Versions of Functor, Applicative, and Monad which include their contained data types as type parameters, allowing restrictions on them in the instance declarations. Every Functor, Applicative and Monad is also an instance of `Functor'`, `Applicative'` and `Monad'`, but the explicit type variables allow additional instances like `Set`.
The hierarchy of the new type classes is as follows:

```
Functor' => Pure & Applicative' => Monad' => MonadPlus'
```

### Data.Functor.MultiParam

```haskell
class Functor' f a b where
   fmap :: (a -> b) -> f a -> f b
```

The only difference to Functor is the presence of the type variables `a` and `b`. This allows the following instance, which is impossible with `Functor` due to the required `Ord`-context:

```haskell
import qualified Data.Set as S

instance (Ord a, Ord b) => Functor' S.Set a where
   fmap' = S.map
```

### Control.Applicative.MultiParam

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
```haskell
> :t \f -> f <*> fromList ([1,2,3]::[Integer])
 :: Ord b => Set (Integer -> b) -> Set b
```

The `Set (Integer -> b)` object cannot be constructed. Nonetheless, this only precludes the use of `<*>` in the context of sets, but doesn't pose any further problems.

### Control.Monad.MultiParam

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

##### Issues with Monad's type variables

`x >>= f` could be defined as `join (fmap' f x)` in ordinary monads, but not in `Monad'`. The reason is the type variable `b`, which is left ambiguous in that definition:

```haskell
> :t \x f -> join (fmap' f x)
(Functor' m a (m b), Monad' m b x, Monad' m (m b) b) => m a -> (a -> m b) -> m b
```

Note the spurious type variable `x` induced by `join :: Monad' m b x => m (m b) -> m b`. Since `join`'s signature only mentions one type variable `b`, the second is left ambiguous. In 'ordinary' usage, it will be discarded, but as a default definition for `(>>=)` in `Monad`, it wouldn't type check.

#### MonadPlus'

```haskell
class (Monad' m a a) => MonadPlus' m a b where
   mzero :: m a
   mplus :: m a -> m a -> m a
```

`MonadPlus'` is a straightforward adaptation of `Control.Monad.MonadPlus`.
